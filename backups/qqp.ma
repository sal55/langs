=== MA 48 ===
=== qqp.m 0 0 1/48 ===
!project =
	module qq_cli

	module qq_arrays
	module qq_bits
	module qq_calldll
	module qq_decls
	module qq_decimal
	module qq_dicts

	module qq_host
	module qq_lex
	module qq_lib
	module qq_lists
	module qq_modules
	module qq_names

	module qq_packed
	module qq_parse
	module qq_PCLTABS
	module qq_pclgen
	module qq_pcllib
	module qq_print
	module qq_records
	module qq_resolve

	module qq_run
	module qq_runaux

	module qq_sets
	module qq_strings

	module qq_syslibs
!	module qq_syslibsdummy

	module qq_tables

!	module qq_show
	module qq_dummyshow

!	module qq_showpcl
	module qq_showpcldummy
!
	module qq_vars
!end
=== qq_cli.m 0 0 2/48 ===
GLOBAL INT NALLDOT
GLOBAL INT NALLDOT1FIELD

!global const syslibname="sysp"
global ichar syslibname
!global const syslibname="minsys"

global enumdata []ichar runnames =
	(load_cc,		$),
	(parse_cc,		$),
	(names_cc,		$),
	(gencode_cc,	$),
	(fixup_cc,		$),
	(run_cc,		$),
end

global byte fshowpcl1
global byte fshowpcl2
!global byte fshowpcl3
global byte fshowast1
global byte fshowast2
global byte fshowst
global byte fshowstflat
global byte fshowtypes
global byte foptimise=1
global byte fwriteqa			!0, 1 or 2
global byte fshowmodules
global byte fallsp

global byte runcode  = run_cc

global ichar sourcestr

global ichar inputfile

global symbol allprocdefs, allstaticdefs

ref strbuffer pclstr

int cmdstartindex

proc main=
	ichar source
	int i,nnames,t,tstart, stopcode
	unit p

!CPL "HI THERE"
	initdata()

	getinputoptions()

!
!FINDSYSLIB("FRED")


!CPL =FOPTIMISE

	readqabundle()

	loadsyslib()

!INT TT:=CLOCK()
	compile_sp(inputfile)

	if fallsp then
		if fshowast1 and runcode=parse_cc then showast(nil, "AST1") fi
		if fshowast2 and runcode>parse_cc then showast(nil, "AST2") fi
!		if fshowpcl2 and runcode=fixup_cc then showpcl(nil, 2) fi
fi

!TT:=CLOCK()-TT
!CPL =TT

!run the stack of sps (laters sps will be run as they are compiled)

	writeqafile()

	for i to nsubprogs do
		stopcode:=runqprogram(subprogs[i], i=nsubprogs)
	od

	showlogfile()

	stop stopcode
end

proc getinputoptions=
	int paramno,pmtype
	ichar name,value
	ichar appstr, appname
	ref function:ichar fnaddr

!fnaddr will be nil unless a built-in app exists
	fnaddr:=findfunction("getbuiltin_app")

	paramno:=1

	while pmtype:=nextcmdparamnew(paramno,name,value,"q") do
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
			if fnaddr then				!treat as data
				--paramno
				exit
			fi
			inputfile:=pcm_copyheapstring(name)
			exit				!leave any other files to app being run
		esac
	od

	if fnaddr then
		appstr:=fnaddr()
!		dobuiltin_app(appstr, appname)
LOADERROR("DO BUILT-IN")

!		dobuiltin_app(appstr)
	elsif not inputfile then
		println "Q7.0 Interpreter"
		println "Usage:"
		println "	",,cmdparams[0],"filename[.q]"
		stop
	fi

	cmdstartindex:=paramno

	setcli(cast(&cmdparams[cmdstartindex]),ncmdparams-cmdstartindex+1)
end

proc do_option(int sw, ichar value)=
	ref byte p

	p:=optionvars[sw]
	if p then
		p^:=optionvalues[sw]
		return
	fi

end

global proc compile_sp(ichar filename, source=nil)=
	ichar qafile
	isubprog sp
	int a, b

!CPL "COMPILESP",=SYSLIBNAME

	sp:=loadsp(filename, source)

	if runcode<parse_cc then return fi

	a:=sp.firstmodule
	b:=sp.lastmodule

	for m in a..b do
		parsemodule(modules[m])
	od
	fixusertypes()
	if fshowast1 and not fallsp then showast(sp,"AST1") fi

	return when runcode<names_cc

	tx_typetable()

	for m in a..b do
		rx_module(modules[m])
	od
	if fshowast2 and not fallsp then showast(sp,"AST2") fi

	return when runcode<gencode_cc
!
	for m in a..b do
		gencodemodule(sp, m)
	od

	if fshowpcl1 and not fallsp then showpcl(sp,1) fi

	fixup_sp(sp)

	if fshowpcl2 and runcode=fixup_cc and not fallsp then showpcl(sp,2) fi

	resetcompiler()
end

proc setcli(ref []ichar cmds, int ncmds)=
	for i to ncmds do
		setcmdparam(i,cmds[i])
	od
end

proc writeqafile=
	[300]char filename
	[maxmodule]ifile sflist
	filehandle f
	int offset, nfiles, fileno
	ifile pm
	int leadmod

	if not fwriteqa then
		return
	fi
	strcpy(filename, changeext(inputfile,"qa"))

!first build a table of source files to be o/p
	nfiles:=0

	LEADMOD:=SUBPROGS[NSUBPROGS].FIRSTMODULE

	SFLIST[++NFILES]:=MODULES[LEADMOD]

	for i to nmodules WHEN I<>LEADMOD do
		pm:=modules[i]


		if pm.issyslib and fwriteqa=1 then		!no syslibs
			nextloop
		fi
		sflist[++nfiles]:=pm
	od

	if nfiles=0 then loaderror("QA:no files") fi

	f:=fopen(filename,"wb")
	if not f then loaderror("Can't create qa file #",filename) fi

	println "Writing ",filename
	fprintln @f,"=== QA # ===",nfiles

	for i to nfiles do
		pm:=sflist[i]

		fprintln @f,"=== #.q # # #/# ===",
			pm.name, pm.issyslib, pm.issupport,i, nfiles

		offset:=getfilepos(f)
		writerandom(f,cast(pm.text),offset,pm.size)
	od

	println @f,"=== END ==="

	for i to nfiles do
		fprintln @f,"# #.q",i,sflist[i].name
	od

	fclose(f)
	stop
end

proc initdata=
	lexinit()
!	inithostlib()

	stprogram:=createdupldef(nil,addnamestr("$prog"),programid)

	os_initwindows()

	firstusertype:=tlast+1

	deletetempfiles()

!	fnosys:=not os_iswindows()
end

proc loadsyslib=
	[300]char str
!	ichar syslibname

!CPL "LOADSYSLIB", =FNOSYS, =USEBUNDLED


	if fnosys then return fi

	if not fsyslibs then usebundled:=0 fi

!CPL "LOADSYSLIB2", =FNOSYS, =USEBUNDLED
	if os_iswindows() then
		syslibname:="syswin.q"
	else
		syslibname:="syslin.q"
	fi

!CPL =SYSLIBNAME, =USEBUNDLED
	if usebundled then				!bundled sys files
!CPL "LOADSYS1", =SYSLIBNAME
		compile_sp(syslibname)
	else							!-ext used
!CPL "LOADSYS2", =SYSLIBNAME
		if os_iswindows() then
			strcpy(str, devdir)
		else
			strcpy(str, "./")		!on Linux, expects to find them locally
		fi
		strcat(str, syslibname)
		compile_sp(str)
	fi
end

proc resetcompiler=
!called at end of compilesp so to reset globals for next sp

!should really recover any resources used here
	nuserxtypes:=0
	userxtypebase:=0
	ref userxrec userxmodelist:=nil
	CLEAR TTXMAP				!later limit to old nuserxtypes for efficiency

	firstusertype:=ntypes+1
end

proc setcmdparam(int index, ichar s)=
!build cmd params for pc program, or set size (usually smaller) when s is nil
	if s=nil then
		nqparams:=index
	elsif index<=maxqparam then
		qparamtable[index]:=pcm_copyheapstring(s)
		nqparams max:=index
	fi
end

proc fixup_sp(isubprog sp)=
	return when runcode<fixup_cc

	for i:=sp.firstmodule to sp.lastmodule do
		fixupmodule(modules[i])
		if foptimise then
			optimise_module(modules[i])
		fi
		fixupcode(modules[i])
	od
end

proc fixproc(symbol d)=
	variant p

	if not d.procfixed then
		d.nextproc:=allprocdefs
		allprocdefs:=d
		d.procfixed:=1
	fi
end

proc fixupmodule(ifile pm)=
	pcl pc,pcstart
	int cmd, index
	symbol d
	variant p

	pc := pcstart := pm.pcstart

	repeat
		cmd:=pc.opcode

		case cmd
		when kprocdef then
			fixproc(pc.def)
		esac

		case pclopnd[cmd]
		when cproc then
			fixproc(d:=pc.def)
			pc.labelref:=d.labelref

		when cstatic then
			d:=pc.def
			if d.isframe then
				pc.offset:=d.index*varsize
				++pc.opcode								!kpushm->kpushf etc
			else
				if d.varptr=nil then
					d.nextstatic:=allstaticdefs
					allstaticdefs:=d
					d.varptr:=pcm_allocz(varsize)		!sets to tvoid
				fi
				pc.varptr:=d.varptr
			fi

		when cgenfield then
!CPL "FIXUP GENFIELD", PC.DEF.NAME, PC.DEF.GENFIELDINDEX
			index:=pc.def.genfieldindex
!			IF INDEX=0 THEN RXERROR_S("FIX/GENFIELD/ZERO?", PC.DEF.NAME) FI
			pc.index:=index


		when cstring then
			pc.objptr:=obj_make_string(pc.svalue, 0)

		end

		++pc
	until cmd=kendmod
end

proc optimise_module(ifile pm)=
	pcl pc

!CPL "OPTIMISE MODULE", PM.NAME

	pc:=pm.pcstart

	do
		exit when pc.opcode=kendmod
		pc:=optim(pc)
	od

!Do static counts
	pc:=pm.pcstart
	while pc.opcode<>kendmod, ++pc do
		++pclcounts[pc.opcode]
	od

end

func optim(pcl pc)pcl=
	int skip, INDEX
	ref genfieldrec g
	pcl pcdest
	byte cmd, newcmd, abc, aux
	macro pcb = (pc+1)
	macro pcc = (pc+2)
	macro pcd = (pc+3)


	cmd:=newcmd:=pc.opcode
!CPL PCLNAMES[CMD], PC-PCSTART

	skip:=0					!extra ops to skip tp next instruction

	if cmd=kjump or cmd in kjumpeq..kjumpgt or cmd in [kjumpt, kjumpf] then
		pcdest:=pc.labelref
		if pcdest.opcode=kjump then
			repeat
				pcdest:=pcdest.labelref
			until pcdest.opcode<>kjump
			pc.labelref:=pcdest
		fi
	fi

	if pcb.haslabel then return pcb fi		!label on B
	abc:=1									!assume ops ABC are condidates
	if pcc.haslabel then abc:=0 fi			!AB only

	case cmd
	when kpushf then
		case pcb.opcode			!opcode that follows
		when kpushf then			!pushf pushf
			if not abc then dopushff fi
			case pcc.opcode
			when kpushf then
				newcmd:=kpushfff
				skip:=2

			when kadd then
				newcmd:=kaddff
				skip:=2
!
			when ksub then
				skip:=2


			when kjumpeq then
				newcmd:=kjmpeqff
				skip:=2
			when kjumpne then
				newcmd:=kjmpneff
				skip:=2
			when kjumplt then
				newcmd:=kjmpltff
				skip:=2
			when kjumple then
				newcmd:=kjmpleff
				skip:=2
			when kjumpge then
				newcmd:=kjmpgeff
				skip:=2
			when kjumpgt then
				newcmd:=kjmpgtff
				skip:=2
			when kindex then
				newcmd:=kindexff
				skip:=2
			else
dopushff:
				newcmd:=kpushff
				skip:=1
			end
		when kpushm then			!pushf pushm
!!				++pclcounts[kpushfm]
!!			skip:=1
		when kpushci then			!pushf pushci
			if not abc then dopushfci fi
			case pcc.opcode
			when kadd then
				newcmd:=kaddfci
				skip:=2
			when ksub then
				newcmd:=ksubfci
				skip:=2
			when kjumplt then
				newcmd:=kjmpltfci
				skip:=2
			when kjumple then
				newcmd:=kjmplefci
				skip:=2
			when kjumpge then
				newcmd:=kjmpgefci
				skip:=2
			when kjumpgt then
				newcmd:=kjmpgtfci
				skip:=2
			when kjumpeq then
				newcmd:=kjmpeqfci
				skip:=2
			when kjumpne then
				newcmd:=kjmpnefci
				skip:=2
			else					!just pushfci
dopushfci:
				newcmd:=kpushfci
				skip:=1

			end

		when kpopm then				!pushf popm
			skip:=1
		when kpopf then				!pushf popf
			newcmd:=kmoveff
			skip:=1
		when kzpopf then			!pushf zpopf
			skip:=1
		when kswitch then			!pushf case
			skip:=1

		when klen then				!pushf len
			skip:=1
		when kupb then				!pushf upb
			skip:=1
		when kpushptr then			!pushf pushptr
			newcmd:=kpushptrf
			skip:=1
		end

	when kpushm then
		case pcb.opcode
!		when kpushm then			!pushm pushm
!			++pclcounts[kpushmm]
!			skip:=1
		when kpushf then			!pushm pushm
			if abc and pcc.opcode=kindex then
				newcmd:=kindexmf
				skip:=2
			else
!				++pclcounts[kpushmf]
				skip:=1
			fi
		when kpopm then				!pushm popm
!			++pclcounts[kmovemm]
			skip:=1
		when kpopf then				!pushm popf
!			++pclcounts[kmovefm]
			skip:=1
		when kpushci then
			newcmd:=kpushmci
			skip:=1
		esac

	when kpushci then
!CPL "PUSHCI", PCLNAMES[PCB.OPCODE]

		case pcb.opcode
		when kpopm then				!pushci popm
!			++pclcounts[kmovemci]
			skip:=1
		when kpopf then				!pushci popf
!CPL "PUSHCI/POPF"
			newcmd:=kmovefci
!			++pclcounts[kmovefci]
			skip:=1
		when kzpopf then			!pushci zpopf
!			++pclcounts[kzmovefci]
			skip:=1
		when kadd then
			newcmd:=kaddci
			skip:=1

		when ksub then
			newcmd:=ksubci
			skip:=1

		when kiand then
			newcmd:=kiandci
			skip:=1

		when kshl then
			newcmd:=kshlci
			skip:=1

		when kshr then
			newcmd:=kshrci
			skip:=1
!
		when kwheneq then
			newcmd:=kwheneqci
			skip:=1
		when kwhenne then
			newcmd:=kwhenneci
			skip:=1

		when kpushfref then
			if abc and pcc.opcode=kbinto then
				case bintotable[pcc.bintoindex].pclop
				when kadd then
					newcmd:=kaddtofci
					skip:=2
				when ksub then
					newcmd:=ksubtofci
					skip:=2
!				when kixor then
!					newcmd:=kixortofc
!					skip:=2
				when kshl then
					newcmd:=kshltofci
					skip:=2
				when kshr then
					newcmd:=kshrtofci
					skip:=2
				esac
			fi

!		elsif a=0 and b not in [kraise,kstop] then
!			pc^:=kpushci0
		esac

	when kpushvoid then
		case pcb.opcode
		when kpushvoid then			!pushvoid pushvoid
			if abc and pcc.opcode=kpushvoid then
!				++pclcounts[kpushv3]
				skip:=2
			else
!				++pclcounts[kpushv2]
				skip:=1
			fi
		esac
	when kpushfref then
		case pcb.opcode
		when kloadincr then
			if abc then
				case pcc.opcode
				when kpushptr then		!loadincr pushptr
					newcmd:=kpushipf
					skip:=2
				when kpopptr then		!loadincr popptr
					newcmd:=kpopipf
					skip:=2
				esac
			fi
		when kbinto then
			if pcb.bintoindex=1 then
				newcmd:=kaddtof
			else
				newcmd:=kbintof
			fi
			skip:=1
		esac

	when kpushmref then
		case pcb.opcode
		when kloadincr then
			if abc then
				case pcc.opcode
				when kpushptr then		!loadincr pushptr
					newcmd:=kpushipm
					skip:=2
				when kpopptr then		!loadincr popptr
					newcmd:=kpopipm
					skip:=2
				esac
			fi
		esac

	when kpopf then
		case pcb.opcode
		when kpushf then
			if pc.def=pcb.def then
!CPL "POPF/PUSH => STOREF"
				newcmd:=kstoref
				skip:=2
			fi
		esac


	end

finish:
	pc.opcode:=newcmd
	if skip then
		aux:=newcmd<>cmd			!an update has been done
		for i to skip do
			++pc
			pc.isaux:=aux
		od
	fi

	pc+1
end

=== qq_arrays.m 0 0 3/48 ===
global proc var_empty_array(int tag, elemtype, lower, variant dest)=
	dest.objptr:=obj_newarray(elemtype,lower, 0)
	dest.tagx:=tag ior hasrefmask
!	dest.usertag:=usertag
end

global proc obj_free_array(object p)=
	if p.length then
		pcm_free(p.ptr, p.alloc64*ttsize[p.elemtag])
	fi

	pcm_free32(p)
end

global proc obj_free_vector(object p)=
	if p.length then
		pcm_free(p.ptr,ttsize[p.usertag])
	fi

	pcm_free32(p)
end

global proc var_make_array(variant a, dest, int lower, n, axtype, elemtype) =
!create a list of n vars starting from a in reverse order (a is the last)
!put the result in dest (note this will be the last the n vars)
!axtype = array, elemtype = void/T
!axtype = vector basetype, elemtype = T

	object p
	ref byte q
	int m

	if axtype<>tarray then				!vector: built-in length and elemtype
		m:=ttlength[axtype]
		if n<>m then
			println =n,=m
			pcerror("Too few/too many elements")
		fi

	elsif n then
		if elemtype=tvoid then			!array: n>0: no elemtype override
			case (a+n-1).tag
			when tint then elemtype:=ti64
			when treal then elemtype:=tr64
			else
				elemtype:=ti64
			esac
		fi

	elsif elemtype=tvoid then			!array: n=0: no elemtype override
		elemtype:=ti64
	fi

	p:=obj_newarray(elemtype,lower,n)
	q:=p.ptr

	to n do
		var_storepacked(q,a,elemtype)
		q+:=ttsize[elemtype]
		++a
	od

	if axtype=tarray then
		dest.tagx:=tarray ior hasrefmask
	else
		dest.tagx:=tvector ior hasrefmask
		p.usertag:=axtype
	fi
	dest.objptr:=p
end

global function obj_newarray(int elemtype, lower,length)object p=
!create a packed array with element-type t, given length and lower bound.
!it will be initialised to zeros

	ref byte q
	int elemsize

	p:=obj_new()
	p.mutable:=1
	if lower in 0..1 then
		p.lower:=lower
	else
		pcerror("Lwb not 0/1")
	fi
	p.length:=length
	p.objtype:=normal_obj
	p.elemtag:=elemtype
	elemsize:=ttsize[elemtype]

	if length then
		p.ptr:=pcm_allocz(length*elemsize)
		p.alloc64:=allocbytes/elemsize
	fi

	return p
end

global function obj_newarray_u(int usertag)object p=
!create a packed array with element-type t, given length and lower bound.
!it will be initialised to zeros

	ref byte q
	int elemsize

	p:=obj_new()
	p.mutable:=1
	p.objtype:=normal_obj
	p.usertag:=usertag
	elemsize:=ttsize[tttarget[usertag]]

	if ttlength[usertag] then
!		p.ptr:=pcm_allocz(ttlength[usertag]*elemsize)
		p.ptr:=pcm_allocz(ttsize[usertag])
		p.alloc64:=allocbytes/elemsize
	fi

	return p
end

global proc var_getix_array(variant a, int index)=
!a is a list, b is an int; return a[b] into a, which will be on the stack usually
	varrec v
	object p
	int elemtype,length

	v:=a^
	p:=a.objptr

	if v.tag=tvector then
		length:=ttlength[p.usertag]
		index-:=ttlower[p.usertag]
		elemtype:=tttarget[p.usertag]
	else
		length:=p.length
		elemtype:=p.elemtag
		index-:=p.lower
	fi

	if u64(index)>=u64(length) then
		pcerror("ax[int] bounds")
	fi

	if elemtype=tu8 then
		a.tagx:=tint
		a.value:=(p.ptr+index)^
	else
		var_loadpacked(p.ptr+index*ttsize[elemtype],elemtype, a)
	fi
end

global proc var_putix_array(variant a, int index, variant x)=
!a[index]:=x
	varrec v
	object p
	int elemtype, length, lower

	v:=a^
	p:=v.objptr

	if v.tag=tvector then
		length:=ttlength[p.usertag]
		lower:=ttlower[p.usertag]
		elemtype:=tttarget[p.usertag]
	else
		length:=p.length
		lower:=p.lower
		elemtype:=p.elemtag
	fi

	index-:=lower

	if u64(index)>=u64(length) then
		if index<0 then
			pcerror("lwb")
		elsif index=length then
			if v.tag=tvector then
				pcerror("Can't append user type")
			fi
			obj_append_array(p,x)
		else
			pcerror("ax[i]:=x bounds")
		fi
	fi

	if elemtype=tu8 then
		if x.tag<>tint then pcerror("rhs not int") fi
		a.tagx:=tint
		(p.ptr+index)^:=x.value
	else
		var_storepacked(p.ptr+index*ttsize[elemtype],x,elemtype)
	fi
end

global proc var_getixref_array(variant a, int index)=
!a[index]:=x
	varrec v
	object p
	int elemtype, length, lower

	v:=a^
	p:=v.objptr

	if v.tag=tvector then
		length:=ttlength[p.usertag]
		lower:=ttlower[p.usertag]
		elemtype:=tttarget[p.usertag]
	else
		length:=p.length
		lower:=p.lower
		elemtype:=p.elemtag
	fi

	index-:=lower

	if u64(index)>=u64(length) then
		if index<0 then
			pcerror("lwb")
		else
			if u64(index)=u64(length) then
PCERROR("PUTIXREF NEEDS IAPPEND")
!				var_iappendarray(a,nil)
				p:=a.objptr
			else
				pcerror("ax[i]:=x bounds")
			fi
		fi
	fi

	a.tagx:=trefpack
	a.elemtag:=elemtype
	a.ptr:=p.ptr+index*ttsize[elemtype]
end

proc obj_append_array(object a, variant x)=
!do in-place append of b to list a
	int n
	ref byte q

	if a.objtype<>normal_obj then
		pcerror("Can't extend slice")
	fi

	if not a.mutable then
		pcnotmut()
	fi

	n:=a.length+1			!new length

	if n>a.alloc64 then		!need more space
		obj_resize_array(a,n)
	else
		a.length:=n
	fi

	q:=a.ptr+(n-1)*ttsize[a.elemtag]

	var_storepacked(cast(q), x, a.elemtag)
end

global proc var_appendto_array(variant a, x)=
	obj_append_array(a.objptr,x)
end

global proc obj_resize_array(object p,int n)=
	ref byte q
	int elemsize

	elemsize:=ttsize[p.elemtag]

	if n<=p.alloc64 then
		p.length:=n
	else
		q:=pcm_alloc(n*elemsize)
		if p.length then
			memcpy(q,p.ptr,p.length*elemsize)
			pcm_free(p.ptr,p.alloc64*elemsize)
		fi
		p.ptr:=q
		p.length:=n
		p.alloc64:=allocbytes/elemsize
	fi
end

global proc var_dupl_array(variant a)=
	object p,q
	int elemsize

	p:=a.objptr
	q:=obj_newarray(p.elemtag, p.lower, p.length)
	a.objptr:=q

	if p.length then
		memcpy(q.ptr, p.ptr,
			p.length*ttsize[p.elemtag])
	fi
end

global proc var_dupl_vector(variant a)=
	object p,q
	int elemsize,length

	p:=a.objptr
	length:=ttlength[p.usertag]
	q:=obj_newarray_u(p.usertag)
	a.objptr:=q

	if length then
		memcpy(q.ptr, p.ptr,ttsize[p.usertag])
	fi
end

global function var_equal_array(variant a,b)int=
	object p:=a.objptr
	object q:=b.objptr
	int length,elemsize:=p.elemtag

	if p.elemtag<>q.elemtag then
		return 0
	fi
	length:=p.length

	if length<>q.length then
		return 0
	fi
	if length=0 then return 1 fi

	return eqbytes(p.ptr, q.ptr, ttsize[p.elemtag]*length)
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
		pcnotmut()
	fi

	if pa.elemtag<>pb.elemtag then
		pcerror("concat/not compat")
	fi
	elemsize:=ttsize[pa.elemtag]

	alen:=pa.length
	blen:=pb.length

	if alen=0 then					!concat to empty array
		if blen then				!copy b to a (else leave a as empty)
			obj_resize_array(pa,blen)
			d:=pa.ptr
			memcpy(d,pb.ptr,blen*elemsize)
		fi
	elsif blen then					!neither array is empty (else leave a unchanged)
		newlen:=alen+blen
		obj_resize_array(pa,newlen)
		d:=pa.ptr+alen*elemsize
		memcpy(d,pb.ptr,blen*elemsize)
	fi
end

global proc var_getslice_array(variant a, int i,j)=
	int alower,elemsize
	object p,q

	p:=a.objptr

	alower:=p.lower
	elemsize:=ttsize[p.elemtag]

	if i<alower or j>p.length+alower-1 or i>j then
		pcerror("array/slice bounds")
	fi

	q:=obj_new()

	q.objtype:=slice_obj
	q.mutable:=p.mutable
	q.lower:=1
	q.ptr:=p.ptr+(i-alower)*elemsize
	q.elemtag:=p.elemtag

	case p.objtype
	when slice_obj then				!slice of a slice!
		q.objptr2:=p.objptr2		!link to original
		obj_shareu(q.objptr2)

	when extslice_obj then
		q.objptr2:=nil
		q.objtype:=extslice_obj
	else
		q.objptr2:=p				!link to original
		++p.refcount
	esac

	q.length:=j-i+1
	a.objptr:=q
end

function u8inarray(byte a,object p)int=
!look for byte value a within array
!return index of first matching value, or lowerbound-1 (ie. 0 for 1-based arrays)
	int i
	ref byte q

	i:=p.lower
	q:=p.ptr

	to p.length do
		if q^=a then
			return i
		fi
		++q
		++i
	od
	return i64.min
end

global function u16inarray(u16 a,object p)int=
	int i
	ref u16 q

	i:=p.lower
	q:=cast(p.ptr)

	to p.length do
		if q^=a then
			return i
		fi
		++q
		++i
	od
	return i64.min
end

global function u32inarray(u32 a,object p)int=
	int i
	ref u32 q

	i:=p.lower
	q:=cast(p.ptr)

	to p.length do
		if q^=a then
			return i
		fi
		++q
		++i
	od
	return i64.min
end

global function u64inarray(u64 a,object p)int=
	int i
	ref u64 q

	i:=p.lower
	q:=cast(p.ptr)

	to p.length do
		if q^=a then
			return i
		fi
		++q
		++i
	od
	return i64.min
end

global function var_inx_array(variant a,b, int usertag)int n =
!a will be int/real/word
	object q:=b.objptr
	int elemtag

	if usertag then				!vector
		elemtag:=tttarget[usertag]
	else
		elemtag:=q.elemtag
	fi

	case elemtag
	when ti8,tu8 then
		n:=u8inarray(a.value,q)
	when ti16,tu16 then
		n:=u16inarray(a.value,q)
	when ti32,tu32 then
		n:=u32inarray(a.value,q)
	when ti64,tu64 then
		n:=u64inarray(a.value,q)
	else
		pcustype("x in array",b)
	esac
	return n
end

global proc var_expand_array(variant p, dest, int m)=
	ref byte q
	int i,n,elemtype,length
	object pa

	pa:=p.objptr

	if p.tag=tarray then
		length:=pa.length
		elemtype:=pa.elemtag
	else
		length:=ttlength[pa.usertag]
		elemtype:=tttarget[pa.usertag]
	fi

	q:=pa.ptr

	n:=1

	to m do
		if n>length then
			dest.tagx:=tvoid
		else
			var_loadpacked(q,elemtype,dest,nil)
			q+:=ttsize[elemtype]
		fi
		++n
		--dest
	od
end
=== qq_bits.m 0 0 4/48 ===
global proc obj_free_bits(object p, int tag)=
	if p.length then
		pcm_free(p.ptr, getbitssize(p.alloc64, p.elemtag))
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
	q:=p.ptr

	bitwidthx:=ttbitwidth[elemtype]
	offset:=0

	to n do
		var_storebit(q,offset,a,elemtype,bitwidthx)
		offset+:=bitwidthx
		if offset>=8 then
			++q
			offset:=0
		fi
		++a
	od

	dest.tagx:=bxtype ior hasrefmask
	dest.objptr:=p
end

global function obj_newbits(int elemtype, lower,length)object p=
!create a packed array with element-type t, given length and lower bound.
!it will be initialised to zeros

	ref byte q
	int nbits,bitwidthx,nbytes

	p:=obj_new()
	p.mutable:=1
	p.lower:=lower
	p.length:=length
	p.objtype:=normal_obj
	p.elemtag:=elemtype

	if length then
		nbytes:=getbitssize(length, elemtype)
		p.ptr:=pcm_allocz(nbytes)
		p.alloc64:=allocbytes*(8/ttbitwidth[elemtype])
	fi

	return p
end

global proc var_getix_bits(variant a, int index)=
!a is a list, b is an int; return a[b] into a, which will be on the stack usually
	object p
	ref byte q
	int elemtype,offset,shift

	p:=a.objptr
	elemtype:=p.elemtag
	index-:=p.lower

	if u64(index)>=u64(p.length) then
		pcerror("ax[int] bounds")
	fi
	q:=p.ptr
	a.tagx:=tint

	index+:=p.indexoffset

	case p.elemtag
	when tu1 then
		a.value:=not not ((q+index>>3)^ iand (1<<(index iand 7)))
	when tu2 then
		shift:=(index iand 3)*2
		a.value:=((q+index>>2)^ iand (3<<shift))>>shift
	when tu4 then
		shift:=(index iand 1)*4
		a.value:=((q+index>>1)^ iand (15<<shift))>>shift
	else
		pcustype_t("bitix",p.elemtag)
	end
end

global proc var_putix_bits(variant a, int index, variant x)=
!a[index]:=x
	object p
	ref byte q
	int elemtype, newoffset

	p:=a.objptr
	elemtype:=p.elemtag

	index-:=p.lower

	if u64(index)>=u64(p.length) then
		if index<0 then
			pcerror("lwb")
		elsif index=p.length then
			obj_append_bits(p,x)
		else
			pcerror("bx[i]:=x bounds")
		fi
	fi

	q:=getindexoffset(p.ptr, p.indexoffset, index, elemtype, newoffset)
	var_storebit(q, newoffset*ttbitwidth[elemtype],x,elemtype,0)
end

global proc var_getixref_bits(variant a, int index)=
!a[index]:=x
	varrec v
	object p
	ref byte q
	int offset, newoffset
	int elemtype

	p:=a.objptr
	elemtype:=p.elemtag
	index-:=p.lower

	if u64(index)>=u64(p.length) then
!
		pcerror("&bx[i] bounds")
	fi

	q:=getindexoffset(p.ptr, p.indexoffset, index, elemtype, newoffset)

	a.tagx:=trefbit
	a.elemtag:=elemtype
	a.ptr:=q
	a.bitoffset:=newoffset*ttbitwidth[elemtype]
end

function getindexoffset(ref byte p, int offset, index, t, &newoffset)ref byte=
!p, with intra-byte offset 'offset', forms a bit pointer to bit-type t
!step it by 'index' elements, and return a new byte-byte, and new offset

	index+:=offset

	case t
	when tu1 then
		p+:=index>>3				!add number of whole bytes
		newoffset:=index iand 7
	when tu2 then
		p+:=index>>2
		newoffset:=index iand 3
	when tu4 then
		index+:=offset>>2
		p+:=index>>1
		newoffset:=index iand 1
	end

	return p
end

proc obj_append_bits(object a, variant x)=
!do in-place append of b to list a
	int n, newoffset, elemtype
	ref byte q

	if a.objtype<>normal_obj then
		pcerror("Can't extend slice")
	fi

	if not a.mutable then
		pcnotmut()
	fi

	n:=a.length+1			!new length
	elemtype:=a.elemtag

	if n>a.alloc64 then		!need more space
		obj_resize_bits(a,n)
	else
		a.length:=n
	fi

	q:=getindexoffset(a.ptr, a.indexoffset, n-a.lower, elemtype, newoffset)
	var_storebit(q,newoffset*ttbitwidth[elemtype],x,elemtype, 0)
end

global proc var_appendto_bits(variant a, x)=
	obj_append_bits(a.objptr,x)
end

global proc obj_resize_bits(object p,int n)=
	ref byte q
	int newsize,elemtype

	elemtype:=p.elemtag

	if n<=p.alloc64 then
		p.length:=n
	else
		newsize:=getbitssize(n,elemtype)
		q:=pcm_alloc(newsize)
		if p.length then
			memcpy(q,p.ptr, bits_bytesize(p))
			pcm_free(p.ptr, getbitssize(p.alloc64, elemtype))
		fi
		p.ptr:=q
		p.length:=n
		p.alloc64:=allocbytes*(8/ttbitwidth[elemtype])
	fi
end

global proc var_dupl_bits(variant a)=
	object p,q
	int elemsize

	p:=a.objptr

	q:=obj_newbits(p.elemtag, p.lower, p.length)
	q.indexoffset:=p.indexoffset

	a.objptr:=q

	if p.length then
		memcpy(q.ptr, p.ptr, bits_bytesize(p))
	fi
end

global function var_equal_bits(variant a,b)int=
	object p:=a.objptr
	object q:=b.objptr
	int length,elemsize:=p.elemtag

	if p.elemtag<>q.elemtag then
		return 0
	fi
	length:=p.length

	if length<>q.length then
		return 0
	fi
	if length=0 then return 1 fi

	return eqbytes(p.ptr, q.ptr, bits_bytesize(p))
end

global proc var_concatto_bits(variant a,b)=
!do in-place append of b to array a
!both a,b must be arrays
!a must own its data
	ref byte d
	int n,alen,blen,newlen,oldbytes,newbytes,elemsize
	variant v
	object pa,pb

	PCERROR("VAR/BITS/NOT READY",$FUNCTION)
	pa:=a.objptr
	pb:=b.objptr

	if not pa.mutable then
		pcnotmut()
	fi

	if pa.elemtag<>pb.elemtag then
		pcerror("concat/not compat")
	fi
	elemsize:=ttsize[pa.elemtag]

	alen:=pa.length
	blen:=pb.length

	if alen=0 then					!concat to empty array
		if blen then				!copy b to a (else leave a as empty)
			obj_resize_bits(pa,blen)
			d:=pa.ptr
			memcpy(d,pb.ptr,blen*elemsize)
		fi
	elsif blen then					!neither array is empty (else leave a unchanged)
		newlen:=alen+blen
!		array_resize(pa,newlen)
		obj_resize_bits(pa,newlen)
		d:=pa.ptr+alen*elemsize
		memcpy(d,pb.ptr,blen*elemsize)
	fi
end

global proc var_getslice_bits(variant a, int i,j)=
	int alower,elemtype,newoffset
	object p,q

	p:=a.objptr

	alower:=p.lower
	elemtype:=p.elemtag

	if i<alower or j>p.length+alower-1 or i>j then
		pcerror("bits/slice bounds")
	fi

	q:=obj_new()

	q.objtype:=slice_obj
	q.mutable:=p.mutable
	q.lower:=1
	q.elemtag:=elemtype

	q.ptr:=getindexoffset(p.ptr, p.indexoffset, i-alower, elemtype, newoffset)
	q.indexoffset:=newoffset

	case p.objtype
	when slice_obj then				!slice of a slice!
		q.objptr2:=p.objptr2		!link to original
		obj_shareu(q.objptr2)

	when extslice_obj then
		q.objptr2:=nil
		q.objtype:=extslice_obj
	else
		q.objptr2:=p				!link to original
		++p.refcount
	esac

	q.length:=j-i+1
	a.objptr:=q
end

global function bits_bytesize(object p)int=
!return how many bytes are used by the object
!should be bits, but set should work; also array?

	return getbitssize(p.length, p.elemtag)
end

global function getbitssize(int n, t)int=
!return bytesize of n bit elements of type t
!will round to whole number of 64-bit values, but expressed as bytes
	int nbits:=n*ttbitwidth[t]
	return ((nbits-1)/64+1)*8			!bytes required in 64-bit blocks
end
=== qq_calldll.m 0 0 5/48 ===
global proc calldll(symbol d, variant args, result, int nargs)=
	symbol e
	const maxparams=100
	[maxparams]int arglist
	int n, retcode, retval,fnindex,libindex
	word dllinst
	ref proc fnaddr
	ichar name

!CPL "CALLDLL",D.NAME

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

	if d.mode in [tr64] then
		retcode:='R'
	else
		retcode:='I'
	fi

	fnaddr:=getlibprocaddr(d)

	retval:=os_calldllfunction(fnaddr, retcode, nargs,&arglist, nil)

	if d.mode then
		packedtovar(retval, d.mode, result)
	fi
end

func getlibprocaddr(symbol d)ref proc fnaddr=
	fnaddr:=dllprocaddr[d.index]

	if fnaddr=nil then
		fnaddr:=loaddllfunction(d)
	fi

	fnaddr
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
!			return word@(convtostringz(a.strptr,a.length))
			return word(convtostringz(a.strptr,a.length))
		when tint,treal,trefpack then
			return p.value
		else
			pcerror("Bad variadic param")
		esac
	fi

	t:=d.mode

	case ttbasetype[t]
	when ti64, tu64, ti32, tu32, ti16, tu16 then
		case s
		when tint, trefpack,trefvar then
			return p.value
		when treal then
			return int(p.xvalue)
		else
error:
			fprintln "'#' should be '#' (param # #)", strmode(s,1), strmode(t),d.name, d.index
			pcerror("DLL: wrong param type")
		esac

	when tr64 then
		case s
		when tint then
			return word@(real(p.value))
		when treal then
			return word@(p.xvalue)
		else
			error
		esac
	when tstringz then
		case s
		when tstring then
			a:=p.objptr
!			return word@(convtostringz(a.strptr,a.length))
			return word(convtostringz(a.strptr,a.length))
		when trefpack then
!			return word@(p.ptr)
			return word(p.ptr)
		else
			error
		esac
	when trefpack then
		case s
		when trefpack then
			return word(p.ptr)
		when tarray, tvector then
			return word(p.objptr.ptr)
		else
			error
		esac
	when tslice then
		return u64(ref byte(p.objptr)+8)		!point to (ptr, length) section

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

	case tbase
	when tvoid then
	when tr64 then
		dest.tagx:=treal
		dest.xvalue:=real@(retval)
	when tr32 then
		PCERROR("dll/r32ret")
	when ti64,tu64 then
		dest.tagx:=tint
		dest.value:=retval
	when ti32 then
		dest.tagx:=tint
		dest.value:=i32(retval)
	when tu32 then
		dest.tagx:=tint
		dest.value:=u32(retval)
	when ti16 then
		dest.tagx:=tint
		dest.value:=i16(retval)
	when tu16 then
		dest.tagx:=tint
		dest.value:=u16(retval)
	when trefpack then
		dest.tagx:=trefpack
		dest.ptr:=cast(retval)
		dest.elemtag:=tttarget[t]
	when tstringz then
		if retval then
			var_make_string(cast(retval), dest)
		else
			var_make_string("", dest)
		fi

	else
		pcerror("Rettype not supported:",ttname[t])
	esac
end

function loaddllfunction(symbol d)ref proc=
	int fnindex,libindex
	word dllinst
	ref proc fnaddr
	ichar name

	fnindex:=d.index
	fnaddr:=dllprocaddr[fnindex]
	return fnaddr when fnaddr

	libindex:=dllproclibindex[fnindex]
	dllinst:=dllinsttable[libindex]

	if dllinst=0 then
		dllinst:=os_getdllinst(libtable[libindex].name)
		if dllinst=0 then
			pcerror("Can't load DLL:",libtable[libindex].name)
		fi
		dllinsttable[libindex]:=dllinst
	fi

	name:=(d.truename|d.truename|d.name)
	fnaddr:=os_getdllprocaddr(dllinst,name)

	if fnaddr=nil then
		pcerror("Can't find DLL func:",name)
	fi
	dllprocaddr[fnindex]:=fnaddr

	return fnaddr
end

=== qq_decls.m 0 0 6/48 ===
!global const fixbytecodes=1		!convert bytecodes to handler addresses

global type unit      	= ref unitrec
global type object    	= ref objrec
global type symbol    	= ref strec
!global type strobject 	= ref stringobjrec
global type variant   	= ref varrec
global type ifile   	= ref filerec
global type isubprog  	= ref subprogrec

global macro pr(a,b)	= (a<<16 ior b)

global const hasrefmask = 0x100

global const varsize    = varrec.bytes

!global record packfieldrec =
!	object structobj			!owner record
!	ichar name
!	i32 packmode				!index into tables
!	i32 offset				!byte offset
!	i32 size					!size
!	i32 length
!end

global record procrec =			!used as linked list
	symbol def
	ref procrec nextproc
end

global record userxrec =
	symbol owner
	ref i16 pmode
	ref userxrec nextmode
end

global record strec =
	ichar name				! name (likely shared pointer with generic entry)
	symbol	owner
	symbol	deflist			! first child name
	symbol	deflistx		! points to last child

	symbol	nextdef			! next name in this list
	symbol	nextdupl		! next instance that shares the same name
	symbol	firstdupl		! first or generic name entry
!	union
		symbol alias		! used for aliasid
!		symbol captured		! localid: captured local in containing scope
!	end

	symbol nextstatic		! link all statics and procs
	symbol nextproc

	union
		u64 a
		pcl labelref			!procs/labels
		variant varptr			!statics
		ichar truename			!dll procs
		symbol atfield			!fields
		int labelno				!proc/label label# before fixup
	end
	union
		u64 b
		unit code				!proc body/initdata
		ref symbol topfieldlist		!structs; point to block of ttlength[mode] top fields
	end
	union
		u64 c
		struct
			i32 index			!frame/param/dllproc/enum/(const)
			i32 capindex		!localid index
		end
	end
	union
		u64 d
		struct
			i16 nparams		!procs/dllprocs
			i16 nlocals		!procs
!			i16 ncaptured		!anonprocs
		end
		struct
			i16 nfields		!records/structs
			i16 maxalign		!structs
!			i32 fieldoffset
			i16 fieldoffset
			i16 baseclassindex		!into baseclass tables
		end
		int genfieldindex		!generic
	end

	u16	subcode
	byte	moduleno
	byte	subprogno
	i16	mode
	i16	hint				!0/tvoid, or hinted mode when .mode=tvar
	u16		flags: (isglobal:2, isimport:1, mstatic:1, misfunc:1, mbyref:1,
							menumx:1,
							moptional:1,  mvarparams:1, isframe:1,
							iscaligned:1,initcode:2)
	byte	forindex		!1 when var is a for-loop index

	byte	symbolcode
	byte	nameid			! generic/static/proc etc

	byte	mutable			! will be 1 for variables; 0 for modules procs, label etc
	byte	namelen			! helps makes lookups faster
	byte	procfixed		! 1 when procs have been fixedup
end

global record lexrec =		!should be 32-byte record
	union
		i64 value				!64-bit int
		real xvalue				!64-bit float
		u64 uvalue			!64-bit word
		ichar svalue			!pointer to string or charconst (not terminated)
		ref strec symptr			!pointer to symbol table entry for name
	end

	i32 pos: (lineno:24, moduleno:8)

	byte symbol
	byte subcode
	u16 slength
end

global record uflagsrec =
	[7]byte	codes
	byte	ulength
end

global record fieldrec =
	ichar name
	i16 recordtype
	i16 fieldtype
	i32 fieldoffset
end

global record unitrec =
	union
		struct
			byte tag
			union
				byte elemtype			!for array constructors
				byte flag				! for incr/in/assign/for etc (see jtag docs)
				byte condcode			! for jcmp
				byte mathsop			! for jmaths/2
				byte pclop				! for junary/jbin/junaryto/jbinto
				byte loopcode			! loop_exit etc for exit/redo/next
				byte jsubcode			! access all above via one field
			end
			[2]byte spare
			i32 pos: (sourceoffset:24, moduleno:8)
		end
		ref void word1
	end

	unit nextunit

	union
		unit a
		symbol def
		symbol labeldef
		i64 value
		u64 uvalue
		r64 xvalue
		ichar svalue
		i64 range_lower
	end

	union
		unit b
		i64 range_upper
		i64 slength
		i16 mode
		[4]byte cmpconds				!for jcmpchain
		struct
			i32 length
			i32 lower
		end
		i64 index		!of enum name; or host index; or could be expr
	end
end

global lexrec nextlx
global lexrec lx
!global const targetbits=64

!global const maxsearchdirs=10
!global [maxsearchdirs]ichar searchdirs
!global int nsearchdirs=0

!global [5]ichar hostdirs
!global int nhostdirs

global int qpos
global int pcerrorpos
global ref filerec pcerrormodule

global const stacksize=70000
global [stacksize]varrec varstack
global variant sptr
global variant stacklimit
global ref byte frameptr

global pcl pcptr

global int stopped

global symbol stprogram			!root of global symbol table
global symbol stmodule			!main module
global symbol stsubprog
global symbol stcurrmodule		!current module during parse, name resolve, code gen
global symbol stcurrproc		!current proc during parse, rx/cocde, or
								! set to stcurrmodule when outside a proc
global ifile currmodule			!set via stcurrmodule.moduleno

global int debug

global int inproc

!Errors
!global [256]char errorline,errorpointer

global record locrec=
	isubprog sp             !owner sp
	ifile pm                !owner module
	symbol def				!if not nil, then containing proc, module etc
	ichar startline			!point to start of line in source
	int lineno              !line number within module
	i32 column				!if not zero, then column number
	i32 status				!1/0 = info exists/not available
end

!Genfield Tables

global record genfieldrec=
	symbol def
	ref genfieldrec nextdef
end

global const maxgenfield=1000
global [maxgenfield]ref genfieldrec genfieldtable
global int ngenfields

global const maxlibfile=50
global const maxdllproc=2000

global int nlibfiles
global [maxlibfile]symbol libtable
global [maxlibfile]byte libtypes
global [maxlibfile]u64 dllinsttable		!instance table

global int ndllprocs
global [maxdllproc]symbol dllproctable
global [maxdllproc]byte dllproclibindex				!lib that dll proc belongs to
global [maxdllproc]ref void dllprocaddr			!pointer to external dll proc

global byte usebundled	 = 1			!whether to use internal libs

global enumdata []ichar dispatchnames=
	(lab_dispatch,		"-lab"),
	(sw_dispatch,		"-sw"),
	(fn_dispatch,		"-fn"),
	(debug_dispatch,	"-debug"),
	(fdebug_dispatch,	"-fdebug"),
	(asm_dispatch,		"-asm"),
end

global const int maxqparam=32
global int nqparams
global [maxqparam]ichar qparamtable

!global ichar err_message
!global varrec err_var1, err_var2
!global ref int err_pcptr

!global ref int stopseq		!point to a 'stop 0' sequence
!global ref int raiseseq		!point to a sequence of several 'raise' cmdcodes

global ref procrec proclist, proclistx
global int nproclist

global ref proc pcl_callbackfn=nil	!address of *PCL* function (pcdata address)

global [0..255]object chrtable		!remember single-character objects

global byte fnosys
global byte fverbose

global [0:256]i16 baseclasstable
global [0:256]ref strec baseclassdef
global int nbaseclasses

global int lastretindex

global const maxsubprog=30
global const maxmodule=200

global record filerec=
	ichar	name				!module name and base filename ("<str>" is anon)
	ichar	path				!path where file resides
	ichar	filespec			!full file path
	ichar	text				!pointer to source text, 0-terminated
	int		size				!source file size includes terminator

	byte	isstring			!1 if a string rather than a file
	byte	issyslib			!1 if a system module
	byte	issupport			!1 if a support file (strinclude); MAY BE STORED ELSEWHERE
	byte	compiled			!1 if compiled

!	i16	subprogno
	byte	subprogno
	byte	islead				!1 if lead module in sp
	union
		i16	moduleno			!useful if using pointer to a source rec
		i16	fileno
	end

	unit	ast					!ast for module-level code

	pcl		pcstart				!nil, or points to generated bytecode for whole module
	pcl		pcend				!points to last allocated pcl rec
	int		pcsize				!pcl size as number of allocated ints (some spare)
	ref i32	pcsourcestart		!each entry is source-pos info (char offset into org source)

	union
		symbol	stmodule
		symbol	def
	end

	symbol	stsubprog
!	symbol	stmacro

	symbol	startfn				!nil, or st entry of start()
	symbol	mainfn				!nil, or st entry of main()
end

global record subprogrec =
	ichar name
	ichar path
	ichar filespec
	i16 firstmodule			!1st is lead module
	i16 lastmodule			!always first..lastmodule
	i16 compiled				!1 if compiled
	byte issyslib
	byte subprogno
end

global [0..maxmodule]ifile	modules
global [maxsubprog]isubprog	subprogs

global int nmodules
global int nsubprogs

global record varrec =
	union
		struct
			union
				struct
					byte	tag
					byte	hasref
					byte	bitoffset
					union
						byte	bitlength		!for refbit/tbit: 0=1 bit, N=bitfield
						byte	exceptiontype
!						byte	genmarker		!1 means is a generator used as a marker
					end
				end
				u32		tagx
			end
			union
				u32 		elemtag
				u32 		frameptr_low
				struct
					i16		frameoffset
					i16		nexceptions
				end
			end
		end
		i64 dummy: (skip:16, range_lower:48)
	end
	union
		i64		value
		r64		xvalue
		u64		uvalue
		u64		range_upper
		object		objptr				!objects where hasref=1
		variant		varptr				!for refvar
		ref byte	ptr					!for refproc etc
		symbol		def					!for tsymbol
		pcl			retaddr
		pcl			labelref
	end
end

global record objrec =
!1st 8 bytes
	u32 refcount
	struct
		byte flags: (lower:1, mutable:1, bittag:2)
		byte objtype
		union
			u16 elemtag
			u16 usertag
			u16 itertag
			struct
				byte bitoffset
				byte indexoffset
			end
			i16 lower16
!			i16 iterended		!0/1 = running/ended
		end
	end

!second 8 bytes (and end of short objects)
	union
		struct
			union
				i64		value
				r64		xvalue
				u64		uvalue
				ichar		strptr
				variant		varptr
				variant		genstack
				ref byte	ptr
				ref[0:]elemtype num
				u64 b
				ref int		retaddr
			end

!3rd 8 bytes
			union
				i64 length
				i64 lower64
				struct
					u32 rows
					u32 columns
				end
				u64 c
				ref byte frameptr
!				symbol		stgen
				struct
					i32 iterpos
					i32 iterupper
				end
			end

!4th 8 bytes (and end of long objects)
			union
				i64 alloc64				!object/item counts, not bytes
				object objptr2
				struct
					i16 neg
					i16 numtype
					i32 expon
				end
				struct
					u32 alloc32
					u32 dictitems
				end
				struct
					u16		genstacksize		!in varrecs
					byte	ngenparams			!params to gen func
				end
				u64 d
			end
		end
		[24]byte bignumdescr
	end
end

global int nalllines

!global const devdir = "c:/qx/"
!global const devdir = "c:/bx/"
global const devdir = "c:/qx/"

!QA files
global const maxqafile=100
global [maxqafile]ichar qafilenames
global [maxqafile]ichar qatext
global [maxqafile]int qasize
global int nqafiles					!non-0 means qa directory in use.

global enumdata []ichar optionnames, []ref byte optionvars, []byte optionvalues =
	(load_sw,		"load",			&runcode,			load_cc),
	(parse_sw,		"parse",		&runcode,			parse_cc),
	(names_sw,		"names",		&runcode,			names_cc),
	(gen_sw,		"gen",			&runcode,			gencode_cc),
	(fixup_sw,		"fixup",		&runcode,			fixup_cc),
	(run_sw,		"run",			&runcode,			run_cc),

	(ast1_sw,		"ast1",			&fshowast1,			1),
	(ast2_sw,		"ast2",			&fshowast2,			1),

	(pcl1_sw,		"pcl1",			&fshowpcl1,			1),
	(pcl2_sw,		"pcl2",			&fshowpcl2,			1),

	(allsp_sw,		"allsp",		&fallsp,			1),

	(st_sw,			"st",			&fshowst,			1),
	(stflat_sw,		"stflat",		&fshowstflat,		1),
	(types_sw,		"types",		&fshowtypes,		1),
	(showmodules_sw,"modules",		&fshowmodules,		1),

	(opt_sw,		"opt",			&foptimise,			1),
	(noopt_sw,		"no",			&foptimise,			0),

	(ext_sw,		"ext",			&usebundled,		0),
	(qa_sw,			"qa",			&fwriteqa,			1),
	(qas_sw,		"qas",			&fwriteqa,			2),

	(verbose_sw,	"v",			&fverbose,			1),
!	(time_sw,		"time",			&fshowtime,			1),

	(nosys_sw,		"nosys",		&fnosys,			1),
	(sys_sw,		"sys",			&fnosys,			0),
end

!GLOBAL INT NUNITS
!GLOBAL INT NPCL

global pcl stopseq		!point to a 'stop 0' sequence
global pcl raiseseq		!point to a sequence of several 'raise' cmdcodes


!temporarily moved from pclgen etc

global int nproclocals			!no. of locals
global pcl pproclocals			!pointer to kprocent pcl op (for updating locals for avs)

global [pclnames.lwb..pclnames.upb]int pclcounts

GLOBAL INT NALLPCL
=== qq_decimal.m 0 0 7/48 ===
const digitwidth   = 9
const digitbase	= 1000000000
const digitfmt	 = "%09d"
const mdigitfmt	 = "z9"

const digitmax	 = digitbase-1

global type elemtype = i32
global const decelemsize = elemtype.bytes

record constrec =
	i64 value
	object bnvalue
	ref constrec nextconst
end

!special values for bignum types
enumdata [0:]ichar fpnames =
	(zero_type = 0,		$),
	(normal_type,		$),
	(inf_type,			$),
	(nan_type,			$),
end

!recognised combinations of bignum types (bintypes)
enumdata =
	nn_types,	 	  ! both numbers (non-zero)
	zz_types,	 	  ! both zero
	ii_types,	 	  ! both infinity
	xx_types,	 	  ! one or both is nan

	nz_types,	 	  ! number/zero
	ni_types,	 	  ! number/infinity

	zn_types,	 	  ! zero/number
	in_types,	 	  ! infinity/number

	zi_types,	 	  ! zero/infinity
	iz_types	 	  ! infinity/zero
end

const maxprec	   = 10 million
!int currprec	   = 100/digitwidth
!int currprec	   = 300/digitwidth

int currprec	   = 500/digitwidth
!int currprec	   = 100'000'000/digitwidth
!!int currprec	   = 1000

int stblz	 	 	 !global set by smalltobig

ref constrec constlist=nil		!use linked list of constant values
global int decstrsize
varrec vtemp					!as used in free/dectemp

macro bn_free(x) = obj_free_dec(x)

global proc obj_free_dec(object p)=
	if p.length then
		pcm_free(p.num, p.length*decelemsize)
	fi

	pcm_free32(p)
end

global proc var_dupl_dec(variant a)=
!NOTE: because decimals are immutable, there may never be any need
!to duplicate a decimal; it can always be shared
!But I've done it now so...

	object p,q
	int size

	q:=a.objptr
	p:=obj_new()
	p.length:=q.length
	p.expon:=q.expon
	p.neg:=q.neg
	p.numtype:=q.numtype

	size:=q.length*decelemsize

	if size then
		p.num:=pcm_alloc(size)
		memcpy(p.num,q.num,size)
	fi

	a.objptr:=p
end

global proc var_empty_dec(variant dest)=
	dest.tagx:=tdecimal+hasrefmask
	dest.objptr:=makebignum(0)
end

global proc var_make_dec_str(ichar s, int length, variant dest)=
	dest.tagx:=tdecimal ior hasrefmask
	dest.objptr:=bn_makestr(s,length)
end

global proc var_make_dec_int(int a, variant dest)=
	dest.tagx:=tdecimal ior hasrefmask
	dest.objptr:=bn_makeint(a)
end

function badnumber:object c=
	c:=makebignum(0)
	c.numtype:=nan_type
	return c
end

function bn_makestr(ichar s, int length=0)object=
	ichar t,u,oldt
	int tlength
	int neg,dpindex,expon,nonzeros,talloc,dpseen
	int leadingzeros, trailingzeros,zerosafterdp
	int d,n,wd,dp,wdp,w,d2,na,nb, c
	object a

	if length=0 then
		length:=strlen(s)
	fi
	if length<=0 then
		return badnumber()
	fi

	t:=malloc(length+1)
	memcpy(t,s,length)
	(t+length)^:=0
	oldt:=t
	tlength:=length+1
	s:=t

	talloc:=length+1+10	 	!allow for extending last wdigit group

	neg:=0
	case s^
	when '+' then ++s
	when '-' then neg:=1; ++s
	esac

	t:=u:=pcm_alloc(talloc)	  !accummulate sig digits into t
	dpindex:=-1
	dpseen:=zerosafterdp:=0
	nonzeros:=0
	leadingzeros:=trailingzeros:=0
	expon:=0

!CPL =S

	do
		if (c:=s^) in '1'..'9' then
			u++^:=s++^
			trailingzeros:=0
			nonzeros:=1
		elsecase c
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
		end
	end

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
!		return obj_make_dec_int(0)
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
	a.neg:=neg
	a.expon:=wd
	u:=t
	a.num[0]:=strvaln(u,na)
	u+:=na
	
	for i:=1 to length-1 do
		a.num[i]:=strvaln(u,w)
		u+:=w
	od

	pcm_free(t,talloc)
	free(oldt)

	return a
end

function readexpon(ichar s)int=
!s points just after 'e' or 'E'
	int neg, expon, c
	neg:=expon:=0

	case s^
	when '+' then ++s
	when '-' then neg:=1; ++s
	esac

	do
		if (c:=s^) in '0'..'9' then
			expon:=expon*10+(s^-'0')
			++s
		elsecase c
		when '_', '\'', '`', ' ' then
			++s
		when 0 then
			exit
		else
			pcerror("make expon?")
		end
	end

	return (neg|-expon|expon)
end

function bn_makeint(int x)object a=
	[256]char str

	if x=0 then
		a:=makebignum(0)
	elsif x in 0..digitmax then
		a:=makebignum(1)
		a.num[0]:=x
	elsif -x in 0..digitmax then
		a:=makebignum(1)
		a.num[0]:=-x
		a.neg:=1
	else
		print @str,x
		a:=bn_makestr(str)
	fi

	return a
end

global function var_tostr_dec(variant a,int fmt)ichar=
	return obj_tostr_dec(a.objptr,fmt)
end

function obj_tostr_dec(object a,int fmt=0)ichar=
	int expon,upper
	ichar s,t

	t:=nil
	if a=nil then
		t:="<void>"
	else
		case a.numtype
		when zero_type then t:=(fmt='E' or fmt='F'|"0.0"|"0")
!		when inf_type then t:=(a.neg|"<-infinity>"|"<infinity>")
		when inf_type then t:=(a.neg|"-Infinity"|"Infinity")
		when nan_type then t:="<NaN>"
		esac
	fi

	if t then
		s:=pcm_alloc(decstrsize:=strlen(t)+1)
		strcpy(s,t)
		return s
	fi

	if fmt=0 or fmt='A' then
		if bn_isint(a) and (a.expon-a.length)*digitwidth<60 then
	 	   fmt:='I'
		elsif abs(a.expon*digitwidth)<60 then
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

function tostring_scient(object a)ichar=
!a is an actual number
	ichar s,t
	int expon,nchars,n,shift
	i64 x,scale

	nchars:=3

	expon:=a.expon*digitwidth

	x:=a.num[0]
	scale:=1
	shift:=0
	while x>=10 do
		x:=x/10
		scale*:=10
		++expon
		++shift
	od

	nchars:=a.length*digitwidth+16	 !allow for 1., and exponent

	s:=t:=pcm_alloc(decstrsize:=nchars)

	if a.neg then
		t++^:='-'
	fi

	print @t,x,,"."
	t+:=strlen(t)

	if shift then
		print @t, shift:"v",,a.num[0]-x*scale:"z*"
		t+:=strlen(t)
	fi

	for i to a.length-1 do
		print @t,a.num[i]:mdigitfmt
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

function tostring_float(object a,int fmt)ichar=
!a is an actual number (not zero, infinity etc)
	int expon,upper,nchars,w,prel,n,showdot
	ichar s,t

	expon:=a.expon
	upper:=a.length-1

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
	nchars+:=a.length*w
	if expon-upper>0 then
		nchars+:=(expon-upper)*w
	fi
	nchars+:=8	 	 		!margin

!   s:=t:=bn_alloc(nchars)
	s:=t:=pcm_alloc(decstrsize:=nchars)
	
	if a.neg then
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
		print @t, a.num[i]:(i>0 or prel|mdigitfmt|"")
		t+:=strlen(t)
		

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

function bn_isint(object a)int =
	return a.length<=a.expon+1
end

global function obj_len_dec(object a)int=
!return number of digits in integer a

RETURN BN_GETPREC(A)

	if not bn_isint(a) then
		return 0
	fi
	if BN_iszero(a) then
		return 1
	fi

	return strlen(strint(a.num[0]))+a.expon*digitwidth
end

global function bn_iszero(object a)int=
	return a.numtype=zero_type
end

global function var_equal_dec(variant a,b)int=
	return bn_equal(a.objptr, b.objptr)
end

global proc var_add_dec(variant a,b)=
	object dest:=bn_init()

	bn_add(dest, a.objptr,b.objptr)
	a.objptr:=dest
end

global proc var_sub_dec(variant a,b)=
	object dest:=bn_init()

	bn_sub(dest, a.objptr,b.objptr)
	a.objptr:=dest
end

global proc var_mul_dec(variant a,b)=
	object dest:=bn_init()

	bn_mul(dest, a.objptr,b.objptr)
	a.objptr:=dest
end

global proc var_div_dec(variant a,b)=
	object dest:=bn_init()

	bn_div(dest, a.objptr,b.objptr)
	a.objptr:=dest
end

global proc var_idiv_dec(variant a,b)=
	object dest:=bn_init()

	bn_idiv(dest, a.objptr,b.objptr)
	a.objptr:=dest
end

global proc var_irem_dec(variant a,b)=
	object dest:=bn_init()

	bn_irem(dest, a.objptr,b.objptr)
	a.objptr:=dest
end

global proc var_neg_dec(variant a)=
!-:=a
!do in place neg; caller should have duplicated value

	bn_negto(a.objptr)
end

global proc var_abs_dec(variant a)=
!abs:=a
!do in place abs; caller should have duplicated value

	bn_absto(a.objptr)
end

global function var_compare_dec(variant a,b)int=
	return bn_cmp(a.objptr, b.objptr)
end

function bn_cmp(object a,b)int=
	object d
	int neg

	if bn_equal(a,b) then
		return 0
	fi

	d:=bn_init()
	bn_sub(d,a,b)
	neg:=d.neg
	bn_free(d)
	return (neg|-1|1)
end

function bn_equal(object a,b)int=
	if a.numtype<>normal_type and a.numtype=b.numtype then
		return a.neg=b.neg
	fi

	if a.length<>b.length or 
	   a.numtype<>b.numtype or 
	   a.neg<>b.neg or 
	   a.expon<>b.expon then
		return 0
	fi

	if a.length=0 then return 1 fi

	return eqbytes(a.num,b.num,a.length*decelemsize)
end

function bn_add(object dest,a,b)int=
	int nega,negb

	case getbintype(a,b)
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
	esac

	nega:=a.neg
	negb:=b.neg

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

function bn_sub(object dest,a,b)int=
	int nega,negb

	case getbintype(a,b)
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
	esac

	nega:=a.neg
	negb:=b.neg

	if not nega and not negb then		!both positive
		bn_subu(dest,a,b)
	elsif nega and negb then			!both negative
		bn_subu(dest,b,a)
	elsif not nega and negb then		!a positive, b negative
		bn_addu(dest,a,b)
	else								!a negative, b positive
		bn_dupl(dest,b)
		bn_negto(dest)
		bn_add(dest,a,dest)
	fi

	return 1
end

proc bn_addu(object dest,a,b)=
	int preca, precb, precc
	int uppera,upperb,upperc, offset, carry,expona,exponb
	int dc
	word j
	ref[0:]elemtype pa,pb
	ref elemtype pax,pbx
	ref elemtype c,c2

	if a.expon<b.expon then	   !A has definite smaller magnitude
		swap(a,b)	 	 	 	!make sure A is always bigger or (approx) equal
	fi

	expona:=a.expon
	exponb:=b.expon
	preca:=a.length
	precb:=b.length

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
	pa:=a.num
	pb:=b.num

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
		memcpy(c2+1,c,precc*decelemsize)
		freesmall(c,precc)
		c:=c2
		++precc
	fi

	smalltobig(dest,c,precc,precc)

	dest.expon:=expona+carry
end

proc bn_subu(object dest,a,b)=
	int preca, precb, precc
	int uppera,upperb,upperc, offset, carry, expona
	int da,db,dc, isneg, z, newprec,diff
	word j
	ref[0:]elemtype pa,pb
	ref elemtype c

!can only do subtract when a>=b; do some basic checks
	isneg:=0
	if a.expon<b.expon then	   !A has definite smaller magnitude
		swap(a,b)	 	 	 	!make sure A is always bigger or (approx) equal
		isneg:=1
	fi

!know that a>=b, and that isneg might be true
retry:
	expona:=a.expon
	preca:=a.length
	precb:=b.length

	offset:=expona-b.expon	 	!for indexing B elements shift to match A
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
	pa:=a.num
	pb:=b.num

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
	 	   pcerror("SUBU/CARRY")
		fi
		swap(a,b)
		isneg:=1
		freesmall(c,precc)
		goto retry
	fi

	smalltobig(dest,c,precc,precc)
	dest.neg:=isneg
	dest.expon:=expona-stblz

end

function makebignum(int length)object=
!ndigits=0 to create a zero value
!these are wide digits
	object a

!	a:=pcm_alloc(objrec.bytes)
	a:=obj_new()
	if length then
		a.num:=pcm_alloc(length*decelemsize)
		a.numtype:=normal_type
	else
		a.num:=nil
		a.numtype:=zero_type
	fi
	a.length:=length

	a.expon:=0
	a.neg:=0

	return a
end

function makesmallnum(int length)ref elemtype=
	return pcm_alloc(length*decelemsize)
end

function smalltobig(object c, ref elemtype a, int length,alloc,offset=0)object =
!copy numeric data from smallnum into new object
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
	 	   c.num:=cast(a)
		else
	 	   c.num:=cast(makesmallnum(newlength))
	 	   memcpy(c.num,a+leadingzeros,newlength*decelemsize)
	 	   freesmall(a+offset,alloc)
		fi
		c.length:=newlength
		c.numtype:=normal_type
		c.expon:=length-1-leadingzeros	 		!include trailing zeros, but not leading ones?
	elsif alloc then	 	 	 	 	 	 	  !result stays at zero
		freesmall(a+offset,alloc)
	fi

	return c
end

proc freesmall(ref elemtype p, int length)=
	pcm_free(p,length*decelemsize)
end

global function bn_init()object=
	object a

	a:=makebignum(0)
	return a
end

proc bn_setzero(object a)=
!clear digit memory only; clear descriptor to a zero number
	if a then
		if a.num then
	 	   freesmall(cast(a.num),a.length)
		fi
		a.num:=nil
		a.length:=0
		a.neg:=0
		a.expon:=0
		a.numtype:=zero_type
	fi
end

proc bn_move(object a,b)=
!move contents of b to a. Original value of a is cleared; b becomes zero

	bn_setzero(a)
	a.bignumdescr:=b.bignumdescr
	clear b.bignumdescr
end

proc bn_dupl(object a,b)=
!copy contents of b to a. Each copy is independent
	object c
	int size

	c:=bn_init()
	c^:=b^
	if c.length then
		c.num:=cast(makesmallnum(size:=c.length))
		memcpy(c.num,b.num, size*decelemsize)
	fi
	bn_move(a,c)
	bn_free(c)
end

proc bn_setinf(object dest) =
	bn_setzero(dest)
	dest.numtype:=inf_type
end

proc bn_setnan(object dest) =
	bn_setzero(dest)
	dest.numtype:=nan_type
end

global proc var_setnan(variant dest) =
	dest.tagx:=tdecimal+hasrefmask
	dest.objptr:=makebignum(0)
	bn_setnan(dest.objptr)
end

global proc var_setinf(variant dest) =
	dest.tagx:=tdecimal+hasrefmask
	dest.objptr:=makebignum(0)
	bn_setinf(dest.objptr)
end

function getbintype(object a,b)int=
!return bintype code for combination of a and b
	int atype:=a.numtype, btype:=b.numtype

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

proc bn_negto(object a)=
	if not bn_iszero(a) then
		a.neg:=not a.neg
	fi
end

proc bn_absto(object a)=
	a.neg:=0
end

function bn_mul(object dest,a,b)int=
	int neg

	case getbintype(a,b)
	when nn_types then
	when zz_types,nz_types,zn_types then
		bn_setzero(dest)
		return 1
	else
		bn_setnan(dest)
		return 0
	esac

	neg:=a.neg<>b.neg
	bn_mulu(dest,a,b)
	if neg then	 !different signs
		bn_negto(dest)
	fi
	return 1
end

function bn_mulp(object dest,a,b, int prec)int=
	int res:=bn_mul(dest,a,b)
	if res then
		bn_setprec(dest,(prec=0|currprec|prec))
	fi
	return res
end

proc bn_mulu(object dest, a,b) =
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
	int i,cx1, nc2, pd,pr
	i64 p,carry,x
	object d
	ref elemtype c
	i64 pdquot,pdrem

	expona:=a.expon
	exponb:=b.expon
	uppera:=a.length-1
	upperb:=b.length-1

	precc:=uppera+upperb+2
	nc2:=precc

!++NMULT

	c:=makesmallnum(nc2)
	memset(c,0,precc*decelemsize)
!   c.expon:=a.expon+b.expon+1
	cx:=precc-1

	for bx:=upperb downto 0 do
		carry:=0

		cx1:=cx
		for ax:=uppera downto 0 do
			p:=i64((a.num[ax]))*i64((b.num[bx]))+carry

			pd:=p/digitbase

!			assem
!				mov rdx,[p]
!				mov rcx, 19342813113834067
!				shr rdx,9
!				mov rax,rdx
!				mul rcx
!				xor eax,eax
!				shr rdx,11
!				mov [pd],rdx
!			end
!			pd:=p/digitbase

			pr:=p-pd*digitbase
!
!			x:=i64((c+cx1)^)+p rem digitbase
			x:=i64((c+cx1)^)+pr

			if x>digitmax then
				carry := pd+1
				(c+cx1--)^ := x-digitbase
			else
				carry:=pd
				(c+cx1--)^:=x
			fi

		od
		(c+cx1)^:=carry
		--cx	 	 	  !for next row, start at next column in dest
	od

	smalltobig(dest,c,precc,nc2)
	dest.expon:=expona+exponb+1-stblz

end

function smallmulto(ref elemtype p,q, int plen, m)int=
!multiply object sequence p inplace, by single digit m
!return new length (will be plen or plen+1, unless result is zero)
!p must be long enough to store the extra digit

	ref elemtype pp,qq
	int carry,d

	case m
	when 0 then
		p^:=0
		return 1
	when 1 then
		memcpy(p,q,plen*decelemsize)
		return plen
	esac

	pp:=p+plen-1
	qq:=q+plen-1
	carry:=0

	to plen do
		d:=i64(qq^)*m+carry
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

function bn_div(object dest,a,b,int prec=0)int=
	int neg

	case getbintype(a,b)
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
	esac

	neg:=a.neg<>b.neg

	bn_fdivu(dest,a,b,prec)

	if neg then
		bn_negto(dest)
	fi
	return 1
end

function bn_idiv(object dest,a,b)int=
	int neg
	case getbintype(a,b)
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
	esac

	neg:=a.neg<>b.neg
	bn_idivu(dest,a,b)
	if neg then
		bn_negto(dest)
	fi
	return 1
end

function bn_idivrem(object dest,rm,a,b)int=
	int nega,negb

	case getbintype(a,b)
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
	esac

	nega:=a.neg
	negb:=b.neg
	bn_idivu(dest,a,b,rm)
	if nega<>negb then	  !different signs
		bn_negto(dest)
	fi
	if nega then bn_negto(rm) fi
	return 1
end

function bn_irem(object dest,a,b)int=
	object rm,d
	int nega

	case getbintype(a,b)
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
	esac

	nega:=a.neg
	d:=bn_init()
	bn_idivu(d,a,b,dest)
	if nega then bn_negto(dest) fi
	bn_free(d)
	return 1
end

proc bn_idivu(object dest,a,b,rm=nil)=
!neither a nor b are zero; both are positive
!integer divide

	ref elemtype c,x,e
	int expona, exponb, badjust, exponc
	int na,nb,nc,nx,ne,nx2,ne2, cx,nupper
	int uppera, upperb, upperc
	int n, k, nexta
	i64 xx,y
	ref elemtype pa,pb

	na:=a.length
	nb:=b.length
	expona:=a.expon
	exponb:=b.expon
	badjust:=exponb+1-nb

	if na>expona+1 or nb>exponb+1 then
		pcerror("idivu:a or b not int")
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
	pa:=cast(a.num)
	pb:=cast(b.num)	 	   !p is not zero, and all digits of interest are present

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
		object d
		d:=bn_init()
		bn_mulu(d,b,dest)
		bn_subu(rm,a,d)
		bn_free(d)
	fi

end

proc bn_fdivu(object dest,a,b,int precision)=
!neither a nor b are zero; both are positive
!integer divide

	ref elemtype c,x,e
	int expona, exponb, badjust, exponc
	int na,nb,nc,nx,ne,nx2,ne2, cx,nupper,nc2
	int uppera, upperb, upperc
	int n, k, nexta
	i64 xx,y
	ref elemtype pa,pb

	na:=a.length
	nb:=b.length
	expona:=a.expon
	exponb:=b.expon

	if precision then
		precision:=((precision-1)/digitwidth+1)	 	!must be multiple of digitwidth
	else
		precision:=currprec
	fi
	nc:=precision

	uppera:=na-1
	upperb:=nb-1
	upperc:=nc-1
	pa:=cast(a.num)
	pb:=cast(b.num)	 	   !p is not zero, and all digits of interest are present

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
		dest.expon:=expona-exponb-1
	else
		smalltobig(dest,c,cx,nc2)
		dest.expon:=expona-exponb
	fi
!   freesmall(c,nc2)
end

function smalldiv(ref elemtype x, b, int &xlen, nb)int =
!x,b are smallnums: arrays of elements, of the exact lengths given
!x is same length as b, or at most one element longer
!(x can also be smaller, but then result is just 0)
!return integer x/b as machine word type 0..digitmax
!when digits are 0..9, then result of x/b is always going to be 0 to 9.

	int k,count
	i64 xx,y
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
	 	   xx:=i64(x^)*digitbase+i64((x+1)^)
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
	 	   pcerror("smalldiv:Y=0")
		fi
	od

	freesmall(e,esize)
	xlen:=nx	 	 	 	 !return modified x, and new length of x
	return k
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
	if carry then pcerror("SSUBTO/CARRY?") fi

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

function bn_getprec(object a)int=
	return a.length*digitwidth
end

proc bn_setprec(object a,int prec)=
	int oldlength,newlength
	object c

	if a.numtype<>normal_type then
		return
	fi

	if prec<1 or prec>maxprec then
		return
	fi

!prec is digit count, not words
	prec:=((prec-1)/digitwidth+1)*digitwidth		!must be multiple of digitwidth

!prec should be rounded up as needed to next multiple of digitwith
	newlength:=prec/digitwidth	 	 	 	   !no. words

	oldlength:=a.length

	if oldlength<=newlength then
		return
	fi

	c:=makebignum(newlength)
	c.neg:=a.neg
	c.expon:=a.expon

	for i:=0 to newlength-1 do
		if i<oldlength then
	 	   c.num[i]:=a.num[i]
		else
	 	   c.num[i]:=0
		fi
	od

	bn_move(a,c)
	bn_free(c)
end

function bn_getglobalprec:int=
	return currprec*digitwidth
end

proc bn_setglobalprec(int prec)=
	currprec:=((prec-1)/digitwidth+1)
end

function bn_makefloat(r64 x)object =
	object a
	[2048]char str

!	sprintf(&.str,"%.15g",x)

	print @str, x:".15g"

	return bn_makestr(&.str)
end

global function dectemp(variant a)variant=
!turn int/real into temporary 
	vtemp.tagx:=tdecimal+hasrefmask

	case a.tag
	when tint then
		vtemp.objptr:=bn_makeint(a.value)
	when treal then
		vtemp.objptr:=bn_makefloat(a.xvalue)
	else
		pcerror("dectemp")
	esac
	a^:=vtemp
	return a
end

global proc freedectemp=
	bn_free(vtemp.objptr)
end

proc bn_ipower(object d, a,i64 n)=
!return a**b for bigints
	object e,f

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

global proc var_power_dec(variant a, int n)=
	object dest:=bn_init()
	bn_ipower(dest,a.objptr,n)
	a.objptr:=dest
end

global function var_convert_dec_int(variant a)int=
!CPL "CONVINT"
	return bn_toint(a.objptr)
end

function bn_toint(object a)i64=
	i64 x
	if not bn_isint(a) then
		pcerror("dec-float->int not ready")
		return 0
	fi
	if bn_iszero(a) then
		return 0
	fi

	x:=0
!	if a.expon>2 or a.expon=2 and a.num[2]>9 then
!		pcerror("dec->int overflow")
!	fi

	for i:=0 to a.length-1 do
		x:=x*digitbase+a.num[i]
	od

	for i:=a.length to a.expon do
		x*:=digitbase
	od

	if a.neg then
		return -x
	else
		return x
	fi
end

=== qq_dicts.m 0 0 8/48 ===
global proc var_make_dict(variant a, dest, int n) =
!create a list of n vars starting from a in reverse order (a is the last)
!put the result in dest (note this will be the last/first of the n vars)
	object p
	variant b
	varrec v

	p:=obj_new_dict(n)

!CPL "NEWDICT",N,P.LENGTH
	b:=p.varptr
	v.tagx:=tdict+hasrefmask
	v.objptr:=p

	to n do
		adddictitem(&v,a,a+1)
		a+:=2
	od
	p.dictitems:=n

	dest^:=v
end

global function obj_new_dict(int n)object p=
	int m

	m:=max(16,nextpoweroftwo(n*2))		!list has 2n entries, min 16, rounded up to 2**x

	p:=obj_newlist(m,1,nil)
	p.dictitems:=0

	return p
end

global proc obj_free_dict(object p,int internal=0)=
!internal=1 means called from expanddict; free only elements, not descriptor
	variant q
	varrec v

	q:=p.varptr
	to p.length do
		var_unshare(q)
		++q
	od
	if p.length then
		pcm_free(p.varptr,p.alloc32*varrec.bytes)
	fi

	if not internal then
		pcm_free32(p)
	fi
end

global proc var_dupl_dict(variant a)=
	object p,q
	variant plist, qlist

	p:=a.objptr
	q:=obj_new_dict(p.dictitems)
	q^:=p^
	q.refcount:=1
	q.mutable:=1

	a.objptr:=q

	if q.length=0 then return fi

	qlist:=q.varptr:=pcm_alloc(p.length*varrec.bytes)
	q.alloc32:=allocbytes/varrec.bytes	!probably same as p.alloc32	
	plist:=p.varptr

	to q.length do
		qlist^:=plist^
		var_dupl(qlist)
		++qlist
		++plist
	od
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
!	xlen:=px.length
!	ylen:=py.length
!
!	if xlen<>ylen then return 0 fi		!unequal lengths
!
!	if xlen=0 then return 1 fi			!both empty
!
!	a:=px.varptr
!	b:=py.varptr
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
!when key not found:
!   doins=1:     Will insert the key and a void value, and return a pointer to the value
!   doins=0:     Will return nil

	int hash,index,size,keytag,wrapped,limit
	i64 keyvalue
	variant q
	object pa,qa,d

	retry:
	d:=vd.objptr

	size:=d.length/2

	index:=(var_gethashvalue(p) iand (size-1))		!0-based index

	q:=d.varptr+index*2							!point to key of key/value pair
	wrapped:=0
	keytag:=p.tag
	keyvalue:=p.value							!when int
	pa:=p.objptr								!when string

	do
!CPL "FD1"
		if q.tag=tvoid then					!unused entry; not found
			exit

		elsif q.tag=keytag then
			case keytag
			when tint,treal,trange then
				if q.value=keyvalue then
					++q
					var_share(q)
					return q
				fi
			when tstring then
				qa:=q.objptr
				if pa.length=qa.length then	!match on length at least
					if memcmp(pa.strptr,qa.strptr,pa.length)=0 then
						++q
						var_share(q)
						return q
					fi
				fi
			when trecord then
				if var_equal_record(p, q) then
					++q
					var_share(q)
					return q
				fi
!			else
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
			q:=d.varptr
		fi
	od

!exit when not found
	if doins then
		limit:=size*3/4
		if d.dictitems>=limit then
			expanddict(vd)
			goto retry
		fi
		q^:=p^
		var_share(q)
		++(d.dictitems)
		return q+1							!point to entry; leave value as void
	else
		return nil
	fi
end

proc expanddict(variant vd)=
!double the size of the dict
	int n,m,i,j,k,oldrefcount
	object d,e
	objrec temp
	variant p,q,r
	varrec ev
	static byte inuse

	if inuse then
		pcerror("expanddict?")
	fi
	inuse:=1

	d:=vd.objptr

	n:=d.alloc32			!nos of keys and values (all slots)
	m:=n/2					!number of dict slots

	p:=d.varptr							!old data

	e:=obj_new_dict(m*2)
!	e:=obj_new_dict(m*4)

	var_objtovar(tdict,e,&ev)

	q:=p

	for i:=1 to m do
		if q.tag<>tvoid then
			r:=var_finddictitem(&ev,q,1)
			++q
			r^:=q++^					!transfer ownership of data
VAR_SHARE(R)

		else
			q+:=2
		fi
	od

	obj_free_dict(d,1)				!get rid of old dict
	oldrefcount:=d.refcount
	d^:=e^							!use new dict, but at same address

	pcm_free(e,e.bytes)

	d.refcount:=oldrefcount

	inuse:=0
end

proc adddictitem(variant d, p, q)=
!d is a dict, p:q are akey:value pair to be added to it
	object da
	variant r

	da:=d.objptr

	if da.length=0 then				!cannot be empty
		pcerror("NULL DICT")
	fi

	r:=var_finddictitem(d,p,1)

	var_share(q)
	var_unshare(r)			!overwrite any existing value
	r^:=q^
end
=== qq_host.m 0 0 9/48 ===

record dimrec=(int lbound, upper, length)

type hostproc0=ref proc
type hostproc1=ref proc(variant a)
type hostproc2=ref proc(variant a,b)
type hostproc3=ref proc(variant a,b,c)
type hostproc4=ref proc(variant a,b,c,d)
type hostproc5=ref proc(variant a,b,c,d,e)

ref[]symbol procrefs				!linear arrays set up from proclists

!record overloadrec=
!	int optype, optype2
!	ref int pchandler
!	ref overloadrec nextrec
!end
!
!ref overloadrec tostr_list			!list of user overloads for tostr
!ref overloadrec convert_list

const noparamtag=tvoid
const nodefault=-999999

global func callhostfunction(int hostfn, variant sp)variant =
	ref proc fnaddr
	int nparams,isfn
	object p

!	fnaddr:=hosttable[hostfn]
	fnaddr:=hosthandlers[hostfn]
	nparams:=hostnparams[hostfn]
	isfn:=hostisfn[hostfn]

!CPL "CALL HOST",HOSTFNNAMES[HOSTFN]

	if fnaddr=nil then
		pcerror("Hostfn not implemented:",hostfnnames[hostfn])
	fi

	case nparams+isfn
	when 0 then
		hostproc0(fnaddr)^()
	when 1 then
		hostproc1(fnaddr)^(sp)
	when 2 then
		hostproc2(fnaddr)^(sp,sp-1)
	when 3 then
		hostproc3(fnaddr)^(sp,sp-1,sp-2)
	when 4 then
		hostproc4(fnaddr)^(sp,sp-1,sp-2,sp-3)
	when 5 then
		hostproc5(fnaddr)^(sp,sp-1,sp-2,sp-3,sp-4)
	else
		pcerror("callhost/proc")
	esac

	to nparams do
		var_unshare(sp) when sp.hasref
		--sp
	od

	return sp
end

global proc pch_leftstr(variant a, b, c, result)=
	int n,length,padchar
	ref char s
	object pa

	padchar:=' '
	case c.tag
	when tvoid then
	when tstring then
		if c.objptr.length=1 then
			padchar:=c.objptr.strptr^
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
	length:=pa.length
	s:=pa.strptr

	if n=0 then
		var_empty_string(result,1)
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
			var_empty_string(result,1)
		fi
	fi
end

global proc pch_rightstr(variant a, b, c, result)=
	int n,length,padchar
	ref char s
	object pa

	padchar:=' '
	case c.tag
	when tvoid then
	when tstring then
		if c.objptr.length=1 then
			padchar:=c.objptr.strptr^
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

	length:=pa.length
	s:=pa.strptr

	result.tagx:=tstring ior hasrefmask

	if n=0 then
		var_empty_string(result,1)
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
			var_empty_string(result,1)
		fi
	fi
end

global proc pch_convlc(variant a, b, result)=
	checkparam(a,tstring)
	result^:=a^
	++result.objptr.refcount
	var_duplu(result)
	var_iconvcase(result,b,0)
end

global proc pch_convuc(variant a, b, result)=
	checkparam(a,tstring)
	result^:=a^
	++result.objptr.refcount
	var_dupl(result) when result.hasref
	var_iconvcase(result,b,1)
end

global proc pch_waitkey(variant result)=
	result.tagx:=tint
	result.value:=os_getch()
end

global proc pch_execwait(variant a, b, c, result)=
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
		workdir:=convtostringz(c.objptr.strptr,c.objptr.length)
	fi
	result.tagx:=tint
	result.value:=os_execwait(convtostringz(pa.strptr,pa.length),flag,workdir)
end

global proc pch_execcmd(variant a, b, c, result)=
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
		workdir:=convtostringz(c.objptr.strptr,c.objptr.length)
	fi
	result.tagx:=tint
	result.value:=os_execcmd(convtostringz(pa.strptr,pa.length),flag)
end

global proc pch_makestr(variant a, b, result)=
	int n

	case a.tag
	when trefpack then
	when tint then
	else
		pcerror("makestr")
	esac

	n:=var_getintvalue(b)

	RESULT.TAGX:=TSTRING IOR HASREFMASK

	RESULT.OBJPTR:=obj_make_strslicexobj(cast(a.ptr),n)
end

global proc pch_makeref(variant a,b,result) =
	ref byte ptr

	case (ttbasetype[a.tag])
	when trefvar,trefpack,tint then
		ptr:=a.ptr
	when tstring,tarray,tlist,tset then
		ptr:=a.objptr.ptr
	else
		pcerror("makeref")
	esac

	result.tagx:=trefpack
	result.ptr:=ptr
	result.elemtag:=var_getintvalue(b)

	case result.elemtag
	when tu1,tu2,tu4 then
		result.tag:=trefbit
		result.bitoffset:=0
		result.bitlength:=0
	esac
end

global proc pch_getcmdparam(variant a, result)=
!a=	void:	return number of cmd params following program name
!a= -2:		return name of invoked interpreter
!a= -1:		return name of .q program when run conventionally
!a= 1..N:   return name of n'th cmd param

	int n
	ref char s

	if a.tag=noparamtag then		!return number of cmds
		result.tagx:=tint
		result.value:=nqparams
		return
	fi

	n:=var_getintvalue(a)
	if n not in 1..nqparams then pcerror("getcmdpm") fi

	var_make_string(qparamtable[n],result)
end

global proc pch_clock(variant result)=
	result.tagx:=tint
	result.value:=os_clock()
end

global proc pch_allocexec(variant a, result)=
	int n
	ref byte p

	n:=var_getintvalue(a)
	p:=os_allocexecmem(n)

	result.tagx:=trefpack
	result.ptr:=p
	result.elemtag:=tu8
end

global proc pch_runnative(variant a, b, result)=
	int n
	ref func(int)int fnptr

	if a.tag<>trefpack then pcerror("runnative?") fi
	fnptr:=cast(a.ptr)

	result.value:=fnptr(b.value)
	result.tagx:=tint
end

global proc pch_setlwb(variant a, b)=
	int n
	object p

	if not a.hasref then error fi
	p:=a.objptr
	if not p.mutable then pcnotmut() fi
	n:=checkparam(b,tint)

	case a.tag
	when tlist then
		if n not in i16.min..i16.max then pcerror("lwb not i16") fi
		p.lower16:=n
	when tarray, tbits then
		if n not in 0..1 then pcerror("lwb not 0/1") fi
		p.lower:=n
	else
error:
		pcerror("Can't set lwb")
	esac
end

global proc pch_ticks(variant result)=
	result.tagx:=tint
	result.value:=os_ticks()
end

global proc pch_sleep(variant a)=
	checkparam(a,tint)
	os_sleep(a.value)
end

global proc pch_random(variant a, result)=
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
			mseed(-n)
			x:=0
		fi
	fi
	result.value:=x
end
!
global proc pch_system(variant a,result) =		!PCH_SYSTEM
	checkparam(a,tstring)
	result.tagx:=tint
	result.value:=system(convtostringz(a.objptr.strptr,a.objptr.length))
end

global proc pch_$getparam(variant a, result)=
	checkparam(a,tint)

	result^:=variant(frameptr-a.value*varsize)^		!param 1/2/3... = offset 16/32/48... (varsize=16)
	if result.hasref then
		++result.objptr.refcount
	fi
end

function checkparam(variant p,int tag,defaultx=nodefault)i64=
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

	var_make_stringn(a.objptr.strptr,n,result,1)
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
	var_make_stringn(a.objptr.strptr+(a.objptr.length-n),n,result,1)
end

proc padstring_right(variant a,int n, fillchar, variant result)=
!a is a string (can be "")
!create a new string of n chars of which the first a.length are from a,
!and the rest are filled with <fillchar>
!n>length always
	ref char s
	int length

	length:=a.objptr.length

	var_new_stringn(n,result)
	s:=result.objptr.strptr

	if length then
		memcpy(s,a.objptr.strptr,length)
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

	length:=a.objptr.length
	padlen:=n-length

	var_make_stringn(nil,n,result,0)

	s:=result.objptr.strptr
	s+:=padlen

	if length then
		memcpy(s,a.objptr.strptr,length)
	fi
	to padlen do
		--s
		s^:=fillchar
	od
end

proc getbounds(variant p,ref dimrec dims,int lower) =
! extract length or bounds from p, and return in dims
! p will be an int, range, or other value coerceable to int
! lower is default lower bound
	int n

	if not p then
		pcerror("New: no bounds")
	fi

	case p.tag
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
	esac
end

global proc pch_new(variant a, b, c, d, result)=
	varrec v
	int i,t,nbytes,ival,nwords,nbits,offset,elemtype,n, usertag
	dimrec dims
	variant qvar
	ref i64 qint
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
		if elemtype>=tu1 and elemtype<=tu4 then
			v.tag:=t:=tbits
			goto dobits2
		fi
!
		p:=obj_newarray(elemtype, dims.lbound, dims.length)

doarray2:
		v.objptr:=p

		if dims.length then
			if d and d.tag<>tvoid then		!initial value supplied
				qbyte:=p.ptr
				to dims.length do
					var_storepacked(qbyte,d,elemtype)
					qbyte+:=ttsize[elemtype]
				od
			fi
		fi
!
	when tvector then
		usertag:=t
		v.tag:=tvector
		elemtype:=tttarget[t]
		dims.length:=ttlength[t]
		dims.lbound:=ttlower[t]
		dims.upper:=dims.length+dims.lbound-1

		d:=b					!any init value: move to d
		p:=obj_newarray_u(t)
!CPL =TTNAME[T], TTNAME[TTBASETYPE[T]]
		goto doarray2
!
	when tbits then
		elemtype:=var_getintvalue(b)
		if elemtype not in tu1..tu4 then
			pcerror("new: bad bits elem")
		fi
		getbounds(c,&dims,1)
dobits2:				!entry point from arrays, when element is bit type

		p:=obj_newbits(elemtype,dims.lbound,dims.length)

		v.objptr:=p

		if dims.length then
			if d and d.tag<>tvoid then		!initial value supplied
				qbyte:=p.ptr

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
		v.tag:=trecord
		usertag:=t

	when tstruct then

		p:=obj_new_struct(t)

		var_objtovar(t,p,&v)
		v.tag:=tstruct
		usertag:=t

		if b and b.tag<>tvoid then
			pcerror("New: struct init")
		fi

	when tint,treal,trefvar then
		v.value:=0
		v.hasref:=0
		if b and b.tag<>tvoid then
			pcerror("NEW(int/value)")
		fi

	when tdict then
		getbounds(b,&dims,1)
		if dims.lbound<>1 then
			pcerror("new:dict:lwb")
		fi
		p:=obj_new_dict(dims.length)
		v.objptr:=p

	when tdecimal then
		var_empty_dec(result)
		return

	else
		pcustype_t("new",t)
	end
finish:

	if usertag then
		v.objptr.usertag:=usertag
	fi

	result^:=v

end

global proc pch_gethostname(variant result) =
	static [256]char name

	strcpy(name,os_gethostname())

	var_make_string(name,result)
end

global proc pch_getprogname(variant result) =
	static [256]char name

	strcpy(name,inputfile)

	var_make_string(name,result)
end

global proc pch_$test(variant a, b, c, result)=
OBJECT P

!PPVAR:=A.VARPTR

CPL "$TEST:",TTNAME[a.TAG]
!!
RESULT.TAGX:=TINT
RESULT.VALUE:=A.VALUE+B.VALUE+C.VALUE

end

global proc pch_$test2(variant a, result)=
!PPP:=A.OBJPTR
	RESULT.TAGX:=TVOID
end

global proc pch_$refcount(variant a, result)=
	result.tagx:=tint
	if a.hasref then
!		result.value:=a.objptr.refcount
		result.value:=a.objptr.refcount-1		!exclude this copy
	else
		result.value:=0
	fi
end

global proc pch_testkey(variant result)=
	result.tagx:=tint
	result.value:=os_kbhit()
end

global proc pch_getos(variant result)=
	var_make_string(os_getos(),result)
end

global proc pch_setmesshandler(variant fn)=
	if fn.tag<>tsymbol or fn.def.nameid<>procid then
		pcerror("Not proc ref")
	fi
!CPL "SETMESSHANDLER NOT READY"
	pcl_callbackfn:=cast(fn.def.labelref)
!PCERROR("SETMESSHANDLER")
	os_setmesshandler(&runproc_m)
end

global proc pch_$smallmemtotal(variant result)=
	result.tagx:=tint
	result.value:=smallmemtotal/varsize
end

global proc pch_$id(variant a, result)=
	result.tagx:=tint
	result.value:=a.value
end

global proc pch_iswindows(variant result)=
	result.tagx:=tint
	result.value:=os_iswindows()
end

global proc pch_$setdebug(variant a)=
	checkparam(a,tint)

	CPL "SETDEBUG................."
	fdebug:=a.value
end

global proc pch_copy(variant a, dest)=
	dest^:=a^
	var_dupl(dest)
end

global proc pch_gethash(variant a,result) =		!PCH_GETHASH
!convert a to hash value
	result.tagx:=tint
	result.value:=var_gethashvalue(a)
end

global proc pch_makeempty(variant a,result)=
	object p
	int t

	t:=ttbasetype[a.tag]
	if t=ttype then
		t:=a.value
	fi

	p:=a.objptr

	case t
	when tlist then
		var_empty_list(p.lower16,result)
		return

	when tstring then
		p:=emptystring
		++p.refcount
	when tarray then
		var_empty_array(t, p.elemtag, p.lower,result)
		return

!	when tvector then
!		var_empty_array(t, tttarget[p.usertag], 1,result)
!		return

	else
		pcustype_t("makeempty?",t)
	esac

	result.tagx:=t ior hasrefmask
	result.objptr:=p
end

global proc pch_$infinity(variant dest)=
	var_setinf(dest)
end

global proc pch_$nan(variant dest)=
	var_setnan(dest)
end

global proc setcmdparam(int index, ichar s)=
!build cmd params for pc program, or set size (usually smaller) when s is nil
	if s=nil then
		nqparams:=index
	elsif index<=maxqparam then
		qparamtable[index]:=pcm_copyheapstring(s)
		nqparams max:=index
	fi
end

global proc pch_$nprocs(variant result)=
	result.tagx:=tint
	result.value:=nproclist
end

proc initprocrefs=
	ref procrec pp
	static int oldnprocs

!	if procrefs and nn=nproclist then
!IF NN<>NPROCLIST THEN
!PCERROR("NPROCLIST HAS CHANGED")
!FI
! return fi
	if oldnprocs=nproclist then
		return
	fi


!CPL "INITPROCREFS",NPROCLIST

	procrefs:=pcm_alloc(nproclist*procrefs[1].bytes)

	pp:=proclist

	for i to nproclist do
		procrefs[i]:=pp.def
		pp:=pp.nextproc
	od
	oldnprocs:=nproclist

!NN:=NPROCLIST
end

global proc pch_$procname(variant a, result)=
	int n:=checkparam(a,tint)

!CPL "PROCNAME",NPROCLIST

	initprocrefs()

	var_make_string(procrefs[n].name, result)
end

global proc pch_$procref(variant a, result)=
	int n:=checkparam(a,tint)
	ref procrec pp

	initprocrefs()

	result.tagx:=tsymbol
	result.def:=cast(procrefs[n])
end

global proc pch_$getstdinout(variant a, result)=
	int n:=checkparam(a,tint)

	result.tagx:=trefpack
	result.ptr:=cast((a.value=1|os_getstdout()|os_getstdin()))
	result.elemtag:=tvoid
end
=== qq_lex.m 0 0 10/48 ===
const etx	= 26
const cr	= 13
const lf	= 10
const tab	= 9

ref char lxsource		!start of module
ref char lxstart		!start of this token
ref char lxsptr
int lxifcond
int longsuffix			!for real nos
int lxlineno

const hstsize	= 32768
!const hstsize	= 65536
const hstmask	= hstsize-1

int nextlxlength
global int lxlength

global [0:hstsize]strec hashtable
symbol hashtablelast

ichar u64maxstr="18446744073709551615"

[0..255]byte namemap			!0/1/2 = other/name/name-upper

global proc lexreadtoken=
!read next token into nextlx
int c,csum,hsum,commentseen
ref char pstart,pnext,p,ss

	nextlx.subcode:=0

	doswitch lxstart:=lxsptr; lxsptr++^
	when 'a'..'e','g'..'z','$','_' then
	dolower:
		nextlx.svalue:=lxsptr-1
	doname:
		hsum:=nextlx.svalue^

		docase namemap[c:=lxsptr++^]
		when 1 then
			hsum:=hsum<<4-hsum+c
		when 2 then
			(lxsptr-1)^:=c+' '
			hsum:=hsum<<4-hsum+c+' '
		else
			--lxsptr
			exit
		end docase

		lookup(nextlx.svalue, lxsptr-nextlx.svalue, (hsum<<5-hsum) iand hstmask)
		return

	when 'A'..'E','G'..'Z' then
	doupper:
		nextlx.svalue:=lxsptr-1
		nextlx.svalue^+:=32
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
		case lxsptr^
		when ')',cr,',',' ' then		!assume single digit decimal
			nextlx.symbol:=intconstsym
			nextlx.subcode:=tint
			nextlx.value:=lxstart^-'0'
		when 'x','X' then
			case lxstart^
			when '0' then		!0x
				++lxsptr
				readhex()
			when '2' then
				++lxsptr
				readbin()
			else
				lxerror("Bad base")
			esac
		else
			--lxsptr
			readdec()
		esac
		return

	when '!', '#' then			!comment to eol
	docomment:

		docase c:=lxsptr++^
		when 13 then
			++lxsptr
			++lxlineno
			exit
		when 10 then
			++lxlineno
			exit
		when etx,0 then
			--lxsptr
			exit
		end
		nextlx.symbol:=eolsym
		return

	when '\\' then			!line continuation

!two stages:
! 1: read chars until any eol chars (unless further '\' seen)
! 2: read until non-white space
		commentseen:=0
		docase lxsptr++^			!read until end of this line
		when cr then
			++lxsptr				!skip lf
			++lxlineno
			exit
		when lf then
!		++nextlx.pos
			++lxlineno
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
		end
!eol seen: now skip 0 or more further eol chars, plus any white space (ie. multiple blank lines)

		docase lxsptr++^
		when cr then
			++lxsptr				!skip lf
			++lxlineno
		when lf then
			++lxlineno
		when ' ',tab then
		else
			--lxsptr
			exit
		end

	when '{' then
		nextlx.symbol:=lcurlysym
		return

	when '}' then
		nextlx.symbol:=rcurlysym
		return

	when '.' then
		case lxsptr^
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
		elsif lxsptr^ in '0'..'9' then			!real const: deal with this after the switch
			--lxsptr
			readreal()
			return
		else
			nextlx.symbol:=dotsym
			return
		esac

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
		when ':' then
			++lxsptr
			case lxsptr^
			when '=' then
				++lxsptr
				nextlx.symbol:=assignsym
				nextlx.subcode:=1			!deep copy
			else
				error
			esac
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
		nextlx.symbol:=barsym
		return

	when '^' then
		nextlx.symbol:=ptrsym
		nextlx.subcode:=1				!when used as ^x
!		nextlx.subcode:=jptrto
		return

	when '@' then
		nextlx.symbol:=atsym
		return

	when '?' then
		nextlx.symbol:=questionsym
		return

!	when '~' then
!		nextlx.symbol:=curlsym
!		return

	when '+' then
		nextlx.symbol:=addsym
		nextlx.subcode:=kadd
		if lxsptr^='+' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=0
		fi

		return

	when '-' then
		nextlx.symbol:=subsym
		nextlx.subcode:=ksub
		case lxsptr^
		when '-' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=1
		when '>' then
			++lxsptr
			nextlx.symbol:=pipesym
		esac
		return

	when '*' then
		nextlx.symbol:=mulsym
		nextlx.subcode:=kmul
		if lxsptr^='*' then
			++lxsptr
			nextlx.symbol:=powersym
			nextlx.subcode:=kpower
		fi
		return

	when '/' then
		nextlx.symbol:=divsym
		nextlx.subcode:=kdiv
		return

	when '%' then
		nextlx.symbol:=idivsym
		nextlx.subcode:=kidiv
		return

	when '=' then
		case lxsptr^
		when '>' then
			nextlx.symbol:=sendtosym
			++lxsptr
		when '=' then
			nextlx.symbol:=samesym
			nextlx.subcode:=ksame
			++lxsptr
		else
			nextlx.symbol:=eqsym
			nextlx.subcode:=eq_cc
		esac
		return

	when '<' then
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=lesym
			nextlx.subcode:=le_cc
		when '>' then
			++lxsptr
			nextlx.symbol:=nesym
			nextlx.subcode:=ne_cc
		when '<' then
			++lxsptr
			nextlx.symbol:=shlsym
			nextlx.subcode:=kshl
		else
			nextlx.symbol:=ltsym
			nextlx.subcode:=lt_cc
		esac
		return

	when '>' then
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=gesym
			nextlx.subcode:=ge_cc
		when '>' then
			++lxsptr
			nextlx.symbol:=shrsym
			nextlx.subcode:=kshr
		else
			nextlx.symbol:=gtsym
			nextlx.subcode:=gt_cc
		esac
		return

	when '&' then
		case lxsptr^
		when '&' then
			++lxsptr
			nextlx.symbol:=concatsym
			nextlx.subcode:=kconcat
		else
			nextlx.symbol:=addrsym
			nextlx.subcode:=0
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
		++lxlineno
		nextlx.symbol:=eolsym
		return
	when lf then			!only lfs not preceded by cr
		nextlx.symbol:=eolsym
		++lxlineno
		return

	when etx,0 then
		nextlx.symbol:=eofsym
		--lxsptr
		return

	else
		c:=(lxsptr-1)^
		if c=0xE2 and lxsptr^=0x88 and (lxsptr+1)^=0x9A then
			lxsptr+:=2
			nextlx.symbol:=mathssym
			nextlx.subcode:=mm_sqrt
			return
		fi

		if c>=128 then		!assume utf8
			goto doname
		fi
error:
		nextlx.symbol:=errorsym
		nextlx.value:=c
		return

	end doswitch
!end switch
!od

end

proc lxreadstring(int termchar)=
!start from char just after " or ' (termchar will be " or ')

	ichar s,t
	int c, d, length, hasescape, a, n
	[8]char str

	if termchar='"' then
		nextlx.symbol:=stringconstsym
	else
		nextlx.symbol:=charconstsym
		nextlx.subcode:=tint
	fi

!do a first pass that terminates length of final string
	length:=0
	hasescape:=0
	t:=nil

	for pass:=1 to 2 do
!CPL =PASS
		s:=lxsptr
		do
			case c:=s++^
			when '\\' then			!escape char
				hasescape:=1
				c:=s^
				if c>='A'  and c<='Z' then c+:=' ' fi
				++s
				case c
				when 'a' then			!bell ('alert')
					c:=7
				when 'b' then			!backspace
					c:=8
				when 'c','r' then		!carriage return
					c:=cr
				when 'e' then			!escape
					c:=27
				when 'f' then			!formfeed
					c:=12
				when 'h' then
					while s^ <> '\\' do
						c:=readhexcode(&s,2,1)
						if pass=2 then
							t^:=c
						fi
						++t
					od
					++s
					--t					!will overwrite last byte

				when 'l','n' then		!linefeed, or linux/c-style newline
					c:=lf
				when 't' then			!tab
					c:=9
				when 'u','v' then		!reserved for unicode, like \x but with 4 hex digits
					t +:= getutf8(readhexcode(&s, (c='u'|4|6)), (pass=2|t|nil))
					nextloop

				when 'w' then			!windows-style cr-lf
					if pass=2 then
						t^:=cr
					fi
					++t
					c:=lf
				when 'x' then	!2-digit hex code follows
					c:=readhexcode(&s,2)
				when 'y' then			!CCI/SM backwards tab
					c:=16
				when 'z' then			!null (not fully supported in code)
					c:=0
				elsecase c
				when '"' then			!embedded double quote
					c:='"'
				when '\\' then
					c:='\\'
				when '\'' then			!embedded single quote
					c:='\''
				when '0' then
					c:=0
				else
					str[1]:=c; str[2]:=0
					lxerror_s("Unknown string escape: #",str)
				end
			when '"','\'' then		!possible terminators
				if c=termchar then		!terminator char
					if s^=c then		!repeated, assume embedded term char
						++s
					else			!was end of string
						exit
					fi
				fi
HASESCAPE:=1
			when cr,lf,0 then
				lxerror("String not terminated")
			esac

			if pass=2 then
				t^:=c
			fi
			++t

		od

		if pass=1 then
			length:=int(t)
!			println "LENGTH IS", LENGTH
!			println =LENGTH
!			println =HASESCAPE
			nextlx.slength:=length+1
			if hasescape then
				nextlx.svalue:=t:=pcm_alloc(length+1)
			elsif length=0 then
				nextlx.svalue:=""
				lxsptr:=s
				return
			else
				nextlx.svalue:=pcm_copyheapstringn(lxsptr,length)
				lxsptr:=s
				return
			fi

		else
			t^:=0
			lxsptr:=s
		fi
	od
end

func readhexcode(ref ref char s, int n, sp=0)int a=
!read n hex digits from from char ptr, and step ptr in the caller
	int c
	a:=0
	for i to n do

		if sp and i.odd then
!CPL "CHECKSP",=SP,=I
			repeat
				c:=(s^)++^
			until c<>' '
		else
			c:=(s^)++^
		fi

		if c in 'A'..'F' then
			a:=a*16+c-'A'+10
		elsif c in 'a'..'f' then
			a:=a*16+c-'a'+10
		elsif c in '0'..'9' then
			a:=a*16+c-'0'
!		elsif c='\\' then
!			--(s^)
!			exit
		else
			lxerror("Bad hex digit")
		fi
	od
	a
end

func getutf8(int c, ref char s)int n =
!convert unicode char c to utf8 sequence at s, consisting of 1-4 bytes, and
!return the number of bytes. s will be zero-terminated
!On error, return zero
	[16]char str
	if s=nil then s:=str fi

	if c<=0x7F then
		n:=1
		s++^:=c

	elsif c<=0x7FF then
		n:=2
		s++^:=2x110_00000 + c.[10..6]
		s++^:=2x10_000000 + c.[5..0]

	elsif c<=0xFFFF then
		n:=3
		s++^:=2x1110_0000 + c.[15..12]
		s++^:=2x10_000000 + c.[11..6]
		s++^:=2x10_000000 + c.[5..0]
	elsif c<=0x10FFFF then
		n:=4
		s++^:=2x11110_000 + c.[20..18]
		s++^:=2x10_000000 + c.[17..12]
		s++^:=2x10_000000 + c.[11..6]
		s++^:=2x10_000000 + c.[5..0]
	else
		n:=0
	fi

	s^:=0
	n
end

global proc lexinit=
!do one-time setup:
! clear the hash table and populated it with reserved words
! do maxnum support and such
	int i!,n
	static int n

	memset(&hashtable,0,hashtable.bytes)
	hashtablelast:=&hashtable[hstsize-1]

	inithashtable()
end

proc readrawstring=
!positioned at " of F"
!read raw string
	ichar pstart
	int length

	nextlx.symbol:=stringconstsym

	pstart:=++lxsptr
	length:=0

	docase lxsptr++^
	when '"' then
		exit
	when cr,lf,0 then
		lxerror("Raw string not terminated")
		--lxsptr
		exit
	else
		++length
	end

	nextlxlength:=length

	nextlx.svalue:=pcm_copyheapstringn(pstart,length)
end

global function lookup(ichar name, int length, hashindex)int=
!lookup rawnamesym with details in nextlx
!hash value already worked out in nextlxhashvalue
!return 1 (found) or 0 (not found)
!in either case, nextlx.symptr set to entry where name was found, or will be stored in
	int j,wrapped,n
	symbol d
	ref char s

	d:=&hashtable[hashindex]
	wrapped:=0

	do
		if (n:=d.namelen)=length and memcmp(d.name,name,n)=0 then	!match
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
	d.name:=pcm_copyheapstringn(name,length)
	d.namelen:=length
	d.symbolcode:=namesym

	nextlx.symptr:=d
	nextlx.symbol:=d.symbolcode
	nextlx.subcode:=d.subcode

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
		hsum:=hsum<<4-hsum + c
	od
	return (hsum<<5-hsum) iand hstmask
end

proc start=
	for c in namemap.bounds do
		if c in 'a'..'z' or c in '0'..'9' or c in ['_','$'] or c in 128..255 then
			namemap[c]:=1
		elsif c in 'A'..'Z' then
			namemap[c]:=2				!upper case
		fi
	od
end

proc inithashtable=
!populate hashtable with standard symbols
	int i
	ichar name

	for i:=1 to stnames.len do
		addstname(stnames[i], stsymbols[i], stsubcodes[i])
	od

	for i to hostfnnames.upb when not hostinternal[i] do
		name:=hostfnnames[i]+2				!skip 'h_'
		addstname(name, khostfnsym, i)

	od
end

proc addstname(ichar name, int symbol, subcode)=
	if lookup(name,strlen(name),gethashvaluez(name)) then
		println name
		abortprogram("Dupl ST entry")
	fi

	nextlx.symptr.symbolcode:=symbol
	nextlx.symptr.subcode:=subcode
end

global proc startlex(ifile pm)=
!	if not fwriteqa then
!		lxsource:=lxsptr:=pm.text
!	else
		lxsource:=lxsptr:=pcm_copyheapstring(pm.text)
!	fi
	nextlx.symbol:=semisym
	nextlx.subcode:=0
	nextlx.moduleno:=pm.moduleno
	lxlineno:=1
end

global function addnamestr(ichar name)symbol=
	lexrec oldlx
	symbol symptr

	oldlx:=nextlx

	nextlxlength:=strlen(name)
	nextlx.svalue:=pcm_alloc(nextlxlength+1)
	memcpy(nextlx.svalue,name,nextlxlength+1)
	lookup(nextlx.svalue, nextlxlength, gethashvaluez(name))
	symptr:=nextlx.symptr

	nextlx:=oldlx

	return symptr
end

global proc ps(ichar caption)=
	print caption,,":::"
	printsymbol(&lx)
end

global proc psnext(ichar caption)=
	print "	",,caption,,":##"
	printsymbol(&nextlx)
end

global proc lex=
!return next token in lx, using lexreadtoken but working a token ahead.
!static int lastline=0
	int lineno,n,dir,namelen
	ref char p
	symbol symptr

	lx:=nextlx				!grab that already read basic token
	lx.lineno:=lxlineno

	lxlength:=nextlxlength

	reenter:

	lexreadtoken()			!read new token for next time around
	reenter2:

	case nextlx.symbol
	when unitnamesym then					!might be user identifier (points to generic entry)
		case lx.symbol
		when intconstsym then
			case nextlx.symptr.subcode
			when million_unit then lx.value *:= 1 million
			when billion_unit then lx.value *:= 1 billion
			when thousand_unit then lx.value *:= 1000
			else
				lxerror("Can't do this unit index")
			esac
			lx.subcode:=tint
			goto reenter
		when realconstsym then
			lxerror("unit symbol after float?")
		else
			nextlx.symbol:=namesym				!convert to actual identifier
		esac

	when sysconstsym then					!ST ENTRY LIMITED TO 16 bits signed
		case nextlx.subcode
		when con_const then
			nextlx.symbol:=intconstsym
			nextlx.value:=0
			nextlx.subcode:=tint
		when pi_const then
			nextlx.symbol:=realconstsym
			nextlx.xvalue:=pi
			nextlx.subcode:=treal
		when tab_const then
			nextlx.symbol:=stringconstsym
			nextlx.svalue:="\t"
			nextlxlength:=1
		when true_const then
			nextlx.symbol:=intconstsym
			nextlx.value:=1
			nextlx.subcode:=tint
		when false_const then
			nextlx.symbol:=intconstsym
			nextlx.value:=0
			nextlx.subcode:=tint

		else
			lxerror("sysconst?")
		esac

	when eolsym then

		case lx.symbol
		when commasym, lsqsym, lbracksym, !ignore eol
			 assignsym,semisym then

			lexreadtoken()
			goto reenter2

			goto reenter
		elsif binopset[lx.symbol] and 	lx.symbol <>minmaxsym then
			lexreadtoken()
			goto reenter2

		esac
		nextlx.symbol:=semisym

	when insym then
		if lx.symbol=notlsym then
			lx.symbol:=insym
			lx.subcode:=1
			goto reenter
		fi
	esac

end

global proc lxerror_s(ichar mess,a)=
	[256]char str
	fprint @str,mess,a
	lxerror(&.str)
end

proc makedecimal(ichar s, int length,base)=
!create a decimal number token

	if base<>10 then
		LXERROR("MAKEDECIMAL/16/2")
	fi

	nextlx.symbol:=decimalconstsym
	nextlx.subcode:=tdecimal
	nextlx.svalue:=pcm_copyheapstringn(s,length)
	nextlxlength:=length
end

proc readdec=
	int c
	ref char dest, destend, pstart
	int islong, length
	[1024]byte str
	word a

	islong:=0

	pstart:=lxsptr

	dest:=&.str
	destend:=dest+str.len-10
	a:=0

	do
		if (c:=lxsptr++^) in '0'..'9' then
			a:=a*10+c-'0'
			dest++^:=c
		elsecase c
		when 'e','E' then
			lxsptr:=pstart
			readreal()
			return
		when '.' then
			if lxsptr^<>'.' then
				lxsptr:=pstart
				readreal()
				return
			fi
			--lxsptr
			exit

		when '_','\'' then
		when 'l','L' then
			dest^:=0
			makedecimal(&.str,dest-&.str,10)
			return

		when 'b','B' then
			length:=dest-&.str
			if length>64 then lxerror("bin overflow") fi
			dest:=&.str
			a:=0
			to length do
				if dest^>='2' then lxerror("bad bin digit") fi
				a:=a*2+dest++^-'0'
			od
			finish

		else
			--lxsptr
			exit
		fi

		if dest>=destend then lxerror("Numlit too long") fi
	end
	length:=dest-&.str

	if length>20 or length=20 and strncmp(&.str,u64maxstr,20)>0 then
		makedecimal(&.str,length,10)
		return
	fi

finish:
	nextlx.symbol:=intconstsym
	nextlx.subcode:=tint
	nextlx.value:=a
end

proc readhex=
	int c
	ref char dest, destend, pstart
	int length
	[1024]byte str
	word a

	pstart:=lxsptr

	dest:=&.str
	destend:=dest+str.len-10
	a:=0

	do
		if (c:=lxsptr++^) in '0'..'9' then
			a:=a*16+c-'0'
			dest++^:=c

		elsif c in 'A'..'F' then
			dest++^:=c
			a:=a*16+c-'A'+10
		elsif c in 'a'..'f' then
			dest++^:=c-32
			a:=a*16+c-'a'+10

		elsecase c
		when '_','\'' then
		when 'l','L' then
			dest^:=0
			makedecimal(&.str,dest-&.str,16)
			return

		when '.' then
			--lxsptr
			exit

		else
			--lxsptr
			exit
		fi

		if dest>=destend then lxerror("Numlit too long") fi
	end
	length:=dest-&.str

	if length>16 then
		makedecimal(&.str,length,16)
		return
	fi

	nextlx.symbol:=intconstsym
	nextlx.subcode:=tint
	nextlx.value:=a
end

proc readbin=
	int c
	ref char dest, destend, pstart
	int length
	[1024]byte str
	word a

	pstart:=lxsptr

	dest:=&.str
	destend:=dest+str.len-10
	a:=0

	do
		case c:=lxsptr++^
		when '0', '1' then
			a:=a*2+c-'0'
			dest++^:=c

		when '_','\'' then
		when 'l','L' then
			dest^:=0
			makedecimal(&.str,dest-&.str,2)
			return

		when '.' then
			--lxsptr
			exit

		elsif c in '2'..'9' then
			lxerror("bin bad digit")
		else
			--lxsptr
			exit
		esac

		if dest>=destend then lxerror("bin overflow") fi
	end
	length:=dest-&.str

	if length>64 then
		makedecimal(&.str,length,2)
		return
	fi

	nextlx.symbol:=intconstsym
	nextlx.subcode:=tint
	nextlx.value:=a
end

proc readreal=
!at '.', or had been in middle of int where . or e were seen, back at the start

	int c,n,negexpon,dotseen,length, fractlen, expon, expseen
	real x
	[1024]char str
	ichar dest, destend, pexpon

	dest:=&.str
	destend:=dest+str.len-100
	length:=negexpon:=dotseen:=expseen:=expon:=fractlen:=0

	do
		if (c:=lxsptr++^) in '0'..'9' then
			dest++^:=c
			++length
			if dotseen then ++fractlen fi
		elsecase c
		when '.' then
			if dotseen then --lxsptr; exit fi
			dotseen:=1
			dest++^:=c


		when 'e','E' then
			if expseen then lxerror("double expon") fi
			expseen:=1
			dest++^:=c
			while lxsptr^=' ' do ++lxsptr od
			if lxsptr^ in ['+','-'] then
				if lxsptr^='-' then negexpon:=1 fi
				dest++^:=lxsptr++^
			fi

			expon:=0
			do
				if (c:=lxsptr++^) in '0'..'9' then
					expon:=expon*10+c-'0'
					dest++^:=c
					if dest>=destend then lxerror("expon?") fi
				elsecase c
				when '_','\'' then
				when 'l','L' then
					dest^:=0
					makedecimal(&.str,dest-&.str,10)
					return
				else
					--lxsptr
					exit all
				fi
			end

		when '_','\'' then

		when 'l','L' then
			makedecimal(&.str,dest-&.str,10)
			return
		else
			--lxsptr
			exit
		fi

		if dest>=destend then lxerror("r64lit too long") fi
	end
	dest^:=0

!------------------------------------------------------------
! Fast way to convert for ordinary numbers (1e100 migt be slower!)
!------------------------------------------------------------
	if negexpon then expon:=-expon fi
	expon-:=fractlen
	x:=0.0

	for i:=1 to length+dotseen do		!digits already range-checked
		c:=str[i]
		if c<>'.' then
			x:=x*10.0+c-'0'
		fi
	od

	if expon>=0 then
		to expon do
			x*:=10.0
		od
	else
		to -expon do
			x/:=10.0
		od
	fi

	nextlx.xvalue:=x
!------------------------------------------------------------
! Best way to covert: more accurate representation, but slower
!------------------------------------------------------------
!	nextlx.xvalue:=strtod(str,nil)
!------------------------------------------------------------


	nextlx.symbol:=realconstsym
	nextlx.subcode:=treal
end

proc readrawxname=
	int c,hsum,length

	nextlx.svalue:=lxsptr
	hsum:=0

	while namemap[c:=lxsptr++^] do
		hsum:=hsum<<4-hsum+c
	od
	--lxsptr

	lookup(nextlx.svalue, lxsptr-nextlx.svalue, (hsum<<5-hsum) iand hstmask)

	return
end
=== qq_lib.m 0 0 11/48 ===
int currlineno
global int nextavindex=0

strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar

const maxlocalunits=500
![maxlocalunits]unitrec unitpool
int nlocalunits

ichar errormess

global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

global proc reportcterror(ichar errortype,mess,int pos, symbol currproc=nil)=
	locrec loc
!CPL "CT1",POS IAND 16777215, CURRPROC, =POS,=QPOS
	loc:=geterrorinfo(pos,currproc)
	println errortype,"Error:"
	println "    ",,mess
	println

	if pos or qpos then
		showerrorsource(loc)
	fi
	stopcompiler(loc)
end

global func geterrorinfo(word pos, symbol currproc=nil)locrec=
!slow is the low word of a string pointer into source code
!moduleno is the module number if known, otherwise 0 to work it out here
!Set up global error vars: errorline, errorpointer, errormodule, errorlineno
	int soffset, moduleno
	locrec loc

	clear loc
	loc.lineno:=pos.[0..23]
	moduleno:=pos.[24..31]

	if moduleno=0 then
		return loc
	fi
!CPL =SOFFSET
!CPL =MODULENO

!	if moduleno=0 then
!		ABORTPROGRAM("GETERRORINFO: no module")
!	fi
	if currproc=nil then
		ABORTPROGRAM("GETERRORINFO: no currproc")
	fi

	loc.pm:=modules[moduleno]
	loc.sp:=subprogs[loc.pm.subprogno]	
	loc.def:=currproc

	return loc
end

proc showerrorsource(locrec loc)=
	ichar s

!	return unless loc.status
	return unless loc.pm

	println "Line:",loc.lineno,"in Module",loc.pm.name,,".q:"

	if loc.def then
		println "In function:",loc.def.name
	fi

!!CPL "///STARTLINE",LOC.STARTLINE
!	print " |"
!	s:=loc.startline
!	while s^ not in [13,10,26,0] do
!		print s^
!		++s
!	od
!	println "|"
	

!	println " |",errorline
!	println " |",errorpointer
end

global proc stopcompiler(locrec loc)=
	filehandle f
	if loc.pm then
		f:=fopen("$error.tmp","w")
!		println @f,modulename,,".q",lineno
		println @f,loc.pm.filespec, loc.lineno
		fclose(f)
		println
		println
	fi

!	OS_GETCH()

	stop 1
end

global proc gerror(ichar mess,unit p=nil)=
!CPL "G1"
	reportcterror("Code Gen",mess,(p|p.pos|qpos),stcurrproc)
end

global proc gerror_s(ichar mess, param,unit p=nil)=
	[300]char str
	print @str, mess, param
	reportcterror("Code Gen",&.str,(p|p.pos|qpos),stcurrproc)
end

global proc serror(ichar mess)=
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

!CPL =P, =QPOS, =STCURRPROC

	reportcterror("Resolve",mess,(p|p.pos|qpos),stcurrproc)
end

global proc rxerror_s(ichar mess,param, unit p=nil)=
	[300]char str
	strcpy(&.str,mess)
	strcat(&.str," ")
	strcat(&.str,param)
	rxerror(str,p)
end

global proc lxerror(ichar mess)=
	reportcterror("Lex",mess,lx.pos,stcurrproc)
end

global proc loaderror(ichar mess,mess2="")=
	[512]char str
	print @str,mess,mess2

	println "Load Error:",str
	println "Stopping"
	stop 1
end

global proc prterror(ichar mess)=
	println "Print error:",mess
	os_getch()
	stop 1
end

function allocunitrec:unit p=
!	p:=pcm_alloc(unitrec.bytes)
	p:=pcm_allocnfz(unitrec.bytes)
!	p:=pcm_alloc32()
!	p:=pcm_alloc64()
!	p:=malloc(64)
!	clear p^

!++NUNITS

!	p.word1:=p.nextunit:=p.a:=p.b:=nil
!	p.nextunit:=p.a:=p.b:=nil
	p.pos:=lx.pos
	return p
end

global function createintunit(i64 a)unit=
	unit u
!	allocunit(u)
	u:=allocunitrec()
	u.tag:=jintconst
	u.value:=a
	return u
end

global function createrealunit(r64 x)unit=
	unit u
!	allocunit(u)
	u:=allocunitrec()
	u.tag:=jrealconst
	u.xvalue:=x
	return u
end

global function createstringunit(ichar s, int slength=-1)unit=
	unit u
	if slength=-1 then
		slength:=strlen(s)
	fi

	u:=allocunitrec()
!	allocunit(u)
	u.tag:=jstringconst
	u.svalue:=pcm_alloc(slength+1)
	if slength then
		memcpy(u.svalue,s,slength)
	fi
	(u.svalue+slength)^:=0
	u.slength:=slength
	return u
end

global function createunit0(int tag)unit=
	unit u
	u:=allocunitrec()
!	allocunit(u)
	u.tag:=tag
	return u
end

global function createunit1(int tag, unit p)unit=
	unit u
	u:=allocunitrec()
!	allocunit(u)
	u.tag:=tag
	u.a:=p
	return u
end

global function createunit2(int tag, unit p,q)unit=
	unit u

	u:=allocunitrec()
!	allocunit(u)

	u.tag:=tag
	u.a:=p
	u.b:=q

	return u
end

global function createname(ref strec p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
!	allocunit(u)
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
		ulistx.nextunit:=p
	fi
	ulistx:=p			!update end-of-list pointer

	p:=p.nextunit
od
end

global function createavname:unit=
!create auto-var name and return pointer to st entry
	symbol p
	[32]char str
	ichar name

!	sprintf(&.str,"av$%d",++nextavindex)
	print @str, "av$",,++nextavindex

	name:=pcm_copyheapstring(&.str)
	p:=addnamestr(name)

	return createname(p)
end

global function convtostringz(ref char svalue,int length)ref char =
! a contains a string object which is a NON-zero-terminated string.
! Set up a pointer to a proper zero-terminated one and return that.
! This uses a ring of 3 static string objects it will only work for strings up to
! a certain length, and only if not more than 3 are needed for any single call.

	const strbufflen=2000
	static [0:strbufflen]char strbuffer1
	static [0:strbufflen]char strbuffer2
	static [0:strbufflen]char strbuffer3
	static [0:strbufflen]char strbuffer4
	static [0:strbufflen]char strbuffer5
	static [0:strbufflen]char strbuffer6
	static int strindex=0		!index of current buffer: cycles between 0,1,2
!	static [0:]ref [0:]char table=(
	static [0:]ref [0:0]char table=(

! [0:]ref [0..15]c8


		&strbuffer1,&strbuffer2,&strbuffer3,
		&strbuffer4,&strbuffer5,&strbuffer6)
!	cast(strbuffer1),cast(strbuffer2),cast(strbuffer3),
!	cast(strbuffer4),cast(strbuffer5),cast(strbuffer6))
	ref[0:]char p
	static ichar longstr=nil


	if length>=strbufflen then
		if longstr then
			free(longstr)
		fi
		longstr:=malloc(length+1)
		memcpy(longstr,svalue,length)
		(longstr+length)^:=0
		return longstr
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

global function findprocname(ref proc fnptr)ichar=
	ichar name
	int n:=$getnprocs()

	for i to n do
		if $getprocaddr(i)=fnptr then
			return $getprocname(i)
		fi
	od

	return "?"
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
	unit q
	[500]char str

!CPL "JEVAL",JTAGNAMES[P.TAG]

!	switch p.tag
	case p.tag
	when jintconst,JOPERATOR then
		additem(strint(p.value))

	when jrealconst then
		additem(strreal(p.value))

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

!	when jcall then
!		jeval(p.a)
!		additem("(")
!
!		q:=p.b
!		while q do
!			jeval(q)
!			q:=q.nextunit
!			if q then additem(",") fi
!		od
!		additem(")")

	when jcallhost then
		additem("Host<")
		additem(hostfnnames[p.index]+2)
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
		if p.tag=jdotindex then
			additem(".")
		fi
		additem("[")
		jeval(p.b)
		additem("]")

	when jkeyindex then
		jeval(p.a)
		additem("{")
		jeval(p.b)
		additem("}")

	when jdot then
		jeval(p.a)
		additem(".")
		jeval(p.b)

	when jassign then
		jeval(p.a)
		additem(":=")
		jeval(p.b)

	when jtypeconst then
		additem(strmode(p.mode))
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

!	when jptr then
!		jeval(p.a)
!		additem("^")
!
!	when jptrto then
!		additem("^")
!		jeval(p.a)
!
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
			if p.cmpconds[i]=0 then exit fi
			additem(jtagnames[p.cmpconds[i]])
			jeval(q)
		od
	when jmaths then
		additem(mathsnames[p.mathsop]+3)
		additem("(")
		jeval(p.a)
		additem(")")

	elsif jflags[p.tag]=2 then
		strcpy(&.str,getopcname(p.tag))
		additem("(")
		jevallist(p.a)
		additem(&.str)
		jevallist(p.b)
		additem(")")

	elsif jflags[p.tag]=1 then
		strcpy(&.str,getopcname(p.tag))
		additem(&.str)
		additem("(")
		jevallist(p.a)
		additem(")")

	else
		CPL jtagnames[p.tag]
		loaderror("CAN'T DO JEVAL:",jtagnames[p.tag])
	end
end

proc jevallist(unit p)=
	unit q

	return unless p

	if p.nextunit then
		additem("(")
		q:=p
		while q do
			jeval(q)
			q:=q.nextunit
			if q then additem(",") fi
		od
		additem(")")
		return
	else
		jeval(p)
	fi
end

global proc additem(ichar s)=
!like genstr, but ensure there is white space separation as needed from the last output
	ichar d
	int lastchar,nextchar

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
!	if c in 'A'..'Z' or c in 'a'..'z' or c in '0'..'9' then
		return 1
	fi
	return 0
end

global function getopcname(int opc)ichar=
!op is a kcode representing an operator
!return the name as it might appear in J code
!caller must check for differences specific to the target
	jtagnames[opc]
end

global proc convertstring(ichar s, t)=
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
		end switch
	od
	t^:=0
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
		d.index:=nproclocals
!GERROR("CREATEAV/PROCLOCALS")
!CPL("CREATEAV/PROCLOCALS")

		pprocentry.n:=nproclocals
	fi							!else created at module level

	return p
end

global proc storemode(symbol owner, int m, ref i16 p)=
	ref userxrec q
!CPL "STOREMODE",STRMODE(M)
	p^:=m
	if m>=0 then return fi

	q:=pcm_alloc(userxrec.bytes)
	q.owner:=owner

	IF OWNER=NIL THEN
		SERROR("STOREMODE/OWNER=0")
	FI

	q.pmode:=p
	q.nextmode:=userxmodelist
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

global function testelem(ref[0:]byte p,int n)int =
!caller must check that n is in range
	return ((p^[n>>3] iand bytemasks[n iand 7])|1|0)
end

global proc setelem(ref[0:]byte p,int n) =
	p^[n>>3] ior:= bytemasks[n iand 7]
end

global proc setelemblock(ref[0:]byte p, int a,b) =
	int ax, bx, nwords, nx, alast,bfirst
	ref u64 q

	if a>b then return fi

    ax:=a iand inot 63
    bx:=b iand inot 63 + 64
	nx:=ax				!start of whole words
	alast:=bfirst:=-1

	nwords:=(bx-ax)/64

	if nwords=1 then
		if ax<>a or b<>(bx-1) then		!neither aligned, sequence is inside one word
			for i:=a to b do
				setelem(cast(p),i)
			od
			return
		fi
	else								!2 words or more
		if ax<>a then					!a not aligned
			--nwords
			nx:=ax+64
			alast:=nx-1
		fi
		if b<>bx-1 then					!b not aligned
			--nwords
			bfirst:=b iand inot 63
		fi

	fi

	if alast>=0 then					!part-word elements before whole words
		for i:=a to alast do
			setelem(cast(p),i)
		od
	fi

	q:=cast(&p[nx>>3])
	to nwords do
		q^:=0xFFFF'FFFF'FFFF'FFFF
		++q
	od

	if bfirst>=0 then				!part-word elements after whole worlds
		for i:=bfirst to b do
			setelem(cast(p),i)
		od
	fi
end

global function ispoweroftwo(i64 x)int=
!when x is a power of two, and is at least 2, then return the power (ie. equiv number of shifts)
!otherwise return zero when x is negative, 0, 1, not a power of two, or more than 2**31
	i64 a
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

global proc deleteunit(unit p,q)=
!delete p, replace by q, so that what was addressed by p now contains q
	unit r:=p.nextunit
	p^:=q^
	p.nextunit:=r
end

global proc skipsemi=
	while lx.symbol=semisym do lex() od
end

global proc checksymbol(int symbol)=
	[100]char str

	if lx.symbol<>symbol then
		fprint @&.str,"# expected, not #",symbolnames[symbol]:"m",symbolnames[lx.symbol]:"m"
		serror(&.str)
	fi
end

global proc skipsymbol(int symbol)=
	checksymbol(symbol)
	lex()
end

global proc pcnotmut=
	pcerror("Not mutable")
end

global func getpcloffset(pcl p, q)int=
	(ref byte(p)-ref byte(q))/pclrec.bytes
end
=== qq_lists.m 0 0 12/48 ===
global object emptylist

proc start=
	emptylist:=obj_new()
	emptylist.lower16:=1
	emptylist.objtype:=normal_obj
end

global proc var_empty_list(int lower, variant dest) =
	object p
	dest.objptr:=obj_newlist(0, lower)
	dest.tagx:=tlist ior hasrefmask
end

global proc var_make_list(variant a, dest, int n, lower) =
!create a list of n vars starting from a in reverse order (a is the last)
!put the result in dest (note this will be the last/first of the n vars)
	object p
	variant b

	p:=obj_newlist(n, lower)

	b:=p.varptr

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
	if lower not in -32768..32767 then pcerror("List LWB not 16-bit") fi
	p.lower16:=lower
	p.length:=n
	p.objtype:=normal_obj

	if n then
		p.varptr:=a:=pcm_alloc(n*varrec.bytes)
		p.alloc64:=allocbytes/varrec.bytes

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

	q:=p.varptr

	to p.length do
		var_unshare(q)
		++q
	od
	if p.length then
		pcm_free(p.varptr,p.alloc64*varrec.bytes)
	fi

	pcm_free32(p)
end

global proc var_getix_list(variant a, int index)=
!put result into a (which will be on the stack)
	variant p
	object q
	word offset
	int lower

	q:=a.objptr

	lower:=q.lower16

	offset:=index-lower
	if offset>=word(q.length) then
		pcerror("getlist[int] bounds")
	fi

	a^:=(q.varptr+offset)^
	var_share(a)
end

global proc var_getslice_list(variant a, int i,j)=
	varrec v,v2
	int alower
	object p,q

	p:=a.objptr

!CPL "LIST SLICE",=I,=J,=P.ULIST.LENGTH,=P.ULIST.VARPTR

	alower:=p.lower16

	if i<alower or j>p.length+alower-1 or i>j then
		pcerror("list/slice bounds")
	fi

	q:=obj_new()

	v.objptr:=q
	q.objtype:=slice_obj
	q.mutable:=p.mutable
	q.lower16:=1
	q.varptr:=p.varptr+i-alower

	case p.objtype
	when slice_obj then				!slice of a slice!
		q.objptr2:=p.objptr2		!link to original
		obj_shareu(q.objptr2)

	when extslice_obj then
		q.objptr2:=nil
		q.objtype:=extslice_obj
	else
		q.objptr2:=p				!link to original
		++p.refcount
!		var_shareu(a)
	esac

	q.length:=j-i+1
	a.objptr:=q
!	var_unshare(a)
end

global proc var_getixref_list(variant a, int index)=
	variant p
	object q
	word offset
	varrec v

	q:=a.objptr

	offset:=index-q.lower16

	if offset>=word(q.length) then
		if int(offset)<0 then
			pcerror("&list[int] lwb")
		elsif offset=q.length then
			if q.objtype<>normal_obj then pcerror("Can't extend slice/ext") fi
			v.tagx:=tvoid
			obj_append_list(q,&v)
!			return
		else
			pcerror("putlist[int] bounds")
		fi
	fi

	p:=q.varptr+offset
!	var_unshare(a)			!a should not disappear; rvalues can't have & applied

	a.tagx:=trefvar
	a.varptr:=p
end

global proc var_putix_list(variant a, int index, variant x)=
	variant dest
	object q
	word offset
	int lower

	q:=a.objptr

	if not q.mutable then pcnotmut() fi

	offset:=index-q.lower16

	if offset>=word(q.length) then
		if int(offset)<0 then
			pcerror("putlist[int] lwb")
		elsif offset=q.length then
			if q.objtype<>normal_obj then pcerror("Can't extend slice/ext") fi
			obj_append_list(q,x)
			return
		else
			pcerror("putlist[int] bounds")
		fi
	fi

	dest:=q.varptr+offset
	var_unshare(dest)
	dest^:=x^				!xfer ownership	
end

global proc var_putslice_list(variant a, int i,j, variant x)=
!insert a substring into a
	variant r,s
	object p,q
	int length,sublength

	p:=a.objptr
	if not p.mutable then pcnotmut() fi
	length:=p.length

	if i<1 or j>p.length or i>j then
		pcerror("list/slice bounds")
	fi
	sublength:=j-i+1

	q:=x.objptr
	if q.length<sublength then
		pcerror("substr too short")
	fi

	r:=p.varptr+i-1
	s:=q.varptr
	to sublength do
		r^:=s^
		var_share(r)
		++r
		++s
	od

end

proc obj_append_list(object a, variant x)=
!do in-place append of b to list a
	int n

	if a.objtype<>normal_obj then
		pcerror("Can't extend slice")
	fi

	if not a.mutable then
		pcnotmut()
	fi

	n:=a.length+1			!new length

	if n>a.alloc64 then		!need more space
		obj_resize_list(a,n)
	else
		a.length:=n
	fi

	if x then
!!REF VOID P,Q
!!P:=REF VOID(A.VARPTR+N-1)
!!Q:=REF VOID(X)
!!MEMCPY(P,Q,16)
!STATIC INT XX
!XX:=12345678
!XX:=0X12345678
!
!freddy:
		(a.varptr+n-1)^:=x^		!transfers ownership
	fi

end

global proc obj_resize_list(object p,int n)=
	variant q
	u32 allocated

	if n<=p.alloc64 then
		p.length:=n
	else
		q:=pcm_alloc(n*varrec.bytes)
!		p.alloc64:=allocbytes/varrec.bytes
		allocated:=allocbytes/varrec.bytes
		if p.length then
			memcpy(q,p.varptr,p.length*varsize)
			pcm_free(p.varptr, p.alloc64*varsize)
		fi
		p.varptr:=q
		p.length:=n
		p.alloc64:=allocated
	fi
end

global proc var_appendto_list(variant a, x)=
!a is a list (was a pointer)
	obj_append_list(a.objptr,x)
end

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

	if q.length=0 then return fi

	qlist:=q.varptr:=pcm_alloc(p.length*varrec.bytes)
	q.alloc64:=allocbytes/varrec.bytes	!probably same as p.alloc64	
	plist:=p.varptr

	to q.length do
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

	oldlength:=q.length
	newlength:=oldlength*m

	if oldlength=0 then return fi

	if newlength<0 then
		pcerror("list*int <0")
	elsif newlength=0 then
		p.objptr:=obj_newlist(0,q.lower16)
		return
	fi

	r:=obj_newlist(newlength, q.lower16)
	a:=r.varptr
	b:=q.varptr
	n:=0

	to newlength do
		a^:=b^
		var_share(a)
		++a
		if oldlength>1 then
			++b
			if ++n=oldlength then
				b:=q.varptr
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

	if px=py then return 1 fi			!same object

	xlen:=px.length
	ylen:=py.length

	if xlen<>ylen then return 0 fi		!unequal lengths

	if xlen=0 then return 1 fi			!both empty

	a:=px.varptr
	b:=py.varptr

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
		pcnotmut()
	fi

	pb:=b.objptr

	alen:=pa.length
	blen:=pb.length

	if alen=0 then					!concat to empty list
		if blen then				!copy b to a (else leave a as empty)
!global proc obj_resize_list(object p,int n)=
			obj_resize_list(pa,blen)
			d:=pa.varptr
			memcpy(d,pb.varptr,blen*varsize)
			to blen do
				var_share(d)
				++d
			od
		fi
	elsif blen then					!neither list is empty (else leave a unchanged)
		newlen:=alen+blen
!		list_resize(pa,newlen)
		obj_resize_list(pa,newlen)
		d:=pa.varptr+alen
		memcpy(d,pb.varptr,blen*varsize)
		to blen do
			var_share(d)
			++d
		od
	fi
end

global function var_inx_list(variant a,b)int =
	int n:=b.objptr.length
	int lowerm1:=b.objptr.lower16-1
	variant x:=b.objptr.varptr

	for i to n do
		if var_equal(a,x)=1 then
			return i+lowerm1
		fi
		++x
	od
	return i64.min
end
=== qq_modules.m 0 0 13/48 ===

global func loadsp(ichar filename, source=nil)isubprog sp=
!source = nil:  load lead module and dependencies from given sourcefile
!source <> nil: source code is given directly. filename can give a name
! to that source text, or if nil, and internal name is applied

	const maxmods=100
	const maxsubs=100
	[maxmods]ichar modnames
	[maxsubs]ichar subnames
	int nmods:=0, nsubs:=0
	int firstmod, lastmod, issyslib:=0
	ifile pm
	symbol d
	[300]char path

!CPL "LOADSP",FILENAME,=SYSLIBNAME

	if source then
		pm:=loadstring(filename, source)
		path[1]:=0
	else
!CPL "////",=EXTRACTBASEFILE(FILENAME), =SYSLIBNAME
		strcpy(path, extractbasefile(filename))
!CPL "////",=PATH, EXTRACTBASEFILE(SYSLIBNAME)
		if syslibname and eqstring(path, extractbasefile(syslibname)) then
			issyslib:=1
		fi

		pm:=loadsourcefile(filename, issyslib)
		if pm=nil then
			loaderror("Can't load lead module: #", filename)
		fi
		strcpy(path, pm.path)
	fi

	for i to nsubprogs do
		if eqstring(pm.name, subprogs[i].name) then
			loaderror("Subprog already loaded: #", sp.name)
		fi
	od

!reader header info
	startlex(pm)

	do
		lex()
		skipsemi()
		case lx.symbol
		when kmodulesym then
			lex()
			checksymbol(namesym)
			if not eqstring(lx.symptr.name, pm.name) then
				if nmods>=maxmods then loaderror("Too many modules in header") fi
				modnames[++nmods]:=lx.symptr.name
			fi

		when kimportsym then
			lex()
			checksymbol(namesym)
			if nsubs>=maxsubs then loaderror("Too many imports in header") fi
			subnames[++nsubs]:=lx.symptr.name

		when semisym then
		else
			exit
		esac
	od

!process nested imports
	for i to nsubs do
		if eqstring(subnames[i],pm.name) then loaderror("Importing self") fi
		compile_sp(getmodulefilename("", subnames[i]))				!recursive load
	od

!create new subprog entry
	if nsubprogs>=maxsubprog then loaderror("Too many subprogs") fi
	sp:=pcm_allocz(subprogrec.bytes)
	subprogs[++nsubprogs]:=sp
	sp.subprogno:=nsubprogs

	firstmod:=nmodules+1
	lastmod:=firstmod+nmods
	if lastmod>maxmodule then loaderror("Too many modules") fi
	nmodules:=lastmod
	pm.subprogno:=nsubprogs
	pm.islead:=1
	pm.moduleno:=firstmod
	pm.stmodule:=d:=createdupldef(stprogram,addnamestr(pm.name),moduleid)
	d.moduleno:=firstmod

	sp.name:=pm.name
	sp.path:=pm.path
	sp.filespec:=pm.filespec
	sp.firstmodule:=firstmod
	sp.lastmodule:=lastmod
	sp.issyslib:=issyslib

!create new set of modules[] entries and load those other modules
!create stmodule entries for each module
	modules[firstmod]:=pm

	for i to nmods do
		pm:=loadsourcefile(getmodulefilename(path, modnames[i]), issyslib)
		if not pm then
			loaderror("Can't load: ##",modnames[i])
		fi
		modules[firstmod+i]:=pm
		pm.stmodule:=d:=createdupldef(stprogram,addnamestr(pm.name),moduleid)
		pm.subprogno:=nsubprogs
		d.moduleno:=pm.moduleno:=firstmod+i
	od

	return sp
end

func getmodulefilename(ichar path, name)ichar =
	static [300]char str

!need to sort out search path etc

	strcpy(str, path)
	strcat(str, name)
	strcat(str, ".q")
	return str
end

global func loadsourcefile(ichar filespec, int issyslib=0)ifile pm=
	ichar s,basefilename
	[300]char str

!CPL "LOADSOURCEFILE", FILESPEC, =ISSYSLIB, =usebundled

	pm:=pcm_allocz(filerec.bytes)

	basefilename:=extractbasefile(filespec)


	pm.filespec:=pcm_copyheapstring(filespec)
	pm.path:=pcm_copyheapstring(extractpath(filespec))
	pm.name:=pcm_copyheapstring(basefilename)
	pm.issyslib:=issyslib

	if nqafiles and loadqafile(pm) then
		return pm
	fi

	if issyslib and usebundled then
		pm.issyslib:=issyslib
		if not loadsysmodule(pm) then
			loaderror("LS:Can't load syslib:",filespec)
		fi
!CPL "LOADED",FILESPEC,"FROM BUNDLE"
		return pm
	fi

!CPL "LOADING SOURCE", FILESPEC,"/",BASEFILENAME


	s:=cast(readfile(filespec))			!will overallocate by a few bytes
	if not s then				!unexpected error
		strcpy(str, "c:/m/libs/")
		strcat(str, basefilename)
		strcat(str, ".q")
!CPL "TRYING:",STR
		s:=cast(readfile(str))
		if not s then
			return nil
		fi
!		CPL "LOADED FROM MLIBS"
	fi
!CPL "LOADED",FILESPEC,"FROM FILE"
	pm.text:=s

	pm.size:=rfsize

	(s+rfsize)^:=0				!replace etx,0 by 0,0 (effectively, just zero)
	return pm
end

global func loadstring(ichar name, source)ifile pm=
	[16]char str
	static int nextstrname=0

	if name=nil then
		print @str, "S$",,++nextstrname
		name:=pcm_copyheapstring(str)
	fi

	pm:=pcm_allocz(filerec.bytes)

	pm.filespec:="<string>"
	pm.path:=""
	pm.name:=name

	pm.text:=source

	pm.size:=strlen(source)
	return pm
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

function loadqafile(ifile pm)int=
	ichar file
	[300]char filename

	strcpy(filename, extractfile(pm.filespec))
!	strcpy(filename, (pm.filespec))

	for i to nqafiles do
		if eqstring(filename,qafilenames[i]) then		!found
			pm.text:=qatext[i]
			pm.size:=qasize[i]
			return 1
		fi
	od
	return 0
end

global proc readqabundle=
	[100]char name
	ichar s, t
	int sys, support

	s:=extractext(inputfile)
	convlcstring(s)
	unless eqstring(s,"qa") then
		return
	end

!Input is a .qa file; load files into qa directory
	s:=readfile(inputfile)
	if s=nil then							!file not found on disk
		loaderror("Can't find QA file ##",inputfile)
	fi

!change inputfile from .qa to .q (later, change to suitable lead module within qa file)
	inputfile:=pcm_copyheapstring(changeext(inputfile,"q"))

	s:=readfileline(s+3)
	readstr(name,'n')
	if not eqstring(name,"qa") then
		loaderror("QA: bad header")
	fi

	--s					!point to previous lf

	s:=findnextlineheader(s)

	do
		if s=nil then
			loaderror("Unexpected EOF in QA file")
			exit
		fi
		s:=readfileline(s)
		readstr(name,'n')
		read sys,support
!		println "Found file",name
		if eqstring(name,"end") then
			exit
		fi
		if nqafiles>=maxqafile then
			loaderror("Too many QA files")
		fi

		t:=findnextlineheader(s)
		if t=nil then
			loaderror("QA error")
		fi

		++nqafiles

		qafilenames[nqafiles]:=pcm_copyheapstring(name)
		qasize[nqafiles]:=t-s-3
		qatext[nqafiles]:=s
		s:=t
	od
!
	for i to nqafiles do
		(qatext[i]+qasize[i])^:=0	
	od
end
=== qq_names.m 0 0 14/48 ===
!Symbol table handling

int sdsize, sdoffset
int sdaligned
int sdlevel
int sdmode
int sdnfields
int sdmaxalign
const int maxstructdepth=10
[maxstructdepth]byte sdunion		!1 if union model 0 for normal offset calc
[maxstructdepth]int sdmaxsize		!accumulate max size of union

global function addglobalname(ichar name)symbol=
!generic name encountered in namedef op. Convert to symbol reference
!will always return a generic strec, either existing, or just created
	lexrec oldlx
	symbol d

	oldlx:=nextlx

	lookup(name,strlen(name),gethashvaluez(name))

	d:=nextlx.symptr
	nextlx:=oldlx
	return d
end

function newstrec:symbol=
	symbol p
	p:=pcm_alloc(strec.bytes)
	memset(p,0,strec.bytes)

	return p
end

global function addsymbol(symbol owner,d, int id, isglobal)symbol e=
!d should be a generic symbol (or if nil, then when name is provided
!to create a suitable generic symbol0
!create a dedicated strec for it, link it in to the dupl chain of the generic
!version, and insert it a child of owner
	symbol f

	e:=newstrec()
	e.name:=d.name
	e.namelen:=d.namelen
	e.owner:=owner
	e.nameid:=id
	e.isframe:=id=frameid or id=paramid

!CPL "ADDSYM",=D.NAME
	if currmodule then
		e.moduleno:=currmodule.moduleno
	fi

	e.firstdupl:=d
	e.isglobal:=isglobal

!IF OWNER.NAMEID<>PROCID THEN
	return e when not owner			!not linked in to anything

IF OWNER.NAMEID NOT IN [PROCID, ANONPROCID] THEN
	e.nextdupl:=d.nextdupl
	d.nextdupl:=e


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

!global function createstroot(ichar name)symbol d=
!	d:=newstrec()
!	d.name:=pcm_copyheapstring(name)
!	d.namelen:=strlen(name)
!	d.nameid:=programid
!
!	return d
!end

global function newusertypex(ref strec d,e=nil)int=
	int i

	if nuserxtypes>=maxuserxtype then
		serror("Too many external user types")
	fi
	++nuserxtypes
	ttnamedefx[nuserxtypes]:=d
!	ttnamedefx2[nuserxtypes]:=e

	ttxmoduleno[nuserxtypes]:=stcurrmodule.moduleno
	return -nuserxtypes
end

global function resolvedottedname(symbol owner, d)symbol e=
!d should be generic

	e:=d.nextdupl
	while e and e.owner<>owner do
		e:=e.nextdupl
	od

	return e
end

global proc addgenfield(symbol d)=
	int index
	symbol dgen
	ref genfieldrec g


	dgen:=d.firstdupl
	index:=dgen.genfieldindex

!CPL "ADDGENFIELD",D.NAME,namenames[d.nameid],=INDEX

	if index=0 then			!first field with this name
		if ngenfields>=maxgenfield then
		gerror("Too many genfields")
		fi
		dgen.genfieldindex:=index:=++ngenfields
	fi

	g:=pcm_alloc(genfieldrec.bytes)
	g.def:=d
	g.nextdef:=genfieldtable[index]
	genfieldtable[index]:=g
end

!global function addusertype(symbol d)int=
!!d is the name of a new user type; the details have been set up inside it
!!but now create the actual type
!
!	if ntypes>=maxtype then pcerror("Too many types") fi
!
!	++ntypes
!	d.mode:=ntypes
!	ttnamedef[ntypes]:=d
!	ttname[ntypes]:=d.name
!
!	return ntypes
!
!end

global function makereftype(int target, symbol owner=nil)int=
!owner <> nil means used for new type so cannot reuse existing ref

	int newtype

	if owner=nil then
		for i:=tlast+1 to ntypes do
			if ttbasetype[i]=trefpack and tttarget[i]=target then
				return i
			fi
		od
	fi

	newtype:=addanontype()
	ttbasetype[newtype]:=trefpack

	storemode(stcurrproc,target,&tttarget[newtype])

	ttsize[newtype]:=8
	ttbitwidth[newtype]:=64
!	ttcat[newtype]:=refcat
	return newtype
end

global function makeaxtype(int target, unit plower, plength)int=
	int newtype,length

	newtype:=addanontype()

	ttbasetype[newtype]:=tvector
	storemode(stcurrproc, target, &tttarget[newtype])

	ttlower[newtype]:=1
	ttlengthexpr[newtype]:=plength
	ttlowerexpr[newtype]:=plower
!	ttcat[newtype]:=blockcat			!may be adjusted later
!	ttsize[newtype]:=length*ttsize[target]



	return newtype
end

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

	if ntypes>=maxtype then gerror("Too many types") fi

	++ntypes
!CPL "ADDANONTYPE",NTYPES
	print @str,"$T",,ntypes

!CPL "ADDANON TYPE", STCURRPROC.NAME
	ttname[ntypes]:=pcm_copyheapstring(str)
	ttowner[ntypes]:=stcurrproc

	return ntypes

end

global proc createusertype(symbol d, int m)=
	storemode(stcurrproc,m,&d.mode)

	if m>tlast and ttnamedef[m]=nil then
		ttnamedef[m]:=d
		ttname[m]:=d.name
		ttowner[m]:=d.owner
!		ttcat[m]:=gettypecat(m)
	fi
end

!global function roundoffset(int offset, alignment)int=
!	int mask
!
!	if alignment=1 then return offset fi
!	mask:=alignment-1
!	while offset iand mask do ++offset od
!
!	return offset
!end

global function getalignment(int m)int=
!return alignment needed for type m, as 1,2,4,8
	int a

	case ttbasetype[m]
	when tvector then
		return getalignment(tttarget[m])
!	when tpstruct then
	when tstruct then
!		return ttnamedef[m].structmaxalign
	esac

	a:=ttsize[m]
	case a
	when 1,2,4,8 then
		return a
	esac
	cpl ttname[m],a
	gerror("Getalign not 1248")

	return 0
end

global proc duplfield(ref strec p,q)=
!p is strec of an existing field, const etc
!q is a newly created strec with the same id and name
!copy the relevant fields of p to q

	if p.code then
		serror("DUPLFIELD")
	fi
	q.atfield:=p.atfield
	q.index:=p.index
	q.fieldoffset:=p.fieldoffset
end

proc writesig(symbol d, filehandle dev)=
	symbol e
	int n
	fprint @dev, "# #(", (d.misfunc|"function"|"proc"), d.name

	e:=d.deflist
!CPL "PARAMS",D.NPARAMS
	n:=0
	while e, e:=e.nextdef do
		if e.nameid=paramid then
			++n
			if e.moptional and e.code then
				fprint @dev,"#=#", e.name, strexpr(e.code).strptr
			elsif e.moptional then
				print @dev,"?",,e.name
			else
				print @dev,e.name
			fi

			if n<d.nparams then
				print @dev,", "
			fi
		fi
	od

	fprintln @dev,")	[#]", d.owner.name

end

global function createdupldef(symbol owner,symptr, int id)symbol=
!create new proc entry
!symptr is the generic st entry for proc's name
	symbol p,q

	p:=newstrec()

	p.name:=symptr.name
	p.namelen:=symptr.namelen
	p.symbolcode:=namesym
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
=== qq_packed.m 0 0 15/48 ===
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

	dest.tagx:=tint

	switch ttbasetype[t]
	when ti8 then
!		dest.tagx:=tint
		dest.value:=ref i8(p)^

	when ti16 then
!		dest.tagx:=tint
		dest.value:=ref i16(p)^

	when ti32 then
!		dest.tagx:=tint
		dest.value:=ref i32(p)^

	when ti64 then
!		dest.tagx:=tint
		dest.value:=ref i64(p)^

	when tu8 then
!		dest.tagx:=tint
		dest.value:=ref byte(p)^

	when tu16 then
!		dest.tagx:=tint
		dest.value:=ref u16(p)^

	when tu32 then
!		dest.tagx:=tint		!BETTER I64
		dest.value:=ref u32(p)^

	when tu64 then
		dest.tagx:=tint
		dest.uvalue:=ref u64(p)^

	when tr64 then
		dest.tagx:=treal
		dest.xvalue:=ref r64(p)^

	when tr32 then
		dest.tagx:=treal
		dest.xvalue:=ref r32(p)^

	when tpackstrc then
		dest.tagx:=tstring ior hasrefmask
		length:=ttlength[t]
		if length>=2 then		!normal fixed string
			length:=getfslength(p,length)
		else				!assume string basetype: char target (length can be 0)
			length:=1
		fi
!		s:=make_strslicexobj(p,length)
		s:=obj_make_strslicexobj(p,length)
		dest.objptr:=s

	when tpackstrz then		!zero-terminated string
		dest.tagx:=tstring ior hasrefmask
		ss:=p
		to ttlength[t] do
			exit when ss^=0
			++ss
		od

		s:=obj_make_strslicexobj(p,ss-ref char(p))
		dest.objptr:=s

	elsecase ttbasetype[t]
	when trefpack then
		dest.tagx:=trefpack
		dest.ptr:=cast(ref i64(p)^)
		dest.elemtag:=tttarget[t]

	when tstruct then
		s:=obj_new()
		s.mutable:=1
		s.ptr:=p
!	dostruct:
		dest.objptr:=s
		dest.tagx:=tstruct ior hasrefmask
		s.usertag:=t
		if ownerobj then
			s.objtype:=slice_obj
			s.objptr2:=ownerobj
			++ownerobj.refcount
		else
			s.objtype:=extslice_obj
		fi
	when tvector then
!global function obj_newarray(int elemtype, lower,length)object p=
		s:=obj_newarray(tttarget[t],ttlower[t],ttlength[t])
		s.mutable:=1
		s.ptr:=p
		dest.objptr:=s
		dest.tagx:=tvector ior hasrefmask
		s.usertag:=t
		if ownerobj then
			s.objtype:=slice_obj
			s.objptr2:=ownerobj
			++ownerobj.refcount
		else
			s.objtype:=extslice_obj
		fi
	else
		pcmxtypestt("loadpacked",ttbasetype[t],t)
	end switch
end

global proc var_storepacked(ref byte p,variant q,int t) =
!p points directly to a packed value of type t, which is to receive a value currently
!in variant q

	int plength,qlength
	int s,sbase,tbase
	object qa

	s:=sbase:=q.tag		!storing coercible sbase type to fixed type tbase
	tbase:=ttbasetype[t]

	switch sbase
	when tint, trefpack then
		switch tbase
!		switch t
		when ti8,tu8 then
			(ref byte(p)^):=q.value
			return
		when ti16,tu16 then
			(ref u16(p)^):=q.value
			return
		when ti32,tu32 then
			(ref i32(p)^):=q.value
			return
		when ti64,tu64,trefpack then
			(ref i64(p)^):=q.value
			return
		when tr32 then
			(ref r32(p)^):=q.value
			return
		when tr64 then
			(ref r64(p)^):=q.value
			return
		end switch

	when treal then
		switch tbase
		when ti32,tu32 then
			(ref i32(p)^):=q.xvalue
			return
		when ti64,tu64 then
			(ref i64(p)^):=q.xvalue
			return
		when tr32 then
		(ref r32(p)^):=q.xvalue
			return
		when tr64 then
			(ref r64(p)^):=q.xvalue
			return
		when ti16,tu16 then
			(ref i16(p)^):=q.xvalue
			return
		end switch

	when tstring then
		qa:=q.objptr
		plength:=ttlength[t]
		qlength:=qa.length
		switch tbase
!		when tstring then			!ref string assumed here to mean special 1-char string
!			if t=tbase then			!if basetype, then means special 1-char string
!				if qlength<>1 then
!					pcerror("Str not len 1")
!				fi
!				(ref char(p)^):=ref char(qa.strptr)^
!				return
!			fi
!			if qlength>plength then		!truncate
!				qlength:=plength
!			fi
!			memcpy(p,qa.strptr,qlength)		!copy the number of chars provided
!			setfslength(cast(p),plength,qlength)
!			return
!
		when tpackstrc then			!ref string assumed here to mean special 1-char string
			if t=tbase then			!if basetype, then means special 1-char string
				if qlength<>1 then
					pcerror("Str not len 1")
				fi
				(ref char(p)^):=ref char(qa.strptr)^
				return
			fi
			if qlength>plength then		!truncate
				qlength:=plength
			fi
			memcpy(p,qa.strptr,qlength)		!copy the number of chars provided
			setfslength(cast(p),plength,qlength)
			return

		when tpackstrz then
			if qlength>=plength then			!truncate as needed; no teminator to allow space for terminator
				memcpy(p,qa.strptr,plength)		!copy the number of chars provided
				(ref byte(p)+plength-1)^:=0			!zero terminator

			else
				memcpy(p,qa.strptr,qlength)		!copy the number of chars provided
				(ref byte(p)+qlength)^:=0			!zero terminator
			fi

			return

		end switch

	when tstruct then
		s:=q.objptr.usertag
		if s<>t then
			pcmxtypestt("spack struct",s,t)
		fi
		memcpy(p,q.objptr.ptr,ttsize[t])
		return

	when tvector then
		s:=q.objptr.usertag
		if s<>t then				!not direct match: check whether compatible
				pcmxtypestt("spack array",s,t)
		fi
		memcpy(p,q.objptr.ptr,ttsize[t])
		return

	end switch

	pcmxtypestt("storepacked (source->dest)",s,t)
end

proc setfslength(ref char s,int m,n) =		!SETFSLENGTH
!set up lengthcode of fixed string starting at s, of maximum length m, with actual length n
!a,b are the last two chars of the fixed string:
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

	b:=p.varptr

	m:=ttlength[rectype]
	d:=ttnamedef[rectype]
	r:=d.topfieldlist

	if n<m then
		pcerror("Too few elements")
	elsif n>m then
		println =n,=m
		pcerror("Too many elements")
	fi

	q:=p.ptr

	to n do
		var_storepacked(q, a, r.mode)
		q+:=ttsize[r.mode]
		++r
		++a
	od

	dest.tagx:=tstruct ior hasrefmask
	p.usertag:=rectype
	dest.objptr:=p
end

global function obj_new_struct(int m)object p=
	int size

	p:=obj_new()
	p.mutable:=1
	p.usertag:=m

	size:=ttsize[m]
	if size then
		p.ptr:=pcm_allocz(size)
	fi

	return p
end

global proc var_dupl_struct(variant a)=
	object p,q
	int size

	p:=a.objptr
!
	size:=ttsize[p.usertag]
	q:=obj_new_struct(p.usertag)
	a.objptr:=q

	memcpy(q.ptr, p.ptr, size)
end

global proc obj_free_struct(object p)=
	pcm_free(p.ptr, ttsize[p.usertag])
	pcm_free32(p)
end

global function var_equal_struct(variant x,y)int=
!assume tags match

	return eqbytes(x.objptr.ptr, y.objptr.ptr, ttsize[x.tag])
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

	d:=ttnamedef[p.usertag]
	r:=(d.topfieldlist+index-1)

	var_loadpacked(p.ptr+r.fieldoffset, r.mode, a)
end
=== qq_parse.m 0 0 16/48 ===
!Parser

int intabledata
ichar tabledataname=nil

const maxdollarstack=10
[maxdollarstack]unit dollarstack		!used for a[$]
int ndollar=0
byte yieldseen

macro readunit=readexpression()
macro readxunit=readunit

int currdllindex
int nextlambdaindex

const maxlisttype=20
[maxlisttype]int listtypestack
int nlisttype
int listtype				!0 / 'PARAM' / 'PRINT' / 'DICT'

global proc parsemodule(ifile pm)=
	unit p

	return when pm.compiled

	currmodule:=pm
	stcurrmodule:=currmodule.def

	startlex(currmodule)

!CPL "PARSE", PM.NAME

	lex()
	lex()
!INT TT:=CLOCK()
!INT NN:=0
!REPEAT
!	LEX()
!++NN
!UNTIL LX.SYMBOL=EOFSYM
!TT:=CLOCK()-TT
!CPL "LEX TIME", TT
!CPL =NN
!STOP

	stcurrproc:=stcurrmodule

	p:=readsunit()

	stcurrmodule.code:=pm.ast:=p

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

	if lx.symbol = assignsym then
		return readassignment(p)
	else
		return readorterms(p)
	fi
end

function readassignment(unit p)unit=
	int pos, isdeep
	unit q, r

	if exprendset[lx.symbol] then return p fi

	p:=readorterms(p)

	if lx.symbol = assignsym then
		isdeep:=lx.subcode
		pos:=lx.pos
		lex()
		p:=createunit2(jassign, p, readassignment(readterm2()))
		p.flag:=isdeep
		p.pos:=pos

	fi
	return p
end
!
function readorterms(unit p)unit =
	int pos
	unit q, r

	if exprendset[lx.symbol] then return p fi

	p:=readandterms(p)

	while lx.symbol=orlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jorlto, p, readunit())
			p.pos:=pos
			exit
		fi

		p:=createunit2(jorl, p, readandterms(readterm2()))
		p.pos:=pos
	od

	while lx.symbol=pipesym do
		lex()
		q:=r:=readterm2()
		if q.tag=jcall then
			r:=q.b
			while r.nextunit, r:=r.nextunit do od
			r.nextunit:=p
			p:=q
		else
			p:=createunit2(jcall, q, p)
		fi

	od

	return p
end

function readandterms(unit p)unit =
	int pos

	p:=readcmpterms(p)

	while lx.symbol=andlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jandlto, p, readunit())
			p.pos:=pos
			exit
		fi

		p:=createunit2(jandl, p, readcmpterms(readterm2()))
		p.pos:=pos
	od

	return p
end

function readcmpterms(unit p)unit =
!creates either jcmp unit (simple 2-operand cmp), or jcmpchain (3/4 operands)
	int pos, n
	unit px, q
	[4]byte conds

	p:=readinterms(p)

	if not cmpopset[lx.symbol] then
		return p
	fi

	clear conds
	px:=p
	p:=createunit1(jcmpchain, p)
	n:=0				!n counts operand after the first

	while cmpopset[lx.symbol] do
		++n
		if n>conds.len then serror("cmpchain: Too many items") fi
		conds[n]:=lx.subcode

		pos:=lx.pos
		lex()

		q:=readinterms(readterm2())
		px.nextunit:=q
		px:=q

		q.pos:=pos
	od

	if n=1 then
		p.tag:=jcmp
		p.condcode:=conds[1]
		q:=p.a
		p.b:=q.nextunit
		q.nextunit:=nil
	else
		p.cmpconds:=conds
	fi	

	return p
end

function readinterms(unit p)unit =
	int pos, tag, flag

	p:=readrangeterm(p)

	docase lx.symbol
	when insym, inxsym then
		tag:=(lx.symbol=insym|jin|jinx)
		flag:=lx.subcode			!in/1 means not in

		pos:=lx.pos
		lex()

		p:=createunit2(tag, p, readrangeterm(readterm2()))
		p.flag:=flag
		p.pos:=pos
	else
		exit
	end docase

	return p
end

function readrangeterm(unit p)unit =
	int pos

	p:=readaddterms(p)

	if lx.symbol=rangesym then
		pos:=lx.pos
		lex()
		p:=createunit2(jmakerange, p, readaddterms(readterm2()))
		p.pos:=pos
	fi

	return p
end

function readaddterms(unit p)unit =
	int pos, opc, a, b, tag
	unit q

	p:=readmulterms(p)
	while addopset[lx.symbol] do
		opc:=lx.subcode
		if lx.symbol=addrsym then
			opc:=kappend
		fi

		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
!CPL "RAT:="
			if opc=kappend then
				tag:=jappendto
			elsif opc=kconcat then
				tag:=jconcatto
			else
				tag:=jbinto
			fi
			p:=createunit2(tag, p, readassignment(readterm2()))
			p.pclop:=opc
			p.pos:=pos
			exit
		fi

		q:=readmulterms(readterm2())
		p:=createunit2(jbin, p, q)
		p.pclop:=opc
		p.pos:=pos
	od

	return p
end

function readmulterms(unit p)unit =
	int pos, opc, a, b
	unit q

	p:=readpowerterms(p)

	while mulopset[lx.symbol] do
		opc:=lx.subcode
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto, p, readassignment(readterm2()))
			p.pclop:=opc

			p.pos:=pos
			exit
		fi

		p:=createunit2(jbin, p, readpowerterms(readterm2()))
		p.pclop:=opc
		p.pos:=pos
	od

	return p
end

function readpowerterms(unit p)unit =
	int pos

	while lx.symbol=powersym do
		pos:=lx.pos
		lex()
		p:=createunit2(jbin, p, readpowerterms(readterm2()))
		p.pclop:=kpower
		p.pos:=pos
	od

	return p
end

function readterm2:unit p=
	int pos

	pos:=lx.pos
	p:=readterm()
	p:=readtermsuffix(p, pos)
	return p
end

function readtermsuffix(unit p, int pos)unit=
	unit q, r
	ref char pbyte
	u64 a
	int opc, oldipl, shift, t, nparams

	docase lx.symbol
	when lbracksym then
		lex()
		q:=readslist(nparams, 1)

		skipsymbol(rbracksym)
		p:=createunit2(jcall, p, q)
		p:=readcondsuffix(p)

	when ptrsym then
		p:=createunit1(jptr, p)
		lex()

	when lsqsym then
		p:=readindex(p, 0)

	when dotsym then
		p:=readdotsuffix(p)

	when lcurlysym then
		p:=readkeyindex(p)

	when colonsym then
		case listtype
		when 'PARAM' then
			lex()
			p:=createunit2(jkeyword, p, readunit())
		when 'DICT' then
			lex()
			p:=createunit2(jkeyvalue, p, readunit())
		else
			exit
		esac

	when incrsym then
		p:=createunit1(jloadincr, p)
		p.flag:=lx.subcode
		lex()

	else
		exit
	end docase

	p.pos:=pos

	return p
end

function readterm:unit=
	unit p, q, r
	ref char pbyte
	u64 a
	int oldipl, opc, oldinrp, pos, shift, t, nparams, length
	byte flag
	ichar s

	record dummy=
		union
			[20]char str
			i64 sa
		end
	end

	dummy ustr

	pos:=lx.pos

	switch lx.symbol
	when namesym then
		p:=createname(lx.symptr)
		p.pos:=lx.pos
		lex()

	when intconstsym then
		p:=createintunit(lx.value)
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

	when charconstsym then
		length:=strlen(lx.svalue)
		ustr.sa:=0
!		sa:=0
		if length>8 then
			serror("char const too long")
		fi
		memcpy(&.ustr.str, lx.svalue, length)
		p:=createintunit(ustr.sa)
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
				p:=createunit0(jvoid)
			fi
		else
			p:=readcast()

		fi

	when addsym then
		p:=checkoperator()
		if not p then
			lex()
			p:=readterm2()
		fi

	when subsym  then
		p:=checkoperator()
		if not p then
			lex()
			if lx.symbol=assignsym then
				opc:=kneg
				dounaryto
			fi
			p:=readterm2()
			if p.tag=jintconst then
				p.value:=-p.value
			else
				p:=createunit1(junary, p)
				p.pclop:=kneg
			fi
		fi

	when inotsym, abssym, ascsym, chrsym then
		p:=checkoperator()
		if not p then
			opc:=lx.subcode
			lex()
			if lx.symbol=assignsym then
dounaryto:
				lex()
				p:=createunit1(junaryto, readterm2())
				p.pclop:=opc
			else
				p:=createunit1(junary, readterm2())
				p.pclop:=opc
			fi
		fi

	when notlsym, istruelsym then
		p:=checkoperator()
		if not p then
			opc:=lx.subcode
			lex()
			if lx.symbol=assignsym then
				opc:=(opc=jnotl | jnotlto | jistruelto)
				lex()
			fi
			p:=createunit1(opc, readterm2())
		fi

	when incrsym  then
		p:=checkoperator()
		if not p then
			opc:=lx.subcode
			lex()
			p:=createunit1(jincrload, readterm2())
			p.flag:=opc
		fi

	when mathssym  then
		opc:=lx.subcode
		lex()
		p:=createunit1(jmaths, readterm2())
		p.pclop:=opc

	when mulsym, divsym, idivsym, iremsym, idivremsym, andlsym, orlsym, 
		iandsym, iorsym, ixorsym, shlsym, shrsym, insym, inxsym, 
		eqsym, nesym, ltsym, lesym, gesym, gtsym, powersym, appendsym, 
		concatsym, propsym, specialopsym then
		unless p:=checkoperator() then
			serror("Operator?")
		end

	when lsqsym then
		p:=readset()

	when minmaxsym then
		if p:=checkoperator() then
		else
			p:=readpair(jbin, lx.subcode)
		fi

	when maths2sym then
		if p:=checkoperator() then
		else
			p:=readpair(jmaths2, lx.subcode)
		fi

	when ksprintsym then
		p:=readsprint()

!	when ksreadsym then
!		p:=readsread()
!
	when addrsym, ptrsym then
		flag:=lx.subcode
		lex()
		p:=createunit1(jaddrof, readterm2())
		p.flag:=flag
		if p.a.tag=jcall then
			if p.a.b then
				serror("Params not allowed")
			fi
			p.a:=p.a.a			!lose the call
		fi

	when compilervarsym then
		p:=readcompilervar()
		lex()

	when dollarsym then
		if intabledata then
			if tabledataname=nil then serror("$:No enum") fi
			s:=tabledataname
			if nextlx.symbol=addsym then
				lex()
				lex()
				checksymbol(intconstsym)
				s+:=lx.value
			fi
			p:=createstringunit(s, -1)
		else
			if ndollar<=0 then
				serror("[$] No array")
			fi
			p:=createunit1(jproperty, dollarstack[ndollar])
			p.pclop:=kupb
		fi
		lex()

	when dotsym, kglobalsym then
		lexchecksymbol(namesym)
		p:=createname(lx.symptr)
		p.pos:=lx.pos
		lex()

	when kmapsym then
!		p:=readmap()
		p:=readpair(jmap)

	when kclampsym then
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		skipsymbol(commasym)
		q:=readunit()
		if lx.symbol=rbracksym and q.tag=jmakerange then
			r:=q.b
			q:=q.a
		else
			skipsymbol(commasym)
			r:=readunit()
			checksymbol(rbracksym)
		fi
		lex()

		q:=createunit2(jbin, p, q)
		q.pclop:=kmax
		p:=createunit2(jbin, q, r)
		p.pclop:=kmin

	when kgotosym then
		p:=readgoto()

	when kifsym then
		p:=readif()

	when kunlesssym then
		p:=readunless()

	when kcasesym, kswitchsym then
		p:=readswitchcase()

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

	when kprintsym, questionsym then
		p:=readprint()

	when kreadsym then
		p:=readread()

	when ktrysym then	!todo
		p:=readtry()
!
	when kraisesym then	!todo
		lex()
		p:=createunit1(jraise, readunit())

	when kswapsym then			!swap using function syntax
		p:=readpair(jswap)
!
	when khostfnsym then
		p:=readhostparams(nil, 1)

	when knilsym then
!		p:=createunit0((lx.subcode=1|jpnil|jnil))
		p:=createunit0(jnil)
		lex()

	when kstrincludesym then
		lex()
		p:=createunit1(jstrinclude, readterm2())

	when kevalsym then
		lex()
		p:=createunit1(jeval, readunit())


	when lcurlysym then
		p:=readlambda()

	else

error:
		cpl symbolnames[lx.symbol]:"d"
		serror("readterm?")
	end switch

	p.pos:=pos
	return p
end

function readsunit(int inwhile=0)unit=
	int lineno, m, globalflag, staticflag
	unit ulist, ulistx, p, q, r
	symbol stname

	lineno:=lx.pos
	ulist:=ulistx:=nil
	globalflag:=local_scope
	staticflag:=0

	repeat
		while lx.symbol=semisym do
			lex()
		od

		switch lx.symbol
		when kstaticsym then
			lex()
			staticflag:=1
			redoloop

		when kglobalsym then
			if globalflag then serror("global global?") fi
			globalflag:=lx.subcode
			lex()
			redoloop

		when kprocsym, kfunctionsym then
			readprocdef(globalflag)
			globalflag:=local_scope

		when kvarsym then
			q:=readvardef(globalflag, staticflag)
			while q do								!initialised decls involve code
				r:=q.nextunit						!unlink from this block first
				q.nextunit:=nil
				addlistunit(ulist, ulistx, q)			!add one by-one
				q:=r
			od

			globalflag:=staticflag:=local_scope

		when kconstsym then
			if staticflag then serror("static?") fi
			readconstdef(globalflag)
			globalflag:=local_scope

		when ktypesym then
			readtypedef(globalflag)
			globalflag:=local_scope

		when krecordsym, kstructsym then
			readrecorddef(globalflag, nil)
			globalflag:=local_scope

		when ktabledatasym then
			readtabledef(globalflag)
			globalflag:=local_scope

		when kimportdllsym then
			readimportdll()

		when kmacrosym then
			readmacrodef(globalflag)
			globalflag:=local_scope

		when eofsym then
			exit

!these are needed to check for an empty sunit preceding
		when rbracksym, kthensym, kelsifsym, kelsesym, kuntilsym, kwhensym, sendtosym, 
				kelsecasesym, kelseswitchsym, kexceptsym, kendsym, rcurlysym then
			exit

		when namesym then
			case nextlx.symbol
!		when dcolonsym, colonsym then
			when colonsym then
				p:=createunit1(jlabeldef, createname(addsymbol(stcurrproc, lx.symptr, labelid, 0)))
				lex()
				lx.symbol:=semisym
				addlistunit(ulist, ulistx, p)
!		when namesym then
!			goto dovar
			else
				goto doexec
			esac
		when kdosym then				!u;u;u;do rather than u;u;u do
			if inwhile then
				exit
			fi
			goto doexec

		when kmodulesym, kimportsym then
			repeat
				lex()
			until lx.symbol=semisym

		when semisym then
		when lsqsym then
			doexec
!	elsif istypestarter() and nextlx.symbol<>lbracksym then
!		goto dovar

		else							!assume a statement

	doexec:
			p:=readunit()

			if p.tag=jname and lx.symbol=namesym then
				serror("Possibly var/let needed")
			fi
			addlistunit(ulist, ulistx, p)
			if lx.symbol=kdosym then
				exit
			fi

		end switch

	until lx.symbol<>semisym

	case lx.symbol
	when rbracksym, kthensym, kelsifsym, kelsesym, kuntilsym, kwhensym, kdosym, sendtosym, 
		kelsecasesym, kelseswitchsym, kexceptsym, kendsym, rcurlysym, commasym, 
		barsym, eofsym then
	else
		serror("Readsunit: "";"" expected, or bad unit starter")
	esac

	if ulist=nil or ulist.nextunit then			!empty or multiple => block
		return createunit1(jblock, ulist)
	else
		return ulist							!single => one unit
	fi
end

proc checkequals=
!check that "=" is current symbol
	if lx.symbol<>eqsym then
		serror("""="" expected")
	fi
end

function readindex(unit p, int dot)unit=
!at '['; dot=0/1 for a[]/a.[]
!syntax is:
![x] or [x, ...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
	unit q, plower, pupper

	lex()

	do
		if ndollar>=maxdollarstack then
			serror("Too many nested a[$]")
		fi
		dollarstack[++ndollar]:=p
		q:=readunit()
		--ndollar

		p:=createunit2((dot|jdotindex|jindex), p, q)

		exit when lx.symbol<>commasym
		lex()
	od
	skipsymbol(rsqsym)
	return p
end

function readdotsuffix(unit p)unit=
!at '.' symbol
!read any modifiers for term currently in p
!multiple .terms can be present
	unit q
	int t
	byte flag

	while lx.symbol=dotsym do
		lex()
		case lx.symbol
		when lsqsym then
			p:=readindex(p, 1)
		when namesym then
			p:=createunit2(jdot, p, createname(lx.symptr))
			lex()
		when  propsym then			!ought to check whether op is allowed in this form
doprop:
!CPL "DOT PROP", JTAGNAMES[LX.SUBCODE]
			p:=createunit1(jproperty, p)
			p.pclop:=lx.subcode
			lex()

		when ktypesym then
			if p.tag<>jtypeconst then
				flag:=1
dogettype:
				p:=createunit1(jgettype, p)
				p.pclop:=flag
			fi
			lex()

		when minmaxsym then
			lx.subcode:=(lx.subcode=kmin|kminval|kmaxval)
			doprop

!		when minsym then
!			lx.subcode:=kminval
!			doprop

		when miscpropsym then
			case lx.subcode
			when 'b' then flag:=0; dogettype
			when 'e' then flag:=2; dogettype
			else
				p:=createunit1(jisvoid, p)
				p.flag:=lx.subcode<>'v'			!0/1 = isvoid/isdef
				lex()
			esac

		when istypesym then
			p:=createunit1(jistype, p)
			p.mode:=lx.subcode
			lex()

		when dollarsym then
			if p.tag not in [jname, jdot] then
				serror("...name.$ needed")
			fi
			p:=createunit1(jsymbol, p)
			lex()

		else
			serror("Unknown dot suffix")
		esac
	od
	return p
end

function readslist(int &nparams, ftrailing=0)unit=
!read comma-separated list of expressions
!positioned at first symbol of first expression
! it might be | or )
!
!donulls=1 means empty expressions are allowed (just comma or terminator, which
!result in null units
!return with symbol at terminating symbol: 1st non comma and is that a unit starter
!iscall=1 when called to read a function-call parameter list; then key:value pairs
!are treated as keyword arguments
!eg: (a, b, c	)
!eg: (a		!
	unit ulist, ulistx
	int oldinparamlist

	ulist:=ulistx:=nil
	nparams:=0

	skipsemi()
	if lx.symbol=rbracksym then		!empty list
		return ulist
	fi

	pushlisttype('PARAM')

int donulls:=1

	do
		skipsemi()
		case lx.symbol
		when commasym then
			serror("null comma expr not allowed")
		when rbracksym then
			exit
		else
			addlistunit(ulist, ulistx, readunit())
			++nparams
			if lx.symbol=commasym then
				lex()
				if lx.symbol=rbracksym then
					if nparams<>1 or not ftrailing then serror("Trailing comma") fi
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

	poplisttype()
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
	case lx.symbol
	when kwhensym then
		lex()
		return createunit2(jif, readunit(), createunit1(jblock, p))
	when kunlesssym then
		lex()
		return createunit2(jif, createunit1(jnotl, readunit()), createunit1(jblock, p))
	else
		return p
	esac
end

function readkeyindex(unit p)unit=
!at '{'
!syntax is:
![x] or [x, ...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
	unit q

	lex()

	q:=readunit()

	if lx.symbol=commasym then
		lex()
		q.nextunit:=readunit()
	fi
	
	p:=createunit2(jkeyindex, p, q)

	skipsymbol(rcurlysym)
	return p
end

function readlbrack:unit=
!positioned at "("
!termsym is rbracksym
!read one of the following:
! (x)		simple expression
! ()		list with no elements
! (x, )		list with one element
! (x, x, ...)		list
! (x|x|x])		if then else fi
! (x|x, ... |x])	select then else end

! (s||s|s)	!list comp [SYNTAX TO BE REVISED]
!return positioned at symbol following closing ")"
	unit ulist, ulistx, p, q, r
	int oldirp, length, lower, lowerseen, elemtype, opc

	lex()					!first symbol of first expression
	ulist:=ulistx:=nil
	length:=0
	lower:=1
	lowerseen:=0

	elemtype:=tvoid

	if lx.symbol=stdtypesym and nextlx.symbol=colonsym then
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

	elsif (binopset[lx.symbol] or unaryopset[lx.symbol] or lx.symbol=propsym) and
			nextlx.symbol=rbracksym then
		if lx.symbol=addrsym then
			opc:=kappend
		else
			opc:=lx.subcode
		fi
doopc:
		p:=createunit0(joperator)
		p.pclop:=opc
		lex()
!		lex()
		skipsymbol(rbracksym)
		return p

	elsecase lx.symbol
	when specialopsym then
		case lx.subcode
		when '-' then opc:=kneg
		when '[]' then opc:=kindex
		else opc:=knop
		esac
		doopc
	when insym then
		opc:=kin
		doopc
	when inxsym then
		opc:=kinx
		doopc

	else					!assume normal expression follows
		p:=readxunit()
	esac

!check symbol after "(expr"
	case lx.symbol
	when rbracksym then			!simple (x) expression
		lex()
		if lowerseen then
			p:=createunit2(jkeyvalue, createintunit(lower), p)
		fi

		return p

	when commasym then
		length:=1
		if nextlx.symbol=rbracksym then		!means one-element list
			lex()
			lex()
			p:=createunit1(jmakelist, p)
			p.length:=length
			p.lower:=lower
			p.elemtype:=elemtype
			return p
		fi

!must be regular list
		ulist:=ulistx:=p
		repeat
			lex()							!skip comma
			if lx.symbol=rbracksym then		!allow , ) to end list
				exit
			fi
			if lx.symbol=commasym then
				serror(", , null expr not allowed")
			fi
			addlistunit(ulist, ulistx, readxunit())
			++length
			skipsemi()						!allow a, b, c;) (works better with a, b, c\ followed by comment on next line followed by ")")
		until lx.symbol<>commasym
		skipsymbol(rbracksym)
		p:=createunit1(jmakelist, ulist)
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
			skipsymbol(rbracksym)
			q.nextunit:=r
			return createunit2(jif, p, q)
		when rbracksym then
			lex()
			return createunit2(jif, p, q)

		esac

!assume selectx expression
		addlistunit(ulist, ulistx, q)	!start with one-element list
		checksymbol(commasym)
		if nextlx.symbol<>barsym then		!(n|a, | using one-element list; not useful but allow it...
			repeat
				lex()				!skip comma
				addlistunit(ulist, ulistx, readxunit())
			until lx.symbol<>commasym
			checksymbol(barsym)
		else
			lex()					!skip |
		fi
		lex()
		r:=readxunit()
		skipsymbol(rbracksym)
		p.nextunit:=r
		return createunit2(jselect, p, ulist)

	when semisym then
		ulist:=ulistx:=p
		repeat
			skipsemi()
			if lx.symbol=rbracksym then
				exit
			fi
			addlistunit(ulist, ulistx, readunit())
		until lx.symbol<>semisym
		skipsymbol(rbracksym)
		return makeblock(ulist)


	else
		serror("(x ...")
	esac
	return nil
end

function readif:unit=
!at 'if'
	int line, kwd, lineno
	unit pthen, pcond, plist, plistx, pelse, p, pelsif

	line:=lx.pos

	kwd:=lx.symbol			!in case coming from elsecase etc

	lex()
	pcond:=readsunit()
	skipsemi()

	skipsymbol(kthensym)

	pthen:=readsunit()

	case lx.symbol
	when kelsifsym then
		lx.symbol:=kifsym		!for .kwd
		pelse:=readif()

	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()
		checkend(kwd)
!		lex()
	when kelsecasesym, kelseswitchsym then
		lx.symbol:=kwd
		pelse:=makeblock(readswitchcase())
	else
		PELSE:=NIL
		checkend(kwd)
!		lex()
	esac

	pthen.nextunit:=pelse
	p:=createunit2(jif, pcond, pthen)
	p.pos:=line

	return p
end

proc checkend(int endkwd1, endkwd2=0, startline=0)=
!check end or end kwd1 or end kwd2, or is fi/esac/do
!return on symbol following any of that, which is expected to be semisym
	[256]char str

	skipsemi()

!symbol must be endsym or fi/esac/od which are endsym with subcode
!check I have end/fi/esac/cp
	if lx.symbol<>kendsym then
		serror("'End' expected")
	fi

	if lx.subcode then
		if lx.subcode in [endkwd1, endkwd2] then
			lex()
			return
		else
error:
			strcpy(str, "Mismatched end ")
			if startline then
				fprint @(&.str+strlen(&.str)), " (from line #)", startline
			fi
			serror(&.str)
		fi
	fi

!only end was used, so skip that now
	lex()

!now, should be semi, or possibly kwd1/2
	if lx.symbol in [endkwd1, endkwd2] then
		lex()
	elsif lx.symbol<>semisym then
		error
	fi
end

function readunless:unit=
	int line
	unit pcond, pthen, pelse, p
	line:=lx.pos
	lex()
	pcond:=readsunit()
	skipsymbol(kthensym)

	pthen:=readsunit()

	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
	else			!assume simple if-then
		pelse:=nil
	fi
	checkend(kunlesssym)
!	lex()
	pthen.nextunit:=pelse
	p:=createunit2(jif, createunit1(jnotl, pcond), pthen)
	p.pos:=line
	return p
end

function readwhile:unit=
	int pos
	unit pcond, pbody, p

	pos:=lx.pos
	lex()

	pcond:=readsunit(1)

	if lx.symbol=commasym then
		lex()
		pcond.nextunit:=readsunit(1)
	fi

	skipsymbol(kdosym)
	pbody:=readsunit()

	checkend(kwhilesym, kdosym)
!	lex()

	p:=createunit2(jwhile, pcond, pbody)
	p.pos:=pos
	return p
end

function readrepeat:unit=
	int pos
	unit pbody, pcond, p

	pos:=lx.pos
	lex()
	pbody:=readsunit()
	skipsymbol(kuntilsym)
	pcond:=readunit()
	p:=createunit2(jrepeat, pbody, pcond)
	p.pos:=pos
	return p
end

function readfor:unit=
!on 'for'; syntax is:
! for term [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/od
! for term in/inrev expr [when expr] do stmts [else stmts] end/od

	int line, opc, down, isforeach
	unit pstep, pvar, pcond, pfrom, pto, pelse, pbody, p, plist, pvar2

	line:=lx.pos
	isforeach:=lx.subcode
	lex()			!skip 'for'
	pvar:=readterm2()

	if pvar.tag<>jname then
		serror("For: name expected")
	else
		pvar.def.forindex:=1
	fi

	opc:=jfor
	pstep:=nil
	pcond:=nil
	pvar2:=nil
	down:=0

	if lx.symbol=commasym then			!double index
		lex()
		pvar2:=readterm2()
	fi

	if lx.symbol=insym then	!assume in/inrev
		lex()
		plist:=readunit()

		case plist.tag
		when jmakerange then		!in a..b: simple iteration
			pfrom:=plist.a
			pto:=plist.b
		when jbounds then			!
			plist.flag:=1
			opc:=jforx
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

	if lx.symbol=kwhensym then
		lex()
		pcond:=readunit()
	fi
	skipsymbol(kdosym)
	pbody:=readsunit()

	if pcond<>nil then
		pbody:=makeblock(createunit2(jif, pcond, pbody))
	fi
	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
		pbody.nextunit:=pelse
	else
		pelse:=nil
	fi
	checkend(kforsym, kdosym)
!	lex()


	case opc
	when jforall then
		pvar.nextunit:=plist
		plist.nextunit:=pvar2
		p:=createunit2(opc, pvar, pbody)

	when jforx then
		pvar.nextunit:=plist
		p:=createunit2(opc, pvar, pbody)
	else
		pvar.nextunit:=pfrom
		pfrom.nextunit:=pto
		pto.nextunit:=pstep
		p:=createunit2(opc, pvar, pbody)
	esac
	p.flag:=down

	if isforeach then
		if p.tag=jforall then
			p.tag:=jforeach
		else
			serror("Foreach?")
		fi
	fi

	p.pos:=line

	if pvar2 and opc<>jforall then
		serror("for i, j not allowed")
	fi

	return p
end

function readdo:unit=
	unit p
	int line

	line:=lx.pos
	lex()
	p:=readsunit()
	checkend(kdosym)
	p:=createunit1(jdo, p)
	p.pos:=line
	return p
end

function readto:unit=
	int line, id
	unit p, pcount, pbody

	line:=lx.pos
	lex()

	pcount:=readunit()

	skipsymbol(kdosym)
	pbody:=readsunit()
	checkend(ktosym, kdosym)
!	lex()

	pcount.nextunit:=createavname()

	p:=createunit2(jto, pcount, pbody)
	p.pos:=line
	return p
end

function makeblock(unit p)unit=
	return createunit1(jblock, p)
end

function readvardef(int isglobal=0, isstatic=0)unit=
!positioned at 'var' 'let'
	int nvars, varid, opc
	symbol d
	unit ulist, ulistx, p

!	m:=readtypespec(lx.symbol=kvarsym)
	lex()
!	M:=TVAR

!	if stcurrproc.nameid=procid then
	if stcurrproc.nameid in [procid, anonprocid] then
		varid:=(isstatic|staticid|frameid)
	else
		varid:=staticid
	fi
	nvars:=0
	ulist:=ulistx:=nil

	while lx.symbol=namesym do
		++nvars

		d:=addsymbol(stcurrproc, lx.symptr, varid, isglobal)

		lex()

		case lx.symbol
		when assignsym then
			opc:=lx.subcode
			if varid=staticid then
!				if stcurrproc.nameid=procid then
				if stcurrproc.nameid in [procid, anonprocid] then
					serror("Need '=' for static in proc")
				fi
			fi

!			d.initcode:=(lx.symbol=assignsym|2|3)
			d.initcode:=opc+2		!0/1 => 2/3

			lex()
			d.code:=readunit()
			if varid=frameid then
				p:=createunit2(jassign, createname(d), d.code)
				p.flag:=opc
				addlistunit(ulist, ulistx, p)
			fi

		when eqsym then
			if varid<>staticid then serror("Need ':=' for non-static") fi
			lex()

			d.initcode:=1
			d.code:=readunit()
		esac

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

proc readconstdef(int isglobal=0)=
!positioned at 'const'
	int nvars
	symbol d

	lex()

	nvars:=0
	while lx.symbol=namesym do
		++nvars

		d:=addsymbol(stcurrproc, lx.symptr, constid, isglobal)
		lexchecksymbol(eqsym)
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
	unit p, q, r

	lex()
	q:=nil

	if exprstarterset[lx.symbol] then
		q:=readunit()
	fi
	p:=createunit1(jreturn, q)

	return readcondsuffix(p)
end

function readprint:unit=
	int opc, flags, fshowname, length
	unit pformat, pdev, printlist, printlistx, p, q
	ref strbuffer expr

	ichar s

	pushlisttype('PRINT')

	opc:=jprint
	flags:=lx.subcode

	if lx.symbol=questionsym then
		flags:=pr_newline
	elsif flags iand pr_format then
		opc:=jfprint
	fi

	lex()

	printlist:=printlistx:=nil
	pformat:=pdev:=nil

	if lx.symbol=atsym then
		lex()
		pdev:=readunit()
		if lx.symbol=commasym then lex() else goto finish fi
	fi
	if opc=jfprint then
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
			addlistunit(printlist, printlistx, createunit0(jnogap))
		when dollarsym then
			addlistunit(printlist, printlistx, createunit0(jspace))
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
				p:=createunit2(jfmtitem, p, readunit())
			fi

			if fshowname then
				expr:=strexpr(p)
				strbuffer_add(expr, "=")
				s:=expr.strptr

				iconvucn(expr.strptr, expr.length)

				addlistunit(printlist, printlistx, q:=createstringunit(s, expr.length))
			fi
			addlistunit(printlist, printlistx, p)
		esac
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish:
	unless flags iand pr_newline then
		if opc=jprint and printlist=nil or opc=jfprint and printlist=nil and pformat=nil then
			serror("No print items")
		fi
	end

	poplisttype()

	if opc=jfprint then
		if pformat=nil then
			serror("No fmt str")
		fi
		if pformat=nil then
			pformat:=makeblock(pformat)
		fi
		pformat.nextunit:=printlist
		p:=createunit2(opc, pdev, pformat)
	else
		p:=createunit2(opc, pdev, printlist)
	fi

	p.flag:=flags
	return p
end

function readread:unit=
	int opc, flags
	unit pformat, pdev, readlist, readlistx, p

	pushlisttype('PRINT')
	flags:=lx.subcode
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
		if lx.symbol=colonsym then
			lex()
			p:=createunit2(jfmtitem, p, readunit())
		fi
		addlistunit(readlist, readlistx, p)
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish:
	if opc=jread and readlist=nil then
		serror("No read items")
	fi

	poplisttype()
	p:=createunit2(jread, pdev, readlist)
	p.flag:=flags
	return p
end

function readloopcontrol:unit=
	int opc
	unit p

	opc:=lx.subcode
	lex()
	if lx.symbol=namesym and eqstring(lx.symptr.name, "all") then
		lex()
		p:=createunit1(jloop, createintunit(0))

	elsif exprstarterset[lx.symbol] then
		p:=createunit1(jloop, readintunit())
	else
		p:=createunit1(jloop, createintunit(1))
	fi
	p.loopcode:=opc

	return readcondsuffix(p)
end

function readintunit:unit p=
	p:=readunit()
	if p.tag<>jintconst then
		serror("int expr needed")
	fi
	return p
end

function readswitchcase:unit=
	int pos, kwd, opc, lineno, rangeused, nwhen
	unit pexpr, pwhenlist, pwhenlistx, pwhen, pwhenx, pelse, p, pthen, pwhenthen, q

	pos:=lx.pos
	kwd:=lx.symbol			!remember kcasesym etc

	opc:=lx.subcode

	lex()

	skipsemi()
	if lx.symbol=kwhensym then
		if kwd=kswitchsym then
			serror("switch expr missing")
		fi
		pexpr:=CREATEUNIT0(JNONE)
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
			addlistunit(pwhen, pwhenx, p)
			if lx.symbol<>commasym then exit fi
			lex()
		od
		if lx.symbol<>kthensym then checksymbol(sendtosym) fi
		lex()
		pthen:=readsunit()
		pwhenthen:=createunit2(jwhenthen, pwhen, pthen)
		pwhenthen.pos:=pos
		addlistunit(pwhenlist, pwhenlistx, pwhenthen)
	od

	case lx.symbol
	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()

		checkend(kwd)
!		lex()
	when kelsifsym then
		lx.symbol:=kwd
		pelse:=makeblock(readif())
	when kelsecasesym, kelseswitchsym then

		lx.symbol:=kwd
		pelse:=readswitchcase()
	else
		PELSE:=NIL
		checkend(kwd)
!		lex()
	esac

	pexpr.nextunit:=pelse

	p:=createunit2(opc, pexpr, pwhenlist)
	p.pos:=pos
	return p
end

function readgoto:unit=
	lex()

	return readcondsuffix(createunit1(jgoto, readunit()))
end

function readstop:unit=
	unit p
	int i
	lex()
	if exprstarterset[lx.symbol] then
		p:=createunit1(jstop, readunit())
	else
		p:=createunit1(jstop, createintunit(0))
	fi
	return readcondsuffix(p)
end

function readcast:unit p=
!just seem basic type name
	int t, opc, pclop

	t:=lx.subcode
	lex()

	if t=trange and lx.symbol=lbracksym then
		lex()
		p:=readunit()
		if p.tag in [jkeyvalue, jkeyword] then
SERROR("MAKERANGELEN")
!			p.tag:=jmakerangelen
		elsif p.tag=jmakerange then
		else
			serror("need a..b or a:n")
		fi
		skipsymbol(rbracksym)
		return p
	fi


!check for standalone value
	case lx.symbol
	when atsym, lbracksym then

	else						!convert to typeconst
		p:=createunit0(jtypeconst)
		p.mode:=t
		return p
	esac

	if lx.symbol=atsym then
		lex()
		opc:=jtypepun
		pclop:=ktypepun
	else
		opc:=jconvert
		pclop:=kconvert
	fi
	checksymbol(lbracksym)
	p:=readterm()

	p:=createunit1(opc, p)
	p.pclop:=pclop
	storemode(stcurrproc, t, &p.mode)
	return p
end

function readset:unit=
!positioned at "["
	int length, nkeyvalues, oldinparamlist
	unit p, ulist, ulistx

	lex()					!first symbol of first expression

	case lx.symbol
	when rsqsym then		!empty set, same as 0
		lex()
		return createunit1(jmakeset, nil)
	when colonsym then
		lexchecksymbol(rsqsym)
		lex()
		return createunit1(jmakedict, nil)
	esac

	pushlisttype('DICT')

	p:=readunit()
	length:=1
	nkeyvalues:=0
	if p.tag=jkeyvalue then ++nkeyvalues fi

	ulist:=ulistx:=p

	while lx.symbol=commasym do
		lex()
		if lx.symbol=rsqsym then exit fi		!allow trailing comma
		addlistunit(ulist, ulistx, p:=readunit())
		if p.tag=jkeyvalue then ++nkeyvalues fi

		++length
		skipsemi()						!allow a, b, c;]
	od

	skipsymbol(rsqsym)

	if nkeyvalues then
		if length>nkeyvalues then serror("dict: mixed elements") fi
		p:=createunit1(jmakedict, ulist)
	else
		p:=createunit1(jmakeset, ulist)
	fi
	p.length:=length
	poplisttype()
	return p
end

global proc readtabledef(int isglobal=0)=
!at 'tabledata' symbol
	int i, ncols, nrows, enums, nextenumvalue, startline, firstvalue
	int ltype, lower
	byte commas:=0, semis:=0
	unit ulist, ulistx, plower, p
	const maxcols=20
	[maxcols]symbol varnames
	[maxcols]unit plist, plistx
	symbol d, nameptr

	const maxrows=500

	enums:=lx.subcode						!whether there is an enums column
	lex()

	firstvalue:=nextenumvalue:=1
	
	nrows:=0			!number of data rows appearing
	ncols:=0			!number of data columns (varnames appearing)

!loop reading variable names
	while lx.symbol=namesym do
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

	skipsemi()

	for i:=1 to ncols do
		plist[i]:=plistx[i]:=nil
	od
	ulist:=ulistx:=nil

	intabledata:=1
	do			!loop per row
		skipsemi()
		if ncols>0 then
			skipsymbol(lbracksym)
		fi
		if ++nrows>maxrows then
			serror("tabledata:too many rows")
		fi

		if enums then
			checksymbol(namesym)

			d:=addsymbol(stcurrproc, lx.symptr, enumid, isglobal)
			lex()

			case lx.symbol
			when eqsym then
				if nrows>1 then serror("tabledata '=' not 1st") fi
				lex()
				p:=readunit()
				if p.tag=jintconst then
					firstvalue:=nextenumvalue:=p.value
				else
					SERROR("TABLEDATA: COMPLEX ENUM VAL")
				fi
			esac

			d.index:=nextenumvalue++

			tabledataname:=d.name

			if ncols then				!comma always expected
				skipsymbol(commasym)		!check it
			fi
		fi

		for i:=1 to ncols do
			addlistunit(plist[i], plistx[i], readunit())
			if i=ncols then
				skipsymbol(rbracksym)
			else
				skipsymbol(commasym)
			fi
		od

		case lx.symbol
		when commasym then
			++commas
			lex()
!			if lx.symbol=closesym then exit fi		!allow trailing comma on last entry
			if lx.symbol=kendsym then exit fi		!allow trailing comma on last entry
		else
			skipsemi()
			if lx.symbol=kendsym then exit fi
			++semis
		esac
!		if lx.symbol<>commasym then exit fi
!		lex()					!should be ( for next entry
!		if lx.symbol=closesym then exit fi		!allow trailing comma on last entry
	od

	if semis and commas then serror("mixed commas") fi

	intabledata:=0

	skipsemi()
	checkend(ktabledatasym, startline:startline)

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
		d:=addsymbol(stcurrproc, varnames[i], staticid, isglobal)

		p:=d.code:=createunit1(jmakelist, plist[i])
		p.length:=nrows
		p.lower:=firstvalue
	od
end

function readtry:unit=
	unit ptry, pexceptlist, pexceptlistx, px, q, exlist, exlistx
	lex()

	ptry:=readsunit()
	pexceptlist:=pexceptlistx:=nil			!list of kexcept items

	while lx.symbol=kexceptsym do
		lex()
		exlist:=exlistx:=nil				!list of exception codes for this 'except'
		do
			addlistunit(exlist, exlistx, readunit())
			if lx.symbol<>commasym then exit fi
			lex()
		od
		skipsymbol(kthensym)
		px:=readsunit()
		addlistunit(pexceptlist, pexceptlistx, createunit2(jexcept, exlist, px))
	od
	checkend(ktrysym)
!	lex()

	return createunit2(jtry, ptry, pexceptlist)
end

function readsprint:unit=
	int opc, flags, isfprint
	unit pformat, pdev, printlist, printlistx, p

	pushlisttype('PRINT')
	opc:=jprint
	flags:=lx.subcode

	lexchecksymbol(lbracksym)
	lex()

	isfprint:=flags iand pr_format

	if flags iand pr_format then
		opc:=jfprint
	fi

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
			addlistunit(printlist, printlistx, createunit0(jnogap))
		else
			p:=readunit()
			if lx.symbol=colonsym then
				lex()
				p:=createunit2(jfmtitem, p, readunit())
			fi
			addlistunit(printlist, printlistx, p)
		fi
		if lx.symbol<>commasym then exit fi
		lex()
	od

	checksymbol(rbracksym)

finish:
	lex()
	if (opc=jprint or opc=jfprint) and printlist=nil then
		serror("No print items")
	fi

	poplisttype()
	if isfprint then
		if pformat=nil then
			serror("No fmt str")
		fi
		pformat.nextunit:=printlist
		p:=createunit2(opc, pdev, pformat)
	else
		p:=createunit2(opc, pdev, printlist)
	fi
	p.flag:=flags
	return p
end

function readsread:unit=
!to work an item at a time:
! a:=sread([fmt])
! b:=sreadln([dev])	returns entire input line, but keeps line for subsequent sread/read
	int opc
	unit pformat, pdev, p, readlist, readlistx

CPL "SREAD"

	pushlisttype('PRINT')
	opc:=lx.subcode
	lexchecksymbol(lbracksym)
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
		if lx.symbol=colonsym then
			lex()
			p:=createunit2(jfmtitem, p, readunit())
		fi
		addlistunit(readlist, readlistx, p)
		if lx.symbol<>commasym then exit fi
		lex()
	od

	checksymbol(rbracksym)

	finish:
	lex()
	if opc=jread and readlist=nil then
		serror("No read items")
	fi

	poplisttype()

	return createunit2(opc, pdev, readlist)
end

proc readimportdll=
!at 'importdll'
	[256]char str
	symbol stproc, d, stname
	int startpos, isfunc, isnew, libtype

	libtype:=lx.subcode

	lexchecksymbol(namesym)
	stname:=lx.symptr

	lexchecksymbol(eqsym)
	lex()

!check for an existing dll with the same name, as this could be adding to it

	isnew:=1
	d:=stname.nextdupl
!	while d do
!		if d.nameid=dllmoduleid then
!			stname:=d
!			isnew:=0
!			exit
!		fi
!		d:=d.nextdupl
!	od
	for i to nlibfiles do
		if eqstring(libtable[i].name, stname.name) then
			stname:=libtable[i]
			isnew:=0
			exit
		fi
	od

	if isnew then			!new
!		stname:=addsymbol(stprogram, stname, dllmoduleid, 0)
		stname:=addsymbol(nil, stname, dllmoduleid, 0)
		if nlibfiles>=maxlibfile then
			serror("Too many DLL libs")
		fi

		libtable[++nlibfiles]:=stname
		libtypes[+nlibfiles]:=libtype
		stname.index:=nlibfiles
	fi

	currdllindex:=stname.index

	startpos:=lx.pos
!------------------------------------
	do
		skipsemi()

		case lx.symbol
		when kprocsym, kfunctionsym then
			isfunc:=lx.symbol=kfunctionsym
			lex()
			case lx.symbol
			when namesym then
				stproc:=addsymbol(stcurrproc, lx.symptr, dllprocid, 1)

			when stringconstsym then
				strcpy(str, lx.svalue)
				convlcstring(str)
				stproc:=addsymbol(stcurrproc, addglobalname(str), dllprocid, 1)
				stproc.truename:=pcm_copyheapstring(lx.svalue)
			else
				serror("fn name expected")
			esac

			stproc.misfunc:=isfunc
			stproc.isimport:=1

			if ndllprocs>=maxdllproc then
				serror("Too many DLL procs")
			fi
			dllproctable[++ndllprocs]:=stproc
			dllproclibindex[ndllprocs]:=currdllindex
			stproc.index:=ndllprocs

			lex()

			if lx.symbol=namesym and eqstring(lx.symptr.name, "as") then
				lexchecksymbol(namesym)

				d:=addsymbol(stproc.owner, lx.symptr, aliasid, 1)
				d.alias:=stproc
				lex()
			fi
			readffiparams(stproc)
		when ktypesym then
			readtypedef(1)
		when kendsym then
!			if nextlx.symbol=kimportdllsym then lex() fi
			exit
		else
			readpackvars(stcurrproc, dllvarid)

		esac
	od	
!--------------------------------
	checkend(kimportdllsym, startline:startpos)
end

proc readffiparams(symbol stproc)=
!at first symbol after func name
!return list of units with dllparam defs (can be empty)
!if there is a result type, then head of list will be a return def type
	int pret, ptype

	if lx.symbol=lbracksym then
		lex()
		if lx.symbol=rbracksym then
			lex()
		else
			ptype:=readtypespec()
			if lx.symbol in [commasym, rbracksym] then		!types only
				readtypeparams(stproc, ptype)
			else
				readtypenameparams(stproc, ptype)
			fi
		fi
	fi

	if lx.symbol in [colonsym, sendtosym] then
		if not stproc.misfunc then serror("Return type for proc?") fi
		lex()
	fi

	pret:=tvoid
	if stproc.misfunc then
		if lx.symbol=semisym then serror("Return type missing") fi
		pret:=readtypespec()
	fi

	storemode(stproc.owner, pret, &stproc.mode)
end

proc readtypeparams(symbol stproc, int ptype)=
!at symbol after ptype
	[32]char str
	int nparams
	symbol stname


	nparams:=0

	do
		++nparams
		print @str, "$", ,nparams

		stname:=addsymbol(stproc, addglobalname(str), dllparamid, 0)
		storemode(stproc, ptype, &stname.mode)
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
	skipsymbol(rbracksym)
end

proc readtypenameparams(symbol stproc, int ptype)=
!at symbol after ptype
	symbol stname

	checksymbol(namesym)
	stname:=addsymbol(stproc, lx.symptr, dllparamid, 0)
	storemode(stproc, ptype, &stname.mode)
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
			stname:=addsymbol(stproc, lx.symptr, dllparamid, 0)
			storemode(stproc, ptype, &stname.mode)
			++stproc.nparams
			lex()
		else
			exit
		esac
	od
	skipsymbol(rbracksym)
end

global proc readrecorddef(int isglobal, symbol d)=
!at 'record' symbol
	int kwd, baseclass, m, startline, caligned
	byte lbopening:=0
	symbol nameptr

	baseclass:=0
	if d then			!entry from 'type name=record...'
		kwd:=ktypesym
		goto gotname
	fi

	kwd:=lx.symbol

	lexchecksymbol(namesym)
	nameptr:=lx.symptr

	lex()

	if lx.symbol=lbracksym then
		lex()
		baseclass:=readtypespec()
		skipsymbol(rbracksym)
	fi

	checkequals()
	lex()


	d:=addsymbol(stcurrproc, nameptr, (kwd=krecordsym|recordid|typeid), isglobal)

	if baseclass then
		if baseclass>0 then serror("baseclass?") fi
		if nbaseclasses>=255 then
				serror("Too many base classes")
		fi
		++nbaseclasses
		storemode(stcurrproc, baseclass, &baseclasstable[nbaseclasses])
		d.baseclassindex:=nbaseclasses
		baseclassdef[nbaseclasses]:=d
	fi

gotname:

	skipsemi()
	startline:=lx.pos

	if lx.symbol=lbracksym then lbopening:=1; lex() fi
	if kwd=krecordsym then
		m:=readrecordbody(d)
	else
		caligned:=0
		m:=readstructbody(d, caligned)
	fi

	if lbopening then
		checksymbol(rbracksym)
		lex()
	else
		checkend(krecordsym, startline:startline)
	fi
end

function readrecordbody(symbol owner)int=
!at first symbol of a class or record body (after 'type T=record', 
! or after 'record T ='
!read fields, constants, types, methods.
!create initially anonymous record type, and return type code
!caller will attached to named type as needed.

!int kwd
	symbol oldstcurrproc, e
	int m, nfields

	m:=addanontype()

	oldstcurrproc:=stcurrproc
	stcurrproc:=owner

	docase lx.symbol
	when kconstsym then
		readconstdef(0)
	when kvarsym then
		readrecordfields(owner)
	when kfunctionsym, kprocsym then
		readprocdef(0)

	when krecordsym then
		readrecorddef(0, nil)
	when ktypesym then
		lex()
		serror("CLASS TYPE")
	when kendsym, rbracksym, rcurlysym then
		exit
	when eofsym then
		serror("Class eof?")
		exit
	when semisym then
		lex()
!	elsif istypestarter() and nextlx.symbol<>lbracksym then
!		readrecordfields(owner)
!
	else
		serror("Unknown record field decl")
	end

	e:=owner.deflist
	nfields:=0
	while e, e:=e.nextdef do
		if e.nameid=fieldid and not e.atfield then
			++nfields
		fi
	od

	owner.nfields:=nfields

	ttfields[m]:=owner.deflist
	ttlength[m]:=nfields
	ttlower[m]:=1
	ttbasetype[m]:=trecord

	createusertype(owner, m)

	e:=owner.deflist
	while e do
		addgenfield(e)
		e:=e.nextdef
	od

	ttsize[m]:=varsize*owner.nfields

	stcurrproc:=oldstcurrproc

	return m
end

proc readrecordfields(symbol owner)=
!positioned at 'var'; read one line of var defs for a record
	int nvars, offset, index
	symbol d

!	m:=readtypespec(1)
	lex()

	nvars:=0
	index:=owner.nfields

	d:=owner.deflist

	offset:=0
	while d, d:=d.nextdef do
		if d.nameid=fieldid and not d.atfield then
			offset+:=varsize
		fi
	od

	while lx.symbol=namesym do
		++nvars

		d:=addsymbol(stcurrproc, lx.symptr, fieldid, 0)
		d.atfield:=nil

		lex()

		if lx.symbol=atsym then
			lex()
			d.atfield:=readatfield()
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

!	stcurrproc.nfields+:=nvars
end

function readstructbody(symbol owner, int caligned)int=
	int m, ngroups, nvars, t
	symbol d, e

	m:=addanontype()

	ngroups:=0

	do
		skipsemi()

		case lx.symbol
		when kstructsym then
			++ngroups
			lex()
			addstructflag(owner, structblockid)

		when kunionsym then
			++ngroups
			lex()
			addstructflag(owner, unionblockid)

		when kendsym then
			if nextlx.symbol in [kstructsym, kunionsym] then lex() fi
doend:
			if ngroups then
				--ngroups
				lex()
				addstructflag(owner, endblockid)
			else
				exit
			fi
		when rbracksym then
			doend

!		when rbracksym then
!			exit

		else
			readpackvars(owner, structfieldid)
!			t:=readtypespec(0)
!
!			nvars:=0
!			while lx.symbol=namesym do
!				++nvars
!				d:=addsymbol(owner, lx.symptr, structfieldid, 0)
!				storemode(owner, t, &d.mode)
!				lex()
!
!				if lx.symbol<>commasym then
!					exit
!				fi
!				lexchecksymbol(namesym)
!			od
!			if nvars=0 then serror("struct decl?") fi
!			owner.nfields:=nvars
		esac
	od

	ttfields[m]:=owner.deflist
	ttlength[m]:=owner.nfields
!CPL =OWNER.NFIELDS

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

	return m
end

proc addstructflag(symbol owner, int id)=
	static int structseqno
	[32]char str

	fprint @str, "$$#", ++structseqno

	addsymbol(owner, addglobalname(str), id, 0)
end

proc readprocdef(int isglobal)=
!at 'proc' etc symbol; read proc def or declaration
	int kwd, startline, nparams, shortfun
	unit pcode
	symbol d, oldstcurrproc
	[256]char str

	kwd:=lx.symbol
	shortfun:=lx.subcode
	lexchecksymbol(namesym)
!
	if stcurrproc.nameid in [procid, anonprocid] then
		serror("Nested proc")
	fi

	oldstcurrproc:=stcurrproc			!usually module, but could be a record
	stcurrproc:=d:=addsymbol(stcurrproc, lx.symptr, procid, isglobal)

	addproc(d)

	lex()

	d.mode:=tvoid

	if lx.symbol=lbracksym then		!possible params
		lex()
		if lx.symbol<>rbracksym then
			readparams(d)
		else
			lex()
		fi
	fi

	checkequals()
	lex()

	startline:=lx.pos

	if not shortfun then
		d.code:=readsunit()
		checkend(kwd, startline:startline)
	else
		d.code:=readunit()
		checksymbol(semisym)
!		lex()
	fi

	if eqstring(d.name, "start") then
		currmodule.startfn:=d
	elsif eqstring(d.name, "main") then
		currmodule.mainfn:=d
	fi

	stcurrproc.misfunc:=kwd=kfunctionsym

	stcurrproc:=oldstcurrproc
end

function readatfield:symbol=
!reading a class or struct body, where owner is that class/struct entry
!positioned at symbol following '@', should be name of an existing field
	symbol p, d

	checksymbol(namesym)
	d:=lx.symptr
	lex()

	p:=stcurrproc.deflist
	while p do
		if eqstring(p.name, d.name) then
			return p
		fi

		p:=p.nextdef
	od
	serror_s("Can't find @ field", d.name)
	return nil
end

function istypestarter:int=
	case lx.symbol
	when stdtypesym, krefsym, kvarsym, lsqsym then
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

	lexchecksymbol(namesym)

	stmacro:=addsymbol(stcurrproc, lx.symptr, macroid, isglobal)
	owner:=stmacro

	lex()

	if lx.symbol=lbracksym then			!may have parameters
		lex()
		if lx.symbol<>rbracksym then
			do
				case lx.symbol
				when namesym then
					stname:=addsymbol(owner, lx.symptr, macroparamid, 0)
					stname.firstdupl:=lx.symptr

					lex()
					if lx.symbol=rbracksym then
						exit
					fi
					skipsymbol(commasym)
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

function readhostparams(unit lhs, int isfn)unit=
!hostfn name has been read
!lhs is not null when lhs.hostfn(...) has been used
!currently at hostfn symbol
	int fnindex, nargs
	unit p, q

	fnindex:=lx.subcode
	lexchecksymbol(lbracksym)
	lex()

	q:=readslist(nargs)

	skipsymbol(rbracksym)

	if lhs then
		lhs.nextunit:=q
		q:=lhs
	fi

	p:=createunit1(jcallhost, q)
	p.index:=fnindex

!	poplisttype()

	return p
end

proc pushlisttype(int ltype)=
	if nlisttype>=maxlisttype then
		serror("listtype overflow")
	fi
	listtypestack[++nlisttype]:=listtype
	listtype:=ltype
end

proc poplisttype=
	listtype:=listtypestack[nlisttype--]
end

function readcompilervar:unit=
	[100]char str
	rsystemtime tm
	static []ichar monthnames=("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

	case lx.subcode
	when cv_lineno then
		return createintunit(lx.lineno)

	when cv_strlineno then
		strcpy(str, strint(lx.lineno))

	when cv_modulename then
		strcpy(&.str, currmodule.name)

	when cv_filename then
!		strcpy(&.str, modules[currmoduleno].filename)
!		strcpy(&.str, currmodule.name)
		strcpy(&.str, currmodule.filespec)
	when cv_function then
		strcpy(&.str, (stcurrproc|stcurrproc.name|"<none>"))
	when cv_date then
		os_getsystime(&tm)
		fprint @&.str, "#-#-#", tm.day, monthnames[tm.month], tm.year:"4"
!
	when cv_time then
		os_getsystime(&tm)
		fprint @&.str, "#:#:#", tm.hour:"2", tm.minute:"z2", tm.second:"z2"

!	when jcvversion then x:=compilerversion
!	when jcvpclversion then x:=pclversion
	else
		serror("compiler var not impl")
	esac

	return createstringunit(pcm_copyheapstring(&.str))
end

function readpair(int tag, pclop=knop)unit p=
!should be at '(', but check
!read (a, b) and produce (opc, a, b ) unit

	ref unitrec a, b

	lexchecksymbol(lbracksym)
	lex()
	a:=readexpression()
	skipsymbol(commasym)
	b:=readexpression()

	if lx.symbol=commasym and tag=jmap then			!allow 3rd item
		lex()
		b.nextunit:=readexpression()
	fi
	skipsymbol(rbracksym)
	p:=createunit2(tag, a, b)
	p.pclop:=pclop
	return p
end

global proc lexchecksymbol(int symbol)=
	lex()
	checksymbol(symbol)
end

proc readtypedef(int isglobal)=
!at 'type'
	int ptype
	symbol d

	lexchecksymbol(namesym)
	d:=addsymbol(stcurrproc, lx.symptr, typeid, isglobal)

	lexchecksymbol(eqsym)
	lex()	

	if lx.symbol=krecordsym then
		lex()
		d.nameid:=recordid
		readrecorddef(isglobal, d)
		return
	fi

	ptype:=readtypespec(owner:d)

!CPL "USERTYPE", PTYPE, D.NAME, =NTYPES, STRMODE(PTYPE), STRMODE(NTYPES)

	createusertype(d, ptype)
end

function readtypespec(int allowvar=0, symbol owner=nil)int=
!full=1 to allow structdefs

	int flags, arraycode, oldipl
	int a, b, t, startline, caligned
	symbol d
	const maxdim=10
	[maxdim]unit lowerdims, lengthdims
	int ndims
	unit x, lowerx, upperx, lengthx

	case lx.symbol
	when lsqsym then
dolsq:
		lex()
		ndims:=0
		pushlisttype(0)
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
						lengthx:=createunit2(jbin, upperx, lowerx)
						lengthx.pclop:=ksub
						lengthx:=createunit2(jbin, lengthx, createintunit(1))
						lengthx.pclop:=kadd
					fi
				else
					case lx.symbol
					when rsqsym, commasym then			![n]
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
		skipsymbol(rsqsym)
		poplisttype()
		t:=readtypespec()

		for i:=ndims downto 1 do
			t:=makeaxtype(t, lowerdims[i], lengthdims[i])
		od
		return t

	when krefsym then
		lex()

		if lx.symbol=stdtypesym and lx.subcode=tvoid then
			lex()
			return makereftype(tvoid, owner)
		else
			return makereftype(readtypespec(), owner)
		fi

	when namesym then
		d:=lx.symptr
		lex()
		if lx.symbol=dotsym then
			lexchecksymbol(namesym)
			t:=newusertypex(d, lx.symptr)
			lex()
			return t
		else
			return newusertypex(d)
		fi

	when stdtypesym then
		case lx.subcode
		when tpackstrz then				!stringz
			lex()
			if lx.symbol=mulsym then
				lex()
				return makestrtype(tpackstrz, readunit())
			else
				return tstringz
			fi

		when tpackstrc then
			lexchecksymbol(mulsym)
			lex()
			return makestrtype(tpackstrc, readunit())

		when tarray then
			lexchecksymbol(lsqsym)
			goto dolsq

		else
			t:=lx.subcode
			case t
			when tint then t:=ti64
			when treal then t:=tr64
			esac

			lex()
			return t
		esac

	when krecordsym then
		if owner=nil then serror("anon record") fi
		lex()
		startline:=lx.pos
		t:=readrecordbody(owner)

		checkend(krecordsym, startline:startline)
		return t

	when kstructsym then
		if owner=nil then serror("anon struct") fi
		lex()
		caligned:=0
		if lx.symbol=kcalignedsym then
			caligned:=1
			lex()
		fi

		startline:=lx.pos
		t:=readstructbody(owner, caligned)

		checkend(kstructsym, startline:startline)
		return t

!	when kvarsym then
!		if not allowvar then
!			serror("var types not allowed")
!		fi
!		lex()
!		if lx.symbol=colonsym then
!			lex()
!			return readtypespec(0)
!		fi
!		return tvar
	else
		serror("Type expected")
	esac

	return t
end

proc readparams(symbol stproc)=
!just after '('
	int isbyref, isoptional
	symbol d

!CPL "READPARAMS_NAMES"

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
		d:=addsymbol(stproc, lx.symptr, paramid, 0)
!		d.mode:=tvar
		++stproc.nparams

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

		isbyref:=isoptional:=0

		if lx.symbol=commasym then
			lex()
			if lx.symbol=ellipsissym then
				stproc.mvarparams:=1
				lex()
				exit
			fi
		else
			exit
		fi
	od

	skipsymbol(rbracksym)
end

func checkoperator:unit p=
	int opc

	if nextlx.symbol in [commasym, rbracksym, semisym] then
		p:=createunit0(joperator)

		if lx.symbol=specialopsym then
			case lx.subcode
			when '-' then opc:=kneg
			when '[]' then opc:=kindex
			else opc:=knop
			esac
			p.pclop:=opc
		elsif cmpopset[lx.symbol] then
SERROR("(CMP OP) NOT READY")
		else

			p.pclop:=lx.subcode
		fi
		lex()
		return p
	fi
	nil
end

func readlambda:unit p=
!at {
	[100]symbol params
	symbol oldstcurrproc, stproc, d
	[20]char str
	int nparams
	byte byref

	case stcurrproc.nameid
	when procid then
!	when procid, anonprocid then
	when anonprocid then serror("Nested {}")
	else serror("{} not in fn")
	esac

	oldstcurrproc:=stcurrproc

	print @str, "$F", ,++nextlambdaindex
	stproc:=addsymbol(stcurrproc, addnamestr(str), anonprocid, 0)
	stcurrproc:=stproc
	addproc(stproc)

	lex()
	nparams:=0
	byref:=0
	if lx.symbol=addrsym then lex(); byref:=1 fi

	if lx.symbol=namesym and nextlx.symbol in [commasym, colonsym] then
		do
			checksymbol(namesym)

			d:=addsymbol(stproc, lx.symptr, paramid, 0)
!*!			d.pindex:=++nparams
			params[++nparams]:=d
			d.mbyref:=byref
			byref:=0

			lex()

			if lx.symbol<>commasym then exit fi
			lex()
		od
		checksymbol(colonsym)
		lex()
	fi

	stproc.nparams:=nparams
	stproc.misfunc:=1

!CPL =NPARAMS
!
!CPL "READ LAMBDA", STPROC.NAME, STPROC.MISFUNC

!*!	stproc.isfunc:=1
!*!	getparamoffsets(&params, nparams)

!read body of lambda
	stproc.code:=readsunit()
	skipsymbol(rcurlysym)

!	p:=createunit1(jmakeclosure, createname(stproc))
	p:=createname(stproc)

	stcurrproc:=oldstcurrproc
	return p
end

proc readpackvars(symbol owner, int id)=
!expected to be typed var-decl inside a struct or importdll/lib body
	int t, nvars
	symbol d

	t:=readtypespec(0)

	nvars:=0
	while lx.symbol=namesym do
		++nvars
		d:=addsymbol(owner, lx.symptr, id, 0)
		storemode(owner, t, &d.mode)
		lex()

		if lx.symbol<>commasym then
			exit
		fi
		lexchecksymbol(namesym)
	od
	if nvars=0 then serror("bad decl?") fi
end
=== qq_pcltabs.m 0 0 17/48 ===
global enumdata [0:]ichar opndnames=
							!   PCL1		PCL2
	(cnone=0,	$),
	(cstatic,	$),			! m Symbol		varptr = address of static variable
	(cframe,	$),			! f Symbol		offset = offset of stack frame var
	(cproc,		$),			! p Symbol		labelref = address of kprocent instr for proc
	(cdllproc,	$),			! x Int			index = index into dllproc table

	(cgenfield,	$),			! g Symbol		index = index into genfieldtable

	(clabel,	$),			! l labelref*	labelref = addr of pcl instr, displayed as pc index
!											(* will be label index until end of codegen)
	(cint,		$),			! i
	(creal,		$),			! r
	(cstring,	$),			! s Stringz		objptr = address of static Object with string
	(cstringz,	$),			! z Stringz
	(ctype,		$),			! t Int			Typecode
	(csymbol,	$),			! d Symbol		Symbol
	(coperator,	$),			! o Int			Operator
	(cmaths,	$),			! m Int			Mathsop
	(chost,		$),			! h Int			Host index
	(cbinto,	$),			! b Int	pclop	Index into bintotable

	(clast,		"?")
end

!these aliases are used so that the enum table is tidier
const p = cproc
const m = cstatic
const f = cframe
const l = clabel
const x = cdllproc
const g = cgenfield
const i = cint
const r = creal
const s = cstring
const z = cstringz
const t = ctype
const d = csymbol
const o = coperator
const b = cbinto
const y = cmaths
const h = chost

global type pcl = ref pclrec

!global record pclrec =
!	byte opcode
!	byte n						! n		nargs/etc
!	byte mode					! t		0/void, or optional type annotation info, or pushas code
!!	byte haslabel				!       1 when this instr is referenced as a table
!	byte flags:(haslabel:1,		!       1 when this instr is referenced as a table
!				isaux:1)		!		1 when part of a compound bytecode seq
!	union
!		struct
!			i16 x, y					! x y	Misc
!		end
!		i32 xy
!	end
!
!	union						! Main operand codes
!		symbol	def				! d v p
!		i64		value			! i
!		u64		uvalue			! w
!		r64		xvalue			! r
!		ichar	svalue			! s
!		int		labelno			! l
!		int		index			! g
!		pcl		labelref		! l
!		i64		offset			!
!		object	objptr			! z
!		i64		typecode		! z
!		byte	mathscode		! m		for kmaths/2: mm_sqrt etc
!		byte	pclop			! o		for (+) etc
!		byte	bintoindex		! b		index into bintotable
!		i64		hostindex		! x		
!		variant	varptr			!
!		struct
!			i32	usertag			! u		!these attributes overlap main operand
!			i32	usertag2		! v
!		end
!		ref label LABADDR			! dummy field; not used
!	end
!end

global record pclrec =
	ref label labaddr
	byte opcode
	byte n						! n		nargs/etc
	byte mode					! t		0/void, or optional type annotation info, or pushas code
	byte flags:(haslabel:1,		!       1 when this instr is referenced as a label
				isaux:1)		!		1 when part of a compound bytecode seq
	union
		struct
			i16 x, y					! x y	Misc
		end
		i32 xy
	end

	union						! Main operand codes
		symbol	def				! d v p
		i64		value			! i
		u64		uvalue			! w
		r64		xvalue			! r
		ichar	svalue			! s
		int		labelno			! l
		int		index			! g
		pcl		labelref		! l
		i64		offset			!
		object	objptr			! z
		i64		typecode		! z
		byte	mathscode		! m		for kmaths/2: mm_sqrt etc
		byte	pclop			! o		for (+) etc
		byte	bintoindex		! b		index into bintotable
		i64		hostindex		! x		
		variant	varptr			!
		struct
			i32	usertag			! u		!these attributes overlap main operand
			i32	usertag2		! v
		end
	end
	int dummy
end

global enumdata  [0:]ichar pclnames, [0:]byte pclopnd, [0:]u32 pclattrs =
	(knop = 0,  $,  0, '    '),   ! simple nop
	(kskip,     $,  0, '    '),   ! ignore on pcl listing

	(kprocdef,  $,  d, '    '),   ! 
	(kprocent,  $,  0, 'n   '),   ! n=number of locals; 
	(kprocend,  $,  0, '    '),  
	(kendmod,   $,  0, '    '),
	(kcomment,  $,  0, '    '),
                                   
	(kpushm,    $,  m, '    '),   ! Push v
	(kpushf,    $,  f, '    '),   ! Push v
	(kpushmref, $,  m, '    '),   ! push &v
	(kpushfref, $,  f, '    '),   ! push &v
	(kpushlab,  $,  l, '    '),   ! push L
	(kpopm,     $,  m, '    '),   ! v := Z
	(kpopf,     $,  f, '    '),   ! v := Z

	(kpushci,   $,  i, '    '),   ! Push i
	(kpushvoid, $,  0, '    '),   ! Push void 
	(kpushnil,  $,  0, '    '),   ! Push nil (ref void)
	(kpushcr,   $,  r, '    '),   ! Push r

	(kpushcs,   $,  s, '    '),   ! Push constant string object

	(kpushtype, $,  t, '    '),   ! Push type constant
	(kpushopc,  $,  o, '    '),   ! Push operator constant
	(kpushsym,  $,  d, '    '),   ! Push symbol reference

	(kpushptr,  $,  0, '    '),   ! Z' := Z^
	(kpopptr,   $,  0, '    '),   ! Z^ := Y

	(kzpopm,    $,  m, '    '),   ! v := Z; don't free v first (static should be zeroed)
	(kzpopf,    $,  f, '    '),   ! v := Z; don't free v first

	(kdupl,     $,  0, '    '),   ! (Z',Y') := (share(Z), Z)
	(kcopy,     $,  0, '    '),   ! Z' := deepcopy(Z)
	(kswap,     $,  0, '    '),   ! swap(Z^, Y^)

	(kconvrefp, $,  0, '    '),   ! Change ref in Z to refpacked

	(kjump,     $,  l, '    '),   ! Jump to L
	(kjumpptr,  $,  0, '    '),   ! Jump to Z

	(kjumpt,    $,  l, '    '),   ! Jump to L when Z is true
	(kjumpf,    $,  l, '    '),   ! Jump to L when Z is false

	(kjumpeq,   $,  l, '    '),   ! Jump to L when Y = Z
	(kjumpne,   $,  l, '    '),   ! Jump to L when Y <> Z
	(kjumplt,   $,  l, '    '),   ! Jump to L when Y < Z
	(kjumple,   $,  l, '    '),   ! Jump to L when Y <= Z
	(kjumpge,   $,  l, '    '),   ! Jump to L when Y >= Z
	(kjumpgt,   $,  l, '    '),   ! Jump to L when Y > Z

!	(kjmpltfci, $,  l, '    '),   ! Jump to L when B < C (B/C in next two ops)

	(kwheneq,   $,  l, '    '),   ! Y = Z:  pop both, jump to L
								  ! Y <> Z: pop Z only, step to next

	(kwhenne,   $,  l, '    '),	  ! Y <> Z:  pop Z only, jump to L
								  ! Y = Z:   pop both, step to next

	(kjumplab,  $,  l, '    '),   ! Jumptable entry

!	(kswitch,   $,  l, 'xy  '),   ! Jumptable has y-x+1 entries
	(kswitch,   $,  0, 'xy  '),   ! Jumptable has y-x+1 entries

	(ktom,      $,  l, '    '),   ! --v; jump to l when v<>0 in next op
	(ktof,      $,  l, '    '),   ! --v; jump to l when v<>0 in next op

	(kformci,   $,  l, '    '),   ! ++v; jump to l when v<=i in next 2 ops: pushm/pushci
	(kforfci,   $,  l, '    '),   ! ++v; jump to l when v<=i in next 2 ops: pushm/pushci

	(kformm,    $,  l, '    '),   ! ++v; jump to l when v<=v in next 2 ops
	(kforff,    $,  l, '    '),   ! ++v; jump to l when v<=v in next 2 ops

	(kcallproc, $,  p, 'n   '),   ! Call &A; n is no. args
	(kcallptr,  $,  0, 'n   '),   ! Call X^; n is no. of params supplied; x is stack adjust
	(kretproc,  $,  0, 'nx  '),   ! n/x are params/locls to free; Return from proc
	(kretfn,    $,  0, 'nxy '),   ! n/x are params/locals to free; y=retval offset in caller; ret from fn
!	(kpopret,   $,  i, '    '),   ! pop stack to caller's return slot; i = offset

	(kmodcall,  $,  d, '    '),   ! 
	(kmodret,   $,  0, '    '),   ! 

	(kcalldll,  $,  x, 'n   '),   ! Call dll function d (sysmbol); n=nargs

	(kcallhost, $,  h, '    '),   ! Call Q host function h (Host index)

	(kunshare,  $,  0, 'n   '),   ! Unshare and pop A var values on stack
	(kaddsp,    $,  0, 'n   '),   ! SP+:=A; note: positive A will push, negative will pop (reverse of the hardware)

	(kstop,     $,  0, 'n   '),   ! Stop program with stopcode Z; n=1 to stop runproc instead

	(kmakelist, $,  0, 'xy  '),   ! x items on stack; make list with lwb y
	(kmakevrec, $,  0, 'xu  '),   ! x items on stack; make record of type u
	(kmakeax,   $,  0, 'xyuv'),   ! x items on stack; make array with lwb y, type u and elemtype v
	(kmakebits, $,  0, 'xyuv'),   ! x items on stack; make bits with lwb y, type u and elemtype v
	(kmaketrec, $,  0, 'xu  '),   ! x items on stack; make struct with type u
	(kmakeset,  $,  0, 'x   '),   ! x items on stack; make set
	(kmakerang, $,  0, '    '),   ! 2 items on stack; make range
	(kmakedict, $,  0, 'x   '),   ! x*2 items on stack (x key:val items); make dict
	(kmakedec,  $,  0, '    '),   ! Turn string on stack to decimal number

	(kincrptr,  $,  0, 'x   '),   ! Z^ +:= x
	(kincrtom,  $,  m, 'x   '),   ! v +:= x
	(kincrtof,  $,  f, 'x   '),   ! v +:= x
	(kloadincr, $,  0, 'x   '),   ! T := Z^; Z^ +:= x; Z' := T
	(kincrload, $,  0, 'x   '),   ! Z^ +:= x; Z' := Z^

	(kneg,      $,  0, '    '),   ! Z':= -Z
	(kabs,      $,  0, '    '),   ! Z' := abs Z
	(knotl,     $,  0, '    '),   ! Z' := not Z
	(kinot,     $,  0, '    '),   ! Z' := inot Z
	(kistruel,  $,  0, '    '),   ! Z' := istrue Z
	(kasc,      $,  0, '    '),   ! Z' := asc(Z)
	(kchr,      $,  0, '    '),   ! Z' := chr(Z)
	(ksqr,      $,  0, '    '),   ! Z' := Z*Z

	(kmaths,    $,  y, '    '),   ! Z' := op(Z)
	(kmaths2,   $,  y, '    '),   ! Z' := op(Y, Z)

	(kunaryto,  $,  o, '    '),   ! Z^ op:= Z
	(knotlto,   $,  0, '    '),   ! Z^ not:= Z

	(klen,      $,  0, '    '),   ! Z' := Z.len
	(klwb,      $,  0, '    '),   ! Z' := Z.lwb
	(kupb,      $,  0, '    '),   ! Z' := Z.upb
	(kbounds,   $,  0, 'n   '),   ! Z' := Z.bounds; n=1: one range value; n=2: two dims
	(kbytesize, $,  0, '    '),   ! Z' := Z.bytesize

	(ktype,     $,  0, 'n   '),   ! Z' := n=0/1/2 = basetype/elemtype
	(kdictsize, $,  0, '    '),   ! Z' := Z.dictsize
	(kisfound,  $,  0, '    '),   ! Z' := Z.isfound
	(kminval, 	$,  0, '    '),   ! Z' := Z.minvalue
	(kmaxval, 	$,  0, '    '),   ! Z' := Z.maxvalue

	(kistype,   $,  t, '    '),   ! Z' := Z.type/etc = t
	(kisvoid,   $,  0, 'n   '),   ! Z' := Z.isvoid (n=0) or not Z.isdef (n=1)
	(kconvert,  $,  t, '    '),   ! Z' := t(Z)
	(ktypepun,  $,  t, '    '),   ! Z' := t@(Z)

	(kadd,      $,  0, '    '),   ! Z' := Y + Z
	(ksub,      $,  0, '    '),   ! Z' := Y - Z
	(kmul,      $,  0, '    '),   ! Z' := Y * Z
	(kdiv,      $,  0, '    '),   ! Z' := Y / Z
	(kidiv,     $,  0, '    '),   ! Z' := Y % Z
	(kirem,     $,  0, '    '),   ! Z' := Y rem Z
	(kidivrem,  $,  0, '    '),   ! (Y', Z') := Y divrem Z
	(kiand,     $,  0, '    '),   ! Z' := Y iand Z
	(kior,      $,  0, '    '),   ! Z' := Y ior Z
	(kixor,     $,  0, '    '),   ! Z' := Y ixor Z
	(kshl,      $,  0, '    '),   ! Z' := Y << Z
	(kshr,      $,  0, '    '),   ! Z' := Y >> Z
	(kin,       $,  0, 'n   '),   ! Z' := Y in Z (n=0) or Y not in Z (n=1)
	(kinx,      $,  0, '    '),   ! Z' := Y inx Z
	(kcmp,      $,  0, 'c   '),   ! Z' := Y c Z
	(kmin,      $,  0, '    '),   ! Z' := min(Y, Z)
	(kmax,      $,  0, '    '),   ! Z' := max(Y, Z)
	(kconcat,   $,  0, '    '),   ! Z' := concat(Y, Z) or Y && Z
	(kappend,   $,  0, '    '),   ! Z' := append(Y, Z) or Y & Z
	(ksame,     $,  0, '    '),   ! Z' := Y == Z

	(kpower,    $,  0, '    '),   ! Z' := Y ** Z

!	(kbinto,    $,  b, '    '),   ! Y^ op:= Z
	(kbinto,    $,  b, '    '),   ! Z^ op:= Y

	(kandlto,   $,  0, '    '),   ! Y^ and:= Z
	(korlto,    $,  0, '    '),   ! Y^ or:= Z
	(kconcatto, $,  0, '    '),   ! Y^ concat:= Z or Y^ &&:= Z
	(kappendto, $,  0, '    '),   ! Y^ append:= Z or Y^ &:= Z

	(kdot,      $,  g, '    '),   ! Z' := Z.g
	(kpopdot,   $,  g, '    '),   ! Z.g := Y
	(kdotref,   $,  g, '    '),   ! Z' := &Z.g

	(kindex,    $,  0, '    '),   ! Z' := Y[Z]
	(kpopix,    $,  0, '    '),   ! Z' := Y[Z]:=X
	(kindexref, $,  0, '    '),   ! Z' := &Y[Z]

	(kkeyindex, $,  0, '    '),   ! Z' := X{Y, Z}
	(kpopkeyix, $,  0, '    '),   ! Y{Z} := X
	(kkeyixref, $,  0, '    '),   ! Z' := &X{Y, Z}

	(kdotix,    $,  0, '    '),   ! Z' := Y.[Z]
	(kpopdotix, $,  0, '    '),   ! Y.[Z] := X
	(kdotixref, $,  0, '    '),   ! Z' := &Y.[Z]

	(kexpand,   $,  0, 'n   '),   ! Z' := Expand Z into n objects are needed

	(kpushtry,  $,  l, 'xy  '),   ! Push try/except into; label/except code/no. exceptions
	(kraise,    $,  0, 'x   '),   ! Raise exception Z
	(kmap,      $,  0, '    '),   ! Z' := map(Y, Z)

!Special combination codes

	(kpushfff,  $,  f, '    '),   ! 
	(kpushff,   $,  f, '    '),   ! 
	(kpushmm,   $,  0, '    '),   ! 
	(kpushfm,   $,  0, '    '),   ! 
	(kpushmf,   $,  0, '    '),   ! 

	(kpushmci,  $,  0, '    '),   ! 
	(kpushfci,  $,  0, '    '),   ! 

	(kmoveff,   $,  0, '    '),   ! 
	(kmovemm,   $,  0, '    '),   ! 
	(kmovefm,   $,  0, '    '),   ! 
	(kmovemf,   $,  0, '    '),   ! 
	(kzmoveff,  $,  0, '    '),   ! 

	(kmovefci,  $,  0, '    '),   ! 
	(kmovemci,  $,  0, '    '),   ! 
	(kzmovefci, $,  0, '    '),   ! 

	(kpushv2,   $,  0, '    '),   ! 
	(kpushv3,   $,  0, '    '),   ! 

	(kjmpeqfci, $,  0, '    '),   ! 
	(kjmpnefci, $,  0, '    '),   ! 
	(kjmpltfci, $,  0, '    '),   ! 
	(kjmplefci, $,  0, '    '),   ! 
	(kjmpgefci, $,  0, '    '),   ! 
	(kjmpgtfci, $,  0, '    '),   ! 

	(kjmpeqff,  $,  0, '    '),   ! 
	(kjmpneff,  $,  0, '    '),   ! 
	(kjmpltff,  $,  0, '    '),   ! 
	(kjmpleff,  $,  0, '    '),   ! 
	(kjmpgeff,  $,  0, '    '),   ! 
	(kjmpgtff,  $,  0, '    '),   ! 

	(kaddfci,   $,  0, '    '),   ! 
	(ksubfci,   $,  0, '    '),   ! 
	(kaddff,    $,  f, '    '),   ! 
	(ksubff,    $,  0, '    '),   ! 

	(kaddci,    $,  0, '    '),   ! 
	(ksubci,    $,  0, '    '),   ! 

	(kiandci,   $,  0, '    '),   ! 
	(kshlci,    $,  i, '    '),   ! 
	(kshrci,    $,  i, '    '),   ! 

	(kbintof,   $,  f, '    '),   ! 

	(kaddtof,   $,  f, '    '),   ! 

	(kaddtofci, $,  f, '    '),   ! 
	(ksubtofci, $,  f, '    '),   ! 
!	(kiandtofc, $,  f, '    '),   ! 
!	(kiortofci, $,  f, '    '),   ! 
!	(kixortofc, $,  f, '    '),   ! 
	(kshltofci, $,  f, '    '),   ! 
	(kshrtofci, $,  f, '    '),   ! 

	(kindexmf,  $,  0, '    '),   ! 
	(kindexff,  $,  0, '    '),   ! 
	(kswitchf,  $,  0, '    '),   ! 
	(kpushptrf, $,  0, '    '),   ! 

	(kpushipm,  $,  0, '    '),   ! 
	(kpushipf,  $,  0, '    '),   ! 
	(kpopipm,   $,  0, '    '),   ! 
	(kpopipf,   $,  0, '    '),   ! 

	(kupbm,     $,  0, '    '),   ! 
	(kupbf,     $,  0, '    '),   ! 
	(klenf,     $,  0, '    '),   ! 

	(kstoref,   $,  0, '    '),   ! 

	(kwheneqci, $,  i, '    '),   ! 
	(kwhenneci, $,  i, '    '),   ! 

	(klastpcl,  $,  0, '    ')
end

global const kfirst=kpushfff
global const klast=kwhenneci

record bintorec=
	int pclop
	ref proc fnadd
	ref proc fnaddmixed
end

global[]bintorec bintotable = (
	(kadd,		cast(var_add),		cast(var_addmixed)),
	(ksub,		cast(var_sub),		cast(var_submixed)),
	(kmul,		cast(var_mul),		cast(var_mulmixed)),
	(kdiv,		cast(var_div),		cast(var_divmixed)),
	(kiand,		cast(var_iand),		nil),
	(kior,		cast(var_ior),		nil),
	(kixor,		cast(var_ixor),		nil),
	(kmin,		cast(var_min),		nil),
	(kmax,		cast(var_max),		nil),
	(kshl,		cast(var_shl),		nil),
	(kshr,		cast(var_shr),		nil),
)

=== qq_pclgen.m 0 0 18/48 ===
!not opcodes, just used internally here for conditional jump logic
!const kjumpt = 1
!const kjumpf = 0

!loop stack data reused by GENMPL
const maxloopindex=20
[maxloopindex, 4]int loopstack
[maxloopindex]int trylevelstack
global int loopindex=0
int looptrylevel			!return by findlooplabel

const maxswitchrange=512
const maxlocals=300
const maxparams=100

const maxunits=400					!for constructors
int trylevel=0
!int currfunction=0				!0/1/2 = not a function/proc/function

!vars within curr procdef
int retindex						!common return point; label no
int retvaloffset					!offset of return value for procs (as stack slots)
int nprocparams						!no. of params
!global int nproclocals				!no. of locals
global pcl pprocentry				!pointer to PROCENT op; for updating locals due to AVs
const retaddrslots = 1				!+1 or +2, added to param indices (depends on return info size)
int procskiplabel

global proc evalunit(unit p, int res=1)=
!p is a single executable unitrec; not a list or const
!should be called via execblock() to executate a code item in a unitrec
!note: sometimes res can be 2, (passing on a res=2 from an outer stmt)
!that should be treated here same as 1 (res=2 has special meaning from pclhasvalue[] only)
	unit a, b
	symbol d
	int procflag, index

!CPL "EVALUNIT", JTAGNAMES[P.TAG]

	qpos:=p.pos

	a:=p.a
	b:=p.b

	switch p.tag
	when jintconst then
		genpc_int(kpushci, p.value)

	when jrealconst then
		genpc_real(kpushcr, p.xvalue)

!	when jenumconstthen
!		genpc_int2(kpushenum, p.value, p.mode)

	when jnone then

	when jstringconst then
		pushstring(p.svalue)

	when jname then
		d:=p.def
!CPL "NAME",D.NAME,NAMENAMES[D.NAMEID]
		case d.nameid
		when paramid then
			genpc_name(kpushm, d)
			if d.mbyref then
				genpc(kpushptr)
			fi

		when frameid, staticid then
			genpc_name(kpushm, d)

		when labelid then
			if d.labelno=0 then
				d.labelno:=createfwdlabel()
			fi
			if not res then
				genpc_lab(kjump, d.labelno)
				return
			else
				genpc_lab(kpushlab, d.labelno)
			fi

!		when dllvarid then
!			genpc_name(kpushx, d)

		else
			genpc_name(kpushsym, d)
		esac

	when jsymbol then			!assume a is jname
		if a.tag=jname then
			genpc_name(kpushsym, a.def)
		else
			gerror(".$ name expected")
		fi

	when jblock then
		if a then
			while a and a.nextunit do
				evalunit(a, 0)
				a:=a.nextunit
			od
			if a then
				evalunit(a, res)
			fi
		else
!			gerror("empty block")
		fi

	when jdecimal then
		pushstring(p.svalue)
		genpc(kmakedec)

	when jcall then
		do_call(p, a, b, res, procflag)
	when jreturn then
		do_return(p, a, res)
	when jcallhost then
		do_callhost(p, a, res)

	when jassign then
		do_assign(a, b, res, p.flag)
	when jto then
		do_to(p, a, b)
	when jif then
		do_if(p, a, b, b.nextunit, res)
	when jfor		then
		do_for(p, a, b
)
	when jforx then
		do_forx(p, a, b)

	when jforall, jforeach then
		do_forall(p, a, b)

	when jwhile then
		do_while(p, a, b)

	when jrepeat then
		do_repeat(p, a, b)

	when jgoto then
		if a.tag=jname and a.def.nameid=labelid then
			d:=a.def
			if d.labelno=0 then
				d.labelno:=createfwdlabel()
			fi
			genpc_lab(kjump, d.labelno)
		else
			evalunit(a)
			genpc(kjumpptr)
		fi

	when jlabeldef then
		d:=a.def
GENCOMMENT(D.NAME)
		if d.labelno=0 then
			d.labelno:=definelabel()
		else
			index:=d.labelno
			definefwdlabel(index)
		fi

	when jloop then
		do_loop(p)

	when jdo then
		do_do(p, a)
	when jcase, jdocase then
		do_case(p, a, b, res)
	when jswitch, jdoswitch then
		do_switch(p, a, b, res)
	when jswap then
		evalref(a)
		evalref(b)
		genpc(kswap)

	when jselect then
		do_select(a, b, res)
	when jprint then
		do_print(p, a, b)
	when jfprint then
		do_fprint(p, a, b, b.nextunit)
	when jread then
		do_read(p, a, b)

	when jstop then
		if a then
			evalunit(a)
		else
			genpc_int(kpushci, 0)
		fi
		genpc(kstop)

	when jtry then
		do_try(p, a, b)

	when jandl then
		do_andl(a, b)
	when jorl then
		do_orl(a, b)
	when jmakelist then
		do_pushlist(a, p.length)
		genpc_xy(kmakelist, p.length, p.lower)

	when jmakeset then
		do_pushlist(a, p.length)
		genpc_xy(kmakeset, p.length)

	when jmakedict then
		do_makedict(a, p.length)

	when jmakerange then
		evalunit(a)
		evalunit(b)
		genpc(kmakerang)

	when jkeyvalue then
		evalunit(a)
		evalunit(b)

	when jmap then
		do_map(p, a, b)

	when jbin then
		case p.pclop
		when kidiv then
		do_idiv(a, b)
		when kirem then
		do_irem(a, b)
		else
			evalunit(a)
			evalunit(b)
			genpc(p.pclop)
		esac

	when jbinto then
		evalunit(b)
		evalref(a)
		genpc(kbinto)
		for i to bintotable.len do
			if bintotable[i].pclop=p.pclop then
				pccurr.bintoindex:=i
				exit
			fi
		else
			gerror("No binto entry")
		od

!		pccurr.bintocode:=p.pclop

	when junary, jproperty then
		evalunit(a)
		genpc(p.pclop)
		if p.pclop=kbounds then pccurr.n:=1 fi

	when junaryto then
		evalref(a)
		genpc(kunaryto)
		pccurr.pclop:=p.pclop

	when jappendto, jconcatto then
		evalunit(b)
		evalref(a)
		genpc((p.tag=jappendto|kappendto|kconcatto))

	when jnotl then
		evalunit(a)
		genpc(knotl)

	when jistruel then
		evalunit(a)
		genpc(kistruel)

	when jistype then
		evalunit(a)
		genpc(kistype)
		pccurr.usertag:=p.mode

	when jisvoid then
		evalunit(a)
		genpc(kisvoid)
		pccurr.n:=p.flag

	when jdot then! do_bin(a, b, kdot)
		evalunit(a)
		if b.def.genfieldindex=0 then gerror(".m?") fi

		genpc_name(kdot, b.def)

	when jindex then
		do_bin(a, b, kindex)

	when jdotindex then
		do_bin(a, b, kdotix)

	when jkeyindex then
		evalunit(a)
		evalunit(b)
		if b.nextunit then
			evalunit(b.nextunit)
		else
			genpc(kpushvoid)
		fi
		genpc(kkeyindex)

	when jptr then
		evalunit(a)
		genpc(kpushptr)

	when jaddrof then
		if p.flag=1 then			! ^x
			if a.tag=jptr then			!^a^ cancel out (a might be byref param)
				evalunit(a.a)
			else
				evalref(a)
			fi
		else
			evalref(a)
			genpc(kconvrefp)
		fi

	when jconvert then
		do_convert(p)

	when jtypepun then
		evalunit(a)
		genpc_int(ktypepun, p.mode)

	when jtypeconst then
		genpc_int(kpushtype, p.mode)

	when joperator then
		genpc_int(kpushopc, p.pclop)

	when jincrload, jloadincr then
		do_incr(p, a, res)

	when jnil then
		genpc(kpushnil)

	when jraise then
		evalunit(a)
		genpc(kraise)

	when jvoid then
		genpc(kpushvoid)

	when jeval then
		evalunit(a)
		genpc_n(kunshare, 1)

	when jgettype then
		evalunit(a)
		genpc_n(ktype, p.flag)

	when jin then
		evalunit(a)
		evalunit(b)
		genpc(kin)
		pccurr.n:=p.flag

	when jinx then
		evalunit(a)
		evalunit(b)
		genpc(kinx)
		pccurr.n:=p.flag

	when jcmp then
		evalunit(a)
		evalunit(b)
		genpc_n(kcmp, p.condcode)

	when jmaths then
		evalunit(a)
		if p.mathsop=mm_sqr then
			genpc(ksqr)
		else
			genpc(kmaths)
			pccurr.mathscode:=p.mathsop
		fi

	when jmaths2 then
		evalunit(a)
		evalunit(b)
		genpc(kmaths2)
		pccurr.mathscode:=p.mathsop

	else
		gerror_s("UNSUPPORTED TAG:", JTAGNAMES[P.TAG], p)
	end switch

	case jhasvalue[p.tag]
	when 0 then
		if res then
			unless p.tag in [jprint, jfprint] and p.flag iand pr_sprint then
				gerror_s("Value expected:", jtagnames[p.tag])
			end
		fi
	when 1 then
		if not res then
			if p.tag=jcall and procflag=1 then		!procs have no ret value
			elsif p.tag in [jincrload, jloadincr] then
			elsif p.tag=jcallhost and hostisfn[p.index]=0 then
			else
				genpc_n(kunshare, 1)
			fi
		fi
	esac						!else ignore when 2, as already dealt with
end

global proc gencodemodule(isubprog sp, int moduleno)=
	const maxanonprocs=100
	[maxanonprocs]symbol anonprocs
	int nanonprocs:=0

	symbol d, e
	int lab
	int a:=sp.firstmodule
	int b:=sp.lastmodule
	ifile pm:=modules[moduleno]
	pcl pc, pctarget
	ref[]byte labelmap

	currmodule:=pm
	stcurrproc:=stcurrmodule:=currmodule.def

!CPL "GENCODE", SP.NAME, PM.NAME

	resetpcl(pm.size)

!GOTO FINISH

	gencomment("Module data init code:")
!SKIP

	qpos:=0
	qpos.[24..31]:=moduleno

!CPL "///////////////", QPOS

!jump around stop/raise block needed for reentry
!	if n=1 then
!CPL $LINENO
	if moduleno=a then
		lab:=createfwdlabel()
		genpc_lab(kjump, lab)
		genpc_n(kstop, 1)
		stopseq:=pccurr

		raiseseq:=pccurr+1
		genpc_int(kpushci, 0)
		genpc(kraise)
		definefwdlabel(lab)
	fi

!CPL $LINENO
	d:=stcurrmodule.deflist
	while d do
		if d.nameid=staticid and d.code then
			evalunit(d.code)
!CPL =D.INITCODE, D.NAME
			if d.initcode=3 then
				genpc(kcopy)
			fi
			genpc_name(kzpopm, d)
		elsif d.nameid=procid then
			e:=d.deflist
			while e do
				if e.nameid=staticid and e.code then
					evalunit(e.code)
					genpc_name(kzpopm, e)
				elsif e.nameid=anonprocid then
					if nanonprocs>=maxanonprocs then gerror("Too many anons") fi
					anonprocs[++nanonprocs]:=e
				fi
				e:=e.nextdef
			od
		fi
		d:=d.nextdef
	od	
!CPL $LINENO

	if moduleno=a then
		for i:=b downto a+1 do
			genpc_name(kmodcall, modules[i].def)
		od
		for i:=b downto a+1 do
			if modules[i].startfn then
				genpc_name(kcallproc, modules[i].startfn)
!				genopnd_int(0)
			fi
		od

		if currmodule.startfn then
			genpc_name(kcallproc, currmodule.startfn)
!			genopnd_int(0)
		fi

		if currmodule.mainfn then
			genpc_name(kcallproc, currmodule.mainfn)
!!			genopnd_int(0)
		fi

		evalunit(stcurrmodule.code, 0)
		genpc_int(kpushci, 0)
		genpc(kstop)
	else
		evalunit(stcurrmodule.code, 0)
		genpc(kmodret)
	fi
!CPL $LINENO

	gencomment("Procs:")
	d:=stcurrmodule.deflist
	while d do
		switch d.nameid
		when procid, anonprocid then
			do_procdef(d)
		when staticid then
!		when typeid then
		when recordid then
			e:=d.deflist
			while e, e:=e.nextdef do
				if e.nameid=procid then
					do_procdef(e)
				fi
			od

		when constid then
		when enumid then
		when labelid then
		when typeid then
		when dllprocid then
		when aliasid then
		when macroid then
		when dllvarid then
		else
			gerror_s("?Module def:", namenames[d.nameid])
		end switch

		d:=d.nextdef
	od	
!CPL $LINENO

	for i to nanonprocs do
		do_procdef(anonprocs[i])
	od

	genpc(kendmod)

!scan pcl operands and convert label operands to pcl ref
	labelmap:=pcm_allocz(nextlabelno)

	pc:=pcstart
	while pc<=pccurr, ++pc do
		if pclopnd[pc.opcode]=clabel then
			lab:=pc.labelno
			pctarget:=labelpctable[lab]
!CPL "LABEL FIXUP:", LAB,"=>", PCTARGET
			if pctarget=nil then
				gerror_s("Lab undef:",strint(lab))
			fi
			labelmap[lab]:=1				!indicate has been referenced
			pc.labelref:=pctarget			!update from 1-NLABEL to 1-NPCL
		fi
	od

!use labelmap to unset those labeled pcl ops that have not been used
	for i to nextlabelno do
		if labelmap[i] then
			labelpctable[i].haslabel:=1
		fi
	od

	pcm_free(labelmap, nextlabelno)

	pm.pcstart:=pcstart
!	pm.pcend:=pcend
	pm.pcend:=pccurr
!	pm.pcsize:=pccurr-pcstart+1
	pm.pcsize:=getpcloffset(pccurr,pcstart)+1
	pm.pcsourcestart:=pcsourcestart
!CPL $LINENO

!CPL "------DONE GENPCL", PCLSTART, PCLNEXT-PCLSTART, PCLNAMES[PCLSTART^]

end

proc do_procdef(symbol p) =
	int nfreevars, nnofreevars
	int isfunc
	symbol oldcurrproc

	oldcurrproc:=stcurrproc			!might be a method

	stcurrproc:=p

	retindex:=createfwdlabel()
	isfunc:=p.misfunc

	genprocentry(p, nfreevars, nnofreevars)

	if p.code=nil then
		gerror_s("Empty proc body", p.name)
	else
		evalunit(p.code, isfunc)

		if isfunc then
!CPL "CHECK BODY", JTAGNAMES[P.CODE.TAG]
			if not checkblockreturn(p.code) then
				gerror("Func: return value missing")
			fi
		fi

	fi

	definefwdlabel(retindex)			!common return point
	genprocexit(nfreevars, nnofreevars, isfunc)
	genpc(kprocend)



	if pprocentry.n=0 then			!skip past procentry instr
		++pprocentry
!		++p.labelref
	fi
	p.labelref:=pprocentry
!CPL "PROCDEF", =PCLNAMES[PPROCENTRY.OPCODE], =P.LABELREF

	stcurrproc:=oldcurrproc
end

proc genprocentry(symbol p, int &nfreevars, &nnofreevars) =		!GENPROCENTRY
	[200]char str
	int n
	symbol d

	genpc_name(kprocdef, p)

	nprocparams:=nproclocals:=0

	d:=p.deflist
	while d do
		case d.nameid
		when frameid then
			++nproclocals
			d.index:=nproclocals
		when paramid then
			++nprocparams
		esac

		d:=d.nextdef
	od

	d:=p.deflist
	n:=nprocparams

	while d, d:=d.nextdef do
		case d.nameid
		when paramid then
			--n
			d.index:=-(n+retaddrslots)
		esac

	od

	retvaloffset:=-(nprocparams+retaddrslots)

!	p.labelno:=definelabel()
!CPL "GENPROCENTRY",P.LABELNO, p.labelref
	genpc_n(kprocent, nproclocals)

	pprocentry:=pccurr

	d:=p.deflist
	while d do
		case d.nameid
		when frameid then
			if d.code then
				evalunit(d.code)
				if d.initcode=3 then
					genpc(kcopy)
				fi
				genpc_name(kzpopm, d)
			fi
		esac

		d:=d.nextdef
	od
end

proc genprocexit(int nfree, nnofree, isfunc)=		!GENPROCEXIT
	int offset

	if isfunc then
		offset:=-(nprocparams+1)*varsize
		genpc_xy(kretfn, nproclocals, offset)
		pccurr.n:=nprocparams
	else
		genpc_n(kretproc, nprocparams)
		pccurr.x:=nproclocals
	fi


!	if isfunc then
!		offset:=-(nprocparams+1)*varsize
!		genpc_int(kpopret, offset)
!	fi
!	if nproclocals then
!		genpc_n(kunshare, nproclocals)
!	fi
!
!	genpc_n(kreturn, nprocparams)
end

proc evalref(unit p)=
	unit a, b, c
	symbol d
	int lab1, lab2
	a:=p.a
	b:=p.b

	switch p.tag
	when jname then
		d:=p.def
		if d.nameid in [procid, dllprocid] then
			gerror("^ not allowed")
		fi	

		if d.nameid=paramid and d.mbyref then
			genpc_name(kpushm, d)
		else
			genpc_name(kpushmref, d)
		fi

	when jdot then! do_binref(a, b, kdotref)
		evalunit(a)
		if b.def.genfieldindex=0 then gerror(".m2?") fi
		genpc_name(kdotref, b.def)

	when jindex then! do_binref(a, b, kindexref)
		evalunit(a)
		evalunit(b)
		genpc(kindexref)

	when jdotindex then! do_binref(a, b, kdotindexref)
!		evalunit(a)
		evalref(a)
		evalunit(b)
		genpc(kdotixref)

	when jkeyindex then! do_binref(a, b, kkeyindexref)
		evalunit(a)
		evalunit(b)
		if b.nextunit then gerror("Def val not allowed") fi
		genpc(kkeyixref)

	when jptr then
		evalunit(a)

	when jif then
		lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)
		lab2:=createfwdlabel()

		genjumpcond(kjumpf, p.a, lab1)
		evalref(p.b)
		genjumpl(lab2)
		definefwdlabel(lab1)
		evalref(p.b.nextunit)
		definefwdlabel(lab2)
	else
!		case p.tag
!		when jif then
!			do_if(p, a, b, c, 1)
!		when jlongif then
!			do_longif(p, a, b, 1)
!		when jselect then
!			do_select(p, a, b, c, 1)
!		when jswitch then
!			do_switch(p, a, b, c, 0, 1)
!		when jcase then
!			do_case(p, a, b, c, 0, 1)
!		else
!			PRINTUNIT(P)
			gerror_s("evalref", jtagnames[p.tag])
!		esac
	end switch
end

proc genjumpcond(int opc, unit p, int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
	unit q, r, s
	int oldpos, lab2, i

	q:=p.a
	r:=p.b

	switch p.tag
	when jandl then
		case opc
		when kjumpf then
			genjumpcond(kjumpf, q, lab)
			genjumpcond(kjumpf, r, lab)
		when kjumpt then
			lab2:=createfwdlabel()
			genjumpcond(kjumpf, q, lab2)
			genjumpcond(kjumpt, r, lab)
			definefwdlabel(lab2)
		esac

	when jorl then
		case opc
		when kjumpf then
			lab2:=createfwdlabel()
			genjumpcond(kjumpt, q, lab2)
			genjumpcond(kjumpf, r, lab)
			definefwdlabel(lab2)
		when kjumpt then
			genjumpcond(kjumpt, q, lab)
			genjumpcond(kjumpt, r, lab)
		esac

	when jnotl then
		case opc
		when kjumpf then
			genjumpcond(kjumpt, q, lab)
		when kjumpt then
			genjumpcond(kjumpf, q, lab)
		esac

	when jistruel then
		genjumpcond(opc, q, lab)

	when jblock then
		while q and q.nextunit do
			evalunit(q)
			q:=q.nextunit
		od
		genjumpcond(opc, q, lab)

	when jcmp then
		evalunit(q)
		evalunit(r)
		gcomparejump(opc, p.condcode, lab)

	when jcmpchain then
		r:=q.nextunit
		i:=1
		if opc=kjumpf then
			while r do
				evalunit(q)
				evalunit(r)
				gcomparejump(kjumpt, revconds[p.cmpconds[i]], lab)
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
					gcomparejump(kjumpt, revconds[p.cmpconds[i]], lab2)
				else
					gcomparejump(kjumpt, p.cmpconds[i], lab)
				fi
				++i
				q:=r
				r:=r.nextunit
			od
			definefwdlabel(lab2)
		fi
	else
		evalunit(p)
		genpc_lab(opc, lab)
	end switch
	qpos:=oldpos

end

proc gcomparejump(int opc, cond, lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode

	if opc=kjumpf then				!need to reverse condition
		cond:=revconds[cond]		!eq_cc => ne_cc, etc
	fi

	genpc_lab(kjumpeq+cond, lab)
end

proc genjumpl(int lab)=
!generate unconditional jump to label
	genpc_lab(kjump, lab)
end

global proc stacklooplabels(int a, b, c)=
!list of labels associated with a loop: a/b/c are redo/next/exit
	if loopindex>=maxloopindex then
		gerror("Too many nested loops")
	fi

	++loopindex
	loopstack[loopindex, 1]:=a
	loopstack[loopindex, 2]:=b
	loopstack[loopindex, 3]:=c
end

global proc unstacklooplabels=
	--loopindex
end

global function findlooplabel(int k, n)int=
!k is 1, 2, 3, 4 for label A, B, C, D
!n is a 1, 2, 3, etc, according to loop nesting index
	int i

	if n=0 then			!outermost loop
		i:=1
	else
		i:=loopindex-(n-1)		!point to entry
	fi

	if i<1 or i>loopindex then
		gerror("Bad loop index")
	fi

	looptrylevel:=trylevelstack[i]
	return loopstack[i, k]
end

proc do_assign(unit a, b, int res, deepcopy=0)=
	unit q
	int n

	if a.tag=b.tag=jmakelist then
		if res then gerror("mult/ass::=") fi
!		if deepcopy then gerror("mult/ass::=") fi
		do_multassign(a, b, deepcopy, res)
		return
	fi

	evalunit(b)
	if deepcopy then
		genpc(kcopy)
	fi

	do_store(a, res)
end

proc do_bin(unit a, b, int opc)=
	evalunit(a)
	evalunit(b)
	genpc(opc)
end

proc do_binref(unit a, b, int opc)=
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
	while a, a:=a.nextunit do
		evalunit(a)
	od
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
	genpc_xy(kmakedict, n)
end

proc do_call(unit p, a, b, int res, &procflag)=
	int nargs, nsimple, isfunc, kwdindex
	symbol d
	unit c
	[maxparams]unit arglist

	isfunc:=1
	nargs:=nsimple:=0
	kwdindex:=0
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

	case a.tag
	when jname then
		d:=a.def
retry:
		case d.nameid
		when procid, anonprocid then
			if d.misfunc then
				genpc(kpushvoid)
				nargs:=pushparams(d, arglist, nargs, kwdindex)
!				genpc_name(kcallfn, d)
				genpc_name(kcallproc, d)
			else					!proc call
				isfunc:=0
				nargs:=pushparams(d, arglist, nargs, kwdindex)
				genpc_name(kcallproc, d)
			fi
			pccurr.n:=nargs

		when dllprocid then
			if not d.misfunc then
				isfunc:=0
			else
				genpc(kpushvoid)
			fi
			nargs:=pushparams(d, arglist, nargs, kwdindex)
			genpc_name(kcalldll, d)
			pccurr.n:=nargs

		when aliasid then
			d:=d.alias
			goto retry
		when staticid, frameid, paramid then
			goto docallptr
!
		else
			gerror_s("CAN'T CALL:", namenames[d.nameid])
		esac
	when jdot then
		if kwdindex then docallptr fi		!share error
		genpc(kpushvoid)
		evalref(a.a)					!push &self arg
		for i to nargs do				!any extra ones
			evalunit(arglist[i])
		od
		evalunit(a)						!push lhs again, this time for dot
		genpc_n(kcallptr, ++nargs)

	else
docallptr:
		if kwdindex then gerror("Kwd params not allowed for fnptr") fi
		genpc(kpushvoid)
		for i to nargs do
			evalunit(arglist[i])
		od
		evalunit(a)
		genpc_n(kcallptr, nargs)
	esac

	if res and not isfunc then
		gerror("Func ret value expected")
	fi

	procflag:=not isfunc
end

function pushparams(symbol d, []unit &arglist, int nargs, kwdindex)int=
!push args for a known, named function
!will deal with missing/optional args, default values, and keyword params
!should work also for dll procs
!In all cases, first nparams items in d.deflist will be parameter names, 
!For dlls with no named params, the entries will be $1 etc.

	int nparams, extra, n
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

	if kwdindex then
		pushkwdparams(d, arglist, nargs, kwdindex)
		return d.nparams
	fi

	extra:=0

	if nargs=nparams then
		for i to nargs do
			evalparam(arglist[i], byreflist[i])
		od
		return nargs
	elsif nargs<nparams then	!trailing args missing
		for i to nargs do
			evalparam(arglist[i], byreflist[i])
		od

		for i:=nargs+1 to nparams do
			p:=paramlist[i]
			if not p.code and not p.moptional then
				gerror_s("Param not optional:", strint(i))
			fi
			if p.code then
				if byreflist[i] then gerror("byref with default val") fi
				evalunit(p.code)
			else
				genpc(kpushvoid)
			fi
		od
		return nparams
	else						!nargs>nparams: variadic
		for i to nparams do
			evalparam(arglist[i], byreflist[i])
		od

		if not d.mvarparams then
			gerror("Too many args")
		fi
		for i:=nparams+1 to nargs do
			evalunit(arglist[i])			!o/p variadic args
		od
		return nargs
	fi
end

proc evalparam(unit a, int byref)=
	if byref then
		evalref(a)
	else
		evalunit(a)
	fi
end


proc pushkwdparams(symbol d, []unit &arglist, int nargs, kwdindex)=
	int nparams, i, j, k
	[maxparams]symbol paramlist
	[maxparams]byte byreflist
	[maxparams]unit keyunits
	unit p, q
	symbol e

	nparams:=d.nparams

	e:=d.deflist
	for i to nparams do
		paramlist[i]:=e
		byreflist[i]:=e.mbyref
		e:=e.nextdef
	od

	if nargs>nparams then
		gerror("Too many args")
	fi

	for i:=kwdindex to nparams do
		keyunits[i]:=nil			!indicate param not set
	od

	for i to kwdindex-1 do			!do positional params
		evalparam(arglist[i], byreflist[i])
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

		if k=0 then gerror_s("Can't find kwd param:", e.name) fi
		if k<kwdindex then gerror_s("Kwd arg already positional:", e.name) fi
		if keyunits[k] then gerror_s("Repeating kwd arg:", e.name) fi

		keyunits[k]:=p.b
	od

	for i:=kwdindex to nparams do
		if keyunits[i]=nil then
			q:=paramlist[i].code
			if q=nil and not paramlist[i].moptional then
				gerror_s("Param not optional:", strint(i))
			fi
			keyunits[i]:=q			!q is nil when default value not set
		fi
	od

!	for i:=nparams downto kwdindex do
	for i:=kwdindex to nparams do
		if keyunits[i] then
			evalparam(keyunits[i], byreflist[i])
		elsif byreflist[i] then
			gerror("byref param not optional")
		else
			genpc(kpushvoid)
		fi
	od
end

proc do_if(unit p, a, b, pelse, int res)=
	int lab1, lab2

	lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)

	if pelse or res then lab2:=createfwdlabel() fi	!label past else part

	genjumpcond(kjumpf, a, lab1)

	evalunit(b, res)

	if pelse or res then
		genjumpl(lab2)
		definefwdlabel(lab1)
		if pelse then
			evalunit(pelse, res)
		else
			genpc(kpushvoid)
		fi
		definefwdlabel(lab2)
	else
		definefwdlabel(lab1)
	fi
end

proc do_do(unit p, a)=
	int lab_abc, lab_d, lab_test
	lab_abc:=definelabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_abc, lab_abc, lab_d)

	evalunit(a, 0)

	genjumpl(lab_abc)
	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_loop(unit p) =
	int n, index

	index:=p.a.value
	if index=0 then index:=loopindex fi

	n:=findlooplabel(p.loopcode, index)
	if n=0 then
CPL "BAD LOOP"
!		gerror("Bad exit/loop index", p)
	else
		genjumpl(n)
	fi
end

proc do_to(unit p, pcount, pbody)=
	int lab_b, lab_c, lab_d
	symbol temp
	unit pav

	pav:=pcount.nextunit
	temp:=pav.def

	evalunit(pcount)
	genpc_name(kzpopm, temp)

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	stacklooplabels(lab_b, lab_c, lab_d)

!check for count being nonzero
	if pcount.tag<>jintconst then			!assume const limit is non-zero
		genpc_name(kpushm, temp)
		genpc_int(kpushci, 0)
		genpc_lab(kjumple, lab_d)

	elsif pcount.value<=0 then		!const <=0, skip body
		genpc_lab(kjump, lab_d)
	fi

	definefwdlabel(lab_b)
	evalunit(pbody, 0)
	definefwdlabel(lab_c)

	genpc_lab(ktom+temp.isframe, lab_b)
	genpc_name(kpushm, temp)

	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_while(unit p, pcond, pbody) =
	int lab_b, lab_c, lab_d, lab_incr
	unit pincr:=pcond.nextunit

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pincr then
		lab_incr:=createfwdlabel()
	else
		lab_incr:=lab_c
	fi

	stacklooplabels(lab_b, lab_c, lab_d)

	genjumpl(lab_incr)		!direct to condition code which is at the end

	definefwdlabel(lab_b)

	evalunit(pbody, 0)

	definefwdlabel(lab_c)

	if pincr then
		evalunit(pincr)
		definefwdlabel(lab_incr)
	fi

	genjumpcond(kjumpt, pcond, lab_b)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_repeat(unit p, a, b) =
	int lab_b, lab_c, lab_d

	lab_b:=definelabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_b, lab_c, lab_d)

	evalunit(a, 0)

	definefwdlabel(lab_c)

	unless b.tag=jintconst and b.value=0 then
		genjumpcond(kjumpf, b, lab_b)
	end

	definefwdlabel(lab_d)
	--loopindex
end

proc do_for(unit p, pvar, pbody)=
! a = pvar, pfrom, pto, [pstep]
! b = pbody [pelse]
	unit pfrom, pto, pstep, pelse, plimit, pautovar
	symbol dvar, limitvar
	int lab_b, lab_c, lab_d, lab_e, opc, oldqpos
	int step, fromval, limit, jumpinto

!CPL "FOR"
	pfrom:=pvar.nextunit
	pto:=pfrom.nextunit
	pstep:=pto.nextunit
	pautovar:=nil
	if pstep then
		gerror("By N not implem")
	fi

	pelse:=pbody.nextunit

	dvar:=pvar.def

	if pto.tag not in [jintconst, jname] or
		 pto.tag=jname and pto.def.isframe<>dvar.isframe then
		pautovar:=createavnamex(stcurrproc)
	fi

	if p.flag then
		step:=-1
	else
		step:=1
	fi

	jumpinto:=1			!assume jumping straight into increment

!now start generating code
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	lab_e:=(pelse|createfwdlabel()|lab_d)
	stacklooplabels(lab_b, lab_c, lab_d)

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
		genpc_int(kpushci, pfrom.value)

		genpc_name(kpopm, dvar)
	else
		evalunit(pfrom)
		genpc_name(kpopm, dvar)

		genpc_name(kincrtom, dvar)
		pccurr.x:=-step

	fi

	if pautovar then
		evalunit(pto)
		limitvar:=pautovar.def
		genpc_name(kzpopm, limitvar)
		pto:=pautovar
	else
		limitvar:=pto.def
	fi

	if jumpinto then
		genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C:
	fi
	definefwdlabel(lab_b)

	evalunit(pbody, 0)				!do loop body

	definefwdlabel(lab_c)

	if step>0 then

		if pto.tag=jintconst then
			opc:=kformci+dvar.isframe
		elsif dvar.isframe=limitvar.isframe then
			opc:=kformm+dvar.isframe
		else
			gerror("for:mixed m/f vars")
		fi

		oldqpos:=qpos
		qpos:=p.pos
		genpc_lab(opc, lab_b)
		qpos:=oldqpos

		genpc_name(kpushm, dvar)

		if pto.tag=jintconst then
			genpc_int(kpushci, pto.value)
		else
			genpc_name(kpushm, limitvar)
		fi
	else
		genpc_name(kincrtom, dvar)
		pccurr.x:=-1
		genpc_name(kpushm, dvar)
		if pto.tag=jintconst then
			genpc_int(kpushci, pto.value)
		else
			genpc_name(kpushm, limitvar)
		fi
		genpc_lab(kjumpge, lab_b)
	fi

	if pelse then
		definefwdlabel(lab_e)
		evalunit(pelse, 0)			!any else part
	fi

	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_forx(unit p, pvar, pbody)=
! a = pvar, pbounds
! b = pbody [pelse]
	unit pbounds, pelse, plimit, pautovar
	symbol dvar, limitvar
	int lab_b, lab_c, lab_d, lab_e, opc

CPL "FORX"

	pbounds:=pvar.nextunit

	pautovar:=createavnamex(stcurrproc)

	pelse:=pbody.nextunit
	dvar:=pvar.def

!now start generating code
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	lab_e:=(pelse|createfwdlabel()|lab_d)
	stacklooplabels(lab_b, lab_c, lab_d)

	evalunit(pbounds)				!stack has lwb, upb
	limitvar:=pautovar.def
	genpc_name(kzpopm, limitvar)

	genpc_int(kpushci, 1)
	genpc(ksub)
	genpc_name(kpopm, dvar)		!from value

	genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C:
	definefwdlabel(lab_b)

	evalunit(pbody, 0)				!do loop body

	definefwdlabel(lab_c)
	if dvar.isframe=limitvar.isframe then
		genpc_lab(kformm+dvar.isframe, lab_b)
	else
		gerror("forx:mixed m/f")
	fi
	genpc_name(kpushm, dvar)
	genpc_name(kpushm, limitvar)

	if pelse then
		definefwdlabel(lab_e)
		evalunit(pelse, 0)			!any else part
	fi

	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_print(unit p, a, b)=
	int issprint
	unit x

	issprint:=p.flag iand pr_sprint

!global const pr_newline = 1
!global const pr_format = 2
!global const pr_sprint = 4


	if issprint then
		callhostfn(h_strstartprint)
	else
		if a then
			evalunit(a)
			callhostfn(h_startprint)
		else
			callhostfn(h_startprintcon)
		fi
	fi

	x:=b

	while x do
		case x.tag
		when jfmtitem then
			evalunit(x.b)
			evalunit(x.a)
			callhostfn(h_print)
		when jnogap then
			callhostfn(h_printnogap)
		when jspace then
			callhostfn(h_printspace)
		else
			evalunit(x)
			callhostfn(h_print_nf)
		esac
		x:=x.nextunit
	od

	if p.flag iand pr_newline then
		callhostfn(h_println)
	fi
	if issprint then
		genpc(kpushvoid)
		callhostfn(h_strendprint)
	else
		callhostfn(h_endprint)
	fi
end

proc do_fprint(unit p, a, b, c)=
	int issfprint
	unit x

	issfprint:=p.flag iand pr_sprint

	if issfprint then
		callhostfn(h_strstartprint)
	else
		if a then
			evalunit(a)
			callhostfn(h_startprint)
		else
			callhostfn(h_startprintcon)
		fi
	fi

	evalunit(b)					!format string
	callhostfn(h_setformat)

	x:=c
	while x do
		case x.tag
		when jfmtitem then
			evalunit(x.b)
			evalunit(x.a)
			callhostfn(h_print)
		when jnogap then
			callhostfn(h_printnogap)
		else
			genpc(kpushvoid)
			evalunit(x)
			callhostfn(h_print)
		esac
		x:=x.nextunit
	od

	if p.flag iand pr_newline then
		callhostfn(h_println)
	fi
	if issfprint then
		genpc(kpushvoid)
		callhostfn(h_strendprint)
	else
		callhostfn(h_endprint)
	fi

end

proc do_read(unit p, a, b)=
unit x, xloop

if p.flag iand pr_newline then
	if a then
		evalunit(a)
		callhostfn(h_readln)
	else
		genpc(kpushvoid)
		callhostfn(h_readln)
	fi
fi

xloop:=b
while xloop do
	x:=xloop
	genpc(kpushvoid)
	if x.tag=jfmtitem then
		evalunit(x.b)
		callhostfn(h_sread)
		x:=x.a
	else
		genpc(kpushvoid)
		callhostfn(h_sread)
	fi
	if x.tag=jname then
		genpc_name(kpopm, x.def)
	else
		evalref(x)
		genpc(kpopptr)
	fi
	xloop:=xloop.nextunit
od
end

proc do_forall(unit p, pindex, pbody)=
!I think form pvar/prange into blocks, then those can be stored together
! a = pindex, plist, pvar
! b = pbody, [pelse]

	int lab_b, lab_c, lab_d, lab_e
	unit ploopvar, plist, pelse, plimitvar, plistvar
	symbol indexvar, limitvar, loopvar, listvar

!CPL "FORALL"
	plist:=pindex.nextunit
	ploopvar:=plist.nextunit

	if ploopvar=nil then			!no discrete index var
		ploopvar:=pindex

		pindex:=createavnamex(stcurrproc)

	fi
	loopvar:=ploopvar.def

	plimitvar:=createavnamex(stcurrproc)

	limitvar:=plimitvar.def
	indexvar:=pindex.def

	if plist.tag<>jname or plist.def.isframe<>loopvar.isframe then			!complex list

		plistvar:=createavnamex(stcurrproc)

		listvar:=plistvar.def
		evalunit(plist)
		genpc_name(kzpopm, listvar)
	else
		plistvar:=plist
		listvar:=plistvar.def
	fi

	unless indexvar.isframe=loopvar.isframe=listvar.isframe then
		gerror("forall: mixed vars")
	end

	pelse:=pbody.nextunit

!set up initial loop var
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	lab_e:=(pelse|createfwdlabel()|lab_d)
	stacklooplabels(lab_b, lab_c, lab_d)

!assume plist is a var where bounds are not known
!(can be optimised for a const range or a const list)
	genpc_name(kpushm, listvar)			!load the list
	genpc_n(kbounds, 2)				!extract bounds as (lower, upper); upper is tos

	genpc_name(kzpopm, limitvar)		!limit:=upb
	genpc_int(kpushci, 1)
	genpc(ksub)
	genpc_name(kzpopm, indexvar)		!index:=lwb-1 (will incr first thing)

	genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C:

	definefwdlabel(lab_b)

!start of iteration, set up next loop variable
	genpc_name(kpushm, listvar)
	evalunit(pindex)

	genpc((p.tag=jforall|kindex|kdotix))
	genpc_name(kpopm, loopvar)

	evalunit(pbody, 0)			!do loop body

	definefwdlabel(lab_c)

	if indexvar.isframe=limitvar.isframe then
		genpc_lab(kformm+indexvar.isframe, lab_b)
	else
		gerror("forall:mixed m/f")
	fi
	genpc_name(kpushm, indexvar)
	genpc_name(kpushm, limitvar)

	if pelse then
		definefwdlabel(lab_e)
		evalunit(pelse, 0)			!any else part
	fi

	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_case(unit p, pindex, pwhenthen, int res) =
!also temporarily deal wit switch/doswitch

	int lab_a, lab_d
	int loopsw, labnextwhen, labstmtstart, fmult
	unit w, wt, pelse

	if pindex.tag=jnone then
		do_case_nc(p, pindex, pwhenthen, res)
		return
	fi

	loopsw:=p.tag=jdocase or p.tag=jdoswitch
	pelse:=pindex.nextunit

	if loopsw then
		lab_a:=definelabel()
		lab_d:=createfwdlabel()
		stacklooplabels(lab_a, lab_a, lab_d)
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
				genpc_lab(kwheneq, labstmtstart)
			else
				genpc_lab(kwhenne, labnextwhen)
			fi
		od
		if fmult then
			definefwdlabel(labstmtstart)
		fi
		evalunit(wt.b, res)

		if not loopsw then
			genjumpl(lab_d)
		else
			genjumpl(lab_a)
		fi
		definefwdlabel(labnextwhen)
		wt:=wt.nextunit
	od

!at else part
	genpc_n(kunshare, 1)

	if pelse then
		evalunit(pelse, res)
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

proc do_case_nc(unit p, pindex, pwhenthen, int res) =
!when no control expression

	int lab_a, lab_d
	int labnextwhen, labstmtstart, fmult
	unit w, wt, pelse

	if p.tag<>jcase then gerror("case-nc") fi

	pelse:=pindex.nextunit

	lab_d:=createfwdlabel()

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
				genpc_lab(kjumpt, labstmtstart)
			else
				genpc_lab(kjumpf, labnextwhen)
			fi
		od
		if fmult then
			definefwdlabel(labstmtstart)
		fi
		evalunit(wt.b, res)

		genjumpl(lab_d)
		definefwdlabel(labnextwhen)
		wt:=wt.nextunit
	od

!at else part
	if pelse then
		evalunit(pelse, res)
	elsif res then
		gerror("Needs Else branch")
!		genpc(kpushvoid)
	fi

	definefwdlabel(lab_d)
end

proc do_try(unit p, a, b) =
	int labend, labx
	unit ptry, x, pexcept, pexcode

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
		genpc_lab(kpushtry, labx)
		genxy(getconstvalue(pexcode), 1)

		evalunit(ptry, 0)
		genjumpl(labend)
		definefwdlabel(labx)
		evalunit(pexcept.b, 0)
		definefwdlabel(labend)
		pexcept:=pexcept.nextunit
	od

	genpc_n(kaddsp, 1)
	--trylevel
end

function unitstoarray(unit p, ref[]unit plist, int maxunits)int=
!convert a linked list of units to a linear list
!return number of units
	int n

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

proc do_select(unit pindex, pplist, int res)=
!generate selectx expression
	int n, labend, i, lab, elselab
	unit x, pelse

	[maxswitchrange]unit plist
	[maxswitchrange+1]pcl labels

	pelse:=pindex.nextunit

	n:=unitstoarray(pplist, &plist, maxswitchrange)

	if n>maxswitchrange then
		gerror("Selectx too complex")
	fi

	labend:=createfwdlabel()

	evalunit(pindex)
!	genpc_int2(kselect, n, 1)
	genpc_xy(kswitch, 1, n)

	for i:=1 to n do
		genpc_lab(kjumplab, 0)
		labels[i]:=pccurr
	od
	genpc_lab(kjumplab, 0)
	labels[n+1]:=pccurr

!scan when statements again, o/p statements
	i:=1
	for i:=1 to n do
		x:=plist[i]
		lab:=definelabel()

		labels[i].labelno:=lab
		evalunit(x, res)

		genjumpl(labend)	!break to end of statement
	od

	elselab:=definelabel()

	labels[n+1].labelno:=elselab

	if pelse then
		evalunit(pelse, res)
	elsif res then
		genpc(kpushvoid)
	fi

	genpc(knop)

	definefwdlabel(labend)
end

proc do_andl(unit x, y)=
	int a, b

	a:=createfwdlabel()
	b:=createfwdlabel()

	genjumpcond(kjumpf, x, a)
	genjumpcond(kjumpf, y, a)

	genpc_int(kpushci, 1)
	genjumpl(b)
	definefwdlabel(a)
	genpc_int(kpushci, 0)
	genpc(knop)
	definefwdlabel(b)
end

proc do_orl(unit x, y)=
	int a, b
	a:=createfwdlabel()
	b:=createfwdlabel()

	genjumpcond(kjumpt, x, a)
	genjumpcond(kjumpt, y, a)
	genpc_int(kpushci, 0)
	genjumpl(b)
	definefwdlabel(a)
	genpc_int(kpushci, 1)
	genpc(knop)
	definefwdlabel(b)
end

proc do_incr(unit p, a, int res)=
	symbol d
	int opc

	opc:=(p.tag=jincrload|kincrload|kloadincr)

	if res then
		do_unaryref(a, opc)

	elsif a.tag=jname then
		d:=a.def
		if d.nameid=paramid and d.mbyref then
			do_unaryref(a, kincrptr)
		else
			genpc_name(kincrtom, a.def)
		fi
	else
		do_unaryref(a, kincrptr)
	fi

	pccurr.x:=(p.flag|-1|1)
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

!	if index=h_allparams and a=nil then
!		nparams:=1
!	else
		nparams:=nargs
!	fi

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
!		elsif i=1 and index=h_allparams and nargs=0 then
!			genpc_name(kpushmref, stcurrproc)
		else
			evalunit(plist[i])
		fi
	od  

	callhostfn(index, res)
end

proc callhostfn(int fnindex, calledasfn=0)=
!assume caller has verified that fn is a function when calledasfn is true
!called should have pushed retval as needed, and <aparams> params

	genpc_int(kcallhost, fnindex)
end

proc genfree(int n)=
	genpc_n(kunshare, n)
end

proc do_return(unit p, a, int res)=
!CPL "RETURN", NAMENAMES[STCURRPROC.NAMEID], STRMODE(STCURRPROC.MODE), STCURRPROC.MISFUNC

!CPL "RETURN", =RES

	if a then
		if not stcurrproc.misfunc then gerror("Proc can't return a value") fi
		evalunit(a)
	else
		if stcurrproc.misfunc then gerror("Func needs return value") fi

!	elsif currfunction=2 then
!		gerror("function needs return value")
	fi

	if not res then
		genjumpl(retindex)
	fi
end

proc do_multassign(unit a, b, int deepcopy, res)=
	unit p, q
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
		if deepcopy then
			genpc(kcopy)
		fi

		do_store(plist[i])
	od
end

proc do_store(unit a, int res=0)=
!store stack value to a
	symbol d
	unit p
	[100]unit plist
	int n

	if res and a.tag<>jname then
		genpc(kdupl)
	fi

	case a.tag
	when jname then
		d:=a.def
		if d.nameid=paramid and d.mbyref then
			if res then genpc(kdupl) fi
			genpc_name(kpushm, d)
			genpc(kpopptr)
		elsif res then
			genpc(kdupl)
			genpc_name(kpopm, d)
		elsif d.nameid in [procid, dllprocid] then
			gerror("Not lvalue")

!		elsif d.nameid=dllvarid then
!			genpc_name(kpopx, d)

		else
			genpc_name(kpopm, d)
		fi

	when jdot then
		evalunit(a.a)
		if a.b.def.genfieldindex=0 then gerror(".m3?") fi
		genpc_name(kpopdot, a.b.def)

	when jindex then
		do_bin(a.a, a.b, kpopix)

	when jdotindex then

		evalref(a.a)
		evalunit(a.b)
		genpc(kpopdotix)
	when jptr then
		evalunit(a.a)
		genpc(kpopptr)

	when jkeyindex then
		do_bin(a.a, a.b, kpopkeyix)

	when jmakelist then			!assign to multiple destinations
		n:=0
		p:=a.a
		while p do
			if n>=plist.len then gerror("Too many elems") fi
			plist[++n]:=p
			p:=p.nextunit
		od
		if n=0 then gerror("Empty lhs list") fi

		genpc_n(kexpand, n)
!		for i:=n downto 1 do
		for i:=1 to n do
			do_store(plist[i])
		od

	when jif then
		evalref(a)
		genpc(kpopptr)

	else
		gerror_s("Can't store to this unit yet:", jtagnames[a.tag], a)
	esac
end

function getconstvalue(unit p)int =
	if p and p.tag=jintconst then
		return p.value
	fi
	gerror("gcv Not const")
	return 0
end

proc do_convert(unit pconv)=
!apply type-conversion t on expression p

!also do constructors
	int n, elemmode, i, lowerx, lbound, m, mbase, nfields
	[maxunits]unit plist
	unit p

	m:=pconv.mode
	p:=pconv.a
	mbase:=ttbasetype[m]

!p.length is no. of elements, but it not used here(unitstoarray will count
!anyway). But a value of -1 (rather than 1) means a trailing comma was used.

	if p.tag<>jmakelist  OR MBASE=TREFPACK then		!assume regular type conversion
			if p.tag=jmakelist then
				deleteunit(p, p.a)
			fi
			evalunit(p)
			genpc_int(kconvert, m)
			return
!		fi
	fi

!a is a usertype
	n:=unitstoarray(p.a, &plist, maxunits)

	if n and plist[1].tag=jkeyvalue then
		case mbase
		when trecord, tstruct then
			do_makerecordkv(m, n, plist)
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
			checkelems(n, nfields, p)
		else				!allow 0 fields; use defaults of 0
			to nfields do
				genpc_int(kpushci, 0)
			od
			n:=nfields
		fi
		genpc_xy((mbase=trecord|kmakevrec|kmaketrec), n)
		pccurr.usertag:=m

	when tlist then		!probably just a list prefix used
		lowerx:=p.lower
		genpc_xy(kmakelist, n, lowerx)

	when tarray then
		genpc_xy(kmakeax, n, p.lower)
		pccurr.usertag:=tarray
		pccurr.usertag2:=p.elemtype

!	when tvector then
	when tvector then
		elemmode:=tttarget[m]
		lowerx:=ttlower[m]

		checkelems(n, ttlength[m], p)
		genpc_xy(kmakeax, n, lowerx)
		pccurr.usertag:=m
		pccurr.usertag2:=elemmode

	when tbits then
		if m=tbits then			!not user-defined
			genpc_xy(kmakebits, n, p.lower)
			pccurr.usertag:=tbits
			pccurr.usertag2:=(p.elemtype=tvoid|tu1|p.elemtype)
		else
			gerror("user-define bit array not ready")
		fi

	when tset then
		genpc_xy(kmakeset, n)

	else
		gerror_s("Convert list", strmode(mbase))
	esac
end

!proc do_case(unit p, pindex, pwhenthen, int res) =
proc checkelems(int n, length, unit p)=
	if n<length then
		gerror("Too few elements")
	elsif n>length then
		gerror("Too many elements")
	fi
end

proc do_switch(unit p, pindex, pwhenthen, int res) =
	int minlab, maxlab, x, y, i, n
	unit w, wt, pelse

	pelse:=pindex.nextunit
!first a first scan over the when expressions; work out range and whether simple or complex
	minlab:=1000000
	maxlab:=-1000000			!highest index seen

	n:=0				!no. different values
	wt:=pwhenthen

	while wt do
		w:=wt.a
		while w do
			case w.tag
			when jmakerange then
				x:=getconstvalue(w.a)
				y:=getconstvalue(w.b)
dorange:
				for i:=x to y do
					minlab :=min(minlab, i)
					maxlab :=max(maxlab, i)
				od
			when jintconst then
				x:=y:=w.value
				goto dorange
			when jtypeconst then
				x:=y:=w.mode
				goto dorange
			else
				gerror_s("Switch when2: not const", strexpr(w).strptr)
			esac
			w:=w.nextunit
		od
		wt:=wt.nextunit
	od

	if maxlab-minlab<=maxswitchrange then
		do_simpleswitch(p, pindex, pwhenthen, pelse, minlab, maxlab, res)
		return
	fi

	gerror("COMPLEX SWITCH/NOT COMPLETE")
end

proc do_simpleswitch(unit p, pindex, pwhenthen, pelse, int a, b, res) =
!a..b is the range of values of the switch which have been checked to
!be in range in terms of span. But the actual values can be anything.
!For example, 1000000 to 10000250 is valid. So, an offset needs to be
!used to bring the range down to 0 to 250

	unit w, wt, q
	int loopsw, n, offset, x, y, x0, i, labstmt, elselab
	[1..maxswitchrange+1]pcl labels
	int lab_a, lab_b, lab_c, lab_d

	loopsw:=p.tag=jdoswitch

	n:=b-a+1
	offset:=a-1		!a..b becomes 1..n

	if loopsw then
		lab_a:=definelabel()
		lab_d:=createfwdlabel()
		stacklooplabels(lab_a, lab_a, lab_d)
	else
		lab_d:=createfwdlabel()
	fi
	elselab:=createfwdlabel()

	evalunit(pindex)

	genpc_xy(kswitch, a, b)

	for i:=1 to n do
		genpc_lab(kjumplab, 0)
		labels[i]:=pccurr
	od

	genpc_lab(kjumplab, 0)			!else label
	labels[n+1]:=pccurr

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
				if labels[i].labelno then			!should have been zero
					println x, char(x)
					gerror("Dupl switch value")
				fi
				labels[i].labelno:=labstmt
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

!fill in zero entries with else
	definefwdlabel(elselab)
	if pelse then		!do else part
		evalunit(pelse, res)
	fi	

	if loopsw then
		genjumpl(lab_a)
		definefwdlabel(lab_d)
		unstacklooplabels()
	else
		definefwdlabel(lab_d)
	fi

	for i:=1 to n do
		if labels[i].labelno=0 then
			labels[i].labelno:=elselab
		fi
	od
	labels[n+1].labelno:=elselab
end

proc do_makerecordkv(int m, nkeyvals, []unit &kvlist)=
	unit p
	[maxunits]unit plist
	int nfields, index
	symbol d:=ttnamedef[m], e, f, k

	e:=d.deflist
	nfields:=0

	while e, e:=e.nextdef do
		if e.nameid in [fieldid, structfieldid] and e.atfield=nil then
			++nfields
			plist[nfields]:=nil
		fi
	od

	for i to nkeyvals do
		k:=kvlist[i].a.def
		p:=kvlist[i].b

		e:=d.deflist
		f:=nil
		while e, e:=e.nextdef do
			if e.nameid in [fieldid, structfieldid] and e.firstdupl=k then
				f:=e
				exit
			fi
		od

		if not f then
			gerror_s("Can't find field:", k.name)
		fi
		index:=f.index
		if plist[index] then
			gerror_s("Dupl key:", k.name)
		fi
		plist[index]:=p
	od

	for i to nfields do
		if plist[i] then
			evalunit(plist[i])
		else
			genpc_int(kpushci, 0)
		fi
	od

	genpc_xy(kmakevrec, nfields)
	pccurr.usertag:=m
end

proc do_idiv(unit a, b)=
	int n

	evalunit(a)
	if b.tag=jintconst and (n:=ispoweroftwo(b.value)) then
		genpc_int(kpushci, n)
		genpc(kshr)
	else
		evalunit(b)
		genpc(kidiv)
	fi
end

proc do_irem(unit a, b)=
	int n
	word m

	evalunit(a)
	if b.tag=jintconst and (n:=ispoweroftwo(b.value)) then
		m:=inot(0xFFFF'FFFF'FFFF'FFFF << n)
		genpc_int(kpushci, M)
		genpc(kiand)
	else
		evalunit(b)
		genpc(kirem)
	fi
end

proc do_map(unit p, popcode, x)=
	evalunit(x)
	if x.nextunit then
		evalunit(x.nextunit)
	fi
	evalunit(popcode)
	genpc(kmap)

	int lab:=createfwdlabel()
	genpc_lab(kjump, lab)		!dummy jump to be moved to runtime-generated code
	genpc(knop)					!stop jump being optimised out
	definefwdlabel(lab)
end

proc pushstring(ichar s)=
	genpc(kpushcs)
	genopnd_strz(s)
end

function checkblockreturn(unit p)int=
!p should be a block unit
!check that the last statement is a return; return 1/0 for return/not return
!just allow or check for return/if/longif for now
	ref unitrec q, r

	if p=nil then return 0 fi
!	if p.tag<>jblock then gerror("CBR?") fi
!
!	q:=p.a
!	if q=nil then return 0 fi		!empty block

!	while r:=q.nextunit do			!get q=last stmt in block
!		q:=r
!	od

	case jhasvalue[p.tag]
	when 0 then return 0
	when 1 then return 1				!assume simple value
	esac								!else 2

!assume complex unit

	case p.tag
	when jblock then
		q:=p.a
		if q=nil then return 0 fi		!empty block
		while r:=q.nextunit do			!get q=last stmt in block
			q:=r
		od
		return checkblockreturn(q)

!	when jreturn then			!that's an easy one...
!		return 1

	when jif then
		return checkblockreturn(p.b) and checkblockreturn(p.b.nextunit)		!all branches must have a return

	else								!assume yes
		return 1


	esac
	return 0
end

=== qq_pcllib.m 0 0 19/48 ===
const pclinitalloc=128

global pcl pcstart				!point to start of current pcl block
global pcl pccurr				!point to last create pcl rec
global pcl pcend				!point to last allocated int (with enough margin for on extra instr)
global int pcalloc				!ints allocated

global ref i32 pcsourcestart
global ref i32 pcsourcecurr

global int pclcurrlineno			!current line number
const pclelemsize=pclrec.bytes
const pcsrcelemsize=i32.bytes

global const labelinitalloc=8192
global ref[]pcl labelpctable		!labelpctable[L] refers to target instr of label L
global int labelalloc
global int nextlabelno

!global [0..pclnames.upb]byte pclnopnds

proc start=
	int nn

	pcm_init()

!label/block tables are not needed after the pcl sequence has been
!generated. But they are not freed; they can be reused, with their
!current sizes, for the next module. (Might be inefficient if there is one
!very large module, then mainly small ones.)

	labelalloc:=labelinitalloc
	labelpctable:=pcm_alloc(pcl.bytes*labelalloc)
end

global proc resetpcl(int sourcesize)=
	int pclsize

	qpos:=0
	nextlabelno:=0
	pclcurrlineno:=0

!pcl dest is reallocated for each module
!Any current pcl data is presumably retained so that it can be run.

	pclsize:=sourcesize			!estimated num of pcl bytecode elements

	pcalloc:=1024					!min
	while pcalloc<pclsize do
		pcalloc<<:=1
	od

	pcstart:=pcm_allocz(pcalloc*pclelemsize)
	pccurr:=pcstart-1
	pcend:=pcstart+pcalloc-8			!allow margin

	pcsourcestart:=pcm_alloc(pcalloc*pcsrcelemsize)
	pcsourcecurr:=pcsourcestart

	pcm_clearmem(labelpctable, pcl.bytes*labelalloc)

end

global proc genpc(int opc)=

	++pccurr

	if pccurr>=pcend then
		extendpcldata()
	fi

!only do overflow check at start of an instruction
	pccurr.opcode:=opc
!IF LABELFLAG THEN CPL "GENPC", PCLNAMES[OPC], =LABELFLAG FI

	++pcsourcecurr
	pcsourcecurr^:=qpos

end

!global proc genopnd_int(i64 x)=
!!no pcindex overflow check needed, as the genpc() check will be sufficient as
!!it would allow for enough operands
!	pccurr.value:=x
!end
!
!global proc genopnd_name(ref strec d)=
!	pccurr.def:=d
!end

global proc genpc_int(int opc, i64 a)=
	genpc(opc)
	pccurr.value:=a
end

global proc genpc_n(int opc, n)=
	genpc(opc)
	pccurr.n:=n
end

global proc genpc_xy(int opc, x, y=0)=
	genpc(opc)
	pccurr.x:=x
	pccurr.y:=y
end

global proc genpc_name(int opc, ref strec d)=
	genpc(opc)
	pccurr.def:=d
end

global proc genopnd_strz(ichar s)=
!s must be a heap string, be a constant, or otherwise be persistent
	pccurr.svalue:=s
end

global proc genopnd_str(object s)=
!s must be a heap string, be a constant, or otherwise be persistent
	pccurr.objptr:=s
end

global proc genopnd_obj(object p)=
	pccurr.objptr:=p
end

global proc genpc_real(int opc, real x)=
	genpc(opc)
	pccurr.xvalue:=x
end

global proc genpc_lab(int opc, int lab)=
	genpc(opc)
	pccurr.labelno:=lab
end

!global proc genopnd_lab(int a)=
!	pccurr.labelno:=a
!end

global proc gencomment(ichar s)=
	genpc(kcomment)
	genopnd_strz(pcm_copyheapstring(s))
end

proc extendpcldata=
	int newpcalloc
	pcl newpcstart
	ref i32 newpcsourcestart

	newpcalloc:=pcalloc*2

!CPL "EXTENDING PCL TABLE TO",=PCLSTART

	newpcstart:=pcm_alloc(pclelemsize*newpcalloc)
	newpcsourcestart:=pcm_alloc(pcsrcelemsize*newpcalloc)

	memcpy(newpcstart,pcstart, getpcloffset(pccurr,pcstart)*pclelemsize)
	memcpy(newpcsourcestart,pcsourcestart, getpcloffset(pccurr,pcstart)*pcsrcelemsize)

	pccurr:=newpcstart+getpcloffset(pccurr,pcstart)
	pcend:=newpcstart+newpcalloc-10
	pcsourcecurr:=newpcsourcestart+(pcsourcecurr-pcsourcestart)

	pcm_free(pcstart,pcalloc*pclelemsize)
	pcm_free(pcsourcestart,pcalloc*pcsrcelemsize)

	pcstart:=newpcstart
	pcalloc:=newpcalloc
	pcsourcestart:=newpcsourcestart
end

global proc extendlabeltable=
	int newlabelalloc
	ref[]pcl newlabeltable

	newlabelalloc:=labelalloc*2

	newlabeltable:=pcm_alloc(pcl.bytes*newlabelalloc)

	memcpy(newlabeltable,labelpctable, labelalloc*pcl.bytes)

	pcm_free(labelpctable,labelalloc*pcl.bytes)

	labelpctable:=newlabeltable
	labelalloc:=newlabelalloc
end

global function definelabel:int=
	if nextlabelno>=labelalloc then extendlabeltable() fi
	++nextlabelno
!CPL "DEF LABEL",NEXTLABELNO
	labelpctable[nextlabelno]:=pccurr+1
	return nextlabelno
end

global function createfwdlabel:int=
	if nextlabelno>=labelalloc then extendlabeltable() fi
	++nextlabelno
!CPL "FWD LABEL",NEXTLABELNO
	labelpctable[nextlabelno]:=nil
	return nextlabelno
end

global proc definefwdlabel(int lab)=
	if labelpctable[lab] then serror("dupl label?") fi

!IF PCCURR.OPCODE=KJUMP AND PCCURR.LABELNO=LAB THEN
!CPL "JUMP NEXT???"
!FI
!
	labelpctable[lab]:=pccurr+1
end

global proc genxy(int x, y=0)=
	pccurr.x:=x
	pccurr.y:=y
end

!GLOBAL PROC SHOWLABS(ICHAR CAPTION)=
!	PRINT "    ",CAPTION,,": ("
!	FOR I TO NEXTLABELNO DO
!		CP LABELPCTABLE[I],$
!	OD
!	CPL ")",NEXTLABELNO
!END
!
=== qq_print.m 0 0 20/48 ===
!Vars for i/o
!Makes use of stdio/fileio/strio/windio as used by Q system
global  int mindev		!one of stdio/fileio/strio/windio
global  int moutdev
global  ref int minchan		!actual file handles
global  filehandle moutchan
global  varrec minvar		!strio: vars to be used as source or dest
global  varrec moutvar		!str: used for sprint(=string) and @&.string (=refvar)

!I/O Constants: print/read i/o channels
global const std_io	= 0		!console i/o
global const file_io	= 1		!uses file channel inchan or outchan
global const str_io	= 2		!uses string instr^ or outstr^
global const wind_io	= 3		!uses window inwind^ or outwind^
global const istr_io	= 4		!used by pcx interpreter

const maxoclevel=6
[0:maxoclevel]i32			moutdevstack
[0:maxoclevel]filehandle	moutchanstack
[0:maxoclevel]varrec		moutvarstack
[0:maxoclevel]byte			mgapstack
[0:maxoclevel]ref char		mfmtstrstack
[0:maxoclevel]ref char		mfmtcurrstack
int noclevels

const maxstrlen=256
const comma=','
const onesixty=1024

global  ichar mfmtstr		!used for format string is nil (no fmt string) or points to fmt string
global  ichar mfmtcurr	!point to next char to use in fmtstr
!global  fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,(0,0))
global  fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,0, 0)
byte mgapneeded

!const minkb_size=16384		! start size of kb buffer
const minkb_size=1048576		! start size of kb buffer
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

![0:]char digits=a"0123456789ABCDEF"

global proc pch_print(variant p, fmt=nil)=
	varrec v
	variant q
!	object a
	ref char s
	varrec emptyfmt

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
	pch_tostr(p,fmt,&v)
	printstr_n(v.objptr.strptr,v.objptr.length)

	var_unshare(&v)
end

global proc pch_print_nf(variant p)=
	pch_print(p, nil)
end

global proc pch_printnogap=
	mgapneeded:=0
end

global proc pch_println=
	if mfmtstr then
		printnextfmtchars(1)
	fi
	mgapneeded:=0
	printstr_n("\n",-1)
end

global proc pch_reread=
	kb_pos:=kb_lastpos
	kb_length:=kb_lastlength
end

global proc pch_rereadln=
	kb_pos:=kb_start
	kb_length:=kb_linelength
end

global proc pch_startprint(variant p)=
	object s

	case ++noclevels
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
	end

	mfmtstr:=nil
	mfmtcurr:=nil

	if p=nil then
		goto doconsole
	fi
	case p.tag
	when tint then
		switch p.value
		when 0 then
	doconsole:
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
		end

	when trefvar then
		p:=p.varptr
		case p.tag
		when tstring then
			moutdev:=istr_io
			moutchan:=nil
			moutvar.tagx:=trefvar
			moutvar.varptr:=p
		
		else
			PRINTLN ttname[p.tag]
			prterror("Print@^?")
		end

	else
		case p.tag
		when trecord, tstruct then		! check for specific records
			moutdev:=std_io
		else
			PRINTLN ttname[p.tag]
			prterror("Can't do startprint...")
		end
	end

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
	case moutdev
	when istr_io then
		p:=moutvar.varptr
	end

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

global proc pch_strstartprint* =
	varrec p

	p.tagx:=tint
	p.value:=1
	pch_startprint(&p)		! do equivalent of @1
end

global proc pch_strendprint(variant dest) =
	if mfmtstr then
		printnextfmtchars(1)
	fi
	if moutdev<>str_io then
		prterror("STRENDPRT/NOT STR")
	fi

	dest^:=moutvar						!transfer ownership
	moutvar.tagx:=tvoid

	pch_endprint()
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

	case dev.tag
	when tvoid then
doconsole:
		readlinen(nil,kb_start,kb_size)	! reads as zero-terminated
		kb_length:=strlen(kb_start)

	when tint then
		case dev.value
		when 0 then
			goto doconsole
		when 1 then
			if testfilech=nil then
				prterror("R@2: file not open")
			fi
			ch:=cast(testfilech)

		else
			ch:=filehandle(dev.value)
		end
!		pc_readlinen(cast(ch),kb_start,kb_size)			! reads as zero-terminated
		readlinen(cast(ch),kb_start,kb_size)			! reads as zero-terminated
		kb_length:=strlen(kb_start)

	when tstring then
		pdev:=dev.objptr
		length:=pdev.length
		if length=0 then
			kb_length:=0
			kb_start^:=0
		elsif length>=kb_size then
			prterror("KB overflow")
		else
			kb_length:=length
			memcpy(kb_start,pdev.strptr,length)
		fi
	else
		pcustype("readln@",dev)
	end

	kb_pos:=kb_start
	kb_lastpos:=kb_pos
	kb_linelength:=kb_length
end

global proc pch_sread(variant fmt,variant dest) =
	int fmtcode
	char c

!pc_cfree(dest)
	fmtcode:=getreadfmtcode(fmt)
	kb_lastpos:=kb_pos
	kb_lastlength:=kb_length

	case fmtcode
	when 'I' then
		stepkbpos(readint(kb_pos,kb_length,dest,0))

	when 'R' then
		stepkbpos(readreal(kb_pos,kb_length,dest))

	when 'N' then
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
			var_empty_string(dest)
		else
			var_make_stringn(kb_pos,kb_length,dest,1)
			kb_pos+:=kb_length
			kb_length:=0
		fi

	when 'C' then
		if kb_length=0 then
			var_empty_string(dest)
		else
			termchar:=kb_pos^
	dochar:
			dest.tagx:=tint
			dest.value:=termchar
			++kb_pos
			--kb_length
		fi

	when 'Z' then			! last terminator!
		dest.tagx:=tint
		dest.value:=termchar

	when 'E' then
		dest.tagx:=tint
		dest.value:=itemerror
	when 'D' then
		stepkbpos(readint(kb_pos,kb_length,dest,1))

	else
		prterror("SREAD/FMT?")
	end
end

global proc pch_sreadln(variant dev, variant dest) =
	pch_readln(dev)
	var_make_stringn(kb_start,kb_length,dest,mutable:1)
end

function readname(ref char s,int length,variant dest)ref char =
	ref char send
	ref char itemstr
	int itemlength
	send:=readitem(s,length,itemstr,itemlength)
	var_make_stringn(itemstr,itemlength,dest,1)

	iconvlcn(dest.objptr.strptr,dest.objptr.length)
	return send
end

function readstring(ref char s,int length,variant dest)ref char =
	ref char send
	ref char itemstr
	int itemlength
	send:=readitem(s,length,itemstr,itemlength)
	var_make_stringn(itemstr,itemlength,dest,1)
	return send
end

function readint(ref char sold,int length,variant dest, int dodec)ref char =
!return point to next char after terminator (which can be just off length of string)
	ref char p,s				! s points to ^str
	ref char send
	ref char itemstr
	int itemlength,numlength

	send:=readitem(sold,length,s,itemlength)

	strtoint(s,itemlength,dest,dodec)

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
	case t
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
		prterror("Readhex/long")
	end

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
	case t
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
	end

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

global function getreadfmtcode(variant p)int =
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
	if p.objptr.length=0 then
!	return 'I'
		return 'A'
	fi

	c:=toupper(p.objptr.strptr^)

	case c
	when 'I', 'R', 'N', 'S', 'F', 'T', 'Z', 'C', 'L', 'H','B','A','E','D' then
		return c
	end

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
		var_make_stringn(s,itemlength,dest,1)
	elsif expon then
		strtoreal(s,itemlength,dest)
	else
		strtoint(s,itemlength,dest,0)
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
		case c
		when ' ', 9, comma, '=', ';' then		! separator
			if quotechar or p=s then			!can be considered part of name if inside quotes, or is only char
				goto normalchar
			fi
			termchar:=c
			exit
		else
	normalchar:
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
		end
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
	i32 numlength

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

proc strtoint(ichar s,int length, variant dest, int dodec)=
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

!classify magnitude of value as:
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
		end
	elsif length=20 then
		if cmpstring(s,"18446744073709551615")<=0 then
			cat:='C'
		else
			cat:='D'
		fi
	else
		cat:='D'
	fi

	if dodec then cat:='D' fi

!now look at sign:
	if signd then
		case cat
		when 'B' then cat:='A'		!-922...808 can be i64
		when 'C' then cat:='D'		!needs longint
		end
	fi

!convert cat to type

	case cat
	when 'A' then t:=tint
!	when 'B','C' then t:=tword
	else t:=tdecimal
	end

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
		var_make_dec_str(s,length,dest)
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
		case c
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
skip:
			++n
			++mfmtcurr
		end
	od
end

global proc pch_setformat(variant p) =
	int n
	ref char s

	if p.tag<>tstring then
		prterror("(str)")
	fi
	if mfmtstr then
		prterror("Setfmt?")
	fi
	n:=p.objptr.length
	mfmtstr:=pcm_alloc(n+1)
	if n then
		memcpy(mfmtstr,p.objptr.strptr,n)
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
	if p.objptr.strptr=nil then
		return &defaultfmt
	else
		strtofmt(p.objptr.strptr,p.objptr.length,fmt)
		return fmt
	fi
fi
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

	oldlen:=p.length

	if p.refcount=0 then	! assume a fixed buffer
		if oldlen=0 then		! first string
			memcpy(p.strptr,t,n)
			p.length:=n
		else				! append to existing string
			memcpy(p.strptr+oldlen,t,n)
			p.length:=oldlen+n
		fi
		return
	fi

	if oldlen=0 then		! first or only string
		p.strptr:=pcm_alloc(n)
		p.length:=n
		p.alloc64:=allocbytes
		memcpy(p.strptr,t,n)

	else				! append to existing string
		newlen:=oldlen+n
		oldbytes:=p.alloc64
		newbytes:=oldlen+n
		if newbytes<=oldbytes then 		! fits in current allocation
			memcpy(p.strptr+oldlen,t,n)
		else					! need new allocation
			newptr:=pcm_alloc(newbytes)
			memcpy(newptr,p.strptr,oldlen)	! existing chars
			memcpy(newptr+oldlen,t,n)		! add new chars
			p.alloc64:=allocbytes
			pcm_free(p.strptr,oldbytes)
			p.strptr:=newptr
		fi
		p.length:=newlen
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
!n is:
! -1:	s is zero-terminated; calculate length
! 0:	s is empty string (no output)
! >0:	n is length of string
	variant  p
	int x
	type fntype= ref function (filehandle f, ichar s, int i, ichar t)int

	if n=-1 then		! was stringz
		n:=strlen(s)
	fi

	if n=0 then
		return
	fi

	case moutdev
	when std_io then
		printstrn_app(s,n,nil)

	when file_io then
		printstrn_app(s,n,cast(moutchan))

	when str_io then
		addstring(moutvar.objptr,s,n)

	when istr_io then
		p:=moutvar.varptr
!CPL "PRINTSTR/STR",=MOUTVAR.OBJPTR
		if p.tag<>tstring then
			prterror("prtstrn1")
		fi
		addstring(moutvar.objptr,s,n)

	when wind_io then
		
	end
end

global proc pch_strtoval(variant p,variant fmt,variant dest) =
!p should be a string, fmt is nil, or contains a string format code for read
!convert string to value, then store in dest
	int fmtcode,length
	byte oldmutable
	object q
	[1024]char str
	ref char s:=&.str

	q:=p.objptr

	if q.length<str.len then
		memcpy(s,q.strptr,q.length)
		str[q.length+1]:=0
	else
		pcerror("STRTOVAL/string too long")
	fi

	fmtcode:=getreadfmtcode(fmt)
	if p.tag<>tstring then
		prterror("strval")
	fi
	length:=p.objptr.length

	case fmtcode
	when 'I' then
		readint(s,length,dest,0)
	when 'D' then
		readint(s,length,dest,1)
	when 'R' then
		readreal(s,length,dest)
	when 'N' then
		readname(s,length,dest)
	when 'S' then
		readstring(s,length,dest)
	when 'H' then
		readhex(s,length,dest)
	when 'B' then
		readbin(s,length,dest)
	when 'A' then
		readany(s,length,dest)
!
	else
		prterror("strval:fmt?")
	end
end

proc tostr_int(variant p,ref fmtrec fmt,object dest) =
	[0:onesixty]char str

	case fmt.charmode
	when 'M','D' then
!CPL "DOMUL"
		domultichar(ref char(&p.value),8,str,fmt)

	when 'C' then

		str[0]:=p.value
		str[1]:=0

	else
!		i64tostrfmt(p.value,str,fmt,0)
		i64tostrfmt(p.value,str,fmt)
	end

	if fmt.showtype then
		addstring(dest,"I:",2)
	fi

	addstring(dest,str,strlen(str))
end

proc tostr_real(variant p, ref fmtrec fmt, object dest) =
	[0:onesixty]char str,str2
	[10]char cfmt
	int n

	if fmt.precision then
		cfmt[1]:='.'
		cfmt[2]:='*'
		cfmt[3]:=fmt.realfmt
		cfmt[4]:=0
		print @str,fmt.precision:"v", p.xvalue:cfmt

	else
		print @str,p.xvalue:"fmt"
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
	oldlen:=q.length
	newlen:=oldlen

	if fmt.quotechar or fmt.minwidth>newlen then
		if fmt.quotechar then
			newlen+:=2
		fi
		if fmt.minwidth>newlen then
			newlen:=fmt.minwidth
		fi
		s:=pcm_alloc(newlen+1)
		strtostrfmt(q.strptr,s,oldlen,fmt)
		addstring(dest,s,newlen)
		pcm_free(s,newlen+1)
	else
		addstring(dest,q.strptr,oldlen)
	fi
end

global proc pch_tostr(variant a, b, result)=
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

proc tostr_range(variant p, ref fmtrec fmt, object dest) =
	[0:onesixty]char str

!	i64tostrfmt(p.range_lower,str,fmt,0)
	i64tostrfmt(p.range_lower,str,fmt)
	strcat(str,"..")
	addstring(dest,str)
!	i64tostrfmt(p.range_upper,str,fmt,0)
	i64tostrfmt(p.range_upper,str,fmt)
	addstring(dest,str)
end

proc tostr_array(variant p, ref fmtrec fmt, object dest) =
	[0:onesixty]char str
	ref byte q
	int i,m,elemtype,a,b,lower, length
	varrec v
	object pa
	ref byte ptr

	m:=p.tag
	pa:=p.objptr

	if p.tag=tarray then
		length:=pa.length
		lower:=pa.lower
		elemtype:=pa.elemtag
	else
		length:=ttlength[pa.usertag]
		lower:=ttlower[pa.usertag]
		elemtype:=tttarget[pa.usertag]
	fi

	a:=lower
	b:=length+lower-1

	q:=pa.ptr

	if fmt.showtype then
		fprint @str,"#[#:#]A",ttname[m],lower,ttname[elemtype]
		addstring(dest,str)
	fi
	addstring(dest,"(")

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

	a:=pa.lower16
	elemtype:=pa.elemtag
	b:=pa.length+a-1
	bitwidthx:=ttbitwidth[elemtype]
	offset:=pa.indexoffset*bitwidthx

	q:=pa.ptr

	if fmt.showtype then
		fprint @str,"#[#:#]A",ttname[m],pa.lower16,ttname[elemtype]
		addstring(dest,str)
	fi
	addstring(dest,"(")

	for i:=a to b do
		var_loadbit(q,offset,elemtype,0,&v)
		offset+:=bitwidthx
		if offset>=8 then
			offset:=0
			++q
		fi
		tostr(&v,fmt,dest)
		if i<b then
			addstring(dest,",",1)
		fi
	od
	addstring(dest,")",1)
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

	pa:=p.objptr
	m:=pa.usertag

	d:=ttnamedef[m]

	r:=d.topfieldlist
	nfields:=ttlength[m]

	needcomma:=0
	addstring(dest,"(")

	for i to nfields do
		var_loadpacked(pa.ptr+r.fieldoffset, r.mode, &v, nil)
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
	while (i<s.length) do
		if testelem(cast(s.ptr),i) then	! element i included
			j:=i+1				! now search for end of this '1' block
			while (j<s.length and testelem(cast(s.ptr),j)) do
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

proc tostr_dict(variant p,ref fmtrec fmt,object dest) =
	[0:onesixty]char str
	variant q
	int i,length,needcomma:=0
	object pa

	if fmt=nil then
		fmt:=&defaultfmt
	fi
	addstring(dest,"[",-1)

	pa:=p.objptr
	q:=pa.varptr		!keys/value pairs

	length:=pa.length/2				!number of pairs

	for i:=length downto 1 do
		if q.tag=tvoid then
			q+:=2
			nextloop
		fi
		if needcomma then
			addstring(dest,",",1)
		fi
		needcomma:=1
		tostr(q,fmt,dest)
		q++
		addstring(dest,":",1)
		tostr(q,fmt,dest)
		q++
	od
	addstring(dest,"]",1)
end

proc tostr_decimal(variant p,ref fmtrec fmt,object dest) =
	ref char s

	s:=var_tostr_dec(p,0)
	addstring(dest,s,-1)
	pcm_free(s,decstrsize)
end

proc tostr(variant p, ref fmtrec fmt, object dest) =
	[1024]char str


!CPL "TOSTR",=STRMODE(P.TAG),=P.TAG

	case p.tag
	when tint then
		tostr_int(p, fmt, dest)

	when treal then
		tostr_real(p, fmt, dest)

	when tstring then
		tostr_str(p, fmt, dest)

	when trange then
		tostr_range(p, fmt, dest)

	when tlist, trecord then
		tostr_list(p, fmt, dest)

	when tarray,tvector then
		tostr_array(p, fmt, dest)

	when tbits then
		tostr_bits(p, fmt, dest)

	when tset then
		tostr_set(p, fmt, dest)

	when tstruct then
		tostr_struct(p, fmt, dest)

	when tdecimal then
		tostr_decimal(p, fmt, dest)

	when tdict then
		tostr_dict(p, fmt, dest)

	when tvoid then
		addstring(dest,"<Void>")

	when trefvar then
		if fmt.showtype then
			fprint @str,"#<#>:",ttname[p.tag],(p.varptr|ttname[p.varptr.tag]|"")
			addstring(dest,str)
		fi
showptr:
		if p.varptr=nil then
			addstring(dest,"nil")
		else
			addstring(dest,strint(cast(p.varptr),"H"))
		fi

	when trefpack then
		if fmt.showtype then
			fprint @str,"#<#>:",ttname[p.tag],(p.varptr|ttname[p.elemtag]|"")
			addstring(dest,str)
		fi
		showptr

	when trefbit then
		if fmt.showtype then
			fprint @str,"#<#>(#,#):",ttname[p.tag],(p.varptr|ttname[p.elemtag]|""),
				p.bitoffset,p.bitlength
			addstring(dest,str)
		fi
		showptr

	when tsymbol then
		if p.def then
			fprint @str,"<#:""#"">",namenames[p.def.nameid],p.def.name
			addstring(dest,str)
		else
			addstring(dest,"<nil>")
		fi
	when ttype then
		fprint @str,"#",ttname[p.value]!+(p.value<=tlast|1|0)
		addstring(dest,str)
	when toperator then
		fprint @str,"(#)", pclnames[p.value]+1
		addstring(dest,str)

!	when tenum then
!		addstring(dest,getenumname(p.elemtag, p.value))

	else
		pcustype("Tostr:",p)
	end
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

	r.refcount:=-r.refcount
	q:=r.varptr

	if p.tag=tlist then
		n:=p.objptr.length
	else
		n:=ttlength[r.usertag]
	fi

	if fmt.newline then
		to n do
			tostr(q,fmt,dest)
			addstring(dest,"\n",-1)
			++q
		od

	else
		addstring(dest,"(",1)
		for i:=n downto 1 do
			tostr(q,fmt,dest)
			++q
			if i<>1 then
				addstring(dest,",",1)
			fi
		od
		addstring(dest,")",1)
	fi
	r.refcount:=-r.refcount
	--listdepth
end
=== qq_records.m 0 0 21/48 ===
global proc var_make_record(variant a, dest, int n, rectype) =
!create a list of n vars starting from a in reverse order (a is the last)
!put the result in dest (note this will be the last the n vars)
	object p
	variant b
	int m

	p:=obj_new_record(rectype,nil)

	b:=p.varptr

	m:=ttlength[rectype]

	if n<m then
		pcerror("Too few elements")
	elsif n>m then
		println =n,=m
		pcerror("Too many elements")
	fi

	to n do
		b^:=a^				!assume list initialised to void
		++a
		++b
	od

	dest.tagx:=trecord ior hasrefmask
	p.usertag:=rectype

!CPL "MAKEREC",TTNAME[RECTYPE],TTNAME[TTBASETYPE[RECTYPE]]

	dest.objptr:=p
end

global function obj_new_record(int m, variant defval)object p=
	variant a
	int n

	p:=obj_new()
	p.mutable:=1
!	p.lower32:=1
	n:=ttlength[m]
!	p.length:=n
	p.objtype:=normal_obj

	if n then
		p.varptr:=a:=pcm_alloc(n*varrec.bytes)

!CPL "NEWREC",TTNAME[M],=DEFVAL
!IF DEFVAL THEN CPL =TTNAME[DEFVAL.TAG],=DEFVAL.VALUE FI

		if defval and defval.tag<>tvoid then
			a:=p.varptr
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

	q:=p.varptr
	to p.length do
		var_unshare(q)
		++q
	od
	if p.length then
		pcm_free(p.varptr,p.length*varrec.bytes)
	fi

	pcm_free32(p)
end

global proc var_dupl_record(variant a)=
	object p,q
	variant plist, qlist
	int length

	p:=a.objptr
	q:=obj_new()
	q^:=p^
	q.refcount:=1
	q.mutable:=1

	a.objptr:=q
	length:=ttlength[p.usertag]

	if length=0 then return fi

	qlist:=q.varptr:=pcm_alloc(length*varrec.bytes)
	plist:=p.varptr

	to length do
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


	px:=x.objptr
	py:=y.objptr
	if px.usertag<>py.usertag then return 0 fi

	if px=py then
		return 1
	fi

	a:=px.varptr
	b:=py.varptr

	to ttlength[px.usertag] do
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
	if offset>=word(ttlength[q.usertag]) then
		pcerror("record[int] bounds")
	fi

	a^:=(q.varptr+offset)^
	var_share(a)
end

global proc var_putix_record(variant a, int index, variant x)=
	variant dest
	object q
	word offset

	q:=a.objptr

	if not q.mutable then pcnotmut() fi

	offset:=index-1
	if offset>=word(ttlength[q.usertag]) then
		pcerror("rec[int] bounds")
	fi

	dest:=q.varptr+offset
	var_unshare(dest)
	dest^:=x^				!xfer ownership	
!	var_share(dest)
end

global proc var_getixref_record(variant a, int index, variant dest)=
	variant p
	object q
	word offset

	q:=a.objptr

	offset:=index-1
	if offset>=word(q.length) then
		pcerror("^rec[int] bounds")
	fi

	p:=q.varptr+offset

	dest.tagx:=trefvar
	dest.varptr:=p
end

=== qq_resolve.m 0 0 22/48 ===
int nprocs

int noexpand
int symbolmode
int macrolevels
int allowmodname

const maxmacroparams=50
[maxmacroparams]symbol macroparams
[maxmacroparams]symbol macroparamsgen
[maxmacroparams]unit macroargs
int nmacroparams
int nmacroargs

const maxstructfields=100
[maxstructfields]symbol structfields
int ntopfields, nallfields

global proc rx_module(ifile pm)=
	currmodule:=pm
	stcurrproc:=stcurrmodule:=currmodule.def
	nprocs:=0

!move this to end of proc to allow module vars generated by assignment
!to be visible inside procs

	rx_passdef(stprogram, stcurrmodule)

	if nprocs=0 then
		rx_unit(stcurrmodule,currmodule.ast)
!	elsif currmodule.ast.a then				!module block not empty
	elsif currmodule.ast then
		RX_UNIT(STCURRMODULE,CURRMODULE.AST)
	fi
end

global proc rx_passdef(symbol owner,p)=
	symbol d

	case p.nameid
!	when moduleid,dllmoduleid then
	when moduleid then
		rx_deflist(p,p.deflist)

	when procid, anonprocid then
		++nprocs
		fixmode(owner,p)
		rx_deflist(p,p.deflist, 0)
		stcurrproc:=p
		rx_unit(p,p.code)
		stcurrproc:=stcurrmodule
		rx_deflist(p,p.deflist, 1)

	when dllprocid then
		fixmode(owner,p)
		rx_deflist(p,p.deflist)

	when constid,staticid,frameid,paramid then
		fixmode(owner,p)
		if p.code then
			rx_unit(owner,p.code)
		fi
	when typeid,recordid then
		fixmode(owner,p)
		rx_deflist(p,p.deflist)

	esac
end

global proc rx_deflist(symbol owner, p, int doanon=0)=
!doanon=0: do all names except anonproc
!doanon=1: do only anonproc
!

	while p do
		if doanon and p.nameid=anonprocid or doanon=0 and p.nameid<>anonprocid then
			rx_passdef(owner,p)
		fi
		p:=p.nextdef
	od
end

global proc rx_unit(symbol owner, unit p)=
	symbol d
	unit a,b
	int n, flags, oldnoexpand,oldsymbolmode, nk

	a:=p.a
	b:=p.b
	qpos:=p.pos

	case p.tag
	when jname then
		resolvename(owner,p)
		if p.tag=jname and p.def.nameid=macroid and not noexpand then
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
		if a.tag=jname then			!can expand possible macro if params not ready
			oldnoexpand:=noexpand; noexpand:=1
			rx_unit(owner,a)
			noexpand:=oldnoexpand
		else
			rx_unit(owner,a)
		fi

		rx_unitlist(owner,b)

		if a.tag=jtypeconst then
			p.tag:=jconvert
			p.a:=b
			p.b:=nil
			p.mode:=a.mode

!CPL "CALL TO CONVERT"

!			if ttbasetype[a.mode]=tenum then
!			else
				nk:=0
				p.a:=createunit1(jmakelist,b)
				n:=0
				while b do
					if b.tag=jkeyword then
						++nk
						b.tag:=jkeyvalue
					fi
					++n
					b:=b.nextunit
				od
				if nk and nk<>n then
					rxerror("Mixed key:value")
				fi
				if a.nextunit then n:=-n fi
				p.a.length:=n
!			fi
		elsif a.tag=jname and a.def.nameid=macroid then
			++macrolevels
			expandmacro(p,a,b)
			rx_unit(owner,p)
			--macrolevels
		fi

	when jbin, jmakerange then
		rx_unit(owner,a)
		if not b then rxerror("Binop missing opnd") fi
		rx_unit(owner,b)
		evalbinop(p,a,b)

	when junary, jproperty then
		rx_unit(owner,a)
		evalmonop(p)

	when jfor then			!a will be jname unit
		resolvename(owner,a,tint)
		a:=a.nextunit
		goto doabc

	when jconvert then
		rx_unit(owner,a)

		evalmonop(p)

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

	when jstrinclude then
!PCERROR("STRINCLUDE")
		rx_unit(owner,a)
		if a.tag<>jstringconst then
			rxerror("Not strconst")
		fi

		ifile pm
		pm:=loadsourcefile(a.svalue,0)

!		n:=getsupportfile(a.svalue,
!			path:sourcefilepaths[modules[p.moduleno].fileno],
!			issupport:1)
		a.svalue:=pm.text
		a.slength:=pm.size-1

		deleteunit(p,a)

	else
doabc:

		flags:=jflags[p.tag]
		if flags>=1 then rx_unitlist(owner,a) fi
		if flags=2 then rx_unitlist(owner,b) fi
	esac
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

	case p.tag
!	when jbytesize then
!		if p.a.tag=jtypeconst then
!			c:=ttsize[p.a.mode]
!			newint
!		fi

	elsecase p.a.tag
	when jintconst then
		a:=p.a.value

		case p.pclop
		when kneg then c:=-a
		when kabs then c:=abs(a)
		else
			return
		esac

newint:
		makeintconst(p,c)

	when jrealconst then
		x:=p.a.xvalue

		case p.pclop
		when kneg then z:=-x
		when kabs then z:=abs(x)
		else
			return
		esac

		makerealconst(p,z)
	else
		return 
	esac
end

proc evalbinop(unit p,lhs,rhs)=
	int a,b,c
	real x,y,z

!CPL "EVALBIN", JTAGNAMES[P.TAG],pclnames[p.pclop]

	case pr(lhs.tag,rhs.tag)
	when pr(jintconst, jintconst) then
		a:=lhs.value
		b:=rhs.value

		case p.pclop
		when kadd then c:=a+b
		when ksub then c:=a-b
		when kmul then c:=a*b
		when kidiv then
			if b=0 then rxerror("x/0") fi
			c:=a/b
		when kpower then c:=a**b
		else
			return
		esac

		makeintconst(p,c)

	when pr(jrealconst, jrealconst) then
		x:=lhs.xvalue
		y:=rhs.xvalue

		case p.pclop
		when kadd then z:=x+y
		when ksub then z:=x-y
		when kmul then z:=x*y
		when kdiv then z:=x/y
		else
			return
		esac

		makerealconst(p,z)
	else
		return 
	esac
end

proc makeintconst(ref unitrec p,i64 value)=
!convert unit p, currently binop or monop, to a const
	p.tag:=jintconst
	p.a:=p.b:=nil
	p.value:=value
	p.mode:=tint
end

proc makerealconst(ref unitrec p,r64 xvalue)=
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

	symbol d,e,f
	unit q
	int moduleno, n

	d:=p.def
	moduleno:=p.moduleno

	if d.nameid<>genericid then			!assume already resolved
		return
	fi

	e:=resolvetopname(owner,d,moduleno,allowmodname)

	if not e then
		case owner.nameid
		when procid, anonprocid then			!add as framevar
			e:=p.def:=addsymbol(owner,d,frameid,0)
		when moduleid then
			e:=p.def:=addsymbol(owner,d,staticid,0)

		else
			rxerror_s("Undefined: #",d.name,p)
		esac
	else
$else:
retry:
!CPL "RESOLVED

		p.def:=e			!update link in kcode

		case e.nameid
		when constid then		!convert namedconst to const
IF SYMBOLMODE THEN
	RETURN
FI

			q:=e.code			!q is knamedconst unit; q.c is value
			rx_unit(owner,q)
			if q.tag not in [jintconst, jrealconst, jstringconst] then
				rxerror_s("Not const expr: #",jtagnames[q.tag])
			fi

			e.mode:=q.mode
			p.tag:=q.tag
			p.value:=q.value
			p.mode:=q.mode
			p.slength:=q.slength
		when enumid then
IF SYMBOLMODE THEN
	RETURN
FI
			p.tag:=jintconst
			p.value:=e.index
			p.mode:=tint

		when staticid then		!deal with python global accesses ?? WTF ???
		when typeid,recordid then
			p.tag:=jtypeconst
			p.mode:=p.def.mode

		when linkid then
			rxerror("FOUND LINK",p)
		when frameid, paramid then
			if stcurrproc.nameid=anonprocid and e.owner.nameid<>anonprocid then
				rxerror("Accessing transient vars from {}")
			fi

		esac
	fi
end

global function resolvetopname(symbol owner,stnewname,int moduleno,allowmod)symbol=
!stnewname points to a symrec with nullid
!This is a top-level name (left-most name of any dotted sequence, or standalone name)
!Search through all the duplicate symrecs (all names with identical names have symrecs that
!are linked together, always starting with a nullid symrec) looking for the best match
!moduleno is the module where the currently generic name is encountered
!(derived from a unit if in an expression, or an STREC if a type in a declaration)

	int extcount,subprogno
	symbol p,q,powner,d,e,extdef,moddef
	[10]symbol ambiglist

	if owner.nameid=anonprocid then
		q:=owner.deflist
!CPL "SEARCHING FOR", STNEWNAME.NAME,"IN ANON:",OWNER.NAME
		while q, q:=q.nextdef do
!CPL "CHECKING",Q.NAME
			if q.firstdupl=stnewname then		!use that match
!CPL "FOUND", STNEWNAME.NAME,"IN ANON:",OWNER.NAME
				return q
			fi
		od
!CPL "NOT FOUND IN ANON:",STNEWNAME.NAME
		owner:=owner.owner
	fi

	if owner.nameid=procid then

!CP "------------SEARCHING PROC",OWNER.NAME,":"
!		q:=owner.deflist
!		while q, q:=q.nextdef do CP Q.NAME,$ OD
!		CPL

		q:=owner.deflist
		while q, q:=q.nextdef do
!CPL "CHECKING",Q.NAME
			if q.firstdupl=stnewname then		!use that match
!CPL "FOUND", STNEWNAME.NAME,"IN PROC:",OWNER.NAME
				return q
			fi
		od
	fi

	p:=stnewname.nextdupl
	subprogno:=modules[moduleno].subprogno

	extcount:=0
	extdef:=moddef:=nil

	while p, p:=p.nextdupl do						!p is next candidate
		powner:=p.owner

		case powner.nameid
		when moduleid then							!candidate is file-scope item
			if powner.moduleno=moduleno then		!same module
				return p
			elsif p.isglobal then	!matches an external module
!				if moduletosub[powner.moduleno]=subprogno or		!within same subprog
				if modules[powner.moduleno].subprogno=subprogno or		!within same subprog
					 p.isglobal=export_scope or
					 p.isimport then 				!visible outside subprog
					++extcount			!if an ext match is closest, there can only be one
					extdef:=p
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
				if allowmod then
					moddef:=p
				fi
			when macroid then
				return p

			esac

		esac
	od

!if here, then no immediate match
!either of moddef/dlldef will be set
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

	return moddef				!will be nil when no match
end

proc resolvedot(symbol owner,unit p)=
	symbol qdef,rdef,d,newd,e,fielddef
	unit q,r
	int nfields,oldallowmod

!CPL "RD1"
	if symbolmode then
		resolvedot_sym(owner, p)
		return
	fi

!CPL "RD2"
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

!CPL =NAMENAMES[D.NAMEID]
	case d.nameid
!	when dllmoduleid,moduleid,typeid,procid,dllprocid then	!M./T./P./C. non-var lhs
	when moduleid,typeid,procid,dllprocid then	!M./T./P./C. non-var lhs
dotype:
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
			end switch

		else
			cpl d.name,,".",,rdef.name
			rxerror("Can't resolve",p)
		fi

	when frameid, staticid, paramid, fieldid, structfieldid then	!X. normal lhs
doexprdot:
		nfields:=0
		fielddef:=nil
		e:=rdef.nextdupl

!CPL "RD31"
		while e do
!CPL "RD32"
			case e.nameid
			when fieldid,structfieldid, constid, procid, typeid, staticid, dllprocid then
				++nfields
				fielddef:=e				!use this when unique
			esac
			e:=e.nextdupl
		od

!CPL "RD35",NFIELDS, RDEF
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
!CPL "RD4"
		cpl namenames[d.nameid]
		rxerror("RXDOT:Unknown nameid",p)
	esac
!CPL "RDX"
end

proc resolvedot_sym(symbol owner,unit p)=
	symbol qdef,rdef,d,newd,e,fielddef
	unit q,r
	int nfields, oldallowmod

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

	case d.nameid
!	when dllmoduleid,moduleid,typeid,procid, dllprocid then	!M./T./P./C. non-var lhs
	when moduleid,typeid,procid, dllprocid then	!M./T./P./C. non-var lhs
	dotype:
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
	esac
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
!CPL "HERE/FD"
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
!p is either the call-unit as this may originally have been, or the same as a:
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

!First step: get list of macro formal parameters

	pm:=d.deflist
	nmacroparams:=0
	while pm do
		if nmacroparams>=maxmacroparams then
			rxerror("macro param overflow")
		fi
		macroparams[++nmacroparams]:=pm
		macroparamsgen[nmacroparams]:=pm.firstdupl		!generic st entry
		pm:=pm.nextdef
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

	m:=p.mode

	if m>=0 then return fi
	m:=-m

	if ttxmap[m] then				!already fixed
		p.mode:=ttxmap[m]
		return
	fi

	d:=ttnamedefx[m]

	e:=resolvetopname(owner,d,ttxmoduleno[m],0)

	if e then
		ttxmap[m]:=e.mode
		p.mode:=e.mode

	else
		rxerror_s("Can't resolve type: #",d.name)
	fi
end

function fixmode2(ref strec owner, int m)int=
!if m is a userx type, fix it up and return fixed up mode
!otherwise just return m
	ref strec d,e
	[256]char str

	if m>=0 then return m fi
	m:=-m

	if ttxmap[m] then				!already fixed
		return ttxmap[m]
	fi

	d:=ttnamedefx[m]

	if owner=nil then rxerror("FM2/owner") fi

	e:=resolvetopname(owner,d,ttxmoduleno[m],0)

	if e then
		ttxmap[m]:=e.mode
		return e.mode
	else
		fprint @&.str,"# in module #, line:#",d.name,modules[ttxmoduleno[m]].name
		rxerror_s("2:Can't resolve type: #",&.str)
	fi
	return 0
end

global proc fixusertypes=
	ref userxrec p
	ref int pmode
	int m, rescan,i

!CPL "FIXUSERTYPES",NUSERXTYPES

	for i:=1 to 2 do
		p:=userxmodelist
		rescan:=0

		while p do
			m:=p.pmode^
			if m<0 then
				m:=fixmode2(p.owner,m)
				if m<0 and i=2 and ttxmap[abs m] then
					m:=ttxmap[abs m]
				fi
				if m<0 then
					rescan:=1
				else
					p.pmode^:=m

					if tttarget[m]=m then
!CPL "RX1"
						rxerror_s("recursive type?",ttname[m])
					fi
				fi
			fi

			p:=p.nextmode
		od
		if not rescan then exit fi

	od
	if rescan then
!CPL "RX2"
		rxerror("FUT Phase Error")
	fi

	for i to nbaseclasses do
		dobaseclass(i)
	od
end

global proc tx_typetable=
!CPL "TXTYPEABLE"
	for i:=tlast+1 to ntypes do
		converttype(i)
	od
end

function getconstint(symbol owner,unit a, int ownerid=0)int=
!process unit found in tt-tables, and convert to int
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
	int oldmodno,pos,ownerid
	int maxalign, nfields, size
	unit plength, plower

	if ttsize[m] then return fi			!assume already done

	owner:=ttowner[m]

	plower:=ttlowerexpr[m]
	plength:=ttlengthexpr[m]

	case ttbasetype[m]
	when tpackstrc,tpackstrz then
		ttsize[m]:=ttlength[m]:=getconstint(owner,plength)
!
	when tvector then
		if m=tarray then CPL "CT:ARRAY/ARRAY" fi
		if ttowner[m] then
			ownerid:=ttowner[m].nameid
		else
			ownerid:=0
		fi
		if plower then
			ttlower[m]:=getconstint(owner,plower,ownerid)
		else
			ttlower[m]:=1
		fi

		if plength then
			ttlength[m]:=getconstint(owner,plength, ownerid)
		else
			ttlength[m]:=0
		fi
		elemtype:=tttarget[m]

		case elemtype
		when tu1,tu2,tu4 then
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
			size:=roundtoblock(size,maxalign)
			d.maxalign:=maxalign
		else
			d.maxalign:=1
		fi

		ttsize[m]:=size
		ttlower[m]:=1
		ttlength[m]:=ntopfields

		d.topfieldlist:=pcm_alloc(symbol.bytes*ntopfields)
		memcpy(d.topfieldlist,&structfields,symbol.bytes*ntopfields)

	when trecord then
!
	else
		CPL "CAN'T DO:",STRMODE(M),strmode(ttbasetype[m])
	esac
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

	while f:=fields[index++] do
		case f.nameid
		when structfieldid then
			converttype(f.mode)
			fieldsize:=ttsize[f.mode]

			if calign then
				alignment:=getalignment(f.mode)
				maxalign max:=alignment
				newoffset:=roundtoblock(offset, alignment)
				size+:=newoffset-offset
			else
				newoffset:=offset
			fi
			f.fieldoffset:=newoffset
			F.INDEX:=INDEX-1
			offset:=newoffset
countfields:
			++nallfields
			if countmode then
				structfields[++ntopfields]:=f

			fi
		when structblockid then
			scanstruct(1, fields, index, fieldsize, offset, calign, maxalign,countmode)

		when unionblockid then
			scanstruct(0, fields, index, fieldsize, offset, calign, maxalign, (countmode|1|0))

		when endblockid then
			isize:=size
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
end

proc dobaseclass(int baseclassindex)=
!do fixups needed for baseclass, that couldn't be in in parser until
!user types were fixed up
	ref strec sttype,d,e,newd
	int baseclass,normalexit

	baseclass:=baseclasstable[baseclassindex]
	sttype:=baseclassdef[baseclassindex]

	d:=ttnamedef[baseclass].deflist
	while d do
		e:=sttype.deflist
		normalexit:=1
		while e do
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
				newd:=addsymbol(sttype,d.firstdupl,linkid,0)
				newd.alias:=d
			else
				newd:=addsymbol(sttype,d.firstdupl,d.nameid,0)
				duplfield(d,newd)
				++sttype.nfields
				ttlength[sttype.mode]:=sttype.nfields
				newd.index:=sttype.nfields
				newd.fieldoffset:=(newd.index-1)*varsize
			esac
			addgenfield(newd)


		fi
		d:=d.nextdef
	od
end
=== qq_run.m 0 0 23/48 ===
!const doretcheck=1
const doretcheck=0

macro steppc = ++pc
macro skip1 = pc+:=2
macro skip2 = pc+:=3

macro zz = sp
macro yy = sp-1

!macro save = pcptr:=pc
macro save = (pcptr:=pc; sptr:=sp)

macro pclerror(x) = (pcptr:=pc; pcerror(x))
macro pclerror2(x,y) = (pcptr:=pc; pcerror(x,y))
macro pclustype(x,t) = (pcptr:=pc; pcustype(x,t))
macro pclmxtypes(x,t,u) = (pcptr:=pc; pcmxtypes(x,t,u))

!macro copyvar(x, y) = x^:=y^
!macro copyvarv(x, y) = x:=y^
!macro copyvar_v(x, y) = x^:=y

macro copyvar(x, y) = (x.dummy:=y.dummy; x.value:=y.value)
macro copyvarv(x, y) = (x.dummy:=y.dummy; x.value:=y.value)
macro copyvar_v(x, y) = (x.dummy:=y.dummy; x.value:=y.value)

global ref[0:]ref label jumptable		!stays nil here
byte getjt

global proc disploop =
	pcl pc
	variant sp
	ref byte fp

!	variant x
!		int index @ x
!		variant dest @ x
!		variant px @ x
!	variant y
!		symbol d @ y
!		int nloc @ y
!	variant z	
!		int n @ z
!		pcl pz @ z
!		object pp @ z
!		object q @ z
!		ref genfieldrec g

	variant x
	variant y
	variant z	
		int n
		int index
		variant dest
		variant px
		symbol d
		int nloc
		pcl pz
		object pp
		object q
		ref genfieldrec g

	int xt,yt, res, lower, upper, moduleno, offset
	variant newsp
	symbol e
	ref[0:]ref label localjumptable

	varrec vx

freddy:

	if getjt then
		jumptable:=localjumptable
!JUMPTABLE:=NIL
		return
	fi

	sp:=sptr
	pc:=pcptr
	fp:=frameptr

IF JUMPTABLE=NIL THEN PCERROR("JUMPTABLE NOT SET") fI
	doswitchx(localjumptable) pc.labaddr

!	doswitchu pc.opcode
!	doswitch pc.opcode
!	docase pc.opcode
!
	when knop      then   ! simple nop
!		unimpl
		steppc

	when kskip     then   ! ignore on pcl listing
		unimpl
		steppc

	when kprocdef  then   ! 
		unimpl
		steppc

	when kprocent  then   ! n=number of locals; 
		to pc.n do
			++sp
			sp.tagx:=tvoid
		od
		steppc

	when kprocend  then 
		unimpl
		steppc

	when kendmod   then 
		unimpl
		steppc

	when kcomment  then 
!		unimpl
		steppc

	when kpushm    then   ! Push v
		++sp
		copyvar(sp, pc.varptr)
		var_share(sp)
		steppc

	when kpushf    then   ! Push v
jpushf:
		++sp
		x:=cast(fp+pc.offset)
!		sp^:=x^
		copyvar(sp, x)

		var_share(sp)
		steppc

	when kpushmref then   ! push &v
jpushmref:
		++sp
		sp.tagx:=trefvar
		sp.varptr:=pc.varptr
		steppc

	when kpushfref then   ! push &v
jpushfref:
		++sp
		sp.tagx:=trefvar
		sp.varptr:=cast(fp+pc.offset)
		steppc

	when kpopm     then   ! v := Z
		x:=pc.varptr
		var_unshare(x)
!		x^:=sp^
		copyvar(x, sp)
		--sp
		steppc

	when kpopf     then   ! v := Z
		x:=cast(fp+pc.offset)
		var_unshare(x)
		copyvar(x, sp)
		--sp
		steppc

	when kpushlab  then   ! push L
		++sp
		sp.tagx:=trefpack
		sp.elemtag:=tvoid
		sp.labelref:=pc.labelref
		steppc

	when kpushci   then   ! Push i
jpushci:
		++sp
		sp.tagx:=tint
		sp.value:=pc.value
		steppc
jpushcix:

	when kpushvoid then   ! Push void 
		++sp
		sp.tagx:=tvoid
		steppc

	when kpushnil  then   ! Push nil (ref void)
		++sp
		sp.tagx:=trefpack
		sp.elemtag:=tvoid
		sp.ptr:=nil
		steppc

	when kpushcr   then   ! Push r
		++sp
		sp.tagx:=treal
		sp.xvalue:=pc.xvalue
		steppc

	when kpushcs   then   ! Push constant string object
		++sp
		sp.tagx:=tstring ior hasrefmask
		sp.objptr:=pc.objptr
		++sp.objptr.refcount
		steppc

	when kpushtype then   ! Push type constant
		++sp
		sp.tagx:=ttype
		sp.value:=pc.typecode
		steppc

	when kpushopc  then   ! Push operator constant
		++sp
		sp.tagx:=toperator
		sp.value:=pc.pclop
		steppc

	when kpushsym  then   ! Push symbol reference
		++sp
		sp.tagx:=tsymbol
		sp.def:=pc.def

		steppc

	when kpushptr  then   ! Z' := Z^
		x:=sp
jpushptr:
		case x.tag
		when trefvar then
			sp^:=x.varptr^

		when trefpack then
			case x.elemtag
			when tu8 then
				sp.tagx:=tint
				sp.value:=x.ptr^
				goto refpackend
			else
				save
				var_loadpacked(x.ptr, x.elemtag, sp, nil)
			esac

		when trefbit then
			save
			var_loadbit(x.ptr, x.bitoffset, x.elemtag, x.bitlength, sp)

		else
			pclustype("Pushptr",x)
		esac

		var_share(sp)
refpackend:
		steppc

	when kpushptrf then
		x:=cast(fp+pc.offset)
		++sp
		++pc
		goto jpushptr

	when kpopptr   then   ! Z^ := Y
		y:=sp--
		x:=sp--

		case y.tag
		when trefvar then
			var_unshare(y.varptr)
			y.varptr^:=x^
		when trefpack then
			save
			var_storepacked(y.ptr,x, y.elemtag)
		when trefbit then
			save
			var_storebit(y.ptr, y.bitoffset, x, y.elemtag, y.bitlength)

		else
			pclustype("Popptr",y)
		esac

		steppc

	when kzpopm    then   ! v := Z; don't free v first
		copyvar(pc.varptr, sp)
		--sp
		steppc

	when kzpopf    then   ! v := Z; don't free v first
		x:=cast(fp+pc.offset)
		copyvar(x, sp)
		--sp
		steppc

	when kdupl     then   ! (Z',Y') := (share(Z), Z)
		++sp
		copyvar(sp, sp-1)
		var_share(sp)
		steppc

	when kcopy     then   ! Z' := deepcopy(Z)
		if sp.hasref then
			copyvarv(vx, sp)
			save
			var_duplu(sp)
			var_unshareu(&vx)
		fi
		steppc

	when kswap     then   ! swap(Z^, Y^)
		x:=sp--
		y:=sp--

		if x.tag=y.tag=trefvar then

			copyvar(&vx, x.varptr)
			copyvar(x.varptr, y.varptr)
			copyvar(y.varptr, &vx)
!!			x:=x.varptr
!!			y:=y.varptr
!
!			swap(x.varptr.dummy, y.varptr.dummy)
!			swap(x.varptr.value, y.varptr.value)

		else
			save
			k_swap(x,y)
		fi
		steppc

	when kconvrefp then   ! Change ref in Z to refpacked
		save
		k_convrefpack(sp)
		steppc

	when kjump     then   ! Jump to L
		pc:=pc.labelref

	when kjumpptr  then   ! Jump to Z
		if sp.tag<>trefpack then pclerror("jumpptr?") fi
		pc:=sp.labelref
		--sp
	when kjumpt    then   ! Jump to L when Z is true
		x:=sp--

		if x.tag=tint then
			if x.value then
				pc:=pc.labelref
			else
				steppc
			fi
		else
			save
			if var_istruel(x) then
				pc:=pc.labelref
			else
				steppc
			fi
			var_unshare(x)
		fi

	when kjumpf    then   ! Jump to L when Z is false
		x:=sp--

		if x.tag=tint then
			if not x.value then
				pc:=pc.labelref
			else
				steppc
			fi
		else
			save
			if not var_istruel(x) then
				pc:=pc.labelref
			else
				steppc
			fi
			var_unshare(x)
		fi

	when kjumpeq   then   ! Jump to L when Y = Z
		y:=sp--
		x:=sp--

		if x.tag=y.tag=tint then
			if x.value=y.value then
				pc:=pc.labelref
			else
				steppc
			fi
		else
			save
			if var_equal(x, y) then
				pc:=pc.labelref
			else
				steppc
			fi
		fi

	when kjumpne   then   ! Jump to L when Y<>= Z
		y:=sp--
		x:=sp--

		if x.tag=y.tag=tint then
			if x.value<>y.value then
				pc:=pc.labelref
			else
				steppc
			fi
		else
			save
			if not var_equal(x, y) then
				pc:=pc.labelref
			else
				steppc
			fi
		fi

	when kjumplt   then   ! Jump to L when Y < Z
		y:=sp--
		x:=sp--

		if x.tag=y.tag=tint then
			if x.value<y.value then
				pc:=pc.labelref
			else
				steppc
			fi
		elsif x.tag=y.tag=treal then
			if x.xvalue<y.xvalue then
				pc:=pc.labelref
			else
				steppc
			fi
		else
			save
			if var_compare(x,y)<0 then
				pc:=pc.labelref
			else
				steppc
			fi
		fi

	when kjumple   then   ! Jump to L when Y <= Z
		y:=sp--
		x:=sp--

		if x.tag=y.tag=tint then
			if x.value<=y.value then
				pc:=pc.labelref
			else
				steppc
			fi
		else
			save
			if var_compare(x,y)<=0 then
				pc:=pc.labelref
			else
				steppc
			fi
		fi

	when kjumpge   then   ! Jump to L when Y >= Z
		y:=sp--
		x:=sp--

		if x.tag=y.tag=tint then
			if x.value>=y.value then
				pc:=pc.labelref
			else
				steppc
			fi
		else
			save
			if var_compare(x,y)>=0 then
				pc:=pc.labelref
			else
				steppc
			fi
		fi

	when kjumpgt   then   ! Jump to L when Y > Z
		y:=sp--
		x:=sp--

		if x.tag=y.tag=tint then
			if x.value>y.value then
				pc:=pc.labelref
			else
				steppc
			fi
		else
			save
			if var_compare(x,y)>0 then
				pc:=pc.labelref
			else
				steppc
			fi
		fi

	when kjmpeqfci   then   ! Jump to L when B = C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		if x.value=(pc+1).value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpnefci   then   ! Jump to L when B <> C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		if x.value<>(pc+1).value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpltfci   then   ! Jump to L when B < C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		if x.value<(pc+1).value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmplefci   then   ! Jump to L when B <= C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		if x.value<=(pc+1).value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpgefci   then   ! Jump to L when B >= C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		if x.value>=(pc+1).value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpgtfci   then   ! Jump to L when B > C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		if x.value>(pc+1).value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpeqff   then   ! Jump to L when B = C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		y:=cast(fp+(pc+1).offset)
		if x.value=y.value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpneff   then   ! Jump to L when B <> C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		y:=cast(fp+(pc+1).offset)
		if x.value<>y.value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpltff   then   ! Jump to L when B < C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		y:=cast(fp+(pc+1).offset)
		if x.value<y.value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpleff   then   ! Jump to L when B <= C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		y:=cast(fp+(pc+1).offset)
		if x.value<=y.value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpgeff   then   ! Jump to L when B >= C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		y:=cast(fp+(pc+1).offset)
		if x.value>=y.value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpgtff   then   ! Jump to L when B > C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		y:=cast(fp+(pc+1).offset)
		if x.value>y.value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kwheneq   then   ! Y = Z:  pop both, jump to L
						  ! Y <> Z: pop Z only; don't jump
		y:=sp--
		x:=sp

		if x.tag=y.tag=tint then
			if x.value=y.value then
				--sp
				pc:=pc.labelref
			else
				steppc
			fi
		else
			save
			res:=k_when(x, y)
			var_unshare(y)
			if res then
				var_unshare(x)
				--sp
				pc:=pc.labelref
			else
				steppc
			fi
		fi

	when kwhenne   then   ! Y <> Z:  pop Z only, jump to L
						  ! Y = Z:   pop both, step to next
jwhenne:
		y:=sp--
		x:=sp

		if x.tag=y.tag=tint then
			if x.value<>y.value then
				pc:=pc.labelref
			else
				--sp
				steppc
			fi
		else
			save
			res:=k_when(x, y)
			var_unshare(y)
			if not res then
				pc:=pc.labelref
			else
				var_unshare(x)
				--sp
				steppc
			fi
		fi

	when kjumplab  then   ! Jumptable entry
		unimpl
		steppc

	when kswitch   then   ! Jumptable has y-x+1 entries
		if sp.tag not in [tint, ttype] then
			pclerror2("switch not int",ttname[sp.tag])
		fi

		n:=sp.value

!CPL "SWITCH", =N, =PC.X, =PC.Y

		if n in pc.x..pc.y then
!CPL "IN", (N-PC.X+1)
			pc:=(pc+n-pc.x+1).labelref
		else
			pc:=(pc+pc.y-pc.x+2).labelref
		fi
		--sp

	when ktom      then   ! --v; jump to l when v<>0 in next op
		x:=(pc+1).varptr
		doto

	when ktof      then   ! --v; jump to l when v<>0 in next op
		x:=cast(fp+(pc+1).offset)
doto:
		if --x.value then
			pc:=pc.labelref
		else
			skip1
		fi

	when kformci   then   ! ++v; jump to l when v<=i in next 2 ops: pushm/pushci
		x:=(pc+1).varptr
		doforfci

	when kforfci   then   ! ++v; jump to l when v<=i in next 2 ops: pushm/pushci
		x:=cast(fp+(pc+1).offset)
doforfci:
		++x.value
		if x.value<=(pc+2).value then
			pc:=pc.labelref
		else
			skip2
		fi

	when kformm    then   ! ++v; jump to l when v<=v in next 2 ops
		x:=(pc+1).varptr
		y:=(pc+2).varptr
		doforff

	when kforff    then   ! ++v; jump to l when v<=v in next 2 ops
		x:=cast(fp+(pc+1).offset)
		y:=cast(fp+(pc+2).offset)
doforff:
		++x.value

		if x.value<=y.value then
			pc:=pc.labelref
		else
			skip2
		fi

	when kcallproc then   ! Call &A; n is no. args
		const countinterval=100
		static int count=countinterval

!		if --count=0 then
!			count:=countinterval
!CPL "OS_PEEK"
!			os_peek()
!CPL "......OS_PEEK"
!		fi
!
!		if sp>=stacklimit then
!			pclerror("Stack Overflow")
!		fi

		++sp
		sp.tagx:=tretaddr
		sp.retaddr := pc+1

		sp.frameptr_low := u64(fp)
		fp:=cast(sp)

		pc:=pc.labelref

	when kcallptr  then   ! Call X^; n is no. of params supplied; x is stack adjust
		if sp.tag<>tsymbol then
			pclerror("Probably undefined function")
		fi

		d:=sp.def
		if d.nameid=linkid then d:=d.alias fi

		if d.nparams<>pc.n then
			pclerror2("Callptr: wrong # params; need:",strint(d.nparams))
		fi

		sp.tagx:=tretaddr
		sp.retaddr := pc+1

		sp.frameptr_low := word(fp)
		fp:=cast(sp)

		pc:=cast(d.labelref)

	when kretproc  then
doretproc:
		to pc.x do
			var_unshare(sp)
			--sp
		od

		n:=pc.n
		pc:=sp.retaddr
		fp:= cast(u64(fp) iand (0xFFFF'FFFF'0000'0000) ior sp.frameptr_low)
		--sp

		to n do
			var_unshare(sp)
			--sp
		od

	when kretfn  then
		x:=variant(fp+pc.y)
		copyvar(x, sp)		!transfer reference
		--sp
		doretproc

	when kmodcall  then   ! 
		d:=pc.def
		moduleno:=d.moduleno

		++sp
		sp.tagx:=tretaddr
		sp.retaddr := pc+1
		pc:=modules[moduleno].pcstart

	when kmodret   then   ! 
		pc:=sp.retaddr

	when kcalldll  then   ! Call dll function d (sysmbol); n=nargs
		n:=pc.n
		save
		SPTR:=SP

		calldll(pc.def, sp-n+1, sp-n, n)
		sp-:=n

		steppc

	when kcallhost then   ! Call Q host function h (Host index)
		save
		sp:=callhostfunction(pc.hostindex, sp)
		steppc

	when kunshare  then   ! Unshare and pop A var values on stack
		to pc.n do
			var_unshare(sp)
			--sp
		od
		steppc

	when kstop     then   ! Stop program with stopcode Z; n=1 to stop runproc instead
		stopped:=1
		sptr:=sp
		exit

	when kmakelist then   ! x items on stack; make list with lwb y
		save
		sp:=k_makelist(sp, pc.y, pc.x)
		steppc

	when kmakevrec then   ! x items on stack; make record of type u
		n:=pc.x
		x:=sp-pc.x+1				!start of data

		save
		var_make_record(x, x, pc.x, pc.usertag)
		sp:=x
		sp.objptr.mutable:=0
		steppc

	when kmakeax   then   ! x items on stack; make array with lwb y, type u and elemtype v
		x:=sp-pc.x+1				!start of data

		var_make_array(x, x, pc.y, pc.x, pc.usertag, pc.usertag2)
		sp:=x
		sp.objptr.mutable:=0
		steppc

	when kmakebits then   ! x items on stack; make bits with lwb y, type u and elemtype v
		unimpl
		steppc

	when kmaketrec then   ! x items on stack; make struct with type u
		n:=pc.x
		x:=sp-n+1				!start of data

		save
		var_make_struct(x, x, n, pc.usertag)
		sp:=x
		sp.objptr.mutable:=0
		steppc

	when kmakeset  then   ! x items on stack; make set
		n:=pc.x

		x:=sp-n+1			!start of data

		save
		var_make_set(x, x, n)
		sp:=x
		sp.objptr.mutable:=0

		steppc

	when kmakerang then   ! 2 items on stack; make range
		y:=sp--
		x:=sp

		unless x.tag=y.tag=tint then
			pclerror("makerange/not int")
		end

		sp.tagx:=trange
		lower:=x.value
		upper:=y.value

		if lower not in -(2**48)..2**48-1 then
			pclerror("Range lwb bounds")
		end

		sp.range_upper:=upper
		sp.range_lower:=lower

		steppc

	when kmakedict then   ! x*2 items on stack (x key:val items); make dict
		n:=pc.x
		x:=sp-n*2+1			!start of data

		save
		var_make_dict(x, x, n)
		sp:=x
		steppc

	when kmakedec  then   ! Turn string on stack to decimal number
!		vx:=sp^
		copyvarv(vx, sp)

		if vx.tag<>tstring then pclerror("Not str") fi
		pp:=vx.objptr
		if pp.length=0 then pclerror("Null str") fi

		save
		var_make_dec_str(pp.strptr, pp.length, sp)

		var_unshare(&vx)

		steppc

	when kincrptr  then   ! Z^ +:= x
		save
		k_incrptr(sp, pc.x)
		--sp
		steppc

	when kincrtom  then   ! v +:= x
		x:=pc.varptr
		doincrto

	when kincrtof  then   ! v +:= x
jincrtof:
		x:=cast(fp+pc.offset)
doincrto:
		case x.tag
		when tint then
			x.value+:=pc.x
		when trefvar then
			x.varptr+:=pc.x
		when trefpack then
			x.ptr+:=ttsize[x.elemtag]*pc.x
		when treal then
			x.xvalue+:=pc.x
		else
			pclustype("incrto",x)
		end
		steppc
jincrtofx:

	when kloadincr then   ! T := Z^; Z^ +:= x; Z' := T
		copyvarv(vx, sp)
!		vx:=sp^
		save
		var_loadptr(sp,sp)
		++sp
!		sp^:=vx
		copyvar_v(sp, vx)
		k_incrptr(sp, pc.x)
		--sp
		steppc

	when kincrload then   ! Z^ +:= x; Z' := Z^
!		vx:=sp^
		copyvarv(vx, sp)
		save
		k_incrptr(sp, pc.x)
		--sp
		var_loadptr(&vx, ++sp)
		steppc

	when kneg      then   ! Z':= -Z
!		vx:=sp^

		copyvarv(vx, sp)
		save
		var_neg(sp)
		var_unshare(&vx)
		steppc

	when kabs      then   ! Z' := abs Z
!		vx:=sp^
		copyvarv(vx, sp)

		save
		var_abs(sp)
		var_unshare(&vx)

		steppc

	when knotl     then   ! Z' := not Z
		save
		res:=not var_istruel(sp)
		var_unshare(sp)
		sp.tagx:=tint
		sp.value:=res
		steppc

	when kinot     then   ! Z' := inot Z
		if sp.tag=tint then
			sp.value:=inot sp.value
		else
			copyvarv(vx, sp)
			save
			var_inot(sp)
			var_unshare(&vx)
		fi

		steppc

	when kistruel  then   ! Z' := istrue Z
		save
		n:=var_istruel(sp)
		var_unshare(sp)
		sp.tagx:=tint
		sp.value:=n

		steppc

	when kasc      then   ! Z' := asc(Z)
		case sp.tag
		when tstring then
			if sp.objptr.length then
				n:=sp.objptr.strptr^
			else
				n:=0
			fi
			var_unshareu(sp)
			sp.tagx:=tint
			sp.value:=n
		else
			pcustype("ASC",sp)
		esac
		steppc

	when kchr      then   ! Z' := chr(Z)
		if sp.tag=tint then
			save
			var_makechar(sp.value, sp)
		else
			pclustype("CHR",sp)
		fi
		steppc

	when ksqr    then   ! Z' := op(Z)
		case sp.tag
		when tint then
			sp.value:=sqr(sp.value)
		when treal then
			sp.xvalue:=sqr(sp.xvalue)
		else
			pclustype("sqr", sp)
		esac
		steppc

	when kmaths    then   ! Z' := op(Z)
		save
		k_maths(sp, pc.mathscode)
		steppc

	when kmaths2   then   ! Z' := op(Y, Z)
		save
		k_maths2(sp-1, sp, pc.mathscode)
		--sp
		steppc

	when kunaryto  then   ! Z^ op:= Z
		unimpl
		steppc

	when knotlto   then   ! Z^ not:= Z
		unimpl
		steppc

	when klen      then   ! Z' := Z.len
		save
		k_len(sp)
		steppc

	when klwb      then   ! Z' := Z.lwb
		save
		k_lwb(sp)
		steppc

	when kupb      then   ! Z' := Z.upb
jupb:
		save
		k_upb(sp)
		steppc

	when kbounds   then   ! Z' := Z.bounds; n=1: one range value; n=2: two dims
		save
		k_bounds(sp, lower, upper)

		if pc.n=2 then				!push as 2 value
			var_unshare(sp)
			sp.tagx:=tint
			sp.value:=lower
			++sp
			sp.tagx:=tint
			sp.value:=upper

		else						!push as 1 range value
			var_unshare(sp)
billy:
			sp.tagx:=trange
			sp.range_lower:=lower
			sp.range_upper:=upper
		fi

		steppc

	when kbytesize then   ! Z' := Z.bytesize
		save
		res:=k_bytesize(sp)
		var_unshare(sp)
		sp.tagx:=tint
		sp.value:=res
		steppc

	when ktype     then   ! Z' := n=0/1/2 = basetype/tag/elemtype
		save
		n:=k_type(sp, pc.n)
		var_unshare(sp)
		sp.tagx:=ttype
		sp.value:=n

		steppc

	when kdictsize then   ! Z' := Z.dictsize
		case sp.tag
		when tdict then
			n:=sp.objptr.dictitems
		when tdecimal then
			n:=sp.objptr.length
		else
			pcustype("Dictitems/digits",sp)
		esac
		var_unshare(sp)
		sp.tagx:=tint
		sp.value:=n
		steppc

	when kisfound  then   ! Z' := Z.isfound
		if sp.tag<>tint then pclerror("isfound") fi
		sp.value:=sp.value<>i64.min
		steppc

	when kminval   then   ! Z' := Z.minvalue
		save
		k_minval(sp)
		steppc

	when kmaxval   then   ! Z' := Z.maxvalue
		save
		k_maxval(sp)
		steppc

	when kistype   then   ! Z' := Z.type/etc = t
		n:=0
		if pc.typecode=trefvar then
			if sp.tag in [trefvar, trefpack, trefbit] then n:=1 fi
		else
			if pc.typecode=sp.tag then n:=1 fi
		fi
		var_unshare(sp)
		sp.tagx:=tint
		sp.value:=n
		steppc

	when kisvoid   then   ! Z' := Z.isvoid (n=0) or not Z.isdef (n=1)
		res:=sp.tag=tvoid
		var_unshare(sp)
		sp.tagx:=tint
		sp.value:=res ixor pc.n
		steppc

	when kconvert  then   ! Z' := t(Z)
		if sp.tag<>pc.usertag then
!			vx:=sp^
			copyvarv(vx, sp)
			save
			var_convert(&vx, pc.usertag, sp)
			var_unshare(&vx)
		fi

		steppc

	when ktypepun  then   ! Z' := t@(Z)
		sp.tagx:=pc.typecode
		steppc

	when kadd      then   ! Z' := Y + Z
jadd:
		y:=sp--

		if sp.tag=y.tag=tint then
			sp.value+:=y.value
		elsif sp.tag=y.tag=treal then
			sp.xvalue+:=y.xvalue
		else
!			vx:=sp^
			copyvarv(vx, sp)

			save
			var_add(sp, y)

			var_unshare(&vx)
			var_unshare(y)
		fi
		steppc

	when ksub      then   ! Z' := Y - Z
		y:=sp--

		if sp.tag=y.tag=tint then
			sp.value-:=y.value
		elsif sp.tag=y.tag=treal then
			sp.xvalue-:=y.xvalue
		else
			copyvarv(vx, sp)

			save
			var_sub(sp, y)

			var_unshare(&vx)
			var_unshare(y)
		fi
		steppc

	when kmul      then   ! Z' := Y * Z
		y:=sp--

		if sp.tag=y.tag=tint then
			sp.value*:=y.value
		elsif sp.tag=y.tag=treal then
			sp.xvalue*:=y.xvalue
		else
!			vx:=sp^
			copyvarv(vx, sp)

			save
			var_mul(sp,y)

			var_unshare(&vx)
			var_unshare(y)
		fi

		steppc

	when kdiv      then   ! Z' := Y / Z
		y:=sp--
		copyvarv(vx, sp)

		if sp.tag=y.tag=treal then
			sp.xvalue/:=y.xvalue
		else	
			save
			var_div(sp,y)

			var_unshare(&vx)
			var_unshare(y)
		fi

		steppc

	when kidiv     then   ! Z' := Y % Z
		y:=sp--
		copyvarv(vx, sp)

		if sp.tag=y.tag=tint then
!			sp.value/:=y.value
			sp.value:=sp.value/y.value
		else	
			save
			var_idiv(sp, y)

			var_unshare(&vx)
			var_unshare(y)
		fi


		steppc

	when kirem     then   ! Z' := Y rem Z
		y:=sp--
		copyvarv(vx, sp)

		save
		var_irem(sp,y)

		var_unshare(&vx)
		var_unshare(y)

		steppc

	when kidivrem  then   ! (Y', Z') := Y divrem Z
		y:=sp
		x:=sp-1
		unless x.tag=y.tag=tint then pclerror("divrem") end
		lower:=x.value/y.value
CPL =X.VALUE, Y.VALUE
		y.value:=x.value rem y.value
		x.value:=lower
		sp:=k_makelist(sp, 1, 2)
		steppc

	when kiand     then   ! Z' := Y iand Z
		y:=sp--

		if sp.tag=y.tag=tint then
			sp.value iand:=y.value
		else
			copyvarv(vx, sp)
			save
			var_iand(sp,y)

			var_unshare(&vx)
			var_unshare(y)
		fi
		steppc

	when kior      then   ! Z' := Y ior Z
		y:=sp--

		if sp.tag=y.tag=tint then
			sp.value ior:=y.value
		else
			copyvarv(vx, sp)
			save
			var_ior(sp,y)

			var_unshare(&vx)
			var_unshare(y)
		fi
		steppc

	when kixor     then   ! Z' := Y ixor Z
		y:=sp--
		if sp.tag=y.tag=tint then
			sp.value ixor:=y.value
		else
			copyvarv(vx, sp)

			save
			var_ixor(sp,y)

			var_unshare(&vx)
			var_unshare(y)
		fi
		steppc

	when kshl      then   ! Z' := Y << Z
		y:=sp--

		if sp.tag=y.tag=tint then
			sp.value <<:=y.value
		else
			copyvarv(vx, sp)

			save
			var_shl(sp,y)

			var_unshare(&vx)
			var_unshare(y)
		fi
		steppc

	when kshr      then   ! Z' := Y >> Z
		y:=sp--

		if sp.tag=y.tag=tint then
			sp.value >>:=y.value
		else
			copyvarv(vx, sp)

			save
			var_shr(sp,y)

			var_unshare(&vx)
			var_unshare(y)
		fi
		steppc

	when kin       then   ! Z' := Y in Z (n=0) or Y not in Z (n=1)
		y:=sp
		x:=--sp

		save
		n:=var_in(x,y) ixor pc.n
		var_unshare(x)
		var_unshare(y)

		sp.tagx:=tint
		sp.value:=n
		steppc

	when kinx      then   ! Z' := Y inx Z
		y:=sp
		x:=--sp

		save
		n:=var_inx(x,y)
		var_unshare(x)
		var_unshare(y)

		sp.tagx:=tint
		sp.value:=n

		steppc

	when kcmp      then   ! Z' := Y c Z
		y:=sp
		x:=--sp

		save
		res:=k_cmp(pc.n, x, y)
		var_unshare(x)
		var_unshare(y)

		sp.tagx:=tint
		sp.value:=res
		steppc

	when kmin      then   ! Z' := min(Y, Z)
		y:=sp--
		x:=sp

		save
		if var_compare(x,y)<0 then		!x is smaller
			var_unshare(y)
		else
			var_unshare(x)
			sp^:=y^
		fi

		steppc

	when kmax      then   ! Z' := max(Y, Z)
		y:=sp--
		x:=sp

		save
		if var_compare(x,y)>=0 then		!x is bigger
			var_unshare(y)
		else
			var_unshare(x)
			sp^:=y^
		fi
		steppc

	when kconcat   then   ! Z' := concat(Y, Z) or Y && Z
		y:=sp--
		vx:=sp^

		var_concat(sp,y)
		var_unshare(&vx)

		steppc

	when kappend   then   ! Z' := append(Y, Z) or Y & Z
		y:=sp--
		vx:=sp^

		var_append(sp,y)
		var_unshare(&vx)

		steppc

	when ksame     then   ! Z' := Y == Z
		y:=sp--
		x:=sp

		if x.hasref and y.hasref and x.objptr=y.objptr then
			res:=1
		else
			res:=0
		fi

		var_unshare(x)
		var_unshare(y)
		sp.tagx:=tint
		sp.value:=res

		steppc

	when kpower    then   ! Z' := Y ** Z
		y:=sp--
		copyvarv(vx, sp)

		save
		var_power(sp, y)

		var_unshare(&vx)
		var_unshare(y)
		steppc

	when kbinto    then   ! Z^ op:= Y
!CPL "BINTO"
		x:=sp--
		y:=sp--

		z:=x.varptr
		if pc.bintoindex=1 and x.tag=trefvar and z.tag=y.tag=tint then
			z.value+:=y.value
		else
			save
			var_inplace(pc.bintoindex, x, y)
			var_unshare(y)
		fi
		steppc

	when kbintof    then   ! A op:= Y
		y:=sp--
		z:=cast(fp+pc.offset)

		if (pc+1).bintoindex=1 and z.tag=y.tag=tint then
			z.value+:=y.value
		else
			save
			vx.tagx:=trefvar
			vx.varptr:=z
			var_inplace((pc+1).bintoindex, &vx, y)
			var_unshare(y)
		fi
		skip1

	when kaddtof    then   ! A +:= Y
		y:=sp--
		z:=cast(fp+pc.offset)

		if z.tag=y.tag=tint then
			z.value+:=y.value
		else
			save
			vx.tagx:=trefvar
			vx.varptr:=z
			var_inplace((pc+1).bintoindex, &vx, y)
			var_unshare(y)
		fi
		skip1

	when kaddtofci    then   ! A +:= B
		z:=cast(fp+(pc+1).offset)

		if z.tag=tint then
			z.value+:=pc.value
		else
			goto jpushci
		fi
		skip2

	when ksubtofci    then   ! A -:= B
		z:=cast(fp+(pc+1).offset)

		if z.tag=tint then
			z.value-:=pc.value
		else
			goto jpushci
		fi
		skip2

	when kshltofci    then   ! A <<:= B
		z:=cast(fp+(pc+1).offset)

		if z.tag=tint then
			z.value<<:=pc.value
		else
			goto jpushci
		fi
		skip2

	when kshrtofci    then   ! A >>:= B
		z:=cast(fp+(pc+1).offset)

		if z.tag=tint then
			z.value>>:=pc.value
		else
			goto jpushci
		fi
		skip2

	when kandlto   then   ! Y^ and:= Z
		unimpl
		steppc

	when korlto    then   ! Y^ or:= Z
		unimpl
		steppc

	when kappendto then   ! Y^ append:= Z or Y^ &:= Z
		px:=sp--
		y:=sp--

		case px.tag
		when trefvar then
			save
			var_appendto(px.varptr, y)
		else
			pclustype("Appendto", px)
		esac
		steppc

	when kconcatto then   ! Y^ concat:= Z or Y^ &&:= Z
		px:=sp--
		y:=sp--

		case px.tag
		when trefvar then
			save
			var_concatto(px.varptr, y)
		else
			pclustype("Concatto", px)
		esac
		steppc

	when kdot      then   ! Z' := Z.g

		g:=genfieldtable[pc.index]

		if g.nextdef=nil and g.def.nameid=fieldid then			!streamline it
			d:=genfieldtable[pc.index].def

			if sp.objptr.usertag<>d.owner.mode then pclerror("Dot1: wrong type") fi
			x:=sp.objptr.varptr+d.fieldoffset/varsize
			var_share(x)
			var_unshare(sp)
			copyvar(sp, x)

		else
			save
			k_dot(sp, g)
		fi
		steppc

	when kpopdot   then   ! Z.g := Y
		g:=genfieldtable[pc.index]

		if g.nextdef=nil and g.def.nameid=fieldid then			!streamline it
			x:=sp--
			y:=sp--

			if x.tag<>trecord then pclerror("Popdot1: not rec") fi
			if not x.objptr.mutable then
				save
				pcnotmut()
			fi

			e:=g.def

			if x.objptr.usertag<>e.owner.mode then pclerror("Popdot1: wrong type") fi
			z:=x.objptr.varptr+e.fieldoffset/varsize

			var_unshare(z)
			copyvar(z, y)
			var_unshare(x)
		else
			save
			sp:=k_popdot(sp, g)
		fi
		steppc

	when kdotref   then   ! Z' := &Z.g
		save
		k_dotref(sp, genfieldtable[pc.index])
		steppc

	when kindex    then   ! Z' := Y[Z]
		y:=sp--
		copyvarv(vx, sp)

		save
		case y.tag
		when tint then
			var_getix(sp,y.value)
		when trange then
			var_getslice(sp,y.range_lower,y.range_upper)
		else
			pclmxtypes("Index",&vx,y)
		esac

		var_unshare(&vx)

		steppc

	when kpopix    then   ! Z' := Y[Z]:=X
		z:=sp--		!index
		y:=sp--		!list etc
		x:=sp--		!value to store

		save
		case z.tag
		when tint then
			var_putix(y, z.value, x)
			var_unshare(y)
		when trange then
			var_putslice(y, z.range_lower, z.range_upper, x)
			var_unshare(x)
			var_unshare(y)
		else
			pclmxtypes("Popix",y,z)
		esac

		steppc

	when kindexref then   ! Z' := &Y[Z]
		y:=sp--
		copyvarv(vx, sp)

		save
		case y.tag
		when tint then
			var_getixref(sp, y.value)
		else
			pclmxtypes("Indexref",sp,y)
		esac

		var_unshare(&vx)
		steppc

	when kkeyindex then   ! Z' := X{Y, Z}
		save
		sp:=k_keyindex(sp)
		steppc

	when kpopkeyix then   ! Y{Z} := X
		save
		sp:=k_popkeyindex(sp)
		steppc

	when kkeyixref then   ! Z' := &X{Y, Z}
		save
		sp:=k_keyindexref(sp)
		steppc

	when kdotix    then   ! Z' := Y.[Z]
		y:=sp--
		copyvarv(vx, sp)

		save
		case y.tag
		when tint then
			var_getdotix(sp, y.value)
		when trange then
			var_getdotslice(sp, y.range_lower, y.range_upper)
		else
			pcmxtypes("Dotindex", &vx, y)
		esac

		var_unshare(&vx)

		steppc

	when kpopdotix then   ! Y.[Z] := X
		z:=sp--		!index
		y:=sp--		!ref to int, string etc
		x:=sp--		!value to store

		save
		case z.tag
		when tint then
			var_putdotix(y, z.value, x)
			var_unshare(y)
		when trange then
			var_putdotslice(y, z.range_lower, z.range_upper, x)
			var_unshare(x)
			var_unshare(y)
		else
			pclmxtypes("Popdotindex",y,z)
		esac
		
		steppc

	when kdotixref then   ! Z' := &Y.[Z]
		y:=sp--

		case y.tag
		when tint then
			var_getdotixref(sp, y.value)
		when trange then
			var_getdotsliceref(sp, y.range_lower, y.range_upper)
		else
			pclmxtypes("Dotindexref",sp,y)
		esac

		steppc

	when kexpand   then   ! Z' := Expand Z into n objects are needed
		x:=sp+pc.n-1
		save
		var_expand(sp, x, pc.n)
		sp:=x

		steppc

	when kpushtry  then   ! Push try/except into; label/except code/no. exceptions
		(++sp).tagx:=texception
		sp.ptr:=cast(pc.labelref)

		sp.frameoffset:=fp-ref byte(sp)		!byte offset
		sp.exceptiontype:=pc.x
		sp.nexceptions:=pc.y
		steppc

	when kraise    then   ! Raise exception Z
		if sp.tag<>tint then
			pcerror("Raise: not Int")
		fi
		
		save
		frameptr:=fp
		pc:=raiseexception(sp.value)		!will unwind stack and set pc to address of exception code
		sp:=sptr
		fp:=frameptr

	when kmap      then   ! Z' := map(Y, Z)
		save
		pc:=k_map(sp, pc, newsp)
		sp:=newsp

	when kaddsp    then   ! SP+:=A; note: positive A will push, negative will pop (reverse of the hardware)
		sp-:=pc.n
		steppc


	when kpushff    then   ! Push f/f
		++sp
		sp^:=cast(fp+pc.offset, variant)^
		var_share(sp)
		++sp
		sp^:=cast(fp+(pc+1).offset, variant)^
		var_share(sp)
		skip1

	when kpushfff    then   ! Push f/f/f
		++sp
		copyvar(sp, cast(fp+pc.offset, variant))
		var_share(sp)
		++sp
		copyvar(sp, cast(fp+(pc+1).offset, variant))
		var_share(sp)
		++sp
		copyvar(sp, cast(fp+(pc+2).offset, variant))
		var_share(sp)
		skip2

	when kpushmci then
		++sp
		copyvar(sp, pc.varptr)
		var_share(sp)
		++sp
		sp.tagx:=tint
		sp.value:=(pc+1).value
		skip1

	when kpushfci then
		++sp
		copyvar(sp, cast(fp+pc.offset, variant))
		var_share(sp)
		++sp
		sp.tagx:=tint
		sp.value:=(pc+1).value
		skip1

	when kaddff    then   !
		x:=cast(fp+pc.offset, variant)
		y:=cast(fp+(pc+1).offset, variant)

		if x.tag=y.tag=tint then
			++sp
			sp.tagx:=tint
			sp.value:=x.value+y.value
			skip2
		else
			goto jpushf
		fi

	when kaddfci    then   !
		x:=cast(fp+pc.offset, variant)

		if x.tag=tint then
			++sp
			sp.tagx:=tint
			sp.value:=x.value+(pc+1).value
			skip2
		else
			goto jpushf
		fi

	when ksubfci    then   !
		x:=cast(fp+pc.offset, variant)

		if x.tag=tint then
			++sp
			sp.tagx:=tint
			sp.value:=x.value-(pc+1).value
			skip2
		else
			goto jpushf
		fi

	when kaddci then
		if sp.tag=tint then
			sp.value+:=pc.value
			skip1
		else
			goto jpushci
		fi

	when ksubci then
		if sp.tag=tint then
			sp.value-:=pc.value
			skip1
		else
			goto jpushci
		fi

	when kiandci then
		if sp.tag=tint then
			sp.value iand:=pc.value
			skip1
		else
			goto jpushci
		fi

	when kshlci then
		if sp.tag=tint then
			sp.value <<:=pc.value
			skip1
		else
			goto jpushci
		fi

	when kshrci then
		if sp.tag=tint then
			sp.value >>:=pc.value
			skip1
		else
			goto jpushci
		fi

	when kmovefci then
		x:=cast(fp+(pc+1).offset)
		var_unshare(x)
		x.tagx:=tint
		x.value:=pc.value
		skip1

	when kmoveff then
		x:=cast(fp+(pc+1).offset)
		y:=cast(fp+pc.offset)
		var_share(y)
		var_unshare(x)
		copyvar(x, y)
		skip1

	when kindexff then
		x:=cast(fp+pc.offset)
doindexff:
		y:=cast(fp+(pc+1).offset)
		++sp
		copyvar(sp, x)

		save
		case y.tag
		when tint then
			var_getix(sp, y.value)
		when trange then
			var_getslice(sp, y.range_lower, y.range_upper)
		else
			pclmxtypes("Indexff",x,y)
		esac
		skip2

	when kindexmf then
		x:=pc.varptr
		goto doindexff

	when kwheneqci then
		x:=sp
		if x.tag=tint then
			if x.value=pc.value then
				--sp
				pc:=(pc+1).labelref
			else
				skip1
			fi
		else
			goto jpushci
		fi

	when kwhenneci then   ! Y <> Z:  pop Z only, jump to L
						  ! Y = Z:   pop both, step to next
		x:=sp

		if x.tag=tint then
			if x.value<>pc.value then
				pc:=(pc+1).labelref
			else
				--sp
				skip1
			fi
		else
			goto jpushci
		fi

	when kupbm then
		++sp
		copyvar(sp, pc.varptr)
		var_share(sp)
		save
		k_upb(sp)
		skip1

	when kpushipm then
		x:=pc.varptr
		if x.tag<>trefpack or x.elemtag<>tu8 then goto jpushmref fi
		goto dopushipf

	when kpushipf then
		x:=cast(fp+pc.offset)
		if x.tag<>trefpack or x.elemtag<>tu8 then goto jpushfref fi
dopushipf:
		++sp
		sp.tagx:=tint
		case x.elemtag
		when tu8 then
			sp.value:=x.ptr^
			x.ptr+:=(pc+1).x
		esac
		skip2

	when kpopipm then
		x:=pc.varptr
		if x.tag<>trefpack or x.elemtag<>tu8 or sp.tag<>tint then goto jpushmref fi
		goto dopopipf

	when kpopipf then
		x:=cast(fp+pc.offset)
		if x.tag<>trefpack or x.elemtag<>tu8 or sp.tag<>tint then goto jpushfref fi
dopopipf:
		case x.elemtag
		when tu8 then
			x.ptr^:=sp.value
			x.ptr+:=(pc+1).x
		esac
		--sp
		skip2

	when kstoref   then
		x:=cast(fp+pc.offset)
		var_share(sp)
		copyvar(x, sp)
		skip1

	when klastpcl  then		! needed for switchu when to trap unimpl extended ops
		unimpl
	else
unimpl:
		pclerror2("Unimpl op:", pclnames[pc.opcode])
		stop 1
	end
end

proc start=
!set up jumptable
	getjt:=1
	disploop()
	getjt:=0
end

global proc fixupcode(ifile pm)=
	pcl pc
	return when jumptable=nil

	pc:=pm.pcstart

	while pc.opcode<>kendmod, ++pc do
		pc.labaddr:=jumptable[pc.opcode]
	od
end

global function runqprogram(isubprog sp, int ismain)int=
	
	return 0 when runcode<run_cc

	sptr:=&varstack[1]
	stacklimit:=&varstack[stacksize-100]
	pcptr:=modules[sp.firstmodule].pcstart
	stopped:=0
!
	int tt:=clock()

!CPL "RUNQ"

	disploop()
!
	tt:=clock()-tt

!	println "Time:",TT
!	println

	return sptr.value
end

=== qq_runaux.m 0 0 24/48 ===
!comment

!GLOBAL INT NRESOLVE, NRFLOOPS, NRFSINGLE, NRFONEINST

global proc pcerror(ichar mess, param="")=
	reportpcerror(pcptr, mess, param)
end

global proc pcustype(ichar mess, variant x) =
	pcustype_t(mess, x.tag)
end

global proc pcustype_t(ichar mess, int t) =
	[256]char str

	fprint @str,"Type not supported: # : #",mess, ttname[t]
	reportpcerror(pcptr, str)
end

global proc pcmxtypes(ichar mess, variant x,y) =
	pcmxtypestt(mess,x.tag,y.tag)
end

global proc pcmxtypestt(ichar mess, int t,u) =
	[256]char str

	fprint @str, "Types not supported: # : #/#",
			mess,ttname[t],ttname[u]
	reportpcerror(pcptr, str)
end

global proc reportpcerror(pcl pcptr, ichar mess, param="")=
	variant s,send
	pcl pc
	int count
	ifile pm
	locrec loc, loc2

!CPL $LINENO
	loc:=getpcerrorpos(pcptr)
!CPL $LINENO
	pm:=loc.pm		!remember first error (written to $error.tmp)

!CPL $LINENO
	println
!	println "*********************************************************"
	println " ":"80p*"
	println "PC Error:"
	println "    ",,mess,,param
	println

	println "Line:",loc.lineno,"in Module",loc.pm.name,,".q:"

	if loc.def then
		println "In function:",loc.def.name
	fi

	s:=sptr
	send:=&varstack[1]
!CPL $LINENO

	count:=0
	while s>=send and count<5 do
		if s.tag=tretaddr then
			pc:=s.retaddr-1		!go back three to get to start of kcall/kcallptr instr
			loc2:=getpcerrorpos(pc)
			println "Called from line",loc2.lineno,"in",loc2.pm.name
			++count
		fi
		--s
	od

CPL "PC/STOPC", LOC.DEF,loc.lineno, =LOC.PM.FILESPEC

	stopcompiler(loc)
end


func getpcerrorpos(pcl pc)locrec =
!given pcptr, set up pcerrorpos, the lsw of the source pointer
!and set up pcerrormodule
	int offset, pos, soffset, moduleno
	pcl pcstart
	ref i32 pcsrcstart
	ifile pm
	locrec loc

	clear loc
	pm:=modules[findmodulefrompc(pc)]

	pcstart:=pm.pcstart
	pcsrcstart:=pm.pcsourcestart

	offset:=getpcloffset(pc,pcstart)
	pos:=(pcsrcstart+offset)^

	loc.lineno:=pos.[0..23]
	moduleno:=pos.[24..31]

	if moduleno=0 then
		MODULENO:=1; SOFFSET:=0
!		ABORTPROGRAM("GETPCPOS: no module")
	fi

	loc.pm:=modules[moduleno]
	loc.sp:=subprogs[pm.subprogno]	
	loc.def:=nil

	return loc
end

function findmodulefrompc(pcl pc)int=
!given pcptr, find which module it's currently executing
	for i to nmodules do
		if pc>=modules[i].pcstart and pc<modules[i].pcend then
			return i
		fi
	od
	println "Can't find pcptr module", pc
	cpl
	stop 1
	return 0
end


global func k_makelist(variant sp, int lower, n)variant=
	variant x,y

	x:=sp-n+1			!start of data
	sp:=x

	var_make_list(x, sp, n, lower)
	sp.objptr.mutable:=0

	sp
end

global proc k_len(variant sp)=
	object p:=sp.objptr
	int n, t

	case sp.tag
	when tlist,tarray,tdict,tbits then
		n:=p.length
	when tstring then
		n:=p.length
	when trecord, tvector, tstruct then
		n:=ttlength[p.usertag]
	when tset then
		n:=p.length
	when trange then
		n:=sp.range_upper-sp.range_lower+1
	when tdecimal then
		n:=obj_len_dec(p)
	when ttype then
		t:=sp.value
		case ttbasetype[t]
		when trecord, tvector, tstruct then
			n:=ttlength[t]
		else
			n:=ttlength[t]
		esac

	else
		pcustype("Len",sp)
	esac

	var_unshare(sp)
	sp.tagx:=tint
	sp.value:=n
end

global proc k_maths(variant sp, int opc)=
	real x

	x:=sp.xvalue

	case sp.tag
	when tint then
		case opc
		when mm_sqr then
			sp.value:=sqr(sp.value)
			return
!		when mm_sign then
!			sp.value:=sign(sp.value)
!			return
		esac
		sp.tagx:=treal
		x:=sp.value
	when treal then
	else
		pcustype("Maths:", sp)
	esac

	case opc
	when mm_sqr then x:=sqr(x)
	when mm_sqrt then x:=sqrt(x)
	when mm_sin then x:=sin(x)
	when mm_cos then x:=cos(x)
	when mm_tan then x:=tan(x)
	when mm_asin then x:=asin(x)
	when mm_acos then x:=acos(x)
	when mm_atan then x:=atan(x)
!	when mm_sign then x:=sign(x)
	when mm_log then x:=log(x)
	when mm_log10 then x:=log10(x)
	when mm_round then
		if x>=0.0 then
			x:=floor(x+0.5)
		else
			x:=ceil(x-0.5)
		fi
	when mm_floor then x:=floor(x)
	when mm_ceil then x:=ceil(x)
	else
		pcerror("Maths op:", mathsnames[opc])
	esac

	sp.xvalue:=x
end

global proc k_maths2(variant x, y, int opc)=
	unless x.tag=y.tag=treal then pcerror("maths2") end

	case opc
!	when mm_atan2 then
!		x.xvalue:=atan2(x.xvalue, y.xvalue)
!	when mm_fmod then
!		x.xvalue:=fmod(x.xvalue, y.xvalue)
	else
		pcerror("Maths2:",mathsnames[opc])
	esac

end

global proc k_lwb(variant sp)=
	object p:=sp.objptr
	int n, t

	case sp.tag
	when tlist then
		n:=p.lower16
	when tstring,tdict then
		n:=1
	when tarray, tbits then
		n:=p.lower
	when trecord,tstruct then
		n:=1
	when tvector then
		n:=ttlower[p.usertag]
	when tset then
		n:=0
	when trange then
		n:=sp.range_lower
	else
		pcustype("Lwb", sp)
	esac

	var_unshare(sp)
	sp.tagx:=tint
	sp.value:=n
end

global proc k_upb(variant sp)=
	object p:=sp.objptr
	int n, t

	case sp.tag
	when tlist then
		n:=p.length+p.lower16-1
	when tstring, tdict then
		n:=p.length
	when tarray, tbits then
		n:=p.length+p.lower-1
	when trecord, tstruct then
		n:=ttlength[p.usertag]

	when tvector then
		t:=p.usertag
		goto dotype

	when tset then
		n:=p.length-1
	when trange then
		n:=sp.range_upper
	when ttype then
		t:=sp.value
dotype:
		case ttbasetype[t]
		when tvector then
!		when tvector then
			n:=ttlength[t]+ttlower[t]-1
		else
			pcustype("t.upb",sp)
		esac

	else
		pcustype("Upb",sp)
	esac

	var_unshare(sp)
	sp.tagx:=tint
	sp.value:=n

end

global proc k_swap(variant x, y)=
	[1024]byte tempbuffer
	varrec v
	int s,t,n
	ref byte p,q
	int a

	if x.tag<>y.tag then
		pcerror("Swap mismatch")
	fi

	case x.tag
	when trefvar then
		v:=x.varptr^
		x.varptr^:=y.varptr^
		y.varptr^:=v
	when trefpack then
		s:=x.elemtag
		t:=y.elemtag
		if s<>t then goto swaperror fi
		n:=ttsize[s]
		case n
		when 1 then
			p:=x.ptr
			q:=y.ptr
			a:=p^
			p^:=q^
			q^:=a
		elsif ttsize[s]<=tempbuffer.bytes then
			memcpy(&tempbuffer,x.ptr,n)
			memcpy(x.ptr,y.ptr,n)
			memcpy(y.ptr,&tempbuffer,n)
		else
			goto swaperror
		esac

	else
swaperror:
		pcmxtypes("Swap",x,y)
	esac
end

global proc k_bounds(variant sp, int &lower, &upper)=
	int a, b, m, t
	object p

	m:=sp.tag
	p:=sp.objptr

	case m
	when tlist then
		a:=p.lower16
		b:=p.length+a-1
	when tarray, tbits then
		a:=p.lower
		b:=p.length+a-1
	when tstring, tdict then
		a:=1
		b:=p.length
	when trange then
		a:=sp.range_lower
		b:=sp.range_upper
	when tstruct,trecord then
		a:=1
		b:=ttlength[p.usertag]
	when tvector then
		t:=p.usertag
		goto dotype

	when tset then
		a:=0
		b:=p.length-1

	when ttype then
		t:=sp.value
dotype:
		case ttbasetype[t]
		when tvector, tstruct then
!		when tvector, tstruct then
			a:=ttlower[t]
			b:=ttlength[t]+a-1
		else
			pcustype("t.bounds",sp)
		esac

	else
		pcustype("Bounds",sp)
	esac

	lower:=a
	upper:=b
end

global func k_type(variant sp, int n)int =
	int t:=sp.tag

	case n
	when 0 then					!base type, eg record
	when 1 then					!user type, eg date
		case t
		when trecord, tstruct, tvector then
			t:=sp.objptr.usertag
		esac
	else						!elemtype for array/vector/pointer
		case t
		when tarray,tbits then
			t:=sp.objptr.elemtag
		when trefpack, trefvar, trefbit then
			t:=sp.elemtag
		when tset then
			t:=tu1
		when tvector then
			t:=tttarget[sp.objptr.usertag]
		else
			t:=tvoid
!			pcustype_t("elemtype",t)
		esac
	esac	

	return t
end

global proc k_dot(variant sp, ref genfieldrec g)=
	symbol d
	variant p
	ref byte q
	int rectype
	varrec v

restart:
	case sp.tag
!	when trecord, tstruct, tstruct then
	when trecord, tstruct then
	when trefvar then
!CPL "DOT REFVAR"
!PCERROR("DOT REFVAR")
		sp:=sp.varptr
		restart
	else
		pcustype("1:dot/not record",sp)
	esac
	rectype:=sp.objptr.usertag

	d:=resolvefield(g, rectype)

	case d.nameid
	when fieldid then
		p:=sp.objptr.varptr+d.fieldoffset/varsize
		var_share(p)
		var_unshare(sp)
		sp^:=p^

	when structfieldid then
		var_loadpacked(sp.objptr.ptr+d.fieldoffset, d.mode, &v, nil)
		var_unshare(sp)
		sp^:=v

    when procid then
		sp.tagx:=tsymbol
		sp.def:=d

    when linkid then
		sp.tagx:=tsymbol
		sp.def:=d.alias

	else
		pcerror("DOT: can't do this fieldtype:",namenames[d.nameid])
	esac
end

global proc k_dotref(variant sp, ref genfieldrec g)=
	symbol d
	variant p
	ref byte q
	int rectype

restart:
	case sp.tag
	when trecord, tstruct then
	when trefvar then
		sp:=sp.varptr
		restart
	else
		pcustype("2:dot/not record",sp)
	esac
	rectype:=sp.objptr.usertag

	d:=resolvefield(g, rectype)

	case d.nameid
	when fieldid then
		p:=sp.objptr.varptr+d.fieldoffset/varsize
!Possible bug when sp is a transient value which is now freed
!But you wouldn't normally use as an lvalue
		var_unshare(sp)

		sp.tagx:=trefvar
		sp.varptr:=p

	when structfieldid then
		q:=sp.objptr.ptr+d.fieldoffset
		var_unshare(sp)
		sp.tagx:=trefpack
		sp.ptr:=q
		sp.elemtag:=d.mode

	else
		pcerror("DOTREF: can't do this fieldtype:",namenames[d.nameid])
	esac
end

global func k_popdot(variant sp, ref genfieldrec g)variant =
	symbol d
	variant p,x,y

	x:=sp--
	y:=sp--

	case x.tag
	when trecord, tstruct then
	else
		pcustype("3:dot/not record",x)
	esac

	d:=resolvefield(g, x.objptr.usertag)

	IF NOT X.HASREF THEN PCERROR("POPDOT") FI

	if not x.objptr.mutable then
		pcnotmut()
	fi

	case d.nameid
	when fieldid then
		p:=x.objptr.varptr+d.fieldoffset/varsize
		var_unshare(p)
		p^:=y^				!transfer
		var_unshare(x)

	when structfieldid then
		var_storepacked(x.objptr.ptr+d.fieldoffset, y, d.mode)
		var_unshare(x)

	else
		pcerror("POPDOT: can't do this fieldtype:",namenames[d.nameid])
	esac

	return sp
end

function resolvefield(ref genfieldrec g, int rectype)symbol d=
!index is a start point in the genfieldtable
!scan the linked list looking for a field/structfield/method etc whose
!owner type matches rectype

	while g do
		d:=g.def
		if d.owner.mode=rectype then return d fi
		g:=g.nextdef
	od

	pcerror("Can't resolve field:",d.name)
	return nil
end

global proc k_convrefpack(variant sp)=
	variant a
	int tag,elemtype
	ref void p
	object pa

	case sp.tag
	when trefvar then
		a:=sp.varptr

		pa:=a.objptr
		case a.tag
		when tint,trefpack then
			p:=&a.value
			elemtype:=ti64
		when treal then
			p:=&a.value
			elemtype:=tr64
		when tarray then
			p:=pa.ptr
			elemtype:=pa.elemtag
		when tbits then
			sp.ptr:=pa.ptr
			sp.bitoffset:=pa.indexoffset*ttbitwidth[pa.elemtag]
			sp.bitlength:=0
			sp.tagx:=trefbit
			sp.elemtag:=pa.elemtag
			return

		when tset then
			sp.ptr:=pa.ptr
			sp.bitoffset:=0
			sp.bitlength:=0
			sp.tagx:=trefbit
			sp.elemtag:=tu1
			return

		when tstring then
			p:=pa.strptr
			elemtype:=tu8
			if p=nil then
				p:=""
			fi
		when tstruct then
			p:=pa.ptr
			elemtype:=pa.usertag
		when tvector then
			p:=pa.ptr
			elemtype:=pa.usertag
		when tdecimal then
			p:=pa.num
			elemtype:=ti32

		else
			pcustype("Getrefpack1",a)
		end
	when trefpack,trefbit then
		return

	else
		pcustype("Getrefpack2",sp)
	end
done:

	sp.tagx:=trefpack
	sp.ptr:=p
	sp.elemtag:=elemtype
end

global proc k_incrptr(variant p, int step)=

	case p.tag
	when trefvar then			!increment what ptr points to
		p:=p.varptr
		case p.tag
		when tint then
			p.value+:=step
		when trefvar then			!incr the pointer
			p.varptr+:=step
		when trefpack then			!incr the pointer
			p.ptr+:=ttsize[p.elemtag]*step
		when treal then
			p.xvalue+:=step
		else
			pcustype("incrptr/refvar",p)
		end
	when trefpack then			!incr the packed type pointed to
		case p.elemtag
		when tu8,ti8 then
			p.ptr^+:=step
		when tu16,ti16 then
			(ref u16(p.ptr))^+:=step
		else
			pcustype_t("incrptr/ref",p.elemtag)
		end

	else
		pcustype("incrptr",p)
	end
end

global func k_cmp(int cc, variant x, y)int res =
	case cc
	when eq_cc then
		var_equal(x, y)
	when ne_cc then
		not var_equal(x, y)
	else
		res:=var_compare(x, y)
		case cc
		when lt_cc then
			res<0
		when le_cc then
			res<=0
		when ge_cc then
			res>=0
		else
			res>0
		esac
	esac
end

global func k_bytesize(variant sp)int=
	int t
	object p:=sp.objptr

	t:=sp.tag

	case t
	when ttype then
		t:=sp.value
	when tstruct, trecord, tvector then
		t:=sp.objptr.usertag
	esac

!t is usertag for structs etc, or base tag
	case t
	when tarray then
		p.length*ttsize[p.elemtag]
	when tset then
		getbitssize(p.length,tu1)
	when tstring then
		p.length
	when tbits then
		bits_bytesize(p)
	when tlist,tdict then
		p.length*varsize
	when trecord, tstruct, tvector then
		ttsize[t]	
	when tdecimal then
		p.length*decelemsize
	else
		ttsize[t]
	esac
end

global func k_when(variant x, y)int =
!	int xt:=x.tag, yt:=y.tag

	case pr(x.tag, y.tag)
	when pr(tint, trange) then
		if x.value in y.range_lower..y.range_upper then
			1
		else
			0
		fi
	when pr(tint,tset), pr(ttype,tset) then
		var_in_set(x, y)
	else
		var_equal(x, y)
	esac
end

global function raiseexception(int exceptno)pcl =
	variant stackend, oldsptr

	stackend:=&varstack[1]
	oldsptr:=sptr
	do

		if sptr<=stackend then
			sptr:=oldsptr
			PCERROR("DEFAULT EXCEPTION")
!			default_exception(exceptno)
		fi
		if sptr.tag=texception and (exceptno=0 or sptr.exceptiontype=exceptno) then
			exit
		fi
		var_unshare(sptr)
		--sptr
	od

!found exception entry on stack; keep it there
	frameptr:=ref byte(sptr)+sptr.frameoffset
	return cast(sptr.ptr)
end

global function runproc_m(ref void amsg)int=
	varrec a,b,dest
	static int rmsg_typeno
	int i,result
	objrec obj

!CPL "RUNPROC/M"
!TESTPPVAR("RUNPROC/M")
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

!TESTPPVAR("RUNPROC/M2")
	memset(&obj,0,objrec.bytes)
	obj.refcount:=99
	obj.ptr:=ref byte(amsg)
	obj.usertag:=rmsg_typeno

	a.tagx:=tstruct ior hasrefmask
	a.objptr:=&obj

!TESTPPVAR("RUNPROC/M3")
	runproc(pcl_callbackfn,&a,nil,&dest)
!TESTPPVAR("RUNPROC/M4")
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
!Use of the stack:
! The stack as it was at the time of the callext call (or via a callback from Windows)
! is entirely unaffected. However some things will be pushed onto it here:
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
	pcl oldpcptr
	byte oldstopped
	int nparams

!STATIC INT RPLEVEL
!++RPLEVEL


!CPL "RUNPROC"
	dest.tagx:=tint
	dest.value:=0

	oldstopped:=stopped		!not really need, as it can be assumed stopped=0
	oldpcptr:=pcptr
	oldsptr:=sptr
	oldframeptr:=frameptr

	(++sptr).tagx:=999				!put in marker (this location might be checked later)

	if b and b.tag then			!must stack in reverse order: (b,a) or (a)
		nparams:=2
		(++sptr)^:=a^
		(++sptr)^:=b^
	elsif a and a.tag then
		nparams:=1
		(++sptr)^:=a^
	else
		nparams:=0
	fi
	(++sptr).tagx:=tretaddr

	sptr.retaddr:=stopseq

	sptr.frameptr_low:=int(frameptr)
	frameptr:=cast(sptr)
	pcptr:=fnptr

!CPL "**********EXEC DISPLOOP:", RPLEVEL
!IF RPLEVEL>1 THEN CPL "@"*90; OS_GETCH(); STOP FI
	disploop()
!CPL "**********DONE exec disploop", RPLEVEL
!--RPLEVEL

!stack will either point to a stop-value, with a retaddr before it,
!or to the first param (or to the proc return value).
	if (sptr-11).tag=tretaddr then		!probably stop used
		dest^:=sptr^
	else								!assume normal return used
	--SPTR
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
end

global func k_keyindex(variant sp)variant=
!x{y}
	variant d,k,p,def

	def:=sp--			!def is any default value to be used
	k:=sp--			!k is the key
	d:=sp				!d is the dict

	if d.tag<>tdict then
		pcustype("dict{}",d)
	fi

	p:=var_finddictitem(d,k,0)
	var_unshare(d)
	var_unshare(k)

	if p then			!found
		sp^:=p^
		var_unshare(def)
	else
		sp^:=def^			!use given default value when not found
	fi
	return sp
end

global func k_popkeyindex(variant sp)variant=
!y[z]:=x
	variant d,k,p,x

	k:=sp--			!k is the key
	d:=sp--			!d is the dict
	x:=sp--			!value to pop

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

	return sp
end

global func k_keyindexref(variant sp)variant=
!y[z]:=x
	variant d,k,p,x

	k:=sp--			!k is the key
	d:=sp				!d is the dict

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

	sp.tagx:=trefvar
	sp.varptr:=p

	return sp
end

global func k_map(variant sp, pcl pc, variant &newsp)pcl=

	static [10]pclrec codeseq

	int nargs

	case sp.tag
	when toperator then
		if jumptable=nil then
			codeseq[1].opcode:=sp.value
		else
!PCERROR("MAP")
			codeseq[1].labaddr:=jumptable[sp.value]
		fi

		--sp
		codeseq[2]:=(pc+1)^			!copy jump lab which follows the map op

!	when tsymbol then
!		nargs:=(pc^=int(cmdmap[kmaps]) |1|2)
!
!!I need to push 2-3 stack entries down to make room a return value slot
!		for i:=0 downto -(nargs+1) do				!0/1 or 0/1/2
!			(sp+i+1)^:=(sp+i)^
!		od
!		(sp-nargs).tagx:=tvoid
!		++sp
!
!		codeseq[1]:=cast(cmdmap[kcallptr])
!		codeseq[2]:=nargs
!		codeseq[3]:=0
!		codeseq[4]:=(pc+1)^			!copy jump lab which follows the applyop
!		codeseq[5]:=(pc+2)^			!include the dest label
!
	else
		pcerror("Apply:no op")
	esac

	newsp:=sp
	return &codeseq[1]				!pass control this short sequence
end

global proc k_maxval(variant sp)=
	i64 a

	if sp.tag=ttype then sp.tag:=sp.value fi

	case sp.tag
	when tu8 then a:=255
	when tu16 then a:=65536
	when tu32 then a:=0xFFFF'FFFF
	when tu64 then a:=0xFFFF'FFFF'FFFF'FFFF
	when ti8 then a:=127
	when ti16 then a:=32767
	when ti32 then a:=0x7FFF'FFFF
	when ti64,tint then a:=0x7FFF'FFFF'FFFF'FFFF
	else
		pcustype("MAXVALUE",sp)
	esac
	sp.tagx:=tint
	sp.value:=a

	++pcptr

end

global proc k_minval(variant sp)=
	i64 a

	if sp.tag=ttype then sp.tag:=sp.value fi

	case sp.tag
!	when tword,tu8,tu16,tu32,tu64 then a:=0
	when tu8,tu16,tu32,tu64 then a:=0
	when ti8 then a:=-128
	when ti16 then a:=-32768
	when ti32 then a:=-0x8000'0000
	when tint,ti64 then a:=-0x8000'0000'0000'0000
!	when tbignum then a:=-0x8000'0000'0000'0000
	else
		pcustype("MINVALUE",sp)
	esac
	sp.tagx:=tint
	sp.value:=a

	++pcptr
end

=== qq_sets.m 0 0 25/48 ===
global proc obj_free_set(object p)=
	if p.length then
		pcm_free(p.ptr, getbitssize(p.alloc64, tu1))
	fi
	pcm_free32(p)
end

global proc var_dupl_set(variant a)=
	object p:=a.objptr
	object q
	int nbytes, nbits:=p.length

	q:=obj_newset(nbits)	

	if nbits then
		memcpy(q.ptr, p.ptr, getbitssize(nbits, tu1))
	fi

	a.objptr:=q
end

global function var_equal_set(variant x,y)int=
	int xbytes:=getsetbytes(x)
	int ybytes:=getsetbytes(y)
	if xbytes<>ybytes then return 0 fi

	return eqbytes(x.objptr.ptr, y.objptr.ptr, xbytes)
end

function getsetbytes(variant x)int=
	int nbits:=x.objptr.length
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
			if a<0 then
				a:=-a-1
				if a>top then
					top:=a
				fi
				nextloop
			fi
			b:=a
		
		else			!assume numeric value of some sort
			b:=a:=var_getintvalue(q)

		end switch
		if a<0 or b<0 then
			pcerror("Neg range element")
		fi

		top max:=a
		top max:=b
		++q
	od

!CPL "MS1",=N,TOP
	s:=obj_newset(top+1)
!CPL "MS2",N

!Second scan to store elements
	q:=data
	to n do
!CPL "LOOP"
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
			if a<0 then nextloop end		!ignore set-length control
		else
			b:=a:=var_getintvalue(q)
		end switch

!CPL "LOOP",A,B
!		for j:=a to b do
!			setelem(cast(s.ptr),j)
!		od
		setelemblock(cast(s.ptr),a,b)
		++q
	od
!CPL "MS3"

	var_objtovar(tset,s,dest)
end

global function obj_newset(int length)object p=
!create a packed array with element-type t, given length and lower bound.
!it will be initialised to zeros

	ref byte q
	int nbits,nbytes

	p:=obj_new()
	p.mutable:=1
	p.length:=length

	nbytes := ((length-1)/64+1)*8		!bytes required in 64-bit blocks

	if length then
		p.ptr := pcm_alloc(nbytes)              !(turns total allocated in 'allocbytes')
		p.alloc64:=u64(allocbytes)*8
		pcm_clearmem(p.ptr,allocbytes)
	else
		p.ptr:=nil
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

	if u64(index)>=u64(p.length) then
		pcerror("set[int] bounds")
	fi

	a.tagx:=tint
	a.value:=not not ((p.ptr+index>>3)^ iand (1<<(index iand 7)))
end

global proc var_putix_set(variant a, int index, variant x)=
!a[index]:=x
	object p
	ref byte q
	int newoffset

	p:=a.objptr
	if not p.mutable then pcnotmut() fi

	if u64(index)>=u64(p.length) then
		if index<0 then
			pcerror("lwb")
		else
			pcerror("set[i]:=x bounds")
		fi
	fi

	q:=getoffset(p.ptr, index, newoffset)

	var_storebit(q, newoffset,x,tu1,0)
end

global proc var_getixref_set(variant a, int index)=
!a[index]:=x
	varrec v
	object p
	ref byte q
	int offset, newoffset

	p:=a.objptr
	if not p.mutable then pcnotmut() fi

	if u64(index)>=u64(p.length) then
		pcerror("&set[i] bounds")
	fi

	q:=getoffset(p.ptr, index,  newoffset)

	a.tagx:=trefbit
	a.elemtag:=tu1
	a.ptr:=q
	a.bitoffset:=newoffset
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

	if u64(i)>=u64(p.length) then
		return 0
	fi

	if	(p.ptr+i>>3)^ iand masks[i iand 7] then
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

	if pp.length>=n then		!already large enough
		return
	fi

	obj_resize_set(pp,n)
end

global proc obj_resize_set(object p,int n)=
	ref byte q
	int newsize,elemtype

	elemtype:=p.elemtag

	if n<=p.alloc64 then
		p.length:=n
	else
!CPL "RESIZE"
		newsize:=getbitssize(n,tu1)
		q:=pcm_allocz(newsize)
		if p.length then
			memcpy(q,p.ptr, getbitssize(p.length,tu1))
			pcm_free(p.ptr, getbitssize(p.alloc64, tu1))
		fi
		p.ptr:=q
		p.length:=n
		p.alloc64:=allocbytes*8
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

global proc inotsetbits(ref word p,int n)=
	to (n-1)/64+1 do
		p^ :=inot p^
		++p
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

	xlen:=px.length
	ylen:=py.length

	if ylen=0 then			!return x unchanged
	elsif xlen=0 then		!return y
		x^:=y^
		var_dupl_set(x)
	else
		px:=x.objptr

		iresizeset(x,ylen)		!make sure x is at least as big as y

		iorsetbits(cast(px.ptr),cast(py.ptr),ylen)

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

	xlen:=px.length
	ylen:=py.length

	if ylen=0 then				!return empty set
		var_emptyset(x)
	elsif xlen=0 then			!return x unchanged
	else						!x iand:= y
		px:=x.objptr

		iresizeset(x,ylen)		!make sure x is at least as big as y

		iandsetbits(cast(px.ptr),cast(py.ptr),ylen)
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

	xlen:=px.length
	ylen:=py.length

	if ylen=0 then				!return x unchanged
		var_emptyset(x)
	elsif xlen=0 then			!return y
		x^:=y^
		var_dupl_set(x)
	else						!x iand:= y
		px:=x.objptr

		iresizeset(x,ylen)		!make sure x is at least as big as y

		ixorsetbits(cast(px.ptr),cast(py.ptr),ylen)
	fi
end

global proc var_inotto_set(variant x) =
!inot:=x
	int xlen
	object px,py

	px:=x.objptr

	xlen:=px.length

	if xlen then				!lease return x unchanged as []
		inotsetbits(cast(px.ptr),xlen)
	fi
end

=== qq_strings.m 0 0 26/48 ===
global object emptystring

proc start=
	emptystring:=obj_new()
	emptystring.refcount:=1
	emptystring.objtype:=normal_obj
end

global proc var_empty_string(variant dest, int mutable=0)=
	dest.tagx:=tstring ior hasrefmask
	if not mutable then
		dest.objptr:=emptystring
		++emptystring.refcount
	else
		dest.objptr:=obj_make_stringn(nil, 0,1)
	fi
end

global proc var_make_string(ichar s, variant dest, int mutable=0)=
!CPL "MAKESTR"
	dest.tagx:=tstring ior hasrefmask
	dest.objptr:=obj_make_string(s,mutable)
end

global proc var_make_stringn(ichar s, int length, variant dest, int mutable=0)=
!CPL "MAKESTRN",LENGTH
	dest.tagx:=tstring ior hasrefmask
	dest.objptr:=obj_make_stringn(s,length,mutable)
end

global function obj_new_string(int n)object p=
	p:=obj_new()

!CPL "NEWSTR:",=P,=N
	p.mutable:=1
	p.length:=n
	p.objtype:=normal_obj

	if n then
		p.strptr:=pcm_alloc(n)
		p.alloc64:=allocbytes
	fi

	return p
end

global function obj_make_string(ichar s,int mutable=0)object p=
	int n

	p:=obj_new_string(n:=strlen(s))
	p.mutable:=mutable

	if n then
		memcpy(p.strptr,s,n)
	fi
	return p
end

global function obj_make_stringn(ichar s,int length,mutable=0)object p=
!when s=nil, then the string data is not initialised

	p:=obj_new_string(length)
	p.mutable:=mutable

	if length then
		if s then
			memcpy(p.strptr,s,length)
		else
			memset(p.strptr,0,length)
		fi

	fi
	return p
end

global proc obj_free_string(object p)=
	if p.length then
		pcm_free(p.strptr,p.alloc64)
	fi

	pcm_free32(p)
end

global proc var_dupl_string(variant a)=
	object p,q

	p:=a.objptr
	q:=obj_new_string(p.length)
	a.objptr:=q

	if q.length then
		memcpy(q.strptr,p.strptr,q.length)
	fi
end

global proc var_getix_string(variant a, int index)=
!put result into a (which will be on the stack)
	object q

	q:=a.objptr

	if word(index-1)>=word(q.length) then
		pcerror("getstring[int] bounds")
	fi

!	var_unshareu(a)
	stringslice(a,index,index,a)
end

global proc var_getixref_string(variant a, int index)=
!put result into a (which will be on the stack)
	object q

	q:=a.objptr

	if word(index-1)>=word(q.length) then
		pcerror("getixref[int] bounds")
	fi

	a.tagx:=trefpack
	a.elemtag:=tu8
	a.ptr:=cast(q.strptr+index-1)
end

global proc var_getdotix_string(variant a, int index)=
!put result into a (which will be on the stack)
	object q

	q:=a.objptr

	if word(index-1)>=word(q.length) then
		pcerror("x.[] bounds")
	fi

	a.tagx:=tint
	a.value:=(q.strptr+index-1)^
end

global proc var_getdotixref_string(variant a, int index,variant dest)=
	object q

	q:=a.objptr

	--index

	if word(index)>=word(q.length) then
		pcerror("x.[] bounds")
	fi

	dest.tagx:=trefpack
	dest.elemtag:=tu8
	dest.ptr:=q.strptr+index
end

global proc var_getslice_string(variant a, int i,j)=
	object p:=a.objptr

	if i<1 or j>p.length or i>j then
		pcerror("string/slice bounds")
	fi

	stringslice(a,i,j,a)
end

proc stringslice(variant a, int i,j, variant dest)=
	object p,q

	p:=a.objptr

	q:=obj_new()
	q.mutable:=p.mutable
	q.length:=j-i+1
	q.objtype:=slice_obj

	case p.objtype
	when slice_obj then				!slice of a slice!
		q.objptr2:=p.objptr2		!link to original
		++q.objptr2.refcount
	when extslice_obj then
		q.objptr2:=nil
		q.objtype:=extslice_obj
	else
		++p.refcount
		q.objptr2:=p				!link to original
	esac
	q.strptr:=p.strptr+i-1

	dest.tagx:=a.tagx
	dest.objptr:=q
end

global proc var_putix_string(variant a, int index, variant x)=
	ichar s
	object p,q
	int length

	p:=a.objptr
	if not p.mutable then pcnotmut() fi
	length:=p.length

	if index not in 1..length then
		if index=length+1 then
			var_addto_string(a,x)
			return
		else
			pcerror("putstring[int] bounds")
		fi
	fi

	s:=p.strptr+index-1
	if x.tag<>tstring then
		pcerror("s[i]:= not str")
	fi
	q:=x.objptr
	if q.length=0 then pcerror("s[i]:=""""") fi
	s^:=q.strptr^
end

global proc var_putslice_string(variant a, int i,j, variant x)=
!insert a substring into a
	ichar s
	object p,q
	int length,sublength

	p:=a.objptr
	if not p.mutable then pcnotmut() fi
	length:=p.length

	if i<1 or j>p.length or i>j then
		pcerror("string/slice bounds")
	fi
	sublength:=j-i+1

	s:=p.strptr+i-1
	if x.tag<>tstring then
		pcerror("s[i..j]:= not str")
	fi
	q:=x.objptr
	if q.length<sublength then
		pcerror("substr too short")
	fi
	memcpy(s,q.strptr, sublength)
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
	if not p.mutable then pcnotmut() fi
	length:=p.length

	if index not in 1..length then
		if index=length+1 then
			var_addto_string_ch(a,ch)
			return
		else
			pcerror("str.[int] bounds")
		fi
	fi

	(p.strptr+index-1)^:=ch
end

global proc obj_resize_string(object p,int n)=
	ichar s
	int oldalloc

	if n<=p.alloc64 then
		p.length:=n
	else
		oldalloc:=p.alloc64
		s:=pcm_alloc(n)
		p.alloc64:=allocbytes
		if p.length then
			memcpy(s,p.strptr,p.length)

			pcm_free(p.strptr, oldalloc)
		fi

		p.strptr:=s
		p.length:=n
	fi
end

global proc var_add_string(variant a,b)=
!a':=a+b; original a is preserved, just that new result is into a
	object p:=a.objptr
	object q:=b.objptr
	object r

	int alen:=p.length
	int blen:=q.length
	int newlen

	if blen=0 then
		var_shareu(a)
		return
	elsif alen=0 then
		var_make_stringn(q.strptr,blen,a,1)
		return
	fi

	newlen:=alen+blen
	r:=obj_new_string(newlen)
	memcpy(r.strptr, p.strptr, alen)
	memcpy(r.strptr+alen, q.strptr, blen)

	a.objptr:=r
end

global proc var_addto_string(variant a,b)=
!a+:=b; inplace add
!a is normally subject of a refvar, so not shared

	object p:=a.objptr
	object q:=b.objptr

!CPL "ADDTO/STR"

	int alen:=p.length
	int blen:=q.length
	int newlen

	if not p.mutable then
		pcnotmut()
	fi

	if blen=0 then
		return
	elsif alen=0 then			!copy b over and share
		var_unshareu(a)
		a^:=b^
		var_duplu(a)
		return
	fi

!CP "AS "
	newlen:=alen+blen
	obj_resize_string(p,newlen)
	memcpy(p.strptr+alen, q.strptr, blen)
end

global proc var_addto_string_ch(variant a,int ch)=
!a+:=ch; inplace add
!a is normally subject of a refvar, so not shared
	object p:=a.objptr
	int alen:=p.length, n
	[32]char str
	ichar s

	if not p.mutable then
		pcnotmut()
	FI

	obj_resize_string(p,alen+1)
	(p.strptr+alen)^:=ch
end

!global function u64tostr(u64 aa,ref char s,word base,int sep)int =		!U64TOSTR

global function var_equal_string(variant x,y)int =
!return 1 if strings in x,y are equal, otherwise 0
	int n,res
	object px,py

	px:=x.objptr
	py:=y.objptr
	if px=py then return 1 fi

	n:=px.length

	if n<>py.length then
		0				!unequal lengths
	elsif n=0 then
		1				!same zero length
	else
!		res:=cmpstringn(px.strptr,py.strptr,n)=0
		eqbytes(px.strptr,py.strptr,n)
	fi
end

global function var_compare_string(variant x,y)int =
!return -1/0/+1
	int res
	object px,py

	px:=x.objptr
	py:=y.objptr

	res:=cmpstring_len(px.strptr, py.strptr, px.length, py.length)
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
			return cmpstring(convtostringz(s,slen),convtostringz(t,tlen))
		fi
	fi
end

global function var_inx_string(variant x,y)int =
!return start index of string x in y, or 0
	int xlen,ylen,result,i,j,k
	ref char sx, sy
	object px,py

	px:=x.objptr
	py:=y.objptr

	xlen:=px.length
	ylen:=py.length

	if xlen=0 or ylen=0 then		!at least one is empty
		return i64.min
	fi

	k:=ylen-xlen
	for i:=0 to k do			!all start positions
		sx:=px.strptr
		sy:=py.strptr+i
		for j:=1 to xlen do			!all chars in y
			if sx^<>sy^  then
				goto nextpos
			fi
			++sx; ++sy
		od
		return i+1
nextpos:
	od
	return i64.min
!	return 0
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
		n:=pa.length			!default is the whole length of the string
	fi

	if a.tag<>tstring then
		pcerror("convcase/notstr")
	fi

	if n<0 then
		pcerror("CONVCASE N<0")
	fi

	if n=0 then
		return
	fi

	if n>pa.length then
	cpl =n,pa.length
		pcerror("convcase/N?")
	fi
	s:=pa.strptr

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

global proc var_makestrslicexobj(ichar s, int length, variant dest)=
	dest.tagx:=tstring ior hasrefmask
	dest.objptr:=obj_make_strslicexobj(s,length)
end

global function obj_make_strslicexobj(ichar s, int length)object=
!s is an existing non-allocated or external string
!create a special string slice object, which for now has the format:
! .objtype=extslice, but .objptr2=0
!length can be 0, then s can be nil or ""
!
	object p

	if length=0 then s:=nil fi

	p:=obj_new()
	p.strptr:=s
	p.mutable:=1
	p.length:=length
	p.objtype:=extslice_obj		!.objptr2 will be zero
	return p
end

function var_asc(variant a)int=
	object p
	if a.tag<>tstring then pcerror("Asc:not str") fi
	p:=a.objptr
	if p.length<1 then pcerror("Asc:empty") fi

	return p.strptr^
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
		memset(dest.objptr.strptr,ch,length)
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

	else				!multiply non-null string by m
		pa:=a.objptr
		oldlen:=pa.length
		if oldlen then			!not empty string
			newlen:=oldlen*m

			v.objptr:=obj_new_string(newlen)
			v.tagx:=tstring ior hasrefmask

			p:=v.objptr.strptr
			if oldlen=1 then
				memset(p,pa.strptr^,m)
			else
				to m do
					memcpy(p,pa.strptr,oldlen)
					p+:=oldlen
				od
			fi
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
	int length:=p.length
	ichar s

	var_make_list(nil, dest, length, 1)
	q:=dest.objptr.varptr
	s:=p.strptr

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

	p:=a.objptr
	b:=dest
	s:=p.strptr
	n:=1

	to m do
		if n>p.length then
			var_empty_string(dest)
		else
			var_make_stringn(s,1, dest,1)
			++s
		fi
		++n
		--+dest
	od
end

global proc var_makechar(int ch,variant dest)=
	varrec v
	[8]char str
	object p

	if ch not in 0..255 then
		pcerror("chr range")
	fi

	p:=chrtable[ch]
	if p=nil then			!create single-char constant
		str[1]:=ch
		str[2]:=0
		var_make_stringn(str,1,&v,0)
				chrtable[ch]:=p:=v.objptr
	fi
	++p.refcount

	dest.tagx:=tstring ior hasrefmask
	dest.objptr:=p
end
=== qq_syslibs.m 0 0 27/48 ===
global const fsyslibs = 1
!global const fsyslibs = 0

global tabledata []ichar syslibnames,[]ichar libtext =
	("syswin.q",		(fsyslibs | strinclude "syswin.q" | "" )),
	("syslin.q",		(fsyslibs | strinclude "syslin.q" | "" )),
	("sysp.q",			(fsyslibs | strinclude "sysp.q" | "" )),
	("windows.q",		(fsyslibs | strinclude "windows.q" | "" )),
	("linux.q",			(fsyslibs | strinclude "linux.q" | "" )),
	("clibp.q",			(fsyslibs | strinclude "clibp.q" | "" )),
	("smlib.q",			(fsyslibs | strinclude "smlib.q" | "" )),
	("winapi.q",		(fsyslibs | strinclude "winapi.q" | "" )),
	("gxlib.q",			(fsyslibs | strinclude "gxlib.q" | "" )),
	("bmlib.q",			(fsyslibs | strinclude "bmlib.q" | "" )),
	("console.q",		(fsyslibs | strinclude "console.q" | "" )),
	("lincon.q",		(fsyslibs | strinclude "lincon.q" | "" )),
	("winconsts.q",		(fsyslibs | strinclude "winconsts.q" | "" )),
	("wingxlib.q",		(fsyslibs | strinclude "wingxlib.q" | "" )),
	("winmessages.q",	(fsyslibs | strinclude "winmessages.q" | "" )),
	("gxmisc.q",		(fsyslibs | strinclude "gxmisc.q" | "" )),
	("dates.q",			(fsyslibs | strinclude "dates.q" | "" )),
end

![syslibnames.len]byte syslibfileno

GLOBAL function findsyslib(ichar filename)ichar=
!filename must be module name with .q extension
!return source code, or nil

	for i to syslibnames.len do
!CPL "FINDSYSLIB",I,FILENAME, SYSLIBNAMES[I]
		if eqstring(filename, syslibnames[i]) then
			return libtext[i]
		fi
	od

	nil
end

global func loadsysmodule(ifile pm)int=
!load the syslib specified within pm, then load it and fully populate pm
!return 0 if it fails
!for internal syslibs, the filename will have no path, but should have an extension
	ichar source

	source:=findsyslib(pm.filespec)

	if source then
		pm.text:=source
		pm.size:=strlen(source)
		return 1
	else
		return 0
	fi
end
=== qq_tables.m 0 0 28/48 ===
!!---
global enumdata	[0:]ichar stdtypenames,
					[0:]byte stdtypewidths =
!                   names       widths  V P M F
	(tvoid=0,		"void",			0),		! - means object is unassigned

! V Variant Types
	(tint,			"int",			64),	! - 64-bit signed int
	(treal,			"real",			64),	! - 64-bit float

	(tdecimal,		"decimal",		0),
	(trange,		"range",		64),	!
	(tset,			"set",			0),		! - Pascal-like bit-set (array of B)
	(tdict,			"dict",			0),		! - Dictionary of X:Y keys and values

	(tvector,		"vector",		0),		! - User-defined array
	(tbits,			"bits",			0),		! - Array of B

	(tstring,		"string",		0),		! - String of u8 elements
	(tlist,			"list",			0),		! - List of V
	(tarray,		"array",		0),		! - Array of T

	(trecord,		"record",		0),		! - Record of V
	(tstruct,		"struct",		0),		! - Record of T (contains tpackrecord instance)

	(trefvar,		"refvar",		64),	! - Pointer to V
	(trefbit,		"refbit",		128),	! - Pointer to B or bitfield
	(trefpack,		"refpack",		64),	! - Pounter to T

	(tsymbol,		"symbol",		64),	! - Named object
!	(tclosure,		"closure",		0),		! - Closure

	(ttype,			"type",			64),	! - Represents a type-code
	(toperator,		"operator",		64),	! - Represents an operator (as a bytecode op)
	(tretaddr,		"retaddr",		0),		! - Return address descriptor, only on stack 
	(texception,	"except",		0),		! - Exception descriptor, only on stack
	(tnumber,		"number",		0),		! - Only used with .istype


! T Pack Types
	(ti8,			"i8",			8),		! - Narrow numeric types
	(ti16,			"i16",			16),
	(ti32,			"i32",			32),
	(ti64,			"i64",			64),
	(tu8,			"u8",			8),
	(tu16,			"u16",			16),
	(tu32,			"u32",			32),
	(tu64,			"u64",			64),
	(tr32,			"r32",			32),
	(tr64,			"r64",			64),

	(tu1,			"u1",			1),
	(tu2,			"u2",			2),
	(tu4,			"u4",			4),

	(tpackstrc,		"packstrc",		0),		! - counted string field (uses get/setfs) (placeholder)
	(tpackstrz,		"packstrz",		0),		! - zero-terminated string field (placeholder)

	(tstringz,		"stringz",		64),	! - Pointer to zero-terminated string
	(trefproc,		"refproc",		64),	! - Pointer to native function (placeholder)

	(tslice,		"slice",		0),		! - slice of T (placeholder)

end

global const tlast=stdtypenames.upb

global const tlastvartag	= trefbit

!these codes are used in the comments
! o		pcl opcode for jbin, jbinto, junary, jproperty etc
! m		mm-code for jmaths/2
! n		for incr-ops, 0/1 means incr/decr
! n		for jisvoid/jin, normally 0, but 1 reverses to 'not isvoid/in'
! n		for FOR, 0/1 means up/down
! cc	for cmp, is a condition code, eq_cc/lt_cc etc
! cv	for jcvattr, is a compiler-var code

global enumdata [0:]ichar jtagnames,			! "jadd" etc
					[0:]byte jflags,			! 0/1/2 = 0, 1 or 2 subtrees
					[0:]byte jhasvalue = 		! whether yields a value (0, 1 or 2=special)

	(jnone = 0,		$,	0,	0),

	(jlabeldef,		$,	1,	0),

	(jassign,		$,	2,	2),			!n=0/1 for assign/deepcopy
	(jkeyword,		$,	2,	1),
	(jkeyvalue,		$,	2,	1),
	(joperator,		$,	0,	1),

	(jblock,		$,	1,	2),
	(jif,			$,	2,	2),
	(jselect,		$,	2,	2),
	(jwhenthen,		$,	2,	0),
	(jcase,			$,	2,	2),
	(jdocase,		$,	2,	0),
	(jswitch,		$,	2,	2),
	(jdoswitch,		$,	2,	0),
	(jrecase,		$,	1,	0),
	(jfor,			$,	2,	0),		! n=0/1 for up/down
	(jforx,			$,	2,	0),		! n=0/1
	(jforall,		$,	2,	0),
	(jforeach,		$,	2,	0),
	(jdo,			$,	1,	0),
	(jto,			$,	2,	0),
	(jwhile,		$,	2,	0),
	(jrepeat,		$,	2,	0),
	(jtry,			$,	2,	0),
	(jexcept,		$,	2,	0),
	(jraise,		$,	1,	0),
	(jcall,			$,	2,	1),
	(jcallhost,		$,	1,	1),
	(jnil,			$,	0,	1),
	(jswap,			$,	2,	0),
	(jgoto,			$,	1,	0),
	(jstop,			$,	1,	0),
	(jreturn,		$,	1,	2),
	(jeval,			$,	1,	0),

	(jtypeconst,	$,	0,	1),
	(jconvert,		$,	1,	1),
	(jtypepun,		$,	1,	1),
	(jmap,			$,	2,	1),
	(jcmpchain,		$,	1,	1),
	(jname,			$,	0,	1),
	(jsymbol,		$,	1,	1),

	(jintconst,		$,	0,	1),
	(jrealconst,	$,	0,	1),
	(jstringconst,	$,	0,	1),
	(jdecimal,		$,	0,	1),

	(jstrinclude,	$,	1,	1),
	(jdot,			$,	2,	1),
	(jindex,		$,	2,	1),
	(jdotindex,		$,	2,	1),
	(jkeyindex,		$,	2,	1),
	(jloop,			$,	2,	0),		!loopcode = loop_exit etc
	(jptr,			$,	1,	1),
	(jaddrof,		$,	1,	1),		!n=0/1 for &/^
	(jvoid,			$,	0,	1),		!value of 'void'

	(jprint,		$,	2,	0),		!n = set of pr_newline etc
	(jfprint,		$,	2,	0),
	(jnogap,		$,	0,	0),
	(jspace,		$,	0,	0),
	(jfmtitem,		$,	2,	0),
	(jread,			$,	2,	0),

	(jincrload,		$,	1,	1),		!n=0/1 for incr/decr
	(jloadincr,		$,	1,	1),		!n=0/1

	(junary,		$,	1,	1),		!opc is pcl op
	(jbin,			$,	2,	1),		!opc is pcl op

	(jmaths,		$,	1,	1),		!m is maths op
	(jmaths2,		$,	2,	1),		!m is maths op
	(jproperty,		$,	1,	1),		!opc is pcl op

	(jbounds,		$,	1,	1),		!n is 0/1 for range/2-vals

	(jgettype,		$,	1,	1),		!n is 0/1/2 for basetype/type/elemtype
	(jistype,		$,	1,	1),		!t is typecode to match
	(jisvoid,		$,	1,	1),		!n=0/1 for isvoid/isdef

	(jcmp,			$,	2,	1),		!cc is condcode
	(jandl,			$,	2,	1),
	(jorl,			$,	2,	1),
	(jnotl,			$,	1,	1),
	(jistruel,		$,	1,	1),
	(jin,			$,	2,	1),		!n=0/1 for in/not in
	(jinx,			$,	2,	1),
	(junaryto,		$,	1,	1),		!opc is pcl opl (kneg etc)
	(jbinto,		$,	2,	0),		!opc is pcl opl (kadd etc)
	(jandlto,		$,	2,	0),
	(jorlto,		$,	2,	0),
	(jnotlto,		$,	1,	0),
	(jistruelto,	$,	1,	0),
	(jappendto,		$,	2,	0),
	(jconcatto,		$,	2,	0),
	(jidivrem,		$,	0,	2),
	(jmakerange,	$,	2,	1),
	(jmakelist,		$,	1,	1),
	(jmakeset,		$,	1,	1),
	(jmakedict,		$,	1,	1),
	(jcvattr,		$,	0,	1),		!cv is compiler var code
end

global enumdata []u64 symbolnames=
!First half are basic tokens returned by lexreadtoken()
	(errorsym,			'error'),		! Lex error
	(dotsym,			'dot'),		! "."
	(commasym,			'comma'),		! ","
	(semisym,			'semi'),		! ";"
	(colonsym,			'colon'),		! ":"
	(assignsym,			'assign'),		! :=
	(sendtosym,			'sendto'),		! =>
	(pipesym,			'pipe'),		! ->
	(lbracksym,			'lbrack'),		! (
	(rbracksym,			'rbrack'),		! )
	(lsqsym,			'lsq'),		! [
	(rsqsym,			'rsq'),		! ]
	(lcurlysym,			'lcurly'),		! {
	(rcurlysym,			'rcurly'),		! }
	(ptrsym,			'ptr'),		! ^
	(barsym,			'bar'),		! |
	(atsym,				'at'),		! @
	(questionsym,		'question'),		! ?
	(addrsym,			'addr'),		! &
	(rangesym,			'range'),		! ..
	(ellipsissym,		'ellipsis'),		! ...

	(addsym,			'add'),		! +
	(subsym,			'sub'),		! -
	(mulsym,			'mul'),		! *
	(divsym,			'div'),		! /
	(idivsym,			'idiv'),		! %
	(iremsym,			'irem'),		! rem
	(idivremsym,		'idivrem'),		! divrem
	(andlsym,			'andl'),		! and
	(orlsym,			'orl'),		! or
	(iandsym,			'iand'),		! iand
	(iorsym,			'ior'),		! ior
	(ixorsym,			'ixor'),		! xor
	(shlsym,			'shl'),		! <<
	(shrsym,			'shr'),		! >>

	(minmaxsym,			'minmax'),		! min/max
	(appendsym,			'append'),		! append
	(concatsym,			'concat'),		! concat
	(insym,				'in'),		! in
	(inxsym,			'inx'),		! inx
	(powersym,			'power'),		! **
	(samesym,			'same'),		! ==

	(eqsym,				'eq'),		! =
	(nesym,				'ne'),		! <>
	(ltsym,				'lt'),		! <
	(lesym,				'le'),		! <=
	(gesym,				'ge'),		! >=
	(gtsym,				'gt'),		! >

	(notlsym,			'notl'),		! not
	(inotsym,			'inot'),		! inot
	(istruelsym,		'istruel'),		! istrue
	(abssym,			'abs'),		! abs
	(ascsym,			'asc'),		! asc
	(chrsym,			'chr'),		! chr

	(mathssym,			'maths'),		! sin etc
	(maths2sym,			'maths2'),		! atan2 etc
	(propsym,			'prop'),		! len etc
	(istypesym,			'istype'),		! .isint etc
	(miscpropsym,		'isvoid'),		! .isvoid/.elemtype etc

	(incrsym,			'incr'),		! -

	(eolsym,			'eol'),		! End of line
	(eofsym,			'eof'),		! Eof seen
	(intconstsym,		'intconst'),		! 123 32 bits signed
	(decimalconstsym,	'decconst'),		! 123 or 123.4 decimal
	(realconstsym,		'fpconst'),		! 123.4 64 bits
	(charconstsym,		'chrconst'),		! 'A' or 'ABCD'
	(stringconstsym,	'strconst'),		! "ABC"

!Second half are tokens that can be yielded after a name lookup:
	(unitnamesym,		'unitname'),		! 
	(namesym,			'name'),		! identifier symbol

	(stdtypesym,		'stdtype'),		! INT, CHAR etc
	(kicharsym,			'ichar'),		! ICHAR
	(kifsym,			'if'),		! 
	(kthensym,			'then'),		! 
	(kelsifsym,			'elsif'),		! 
	(kelsesym,			'else'),		! 
	(kelsecasesym,		'elsecase'),		! 
	(kelseswitchsym,	'elsesw'),		! 
	(kelseselectsym,	'elsesel'),		! 
	(kendsym,			'end'),		! 
	(kunlesssym,		'unless'),		! 
	(kcasesym,			'case'),		! CASE/DOCASE
	(krecasesym,		'recase'),		! RECASE
	(kwhensym,			'when'),		! 
	(kforsym,			'for'),		! 
	(ktosym,			'to'),		! TO/DOWNTO
	(kbysym,			'by'),		! 
	(kdosym,			'do'),		! 
	(kwhilesym,			'while'),		! 
	(krepeatsym,		'repeat'),		! 
	(kuntilsym,			'until'),		! 
	(kreturnsym,		'return'),		! 
	(kstopsym,			'stop'),		! 
	(kloopsym,			'loop'),		! EXIT/NEXT/LOOP/REDO/RESTART
	(kgotosym,			'goto'),		! GO/GOTO
	(kswitchsym,		'switch'),		! SWITCH/DOSWITCH
	(kprintsym,			'print'),		! PRINT/PRINTLN/FPRINT/FPRINTLN
	(ksprintsym,		'sprint'),		! SPRINT/SFPRINT
	(kreadsym,			'read'),		! READ/READLN
	(kprocsym,			'proc'),		! PROC
	(kfunctionsym,		'func'),		! FUNCTION
	(klabelsym,			'label'),		! LABEL
	(krecordsym,		'record'),		! RECORD
	(kstructsym,		'struct'),		! STRUCT
	(kunionsym,			'union'),		! UNION
	(kmodulesym,		'module'),		!
	(kimportsym,		'import'),		!
	(kimportdllsym,		'importd'),		! IMPORTDLL
	(ktypesym,			'type'),		! TYPE
	(krefsym,			'ref'),		! REF
	(kvarsym,			'var'),		! VAR
	(kmacrosym,			'macro'),		! MACRO
	(koperatorsym,		'op'),		! OPERATOR
	(kconstsym,			'const'),		! 
	(kglobalsym,		'global'),		! global
	(kstaticsym,		'static'),		! STATIC
	(kcalignedsym,		'calign'),		! $CALIGNED

	(ktrysym,			'try'),		! 
	(kexceptsym,		'except'),		! 
	(kraisesym,			'raise'),		! 
	(kcastsym,			'cast'),		! CAST
	(compilervarsym,	'compvar'),		! $lineno etc
	(dollarsym,			'dollar'),		! to be used for current array upperbound; also tabledata names
	(kevalsym,			'eval'),		! EVAL
	(ktabledatasym,		'tabdata'),		! tabledata
	(kmapsym,			'map'),		! MAP
	(kclampsym,			'clamp'),		! CLAMP
	(kswapsym,			'swap'),		! SWAP
	(sysconstsym,		'sysconst'),		! nil, etc
	(khostfnsym,		'hostfn'),		! LEFT, CONVLC etc
	(knilsym,			'nil'),		! NIL/PNIL
	(kstrincludesym,	'strincl'),		! STRINCLUDE
	(specialopsym,		'specop'),		! $NEG, $INDEX etc
end

global enumdata =
	pi_const,
	tab_const,
	con_const,
	true_const,
	false_const
end

global enumdata =
	thousand_unit,
	million_unit,
	billion_unit
end

global enumdata [0:]ichar namenames =
	(genericid=0,	$),		! - 		Generic name, not yet resolved
	(programid,		$),		!
	(subprogid,		$),		!
	(moduleid,		$),		!
	(dllmoduleid,	$),		!
	(procid,		$),		!sub/fun/method/op name
	(anonprocid,	$),		!closure
	(dllprocid,		$),		!
	(dllvarid,		$),		!
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
	(linkid,		$),		!
	(macroid,		$),		!
	(macroparamid,	$),		!
	(structblockid,	$),		! pseudo names used
	(unionblockid,	$),		!
	(endblockid,	$),		!
end

global enumdata [0:]ichar objtypenames =
	(normal_obj=0,	$),
	(slice_obj,		$),
	(extslice_obj,	$)
end

global enumdata [0:]ichar scopenames=
	(local_scope=0,		$), ! 		!module
	(global_scope,		$), ! 		!global/inter-module
	(export_scope,		$), ! 		!export/inter-subprog
end

global tabledata []ichar stnames, []byte stsymbols, []byte stsubcodes=

	("if",			kifsym,			0),
	("then",		kthensym,		0),
	("elsif",		kelsifsym,		jif),
	("else",		kelsesym,		0),
	("elsecase",	kelsecasesym,	jcase),
	("elseswitch",	kelseswitchsym,	jswitch),
	("case",		kcasesym,		jcase),
	("docase",		kcasesym,		jdocase),
	("recase",		krecasesym,		jrecase),
	("when",		kwhensym,		0),
	("for",			kforsym,		0),
	("foreach",		kforsym,		1),
	("to",			ktosym,			0),
	("downto",		ktosym,			1),
	("by",			kbysym,			0),
	("do",			kdosym,			0),
	("end",			kendsym,		0),
	("while",		kwhilesym,		0),
	("repeat",		krepeatsym,		0),
	("until",		kuntilsym,		0),
	("always",		kuntilsym,		0),
	("return",		kreturnsym,		0),
	("stop",		kstopsym,		0),

	("redoloop",	kloopsym,		loop_redo),

	("nextloop",	kloopsym,		loop_next),

	("exit",		kloopsym,		loop_exit),

	("goto",		kgotosym,		0),
	("switch",		kswitchsym,		jswitch),
	("doswitch",	kswitchsym,		jdoswitch),
	("tabledata",	ktabledatasym,	0),
	("enumdata",	ktabledatasym,	1),
	("clamp",		kclampsym,		0),
	("maps",		kmapsym,		0),
	("mapss",		kmapsym,		0),
	("eval",		kevalsym,		0),

	("print",		kprintsym,		0),
	("println",		kprintsym,		pr_newline),
	("fprint",		kprintsym,		pr_format),
	("fprintln",	kprintsym,		pr_format + pr_newline),
	("sprint",		ksprintsym,		pr_sprint),
	("sfprint",		ksprintsym,		pr_sprint + pr_format),

	("cp",			kprintsym,		0),
	("cpl",			kprintsym,		pr_newline),

	("read",		kreadsym,		0),
	("readln",		kreadsym,		pr_newline),

	("cast",		kcastsym,		13),

	("proc",		kprocsym,		0),
	("subx",		kprocsym,		1),

	("function",	kfunctionsym,	0),

	("func",		kfunctionsym,	0),
	("fun",			kfunctionsym,	1),
	("method",		kfunctionsym,	0),

	("type",		ktypesym,		0),
	("record",		krecordsym,		0),
	("struct",		kstructsym,		0),
	("union",		kunionsym,		0),
	("ref",			krefsym,		0),
	("var",			kvarsym,		0),

	("macro",		kmacrosym,		0),

	("static",		kstaticsym,		0),
	("$caligned",	kcalignedsym,	0),
	
	("const",		kconstsym,		0),

	("module",		kmodulesym,		0),
	("import",		kimportsym,		0),

	("importdll",	kimportdllsym,	'D'),
	("strinclude",	kstrincludesym,	0),
	("unless",		kunlesssym,		0),

	("try",			ktrysym,		0),
	("except",		kexceptsym,		0),
	("raise",		kraisesym,		0),

	("global",		kglobalsym,		global_scope),
	("export",		kglobalsym,		export_scope),

	("swap",		kswapsym,		0),

	("void",		stdtypesym,		tvoid),

	("int",			stdtypesym,		tint),
	("real",		stdtypesym,		treal),

	("string",		stdtypesym,		tstring),
	("list",		stdtypesym,		tlist),
	("array",		stdtypesym,		tarray),
	("vector",		stdtypesym,		tvector),
	("bits",		stdtypesym,		tbits),
	("set",			stdtypesym,		tset),
	("dict",		stdtypesym,		tdict),
	("decimal",		stdtypesym,		tdecimal),
	("longint",		stdtypesym,		tdecimal),
	("typetype",	stdtypesym,		ttype),
	("range",		stdtypesym,		trange),
	("recordtype",	stdtypesym,		trecord),

	("cvoid",		stdtypesym,		tvoid),
	("i8",			stdtypesym,		ti8),
	("i16",			stdtypesym,		ti16),
	("i32",			stdtypesym,		ti32),
	("i64",			stdtypesym,		ti64),

	("bit",			stdtypesym,		tu1),
	("u1",			stdtypesym,		tu1),
	("u2",			stdtypesym,		tu2),
	("u4",			stdtypesym,		tu4),
	("byte",		stdtypesym,		tu8),
	("u8",			stdtypesym,		tu8),
	("u16",			stdtypesym,		tu16),
	("u32",			stdtypesym,		tu32),
	("u64",			stdtypesym,		tu64),

	("r32",			stdtypesym,		tr32),
	("r64",			stdtypesym,		tr64),

!	("int8",		stdtypesym,		ti8),
!	("int16",		stdtypesym,		ti16),
!	("int32",		stdtypesym,		ti32),
!	("int64",		stdtypesym,		ti64),

!	("word8",		stdtypesym,		tu8),
!	("word16",		stdtypesym,		tu16),
!	("word32",		stdtypesym,		tu32),
!	("word64",		stdtypesym,		tu64),

!	("r32",		stdtypesym,		tr32),
!	("r64",		stdtypesym,		tr64),

	("stringc",		stdtypesym,		tpackstrc),
	("stringz",		stdtypesym,		tpackstrz),
	("cstring",		stdtypesym,		tpackstrz),
	("ichar",		stdtypesym,		tstringz),

	("million",		unitnamesym,	million_unit),
	("billion",		unitnamesym,	billion_unit),
!	("thousand",	unitnamesym,	thousand_unit),
	("as",			unitnamesym,	0),

	("$lineno",		compilervarsym,	cv_lineno),
	("$strlineno",	compilervarsym,	cv_strlineno),
	("$filename",	compilervarsym,	cv_filename),
	("$modulename",	compilervarsym,	cv_modulename),
	("$function",	compilervarsym,	cv_function),
	("$date",		compilervarsym,	cv_date),
	("$time",		compilervarsym,	cv_time),
!	("$version",	compilervarsym,	J_cvversion),
	("$",			dollarsym,		0),

	("and",			andlsym,		jandl),
	("or",			orlsym,			jorl),
	("iand",		iandsym,		kiand),
	("ior",			iorsym,			kior),
	("ixor",		ixorsym,		kixor),
	("in",			insym,			0),
	("inx",			inxsym,			0),
	("rem",			iremsym,		kirem),
	("divrem",		idivremsym,		kidivrem),
	("min",			minmaxsym,		kmin),
	("max",			minmaxsym,		kmax),

	("not",			notlsym,		jnotl),
	("istrue",		istruelsym,		jistruel),
	("inot",		inotsym,		kinot),
	("abs",			abssym,			kabs),
	("asc",			ascsym,			kasc),
	("chr",			chrsym,			kchr),
	("sqrt",		mathssym,		mm_sqrt),
	("sqr",			mathssym,		mm_sqr),
	("cos",			mathssym,		mm_cos),
	("sin",			mathssym,		mm_sin),
	("tan",			mathssym,		mm_tan),
	("asin",		mathssym,		mm_asin),
	("acos",		mathssym,		mm_acos),
	("atan",		mathssym,		mm_atan),
	("atan2",		maths2sym,		mm_atan2),
	("sign",		mathssym,		mm_sign),
	("log",			mathssym,		mm_log),
	("log10",		mathssym,		mm_log10),
	("exp",			mathssym,		mm_exp),
	("round",		mathssym,		mm_round),
	("floor",		mathssym,		mm_floor),
	("ceil",		mathssym,		mm_ceil),
	("fract",		mathssym,		mm_fract),
	("fmod",		maths2sym,		mm_fmod),

	("append",		appendsym,		kappend),
	("concat",		concatsym,		kconcat),

	("len",			propsym,		klen),
	("lwb",			propsym,		klwb),
	("upb",			propsym,		kupb),
	("bounds",		propsym,		kbounds),
!	("bitwidth",	propsym,		jbitwidth),
	("bytes",		propsym,		kbytesize),
	("isfound",		propsym,		kisfound),
	("dictitems",	propsym,		kdictsize),

!	("odd"	,		propsym,		kmaxval),
!	("even",		propsym,		kmaxval),

!	("basetype",	propsym,		jbasetype),
!!	("usertype",	propsym,		jusertype),
	("basetype",	miscpropsym,	'b'),
	("elemtype",	miscpropsym,	'e'),

!	("dictitems",	miscpropsym,	'dict'),

	("isvoid",		miscpropsym,	'v'),
	("isdef",		miscpropsym,	'd'),
	("defined",		miscpropsym,	'd'),

	("isint",		istypesym,		tint),
	("isreal",		istypesym,		treal),
	("islist",		istypesym,		tlist),
	("isstring",	istypesym,		tstring),
	("isrange",		istypesym,		trange),
	("ispointer",	istypesym,		trefvar),
	("isarray",		istypesym,		tarray),
	("isrecord",	istypesym,		trecord),
	("isset",		istypesym,		tset),
	("isnumber",	istypesym,		tnumber),
!	("ismutable",	istypesym,		jismutable),
!	("odd",			istypesym,		jodd),
!	("even",		istypesym,		jeven),

	("fi",			kendsym,		kifsym),
	("esac",		kendsym,		kcasesym),
	("od",			kendsym,		kdosym),

	("nil",			knilsym,		0),
	("con",			sysconstsym,	con_const),
	("pi",			sysconstsym,	pi_const),
	("true",		sysconstsym,	true_const),
	("false",		sysconstsym,	false_const),

	("$neg",		specialopsym,	'-'),
!	("$index",		specialopsym,	'[]'),

	("$$dummy",		0,				0)
end

global enumdata [0:]ichar hostfnnames, [0:]byte hostnparams, [0:]byte hostisfn,
			[0:]byte hostinternal, [0:]ref proc hosthandlers =
!                    name  np isfn int
	(h_dummy=0,			$,	0,	0,	1,	cast(nil)                ),

	(h_startprint,		$,	1,	0,	1,	cast(pch_startprint)     ),	!startprint(x)	Set o/p dev for following print items
	(h_startprintcon,	$,	0,	0,	1,	cast(pch_startprintcon)  ),	!startprintcon()	Set console dev for following print items
	(h_strstartprint,	$,	0,	0,	1,	cast(pch_strstartprint)  ),	!strstartprint()	Set o/p dev for internal string
	(h_setformat,		$,	1,	0,	1,	cast(pch_setformat)      ),	!setformat(x)	Set up format string for following print items up to str/endprint
	(h_endprint,		$,	0,	0,	1,	cast(pch_endprint)       ),	!endprint()	Restore o/p dev
	(h_strendprint,		$,	0,	1,	1,	cast(pch_strendprint)    ),	!strendprint()	Restore o/p dev, and return result as string
	(h_print,			$,	2,	0,	1,	cast(pch_print)          ),		!print(x,[y])	Print x, using default format code or y
	(h_print_nf,		$,	1,	0,	1,	cast(pch_print_nf)       ),		!print(x)		Print x, using default format code

	(h_println,			$,	0,	0,	1,	cast(pch_println)        ),	!println()	Print newline
	(h_printnogap,		$,	0,	0,	1,	cast(pch_printnogap)     ),	!printnogap()	Suppress any gap before next print item
	(h_printspace,		$,	0,	0,	1,	cast(pch_printspace)     ),	!printspace		Extra space at beg or end

	(h_readln,			$,	1,	0,	1,	cast(pch_readln)         ),	!sreadln(x)	Read line from console or device x, into read buffer
	(h_sreadln,			$,	1,	1,	0,	cast(pch_sreadln)        ),	!sreadln(x)	Read line from console or device x, into read buffer
	(h_sread,			$,	1,	1,	0,	cast(pch_sread)          ),	!sread([x])	Read item from read buffer, with/without format code
	(h_rereadln,		$,	0,	0,	0,	cast(pch_rereadln)       ),	!sread([x])	Read item from read buffer, with/without format code
	(h_reread,			$,	0,	0,	0,	cast(pch_reread)         ),	!sread([x])	Read item from read buffer, with/without format code

	(h_strtoval,		$,	2,	1,	0,	cast(pch_strtoval)       ),	!
	(h_tostr,			$,	2,	1,	0,	cast(pch_tostr)          ),	!

	(h_leftstr,			$,	3,	1,	0,	cast(pch_leftstr)        ),
	(h_rightstr,		$,	3,	1,	0,	cast(pch_rightstr)       ),
	(h_convlc,			$,	2,	1,	0,	cast(pch_convlc)         ),
	(h_convuc,			$,	2,	1,	0,	cast(pch_convuc)         ),

	(h_waitkey,			$,	0,	1,	0,	cast(pch_waitkey)        ),
	(h_testkey,			$,	0,	1,	0,	cast(pch_testkey)        ),
	(h_execwait,		$,	3,	1,	0,	cast(pch_execwait)       ),
	(h_execcmd,			$,	3,	1,	0,	cast(pch_execcmd)        ),
	(h_system,			$,	1,	1,	0,	cast(pch_system)         ),

	(h_makestr,			$,	2,	1,	0,	cast(pch_makestr)        ),
	(h_makeref,			$,	2,	1,	0,	cast(pch_makeref)        ),

	(h_new,				$,	4,	1,	0,	cast(pch_new)            ),
!	(h_setoverload,		$,	3,	0,	0,	cast(pch_setoverload)    ),

	(h_getcmdparam,		$,	1,	1,	0,	cast(pch_getcmdparam)    ),
	(h_gethostname,		$,	0,	1,	0,	cast(pch_gethostname)    ),
	(h_getprogname,		$,	0,	1,	0,	cast(pch_getprogname)    ),

!	(h_$setpcerror,		$,	1,	0,	0,	cast(pch_$setpcerror)    ),
	(h_$setdebug,		$,	1,	0,	0,	cast(pch_$setdebug)      ),
	(h_$test2,			$,	2,	1,	0,	cast(pch_$test2)         ),
	(h_$test,			$,	3,	1,	0,	cast(pch_$test)          ),
	(h_$refcount,		$,	1,	1,	0,	cast(pch_$refcount)      ),

	(h_ticks,			$,	0,	1,	0,	cast(pch_ticks)          ),
	(h_clock,			$,	0,	1,	0,	cast(pch_clock)          ),
	(h_sleep,			$,	1,	0,	0,	cast(pch_sleep)          ),
	(h_random,			$,	1,	1,	0,	cast(pch_random)         ),
	(h_gethash,			$,	1,	1,	0,	cast(pch_gethash)        ),
	(h_getos,			$,	0,	1,	0,	cast(pch_getos)          ),
	(h_iswindows,		$,	0,	1,	0,	cast(pch_iswindows)      ),
	(h_setmesshandler,	$,	1,	0,	0,	cast(pch_setmesshandler) ),
	(h_$getstdinout,	$,	1,	1,	0,	cast(pch_$getstdinout)   ),
	(h_$getparam,		$,	1,	1,	0,	cast(pch_$getparam)      ),
	(h_makeempty,		$,	1,	1,	0,	cast(pch_makeempty)      ),
	(h_$smallmemtotal,	$,	0,	1,	0,	cast(pch_$smallmemtotal) ),
	(h_$id,				$,	1,	1,	0,	cast(pch_$id)            ),
	(h_copy,			$,	1,	1,	0,	cast(pch_copy)           ),
	(h_$nan,			$,	0,	1,	0,	cast(pch_$nan)           ),
	(h_$infinity,		$,	0,	1,	0,	cast(pch_$infinity)      ),

	(h_$nprocs,			$,	0,	1,	0,	cast(pch_$nprocs)        ),
	(h_$procname,		$,	1,	1,	0,	cast(pch_$procname)      ),
	(h_$procref,		$,	1,	1,	0,	cast(pch_$procref)       ),

	(h_allocexec,		$,	1,	1,	0,	cast(pch_allocexec)      ),
	(h_runnative,		$,	2,	1,	0,	cast(pch_runnative)      ),
	(h_setlwb,			$,	2,	0,	0,	cast(pch_setlwb)         ),

	(h_last,			$,	0,	0,	1,	cast(nil)                )
end

global []byte D_binopset = (
	andlsym, orlsym, eqsym, nesym, ltsym, lesym, gtsym, gesym, addsym,
	subsym, mulsym, divsym, idivsym, iremsym, iandsym, iorsym, ixorsym,
	shlsym, shrsym, minmaxsym,	concatsym, powersym,
	idivremsym,  maths2sym, appendsym, addrsym, samesym )

global [0..symbolnames.upb]byte binopset

global []byte D_unaryopset = (
	notlsym, inotsym, abssym, istruelsym, ascsym, chrsym,
	mathssym)

global [0..symbolnames.upb]byte unaryopset

global []byte D_addopset=(addsym, subsym, iandsym, iorsym, ixorsym,
		concatsym, appendsym, minmaxsym, addrsym, samesym)

global []byte D_cmpopset=(eqsym, nesym, ltsym, lesym, gesym, gtsym)

global []byte D_mulopset=(mulsym, divsym, idivsym, iremsym, shlsym, shrsym, idivremsym)

global [0..symbolnames.upb]byte addopset
global [0..symbolnames.upb]byte cmpopset
global [0..symbolnames.upb]byte mulopset
global [0..symbolnames.upb]byte exprendset

global []int D_exprstarterset= (lbracksym,lsqsym,ptrsym,addrsym,namesym,
	incrsym,intconstsym,decimalconstsym,realconstsym,charconstsym,
	stringconstsym,stdtypesym, kmapsym, lcurlysym,
	ksprintsym,dollarsym,compilervarsym, kclampsym,
	krefsym, kcastsym, ellipsissym,
	knilsym, khostfnsym, kifsym, krecordsym, kstructsym)

global [0:symbolnames.len]byte exprstarterset

!type tables
global const maxtype=250

global [0..maxtype]ichar ttname
global [0..maxtype]symbol ttnamedef
global [0..maxtype]i16 ttbasetype
global [0..maxtype]i16 tttarget

global [0..maxtype]int ttlower
global [0..maxtype]int ttlength

global [0..maxtype]unit ttlowerexpr
global [0..maxtype]unit ttlengthexpr

global [0..maxtype]int ttsize
global [0..maxtype]byte ttbitwidth
global [0..maxtype]symbol ttfields		!for initialially anonymous record field lists
global [0..maxtype]byte ttcaligned
global [0..maxtype]symbol ttowner
global int ntypes
global int firstusertype				!starts at ntypes+1, augments for each sp

global const int maxuserxtype=5000
global int nuserxtypes
global int userxtypebase			!first index (growing downwards) of userxtypes in current module
global ref userxrec userxmodelist	!list of all references to userx modes

global [0:maxuserxtype]symbol ttnamedefx
global [0:maxuserxtype]int ttxmap
global [0:maxuserxtype]byte ttxmoduleno

global [0..h_last]byte hostlvset

proc start=
!	translate into an instant lookup format
	int i

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
	od

	ntypes:=tlast
end

!flags used with print/fprint/fprint
global const pr_newline = 1
global const pr_format = 2
global const pr_sprint = 4

global enumdata []ichar cvnames =
	(cv_lineno,		$),
	(cv_strlineno,	$),
	(cv_filename,	$),
	(cv_modulename,	$),
	(cv_function,	$),
	(cv_date,		$),
	(cv_time,		$),
end

global enumdata []ichar loopnames =
	(loop_redo,		$),		!must be in this order: start of loop body
	(loop_next,		$),		!end of loop body
	(loop_exit,		$),		!past end of loop
end

global enumdata []ichar mathsnames =
	(mm_sqrt,		$),
	(mm_sqr,		$),
	(mm_sin,		$),
	(mm_cos,		$),
	(mm_tan,		$),
	(mm_asin,		$),
	(mm_acos,		$),
	(mm_atan,		$),
	(mm_sign,		$),
	(mm_log,		$),
	(mm_log10,		$),
	(mm_exp,		$),
	(mm_round,		$),
	(mm_floor,		$),
	(mm_ceil,		$),
	(mm_fract,		$),
	(mm_fmod,		$),
	(mm_atan2,		$),
end

!can't start from 0, as 0 in cmpchain list means no more conds
global enumdata [0:]ichar condnames, [0:]byte revconds =
	(eq_cc = 0,	"eq",	ne_cc),			!order must match kjumpeq..kumpgt pcl ops
	(ne_cc,		"ne",	eq_cc),
	(lt_cc,		"lt",	ge_cc),
	(le_cc,		"le",	gt_cc),
	(ge_cc,		"ge",	lt_cc),
	(gt_cc,		"gt",	le_cc),
end
=== qq_dummyshow.m 0 0 29/48 ===
!labels are just numbers 1,2,3 which index both of these tables
!labelblocktable is the pclblock no (as all labels are shared across the program)
!labeloffsettable is the offset into the pclblock

!global const labelinitalloc=8192
!global ref[]int labeloffsettable
!global int labelalloc
!global int nextlabelno
ref[0:]int labelmap
int currlineno
symbol currpclproc

strbuffer pclv
global ref strbuffer pcldest = &pclv

global proc printunit(ref unitrec p,int level=0,ichar prefix="*",filehandle dev=nil)=		!PRINTUNIT
end

global proc printglobalsymbols(filehandle f=nil)=
end

global proc printst(filehandle f,symbol p,int level=0)=
end

global proc printglobalsymbols_full(filehandle f=nil)=
end

global proc printstfull(filehandle f,symbol p,int level=0)=
end

global proc printtypetables(filehandle f)=
end

!global function getpclname:ichar=
!	return "<no show.m>"
!end

!function getpclcode(ref int pc)int=
!	for i in pclnames.bounds do
!		if cast(pc^,ref void)=handlertable[i] then
!			return i
!		fi
!	od
!	return 0
!end

global proc showlogfile=
end

global proc showast(isubprog sp, ichar file)=
end

!global proc showpcl(isubprog sp, int pass)=
!end

global proc showmpl(int pass)=
end

global proc printsymbol(ref lexrec lp)=
end

global function strmode(int t, expand=0)ichar=
	ttname[t]
end

global proc deletetempfiles=
end
=== qq_showpcldummy.m 0 0 30/48 ===
global proc showpcl(isubprog sp, int pass)=
end

global proc showpcl2(isubprog sp, int pass)=
end

global proc writeallpcl(ifile pm, int pass)=
end

=== qq_vars.m 0 0 31/48 ===
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

!global macro var_share(x) = var_shareu(x) when x.hasref
global macro var_share(x) = if x.hasref then ++x.objptr.refcount fi
global macro var_unshare(x) = var_unshareu(x) when x.hasref
global macro var_dupl(x) = var_duplu(x) when x.hasref
global macro var_shareu(x) = ++x.objptr.refcount

objrec zeroobj

global proc var_unshareu(variant p)=
	if --p.objptr.refcount<=0 then
		var_free(p)
	fi
end

global proc obj_shareu(object p)=
	++p.refcount
end

global function void_new:variant p =
	p:=pcm_alloc(varrec.bytes)
	p.tagx:=tvoid
	return p
end

global function obj_new:object p=
	P:=PCM_ALLOC(32)
	P^:=ZEROOBJ

	p.refcount:=1
	return p
end

global function var_getintvalue(variant p)int =
! return int value from variant, which should be a numeric type
	case p.tag
	when tint,ttype then
		return p.value
	when treal then
		return p.xvalue
	else
		pcustype("getintvalue",p)
	esac
	return 0
end

global proc var_fromobj(int tag,object p, variant dest)=
	dest.tagx:=tag ior hasrefmask
	dest.objptr:=p
end

global proc var_free(variant a)=
	varrec v
	object q:=a.objptr

	case q.objtype
	when normal_obj then
		case a.tag
		when tlist then
			obj_free_list(q)
		when trecord then
			obj_free_record(q)
		when tstring then
			obj_free_string(q)
		when tarray then
			obj_free_array(q)
		when tvector then
			obj_free_vector(q)
		when tbits then
			obj_free_bits(q,a.tag)
		when tstruct then
			obj_free_struct(q)
		when tdict then
			obj_free_dict(q)
		when tset then
			obj_free_set(q)
		when tdecimal then
			obj_free_dec(q)
		else
!			pcustype_t("free", a.tag)
			pcustype("free", a)
		esac
	when slice_obj then
		v.tagx:=a.tag
		v.objptr:=q.objptr2
		var_unshareu(&v)
		pcm_free32(q)
	else
		pcm_free32(q)
	esac
end

global proc var_duplu(variant a)=
	case a.tag
	when tstring then
		var_dupl_string(a)
	when tset then
		var_dupl_set(a)
	when tlist then
		var_dupl_list(a)
	when tdict then
		var_dupl_dict(a)
	when tarray then
		var_dupl_array(a)
	when tvector then
		var_dupl_vector(a)
	when tbits then
		var_dupl_bits(a)
	when trecord then
		var_dupl_record(a)
	when tstruct then
		var_dupl_struct(a)
	when tdecimal then
		var_dupl_dec(a)
	else
		pcustype_t("dupl", a.tag)
	esac
end

global proc var_neg(variant a)=
	case a.tag
	when tint then
		a.value:=-a.value
	when treal then
		a.xvalue:=-a.xvalue
	when tdecimal then
		var_dupl_dec(a)
		var_neg_dec(a)
	when tset then
		var_dupl_set(a)
		var_inotto_set(a)
	else
		pcustype_t("neg", a.tag)
	esac
end

global proc var_abs(variant a)=
	case a.tag
	when tint then
		a.value:=abs a.value
	when treal then
		a.xvalue:= abs a.xvalue
	when tdecimal then
		var_dupl_dec(a)
		var_abs_dec(a)
	else
		pcustype_t("abs", a.tag)
	esac
end

global proc var_inot(variant a)=
	case a.tag
	when tint then
		a.value:=inot a.value
	when tset then
		var_dupl_set(a)
		var_inotto_set(a)
	else
		pcustype_t("inot", a.tag)
	esac
end

global function var_istruel(variant a)int=
	case a.tag
	when tint, trefvar, trefpack, trefbit, ttype, tsymbol then
		return istrue a.value
	when treal then
		return (a.xvalue<>0|1|0)
!	when tdecimal then
!		return var_istruel_decimal(a)
	when tstring,tlist,tarray,tbits,tvector then
		return a.objptr.length<>0
	when tset then
		return a.objptr.length<>0
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
	when tdecimal then
		return not bn_iszero(a.objptr)
	when tvoid then
		return 0
	else
		pcustype_t("istruel", a.tag)
	esac
	return 0
end

!global function var_negto(variant p)int=
!	variant a:=p.varptr
!
!	if p.tag<>trefvar then
!		return 0
!	fi
!	case a.tag
!	when tint then
!		a.value:=-a.value
!	when treal then
!		a.xvalue:=-a.xvalue
!!	when tdecimal then
!!		var_negto_decimal(a)
!	when tset then
!		var_inotto_set(a)
!	else
!		pcustype_t("negto", a.tag)
!	esac
!	return 1
!end
!
!global function var_absto(variant p)int=
!	variant a:=p.varptr
!
!	if p.tag<>trefvar then
!		return 0
!	fi
!	case a.tag
!	when tint then
!		a.value:= abs a.value
!	when treal then
!		a.xvalue:= abs a.xvalue
!!	when tdecimal then
!!		var_absto_decimal(a)
!	else
!		pcustype_t("absto", a.tag)
!	esac
!	return 1
!end
!
!global function var_inotto(variant p)int=
!	variant a:=p.varptr
!
!	if p.tag<>trefvar then
!		return 0
!	fi
!	case a.tag
!	when tint then
!		a.value:=inot a.value
!	when tset then
!		var_inotto_set(a)
!	else
!		pcustype_t("inotto", a.tag)
!	esac
!	return 1
!end

global proc var_add(variant a, b)=
	if a.tag<>b.tag then
		var_addmixed(a,b)
		return
	fi

	case a.tag
	when tint then
		a.value+:=b.value
	when treal then
		a.xvalue+:=b.xvalue
	when tdecimal then
		var_add_dec(a,b)
	when tstring then
		var_add_string(a,b)
	when tset then
		var_dupl_set(a)
		var_iorto_set(a,b)
	else
		pcustype_t("add", a.tag)
	esac
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
!	when pr(tint,		tword)    then
!		a.value+:=b.value
!	when pr(tword,		tint)     then
!		newtag:=tint
!		a.value+:=b.value
	when pr(tint,tdecimal),
		 pr(treal,tdecimal) then
		newtag:=tdecimal
		var_add_dec(dectemp(a),b)
		freedectemp()
	when pr(tdecimal,tint),
		 pr(tdecimal,treal) then
		var_add_dec(a,dectemp(b))

	when pr(trefpack,	tint) then
		if a.ptr=nil then pcerror("Nil+x") fi
		a.ptr+:=ttsize[a.elemtag]*b.value
	else
		pcmxtypes("Addmixed",a,b)
	esac

	a.tag:=newtag
end

global function var_addto(variant p,b)int=
!p^+:=b
!handles case where p is a refvar and types are mostly matched
	variant a:=p.varptr
	int newtag

	return 0 when p.tag<>trefvar
	newtag:=a.tag

	if a.tag<>b.tag then
		if newtag=tstring and b.tag=tint then
			var_addto_string_ch(a,b.value)
			return 1
		fi
		return 0

	fi

	switch a.tag
	when tint then
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
	end switch

	a.tag:=newtag

	return 1
end


global proc var_sub(variant a, b)=
	ref byte p,q
	int elemsize,x

	if a.tag<>b.tag then
		var_submixed(a,b)
		return
	fi
	case a.tag
	when tint then
		a.value-:=b.value
	when treal then
		a.xvalue-:=b.xvalue
	when tdecimal then
		var_sub_dec(a,b)
	when trefpack then
		p:=a.ptr
		q:=b.ptr
		case elemsize:=ttsize[a.elemtag]
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
	esac
end

global proc var_submixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		newtag:=treal
		a.xvalue:=a.value-b.xvalue
		
	when pr(treal,		tint)     then
		a.xvalue-:=b.value
	when pr(tint,tdecimal),
		 pr(treal,tdecimal) then
		newtag:=tdecimal
		var_sub_dec(dectemp(a),b)
		freedectemp()
	when pr(tdecimal,tint),
		 pr(tdecimal,treal) then
		var_sub_dec(a,dectemp(b))


!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	when pr(trefpack,	tint) then
		a.ptr-:=ttsize[a.elemtag]*b.value
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
	case a.tag
	when tint then
		a.value*:=b.value
	when treal then
		a.xvalue*:=b.xvalue
	when tdecimal then
		var_mul_dec(a,b)
	when tset then
		var_dupl_set(a)
		var_iandto_set(a,b)
	else
		pcustype_t("mul", a.tag)
	esac
end

global proc var_mulmixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		newtag:=treal
		a.xvalue:=a.value*b.xvalue
		
	when pr(treal,		tint)     then
		a.xvalue*:=b.value
	when pr(tint,tdecimal),
		 pr(treal,tdecimal) then
		newtag:=tdecimal
		var_mul_dec(dectemp(a),b)
		freedectemp()
	when pr(tdecimal,tint),
		 pr(tdecimal,treal) then
		var_mul_dec(a,dectemp(b))

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
	case a.tag
	when tint then
		a.tagx:=treal
		a.xvalue:=real(a.value)/b.value
!		var_div_int(a,b)
	when treal then
		a.xvalue/:=b.xvalue
	when tdecimal then
		var_div_dec(a,b)
	else
		pcustype_t("div", a.tag)
	esac
end

global proc var_divmixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		newtag:=treal
		a.xvalue:=a.value/b.xvalue
	when pr(treal,		tint)     then
		a.xvalue/:=b.value
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
		pcerror("idivmixed")
		return
	fi
	case a.tag
	when tint then
!CPL "/////////////IDIV",A.VALUE,B.VALUE
		if b.value then
			a.value:=a.value/b.value
		else
			pcerror("Divide by 0")
		fi
!		var_idiv_int(a,b)
	when tdecimal then
		var_idiv_dec(a,b)
	else
		pcustype_t("idiv", a.tag)
	esac
end

!global proc var_idivmixed(variant a, b)=
!	int newtag:=a.tag
!	case pr(a.tag,		b.tag)
!!	when pr(tint,		tdecimal) then
!!		pcerror("int/dec")
!!	when pr(tdecimal,	tint)     then
!!		pcerror("dec/int")
!!	when pr(treal,		tdecimal) then
!!		pcerror("real/dec")
!!	when pr(tdecimal,	treal)    then
!!		pcerror("dec/real")
!	else
!		pcmxtypes("Idivmixed",a,b)
!	esac
!
!	a.tag:=newtag
!end

global proc var_irem(variant a, b)=
	if a.tag<>b.tag then
		pcerror("iremmixed")
		return
	fi

	case a.tag
	when tint then
		a.value:=a.value rem b.value

!		var_irem_int(a,b)
	when tdecimal then
		var_irem_dec(a,b)
	else
		pcustype_t("irem", a.tag)
	esac
end

!global proc var_iremmixed(variant a, b)=
!	int newtag:=a.tag
!	case pr(a.tag,		b.tag)
!!	when pr(tint,		tdecimal) then
!!		pcerror("int/dec")
!!	when pr(tdecimal,	tint)     then
!!		pcerror("dec/int")
!!	when pr(treal,		tdecimal) then
!!		pcerror("real/dec")
!!	when pr(tdecimal,	treal)    then
!!		pcerror("dec/real")
!	else
!		pcmxtypes("Iremmixed",a,b)
!	esac
!
!	a.tag:=newtag
!end

global proc var_iand(variant a, b)=
	if a.tag<>b.tag then
		pcerror("iand mixed")
		return
	fi
	case a.tag
	when tint then
		a.value iand:= b.value
	when tset then
		var_dupl_set(a)
		var_iandto_set(a,b)
	else
		pcustype_t("iand", a.tag)
	esac
end

global proc var_ior(variant a, b)=
	if a.tag<>b.tag then
		pcerror("ior mixed")
!		var_iormixed(a,b)
		return
	fi
	case a.tag
	when tint then
		a.value ior:=b.value

	when tset then
		var_dupl_set(a)
		var_iorto_set(a,b)
	else
		pcustype_t("ior", a.tag)
	esac
end

global proc var_ixor(variant a, b)=
	if a.tag<>b.tag then
		pcerror("ixor mixed")
!		var_ixormixed(a,b)
		return
	fi
	case a.tag
	when tint then
		a.value ixor :=b.value
	when tset then
		var_dupl_set(a)
		var_ixorto_set(a,b)
	else
		pcustype_t("ixor", a.tag)
	esac
end

global proc var_shl(variant a, b)=
	if a.tag<>b.tag then
		pcerror("ishl mixed")
!		var_shlmixed(a,b)
		return
	fi
	case a.tag
	when tint then
		a.value<<:=b.value

!		var_shl_int(a,b)
!	when tdecimal then
!		var_shl_decimal(a,b)
	else
		pcustype_t("shl", a.tag)
	esac
end

global proc var_shr(variant a, b)=
	if a.tag<>b.tag then
		pcerror("ishr mixed")
!		var_shrmixed(a,b)
		return
	fi
	case a.tag
	when tint then
		a.value>>:=b.value
!	when tint, tword then
!		var_shr_int(a,b)
!	when tdecimal then
!		var_shr_decimal(a,b)
	else
		pcustype_t("shr", a.tag)
	esac
end

global function var_in(variant a, b)int n=
	case pr(a.tag,b.tag)
	when pr(tint, tset), pr(ttype,tset) then
		return var_in_set(a,b)
	when pr(tint, trange) then
		return (a.value in b.range_lower..b.range_upper|1|0)

	elsecase b.tag
	when tlist, tstring, tarray, tvector then
		n:=var_inx(a,b)
		return (n<>i64.min|1|0)
	else
		pcmxtypes("in", a,b)
	esac
	return 0
end

global function var_inx(variant a, b)int n=
	case pr(a.tag,b.tag)
	when pr(tstring,tstring) then
		return var_inx_string(a,b)
	when pr(tint, tarray), pr(treal, tarray) then
		return var_inx_array(a,b, tvoid)
	when pr(tint, tvector), pr(treal, tvector) then
		return var_inx_array(a,b, b.objptr.usertag)
	elsecase b.tag
	when tlist then
		return var_inx_list(a,b)
	else
		pcmxtypes("inx", a,b)
	esac
	return 0
end

global function var_equal(variant a,b)int=
!can be called when a/b have same tags, or were mixed and have been
!converted, but also they haven't been checked.
	if a.tag<>b.tag then
		return var_equalmixed(a,b)
	fi

	case a.tag
	when tint, trefvar, trefpack, ttype, tsymbol, toperator then
		return a.value=b.value
!	when trefpack, trefbit then
!		return var_equal_refpack(a, b)
	when treal then
		return (a.xvalue=b.xvalue|1|0)
	when tdecimal then
		return var_equal_dec(a, b)
	when tstring then
		return var_equal_string(a, b)
	when tset then
		return var_equal_set(a, b)
	when tlist then
		return var_equal_list(a, b)
	when tdict then
		return var_equal_dict(a, b)
	when tarray, tvector then
		return var_equal_array(a, b)
	when tbits then
		return var_equal_bits(a, b)
	when trecord then
		return var_equal_record(a, b)
	when tstruct then
		return var_equal_struct(a, b)
	else
		pcustype_t("equal", a.tag)
	esac
	return 0
end

global function var_equalmixed(variant a, b)int=
	int result

	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		return (a.value=b.xvalue|1|0)
		
	when pr(treal,		tint)     then
		return (a.xvalue=b.value|1|0)
!		a.value+:=b.value
!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	when pr(tint,tdecimal),
		 pr(treal,tdecimal) then
		result:=var_equal_dec(dectemp(a),b)
		freedectemp()
		return result
	when pr(tdecimal,tint),
		 pr(tdecimal,treal) then
		return var_equal_dec(a,dectemp(b))

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

	case a.tag
	when tint, trefpack then
		return (a.value<b.value|-1|(a.value>b.value|1|0))
	when treal then
		return (a.xvalue<b.xvalue|-1|(a.xvalue>b.xvalue|1|0))
	when tdecimal then
		return var_compare_dec(a,b)
	when tstring then
		return var_compare_string(a,b)
	else
		pcustype_t("compare", a.tag)
	esac
	return 0
end

global function var_comparemixed(variant a, b)int=
	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		return (a.value<b.xvalue|-1|(a.value>b.xvalue|1|0))
!		
	when pr(treal,		tint)     then
		return (a.xvalue<b.value|-1|(a.xvalue>b.value|1|0))
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

	case a.tag
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
	esac
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
		var_unshareu(b)		!caller expects ownership to be xfered
	when tlist then
dolist:
		var_dupl_list(a)
		var_appendto_list(a,b)
	when tarray then
doarray:
		var_dupl_array(a)
		var_appendto_array(a,b)
	when tbits then
dobits:
		var_dupl_bits(a)
		var_appendto_bits(a,b)
	else
error:
		pcustype_t("append", a.tag)
	end
end

global proc var_min(variant a,b)=
	if a.tag<>b.tag then
		pcerror("VARMIN")
		return
	fi

	if var_compare(a,b)<0 then		!x is smaller
		var_unshare(b)
	else
		var_unshare(a)
		a^:=b^
	fi
end

global proc var_max(variant a,b)=
	if a.tag<>b.tag then
		pcerror("VARMAX")
!		var_maxmixed(a,b)
		return
	fi

	if var_compare(a,b)>=0 then		!x is bigger
		var_unshare(b)
	else
		var_unshare(a)
		a^:=b^
	fi
end

global function var_concatto(variant a,b)int=
!	return 0
	if a.tag<>b.tag then pcerror("concatto/mixed") fi
	case a.tag
	when tstring then
		var_addto_string(a,b)
	when tlist then
		var_concatto_list(a,b)
	when tarray then
		var_concatto_array(a,b)
	else
		pcustype("concat",a)
	esac
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

	case a.tag
	when tstring then
		var_addto_string(a,b)
		var_unshareu(b)		!caller expects ownership to be xfered
	when tlist then
dolist:
		var_appendto_list(a,b)
	when tarray then
doarray:
		var_appendto_array(a,b)
	when tbits then
dobits:
		var_appendto_bits(a,b)
	else
		pcustype("append",a)
		return 0
	esac
	return 1
end

global proc var_getix(variant a, int index)=
	case a.tag
	when tstring then
		var_getix_string(a,index)
	when tlist, tdict then
		var_getix_list(a,index)
	when tarray,tvector then
		var_getix_array(a,index)
	when tbits then
		var_getix_bits(a,index)
	when tset then
		var_getix_set(a,index)
	when trecord then
		var_getix_record(a,index)
	when trange then
		if index in a.range_lower..a.range_upper then
			a.tagx:=tint
			a.value:=index
		else
			pcerror("range/bounds")
		fi

!	when tstruct then
!		var_getix_struct(a,index)
	else
		pcustype_t("getix", a.tag)
	esac
end

global proc var_putix(variant a, int index, variant x)=

	case a.tag
	when tstring then
		var_putix_string(a,index,x)
		var_unshareu(x)
	when tlist then
		var_putix_list(a,index,x)
	when tarray,tvector then
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
	esac
end

global proc var_getixref(variant a, int index)=
	case a.tag
	when tstring then
		var_getixref_string(a,index)
	when tlist then
		var_getixref_list(a,index)
	when tarray,tvector then
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
	esac
end

global proc var_getslice(variant a, int i,j)=
	case a.tag
	when tstring then
		var_getslice_string(a,i,j)
	when tlist then
		var_getslice_list(a,i,j)
	when tarray then
		var_getslice_array(a,i,j)
	when tbits then
		var_getslice_bits(a,i,j)
	else
		pcustype_t("getslice", a.tag)
	esac
end

global proc var_putslice(variant a, int i,j, variant x)=
	if a.tag<>x.tag then
		pcerror("putslice: not compatible")
	fi

	case a.tag
	when tstring then
		var_putslice_string(a,i,j,x)
	when tlist then
		var_putslice_list(a,i,j,x)
!	when tarray then
!		var_putslice_array(a,i,j,x)
!	when tbits then
!		var_putslice_bits(a,i,j,x)
	else
		pcustype_t("putslice", a.tag)
	esac
end

global proc var_getdotix(variant a, int index)=
	case a.tag
	when tint then
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
	esac
end

global proc var_putdotix(variant p, int index, variant x)=
	variant a

	if p.tag=trefvar then
		a:=p.varptr

		case a.tag
		when tint then
			if index not in 0..63 then
				pcerror("int.[int]:= bounds")
			fi
			var_storebit(cast(&a.value),index,x,tu1,1)

		when tstring then
			var_putdotix_string(a,index,x)
		when trecord then
			var_putix_record(a,index,x)
!	when tstruct then
!		var_putdotix_struct(a,index,x)
		else
			pcustype("putdotix", a)
		esac
	else
		pcustype("putdotix",p)
	fi
end

global proc var_getdotixref(variant p, int index)=
	variant a

	if p.tag=trefvar then
		a:=p.varptr

		case a.tag
		when tint then
			if index not in 0..63 then
				pcerror("&int.[int] bounds")
			fi
			p.ptr:=cast(&a.value)
			p.tagx:=trefbit
			p.elemtag:=tu1
			p.bitoffset:=index
			p.bitlength:=1
		when tstring then
			var_getdotixref_string(a,index,p)
		when trecord then
			var_getixref_record(a,index,p)
!		when tstruct then
!			var_getdotixref_struct(a,index)
		else
			pcustype_t("getdotixref", a.tag)
		esac
	else
		pcustype("not refvar",p)
	fi
end

global proc var_getdotslice(variant a, int i,j)=
	case a.tag
	when tint then

		if i>j then swap(i,j) fi
		if i<0 or j>63 then pcerror("int.[slice] bounds") fi
		a.value:=(a.value>>i)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))

!		var_getdotslice_int(a,i,j)
	when tstring then
		var_getslice_string(a,i,j)
	else
		pcustype_t("getdotslice", a.tag)
	esac
end

global proc var_putdotslice(variant p, int i,j, variant x)=
	variant a

	if p.tag=trefvar then
		a:=p.varptr

		case a.tag
		when tint then
			if i>j then swap(i,j) fi
			if i<0 or j>63 then pcerror("int.[slice]:= bounds") fi
			var_storebit(cast(&a.value), i,x,tu1,j-i+1)
		when tstring then
			var_putslice_string(a,i,j,x)
		else
			pcustype("putdotslice", a)
		esac
	else
		pcustype("not ref",p)
	fi
end

global proc var_getdotsliceref(variant p, int i,j)=
	variant a

	if p.tag=trefvar then
		a:=p.varptr

		case a.tag
		when tint then
			if i>j then swap(i,j) fi
			if i<0 or j>63 then
				pcerror("&int.[slice] bounds")
			fi
			p.ptr:=cast(&a.value)
			p.tagx:=trefbit
			p.elemtag:=tu1
			p.bitoffset:=i
			p.bitlength:=j-i+1
!			var_getdotsliceref_int(a,i,j)
		else
			pcustype("getdotsliceref", a)
		esac
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
	int n,length

!CPL "EXPAND",=M,TTNAME[A.TAG]

	if m<2 then pcerror("Expand: LHS too few") fi

	case a.tag
	when tlist then
		p:=a.objptr
		length:=p.length
dolist:
		b:=dest
		c:=p.varptr
		n:=1

		to m do
			if n>length then
				dest.tagx:=tvoid
			else
				dest^:=c^
				var_share(dest)
				++c
			fi
			++n
			--dest
		od

	when trange then			!expand to two ints
		dest.tagx:=tint
		dest.value:=a.range_lower
		--dest
		dest.tagx:=tint
		dest.value:=a.range_upper
		to m-2 do
			--dest
			dest.tagx:=tvoid
		od

	when tstring then
		var_expand_string(a, dest, m)
!
	when trecord then
		p:=a.objptr
		length:=ttlength[p.usertag]
		goto dolist

	when tarray, tvector then
		var_expand_array(a, dest, m)

	else
		pcustype("expand",a)
	esac
end

!global function var_minto(variant p,b)int=
!	variant a:=p.varptr
!	int newtag
!
!	if p.tag<>trefvar then
!		return 0
!	fi
!	if newtag:=a.tag<>b.tag then
!		return 0
!	fi
!
!	case a.tag
!	when tint then
!		a.value min:=b.value
!!	when treal then
!!		a.xvalue min:=b.xvalue
!!!		var_addto_real(a,b)
!	when tdecimal then
!		if var_compare_dec(a,b)>0 then
!			var_shareu(b)
!			var_unshareu(a)
!			a^:=b^
!		fi
!
!!!	when tstring then
!!!		var_addto_string(a,b)
!	when tstring then
!		if var_compare_string(a,b)>0 then		!b is smaller
!			var_shareu(b)
!			var_unshareu(a)
!			a^:=b^
!		fi
!
!!!	when tset then
!!!		var_addto_set(a,b)
!	else
!		return 0
!	esac
!	return 1
!end
!
!global function var_maxto(variant p,b)int=
!	variant a:=p.varptr
!	int newtag
!
!	if p.tag<>trefvar then
!		return 0
!	fi
!	if newtag:=a.tag<>b.tag then
!		return 0
!	fi
!
!	case a.tag
!	when tint then
!		a.value max:=b.value
!!	when treal then
!!		a.xvalue min:=b.xvalue
!!!		var_addto_real(a,b)
!	when tdecimal then
!		if var_compare_dec(a,b)<0 then
!			var_shareu(b)
!			var_unshareu(a)
!			a^:=b^
!		fi
!!!	when tdecimal then
!!!		var_addto_decimal(a,b)
!!!	when tstring then
!!!		var_addto_string(a,b)
!	when tstring then
!		if var_compare_string(a,b)<0 then		!b is bigger
!			var_shareu(b)
!			var_unshareu(a)
!			a^:=b^
!		fi
!
!!!	when tset then
!!!		var_addto_set(a,b)
!	else
!		return 0
!	esac
!	return 1
!end
!

!global proc var_inplace(variant px,y, ref proc(variant,variant) fnadd, fnaddmixed=nil)=
!	varrec x
!	varrec z
!
!	var_loadptr(px,&x)
!!z:=x
!
!	if x.tag=y.tag then
!		fnadd^(&x,y)
!	elsif fnaddmixed then
!		fnaddmixed^(&x,y)
!	else
!!		if u64(fnadd)=u64(var_add) and x.tag=tstring and y.tag=tint then
!!			var_addto_string_ch(&x,y.value)
!!		else
!!
!!			pcerror("Inplace mixed")
!			pcmxtypes("Inplace mixed",&x,y)
!!		fi
!	fi
!
!!var_unshare(&z)
!	var_storeptr(px,&x)
!end
!
global proc var_inplace(int index, variant px, y)=
	ref proc(variant, variant) fnadd, fnaddmixed
	varrec x
	varrec z

	if bintotable[index].pclop=kadd then
		if var_addto(px, y) then
			return
		fi
	fi

	fnadd:=cast(bintotable[index].fnadd)
	fnaddmixed:=cast(bintotable[index].fnaddmixed)

	var_loadptr(px,&x)
	if x.tag=y.tag then
		fnadd^(&x,y)
!	elsif u64(fnadd)=u64(var_add) and x.tag=tstring and y.tag=tint then
!		var_addto_string_ch(&x,y.value)
	elsif fnaddmixed then
		fnaddmixed^(&x,y)
	else
!		pcerror("Inplace mixed")
CPL PCLNAMES[BINTOTABLE[INDEX].PCLOP]
		pcmxtypes("Inplace mixed",&x,y)
	fi

!var_unshare(&z)
	var_storeptr(px,&x)
end

global proc var_inplace_unary(variant px, ref proc(variant) fnneg)=
	varrec x

!CPL "INPLACE UNARY"

	var_loadptr(px,&x)
	fnneg^(&x)
	var_storeptr(px,&x)
end

global proc var_loadptr(variant x,y)=
!y:=x^

	case x.tag
	when trefvar then
		y^:=(x.varptr)^
		if y.hasref then
			++y.objptr.refcount
		fi

	when trefpack then
		var_loadpacked(x.ptr,x.elemtag,y,nil)

	when trefbit then
		var_loadbit(x.ptr, x.bitoffset, x.elemtag, x.bitlength, y)

	else
		pcustype("var_loadptr",x)
	esac
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

	case p.tag
	when trefvar then
		dest:=p.varptr
		var_unshare(dest)
!		var_share(q)
		dest^:=q^

	when trefpack then
		var_storepacked(ref byte(p.ptr),q,p.elemtag)
!		var_unshare(q)

	when trefbit then
		var_storebit(p.ptr,p.bitoffset,q,p.elemtag,p.bitlength)

	else
		pcustype("var_popptr",p)
	esac
end

global proc var_loadbit(ref byte p,int shift,t,bitlength,variant dest) =
!t is tu1/tu2/tu4 load bitfield from p^ at given bit offset, to dest
	ref word pd
	word mask

	dest.tagx:=tint
	case (t)
	when tu1 then
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

	when tu2 then
		dest.value:=(p^ iand (3<<shift))>>shift
	when tu4 then
		dest.value:=(p^ iand (15<<shift))>>shift
	else
		pcustype_t("loadbit",t)
	esac

end

global proc var_storebit(ref byte p,int shift,variant q,int t,bitlength) =
!t is tu1/tu2/tu4 store bitfield to p^ at given bit offset, from dest
!shift will be 0,1,2,3,4,5,6,7
	ref word pd
	byte bb
	word mask1,mask2,newvalue

	if q.tag<>tint then
		pcerror("storebit not int")
	fi

	case (t)
	when tu1 then
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

	when tu2 then
		p^:=(p^ iand inot(3<<shift)) ior ((q.value iand 3)<<shift)
	when tu4 then
		p^:=(p^ iand inot(15<<shift)) ior ((q.value iand 15)<<shift)
	else
		pcustype_t("storebit",t)
	esac
end

global proc var_convert(variant x, int t, variant dest)=
!convert x to type t and store new value in dest
!dest will be different location from x
	int s,tbase
	i64 aa
	varrec bn

	dest^:=x^

	s:=x.tag
!	if s=t and s<tlist then		!same type
	if s=t then		!same type
		return 							!Note: heap types such as arrays must match on elemtypes too
	fi
	tbase:=t

	dest.tag:=t			!assume works, so pre-set tag

	case s
	when tint then
		case tbase
		when tint then			!no changes needed
		when treal then
			dest.xvalue:=x.value
		when tdecimal then
!			var_make_dec_int(sptr.value,dest)
			var_make_dec_int(x.value,dest)
!		elsif ttbasetype[t]=tenum then
!			dest.tag:=tenum
!			dest.elemtag:=t

		else
			pcustype_t("conv int=>",t)
		esac

	when treal then
		case tbase
		when tint then
			dest.value:=x.xvalue
		else
			pcustype_t("conv real=>",t)
		esac

	when trefpack,trefvar,trefbit then
		case ttbasetype[tbase]
		when tint then
		when trefpack then
			dest.tag:=trefpack
			dest.elemtag:=tttarget[t]
		else
			pcustype_t("conv ptr=>",t)
		esac
	when tstring then
		case tbase
		when tlist then
			var_convert_string_list(x,t,dest)

		when tdecimal then
			var_make_dec_str(x.objptr.strptr, x.objptr.length, dest)
		when tstring then
		else
			pcustype_t("string=>",t)
		esac

	when ttype then
		if tbase<>tint then
			pcustype_t("type=>",t)
		fi

	when tdecimal then
		case (tbase)
		when tint then
			aa:=var_convert_dec_int(x)
			dest.tagx:=tint
			dest.value:=aa

		else
			pcustype_t("decimal=>",t)
		esac

!	when tenum then
!		case tbase
!		when tint then			!no changes needed
!			dest.tagx:=tint
!		when tenum then
!			dest.elemtag:=x.elemtag
!		else
!			pcustype_t("conv enum=>",t)
!		esac

	else
		pcmxtypestt("Convert s.t",s,t)
	esac

end

global function var_gethashvalue(variant p)int=
	int hsum,csum,c,n,i,result
	ref char s,s0
	object q

	case p.tag
	when tstring then
		n:=p.objptr.length
		if not n then return 0 fi
		hsum:=0
		s:=p.objptr.strptr
		to n do
			c:=s++^
			hsum:=(hsum<<4-hsum) +c
		od
		result:=hsum<<5-hsum

		return result iand 0x7FFF'FFFF'FFFF'FFFF		!keep positive

	when tint,treal,trange then
		return p.value
	when tdecimal then
		q:=p.objptr
		if q.length=0 then
			return 0
		else
			return q.num[0]
		fi
	when trecord then
		return int(p.objptr)
	else
		pcustype("Can't hash:",p)
	esac
	return 0
end

global proc var_objtovar(int tag, object p, variant q)=
	q.tagx:=tag ior hasrefmask
	q.objptr:=p
end

global proc var_putdotix_intint(variant a, int index, variant b)=
!a.[index]:=b
!a, b are both ints
	word x:=a.value
	word y:=b.value

	if index not in 0..63 then
		pcerror("int.[int]:= bounds")
	fi

	a.value:=x iand inot (1<<index) ior y<<index
end

global proc var_power(variant a, b)=
	if a.tag<>b.tag then
		var_powermixed(a,b)
		return
	fi

	case a.tag
	when tint then
		a.value:=a.value**b.value
	when treal then
!		a.xvalue:=a.xvalue**b.xvalue
		a.xvalue:=pow(a.xvalue,b.xvalue)
	when tdecimal then
		var_power_dec(a,var_convert_dec_int(b))
	else
		pcustype_t("power", a.tag)
	esac
end

global proc var_powermixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		newtag:=treal
		a.xvalue:=pow(a.value,b.xvalue)
		
	when pr(treal,		tint)     then
		a.xvalue:=pow(a.xvalue,b.value)

	when pr(tdecimal, tint) then
		var_power_dec(a,b.value)

	else
		pcmxtypes("Powermixed",a,b)
	esac

	a.tag:=newtag
end

=== syswin.q 0 1 32/48 ===
!Q standard library - Windows

!===============================
module sysp
module clibp
module winapi
module windows

module gxlib
module bmlib
module console
module winconsts
module wingxlib
module winmessages
module gxmisc
module dates
module smlib
!===============================

=== syslin.q 0 1 33/48 ===
!Q standard library - Linux

!===============================
module sysp
module clibp
module linux
!module winapi

!module gxlib
!module bmlib
!module console
module lincon
!module winconsts
!module wingxlib
!module winmessages
!module gxmisc
!module dates
!module smlib
!===============================

export macro console = lincon
=== sysp.q 0 1 34/48 ===
!Q Main Library

export type rkey=struct	!key info as it's used locally
	u16	charcode
	byte	keycode
	byte	shift
end

export var ncmdparams
export var cmdparams
export var stclock=0

export const tab="\t"

export var readfilesize

export var infinity=$infinity()
export var nan=$nan()

proc start=

	ncmdparams:=getcmdparam()

	cmdparams:=new(list,1..ncmdparams)

	s::=""
	for i:=1 to ncmdparams do
		cmdparams[i]:=getcmdparam(i)
		s+:=cmdparams[i]+" "
    od

	sreadln(s)
end

export proc reporterror(m)=
#print "Error:" followed by message m. Is that it? Count as being deprecated

	println "Error:",m
end

export func splitstring(s,?sep)=
#split up the string s into strings separated by the sep sequence
#return a list of all the individual strings, excluding the sep seq

	if s="" or sep="" then return (s,) fi

	a::=()
	ns:=0

	if sep.isvoid then			!use any white space of variable width

		whitespace:=(' ','\t',13,10)

		s:=s+chr(0)
		p:=&s
		t::=""
		instr:=0

		while c:=p++^ do
			if c in whitespace then
				if instr then
					a[++ns]:=t
					t::=""
					instr:=0
				fi
			else
				instr:=1
				t+:=c
			fi
		od
	
		if t then
			a[++ns]:=t
		fi

		return a

	else
		do
			n:=sep inx s
			if not n.isfound then
				a[++ns]:=s
				return a
			fi
			t:=leftstr(s,n-1)
			a[++ns]:=t
			s:=rightstr(s,-(n+sep.len-1))
		od
	fi
	return ""
end

export func joinstrings(a,sep)=
#join the strings in list, using the given separator string
#return new single string
	if a.upb=0 then return "" fi
	s:=a[1]
	for i:=2 to a.upb do
		s:=s+sep+a[i]
	od
	return s
end

export proc abort(s)=
#Print message, pause for keypress, then stop the interpreter with stopcode 1

	println "Abort:",s,"Error"
	waitkey()
	stop 1
end

export func extractpath(fs)=
#fs is a full filespec string
#extract any path from it and return that; ie, strip the filename
#otherwise return ""
	l:=fs.len
	for i:=l downto 1 do
		if chr(fs.[i]) in "\\/:" then
			return leftstr(fs,i)
		fi
	od
	return ""
end

export func extractfile(fs)=
#return filename portion of path fs
	p:=extractpath(fs)
	if p="" then return fs fi
	return rightstr(fs,-p.len)
end

export func extractbasefile(fs)=
#return filename portion of path fs
	f:=extractfile(fs)
	if f="" then return "" fi
	e:=extractext(f)
	if e.len then
		f:=leftstr(f,-e.len)
	fi
	if rightstr(f)="." then
		f:=leftstr(f,-1)
	fi
	return f
end

export func extractext(fs,period=0)=
#extract extension part of filespec fs
#endings of "xxx" (no extension) and "xxx." both return ""
#with period=1, then "xxx" returns "" and "xxx." returns . (so can be used to
#override default extensions)

	f:=extractfile(fs)
	if f="" then return "" fi
	e:=""
	do
		n:="." inx f
		if n.isfound then
			e:=rightstr(f,-n)
			if e="" then		!. ending
				return (period.defined and period|"."|"")
			fi

			f:=e
		else
			exit
		fi
	od

	return e
end

export func changeext(file,newext,soft=0)=
#normally face a change of extension to the file spec
#use soft=1 to only change extension if no extension is present (a "." ending is an extension)
	ext:=extractext(file)

	p:=extractpath(file)
	bf:=extractbasefile(file)
	ep:=extractext(file,1)

	if soft and ep<>"" then return file fi		!has extension, don't change!

	if newext="" then
		return p+bf
	elsif leftstr(newext)="." then
		return p+bf+newext
	else
		return p+bf+"."+newext
	fi
end

export func addpath(path,file)=
#If file doesn't already have an absolute path (here, starting with \ / or ?:)
#then prepend 'path', which must end with \ or /
	if leftstr(file) in "/\\." or file.len>=2 and file.[2]=":" then
		return file
	fi
	return path+file
end

export func addext(file,ext)=
#add extension to filename, if it doesn't already have it's own extenstion

	if extractext(file,1)="" then
		return changeext(file,ext)
	fi
	return file
end

export func replacestr (s,a,b)=
#if string a exists in s, then replace with b
#return original or modified s
	do
		n:=a inx s
		if not n.isfound then return s fi
		s:=leftstr(s,n-1)+b+rightstr(s,1-n-a.len)
	od
	return ""
end

export func parsecmdparams(cmd)=
#cmd consists of:
#blocks of text separated by whitespace or commas
#each block is one of these formats
# ...		param only
# /...		consists of switches only
# .../...	param followed by switches
#return of (params,switches), where each is a list of strings
#note that any correspondence between params and switches is lost; all switches assumed
#to be global, but can appear anywhere
#NOTE: cmd can also already be a list of blocks

	const dash="-"

	if cmd.islist then
		blocks:=cmd
	else
		sreadln(cmd)
		blocks::=()
		do
			read a:"s"
			if a="" then exit fi
			blocks append:=a
		od
	fi

	params::=()
	switches::=()

	for x in blocks do
		n:=dash inx x
!		if n=0 then		!pure param
!			params append:=x
		if n=1 then		!pure switches
			switches concat:=splitstring(convlc(rightstr(x,-1)),"/")
		else			!param followed by switches
			params append:=x
!			params append:=leftstr(x,n-1)
!			switches concat:=splitstring(convlc(rightstr(x,-n)),"/")
		fi
	od

	return (params,switches)
end

export proc waitsec(secs)=
#wait for given number of seconds, which can be a float. Call sleep()
	sleep(int(secs*1000))
end

export func cmd_getswitches=
#params is a list of strings, which
#read all switches, and return a list of switch names (minus the "/")
#each string can have more than one switch
#some switches can follow a name in a string

	switches::=()
	for i:=1 to cmdparams.upb do		!use 1..len in case called on <cmdparams> which has lwb 0
		s:=cmdparams[i]
		if leftstr(s) in "-/" then
			switches append:=convlc(rightstr(s,-1))
		fi
	od
	return switches
end

export func cmd_getparams=
#params is a list of strings
#return list of actual params, not including any switches
#switches are read separately using cmd_getswitches, but are not associated with
#specific params. That would need to be done here (when / is detected in the middle
#of a param, then make use readswitches. But to return that info, may be best to
#create a parallel function)

	cmds::=()

	for i:=1 to cmdparams.upb do
		pm:=cmdparams[i]
!for pm in params do
		if leftstr(pm) in "/-" then
			nextloop
		fi
!	n:="/" in pm
!	if n=0 then
			cmds append:=pm
!	else
!		cmds append:=leftstr(pm,n-1)
!	fi
	od
	return cmds
end

export func starttimer=
#Start timer and remember ticks at this point
	return stclock:=ticks()
end

export func stoptimer=
#Return number of ticks since starttimer was called, or last stoptimer
#as the count is reset

 	d:=ticks()-stclock
	stclock:=ticks()
	return d
end

export func bnfact(n)=
#n is limited to 9 million million million

	if n<=2 then
		return longint(n)
	fi

	f:=1L
	g:=2L
	to n-1 do
		f:=f*g
		g:=g+1L

	od
	return f
end

export proc isort(a,?ll,?rr)=
#inplace quicksort of a, which is anything that is indexable
#ll rr are used for recursive calls
	if ll.isvoid then
		ll:=a.lwb
		rr:=a.upb
	fi

	i:=ll
	j:=rr

	pivot:=a[(ll+rr)%2]

	repeat
		while pivot>a[i] and i<rr do ++i od
		while pivot<a[j] and j>ll do --j od
		if i<=j then
			swap(a[i],a[j])
			++i
			--j
		fi
	until i>j
	if ll<j then isort(a,ll,j) fi
	if i<rr then isort(a,i,rr) fi
end

export func sort(a)=
#quicksort a and return newly sorted list. Duplicates a then uses isort
	b::=a
	isort(b)
	return b
end

export func pcerror(m)=
#Force an interpreter error; advantage is that source location is reported.

	println "Internal error:",m
	a:=b+c
	return 0
end



!=========================================
export proc insert(&a, b, c)=
#insert value c just before index b
#c is always a single value; to insert a sequence c, use insertn()
	n:=a.upb
	a[n+1]:=c
	for i:=n downto b do
		swap(a[i+1],a[i])
	od
end

export proc isort2(a,b,?ll,?rr)=
#Like isort but also sorts b in parallel; sort order is determined by a however
	if ll.isvoid then
		ll:=a.lwb
		rr:=a.upb
	fi

	i:=ll
	j:=rr

	pivot:=a[(ll+rr)%2]

	repeat
		while pivot>a[i] and i<rr do ++i od
		while pivot<a[j] and j>ll do --j od
		if i<=j then
			swap(a[i],a[j])
			swap(b[i],b[j])
			++i
			--j
		fi
	until i>j
	if ll<j then isort2(a,b,ll,j) fi
	if i<rr then isort2(a,b,i,rr) fi
end

export func left(a,n=1)=
#return leftmost n elements of a (default left element)
#when n is negative, all except rightmost -n

	if n>=0 then
		return take(a,n)
	else
		return take(a,a.len+n)
	fi
end

export func right(a,n=1)=
#return rightmost n elements of a (default right element)
#when n is negative, all except leftmost -n

	if n>=0 then
		return drop(a,a.len-n)
	else
		return drop(a,-n)
	fi
end

export func reverse(a)=
#return reversed version of a
#when 0, returns empty
#when 1 element, returns a distinct, writeable copy

	if a.len=0 then
		return makeempty(a)
	fi
	b::=a

	if a then
		for i in a.bounds do
			b[a.upb-i+a.lwb]:=a[i]
		od
	fi
	return b
end

export func expandrange(a,step=1)=
#Turn range a into a list of inclusive values
	x::=()
	i:=a.lwb
	while i<=a.upb do
		x append:=i
		i+:=step
	od
	return x
end

export func head(a)=
#return first element, or empty when empty

	if a.len then
		return a[a.lwb]
	else
		return makeempty(a)
	fi
end

export func tail(a)=
#return all except the first element
#returns empty when only 0 or 1 elements

	case a.len
	when 0,1 then
		return makeempty(a)
	esac
	return a[2..$]
end

export func init(a)=
#return all except last element
#returns empty when only 0 or 1 elements
	case a.len
	when 0,1 then
		return makeempty(a)
	esac
	return a[a.lwb..$-1]
end

export func last(a)=
#return last element, or empty
	if a.len then
		return a[$]
	else
		return makeempty(a)
	fi
end

export func take(a,n)=
#return first n elements from list/string a
#returns () or "" when a is empty
#n > 0 (n<=0 returns empty)

	if a.len=0 or n<=0 then
		return makeempty(a)
	fi
	if n>=a.len then
		return a
	fi
	return a[a.lwb..a.lwb+n-1]
end

export func drop(a,n)=
#skips first n elements of a then returns the rest
#returns () when empty, or skipping the whole list
#n >= 0

	if a.len=0 or n>=a.len then
		return makeempty(a)
	fi
	if n<=0 then
		return a
	fi
	return a[a.lwb+n..$]
end

export func zip(a,b)=
#return a list consisting of alternate elements from a and b
#uses smaller of the two dimensions

	n:=min(a.len,b.len)
	c::=()

!	j:=a.lwb; k:=b.lwb
	(j, k) := (a.lwb, b.lwb)

	to n do
		c append:=a[j++]
		c append:=b[k++]
	od
	return c
end

export func repeatlist(a,n)=
#duplicate a n times, and return the result
#this ought to be built-in as a*n, but that's only implemented for a.len=1

	b:=makeempty(a)
	to n do
		b concat:=a
	od
	return b
end

!export func minimum(a)=
!#return minimum value of elements in a
!	if not a then
!		return void
!	fi
!	x:=head(a)
!	for y in tail(a) do
!		x min:=y
!	od
!	return x
!end
!
!export func maximum(a)=
!#return maximum value of elements in a
!	if not a then
!		return void
!	fi
!	x:=head(a)
!	for y in tail(a) do
!		x max:=y
!	od
!	return x
!end
!
!export func sumlist(a)=
!# apply "+" between all elements of a, and return result
!# all elements must be compatble (all strings or all numbers for example)
!# returns void then a is empty, or head(a) when just one element
!
!	if not a then
!		return void
!	fi
!	x:=head(a)
!	for y in tail(a) do
!		x +:=y
!	od
!	return x
!end

export proc delete(&a,?b)=
#delete element b
	n:=a.upb
	if b.isvoid then b:=n fi

	if n=b=1 then
		a::=()
		return
	fi

	if b>n then return fi
	if b<a.lwb then return fi
	for i:=b to n-1 do
		swap(a[i],a[i+1])			!swap is faster for complex elements
	od
!a[n]:=0		!don't leave any heap data beyond new end of list

	resize(a,n-1)
end

export proc resize(&a,n)=
#hange the upper bound of a to n

	if n<a.lwb then
		a:=makeempty(a)
		return
	fi

	a::=a[a.lwb..n]			!duplication forces original to be freed
end
 
export func makebits(data,t=bit)=
#turn data (list, array, or bit array of different type) into a bit array

	a:=new(bits,t,data.bounds)
	for i:=data.lwb to data.upb do
		a[i]:=data[i]
	od
	return a
end

export func makearray(data,t=i64)=
#turn data (list, array of different type, or bit array) into an array of 
#given element type

	a:=new(array,t,data.bounds)
	for i:=data.lwb to data.upb do
		a[i]:=data[i]
	od
	return a
end

export func tolist(a)=
#convert a, a string, array or bits, to a list, and return that list

	case a.basetype
	when array,string,bits then
		b:=new(list,a.bounds)
		for i,x in a do
			b[i]:=x
		od
		return b
!	when string then
!		b:=new(list,a.len)
!		i:=1
!		for i,x in a do
!			b[i++]:=x
!		od
!		return b

	when list then
		return a
	else
		pcerror("tolist:"+tostr(a.type))
	esac
	return 0
end

export func toarray(a,?t)=
#convert a, a list, string, array or bits, to an array, and return that array
#can be used to turn one array type into another
	case a.basetype
	when list then
		if t.isvoid then
			if a then
				t:=a[a.lwb].type
			else
				t:=i32
			fi
		fi

	when bits then
		if t.isvoid then
			t:=byte
		fi

	when string then
		if t.isvoid then t:=byte fi
		b:=new(array,t,a.len)
		foreach i,x in a do
			b[i]:=x
		od
		return b
	when array then
		if t.isvoid then
			return a
		fi
		u:=e.elemtype
		if t=u then return a fi
	else
		pcerror("toarray:"+tostr(a.type))
	esac
	b:=new(array,t,a.bounds)

	for i,x in a do
		b[i]:=x
	od
	return b
end

export func tobits(a,t=bit)=
#convert a, a list, array or other bit array, into a bit array

	case a.basetype
	when list,array then

	when bits then
		if a.elemtype=t then
			return a
		fi

	else
		pcerror("tobits:"+tostr(a.type))
	esac
	b:=new(bits,t,a.bounds)
	for i,x in a do
		b[i]:=x
	od
	return b
end

export func listtostring(a)=
#a should be a list or array
#interpreter elements as characters and form a single string
	s:=""
	for x in a do
		s+:=chr(x)
	od
	return s
end

export func qversion=
	return "4.0"
end

export proc issort(a,?ll,?rr)=
#Version of isort that works with dot-indexing

	if ll.isvoid then
		ll:=a.lwb
		rr:=a.upb
	fi

	i:=ll
	j:=rr

	pivot:=a.[(ll+rr)%2]

	repeat
		while pivot>a.[i] and i<rr do ++i od
		while pivot<a.[j] and j>ll do --j od
		if i<=j then
			swap(a.[i],a.[j])
			++i
			--j
		fi
	until i>j
	if ll<j then issort(a,ll,j) fi
	if i<rr then issort(a,i,rr) fi
end

export func ssort(a)=
#Version of sort() with dot-indexing, eg. strings, or int bits

	b::=a
	issort(b)
	return b
end

export func maketable(rows, cols, initval=0)=
#Create a table: a rectangular list, set to either 0 or to initval
#Each rows/cols is a range, or length

	row:=new(list,cols,initval)

	table::=new(list,rows)
	if rows.isint then rows:=1..rows fi

	for i in rows do
		table[i]::=row
	od

	return table
end

export func mapv(op,a)=
#Apply operator or suitable unary func to all elements of vector a,
#and return new list 
	b::=makeempty(a)
	for i,x in a do
			b[i]:=mapss(op,x)
	od
	return b
end

export func mapvv(op,a,b)=
#Apply op or func between corresponding elements of vectors a and b
	c::=makeempty(a)
	for i,x in a do
		c[i]:=mapss(op,x,b[i])
	od
	return c
end

export func mapvs(op,a,bs)=
#Apply op or func between elements of vector a and single value bs
	c::=makeempty(a)
	for i,x in a do
		c[i]:=mapss(op,x,bs)
	od
	return c
end

export func mapsv(op,as,b)=
#Apply op or func between elements of single value as and vector b
!	c::=makeempty(b)
	c::=()
	for i,x in b do
		c[i]:=mapss(op,as,x)
	od
	return c
end

export func openfile(name,option="rb")=
#Open a file for reading. Uses C's fopen and default option is for binary mode
#Return a valid file handle, which is an i64 value, or 0 when not found
	if not name.isstring or name="" then
		return 0
	fi
	return fopen(name,option)
end

export func createfile(name,options="wb")=
#Create a new file and return its handle, or 0 if there was an error
	if not name.isstring or name="" then return 0 fi
	return fopen(name,options)
end

export func closefile(f)=
#close the file associated with handle f
	return fclose(f)=0
end

export func checkfile(name)=
#return 1 if file name exists, otherwise 0
	file:=fopen(name,"rb")
	if file=0 then return 0 fi
	fclose(file)
	return 1
end

export func eof(f)=
#return 1 if at eof on currently open file handle f
	c:=fgetc(f)
	if c=-1 then return 1 fi

	ungetc(c,f)
	return 0
end

export func getfilesize(f)=
#return size of bytes of currently open file f
	p:=ftell(f)			!p=current position
	fseek(f,0,2)		!get eof position
	size:=ftell(f)		!size in bytes
	fseek(f,p,0)		!restore file position
	return size
end

export func getfilesize64(f)=
#return size of bytes of currently open file f
	p:=_ftelli64(f)			!p=current position
	_fseeki64(f,0,2)		!get eof position
	size:=_ftelli64(f)		!size in bytes
	_fseeki64(f,p,0)		!restore file position
	return size
end

export func setfilepos(f,offset)=
#set position in file f to given byte offset
	return fseek(f,offset,0)
end

export func getfilepos(f)=
#return current file position
	return ftell(f)
end

export func readrandom(f,mem,offset,size)=
#read size bytes from file f, to memory at mem, from given offset
#returns number of bytes read
#mem needs to be a pointer
#new file offset will be offset+size (or offset+byte read if smaller)
	fseek(f,offset,0)
	return fread(mem,1,size,f)
end

export func writerandom(f,mem,offset,size)=
#write size bytes from memory at mem, to current file f from given offset
#returns bytes written
	fseek(f,offset,0)
	return fwrite(mem,1,size,f)
end

export func readbytes(f,mem,size)=
#read size bytes from current position in file f to mem
	return fread(mem,1,size,f)
end

export func writebytes(f,mem,size)=
#write size bytes from mem to current position in f
	return fwrite(mem,1,size,f)
end

export func inbyte(file)=		!INBYTE
	return fgetc(file)
end

export func inword(file)=		!INWORD
	bb:=fgetc(file)
	return fgetc(file)<<8+bb
end

export func inlong(file)=		!INLONG
	ww:=inword(file)
	return inword(file)<<16+ww
end

export proc outbyte(file,x)=		!OUTBYTE
!writerandom(file,&x,getfilepos(file),1)
	fputc(x,file)
end

export proc outword(file,x)=		!OUTWORD
	outbyte(file,x iand 255)
	outbyte(file,x.[15..8])
end

export proc outlong(file,x)=		!OUTLONG
	outword(file,x iand 65535)
	outword(file,x>>16)
end

export func instring(file)=		!INSTRING
	s::=""
	do
		c:=inbyte(file)
		if c=0 then return s fi
		s+:=c
	od
	return s
end

export func appendfile(a,b)=
#append line-based text file a to file b

	f:=openfile(a)
	if f=0 then return 0 fi

	h:=openfile(b,"ab")
	if h=0 then return 0 fi

	while not eof(f) do
		readln @f,x:"l"
		println @h,x
	od

	closefile(f)
	closefile(h)
	return 1
end

export func readblockfile(filename,doetx=0)=
#read text file into a memory block
#block is allocated here
#return byte pointer to start of block, or nil
#doetx=1 to add etx byte to end

	f:=openfile(filename)
	if f=0 then return nil fi

	n:=getfilesize(f)
	readfilesize:=n

	s:=malloc(n+doetx)
	if s=0 then abort("Readfile/Malloc fails") fi
	sptr:=makeref(s,byte)

!	readrandom(f,&s,0,n)
	readrandom(f,s,0,n)

	if doetx then
		(sptr+n)^:=26
	fi

	closefile(f)
	return sptr
end

export func readstrfile(filename,doetx=0)=
#read text file into a single string
#return string, or 0 if there was an error

	f:=openfile(filename)
	if f=0 then return 0 fi

	n:=getfilesize(f)
	readfilesize:=n

	ptr:=malloc(n+1+doetx)
	if ptr=0 then abort("Readfile/Malloc fails") fi

	readrandom(f,ptr,0,n)
	if doetx then
		(makeref(ptr,byte)+n)^:=26
	fi

	closefile(f)

	s::=makestr(ptr,n+doetx)

	free(ptr)
	return s
end

export func writestrfile(filename,s)=
#read text file from a single string
#return status

	f:=createfile(filename)
	if f=0 then return 0 fi

	writerandom(f,makeref(s,byte),0,s.len)

	return closefile(f)
end

export func readbinfile(filename)=
#read binary file into byte array
#return () (empty list not array) on error

	f:=openfile(filename)
	if f=0 then return 0 fi

	n:=getfilesize(f)
	readfilesize:=n

	a:=new(array,byte,n)
	readrandom(f,&a,0,n)

	closefile(f)
	return a
end

export func writebinfile(filename,a)=
#write binary file from byte array a
#return status 1/0

	f:=createfile(filename)
	if f=0 then return 0 fi

	writerandom(f,(&a),0,a.len)

	closefile(f)
	return 1
end

export func writeblockfile(filename,p,length)=
#return status 1/0

	f:=createfile(filename)
	if f=0 then return 0 fi

	if not writerandom(f,p,0,length) then return 0 fi

	closefile(f)
	return 1
end

export func erasefile(filename)=
#delete given file, return status (check msdn)
	return remove(filename)
end

export func renamefile(oldfilename,newfilename)=
#rename file, return status (check msnd)
	return rename(oldfilename,newfilename)
end

export func readtextfile(file)=
#read text file into a list of strings; one per line
#return list, or 0 on error
	f:=openfile(file)
	if not f then
		return 0 
	fi

	readfilesize:=getfilesize(f)
	a::=()

	while not eof(f) do
		a append:= sreadln(f)
	od
	closefile(f)
	return a
end

export func writetextfile(file,a)=
#write list of strings <a> as a text file <file>
	f:=createfile(file)
	if not f then return 0 fi

	for i:=a.lwb to a.upb do
		println @f,a[i]
	od
	closefile(f)
	return 1
end

export func readbinaryfile(filename,t)=
#read binary file consisting of an array of type t values, into array of t
#return () (empty list not array) on error

	f:=openfile(filename)
	if f=0 then return () fi

	n:=getfilesize(f)
	readfilesize:=n
	elems:=n%t.bytes

	a:=new(array,t,elems)
	readrandom(f,&a,0,n)

	closefile(f)
	return a
end

export func writebinaryfile(filename,data)=
#write binary file from array of a fixed type to a file
#return 1/0 status
	return writeblockfile(filename,&data,data.bytes)
end

export func confirm(m,caption="Confirm",default=1)=
#Pop-up box to ask for confirmationdefault=1/2/3 for yes/no/cancel button
#Return 1 or 0

	flags:=0x20000+0x20	!foreground window/question mark icon
	flags ior:=3		!yes/no/cancel

	flags ior:=(default|0,0x100,0x200|0)

	status:=messagebox(nil,m,caption,flags)
	return status=6
end

export func messagebox(a=nil,mess,caption="Caption",d=0)=
#Standard Windows' Messagebox
	return messageboxa(nil,mess,caption,d)
end

export proc beep1=
#Standard beep
	messagebeep(0)
end

export proc mem(mess)=
	static var startmem
	if startmem.isvoid then
		startmem:=$smallmemtotal()
	fi
	println mess,,":",$smallmemtotal()-startmem
end

export func reduce(op, a)=
	x:=head(a)
	for y in tail(a) do
		x:=mapss(op,x,y)
	od
	x
end
=== windows.q 0 1 35/48 ===
export func dirlist(s,t=1)=
#s is a export filename (eg. "*.dwg") with possible drive/path; scan
#directory for all matching files and return as a list of names
#also returns total no. of files so far
#t= +1	Include normal files, no sub-directory names
#t= +2  Include directories
#t= +3  Include all files including directories
#t= +4  Convert to lower case
#t=  0  Defaults to +1

!CPL "DIRLIST/WINLIB"

	if t.isvoid then t:=1 fi			!files only

	nfiles:=0
	data::=()
	file:=new(ws_finddata)

	if (hfind:=findfirstfile(s,&file))<>-1 then	!at least one file
		repeat
			if (file.fileattributes iand 16) then		!this is a directory
				if (t iand 2)=0 then goto skip fi		!no directories
			else						!this is a file
				if (t iand 1)=0 then goto skip fi
			fi
			++nfiles
			if (t iand 4) then				!to lower case
				data[nfiles]:=convlc(file.filename)
			else
				data[nfiles]::=file.filename
			fi
	skip:
		until not findnextfile(hfind,&file)
		findclose(hfind)
	fi
	return data
end

export func setcurrdir(newdir)=
#Set current directory; return Windows' status code
	return setcurrentdirectory(newdir)
end

export func getcurrdir=
#Return current directory name, always ends with \ or /
	a:=new(array,byte,256)
	n:=getcurrentdirectory(a.len,&a[1])

	if n then
		dir::=makestr(&a[1],n)
	else
		dir:=""
	fi

	if not (rightstr(dir) in "\\/") then dir +:= "\\" fi
	return dir
end

export func createdir(name)=
#Create a new directory
	return createdirectory(name,0)
end

export func direxists(path)=
#Return 1 if directory path exists
	const file_attribute_directory=16
	const invalid_file_attributes=-1

	attrib := getfileattributesa(path)

	return attrib<>invalid_file_attributes and (attrib iand file_attribute_directory)
end

=== linux.q 0 1 36/48 ===
export func dirlist(s,t=1)=
ABORT("DIRLIST")
0
end

export func setcurrdir(newdir)=
#Set current directory; return Windows' status code
	system("cd "+newdir)=0
end

export func getcurrdir=
#Return current directory name, always ends with \ or /
ABORT("GETCURRDIR")
0
end

export func createdir(name)=
#Create a new directory
	if not direxists(name) then
		system("mkdir "+name)=0
	else
		1
	fi
end

export func direxists(path)=
#Return 1 if directory path exists
	checkfile(path+"/.")

end

=== clibp.q 0 1 37/48 ===
importdll msvcrt=
!importdll msvcr100=
!importdll msvcr120=
	func "malloc"        (i64)ref byte
	func realloc(i64, i32)i64
	proc free        (i64)
	proc memset      (ref byte, i32, i32)
	proc memcpy      (ref byte, ref byte, i32)
	func memcmp      (ref byte, ref byte, i32)i32
!	func clock       :i32
	func ftell       (i64)i32
	func _ftelli64   (i64)i64
	func fseek       (i64, i32, i32)i32
	func _fseeki64   (i64, i32, i32)i64
	func fread       (ref byte, i32, i32, i64)i32
	func fwrite      (ref byte, i32, i32, i64)i32
	func getc   (i64)i32
	func ungetc (i32, i64)i32
	func fopen       (stringz, stringz)i64
	func fclose      (i64)i32
	func fgets       (ref byte, i32, i64)ref byte
	func remove      (stringz)i32
	func rename      (stringz, stringz)i32
	func getchar     :i32
	proc putchar     (i32)
	proc setbuf      (i64, i64)

	func rand        :i32
	proc srand       (i32)

	func puts        (stringz)i32
	func printf      (stringz, ...)i32

	func sprintf     (stringz, stringz, ...)i32

	func sscanf      (stringz, stringz, ...)i32
	func isalpha     (i32)i32
	func tolower     (i32)i32
	func strlen      (ref byte)i32
	func atoi        (stringz)i32

!   clang func system      (stringz)i32

	func fgetc  (i64)i32
	func fputc  (i32,  i64)i32
	func fprintf     (i64, stringz, ...)i32
	func fputs       (stringz,  i64)i32
	func feof        (i64)i32
!   clang func getch       :i32
	func _getch      :i32
	proc fflush      (ref void)
	proc tcflush     (int, int)

end

global const c_eof     = -1
global const seek_set  = 0
global const seek_curr = 1
global const seek_end  = 2

=== smlib.q 0 1 38/48 ===

export var popuplist::=()
export var focuslist::=()
export var npopups=0
export var message
export var messw, messa, messb
export var wpopup=nil

record blockrec=
	var posx, posy				!pixel position of top left of block: relative to other
								!blocks, later within client area of containing window
	var dimx,dimy				!'client' area of block, including margins, cells and gaps
	var celldimx, celldimy		!size of each cell, in pixels
	var cellsx, cellsy			!number of identically-svert hoz and vertical controls
	var gapx, gapy				!gap between cells, in pixels
	var marginx, marginy		!margins around all cells
	var labelwidth				!for edit boxes, how many pixels on left are for label
	var cellposx,cellposy		!position of top left cell within block
	var pitchx,pitchy			!1st cell is at for edit boxes, how many pixels on left are for label
	var blockstyle				!style record for blocks
	var name					!name for debugging
	var dir						!'H' or 'V' for stepping direction

end

var blocklist::=()
var nblocks=0
var	currblock =nil				!current blockrec
var	currgroup =nil				!rwindow corresponding to currblock
var	currpopup =nil				!rwindow for menu to contain current set of blocks
var cellx, celly				!current cell within current block
var slposx,slposy,sldir			!set by smcreate in case sl-functions are used

!proc showblockinfo(block)=
!println "Block:      ",(block.name.isdef|block.name|"")
!println "Pos:        ",block.posx,block.posy
!println "Dim:        ",block.dimx,block.dimy
!println "CellDim:    ",block.celldimx,block.celldimy
!println "Gap:        ",block.gapx,block.gapy
!println "Cells:      ",block.cellsx,block.cellsy
!println "Gaps:       ",block.gapx,block.gapy
!println "Margins:    ",block.marginx,block.marginy
!println "Labelwidth: ",block.labelwidth
!println "Cellpos:    ",block.cellposx,block.cellposy
!println "Pitch:      ",block.pitchx,block.pitchy
!println "Dir:        ",block.dir
!println
!end

export proc sminit=
	blocklist::=()
	nblocks:=0
end

export func smdefblock(?dim,cells=1,style="",gap=0,labeldim="",margin=0,dir='V')=
!define a matrix of cells, all the same size, to be used as controls
!dim	is a the size of each cell, as pixel dims, or as a sample string
!cells	is a the hoz and vert cell count. Or it can be an int for vert column only
!gap	is the inter-cell gap, specified in pixels. Default is to use chx or chy.
!		gap can be (x,y), or just n for the same gap in hoz and vert
!style	Currently, a string containing various styles and options. Will be
!	compatible with old uses of these functions. Or can be replaced with a dict
!label	When specified, is a sample string givibg the length of the label on the left
!		of edit boxes; can also be a pixel width
!return handle to block
!also add block to export blocklist

	block:=new(blockrec,0)
	block.dir:=dir
!block.dir:='H'

	if dim.isstring then
		block.celldimx:=gxtextwidth(labelfont,dim)+smx*2
		block.celldimy:=chy+smy*2
	else
		(block.celldimx,block.celldimy):=dim
	fi

	if cells.isint then
		block.cellsx:=1
		block.cellsy:=cells
	else
		(block.cellsx,block.cellsy):=cells
	fi

	if gap.isint then
		block.gapx:=gap
		block.gapy:=gap
	else
		(block.gapx,block.gapy):=gap
	fi

	if margin.isint then
		block.marginx:=margin
		block.marginy:=margin
	else
		(block.marginx,block.marginy):=margin
	fi

	if labeldim then
		if labeldim.isstring then
			block.labelwidth:=gxtextwidth(labelfont,labeldim)+smx*2
		else
			block.labelwidth:=labeldim
		fi
		block.celldimx+:=block.labelwidth
	fi

	block.blockstyle:=readstylestr(style)

!CPL "BLOCKSTYLE",STYLE,BLOCK.BLOCKSTYLE

!now work out overall size of the block, and the pitch between cells
!this needs to take account of the frame size of each cell, which depends on
!its border style
!block position is done at a higher level using smorder

	bdx:=bdy:=1				!use border widths of 0 for now (and assume same all round)

	block.pitchx:=block.celldimx+bdx*2+block.gapx		!hoz pitch
	block.pitchy:=block.celldimy+bdy*2+block.gapy		!vert

!CPL "PITCHY",BLOCK.PITCHY,=BDY,=BLOCK.GAPY
!CPL "MARGINY",BLOCK.MARGINY

	block.cellposx:=block.marginx+bdx					!position of client area of 1st cell
	block.cellposy:=block.marginy+bdy

	block.dimx:=block.pitchx*block.cellsx-block.gapx+block.marginx*2
	block.dimy:=block.pitchy*block.cellsy-block.gapy+block.marginy*2

	blocklist[++nblocks]:=block

	return block
end

export func smmenusize(margin=chy)=
!work out overall bounding box for all blocks, and relocate blocks (or set their
!pos values) so that each is positioned within to the bounding box rectangle
!return (dimx,dimy)

!get bounding box in (x1,y1), (x2,y2)
	for i,block in blocklist do
		if i=1 then				!first block
			x1:=block.posx
			y1:=block.posy
			x2:=x1+block.dimx-1
			y2:=y1+block.dimy-1
		else
			x1 min:=block.posx
			y1 min:=block.posy
			x2 max:=block.posx+block.dimx-1
			y2 max:=block.posy+block.dimy-1
		fi
	od

!now, need to relocate each block so they stay at the same position relative to
!each other, but are positioned within the client area of an owner window
!this effectively relocates (x1,y1) to (0,0), so the offset to be applied to
!each block is -(x1,y1), plus (margin,margin)

	for block in blocklist do
		block.posx+:=margin-x1
		block.posy+:=margin-y1
	od

!return (x1-margin,y1-margin,x2-y1+margin*2+1,y2-y1+margin*2+1)
	return (x2-x1+margin*2+1,y2-y1+margin*2+1)
end

export proc smorder(blocks,dir='D')=
!take a blocks, and arrange all in a line, relative to the first
!dir is one of "U", "D", "L", "R" (or can be char codes or in lower case)
!some block elements can be an integer specifing a gap between the blocks.
!the gap is specified in pixels. The default gap is chx for hoz and chy for vertical
!(***I THINK that the gap override is only between two blocks, so needs the reset to
!default after. That mean also that the first list item must be a block***)

	if dir.isstring then
		dir:=asc(convuc(dir))			!"r","R" or 'R' possible, but not 'r'
	fi

	bdx:=bdy:=1						!border widths for the blocks

	dx:=chx+bdx
	dy:=chy+bdy
	firstblock:=1

	for block in blocks do
		if block.isint then			!is a gap
			dx:=block+bdx*2
			dy:=block+bdy*2
			nextloop
		fi
		if firstblock then
			lastblock:=block
			firstblock:=0
			nextloop
		fi
		case dir
		when 'D' then			!add below
			block.posx:=lastblock.posx
			block.posy:=lastblock.posy+lastblock.dimy+dy
		when 'R' then
			block.posx:=lastblock.posx+lastblock.dimx+dx
			block.posy:=lastblock.posy
		when 'U' then			!add above
			block.posx:=lastblock.posx
			block.posy:=lastblock.posy-block.dimy-dy
		when 'L' then
			block.posx:=lastblock.posx-block.dimx-dx
			block.posy:=lastblock.posy
		esac
		lastblock:=block
	od
end

proc showtestmenu(dim)=
	wapplic:=gxcreatewindow(dim:dim,caption:"test")

	for block in blocklist do
		gxbutton(pos:(block.posx,block.posy),dim:(block.dimx,block.dimy),caption:block.name,
		owner:wapplic,style:[ss_border:bs_simplew])
	od

	eventloop()
end

export func smcreate(caption="",?dim,?pos)=
!create a pop-up menu window
!dim ix (x,y) client area size in pixels
!?pos is optional position, but can also existing button, then menu is placed nearby
!Normally used after after series of smdefblock etc calls to setup a menu layout
!Dim usually is a call to smmenusize which exactly contains the blocks
!return handle to window

	if dim.isvoid then dim:=smmenusize() fi

	w:=gxcreatewindow(caption:caption, dim:dim, options:[wf_minmax:0],pos:pos)
!	w:=gxcreatewindow(caption:caption, dim:dim)
	w.windclass:=popup_class
	currpopup:=w
	setforegroundwindow(w.gdi.hwnd)

	slposx:=chx
	slposy:=chy
	sldir:=(dim[1]>dim[2]|'H'|'V')

	wpopup:=w

	oldfocus:=wfocus
	if wfocus then
		gxkillfocus()
	fi

	popuplist[++npopups]:=w
	focuslist[npopups]:=oldfocus		!of underlying window

	return w
end

export func smblock(block,border=0)=
!set block as the current block for subsequent 
!unlike old versions of the library, an actual window is created for the block,
!and a handle to that is returned. That is a child group control.

	wblock:=gxpanel(pos:(block.posx,block.posy),dim:(block.dimx,block.dimy),
		owner:currpopup, style:[ss_border:border])
	currblock:=block
	currgroup:=wblock
	cellx:=celly:=1

	return wblock
end

!export func smpanel=
!!set block as the current block for subsequent 
!!unlike old versions of the library, an actual window is created for the block,
!!and a handle to that is returned. That is a child group control.
!
!	return gxpanel(pos:getsmpos(),dim:getsmdim(),
!		owner:currgroup, style:[ss_border:border])
!end
!
export proc smclose=
!NOTE: for nested menus, ie. invoking another popup menu while one is still
!on the screen, requires:
! * blocklist needs to be moved elsewhere, eg. to data field of current popup rwindow
! * Then smclose can close blocks in that list, not the export one
! * Global blocklist can be reused
! * It might require that the owner window is disabled from being clicked on, but
!   that will be awkward to do without disabling each control within it. Or perhaps
!   this is a check that can be done within process_wmmessage, to see if click-window
!   has an owner that has been disabled.
!    Disabling is one with smcreate, and re-enableing here in smclose

	gxclose(wpopup)
!	for block in blocklist do
!		block:=0
!!		freehandle(block)
!	od

	oldfocus:=focuslist[npopups]
	--npopups
	if npopups then
		wpopup:=popuplist[npopups]
		if oldfocus then
			gxfocus(oldfocus)
		fi
	else
		wpopup:=nil
	fi
end

!export proc smoff=
!	smclose()
!end

proc nextcell=
!step cellx,y to next cell within current block
	if currblock.dir='V' then
		++celly
		if celly>currblock.cellsy then
			celly:=1
			++cellx
		fi
	else					!hoz
		++cellx
		if cellx>currblock.cellsx then
			cellx:=1
			++celly
		fi
	fi
end

func getsmpos=
	return ((cellx-1)*currblock.pitchx+currblock.cellposx,
        (celly-1)*currblock.pitchy+currblock.cellposy)
end

func getsmdim=
	return (currblock.celldimx,currblock.celldimy)
end

func getslpos=
	return (slposx,slposy)
end

func getsldim(s)=
	if s.isint then
		return (s*chx+chx*2,chy+smy*2)
	else
		return (gxtextwidth(labelfont,s)+smx*2,chy+smy*2)
	fi
end

proc nextslcell(dim)=
	if sldir='H' then
		slposx+:=dim[1]+chx
	else
		slposy+:=dim[2]+chy
	fi
end

export func smcmd(caption,id=0,enable=1)=
!create button within current block
!caption can also be an integer code:
! 0		skip this cell (just leave a blank space)
! -1	insert divider line
!when id is omitted or is zero, then creates a static label instead

	if caption.isint then
		case caption
		when 0 then
		when -1 then
		esac
		nextcell()
		return nil
	fi

	if id=0 then
		return smlabel(caption)
	fi

	ss:=[ss_border:bs_ownpanel]

	w:=gxbutton(pos:getsmpos(), dim:getsmdim(), caption:caption, id:id,
		owner:currgroup, style:ss, enable:enable)
	nextcell()
	return w
end

export func smhozscrollbar(id=0)=
	w:=gxhozscrollbar(owner:currgroup, pos:getsmpos(), dim:getsmdim(), id:id,
	style:[ss_border:bs_simplew])

	nextcell()
	return w
end

export func smvertscrollbar(id=0)=
	w:=gxvertscrollbar(owner:currgroup, pos:getsmpos(), dim:getsmdim(), id:id,
	style:[ss_border:bs_simplew])

	nextcell()
	return w
end

export func smlabel(caption)=
	pos:=getsmpos()
	dim:=(currblock.celldimx,currblock.celldimy)

	w:=gxlabel(pos:pos,dim:dim,caption:caption,owner:currgroup)
	nextcell()
	return w
end

export func smarrow(dir,id)=
	pos:=getsmpos()
	dim:=(currblock.celldimx,currblock.celldimy)

	w:=gxarrow(pos:pos,dim:dim,dir:dir,owner:currgroup)
	nextcell()
	return w
end

export func smtoggle(caption,linkvar,id=0,enable=1)=
!create toggle control within current block

	w:=gxtoggle(pos:getsmpos(), dim:getsmdim(), caption:caption,
				linkvar:linkvar,id:id,owner:currgroup, enable:enable,
				style:currblock.blockstyle)

	nextcell()
	return w
end

export func smselect(caption,linkvar,onvalue=1,id=0,enable=1)=
!create toggle control within current block

	w:=gxselect(pos:getsmpos(), dim:getsmdim(), caption:caption,
			linkvar:linkvar,onvalue:onvalue,
			id:id,owner:currgroup, enable:enable, style:currblock.blockstyle)

	nextcell()
	return w
end

export func smeditbox(?caption,linkvar,id=0,enable=1,?style)=
!create toggle control within current block
!CPL "SMED",CURRBLOCK
	pos:=getsmpos()
	dim:=getsmdim()

!CPL =DIM

	if caption.isdef then
		gxlabel(pos:pos, dim:(currblock.labelwidth-chx,dim[2]), caption:caption,
		 owner:currgroup)
		pos[1]+:=currblock.labelwidth
		dim[1]-:=currblock.labelwidth
	fi

	w:=gxeditbox(pos:pos, dim:dim,
		linkvar:linkvar,
		id:id,owner:currgroup, enable:enable, style:getstyle(style))
!	gxdrawmode(w,dm_screenmemory)

	nextcell()
	return w
end

export func smlistbox(linkvar,id=0,enable=1)=

!CPL =CURRBLOCK.CELLSY,"(ROWS)"
!CPL =CURRBLOCK.PITCHY,"(PITCH)"
!CPL =CURRBLOCK.CELLDIMY,"(CELLDIMY)"
!CPL =CURRBLOCK.CELLPOSY,"(OFFSET)"
!CPL =CURRBLOCK.GAPY,"(GAPY)"
!CPL =CURRBLOCK.DIMY,"(DIMY)"
!CPL "LBDIMY=",CURRBLOCK.DIMY-CURRBLOCK.MARGINY*2
!CPL =CURRBLOCK.MARGINY

!$SETDEBUG(1)
	w:=gxlistbox(pos:getsmpos(),
		dim:(currblock.dimx-currblock.marginx*2,currblock.dimy-currblock.marginy*2),
		linkvar:linkvar,
		style:[ss_vscroll:1,
		ss_border:bs_simplew],
		rows:currblock.cellsy,
		pitch:currblock.pitchy,
	!	offset:currblock.cellposy,
		id:id,owner:currgroup)
!CPL "SMLB2"
	return w
end

export func sllabel(caption)=
	pos:=getslpos()
	dim:=getsldim(caption)

	w:=gxlabel(pos:pos,dim:dim,caption:caption,owner:currpopup)
	nextslcell(dim)
	return w
end

export func slcmd(caption,id=201,enable=1)=
!create button within current block
!caption can also be an integer code:
! 0		skip this cell (just leave a blank space)
! -1	insert divider line
!when id is omitted or is zero, then creates a static label instead

	pos:=getslpos()
	dim:=getsldim(caption)
	ss:=[ss_border:bs_simplew]

	w:=gxbutton(pos:pos,dim:dim,caption:caption,id:id,owner:currpopup, 
		style:ss)
	nextslcell(dim)
	return w
end

export func sleditbox(linkvar,width=30,id=0,enable=1)=
	pos:=getslpos()
	dim:=getsldim(width)

	w:=gxeditbox(pos:pos, dim:dim,
		linkvar:linkvar,
		id:id,owner:currpopup)

	nextslcell(dim)
	return w
end

export func smok(caption="OK",enable=1)=
	return smcmd(caption,mm_ok,enable)
end

export func smcancel(caption="Cancel",enable=1)=
	return smcmd(caption,mm_cancel,enable)
end

export func slok(caption="OK")=
	return slcmd(caption,mm_ok)
end

export func slcancel(caption="Cancel")=
	return slcmd(caption,mm_cancel)
end

export proc smokcancel=
	smok()
	smcancel()
end

export proc slinit(w)=
	currpopup:=w
	slposx:=chx
	slposy:=0
	sldir:=(w.dimx>w.dimy|'H'|'V')
end

export proc settab(?a,?b,?c,?d,?e,?f,?g,?h,?i)=
	static var oldtabs

	if not a.defined then
		gxtabstops(oldtabs)
		return
	fi

	oldtabs:=gxtabstops()
	params::=allparams()

!	gxtabstops(allparams())
	gxtabstops(param)
end

export proc smupdatevalue(w)=
	gxupdate(w)
end

export proc setfocus(w,?b)=
	gxfocus(w)
end

export proc askmenu(a)=
	message:=gxaskmess(1)
	messw:=currmess.wind
end

func readstylestr(s)=
!read cell style string s, and return option dict
	d:=new(dict)
	if s="" then return d fi

	s:=convuc(s)

	foreach c in s do
		case c
		when 'X' then d{ss_marktype}:=check_mark
		when 'M' then d{ss_marktype}:=radio_mark
		when 'I' then d{ss_marktype}:=invert_mark
		when 'R' then d{ss_returnmess}:=1
		when 'N' then d{ss_noupdate}:=1
		esac
	od
	return d
end

func getstyle(style)=
	if style.defined then
		return readstylestr(style)
	else
		return currblock.blockstyle
	fi
end
=== winapi.q 0 1 39/48 ===

export type wt_word		= u16
export type wt_bool		= u32
export type wt_dword	= u32
export type wt_wchar	= u16
export type wt_char		= byte
export type wt_ichar	= stringz
export type wt_string	= stringz
export type wt_ptr		= ref byte
export type wt_wndproc	= u64

export type wt_handle	= ref void
export type wt_int		= i32
export type wt_uint		= u32
export type wt_long		= i32
export type wt_wparam	= u64
export type wt_lparam	= u64
export type wt_size		= u64

export type wt_wparam32	= u32
export type wt_lparam32	= u32
export type wt_handle32	= u32
export type wt_ptr32	= u32
export type wt_string32	= u32
export type wt_wndproc32	= u32

export type wt_wparam64	= u64
export type wt_lparam64	= u64
export type wt_handle64	= u64
export type wt_ptr64	= u64
export type wt_string64	= u64
export type wt_wndproc64= u64

export type wt_result	= u64
export type wt_intptr	= u64
export type wt_coord	= u32

export type ws_spoint= struct
	i16 x,y
end

export type ws_srect=struct
	i16 leftx,top, rightx,bottom
end

export type ws_charinfo=struct
	union
		wt_word	unicodechar
		wt_char	asciichar
	end union
	wt_word		attributes
end

export type ws_palette16=[0..15]i32

export type ws_console=struct
	ws_spoint size,pos
	wt_word attributes
	ws_srect window
	ws_spoint maxwindowsize
end

export type ws_consoleex=struct
	i32 recsize
	ws_spoint size,pos
	wt_word attributes
	ws_srect window
	ws_spoint maxwindowsize
	wt_word wpopup
	i32 fullscreen
	ws_palette16 palette
end

export type ws_keyevent = struct $caligned
	wt_word	eventtype
		wt_bool	keydown
		wt_word	repeatcount
		wt_word	virtualkeycode
		wt_word	virtualscancode
		union
			wt_word unicodechar
			wt_char asciichar
		end
		wt_dword controlkeystate
end

export type ws_cursor=struct
	i32 size,visible
end

export var hconsole, hconsolein

export const stdoutputhandle=0xffff_fff5
export const stdinputhandle=0xfffffff6
export const stderrorputhandle=0xfffffff4
export const invalidhandlevalue=0xffffffff

export const maxpathlen=260

type spath=stringz*maxpathlen
type sshort=stringz*14
!
export type ws_filetime=struct
	i32 ftlow
	i32 fthigh
end

export type ws_finddata=struct
	i32		fileattributes
	ws_filetime	creationtime
	ws_filetime	lastaccesstime
	ws_filetime	lastwritetime
	i32		filesizehigh
	i32		filesizelow
	i32		reserved0
	i32		reserved1
	spath		filename
	sshort		shortfilename
end

export type ws_systemtime = struct
	u16	year
	u16	month
	u16	dayofweek
	u16	day
	u16	hour
	u16	minute
	u16	second
	u16	milliseconds
end

export type ws_msg64 = struct $caligned
	ref void	hwnd
	i32		message
	i64		wparam
	i64		lparam
	i32		time
	i32		ptx
	i32		pty
end

export type ws_point = struct
	i32 x, y
end

export type ws_rect=struct		!rect record occupying 16 bytes
	union
		struct
			i32 leftx,top, rightx,bottom
		end
		struct
			union i32 x,x1 end
			union i32 y,y1 end
			i32 x2,y2
		end
	end
end

export type ws_logbrush = struct
	i32 lbstyle
	i32 lbcolour
	i32 lbhatch
end

export type ws_textmetrics = struct
	i32	height
	i32	ascent
	i32	descent
	i32	i32ernalleading
	i32	externalleading
	i32	avecharwidth
	i32	maxcharwidth
	i32	weight
	i32	overhang
	i32	digitizedaspectx
	i32	digitizedaspecty
	byte	firstchar
	byte	lastchar
	byte	defaultchar
	byte	breakchar
	byte	italic
	byte	underlined
	byte	struckout
	byte	pitchandfamily
	byte	charset
end
!=========================================

export type ws_bitmapv5header = struct
	i32	size
	i32	width
	i32	height
	u16	planes
	u16	bitcount
	i32	compression
	i32	sizeimage
	i32	xpelspermeter
	i32	ypelspermeter
	i32	clrused
	i32	clrimportant
	i32	redmask
	i32	greenmask
	i32	bluemask
	i32	alphamask
	i32	cstype
	[1..9]i32 endpoints
	i32	redgamma
	i32	greengamma
	i32	bluegamma
	i32	intent
	i32	profiledata
	i32	profilesize
	i32	reserved
end

export type ws_bitmapfileheader = struct
	wt_word		typex
	wt_dword	size
	wt_word		res1, res2
	wt_dword	offbits
end

export type ws_bitmapinfoheader = struct
	wt_dword 	size
	wt_long		width
	wt_long		height
	wt_word		planes
	wt_word		bitcount
	wt_dword	compression
	wt_dword	sizeimage
	wt_long		xpelspermetre
	wt_long		ypelspermetre
	wt_dword	clrused
	wt_dword	clrimportant
end

export type ws_paintstruct = struct
!	i64		hdc
	i64		hdc
	i32		erase
	ws_rect		paintrect
	i32		restore
	i32		incupdate
	[32]byte	rgbreserved
end

!32-BIT VERSION
export type ws_openfilename32 = struct
	wt_dword		structsize
	wt_handle32		owner
	wt_handle32		instance
	wt_string32		filter
	wt_string32		customfilter
	wt_dword		maxcustfilter
	wt_dword		filterindex
	wt_string32		file
	wt_dword		maxfile
	wt_string32		filetitle
	wt_dword		maxfiletitle
	wt_string32		initialdir
	wt_string32		title
	wt_dword		flags
	wt_word			fileoffset
	wt_word			fileextension
	wt_string32		defext
	wt_lparam32		custdata
	wt_wndproc32	hook
	wt_string32		templatename
	wt_ptr32		reserved1
	wt_dword		reserved2
	wt_dword		flagsex
end

!64-BIT VERSION
export type ws_openfilename64 = struct $caligned
	wt_dword		structsize
	wt_handle64		owner
	wt_handle64		instance
	wt_string64		filter
	wt_string64		customfilter
	wt_dword		maxcustfilter
	wt_dword		filterindex
	wt_string64		file
	wt_dword		maxfile
	wt_string64		filetitle
	wt_dword		maxfiletitle
	wt_string64		initialdir
	wt_string64		title
	wt_dword		flags
	wt_word			fileoffset
	wt_word			fileextension
	wt_string64		defext
	wt_lparam64		custdata
	wt_wndproc64	hook
	wt_string64		templatename
	wt_ptr64		reserved1
	wt_dword		reserved2
	wt_dword		flagsex
end

importdll kernel32=
	func	"GetLastError"					:wt_dword
	func	"GetStdHandle"					(wt_dword)wt_handle
	func	"WriteConsoleA" as writeconsole				(wt_handle,wt_string,wt_dword,wt_ptr,wt_ptr)wt_bool
	func	"SetConsoleCursorPosition"		(wt_handle,wt_coord)wt_bool
	func	"GetConsoleScreenBufferInfo"	(wt_handle,wt_ptr)wt_bool
	func	"SetConsoleMode"				(wt_handle,wt_dword)wt_bool
	func	"WriteConsoleOutputA" as writeconsoleoutput			(wt_handle,wt_ptr,wt_coord,wt_coord,wt_ptr)wt_bool

	func	"GetConsoleScreenBufferInfoEx"	(wt_handle,wt_ptr)wt_bool
	func	"SetConsoleScreenBufferInfoEx"	(wt_handle,wt_ptr)wt_bool
	func	"GetConsoleWindow"				:wt_handle

	func	"SetConsoleTextAttribute"		(wt_handle,wt_word)wt_bool
	func	"SetConsoleTitleA" as setconsoletitle				(wt_string)wt_bool
	func	"ReadConsoleInputA" as readconsoleinput			(wt_handle,wt_ptr,wt_dword,wt_ptr)wt_bool
	func	"PeekConsoleInputA"			(wt_handle,wt_ptr,wt_dword,wt_ptr)wt_bool
	func	"FlushConsoleInputBuffer"		(wt_handle)wt_bool
	func	"SetConsoleWindowInfo"			(wt_handle,wt_bool,wt_ptr)wt_bool
	func	"SetConsoleScreenBufferSize"	(wt_handle,wt_coord)wt_bool
	func	"GetConsoleCursorInfo"			(wt_handle,wt_ptr)wt_bool
	func	"SetConsoleCursorInfo"			(wt_handle,wt_ptr)wt_bool
	func	"GetNumberOfConsoleInputEvents"(wt_handle,wt_ptr)wt_bool

	func	"FindFirstFileA" as findfirstfile		(stringz,ref i32)i32
	func	"FindNextFileA"  as findnextfile			(i32,ref i32)i32
	func	"FindClose"					(i32)i32
	func	"SetCurrentDirectoryA" as setcurrentdirectory	(stringz)i32
	func	"GetCurrentDirectoryA" as getcurrentdirectory	(i32,i32)i32
	func	"CreateDirectoryA" as createdirectory		(stringz,i32)i32
	func	"GetFileAttributesA"			(stringz)i32
	func	"GetModuleHandleA" as getmodulehandle		(wt_string)wt_handle
	func	"GetTickCount"								:wt_dword
	func	"GlobalAlloc"									(wt_uint,wt_size)wt_handle
	func	"GlobalLock"									(wt_handle)wt_ptr
	func	"GlobalUnlock"								(wt_handle)wt_bool
	func	"GlobalSize"									(wt_handle)wt_size

	func	"GetSystemTime"(ref byte)i32
	func	"Beep"							(wt_dword, wt_dword)wt_bool
	func	"SetConsoleCP"								(wt_uint)wt_bool
	func	"GetCommandLineA" : stringz
end

importdll user32=
	func	"CreateWindowExA" as createwindowex		(wt_dword, wt_string, wt_string, wt_dword, wt_int,wt_int,wt_int,wt_int,
													 wt_handle, wt_handle, wt_handle, wt_ptr)wt_handle

	func "GetMessageA" as getmessage				(wt_ptr, wt_handle, wt_uint, wt_uint)wt_bool
	func "TranslateMessage"							(wt_ptr)wt_bool
	func "DispatchMessageA" as dispatchmessage		(wt_ptr)wt_result
	func "SetTimer"									(wt_handle,wt_intptr,wt_uint,wt_ptr)wt_intptr
	func "KillTimer"								(wt_handle,wt_intptr)wt_bool
	func "SystemParametersInfoA"					(wt_uint,wt_uint,wt_ptr,wt_uint)wt_bool
	func "GetSystemMetrics"							(wt_int)wt_int
!	func "CreateMenu"								:int
	func "AppendMenuA" as appendmenu				(wt_handle,wt_uint,wt_intptr,wt_string)wt_bool
	func "GetDC"									(wt_handle)wt_handle
	func "ReleaseDC"								(wt_handle,wt_handle)wt_int

	func "SendMessageA" as sendmessage				(wt_handle,wt_uint,wt_wparam,wt_lparam)wt_result
	func "PostMessageA" as postmessage				(wt_handle,wt_uint,wt_wparam,wt_lparam)wt_bool
	func "PeekMessageA" as peekmessage				(wt_ptr,wt_handle,wt_uint,wt_uint,wt_uint)wt_bool
	func "BeginPaint"								(wt_handle,wt_ptr)wt_handle
	func "EndPaint"									(wt_handle,wt_ptr)wt_bool
	proc "PostQuitMessage"							(wt_int)
	func "LoadIconA" as loadicon					(wt_handle,wt_string)wt_handle
	func "LoadCursorA" as loadcursor				(wt_handle,wt_string)wt_handle
	func "SetCursor"								(wt_handle)wt_handle
	func "DrawMenuBar"								(wt_handle)wt_bool
	func "GetSystemMenu"							(wt_handle,wt_bool)wt_handle
	func "CreateMenu"								:wt_handle
	func "CreatePopupMenu"							:wt_handle
	func "DestroyMenu"								(wt_handle)wt_bool
	func "CheckMenuItem"							(wt_handle,wt_uint,wt_uint)wt_dword
	func "EnableMenuItem"							(wt_handle,wt_uint,wt_uint)wt_bool
	func "GetSubMenu"								(wt_handle,wt_int)wt_handle
	func "GetMenuItemID"							(wt_handle,wt_int)wt_uint
	func "GetMenuItemCount"							(wt_handle)wt_int
	func "InsertMenuA" as insertmenu				(wt_handle,wt_uint,wt_uint,wt_intptr,wt_string)wt_bool
	func "ModifyMenuA" as modifymenu				(wt_handle,wt_uint,wt_uint,wt_intptr,wt_string)wt_bool
	func "RemoveMenu"								(wt_handle,wt_uint,wt_uint)wt_bool
	func "DeleteMenu"								(wt_handle,wt_uint,wt_uint)wt_bool

	func "DestroyWindow"							(wt_handle)wt_bool
	func "InvalidateRect"							(wt_handle,wt_ptr,wt_bool)wt_bool
	func "ValidateRect"							(wt_handle,wt_ptr)wt_bool
	func "ShowWindow"								(wt_handle,wt_int)wt_bool
	func "GetClassLongA" as getclassint			(wt_handle,wt_int)wt_word
	func "SetClassLongA" as setclasslong			(wt_handle,wt_int,wt_dword)wt_word
	func "SetWindowTextA" as setwindowtext			(wt_handle,wt_string)wt_bool
	func "GetWindowTextA" as getwindowtext			(wt_handle,wt_string,wt_int)wt_int
	func "GetWindowTextLengthA" as getwindowtextlength	(wt_handle)wt_int
	func "GetKeyState"								(wt_int)wt_word

!	func "GetWindowLongPtrA" as getwindowlongptr	(wt_handle,wt_int)i64
!	func "SetWindowLongPtrA" as setwindowlongptr	(wt_handle,wt_int,wt_int)i64
	func "GetWindowLongA" as getwindowlongptr		(wt_handle,wt_int)i64
	func "SetWindowLongA" as setwindowlongptr		(wt_handle,wt_int,i64)i64

	func "GetClientRect"							(wt_handle,wt_ptr)wt_bool
	func "ClientToScreen"							(wt_handle,wt_ptr)wt_bool
	func "ScreenToClient"							(wt_handle,wt_ptr)wt_bool
	func "GetWindowRect"							(wt_handle,wt_ptr)wt_bool
	func "GetSysColor" as getsyscolour				(wt_int)wt_dword
	func "GetScrollInfo"							(wt_handle,wt_int,wt_ptr)wt_bool
	func "GetMenu"									(wt_handle)wt_handle
	func "SetMenu"									(wt_handle,wt_handle)wt_ptr
	func "TrackPopupMenu"							(wt_handle,wt_uint,wt_int,wt_int,wt_int,wt_handle,wt_ptr)wt_bool
	func "GetMenuState"								(wt_handle,wt_uint,wt_uint)wt_uint
	func "MessageBoxA" 								(wt_handle a=nil,wt_string message, wt_string caption="Caption", wt_uint b=0)wt_int
	func "OpenClipboard"							(wt_handle)wt_bool
	func "CloseClipboard"							:wt_bool
	func "EmptyClipboard"							:wt_bool
	func "GetClipboardData"							(wt_uint)wt_handle
	func "SetClipboardData"							(wt_uint,wt_handle)wt_handle
	func "MessageBeep"								(wt_uint x=0)wt_bool
	func "SetActiveWindow"							(wt_handle)wt_handle
	func "SetForegroundWindow"						(wt_handle)wt_bool
end

importdll gdi32=
	func "Rectangle"								(wt_handle,wt_int,wt_int,wt_int,wt_int)wt_bool
	func "RoundRect"								(wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int)wt_bool
	func "Ellipse"									(wt_handle,wt_int,wt_int,wt_int,wt_int)wt_bool
	func "Arc"										(wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int)wt_bool
	func "Chord"									(wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int)wt_bool
	func "Pie"										(wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int)wt_bool
	func "Polygon"									(wt_handle,wt_handle,wt_int)wt_bool
	func "TextOutA" as textout						(wt_handle,wt_int,wt_int,wt_string,wt_int)wt_bool
	func "TextOutW" 								(wt_handle,wt_int,wt_int,wt_ptr,wt_int)wt_bool
	func "GetStockObject"							(wt_int)wt_handle
	func "SelectObject"								(wt_handle,wt_handle)wt_handle
	func "CreateDCA" as createdc					(wt_string,wt_string,wt_string,wt_ptr)wt_handle
	func "MoveToEx"									(wt_handle a,wt_int b,wt_int c,wt_ptr d=nil)wt_bool
	func "CreatePen"								(wt_int,wt_int,wt_dword)wt_handle
	func "CreateSolidBrush"							(wt_dword)wt_handle
	func "CreateBrushIndirect"						(wt_ptr)wt_handle
	func "LineTo"									(wt_handle,wt_int,wt_int)wt_bool
	func "GetPixel"									(wt_handle,wt_int,wt_int)wt_dword
	func "SetPixel"									(wt_handle,wt_int,wt_int,wt_dword)wt_dword
	func "SetGraphicsMode"							(wt_handle,wt_int)wt_int
	func "CreateFontIndirectA" as createfontindirect	(wt_ptr)wt_handle
	func "CreateFontA" as createfont \
			(wt_int height, wt_int width=0, wt_int escapement=0, wt_int orientation=0, wt_int bold=0,
			 wt_dword italic=0, wt_dword underline=0, wt_dword strikeout=0, wt_dword charset=0,
			 wt_dword outprec=0, wt_dword clipprec=0, wt_dword quality=0, wt_dword pitch=0, wt_string facename)wt_handle
	func "SaveDC"									(wt_handle)wt_int
	func "GetTextMetricsA" as gettextmetrics		(wt_handle,wt_ptr)wt_bool
	func "DeleteObject"								(wt_handle)wt_bool
	func "RestoreDC"								(wt_handle,wt_int)wt_bool
	func "GetTextExtentPoint32A" as gettextextentpoint32	(wt_handle,wt_string,wt_int,wt_ptr)wt_bool
	func "GetObjectA" as getobject					(wt_handle,wt_int,wt_ptr)wt_int
	func "CreatePalette"							(wt_ptr)wt_handle
	func "GetWindowExtEx"							(wt_handle,wt_ptr)wt_bool
	func "CreateCompatibleBitmap"					(wt_handle,wt_int,wt_int)wt_handle
	func "SetBitmapBits"							(wt_handle,wt_dword,wt_ptr)wt_long
	func "SelectPalette"							(wt_handle,wt_handle,wt_bool)wt_handle
	func "RealizePalette"							(wt_handle)wt_uint
	func "SetDIBitsToDevice"						(wt_handle,wt_int,wt_int,wt_dword,wt_dword,wt_int,wt_int,wt_uint,wt_uint,wt_ptr,wt_ptr,wt_uint)wt_int
	func "StretchDIBits"							(wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_ptr,wt_ptr,wt_uint,wt_dword)wt_int
	func "SetStretchBltMode"						(wt_handle,wt_int)wt_int
	func "PatBlt"									(wt_handle,wt_int,wt_int,wt_int,wt_int,wt_dword)wt_bool
	func "BitBlt"									(wt_handle,wt_int,wt_int,wt_int,wt_int,wt_handle,wt_int,wt_int,wt_dword)wt_bool
	func "SetROP2"									(wt_handle,wt_int)wt_int
	func "CreateCompatibleDC"						(wt_handle)wt_handle
	func "DeleteDC"									(wt_handle)wt_bool
	func "CreateBitmap"								(wt_int,wt_int,wt_uint,wt_uint,wt_ptr)wt_handle
	func "CreateBitmapIndirect"						(wt_ptr)wt_handle
	func "CreateDIBitmap"							(wt_handle,wt_ptr,wt_dword,wt_ptr,wt_ptr,wt_uint)wt_handle
	func "CreateDIBSection"							(wt_handle,wt_ptr,wt_uint,wt_ptr,wt_handle,wt_dword)wt_handle
	func "StretchBlt"								(wt_handle,wt_int,wt_int, wt_int,wt_int,wt_handle, wt_int,wt_int,wt_int, wt_int,wt_dword)wt_bool
	func "PlgBlt"									(wt_handle,wt_ptr,wt_handle, wt_int,wt_int,wt_int,wt_int, wt_handle, wt_int,wt_int)wt_bool
	func "SetTextColor"  as settextcolour			(wt_handle,wt_dword)wt_dword
	func "SetTextAlign"								(wt_handle,wt_uint)wt_uint
	func "SetTextJustification"						(wt_handle,wt_int,wt_int)wt_bool
	func "SetBkColor"  as setbkcolour				(wt_handle,wt_dword)wt_dword
	func "SetBkMode"								(wt_handle,wt_int)wt_int
	func "GetBkColor"  as getbkcolour				(wt_handle)wt_dword
	func "GetBkMode"								(wt_handle)wt_int
	func "StartDocA" as startdoc					(wt_handle,wt_ptr)wt_int
	func "StartPage"								(wt_handle)wt_int
	func "EndPage"									(wt_handle)wt_int
	func "EndDoc"									(wt_handle)wt_int
	func "AbortDoc"									(wt_handle)wt_int
	func "GetViewportOrgEx"							(wt_handle,wt_ptr)wt_bool
	func "GetDIBits"								(wt_handle,wt_handle,wt_uint,wt_uint,wt_ptr,wt_ptr,wt_uint)wt_int
	func "GetDIBColorTable" as getdibcolourtable	(wt_handle,wt_uint,wt_uint,wt_ptr)wt_uint
	func "SetDIBColorTable" as setdibcolourtable	(wt_handle,wt_uint,wt_uint,wt_ptr)wt_uint
	func "GetTextAlign"								(wt_handle)wt_uint
end

importdll comdlg32=
	func "GetOpenFileNameA"							(wt_ptr)wt_bool
	func "GetSaveFileNameA"							(wt_ptr)wt_bool
end
=== gxlib.q 0 1 40/48 ===
!MODULE winmessages
module sysp

module winconsts
module winapi
module wingxlib
module gxmisc

VAR GGDI

export var debug=0

export var messhandlertable=9000	!message by windowclass table of message handlers
export var actionhandlertable

export var chx,chy		!default text sizes for menus
export var cha,chd		!ascender/descender heights
export const smx=3		!margins around button text in pixels (both sides)
export const smy=4
export var arrowdim
export var markdim
export var buttonheight
export var listrowheight
export const labelfont=1

export var tabstops=(8,)*20

export var wmouse=nil
export var wfocus=nil
export var wprinter=nil
export var lastmousepos=nil
export var lastmousewindow=nil
export var currmousewindow=nil

export var mousepos
export var mousesw
export var quitmess=0
export var dragmode=0
export var lastbuttontime=0

export var buttonstate=0
export var wmessagetable		!see initdata
export var buttontable		!see initdata

const maxqueuesize=100
export var messagequeue=()
export var nmessages=()

const dragtol=1

export var copymode=4

var vktomesstable

!Describe gx window (also bitmap, control or child window)
export record rwindow =

	var windclass					! type of window (popup, control, etc)
	var flags						! general purpose flags
	var style						! stylerec entry
	var name						! Optional debugging name

	var owner						! owner when this is a child window
	var index						! index 1..n when part of a list (eg. .childlist of owner)
	var childlist					! list of child windows

!framepos/dim describe the overall window size including Windows or gx-drawn borders,
	var frameposx, frameposy		! top left of frame, in screen or owner window client coords
	var framedimx, framedimy		! pixels dims including frame and caption

	var	posx, posy					! Pixel pos client area as seen by application
	var	dimx, dimy					! Pixel dims of client area as seen by application

	var gdi							! (rgdistate)	gdi state record

	var enable						! 1 to enable toggle/button/arrow etc, 0 to disable and show greyed out
	var id							! button/et al: command code associated with control
	var text						! caption or label or primary data
!		var data 	@text
	var linkvar						! pointer to linked var for toggle/select/scroll
	var gindex						! window export index

	var attrs						! general purpose attributes, depends on window class

!bitmap data
	var pixelbits					! 1, 4, 16, 24, 32 bits per pixel
	var pixelptr					! pointer to image data
	var pixelbytes					!bytes/pixel (round up to next whole byte)
	var linebytes					!bytes/per row, also pitch
	var framebytes
	var paltype						!0, or palette type

	var closed
end

export enumdata paltypenames =
	(no_pal=0,		$),
	(greyscale_pal,	$),
	(tinted_pal,	$),
	(colour_pal,	$),
	(uv_pal,		$),
end

export type rgdistate = struct
	ref void hwnd				! win32 handle (hwnd)
	ref void hdc				! 0 or device context handle for hwnd
	ref void hwnd2				! secondary window/memory backup
	ref void hdc2				! 0 or hwnd3 DC screen or memory hdc
	i64 originalwndproc	! win32 control handling proc
	union
		i64 menuhandle		! win32 handle to any menubar
		i64 oldbmobj			!used for bitmaps
	end
	i32 drawmode			! see dm- drawcodes
	i32 updated				! 1 when pixels have changed

	i32 posx,posy			!current drawing position
	i32 pencolour			! current line colour (rgb)
	i32 penwidth			! current line width

	i32 penstyle			! current line dotted style

	i32 xormode				! 0=normal, 1=xor
	i32 brushcolour			! current brush colour
	i32 brushstyle			! current brush style (bs_solid/etc)
	i32 brushpattern		! current brush hatch/bitmap pattern

	i32 font				! current font number
end

export enumdata marktypenames =
	(no_mark=0,			$),
	(radio_mark,		$),
	(tick_mark,			$),
	(check_mark,		$),
	(invert_mark,		$),
	(outline_mark,		$),
	(bold_mark,			$),
end

export enumdata hilitetypenames =
	(no_hilite=0,		$),
	(invert_hilite,		$),
	(outline_hilite,	$),
end

export record togglerec=		!for toggle and select
	var textoffset
	var onvalue
end

export record scrollbarrec=		!scroll bars
	var limits						!range
	var span						!portion of limits represented by visible data (0 means not relevant)
	var thumbsize					!pixel length of thumb (vert or hoz extent along scrollbar)
	var thumbspan					!pixels that the thumb can move
	var thumbpos					!current thumb position in pixels from start of scrollbar
	var currpos						!current position, will be in limits range
	var dragmode					!1 if thum currently being dragged
end

export record editboxrec=		!edit boxes
!current char position within edit text
!if text has N characters, then position will be 1..N+1
!The position is just /before/ the stated character
!This matches column position within the MED text editor
	var currpos						!cursor position, 1 to N+1 (N=chars in edit text)
	var caretpos					!current pixel position of any caret
	var textpos						!start x,y pixel position of text, set by gxjust_text
end

export record listboxrec=
	var rows						!number of displayed rows
	var pagepos						!data position corresponding to row 1 of display
	var length						!all data items, same as linkvar^.len
	var currpos						!cursor position within the data, 1 to N (can be 0 when N=0)
	var pitch, offset				!pixel dims of each row
end

export record rmessage=
	var		wind		!main window/button associated with message
	var		menuwind	!top-level window owning button
	var		message		!message number
	var		state		!button/shift key state at time of message
	var		a,b			!general purpose data, depends on message
	var		x,y			!current mouse position
end

!contains many different flags and style codes for assorted controls
!But at one mostly byte each, is very low overhead (32 values is same as two vars)

export type stylerec = struct
	byte	border				!bs_ code
	byte	justify				!'L', 'R', 'C'
	byte	vjustify			!'T', 'B', 'M'
	byte	windbgnd			!colour index for window background
	byte	textfgnd			!colour index
	byte	textbgnd			!
	byte	bgndmode			!
	byte	textfont			!font index
	byte	textsize			!pixel size
	byte	textbold			!1 if bold
	byte	textitalic			!1 if italic
	byte	ispassword			!1 when edit field is a password
	byte	fieldwidth			!edit field maximum char width
	byte	dir					!'L','R','U','D'
	byte	marktype			!xxx_mark style, or:
	byte	hilitetype			!xxx_hilite style
	byte	iframe				!1: pos/dim include frame
	byte	imark				!1: pos/dim include mark for toggle/select
	byte	hscroll				!1: include windows-drawn hoz scroll bar
	byte	vscroll				!1: include windows-drawn vert scroll bar
	byte	lbchange			!1: return mm_change on list boxes when row has changed
	byte	returnmess			!1: return id code when clicking toggle/select/editbox
	byte	noupdate			!1: don't change or allow editing on toggle/select/editbox
end

export record rpoint = var x,y end
export record rrect  =
	var pos,dim
end

export record rframe =
	var x1,y1,x2,y2
end

export record getrec=
	fun getbounds(&self)= 0
	fun getitem(&self,n)= 0
	fun getstritem(&self,n)= ""
end

export enumdata stylenames =	! (default)
	(ss_border,			$),		! Border style (wbs_simple)
	(ss_justify,		$),		! 'L' 'C' 'R'	Horizontal text justify ('L')
	(ss_vjustify,		$),		! 'T' 'M' 'B'	Vertical text justify ('M' for buttons)
	(ss_textfgnd,		$),		! Text colour index (black)
	(ss_textbgnd,		$),		! Text background colour index (if opaque mode) (0)
	(ss_bgndmode,		$),		! 0
	(ss_textfont,		$),		! Text font number (1)
	(ss_textsize,		$),		! (0)
	(ss_textbold,		$),		! (0)
	(ss_textitalic,		$),		! (0)
	(ss_ispassword,		$),		! (0)
	(ss_marktype,		$),		! Toggle/select mark style (radio_mark)
	(ss_hilitetype,		$),		! Toggle/select hilite style (no_hilite)
	(ss_iframe,			$),		! Whether pos and dim include frame width (also caption bar/menu for windows)
	(ss_windbgnd,		$),		! Background colour of window or button (ltgrey)
	(ss_imark,			$),		! Background colour of window or button (ltgrey)
	(ss_hscroll,		$),		! (0)
	(ss_vscroll,		$),		! (0)
	(ss_lbchange,		$),		! (0)
	(ss_returnmess,		$),		! (0) Toggle/select/editbox, return id when clicked
	(ss_noupdate,		$),		! (0) Toggle/select/editbox, don't change or allow edit
end

export enumdata drawmodenames =
								!HDC	HDC2	Restore
	(dm_screen=0,		$),		!screen	--		Custom routine	Draw directly to screen; no mem backup
	(dm_memory,			$),		!memory	--		NA				Draw to memory only; no screen hdc (eg. bitmap)
	(dm_screenmemory,	$),		!screen	memory	Blit mem->scr	Draw to both screen and memory at same time
	(dm_memoryscreen,	$),		!memory	screen	Blit mem->scr	Draw to memory; update screen periodically
end

!var windowlist=nil

export enumdata wfnames =
	(wa_rightclick=0,	$),		!allow right click
	(wa_middleclick,	$),		!allow middle click
	(wa_leftdbl,		$),		!allow left double click
	(wa_rightdbl,		$),		!allow right double click
	(wa_middledbl,		$),		!allow middle double click
	(wa_leftdrag,		$),		!allow left drag
	(wa_rightdrag,		$),		!etc
	(wa_middledrag,		$),
	(wa_autoupdate,		$),		!auto update screen for toggles/etc
	(wa_tab,			$), 	!allow tab to switch to next button which has watab
	(wa_strvar,			$), 	!1 for listbox linkvar to use string not index
	(wa_retmess,		$), 	!1 for button to return .value as mess not qmcommand
	(wa_retsel,			$), 	!1 for button to return .value as mess not qmcommand
	(wa_memory,			$), 	!1 when hdc/hdcmem have been switched, hdc points to memory dev
	(wa_maximised,		$),		!1 when maximised, 0 when normal/minimised
	(wa_param1,			$), 	!general purpose control-specific flags
	(wa_param2,			$), 
	(wa_useenter,		$),
	(wa_closed,			$),		!whether window has been closed

	(wa_$last,			$)
end

const wa_needdbl	= wa_param1	!1 requires double-click on listbox to return wmcommand
const wa_editdd		= wa_param2	!1 means editable dropdown box

!GX border styles, used for child windows.
!some child windows
export enumdata bsnames, bscat, bswidths=
	(bs_none=0,		$,	0,	ws_rect(0,0,0,0)),			!no border
!	(bs_windows,	$,	'W',	ws_rect(0,0,0,0)),			!windows-drawn, but no own-drawn border
	(bs_simplew,	$,	'W',	ws_rect(1,1,1,1)),			!single 1-pixel black line, windows drawn
	(bs_simple,		$,	'X',	ws_rect(1,1,1,1)),			!single 1-pixel black line
	(bs_thick,		$,	'X',	ws_rect(2,2,2,2)),			!2-pixel border
	(bs_panel,		$,	'X',	ws_rect(1,1,1,1)),			!raised panel, 1-pixel
	(bs_inset,		$,	'X',	ws_rect(1,1,1,1)),			!inset panel, 1-pixel
	(bs_ownsimple,	$,	'I',	ws_rect(0,0,0,0)),			!included inset panel, 1-pixel (drawn as part of client area)
	(bs_ownpanel,	$,	'I',	ws_rect(0,0,0,0)),			!included inset panel, 1-pixel (drawn as part of client area)
	(bs_owninset,	$,	'I',	ws_rect(0,0,0,0)),			!included inset panel, 1-pixel
	(bs_testext,	$,	'X',	ws_rect(10,10,10,10)),
	(bs_testint,	$,	'I',	ws_rect(8,8,8,8)),
	(bs_dummy,		$,	0,	ws_rect(0,0,0,0))
end

export enumdata windowclassnames, defaultborderstyles =
	(no_class=0,		$,	bs_none),			!Unassigned
	(window_class,		$,	wbs_resize),		!Main window
	(memwindow_class,	$,	wbs_none),			!memory backup to any window
	(popup_class,		$,	wbs_thick),			!Pop-up window (forms a stack)
	(float_class,		$,	bs_thick),			!Independent window
	(bitmap_class,		$,	bs_none),			!(image handling)
	(screen_class,		$,	bs_none),			!Describes the desktop screen (not owned by my app)
	(printer_class,		$,	bs_none),			!Used for printing

	(group_class,		$,	bs_inset),			!Used mainly for grouping other buttons (eg. for Smdefblock)
	(panel_class,		$,	bs_inset),			!General purpose panel for drawing in etc
!	(button_class,		$,	bs_panel),			!Click button
	(button_class,		$,	bs_simplew),		!Click button
	(toggle_class,		$,	bs_none),			!Toggle button (can be composite, eg mark and label)
	(select_class,		$,	bs_none),			!Select from several choices
	(editbox_class,		$,	bs_simplew),		!Single-line edit control
	(scrollbar_class,	$,	bs_simplew),		!Hoz or vert scroll bar (Some windows can also have Windows-drawn scroll bars)
	(listbox_class,		$,	bs_simplew),		!List of options (scrollable usually)
	(dropdown_class,	$,	bs_none),			!Button revealing attached listbox when clicked
	(framebar_class,	$,	bs_panel),			!Left or right full-height panel used for toolboxes etc
	(statusbar_class,	$,	bs_panel),			!Top or bottom full-width panel used for scrollbars
	(tooltip_class,		$,	bs_simplew),		!Tooltops displayed when hovering over enabled buttons
	(arrow_class,		$,	bs_ownpanel),		!Click button normally displaying an error in one of 4 orientations
	(mark_class,		$,	bs_none),			!Toggle or select mark
	(label_class,		$,	bs_none),			!Contains unclickable text usually
	(dummy_class,		$,	bs_none)
end

export enumdata actionnames=
	(draw_w,		$),
	(update_w,		$),
	(last_w,		$),
end

!MM Message Numbers

export enumdata messagenames=

!all messages have x,y coord relative to top most window or child window
!
	(mm_null=0,			$),		! empty message

!window messages
	(mm_activate,		$),		! (w,a) a=1/0 activate/deactivate window
	(mm_close,			$),		! (w) close window (X button clicked)
	(mm_sizewindow,		$),		! (w...) resize window
	(mm_movewindow,		$),		! (w...) move window
	(mm_restore,		$),		! (w...) repaint window

!cursor/focus messages
	(mm_setcursor,		$),		! (w...) update cursor type
	(mm_setfocus,		$),		! (w...) set focus to w
	(mm_killfocus,		$),		! (w...) lose focus from w

!basic mouse messages (click messages can be promoted to other messages eg mm_command)
	(mm_move,			$),		! (w,x,y,b) mouse move, btns up/down (also drag messages when down)
	(mm_click,			$),		! (w,x,y) left btn click, can be promoted to mm_command etc depending on context
	(mm_dblclick,		$),		! (w,x,y) left btn dbl click, usu promoted
	(mm_clickup,		$),		! (w,x,y) left btn released
	(mm_rclick,			$),		! (w,x,y) right click in window, these usu. promoted
	(mm_rdblclick,		$),		! (w,x,y) right double click
	(mm_rclickup,		$),		! (w,x,y) right button released
	(mm_mclick,			$),		! (w,x,y) middle button versions of above
	(mm_mdblclick,		$),		! (w,x,y)
	(mm_mclickup,		$),		! (w,x,y)
	(mm_hover,			$),		! (w,x,y) paused over button

	(mm_onwindow,		$),		! (w,x,y) newly over a window
	(mm_offwindow,		$),		! (w,x,y) just came off window
	(mm_draw,			$),		! (w,x,y) redraw window
	(mm_update,			$),		! (w,x,y) update window (change of pos etc)

!drag messages
	(mm_startdrag,		$),		! (w) start mouse movement with some btns down
	(mm_rstartdrag,		$),		! (w)
	(mm_mstartdrag,		$),		! (w)
	(mm_drag,			$),		! (w,x,y) moving mouse with buttons down (also qmmove sent)
	(mm_enddrag,		$),		! (w,x,y) all buttons up after drag

!left command
	(mm_command,		$),		! (w,id) button clicked, id and sub-event given
	(mm_dblcommand,		$),		! (w,id) button double clicked

!right command
	(mm_rcommand,		$),		! (w,id) right click button
	(mm_rdblcommand,	$),		! (w,id) right double click button

!middle command
	(mm_mcommand,		$),		! (w,id)
	(mm_mdblcommand,	$),		! (w,id)

!general key messages
	(mm_char,			$),		! (w,ch)
	(mm_key,			$),		! (w,k,shift)
	(mm_keyup,			$),		! (w,k,shift)

!scroll/select/wheel messages
	(mm_sethozpos,		$),		! (w,pos)		New logical position set by hoz scrollbar
	(mm_setvertpos,		$),		! (w,pos)		from vertical scrollbar
	(mm_select,			$),		! (w,n)			Set nth item as current/highlighted/selected item
	(mm_pick,			$),		! (w,n)			Pick and return item n
	(mm_wheel,			$),		! (w,delta)		Move log pos etc but depends on context
	(mm_lbchange,		$),		! (w,n)			A listbox position has changed

!misc messages
	(mm_timer,			$),		! (w)

!high level window messages
	(mm_cancel,			$),		! (w)
	(mm_ok,				$),		! (w)
	(mm_help,			$),		! (w,id)
	(mm_cmdline,		$),		! (w,s)

!specific key messages
	(mm_leftkey ,		$),		! (w,shift)
	(mm_rightkey,		$),		! (w,shift)
	(mm_upkey,			$),		! (w,shift)
	(mm_downkey,		$),		! (w,shift)
	(mm_pageupkey,		$),		! (w,shift)
	(mm_pagedownkey,	$),		! (w,shift)
	(mm_homekey,		$),		! (w,shift)
	(mm_endkey,			$),		! (w,shift)
	(mm_tabkey,			$),		! (w,shift)
	(mm_bskey,			$),		! (w,shift)
	(mm_deletekey,		$),		! (w,shift)
	(mm_enterkey,		$),		! (w,shift)
	(mm_insertkey,		$),		! (w,shift)
	(mm_functionkey,	$),		! (w,shift)

!Other messages for controls, mainly for attached arrow buttons
	(mm_up,				$),		! (w,id) Arrow up/etc
	(mm_down,			$),		! (w,id)
	(mm_right,			$),		! (w,id)
	(mm_left,			$),		! (w,id)
	(mm_edit,			$),		! (w,id)	Update of edit box
	(mm_edited,			$),		! (w,id)	Finished edit box entry (tab etc)
	(mm_last,			$)
end

!export const mm_scroll	= mm_hscroll	!general scroll independent of orientation

!!other messages wmuser+ are user assigned, usually applied as
!ids to controls. Depending on the options to Waitmess(), a wm_commmand message
!is coverted to a direct message number. So (wm_command, 230) is covered to
!message 230. So message ids have to start from 200 so that they occupy a different
!number space from normal message codes.
export const mm_user	= 200

export const kb_lbutton	= 0x1	!used in buttonstate
export const kb_rbutton	= 0x2
export const kb_mbutton	= 0x4

export const kb_shift	= 0x8	!used in shiftstate
export const kb_ctrl	= 0x10
export const kb_alt		= 0x20
export const kb_capslock	= 0x40
export const kb_dblclick	= 0x80	!used for some messages that don't have dblclick versions,

export const kb_rshift	= 0x100
export const kb_rctrl	= 0x200
export const kb_ralt	= 0x400

export enumdata colournames, colourvalues =
!					   BB'GG'RR
	(black,		$,	0x_00'00'00),
	(red,		$,	0x_00'00'C0),
	(dkred,		$,	0x_00'00'90),
	(red3,		$,	0x_00'00'70),
	(green,		$,	0x_00'C0'00),
	(dkgreen,	$,	0x_00'90'00),
	(green3,	$,	0x_00'70'00),
	
	(blue,		$,	0x_C0'00'00),
	(dkblue,	$,	0x_90'00'00),
	(blue3,		$,	0x_70'00'00),

	(cyan,		$,	0x_c0'c0'00),
	(dkcyan,	$,	0x_90'90'00),
	(cyan3,		$,	0x_70'70'00),

	(magenta,	$,	0x_c0'00'c0),
	(dkmagenta,	$,	0x_90'00'90),
	(magenta3,	$,	0x_70'00'70),

	(yellow,	$,	0x_00'C0'C0),
	(dkyellow,	$,	0x_00'90'90),
	(yellow3,	$,	0x_00'70'70),
	(yellow4,	$,	0x_00'50'50),

	(white,		$,	0x_FF'FF'FF),
	(ltgrey,	$,	0x_C0'C0'C0),
	(grey,		$,	0x_90'90'90),
	(dkgrey,	$,	0x_70'70'70),

	(ltorange,	$,	0x_00'A0'FF),
	(orange,	$,	0x_00'60'FF),
	(flesh,		$,	0x_70'85'EE),
	(pink,		$,	0x_9A'32'DB),
	(dkpink,	$,	0x_72'24'A9),
	(brown,		$,	0x_46'43'7D),
	(blue4,		$,	0x_B7'1C'5E),
	(blue5,		$,	0x_6F'3D'0D),
	(olive,		$,	0x_05'A0'88),
	(ltbrown,	$,	0x_00'70'B0),

	(blue6,		$,	0x_9C'63'1C),
	(green4,	$,	0x_12'51'11),
	(purple,	$,	0x_5E'0D'73),
	(blue7,		$,	0x_E6'27'1C),
	(crimson,	$,	0x_15'2A'D3),
	(violet,	$,	0x_54'16'A0),
	(blue8,		$,	0x_86'68'1E),
	(dkorange,	$,	0x_25'6A'D4),
	(green5,	$,	0x_09'46'41),
	(blue9,		$,	0x_65'0A'1D),

	(ltred,		$,	0x_00'00'FF),
	(ltgreen,	$,	0x_00'FF'00),
	(ltblue,	$,	0x_FF'00'00),
	(ltcyan,	$,	0x_FF'FF'00),
	(ltmagenta,	$,	0x_FF'00'FF),
	(ltyellow,	$,	0x_00'FF'FF),

!The following are the Windows system colours, set up as indices
!Init needs need to retrieve the values and set up the rgb values in this table
	(button_col,	$,	0),		!button colour
	(window_col,	$,	0),		!window colour
	(text_col,		$,	0),		!text in windows
end

export const skipmess = 1		!message has been processed; caller must wait for another message
export const thismess = 0		!caller should deal with this message (it has not been processed, or has been but caller can process it too)

export var bmbgnd
export var defstyle			!set initdata
export var currmess=nil

export var wapplic=nil
export var wscreen=nil

var data,ndata
var tabstack,ntab
var breakflag

const k_menu=30000
const kdivide=30001
const kcolumn=30002
const kfilehistory=30003

var caretdrawn=0
var dkcolour=0x000000
var ltcolour=0xFFFFFF
var thumbdragmode=0
var thumbstartpos=0

var dirtomess=['L':mm_left,'R':mm_right,'U':mm_up,'D':mm_down]

proc start=
!CPL "GXLIB START"

	initdata()
	mxinit()
	initmenuhandlers()
end

export proc setupgdi(w,hwnd)=			!SETUPGDI
!NOTE: rare care of a func within a data header. This is to allow bitmap module
!to be higher up the hierarchy than gx, but still let it use some gx functions.

!set up the default gdi descriptor for window w

	if w.gdi then
		return			!assume already done
	fi

	gdi:=new(rgdistate)
	gdi.hwnd:=hwnd

!start with default drawmodes of dm_screen, or dm_memory for bitmaps
!These can be converted later using gxdrawmode

	if w.type=rwindow then
		gdi.hdc:=getdc(hwnd)
		gdi.drawmode:=dm_screen
	else						!assume bitmap
		gdi.hdc:=createcompatibledc(nil)
		gdi.drawmode:=dm_memory
	fi

	gdi.posx:=gdi.posy:=0
	gdi.updated:=0
	gdi.font:=0
	gdi.pencolour:=getsyscolour(colour_windowtext)
	gdi.penwidth:=0
	gdi.penstyle:=ps_solid
	gdi.xormode:=0
	gdi.brushcolour:=0xff'ff'ff
	gdi.brushstyle:=bs_solid
	gdi.brushpattern:=0
	w.gdi:=gdi
	end

export const arleft = "<"
export const arright = ">"
export const arup = "^"
export const ardown = "V"

export var allwindows::=()			!list of all windows and controls

export func ctrlpressed = return (currmess.state iand kb_ctrl) end
export func shiftpressed = return (currmess.state iand kb_shift) end

proc initdata=
	messagequeue:=new(list,100)
	nmessages:=0

	colourvalues::=colourvalues

	colourvalues[button_col]:=getsyscolour(colour_btnface)

	colourvalues[window_col]:=getsyscolour(colour_window)
	colourvalues[text_col]:=getsyscolour(colour_windowtext)

	defstyle:=new(stylerec)
	defstyle.border		:= bs_simplew
	defstyle.justify	:= 'L'
	defstyle.vjustify	:= 'M'
	defstyle.textfgnd	:= black
	defstyle.marktype	:= check_mark
	defstyle.hilitetype	:= no_hilite
	defstyle.windbgnd	:= button_col
	defstyle.imark		:= 1
!defstyle.windbgnd	:= window_col

	init_handlertables()
	d:=gxchardim(labelfont)
	chx:=d.x
	chy:=d.y

	d:=gxchardim(0,1)
	cha:=d.x
	chd:=d.y
	arrowdim:=chy+2
	markdim:=arrowdim-2

	buttonheight:=chy+smy*2
	listrowheight:=chy+smy*2
end

export func gxcreatewindow(?caption,?pos,?dim,?options,owner=nil)=		!CREATEWINDOW
	#create a popup window which is not a child window.
	#(nevertheless, it can have an owner window, such as the main window of the
	#application)
	#returns an rwindow handle
	#caption	optional caption txt
	#pos		(x,y) is pixel pos of top left corner in screen coordinates (of frame?)
	#		"cent" to place centrally
	#		omitted: use default placement
	#dim		(width,height) overall pixel size
	#		"max" maximised
	#		"desktop" fill desktop screen
	#		omitted: use (640,480)
	#owner	optional owner window (default nil)
	#options	option dict, default is [wf_caption:1, wf_border:wbs_resize]

	hwnd:=wx_createpopup(caption,pos,dim,options,(owner|owner.gdi.hwnd|nil))

	w:=newwindow(hwnd,0,no_class,bs_windows)

	if wapplic=nil then
		wapplic:=w
	fi

	W.STYLE:=NEW(STYLEREC)
	W.STYLE.BORDER:=0
	W.WINDCLASS:=WINDOW_CLASS
	W.STYLE.WINDBGND:=WINDOW_COL
	W.ENABLE:=1
	W.FLAGS.[WA_LEFTDRAG]:=1
	W.FLAGS.[WA_LEFTDBL]:=1

	setwindowdims_w(w,hwnd)

	setupgdi(w,hwnd)
	gxfont(w,1)

	GXDRAWMODE(W,DM_SCREENMEMORY)
	GXCLEAR(W)

	return w
end

proc setwindowdims_w(w,hwnd)=			!SETWINDOWDIMS
!use windows functions to set up client and frame pos and dims of top-level window

	box:=new(ws_rect)
	getwindowrect(hwnd,&box)
	w.frameposx:=box.x
	w.frameposy:=box.y
	w.framedimx:=box.x2-box.x
	w.framedimy:=box.y2-box.y

	getclientrect(hwnd,&box)

	w.dimx:=box.x2-box.x
	w.dimy:=box.y2-box.y

	pt:=ws_point(0,0)
	clienttoscreen(hwnd,&pt)		!pos starts at 0,0
	w.posx:=pt.x
	w.posy:=pt.y
end

proc setwindowdims_c(w,hwnd)=			!SETWINDOWDIMS
!use windows functions to set up client and frame pos and dims of child window


	box:=new(ws_rect)
	getwindowrect(hwnd,&box)			!client dims also Windows frame dims as has no Windows border
	w.posx:=box.x-w.owner.posx
	w.posy:=box.y-w.owner.posy
	w.dimx:=box.x2-box.x
	w.dimy:=box.y2-box.y

	widths:=bswidths[w.style.border]
	if bscat[w.style.border]='I' then widths:=ws_rect(0,0,0,0) fi

	w.frameposx:=w.posx-widths.x1
	w.frameposy:=w.posy-widths.y1
	w.framedimx:=w.dimx+widths.x1+widths.x2
	w.framedimy:=w.dimy+widths.y1+widths.y2
end

export proc gxclear(w,?colour)=			!GXCLEAR
	#fill window w with <colour>, or with current background if omitted

!RETURN
	gdi:=w.gdi
	gdi.updated:=1

	gxcolour(w,getrgb(black))
	gxstyle(w,0)

	if colour.isvoid then
		colour:=getrgb(w.style.windbgnd)
	fi

	oldpenstyle:=gdi.penstyle
	oldbrushstyle:=gdi.brushstyle

	gxbrushstyle(w,bs_solid)
	gxstyle(w,ps_null)

	gxfillrect(w,0,0,w.dimx,w.dimy,colour)
	gxbrushstyle(w,oldbrushstyle)
	gxstyle(w,oldpenstyle)
end

export func gxstyle(w,?style)=			!GXSTYLE
	#style omitted: get pen current pen style
	#style supplied: set pen style for subsequent line drawing
	#Style is a char code or int refering to a small variety of Windows dotted styles:
	#	0 S |		Solid
	#	Space		Null (pen up?)
	#	-			Dotted
	#	:			Dashdotdot
	#	!			Dashdotd
	#	F			Inside frame

	gdi:=w.gdi

	if style.isdef and gdi.penstyle<>style then
		case style
		when '!' then style:=ps_dashdot
		when ':' then style:=ps_dashdotdot
		when '-' then style:=ps_dot
		when ' ' then style:=ps_null
		when 'D' then style:=ps_alternate
		when '|','S',0 then style:=ps_solid
		when 'F' then style:=ps_insideframe
		esac

		gdi.penstyle:=style
		if style>=10 then style:=ps_dot fi
		deleteobject(selectobject(gdi.hdc,createpen(style,gdi.penwidth,gdi.pencolour)))
		if gdi.drawmode=dm_screenmemory then
			deleteobject(selectobject(gdi.hdc2,createpen(style,gdi.penwidth,gdi.pencolour)))
		fi
	fi
	return gdi.penstyle
end

export proc gxbrushstyle(w,?style,?pattern)=		!GXBRUSHSTYLE
	#Set Windows brush style and pattern
	#Style supplied:	set style
	#pattern supplied:	set style
	#style is:		S, H, Space, B for Solid, Hatched, Null, DIB
	#pattern is:	- | \ / + x/X for Hoz, Vert, Diag, Fwd Diag, Cross, Diag Cross

	gdi:=w.gdi
	brush:=new(ws_logbrush)

	if style.isdef then
		if style<>gdi.brushstyle then
			case style
			when 'S' then style:=bs_solid
			when 'H' then style:=bs_hatched
			when ' ' then style:=bs_null
			when 'B' then style:=bs_dibpattern
			esac

			gdi.brushstyle:=style
		fi
		gdi.brushpattern:=0		!default to no pattern, will be changed by pattern if supplied
	fi

	if pattern.isdef and pattern<>gdi.brushpattern then
		case pattern
		when '-' then pattern:=hs_horizontal
		when '|' then pattern:=hs_vertical
		when '\\' then pattern:=hs_fdiagonal
		when '/' then pattern:=hs_bdiagonal
		when '+' then pattern:=hs_cross
		when 'x','X' then pattern:=hs_diagcross
		esac
		gdi.brushpattern:=pattern
	fi

	brush.lbstyle:=gdi.brushstyle
	brush.lbcolour:=gdi.brushcolour
	brush.lbhatch:=gdi.brushpattern

	deleteobject(selectobject(gdi.hdc,createbrushindirect(&brush)))
	if gdi.drawmode=dm_screenmemory then
		deleteobject(x:=selectobject(gdi.hdc2,createbrushindirect(&brush)))
	fi
end

export func gxbrushcolour(w,?colour)=			!GXBRUSHCOLOUR
	#colour supplied:	set current fill colour
	#colour omitted:	return current fill colour

	gdi:=w.gdi

	if colour.isdef and colour<>gdi.brushcolour then
		gdi.brushcolour:=colour
		brush:=new(ws_logbrush)
		brush.lbstyle:=gdi.brushstyle
		brush.lbcolour:=colour
		brush.lbhatch:=gdi.brushpattern

		deleteobject(selectobject(gdi.hdc,createbrushindirect(&brush)))
		if gdi.drawmode=dm_screenmemory then
			deleteobject(selectobject(gdi.hdc2,createbrushindirect(&brush)))
		fi
	fi
	return gdi.brushcolour
end

export proc gxfillrect(w,x,y,width,height,?colour,mode=0)=		!GXFILLRECT
	#Draw filled rectangle with optional outline
	#x,y are top-left coordinates
	#width, height are overall pixel dimensions, inclusive; they include any outline
	#(When the outline is drawn, the filled region is 1 pixel smaller all round)
	#colour is the colour of the filled region (current brush colour when omitted)
	#mode=1 to draw the outline, or mode=0 (default) to omit it
	#The outline is drawn in the current pen colour

	gdi:=w.gdi
	gdi.updated:=1

	oldbrushcolour:=gdi.brushcolour
	if colour.isdef then
		gxbrushcolour(w,colour)
	fi

	oldpenstyle:=gdi.penstyle
	if mode=0 then		!inside only
		gxstyle(w,ps_null)
	fi

	if height<0 then y:=y+height+1; height:=-height fi
	if width<0 then x:=x+width+1; width:=-width fi

	if mode=0 then		!inside only, needs extra pixel width
		rectangle(gdi.hdc,x, y,x+width+1,y+height+1)
		if gdi.drawmode=dm_screenmemory then
			rectangle(gdi.hdc2,x,y,x+width+1,y+height+1)
		fi
	else			!inside and outside
		rectangle(gdi.hdc,x, y, x+width, y+height)
		if gdi.drawmode=dm_screenmemory then
			rectangle(gdi.hdc2,x,y,x+width,y+height)
		fi
	fi
	gxstyle(w,oldpenstyle)
	gxbrushcolour(w,oldbrushcolour)
end

export func gxcolour(w,?colour)=		!GXCOLOUR
	# colour supplied:	set current outline colour for subsequent line drawing
	# colour omitted:	return current outline colour

	gdi:=w.gdi

	if colour.isdef and gdi.pencolour<>colour then
		gdi.pencolour:=colour
		gdi.xormode:=0
		deleteobject(selectobject(gdi.hdc,createpen(gdi.penstyle,gdi.penwidth,gdi.pencolour)))
		setrop2(gdi.hdc,r2_copypen)
		if gdi.drawmode=dm_screenmemory then
			deleteobject(selectobject(gdi.hdc2,createpen(gdi.penstyle,gdi.penwidth,gdi.pencolour)))
			setrop2(gdi.hdc2,r2_copypen)
		fi

	fi

	return gdi.pencolour
end

export proc gxsetpen(w,pen)=
	gxcolour(w,getrgb(pen))
end

export proc gxline(w,x,y,?x2,?y2)=		!GXLINE
	#gxline(w,x,y)			Draw line from current position to x,y
	#gxline(w,x,y,x2,y2)	Draw line from x,y to x2,y2

	gdi:=w.gdi

	if x2.isvoid then		!assume 2 params
		x2:=x
		y2:=y

		movetoex(gdi.hdc,gdi.posx, gdi.posy)
		if gdi.drawmode=dm_screenmemory then
			movetoex(gdi.hdc2,gdi.posx, gdi.posy)
		fi
	else
		movetoex(gdi.hdc,x, y)
		if gdi.drawmode=dm_screenmemory then
			movetoex(gdi.hdc2,x, y)
		fi
		gdi.posx:=x
		gdi.posy:=y
	fi

	lineto(gdi.hdc,x2,y2)

	if gdi.drawmode=dm_screenmemory then
		lineto(gdi.hdc2,x2,y2)
	fi
	gdi.posx:=x2
	gdi.posy:=y2
end

export func gxwidth(w,width)=
!get/set pen width for subsequent line drawing
	gdi:=w.gdi
	if width.isvoid then
		return gdi.penwidth
	fi

	if gdi.penwidth<>width then
		gdi.penwidth:=width
		deleteobject(selectobject(gdi.hdc,createpen(gdi.penstyle,gdi.penwidth,gdi.pencolour)))
		if gdi.drawmode=dm_screenmemory then
			deleteobject(selectobject(gdi.hdc2,createpen(gdi.penstyle,gdi.penwidth,gdi.pencolour)))
		fi

	fi
	return width
end

export proc gxlinerel(w,dx,dy)=		!GXLINEREL
	#Draw line from current position, to current position + (dx,dy)

	gdi:=w.gdi
	movetoex(gdi.hdc, gdi.posx, gdi.posy)
	if gdi.drawmode=dm_screenmemory then
		movetoex(gdi.hdc2, gdi.posx, gdi.posy)
	fi
	x:=gdi.posx+dx
	y:=gdi.posy+dy
	gxline(w,x,y)
	gdi.posx:=x
	gdi.posy:=y
end

export proc gxmove(w,x2,y2)=		!GXMOVE
	#Set current position to x2,y2

	gdi:=w.gdi

	movetoex(gdi.hdc, x2, y2)
	if gdi.drawmode=dm_screenmemory then
		movetoex(gdi.hdc2,x2, y2)
	fi
	gdi.posx:=x2
	gdi.posy:=y2
end

export proc gxmoverel(w,dx,dy)=		!GXMOVEREL
	#	Set current position to current position+(dx,dy)
	gdi:=w.gdi

	gdi.posx+:=dx
	gdi.posy+:=dy

	movetoex(gdi.hdc,gdi.posx, gdi.posy)
	if gdi.drawmode=dm_screenmemory then
		movetoex(gdi.hdc2,gdi.posx, gdi.posy)
	fi
end

export proc gxrect(w,x,y,width,height)=		!GXRECT
	#draw outline rectangle starting from x,y at top left, in current pen colour
	#overall size is width by height pixels inclusive (x,y to x+width+1,y+height-1)
	#outline is 1 pixel wide

	gdi:=w.gdi
	gdi.updated:=1
	if height<0 then y:=y+height+1; height:=-height fi
	if width<0 then x:=x+width+1; width:=-width fi

	oldbrushstyle:=gdi.brushstyle
	gxbrushstyle(w,bs_hollow)

	rectangle(gdi.hdc,x, y, x+width, y+height)
	if gdi.drawmode=dm_screenmemory then
		rectangle(gdi.hdc2,x, y, x+width,y+height)
	fi
	gxbrushstyle(w,oldbrushstyle)
end

export proc gxcircle(w,x,y,r)=		!GXCIRCLE
	#draw circle at centre x,y in window w, of radius r, using current pen colour
	#outline is 1 pixel wide

	gdi:=w.gdi
	gdi.updated:=1
	oldbrushstyle:=gdi.brushstyle
	gxbrushstyle(w,bs_hollow)

	ellipse(gdi.hdc,x-r, y-r, x+r-1, y+r-1)
	if gdi.drawmode=dm_screenmemory then
		ellipse(gdi.hdc2,x-r, y-r, x+r-1, y+r-1)
	fi
	gxbrushstyle(w,oldbrushstyle)
end

export proc gxfillcircle(w,x,y,r,?colour,mode=0)=		!GXFILLCIRCLE
	#Draw filled circle with optional outline
	#x,y is the centre, r is the radius
	#(When the outline is drawn, the filled region is 1 pixel smaller all round)
	#colour is the colour of the filled region (current brush colour when omitted)
	#mode=1 to draw the outline, or mode=0 (default) to omit it
	#The outline is drawn in the current pen colour
	gdi:=w.gdi

	gdi.updated:=1
	oldbrushcolour:=gdi.brushcolour
	if colour.isdef then
		gxbrushcolour(w,colour)
	fi

	oldpenstyle:=gdi.penstyle
	if mode=0 then		!inside only
		gxstyle(w,ps_null)
	fi

	ellipse(gdi.hdc,x-r, y-r, x+r-1, y+r-1)
	if gdi.drawmode=dm_screenmemory then
		ellipse(gdi.hdc2,x-r, y-r, x+r-1, y+r-1)
	fi

	gxstyle(w,oldpenstyle)
	gxbrushcolour(w,oldbrushcolour)
end

export func gxpixel(w,x,y,?colour)=		!GXPIXEL
	#colour provided: set pixel at point x,y of window w to colour
	#colour omitted: return pixel colour from point x,y
!set pixel at x,y with given rgb colour, or return pixel value if omitted (-1)
	w.gdi.updated:=1

	if colour.isvoid then		!get pixel
		res:=getpixel(w.gdi.hdc, x, y)
		if w.gdi.drawmode=dm_screenmemory then
			getpixel(w.gdi.hdc2, x, y)
		fi
		return res
	else
		setpixel(w.gdi.hdc,x, y, colour)
		if w.gdi.drawmode=dm_screenmemory then
			setpixel(w.gdi.hdc2,x,y,colour)
		fi
		return colour
	fi
end

export func gxcaption(w,?caption)=		!GXCAPTION
	#caption omitted:  return current window caption text
	#caption provided: set new window caption text

	case w.windclass
	when window_class,popup_class then

		if caption.isdef then		!set text
			setwindowtext(w.gdi.hwnd,caption)
			return caption
		else
			buffer:=new(array,byte,512)
			n:=getwindowtext(w.gdi.hwnd,int(&buffer),buffer.len)
			if n then
				s:=makestr(&buffer,n)		!needs assigment to ensure a copy is made befor buffer
											!is freed (assignment of return value might do the same)
			else
				s:=""
			fi
			return s
		fi
	esac

	if caption.isdef then		!set text
		w.text:=caption
		gxdraw(w)
	fi

	return w.text
end

export proc gxtext(w,s,?x,?y)=		!GXTEXT
	#Display text s
	#Text is drawn starting at (x,y) when provided, otherwise at current position
	#insertion point refers either to base line, or to top left of cell (not sure)
	#Text is drawn in current font, size, style and mode
	#Text typically contains no control codes, but can also contain cr and lf (also
	#tabs, but currently position isn't changed). However, text with control codes is
	#drawn a character at a time
	#Finishes with current position set to the end of the text

!uses chr(16) for reverse tab. Reverse tabs are usually encoded as:
!  chr(9)+chr(16), ie. normal tab then reverse tab
!Reverse tab, after tabbing to next stop, then moves position back by width
!of next substring

	return when s=""

	gdi:=w.gdi

	if x.isvoid then x:=gdi.posx fi
	if y.isvoid then y:=gdi.posy fi
	gdi.updated:=1

!scan the string and create a table of substrings and control codes
	startpos::=lengths::=()
	ngroups:=0

	foreach i,c in s do
		if c<32 then
			++ngroups
			startpos[ngroups]:=i
			lengths[ngroups]:=0
		else
			if ngroups and lengths[ngroups] then		!extend this group
				++lengths[ngroups]
			else										!start new substring group
				++ngroups
				startpos[ngroups]:=i
				lengths[ngroups]:=1
			fi
		fi
	od

	for i,l in lengths do
		pos:=startpos[i]
		if l then
			slicex:=pos..pos+l-1
			textout(gdi.hdc,x, y,s.[slicex],l)

			if gdi.drawmode=dm_screenmemory then
				textout(gdi.hdc2,x,y,s.[slicex],l)
			fi
			x +:= gxtextwidth(w,s.[slicex])

		else				!Deal with control codes
			case s.[pos]
			when 13 then
				x:=0
			when 10 then
				y+:=20				!NEEDS TO PICK CURRENT FONT DIMS

			when 9,16 then			!16 will be used as reverse tab
				currx:=x
				x:=0
				for t in tabstops do
					x+:=t*chx				!MUST BE CURRENT FONT NOT CHX
					if x>currx then exit fi
				od
				while x<=currx do x+:=chx*8 od

				if s.[pos]=16 and i<ngroups and lengths[i+1] then	!reverse offset for next substring
					pos:=startpos[i+1]
					x -:= gxtextwidth(w,s.[pos..pos+lengths[i+1]-1])+1
				fi

			esac	
		fi
	od

	gdi.posx:=x
	gdi.posy:=y
end

export func gxtextcolour(w,?colour,?bgndcolour)=		!GXTEXTCOLOUR
	#Set either text foreground colour or background colour, or both, or neither,
	#depending on which are supplied
	#Text colours will be colour indices, not rgb
	#Always returned current or new foreground

	gdi:=w.gdi

	if colour.isdef and colour<>w.style.textfgnd then
		w.style.textfgnd:=colour
		settextcolour(gdi.hdc,getrgb(colour))
		if gdi.drawmode=dm_screenmemory then
			settextcolour(gdi.hdc2,getrgb(colour))
		fi
	fi
	if bgndcolour.isdef and bgndcolour<>w.style.textbgnd then
		gxbgndcolour(w,bgndcolour)
	fi

	return w.style.textfgnd
end

export func gxtextwidth(font,?s)=		!GXTEXTWIDTH
	# font is a window, or a font number within fonttable
	# font can be zero (then uses font 1)
	# return total pixel width of string s, using given font

	if s="" then return 0 fi

!if not font.isint then			!aasume font is window
!CPL =FONT
!CPL =FONT.TYPE


!if not font.ispointer then			!aasume font is window
	if not font.isint then			!aasume font is window
		font:=font.gdi.font
	fi
	if font=0 then font:=1 fi

	selectobject(screendc,fonttable[font])
	widthheight:=new(ws_point)

!CPL =S

	gettextextentpoint32(screendc,s,s.len,&widthheight)

	return widthheight.x
end

export func gxloadfont(n,facename,?style,height=0,width=0)=		!GXLOADFONT
	#define new font
	#N is index into fonttable
	#facename is the name of the font
	#Style is optional font style, a string containing any of:
	#	B,b		Bold
	#	I,i		Italic
	#	U,u		Underline
	#	S,s		Strikeout
	#Height is height of text (default 0, gives default height?)
	#Weight is width; defautl 0 normally used for normal aspect of text

	CPL "GXLOADFONT"

	if n<=0 then return 0 fi
!n:=min(n,nglobalfonts)

	if style.isvoid then style:="" fi

	if n<=nglobalfonts and fonttable[n] then			!remove existing font
		igxremovefont(n)
	fi

	p:=style
	bold:=400
	italic:=0
	underline:=0
	strikeout:=0
	for c in style do
		case asc(convuc(c))
		when 'B' then bold:=700
		when 'I' then italic:=1
		when 'U' then underline:=1
		when 'S' then strikeout:=1
		esac
	od

	hfont:=createfont(
		facename:	facename,
		height:		height,
		width:		width,
		bold:		bold,
		italic:		italic,
		underline:	underline,
		charset:	0,
		quality:	2,
		escapement:	0,
		orientation:0)

	if hfont=0 then
		hfont:=getstockobject(system_font)
	fi

	fonttable[n]:=hfont
	nglobalfonts:=max(n,nglobalfonts)

	selectobject(screendc,fonttable[n])

	tm:=new(ws_textmetrics)

	gettextmetrics(screendc,&tm)

	fontdimtable[n]::=ws_point(tm.avecharwidth, tm.height+tm.externalleading)
	fontvdimtable[n]::=ws_point(tm.ascent, tm.descent)

	selectobject(screendc,getstockobject(system_font))

	return n
end

proc igxremovefont(n)=
!unload font n, free table entry

	unless n in 1..nglobalfonts then return end
	if fonttable[n]=0 then return fi	!already freed

!w:=windowlist
!while w<>nil do
!	if w.gdi.hdc and w.gdi.font>0 then		!font was in use, set as undefined
!		w.gdi.font:=1
!		selectobject(w.gdi.hdc,fonttable[w.gdi.font])
!		if w.gdi.hdc2 then
!			selectobject(w.gdi.hdc2,fonttable[w.gdi.font])
!		fi
!	fi
!	w:=w.nextwind
!od

	deleteobject(fonttable[n])		!get rid of this font
	fonttable[n]:=0
end

export func gxfont(w,font=1)=		!GXFONT
	# select font from font table for subsequent text display; default is font 1

	if not w then w:=wapplic fi
	if not w then w:=wscreen fi

	gdi:=w.gdi

	if font.isdef and font<>gdi.font then
		if font not in 1..nglobalfonts then
			abort("Bad font number "+tostr(font))
		fi
		gdi.font:=font
		if fonttable[font]=0 then
			abort("Font not in use "+tostr(font))
		fi

		oldhfont:=selectobject(gdi.hdc,fonttable[font])
		sendmessage(gdi.hwnd,wm_setfont,fonttable[font],0)

		if gdi.drawmode=dm_screenmemory then
			oldhfont:=selectobject(gdi.hdc2,fonttable[font])
			sendmessage(gdi.hwnd2,wm_setfont,fonttable[font],0)
		fi
		if fontdimtable[font].x=0 then		!set up dims
			gxchardim(font,0)
		fi
	fi
	return gdi.font
end

func hascontrolchars(s)=		!TESTCTRLCHAR
!scan string s looking for control chars
!return 1 if control chars (<20H) are present

	foreach c in s do
		if c<32 then return 1 fi
	od
	return 0
end

export func gxchardim(font,vert=0)=			!GXCHARDIM
	#return font char average width/height info as a point rec
	#wfont is a font number, or hwindow when the current font in that window is used
	#vert=1 means get ascent/descent pair instead of (vert=0) average width/height

	if not font.isint then
		font:=font.gdi.font
	fi
	if font=0 then font:=1 fi

!CPL =FONTDIMTABLE,=FONT

	if fontdimtable[font].x=0 then		!probably stock fonts not setup with gxloadfont
		selectobject(screendc,fonttable[font])
		tm:=new(ws_textmetrics)
		gettextmetrics(screendc,&tm)

		fontdimtable[font]::=ws_point(tm.avecharwidth, tm.height+tm.externalleading)

		fontvdimtable[font]::=ws_point(tm.ascent, fontvdimtable[font].y:=tm.descent)

		selectobject(screendc,getstockobject(ansi_var_font))
	fi

	if vert then
		return fontvdimtable[font]
	fi

	return fontdimtable[font]
end

export func gxbgndcolour(w,?colour)=		!GXBGNDCOLOUR
	#Set background colour (for text mainly)
	#colour will be a colour index
	#return current colour when omitted
	gdi:=w.gdi

	if colour.isdef then

		if colour<>w.style.textbgnd then
			w.style.textbgnd:=colour
			setbkcolour(gdi.hdc,getrgb(colour))
			if gdi.drawmode=dm_screenmemory then
				setbkcolour(gdi.hdc2,getrgb(colour))
			fi
		fi
		gxbgndmode(w,(colour<>w.style.windbgnd|1|0))
	fi
	return w.style.textbgnd
end

export func gxbgndmode(w,?mode)=		!GXBGNDMODE
	#	mode supplied: set new background mode:
	#		1 y Y T		Set opaque (T for True? Looks like Transparent)
	#		0 n N F		Set Transparent

	gdi:=w.gdi

	if mode.isdef  then
		case mode
		when 1,'y','Y','T' then
			w.style.bgndmode:=opaque
		else
			w.style.bgndmode:=transparent
		esac

		setbkmode(gdi.hdc,mode+1)
		if gdi.drawmode=dm_screenmemory then
			setbkmode(gdi.hdc2,mode+1)
		fi
	fi
	return w.style.bgndmode
end

export proc gxhighlight(w,x,y,width,height)=		!GXHIGHLIGHT
	#Invert rectangular region
	const dstinvert=0x00550009	!patblt
	gdi:=w.gdi

	gdi.updated:=1
	patblt(gdi.hdc, x, y, width,height,dstinvert)
	if gdi.drawmode=dm_screenmemory then
		patblt(gdi.hdc2, x,y, width,height,dstinvert)
	fi
end

export proc gxbitblt(w,x2,y2,width,height,x,y)=			!GXBITBLT
	#Copy rectangular region of window to another location
	gdi:=w.gdi
	gdi.updated:=1
	bitblt(gdi.hdc, x2, y2, width,height,
				gdi.hdc,x,y,srccopy)

	if gdi.drawmode=dm_screenmemory then
		bitblt(gdi.hdc2,x2,y2,width,height,gdi.hdc2,x,y,srccopy)
	fi
end

export func gxaskmess(mode=0)=
	#wait for next message and return message number
	#return 0 if close or quit message seen
	#some messages will be ignored here (processsed via procmess) and will wait for next
	#mode=0		Return currmess.message
	#mode=1		Return currmess.message, but if a mm_command message, then return
	#			the command id. This means message numbers and command ids share the
	#			same space. This should work because messages are below 200, and ids above 200

!CPL "AM1",$REFCOUNT(WAPPLIC)
	repeat
		if mxwait_mm_message()=0 then
CPL "GXAM MAXWAIT =0"
			return mm_cancel
		fi
		if quitmess then return mm_cancel fi
		x:=process_message(currmess)

		if currmess.message=mm_key and currmess.a=27 then
			return mm_cancel
		fi

	until x=thismess					!message ready to return

	if mode=1 and currmess.message=mm_command then
		return currmess.a
	fi

	return currmess.message
end

func process_message(mess)=
	#user or default event processing for mm message
	#will call event handler if there is one
	#returns 1 (skipmess) if message has been processed here; caller must wait for another message
	#returns 0 (thismess) caller should deal with this message (it has not been processed, or has beenbut caller can process it too)

!	CP "	";SHOWMESSAGE("PROCMESS",MESS)


	if mess.wind=nil then
		return thismess
	fi

	case mess.message
	when mm_close then
		return thismess
	esac

!CPL "PROCESS MESSAGE"

	status:=domessage(mess)
!CPL =STATUS,"AFTER DOMESS"

	return status
end

export proc docs=
!export proc where dostringzs for entire lib can go
	#Option dicts: used as args to gccreatewindow/gxcreatechildwindow:
	#	wf_border		Border style; see wbs_ enums
	#	wf_resize		1 for resizable border (for top-level windows)
	# wf_hscroll		1 for horizontal scrollbar
	# wf_vscroll		1 for vertical scrollbar
	# wf_menu			1 for a menubar
	# wf_caption		1 for a caption bar (needs to be the right kind of border too)
	# wf_max			1 for a max button
	# wf_minmax		1 for a min/max buttons
	# wf_sysmenu		1 for a system menu (right-click on top left I think)
	# wf_desktop		1 to fill desktop
	# wf_clip			1 to clip windows to desktop
	# wf_show			1 to show window after creating
	# wf_iframe		1 for pos/dim to refer to frame rather than client area
	# wf_cent			1 to centre window
	# wf_toolwind		1 for tool window (not sure what this means)

end

func newwindow(hwnd,index,windclass,borderstyle)=

	w:=new(rwindow,0)
	w.windclass:=windclass
!w.style.borderstyle:=borderstyle
	w.index:=index
	w.childlist::=()
	w.owner:=nil
	w.closed:=0

	addwindow(w)
	wx_setw(hwnd,w.gindex)
	return w
end

export func getrgb(index)=
	if index=0 then return 0 fi
	return colourvalues[index]
end

func readstyle(owner,windclass,options)=
!process gx options stored in the given dict type
!return a stylerec all filled in

	if options.type=stylerec then			!already a stylerec
		return options
	fi

	ss:=new(stylerec)
	if options.isvoid then				!use bunch of defaults
		d::=defstyle
		d.border:=defaultborderstyles[windclass]
		return d
	fi

	ss.border	:=options{ss_border,defaultborderstyles[windclass]}
	ss.justify	:=options{ss_justify,defstyle.justify}
	ss.vjustify	:=options{ss_vjustify,defstyle.vjustify}
	ss.textfgnd	:=options{ss_textfgnd,defstyle.textfgnd}
	ss.textbgnd	:=options{ss_textbgnd,defstyle.textbgnd}
	ss.bgndmode	:=options{ss_bgndmode,defstyle.bgndmode}

	ss.iframe	:=options{ss_iframe,0}

	ss.hilitetype	:=options{ss_hilitetype,defstyle.hilitetype}
	ss.marktype	:=options{ss_marktype,(ss.hilitetype|0|defstyle.marktype)}
	ss.imark	:=options{ss_imark,defstyle.imark}

	if windclass in [toggle_class, select_class,mark_class] and ss.marktype then
		def:=owner.style.windbgnd
	else
		def:=defstyle.windbgnd
	fi

	ss.windbgnd	:=options{ss_windbgnd,def}
	ss.hscroll	:=options{ss_hscroll,0}
	ss.vscroll	:=options{ss_vscroll,0}
	ss.lbchange	:=options{ss_lbchange,0}
	ss.returnmess	:=options{ss_returnmess,0}
	ss.noupdate	:=options{ss_noupdate,0}

	return ss
end

export func gxpanel(owner,pos,dim,?style)=
	ss:=readstyle(owner,panel_class,style)
	w:=gxcontrol(owner,panel_class,pos,dim,ss)
	gxdraw(w)

	return w
end

export func gxgroup(owner,pos,dim,?style)=
	ss:=readstyle(owner,group_class,style)
	w:=gxcontrol(owner,group_class,pos,dim,ss)
	gxdraw(w)

	return w
end

export func gxstatusbar(owner,pos,dim,?style)=

	ss:=readstyle(owner,statusbar_class,style)

	if ss.iframe=0 then					!frame not included, but can't have it leaking outside owner
		bs:=ss.border
		if bscat[bs]<>'I' then			!do adjustments
			dim+:=bswidths[bs].y1+bswidths[bs].y2
		fi
		ss.iframe:=1					!stop gxcontrol expanding dims
	fi

	(ecapos,ecadim):=gxclientarea(owner)

	if pos.isint then pos:=chr(pos) fi
!if pos.ispointer then pos:=chr(pos) fi
	if convuc(pos) in "T TOP" then			!along the top
		pos:=ecapos
		dir:='T'
	else									!along the bottom
		pos:=(ecapos[1],ecadim[2]-dim+ecapos[2])
		dir:='B'
	fi
	dim:=(ecadim[1],dim)

	ss.dir:=dir

	w:=gxcontrol(owner,statusbar_class,pos,dim,ss)

	gxdraw(w)

	return w
end

export func gxframebar(owner,pos,dim,?style)=

	ss:=readstyle(owner,framebar_class,style)
	if ss.iframe=0 then					!frame not included, but can't have it leaking outside owner
		bs:=ss.border
		if bscat[bs]<>'I' then			!do adjustments
			dim+:=bswidths[bs].y1+bswidths[bs].y2
		fi
		ss.iframe:=1					!stop gxcontrol expanding dims
	fi

	(ecapos,ecadim):=gxclientarea(owner)

!if pos.isint then pos:=chr(pos) fi
	if pos.ispointer then pos:=chr(pos) fi
	if convuc(pos) in "L LEFT" then			!along the left
		pos:=ecapos
		dir:='L'
	else									!along the right
		pos:=(ecadim[1]-dim+ecapos[1],ecapos[2])
		dir:='R'
	fi
	dim:=(dim,ecadim[2])

	ss.dir:=dir

	w:=gxcontrol(owner,	framebar_class,pos,dim,ss)

	gxdraw(w)

	return w
end

export func gxbutton(owner,pos,dim,caption,?style,id=201,enable=1)=
	#create clickable button
	#returns rwindow

	ss:=readstyle(owner,button_class,style)

	w:=gxcontrol(owner,button_class,pos,dim,ss)
	w.id:=id

	w.text:=caption
	w.enable:=enable
	gxdraw(w)

	return w
end

export func gxlabel(owner,pos,dim,caption,?style)=
	#create static label button
	#returns rwindow

	ss:=readstyle(owner,label_class,style)

	w:=gxcontrol(owner,label_class,pos,dim,ss)

	w.text:=caption
	gxdraw(w)

	return w
end

func gxcontrol(owner,windclass=button_class,pos,dim,?ss)=

	if ss.type=dict or ss.isvoid then
		ss:=readstyle(owner,windclass,ss)
	fi
	wb:=wbs_none
	case ss.border			!find wbs- version of windows-drawn borders
	when bs_simplew then
		wb:=wbs_simple
	esac

	if ss.iframe and bscat[ss.border]<>'I' then
		widths:=bswidths[ss.border]
		pos[1]+:=widths.x1
		pos[2]+:=widths.y1
		dim[1]-:=widths.x1+widths.x2
		dim[2]-:=widths.y1+widths.y2
	FI

	hwnd:=wx_createcontrol(pos:pos,dim:dim,border:wb,owner:owner.gdi.hwnd)

	if hwnd=0 then
		abort("Can't create control window")
	fi

	w:=newwindow(hwnd,0,no_class,ss.border)
	w.windclass:=windclass
	w.style:=ss
	w.owner:=owner
	w.enable:=1

	setwindowdims_c(w,hwnd)
	setupgdi(w,hwnd)

	gxdrawmode(w,dm_screenmemory)

	gxfont(w,labelfont)

	gxtextcolour(w,w.style.textfgnd,w.style.textbgnd)

	gxbgndmode(w,w.style.bgndmode)

!link into owner
	w.owner.childlist append:=w
	w.index:=w.owner.childlist.upb

	return w
end

export func gxtoggle(owner,pos,dim,caption="",linkvar,?style,id=201,enable=1)=

	(posx,posy):=pos
	(dimx,dimy):=dim
	textoffset:=0


	ss:=readstyle(owner,toggle_class,style)

!work out whether an auxiliary window is needed

	if ss.marktype then
		if ss.imark=0 then			!dims don't include the mark
			posx-:=markdim
			dimx+:=markdim
			textoffset:=markdim
		fi
	fi

	w:=gxcontrol(owner,toggle_class,(posx,posy),(dimx,dimy),ss)
!RETURN 0
	w.linkvar:=linkvar
	w.id:=id
	w.text:=caption
	w.attrs:=togglerec(textoffset,1)
	w.enable:=enable

	if w.style.marktype then
!	gxmark(owner:w,pos:(0,(w.dimy-markdim)%2),id:id,style:style)
		gxmark(owner:w,pos:(0,(w.dimy-markdim)%2),id:id, style:ss)
	fi

	gxdraw(w)
	return w
end

export func gxselect(owner,pos,dim,caption="",linkvar,onvalue,?style,id=201,enable=1)=

	(posx,posy):=pos
	(dimx,dimy):=dim
	textoffset:=0

	ss:=readstyle(owner,select_class,style)

!work out whether an auxiliary window is needed
	if ss.marktype and ss.imark=0 then			!dims don't include the mark
		posx-:=markdim
		dimx+:=markdim
		textoffset:=markdim
	fi

	w:=gxcontrol(owner,select_class,(posx,posy),(dimx,dimy),ss)

	w.linkvar:=linkvar
	w.id:=id
	w.text:=caption
	w.attrs:=togglerec(textoffset,onvalue)
	w.enable:=enable
	if w.style.marktype then
		gxmark(owner:w,pos:(0,(w.dimy-markdim)%2),id:id, style:style)
	fi

	gxdraw(w)
	return w
end

export proc showmessage(caption,mess)=
!RETURN
!	CPL MESS.MESSAGE
	cp caption,,":",leftstr(messagenames[mess.message],14)
	cp "A:",,mess.a,"B:",,mess.b
	cp " (X:",,mess.x,"Y:",,mess.y,,") Buttons:",mess.state:"b"

	if mess.wind then
		cpl "	Window:",mess.wind.name
	else
		cpl
	fi
end

func domessage(mess)=

	m:=mess.message
	w:=mess.wind

	case m
	when mm_move,mm_setcursor then
		return skipmess
	esac

!CPL "DOMESSAGE",MESSAGENAMES[M]

!	CPL messhandlertable[m,w.windclass]
	x:=messhandlertable[m,w.windclass](mess,w)

	return x
end

proc init_handlertables=
	messhandlertable:=maketable(mm_null..mm_last, no_class..dummy_class, nil)
	actionhandlertable:=maketable(actionnames.bounds, no_class..dummy_class, nil)

	messalltable:=new(list,mm_null..mm_last,0)		!for all mess_mess_all handlers
	fnallall:=nil									!for single mess_all_all handler
	fnfixups:=nil

	actionalltable:=new(list,actionnames.bounds,0)

	for d to $nprocs() do
		fnptr:=$procref(d)
		fnname:=$procname(d)

		(name,messname,windname):=splitstring(fnname,"_")			!split func name

		if fnname="gxhandler_fixups" then
			fnfixups:=fnptr
		elsif leftstr(fnname,5)="mess_" then
			if messname="all" and windname="all" then
				fnallall:=fnptr
			else
				message:=("mm_"+messname) inx messagenames
				if not message.isfound then
					ABORT("CAN'T FIND MESSAGE <"+messname+">")
				fi

				if windname="all" then				!assume <mess> all
					messalltable[message]:=fnptr
				else
					messhandlertable[message,WX:=findwindclass(windname)]:=fnptr
				fi
			fi

		elsif leftstr(fnname,8)="do_draw_" or leftstr(fnname,10)="do_update_" then
			action:=messname+"_w" inx actionnames
			if not action.isfound then
				ABORT("CAN'T FIND ACTION "+MESSNAME)
			fi
			if windname="all" then
				actionalltable[action]:=fnptr
			else
				windclass:=findwindclass(windname)
				actionhandlertable[action,windclass]:=fnptr
			fi
		fi
	od

!!do some manual fixups
	if fnfixups then
		fnfixups()
	fi

	for mx:=0 to mm_last do
		for wx:=0 to dummy_class do
			if not messhandlertable[mx,wx] then
				messhandlertable[mx,wx]:=(messalltable[mx]|messalltable[mx]|fnallall)
			fi
		od
	od

	if not fnallall then
		pcerror("Can't find all/all mess handler")
	fi

	for ax:=1 to DRAW_w do
		for wx:=0 to dummy_class do
			if not actionhandlertable[ax,wx] then
				if not actionalltable then
					pcerror("No DO/ALL handler for:"+actionnames[ax])
				fi
				actionhandlertable[ax,wx]:=actionalltable[ax]
			fi
		od
	od
end

func findwindclass(name)=
		windclass:=name+"_class" inx windowclassnames
		unless windclass.isfound then
			ABORT("CAN'T FIND WINDOW "+windname)
		end
		return windclass
end

export proc gxdraw(w)=
	fnptr:=actionhandlertable[draw_w,w.windclass]

	if fnptr then
		fnptr(w)
	else
		cpl "NO DRAW HANDLER",windowclassnames[w.windclass],w.name
		waitkey()
		stop
	fi
end

export proc gxupdate(w)=
	fnptr:=actionhandlertable[update_w,w.windclass]
	if fnptr then
		fnptr(w)
	else
		gxdraw(w)
	fi
end

export proc eventloop=
	do
		m:=gxaskmess()

		SHOWMESSAGE("EVENTLOOP",CURRMESS)

		case m
		when 0,mm_cancel then
			return
		esac

	od
end

export func gxeditbox(owner,pos,dim,linkvar,?style,id=201,enable=1)=

	ss:=readstyle(owner,editbox_class,style)

	w:=gxcontrol(owner,editbox_class,pos,dim,ss)

	w.linkvar:=linkvar
	w.id:=id
	w.attrs:=new(editboxrec)
	w.attrs.currpos:=linkvar^.len+1
	w.enable:=enable

	gxdraw(w)
	return w
end

export proc gxebchange(w,?linkvar,charpos=-1)=

	if linkvar.isdef then
		w.linkvar:=linkvar
	fi

	if charpos=-1 then
		w.attrs.currpos:=w.linkvar^.len+1
	else
		w.attrs.currpos:=charpos
	fi
	gxupdate(w)
end

export proc gxsetlbdata(w,linkvar,?pos)=
	w.linkvar:=linkvar
	if pos.isvoid then
		pos:=(linkvar^|1|0)
	fi
	w.attrs.currpos:=pos

	if w.childlist[1] then
!	gxsetscrolllimits(ws,linkvar^.bounds,w.attrs.rows)
		gxsetscrolllimits(ws,getlvbounds(linkvar),w.attrs.rows)
		gxscrollpos(ws,pos)
	fi
end

export proc gxsetlbpos(w,pos)=
!change in pos
	w.attrs.currpos:=pos

!work out screen row
	if pos then
		oldpagepos:=w.attrs.pagepos
		if pos<oldpagepos then
			w.attrs.pagepos:=pos
		elsif pos>oldpagepos+w.attrs.rows-1 then
			w.attrs.pagepos:=pos-w.attrs.rows+1
		fi
		if w.attrs.pagepos<>oldpagepos then
			if w.childlist then
				gxscrollpos(w.childlist[1],w.attrs.pagepos)
			fi
			m:=mm_draw
		else
			m:=mm_update
		fi
	else
		m:=mm_draw
	fi

	postmess(w,m)
	if w.style.lbchange then
		postmess(w,mm_lbchange,w.attrs.currpos)
	fi
end

export proc gxsetlbpage(w,pagepos)=
!change in pagepos (originates from scrollbar message)
	w.attrs.pagepos:=pagepos

	oldpos:=w.attrs.currpos
	if oldpos<pagepos then
		w.attrs.currpos:=pagepos
	elsif oldpos>=pagepos+w.attrs.rows then
		w.attrs.currpos:=pagepos+w.attrs.rows-1
	fi

	if w.childlist then
		gxscrollpos(w.childlist[1],pagepos)
	fi

	postmess(w,mm_draw)
	if w.style.lbchange and oldpos<>w.attrs.currpos then
		postmess(w,mm_lbchange,w.attrs.currpos)
	fi
end

export func gxlistbox(owner,pos,dim,linkvar,?style,id=201,rows=0,pitch=0,offset=0)=

	ss:=readstyle(owner,listbox_class,style)

	(dimx,dimy):=dim
	if ss_vscroll and ss_imark=0 then			!dims don't include the scrollbar
		dimx+:=arrowdim
	fi

	w:=gxcontrol(owner,listbox_class,pos,(dimx,dimy),ss)
	w.linkvar:=linkvar
	w.id:=id
	w.attrs:=new(listboxrec)

	if pitch=0 then								!calculate all these here
		pitch:=listrowheight
		offset:=0
		rows:=w.dimy%pitch
	fi
	w.attrs.rows:=rows
	w.attrs.pitch:=pitch
	w.attrs.offset:=offset

	w.attrs.pagepos:=1
	w.attrs.currpos:=(getlvbounds(linkvar).len|1|0)

	if w.style.vscroll then
		ws:=gxvertscrollbar(owner:w,pos:(w.dimx-arrowdim,0),dim:w.dimy,id:id,style:style)
		gxsetscrolllimits(ws,getlvbounds(linkvar),w.attrs.rows)

		gxscrollpos(ws,getlvbounds(linkvar).lwb)
	fi

	gxdraw(w)
	return w
end

export func gxarrow(owner,pos,?dim,dir,?style,id=201)=

	ss:=readstyle(owner,arrow_class,style)
	if dim.isvoid then
		dim:=(arrowdim,arrowdim)
	fi

	w:=gxcontrol(owner,arrow_class,pos,dim,ss)
	w.id:=id
	if dir.isstring then dir:=asc(dir) fi
	case dir					!allow compass bearings too, but convert to UDLR
	when 'N' then dir:='U'
	when 'E' then dir:='R'
	when 'S' then dir:='D'
	when 'W' then dir:='L'
	esac

	w.style.dir:=dir			!don't dir allow via style options
	gxdraw(w)

	return w
end

export proc gxsetscrolllimits(w,limits,span=0)=
!set up or change scrollbar limits
!span=0:
!	Pure ranging control. Limits are actual range of the thumb.
!	Thumb is drawn at a fixed, nominal size. Might be suppressed when limits are <=1
!	Initial position set to limits.lwb
!span=M:
!	Paging control, such as used on a listbox or text editor.
!	Span can be the number of rows display at one time.
!	Limit can be single number N, or range 1..N.
!	Actual scroll bar range will be 1..N-M+1. When upper limit<1 then
!	limit will be 1..1, and thumb might not be drawn
!	Data position will: actually there /is/ not data position, except for the
!	data position represented by the top row, which will be the same as the scroll
!	position.
!Arrows should be disabled (and perhaps thumb suppressed) when scroll range if 1..1,
!or data range is nor larger than a span

	w.attrs.span:=span
	if w.style.dir='H' then
		width:=w.dimx
	else
		width:=w.dimy
	fi
	m:=width-arrowdim*2				!number of pixels movement between arrows

	if span=0 then						!pure scrolling control
		w.attrs.limits:=limits
		w.attrs.currpos:=limits.lwb
		w.attrs.thumbsize:=arrowdim
		enable:=limits.len>1
		w.attrs.thumbsize:=arrowdim*enable
	else
		if limits.isrange then
			length:=limits.len
		else
			length:=limits
		fi
		if length<=span then
			enable:=0
			w.attrs.limits:=1..1
			w.attrs.thumbsize:=0
		else
			w.attrs.limits:=1..length-span+1
			enable:=1
			w.attrs.thumbsize:=max(10,int(m*(span/length)))
		fi
	fi

	w.attrs.currpos:=w.attrs.limits.lwb
	w.enable:=enable

	w.attrs.thumbspan:=m-w.attrs.thumbsize		!movement available to thumb
	w.attrs.thumbpos:=arrowdim
	postmess(w,mm_draw)
end

export func gxscrollpos(w,pos,u=0)=
!
	if pos.isvoid then
		return w.attrs.currpos
	fi

	CPL =POS
	CPL =W.ATTRS.CURRPOS
	CPL =W.ATTRS.LIMITS

	w.attrs.currpos:=pos
	if pos not in w.attrs.limits then
		pcerror("Bad scroll pos")
	fi

	tpos:=int(w.attrs.thumbspan*((pos-w.attrs.limits.lwb)/(w.attrs.limits.len-1)))
	w.attrs.thumbpos:=arrowdim+tpos

	w.childlist[1].enable:=pos>w.attrs.limits.lwb
	w.childlist[2].enable:=pos<w.attrs.limits.upb

	if u then
		postmess(w,mm_update)
	fi
	return 0
end

export func gxhozscrollbar(owner,pos,dim,?style,id=201)=

	ss:=readstyle(owner,scrollbar_class,style)
	width:=arrowdim
	if dim.isint then
!if dim.ispointer then
		dim:=(dim,width)
	else
		width:=dim[1]
	fi

	w:=gxcontrol(owner,scrollbar_class,pos,dim,ss)
	w.id:=id
	w.style.dir:='H'

	w.attrs:=new(scrollbarrec)
	w.flags.[wa_leftdrag]:=1

!Now, create the arrows at each end. The thumbbar is not an explicit control,
!it's just a drawn box
	wa:=gxarrow(owner:w, pos:(0,0), dim:(width,width),dir:'L')
	wb:=gxarrow(owner:w, pos:(dim[1]-width,0), dim:(width,width),dir:'R')

	gxsetscrolllimits(w,1..200,20)
	gxscrollpos(w,1)

	gxdraw(w)

	return w
end

export func gxvertscrollbar(owner,pos,dim,?style,id=201)=
	ss:=readstyle(owner,scrollbar_class,style)
	width:=arrowdim
	if dim.isint then
!if dim.ispointer then
		dim:=(width,dim)
	else
		width:=dim[2]
	fi

	w:=gxcontrol(owner,scrollbar_class,pos,dim,ss)
	w.id:=id
	w.style.dir:='V'

	w.attrs:=new(scrollbarrec)
	w.flags.[wa_leftdrag]:=1

!Now, create the arrows at each end. The thumbbar is not an explicit control,
!it's just a drawn box
	wa:=gxarrow(owner:w, pos:(0,0), dim:(width,width),dir:'U')
	wb:=gxarrow(owner:w, pos:(0,dim[2]-width), dim:(width,width),dir:'D')
	gxsetscrolllimits(w,100..200,2)

	gxscrollpos(w,100)

	gxdraw(w)

	return w
end

export func gxmark(owner,pos,?dim,?style,id=201)=

	ss:=readstyle(owner,mark_class,style)
	if dim.isvoid then
		dim:=(markdim,markdim)
	fi

	w:=gxcontrol(owner,mark_class,pos,dim,style)
	w.id:=id
	gxdraw(w)

	return w
end

export proc gxfocus(w)=
!switch focus to window w
	if wfocus==w then
		return
	fi

	if wfocus then
		domessage(makemess(wfocus,mm_killfocus))
	fi
	caretdrawn:=0
	domessage(makemess(w,mm_setfocus))
end

export proc gxkillfocus=
	if wfocus then
		drawcaret(0)
	fi
	wfocus:=nil
end

export func gxcopy(w,?bm,x=0,y=0,scalex=1.0,scaley=0,sx=0,sy=0,dimx=0,dimy=0)=		!GXCOPY
!copy bitmap bm to window w, at position x,y in w. Scalex/y can be 0 for 1:1,
!or Scalex/y can be any real value for unequal x/y scaling
!For equal x/y scaling, Scaley can be 0
!Entire bitmap is copied (sx,sy,w,h all 0); for portion, set sx,sy to top left of rect
!and w,h to size to be copied
![1..100]char str

	if bm.isvoid then
		bm:=w
		w:=nil
	fi
	if bm.isvoid then
		return nil
	fi

	if dimx=0 then dimx:=bm.dimx-sx fi
	if dimy=0 then dimy:=bm.dimy-sy fi

	if scalex=0 then scalex:=1.0 fi
	if scaley=0 then scaley:=scalex fi

	if w=nil then		!create appropriate window
		w:=gxcreatewindow(caption:"Bitmap "+tostr(bm.pixelbits)+" bit",pos:(500,500),
				dim:(bm.dimx*scalex,bm.dimy*scaley))
		w.gdi.drawmode:=dm_screenmemory			!default when using auto-window
	fi

	gdi:=w.gdi
	gdi.updated:=1

	mode:=copymode

	setstretchbltmode(gdi.hdc,mode)
	stretchblt(gdi.hdc, x, y,int(dimx*scalex),int(dimy*scaley),
												bm.gdi.hdc,sx,sy,dimx,dimy, srccopy)
	if gdi.drawmode=dm_screenmemory then
		setstretchbltmode(gdi.hdc2,mode)
		stretchblt(gdi.hdc2,x,y,int(dimx*scalex),int(dimy*scaley),
												bm.gdi.hdc,sx,sy,dimx,dimy, srccopy)
	fi

	return w
end

export proc gxrestore(w,?r)=
!repaint window w
!only called when repaint can be done from a backup
!r is the region to restore within w; or restore all if omitted

	if r.isvoid then
		x1:=y1:=0

		width:=w.dimx
		height:=w.dimy
	else
		x1:=r.x1
		y1:=r.x2
		width:=r.x2-x1+1
		height:=r.y2-y1+1
	fi

	case w.gdi.drawmode
	when dm_screen then			!can't restore; need to call gx_draw
		gxdraw(w)
	when dm_screenmemory then
		destdc:=w.gdi.hdc
		sourcedc:=w.gdi.hdc2
	when dm_memoryscreen then
		destdc:=w.gdi.hdc2
		sourcedc:=w.gdi.hdc
	else
		abort("gxrest/?")
	esac

	bitblt(destdc,x1,y1, width,height, sourcedc, x1,y1, srccopy)

end

export func gxdrawmode(w,?drawmode)=
!set or get drawmode
!really requires window to be cleared afterwards.

	olddrawmode:=w.gdi.drawmode
	if drawmode.isvoid then
		return olddrawmode
	fi

	if olddrawmode=drawmode then		!already set
		return drawmode
	elsif olddrawmode<>dm_screen then	!can only change screen => screenmemory/memoryscreen
		abort("gxdrawmode2")			!not memory to anything else
	fi

!assuming currently on screen, will need extra compatible bitmap
	memhwnd:=createcompatiblebitmap(screendc,w.dimx,w.dimy)
	memhdc:=createcompatibledc(nil)
	selectobject(memhdc,memhwnd)

!need to change draw mode
	case drawmode
	when dm_screenmemory then
		w.gdi.hwnd2:=memhwnd
		w.gdi.hdc2:=memhdc
	when dm_memoryscreen then
		w.gdi.hwnd2:=w.gdi.hwnd			!screen becomes secondary
		w.gdi.hdc2:=w.gdi.hdc
		w.gdi.hwnd:=memhwnd
		w.gdi.hdc:=memhdc
	else
		abort("gxdrawmode?")
	esac

	w.gdi.drawmode:=drawmode
	return drawmode
end

export proc switchdest(w)=
!for a window with screenmemory drawmode, switch things around so that
!it's drawing into the memory area only
	gdi:=w.gdi

	case gdi.drawmode
	when dm_screenmemory then
		t:=gdi.hwnd; gdi.hwnd:=gdi.hwnd2; gdi.hwnd2:=t
		t:=gdi.hdc; gdi.hdc:=gdi.hdc2; gdi.hdc2:=t
		gdi.drawmode:=dm_memory
	when dm_memory then
		t:=gdi.hwnd; gdi.hwnd:=gdi.hwnd2; gdi.hwnd2:=t
		t:=gdi.hdc; gdi.hdc:=gdi.hdc2; gdi.hdc2:=t
		gdi.drawmode:=dm_screenmemory
	esac
end

export proc gxclose(w)=

!CPL "GXCLOSE1",$REFCOUNT(W)

	case w.windclass
	when bitmap_class then
	else
!ID:=$ID(W)
		if issubwindow(w,wfocus) then
			wfocus:=nil
		fi

		if issubwindow(w,wmouse) then	
			lastmousewindow:=nil
			wmouse:=nil
		fi

		destroywindow(w.gdi.hwnd)

!IF $ID(CURRMESS.WIND)=ID THEN
!CPL "-------CLOSED CURRMESS.WIND"
!CURRMESS.WIND:=NIL
!FI
!CPL "GXCLOSE2",$REFCOUNT(W)

		gxfreewindow(w)
!CPL "GXCLOSE3",$REFCOUNT(W)

	IF W=WAPPLIC THEN WAPPLIC:=NIL FI

	esac
end

proc gxfreewindow(w)=
!recover memory used by this window and all childwindows
	for wc in w.childlist do
		gxfreewindow(wc)
	od

	removewindow(w)

!CPL "FREEING2",$ID(W),$ID(W.GDI), $REFCOUNT(W), $REFCOUNT(W.GDI)
	w.gdi:=0

!	w:=0
end

export func gxmsgbox(message,caption="",options="")=

	const mb_abortretryignore	= 0x02
	const mb_applmodal			= 0x00
	const mb_defbutton1			= 0x00
	const mb_defbutton2			= 100
	const mb_defbutton3			= 200
	const mb_defbutton4			= 300
	const mb_help				= 4000
	const mb_iconasterisk		= 40
	const mb_iconerror			= 10
	const mb_iconexclamation	= 30
	const mb_iconhand			= mb_iconerror
	const mb_iconinformation	= mb_iconasterisk
	const mb_iconquestion		= 20
	const mb_iconstop			= mb_iconhand
	const mb_iconwarning		= mb_iconexclamation
	const mb_ok					= 0x00
	const mb_okcancel			= 0x01
	const mb_retrycancel		= 0x05
	const mb_right				= 80000
	const mb_setforeground		= 10000
	const mb_systemmodal 		= 1000
	const mb_taskmodal			= 2000
	const mb_yesno				= 0x04
	const mb_yesnocancel		= 0x03
	const mb_topmost			= 0x040000

!return values
	const idfail	= 0
	const idok		= 1
	const idcancel	= 2
	const idabort	= 3
	const idretry	= 4
	const idignore	= 5
	const idyes		= 6
	const idno		= 7

	static var rettable=(0:"fail","ok","cancel","abort","retry","ignore","yes","no",
			"","","tryagain","continue")

	static var styletable=(
	("bari",mb_abortretryignore),
	("bo",mb_ok),
	("boc",mb_okcancel),
	("brc",mb_retrycancel),
	("byn",mb_yesno),
	("bync",mb_yesnocancel),
	("ix",mb_iconexclamation),
	("iw",mb_iconwarning),
	("ii",mb_iconinformation),
	("iq",mb_iconquestion),
	("is",mb_iconstop),
	("ie",mb_iconerror),
	("ih",mb_iconhand),
	("d1",mb_defbutton1),
	("d2",mb_defbutton2),
	("d3",mb_defbutton3),
	("d4",mb_defbutton4),
	("h",mb_help),
	("rj",mb_right),
	("sm",mb_systemmodal))

	hwnd:=nil

	style:=0
	optioncodes:=splitstring(options," ")

	for opt in optioncodes do
		for i to styletable.len do
			if styletable[i,1]=opt then style ior:=styletable[i,2] fi
		od
	od

	style ior:=0x10000

	x:=messageboxa(hwnd,message,caption,style)
	return rettable[x]
end


export proc gxhandler(windclass,mess,fnptr)=
!windclass is a window, or a window class
!override the current message handler for w's window class, and fo message mess

!if not windclass.isint then
	if not windclass.ispointer then
		windclass:=windclass.windclass
	fi

!CPL "SETTING GXHANDLER",MESS,WINDCLASS,FNPTR
	messhandlertable[mess,windclass]:=fnptr
end

export func gxaskfile(caption="File",filespec="*.*",deffile="",startdir="")=

	save:=0
	if caption='*' then
		save:=1
		caption:=rightstr(caption,-1)
	fi

	filters:=array(filespec+"@@@")		!turn into a byte-array

	for i,bb in filters do			!convert all @ into embedded zeros
		if bb='@' then filters[i]:=0 fi
	od

	ofn:=new((iswin32|ws_openfilename32|ws_openfilename64))

	ofn.structsize:=ofn.bytes
	ofn.owner:=wapplic.gdi.hwnd
	ofn.instance:=getmodulehandle(0)
	ofn.filter:=int(&filters)
	ofn.flags:=ofn_explorer ior ofn_nochangedir ior ofn_hidereadonly !IOR OFN_NOVALIDATE

	ofn.initialdir:=getstringz(startdir)

	ofn.defext:=getstringz("")

	result:=new(array,byte,300)

	result[1]:=0
	if deffile<>"" then
		memcpy(&result,&deffile,deffile.len)
	fi

	ofn.file:=int(&result)

	ofn.maxfile:=256
	ofn.title:=getstringz(caption)

	if not (not save | getopenfilenamea(&ofn) | getsavefilenamea(&ofn)) then
		result[1]:=0		!return "" on error
	fi

	return string(result)
end

export func gxcurrpos(w)=
	return w.attrs.currpos
end

export func gxtabstops(?tabs,signed=0)=
	if tabs.isdef then
		tabstops::=tabs
		if signed then
			for i,x in tabstops do
				tabstops[i]:=abs(x)
			od
		fi
	fi
	return tabstops

end

export func getlvbounds(linkvar)=
	if linkvar.ispointer and linkvar^.islist then
		return linkvar^.bounds
	else
		return linkvar.getbounds()
	fi
	return 0
end

export func getlvitem(linkvar,n)=
	if linkvar.ispointer and linkvar^.islist then
		return linkvar^[n]
	else
		PCERROR("GETLVITEM")
	fi
	return 0
end

export func getlvstritem(linkvar,n)=
	if linkvar.ispointer and linkvar^.islist then
		return tostr(linkvar^[n])
	else
		return linkvar.getstritem(n)
	fi
	return 0
end

export proc gxtext16(w,s,n,x=0,y=0)=		!GXTEXT
		gdi:=w.gdi

		textoutw(gdi.hdc,x, y,&s,n)
		if gdi.drawmode=dm_screenmemory then
			textoutw(gdi.hdc2,x,y,&s,n)
		fi
end

export func gxenable(w,flag)=
	if flag.isdef then
		w.enable:=flag
		gxupdate(w)
	fi
	return w.enable
end

export func gxclientarea(w)=
!scan child windows of w, work out remaining client area after taking account of
!framebars etc
!return (pos, dim), each being a 2-element list

	aposx:=aposy:=0

	adimx:=w.dimx
	adimy:=w.dimy

	centx:=(aposx+adimx)%2
	centy:=(aposy+adimy)%2

!for cw in w.childlist when cw.windclass in [statusbar_class,framebar_class] do
	for cw in w.childlist do

		(posx,posy):=(cw.frameposx,cw.frameposy)
		(dimx,dimy):=(cw.framedimx,cw.framedimy)

!need to find out which of the four sides the bar is against, and set up side= L R T B
		case cw.style.dir
		when 'B' then				!bottom
			if posy<(aposy+adimy) then
				adimy-:=dimy
			fi

		when 'T' then				!top
			if (posy+dimy)>aposy then		!
				aposy+:=(posy+dimy)
				adimy-:=(posy+dimy)
			fi

		when 'R' then				!right
			if posx<(aposx+adimx) then
				adimx-:=dimx
			fi

		when 'L' then				!LEFT
			if (posx+dimx)>aposx then		!
				aposx+:=(posx+dimx)
				adimx-:=(posx+dimx)
			fi
		else

			if dimx>dimy then			!assume hoz
				if posy>centy then			!assume bottom
					if posy<(aposy+adimy) then
						adimy-:=dimy
					fi

				else					!top
					if (posy+dimy)>aposy then		!
						aposy+:=(posy+dimy)
						adimy-:=(posy+dimy)
					fi
				fi
			else					!assume vert
				if posx>centx then			!assume right

					if posx<(aposx+adimx) then
						adimx-:=dimx
					fi

				else					!left

					if (posx+dimx)>aposx then		!
						aposx+:=(posx+dimx)
						adimx-:=(posx+dimx)
					fi

				fi
			fi
		esac
	od

	return ((aposx,aposy), (adimx,adimy))
end

export func addwindow(w)=
!w is a newly created window
!add it to all windows
	n:=nil inx allwindows
	if not n.isfound then
		n:=allwindows.len+1
	fi

	allwindows[n]:=w
	w.gindex:=n
	return n
end

export proc removewindow(w)=
!remove w from all windows
	n:=w inx allwindows
	if n.isfound then
		allwindows[n]:=nil
	fi

	for i to nmessages do
		m:=messagequeue[i]
		if m.wind==w then
			m.wind:=nil
		fi
	od

!	if currmess.wind==w then
!		currmess.wind:=nil
!	fi

end

func process_wmmessage(msg)=
!STATIC VAR CC=0
!CPL "PROCESS/WMMESSAGE",++CC
!CPL "PROC/WM1",MSG, MSG.HWND
		x:=process_wmmessage2(msg)
!CPL "RETURN X:",X
!$SETDEBUG(1)
		return x
end

func process_wmmessage2(msg)=
!msg is a windows rmsg record
!Called from MainWndProc callback func (via mechanisms for B code to call into MPL code)
!this func processes some wm_ Windows messages and converts them
!into mm_ messages as necessary
!It returns:
!	0 The wm_ message has been processed
!	1 The wm_ message has not been processed, and the caller should call DefWindowProc.
!	  Or, the DefWindowProc should also be called anyway.

!CPL "PROCESSWMM2",MSG.MESSAGE, WINMESSAGENAMES{MSG.MESSAGE}

	hwnd:=msg.hwnd
!CPL "PM2",=HWND,MSG.HWND
	w:=getwindow(hwnd)
!CPL "AFTER GW1"

	message:=msg.message
	wparam:=msg.wparam
	lparam:=msg.lparam

	case msg.message
	when wm_command then
		w:=getwindow(lparam)			!w was owner, use control window
!CPL "AFTER GW2"
		i:=wparam iand 0xffff			!id
		j:=wparam>>16				!notify code
		m:=mm_command

		if not w then
			w:=wapplic
		fi

		postmess(w,m,i,j,0)

		return 0

	when wm_activate then
		if wparam then				!being activated
		fi

	when wm_syskeydown,wm_syskeyup,wm_keydown,wm_keyup then
!
!	STATIC VAR COUNT=0
!
!	IF MSG.MESSAGE=WM_KEYDOWN THEN
!	CPL "KEY",++COUNT
!	FI

		if dokeymessage(hwnd,message,wparam,lparam) then
			return 0
		fi

	when wm_char then
		postmess((wfocus|wfocus|w),mm_char,wparam,lparam,0)

	when wm_close then
		if w==wapplic then
			postmess(w,mm_close,0,0,0)
			return 0
		else
			postmess(w,mm_cancel,0,0,0)
			return 0
		fi

	when wm_timer then
		if not background and not stationary then		!test for pausing of mouse
			if gettickcount()-lastxytime>pausetime then
				stationary:=1
			fi
		fi

	when wm_destroy then
		if w and wapplic and w==wapplic then
			killtimer(hwnd,1)
!*		if tick then killtimer(hwnd,1) fi
!CPL "**********************POSTING QUIT WM",WAPPLIC


!			postquitmessage(0)			!mm_quit message
			return 0
		else
			return 1
		fi

!when wm_setcursor then
!	postmess(w,mm_setcursor,wparam,lparam,0)

	when wm_mousemove then

		buttonstate:=wparam iand (kb_lbutton ior kb_rbutton ior kb_mbutton)
		mousepos.x:=lparam iand 65535
		mousepos.y:=lparam>>16

	domousemove:
		xyvalid:=1				!known again
		setnewmousewindow(w)

		wmouse:=w
		postmess(wmouse,mm_move)

		lastxy::=getscreencoords(wmouse,mousepos)
		lastxytime:=gettickcount()
		stationary:=0

!do drag processing; states are:
!pen up/recent pen down/first drag/subsequent drag
!any drag messages are sent as well as mm_move messages
!dragmode=1/2/3 indicates drag has started (reset by buttonswitching)

		if buttonstate<>0 and lastmousewindow<>nil then		!switch pressed
			pt:=getscreencoords(lastmousewindow,lastmousepos)
			dx:=lastxy.x-pt.x
			dy:=lastxy.y-pt.y

			if dragmode then		!1st drag message already generated
				postmess(lastmousewindow,mm_drag,dx,dy,-1)			!send latest drag coords

			else				!test for drag enabling
				if ((mousesw=1 and lastmousewindow.flags.[wa_leftdrag]<>0) or \
								(mousesw=2 and lastmousewindow.flags.[wa_rightdrag]<>0) or \
								(mousesw=3 and lastmousewindow.flags.[wa_middledrag]<>0)) and \
							(abs(dx)>dragtol or abs(dy)>dragtol) then
					dragmode:=mousesw
					postmess(lastmousewindow,mm_startdrag,dx,dy,-1)		!send latest drag coords
				fi

			fi
		else
			if dragmode then
				postmess(lastmousewindow,mm_enddrag,dx,dy,-1)	!send latest drag coords
				dragmode:=0
			fi
		fi

		return 0

	when wm_enteridle then		!enter idle
		idlemode:=1
		return 0

	when wm_paint then

		if w<>nil then
			ps:=new(ws_paintstruct)
			rect:=new(ws_rect)
			beginpaint(hwnd,&ps)
			postmess(w,mm_restore,0,0,0)
			endpaint(hwnd,&ps)
			return 0
		fi

	when wm_erasebkgnd then

	when wm_move then
		if w<>nil then
!*!		gxmovewindow(w,lparam iand 65535,lparam>>16)
		fi

	when wm_size then
		x:=lparam iand 0xffff
		y:=lparam>>16
		if w<>nil  and (w.dimx<>x or w.dimy<>y) then
!*!		gxmplresize(w,x,y,wparam)
			return 0
		fi

!when wm_killfocus,wm_setfocus then

	when wm_contextmenu then
		sendmess(w,mm_rclick,wparam>>16,wparam iand 0xffff,0)
		return 0

	when wm_mousewheel then
		if not wmouse then wmouse:=w fi
		postmess(wmouse,mm_wheel,int(wparam>>16),wparam iand 0xffff,0)
		return 0

	when wm_nclbuttondown,wm_nclbuttondblclick then

	when wm_activateapp then
		if wparam then
			postmess(w,mm_activate,1,0,0)
		fi

	else
	btnmessages:
!check for sequential messages
!CPL "FALLTHROUGH"
		if message>=wm_lbuttondown and message<=wm_mbuttondblclk then
			buttonmessages(hwnd,message,wparam,lparam)
			return 0
		fi
	esac
!end
!fall-through here to do default message processing instead of/in addition to local processing
	return 1	!defwindowproc(hwnd,imsg,wparam,lparam)
end

export proc mxinit=
	wmessagetable := [\
		wm_lbuttondown:		mm_click,
		wm_lbuttonup:		mm_clickup,
		wm_lbuttondblclk:	mm_dblclick,

		wm_rbuttondown:		mm_rclick,
		wm_rbuttonup:		mm_rclickup,
		wm_rbuttondblclk:	mm_rdblclick,

		wm_mbuttondown:		mm_mclick,
		wm_mbuttonup:		mm_mclickup,
		wm_mbuttondblclk:	mm_mdblclick]

!table gives button number 1,2,3 for Windows button message (always 0 for button up)
	buttontable	:= [\
		wm_lbuttondown:		1,
		wm_lbuttonup:		0,
		wm_lbuttondblclk:	1,

		wm_rbuttondown:		2,
		wm_rbuttonup:		0,
		wm_rbuttondblclk:	2,

		wm_mbuttondown:		3,
		wm_mbuttonup:		0,
		wm_mbuttondblclk:	3]

	mousepos:=new(ws_point)

	setmesshandler(process_wmmessage)
!CPL =PROCESS_WMMESSAGE
!setmesshandler(bill)

	vktomesstable:=[\
		vkleft:		mm_leftkey,
		vkright:	mm_rightkey,
		vkup:		mm_upkey,
		vkdown:		mm_downkey,
		vkpageup:	mm_pageupkey,
		vkpagedown:	mm_pagedownkey,
		vkhome:		mm_homekey,
		vkend:		mm_endkey,
		vktab:		mm_tabkey,
		vkbackspace:	mm_bskey,
		vkdelete:	mm_deletekey,
		vkenter:	mm_enterkey,
		vkinsert:	mm_insertkey,
		vkescape:	mm_cancel
	]
end

export func postmess(w,mess,a=0,b=0,c=0)=
!add message m to end of message queue
!use mess+1000 to add message to start of queue rather than the end

	if w=nil then w:=wapplic fi
	if w=nil then
 return 0 fi

	if w.flags.[wa_closed] then

 return 0 fi

	if mess>=1000 then
		headx:=1; mess-:=1000
	else
		headx:=0
	fi

!check if new message can be combined with an old message
	case mess
	when mm_sethozpos,mm_setvertpos,mm_draw,mm_restore,mm_update then
		for i:=1 to nmessages do
			m:=messagequeue[i].message
			if m=mess and w==messagequeue[i].wind then				!use the old message but update any params
				messagequeue[i].a:=a
				messagequeue[i].b:=b
				return 0
			elsif mess=mm_draw and m=mm_update then		!convert update to draw
				messagequeue[i].message:=mm_draw
				return 0
			fi
		od
	esac

	if quitmess or nmessages>=maxqueuesize then
		return 0
	fi

	postmsg(makemess(w,mess,a,b,c))

	return 0					!return zero for use in mainwndproc
end

export func postmsg(msg,headx=0)=
!add complete message msg to end of message queue
!use head=1 to add to start of queue rather than the end

	if quitmess or nmessages>=maxqueuesize then
		return 0
	fi

	if msg.wind.flags.[wa_closed] then return 0 fi

	if headx then

!avoid dupl paint messages
		++nmessages
		for i:=nmessages downto 2 do
			messagequeue[i]:=messagequeue[i-1]
		od
		messagequeue[1]:=msg

	else
		++nmessages
		messagequeue[nmessages]:=msg
	fi

	return 0					!return zero for use in mainwndproc
end

export proc sendmess(w,mess,a=0,b=0,c=0)=
!add message m to head of message queue
!(may be 100% handled in q smlib)

	if w=nil then return fi
	if w.flags.[wa_closed] then return fi

	sendmsg(makemess(w,mess,a,b,c))
end

proc sendmsg(msg)=
!call event handler for msg or add to head of queue
	if msg.wind.flags.[wa_closed] then return fi
	postmsg(msg,1)
end

export func makemess(w,mess,a=0,b=0,state=-1)=
!turn params into a new messrec @nemm_ess
!the q version makemess also accepts makemess(w,msg)

	if w=nil then w:=wapplic fi

	m:=new(rmessage,0)

	m.wind:=w

	m.message:=mess
	m.a:=a
	m.b:=b
	m.state:=state

	m.x:=mousepos.x
	m.y:=mousepos.y

	if m.state=-1 then m.state:=getshiftstate() fi

	return m
end

func dokeymessage(hwnd,msg,wparam,lparam)=
!return 1 if message has been dealt with

	case msg
	when wm_syskeydown then
		if wparam=vkf10 then msg:=wm_keydown; goto dokey fi

	when wm_syskeyup then
		if wparam=vkf10 then msg:=wm_keyup; goto dokey fi

	when wm_keydown,wm_keyup then
	dokey:
		case wparam
		when vkshift,vkctrl,vkalt,vkcapslock then
		else
			w:=wfocus
!		if not w then w:=wx_getw(hwnd) fi
!CPL "XXXXX"
			if not w then w:=getwindow(hwnd)
!CPL "AFTER GW3"
 fi
!CPL =GETSHIFTSTATE()
!		postmess(w,(msg=wm_keydown|mm_key|mm_keyup),wparam,getshiftstate(),lparam)
			postmess(w,(msg=wm_keydown|mm_key|mm_keyup),wparam,lparam,-1)
			return 1
		esac
	esac
	return 0
end

func getshiftstate=
	state:=0

	if getkeystate(vklshift) iand 0x8000 then state ior:=kb_shift fi
	if getkeystate(vklcontrol) iand 0x8000 then state ior:=kb_ctrl fi
	if getkeystate(vklalt) iand 0x8000 then state ior:=kb_alt fi

	if getkeystate(vkrshift) iand 0x8000 then state ior:=kb_rshift fi
	if getkeystate(vkrcontrol) iand 0x8000 then state ior:=kb_rctrl fi
	if getkeystate(vkralt) iand 0x8000 then
		state ior:=kb_ralt
		state iand:=(inot kb_ctrl)			!AltGr gives Lctrl+Ralt; return Ralt only
	fi
	if getkeystate(vkcapslock) iand 1 then state ior:=kb_capslock fi

	return state ior buttonstate
end

proc buttonmessages(hwnd,msg,wp,lp)=
!process Windows mouse message <msg>

!update button from wparam, excluding ctrl/shift (which are updated from key msgs)
	buttonstate:=wp iand (kb_lbutton ior kb_rbutton ior kb_mbutton)

!update mouse position
	mousepos.x:=lp iand 0xffff
	mousepos.y:=int(lp)>>16
	wmouse:=getwindow(hwnd)
!CPL "AFTER GW4"

!set mousesw to last pressed button (1,2,3) or 0 if one just released
!(note other buttons may still be down, used for drag processing)
	mousesw:=buttontable{msg}

	if mousesw then			!down up on click or dblclick
		lastbuttontime:=gettickcount()
		lastmousepos::=mousepos
		lastmousewindow:=wmouse
	else
		mousesw:=0

		if dragmode then
			postmess(lastmousewindow,mm_enddrag,0,0,-1)
			dragmode:=0
		fi

		lastbuttontime:=0
		lastmousewindow:=nil
	fi

	newmess:=wmessagetable{msg}

!filter double-click messages and convert to repeated click if not enabled
	case newmess
	when mm_dblclick then unless wmouse.flags.[wa_leftdbl] then newmess:=mm_click end
	when mm_rdblclick then unless wmouse.flags.[wa_rightdbl] then newmess:=mm_click end
	esac

	postmess(wmouse,newmess,wmouse.id,0,-1)
end

proc setnewmousewindow(w)=
	return when not currmousewindow
	unless w==currmousewindow then		!changed
		if currmousewindow<>nil then
			postmess(currmousewindow,mm_offwindow,0,0,0)
		fi

		currmousewindow:=w
		postmess(w,mm_onwindow,0,0,0)
	end unless
end

proc frame2rect(f,r)=
	r^.x:=f^.x
	r^.y:=f^.y

	r^.dimx:=f^.x2-f^.x1+1
	r^.dimy:=f^.y2-f^.y1+1
end

export func mxwait_mm_message=
	#do windows dispatch loop
	#calling dispatchmessage() results in mainwndproc being called in interpreter,
	#which passes the Windows message params on to process_wmmessage() in this module
	#process_wmmessage() converts wm-messages to mpl mm-messages
	#return when at least one mm message is ready; (will return immediately if there
	#is already one in the queue)
	#return value is normall 1, or 0 when quitmess has been encountered

	if quitmess then				!quit message already seen
CPL "-----------------QUITMESS SEEN"
		return 0
	fi

	windmsg:=new((iswin32|ws_msg32|ws_msg64))

	while nmessages<=0 do
		if x:=getmessage(&windmsg,nil,0,0)<>0 then
			w:=windmsg.hwnd
			translatemessage(&windmsg)
			dispatchmessage(&windmsg)
		else
CPL "----GETMESSAGE RETURMS 0"
			quitmess:=1
			exit
		fi
	od

	if not nmessages then			!assume quit message seen
CPL "----------NO MESSAGES"
		return 0
	fi	

	currmess:=messagequeue[1]
	--nmessages

	xlatkeyboard()

	for i:=1 to nmessages do
		messagequeue[i]:=messagequeue[i+1]
	od
	return 1
end

proc xlatkeyboard=
!expand any mm_key messages to special key messages
!uses and modified currmess
	m:=currmess.message

	if m=mm_key then

		k:=currmess.a
		if k>=vkf1 and k<=vkf12 then
			newmsg:=currmess
			currmess.message:=mm_functionkey
			currmess.a:=k-vkf1+1
		else
			keymess:=vktomesstable{k,0}
			if keymess then
				currmess.message:=keymess
			fi
		fi
	fi
end

func getscreencoords(w,pos)=
	pt::=pos
	if not w then
		PCERROR("GSC/W=0")
	fi

	clienttoscreen(w.gdi.hwnd,&pt)		!pos starts at 0,0
	return pt
end

export func getwindow(hwnd)=
!convert hwnd to window
!return nil if any problem
	if hwnd=0 then
		return nil
	fi

	index:=wx_getw(hwnd)
	if index then
		return allwindows[index]
	fi
	return nil
end

proc initmenuhandlers=
	ltcolour:=getrgb(ltgrey)
	dkcolour:=getrgb(dkgrey)
end

proc gxhandler_fixups=
!do some manual fixups for various shared handlers
!(the automatic fixup routine allows multiple window classes per message, but not
! multiple message per window class)
	messhandlertable[mm_startdrag,scrollbar_class]:=mess_drag_scrollbar
	messhandlertable[mm_enddrag,scrollbar_class]:=mess_drag_scrollbar
	messhandlertable[mm_leftkey,scrollbar_class]:=mess_upkey_scrollbar
end

func mess_all_all(mess,w)=
!CPL "MESSAA1"
	case mess.message
	when mm_startdrag,mm_drag,mm_enddrag then
	when mm_command then
	when mm_ok,mm_cancel then
	when mm_click then
		case w.windclass
		when label_class, group_class then
			return skipmess
		esac
	when mm_key then
	when mm_sethozpos,mm_setvertpos then
	when mm_pick,mm_lbchange then
	when mm_leftkey,mm_rightkey,mm_upkey,mm_downkey,mm_enterkey,mm_tabkey then
	when mm_pageupkey,mm_pagedownkey then
	when mm_homekey, mm_endkey then
	when mm_functionkey then
	when mm_wheel then
	when mm_edited then
	else
!CPL "MESSAA2"
		return skipmess
	esac
!CPL "MESSAA3"

	return thismess
end

func mess_restore_all(mess,w)=
	gxrestore(W)

	return skipmess
end

func mess_killfocus_all(mess,w)=
!note: can be called from mess_setfocus_all, with a different mess, but correct w
!assume w is same as wfocus

	drawcaret(0)
	wfocus:=nil

	return skipmess
end

func mess_setfocus_all(mess,w)=
	if wfocus then
		mess_killfocus_all(mess,wfocus)
	fi

	wfocus:=w
	drawcaret(1)
	return skipmess
end

func mess_update_all(mess,w)=
	gxupdate(w)
	return skipmess
end

func mess_draw_all(mess,w)=
	gxdraw(w)
	return skipmess
end

func mess_click_select(mess,w)=
	if w.enable then
		if not w.style.noupdate then
			p:=w.linkvar
			p^:=w.attrs.onvalue
			for wc in w.owner.childlist do
				if wc.windclass=select_class and wc.linkvar=p then
					gxdraw(wc)
				fi
			od
		fi
		if w.style.returnmess then
			postmess(w,mm_command,w.id)
		fi
	fi
	return skipmess
end

func mess_click_toggle(mess,w)=
	if w.enable then
		if not w.style.noupdate then
			w.linkvar^:=not w.linkvar^
			gxdraw(w)
		fi
		if w.style.returnmess then
			postmess(w,mm_command,w.id)
		fi
	fi
	return skipmess
end

func mess_click_button(mess,w)=

	if w.enable=0 then
		beep1()
		return skipmess
	fi

	if w.id in 0..199 then				!speficies an actual message number (but no params)
		postmess(w,w.id)
	else
		postmess(w,mm_command,w.id)
	fi
	return skipmess
end

func mess_click_editbox(mess,w)=
	if w.enable then
		if not w.style.noupdate then
			unless w==wfocus then
				gxfocus(w)
			end
		fi
		if w.style.returnmess then
			postmess(w,mm_command,w.id)
		fi
	fi

	return skipmess
end

func mess_click_arrow(mess,w)=

	case w.owner.windclass
	when scrollbar_class then
		postmess(w.owner,dirtomess{w.style.dir},w.id,0,-1)
	else
		mess.message:=dirtomess{w.style.dir}
		mess.a:=w.id
		return thismess
	esac
	return skipmess
end

func mess_click_mark(mess,w)=

	case w.owner.windclass
	when toggle_class,select_class then
		postmess(w.owner,mess.message,w.id,0,-1)
	esac
	return skipmess
end

func mess_click_listbox(mess,w)=
	gxfocus(w)

	y:=max(w.attrs.offset,mess.y)

	pos:=(y-w.attrs.offset)%w.attrs.pitch+w.attrs.pagepos
	if pos<=getlvbounds(w.linkvar).len then
		gxsetlbpos(w,pos)
		postmess(w,mm_pick,pos)
	fi

	return skipmess
end

func mess_click_scrollbar(mess,w)=
	onthumb:=isonthumb(w,(w.style.dir='H'|mess.x|mess.y))
	step:=w.attrs.span
	a:=w.attrs.currpos

	case w.owner.windclass
	when listbox_class then
		case onthumb
		when -1 then
			if a>w.attrs.limits.lwb then
				a:=max(a-step,w.attrs.limits.lwb)
				gxsetlbpage(w.owner,a)
			fi
		when 1 then
			if a<w.attrs.limits.upb then
				a:=min(a+step,w.attrs.limits.upb)
				gxsetlbpage(w.owner,a)
			fi
		esac
	else
		if not step then step:=10 fi

		case onthumb
		when -1 then
			if a>w.attrs.limits.lwb then
				a:=max(a-step,w.attrs.limits.lwb)
				gxscrollpos(w,a,1)
				postmess(w,mm_sethozpos,a)
			fi
		when 1 then
			if a<w.attrs.limits.upb then
				a:=min(a+step,w.attrs.limits.upb)
				gxscrollpos(w,a,1)
				postmess(w,mm_sethozpos,a)
			fi
		esac
	esac
	return skipmess
end

func mess_wheel_scrollbar(mess,w)=
	delta:=currmess.a
	n:=abs(currmess.a%120)
	to n do
		case w.windclass
		when scrollbar_class then
	doscroll:
			postmess(w,(delta>0|mm_up|mm_down))
		when listbox_class then
			if w.childlist then
				w:=w.childlist[1]
				goto doscroll
			fi
			postmess(w,(delta>0|mm_upkey|mm_downkey))
		esac
	od
	return skipmess
end

func mess_up_scrollbar(mess,w)=
	a:=w.attrs.currpos
	if a<=w.attrs.limits.lwb then
		return skipmess
	fi
	case w.owner.windclass
	when listbox_class then
		gxsetlbpage(w.owner,a-1)
		return skipmess
	else
		--a
		gxscrollpos(w,a,1)
		postmess(w,mm_setvertpos,a)
	esac
	return skipmess
end

func mess_left_scrollbar(mess,w)=

	case w.owner.windclass
	when listbox_class then
		return skipmess
	else
		a:=w.attrs.currpos
		if a>w.attrs.limits.lwb then
			--a
			gxscrollpos(w,a,1)
			postmess(w,mm_sethozpos,a)
		fi
	esac
	return skipmess
end

func mess_right_scrollbar(mess,w)=

	case w.owner.windclass
	when listbox_class then
		return skipmess
	else
		a:=w.attrs.currpos
		if a<w.attrs.limits.upb then
			++a
			gxscrollpos(w,a,1)
			postmess(w,mm_sethozpos,a)
		fi

	esac
	return skipmess
end

func mess_down_scrollbar(mess,w)=

	a:=w.attrs.currpos
	if a>=w.attrs.limits.upb then
		return thismess
	fi
	case w.owner.windclass
	when listbox_class then
		gxsetlbpage(w.owner,a+1)
		return skipmess
	else
		++a
		gxscrollpos(w,a,1)
		postmess(w,mm_setvertpos,a)

	esac
	return skipmess
end

func mess_drag_scrollbar(mess,w)=
	case mess.message
	when mm_startdrag then
		if isonthumb(w,(w.style.dir='H'|mess.x|mess.y))=0 then
			thumbdragmode:=1			!then treat as mm_drag
			thumbstartpos:=w.attrs.thumbpos-arrowdim		!use thumb pos at start of drag
		else							!dragging other part of scrollbar
			return skipmess
		fi
	when mm_enddrag then
		thumbdragmode:=0
		return skipmess
	elsif not thumbdragmode then
		return skipmess
	esac

	offset:=(w.style.dir='H'|mess.a|mess.b)		!pixel offset from initial drag start pos
	newpos:=thumbstartpos+offset						!could outside thumb span range

	pos:=int(round((newpos/w.attrs.thumbspan)*(w.attrs.limits.len-1)+w.attrs.limits.lwb))
	pos:=clamp(pos,w.attrs.limits.lwb,w.attrs.limits.upb)

	case w.owner.windclass
	when listbox_class then
		gxsetlbpage(w.owner,pos)
	else
		gxscrollpos(w,pos,1)
		postmess(w,(w.style.dir='H'|mm_sethozpos|mm_setvertpos),pos)
	esac
	return skipmess
end

func mess_move_button(mess,w)=
	return skipmess
end

func mess_move_all(mess,w)=
	return skipmess
end

func mess_char_editbox(mess,w)=
!	SHOWMESSAGE("CHAREDIT",MESS)

		if mess.a not in 32..255 then
!	cpl "CHAR/EDIT2",MESS.A,VKENTER
			case mess.a
			when vkenter then
				CPL "ENTER"
				m:=mm_edited
			else
				m:=mm_key
			esac

			if wapplic then
!	CPL "CHAR/EDIT CONTROL",=MESS.A,MESS.B
				postmess(wapplic,m, mess.a,mess.b,mess.state)
			fi
			return skipmess
		fi
		if not w.enable or w.style.noupdate then return skipmess fi
		s:=w.linkvar^
		n:=w.attrs.currpos
		c:=chr(mess.a)

		if n>s.len then				!at end
			s+:=c
		elsif n=1 then				!at start
			s:=c+s
		else						!in middle
			s:=leftstr(s,n-1)+c+rightstr(s,-(n-1))
		fi

		w.linkvar^:=s
		++w.attrs.currpos
		gxdraw(w)

		return skipmess
end

func mess_key_editbox(mess,w)=
!	CPL "KEY/EDITBOX"

	postmess(wapplic,mm_key,mess.a,mess.b,mess.state)

	return skipmess
end

func mess_leftkey_editbox(mess,w)=
	if ctrlpressed() then
		postmess(wapplic,mm_leftkey,mess.a,mess.b,mess.state)
		return skipmess
	fi

	if w.attrs.currpos>1 then
		drawcaret(0)
		--w.attrs.currpos
		drawcaret(1)
	fi
	return skipmess
end

func mess_rightkey_editbox(mess,w)=
	if ctrlpressed() then
		postmess(wapplic,mm_rightkey,mess.a,mess.b,mess.state)
		return skipmess
	fi

	if w.attrs.currpos<=w.linkvar^.len then
		drawcaret(0)
		++w.attrs.currpos
		drawcaret(1)
	fi
	return skipmess
end

func mess_bskey_editbox(mess,w)=
	s:=w.linkvar^
	if not s then return skipmess fi
	n:=w.attrs.currpos
	if n=1 then return skipmess fi

	if n>s.len then				!at end
		s:=leftstr(s,-1)
	else						!in middle
		s:=leftstr(s,n-2)+rightstr(s,-(n-1))
	fi

	w.linkvar^:=s
	--w.attrs.currpos
	gxdraw(w)

	return skipmess
end

func mess_deletekey_editbox(mess,w)=
	s:=w.linkvar^
	if not s then return skipmess fi
	n:=w.attrs.currpos
	if n>s.len then return skipmess fi

	if n=1 then				!at start
		s:=rightstr(s,-1)
	else						!in middle
		s:=leftstr(s,n-1)+rightstr(s,-n)
	fi
	w.linkvar^:=s
	gxdraw(w)

	return skipmess
end

func mess_homekey_editbox(mess,w)=
	if ctrlpressed() then
		postmess(wapplic,mm_homekey,mess.a,mess.b,mess.state)
		return skipmess
	fi

	drawcaret(0)
	w.attrs.currpos:=1
	drawcaret(1)

	return skipmess
end

func mess_homekey_listbox(mess,w)=
	if w.attrs.currpos>1 then
		gxsetlbpos(w,1)
	fi

	return skipmess
end

func mess_endkey_editbox(mess,w)=
	if ctrlpressed() then
		postmess(wapplic,mm_endkey,mess.a,mess.b,mess.state)
		return skipmess
	fi

	drawcaret(0)
	w.attrs.currpos:=w.linkvar^.len+1
	drawcaret(1)

	return skipmess
end

func mess_endkey_listbox(mess,w)=
!if w.attrs.currpos<w.linkvar^.len then
	if w.attrs.currpos<getlvbounds(w.linkvar).len then
!	gxsetlbpos(w,w.linkvar^.len)
		gxsetlbpos(w,getlvbounds(w.linkvar).len)
	fi

	return skipmess
end

func mess_upkey_listbox(mess,w)=
	if w.attrs.currpos>1 then
		gxsetlbpos(w,w.attrs.currpos-1)
	fi

	return skipmess
end

func mess_upkey_scrollbar(mess,w)=
!assume that this is independent scrollbar
!(linked scrollbar wouldn't get the focus)

	a:=w.attrs.currpos
	if a>w.attrs.limits.lwb then
		--a
		gxscrollpos(w,a,1)
		postmess(w,mm_setvertpos,a)
	fi
	return skipmess
end

func mess_downkey_listbox(mess,w)=
!if w.attrs.currpos<w.linkvar^.len then
	if w.attrs.currpos<getlvbounds(w.linkvar).len then
		gxsetlbpos(w,w.attrs.currpos+1)
	fi

	return skipmess
end

func mess_pageupkey_listbox(mess,w)=
	if (a:=w.attrs.currpos)>1 then
		a:=max(a-w.attrs.rows,1)
		gxsetlbpos(w,a)
	fi

	return skipmess
end

func mess_pagedownkey_listbox(mess,w)=
	if (a:=w.attrs.currpos)<getlvbounds(w.linkvar).len then
		a:=min(a+w.attrs.rows,getlvbounds(w.linkvar).len)
		gxsetlbpos(w,a)
	fi

	return skipmess
end

func mess_enterkey_listbox(mess,w)=
	if w.attrs.currpos then
		postmess(w,mm_pick,w.attrs.currpos)
	fi

	return skipmess
end

proc do_draw_all(w)=
	gxclear(w)
	drawborder(w)
	drawchildborders(w)
end

proc do_draw_button(w)=
	gxclear(w)

	gxtext_just(w,w.text,0,w.enable)

	drawborder(w)

end

proc do_draw_label(w)=
	do_draw_button(w)
end

proc do_draw_toggle(w)=
	gxclear(w)

	VALSTR:=""

	turnedon:=istrue w.linkvar^

	if w.style.marktype then
		drawmark(w.childlist[1],turnedon,w.enable)

		gxtext_just(w,w.text+valstr,markdim,w.enable)
	else
!	case w.style.hilitetype
!	when invert_hilite then
			if turnedon then
				gxclear(w,getrgb(green))
			fi
!	esac

		gxtext_just(w,w.text+valstr)
	fi
end

proc do_draw_select(w)=
	gxclear(w)

	turnedon:=w.linkvar^=w.attrs.onvalue

	if w.style.marktype then
		drawmark(w.childlist[1],turnedon,w.enable)
		gxtext_just(w,w.text,markdim,w.enable)
	else
		case w.style.hilitetype
		when invert_hilite then
			if turnedon then
				gxclear(w,getrgb(white))
			fi
		esac
			gxtext_just(w,w.text)
	fi
end

proc do_draw_editbox(w)=
	gxclear(w)

	gxtext_just(w,w.linkvar^,enable:w.enable)

!Now, have to draw the cursor
	unless wfocus==w then			!only draw it when this window has the focus
		return
	end

	caretdrawn:=0

	drawcaret(1)
end

proc do_draw_arrow(w)=
	gxclear(w)

	drawborder(w)
	drawarrow(w,w.enable)
end

proc do_draw_mark(w)=

	case w.owner.windclass
	when toggle_class, select_class then
		return					!mark drawn by owner
	esac

	gxclear(w,getrgb(w.owner.style.windbgnd))

	drawborder(w)
end

proc do_draw_scrollbar(w)=
	gxclear(w)
	drawborder(w)
	gxdraw(w.childlist[1])			!arrows
	gxdraw(w.childlist[2])

!now draw the thumb
	if w.attrs.thumbsize then
		if w.style.dir='H' then
			x:=w.attrs.thumbpos
			dx:=w.attrs.thumbsize
			drawthumb(w,x,0,dx,w.dimy)
		else
			y:=w.attrs.thumbpos
			dy:=w.attrs.thumbsize
			drawthumb(w,0,y,w.dimx,dy)
		fi
	fi
end

proc do_draw_listbox(w)=
	gxclear(w)
	drawborder(w)
	if w.childlist then			!scrollbar
		gxdraw(w.childlist[1])
	fi

	for i:=1 to w.attrs.rows do
		k:=i+w.attrs.pagepos-1
		if k<=getlvbounds(w.linkvar).len then
			drawlbtext(w,i,getlvstritem(w.linkvar,k),0,k=w.attrs.currpos)
		fi
	od
end

proc do_update_all(w)=
	gxdraw(w)
end

proc do_update_listbox(w)=
	gxdraw(w)
end

proc drawcaret(x)=
!x=1: draw caret in wfocus window at current position
!x=0: delete caret in wsfocus window
!returns x-pixel position of caret

	if wfocus=nil then		!no window has focus
		caretdrawn:=0
		return
	fi

	case wfocus.windclass
	when editbox_class then
		if x then			!new caret
			if caretdrawn then return fi	!already drawn
			xpos:=getcaretpos(wfocus.linkvar^,wfocus.attrs.currpos,0)
			wfocus.attrs.caretpos:=xpos			!record position
		else			!delete caret
			if not caretdrawn then return fi	!already deleted
			xpos:=wfocus.attrs.caretpos		!use stored value
		fi

		caretwidth:=2

		gxhighlight(wfocus,xpos+wfocus.attrs.textpos[1],wfocus.attrs.textpos[2]-chd,caretwidth,20)

		caretdrawn:=x
	esac
end

func getcaretpos(s,pos,offset)=
!return pixel position of in front of pos'th character in string s
!offset is no. of chars not shown, to left of string
	if pos=1 then return 0 fi

	return wx_gettextwidth(wfocus.gdi.hdc, leftstr(s,pos-1))
end

proc drawborder(w)=
!do own-drawn borders
!other kinds of borders are windows-drawn, no-border, and the main bs- style
!borders which exist in the owner's client area
!for own-drawn borders, the window should have been cleared first

	case bscat[w.style.border]
	when 0 then					!no border
		return
	when 'W' then				!windows-drawn
		return
	when 'X' then				!external (drawn in owner's client space
		posx:=w.frameposx
		posy:=w.frameposy
		dimx:=w.framedimx
		dimy:=w.framedimy
		bs:=w.style.border

		bs:=w.style.border
		wo:=w.owner
		case bs
		when bs_simple then			!USUALLY BS_SIMPLE converts to BS_WINDOWS; must be override
			gxcolour(wo,0)
			gxrect(wo,posx,posy,dimx,dimy)
		when bs_thick then
		when bs_panel then
			gxcolour(wo,ltcolour)
			gxline(wo,posx+dimx-1,posy, posx,posy)
			gxline(wo,posx,posy+dimy-1)
			gxcolour(wo,dkcolour)
			gxline(wo,posx+dimx-1,posy+dimy-1)
			gxline(wo,posx+dimx-1,posy)
		when bs_inset then
			gxcolour(wo,dkcolour)
			gxline(wo,posx+dimx-1,posy, posx,posy)
			gxline(wo,posx,posy+dimy-1)
			gxcolour(wo,ltcolour)
			gxline(wo,posx+dimx-1,posy+dimy-1)
			gxline(wo,posx+dimx-1,posy)
		when bs_testext then
			gxcolour(wo,0)
			gxrect(wo,posx,posy,dimx,dimy)
			gxrect(wo,posx+9,posy+9,dimx-18,dimy-18)
		esac
	when 'I' then				!internal (drawn within window's client space
		posx:=w.frameposx
		posy:=w.frameposy
		dimx:=w.dimx
		dimy:=w.dimy

		case w.style.border
		when bs_ownpanel then
			gxcolour(w,ltcolour)
			gxline(w,w.framedimx-1,0,0,0)
			gxline(w,0,w.framedimy-1)
			gxcolour(w,dkcolour)
			gxline(w,w.framedimx-1,w.framedimy-1)
			gxline(w,w.framedimx-1,0)

		when bs_owninset then
			gxcolour(w,dkcolour)
			gxline(w,w.framedimx-1,0,0,0)
			gxline(w,0,w.framedimy-1)
			gxcolour(w,ltcolour)
			gxline(w,w.framedimx-1,w.framedimy-1)
			gxline(w,w.framedimx-1,0)
		when bs_ownsimple then
			gxcolour(w,0)
			gxrect(w,0,0,w.framedimx,w.framedimy)
		when bs_testint then
			gxcolour(w,0)
			gxrect(w,0,0,dimx,dimy)
			gxrect(w,7,7,dimx-14,dimy-14)
		esac
	esac
end

proc drawchildborders(w)=
	if not w.childlist then
		return
	fi
	for wc in w.childlist do
		if wc.style.border in [bs_simple,bs_thick,bs_panel,bs_inset] then
			drawborder(wc)
		fi
	od
end

proc drawarrow(w,enable)=
!w has already been cleared
!e=1/omitted to enable, 0 to disable (shown grey)
	const factor=0.3

	gxsetpen(w,(enable|black|dkgrey))

	width:=w.dimx
	height:=w.dimy

	case w.style.dir
	when 'D' then
		x:=int(round(width/2)-1)

		wd:=0

		h:=int(round(min(height,width)*factor))
		if h<3 then h:=3 fi
		y:=int((height+h)*0.5)-1

		to h do
			gxline(w,x,y,x+wd,y)
			x-:=1
			y-:=1
			wd+:=2
		od

	when 'U' then
		x:=int(round(width/2)-1)
		wd:=0

		h:=int(round(min(height,width)*factor))
		if h<3 then h:=3 fi
		y:=int(round((height-h)*0.5))
		to h do
			gxline(w,x,y,x+wd,y)
			x-:=1
			y+:=1
			wd+:=2
		od

	when 'L' then
		y:=height%2

		ht:=0
		wd:=y

		wd:=int(round(min(height,width)*factor))
		if wd<3 then wd:=3 fi
		x:=int(round((width-wd)*0.5)-1)

		to wd do
			gxline(w,x,y,x,y+ht)
			y-:=1
			x+:=1
			ht+:=2
		od

	when 'R' then
		y:=height%2
		ht:=0

		wd:=int(round(min(height,width)*factor))
		if wd<3 then wd:=3 fi
		x:=int(round((width+wd)*0.5)-1)

		to wd do
			gxline(w,x,y,x,y+ht)
			y-:=1
			x-:=1
			ht+:=2
		od
	esac
end

export proc gxtext_just(w,s,offset=0,enable=1)=
		dimx:=w.dimx
		dimy:=w.dimy
		width:=wx_gettextwidth(w.gdi.hdc, s)
		height:=chy				!assume basic font

		case w.style.justify
		when 'L' then	x:=smx
		when 'R' then	x:=dimx-width-smx
		else
					x:=(dimx-width)%2
		esac

		case w.style.vjustify
		when 'T' then	y:=smy
		when 'B' then	y:=dimy-height-smy
		else
					y:=(dimy-height)%2!		-smy%2
		esac

		if not enable then
			oldtextfgnd:=w.style.textfgnd
			gxtextcolour(w,grey)
		fi

		gxtext(w,s,x+offset,y)

		if not enable then
			gxtextcolour(w,oldtextfgnd)
		fi
		if w.windclass=editbox_class then
			w.attrs.textpos:=(x+offset,y)
		fi
end

proc drawthumb(w,x,y,dx,dy)=
!w is a scrollbar, vert or hoz
!draw thumb within w, as a simple rectangle starting at x,y at top left of size dx,dy

	gxcolour(w,0)
	gxrect(w,x,y,dx,dy)
	gxfillrect(w,x+1,y+1,dx-2,dy-2,getrgb(grey))
end

func isonthumb(w,d)=
!w is a scrollbar, d is a pixel position along it (0 being at left or top)
!return:
! -1	is before the thumb
!  0	is on the thumb
! +1	if after the thumb
!d can specify a spot off the thumbar if being dragged

	a:=w.attrs.thumbpos
	b:=w.attrs.thumbsize

	if d<a then
		return -1
	elsif d>(a+b) then
		return 1
	else
		return 0
	fi
end

proc drawmark(w,turnedon,enable)=
!!w has already been cleared
!draw checked check mark, tick, or radio button, according to whether
!turnedon is 1 or 0
!e=1 to enable, 0 to disable (shown grey)

	gxclear(w,getrgb(w.owner.style.windbgnd))
	gxsetpen(w,(enable|black|red))

	width:=w.dimx
	height:=w.dimy
	x:=y:=1
	wd:=width-2
	ht:=height-2
	gxrect(w,x,y,wd,ht)
	if not turnedon then return fi

	case w.style.marktype
	when radio_mark then

		gxfillrect(w,x+3,y+3,wd-6,ht-6,getrgb(red))

	when check_mark then

		gxline(w,x,y,x+wd-1,y+ht-1)
		gxline(w,x+wd-1,y,x,y+ht-1)

	when tick_mark then

		gxline(w,x+3,y+ht%2,x+wd%2,y+ht-4)
		gxline(w,x+wd-3,y+2)

	esac
end

proc drawlbtext(w,row,text,clr=0,hilite=0)=
!draw text inside given row of listbox w
!clr=1 to clear the background first (not needed when entired lb has been cleared)
!hilite=1 to highlight this row

	x:=0
	y:=(row-1)*w.attrs.pitch+w.attrs.offset

	if clr or hilite then
		gxfillrect(w,x,y,w.dimx,w.attrs.pitch,(hilite|getrgb(grey)|getrgb(w.style.windbgnd)))
	fi

	if hilite then
		oldtextcolour:=gxtextcolour(w)
		gxtextcolour(w,white)
	fi
!RETURN

	gxtext(w,text,x+smx,y+smy)
	if hilite then
		gxtextcolour(w,oldtextcolour)
	fi
end

func readnextitem(a)=
!return (level,value,labelx,options)
!special values used for divider, new column, new menu
!next line of file should already have been read

	if a="" then return list(0,0,0,0) fi

	level:=1
	tabs:=0
	options:=""

	while asc(a) in [9,' '] do tabs+:=1; a:=rightstr(a,-1) od

	if a="" then return list(0,0,0,0) fi

	case asc(a)
	when '!' then
		return list(0,0,0,0)
	esac

	if tabs then
		j:=0
		for i:=1 to ntab do
			if tabs=tabstack[i] then j:=i; exit fi
		od

		if j=0 then
			if tabs>tabstack[ntab] then
				ntab+:=1
				tabstack[ntab]:=tabs
			fi
			level:=ntab
		else
			level:=j
			if j<ntab then ntab:=j fi
		fi
	fi

	if asc(a) in ['0'..'9'] then
		value:=strtoval(a)
		n:=" " inx a
		if not n.isfound then
			n:=chr(9) in a
		fi
		if n.isfound then
			labelx:=rightstr(a,-n)
		else
			labelx:="?"
		fi

	else			!no preceding number, maybe top-level menu

		if "=" in a then	!command def for mpl
			return (0,0,0,0)
		fi

		value:=k_menu
		labelx:=a
		case convlc(labelx)
		when "hozbreak","divider" then
			value:=kdivide
		when "vertbreak" then
			value:=kcolumn
		when "filehistory" then
			value:=kfilehistory
		else
			if leftstr(labelx)="-" then value:=kdivide fi
		esac
	fi

	if labelx="" then			!maybe [cmd] only
		return list(0,0,0,0)
	fi

	return (level,value,(labelx),options)
end

func readmenu(m,n,level)=
!starting at index n in data, read all following items that are
!at lower level (ie. higher level number) than given level
!insert items into menu handle m
!return index of next item in data, which is at <level> or higher
!will stop at end of data, and return ndata+1

	restartx:
	for i:=n to ndata do
		(l,value,labelx,options):=data[i]

		if l<=level then		!end of this submenu
			return i
		fi

		flags:=breakflag
		enable:=1
		if rightstr(labelx)="?" then
			enable:=0
			labelx:=leftstr(labelx,-1)
		fi

		if options<>"" then
			if "H" in options then flags+:="h" fi
			if "C" in options then flags+:="c" fi
		fi

		case value
		when kdivide then
			gxaddmb(m,style:"d")
		when kcolumn then
			breakflag:="v"
		when k_menu then		!submenu
			newm:=gxcreatemb()
			n:=readmenu(newm,i+1,l)
			gxaddmb(m,labelx,newm,"p"+flags,enable)
			breakflag:=""
			goto restartx
		when kfilehistory then
			nfiles:=8
			gxaddmb(m,"filehistory",1060,breakflag)
		else				!ordinary command
	normalcmd:
			gxaddmb(m,labelx,value,flags,enable)
			breakflag:=""
		esac

	skip:
	od

	return ndata+1			!eod reached
end

func mbreaddata(a)=
!a is a list of tab-indented strings for a menu bar
!a can also be a text file containing the strings

	tabstack::=(0,)
	ntab:=1
	data::=()
	ndata:=0
	breakflag:=""

	if a.isstring then		!read from file
		a:=readtextfile(a)
		if a=0 then
			a:=("CANTOPENFILE",)
		fi
	fi

	for i:=1 to a.upb do
		x:=readnextitem(a[i])

		if x[1] then
			++ndata
			data[ndata]:=x
		fi
	od

	m:=gxcreatemb()
	readmenu(m,1,0)
	return m
end

export func gxmenubar(w,?a)=
!called as:
!	gxmenubar(a):	create standalone menu; return handle
!	gxmenubar(w,a):	add menu to windows w (returns 0)
!a:
!	string:			assume this is a filename containing menubar tabbed layout
!	list:			a list of strings containing the data

	if a.defined then		!w,m: read menu into window w
		m:=mbreaddata(a)

		if not w.ispointer then
			while w.owner<>nil do
				w:=w.owner
			od
		fi

		gxsetmb(w,m)
		return 0
	else				!create standalone menu, return handle
		return mbreaddata(w)
	fi
end

func gxcreatemb(?s)=
	if s.defined and s in "Pp" then
		return createpopupmenu()
	else
		return createmenu()
	fi
end

proc gxsetmb(w,m)=

	hwnd:=w.gdi.hwnd
	a:=getmenu(hwnd)
	s:=setmenu(hwnd,m)
	if a then destroymenu(a) fi
end

func gxaddmb(wm,caption="X",id=0,style="",enable=0)=

	if wm.ispointer then				!assume handle
		hmenu:=wm
		wm:=nil
	else
		hmenu:=getmenu(wm.gdi.hwnd)
	fi

	flags:=mf_string ior mf_unchecked

	if not enable then flags ior:=mf_greyed fi

	foreach c in convuc(style) do
		case c
		when 0 then exit
		when 'P' then flags ior:=mf_popup
		when 'D' then flags ior:=mf_separator
		when 'B' then flags ior:=mf_menubreak
		when 'V' then flags ior:=mf_menubarbreak
		when 'H' then flags ior:=mf_help
		when 'C' then flags ior:=mf_checked
		esac
	od

	if appendmenu(hmenu,flags,id,caption) then
		if wm<>nil then drawmenubar(wm.gdi.hwnd) fi
		return hmenu
	fi
	return 0
end

proc gxshowmb(wm,w,x,y)=
!update menu associated with window; call this func if it has been updated
!when wm is a menu handle, draw the popup on the screen at x,y
!if wm.isint then
	if wm.ispointer then

		if not y.defined then
			x:=w
			y:=x
			w:=nil
			hwnd:=wapplic.gdi.hwnd
		else
			hwnd:=w.gdi.hwnd
		fi

		pos:=ws_point(x,y)

		if w<>nil then
			clienttoscreen(w.gdi.hwnd,&pos)
		fi

		trackpopupmenu(wm,0,pos.x,pos.y,0,hwnd,0)
	else
		drawmenubar(wm.gdi.hwnd)
	fi
end

func gxenablemb(wm,id,enable)=

	if wm.ispointer then				!assume handle
		hmenu:=wm
	else
		hmenu:=getmenu(wm.gdi.hwnd)
	fi

	if enable.defined then
		return enablemenuitem(hmenu,id,(enable|0|mf_greyed)+mf_bycommand)
	else
		return (getmenustate(hmenu,id,mf_bycommand) iand mf_greyed|0|1)
	fi
end

func gxcheckmb(wm,id,check)=
	if wm.ispointer then				!assume handle
		hmenu:=wm
	else
		hmenu:=getmenu(wm.gdi.hwnd)
	fi

	if check.defined then
		return checkmenuitem(hmenu,id,(check|mf_checked|mf_unchecked)+mf_bycommand)
	else
		return (getmenustate(hmenu,id,mf_bycommand) iand mf_checked|1|0)
	fi
end

proc gxclosemb(m)=
	destroymenu(m)
end

export func gxconfirm(m)=
	x:=gxmsgbox(m,"Confirm","byn")
	return x="yes"
end

func issubwindow(w,w2)=
	while w2 do
		if w2==w then return 1 fi
		w2:=w2.owner
	od
	return 0
end

export proc flushmessages=

end

PROC CHECKWIND(W, NAME)=
	IF W=NIL THEN RETURN FI
	IF W.NAME=NAME THEN
		CPL "CHECKWIND MATCHES****************"
	FI
END

EXPORT PROC CHECKCLOSED(NAME)=
!CPL "CC:",=NAME
!CPL "CC:",=CURRMESS.WIND

	CHECKWIND(CURRMESS.WIND,NAME)
	CHECKWIND(WFOCUS,NAME)

	for w in allwindows do
		checkwind(w, name)
!CPL =W
!CPL =W.CHILDLIST.TYPE
		if w then
		for wc in w.childlist do
			checkwind(wc, name)
		od
		fi
	od

	for i to nmessages do
		m:=messagequeue[i]
		checkwind(m.wind,name)
	od

	CPL "CC: OK*****"
END
=== bmlib.q 0 1 41/48 ===
VAR DEBUG=0

importdll imglib =
    func imgload_rgb		(stringz, ref byte, ref byte, ref byte, i32)ref byte

    func imgload_bgr		(stringz, ref i32, ref i32, ref i32, i32)ref byte

!    func nanoloadjpeg		(stringz, ref i32, ref i32, ref i32)ref byte
!    func loadjpegm			(stringz, ref i64, ref i64, ref i64)ref byte

    proc          imgload_free		(ref byte)
    func imgsave_jpeg_rgb	(stringz, ref byte, i32, i32, i32)i32
    func imgsave_jpeg_bgr	(stringz, ref byte, i32, i32, i32)i32
end

importdll jpeg =
    func loadjpegm			(stringz, ref i64, ref i64, ref i64)ref byte
end

type bmpheader = struct
	ws_bitmapfileheader fh
	ws_bitmapinfoheader bh
end

var	shifts=[2:1, 4:2, 8:3, 16:4, 32:5, 64:6]

proc main=

!	CPL "TESTING BMMAIN NEW"
!	FILE:="C:/JPEG/girl.jpg"
!	FILE:="C:/JPEG/girl.png"
!	FILE:="C:/JPEG/fifteen.png"
	FILE:="C:/JPEG/CARD2.jpg"
!	FILE:="C:/JPEG/MONA.jpg"
!
	BM:=BMLOAD(FILE)
!	CPL =BM.TYPE
!	CPL =BM
!	IF NOT BM THEN STOP FI
!
!	BMSAVE("freddy.jpg", BM)
!

!	w:=640
!	h:=480
!	w:=640
!	h:=48
!
!	bm:=bmcreate(8,w,h)
!!	gxclear(bm, 0xFF7FFF)
!
!	gxtext(bm,"Hello, World")
!	for y:=0 to h-1 do
!		for x:=0 to w-1 do
!			c:=0x00'FF'00
!!			gxpixel(bm,x,y,c<<16+c<<8+c)
!			gxpixel(bm,x,y,c)
!		od
!	od
!	

!	bm2:=bmtopal(bm)
!	bm2:=bmrgb24torgb32(bm)
!	bm2:=bmtogrey(bm,8)
!	bm2:=bmtogrey(bm,24)
!	bm2:=bmdupl(bm)

!BM2.PALTYPE:=COLOUR_PAL
!BMRESETPALETTE(BM2)
!CPL =BM2.PIXELBITS
!	BMSAVE("PALA.PPM",BM2)
!	BMSAVE("FRED.bmp",BM2)
!	BMSAVE("PALB.PPM",BM2,1)


!	CPL "LOADED"
!	STOP

	w:=GXCREATEWINDOW(DIM:(1800,700),caption:"Hi There")
	gxcopy(w,bm)
!	gxcopy(w,bm,scalex:0.5)
!	gxcopy(w,bm,scalex:5.0, x:100)
!	gxcopy(w,bm,scalex:2.0, x:100)
!	gxcopy(w,bm)
!T:=clock()
!to 100 do
!	gxcopy(w,bm)
!od
global const srccopy =  13369376

!CPL =SRCCOPY
	bitblt(w.gdi.hdc, 0, 0, 500,300,
				w.gdi.hdc,0,0,srccopy)


!	gxcopy(w,bm)
!	gxcopy(w,bm)
!	gxcopy(w,bm)
!	gxcopy(w,bm)
!cpl =clock()-t
!WAITKEY()
	eventloop()

end

export func bmcreate(pixelbits,width,height)=
!create new bitmap with given specs, return handle to bitmap (=rwindow ref)
!when maskptr<>nil, set up mask values

	bminfo:=new(ws_bitmapv5header)
	bminfo.size:=ws_bitmapv5header.bytes
	bminfo.width:=width
	bminfo.height:=-height
	bminfo.planes:=1
	bminfo.bitcount:=pixelbits

	pixelptr:=nil

	if pixelbits not in [8,24,32] then
		abort("bmcreate pixel size not supported:"+tostr(pixelbits))
	fi

!	hwnd:=createdibsection(screendc,&bminfo,0,&pixelptr,0,0)

!CPL =BMINFO.BYTES
!CPL =WS_BITMAPV5HEADER.BYTES
!CPL =BMINFO.SIZE
!CPL =BMINFO.WIDTH
!CPL =BMINFO.HEIGHT
!CPL =BMINFO.PLANES
!CPL =BMINFO.BITCOUNT

	hwnd:=createdibsection(nil,&bminfo,0,&pixelptr,nil,0)

	pixelptr:=makeref(pixelptr,byte)

	if hwnd=0 then
		error:=getlasterror()
		abort("bmcreate:CreateDIB failed:"+tostr(error))
	fi

!now create a bm record based around this handle

	bm:=new(rwindow,0)
	bm.windclass:=bitmap_class

	bm.dimx:=width
	bm.dimy:=abs(height)		!neg height used for top-down bitmaps

	bm.style:=defstyle

	bm.pixelbits:=pixelbits
	bm.pixelptr:=pixelptr

!set bytes per pixel
	bm.pixelbytes:=pixelbits%8

!set bytes per scanline
	n:=bm.pixelbytes*width

!n must be a multiple of 4 bytes
	if (n iand 3)<>0 then	!make bytes a multiple of 4
		n:=(n+4) iand 0xfffc
	fi
	bm.linebytes:=n
!CPL "XXX",=BM.TYPE
!CPL =BM.BASETYPE

	bm.framebytes:=bm.linebytes*bm.dimy

!set palette colours, using winrgb order
	if pixelbits=8 then
		palette:=new(array,i32,0..255)
		bm.paltype:=greyscale_pal
		colour:=0
		for i:=0 to 255 do
			palette[i]:=colour
			colour+:=0x10101
		od
	fi

	setupgdi(bm,hwnd)

	bm.gdi.hdc:=createcompatibledc(nil)
	bm.gdi.drawmode:=dm_memory
	bm.gdi.oldbmobj:=selectobject(bm.gdi.hdc,hwnd)	!should store original bitmap
	setstretchbltmode(bm.gdi.hdc,4)			!average pixels for best result

	bmputpalette(bm,palette)

	return bm
end

export func bmgetpalette(bm)=
!extract entire palette to p, in bmrgb order
	if bm.paltype then
		palette:=new(array,i32,0..256)
		getdibcolortable(bm.gdi.hdc,0,256,&palette)
		palette[256]:=bm.paltype
		reversepalette(palette)
	else
		palette:=()
	fi
	return palette
end

export proc bmputpalette(bm,p,reverse=1)=
!update entire palette from p, in bmrgb order
	if bm.paltype then
		if reverse then reversepalette(p) fi		!fix colours
		setdibcolortable(bm.gdi.hdc,0,256,&p)	!store
		if reverse then reversepalette(p) fi			!restore orignal palette
		if p.upb=256 then
			bm.paltype:=p[256]
		fi
	fi
end

export func bmcolour(bm,n,?colour)=
!get/set palette info:
!n=given:
! colour given: update colour entry
! colour omitted(-1): return colour value

	if colour.isdef then		!set colour
		colour:=revpixel(colour)
		setdibcolortable(bm.gdi.hdc,n,1,&colour)
		return colour
	else				!get colour
		colour:=0
		getdibcolortable(bm.gdi.hdc,n,1,&colour)
		return revpixel(colour)
	fi
end

export proc reversepalette(&p)=
!reverse values of 32-bit colour data at p
	for i:=0 to 255 do
		p[i]:=revpixel(p[i])
	od
end

export func revpixel(a)=
!change rgb to bgr
!windows colours use red in lsb, bitmaps use blue in lsb, in 24-bit pixels and palette colours
return (a iand 0x00ff00) ior (a>>16 iand 255) ior ((a iand 255)<<16)
end

export proc bmshow(bm)=
	gxcopy(bm)
end

export proc bmfree(bm)=
	return when bm=nil
	if not deletedc(bm.gdi.hdc) then
		pcerror("ERROR DELETING BM/HDC")
	fi

	if not deleteobject(bm.gdi.hwnd) then
		pcerror("ERROR DELETING DIB")
	fi
end

export func bmdupl(bm)=
	newbm:=bmcreate(bm.pixelbits, bm.dimx, bm.dimy)
	memcpy(newbm.pixelptr, bm.pixelptr, bm.linebytes*bm.dimy)

	bmduplpalette(newbm,bm)

	return newbm
end

export proc bmduplpalette(newbm,bm)=
	if bm.paltype then
		pal:=bmgetpalette(bm)
		bmputpalette(newbm,pal)
		newbm.paltype:=bm.paltype
	fi
end

export func bmduplz(bm)=
	newbm:=bmcreate(bm.pixelbits, bm.dimx, bm.dimy)
	return newbm
end

export func bmgetptr(bm,x,y)=
!return byte pointer to given pixel
	return bm.pixelptr+(bm.linebytes*y+x*bm.pixelbytes)
end

export func bmgetrowptr(bm,y)=
	return bm.pixelptr+y*bm.linebytes
end

!export func bmgetpixel(bm,y)=
!	return bm.pixelptr+y*bm.linebytes
!end

func getcbbitmap(hwnd)=

	p:=globallock(hwnd)
	hsize:=ws_bitmapinfoheader.bytes
	bm:=nil

	if p then
		p:=makeref(p,ws_bitmapinfoheader)
		pb:=makeref(p,byte)

		bm:=bmcreate(p^.bitcount,p^.width,p^.height)
		offset:=(bm.paltype|1024|0)		!offset due to palette table

		if offset then
			setdibcolortable(bm.gdi.hdc,0,256,pb+hsize)
		fi

		pb:=pb+hsize+offset
		for y:=0 to bm.dimy-1 do
			q:=bmgetrowptr(bm,bm.dimy-y-1)
			memcpy(q,pb,bm.linebytes)
			pb:=pb+bm.linebytes
		od

	fi

	globalunlock(hwnd)

	return bm
end

export func bmgetclipboard=
!get image from clipboard if one is there, otherwise return nil
	if openclipboard(nil)=0 then
		return nil
	fi

	hwnd:=getclipboarddata(cf_dib)

	bm:=nil
	if hwnd then
		bm:=getcbbitmap(hwnd)
	fi

	closeclipboard()

	return bm
end

export func bmputclipboard(bm)=
	if openclipboard(0)=0 then
		return nil
	fi

	emptyclipboard()

	hwnd:=putcbbitmap(bm)
	if hwnd then
		setclipboarddata(cf_dib,hwnd)
	fi

	closeclipboard()
	return 1
end

func putcbbitmap(bm)=
	var mem

	hsize:=ws_bitmapinfoheader.bytes
	psize:=(bm.paltype|1024|0)
	fsize:=bm.linebytes*bm.dimy

	hmem:=globalalloc(0,hsize+psize+fsize)
	mem:=makeref(globallock(hmem),byte)
	mem:=0!makeref(globallock(hmem),byte)

	hdr:=new(ws_bitmapinfoheader)
	hdr.size:=hsize
	hdr.width:=bm.dimx
	hdr.height:=bm.dimy
	hdr.bitcount:=bm.pixelbits
	hdr.planes:=1
	hdr.xpelspermetre:=11811
	hdr.ypelspermetre:=11811
	hdr.clrused:=0

	memcpy(mem,&hdr,hsize)

	if psize then
		pal:=bmgetpalette(bm)
		memcpy(mem+hsize,&pal,psize)
	fi

	mem:=mem+hsize+psize
	for y:=0 to bm.dimy-1 do
		p:=bmgetrowptr(bm,bm.dimy-1-y)
		memcpy(mem, p, bm.linebytes)
		mem:=mem+bm.linebytes
	od
	globalunlock(hmem)

	return hmem
end

proc copy24to8(newbm,oldbm)=
!both images are same size. Copy 1st plane of 24-bit oldbm to 8-bit newbm
	for y:=0 to oldbm.dimy-1 do
		p:=bmgetrowptr(newbm,y)
		q:=bmgetrowptr(oldbm,y)
		to oldbm.dimx do
			p++^:=q^
			q:=q+3
		od
	od
end

!proc copy8to24(newbm,oldbm)=
!!both images are same size. Copy only plane of 8-bit oldbm to all planes of 24-bit newbm
!	for y:=0 to oldbm.dimy-1 do
!		p:=bmgetrowptr(newbm,y)
!		q:=bmgetrowptr(oldbm,y)
!		to oldbm.dimx do
!			p++^:=q^
!			q:=q+3
!		od
!	od
!end

export proc bmresetpalette(bm)=
# set palette back to greyscale
	pal:=new(array,i32,0..256)
	colour:=0
	for i:=0 to 255 do
		pal[i]:=colour
		colour+:=0x010101
	od
	bmputpalette(bm,pal)
	bm.paltype:=greyscale_pal
end

func makescalemap(x)=
!x=0..1; create 256-element lookup table to multiply 0..255 by x
	map:=new(list,0..255)
	for i:=0 to 255 do
		map[i]:=clamp(int(round(i*x)),0,255)
	od
	return map
end

func bmunimpl(mess)=
!ABORT("UNIMPLEMENTED: "+MESS)
PRINTLN "UNIMPLEMENTED:",MESS
PRINTLN "ABORTING"
STOP
return 0
end

!===========================================================================
!========= HANDLERS
!===========================================================================

export func bmload(filename)=
!CPL "BMLOAD:",FILENAME
	case e:=convlc(extractext(filename))
	when "jpg","jpeg" then
		return bmloadjpg(filename)
	when "bmp" then
		return bmloadbmp(filename)
!	when "pgm" then
!		return bmloadpgm_p2p5(filename)
	when "ppm","pgm" then
		return bmloadppm_p3p6(filename)
	when "png" then
		return bmloadpng(filename)
	when "" then				!try all
		exts:=("jpg","bmp","pgm","ppm","png")
		for ext in exts do
			bm:=bmload(addext(filename,ext))
			if bm then
				return bm
			fi
		od
		return nil
	else
		println "CAN'T LOAD",E,"IMAGE"
		return nil
	esac
	return nil
end

func bmloadbmp(filename)=
	f:=openfile(filename)
	if not f then return nil fi

	fileheader:=new(bmpheader)

	readrandom(f,&fileheader,0,bmpheader.bytes)
	filedimx:=fileheader.bh.width
	filedimy:=fileheader.bh.height
	invert:=1
	if filedimy<0 then
		filedimy:=abs(filedimy)
		invert:=0
	fi

	if fileheader.fh.typex<>'BM' then
		closefile(f)
		return nil
	fi

	if fileheader.bh.compression<>0 then
		closefile(f)
		return nil
	fi

	bm:=bmcreate(fileheader.bh.bitcount,filedimx,filedimy)
	framebytes:=bm.linebytes*filedimy

	if bm.paltype then
		palette:=new(array,i32,0..255)
		readrandom(f,&palette,bmpheader.bytes,1024)
		bmputpalette(bm,palette,0)
		colour:=0
		for i:=0 to 255 do
			if palette[i]<>colour then
				bm.paltype:=colour_pal
				exit
			fi
			colour+:=0x010101
		od

	fi

	readrandom(f,bm.pixelptr,fileheader.fh.offbits,framebytes)
	closefile(f)


	if invert then
		n:=bm.linebytes
		buffer:=makeref(malloc(n),byte)

		for y:=0 to filedimy%2 do
			p:=bmgetrowptr(bm,y)
			q:=bmgetrowptr(bm,filedimy-1-y)
!
			memcpy(buffer,p,n)
			memcpy(p,q,n)
			memcpy(q,buffer,n)
		od
		free(buffer)
	fi

	return bm
end

export func bmloadjpg(filename)=
	w:=h:=n:=0

CPL "LOADJ1"
!	p:=imgload_bgr(filename,&w,&h,&n,3)

!	p:=imgload_bgr(filename,&w,&h,&n,3)
	p:=loadjpegm(filename,&w,&h,&n);
!	p:=nanoloadjpeg(filename,&w,&h,&n)


!	p:=imgload_bgr(filename,&w,&h,&n)
CPL "LOADJ2",=P, W,H,N
	IF P=NIL THEN PCERROR("CAN'T LOAD JPG") FI

	pixelbits:=n*8

	if p=nil then
		return nil
	fi

	q:=makeref(p,byte)

	bm:=bmcreate(pixelbits,w,h)

	nbytes:=w*h*n
	dest:=makeref(bm.pixelptr,byte)

	to h do
		memcpy(dest,q,w*n)
		dest:=dest+bm.linebytes
		q:=q+w*n
	od

!	imgload_free(p)

	return bm
end

func bmloadpbm_p1p4(filename)=
CPL "CAN'T LOAD PBM"
RETURN NIL
!return bmunimpl("bmloadpbm")
end

func bmloadpgm_p2p5(filename)=

CPL "LOAD PGM P25",FILENAME
	f:=openfile(filename,"rb")
	if f=0 then return nil fi

	readln @f, sig:"s"

!CPL =SIG
	case sig
	when "P5" then
		binary:=1
	when "P2" then
		binary:=0
	else
		abort("Can't read pgm")
		return nil
	esac	

	width:=readnextint(f)
	height:=readnextint(f)
	maxpix:=readnextint(f)

	bm:=bmcreate(8,width,height)

	linebytes:=width
	dest:=makeref(bm.pixelptr,byte)

CPL =WIDTH,=HEIGHT, LINEBYTES,=BM
	to height do
		if binary then
			readbytes(f,dest,width)			!will be bgr
		else
			p:=dest
			to linebytes do
				p++^:=readffint(f)
			od
		fi

		dest:=dest+bm.linebytes
	od

	closefile(f)

	return bm
end

func bmloadppm_p3p6(filename)=
!read p6 ppm which is binary 24-bit, but will also recognise other formats

!CPL "P3P6"
	f:=openfile(filename,"rb")
	if f=0 then return nil fi

	readln @f, sig:"s"

!CPL =SIG
	case sig
	when "P6" then
		binary:=1
	when "P3" then
		binary:=0
	when "P5","P2" then
		closefile(f)
		return bmloadpgm_p2p5(filename)
!	when "P4","P1" then
!		closefile(f)
!		return bmloadpbm_p1p4(filename)
	else
		abort("Can't read ppm:"+sig)
		return nil
	esac	

!CPL "READING P6/P3 PPM"

	width:=readnextint(f)
	height:=readnextint(f)
	maxpix:=readnextint(f)

	bm:=bmcreate(24,width,height)

	linebytes:=width*3
	dest:=makeref(bm.pixelptr,byte)

	to height do
		if binary then
			readbytes(f,dest,linebytes)			!will be bgr
		else
			p:=dest
			to linebytes do
				p++^:=readffint(f)
			od
		fi

		p:=dest								!convert to rgb
		to width do
			swap(p^,(p+2)^)
			p:=p+3
		od

		dest:=dest+bm.linebytes
	od

	closefile(f)

	return bm
end

func readnextint(f)=
	read x
	while not x.isint and not eof(f) do
		readln @f,x
	od
	if not x.isint then return 0 fi
	return x
end

func readffint(f)=
!read next free-format int from f
	repeat
		c:=inbyte(f)
	until c in '0'..'9'

	a:=c-'0'
	do
		c:=inbyte(f)
		if c in '0'..'9' then
			a:=a*10+c-'0'
		else
			exit
		fi
	od

	return a
end

func bmloadpng(filename)=
	w:=h:=n:=0

CPL "LOADP1"
!	p:=imgload_bgr(filename,&w,&h,&n,3)

	p:=imgload_bgr(filename,&w,&h,&n,0)

!	p:=nanoloadjpeg(filename,&w,&h,&n)



!	p:=imgload_bgr(filename,&w,&h,&n)
CPL "LOADP2",=P, W,H,N

	pixelbits:=n*8

	if p=nil then
		return nil
	fi

	q:=makeref(p,byte)

	bm:=bmcreate(pixelbits,w,h)

	nbytes:=w*h*n
	dest:=makeref(bm.pixelptr,byte)

	to h do
		memcpy(dest,q,w*n)
		dest:=dest+bm.linebytes
		q:=q+w*n
	od

	imgload_free(p)

	return bm
end

export func bmsave(filename,bm,binary=0)=
	case e:=convlc(extractext(filename))
	when "jpg","jpeg" then
		return bmsavejpg(filename,bm)
	when "bmp" then
		return bmsavebmp(filename,bm)
	when "ppm","pgm" then
		return bmsaveppm_p3p6(filename,bm,binary)
	else
		println "CAN'T SAVE",E,"IMAGE"
		return nil
	esac
	return nil
end

func bmsavebmp(filename,bm)=
	w:=bm.dimx
	h:=bm.dimy
	pixelbytes:=bm.pixelbytes
	framebytes:=bm.linebytes*h
	palettebytes:=(pixelbytes=1|1024|0)

	bmfile:=createfile(filename)
	if bmfile=nil then
		return 0
	fi

	fileheader:=new(bmpheader)

	fileheader.fh.typex:='BM'
	fileheader.fh.offbits:=bmpheader.bytes+palettebytes
	fileheader.fh.size:=fileheader.fh.offbits+framebytes
	fileheader.bh.size:=ws_bitmapinfoheader.bytes
	fileheader.bh.width:=bm.dimx
	fileheader.bh.height:=-bm.dimy
	fileheader.bh.bitcount:=bm.pixelbits
	fileheader.bh.planes:=1
	fileheader.bh.xpelspermetre:=11811		!300 dpi
	fileheader.bh.ypelspermetre:=11811
	fileheader.bh.clrused:=0

	writerandom(bmfile,&fileheader,0,bmpheader.bytes)

	if palettebytes then
		palette:=bmgetpalette(bm)
		reversepalette(palette)
		writerandom(bmfile,&palette,bmpheader.bytes,palettebytes)
	fi

	writerandom(bmfile,bm.pixelptr,fileheader.fh.offbits,framebytes)

	return closefile(bmfile)
end

func bmsavejpg(filename,bm)=
	w:=bm.dimx
	h:=bm.dimy
	pixelbytes:=bm.pixelbytes
	linebytes:=bm.linebytes

	p:=q:=malloc(pixelbytes*w*h)

	s:=makeref(bm.pixelptr,byte)

	to h do
		memcpy(q,s,w*pixelbytes)
		q:=q+bm.linebytes
		s:=s+w*pixelbytes
	od

	status:=imgsave_jpeg_bgr(filename,p,w,h,pixelbytes)

	free(p)

	return status
end

func bmsavepbm_p1p4(filename,bm,binary)=
return bmunimpl("bmsaveppm-1bit")
end

func bmsavepgm_p2p5(filename,bm,binary)=
	width:=bm.dimx
	height:=bm.dimy

	f:=createfile(filename)

	CPL "WRITEPGM",filename

	if not f then return 0 fi

	println @f,(binary|"P5"|"P2")
	println @f,width,height
	println @f,"255"

	buffer:=data

	buffer:=malloc(bm.linebytes)
	if buffer=nil then return 0 fi
	buffer:=makeref(buffer,byte)

	linebytes:=width			!also number of values per line when in text mode

	for y:=0 to height-1 do
		memcpy(buffer,bmgetrowptr(bm,y),linebytes)
		if binary then
			writebytes(f,buffer,linebytes)
		else
			p:=buffer
			to linebytes do
				print @f,p++^,," "
			od
			println @f
		fi
	od
	closefile(f)
	return 1
end

func bmsaveppm_p3p6(filename,bm,binary)=
!	return bmunimpl("bmsaveppm")

	case bm.pixelbits
	when 24 then
	when 8 then
		return bmsavepgm_p2p5(filename,bm,binary)
	else
		return 0
	esac

	width:=bm.dimx
	height:=bm.dimy

	f:=createfile(filename)

	CPL "WRITEPPM",filename

	if not f then return 0 fi

	println @f,(binary|"P6"|"P3")
	println @f,width
	println @f,height
	println @f,"255"

	buffer:=data

	buffer:=malloc(bm.linebytes)
	if buffer=nil then return 0 fi
	buffer:=makeref(buffer,byte)

	linebytes:=width*3			!also number of values per line when in text mode

	for y:=0 to height-1 do
		memcpy(buffer,bmgetrowptr(bm,y),linebytes)
		p:=buffer					!convert to bgr
		to width do
			swap(p^,(p+2)^)
			p:=p+3
		od
		if binary then
			writebytes(f,buffer,linebytes)
		else
			p:=buffer
			to linebytes do
				print @f,p++^,," "
			od
			println @f
		fi
	od
	closefile(f)
	return 1
end

export func bmrotate(bm, angle)=
	case angle
	when 0 then return bmdupl(bm)
	when -90 then return bmrotleft90(bm)
	when +90 then return bmrotright90(bm)
	when 180 then return rot180(bm)
	esac
	return bmunimpl("bmrotate by "+tostr(angle))
end

export func bmrotleft90(bm)=
	case bm.pixelbits
	when 8 then return rotleft90_8(bm)
	when 24 then return rotleft90_24(bm)
	when 32 then return bmunimpl("ROTLEFT90/32")
	esac
	return nil
end

export func bmrotright90(bm)=
	case bm.pixelbits
	when 8 then return rotright90_8(bm)
	when 24 then return rotright90_24(bm)
	when 32 then return bmunimpl("ROTRIGHT90/32")
	esac
	return nil
end

export func rot180(bm)=
	newbm1:=bmfliphoz(bm)
	newbm2:=bmflipvert(newbm1)
	bmfree(newbm1)
	return newbm2
end

func rotleft90_8(bm)=
	w:=bm.dimx
	h:=bm.dimy
	linebytes:=bm.linebytes

	newbm:=bmcreate(8,h,w)

	for y:=0 to w-1 do
		q:=bmgetptr(bm,w-y-1,0)
		p:=bmgetrowptr(newbm,y)

		to h do
			p++^:=q^
!			q:=q+w
			q:=q+linebytes
		od
	od

	bmduplpalette(newbm,bm)
	return newbm
end

func rotright90_8(bm)=
	w:=bm.dimx
	h:=bm.dimy
	linebytes:=bm.linebytes

	newbm:=bmcreate(8,h,w)

	for y:=0 to w-1 do
		q:=bmgetptr(bm,y,h-1)
		p:=bmgetrowptr(newbm,y)

		to h do
			p++^:=q^
			q:=q-linebytes
		od
	od

	bmduplpalette(newbm,bm)
	return newbm
end

!function rotleft90_24(bm)=
!	newbm:=bmcreate(24,bm.dimy,bm.dimx)
!
!	xform:=new(array,ws_point,3)
!	xform[1].y:=bm.dimx
!	xform[3].x:=bm.dimy
!	xform[3].y:=bm.dimx
!
!	plgblt(newbm.gdi.hdc,&xform, bm.gdi.hdc,0,0,bm.dimx,bm.dimy, nil,0,0)
!
!	return newbm
!end

func rotleft90_24(bm)=
	w:=bm.dimx
	h:=bm.dimy

	newbm:=bmcreate(24,h,w)

	for y:=0 to w-1 do
		q:=bmgetptr(bm,w-y-1,0)
		p:=bmgetrowptr(newbm,y)

		to h do
			p++^:=q^
			p++^:=(q+1)^
			p++^:=(q+2)^
			q:=q+bm.linebytes
		od
	od

	return newbm
end

!function rotright90_24(bm)=
!	newbm:=bmcreate(24,bm.dimy,bm.dimx)
!
!	xform:=new(array,ws_point,3)
!	xform[1].X:=bm.dimy
!	xform[2].x:=bm.dimy
!	xform[2].y:=bm.dimx
!
!	plgblt(newbm.gdi.hdc,&xform, bm.gdi.hdc,0,0,bm.dimx,bm.dimy, nil,0,0)
!
!	return newbm
!end

func rotright90_24(bm)=
	w:=bm.dimx
	h:=bm.dimy

	newbm:=bmcreate(24,h,w)

	for y:=0 to w-1 do
		q:=bmgetptr(bm,y,h-1)
		p:=bmgetrowptr(newbm,y)

		to h do
			p++^:=q^
			p++^:=(q+1)^
			p++^:=(q+2)^
			q:=q-bm.linebytes
		od
	od

	return newbm
end

func rotate8(bm,angle)=
return bmunimpl("rotate8")
end

export func bmfliphoz(bm)=
	case bm.pixelbytes
    when 1 then return fliphoz8(bm)
    when 3 then return fliphoz24(bm)
    when 4 then return fliphoz32(bm)
	esac
	return nil
end

func fliphoz8(bm)=
	newbm:=bmdupl(bm)

	w:=newbm.dimx
	h:=newbm.dimy
	buffer:=makeref(malloc(bm.linebytes),byte)

	for y:=0 to h-1 do
		p:=bmgetrowptr(newbm,y)
		q:=p+w-1
		for x:=0 to w%2 do
			t:=p^
			p^:=q^
			q^:=t
			++p; --q
		od
	od

	return newbm
end

func fliphoz24(bm)=
	newbm:=bmdupl(bm)

	w:=newbm.dimx
	h:=newbm.dimy
	buffer:=makeref(malloc(bm.linebytes),byte)

	for y:=0 to h-1 do
		p:=bmgetrowptr(newbm,y)
		memcpy(buffer,p,bm.linebytes)
		q:=buffer+(w-1)*3

		to w do
			p++^:=q++^
			p++^:=q++^
			p++^:=q^

			q:=q-5
		od
	od

	return newbm
end

func fliphoz32(bm)=
return bmunimpl("fliphoz_32")
end

export func bmflipvert(bm)=
	newbm:=bmdupl(bm)

	w:=newbm.dimx
	h:=newbm.dimy
	n:=bm.linebytes
	buffer:=makeref(malloc(n),byte)

	for y:=0 to h%2 do
		p:=bmgetrowptr(newbm,y)
		q:=bmgetrowptr(newbm,h-1-y)
!
		memcpy(buffer,p,n)
		memcpy(p,q,n)
		memcpy(q,buffer,n)
	od
	free(buffer)

	return newbm
end

export func bmrepeat(bm,cols,rows)=
	w:=bm.dimx
	h:=bm.dimy
	newbm:=bmcreate(bm.pixelbits, w*cols, h*rows)
	linebytes:=bm.linebytes

	for y:=0 to h-1 do
		s:=bmgetrowptr(bm,y)
		for r:=0 to rows-1 do
			for c:=0 to cols-1 do
				memcpy(bmgetptr(newbm,c*w,r*h+y),s,linebytes)
			od
		od
	od

	if bm.pixelbits=8 then
		bmduplpalette(newbm,bm)
	fi

	return newbm
end

export func bmscale(bm, sx,?sy)=
	if sy.isvoid then sy:=sx fi
	case bm.pixelbits
	when 8 then return scalex8(bm,sx,sy)
	when 24,32 then return scalex24(bm,sx,sy)
	esac
	return nil
end

func scalex8(bm,sx,sy)=
	w:=bm.dimx
	h:=bm.dimy

	neww:=int(round(w*sx))
	newh:=int(round(h*sy))

	newbm:=bmcreate(24, neww,newh)
	return nil when not newbm

	bm24:=bmtorgb(bm,24)

	stretchblt(newbm.gdi.hdc,0,0,neww,newh,bm24.gdi.hdc,0,0,w,h, srccopy)

	if bm.paltype=greyscale_pal then
		newbm8:=bmgetplane(newbm,"R")
	else
		newbm8:=bmtopal(newbm)
	fi
	bmfree(newbm)

	return newbm8
end

func scalex24(bm,sx,sy)=
	w:=bm.dimx
	h:=bm.dimy

	neww:=int(round(w*sx))
	newh:=int(round(h*sy))

	if neww<8 or newh<8 then return nil fi

	newbm:=bmcreate(bm.pixelbits, neww,newh)
	if newbm then
		stretchblt(newbm.gdi.hdc,0,0,neww,newh,bm.gdi.hdc,0,0,w,h, srccopy)
	fi

	return newbm
end

func bmscaleupi8(bm,sx,sy)=
return bmunimpl("bmscaleupi8")
end

func bmscaleupi24(bm,sx,sy)=
return bmunimpl("bmscaleupi24")
end

func bmscaleupi32(bm,sx,sy)=
return bmunimpl("bmscaleupi32")
end

func bmscaledowni8(bm,sx,sy)=
return bmunimpl("bmscaledowni8")
end

func bmscaledowni24(bm,sx,sy)=
return bmunimpl("bmscaledowni24")
end

func bmscaledowni32(bm,sx,sy)=
return bmunimpl("bmscaledowni32")
end

export func bmneg(bm)=
	newbm:=bmdupl(bm)

	dx:=newbm.dimx-1
	dy:=newbm.dimy-1
	n:=newbm.linebytes
	do32:=0
	if n rem 4=0 then
		do32:=1
		n:=n%4
	fi

	for y:=0 to dy do
		if do32 then
			p:=makeref(bmgetrowptr(newbm,y),i32)
			to n do
				p++^ := p^ ixor 0xFFFFFFFF
			od
		else
			p:= bmgetrowptr(newbm,y)
			to n do
				p++^ := p^ ixor 255
			od
		fi
	od
	return newbm
end

export func bmmap(bm,map, channels="RGB")=
	if channels="" then channels:="RGB" fi

	case bm.pixelbits
	when 8 then
		return mapall(bm,map)
	when 24 then
		if channels="RGB" then
			return mapall(bm,map)
		fi
		return mapchan_24(bm,map,channels,0)
	when 32 then
		if channels="RGBA" then
			return mapall(bm,map)
		fi
		return mapchan_24(bm,map,channels,1)
	esac

	return nil
end

func mapall(bm,map)=
	newbm:=bmdupl(bm)
	p:=newbm.pixelptr
	to newbm.framebytes do
		p^:=map[p^]
		++p
	od
	return newbm
end

func mapchan_24(bm,map,channels,alpha=0)=
	dored:="R" in channels
	dogreen:="G" in channels
	doblue:="B" in channels
	doalpha:="A" in channels

	newbm:=bmdupl(bm)

	for y:=0 to newbm.dimy-1 do
		p:=bmgetrowptr(newbm,y)
		to newbm.dimx do
			if doblue then p^:=map[p^] fi
			++p
			if dogreen then p^:=map[p^] fi
			++p
			if dored then p^:=map[p^] fi
			++p
			if alpha then
				if doalpha then p^:=map[p^] fi
				++p
			fi
		od
	od

	return newbm
end

!function mapchan_32(bm,map,channels)=
!return bmunimpl("mapchan_32")
!end

export func bmbright(bm,dx,channels="RGB")=
	return bmunimpl("bmbright")
end

export func bmcont(bm,x,channels="RGB")=
	return bmunimpl("bmcont")
end

export func bmgamma(bm,x,channels="RGB")=
return bmunimpl("bmgamma")
end

export func bmtogrey(bm,destbits=24)=
	if destbits=0 then destbits:=bm.pixelbits fi
	case bm.pixelbits
	when 8 then
		case destbits
		when 8 then
			return pal8togrey8(bm)
		when 24 then
			cm:=pal8togrey8(bm)
			newbm:=grey8torgb24(cm)
			bmfree(cm)
			return newbm
		esac
	when 24,32 then
		case destbits
		when 8 then
			return rgb24togrey8(bm)
		when 24 then
			cm:=rgb24togrey8(bm)
			newbm:=grey8torgb24(cm)
			bmfree(cm)
			return newbm
		esac
	esac
CPL =BM.PIXELBITS, =DESTBITS
	return bmunimpl("bmtogrey bad combos")

end

func pal8togrey8(bm)=
	w:=bm.dimx
	h:=bm.dimy

	(rmap, gmap, bmap):=getlumtables()

	newbm:=bmcreate(8,w,h)
	pal:=bmgetpalette(bm)

	for y:=0 to h-1 do
		p:=bmgetrowptr(newbm,y)
		q:=bmgetrowptr(bm,y)

		to w do
			colour:=pal[q++^]
			r:=colour.[0..7]
			g:=colour.[8..15]
			b:=colour.[16..23]
			p++^:=rmap[r]+gmap[g]+bmap[b]
		od
	od

	return newbm
end

func pal8togrey24(bm)=
return bmunimpl("pal8togrey24")
end

func rgb24togrey8(bm)=
!does 24/32 bits
	qincr:=(bm.pixelbits=32|1|0)
	w:=bm.dimx
	h:=bm.dimy

	(rmap, gmap, bmap):=getlumtables()

	newbm:=bmcreate(8,w,h)

	for y:=0 to h-1 do
		p:=bmgetrowptr(newbm,y)
		q:=bmgetrowptr(bm,y)

		to w do
			b:=q++^; g:=q++^; r:=q++^
			p++^:=rmap[r]+gmap[g]+bmap[b]
			q:=q+qincr
		od
	od

	return newbm
end

func rgb24togrey24(bm)=
return bmunimpl("rgb24togrey24")
end

export func bmtorgb(bm,destbits=24)=
	if destbits=0 then destbits:=24 fi
	case bm.pixelbits
	when destbits then
		return bmdupl(bm)

	when 8 then
		case destbits
		when 8 then
			bmunimpl("8 to 8 bits rgb")
		when 24 then
			if bm.paltype=greyscale_pal then
				return grey8torgb24(bm)
			else
				return paltorgb24(bm)
			fi
		esac
	when 24 then
		if destbits=32 then
			return bmrgb24torgb32(bm)
		fi
	when 32 then
		if destbits=24 then
			return bmrgb32torgb24(bm)
		fi
	esac
CPL =BM.PIXELBITS, =DESTBITS
	return bmunimpl("bmtorgb bad combos")
end

func paltorgb24(bm)=
	w:=bm.dimx
	h:=bm.dimy

	newbm:=bmcreate(24,w,h)
	pal:=bmgetpalette(bm)

	for y:=0 to h-1 do
		p:=bmgetrowptr(newbm,y)
		q:=bmgetrowptr(bm,y)

		to w do
			colour:=pal[q++^]
			r:=colour.[0..7]
			g:=colour.[8..15]
			b:=colour.[16..23]

			p++^:=b
			p++^:=g
			p++^:=r
		od
	od

	return newbm
end

func grey8torgb24(bm)=
	w:=bm.dimx
	h:=bm.dimy

	newbm:=bmcreate(24,w,h)

	for y:=0 to h-1 do
		p:=bmgetrowptr(newbm,y)
		q:=bmgetrowptr(bm,y)

		to w do
			lum:=q++^
			p++^:=lum
			p++^:=lum
			p++^:=lum
		od
	od

	return newbm
end

export func bmrgb24torgb32(bm)=
	w:=bm.dimx
	h:=bm.dimy
	newbm:=bmcreate(32,w,h)

	for y:=0 to h-1 do
		q:=bmgetrowptr(bm,y)
		p:=bmgetrowptr(newbm,y)
		to w do
			p++^:=q++^
			p++^:=q++^
			p++^:=q++^
			p++^:=128
		od
	od
	return newbm
end

export func bmrgb32torgb24(bm)=
	w:=bm.dimx
	h:=bm.dimy
	newbm:=bmcreate(24,w,h)

	for y:=0 to h-1 do
		q:=bmgetrowptr(bm,y)
		p:=bmgetrowptr(newbm,y)
		to w do
			p++^:=q++^
			p++^:=q++^
			p++^:=q++^
			q++
		od
	od
	return newbm
end

export func bmtopal(bm)=
	if bm.pixelbits=8 then return bmdupl(bm) fi
	qincr:=(bm.pixelbits=32)
	w:=bm.dimx
	h:=bm.dimy

	newbm:=bmcreate(8,w,h)

	pal:=new(array,i32,0..255)

!create special palette mapping for rrrgggbb
	for r:=0 to 7 do
		for g:=0 to 7 do
			for b:=0 to 3 do
				index:=r<<5+g<<2+b
!				pal[index]:=r<<5+g<<13+b<<18
				pal[index]:=r<<5+g<<13+b<<22
			od
		od
	od
	bmputpalette(newbm,pal)
	bm.paltype:=colour_pal

	for y:=0 to h-1 do
		p:=bmgetrowptr(newbm,y)
		q:=bmgetrowptr(bm,y)

!noise:=-16..16
!noise:=-8..8

		to w do
			b:=q++^; g:=q++^; r:=q++^

!			r+:=clamp(random(noise),0,255)
!			g+:=clamp(random(noise),0,255)
!			b+:=clamp(random(noise),0,255)
!
			p++^:=r>>5<<5 + g>>5<<2 + b>>6
			q:=q+qincr
		od
	od

	return newbm

end

export func bmsplittorgb(bm,greydest=1)=
# split 24-bit bitmap into three separate 8-bit planes
# return 3 new bitmaps in the order red, green, blue
# return () on error
# dogreyscale=1 for each image to have a greyscale palette. Otherwise
# the red image will be shades of red, etc

	if bm.pixelbits<24 then
		return ()
	fi
	channels:=bm.pixelbytes

	w:=bm.dimx
	h:=bm.dimy

	pal:=new(array,i32,0..255)
	images::=()

	for offset:=channels-1 downto 0 do

		newbm:=bmcreate(8,w,h)

		for y:=0 to h-1 do
			q:=bmgetrowptr(bm,y)+offset
			p:=bmgetrowptr(newbm,y)
			to w do
				p++^:=q^
				q:=q+channels
			od
		od

		if not greydest then
			colour:=0
			incr:=(3-offset|0x00'00'01,0x00'01'00,0x01'00'00|0x01'01'01)
			for i:=0 to 255 do
				pal[i]:=colour
				colour+:=incr
			od
			bmputpalette(newbm,pal)
			newbm.paltype:=tinted_pal
		fi

		images append:=newbm
	od

!image order is r,g,b, or a,r,g,b
	if images.len=4 then
		return (images[2],images[3],images[4],images[1])
	else
		return images
	fi
end

export func bmsplittoyuv(bm)=
# split 24-bit bitmap into three separate 8-bit planes
# return 3 new bitmaps in the order y, u, v
# return () on error

	needfree:=0
	case bm.pixelbits
	when 24 then
	when 32 then
		bm:=bmtorgb(bm,24)
		needfree:=1
	else
		return nil
	esac

!get y plane first with existing routine
	greybm:=bmtogrey(bm,8)

	w:=bm.dimx
	h:=bm.dimy

!equations used are:
! u:=int(round(0.492*(b-y)+128)
! v:=int(round(0.702*(r-y)+128)
!b-y and r-y will be in range +/- 255

	umap:=new(list,-255..255)
	vmap:=new(list,-255..255)
	for i:=-255 to 255 do
		umap[i]:=int(round(0.492*(i)+128))
		vmap[i]:=int(round(0.702*(i)+128))
	od

	ubm:=bmcreate(8,w,h)
	vbm:=bmcreate(8,w,h)
	for c:=1 to 2 do
		if c=1 then
			offset:=0
			map:=umap
		else
			offset:=2
			map:=vmap
		fi

		for yy:=0 to h-1 do
			py:=bmgetrowptr(greybm,yy)
			p:=bmgetrowptr(bm,yy)			!point to bgr pixels in original

			pu:=bmgetrowptr(ubm,yy)
			pv:=bmgetrowptr(vbm,yy)
			to w do
				y:=py++^
				r:=(p+2)^
				b:=p^
				pu++^:=umap[b-y]
!CPL =B,=Y,=PV,=VMAP.TYPE
				pv++^:=vmap[r-y]

				p:=p+3
			od
		od
	od

!create special greyscale for u/v images, since point of zero colour
!info has been offset to +128
	pal:=new(array,i32,0..256)
	colour:=0
	pal[128]:=0
	for i:=1 to 127 do
		colour+:=0x020202
		pal [i+128]:=colour
		pal [128-i]:=colour
	od
	pal[256]:=uv_pal
	bmputpalette(ubm,pal)
	bmputpalette(vbm,pal)

	if needfree then
		bmfree(bm)
	fi

	return (greybm,ubm,vbm)
end

export func bmgetplane(bm,plane)=
# plane is one of "R","G","B"
# extract given plane of a 24-bit bitmaps into a single 8-bit greyscale image
# Return new image

	incr:=bm.pixelbytes
	if plane.len<>1 or bm.pixelbytes<3 then
		return nil
	fi

	case asc(plane)
	when 'R' then offset:=2
	when 'G' then offset:=1
	when 'B' then offset:=0
	when 'A' then offset:=3
	else return nil
	esac

	w:=bm.dimx
	h:=bm.dimy
	newbm:=bmcreate(8,w,h)

	for y:=0 to h-1 do
		q:=bmgetrowptr(bm,y)+offset
		p:=bmgetrowptr(newbm,y)
		to w do
			p++^:=q^
			q:=q+incr
		od
	od

	return newbm
end

export func bmjoinrgb(redbm,greenbm,bluebm,alphabm=nil)=

	w:=redbm.dimx
	h:=redbm.dimy

	newbm:=bmcreate((alphabm|32|24),w,h)

	for y:=0 to h-1 do
		p:=bmgetrowptr(newbm,y)

		r:=bmgetrowptr(redbm,y)
		g:=bmgetrowptr(greenbm,y)
		b:=bmgetrowptr(bluebm,y)

		if alphabm then
			a:=bmgetrowptr(alphabm,y)
			to w do
				p++^:=b++^
				p++^:=g++^
				p++^:=r++^
				p++^:=a++^
			od
		else
			to w do
				p++^:=b++^
				p++^:=g++^
				p++^:=r++^
			od
		fi
	od

	return newbm
end

export func bmjoinyuv(ybm,ubm,vbm)=
# combine y, u, v separations into a single rgb image
# return new bitmap, or nil

	if ybm.pixelbits<>8 then
		return nil
	fi

	w:=ybm.dimx
	h:=ybm.dimy

	v1425map:=new(list,0..255)
	v726map:=new(list,0..255)
	u395map:=new(list,0..255)
	u2032map:=new(list,0..255)

	for i:=0 to 255 do
		v1425map[i]:=int(round(1.425*(i-128)))
		v726map[i]:=int(round(0.726*(i-128)))
		u395map[i]:=int(round(0.395*(i-128)))
		u2032map[i]:=int(round(2.032*(i-128)))
	od

	newbm:=bmcreate(24,w,h)

	for yy:=0 to h-1 do
		p:=bmgetrowptr(newbm,yy)
		qy:=bmgetrowptr(ybm,yy)
		qu:=bmgetrowptr(ubm,yy)
		qv:=bmgetrowptr(vbm,yy)

!		to w do
		FOR X:=0 TO W-1 DO
			y:=qy++^
			r:=y+v1425map[qv^]
			g:=y-u395map[qu^]-v726map[qv^]
			b:=y+u2032map[qu^]
			++qu
			++qv
			p++^:=clamp(b,0,255)
			p++^:=clamp(g,0,255)
			p++^:=clamp(r,0,255)
		od
	od
	return newbm
end

export func bmblur(bm,n)=
	case bm.pixelbits
	when 8 then
		return blur8(bm,n)
	when 24 then
		return blur24(bm,n)
	when 32 then
		return blur32(bm,n)
	esac
	return nil
end

func blur8(bm,n)=
	shift:=shifts{n,1}

	newbm:=bmdupl(bm)
	iblurhoz8(newbm,n)

	newbm2:=rotleft90_8(newbm)
	iblurhoz8(newbm2,n)

	newbm3:=rotright90_8(newbm2)
	bmfree(newbm)
	bmfree(newbm2)

	bmduplpalette(newbm3,bm)
	return newbm3
end

func blur24(bm,n)=
	(r,g,b):=bmsplittorgb(bm)

	r2:=bmblur(r,n)
	g2:=bmblur(g,n)
	b2:=bmblur(b,n)

	newbm:=bmjoinrgb(r2,g2,b2)
	bmfree(r2)
	bmfree(g2)
	bmfree(b2)

	return newbm
end

func blur32(bm,n)=
return bmunimpl("blur32")
end

proc iblurhoz8(bm,n)=
	shift:=shifts{n,1}

	w:=bm.dimx
	h:=bm.dimy

	for y:=0 to h-1 do
		p:=bmgetrowptr(bm,y)
!		blurhelper(w-n-1, n, shift, p)

		to w-n-1 do
			sum:=0
			q:=p
			to n do
				sum+:=q++^
			od
			p++^:=sum>>shift
		od
	od
end

proc blurhelper(m, n, shift, p)=
	var sum
	var q

	to m do
		sum:=0
		q:=p
		to n do
			sum+:=q++^
		od
		p++^:=sum>>shift
	od
end

func blurhoz24(bm,n)=
return bmunimpl("blurhoz24")
end

func blurhoz32(bm,n)=
return bmunimpl("blurhoz32")
end

export func bmsharpen(bm,n=0)=
	case bm.pixelbits
	when 8 then
		return sharpen8(bm,n)
	when 24 then
		return sharpen24(bm,n)
	when 32 then
		return sharpen32(bm,n)
	esac
	return nil
end

export func sharpen8(bm,n)=
!blur in-place horizontally by averaging each set of n pixels
!n must be multiple of 2 from 2 to 64
!return new modified image

	w:=bm.dimx
	h:=bm.dimy

	newbm:=bmdupl(bm)

	for y:=1 to h-2 do
		p:=bmgetptr(newbm,1,y)

		q:=bmgetptr(bm,1,y-1)
		r:=bmgetptr(bm,1,y)
		s:=bmgetptr(bm,1,y+1)

		to w-2 do
!			abcdefghij
			a:=(q-1)^
			b:=q^
			c:=(q+1)^
			d:=(r-1)^
			e:=r^
			f:=(r+1)^
			g:=(s-1)^
			h:=s^
			i:=(s+1)^

! a b c
! d e f
! g h i
!			sum:=e*4-b-d-f-h
!			p^:=clamp(p^+sum%4,0,255)

			sum:=e*8-a-b-c-d-f-g-h-i
			p^:=clamp(p^+sum%8,0,255)

!			sum:=e*4+c+g+i-2*(b+d+f+h)
!			p^:=clamp(p^+sum%4,0,255)

			++p
			++q
			++r
			++s
		od
	od

	return newbm

end

export func sharpen24(bm,n)=
	(r,g,b):=bmsplittorgb(bm)

	r2:=bmsharpen(r,n)
	g2:=bmsharpen(g,n)
	b2:=bmsharpen(b,n)

	newbm:=bmjoinrgb(r2,g2,b2)
	bmfree(r2)
	bmfree(g2)
	bmfree(b2)

	return newbm
end

export func sharpen32(bm,n)=
return bmunimpl("bmsharpen32")
end

func getlumtables=
	rmap:=makescalemap(0.299)
	gmap:=makescalemap(0.587)
	bmap:=makescalemap(0.111)
	return (rmap, gmap, bmap)
end
=== console.q 0 1 42/48 ===
!!Virtual keycodes
export const vklbutton=1		!note these are physical not logical buttons
export const vkrbutton=2
export const vkmbutton=4		!middle button is correct
export const vkbackspace=8
export const vktab=9
export const vkclear=12
export const vkenter=13
export const vkshift=16
export const vkctrl=17
export const vkalt=18
export const vkbreak=19
export const vkcapslock=20
!export const vkrshift=21
export const vkrctrl=22
!export const vkralt=23
export const vkinslock=24
export const vkescape=27
export const vkspace=32
export const vkpageup=33
export const vkpagedown=34
export const vkend=35
export const vkhome=36
export const vkleft=37
export const vkup=38
export const vkright=39
export const vkdown=40
export const vkinsert=45
export const vkdelete=46
export const vkhelp=47
export const vk0='0'
export const vka='A'
export const vkwindows=91
export const vkrightbutton=93
export const vknumpad0=96		!96..105 = '0'..'9'
export const vkmul=106
export const vkadd=107
export const vksub=109
export const vkdecimal=110
export const vkdiv=111
export const vkf1=112
export const vkf2=113
export const vkf3=114
export const vkf4=115
export const vkf5=116
export const vkf6=117
export const vkf7=118
export const vkf8=119
export const vkf9=120
export const vkf10=121
export const vkf11=122
export const vkf12=123
!export const vklsq=128
!export const vkrsq=129
!export const vksemi=130
!export const vkquote=131
!export const vkstroke=132
!export const vkdot=133
!export const vkcomma=134
!export const vkbackslash=135
!export const vkquote2=136
!export const vkequals=137
!export const vkminus=138
!export const vkhash=139
export const vklshift=160
export const vkrshift=161
export const vklcontrol=162
export const vkrcontrol=163
export const vklalt=164
export const vkralt=165

!oem codes
export const vkminus=189
export const vkequals=187
export const vklsq=219
export const vkrsq=221
export const vksemi=186
export const vkquote=192
export const vkhash=222
export const vkcomma=188
export const vkperiod=190
export const vkslash=191
export const vkbackslash=220
export const vkbackquote=223

export const con_black=0
export const con_dkblue=1
export const con_dkred=2
export const con_dkmagenta=3
export const con_dkgreen=4
export const con_dkcyan=5
export const con_dkyellow=6
export const con_dkgrey=7
export const con_grey=8
export const con_blue=9
export const con_red=10
export const con_magenta=11
export const con_green=12
export const con_cyan=13
export const con_yellow=14
export const con_white=15


export record winrec =
	var posx,posy
	var cols,rows
	var fgnd,bgnd			!default text/background colour

	var columns			!used when divided into columns
	var itemcols			!width of each column
	var pagesize			!columns*rows

	var name

	var hdata			!pointer to data record, or is nil
end

export var wconscreen
export var screencols,screenrows

export var chardata			!string these two represent row of the console
export var attrdata			!string

export var defscreenfgnd=con_black
export var defscreenbgnd=con_grey
export var rlkey=0		!set by readline, when special key has been input
export var rlbuffer			!contents of readline buffer when special key pressed

var cmdindex,ncmds
var cmdhistory

export const capsmask  = 0x8		!shift states as they are in .keyshift
export const altmask   = 0x4
export const ctrlmask  = 0x2
export const shiftmask = 0x1

export const capsbit=3
export const altbit=2
export const ctrlbit=1
export const shiftbit=0

var keypending=0
var lastkey
var pendkey
export var hconsole, hconsolein
var colourpalette

!export var wscreencols,wscreenrows
!export var currbgnd=-1,currfgnd=-1
export var currbgnd=con_grey, currfgnd=con_black

!export var screencolour=con_dkred..con_grey

!export var colourmap
export VAR SUPPRESS=0

VAR ALLCHARS

proc START=
!if iswindows() then
!	CPL "WINCON INIT"
		init()
!fi
	end

proc main=
	init()
	settitle("New Title")

!keyscreentest()

!W:=MAKEWIN((1,20),(20,20))
!CLEARWIN(W)

!SHOWTEXT("^^^^^^^^^^^^^^^^^")

a:=rkey(10,20,30)
!	setpos(12, 10)
!	print "***********hello"
!	waitkey()
end

proc keyscreentest=
	(cols,rows):=(screencols, screenrows)
	CPL =COLS,=ROWS

	row:=rows%2
	col:=cols%2
	ch:="X"

	setcolour(6,1)

	do
		setpos(col,row)
		cp ch
		setpos(col,row)
		k:=getkey().keycode
		case k
		when 27 then
			exit
		when vkleft then col:=max(1,col-1)
		when vkright then col:=min(cols,col+1)
		when vkup then row:=max(1,row-1)
		when vkdown then row:=min(rows,row+1)
		esac
	od

!waitkey()

end

export func makerspoint(x,y)=
!combine x,y into 32-bit value (rspoint)
	return y<<16 ior x
end

export proc setpos(col,row)=

!!ROW+:=10
!fprint "\s[{#};{#}H",row,col

	setconsolecursorposition(hconsole,makerspoint(col-1,row-1))
end

export func getpos=
	info:=new(ws_console)
	getconsolescreenbufferinfo(hconsole,&info)
	return (info.pos.x+1,info.pos.y+1)
end

export proc init(cols=100)=
!static var setdimdone=0

!CPL "CONSOLE INIT-----------"


!	consolesw.init(cols)
	cmdhistory::=()	!"one","two","three","four")
	ncmds:=cmdhistory.upb
	cmdindex:=0

!screencols:=consolesw.wscreencols
!screenrows:=consolesw.wscreenrows
!

	hconsole:=getstdhandle(-11)
	hconsolein:=getstdhandle(-10)
	lastkey:=new(ws_keyevent)
	lastkey.repeatcount:=0
	pendkey:=new(ws_keyevent)

	setdims(cols,60)
!	setdims(50,20)

	getdims()

!CPL =SCREENCOLS

	wconscreen:=makewin((1,1),(screencols,screenrows),defscreencolour)

	colourpalette:=new(ws_palette16)

	setstdpalette()
end

export func setcursor(?visible)=
	cursor:=new(ws_cursor)
	getconsolecursorinfo(hconsole,&cursor)

	if visible.defined then
		cursor.visible:=visible
		setconsolecursorinfo(hconsole,&cursor)
	fi
	return cursor.visible
end

export proc setcolour(fgnd, ?bgnd)=
!call with as (fgnd,bgnd) or as (fgnd..bgnd)

	if bgnd.isvoid then bgnd:=currbgnd fi

	if fgnd=currfgnd and bgnd=currbgnd then
!		return
	fi

	currfgnd:=fgnd
	currbgnd:=bgnd

	setconsoletextattribute(hconsole,(bgnd*16+fgnd))
end

export proc settitle(caption)=
	setconsoletitle(caption)
end

export func getkeychar=
!wait for any key, return single char code; as returned by C's getch()
	return waitkey()
end

export func getkey2=
!wait for any key, return keyrec
!includes shift key presses as discrete keys
!use getkey() to ignore these

	return getchx()

	k:=getchx()			!get keyrec, encoded as int

	key:=new(rkey)			!convert to proper keyrec
	key.charcode:=k iand 65535
	key.shift:=k>>24
	key.keycode:=k.[23..16]
!CPL "GK2:",KEY

	return key
end

export func getkey=
!calls igetkey but doesn't return shift keys as discrete key presses
	do
		k:=getkey2()
		case k.keycode
		when vkshift,vkctrl,vkalt,vkcapslock then
		else
			exit
		esac
	od
	return k
end

export func keyready=
	return testkey()
end

export proc showtext(s,?x,?y)=

	if x.defined then
		setpos(x,y)
	fi

	count:=0
	if s then
		if not suppress then
			writeconsole(hconsole,s,s.len,&count,nil)
		fi
	fi
end

proc setwindowsize(cols,rows)=
	r:=new(ws_srect)
	r.leftx:=0
	r.rightx:=cols-1
	r.top:=0
	r.bottom:=rows-1
	if not setconsolewindowinfo(hconsole,1,&r) then
!	CPL "WINDOW ERROR 1"
!	abort("Window error 1")
	fi
end

export proc setdims(cols,rows)=
!set new size for console, by reinitialising

	maxcol:=cols
	maxrow:=rows

	info:=new(ws_console)
	oldscreenattributes:=info.attributes
	oldscreensize:=info.size

	oldcols:=info.window.rightx-info.window.leftx+1
	oldrows:=info.window.bottom-info.window.top+1

	IF OLDSCREENSIZE.X>COLS OR OLDSCREENSIZE.Y>ROWS THEN	!need to reduce window size first
		setwindowsize(oldscreensize.x min cols, oldscreensize.y min rows)
	fi

!Set the new size of the entire (virtual) console window
	if setconsolescreenbuffersize(hconsole,rows<<16+cols)=0 then
!	abort("Buffer size error")
	fi

!now set the size of the displayed portion of it; in this case exactly the same
!size as the buffer, with no scrollbars
	setwindowsize(cols,rows)

	wscreencols:=cols
	wscreenrows:=rows

!hide blinking cursor
	cursor:=new(ws_cursor)
	cursor.size:=10
	cursor.visible:=1
end

export proc setpalette(index,colour)=
!index is 0..15; colour is an rgb value bbggrr
!updates local palette array
!to update actual console, use writepalette
	colourpalette[index]:=colour
end

export proc writepalette=
	r:=new(ws_consoleex)
	r.recsize:=ws_consoleex.bytes
	X:=getconsolescreenbufferinfoex(hconsole,&r)

	r.palette:=colourpalette

	R.WINDOW.RIGHTX:=R.WINDOW.RIGHTX+1		!workaround off-by-one bug
	R.WINDOW.BOTTOM:=R.WINDOW.BOTTOM+1

	X:=setconsolescreenbufferinfoex(hconsole,&r)

!export proc READPALETTE=
!r:=new(rconsoleex)
!r.recsize:=rconsoleex.bytes
!x:=getconsolescreenbufferinfoex(hconsole,&r)
!
!CPL "GCSBI X=",X
!FOR I:=0 TO 15 DO
! CPL I,":",R.PALETTE[I]:"H"
!OD
!
end

proc setstdpalette=
!export const con_black=0
!export const con_dkblue=1
!export const con_dkred=2
!export const con_dkmagenta=3
!export const con_dkgreen=4
!export const con_dkcyan=5
!export const con_dkyellow=6
!export const con_grey=7
!export const con_dkgrey=8
!export const con_blue=9
!export const con_red=10
!export const con_magenta=11
!export const con_green=12
!export const con_cyan=13
!export const con_yellow=14
!export const con_white=15

!R G B
	cols:=(
	(0,		0,		0),			!black
	(0,		0,		128),		!dk blue
	(128,	0,		0),			!dk red
	(128,	0,		128),		!dk magenta
	(0,		128,	0),			!dk green
	(0,		128,	128),		!dk cyan
	(128,	128,	0),			!dk yellow
	(128,	128,	128),		!dk grey
	(192,	192,	192),		!grey
	(0,		0,		192),		!blue
	(192,	0,		0),			!red
	(192,	0,		192),		!magenta
	(0,		192,	0),			!green
	(0,		192,	192),		!cyan
	(192,	192,	0),			!yellow
	(255,	255,	255))		!white

	for i,c in cols do
		setpalette(i-1,c[3]<<16+c[2]<<8+c[1])
	od
!CPL "WRITEPAL"; WAITKEY()
	writepalette()
end

proc getdims=
	info:=new(ws_console)
	getconsolescreenbufferinfo(hconsole,&info)

	screencols:=info.window.rightx-info.window.leftx+1
	screenrows:=info.window.bottom-info.window.top+1
end

export func getchx=
	const rightaltmask	= 1				!masks used by .controlkeystate
	const leftaltmask	= 2
	const leftctrlmask	= 8
	const rightctrlmask	= 4
	const shiftmask		= 16
	const capsmask		= 128
	const scrollmask	= 64

	const leftctrlbit	= 3		!for c.l.p
	const rightctrlbit	= 2

	if keypending then
		lastkey:=pendkey
		keypending:=0
	else
		if lastkey.repeatcount=0 then
			repeat
				count:=0
				readconsoleinput(hconsolein,&lastkey,1,&count)
			until lastkey.eventtype=1 and lastkey.keydown=1
		fi
	fi

	altdown		:= (lastkey.controlkeystate iand (leftaltmask ior rightaltmask)|1|0)
	ctrldown	:= (lastkey.controlkeystate iand (leftctrlmask ior rightctrlmask)|1|0)
	shiftdown	:= (lastkey.controlkeystate iand shiftmask|1|0)
	capslock	:= (lastkey.controlkeystate iand capsmask|1|0)

	lastkey.repeatcount:=lastkey.repeatcount-1

	charcode:=lastkey.asciichar
	keycode:=lastkey.virtualkeycode iand 255

!for keycodes in range 186 to 223, which are all stand-alone punctuation keys, I might
!wish to set charcode to the appropriate printed char code (currently charcode will be
!zero, and keyboard handlers need to detect keycodes such as vkequals)
!
	if altdown and ctrldown and charcode=166 then
		altdown:=ctrldown:=0;
	else
		if altdown or ctrldown then
			charcode:=0;
			if keycode>='A' and keycode<= 'Z' then
				charcode:=keycode-'@'
			fi
		fi
	fi

	keyshift:=capslock<<3 ior altdown<<2 ior ctrldown<<1 ior shiftdown

	keyshift.[4]:=lastkey.controlkeystate.[leftctrlbit]		!for c.l.p
	keyshift.[5]:=lastkey.controlkeystate.[rightctrlbit]

!need to be more ruthless with how keycoded and charcodes are combined.
!More combinations need to have only charcode or keycode set, and the other zero

	switch charcode
	when 'A'..'Z','a'..'z','0'..'9' then
	when 8,9,13,27,' ','`' then
	when 0 then				!already key-only event
	else
		keycode:=0
	end switch

	return rkey(charcode,keycode,keyshift)

end

export proc flushkeyboard=
	flushconsoleinputbuffer(hconsolein)
end

export proc w_writeconsolerow(text, attributes, length, row)=
	buffersize:=1<<16+length
	coord:=0

	box:=ws_srect(0,row-1,length-1,row-1)

	buffer:=new(array,ws_charinfo,length)

	for i:=1 to length do
		x:=new(ws_charinfo)
		x.asciichar  := text.[i]
		x.attributes := attributes.[i]
!	x.attributes := attributes.[1]
		buffer[i]:=x
	od
!CPL "HELLO",text; WAITKEY()

	writeconsoleoutputa(hconsole, &buffer,buffersize,coord,&box)
end

export func setclipboard(s)=
!copy text to the Windows clipboard
!return status 0 if no clipboard o/p was possible
	const ghnd=2 + 0x40

	if openclipboard(nil)=0 then
		return 0
	fi

	emptyclipboard()

	if s<>"" then
		h:=globalalloc(ghnd,s.len+1)
		p:=globallock(h)

		memcpy(p,&s,s.len+1)
	globalunlock(h)
		setclipboarddata(cf_text,h)
	fi

	closeclipboard()

	return 1
end

export func getclipboard=
!copy text from Windows clipboard
!return clipboard text, or "" when error or not text data available

	if openclipboard(nil)=0 then
		return ""
	fi

	htext:=getclipboarddata(cf_text)

	if not htext then
		return ""
	fi

	size:=globalsize(htext)		!should include zero terminator

	p:=globallock(htext)
	s:=makestr(p,size-1)		!assignment should copy the string data

	globalunlock(htext)

	closeclipboard()
	return s
end

export func makewin(pos, dims, fgnd=con_black,bgnd=con_grey,name="Anon")=
!export func makewin(pos, dims, ?colour)=

	w:=new(winrec)
	w.posx:=pos[1]
	w.posy:=pos[2]
	w.cols:=dims[1]
	w.rows:=dims[2]
	w.columns:=1
	if dims.len>=3 then
		w.columns:=dims[3]
	fi

!CPL =POS,=DIMS,=W.COLUMNS

	w.itemcols:=w.cols%w.columns
	w.pagesize:=w.rows*w.columns
	w.hdata:=nil

	w.fgnd:=fgnd
	w.bgnd:=bgnd
	w.name:=name

	return w
end

export proc clearwin(w)=
!clear region used by listbox
!can clear multi-columns at once
	spaces:=" "*w.cols

	setcolour(w.fgnd,w.bgnd)
	for i:=1 to w.rows do
		showtext(spaces,w.posx,w.posy+i-1)
	od
	setpos(w.posx,w.posy)
end

export proc wsetpos(w,col,row)=
	setpos(w.posx+col-1,w.posy+row-1)
end

export proc wshowtext(w,s,?col,?row)=
	if col.defined then
		showtext(s,w.posx+col-1,w.posy+row-1)
	else
		showtext(s)
	fi
end

export proc wshowtext_b(w,s, colrow, fgnd,bgnd)=
!version of wshowtext that dumps into char/attr buffer.
!w is used for absolute column number

	if colrow.islist then
		col:=colrow[1]
	else
		col:=colrow
	fi


	length:=s.len
	offset:=w.posx-1	!hoz offset

	chardata.[(col+offset)..(col-1+length+offset)]:=s

!	attr:=consolesw.colourmap[bgnd]<<4+consolesw.colourmap[fgnd]
	attr:=bgnd<<4+fgnd

	attrdata.[(col+offset)..(col-1+length+offset)]:=chr(attr)*length
end

export proc updateconsolerow(w, row)=
!write out latest contents to chardata/attrdata to console
!this represents an entire composite wlineno+wvgap+wedit row, for given row within wedit
!etc
	w_writeconsolerow(chardata,attrdata,screencols,w.posy+row-1)
end

export func getkeyname(key)=
	case key.keycode
	when vkleft then name:="left"
	when vkright then name:="right"
	when vkup then name:="up"
	when vkdown then name:="down"
	when vkpageup then name:="pageup"
	when vkpagedown then name:="pagedown"
	when vkhome then name:="home"
	when vkend then name:="end"
	when vkinsert then name:="insert"
	when vkdelete then name:="delete"
	when vktab then name:="tab"
	when vkescape then name:="escape"
	when vkbackspace then name:="backspace"
	when vkenter then name:="enter"
	when vkf1..vkf12 then name:="f"+tostr(key.keycode-vkf1+1)
	when vkspace then name:="space"
	else
		if key.charcode in [1..26] then	!ctrl code
			name:=chr(key.charcode+'a'-1)
		elsif key.charcode in ['!','"','£','$','%','^','&','*','(',')','-','_','+','=','[',']',
		'{','}',':',';','\'','@','~','#','<','>',',','.','/','¬','¦','|','\\','?'] then
			name:=chr(key.charcode)
			key.shift iand:=inot shiftmask		!ignore any shift press needed to get char

		elsif key.keycode in ['A'..'Z','0'..'9'] then
			if (key.shift iand (ctrlmask ior altmask))=0 then
				name:=chr(key.charcode)
				key.shift iand:=inot shiftmask
			else
				name:=convlc(chr(key.keycode))
			fi
		elsif key.keycode in (186..223) then
			case key.keycode
			when vkminus then name:="-"
			when vkequals then name:="="
			when vklsq then name:="["
			when vkrsq then name:="]"
			when vksemi then name:=";"
			when vkquote then name:="'"
			when vkhash then name:="#"
			when vkcomma then name:=","
			when vkperiod then name:="."
			when vkslash then name:="/"
			when vkbackslash then name:="\\"
			when vkbackquote then name:="`"
			else
				return "?"
			esac
		else
			return "?"
		fi
	esac

	prefix::="*"
	if key.shift iand shiftmask then prefix+:="s" fi
	if key.shift iand ctrlmask then prefix+:="c" fi
	if key.shift iand altmask then prefix+:="a" fi
	return prefix+name

end

export func keynametokey(name)=
!given a key name in the format "*...", reconstruct an rkey record, and return that
	charcode:=shift:=keycode:=0

	name:=rightstr(name,-1)		!get rid of "*"

	if name.len=1 then		!simple printable key, no shifts
		charcode:=asc(name)
		goto simplekey

	else				!any letters s,c,a on left indicate a modifier
		while name.len>1 do
			case leftstr(name)
			when "s" then
				shift ior:=shiftmask
				name:=rightstr(name,-1)
			when "c" then
				shift ior:=ctrlmask
				name:=rightstr(name,-1)
			when "a" then
				shift ior:=altmask
				name:=rightstr(name,-1)
			else
				exit
			esac
		od

		case name
		when "left" then keycode:=vkleft
		when "right" then keycode:=vkright
		when "up" then keycode:=vkup
		when "down" then keycode:=vkdown
		when "pageup" then keycode:=vkpageup
		when "pagedown" then keycode:=vkpagedown
		when "home" then keycode:=vkhome
		when "end" then keycode:=vkend
		when "insert" then keycode:=vkinsert
		when "delete" then keycode:=vkdelete
		when "tab" then keycode:=charcode:=vktab
		when "escape" then keycode:=vkescape
		when "backspace" then keycode:=charcode:=vkbackspace
		when "enter" then keycode:=charcode:=vkenter
		when "space" then keycode:=charcode:=vkspace
		else
			if name.len>=2 and leftstr(name)="f" then	!function key
				keycode:=vkf1+strtoval(rightstr(name,-1))-1
			elsif name.len=1 then				!ordinary key, but with shifts
	simplekey:
				c:=asc(name)
				case c
				when ['A'..'Z'] then
					keycode:=c
				when ['a'..'z'] then
					keycode:=c-' '
				when ['0'..'9'] then
					keycode:=c
				when '-','_' then keycode:=vkminus
				when '=','+' then keycode:=vkequals
				when '[','{' then keycode:=vklsq
				when ']','}' then keycode:=vkrsq
				when ';',':' then keycode:=vksemi
				when '\'','@' then keycode:=vkquote
				when ',','<' then keycode:=vkcomma
				when '.','>' then keycode:=vkperiod
				when '/','?' then keycode:=vkslash
				when '\\','|' then keycode:=vkbackslash
				when '`','¬' then keycode:=vkbackquote
				when '#','~' then keycode:=vkhash
				when '!' then keycode:='1'
				when '"' then keycode:='2'
				when '£' then keycode:='3'
				when '$' then keycode:='4'
				when '%' then keycode:='5'
				when '^' then keycode:='6'
				when '&' then keycode:='7'
				when '*' then keycode:='8'
				when '(' then keycode:='9'
				when ')' then keycode:='0'
				else
					pcerror("keynametokey")
				end
			fi
		esac
	fi

	if shift iand (altmask ior ctrlmask) then
		charcode:=0
		if keycode in 'A'..'Z' then
			charcode:=keycode-'@'
		fi
	fi

	key:=new(rkey)			!convert to proper keyrec
	key.charcode:=charcode
	key.shift:=shift
	key.keycode:=keycode
	return key
end

export proc clearscreen(?bgnd,?fgnd)=

if bgnd.isvoid then bgnd:=defscreenbgnd fi
if fgnd.isvoid then fgnd:=defscreenfgnd fi
setcolour(fgnd,bgnd)

for i:=1 to screenrows do
	setpos(1,i)
	showtext(" "*screencols)
!	showtext("*"*screencols)
od
setpos(1,1)
end

export func readline(?cmdline,donewline=1)=
!this func doesn't handle tabs properly
!would need to maintain 2 buffers, one with tabs translated to spaces
!or convert tabs to another char which is translated back to tabs on exit
!return with input buffer set to the line, but also returns the complete line
!newline=1 to end with a newline, 0 to leave it

!readln
!return

	buffer:=""
	nchars:=0
!congetpos()

!NOTE: getpos is dodgy using TERMCON; MAY NEED CALLER TO SPECIFY START POINT
	(startx,starty):=(getpos())

	pos:=0		!with nchars shown, pos can be 0 to nchars

	reenter:
	if cmdline.defined and cmdline<>"" then
		buffer:=cmdline
	reenter2:
		pos:=nchars:=buffer.len
	fi

	do
! print "_"
		rlkey:=0			!normal input starts with "*" will expect rlkey to be a keyrec
		setpos(startx,starty)
		print buffer
		setpos(startx+pos,starty)

		key:=getkey()
		keycode:=key.keycode
		keyshift:=key.shift

		case keycode
		when vkpageup,vkpagedown,vkup,vkdown,vkinsert,vkf1..vkf12 then

	dospecial:
		rlbuffer:=buffer
			oldbufferlen:=buffer.len		!to help erase old buffer
			buffer:=getkeyname(key)
			rlkey:=key				!allow caller to use key code rather than name
			exit

		when vkleft then
			if buffer="" then goto dospecial fi
			if (keyshift iand 7) then goto dospecial fi

			if pos>0 then
				--pos
			fi

		when vkhome then
			if buffer="" then goto dospecial fi
			if (keyshift iand 7) then goto dospecial fi
			pos:=0

		when vkend then
			if buffer="" then goto dospecial fi
			if (keyshift iand 7) then goto dospecial fi
			pos:=nchars

		when vkright then
			if buffer="" then goto dospecial fi
			if (keyshift iand 7) then goto dospecial fi
			if pos<nchars then
				++pos
			fi

		when vkenter then

!  println
			exit

		when vkbackspace then

			if (keyshift iand 7) then goto dospecial fi
			if nchars then
				setpos(startx,starty)
				print " "*buffer.len

				case pos
				when 0 then			!not allowed
				when nchars then		!at end
					buffer:=leftstr(buffer,-1)
					--nchars
					--pos
				else				!in middle
					buffer:=leftstr(buffer,pos-1)+rightstr(buffer,-(pos))
					--nchars
					--pos
				esac

			fi

		when vkdelete then
			if (keyshift iand 7) then goto dospecial fi
			if nchars and nchars=pos then
				goto delline
			fi
			if nchars=0 then
				goto dospecial
			fi
			if nchars then
!CPL "\NNCHARS",=NCHARS,++CCC,=POS,"\N"
				setpos(startx,starty)
				print " "*buffer.len

				case pos
				when nchars then		!not allowed
!			when 0 then			!at start
!				buffer:=leftstr(buffer,-1)
!				--nchars
				else				!in middle
					buffer:=leftstr(buffer,pos)+rightstr(buffer,-(pos+1))
					--nchars
!    --pos
				esac

			fi

		when vkescape then
			if nchars=0 then
				goto dospecial
!   oldbufferlen:=buffer.len
!   buffer:="*esc"
!   exit
			fi
	delline:
			setpos(startx,starty)
			print " "*buffer.len

			buffer:=""
			nchars:=pos:=0

		when vktab then
			goto normalkey

		else
	normalkey:
			if (key.charcode>=' ' or key.charcode=9) then
				if pos=0 then
					buffer:=chr(key.charcode)+buffer
				elsif pos=nchars then
					buffer:=buffer+chr(key.charcode)
				else
					buffer:=leftstr(buffer,pos)+chr(key.charcode)+rightstr(buffer,-(pos))
				fi
				++nchars
				++pos
			else
				GOTO DOSPECIAL
				print "<",keycode,key.charcode,">"
			fi

		esac
	od

	case buffer
	when "*cup","*cdown" then
		if ncmds then
			setpos(startx,starty)
			print " "*oldbufferlen

			if cmdindex=0 then		!get started on last
				cmdline:=cmdhistory[ncmds]
				cmdindex:=ncmds
				goto reenter
			fi

			if buffer="*cup" and cmdindex>1 then
				--cmdindex
			elsif buffer="*cdown" and cmdindex<ncmds then
				++cmdindex
			fi
			cmdline:=cmdhistory[cmdindex]
			goto reenter
		fi
		buffer:=""
		goto reenter2
	esac

	if buffer.len>1 and leftstr(buffer)<>"*" then
		if ncmds=0 or cmdhistory[ncmds]<>buffer then
			cmdhistory[++ncmds]:=buffer
		fi
		cmdindex:=0
	fi

	if donewline then println fi

	return sreadln(buffer)
end

export proc wsetcolumns(w,columns)=
	w.columns:=columns
	w.itemcols:=w.cols%w.columns
	w.pagesize:=w.rows*w.columns
end

=== lincon.q 0 1 43/48 ===
!!Virtual keycodes
export const vklbutton=1		!note these are physical not logical buttons
export const vkrbutton=2
export const vkmbutton=4		!middle button is correct
export const vkbackspace=8
export const vktab=9
export const vkclear=12
export const vkenter=13
export const vkshift=16
export const vkctrl=17
export const vkalt=18
export const vkbreak=19
export const vkcapslock=20
!export const vkrshift=21
export const vkrctrl=22
!export const vkralt=23
export const vkinslock=24
export const vkescape=27
export const vkspace=32
export const vkpageup=33
export const vkpagedown=34
export const vkend=35
export const vkhome=36
export const vkleft=37
export const vkup=38
export const vkright=39
export const vkdown=40
export const vkinsert=45
export const vkdelete=46
export const vkhelp=47
export const vk0='0'
export const vka='A'
export const vkwindows=91
export const vkrightbutton=93
export const vknumpad0=96		!96..105 = '0'..'9'
export const vkmul=106
export const vkadd=107
export const vksub=109
export const vkdecimal=110
export const vkdiv=111
export const vkf1=112
export const vkf2=113
export const vkf3=114
export const vkf4=115
export const vkf5=116
export const vkf6=117
export const vkf7=118
export const vkf8=119
export const vkf9=120
export const vkf10=121
export const vkf11=122
export const vkf12=123
!export const vklsq=128
!export const vkrsq=129
!export const vksemi=130
!export const vkquote=131
!export const vkstroke=132
!export const vkdot=133
!export const vkcomma=134
!export const vkbackslash=135
!export const vkquote2=136
!export const vkequals=137
!export const vkminus=138
!export const vkhash=139
export const vklshift=160
export const vkrshift=161
export const vklcontrol=162
export const vkrcontrol=163
export const vklalt=164
export const vkralt=165

!oem codes
export const vkminus=189
export const vkequals=187
export const vklsq=219
export const vkrsq=221
export const vksemi=186
export const vkquote=192
export const vkhash=222
export const vkcomma=188
export const vkperiod=190
export const vkslash=191
export const vkbackslash=220
export const vkbackquote=223


export enumdata		rr,     gg,     bb =
	(con_black=0,	0,		0,		0),
	(con_dkblue,	0,		0,		128),
	(con_dkred,		128,	0,		0),
	(con_dkmagenta,	128,	0,		128),
	(con_dkgreen,	0,		128,	0),
	(con_dkcyan,	0,		128,	128),
	(con_dkyellow,	128,	128,	0),
	(con_dkgrey,	128,	128,	128),
	(con_grey,		192,	192,	192),
	(con_blue,		0,		0,		255),
	(con_red,		255,	0,		0),
	(con_magenta,	255,	0,		255),
	(con_green,		0,		255,	0),
	(con_cyan,		0,		255,	255),
	(con_yellow,	255,	255,	0),
	(con_white,		255,	255,	255),
end


var digits=['0'..'9']
var navkeys=['A':vkup, 'B':vkdown, 'C': vkright, 'D':vkleft, 'H':vkhome, 'F':vkend,
				'P':vkf1, 'Q':vkf2, 'R': vkf3, 'S':vkf4]

var fnkeys= [15:vkf5, 17:vkf6, 18:vkf7, 19:vkf8, 20:vkf9, 21:vkf10, 23:vkf11, 24:vkf12]

const capsmask  = 0x8		!shift states as they are in .keyshift
const altmask   = 0x4
const ctrlmask  = 0x2
const shiftmask = 0x1

const capsbit=3
const altbit=2
const ctrlbit=1
const shiftbit=0

var shiftcodes = [5:ctrlmask, 2:shiftmask, 3:altmask, 4:shiftmask+altmask, 7:ctrlmask+altmask]

export var wconscreen
export var screencols,screenrows
export var currbgnd=-1,currfgnd=-1

export var chardata			!string these two represent row of the console
export var attrdata			!string

export var rlkey=0		!set by readline, when special key has been input
export var rlbuffer			!contents of readline buffer when special key pressed

var cmdindex,ncmds
var cmdhistory

!export var screencolour=con_dkred..con_grey

export record winrec =
	var posx,posy
	var cols,rows
	var fgnd,bgnd			!default text/background colour

	var columns			!used when divided into columns
	var itemcols			!width of each column
	var pagesize			!columns*rows

	var name

	var hdata			!pointer to data record, or is nil
end

export proc init(cols=100)=
	getdims()

	cmdhistory::=()	!"one","two","three","four")
	ncmds:=cmdhistory.upb
	cmdindex:=0

	wconscreen:=makewin((1,1),(screencols,screenrows),defscreencolour)

end

proc getdims=
	(screencols,screenrows):=getscreensize()
end

export proc setpos(column,row)=
	fprint "\e[#;#H",row,column
end

proc setfgndcol(c)=
	fprint "\e[38;2;#;#;#m", rr[c], gg[c], bb[c]
end

proc setbgndcol(c)=
	fprint "\e[48;2;#;#;#m", rr[c], gg[c], bb[c]
end

export proc setbgndrgb(r,g,b)=
	fprint "\e[48;2;#;#;#m", r,g,b
end

export proc setbold(bold)=
	fprint "\e[#m",(bold|1|21)
end

export proc setitalic(italic)=
	fprint "\e[#m",(italic|3|23)
end

export func getpos=
	print "\e[6n"
	readkey()		!escape
	readkey()		![

	(row,column,c):=readkbdsequence()
	return (column,row)
end

export func setcursor(?visible)=
	return 1
end

export proc setcolour(fgnd, ?bgnd)=
!call with as (fgnd,bgnd) or as (fgnd..bgnd)

	if bgnd.isvoid then bgnd:=currbgnd fi

	if fgnd<>currfgnd then
		setfgndcol(fgnd)
		currfgnd:=fgnd
	fi

	if bgnd<>currbgnd then
		setbgndcol(bgnd)
		currbgnd:=bgnd
	fi
end

export proc settitle(caption)=
end

export func keyready=
	return pcerror("Linux/keyready")
end

!export proc showtext(s)=
!	if s then
!		print s
!	fi
!end

export proc showtext(s,?x,?y)=

	if x.defined then
		setpos(x,y)
	fi

	count:=0
	if s then
!		if not suppress then
			print s
!			writeconsole(hconsole,s,s.len,&count,nil)
!		fi
	fi
end

export proc setdims(cols,rows)=
	pcerror("linux/setdims")
end

export proc setpalette(index,colour)=
	pcerror("linux/setpallete")
end

func getscreensize=
	savepos()
	setpos(999,999)
	(cols,rows):=getpos()
	restorepos()
	return (cols,rows)
end

proc savepos=
	print "\e[s"
end

proc restorepos=
	print "\e[u"
end

func readkey=
	return waitkey()
end

func readintseq(c)=
!c is '0' to '9'
!read integer sequence up to first non-digit
!return (number, terminator character)
	x:=c-'0'
	do
		c:=readkey()
		if c in digits then
			x:=x*10+c-'0'
		else
			exit
		fi
	od
	return (x,c)
end

func readkbdsequence=
!Some key escape sequences for control chars in Linux look like this:
! <esc> "[" [x[";"y] c/"~"
!Parts in "..." are actual characters
!x and y are optional integers, c is a capital letter
!The sequence may have 0, 1 or 2 numbers (separated with ;) and end with
!a capital letter, or "~"
!the "[" has already been read
!return (X, Y, C)
!X or Y will be zero if not present. C will 'A' etc, or 0 if it ends with "~"
!-1 is returned on error

	x:=y:=0

	c:=readkey()

	if c in digits then
		(x,c):=readintseq(c)
		if c=';' then
			c:=readkey()
			if c not in digits then return -1 fi
			(y,c):=readintseq(c)
		fi
	fi

	if c='~' then
		return (x,y,0)
	fi
	return (x,y,c)				!assume A-Z
end

!function keyname(k,shift=0)=
!return getkeyname(rkey(0,k,shift))
!end

export func getkey=
!read key events via readkey()
!convert escape sequences to Windows virtual keys

	k:=readkey()				!LINUX ONLY

!CPL "<<<<<",K,">>>>>"

	case k
	when 10 then
		return rkey(13,vkenter,0)
	when 8 then
		return rkey(127, vkbackspace, ctrlmask)
	when 127 then
		return rkey(127, vkbackspace, 0)
	when 9 then
!	CPL "TAB1"
		return rkey(vktab,vktab,0)
	when 'A'..'Z', '0'..'9', ' ' then
		return rkey(k,k,0)
	when 'a'..'z' then
		return rkey(k,k-' ',0)
	when 27 then
	when 1..31 then
		return rkey(k,0,ctrlmask)
	else
		c:=k
		case k
		when '[','{' then k:=vklsq
		when ']','}' then k:=vkrsq
		else
			k:=0
		esac

		return rkey(c,k,0)
	esac

!CPL "ESC SEEN"

!escape seen; look at next key
	k:=readkey()

	case k
	when 27 then			!esc/esc => single escape
		return rkey(0,k,0)

	when 10 then			!esc/10 => alt enter
		return rkey(0,vkenter,altmask)

	when 8,127 then			!esc/bs => alt bs
		return rkey(0,vkbackspace,altmask)

	when 'O' then			!short set of function keys
		(x,y,c):=readkbdsequence()

!	CPL "O",x,y,chr(c)
		return rkey(0,navkeys{c},shiftcodes{y,0})

	when '[' then
		(x,y,c):=readkbdsequence()

		case c
		when 'Z' then						!shift+tab
			return rkey(9,9,shiftmask)
!		return rkey(0,9,shiftmask)
		when 'A','B','C','D','H','F','P','Q','R','S' then		!cursor keys, fn1..4; assume x=1
			return rkey(0,navkeys{c},shiftcodes{y,0})
		esac

		case x
		when 2,3,5,6 then
			shift:=0
			case y
			when 5 then shift:=ctrlmask
			when 3 then shift:=altmask
			when 7 then shift:=altmask+ctrlmask
			esac
			return rkey(0,(x|0,vkinsert,vkdelete,0,vkpageup|vkpagedown),shift)
		when 15..24 then
			return rkey(0,fnkeys{x},shiftcodes{y,0})
		esac

	when 'A'..'Z' then			!must have been alt version (some esc letter codes above)
		return rkey(k-64,0,altmask) 

	when 'a'..'z' then			!must have been alt version (some esc letter codes above)
		return rkey(k-96,0,altmask) 

	when '0'..'9' then
		return rkey(0,k,altmask) 

	esac
!	CPL "ESC 91"

!Code 91 SEEN
	return rkey(0,'?',0)
end

proc screentest=

	savepos()
	setpos(10,10)
	setfgndcol(5)
	setbgndcol(3)
	setbold(1)
	setitalic(1)
	println "	HELLO	"
	setbold(0)
	setitalic(0)
	restorepos()
	println "	Goodbye	"

	(cols,rows):=getscreensize()
	cpl =rows,=cols
	waitkey()
end

!proc keytest=
!
!lastkey:=0
!
!!do
!!	k:=readkey()
!!	if k=27 and lastkey=27 then exit fi
!!	if k=27 then
!!		cpl
!!		cp "ESC "
!!	elsif k in 32..126 then
!!		cp chr(k)
!!	else
!!		cp "<"+tostr(k)+">"
!!	fi
!!	lastkey:=k
!!od
!
!do
!	k:=getkey()
!	cpl getkeyname(k),k
!	if k.keycode=27 then exit fi
!od
!
!end

proc keyscreentest=
	(cols,rows):=getscreensize()
	CPL =COLS,=ROWS

	row:=rows%2
	col:=cols%2
	ch:="X"

	setfgndcol(6)
	setbgndcol(1)

	do
		setpos(col,row)
		cp ch
		setpos(col,row)
		k:=getkey().keycode
		case k
		when 27 then
			exit
		when vkleft then col:=max(1,col-1)
		when vkright then col:=min(cols,col+1)
		when vkup then row:=max(1,row-1)
		when vkdown then row:=min(rows,row+1)
		esac
	od

!	waitkey()

end

proc main=

!	keytest()
!	screentest()
	keyscreentest()
end

proc start=
!CPL "LINCON START"
	if not iswindows() then
		init()
	fi
end

export proc w_writeconsolerow(text, attributes, length, row)=
!pcerror("lincon/writeconsolerow")
!buffersize:=1<<16+length
!coord:=0

!setpos(1,row)
!print leftstr(text,length)

	setpos(1,row)
	for i:=1 to length-1 do
		attrs:=attributes.[i]
		c:=text.[i]
	!	setcolour(attrs>>4, attrs iand 15)
		setcolour(attrs iand 15, attrs>>4)
		print chr(c)
	od
end

export proc flushkeyboard=
	tcflush(0, 0)
end

export func setclipboard(s)=
	abort("linux/setclipboard")
	return 0
end

export func getclipboard=
	abort("linux/getclipboard")
	return ""
end

export proc clearscreen(?bgnd,?fgnd)=

	system("clear")
end

export func makewin(pos, dims, fgnd=con_black,bgnd=con_grey,name="Anon")=
!export func makewin(pos, dims, ?colour)=

	w:=new(winrec)
	w.posx:=pos[1]
	w.posy:=pos[2]
	w.cols:=dims[1]
	w.rows:=dims[2]
	w.columns:=1
	if dims.len>=3 then
		w.columns:=dims[3]
	fi

!CPL =POS,=DIMS,=W.COLUMNS

	w.itemcols:=w.cols%w.columns
	w.pagesize:=w.rows*w.columns
	w.hdata:=nil

	w.fgnd:=fgnd
	w.bgnd:=bgnd
	w.name:=name

	return w
end

export proc clearwin(w)=
!clear region used by listbox
!can clear multi-columns at once
	spaces:=" "*w.cols

	setcolour(w.fgnd,w.bgnd)
	for i:=1 to w.rows do
		showtext(spaces,w.posx,w.posy+i-1)
	od
	setpos(w.posx,w.posy)
end

export proc wshowtext(w,s,?col,?row)=
	if col.defined then
		showtext(s,w.posx+col-1,w.posy+row-1)
	else
		showtext(s)
	fi
end

export proc wsetpos(w,col,row)=
	setpos(w.posx+col-1,w.posy+row-1)
end

export proc wshowtext_b(w,s,colrow,fgnd,bgnd)=
!version of wshowtext that dumps into char/attr buffer.
!w is used for absolute column number

	if colrow.islist then
		(col, row):=colrow
	else
		col:=colrow
		ROW:=1
	fi

	setcolour(fgnd, bgnd)

	wshowtext(w,s, col, row)
!	showtext(s, col, row)


!	length:=s.len
!	offset:=w.posx-1	!hoz offset
!
!!CPL =CHARDATA
!
!	chardata.[(col+offset)..(col-1+length+offset)]:=s
!
!!	attr:=consolesw.colourmap[bgnd]<<4+consolesw.colourmap[fgnd]
!	attr:=bgnd<<4+fgnd
!
!	attrdata.[(col+offset)..(col-1+length+offset)]:=chr(attr)*length
end

export proc updateconsolerow(w,row)=
!write out latest contents to chardata/attrdata to console
!this represents an entire composite wlineno+wvgap+wedit row, for given row within wedit
!etc
!	w_writeconsolerow(chardata,attrdata,screencols,row)
end

export func getkeyname(key)=
	case key.keycode
	when vkleft then name:="left"
	when vkright then name:="right"
	when vkup then name:="up"
	when vkdown then name:="down"
	when vkpageup then name:="pageup"
	when vkpagedown then name:="pagedown"
	when vkhome then name:="home"
	when vkend then name:="end"
	when vkinsert then name:="insert"
	when vkdelete then name:="delete"
	when vktab then name:="tab"
	when vkescape then name:="escape"
	when vkbackspace then name:="backspace"
	when vkenter then name:="enter"
	when vkf1..vkf12 then name:="f"+tostr(key.keycode-vkf1+1)
	when vkspace then name:="space"
	else
		if key.charcode in [1..26] then	!ctrl code
			name:=chr(key.charcode+'a'-1)
		elsif key.charcode in ['!','"','£','$','%','^','&','*','(',')','-','_','+','=','[',']',
		'{','}',':',';','\'','@','~','#','<','>',',','.','/','¬','¦','|','\\','?'] then
			name:=chr(key.charcode)
			key.shift iand:=inot shiftmask		!ignore any shift press needed to get char

		elsif key.keycode in ['A'..'Z','0'..'9'] then
			if (key.shift iand (ctrlmask ior altmask))=0 then
				name:=chr(key.charcode)
				key.shift iand:=inot shiftmask
			else
				name:=convlc(chr(key.keycode))
			fi
		elsif key.keycode in (186..223) then
			case key.keycode
			when vkminus then name:="-"
			when vkequals then name:="="
			when vklsq then name:="["
			when vkrsq then name:="]"
			when vksemi then name:=";"
			when vkquote then name:="'"
			when vkhash then name:="#"
			when vkcomma then name:=","
			when vkperiod then name:="."
			when vkslash then name:="/"
			when vkbackslash then name:="\\"
			when vkbackquote then name:="`"
			else
				return "?"
			esac
		else
			return "?"
		fi
	esac

	prefix::="*"
	if key.shift iand shiftmask then prefix+:="s" fi
	if key.shift iand ctrlmask then prefix+:="c" fi
	if key.shift iand altmask then prefix+:="a" fi
	return prefix+name

end

export func readline(?cmdline,donewline=1)=
!this func doesn't handle tabs properly
!would need to maintain 2 buffers, one with tabs translated to spaces
!or convert tabs to another char which is translated back to tabs on exit
!return with input buffer set to the line, but also returns the complete line
!newline=1 to end with a newline, 0 to leave it

!readln
!return

	buffer:=""
	nchars:=0
!congetpos()

!NOTE: getpos is dodgy using TERMCON; MAY NEED CALLER TO SPECIFY START POINT
	(startx,starty):=(getpos())

	pos:=0		!with nchars shown, pos can be 0 to nchars

	reenter:
	if cmdline.defined and cmdline<>"" then
		buffer:=cmdline
	reenter2:
		pos:=nchars:=buffer.len
	fi

	do
! print "_"
		rlkey:=0			!normal input starts with "*" will expect rlkey to be a keyrec
		setpos(startx,starty)
		print buffer
		setpos(startx+pos,starty)

		key:=getkey()
		keycode:=key.keycode
		keyshift:=key.shift

		case keycode
		when vkpageup,vkpagedown,vkup,vkdown,vkinsert,vkf1..vkf12 then

	dospecial:
		rlbuffer:=buffer
			oldbufferlen:=buffer.len		!to help erase old buffer
			buffer:=getkeyname(key)
			rlkey:=key				!allow caller to use key code rather than name
			exit

		when vkleft then
			if buffer="" then goto dospecial fi
			if (keyshift iand 7) then goto dospecial fi

			if pos>0 then
				--pos
			fi

		when vkhome then
			if buffer="" then goto dospecial fi
			if (keyshift iand 7) then goto dospecial fi
			pos:=0

		when vkend then
			if buffer="" then goto dospecial fi
			if (keyshift iand 7) then goto dospecial fi
			pos:=nchars

		when vkright then
			if buffer="" then goto dospecial fi
			if (keyshift iand 7) then goto dospecial fi
			if pos<nchars then
				++pos
			fi

		when vkenter then

!  println
			exit

		when vkbackspace then

			if (keyshift iand 7) then goto dospecial fi
			if nchars then
				setpos(startx,starty)
				print " "*buffer.len

				case pos
				when 0 then			!not allowed
				when nchars then		!at end
					buffer:=leftstr(buffer,-1)
					--nchars
					--pos
				else				!in middle
					buffer:=leftstr(buffer,pos-1)+rightstr(buffer,-(pos))
					--nchars
					--pos
				esac

			fi

		when vkdelete then
			if (keyshift iand 7) then goto dospecial fi
			if nchars and nchars=pos then
				goto delline
			fi
			if nchars=0 then
				goto dospecial
			fi
			if nchars then
!CPL "\NNCHARS",=NCHARS,++CCC,=POS,"\N"
				setpos(startx,starty)
				print " "*buffer.len

				case pos
				when nchars then		!not allowed
!			when 0 then			!at start
!				buffer:=leftstr(buffer,-1)
!				--nchars
				else				!in middle
					buffer:=leftstr(buffer,pos)+rightstr(buffer,-(pos+1))
					--nchars
!    --pos
				esac

			fi

		when vkescape then
			if nchars=0 then
				goto dospecial
!   oldbufferlen:=buffer.len
!   buffer:="*esc"
!   exit
			fi
	delline:
			setpos(startx,starty)
			print " "*buffer.len

			buffer:=""
			nchars:=pos:=0

		when vktab then
			goto normalkey

		else
	normalkey:
			if (key.charcode>=' ' or key.charcode=9) then
				if pos=0 then
					buffer:=chr(key.charcode)+buffer
				elsif pos=nchars then
					buffer:=buffer+chr(key.charcode)
				else
					buffer:=leftstr(buffer,pos)+chr(key.charcode)+rightstr(buffer,-(pos))
				fi
				++nchars
				++pos
			else
				GOTO DOSPECIAL
				print "<",keycode,key.charcode,">"
			fi

		esac
	od

	case buffer
	when "*cup","*cdown" then
		if ncmds then
			setpos(startx,starty)
			print " "*oldbufferlen

			if cmdindex=0 then		!get started on last
				cmdline:=cmdhistory[ncmds]
				cmdindex:=ncmds
				goto reenter
			fi

			if buffer="*cup" and cmdindex>1 then
				--cmdindex
			elsif buffer="*cdown" and cmdindex<ncmds then
				++cmdindex
			fi
			cmdline:=cmdhistory[cmdindex]
			goto reenter
		fi
		buffer:=""
		goto reenter2
	esac

	if buffer.len>1 and leftstr(buffer)<>"*" then
		if ncmds=0 or cmdhistory[ncmds]<>buffer then
			cmdhistory[++ncmds]:=buffer
		fi
		cmdindex:=0
	fi

	if donewline then println fi

	return sreadln(buffer)
end

export proc wsetcolumns(w,columns)=
	w.columns:=columns
	w.itemcols:=w.cols%w.columns
	w.pagesize:=w.rows*w.columns
end

=== winconsts.q 0 1 44/48 ===
!Windows win32 constants

global const driverversion =  0
global const technology =  2
global const horzsize =  4
global const vertsize =  6
global const horzres =  8
global const vertres =  10
global const bitspixel =  12
global const bitplanes =  14
global const numbrushes =  16
global const numpens =  18
global const nummarkers =  20
global const numfonts =  22
global const numcolours =  24
global const pdevicesize =  26
global const curvecaps =  28
global const linecaps =  30
global const polygonalcaps =  32
global const textcaps =  34
global const clipcaps =  36
global const rastercaps =  38
global const aspectx =  40
global const aspecty =  42
global const aspectxy =  44
global const logpixelsx =  88
global const logpixelsy =  90
global const sizepalette =  104
global const numreserved =  106
global const colourres =  108
global const physicalwidth =  110
global const physicalheight =  111
global const physicaloffsetx =  112
global const physicaloffsety =  113
global const scalingfactorx =  114
global const scalingfactory =  115
global const fw_dontcare =  0
global const fw_thin =  100
global const fw_extralight =  200
global const fw_ultralight =  200
global const fw_light =  300
global const fw_normal =  400
global const fw_regular =  400
global const fw_medium =  500
global const fw_semibold =  600
global const fw_demibold =  600
global const fw_bold =  700
global const fw_extrabold =  800
global const fw_ultrabold =  800
global const fw_heavy =  900
global const fw_black =  900
global const cs_vredraw =  1
global const cs_hredraw =  2
global const cs_keycvtwindow =  4
global const cs_dblclks =  8
global const cs_owndc =  32
global const cs_classdc =  64
global const cs_parentdc =  128
global const cs_nokeycvt =  256
global const cs_noclose =  512
global const cs_savebits =  2048
global const cs_bytealignclient =  4096
global const cs_bytealignwindow =  8192
global const cs_publicclass =  16384
global const sw_hide =  0
global const sw_shownormal =  1
global const sw_normal =  1
global const sw_showminimized =  2
global const sw_showmaximized =  3
global const sw_maximize =  3
global const sw_shownoactivate =  4
global const sw_show =  5
global const sw_minimize =  6
global const sw_showminnoactive =  7
global const sw_showna =  8
global const sw_restore =  9
global const sw_showdefault =  10
global const sw_max =  10
global const pm_noremove =  0
global const pm_remove =  1
global const pm_noyield =  2
global const wm_null =  0
global const wm_create =  1
global const wm_destroy =  2
global const wm_move =  3
global const wm_size =  5
global const wm_activate =  6
global const wa_inactive =  0
global const wa_active =  1
global const wa_clickactive =  2
global const wm_setfocus =  7
global const wm_killfocus =  8
global const wm_enable =  10
global const wm_setredraw =  11
global const wm_settext =  12
global const wm_gettext =  13
global const wm_gettextlength =  14
global const wm_paint =  15
global const wm_close =  16
global const wm_queryendsession =  17
global const wm_quit =  18
global const wm_queryopen =  19
global const wm_erasebkgnd =  20
global const wm_syscolourchange =  21
global const wm_endsession =  22
global const wm_showwindow =  24
global const wm_wininichange =  26
global const wm_devmodechange =  27
global const wm_activateapp =  28
global const wm_fontchange =  29
global const wm_timechange =  30
global const wm_cancelmode =  31
global const wm_setcursor =  32
global const wm_mouseactivate =  33
global const wm_childactivate =  34
global const wm_queuesync =  35
global const wm_getminmaxinfo =  36
global const wm_drawitem =  43
global const wm_notify =  78
global const wm_contextmenu =  123
global const wm_geticon =  127
global const wm_seticon =  128
global const wm_nchittest =  132

global const wm_nclbuttondown	= 161
global const wm_nclbuttonup	= 162
global const wm_nclbuttondblclick	= 163

global const wm_menurbuttonup	= 290

global const wm_parentnotify =  528
global const wm_dropfiles =  563
global const wm_enteridle =  289
global const wm_user =  1024
global const wm_mdicreate =  544
global const wm_mdidestroy =  545
global const wm_mdiactivate =  546
global const wm_mdirestore =  547
global const wm_mdinext =  548
global const wm_mdimaximize =  549
global const wm_mditile =  550
global const wm_mdicascade =  551
global const wm_mdiiconarange =  552
global const wm_mdigetactive =  553
global const wm_mdisetmenu =  560
global const wm_entersizemove =  561
global const wm_exitsizemove =  562
global const wm_mdirefrshmenu =  564
global const wm_lbuttondblclk =  515
global const wm_rbuttondblclk =  518
global const wm_lbuttondown =  513
global const wm_rbuttondown =  516
global const wm_mbuttondown =  519
global const wm_mousemove =  512
global const wm_lbuttonup =  514
global const wm_rbuttonup =  517
global const wm_mbuttonup =  520
global const wm_mbuttondblclk =  521
global const wm_mousewheel =  522
global const snd_filename =  131072
global const snd_async =  1
global const dt_singleline =  32
global const dt_centre =  1
global const dt_vcentre =  4
global const ws_overlapped =  0
global const ws_popup =  2147483648
global const ws_child =  1073741824
global const ws_minimize =  536870912
global const ws_visible =  268435456
global const ws_disabled =  134217728
global const ws_clipsiblings =  67108864
global const ws_clipchildren =  33554432
global const ws_maximize =  16777216
global const ws_caption =  12582912
global const ws_border =  8388608
global const ws_dlgframe =  4194304
global const ws_hscroll =  1048576
global const ws_vscroll =  2097152
global const ws_sysmenu =  524288
global const ws_thickframe =  262144
global const ws_group =  131072
global const ws_tabstop =  0
global const ws_scrollbars =  3145728
global const ws_minimizebox =  131072
global const ws_maximizebox =  65536
global const ws_tiled =  0
global const ws_iconic =  536870912
global const ws_sizebox =  262144
global const ws_overlappedwindow =  13565952
global const ws_tiledwindow =  13565952
global const ws_popupwindow =  -2138570752
global const ws_childwindow =  1073741824
global const ws_ex_acceptfiles =  16
global const ws_ex_appwindow =  262144
global const ws_ex_clientedge =  512
global const ws_ex_contexthelp =  1024
global const ws_ex_controlparent =  65536
global const ws_ex_dlgmodalframe =  1
global const ws_ex_left =  0
global const ws_ex_leftscrollbar =  16384
global const ws_ex_ltrreading =  0
global const ws_ex_mdichild =  64
global const ws_ex_noparentnotify =  4
global const ws_ex_overlappedwindow =  768
global const ws_ex_palettewindow =  392
global const ws_ex_right =  4096
global const ws_ex_rightscrollbar =  0
global const ws_ex_rtlreading =  8192
global const ws_ex_staticedge =  131072
global const ws_ex_toolwindow =  128
global const ws_ex_topmost =  8
global const ws_ex_transparent =  32
global const ws_ex_windowedge =  256

global const gw_hwndfirst =  0
global const gw_hwndlast =  1
global const gw_hwndnext =  2
global const gw_hwndprev =  3
global const gw_owner =  4
global const gw_child =  5
global const gw_enabledpopup =  6
global const cb_geteditsel =  320
global const cb_limittext =  321
global const cb_seteditsel =  322
global const cb_addstring =  323
global const cb_deletestring =  324
global const cb_dir =  325
global const cb_getcount =  326
global const cb_getcursel =  327
global const cb_getlbtext =  328
global const cb_getlbtextlen =  329
global const cb_insertstring =  330
global const cb_resetcontent =  331
global const cb_findstring =  332
global const cb_findstringexact =  344
global const cb_selectstring =  333
global const cb_setcursel =  334
global const cb_showdropdown =  335
global const cb_getitemdata =  336
global const cb_setitemdata =  337
global const cb_getdroppedcontrolrect =  338
global const cb_setitemheight =  339
global const cb_getitemheight =  340
global const cb_setextendedui =  341
global const cb_getextendedui =  342
global const cb_getdroppedstate =  343
global const cb_setlocale =  345
global const cb_getlocale =  346
global const cb_gettopindex =  347
global const cb_settopindex =  348
global const cb_gethorizontalextent =  349
global const cb_sethorizontalextent =  350
global const cb_getdroppedwidth =  351
global const cb_setdroppedwidth =  352
global const cb_initstorage =  353
global const cb_multipleaddstring =  355
global const bm_click =  245
global const bm_getcheck =  240
global const bm_getimage =  246
global const bm_getstate =  242
global const bm_setcheck =  241
global const bm_setimage =  247
global const bm_setstate =  243
global const bm_setstyle =  244
global const cf_bitmap =  2
global const cf_dib =  8
global const cf_palette =  9
global const cf_enhmetafile =  14
global const cf_metafilepict =  3
global const cf_oemtext =  7
global const cf_text =  1			!used in sys
global const cf_unicodetext =  13
global const cf_dif =  5
global const cf_dspbitmap =  130
global const cf_dspenhmetafile =  142
global const cf_dspmetafilepict =  131
global const cf_dsptext =  129
global const cf_gdiobjfirst =  768
global const cf_gdiobjlast =  1023
global const cf_hdrop =  15
global const cf_locale =  16
global const cf_ownerdisplay =  128
global const cf_pendata =  10
global const cf_privatefirst =  512
global const cf_privatelast =  767
global const cf_riff =  11
global const cf_sylk =  4
global const cf_wave =  12
global const cf_tiff =  6

global const tcif_text =  1
global const tcif_image =  2
global const tcif_param =  8
global const tcif_rtlreading =  4

global const wm_keydown =  256
global const wm_keyup =  257
global const wm_char =  258
global const wm_syschar =  262
global const wm_sysdeadchar =  263
global const wm_syskeydown =  260
global const wm_syskeyup =  261
global const mf_insert =  0
global const mf_change =  128
global const mf_append =  256
global const mf_delete =  512
global const mf_remove =  4096
global const mf_bycommand =  0
global const mf_byposition =  1024
global const mf_separator =  2048
global const mf_enabled =  0
global const mf_grayed =  1
global const mf_greyed =  1
global const mf_disabled =  2
global const mf_unchecked =  0
global const mf_checked =  8
global const mf_usecheckbitmaps =  512
global const mf_string =  0
global const mf_bitmap =  4
global const mf_ownerdraw =  256
global const mf_popup =  16
global const mf_menubarbreak =  32
global const mf_menubreak =  64
global const mf_unhilite =  0
global const mf_hilite =  128
global const mf_sysmenu =  8192
global const mf_help =  16384
global const mf_mouseselect =  32768

!global const bn_clicked =  0
!global const bn_dblclk =  5
!global const bn_disable =  4
!global const bn_doubleclicked =  5
!global const bn_hilite =  2
!global const bn_killfocus =  7
!global const bn_paint =  1
!global const bn_pushed =  2
!global const bn_setfocus =  6
!global const bn_unhilite =  3
!global const bn_unpushed =  3
!global const en_setfocus =  256
!global const en_killfocus =  512
!global const en_change =  768
!global const en_update =  1024
!global const en_errspace =  1280
!global const en_maxtext =  1281
!global const en_hscroll =  1537
!global const en_vscroll =  1538
!global const lbn_errspace =  -2
!global const lbn_selchange =  1
!global const lbn_dblclk =  2
!global const lbn_selcancel =  3
!global const lbn_setfocus =  4
!global const lbn_killfocus =  5
!global const cbn_errspace =  -1
!global const cbn_selchange =  1
!global const cbn_dblclk =  2
!global const cbn_setfocus =  3
!global const cbn_killfocus =  4
!global const cbn_editchange =  5
!global const cbn_editupdate =  6
!global const cbn_dropdown =  7
!global const cbn_closeup =  8
!global const cbn_selendok =  9
!global const cbn_selendcancel =  10
!
!global const cbs_autohscroll =  64
!global const cbs_disablenoscroll =  2048
!global const cbs_dropdown =  2
!global const cbs_dropdownlist =  3
!global const cbs_hasstrings =  512
!global const cbs_lowercase =  16384
!global const cbs_nointegralheight =  1024
!global const cbs_oemconvert =  128
!global const cbs_ownerdrawfixed =  16
!global const cbs_ownerdrawvariable =  32
!global const cbs_simple =  1
!global const cbs_sort =  256
!global const cbs_uppercase =  8192

global const wm_command =  273
global const wm_menuselect =  287
global const wm_cut =  768
global const wm_copy =  769
global const wm_paste =  770
global const wm_clear =  771
global const wm_undo =  772
global const em_getsel =  176
global const em_setsel =  177
global const em_scroll =  181
global const em_linescroll =  182
global const em_scrollcaret =  183
global const em_getmodify =  184
global const em_setmodify =  185
global const em_getlinecount =  186
global const em_lineindex =  187
global const em_sethandle =  188
global const em_gethandle =  189
global const em_getthumb =  190
global const em_linelength =  193
global const em_replacesel =  194
global const em_getline =  196
global const em_limittext =  197
global const em_canundo =  198
global const em_undo =  199
global const em_fmtlines =  200
global const em_linefromchar =  201
global const em_settabstops =  203
global const em_setpasswordchar =  204
global const em_emptyundobuffer =  205
global const em_getfirstvisibleline =  206
global const em_setreadonly =  207
global const em_setwordbreakproc =  208
global const em_getwordbreakproc =  209
global const em_getpasswordchar =  210
global const em_setlimittext =  197
global const em_getseltext =  1086
global const em_setcharformat =  1092
global const em_getcharformat =  1082
global const em_settextmode =  1113
global const em_gettextmode =  1114
global const em_gettextex =  1118
global const em_gettextlengthex =  1119
global const tm_plaintext =  1
global const tm_richtext =  2
global const tm_singlelevelundo =  4
global const tm_multilevelundo =  8
global const tm_singlecodepage =  16
global const tm_multicodepage =  32
global const scf_word =  2
global const scf_selection =  1
global const sb_getborders =  1031
global const sb_getparts =  1030
global const sb_getrect =  1034
global const sb_gettextw =  1037
global const sb_gettextlengthw =  1036
global const sb_settextw =  1035
global const sb_gettexta =  1026
global const sb_gettextlengtha =  1027
global const sb_settexta =  1025
global const sb_gettext =  1026
global const sb_gettextlength =  1027
global const sb_settext =  1025
global const sb_setminheight =  1032
global const sb_setparts =  1028
global const sb_simple =  1033
global const wm_setfont =  48
global const wm_getfont =  49
global const gm_advanced =  2
global const transparent =  1
global const opaque =  2
global const mwt_identity =  1
global const cw_usedefault =  0x8000'0000
global const idc_arrow =  32512
global const idc_ibeam =  32513
global const idc_wait =  32514
global const idc_cross =  32515
global const idc_uparrow =  32516
global const idc_sizenwse =  32642
global const idc_sizenesw =  32643
global const idc_sizewe =  32644
global const idc_sizens =  32645
global const idc_sizeall =  32646
global const idc_no =  32648
global const idc_appstarting =  32650
global const idc_help =  32651
global const idi_application =  32512
global const idi_hand =  32513
global const idi_question =  32514
global const idi_exclamation =  32515
global const idi_asterisk =  32516
global const idi_winlogo =  32517
global const idc_size =  32640
global const idc_icon =  32641
global const arrowpointer =  32512
global const ibeampointer =  32513
global const waitpointer =  32514
global const crosspointer =  32515
global const uparrowpointer =  32516
global const sizenwsepointer =  32642
global const sizeneswpointer =  32643
global const sizewepointer =  32644
global const sizenspointer =  32645
global const sizeallpointer =  32646
global const nopointer =  32648
global const appstartingpointer =  32650
global const helpicon =  32651
global const applicationicon =  32512
global const handicon =  32513
global const questionicon =  32514
global const exclamationicon =  32515
global const asteriskicon =  32516
global const winlogoicon =  32517
global const sizepointer =  32640
global const iconicon =  32641
global const sm_cymin =  29
global const sm_cxmin =  28
global const sm_arrange =  56
global const sm_cleanboot =  67
global const sm_cmetrics =  76
global const sm_cmousebuttons =  43
global const sm_cxborder =  5
global const sm_cyborder =  6
global const sm_cxcursor =  13
global const sm_cycursor =  14
global const sm_cxdlgframe =  7
global const sm_cydlgframe =  8
global const sm_cxdoubleclk =  36
global const sm_cydoubleclk =  37
global const sm_cxdrag =  68
global const sm_cydrag =  69
global const sm_cxedge =  45
global const sm_cyedge =  46
global const sm_cxfixedframe =  7
global const sm_cyfixedframe =  8
global const sm_cxframe =  32
global const sm_cyframe =  33
global const sm_cxfullscreen =  16
global const sm_cyfullscreen =  17
global const sm_cxhscroll =  21
global const sm_cyhscroll =  3
global const sm_cxhthumb =  10
global const sm_cxicon =  11
global const sm_cyicon =  12
global const sm_cxiconspacing =  38
global const sm_cyiconspacing =  39
global const sm_cxmaximized =  61
global const sm_cymaximized =  62
global const sm_cxmaxtrack =  59
global const sm_cymaxtrack =  60
global const sm_cxmenucheck =  71
global const sm_cymenucheck =  72
global const sm_cxmenusize =  54
global const sm_cymenusize =  55
global const sm_cxminimized =  57
global const sm_cyminimized =  58
global const sm_cxminspacing =  47
global const sm_cyminspacing =  48
global const sm_cxmintrack =  34
global const sm_cymintrack =  35
global const sm_cxscreen =  0
global const sm_cyscreen =  1
global const sm_cxsize =  30
global const sm_cysize =  31
global const sm_cxsizeframe =  32
global const sm_cysizeframe =  33
global const sm_cxsmicon =  49
global const sm_cysmicon =  50
global const sm_cxsmsize =  52
global const sm_cysmsize =  53
global const sm_cxvscroll =  2
global const sm_cyvscroll =  20
global const sm_cyvthumb =  9
global const sm_cycaption =  4
global const sm_cykanjiwindow =  18
global const sm_cymenu =  15
global const sm_cysmcaption =  51
global const sm_dbcsenabled =  42
global const sm_debug =  22
global const sm_menudropalignment =  40
global const sm_mideastenabled =  74
global const sm_mousepresent =  19
global const sm_mousewheelpresent =  75
global const sm_network =  63
global const sm_penwindows =  41
global const sm_reserved1 =  24
global const sm_reserved2 =  25
global const sm_reserved3 =  26
global const sm_reserved4 =  27
global const sm_secure =  44
global const sm_showsounds =  70
global const sm_slowmachine =  73
global const sm_swapbutton =  23
global const arw_bottomleft =  0
global const arw_bottomright =  1
global const arw_hide =  8
global const arw_topleft =  2
global const arw_topright =  3
global const arw_down =  4
global const arw_left =  0
global const arw_right =  0
global const arw_up =  4
global const white_brush =  0
global const ltgray_brush =  1
global const gray_brush =  2
global const dkgray_brush =  3
global const black_brush =  4
global const null_brush =  5
global const hollow_brush =  5
global const white_pen =  6
global const black_pen =  7
global const null_pen =  8
global const oem_fixed_font =  10
global const ansi_fixed_font =  11
global const ansi_var_font =  12
global const system_font =  13
global const device_default_font =  14
global const default_palette =  15
global const system_fixed_font =  16
global const stock_last =  16

!global const sbm_setpos =  224
!global const sbm_getpos =  225
!global const sbm_setrange =  226
!global const sbm_setrangeredraw =  230
!global const sbm_getrange =  227
!global const sbm_enable_arrows =  228
!global const sbs_horz =  0
!global const sbs_vert =  1
!global const sbs_topalign =  2
!global const sbs_leftalign =  2
!global const sbs_bottomalign =  4
!global const sbs_rightalign =  4
!global const sbs_sizeboxtopleftalign =  2
!global const sbs_sizeboxbottomrightalign =  4
!global const sbs_sizebox =  8

global const wm_hscroll =  276
global const wm_vscroll =  277

!global const sb_horz =  0
!global const sb_hoz =  0
!global const sb_vert =  1
!global const sb_ctl =  2
!global const sb_both =  3
!global const sb_lineup =  0
!global const sb_lineleft =  0
!global const sb_linedown =  1
!global const sb_lineright =  1
!global const sb_pageup =  2
!global const sb_pageleft =  2
!global const sb_pagedown =  3
!global const sb_pageright =  3
!global const sb_thumbposition =  4
!global const sb_thumbtrack =  5
!global const sb_top =  6
!global const sb_left =  6
!global const sb_bottom =  7
!global const sb_right =  7
!global const sb_endscroll =  8
!global const sif_disablenoscroll =  8
!global const sif_page =  2
!global const sif_pos =  4
!global const sif_range =  1
!global const sif_trackpos =  16
!global const sif_all =  23

global const wm_ctlcolourmsgbox =  306
global const wm_ctlcolouredit =  307
global const wm_ctlcolourlistbox =  308
global const wm_ctlcolourbtn =  309
global const wm_ctlcolourdlg =  310
global const wm_ctlcolourscrollbar =  311
global const wm_ctlcolourstatic =  312
global const wm_timer =  275

global const srccopy =  13369376
global const srcpaint =  15597702
global const srcand =  8913094
global const srcinvert =  6684742
global const srcerase =  4457256

global const notsrccopy =  3342344
global const notsrcerase =  1114278
global const mergecopy =  12583114
global const mergepaint =  12255782
global const patcopy =  15728673
global const patpaint =  16452105
global const patinvert =  5898313
global const dstinvert =  5570569
global const blackness =  66
global const whiteness =  16711778

global const r2_black =  1
global const r2_notmergepen =  2
global const r2_masknotpen =  3
global const r2_notcopypen =  4
global const r2_maskpennot =  5
global const r2_not =  6
global const r2_xorpen =  7
global const r2_notmaskpen =  8
global const r2_maskpen =  9
global const r2_notxorpen =  10
global const r2_nop =  11
global const r2_mergenotpen =  12
global const r2_copypen =  13
global const r2_mergepennot =  14
global const r2_mergepen =  15
global const r2_white =  16
global const r2_last =  16

global const gdi_error =  4294967295
global const hgdi_error =  4294967295
global const clr_invalid =  4278190080
global const clr_default =  4278190080
global const clr_none =  4294967295
global const ofn_readonly =  1
global const ofn_overwriteprompt =  2
global const ofn_hidereadonly =  4
global const ofn_nochangedir =  8
global const ofn_showhelp =  16
global const ofn_enablehook =  32
global const ofn_enabletemplate =  64
global const ofn_enabletemplatehandle =  128
global const ofn_novalidate =  256
global const ofn_allowmultiselect =  512
global const ofn_extensiondifferent =  1024
global const ofn_pathmustexist =  2048
global const ofn_filemustexist =  4096
global const ofn_createprompt =  8192
global const ofn_shareaware =  16384
global const ofn_noreadonlyreturn =  32768
global const ofn_notestfilecreate =  65536
global const ofn_nonetworkbutton =  131072
global const ofn_nolongnames =  262144
global const ofn_explorer =  524288
global const ofn_nodereferencelinks =  1048576
global const ofn_longnames =  2097152
global const ofn_sharefallthrough =  2
global const ofn_sharenowarn =  1
global const ofn_sharewarn =  0
!global const gmem_fixed =  0
!global const gmem_moveable =  2
!global const gmem_nocompact =  16
!global const gmem_nodiscard =  32
!global const gmem_zeroinit =  64
!global const gmem_modify =  128
!global const gmem_discardable =  256
!global const gmem_not_banked =  4096
!global const gmem_share =  8192
!global const gmem_ddeshare =  8192
!global const gmem_notify =  16384
!global const gmem_lower =  4096
!global const gmem_valid_flags =  32626
!global const gmem_invalid_handle =  32768
!global const gmem_clipboard =  8194
!global const ghnd =  66
!global const gptr =  64
!global const pd_allpages =  0
!global const pd_collate =  16
!global const pd_disableprinttofile =  524288
!global const pd_enableprinthook =  4096
!global const pd_enableprinttemplate =  16384
!global const pd_enableprinttemplatehandle =  65536
!global const pd_enablesetuphook =  8192
!global const pd_enablesetuptemplate =  32768
!global const pd_enablesetuptemplatehandle =  131072
!global const pd_hideprinttofile =  1048576
!global const pd_nopagenums =  8
!global const pd_noselection =  4
!global const pd_nowarning =  128
!global const pd_pagenums =  2
!global const pd_printsetup =  64
!global const pd_printtofile =  32
!global const pd_returndc =  256
!global const pd_returndefault =  1024
!global const pd_returnic =  512
!global const pd_selection =  1
!global const pd_showhelp =  2048
!global const pd_usedevmodecopies =  262144
!global const pd_usedevmodecopiesandcollate =  262144
global const dib_rgb_colours =  0
global const dib_pal_colours =  1
global const dib_pal_indices =  2
global const dib_pal_physindices =  2
global const dib_pal_logindices =  4
global const stm_seticon =  368
global const stm_setimage =  370
global const lr_loadfromfile =  16
global const image_bitmap =  0
global const image_icon =  1
global const lr_copydeleteorg =  8
global const lr_copyreturnorg =  4
global const lr_monochrome =  1
global const lr_createdibsection =  8192
global const lr_defaultsize =  64
global const ss_icon =  3
global const ss_bitmap =  14
global const gcl_menuname =  -8
global const gcl_hbrbackground =  -10
global const gcl_hcursor =  -12
global const gcl_hicon =  -14
global const gcl_hmodule =  -16
global const gcl_cbwndextra =  -18
global const gcl_cbclsextra =  -20
global const gcl_wndproc =  -24
global const gcl_style =  -26
global const gcw_atom =  -32
global const colour_scrollbar =  0
global const colour_background =  1
global const colour_desktop =  1
global const colour_activecaption =  2
global const colour_inactivecaption =  3
global const colour_menu =  4
global const colour_window =  5
global const colour_windowframe =  6
global const colour_menutext =  7
global const colour_windowtext =  8
global const colour_captiontext =  9
global const colour_activeborder =  10
global const colour_inactiveborder =  11
global const colour_appworkspace =  12
global const colour_highlight =  13
global const colour_highlighttext =  14
global const colour_btnface =  15
global const colour_3dface =  15
global const colour_btnshadow =  16
global const colour_3dshadow =  16
global const colour_graytext =  17
global const colour_btntext =  18
global const colour_inactivecaptiontext =  19
global const colour_btnhighlight =  20
global const colour_3dhilight =  20
global const colour_3ddkshadow =  21
global const colour_3dlight =  22
global const colour_infotext =  23
global const colour_infobk =  24
global const colour_tooltipbk =  24
global const mk_lbutton =  1
global const mk_rbutton =  2
global const mk_shift =  4
global const mk_control =  8
global const mk_mbutton =  16
global const cbm_createdib =  2
global const cbm_init =  4
global const cc_enablehook =  16
global const cc_enabletemplate =  32
global const cc_enabletemplatehandle =  64
global const cc_fullopen =  2
global const cc_preventfullopen =  4
global const cc_rgbinit =  1
global const cc_showhelp =  8
global const cc_solidcolour =  128
global const cf_screenfonts =  1
global const cf_printerfonts =  2
global const cf_effects =  256
global const size_restored =  0
global const size_minimized =  1
global const size_maximized =  2
global const size_maxshow =  3
global const size_maxhide =  4
!global const gwl_wndproc =  -4
!global const gwl_hinstance =  -6
!global const gwl_hwndparent =  -8
!global const gwl_style =  -16
!global const gwl_exstyle =  -20
global const gwl_userdata =  -21
global const gwl_id =  -12
global const ta_top =  0
global const ta_left =  0
global const ta_noupdatecp =  0
global const ta_updatecp =  1
global const ta_right =  2
global const ta_centre =  6
global const vta_centre =  6
global const ta_bottom =  8
global const ta_baseline =  24
global const vta_baseline =  24
global const ta_rtlreading =  256
global const aligntop =  0
global const alignbottom =  8
global const alignbaseline =  24
global const aligncentre =  6
global const alignleft =  0
global const alignright =  2

global const em_exgetsel =  1076
global const em_exlimittext =  1077
global const em_exlinefromchar =  1078
global const em_exsetsel =  1079
global const em_getparaformat =  1085
global const em_setparaformat =  1095
global const em_streamin =  1097
global const em_streamout =  1098
global const em_gettextrange =  1099
global const em_findtext =  1080
global const em_findtextex =  1103

!global const ttf_idishwnd =  1
!global const ttf_centretip =  2
!global const ttf_rtlreading =  4
!global const ttf_subclass =  16
!global const ttf_track =  32
!global const ttf_absolute =  128
!global const ttf_transparent =  256
!global const ttf_di_setitem =  32768

global const hwnd_top =  0
global const hwnd_bottom =  1
global const hwnd_topmost =  -1
global const hwnd_notopmost =  -2

global const normalwind =  0
global const modalwind =  -1
global const dialogwind =  -2
global const minimize =  2
global const maximize =  3
global const shiftmask =  1
global const controlmask =  2
global const altmask =  4
global const windowcolour =  15
global const ps_geometric =  65536
global const ps_cosmetic =  0
global const ps_alternate =  8
global const ps_solid =  0
global const ps_dash =  1
global const ps_dot =  2
global const ps_dashdot =  3
global const ps_dashdotdot =  4
global const ps_null =  5
global const ps_insideframe =  6
global const ps_userstyle =  7
global const ps_endcap_round =  0
global const ps_endcap_square =  256
global const ps_endcap_flat =  512
global const ps_join_bevel =  4096
global const ps_join_miter =  8192
global const ps_join_round =  0
global const ps_style_mask =  15
global const ps_endcap_mask =  3840
global const ps_type_mask =  983040
global const bs_solid =  0
global const bs_hollow =  1
global const bs_null =  1
global const bs_hatched =  2
global const bs_pattern =  3
global const bs_dibpattern =  5
global const bs_dibpatternpt =  6
global const bs_pattern8x8 =  7
global const bs_dibpattern8x8 =  8
global const hs_horizontal =  0
global const hs_vertical =  1
global const hs_fdiagonal =  2
global const hs_bdiagonal =  3
global const hs_cross =  4
global const hs_diagcross =  5

!global const gl_points =  0
!global const gl_lines =  1
!global const gl_line_loop =  2
!global const gl_line_strip =  3
!global const gl_triangles =  4
!global const gl_triangle_strip =  5
!global const gl_triangle_fan =  6
!global const gl_quads =  7
!global const gl_quad_strip =  8
!global const gl_polygon =  9

global const spi_getworkarea =  48

proc start=
end

=== wingxlib.q 0 1 45/48 ===
!import winmessages
!import winconsts
!import gxmisc
!import winapi

!module winapi

export var hwapplic=nil
export var hwchild=nil
export var iswin32
export var screendc

export var nglobalfonts=0
export var fonttable::=()			![]font handles
export var fontdimtable::=()		![]rpoint (width,total line height)
export var fontvdimtable::=()		![]rpoint (ascenders, descenders) 

proc start	=
	initdata()
end

proc initdata=
!CPL "---------WINGXLIB"
	iswin32:=(getos()="W32")
	screendc:=getdc(nil)

	fonttable:=(0,)*20
	fontdimtable:=(0,)*20
	fontvdimtable:=(0,)*20

	fonttable[1]:=getstockobject(17)	!default gui
	fonttable[2]:=getstockobject(13)	!system font
	fonttable[3]:=getstockobject(16)	!system fixed
	fonttable[4]:=getstockobject(10)	!oem fixed
	for i:=1 to 4 do
		fontdimtable[i]::=ws_point(0,0)
		fontvdimtable[i]::=ws_point(0,0)
	od
	nglobalfonts:=4
end

func checkoption(optionnames,optionvalues,name,default=-1)=
!search for option with given name
!return value of option, or -1 if not present
!options (which can be void) will be a list of (name,value) list pairs

	n:=name in optionnames
	if not n then return default fi
	return optionvalues[n]
end

global proc wx_waitmess=
	windmsg:=new((iswin32|ws_msg32|ws_msg64))

	do
		if getmessage(&windmsg,nil,0,0)<>0 then
			w:=windmsg.hwnd
			if windmsg.message=wm_keydown and windmsg.wparam=27 then exit fi
			if windmsg.message=wm_timer then CPL "TIMER!!" fi
			translatemessage(&windmsg)
			dispatchmessage(&windmsg)
			if windmsg.message=wm_close then exit fi
		else
			exit
		fi
	od
end

global func wx_getw(hwnd)=
!return allwindow-index of window that has been stored into it
	n:=getwindowlongptr(hwnd, gwl_userdata)
	return n
end

global proc wx_setw(hwnd,index)=
!store mm window handle into win32 window
!index is .gindex (index into allwindows)
	setwindowlongptr(hwnd, gwl_userdata, index)
end

global func wx_gettextwidth(hdc,s)=
	size:=new(ws_point)
	gettextextentpoint32(hdc,s,s.len,&size)
	return size.x
end

global func wx_createpopup(?caption,?pos,?dim,?options,owner=nil)=
!wrapper around win32 createwindow
!return win32 handle to newly created window
	const gap=40
	const smallestwidth=150

	if options.isvoid then
 options:=[wf_caption:1,wf_border:wbs_resize]
	fi

	posx:=posy:=-1
	dimx:=640
	dimy:=480
	fcentre:=0
	fautopos:=0
	fmax:=fdesktop:=0
!	fconsole:=0

	if caption.isvoid then caption:="<No Caption>" fi

	if dim.defined then
		if dim.isstring and dim="max" then
			fmax:=1
		elsif dim.isstring and dim="desktop" then
			fdesktop:=1
!		elsif dim.isstring and dim="console" then
!			fconsole:=1
		else
			dimx:=dim[1]
			dimy:=dim[2]
		fi
	fi

	if pos.isvoid or pos="cent" then
		fcentre:=1
	elsif pos="auto" then
		fautopos:=1
	elsif pos.defined and not pos.isstring then
		posx:=pos[1]
		posy:=pos[2]
	else				!check options?
		abort("gxcw bad pos")
	fi

	bstyle:=bxstyle:=0
	nocap:=0			!whether to suppress caption

	framex:=framey:=0

	case options{wf_border,wbs_resize}
	when wbs_none then		!no border
		nocap:=1
		framex:=0
		framey:=0
	when wbs_simple then		!single line
		nocap:=1
		bstyle:=ws_border
		framex:=1
		framey:=1
	when wbs_thick then		!thick line
		bstyle:=ws_dlgframe
		fixedframe:=0
		framex:=getsystemmetrics(sm_cxfixedframe)
		framey:=getsystemmetrics(sm_cyfixedframe)
	when wbs_resize then
		bstyle:=ws_sizebox
		framex:=getsystemmetrics(sm_cxsizeframe)
		framey:=getsystemmetrics(sm_cysizeframe)
	when wbs_sunken,wbs_sunken2 then		!sunken
		bstyle:=ws_dlgframe
		bxstyle:=ws_ex_clientedge
		framex:=5
		framey:=5
	when wbs_sunkenrs then
		bstyle:=ws_sizebox
		bxstyle:=ws_ex_clientedge
		framex:=6
		framey:=6
	esac

	capheight:=getsystemmetrics(sm_cycaption)
	mbheight:=getsystemmetrics(sm_cymenu)

	style:=0
	exstyle:=0

	if options{wf_show,1} then
		style ior:=ws_visible
	fi

	mxleft:=framex
	mxright:=framey
	mytop:=framey+capheight
	mybottom:=framey
	showstyle:=sw_shownormal

	hcwmenu:=nil
	if options{wf_menu,0}=1 then
		mytop+:=mbheight
		hcwmenu:=createmenu()
		appendmenu(hcwmenu,0,998,"fred")
	fi

	style ior:=ws_clipchildren

	if nocap or options{wf_caption,1}=0 then
		mytop-:=capheight
		style ior:=ws_popup
	fi

	if options{wf_iframe,0}=0 then
		if not fautopos then
			posx-:=mxleft
			posy-:=mytop
		fi
		dimx+:=mxleft+mxright
		dimy+:=mytop+mybottom
	fi

	if fcentre or options{wf_cent,0}=1 then
		fautopos:=0
		box:=new(ws_rect)
		systemparametersinfoa(spi_getworkarea,0,&box,0)
		posx:=box.rightx%2-dimx%2
		posy:=(box.bottom-box.top)%2-dimy%2+box.top
	fi

	if fmax or options{wf_max,0} then
		showstyle:=sw_maximize
		style ior:=ws_maximize
	fi


	if options{wf_minmax,1}=1 then
		style ior:=(ws_maximizebox ior ws_minimizebox)

	fi

	if options{wf_sysmenu,1}=1 then
		style ior:=ws_sysmenu
	fi

	if fautopos=0 and options{wf_clip,0}=1 then
		box:=new(ws_rect)
		systemparametersinfoa(spi_getworkarea,0,&box,0)

		if posx<box.leftx+gap then posx:=box.leftx+gap fi

		if posy<box.top+gap then posy:=box.top+gap fi
		dimxmin:=dimx max smallestwidth
		if posx+dimxmin>=box.rightx+gap then posx:=box.rightx-gap-dimxmin fi
		if posy+dimy>=box.bottom+gap then posy:=box.bottom-gap-dimy fi
	elsif fautopos then
		posx:=posy:=cw_usedefault
	fi

	if fdesktop or options{wf_desktop,0}=1 then
		box:=new(ws_rect)
		systemparametersinfoa(spi_getworkarea,0,&box,0)
		posx:=box.leftx
		posy:=box.top
		dimx:=box.rightx-box.leftx
		dimy:=box.bottom-box.top
	fi

	if options{wf_toolwind,0}=1 then
		exstyle ior:=ws_ex_toolwindow
	fi

	classname:="pcc001"

	STYLE IOR:=WS_VISIBLE

	style ior:=bstyle
	exstyle ior:=bxstyle

!	if fconsole then
!		hwnd:=getconsolewindow()
!CPL "CONSOLE", =HWND
!STOP
!
!	else

		hwnd:=createwindowex(
			exstyle,
			classname,
			caption,
			style,
			posx,posy,			!initial position and size
			dimx,dimy,
			owner,			!will be 0 for 1st window, other popups use hwapplic as owner
			hcwmenu,			!menu handle
			nil,	!proginstance,		!instance handle
			nil)			!creation params
!	fi

	if hwnd=nil then
		e:=getlasterror()
		abort("wx:Can't create popup window "+tostr(e))
	fi
	return hwnd
end

global func wx_createcontrol(?pos,?dim,border=wbs_simple,owner)=
!wrapper around win32 createwindow
!return win32 handle to newly created window
	const gap=40
	const smallestwidth=150

	posx:=posy:=0
	dimx:=160
	dimy:=120

	if dim.defined then
		dimx:=dim[1]
		dimy:=dim[2]
	fi

	if pos.defined then
		posx:=pos[1]
		posy:=pos[2]
	fi

	bstyle:=bxstyle:=0

	case border
	when wbs_none then			!no border
	when wbs_simple then		!single line
		bstyle:=ws_border
	else
		pcerror("createcontrol/bad border "+wbsnames[border])
	esac

	style:=0
	exstyle:=0

	style ior:=ws_clipchildren

	classname:="pcc001"

	style ior:=ws_child
	style ior:=ws_visible

	style ior:=bstyle
	exstyle ior:=bxstyle

	hwnd:=createwindowex(
		exstyle,
		classname,
		nil,
		style,
		posx,posy,			!initial position and size
		dimx,dimy,
		owner,				!will be 0 for 1st window, other popups use hwapplic as owner
		nil,				!menu handle
		nil,
		nil)				!creation params

	if hwnd=0 then
		e:=getlasterror()
		abort("wx:Can't create child window "+tostr(e))
	fi

	return hwnd
end

=== winmessages.q 0 1 46/48 ===
export var winmessagenames=[
	(0:"wm_null"),
	(1:"wm_create"),
	(2:"wm_destroy"),
	(3:"wm_move"),
	(4:"pgk_menu"),
	(5:"wm_size"),
	(6:"wm_activate"),
	(7:"wm_setfocus"),
	(8:"wm_killfocus"),
	(9:"cbn_selendok"),
	(10:"wm_enable"),
	(11:"wm_setredraw"),
	(12:"wm_settext"),
	(13:"wm_gettext"),
	(14:"wm_gettextlength"),
	(15:"wm_paint"),
	(16:"wm_close"),
	(17:"wm_queryendsession"),
	(18:"wm_quit"),
	(19:"wm_queryopen"),
	(20:"wm_erasebkgnd"),
	(21:"wm_syscolorchange"),
	(22:"wm_endsession"),
	(24:"wm_showwindow"),
	(26:"wm_wininichange"),
	(27:"wm_devmodechange"),
	(28:"wm_activateapp"),
	(29:"wm_fontchange"),
	(30:"wm_timechange"),
	(31:"wm_cancelmode"),
	(32:"wm_setcursor"),
	(33:"wm_mouseactivate"),
	(34:"wm_childactivate"),
	(35:"wm_queuesync"),
	(36:"wm_getminmaxinfo"),
	(38:"wm_painticon"),
	(39:"wm_iconerasebkgnd"),
	(40:"wm_nextdlgctl"),
	(42:"wm_spoolerstatus"),
	(43:"wm_drawitem"),
	(44:"wm_measureitem"),
	(45:"wm_deleteitem"),
	(46:"wm_vkeytoitem"),
	(47:"wm_chartoitem"),
	(48:"wm_setfont"),
	(49:"wm_getfont"),
	(50:"wm_sethotkey"),
	(51:"wm_gethotkey"),
	(55:"wm_querydragicon"),
	(57:"wm_compareitem"),
	(64:"tbif_size"),
	(65:"wm_compacting"),
	(70:"wm_windowposchanging"),
	(71:"wm_windowposchanged"),
	(72:"wm_power"),
	(74:"wm_copydata"),
	(75:"wm_canceljournal"),
	(78:"wm_notify"),
	(80:"wm_inputlangchangerequest"),
	(81:"wm_inputlangchange"),
	(82:"wm_tcard"),
	(83:"wm_help"),
	(84:"wm_userchanged"),
	(85:"wm_notifyformat"),
	(123:"wm_contextmenu"),
	(124:"wm_stylechanging"),
	(125:"wm_stylechanged"),
	(126:"wm_displaychange"),
	(127:"wm_geticon"),
	(128:"wm_seticon"),
	(129:"wm_nccreate"),
	(130:"wm_ncdestroy"),
	(131:"wm_nccalcsize"),
	(132:"wm_nchittest"),
	(133:"wm_ncpaint"),
	(134:"wm_ncactivate"),
	(135:"wm_getdlgcode"),
	(160:"wm_ncmousemove"),
	(161:"wm_nclbuttondown"),
	(162:"wm_nclbuttonup"),
	(163:"wm_nclbuttondblclk"),
	(164:"wm_ncrbuttondown"),
	(165:"wm_ncrbuttonup"),
	(166:"wm_ncrbuttondblclk"),
	(167:"wm_ncmbuttondown"),
	(168:"wm_ncmbuttonup"),
	(169:"wm_ncmbuttondblclk"),
	(176:"em_getsel"),
	(177:"em_setsel"),
	(178:"em_getrect"),
	(179:"em_setrect"),
	(180:"em_setrectnp"),
	(181:"em_scroll"),
	(182:"em_linescroll"),
	(183:"em_scrollcaret"),
	(184:"em_getmodify"),
	(185:"em_setmodify"),
	(186:"em_getlinecount"),
	(187:"em_lineindex"),
	(188:"em_sethandle"),
	(189:"em_gethandle"),
	(190:"em_getthumb"),
	(193:"em_linelength"),
	(194:"em_replacesel"),
	(196:"em_getline"),
	(197:"em_setlimittext"),
	(198:"em_canundo"),
	(199:"em_undo"),
	(200:"em_fmtlines"),
	(201:"em_linefromchar"),
	(203:"em_settabstops"),
	(204:"em_setpasswordchar"),
	(205:"em_emptyundobuffer"),
	(206:"em_getfirstvisibleline"),
	(207:"em_setreadonly"),
	(208:"em_setwordbreakproc"),
	(209:"em_getwordbreakproc"),
	(210:"em_getpasswordchar"),
	(211:"em_setmargins"),
	(212:"em_getmargins"),
	(213:"em_getlimittext"),
	(214:"em_posfromchar"),
	(215:"em_charfrompos"),
	(224:"sbm_setpos"),
	(225:"sbm_getpos"),
	(226:"sbm_setrange"),
	(227:"sbm_getrange"),
	(228:"sbm_enable_arrows"),
	(230:"sbm_setrangeredraw"),
	(233:"sbm_setscrollinfo"),
	(234:"sbm_getscrollinfo"),
	(240:"bm_getcheck"),
	(241:"bm_setcheck"),
	(242:"bm_getstate"),
	(243:"bm_setstate"),
	(244:"bm_setstyle"),
	(245:"bm_click"),
	(246:"bm_getimage"),
	(247:"bm_setimage"),
	(255:"wm_input"),
	(256:"wm_keydown"),
	(257:"wm_keyup"),
	(258:"wm_char"),
	(259:"wm_deadchar"),
	(260:"wm_syskeydown"),
	(261:"wm_syskeyup"),
	(262:"wm_syschar"),
	(263:"wm_sysdeadchar"),
	(269:"wm_ime_startcomposition"),
	(270:"wm_ime_endcomposition"),
	(271:"wm_ime_composition"),
	(272:"wm_initdialog"),
	(273:"wm_command"),
	(274:"wm_syscommand"),
	(275:"wm_timer"),
	(276:"wm_hscroll"),
	(277:"wm_vscroll"),
	(278:"wm_initmenu"),
	(279:"wm_initmenupopup"),
	(287:"wm_menuselect"),
	(288:"wm_menuchar"),
	(289:"wm_enteridle"),
	(290:"wm_menurbuttonup"),
	(295:"wm_changeuistate"),
	(296:"wm_updateuistate"),
	(297:"wm_queryuistate"),
	(306:"wm_ctlcolormsgbox"),
	(307:"wm_ctlcoloredit"),
	(308:"wm_ctlcolorlistbox"),
	(309:"wm_ctlcolorbtn"),
	(310:"wm_ctlcolordlg"),
	(311:"wm_ctlcolorscrollbar"),
	(312:"wm_ctlcolorstatic"),
	(320:"cb_geteditsel"),
	(321:"cb_limittext"),
	(322:"cb_seteditsel"),
	(323:"cb_addstring"),
	(324:"cbem_deleteitem"),
	(325:"cb_dir"),
	(326:"cb_getcount"),
	(327:"cb_getcursel"),
	(328:"cb_getlbtext"),
	(329:"cb_getlbtextlen"),
	(330:"cb_insertstring"),
	(331:"cb_resetcontent"),
	(332:"cb_findstring"),
	(333:"cb_selectstring"),
	(334:"cb_setcursel"),
	(335:"cb_showdropdown"),
	(336:"cb_getitemdata"),
	(337:"cb_setitemdata"),
	(338:"cb_getdroppedcontrolrect"),
	(339:"cb_setitemheight"),
	(340:"cb_getitemheight"),
	(341:"cb_setextendedui"),
	(342:"cb_getextendedui"),
	(343:"cb_getdroppedstate"),
	(344:"cb_findstringexact"),
	(345:"cb_setlocale"),
	(346:"cb_getlocale"),
	(347:"cb_gettopindex"),
	(348:"cb_settopindex"),
	(349:"cb_gethorizontalextent"),
	(350:"cb_sethorizontalextent"),
	(351:"cb_getdroppedwidth"),
	(352:"cb_setdroppedwidth"),
	(353:"cb_initstorage"),
	(368:"stm_seticon"),
	(369:"stm_geticon"),
	(370:"stm_setimage"),
	(371:"stm_getimage"),
	(384:"lb_addstring"),
	(385:"lb_insertstring"),
	(386:"lb_deletestring"),
	(387:"lb_selitemrangeex"),
	(388:"lb_resetcontent"),
	(389:"lb_setsel"),
	(390:"lb_setcursel"),
	(391:"lb_getsel"),
	(392:"lb_getcursel"),
	(393:"lb_gettext"),
	(394:"lb_gettextlen"),
	(395:"lb_getcount"),
	(396:"lb_selectstring"),
	(397:"lb_dir"),
	(398:"lb_gettopindex"),
	(399:"lb_findstring"),
	(400:"lb_getselcount"),
	(401:"lb_getselitems"),
	(402:"lb_settabstops"),
	(403:"lb_gethorizontalextent"),
	(404:"lb_sethorizontalextent"),
	(405:"lb_setcolumnwidth"),
	(406:"lb_addfile"),
	(407:"lb_settopindex"),
	(408:"lb_getitemrect"),
	(409:"lb_getitemdata"),
	(410:"lb_setitemdata"),
	(411:"lb_selitemrange"),
	(412:"lb_setanchorindex"),
	(413:"lb_getanchorindex"),
	(414:"lb_setcaretindex"),
	(415:"lb_getcaretindex"),
	(416:"lb_setitemheight"),
	(417:"lb_getitemheight"),
	(418:"lb_findstringexact"),
	(421:"lb_setlocale"),
	(422:"lb_getlocale"),
	(423:"lb_setcount"),
	(424:"lb_initstorage"),
	(425:"lb_itemfrompoint"),
	(512:"wm_mousemove"),
	(513:"wm_lbuttondown"),
	(514:"wm_lbuttonup"),
	(515:"wm_lbuttondblclk"),
	(516:"wm_rbuttondown"),
	(517:"wm_rbuttonup"),
	(518:"wm_rbuttondblclk"),
	(519:"wm_mbuttondown"),
	(520:"wm_mbuttonup"),
	(521:"wm_mbuttondblclk"),
	(522:"wm_mousewheel"),
	(523:"wm_xbuttondown"),
	(524:"wm_xbuttonup"),
	(525:"wm_xbuttondblclk"),
	(528:"wm_parentnotify"),
	(529:"wm_entermenuloop"),
	(530:"wm_exitmenuloop"),
	(531:"wm_nextmenu"),
	(532:"wm_sizing"),
	(533:"wm_capturechanged"),
	(534:"wm_moving"),
	(536:"wm_powerbroadcast"),
	(537:"wm_devicechange"),
	(544:"wm_mdicreate"),
	(545:"wm_mdidestroy"),
	(546:"wm_mdiactivate"),
	(547:"wm_mdirestore"),
	(548:"wm_mdinext"),
	(549:"wm_mdimaximize"),
	(550:"wm_mditile"),
	(551:"wm_mdicascade"),
	(552:"wm_mdiiconarrange"),
	(553:"wm_mdigetactive"),
	(560:"wm_mdisetmenu"),
	(561:"wm_entersizemove"),
	(562:"wm_exitsizemove"),
	(563:"wm_dropfiles"),
	(564:"wm_mdirefreshmenu"),
	(641:"wm_ime_setcontext"),
	(642:"wm_ime_notify"),
	(643:"wm_ime_control"),
	(644:"wm_ime_compositionfull"),
	(645:"wm_ime_select"),
	(646:"wm_ime_char"),
	(656:"wm_ime_keydown"),
	(657:"wm_ime_keyup"),
	(673:"wm_mousehover"),
	(675:"wm_mouseleave"),
	(689:"wm_wtssession_change"),
	(768:"wm_cut"),
	(769:"wm_copy"),
	(770:"wm_paste"),
	(771:"wm_clear"),
	(772:"wm_undo"),
	(773:"wm_renderformat"),
	(774:"wm_renderallformats"),
	(775:"wm_destroyclipboard"),
	(776:"wm_drawclipboard"),
	(777:"wm_paintclipboard"),
	(778:"wm_vscrollclipboard"),
	(779:"wm_sizeclipboard"),
	(780:"wm_askcbformatname"),
	(781:"wm_changecbchain"),
	(782:"wm_hscrollclipboard"),
	(783:"wm_querynewpalette"),
	(784:"wm_paletteischanging"),
	(785:"wm_palettechanged"),
	(786:"wm_hotkey"),
	(791:"wm_print"),
	(792:"wm_printclient"),
	(896:"wm_penwinirst"),
	(911:"wm_penwinlast"),
	(1024:"infotipsize"),
	(1025:"cbem_insertitema"),
	(1026:"cbem_setimagelist"),
	(1027:"cbem_getimagelist"),
	(1028:"cbem_getitema"),
	(1029:"cbem_setitema"),
	(1030:"cbem_getcombocontrol"),
	(1031:"cbem_geteditcontrol"),
	(1032:"cbem_setexstyle"),
	(1033:"cbem_getextendedstyle"),
	(1034:"cbem_haseditchanged"),
	(1035:"cbem_insertitemw"),
	(1036:"cbem_setitemw"),
	(1037:"cbem_getitemw"),
	(1038:"cbem_setextendedstyle"),
	(1039:"ttm_getcurrenttoola"),
	(1040:"ttm_windowfrompoint"),
	(1041:"ttm_trackactivate"),
	(1042:"ttm_trackposition"),
	(1043:"ttm_settipbkcolor"),
	(1044:"ttm_settiptextcolor"),
	(1045:"ttm_getdelaytime"),
	(1046:"ttm_gettipbkcolor"),
	(1047:"ttm_gettiptextcolor"),
	(1048:"ttm_setmaxtipwidth"),
	(1049:"ttm_getmaxtipwidth"),
	(1050:"ttm_setmargin"),
	(1051:"ttm_getmargin"),
	(1052:"ttm_pop"),
	(1053:"tb_getitemrect"),
	(1054:"tb_buttonstructsize"),
	(1055:"tb_setbuttonsize"),
	(1056:"tb_setbitmapsize"),
	(1057:"tb_autosize"),
	(1059:"tb_gettooltips"),
	(1060:"tb_settooltips"),
	(1061:"tb_setparent"),
	(1063:"tb_setrows"),
	(1064:"tb_getrows"),
	(1065:"tb_getbitmapflags"),
	(1066:"tb_setcmdid"),
	(1067:"tb_changebitmap"),
	(1068:"tb_getbitmap"),
	(1069:"tb_getbuttontexta"),
	(1070:"tb_replacebitmap"),
	(1071:"tb_setindent"),
	(1072:"tb_setimagelist"),
	(1073:"tb_getimagelist"),
	(1074:"ttm_addtoolw"),
	(1075:"ttm_deltoolw"),
	(1076:"ttm_newtoolrectw"),
	(1077:"ttm_gettoolinfow"),
	(1078:"ttm_settoolinfow"),
	(1079:"ttm_hittestw"),
	(1080:"ttm_gettextw"),
	(1081:"ttm_updatetiptextw"),
	(1082:"ttm_enumtoolsw"),
	(1083:"ttm_getcurrenttoolw"),
	(1084:"tb_setmaxtextrows"),
	(1085:"tb_gettextrows"),
	(1086:"em_getseltext"),
	(1087:"em_hideselection"),
	(1088:"em_pastespecial"),
	(1089:"em_requestresize"),
	(1090:"em_selectiontype"),
	(1091:"tb_insertbuttonw"),
	(1092:"tb_addbuttonsw"),
	(1093:"tb_hittest"),
	(1094:"em_setolecallback"),
	(1095:"em_setparaformat"),
	(1096:"em_settargetdevice"),
	(1097:"em_streamin"),
	(1098:"em_streamout"),
	(1099:"tb_getbuttontextw"),
	(1100:"tb_saverestorew"),
	(1101:"tb_addstringw"),
	(1102:"em_getoptions"),
	(1103:"tb_getinsertmark"),
	(1104:"tb_setinsertmark"),
	(1105:"tb_insertmarkhittest"),
	(1106:"tb_movebutton"),
	(1107:"tb_getmaxsize"),
	(1108:"tb_setextendedstyle"),
	(1109:"tb_getextendedstyle"),
	(1110:"tb_getpadding"),
	(1111:"tb_setpadding"),
	(1112:"tb_setinsertmarkcolor"),
	(1113:"tb_getinsertmarkcolor"),
	(1114:"tb_mapacceleratorw"),
	(1124:"em_setpunctuation"),
	(1125:"wm_choosefont_setlogfont"),
	(1126:"wm_choosefont_setflags"),
	(1127:"udm_setpos"),
	(1128:"udm_getpos"),
	(1129:"udm_setbuddy"),
	(1130:"udm_getbuddy"),
	(1131:"udm_setaccel"),
	(1132:"udm_getaccel"),
	(1133:"udm_setbase"),
	(1134:"udm_getbase"),
	(1135:"psm_settitlea"),
	(1136:"psm_setwizbuttons"),
	(1137:"psm_pressbutton"),
	(1138:"psm_setcurselid"),
	(1139:"psm_setfinishtexta"),
	(1140:"psm_gettabcontrol"),
	(1141:"psm_isdialogmessage"),
	(1142:"psm_getcurrentpagehwnd"),
	(1144:"psm_settitlew"),
	(1145:"psm_setfinishtextw"),
	(1157:"dl_begindrag"),
	(1158:"dl_dragging"),
	(1159:"dl_dropped"),
	(1160:"dl_canceldrag"),
	(1280:"en_errspace"),
	(1281:"en_maxtext"),
	(1537:"en_hscroll"),
	(1538:"en_vscroll"),
	(1792:"en_msgfilter"),
	(1793:"en_requestresize"),
	(1794:"en_selchange"),
	(1795:"en_dropfiles"),
	(1796:"en_protected"),
	(1797:"en_correcttext"),
	(1798:"en_stopnoundo"),
	(1799:"en_imechange"),
	(1800:"en_saveclipboard"),
	(1801:"en_oleopfailed"),
	(4096:"lvm_getbkcolor"),
	(4097:"lvm_setbkcolor"),
	(4098:"lvm_getimagelist"),
	(4099:"lvm_setimagelist"),
	(4100:"lvm_getitemcount"),
	(4101:"lvm_getitema"),
	(4102:"lvm_setitema"),
	(4103:"lvm_insertitema"),
	(4104:"lvm_deleteitem"),
	(4105:"lvm_deleteallitems"),
	(4106:"lvm_getcallbackmask"),
	(4107:"lvm_setcallbackmask"),
	(4108:"lvm_getnextitem"),
	(4109:"lvm_finditema"),
	(4110:"lvm_getitemrect"),
	(4111:"lvm_setitemposition"),
	(4112:"lvm_getitemposition"),
	(4113:"lvm_getstringwidtha"),
	(4114:"lvm_hittest"),
	(4115:"lvm_ensurevisible"),
	(4116:"lvm_scroll"),
	(4117:"lvm_redrawitems"),
	(4118:"lvm_arrange"),
	(4119:"lvm_editlabela"),
	(4120:"lvm_geteditcontrol"),
	(4121:"lvm_getcolumna"),
	(4122:"lvm_setcolumna"),
	(4123:"lvm_insertcolumna"),
	(4124:"lvm_deletecolumn"),
	(4125:"lvm_getcolumnwidth"),
	(4126:"lvm_setcolumnwidth"),
	(4129:"lvm_createdragimage"),
	(4130:"lvm_getviewrect"),
	(4131:"lvm_gettextcolor"),
	(4132:"lvm_settextcolor"),
	(4133:"lvm_gettextbkcolor"),
	(4134:"lvm_settextbkcolor"),
	(4135:"lvm_gettopindex"),
	(4136:"lvm_getcountperpage"),
	(4137:"lvm_getorigin"),
	(4138:"lvm_update"),
	(4139:"lvm_setitemstate"),
	(4140:"lvm_getitemstate"),
	(4141:"lvm_getitemtexta"),
	(4142:"lvm_setitemtexta"),
	(4143:"lvm_setitemcount"),
	(4144:"lvm_sortitems"),
	(4145:"lvm_setitemposition32"),
	(4146:"lvm_getselectedcount"),
	(4147:"lvm_getitemspacing"),
	(4148:"lvm_getisearchstringa"),
	(4171:"lvm_getitemw"),
	(4172:"lvm_setitemw"),
	(4173:"lvm_insertitemw"),
	(4179:"lvm_finditemw"),
	(4183:"lvm_getstringwidthw"),
	(4191:"lvm_getcolumnw"),
	(4192:"lvm_setcolumnw"),
	(4193:"lvm_insertcolumnw"),
	(4211:"lvm_getitemtextw"),
	(4212:"lvm_setitemtextw"),
	(4213:"lvm_getisearchstringw"),
	(4214:"lvm_editlabelw"),
	(4352:"tvm_insertitema"),
	(4353:"tvm_deleteitem"),
	(4354:"tvm_expand"),
	(4356:"tvm_getitemrect"),
	(4357:"tvm_getcount"),
	(4358:"tvm_getindent"),
	(4359:"tvm_setindent"),
	(4360:"tvm_getimagelist"),
	(4361:"tvm_setimagelist"),
	(4362:"tvm_getnextitem"),
	(4363:"tvm_selectitem"),
	(4364:"tvm_getitema"),
	(4365:"tvm_setitema"),
	(4366:"tvm_editlabela"),
	(4367:"tvm_geteditcontrol"),
	(4368:"tvm_getvisiblecount"),
	(4369:"tvm_hittest"),
	(4370:"tvm_createdragimage"),
	(4371:"tvm_sortchildren"),
	(4372:"tvm_ensurevisible"),
	(4373:"tvm_sortchildrencb"),
	(4374:"tvm_endeditlabelnow"),
	(4375:"tvm_getisearchstringa"),
	(4402:"tvm_insertitemw"),
	(4414:"tvm_getitemw"),
	(4415:"tvm_setitemw"),
	(4416:"tvm_getisearchstringw"),
	(4417:"tvm_editlabelw"),
	(4608:"hdm_getitemcount"),
	(4609:"hdm_insertitema"),
	(4610:"hdm_deleteitem"),
	(4611:"hdm_getitema"),
	(4612:"hdm_setitema"),
	(4613:"hdm_layout"),
	(4614:"hdm_hittest"),
	(4618:"hdm_insertitemw"),
	(4619:"hdm_getitemw"),
	(4620:"hdm_setitemw"),
	(4864:"tcm_first"),
	(4866:"tcm_getimagelist"),
	(4867:"tcm_setimagelist"),
	(4868:"tcm_getitemcount"),
	(4869:"tcm_getitema"),
	(4870:"tcm_setitema"),
	(4871:"tcm_insertitema"),
	(4872:"tcm_deleteitem"),
	(4873:"tcm_deleteallitems"),
	(4874:"tcm_getitemrect"),
	(4875:"tcm_getcursel"),
	(4876:"tcm_setcursel"),
	(4877:"tcm_hittest"),
	(4878:"tcm_setitemextra"),
	(4904:"tcm_adjustrect"),
	(4905:"tcm_setitemsize"),
	(4906:"tcm_removeimage"),
	(4907:"tcm_setpadding"),
	(4908:"tcm_getrowcount"),
	(4909:"tcm_gettooltips"),
	(4910:"tcm_settooltips"),
	(4911:"tcm_getcurfocus"),
	(4912:"tcm_setcurfocus"),
	(4924:"tcm_getitemw"),
	(4925:"tcm_setitemw"),
	(4926:"tcm_insertitemw"),
	(5120:"pgm_first"),
	(8192:"ccm_first")]

proc start=
end
=== gxmisc.q 0 1 47/48 ===
export enumdata optionnames =
	(wf_border,		$),		! wbs_simple
	(wf_resize,		$),		! 0
	(wf_hscroll,	$),		! 0
	(wf_vscroll,	$),		! 0
	(wf_menu,		$),		! 0
	(wf_caption,	$),		! 1
	(wf_max,		$),		! 0
	(wf_minmax,		$),		! 1
	(wf_sysmenu,	$),		! 1
	(wf_desktop,	$),		! 0
	(wf_clip,		$),		! 0
	(wf_show,		$),		!
	(wf_iframe,		$),		! 1
	(wf_cent,		$),		!
	(wf_toolwind,	$)		!
end

!Windows border styles, used for pop-up windows. Could also be used for
!some child windows
export enumdata wbsnames=
	(wbs_none=0,$),
	(wbs_simple,$),
	(wbs_thick,$),
	(wbs_resize,$),
	(wbs_sunken,$),
	(wbs_sunken2,$),
	(wbs_sunkenrs,$),
	(wbs_dummy,$)
end
=== dates.q 0 1 48/48 ===

export var daynames=("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

export var Monthnames=("January","February","March","April","May","June","July",
		"August","September","October","November","December")

export var days=(31,28,31, 30,31,30, 31,31,30, 31,30,31)

export record rdate=
	var day,month,year
end

export record rdatetime = 
	var	day
	var	month
	var	year
	var	hour
	var	minute
	var	second
	var	milliseconds
	var	dayofweek
end

!proc start=
!end
!
!proc main=
!end

export func makedatetime(d,m,y, h=0, minute=0, s=0)=

	d:=rdatetime(d,m,y, h,minute,s,0,0)
	d.dayofweek:=getdow(d)
	return d
end

export proc setdow(&d)=
	d.dayofweek:=getdow(d)
end

export func strdate(d,sep="-")=
!return leftstr(daynames[d.dayofweek],3)+" "+tostr(d.day)+sep+leftstr(monthnames[d.month],3)+sep+tostr(d.year)
	return tostr(d.day)+sep+leftstr(monthnames[d.month],3)+sep+tostr(d.year)
end

export func strtime(d,sep=":")=
	return tostr(d.hour)+sep+tostr(d.minute,"z2")+sep+tostr(d.second,"z2")
end

export func strdow(d,n=0)=
	if n then
		return leftstr(daynames[d.dayofweek],n)
	else
		return daynames[d.dayofweek]
	fi
end

export func strdatetime(d,dsep="-",tsep=":")=
	return strdate(d,dsep)+" "+strtime(d,tsep)
end

export func parsedate(s,defdate)=
!parse string s into a new date record
!def = default date to work from, eg. for missing year
!return date record obtained, or 0 if error

	day:=defdate.day
	month:=defdate.month
	year:=defdate.year
	if s.[1]=" " then s:=rightstr(s,-1) fi

	sepset:=[' ', '-', '/', '.']

	seppos:=0
	for i:=1 to s.len do if s.[i] in sepset then seppos:=i; exit fi od

	if not seppos then		!day only
		day:=strtoval(s)
		goto gotday
	fi
	day:=strtoval(leftstr(s,seppos-1))

	s:=rightstr(s,-seppos)		!month and possible year
	seppos:=0
	for i:=1 to s.len do if s.[i] in sepset then seppos:=i; exit fi od

	if seppos then
		monthstr:=leftstr(s,seppos-1)
		yearstr:=rightstr(s,s.len-seppos)
	else
		monthstr:=s
		yearstr:=""
	fi

	if asc(leftstr(monthstr)) in ['0'..'9'] then	!numeric month
		month:=strtoval(monthstr)
		if month<1 or month>12 then
			return 0
		fi
	else
		month:=0
		for i:=1 to 12 do
			if convlc(leftstr(monthnames[i],3))=convlc(leftstr(monthstr,3)) then
				month:=i
				exit
			fi
		od
		if not month then
			return 0
		fi
	fi

	if yearstr<>"" then
		year:=strtoval(yearstr)
		if year<200 then
			if year in [00..89] then
				year+:=2000
			else
				year+:=1900
			fi
		fi
	fi

gotday:
!check the date, rather than correct using addday(d,0)
	dd:=days[month] 
	if leapyear(year) and month=2 then dd+:=1 fi
	if day<1 or day>dd then return 0 fi
	if year<1990 or year>2089 then return 0 fi
	return makedatetime(day,month,year)
end

export func leapyear(y)=
!return true if y (eg. 1994) is a leap year
	return (y-1900) rem 4=0
end

export func getdow(d)=
!return day of week for given date, returning 1..7 (monday..sunday)
	return ((getday(d)-1) rem 7)+1
end

export func getday(d)=
!return day number for date d, measured from 1.1.90
	day:=0
	for i:=1990 to d.year-1 do
		day+:=(leapyear(i)|366|365)
	od

	for i:=1 to d.month-1 do
		day+:=(i=2|(leapyear(d.year)|29|28)|days[i])
	od
	day+:=d.day
	return day
end

export func getdays(m,y)=
!return no. of days in month m, for year y
	if leapyear(y) and m=2 then return 29 fi
	return days[m]
end

export func getmonthname(m,?n)=
	if not m.isint then
		m:=m.month
	fi
	m:=monthnames[m]
	if n.defined then m:=leftstr(m,n) fi
	return m
end

export func getdayname(d,?n)=
	if not d.isint then
		d:=getdow(d)
	fi
	d:=daynames[d]
	if n.defined then d:=leftstr(d,n) fi
	return d
end

export func addday(d0,i)=
	d:=d0
	if i>0 then
		to i do
			++d.day
			if d.day>getdays(d.month,d.year) then
				d.day:=1
				++d.month
				if d.month>12 then
					d.month:=1
					++d.year
				fi
			fi
		od
	else
		to -i do
			--d.day
			if d.day<1 then
				--d.month
				if d.month<1 then
					d.month:=12
					--d.year
				fi
				d.day:=getdays(d.month,d.year)
			fi
		od
	fi

!do checking
	if d.year<1990 then d:=makedatetime(1,1,1990) fi
	if d.year>2089 then d:=makedatetime(31,12,2089) fi

	dd:=getdays(d.month,d.year)
	if leapyear(d.year) and d.month=2 then dd+:=1 fi
	if d.day<1 then d.day:=1 fi
	if d.day>dd then d.day:=dd fi
	setdow(d)
	return d
end

export func getdatetime=
	tm:=getsystime()

	return rdatetime(tm.day,tm.month,tm.year,
			tm.hour, tm.minute, tm.second, tm.milliseconds,tm.dayofweek)
end

export func getsystime=
	tm:=new(ws_systemtime)
	getsystemtime(&tm)

	if tm.dayofweek=0 then
		tm.dayofweek:=7
	fi

	return tm
end
=== END ===
1 qqp.m 0 0
2 qq_cli.m 0 0
3 qq_arrays.m 0 0
4 qq_bits.m 0 0
5 qq_calldll.m 0 0
6 qq_decls.m 0 0
7 qq_decimal.m 0 0
8 qq_dicts.m 0 0
9 qq_host.m 0 0
10 qq_lex.m 0 0
11 qq_lib.m 0 0
12 qq_lists.m 0 0
13 qq_modules.m 0 0
14 qq_names.m 0 0
15 qq_packed.m 0 0
16 qq_parse.m 0 0
17 qq_pcltabs.m 0 0
18 qq_pclgen.m 0 0
19 qq_pcllib.m 0 0
20 qq_print.m 0 0
21 qq_records.m 0 0
22 qq_resolve.m 0 0
23 qq_run.m 0 0
24 qq_runaux.m 0 0
25 qq_sets.m 0 0
26 qq_strings.m 0 0
27 qq_syslibs.m 0 0
28 qq_tables.m 0 0
29 qq_dummyshow.m 0 0
30 qq_showpcldummy.m 0 0
31 qq_vars.m 0 0
32 syswin.q 0 1
33 syslin.q 0 1
34 sysp.q 0 1
35 windows.q 0 1
36 linux.q 0 1
37 clibp.q 0 1
38 smlib.q 0 1
39 winapi.q 0 1
40 gxlib.q 0 1
41 bmlib.q 0 1
42 console.q 0 1
43 lincon.q 0 1
44 winconsts.q 0 1
45 wingxlib.q 0 1
46 winmessages.q 0 1
47 gxmisc.q 0 1
48 dates.q 0 1
