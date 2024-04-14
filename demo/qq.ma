=== MA 31 ===
=== qq.m 0 0 1/31 ===
project =
	module qq_cli
!	module qq_embed
	module qq_api
!	module qq_cli
	module qq_arrays
	module qq_bits
	module qq_calldll

	module qq_decimal

	module qq_decls
	module qq_dicts

	module qq_jhandlers
!	module qq_jhandlers_hll
!
	module qq_khandlers
	module qq_host
	module qq_lex
	module qq_lib
	module qq_lists
	module qq_modules
	module qq_names

	module qq_optim
!	module qq_optimdummy

	module qq_packed
	module qq_parse
	module qq_print
	module qq_pclgen
	module qq_pcllib
	module qq_records
	module qq_resolve
	module qq_sets
	module qq_strings

!	module qq_switch

!	module qq_syslibs
	module qq_syslibsdummy

	module qq_tables

	module qq_show
!	module qq_dummyshow
!
	module qq_vars
end
=== qq_cli.m 0 0 2/31 ===

int cmdstartindex

proc main=
	ichar source
	int i,nnames,t,tstart, stopcode
	unit p

!CPL =UNITREC.BYTES
!CPL =klastpcl
!STOP

	initdata()

	getinputoptions()

!	if fverbose then
!		println dispatchnames[dispatchtype],"Opt:",foptimise
!	fi

	readqabundle()
	loadsyslib()

	compile_sp(inputfile)

	if fallsp then
		if fshowast1 and runcode=parse_cc then showast(nil, "AST1") fi
		if fshowast2 and runcode>parse_cc then showast(nil, "AST2") fi
		if (fshowpcl1 or fshowpcl2) and runcode=gencode_cc then showpcl(nil, 1) fi
		if fshowpcl3 and runcode=fixup_cc then showpcl(nil, 3) fi
	fi

!run the stack of sps (laters sps will be run as they are compiled)

	writeqafile()

!	for i to nsubprogs do
!		stopcode:=runprogram(subprogs[i])
!	od
	stopcode:=runqprogram(subprogs[nsubprogs])

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
		println "Q5.2 Interpreter"
		println "Usage:"
		println "	",,sysparams[1],"filename[.q]"
		stop
	fi

	if dispatchtype in [debug_dispatch,fdebug_dispatch] then
		hasbytecodes:=1
	else
		hasbytecodes:=0
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

	case sw
	when asmopt_sw then
		foptimise:=1
		dispatchtype:=asm_dispatch
	esac

end

=== qq_api.m 0 0 3/31 ===
!global const syslibname="qlib"
global const syslibname="sysp"
!global const syslibname="minsys"

global enumdata []ichar runnames =
	(load_cc,		$),
	(parse_cc,		$),
	(names_cc,		$),
	(gencode_cc,	$),
!	(optim_cc,		$),
	(fixup_cc,		$),
	(run_cc,		$),
end

global byte fshowpcl1
global byte fshowpcl2
global byte fshowpcl3
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

global const maxstatic=11000
global [maxstatic]variant statictable
global [maxstatic]symbol staticdefs
global int nstatics

global const maxproc=11000				!used for fixups
!global const maxproc=50000
global [maxproc]ref int proctable
global [maxproc]symbol procdefs
global int nprocs

ref strbuffer pclstr

global proc compile_sp(ichar filename, source=nil)=
	ichar qafile
	isubprog sp
	int a, b

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

	if foptimise and dispatchtype=asm_dispatch then
		for m in a..b do
			optimise_module(m)
		od
		if fshowpcl2 and not fallsp then showpcl(sp,2) fi
	fi

	fixup_sp(sp)

	resetcompiler()
end

global proc setcli(ref []ichar cmds, int ncmds)=
	for i to ncmds do
		setcmdparam(i,cmds[i])
	od
end

proc fixup_sp(isubprog sp)=
	return when runcode<fixup_cc

	for i:=sp.firstmodule to sp.lastmodule do
		fixupmodule(modules[i])
	od

!	if fshowpcl3 and not fallsp then showpcl(sp,3) fi
end

global proc writeqafile=
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

global proc initdata=
	lexinit()
!	inithostlib()

	stprogram:=createdupldef(nil,addnamestr("$prog"),programid)

	os_initwindows()
	if dispatchtype=asm_dispatch then
		initjhandlers()
	fi

	firstusertype:=tlast+1

	deletetempfiles()

end

proc fixproc(symbol d)=
	ref int z
	variant p

	if not d.procfixed then
		if nprocs>=maxproc then gerror("Too many procs") fi
		z:=modules[d.moduleno].pcstart+d.labelno
		p:=pcm_alloc(varsize)
		proctable[++nprocs]:=z
		procdefs[nprocs]:=d
		d.pcaddress:=z
		d.procfixed:=1
	fi

end

proc fixupmodule(ifile pm)=
	ref int pc,pcstart,z
	int cmd,y
	symbol d
	variant p

	pc := pcstart := pm.pcstart

	repeat
		cmd:=pc^

		pc^:=cast(cmdmap[cmd])			!convert cmd index to labeladdr/functionaddr/same index

		++pc

		case cmd
		when kprocdef then
			fixproc(cast(pc^))
		esac

		for i to pclnopnds[cmd] do
			case pclfmt[cmd,i]
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
				ref stringrec ps:=cast(pc^)
				pc^:=cast(obj_make_stringn(ps.svalue,ps.length,0))

			when clabel then
				y:=int(pcstart+pc^)
				pc^:=y

			end
			++pc
		od
	until cmd=kendmodule
end

global function runqprogram(isubprog sp)int=
	ref proc fnptr
	int cmd,SS
	ref int pcstart

	return 0 when runcode<run_cc

	if fverbose then
		println dispatchnames[dispatchtype],"Opt:",foptimise
	fi

	sptr:=&varstack[1]
	stacklimit:=&varstack[stacksize-100]
	pcstart:=pcptr:=modules[sp.firstmodule].pcstart
	pcerrorpos:=0
	stopped:=0

	disploop()

	return sptr.value
end

proc disploop=

	case dispatchtype
	when fn_dispatch then
		disploop_fn()

	when debug_dispatch then
		disploop_deb(0)

	when fdebug_dispatch then
		disploop_deb(1)

	when asm_dispatch then
		disploop_asm()

!	when sw_dispatch then
!		disploop_sw()
!
	else
		loaderror("Dispatch not supported: ##",dispatchnames[dispatchtype])
	esac
end

proc disploop_fn=
	type fnptr = ref proc

	repeat
		(fnptr(pcptr^))^()
	until stopped
end

proc disploop_deb(int fdeb)=
	locrec loc

	fdebug:=fdeb	

	repeat
		if fdebug then
			loc:=getpcerrorpos(pcptr)
			print loc.pm.name:"13jl",loc.lineno:"6",$
			println pclnames[pcptr^],=pcptr,=SPTR,TTNAME[SPTR.TAG]
		fi
		++pclcounts[pcptr^]

		pclhandlers[pcptr^]^()

	until stopped
end

global proc setcmdmap=
	if dispatchtype=asm_dispatch and not asmavailable() then
		dispatchtype:=fn_dispatch
	fi

	for i:=1 to klastpcl do
		case dispatchtype
		when fn_dispatch then
			cmdmap[i]:=pclhandlers[i]
		when debug_dispatch, fdebug_dispatch, sw_dispatch then
			cmdmap[i]:=cast(i)
		when asm_dispatch then
			cmdmap[i]:=jhandlertable[i]
		esac
	od
end

global function runproc_m(ref void amsg)int=
	varrec a,b,dest
	static int rmsg_typeno
	int i,result
	objrec obj

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

	memset(&obj,0,objrec.bytes)
	obj.refcount:=99
	obj.ptr:=ref byte(amsg)
	obj.usertag:=rmsg_typeno

	a.tagx:=tstruct ior hasrefmask
	a.objptr:=&obj

	runproc(pcl_callbackfn,&a,nil,&dest)
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
	ref int oldpcptr
	byte oldstopped
	int nparams

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

	disploop()


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

proc resetcompiler=
!called at end of compilesp so to reset globals for next sp

!should really recover any resources used here
	nuserxtypes:=0
	userxtypebase:=0
	ref userxrec userxmodelist:=nil
	CLEAR TTXMAP				!later limit to old nuserxtypes for efficiency

	firstusertype:=ntypes+1
end

global proc loadsyslib=
	[300]char str

	setcmdmap()

	if fnosys then return fi

	if not fsyslibs then usebundled:=0 fi

	if usebundled then				!bundled sys files
		compile_sp(syslibname+".q")
	else
		strcpy(str, devdir)
		strcat(str, syslibname+".q")
		compile_sp(str)
	fi

	if runcode=run_cc and not fwriteqa then
		runqprogram(subprogs[1])
	fi
end
=== qq_arrays.m 0 0 4/31 ===
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

!	var_unshare(&v)
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

!global proc var_putslice_array(variant a, int i,j, variant x)=
!!insert a substring into a
!	ref byte r,s
!	object p,q
!	int length,sublength,elemsize
!
!	if a.tag=tvector then
!		pcerror("userax/putslice")
!	fi
!	p:=a.objptr
!	if not p.mutable then pcnotmut() fi
!	length:=p.length
!
!	if i<1 or j>p.length or i>j then
!		pcerror("array/slice bounds")
!	fi
!
!	sublength:=j-i+1
!
!	q:=x.objptr
!	if q.length<sublength then
!		pcerror("substr too short")
!	fi
!	if p.elemtag<>q.elemtag then
!		pcerror("Not compat")
!	fi
!	elemsize:=ttsize[p.elemtag]
!
!	r:=p.ptr+(i-1)*elemsize
!	s:=q.ptr
!	memcpy(r,s,sublength*elemsize)
!end

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

global function u16inarray(word16 a,object p)int=
	int i
	ref word16 q

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

global function u32inarray(word32 a,object p)int=
	int i
	ref word32 q

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

global function u64inarray(word64 a,object p)int=
	int i
	ref word64 q

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
=== qq_bits.m 0 0 5/31 ===
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

PCERROR_S("VAR/BITS/NOT READY",$FUNCTION)
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
!global proc obj_resize_bits(object p,int n)=
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

!global proc var_putslice_bits(variant a, int i,j, variant x)=
!!insert a substring into a
!	ref byte pp,qq
!	object p,q
!	int length,sublength,elemtype,offsetp,offsetq,bitwidthx
!	varrec v
!
!	p:=a.objptr
!	if not p.mutable then pcnotmut() fi
!	length:=p.length
!	elemtype:=p.elemtag
!
!	if i<1 or j>p.length or i>j then
!		pcerror("bits/slice bounds")
!	fi
!
!	sublength:=j-i+1
!
!	q:=x.objptr
!	if q.length<sublength then
!		pcerror("substr too short")
!	fi
!	if p.elemtag<>q.elemtag then
!		pcerror("Not compat")
!	fi
!
!	bitwidthx:=ttbitwidth[elemtype]
!
!	pp:=getindexoffset(p.ptr, p.indexoffset, i-p.lower, elemtype, offsetp)
!	qq:=getindexoffset(q.ptr, q.indexoffset, 0, elemtype, offsetq)
!	offsetq*:=bitwidthx
!	offsetq*:=bitwidthx
!
!	to sublength do
!		var_loadbit(qq, offsetq, elemtype,0, &v)
!		var_storebit(pp, offsetp, &v, elemtype,0)
!		offsetp+:=bitwidthx
!		if offsetp>=8 then ++pp; offsetp:=0 fi
!		offsetq+:=bitwidthx
!		if offsetq>=8 then ++qq; offsetq:=0 fi
!	od	
!end

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
=== qq_calldll.m 0 0 6/31 ===
global proc calldll(symbol d, variant args, result, int nargs)=
	symbol e
	const maxparams=100
	[maxparams]int arglist
	int n, retcode, retval,fnindex,libindex
	word dllinst
	ref proc fnaddr
	ichar name

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
		dest.value:=int32(retval)
	when tu32 then
		dest.tagx:=tint
		dest.value:=word32(retval)
	when ti16 then
		dest.tagx:=tint
		dest.value:=int16(retval)
	when tu16 then
		dest.tagx:=tint
		dest.value:=word16(retval)
	when trefpack then
		dest.tagx:=trefpack
		dest.ptr:=cast(retval)
		dest.elemtag:=tttarget[t]
	when tstringz then
		var_make_string(cast(retval), dest)

	else
		pcerror_s("Rettype not supported:",ttname[t])
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
			pcerror_s("Can't load DLL:",libtable[libindex].name)
		fi
		dllinsttable[libindex]:=dllinst
	fi

	name:=(d.truename|d.truename|d.name)
!CPL "LOADING DLL FN",NAME
	fnaddr:=os_getdllprocaddr(dllinst,name)

	if fnaddr=nil then
		pcerror_s("Can't find DLL func:",name)
	fi
!CPL "LOADED DLL FUNC",NAME,"AT", FNADDR
	dllprocaddr[fnindex]:=fnaddr

	return fnaddr
end

=== qq_decimal.m 0 0 7/31 ===
const digitwidth   = 9
const digitbase	= 1000000000
const digitfmt	 = "%09d"
const mdigitfmt	 = "z9"

const digitmax	 = digitbase-1

global type elemtype = int32
global const decelemsize = elemtype.bytes

record constrec =
	int64 value
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
	int64 x,scale

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
!CPL "OBLLENDEC",=BN_ISINT(A),=BN_ISZERO(A),=A.UDEC.LENGTH,FPNAMES[A.UDEC.NUMTYPE],=A.UDEC.NUMTYPE

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

!CPL "MAKE BIG NUM IS ZERO",A.UDEC.EXPON
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
!			x:=int64((c+cx1)^)+p rem digitbase
			x:=int64((c+cx1)^)+pr

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
	int64 xx,y
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
	int64 xx,y
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

function bn_makefloat(real64 x)object =
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

proc bn_ipower(object d, a,int64 n)=
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

function bn_toint(object a)int64=
	int64 x
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

=== qq_decls.m 0 0 8/31 ===
!global const fixbytecodes=1		!convert bytecodes to handler addresses

!global int dispatchtype=fn_dispatch
global byte dispatchtype=asm_dispatch
global int hasbytecodes=1			!depends on dispatchcode

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

global record packfieldrec =
	object structobj			!owner record
	ichar name
	int32 packmode				!index into tables
	int32 offset				!byte offset
	int32 size					!size
	int32 length
end

global record procrec =			!used as linked list
	symbol def
	ref procrec nextproc
end

global record userxrec =
	symbol owner
	ref int16 pmode
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
		unit code				!proc body/initdata
		ref symbol topfieldlist		!structs; point to block of ttlength[mode] top fields
	end
	union
		u64 c
		struct
			int32 index			!frame/param/dllproc/enum/(const)
			int32 capindex		!localid index
		end
	end
	union
		u64 d
		struct
			int16 nparams		!procs/dllprocs
			int16 nlocals		!procs
!			int16 ncaptured		!anonprocs
		end
		struct
			int16 nfields		!records/structs
			int16 maxalign		!structs
!			int32 fieldoffset
			int16 fieldoffset
			int16 baseclassindex		!into baseclass tables
		end
		int genfieldindex		!generic
	end

	word16	subcode
	byte	moduleno
	byte	subprogno
	int16	mode
	int16	hint				!0/tvoid, or hinted mode when .mode=tvar
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
		int64 value				!64-bit int
		real xvalue				!64-bit float
		word64 uvalue			!64-bit word
		ichar svalue			!pointer to string or charconst (not terminated)
		ref strec symptr			!pointer to symbol table entry for name
	end

	int32 pos: (sourceoffset:24, moduleno:8)

	byte symbol
	byte subcode
	word16 slength
end

global record uflagsrec =
	[7]byte	codes
	byte	ulength
end

global record fieldrec =
	ichar name
	int16 recordtype
	int16 fieldtype
	int32 fieldoffset
end

global record unitrec =
	union
		struct
			int16 tag
			union
				byte elemtype			!for array constructors
			end
			union
				byte nparams
				byte enumindex
			end
			int32 pos: (sourceoffset:24, moduleno:8)
		end
		ref void word1
	end

	unit nextunit

	union
		unit a
		symbol def
		symbol labeldef
		int64 value
		word64 uvalue
		real64 xvalue
		ichar svalue
		int64 range_lower
		int pclopcode
	end

	union
		unit b
		int64 range_upper
		int64 slength
		int16 mode
		[4]byte cmpgenop
		struct
			int32 length
			int32 lower
		end
		int64 index		!of enum name; or host index; or could be expr
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
!global const stacksize=50'000'000
global [stacksize]varrec varstack
global variant sptr
global variant stacklimit
global ref byte frameptr

global ref int pcptr

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
	int column				!if not zero, then column number
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

global ichar err_message
global varrec err_var1, err_var2
global ref int err_pcptr

global ref int stopseq		!point to a 'stop 0' sequence
global ref int raiseseq		!point to a sequence of several 'raise' cmdcodes

global ref procrec proclist, proclistx
global int nproclist

global ref proc pcl_callbackfn=nil	!address of *PCL* function (pcdata address)

global [0..255]object chrtable		!remember single-character objects

global byte fnosys
global byte fverbose

global [0:256]int16 baseclasstable
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

!	int16	subprogno
	byte	subprogno
	byte	islead				!1 if lead module in sp
	union
		int16	moduleno			!useful if using pointer to a source rec
		int16	fileno
	end

	unit	ast					!ast for module-level code

	ref int	pcstart				!nil, or points to generated bytecode for whole module
	ref int	pcend				!points to last allocated int
	int		pcsize				!pcl size as number of allocated ints (some spare)
	ref i32	pcsrcstart			!each entry is source-pos info (char offset into org source)

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
	int16 firstmodule			!1st is lead module
	int16 lastmodule			!always first..lastmodule
	int16 compiled				!1 if compiled
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
						byte	genmarker		!1 means is a generator used as a marker
					end
				end
				word32		tagx
			end
			union
				word32 		elemtag
				word32 		frameptr_low
				struct
					i16		frameoffset
					i16		nexceptions
				end
			end
		end
		i64 dummy: (skip:16, range_lower:48)
	end
	union
		int64		value
		real64		xvalue
		word64		uvalue
		word64		range_upper
		object		objptr				!objects where hasref=1
		variant		varptr				!for refvar
		ref byte	ptr					!for refproc etc
		symbol		def					!for tsymbol
		ref int		retaddr
	end
end

export record objrec =
!1st 8 bytes
	word32 refcount
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
				int64		value
				real64		xvalue
				word64		uvalue
				ichar		strptr
				variant		varptr
				variant		genstack
				ref byte	ptr
				ref[0:]elemtype num
				word64 b
				ref int		retaddr
			end

!3rd 8 bytes
			union
				int64 length
				int64 lower64
				struct
					word32 rows
					word32 columns
				end
				word64 c
				ref byte frameptr
!				symbol		stgen
				struct
					int32 iterpos
					int32 iterupper
				end
			end

!4th 8 bytes (and end of long objects)
			union
				int64 alloc64				!object/item counts, not bytes
				object objptr2
				struct
					int16 neg
					int16 numtype
					int32 expon
				end
				struct
					word32 alloc32
					word32 dictitems
				end
				struct
					u16		genstacksize		!in varrecs
					byte	ngenparams			!params to gen func
				end
				word64 d
			end
		end
		[24]byte bignumdescr
	end
end

global [pclnames.lwb..pclnames.upb]int pclcounts

global record stringrec=
	ichar svalue
	int length
end

global int nalllines

global const devdir = "c:/qx52/"

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
	(pcl3_sw,		"pcl3",			&fshowpcl3,			1),

	(allsp_sw,		"allsp",		&fallsp,			1),

	(st_sw,			"st",			&fshowst,			1),
	(stflat_sw,		"stflat",		&fshowstflat,		1),
	(types_sw,		"types",		&fshowtypes,		1),
	(showmodules_sw,"modules",		&fshowmodules,		1),

	(fn_sw,			"fn",			&dispatchtype,		fn_dispatch),
	(asm_sw,		"asm",			&dispatchtype,		asm_dispatch),
	(debug_sw,		"debug",		&dispatchtype,		debug_dispatch),
	(fdebug_sw,		"fdebug",		&dispatchtype,		fdebug_dispatch),
	(sw_sw,			"sw",			&dispatchtype,		sw_dispatch),

	(opt_sw,		"opt",			&foptimise,			1),
	(asmopt_sw,		"asmopt",		nil,				0),

	(ext_sw,		"ext",			&usebundled,		0),
	(qa_sw,			"qa",			&fwriteqa,			1),
	(qas_sw,		"qas",			&fwriteqa,			2),

	(verbose_sw,	"v",			&fverbose,			1),
!	(time_sw,		"time",			&fshowtime,			1),

	(nosys_sw,		"nosys",		&fnosys,			1),
end
=== qq_dicts.m 0 0 9/31 ===
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
	int64 keyvalue
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
=== qq_jhandlers.m 0 0 10/31 ===
!ASM bytecode handlers

!int showasmflag=1
int showasmflag=2
!int showasmflag=0

const kopnda	= 8
const kopndb	= 16
const kopndc	= 24
const kopndd	= 32

const ktag			= varrec.tag
const khasref		= varrec.hasref
const krefelemtag	= varrec.elemtag
const kelemtag		= varrec.elemtag
const kvarptr		= varrec.varptr
const kobjptr		= varrec.objptr
const kptr			= varrec.ptr
const kvalue		= varrec.value
const kxvalue		= varrec.xvalue
const kretaddr		= varrec.retaddr
const kframeptr_low	= varrec.frameptr_low
!const krange_low	= varrec.range_lower
!const krange_high	= varrec.range_upper

const jrefcount		= objrec.refcount
const jmutable		= objrec.flags			!byte flags; bit 0=mutable flag
const jusertag		= objrec.usertag
const jvarptr		= objrec.varptr
const jlength		= objrec.length
const jlower16		= objrec.lower16
const jobjptr2		= objrec.objptr2

const gdef			= genfieldrec.def
const gnextdef		= genfieldrec.nextdef
const sowner		= strec.owner
const smode			= strec.mode
const snameid		= strec.nameid
const soffset		= strec.fieldoffset

const varshift		= 4

!offsets from the top of the stack, of x, x/y, or x/y/z operands
!the x operand is first pushed, so has a variable offset
!the a suffix means top of stack; b is next; c is last

const xa		= 0				! one operand x

const xb		= -varsize		! two operands x y
const ya		= 0

const xc		= -varsize*2	! three operands x y z
const yb		= -varsize
const za		= 0


const intpsize	= 8

const intpsize2	= intpsize*2
const intpsize4	= intpsize*4
const intpsize6	= intpsize*6

macro jumpnext = asm jmp [Dprog]

!macro jumpnext   =
!	assem
!		*saveregs
!		call showasmcmd
!		*loadregs
!
!		jmp [Dprog]
!	end
!
macro saveregs   =
	assem
		mov [pcptr],Dprog
		mov [sptr],Dsptr
		mov [frameptr],Dframe
	end

!macro saveregs   =
!	assem
!		mov [pcptr],Dprog
!		mov [sptr],Dsptr
!		mov [frameptr],Dframe
!		call showasmcmd
!	end

macro loadregs   =
	assem
		mov Dprog,[pcptr]
		mov Dsptr,[sptr]
		mov Dframe,[frameptr]
	end

macro pushvar    = asm add Dsptr, varsize

macro popvar     = asm sub Dsptr, varsize

macro pushvar2   = asm add Dsptr, varsize+varsize
macro popvar2    = asm sub Dsptr, varsize+varsize

macro pushvar3   = asm add Dsptr, varsize+varsize+varsize
macro popvar3    = asm sub Dsptr, varsize+varsize+varsize

macro jumpskip1  =
	assem
		add Dprog,8
		*jumpnext
	end

macro jumpskip2  =
	assem
		add Dprog,16
		*jumpnext
	end

macro jumpskip3  =
	assem
		add Dprog,24
		*jumpnext
	end

macro jumpskip4  =
	assem
		add Dprog,32
		*jumpnext
	end

macro jumpskip5  =
	assem
		add Dprog,40
		*jumpnext
	end

macro jumpskip6  =
	assem
		add Dprog,48
		*jumpnext
	end

macro loadskip1  =
	assem
		mov Dsptr,[sptr]
		mov Dframe,[frameptr]
		mov Dprog,[pcptr]
		add Dprog,8
		*jumpnext
	end

macro loadskip2  =
	assem
		mov Dsptr,[sptr]
		mov Dframe,[frameptr]
		mov Dprog,[pcptr]
		add Dprog,16
		*jumpnext
	end

macro callunshareu_d4    =
	assem
		mov D10,D4
		*saveregs
		call var_unshareu
		*loadregs
	end

macro callunshareu_dsptr =
	assem
		mov D10, Dsptr
		*saveregs
		call var_unshareu
		*loadregs
	end

macro callunshareu =
	assem
		*saveregs
		call var_unshareu
		*loadregs
	end

macro callduplu_dsptr =
	assem
		mov D10, Dsptr
		*saveregs
		call var_duplu
		*loadregs
	end

macro callvarfree =
	assem
		*saveregs
		call var_free
		*loadregs
	end

global [0..pclnames.upb]ref proc jhandlertable

proc showasmcmd=
	if showasmflag<2 then return fi
	println "showasmcmd",showasmflag,strpcptr(), pcptr, sptr, ttname[sptr.tag]
end

global proc initjhandlers=
	ichar name
	static int handlersdone=0

	if handlersdone then return fi

	for i to $getnprocs() do
		name:=$getprocname(i)
		if eqbytes(name,"j_",2) then
			for k:=0 to pclnames.upb do
				if eqstring(name+2,pclnames[k]+1) then		!skip "j_" and "k"
					jhandlertable[k]:=$getprocaddr(i)
					exit
				fi
			else
				pcerror_s("Unknown j-handler",name)
			od
		fi
	od

	for i in jhandlertable.bounds when jhandlertable[i]=nil do
		jhandlertable[i]:=cast(junimpl)
	od

	handlersdone:=1
end

global function asmavailable:int =
	 1
end

function strpcptr:ichar=
	for i in jhandlertable.bounds do
		if jhandlertable[i]=cast(pcptr^,ref proc) then
			return pclnames[i]
		fi
	od
	return "<not found>"
end

global function disploop_asm:ref int =
	disploop()
	return nil
end

threadedproc disploop*=
!Note: this is reentrant

	assem
		push D9
		push D8
		push D7
		push D6

		push D5
		push D4
		push D3
		push Dframe

		sub dstack,40

		*loadregs
MOV DFRAME,[FRAMEPTR]
		*jumpnext
	end

stoplabel:

	assem
		add dstack, 40

		pop Dframe
		pop D3
		pop D4
		pop D5

		pop D6
		pop D7
		pop D8
		pop D9
	end
end

threadedproc junimpl*=
	pcerror("-asm Unimplemented (use -debug to see opcode)")
end

threadedproc j_comment*=
	saveregs

!	skip(1)
	pcptr+:=2

	loadregs
	jumpnext
end

threadedproc j_nop*=
	saveregs
	k_nop()
	loadregs
	jumpnext
end

threadedproc j_procentry*=
	assem
		mov A3,[Dprog+kopnda]
loop1:
		*pushvar
		mov word32 [Dsptr+ktag],tvoid
!		mov word64 [Dsptr+kvalue],0
		dec A3
		jnz loop1
		*jumpskip2
	end

	saveregs
	k_procentry()
	loadregs
	jumpnext
end

threadedproc j_pushm*=
	assem
		mov D4,[Dprog+kopnda]
		*pushvar
		mov D0,[D4+ktag]
		mov [Dsptr+ktag],D0
		mov D1,[D4+kvalue]
		mov [Dsptr+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
	L2:
		*jumpskip2
	end
	saveregs
	k_pushm()
	loadregs
	jumpnext
end

threadedproc j_pushf*=
	assem
		mov D4,[Dprog+kopnda]
		add D4,Dframe
		*pushvar
		mov D0,[D4+ktag]
		mov [Dsptr+ktag],D0
		mov D1,[D4+kvalue]
		mov [Dsptr+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
	L2:
		*jumpskip2
	end

	saveregs
	k_pushf()
	loadregs
	jumpnext
end

threadedproc j_pushmref*=
	assem
		*pushvar
		mov word32 [Dsptr+ktag],trefvar
		mov D4,[Dprog+kopnda]
		mov [Dsptr+kvarptr],D4
		*jumpskip2
	end
	saveregs
	k_pushmref()
	loadregs
	jumpnext
end

threadedproc j_pushfref*=
	assem
		*pushvar
		mov word32 [Dsptr+ktag],trefvar
		mov D4,[Dprog+kopnda]
		lea D0,[D4+Dframe]
		mov [Dsptr+kvarptr],D0
		*jumpskip2
	end

	saveregs
	k_pushfref()
	loadregs
	jumpnext
end

threadedproc j_popm*=
	assem
		mov D4,[Dprog+kopnda]

		cmp byte [D4+khasref],1
		jnz L2

		*callunshareu_d4
		mov D4,[Dprog+kopnda]

L2:
		mov D0,[Dsptr+ktag]
		mov [D4+ktag],D0
		mov D1,[Dsptr+kvalue]
		mov [D4+kvalue],D1

		*popvar
		*jumpskip2
	end

	saveregs
	k_popm()
	loadregs
	jumpnext
end

threadedproc j_storem*=
	assem
		cmp byte [Dsptr+khasref],1
		jnz L1
		mov D0,[Dsptr+kobjptr]
		inc word32 [D0+jrefcount]
L1:
		mov D4,[Dprog+kopnda]

		cmp byte [D4+khasref],1
		jnz L2

		*callunshareu_d4
		mov D4,[Dprog+kopnda]

L2:
		mov D0,[Dsptr+ktag]
		mov [D4+ktag],D0
		mov D1,[Dsptr+kvalue]
		mov [D4+kvalue],D1

		*jumpskip2
	end

	saveregs
	k_storem()
	loadregs
	jumpnext
end

threadedproc j_popf*=
	assem
		mov D4,[Dprog+kopnda]
		add D4,Dframe

		cmp byte [D4+khasref],1
		jnz L2
		*callunshareu_d4

L2:
		mov D0,[Dsptr+ktag]
		mov [D4+ktag],D0
		mov D1,[Dsptr+kvalue]
		mov [D4+kvalue],D1

		*popvar
		*jumpskip2
	end

	saveregs
	k_popf()
	loadregs
	jumpnext
end

threadedproc j_storef*=
	assem
		cmp byte [Dsptr+khasref],1
		jnz L1
		mov D0,[Dsptr+kobjptr]
		inc word32 [D0+jrefcount]
L1:
		mov D4,[Dprog+kopnda]
		add D4,Dframe

		cmp byte [D4+khasref],1
		jnz L2
		*callunshareu_d4

L2:
		mov D0,[Dsptr+ktag]
		mov [D4+ktag],D0
		mov D1,[Dsptr+kvalue]
		mov [D4+kvalue],D1

		*jumpskip2
	end

	saveregs
	k_storef()
	loadregs
	jumpnext
end

threadedproc j_pushci*=
	assem
		*pushvar
		mov word32 [Dsptr+ktag],tint
		mov D0,[Dprog+kopnda]
		mov [Dsptr+kvalue],D0
		*jumpskip2
	end

	saveregs
	k_pushci()
	loadregs
	jumpnext
end

threadedproc j_pushvoid*=
	assem
		*pushvar
		mov word32 [Dsptr+ktag],tvoid
		*jumpskip1
	end

	saveregs
	k_pushvoid()
	loadregs
	jumpnext
end

threadedproc j_pushnil*=
	assem
		*pushvar
		mov word32 [Dsptr+ktag],trefpack
		mov word32 [Dsptr+krefelemtag],tvoid
		mov word64 [Dsptr+kptr],0
		*jumpskip1
	end

	saveregs
	k_pushnil()
	loadregs
	jumpnext
end

threadedproc j_pushcr*=
	assem
		*pushvar
		mov word32 [Dsptr+ktag],treal
		mov D0,[Dprog+kopnda]
		mov [Dsptr+kvalue],D0
		*jumpskip2
	end

	saveregs
	k_pushcr()
	loadregs
	jumpnext
end

threadedproc j_pushcs*=
	assem
		*pushvar
		mov word32 [Dsptr+ktag],tstring ior hasrefmask
		mov D0,[Dprog+kopnda]
		mov [Dsptr+kobjptr],D0
		inc word32 [D0+jrefcount]
		*jumpskip2
	end

	saveregs
	k_pushcs()
	loadregs
	jumpnext
end

threadedproc j_pusht*=
	saveregs
	k_pusht()
	loadregs
	jumpnext
end

threadedproc j_pushsymbol*=
	saveregs
	k_pushsymbol()
	loadregs
	jumpnext
end

threadedproc j_pushptr*=
	assem
		cmp byte [Dsptr+ktag],trefvar
		jnz L1
		mov D4,[Dsptr+kvarptr]

		mov D0,[D4+ktag]
		mov [Dsptr+ktag],D0
		mov D1,[D4+kvalue]
		mov [Dsptr+kvalue],D1
		and A0,hasrefmask
		jz L12
		inc word32 [D1+jrefcount]
L12:
		*jumpskip1

L1:
		cmp byte [Dsptr+ktag],trefpack
		jnz L2
		mov D4,[Dsptr+kptr]
		movzx A0,word16 [Dsptr+krefelemtag]
		cmp A0,ti32
		jnz L10
		mov word32 [Dsptr+ktag],tint
		mov A0,[D4]
		movsxd D0,A0
		mov [Dsptr+kvalue],D0
		*jumpskip1
L10:
		cmp A0,tu8
		jnz L11
		mov word32 [Dsptr+ktag],tint
		movzx A0,byte [D4]
		mov [Dsptr+kvalue],D0
		*jumpskip1
L11:

L2:
L99:
	end
	saveregs
	k_pushptr()
	loadregs
	jumpnext
end

threadedproc j_popptr*=
	saveregs
	k_popptr()
	loadregs
	jumpnext
end

threadedproc j_zpopm*=
	saveregs
	k_zpopm()
	loadregs
	jumpnext
end

threadedproc j_zpopf*=
	assem
		mov D4,[Dprog+kopnda]
		add D4,Dframe
		mov D0,[Dsptr+ktag]
		mov [D4+ktag],D0
		mov D1,[Dsptr+kvalue]
		mov [D4+kvalue],D1

		*popvar
		*jumpskip2
	end

	saveregs
	k_zpopf()
	loadregs
	jumpnext
end

threadedproc j_dupl*=
	assem
		*pushvar
		mov D0,[Dsptr+xb]			!D0 = tag etc
		mov [Dsptr+ya],D0
		mov D1,[Dsptr+xb+kvalue]	!D1 = value/objptr etc
		mov [Dsptr+ya+kvalue],D1

		and A0,hasrefmask
		jz L1
		inc word32 [D1+jrefcount]
L1:

		*jumpskip1
	end

	saveregs
	k_dupl()
	loadregs
	jumpnext
end

threadedproc j_copy*=
	static varrec x

	assem
		cmp byte [dsptr+khasref],1
		jnz L1

		mov D0,[Dsptr]
		mov [x],D0
		mov D0,[Dsptr+kvalue]
		mov [x+kvalue],D0

		*callduplu_dsptr
		lea D10,[x]
		*callunshareu

L1:		*jumpskip1
	end

	saveregs
	k_copy()
	loadregs
	jumpnext
end

threadedproc j_swap*=
	saveregs
	k_swap()
	loadregs
	jumpnext
end

threadedproc j_convrefpack*=
	saveregs
	k_convrefpack()
	loadregs
	jumpnext
end

    threadedproc j_jump*=
        assem
            mov Dprog,[Dprog+kopnda]
            *jumpnext
        end
    end

    threadedproc j_jumpptr*=
        saveregs
        k_jumpptr()
        loadregs
        jumpnext
    end

threadedproc j_jumptrue*=
	assem
		cmp byte [Dsptr+xa+ktag],tint
		jnz L1

		mov D0,[Dsptr+xa+kvalue]
		and D0,D0
		jz L2
		mov Dprog,[Dprog+kopnda]
		*popvar
		*jumpnext
L2:
		*popvar
		*jumpskip2
L1:
	end
	saveregs
	k_jumptrue()
	loadregs
	jumpnext
end

threadedproc j_jumpfalse*=
	assem
		cmp byte [Dsptr+xa+ktag],tint
		jnz L1

		mov D0,[Dsptr+xa+kvalue]
		and D0,D0
		jnz L2
		mov Dprog,[Dprog+kopnda]
		*popvar
		*jumpnext
L2:
		*popvar
		*jumpskip2
L1:
	end

	saveregs
	k_jumpfalse()
	loadregs
	jumpnext
end

threadedproc j_jumpeq*=
	assem
		mov B0,[Dsptr+xb+ktag]
		mov B1,[Dsptr+ya+ktag]
		cmp B0,B1
		jnz L99

		cmp B0,tint
		jnz L1

		mov D0,[Dsptr+xb+kvalue]
		cmp D0,[Dsptr+ya+kvalue]
		jz Ltrue
		*popvar2
		*jumpskip2

Ltrue:
		*popvar2
		mov Dprog,[Dprog+kopnda]
		*jumpnext
L1:
		cmp B0,treal
		jnz L3

		movq XMM0,[Dsptr+xb+kvalue]
		comisd XMM0,[Dsptr+ya+kvalue]
		jz Ltrue
		*popvar2
		*jumpskip2

L3:
L99:
	end

	saveregs
	k_jumpeq()
	loadregs
	jumpnext
end

threadedproc j_jumpne*=
	assem
		mov B0,[Dsptr+xb+ktag]
		mov B1,[Dsptr+ya+ktag]
		cmp B0,B1
		jnz L99

		cmp B0,tint
		jnz L1

		mov D0,[Dsptr+xb+kvalue]
		cmp D0,[Dsptr+ya+kvalue]
		jnz Ltrue
		*popvar2
		*jumpskip2

Ltrue:
		*popvar2
		mov Dprog,[Dprog+kopnda]
		*jumpnext
L1:
		cmp B0,treal
		jnz L3

		movq XMM0,[Dsptr+xb+kvalue]
		comisd XMM0,[Dsptr+ya+kvalue]
		jnz Ltrue
		*popvar2
		*jumpskip2

L3:
L99:
	end

	saveregs
	k_jumpne()
	loadregs
	jumpnext
end

threadedproc j_jumplt*=
	assem
		mov B0,[Dsptr+xb+ktag]
		mov B1,[Dsptr+ya+ktag]
		cmp B0,B1
		jnz L99

		cmp B0,tint
		jnz L1

		mov D0,[Dsptr+xb+kvalue]
		cmp D0,[Dsptr+ya+kvalue]
		jl Ltrue
		*popvar2
		*jumpskip2

Ltrue:
		*popvar2
		mov Dprog,[Dprog+kopnda]
		*jumpnext
L1:
		cmp B0,treal
		jnz L3

		movq XMM0,[Dsptr+xb+kvalue]
		comisd XMM0,[Dsptr+ya+kvalue]
		jb Ltrue
		*popvar2
		*jumpskip2

L3:
L99:
	end

	saveregs
	k_jumplt()
	loadregs
	jumpnext
end

threadedproc j_jumple*=
	assem
		mov B0,[Dsptr+xb+ktag]
		mov B1,[Dsptr+ya+ktag]
		cmp B0,B1
		jnz L99

		cmp B0,tint
		jnz L1

		mov D0,[Dsptr+xb+kvalue]
		cmp D0,[Dsptr+ya+kvalue]
		jle Ltrue
		*popvar2
		*jumpskip2

Ltrue:
		*popvar2
		mov Dprog,[Dprog+kopnda]
		*jumpnext
L1:
		cmp B0,treal
		jnz L3

		movq XMM0,[Dsptr+xb+kvalue]
		comisd XMM0,[Dsptr+ya+kvalue]
		jbe Ltrue
		*popvar2
		*jumpskip2

L3:
L99:
	end

	saveregs
	k_jumple()
	loadregs
	jumpnext
end

threadedproc j_jumpge*=
	assem
		mov B0,[Dsptr+xb+ktag]
		mov B1,[Dsptr+ya+ktag]
		cmp B0,B1
		jnz L99

		cmp B0,tint
		jnz L1

		mov D0,[Dsptr+xb+kvalue]
		cmp D0,[Dsptr+ya+kvalue]
		jge Ltrue
		*popvar2
		*jumpskip2

Ltrue:
		*popvar2
		mov Dprog,[Dprog+kopnda]
		*jumpnext
L1:
		cmp B0,treal
		jnz L3

		movq XMM0,[Dsptr+xb+kvalue]
		comisd XMM0,[Dsptr+ya+kvalue]
		jae Ltrue
		*popvar2
		*jumpskip2

L3:
L99:
	end

	saveregs
	k_jumpge()
	loadregs
	jumpnext
end

threadedproc j_jumpgt*=
	assem
		mov B0,[Dsptr+xb+ktag]
		mov B1,[Dsptr+ya+ktag]
		cmp B0,B1
		jnz L99

		cmp B0,tint
		jnz L1

		mov D0,[Dsptr+xb+kvalue]
		cmp D0,[Dsptr+ya+kvalue]
		jg Ltrue
		*popvar2
		*jumpskip2

Ltrue:
		*popvar2
		mov Dprog,[Dprog+kopnda]
		*jumpnext
L1:
		cmp B0,treal
		jnz L3

		movq XMM0,[Dsptr+xb+kvalue]
		comisd XMM0,[Dsptr+ya+kvalue]
		ja Ltrue
		*popvar2
		*jumpskip2

L3:
L99:
	end

	saveregs
	k_jumpgt()
	loadregs
	jumpnext
end

threadedproc j_jumptesteq*=
	assem
		cmp word16 [Dsptr+ya+ktag],tint
		jnz L99
		cmp word16 [Dsptr+xb+ktag],tint
		jnz L99
		mov D0,[Dsptr+ya+kvalue]
		cmp D0,[Dsptr+xb+kvalue]
		jnz L2
!equal, so pop both and jump
		*popvar2
		mov Dprog,[Dprog+kopnda]
		*jumpnext
!not equal: keep x on stack
L2:
		*popvar
		*jumpskip2

L99:
	end

	saveregs
	k_jumptesteq()
	loadregs
	jumpnext
end

threadedproc j_jumptestne*=
	assem
		cmp word16 [Dsptr+ya+ktag],tint
		jnz L1
		cmp word16 [Dsptr+xb+ktag],tint
		jnz L1
		mov D0,[Dsptr+ya+kvalue]
		cmp D0,[Dsptr+xb+kvalue]
		jz L2
!not equal, so pop y and jump
		*popvar
		mov Dprog,[Dprog+kopnda]
		*jumpnext
L2:
		*popvar2
		*jumpskip2

L1:
	end

	saveregs
	k_jumptestne()
	loadregs
	jumpnext
end

threadedproc j_switch*=
	assem
		cmp word16 [Dsptr+ktag],tint
		jnz L1						!get M to deal with errors
		mov D4,[Dsptr+kvalue]		!switch index
		*popvar
		sub D4,[Dprog+kopndb]		!index-lower; now 0-based index
		cmp D4,[Dprog+kopnda]		!index0>=n?
		jae L2						!out of range
!in range
		shl D4,1
		mov Dprog,[Dprog+D4*8+intpsize4]
		*jumpnext
!out of range
L2:
		mov D5,[Dprog+kopnda]
		shl D5,1
		mov Dprog,[Dprog+D5*8+intpsize4]
		*jumpnext

L1:
	end

	saveregs
	k_switch()
	loadregs
	jumpnext
end

threadedproc j_tom*=
	assem
		mov D4,[Dprog+kopnda]
		mov D5,[Dprog+kopndb]
		dec word64 [D5+kvalue]
		jz L1
		mov Dprog,D4
		*jumpnext
L1:
		*jumpskip3
	end

	saveregs
	k_tom()
	loadregs
	jumpnext
end

threadedproc j_tof*=
	assem
		mov D4,[Dprog+kopnda]
		mov D5,[Dprog+kopndb]
		dec word64 [Dframe+D5+kvalue]
		jz L1
		mov Dprog,D4
		*jumpnext
L1:
		*jumpskip3
	end

	saveregs
	k_tof()
	loadregs
	jumpnext
end

threadedproc j_formci*=
	saveregs
	k_formci()
	loadregs
	jumpnext
end

threadedproc j_forfci*=
	assem
		mov D4,[Dprog+kopnda]		!label
		mov D0,[Dprog+kopndb]		!a
		inc word64 [Dframe+D0+kvalue]	!++a
		mov D0,[Dframe+D0+kvalue]
		cmp A0,[Dprog+kopndc]
		jg L1
		mov Dprog,D4
		*jumpnext
	L1:
		*jumpskip4
	end

	saveregs
	k_forfci()
	loadregs
	jumpnext
end

threadedproc j_formm*=
	saveregs
	k_formm()
	loadregs
	jumpnext
end

threadedproc j_forff*=
	assem
		mov D0,[Dprog+kopndb]		!b
		mov D5,[Dprog+kopndc]		!c
		mov D4,[Dprog+kopnda]		!label

		inc word64 [Dframe+D0+kvalue]
		mov D0,[Dframe+D0+kvalue]
		cmp D0,[Dframe+D5+kvalue]

		jg L1
		mov Dprog,D4
		*jumpnext
	L1:
		*jumpskip4
	end

	saveregs
	k_forff()
	loadregs
	jumpnext
end

threadedproc j_fordmci*=
	saveregs
	k_fordmci()
	loadregs
	jumpnext
end

threadedproc j_fordfci*=
	saveregs
	k_fordfci()
	loadregs
	jumpnext
end

threadedproc j_fordmm*=
	saveregs
	k_fordmm()
	loadregs
	jumpnext
end

threadedproc j_fordff*=
	saveregs
	k_fordff()
	loadregs
	jumpnext
end

threadedproc j_callproc*=
	const countinterval=100
	static int count=countinterval

	assem
		dec word32 [count]
		jz L99

		*pushvar
		mov word32 [Dsptr+ktag],tretaddr
		lea D0,[Dprog+24]		! return address
		mov [Dsptr+kretaddr],D0
		mov [Dsptr+kframeptr_low],Aframe
		mov Dframe,Dsptr
		mov [frameptr],Dframe
		mov Dprog,[Dprog+kopnda]
		*jumpnext

L99:
		mov word32 [count],countinterval
	end

	saveregs
	k_callproc()
	loadregs
	jumpnext
end

threadedproc j_callptr*=
	saveregs
	k_callptr()
	loadregs
	jumpnext
end

threadedproc j_return0*=
	assem
		mov Dprog,[Dsptr+kretaddr]
		mov Aframe,[Dsptr+kframeptr_low]
		*popvar
		*jumpnext
	end

	saveregs
	k_return0()
	loadregs
	jumpnext
end

threadedproc j_return*=

	assem
		mov D5,[Dprog+kopnda]		!nargs

		mov Dprog,[Dsptr+kretaddr]
		mov Aframe,[Dsptr+kframeptr_low]
		*popvar

		and D5,D5
		jz L2				!no args
L1:		cmp byte [Dsptr+khasref],1
		jnz L11
		*callunshareu_dsptr

L11:	*popvar
		dec D5
		jnz L1

L2:
		*jumpnext
	end

	saveregs
	k_return()
	loadregs
	jumpnext
end

threadedproc j_popretval*=
	assem
		mov D4,[Dprog+kopnda]
		mov D0,[Dsptr+ktag]
		mov [Dframe+D4+ktag],D0
		mov D1,[Dsptr+kvalue]
		mov [Dframe+D4+kvalue],D1
		*popvar
		*jumpskip2
	end

	saveregs
	k_popretval()
	loadregs
	jumpnext
end

threadedproc j_modulecall*=
	saveregs
	k_modulecall()
	loadregs
	jumpnext
end

threadedproc j_modulereturn*=
	saveregs
	k_modulereturn()
	loadregs
	jumpnext
end

threadedproc j_calldll*=
	saveregs
	k_calldll()
	loadregs
	jumpnext
end

threadedproc j_callhost*=
	saveregs
	k_callhost()
	loadregs
	jumpnext
end

threadedproc j_unshare*=
	assem
		mov D5,[Dprog+kopnda]		!assume > 0

L1:		cmp byte [Dsptr+khasref],1
		jnz L2
		*callunshareu_dsptr
L2:		*popvar
		dec D5
		jnz L1
		*jumpskip2
	end

	saveregs
	k_unshare()
	loadregs
	jumpnext
end

threadedproc j_stop*=
	saveregs

	asm jmp disploop.stoplabel
end

threadedproc j_stoprunproc*=
	saveregs
	asm jmp disploop.stoplabel
end

threadedproc j_makelist*=
	saveregs
	k_makelist()
	loadregs
	jumpnext
end

threadedproc j_makerecord*=
	saveregs
	k_makerecord()
	loadregs
	jumpnext
end

threadedproc j_makearray*=
	saveregs
	k_makearray()
	loadregs
	jumpnext
end

threadedproc j_makebits*=
	saveregs
	k_makebits()
	loadregs
	jumpnext
end

threadedproc j_makestruct*=
	saveregs
	k_makestruct()
	loadregs
	jumpnext
end

threadedproc j_makeset*=
	saveregs
	k_makeset()
	loadregs
	jumpnext
end

threadedproc j_makerange*=
	saveregs
	k_makerange()
	loadregs
	jumpnext
end

threadedproc j_makerangelen*=
	saveregs
	k_makerangelen()
	loadregs
	jumpnext
end

threadedproc j_makedict*=
	saveregs
	k_makedict()
	loadregs
	jumpnext
end

threadedproc j_makedecimal*=
	saveregs
	k_makedecimal()
	loadregs
	jumpnext
end

threadedproc j_incrptr*=
	assem
		cmp byte [Dsptr+ktag],trefvar
		jnz L99
		mov D4,[Dsptr+kvarptr]
		mov B0,[D4+ktag]
		cmp B0,tint
		jnz L1
		inc word64 [D4+kvalue]
		*popvar
		*jumpskip1

L1:
!		cmp B0,trefpack
!		jnz L2
!		movzx A3,word16 [Dsptr+krefelemtag]
!		mov D3,[D3*8+ttsize]
!		add [D4+kptr],D3
!		*popvar
!		*jumpskip1

L2:

L99:
	end

	saveregs
	k_incrptr()
	loadregs
	jumpnext
end

threadedproc j_incrtom*=
	assem
		mov D4,[Dprog+kopnda]
		cmp byte [D4+ktag],tint
		jnz L1
		inc word64 [D4+kvalue]
		*jumpskip2

	L1:
		cmp byte [D4+ktag],trefpack
		jnz L2
		movzx A0,word16 [D4+krefelemtag]
!		mov A0,[D0*8+ttsize]
		lea D1,[ttsize]
		mov A0,[D1+D0*8]
		add [D4+kvarptr],D0
		*jumpskip2

L2:
	end

	saveregs
	k_incrtom()
	loadregs
	jumpnext
end

threadedproc j_incrtof*=
	assem
		mov D4,[Dprog+kopnda]
		add D4,Dframe
		cmp byte [D4+ktag],tint
		jnz L1
		inc word64 [D4+kvalue]
		*jumpskip2

	L1:
		cmp byte [D4+ktag],trefpack
		jnz L2
		movzx A0,word16 [D4+krefelemtag]
!		mov A0,[D0*8+ttsize]
		lea D1,[ttsize]
		mov A0,[D1+D0*8]
		add [D4+kvarptr],D0
		*jumpskip2

L2:
	end

	saveregs
	k_incrtof()
	loadregs
	jumpnext
end

threadedproc j_loadincr*=
	assem
		cmp byte [Dsptr+ktag],trefvar
		jnz L99
		mov D4,[Dsptr+kvarptr]

		mov B0,[D4+ktag]
		cmp B0,tint
		jnz L1
!refvar int
		mov D0,[D4+kvalue]
		inc word64 [D4+kvalue]
		mov word32 [Dsptr+ktag],tint
		mov [Dsptr+kvalue],D0
		*jumpskip1

L1:
		cmp B0,trefpack
		jnz L2
!		cmp word16 [D4+krefelemtag],tu8
		cmp word32 [D4+krefelemtag],tu8
		jnz L2

!refvar refpack u8
		mov D0,[D4+kptr]
		inc word64 [D4+kptr]

		mov word32 [Dsptr+ktag],trefpack
!		mov word16 [Dsptr+krefelemtag],tu8
		mov word32 [Dsptr+krefelemtag],tu8
		mov [Dsptr+kptr],D0
		*jumpskip1

L2:

L99:
	end
!
	saveregs
	k_loadincr()
	loadregs
	jumpnext
end

threadedproc j_incrload*=
	assem
		cmp byte [Dsptr+ktag],trefvar
		jnz L99
		mov D4,[Dsptr+kvarptr]

		mov B0,[D4+ktag]
		cmp B0,tint
		jnz L1
!refvar int
		inc word64 [D4+kvalue]
		mov D0,[D4+kvalue]
		mov word32 [Dsptr+ktag],tint
		mov [Dsptr+kvalue],D0
		*jumpskip1

L1:
		cmp B0,trefpack
		jnz L2
!		cmp word16 [D4+krefelemtag],tu8
		cmp word32 [D4+krefelemtag],tu8
		jnz L2

!refvar refpack u8
		inc word64 [D4+kptr]
		mov D0,[D4+kptr]
		mov word32 [Dsptr+ktag],trefpack
!		mov word16 [Dsptr+krefelemtag],tu8
		mov word32 [Dsptr+krefelemtag],tu8
		mov [Dsptr+kptr],D0
		*jumpskip1

L2:

L99:
	end

	saveregs
!CPL "J:CAN'T DO INCRLOAD",TTNAME[SPTR.TAG]
	k_incrload()
	loadregs
	jumpnext
end

threadedproc j_decrptr*=
	saveregs
	k_decrptr()
	loadregs
	jumpnext
end

threadedproc j_decrtom*=
	assem
		mov D4,[Dprog+kopnda]
		cmp byte [D4+ktag],tint
		jnz L1
		dec word64 [D4+kvalue]
		*jumpskip2

	L1:
		cmp byte [D4+ktag],trefpack
		jnz L2
		movzx A0,word16 [D4+krefelemtag]
!		mov A0,[D0*8+ttsize]
		lea D1,[ttsize]
		mov A0,[D1+D0*8]
		sub [D4+kvarptr],D0
		*jumpskip2

L2:
	end

	saveregs
	k_decrtom()
	loadregs
	jumpnext
end

threadedproc j_decrtof*=
	assem
		mov D4,[Dprog+kopnda]
		cmp byte [Dframe+D4+ktag],tint
		jnz L1
		dec word64 [Dframe+D4+kvalue]
		*jumpskip2

	L1:
		cmp byte [Dframe+D4+ktag],trefpack
		jnz L2
		movzx A0,word16 [Dframe+D4+krefelemtag]
!		mov A0,[D0*8+ttsize]
		lea D1,[ttsize]
		mov A0,[D1+D0*8]
		sub [Dframe+D4+kvarptr],D0
		*jumpskip2

L2:
	end

	saveregs
	k_decrtof()
	loadregs
	jumpnext
end

threadedproc j_loaddecr*=
	saveregs
	k_loaddecr()
	loadregs
	jumpnext
end

threadedproc j_decrload*=
	saveregs
	k_decrload()
	loadregs
	jumpnext
end

threadedproc j_incr*=
	saveregs
	k_incr()
	loadregs
	jumpnext
end

threadedproc j_decr*=
	saveregs
	k_decr()
	loadregs
	jumpnext
end

threadedproc j_neg*=
	saveregs
	k_neg()
	loadregs
	jumpnext
end

threadedproc j_abs*=
	saveregs
	k_abs()
	loadregs
	jumpnext
end

threadedproc j_notl*=
	saveregs
	k_notl()
	loadregs
	jumpnext
end

threadedproc j_inot*=
	assem
		cmp byte [Dsptr+ktag],tint
		jnz L1

		not word64 [dsptr+kvalue]
!		mov D0,[dsptr+kvalue]
!		not D0
!		mov [dsptr+kvalue],D0
		*jumpskip1
L1:
	end

	saveregs
	k_inot()
	loadregs
	jumpnext
end

threadedproc j_istruel*=
	saveregs
	k_istruel()
	loadregs
	jumpnext
end

threadedproc j_asc*=
	saveregs
	k_asc()
	loadregs
	jumpnext
end

threadedproc j_chr*=
	assem
		cmp byte [Dsptr+ktag],tint
		jnz L99						!not int; B deals with the error
		mov D0,[Dsptr+kvalue]
		cmp D0,255
		ja L99						!not in range
		lea D1,[chrtable]
		mov D0,[D1+D0*8]
!		mov D0,[D0*8+chrtable]
		and D0,D0
		jz L99						!value not cached; B will fill it in
		mov word32 [Dsptr+ktag],tstring+hasrefmask
		mov [Dsptr+kvalue],D0				!point to object
		inc word32 [D0+jrefcount]
		*jumpskip1
L99:
	end

	saveregs
	k_chr()
	loadregs
	jumpnext
end

threadedproc j_sqrt*=
	assem
		cmp word16 [Dsptr+ktag],tint
		jnz L1
		fild word32 [Dsptr+kvalue]
		fsqrt
		mov word32 [Dsptr+ktag],treal
		fstp word64 [Dsptr+kvalue]
		*jumpskip1
L1:
		cmp word16 [Dsptr+ktag],treal
		jnz L2

		movq xmm0,[Dsptr+kvalue]
		sqrtsd xmm0,xmm0
		movq [Dsptr+kvalue],xmm0

		*jumpskip1
L2:
	end

	saveregs
	k_sqrt()
	loadregs
	jumpnext
end

threadedproc j_sqr*=
	assem
		cmp word16 [Dsptr+ktag],tint
		jnz L1
		mov D0,[Dsptr+kvalue]
		imul2 D0,D0
		mov [Dsptr+kvalue],D0
		*jumpskip1
L1:
		cmp word16 [Dsptr+ktag],treal
		cmp word16 [Dsptr+ktag],treal
		jnz L2

		movq xmm0,[Dsptr+kvalue]
		mulsd xmm0,xmm0
		movq [Dsptr+kvalue],xmm0

		*jumpskip1
L2:
	end

	saveregs
	k_sqr()
	loadregs
	jumpnext
end

threadedproc j_sin*=
	saveregs
	k_sin()
	loadregs
	jumpnext
end

threadedproc j_cos*=
	saveregs
	k_cos()
	loadregs
	jumpnext
end

threadedproc j_tan*=
	saveregs
	k_tan()
	loadregs
	jumpnext
end

threadedproc j_asin*=
	saveregs
	k_asin()
	loadregs
	jumpnext
end

threadedproc j_acos*=
	saveregs
	k_acos()
	loadregs
	jumpnext
end

threadedproc j_atan*=
	saveregs
	k_atan()
	loadregs
	jumpnext
end

threadedproc j_sign*=
	saveregs
	k_sign()
	loadregs
	jumpnext
end

threadedproc j_log*=
	saveregs
	k_log()
	loadregs
	jumpnext
end

threadedproc j_log10*=
	saveregs
	k_log10()
	loadregs
	jumpnext
end

threadedproc j_exp*=
	saveregs
	k_exp()
	loadregs
	jumpnext
end

threadedproc j_round*=
	saveregs
	k_round()
	loadregs
	jumpnext
end

threadedproc j_floor*=
	saveregs
	k_floor()
	loadregs
	jumpnext
end

threadedproc j_ceil*=
	saveregs
	k_ceil()
	loadregs
	jumpnext
end

threadedproc j_fract*=
	saveregs
	k_fract()
	loadregs
	jumpnext
end

threadedproc j_fmod*=
	saveregs
	k_fmod()
	loadregs
	jumpnext
end

threadedproc j_negto*=
	saveregs
	k_negto()
	loadregs
	jumpnext
end

threadedproc j_absto*=
	saveregs
	k_absto()
	loadregs
	jumpnext
end

threadedproc j_inotto*=
	saveregs
	k_inotto()
	loadregs
	jumpnext
end

threadedproc j_notlto*=
	saveregs
	k_notlto()
	loadregs
	jumpnext
end

threadedproc j_len*=
	assem
		mov W0, [Dsptr+ktag]
		cmp B0,tlist
		jz L1
		cmp B0,tstring
		jz L1
		cmp B0,tarray
		jnz L99
L1:
		mov D1,[Dsptr+kobjptr]
		mov D3, [D1+jlength]

!		and W0,hasrefmask
!		jz L2
		*callunshareu_dsptr
!L2:
		mov word32 [Dsptr+ktag],tint
		mov [Dsptr+kvalue],D3
		*jumpskip1
L99:
	end

	saveregs
	k_len()
	loadregs
	jumpnext
end

threadedproc j_lwb*=
	saveregs
	k_lwb()
	loadregs
	jumpnext
end

threadedproc j_upb*=
	assem
		mov W0, [Dsptr+ktag]
		cmp B0,tlist
		jnz L99
L1:
		mov D1,[Dsptr+kobjptr]
		mov D3, [D1+jlength]
		movsx D4, word16 [D1+jlower16]
		lea D3,[D3+D4-1]	

		*callunshareu_dsptr
!L2:
		mov word32 [Dsptr+ktag],tint
		mov [Dsptr+kvalue],D3
		*jumpskip1
L99:
	end

	saveregs
	k_upb()
	loadregs
	jumpnext
end

threadedproc j_bounds*=
	saveregs
	k_bounds()
	loadregs
	jumpnext
end

threadedproc j_boundsx*=
	saveregs
	k_boundsx()
	loadregs
	jumpnext
end

threadedproc j_bitwidth*=
	saveregs
	k_bitwidth()
	loadregs
	jumpnext
end

threadedproc j_bytesize*=
	saveregs
	k_bytesize()
	loadregs
	jumpnext
end

!threadedproc j_type*=
!	saveregs
!	k_type()
!	loadregs
!	jumpnext
!end

threadedproc j_elemtype*=
	saveregs
	k_elemtype()
	loadregs
	jumpnext
end

threadedproc j_basetype*=
	saveregs
	k_basetype()
	loadregs
	jumpnext
end

threadedproc j_type*=
	saveregs
	k_type()
	loadregs
	jumpnext
end

threadedproc j_dictitems*=
	saveregs
	k_dictitems()
	loadregs
	jumpnext
end

threadedproc j_minvalue*=
	saveregs
	k_minvalue()
	loadregs
	jumpnext
end

threadedproc j_maxvalue*=
	saveregs
	k_maxvalue()
	loadregs
	jumpnext
end

threadedproc j_isint*=
	assem
		mov W0,[Dsptr+ktag]
		cmp B0,tint
		setz B3
		movzx D3,B3
		and W0,hasrefmask
		jz L1
		*callunshareu_dsptr
L1:		mov word32 [Dsptr],tint
		mov [Dsptr+kvalue],D3
		*jumpskip1
	end

	saveregs
	k_isint()
	loadregs
	jumpnext
end

threadedproc j_isreal*=
	saveregs
	k_isreal()
	loadregs
	jumpnext
end

threadedproc j_isstring*=
	saveregs
	k_isstring()
	loadregs
	jumpnext
end

threadedproc j_isrange*=
	saveregs
	k_isrange()
	loadregs
	jumpnext
end

threadedproc j_isnumber*=
	saveregs
	k_isnumber()
	loadregs
	jumpnext
end

threadedproc j_islist*=
	assem
		mov W0,[Dsptr+ktag]
		cmp B0,tlist
		setz B3
		movzx D3,B3
		and W0,hasrefmask
		jz L1
		*callunshareu_dsptr
L1:		mov word32 [Dsptr],tint
		mov [Dsptr+kvalue],D3
		*jumpskip1
	end

	saveregs
	k_islist()
	loadregs
	jumpnext
end

threadedproc j_isrecord*=
	saveregs
	k_isrecord()
	loadregs
	jumpnext
end

threadedproc j_ispointer*=
	saveregs
	k_ispointer()
	loadregs
	jumpnext
end

threadedproc j_isarray*=
	saveregs
	k_isarray()
	loadregs
	jumpnext
end

threadedproc j_ismutable*=
	saveregs
	k_ismutable()
	loadregs
	jumpnext
end

threadedproc j_isset*=
	saveregs
	k_isset()
	loadregs
	jumpnext
end

threadedproc j_isvoid*=
	saveregs
	k_isvoid()
	loadregs
	jumpnext
end

threadedproc j_isdef*=
	assem
		cmp byte [Dsptr+ktag],tvoid
		jnz L1
		mov D3,0
		jmp L2
L1:		mov D3,1
L2:
		cmp byte [Dsptr+khasref],1
		jnz L3
		*callunshareu_dsptr
L3:
		mov word32 [Dsptr+ktag],tint
		mov [Dsptr+kvalue],D3
		*jumpskip1
	end

	saveregs
	k_isdef()
	loadregs
	jumpnext
end

threadedproc j_isequal*=
	saveregs
	k_isequal()
	loadregs
	jumpnext
end

threadedproc j_convert*=
	saveregs
	k_convert()
	loadregs
	jumpnext
end

threadedproc j_typepun*=
	saveregs
	k_typepun()
	loadregs
	jumpnext
end

threadedproc j_add*=
	assem
		mov B0, [Dsptr+xb+ktag]
		mov B1, [Dsptr+ya+ktag]
		cmp B0,B1
		jnz L99
		cmp B0,tint
		jnz L1
		mov D0,[Dsptr+ya+kvalue]
		add [Dsptr+xb+kvalue],D0
		*popvar
		*jumpskip1
L1:
		cmp B0,treal
		jnz L2

		fld word64 [Dsptr+xb+kvalue]
		fld word64 [Dsptr+ya+kvalue]
		fadd
		fstp word64 [Dsptr+xb+kvalue]

		*popvar
		*jumpskip1
L2:


L99:
	end

	saveregs
	k_add()
	loadregs
	jumpnext
end

threadedproc j_sub*=
	assem
		mov B0, [Dsptr+xb+ktag]
		mov B1, [Dsptr+ya+ktag]
		cmp B0,B1
		jnz L99

		cmp B0,tint
		jnz L1
		mov D0,[Dsptr+ya+kvalue]
		sub [Dsptr+xb+kvalue],D0
		*popvar
		*jumpskip1
L1:
		cmp B0,treal
		jnz L2

		fld word64 [Dsptr+xb+kvalue]
		fld word64 [Dsptr+ya+kvalue]
		fsub
		fstp word64 [Dsptr+xb+kvalue]

		*popvar
		*jumpskip1
L2:
L99:
	end

	saveregs
	k_sub()
	loadregs
	jumpnext
end

threadedproc j_mul*=
	assem
		mov B0, [Dsptr+xb+ktag]
		mov B1, [Dsptr+ya+ktag]
		cmp B0,B1
		jnz L99
		cmp B0,tint
		jnz L1

		mov D0,[Dsptr+xb+kvalue]
		imul word64 [Dsptr+ya+kvalue]
		mov [Dsptr+xb+kvalue],D0
		*popvar
		*jumpskip1
L1:
		cmp B0, treal
		jnz L2

		fld word64 [Dsptr+xb+kvalue]
		fld word64 [Dsptr+ya+kvalue]
		fmul
		fstp word64 [Dsptr+xb+kvalue]

		*popvar
		*jumpskip1

L2:
L99:
	end

	saveregs
	k_mul()
	loadregs
	jumpnext
end

threadedproc j_div*=
	assem
		mov B0, [Dsptr+xb+ktag]
		mov B1, [Dsptr+ya+ktag]
		cmp B0,B1
		jnz L99
	
		cmp B0, treal
		jnz L2

		fld word64 [Dsptr+xb+kvalue]
		fld word64 [Dsptr+ya+kvalue]
		fdiv
		fstp word64 [Dsptr+xb+kvalue]

		*popvar
		*jumpskip1

L2:
L99:
	end

	saveregs
	k_div()
	loadregs
	jumpnext
end

threadedproc j_idiv*=
	assem
		cmp byte [Dsptr+xb+ktag],tint
		jnz L1
		cmp byte [Dsptr+ya+ktag],tint
		jnz L1
		mov D0,[Dsptr+xb+kvalue]
		cqo
		mov D1, [Dsptr+ya+kvalue]
		and D1,D1
		jz L1
		idiv D1
		mov [Dsptr+xb+kvalue],D0
		*popvar
		*jumpskip1
L1:
	end

	saveregs

	k_idiv()
	loadregs
	jumpnext
end

threadedproc j_irem*=
	assem
		cmp byte [Dsptr+xb+ktag],tint
		jnz L1
		cmp byte [Dsptr+ya+ktag],tint
		jnz L1
		mov D0,[Dsptr+xb+kvalue]
		cqo
		idiv word64 [Dsptr+ya+kvalue]
		mov [Dsptr+xb+kvalue],D11
		*popvar
		*jumpskip1
L1:
	end
	saveregs
	k_irem()
	loadregs
	jumpnext
end

threadedproc j_idivrem*=
	saveregs
	k_idivrem()
	loadregs
	jumpnext
end

threadedproc j_iand*=
	assem
		cmp byte [Dsptr+xb+ktag],tint
		jnz L1
		cmp byte [Dsptr+ya+ktag],tint
		jnz L1
		mov D0,[Dsptr+ya+kvalue]
		and [Dsptr+xb+kvalue],D0
		*popvar
		*jumpskip1
L1:
	end

	saveregs
	k_iand()
	loadregs
	jumpnext
end

threadedproc j_ior*=
	assem
		cmp byte [Dsptr+xb+ktag],tint
		jnz L1
		cmp byte [Dsptr+ya+ktag],tint
		jnz L1
		mov D0,[Dsptr+ya+kvalue]
		or [Dsptr+xb+kvalue],D0
		*popvar
		*jumpskip1
L1:
	end
	saveregs
	k_ior()
	loadregs
	jumpnext
end

threadedproc j_ixor*=
	assem
		cmp byte [Dsptr+xb+ktag],tint
		jnz L1
		cmp byte [Dsptr+ya+ktag],tint
		jnz L1
		mov D0,[Dsptr+ya+kvalue]
		xor [Dsptr+xb+kvalue],D0
		*popvar
		*jumpskip1
L1:
	end
	saveregs
	k_ixor()
	loadregs
	jumpnext
end

threadedproc j_shl*=
	assem
		cmp byte [Dsptr+xb+ktag],tint
		jnz L1
		cmp byte [Dsptr+ya+ktag],tint
		jnz L1
		mov rdx,Dsptr
		mov cl,[Dsptr+ya+kvalue]
		shl word64 [rdx+xb+kvalue],cl
		mov Dsptr,rdx
		*popvar
		*jumpskip1
L1:
	end

	saveregs
	k_shl()
	loadregs
	jumpnext
end

threadedproc j_shr*=
	assem
		cmp byte [Dsptr+xb+ktag],tint
		jnz L1
		cmp byte [Dsptr+ya+ktag],tint
		jnz L1
		mov rdx,Dsptr
		mov cl,[Dsptr+ya+kvalue]
		sar word64 [rdx+xb+kvalue],cl
		mov Dsptr,rdx
		*popvar
		*jumpskip1
L1:
	end
	saveregs
	k_shr()
	loadregs
	jumpnext
end

threadedproc j_in*=
	saveregs
	k_in()
	loadregs
	jumpnext
end

threadedproc j_notin*=
	saveregs
	k_notin()
	loadregs
	jumpnext
end

threadedproc j_inx*=
	saveregs
	k_inx()
	loadregs
	jumpnext
end

threadedproc j_eq*=
	saveregs
	k_eq()
	loadregs
	jumpnext
end

threadedproc j_ne*=
	saveregs
	k_ne()
	loadregs
	jumpnext
end

threadedproc j_lt*=
	saveregs
	k_lt()
	loadregs
	jumpnext
end

threadedproc j_le*=
	saveregs
	k_le()
	loadregs
	jumpnext
end

threadedproc j_ge*=
	saveregs
	k_ge()
	loadregs
	jumpnext
end

threadedproc j_gt*=
	saveregs
	k_gt()
	loadregs
	jumpnext
end

threadedproc j_min*=
	saveregs
	k_min()
	loadregs
	jumpnext
end

threadedproc j_max*=
	saveregs
	k_max()
	loadregs
	jumpnext
end

threadedproc j_concat*=
	saveregs
	k_concat()
	loadregs
	jumpnext
end

threadedproc j_append*=
	saveregs
	k_append()
	loadregs
	jumpnext
end

threadedproc j_power*=
	saveregs
	k_power()
	loadregs
	jumpnext
end

threadedproc j_atan2*=
	saveregs
	k_atan2()
	loadregs
	jumpnext
end

threadedproc j_addto*=
	assem
		mov D4,[Dsptr+xb+kvarptr]
		cmp byte [Dsptr+xb+ktag],trefvar	!lhs is ref var?
		jnz L99
		cmp byte [Dsptr+ya+ktag],tint		!rhs is int
		jnz L1
		cmp byte [D4+ktag],tint			!lhs is ref var:int?
		jnz L99
		mov D1,[Dsptr+kvalue]
		mov D0,[D4+kvalue]
		add D0,D1
		mov [D4+kvalue],D0
		*popvar2
		*jumpskip1

L1:
		cmp byte [Dsptr+ya+ktag],treal		!rhs is real
		jnz L2
		cmp byte [D4+ktag],treal			!lhs is ref var:real?
		jnz L99								!mixed

		movq xmm0,[D4+kvalue]
		addsd xmm0,[Dsptr+kvalue]
		movq [D4+kvalue],xmm0
		*popvar2
		*jumpskip1
L2:
L99:
	end

	saveregs
	k_addto()
	loadregs
	jumpnext
end

threadedproc j_subto*=
	assem
		mov D4,[Dsptr+xb+kvarptr]
		cmp byte [Dsptr+xb+ktag],trefvar	!lhs is ref var?
		jnz L99
		cmp byte [Dsptr+ya+ktag],tint		!rhs is int
		jnz L1
		cmp byte [D4+ktag],tint			!lhs is ref var:int?
		jnz L99
		mov D1,[Dsptr+kvalue]
		mov D0,[D4+kvalue]
		sub D0,D1
		mov [D4+kvalue],D0
		*popvar2
		*jumpskip1
L1:
		cmp byte [Dsptr+ya+ktag],treal		!rhs is real
		jnz L2
		cmp byte [D4+ktag],treal			!lhs is ref var:real?
		jnz L99								!mixed

		movq xmm0,[D4+kvalue]
		subsd xmm0,[Dsptr+kvalue]
		movq [D4+kvalue],xmm0
		*popvar2
		*jumpskip1
L2:
L99:
	end

	saveregs
	k_subto()
	loadregs
	jumpnext
end

threadedproc j_multo*=
	saveregs
	k_multo()
	loadregs
	jumpnext
end

threadedproc j_divto*=
	saveregs
	k_divto()
	loadregs
	jumpnext
end

threadedproc j_idivto*=
	saveregs
	k_idivto()
	loadregs
	jumpnext
end

threadedproc j_andlto*=
	saveregs
	k_andlto()
	loadregs
	jumpnext
end

threadedproc j_orlto*=
	saveregs
	k_orlto()
	loadregs
	jumpnext
end

threadedproc j_iandto*=
	assem
		cmp byte [Dsptr+xb+ktag],trefvar	!lhs is ref var?
		jnz L1
		cmp byte [Dsptr+ya+ktag],tint		!rhs is int
		jnz L1
		mov D4,[Dsptr+xb+kvarptr]
		cmp byte [D4+ktag],tint			!lhs is ref var:int?
		jnz L1
		mov D1,[Dsptr+kvalue]
		mov D0,[D4+kvalue]
		and D0,D1
		mov [D4+kvalue],D0
		*popvar2
		*jumpskip1
L1:
	end

	saveregs
	k_iandto()
	loadregs
	jumpnext
end

threadedproc j_iorto*=
	assem
		cmp byte [Dsptr+xb+ktag],trefvar	!lhs is ref var?
		jnz L1
		cmp byte [Dsptr+ya+ktag],tint		!rhs is int
		jnz L1
		mov D4,[Dsptr+xb+kvarptr]
		cmp byte [D4+ktag],tint			!lhs is ref var:int?
		jnz L1
		mov D1,[Dsptr+kvalue]
		mov D0,[D4+kvalue]
		or D0,D1
		mov [D4+kvalue],D0
		*popvar2
		*jumpskip1
L1:
	end

	saveregs
	k_iorto()
	loadregs
	jumpnext
end

threadedproc j_ixorto*=
	assem
		cmp byte [Dsptr+xb+ktag],trefvar	!lhs is ref var?
		jnz L1
		cmp byte [Dsptr+ya+ktag],tint		!rhs is int
		jnz L1
		mov D4,[Dsptr+xb+kvarptr]
		cmp byte [D4+ktag],tint			!lhs is ref var:int?
		jnz L1
		mov D1,[Dsptr+kvalue]
		mov D0,[D4+kvalue]
		xor D0,D1
		mov [D4+kvalue],D0
		*popvar2
		*jumpskip1
L1:
	end

	saveregs
	k_ixorto()
	loadregs
	jumpnext
end

threadedproc j_shlto*=
	assem
		cmp byte [Dsptr+xb+ktag],trefvar	!lhs is ref var?
		jnz L1
		cmp byte [Dsptr+ya+ktag],tint		!rhs is int
		jnz L1
		mov D4,[Dsptr+xb+kvarptr]
		cmp byte [D4+ktag],tint			!lhs is ref var:int?
		jnz L1
		mov cl,[Dsptr+kvalue]
!	shl word64 [D4+kvalue],cl
		mov D0,[D4+kvalue]
		shl D0,cl
		mov [D4+kvalue],D0
		*popvar2
		*jumpskip1
L1:
	end

	saveregs
	k_shlto()
	loadregs
	jumpnext
end

threadedproc j_shrto*=
	saveregs
	k_shrto()
	loadregs
	jumpnext
end

threadedproc j_minto*=
	saveregs
	k_minto()
	loadregs
	jumpnext
end

threadedproc j_maxto*=
	saveregs
	k_maxto()
	loadregs
	jumpnext
end

threadedproc j_concatto*=
	saveregs
	k_concatto()
	loadregs
	jumpnext
end

threadedproc j_appendto*=
	saveregs
	k_appendto()
	loadregs
	jumpnext
end

threadedproc j_dot*=

	assem
JMP L99
!		cmp byte [Dsptr+ktag],trecord
!		jnz L99
!		movzx D3, word16 [Dsptr+kusertag]		!rectype: actual record type
!		mov D5, [Dprog+kopnda]			!index: (genfieldindex)
!		and D5,D5
!		jz L99							!'not a field' error?
!
!		mov D4, [D5*8+genfieldtable-8]	!g: pointer to genfieldrec
!
!L1:		and D4,D4
!		jz L99							!no more entries
!		mov D0,[D4+gdef]				!d: g.def
!		mov D1,[D0+sowner]				!d.owner
!		cmp W3,[D1+smode]				!rectype=d.owner.mode
!		jz L3							!found 
!		mov D4,[D4+gnextdef]
!		jmp L1							!next genfield
!!found possible field in d in D0
!L3:		cmp byte [D0+snameid],fieldid
!		jnz L99							!not a field; don't handle that here
!!got a regular field; now find the offset in the record
!		mov D3,[Dsptr+kobjptr]
!		mov D3,[D3+jvarptr]				!point to record fields
!
!		movzx D0,word16 [D0+soffset]
!		add D3,D0
!
!		mov D0,[D3]
!		mov D1,[D3+kvalue]
!		test W0,hasrefmask
!		jz L4
!		inc word32 [D1+jrefcount]
!L4:
!		push D0
!		push D1
!		*callunshareu_dsptr				!unshare the record
!		pop D1
!		pop D0
!		mov [Dsptr],D0
!		mov [Dsptr+kvalue],D1
!		*jumpskip2
	end
L99:

	saveregs
	k_dot()
	loadregs
	jumpnext
end

threadedproc j_index*=
	static varrec v

	assem
JMP L99
		cmp byte [Dsptr+ya+ktag],tint
		jnz L99

!int index:
		mov D6,[Dsptr+xb+ktag]
		cmp B6,tlist
		jnz L2

!list[int]
		mov D5,[Dsptr+xb+kobjptr]

		mov D4,[Dsptr+ya+kvalue]	!index
		movsx D3,word16[D5+jlower16]
		sub D4,D3					!0-base
		cmp D4,[D5+jlength]
		jae L99						!bounds error: let M deal with it

		shl A4,varshift				!index*varsize
		add D4,[D5+jvarptr]			!point to element

		*popvar						!pop list, stack contains list descriptor

		mov D0,[D4+ktag]
		mov [Dsptr+ktag],D0			!replace index by list element

		mov D1,[D4+kvalue]
		mov [Dsptr+kvalue],D1

		and A0,hasrefmask
		jz L11
		inc word32 [D1+jrefcount]
L11:
		dec word32 [D5+jrefcount]	!dec count of original list
		jnz L12
		mov [v+ktag],D6
		mov [v+kobjptr],D5
		lea D10,[v]
		*callvarfree
L12:
		*jumpskip1

L2:
L3:
L99:
	end


	saveregs
	k_index()
	loadregs
	jumpnext
end

threadedproc j_dotindex*=
	saveregs
	k_dotindex()
	loadregs
	jumpnext
end

threadedproc j_keyindex*=
	saveregs
	k_keyindex()
	loadregs
	jumpnext
end

threadedproc j_dotref*=
	saveregs
	k_dotref()
	loadregs
	jumpnext
end

threadedproc j_indexref*=
	static varrec v

	assem
!JMP L99
		cmp byte [Dsptr+ya+ktag],tint
		jnz L99

!int index:
		mov D6,[Dsptr+xb+ktag]
		cmp B6,tlist
		jnz L2

!list[int]
		mov D5,[Dsptr+xb+kobjptr]

		mov D4,[Dsptr+ya+kvalue]	!index
		movsx D3, word16[D5+jlower16]
		sub D4,D3					!0-base
		cmp D4,[D5+jlength]
		jae L99						!bounds error: let M deal with it

		shl A4,varshift				!index*varsize
		add D4,[D5+jvarptr]			!point to element

		*popvar						!pop list, stack contains list descriptor

		dec word32 [D5+jrefcount]	!dec count of original list
		jnz L12
		mov D10,Dsptr
		*callvarfree
L12:
		mov word32 [Dsptr+ktag],trefvar
		mov [Dsptr+kobjptr],D4

		*jumpskip1

L2:
L3:
L99:
	end

	saveregs
	k_indexref()
	loadregs
	jumpnext
end

threadedproc j_dotindexref*=
	saveregs
	k_dotindexref()
	loadregs
	jumpnext
end

threadedproc j_keyindexref*=
	saveregs
	k_keyindexref()
	loadregs
	jumpnext
end

threadedproc j_popdot*=
	assem
JMP L99
!		cmp byte [Dsptr+ktag],trecord
!		jnz L99
!
!		movzx D3, word16 [Dsptr+kusertag]		!rectype: actual record type
!		mov D5, [Dprog+kopnda]			!index: (genfieldindex)
!		and D5,D5
!		jz L99							!'not a field' error?
!
!		mov D4,[Dsptr+kobjptr]
!		mov B0,[D4+jmutable]
!		and B0,1
!		jz L99						!not mutable
!
!		mov D4, [D5*8+genfieldtable-8]	!g: pointer to genfieldrec
!
!L1:		and D4,D4
!		jz L99							!no more entries
!		mov D0,[D4+gdef]				!d: g.def
!		mov D1,[D0+sowner]				!d.owner
!		cmp W3,[D1+smode]				!rectype=d.owner.mode
!		jz L3							!found 
!		mov D4,[D4+gnextdef]
!		jmp L1							!next genfield
!!found possible field in d in D0
!L3:		cmp byte [D0+snameid],fieldid
!		jnz L99							!not a field; don't handle that here
!
!!got a regular field; now find the offset in the record
!		mov D3,[Dsptr+kobjptr]
!		mov D3,[D3+jvarptr]				!point to record fields
!
!		movzx D0,word16 [D0+soffset]
!		add D3,D0
!!D3 points to dest field; need to unshare first
!		cmp byte [D3+khasref],1
!		jnz L5
!		mov D10,D3
!		*callunshareu
!L5:
!		mov D0,[Dsptr+xb]				!get value next below stack
!		mov D1,[Dsptr+xb+kvalue]
!		test W0,hasrefmask
!		jz L4
!		inc word32 [D1+jrefcount]
!L4:
!		push D0
!		push D1
!		*callunshareu_dsptr				!unshare the record
!		pop D1
!		pop D0
!		*popvar2						!lose record, and value that is now in D0/D1
!		mov [D3],D0
!		mov [D3+kvalue],D1
!		*jumpskip2
	end
L99:

	saveregs
	k_popdot()
	loadregs
	jumpnext
end

threadedproc j_popindex*=
	static varrec v
	assem
JMP L99

		cmp byte [Dsptr+za+ktag],tint
		jnz L99

!int index:
		mov D6,[Dsptr+yb+ktag]
		cmp B6,tlist
		jnz L99

!list[int]
		mov D5,[Dsptr+yb+kobjptr]
		mov B0,[D5+jmutable]
		and B0,1
		jz L99						!not mutable

		mov D4,[Dsptr+ya+kvalue]	!index
		movsx D3,word16[D5+jlower16]
		sub D4,D3		!0-base
		cmp D4,[D5+jlength]
		jae L99						!bounds error or extend: let M deal with it

		shl A4,varshift				!index*varsize
		add D4,[D5+jvarptr]			!point to element

		mov D0,[Dsptr+xc]			!xfer ref count
		mov D1,[Dsptr+xc+kvalue]
		mov [D4],D0
		mov [D4+kvalue],D1

		dec word32 [D5+jrefcount]	!dec count of original list
		jnz L12
		mov [v+ktag],D6
		mov [v+kobjptr],D5
		lea D10,[v]
		*callvarfree
L12:
		*popvar3
		*jumpskip1

L99:

	end

	saveregs
	k_popindex()
	loadregs
	jumpnext

L34:

end

threadedproc j_popdotindex*=
	saveregs
	k_popdotindex()
	loadregs
	jumpnext
end

threadedproc j_popkeyindex*=
	saveregs
	k_popkeyindex()
	loadregs
	jumpnext
end

threadedproc j_expand*=
	saveregs
	k_expand()
	loadregs
	jumpnext
end

threadedproc j_pushtry*=
	saveregs
	k_pushtry()
	loadregs
	jumpnext
end

threadedproc j_raise*=
	saveregs
	k_raise()
	loadregs
	jumpnext
end

threadedproc j_pushoperator*=
	saveregs
	k_pushoperator()
	loadregs
	jumpnext
end

threadedproc j_maps*=
	saveregs
	k_maps()
	loadregs
	jumpnext
end

threadedproc j_mapss*=
	saveregs
	k_mapss()
	loadregs
	jumpnext
end

threadedproc j_addsp*=
	saveregs
	k_addsp()
	loadregs
	jumpnext
end

threadedproc j_pushff*=
	assem
		*pushvar2
		mov D4,[Dprog+kopnda]
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov D4,[Dprog+kopndb]
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+ya+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+ya+kvalue],D1
		and A0,hasrefmask
		jz L3
		inc word32 [D1+jrefcount]
L3:
		*jumpskip4
	end
end

threadedproc j_pushmm*=
	assem
		*pushvar2
		mov D4,[Dprog+kopnda]
		mov D0,[D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov D4,[Dprog+kopndb]
		mov D0,[D4+ktag]
		mov [Dsptr+ya+ktag],D0
		mov D1,[D4+kvalue]
		mov [Dsptr+ya+kvalue],D1
		and A0,hasrefmask
		jz L3
		inc word32 [D1+jrefcount]
L3:
		*jumpskip4
	end
end

threadedproc j_pushfm*=
	assem
		*pushvar2
		mov D4,[Dprog+kopnda]
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov D4,[Dprog+kopndb]
		mov D0,[D4+ktag]
		mov [Dsptr+ya+ktag],D0
		mov D1,[D4+kvalue]
		mov [Dsptr+ya+kvalue],D1
		and A0,hasrefmask
		jz L3
		inc word32 [D1+jrefcount]
L3:
		*jumpskip4
	end
end

threadedproc j_pushmf*=
	assem
		*pushvar2
		mov D4,[Dprog+kopnda]
		mov D0,[D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov D4,[Dprog+kopndb]
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+ya+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+ya+kvalue],D1
		and A0,hasrefmask
		jz L3
		inc word32 [D1+jrefcount]
L3:
		*jumpskip4
	end
end

threadedproc j_pushfff*=
	assem
		*pushvar3
		mov D4,[Dprog+kopnda]
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xc+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xc+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov D4,[Dprog+kopndb]
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+yb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+yb+kvalue],D1
		and A0,hasrefmask
		jz L3
		inc word32 [D1+jrefcount]
L3:
		mov D4,[Dprog+kopndc]
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+za+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+za+kvalue],D1
		and A0,hasrefmask
		jz L4
		inc word32 [D1+jrefcount]
L4:
		*jumpskip6
	end
end


threadedproc j_nop2*=
	jumpskip2
end

threadedproc j_skip*=
	jumpskip1
end

threadedproc j_pushci0*=
	assem
		*pushvar
		mov word32 [Dsptr+ktag],tint
		xor D0,D0
		mov [Dsptr+kvalue],D0
		*jumpskip2
	end
end

threadedproc j_moveff*=
	assem
		mov D5,[Dprog+kopndb]
		cmp byte [Dframe+D5+khasref],1
		jnz L1
		mov D1,[Dframe+D5+kobjptr]
		inc word32 [D1+jrefcount]		!increment before freeing (in case of a:=a)
	L1:
		mov D4,[Dprog+kopnda]
		add D4,Dframe
		cmp byte [D4+khasref],1
		jnz L2
		*callunshareu_d4
	L2:
		mov D5,[Dprog+kopndb]
		mov D0,[Dframe+D5+ktag]
		mov [D4+ktag],D0
		mov D1,[Dframe+D5+kvalue]
		mov [D4+kvalue],D1

		*jumpskip4
	end
end

threadedproc j_zmoveff*=
	assem
		mov D5,[Dprog+kopndb]
		cmp byte [Dframe+D5+khasref],1
		jnz L1
		mov D1,[Dframe+D5+kobjptr]
		inc word32 [D1+jrefcount]		!increment before freeing (in case of a:=a)
	L1:
		mov D4,[Dprog+kopnda]
		add D4,Dframe
		mov D5,[Dprog+kopndb]
		mov D0,[Dframe+D5+ktag]
		mov [D4+ktag],D0
		mov D1,[Dframe+D5+kvalue]
		mov [D4+kvalue],D1

		*jumpskip4
	end
end

threadedproc j_movefm*=
	assem
		mov D5,[Dprog+kopndb]
		cmp byte [D5+khasref],1
		jnz L1
		mov D1,[D5+kobjptr]
		inc word32 [D1+jrefcount]
	L1:
		mov D4,[Dprog+kopnda]
		add D4,Dframe
		cmp byte [D4+khasref],1
		jnz L2
		*callunshareu_d4
	L2:
		mov D5,[Dprog+kopndb]
		mov D0,[D5+ktag]
		mov [D4+ktag],D0
		mov D1,[D5+kvalue]
		mov [D4+kvalue],D1

		*jumpskip4
	end
end

threadedproc j_movemf*=
	assem
		mov D5,[Dprog+kopndb]
		cmp byte [Dframe+D5+khasref],1
		jnz L1
		mov D1,[Dframe+D5+kobjptr]
		inc word32 [D1+jrefcount]
	L1:
		mov D4,[Dprog+kopnda]
		cmp byte [D4+khasref],1
		jnz L2
		*callunshareu_d4
	L2:
		mov D5,[Dprog+kopndb]
		mov D0,[Dframe+D5+ktag]
		mov [D4+ktag],D0
		mov D1,[Dframe+D5+kvalue]
		mov [D4+kvalue],D1

		*jumpskip4
	end
end

threadedproc j_movemm*=
	assem
		mov D5,[Dprog+kopndb]
		cmp byte [D5+khasref],1
		jnz L1
		mov D1,[D5+kobjptr]
		inc word32 [D1+jrefcount]
	L1:
		mov D4,[Dprog+kopnda]
		cmp byte [D4+khasref],1
		jnz L2
		*callunshareu_d4
	L2:
		mov D5,[Dprog+kopndb]
		mov D0,[D5+ktag]
		mov [D4+ktag],D0
		mov D1,[D5+kvalue]
		mov [D4+kvalue],D1

		*jumpskip4
	end
end

threadedproc j_movefci*=
	assem
		mov D4,[Dprog+kopnda]
		add D4,Dframe
		cmp byte [D4+khasref],1
		jnz L1
		*callunshareu_d4
	L1:
		mov word32 [D4+ktag],tint
		mov D0,[Dprog+kopndb]
		mov [D4+kvalue],D0
		*jumpskip4
	end
end

threadedproc j_zmovefci*=
	assem
		mov D4,[Dprog+kopnda]
		add D4,Dframe
	L1:
		mov word32 [D4+ktag],tint
		mov D0,[Dprog+kopndb]
		mov [D4+kvalue],D0
		*jumpskip4
	end
end

threadedproc j_movemci*=
	assem
		mov D4,[Dprog+kopnda]
		cmp byte [D4+khasref],1
		jnz L1
		*callunshareu_d4
	L1:
		mov word32 [D4+ktag],tint
		mov D0,[Dprog+kopndb]
		mov [D4+kvalue],D0
		*jumpskip4
	end
end

threadedproc j_pushvoid2*=
	assem
		*pushvar2
		mov word32 [Dsptr+ya+ktag],tvoid
		mov word32 [Dsptr+xb+ktag],tvoid
		*jumpskip2
	end
end

threadedproc j_pushvoid3*=
	assem
		*pushvar3
		mov word32 [Dsptr+za+ktag],tvoid
		mov word32 [Dsptr+yb+ktag],tvoid
		mov word32 [Dsptr+xc+ktag],tvoid
		*jumpskip3
	end
end

threadedproc j_unshare1*=
	assem
		cmp byte [Dsptr+khasref],1
		jnz L1
		*callunshareu_dsptr
L1:		*popvar
		*jumpskip2
	end
end

threadedproc j_unshare2*=
	assem
		cmp byte [Dsptr+ya+khasref],1
		jnz L1
		*callunshareu_dsptr
L1:
		cmp byte [Dsptr+xb+khasref],1
		jnz L2
		lea D10,[Dsptr+xb]
		*callunshareu
L2:		*popvar2

		*jumpskip2
	end
end

threadedproc j_unshare3*=
	assem
		cmp byte [Dsptr+za+khasref],1
		jnz L1
		*callunshareu_dsptr
L1:
		cmp byte [Dsptr+yb+khasref],1
		jnz L2
		lea D10,[Dsptr+yb]
		*callunshareu
L2:
		cmp byte [Dsptr+xc+khasref],1
		jnz L3
		lea D10,[Dsptr+xc]
		*callunshareu
L3:		*popvar3

		*jumpskip2
	end
end

threadedproc j_procentry1*=
	assem
		*pushvar
		mov word32 [Dsptr+ktag],tvoid
		*jumpskip2
	end
end

threadedproc j_procentry2*=
	assem
		*pushvar2
		mov word32 [Dsptr+ya+ktag],tvoid
		mov word32 [Dsptr+xb+ktag],tvoid
		*jumpskip2
	end
end

threadedproc j_jumpeqfci*=
	assem
		mov D4,[Dprog+kopndb]
		mov D5,[Dprog+kopndc]
		cmp byte [D4+Dframe+ktag],tint
		jnz L99

		mov D0,[Dframe+D4+kvalue]
		cmp D0,D5
		jnz Lfalse
		mov Dprog,[Dprog+kopnda]
		*jumpnext
Lfalse:
		*jumpskip6
L99:
		*pushvar2
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov word32 [Dsptr+ya+ktag],tint
		mov [Dsptr+ya+kvalue],D5

		add Dprog,intpsize4
		jmp j_jumpeq
!		*jumpnext
	end
end

threadedproc j_jumpnefci*=
	assem
		mov D4,[Dprog+kopndb]
		mov D5,[Dprog+kopndc]
		cmp byte [D4+Dframe+ktag],tint
		jnz L99

		mov D0,[Dframe+D4+kvalue]
		cmp D0,D5
		jz Lfalse
		mov Dprog,[Dprog+kopnda]
		*jumpnext
Lfalse:
		*jumpskip6
L99:
		*pushvar2
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov word32 [Dsptr+ya+ktag],tint
		mov [Dsptr+ya+kvalue],D5

		add Dprog,intpsize4
		jmp j_jumpne
!		*jumpnext
	end
end

threadedproc j_jumpltfci*=
	assem
		mov D4,[Dprog+kopndb]
		mov D5,[Dprog+kopndc]
		cmp byte [D4+Dframe+ktag],tint
		jnz L99

		mov D0,[Dframe+D4+kvalue]
		cmp D0,D5
		jge Lfalse
		mov Dprog,[Dprog+kopnda]
		*jumpnext
Lfalse:
		*jumpskip6
L99:
		*pushvar2
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov word32 [Dsptr+ya+ktag],tint
		mov [Dsptr+ya+kvalue],D5

		add Dprog,intpsize4
		jmp j_jumplt
!		*jumpnext
	end
end

threadedproc j_jumplefci*=
	assem
		mov D4,[Dprog+kopndb]
		mov D5,[Dprog+kopndc]
		cmp byte [D4+Dframe+ktag],tint
		jnz L99

		mov D0,[Dframe+D4+kvalue]
		cmp D0,D5
		jg Lfalse
		mov Dprog,[Dprog+kopnda]
		*jumpnext
Lfalse:
		*jumpskip6
L99:
		*pushvar2
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov word32 [Dsptr+ya+ktag],tint
		mov [Dsptr+ya+kvalue],D5

		add Dprog,intpsize4
		jmp j_jumple
!		*jumpnext
	end
end

threadedproc j_jumpgefci*=
	assem
		mov D4,[Dprog+kopndb]
		mov D5,[Dprog+kopndc]
		cmp byte [D4+Dframe+ktag],tint
		jnz L99

		mov D0,[Dframe+D4+kvalue]
		cmp D0,D5
		jl Lfalse
		mov Dprog,[Dprog+kopnda]
		*jumpnext
Lfalse:
		*jumpskip6
L99:
		*pushvar2
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov word32 [Dsptr+ya+ktag],tint
		mov [Dsptr+ya+kvalue],D5

		add Dprog,intpsize4
		jmp j_jumpge
!		*jumpnext
	end
end

threadedproc j_jumpgtfci*=
	assem
		mov D4,[Dprog+kopndb]
		mov D5,[Dprog+kopndc]
		cmp byte [D4+Dframe+ktag],tint
		jnz L99

		mov D0,[Dframe+D4+kvalue]
		cmp D0,D5
		jle Lfalse
		mov Dprog,[Dprog+kopnda]
		*jumpnext
Lfalse:
		*jumpskip6
L99:
		*pushvar2
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov word32 [Dsptr+ya+ktag],tint
		mov [Dsptr+ya+kvalue],D5

		add Dprog,intpsize4
		jmp j_jumpgt
!		*jumpnext
	end
end

threadedproc j_switchf*=
	assem
		mov D3,[Dprog+kopnda]
		cmp word16 [D3+Dframe+ktag],tint
		jnz L99							!get M deal with errors
		mov D4,[D3+Dframe+kvalue]		!switch index
		sub D4,[Dprog+kopndc]			!index-lower! now 0-based index
		cmp D4,[Dprog+kopndb]			!index0>=n?
		jae L2							!out of range
!in range
		shl D4,1
		mov Dprog,[Dprog+D4*8+intpsize6]
		*jumpnext
!out of range
	L2:
		mov D5,[Dprog+kopndb]
		shl D5,1
		mov Dprog,[Dprog+D5*8+intpsize6]
		*jumpnext

	L99:
	end
	pcerror("jswitchf/not int")
end

threadedproc j_addfci*=
	assem
		mov D4,[Dprog+kopnda]
		mov D5,[Dprog+kopndb]
		cmp byte [D4+Dframe+ktag],tint
		jnz L1
		*pushvar
		mov word32 [Dsptr+ktag],tint
		mov D0,[Dframe+D4+kvalue]
		add D0,D5
		mov [Dsptr+kvalue],D0
		*jumpskip5
L1:
		*pushvar2

		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:

		mov word32 [Dsptr+ya+ktag],tint
		mov [Dsptr+ya+kvalue],D5
		add Dprog,intpsize4
		jmp j_add
!		*jumpnext
	end
end

threadedproc j_subfci*=
	assem
		mov D4,[Dprog+kopnda]
		mov D5,[Dprog+kopndb]
		cmp byte [D4+Dframe+ktag],tint
		jnz L1
		*pushvar
		mov word32 [Dsptr+ktag],tint
		mov D0,[Dframe+D4+kvalue]
		sub D0,D5
		mov [Dsptr+kvalue],D0
		*jumpskip5
L1:
		*pushvar2

		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:

		mov word32 [Dsptr+ya+ktag],tint
		mov [Dsptr+ya+kvalue],D5
		add Dprog,intpsize4
		jmp j_sub
!		*jumpnext
	end
end

threadedproc j_indexff*=
	assem
		mov D2,[Dprog+kopnda]
		mov D3,[Dprog+kopndb]
!JMP L99
		cmp byte [D2+Dframe+ktag],tlist
		jnz L99
		cmp byte [D3+Dframe+ktag],tint
		jnz L99

!list[int]
		mov D6,[D2+Dframe+ktag]

		mov D5,[D2+Dframe+kobjptr]
		mov D4,[D3+Dframe+kvalue]		!index
		movsx D6,word16[D5+jlower16]
		sub D4,D6						!0-base
		cmp D4,[D5+jlength]
		jae L99					!bounds error: let B deal with it

!jmp L99
		shl A4,varshift				!index*varsize
		add D4,[D5+jvarptr]			!point to element

		*pushvar
		mov D0,[D4+ktag]
		mov [Dsptr+ktag],D0			!replace index by list element
		mov D1,[D4+kvalue]
		mov [Dsptr+kvalue],D1
		and A0,hasrefmask
		jz L1
		inc word32 [D1+jrefcount]
L1:
		*jumpskip5

L99:
		*pushvar2
		mov D0,[Dframe+D2+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D2+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L12
		inc word32 [D1+jrefcount]
L12:
		mov D0,[Dframe+D3+ktag]
		mov [Dsptr+ya+ktag],D0
		mov D1,[Dframe+D3+kvalue]
		mov [Dsptr+ya+kvalue],D1
!		and A0,hasrefmask				!not needed for int/range index
!		jz L13
!		inc word32 [D1+jrefcount]
L13:

		add Dprog,intpsize4
		jmp j_index
!		*jumpnext
	end

end

threadedproc j_addff*=
	assem
		mov D4,[Dprog+kopnda]
		mov D5,[Dprog+kopndb]
		cmp byte [D4+Dframe+ktag],tint
		jnz L1
		cmp byte [D5+Dframe+ktag],tint
		jnz L1
		*pushvar
		mov word32 [Dsptr+xa+ktag],tint
		mov D0,[Dframe+D4+kvalue]
		add D0,[Dframe+D5+kvalue]
		mov [Dsptr+kvalue],D0
		*jumpskip5

L1:
		*pushvar2
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov D0,[Dframe+D5+ktag]
		mov [Dsptr+ya+ktag],D0
		mov D1,[Dframe+D5+kvalue]
		mov [Dsptr+ya+kvalue],D1
		and A0,hasrefmask
		jz L3
		inc word32 [D1+jrefcount]
L3:

		add Dprog,intpsize4
		jmp j_add
	end
end

threadedproc j_subff*=
	assem
		mov D4,[Dprog+kopnda]
		mov D5,[Dprog+kopndb]
		cmp byte [D4+Dframe+ktag],tint
		jnz L1
		cmp byte [D5+Dframe+ktag],tint
		jnz L1
		*pushvar
		mov word32 [Dsptr+xa+ktag],tint
		mov D0,[Dframe+D4+kvalue]
		sub D0,[Dframe+D5+kvalue]
		mov [Dsptr+kvalue],D0
		*jumpskip5

L1:
		*pushvar2
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov D0,[Dframe+D5+ktag]
		mov [Dsptr+ya+ktag],D0
		mov D1,[Dframe+D5+kvalue]
		mov [Dsptr+ya+kvalue],D1
		and A0,hasrefmask
		jz L3
		inc word32 [D1+jrefcount]
L3:

		add Dprog,intpsize4
		jmp j_sub
	end
end

threadedproc j_pushincrptrm *=
	assem
		mov D2,[Dprog+kopnda]
		jmp j_pushincrptrf.L0
	end
end

threadedproc j_pushincrptrf *=
!do pushf/loadincr/pushptr, optimised for byte-pointers
!used in the rvalue: p++^ 

	assem
		mov D2,[Dprog+kopnda]
		add D2,Dframe
!JMP L99
L0:
		cmp byte [D2+ktag],trefpack
		jnz L99							!not pointer to packed type
		cmp byte [D2+krefelemtag],tu8
		jnz L99

!pointer to byte
		mov D5,[D2+kptr]
		inc word64 [D2+kptr]
		movzx A0,byte [D5]

		*pushvar
		mov word32 [Dsptr+ktag],tint
		mov [Dsptr+kvalue],D0
		*jumpskip4

L99:
		*pushvar
		mov word32 [Dsptr+ktag],trefvar
		mov [Dsptr+kvarptr],D2
		add Dprog,intpsize2			!point at loadincr (with pushptr next)
		jmp j_loadincr
	end
end

threadedproc j_popincrptrm *=
	assem
		mov D2,[Dprog+kopnda]
		jmp j_popincrptrf.L0
	end
end

threadedproc j_popincrptrf *=
	assem
		mov D2,[Dprog+kopnda]
		add D2,Dframe
L0:
!JMP L99
		cmp byte [D2+ktag],trefpack
		jnz L99							!not pointer to packed type
		cmp byte [Dsptr+ktag],tint
		jnz L99							!let M deal with conversions or errors

		cmp byte [D2+krefelemtag],tu8
		jnz L2

!pointer to byte
		mov D5,[D2+kptr]
		inc word64 [D2+kptr]

		mov D0,[Dsptr+kvalue]
		mov [D5],B0
		*popvar
		*jumpskip4

L2:
!		cmp word16 [D2+krefelemtag],ti32
		cmp word32 [D2+krefelemtag],ti32
		jnz L3

!pointer to int32
		mov D5,[D2+kptr]
		add word64 [D2+kptr],4

		mov D0,[Dsptr+kvalue]
		mov [D5],A0
		*popvar
		*jumpskip4
L3:
L99:
		*pushvar
		mov word32 [Dsptr+ktag],trefvar
		lea D0,[D2]
		mov [Dsptr+kvarptr],D0
		add Dprog,intpsize2			!point at loadincr (with pushptr next)
		jmp j_loadincr
	end

end

threadedproc j_jumpltff*=
	assem
		mov D4,[Dprog+kopndb]
		mov D5,[Dprog+kopndc]
		cmp byte [D4+Dframe+ktag],tint
		jnz L99
		cmp byte [D5+Dframe+ktag],tint
		jnz L99

		mov D0,[Dframe+D4+kvalue]
		cmp D0,[Dframe+D5+kvalue]
		jge Lfalse
		mov Dprog,[Dprog+kopnda]
		*jumpnext
Lfalse:
		*jumpskip6
L99:
		*pushvar2
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov D0,[Dframe+D5+ktag]
		mov [Dsptr+ya+ktag],D0
		mov D1,[Dframe+D5+kvalue]
		mov [Dsptr+ya+kvalue],D1
		and A0,hasrefmask
		jz L3
		inc word32 [D1+jrefcount]
L3:
		add Dprog,intpsize4
		jmp j_jumplt
	end
end

threadedproc j_jumpleff*=
	assem
		mov D4,[Dprog+kopndb]
		mov D5,[Dprog+kopndc]
		cmp byte [D4+Dframe+ktag],tint
		jnz L99
		cmp byte [D5+Dframe+ktag],tint
		jnz L99

		mov D0,[Dframe+D4+kvalue]
		cmp D0,[Dframe+D5+kvalue]
		jg Lfalse
		mov Dprog,[Dprog+kopnda]
		*jumpnext
Lfalse:
		*jumpskip6
L99:
		*pushvar2
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov D0,[Dframe+D5+ktag]
		mov [Dsptr+ya+ktag],D0
		mov D1,[Dframe+D5+kvalue]
		mov [Dsptr+ya+kvalue],D1
		and A0,hasrefmask
		jz L3
		inc word32 [D1+jrefcount]
L3:
		add Dprog,intpsize4
		jmp j_jumple
	end
end

threadedproc j_jumpgeff*=
	assem
		mov D4,[Dprog+kopndb]
		mov D5,[Dprog+kopndc]
		cmp byte [D4+Dframe+ktag],tint
		jnz L99
		cmp byte [D5+Dframe+ktag],tint
		jnz L99

		mov D0,[Dframe+D4+kvalue]
		cmp D0,[Dframe+D5+kvalue]
		jl Lfalse
		mov Dprog,[Dprog+kopnda]
		*jumpnext
Lfalse:
		*jumpskip6
L99:
		*pushvar2
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov D0,[Dframe+D5+ktag]
		mov [Dsptr+ya+ktag],D0
		mov D1,[Dframe+D5+kvalue]
		mov [Dsptr+ya+kvalue],D1
		and A0,hasrefmask
		jz L3
		inc word32 [D1+jrefcount]
L3:
		add Dprog,intpsize4
		jmp j_jumpge
	end
end

threadedproc j_jumpgtff*=
	assem
		mov D4,[Dprog+kopndb]
		mov D5,[Dprog+kopndc]
		cmp byte [D4+Dframe+ktag],tint
		jnz L99
		cmp byte [D5+Dframe+ktag],tint
		jnz L99

		mov D0,[Dframe+D4+kvalue]
		cmp D0,[Dframe+D5+kvalue]
		jle Lfalse
		mov Dprog,[Dprog+kopnda]
		*jumpnext
Lfalse:
		*jumpskip6
L99:
		*pushvar2
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov D0,[Dframe+D5+ktag]
		mov [Dsptr+ya+ktag],D0
		mov D1,[Dframe+D5+kvalue]
		mov [Dsptr+ya+kvalue],D1
		and A0,hasrefmask
		jz L3
		inc word32 [D1+jrefcount]
L3:
		add Dprog,intpsize4
		jmp j_jumpgt
	end
end

threadedproc j_jumpeqff*=
	assem
		mov D4,[Dprog+kopndb]
		mov D5,[Dprog+kopndc]
		cmp byte [D4+Dframe+ktag],tint
		jnz L99
		cmp byte [D5+Dframe+ktag],tint
		jnz L99

		mov D0,[Dframe+D4+kvalue]
		cmp D0,[Dframe+D5+kvalue]
		jnz Lfalse
		mov Dprog,[Dprog+kopnda]
		*jumpnext
Lfalse:
		*jumpskip6
L99:
		*pushvar2
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov D0,[Dframe+D5+ktag]
		mov [Dsptr+ya+ktag],D0
		mov D1,[Dframe+D5+kvalue]
		mov [Dsptr+ya+kvalue],D1
		and A0,hasrefmask
		jz L3
		inc word32 [D1+jrefcount]
L3:
		add Dprog,intpsize4
		jmp j_jumpeq
	end
end

threadedproc j_jumpneff*=
	assem
		mov D4,[Dprog+kopndb]
		mov D5,[Dprog+kopndc]
		cmp byte [D4+Dframe+ktag],tint
		jnz L99
		cmp byte [D5+Dframe+ktag],tint
		jnz L99

		mov D0,[Dframe+D4+kvalue]
		cmp D0,[Dframe+D5+kvalue]
		jz Lfalse
		mov Dprog,[Dprog+kopnda]
		*jumpnext
Lfalse:
		*jumpskip6
L99:
		*pushvar2
		mov D0,[Dframe+D4+ktag]
		mov [Dsptr+xb+ktag],D0
		mov D1,[Dframe+D4+kvalue]
		mov [Dsptr+xb+kvalue],D1
		and A0,hasrefmask
		jz L2
		inc word32 [D1+jrefcount]
L2:
		mov D0,[Dframe+D5+ktag]
		mov [Dsptr+ya+ktag],D0
		mov D1,[Dframe+D5+kvalue]
		mov [Dsptr+ya+kvalue],D1
		and A0,hasrefmask
		jz L3
		inc word32 [D1+jrefcount]
L3:
		add Dprog,intpsize4
		jmp j_jumpne
	end
end

threadedproc j_pushptrf*=
	assem
		*pushvar
		mov D2,[Dprog+kopnda]
		add D2,Dframe
!JMP L99
		cmp byte [D2+ktag],trefvar
		jnz L1
		mov D4,[D2+kvarptr]

		mov D0,[D4+ktag]
		mov [Dsptr+ktag],D0
		mov D1,[D4+kvalue]
		mov [Dsptr+kvalue],D1
		and A0,hasrefmask
		jz L0
		inc word32 [D1+jrefcount]
L0:
		*jumpskip3

L1:
		cmp byte [D2+ktag],trefpack
		jnz L2

		mov D4,[D2+kptr]
		movzx A0,word16 [D2+krefelemtag]

		cmp A0,tu8
		jnz L10
		mov word32 [Dsptr+ktag],tint
		movzx A0,byte [D4]
		mov [Dsptr+kvalue],D0
		*jumpskip3
L10:
		cmp A0,ti32
		jnz L11
		mov word32 [Dsptr+ktag],tint
		mov A0,[D4]
		movsxd D0,A0
		mov [Dsptr+kvalue],D0
		*jumpskip3
L11:
L2:
L99:
		mov D0,[D2+ktag]
		mov [Dsptr+xa+ktag],D0
		mov D1,[D2+kvalue]
		mov [Dsptr+xa+kvalue],D1			!assume pointer is not a heap object
		add Dprog, intpsize2
		jmp j_pushptr
	end
end

threadedproc j_lenf*=
	assem
		mov D4,[Dprog+kopnda]
		add D4,Dframe
!JMP L99
		mov W0, [D4+ktag]
		cmp B0,tlist
		jz L1
		cmp B0,tstring
		jz L1
		cmp B0,tarray
		jnz L99
L1:
		mov D1,[D4+kobjptr]
		mov D3, [D1+jlength]

		*pushvar
		mov word32 [Dsptr+ktag],tint
		mov [Dsptr+kvalue],D3
		*jumpskip3
L99:
		*pushvar
		mov D0,[D4+ktag]
		mov [Dsptr+ktag],D0
		mov D1,[D4+kvalue]
		mov [Dsptr+kvalue],D1
		and A0,hasrefmask
		jz L3
		inc word32 [D1+jrefcount]
L3:	
		add Dprog,intpsize2
		jmp j_len
	end


end

threadedproc j_even*=
	saveregs
	k_even()
	loadregs
	jumpnext
end

threadedproc j_odd*=
	saveregs
	k_odd()
	loadregs
	jumpnext
end

threadedproc j_isfound*=
	saveregs
	k_isfound()
	loadregs
	jumpnext
end

=== qq_khandlers.m 0 0 11/31 ===
!HLL bytecode handlers

global macro getopnda = (pcptr+1)^
global macro getopndb = (pcptr+2)^
global macro getopndc = (pcptr+3)^
global macro getopndd = (pcptr+4)^

!step pcptr to next bytecode, skipping n operands
global macro skip(n) = pcptr:=pcptr+(n+1)


!const doretcheck=1
const doretcheck=0

global proc kunimpl=
	if hasbytecodes then
		pcerror_s("Unimplemented:",pclnames[pcptr^])
	else
		pcerror("Unimplemented (use -fdebug to see opcode)")
	fi
end

global proc k_pushci=
	++sptr
	sptr.tagx:=tint
	sptr.value:=getopnda
	skip(1)
end

global proc k_pushnil=
	++sptr
	sptr.tagx:=trefpack
	sptr.elemtag:=tvoid
	sptr.ptr:=nil
	++pcptr
end

global proc k_pushcs=
	++sptr

	sptr.tagx:=tstring ior hasrefmask
	sptr.objptr:=cast(getopnda)
	++sptr.objptr.refcount
	skip(1)
end

global proc k_pushcr=
	++sptr
	sptr.tagx:=treal

	sptr.xvalue:=real@(getopnda)
	skip(1)
end

global proc k_stop=
	stopped:=1
end

global proc k_stoprunproc=
	stopped:=1
end

global proc k_pushm=
	++sptr
	sptr^:=variant(getopnda)^
	var_share(sptr)

	skip(1)
end

global proc k_pushf=
	++sptr
	sptr^:=variant(frameptr+getopnda)^
	var_share(sptr)

	skip(1)
end

global proc k_pushff=
	sptr+:=2
	(sptr-1)^:=variant(frameptr+getopnda)^
	var_share(sptr-1)

	sptr^:=variant(frameptr+getopndb)^
	var_share(sptr)

	skip(3)
end

global proc k_pushmref=
	++sptr
	sptr.tagx:=trefvar
	sptr.varptr:=variant(getopnda)

	skip(1)
end

global proc k_pushfref=
	++sptr
	sptr.tagx:=trefvar
	sptr.varptr:=variant(frameptr+getopnda)

	skip(1)
end

global proc k_popm=
	variant p

	p:=variant(getopnda)
	var_unshare(p)
	p^:=sptr^				!transfer reference
	--sptr

	skip(1)
end

global proc k_storem=
	variant p

	p:=variant(getopnda)
	var_share(sptr)
	var_unshare(p)
	p^:=sptr^				!transfer reference

	skip(1)
end

global proc k_zpopm=
	(variant(getopnda))^:=sptr^				!transfer reference

	--sptr
	skip(1)
end

global proc k_popf=
	variant p

	p:=variant(frameptr+getopnda)
	var_unshare(p)
	p^:=sptr^				!transfer reference
	--sptr

	skip(1)
end

global proc k_storef=
	variant p

	p:=variant(frameptr+getopnda)
	var_share(sptr)
	var_unshare(p)
	p^:=sptr^				!transfer reference

	skip(1)
end

global proc k_zpopf=
	variant p

	p:=variant(frameptr+getopnda)
	p^:=sptr^				!transfer reference
	--sptr

	skip(1)
end

global proc k_popretval=
	variant p

	p:=variant(frameptr+getopnda)
	p^:=sptr^				!transfer reference
	--sptr

	skip(1)
end

global proc k_tom=
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

global proc k_add=
	variant y:=sptr--
	varrec x:=sptr^

	var_add(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_sub=
	variant y:=sptr--
	varrec x:=sptr^

	var_sub(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_mul=
	variant y:=sptr--
	varrec x:=sptr^

	var_mul(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_div=
	variant y:=sptr--
	varrec x:=sptr^

	var_div(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_idiv=
	variant y:=sptr--
	varrec x:=sptr^

	var_idiv(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_irem=
	variant y:=sptr--
	varrec x:=sptr^

	var_irem(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_iand=
	variant y:=sptr--
	varrec x:=sptr^

	var_iand(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_ior=
	variant y:=sptr--
	varrec x:=sptr^

	var_ior(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_ixor=
	variant y:=sptr--
	varrec x:=sptr^

	var_ixor(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_shl=
	variant y:=sptr--
	varrec x:=sptr^

	var_shl(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_shr=
	variant y:=sptr--
	varrec x:=sptr^

	var_shr(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_sqr=
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

global proc k_sign=
	case sptr.tag
	when tint then
		sptr.value:=(sptr.value<0|-1|(sptr.value>0|1|0))
	when treal then
		sptr.tag:=tint
		sptr.value:=(sptr.xvalue<0|-1|(sptr.xvalue>0|1|0))
!	when tdecimal then
	else
		pcustype("Sign",sptr)
	esac

	++pcptr
end

global proc k_sqrt=		domaths(ksqrt) end
global proc k_sin=		domaths(ksin) end
global proc k_cos=		domaths(kcos) end
global proc k_tan=		domaths(ktan) end
global proc k_asin=		domaths(kasin) end
global proc k_acos=		domaths(kacos) end
global proc k_atan=		domaths(katan) end
global proc k_log=		domaths(klog) end
global proc k_log10=	domaths(klog10) end
global proc k_exp=		domaths(kexp) end
global proc k_round=	domaths(kround) end
global proc k_floor=	domaths(kfloor) end
global proc k_ceil=		domaths(kceil) end
global proc k_fract=	domaths(kfract) end

global proc k_neg=
	varrec x:=sptr^

	var_neg(sptr)
	var_unshare(&x)

	++pcptr
end

global proc k_negto=
	variant px:=sptr--

!	if not var_negto(px) then
		var_inplace_unary(px, cast(var_neg))
!	end

	++pcptr
end

global proc k_absto=
	variant px:=sptr--

!	if not var_absto(px) then
		var_inplace_unary(px, cast(var_abs))
!	end

	++pcptr
end

global proc k_inotto=
	variant px:=sptr--

!	if not var_inotto(px) then
		var_inplace_unary(px, cast(var_inot))
!	end

	++pcptr
end

global proc k_atan2=
	pcerror("ATAN2 NOT READY")
end

global proc k_fmod=
	pcerror("FMOD NOT READY")
end


global proc k_abs=
	varrec x:=sptr^

	var_abs(sptr)
	var_unshare(&x)

	++pcptr
end

global proc k_inot=
	varrec x:=sptr^

	var_inot(sptr)
	var_unshare(&x)

	++pcptr
end

global proc k_istruel=
	int res

	res:=var_istruel(sptr)
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res

	++pcptr
end

global proc k_notl=
	int res

	res:=not var_istruel(sptr)
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res

	++pcptr
end

global proc k_jumpeq=
	variant y:=sptr--
	variant x:=sptr--

	if var_equal(x,y) then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)
end

global proc k_jumpne=
	variant x,y

	y:=sptr--
	x:=sptr--

	if not var_equal(x,y) then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)
end

global proc k_jumplt=
	variant x,y

	y:=sptr
	x:=sptr-1

	sptr-:=2

	if var_compare(x,y)<0 then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)
end

global proc k_jumple=
	variant x,y

	y:=sptr--
	x:=sptr--

	if var_compare(x,y)<=0 then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)
end

global proc k_jumpge=
	variant x,y

	y:=sptr
	x:=sptr-1

	sptr-:=2
	if var_compare(x,y)>=0 then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)
end

global proc k_jumpgt=
	variant x,y

	y:=sptr--
	x:=sptr--

	if var_compare(x,y)>0 then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)
end

global proc k_jumpfalse=
	variant x:=sptr--

	if not var_istruel(x) then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)

end

global proc k_jumptrue=
	variant x:=sptr--

	if var_istruel(x) then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
end

global proc k_incrtom=
	variant p
!
	p:=variant(getopnda)
	case p.tag
	when tint then
		++p.value
  	when trefvar then
		++p.varptr
	when trefpack then
		p.ptr+:=ttsize[p.elemtag]
	when treal then
		p.xvalue+:=1

	else
		pcustype("incrtom",p)
	end
	skip(1)
end

global proc k_incrtof=
	variant p

	p:=variant(frameptr+getopnda)
	case p.tag
	when tint then
		++p.value
  	when trefvar then
		++p.varptr
	when trefpack then
		p.ptr+:=ttsize[p.elemtag]
	when treal then
		p.xvalue+:=1
	else
		pcustype("incrtof",p)
	esac
	skip(1)
end

global proc k_decrtom=
	variant p

	p:=variant(getopnda)
	case p.tag
	when tint then
		--p.value
  	when trefvar then
		--p.varptr
	when trefpack then
		p.value-:=ttsize[p.elemtag]
	when treal then
		p.xvalue-:=1
	else
		pcustype("decrtom",p)
	esac
	skip(1)
end

global proc k_decrtof=
	variant p

	p:=variant(frameptr+getopnda)
	case p.tag
	when tint then
		--p.value
	when treal then
		p.xvalue-:=1
  	when trefvar then
		--p.varptr
	when trefpack then
		p.ptr-:=ttsize[p.elemtag]
  	else
		pcustype("decrtof",p)
	esac
	skip(1)
end

global proc k_incrload=
	varrec v

	v:=sptr^
	k_incrptr()
	var_loadptr(&v,++sptr)
end

global proc k_loadincr=
	varrec v

	v:=sptr^
	var_loadptr(sptr,sptr)
	++sptr
	sptr^:=v

	k_incrptr()
end

global proc k_decrload=
	varrec v

	v:=sptr^
	k_decrptr()
	var_loadptr(&v,++sptr)
end

global proc k_loaddecr=
	varrec v

	v:=sptr^
	var_loadptr(sptr,sptr)
	++sptr
	sptr^:=v

	k_decrptr()
end

global proc k_incrptr=
	variant p

	p:=sptr--

	case p.tag
	when trefvar then			!increment what ptr points to
		p:=p.varptr
		case p.tag
		when tint then
			++p.value
		when trefvar then			!incr the pointer
			++p.varptr
		when trefpack then			!incr the pointer
			p.ptr+:=ttsize[p.elemtag]
		when treal then
			p.xvalue+:=1
		else
			pcustype("incrptr/refvar",p)
		end
	when trefpack then			!incr the packed type pointed to
		case p.elemtag
		when tu8,ti8 then
			++(p.ptr)^
		when tu16,ti16 then
			++ref u16(p.ptr)^
		else
			pcustype_t("incrptr/ref",p.elemtag)
		end

	else
		pcustype("incrptr",p)
	end
	++pcptr
end

global proc k_decrptr=
	variant p

	p:=sptr--

	case p.tag
	when trefvar then			!increment what ptr points to
		p:=p.varptr
		case p.tag
		when tint then
			--p.value
		when trefvar then			!incr the pointer
			--p.varptr
		when trefpack then			!incr the pointer
			p.ptr-:=ttsize[p.elemtag]
		when treal then
			p.xvalue-:=1
		else
			pcustype("incrptr/refvar",p)
		end
	when trefpack then			!incr the packed type pointed to
		case p.elemtag
		when tu8,ti8 then
			--(p.ptr)^
		when tu16,ti16 then
			--ref u16 (p.ptr)^
		else
			pcustype_t("incrptr/ref",p.elemtag)
		end

	else
		pcustype("incrptr",p)
	end
	++pcptr
end

global proc k_pushvoid=
	++sptr
	sptr.tagx:=tvoid
	++pcptr
end

global proc k_callproc=
	const countinterval=100
	static int count=countinterval

!	if --count=0 then
!		count:=countinterval
!		os_peek()
!	fi

	if sptr>=stacklimit then
		pcerror("Stack Overflow")
	fi

	++sptr
	sptr.tagx:=tretaddr
	sptr.retaddr := pcptr+3

	sptr.frameptr_low := word(frameptr)
	frameptr:=cast(sptr)

	pcptr:=cast(getopnda)

end

global proc k_callptr=
	symbol d

	if sptr.tag<>tsymbol then
		pcerror("Probably undefined function")
	fi
	d:=sptr.def
	if d.nameid=linkid then
		d:=d.alias
	fi

!CPL "CALLPTR",D.NAME

!check. no. of params
	if d.nparams<>getopnda then
		pcerror_s("Callptr: wrong # params; need:",strint(d.nparams))
	fi

	sptr.tagx:=tretaddr
	sptr.retaddr := pcptr+3

	sptr.frameptr_low := word(frameptr)
	frameptr:=cast(sptr)

	pcptr:=cast(d.pcaddress)
end

global proc k_procentry=
	to getopnda do
		++sptr
		sptr.tagx:=tvoid
	od
	skip(1)
end

global proc k_return=
	int nargs

	if doretcheck then
		if sptr.tag<>tretaddr then
			pcerror_s("Not tretaddr:",ttname[sptr.tag])
		fi
	fi

	nargs:=getopnda

	pcptr:=sptr.retaddr

	(ref int32(&frameptr))^:=sptr.frameptr_low

	--sptr

	to nargs do
		var_unshare(sptr)
		--sptr
	od
end

global proc k_return0=
	int nargs

	if doretcheck then
		if sptr.tag<>tretaddr then
			pcerror_s("Not tretaddr:",ttname[sptr.tag])
		fi
	fi

	pcptr:=sptr.retaddr

	(ref int32(&frameptr))^:=sptr.frameptr_low

	--sptr
end

global proc k_unshare=
	to getopnda do
		var_unshare(sptr)
		--sptr
	od
	skip(1)
end

global proc k_unshare1=
	var_unshare(sptr)
	--sptr
	++pcptr
end

global proc k_formci=
	variant p

	p:=cast(getopndb)

	++p.value

	if p.value<=getopndc then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

global proc k_forfci=
	variant p

	p:=cast(frameptr+getopndb)

	++p.value

	if p.value<=getopndc then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

global proc k_fordmci=
	variant p

	p:=cast(getopndb)

	--p.value

	if p.value>=getopndc then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

global proc k_fordfci=
	variant p

	p:=cast(frameptr+getopndb)

	--p.value

	if p.value>=getopndc then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

global proc k_formm=
	variant p,q

	p:=cast(getopndb)
	q:=cast(getopndc)

	++p.value

	if p.value<=q.value then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

global proc k_fordmm=
	variant p,q

	p:=cast(getopndb)
	q:=cast(getopndc)

	--p.value

	if p.value>=q.value then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

global proc k_forff=
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

global proc k_fordff=
	variant p,q

	p:=cast(frameptr+getopndb)
	q:=cast(frameptr+getopndc)

	--p.value

	if p.value>=q.value then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

global proc k_comment=
	skip(1)
end

global proc k_makelist=
	variant x,y
	int n

	n:=getopnda

	x:=sptr-n+1			!start of data
	sptr:=x

	var_make_list(x,sptr,n,getopndb)
	sptr.objptr.mutable:=0

	skip(2)
end

global proc k_makedict=
	variant x
	int n

	n:=getopnda

	x:=sptr-n*2+1			!start of data

	var_make_dict(x,x,n)
	sptr:=x

	skip(1)
end

global proc k_makeset=
	variant x
	int n

	n:=getopnda

	x:=sptr-n+1			!start of data

	var_make_set(x,x,n)
	sptr:=x
	sptr.objptr.mutable:=0

	skip(1)
end

global proc k_makerecord=
	variant x,y
	int n

	n:=getopnda

	x:=sptr-n+1				!start of data

	var_make_record(x,x,n,getopndb)
	sptr:=x
	sptr.objptr.mutable:=0

	skip(2)
end

global proc k_makestruct=
	variant x,y
	int n

	n:=getopnda

	x:=sptr-n+1				!start of data

	var_make_struct(x,x,n,getopndb)
	sptr:=x
	sptr.objptr.mutable:=0

	skip(2)
end

global proc k_makearray=
	variant x
	int n

	n:=getopndb

	x:=sptr-n+1				!start of data

	var_make_array(x,x,getopnda, n, getopndc, getopndd)
	sptr:=x
	sptr.objptr.mutable:=0

	skip(4)
end

global proc k_makebits=
	variant x
	int n

	n:=getopndb

	x:=sptr-n+1				!start of data

	var_make_bits(x,x,getopnda, n, getopndc, getopndd)
	sptr:=x
	sptr.objptr.mutable:=0

	skip(4)
end

global proc k_index=
!x[y]
	variant y,z
	varrec x

	y:=sptr--
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

global proc k_popindex=
!y[z]:=x
	variant x,y,z

	z:=sptr--		!index
	y:=sptr--		!list etc
	x:=sptr--		!value to store

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

global proc k_indexref=
!&x[y]
	variant y,p
	varrec x

	y:=sptr--
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

global proc k_keyindex=
!x{y}
	variant d,k,p,def

	def:=sptr--			!def is any default value to be used
	k:=sptr--			!k is the key
	d:=sptr				!d is the dict

	if d.tag<>tdict then
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

global proc k_popkeyindex=
!y[z]:=x
	variant d,k,p,x

	k:=sptr--			!k is the key
	d:=sptr--			!d is the dict
	x:=sptr--			!value to pop

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

global proc k_keyindexref=
!y[z]:=x
	variant d,k,p,x

	k:=sptr--			!k is the key
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

global proc k_dot=
	symbol d
	variant p, sp
	ref byte q
	int rectype
	varrec v

!CPL "DOT",TTNAME[SPTR.TAG]

	sp:=sptr

restart:
	case sp.tag
!	when trecord, tstruct, tstruct then
	when trecord, tstruct then
	when trefvar then
		sp:=sp.varptr
		restart
	else
		pcustype("1:dot/not record",sp)
	esac
	rectype:=sp.objptr.usertag

	d:=resolvefield(getopnda, rectype)

	case d.nameid
	when fieldid then
		p:=sp.objptr.varptr+d.fieldoffset/varsize
		var_share(p)
		var_unshare(sptr)
		sptr^:=p^

	when structfieldid then
		var_loadpacked(sp.objptr.ptr+d.fieldoffset, d.mode, &v, nil)
		var_unshare(sptr)
		sptr^:=v

    when procid then
		sptr.tagx:=tsymbol
		sptr.def:=d

    when linkid then
		sptr.tagx:=tsymbol
		sptr.def:=d.alias

	else
		pcerror_s("DOT: can't do this fieldtype:",namenames[d.nameid])
	esac

	skip(1)
end

global proc k_dotref=
	symbol d
	variant p, sp
	ref byte q
	int rectype

	sp:=sptr

restart:
	case sp.tag
	when trecord, tstruct then
	when trefvar then
		sp:=sp.varptr
		restart
	else
		pcustype("2:dot/not record",sptr)
	esac
	rectype:=sp.objptr.usertag

	d:=resolvefield(getopnda, rectype)

	case d.nameid
	when fieldid then
		p:=sp.objptr.varptr+d.fieldoffset/varsize
!Possible bug when sptr is a transient value which is now freed
!But you wouldn't normally use as an lvalue
		var_unshare(sptr)

		sptr.tagx:=trefvar
		sptr.varptr:=P

	when structfieldid then
		q:=sp.objptr.ptr+d.fieldoffset
		var_unshare(sptr)
		sptr.tagx:=trefpack
		sptr.ptr:=q
		sptr.elemtag:=d.mode

	else
		pcerror_s("DOTREF: can't do this fieldtype:",namenames[d.nameid])
	esac

	skip(1)
end

global proc k_popdot=
	symbol d
	variant p,x,y

	x:=sptr--
	y:=sptr--

	case x.tag
!	when trecord, tstruct then
	when trecord, tstruct then
	else
!		pcerror("popdot/not record")
		pcustype("3:dot/not record",x)
	esac

	d:=resolvefield(getopnda, x.objptr.usertag)

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
!		var_loadpacked(sptr.objptr.ptr+d.fieldoffset, d.mode, &v, nil)
		var_storepacked(x.objptr.ptr+d.fieldoffset, y, d.mode)
		var_unshare(x)

	else
		pcerror_s("POPDOT: can't do this fieldtype:",namenames[d.nameid])
	esac

	skip(1)
end

global proc k_dotindex=
!x.[y]
	variant y,z
	varrec x


	y:=sptr--
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

global proc k_dotindexref=
!x.[y]
	variant y,p
	varrec x

	y:=sptr--
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

global proc k_popdotindex=
!y[z]:=x
	variant x,y,z,py

	z:=sptr--		!index
	y:=sptr--		!ref to int, string etc
	x:=sptr--		!value to store

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

global proc k_len=
	variant x:=sptr
	object p:=x.objptr
	int n, t

	case x.tag
	when tlist,trecord,tarray,tdict,tbits then
		n:=p.length
	when tstring then
		n:=p.length
	when trecord, tvector, tstruct then
		n:=ttlength[p.usertag]
	when tset then
		n:=p.length
	when trange then
		n:=x.range_upper-x.range_lower+1
	when tdecimal then
		n:=obj_len_dec(p)
!	when ttype then
!		t:=sptr.value
!dotype:
!		case ttbasetype[t]
!		when tenum then
!			n:=ttlower[t]
!		else
!			pcustype("t.len",x)
!		esac
	else
		pcustype("Len",x)
	esac

	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

global proc k_upb=
	variant x:=sptr
	object p:=x.objptr
	int n, t

	case x.tag
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
		n:=x.range_upper
!	when tenum then
!		t:=x.elemtag
!		goto dotype
	when ttype then
		t:=sptr.value
dotype:
		case ttbasetype[t]
		when tvector then
!		when tvector then
			n:=ttlength[t]+ttlower[t]-1
		else
			pcustype("t.upb",x)
		esac

	else
		pcustype("Upb",x)
	esac

	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

global proc k_lwb=
	variant x:=sptr
	object p:=x.objptr
	int n, t

	case x.tag
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
		n:=x.range_lower
!	when tenum then
!		n:=ttlower[x.elemtag]
!	when ttype then
!		t:=sptr.value
!dotype:
!		case ttbasetype[t]
!		when tenum then
!			n:=ttlower[t]
!		else
!			pcustype("t.lwb",x)
!		esac

	else
		pcustype("Lwb",x)
	esac

	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

global proc k_bounds=
	do_bounds(0)
	++pcptr
end

global proc k_boundsx=
	do_bounds(1)
	++pcptr
end

proc do_bounds(int sx)=
	int a,b,m, t
	object p

	m:=sptr.tag
	p:=sptr.objptr

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
		a:=sptr.range_lower
		b:=sptr.range_upper
	when tstruct,trecord then
		a:=1
		b:=ttlength[p.usertag]
	when tvector then
		t:=p.usertag
		goto dotype

	when tset then
		a:=0
		b:=p.length-1
!	when tenum then
!		t:=sptr.elemtag
!		goto dotype

	when ttype then
		t:=sptr.value
dotype:
		case ttbasetype[t]
		when tvector, tstruct then
!		when tvector, tstruct then
			a:=ttlower[t]
			b:=ttlength[t]+a-1
		else
			pcustype("t.bounds",sptr)
		esac

	else
		pcustype("Bounds",sptr)
	esac

	if sx then
		var_unshare(sptr)
		sptr.tagx:=tint
		sptr.value:=a
		++sptr
		sptr.tagx:=tint
		sptr.value:=b

	else
		var_unshare(sptr)
		sptr.tagx:=trange
		sptr.range_lower:=a
		sptr.range_upper:=b
	fi
end


global proc k_dictitems=
	int n

	case sptr.tag
	when tdict then
		n:=sptr.objptr.dictitems
	when tdecimal then
		n:=sptr.objptr.length
	else
		pcustype("Dictitems/digits",sptr)
	esac
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

global proc k_isfound=
	int n

	if sptr.tag<>tint then pcerror("isfound") fi
	sptr.value:=sptr.value<>i64.min

	++pcptr
end

global proc k_append=
	variant y:=sptr--
	varrec x:=sptr^

	var_append(sptr,y)
	var_unshare(&x)

	++pcptr
end

global proc k_concat=
	variant y:=sptr--
	varrec x:=sptr^

	var_concat(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_appendto=
!x append:= y
	variant px,x,y

	y:=sptr--
	px:=sptr--

	case px.tag
	when trefvar then
		var_appendto(px.varptr,y)
	else
		pcustype("Appendto",px)
	esac
	++pcptr
end

global proc k_concatto=
!x append:= y
	variant px,x,y

	y:=sptr--
	px:=sptr--

	case px.tag
	when trefvar then
		var_concatto(px.varptr,y)
		var_unshare(y)
	else
		pcustype("Concatto",px)
	esac
	++pcptr
end

global proc k_addto=
!x +:= y
	variant y:=sptr--
	variant px:=sptr--

	if not var_addto(px, y) then
		var_inplace(px,y, cast(var_add), cast(var_addmixed))
	fi

	var_unshare(y)
	++pcptr
end

global proc k_subto=
!x -:= y
	variant y:=sptr--
	variant px:=sptr--

!	if not var_subto(px, y) then
		var_inplace(px,y, cast(var_sub))
!	end

	var_unshare(y)
	++pcptr
end

global proc k_multo=
!x *:= y
	variant y:=sptr--
	variant px:=sptr--

!	if not var_multo(px, y) then
		var_inplace(px,y, cast(var_mul))
!	end

	var_unshare(y)
	++pcptr
end

global proc k_divto=
!x /:= y
	variant y:=sptr--
	variant px:=sptr--

!	if not var_divto(px, y) then
		var_inplace(px,y, cast(var_div))
!	end

	var_unshare(y)
	++pcptr
end

global proc k_idivto=
!px^ %:= y
	variant y:=sptr--
	variant px:=sptr--

!	if not var_idivto(px, y) then
		var_inplace(px,y, cast(var_idiv))
!	end

	var_unshare(y)
	++pcptr
end

global proc k_iandto=
!px^ iand:= y
	variant y:=sptr--
	variant px:=sptr--

!	if not var_iandto(px, y) then
		var_inplace(px,y, cast(var_iand))
!	end

	var_unshare(y)
	++pcptr
end

global proc k_iorto=
!px^ ior:= y
	variant y:=sptr--
	variant px:=sptr--

!	if not var_iorto(px, y) then
		var_inplace(px,y, cast(var_ior))
!	end

	var_unshare(y)
	++pcptr
end

global proc k_ixorto=
!px^ ixor:= y
	variant y:=sptr--
	variant px:=sptr--

!	if not var_ixorto(px, y) then
		var_inplace(px,y, cast(var_ixor))
!	end

	var_unshare(y)
	++pcptr
end

global proc k_shlto=
!x <<:= y
	variant y:=sptr--
	variant px:=sptr--

!	if not var_shlto(px, y) then
		var_inplace(px,y, cast(var_shl))
!	end

	var_unshare(y)
	++pcptr
end

global proc k_shrto=
!x >>:= y
	variant y:=sptr--
	variant px:=sptr--

!	if not var_shrto(px, y) then
		var_inplace(px,y, cast(var_shr))
!	end

	var_unshare(y)
	++pcptr
end

global proc k_copy=
	varrec x

	if sptr.hasref then
		x:=sptr^
		var_duplu(sptr)
		var_unshareu(&x)
	fi

	++pcptr
end

global proc k_dupl=
	++sptr
	sptr^:=(sptr-1)^
	var_share(sptr)
	++pcptr
end

global proc k_makerange=
	variant x,y

	y:=sptr--
	x:=sptr

	int lower, upper

	if x.tag<>tint or y.tag<>tint then
		pcerror("makerange/not int")
	fi

	sptr.tagx:=trange
	lower:=x.value
	upper:=y.value

	if lower not in -(2**48)..2**48-1 then
CPL =LOWER
		pcerror("Range lwb bounds")
	end

	sptr.range_upper:=upper
	sptr.range_lower:=lower

	++pcptr
end

global proc k_makerangelen=
	variant x,y

	y:=sptr--
	x:=sptr

	if x.tag<>tint or y.tag<>tint then
		pcerror("makerangelen/not int")
	fi

	sptr.tagx:=trange
	sptr.range_upper:=x.value+y.value-1
	sptr.range_lower:=x.value

	++pcptr
end

global proc k_makedecimal=
	varrec x
	object p

	x:=sptr^

	if x.tag<>tstring then pcerror("Not str") fi
	p:=x.objptr
	if p.length=0 then pcerror("Null str") fi

	var_make_dec_str(p.strptr, p.length, sptr)

	var_unshare(&x)

	++pcptr
end

global proc k_makeclosure=
	PCERROR("MK CLOSURE NOT READY")

	++pcptr
end

function resolvefield(int index, rectype)symbol d=
!index is a start point in the genfieldtable
!scan the linked list looking for a field/structfield/method etc whose
!owner type matches rectype
	ref genfieldrec g

	if index=0 then pcerror("Not a field") fi

	g:=genfieldtable[index]

	while g do
		d:=g.def
		if d.owner.mode=rectype then return d fi
		g:=g.nextdef
	od

	pcerror_s("Can't resolve field:",d.name)
	return nil
end

global proc k_pushptr=
	variant p

	p:=sptr

	case p.tag
	when trefvar then
		if not p.varptr then pcerror("Nil^") fi
		sptr^:=p.varptr^

	when trefpack then
		if not p.ptr then pcerror("Nil^") fi
		var_loadpacked(p.ptr,p.elemtag, sptr, nil)

	when trefbit then
		var_loadbit(p.ptr, p.bitoffset, p.elemtag, p.bitlength, sptr)

!	when tsymbol then

	else
		pcustype("Pushptr",p)
	esac

	var_share(sptr)

	++pcptr	
end

global proc k_popptr=
	variant p,x,y

	p:=sptr--
	x:=sptr--

	case p.tag
	when trefvar then
		var_unshare(p.varptr)
		p.varptr^:=x^
	when trefpack then
		var_storepacked(p.ptr,x,p.elemtag)
	when trefbit then
		var_storebit(p.ptr, p.bitoffset, x, p.elemtag, p.bitlength)

	else
		pcustype("Popptr",p)
	esac

	++pcptr	
end
!
global proc k_islist=
	istype(tlist)
end

global proc k_isarray=
	istype(tarray)
end

global proc k_isstring=
	istype(tstring)
end

global proc k_isrecord=
	istype(trecord)
end

global proc k_swap=
	[1024]byte tempbuffer
	variant x,y
	varrec v
	int xt,yt,s,t,n
	ref byte p,q
	int a

	x:=sptr--
	y:=sptr--

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

	++pcptr
end

global proc k_jumptesteq=
!jump to L when x=y
! x<>y: keep x on the stack, skip
! x=y:  pop both jump
	variant x,y
	int xt,yt,res

	y:=sptr--
	x:=sptr
	xt:=x.tag
	yt:=y.tag

	if xt<>yt then
		case pr(xt,yt)
		when pr(tint,trange) then
			if x.value not in y.range_lower..y.range_upper then
				skip(1)
				return
			fi
		when pr(tint,tset), pr(ttype,tset) then
			if not var_in_set(x,y) then
				skip(1)
				return
			fi
		esac
		var_unshare(x)
		var_unshare(y)
		--sptr
		pcptr:=cast(getopnda)
		return
	fi

	res:=var_equal(x,y)
	var_unshare(y)
	if res then
		var_unshare(x)
		--sptr
		pcptr:=cast(getopnda)
		return
	fi

	skip(1)
end

global proc k_jumptestne=
!jump to L when x=y
! x<>y: keep x on the stack, skip
! x=y:  pop both jump
	variant x,y
	int xt,yt,res

	y:=sptr--
	x:=sptr
	xt:=x.tag
	yt:=y.tag

	if xt<>yt then
		case pr(xt,yt)
		when pr(tint,trange) then
			if x.value in y.range_lower..y.range_upper then
				--sptr
				skip(1)
				return
			fi
		when pr(tint,tset), pr(ttype,tset) then
			if var_in_set(x,y) then
				--sptr
				skip(1)
				return
			fi
		esac

		var_unshare(y)
!		--sptr
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
	--sptr

	skip(1)
end

global proc k_jump=
	pcptr:=cast(getopnda)
end

global proc k_jumpptr=
	symbol d

	if sptr.tag<>tsymbol then
		pcerror("symbol expected")
	fi
	d:=cast(sptr.def)
	++sptr
	if d.nameid<>labelid then
		pcerror("label expected")
	fi
	if not d.procfixed then
		d.pcaddress:=modules[d.moduleno].pcstart+d.labelno
		d.procfixed:=1
	fi

	pcptr:=d.pcaddress
end

global proc k_incr=
	case sptr.tag
	when tint then
		++sptr.value
	else
		pcustype("incr",sptr)
	esac

	++pcptr
end

global proc k_decr=
	case sptr.tag
	when tint then
		--sptr.value
	else
		pcustype("decr",sptr)
	esac

	++pcptr
end

global proc k_chr=
	[8]char str

	if sptr.tag=tint then
		var_makechar(sptr.value,sptr)
!!		if sptr.uvalue>=128 then pcerror("chr:not ASCII") fi
!		if sptr.uvalue>255 then pcerror("chr:not ASCII") fi
!		str[1]:=sptr.value
!		str[2]:=0
!		var_make_stringn(&.str,1,sptr,1)
	else
		pcustype("CHR",sptr)
	fi
	++pcptr
end

global proc k_asc=
	int c

	case sptr.tag
	when tstring then
		if sptr.objptr.length then
			c:=sptr.objptr.strptr^
		else
			c:=0
		fi
		var_unshareu(sptr)
		sptr.tagx:=tint
		sptr.value:=c
	else
		pcustype("ASC",sptr)
	esac

	++pcptr
end

global proc k_pusht=
	++sptr
	sptr.tagx:=ttype
	sptr.value:=getopnda
	skip(1)
end

!global proc k_type=
!	int t:=sptr.tag
!	var_unshare(sptr)
!	sptr.tagx:=ttype
!	sptr.value:=t
!	++pcptr
!end
!
global proc k_basetype=
	int t:=sptr.tag

	case t
	when trecord, tstruct, tvector then
		t:=ttbasetype[sptr.objptr.usertag]
!	when tenum then
!		t:=ttbasetype[sptr.elemtag]
	esac

	var_unshare(sptr)
	sptr.tagx:=ttype
	sptr.value:=t
	++pcptr
end

global proc k_type=
	int t:=sptr.tag			!same as .type when no usertype

	case t
	when trecord, tstruct, tvector then
		t:=sptr.objptr.usertag
!	when tenum then
!		t:=sptr.elemtag
	esac

	var_unshare(sptr)
	sptr.tagx:=ttype
	sptr.value:=t
	++pcptr
end

global proc k_elemtype=
	int t:=sptr.tag

	case t
	when tarray,tbits then
		t:=sptr.objptr.elemtag
	when trefpack, trefvar, trefbit then
		t:=sptr.elemtag
	when tset then
		t:=tu1
	when tvector then
		t:=tttarget[sptr.objptr.usertag]
	else
		pcustype_t("elemtype",t)
	esac

	var_unshare(sptr)
	sptr.tagx:=ttype
	sptr.value:=t
	++pcptr
end

global proc k_nop= ++pcptr end

global proc k_modulecall=
	symbol d:=cast(getopnda)
	int moduleno:=d.moduleno

	++sptr
	sptr.tagx:=tretaddr
	sptr.retaddr := pcptr+2

	pcptr:=modules[moduleno].pcstart
end

global proc k_modulereturn=
	pcptr:=sptr.retaddr
	--sptr
end

global proc k_maxvalue=
	int64 a

	if sptr.tag=ttype then sptr.tag:=sptr.value fi

	case sptr.tag
	when tu8 then a:=255
	when tu16 then a:=65536
	when tu32 then a:=0xFFFF'FFFF
	when tu64 then a:=0xFFFF'FFFF'FFFF'FFFF
	when ti8 then a:=127
	when ti16 then a:=32767
	when ti32 then a:=0x7FFF'FFFF
	when ti64,tint then a:=0x7FFF'FFFF'FFFF'FFFF
	else
		pcustype("MAXVALUE",sptr)
	esac
	sptr.tagx:=tint
	sptr.value:=a

	++pcptr

end

global proc k_minvalue=
	int64 a

	if sptr.tag=ttype then sptr.tag:=sptr.value fi

	case sptr.tag
!	when tword,tu8,tu16,tu32,tu64 then a:=0
	when tu8,tu16,tu32,tu64 then a:=0
	when ti8 then a:=-128
	when ti16 then a:=-32768
	when ti32 then a:=-0x8000'0000
	when tint,ti64 then a:=-0x8000'0000'0000'0000
!	when tbignum then a:=-0x8000'0000'0000'0000
	else
		pcustype("MINVALUE",sptr)
	esac
	sptr.tagx:=tint
	sptr.value:=a

	++pcptr
end

global proc k_callhost=
	int n:=getopnda

	callhostfunction(n)
	skip(1)
end

global proc k_expand=
	variant dest
	int n
	
	n:=getopnda
	dest:=sptr+n-1

	var_expand(sptr,dest,n)
	sptr:=dest

	skip(1)
end

global proc k_pushsymbol=
	symbol d:=cast(getopnda)
	++sptr
	sptr.tagx:=tsymbol
	sptr.def:=cast(getopnda)

	skip(1)
end

global proc k_eq=
	variant x,y
	int res

	y:=sptr
	x:=--sptr

	res:=var_equal(x,y)
	var_unshare(x)
	var_unshare(y)

	sptr.tagx:=tint
	sptr.value:=res

	++pcptr
end

global proc k_ne=
	variant x,y
	int res

	y:=sptr
	x:=--sptr

	res:=not var_equal(x,y)
	var_unshare(x)
	var_unshare(y)

	sptr.tagx:=tint
	sptr.value:=res

	++pcptr
end

global proc k_lt= do_cmp(klt) end

global proc k_le= do_cmp(kle) end
global proc k_ge= do_cmp(kge) end
global proc k_gt= do_cmp(kgt) end

proc do_cmp(int opc)=
	variant x,y
	int res

	y:=sptr
	x:=--sptr

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

global proc k_calldll=
	symbol d:=cast(getopnda)
	int nargs:=getopndb
	variant p

	calldll(d, sptr-nargs+1, sptr-nargs, nargs)

	sptr-:=nargs
	skip(2)
end

global proc k_in=
	variant x,y
	int n

	y:=sptr
	x:=--sptr

	n:=var_in(x,y)
	var_unshare(x)
	var_unshare(y)

	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

global proc k_notin=
	variant x,y
	int n

	y:=sptr
	x:=--sptr

	n:=not var_in(x,y)
	var_unshare(x)
	var_unshare(y)

	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

global proc k_inx=
	variant x,y
	int n

	y:=sptr
	x:=--sptr

	n:=var_inx(x,y)
	var_unshare(x)
	var_unshare(y)

	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

global proc k_convrefpack =
	variant a
	int tag,elemtype
	ref void p
	object pa

	case sptr.tag
	when trefvar then
		a:=sptr.varptr

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
			sptr.ptr:=pa.ptr
			sptr.bitoffset:=pa.indexoffset*ttbitwidth[pa.elemtag]
			sptr.bitlength:=0
			sptr.tagx:=trefbit
			sptr.elemtag:=pa.elemtag
			++pcptr
			return

		when tset then
			sptr.ptr:=pa.ptr
			sptr.bitoffset:=0
			sptr.bitlength:=0
			sptr.tagx:=trefbit
			sptr.elemtag:=tu1
			++pcptr
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
		++pcptr
		return

	else
		pcustype("Getrefpack2",sptr)
	end
done:

	sptr.tagx:=trefpack
	sptr.ptr:=p
	sptr.elemtag:=elemtype

	++pcptr
end

global proc k_isdef=
	int res:=sptr.tag<>tvoid
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res
	++pcptr
end

global proc k_isvoid=
	int res:=sptr.tag=tvoid
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res
	++pcptr
end

global proc k_isint=
	istype(tint)
end

global proc k_isnumber=
	int res

	if sptr.tag in [tint,treal,tdecimal] then
		res:=1
	else
		res:=0
	fi
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res
	++pcptr
end

global proc k_ismutable=
	int res

	if sptr.hasref then
		res:=sptr.objptr.mutable
!	elsif sptr.tag=symbol then
!		res
!		res:=1
	else
		res:=1
	fi
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res
	++pcptr
end

global proc k_isreal=
	istype(treal)
end

global proc k_isrange=
	istype(trange)
end

global proc k_isset=
	istype(tset)
end

global proc k_ispointer=
	istype(trefvar,trefpack)
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

global proc k_convert=
	varrec x
	int t

	t:=getopnda

	if sptr.tag<>t then
		x:=sptr^
		var_convert(&x,t,sptr)
		var_unshare(&x)
	fi

	skip(1)
end

global proc k_switch=
	int index,n,lower

	n:=getopnda
	lower:=getopndb

	case sptr.tag
	when tint,ttype then
	else
		pcerror_s("switch not int",ttname[sptr.tag])
	esac

	index:=sptr.value-lower		!now 0-based index

	--sptr

	if u64(index)>=u64(n) then			!out of range
		pcptr:=ref int((pcptr+n*2+4)^)
	else					!in range
		pcptr:=ref int((pcptr+index*2+4)^) 	!+3 for sw cmd + 1 to label part of (kjumptable,label) pair
	fi
end

global proc k_bytesize=
	int n,t,usert
	object p:=sptr.objptr

	t:=sptr.tag

	case t
	when ttype then
		t:=sptr.value
	when tstruct, trecord, tvector then
		t:=sptr.objptr.usertag
	esac

!t is usertag for structs etc, or base tag
	case t
	when tarray then
		n:=p.length*ttsize[p.elemtag]
	when tset then
		n:=getbitssize(p.length,tu1)
	when tstring then
		n:=p.length
	when tbits then
		n:=bits_bytesize(p)
	when tlist,tdict then
		n:=p.length*varsize
	when trecord, tstruct, tvector then
		n:=ttsize[t]	
	when tdecimal then
		n:=p.length
		if n then
			n:=n*decelemsize
		fi
	else
		n:=ttsize[t]
	esac

	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

global proc k_bitwidth=
	if sptr.tag=ttype then
		sptr.value:=ttbitwidth[sptr.value]
	elsif ttbitwidth[sptr.tag] then
		sptr.value:=ttbitwidth[sptr.tag]
	else
		pcerror("bitwidth")
	fi

	sptr.tagx:=tint

	++pcptr
end

global proc k_min=
	variant x,y

	y:=sptr--
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


global proc k_max=
	variant x,y

	y:=sptr--
	x:=sptr

	if var_compare(x,y)>=0 then		!x is bigger
		var_unshare(y)
	else
		var_unshare(x)
		sptr^:=y^
	fi

	++pcptr
end

global proc k_addsp=
	sptr-:=getopnda
	skip(1)
end

global proc k_pushtry=
	(++sptr).tagx:=texception
	sptr.ptr:=ref byte(getopnda)
	sptr.frameoffset:=frameptr-ref byte(sptr)		!byte offset
	sptr.exceptiontype:=getopndb
	sptr.nexceptions:=getopndc
	skip(3)
end

global proc k_raise=
	if sptr.tag<>tint then
		pcerror("Raise: not Int on stack [not proceeding direct to RAISE]")
	fi
	pcptr:=raiseexception(sptr.value)				!will unwind stack and set pcptr to address of exception code
end

global proc k_isequal=
	variant x,y
	int res

	y:=sptr--
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

global proc k_minto=
!x min:= y
	variant y:=sptr--
	variant px:=sptr--

!	if not var_minto(px, y) then
		var_inplace(px,y, cast(var_min))
!	end

	var_unshare(y)
	++pcptr
end

global proc k_maxto=
!x max:= y
	variant y:=sptr--
	variant px:=sptr--

!	if not var_maxto(px, y) then
		var_inplace(px,y, cast(var_max))
!	end

	var_unshare(y)
	++pcptr
end

global proc k_power=
	variant y:=sptr--
	varrec x:=sptr^

	var_power(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

proc domaths(int opcode)=
	case sptr.tag
	when tint then
		sptr.tagx:=treal
		sptr.xvalue:=getmaths(opcode,sptr.value)

	when treal then
		sptr.xvalue:=getmaths(opcode,sptr.xvalue)

	else
		pcustype("Maths:",sptr)
	end
	++pcptr
end

function getmaths(int opcode, real x)real=
	case opcode
	when ksqrt then return sqrt(x)
	when ksin then return sin(x)
	when kcos then return cos(x)
	when ktan then return tan(x)
	when kasin then return asin(x)
	when kacos then return acos(x)
	when katan then return atan(x)
	when klog then return log(x)
	when klog10 then return log10(x)
!	when klg then return lg(x)
	when kexp then return exp(x)
	when kround then
		if x>=0.0 then
			return floor(x+0.5)
		else
			return ceil(x-0.5)
		fi

	when kfloor then
		return floor(x)
	when kceil then
		x:=ceil(x)
		if x=0.0 then x:=0.0 FI
		return ceil(x)

!
!	when kfract then return fract(x)
	else
		pcerror_s("Maths",pclnames[opcode])
	end
	return 0.0
end

global proc k_typepun=
	sptr.tagx:=getopnda
	skip(1)
end

global proc k_andlto=
!px^ iand:= y
	variant y:=sptr--
	variant px:=sptr--
	variant x:=px.varptr

	if px.tag<>trefvar or x.tag<>tint then pcerror("andlto") fi

	x.value iand:=var_istruel(y)
	var_unshare(y)

	++pcptr
end

global proc k_orlto=
!px^ iand:= y
	variant y:=sptr--
	variant px:=sptr--
	variant x:=px.varptr

	if px.tag<>trefvar or x.tag<>tint then pcerror("orlto") fi

	x.value ior:=var_istruel(y)
	var_unshare(y)

	++pcptr
end

global proc k_notlto=
!px^ iand:= y
	variant px:=sptr--
	variant x:=px.varptr

	if px.tag<>trefvar or x.tag<>tint then pcerror("notlto") fi

	x.value ixor:=1

	++pcptr
end

global proc k_pushoperator=
	++sptr
	sptr.tagx:=toperator
	sptr.value:=getopnda
	skip(1)
end

global proc k_maps=
	k_mapss()
end

global proc k_mapss=
	static [10]int codeseq

	int nargs

	case sptr.tag
	when toperator then
		codeseq[1]:=cast(cmdmap[sptr.value])
		--sptr
		codeseq[2]:=(pcptr+1)^			!copy jump lab which follows the applyop
		codeseq[3]:=(pcptr+2)^			!include the dest label
	when tsymbol then
		nargs:=(pcptr^=int(cmdmap[kmaps]) |1|2)

!I need to push 2-3 stack entries down to make room a return value slot
		for i:=0 downto -(nargs+1) do				!0/1 or 0/1/2
			(sptr+i+1)^:=(sptr+i)^
		od
		(sptr-nargs).tagx:=tvoid
		++sptr

		codeseq[1]:=cast(cmdmap[kcallptr])
		codeseq[2]:=nargs
		codeseq[3]:=0
		codeseq[4]:=(pcptr+1)^			!copy jump lab which follows the applyop
		codeseq[5]:=(pcptr+2)^			!include the dest label

	else
		pcerror("Apply:no op")
	esac
	pcptr:=&codeseq[1]				!pass control this short sequence
end

global proc k_idivrem=
	PCERROR("IDIVREM")
end

global proc k_odd=
	case sptr.tag
	when tint then
		sptr.value:=sptr.value.odd
	else
		pcustype("Odd",sptr)
	esac
	sptr.tagx:=tint
	++pcptr
end

global proc k_even=
	case sptr.tag
	when tint then
		sptr.value:=sptr.value.even
	else
		pcustype("Even",sptr)
	esac
	sptr.tagx:=tint
	++pcptr
end

=== qq_host.m 0 0 12/31 ===

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

!global [0..hostfnnames.upb]ref proc hosttable

global proc callhostfunction(int hostfn) =
	ref proc fnaddr
	int nparams,isfn
	object p

!	fnaddr:=hosttable[hostfn]
	fnaddr:=hosthandlers[hostfn]
	nparams:=hostnparams[hostfn]
	isfn:=hostisfn[hostfn]

!CPL "CALL HOST",HOSTFNNAMES[HOSTFN]

	if fnaddr=nil then
		pcerror_s("Hostfn not implemented:",hostfnnames[hostfn])
	fi

	case nparams+isfn
	when 0 then
		hostproc0(fnaddr)^()
	when 1 then
		hostproc1(fnaddr)^(sptr)
	when 2 then
		hostproc2(fnaddr)^(sptr,sptr-1)
	when 3 then
		hostproc3(fnaddr)^(sptr,sptr-1,sptr-2)
	when 4 then
		hostproc4(fnaddr)^(sptr,sptr-1,sptr-2,sptr-3)
	when 5 then
		hostproc5(fnaddr)^(sptr,sptr-1,sptr-2,sptr-3,sptr-4)
	else
		pcerror("callhost/proc")
	esac

	to nparams do
		var_unshare(sptr) when sptr.hasref
		--sptr
	od
end

!global proc inithostlib=
!
!	ichar name
!	int n:=$getnprocs()
!
!	for i to n do
!		name:=$getprocname(i)
!		if eqbytes(name,"pch_",4) then		!(should be OK with v short fn names)
!			for k:=0 to hostfnnames.upb do
!				if eqstring(name+4,hostfnnames[k]+2) then		!skip "pch_" and "h_"
!					hosttable[k]:=$getprocaddr(i)
!					exit
!				fi
!			else
!				loaderror("Unknown hostfn",name)
!			od
!		fi
!	od
!end

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
!OBJECT P
!P:=A.OBJPTR

!CPL "$TEST:",=A, =A.OBJPTR
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
		result.value:=a.objptr.refcount
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
	pcl_callbackfn:=cast(fn.def.pcaddress)
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
=== qq_lex.m 0 0 13/31 ===
const etx	= 26
const cr	= 13
const lf	= 10
const tab	= 9

ref char lxsource		!start of module
ref char lxstart		!start of this token
ref char lxsptr
int lxifcond
int longsuffix			!for real nos
int lxfileno

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
			exit
		when 10 then
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
			++nalllines
			exit
		when lf then
!		++nextlx.pos
			++nalllines
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
			++nalllines
		when lf then
			++nalllines
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
		nextlx.subcode:=jptrto
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
		case lxsptr^
		when '-' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=jdecrload
			return
		when '>' then
			++lxsptr
			nextlx.symbol:=pipesym
		else
			nextlx.subcode:=jsub
		esac
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
		nextlx.subcode:=jdiv
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
		else
			nextlx.symbol:=eqsym
			nextlx.subcode:=jeq
		esac
		return

	when '<' then
		case lxsptr^
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
		esac
		return

	when '>' then
		case lxsptr^
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
		esac
		return

	when '&' then
		case lxsptr^
		when '&' then
			++lxsptr
			nextlx.symbol:=daddrsym
			nextlx.subcode:=jconcat
		else
			nextlx.symbol:=addrsym
			nextlx.subcode:=jaddrof
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
		++nalllines
		nextlx.symbol:=eolsym
		return
	when lf then			!only lfs not preceded by cr
		nextlx.symbol:=eolsym
		++nalllines
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
			nextlx.subcode:=jsqrt
			return
		fi



		if c>=128 then		!assume utf8
			goto doname
		fi
		nextlx.symbol:=errorsym
		nextlx.value:=c
		return

	end doswitch
!end switch
!od

end

!proc OLDlxreadstring(int termchar)=
!!read string inplace: new string, with expanded control characters,
!!is stored on top of original string in the source
!!new string is same length or shorter
!!on entry, lxsptr points to char after " or '
!
!	ichar dest,pstart
!	int c,d,length,hasescape
!	[8]char str
!
!	if termchar='"' then
!		nextlx.symbol:=stringconstsym
!	else
!		nextlx.symbol:=charconstsym
!		nextlx.subcode:=tint
!	fi
!
!	pstart:=lxsptr
!
!	length:=0
!	hasescape:=0
!
!	docase c:=lxsptr++^
!	when '\\' then			!escape char
!		c:=lxsptr^
!		if c in 'A'..'Z' then c+:=' ' fi
!		++lxsptr
!		hasescape:=1
!
!		case c
!		when 'w' then
!			length+:=2
!		when 'x' then	!2-digit hex code follows
!			lxsptr+:=2
!			++length
!		else				!assume valid escape (bad ones detected in next pass)
!			++length
!		esac
!
!	when '"','\'' then		!possible terminators
!		if c=termchar then		!terminator char
!			if lxsptr^=c then		!repeated, assume embedded term char
!				hasescape:=1
!				++lxsptr
!				++length
!			else			!was end of string
!				exit
!			fi
!		else
!			++length
!		fi
!	when cr,lf,0 then
!		lxerror("String not terminated")
!	else
!		++length
!	end docase
!
!	nextlxlength:=length
!
!	if length=0 then
!		nextlx.svalue:=""
!		return
!	elsif not hasescape then
!		nextlx.svalue:=pcm_copyheapstringn(pstart,length)
!		return
!	fi
!
!	nextlx.svalue:=dest:=pcm_alloc(length+1)
!
!	do
!		case c:=pstart++^
!		when '\\' then			!escape char
!			c:=pstart^
!			if c>='A'  and c<='Z' then c+:=' ' fi
!			++pstart
!			switch c
!			when 'a' then			!bell ('alert')
!				c:=7
!			when 'b' then			!backspace
!				c:=8
!			when 'c','r' then		!carriage return
!					c:=cr
!			when 'e' then			!end-of-text
!CPL "<E> ETX SEEN"
!				c:=26
!			when 'f' then			!formfeed
!				c:=12
!			when 'l','n' then		!linefeed, or linux/c-style newline
!				c:=lf
!			when 's' then			!eScape
!				c:=27
!			when 't' then			!tab
!				c:=9
!!			when 'u' then			!reserved for unicode, like \x but with 4 hex digits
!			when 'v' then			!vertical tab
!				c:=11
!			when 'w' then			!windows-style cr-lf
!				dest++^:=cr
!				c:=lf
!			when 'x' then	!2-digit hex code follows
!				c:=0
!				to 2 do
!					if (d:=pstart++^) in 'A'..'F' then
!						c:=c*16+d-'A'+10
!					elsif d in 'a'..'f' then
!						c:=c*16+d-'a'+10
!					elsif d in '0'..'9' then
!						c:=c*16+d-'0'
!					else
!						lxerror("Bad \\x code")
!					fi
!				od
!			when 'y' then			!CCI/SM backwards tab
!				c:=16
!			when 'z' then		!null (not fully supported in code)
!				c:=0
!			elsecase c
!			when '"','Q' then		!embedded double quote
!				c:='"'
!			when '\\' then
!				c:='\\'
!			when '\'' then			!embedded single quote
!				c:='\''
!			when '0' then
!				c:=0
!			else
!				str[1]:=c; str[2]:=0
!				lxerror_s("Unknown string escape: \\%s",&.str)
!			end
!		when '"','\'' then		!possible terminators
!			if c=termchar then		!terminator char
!				if pstart^=c then		!repeated, assume embedded term char
!					++pstart
!				else			!was end of string
!					exit
!				fi
!			fi
!		when cr,lf,etx,0 then
!			lxerror("String not terminated")
!		esac
!
!		dest++^:=c
!	od
!	(nextlx.svalue+nextlxlength)^:=0
!end

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
	lxfileno:=pm.moduleno

	nextlx.symbol:=semisym
	nextlx.subcode:=0
	nextlx.moduleno:=pm.moduleno
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

	lxlength:=nextlxlength
	lx.sourceoffset:=lxstart-lxsource

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
		elsif binopset[lx.symbol] and 	lx.symbol not in [maxsym, minsym] then
			lexreadtoken()
			goto reenter2

		esac
		nextlx.symbol:=semisym

	when insym then
		if lx.symbol=notlsym then
			lx.symbol:=notinsym
			lx.subcode:=jnotin
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
=== qq_lib.m 0 0 14/31 ===
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
!CPL "CT1",POS IAND 16777215, CURRPROC
	loc:=geterrorinfo(pos,currproc)
!CPL "CT2"
	println errortype,"Error:"
	println "    ",,mess
	println

!CPL "CT3"
	showerrorsource(loc)
!CPL "CT4"
	stopcompiler(loc)
end

global func geterrorinfo(word pos, symbol currproc=nil)locrec=
!slow is the low word of a string pointer into source code
!moduleno is the module number if known, otherwise 0 to work it out here
!Set up global error vars: errorline, errorpointer, errormodule, errorlineno
	int soffset, moduleno
	locrec loc

	clear loc
	soffset:=pos.[0..23]
	moduleno:=pos.[24..31]

!CPL =SOFFSET
!CPL =MODULENO

	if moduleno=0 then
		ABORTPROGRAM("GETERRORINFO: no module")
	fi
	if currproc=nil then
		ABORTPROGRAM("GETERRORINFO: no currproc")
	fi

	loc.pm:=modules[moduleno]
	loc.sp:=subprogs[loc.pm.subprogno]	
	loc.def:=currproc

!CPL "GEIX"
!	loc.lineno:=getlineno(loc.p.text, soffset, loc.loc.column)
	setlineno(&loc, soffset)

	return loc
end

!global function getlineno(ichar source, int offset, ichar startline, int &column)int=
global proc setlineno(ref locrec loc, int offset)=
!loc contains sp/pm, fill in startline/lineno/column given char offset within module
	ichar sline, s, source:=loc.pm.text
	

	sline:=source+offset

	while sline>source and sline^<>10 do --sline od
	if sline^=10 then ++sline fi
	loc.startline:=sline
	loc.column:=source+offset-sline

	s:=sline
	loc.lineno:=1
	while s>source do
		if s^=10 then ++loc.lineno fi
		--s
	od
end

proc showerrorsource(locrec loc)=
	ichar s
	println "Line:",loc.lineno,"in Module",loc.pm.name,,".q:"

	if loc.def then
		println "In function:",loc.def.name
	fi

!CPL "///STARTLINE",LOC.STARTLINE
	print " |"
	s:=loc.startline
	while s^ not in [13,10,26,0] do
		print s^
		++s
	od
	println "|"
	

!	println " |",errorline
!	println " |",errorpointer
end

global proc stopcompiler(locrec loc)=
	filehandle f
	f:=fopen("$error.tmp","w")
!	println @f,modulename,,".q",lineno
	println @f,loc.pm.filespec, loc.lineno
	fclose(f)
	println
	println

!	OS_GETCH()

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

global proc pcnotmut=
	pcerror("Not mutable")
end

global proc pcerror(ichar mess)=
	errormess:=mess
	reportpcerror(mess,pcptr)
end

global proc pcerror_s(ichar mess, param)=
	[300]char str
	errormess:=mess
	strcpy(&.str,mess)
	strcat(&.str," ")
	strcat(&.str,param)
	reportpcerror(str,pcptr)
end

global proc reportpcerror(ichar mess, ref int pcptr)=
	variant s,send
	ref int pc
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
	println "    ",,mess
	println

!CPL $LINENO
	showerrorsource(loc)
!CPL $LINENO

	s:=sptr
	send:=&varstack[1]
!CPL $LINENO

	count:=0
	while s>=send and count<5 do
		if s.tag=tretaddr then
			pc:=s.retaddr-3		!go back three to get to start of kcall/kcallptr instr
			loc2:=getpcerrorpos(pc)
			println "Called from line",loc2.lineno,"in",loc2.pm.name
			++count
		fi
		--s
	od
!CPL $LINENO

	stopcompiler(loc)
end

global func getpcerrorpos(ref int pc)locrec =
!given pcptr, set up pcerrorpos, the lsw of the source pointer
!and set up pcerrormodule
	int offset, pos, soffset, moduleno
	ref int pcstart
	ref int32 pcsrcstart
	ifile pm
	locrec loc

	clear loc
	pm:=modules[findmodulefrompc(pc)]

	pcstart:=pm.pcstart
	pcsrcstart:=pm.pcsrcstart

	offset:=pc-pcstart
	pos:=(pcsrcstart+offset)^

	soffset:=pos.[0..23]
	moduleno:=pos.[24..31]

	if moduleno=0 then
MODULENO:=1; SOFFSET:=0
!		ABORTPROGRAM("GETPCPOS: no module")
	fi
!	if currproc=nil then
!		ABORTPROGRAM("GETPCPOS: no currproc")
!	fi

	loc.pm:=modules[moduleno]
	loc.sp:=subprogs[pm.subprogno]	
!	loc.def:=currproc
	loc.def:=nil

	setlineno(&loc, soffset)

	return loc
end

!global proc loaderror(ichar mess,mess2="",mess3="")=
global proc loaderror(ichar mess,mess2="")=
	[512]char str
	if strchr(mess,'#') then
!		fprint @str,mess,mess2,mess3
		fprint @str,mess,mess2
	else
		print @str,mess
	fi

	println "Load Error:",str
	println "Stopping"
	stop 1
end

function findmodulefrompc(ref int pc)int=
!given pcptr, find which module it's currently executing
	for i to nmodules do
		if pc>=modules[i].pcstart and pc<modules[i].pcend then
			return i
		fi
	od
	println "Can't find pcptr module",pc
!RETURN 1
	if errormess then
		fprintln "(#)",errormess
	fi
	stop 1
	return 0
end

global proc prterror(ichar mess)=
	println "Print error:",mess
	os_getch()
	stop 1
end

global proc pcustype(ichar mess, variant x) =
	pcustype_t(mess, x.tag)
end

global proc pcustype_t(ichar mess, int t) =
	[256]char str

	fprint @str,"Type not supported: # : #",mess, ttname[t]
	reportpcerror(str,pcptr)
end

global proc pcmxtypes(ichar mess, variant x,y) =
	pcmxtypestt(mess,x.tag,y.tag)
end

global proc pcmxtypestt(ichar mess, int t,u) =
	[256]char str

	fprint @str, "Types not supported: # : #/#",
			mess,ttname[t],ttname[u]
	reportpcerror(str,pcptr)
end

global function allocunitrec:unit p=
	p:=pcm_alloc(unitrec.bytes)
!	p:=pcm_alloc32()
!	p:=pcm_alloc64()
!	p:=malloc(64)
!	clear p^

	p.word1:=p.nextunit:=p.a:=p.b:=nil
	p.nextunit:=p.a:=p.b:=nil
	p.pos:=lx.pos
	return p
end

global function createintunit(int64 a)unit=
	unit u
	u:=allocunitrec()
	u.tag:=jintconst
	u.value:=a
	return u
end

global function createrealunit(real64 x)unit=
	unit u
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
	u.tag:=tag
	return u
end

global function createunit1(int tag, unit p)unit=
	unit u
	u:=allocunitrec()
	u.tag:=tag
	u.a:=p
	return u
end

global function createunit2(int tag, unit p,q)unit=
	unit u

	u:=allocunitrec()

	u.tag:=tag
	u.a:=p
	u.b:=q

	return u
end

global function createname(ref strec p)ref unitrec=
	ref unitrec u

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
	when jintconst then
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
			if p.cmpgenop[i]=0 then exit fi
			additem(jtagnames[p.cmpgenop[i]])
			jeval(q)
		od

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
!	[16]char str
!	u64 a @ str
	static [2]u64 a
!	ichar s

	a[2]:=0

	a[1]:=jshortnames[opc]
	if a[1]=0 then
		return jtagnames[opc]+1
	else
		return cast(&a)
	fi
!	s:=jshortnames[opc]
!	if s=nil then
!		s:=jtagnames[opc]+1
!	fi
!	return s
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

global function raiseexception(int exceptno)ref int =
	variant stackend,oldsptr

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

global proc raise_error(int error_no)=
!exception raised internally (not in user code)
!caller may not be able to manipulate pcptr
!here, push the error number, and set pcptr to point to a
!block of several kraise opcodes, as it is not how the byte-code
!handler, when it proceeds, will step pcptr

	(++sptr).tagx:=tint
	sptr.value:=error_no

	err_pcptr:=pcptr

	pcptr:=raiseseq
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

=== qq_lists.m 0 0 15/31 ===
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
	word32 allocated

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
=== qq_modules.m 0 0 16/31 ===

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

!CPL "LOADSP",FILENAME

	if source then
		pm:=loadstring(filename, source)
		path[1]:=0
	else
		if eqstring(extractbasefile(filename), syslibname) then
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
			loaderror("LS:Can't load syslib",filespec)
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
		CPL "LOADED FROM MLIBS"
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
=== qq_names.m 0 0 17/31 ===
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
			pcerror("Too many genfields")
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

global function makeslicetype(int target)int=
	int newtype,length

	newtype:=addanontype()



	ttbasetype[newtype]:=tslice
	storemode(stcurrproc, target, &tttarget[newtype])

	ttlower[newtype]:=1
	ttsize[newtype]:=16

!CPL "SLICE",=STRMODE(TTBASETYPE[NEWTYPE])

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
	pcerror("Getalign not 1248")

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
=== qq_optim.m 0 0 18/31 ===
ref int pc, pcstart

macro getopnda = (pc+1)^
macro getopndb = (pc+2)^
macro getopndc = (pc+3)^
macro getopndd = (pc+4)^
macro getopnde = (pc+5)^

macro putopnda(x) = (pc+1)^:=x
macro putopndb(x) = (pc+2)^:=x
macro putopndc(x) = (pc+3)^:=x
macro putopndd(x) = (pc+4)^:=x
macro putopnde(x) = (pc+5)^:=x

ref[0:]int labelmap

global proc optimise_module(int n)=
	int cmd, nopnds, size:=0, x
	pc := pcstart := modules[n].pcstart

!CPL "OPTIM",N

	size:=modules[n].pcsize+1
	labelmap:=pcm_allocz(size*int.bytes)

	repeat
		cmd:=pc^
		nopnds:=pclnopnds[cmd]

		for i to nopnds do
			case pclfmt[cmd,i]
			when cnone then
				exit
			when clabel then
				x:=(pc+i)^
				labelmap^[x]:=1		!allow up to 4 labels at this loc
			esac

		od
		pc+:=nopnds+1
	until cmd in [kzero,kendmodule]

	pc:=pcstart
	repeat
!CPL =PC
		cmd:=pc^
!		optimise_op(cmd)

		case cmd
		when kendmodule then
		else
			optimise_op(cmd)
		esac


	until cmd=kendmodule

!CPL "DONE",SIZE
	if size then pcm_free(labelmap,size) fi
end

proc putnops(int offset,n)=
	to n do
		(pc+offset++)^:=kskip
!		(pc+offset++)^:=knop
	od
end

proc optimise_op(int cmd)=
!optimise a pcl op 'cmd' at pc^ (pc needs to be global)
!If not optimised, leaves it unchanged and sets pc to the following opcode
!When optimised, 1 or more opcodes are modified (usually into one op, with possible
!passing using nops) and sets pc to the next opcode following that sequence
	int skip, offset, secondlab, cmd2, skip2, x,y,z
	int a,b, thisloc, thislab, destcmd, destlab

	skip:=pclnopnds[cmd]+1			!default offset to next instruction
	a:=getopnda
	b:=getopndb
	offset:=pc-pcstart				!offset of this opcode

!CPL "OPTIM",PC,PCLNAMES[CMD],=SKIP

!	PC+:=SKIP
!	RETURN

!check single opcode optimisations first (free/procentry etc)
!otherwise they will not be done if followed by a label
	case cmd
	when kunshare then
		case a
		when 1,2,3 then
			pc^:=(a|kunshare1,kunshare2,kunshare3|0)
			putnops(1,1)
			skip:=2
			return
		esac

	when kprocentry then
		case a
		when 1,2 then
			pc^:=(a|kprocentry1,kprocentry2|0)
			putnops(1,1)
			skip:=2
			return
		esac
	when kjump,kjumpeq,kjumpne,kjumplt,kjumple,kjumpge,kjumpgt then
!						kjumptesteq,kjumptestne then
			thisloc:=offset
			thislab:=a
			if cmd=kjump and thisloc+2=thislab then
				pc^:=knop2
				putopnda(0)
				pc+:=skip
				return
			elsif destcmd=kjump then
				destcmd:=(pcstart+thislab)^
				destlab:=(pcstart+thislab+1)^
!				CPL thisloc,,": JUMP TO JUMP",thislab,PCLNAMES[CMD],=destlab
				putopnda(destlab)
				pc+:=skip
				return
			fi
	esac

	if labelmap[offset+skip] then	!followed by a label; don't bother
		pc+:=skip					!with 2/3-opcode optimising
		return
	fi

	secondlab:=0
	cmd2:=(pc+skip)^
	skip2:=pclnopnds[cmd2]+1
	if labelmap[offset+skip+skip2] then
		secondlab:=1
!		CPL "TWO LABELS"
	fi
	
!CPL "OPTIM3"

	switch cmd
	when kpushf then
		switch b				!opcode that follows
		when kpushf then			!pushf pushf
			if secondlab then dopushff fi
			switch getopndd
			when kpushf then
				pc^:=kpushfff
				putopndb(getopndc)
				putopndc(getopnde)
				putnops(4,2)
				skip:=6

			when kadd then
				pc^:=kaddff
				putopndb(getopndc)
				putnops(3,1)		!leave final opc
				skip:=5
			when ksub then
				pc^:=ksubff
				putopndb(getopndc)
				putnops(3,1)
				skip:=5

			when kjumpeq then
				pc^:=kjumpeqff
				dojumpxxff
			when kjumpne then
				pc^:=kjumpneff
				dojumpxxff
			when kjumplt then
				pc^:=kjumpltff
dojumpxxff:
				x:=getopnda
				y:=getopndc
				z:=getopnde
				putopnda(z)
				putopndb(x)
				putopndc(y)
!NOTE: these nops will overwrite the original LAB of the Jumpge opcode.
!Necessary when jumpgeff has to revert back to normal jumpge
!The Nops are not needed anyway because both outcomes, when the optimised
!jumpgeff is used, will skip these bytes, either by jumping to LAB, or skipping +6
!				putopndd(knop)
!				putopnde(knop)

				skip:=6
			when kjumple then
				pc^:=kjumpleff
				dojumpxxff
			when kjumpge then
				pc^:=kjumpgeff
				dojumpxxff
			when kjumpgt then
				pc^:=kjumpgtff
				dojumpxxff
			when kindex then
				pc^:=kindexff
				putopndb(getopndc)
				putnops(3,1)		!leave final opc
				skip:=5

			else
dopushff:
				pc^:=kpushff
				putopndb(getopndc)
				putnops(3,1)
				skip:=4
			end
		when kpushm then			!pushf pushm
!CPL "PUSHF PUSHM"
				pc^:=kpushfm
				putopndb(getopndc)
				putnops(3,1)
				skip:=4
		when kpushci then			!pushf pushci
!CPL "PUSHF/PUSHCI"
			if secondlab then finish fi
			switch getopndd
			when kadd then
				pc^:=kaddfci
				putopndb(getopndc)
				putnops(3,1)		!leave final opc
				skip:=5
			when ksub then
				pc^:=ksubfci
				putopndb(getopndc)
				putnops(3,1)
				skip:=5

			when kjumplt then
				pc^:=kjumpltfci
dojumpxxfci:
				x:=getopnda
				y:=getopndc
				z:=getopnde
				putopnda(z)
				putopndb(x)
				putopndc(y)
!				putopndd(knop)
!				putopnde(knop)
				skip:=6
			when kjumple then
				pc^:=kjumplefci
				dojumpxxfci
			when kjumpge then
				pc^:=kjumpgefci
				dojumpxxfci
			when kjumpgt then
				pc^:=kjumpgtfci
				dojumpxxfci
			when kjumpeq then
				pc^:=kjumpeqfci
				dojumpxxfci
			when kjumpne then
				pc^:=kjumpnefci
				dojumpxxfci
			end
!
		when kpopm then				!pushf popm
!CPL "PUSHM POPM"
			pc^:=kmovemf
			domoveff
		when kpopf then				!pushf popf
			pc^:=kmoveff
domoveff:
			x:=a
			putopnda(getopndc)
			putopndb(x)
			putnops(3,1)
			skip:=4
		when kzpopf then			!pushf zpopf
!CPL "PUSHF ZPOPF"
			pc^:=kzmoveff
			domoveff

		when kswitch then			!pushf switch
			pc^:=kswitchf
			putopndb(getopndc)
			putopndc(getopndd)
			putnops(4,1)
			skip:=5
	
		when klen then				!pushf len
!CPL "PUSHF LEN"
			pc^:=klenf
			putnops(2,1)
			skip:=3
		when kpushptr then			!pushf pushptr
			pc^:=kpushptrf
			putnops(2,1)
			skip:=3
		end
	when kpushm then
		case b
		when kpushm then			!pushm pushm
!CPL "PUSHM PUSHM"
			pc^:=kpushmm
			putopndb(getopndc)
			putnops(3,1)
			skip:=4
		when kpushf then			!pushm pushm
!CPL "PUSHM PUSHF"
			pc^:=kpushmf
			putopndb(getopndc)
			putnops(3,1)
			skip:=4
		when kpopm then				!pushm popm
!CPL "PUSHM POPM"
			pc^:=kmovemm
			domoveff
		when kpopf then				!pushm popf
!CPL "PUSHM POPF"
			pc^:=kmovefm
			domoveff
		esac
!
	when kpushci then
		case b
		when kpopm then				!pushci popm
!CPL "PUSHCI POPM"
			pc^:=kmovemci
			domoveff
		when kpopf then				!pushci popf
			pc^:=kmovefci
			domoveff
		when kzpopf then			!pushci zpopf
!CPL "PUSHCI ZPOPF"
			pc^:=kzmovefci
			domoveff
		elsif a=0 and b not in [kraise,kstop] then
			pc^:=kpushci0
		esac

	when kpushvoid then
		case a
		when kpushvoid then			!pushvoid pushvoid
			if not secondlab and b=kpushvoid then
				pc^:=kpushvoid3
				putnops(1,2)
				skip:=3
			else
				pc^:=kpushvoid2
				putnops(1,1)
				skip:=2
			fi
		esac
	when kpushfref then
		case b
!		when kpushf then			!pushfref pushf
!		when kpushci then			!pushfref pushci
		when kloadincr then
			if not secondlab then
				case getopndc
				when kpushptr then		!loadincr pushptr
					pc^:=kpushincrptrf
					putnops(2,1)		!loadincr=>skip, but keep the pushptr
					skip:=4
				when kpopptr then		!loadincr popptr
					pc^:=kpopincrptrf
					putnops(2,1)		!loadincr=>skip, but keep the pushptr
					skip:=4
				esac
			fi
		esac

	when kpushmref then
		case b
		when kloadincr then
			if not secondlab then
				case getopndc
				when kpushptr then		!loadincr pushptr
					pc^:=kpushincrptrm
					putnops(2,1)		!loadincr=>skip, but keep the pushptr
					skip:=4
				when kpopptr then		!loadincr popptr
					pc^:=kpopincrptrm
					putnops(2,1)		!loadincr=>skip, but keep the pushptr
					skip:=4
				esac
			fi
		esac

!	when kpopretval then
!		if getopndb not in [kreturn0, kreturn] then
!			CPL "POPRETVAL NOT FOLLOWED BY RET:",PCLNAMES[GETOPNDB]
!		FI
!
!	when kisint, kislist then
!		case a
!		when kjumptrue then			!isint/etc jumptrue
!		when kjumpfalse then		!isint/etc jumpfalse
!		esac
	end

finish:
	pc+:=skip
end

=== qq_packed.m 0 0 19/31 ===
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
		dest.value:=ref int32(p)^

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
			(ref int32(p)^):=q.value
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
			(ref int32(p)^):=q.xvalue
			return
		when ti64,tu64 then
			(ref int64(p)^):=q.xvalue
			return
		when tr32 then
		(ref r32(p)^):=q.xvalue
			return
		when tr64 then
			(ref r64(p)^):=q.xvalue
			return
		when ti16,tu16 then
			(ref int16(p)^):=q.xvalue
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
=== qq_parse.m 0 0 20/31 ===
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

	lex()
	lex()

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

	if lx.symbol in [assignsym, deepcopysym] then
		return readassignment(p)
	else
		return readorterms(p)
	fi
end

function readassignment(unit p)unit=
	int pos,opc
	unit q,r

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
			p:=createunit2(jorlto,p,readunit())
			p.pos:=pos
			exit
		fi

		p:=createunit2(jorl,p,readandterms(readterm2()))
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
	else
		p.cmpgenop:=genops
	fi	

	return p
end

function readinterms(unit p)unit =
	int pos,opc

	p:=readrangeterm(p)

	docase lx.symbol
	when insym, notinsym, inxsym then
		opc:=lx.subcode

		pos:=lx.pos
		lex()

		p:=createunit2(opc,p,readrangeterm(readterm2()))
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
		case opc:=lx.subcode
		when jaddrof then opc:=jappend
!		when jdaddrof then opc:=jconcat
		esac

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

	while lx.symbol=powersym do
		pos:=lx.pos
		lex()
		p:=createunit2(jpower,p,readpowerterms(readterm2()))
		p.pos:=pos
	od

	return p
end

function readterm2:unit p=
	int pos

	pos:=lx.pos
	p:=readterm()
	p:=readtermsuffix(p,pos)
	return p
end

function readtermsuffix(unit p, int pos)unit=
	unit q,r
	ref char pbyte
	word64 a
	int opc,oldipl,shift,t,nparams

	docase lx.symbol
	when lbracksym then
		lex()
		q:=readslist(nparams,1)

		skipsymbol(rbracksym)
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
		case listtype
		when 'PARAM' then
			lex()
			p:=createunit2(jkeyword,p,readunit())
		when 'DICT' then
			lex()
			p:=createunit2(jkeyvalue,p,readunit())
		else
			exit
		esac

	when incrsym then
		case lx.subcode
		when jincrload then opc:=jloadincr
		when jdecrload then opc:=jloaddecr
		esac
		lex()
		p:=createunit1(opc,p)

	else
		exit
	end docase

	p.pos:=pos

	return p
end

function readterm:unit=
	unit p,q,r
	ref char pbyte
	word64 a
	int oldipl,opc,oldinrp,pos,shift,t,nparams,length

	record dummy=
		union
			[20]char str
			int64 sa
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
		memcpy(&.ustr.str,lx.svalue,length)
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
				p:=createunit0(jnull)
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
				opc:=jneg
				goto dounary
			fi
			p:=readterm2()
			if p.tag=jintconst then
				p.value:=-p.value
			else
				p:=createunit1(jneg, p)
			fi
		fi

	when notlsym, istruelsym, inotsym, abssym, sqrsym, signsym,ascsym, chrsym,
			incrsym, decrsym, mathssym, maths2sym  then
		p:=checkoperator()
		if not p then
			opc:=lx.subcode
			lex()
			if lx.symbol=assignsym then
dounary:
				lex()
				p:=createunit1(jtocodes[opc],readterm2())
			else
				p:=createunit1(opc, readterm2())
			fi
		fi

	when mulsym, divsym, idivsym, iremsym, andlsym, orlsym,
		iandsym, iorsym, ixorsym, shlsym, shrsym, insym, notinsym, inxsym,
		eqsym,nesym,ltsym, lesym,gesym,gtsym,powersym, appendsym,
		concatsym,  daddrsym, propsym, isequalsym, specialopsym then
		unless p:=checkoperator() then
			serror("Operator?")
		end

	when lsqsym then
		p:=readset()

	when minsym, maxsym then
		if p:=checkoperator() then
		else
			p:=readpair(lx.subcode)
		fi

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

	when compilervarsym then
		p:=readcompilervar()
		lex()

	when dollarsym then
		if intabledata then
			if tabledataname=nil then serror("$:No enum") fi
			p:=createstringunit(tabledataname,-1)
		else
			if ndollar<=0 then
				serror("[$] No array")
			fi
			p:=createunit1(jupb,dollarstack[ndollar])
		fi
		lex()

	when dotsym,kglobalsym then
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
		p:=createunit1(jraise,readunit())

	when kswapsym then			!swap using function syntax
		p:=readpair(jswap)
!
	when khostfnsym then
		p:=readhostparams(nil,1)

	when knilsym then
!		p:=createunit0((lx.subcode=1|jpnil|jnil))
		p:=createunit0(jnil)
		lex()

	when kstrincludesym then
		lex()
		p:=createunit1(jstrinclude,readterm2())

	when kevalsym then
		lex()
		p:=createunit1(jeval,readunit())


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
	int lineno,m,globalflag,staticflag
	unit ulist,ulistx,p,q,r
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

		when kprocsym,kfunctionsym then
			readprocdef(globalflag)
			globalflag:=local_scope

		when kvarsym then
			q:=readvardef(globalflag,staticflag)
			while q do								!initialised decls involve code
				r:=q.nextunit						!unlink from this block first
				q.nextunit:=nil
				addlistunit(ulist,ulistx,q)			!add one by-one
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
		when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,sendtosym,
				kelsecasesym,kelseswitchsym,kexceptsym,kendsym,rcurlysym then
			exit

		when namesym then
			case nextlx.symbol
!		when dcolonsym,colonsym then
			when colonsym then
				p:=createunit1(jlabeldef,createname(addsymbol(stcurrproc, lx.symptr, labelid, 0)))
				lex()
				lx.symbol:=semisym
				addlistunit(ulist,ulistx,p)
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
			addlistunit(ulist,ulistx,p)
			if lx.symbol=kdosym then
				exit
			fi

		end switch

	until lx.symbol<>semisym

	case lx.symbol
	when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,kdosym,sendtosym,
		kelsecasesym,kelseswitchsym,kexceptsym,kendsym,rcurlysym,commasym,
		barsym,eofsym then
	else
		serror("Readsunit: "";"" expected, or bad unit starter")
	esac

	if ulist=nil or ulist.nextunit then			!empty or multiple => block
		return createunit1(jblock,ulist)
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

function readindex(unit p,int dot)unit=
!at '['; dot=0/1 for a[]/a.[]
!syntax is:
![x] or [x,...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
	unit q,plower,pupper

	lex()

	do
		if ndollar>=maxdollarstack then
			serror("Too many nested a[$]")
		fi
		dollarstack[++ndollar]:=p
		q:=readunit()
		--ndollar

		p:=createunit2((dot|jdotindex|jindex),p,q)

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

	while lx.symbol=dotsym do
		lex()
		case lx.symbol
		when lsqsym then
			p:=readindex(p,1)
		when namesym then
			p:=createunit2(jdot,p,createname(lx.symptr))
			lex()
		when  propsym then			!ought to check whether op is allowed in this form
doprop:
!CPL "DOT PROP",JTAGNAMES[LX.SUBCODE]
			p:=createunit1(lx.subcode,p)
			lex()

		when ktypesym then
			if p.tag<>jtypeconst then
				p:=createunit1(jtype,p)
			fi
			lex()

		when maxsym then
			lx.subcode:=jmaxvalue
			goto doprop

		when minsym then
			lx.subcode:=jminvalue
			lx.symbol:=propsym
			goto doprop
		when dollarsym then
			if p.tag not in [jname,jdot] then
				serror("...name.$ needed")
			fi
			p:=createunit1(jsymbol,p)
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
!eg: (a,b,c	)
!eg: (a		!
	unit ulist,ulistx
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
			addlistunit(ulist,ulistx,readunit())
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
		return createunit2(jif,readunit(),createunit1(jblock,p))
	when kunlesssym then
		lex()
		return createunit2(jif, createunit1(jnotl,readunit()),createunit1(jblock,p))
	else
		return p
	esac
end

function readkeyindex(unit p)unit=
!at '{'
!syntax is:
![x] or [x,...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
	unit q

	lex()

	q:=readunit()

	if lx.symbol=commasym then
		lex()
		q.nextunit:=readunit()
	fi
	
	p:=createunit2(jkeyindex,p,q)

	skipsymbol(rcurlysym)
	return p
end

function readlbrack:unit=
!positioned at "("
!termsym is rbracksym
!read one of the following:
! (x)		simple expression
! ()		list with no elements
! (x,)		list with one element
! (x,x,...)		list
! (x|x|x])		if then else fi
! (x|x,... |x])	select then else end

! (s||s|s)	!list comp [SYNTAX TO BE REVISED]
!return positioned at symbol following closing ")"
	unit ulist,ulistx, p,q,r
	int oldirp,length,lower,lowerseen,elemtype,opc

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
		opc:=jpclcodes[lx.subcode]
		if lx.symbol=addrsym then opc:=kappend fi
doopc:
		if opc=kzero then pcerror("Bad op") fi

		p:=createunit0(joperator)
		p.pclopcode:=opc
		lex()
!		lex()
		skipsymbol(rbracksym)
		return p
	elsecase lx.symbol
	when specialopsym then
		case lx.subcode
		when '-' then opc:=kneg
		when '[]' then opc:=kindex
		else opc:=kzero
		esac
		doopc
	when insym then
		opc:=kin
		doopc
	when notinsym then
		opc:=knotin
		doopc
	when inxsym then
		opc:=kinx
		doopc
	when daddrsym then
		opc:=kconcat
		doopc

	else					!assume normal expression follows
		p:=readxunit()
	esac

!check symbol after "(expr"
	case lx.symbol
	when rbracksym then			!simple (x) expression
		lex()
		if lowerseen then
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
			addlistunit(ulist,ulistx,readxunit())
			++length
			skipsemi()						!allow a,b,c;) (works better with a,b,c\ followed by comment on next line followed by ")")
		until lx.symbol<>commasym
		skipsymbol(rbracksym)
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
			skipsymbol(rbracksym)
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
			until lx.symbol<>commasym
			checksymbol(barsym)
		else
			lex()					!skip |
		fi
		lex()
		r:=readxunit()
		skipsymbol(rbracksym)
		p.nextunit:=r
		return createunit2(jselect,p,ulist)

	when semisym then
		ulist:=ulistx:=p
		repeat
			skipsemi()
			if lx.symbol=rbracksym then
				exit
			fi
			addlistunit(ulist,ulistx,readunit())
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
	unit pthen,pcond, plist,plistx, pelse, p, pelsif

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
	when kelsecasesym,kelseswitchsym then
		lx.symbol:=kwd
!	SERROR("ELSECASE NOT READY")
		pelse:=makeblock(readswitchcase())
	else
		PELSE:=NIL
		checkend(kwd)
!		lex()
	esac

	pthen.nextunit:=pelse
	p:=createunit2(jif,pcond,pthen)
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
			strcpy(str,"Mismatched end ")
			if startline then
				fprint @(&.str+strlen(&.str))," (from line #)",startline
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
	p:=createunit2(jif,createunit1(jnotl,pcond),pthen)
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

	checkend(kwhilesym,kdosym)
!	lex()

	p:=createunit2(jwhile,pcond,pbody)
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
	p:=createunit2(jrepeat,pbody,pcond)
	p.pos:=pos
	return p
end

function readfor:unit=
!on 'for'; syntax is:
! for term [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/od
! for term in/inrev expr [when expr] do stmts [else stmts] end/od

	int line, opc, down, isforeach
	unit pstep, pvar, pcond, pfrom, pto, pelse, pbody, p, plist,pvar2

	line:=lx.pos
	isforeach:=lx.subcode
	lex()			!skip 'for'
	pvar:=readterm2()

	if pvar.tag<>jname then
		serror("For: name expected")
	else
		pvar.def.forindex:=1
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

	if lx.symbol=kwhensym then
		lex()
		pcond:=readunit()
	fi
	skipsymbol(kdosym)
	pbody:=readsunit()

	if pcond<>nil then
		pbody:=makeblock(createunit2(jif,pcond,pbody))
	fi
	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
		pbody.nextunit:=pelse
	else
		pelse:=nil
	fi
	checkend(kforsym,kdosym)
!	lex()


	case opc
	when jforall then
		pvar.nextunit:=plist
		plist.nextunit:=pvar2
		p:=createunit2((down|jforallrev|jforall),pvar,pbody)

	when jforupx then
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
	unit p
	int line

	line:=lx.pos
	lex()
	p:=readsunit()
	checkend(kdosym)
	p:=createunit1(jdo,p)
	p.pos:=line
	return p
end

function readto:unit=
	int line,id
	unit p, pcount, pbody

	line:=lx.pos
	lex()

	pcount:=readunit()

	skipsymbol(kdosym)
	pbody:=readsunit()
	checkend(ktosym,kdosym)
!	lex()

	pcount.nextunit:=createavname()

	p:=createunit2(jto,pcount,pbody)
	p.pos:=line
	return p
end

function makeblock(unit p)unit=
	return createunit1(jblock,p)
end

function readvardef(int isglobal=0, isstatic=0)unit=
!positioned at 'var' 'let'
	int nvars,varid, opc
	symbol d
	unit ulist, ulistx, p

!	m:=readtypespec(lx.symbol=kvarsym)
	lex()
!	M:=TVAR

!	if stcurrproc.nameid=procid then
	if stcurrproc.nameid in [procid,anonprocid] then
		varid:=(isstatic|staticid|frameid)
	else
		varid:=staticid
	fi
	nvars:=0
	ulist:=ulistx:=nil

	while lx.symbol=namesym do
		++nvars

		d:=addsymbol(stcurrproc, lx.symptr, varid, isglobal)
!		storemode(stcurrproc,m,&d.mode)

		lex()

		case lx.symbol
		when assignsym,deepcopysym then
			opc:=lx.subcode
			if varid=staticid then
!				if stcurrproc.nameid=procid then
				if stcurrproc.nameid in [procid,anonprocid] then
					serror("Need '=' for static in proc")
				fi
			fi
			d.initcode:=(lx.symbol=assignsym|2|3)
			lex()
			d.code:=readunit()
			if varid=frameid then
				p:=createunit2(opc,createname(d),d.code)
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
	unit p,q,r

	lex()
	q:=nil

	if exprstarterset[lx.symbol] then
		q:=readunit()
	fi
	p:=createunit1(jreturn, q)

	return readcondsuffix(p)
end

function readprint:unit=
	int opc, isfprint, fshowname, length
	unit pformat, pdev, printlist,printlistx, p,q
	ref strbuffer expr

	ichar s

	pushlisttype('PRINT')

	if lx.symbol=questionsym then
		opc:=jprintln
	else
		opc:=lx.subcode
	fi

	case opc
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
			if lx.symbol=colonsym then
				lex()
				p:=createunit2(jfmtitem,p,readunit())
			fi

			if fshowname then
				expr:=strexpr(p)
				strbuffer_add(expr,"=")
				s:=expr.strptr

				iconvucn(expr.strptr,expr.length)

				addlistunit(printlist,printlistx,q:=createstringunit(s,expr.length))
			fi
			addlistunit(printlist,printlistx,p)
		esac
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish:
	if opc=jprint and printlist=nil then
		serror("No print items")
	fi
	if opc=jfprint and printlist=nil and pformat=nil then
		serror("No print items")
	fi

	poplisttype()
	if isfprint then
		if pformat=nil then
			serror("No fmt str")
		fi
		if pformat=nil then
			pformat:=makeblock(pformat)
		fi
		pformat.nextunit:=printlist
		return createunit2(opc,pdev,pformat)
		return pformat
	else

		return createunit2(opc,pdev,printlist)
	fi

end

function readread:unit=
	int opc
	unit pformat, pdev, readlist, readlistx, p

	pushlisttype('PRINT')
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
		if lx.symbol=colonsym then
			lex()
			p:=createunit2(jfmtitem,p,readunit())
		fi
		addlistunit(readlist,readlistx,p)
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish:
	if opc=jread and readlist=nil then
		serror("No read items")
	fi

	poplisttype()
	return createunit2(opc,pdev,readlist)
end

function readloopcontrol:unit=
	int opc
	unit p

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

function readswitchcase:unit=
	int pos, kwd, opc, lineno,rangeused, nwhen
	unit pexpr,pwhenlist,pwhenlistx,pwhen,pwhenx,pelse,p,pthen,pwhenthen,q

	pos:=lx.pos
	kwd:=lx.symbol			!remember kcasesym etc
	opc:=lx.subcode			!pick up tag: kcase etc

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
			addlistunit(pwhen,pwhenx,p)
			if lx.symbol<>commasym then exit fi
			lex()
		od
		if lx.symbol<>kthensym then checksymbol(sendtosym) fi
		lex()
		pthen:=readsunit()
		pwhenthen:=createunit2(jwhenthen,pwhen,pthen)
		pwhenthen.pos:=pos
		addlistunit(pwhenlist,pwhenlistx,pwhenthen)
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

	p:=createunit2(opc,pexpr,pwhenlist)
	p.pos:=pos
	return p
end

function readgoto:unit=
	lex()

	return readcondsuffix(createunit1(jgoto,readunit()))
end

function readstop:unit=
	unit p
	int i
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
	int t,opc

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
		skipsymbol(rbracksym)
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
	p:=readterm()

	p:=createunit1(opc,p)
	storemode(stcurrproc,t,&p.mode)
	return p
end

function readset:unit=
!positioned at "["
	int length,nkeyvalues,oldinparamlist
	unit p,ulist,ulistx

	lex()					!first symbol of first expression

	case lx.symbol
	when rsqsym then		!empty set, same as 0
		lex()
		return createunit1(jmakeset,nil)
	when colonsym then
		lexchecksymbol(rsqsym)
		lex()
		return createunit1(jmakedict,nil)
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
		addlistunit(ulist,ulistx,p:=readunit())
		if p.tag=jkeyvalue then ++nkeyvalues fi

		++length
		skipsemi()						!allow a,b,c;]
	od

	skipsymbol(rsqsym)

	if nkeyvalues then
		if length>nkeyvalues then serror("dict: mixed elements") fi
		p:=createunit1(jmakedict,ulist)
	else
		p:=createunit1(jmakeset,ulist)
	fi
	p.length:=length
	poplisttype()
	return p
end

global proc readtabledef(int isglobal=0)=
!at 'tabledata' symbol
	int i,ncols,nrows,enums,nextenumvalue,startline, firstvalue
	int ltype,lower
	byte commas:=0, semis:=0
	unit ulist,ulistx, plower, p
	const maxcols=20
	[maxcols]symbol varnames
	[maxcols]unit plist,plistx
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

			d:=addsymbol(stcurrproc,lx.symptr,enumid, isglobal)
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
			addlistunit(plist[i],plistx[i],readunit())
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
	checkend(ktabledatasym,startline:startline)

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

		p:=d.code:=createunit1(jmakelist,plist[i])
		p.length:=nrows
		p.lower:=firstvalue
	od
end

function readtry:unit=
	unit ptry, pexceptlist, pexceptlistx, px, q, exlist,exlistx
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
		skipsymbol(kthensym)
		px:=readsunit()
		addlistunit(pexceptlist,pexceptlistx,createunit2(jexcept,exlist,px))
	od
	checkend(ktrysym)
!	lex()

	return createunit2(jtry,ptry,pexceptlist)
end

function readsprint:unit=
	int opc,isfprint
	unit pformat, pdev, printlist, printlistx, p

	pushlisttype('PRINT')
	opc:=lx.subcode
	lexchecksymbol(lbracksym)
	lex()

	case opc
	when jsfprint then
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
			if lx.symbol=colonsym then
				lex()
				p:=createunit2(jfmtitem,p,readunit())
			fi
			addlistunit(printlist,printlistx,p)
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
		return createunit2(opc,pdev,pformat)
	else
		return createunit2(opc,pdev,printlist)
	fi

end

function readsread:unit=
!to work an item at a time:
! a:=sread([fmt])
! b:=sreadln([dev])	returns entire input line, but keeps line for subsequent sread/read
	int opc
	unit pformat,pdev,p, readlist,readlistx

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
			p:=createunit2(jfmtitem,p,readunit())
		fi
		addlistunit(readlist,readlistx,p)
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

	return createunit2(opc,pdev,readlist)
end

proc readimportdll=
!at 'importdll'
	[256]char str
	symbol stproc,d, stname
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
!		stname:=addsymbol(stprogram,stname,dllmoduleid,0)
		stname:=addsymbol(nil,stname,dllmoduleid,0)
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
		when kfflangsym then
			lex()
		when kprocsym,kfunctionsym then
			isfunc:=lx.symbol=kfunctionsym
			lex()
			case lx.symbol
			when namesym then
				stproc:=addsymbol(stcurrproc, lx.symptr, dllprocid, 1)

			when stringconstsym then
				strcpy(str,lx.svalue)
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

			if lx.symbol=namesym and eqstring(lx.symptr.name,"as") then
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
	skipsymbol(rbracksym)
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
		storemode(stcurrproc,baseclass,&baseclasstable[nbaseclasses])
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
		m:=readstructbody(d,caligned)
	fi

	if lbopening then
		checksymbol(rbracksym)
		lex()
	else
		checkend(krecordsym,startline:startline)
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
	int m

	m:=addanontype()

	oldstcurrproc:=stcurrproc
	stcurrproc:=owner

	docase lx.symbol
	when kconstsym then
		readconstdef(0)
	when kvarsym then
		readrecordfields(owner)
	when kfunctionsym,kprocsym then
		readprocdef(0)

	when krecordsym then
		readrecorddef(0, nil)
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
	elsif istypestarter() and nextlx.symbol<>lbracksym then
		readrecordfields(owner)
	else
		serror("Unknown record field decl")
	end

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

	ttsize[m]:=varsize*owner.nfields

	stcurrproc:=oldstcurrproc

	return m
end

proc readrecordfields(symbol owner)=
!positioned at 'var'; read one line of var defs for a record
	int nvars,offset,index
	symbol d

!	m:=readtypespec(1)
	lex()

	nvars:=0
	index:=owner.nfields
	offset:=index*varsize

	while lx.symbol=namesym do
		++nvars

		d:=addsymbol(stcurrproc, lx.symptr, fieldid, 0)
!		storemode(owner, m, &d.mode)
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

		case lx.symbol
		when kstructsym then
			++ngroups
			lex()
			addstructflag(owner,structblockid)

		when kunionsym then
			++ngroups
			lex()
			addstructflag(owner,unionblockid)

		when kendsym then
			if nextlx.symbol in [kstructsym, kunionsym] then lex() fi
doend:
			if ngroups then
				--ngroups
				lex()
				addstructflag(owner,endblockid)
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
!				d:=addsymbol(owner, lx.symptr,structfieldid, 0)
!				storemode(owner,t,&d.mode)
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

	fprint @str,"$$#",++structseqno

	addsymbol(owner, addglobalname(str),id, 0)
end

proc readprocdef(int isglobal)=
!at 'proc' etc symbol; read proc def or declaration
	int kwd,startline, nparams, shortfun
	unit pcode
	symbol d, oldstcurrproc
	[256]char str

	kwd:=lx.symbol
	shortfun:=lx.subcode
	lexchecksymbol(namesym)
!
	if stcurrproc.nameid in [procid,anonprocid] then
		serror("Nested proc")
	fi

	oldstcurrproc:=stcurrproc			!usually module, but could be a record
	stcurrproc:=d:=addsymbol(stcurrproc,lx.symptr,procid,isglobal)

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
		checkend(kwd,startline:startline)
	else
		d.code:=readunit()
		checksymbol(semisym)
!		lex()
	fi

	if eqstring(d.name,"start") then
		currmodule.startfn:=d
	elsif eqstring(d.name,"main") then
		currmodule.mainfn:=d
	fi

	stcurrproc.misfunc:=kwd=kfunctionsym

	stcurrproc:=oldstcurrproc
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
	when stdtypesym, krefsym, kvarsym, kslicesym, lsqsym then
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
					stname:=addsymbol(owner,lx.symptr,macroparamid,0)
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

function readhostparams(unit lhs,int isfn)unit=
!hostfn name has been read
!lhs is not null when lhs.hostfn(...) has been used
!currently at hostfn symbol
	int fnindex, nargs
	unit p,q

	fnindex:=lx.subcode
	lexchecksymbol(lbracksym)
	lex()

	q:=readslist(nargs)

	skipsymbol(rbracksym)

	if lhs then
		lhs.nextunit:=q
		q:=lhs
	fi

	p:=createunit1(jcallhost,q)
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
	static []ichar monthnames=("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	locrec loc

	case lx.subcode
	when jcvlineno then
		loc.pm:=currmodule
		setlineno(&loc, lx.sourceoffset)
		return createintunit(loc.lineno)

	when jcvstrlineno then
		loc.pm:=currmodule
		setlineno(&loc, lx.sourceoffset)
		strcpy(str, strint(loc.lineno))

	when jcvmodulename then
		strcpy(&.str,currmodule.name)

	when jcvfilename then
!		strcpy(&.str,modules[currmoduleno].filename)
!		strcpy(&.str,currmodule.name)
		strcpy(&.str,currmodule.filespec)
	when jcvfunction then
		strcpy(&.str,(stcurrproc|stcurrproc.name|"<none>"))
	when jcvdate then
		os_getsystime(&tm)
		fprint @&.str,"#-#-#",tm.day,monthnames[tm.month],tm.year:"4"
!
	when jcvtime then
		os_getsystime(&tm)
		fprint @&.str,"#:#:#",tm.hour:"2",tm.minute:"z2",tm.second:"z2"

!	when jcvversion then x:=compilerversion
!	when jcvpclversion then x:=pclversion
	else
		serror("compiler var not impl")
	esac

	return createstringunit(pcm_copyheapstring(&.str))
end

function readpair(int opc)unit=
!should be at '(', but check
!read (a,b) and produce (opc, a, b ) unit

	ref unitrec a,b

	lexchecksymbol(lbracksym)
	lex()
	a:=readexpression()
	skipsymbol(commasym)
	b:=readexpression()

	if lx.symbol=commasym and opc=jmap then			!allow 3rd item
		lex()
		b.nextunit:=readexpression()
	fi
	skipsymbol(rbracksym)
	return createunit2(opc, a,b)
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

!CPL "USERTYPE",PTYPE,D.NAME,=NTYPES,STRMODE(PTYPE),STRMODE(NTYPES)

	createusertype(d, ptype)
end

function readtypespec(int allowvar=0, symbol owner=nil)int=
!full=1 to allow structdefs

	int flags, arraycode, oldipl
	int a,b,t, startline, caligned
	symbol d
	const maxdim=10
	[maxdim]unit lowerdims,lengthdims
	int ndims
	unit x,lowerx, upperx, lengthx

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
		skipsymbol(rsqsym)
		poplisttype()
		t:=readtypespec()

		for i:=ndims downto 1 do
			t:=makeaxtype(t,lowerdims[i],lengthdims[i])
		od
		return t

	when krefsym then
		lex()

		if lx.symbol=stdtypesym and lx.subcode=tvoid then
			lex()
			return makereftype(tvoid,owner)
		else
			return makereftype(readtypespec(),owner)
		fi

	when namesym then
		d:=lx.symptr
		lex()
		if lx.symbol=dotsym then
			lexchecksymbol(namesym)
			t:=newusertypex(d,lx.symptr)
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
			return makestrtype(tpackstrc,readunit())

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

		checkend(krecordsym,startline:startline)
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
		t:=readstructbody(owner,caligned)

		checkend(kstructsym,startline:startline)
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
	when kslicesym then
		lexchecksymbol(lsqsym)
		lexchecksymbol(rsqsym)
		lex()
		t:=makeslicetype(readtypespec(0))

	else
		serror("Type expected")
	esac

	return t
end

proc readparams(symbol stproc)=
!just after '('
	int isbyref,isoptional
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
		d:=addsymbol(stproc, lx.symptr, paramid,0)
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
			else opc:=kzero
			esac
			p.pclopcode:=opc
		else
			p.pclopcode:=jpclcodes[lx.subcode]
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

	print @str,"$F",,++nextlambdaindex
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
!CPL "READ LAMBDA",STPROC.NAME, STPROC.MISFUNC

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
		d:=addsymbol(owner, lx.symptr,id, 0)
		storemode(owner,t,&d.mode)
		lex()

		if lx.symbol<>commasym then
			exit
		fi
		lexchecksymbol(namesym)
	od
	if nvars=0 then serror("bad decl?") fi
end
=== qq_print.m 0 0 21/31 ===
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
[0:maxoclevel]int32			moutdevstack
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
		when 'B' then cat:='A'		!-922...808 can be int64
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
=== qq_pclgen.m 0 0 22/31 ===
!not opcodes, just used internally here for conditional jump logic
const kjumpt = 1
const kjumpf = 0

!loop stack data reused by GENMPL
const maxloopindex=20
[maxloopindex,4]ref int loopstack
[maxloopindex]int trylevelstack
global int loopindex=0
int looptrylevel			!return by findlooplabel

const maxswitchrange=512
const maxlocals=300
const maxparams=100

const maxunits=400					!for constructors
int trylevel=0
int currfunction=0				!0/1/2 = not a function/proc/function

!vars within curr procdef
int retindex						!common return point; label no
int retvaloffset					!offset of return value for procs (as stack slots)
int nprocparams						!no. of params
global int nproclocals				!no. of locals
global ref int pproclocals			!pointer to pcl operand of kprocentry; may need updating
const retaddrslots = 1				!+1 or +2, added to param indices (depends on return info size)
int procskiplabel

global proc evalunit(unit p,int res=1)=
!p is a single executable unitrec; not a list or const
!should be called via execblock() to executate a code item in a unitrec
!note: sometimes res can be 2, (passing on a res=2 from an outer stmt)
!that should be treated here same as 1 (res=2 has special meaning from pclhasvalue[] only)
	unit a,b
	symbol d
	object ss
	int procflag,index

	qpos:=p.pos

	a:=p.a
	b:=p.b

	switch p.tag
	when jintconst      then
		genpc_int(kpushci,p.value)

	when jrealconst     then
		genpc_real(kpushcr,p.xvalue)

!	when jenumconst      then
!		genpc_int2(kpushenum, p.value, p.mode)

	when jnone then

	when jstringconst   then
		pushstring(p.svalue, p.length)

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
		when labelid then
			if not res then
				if d.labelno=0 then
					d.labelno:=createfwdlabel()
				fi
				genpc_lab(kjump,d.labelno)
				return
			else
				genpc_name(kpushsymbol,d)
			fi

		when dllvarid then
			genpc_name(kpushx, d)

		else
			genpc_name(kpushsymbol,d)
		esac

	when jsymbol        then			!assume a is jname
		if a.tag=jname then
			genpc_name(kpushsymbol,a.def)
		else
			gerror(".$ name expected")
		fi

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
		pushstring(p.svalue, p.length)
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
	when jwhile         then do_while(p,a,b)
	when jrepeat        then do_repeat(p,a,b)
	when jgoto          then
		if a.tag=jname and a.def.nameid=labelid then
			d:=a.def
			if d.labelno=0 then
				d.labelno:=createfwdlabel()
			fi
			genpc_lab(kjump,d.labelno)
		else
			evalunit(a)
			genpc(kjumpptr)
		fi

	when jlabeldef      then
		d:=a.def
		if d.labelno=0 then
			d.labelno:=definelabel()
		else
			index:=d.labelno
			definefwdlabel(index)
		fi

	when jredo          then do_exit(p,1)
	when jnext          then do_exit(p,2)
	when jexit          then do_exit(p,3)
	when jdo 			then do_do(p,a)
	when jcase,jdocase then do_case(p,a,b,res)
	when jswitch, jdoswitch then do_switch(p,a,b,res)
	when jswap          then
		evalref(a)
		evalref(b)
		genpc(kswap)

	when jselect        then do_select(a,b,res)
	when jprint,jprintln,jsprint    then do_print(p,a,b)
	when jfprint,jfprintln,jsfprint then do_fprint(p,a,b,b.nextunit)
	when jread,jreadln  then do_read(p,a,b)
!	when jnew           then do_new(p)

	when jstop          then
		if a then
			evalunit(a)
		else
			genpc_int(kpushci,0)
		fi
		genpc(kstop)

	when jtry           then do_try(p,a,b)

	when jandl          then do_andl(a,b)
	when jorl          then do_orl(a,b)
	when jmakelist then
		do_pushlist(a,p.length)
		genpc_int2(kmakelist,p.length,p.lower)

	when jmakeset then
		do_pushlist(a,p.length)
		genpc_int(kmakeset,p.length)

	when jmakedict then do_makedict(a,p.length)

!	when jmakeclosure then do_makeclosure(a)

	when jkeyvalue      then
		evalunit(a)
		evalunit(b)
	when jmap           then do_map(p,a,b)

	when jadd, jsub, jmul, jdiv, jidivrem, jiand, jior, jixor,
		 jshl, jshr, jin, jnotin, jinx, jmin, jmax, jmakerange, jmakerangelen,
		 jeq, jne, jlt, jle, jge, jgt, jpower,
		 jconcat, jappend,jisequal then
		evalunit(a)
		evalunit(b)
		genpc(jpclcodes[p.tag])

	when jaddto, jsubto, jmulto, jdivto, jidivto, jiandto, jiorto, jixorto,
		 jshlto, jshrto, jminto, jmaxto, jconcatto, jappendto,
		 jandlto, jorlto then
		evalref(a)
		evalunit(b)
		genpc(jpclcodes[p.tag])

	when jidiv then
		do_idiv(p,a,b)

	when jirem then
		do_irem(p,a,b)

	when jneg, jabs, jlwb, jupb, jlen, jbounds, jnotl, jinot,jisarray,
		 jisint, jisreal, jbytesize, jisdef, jround, jisvoid, jbitwidth,
		 jistruel, jsqr, jsqrt, jislist, jasc,jchr, jisstring, jisset,
		 jbasetype, jtype, jelemtype, jispointer, jisrange, jisrecord,
		 jfloor, jceil, jboundsx, jisnumber, jismutable, jsign,
		 jsin,jcos,jtan, jasin, jacos, jatan, jexp, jlog, jlog10,
		 jminvalue, jmaxvalue, jdictitems, jodd, jeven, jisfound then
		do_unary(a,jpclcodes[p.tag])

	when jnegto, jabsto, jinotto, jnotlto then
		evalref(a)
		genpc(jpclcodes[p.tag])

	when jdot           then! do_bin(a,b,kdot)
		evalunit(a)
		genpc_name(kdot,b.def)

	when jindex         then do_bin(a,b,kindex)
	when jdotindex      then do_bin(a,b,kdotindex)
	when jkeyindex      then
		evalunit(a)
		evalunit(b)
		if b.nextunit then
			evalunit(b.nextunit)
		else
			genpc(kpushvoid)
		fi
		genpc(kkeyindex)

	when jptr           then do_unary(a,kpushptr)
	when jptrto then
		if a.tag=jptr then			!^a^ cancel out (a might be byref param)
			evalunit(a.a)
		else
			evalref(a)
		fi

	when jaddrof        then
		evalref(a)
		genpc(kconvrefpack)

	when jconvert       then
		do_convert(p)

	when jtypepun       then
		evalunit(a)
		genpc_int(ktypepun,p.mode)

	when jtypeconst     then
		genpc_int(kpusht,p.mode)
	when joperator      then
		genpc_int(kpushoperator,p.pclopcode)


	when jincrload, jdecrload, jloadincr, jloaddecr then
		do_incr(p,a,res)
!
	when jnil           then
		genpc(kpushnil)

	when jraise         then do_unary(a,kraise)

	when jnull then
		genpc(kpushvoid)

	when jeval then
		evalunit(a)
		genpc_int(kunshare,1)

	else
		gerror_s("UNSUPPORTED TAG:",JTAGNAMES[P.TAG],p)
	end switch

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
end

global proc gencodemodule(isubprog sp, int moduleno)=
	const maxanonprocs=100
	[maxanonprocs]symbol anonprocs
	int nanonprocs:=0

	symbol d,e
	int lab
	int a:=sp.firstmodule
	int b:=sp.lastmodule
	ifile pm:=modules[moduleno]

	currmodule:=pm
	stcurrproc:=stcurrmodule:=currmodule.def

!CPL "GENCODE",SP.NAME,PM.NAME

	resetpcl(pm.size)

!GOTO FINISH

	gencomment("Module data init code:")

	qpos:=0
	qpos.[24..31]:=moduleno

!CPL "///////////////",QPOS

!jump around stop/raise block needed for reentry
!	if n=1 then
	if moduleno=a then
		lab:=createfwdlabel()
		genpc_lab(kjump,lab)
		genpc(kstoprunproc)
		stopseq:=pcllast

		raiseseq:=pcllast+1
		genpc_int(kpushci,0)
		genpc(kraise)
		definefwdlabel(lab)
	fi

	d:=stcurrmodule.deflist
	while d do
		if d.nameid=staticid and d.code then
			evalunit(d.code)
			if d.initcode=3 then
				genpc(kcopy)
			fi
			genpc_name(kzpopm,d)
		elsif d.nameid=procid then
			e:=d.deflist
			while e do
				if e.nameid=staticid and e.code then
					evalunit(e.code)
					genpc_name(kzpopm,e)
				elsif e.nameid=anonprocid then
					if nanonprocs>=maxanonprocs then gerror("Too many anons") fi
					anonprocs[++nanonprocs]:=e
				fi
				e:=e.nextdef
			od
		fi
		d:=d.nextdef
	od	

	if moduleno=a then
		for i:=b downto a+1 do
			genpc_name(kmodulecall, modules[i].def)
		od
		for i:=b downto a+1 do
			if modules[i].startfn then
				genpc_name(kcallproc, modules[i].startfn)
				genopnd_int(0)
			fi
		od

		if currmodule.startfn then
			genpc_name(kcallproc, currmodule.startfn)
			genopnd_int(0)
		fi

		if currmodule.mainfn then
			genpc_name(kcallproc, currmodule.mainfn)
			genopnd_int(0)
		fi

		evalunit(stcurrmodule.code,0)
		genpc_int(kpushci,0)
		genpc(kstop)
	else
		evalunit(stcurrmodule.code,0)
		genpc(kmodulereturn)
	fi

	gencomment("Procs:")
	d:=stcurrmodule.deflist
	while d do
		switch d.nameid
		when procid,anonprocid then
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
			gerror_s("?Module def:",namenames[d.nameid])
		end switch

		d:=d.nextdef
	od	

	for i to nanonprocs do
		do_procdef(anonprocs[i])
	od

	genpc(kendmodule)

FINISH:
	pm.pcstart:=pclstart
	pm.pcend:=pclend
	pm.pcsize:=pclend-pclstart
	pm.pcsrcstart:=pclsrcstart

!CPL "------DONE GENPCL",PCLSTART,PCLNEXT-PCLSTART, PCLNAMES[PCLSTART^]

end

proc do_procdef(symbol p) =
	int nfreevars,nnofreevars
	int isfunc
	symbol oldcurrproc

	oldcurrproc:=stcurrproc			!might be a method

	stcurrproc:=p

	retindex:=createfwdlabel()
	isfunc:=p.misfunc

	genprocentry(p,nfreevars,nnofreevars)

	if p.code=nil then
		gerror_s("Empty proc body",p.name)
	else
		evalunit(p.code, isfunc)

	fi

	definefwdlabel(retindex)			!common return point
	genprocexit(nfreevars,nnofreevars,isfunc)
	genpc(kprocend)

	if pproclocals^=0 then
		p.labelno:=procskiplabel
	fi
	stcurrproc:=oldcurrproc
end

proc genprocentry(symbol p, int &nfreevars,&nnofreevars) =		!GENPROCENTRY
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
!
	p.labelno:=definelabel()
	genpc_int(kprocentry, nproclocals)
	procskiplabel:=definelabel()

	pproclocals:=pclnext-1

	d:=p.deflist
	while d do
		case d.nameid
		when frameid then
			if d.code then
				evalunit(d.code)
				if d.initcode=3 then
					genpc(kcopy)
				fi
				genpc_name(kzpopf, d)
			fi
		esac

		d:=d.nextdef
	od
end

proc genprocexit(int nfree,nnofree,isfunc)=		!GENPROCEXIT
	int offset

	if isfunc then
		offset:=-(nprocparams+1)*varsize
		genpc_int(kpopretval,offset)
	fi
	if nproclocals then
		genpc_int(kunshare,nproclocals)
	fi

	if nprocparams=0 then
		genpc(kreturn0)
	else
		genpc_int(kreturn,nprocparams)
	fi
end

proc evalref(unit p)=
	unit a,b,c
	symbol d
	int lab1,lab2
	a:=p.a
	b:=p.b

	switch p.tag
	when jname then
		d:=p.def
		if d.nameid in [procid,dllprocid] then
			gerror("^ not allowed")
		fi	

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

	when jdotindex then! do_binref(a,b,kdotindexref)
!		evalunit(a)
		evalref(a)
		evalunit(b)
		genpc(kdotindexref)

	when jkeyindex then! do_binref(a,b,kkeyindexref)
		evalunit(a)
		evalunit(b)
		if b.nextunit then gerror("Def val not allowed") fi
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

proc genjumpcond(int opc,unit p,int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
	unit q,r,s
	int oldpos, lab2, i

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
		evalunit(q)
		evalunit(r)
		gcomparejump(opc,p.tag,lab)

	when jcmpchain then
		r:=q.nextunit
		i:=1
		if opc=kjumpf then
			while r do
				evalunit(q)
				evalunit(r)
				gcomparejump(kjumpt,reversecond(p.cmpgenop[i]),lab)
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
					gcomparejump(kjumpt,reversecond(p.cmpgenop[i]),lab2)
				else
					gcomparejump(kjumpt,p.cmpgenop[i],lab)
				fi
				++i
				q:=r
				r:=r.nextunit
			od
			definefwdlabel(lab2)
		fi
	else
		evalunit(p)
		genpc_lab((opc=kjumpt|kjumptrue|kjumpfalse),lab)
	end switch
	qpos:=oldpos

end

proc gcomparejump(int jumpopc,int cond, lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	int opc

!	cond:=p.tag				!eqop,neop, etc

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

global proc stacklooplabels(ref int a,b,c)=
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

global proc unstacklooplabels=
	--loopindex
end

global function findlooplabel(int k,n)int=
!k is 1,2,3,4 for label A,B,C,D
!n is a 1,2,3, etc, according to loop nesting index
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
	return loopstack[i,k]^
end

proc do_assign(unit a,b, int res,deepcopy=0)=
	unit q
	int n

	if a.tag=b.tag=jmakelist then
		if res then gerror("mult/ass::=") fi
!		if deepcopy then gerror("mult/ass::=") fi
		do_multassign(a,b, deepcopy, res)
		return
	fi

	evalunit(b)
	if deepcopy then
		genpc(kcopy)
	fi

	do_store(a,res)
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
	genpc_int(kmakedict,n)
end

proc do_call(unit p,a,b,int res, &procflag)=
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
!				genpc_name(kcallfn,d)
				genpc_name(kcallproc,d)
			else					!proc call
				isfunc:=0
				nargs:=pushparams(d, arglist, nargs, kwdindex)
				genpc_name(kcallproc,d)
			fi
			genopnd_int(nargs)

		when dllprocid then
			if not d.misfunc then
				isfunc:=0
			else
				genpc(kpushvoid)
			fi
			nargs:=pushparams(d, arglist, nargs, kwdindex)
			genpc_name(kcalldll,d)
			genopnd_int(nargs)
!			genopnd_int(d.mode)
		when aliasid then
			d:=d.alias
			goto retry
		when staticid, frameid, paramid then
			goto docallptr
!
		else
			gerror_s("CAN'T CALL:",namenames[d.nameid])
		esac
	when jdot then
		if kwdindex then docallptr fi		!share error
		genpc(kpushvoid)
		evalref(a.a)					!push &self arg
		for i to nargs do				!any extra ones
			evalunit(arglist[i])
		od
		evalunit(a)						!push lhs again, this time for dot
		genpc(kcallptr)
		++nargs
		genopnd_int(nargs)
		genopnd_int(0)

	else
docallptr:
		if kwdindex then gerror("Kwd params not allowed for fnptr") fi
		genpc(kpushvoid)
		for i to nargs do
			evalunit(arglist[i])
		od
		evalunit(a)
		genpc(kcallptr)
		genopnd_int(nargs)
		genopnd_int(0)
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

	int nparams, extra,n
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
			evalparam(arglist[i],byreflist[i])
		od
		return nargs
	elsif nargs<nparams then	!trailing args missing
		for i to nargs do
			evalparam(arglist[i],byreflist[i])
		od

		for i:=nargs+1 to nparams do
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
		return nparams
	else						!nargs>nparams: variadic
		for i to nparams do
			evalparam(arglist[i],byreflist[i])
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
	int nparams, i,j,k
	[maxparams]symbol paramlist
	[maxparams]byte byreflist
	[maxparams]unit keyunits
	unit p,q
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
		evalparam(arglist[i],byreflist[i])
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

!	for i:=nparams downto kwdindex do
	for i:=kwdindex to nparams do
		if keyunits[i] then
			evalparam(keyunits[i],byreflist[i])
		elsif byreflist[i] then
			gerror("byref param not optional")
		else
			genpc(kpushvoid)
		fi
	od
end

proc do_if(unit p,a,b,pelse, int res)=
	int lab1,lab2

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

proc do_do(unit p,a)=
	int lab_abc,lab_d,lab_test
	lab_abc:=definelabel()
	lab_d:=createfwdlabel()

	stacklooplabels(&lab_abc, &lab_abc, &lab_d)

	evalunit(a,0)

	genjumpl(lab_abc)
	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_exit(unit p,int k) =
	int n,index

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

proc do_to(unit p,pcount,pbody)=
	int lab_b,lab_c,lab_d
	symbol temp
	unit pav

	pav:=pcount.nextunit
	temp:=pav.def

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
	int lab_b,lab_c,lab_d,lab_incr
	unit pincr:=pcond.nextunit

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pincr then
		lab_incr:=createfwdlabel()
	else
		lab_incr:=lab_c
	fi

	stacklooplabels(&lab_b, &lab_c, &lab_d)

	genjumpl(lab_incr)		!direct to condition code which is at the end

	definefwdlabel(lab_b)

	evalunit(pbody,0)

	definefwdlabel(lab_c)

	if pincr then
		evalunit(pincr)
		definefwdlabel(lab_incr)
	fi

	genjumpcond(kjumpt,pcond,lab_b)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_repeat(unit p,a,b) =
	int lab_b, lab_c, lab_d

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
	unit pfrom, pto, pstep, pelse,plimit,pautovar
	symbol dvar, limitvar
	int lab_b,lab_c,lab_d,lab_e,opc,oldqpos
	int step, fromval, limit, jumpinto

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
		genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C:
	fi
	definefwdlabel(lab_b)

	evalunit(pbody,0)				!do loop body

	definefwdlabel(lab_c)

	if pto.tag=jintconst then
		opc:=(step<0|kfordmci|kformci)+dvar.isframe
	elsif dvar.isframe=limitvar.isframe then
		opc:=(step<0|kfordmm|kformm)+dvar.isframe
	else
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
	unit pbounds, pelse,plimit,pautovar
	symbol dvar, limitvar
	int lab_b,lab_c,lab_d,lab_e,opc

	pbounds:=pvar.nextunit

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
	limitvar:=pautovar.def
	genpc_name(kzpopm+limitvar.isframe,limitvar)

	genpc(kdecr)
	genpc_name(kpopm+dvar.isframe,dvar)		!from value

	genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C:
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
	int issprint
	unit x

	issprint:=p.tag=jsprint

	if issprint then
CPL "//////ISSPRINT"
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

	if p.tag=jprintln then
		callhostfn(h_println)
	fi
	if issprint then
		genpc(kpushvoid)
		callhostfn(h_strendprint)
	else
		callhostfn(h_endprint)
	fi
end

proc do_fprint(unit p,a,b,c)=
	int issfprint
	unit x

	issfprint:=p.tag=jsfprint

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

	if p.tag=jfprintln then
		callhostfn(h_println)
	fi
	if issfprint then
		genpc(kpushvoid)
		callhostfn(h_strendprint)
	else
		callhostfn(h_endprint)
	fi

end

proc do_read(unit p,a,b)=
unit x,xloop

if p.tag=jreadln then
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
		genpc_name(kpopm+x.def.isframe,x.def)
	else
		evalref(x)
		genpc(kpopptr)
	fi
	xloop:=xloop.nextunit
od
end

proc do_forall(unit p,pindex,pbody)=
!I think form pvar/prange into blocks, then those can be stored together
! a = pindex, plist, pvar
! b = pbody, [pelse]

	int lab_b,lab_c,lab_d,lab_e
	unit ploopvar, plist, pelse, plimitvar, plistvar
	symbol indexvar,limitvar,loopvar, listvar

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

	genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C:

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

proc do_case(unit p,pindex,pwhenthen,int res) =
!also temporarily deal wit switch/doswitch

	int lab_a,lab_d
	int loopsw,labnextwhen,labstmtstart,fmult
	unit w,wt,pelse

	if pindex.tag=jnone then
		do_case_nc(p,pindex,pwhenthen,res)
		return
	fi

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

proc do_case_nc(unit p,pindex,pwhenthen,int res) =
!when no control expression

	int lab_a,lab_d
	int labnextwhen,labstmtstart,fmult
	unit w,wt,pelse

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
				genpc_lab(kjumptrue,labstmtstart)
			else
				genpc_lab(kjumpfalse,labnextwhen)
			fi
		od
		if fmult then
			definefwdlabel(labstmtstart)
		fi
		evalunit(wt.b,res)

		genjumpl(lab_d)
		definefwdlabel(labnextwhen)
		wt:=wt.nextunit
	od

!at else part
	if pelse then
		evalunit(pelse,res)
	elsif res then
		gerror("Needs Else branch")
!		genpc(kpushvoid)
	fi

	definefwdlabel(lab_d)
end

proc do_try(unit p,a,b) =
	int labend,labx
	unit ptry,x,pexcept,pexcode

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

proc do_select(unit pindex,pplist,int res)=
!generate selectx expression
	int n,labend,i,lab,elselab
	unit x,pelse

	[maxswitchrange]unit plist
	[maxswitchrange+1]int labels

	pelse:=pindex.nextunit

	n:=unitstoarray(pplist,&plist,maxswitchrange)

	if n>maxswitchrange then
		gerror("Selectx too complex")
	fi

	labend:=createfwdlabel()

	evalunit(pindex)
!	genpc_int2(kselect,n,1)
	genpc_int2(kswitch,n,1)

	for i:=1 to n do
		labels[i]:=pclnext-pclstart		!store destination code index
		genpc_lab(kjumplabel,0)
	od
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
	int a,b

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
	int a,b
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
dounary:
		do_unaryref(a,(p.tag in [jincrload,jloadincr]|kincrptr|kdecrptr))
	fi
end

proc do_new(unit p)=
	int n
	unit q

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

	callhostfn(h_new)

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
!			genpc_name(kpushmref,stcurrproc)
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
end

proc genfree(int n)=
	genpc_int(kunshare,n)
end

proc do_return(unit p,a)=
	if a then
		evalunit(a)
	elsif currfunction=2 then
		gerror("function needs return value")
	fi

	genjumpl(retindex)
end

proc do_multassign(unit a,b, int deepcopy, res)=
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
		if deepcopy then
			genpc(kcopy)
		fi

		do_store(plist[i])
	od
end

proc do_store(unit a,int res=0)=
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
			genpc_name(kpushf,d)
			genpc(kpopptr)
		elsif res then
			genpc_name(kstorem+d.isframe,d)
		elsif d.nameid=dllvarid then
			genpc_name(kpopx,d)

		else
			genpc_name(kpopm+d.isframe,d)
		fi

	when jdot then
		evalunit(a.a)
		genpc_name(kpopdot,a.b.def)

	when jindex then
		do_bin(a.a, a.b, kpopindex)
	when jdotindex then

		evalref(a.a)
		evalunit(a.b)
		genpc(kpopdotindex)
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

proc do_convert(unit pconv)=
!apply type-conversion t on expression p

!also do constructors
	int n,elemmode,i,lowerx,lbound,m,mbase,nfields
	[maxunits]unit plist
	unit p

	m:=pconv.mode
	p:=pconv.a
	mbase:=ttbasetype[m]

!p.length is no. of elements, but it not used here(unitstoarray will count
!anyway). But a value of -1 (rather than 1) means a trailing comma was used.

	if p.tag<>jmakelist  OR MBASE=TREFPACK then		!assume regular type conversion
			if p.tag=jmakelist then
				deleteunit(p,p.a)
			fi
			evalunit(p)
			genpc_int(kconvert,m)
			return
!		fi
	fi

!a is a usertype
	n:=unitstoarray(p.a,&plist,maxunits)

	if n and plist[1].tag=jkeyvalue then
		case mbase
		when trecord, tstruct then
			do_makerecordkv(m,n,plist)
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
		lowerx:=p.lower
		genpc_int2(kmakelist,n,lowerx)

	when tarray then
		genpc_int4(kmakearray,p.lower,n,tarray,p.elemtype)

!	when tvector then
	when tvector then
		elemmode:=tttarget[m]
		lowerx:=ttlower[m]

		checkelems(n,ttlength[m],p)
		genpc_int4(kmakearray,lowerx,n,m,elemmode)

	when tbits then
		if m=tbits then			!not user-defined
			genpc_int4(kmakebits,p.lower,n,tbits,(p.elemtype=tvoid|tu1|p.elemtype))
		else
			gerror("user-define bit array not ready")
		fi

	when tset then
		genpc_int(kmakeset,n)

	else
		gerror_s("Convert list",strmode(mbase))
	esac
end

!proc do_case(unit p,pindex,pwhenthen,int res) =
proc checkelems(int n,length, unit p)=
	if n<length then
		gerror("Too few elements")
	elsif n>length then
		gerror("Too many elements")
	fi
end

proc do_switch(unit p,pindex,pwhenthen, int res) =
	int minlab,maxlab,x,y,i,n
	unit w,wt, pelse

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
				gerror_s("Switch when2: not const",strexpr(w).strptr)
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

!fill in zero entries with else
	definefwdlabel(elselab)
	if pelse then		!do else part
		evalunit(pelse,res)
	fi	

	if loopsw then
		genjumpl(lab_a)
		definefwdlabel(lab_d)
		unstacklooplabels()
	else
		definefwdlabel(lab_d)
	fi

	for i:=1 to n do
		if labels[i]^=0 then
			labels[i]^:=elselab
		fi
	od
	labels[n+1]^:=elselab
end

proc do_makerecordkv(int m,nkeyvals, []unit &kvlist)=
	unit p
	[maxunits]unit plist
	int nfields, index
	symbol d:=ttnamedef[m], e, f, k

	e:=d.deflist
	nfields:=0

	while e,e:=e.nextdef do
		if e.nameid in [fieldid,structfieldid] and e.atfield=nil then
			++nfields
			plist[nfields]:=nil
		fi
	od

	for i to nkeyvals do
		k:=kvlist[i].a.def
		p:=kvlist[i].b

		e:=d.deflist
		f:=nil
		while e,e:=e.nextdef do
			if e.nameid in [fieldid,structfieldid] and e.firstdupl=k then
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
	od

	for i to nfields do
		if plist[i] then
			evalunit(plist[i])
		else
			genpc_int(kpushci,0)
		fi
	od

	genpc_int2(kmakerecord,nfields,m)
end

proc do_idiv(unit p,a,b)=
	int n

	evalunit(a)
	if b.tag=jintconst and (n:=ispoweroftwo(b.value)) then
		genpc_int(kpushci,n)
		genpc(kshr)
	else
		evalunit(b)
		genpc(kidiv)
	fi
end

proc do_irem(unit p,a,b)=
	int n
	word m

	evalunit(a)
	if b.tag=jintconst and (n:=ispoweroftwo(b.value)) then
		m:=inot(0xFFFF'FFFF'FFFF'FFFF << n)
		genpc_int(kpushci,M)
		genpc(kiand)
	else
		evalunit(b)
		genpc(kirem)
	fi
end

proc do_map(unit p,popcode,x)=
	evalunit(x)
	if x.nextunit then
		evalunit(x.nextunit)
	fi
	evalunit(popcode)
	genpc((x.nextunit|kmapss|kmaps))

	int lab:=createfwdlabel()
	genpc_lab(kjump,lab)		!dummy jump to be moved to runtime-generated code
	genpc(knop)					!stop jump being optimised out
	definefwdlabel(lab)
end

proc pushstring(ichar s, int length)=
	genpc(kpushcs)
	ref stringrec ps:=pcm_alloc(stringrec.bytes)
	ps.svalue:=s
	ps.length:=length
	genopnd_int(cast(ps))
end

=== qq_pcllib.m 0 0 23/31 ===
const pclinitalloc=128

global ref int pclstart				!point to start of current pcl block
global ref int pclnext				!point to next available int in pcl block
global ref int pclend				!point to last allocated int (with enough margin for on extra instr)
global ref int pcllast				!points to start of last pcl instruction in pcl block
global int pclalloc					!ints allocated

global ref int32 pclsrcstart
global ref int32 pclsrcnext

global int pclcurrlineno			!current line number
const pclelemsize=int.bytes
const pclsrcelemsize=int32.bytes

global const labelinitalloc=8192
global ref[]int labeloffsettable
global int labelalloc
global int nextlabelno
int labelflag

global [0..pclnames.upb]byte pclnopnds

proc start=
	int nn

	for i:=1 to klastpcl do
		nn:=0
		for j:=1 to 4 do
			if pclfmt[i,j]=0 then exit fi
			++nn
		od
		pclnopnds[i]:=nn
	od

	pcm_init()

!label/block tables are not needed after the pcl sequence has been
!generated. But they are not freed; they can be reused, with their
!current sizes, for the next module. (Might be inefficient if there is one
!very large module, then mainly small ones.)

	labelalloc:=labelinitalloc
	labeloffsettable:=pcm_alloc(int.bytes*labelalloc)
end

global proc resetpcl(int sourcesize)=
	int pclsize

	qpos:=0
	nextlabelno:=0
	pclcurrlineno:=0

!pcl dest is reallocated for each module
!Any current pcl data is presumably retained so that it can be run.

	pclsize:=sourcesize			!estimated num of pcl bytecode elements

	pclalloc:=1024					!min
	while pclalloc<pclsize do
		pclalloc<<:=1
	od

	pclstart:=pcm_alloc(pclalloc*pclelemsize)
	pclnext:=pclstart
	pclend:=pclstart+pclalloc-16			!allow margin for 1-2 pcl ops
	pcllast:=nil

	pclsrcstart:=pcm_alloc(pclalloc*pclsrcelemsize)
	pclsrcnext:=pclsrcstart

end

global proc genpc(int opc)=
!	if opc=0 then
!		GERROR("ZERO PCL OP?")
!	fi
!IF OPC>PCLNAMES.LEN THEN
!GERROR("GENPC:BAD OPC")
!FI

	if pclnext>=pclend then
		extendpcldata()
	fi

!only do overflow check at start of an instruction
	pclnext^:=opc
	pcllast:=pclnext

!IF QPOS=0 THEN
!	CPL "STORE QPOS",QPOS, PCLNAMES[OPC]
!FI
	pclsrcnext^:=qpos

	++pclnext
	++pclsrcnext
	labelflag:=0
end

global proc genopnd_int(int64 x)=
!no pcindex overflow check needed, as the genpc() check will be sufficient as
!it would allow for enough operands
	pclnext++^:=x
	++pclsrcnext
end

global proc genopnd_name(ref strec d)=
!	pclnext++^:=int@(d)
	pclnext++^:=int(d)
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
!	if pcllast^=kpopf and opc=kpushf and not labelflag and (pcllast+1)^=int64@(d) then
	if pcllast^=kpopf and opc=kpushf and not labelflag and (pcllast+1)^=int64(d) then
		pcllast^:=kstoref
		return
	fi

	genpc(opc)
!	pclnext++^:=int64@(d)
	pclnext++^:=int64(d)
	++pclsrcnext
end

global proc genopnd_strz(ichar s)=
!s must be a heap string, be a constant, or otherwise be persistent
!	pclnext++^:=int64@(s)
	pclnext++^:=int64(s)
	++pclsrcnext
end

global proc genopnd_str(object s)=
!s must be a heap string, be a constant, or otherwise be persistent
!	pclnext++^:=int64@(s)
	pclnext++^:=int64(s)
	++pclsrcnext
end

global proc genopnd_obj(object p)=
!	pclnext++^:=int64@(p)
	pclnext++^:=int64(p)
	++pclsrcnext
end

global proc genpc_real(int opc, real x)=
	genpc(opc)
	pclnext++^:=int64@(x)
!	pclnext++^:=int64(x)
	++pclsrcnext
end

global proc genpc_lab(int opc, int a)=
	int lastpc
	genpc(opc)
	genopnd_lab(a)
end

global proc genopnd_lab(int a)=
	int lastpc

	if a>=0 then				!normal, defined label
		pclnext++^:=a
!CPL "GENLAB",=A
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

!global function getdottedname(ref strec p)ichar=
!!build full dotted name for st item p
!	static [256]char str
!	[256]char str2
!	ref strec owner
!
!	strcpy(&.str,p.name)
!	return &.str
!end
!
proc extendpcldata=
	int newpclalloc
	ref int newpclstart
	ref int32 newpclsrcstart

	newpclalloc:=pclalloc*2

!CPL "EXTENDING PCL TABLE TO",=PCLSTART

	newpclstart:=pcm_alloc(pclelemsize*newpclalloc)
	newpclsrcstart:=pcm_alloc(pclsrcelemsize*newpclalloc)

	memcpy(newpclstart,pclstart, (pclnext-pclstart)*pclelemsize)
	memcpy(newpclsrcstart,pclsrcstart, (pclnext-pclstart)*pclsrcelemsize)

	pclnext:=newpclstart+(pclnext-pclstart)
	pclend:=newpclstart+newpclalloc-10
	pcllast:=newpclstart+(pcllast-pclstart)
	pclsrcnext:=newpclsrcstart+(pclsrcnext-pclsrcstart)

	pcm_free(pclstart,pclalloc*pclelemsize)
	pcm_free(pclsrcstart,pclalloc*pclsrcelemsize)

	pclstart:=newpclstart
	pclalloc:=newpclalloc
	pclsrcstart:=newpclsrcstart
end

global proc extendlabeltable=
	int newlabelalloc
	ref[]int newlabeltable

	newlabelalloc:=labelalloc*2

	newlabeltable:=pcm_alloc(int.bytes*newlabelalloc)

	memcpy(newlabeltable,labeloffsettable, labelalloc*int.bytes)

	pcm_free(labeloffsettable,labelalloc*int.bytes)

	labeloffsettable:=newlabeltable
	labelalloc:=newlabelalloc
end

global function definelabel:int=
	if nextlabelno>=labelalloc then extendlabeltable() fi
	++nextlabelno
	labeloffsettable^[nextlabelno]:=pclnext-pclstart
	labelflag:=1
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
	int newlab,index,laboffset,pc,nextpc

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
	labelflag:=1
end

!global function isstatic(symbol d)int=
!	return d.nameid=staticid
!end
=== qq_records.m 0 0 24/31 ===
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

=== qq_resolve.m 0 0 25/31 ===
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

	when jadd, jsub, jmul, jdiv, jidiv, jirem, jiand, jior, jixor,
		jshl, jshr, jmakerange then
		rx_unit(owner,a)
		if not b then rxerror("Binop missing opnd") fi
		rx_unit(owner,b)
		evalbinop(p,a,b)

	when jneg, jabs,jlen, jlwb, jupb,jbytesize,jsqrt then
		rx_unit(owner,a)
		evalmonop(p)

	when jforup,jfordown then			!a will be jname unit
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
	when jbytesize then
		if p.a.tag=jtypeconst then
			c:=ttsize[p.a.mode]
			newint
		fi
	
	elsecase p.a.tag
	when jintconst then
		a:=p.a.value

		case p.tag
		when jneg then c:=-a
		when jabs then c:=abs(a)
		else
			return
		esac

newint:
		makeintconst(p,c)

	when jrealconst then
		x:=p.a.xvalue

		case p.tag
		when jneg then z:=-x
		when jabs then z:=abs(x)
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

	case pr(lhs.tag,rhs.tag)
	when pr(jintconst, jintconst) then
		a:=lhs.value
		b:=rhs.value

		case p.tag
		when jadd then c:=a+b
		when jsub then c:=a-b
		when jmul then c:=a*b
		when jidiv then
			if b=0 then rxerror("x/0") fi
			c:=a/b
		else
			return
		esac

		makeintconst(p,c)

	when pr(jrealconst, jrealconst) then
		x:=lhs.xvalue
		y:=rhs.xvalue

		case p.tag
		when jadd then z:=x+y
		when jsub then z:=x-y
		when jmul then z:=x*y
		when jdiv then z:=x/y
		else
			return
		esac

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
						rxerror_s("recursive type?",ttname[m])
					fi
				fi
			fi

			p:=p.nextmode
		od
		if not rescan then exit fi

	od
	if rescan then
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
=== qq_sets.m 0 0 26/31 ===
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
		p.alloc64:=word64(allocbytes)*8
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

=== qq_strings.m 0 0 27/31 ===
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
=== qq_syslibsdummy.m 0 0 28/31 ===
global const fsyslibs = 0

global func loadsysmodule(ifile pm)int=
	return 0
end
=== qq_tables.m 0 0 29/31 ===
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

global enumdata [0:]ichar jtagnames,			! "jadd" etc
					[0:]u32 jshortnames,		! "+" etc, used in jeval()
					[0:]byte jflags,			! 0/1/2 = 0, 1 or 2 subtrees
					[0:]int16 jpclcodes,		! for arith, corresponding pcl opc
					[0:]byte jtocodes,	 		! for arith, corresponding jaddto op etc
					[0:]byte jhasvalue = 		! whether yields a value (0, 1 or 2=special)
	(jnone=0,			$,		0,		0,		0,			0,	0),	
	(jlabeldef,			$,		0,		1,		0,			0,	0),
	(jassign,			$,		0,		2,		0,			0,	2),
	(jdeepcopy,			$,		0,		2,		0,			0,	2),
	(jkeyword,			$,		0,		2,		0,			0,	1),
	(jkeyvalue,			$,		0,		2,		0,			0,	1),
	(joperator,			$,		0,		0,		0,			0,	1),
	(jblock,			$,		0,		1,		0,			0,	2),
	(jif,				$,		0,		2,		0,			0,	2),
	(jselect,			$,		0,		2,		0,			0,	2),
	(jwhenthen,			$,		0,		2,		0,			0,	0),
	(jcase,				$,		0,		2,		0,			0,	2),
	(jdocase,			$,		0,		2,		0,			0,	0),
	(jswitch,			$,		0,		2,		0,			0,	2),
	(jdoswitch,			$,		0,		2,		0,			0,	0),
	(jrecase,			$,		0,		1,		0,			0,	0),
	(jforup,			$,		0,		2,		0,			0,	0),
	(jforupx,			$,		0,		2,		0,			0,	0),
	(jfordown,			$,		0,		2,		0,			0,	0),
	(jfordownx,			$,		0,		2,		0,			0,	0),
	(jforall,			$,		0,		2,		0,			0,	0),
	(jforallrev,		$,		0,		2,		0,			0,	0),
	(jforeach,			$,		0,		2,		0,			0,	0),
	(jdo,				$,		0,		1,		0,			0,	0),
	(jto,				$,		0,		2,		0,			0,	0),
	(jwhile,			$,		0,		2,		0,			0,	0),
	(jrepeat,			$,		0,		2,		0,			0,	0),
	(jtry,				$,		0,		2,		0,			0,	0),
	(jexcept,			$,		0,		2,		0,			0,	0),
	(jraise,			$,		0,		1,		0,			0,	0),
	(jcall,				$,		0,		2,		0,			0,	1),
	(jcallhost,			$,		0,		1,		0,			0,	1),
	(jnil,				$,		0,		0,		0,			0,	1),
	(jswap,				$,		0,		2,		0,			0,	0),
	(jgoto,				$,		0,		1,		0,			0,	0),
	(jstop,				$,		0,		1,		0,			0,	0),
	(jreturn,			$,		0,		1,		0,			0,	2),
	(jtypeconst,		$,		0,		0,		0,			0,	1),
	(jeval,				$,		0,		1,		0,			0,	0),

	(jconvert,			$,		0,		1,		0,			0,	1),
	(jtypepun,			$,		0,		1,		0,			0,	1),
	(jmap,				$,		0,		2,		kmaps,		0,	1),

	(jcmpchain,			$,		0,		1,		0,			0,	1),
	(jname,				$,		0,		0,		0,			0,	1),
	(jsymbol,			$,		0,		1,		0,			0,	1),
	(jintconst,			$,		0,		0,		0,			0,	1),
	(jrealconst,		$,		0,		0,		0,			0,	1),
	(jstringconst,		$,		0,		0,		0,			0,	1),
	(jstrinclude,		$,		0,		1,		0,			0,	1),
	(jdot,				$,		0,		2,		0,			0,	1),
	(jindex,			$,		0,		2,		0,			0,	1),
	(jdotindex,			$,		0,		2,		0,			0,	1),
	(jkeyindex,			$,		0,		2,		0,			0,	1),
	(jredo,				$,		0,		2,		0,			0,	0),
	(jnext,				$,		0,		2,		0,			0,	0),
	(jexit,				$,		0,		2,		0,			0,	0),
	(jptr,				$,		0,		1,		0,			0,	1),
	(jaddrof,			$,		0,		1,		0,			0,	1),
	(jptrto,			$,		0,		1,		0,			0,	1),
!	(jdaddrof,			$,		0,		1,		0,			0,	1),
	(jnull,				$,		0,		0,		0,			0,	1),
	(jprint,			$,		0,		2,		0,			0,	0),
	(jprintln,			$,		0,		2,		0,			0,	0),
	(jfprint,			$,		0,		2,		0,			0,	0),
	(jfprintln,			$,		0,		2,		0,			0,	0),
	(jsprint,			$,		0,		2,		0,			0,	1),
	(jsfprint,			$,		0,		2,		0,			0,	1),
	(jnogap,			$,		0,		0,		0,			0,	0),
	(jspace,			$,		0,		0,		0,			0,	0),
	(jfmtitem,			$,		0,		2,		0,			0,	0),
	(jread,				$,		0,		2,		0,			0,	0),
	(jreadln,			$,		0,		2,		0,			0,	0),
	(jdecimal,			$,		0,		0,		0,			0,	1),
	(jincr,				$,		'++',		1,		0,			0,	1),
	(jdecr,				$,		'--',		1,		0,			0,	1),
	(jincrload,			$,		0,		1,		kincrload,	0,	1),
	(jdecrload,			$,		0,		1,		kdecrload,	0,	1),
	(jloadincr,			$,		0,		1,		kloadincr,	0,	1),
	(jloaddecr,			$,		0,		1,		kloaddecr,	0,	1),
	(jneg,				$,		'-',		1,		kneg,		jnegto,	1),
	(jabs,				$,		'abs',		1,		kabs,		jabsto,	1),
	(jnotl,				$,		'not',		1,		knotl,		jnotlto,	1),
	(jinot,				$,		'inot',		1,		kinot,		jinotto,	1),
	(jistruel,			$,		'isT',		1,		kistruel,	0,	1),
	(jasc,				$,		0,		1,		kasc,		0,	1),
	(jchr,				$,		0,		1,		kchr,		0,	1),
	(jsqrt,				$,		0,		1,		ksqrt,		0,	1),
	(jsqr,				$,		0,		1,		ksqr,		0,	1),
	(jsin,				$,		0,		1,		ksin,		0,	1),
	(jcos,				$,		0,		1,		kcos,		0,	1),
	(jtan,				$,		0,		1,		ktan,		0,	1),
	(jasin,				$,		0,		1,		kasin,		0,	1),
	(jacos,				$,		0,		1,		kacos,		0,	1),
	(jatan,				$,		0,		1,		katan,		0,	1),
	(jlog,				$,		0,		1,		klog,		0,	1),
	(jlog10,			$,		0,		1,		klog10,		0,	1),
	(jexp,				$,		0,		1,		kexp,		0,	1),
	(jround,			$,		0,		1,		kround,		0,	1),
	(jfloor,			$,		0,		1,		kfloor,		0,	1),
	(jceil,				$,		0,		1,		kceil,		0,	1),
	(jfract,			$,		0,		2,		kfract,		0,	1),
	(jfmod,				$,		0,		2,		kfmod,		0,	1),
	(jsign,				$,		0,		1,		ksign,		0,	1),
	(jnegto,			$,		0,		1,		knegto,		0,	0),
	(jabsto,			$,		0,		1,		kabsto,		0,	0),
	(jnotlto,			$,		0,		1,		knotlto,	0,	0),
	(jinotto,			$,		0,		1,		kinotto,	0,	0),
	(jlen,				$,		0,		1,		klen,		0,	1),
	(jlwb,				$,		0,		1,		klwb,		0,	1),
	(jupb,				$,		0,		1,		kupb,		0,	1),
	(jbounds,			$,		0,		1,		kbounds,	0,	1),
	(jboundsx,			$,		0,		1,		kboundsx,	0,	1),
	(jbitwidth,			$,		0,		1,		kbitwidth,	0,	1),
	(jbytesize,			$,		0,		1,		kbytesize,	0,	1),
!	(jtype,				$,		0,		1,		ktype,		0,	1),
	(jelemtype,			$,		0,		1,		kelemtype,	0,	1),
	(jbasetype,			$,		0,		1,		kbasetype,	0,	1),
	(jtype,				$,		0,		1,		ktype,		0,	1),
	(jdictitems,		$,		0,		1,		kdictitems,	0,	1),
	(jisfound,			$,		0,		1,		kisfound,	0,	1),
	(jminvalue,			$,		0,		1,		kminvalue,	0,	1),
	(jmaxvalue,			$,		0,		1,		kmaxvalue,	0,	1),
	(jisint,			$,		0,		1,		kisint,		0,	1),
	(jisreal,			$,		0,		1,		kisreal,	0,	1),
	(jisstring,			$,		0,		1,		kisstring,	0,	1),
	(jisrange,			$,		0,		1,		kisrange,	0,	1),
	(jisnumber,			$,		0,		1,		kisnumber,	0,	1),
	(jislist,			$,		0,		1,		kislist,	0,	1),
	(jisrecord,			$,		0,		1,		kisrecord,	0,	1),
	(jispointer,		$,		0,		1,		kispointer,	0,	1),
	(jisarray,			$,		0,		1,		kisarray,	0,	1),
	(jismutable,		$,		0,		1,		kismutable,	0,	1),
	(jisset,			$,		0,		1,		kisset,		0,	1),
	(jisvoid,			$,		0,		1,		kisvoid,	0,	1),
	(jisdef,			$,		0,		1,		kisdef,		0,	1),
	(jisequal,			$,		0,		2,		kisequal,	0,	1),
	(jodd,				$,		0,		2,		kodd,		0,	1),
	(jeven,				$,		0,		2,		keven,		0,	1),
	(jadd,				$,		'+',		2,		kadd,		jaddto,	1),
	(jsub,				$,		'-',		2,		ksub,		jsubto,	1),
	(jmul,				$,		'*',		2,		kmul,		jmulto,	1),
	(jdiv,				$,		'/',		2,		kdiv,		jdivto,	1),
	(jidiv,				$,		'%',		2,		kidiv,		jidivto,	1),
	(jirem,				$,		'rem',		2,		kirem,		0,	1),
	(jidivrem,			$,		0,		2,		kidivrem,	0,	1),
	(jiand,				$,		'iand',		2,		kiand,		jiandto,	1),
	(jior,				$,		'ior',		2,		kior,		jiorto,	1),
	(jixor,				$,		'ixor',		2,		kixor,		jixorto,	1),
	(jshl,				$,		'<<',		2,		kshl,		jshlto,	1),
	(jshr,				$,		'>>',		2,		kshr,		jshrto,	1),
	(jin,				$,		'in',		2,		kin,		0,	1),
	(jnotin,			$,		'~in',	2,		knotin,		0,	1),
	(jinx,				$,		'inx',		2,		kinx,		0,	1),
	(jinrev,			$,		0,		2,		0,			0,	0),
	(jandl,				$,		'and',		2,		0,		jandlto,	1),
	(jorl,				$,		'or',		2,		0,		jorlto,	1),
	(jeq,				$,		'=',		2,		keq,		0,	1),
	(jne,				$,		'<>',		2,		kne,		0,	1),
	(jlt,				$,		'<',		2,		klt,		0,	1),
	(jle,				$,		'<=',		2,		kle,		0,	1),
	(jge,				$,		'>=',		2,		kge,		0,	1),
	(jgt,				$,		'>',		2,		kgt,		0,	1),
	(jmin,				$,		'min',		2,		kmin,		jminto,	1),
	(jmax,				$,		'max',		2,		kmax,		jmaxto,	1),
	(jconcat,			$,		0,		2,		kconcat,	jconcatto,	1),
	(jappend,			$,		0,		2,		kappend,	jappendto,	1),
	(jpower,			$,		'**',		2,		kpower,		0,	1),
	(jatan2,			$,		0,		2,		katan2,		0,	1),
	(jaddto,			$,		0,		2,		kaddto,		0,	0),
	(jsubto,			$,		0,		2,		ksubto,		0,	0),
	(jmulto,			$,		0,		2,		kmulto,		0,	0),
	(jdivto,			$,		0,		2,		kdivto,		0,	0),
	(jidivto,			$,		0,		2,		kidivto,	0,	0),
	(jandlto,			$,		0,		2,		kandlto,	0,	0),
	(jorlto,			$,		0,		2,		korlto,		0,	0),
	(jiandto,			$,		0,		2,		kiandto,	0,	0),
	(jiorto,			$,		0,		2,		kiorto,		0,	0),
	(jixorto,			$,		0,		2,		kixorto,	0,	0),
	(jshlto,			$,		0,		2,		kshlto,		0,	0),
	(jshrto,			$,		0,		2,		kshrto,		0,	0),
	(jminto,			$,		0,		2,		kminto,		0,	0),
	(jmaxto,			$,		0,		2,		kmaxto,		0,	0),
	(jconcatto,			$,		0,		2,		kconcatto,	0,	0),
	(jappendto,			$,		0,		2,		kappendto,	0,	0),

	(jmakerange,		$,		0,		2,		kmakerange,	0,	1),
	(jmakerangelen,		$,		0,		2,		kmakerangelen,	0,	1),
	(jmakelist,			$,		0,		1,		kmakelist,	0,	1),
	(jmakeset,			$,		0,		1,		kmakeset,	0,	1),
	(jmakedict,			$,		0,		1,		kmakedict,	0,	1),

	(jcvlineno,			$,		0,		0,		0,	0,	1),
	(jcvstrlineno,		$,		0,		0,		0,	0,	1),
	(jcvmodulename,		$,		0,		0,		0,	0,	1),
	(jcvfilename,		$,		0,		0,		0,	0,	1),
	(jcvfunction,		$,		0,		0,		0,	0,	1),
	(jcvdate,			$,		0,		0,		0,	0,	1),
	(jcvtime,			$,		0,		0,		0,	0,	1),
	(jcvversion,		$,		0,		0,		0,	0,	1),
	(jcvpclversion,		$,		0,		0,		0,	0,	1),
end

global type qd=[4]byte

global enumdata [0:]ichar opndnames=
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
	(coperator,	$),			!o Operator		Operator

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
const o = coperator

!Stack operands labeled X,Y,Z:
!X		X is top of the stack (1 operand)
!X,Y	Y is top of the stack (2 operands)
!X,Y,Z	Z is top of the stack (3 operands)
!suffixes a,b,c help indicate which operand goes where:
!a		always top of the second
!b		always second from the top
!c		always third from the top
!So Xb and Ya when there are two operands; Y is on top
!flags abcB uses a '1' bit to indicate that .a, .b or .c contains a unit list

!! ----

global enumdata  [0:]ichar pclnames, [0:]qd pclfmt, [0:]REF PROC pclhandlers =
	(kzero=0,		$,	(0,0,0,0),	cast(kunimpl        )),
	(knop,			$,	(0,0,0,0),	cast(k_nop          )),		!simple nop
	(kskip,			$,	(0,0,0,0),	cast(kunimpl        )),		!ignore on pcl listing

	(kprocdef,		$,	(d,0,0,0),	cast(kunimpl        )),		!
	(kprocentry,	$,	(i,0,0,0),	cast(k_procentry    )),		!A=number of locals; 
	(kprocend,		$,	(0,0,0,0),	cast(kunimpl        )),
	(kendmodule,	$,	(0,0,0,0),	cast(kunimpl        )),		!Last 'executable' opcode
	(kcomment,		$,	(z,0,0,0),	cast(k_comment      )),

	(klabeldef,		"",	(d,0,0,0),	cast(kunimpl        )),		!

	(kpushm,		$,	(m,0,0,0),	cast(k_pushm        )),		!Push [A]
	(kpushf,		$,	(f,0,0,0),	cast(k_pushf        )),		!Push [A]
	(kpushmref,		$,	(m,0,0,0),	cast(k_pushmref     )),		!push &A
	(kpushfref,		$,	(f,0,0,0),	cast(k_pushfref     )),		!push &A
	(kpopm,			$,	(m,0,0,0),	cast(k_popm         )),		!A:=Xa
	(kpopf,			$,	(f,0,0,0),	cast(k_popf         )),		!A:=Xa
	(kstorem,		$,	(m,0,0,0),	cast(k_storem       )),		!A:=Xa
	(kstoref,		$,	(f,0,0,0),	cast(k_storef       )),		!A:=Xa
	(kpushx,		$,	(d,0,0,0),	cast(kunimpl        )),		!Push [A]	DLL VARs
	(kpopx,			$,	(d,0,0,0),	cast(kunimpl        )),		!Pop [A]

	(kpushci,		$,	(i,0,0,0),	cast(k_pushci       )),		!Push constant signed int
	(kpushvoid,		$,	(0,0,0,0),	cast(k_pushvoid     )),		!
	(kpushnil,		$,	(0,0,0,0),	cast(k_pushnil      )),		!
	(kpushcr,		$,	(r,0,0,0),	cast(k_pushcr       )),		!Push constant real
	(kpushcn,		$,	(n,0,0,0),	cast(kunimpl        )),		!Push range

	(kpushcs,		$,	(s,0,0,0),	cast(k_pushcs       )),		!Push constant string object

	(kpusht,		$,	(t,0,0,0),	cast(k_pusht        )),		!Push type constant
	(kpushsymbol,	$,	(d,0,0,0),	cast(k_pushsymbol   )),		!Push symbol reference
	(kpushoperator,	$,	(o,0,0,0),	cast(k_pushoperator )),		!Push operator code (pcl code)

	(kpushptr,		$,	(0,0,0,0),	cast(k_pushptr      )),		!Push Xa^
	(kpopptr,		$,	(0,0,0,0),	cast(k_popptr       )),		!Ya^:=Xb; then pop both

	(kzpopm,		$,	(m,0,0,0),	cast(k_zpopm        )),		!Pop A; do not free A first
	(kzpopf,		$,	(f,0,0,0),	cast(k_zpopf        )),		!Pop A; do not free A first

	(kdupl,			$,	(0,0,0,0),	cast(k_dupl         )),		!Xa:=share(Xa), keep original on stack
	(kcopy,			$,	(0,0,0,0),	cast(k_copy         )),		!Xa:=deepcopy(Xa)
	(kswap,			$,	(0,0,0,0),	cast(k_swap         )),		!Yb^:=:Xa^; Xa^:=:A; A:=:B

	(kconvrefpack,	$,	(0,0,0,0),	cast(k_convrefpack  )),		!Change ref in X to refpacked

	(kjump,			$,	(l,0,0,0),	cast(k_jump         )),		!Jump to L
	(kjumpptr,		$,	(0,0,0,0),	cast(k_jumpptr      )),		!Jump to Xa^

	(kjumptrue,		$,	(l,0,0,0),	cast(k_jumptrue     )),		!Jump to L when Xa is true
	(kjumpfalse,	$,	(l,0,0,0),	cast(k_jumpfalse    )),		!Jump to L when Xa is false

	(kjumpeq,		$,	(l,0,0,0),	cast(k_jumpeq       )),		!Jump to L when Xb=Ya, Xa=A, A=B; (X,Y popped)
	(kjumpne,		$,	(l,0,0,0),	cast(k_jumpne       )),		!Jump to L when Xb<>Ya
	(kjumplt,		$,	(l,0,0,0),	cast(k_jumplt       )),		!Jump to L when Xb<Ya
	(kjumple,		$,	(l,0,0,0),	cast(k_jumple       )),		!Jump to L when Xb<=Ya
	(kjumpge,		$,	(l,0,0,0),	cast(k_jumpge       )),		!Jump to L when Xb>=Ya
	(kjumpgt,		$,	(l,0,0,0),	cast(k_jumpgt       )),		!Jump to L when Xb>Ya

	(kjumptesteq,	$,	(l,0,0,0),	cast(k_jumptesteq   )),		!Jump to L when Xb=Ya (Ya popped), or Xa=A; int/set and int/range use 'in' to compare
	(kjumptestne,	$,	(l,0,0,0),	cast(k_jumptestne   )),		!Jump to L when Xb<>Ya

	(kjumplabel,	$,	(l,0,0,0),	cast(kunimpl        )),		!Jumptable entry

	(kswitch,		$,	(i,i,0,0),	cast(k_switch       )),		!Jumptable has n entries, ci is lower bound. Jump indexed by Xa

	(ktom,			$,	(l,m,0,0),	cast(k_tom          )),		!
	(ktof,			$,	(l,f,0,0),	cast(k_tof          )),		!

	(kformci,		$,	(l,m,i,0),	cast(k_formci       )),		!
	(kforfci,		$,	(l,f,i,0),	cast(k_forfci       )),		!
	(kformm,		$,	(l,m,m,0),	cast(k_formm        )),		!
	(kforff,		$,	(l,f,f,0),	cast(k_forff        )),		!

	(kfordmci,		$,	(l,m,i,0),	cast(k_fordmci      )),		!
	(kfordfci,		$,	(l,f,i,0),	cast(k_fordfci      )),		!
	(kfordmm,		$,	(l,m,m,0),	cast(k_fordmm       )),		!
	(kfordff,		$,	(l,f,f,0),	cast(k_fordff       )),		!

	(kcallproc,		$,	(p,i,0,0),	cast(k_callproc     )),		!Call &A; A is cmemoryref; B is no. args
	(kcallptr,		$,	(i,i,0,0),	cast(k_callptr      )),		!Call X^; A is no. of params supplied; B is stack adjust
	(kreturn0,		$,	(0,0,0,0),	cast(k_return0      )),		!A is no. params to free; Return from function, with optional value in caller's retval slot
	(kreturn,		$,	(i,0,0,0),	cast(k_return       )),		!A is no. params to free; Return from function, with optional value in caller's retval slot
	(kpopretval,	$,	(i,0,0,0),	cast(k_popretval    )),		!pop stack to caller's return slot; i=offset

	(kmodulecall,	$,	(d,0,0,0),	cast(k_modulecall   )),		!
	(kmodulereturn,	$,	(0,0,0,0),	cast(k_modulereturn )),		!

	(kcalldll,		$,	(x,i,0,0),	cast(k_calldll      )),		!Call dll function A (sysmbol); B=nargs

	(kcallhost,		$,	(i,0,0,0),	cast(k_callhost     )),		!Call Q host function A (Host index)

	(kunshare,		$,	(i,0,0,0),	cast(k_unshare      )),		!Unshare and pop A var values on stack
	(kaddsp,		$,	(i,0,0,0),	cast(k_addsp        )),		!SP+:=A; note: positive A will push, negative will pop (reverse of the hardware)

	(kstop,			$,	(0,0,0,0),	cast(k_stop         )),		!Stop program and return value X to any calling program
	(kstoprunproc,	$,	(0,0,0,0),	cast(k_stoprunproc  )),		!Used for reentrant callback calls

	(kmakelist,		$,	(i,i,0,0),	cast(k_makelist     )),		!A items on stack; make list with lwb B
	(kmakerecord,	$,	(i,t,0,0),	cast(k_makerecord   )),		!A items on stack; make record of type B
	(kmakearray,	$,	(i,i,t,t),	cast(k_makearray    )),		!A items on stack; make array with lwb B, type C and elemtype D
	(kmakebits,		$,	(i,i,t,t),	cast(k_makebits     )),		!A items on stack; make bits with lwb B, type C and elemtype D
	(kmakestruct,	$,	(i,t,0,0),	cast(k_makestruct   )),		!A items on stack; make struct with type B
	(kmakeset,		$,	(i,0,0,0),	cast(k_makeset      )),		!A items on stack; make set
	(kmakerange,	$,	(0,0,0,0),	cast(k_makerange    )),		!2 items on stack; make range
	(kmakerangelen,	$,	(0,0,0,0),	cast(k_makerangelen )),		!2 items on stack; make range; 2nd is length
	(kmakedict,		$,	(i,0,0,0),	cast(k_makedict     )),		!A*2 items on stack (A key:val items); make dict
	(kmakedecimal,	$,	(0,0,0,0),	cast(k_makedecimal  )),		!Turn string on stack to decimal number

	(kincrptr,		$,	(0,0,0,0),	cast(k_incrptr      )),		!++Xa^
	(kincrtom,		$,	(m,0,0,0),	cast(k_incrtom      )),		!++A
	(kincrtof,		$,	(f,0,0,0),	cast(k_incrtof      )),		!++A
	(kloadincr,		$,	(0,0,0,0),	cast(k_loadincr     )),		!T:=Xa^++
	(kincrload,		$,	(0,0,0,0),	cast(k_incrload     )),		!T:=--Xa^

	(kdecrptr,		$,	(0,0,0,0),	cast(k_decrptr      )),		!--Xa^; pop X
	(kdecrtom,		$,	(m,0,0,0),	cast(k_decrtom      )),		!--A
	(kdecrtof,		$,	(f,0,0,0),	cast(k_decrtof      )),		!--A
	(kloaddecr,		$,	(0,0,0,0),	cast(k_loaddecr     )),		!T:=Xa^--
	(kdecrload,		$,	(0,0,0,0),	cast(k_decrload     )),		!T:=--Xa^

	(kincr,			$,	(0,0,0,0),	cast(k_incr         )),		!T:=++T
	(kdecr,			$,	(0,0,0,0),	cast(k_decr         )),		!T:=--T

	(kneg,			$,	(0,0,0,0),	cast(k_neg          )),		!T:=-Xa; T:=-A
	(kabs,			$,	(0,0,0,0),	cast(k_abs          )),		!abs Xa
	(knotl,			$,	(0,0,0,0),	cast(k_notl         )),		!not Xa
	(kinot,			$,	(0,0,0,0),	cast(k_inot         )),		!inot Xa
	(kistruel,		$,	(0,0,0,0),	cast(k_istruel      )),		!istrue Xa
	(kasc,			$,	(0,0,0,0),	cast(k_asc          )),		!asc Xa
	(kchr,			$,	(0,0,0,0),	cast(k_chr          )),		!chr Xa

	(ksqrt,			$,	(0,0,0,0),	cast(k_sqrt         )),		!sqrt Xa
	(ksqr,			$,	(0,0,0,0),	cast(k_sqr          )),		!sqr Xa
	(ksin,			$,	(0,0,0,0),	cast(k_sin          )),		!sin Xa
	(kcos,			$,	(0,0,0,0),	cast(k_cos          )),		!cos Xa
	(ktan,			$,	(0,0,0,0),	cast(k_tan          )),		!tan Xa
	(kasin,			$,	(0,0,0,0),	cast(k_asin         )),		!asin Xa
	(kacos,			$,	(0,0,0,0),	cast(k_acos         )),		!acos Xa
	(katan,			$,	(0,0,0,0),	cast(k_atan         )),		!atan Xa
	(ksign,			$,	(0,0,0,0),	cast(k_sign         )),		!sign Xa
	(klog,			$,	(0,0,0,0),	cast(k_log           )),	!log Xa
	(klog10,		$,	(0,0,0,0),	cast(k_log10         )),	!log10 Xa
	(kexp,			$,	(0,0,0,0),	cast(k_exp          )),		!exp Xa
	(kround,		$,	(0,0,0,0),	cast(k_round        )),		!round Xa
	(kfloor,		$,	(0,0,0,0),	cast(k_floor        )),		!floor Xa
	(kceil,			$,	(0,0,0,0),	cast(k_ceil         )),		!ceil Xa
	(kfract,		$,	(0,0,0,0),	cast(k_fract        )),		!fract Xa
	(kfmod,			$,	(0,0,0,0),	cast(k_fmod         )),		!fmod(Xb, Ya)

	(knegto,		$,	(0,0,0,0),	cast(k_negto        )),		!-:=Xa^; -:=A
	(kabsto,		$,	(0,0,0,0),	cast(k_absto        )),		!abs:=^Xa; pop Xa
	(kinotto,		$,	(0,0,0,0),	cast(k_inotto       )),		!inot:=Xa^; pop Xa
	(knotlto,		$,	(0,0,0,0),	cast(k_notlto       )),		!not:=Xa^; pop Xa

	(klen,			$,	(0,0,0,0),	cast(k_len          )),		!T:=Xa.len
	(klwb,			$,	(0,0,0,0),	cast(k_lwb          )),		!Xa.lwb
	(kupb,			$,	(0,0,0,0),	cast(k_upb          )),		!Xa.upb
	(kbounds,		$,	(0,0,0,0),	cast(k_bounds       )),		!Xa.bounds (as one range value)
	(kboundsx,		$,	(0,0,0,0),	cast(k_boundsx      )),		!Xa.bounds (as two ints)
	(kbitwidth,		$,	(0,0,0,0),	cast(k_bitwidth     )),		!Xa.bitwidth
	(kbytesize,		$,	(0,0,0,0),	cast(k_bytesize     )),		!Xa.bytesize
!	(ktype,			$,	(0,0,0,0),	cast(k_type         )),		!Xa.type
	(ktype,			$,	(0,0,0,0),	cast(k_type 	    )),		!Xa.type
	(kbasetype,		$,	(0,0,0,0),	cast(k_basetype     )),		!Xa.basetype
	(kelemtype,		$,	(0,0,0,0),	cast(k_elemtype     )),		!Xa.elemtag
	(kdictitems,	$,	(0,0,0,0),	cast(k_dictitems    )),		!Xa.dictitems
	(kisfound,		$,	(0,0,0,0),	cast(k_isfound      )),		!Xa.isfound
	(kminvalue,		$,	(0,0,0,0),	cast(k_minvalue     )),		!Xa.minvalue
	(kmaxvalue,		$,	(0,0,0,0),	cast(k_maxvalue     )),		!Xa.maxvalue
	(kisint,		$,	(0,0,0,0),	cast(k_isint        )),		!Xa.isint
	(kisreal,		$,	(0,0,0,0),	cast(k_isreal       )),		!Xa.isreal
	(kisstring,		$,	(0,0,0,0),	cast(k_isstring     )),		!Xa.isstring
	(kisrange,		$,	(0,0,0,0),	cast(k_isrange      )),		!Xa.isrange
	(kisnumber,		$,	(0,0,0,0),	cast(k_isnumber     )),		!Xa.isnumber
	(kislist,		$,	(0,0,0,0),	cast(k_islist       )),		!Xa.isarray
	(kisrecord,		$,	(0,0,0,0),	cast(k_isrecord     )),		!Xa.isrecord
	(kispointer,	$,	(0,0,0,0),	cast(k_ispointer    )),		!Xa.ispointer
	(kisarray,		$,	(0,0,0,0),	cast(k_isarray      )),		!Xa.isarray
	(kismutable,	$,	(0,0,0,0),	cast(k_ismutable    )),		!Xa.ismutable
	(kisset,		$,	(0,0,0,0),	cast(k_isset        )),		!Xa.isset
	(kisvoid,		$,	(0,0,0,0),	cast(k_isvoid       )),		!Xa.isvoid
	(kisdef,		$,	(0,0,0,0),	cast(k_isdef        )),		!Xa.isdef
	(kisequal,		$,	(0,0,0,0),	cast(k_isequal      )),		!Xb==Ya
	(kconvert,		$,	(t,0,0,0),	cast(k_convert      )),		!Xa==A(Xa)
	(ktypepun,		$,	(t,0,0,0),	cast(k_typepun      )),		!Xa==A@(Xa)
	(kodd,			$,	(0,0,0,0),	cast(k_odd          )),		!Xa==Xa.odd
	(keven,			$,	(0,0,0,0),	cast(k_even         )),		!Xa==Xa.even

	(kadd,			$,	(0,0,0,0),	cast(k_add          )),		!T:=Xb+Ya
	(ksub,			$,	(0,0,0,0),	cast(k_sub          )),		!Xb-Ya
	(kmul,			$,	(0,0,0,0),	cast(k_mul          )),		!Xb*Ya
	(kdiv,			$,	(0,0,0,0),	cast(k_div          )),		!Xb/Ya
	(kidiv,			$,	(0,0,0,0),	cast(k_idiv         )),		!Xb%Ya
	(kirem,			$,	(0,0,0,0),	cast(k_irem         )),		!Xb rem Ya
	(kidivrem,		$,	(0,0,0,0),	cast(k_idivrem      )),		!Xb divrem Ya
	(kiand,			$,	(0,0,0,0),	cast(k_iand         )),		!Xb iand Ya
	(kior,			$,	(0,0,0,0),	cast(k_ior          )),		!Xb ior Ya
	(kixor,			$,	(0,0,0,0),	cast(k_ixor         )),		!Xb ixor Ya
	(kshl,			$,	(0,0,0,0),	cast(k_shl          )),		!Xb shl Ya
	(kshr,			$,	(0,0,0,0),	cast(k_shr          )),		!Xb shr Ya
	(kin,			$,	(0,0,0,0),	cast(k_in           )),		!Xb in Ya
	(knotin,		$,	(0,0,0,0),	cast(k_notin        )),		!Xb notin Ya
	(kinx,			$,	(0,0,0,0),	cast(k_inx          )),		!Xb inx Ya
	(keq,			$,	(0,0,0,0),	cast(k_eq           )),		!Xb=Ya
	(kne,			$,	(0,0,0,0),	cast(k_ne           )),		!Xb<>Ya
	(klt,			$,	(0,0,0,0),	cast(k_lt           )),		!Xb<Ya
	(kle,			$,	(0,0,0,0),	cast(k_le           )),		!Xb<=Ya
	(kge,			$,	(0,0,0,0),	cast(k_ge           )),		!Xb>=Ya
	(kgt,			$,	(0,0,0,0),	cast(k_gt           )),		!Xb>Ya
	(kmin,			$,	(0,0,0,0),	cast(k_min          )),		!Xb min Ya
	(kmax,			$,	(0,0,0,0),	cast(k_max          )),		!Xb max Ya
	(kconcat,		$,	(0,0,0,0),	cast(k_concat       )),		!Xb concat Ya
	(kappend,		$,	(0,0,0,0),	cast(k_append       )),		!Xb append Ya

	(kpower,		$,	(0,0,0,0),	cast(k_power        )),		!Xb power Ya
	(katan2,		$,	(0,0,0,0),	cast(k_atan2        )),		!Xb atan2 Ya

	(kaddto,		$,	(0,0,0,0),	cast(k_addto        )),		!Xb^+:=Y or Xa^+:=A or A+:=B
	(ksubto,		$,	(0,0,0,0),	cast(k_subto        )),		!Xb^-:=Ya
	(kmulto,		$,	(0,0,0,0),	cast(k_multo        )),		!Xb^*:=Ya
	(kdivto,		$,	(0,0,0,0),	cast(k_divto        )),		!Xb^/:=Ya
	(kidivto,		$,	(0,0,0,0),	cast(k_idivto       )),		!Xb^%:=Ya

	(kandlto,		$,	(0,0,0,0),	cast(k_andlto       )),		!Xb^ and:=Ya
	(korlto,		$,	(0,0,0,0),	cast(k_orlto        )),		!Xb^ or:=Ya
	(kiandto,		$,	(0,0,0,0),	cast(k_iandto       )),		!Xb^ iand:=Ya
	(kiorto,		$,	(0,0,0,0),	cast(k_iorto        )),		!Xb^ ior:=Ya
	(kixorto,		$,	(0,0,0,0),	cast(k_ixorto       )),		!Xb^ ixor:=Ya
	(kshlto,		$,	(0,0,0,0),	cast(k_shlto        )),		!Xb^ shl:=Ya
	(kshrto,		$,	(0,0,0,0),	cast(k_shrto        )),		!Xb^ shr:=Ya
	(kminto,		$,	(0,0,0,0),	cast(k_minto        )),		!Xb^ min:=Ya
	(kmaxto,		$,	(0,0,0,0),	cast(k_maxto        )),		!Xb^ max:=Ya
	(kconcatto,		$,	(0,0,0,0),	cast(k_concatto     )),		!Xb^ concat:=Ya
	(kappendto,		$,	(0,0,0,0),	cast(k_appendto     )),		!Xb^ concat:=Ya

	(kdot,			$,	(g,0,0,0),	cast(k_dot          )),		!T:=Xa.A
	(kindex,		$,	(0,0,0,0),	cast(k_index        )),		!T:=Xb[Ya]
	(kdotindex,		$,	(0,0,0,0),	cast(k_dotindex     )),		!T:=Xb.[Ya]
	(kkeyindex,		$,	(0,0,0,0),	cast(k_keyindex     )),		!T:=Xc{Yb,Za}

	(kdotref,		$,	(g,0,0,0),	cast(k_dotref       )),		!T:=&Xa.A
	(kindexref,		$,	(0,0,0,0),	cast(k_indexref     )),		!T:=&Xb[Ya]
	(kdotindexref,	$,	(0,0,0,0),	cast(k_dotindexref  )),		!T:=&Xb.[Ya]
	(kkeyindexref,	$,	(0,0,0,0),	cast(k_keyindexref  )),		!T:=&Xc{Ya,Za}

	(kpopdot,		$,	(g,0,0,0),	cast(k_popdot       )),		!Ya.A:=Xb
	(kpopindex,		$,	(0,0,0,0),	cast(k_popindex     )),		!Yb[Za]:=Xc
	(kpopdotindex,	$,	(0,0,0,0),	cast(k_popdotindex  )),		!Yb.[Za]:=Xc
	(kpopkeyindex,	$,	(0,0,0,0),	cast(k_popkeyindex  )),		!Yb{Za}:=Xc

	(kexpand,		$,	(i,0,0,0),	cast(k_expand       )),		!Expand Xa when A objects are needed

	(kpushtry,		$,	(l,i,i,0),	cast(k_pushtry      )),		!Push try/except into; label/except code/no. exceptions
	(kraise,		$,	(0,0,0,0),	cast(k_raise        )),		!Raise exception Xa
	(kmaps,			$,	(0,0,0,0),	cast(k_maps         )),		!Xa:=map(Xb,Ya)
	(kmapss,		$,	(0,0,0,0),	cast(k_mapss        )),		!Xa:=map(Xc,Yb,Za)

!Special composite opcodes used in asm optimiser

	(kpushff,		$,	(f,f,0,0),	cast(nil)             ),		!
	(kpushmm,		$,	(m,m,0,0),	cast(nil)             ),		!
	(kpushfm,		$,	(f,m,0,0),	cast(nil)             ),		!
	(kpushmf,		$,	(m,f,0,0),	cast(nil)             ),		!

	(kmoveff,		$,	(f,f,0,0),	cast(nil)             ),		!
	(kzmoveff,		$,	(f,f,0,0),	cast(nil)             ),		!
	(kmovefm,		$,	(f,m,0,0),	cast(nil)             ),		!
	(kmovemf,		$,	(m,f,0,0),	cast(nil)             ),		!
	(kmovemm,		$,	(m,m,0,0),	cast(nil)             ),		!

	(kmovefci,		$,	(f,i,0,0),	cast(nil)             ),		!
	(kzmovefci,		$,	(f,i,0,0),	cast(nil)             ),		!
	(kmovemci,		$,	(m,i,0,0),	cast(nil)             ),		!

	(kpushfff,		$,	(f,f,f,0),	cast(nil)             ),		!
	(knop2,			$,	(i,0,0,0),	cast(nil)             ),		!
	(kpushci0,		$,	(i,0,0,0),	cast(nil)             ),		!
	(kpushvoid2,	$,	(0,0,0,0),	cast(nil)             ),		!
	(kpushvoid3,	$,	(0,0,0,0),	cast(nil)             ),		!

	(kunshare1,		$,	(0,0,0,0),	cast(nil)             ),		!
	(kunshare2,		$,	(0,0,0,0),	cast(nil)             ),		!
	(kunshare3,		$,	(0,0,0,0),	cast(nil)             ),		!
	(kprocentry1,	$,	(0,0,0,0),	cast(nil)             ),		!
	(kprocentry2,	$,	(0,0,0,0),	cast(nil)             ),		!

	(kjumpeqfci,	$,	(l,f,i,0),	cast(nil)             ),		!
	(kjumpnefci,	$,	(l,f,i,0),	cast(nil)             ),		!
	(kjumpltfci,	$,	(l,f,i,0),	cast(nil)             ),		!
	(kjumplefci,	$,	(l,f,i,0),	cast(nil)             ),		!
	(kjumpgefci,	$,	(l,f,i,0),	cast(nil)             ),		!
	(kjumpgtfci,	$,	(l,f,i,0),	cast(nil)             ),		!

	(kjumpeqff,		$,	(l,f,f,0),	cast(nil)             ),		!
	(kjumpneff,		$,	(l,f,f,0),	cast(nil)             ),		!
	(kjumpltff,		$,	(l,f,f,0),	cast(nil)             ),		!
	(kjumpleff,		$,	(l,f,f,0),	cast(nil)             ),		!
	(kjumpgeff,		$,	(l,f,f,0),	cast(nil)             ),		!
	(kjumpgtff,		$,	(l,f,f,0),	cast(nil)             ),		!

	(kaddfci,		$,	(f,i,0,0),	cast(nil)             ),		!
	(ksubfci,		$,	(f,i,0,0),	cast(nil)             ),		!

	(kaddff,		$,	(f,f,0,0),	cast(nil)             ),		!
	(ksubff,		$,	(f,f,0,0),	cast(nil)             ),		!
	(kindexff,		$,	(f,f,0,0),	cast(nil)             ),		!

	(kpushincrptrm,	$,	(m,0,0,0),	cast(nil)             ),		!
	(kpushincrptrf,	$,	(f,0,0,0),	cast(nil)             ),		!
	(kpopincrptrm,	$,	(m,0,0,0),	cast(nil)             ),		!
	(kpopincrptrf,	$,	(f,0,0,0),	cast(nil)             ),		!

	(kswitchf,		$,	(f,i,i,0),	cast(nil)             ),		!
	(klenf,			$,	(f,0,0,0),	cast(nil)             ),		!
	(kpushptrf,		$,	(f,0,0,0),	cast(nil)             ),		!

	(klastpcl,		$,	(0,0,0,0),	cast(nil)             )
end

global [0..klastpcl]ref void cmdmap			!map cmd index to possible fn/label address

global enumdata []u64 symbolnames=
!First half are basic tokens returned by lexreadtoken()
	(errorsym,			'error'),		! Lex error
	(dotsym,			'dot'),		! "."
	(lexdotsym,			'lexdot'),		! ".", used at bol to prefix lexical 
	(commasym,			'comma'),		! ","
	(semisym,			'semi'),		! ";"
	(colonsym,			'colon'),		! ":"
	(dcolonsym,			'dcolon'),		! "::"
	(assignsym,			'assign'),		! :=
	(deepcopysym,		'deepcopy'),		! ::=
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
	(daddrsym,			'daddr'),		! &&
!	(curlsym,			'curl'),		! ~
	(rangesym,			'range'),		! ..
	(ellipsissym,		'ellipsis'),		! ...
	(hashsym,			'hash'),		! #

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

	(minsym,			'min'),		! min
	(maxsym,			'max'),		! max
	(appendsym,			'append'),		! append
	(concatsym,			'concat'),		! concat
	(insym,				'in'),		! in
	(notinsym,			'notin'),		! notin
	(inxsym,			'inx'),		! inx
	(inrevsym,			'inrev'),		! inrevsym
	(powersym,			'power'),		! **

	(eqsym,				'eq'),		! =
	(nesym,				'ne'),		! <>
	(ltsym,				'lt'),		! <
	(lesym,				'le'),		! <=
	(gesym,				'ge'),		! >=
	(gtsym,				'gt'),		! >
	(isequalsym,		'isequal'),		! ==

	(notlsym,			'notl'),		! not
	(inotsym,			'inot'),		! inot
	(istruelsym,		'istruel'),		! istrue
	(abssym,			'abs'),		! abs
	(sqrsym,			'sqr'),		! sqr
	(signsym,			'sign'),		! sign
	(ascsym,			'asc'),		! asc
	(chrsym,			'chr'),		! chr

	(mathssym,			'maths'),		! sin etc
	(maths2sym,			'maths2'),		! atan2 etc
	(propsym,			'prop'),		! len etc

	(incrsym,			'incr'),		! -
	(decrsym,			'decr'),		! -

	(eolsym,			'eol'),		! End of line
	(eofsym,			'eof'),		! Eof seen
	(rawnamesym,		'rawname'),		! unassigned name before lookup
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
	(kcasesym,			'case'),		! CASE
	(kdocasesym,		'docase'),		! DOCASE
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
	(kswitchsym,		'switch'),		! SWITCH
	(kdoswitchsym,		'doswitch'),		! DOSWITCH
	(kprintsym,			'print'),		! PRINT/PRINTLN/FPRINT/FPRINTLN
	(ksprintsym,		'sprint'),		! SPRINT/SFPRINT
	(kreadsym,			'read'),		! READ/READLN
	(ksreadsym,			'sread'),		! SREAD
	(ksreadlnsym,		'sreadln'),		! SREADLN
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
	(kslicesym,			'slice'),		! SLICE
	(kmacrosym,			'macro'),		! MACRO
	(koperatorsym,		'op'),		! OPERATOR
	(kconstsym,			'const'),		! 
	(kfflangsym,		'fflang'),		! JLANG CLANG WINDOWS HOST
	(kglobalsym,		'global'),		! global
	(kstaticsym,		'static'),		! STATIC
	(kcalignedsym,		'calign'),		! $CALIGNED

	(ktrysym,			'try'),		! 
	(kexceptsym,		'except'),		! 
	(kraisesym,			'raise'),		! 
	(kextendsym,		'extend'),		!
	(kblocksym,			'block'),		!
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
	(khostsym,			'host'),		! HOST
	(knilsym,			'nil'),		! NIL/PNIL
	(kstrincludesym,	'strincl'),		! STRINCLUDE
	(specialopsym,		'specop'),		! $NEG, $INDEX etc
	(kdummysym,			'dummy')		!
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
	("docase",		kdocasesym,		jdocase),
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

!	("redo",		questionsym,	0),
!	("redo",		kloopsym,		jredo),
	("redoloop",	kloopsym,		jredo),

!	("next",		questionsym,	0),
!	("next",		kloopsym,		jnext),
	("nextloop",	kloopsym,		jnext),

	("exit",		kloopsym,		jexit),
	("exitloop",	kloopsym,		jexit),
	("goto",		kgotosym,		0),
	("switch",		kswitchsym,		jswitch),
	("doswitch",	kdoswitchsym,	jdoswitch),
	("tabledata",	ktabledatasym,	0),
	("enumdata",	ktabledatasym,	1),
	("clamp",		kclampsym,		0),
	("maps",		kmapsym,		0),
	("mapss",		kmapsym,		0),
	("eval",		kevalsym,		0),

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

	("cast",		kcastsym,		13),

	("proc",		kprocsym,		0),
	("sub",			kprocsym,		1),

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
	("slice",		kslicesym,		0),

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

	("clang",		kfflangsym,		0),
	("windows",		kfflangsym,		0),

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

	("int8",		stdtypesym,		ti8),
	("int16",		stdtypesym,		ti16),
	("int32",		stdtypesym,		ti32),
	("int64",		stdtypesym,		ti64),

	("word8",		stdtypesym,		tu8),
	("word16",		stdtypesym,		tu16),
	("word32",		stdtypesym,		tu32),
	("word64",		stdtypesym,		tu64),

	("real32",		stdtypesym,		tr32),
	("real64",		stdtypesym,		tr64),

	("stringc",		stdtypesym,		tpackstrc),
	("stringz",		stdtypesym,		tpackstrz),
	("cstring",		stdtypesym,		tpackstrz),
	("ichar",		stdtypesym,		tstringz),

	("million",		unitnamesym,	million_unit),
	("billion",		unitnamesym,	billion_unit),
!	("thousand",	unitnamesym,	thousand_unit),
	("as",			unitnamesym,	0),

	("$lineno",		compilervarsym,	jcvlineno),
	("$strlineno",	compilervarsym,	jcvstrlineno),
	("$filename",	compilervarsym,	jcvfilename),
	("$modulename",	compilervarsym,	jcvmodulename),
	("$function",	compilervarsym,	jcvfunction),
	("$date",		compilervarsym,	jcvdate),
	("$time",		compilervarsym,	jcvtime),
!	("$version",	compilervarsym,	J_cvversion),
	("$",			dollarsym,		0),

	("and",			andlsym,		jandl),
	("or",			orlsym,			jorl),
	("iand",		iandsym,		jiand),
	("ior",			iorsym,			jior),
	("ixor",		ixorsym,		jixor),
	("in",			insym,			jin),
	("notin",		notinsym,		jnotin),
	("inx",			inxsym,			jinx),
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
	("log",			mathssym,		jlog),
	("log10",		mathssym,		jlog10),
	("exp",			mathssym,		jexp),
	("round",		mathssym,		jround),
	("floor",		mathssym,		jfloor),
	("ceil",		mathssym,		jceil),
	("fract",		mathssym,		jfract),
	("fmod",		maths2sym,		jfmod),

	("append",		appendsym,		jappend),
	("concat",		concatsym,		jconcat),

	("len",			propsym,		jlen),
	("lwb",			propsym,		jlwb),
	("upb",			propsym,		jupb),
	("bounds",		propsym,		jbounds),
	("bitwidth",	propsym,		jbitwidth),
	("bytes",		propsym,		jbytesize),
	("minvalue",	propsym,		jminvalue),
	("maxvalue",	propsym,		jmaxvalue),
	("basetype",	propsym,		jbasetype),
!	("usertype",	propsym,		jusertype),
	("elemtype",	propsym,		jelemtype),
	("dictitems",	propsym,		jdictitems),
!	("decdigits",	propsym,		jdecdigits),
	("isfound",		propsym,		jisfound),
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
	("isnumber",	propsym,		jisnumber),
	("ismutable",	propsym,		jismutable),
	("odd",			propsym,		jodd),
	("even",		propsym,		jeven),

	("fi",			kendsym,		kifsym),
	("esac",		kendsym,		kcasesym),
	("od",			kendsym,		kdosym),

	("nil",			knilsym,		0),
	("con",			sysconstsym,	con_const),
	("pi",			sysconstsym,	pi_const),
	("true",		sysconstsym,	true_const),
	("false",		sysconstsym,	false_const),

	("$neg",		specialopsym,	'-'),
	("$index",		specialopsym,	'[]'),

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
	shlsym, shrsym, minsym, maxsym,	concatsym, powersym, isequalsym,
	idivremsym,  maths2sym, appendsym, addrsym )

global [0..symbolnames.upb]byte binopset

global []byte D_unaryopset = (
	notlsym, inotsym, abssym, istruelsym, sqrsym, signsym, ascsym, chrsym,
	mathssym)

global [0..symbolnames.upb]byte unaryopset

global []byte D_addopset=(addsym, subsym, iandsym, iorsym, ixorsym,
		concatsym, appendsym, minsym, maxsym, addrsym, daddrsym)

global []byte D_cmpopset=(eqsym, nesym, ltsym, lesym, gesym, gtsym, isequalsym)

global []byte D_mulopset=(mulsym, divsym, idivsym, iremsym, shlsym, shrsym)

global [0..symbolnames.upb]byte addopset
global [0..symbolnames.upb]byte cmpopset
global [0..symbolnames.upb]byte mulopset
global [0..symbolnames.upb]byte exprendset

global []int D_exprstarterset= (lbracksym,lsqsym,ptrsym,addrsym,namesym,
	incrsym,decrsym,intconstsym,decimalconstsym,realconstsym,charconstsym,
	stringconstsym,stdtypesym, kmapsym, lcurlysym,
!	ksprintsym,ksreadsym,ksreadlnsym,knewsym,dollarsym,compilervarsym, kclampsym,
	ksprintsym,ksreadsym,ksreadlnsym,dollarsym,compilervarsym, kclampsym,
	krefsym, kcastsym, ellipsissym,
	knilsym, khostfnsym, kifsym, krecordsym, kstructsym)

global [0:symbolnames.len]byte exprstarterset

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

=== qq_show.m 0 0 30/31 ===
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

const logfile="qq.log"

global proc printunit(ref unitrec p,int level=0,ichar prefix="*",filehandle dev=nil)=		!PRINTUNIT
!p is a tagrec
	ref unitrec q
	symbol d
	int t,flags
	ichar idname
	int64 a
	real32 x32

	if p=nil then
		return
	fi

	currlineno:=p.pos iand 16777215

!CPL "PRINTUNIT:",P,P.TAG,JTAGNAMES[P.TAG],=LEVEL,CURRLINENO


!if p.lineno then
!fi

	print @dev,p,":"
	print @dev,getprefix(level,prefix,p)

	idname:=jtagnames[p.tag]
	if idname^='j' then ++idname fi
	print @dev,idname,,": "


	case p.tag
	when jname then
		d:=p.def
!		print @dev,d.name,namenames[d.nameid],"Module:",p.moduleno
		if d.owner then print @dev,d.owner.name,,"." fi

		print @dev,d.name,namenames[d.nameid],"Module:",p.moduleno
		if d.truename and d.nameid=dllprocid then
			print @dev," ",d.truename
		fi

	when jintconst then
		print @dev,p.value

!	when jenumconst then
!		fprint @dev,"# (#:#)",p.value, strmode(p.mode),getenumname(p.mode, p.value)

	when jrealconst then
		print @dev,p.xvalue

	when jstringconst then
		fprint @dev,"""#""",p.svalue

	when jdecimal then
		print @dev,p.svalue,,"L"

	when jcmpchain then
		for i to 4 do
!CP P.CMPGENOP[I],$
			if p.cmpgenop[i]=0 then exit fi
			print @dev,jtagnames[p.cmpgenop[i]],$
		od

	when joperator then
		print @dev, pclnames[p.pclopcode]

!CPL JTAGNAMES.LEN

!when jmakestrtype then
!	print @dev, ttname[p.strtype]

!when jimport then
!	print @dev, p.def.name

!when jprocdef then
!	print @dev, p.def.name
!	if p.mglobal then print @dev, " (global)" fi
!	if p.isfn then print @dev, " (func)" fi
!
!when jrecorddef then
!	print @dev, p.def.name
!	if p.mglobal then print @dev, " (global)" fi
!
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
!	print @dev,p.svalue,"Len:",p.slength
!
!when ktypeconst then
!	print @dev,typename(p.mode),typename(p.value)
!
!when koperator then
!	print @dev,pclnames[p.opcode]+1
!
!when kconvert,ktypepun then
!	print @dev,convnames[p.opcode]," to:",strmode(p.newmode)
!
!when kmakelist,kmultexpr then
!	print @dev,"Len:",p.length
!
!when kdot then
!	print @dev,"Offset:",p.offset
!
	when jcallhost then
		print @dev,hostfnnames[p.index]+2

!when kindex, kptr then
!
!when kexit,kredo,krestart,knext then
!	print @dev,"#",,p.index
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
		p:=p.nextunit
	od
end

function getprefix(int level,ichar prefix,ref unitrec p)ichar=		!GETPREFIX
!combine any lineno info with indent string, return string to be output at start of a line
	static [1024]char str
	[1024]char indentstr
	[16384]char modestr

	indentstr[1]:=0
	!if level>10 then level:=10 fi
	if level>20 then level:=10 fi

	to level do
		strcat(&.indentstr,"- ")
	od

	strcpy(&.str,getlineinfok())
	strcat(&.str,&.indentstr)
	strcat(&.str,prefix)
	if prefix^ then
		strcat(&.str," ")
	fi

	return &.str
end

function getlineinfok:ichar=			!GETLINEINFO
	static [40]char str

!	sprintf(&.str,"%04d ",currlineno)
	strcpy(str,strint(currlineno,"z4"))
	return &.str
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
	ref strec q

	printstrec(f,p,level)

	q:=p.deflist

	while q<>nil do
		printst(f,q,level+1)
		q:=q.nextdef
	od
end

proc printstrec(filehandle f,symbol p,int level)=
	strec dd
	ref byte q
	strbuffer v
	ref strbuffer d:=&v
	int col,offset,n
	const tabstr="    "
	[256]char str
	ichar s

!CPL "PRINTSTREC",P.NAME,NAMENAMES[P.NAMEID]

!RETURN

	offset:=0
	to level do
		print @f,tabstr
		offset+:=4
		col+:=4
	od

	print @f,padstr(p.name,22-offset,"-")
	print @f, padstr(namenames[p.nameid],12,".")

	col:=40
	dd:=p^


	if dd.isimport then
		print @f,"Imp "
	elsif dd.isglobal then
		print @f,(dd.isglobal|"Glob ","Exp "|"Local ")
	fi

	if dd.mbyref then
		print@f,"byref "
	fi
	if dd.moptional then
		print@f,"opt "
	fi

	if dd.moduleno then
		fprint @f,"Modno:#",dd.moduleno
	fi

	print @f,"=========="

	if dd.owner then
		fprint @str,"(#)",dd.owner.name
		print @f, padstr(&.str,18,"-")
	else
		print @f, padstr("()",18,"-")
	fi


	case dd.nameid
	when fieldid,frameid,paramid,enumid then
		print @f," Ix:",dd.index,," "
		if dd.nameid=fieldid and dd.atfield then
			print @f,"@",dd.atfield.name,$
		fi
		print @f," Offset:",dd.fieldoffset,," "

	when structfieldid then
		print @f," Offset:",dd.fieldoffset,," Ix:",DD.INDEX,$
	when recordid then
		print @f," Nfields:",dd.nfields,," "
	when procid, dllprocid, anonprocid then
		fprint @f," Nparms:# ",dd.nparams,=dd.misfunc

	esac	

	case dd.nameid
	when frameid, staticid,constid,macroid,paramid,dllparamid then
		if dd.code then
			case dd.initcode
			when 3 then s:="::="
			when 2 then s:=":="
			else s:="="
			esac

			print @f, s, strexpr(dd.code).strptr,$
		fi
	esac

	if dd.mode then
		fprint @f,"Mode:#",strmode(dd.mode),dd.mode
	fi
!	fprint @f,"Mode:#",(dd.mode)
!
!	PRINT @F," Moduleno:",P.MODULENO
!
	println @f
	ichar tab:="          "
end

global proc printtypetables(filehandle f)=
	symbol d

CPL "PRINT TYPE TABLES",NUSERXTYPES
	println @f,"PROC TYPE TABLES"
!	for m:=0 to ntypes do
	for m:=0 to ntypes do
!	for m:=tlast+1 to ntypes do
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

	od

	ref userxrec p
INT M
	p:=userxmodelist
!	rescan:=0

!global [0:maxuserxtype]symbol ttnamedefx
!!global [0:maxuserxtype]symbol ttnamedefx2
!global [0:maxuserxtype]int ttxmap
!global [0:maxuserxtype]byte ttxmoduleno
!
	for i:=1 to nuserxtypes do
		println @f, i, -i,ttnamedefx[i].name
	od
end

global proc showsttree=
	filehandle f
	ifile m
	symbol d
	ref genfieldrec g
	ref procrec p

	return unless fshowst

	f:=fopen("ST","w")
	printglobalsymbols(f)
!	printglobalsymbols_full(f)

	println @f
	println @f,"Modules",nmodules
	for i to nmodules do
		m:=modules[i]
		IF M THEN
			println @f,"	",,i,,":",m.name,=m.compiled,=m.pcstart,=m.pcsize
		ELSE
			PRINTLN @F,"MODULE",I,"MISSING"
		FI
	od

!	println @f
!	println @f,"Source Files",nsourcefiles
!	for i to nsourcefiles do
!!		println @f,"	",,i,,":",m.name,=m.startfn,=m.mainfn,=m.ast,=m.pcstart,=m.pcsize,
!		println @f,"	",,i,,":",sourcefilenames[i],=sourcefilesys[i],=sourcefilesupport[i]
!	od
!
	println @f
	println @f,"PROC Global GenField Table",ngenfields
	for i to ngenfields do
		g:=genfieldtable[i]
		if g=nil then nextloop fi
		fprintln @f,"   #) #:",i,g.def.name
		while g do
			d:=g.def
			println @f,"      ",d.name, namenames[d.nameid],d.owner.name
			g:=g.nextdef
		od
	od
	println @f


	println @f,"DLL Table", nlibfiles
	for i to nlibfiles do
		println @f, i,":",libtable[i].name, dllinsttable[i], libtypes[i]:"c"
	od
	println @f

	println @f,"DLL Proc Table", ndllprocs
	for i to ndllprocs do
		d:=dllproctable[i]
		println @f, i,":",d.name, dllproclibindex[i], dllprocaddr[i],(d.mvarparams|"Variadic"|""),
			libtypes[dllproclibindex[i]]:"c",=D.INDEX,=DLLPROCTABLE[D.INDEX],=D
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

global proc showtypes=
	filehandle f
	ref filerec m

	return unless fshowtypes
	return when runcode=run_cc

	f:=fopen("TYPES","w")
	printtypetables(f)

	fclose(f)
end

global proc showpcl(isubprog sp, int pass)=
	filehandle f

	return when runcode=run_cc

	gs_init(pcldest)
	gs_str(pcldest,"PROC ALL PCL pass:")
	gs_strint(pcldest,pass)
	gs_line(pcldest)

	if sp then
		showpcl2(sp, pass)
	else
		for i to nsubprogs do
			showpcl2(subprogs[i], pass)
		od
	fi

!CPL "SHOWPCL",PASS
	f:=fopen((pass|"PCL1","PCL2"|"PCL3"),"w")
	if not f then return fi
	gs_println(pcldest,f)
!CPL "WROTE PCL FILE"
!OS_GETCH()

	fclose(f)
end

global proc showpcl2(isubprog sp, int pass)=

!CPL "SHOWPCL2",SP.NAME

	for i:=sp.firstmodule to sp.lastmodule do
!CPL "WALLPCL",I,MODULES[I].NAME
		writeallpcl(modules[i],pass)
	od
end

global proc showast(isubprog sp, ichar file)=
	filehandle f
	ifile pm
	symbol d
	int k,i

	return when runcode=run_cc

	f:=fopen(file,"w")
	return unless f

	println @f,"PROC",file,,":"


	if sp then
		showast2(f, sp)
	else
		for i to nsubprogs do
			showast2(f, subprogs[i])
		od
	fi

	fclose(f)
end

global proc showast2(filehandle f, isubprog sp)=
	ifile pm
	symbol d, e
	int k,i

	println @f,"Proc Subprog",sp.name,,": ******\n"
	for i:=sp.firstmodule to sp.lastmodule do
		pm:=modules[i]

		println @f,"Module:",pm.name
		printunit(pm.ast, dev:f)
		d:=pm.def.deflist
		while d, d:=d.nextdef do
!			if d.nameid=procid then
			if d.nameid=procid then
!CPL "PROC:",D.NAME
				println @f,"\n---PROC",d.name
				printunit(d.code, dev:f)

				e:=d.deflist
				while e, e:=e.nextdef do
					if e.nameid=anonprocid then
						println @f,"\n---ANONPROC",e.name
CPL "ANON",E.CODE
						printunit(e.code, dev:f)
					fi
				od

			fi
		od
		println @f
	od
end

global proc showlogfile=
	[256]char str
	filehandle logdev

!CPL "SHOWLOG1"
!OS_GETCH()

	if fshowpcl1+fshowpcl2+fshowpcl3+fshowast1+fshowast2+
			fshowst+fshowtypes+fshowmodules+fshowstflat=0 then return fi
!CPL "SHOWLOG2",RUNCODE, RUN_CC
	if runcode=run_cc then
		return
	fi

CPL "PRESS KEY"; OS_GETCH()

!CPL "SHOWLOG2"

	if fshowst then
		showsttree()
	fi
!CPL "SHOWLOG3"

	if fshowstflat then
		showstflat()
	fi
!CPL "SHOWLOG4"

	if fshowtypes then
		showtypes()
	fi

!CPL "SHOWLOG5"
	logdev:=fopen(logfile,"w")

!CPL "SHOWLOG3",=FSHOWMODULES
	if fshowmodules then showmoduleinfo(logdev) fi
!CPL "SHOWLOG6"

	if runcode>=fixup_cc and fshowpcl3 then addtolog("PCL3",logdev) fi
	if runcode>=gencode_cc and foptimise and fshowpcl2 then addtolog("PCL2",logdev) fi
	if runcode>=gencode_cc and fshowpcl1 then addtolog("PCL1",logdev) fi
	if runcode>=names_cc and fshowast2 then addtolog("AST2",logdev) fi
	if runcode>=parse_cc and fshowast1 then addtolog("AST1",logdev) fi
	if fshowst then addtolog("ST",logdev) fi
	if fshowstflat then addtolog("STFLAT",logdev) fi
	if fshowtypes then addtolog("TYPES",logdev) fi
	fclose(logdev)

!	fprint @&.str,"c:/m/scripts/med.bat -w #",logfile
	fprint @&.str,"c:/m/scripts/med.bat #",logfile
!CPL =&.STR
!os_GETCH()

!	os_execwait(&.str,1,nil)
	os_execwait(str,0,nil)

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

global proc showstflat=
	filehandle f
	symbol p

	return unless fshowstflat

	f:=fopen("STFLAT","w")

	println @f,"GLOBAL FLAT SYMBOL TABLE:"

	for i:=0 to hashtable.upb-1 do
		p:=cast(&hashtable[i])
		if p.name then
			case p.symbolcode
			when namesym then
				println @f,i,p,":",p.name,symbolnames[p.symbolcode]:"m",namenames[p.nameid]
				p:=p.nextdupl
				while p do
					int sym:=p.symbolcode
					if sym=0 then sym:=errorsym fi
					println @f,"	",p,p.name,symbolnames[sym]:"m",namenames[p.nameid],
						"(From",(p.owner|p.owner.name|"-"),,")"
					p:=p.nextdupl
				od
			esac
		fi
	od
!
	fclose(f)
end

global proc showmoduleinfo(filehandle dev)=
	ifile pm
	ref subprogrec ps
	static ichar tab="    "

CPL "SMI0"
	println @dev,"Project Structure:"
	println @dev,"---------------------------------------"
	println @dev,"Modules",nmodules
	for i to nmodules do
		pm:=modules[i]
!CPL "SMI",I,PM

!		if i>1 and pm.subprogno<>modules[i-1].subprogno then
!			println @dev
!		fi
!
		print @dev, tab,i:"2",pm.name:"16jl", "Lead:",pm.islead, "Sys:",pm.issyslib, "Path:",pm.path,
			"Sub:",subprogs[pm.subprogno].name,"File:",pm.filespec
!		if pm.stmacro then
!			print @dev," Alias:",pm.stmacro.name
!		fi
!		print @dev, "START:",pm.startfn
!		if i=mainmoduleno then print @dev, "<MAIN>" fi
!	PRINT @DEV,"<TEMP MODULE INFO>"
		println @dev
	od
	println @dev

	println @dev,"Subprograms",nsubprogs
	for i to nsubprogs do
		ps:=subprogs[i]
		println @dev, tab,i,ps.name,"Sys:",ps.issyslib, "Path:",ps.path,
			 "Spec:",ps.filespec,"Comp:",ps.compiled
		if ps.firstmodule then
			print @dev, tab,tab,ps.firstmodule,ps.lastmodule,,": "
			for j:=ps.firstmodule to ps.lastmodule do
				print @dev, modules[j].name,$
			od
			println @dev
		fi
	od
	println @dev
!
!	println @dev,"Sourcefiles",nsourcefiles
!	for i to nsourcefiles do
!		println @dev, tab,i,sourcefilenames[i]
!		if sourcefilepaths[i]^ then println @dev, tab,tab,sourcefilepaths[i] fi
!		println @dev, tab,tab,sourcefilespecs[i]
!		println @dev, tab,tab,=sourcefilesizes[i]
!		println @dev, tab,tab,=sourcefilesys[i]
!		println @dev, tab,tab,=sourcefilesupport[i]
!!		println @dev, tab,tab,=sourcefiledupl[i]
!	od
!	println @dev
!
!!	println @dev,"Header Variables:"
!!	for i to headervars.len do
!!		fprintln @dev,"\t#: #",headervarnames[i],headervars[i]
!	od
!	println @dev
!	println @dev,"---------------------------------------"

	return unless stprogram
	println @dev,"Symboltable:"
	symbol d:=stprogram.deflist
	while d, d:=d.nextdef do
		ichar id
		case d.nameid
		when moduleid then id:="Mod"
		when subprogid then id:="Sub"
		else id:="---"
		esac
		fprintln @dev,"    # # (m#, s#)",d.name,id,d.moduleno, d.subprogno
	od
	println @dev

end

global proc printsymbol(ref lexrec lp)=
	lexrec l
	l:=lp^

!	printf("%-18s",symbolnames[l.symbol])
!	print symbolnames[l.symbol]:"18 jl"
	print symbolnames[l.symbol]:"m 18 jl"

	case l.symbol
	when namesym then
!	print l.symptr.name

		printstr_n(l.symptr.name,l.symptr.namelen)
	when intconstsym then
		case l.subcode
		when tint then print l.value,"int"
!		when tword then print l.uvalue,"word"
		else print l.value
		esac

	when realconstsym then
		print l.xvalue

	when stringconstsym then
		print """",$
		printstr(l.svalue)
		print $,""""
	when charconstsym then
		print "'",$
		printstr(l.svalue)
		print $,"'"
	when decimalconstsym then
		printstr(l.svalue)
		print "L"
	when assignsym,addrsym,ptrsym,deepcopysym,rangesym then
		print jtagnames[l.subcode]
	elsif l.subcode then
		print "#",l.subcode
	end

	println

end

global function strmode(int t, expand=0)ichar=
	static [2048]char str

	istrmode(t,&.str,expand)
	return str
end

proc istrmode(int t, ichar dest,int expand=1)=
	static [2048]char str
	symbol d

	if t<0 then
		strcpy(dest,"*")
		strcat(dest,ttnamedefx[-t].name)
!		if ttnamedefx2[-t] then
!			strcat(dest,".")
!			strcat(dest,ttnamedefx2[-t].name)
!		fi
		return
	fi

!CPL "MM1",T

	if t<tlast then
!CPL "MM2",T,=TTNAME[T]
		strcpy(dest,ttname[t])
		return
	fi

	case ttbasetype[t]
	when trefpack then
		strcpy(dest,"ref ")
		istrmode(tttarget[t], dest+strlen(dest),0)
	when tvector then
		fprint @dest, "[#..#]",ttlower[t],ttlength[t]+ttlower[t]-1
		istrmode(tttarget[t], dest+strlen(dest),0)

!	when tslice then
!		strcpy(dest, "slice[]")
!		istrmode(tttarget[t], dest+strlen(dest),0)

	when tstruct then
!		if not expand then recase else fi
		if not expand then goto $else fi
		strcpy(dest,"struct(")
dostruct:
		d:=ttfields[t]
		while d, d:=d.nextdef do
			istrmode(d.mode, dest+strlen(dest),0)
			strcat(dest, " ")
			strcat(dest, d.name)
			if d.nextdef then
				strcat(dest, ", ")
			fi
		od
		strcat(dest,")")
	when trecord then
!		if not expand then recase else fi
		if not expand then goto $else fi
		strcpy(dest,"record(")
		goto dostruct

!	when tenum then
!!		if not expand then recase else fi
!		if not expand then $else fi
!		strcpy(dest,"enum(")
!		d:=ttfields[t]
!		while d, d:=d.nextdef do
!			if d.nameid=enumid and d.mode=t then
!				strcat(dest, d.name)
!				strcat(dest, " ")
!			fi
!		od
!		strcat(dest,")")

	else
$else:
!CPL "STRMODE BASETYPE"!,STDTYPENAMES[TTBASETYPE[T]]
		strcpy(dest,ttname[t])
	esac
end

proc writepcl(ref int pcstart,pc, ref int32 pclsource, int pass, ichar sourcecode)=
!write pc instruction to ttdeststr, as a single line of pcl
!index is index of ins in pccode/pcdata
	[512]char str
	qd fmt
	int cmdcode,a,needcomma,i,offset,labeldone,commentdone,soffset,moduleno
	ref strec d
	const tabx="!      ----------"

!CPL "WRITEPCL",PC,PC^!,PCLNAMES[PC^]

	cmdcode:=pc^

	memcpy(&fmt,&pclfmt[cmdcode],fmt.bytes)
	labeldone:=commentdone:=0

	case cmdcode
	WHEN KSKIP THEN
		RETURN

	when kprocdef then
!	CPL "--PROCDEF",SYMBOL(PC^).NAME
		currpclproc:=cast((pc+1)^)
		gstr(tabx)
		gstr("Procdef:")
		gstr(currpclproc.name)
		gline()
		return
	when kprocend then
		gstr(tabx)
		gstrln("End")
		return
	esac

	offset:=PC-PCSTART

	soffset:=(pclsource+offset)^
	currlineno:=soffset iand 16777215
!	moduleno:=soffset.[24..31]


	++pc

!	if currlineno then
		fprint @str,"# [#]: #: ",pc-1:"8zh", currlineno:"05jr", pc-pcstart-1:"4"
!		fprint @str,"# #M #L: #: ",pc-1:"8zh", moduleno:"2",currlineno:"5", pc-pcstart-1:"4"
!	else
!		fprint @str,"# #: ",pc-1:"8zh",pc-pcstart-1:"4"
!	fi

	gstr(&.str)

	offset:=pc-pcstart-1


	if labelmap^[offset] then
!CPL "WRITEPCL LABEL",=OFFSET
		glabeldef(offset)
		gstr(&.str)
	fi

	case cmdcode
	when kprocdef then
		currpclproc:=cast(pc^)
		return
	when kcomment then
		gstr("! ")
		gstrln(cast(pc^))
		return
	when klabeldef then
		gstr(ref strec(pc^).name)
		GSTR(" /")
		GSTR(NAMENAMES[REF STREC(PC^).NAMEID])
		++PC

		gstrln(":")
		return
	esac

	strcpy(&.str,pclnames[cmdcode]+1)

	a:=1
	gs_leftstr(pcldest," ",7,'-')
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
	static [512]char str,str2
	symbol d
	ichar suffix,s
	int slen
	object p
	ref stringrec ps

!IF PASS=3 THEN
!	RETURN "OPND"
!FI

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
			strcat(str,hostfnnames[x]+2)
		fi

	when creal then
		strcpy(str,strreal(real@(x)))

	when crange then
		fprint @str,"#..#",x iand 0xFFFF'FFFF,x>>32

	when cstring then
		if pass<=2 then
!			s:=cast(x)
!			slen:=strlen(s)
!			goto dostring
			ps:=cast(x)
			s:=ps.svalue
			slen:=ps.length
			goto dostring
		else
!RETURN "<STR>"
			p:=cast(x)
			if (slen:=p.length)=0 then return """" fi
			s:=p.strptr
			goto dostring
		fi

	when cstringz then

		s:=cast(x)
		slen:=strlen(s)
	dostring:
		if slen>=255 then slen:=255 fi
		memcpy(&.str,s,slen)			!truncate too-long strings
		str[slen+1]:=0
		convertstring(&.str,&.str2)
		fprint @str,"""#""",&.str2

	when cmemory then
		if pass<=2 then
			strcpy(str,d.name)
		else
!RETURN "MEM"
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
		if pass<=2 then
			strcpy(str,d.name)
		else
!RETURN "FRAME"
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
		if pass<=2 then
			strcpy(str,d.name)
		else
!RETURN "PROC"
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
!		fprint @str,"[DLL:#]",getdottedname(d)
		fprint @str,"[DLL:#]",d.name

	when cgenfield then
		if pass<=2 then
			d:=symbol(x)
			fprint @str,".#",d.name
		else
!RETURN "GENFIELD"
			fprint @str,"## (#)","#",x, genfieldtable[x].def.name
		fi
!
	when ctype then
		fprint @str,"T:# <#>",strmode(x),int(x)

	when clabel then
		if pass<=2 then
			fprint @str,"L#",x
		else
!RETURN "LABEL"
			fprint @str,"&# (L#)",x:"h",ref int(x)-pcstart
		fi

	when coperator then
		fprint @str,"(#)",pclnames[x]

	else
	other:
		fprint @str,"<# #>",fmt,opndnames[fmt]
	esac
	return str
end

global proc writeallpcl(ifile pm, int pass)=
!display code currently in pccode/pcopnd
	int cmd,i,lastline,line,labno,offset,index,size,nopnds,x,y
	ref int pc,pclcode
	ref int32 pclsource
	ichar sourcecode
	ichar name

	currlineno:=0

!if n=1 then
!	gs_init(pcldest)
!fi

!CPL "WRITEALLPCL",PM.NAME, PM.PCSTART

	if pass=3 and not hasbytecodes then
		gstrln("Can't show PCL; use -debug")
		return
	fi

	gstr("PCL FOR MODULE:")
	gstrln(pm.name)

	pc:=pclcode:=pm.pcstart
	pclsource:=pm.pcsrcstart
	sourcecode:=pm.text

	size:=pm.pcsize
	labelmap:=pcm_allocz((size+1)*int.bytes)

!CPL "ALLPCL",PC,PC^
	repeat
		cmd:=pc^
!CPL "A1",PC,pclnames[CMD]
		nopnds:=pclnopnds[cmd]

		for i to nopnds do
			case pclfmt[cmd,i]
			when cnone then
				exit
			when clabel then
				x:=(pc+i)^
!CPL "CLABEL",X,LABELMAP,=SIZE,=PASS
				if pass=3 then
					x:=ref int(x)-pclcode
				fi
!CPL "	CLABEL",X,LABELMAP,=SIZE,=PASS
IF INT(X)>100000 THEN
!	CPL "X LABEL ALREADY CONVERTED"
ELSE

				labelmap^[x]:=1		!allow up to 4 labels at this loc
FI
			esac

		od
		pc+:=pclnopnds[cmd]+1
	until cmd in [kzero,kendmodule]

	pc:=pm.pcstart

!CPL "ALLPCL2",PC
	repeat
		cmd:=pc^

!CPL PC,=PCLNAMES[CMD],=PASS

		writepcl(pclcode,pc,pclsource, pass, sourcecode)
!CPL "DONEWRITEPCL"
		pc+:=pclnopnds[cmd]+1
	until cmd in [kzero,kendmodule]
!CPL "DONE2"

	gline()
	pcm_free(labelmap,(size+1)*int.bytes)



!CPL "DONE3"
end

global proc deletetempfiles=
	remove("PCL1")
	remove("PCL2")
	remove("PCL3")
	remove("AST1")
	remove("AST2")
	remove("TYPES")
	remove("STFLAT")
	remove("ST")
	remove(logfile)
end
=== qq_vars.m 0 0 31/31 ===
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
	when tint, trefvar, trefpack, ttype, tsymbol then
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
global proc var_inplace(variant px,y, ref proc(variant,variant) fnadd, fnaddmixed=nil)=
	varrec x
	varrec z

	var_loadptr(px,&x)
!z:=x

	if x.tag=y.tag then
		fnadd^(&x,y)
	elsif fnaddmixed then
		fnaddmixed^(&x,y)
	else
!		if u64(fnadd)=u64(var_add) and x.tag=tstring and y.tag=tint then
!			var_addto_string_ch(&x,y.value)
!		else
!
!			pcerror("Inplace mixed")
			pcmxtypes("Inplace mixed",&x,y)
!		fi
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
			var_make_dec_int(sptr.value,dest)
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

=== END ===
1 qq.m
2 qq_cli.m
3 qq_api.m
4 qq_arrays.m
5 qq_bits.m
6 qq_calldll.m
7 qq_decimal.m
8 qq_decls.m
9 qq_dicts.m
10 qq_jhandlers.m
11 qq_khandlers.m
12 qq_host.m
13 qq_lex.m
14 qq_lib.m
15 qq_lists.m
16 qq_modules.m
17 qq_names.m
18 qq_optim.m
19 qq_packed.m
20 qq_parse.m
21 qq_print.m
22 qq_pclgen.m
23 qq_pcllib.m
24 qq_records.m
25 qq_resolve.m
26 qq_sets.m
27 qq_strings.m
28 qq_syslibsdummy.m
29 qq_tables.m
30 qq_show.m
31 qq_vars.m
