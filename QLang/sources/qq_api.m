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
