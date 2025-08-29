=== MA 40 ===
=== mm.m 0 0 1/40 ===
!project =
	module mm_cli

	module mm_gentcl
	module mm_libtcl
	module mm_blocktcl

	module mm_decls

	module mm_diags
!	module mm_diags_dummy
!
	module mm_export_dummy
!	module mm_exportq
!	module mm_exportm

	module mm_lex
	module mm_lib

!	module mm_libsources
	module mm_libsources_dummy
!
	module mm_modules
	module mm_name
	module mm_parse

	module mm_support
	module mm_tables
	module mm_type

	import tcl

!	module tc_api
!	module tc_decls
!	module tc_diags
!	module tc_tables
!
!	module mc_decls_x		!x64-generating API
!	module mc_lib_x
!	module mc_asm_x
!
!!	module mc_gen_x			!tcl -> mcl/x64
!!	module mc_aux_x
!!	module mc_conv_x
!!	module mc_temp_x
!
!	module mc_gen_xb			!tcl -> mcl/x64
!	module mc_aux_xb
!	module mc_conv_xb
!	module mc_temp_xb
!
!	module mc_objdecls
!	module mc_genss
!	module mc_writeexe
!
!	module mc_writess
!	module mc_disasm
!
!!	module mx_run_dummy
!	module mx_decls
!	module mx_run
!	module mx_lib
!	module mx_write
!	module mx_show
!




!global type int8	= i8
!global type int16	= i16
!global type int32	= i32
!global type int64	= i64
!
!global type word8	= u8
!global type word16	= u16
!global type word32	= u32
!global type word64	= u64
!
!global type char64	= c64
!
!global type real32	= r32
!global type real64	= r64


proc main=
	main2()
end

=== tcl.m 0 0 2/40 ===
	MODULE TC_API
	MODULE TC_DECLS
	MODULE TC_DIAGS
	MODULE TC_TABLES

	module mc_decls_x		!x64-generating API
	module mc_lib_x
	module mc_asm_x

!	module mc_gen_x			!tcl -> mcl/x64
!	module mc_aux_x
!	module mc_conv_x
!	module mc_temp_x

	module mc_gen_xb			!tcl -> mcl/x64
	module mc_aux_xb
	module mc_conv_xb
	module mc_temp_xb

	module mc_objdecls
	module mc_genss
	module mc_writeexe

	module mc_writess
	module mc_disasm

!	module mx_run_dummy
	module mx_decls
	module mx_run
	module mx_lib
	module mx_write
	module mx_show

=== tc_api.m 0 0 3/40 ===
int STSEQNO

const freduce=0				!addpx etc
!const freduce=1

export tcl tcstart			!start of tcl block
export tcl tccurr			!point to current tcl op
export tcl pcend			!point to last allocated tclrec
global int pcalloc			!number of tclrecs allocated
byte pcfixed				!whether code is fixed up
global int pcseqno
int pcneedfntable			!whether kgetnprocs etc are used

const pcelemsize = tclrec.bytes
global const tclbasesize = tclrec.bytes - 3*tclopnd.bytes	!base size excludes a,b,c fields

export int mlabelno
export byte phighmem
export byte pfullsys
global byte fpshortnames

!THESE DON'T WORK, AS THEY CAN'T BE SHARED. Eg. may need different types
! or need .isvariadic
!const maxsmallint=64
![0..maxsmallint]tclopnd smallintoperands

global ichar longstring					!used in stropnd
global int longstringlen

export int pstartclock
export int mcltime
export int sstime
export int exetime

global ref func (int pos, ichar &filename, &sourceline)int igetmsourceinfo

export byte fregoptim = 1
export byte fpeephole
export byte tc_useruntcl=0
!global byte pfullsys
export byte pverbose

export int mmpos

[maxfixedtemp]tempmoderec tempmodes
int nredtemps


!---------------------------------------------------

proc start=
!CPL =PSTREC.BYTES
!CPL =TCLREC.BYTES
!CPL =opndREC.BYTES
!CPL =mclREC.BYTES
!CPL =tclbasesize
!CPL =TCLNAMES.LEN
!CPL =regset.len
!CPL =workregs.len
!CPL =REGNAMES.LEN
!CPL =v31
!CPL =rfirst, =rlast

!	for i:=0 to maxsmallint do
!		smallintoperands[i]:=tc_genint0(i)
!	od
end

export proc tcl_start =
!reset tcstart/tccurr for new TCL sequence (new proc or new init data)
	tcstart:=pcm_allocnfz(tclbasesize)
	tcstart.opcode:=knop
	tccurr:=tcstart
	ntemps:=0				!keep track of max used in proc

end

export func tcl_end:tcl pc=
!Terminate sequence; sets tcstart to nil so that tcl cannot be generated
!outside proc bodies etc
!But caller should copy tcstart value, or use its value returned here

	pc:=tcstart
	if pc.opcode=knop then
		pc.next
	else
		pc
	fi
end

export func tcl_writetcl(ichar filename=nil)ichar=
	ref strbuffer d

	d:=writealltcl("caption")

	if filename then
		if pverbose then println "Writing TCL",filename fi
		writefile(filename, d.strptr, d.length)
		""
	else
		d.strptr
	fi
end

export func tcl_writepst(ichar filename=nil)ichar=
	ref strbuffer d

	d:=writepst()

	if filename then
		if pverbose then println "Writing PST",filename fi
		writefile(filename, d.strptr, d.length)
		""
	else
		d.strptr
	fi
end

global proc perror(ichar mess)=
	perror_s(mess, nil)
end

global proc perror_s(ichar mess, param=nil)=
	print "TCL error:",mess
	if param then
		print ":",param
	fi

	stop 1
end

global proc tclerror(ichar mess)=
	println "TCL Error:", mess
	println
	stop 1
end

export func tc_makesymbol(ichar s, int id)psymbol d=
!Create a new st entry
!local/param/null-id names are not linked to psymbol table
!all others become part of main ST
!Only local/param have .owner set to currfunc

	d:=pcm_allocnfz(pstrec.bytes)
	d.name:=pcm_copyheapstring(s)
	d.seqno:=++stseqno

	case id
	when import_id then
		d.imported:=1
	when export_id then
		d.exported:=1
		id:=proc_id
	esac

	d.id:=id
!
!	if id in [local_id, param_id] then
!!*!		d.owner:=currfunc
!	elsif id then
!		tc_addsymbol(d)
!	fi
!
	d
end

export proc tc_addproc(psymbol d)=
	if pproctable=nil then
		pproctable:=pproctablex:=d
	else
		pproctablex.next:=d
		pproctablex:=d
	fi
end

export proc tc_addparam(psymbol d)=
	psymbol p

	tclerror("No proc") unless currfunc

!CPL "ADDPARAM", D.NAME

	p:=currfunc.nextparam

	if p=nil then
		currfunc.nextparam:=d
	else
		while p.nextparam do p:=p.nextparam od		!look for last
		p.nextparam:=d
	fi
	++currfunc.nparams
end

export proc tc_addlocal(psymbol d)=
	psymbol p

	tclerror("No proc") unless currfunc

	p:=currfunc.nextlocal

	if p=nil then
		currfunc.nextlocal:=d
	else
		while p.nextlocal do p:=p.nextlocal od		!look for last
		p.nextlocal:=d
	fi
	++currfunc.nlocals
end

export proc tc_addstatic(psymbol d)=
!add to global static if outside a function, or to current function

	psymbol p

!CPL "ADD STATIC", D.NAME

	if currfunc=nil then
		if pstatictable=nil then
			pstatictable:=pstatictablex:=d
		else
			pstatictablex.next:=d
			pstatictablex:=d
		fi
	else

		p:=currfunc.nextstatic

		if p=nil then
			currfunc.nextstatic:=d
		else
			while p.nextstatic do p:=p.nextstatic od		!look for last
			p.nextstatic:=d
		fi
!		++currfunc.nstatics
	fi
end

global func newtcl(int opcode, nopnds)tcl p=

	p:=pcm_allocnfz(nopnds*tclopnd.bytes+tclbasesize)

	tccurr.next:=p
	tccurr:=p

	tccurr.opcode:=opcode
	tccurr.nopnds:=nopnds
	tccurr.pos:=mmpos
	tccurr.seqno:=++pcseqno
	tccurr.ndest:=tclwrite[opcode]			!move/call are set manually

	return tccurr
end

global func newopnd:tclopnd=
	pcm_allocnfz(opndrec.bytes)
end

export proc tc_gen(int opcode, tclopnd a=nil, b=nil, c=nil)=
!general purpose, up to 3 operands.
!Any temp dest must be allocated by caller

	tcl p
	int n:=tclnopnds[opcode]

	p:=newtcl(opcode, n)

	if n then
		p.a:=a
		if n>=2 then
			p.b:=b
			if n=3 then
				p.c:=c
			fi
		fi
	fi

	if opcode=kmove and a.optype=temp_opnd then
		p.ndest:=1
	fi
end

export proc tc_gen4(int opcode, tclopnd a,b,c,d) =
	tcl p

	p:=newtcl(opcode, 4)

	p.a:=a
	p.b:=b
	p.c:=c
	p.abc[4]:=d
end

export func tc_gent(int opcode, tclopnd b, c=nil)tclopnd=
!any op that returns a single temp. One or two rvalue operands can be provided

	tcl p
	int n:=tclnopnds[opcode]

	if b then
		n:=2
		if c then
			n:=3
		fi
	fi

	p:=newtcl(opcode, n)
	p.a:=tc_gentemp()
	if n>=2 then
		p.b:=b
		if n=3 then
			p.c:=c
		fi
	fi

	if opcode=kmove then
		p.ndest:=1
	fi
	return p.a
end

export func tc_gent4(int opcode, tclopnd b,c,d)tclopnd =
!4-opnd instrs only occur with single temp result
	tcl p

	p:=newtcl(opcode, 4)

	p.a:=tc_gentemp()
	p.b:=b
	p.c:=c
	p.abc[4]:=d

	p.a
end

export func tc_gen_ix(int opcode, tclopnd b, c=nil, int scale=1, offset=0)tclopnd tx =
!for iloadx/addpx only
	
	IF B=NIL THEN B:=TC_GENINT(0) FI
	IF C=NIL THEN C:=TC_GENINT(0) FI
	tx:=tc_gent(opcode, b,c)

	tccurr.scale:=scale
	tccurr.extra:=offset
	tx
end

export proc tc_gen_ixs(int opcode, tclopnd a, b, c=nil, int scale=1, offset=0) =
!for istorex only

!	IF A=NIL THEN A:=GENINT(0) FI
	IF B=NIL THEN B:=TC_GENINT(0) FI
	IF C=NIL THEN C:=TC_GENINT(0) FI
!INT OLDOP:=PCCURR.OPCODE
	tc_gen(opcode, a,b,c)

	tccurr.scale:=scale
	tccurr.extra:=offset
end

export proc tc_gen_call(tclopnd fn, int nret, nargs)=
!nret is number of ret opnds in extretopnds
!nargs is the number of args opnds in extparamopnds
	tcl p
	tclopnd x
	int argoffset

	p:=newtcl(kcall, nret+nargs+1)

	p.abc[nret+1]:=fn
	p.abc[nret+1].opmode:=tpu64
	p.nret:=nret
	p.nargs:=nargs
	p.argoffset:=argoffset:=nret+1

	for i to nret do
		p.abc[i]:=x:=extretopnds[i]			!offset past .a
	od

	for i to nargs do
		p.abc[i+argoffset]:=x:=extparamopnds[i]
	od
end

export proc tc_gen_cond(int opcode, cond, tclopnd a, b, c)=
	tc_gen(opcode, a, b, c)
	tccurr.cond:=cond
end

export func tc_genint(int a)tclopnd p=
!	if a in 0..maxsmallint then
!		return smallintoperands[a]
!	fi
	p:=newopnd()
	p.value:=a
	p.optype:=int_opnd
	return p
end

global func tc_genint0(int a)tclopnd p=
	p:=newopnd()
	p.value:=a
	p.optype:=int_opnd
	return p
end

export func tc_genreal(real x)tclopnd p=
	p:=newopnd()
	p.xvalue:=x
	p.optype:=real_opnd
	return p
end

export func tc_genr32(real x)tclopnd p=
	p:=newopnd()
	p.xvalue32:=x
	p.optype:=r32_opnd
	return p
end

export func tc_genstring(ichar s)tclopnd p=
	p:=newopnd()
	p.svalue:=pcm_copyheapstring(s)
!	p.svalue:=s
	p.optype:=string_opnd
	return p
end

export function tc_gendata(ref byte s, int length)tclopnd p=
	static [1..8]byte types=(tpu8, tpu16, tpblock, tpu32, tpblock, tpblock, tpblock, tpu64)
	p:=newopnd()
	p.svalue:=s			! assume already saved on heap
	p.optype:=data_opnd
!	p.opmode:=tpblock
!
!IF LENGTH IN 1..8 THEN
!	P.OPMODE:=TYPES[LENGTH]
!FI
!
!	p.opsize:=length
	return p
end

export func tc_genlabel(int labelno)tclopnd p=
	p:=newopnd()
	p.labelno:=labelno
	p.optype:=label_opnd
	return p
end

export func tc_genmem(psymbol d)tclopnd p=
	p:=newopnd()
	p.def:=d
	p.optype:=mem_opnd
	return p
end

export func tc_genmemaddr(psymbol d)tclopnd p=
	p:=newopnd()
	p.def:=d
	p.optype:=memaddr_opnd
	return p
end

export func tc_genname(ichar s)tclopnd p=
	return tc_genmem(tc_makesymbol(s, misc_id))
end

export func tc_gennameaddr(ichar s)tclopnd p=
	return tc_genmemaddr(tc_makesymbol(s, misc_id))
end

global func getfullname(psymbol d, int backtick=0)ichar=
!create fully qualified name into caller's dest buffer
	static [256]char str
	int n:=0
	psymbol e:=d

!	if fpshortnames then return d.name fi

	str[1]:=0
	if backtick then
		strcpy(str, "`")
	fi

	if d.imported then
		if backtick then
			strcat(str, d.name)
			strcat(str, "*")
		else
			strcat(str, d.name)
		fi
		return str
	fi


	if d.id in [local_id, param_id] then
IF CURRFUNC=NIL THEN
ABORTPROGRAM("CURRFUNC=0\N\N")
FI
		strcat(str, currfunc.name)
		strcat(str, ".")
		strcat(str, d.name)
		return str
	fi

	if backtick then
		strcat(str, d.name)
	else
		return d.name
	fi
end

export func strpmode(int mode, size=0)ichar=
	static [32]char str

	strcpy(str, "")

	case mode
	when tpblock then
		strcpy(str, "mem:")
		strcat(str, strint(size))
		str
	when tpvoid then
		"---"
	else
		pstdnames[mode]
	esac
end

export proc tc_setmode(int m, size=0)=
	tccurr.mode:=m

	if size then
		tccurr.size:=size
	else
		tccurr.size:=psize[tccurr.mode]
	fi

	if tclhastype[tccurr.opcode]=2 then
		tccurr.mode2:=tccurr.mode
	fi
end

export proc tc_setmode2(int m)=
	tccurr.mode2:=m
end

export proc tc_setimport(psymbol d)=
!allow the use of tc_addlocal
!use d=nil when done

	currfunc:=d
	return unless d

!CPL "SETIMPORT", D.NAME

	if pimporttable=nil then
		pimporttable:=pimporttablex:=d
	else
		pimporttablex.next:=d
		pimporttablex:=d
	fi
end

export proc tc_comment(ichar s)=
!	return when fregoptim or fpeephole		!will get skipped anyway

	tc_gen(kcomment, tc_genstring(s))
end

export proc tc_currfunc(psymbol d)=
	currfunc:=d
end

export proc tc_addplib(ichar name)=
	if nplibfiles>=maxplibfile then perror("Too many libs") fi

!CPL "ADDPLIB",NAME

!	plibfiles[++nplibfiles]:=pcm_copyheapstring(name)
	plibfiles[++nplibfiles]:=pcm_copyheapstring(changeext(name,""))
end

export func tc_makeind(tclopnd a, int m, size=8)tclopnd p=
	tcl pold

	p:=newopnd()
	if a=nil then perror("MAKEIND A=0") fi
	p^:=a^
!TC_COMMENT(ADDSTR("MAKEIND1:", OPNDNAMES[P.OPTYPE]))
	case p.optype

	when memaddr_opnd then
		p.optype:=mem_opnd

	when temp_opnd, mem_opnd then

!	else							!everything else needs explicit loadptr to a new temp
!									!note will not detect invalid ops like floats or strings

!IF P.OPTYPE=TEMPPTR_OPND THEN
!	CPL "MAKEIND/TEMPPTR"
!FI
		pold:=tccurr
		p:=tc_gen_ix(kiloadx, a, tc_genint(0))
		checkaddpx(pold)

!		tc_gen2(kiload, p:=tc_gentemp(tpu64),a)

		tc_setmode(m, size)

	else
		perror("makeind?")
	esac

	return p
end

export func tc_makeindlv(tclopnd a, int m, size=8)tclopnd p=
	p:=newopnd()
	if a=nil then perror("MAKEIND A=0") fi

	p^:=a^

	case p.optype
	when memaddr_opnd then
		p.optype:=mem_opnd
!	when mem_opnd then
!		p.optype:=memptr_opnd

	when temp_opnd then
!		p.optype:=tempptr_opnd

!	when tempptr_opnd, memptr_opnd then
!	when memptr_opnd then
	when mem_opnd then

		tc_gen(kmove, p:=tc_gentemp(),a)
		tc_setmode(tpu64)
		p:=tc_makeindlv(p, m)

		return p

	else
		perror("makeindlv?")

	esac

	return p
end

export func tc_gentemp:tclopnd p=
	int n
	p:=newopnd()

!CPL "GENTEMP", STRMODE(M)

	n:=++ntemps

	p.tempno:=n
	p.optype:=temp_opnd

	return p
end

export func convertstring(ichar s, t, int length)int=
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
!length is that of s; final length may be up to 4 times as long

!returns actual length of t
	int c
	ichar t0:=t
	[16]char str

	to length do
		case c:=s++^
		when '"' then
			t++^:='\\'
			t++^:='"'
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
!		when 7,8,26,27 then
!			t++^:='<'
!			t++^:=c/10+'0'
!			t++^:=(c rem 10)+'0'
!			t++^:='>'
		elsif c in 32..126 then
			t++^:=c
		else
!			t++^:='\\'			!hex
!			t++^:='x'
!			print @str,c:"z2h"
!			t++^:=str[1]
!			t++^:=str[2]

			t++^:='\\'			!octal
!			t++^:='x'
			print @str,c:"z3x8"
			t++^:=str[1]
			t++^:=str[2]
			t++^:=str[3]
		esac
	od

	t^:=0

	return t-t0
end

EXPORT proc merror(ichar mess,ichar param="")=
	int lineno
	ichar filename, sourceline

	if igetmsourceinfo then
		lineno:=igetmsourceinfo(mmpos, filename, sourceline)
		CPL =LINENO
		CPL =FILENAME
	else
CPL "NO LINE INFO"
		lineno:=0
		filename:="?"
	fi

	if currfunc then
		println "Proc:", currfunc.name
	fi

	fprintln "MCL Error: # (#) on Line: # in #, TCL:#",mess,param, lineno, filename, pcseqno
OS_GETCH()
	pcerrorstop(filename, lineno)
end

global proc pcerrorstop(ichar filename,int lineno)=
	filehandle f
	f:=fopen("$error.tmp","w")
	println @f,filename,lineno
	println
	fclose(f)
	stop 1
end

export func getbasename(ichar s)ichar t=
	t:=s+strlen(s)-1
	while t>s and (t-1)^<>'.' do
		--t
	od

	return t
end

export proc tcl_genmcl=
	genmcl()
end

export proc tcl_setflags(int highmem=-1, verbose=-1, shortnames=-1) =

	if highmem>=0 then phighmem:=highmem fi

	if verbose>=0 then pverbose:=verbose fi
	if shortnames>=0 then fpshortnames:=shortnames fi
end

export func addstr(ichar s, t)ichar=
static [256]char str
	strcpy(str, s)
	strcat(str, t)
	str
end

export func tcl_writeasm(ichar filename=nil, int atype='AA')ichar=
	ref strbuffer asmstr
	filehandle f

!CPL "WRITEASM"

!	if assemtype<>atype then
!		tclerror("Wrong ASM Module")
!	fi

!	if assemtype='NASM' then
!		phighmem:=2
!	fi

!CPL "WASM:",$LINENO
	tcl_genmcl()

!CPL "WASM:",$LINENO

	asmstr:=getassemstr()
!CPL "WASM:",$LINENO

	if filename then
!CPL "WRITEASM/file"
		if pverbose then println "Writing ASM", filename fi
!		println "Writing ASM", filename

!CPL "WASM:",$LINENO
		f:=fopen(filename,"w")
!CPL "WASM:",$LINENO
		gs_println(asmstr, f)
!CPL "WASM:",$LINENO
		fclose(f)

		gs_free(asmstr)
		nil
	else
!CPL "WRITEASM/STR"
		asmstr.strptr
	fi
end

export proc tcl_writedll(ichar filename)=
	phighmem:=2
	genmcl()
	genss()
	int tt:=os_clock()
	writeexe(filename, 1)
	exetime:=os_clock()-tt
end

export proc tcl_writeexe(ichar filename)=

!CPL "WX",$LINENO
	genmcl()
!CPL "WX",$LINENO

	genss()
!CPL "WX",$LINENO
	int tt:=os_clock()
!CPL "WX",$LINENO
	writeexe(filename, 0)
!CPL "WX",$LINENO
	exetime:=os_clock()-tt
end

export proc tcl_genss(int obj=0)=
	genmcl()
	genss(obj)
end

export func tcl_writess(ichar filename=nil, int obj=0)ichar =
	ref strbuffer ssstr
	filehandle f

	genmcl()
	genss(obj)

	ssstr:=writessdata(not obj)

	if filename then
		f:=fopen(filename,"w")
		gs_println(ssstr, f)
		fclose(f)

		gs_free(ssstr)
		nil
	else
		ssstr.strptr
	fi
end

export proc tcl_runtcl=
end

export proc tcl_writeobj(ichar filename)= end

!global proc tcl_genss= end
!global proc tcl_runtcl= end
!export proc tcl_writeobj(ichar filename)= end
!export proc tcl_writeexe(ichar filename)= end
!export proc tcl_writedll(ichar filename)= end
export proc tcl_writemx(ichar filename)= end
export proc tcl_writemcl= end
!global proc tcl_exec= end
export proc tcl_cmdskip(int a)=end

export proc tcl_exec(int run=1)=
!	pcmdskip:=cmdskip
	genmcl()
	genss()
	runlibfile("dummy", pcmdskip, run)
end

export proc checkaddpx(tcl p, int id=0)=
!addpx/loadpx has just been generated; see if it can be combined with previous addpx
!assume it looks like this:
! p  T1 := bp  + cp*sp + extrap        (p)
! q  T2 := T1 +  cq*sq + extraq        (q:=tccurr)
!For this to work, p must be addpx, q is addpx/loadpx, and cq must be int-opnd or
! be missing; T1 must only be used in these 2 ops

	tcl q:=tccurr

!CPL "CHECK1",TCLNAMES[P.OPCODE], =ID
	return unless freduce

	return unless p.opcode=kaddpx
	return unless q.c=nil or q.c.optype=int_opnd
	return unless p.a=q.b
	return unless q.islast.[2] = 1

	p.a:=q.a							!move T2 over to P

	if q.c then							!I think that .c is optional
		p.extra +:= q.c.value*q.scale
	fi
	p.extra +:= q.extra
	p.opcode := q.opcode				!move opcode in case loadpx

	tccurr:=p							!discard new tcl op
end

export proc checkaddpx_store(tcl p, int id=0)=
!storepx has just been generated; see if it can be combined with previous addpx
!assume it looks like this:
! p  T1 := bp  + cp*sp + extrap        (tccurr-1)
! q  (T1 +  bq*sq + extraq)^ := c       (tcurr)

!For this to work, p must be addpx, q is storepx, and bq must be int-opnd or
! be missing; T1 must only be used in these 2 ops

	return unless freduce

	tcl q:=tccurr

	return unless p.opcode=kaddpx
	return unless q.b=nil or q.b.optype=int_opnd
	return unless p.a=q.a
	return unless q.islast.[1]=1

	if q.b then							!I think that .c is optional
		p.extra +:= q.b.value*q.scale
	fi
	p.extra +:= q.extra
	p.opcode := kistorex				!addpx becomes storepx

	moveopnd(p, p, 1, 2)
	moveopnd(p, p, 2, 3)
	moveopnd(p, q, 3, 3)

	--tccurr							!discard new tcl op
end

proc moveopnd(tcl p, q, int a, b)=
	if q.abc[b]=nil then
		p.abc[a]:=tc_genint(0)
		p.islast.[a]:=0
		return
	fi

	p.abc[a]:=q.abc[b]

	if q.islast.[b] then
		p.islast.[a]:=1
	else
		p.islast.[a]:=0
	fi
	q.islast.[b]:=0
	dummy()
end

global proc dummy()=
end

export proc scanproctemps(psymbol d)=
!build templist table for proc d, and fixup ltmode/islast fields in tcl code
	tcl p:=d.code, plast
	int ndest, temp
	ref temprec pt
	tclopnd a
	int ntempts

	ntemps:=d.maxtemp

	if ntemps<=maxfixedtemp then
		templist:=&fixedtemplist
		for i to ntemps do
			memset(templist, 0, ntemps*temprec.bytes)
		od

	else
!		gerror("Can't do that many temps right now")
		templist:=pcm_allocz(ntemps*temprec.bytes)
	fi

	while p, p:=p.next do
		ndest:=p.ndest

		for i to p.nopnds do
			a:=p.abc[i]
			nextloop when a.optype<>temp_opnd
			temp:=a.tempno

!CPL =TEMP, TCLNAMES[P.OPCODE], =NDEST

			pt:=&templist[temp]
!CPL "LOOP", tclnames[p.opcode], =i, =temp,"//LAST:",,PT.LASTTCL,"LTC:",,PT.LTCOUNT,
!"RTC:",,PT.RTCOUNT,"IX:",,PT.INDEX

			plast:=pt.lasttcl

			if i<=ndest then				!ltemp
				if pt.rtcount then PERROR("SCANT1?") fi

				if plast=nil then			!first write
					if i=1 then
						p.ltmode:=1			!Set all 1st ltemps to 'Tm'
						p.firstlt:=1
!CPL "  FIRST WRITE"
					fi
				else						!subsequent write
					if i>1 then PERROR("SCANT3?") fi
					p.ltmode:=1
				fi

				++pt.ltcount
!CPL " ",=PT.LTCOUNT

			else							!rtemp
				if pt.ltcount=0 or plast=nil then PERROR("SCANT2?") fi

!CPL " ",=PT.RTCOUNT
				if pt.rtcount then			!previous rtemps exist
!CPL "  SUBSEQ RTEMP"
					plast.islast.[pt.index]:=0		!make the last not the last!
				elsif pt.index=1 then		!1st rtemp after leftmost ltemp
!CPL "  FIRST RTEMP",=PT.INDEX
					case pt.ltcount
					when 1 then
						plast.ltmode:=0		!change from tentative Tm/WN to W1 temp
					else					!assume 2 or more
						plast.ltmode:=2		!change last Tm temp to Tx
					esac
				fi

!CPL "  SET ISLAST"
				p.islast.[i]:=1				!assume this will be last rtemp

				++pt.rtcount
			fi

			pt.lasttcl:=p					!always points to last tcl that uses this temp
			pt.index:=i
		od
	od

	if ntemps>maxfixedtemp then
		pcm_free(templist, ntemps*temprec.bytes)
	fi
end

export proc reducetemps(psymbol d)=
!build templist table for proc d, and fixup ltmode/islast fields in tcl code
	tcl p:=d.code, plast
	int ndest, temp
	ref temprec pt
	tclopnd a
	int ntempts

!CPL "REDUCE TEMPS"
	ntemps:=d.maxtemp
	nredtemps:=0

	if ntemps<=maxfixedtemp then
		tempmap:=&fixedtempmap
		memset(tempmap, 0, ntemps)

	else
		tempmap:=pcm_allocz(ntemps)
	fi

	memset(&tempmodes, 0, min(ntemps, maxfixedtemp)*tempmoderec.bytes)

!set up translation map
	while p, p:=p.next do
		for i to p.nopnds do
			maptemp(D,p, i)
		od
	od

!now convert all the temp nos. Note that operands can be shared so can be
!visited more than once; a flag .reduced ensures only converted once
	p:=d.code
	while p, p:=p.next do
		for i to p.nopnds do
			a:=p.abc[i]
			if a.optype=temp_opnd and not a.reduced then
				a.tempno:=tempmap[a.tempno]
				a.reduced:=1
			fi
		od
	od

	if ntemps>maxfixedtemp then
		pcm_free(tempmap, ntemps)
	fi

	d.tempmodes:=pcm_alloc(nredtemps*tempmoderec.bytes)

	for i to nredtemps do
		d.tempmodes[i]:=tempmodes[i]
	od

	d.maxtemp:=nredtemps


!CPL "REDUCED TEMPS FROM",NTEMPS,"TO",NREDTEMPS, D.NAME,"///////",=d.maxtemp

!FI
end

proc maptemp(PSYMBOL D, tcl p, int n)=
	int oldtemp, newtemp
	int mode, size
	tclopnd a:=p.abc[n]
	ref tempmoderec pm

	return when a.optype<>temp_opnd

	oldtemp:=a.tempno

	if tempmap[oldtemp] then				!already mapped
		newtemp:=tempmap[oldtemp]
		finish	
	fi

!assume this is a new ltemp modified for first time.
!first get mode info

	mode:=p.mode
	size:=p.size

	if p.opcode=kcall then
		mode:=a.opmode
		size:=a.opsize
	fi

	for i to nredtemps do
		pm:=&tempmodes[i]
		if pm.used=0 then
			if pm.mode=mode and pm.size=size then
				pm.used:=1
				tempmap[oldtemp]:=newtemp:=i
				finish
			fi
		fi
	od

	if nredtemps>=maxfixedtemp then
		merror("Too many new temps")
	fi

	pm:=&tempmodes[++nredtemps]
	pm.mode:=mode

	pm.size:=size
	pm.used:=oldtemp
	tempmap[oldtemp]:=newtemp:=nredtemps

finish:
	if p.islast.[n] then			!last used of it; free this combo
		tempmodes[newtemp].used:=0
	fi
end


=== tc_decls.m 0 0 4/40 ===

export type psymbol = ref pstrec

!export record pstrec = $caligned
export record pstrec =
	ichar name
	psymbol next				!proc or global static
	psymbol nextparam
	psymbol nextlocal
	psymbol nextstatic
	tcl code					!proc body; istatic init data
	ref[]tempmoderec tempmodes	!procs only

!	psymbol owner
!	psymbol generic				!locals/params: version in global ST

!	ref procinforec info		!procs: info to help codegen	

	union
!		tcl pcaddr				!for procs: entry point to func
		ref proc dlladdr		!for imported funcs
		ivoid staddr			!var statics: address
		psymbol cprocowner		!C target:var statics: owner proc
		int retindex			!local return label index (used by mcl etc)

	end
	ref fwdrec fwdrefs			!fwd ref chain

	byte id
	byte opcode					!for opcode_rw
	byte subcode				!for jumpcc_rw/setcc_rw/type_rw
	byte nrefs
	i32 offset

	byte mode
!	byte isentry
	byte nretvalues				!func: number of return values (0 for proc)
	byte variadic				!0 or N: fixed params of variadic function

	byte dllindex				!for dllproc: which dll in dlltable
	byte reg
	byte reftype
	byte segment

	i32 labelno
	u32 seqno

	u32 size
	i16 stindex
	i16 importindex

	u32 maxtemp				!for procs set after tcl codegen
	u32 pos: (sourceoffset:24, fileno:8)

	i16 nlocals
	i16 impindex
	i16 expindex
	u16 flags:(chasstatics:1, addrof:1, atvar:1, used:1,
				imported:1, exported:1, isthreaded:1, ishandler:1,
				isentry:1)

!	byte scope
	byte nparams
	byte align					!for variables
	struct						!for params only
		byte paramcat				!tc_reg/move/stack/spill
		byte fromreg				!tc_reg: move from .fromreg to .reg
	end
!	byte maxtemp				!for procs set after tcl codegen

end

export type tcl = ref tclrec

export record tclrec =
	tcl next
!--
	union						!two 32-bit params used according to opcode
		struct					!pointer ops
			i32 scale			!scale factor for offset
			i32 extra			!extra constant byte offset, already scaled
		end
		struct					!call/etc
			i32 nargs			!number of args set via setparam
!			i32 nret			!0/1/2 for call proc/func/mult
		end
		struct					!switch
			i32 minlab
			i32 maxlab
		end

!following single values set by tc_gen_n or tc_gen_cond or tc_gen_op
		i32 index				!general sequential index for setparam/temp etc
		i32 fnindex			!sysfn index number
		i32 cond				!tcl condition code for jumpcc etc
		i32 step				!always +ve fixed step size for forup/fordown; also INCR
		i32 truncmode			!convert/truncate: truncated mode

		struct
			i32 x				!common access to these two params
			i32 y
		end
	end
!--
	u32 pos:(psourceoffset:24, pfileno:8)
	u32 size
!--
	byte mode
	byte mode2
	byte opcode
	byte flags:(isglobal:1,
				isvariadic:1,	!for calls: calling a variadic function
				firstlt:1)

	byte nopnds
	byte argoffset				!So that p.abc[i+offset] accesses i'th argument
	union
		byte ndest				!no. of dest temps, usually 0 or 1, can N for calls, 2 for divrem
		byte nret				!alias for same value
	end
	byte ltmode					!is 0/1/2 for first ltemp, which can be Tm/Tx
!--
	u32 islast

	u16 seqno
	[2]byte spare2
!--	
!only allocated up to here; the rest depends on number of tclopnds

	union
		struct
			tclopnd a,b,c					!only present for correct .nopnds
		end
		[]tclopnd abc
!		[-1:]tclopnd args				!args [1] corresponds with abc[3]
	end
end

export type tclopnd = ref opndrec

export record opndrec =
	union
		i64 value
		r64 xvalue
		r32 xvalue32
		ichar svalue
		psymbol def
		struct
			u32 tempno
			byte reg
		end

		int labelno
!		unit asmcode
	end
	u32  opsize
	byte optype
	byte opmode
	byte flags:(isvariadic:1,			!variadic argument (outside fixed params)
				isbinary:1,
				isstring:1,
				reduced:1)
	byte spare1
end

global record tempmoderec=
	u32 size
	i32 offset
	byte mode
	byte used
	[6]byte dummy
end

global record temprec =
!first group is the prepass to set up .ltmode/.islast within tcl ops

	tcl lasttcl				!nil to start, then reference to tcl that has last ref
	u16 ltcount				!how many writes so far to a temp
	u16 rtcount				!how many reads so for of a temp
	byte index				!index of operand for the last temp seen
	[3]byte spare
!second group is used during mcl pass for reg allocation and temp management

!	byte loc				!reg, spilled or mult
!	byte reg				!allocated reg (mult temps must use same reg)
!	byte SPARE
end

global record fwdrec =
	ref fwdrec nextfwd
	i32 offset
	i16 reltype
	i16 seg
end

export byte tcldone, mcldone, ssdone, objdone, exedone

global [maxfixedtemp]temprec fixedtemplist
global [maxfixedtemp]byte fixedtempmap
global ref[]temprec templist			!points to above for small numbers of temps
global ref[]byte tempmap

!global const maxfixedtemp=256
global const maxfixedtemp=512
global const maxltemp=4					!as used by multiple func return

export int ntemps

global psymbol pstatictable, pstatictablex
global psymbol pproctable, pproctablex
global psymbol pimporttable, pimporttablex

global psymbol currprog
export psymbol currfunc
global psymbol blockretname
export psymbol entryproc		!entry point func

global const maxparam=100

global const maxplibfile=50
global [maxplibfile]ichar plibfiles
global [maxplibfile]u64 plibinst
global int nplibfiles

strbuffer sbuffer
global ref strbuffer pdest=&sbuffer
global int pcmdskip

EXPORT ICHAR $PMODULENAME

export [pmaxtuplesize]tclopnd extretopnds		!temps to hold func results
export [maxparam]tclopnd extparamopnds

global byte pcheckunusedlocals

global const pmaxtuplesize = 4
=== tc_diags.m 0 0 5/40 ===
!byte fshowallmodes=1
byte fshowallmodes=0

int currlineno
int currfileno

strbuffer sbuffer
ref strbuffer dest=&sbuffer
int destlinestart

tcl currtcl

!const tab1="  "
!const tab2="    "
const tab1="    "
!const tab2=tab1+tab1

const labprefix = "L"
!const labprefix = "_"

const tclindent = 1

!const fshowsymbols=1
const fshowsymbols=0

ref[]byte labeltab

[tclnames.bounds]int countS
[tclnames.bounds, PSTDNAMES.bounds]int types

global proc strtcl(tcl p, int inlinex=0)=
!inlinex=1 when generating inlinex comments for strmcl
	int opcode, nopnds
	tclopnd a,b,c
	int ntypes, defused

	const showformatted=1
	opcode:=p.opcode

!++COUNTS[OPCODE]
!++TYPES[OPCODE, P.MODE]

	currtcl:=p			!currtcl used in psopnd etc

!CPL "----STRTCL",TCLNAMES[OPCODE], CURRFUNC.NAME

	nopnds:=p.nopnds
	ntypes:=tclhastype[opcode]

	a:=b:=c:=nil

	if nopnds then
		a:=p.a
		if nopnds>1 then
			b:=p.b
			if nopnds>2 then
				c:=p.c
			fi
		fi
	fi

	case opcode
	when klabel then
		strlabel(a.labelno,1)
		return
	when kcomment then
		psstr("!")
		psstr(a.svalue)
		return
!
	esac

	psstr(tab1)
!
!PSSTR(STRINT(INT(P),"H"))
!PSSTR(" ")
!PSSTR(STRINT(P.SEQNO,"4Z"))
!PSSTR(" ")

	defused:=0
	if not showformatted then
		goto default
	fi

	switch opcode
	when kmove then

	IF NOT A OR NOT B THEN
		CPL "MOVE BAD OPNDS"
	FI
	IF P.MODE=TPVOID THEN
		CPL "MOVE NO MODE"
	FI



		psopnd(1)
		psassign()
		psopnd(2)

	when klabel then

	when kjump then
		psstr("goto ")
		psopnd(1)

	when kijump then
		psstr("goto ")
		psopnd(1)
		psstr("^")

	when kadd..kfmod then
!IF A.OPTYPE<>TEMP_OPND THEN PSSTR("TTTADD/NONTEMP ") FI
		psopnd(1)
		psassign()
		psbinop(p.opcode,2,3)

	when kneg..ktypepun then
		psopnd(1)
		psassign()
		psmonop(p.opcode,2)

	when kaddto..ksubpxto then
		psbinop(p.opcode,1,2)

	when knegto..ktoboolto then
		psmonop(p.opcode,1)

	when kjumpcc then
		psstr("if ")
		psopnd(2)
		psstr(ccshortnames[p.cond])
		psopnd(3)
		psstr(" then goto ")
		psopnd(1)

	when ksetcc then
		psopnd(1)
		psassign()
		psopnd(2)
		psstr(ccshortnames[p.cond])
		psopnd(3)

	when kjumpf then
		psstr("if not ")
		psopnd(2)
		psstr(" then goto ")
		psopnd(1)

	when kjumpt then
		psstr("if ")
		psopnd(2)
		psstr(" then goto ")
		psopnd(1)

	when kjumpin, kjumpout then
		psstr("if ")
		psopnd(2)
		psstr((opcode=kjumpin|" in "|" not in "))
		psopnd(3)
		psstr(" .. ")
		psopnd(4)
		psstr(" then goto ")
		psopnd(1)

	when kforup, kfordown then
		psopnd(2)
		psstr((opcode=kforup|" +:= "|" -:= "))
		psint(p.step)

		psstr("; if ")
		psopnd(2)
		psstr((opcode=kforup|" <= "|" >= "))
		psopnd(3)
		psstr(" then goto ")
		psopnd(1)

	when kto then
		psopnd(2)
		psstr("--; if ")
		psopnd(2)
		psstr(" then goto ")
		psopnd(1)

	when kaddpx, kiloadx then
!IF OPCODE=KLOADPX AND A.OPTYPE<>TEMP_OPND THEN PSSTR("TTTLOADPX/NONTEMP ") FI
		psopnd(1)
		psstr(" := ")
		psptr(2, 3, p.scale, p.extra)
		if opcode=kiloadx then
			psstr("^")
		fi

	when kistorex then
		psptr(1, 2, p.scale, p.extra)
		psstr("^ := ")
		psopnd(3)

!	when ksysproc, ksysprocx then
!		psstr(sysfnnames[p.fnindex]+3)
!		psstr("(")
!		for i to p.nopnds do
!			psopnd(p.abc[i])
!			if i<p.nopnds then psstr(",") fi
!		od
!		psstr(")")
!
!	when ksysfn,ksysfnx then
!		psopnd(1)
!		psstr(" := ")
!		psstr(sysfnnames[p.fnindex]+3)
!		psstr("(")
!		if p.nopnds>1 then
!			psopnd(2)
!			if p.nopnds=3 then
!				psstr(",")
!				psopnd(3)
!			fi
!		fi
!		psstr(")")
!
	when kiswap then
		psstr("swap(")
		psopnd(1)
		psstr(",")
		psopnd(2)
		psstr(")")

!	when kblocktemp then
!		psstr("B")
!		psint(p.index)

	when kswitch, kswitchu then
		psstr((opcode=kswitch|"switch "|"switchu "))
		psopnd(3)
		psstr(" (")
		psopnd(1)
		psopnd(2)
		psint(p.minlab)
		psstr(":")
		psint(p.maxlab)
		psstr(")")

	when kcall then
		for i to p.nret do
!			psopnd(p.abc[i+1])
			psopnd(i)
			if i<p.nret then psstr(", ") fi
		od
		if p.nret then psstr(" := ") fi

		psopnd(p.nret+1)
		psstr("(")
		for i to p.nargs do
			psopnd(i+p.argoffset)
!			a:=p.abc[i+p.argoffset]
!			if a.isvariadic then psstr("?") fi
			if i<p.nargs then psstr(", ") fi
		od
		psstr(")")
!		if p.isvariadic then psstr("?") fi

	when kincrto, kdecrto then
		psopnd(1)
		if p.step=1 then
			psstr((opcode=kincrto|"++"|"--"))
		else
			psstr((opcode=kincrto|"+:="|"-:="))
			psint(p.step)
		fi

	when kstop then
		psstr("stop ")
		psopnd(1)

!	when kclear, kretfn, kretproc, kswlabel, kdata,
!			kretmult, keval then
!		goto default
!
	when kloadbit, kloadbf then
		psopnd(1)
		psassign()
		psopnd(2)
		psstr(".[")
		psopnd(3)
		if opcode=kloadbf then
			psstr("..")
			psopnd(4)
		fi
		psstr("]")

	when kstorebit then
		psopnd(1)
		psstr(".[")
		psopnd(2)
		psstr("]")
		psassign()
		psopnd(3)

	when kstorebf then
		psopnd(1)
		psstr(".[")
		psopnd(2)
		psstr("..")
		psopnd(3)
		psstr("]")
		psassign()
		psopnd(4)

	when kincrload, kdecrload then
		psopnd(1)
		psassign()
		psstr((opcode=kincrload|"++"|"--"))
		psopnd(2)

	when kloadincr, kloaddecr then
		psopnd(1)
		psassign()
		psopnd(2)
		psstr((opcode=kloadincr|"++"|"--"))

	when kretfn then
		psstr("return ")
		psopnd(1)

	when kretproc then
		psstr("return")

	else
		PSSTR("* ")				!may need attention
default:
!CPL "DEFAULT"
		psstr(tclnames[opcode])
		psstr(" ")
		defused:=1
		for i to nopnds do
			psopnd(i)
			psstr(" ")
		od

IF OPCODE IN [KSUBPX, KSUBP] AND P.EXTRA THEN
	CPL "SUBP/X HAS EXTRA OFFSET"
os_getch()
FI

	end switch

	if inlinex then
		PSTABTO(30)
!		psstr("   ")
	else
		PSTABTO(40)
	fi

	case ntypes
	when 1, 2 then
		psmode(p.mode, p.size)
!		if ntypes=2 and p.mode<>p.mode2 then
		if ntypes=2 then
			psstr("/")
			psmode(p.mode2)
		fi
		IF FSHOWALLMODES THEN RECASE 3 FI
	when 3 then
		if a then
			psstr("(")
			for i to nopnds do
				a:=p.abc[i]
				if i>1 then psstr(",") fi
				if i=p.nret+1 and i>1 then psstr(" ") fi
				if i=p.nret+2 then psstr(" ") fi
				if a.opmode then
					psmode(a.opmode, a.opsize)
					if a.isvariadic then psstr("?") fi
				else
					psstr("---")
				fi
			od
			psstr(")")
			PSMODE(P.MODE, P.SIZE)

		fi
		psstr(" ")
	else
		psstr("---")
	esac

	if inlinex then
		psstr(" ")
	else
		PSTABTO(56)
	fi

	GS_LEFTSTR(DEST,TCLNAMES[OPCODE],9)
!IF OPCODE=KMOVE AND A.OPTYPE=TEMP_OPND THEN PSSTR("MT") FI

!return when inlinex

	PSSTR("|")
!CPL $LINENO,A,=P.NOPNDS
!RETURN

!if not showformatted then
	if defused and (p.x or p.y) then
		psstr(" X:")
		psint(p.x)
		psstr(" Y:")
		psstr(" Y:")
		psint(p.Y)
	fi

	if p.isglobal then psstr(" Isglobal") fi
	if p.isvariadic then psstr(" Isvariadic") fi
!
!IF OPCODE=KCALL THEN
!	PSSTR(" NRET:"); PSINT(P.NRET)
!	PSSTR(" NARGS:"); PSINT(P.NARGS)
!	PSSTR(" ARGOFF:"); PSINT(P.ARGOFFSET)
!FI

INT FIRSTLAST:=1
	for i to nopnds do
		a:=p.abc[i]
!		if a and a.optype in [temp_opnd, tempptr_opnd] then
		if a and a.optype=temp_opnd then
!			if i<=p.ndest and p.ltmode[i]<2 then
!			if i<=p.ndest then
			if i<=p.ndest AND (I>1 OR P.LTMODE<2) then
!				psstr(" (")
FIRSTLAST:=0
				psstr("<")
				psstr(strtemp(a.tempno))
				psstr(": ")
			fi
			if p.islast.[i] then
				IF FIRSTLAST THEN PSSTR("   "); FIRSTLAST:=0 FI
				psstr(":")
				psstr(strtemp(a.tempno))
				psstr(">")
!				psstr(")")
			fi
		fi
	od
!CPL $LINENO

!	PSSTR(" ")
!	PSINT(INT(P.A)); PSSTR(" ")
!	PSINT(INT(P.B)); PSSTR(" ")
!	PSINT(INT(P.C))

!IF P.NDEST THEN
!PSSTR(" LT:")
!PSINT(P.LTMODE)
!FI

end

proc psopnd(int n)=
	tclopnd a
	byte ptrflag

	a:=currtcl.abc[n]

	psstr(stropnd(a))

	ptrflag:=tcltempptr[currtcl.opcode]

	if a.optype=temp_opnd then
!		if n=1 and currtcl.ltmode then
!			psstr((currtcl.ltmode=2|"x"|"m"))
!		fi
!		if n=1 and currtcl.firstlt then psstr(".") fi
		if ptrflag=n or ptrflag=3 and n<3 then
			psstr("^")
		fi
	fi
end

global function stropnd(tclopnd p)ichar=
	[maxparam]tclopnd paramopnds
	static[512]char str
	[4]char str2
	psymbol d
	ref byte q
	int length

!RETURN "<OPND>"

	if p=nil then
		return "-"
	fi

	case p.optype
	when int_opnd then
		return strint(p.value)
!		return strint(p.value,"H")
!		return strint(p.value,"HU")

	when real_opnd then
		return strreal(p.xvalue)
	when r32_opnd then
		return strreal(p.xvalue32)

	when string_opnd then
		length:=strlen(p.svalue)
		if length<str.len/4 then
			strcpy(str,"""")
			convertstring(p.svalue,&.str+1, length)
			strcat(str,"""")
		else
			return "<Long str>"
		fi

	when mem_opnd then
		print @str,p.def.name

	when memaddr_opnd then
		fprint @str,"&#",p.def.name

	when temp_opnd then
		return strtemp(p.tempno)

	when label_opnd then
!		fprint @str,"L# ",p.labelno
		fprint @str,"## ", labprefix, p.labelno
!		fprint @str,"## ","#",p.labelno

	when data_opnd then
		q:=p.svalue

		if p.isstring then
			print @str, "S<"

			to min(currtcl.size,40) do
				str2[1]:=q^; str2[2]:=0
				strcat(str, str2)
				++q
			od
		else						!binary, or normal non-data-string data
			print @str, "B<"
			to min(currtcl.size,10) do
				strcat(str, strint(q^,"Hz2"))
				strcat(str, " ")
				++q
			od
!		else
!			STRCPY(STR, "<NOT S OR B>")
		fi
		strcat(str, ">")

	else
		return "<TCLOPND?>"
	esac

	return str
end

global function strtclstr(tcl p)ichar =
	gs_init(dest)

	destlinestart:=1
	strtcl(p,1)

	return dest.strptr
end

global proc strlabel(int labelno,colon=0)=
!	psstr("L")
	psstr(labprefix)
	psint(labelno)
	if colon then
		psstr(":")
	fi
	psstr(" ")
end

global func writealltcl(ichar caption)ref strbuffer=
!write all tcl code in system by scanning all procs
!tcl code is only stored per-proc
	tcl p
	psymbol d,e
	tclopnd a

!CPL "WRITEALL PCL"

	gs_str(dest,"PROC ")
	gs_strln(dest,caption)
	gs_strln(dest,"!DATA ------------------------------------------------------")

!GS_STRLN(DEST,"<WRITEPSTALLTCL TO BE REVISED>")

!CHECKCOMM("ALL1")

!GS_STRLN(DEST, "<TCL WRITE NOT READY>")
!
	labeltab:=pcm_allocz(mlabelno)			!indexed 1..labelno

	d:=pstatictable

	while d, d:=d.next do
		if d.id=static_id then
			psstr("var ")
			psstr(strpmode(d.mode, d.size))
			psstr(" ")
			psstr(d.name)

			if d.code then
				psstr(" = ")
				currtcl:=d.code
				psdata(d.code)
			else
				psline()
			fi
		fi
	od
	psline()

	gs_strln(dest,"!IMPORTS ------------------------------------------------------")
	d:=pimporttable

	while d, d:=d.next do
		currfunc:=d
!cpl "import",d.name, idnames[d.id]
!		if d.id=proc_id then
			psstr("Import ")
			psprocsig(d, "")

!			psstrline("End")
			psline()

!		fi
	od
	psline()

	gs_strln(dest,"!CODE ------------------------------------------------------")
	d:=pproctable

	while d, d:=d.next do
		currfunc:=d
		if d.id=proc_id then
			psprocsig(d)

			psprocbody(d)

			psstrline("End")
			psline()

		fi
	od
	psline()
!
	pcm_free(labeltab, mlabelno)
!
!INT NN:=0, TT:=0
!FOR I IN COUNTS.BOUNDS WHEN COUNTS[I] DO
!	PRINT ++NN:"2",,":", TCLNAMES[I]:"10JL", COUNTS[I]:"5JR", $
!
!
!	FOR J IN PSTDNAMES.BOUNDS WHEN TYPES[I,J] DO
!		PRINT STRPMODE(J),,":",,TYPES[I,J],$
!	OD
!	CPL
!
!	TT+:=COUNTS[I]
!OD
!CPL
!CPL =TT
!CPL

	return dest
end

proc writetcl(tcl p)=
	tclopnd a

	a:=p.a

!	unless p.opcode=klabel and not labeltab[a.labelno] then
		strtcl(p)
		gs_line(dest)
		psstrline("") when p.opcode=keval
!	end
end

proc psbinop(int opc, a, b)=
	tabledata []byte opcodes, []ichar opnames =
		(kadd,		"+"),
		(ksub,		"-"),
		(kmul,		"*"),
		(kdiv,		"/"),
		(kidiv,		"%"),
		(kirem,		"rem"),
		(kbitand,	"iand"),
		(kbitor,	"ior"),
		(kbitxor,	"ixor"),
		(kshl,		"<<"),
		(kshr,		">>"),
!		(kand,		"and"),
!		(kor,		"or"),
!		(kaddpx,	"+(pi)"),
		(ksubpx,	"-(pi)"),
		(ksubp,		"-(pp)"),
		(kpower,	"**"),
		(kaddto,	"+:="),
		(ksubto,	"-:="),
		(kmulto,	"*:="),
		(kdivto,	"/:="),
		(kidivto,	"%:="),
		(kiremto,	"rem:="),
		(kbitandto,	"iand:="),
		(kbitorto,	"ior:="),
		(kbitxorto,	"ixor:="),
		(kshlto,	"<<:="),
		(kshrto,	">>:="),
		(kmaxto,	"max:="),
		(kminto,	"min:="),
!		(kandto,	"and:="),
!		(korto,		"or:="),
	end

	for i to opcodes.len do
		if opc=opcodes[i] then
			psopnd(a)
			psstr(" ")
			psstr(opnames[i])
			psstr(" ")
			psopnd(b)
			return
		fi
	od

	psstr(tclnames[opc])	
	psstr("(")
	psopnd(a)
	psstr(",")
	psopnd(b)
	psstr(")")
end

proc psmonop(int opc, int a)=
	tabledata []byte opcodes, []ichar opnames =
		(kneg,		"-"),
		(kbitnot,	"inot "),
		(knot,		"not "),
		(knegto,	"-:="),
		(kabsto,	"abs:="),
		(kbitnotto,	"inot:="),
		(knotto,	"not:="),
		(ktoboolto,	"istrue:="),
	end

	for i to opcodes.len do
		if opc=opcodes[i] then
			psstr(opnames[i])
			psopnd(a)
			return
		fi
	od

	psstr(tclnames[opc])	
	psstr("(")
	psopnd(a)
	psstr(")")
end

proc psassign=
	gs_str(dest," := ")
end

global proc psmode(int mode, size=0) =
	psstr(strpmode(mode, size))
end

proc psprocsig(psymbol d, ichar term=" =")=
	psymbol e
	byte comma:=0
	int lastmode:=tpvoid, m, lastsize, size

	psstr("Proc ")
	psstr(d.name)
	psstr("(")

	e:=d.nextparam

	while e, e:=e.nextparam do
		if comma then psstr(", ") fi
		if e.mode<>lastmode and e.size<>lastsize then
			lastmode:=e.mode
			lastsize:=e.size
			psstr(strpmode(lastmode, lastsize))
			psstr(" ")
		fi
		psstr(e.name)

		comma:=1
	od
	psstr(")")

	if d.mode then
		psstr(strpmode(d.mode, d.size))
	fi
!	psstr(":")
	psstrline(term)

	comma:=0
	e:=d.nextlocal
	while e, e:=e.nextlocal do
		if comma then psline() fi
		psstr(tab1)
		psstr(strpmode(e.mode, e.size))
		psstr(" ")
		psstr(e.name)
		comma:=1
	od
	if comma then psline() fi
	if d.nextlocal then psline() fi

	e:=d.nextstatic

	while e, e:=e.nextstatic do
		if comma then psline() fi
		psstr(tab1)
		psstr(strpmode(e.mode, e.size))
		psstr(" ")
		psstr(e.name)

		if e.code then
			psstr(" = ")
			currtcl:=e.code
			psdata(e.code)
		fi
	od
	if d.nextstatic then psline() fi
end

proc psdata(tcl p)=
	tclopnd a
	byte mult:=istrue p.next

	if mult then psstrline("(") fi

	while p, p:=p.next do
		a:=p.a
		if mult then psstr("    ") fi
!		psstr(tclnames[p.opcode])
!		psstr(" ")
		psstr(stropnd(a))
		psstr(" ")

!!		psmode(a.opmode, a.opsize)
!		psmode(p.mode, p.size)
		if p.next then psstr(",") fi
		psline()
	od

	if mult then psstrline(")") fi
end

proc psprocbody(psymbol d)=
	tcl p
	tclopnd a

	p:=d.code

	return unless p

!do first pass populating label table

	while p, p:=p.next do
		if p.opcode<>klabel then
			for i to p.nopnds do
				a:=p.abc[i]
				if a.optype=label_opnd then

					labeltab[a.labelno]:=1
				fi
			od
		fi
	od

	p:=d.code				!skip nop
	destlinestart:=dest.length

	while p, p:=p.next do
		writetcl(p)
		destlinestart:=dest.length
	od

end

proc psptr(int a, b, scale, offset)=
!CPL "PSPTR",A,B
	tclopnd ax:=currtcl.abc[a]
	tclopnd bx:=currtcl.abc[b]

	psstr("(")
	psopnd(a)

	if bx then
		if bx.optype=int_opnd then
			offset+:=bx.value*scale
		else
			psstr(" + ")
			psopnd(b)
			if scale>1 then
				psstr("*")
				psint(scale)
			fi
		fi
	fi

	if offset>0 then
		psstr(" + ")
		psint(offset)

	elsif offset<0 then
		psstr(" - ")
		psint(-offset)
	fi
	psstr(")")
end

global proc psstr(ichar s)=
	gs_str(dest,s)
end

global proc psstrline(ichar s)=
	gs_str(dest,s)
	gs_line(dest)
end

global proc psline=
	gs_line(dest)
end

global proc psint(int a)=
	gs_str(dest,strint(a))
end

global proc psname(psymbol d)=
	gs_str(dest,getfullname(d))
end

global proc pstabto(int n)=
	int col:=dest.length-destlinestart
	while n>col do psstr(" "); ++col od
end

global func writepst:ref strbuffer=
	gs_init(dest)

	gs_strln(dest,"------------------------------------------------------")
	writepst2("PROC PST Global Static Table", pstatictable)
	psline()
	writepst2("PROC PST Global Proc Table", pproctable)
	psline()

	writepst2("PROC PST Global Import Table", pimporttable)
	psline()

	return dest
end

global proc writepst2(ichar caption, psymbol d)=
	int i:=0, j
	psymbol e

	psstrline(caption)
	psline()

	while d, d:=d.next do
!PSSTR(STRINT(INT(D),"H"))
!PSSTR(" ")
		writepsymbol(d, "25jl")

		if d.id in [proc_id, import_id] then
			e:=d.nextparam
			j:=0
			while e, e:=e.nextparam do
				psstr("    ")
				writepsymbol(e, "21jl")
			od
			e:=d.nextlocal
			j:=0
			while e, e:=e.nextlocal do
				psstr("    ")
				writepsymbol(e, "21jl")
			od
			e:=d.nextstatic
			j:=0
			while e, e:=e.nextstatic do
				psstr("    ")
				writepsymbol(e, "21jl")
			od
		fi
!PSLINE()
	od
	psline()
end

proc writepsymbol(psymbol d, ichar fmt)=
	byte localfile:=0
	[256]char str

	print @str, d.seqno:"4", idnames[d.id]
	psstr(str)
	to 8-strlen(idnames[d.id]) do psstr(" ") od

	str[1]:=0

!	print @str, d.name:fmt
	print @str, d.name:fmt,D,$
	psstr(str)


	psstr(strpmode(d.mode, d.size))

	if d.id=proc_id then
		psstr(" Pm:")
		psint(d.nparams)
		psstr(" Loc:")
		psint(d.nlocals)
	fi

	if d.exported then psstr(" Exported") fi
	if d.imported then psstr(" Imported") fi
	if d.variadic then psstr(" Varpms:"); psint(d.variadic) fi
	if d.isthreaded then psstr(" TC") fi
!*!	if d.reg then psstr(" "); psstr(regnames[d.reg]) fi
!	if d.hasdot then psstr(" Dot") fi
	if d.isentry then psstr(" ENTRY PT") fi

	if d.id in [local_id, param_id, static_id] then
		PSSTR(" Align:")
		psint(d.align)
	fi

!	if d.id=proc_id then psstr(" .PCADDR ="); PSSTR(STRINT(CAST(D.PCADDR),"H")) fi

!	if d.owner then
!		psstr(" (")
!		psint(d.owner.seqno)
!		psstr(" ")
!		psstr(d.owner.name)
!		psstr(")")
!	fi	

	if ctarget and d.id=static_id and d.cprocowner then
		psstr(" (Proc:")
		psstr(d.cprocowner.name)
		psstr(") (D:")
!		psint(cast(d.pcdata))
!*!		psstr(strint(cast(d.pcdata),"H"))
		psstr(")")
	fi
	if ctarget and d.id=proc_id and d.chasstatics then
		psstr(" Has statics")
!		psint(d.chasstatics)
	fi

	psline()
end

global func strtemp(int temp)ichar=
	static [16]char str
	[2]char cc
	str[1]:='T'
	str[2]:=0

	strcat(str, strint(temp))

!	cc[1]:=temp+'a'-1
!	cc[2]:=0
!	strcat(str, cc)

	str
end

=== tc_tables.m 0 0 6/40 ===
!type system

export enumdata \
		[0:]ichar pstdnames,
		[0:]byte psize,

		[0:]byte psigned,
		[0:]byte pint,

		[0:]byte pfloat,
		[0:]byte pwide =

!                 names       size  s  i  f  w
	(tpvoid=0,    "void",    	0,	0, 0, 0, 0),

	(tpr32,       "r32",    	4,	0, 0, 1, 0),
	(tpr64,       "r64",    	8,	0, 0, 1, 1),

	(tpu8,        "u8",      	1,	0, 1, 0, 0),
	(tpu16,       "u16",    	2,	0, 1, 0, 0),
	(tpu32,       "u32",    	4,	0, 1, 0, 0),
	(tpu64,       "u64",    	8,	0, 1, 0, 0),

	(tpi8,        "i8",      	1,	1, 1, 0, 0),
	(tpi16,       "i16",    	2,	1, 1, 0, 0),
	(tpi32,       "i32",    	4,	1, 1, 0, 0),
	(tpi64,       "i64",    	8,	1, 1, 0, 0),

	(tpblock,     "mem",   		0,	0, 0, 0, 0),
	(tpvector,    "vec",   		0,	0, 0, 0, 0),

	(tplast,      "$last",   	0,	0, 0, 0, 0),


end

export const tpref = tpu64

export enumdata [0:]ichar opndnames =
	(no_opnd=0,			$),

	(mem_opnd,			$),
	(temp_opnd,			$),
	(memaddr_opnd,		$),

	(int_opnd,			$),
	(real_opnd,			$),
	(r32_opnd,			$),

	(string_opnd,		$),		!reference to a string elsewhere (so like a label)
	(stringimm_opnd,	$),		!immediate string using .asciz/.ascii
	(label_opnd,		$),
	(data_opnd,			$),

!	(metastring_opnd,	$),
end

!tclhastype:
! 0 		no type info
! 1			tcl.mode (t)
! 2			tcl.mode & tcl.mode2 (t & u)
! 3			Uses arg types only (eg. CALL)

export enumdata [0:]ichar tclnames,
				[0:]byte tclwrite,			!1/2: known fixed no. of temp dests; 0=not used, or depends on context
				[0:]byte tclhastype,
				[0:]byte tcltempptr,		!1/2/3 means temp in opnd 1/2/both must be T^
				[0:]byte tclnopnds =		!number of operands (some like call/retmult are variable)

!TCL opcodes
! T	lvalue			T3
! M	lvalue			x, T (x is static/local/proc)
! P lvalue			x, T3^

! a b c d			Rvalues: T3, x, &x, 123 4.56 "ABC" L123
! D					Data Rvalue: x, &x, 123 4.56 "ABC" L123 <datastr>

! L Label index		Labels
! d symbol			M (ST entry)

!** means opcode needs a 4th tclopnd; this needs specialing handling for temps

!                    Wr Types T^  N          (a b c)
	(knop=0,	$+1,  0,  0,  0,  0),  !     (- - -)
	(kcomment,	$+1,  0,  0,  0,  1),  !     (a - -)
  
	(kmove,		$+1,  0,  1,  0,  2),  !     (M b -)	M := b
	(keval,		$+1,  0,  1,  0,  1),  !     (a - -)
  
	(kaddpx,	$+1,  1,  1,  0,  3),  ! s,n (T b c)	T := b + c*s + n
	(kiloadx,	$+1,  1,  1,  0,  3),  ! s,n (T b c)	T :=(b + c*s + n)^
	(kistorex,	$+1,  0,  1,  0,  3),  ! s,n (b c r)	(a + b*s + n)^ := c
  
	(kcall,		$+1,  0,  3,  0,  0),  ! r,n (a - -)	([T ...] F [r ...]) r=nret, n=nargs
	(kretproc,	$+1,  0,  0,  0,  0),  !     (- - -)	return
	(kretfn,	$+1,  0,  1,  0,  1),  !     (a - -)	return a
	(kretmult,	$+1,  0,  3,  0,  0),  ! n   (a ...)	return n values

	(kjump,		$+1,  0,  0,  0,  1),  !     (L - -)	goto L
	(kjumpcc,	$+1,  0,  1,  0,  3),  ! cc  (L b c)	goto L when b cc c
	(kjumpt,	$+1,  0,  1,  0,  2),  !     (L b -)	goto L when istrue(b)
	(kjumpf,	$+1,  0,  1,  0,  2),  !     (L b -)	goto L when not istrue(b)
	(kijump,	$+1,  0,  1,  0,  1),  !     (a - -)	goto a
	(ksetcc,	$+1,  1,  1,  0,  3),  ! cc  (T b c)	T := b cc c
  
	(kto,		$+1,  0,  1,  0,  2),  !     (L b -)	--b; goto L when b<>0
	(kforup,	$+1,  0,  1,  0,  3),  ! n   (L b c)	b+:=n; goto L when b <= c
	(kfordown,	$+1,  0,  1,  0,  3),  ! n   (L b c)	b-:=n; goto L when b >= c

	(kiswap,	$+1,  0,  1,  3,  2),  !     (P P -)	swap(P, P)
  
	(kadd,		$+1,  1,  1,  0,  3),  !     (T b c)	T := b + c
	(ksub,		$+1,  1,  1,  0,  3),  !     (T b c)	T := b - c
	(kmul,		$+1,  1,  1,  0,  3),  !     (T b c)	T := b * c
	(kdiv,		$+1,  1,  1,  0,  3),  !     (T b c)	T := b / c (float only)
	(kidiv,		$+1,  1,  1,  0,  3),  !     (T b c)	T := b / c (int only; b % c)
	(kirem,		$+1,  1,  1,  0,  3),  !     (T b c)	T := b irem c
	(kbitand,	$+1,  1,  1,  0,  3),  !     (T b c)	T := b iand c
	(kbitor,	$+1,  1,  1,  0,  3),  !     (T b c)	T := b ior c
	(kbitxor,	$+1,  1,  1,  0,  3),  !     (T b c)	T := b ixor c
	(kshl,		$+1,  1,  1,  0,  3),  !     (T b c)	T := b << c
	(kshr,		$+1,  1,  1,  0,  3),  !     (T b c)	T := b >> c
	(kmin,		$+1,  1,  1,  0,  3),  !     (T b c)	T := min(b, c)
	(kmax,		$+1,  1,  1,  0,  3),  !     (T b c)	T := max(b, c)
  
	(katan2,	$+1,  1,  1,  0,  3),  !     (T b c)	T := atan2(b, c)
	(kpower,	$+1,  1,  1,  0,  3),  !     (T b c)    T := b ** c
	(kfmod,		$+1,  1,  1,  0,  3),  !     (T b c)
  
	(ksubpx,	$+1,  1,  1,  0,  3),  ! s   (T b c)	T := b - c*s
	(ksubp,		$+1,  1,  1,  0,  3),  ! s   (T b c)	T := (b - c)/s

	(kneg,		$+1,  1,  1,  0,  2),  !     (T b -)	T := -b
	(kabs,		$+1,  1,  1,  0,  2),  !     (T b -)    T := abs b
	(kbitnot,	$+1,  1,  1,  0,  2),  !     (T b -)    T := inot b
	(knot,		$+1,  1,  1,  0,  2),  !     (T b -)    T := not b
	(ktoboolt,	$+1,  1,  2,  0,  2),  !     (T b -)    T := istrue b
	(ktoboolf,	$+1,  1,  2,  0,  2),  !     (T b -)    T := not istrue b
  
	(ksqr,		$+1,  1,  1,  0,  2),  !     (T b -)    T := sqr(b)
  
	(ksqrt,		$+1,  1,  1,  0,  2),  !     (T b -)	T := sqrt(b)
	(ksin,		$+1,  1,  1,  0,  2),  !     (T b -)	T := sin(b)
	(kcos,		$+1,  1,  1,  0,  2),  !     (T b -)	T := cos(b)
	(ktan,		$+1,  1,  1,  0,  2),  !     (T b -)	T := tan(b)
	(kasin,		$+1,  1,  1,  0,  2),  !     (T b -)	T := asin(b)
	(kacos,		$+1,  1,  1,  0,  2),  !     (T b -)	T := asin(b)
	(katan,		$+1,  1,  1,  0,  2),  !     (T b -)	T := atan(b)
 
	(klog,		$+1,  1,  1,  0,  2),  !     (T b -)	T := log(b)
	(klog10,	$+1,  1,  1,  0,  2),  !     (T b -)	T := log10(b)
	(kexp,		$+1,  1,  1,  0,  2),  !     (T b -)	T := exp(b)
	(kround,	$+1,  1,  1,  0,  2),  !     (T b -)	T := round(b)
	(kceil,		$+1,  1,  1,  0,  2),  !     (T b -)	T := ceil(b)
	(kfloor,	$+1,  1,  1,  0,  2),  !     (T b -)	T := floor(b)
	(kfract,	$+1,  1,  1,  0,  2),  !     (T b -)	T := fract(b)
	(ksign,		$+1,  1,  1,  0,  2),  !     (T b -)	T := sign(b)

	(kfloat,    $+1,  1,  2,  0,  2),  !     (T b -)	T := float(b)
	(kfix,		$+1,  1,  2,  0,  2),  !     (T b -)	T := fix(b)
	(ktruncate,	$+1,  1,  2,  0,  2),  !     (T b -)	T := u(b)
	(kfwiden,	$+1,  1,  2,  0,  2),  !     (T b -)	T := r64(b)
	(kfnarrow,	$+1,  1,  2,  0,  2),  !     (T b -)	T := r32(b)
	(kwiden,	$+1,  1,  2,  0,  2),  !     (T b -)	T := t(b)
  
	(ktypepun,	$+1,  1,  2,  0,  2),  !     (T b -)	T := t(u@(b))
  
	(kaddto,	$+1,  0,  1,  1,  2),  !     (P b -)    P +:= b
	(ksubto,	$+1,  0,  1,  1,  2),  !     (P b -)	P -:= b
	(kmulto,	$+1,  0,  1,  1,  2),  !     (P b -)	P *:= b
	(kdivto,	$+1,  0,  1,  1,  2),  !     (P b -)	P /:= b (float)
	(kidivto,	$+1,  0,  1,  1,  2),  !     (P b -)	P /:= b (int: %:= b)
	(kiremto,	$+1,  0,  1,  1,  2),  !     (P b -)	P irem:= b
	(kbitandto,	$+1,  0,  1,  1,  2),  !     (P b -)	P iand:= b
	(kbitorto,	$+1,  0,  1,  1,  2),  !     (P b -)	P ior:= b
	(kbitxorto,	$+1,  0,  1,  1,  2),  !     (P b -)	P ixor:= b
	(kshlto,	$+1,  0,  2,  1,  2),  !     (P b -)	P <<:= b
	(kshrto,	$+1,  0,  2,  1,  2),  !     (P b -)	P >>:= b
	(kminto,	$+1,  0,  1,  1,  2),  !     (P b -)	P min:= b
	(kmaxto,	$+1,  0,  1,  1,  2),  !     (P b -)	P max:= b
	(kaddpxto,	$+1,  0,  1,  1,  2),  ! s   (P b -)    P +:= b*s
	(ksubpxto,	$+1,  0,  1,  1,  2),  !     (P b -)    P -:= b*s
 
	(knegto,	$+1,  0,  1,  1,  1),  !     (P - -)    -:=P
	(kabsto,	$+1,  0,  1,  1,  1),  !     (P - -)    abs:=P
	(kbitnotto,	$+1,  0,  1,  1,  1),  !     (P - -)	inot:=P
	(knotto,	$+1,  0,  1,  1,  1),  !     (P - -)    not:=P
	(ktoboolto,	$+1,  0,  1,  1,  1),  !     (P - -)    istrue+:=P
  
	(kincrto,	$+1,  0,  1,  1,  1),  !     (P - -)	++P
	(kdecrto,	$+1,  0,  1,  1,  1),  !     (P - -)	--P
	(kincrload,	$+1,  1,  1,  2,  2),  !     (T P -)	T := ++P
	(kdecrload,	$+1,  1,  1,  2,  2),  !     (T P -)	T := --P
	(kloadincr,	$+1,  1,  1,  2,  2),  !     (T P -)	T := P++
	(kloaddecr,	$+1,  1,  1,  2,  2),  !     (T P -)	T := P--
  
	(kswitch,	$+1,  0,  1,  0,  3),  ! x,y (L L2 c)	switch on c; L=jumptable, L2=else label
	(kswitchu,	$+1,  0,  1,  0,  3),  ! x,y (L L2 c)	switch on c; L=jumptable, L2=else label; unchecked
	(kswlabel,	$+1,  0,  0,  0,  1),  !     (L - -)	label for switch jump table

	(kstop,		$+1,  0,  0,  0,  1),  !     (a - -)
	(klabel,	$+1,  0,  0,  0,  1),  !     (L - -)
  
	(kdata,		$+1,  0,  1,  0,  1),  !
  
	(kloadbit,	$+1,  1,  1,  0,  3),  !     (T b c)	T := b.[c]
	(kloadbf,	$+1,  1,  1,  0,  4),  !     (T b c d)	T := b.[c..d]
	(kstorebit,	$+1,  0,  1,  1,  3),  !	 (P b c)	P.[b] := c
	(kstorebf,	$+1,  0,  1,  1,  4),  !     (P b c d)	P.[b..c] := d
	(kidivrem,	$+1,  2,  1,  0,  4),  !     (T T c d)  (T1, T2) := C divrem d

	(kjumpin,	$+1,  0,  1,  0,  4),  !     (L b c d)  goto L when b in c..d
	(kjumpout,	$+1,  0,  1,  0,  4),  !     (L b c d)  goto L when b not in c..d

	(ksetjmp,	$+1,  0,  1,  0,  1),  !     (a - -)
	(klongjmp,	$+1,  0,  1,  0,  2),  !     (a b -)
	(kgetr0,	$+1,  1,  1,  0,  1),  !     (T - -)    get value of r0 put there by set/longjmp
  
	(kclear,	$+1,  0,  1,  1,  1),  !     (P - -)    clear P
  
	(klast,		$+1,  0,  0,  0,  0),  !

end

export enumdata [0:]ichar ccnames, [0:]ichar ccshortnames =
	(no_cc=0,	"xx",	"?"),
	(eq_cc,		"eq",	" = "),
	(ne_cc,		"ne",	" != "),
	(lt_cc,		"lt",	" < "),
	(le_cc,		"le",	" <= "),
	(ge_cc,		"ge",	" >= "),
	(gt_cc,		"gt",	" > "),
end

export enumdata [0:]ichar idnames
	(null_id=0,		"--"),			!Not set (used for overall program name)
	(import_id,		"Import"),		!Imported symbol (proc or static)
	(proc_id,		"Proc"),		!Local proc
	(static_id,		"Static"),		!Local static
	(local_id,		"Local"),		!Function local var
	(param_id,		"Param"),		!Function param
	(label_id,		"Label"),		!Used in assembly
	(export_id,		"Export"),		!Used by makesymbol, is converted to proc_id/.exported
	(misc_id,		"Misc"),		!?
	(program_id,	"Program"),		!?
end

[]byte callops_d = (kcall, katan2, kpower, kfmod, ksin, kcos, ktan,
	kasin, kacos, katan, klog, klog10, kexp, kpower, kceil, kfloor)

export [tclnames.bounds]byte tclhascall			!has 1 when op may involve a call

proc start=
	for x in callops_d do
		tclhascall[x]:=1
	od
end

!Docs

!TYPEPUN combinations recognised for 64-bit targets:
!  T1 := typepun(sx)                     i64/r32         |<T1: 
!  T3 := typepun(sx)                     u64/r32         |<T3: 
!
!  T1 := typepun(x)                      i64/r64         |<T1: 
!  T3 := typepun(x)                      u64/r64         |<T3: 
!
!  T5 := typepun(a)                      r32/i64         |<T5: 
!  T5 := typepun(u)                      r32/u64         |<T5: 
!
!  T7 := typepun(a)                      r64/i64         |<T7: 
!  T7 := typepun(u)                      r64/u64         |<T7: 
!
!  T1 := typepun(x)                      i64/r64         |<T1: 
!  T3 := typepun(x)                      u64/r64         |<T3: 

!r32 source is typepunnded to 32 bits but is then widened to 64. The alternative
!would have been an untidy extra conversion. Otherwise widths must match.
=== mc_decls_x.m 0 0 7/40 ===
export type mclopnd = ref mclopndrec

export record mclopndrec =
!	ref pstrec labeldef	!nil, or handle of strec for label
	union
		psymbol def
		i64 value		!immediate value
		r64 xvalue	!immediate real value, mainly for dq
		ichar svalue	!immediate string
		int labelno
		int sysfn
		struct
			i32 tempno
			byte lasttemp		!set to 1 if .islast applied to the temp
		end
	end

	u16 misc: (			! bitfields
		size:5,			! one of 1 2 4 8
		scale:4,		! one of 1 2 4 8
		mode:3,			! R, X, imm, [mem]
		valtype:4)

	byte reg			!0, or main register
	byte regix			!0, or index register
	i32 offset			!additional offset to memory operands
end

export type mcl = ref mclrec

export record mclrec = !$caligned
	ref mclrec lastmcl, nextmcl
	mclopnd a,b
	byte c
	byte opcode
	byte cond
	byte spare1
	u32 seqno
	union
		u32 mpos
		u32 lineno				!used by aa assembler
	end
	u32 spare2

!	union
!		[r0..r15]byte regfreed		!1 indicates work-register freed after this instr
!		pair regfreedpr
!	end
end

export enumdata [0:]ichar valtypenames =
	(no_val=0,		$),		!no operand
	(intimm_val,	$),		!immediate int
	(realimm_val,	$),		!immediate real (mainly for dq etc)
	(realmem_val,	$),		!indirect real (for movq etc)
	(stringimm_val,	$),		!immediate string, for comments, or address of string etc
	(def_val,		$),		!var/proc name
	(label_val,		$),		!label index
!	(labelind_val,	$),		!label index
	(name_val,		$),		!immediate string must be output as ah unquoted name
	(temp_val,		$),		!index of pclopnd temp (later becomes ptr to descriptor?)
	(data_val,		$),		!data string
!	(syscall_val,	$),		!
end

export enumdata []ichar mclnames, []byte mclnopnds, []byte mclcodes =

	(m_procstart,		$,		0,		0),		!
	(m_procend,			$,		0,		0),		!
	(m_comment,			$,		0,		0),		!
!	(m_blank,			$,		0,		0),		!
!	(m_deleted,			$,		0,		0),		!
	(m_labelname,		$,		0,		0),		!
	(m_define,			$,		0,		0),		!
	(m_definereg,		$,		0,		0),		!
	(m_definetemp,		$,		0,		0),		!
	(m_trace,			$,		0,		0),		!
	(m_endx,			$,		0,		0),		!

	(m_label,			$,		1,		0),		!
	(m_nop,				$,		0,		0x90),		!
!	(m_param,			$,		1,		0),		!
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
	(m_jmpcc,			$,		1,		0),		!
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
	(m_setcc,			$,		1,		0),		!

	(m_bsf,				$,		2,		0xBC),	!
	(m_bsr,				$,		2,		0xBD),	!

	(m_shld,			$,		2,		0xA4),	!
	(m_shrd,			$,		2,		0xAC),	!

	(m_sqrtss,			$,		2,		0x51),	!
	(m_sqrtsd,			$,		2,		0x51),	!

	(m_addss,			$,		2,		0x58),	!
	(m_addsd,			$,		2,		0x58),	!

	(m_subss,			$,		2,		0x5C),	!
	(m_subsd,			$,		2,		0x5C),	!

	(m_mulss,			$,		2,		0x59),	!
	(m_mulsd,			$,		2,		0x59),	!

	(m_divss,			$,		2,		0x5E),	!
	(m_divsd,			$,		2,		0x5E),	!

	(m_comiss,			$,		2,		0),		!
	(m_comisd,			$,		2,		0x2F),	!
	(m_ucomisd,			$,		2,		0x2E),	!

	(m_xorps,			$,		2,		0x57),	!
	(m_xorpd,			$,		2,		0x57),	!

	(m_andps,			$,		2,		0x54),	!
	(m_andpd,			$,		2,		0x54),	!

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
	(m_minsd,			$,		2,		0x5D),	!
	(m_maxss,			$,		2,		0x5F),	!
	(m_maxsd,			$,		2,		0x5F),	!

	(m_db,				$,		1,		0),		!
	(m_dw,				$,		1,		0),		!
	(m_dd,				$,		1,		0),		!
	(m_dq,				$,		1,		0),		!
	(m_ascii,			$,		1,		0),		!
	(m_asciiz,			$,		1,		0),		!
!	(m_ddoffset,		$,		1,		0),		!

!	(m_segment,			$,		1,		0),		!
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
	(m_bswap,			$,		1,		0),		!

	(m_finit,			$,		0,		0),		!

	(m_fldz,			$,		0,		0xEE),	!
	(m_fld1,			$,		0,		0xE8),	!
	(m_fldpi,			$,		0,		0xEB),	!
	(m_fld2t,			$,		0,		0xE9),	!
	(m_fld2e,			$,		0,		0xEA),	!
	(m_fldlg2,			$,		0,		0xEC),	!
	(m_fldln2,			$,		0,		0xED),	!

	(m_cpuid,			$,		0,		0),		!

	(m_xxxx,			$,		0,		0xF4),	!
	(m_halt,			$,		0,		0xF4),	!
end

export enumdata [0:]ichar regnames, [0:]byte regcodes =
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

	(xr0,		$,	0),			!xmm0
	(xr1,		$,	1),
	(xr2,		$,	2),
	(xr3,		$,	3),
	(xr4,		$,	4),
	(xr5,		$,	5),
	(xr6,		$,	6),
	(xr7,		$,	7),
	(xr8,		$,	8),
	(xr9,		$,	9),
	(xr10,		$,	10),
	(xr11,		$,	11),
	(xr12,		$,	12),
	(xr13,		$,	13),
	(xr14,		$,	14),
	(xr15,		$,	15),

end

export const rframe = r14
export const rstack = r15

export enumdata [0:]ichar condnames, [0:]ichar asmcondnames,
		[0:]int asmrevcond =

	(ov_cond=0,	"ov",	"o",		nov_cond),
	(nov_cond,	"nov",	"no",		ov_cond),

	(ltu_cond,	"ltu",	"b",		geu_cond),
	(geu_cond,	"geu",	"ae",		ltu_cond),

	(eq_cond,	"eq",	"z",		ne_cond),
	(ne_cond,	"ne",	"nz",		eq_cond),

	(leu_cond,	"leu",	"be",		gtu_cond),
	(gtu_cond,	"gtu",	"a",		leu_cond),

	(s_cond,	"s",	"s",		ns_cond),
	(ns_cond,	"ns",	"ns",		s_cond),

	(p_cond,	"p",	"p",		np_cond),
	(np_cond,	"np",	"np",		p_cond),

	(lt_cond,	"lt",	"l",		ge_cond),
	(ge_cond,	"ge",	"ge",		lt_cond),

	(le_cond,	"le",	"le",		gt_cond),
	(gt_cond,	"gt",	"g",		le_cond),

	(flt_cond,	"flt",	"b",		fge_cond),		!special floating point codes
	(fge_cond,	"fge",	"ae",		flt_cond),
	(fle_cond,	"fle",	"be",		fgt_cond),
	(fgt_cond,	"fgt",	"a",		fle_cond)
end

global const z_cond = eq_cond
global const nz_cond = ne_cond

export enumdata [0:]ichar segmentnames =
	(no_seg=0,		$),
	(code_seg,		$),
	(idata_seg,		$),
	(zdata_seg,		$),
	(rodata_seg,	$),
	(impdata_seg,	$),
end

export enumdata [0:]ichar reftypenames =
	(extern_ref=0,		$),		!is external
	(fwd_ref,			$),		!not yet reached
	(back_ref,			$),		!has been reached
end

export enumdata [0:]ichar opndnames_ma =
	(a_none=0,	$),
	(a_reg,		$),		! Ri
	(a_imm,		$),		! d including def name, label etc
	(a_mem,		$),		! any memory modes: [d], [R], [R*4+R2+d+imm] etc
	(a_cond,	$),		! a condition code for jcc/setcc
	(a_xreg,	$),		! xmm register
end

global enumdata [0:]ichar pmcnames =
	(pmc_ignore=0,	"Ignore"),
	(pmc_stack,		"Stack"),
	(pmc_reg,		"Reg"),
	(pmc_spill,		"Spill"),
	(pmc_move,		"Move"),
end	

global enumdata [0:]ichar locnames =
	(no_loc=0,	"unused"),				!temp not in use yet, or retired
	(reg_loc,	"reg"),					!temp in use in given register
	(mem_loc,	"mem"),					!temp in use but spilled to memory
	(mult_loc,	"mult"),				!multi-temp in used but not using reg or mem
end


!global [r0..r15]byte workregs, workxregs		!1 indicates available work regs
!global int nworkregs, nworkxregs				!no. workregs assigned
!global int nregvars, nxregvars					!no. reg vars allocated (consec regs)
!global int maxregvars, maxxregvars				!no. reg vars available
!
!global int xregmax
!

!global [r0..r15]byte regset			!register in-use flags: 0/1: free/in-use
!global [r0..r15]byte xregset		!same for xregs
!
!global [r0..r15]byte isregvar
!global [r0..r15]byte isxregvar
!
!global record pair =
!	u64 low, high
!end

!global pair regsetpr @ regset
!global pair isregvarpr @ isregvar
!global const u64 invertbytes = 0x0101'0101'0101'0101
!
global [r0..r15]byte usedregs		!1 means used during proc
global [r0..r15]byte usedxregs		!1 means used during proc

global byte noxorclear		!1 to suppress xor optimisation

!global macro zz = noperands
!global macro yy = noperands-1
!global macro xx = noperands-2
!global macro ww = noperands-3

!global const maxcalldepth=32
!global [maxcalldepth]byte callalign		!pending 1-slot alignment for syscalls
!global [maxcalldepth]byte callblockret	!1 if fnc returns a block
!global [maxcalldepth]u32 callblocksize	!size of any returned block
!global [maxcalldepth,4]u32 callargsize	!size of any block pushed in low args
!global int ncalldepth
!
global int lababs32, lababs64
global int labneg32, labneg64
global int labmask63, laboffset64
global int labzero
global int kk0used=0

export ref mclrec mccode, mccodex		!genmc adds to this linked list

global int currsegment=0

global mclopnd dstackopnd
global mclopnd dframeopnd

global [r0..r15,1..8]mclopnd regtable

global [-128..64]mclopnd frameregtable

global record constrec =
	union
		int value
		real xvalue
		ichar svalue
	end
	ref constrec nextconst
	int labelno
end

global ref constrec cstringlist
global ref constrec vstringlist
global ref constrec creallist
global ref constrec cr32list

!global psymbol currasmproc

global int lab_funcnametable
global int lab_funcaddrtable
global int lab_funcnprocs

export record relocrec =			!informal version
	ref relocrec nextreloc
	int reloctype
	int offset
	int stindex
end

!record used for expanding buffers. Expansion is not automatic: buffercheck(n)
!is needed at strategic points to ensure that are at least n bytes left
export record dbuffer =
	ref byte pstart
	union
		ref byte pcurr
		ref u16 pcurr16
		ref u32 pcurr32
		ref u64 pcurr64
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

global const init_ss_symbols=32768				!exported to coff
global ref []psymbol ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize

global ref[]psymbol labeldeftable

global int aaseqno
global int aapos

!The following are highly dependent on the ordering of the base types being:
! r32 r64 ints... block ..., with r32 having value 1
!They assume mode is not void, and for ispfloat, is not a block

global macro ispwide(m)  = m - 1
global macro ispfloat(m) = m <= tpr64
global macro ispint(m)   = m > tpr64	!when block type is not expected

EXPORT [1..8]byte regmodes=(tpu8, tpu16, 0, tpu32, 0,0,0, tpu64)

global int pmode
global tcl currtcl

global ref mclrec mclprocentry
global ref mclrec mce_oldmccodex, mce_lastmcl, mce_nextmcl		!used by reset/setmclentry
global ref mclrec mcf_oldmccodex, mcf_lastmcl, mcf_nextmcl		!used by reset/setmclentry for frame setup

!global byte fpshortnames
global byte fpcheckunusedlocals
!export byte phighmem

global record riprec =
	ref riprec next
	u32 offset			!within code segment, offset of d32 field
	i32 immsize			!0,1,4 bytes of trailing imm field
end

!global record fwdrec =
!	ref fwdrec nextfwd
!	i32 offset
!	i16 reltype
!	i16 seg
!end

global ref riprec riplist

!export ref proc (ref void) idomcl_assem
!export ref func (ref void)int icheckasmlabel
!export ref func (int)psymbol igethostfn

global const maxblocktemps=50
global [maxblocktemps]psymbol blockdefs
global int nblocktemps

global []int multregs=(r0,r1,r2,r10,r11,r12)
global []int multxregs=(r0,r1,r2,r3,r4,r5)

global u64 workset						!has .[reg]=1 when in use as work reg or temp
global u64 tempset						!has .[reg]=1 when in use as temp

global int nextworkreg, nextworkxreg		!per instruction
global int highworkreg, highworkxreg		!per function

global [maxfixedtemp]byte temploc			!loc-code of temp T
global [maxfixedtemp]byte tempreg			!reg-code of temp T when in loc is reg_loc or mult
global [maxfixedtemp]byte tempspilled		!1 when temp is spilled
!global [maxfixedtemp]i32 tempoffset			!for basic codegen
!global [maxfixedtemp]byte tempmode			!
!global [maxfixedtemp]u32 tempsize			!

global ref[]byte ptemploc, ptempreg			!will point to above, or heap allocated versions

global [pstdnames.bounds]byte ploadopx

global [pstdnames.bounds]byte ploadop

proc start=
	for i in ploadop.bounds do ploadop[i]:=m_nop od

	ploadop[tpu8]:=ploadop[tpu16]:=ploadop[tpu32]:=m_movzx
	ploadop[tpi8]:=ploadop[tpi16]:=ploadop[tpi32]:=m_movsx
	ploadop[tpr32]:=m_movd
	ploadop[tpr64]:=m_movq
	ploadop[tpu64]:=ploadop[tpi64]:=m_mov
end

GLOBAL INT REGVARA
GLOBAL INT XREGVARA

GLOBAL INT WORKREGA
GLOBAL INT WORKREGB
GLOBAL INT WORKXREGA
GLOBAL INT WORKXREGB
=== mc_lib_x.m 0 0 8/40 ===
!const fuseregtable=1
!const fuseregtable=0

global const targetsize=8

export const ctarget=0

!global int mclseqno
EXPORT int mclseqno
EXPORT int NMCLOPND

global int mstackdepth

[-1..10]mclopnd smallinttable
[20]psymbol nametable
int nnametable

global macro isframex(d) = (d.id in [local_id, param_id])

export proc mclinit(int bypass=0)=
	mclopnd a
	int r, s

	if mclrec.bytes>64 then ABORTPROGRAM("MCLREC>64B") fi

	for r:=r0 to r15 do
		regtable[r, 1]:=mgenreg0(r, 1)
		regtable[r, 2]:=mgenreg0(r, 2)
		regtable[r, 4]:=mgenreg0(r, 4)
		regtable[r, 8]:=mgenreg0(r, 8)
	od

	for i in frameregtable.bounds do
		a:=newmclopnd()
		a.mode:=a_mem
		a.reg:=rframe
		a.size:=8
		a.offset:=i
		frameregtable[i]:=a
	end

	dframeopnd:=mgenreg(rframe, tpu64)
	dstackopnd:=mgenreg(rstack, tpu64)

	initmcdest()

	setsegment('C')

	lab_funcnametable:=0
	lab_funcaddrtable:=0

	for i in smallinttable.bounds do
		smallinttable[i]:=mgenint0(i, 8)
	od

!bypass is used when directly using mcl api (eg. from an external assembler)
!then genmcl(), called from pcl functions, is a no-op
	if bypass then
		mcldone:=1
	fi
end

global proc initmcdest=
!reset mccode/mccodex
!called should have saved any values from last linked list 
	mccode:=mccodex:=nil
!	clear rtsproclabels
end

EXPORT proc genmc(int opcode, mclopnd a=nil, b=nil)=		!used in do_mcl/assem in host
	ref mclrec m, oldm
	int labno

	m:=pcm_allocnfz(mclrec.bytes)

	m.opcode:=opcode
	m.seqno:=++mclseqno
	m.mpos:=mmpos

	m.a:=a
	m.b:=b

	case opcode
	when m_lea then
		if b and b.valtype=def_val then
			b.def.addrof:=1
		fi
	when m_label then
		labno:=a.labelno

	when m_mov then				!change to movd/q if needed
		if a.mode=a_xreg or (b and b.mode=a_xreg) then
			m.opcode:=(a.size=8|m_movq|m_movd)
		fi
	esac

	if mccode then
		m.lastmcl:=mccodex
		mccodex.nextmcl:=m
		mccodex:=m
	else
		mccode:=mccodex:=m
	fi
end

export proc genmc_cond(int opcode, cond, mclopnd a=nil, b=nil)=
	genmc(opcode, a, b)
	mccodex.cond:=cond
end

export proc genmc_label(int opcode, labelno)=
	genmc(opcode, mgenlabel(labelno))
end

global proc genmc_string(int opcode, ichar s)=
!as genmc but uses a single immediate string operand
	genmc(opcode, mgenstring(s))
end

global proc genmc_def(int opcode,  psymbol d)=
!as genmc but uses a single immediate string operand
	genmc(opcode, mgenmem(d))
end

global proc genmc_defaddr(int opcode,  psymbol d)=
!as genmc but uses a single immediate string operand
	genmc(opcode, mgenmemaddr(d))
end

global proc genmc_int(int opcode,  int a)=
!as genmc but uses a single immediate string operand
	genmc(opcode, mgenint(a))
end

global proc genmc_name(int opcode,  ichar name)=
!as genmc but uses a single immediate string operand
	genmc(opcode, mgenname(name))
end

func newmclopnd:mclopnd a=
!	a:=pcm_allocz(mclopndrec.bytes)
	a:=pcm_allocnfz(mclopndrec.bytes)

++NMCLOPND
	return a
end

global func duplopnd(mclopnd a)mclopnd=
	mclopnd b
!	b:=pcm_alloc(mclopndrec.bytes)
	b:=pcm_allocnfz(mclopndrec.bytes)
	b^:=a^
	return b
end

EXPORT func mgenindex(int areg=0, ireg=0, scale=1, offset=0, size=0, labno=0, psymbol def=nil)mclopnd=
!construct a mem address mode
	mclopnd a
	a:=newmclopnd()

	a.mode:=a_mem
	a.reg:=areg

	if areg=rframe or ireg=rframe then usedregs[rframe]:=1 fi

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
			usedregs[rframe]:=1
		fi
	fi

	return a
end

global proc mcomment(ichar s)=
!if not debugmode then return fi
!	if s=nil or s^=0 then
!		genmc(m_blank)
!	else
		genmc_string(m_comment, s)
!	fi
end

export func mgenstring(ichar s, int length=-1)mclopnd=
	mclopnd a
	a:=newmclopnd()
	a.mode:=a_imm

	if length<0 then
		length:=strlen(s)
	fi

	a.svalue:=pcm_alloc(length+1)
	memcpy(a.svalue, s, length)
	(a.svalue+length)^:=0

	a.valtype:=stringimm_val
	a.size:=8
	return a
end

export func mgendata(ref byte p, int length)mclopnd=
	mclopnd a
	a:=newmclopnd()
	a.mode:=a_imm
	a.svalue:=pcm_copyheapstringn(p, length)

	a.valtype:=data_val
	a.size:=length
	return a
end

global func mgenname(ichar s)mclopnd=
	[64]char str
	mclopnd a
	a:=newmclopnd()
	a.mode:=a_imm
	a.svalue:=pcm_copyheapstring(s)
	a.valtype:=name_val
	a.size:=8

	return a
end

global proc setsegment(int seg, align=1)=
!seg is 'D', 'Z', 'C', 'R' for data, zdata, code, rdata
	int opc, oldalign

	if seg<>currsegment then
		case seg
		when 'I' then opc:=m_isegment
		when 'Z' then opc:=m_zsegment
		when 'C' then opc:=m_csegment
		when 'R' then MERROR("CAN'T DO RODATA SEG")
		else
			MERROR("BAD SEG CODE")
		esac
		if mccodex and mccodex.opcode in [m_isegment, m_zsegment, m_csegment] then
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
		genmc(m_align, mgenint(align))
	fi
end

global func changeopndsize(mclopnd a, int size)mclopnd=
	mclopnd b

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

global func applyoffset(mclopnd a, int offset, int size=0)mclopnd=
!astr is an asm operand
!add possible byte offset
	mclopnd b

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

export func mgenint(i64 x, int mode=tpi64)mclopnd a=
	int size:=psize[mode]

	if x in -1..10 and size=8 then
		return smallinttable[x]
	fi

	a:=newmclopnd()
	a.mode:=a_imm

	a.value:=x
	a.valtype:=intimm_val
	a.size:=size

	return a
end

global func mgenint0(i64 x, int size=8)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_imm

	a.value:=x
	a.valtype:=intimm_val
	a.size:=size

	return a
end

global func mgenrealmem(r64 x, int mode=tpr64)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_mem

	if ispwide(mode) then
		a.value:=getrealindex(x)
	else
		a.value:=getr32index(x)

	fi
	a.valtype:=label_val
	a.size:=psize[mode]
	return a
end

export func mgenrealimm(r64 x, int mode=tpr64)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_imm
	a.xvalue:=x
	a.valtype:=realimm_val
	a.size:=psize[mode]
	return a
end

EXPORT func mgenlabel(int x=0)mclopnd a=
!x is a label index
!generate immediate operand containing label
	a:=newmclopnd()
	a.mode:=a_imm

	if x=0 then x:=++mlabelno fi
	a.value:=x
	a.valtype:=label_val
	a.size:=8

	return a
end

global func mgenlabelmem(int x)mclopnd a=
!x is a label index
!generate immediate operand containing label

	a:=mgenlabel(x)
	a.mode:=a_mem
	return a
end

export func mgenmem(psymbol d, int mode=tpu64)mclopnd a=
	int reg

	if d.reg then
		if pfloat[d.mode] then
			return mgenxregvar(d)
		else
			return mgenregvar(d, mode)
		fi
	fi

	reg:=rnone
	if isframex(d) then
!		if not foptim and (int(d.offset) in -128..64) and ttsize[d.mode]=8 then
!			return frameregtable[d.offset]
!		fi

		reg:=rframe
		usedregs[rframe]:=1

	fi

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.def:=d
	++d.nrefs
	a.valtype:=def_val

	if mode then
		a.size:=psize[mode]
	else
		a.size:=min(d.size, 8)
	fi
	if a.size=0 then a.size:=8 fi

	return a
end

EXPORT func mgenmemaddr(psymbol d)mclopnd=
	mclopnd a

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

global func mgenreg0(int reg, size=8)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_reg
	a.reg:=reg
	a.size:=size

IF SIZE=0 THEN MERROR("1:SIZE=0") FI
	return a
end

EXPORT func mgenxreg(int xreg, size=8)mclopnd=
	mclopnd a

!	if xreg=rnone then xreg:=++currxregno fi
	a:=newmclopnd()

	a.mode:=a_xreg
	a.reg:=xreg
	a.size:=size
IF SIZE=0 THEN MERROR("2:SIZE=0") FI
	return a
end

EXPORT func mgenreg(int reg, mode=tpi64)mclopnd a =
!EXPORT func mgenreg(int reg, mode)mclopnd a =
	int size:=psize[mode]

!	if pfloat[mode] and reg>=xr0 then
	if reg>=xr0 then
		a:=newmclopnd()
		a.mode:=a_xreg
		a.reg:=reg
		a.size:=psize[mode]
		a
	else
		if size=0 then size:=8 fi
		usedregs[reg]:=1

!IF REG IN R10..R13 THEN REGSET[REG]:=1 FI

!		if fuseregtable then
!			return regtable[reg, size]
!		fi
		return mgenreg0(reg, size)
	fi
end

!global func mgenregi(int reg, mode=tpi64)mclopnd a =
!!	if fuseregtable then
!!		return regtable[reg, psize[mode]]
!!	fi
!	return mgenreg0(reg, psize[mode])
!end

global func mgenireg(int reg, mode=tpi64, offset=0)mclopnd=
	mclopnd a

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.size:=psize[mode]
	a.offset:=offset

	return a
end

global func mgentemp(int n, mode=tpu64)mclopnd a=
!pcl temps are used to spill pcl operands from a register
!they will always be 64 bits

!	int reg, size
!
!	if pcltempflags[n] then			!already in use
!		return changeopndsize(pcltempopnds[n], psize[mode])
!	fi
!
	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=rframe
!	usedregs[rframe]:=1
	a.valtype:=temp_val
	a.size:=psize[mode]
	a.tempno:=n
	a.offset:=currfunc.tempmodes[n].offset
!
!	pcltempopnds[n]:=a
!	pcltempflags[n]:=1

	return a
end

global func roundsizetg(int size)int=
!make sure size is round up to next multiple of targetsize (4 or 8)
	if size iand 7=0 then return size fi
	return size+(8-(size iand 7))
end

global proc merroropnd(ichar mess, int opndtype)=
	fprintln "MCL Opnd not supported: # (#) [#]", mess, opndnames[opndtype]
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mmpos>>24], mmpos iand 16777215)
end

global func mcreatefwdlabel:int =
	return ++mlabelno
end

global proc mdefinefwdlabel(int lab) =
	genmc(m_label, mgenlabel(lab))
end

global func mgenextname(ichar s)mclopnd=
	[64]char str
	psymbol d
	static [20]psymbol table
	static int ntable

	strcpy(&.str, s)
	str[strlen(s)]:=0			!lose final *

	d:=findnamesym(str)

	if not d then
		d:=pcm_allocnfz(pstrec.bytes)

		d.name:=pcm_copyheapstring(&.str)
		d.id:=import_id
		d.imported:=1
		addnamesym(d)
	fi

	return mgenmemaddr(d)
end

global func mgenregvar(psymbol d, int mode)mclopnd a=
	a:=mgenreg(d.reg, mode)
!	isregvar[d.reg]:=1

	return a
end

global func mgenxregvar(psymbol d)mclopnd a=
	a:=mgenxreg(d.reg)
!	isxregvar[d.reg]:=1

	return a
end

global func getprimreg(mclopnd ax)int =
!get primary reg value; only one should be active
!return 0 if no regs
!//error if both regs are active

	if ax.reg then
!		if ax.regix then merror("getprim?") fi
		ax.reg
	else
		ax.regix	!0 if no regs used
	fi
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
		genmc(m_sub, dstackopnd, mgenint(n))
	fi
end

global proc popstack(int n)=
	if n then
		genmc(m_add, dstackopnd, mgenint(n))
	fi
end

global func getstringindex(ichar s)int=
	if s=nil then			!assume nil
		kk0used:=++mlabelno
		return kk0used
	fi

	if cstringlist and eqstring(cstringlist.svalue, s) then
		return cstringlist.labelno
	fi

	return addconst(cstringlist, cast(s))
end

global func addconst(ref constrec &clist, int value)int=
	ref constrec p
	p:=pcm_allocnfz(constrec.bytes)
	p.value:=value
	p.labelno:=++mlabelno
	p.nextconst:=clist
	clist:=p
	return mlabelno
end

global func getrealindex(real x)int=
	return addconst(creallist, cast@(x, int))
end

global func getr32index(real x)int=
	return addconst(cr32list, cast@(x, int))
end

!global func ispoweroftwo(i64 x)int=
EXPORT func ispoweroftwo(i64 x)int=
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

global proc axerror(ichar mess)=
	CPL "AX ERROR:", mess, "AASEQ:", aaseqno
	CPL
	STOP 1

end

global func newblocktemp(int size)psymbol=
	[16]char str
	psymbol d

	if nblocktemps>maxblocktemps then
		merror("Too many block temps")
	fi
	++nblocktemps

	fprint @str, "$B#", nblocktemps

MERROR("NEWBLOCKTEMP")

!	d:=pc_makesymbol(str, misc_id)
	d.mode:=tpblock
	d.size:=size
	d.used:=1
	d.id:=local_id
	d.nextlocal:=currfunc.nextlocal
!*! 	d.owner:=currfunc
	currfunc.nextlocal:=d

	blockdefs[nblocktemps]:=d
	d
end

global func findnamesym(ichar s)psymbol d=
!search for s in cache of named symbols

	for i to nnametable do
		if eqstring(s, nametable[i].name) then
			return nametable[i]
		fi
	od
	nil
end

global proc addnamesym(psymbol d)=
!add new name symbol, which should be unique

	if nnametable<nametable.len then
		nametable[++nnametable]:=d
	else
		merror("Ext nametab overflow")
	fi
end

export proc callproc(ichar cpname, name, int lineno)=
RETURN
end

func mgenstringx(ichar s)mclopnd=
	mgenlabelmem(getstringindex(s))
end

global proc clearreg(mclopnd ax)=
	if ax.size=8 then
		ax:=changeopndsize(ax, 4)
	fi
	genmc(m_xor, ax, ax)
end

global proc genstringtable=
	ref constrec p

	return unless cstringlist

	mcomment("String Table")

	setsegment('I', 8)

!	if kk0used then
!		genmc(m_label, mgenlabel(kk0used))
!		gendb(0)
!	fi

	p:=cstringlist
	while p, p:=p.nextconst do
		genmc_label(m_label, p.labelno)
		genstring_db(p.svalue, strtype:0)
	od
end

global proc mcomm(ichar s, t="", u="")=
	[256]char str
	print @str, s, t, u
	mcomment(pcm_copyheapstring(str))
end

global proc genstring_db(ichar s, int length=-1, strtype)=
!string table generated in ax pass, so is just text
!this is target-specific, so should really be moved
!strtype should be zero for a normal string, then a zero-terminator is added.
	int i, c, seqlen
	ref char seq

	if length=-1 then
		length:=strlen(s)
	fi

	if length=0 then
		gendb(0)
		return
	fi

	seqlen:=0

	to length do
		c:=s++^
!		if c<32 or c>=127 or c='\"' then
		if c<32 or c>=127 or c in ['\"', '\\'] then
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
	if strtype=0 then
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

global proc genrealtable=
	ref constrec p

	return unless creallist or cr32list

	mcomment("Real Table")
	setsegment('I',8)
	p:=creallist
	while p, p:=p.nextconst do
		genmc(m_label,mgenlabel(p.labelno))

		if p.xvalue=infinity then
			genmc(m_dq, mgenint(u64@(p.xvalue)))
		else
			genmc(m_dq, mgenrealimm(p.xvalue,tpr64))
		fi
	od

	mcomment("Real32 Table")
	p:=cr32list
	while p, p:=p.nextconst do
		genmc(m_label,mgenlabel(p.labelno))
		if p.xvalue=infinity then
			genmc(m_dd, mgenint(int@(r32(p.xvalue))))
		else
			genmc(m_dd, mgenrealimm(p.xvalue,tpr32))
		fi

	od
end

=== mc_asm_x.m 0 0 9/40 ===
!const fshowseq=1
const fshowseq=0

!const showsizes=1
const showsizes=0

!const showfreed=1
const showfreed=0

[r0..xr15]psymbol regvars		!nil, or strec when it uses that reg

export int assemtype='AA'

proc writemcl(int index, ref mclrec mcl)=
	if mcl.opcode=m_comment and mcl.a.svalue^='?' then
	else
		strmcl(mcl)
		gs_line(pdest)
	fi
end

global proc strmcl(ref mclrec mcl)=
	static [512]char str
	[128]char opcname
	mclopnd a, b
	int opcode, cond, sizepref
	psymbol d

	opcode:=mcl.opcode
	str[1]:=0

	cond:=mcl.cond
	a:=mcl.a
	b:=mcl.b

!CPL "STRMCL", MCLNAMES[OPCODE], A, B

	case opcode
	when m_procstart then

		asmstr(";Proc ")
		asmstr(a.def.name)
		currfunc:=a.def
		clear regvars

		return

	when m_procend then
		asmstr(";End\n")
		currfunc:=nil

		return

	when m_comment then
		if a.svalue^ then
			asmchar(';')
			asmstr(a.svalue)
		fi
		return
	when m_endx then
		return

	when m_labelname then				!label name will be complete and will have colon(s)
		d:=a.def
		case a.valtype
		when def_val then
			asmstr(getdispname(d))
		when stringimm_val then
			asmstr(a.svalue)
			return
		else
			merror("strmcl/lab")
		esac

		asmstr(":")

		if d.exported then
			if eqstring(getbasename(d.name), d.name) then
				asmstr(":")
			else
				asmstr("\n`")
				asmstr(getbasename(d.name))
				asmstr("::")
			fi
		fi

!ASMSTR(" ")
!ASMSTR(STRINT(INT(D), "H"))

		return

	when m_label then
		if a.valtype=label_val then
			fprint @str, "L#:", a.value
		else
			recase m_labelname
		fi
		asmstr(&.str)
		return

	when m_define then
		d:=a.def
		asmstr("    ")
		asmstr(getdispname(d))
		asmstr(" = ")
		asmint(d.offset)
		return

	when m_definereg then
		d:=a.def
		asmstr("    ")
		asmstr(getdispname(d))
		regvars[d.reg]:=d

		asmstr(" = ")
		asmstr(getregname(d.reg, d.size))
		return

	when m_definetemp then
		asmstr("    ")
		asmstr(gettempname(currfunc, a.tempno))
		asmstr(" = ")
		asmint(a.offset)
		return

	when m_asciiz then
		asmstr("    db ")
		asmstr(mstropnd(a))
		asmstr(", 0")
		return


	esac

	case opcode
	when m_jmpcc then
		print @&.opcname, "j", ,asmcondnames[cond]

	when m_setcc then
		print @&.opcname, "set", ,asmcondnames[cond]

	when m_cmovcc then
		print @&.opcname, "cmov", ,asmcondnames[cond]

	when m_and then
		strcpy(&.opcname, "and")
	when m_or then
		strcpy(&.opcname, "or")
	when m_xor then
		strcpy(&.opcname, "xor")
	when m_not then
		strcpy(&.opcname, "not")

	ELSIF OPCODE>M_HALT THEN
		STRCPY(&.OPCNAME, STRINT(OPCODE))

	else
		strcpy(&.opcname, mclnames[opcode]+2)
	esac

	ipadstr(&.opcname, (opcode=m_dq|4|10), " ")
	ipadstr(&.str, 4)

	strcat(&.str, &.opcname)

	asmstr(&.str)

	if a and b then		!2 operands
		sizepref:=needsizeprefix(opcode, a, b)
!
		asmopnd(a, sizepref)
		asmstr(", 	")
		asmopnd(b, sizepref)

		if mcl.c then
			asmstr(", ")
			asmstr(strint(mcl.c))
		fi

	elsif a and a.mode then								!1 operand
		if opcode=m_call then
			asmopnd(a, 0, opcode)
		else
			asmopnd(a, 1, opcode)
		fi
	fi

	if showsizes then
		if a then
			asmstr("  ; ")
			asmstr(strint(a.size))
			if b then
				asmstr("/")
				asmstr(strint(b.size))
			fi
		fi
	fi

	if fshowseq then
		asmstr(" ; ")
		asmstr(strint(mcl.seqno, "z5"))
	fi

end

global func strmclstr(ref mclrec m)ichar=
	gs_init(pdest)
	strmcl(m)
	return pdest.strptr
end

global func mstropnd(mclopnd a, int sizeprefix=0, opcode=0)ichar=
	static [512]char str
	[128]char str2
	ichar plus, t
	int offset, tc

	str[1]:=0

	case a.mode
	when a_reg, a_xreg then
		return strreg(a.reg, a.size)

	when a_imm then
		if opcode=m_dq and a.valtype=intimm_val then
			if a.value in 0..9 then
				strcat(&.str, strint(a.value))
			else
				strcat(&.str, "0x")
				strcat(&.str, strword(a.value, "H"))
			fi
		else
			strcpy(&.str, strvalue(a))
		fi

	when a_mem then
		case a.valtype
		when intimm_val then
			strcpy(&.str, strint(a.value))
		when realimm_val then
			strcpy(&.str, strreal(a.xvalue))
		when realmem_val then
			fprint @&.str, "M#", a.xvalue
		esac

		strcat(&.str, getsizeprefix(a.size, sizeprefix))
		strcat(&.str, "[")

		plus:=""
		if a.reg then
			strcat(&.str, strreg(a.reg, 8))
			plus:=" + "
		fi
		if a.regix then
			strcat(&.str, plus)
			strcat(&.str, strreg(a.regix, 8))
			plus:=" + "

			if a.scale>1 then
				strcat(&.str, "*")
				strcat(&.str, strint(a.scale))
			fi
		fi

		if a.valtype in [def_val, label_val, temp_val] then
			if plus^ then
				strcat(&.str, plus)
			fi
			strcat(&.str, strvalue(a))
	    elsif offset:=a.offset then
			print @&.str2, offset:" + "
			strcat(&.str, &.str2)
		fi
		strcat(&.str, "]")

	else
		println "BAD OPND", A.MODE
		return "<BAD OPND>"
	esac

	return &.str
end

global func strvalue(mclopnd a)ichar=
	static [512]char str
	[128]char str2
	psymbol def
	i64 value, offset, length
	ichar ss

	def:=a.def
	value:=a.value

	strcpy(&.str, "")

	case a.valtype
	when def_val then
		strcat(&.str, getdispname(def))

	addoffset:
		if offset:=a.offset then
			print @&.str2, (offset>0|"+"|""), ,offset
			strcat(&.str, &.str2)
		fi

!STRCAT(STR, " ")
!STRCAT(STR, STRINT(INT(DEF), "H"))


	when intimm_val then
		strcat(&.str, strint(value))

	when realimm_val then
		print @&.str, a.xvalue:"20.20"

	when realmem_val then
		strcat(&.str, "M")
		strcat(&.str, strreal(a.xvalue))

	when stringimm_val then
		strcat(&.str, """")
		strcat(&.str, a.svalue)
		strcat(&.str, """")

	when name_val then
		strcat(&.str, a.svalue)

	when label_val then
		strcat(&.str, "L")
		strcat(&.str, strint(a.labelno))
		goto addoffset

	when temp_val then
		return gettempname(currfunc, a.tempno)

	else
		merror("Stropnd?")
	esac

	return &.str
end

global proc asmopnd(mclopnd a, int sizeprefix=0, opcode=0)=
	asmstr(mstropnd(a, sizeprefix, opcode))
end

global func getregname(int reg, size=8)ichar=
	static [1..17]ichar prefix=("B", "W", "", "A", "", "", "", "D", "", "", "", "", "", "", "", "Q", "N")
	static [32]char str
	[16]char str2
	ichar rs
	int size2

!	if useintelregs then
!		return nregnames[size, reg]
!	fi

	size2:=size
	if size2>16 then
		size2:=17
	fi

	case reg
	when rnone then return "-"
	when rframe then rs:="fp"
	when rstack then rs:="sp"
	elsif reg>=xr0 then
		print @str, "XMM", ,reg-xr0
		return str

	else
		getstrint(reg-r0, &.str2)
		rs:=&.str2
	esac

	print @&.str, prefix[size2], ,rs
	return &.str
end

proc asmstr(ichar s)=
	gs_str(pdest, s)
end

proc asmint(int a)=
	asmstr(strint(a))
end

proc asmchar(int c)=
	gs_char(pdest, c)
end

global func getdispname(psymbol d)ichar=
	static [256]char str

	if d.reg then
		fprint @str, "##R.#", (fpshortnames|""|"`"), (pfloat[d.mode]|"X"|""),
			 (fpshortnames|d.name|getfullname(d))
		return str
	fi

	if fpshortnames then
		return getbasename(d.name)
	else
		return getfullname(d, backtick:1)
	fi
end 

global func gettempname(psymbol d, int n)ichar=
	static [128]char str

	if fpshortnames or d=nil then
		print @str, "T", ,n
	else
		fprint @str, "#.$T#", getdispname(d), n
	fi
	str
end

global func strreg(int reg, size=8)ichar=
	psymbol d

	d:=regvars[reg]

	if d and psize[d.mode]=size then
		return getdispname(d)
	fi
	getregname(reg, size)
end

export func getassemstr:ref strbuffer=
!write all mcl code in system by scanning all procs
!mcl code is only stored per-proc
	psymbol d, e
	ref mclrec m
	[32]char str2, str3
	int i

	gs_init(pdest)
!
	case phighmem
	when 1 then asmstr("    $userip\n")
	when 2 then asmstr("    $highmem\n")
	esac

	m:=mccode
	i:=1
	while m do
		writemcl(i, m)
		++i
		m:=m.nextmcl
	od
	return pdest
end

global func needsizeprefix(int opcode, mclopnd a, b)int=
	case opcode
	when m_movsx, m_movzx, m_cvtsi2ss, m_cvtsi2sd then
		return 1

	when m_cvtss2si, m_cvtsd2si, m_cvttss2si, m_cvttsd2si then
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

global func getsizeprefix(int size, enable=0)ichar=
	static []ichar table=("byte ", "u16 ", "", "u32 ", "","","", "u64 ")
	if not enable or size not in 1..8 then
		""
	else
		table[size]
	fi
end

=== mc_gen_xb.m 0 0 10/40 ===
!const fshowtcl=2
!const fshowtcl=1
const fshowtcl=0

!!const fshowworkregs=1
!const fshowworkregs=0

!global const docalltrace=1
!global const docalltrace=0

GLOBAL INT DEBUG

!global int framebytes, frameoffset, paramoffset

[tclnames.bounds]ref proc(tcl) px_handlertable

[6]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
![6]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

global int mretindex

global proc genmcl(ichar dummy=nil)=

	return when mcldone

!CPL "DOING GENMCL2"

	IF FSHOWTCL THEN CPL "********* ASM HAS TCL INFO *********" FI

!CPL =CURRFUNC

	int tt:=os_clock()

	inithandlers()
	mclinit()
!CPL $LINENO

	mcomment("X64 CODE...")


!CPL $LINENO
	do_statics()
!CPL $LINENO

	mcomment("")
!CPL $LINENO

	do_procs()
!CPL $LINENO

!CPL "Done ConvertTCL"

	genrealtable()
!CPL $LINENO
	genabsneg()
	genstringtable()
!CPL $LINENO

	genmc(m_endx)					!need as buffer in optimiser
	genmc(m_endx)
!CPL $LINENO

	mcldone:=1

	mcltime:=os_clock()-tt
!CPL $LINENO

end

!FUNC CHECKFPUSED(MCLOPND A)int=
!	RETURN 0 WHEN A=NIL
!	if a.reg=rframe or a.regix=rframe then return 1 fi
!	0
!END
!
proc converttcl(tcl p)=

!RETURN WHEN P.OPCODE IN [KCOMMENT]
!IF P.OPCODE NOT IN [KCOMMENT, KLABEL] THEN CPL "    CONV", TCLNAMES[P.OPCODE], STRWORKREGS()FI

	doshowtcl(p) when fshowtcl

	pmode:=p.mode
	currtcl:=p
	mmpos:=p.pos

	pcseqno:=p.seqno

!	IF P.OPCODE NOT IN [KCOMMENT, KLABEL] THEN
!		MCOMMENT("CLEAR REGS")
!		CLEARREGX(R1)
!		FOR R IN R0..R13 WHEN R NOT IN R3..R9 DO
!!			GENMC(M_XOR, MGENREG(R, TPU32), MGENREG(R, TPU32))
!		OD
!	FI


	px_handlertable[p.opcode]^(p)

!	MCOMMENT("CLEAR REG")
!	CLEARREGX(R0, P.OPCODE)
!	CLEARREGX(R1, P.OPCODE)
!	CLEARREGX(R2, P.OPCODE)
!!	CLEARREGX(R9, P.OPCODE)
!	CLEARREGX(R10, P.OPCODE)
!	CLEARREGX(R11, P.OPCODE)
!	CLEARREGX(R12, P.OPCODE)
!	CLEARREGX(R13, P.OPCODE)
!
	nextworkreg:=r0				!reset
	nextworkxreg:=xr4
end

PROC CLEARREGX(INT R, OPCODE)=
	RETURN WHEN OPCODE IN [KCOMMENT, KLABEL, KDATA]
	RETURN WHEN OPCODE IN [KSWLABEL]

	GENMC(M_XOR, MGENREG(R, TPU32), MGENREG(R, TPU32))
END

proc inithandlers=
	static byte initdone=0
	ichar name, s
	int n

	if initdone then return fi

	n:=$getnprocs()

	for i to n do
		name:=$getprocname(i)
		if eqbytes(name, "tx_", 3) then
			for k in tclnames.bounds do
				s:=tclnames[k]
				if s^='k' then ++s fi				!some are kload, others just store
				if eqstring(s, name+3) then
					px_handlertable[k]:=$getprocaddr(i)
					exit
				fi
			else
				merror("Invalid handler name:", name)
			od
		fi
	od

	static [, 2]byte dupltable = (
!!mapping this op   =>  uses same handler as:
		(ktoboolf, 		ktoboolt)
		(kfordown, 		kforup)
		(kdecrto, 		kincrto)
		(kdecrload,		kincrload)
		(kloaddecr,		kloadincr)
		(kswitchu,		kswitch)
!		(kicallp, 		kcallp)
!		(kicallf, 		kcallp)
!
!		(kendmx, 		kresetmx)
!		(ktcproc, 		kproc)
!
!		(kidivto, 		kidiv)
!		(kiremto, 		kirem)
		)


	for i to dupltable.len do
		px_handlertable[dupltable[i, 1]]:=px_handlertable[dupltable[i, 2]]
	end

	for i in px_handlertable.bounds do
		if not px_handlertable[i] then
			px_handlertable[i]:=cast(&unimpl)
		fi
	od

	initdone:=1
end

proc do_statics=
	psymbol p, d

	mcomment("STATICS")

	d:=pstatictable

	while d, d:=d.next do
		do_staticvar(d)
	od

	p:=pproctable

	while p, p:=p.next do

		d:=p.nextstatic

		while d, d:=d.nextstatic do
!CPL "LOCALS", P.NAME, D.NAME
			do_staticvar(d)
		od
	od

end

proc do_staticvar(psymbol d)=
	int size:=d.size
	tcl p

	setsegment((d.code|'I'|'Z'), d.align)
!	genmc_name(m_labelname, d.name)
	genmc_def(m_labelname, d)

!	if d.atvar then
!		return
!	elsif d.code then
	if d.code then
		p:=d.code
		while p, p:=p.next do
!			do_staticdata(p)
!			do_staticdata(p.a)
!			do_staticdata(p.a, P)
			do_staticdata(p.a, P, D)
		od
	else
		genmc_int(m_resb, d.size)
	fi


	mcomment("")
end

proc do_staticdata(tclopnd a, TCL P, PSYMBOL D)=
	static [1..8]byte ops= (m_db, m_dw, 0, m_dd, 0, 0, 0, m_dq)
	[256]CHAR STR

!	FPRINT @str, "DATA: Opnd:(#) D:# P:# A:#  Value:#=", OPNDNAMES[a.optype], 
!		STRPMODE(D.MODE, D.SIZE), 
!		STRPMODE(P.MODE, P.SIZE), 
!		 strpmode(a.mode, a.size), 
!			A.VALUE
!
!!CPL "--DATA:", D.NAME, ":", "D:", STRPMODE(D.MODE, D.SIZE), "P:", STRPMODE(P.MODE, P.SIZE), 
!!	"A:",   STRPMODE(A.MODE, A.SIZE)
!
!	mcomment(str)

!	if p.mode=tpblock then
!		do_blockdata(p)
!		return
!	fi

	case a.optype
	when int_opnd then
		genmc_int(ops[p.size], a.value)
	when real_opnd then
		genmc_int(ops[p.size], u64@(a.xvalue))
	when r32_opnd then
		genmc_int(ops[p.size], u32@(a.xvalue32))

	when string_opnd then
		genmc_label(m_dq, getstringindex(a.svalue))

	when data_opnd then
		do_blockdata(p)
		return
!		genmc(m_ascii)
!		mgendata(a.svalue, p.size)
!
	when memaddr_opnd then
		genmc_defaddr(m_dq, a.def)

	when label_opnd then
		genmc_label(m_dq, a.labelno)

	else
		mcomm("Opnd not ready:", opndnames[a.optype])
	esac
	psline()

end

proc do_blockdata(tcl p) =
	ref byte s
	ref u64 d
	ref byte db@d
	int n,nqwords,nwords,r
	tclopnd a:=p.a

MCOMMENT("BLOCK DATA")

	n:=p.size
	return when n=0

	nwords:=n/8

!	IF P.MODE=TPBLOCK THEN
		d:=cast(a.svalue)
!	ELSE					!1/2/4/8-byte blocks may have u8-64 types
!CPL "POINT TO .VALUE"
!MCOMMENT("BLOCKDATA WITH INT TYPE")
!		d:=cast(&a.value)
!	fi

	to nwords do
		genmc(m_dq, mgenint(d++^))
	od

	r:=n-nwords*8
	to r do
		genmc(m_db, mgenint(db++^))
	od
!	if r then
!
!		genstring_db(cast(d), r, 'B')
!	fi
	MCOMMENT("ENDDATA")
end

proc do_procs=
	psymbol p

	mcomment("FUNCTIONS")

	p:=pproctable

	while p, p:=p.next do

		do_procdef(p)

	od

end

proc do_procdef(psymbol p)=
	tcl pc


	currfunc:=p


	mretindex:=p.retindex
	if mretindex=0 then mretindex:=++mlabelno fi

	setsegment('C', 1)
!	mcomment(" ------------- Proc:", p.name)
	genmc_def(m_procstart, p)
	genmc_def(m_labelname, p)
!	genmc_name(m_labelname, p.name)
!	if p.ismain then
!		genmc_name(m_labelname, "main")
!	fi

	ntemps:=p.maxtemp

	if ntemps<=maxfixedtemp then
		ptemploc:=&temploc
		ptempreg:=&tempreg
		memset(ptemploc, 0, ntemps)
		memset(ptempreg, 0, ntemps)
		memset(&tempspilled, 0, ntemps)

	else
		ptemploc:=pcm_allocz(ntemps)
		ptempreg:=pcm_allocz(ntemps)
	fi

	do_proccode_a()

	mcomment("?>>")
	mclprocentry:=mccodex

	if p.isentry and p.nparams=2 then
		fixmain()
	fi

	pc:=p.code

	while pc, pc:=pc.next do
!		mcomment(" do next tcl code:", tclnames[pc.opcode])
		converttcl(pc)
	od

	if mclprocentry=mccodex then		!empty body: add dummy mcl op
		mcomment("---")					!injection of entry code goes wrong otherwise
	fi

!CPL $LINENO
	do_proccode_b()
!CPL $LINENO
	do_proccode_c()
!CPL $LINENO

	if workset or tempset then
		CPL("WORK/TEMPSET NOT EMPTY")
!		MERROR("WORK/TEMPSET NOT EMPTY")
		MCOMMENT("WORK/TEMPSET NOT EMPTY")
	fi

!	mcomment("End", p.name)
	genmc(m_procend)
!CPL $LINENO
	mcomment("")

	if ntemps>maxfixedtemp then
		pcm_free(ptemploc, ntemps)
		pcm_free(ptempreg, ntemps)
		ptemploc:=&temploc
		ptempreg:=&tempreg
	fi
	currfunc:=nil
!CPL $LINENO
end

global proc start=
	ptemploc:=&temploc
	ptempreg:=&tempreg
end

global proc unimpl(tcl p)=
	[100]char str

	fprint @str, "Unimpl: # (#)", tclnames[p.opcode], strpmode(pmode, p.size)
!	fprint @str, "Unimpl: # (#)", tclnames[p.opcode]

	CPL STR

	mcomment(pcm_copyheapstring(str))
end

proc doshowtcl(tcl p)=
	[1256]char str

	return unless fshowtcl


	case p.opcode
!	when kretproc, kretfn then
	when kcomment, klabel, kswlabel then
!	when klabel then
	else
		IF FSHOWTCL=2 THEN MCOMMENT("") FI
!		strcpy(&.str, "                       ")
		strcpy(&.str, "-"*24)
		strcat(&.str, strtclstr(p))
		mcomment(pcm_copyheapstring(&.str))
		IF FSHOWTCL=2 THEN MCOMMENT("") FI
	esac
end

global proc fixmain=
!d is a main func with 2 params
!convert params to locald, add more locals needed for calling __getmainargs
	psymbol d:=currfunc, e
	psymbol dn, dargs, denv, dinfo
	mclopnd ax

	dn:=d.nextparam
	dargs:=dn.nextparam

!add 2 new locals
!	denv:=tc_makesymbol("$env", local_id)
	denv:=tc_makesymbol("$env", static_id)
	denv.mode:=tpref
	denv.size:=8

!	dinfo:=tc_makesymbol("$info", local_id)
	dinfo:=tc_makesymbol("$info", static_id)
	dinfo.mode:=tpblock
	dinfo.size:=128


	setsegment('Z',8)
	genmc(m_label, mgenmemaddr(dinfo))
	genmc(m_resb, mgenint(128))
	genmc(m_label, mgenmemaddr(denv))
	genmc(m_resb, mgenint(8))
	setsegment('C',1)
	tc_addlocal(denv)
	tc_addlocal(dinfo)

!remove dn/dargs as params

	dn.nextparam:=dargs.nextparam:=d.nextparam:=nil
	d.nparams:=0
	dn.id:=local_id
	dn.used:=1
	dargs.id:=local_id
	dargs.used:=local_id
!
!add them as locals

	tc_addlocal(dargs)
	tc_addlocal(dn)

	genmc(m_push, ax:=mgenreg(r0))
	genmc(m_lea , ax, mgenmem(dinfo))
DINFO.ADDROF:=1
	genmc(m_push, ax)
	genmc(m_sub, dstackopnd, mgenint(32))
	genmc(m_lea,  mgenreg(r10), mgenmem(dn))
DN.ADDROF:=1
	genmc(m_lea,  mgenreg(r11), mgenmem(dargs))
DARGS.ADDROF:=1
	genmc(m_lea,  mgenreg(r12), mgenmem(denv))
DENV.ADDROF:=1
	clearreg(mgenreg(r13))
	genmc(m_call, mgenextname("__getmainargs*"))
!
	genmc(m_sub, dstackopnd, mgenint(48))

!do pcmdskip fixes
	if pcmdskip then
		genmc(m_sub, mgenmem(dn), mgenint(pcmdskip, tpi32))
		genmc(m_add, mgenmem(dargs), mgenint(pcmdskip*8))
	fi

end
=== mc_aux_xb.m 0 0 11/40 ===
!ref mclrec mclframesetup

global int nsaveregs, nsavefregs		!number of integer/float non-vols to be saved
global int nspilled						!spilled int/float registers

global int framesize					!counts bytes in stack frame
!global int framebytes, frameoffset, paramoffset
global int paramstackoffset
global int paramspilloffset

psymbol dblockarg

int nsavedregs, nsavedxregs

global proc do_proccode_a=
	nextworkreg:=r0						!first 3 are volatile
	nextworkxreg:=xr4					!first 2 are volatile

	highworkreg:=nextworkreg			!assume workreg will be used
	highworkxreg:=nextworkxreg

end

global proc do_proccode_b=
! Stack layout (grows downwards)
!	| ...
!	| Pushed arg 6
!	| Pushed arg 5
!	| Shadow space 32-byte		For spilled args (always present even with 0-3 args)
!	| ----------
!	| Pushed return address		Via 'call'
!	| ----------				Above done in caller; below in callee
!	| Pushed nonvol workregs	If extend to R3 and above
!	| Pushed nonvol workxregs	If extend to XR6 and above
!	| ----------				Above done in caller; below in callee
!	| Pushed FP					Save FP
!	| ----------
!	! Local vars				All locals (when used)
!	| ----------
!	| Temps						All temps
!	| ----------
!	| 32-byte shadow space		For any calls made in this func
!	| [Stack adj]				Extra slot may be added to keep stack pointer 16-byte aligned

	int retmode, hasequiv, offset, size, reg
	int nsavedbytes, paramoffset
!*!	mclopnd ax
	psymbol d
	[100]char str, newname
	int r, n

!CPL $LINENO

	if currfunc.tempmodes=nil then merror("Needs tempmodes") fi

	setmclentry(mclprocentry)

	framesize:=0
	dblockarg:=nil

!NEXTWORKREG:=R4
!NEXTWORKXREG:=XR7

	nsavedregs:=max(highworkreg-r2, 0)
	nsavedxregs:=max(highworkxreg-xr5, 0)
	nsavedbytes:=(nsavedregs+nsavedxregs)*8

!CPL $LINENO
!CPL CURRFUNC.NAME,"HIGHWORKREG=", STRREG(HIGHWORKREG)
!CPL CURRFUNC.NAME,"HIGHWORKXREG=", STRREG(HIGHWORKXREG)


!allocate offsets to args, and set defines

!CPL "PROC B", =CURRFUNC.NAME, =CURRFUNC.NPARAMS, STRPMODE(CURRFUNC.MODE, CURRFUNC.SIZE)

	if currfunc.mode=tpblock then	!need to inject extra parameter

		dblockarg:=tc_makesymbol("$block", param_id)
		dblockarg.nextparam:=currfunc.nextparam
		dblockarg.mode:=tpblock
		dblockarg.size:=currfunc.size

		currfunc.nextparam:=dblockarg
		++currfunc.nparams
	fi
!CPL "PROC B2", =CURRFUNC.NAME, =CURRFUNC.NPARAMS, STRPMODE(CURRFUNC.MODE, CURRFUNC.SIZE)

!IF NSAVEDBYTES THEN
!	CPL "Saving:", nsavedregs, nsavedxregs,"in", currfunc.name
!FI

	d:=currfunc.nextparam
	paramoffset:=16+nsavedbytes		!between top of stackframe and 1st param is fp/retaddr/saved

	while d, d:=d.nextparam do
		d.offset:=paramoffset
!CPL "SET DOFFSET", D.NAME, PARAMOFFSET
		paramoffset+:=8
!		if d.used then
			genmc_def(m_define, d)
!		elsif pcheckunusedlocals then
!			println "Unused param:", d.name, "in", currfunc.name
!		fi

	od
!CPL $LINENO


!allocate offsets to locals, and set defines
	d:=currfunc.nextlocal
	while d, d:=d.nextlocal do
		if not d.used then
			if pcheckunusedlocals then
				println "Unused local:", d.name, "in", currfunc.name
			fi
			nextloop
		fi

		size:=psize[d.mode]
		if d.mode=tpblock then
			size:=d.size
		fi

		if d.atvar then
			MERROR("PCODEB/@")
        else
			framesize+:=roundsizetg(size)
			d.offset:=-framesize
			genmc_def(m_define, d)
		fi
	od

!CPL "FS AFTER LOCALS", FRAMESIZE

!allocate offsets to temps, and set defines
	ntemps:=currfunc.maxtemp
	if ntemps>maxfixedtemp then merror("Too many temp") fi

	for i to ntemps do
!CPL I,STRPMODE(CURRFUNC.TEMPMODES[I].MODE),"//"
!CPL I,CURRFUNC.TEMPMODES[i].MODE,"//", =CURRFUNC.TEMPMODES[I].SIZE
		framesize+:=roundsizetg(currfunc.tempmodes[i].size)

		genmc(m_definetemp, mgentemp(i))
!CPL =FRAMESIZE
		mccodex.a.offset:=-framesize
		currfunc.tempmodes[i].offset:=-framesize
!		if -framesize < i16.min then merror("Temp offset outside > 16 bits") fi
		if -framesize < -8000000 then merror("Temp offset outside > 23 bits") fi
	od

!CPL "FS AFTER TEMPS", FRAMESIZE


	framesize+:=32									!shadow space
!CPL "FS AFTER SHADOW SPACE", FRAMESIZE

	if (framesize+nsavedbytes) iand 8 then			!keep stack frame 16-byte aligned
		framesize+:=8
	end

	savevolregs(nsavedregs, nsavedxregs)

!generate stack entry code proper:

	genmc(m_push, dframeopnd)
	genmc(m_mov, dframeopnd, dstackopnd)
	pushstack(framesize)

!spill any args to shadow space
	spillparams()

!	MCOMM("="*40)
	resetmclentry()
end

global proc do_proccode_c=
	int offset
	mclopnd ax, bx

!	MCOMM("="*40)

	genmc(m_label, mgenlabel(mretindex))

	if dblockarg then
!		MCOMMENT("BLOCK RETURN COPY NEEDED")
!D0 has address of block to be copied
!It needs to be returned on D0 after copying
MCOMMENT("BLOCKRET1")

		ax:=mgenireg(r0)
		bx:=mgenreg(r1)
		genmc(m_mov, bx, mgenmem(dblockarg))
		nextworkreg:=r2
		copyblock(mgenireg(r1), ax, dblockarg.size)		!does not affect r0
		genmc(m_xchg,  mgenreg(r0), bx)

MCOMMENT("BLOCKRET2")

	fi

	popstack(framesize)
	genmc(m_pop, dframeopnd)
	restorevolregs(nsavedregs, nsavedxregs)

	genmc(m_ret)
end

proc spillparams=
	psymbol d
	mclopnd ax
	int offset:=16, regoffset:=0, xregoffset, firstoffset

	regoffset:=0

	d:=currfunc.nextparam

	if currfunc.variadic then				!C proc def using ...
		firstoffset:=d.offset				!param offsets may be pushed up

		for i:=currfunc.nparams to 3 do				!0-based; if nparams=2, loops over 2..3 as 0..1 are normal
			ax:=mgenindex(areg:rframe, size:8, offset:i*8+firstoffset)
			genmc(m_mov, ax, mgenreg(i+r10))
		od
	fi

	while d, d:=d.nextparam do
		if regoffset>3 then exit fi

		if d.used or regoffset=0 then
			ax:=mgenindex(areg:rframe, size:8, offset:d.offset)
			case d.mode
			when tpr64 then
				genmc(m_movq, ax, mgenxreg(regoffset+xr0))
			when tpr32 then
				genmc(m_movd, changeopndsize(ax,4), mgenxreg(regoffset+xr0))
			else
				genmc(m_mov, ax, mgenreg(regoffset+r10))
			esac
		fi

		offset+:=8
		++regoffset
	od

end

proc savevolregs(int nregs, nxregs)=
	int reg
	mclopnd ax

	reg:=r3
	to nregs do
		genmc(m_push, mgenreg(reg++))
	od

	reg:=xr6
	ax:=mgenreg(r0)
	to nxregs do
		genmc(m_movq, ax, mgenreg(reg++))
		genmc(m_push, ax)
	od
end

proc restorevolregs(int nregs, nxregs)=
	int reg
	mclopnd ax

	reg:=xr6+nxregs
	ax:=mgenreg(r13)

	to nxregs do
		genmc(m_pop, ax)
		genmc(m_movq, mgenreg(--reg), ax)
	od
	reg:=r3+nregs
	to nregs do
		genmc(m_pop, mgenreg(--reg))
	od

end

proc gendq(int a)=
	genmc_int(m_dq, a)
end

global proc genabsneg=
	if lababs32+lababs64+labneg32+labneg64 then
		setsegment('I', 16)
	fi

	if lababs32 then
		mcomment("lababs32")
		genmc_label(m_label, lababs32)
		gendq(0x7FFF'FFFF'7FFF'FFFF)
		gendq(0x7FFF'FFFF'7FFF'FFFF)
	fi
	if lababs64 then
		mcomment("lababs64")
		genmc_label(m_label, lababs64)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
	fi

	if labneg32 then
		mcomment("labneg32")
		genmc_label(m_label, labneg32)
		gendq(0x8000'0000'8000'0000)
		gendq(0x8000'0000'8000'0000)
	fi
	if labneg64 then
		mcomment("labneg64")
		genmc_label(m_label, labneg64)
		gendq(0x8000'0000'0000'0000)
		gendq(0x8000'0000'0000'0000)
	fi

	if labzero then
		mcomment("labzero")
		genmc_label(m_label, labzero)
		gendq(0)
	fi

	if labmask63 then
		mcomment("mask63/offset64")
		genmc_label(m_label, labmask63)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		genmc_label(m_label, laboffset64)
		gendq(0x43E0'0000'0000'0000)
	fi
end

!global proc do_blockdata(tcl p) =
!	ref byte s
!	ref u64 d
!	int n, nqwords, nwords, r
!
!	n:=p.size
!	return when n=0
!
!	nwords:=n/8
!
!	d:=cast(p.svalue)
!	to nwords do
!		genmc_int(m_dq, d++^)
!	od
!
!	r:=n-nwords*8
!!CPL "DOBLOCKDATA", =N, =NWORDS, =R
!	s:=cast(d)
!	for i to r do
!		genmc_int(m_db, s++^)
!	od
!!
!!	if r then
!!!CPL "BLOCK/END"
!!		genmc_string(m_ascii), s)
!!!		genstring_db(cast(d), r, 0)
!!	fi
!!	MGENCOMMENT("ENDDATA")
!
!end

proc setmclentry(mcl p)=
!temporarily set mcl insertion before p

	mce_oldmccodex:=mccodex
	mccodex:=p
	mce_lastmcl:=p.lastmcl
	mce_nextmcl:=p.nextmcl
end

func resetmclentry:mcl pnew =
!restore mcl insertion point to normal
!restireturn mcl instruction that followed	
	mccodex.lastmcl:=mce_lastmcl
	mccodex.nextmcl:=mce_nextmcl
	pnew:=mccodex
	mccodex:=mce_oldmccodex
	pnew
end

proc setmclentryf(mcl p)=
!temporarily set mcl insertion before p

	mcf_oldmccodex:=mccodex
	mccodex:=p
	mcf_lastmcl:=p.lastmcl
	mcf_nextmcl:=p.nextmcl
end

func resetmclentryf:mcl pnew =
!restore mcl insertion point to normal
!restireturn mcl instruction that followed	
	mccodex.lastmcl:=mcf_lastmcl
	mccodex.nextmcl:=mcf_nextmcl
	pnew:=mccodex
	mccodex:=mcf_oldmccodex
	pnew
end

func checkisleaf(tcl p, int &maxargs)int isleaf=
	isleaf:=1
	maxargs:=0

	while p, p:=p.next do
		if tclhascall[p.opcode] then
			isleaf:=0

			if p.opcode = kcall then
				maxargs max:=p.nargs
			else						!other op that may use call
				maxargs max:=2			!just assume 2
			fi

		fi
	od

	return isleaf
end

global proc clearblock(mclopnd ax, int n)=
!ax will always be D0 containing the address of the block that is to be cleared
	mclopnd rx, rcount
	int nwords, lab, oddbytes, offset, workreg, countreg

!	ax.mode:=a_mem			!turn into [D0]
	ax:=mgenireg(ax.reg)

	oddbytes:=n rem 8		!will be zero, or 1..7

	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of u64s (ie. octobytes)

	rx:=mgenreg(getworkireg())
	clearreg(rx)

	offset:=0

	if 1<=nwords<=8 then		!use unrolled code (no loop)
		ax:=changeopndsize(ax, 8)

		to nwords do
			genmc(m_mov, applyoffset(ax, offset), rx)
			offset+:=8
		od

	elsif nwords<>0 then		!use a loop

!SPLIT INTO xx VERSIONS:
! NWORDS IS A MULTIPLE OF 4, so can write 4 words at a time, in 1/4 of iterations
! Or do one word at a time like now.
! nword is a multiple of 4 happens when N is a multiple of 32 bytes, which will
! always be the case for power-of-two sizes of 32 bytes or more. 32/64 may already
! be done without a loop. So non-part-unrolled version only really for odd array or
! struct sizes, such as [100]char.

		if nwords iand 3 then		!not 4n

			rcount:=mgenreg(countreg:=getworkireg())
			lab:=++mlabelno

!			ax:=makesimpleaddr(ax)

			genmc(m_mov, rcount, mgenint(nwords))
			genmc(m_label, mgenlabel(lab))
			genmc(m_mov, ax, rx)

			genmc(m_add, mgenreg(ax.reg), mgenint(8))

			genmc(m_dec, rcount)
			genmc_cond(m_jmpcc, ne_cond, mgenlabel(lab))

			offset:=0
		else
			rcount:=mgenreg(countreg:=getworkireg())
			lab:=++mlabelno

!			ax:=makesimpleaddr(ax)
			genmc(m_mov, rcount, mgenint(nwords/4))
			genmc(m_label, mgenlabel(lab))

			for i to 4 do
				genmc(m_mov, applyoffset(ax, offset), rx)
				offset+:=8
			od

			genmc(m_add, mgenreg(ax.reg), mgenint(targetsize*4))

			genmc(m_dec, rcount)
			genmc_cond(m_jmpcc, ne_cond, mgenlabel(lab))

			offset:=0
		fi
	fi

	if oddbytes then
		n:=oddbytes						!1..7

		if n>=4 then
			rx:=changeopndsize(rx, 4)
			genmc(m_mov, applyoffset(ax, offset, 4), rx)
			n-:=4
			offset+:=4
		fi
		if n>=2 then
			rx:=changeopndsize(rx, 2)
			genmc(m_mov, applyoffset(ax, offset, 2), rx)
			n-:=2
			offset+:=2
		fi
		if n=1 then
			rx:=changeopndsize(rx, 1)
			genmc(m_mov, applyoffset(ax, offset, 1), rx)
		fi
	fi
end

global proc copyblock(mclopnd ax,bx, int n)=
!ax, bx refer to memory; do ax:=bx for n bytes

	mclopnd rx, rcount
	int nwords, lab, oddbytes, offset, workreg, countreg, axreg

	if n=16 then
		rx:=mgenreg(getworkxreg())
		genmc(m_movdqu, rx, bx)
		genmc(m_movdqu, ax, rx)
		return
	fi

	oddbytes:=n rem 8		!will be zero, or 1..7
	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of u64s (ie. octobytes)

	rx:=mgenreg(getworkireg())		!work reg

	offset:=0
		ax:=makesimpleaddr(ax)
		bx:=makesimpleaddr(bx)

	if nwords in 1..4 then		!use unrolled code (no loop)
		ax:=changeopndsize(ax, targetsize)
		bx:=changeopndsize(bx, targetsize)

		to nwords do
			genmc(m_mov, rx, applyoffset(bx, offset))
			genmc(m_mov, applyoffset(ax, offset), rx)
			offset+:=8
		od

	elsif nwords then			!use a loop
		rcount:=mgenreg(getworkireg())
		lab:=++mlabelno

!		ax:=makesimpleaddr(ax)
!		bx:=makesimpleaddr(bx)
		ax.size:=8

		genmc(m_mov, rcount, mgenint(nwords))
		genmc(m_label, mgenlabel(lab))
		genmc(m_mov, rx, bx)
		genmc(m_mov, ax, rx)

		genmc(m_add, mgenreg(ax.reg), mgenint(targetsize))
		genmc(m_add, mgenreg(bx.reg), mgenint(targetsize))

		genmc(m_dec, rcount)
		genmc_cond(m_jmpcc, ne_cond, mgenlabel(lab))

		offset:=0
	fi

	if oddbytes then
		n:=oddbytes						!1..7

		if n>=4 then
			rx:=changeopndsize(rx, 4)
			genmc(m_mov, rx, applyoffset(bx, offset, 4))
			genmc(m_mov, applyoffset(ax, offset, 4), rx)
			n-:=4
			offset+:=4
		fi
		if n>=2 then
			rx:=changeopndsize(rx, 2)
			genmc(m_mov, rx, applyoffset(bx, offset, 2))
			genmc(m_mov, applyoffset(ax, offset, 2), rx)
			n-:=2
			offset+:=2
		fi
		if n=1 then
			rx:=changeopndsize(rx, 1)
			genmc(m_mov, rx, applyoffset(bx, offset, 1))
			genmc(m_mov, applyoffset(ax, offset, 1), rx)
		fi
	fi
end

global func scaleindex(mclopnd ax, int scale)int=
!when scale is 1/2/3/4, return scale unchanged
!anything else, scale value in ax, return 1
	int n
	if scale in [1,2,4,8] then return scale fi

	mulimm(ax,scale)
	return 1
end

global proc mulimm(mclopnd ax, int n)=
!multiply operand in ax (a simple reg) by constant n
!will try efficient method if possible, otherwise use normal multiply 
	int shifts,m

	case n
	when 0 then
		clearreg(ax)
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

global func gethostfn(int opc)psymbol d =
	ichar name, namec

!try manual seach through pcl code
	case opc
	when kpower then
		name:="msys.m$power_i64"		!msys or msysc
		namec:="msysc.m$power_i64"
	else
		name:=nil
	esac

	if name then
		psymbol ps:=pproctable
		while ps, ps:=ps.next do
			if eqstring(name, ps.name) or eqstring(namec, ps.name) then
				return ps
			fi
		od
	fi

	merror("gethostfn?", tclnames[opc])
	nil
end

=== mc_conv_xb.m 0 0 12/40 ===
!convert tcl to mcl cond codes
!order is in eq ne lt le ge gt
[6]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
[6]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

proc tx_nop*(tcl p) =
	unimpl(p)
end

proc tx_comment*(tcl p) =
	mcomment(p.a.svalue)
!	mcomment("<COMMENT>")
end

proc tx_move*(tcl p) =	! M := b
	mclopnd ax

	ax:=loadopnd(p.b)
	storeopnd(p.a, ax)
end

proc tx_eval*(tcl p) =
	loadopnd(p.a)
end

proc tx_iloadx*(tcl p) =	! T :=(b + c*s + n)^
	mclopnd ax, bx, cx, px
	tclopnd c:=p.c
	int scale:=p.scale, offset:=p.extra

	bx:=loadopnd(p.b, tpu64)

	if scale=0 then merror("ILOADPX SCALE=0") fi

	unless c.optype=int_opnd and c.value=0 then
		cx:=loadopnd(c, tpi64)
		if scale not in [1,2,4,8] then
			genmc(m_imul2, cx, mgenint(scale))
			scale:=1
		fi
!		px:=mgenindex(areg:bx.reg, ireg:cx.reg, scale:scale, offset:offset, size:p.size)
		px:=mgenindex(areg:bx.reg, ireg:cx.reg, scale:scale, offset:offset)
	else
		px:=mgenireg(bx.reg, pmode, offset)
	end	

	if pmode=tpblock then
		if px.reg and px.regix=rnone then
			ax:=mgenreg(px.reg)
		else
			ax:=mgenreg(getworkireg())
			genmc(m_lea, ax, px)
		fi
	else
		ax:=mgenreg(getworkreg(pmode), pmode)
		genmc(m_mov, ax, px)
	fi

	storeopnd(p.a, ax)
end

proc tx_istorex*(tcl p) =	! (a + b*s + n)^ := c
	mclopnd ax, bx, cx, px
	tclopnd b:=p.b
	int scale:=p.scale, offset:=p.extra

	ax:=loadopnd(p.a, tpu64)

	if scale=0 then merror("ILOADPX SCALE=0") fi

	unless b.optype=int_opnd and b.value=0 then
		bx:=loadopnd(b, tpi64)
		if scale not in [1,2,4,8] then
			genmc(m_imul2, bx, mgenint(scale))
			scale:=1
		fi
		px:=mgenindex(areg:ax.reg, ireg:bx.reg, scale:scale, offset:offset, size:p.size)
	else
		px:=mgenireg(ax.reg, pmode, offset)
	end	

	cx:=loadopnd(p.c)
	if pmode=tpblock then
!CPL "ISTORE BLOCK", MSTROPND(PX), MSTROPND(CX)
		cx:=mgenireg(cx.reg)
		copyblock(px, cx, currtcl.size)
	else

		genmc(m_mov, px, changeopndsize(cx, p.size))
	fi

end

proc tx_call*(tcl p) =	! ([T ...] F [r ...]) r=nret, n=nargs
	do_call(p)
end

proc tx_retproc*(tcl p) =	! return
	genmc_label(m_jmp, mretindex)
end

proc tx_retfn*(tcl p) =	! return a

	if pfloat[p.mode] then
		loadopnd(p.a, reg:xr0)
	else
		loadopnd(p.a, reg:r0)
	fi
	genmc_label(m_jmp, mretindex)
end

proc tx_retmult*(tcl p) =	! return n values
	unimpl(p)
end

proc tx_jump*(tcl p) =	! goto L
	genmc_label(m_jmp, p.a.labelno)
end

proc tx_jumpcc*(tcl p) =	! goto L when b cc c
	do_jumpcc(p)
end

proc tx_jumpt*(tcl p) =	! goto L when istrue(b)
	do_jumptf(p, nz_cond)
end

proc tx_jumpf*(tcl p) =	! goto L when not istrue(b)
	do_jumptf(p, z_cond)
end

proc tx_ijump*(tcl p) =	! goto a
!	mclopnd ax
!
!	ax:=loadopnd(p.a)

!	genmc(m_jmp, mgenireg(ax.reg))
	genmc(m_jmp, loadopnd(p.a))
end

proc tx_setcc*(tcl p) =	! T := b cc c
	do_setcc(p)
end

proc tx_to*(tcl p) =	! --b; goto L when b<>0
	mclopnd ax
	tclopnd b:=p.b

	ax:=loadopnd(b)
	genmc(m_dec, ax)
	storeopnd(b, ax)
	genmc_cond(m_jmpcc, nz_cond, mgenlabel(p.a.labelno))
end

proc tx_forup*(tcl p) =	! b+:=n; goto L when b <= c
	do_for(p)
end

proc tx_iswap*(tcl p) =	! swap(P, P)
	mclopnd px, qx, ax, bx

	px:=loadptropnd(p.a)
	qx:=loadptropnd(p.b)

	ax:=mgenreg(getworkreg(pmode), pmode)
	bx:=mgenreg(getworkreg(pmode), pmode)

	genmc(m_mov, ax, px)
	genmc(m_mov, bx, qx)

	genmc(m_mov, qx, ax)
	genmc(m_mov, px, bx)

end

proc tx_add*(tcl p) =	! T := b + c
	do_binarith(p, m_add, m_addss)
end

proc tx_sub*(tcl p) =	! T := b - c
	do_binarith(p, m_sub, m_subss)
end

proc tx_mul*(tcl p) =	! T := b * c
	do_binarith(p, m_imul2, m_mulss)
end

proc tx_div*(tcl p) =	! T := b / c (float only)
	do_binarith(p, m_nop, m_divss)
end

proc tx_idiv*(tcl p) =	! T := b / c (int only; b % c)
	do_divrem(p, issigned:psigned[pmode], isdiv:1)
end

proc tx_irem*(tcl p) =	! T := b irem c
	do_divrem(p, issigned:psigned[pmode], isdiv:0)
end

proc tx_bitand*(tcl p) =	! T := b iand c
	do_bitbin(p, m_and)
end

proc tx_bitor*(tcl p) =	! T := b ior c
	do_bitbin(p, m_or)
end

proc tx_bitxor*(tcl p) =	! T := b ixor c
	do_bitbin(p, m_xor)
end

proc tx_shl*(tcl p) =	! T := b << c
	do_shift(p, m_shl)
end

proc tx_shr*(tcl p) =	! T := b >> c
	do_shift(p, (psigned[pmode]|m_sar|m_shr))
end

proc tx_min*(tcl p) =	! T := min(b, c)
	if pfloat[pmode] then
		do_max_float(p, m_minss+pwide[pmode])
	else
		do_max_int(p, (psigned[pmode]|gt_cond|gtu_cond))
	fi
end

proc tx_max*(tcl p) =	! T := max(b, c)
	if pfloat[pmode] then
		do_max_float(p, m_maxss+pwide[pmode])
	else
		do_max_int(p, (psigned[pmode]|lt_cond|ltu_cond))
	fi
end

proc tx_subpx*(tcl p) =	! T := b - c*s
	int scale
	mclopnd ax, bx
	tclopnd b

	scale:=p.scale

	ax:=loadopnd(p.b)
	b:=p.c

	if b.optype=int_opnd then
		genmc(m_sub, ax, mgenint(b.value*scale))
	else
		bx:=loadopnd(b)
		scale:=scaleindex(bx, scale)
		if scale>1 then
			mulimm(bx, scale)
		fi
		genmc(m_sub, ax, bx)
	fi

	storeopnd(p.a, ax)

end

proc tx_subp*(tcl p) =	! T := (b - c)/s
	mclopnd ax, bx
	int n, scale:=p.scale

	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)

	genmc(m_sub, ax, bx)

	if scale>1 then
		n:=ispoweroftwo(scale)
		if n then
			genmc(m_shr, ax, mgenint(n))
		else
			if ax.reg=r0 then
				genmc(m_cqo)
				genmc(m_mov, bx, mgenint(scale))
				genmc(m_idiv, bx)
			else
				merror("subp?")
			fi
		fi
	fi

	storeopnd(p.a, ax)
end

proc tx_atan2*(tcl p) =	! T := atan2(b, c)
	unimpl(p)
end

proc tx_power*(tcl p) =	! T := b ** c
	psymbol d
	
	if pint[pmode] then
		d:=gethostfn(kpower)
		do_host(p, d, 2)
	else
		do_maths(p, "pow*", 2)
	fi
end

proc tx_fmod*(tcl p) =
	unimpl(p)
end

proc tx_neg*(tcl p) =	! T := -b
	mclopnd ax

	ax:=loadopnd(p.b)

	if pfloat[pmode] then
		if ispwide(pmode) then
			if not labneg64 then labneg64:=mcreatefwdlabel() fi
			genmc(m_xorpd, ax, mgenlabelmem(labneg64))
		else
			if not labneg32 then labneg32:=mcreatefwdlabel() fi
			genmc(m_xorps, ax, mgenlabelmem(labneg32))
		fi
	else
		genmc(m_neg, ax)
	fi

	storeopnd(p.a, ax)
end

proc tx_abs*(tcl p) =	! T := abs b
! T := abs b
	mclopnd ax,lx

	ax:=loadopnd(p.b)

	if pint[pmode] then
		genmc(m_cmp, ax, mgenint(0))

		genmc_cond(m_jmpcc, ge_cond, lx:=mgenlabel(++mlabelno))
		genmc(m_neg,ax)
		genmc(m_label, lx)

	else
		if pwide[pmode] then
			if not lababs64 then lababs64:=mcreatefwdlabel() fi
			genmc(m_andpd, ax, mgenlabelmem(lababs64))
		else
			if not lababs32 then lababs32:=mcreatefwdlabel() fi
			genmc(m_andps, ax, mgenlabelmem(lababs32))
		fi
	fi

	storeopnd(p.a, ax)
end

proc tx_bitnot*(tcl p) =	! T := inot b
	mclopnd ax

	ax:=loadopnd(p.b)
	genmc(m_not, ax)
	storeopnd(p.a, ax)
end

proc tx_not*(tcl p) =	! T := not b
	mclopnd ax

	if not pint[pmode] then merror("not") fi
	ax:=loadopnd(p.b)
	genmc(m_xor, ax, mgenint(1))
	storeopnd(p.a, ax)
end

proc tx_toboolt*(tcl p) =	! T := istrue b
!toboolf implements 'not b' in HLL when b is not known to a boolean

	mclopnd ax, bx, cx
	byte pmode2:=p.mode2

	ax:=loadopnd(p.b, pmode2)

	if pfloat[pmode2] then
		bx:=mgenreg(getworkxreg(), pmode2)
		cx:=mgenreg(getworkireg(), tpu8)
		genmc(m_xorps+pwide[pmode2], bx, bx)
		genmc(m_comiss+pwide[pmode2], ax, bx)

		genmc_cond(m_setcc, (p.opcode=ktoboolt|ne_cond|eq_cond), cx)
		genmc(m_movzx, changeopndsize(cx,4),cx)		!4 works for u32/u64
		storeopnd(p.a, cx)

	else
		genmc(m_test, ax, ax)
		genmc_cond(m_setcc, (p.opcode=ktoboolt|ne_cond|eq_cond), bx:=changeopndsize(ax,1))
		genmc(m_movzx, changeopndsize(ax,4),bx)
		storeopnd(p.a, ax)
	fi
end

proc tx_sqr*(tcl p) =	! T := sqr(b)
	mclopnd ax
	int opc

	ax:=loadopnd(p.b)
	if pfloat[pmode] then
		opc:=m_mulss+pwide[pmode]
	else
		opc:=m_imul2
	fi

	genmc(opc, ax, ax)
	storeopnd(p.a, ax)
end

proc tx_sqrt*(tcl p) =	! T := sqrt(b)
	mclopnd fx

	fx:=loadopnd(p.b)
	genmc(m_sqrtss+pwide[pmode], fx, fx)
	storeopnd(p.a, fx)
end

proc tx_sin*(tcl p) =	! T := sin(b)
	do_maths(p, "sin*")
end

proc tx_cos*(tcl p) =	! T := cos(b)
	do_maths(p, "cos*")
end

proc tx_tan*(tcl p) =	! T := tan(b)
	do_maths(p, "tan*")
end

proc tx_asin*(tcl p) =	! T := asin(b)
	do_maths(p, "asin*")
end

proc tx_acos*(tcl p) =	! T := asin(b)
	do_maths(p, "acos*")
end

proc tx_atan*(tcl p) =	! T := atan(b)
	do_maths(p, "atan*")
end

proc tx_log*(tcl p) =	! T := log(b)
	do_maths(p, "log*")
end

proc tx_log10*(tcl p) =	! T := log10(b)
	do_maths(p, "log10*")
end

proc tx_exp*(tcl p) =	! T := exp(b)
	do_maths(p, "exp*")
end

proc tx_round*(tcl p) =	! T := round(b)
	do_maths(p, "round*")
end

proc tx_ceil*(tcl p) =	! T := ceil(b)
	do_maths(p, "ceil*")
end

proc tx_floor*(tcl p) =	! T := floor(b)
	do_maths(p, "floor*")
end

proc tx_fract*(tcl p) =	! T := fract(b)
	unimpl(p)
end

proc tx_sign*(tcl p) =	! T := sign(b)
	unimpl(p)
end

proc tx_float*(tcl p) =	! T := float(b)
	mclopnd fx, ax

	ax:=loadopnd(p.b, p.mode2)
	fx:=mgenreg(getworkxreg(), pmode)

	genmc(m_cvtsi2ss+pwide[pmode], fx, ax)

	storeopnd(p.a, fx)
end

proc tx_fix*(tcl p) =	! T := fix(b)
	mclopnd fx, ax, bx

	fx:=loadopnd(p.b, p.mode2)
	bx:=ax:=mgenreg(getworkireg(), pmode)
	if bx.size<4 then bx:=changeopndsize(bx, 4) fi

	genmc(m_cvttss2si+pwide[p.mode2], bx, fx)

	storeopnd(p.a, ax)
end

proc tx_truncate*(tcl p) =	! T := u(b)
	mclopnd ax, bx
	byte pmode2:=p.mode2

	bx:=loadopnd(p.b, pmode2)
	ax:=changeopndsize(bx, p.size)

	if p.size<>psize[pmode2] then
		genmc(ploadop[pmode2], ax, bx)
	fi

	storeopnd(p.a, ax)
end

proc tx_fwiden*(tcl p) =	! T := r64(b)
	mclopnd fx, gx

	fx:=loadopnd(p.b, tpr32)
	gx:=changeopndsize(fx, 8)

	genmc(m_cvtss2sd, gx, fx)
	storeopnd(p.a, gx)
end

proc tx_fnarrow*(tcl p) =	! T := r32(b)
	mclopnd fx, gx

	fx:=loadopnd(p.b, tpr64)
	gx:=changeopndsize(fx, 4)

	genmc(m_cvtsd2ss, gx, fx)
	storeopnd(p.a, gx)
end

proc tx_widen*(tcl p) =	! T := t(b)
	mclopnd ax, bx

	ax:=loadopnd(p.b, p.mode2)

	if pmode=tpu64 and p.mode2=tpu32 then		!32-bit load should have zeroed top half
		bx:=changeopndsize(ax, p.size)
	else
		genmc((psigned[p.mode2]|m_movsx|m_movzx), bx:=changeopndsize(ax, p.size), ax)
	fi

	storeopnd(p.a, bx)

end

proc tx_typepun*(tcl p) =	! T := t(u@(b))
!  T1 := typepun(sx)                     i64/r32         |<T1: 
!  T3 := typepun(sx)                     u64/r32         |<T3: 
!
!  T1 := typepun(x)                      i64/r64         |<T1: 
!  T3 := typepun(x)                      u64/r64         |<T3: 
!
!  T5 := typepun(a)                      r32/i64         |<T5: 
!  T5 := typepun(u)                      r32/u64         |<T5: 
!
!  T7 := typepun(a)                      r64/i64         |<T7: 
!  T7 := typepun(u)                      r64/u64         |<T7: 
!
!  T1 := typepun(x)                      i64/r64         |<T1: 
!  T3 := typepun(x)                      u64/r64         |<T3: 
!note r32 with sign-bit set if NOT sign-extended, as r32 bit pattern is considered as u32

	byte smode:=p.mode2
	mclopnd ax, bx:=loadopnd(p.b, smode)

	if pfloat[smode] then
		ax:=mgenreg(getworkireg())
!CPL =MSTROPND(BX)

	else
		ax:=mgenreg(getworkxreg(), pmode)
!CPL =MSTROPND(BX)
	fi
	genmc(m_movq, ax, changeopndsize(bx,8))				!basically switch between float/non-float reg

	storeopnd(p.a, bx)
end

proc tx_addto*(tcl p) =	! P +:= b
	do_bintoarith(p, m_add, m_addss)
end

proc tx_subto*(tcl p) =	! P -:= b
	do_bintoarith(p, m_sub, m_subss)
end

proc tx_multo*(tcl p) =	! P *:= b
	do_bintoarith(p, m_imul2, m_mulss)
end

proc tx_divto*(tcl p) =	! P /:= b (float)
	do_bintoarith(p, m_nop, m_divss)
end

proc tx_idivto*(tcl p) =	! P /:= b (int: %:= b)
	do_divremto(p, issigned:psigned[pmode], isdiv:1)
end

proc tx_iremto*(tcl p) =	! P irem:= b
	do_divremto(p, issigned:psigned[pmode], isdiv:0)
end

proc tx_bitandto*(tcl p) =	! P iand:= b
	do_bitbinto(p, m_and)
end

proc tx_bitorto*(tcl p) =	! P ior:= b
	do_bitbinto(p, m_or)
end

proc tx_bitxorto*(tcl p) =	! P ixor:= b
	do_bitbinto(p, m_xor)
end

proc tx_shlto*(tcl p) =	! P <<:= b
	do_shiftto(p, m_shl)
end

proc tx_shrto*(tcl p) =	! P >>:= b
	do_shiftto(p, (psigned[pmode]|m_sar|m_shr))
end

proc tx_minto*(tcl p) =	! P min:= b
	if pfloat[pmode] then
		do_maxto_real(p, leu_cond)
	else
		do_maxto_int(p, (psigned[pmode]|le_cond|leu_cond))
	fi
end

proc tx_maxto*(tcl p) =	! P max:= b
	if pfloat[pmode] then
		do_maxto_real(p, geu_cond)
	else
		do_maxto_int(p, (psigned[pmode]|ge_cond|geu_cond))
	fi
end

proc tx_addpxto*(tcl p) =	! P +:= b*s
	mclopnd px, bx
	tclopnd b:=p.b

	px:=loadptropnd(p.a)

	if b.optype=int_opnd then
		genmc(m_add, px, mgenint(b.value*p.scale))
	else
		bx:=loadopnd(p.b)
		mulimm(bx, p.scale)
		genmc(m_add, px, bx)
	fi
end

proc tx_subpxto*(tcl p) =	! P -:= b*s
	mclopnd px, bx
	tclopnd b:=p.b

	px:=loadptropnd(p.a)

	if b.optype=int_opnd then
		genmc(m_sub, px, mgenint(b.value*p.scale))
	else
		bx:=loadopnd(b, pmode)
		mulimm(bx, p.scale)
		genmc(m_sub, px, bx)
	fi
end

proc tx_negto*(tcl p) =	! -:=P
	unimpl(p)
end

proc tx_absto*(tcl p) =	! abs:=P
	unimpl(p)
end

proc tx_bitnotto*(tcl p) =	! inot:=P
	unimpl(p)
end

proc tx_notto*(tcl p) =	! not:=P
	unimpl(p)
end

proc tx_toboolto*(tcl p) =	! istrue+:=P
	unimpl(p)
end

proc tx_incrto*(tcl p) =	! ++P
	mclopnd px

	px:=loadptropnd(p.a)
	genmc((p.opcode=kincrto|m_add|m_sub), px, mgenint(p.step))
end

proc tx_incrload*(tcl p) =	! T := ++P
	mclopnd ax, px

	px:=loadptropnd(p.b)
	genmc((p.opcode=kincrload|m_add|m_sub), px, mgenint(p.step))
	ax:=mgenreg(getworkireg(), pmode)
	genmc(m_mov, ax, px)
	storeopnd(p.a, ax)

end

proc tx_loadincr*(tcl p) =	! T := P++
	mclopnd ax, px

	px:=loadptropnd(p.b)
	ax:=mgenreg(getworkireg(), pmode)
	genmc(m_mov, ax, px)
	storeopnd(p.a, ax)

	genmc((p.opcode=kloadincr|m_add|m_sub), px, mgenint(p.step))
end

proc tx_switch*(tcl p) =	! switch on c; L=jumptable, L2=else label
	mclopnd ax
	int minlab:=p.minlab, maxlab:=p.maxlab

	ax:=loadopnd(p.c)								!load index

	if minlab then
		genmc(m_sub, ax, mgenint(minlab))			!base it from zero
	fi
	if p.opcode=kswitch then						!do range check (kswitchu is unchecked)
		genmc(m_cmp, ax, mgenint(maxlab-minlab+1))
		genmc_cond(m_jmpcc, geu_cond, mgenlabel(p.b.labelno))		!jump to else
	fi

	genmc(m_jmp, mgenindex(ireg:ax.reg, scale:8, labno:p.a.labelno))	!jump via table
end

proc tx_swlabel*(tcl p) =	! label for switch jump table
	genmc(m_dq, mgenlabel(p.a.labelno))
end

proc tx_addpx*(tcl p) =	! T := b + c*s + n
	mclopnd ax, bx, cx, px
	tclopnd c:=p.c
	int scale:=p.scale, offset:=p.extra

	bx:=loadopnd(p.b)

	if scale=0 then merror("ADDPX SCALE=0") fi

	ax:=mgenreg(getworkreg(pmode), pmode)

	unless c.optype=int_opnd and c.value=0 and offset=0 then
		cx:=loadopnd(c)
		if scale not in [1,2,4,8] then
			genmc(m_imul2, cx, mgenint(scale))
			scale:=1
		fi
		px:=mgenindex(areg:bx.reg, ireg:cx.reg, scale:scale, offset:offset, size:p.size)
		genmc(m_lea, ax, px)
	else
		ax:=bx
	end	

	storeopnd(p.a, ax)
end

proc tx_stop*(tcl p) =
	mclopnd ax

	ax:=loadopnd(p.a, reg:r10)

	genmc(m_call, mgenextname("exit*"))
end

proc tx_label*(tcl p) =
	genmc(m_label, mgenlabel(p.a.labelno))
end

proc tx_data*(tcl p) =
	unimpl(p)
end

proc tx_loadbit*(tcl p) =	! T := b.[c]
!t:=b.[c]
	tclopnd c:=p.c
	mclopnd ax, bx
	int i,m

	if c.optype=int_opnd then
		i:=c.value
		m:=(i in 0..31|tpu32|tpu64)

		ax:=loadopnd(p.b, m)
		if i then
			genmc(m_shr, ax, mgenint(i, m))
		fi
		goto skip when i=63
	else
		ax:=loadopnd(p.b)
		bx:=loadopnd(c, tpu8, reg:r10)
		genmc(m_shr, ax, bx)
	fi

	genmc(m_and, changeopndsize(ax, 4), mgenint(1, tpu32))
skip:

	storeopnd(p.a, ax)

end

proc tx_loadbf*(tcl p) =	! T := b.[c..d]
	tclopnd c:=p.c, d:=p.abc[4]
	mclopnd ax

	if c.optype=d.optype=int_opnd then
		ax:=do_loadbf_const(p, c.value, d.value)
	else
		ax:=do_loadbf_var(p)
	fi

	storeopnd(p.a, ax)
end

proc tx_storebit*(tcl p) =	! P.[b] := c
	do_storebit(p)
end

proc tx_storebf*(tcl p) =	! P.[b..c] := d
	do_storebf(p)
end

proc tx_idivrem*(tcl p) =	! (T1, T2) := C divrem d
	do_divrem(p, issigned:psigned[pmode], isdiv:2)
end

proc tx_jumpin*(tcl p) =	! goto L when b in c..d
	mclopnd ax, bx, cx
	int lab

	lab:=mcreatefwdlabel()

	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)

!note: jumpout is only defined for i64
	genmc(m_cmp, ax, bx)
	genmc_cond(m_jmpcc, lt_cond, mgenlabel(lab))

	cx:=loadopnd(p.abc[4])
	genmc(m_cmp, ax, cx)
	genmc_cond(m_jmpcc, le_cond, mgenlabel(p.a.labelno))
	mdefinefwdlabel(lab)

end

proc tx_jumpout*(tcl p) =	! goto L when b not in c..d
	mclopnd lx, ax, bx, cx

	lx:=mgenlabel(p.a.labelno)
	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)

!note: jumpout is only defined for i64
	genmc(m_cmp, ax, bx)
	genmc_cond(m_jmpcc, lt_cond, lx)

	cx:=loadopnd(p.abc[4])
	genmc(m_cmp, ax, cx)
	genmc_cond(m_jmpcc, gt_cond, lx)
end

proc tx_clear*(tcl p) =	! clear P
	mclopnd ax

	ax:=loadopnd(p.a, tpu64)
	clearblock(ax, p.size)
end

proc do_binarith(tcl p, int iopc, fopc)=
!opc is integer op, fopc is r32 op; get r64 op by adding 1
	mclopnd ax, bx
	int opc

	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)

	if pfloat[pmode] then
		opc:=fopc+pwide[pmode]
!		if p.size=8 then ++opc fi
	else
		opc:=iopc
	fi

	genmc(opc, ax, bx)
	storeopnd(p.a, ax)
end

proc do_call(tcl p)=
!call function:
! adjust stack if needed
! push stacked args
! load reg args
! create shadowspace if there were stacked args
! call the function
! pop all the extra stuff pushed to SP
! store results in dest temps
	int pushedbytes:=0, nargs:=p.nargs, nret:=p.nret, argoffset:=p.argoffset
	byte blockret
	u32 blocksize
	tclopnd a
	mclopnd ax, bx

!CPL "DOCALL", =NARGS, =NRET, =ARGOFFSET

	blockret:=0
	blocksize:=0
	if p.mode=tpblock then
		case nret
		when 0 then merror("block ret not used")
		when 1 then
		else merror("block/mult ret")
		esac

		if p.isvariadic then merror("block/variadic") fi

		blockret:=1
		blocksize:=p.size

		--argoffset
		++nargs

	fi

	if nargs>4 then			!need to push some args
		if nargs.odd then
			pushstack(8)
			pushedbytes:=8
		fi
		for i:=nargs downto 5 do
			a:= p.abc[i+argoffset]
			ax:= loadopnd(a, reg:r10)
			ax:=changeopndsize(ax, 8)

			if p.isvariadic and a.opmode=tpr32 then		!needs widening
				genmc(m_movq, bx:=mgenreg(xr0), ax)
				genmc(m_cvtss2sd, bx, changeopndsize(bx, 4))
				genmc(m_movq, ax, bx)
			fi

			genmc(m_push, ax)
			pushedbytes+:=8
		od
	fi

	for i to min(nargs,4) do
		if blockret and i=1 then
			a:=p.a					!will be temp
!			MCOMMENT("PUSH BLOCK ADDR")
			genmc(m_lea, mgenreg(r10), mgentemp(a.tempno))

		else

			a:=p.abc[i+argoffset]
			if pfloat[a.opmode] then
				if a.isvariadic and a.opmode=tpr32 then
					ax:=loadopnd(a, a.opmode, reg:xr0+i-1)
					bx:=changeopndsize(ax, 8)
					genmc(m_cvtss2sd, bx, ax)
					genmc(m_movq, mgenreg(r10+i-1), bx)

				else
					loadopnd(a, a.opmode, reg:xr0+i-1)
					if a.isvariadic then
						loadopnd(a, a.opmode, reg:r10+i-1)
					fi
				fi

			else
				loadopnd(a, a.opmode, reg:r10+i-1)
			fi
		fi
	od
!CPL $LINENO

	if nargs>4 then
		pushstack(32)
		pushedbytes+:=32
	fi

!CPL $LINENO
	a:=p.abc[nret+1]						!function operand
	if a.optype=memaddr_opnd then			!simple direct function
		genmc(m_call, mgenmemaddr(a.def))
	else
!CPL "HERE", STROPND(A), STRPMODE(A.OPMODE), STRPMODE(PMODE)
		ax:=loadopnd(a, tpu64)
		genmc(m_call, ax)
	fi
!CPL $LINENO

	if pushedbytes then
		popstack(pushedbytes)
	fi
!CPL $LINENO

	if not blockret then					!block already copied by callee into temp
		for i to nret do
			a:=p.abc[i]
			if pfloat[a.opmode] then
				ax:=mgenreg(xr0+i-1, a.opmode)
			else
				ax:=mgenreg(r0+i-1)
			fi
			storeopnd(a, ax, mode:a.opmode)
		od
	fi
!CPL $LINENO

end

proc do_jumpcc(tcl p) =
	int mcond
	byte mode:=p.mode
	mclopnd ax, bx, lx

	mcond:=ucondcodes[p.cond]
	lx:=mgenlabel(p.a.labelno)

	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)

	if mode=tpblock then
		merror("jumpcc/block")
	elsif pint[mode] then
		if psigned[mode] then
			mcond:=scondcodes[p.cond]
		fi
		genmc(m_cmp, ax, bx)
	else
		genmc(m_comiss+pwide[mode], ax, bx)
	fi

	genmc_cond(m_jmpcc, mcond, lx)

end

proc do_setcc(tcl p) =
	int mcond
	byte mode:=p.mode
	mclopnd ax, bx, cx

!note: could share most of following with jumpcc:
	mcond:=ucondcodes[p.cond]

	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)

	if mode=tpblock then
		merror("SETcc/block")
	elsif pint[mode] then
		if psigned[mode] then
			mcond:=scondcodes[p.cond]
		fi
		genmc(m_cmp, ax, bx)
	else
		genmc(m_comiss+pwide[mode], ax, bx)
	fi

	cx:=mgenreg(getworkireg(), tpu8)

	genmc_cond(m_setcc, mcond, cx)
	genmc(m_movzx, changeopndsize(cx, 4), cx)

	storeopnd(p.a, cx)
end

proc do_for(tcl p)=
!	(kforup,	$+1,  0,  1,  0),  ! n   (L b c)	b+:=n; goto L when b <= c
!	(kfordown,	$+1,  0,  1,  0),  ! n   (L b c)	b-:=n; goto L when b >= c

	mclopnd bx
	byte up:=p.opcode=kforup
	psymbol d

	bx:=loadopnd(p.b)								!index var
	genmc((up|m_add|m_sub), bx, mgenint(p.step))	!incr/decr it
	storeopnd(p.b, bx)

	genmc(m_cmp, bx, loadopnd(p.c))

	genmc_cond(m_jmpcc, (up|le_cond|ge_cond), mgenlabel(p.a.labelno))

end

proc do_jumptf(tcl p, int cond)=
	mclopnd ax, bx

	ax:=loadopnd(p.b)

	if pint[pmode] then
		genmc(m_test, ax, ax)
	else
		bx:=mgenreg(getworkxreg(), psize[pmode])
		genmc(m_xorps+pwide[pmode], bx, bx)
		genmc(m_comiss+pwide[pmode], ax, bx)
	fi

	genmc_cond(m_jmpcc, cond, mgenlabel(p.a.labelno))
end

proc do_bitbin(tcl p, int opc)=
	mclopnd ax, bx

	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)
	genmc(opc, ax, bx)
	storeopnd(p.a, ax)
end

proc do_shift(tcl p, int opc)=
	mclopnd ax, bx
	tclopnd b:=p.c

	ax:=loadopnd(p.b)

	if b.optype=int_opnd then
		genmc(opc, ax, mgenint(b.value))
	else
		bx:=loadopnd(p.c, reg:r10)				!rcx = cl
		genmc(opc, ax, changeopndsize(bx,1))
	fi

	storeopnd(p.a, ax)
end

proc do_bintoarith(tcl p, int iopc, fopc)=
	mclopnd px, ax, bx
	byte wide

	px:=loadptropnd(p.a)

	bx:=loadopnd(p.b)

	if pfloat[pmode] then
		wide:=pwide[pmode]
		ax:=mgenreg(getworkxreg(), pmode)
		genmc(m_movd+wide, ax, px)
		genmc(fopc+wide, ax, bx)
		genmc(m_movd+wide, px, ax)
	elsif iopc=m_imul2 then
		ax:=mgenreg(getworkireg(), pmode)
		genmc(m_mov, ax, px)
		genmc(m_imul2, ax, bx)
		genmc(m_mov, px, ax)

	else
		genmc(iopc, px, bx)
	fi
end

proc do_bitbinto(tcl p, int opc)=
	mclopnd px, bx

	px:=loadptropnd(p.a)
	bx:=loadopnd(p.b)

	genmc(opc, px, bx)
end

proc do_shiftto(tcl p, int opc)=
	mclopnd px, bx
	tclopnd b:=p.b

	px:=loadptropnd(p.a)

	if b.optype=int_opnd then
		genmc(opc, px, mgenint(b.value))
	else
		bx:=loadopnd(b, reg:r10)
		genmc(opc, px, changeopndsize(bx,1))
	fi
end

global proc do_divrem(tcl p, int issigned, isdiv)=
!isdiv = 0/1/2 = rem/div/divrem
! Z' := Y % Z
	mclopnd ax, bx, rx
	tclopnd a, b
	int opc, n, shifts

	if isdiv=2 then
		a:=p.c
		b:=p.abc[4]
	else
		a:=p.b
		b:=p.c
	fi

	ax:=loadopnd(a, reg:r0)
	nextworkreg:=r1

	if b.optype=int_opnd and isdiv=1 then
		n:=b.value

		case n
		when 0 then
			merror("Divide by zero")
		when 1 then
			return
		else
			shifts:=ispoweroftwo(n)
			if shifts then
				genmc((issigned|m_sar|m_shr), ax, mgenint(shifts))
				storeopnd(p.a, ax)
				return
			fi
		esac
	fi 

	bx:=loadopnd(b)
	rx:=mgenreg(r11)

	if issigned then
		opc:=
			case psize[pmode]
			when 8 then	m_cqo
			when 4 then	m_cdq
			when 2 then	m_cwd
			else merror("div/u8"); 0
			esac
		genmc(opc)

		opc:=m_idiv
	else
		clearreg(rx)
		opc:=m_div
	fi

	genmc(opc, bx)

	case isdiv
	when 0 then				!rem; result in r11
		storeopnd(p.a, rx)
	when 1 then				!div; result in r0
		storeopnd(p.a, ax)
	else					!divrem; results in r0:r11
		storeopnd(p.a, ax)
		storeopnd(p.b, rx)
	esac

end

proc do_max_int(tcl p, int cond)=
	mclopnd ax,bx

	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)

	genmc(m_cmp, ax, bx)
	genmc_cond(m_cmovcc, cond, ax, bx)

	storeopnd(p.a, ax)
end

global proc do_max_float(tcl p, int opc)=
	mclopnd ax,bx

	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)

	genmc(opc, ax, bx)

	storeopnd(p.a, ax)
end

global proc do_maxto_int(tcl p, int cond)=
	mclopnd px, ax, bx
	int lab

	px:=loadptropnd(p.a)
	ax:=mgenreg(getworkireg(), pmode)
	genmc(m_mov, ax, px)

	bx:=loadopnd(p.b)

	genmc(m_cmp, ax, bx)
	lab:=mcreatefwdlabel()

	genmc_cond(m_jmpcc, cond, mgenlabel(lab))
	genmc(m_mov, px, bx)
	mdefinefwdlabel(lab)
end

global proc do_maxto_real(tcl p, int cond)=
	mclopnd px, ax, bx
	int lab

	px:=loadptropnd(p.a)
	bx:=loadopnd(p.b)

	ax:=mgenreg(getworkxreg(), pmode)
	genmc(m_mov, ax, px)

	genmc(m_comiss+pwide[pmode], ax, bx)
	lab:=mcreatefwdlabel()

	genmc_cond(m_jmpcc, cond, mgenlabel(lab))
	genmc(m_mov, px, bx)
	mdefinefwdlabel(lab)
end

func do_loadbf_const(tcl p, int i, j)mclopnd =
	mclopnd ax, mx
	word mask

	ax:=loadopnd(p.b)

	if j=63 then			!signed field includes sign bit; assume i>0
		genmc(m_sar, ax, mgenint(i))
	else

		if i then
			genmc(m_shr, ax, mgenint(i))
		fi

		mask:=inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))

		if mask>=word(i32.max) then
			mx:=mgenreg(getworkireg())
			genmc(m_mov, mx, mgenint(mask))

		else
			mx:=mgenint(mask)
		fi

		genmc(m_and, ax, mx)
	fi

	ax
end

func do_loadbf_var(tcl p)mclopnd =
	merror("loadbf_var")
	nil
end

proc do_storebit(tcl p) =
!	P.[b] := c
	tclopnd b:=p.b, c:=p.c
	mclopnd px, ax, cx, ix
	int i, offset
	byte mask1s, mask0s

	if b.optype=int_opnd then		!a.[k] := 0/1/x
		px:=loadptropnd(p.a)
		px:=changeopndsize(px, 1)	!update only a specific byte
		i:=b.value	
		offset:=i/8					! byte offset 0..7
		i iand:=7					! i will be bit index 0..7
		px:=applyoffset(px, offset)	! point to that byte

		mask0s:=1<<i				!eg 00001000
		mask1s:=inot(1<<i)			!eg 11110111

		if c.optype=int_opnd then
			if c.value=0 then
				genmc(m_and, px, mgenint(mask1s, pmode))
			else
				genmc(m_or, px, mgenint(mask0s, pmode))
			fi
		else
			ax:=loadopnd(c, tpu8)
			genmc(m_and, px, mgenint(mask1s, pmode))		!clear dest bit first
			if i then
				genmc(m_shl, ax, mgenint(i, tpu8))
			fi
			genmc(m_or, px, ax)							!add in 0 or 1
		fi
	elsif c.optype=int_opnd then						!A.[i]:=0/1
		px:=loadptropnd(p.a)

		ax:=mgenreg(getworkireg())
		genmc(m_mov, ax, mgenint(1))
		
		cx:=mgenreg(r10,tpu64)
		ix:=loadopnd(b, tpi64, reg: r10)
		genmc(m_shl, ax, changeopndsize(cx, 1))

!Now have 00001000 for ezzmple in ax
		if c.value=0 then
			genmc(m_not, ax)				!change to 111101111
			genmc(m_and, px, ax)			!set to 0
		else								!set to 1 (assume c.value was 1)
			genmc(m_or, px, ax)
		fi

	else
			merror("Storebit: both vars")
	fi
end

global proc do_storebf(tcl p) =
!	P.[b..c] := d
	mclopnd px, rx, mx, dx
	tclopnd b:=p.b, c:=p.c, d:=p.abc[4]
	int i, j
	word mask

	unless b.optype=c.optype=int_opnd then
		merror("storebf not imm")
	end

	dx:=loadopnd(d)							!rhs: value to store

	px:=loadptropnd(p.a)

	i:=b.value
	j:=c.value

	mx:=mgenreg(getworkireg())
	rx:=mgenreg(getworkireg())

	genmc(m_mov, rx, px)

	mask:=inot((inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i)

	genmc(m_mov, mx, mgenint(mask))

	if i then
		genmc(m_shl, dx, mgenint(i))
	fi

	genmc(m_and, rx, changeopndsize(mx, p.size))
	genmc(m_or, rx, dx)

	genmc(m_mov, px, changeopndsize(rx, p.size))
end

proc do_maths(tcl p, ichar opname, int nargs=1)=
	do_callrts(p, opname, nil, nargs)
end

proc do_host(tcl p, psymbol d, int nargs=1)=
	do_callrts(p, nil, d, nargs)
end

global proc do_callrts(tcl p, ichar opname, psymbol d, int nargs)=
!simpler version of do_call where args are always <=4, and no variadics,
!there is always one return value, and func call is always direct
!mode of args and return value assumed to be all pmode
	tclopnd a
	mclopnd ax

	for i to nargs do
		a:=p.abc[i+1]
		if pfloat[pmode] then				!mode of tcl op not operands
			loadopnd(a, reg:xr0+i-1)
		else
			loadopnd(a, reg:r10+i-1)
		fi
	od

	if opname then
		genmc(m_call, mgenextname(opname))
	else
		genmc(m_call, mgenmemaddr(d))
	fi

	if pfloat[pmode] then
		ax:=mgenreg(xr0, pmode)
	else
		ax:=mgenreg(r0)
	fi

	storeopnd(p.a, ax)
end

global proc do_divremto(tcl p, int issigned, isdiv)=
! P /:= b or P rem:= b

!isdiv = 0/1 = rem/div
! Z' := Y % Z
	mclopnd px, ax, bx, rx
	tclopnd b:=p.b
	int opc, n, shifts

	ax:=mgenreg(r0, pmode)
	nextworkreg:=r1
	px:=loadptropnd(p.a)

	genmc(m_mov, ax, px)

	bx:=loadopnd(b)

	rx:=mgenreg(r11, pmode)

	if issigned then
		opc:=
			case psize[pmode]
			when 8 then	m_cqo
			when 4 then	m_cdq
			when 2 then	m_cwd
			else merror("div/u8"); 0
			esac
		genmc(opc)

		opc:=m_idiv
	else
		clearreg(rx)
		opc:=m_div
	fi

	genmc(opc, bx)

	genmc(m_mov, px, (isdiv|ax|rx))
end

proc tx_setjmp*(tcl p)=
	mclopnd px, ax, bx
	int lab:=mcreatefwdlabel()


	ax:=loadopnd(p.a)
	px:=mgenireg(ax.reg)

	bx:=mgenreg(getworkireg())
	genmc(m_mov, bx, mgenlabel(lab))
	genmc(m_mov, px, bx)

	genmc(m_mov, applyoffset(px,8), dstackopnd)
	genmc(m_mov, applyoffset(px,16), dframeopnd)

	clearreg(ax)
	mdefinefwdlabel(lab)

end

proc tx_longjmp*(tcl p)=
	mclopnd px, ax, bx, cx

	bx:=loadopnd(p.b, reg:r0)			!ret value
	nextworkreg++
	highworkreg max:=nextworkreg

	ax:=loadopnd(p.a)
	px:=mgenireg(ax.reg)

	genmc(m_mov, dstackopnd, applyoffset(px,8))
	genmc(m_mov, dframeopnd, applyoffset(px,16))

	cx:=mgenreg(getworkireg())
!
	genmc(m_mov, cx, px)		!load stored return address
	genmc(m_jmp, cx)			!
end

proc tx_getr0*(tcl p)=
	storeopnd(p.a, mgenreg(r0))
end
=== mc_temp_xb.m 0 0 13/40 ===

global func loadopnd(tclopnd a, int mode=pmode, reg=rnone, copy=0)mclopnd =
!Load operand into a register, and return register number

!* When a dest register is not provided:
!
!  * For something not in a register, allocate a workreg and load it
!  * If already in a register, return that register ...
!  * ... unless a flag says it needs a copy
!
!* When a dest register is provided:
!
!  * If already in a register, then move (unless already in dest)
!  * Otherwise, use that instead of a new work register
!
!* When this is a temp, it will either be in a reg, or be spilled.
!  Loading a spilled temp will alter the table entries. Moving a temp
!  to a new specified register (eg. an arg reg) will not alter table entries
!
!* For regvars, they can be used in situ, unless a dest reg is given, or the
!  flag says copy

!Note: for float operands, specifying a non-float register is valid: it will
!load to that integer register (this may be need to load a value which is then
!more simply pushed). But ultimately it may be better to use a dedicated pushopnd func.

	psymbol d
	mclopnd ax, bx

	if mode=tpvoid then mode:=a.opmode fi

	ax:=mgenreg(getworkregc(mode, reg), mode)

	case a.optype
	when mem_opnd then
		bx:=mgenmem(a.def, mode)
		if mode=tpblock and a.def.id<>param_id then
			dolea
		fi
domov:
		genmc(m_mov, ax, bx)

	when temp_opnd then
		bx:=mgentemp(a.tempno, mode)
		if mode=tpblock then
			dolea
		fi
		domov

	when int_opnd then
		CASE CURRTCL.SIZE
		WHEN 2 THEN
			A.VALUE IAND:=0xFFFF
		WHEN 4 THEN
			A.VALUE IAND:=0xFFFF'FFFF
		ESAC

		bx:=mgenint(a.value, pmode)
		domov

	when real_opnd then
		bx:=mgenrealmem(a.xvalue, tpr64)
		domov

	when r32_opnd then
		bx:=mgenrealmem(a.xvalue32, tpr32)
		domov

	when string_opnd then
		bx:=mgenlabelmem(getstringindex(a.svalue))
dolea:
		genmc(m_lea, ax, bx)


	when label_opnd then
		bx:=mgenlabelmem(a.labelno)
		dolea

	when memaddr_opnd then
		d:=a.def
		bx:=mgenmem(d, mode)
		if d.id=param_id and d.mode=tpblock then		!pcl mode will be u64
            domov
		else
			dolea
		fi

	else
		merror("Loadopnd:", opndnames[a.optype])
	esac

!IF AX.SIZE=0 THEN CPL "SIZE=0" FI
!IF BX AND BX.SIZE=0 THEN CPL "BSIZE=0", MCLNAMES[MCCODEX.OPCODE], OPNDNAMES[A.OPTYPE] FI

	ax
end

global func loadptropnd(tclopnd a, int reg=rnone)mclopnd=
	mclopnd ax

	reg:=getworkiregc(reg)
	ax:=mgenreg(reg)

	case a.optype
	when mem_opnd then
		genmc(m_lea, ax, mgenmem(a.def))

	when temp_opnd then
		genmc(m_mov, ax, mgentemp(a.tempno))
	else
		merror("Loadptrop", opndnames[a.optype])
	esac

	mgenireg(reg, pmode)
end

global func getworkireg:int=
	if nextworkreg>r9 then
		merror("No more work regs")
	fi

	nextworkreg++
	highworkreg max:=nextworkreg
	nextworkreg
end

global func getworkxreg:int=
	if nextworkxreg>xr15 then
		merror("No more work xregs")
	fi
	nextworkxreg++
	highworkxreg max:=nextworkxreg
	nextworkxreg
end

global func getworkregc(int mode, reg=rnone)int =
!get workreg conditionally: only reg is not specified
	if reg then
		reg
	else
		getworkreg(mode)
	fi
end

func getworkiregc(int reg=rnone)int=
	if reg then
		reg
	else
		getworkireg()
	fi
end

func getworkxregc(int reg=rnone)int=
	if reg then
		reg
	else
		getworkxreg()
	fi
end

global func getworkreg(int mode)int =
	if ispfloat(mode) then
		getworkxreg()
	else
		getworkireg()
	fi
end

global proc storeopnd(tclopnd a, mclopnd bx, int mode=pmode) =
!store operand currently in register bx, to tcl operand a
!a will a simple variable, or temp
!note: bx will not necessarily be the correct size for dest
	mclopnd ax

!cpl "STOREOPND", MSTROPND(BX)

	if mode<>tpblock then
		bx:=changeopndsize(bx, psize[mode])
	fi

	case a.optype
	when mem_opnd then
		ax:=mgenmem(a.def, mode)
	when temp_opnd then
		ax:=mgentemp(a.tempno, mode)
	else
		merror("Storeopnd:", opndnames[a.optype])
	esac

	if bx.reg>=xr0 then
		genmc((bx.size=8|m_movq|m_movd), ax, bx)
	elsif mode=tpblock then
		bx:=mgenireg(bx.reg)

		copyblock(ax, bx, currtcl.size)
	else
		genmc(m_mov, ax, bx)
	fi
end

global func makesimpleaddr(mclopnd ax)mclopnd bx=
!assume ax is an ireg, but need a simple one with areg set but not ireg
	int newreg, reg, regix

!CPL "MSA CALLED", MSTROPND(AX)

	reg:=ax.reg
	regix:=ax.regix
	if reg=rframe then reg:=rnone fi

	if ax.mode<>a_mem then merror("MSA") fi

	if reg=rnone and regix=rnone then
		newreg:=getworkireg()
	elsif reg then				![reg] only; already simple
		return ax
	elsif regix then			![regix] only; may be scaled; use lea anyway
		newreg:=regix
	else						![reg+regix]
		newreg:=regix
	fi

	bx:=mgenireg(newreg)

	genmc(m_lea, mgenreg(newreg), ax)
	return bx
end
=== mc_objdecls.m 0 0 14/40 ===
global record imagefileheader =
	u16	machine
	u16	nsections
	u32	timedatestamp
	u32	symtaboffset
	u32	nsymbols
	u16	optheadersize
	u16	characteristics
end

global record imagedir =
	u32	virtualaddr
	u32	size
end

global record optionalheader =			!exe/dll only
	u16  magic
	byte     majorlv
	byte     minorlv
	u32 codesize
	u32 idatasize
	u32 zdatasize
	u32 entrypoint
	u32 codebase
!	u32 datebase		!32-bit exe files only
	u64	imagebase
	u32 sectionalignment
	u32 filealignment
	u16  majorosv
	u16  minorosv
	u16  majorimagev
	u16  minorimagev
	u16  majorssv
	u16  minorssv
	u32 win32version
	u32 imagesize
	u32 headerssize
	u32 checksum
	u16  subsystem
	u16  dllcharacteristics
	u64   stackreserve
	u64   stackcommit
	u64   heapreserve
	u64   heapcommit
	u32 loaderflags
	u32 rvadims
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
		u32	physical_address
		u32	virtual_size
	end
	u32	virtual_address
	u32	rawdata_size
	u32	rawdata_offset
	u32	relocations_ptr
	u32	linenos_offset
	u16	nrelocs
	u16	nlinenos
	u32	characteristics
end

global record imagesymbol =
	union
		[8]char shortname
		struct
			u32	shortx
			u32	longx
		end
		u64 longname
	end
	u32	value
	i16	sectionno
	u16	symtype
	byte	storageclass
	byte	nauxsymbols
end

global record importdirrec =
	u32	implookuprva
	u32	timedatestamp
	u32	fwdchain
	u32	namerva
	u32	impaddressrva
end

global record coffrelocrec =
	i32	virtualaddr
	i32	stindex
	i16	reloctype
end

global enumdata [0:]ichar relocnames =
	(abs_rel = 0,	$),
	(addr64_rel,	$),
	(addr32_rel,	$),
	(addr32nb_rel,	$),
	(rel32_rel,		$),
	(rel321_rel,	$),
	(rel8_rel,		$),				!used within assembler only, not in coff format
end

global record auxsectionrec = 
	i32 length
	i16 nrelocs
	i16 nlines
	i32 checksum
	i16 sectionno
	i32 dummy
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
	u32 exportflags
	u32 timedatestamp
	u16 majorversion
	u16 minorversion
	u32 namerva
	u32 ordinalbase
	u32 naddrtable
	u32 nnamepointers
	u32 expaddressrva
	u32 namepointerrva
	u32 ordtablerva
end
=== mc_genss.m 0 0 15/40 ===
const wmask = 2x1000				!1 means 64-bit operand size
const rmask = 2x0100				!extends mod/rm reg field
const xmask = 2x0010				!extends sib index field
const bmask = 2x0001				!extends mod/rm r/m field, also sib base field

BYTE DEBUG


const wbit = 3

byte rex
byte sizeoverride					!32=>16 switch
byte addroverride					!32=>16 switch
byte f2override						!xmm regs
byte f3override						!xmm regs
byte nowmask						!disable w-bit
byte usesizeb						!1 tests opnd b for wmask

GLOBAL record amoderec =					!return from genrm
!record amoderec =					!return from genrm
	byte modrm						!
	byte sib						!
	i8 usesib						!-1/0/1 = rip/not used/used
	byte dispsize					!0, 1 or 4
	i32 offset					!for dispsize = 1/4
end

mclopnd extraparam
ref[]tempmoderec tempmodes


int currseg=0
ref dbuffer currdata				!copy of ss_idata or ss_code
ref relocrec currrelocs
int nrelocs

[r0..r15]byte ishighreg				!high regs have 0x40 (see start)

REF MCLREC CURRMCL
ref riprec ripentry

macro genbyte(x) = currdata.pcurr++^:=x

!PROC GENBYTE(int x)=
!IF DEBUG THEN CPL "GENBYTE:",X:"2ZH" FI
!	currdata.pcurr++^:=x
!end
!

macro makemodrm(mode,opc,rm) = mode<<6+opc<<3+rm

global proc genss(int obj=0)=
	int index
	ref mclrec m

	return when ssdone

	sstime:=os_clock()

	initlib(mlabelno)
!CPL "INITLAB 50"
!	initlib(50)

	ss_zdatalen:=0
	ss_zdata:=buffercreate()
	ss_idata:=buffercreate()
	ss_code:=buffercreate()
	ss_idatarelocs:=nil
	ss_coderelocs:=nil
	ss_nsymbols:=0

	switchseg(code_seg)

	aaseqno:=9999
	extraparam:=nil

!	fixregvar()

	m:=mccode
	index:=0

	while m do
		doinstr(m,++index)
		m:=m.nextmcl
	od
!CPL "DONE GENSS LOOP"

	switchseg(0)					!update ss_currrelocs etc

	if bufferlength(ss_zdata) then
		axerror("Zdata contains code or data")
	fi

	if obj then					!do fixups needed for .obj files
		ref riprec pr			!(exe module does its own fixups)
		ref byte codeaddr
		ref u32 offsetptr

		codeaddr:=bufferelemptr(ss_code, 0)
			pr:=riplist
			while pr, pr:=pr.next do
				offsetptr:=ref u32(codeaddr+pr.offset)
				offsetptr^-:=pr.immsize
		od
	fi

	ssdone:=1
	sstime:=os_clock()-sstime

end

proc doinstr(ref mclrec m,int index)=
	mclopnd a,b
	psymbol d,e
	int x,offset,shortjmp,n

!CPL "DOINSTR",MCLNAMES[M.OPCODE], M.SEQNO

	if currdata.pend-currdata.pcurr<1024 then
		bufferexpand(currdata)
	fi

	rex:=sizeoverride:=addroverride:=f2override:=f3override:=nowmask:=usesizeb:=0

	a:=m.a
	b:=m.b

	aaseqno:=m.seqno
	aapos:=m.mpos
	ripentry:=nil
	CURRMCL:=M

	switch m.opcode
	when m_procstart then
		currfunc:=m.a.def
		tempmodes:=currfunc.tempmodes

	when m_procend then
	when m_define then

	when m_definereg then
	when m_definetemp then

	when m_labelname then
		case a.valtype
		when stringimm_val then
		when def_val then
			d:=a.def
			d.reftype:=back_ref
			d.segment:=currseg
			d.offset:=getcurrdatalen(6)
			if d.exported then
				getstindex(d)
			fi

			dofwdrefs(d)
		esac

	when m_label then

		if a.valtype=def_val then			!named label (probably from assembler)
			d:=a.def
		else
			d:=labeldeftable[a.labelno]
		fi
	
		d.reftype:=back_ref
		d.segment:=currseg
		d.offset:=getcurrdatalen(6)

		if d.exported then
			getstindex(d)
		fi

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

	when m_neg, m_not, m_mul, m_imul, m_div, m_idiv then
		do_neg(a,mclcodes[m.opcode])

	when m_add, m_sub, m_and, m_or, m_xor, m_adc, m_sbb, m_cmp then
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
			if x<1 or x>16384 then axerror("align2") fi
			buffercheck(currdata, x)
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

	when m_comisd, m_ucomisd then
		do_arithxmm(a,b,0x66,mclcodes[m.opcode])

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

!	when m_param then
!		extraparam:=a

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

	when m_bswap then
		do_bswap(a)

	when m_shld, m_shrd then
		do_dshift(a, b, m.c, mclcodes[m.opcode])

	when m_comment, m_endx then
	else
		println "*** SS:Can't do opcode",mclnames[m.opcode],"line",aaseqno,=M.OPCODE,=M_HALT
	CPL
	CPL
	AXERROR("STOPPING")
!	end switch
	end
!CPL "..........DONE"; STOP WHEN OS_GETCH()=27
end

proc start=
	ishighreg[r3]:=0x40
	ishighreg[r5]:=0x40
	ishighreg[r14]:=0x40
	ishighreg[r15]:=0x40
end

proc genword(int x)=
	addword(currdata,x)
end

proc gendword(int x)=
	adddword(currdata,x)
end

proc genqword(i64 x)=
	addqword(currdata,x)
end

proc genopnd(mclopnd a,int size=0)=
!generate any label/offset/label+offset/immstring part
!ignore reg etc
!any labels, assume abs addresses of 32 or 64 bits
	ref char s
	i64 x
	int length

	if size=0 then size:=a.size fi

!IF A.VALTYPE=DEF_VAL THEN
!	CPL "GENOPND",VALTYPENAMES[A.VALTYPE]
!FI

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
			r32 x32
			x32:=a.xvalue
			gendword(i32@(x32))
!		when realmem_val then
!			CPL "		OPND/REALMEM4"
!		when stringimm_val then
!			CPL "		OPND/STRINGIMM4"
		when def_val,label_val then
			genabs32(a)
!		when name_val then
!			CPL "		OPND/NAME4"
		else
			cpl valtypenames[a.valtype]
			axerror("OPND/4/VALTYPE?")
		esac

	when 8 then
		case a.valtype
		when intimm_val then
			genqword(a.value)
		when realimm_val then
			genqword(i64@(a.xvalue))
!		when realmem_val then
!			CPL "		OPND/REALMEM8",ALINENO
!		when stringimm_val then
!			CPL "		OPND/STRINGIMM8"
		when def_val,label_val then
			genabs64(a)
!		when name_val then
!			CPL "		OPND/NAME8"
		else
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

!	r:=pcm_alloc(relocrec.bytes)
	r:=pcm_allocnfz(relocrec.bytes)
	r.nextreloc:=currrelocs
	r.reloctype:=reloctype
	r.offset:=getcurrdatalen(1)-adjust
	r.stindex:=stindex

	++nrelocs
	currrelocs:=r
end

func getstindex(psymbol d)int=
!retrieve existing obj st index, or create new one
	if d.stindex=0 then
		if ss_nsymbols>=ss_symboltablesize then
			extendsymboltable()
		fi
		d.stindex:=++ss_nsymbols

		ss_symboltable[d.stindex]:=d

		if d.segment=0 then
			if d.imported then
				d.segment:=code_seg
			fi
		fi

	fi
	return d.stindex
end

proc genrel32(mclopnd a)=
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
	else								!external psymbol
		gendword(a.offset)		!this is probably just zero
		addrelocitem(rel32_rel,d)
	esac
end

func getdef(mclopnd a,int dneeded=0)psymbol =
	psymbol d

	if a.mode in [a_mem,a_imm] then
		case a.valtype
		when label_val then
			return labeldeftable[a.labelno]
		when def_val then
			d:=a.def
			if d.reftype=0 then
				if not d.imported then
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

proc genabs32(mclopnd a)=
!absolute refs to labels
	psymbol d

	d:=getdef(a,1)

	case d.reftype
	when back_ref then

		gendword(d.offset+a.offset)
		addrelocitem(addr32_rel,d)

	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(4),addr32_rel,currseg)
		if d.id in [local_id, param_id] then
			gendword(d.offset+a.offset)
		else
			gendword(a.offset)
			addrelocitem(addr32_rel,d)
		fi

	else								!external psymbol
		gendword(a.offset)					!this is probably just zero
		addrelocitem(addr32_rel,d)
	esac
end

proc genabs64(mclopnd a)=
!absolute refs to labels
	psymbol d

	d:=getdef(a,1)

	case d.reftype
	when back_ref then
		genqword(d.offset+a.offset)
		addrelocitem(addr64_rel,d)

	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(5),addr64_rel,currseg)
		if d.id in [local_id, param_id] then
			genqword(d.offset+a.offset)
		else
			genqword(a.offset)
			addrelocitem(addr64_rel,d)
		fi

	else								!external psymbol
		genqword(a.offset)				!this is probably just zero
		addrelocitem(addr64_rel,d)
	esac
end

func getrel32(psymbol d,int offset)int=
!get rel difference between offset in this segment, and label d
	if d.reftype=back_ref then					!defined earlier in this segment
		if d.segment<>currseg then
			axerror("Rel label across segments2")
		fi
		return d.offset-(offset+1)
	else
		return i32.max
	fi
end

proc dofwdrefs(psymbol d)=
!label d has been encountered
!update any fwd refs
!assume inside same offset, at least for rel-32 which only works in text segment
	ref fwdrec f
	int offset, seg
	ref byte p8
	ref i32 p32
	ref i64 p64
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
	if f2override then genbyte(0xF2) fi
	if f3override then genbyte(0xF3) fi
	if sizeoverride then genbyte(0x66) fi
	if addroverride then genbyte(0x67) fi

	if nowmask then rex.[wbit]:=0 fi

	if rex then genbyte(rex iand 15+0x40) fi
end

func isbytesized(i64 x)int=
	return -128<=x<=127
end

func isdwordsized(i64 x)int=
	return i32.min<=x<=i32.max
end

proc genamode(mclopnd a, amoderec am)=
	psymbol d
	ref riprec pr

	genbyte(am.modrm)

	if am.usesib=1 then
		genbyte(am.sib)
	fi

	case am.dispsize			!disp bytes
	when 0 then
	when 1 then
		genbyte(am.offset)
	when 4 then
		if am.usesib=-1 then
			pr:=pcm_alloc(riprec.bytes)
			pr.next:=riplist
			pr.offset:=currdata.pcurr-currdata.pstart
			ripentry:=riplist:=pr
		fi
		case a.mode
		when a_mem then

			case a.valtype
			when def_val, label_val then
				genabs32(a)
			when no_val, temp_val then
				gendword(am.offset)
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

proc setopsize(mclopnd a)=
	case a.size
	when 8 then			!override default 4 bytes
	    rex ior:=wmask
	when 4 then			!assume 4 bytes is default
	when 1 then			!assume set via specific opcodes
	when 2 then			!override default 4 bytes
		sizeoverride:=1
	else
		axerror("Operand size not set")
	esac
end

func getdispsize(mclopnd a, i32 &offset)int=
!look at imm/mem displacement, and return (0,1 or 4) and offset
!0 is returned when no disp is needed (no labeldef and offset is zero)
!unless mand=1 then 1 is returned
	psymbol d

	d:=getdef(a)
	offset:=a.offset

!CPL "GDS", D, VALTYPENAMES[A.VALTYPE], OFFSET

	if d then
		if d.id in [local_id, param_id] then
			offset+:=d.offset
		else
			return 4
		fi
	elsif a.valtype=temp_val and tempmodes then
!CPL "------TEMP", A.TEMPNO, =TEMPMODES, =TEMPMODES[A.TEMPNO].OFFSET
		offset+:=tempmodes[a.tempno].offset

	fi

	if offset then
		return (isbytesized(offset)|1|4)
	else
		return 0
	fi
end

proc checkhighreg(mclopnd a)=
	if a.mode=a_reg then
		rex ior:=ishighreg[a.reg]
	fi
end

proc do_loop(mclopnd a,int opc)=
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

proc do_jcxz(mclopnd a,int opsize)=
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

proc do_call(mclopnd a)=
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

		genxrm(0xFF, 2, a)

	esac
end

proc do_jmp(mclopnd a,ref mclrec m)=
	int am, regcode, offset, shortjmp
	psymbol d:=getdef(a)

	case a.mode
	when a_imm then
		offset:=getrel32(d, getcurrdatalen(11)+1)
		if offset<0 and offset>-126 then
			genbyte(0xEB)
			genbyte(offset)
		else
			shortjmp:=0
			if offset>0 then				!fwd jump
!check if destlabel occurs within next 8 instrs, then likely to need short disp
				shortjmp:=checkshortjump(m, d)
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
		case a.size
		when 0 then a.size:=8
		when 1,2,4 then
			axerror("jmp[]size")
		esac

		genxrm(0xFF, 4, a)
	esac

end

func getcurrdatalen(int id)int=
!I think that zdata-seg is only likely when id=6

	if currseg=zdata_seg then
		return ss_zdatalen
	fi
	return bufferlength(currdata)
end

proc do_cmovcc(int cond, mclopnd a,b)=
	if a.size<>b.size and b.size then
		axerror("1:Opnd size mismatch")
	fi
	if a.size=1 then axerror("cmov/byte") fi

	genrrm(0x0F'40+cond, a, b)
end

proc do_fmem(mclopnd a, int freal, code)=
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
		when 10,16 then
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
	
	genxrm(0xD9+mf<<1, code, a)
end

proc genrel8(mclopnd a)=
!a is a known fwd reference, and expected to be <=127 bytes
	psymbol d

	d:=getdef(a,1)

	if d.reftype=fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(3),rel8_rel)
		genbyte(0)
	else								!external psymbol
		axerror("genrel8")
	fi
end

func checkshortjump(ref mclrec m,psymbol d)int=
!at mccode[index] which should contain a jmp/jmpcc instruction
!d is the labeldef being jumped to
!return 1 if this is certain to be a short jump (8-bit disp) otherwise 0 
!return 0
! d can be a named label, or a labelno; either should have .labelno set
	int n
	mclopnd a

	n:=0
	m:=m.nextmcl
	while m and n<=8 do
		case m.opcode
		when m_label then
			a:=m.a

			case a.valtype
			when label_val then
				if a.labelno=d.labelno then return 1 fi
			when def_val then
				if a.def=d then return 1 fi
			esac

		when m_comment, m_endx then
		when m_resb then
			return 0
		else
			++n
		esac
		m:=m.nextmcl
	od

	return 0
end

func addfwdref(ref fwdrec p, int offset, reltype, seg=0)ref fwdrec=
	ref fwdrec q

!	q:=pcm_alloc(fwdrec.bytes)
	q:=pcm_allocnfz(fwdrec.bytes)
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

proc do_popcnt(mclopnd a,b)=
	int am, regcode

	if b.mode=a_mem then
		if b.size=0 then b.size:=8 fi
	fi

	f3override:=1
	genrrm(0x0F'B8, a, b)
end

proc do_bsf(mclopnd a,b, int opc)=
	int am, regcode

	if b.mode=a_mem then
		if b.size=0 then b.size:=8 fi
	fi
	if a.size<>b.size then axerror("bsf size") fi

	genrrm(0x0F<<8+opc, a, b)
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

global proc initlib(int nlabels)=
	[256]char str
	psymbol d

	ss_symboltable:=pcm_alloc(init_ss_symbols*ref void.bytes)
	ss_symboltablesize:=init_ss_symbols
	ss_nsymbols:=0
	labeldeftable:=pcm_alloc(nlabels*ref void.bytes)

!IF NLABELS<MLABELNO THEN
!CPL "INITLAB: BAD LABEL COUNT"
!CPL "INITLAB: BAD LABEL COUNT"
!!SROP
!STOP
!FI
!
!CPL "//INITLIB", NLABELS
!STOP

	for i to nlabels do
		d:=labeldeftable[i]:=pcm_allocnfz(pstrec.bytes)

!CPL "SETTING LAB",I,"TO",D
		d.labelno:=i
		fprint @&.str,"l#",i
		d.name:=pcm_copyheapstring(&.str)
		d.reftype:=fwd_ref
	od
end

global func buffercreate(int size=1024)ref dbuffer=
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

global func bufferlength(ref dbuffer a)int=
	return a.pcurr-a.pstart
end

global func bufferelemptr(ref dbuffer a, int offset)ref void=
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

global proc addqword(ref dbuffer a, i64 x)=
	a.pcurr64^:=x
	++(a.pcurr64)
end

proc genxrm(int opcode, code, mclopnd b)=
!deal with /d instructions, where code = 0..7
	amoderec am

	setopsize(b)

	am:=genrm(0, code, b, 0)

	case currmcl.opcode
	when m_push, m_pop then rex.[wbit]:=0
	esac


!	if opbytes[2] then genbyte(opbytes[2]) fi		!extra opcodes will not be 0
!	if opbytes[1] then genbyte(opbytes[1]) fi
	if opcode.[16..23] then genbyte(opcode.[16..24]) fi
	genrex()
	if opcode.[8..15] then genbyte(opcode.[8..15]) fi

	genbyte(opcode)
	genamode(b,am)
end

proc genrrm(int opcode, mclopnd a, b)=
!deal with /r instructions; rrm = reg,reg/mem
!opcode represents 1, 2 and maybe 3-byte(?) opcodes, with last in lsb place
!a is a register mclopnd, b is a register or memory mclopnd, always
!when data direction is the other way, as in mov reg/mem, reg, then reverse mclopnds
!Output will be:
! Any override prefixes
! Any rex prefix
! 1 or 2 (or 3?) opcode bytes
! modrm byte
! possible sib byte
! Any address offset (which may be an imm value, or fwd label, or external etc)
!Any additional immediate data that follows should be added by the caller.
!There are two main modes:
! REG, REG   2 registers are involved; there is no address offset, no sib byte
! REG, MEM   1, 2 or 3 registers are involved. Last 2 may be base and index registers,
!            and the index register may be scaled
	amoderec am
!	[0..7]byte opbytes @opcode

!	checkhighreg(a)
	if a.mode=a_reg then rex ior:=ishighreg[a.reg] fi

	setopsize(a)

	if usesizeb then				!wmask comes from b
		rex.[wbit]:=0
		if b.size=8 then rex ior:=wmask fi
	fi

	am:=genrm(a.reg, 0, b, a.mode=a_xreg)

	if opcode.[16..23] then genbyte(opcode.[16..24]) fi
	genrex()
	if opcode.[8..15] then genbyte(opcode.[8..15]) fi

	genbyte(opcode)

genamode(b,am)
end

func getregcode(int reg, int mask, isxreg=0)int regcode=
!convert m-register code (1 to 16/20) to hardware code (0..7 plus any rex bits)
!mask is the rex bit to set for high registers
!isxreg is 1 for float registers, where I don't need to do the usual mapping

!	if not isxreg then
		regcode:=regcodes[reg]
!	else
!		regcode:=reg-xr0
!	fi

	if regcode>=8 then
		regcode-:=8
		rex ior:=mask
	fi
	regcode
end

proc checkimmrange(int value, size)=
	case size
	when 1 then
		unless -128<=value<=255 then axerror("exceeding byte value") end

	when 2 then
		unless -32768<=value<=65535 then axerror("exceeding u16 value") end
	else
		unless -0x8000'0000<=value<=0xFFFF'FFFF then axerror("2:exceeding u32 value") end
	esac
end

func genrm(int reg, opc, mclopnd b, int isxreg=0)amoderec=
!reg =  0:	opc is a 3-bit code that goes in reg field of mod-reg-rm
!reg >= r0:	reg is an M-register code, which is converted to a machine reg encoding
!			of 3 bits (to go in middle of modrm byte), and may set rex.r bit for high
!			regs; opc is 0 in this case
!
!b is mclopnd containing rhs reg value, or is mem mclopnd using base/index regs and addr
!For now, return same encoded value as old genrm

	static []int scaletable=( 0, 1, 0, 2, 0, 0, 0, 3)
	int mode, rm, scale, index, base
	int regix, code, ismem
	amoderec am

	clear am

!deal with first reg/opc field

	if reg then				!else use opc as-is
		opc:=getregcode(reg, rmask, isxreg)
	fi

	case b.mode
	when a_reg, a_xreg then			!modrm can only ref to a single register
		rm:=getregcode(b.reg, bmask, b.mode=a_xreg)
		rex ior:=ishighreg[b.reg]

		am.modrm:=makemodrm(3,opc,rm)
		return am

	when a_mem then
		ismem:=1
		case b.valtype
		when def_val then
			if b.def.id=static_id then ismem:=2 fi
		when realmem_val then ismem:=2
		when label_val then ismem:=2
		esac

	else
		axerror("genrm not mem")
	esac

	mode:=rm:=0				!modrm is (mode, x, rm), of (2,3,3) bits
	scale:=0				!0=modrm only; 1/2/4/8 means sib used

	reg:=b.reg
	regix:=b.regix

	if reg=regix=0 then						!address only
		mode:=0
		rm:=4
		scale:=1
		index:=4
		base:=5
		am.dispsize:=4

	elsif b.scale<=1 and regix=0 then			!simple address mode (no sib)
SIMPLE:
		am.dispsize:=getdispsize(b, am.offset)
		if am.dispsize then
			mode:=(am.dispsize=1|1|2)
		fi

		rm:=base:=getregcode(reg, bmask)

		if rm<>4 then
			if rm=5 and am.dispsize=0 then
				mode:=1; am.dispsize:=1
			fi
			index:=0
		else
			index:=4				!means no index
			scale:=1				!force sib

		fi
	elsif regix and reg=0 then

IF B.SCALE<=1 THEN					!try and avoid sib
		SWAP(REG, REGIX)
		GOTO SIMPLE
FI

		am.dispsize:=4
		mode:=0
		rm:=4
		scale:=(b.scale|b.scale|1)
		base:=5
		index:=getregcode(regix, xmask)
		if regix=rstack then axerror("Scaled rstack?") fi

	else									!assume regix used; optional reg and disp
		am.dispsize:=getdispsize(b, am.offset)
		if am.dispsize then
			mode:=(am.dispsize=1|1|2)
		fi
		rm:=4

		scale:=(b.scale|b.scale|1)
!CP "SCAD"
		if reg=0 then
			base:=5
		else
			if reg in [rframe,r7] and am.dispsize=0 then
				mode:=1; am.dispsize:=1
			fi
			base:=getregcode(reg, bmask)
		fi

		if regix=0 then
			index:=4
		else
			index:=getregcode(regix, xmask)
			if not reg then
				am.dispsize:=4
			fi
			if regix=rstack and scale>1 then axerror("Can't scale rstack") fi
		fi
	fi

	if scale then
		am.sib:=scaletable[scale]<<6 + index<<3 + base
		am.usesib:=1
	fi

	if am.dispsize=4 and ismem then
		if reg or regix then
			if phighmem=2 AND ISMEM=2 then
				CPL "Addr32 can't use RIP, line",aaseqno,STRMCLSTR(CURRMCL)
			fi
		elsif phighmem then
			am.usesib:=-1
			mode:=0
			rm:=5
		fi
	fi

	am.modrm:=makemodrm(mode, opc, rm)

	return am
end

proc do_arith(mclopnd a,b,int code)=
!code is 3-bit 0..7 value indicating which of add, sub, and, or, xor, adc, sbb, cmp
!ops is being done
	psymbol d
	int opc, dispsize
	i64 x

	case a.mode
	when a_reg then
		case b.mode
		when a_reg,a_mem then
			opc:=code<<3 ior (a.size=1|0x02|0x03)
			genrrm(opc, a, b)

		when a_imm then
	doregimm:
			d:=getdef(b)
			if d then
				if a.size<4 then axerror("add imm/size") fi
				genxrm(0x81, code, a)
				genopnd(b,4)
				return
			fi

			x:=b.value
			dispsize:=1
			if a.size=1 then
				opc:=0x80
!				if x not in -128..127 then axerror("Exceeding i8 range") fi
				checkimmrange(x,1)
				if x not in -128..255 then axerror("Exceeding i8/u8 range") fi
			elsif -128<=x<=127 then
				opc:=0x83
			else
				checkimmrange(x,4)
				opc:=0x81
				dispsize:=(a.size=2|2|4)
			fi

			genxrm(opc, code, a)

			case dispsize
			when 1 then genbyte(x)
			when 2 then genword(x)
			when 4 then gendword(x)
			esac
			fixrip(dispsize)

		else
			axerror("ADD reg,???")
		esac

	when a_mem then
		case b.mode
		when a_reg then
			opc:=code<<3 ior (b.size=1|0x00|0x01)
			genrrm(opc, b, a)

		when a_imm then
			goto doregimm
		else
			axerror("ADD mem,???")
		esac

	else
	cpl opndnames_ma[code],=CODE
		axerror("1:Can't add to this opnd")
	esac
end

proc do_mov(mclopnd a,b)=
	int regcode, opc, dispsize
	i64 value
	psymbol d:=getdef(b)

	case a.mode
	when a_reg then
		case b.mode
		when a_reg, a_mem then
			if a.size<>b.size and b.size then axerror("2:Opnd size mismatch") fi

			genrrm((a.size=1|0x8A|0x8B), a, b)

		when a_imm then
			value:=b.value

			regcode:=getregcode(a.reg, bmask)
			setopsize(a)
			if d and a.size<=2 then axerror("mov imm?") fi

			CHECKHIGHREG(A)

			case a.size
			when 1 then
				unless -128<=value<=255 then axerror("exceeding byte value") end
				genrex()
				genbyte(0xB0+regcode)
				genbyte(value)

			when 2 then
				unless -32768<=value<=65535 then axerror("exceeding u16 value") end
				genbyte(0x66)
				genrex()
				genbyte(0xB8+regcode)
				genword(value)
			when 4 then
				if d then
					genrex()
					genbyte(0xB8+regcode)
					genopnd(b,4)
				else
					unless -0x8000'0000<=value<=u32(0xFFFF'FFFF) then
						CPL value,ref void(value)
						axerror("1:exceeding u32 value")
					end
doreg32:
					genrex()
					genbyte(0xB8+regcode)
					gendword(value)
				fi

			else							!assum 8 bytes
				if d then
					rex ior:=wmask
					genrex()
					genbyte(0xB8+regcode)
					genopnd(b,8)
				else
					if value>=0 and value<=0xFFFF'FFFF then		!mov r64,imm -> r32,imm
						rex.[wbit]:=0
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

			if a.size=0 then a.size:=b.size fi
			if a.size<>b.size and a.size then axerror("3:Opnd size mismatch") fi
			genrrm((b.size=1|0x88|0x89), b, a)

		when a_imm then
			value:=b.value

			if a.size=0 then a.size:=1 fi
			if d and a.size<=2 then axerror("mov imm?") fi
			setopsize(a)
			opc:=(a.size=1|0xC6|0xC7)

			if not d then checkimmrange(value, a.size) fi

			genxrm(opc, 0, a)
			value:=b.value

			dispsize:=a.size
			case a.size
			when 1 then
				genbyte(value)
	
			when 2 then
				genword(value)
			when 4,8 then
				genopnd(b,4)
				dispsize:=4
			esac
			fixrip(dispsize)	
		else
			axerror("MOV MEM/?")
		esac
	else
		axerror("MOV ?/..")
	esac
end

proc do_push(mclopnd a)=
	int code

	if a.size=0 then a.size:=8 fi

	case a.mode
	when a_reg then
		if a.size<>8 then axerror("pushreg not 64-bit") fi
		code:=getregcode(a.reg, bmask)
		rex.[wbit]:=0
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
		genxrm(0xFF, 6, a)

	else
		axerror("push opnd?")
	esac
end

proc do_pop(mclopnd a)=
	int code

	if a.size=0 then a.size:=8 fi

	case a.mode
	when a_reg then
		if a.size<>8 then axerror("popreg not 64-bit") fi
		code:=getregcode(a.reg, bmask)
		genrex()
		genbyte(0x58+code)

	when a_mem then
		if a.size<>8 then axerror("pop not 64-bit") fi
		genxrm(0x8F, 0, a)
	else
		axerror("pop opnd?")
	esac
end

proc do_inc(mclopnd a,int code)=
!inc/dec

	case a.mode
	when a_reg, a_mem then
		genxrm((a.size=1|0xFE|0xFF), code, a)
	else
		axerror("inc/opnd?")
	esac
end

proc do_neg(mclopnd a,int code)=
!neg/not/mul/imul/div/idiv
	case a.mode
	when a_reg, a_mem then
		genxrm((a.size=1|0xF6|0xF7), code, a)
	else
		axerror("neg/div/etc opnd?")
	esac
end

proc do_lea(mclopnd a,b)=
	int regcode, am

	unless a.mode=a_reg and b.mode=a_mem then
		axerror("LEA not reg/mem")
	end

	if a.size<4 then
CPL =A.SIZE
 axerror("LEA size error") fi

	genrrm(0x8D, a, b)
end

proc do_movsx(mclopnd a,b,int opc)=
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

	genrrm(0x0F<<8+(b.size=1|opc|opc+1), a, b)
end

proc do_exch(mclopnd a,b)=
	int regcode, am

	if a.mode=a_reg and b.mode=a_reg and (a.reg=r0 or b.reg=r0) and a.size<>1 then		!simple r0/reg
		if a.reg<>r0 then				!get a to be r0
			swap(a,b)
		fi
		if a.size<>b.size then axerror("exch size") fi

		setopsize(a)
		regcode:=getregcode(b.reg, bmask)
		genrex()
		genbyte(0x90+regcode)
		return
	fi

	if a.mode=a_mem then swap(a,b) fi

	unless a.mode=a_reg and (b.mode=a_reg or b.mode=a_mem) then axerror("exch opnds") end
	if b.size=0 and b.mode=a_mem then b.size:=a.size fi
	if a.size<>b.size then axerror("exch size") fi

	genrrm((a.size=1|0x86|0x87), a, b)
end

proc do_movsxd(mclopnd a,b)=
	int regcode, am

	if b.mode=a_mem and b.size=0 then b.size:=4 fi

	if a.size<>8 or b.size>4 then axerror("movsxd size") fi

	if a.mode<>a_reg or (b.mode<>a_reg and b.mode<>a_mem) then
		axerror("movsxd opnds")
	fi

	genrrm(0x63, a, b)
end

proc do_imul2(mclopnd a,b)=
	int regcode, am, opc, dispsize
	i64 value

	if a.mode<>a_reg then
		axerror("imul2 opnds")
	fi
	if b.size=0 then b.size:=a.size fi
	if a.size=1 then axerror("imul2 byte") fi

	case b.mode
	when a_reg,a_mem then
		if a.size<>b.size then axerror("imul2 size") fi

		genrrm(0x0F'AF, a, b)

	when a_imm then						!imul reg1,reg2,imm but implemented as imul reg,imm
		if getdef(b) then axerror("mul/label") fi
		value:=b.value

		if -128<=value<=127 then
			opc:=0x6B
		else
			opc:=0x69
		fi

		genrrm(opc, a, a)

		if -128<=value<=127 then
			genbyte(value)
			dispsize:=1
		elsif a.size=2 then
			genword(value)
			dispsize:=2
		else
			gendword(value)
			dispsize:=4
		fi
		fixrip(dispsize)
	else
		axerror("imul2 opnds")
	esac
end

proc do_shift(mclopnd a,b,int code)=
	int w,opc,needdisp

	if a.mode<>a_reg and a.mode<>a_mem then axerror("shift opnds1?") fi
	if getdef(b) then axerror("shift/label") fi
	w:=(a.size=1|0|1)
	needdisp:=0

	case b.mode
	when a_imm then
		if b.value=1 then
			opc:=0xD0+w
		else
			opc:=0xC0+w
			needdisp:=1
		fi
	when a_reg then
		if b.reg<>r10 or b.size<>1 then axerror("cl or b10 needed") fi
		opc:=0xD2+w
	else
		axerror("shift opnds2?")
	esac

	genxrm(opc, code, a)

	if needdisp then genbyte(b.value); fixrip(1) fi
end

proc do_test(mclopnd a,b)=
	i64 value
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
		genxrm((a.size=1|0xF6|0xF7), 0, a)

		case a.size
		when 1 then
			genbyte(value)
		when 2 then
			genword(value)
		else
			gendword(value)
		esac
		fixrip(a.size)

	elsif a.mode in [a_reg, a_mem] and b.mode=a_reg then
domemreg:
		genrrm((a.size=1|0x84|0x85), a, b)

	elsif a.mode=a_reg and b.mode=a_mem then
		swap(a,b)
		goto domemreg
	else
		axerror("test opnds")
	fi

end

proc do_setcc(int cond, mclopnd b)=
!a is cond
!b is byte reg/mem

	if b.mode not in [a_reg, a_mem] or b.size>1 then axerror("setcc opnd/size") fi

	genxrm(0x0F'90+cond, 0, b)
end

proc checksize(mclopnd a, int size1=0, size2=0)=
	if a.size=0 then axerror("Need size") fi
	if size1 and a.size not in [size1,size2] then
		CPL =A.SIZE
		axerror("Wrong size")
	fi
end

proc do_arithxmm(mclopnd a,b,int prefix,opc)=
	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("arithxmm opnds")
	fi

	if prefix then genbyte(prefix) fi
	genrrm(0x0F<<8+opc, a, b)
end

proc do_logicxmm(mclopnd a,b,int opc,size)=
	int am, regcode

	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("logicxmm opnds")
	fi

	if size=8 then genbyte(0x66) fi

	genrrm(0x0F<<8+opc, a, b)
end

proc do_convertfloat(mclopnd a,b,int prefix)=
!cvtss2sd and cvtsd2ss
	int am, regcode

	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("convertfloat opnds")
	fi
	genbyte(prefix)
	nowmask:=1
	genrrm(0x0F'5A, a,b)
end

proc do_fix(mclopnd a,b,int prefix,opc)=
	int am, regcode

	if a.mode<>a_reg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("fix opnds")
	fi

	checksize(a, 4, 8)
	
	b.size:=(prefix=0xF3|4|8)

	genbyte(prefix)
	genrrm(0x0F<<8+opc, a, b)
end

proc do_float(mclopnd a,b,int prefix)=
!cvtss2si and cvtsd2si
	if a.mode<>a_xreg or (b.mode<>a_reg and b.mode<>a_mem) then
		axerror("float opnds")
	fi

	checksize(b, 4, 8)
!
	a.size:=(prefix=0xF3|4|8)

	genbyte(prefix)
	usesizeb:=1
	genrrm(0x0F'2A, a, b)
end

proc do_movxmm(mclopnd a,b,int size)=
!do movd/movq depending on size being 4 or 8
	int am, regcode, regcode1, regcode2

	case a.mode
	when a_reg then
		case b.mode
		when a_xreg then
			if a.size<>size then axerror("1:movdq size") fi
			b.size:=a.size

			sizeoverride:=1
			genrrm(0x0F'7E, b, a)

		else
			axerror("movdq reg,?")
		esac
	when a_xreg then
		case b.mode
		when a_reg then
			a.size:=b.size
			if b.size<>size then axerror("3:movdq size") fi
			sizeoverride:=1
			genrrm(0x0F'6E, a, b)

		when a_xreg then
			a.size:=b.size
			f3override:=1
			genrrm(0x0F'7E, a, b)

		when a_mem then
			if b.size=0 then b.size:=a.size fi
!			if b.size<>size then axerror("31:movdq size") fi
			if b.size<>size then axerror("31:movdq size") fi
!
			if size=4 then
				sizeoverride:=1
				nowmask:=1
				genrrm(0x0F'6E, a, b)

			else
				f3override:=1
				nowmask:=1
				genrrm(0x0F'7E, a, b)
			fi

		else
			axerror("movdq xreg,?")
		esac
	when a_mem then
		case b.mode
		when a_xreg then
			if a.size and a.size<>size then axerror("5:movdq size") fi

			sizeoverride:=1
			genrrm((size=4|0x0F'7E|0x0F'D6), b,a)

		else
			axerror("movdq mem,?")
		esac

	else
		axerror("movdq opnds")
	esac
end

proc fixrip(int dispsize)=
	ref byte codeaddr
	ref u32 offsetptr

	if not ripentry then return fi

	case dispsize
	when 0 then return
	when 1,2,4 then
	else
CPL =DISPSIZE
		axerror("fixrip disp?")
	esac
	ripentry.immsize:=dispsize
end

proc do_bswap(mclopnd a)=
	int code
	if a.mode<>a_reg or a.size<4 then axerror("bswap reg>") fi

	setopsize(a)

	code:=getregcode(a.reg, bmask)

	genrex()
	genbyte(0x0F)
	genbyte(0xC8 + code)
end

proc do_movdqx(mclopnd a, b, int prefix)=
	prefix:=prefix<<16 + 0x0F<<8

	if a.size=0 then a.size:=16 fi
	if b.size=0 then b.size:=16 fi

	if a.mode=a_mem then
		genrrm(prefix+0x7F, b, a)
	else
		genrrm(prefix+0x6F, a, b)
	fi
end

proc do_dshift(mclopnd a, b, int c, opc)=

	if a.size=0 then a.size:=b.size fi
	if a.size<>b.size or a.size<=1 then axerror("dshift/size") fi

	sizeoverride:=0
	genrrm(0x0F<<8+opc, b, a)
	genbyte(c)
end
=== mc_writeexe.m 0 0 16/40 ===
!Create .exe file from SS-data (code, data, reloc and psymbol tables)

[maxplibfile]i64 libinsttable
[maxplibfile]ichar libinstnames
[maxplibfile]int libnotable			!index into dlltable

global const zsect=3
global const dsect=2
global const csect=1
global const isect=4

record basereloc =
	ref basereloc nextitem
	u32 address				!virtual address
	i32 reloctype
end

ref basereloc basereloclist
int nbaserelocs
int maxrelocaddr
const maxbaseblock=500
[maxbaseblock]int blockbases
[maxbaseblock]i32 blockcounts
[maxbaseblock]i32 blockbytes
[maxbaseblock]byte blockpadding
int nbaseblocks
int basetablesize


const filealign = 512
!const filealign = 32
const sectionalign = 4096
const exe_imagebase = 0x40'0000
const dll_imagebase = 0x1000'0000

global int imagebase

int imagesize
int filesize
ref[]i64 thunktable				!point into code segment
int fileiatoffset
int fileiatsize
psymbol stentrypoint				!psymbol to be the entry point
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

global proc writeexe(ichar outfile, int dodll, ichar entrypoint=nil)=
	return when exedone

	genexe1(entrypoint, outfile, dodll)
	genexe2(outfile, dodll)

	exedone:=1
end

global proc genexe1(ichar entrypoint, outfile, int dodll)=
!manipulate the ss data to fill in all the details needed for exe format
	int offset
	ref byte codeaddr				!mem address of start of code seg
	ref u32 offsetptr

	initsectiontable()

	dllfilename:=extractfile(outfile)
	isdll:=dodll

	imagebase:=(isdll|dll_imagebase|exe_imagebase)

	userentrypoint:=entrypoint
	loadlibs()
	scanst()				!build dll/import tables

	getoffsets()

	relocdata(&sectiontable[csect])
	relocdata(&sectiontable[dsect])

	codeaddr:=bufferelemptr(sectiontable[csect].data, 0)

	if phighmem then
		ref riprec pr

		pr:=riplist
		while pr, pr:=pr.next do
			offsetptr:=ref u32(codeaddr+pr.offset)
			offset:=getripoffset(pr.offset, offsetptr^-imagebase, pr.immsize)
			offsetptr^:=offset	
		od
	fi
end

global proc genexe2(ichar outfile, int dodll)=
!construct the exe image in memory, then write out the file
	imagefileheader header
	optionalheader optheader
	int offset,i
	i64 aa

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

	if pverbose=2 then
		println "EXE size:  ", dataptr-datastart:"10s,jr"
		println
	fi

	if writefile(outfile,datastart,dataptr-datastart)=0 then
		println "Error writing exe file (possibly still running)"
		stop 1
	fi
end

proc loadlibs=
!load library instances
	int i
	i64 hinst
	ichar file
	[300]char filename

	for i to nplibfiles when plibfiles[i]^<>'$' do
		strcpy(&.filename, plibfiles[i])
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

	if pverbose then
		println "Code size: ", bufferlength(ss_code):"10s,jr","bytes"
		if pverbose=2 then
			println "Idata size:", bufferlength(ss_idata):"10s,jr"
			println "Code+Idata:", bufferlength(ss_code)+bufferlength(ss_idata):"10s,jr"
			println "Zdata size:", ss_zdatalen:"10s,jr"
		fi
	fi

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

func extractlibname(ichar name, int &libno,moduleno)ichar=
!if name contains a dot, eg lib.abc, then set libno to index of "lib", and return "abc"
!otherwise return original name
	ref char s,name2
	[256]char str
	[256]char str2
	int i

	name2:=nil

	reenter:
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
		CPL NAME
		axerror("Can't find external function")
	od

!found in search lib n
	if libno:=libnotable[n] then			!already added this library
		++dlltable[libno].nprocs
		return name
	fi

!first use of this lib
	strcpy(&.str, plibfiles[n])
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
	ichar name, libname, basename

	for i:=1 to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.imported then
			if nimports>=maximports then axerror("genexe: Too many imports") fi
			++nimports
			name:=extractlibname(d.name,libno,1)
			importtable[nimports].libno:=libno			!0 if no lib
			importtable[nimports].name:=name				!original, or 2nd part of lib.name
			importtable[nimports].def:=d

			d.importindex:=nimports
		elsif d.exported then
			basename:=getbasename(d.name)
			if userentrypoint then
				if eqstring(basename,userentrypoint) then
					stentrypoint:=d
				fi
			else
!				if eqstring(basename,"main") and not isdll then
				if d.isentry and not isdll then
					stentrypoint:=d
				fi
			fi

			if nexports>=maxexports then axerror("gendll: Too many exports") fi
			++nexports

			exporttable[nexports].def:=d
			exporttable[nexports].name:=getbasename(d.name)
		fi
	od
end

proc relocdata(ref sectionrec s)=
	ref sectionrec u
	ref relocrec r
	ref byte p
	ref u32 p32
	ref u64 p64
	psymbol d
	int offset,index,thunkoffset,iatoffset

	p:=bufferelemptr(s.data,0)
	r:=s.relocs

	while r do
		d:=ss_symboltable[r.stindex]
		index:=d.importindex				!into importtable
		thunkoffset:=importtable[index].thunkoffset

		case r.reloctype
		when rel32_rel then
			if not d.imported then
				axerror("rel32/not imported")
			fi
			(ref u32(p+r.offset)^:=thunkoffset-r.offset-4)
!
		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d.imported then
				(ref u32(p+r.offset)^:=imagebase+thunkoffset+sectiontable[csect].virtoffset)
			else
				u:=nil
!CPL =D.NAME, D.SEGMENT, =D
				case d.segment
				when zdata_seg then u:=&sectiontable[zsect]
				when idata_seg then u:=&sectiontable[dsect]
				when code_seg then u:=&sectiontable[csect]
				else
					PRINTLN D.NAME,D.SEGMENT
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
!	ref sectionrec u
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
			if d.imported then
			else

IF R.RELOCTYPE=ADDR32_REL THEN
!PRINTLN "??BASE RELOC",(D.SEGMENT=CODE_SEG|"CODE"|"DATA"),(R.RELOCTYPE=ADDR32_REL|"ADDR32"|"ADDR64")
ELSE
				newbasereloc(s.virtoffset+r.offset, r.reloctype)
FI

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
				axerror("Entry point not found: main")
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
!	header.subsystem:=2

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

	i64 aa
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

proc writeexporttable(ref byte pstart)=
	const maxexports=2000
	[maxexports]int sortindex
	ref exportdirrec phdr := cast(pstart)
	ref u32 paddrtable
	ref u32 pnametable
	ref u16 pordtable
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

func getexporttablesize:int=
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

	p:=pcm_allocnfz(basereloc.bytes)
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
!	for i to nbaseblocks do
		if blockcounts[i].odd then
			++blockcounts[i]
			++blockpadding[i]
		fi
		blockbytes[i]:=blockcounts[i]*2+8
		basetablesize+:=blockbytes[i]
	od
end

proc writebasereloctable(ref byte pstart)=
	
	ref u32 p32
	ref u16 p16
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
!Sort exporttable by name. This is done by building a set of sorted indices into
!sortindex
	psymbol d,e

!First, store 1..nexports into sortindex
	for i to nexports do
		sortindex[i]:=i
	od

!do bubble sort for now
	repeat
		int swapped:=0
		for i:=1 to nexports-1 do

			d:=exporttable[sortindex[i]].def
			e:=exporttable[sortindex[i+1]].def

			if strcmp(getbasename(d.name), getbasename(e.name))>0 then
				swapped:=1
				swap(sortindex[i], sortindex[i+1])
			fi
		od
	until not swapped

end

func getsectionno(int segment)int=
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
	ref i64 paddr,pname
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
	ref u32 pextra

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

	for i to nimports do
		importtable[i].thunkoffset:=thunkptr-codebase
		if phighmem=0 then
			thunkptr++^:=0x48
			thunkptr++^:=0xFF
			thunkptr++^:=0x24
			thunkptr++^:=0x25
			thunkaddr:=imagebase+importtable[i].iatoffset
			(ref i32(thunkptr)^:=thunkaddr)
			thunkptr+:=4
		else					!use rip mode

			thunkptr++^:=0x48
			thunkptr++^:=0xFF
			thunkptr++^:=0x25
			thunkaddr:=imagebase+importtable[i].iatoffset
			(ref i32(thunkptr)^:=getripoffset(int(thunkptr-codebase),thunkaddr-imagebase))
			thunkptr+:=4
			thunkptr++^:=0x90
		fi
	od
!-----------------------------------------------
end

func getripoffset(int addr, dest, int extra=0)int=
!work out the rip offset for a d32 field at <addr>, to <dest>
!opbytes is the number of opcode bytes that precede the field
!addr is offset of d32 field with codeseg, from start of code segment
!dest is offset within image, relative to imagebase
!extra is 0/1/2/4 bytes of imm data that some instructions will have

	addr+:=sectiontable[csect].virtoffset		!now is offset rel to imagebase
	dest-(addr+4)-extra
end

=== mc_writess.m 0 0 17/40 ===

export function writessdata(int fexe)ref strbuffer=
	gs_init(pdest)
GS_STRLN(PDEST,"HELLO/SS")
	showssdata(fexe)

	gs_line(pdest)
	return pdest
end

proc showssdata(int fexe)=
	tclerror("SS not done") when not ssdone

	gs_strln(pdest,(fexe|"EXE FORMAT"|"AFTER GENSS"))

!	showsections()

	gs_line(pdest)

!	showsectionrelocs2("Idata",ss_idatarelocs,ss_nidatarelocs)
!	showsectionrelocs2("Code",ss_coderelocs,ss_ncoderelocs)

	gs_str(pdest,"proc Section Zdata: ")
	gs_strint(pdest,ss_zdatalen)
	gs_line(pdest)

	showsectiondata(&sectiontable[dsect])
	showsectioncode(&sectiontable[csect])
	if fexe then
		showsectiondata(&sectiontable[isect])
	fi

	showsymboltable2()
	showimporttable()
	gs_strln(pdest,"END OF GENSS")

end

proc showsectiondata(ref sectionrec d)=
int i,k,length,bb
	[128]char str,str2
	ref byte p

	gs_str(pdest,"proc Section ")
	gs_str(pdest,d.name)
	gs_str(pdest," Size:")
	gs_strint(pdest,d.virtsize)
	gs_line(pdest)
	gs_line(pdest)

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

	gs_str(pdest,&.str2)

	for i:=1 to length do
		bb:=p++^
		print @&.str2,bb:"z2H",," "
		gs_str(pdest,&.str2)

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
					gs_str(pdest,"   ")
					strcat(&.str," ")
				od
			fi
			gs_str(pdest,"	[")
			gs_str(pdest,&.str)
			gs_strln(pdest,"]")
			k:=0
			str[1]:=0
			baseaddr+:=16
			print @&.str2,baseaddr:"z8h",,": "
			gs_str(pdest,&.str2)
		fi
	od
	if k=0 then
		gs_line(pdest)
	fi

	gs_line(pdest)
	if k then gs_line(pdest) fi
end

proc showsectioncode(ref sectionrec p)=
ref byte codeptr,codeend,codestart
	int length,offset
	ichar s
	[16]char str

	gs_strln(pdest, "proc Section Code")

	length:=p.virtsize
	codestart:=codeptr:=bufferelemptr(p.data,0)
	codeend:=codeptr+length

	ref byte baseaddr:=cast(imagebase+p.virtoffset)

!INT TT:=CLOCK(), N:=0
	while codeptr<codeend do
		offset:=codeptr-codestart
		s:=decodeinstr(codeptr,baseaddr+offset)
!++N
		exit when s=nil

		print @&.str,offset:"4",," "
		gs_str(pdest,&.str)

		gs_strln(pdest,s)
	od
!TT:=CLOCK()-TT
!CPL "DECODE TIME:",TT,N
!OS_GETCH()
	gs_line(pdest)
end

proc showsectionrelocs2(ichar caption,ref relocrec relocs, int nrelocs)=
	ref relocrec r

	gs_str(pdest,"proc Section Relocs: ")
	gs_str(pdest,caption)
	gs_str(pdest," ")
	gs_strint(pdest,nrelocs)
	gs_line(pdest)

	r:=relocs

	while r do

		gs_str(pdest,"Reloc: ")
		gs_str(pdest,relocnames[r.reloctype])
		gs_str(pdest," Offset: ")
		gs_strint(pdest,r.offset)
		gs_str(pdest," ST Index: ")
		gs_strint(pdest,r.stindex)
		gs_str(pdest," ")
		gs_str(pdest,ss_symboltable[r.stindex].name)
		gs_line(pdest)

		r:=r.nextreloc
	od
	gs_line(pdest)

end

proc gs_value(ichar caption, i64 value)=
	[256]char str

	strcpy(&.str,caption)
	strcat(&.str,":")
	ipadstr(&.str,20)
	gs_str(pdest,&.str)

	fprint @&.str,"0x# #",value:"H",value
	gs_strln(pdest,&.str)
end

proc showsymboltable2=

	gs_strln(pdest,"Proc Symbol Table")
	int i
	for i:=1 to ss_nsymbols do
		gs_strint(pdest,i)
		gs_str(pdest,": ")
		gs_strln(pdest,ss_symboltable[i].name)
	od
	gs_line(pdest)
end

proc showimporttable=
	[256]char str
	dllrec d
	importrec p


	gs_strln(pdest,"Proc Dll List")
	int i
	for i:=1 to ndlls do
		gs_strint(pdest,i)
		gs_str(pdest,": ")
		gs_str(pdest,dlltable[i].name)
		gs_str(pdest," ")
		gs_strint(pdest,dlltable[i].nprocs)
		gs_line(pdest)
		gs_value("		Name Table Offset",dlltable[i].nametableoffset)
		gs_value("		Addr Table Offset",dlltable[i].addrtableoffset)
		gs_value("		DLL Name Offset  ",dlltable[i].dllnameoffset)
	od
	gs_line(pdest)
	gs_strln(pdest,"Proc Import List")

	for i:=1 to nimports do
		p:=importtable[i]

		gs_strint(pdest,i)
		gs_str(pdest,": ")
		if p.libno then
			strcpy(&.str,p.name)
			ipadstr(&.str,16)
			gs_str(pdest,&.str)
			gs_str(pdest," (")
			gs_str(pdest,dlltable[p.libno].name)
			gs_strln(pdest,")")

			gs_value("	IAT Offset        ",p.iatoffset)
			gs_value("	Thunk Offset      ",p.thunkoffset)
			gs_value("	Hint/Name Offset  ",p.hintnameoffset)

		else
			strcpy(&.str,p.name)
			ipadstr(&.str,20)
			gs_str(pdest,&.str)
			gs_strln(pdest," (---)")
		fi
	od
	gs_line(pdest)
end

proc showsections=
	sectionrec s
	int i

	gs_strln(pdest,"proc Section Headersxxx")
	gs_line(pdest)

	for i:=1 to nsections do
		s:=sectiontable[i]

		gs_str(pdest,"Section ")
		gs_strint(pdest,i)
		gs_str(pdest,": ")
		gs_str(pdest,s.name)
		gs_str(pdest,"  (")
		gs_str(pdest,segmentnames[s.segtype])
		gs_strln(pdest,")")

		gs_value("    Raw Offset",s.rawoffset)
		gs_value("    Raw Size",s.rawsize)
		gs_value("    Virtual Offset",s.virtoffset)
		gs_value("    Virtual Size",s.virtsize)
		gs_value("    Nrelocs",s.nrelocs)
		gs_value("    Data",int(s.data))
		gs_line(pdest)

	od
end

=== mc_disasm.m 0 0 18/40 ===

const showmregs=1
!const showmregs=0

const halt=0xF4

int nmodules
int xfchsmask_pd

enumdata [0:]ichar opnames =
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

enumdata []ichar addrmodenames=		! rm modes
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
int ripmode							!1 for rip-relative
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

	retry:						!back here after prefix byte seen

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
		if sizeoverride then
			genintd(readint16())
		else
			genintd(readint32())
		fi

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
	doexch:
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
!		println "	end of code [halt]"
!		return nil

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
	doff:
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
	end switch


!at this point, deststr contains the decoded instruction
!need to put in address, bytes etc

!	if baseaddr then
		print @&.str,baseaddr:"z6h",,": "
!	else
!		print @&.str,pstart:"z6h",,": "
!	fi

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
	doarith:
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
	error:
		genstr("Unknown opcode 2-byte opcode: 0F ")
    genhex(opc)
	end switch
end

proc decodeaddr(int w=0)=
!codeptr points to modrm byte, with possible sib and/or disp following
!decode modrm, sib and disp
!store result in amode:
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
	ripmode:=0

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
			ripmode:=1
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
	return (ref i8(codeptr++))^
end

function readword16:word=
	word a
	a:=ref u16(codeptr)^
	codeptr+:=2
	return a
end

function readint16:int=
	int a
	a:=ref i16(codeptr)^
	codeptr+:=2
	return a
end

function readword32:word=
	word a
	a:=ref u32(codeptr)^
	codeptr+:=4
	return a
END

function readint32:int=
	int a
	a:=ref i32(codeptr)^
	codeptr+:=4
	return a
END

function readi64:i64=
	i64 a
	a:=ref i64(codeptr)^
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

	if ripmode then
		genstr("rip:")
	fi
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
GENSTR("<INDEX>")
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
!	if addrmode=amrel then genstr("+RIP") fi
end

proc genstr(ichar s)=
	strcat(&.deststr,s)
end

proc genintd(i64 a)=
	genstr(strint(a))
end

proc genhex(i64 a)=
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

function readimm8:i64=
!like readimm but can 8 bytes too
	if opsize<8 then return readimm() fi
	return readi64()
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
		case reg
		when 5 then reg:=17
		when 6 then reg:=19
		when 7 then reg:=20
		when 8 then reg:=18
		esac

!		reg+:=12				!5..8 => 17..20


	fi
end

proc getsilx(int &reg)=
!as getsil but used for basereg, which must have addrmode=amreg
	if addrmode=amreg and opsize=1 and rex=0 and reg>=5 and reg<=8 then
		case reg
		when 5 then reg:=17
		when 6 then reg:=19
		when 7 then reg:=20
		when 8 then reg:=18
		esac

!		reg+:=12				!5..8 => 17..20
	fi
end
=== mx_decls.m 0 0 19/40 ===
!Declarations for M-Code scheme
!Terms:
! MCU		MCode Unit, binary code/data/imports/relocs for whole program (LIBREC)
! MCB		MCU rendered to flat data block, written out as .mx/.ml file
! MCX		MCU with allocations, imports and fixups done to make it ready to run
! LIB		Informal reference to MCU or MCX in-memory data; or to MX/ML file
!
! MCU is created from SS data (which normally was used to generate EXE)
! An MCU block is either converted to MCB which is then sent to an MX/ML file;
! or it is directly fixed up into MCX to allow immediate execution
!
! MCB can be read from a file into a memory block. MCB data contains a lineat
! set of tagged data blocks. Those blocks are scanned to form a normal MCU/LIBREC
! data-structure. That MCU can be fixed up like above to make it ready to be
! run or any function to be called.


!single byte tags in mcx file

export const mcxsig = 'MCX\x1A'

export enumdata [0:]ichar mcxdirnames =
	(pad_dir = 0,		$),		! nothing follows except next tag; for padding/alignment
	(version_dir,		$),		! STR string follows with version code (stringz)
	(code_dir,			$),		! N(u32) then N bytes of code data
	(idata_dir,			$),		! N(u32) then N bytes init data
	(zdata_dir,			$),		! N(u32) (no data follows)
	(reloc_dir,			$),		! N(u32) then N records follow
	(dlls_dir,			$),		! N(u32) then N STR items, the DLL base names
	(libs_dir,			$),		! N(u32) then N STR items, the MCX base names (ML libs)
	(importsymbols_dir,	$),		! N(u32) then N STR items, the imported names
	(exportsymbols_dir,	$),		! N(u32) then N STR items, the exported names
	(exportsegs_dir,	$),		! N(u32) then N u8 items, each is a segment code
	(exportoffsets_dir,	$),		! N(u32) then N u32 items, each an offset in the segment
	(entry_dir,			$),		! N(u32) N is a byte offset within code segment for entry point
	(end_dir,			$),		! nothing follows; end of file
end

!Reloc item record
! For Locabs-codes, the field contains the offset of the local symbol within target segment
! For Imp-codes, the field contains zero bytes

export record mcxreloc =
	u32		offset			! Offset with .segment of the reloc item
	union
		u16		stindex			! For Imp-codes, index into global import tables
		byte	targetsegment	! For Loc-codes, target segment refered to
	end
	byte	segment			! Segment containing the reloc item
	byte	reloctype		! Reloc code (see enums); also sets size of reloc item
end



!Relocation codes

export enumdata [0:]ichar mcxrelocnames =
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

!export enumdata []ichar segmentnames =
!	(code_seg,		"code"),
!	(idata_seg,		"idata"),
!	(zdata_seg,		"zdata"),
!	(rodata_seg,	"rodata"),
!	(impdata_seg,	$),
!end

!Describe an MCX program loaded into memory


export record librec=
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
	ref[]i16		importxreftable	! map symbol index to global one

	ichar			filespec		!full path
	ichar			libname			!base name of library
	ref byte		entryaddr		!start address (left at nil when entryoffset not set)
	int				libno			!index of this entry in progtable
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
global [maxsymbols]i16	symbollibindex	! Lib index where defined
global [maxsymbols]byte		symboldllindex	! DLL index of library where found
global int nsymbols

export int nsymimports=0, nsymexports=0
=== mx_run.m 0 0 20/40 ===
!Translate SS data directly into MCU block, then try and run that

global func writememlib(ichar filename)ref librec plib=
!write ss to mcu
	int n, k
	librec lib

	clear lib

	ss_zdatalen:=roundtoblock(ss_zdatalen, 8)

	roundsegment(ss_code,8,0x90)
	roundsegment(ss_idata,8,0)

	lib.version:="0.1234"

	lib.filespec:=filename
	lib.libname:=pcm_copyheapstring(extractbasefile(filename))
	lib.libno:=1

	countsymbols()
	writerelocs(&lib)

	lib.zdatasize:=ss_zdatalen
	lib.codesize:=bufferlength(ss_code)
	lib.idatasize:=bufferlength(ss_idata)

	lib.codeptr:=bufferelemptr(ss_code,0)
	lib.idataptr:=bufferelemptr(ss_idata,0)

	int ndlls:=0, nlibs:=0
	for i to nplibfiles when plibfiles[i]^<>'$' do
!		if libtypes[i]='D' then ++ndlls else ++nlibs fi
		++ndlls
	od

	lib.ndlllibs:=ndlls
	lib.nlibs:=nlibs

	lib.dllnames:=pcm_alloc(ichar.bytes*ndlls)
	lib.libnames:=pcm_alloc(ichar.bytes*nlibs)

	k:=0
!	for i to nlibfiles when libfiles[i]^<>'$' and libtypes[i]='D' do
	for i to nplibfiles when plibfiles[i]^<>'$' do
		lib.dllnames[++k]:=plibfiles[i]
	od

!	k:=0
!	for i to nlibfiles when libfiles[i]^<>'$' and libtypes[i]='L' do
!		lib.libnames[++k]:=libfiles[i]
!	od

	addsymbols(&lib)
	plib:=pcm_allocnfz(librec.bytes)
	memcpy(plib, &lib, librec.bytes)	

	return plib
end

proc roundsegment(ref dbuffer p, int align, value)=
	int length:=bufferlength(p)
	int newlength:=roundtoblock(length, align)

	buffercheck(p, align)

	to newlength-length do
		p.pcurr++^:=value
	od
end

proc writerelocs(ref librec lib)=
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
				if d.imported then
					newr.stindex:=d.impindex
					newr.reloctype:=imprel32_rel
				else
					axerror("rel32/rel not imported")
				fi
			when addr32_rel, addr64_rel then
				if d.imported then
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

proc addsymbols(ref librec lib)=
	psymbol d, stentry:=nil
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
		d:=ss_symboltable[i]
		lib.importnames[++k]:=d.name
	od

	k:=0
	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then
			if d.isentry then
				stentry:=d
			fi
			lib.exports[++k]:=d.name
			lib.exportsegs[k]:=d.segment
			lib.exportoffsets[k]:=d.offset
		fi
	od

	if stentry then
		lib.entryoffset:=stentry.offset
	else
CPL "No MAIN Found"
		lib.entryoffset:=-1
	fi
end

global proc countsymbols=
	psymbol d
	for i:=1 to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.exported then d.expindex:=++nsymexports fi
		if d.imported then d.impindex:=++nsymimports fi
	od
end

global proc runlibfile(ichar filename, int cmdskip, run=0)=
!LOADERROR("RUNLIBFILE")

	ref librec plib

	plib:=writememlib(filename)

	loadmemmcu(plib)
	fixuplib(plib)

!	if fshowmx then
!		LOADERROR("SHOWMX missing")
!		initlogfile()
!		showlibs()
!		closelogfile()
!	else

	if run then
		runprogram(plib, cmdskip)
	fi
!	fi
end

=== mx_lib.m 0 0 21/40 ===
global enumdata [0:]ichar rsegmentnames =
	(no_seg=0,		$),
	(code_rseg,		$),
	(idata_rseg,		$),
	(zdata_rseg,		$),
	(rodata_rseg,	$),
	(impdata_rseg,	$),
end

global func readlibfile(ichar filespec, ref byte p)ref librec plib=
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

	plib:=pcm_allocnfz(librec.bytes)
	memcpy(plib, &lib, librec.bytes)	

	return plib
end

func readbyte(ref byte &p)int=
	return p++^
end
!
func readu32(ref byte &p)u64 x=
	x:=ref u32(p)^
	p+:=4
	x
end

func readstring(ref byte &p)ichar s=
	s:=pcm_copyheapstring(p)

	while (++p)^ do od
	++p

	return s
end

global proc alloclibdata(ref librec lib)=
	int tablesize, n
	ref byte p

	lib.zdataptr:=pcm_allocz(lib.zdatasize)
!CPL "RUN/MX: NO ALLOCZ"
!	lib.zdataptr:=pcm_alloc(lib.zdatasize)

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

global proc error(ichar mess, param="")=
	if param^ then
		fprintln mess,param
	else
		println mess
	fi
	println "Aborting"
	stop 1
end

global proc loadmemmcu(ref librec lib)=
!load mcu into lib tables and load any dependencies

	int newlib
	ichar name:=lib.libname

	checknew(name,lib.filespec)

	newlib:=mxaddlib(name)
	libtable[newlib]:=lib

	loadimports(lib)
end

global proc checknew(ichar name, filename)=
	if findlib(name) then
		error("Lib already exists:",filename)
	fi
end

global func findlib(ichar name)int n=
!find an existing library existing

	for i to nlibs do
		if eqstring(name,libnametable[i]) then return i fi
	od
	return 0
end

global func mxaddlib(ichar name)int n=
!add a new lib slot with given name
	if nlibs>=maxlibs then 
		error("Too many libs")
	fi

	libnametable[++nlibs]:=name
	return nlibs
end

!export proc fixuplib(ref librec lib)=
global proc fixuplib(ref librec lib)=
!do second fixup pass, which is done across global symbols, but then 
!all relocs are done for all libs which are not yet relocated

	loaddlls()				!global
	checksymbols()			!global
	dorelocations()			!all libs
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

func finddllsymbol(ichar name, int &dllindex)ref void p=
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

proc checksymbols=
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
		qaddr++^:=cast(symboladdress[index])

	od

!Now do the actual relocations
	for i to lib.nrelocs do
		r:=lib.reloctable[i]
		case r.segment
		when code_rseg then p:=lib.codeptr+r.offset
		when idata_rseg then p:=lib.idataptr+r.offset
		when zdata_rseg then p:=lib.zdataptr+r.offset
		esac

		case r.reloctype
		when locabs32_rel then
			targetoffset:=ref u32(p)^
			case r.targetsegment
			when code_rseg then
				(ref u32(p)^ := cast(lib.codeptr+targetoffset))
			when idata_rseg then
				(ref u32(p)^ := cast(lib.idataptr+targetoffset))
			when zdata_rseg then
				(ref u32(p)^ := cast(lib.zdataptr+targetoffset))
			esac

		when locabs64_rel then
			targetoffset:=ref u32(p)^
			case r.targetsegment
			when code_rseg then
				(ref u64(p)^ := cast(lib.codeptr+targetoffset))
			when idata_rseg then
				(ref u64(p)^ := cast(lib.idataptr+targetoffset))
			when zdata_rseg then
				(ref u64(p)^ := cast(lib.zdataptr+targetoffset))
			esac

		when impabs64_rel then

			index:=lib.importxreftable[r.stindex]			!global index
			(ref u64(p)^+:=cast(symboladdress[index],u64))

		when impabs32_rel then
			index:=lib.importxreftable[r.stindex]			!global index
			(ref u32(p)^+:=cast(symboladdress[index],u64))

		when imprel32_rel then
			if r.segment<>code_rseg then error("imprel32?") fi
			index:=r.stindex								!local index
			q:=lib.codeptr+lib.codesize+(index-1)*8

			(ref u32(p)^ := q-(p+4))	!offset to thunk entry
		esac

	od

	librelocated[lib.libno]:=1

end

global proc loadimports(ref librec plib)=
! load imported libs
! do first fixup pass which sets up tables adds imports/exports to global table
! This is done per libs and can be called on imported sub-libs

	ref librec qlib
	ichar name

	for i to plib.nlibs do
		dosublib(plib.libnames[i])
	od

	alloclibdata(plib)
	dosymbols(plib)
end

proc dosublib(ichar name)=
	ref librec qlib
	int n:=findlib(name)

	if not n then									!not already loaded
		n:=mxaddlib(name)
		println "Loading sublib", name
		qlib:=loadlibfile(addext(name,"ml"),n)		!get mcu
		loadimports(qlib)						!recursive call
	fi
end

global func loadlibfile(ichar filename, int libno)ref librec plib=
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
			CPL "Dupl symbol:",lib.exports[i]
			NEXTLOOP
		fi
		symboldefined[ix]:=1

		case lib.exportsegs[i]
		when code_rseg then baseaddr:=lib.codeptr
		when idata_rseg then baseaddr:=lib.idataptr
		when zdata_rseg then baseaddr:=lib.zdataptr
		else baseaddr:=nil
		esac

		symboladdress[ix]:=cast(baseaddr+lib.exportoffsets[i])
		symbollibindex[ix]:=lib.libno

	od
end

func readmxfile(ichar filename)ref byte p=
!read in mx/ml file into an mcb block, add end_dir byte at the end just in case
!return pointer to mcb block

	p:=readfile(filename)
	return nil when p=nil
	(p+rfsize)^:=end_dir		!add eof-marker

	return p
end

proc adddll(ichar name)=
	for i to ndlllibs do
		if eqstring(name,dllnametable[i]) then return fi
	od

	if ndlllibs>=maxdlls then 
		error("Too many DLLs")
	fi

	dllnametable[++ndlllibs]:=name
end

func addsymbol(ichar name)int=
	for i to nsymbols do
		if eqstring(name,symbolnametable[i]) then return i fi
	od

	if nsymbols>=maxsymbols then 
		error("Too many Imports")
	fi

	symbolnametable[++nsymbols]:=name
	return nsymbols
end

proc setspecialglobals(int cmdskip)=
!adjust cmdparams visible to application by setting $cmdskip flag
!	for i to nsymbols when symbolnametable[i]^='$' do
	for i to nsymbols do
		if eqstring(symbolnametable[i],"msys.$cmdskip") or
			eqstring(symbolnametable[i],"$cmdskip") then

			(ref byte(symboladdress[i])^:=cmdskip)
!			(ref byte(symboladdress[i])^:=0)
		fi
	od
end

global proc runprogram(ref librec lib, int cmdskip=0)=
	ref proc fnptr
	int libno:=lib.libno

	for i to nlibs when i<>libno and not libinitdone[i] do
		calllibinit(libtable[i])
	od

	if lib.entryaddr=nil then
		error("No entry point found")
	fi

	setspecialglobals(cmdskip)

	fnptr:=cast(lib.entryaddr)

	fnptr()

	libinitdone[libno]:=1
end

global proc calllibinit(ref librec lib)=
	ref proc fnptr
	int libno:=lib.libno

	if lib.entryaddr then
		fnptr:=cast(lib.entryaddr)
		fnptr()
	fi
	libinitdone[lib.libno]:=1
end

export func findsymbol(ichar name)ref void=

	for i to nsymbols do
		if eqstring(symbolnametable[i], name) then
			return symboladdress[i]
		fi
	od
	return nil
end

global func loadmx(ichar filename)ref librec plib=
!load mx/ml into mcu then scan for other imported libraries
	int newlib
	ichar name

	name:=pcm_copyheapstring(convlcstring(extractbasefile(filename)))
	checknew(name,filename)

	newlib:=mxaddlib(name)

	plib:=loadlibfile(filename,newlib)

	loadimports(plib)
	return plib
end

global func loadmemmcb(ichar filename, ref byte p)ref librec plib=
!read from mcb block in memory
!'filename' is just an identifying string

	int newlib
	ichar name

	name:=pcm_copyheapstring(convlcstring(extractbasefile(filename)))
	checknew(name,filename)

	newlib:=mxaddlib(name)
	plib:=readlibfile(filename,p)
	plib.libno:=newlib
	libtable[newlib]:=plib	

	loadimports(plib)
	return plib
end

=== mx_write.m 0 0 22/40 ===
!Translate SS data directly into MCB block, then write as mx/ml file

ref dbuffer dest

psymbol entrypoint

global proc writemcx(ichar filename)=
	int n

	ss_zdatalen:=roundtoblock(ss_zdatalen, 8)

	roundsegment(ss_code,8,0x90)
	roundsegment(ss_idata,8,0)

	dest:=buffercreate()

	genu32(mcxsig)

	genbyte(version_dir)
	genstring("0.1234")

	countsymbols()
	writerelocs()

	genbyte(zdata_dir)
	genu32(ss_zdatalen)

	genbyte(code_dir)
	genu32(n:=bufferlength(ss_code))
	genblock(bufferelemptr(ss_code,0), n)

	genbyte(idata_dir)
	genu32(n:=bufferlength(ss_idata))

	genblock(bufferelemptr(ss_idata,0), n)

	int ndlls:=0, nlibs:=0
	for i to nplibfiles when plibfiles[i]^<>'$' do
		++ndlls
	od

	genbyte(dlls_dir)
	genu32(ndlls)
!	for i to nplibfiles when libfiles[i]^<>'$' and libtypes[i]='D' do
	for i to nplibfiles when plibfiles[i]^<>'$' do
		genstring(plibfiles[i])
	od

	writesymbols()

	genbyte(end_dir)

	writefile(filename, dest.pstart, dest.pcurr-dest.pstart)
end

global proc writerelocs=
	ref relocrec oldr
	mcxreloc newr
	int n,count
	psymbol d
	ref u64 baseptr64
	ref u32 baseptr32@baseptr64

	genbyte(reloc_dir)
	genu32(n:=ss_nidatarelocs+ss_ncoderelocs)

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
				if d.imported then
					newr.stindex:=d.impindex
					newr.reloctype:=imprel32_rel
				else
					axerror("rel32/rel not imported")
				fi
			when addr32_rel, addr64_rel then
				if d.imported then
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

proc writesymbols=
	psymbol d
	int n
	ichar name

	genbyte(importsymbols_dir)
	genu32(nsymimports)

	for i to ss_nsymbols when ss_symboltable[i].impindex do
		d:=ss_symboltable[i]
		genstring(d.name)
	od

	genbyte(exportsymbols_dir)
	genu32(nsymexports)

	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then
!			if eqstring(d.name, "main") then
			if d.isentry then
				entrypoint:=d
			fi
			genstring(d.name)
		fi
	od

	genbyte(exportsegs_dir)
	genu32(nsymexports)
	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then
			genbyte(d.segment)
		fi
	od

	genbyte(exportoffsets_dir)
	genu32(nsymexports)
	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then
			genu32(d.offset)
		fi
	od

	genbyte(entry_dir)		!must be present; writes 0xFFFFFFFF when no entry point
	if entrypoint then
		genu32(entrypoint.offset)
	else
		genu32(0xFFFF'FFFF)
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

proc genu32(int x)=
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
=== mx_show.m 0 0 23/40 ===
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

!		os_execwait(&.str,1,nil)
		os_execwait(&.str,0,nil)
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
			rsegmentnames[lib.exportsegs[i]]:"8jl",
			lib.exportoffsets[i]:"8zh"
		showstrln(str)
	od
	showstrln("")


	fprint @str,"Entry point offset:  #",lib.entryoffset
	showstrln(str)
	fprint @str,"Entry point address: #",lib.entryaddr
	showstrln(str)
	showstrln("")

FINISH:

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
			rsegmentnames[r.segment]:"8jl",
			r.offset:"8zh",,"  "

		m:=strlen(str)
		case r.reloctype
		when locabs32_rel, locabs64_rel then
			case r.segment
			when code_rseg then baseptr64:=cast(lib.codeptr+r.offset)
			when idata_rseg then baseptr64:=cast(lib.idataptr+r.offset)
			esac
			if r.reloctype=locabs32_rel then
				targetoffset:=baseptr32^
			else
				targetoffset:=baseptr64^
			fi

			print @&.str+m, rsegmentnames[r.targetsegment]:"6jlt:",,targetoffset:"8zh"
		else
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
S:=NIL
!		s:=decodeinstr(codeptr,baseaddr+offset)
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
=== mm_cli.m 0 0 24/40 ===

global ichar syslibname=""

!macro SHOW(m) = println m
macro SHOW(m) = eval 0

!main production options; passnames are also file extensions for outputs

global enumdata []ichar passnames =
!								Output (when this is the final step)
	(load_pass,		$),
	(parse_pass,	$),
	(fixup_pass,	$),
	(name_pass,		$),
	(type_pass,		$),

	(ma_pass,		"ma"),			! .ma     These are are special
	(getst_pass,	"list"),		! .list
	(getproj_pass,	"proj"),		! .prog

	(tcl_pass,		"tcl"),			! .tcl
	(runtcl_pass,	"(int)"),		! interpret
	(mcl_pass,		"asm"),			! .asm
	(obj_pass,		"obj"),			! .obj (via .asm and aa)
	(dll_pass,		"dll"),			! .dll
	(exe_pass,		"exe"),			! .exe
	(mx_pass,		"mx"),			! .mx
	(run_pass,		"(run)"),		! run in-memory
end

enumdata []ichar optionnames, []byte optionvalues =

!special outputs
	(ma_sw,			"ma",			ma_pass),
	(getst_sw,		"getst",		getst_pass),
	(getproj_sw,	"getproj",		getproj_pass),

!normal production outputs
	(load_sw,		"load",			load_pass),
	(parse_sw,		"parse",		parse_pass),
	(fixup_sw,		"fixup",		fixup_pass),
	(name_sw,		"name",			name_pass),
	(type_sw,		"type",			type_pass),
	(tcl_sw,		"p",			tcl_pass),
	(tcl2_sw,		"tcl",			tcl_pass),
	(runtcl_sw,		"i",			runtcl_pass),
	(asm_sw,		"a",			mcl_pass),
	(asm2_sw,		"mcl",			mcl_pass),
	(obj_sw,		"obj",			obj_pass),
	(dll_sw,		"dll",			dll_pass),
	(dll2_sw,		"d",			dll_pass),
	(exe_sw,		"exe",			exe_pass),		!default
	(mx_sw,			"mx",			mx_pass),
	(run_sw,		"r",			run_pass),		!default with ms.exe

	(sys_sw,		"sys",			2),
	(minsys_sw,		"min",			1),
	(nosys_sw,		"nosys",		0),
	(clinux_sw,		"linux",		0),

	(noopt_sw,		"no",			0),
	(nopeephole_sw,	"nopeep",		0),
	(noregoptim_sw,	"noregs",		0),

!diagnostic outputs
	(ast1_sw,		"ast1",			0),
	(ast2_sw,		"ast2",			0),
	(ast3_sw,		"ast3",			0),
	(showc_sw,		"showc",		0),
	(showtcl_sw,	"showtcl",		0),
	(showasm_sw,	"showasm",		0),
	(st_sw,			"st",			0),
	(stflat_sw,		"stflat",		0),
	(types_sw,		"types",		0),
	(showss_sw,		"showss",		0),
	(showmodules_sw,"modules",		0),
	(pst_sw,		"pst",			0),

	(shortnames_sw,	"shortnames",	0),


	(time_sw,		"time",			0),
	(v_sw,			"v",			2),
	(vv_sw,			"vv",			3),
	(quiet_sw,		"q",			0),
	(csize_sw,		"cs",			1),
	(size_sw,		"ss",			2),
	(help_sw,		"h",			0),
	(help2_sw,		"help",			0),
	(ext_sw,		"ext",			0),
	(out_sw,		"o",			0),
	(outpath_sw,	"outpath",		0),
	(unused_sw,		"unused",		0),

	(norip_sw,		"norip",		0),
	(himem_sw,		"himem",		2),
end


byte msfile

global const logfile="mx.log"

ichar outext=""				!for reporting of primary o/p file

global int startclock, endclock
global int cmdskip

global ichar inputfile

global int loadtime
global int parsetime
global int resolvetime
global int typetime
global int ctime
global int tcltime
global int compiletime

global proc main2=
!STOP
!proc main=
	unit p,q,r
	int m,fileno,ntokens,t,tt

!cpl "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

!CPL =TCLNAMES.LEN
!CPL =Pstrec.bytes
!CPL =TCLREC.bytes
!CPL =MCLREC.bytes
CPL =Pstrec.bytes

!FOR S IN TCLNAMES DO
!	fprintln "proc tx_#*(tcl p) =", s
!	println "\tunimpl(p)"
!	println "end"
!	println
!OD
!

	startclock:=os_clock()
PSTARTCLOCK:=STARTCLOCK
	initdata()

	getinputoptions()

	showcompilemess()

! Do early passes common to all options

	loadproject(inputfile)

	do_parse()
!CPL $LINENO
	do_name()
!CPL $LINENO
	do_type()
!CPL $LINENO

! Special outputs can be done at this point
	do_writema(inputfile)	when passlevel=ma_pass			! terminates
!CPL $LINENO
	do_getinfo(inputfile)	when passlevel in [getst_pass, getproj_pass]		! terminates
	do_writeexports()		when passlevel=dll_pass
!CPL $LINENO

	do_gentcl()
!CPL $LINENO

	tcl_runtcl() when passlevel=runtcl_pass				!terminates
!CPL $LINENO

! Deal with chosen output kind

!CPL PASSNAMES[PASSLEVEL]

	if passlevel>=mcl_pass then
		do_genmcl(passlevel=mcl_pass)

		case passlevel
		when obj_pass then
!CPL $LINENO
			tcl_writeobj(changeext(outfile, "obj"))	

		when exe_pass then
!CPL $LINENO
			tcl_writeexe(changeext(outfile, "exe"))	

IF FSHOWSS THEN
	tcl_writess("SS")
FI

		when dll_pass then
!CPL $LINENO
			tcl_writedll(changeext(outfile, "dll"))	

		when mx_pass then
!CPL $LINENO
			tcl_writemx(changeext(outfile, "mx"))	

		when run_pass then
!CPL $LINENO
			tcl_exec()

		esac
	fi
!CPL $LINENO

	showsurveys()

	showlogfile()
	showtimings() when fshowtiming

	if fverbose=3 then println "Finished." fi

end

proc showcompilemess=
	if fverbose>=1 and not msfile then
		fprintln "Compiling # to #",inputfile,changeext(outfile,(ctarget|"c"|passnames[passlevel]))
	fi
end

proc do_parse=
!	if fverbose=3 then println "PARSE" fi

	return unless passlevel>=parse_pass

	int tt:=clock()

	for i to nmodules do
		parsemodule(modules[i])
	od
	parsetime:=clock()-tt

	if passlevel>=fixup_pass then
!		if fverbose=3 then println "FIXUP" fi
		fixusertypes()
	fi

	fixstartprocs()
!
	if fshowast1 then showast("AST1") fi
end

proc do_name=
!	if fverbose=3 then println "NAME" fi
	return unless passlevel>=name_pass

	int tt:=clock()
	rx_typetable()

	for i:=2 to nmodules do
		rx_module(i)
	od
	rx_module(1)
	resolvetime:=clock()-tt

	if fshowast2 then showast("AST2") fi
end

proc do_type=
!	if fverbose=3 then println "TYPE" fi

	return unless passlevel>=type_pass
	int tt:=clock()
	tx_typetable()

	for i:=1 to nmodules do
		tx_module(i)
	od
	tx_allprocs()
	typetime:=clock()-tt

	if fshowast3 then showast("AST3") fi
end

proc do_gentcl=
!	if fverbose=3 then println "GENTCL" fi
	return unless passlevel>=tcl_pass

	int tt:=clock()

!CPL "GENTCL-------------"

	codegen_tcl()

	tcltime:=clock()-tt

!CPL =FREGOPTIM, =FPEEPHOLE

!	tcl_reducetest() when fregoptim or fpeephole
!	tcltime:=clock()-tt

	if fshowtcl or passlevel=tcl_pass then
!CPL "NOT WRITING TCL"
		tcl_writetcl(changeext(outfile, "tcl"))
	fi

	if fshowpst and passlevel=tcl_pass then		!for mcl+ it is o/p later
		tcl_writepst("PSYMTAB")
	fi

end

proc do_genmcl(int flog=0)=
!	if fverbose=3 then println "GENMCL" fi

	return unless passlevel>=mcl_pass
	int tt:=clock()

	tcl_genmcl()
	mcltime:=clock()-tt
!CPL =MCLTIME

	if flog then
		tcl_writeasm(changeext(outfile, (ctarget|"c"|"asm")))
	fi

	if fshowpst and passlevel>tcl_pass then
		tcl_writepst("PSYMTAB")
	fi

end

proc initdata=
	imodule pm
	ifile pf

	pcm_init()
	lexsetup()
	init_tt_tables()
	initbblib()

	pm:=pcm_allocz(modulerec.bytes)

	pm.name:="PROGRAM"

	stprogram:=createdupldef(nil,addnamestr("$prog"),programid)
	pm.stmodule:=stprogram
	modules[0]:=pm

!*!	igetmsourceinfo:=cast(mgetsourceinfo)

!	idomcl_assem:=cast(domcl_assem)
!	igethostfn:=cast(findhostfn)

	REMOVE("PSYMTAB")

end

proc getinputoptions=
	int paramno,pmtype,sw,extlen
	ichar name,value,ext
	[300]char filespec

	if tc_useruntcl then
		passlevel:=runtcl_pass
		fverbose:=0
	fi
	paramno:=1

	if eqstring(extractfile(os_gethostname()),"ms.exe") then
		msfile:=1
		fverbose:=0
		do_option(run_sw, "")
	fi

	while pmtype:=nextcmdparamnew(paramno,name,value,"m") do
		case pmtype
		when pm_option then

			convlcstring(name)
			for sw to optionnames.len do
				if eqstring(name,optionnames[sw]) then
					do_option(sw,value,paramno)
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

!CPL =PASSNAMES[PASSLEVEL]

			if passlevel in [run_pass, runtcl_pass] then
				cmdskip:=paramno-1+$CMDSKIP
!CPL "EXITG1"
				exit
			fi

		when pm_libfile then
			loaderror("Lib files go in module headers")
		else
			loaderror("Invalid params")
		esac

	od

	if passlevel=0 then
		if not ctarget then
			passlevel:=exe_pass
			outext:="exe"
		else
			passlevel:=mcl_pass
			outext:="c"
		fi
	fi

	case passlevel
	when obj_pass, dll_pass then
		highmem:=2
	when mcl_pass then
!*!		if assemtype='NASM' then highmem:=2 fi
	when mx_pass, run_pass then
		highmem:=0
	esac

	if inputfile=nil then
		showcaption()
		println "Usage:"
		println "   ",cmdparams[0]," prog[.m]     Compile prog.m to prog.exe"
		println "   ",cmdparams[0]," -h           Show all options"
		stop

	else
!default output
		outfile:=pcm_copyheapstring(inputfile)

		if destfilename then
			outfile:=destfilename
		fi

		if destfilepath then
			strcpy(&.filespec,destfilepath)
			strcat(extractfile(&.filespec), outfile)
			outfile:=pcm_copyheapstring(&.filespec)	
		fi
	fi

	ext:=extractext(inputfile)
	extlen:=strlen(ext)
	strcpy(filespec, changeext(cmdparams[0],ext))
	convlcstring(filespec)
	if eqstring(filespec, inputfile) and passlevel=exe_pass then
		strcpy(&.filespec+strlen(filespec)-extlen-1, "2.m")
		outfile:=pcm_copyheapstring(filespec)
		println "New dest=",outfile
	fi

	tcl_setflags(highmem:highmem, shortnames:fshortnames)
	tcl_cmdskip(cmdskip)
	if msyslevel=2 then pfullsys:=1 fi

end

proc do_option(int sw, ichar value, int paramno=0)=
	static byte outused, outpathused
	byte newpass

!CPL "DOOPTION", OPTIONNAMES[SW], PASSLEVEL

	if sw in ma_sw..run_sw then
		newpass:=optionvalues[sw]
		if passlevel and newpass<>passlevel then
			loaderror("Conflicting pass:", optionnames[sw])
		fi
		passlevel:=newpass
		outext:=passnames[sw]

		case sw
		when runtcl_sw then			!in case occurs at end
			cmdskip:=paramno-1+$CMDSKIP
		esac

		return
	fi

	if sw in ast1_sw..pst_sw then
		fshowdiags:=1
	fi

	case sw
	when ast1_sw then fshowast1:=1
	when ast2_sw then fshowast2:=1
	when ast3_sw then fshowast3:=1
	when showtcl_sw then fshowtcl:=1
	when showc_sw then fshowc:=1
	when showasm_sw then fshowasm:=1
	when st_sw then fshowst:=1
	when stflat_sw then fshowstflat:=1
	when pst_sw then fshowpst:=1
	when types_sw then fshowtypes:=1
	when showss_sw then fshowss:=1
	when showmodules_sw then fshowmodules:=1

	when clinux_sw then clinux:=1

	when sys_sw, minsys_sw, nosys_sw then msyslevel:=optionvalues[sw]

	when noopt_sw then fpeephole:=fregoptim:=0
	when nopeephole_sw then fpeephole:=0
	when noregoptim_sw then fregoptim:=0

	when time_sw then fshowtiming:=1
	when v_sw, vv_sw, quiet_sw then fverbose:=optionvalues[sw]
	when csize_sw, size_sw then pverbose:=optionvalues[sw]

	when help_sw, help2_sw then showhelp(); stop
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

	when shortnames_sw then fshortnames:=1

	when norip_sw, himem_sw then highmem:=optionvalues[sw]

	end case

end

proc showcaption=
	println "M Compiler [M8.0]", $date, $time
end

global proc showhelp=
	println strinclude(langhelpfile)
end

proc do_writeexports=
	[300]char str

	strcpy(str, extractbasefile(outfile))
	writeexports(outfile, changeext(str, "dll"))
!	stop
end

func getoutfilename(ichar file,ext)ichar=
	return pcm_copyheapstring(changeext(file,ext))
end

proc fixstartprocs=
!make sure each module has a start proc
!make sure the lead module has a main proc
	imodule ms
	isubprog ps
	symbol d
	unit p, q
	int s

	for i to nsubprogs do
		ps:=subprogs[i]
		if ps.mainmodule=0 then
			ps.mainmodule:=ps.firstmodule
		fi
	od


	for i to nmodules do
		ms:=modules[i]
		if ms.ststart then
			subproghasstart[ms.subprogno]:=1
		fi
	od

	for i to nmodules do
		ms:=modules[i]
		if ms.ststart=nil then
			s:=ms.subprogno
			if subproghasstart[s] and subprogs[s].mainmodule=i then
				ms.ststart:=addstartproc(ms.stmodule,"start", program_scope,i)
			fi
		fi

	od
end

func addstartproc(symbol owner, ichar name, int scope,moduleno)symbol stproc=
	stproc:=getduplnameptr(owner,addnamestr(name),procid)
	stproc.scope:=scope
	stproc.moduleno:=moduleno
	stproc.subprogno:=moduletosub[moduleno]
	stproc.code:=makeblock(nil)
	adddef(owner,stproc)
	addtoproclist(stproc)

	return stproc
end

PROC SHOWSURVEYS=

!CPL =MLABELNO

!CPL =NALLEXPR
!CPL =NFASTEXPR

!CPL =NALLGENPCHIST[0]
!CPL =NALLGENPCHIST[1]
!CPL =NALLGENPCHIST[2]
!CPL =NALLGENPCHIST[3]
!CPL =NALLGENPC1
END
=== mm_gentcl.m 0 0 25/40 ===
const fscantemps=1
!const fscantemps=0

const freducetemps=1
!const freducetemps=0

global int initstaticsindex
export tcl tcldoswx

const maxnestedloops	= 50

global [maxnestedloops, 4]int loopstack
global int loopindex							!current level of nested loop/switch blocks

unitrec zero_unit
global unit pzero=&zero_unit

global psymbol pnprocs, pprocname, pprocaddr

int nvarlocals, nvarparams

macro divider = tc_comment("="*40)

global proc codegen_tcl=
	symbol d
	ref procrec pp

	return when tcldone

	dolibs()

	pp:=staticlist
	while pp do
		d:=pp.def
		dostaticvar(d)
		pp:=pp.nextproc
	od

	for i to ndllproctable do
		gendllproc(dllproctable[i])
	od

	pp:=proclist
	while pp do
		d:=pp.def
		genprocdef(currproc:=d)
		pp:=pp.nextproc
	od

	scanprocs()

	tcldone:=1
end

proc genprocdef (symbol d) =
	[256]char str
	imodule ms
	psymbol p, q
	symbol e
	byte ismain:=0

	ms:=modules[d.moduleno]
	tcldoswx:=nil

	p:=getpsymbol(d)	

	tc_addproc(p)
	tc_currfunc(p)

	e:=d.deflist

	while e, e:=e.nextdef do
		nextloop when e.atvar
		q:=getpsymbol(e)

		case e.nameid
		when paramid then
			tc_addparam(q)
		when frameid then
			tc_addlocal(q) unless e.atvar
		when staticid then
			tc_addstatic(q)
			if not d.atvar then
				do_idata(e)
			fi
		esac
	od

	tcl_start()

	mmpos:=d.pos

	if d=ms.stmain and moduletosub[d.moduleno]=mainsubprogno then
		ismain:=1
		entryproc:=p
		p.isentry:=1
		genmain(d)

	elsif d=ms.ststart then
		genstart(d)
	fi

	p.retindex:=++mlabelno
	divider()

	if d.hasdoswx then
		tcldoswx:=tccurr
	fi

	evalu(d.code)

	divider()

	if ismain then
		tc_gen(kstop, tc_genint(0))
		setmode(ti64)
	fi

	p.code:=tcl_end()

	tc_currfunc(nil)
	p.maxtemp:=ntemps

	scanproctemps(p) when fscantemps
!	reducetemps(p) when freducetemps and ctarget
	reducetemps(p) when freducetemps

end

proc genmain(symbol p)=
	symbol d
	for i to nsubprogs when i<>mainsubprogno do
		d:=modules[subprogs[i].mainmodule].ststart
		docallproc(d)
	od
	d:=modules[subprogs[mainsubprogno].mainmodule].ststart
	docallproc(d)

	entryproc:=getpsymbol(p)
end

proc genstart(symbol p)=
	symbol d
	int lead:=0, m,s

	m:=p.moduleno
	s:=p.subprogno

	if s=mainsubprogno and p.moduleno=subprogs[s].mainmodule then
		LEAD:=1
	elsif p.moduleno=subprogs[s].firstmodule then
		LEAD:=2
	fi

	if lead then
		for i to nmodules when moduletosub[i]=s and i<>m do
			d:=modules[i].ststart
			docallproc(d)
		od
	fi
end

proc gendllproc(symbol p)=
	symbol e

	tc_setimport(getpsymbol(p))

	e:=p.deflist
	while e, e:=e.nextdef do
		tc_addparam(getpsymbol(e))
	od
	tc_setimport(nil)

end

proc dolibs=
	for i to nlibfiles when libfiles[i]^<>'$' do
		tc_addplib(libfiles[i])
	od
end

proc dostaticvar(symbol d)=
	psymbol p

	if d.isimport then return fi
!
	if d.scope = program_scope and d.name^='$' then
		if eqstring(d.name, "$cmdskip") then
			d.scope:=export_scope				!export from mlib subprog
		fi
	fi

	if d.atvar=1 then
		return
	fi

	p:=getpsymbol(d)
	tc_addstatic(p)

	do_idata(d)
end

proc do_idata(symbol d)=
	tcl pc

	return unless d.code

	tcl_start()
	mmpos:=d.pos

	genidata(d.code)

	pc:=tcl_end()

	d.pdef.code:=pc
end

proc genidata(unit p, int doterm=1, am='A', offset=0)=
	[2000]byte data
	int t, tbase
	byte allbytes, nbytes
	unit q, a
	symbol d
	ref char s

	t:=p.mode
	mmpos:=p.pos
	tbase:=ttbasetype[t]

	case p.tag
	when jconst then
		if ttisref[t] then
			if t=trefchar then
				if p.svalue then
					if p.strtype='B' then gerror("1:B-str?") fi
					tc_gen(kdata, tc_genstring(p.svalue))
				else
					tc_gen(kdata, tc_genint(0))
				fi
			else
				tc_gen(kdata, tc_genint(p.value))
			fi
			setmode(ti64)
		elsif ttisreal[t] then
			if tbase=tr64 then
				tc_gen(kdata, tc_genreal(p.xvalue))
			else
				tc_gen(kdata, tc_genr32(p.xvalue))
			fi
			setmode(t)

		elsif ttbasetype[t]=tarray then
			IF P.STRTYPE=0 THEN GERROR("IDATA/ARRAY/NOT BLOCKDATA") FI
			tc_gen(kdata, tc_gendata(p.svalue, p.slength))
			setmode_u(p)

		else						!assume int/word
			tc_gen(kdata, tc_genint(p.value))
			setmode_u(p)
		fi

	when jmakelist then
		q:=p.a

		allbytes:=1
		nbytes:=0
		while q, q:=q.nextunit do
			if q.tag=jconst and q.mode=tu8 and nbytes<data.len then
				data[++nbytes]:=q.value
			else
				allbytes:=0
				exit
			end
		end

		if allbytes and nbytes then		!was all byte constants, not in data[1..nbytes]
			tc_gen(kdata, tc_gendata(pcm_copyheapstringn(cast(&data), nbytes), nbytes))
			setmode_u(p)

		else
			q:=p.a
			while q, q:=q.nextunit do
				genidata(q)
			od
		fi

	when jname then
		d:=p.def
		case d.nameid
		when staticid, procid, dllprocid then
			tc_gen(kdata, genmemaddr_d(d))
			if offset then
				tccurr.extra:=offset
			fi
			if am='P' then
				setmode(tu64)
			else
				setmode(t)
			fi
		when labelid then
			if d.index=0 then d.index:=++mlabelno fi
			tc_gen(kdata, tc_genlabel(d.index))
			setmode(ti64)
		else
			gerror("Idata &frameXXX")
		esac
		return
	when jconvert then
		genidata(p.a)
	when jshorten then
		tc_gen(kdata, tc_genint(p.a.value))
!		tccurr.a.opmode:=tpi64
!		tccurr.a.opsize:=8
		setmode(t)

	when jaddrof, jaddroffirst then
		genidata(p.a, am:'P', offset:(p.b|p.b.value|0))
	else
		gerror_s("IDATA: ", jtagnames[p.tag], p)

	esac
end

global func genmem_u(unit p)tclopnd=
	tc_genmem(getpsymbol(p.def))
end

global func genmem_d(symbol d)tclopnd=
	tc_genmem(getpsymbol(d))
end

global func genmemaddr_d(symbol d)tclopnd=
	tc_genmemaddr(getpsymbol(d))
end

global func genmemaddr_u(unit p)tclopnd=
	return tc_genmemaddr(getpsymbol(p.def))
end

global func definelabel:int =
	tc_gen(klabel, tc_genlabel(++mlabelno))
	return mlabelno
end

global func createfwdlabel:int =
	return ++mlabelno
end

global proc definefwdlabel(int lab) =
	tc_gen(klabel, tc_genlabel(lab))
end

global func reversecond(int cc)int=
!reverse conditional operator
	case cc
	when eq_cc then cc:=ne_cc
	when ne_cc then cc:=eq_cc
	when lt_cc then cc:=ge_cc
	when le_cc then cc:=gt_cc
	when ge_cc then cc:=lt_cc
	when gt_cc then cc:=le_cc
	esac

	return cc
end

global func reversecond_order(int cc)int=
	case cc
	when eq_cc then cc:=eq_cc
	when ne_cc then cc:=ne_cc
	when lt_cc then cc:=gt_cc
	when le_cc then cc:=ge_cc
	when ge_cc then cc:=le_cc
	when gt_cc then cc:=lt_cc
	esac

	return cc
end

global proc stacklooplabels(int a, b, c)=
!don't check for loop depth as that has been done during parsing
	++loopindex
	if loopindex>maxnestedloops then
		gerror("Too many nested loops")
	fi

	loopstack[loopindex, 1]:=a
	loopstack[loopindex, 2]:=b
	loopstack[loopindex, 3]:=c

end

global func findlooplabel(int k, n)int=
!k is 1, 2, 3 for label A, B, C
!n is a 1, 2, 3, according to loop nesting index
	int i

	i:=loopindex-(n-1)		!point to entry
	if i<1 or i>loopindex then gerror("Bad loop index") fi
	return loopstack[i, k]
end

global func gensysfn(int fnindex, int mode, unit a=nil, b=nil)tclopnd tx=
	tx:=tc_gentemp()
	setopndmode(tx, mode)
	gensysproc(fnindex, a, b, tx)
	tx
end

global proc gensysproc(int fnindex, unit a=nil, b=nil, tclopnd dx=nil)=
	tclopnd fx, ax, bx
	int nargs:=0, nret:=0
	symbol d

	if a then
		++nargs
		ax:=extparamopnds[1]:=evalu(a)
		setopndmode(ax, a.mode)
		if b then
			 ++nargs
			ax:=extparamopnds[2]:=evalu(b)
			if b=nullunit then				!usually optional fmt arg
				setopndmode(ax, tu64)
			else
				setopndmode(ax, b.mode)
			fi
		fi
	fi

	if dx then
		nret:=1
		extretopnds[1]:=dx
	fi

	d:=getsysfnhandler(fnindex)

	if d then
		fx:=tc_genmemaddr(getpsymbol(d))
	else
		fx:=tc_gennameaddr(sysfnnames[fnindex]+3)
	fi

	tc_gen_call(fx, nret, nargs)
end

proc start=
	zero_unit.tag:=jconst
	zero_unit.mode:=ti64
	zero_unit.value:=0
	zero_unit.resultflag:=1
end

global func getsysfnhandler(int fn)symbol p=
	[300]char str
	int report

	if sysfnhandlers[fn] then
		return sysfnhandlers[fn]
	fi

	strcpy(str, "m$")
	strcat(str, sysfnnames[fn]+3)	!"sf_stop" => "m$stop"

	ref procrec pp:=proclist
	while pp, pp:=pp.nextproc do
!CPL "  CMP", PP.DEF.NAME:"16JL", STR
		if eqstring(pp.def.name, str) then
			sysfnhandlers[fn]:=pp.def
!CPL "	FOUND", SYSFNNAMES[FN]
			return pp.def
		fi
	od

!	report:=passlevel>asm_pass
	report:=1
!	report:=0

	if report then
!		println "Sysfn not found:", &.str
	fi
	if fn<>sf_unimpl then
		p:=getsysfnhandler(sf_unimpl)
RETURN NIL
		if p=nil and report then
			gerror("No m$unimpl")
		fi
		return p
	fi

	return nil
end

proc docallproc(symbol d)=
!call a simple proc, eg. start(), with no args
	if d then
		tc_gen_call(tc_genmemaddr(getpsymbol(d)), 0, 0)
	fi
end

proc scanprocs=
	const maxprocs=1000
	[maxprocs]psymbol proctable
	int nprocs:=0
	ref procrec pp
	symbol d

	pp:=proclist
	while pp, pp:=pp.nextproc do
		d:=pp.def
		if d.ishandler then
			if nprocs>=maxprocs then gerror("TCL proctab overflow") fi
			proctable[++nprocs]:=d.pdef
		fi
	od

	if nprocs=0 and pnprocs=nil then
		pnprocs:=tc_makesymbol("$nprocs", static_id)
		pnprocs.mode:=tpi64
		goto finish
	fi

	setfunctab()

	tc_addstatic(pprocaddr)
	pprocaddr.mode:=tpblock
	pprocaddr.size:=nprocs*8

	tcl_start()

	for i to nprocs do
		tc_gen(kdata, tc_genmemaddr(proctable[i]))
		setmode(tu64)
	od
	pprocaddr.code:=tcl_end()

	tc_addstatic(pprocname)
	pprocname.mode:=tpblock
	pprocname.size:=nprocs*8
!
	tcl_start()
	for i to nprocs do
		tc_gen(kdata, tc_genstring(getbasename(proctable[i].name)))
		setmode(tu64)
	od
	pprocname.code:=tcl_end()

finish:
	tc_addstatic(pnprocs)
	pnprocs.mode:=tpi64
	pnprocs.size:=8

	tcl_start()
	tc_gen(kdata, tc_genint(nprocs))
	setmode(ti64)
	pnprocs.code:=tcl_end()
end

global proc setfunctab=
	if pnprocs=nil then
		pnprocs:=tc_makesymbol("$nprocs", static_id)
		pnprocs.mode:=tpi64
		pprocname:=tc_makesymbol("$procname", static_id)
		pprocaddr:=tc_makesymbol("$procaddr", static_id)
	fi
end
=== mm_libtcl.m 0 0 26/40 ===
!global [maxtuplesize]tclopnd extretopnds		!temps to hold func results
!global [maxparam]tclopnd extparamopnds

global func getpsymbol(symbol d)psymbol p=
	symbol e
	[256]char str
	[16]symbol chain
	int n

	return nil when d=nil

	if d.pdef then return d.pdef fi

	if d.atvar and d.equivvar then
		getpsymbol(e:=getequivdef(d))
!		e.pdef.atvar:=1
		d.pdef:=e.pdef
		return e.pdef
	fi

	if d.nameid in [frameid, paramid] or d.isimport then
		strcpy(str, (d.truename|d.truename|d.name))
	else
		e:=d
		n:=0
		repeat
			chain[++n]:=e
			e:=e.owner
		until e=nil or e.nameid=programid

		strcpy(str,chain[n].name)
		for i:=n-1 downto 1 do
			strcat(str,".")
			if chain[i].truename then
				strcat(str,chain[i].truename)
			else
				strcat(str,chain[i].name)
			fi
		od
	fi

	d.pdef:=p:=tc_makesymbol(str, name2pid[d.nameid])

	p.mode:=gettclmode(d.mode)

	p.size:=ttsize[d.mode]
	p.pos:=d.pos

	if d.scope=export_scope then p.exported:=1 fi
	if d.nameid in [dllprocid, dllvarid] then p.imported:=1 fi
	p.used:=d.used

	p.labelno:=d.index
	p.ishandler:=d.ishandler
	p.isthreaded:=d.isthreaded

	p.variadic:=d.varparams
	p.align:=getalignment(d.mode)		!mainly for vars, but no harm for procs etc

	e:=d.owner

	return p
end

global proc setmode(int mode)=
	tc_setmode(gettclmode(mode), ttsize[mode])
end

global proc setmode2(int mode)=
	tc_setmode2(gettclmode(mode))
end

global proc setmode_u(unit p)=
	int mode:=p.mode
	tc_setmode(gettclmode(mode), ttsize[mode])
end

func getequivdef(symbol d)symbol=
!assume that d.atvar/d.equivvar are set
	unit p

	p:=d.equivvar
	case p.tag
	when jname then
		p.def
	when jconvert then
		p.a.def			!assume points to name
	else
		gerror("geteqv")
		nil
	esac
end

global func gendest(tclopnd dx)tclopnd=
	if dx then return dx fi
	return tc_gentemp()
end

global func makeind(tclopnd a, int m)tclopnd p=
	tc_makeind(a, gettclmode(m), ttsize[m])
end

global func makeindlv(tclopnd a, int m, size=0)tclopnd p=
	tc_makeindlv(a, gettclmode(m), ttsize[m])
end

global proc setopndmode(tclopnd p, int m)=
	p.opmode:=gettclmode(m)
	p.opsize:=ttsize[m]
end

=== mm_blocktcl.m 0 0 27/40 ===
!dummy

!const freduce=0
!!const freduce=1

const maxnestedloops	= 50

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

global func evalu(unit p)tclopnd tx=
!p is a single executable unitrec; not a list or const
!should be called via execblock() to executate a code item in a unitrec
	unit a, b, c
	symbol d
	[128]char str

	if p=nil then return nil fi
	mmpos:=p.pos

	a:=p.a
	b:=p.b
	c:=p.c

	tx:=nil

!CPL "EVALUNIT", JTAGNAMES[P.TAG],CURRFUNC.NAME

	switch p.tag
	when jconst         then tx:=do_const(p)
	when jname          then tx:=do_name(p)
	when jblock then
		tx:=do_block(p)

	when jcall          then tx:=do_callproc(p, a, b)
	when jreturn        then
		tx:=do_return(p, a)

	when jreturnmult    then
!		tx:=do_returnmult(p, a)
		tx:=do_returnmult(p, a)

	when jassign        then tx:=do_assign(p,a,b)
	when jassignmm      then do_assignmm(p,a,b)
	when jassignms      then do_assignms(p,a,b)
	when jassignmdrem   then do_assignmdrem(p,a,b)
	when jto            then do_to(p, a, b)
	when jif            then tx:=do_if(p, a, b, c, 0)
	when jforup         then do_for(p, a, b, c, 0)
	when jfordown       then do_for(p, a, b, c, 1)
	when jforall        then do_forall(p, a, b, c, 0)
	when jforallrev     then do_forall(p, a, b, c, 1)
	when jwhile         then do_while(p, a, b, c)
	when jrepeat        then do_repeat(p, a, b)
	when jgoto          then do_goto(a)
	when jlabeldef      then do_labeldef(p)
	when jredo          then do_exit(p, 1)
	when jnext          then do_exit(p, 2)
	when jexit          then do_exit(p, 3)
	when jdo            then do_do(p, a, b)
	when jcase          then tx:=do_case(p, a, b, c, 0, 0)
	when jdocase        then do_case(p, a, b, c, 1, 0)
	when jswitch, jdoswitch, jdoswitchu, jdoswitchx then
		tx:=do_switch(p, a, b, c, 0)
	when jrecase        then do_recase(p, a)
	when jswap          then do_swap(p, a, b)
	when jselect        then tx:=do_select(p, a, b, c, 0)
	when jprint, jprintln then
		do_print(p, a, b)
	when jfprint, jfprintln then
		do_print(p, a, b)
!	when jread	         then tx:=do_read(p, a)
	when jread	         then tx:=do_read(p, a)
	when jreadln        then do_readln(a)
	when jstop          then do_stop(p, a)
	when jeval          then tx:=do_evalu(a)
!
	when jandl, jorl   then tx:=do_andorl(p)

	when jcmp then
		tx:=do_setcc(p, a, b)
	when jcmpchain then
		tx:=do_setccchain(p, a)

	when jbin then
           tx:=do_bin(p, a, b)

	when jindex         then tx:=do_index(p, a, b)
	when jslice         then tx:=do_slice(p, a, b)
	when jdotindex      then tx:=do_dotindex(p, a, b)
	when jdotslice      then tx:=do_dotslice(p, a, b)
	when jdot           then tx:=do_dot(p)
	when jptr           then tx:=do_ptr(p, a, b)
	when jaddrof        then tx:=evalref(a, b)
	when jaddroffirst   then tx:=evalref(a, b)
	when jconvert       then tx:=do_convert(p, a)
	when jtypepun       then
		tx:=do_typepun(p, a)

	when junary then
		tx:=do_unary(p, a)

	when jnotl          then tx:=do_notl(p, a)
	when jistruel       then tx:=do_istruel(p, a)
	when jisfalsel       then tx:=do_isfalsel(p, a)

!	when jsliceptr      then do_sliceptr(p, a)
!
	when jincr          then
		if p.tclop in [kincrto, kdecrto] then
			do_incr(p, a)
		else
			tx:=do_incrload(p, a)
		fi
!
	when jbinto then
		do_binto(p, a, b)
!
	when junaryto then
		do_unaryto(p, a)
!
	when jsyscall then
		tx:=do_syscall(p, a)
!
	when jshorten then
		tx:=evalu(a)

	when jclear then
		tc_gen(kclear, evalref(a))
		setmode_u(a)

	when jcvlineno then
		tx:=tc_genint(getlineno(p.pos))

	when jsourceline then
 		tc_comment(" ")
 		tc_comment(p.a.svalue)

	when jnull then
		tx:=tc_genint(0)
	else

		GERROR_S("EVALUNIT NOT IMPL:",JTAGNAMES[P.TAG])
	end switch

	if p.mode<>tvoid and not p.resultflag AND TX then
		case p.tag
		when jassign, jcall, jsyscall then
		else
			if not jsolo[p.tag] then
				printunit(p)
				gerror_s("Not allowed by itself:", jtagnames[p.tag])
			fi

			tc_gen(KEVAL, TX)
			tx:=nil
		esac
	fi

	return tx
end

func evalref(unit p, q=nil)tclopnd tx=
	unit a, b, c
	tcl pold
	a:=p.a
	b:=p.b
	c:=p.c
	tclopnd bx, ux

	switch p.tag
	when jname then
!		tc_gen(kgetaddr, tx:=gendest(dx, p), genmem_d(p.def))
		tx:=genmemaddr_d(p.def)
		if q then				!offset applied
			bx:=evalu(q)
			pold:=tccurr
			ux:=tc_gen_ix(kaddpx, tx, bx, 1, 0)
			tx:=ux
			checkaddpx(pold, 200)
			setmode(tu64)
		fi

	when jindex then
		tx:=do_indexref(p, a, b)

	when jdot then
		tx:=do_dotref(p)

	when jptr then
		tx:=evalu(p.a)

	else
		case p.tag
		when jif then
			tx:=do_if(p, a, b, c, 1)

		elsif ttisblock[p.mode] then
			tx:=evalu(p)
		else
			PRINTUNIT(P)
			gerror("evalref")
		esac
	end switch

	return tx
end

global func evalx(unit p, int isref)tclopnd tx=
!call either eval (isref=0) or evalref(isref=1)
	if isref then
		tx:=evalref(p, nil)
	else
		tx:=evalu(p)
	fi
	return tx
end

global func evalblock(unit p)tclopnd tx=
	tx:=evalu(p)
	return tx
end

func evallv(unit p)tclopnd tx=
!	tx:=makeindlv(evalref(p, nil), p, p.mode)
	tx:=makeindlv(evalref(p, nil), p.mode)
	return tx
end

func do_block(unit p)tclopnd tx=
	unit a:=p.a

	while a and a.nextunit do
		evalu(a)
		a:=a.nextunit
	od
	if a then
		tx:=evalu(a)
		return tx
	fi
	return nil
end

proc docond(int opc, unit p, int lab)=
	genjumpcond(opc, p, lab)
end

proc genjumpcond(int opc, unit p, int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
	unit q, r, s
	int lab2, i
	tclopnd sx, qx, rx

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

	when jnotl, jisfalsel then
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
			evalu(q)
			q:=q.nextunit
		od
		genjumpcond(opc, q, lab)

	when jcmp then
		gcomparejump(opc, p.tclcond, q, r, lab)

	when jinrange then
		tc_gen4((opc=kjumpf|kjumpout|kjumpin), tc_genlabel(lab), 
				evalu(q), evalu(r.a), evalu(r.b))
		setmode_u(q)

	when jinset then
		s:=r.a
		if s=nil then
			gerror("empty set")
		fi

		if opc=kjumpf then
			lab2:=createfwdlabel()
			qx:=evalu(q)

			while s do
				sx:=evalu(s)
				s:=s.nextunit
				if s then
					tc_gen_cond(kjumpcc, eq_cc, tc_genlabel(lab2), qx, sx)
				else
					tc_gen_cond(kjumpcc, ne_cc, tc_genlabel(lab), qx, sx)
				fi
				setmode_u(q)
			od
			definefwdlabel(lab2)
		else
			qx:=evalu(q)

			while s, s:=s.nextunit do
				sx:=evalu(s)
				tc_gen_cond(kjumpcc, eq_cc, tc_genlabel(lab), qx, sx)
				setmode_u(q)
			od
			
		fi

	when jcmpchain then
		r:=q.nextunit
		qx:=evalu(q)
		i:=1

		if opc=kjumpf then
			while r do
				rx:=evalu(r)
				tc_gen_cond(kjumpcc, reversecond(p.cmpgenop[i]), tc_genlabel(lab), qx, rx)

				setmode_u(q)
				++i
				qx:=rx
				r:=r.nextunit
			od
		
		else
			lab2:=createfwdlabel()
			while r do
				rx:=evalu(r)
				if r.nextunit then
					tc_gen_cond(kjumpcc, reversecond(p.cmpgenop[i]), tc_genlabel(lab2), qx, rx)
				else
					tc_gen_cond(kjumpcc, p.cmpgenop[i], tc_genlabel(lab), qx, rx)
				fi
				setmode_u(q)
				++i
				qx:=rx
				r:=r.nextunit
			od
			definefwdlabel(lab2)
		fi
	else			!other, single expression
			tc_gen(opc, tc_genlabel(lab), evalu(p))
			setmode_u(p)
	end switch
end

proc gcomparejump(int jumpopc, int cond, unit lhs, rhs, int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode

	if jumpopc=kjumpf then			!need to reverse condition
		cond:=reversecond(cond)			!eqop => neop, etc
	fi

	tc_gen_cond(kjumpcc, cond, tc_genlabel(lab), evalu(lhs), evalu(rhs))
	setmode_u(lhs)
end

proc genjumpl(int lab)=
!generate unconditional jump to label
	tc_gen(kjump, tc_genlabel(lab))
end

proc unimpl(ichar mess)=
	gerror_s("Unimplemented: #", mess)
end

func do_const(unit p)tclopnd tx =
	int mode:=p.mode

	if ttisinteger[mode] then
		tx:=tc_genint(p.value)
	elsif ttisreal[mode] then
		if ttsize[mode]=4 then
			tx:=tc_genr32(p.xvalue)
		else
			tx:=tc_genreal(p.xvalue)
		fi

	elsif ttisref[mode] then
		if p.isastring then
			tx:=tc_genstring(p.svalue)
		else
			tx:=tc_genint(p.value)
		fi
	elsif mode=tbool64 then
		tx:=tc_genint(p.value)
	else
		gerror("do_const")
		return nil

	fi
	tx
end

proc do_null(unit p, a, b) =
	unimpl("do_null")
end

func do_name(unit p)tclopnd q=
	symbol d

	d:=p.def
	q:=nil


	case d.nameid
	when procid, dllprocid then
		q:=genmemaddr_d(d)

	when labelid then
		if d.index=0 then
			d.index:=++mlabelno
		fi
		if p.resultflag then
			q:=tc_genlabel(d.index)
		else
			genjumpl(d.index)
		fi

	else
		q:=genmem_d(d)
	esac
	return q
end

proc do_stop(unit p, a) =
	tc_gen(kstop, (a|evalu(a)|tc_genint(0)))
end


func do_andorl(unit p)tclopnd tx=
!do short-circuit evaluation of a&&b or a||b
!return operand containing 1 or 0
	int lab1, lab2

	lab1:=createfwdlabel()			!dest label of main condition (to end of if, or start if else)

	tx:=tc_gentemp()

	genjumpcond(kjumpf, p, lab1)

	lab2:=createfwdlabel()			!label past else part
	tc_gen(kmove, tx, tc_genint(1))
	setmode_u(p.a)

	genjumpl(lab2)

	definefwdlabel(lab1)
	tc_gen(kmove, tx, tc_genint(0))
	setmode_u(p.a)

	definefwdlabel(lab2)
	tx
end

func do_notl(unit p, a)tclopnd tx=
	tclopnd ax

	ax:=evalu(a)
	tx:=tc_gent(knot, ax)
	setmode(ti64)
	return tx
end

func do_istruel(unit p, a)tclopnd tx =
	tx:=tc_gent(ktoboolt, evalu(a))
	setmode(ti64)
	setmode2(a.mode)
	return tx
end

func do_isfalsel(unit p, a)tclopnd tx =
	tx:=tc_gent(ktoboolf,  evalu(a))
	setmode(ti64)
	setmode2(a.mode)
	return tx
end

func do_typepun(unit p, a)tclopnd tx =
!assume this is only used when needed

	tx:=tc_gent(ktypepun, evalu(a))
	setmode(p.mode)
	setmode2(a.mode)
	return tx
end

global func islogical(unit p)int=			!ISLOGICAL
!return 1 if p is known to have a logical value
	case p.tag
	when jistruel, jnotl, jandl, jorl then
		return 1
	esac
	return 0
end

func do_assign(unit p, a, b)tclopnd =
!fstore=1 when result is needed
	tclopnd lhs, rhs, ax
	tcl pold
	unit c
	symbol d
	int offset

	if b.tag=jmakelist then
		if p.resultflag then gerror("Share x:=(a, b, c)") fi
		if ttbasetype[a.mode]=tarray then
			do_assignarray(a, b)
		else
			do_assignrecord(a, b)
		fi
		return nil
	fi

!Special handling for index/slice/dot
	rhs:=evalu(b)

	case a.tag
	when jindex then
		do_storeindex(p, a.a, a.b, rhs)
		finish
	when jslice then
GERROR("ASS/SLICE")
		finish
	when jdot then
		do_storedot(a, a.b, rhs)
		finish
	esac

	switch a.tag
	when jname then
		tc_gen(kmove, genmem_u(a), rhs)

	when jptr then
		ax:=evalu(a.a)
		pold:=tccurr
		tc_gen_ixs(kistorex, ax, tc_genint(0), rhs)
		checkaddpx_store(pold, 101)

	when jdotindex then

		tc_gen(kstorebit, evallv(a.a), evalu(a.b), rhs)

	when jdotslice then
		tc_gen4(kstorebf, evallv(a.a), evalu(a.b.a), evalu(a.b.b), rhs)

	when jif then
		a.resultflag:=1
		lhs:=do_if(a, a.a, a.b, a.c, isref:1)
		tc_gen_ixs(kistorex, lhs, tc_genint(0), rhs)

	else
		cpl jtagnames[a.tag]
		gerror("Can't assign")
	end switch

	setmode_u(a)

!	setmemmode2(a, b)

finish:

	return rhs
end

func do_bin(unit p, a, b)tclopnd tx =
	tclopnd ax, bx
	tcl pold

	ax:=evalu(a)
	bx:=evalu(b)

	case p.tclop
	when kaddpx then
		pold:=tccurr
		tx:=tc_gen_ix(kaddpx, ax, bx, ttsize[tttarget[a.mode]])
		checkaddpx(pold, 102)

	when ksubpx, ksubp then
		tx:=tc_gent(p.tclop, ax, bx)
		tccurr.scale:=ttsize[tttarget[a.mode]]

	else							!most binary ops
		tx:=tc_gent(p.tclop, ax, bx)
	esac

	setmode_u(p)
	return tx
end

func do_setcc(unit p, a, b)tclopnd tx =
!	tc_gen_cond(ksetcc, p.condcode, tx:=gendest(dx, a), evalu(a), evalu(b))
	tc_gen_cond(ksetcc, p.tclcond, tx:=tc_gentemp(), evalu(a), evalu(b))
	setmode_u(a)
	return tx
end

func do_setccchain(unit p, q)tclopnd tx =
	int lab1, lab2, i
	unit r
	tclopnd qx, rx

	lab1:=createfwdlabel()
	lab2:=createfwdlabel()

	tx:=tc_gentemp()

	r:=q.nextunit

	qx:=evalu(q)
	i:=1
	while r do
		rx:=evalu(r)
		tc_gen_cond(kjumpcc, reversecond(p.cmpgenop[i]), tc_genlabel(lab1), qx, rx)

		setmode_u(q)
		++i
		qx:=rx
		r:=r.nextunit
	od

	tc_gen(kmove, tx, tc_genint(1))
	setmode(ti64)
	tc_gen(kjump, tc_genlabel(lab2))
	definefwdlabel(lab1)
	tc_gen(kmove, tx, tc_genint(0))
	setmode(ti64)
	definefwdlabel(lab2)

	return tx
end

proc do_binto(unit p, a, b)=
	tclopnd tx

	if a.tag=jname then
		tc_gen(p.tclop, evalu(a), evalu(b))
	else
		tc_gen(p.tclop, evallv(a), evalu(b))
	fi
	setmode_u(a)

	if ttisref[a.mode] and ttisinteger[b.mode] then
		tccurr.scale:=ttsize[tttarget[a.mode]]
	fi
end

func do_unary(unit p, a)tclopnd tx =
	tx:=tc_gent(p.tclop, evalu(a))
	setmode_u(p)
	return tx
end

proc do_unaryto(unit p, a)=
	tclopnd tx
	if a.tag=jname then
		tc_gen(p.tclop, evalu(a))
	else
		tc_gen(p.tclop, evallv(a))
	fi
	setmode_u(a)
end

func do_ptr(unit p, a, b)tclopnd tx =

!	tx:=makeind(evalu(a), p, tttarget[a.mode])
	tx:=makeind(evalu(a), tttarget[a.mode])

	return tx
end

proc do_labeldef(unit p)=
	symbol d

	d:=p.def
	if d.index=0 then
		d.index:=++mlabelno
	fi
	tc_gen(klabel, tc_genlabel(d.index), genmem_d(d))
end

proc do_goto(unit a)=
	symbol d

	case a.tag
	when jname and a.def.nameid=labelid then
		d:=a.def
		if d.index=0 then
			d.index:=++mlabelno
		fi
		tc_gen(kjump, tc_genlabel(d.index))

	else
		tc_gen(kijump, evalu(a))
		setmode(tu64)
	esac
end

proc do_do(unit p, a, b) =
	int lab_abc, lab_d

	lab_abc:=definelabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_abc, lab_abc, lab_d)

	evalblock(a)

	genjumpl(lab_abc)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_to(unit p, a, b) =
	unit avar
	int lab_b, lab_c, lab_d, count
	tclopnd cx

	cx:=tc_gentemp()

	a.mode:=ti64

	tc_gen(kmove, cx, evalu(a))
	setmode(ti64)

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	stacklooplabels(lab_b, lab_c, lab_d)

!check for count being nonzero
	if a.tag<>jconst then			!assume const limit is non-zero
		tc_gen_cond(kjumpcc, le_cc, tc_genlabel(lab_d), cx, tc_genint(0))
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

	tc_gen(kto, tc_genlabel(lab_b), cx)
	setmode(ti64)

	definefwdlabel(lab_d)
	--loopindex
end

proc do_while(unit p, pcond, pbody, pincr) =
	int lab_b, lab_c, lab_d, lab_incr

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

	evalblock(pbody)

	definefwdlabel(lab_c)

	if pincr then
		evalblock(pincr)
		definefwdlabel(lab_incr)
	fi

	docond(kjumpt, pcond, lab_b)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_repeat(unit p, a, b) =
	int lab_ab, lab_c, lab_d

	lab_ab:=definelabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_ab, lab_c, lab_d)

	evalblock(a)

	definefwdlabel(lab_c)

	unless b.tag=jconst and b.value=0 then
		docond(kjumpf, b, lab_ab)
	end

	definefwdlabel(lab_d)
	--loopindex
end

proc do_exit(unit p, int k) =
	int n, index

	index:=p.index
	if index=0 then index:=loopindex fi

	n:=findlooplabel(k, index)
	if n=0 then
		gerror("Bad exit/loop index", p)
	else
		genjumpl(n)
	fi
end

func do_if(unit p, pcond, plist, pelse, int isref)tclopnd tx =
	int labend, n, lab2

	labend:=createfwdlabel()

	n:=0

	if p.mode<>tvoid then
		tx:=tc_gentemp()
	else
		tx:=nil
	fi

	while pcond, (pcond:=pcond.nextunit; plist:=plist.nextunit) do
		lab2:=createfwdlabel()

		docond(kjumpf, pcond, lab2)

		fixtemp(p, tx, evalx(plist, isref))

		if pcond.nextunit or pelse then
			genjumpl(labend)
		fi
		definefwdlabel(lab2)
	od

	if pelse then
		fixtemp(p, tx, evalx(pelse, isref))
	fi

	definefwdlabel(labend)
	return tx
end

func do_return(unit p, a)tclopnd tx =
	if a then
		tc_gen(kretfn, tx:=evalu(a))
		setmode_u(a)
	else
		tc_gen(kretproc)
	fi
	tx
end

func do_returnmult(unit p, a)tclopnd tx =
	[maxtuplesize]tclopnd results
	int n

	n:=0
	while a, a:=a.nextunit do
		results[++n]:=evalu(a)
	od

	if n>maxtuplesize then gerror("Too many ret values") fi

	case n
	when 2 then
		tc_gen(kretmult, results[1], results[2])
	when 3 then
		tc_gen(kretmult, results[1], results[2], results[3])
	when 4 then
		tc_gen4(kretmult, results[1], results[2], results[3], results[4])
	else
		gerror("Retmult>4")
	esac

	results[1]
end

func do_callproc(unit p, a, b)tclopnd tx =
	[maxparams]tclopnd paramopnds
!	[maxparam]int parammodes
	[maxparams]unit paramlist
	unit c
	int nret, nparams, NUSED, ffi, i, mode
	symbol d
	tclopnd ax

	case a.tag
	when jname then
		d:=a.def
	when jptr then
		d:=ttnamedef[a.mode]

	else
		gerror("call/not ptr")
	esac

	ffi:=d.nameid=dllprocid

	tx:=nil
	nparams:=0

	c:=b
	while c, c:=c.nextunit do
		if nparams>=maxparams then gerror("call:too many params") fi
		paramlist[++nparams]:=c
	od

!need to store arg operands locally, as there may be nested called
	for i:=nparams downto 1 do				!normal RTL eval order (need for temp allocs when compiling tx/mm)
!	for i:=1 to nparams do					!new LRT eval order
		paramopnds[i]:=evalu(paramlist[i])
		setopndmode(paramopnds[i], paramlist[i].mode)
	od

!copy local args to global table
	for i to nparams do
		extparamopnds[i]:=paramopnds[i]

		if ffi and d.varparams and i>d.varparams and i<=4 then
			extparamopnds[i].isvariadic:=1			!whether params pushed as variadic
		fi
	od

	ax:=evalref(a)
	nret:=d.nretvalues
	if not p.resultflag then nret:=0 fi

	case nret
	when 0 then					!proc
		tc_gen_call(ax, 0, nparams)

	when 1 then					!func
		tx:=extretopnds[1]:=tc_gentemp()
		setopndmode(tx, d.mode)
		tc_gen_call(ax, 1, nparams)

	else						!mult func
!ASSUME that all return values are used. Zero return values are check above.
!For 1 <= nused < nret, I need to find a way to get the information. Possibly
!it can be the value of .result flag. For now, assume NUSED=NRET
		NUSED:=NRET

		for i to nused do
!			extretopnds[i]:=tc_gentemp(gettclmode(ttmult[d.mode, i]))
			extretopnds[i]:=tc_gentemp()
			setopndmode(extretopnds[i], ttmult[d.mode, i])
		od

		tc_gen_call(ax, nused, nparams)
	esac

	tccurr.mode:=gettclmode(p.mode)
	tccurr.size:=ttsize[p.mode]

	if d.varparams then
		tccurr.isvariadic:=1
	fi

	if ttisblock[p.mode] and not p.resultflag then	!???
		return nil
	else
		return tx
	fi
end

proc do_print(unit p, a, b) =
	tclopnd ax
	unit q, r, fx
	int m, fn

	if a then
		if ttbasetype[a.mode]<>tref then gerror("@dev no ref") fi

		case ttbasetype[tttarget[a.mode]]
		when tvoid then
			gensysproc(sf_print_startfile, a)
		when tc8 then
			gensysproc(sf_print_startstr, a)
		when tref then
			gensysproc(sf_print_startptr, a)
		else
			gerror("@dev?")
		esac
	else
		gensysproc(sf_print_startcon)
	fi

	q:=b

	case p.tag
	when jfprint, jfprintln then
		if ttbasetype[q.mode]<>tref or ttbasetype[tttarget[q.mode]]<>tc8 then
			gerror("string expected")
		fi
		gensysproc(sf_print_setfmt, q)
		q:=p.c
	esac

	while q do
		case q.tag
		when jfmtitem then
			fx:=q.b
			r:=q.a
			m:=r.mode
		when jnogap then
			gensysproc(sf_print_nogap)
			q:=q.nextunit
			nextloop
		when jspace then
			gensysproc(sf_print_space)
			q:=q.nextunit
			nextloop
		else
			fx:=nullunit
			r:=q
			m:=q.mode
		esac

		switch ttbasetype[m]
		when ti64 then
			fn:=sf_print_i64
			if not fx then fn:=sf_print_i64_nf fi
		when tu64 then
			fn:=sf_print_u64
		when tr32 then
			fn:=sf_print_r32
		when tr64 then
			fn:=sf_print_r64
		when tref then
			if tttarget[m]=tc8 or tttarget[m]=tarray and tttarget[tttarget[m]]=tc8 then
				fn:=sf_print_str
				if not fx then fn:=sf_print_str_nf fi
			else
				fn:=sf_print_ptr
				if not fx then fn:=sf_print_ptr_nf fi
			fi
		when tarray then
			GERROR("PRINTARRAY")
			q:=q.nextunit
		when trecord then
			GERROR("PRINTRECORD")
		when tslice then
			if tttarget[m]=tc8 then
				fn:=sf_print_strsl
			else
				gerror("PRINTSLICE")
			fi

		when tc64 then
			fn:=sf_print_c8

		else
			gerror_s("PRINT/T=#", strmode(m))
		end switch

		case fn
		when sf_print_i64_nf, sf_print_str_nf, sf_print_ptr_nf then
			gensysproc(fn, r)
		else
			gensysproc(fn, r, fx)
		esac


		q:=q.nextunit
	od

	case p.tag
	when jprintln, jfprintln then
		gensysproc(sf_print_newline)
	esac
	gensysproc(sf_print_end)

end

proc do_incr(unit p, a) =
	tclopnd tx

	if a.tag=jname then
		tc_gen(p.tclop, genmem_u(a))
	else

		tc_gen(p.tclop, evallv(a))
	fi

	setmode_u(a)
	setincrstep(a.mode)
end

proc setincrstep(int m)=
	tccurr.step:=1

	if ttisref[m] then
		tccurr.step:=ttsize[tttarget[m]]
	fi
end

func do_incrload(unit p, a)tclopnd =
	tclopnd tx
	int opc

	tx:=tc_gentemp()

	if a.tag=jname then
		tc_gen(p.tclop, tx, genmem_u(a))
	else
		tc_gen(p.tclop, tx, evallv(a))
	fi
	setmode_u(a)
	setincrstep(a.mode)
	return tx
end

proc do_for(unit p, pindex, pfrom, pbody, int down) =
	unit pto, pstep, pelse, px, plimit
	int lab_b, lab_c, lab_d, lab_e
	int a, b, step
	tclopnd qindex, qfrom, qto

	pto:=pfrom.nextunit
	pstep:=pto.nextunit
	pelse:=pbody.nextunit

	if pto.tag=jptr then
		px:=pto.a
		symbol d
		if px.tag=jname and (d:=px.def).nameid=paramid and
			 d.parammode=byref_param then
			gerror("Possibly using &param as for-loop limit")
		fi
	fi

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pelse then
		lab_e:=createfwdlabel()
	else
		lab_e:=lab_d
	fi

	stacklooplabels(lab_b, lab_c, lab_d)

!now start generating code
	qindex:=evalu(pindex)
	qfrom:=evalu(pfrom)
	qto:=evalu(pto)

	tc_gen(kmove, qindex, qfrom)
	setmode_u(pindex)

	if pfrom.tag=jconst and pto.tag=jconst then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			tc_gen(kjump, tc_genlabel(lab_e))
		fi
	else
		if pfrom.tag=jconst then				!reverse condition; compare mem:imm
			tc_gen_cond(kjumpcc, (down|gt_cc|lt_cc), tc_genlabel(lab_e), qto, qfrom)
		else
			tc_gen_cond(kjumpcc, (down|lt_cc|gt_cc), tc_genlabel(lab_e), qindex, qto)
		fi
		setmode_u(pindex)
	fi

	definefwdlabel(lab_b)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	if pstep then
		if pstep.tag<>jconst then
			gerror("for/step non-const not ready")
		fi
		step:=pstep.value
		if step<=0 then
			gerror("Bad for-step")
		fi
		tc_gen((down|kfordown|kforup), tc_genlabel(lab_b), qindex, qto)
		tccurr.index:=step
	else
		tc_gen((down|kfordown|kforup), tc_genlabel(lab_b), qindex, qto)
		tccurr.index:=1
	fi
	setmode_u(pindex)

	if pelse then
		definefwdlabel(lab_e)
		evalblock(pelse)
	fi

	definefwdlabel(lab_d)
	--loopindex
end

proc do_forall(unit p, pindex, plist, pbody, int down) =
	unit plocal, pfrom, pto, pelse, px, plimit, passign
	int lab_b, lab_c, lab_d, lab_e
	int a, b, step
	tclopnd qindex, qlocal, qfrom, qto

	plocal:=pindex.nextunit
	pfrom:=plocal.nextunit
	pto:=pfrom.nextunit
	passign:=plist.nextunit
	pelse:=pbody.nextunit

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pelse then
		lab_e:=createfwdlabel()
	else
		lab_e:=lab_d
	fi

	stacklooplabels(lab_b, lab_c, lab_d)

!now start generating code
	qindex:=evalu(pindex)
	qlocal:=evalu(plocal)
	qfrom:=evalu(pfrom)
	qto:=evalu(pto)

	tc_gen(kmove, qindex, qfrom)
	setmode_u(pindex)

	if pfrom.tag=jconst and pto.tag=jconst then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			tc_gen(kjump, tc_genlabel(lab_e))
		fi
	else
		if pfrom.tag=jconst then				!reverse condition; compare mem:imm
			tc_gen_cond(kjumpcc, (down|gt_cc|lt_cc), tc_genlabel(lab_e), qto, qfrom)
		else
			tc_gen_cond(kjumpcc, (down|lt_cc|gt_cc), tc_genlabel(lab_e), qindex, qto)
		fi
		setmode_u(pindex)
	fi

	definefwdlabel(lab_b)

	evalu(passign)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	tc_gen((down|kfordown|kforup), tc_genlabel(lab_b), qindex, qto)
	tccurr.index:=1
	setmode_u(pindex)

	if pelse then
		definefwdlabel(lab_e)
		evalblock(pelse)
	fi

	definefwdlabel(lab_d)
	--loopindex
end

func do_convert(unit p, a)tclopnd tx =
	int opc


	case p.convcode
	when kksoftconv then
		tx:=evalu(a)
	when kkerror then
		gerror("CONV/ERROR")
	else
		tc_gen(convtotcl[p.convcode], tx:=tc_gentemp(), evalu(a))
		setmode_u(p)
		setmode2(p.convmode)
	esac

	return tx
end

func do_dot(unit pdot)tclopnd tx =
	unit a
	int offset
	tcl pold
	tclopnd ax

	a:=nil
	offset:=getdotoffset(pdot, a)

	ax:=evalref(a)
	pold:=tccurr

	tx:=tc_gen_ix(kiloadx, ax, nil, 1, offset)
	checkaddpx(pold, 103)

	setmode_u(pdot)

	return tx
end

func do_dotref(unit pdot)tclopnd tx =
	unit a
	int offset
	tcl pold
	tclopnd ax

	a:=nil
	offset:=getdotoffset(pdot, a)

	ax:=evalref(a)
	pold:=tccurr
	tx:=tc_gen_ix(kaddpx, ax, nil, 1, offset)
	setmode(tu64)
	checkaddpx(pold, 104)

	return tx
end

proc do_storedot(unit pdot, pfield, tclopnd rhs) =
	unit a
	int offset
	tcl pold
	tclopnd ax

	a:=nil
	offset:=getdotoffset(pdot, a)

	ax:=evalref(a)
	pold:=tccurr

	tc_gen_ixs(kistorex, ax, tc_genint(0), rhs, 1, offset)
	checkaddpx_store(pold, 105)

	setmode_u(pdot)
end

func do_index(unit p, parray, pindex)tclopnd tx =
	int addoffset, scale
	tcl pold
	tclopnd ax, bx

	addoffset:=getindexoffset(pindex)

	scale:=ttsize[tttarget[parray.mode]]

	ax:=evalarray(parray)
	bx:=evalu(pindex)

	pold:=tccurr
	tx:=tc_gen_ix(kiloadx, ax, bx, 
		scale, -ttlower[parray.mode]*scale + addoffset*scale)
	checkaddpx(pold, 106)

	setmode_u(p)

	return tx
end

func evalarray(unit p)tclopnd tx=
	tclopnd px
	case ttbasetype[p.mode]
	when tslice then
		px:=evalref(p)
		tc_gen(kiloadx, tx:=tc_gentemp(), px)
		setmode(tu64)
		tx

	elsif p.mode=trefchar then
		evalu(p)
	else
		evalref(p)
	esac
end

proc do_storeindex(unit p, parray, pindex, tclopnd rhs) =
	tclopnd px
	int addoffset, scale, emode
	tcl pold
	tclopnd ax, bx

	addoffset:=getindexoffset(pindex)

	scale:=ttsize[emode:=tttarget[parray.mode]]

	ax:=evalarray(parray)
	bx:=evalu(pindex)
	pold:=tccurr
	tc_gen_ixs(kistorex, ax, bx, rhs, 
		scale, -ttlower[parray.mode]*scale + addoffset*scale)
	checkaddpx_store(pold, 107)
!
	setmode(emode)
end

func do_indexref(unit p, parray, pindex)tclopnd tx =
	int addoffset, scale
	tcl pold
	tclopnd ax, bx

	addoffset:=getindexoffset(pindex)

	scale:=ttsize[tttarget[parray.mode]]

	ax:=evalarray(parray)
	bx:=evalu(pindex)

	pold:=tccurr
	tx:=tc_gen_ix(kaddpx, ax, bx, scale, -ttlower[parray.mode]*scale+addoffset*scale)

	checkaddpx(pold, 108)
	setmode(tu64)

	return tx
end

func getindexoffset(unit &pindex)int offset=
!convert index like [i+3] to [i], returning the +3 etc as a separate offset
	int addoffset:=0

	if pindex.tag=jbin and pindex.tclop in [kadd, ksub] then

!	case pindex.tag
!	when jadd, jsub then
		if pindex.b.tag=jconst then		!incorporate const offset into lwb adjustment
			addoffset:=(pindex.tclop=kadd|pindex.b.value|-pindex.b.value)
			pindex:=pindex.a
		fi
	fi
	return addoffset
end

func do_switch(unit p, pindex, pwhenthen, pelse, int isref)tclopnd tx =
!'looptype' is set up here:
! 0 = switch	normal switch (range-checked)
! 1 = doswitch	looping switch (range-checked)
! 2 = doswitchu	looping switch via computed goto/indexed (both non-range-checked)
! 3 = doswitchx	looping switch via computed goto/labels

	const maxlabels = 1000
	int minlab, maxlab, n, iscomplex, i
	int lab_a, lab_b, lab_d, labjump, elselab, labstmt, ax, bx, ismult

	tcl pprev, p1
	symbol djump
	byte looptype, opc

	[0..maxlabels]tcl labels
	unit w, wt, pjump

	case p.tag
	when jswitch then
		looptype:=0; opc:=kswitch
	when jdoswitch then
dodosw:
		looptype:=1; opc:=kswitch
	when jdoswitchu then
		if ctarget then dodosw fi			
		looptype:=2; opc:=kswitchu
	else
		looptype:=3
	esac

	if p.mode<>tvoid then
		tx:=tc_gentemp()
	else
		tx:=nil
	fi

	minlab:=1000000
	maxlab:=-1000000		!highest index seen

	n:=0				!no. different values
	iscomplex:=0			!whether complex switch

	wt:=pwhenthen
	while wt do
		w:=wt.a
		while w do		!for each when expression
			case w.tag
			when jmakerange then
				ax:=w.a.value
				bx:=w.b.value
	dorange:
				for i:=ax to bx do
					minlab := min(i, minlab)
					maxlab := max(i, maxlab)
				od
			when jconst then		!assume int
				ax:=bx:=w.value
				goto dorange
			else
				gerror_s("Switch when2: not const: #", strexpr(w).strptr)
			esac
			w:=w.nextunit
		od
		wt:=wt.nextunit
	od

	n:=maxlab-minlab+1
	if n>maxlabels then
		gerror("Switch too big")
	fi

	if looptype then
		lab_a:=definelabel()
		lab_d:=createfwdlabel()
		stacklooplabels(lab_a, lab_a, lab_d)
	else
		lab_d:=createfwdlabel()
	fi

	labjump:=createfwdlabel()
	elselab:=createfwdlabel()

	if looptype=3 then		!need to initialise pointer to JT
		pjump:=pindex.nextunit
		if pjump.tag<>jname then gerror("doswx not name") fi
		djump:=pjump.def
		if ttbasetype[djump.mode]<>tref then gerror("doswx not ref") fi

		pprev:=tccurr
		tc_gen(kmove, genmem_u(pjump), tc_genlabel(labjump))
		setmode(tu64)

		if tcldoswx=nil then
			gerror("doswx in main?")
		fi

!move just-create <move> op to just before instr following tcldosw

		p1:=tcldoswx.next
		tcldoswx.next:=tccurr
		tccurr.next:=p1
		tccurr:=pprev
		pprev.next:=nil	

	fi

	if looptype<>3 then
		doswx_dispatch(opc, pindex, labjump, elselab, minlab, maxlab)

	else
		tc_gen(kijump, evalu(pindex))
		setmode(tu64)
	fi

	definefwdlabel(labjump)

	for i:=minlab to maxlab do			!fill with else labels first
		tc_gen(kswlabel, tc_genlabel(elselab))
		labels[i]:=tccurr
	od

!scan when statements again, o/p statements

	wt:=pwhenthen
	while wt do
		labstmt:=definelabel()
		w:=wt.a
		while w do
			case w.tag
			when jmakerange then
				ax:=w.a.value
				bx:=w.b.value
			when jconst then
					ax:=bx:=int(w.value)
			esac
			for i:=ax to bx do
				labels[i].a.labelno:=labstmt
			od
			w:=w.nextunit
		od

		fixtemp(p, tx, evalx(wt.b, isref))

!		genjumpl((loopsw|lab_a|lab_d))
		case looptype
		when 0 then
			genjumpl(lab_d)
		when 1 then
			genjumpl(lab_a)
		when 2 then
			doswx_dispatch(opc, pindex, labjump, elselab, minlab, maxlab)
		else
			tc_gen(kijump, evalu(pindex))
			setmode(tu64)
		esac

		wt:=wt.nextunit
	od

	definefwdlabel(elselab)
	if pelse then
		fixtemp(p, tx, evalx(pelse, isref))
	fi

	if looptype then
		case looptype
		when 1 then
			genjumpl(lab_a)
		when 2 then
			doswx_dispatch(opc, pindex, labjump, elselab, minlab, maxlab)
		else
			tc_gen(kijump, evalu(pindex))
			setmode(tu64)
		esac
		--loopindex
	fi

	definefwdlabel(lab_d)

	return tx
end

proc doswx_dispatch(int opc, unit pindex, int labjump, elselab, minlab, maxlab) =
	tc_gen(opc, tc_genlabel(labjump), tc_genlabel(elselab), evalu(pindex))
	setmode_u(pindex)
	tccurr.minlab:=minlab
	tccurr.maxlab:=maxlab
end

func do_select(unit p, a, b, c, int isref)tclopnd tx =
	const maxlabels=256
	[maxlabels]tcl labels
	int labend, labjump, n, i, elselab, labstmt
	unit q

	if p.mode<>tvoid then
		tx:=tc_gentemp()
	else
		tx:=nil
	fi

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

	tc_gen(kswitch, tc_genlabel(labjump), tc_genlabel(elselab), evalu(a))
	setmode_u(a)

	tccurr.minlab:=1
	tccurr.maxlab:=n

	definefwdlabel(labjump)

	q:=b
	i:=0
	for i:=1 to n do
		tc_gen(kswlabel, tc_genlabel(elselab))
		labels[i]:=tccurr
	od

	q:=b
	i:=0
	while q do
		labstmt:=definelabel()
		++i
		labels[i].a.labelno:=labstmt
		fixtemp(p, tx, evalx(q, isref))
		genjumpl(labend)
		q:=q.nextunit
	od

	definefwdlabel(elselab)

	fixtemp(p, tx, evalx(c, isref))

	definefwdlabel(labend)
	return tx
end

func do_case(unit p, pindex, pwhenthen, pelse, int loopsw, isref)tclopnd tx =
	const maxcase=255					!256 including else
	[maxcase+1]int labtable
	[maxcase+1]unit unittable
	int ncases, opc

	int lab_abc, lab_d, labnextwhen, labstmtstart, labelse
	unit w, wt
	tclopnd cx

	if pindex=nil then
		GERROR("EMPTY CASE NOT DONE")
	fi

	if loopsw then
		lab_abc:=definelabel()		!start of loop
		lab_d:=createfwdlabel()	!end of case/end of loop
		stacklooplabels(lab_abc, lab_abc, lab_d)
	else
		lab_d:=createfwdlabel()	!end of case/end of loop
	fi

	ncases:=0

	if casedepth>=maxcasedepth then
		gerror("case nested too deeply")
	fi
	casestmt[++casedepth]:=p

	wt:=pwhenthen

	if wt then
		cx:=evalu(pindex)

		while wt do
			w:=wt.a
			if ncases>=maxcase then
				gerror("too many cases")
			fi
			labtable[++ncases]:=createfwdlabel()
			unittable[ncases]:=wt.b

			while w, w:=w.nextunit do
				tc_gen_cond(kjumpcc, eq_cc, tc_genlabel(w.whenlabel:=labtable[ncases]), cx, evalu(w))
				setmode_u(w)
			od

			wt:=wt.nextunit
		od

	fi

	labelse:=createfwdlabel()
	caseelse[casedepth]:=labelse
	genjumpl(labelse)

	if p.mode<>tvoid then
		tx:=tc_gentemp()
	else
		tx:=nil
	fi

	for i:=1 to ncases do
		definefwdlabel(labtable[i])
		fixtemp(p, tx, evalx(unittable[i], isref))

		if loopsw then
			genjumpl(lab_abc)
		else
			genjumpl(lab_d)
		fi
	od

	definefwdlabel(labelse)

	if pelse then
		fixtemp(p, tx, evalx(pelse, isref))
	fi

	if loopsw then
		genjumpl(lab_abc)
		definefwdlabel(lab_d)
		--loopindex
	else
		definefwdlabel(lab_d)
	fi
!
	--casedepth

	return tx
end

proc fixtemp(unit p, tclopnd tx, ax)=
	if tx then
		tc_gen(kmove, tx, ax)
		setmode_u(p)
	fi
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
			if w.tag=jconst and ttisinteger[w.mode] and w.value=casevalue then
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

proc do_swap(unit p, a, b) =
	if a.tag=b.tag=jname then
		tc_gen(kiswap, evalu(a), evalu(b))
	else
		tc_gen(kiswap, evallv(a), evallv(b))
	fi

	setmode_u(a)
end

func do_dotindex(unit p, a, b)tclopnd tx =
	tx:=tc_gent(kloadbit, evalu(a), evalu(b))
	setmode(ti64)
	return tx
end

func do_dotslice(unit p, a, b)tclopnd tx =
	tclopnd ax, bx
	ax:=evalu(a)
	bx:=evalu(b.a)

	tx:=tc_gent4(kloadbf, ax, bx, evalu(b.b))
	setmode(ti64)
	return tccurr.a
end

func do_evalu(unit a)tclopnd tx =
	tx:=tc_gent(kmove, evalu(a))
	setmode_u(a)

	tc_gen(keval, tx)
	setmode_u(a)
	tx
end

func do_read(unit p, a)tclopnd tx =
	unit fx
	int m, fn

	if a then			!format
		fx:=a
	else
!		fx:=tc_genint(0)
		fx:=nullunit
	fi

	m:=p.mode

!	tx:=gendest(dx)

	if ttisinteger[m] then
		fn:=sf_read_i64
	elsif ttisreal[m] and ttsize[m]=8 then
		fn:=sf_read_r64
	elsif m=trefchar then
		fn:=sf_read_str
	else
		GERROR("CAN'T READ THIS ITEM")
	fi
	tx:=gensysfn(fn, p.mode, fx)

	return tx
end

proc do_readln(unit a) =
	if a then
		if ttbasetype[a.mode]<>tref then gerror("@dev no ref") fi

		case ttbasetype[tttarget[a.mode]]
		when tvoid then
			gensysproc(sf_read_fileline, a)
		when tu8, tc8 then
			gensysproc(sf_read_strline, a)
		else
			gerror("rd@dev?")
		esac
	else
		gensysproc(sf_read_conline)
	fi
end

func do_slice(unit p, pslice, prange)tclopnd tx =
GERROR("SLICE")
!	int addoffset
!
!	addoffset:=getindexoffset(prange.a)
!
!!	tc_gen(ksetopndd, evalu(prange.b))
!	tc_gen4(kgetslice, tx:=gendest(dx, p), evalref(pslice), evalu(prange.a), 
!		evalu(prange.b))
!
!	!setmode(getmemmode(p))
!	tccurr.scale:=ttsize[tttarget[pslice.mode]]
!
!	tccurr.extra:=-ttlower[pslice.mode]*tccurr.scale + addoffset*tccurr.scale
	return tx
end

func do_syscall(unit p, a)tclopnd tx=
	psymbol pname

	setfunctab()

	case p.fnindex
	when sf_getnprocs then
		tx:=tc_gent(kmove, tc_genmem(pnprocs))

	when sf_getprocname then
		pname:=pprocname
doprocname:
!		tc_gen_ix(kiloadx, tx, tc_gennameaddr(name), evalu(a), 8, -8)
		tx:=tc_gen_ix(kiloadx, tc_genmemaddr(pname), evalu(a), 8, -8)

	when sf_getprocaddr then
		pname:=pprocaddr
		doprocname
	else
		TC_COMMENT("SYSCALL")
	esac

	setmode_u(p)

	return tx
end

func getdotoffset(unit p, &pname)int=
!return accumulated offset of this and nested dot-expressions, 
!also returns the terminal dot unit to be evaluated
	int offset, axmode

	case p.tag
	when jdot then
		offset:=getdotoffset(p.a, pname)
		return p.offset+offset

	else							!anything else, is the start expression
		pname:=p
		return p.offset
	esac
end

proc do_assignmm(unit p, a, b) =
!fstore=1 when result is needed
	[50]tclopnd temps
	int n
	tclopnd ax, bx
	unit q

	q:=b.a
	n:=0

	while q, q:=q.nextunit do
		++n
		if n>temps.upb then gerror("assignmm?") fi

		bx:=evalu(q)
		if bx.optype<>mem_opnd then
			ax:=bx
		else						!need to copy value to allow swap/rotate
			ax:=gendest(nil)
			tc_gen(kmove, ax, bx)
			setmode_u(q)
		fi
		temps[n]:=ax

	od

	q:=a.a
	n:=0
	while q, q:=q.nextunit do
		++n
		tc_gen(kmove, evallv(q), temps[n])
		setmode_u(q)
	od

end

proc do_assignms(unit p, a, b) =
!fstore=1 when result is needed
!	[50]tclopnd temps
	int nlhs, nret
	tclopnd ax, bx
	unit q
	tcl pcall

	bx:=evalu(b)
	pcall:=tccurr

	if pcall.opcode<>kcall then
		gerror("assignms not call")
	fi
	nret:=pcall.nret
	nlhs:=a.length

	if nlhs<>nret then gerror("ass/ms N?") fi

	q:=a.a
	for i to nret do
		ax:=pcall.abc[i]
		tc_gen(kmove, evallv(q), pcall.abc[i])
		setmode_u(q)
	
		q:=q.nextunit
	od

end

proc do_assignmdrem(unit p, a, b) =
	tclopnd ax, bx, tx1, tx2
	unit q

	ax:=evalu(b.a)
	bx:=evalu(b.b)

	tx1:=tc_gentemp()
	tx2:=tc_gentemp()

	tc_gen4(kidivrem, tx1, tx2, ax, bx)
	setmode_u(b.a)

	q:=a.a
	tc_gen(kmove, evallv(q), tx1)
	setmode_u(q)
	tc_gen(kmove, evallv(q.nextunit), tx2)
	setmode_u(q)
end

proc do_assignarray(unit a, b)=
!a is an array type; b is makelist
	unit q
	int scale
	tcl pold
	tclopnd ax, bx

!	if ttbasetype[tttarget[a.mode]]=tc8 then
!		gerror("Assignment not suitable for []char type")
!	fi

	q:=b.a
	for i to b.length do
		scale:=ttsize[tttarget[a.mode]]

		ax:=evalarray(a)
		bx:=evalu(q)

		pold:=tccurr
		tc_gen_ixs(kistorex, ax, tc_genint(i), bx,
			scale, -ttlower[a.mode]*scale)
		checkaddpx_store(pold, 110)
		setmode(tttarget[a.mode])

		q:=q.nextunit
	od
end

proc do_assignrecord(unit a, b)=
!a is a record, b is makelist

	unit q
	int m, fieldtype
	symbol d, e
	tcl pold
	tclopnd ax, bx

	m:=a.mode
	d:=ttnamedef[m]

	e:=d.deflist
	q:=b.a
	while e, e:=e.nextdef do
		if e.nameid=fieldid and e.mode<>tbitfield then
			ax:=evallv(a)
			bx:=evalu(q)
			pold:=tccurr
			tc_gen_ixs(kistorex, ax, tc_genint(0), bx, 1, e.offset)
			checkaddpx_store(pold, 111)
			setmode(e.mode)

			q:=q.nextunit
		fi
	od
end

=== mm_decls.m 0 0 28/40 ===
global const maxmodule=300
global const maxsubprog=30
global const maxlibfile=50
global const maxsourcefile=300

global type symbol		= ref strec
global type unit  		= ref unitrec
global type imodule   	= ref modulerec
global type ifile   	= ref filerec
global type isubprog  	= ref subprogrec

global macro pr(a,b)	= (a<<16 ior b)

global record tokenrec =
	byte symbol
	byte subcode
	u16 slength				!string length; includes any zero term
	u32 pos: (sourceoffset:24, fileno:8)

	union
		ref strec symptr		!pointer to symbol table entry for name
		i64 value				!64-bit int
		real xvalue				!64-bit float
		u64 uvalue			!64-bit word
		ichar svalue			!pointer to string or charconst (not terminated)
	end
end

global record procrec =
	symbol def
	ref procrec nextproc
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
	ref i32 pmode
end

global record posrec=
	u32 pos: (sourceoffset:24, fileno:8)
end

global record uflagsrec =
	[7]byte	codes
	byte	ulength
end

global record strec = $caligned
	ichar name
	ref strec owner
	ref strec deflist
	ref strec deflistx
	ref strec nextdef
	ref strec nextdupl
	ref strec firstdupl			!point to generic version
!	union
	psymbol pdef			!tcl st version
!		unit pdoswx				!doswitchx used for ctarget
!	end

	unit code			!var idata/proc body/taggedunion tag value/etc

	i32 mode
	byte namelen
	byte symbol
	byte nameid
	byte subcode

	i32 index				!misc; eg. holds label numbers
	i32 offset

	u32 pos: (sourceoffset:24, fileno:8)
	u16 flags: (
		isstatic:1,
		hasdoswx:1,
		txdone:1,
		circflag:1,

		islet:1,
		addrof:1,
!		noreg:1,
		ishandler:1,

		atfield:1,
		atvar:1,
		istabdata:1,			!mark parallel enum/tabdata arrays

		issubprog:1,			!set in resolvetopname: module is also a subprog

		isimport:1)

	byte moduleno
	byte subprogno

	unit equivvar

	struct				!when a proc
		ichar truename			!for imported name only
		ref strec paramlist

		byte dllindex			!for dllproc: which dll in dlltable

		byte nretvalues			!func: number of return values (0 for proc)
		byte varparams			!0 or 1; variadic params in B and FF
		byte isthreaded			!
	end

	struct						!when a record or record field
		ref strec equivfield
		uflagsrec uflags
		i32 baseclass
		byte bitfieldwidth		!width of bitfield in record
		byte align				!0, 2, 4, 8, 16 or 255 (auto-align)
		byte bitoffset			!0..31 for bitfields in records
		byte equivoffset
	end

	struct				!when a param name
		ref strec nextparam
		byte parammode			!0=byval_param, in_param, byref_param
		byte optional			!0 or 1	
		byte variadic			!variadic parameter for B code
		byte dummy3				!variadic parameter for B code
	end

	i16 regsize
	i16 maxalign		!for record types (doesn't fit above)
	u16 used

	byte scope
	byte equals			!for vars/params: 1/2/3 means =/:=/::= used

end


global record unitrec =
	byte tag				!jcode tag number
	byte insptr
	byte txcount
	byte spare
	u32 pos: (sourceoffset:24, fileno:8)

	unit nextunit

	union
		struct
			union
				unit	a
				symbol	def
				symbol	labeldef
				i64	value
				u64	uvalue
				r64	xvalue
				ichar	svalue
				i64	range_lower
			end

			union
				unit	b
				i64	range_upper
			end

			union
				unit	c
				[4]i16	cmptclmode
			end
		end
		[3]unit abc
	end

	union						!misc stuff depends on tag
		struct					!const string
			u32 slength			!includes any zero term
			byte isastring
			char strtype		!0/'B'/'S' = normal / bindata / strdata
		end

		struct					!name
			byte dottedname		!for jname: 1=resolved from fully qualified dotted seq
			byte avcode			!jname for/autovars: 'I','T','S' = index/to/step autovars
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
				byte compactif	!for jif, 1 is using (a|b|c)
			end
			u64 reginfo
		end

		union					!for makelist
			u32 length		!number of elements
			byte makearray		!1 for makelist to create array-var not list-var
		end
		byte addroffirst	!1 for jnameaddr when derived from &.name

		u32 offset			!for jdot
		i32 whenlabel			!label no associated with when expr; for recase
		i32 swapvar			!for j-swap: 1 when swapping var:ref

		struct
			union
				i16 bitopindex	!
				i16 opcindex		!operator nodes
				i16 fnindex		!sf_add_var etc
!				i16 condcode		!tcl_eq etc; for jeq etc
				i16 bfcode
			end
		end
		i32 index
		[4]byte cmpgenop			!cmpchain: up to 8 genops
	end

	i32 mode
	union
		i32 convmode	!convert/typepun: source/target(?) mode (will be widened to give unit mode)
!		i32 memmode	!name/ptr/index/dot: void=LVALUE; non-void=RVALUE
		i32 elemmode	!for jnew/newvar
	end

	byte moduleno
	byte subprogno
	byte initlet		!1 for an assignment that initialises a let
	byte isconst		!1 for jconst, and jmakerange with const range

	byte resultflag		!1 when the result of this unit is needed; 0=void or discarded
	union
		byte tclop			!generic operator for jbin, incr etc
		byte propcode		!kklen etc
		byte inv			!notin
		byte convcode		!kkfix etc
	end
	byte istrueconst	!1 for actual "123" etc, not result of reduction
	byte tclcond		!eq_cc etc
end

global record modulerec=
	ichar	name				!module name and base filename
	ifile	file
	i16	moduleno			!useful if using pointer to a source rec
	i16	subprogno
	i16	fileno
	byte	issyslib
	byte	islead				!1 if lead module in sp

	union
		symbol	stmodule
		symbol	def
	end

	symbol	stsubprog
	symbol	stmacro

	symbol	ststart				!nil, or st entry of start()
	symbol	stmain				!nil, or st entry of main()
end

global record filerec=
	ichar	name				!module name and base filename
	ichar	filename			!base filename + extension
	ichar	path				!path where file resides
	ichar	filespec			!full file path
	ichar	text				!pointer to source text, 0-terminated
	ichar	dupl				!for ma files
	int		size				!source file size includes terminator

	byte	issyslib			!1 if a system module
	byte	issupport			!1 if a support file (strinclude); MAY BE STORED ELSEWHERE
	byte	compiled			!1 if compiled
	byte	islead				!1 if lead module in sp

	i16	subprogno
	i16	moduleno			!0, or moduleno

	i16	fileno				!refers to self
	i16	spare

end

global record subprogrec =
	ichar name
	i16 firstmodule			!will be header module or same as mainmodule if no header
	i16 mainmodule			!0, or module containing 'main'
	i16 lastmodule			!always first..lastmodule
!	i16 compiled				!1 if compiled
	byte flags:(compiled:1, issyslib:1)
	byte subprogno
end

global [0..maxmodule]imodule	modules
global [0..maxmodule]byte		moduletosub				!convert module no to subprog no
global [0..maxsubprog]isubprog	subprogs
global [0..maxsourcefile]ifile	sources
global [0..maxsubprog]byte		subproghasstart

global int nmodules
global int nsubprogs
global int nsourcefiles
global int nlibfiles

global symbol stprogram		!root into the symbol table
global symbol stmodule		!main module
global int currmoduleno				!used when compiling modules
global byte loadedfromma	!1 if source/support files are in loaded .ma file

global tokenrec lx				!provides access to current token data
global tokenrec nextlx			!provides access to next token

global [0..maxlibfile]ichar libfiles

global int mainsubprogno		!index of main subprog (eg. may be before/after syslib)

!global const int maxtype=6'000
global const int maxtype=16'000

global int ntypes

global [0..maxtype]symbol		ttnamedef
global [0..maxtype]symbol		ttowner			!for ttlowerexpr/rtlengthexpr

global [0..maxtype]i32		ttbasetype		!basetype
global [0..maxtype]ichar		ttname

global [0..maxtype]u32		ttsize
global [0..maxtype]byte			ttsizeset
global [0..maxtype]i32		ttlower 		!.lbound (default 1)
global [0..maxtype]i32		ttlength 		!elements in array/record/tuple
global [0..maxtype]ref[]i32	ttmult 			!ttlength elements in tuple

global [0..maxtype]unit			ttdimexpr		!length, lower:length, or lower..upper

global [0..maxtype]i32		tttarget 		!for array/ref types
global [0..maxtype]byte			ttusercat
global [0..maxtype]i32		ttlineno

global [0..maxtype]byte			ttsigned		!is i8 i16 i32 i64
global [0..maxtype]byte			ttisreal		!is r32 r64
global [0..maxtype]byte			ttisinteger		!is i8..i64/u8..u64/c8..c64
global [0..maxtype]byte			ttisshort		!is i8/i16/i32/u8/u16/u32/c8/c16
global [0..maxtype]byte			ttisref			!is a pointer

global [0..maxtype]byte			ttisblock		!is a variant

!global const int maxtypename=4'000
!global const int maxtypename=8'000
global const int maxtypename=38'000

global [0..maxtypename]typenamerec typenames
global [0..maxtypename]posrec typenamepos
global int ntypenames

global [0..symbolnames.upb]byte typestarterset

global symbol currproc

global int headermode=0

global ref procrec proclist,proclistx			!linked list of all procs
global ref procrec staticlist,staticlistx		!linked list of all static
global ref procrec constlist,constlistx		!linked list of all export consts

global unit nullunit

global const maxdllproc=1000

global int ndllproctable
global [maxdllproc]symbol dllproctable

global int fverbose=1		!1=normal, 0=less verbose, 2/3 = more verbose

global byte msyslevel=2		!0/1/2 = none/min/normal
global byte mvarlib=0		!0/1 = none/yes
global byte fvarnames=0		!display of names in asm/mcl

global byte fshowtiming
global byte fshowss
global byte fshowc
global byte fshowtcl
global byte fshowasm
global byte fshowast1
global byte fshowast2
global byte fshowast3
global byte fshowst
global byte fshowpst
global byte fshowstflat
global byte fshowtypes
global byte fshowmodules
global byte fshowdiags			!1 means any of the above set

global byte fcheckunusedlocals=0

global byte highmem=1			!enable rip by default
global byte clinux				!1 when clang_pass targeting linux

global byte dointlibs=fsyslibs

!passlevel used for compiler debug only
global int passlevel=0
global int libmode=0					!1 means eventual ML/LIB target
global int fshortnames					!mcl/asm display

global ichar outfile					!one of the following two
global ichar destfilename				!nil, or override outfile
global ichar destfilepath				!nil, or set path of outfile

global int nunits
global int nunitsmem

!global const langhomedir	= "C:/mx/"
global const langhomedir	= "C:/bx/"
!global const langhomedir	= "C:/bx2/"

global const langhelpfile	= "mm_help.txt"

!GLOBAL INT NALLCALLS
!GLOBAL INT NUSESTACK
!GLOBAL INT NUSEMIXEDSTACK

!GLOBAL INT NGENINT
!GLOBAL INT NGENSMALLINT

!GLOBAL INT NALLEXPR
!GLOBAL INT NFASTEXPR
!
=== mm_diags.m 0 0 29/40 ===
int currlineno
int currfileno

strbuffer sbuffer
ref strbuffer dest=&sbuffer

const tab1="\t"
const tab2="\t\t"

!const fshowsymbols=1
const fshowsymbols=0

global proc printst(filehandle f, ref strec p, int level=0)=
	ref strec q

	printstrec(f, p, level)

	q:=p.deflist

	while q<>nil do
		printst(f, q, level+1)
		q:=q.nextdef
	od
end

proc printstrec(filehandle f, ref strec p, int level)=
	strec dd
	ref byte q
	strbuffer v
	ref strbuffer d:=&v
	int col, offset, n
	const tabstr="    "
	[256]char str

	gs_init(d)

	print @str, p
	gs_str(d, str)
	gs_str(d, " ")

	offset:=0
	to level do
		gs_str(d, tabstr)
		offset+:=4
	od
	gs_str(d, ":")

	gs_leftstr(d, p.name, 28-offset, '-')
	gs_leftstr(d, namenames[p.nameid], 12, '.')

	col:=gs_getcol(d)
	dd:=p^


	gs_str(d, "[")
	if p.isimport then
		gs_str(d, "Imp ")
	else
		gs_str(d, SCOPENAMES[P.SCOPE])
		gs_str(d, " ")
	fi

	if dd.isstatic then
		gs_str(d, "Stat")
	fi

	if dd.nameid=paramid and dd.parammode then
		gs_str(d, parammodenames[dd.parammode])
	fi

	if dd.align then
		gs_str(d, "@@")
		gs_strint(d, dd.align)
		gs_str(d, " maxalign:")
		gs_strint(d, dd.maxalign)
		gs_str(d, " ")
	fi
	if dd.optional then
		gs_str(d, "Opt ")
	fi
	if dd.varparams then
		gs_str(d, "Var:")
		gs_strint(d, dd.varparams)
		gs_str(d, " ")
	fi

	if dd.moduleno then
		if dd.nameid<>subprogid then
			print @&.str, "Modno#",,dd.moduleno
		else
			print @&.str, "Subno#",,dd.subprogno
		fi
		gs_str(d, &.str)
	fi

	if dd.used then
		gs_str(d, "U ")
	fi

	if dd.isthreaded then
		gs_str(d, "Threaded ")
	fi


	gs_str(d, "]")
	gs_padto(d, col+10, '=')

	if p.owner then
		fprint @&.str, "(#)", p.owner.name
		gs_leftstr(d, &.str, 18, '-')
	else
		gs_leftstr(d, "()", 18, '-')
	fi

	case p.mode
	when tvoid then
		gs_str(d, "Void ")
	else
		GS_STRINT(D, P.MODE)
		GS_STR(D, ":")

		gs_str(d, strmode(p.mode))
		gs_str(d, " ")
	esac

	case p.nameid
	when fieldid, paramid then
		gs_str(d, " Offset:")
		gs_strint(d, p.offset)
		if p.mode=tbitfield then
			gs_str(d, " Bitoffset:")
			gs_strint(d, p.bitoffset)
			gs_str(d, ":")
			gs_strint(d, p.bitfieldwidth)
		fi

		sprintf(&.str, "%.*s", int(p.uflags.ulength), &p.uflags.codes)
		print @&.str, p.uflags.ulength:"v", ichar(&p.uflags.codes):".*"
		gs_str(d, " UFLAGS:")
		gs_str(d, &.str)
		gs_str(d, "-")
		gs_strint(d, p.uflags.ulength)

		if p.code then
			gs_str(d, "/:=")
			gs_strvar(d, strexpr(p.code))
		fi

		if p.nameid=paramid and p.variadic then
			gs_str(d, "...")
		fi
	when procid then

		gs_str(d, "Index:")
		gs_strint(d, p.index)

		gs_str(d, " Nret:")
		gs_strint(d, p.nretvalues)

	when dllprocid then
		gs_str(d, "Index/PCaddr:")
		gs_strint(d, p.index)
		if p.truename then
			gs_str(d, " Truename:")
			gs_str(d, p.truename)
		fi

	when staticid then
		if p.code then
			gs_str(d, "=")
			gs_strvar(d, strexpr(p.code))
		fi

	when frameid then
		if p.code then
			gs_str(d, ":=")
			gs_strvar(d, strexpr(p.code))
		fi

	when constid then
		gs_str(d, "Const:")
		gs_strvar(d, strexpr(p.code))

	when typeid then
		if p.baseclass then
			gs_str(d, "Baseclass:")
			GS_STR(D, "<HAS BASECLASS>")
		fi
!	when enumid then
!		gs_str(d, "Enum:")
!		gs_strint(d, p.index)
!	when dllmoduleid then
!		gs_str(d, "DLL#:")
!		gs_strint(d, p.dllindex)
	esac

	if p.atfield then
		gs_str(d, " @")
		gs_str(d, p.equivfield.name)
		gs_str(d, " +")
		gs_strint(d, p.equivoffset)
	fi
	if p.atvar then
		gs_strvar(d, strexpr(p.equivvar))
	fi

!gs_str(d, " Module# ")
!gs_strint(d, p.moduleno)
!
	gs_str(d, " Lineno: ???")
!gs_strint(d, p.lineno iand 16777215)

	gs_println(d, f)

	case p.nameid
	when constid, frameid, staticid, macroid then
		if p.code then
			printunit(p.code, dev:f)
		fi
	esac
end

global proc printstflat(filehandle f)=
symbol p
println @f, "GLOBAL SYMBOL TABLE:"

for i:=0 to hashtable.upb-1 do
	p:=hashtable[i]
	if p=nil then nextloop fi

!	IF P.NEXTDUPL=NIL THEN NEXTLOOP FI

	case p.symbol
	when namesym then
		println @f, i:"5", p, p.name, symbolnames[p.symbol],,":",,namenames[p.nameid]
		p:=p.nextdupl
		while p do
			print @f, "     ", p, p.name, symbolnames[p.symbol],,":",,namenames[p.nameid]
			if p.owner then
				fprint @f, " (From #:#)", p.owner.name, namenames[p.owner.nameid]
			fi

			println @f

			p:=p.nextdupl
		od
	esac
od
end

global proc printcode(filehandle f, ichar caption)=
ref strec p
ref procrec pp

pp:=proclist

while pp do
	p:=pp.def

	print @f, p.name,,"=", (p.scope|"Sub", "Prog", "Exp"|"Mod")
	if p.owner.nameid=typeid then
		print @f, " in record", p.owner.name
	fi
	println @f
	printunit(p.code, 0, 1, dev:f)
	println @f
	pp:=pp.nextproc
od
end

global proc printunit(ref unitrec p, int level=0, number=0, filehandle dev=nil)=
!p is a tagrec
	ref unitrec q
	ref strec d
	int t
	ichar idname
	i64 a
	r32 x32
	static int cmpchain=0

	if p=nil then
		return
	fi

	if p.pos then
		currlineno:=getlineno(p.pos)
		currfileno:=p.fileno
	fi

!	print @dev, p, ":"
	print @dev, getprefix(level, number, p)

	idname:=jtagnames[p.tag]
	print @dev, idname,,": "

	case p.tag
	when jname then
		d:=p.def

		print @dev, d.name, namenames[d.nameid]

		if d.code then
			print @dev, " {",,jtagnames[d.code.tag],,"}"
		fi

		print @dev, " ",,getdottedname(d)!, q
		print @dev, (p.dottedname|" {Dotted}"|"")

		if p.c then
			print @dev, " Lastcall:", p.c
		fi

		if p.addroffirst then
			print @dev, " Addroffirst."
		fi

		print @dev, " Moduleno:", p.moduleno

!		if p.avcode then print @dev, " AV:", char(p.avcode) fi
		if p.avcode then print @dev, " AV:", char(p.avcode), $ fi

	PRINT @DEV, =P.INDEX


	when jlabeldef then
		println @dev, p.def.name

	when jconst then
		t:=p.mode
		a:=p.value
		if t=trefchar then
			if p.slength>256 then
				print @dev, """",,"(LONGSTR)", """ *",,p.slength
			elsif p.slength then
				print @dev, """",,p.svalue,,""" *",,p.slength
			else
				print @dev, """"""
			fi

		elsecase ttbasetype[t]
		when ti64, ti32, ti16, ti8 then print @dev, i64(a)
		when tu64, tu32, tu16, tu8 then print @dev, u64(a)
		when tc64, tc8 then print @dev, chr(a)

		when tr32, tr64 then
			print @dev, p.xvalue
		when tref then
			if p.value then
				print @dev, "#",,p.value, P.SLENGTH
			else
				print @dev, "NIL"
			fi
		when tbool then
			print @dev, (p.value|"True"|"False")
		when tarray then
			print @dev, "<ARRAY>", =P.STRTYPE, =P.SLENGTH
		else
			println =typename(t), typename(ttbasetype[t])
			PRINT @DEV, "<PRINTUNIT BAD CONST PROBABLY VOID"
		fi
		print @dev, " ",,typename(t)
		if p.isastring then
!			print @dev, " <isstr>"
			fprint @dev, " <isstr>(#)", p.strtype
		fi

		if p.whenlabel then
			print @dev, " *L",,p.whenlabel
		fi

	when jdecimal then
		print @dev, p.svalue, "Len:", p.slength

	when jtypeconst then
		print @dev, typename(p.mode), typename(p.value)

	when jbitfield then
		print @dev, bitfieldnames[p.bfcode]+3

	when jconvert, jtypepun then
		print @dev, " Convmode:", strmode(p.convmode)

	when jmakelist then
		print @dev, "Len:", p.length, " Makeax:", p.makearray

	when jdot then
		print @dev, "Offset:", p.offset

	when jindex, jptr then

	when jexit, jredo, jnext then
		print @dev, "#",,p.index

	when jsyscall then
		print @dev, sysfnnames[p.fnindex]+3

	when jmakeset then
	when jcmpchain then
		for i to p.cmpgenop.len do
			if p.cmpgenop[i]=0 then exit fi
			print @dev, ccnames[p.cmpgenop[i]],," "
		od
	esac

	if p.isconst then
		print @dev, " Is const"
	else
		print @dev, " Not const"
	fi

	case p.tag
	when jbin, jbinto, junary, junaryto, jincr, 
		jandl, jorl, jnotl, jistruel then
		if p.tclop then
			fprint @dev, " TCL<#>", tclnames[p.tclop]
		else
			fprint @dev, " no-op"
		fi
	when jprop then
		fprint @dev, " Prop<#>", propnames[p.propcode]
	when jconvert then
		fprint @dev, " Conv<#>", convnames[p.convcode]
	when jcmp then
		fprint @dev, " TCLcond<#>", ccnames[p.tclcond]
	esac


	println @dev

	for i to jsubs[p.tag] do
		printunitlist(dev, p.abc[i], level+1, i)
	od
end

proc printunitlist(filehandle dev, ref unitrec p, int level=0, number=0)=
	if p=nil then return fi

	while p do
		printunit(p, level, number, dev)
		p:=p.nextunit
	od
end

func getprefix(int level, number, ref unitrec p)ichar=
!combine any lineno info with indent string, return string to be output at start of a line
	static [1024]char str
	[1024]char indentstr
	[16384]char modestr
	ichar isexpr

	indentstr[1]:=0
	if level>10 then level:=10 fi

	to level do
		strcat(&.indentstr, "- ")
	od

	isexpr:="S"
	if jisexpr[p.tag] then isexpr:="E" fi

	case p.tag
	when jif, jswitch, jcase, jselect then
		if p.mode=tvoid then
			isexpr:="S"
		fi
	esac

	fprint @&.modestr, "# #:#", isexpr, (p.resultflag|"RES"|"---"), strmode(p.mode)
	modestr[256]:=0

	strcat(&.modestr, "-----------------------------")
	modestr[17]:=' '
	modestr[18]:=0

	strcpy(&.str, getlineinfok())
	strcat(&.str, &.modestr)
	strcat(&.str, &.indentstr)
	strcat(&.str, strint(number))
!	if prefix^ then
		strcat(&.str, " ")
!	fi

	return &.str
end

func getlineinfok:ichar=			!GETLINEINFO
	static [40]char str

	fprint @&.str, "# # ", CURRFILENO:"Z2", currlineno:"z4"
	return &.str
end

global proc printmodelist(filehandle f)=
	int mbase
	static ichar tab="\t"

!	PRINTLN @F, =NTYPENAMES
!	FOR I TO NTYPENAMES DO
!		PRINTLN @F, I, TYPENAMES[I].DEF.NAME
!	OD
!	PRINTLN @F
!
	println @f, "MODELIST", ntypes

	for m:=0 to ntypes do
		println @f, m:"4", strmode(m)
		mbase:=ttbasetype[m]

		println @f, tab, "Basetype:", mbase, strmode(mbase)
		println @f, tab, "ttname:", ttname[m]
		println @f, tab, "ttnamedef:", ttnamedef[m], (ttnamedef[m]|ttnamedef[m].name|"-")
		println @f, tab, "Target:", strmode(tttarget[m])
		println @f, tab, "Size:", ttsize[m], "Sizeset", ttsizeset[m]
		fprintln @f, "# Bounds: #..#  Length:#", tab, ttlower[m], ttlower[m]+ttlength[m]-1, ttlength[m]
		if mbase=ttuple then
			print @f, tab, "Mult:"
			for i to ttlength[m] do print @f, strmode(ttmult[m, i]),," " od
			println @f
		fi
		println @f, tab, "Signed:", ttsigned[m]
		println @f, tab, "Isreal:", ttisreal[m]
		println @f, tab, "Isinteger:", ttisinteger[m]
		println @f, tab, "Isshort:", ttisshort[m]
		println @f, tab, "Isref:", ttisref[m]
		println @f, tab, "Isblock:", ttisblock[m]
		println @f
	od
end

global proc showprojectinfo(filehandle dev)=
	imodule pm
	isubprog ps
	static ichar tab="    "
	ichar s
	byte isfirst, ismain

	println @dev, "Project Structure:"
	println @dev, "---------------------------------------"
	println @dev, "Modules", nmodules
	for i to nmodules do
		pm:=modules[i]

		if i>1 and pm.subprogno<>modules[i-1].subprogno then
			println @dev
		fi
		ps:=subprogs[moduletosub[i]]

			isfirst:=ps.firstmodule=i
			ismain:=ps.mainmodule=i

			if isfirst and ismain then s:="hm"
			elsif isfirst then s:="h "
			elsif ismain then s:="m "
			else s:="  " 
			fi

			print @dev, tab, i:"2", s, 
			pm.name:"16jl", "Sys:", pm.issyslib, 
			"Sub:", subprogs[pm.subprogno].name, "Fileno:", pm.fileno

		if pm.stmacro then
			print @dev, " Alias:", pm.stmacro.name
		fi
		if pm.stmain then
			print @dev, $, pm.stmain.name, ":", scopenames[pm.stmain.scope], pm.stmain
		fi
		if pm.ststart then
			print @dev, $, pm.ststart.name, ":", scopenames[pm.ststart.scope], pm.ststart
		fi

		println @dev
	od
	println @dev

	println @dev, "Subprograms", nsubprogs, =mainsubprogno
	for i to nsubprogs do
		ps:=subprogs[i]
		println @dev, tab, i, ps.name, "Sys:", ps.issyslib!, =PS.STSUBPROG

		if ps.firstmodule then
			print @dev, tab, tab
			for j:=ps.firstmodule to ps.lastmodule do
				print @dev, $, modules[j].name, "(", MODULES[J].STSUBPROG, ")"
			od
			println @dev
		fi
	od
	println @dev

	println @dev, "Sourcefiles", nsourcefiles
	ifile pf
	for i to nsourcefiles do
		pf:=sources[i]
		fprintln @dev, "  #:  Name=# File=# Path=# Spec=# Size=#", 
			i:"2", pf.name:"jl16", pf.filename:"jl18", pf.path:"20jl", pf.filespec:"30jl", pf.size:"7"
	od
	println @dev

	println @dev, "Link files", nlibfiles
	for i to nlibfiles do
		println @dev, tab, libfiles[i]:"16jl"
	od
	println @dev
end

global proc showlogfile=
	[256]char str
	filehandle logdev
	int size
	ref strbuffer ss

	return unless fshowdiags

	logdev:=fopen(logfile, "w")

	if fshowmodules then showprojectinfo(logdev) fi


	if fshowss and passlevel>=obj_pass then
		addtolog("SS", logdev)
	fi

	if fshowasm and passlevel>=mcl_pass then
		if ctarget then
			println @logdev, "PROC CLANG"
			addtolog(changeext(outfile, "c"), logdev)
		else
			println @logdev, "PROC ASSEMBLY"
			addtolog(changeext(outfile, "asm"), logdev)
		fi
	fi

	if fshowtcl and passlevel>=tcl_pass then
		addtolog(changeext(outfile, "tcl"), logdev)
	fi
!	if fshowc and dpasslevel>=dclang_pass then
!		addtolog(changeext(outfile, "c"), logdev)
!	fi
	if fshowpst and passlevel>=tcl_pass then
		addtolog("PSYMTAB", logdev)
	fi

	if fshowast3 and passlevel>=type_pass then addtolog("AST3", logdev) fi
	if fshowast2 and passlevel>=name_pass then addtolog("AST2", logdev) fi
	if fshowast1 and passlevel>=parse_pass then addtolog("AST1", logdev) fi

	if fshowst then
		showsttree("SYMBOL TABLE", logdev)
	fi
	if fshowstflat then
		showstflat("FLAT SYMBOL TABLE", logdev)
	fi
!
	if fshowtypes then
		printmodelist(logdev)
	fi

	size:=getfilesize(logdev)
	fclose(logdev)

	if size then
CPL "PRESS KEY..."; if OS_GETCH()=27 then stop fi
		print @&.str, "\\m\\ed.bat ", logfile

		if checkfile("mm.m") then
			os_execwait(&.str, 0, nil)
		else
			println "Diagnostic outputs written to", logfile
		fi
	fi
end

proc showstflat(ichar caption, filehandle f)=
	println @f, "PROC", caption
	printstflat(f)
	println @f
end

proc showsttree(ichar caption, filehandle f)=
	println @f, "PROC", caption
	printst(f, stprogram)
	println @f

	println @f, "Proc List:"
	ref procrec pp:=proclist
	while pp do
		symbol d:=pp.def
		fprintln @f, "#	#.# (#) Mod:", d, d.owner.name, d.name:"20jl", namenames[d.nameid], 
			d.moduleno
		pp:=pp.nextproc
	od
	println @f, "End\n"

	println @f, "DLL Proc List:"
	for i to ndllproctable do
		d:=dllproctable[i]
		fprintln @f, "#	#.# (#) Mod:", d, d.owner.name, d.name:"20jl", namenames[d.nameid], 
			d.moduleno
	od
	println @f, "End\n"
end

global proc showast(ichar filename)=
	filehandle f

	f:=fopen(filename, "w")
	return unless f

	println @f, "PROC", filename
	printcode(f, "")
	println @f
	fclose(f)
end

global proc printsymbol(ref tokenrec lp)=
	tokenrec l
	l:=lp^

	printf("%-18s", symbolnames[l.symbol])

	switch l.symbol
	when namesym then
		printstrn(l.symptr.name, l.symptr.namelen)

		if l.subcode then
			fprint " [#]", symbolnames[l.subcode]
		fi

	when intconstsym then
		case l.subcode
		when tint then print l.value, "int"
		when tword then print l.uvalue, "word"
		else print l.value
		esac

	when realconstsym then
		print l.xvalue

	when stringconstsym then
		print """"
		printstr(l.svalue)
		print """", strlen(l.svalue)

	when charconstsym then
		print "'"
		printstr(l.svalue)
		print "'"

	when assignsym, addrsym, ptrsym, rangesym, 
		andlsym, orlsym, eqsym, cmpsym, addsym, subsym, 
		mulsym, divsym, idivsym, iremsym, iandsym, iorsym, ixorsym, shlsym, shrsym, 
		minsym, maxsym, powersym then
		print symbolnames[l.symbol]
	elsif l.subcode then
		fprint "SUBCODE:", l.subcode
!	fprint "#", symbolnames[l.subcode]
	end

	println $, =lx.fileno

end

proc showtime(ichar caption, int t)=
	fprintln "# # ms # %", caption:"12jl", t:"5", (t*100.0)/compiletime:"5.1jr"
end

global proc showtimings=
	endclock:=os_clock()
	compiletime:=endclock-startclock
!
	showtime("Load:", 		loadtime)
	showtime("Parse:", 		parsetime)
	showtime("Resolve:", 	resolvetime)
	showtime("Type:", 		typetime)
	showtime("TCL:", 		tcltime)
	showtime("MCL:", 		mcltime)
	showtime("SS:", 			sstime)
	showtime("EXE:", 		exetime)
	println "-----------------------------"
	showtime("Total:", 		compiletime)
end

=== mm_export_dummy.m 0 0 30/40 ===
!hello

global proc writeexports(ichar basefile, modulename)=
end
=== mm_lex.m 0 0 31/40 ===
macro hashc(hsum,c)=hsum<<4-hsum+c
!macro hashw(hsum)=(hsum<<5-hsum)
macro hashw(hsum)=hsum

const maxstackdepth=20
[maxstackdepth]ref char lxstart_stack
[maxstackdepth]ref char lxsource_stack
[maxstackdepth]ref char lxsptr_stack
[maxstackdepth]int lxfileno_stack
[maxstackdepth]tokenrec lxnextlx_stack
[maxstackdepth]byte lximport_stack
global int sourcelevel=0
global int lximport

const cr	= 13
const lf	= 10
const tab	= 9

ref char lxsource
ref char lxstart
ref char lxsptr
int lxifcond

int lxfileno
global const hstsize	= 65536
!global const hstsize	= 65536*4
global const hstmask	= hstsize-1

global [0:hstsize]symbol hashtable
[0..255]byte namemap			!0/1/2 = other/name/name-upper

ichar u64maxstr="18446744073709551615"

global proc lex=
	int lena,lenb
	ref char p

	lx:=nextlx				!grab that already read basic token
	lx.sourceoffset:=lxstart-lxsource

	docase lexreadtoken(); nextlx.symbol
	when eolsym then
		if lx.symbol in [commasym, lsqsym, lbracksym] or
			symboloptypes[lx.symbol]=bin_op and 
			lx.symbol not in [maxsym, minsym] then
		else
			nextlx.symbol:=semisym
			nextlx.subcode:=1
			EXIT
		fi

	when kincludesym then
		doinclude()

	when namesym then
		case nextlx.subcode
		when unitnamesym then
			case lx.symbol
			when intconstsym then
				case nextlx.symptr.index
				when million_unit then lx.value *:= 1 million
				when billion_unit then lx.value *:= 1 billion
				else
					lxerror("Can't do this unit index")
				esac
				lx.subcode:=setinttype(lx.value)
			when realconstsym then
				lxerror("Unit suffix after float not implem")
			else
				nextlx.symbol:=namesym
				exit
			esac

		else
			nextlx.symbol:=namesym
			exit
		esac

	when rawxnamesym then
		nextlx.symbol:=namesym
		exit

	when insym then
		if lx.symbol=notlsym then
			lx.symbol:=notinsym
			lx.subcode:=1
		else
			exit
		fi

	else
		exit
	end docase

	nextlx.fileno:=lxfileno

end

global proc lexreadtoken=
!read next token into nextlx
	int c,hsum
	ref char sptr, lxsvalue
	int length,commentseen
	ref char p, q
	byte instr
	[256]char str

	nextlx.subcode:=0

	doswitch lxstart:=lxsptr; lxsptr++^
	when 'a'..'z','_','$' then
		lxsvalue:=lxsptr-1
	doname:
		hsum:=lxsvalue^

		sptr:=lxsptr

		docase namemap[c:=sptr++^]
		when 1 then
			hsum:=hsum<<4-hsum+c
		when 2 then
			(sptr-1)^:=c+' '
			hsum:=hsum<<4-hsum+c+' '
		else
			lxsptr:=sptr-1
			exit
		end docase

		if c='"' then
			if lxsvalue+1=ref char(lxsptr) then
				case c:=toupper(lxsvalue^)
				when  'F','R' then 
					readrawstring()
					return
				when  'S','B','A' then 
					readarraystring(c)
					return
				esac
			fi
		fi

		lookup(lxsvalue, lxsptr-lxsvalue, hashw(hsum))

		return

	when 'A'..'Z' then
		lxsvalue:=lxsptr-1
		lxsvalue^+:=32
		goto doname

	when '0'..'9' then
		lxstart:=lxsptr-1
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

	when '!' then			!comment to eol
docomment:
		docase c:=lxsptr++^
		when 13 then
			++lxsptr
			exit
		when 10 then
			exit
		when 0 then
			--lxsptr
			exit
		end
		nextlx.symbol:=eolsym
		return

	when '#' then
		nextlx.symbol:=hashsym
		return

	when '\\' then			!line continuation

!two stages:
! 1: read chars until any eol chars (unless further '\' seen)
! 2: read until non-white space
		commentseen:=0
		docase lxsptr++^			!read until end of this line
		when cr then
!			++nextlx.pos
			++lxsptr				!skip lf
			exit
		when lf then
!			++nextlx.pos
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
		end docase
!eol seen: now skip 0 or more further eol chars, plus any white space (ie. multiple blank lines)

		docase lxsptr++^
		when cr then
			++lxsptr				!skip lf
		when lf then
		when ' ',tab then
		else
			--lxsptr
			exit
		end docase

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
LXERROR(".123 not done")
!			readrealnumber(nil,0,10)
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
			nextlx.subcode:=jassign		!helps treat as opsym which all have k-code as subcode
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
!		if lxsptr^='|' then
!			++lxsptr
!			nextlx.symbol:=dbarsym
!		else
			nextlx.symbol:=barsym
!		fi
		return

	when '^' then
		nextlx.symbol:=ptrsym
		return

	when '@' then
!		if lxsptr^='@' then
!			++lxsptr
!			nextlx.symbol:=datsym
!		else
			nextlx.symbol:=atsym
!		fi
		return

	when '?' then
		p:=str; q:=lxsptr+1
		while q^ not in [cr, lf, 0] do
			p++^:=q++^
		od
		p^:=0

		nextlx.svalue:=pcm_copyheapstring(str)
		nextlx.symbol:=questionsym
		return


	when '~' then
!		nextlx.symbol:=curlsym
!		return

	when '+' then
		nextlx.symbol:=addsym
		if lxsptr^='+' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=kincrto
			return
		fi
		return

	when '-' then
		nextlx.symbol:=subsym
		case lxsptr^
		when '-' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=kdecrto
			return
		when '>' then
			++lxsptr
			nextlx.symbol:=pipesym
			return
		esac
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
		else
			nextlx.symbol:=eqsym
			nextlx.subcode:=eq_cc
		esac
		return

	when '<' then
		nextlx.symbol:=cmpsym
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.subcode:=le_cc
		when '>' then
			++lxsptr
			nextlx.subcode:=ne_cc
		when '<' then
			++lxsptr
			nextlx.symbol:=shlsym
		else
			nextlx.subcode:=lt_cc
		esac
		return

	when '>' then
		nextlx.symbol:=cmpsym
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=cmpsym
			nextlx.subcode:=ge_cc
		when '>' then
			++lxsptr
			nextlx.symbol:=shrsym
		else
			nextlx.symbol:=cmpsym
			nextlx.subcode:=gt_cc
		esac
		return

	when '&' then
		case lxsptr^
			when '&' then
			++lxsptr
			nextlx.symbol:=daddrsym
			nextlx.subcode:=jdaddrvv
		when '.' then
			++lxsptr
			nextlx.symbol:=anddotsym
			nextlx.subcode:=0
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
		nextlx.symbol:=eolsym
		return
	when lf then			!only lfs not preceded by cr
		nextlx.symbol:=eolsym
		return

	when 0 then
		if sourcelevel then
			unstacksource()
			RETURN
		else
			nextlx.symbol:=eofsym
			--lxsptr
			return
		fi

	else
		lxerror("Unknown char")
!		nextlx.symbol:=errorsym
		return

	end doswitch

end

global proc lexsetup=
!do one-time setup:
! clear the hash table and populated it with reserved words

	inithashtable()
end

global proc printstrn(ichar s, int length)=
	if length then
		print length:"v",s:".*"
	fi
end

proc readrawstring=
!positioned at " of F"
!read raw string
	ichar dest
	int c

	nextlx.symbol:=stringconstsym
	nextlx.svalue:=++lxsptr

	dest:=lxsptr				!form string into same buffer

	docase c:=lxsptr++^
	when '"' then
		if lxsptr^='"' then		!repeated, assume embedded term char
			dest++^:='"'
			++lxsptr
		else			!was end of string
!			(lxsptr-1)^:=0
			exit
		fi
	when cr,lf,0 then
		lxerror("Raw string not terminated")
		--lxsptr
		exit
	else
		dest++^:=c
	end docase
	nextlx.slength:=lxsptr-nextlx.svalue
	nextlx.svalue:=pcm_copyheapstringn(nextlx.svalue, nextlx.slength)
end

proc lookup(ref char name, int length, hashindex)=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!in either case, lx.symptr set to entry where name was found, or will be stored in
	int wrapped, j
	symbol d

	j:=hashindex iand hstmask

	d:=hashtable[j]
	wrapped:=0

	do
		if d=nil then exit fi

!		if (n:=d.namelen)=length and memcmp(d.name,name,n)=0 then	!match
		if d.namelen=length and memcmp(d.name,name,length)=0 then	!match
			nextlx.symptr:=d
			nextlx.symbol:=d.symbol
			nextlx.subcode:=d.subcode
			return
		fi

		if ++j>=hstsize then
			if wrapped then
				abortprogram("HASHTABLE FULL")
			fi
			wrapped:=1
			j:=0
		fi
		d:=hashtable[j]
	od

!exit when not found; new name will go in entry pointed to by lxsymptr

	d:=pcm_allocnfz(strec.bytes)

	hashtable[j]:=d

	d.name:=pcm_copyheapstringn(name,length)
	d.namelen:=length
	d.symbol:=namesym

	nextlx.symptr:=d
	nextlx.symbol:=d.symbol
!	nextlx.subcode:=d.subcode
end

func lookupsys(ref char name)int=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!return 1 (found) or 0 (not found)
!in either case, lx.symptr set to entry where name was found, or will be stored in
	int j, wrapped, hashvalue

	j:=gethashvaluez(name) iand hstmask

	lx.symptr:=hashtable[j]
	wrapped:=0

	do
		if lx.symptr=nil then
			exit
		elsif eqstring(lx.symptr.name,name) then	!match
			println "Lex dupl name:",name
			stop 1 
!			lxerror("sys dupl name?")
		fi

		if ++j>=hstsize then
			if wrapped then
				abortprogram("SYS:HASHTABLE FULL")
			fi
			wrapped:=1
			j:=0
		fi
		lx.symptr:=hashtable[j]
	od

!exit when not found; new name will go in entry pointed to by lxsymptr
	lx.symptr:=pcm_allocnfz(strec.bytes)
	hashtable[j]:=lx.symptr

	lx.symptr.name:=name				!assume can be shared (stored in a table)
	lx.symptr.namelen:=strlen(name)
	lx.symptr.symbol:=namesym			!usually replaced with actual symbol details

	return 0
end

func gethashvaluez(ichar s)int=
!get identical hash func to that calculated by lexreadtoken
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


	for i:=1 to stnames.len do
		lookupsys(stnames[i])

		lx.symptr.symbol:=stsymbols[i]

		case stsymbols[i]
		when unitnamesym then
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

!	for i:=0 to hstsize-1 do
!		if hashtable[i] then
!		fi
!	od
end

global proc addreservedword(ichar name,int symbol,subcode, regsize=0)=
	lookupsys(name)

	lx.symptr.symbol:=namesym
	lx.symptr.subcode:=symbol
	lx.symptr.index:=subcode

	lx.symptr.regsize:=regsize
end

proc doinclude=
	ichar file
	ifile pf

	lexreadtoken()
	if nextlx.symbol<>stringconstsym then lxerror("include: string expected") fi
	file:=nextlx.svalue
	convlcstring(file)
	file:=addext(file,"m")		!add in extension if not present; assume same as source

	pf:=getsupportfile(file, path:sources[lxfileno].path)
	lexreadtoken()
	stacksource(pf.fileno)
end

global proc startlex(ifile file)=
!start processing one of the file in sourcefile tables as source code
!assume it is a complete header or module

	lxsource:=lxsptr:=file.text

	nextlx.pos:=0
	lxfileno:=file.fileno

	nextlx.symbol:=semisym
	nextlx.subcode:=0
end

global func addnamestr(ichar name)ref strec=
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

global proc stacksource(int fileno, isimport=0)=
!introduce new source level for macros or include files
!not used for main source

	if sourcelevel>=maxstackdepth then
		lxerror("Include file/macro overflow")
	fi
	++sourcelevel
	lxstart_stack[sourcelevel]:=lxstart
	lxsource_stack[sourcelevel]:=lxsource
	lxsptr_stack[sourcelevel]:=lxsptr
	lxfileno_stack[sourcelevel]:=lxfileno
	lxnextlx_stack[sourcelevel]:=nextlx
	lximport_stack[sourcelevel]:=lximport
	lximport:=isimport

	lxsource:=lxsptr:=sources[fileno].text

	nextlx.pos:=0
	lxfileno:=fileno

	nextlx.symbol:=semisym
	nextlx.subcode:=0
end

global proc unstacksource=
	if sourcelevel>0 then			!check that some source is stacked
		lxstart:=lxstart_stack[sourcelevel]
		lxsource:=lxsource_stack[sourcelevel]
		lxsptr:=lxsptr_stack[sourcelevel]
		nextlx:=lxnextlx_stack[sourcelevel]
		lxfileno:=lxfileno_stack[sourcelevel]
		lximport:=lximport_stack[sourcelevel]

		--sourcelevel
	fi
end

proc readarraystring(int prefix)=
	++lxsptr
	lxreadstring('"')

	if prefix='S' then
		nextlx.subcode:='S'
!CPL "RAX/STR"
	else
		--NEXTLX.SLENGTH
		nextlx.subcode:='B'
!CPL "RAX/BIN"
	fi
end

func setinttype(u64 a)int=
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

	while namemap[c:=lxsptr++^] do
		hsum:=hsum<<4-hsum+c
	od
	--lxsptr

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
			nextlx.slength:=length+1
!CPL "LXREADSTRING", LENGTH, NEXTLX.SLENGTH
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

proc readdec=
	int c
	ref char dest, destend, pstart
	int islong, length
	[1024]char str
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
			nodecimal()

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

	if length>20 or length=20 and strncmp(str,u64maxstr,20)>0 then
		nodecimal()
	fi

finish:
	nextlx.symbol:=intconstsym
	nextlx.subcode:=setinttype(a)
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
			nodecimal()

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
		LXERROR("MAKEDEC")
		return
	fi

	nextlx.symbol:=intconstsym
	nextlx.subcode:=setinttype(a)
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
			nodecimal()

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
		nodecimal()
	fi

	nextlx.symbol:=intconstsym
	nextlx.subcode:=setinttype(a)
	nextlx.value:=a
end

proc readreal=
!at '.', or had been in middle of int where . or e were seen, back at the start

	int c,negexpon,dotseen,length, fractlen, expon, expseen
	real x
	[1024]char str
	ichar dest, destend
	u64 a

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
					nodecimal()
					return
				else
					--lxsptr
					exit all
				fi
			end

		when '_','\'' then

		when 'l','L' then
			nodecimal()
			return
		else
			--lxsptr
			exit
		fi

		if dest>=destend then lxerror("r64lit too long") fi
	end
	dest^:=0

	if expseen and expon>=0 and not dotseen then		!read as integer
		a:=0
		for i to length do				!digits already range checked
			a:=a*10+str[i]-'0'
		od
		to expon do
			a:=a*10
		od
		nextlx.symbol:=intconstsym
		nextlx.subcode:=setinttype(a)
		nextlx.value:=a
		return
	fi


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

!IF EXPSEEN AND NOT DOTSEEN THEN
!	CPL "READREAL NO DOT", X
!FI
end

proc nodecimal=
	lxerror("Decimal not ready")
end

proc start=
	for c in namemap.bounds do
		if c in 'a'..'z' or c in '0'..'9' or c in ['_','$'] then
			namemap[c]:=1
		elsif c in 'A'..'Z' then
			namemap[c]:=2				!upper case
		fi
	od
end

=== mm_lib.m 0 0 32/40 ===
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

ref strbuffer jdest

global ichar framevarname			!normally nil, set to frame var def to display in comment

global macro isnum(m)  = m <= tlastnum
global macro isnumx(m) = m <= tlastnum
global macro isnumf(m) = m <= tr32
global macro isnumi(m) = (m in [ti64, tu64, tc64])
global macro isbool(m) = (m in [tbool8, tbool64])

global macro isint(m) = m>tr32

global func newstrec:symbol=
	symbol p

!	p:=pcm_alloc(strec.bytes)
!	clear p^

	p:=pcm_allocnfz(strec.bytes)

	p.pos:=lx.pos
	p.moduleno:=currmoduleno
	p.subprogno:=moduletosub[currmoduleno]
	return p
end

global func getduplnameptr(symbol owner,symptr,int id)symbol=
!create duplicate stentry
!owner is the owner ST
!symptr points to the current generic entry for the name (nameid=0)
!id is the desired nameid
!new entry is created, and added to the dupl chain for this name
!return pointer to new strec; this can be stored directly in a -def unit
!but such nameptrs are not allowed elsewhere; it must be wrapped in a knameunit
	symbol p

	p:=newstrec()

	p.name:=symptr.name
	p.namelen:=symptr.namelen
	p.symbol:=namesym
	p.owner:=owner
	p.nameid:=id

!IF ID NOT IN [FRAMEID, PARAMID] THEN
	p.nextdupl:=symptr.nextdupl
	symptr.nextdupl:=p
!FI
	p.firstdupl:=symptr

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

global func createname(symbol p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=jname
	u.def:=p

	return u
end

global func createunit0(int tag)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=tag
	return u
end

global func createunit1(int tag, ref unitrec p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=tag
	u.a:=p
	return u
end

global func createunit2(int tag, ref unitrec p,q)ref unitrec=
	ref unitrec u

	u:=allocunitrec()

	u.tag:=tag
	u.a:=p
	u.b:=q
	return u
end

global func createunit3(int tag, ref unitrec p,q,r)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=tag
	u.a:=p
	u.b:=q
	u.c:=r
	return u
end

global proc insertunit(unit p,int tag)=
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

global func createconstunit(u64 a, int t)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=jconst
	u.value:=a
	u.mode:=t

	u.isconst:=1
	return u
end

global func createstringconstunit(ichar s, int length)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=jconst
	u.svalue:=s
	u.mode:=trefchar
	u.isastring:=1

	if length=-1 then
		u.slength:=strlen(s)+1
	else
		u.slength:=length
	fi
	return u
end

global func newtypename(symbol a,b)int=
	if ntypenames>=maxtypename then
		serror("Too many type names")
	fi
	++ntypenames
	typenames[ntypenames].defa:=a		!leave .owner/.pmode to be filled in
	typenames[ntypenames].defb:=b		!used type's mode is used

	typenamepos[ntypenames].pos:=lx.pos

	return -ntypenames
end

global func createusertype(symbol stname)int=
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

global func createusertypefromstr(ichar name)int=
!create new, named user type
	symbol stname

	stname:=getduplnameptr(stmodule,addnamestr(name),typeid)
	return createusertype(stname)
end

global func getrangelwbunit(ref unitrec p)ref unitrec=
	if p.tag=jmakerange then
		return p.a
	else
		p:=createunit1(jprop,p)
		p.propcode:=kklwb
		return p
	fi
end

global func getrangeupbunit(ref unitrec p)ref unitrec=
	if p.tag=jmakerange then
		return p.b
	else
		p:=createunit1(jprop,p)
		p.propcode:=kkupb
		return p
	fi
end

global func createarraymode(symbol owner,int target,unit dimexpr, int typedefx)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
	int k,m

	if typedefx=0 then		!anon type
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=tarray
	ttlower[m]:=1
	ttdimexpr[m]:=dimexpr
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner

	ttisblock[m]:=1

	return m
end

func sameunit(unit p,q, symbol powner=nil, qowner=nil)int=
!p are q are units just parses; no name resolving or type checking
!do a simple check to see if they are the same unit
	if p=q then return 1 fi
	if p=nil or q=nil then return 0 fi

	if p.tag<>q.tag then return 0 fi

	case p.tag
	when jconst then
		return p.value=q.value
	when jmakerange,jkeyvalue then
		return sameunit(p.a, q.a) and sameunit(p.b, q.b)
	when jname then
		if p.def=q.def and powner=qowner then
			return 1
		fi
	esac

	return 0

end

global func createarraymodek(symbol owner,int target,int lower,length, int typedefx)int=
!lower is lower bound of array
	int atype,m

	atype:=tarray

	if typedefx=0 then		!anon type
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
	ttisblock[m]:=1

	return m
end

global func nextautotype:ichar=
	static [32]char str

	print @&.str,"$T",,++autotypeno
	return &.str
end

global func createslicemode(symbol owner,int slicetype,target,unit dimexpr, int typedefx=0)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
	int m

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
	ttisblock[m]:=1

	return m
end

global func createslicemodek(symbol owner,int target,lower, int typedefx=0)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
	int m

	if typedefx=0 then		!anon type
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=tslice
	ttlower[m]:=lower
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner
	ttisblock[m]:=1

	return m
end

global func createrefmode(symbol owner,int target,typedefx=0)int=
	int k,m
!	int a,b

	if typedefx=0 then		!anon type
		for k:=tlast to ntypes when ttisref[k] do
			if tttarget[k]=target then
				return k
			fi
		od
!		FI
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

global func createrefprocmode(symbol owner,stproc, paramlist,int kwd, prettype,typedefx)int=
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
	ttsigned[dest]		:= ttsigned[source]
	ttisreal[dest]		:= ttisreal[source]
	ttisinteger[dest]	:= ttisinteger[source]
	ttisshort[dest]		:= ttisshort[source]
	ttisref[dest]		:= ttisref[source]
	ttisblock[dest]		:= ttisblock[source]
end

global func getdottedname(symbol p)ichar=
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

global func getavname(symbol owner,int id=frameid)symbol=
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
		print @&.str,"sv_",,++nextsvindex
	fi

	name:=pcm_copyheapstring(&.str)
	addnamestr(name)

	p:=getduplnameptr(owner,addnamestr(name),id)
	p.used:=1

	p.mode:=tint

	adddef(owner,p)
	return p
end

global proc unionstr_clear(ref uflagsrec u)=
	((ref u64(u))^:=0)		!clear flags and length togetjer
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

global func unionstr_last(ref uflagsrec u)int=
	if u.ulength then
		return u.codes[u.ulength]
	fi
	return 0 
end

global proc unionstr_copy(ref uflagsrec u,v)=
	memcpy(u,v,uflagsrec.bytes)
end

global func createrecordmode(symbol owner,int typedefx)int=
!typedef is nil, or an empty moderec belonging to a user type
!owner is an strec for the name def:
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
	ttisblock[m]:=1

	return m
end

global func createtuplemode(symbol owner,[]int &elements,int elementslen, typedefx)int=
	int m

	if typedefx=0 then
		m:=createusertype(owner)
	else
		m:=typedefx
	fi
	ttbasetype[m]:=ttuple
	ttusercat[m]:=1
	ttlength[m]:=elementslen
	ttmult[m]:=pcm_alloc(elementslen*i32.bytes)
	for i to elementslen do
		storemode(owner,elements[i],ttmult[m,i])
	od

	return m
end

global func strexpr(ref unitrec p)ref strbuffer=
!vx_makestring("",exprstr)
	gs_init(exprstr)

	jevalx2(exprstr,p)
	return exprstr
end

global proc jevalx2(ref strbuffer dest, ref unitrec p)=			!JEVAL
	jdest:=dest
	jevalx(p)
end

global proc jevalx(ref unitrec p)=			!JEVAL
!p represents an expression. It can be a unitrec only, not a list (lists only occur inside
!kmakelist and kmakeset units, which specially dealt with here)
!dest is a destination string. Special routines such as gs_additem() are used, which take care
!of separators so that successive alphanumeric items don't touch
	unit q,a,b
	[500]char str
	int length

	if p=nil then
		return
	fi

	a:=p.a
	b:=p.b

	case p.tag
	when jconst then

		case ttbasetype[p.mode]
		when ti32,ti64,ti8,ti16 then
			getstrint(p.value,&.str)
		when tu32,tu64,tu8,tu16 then
			strcpy(&.str,strword(p.uvalue))
		when tc8,tc64 then
			str[1]:=p.uvalue
			str[0]:=0

		when treal,tr32 then
			print @&.str,p.xvalue
		when tref then
			if p.mode=trefchar and p.isastring then
				if p.slength>str.len/4 then
					strcpy(&.str,"LONGSTR)")
				else
					convertstring(p.svalue, &.str, p.slength)
				fi
				jadditem("""")
				jadditem(&.str)
				jadditem("""")
				return
			else
				print @&.str,ref void(p.value)
			fi
		else
			strcpy(&.STR,"<EVAL/CONST PROBABLY VOID>")
		esac
		jadditem(&.str)

	when jname then
		jadditem(p.def.name)

	when jbin,jcmp then

		strcpy(&.str,tclnames[p.tclop])
		jadditem("(")
		jevalx(a)
		jadditem(&.str)
		jevalx(b)
		jadditem(")")

	when junary, jistruel, jnotl then

		strcpy(&.str,tclnames[p.tclop])
		jadditem(&.str)
		jadditem("(")

		if a.tag=jtypeconst then
			jadditem(STRMODE(a.value))
		else
			jevalx(a)
		fi
		jadditem(")")

	when jprop then

		strcpy(&.str,propnames[p.propcode])
		jadditem(&.str)
		jadditem("(")
		jevalx(a)
		jadditem(")")

	when jcall then
		jevalx(a)
		jadditem("(")

		q:=b
		while q do
			jevalx(q)
			q:=q.nextunit
			if q then jadditem(",") fi
		od
		jadditem(")")

	when jindex,jdotindex,jslice,jdotslice then
		jevalx(a)
		if p.tag=jdotindex or p.tag=jdotslice then
			jadditem(".")
		fi
		jadditem("[")
		jevalx(b)
		jadditem("]")

	when jdot then
		jevalx(a)
		jadditem(".")
		jevalx(b)

	when jmakelist then
		jadditem("(")

		q:=a
		while q do
			jevalx(q)
			q:=q.nextunit
			if q then jadditem(",") fi
		od
		jadditem(")")

	when jmakerange then
		jadditem("(")
		jevalx(a)
		jadditem("..")
		jevalx(b)
		jadditem(")")

	when jassign then
		jevalx(a)
		jadditem(":=")
		jevalx(b)

	when jif then
		jadditem("(")
		jevalx(a)
		jadditem("|")
		jevalx(b)
		jadditem("|")
		jevalx(p.c)
		jadditem(")")

	when jtypeconst then
		jadditem(strmode(p.mode))

	when jconvert,jtypepun then

		jadditem(strmode(p.convmode))
		if p.tag=jtypepun then
			jadditem("@")
		fi
		jadditem("(")
		jevalx(a)
		jadditem(")")

	when jshorten then

		jadditem("shorten(")
		jevalx(a)
		jadditem(")")
	when jautocast then

		jadditem("cast(")
		jevalx(a)
		jadditem(")")
	when jkeyvalue then
		jevalx(a)
		jadditem(":")
		if b then
			jevalx(p.b)
		else
			jaddstr("-")
		fi

	when jptr then
		jevalx(a)
		jadditem("^")

	when jblock then
		jadditem("<JBLOCK>")

	when jnull then
		jaddstr("<nullunit>")

	when jaddrof then
		jadditem("&")
		jevalx(a)
		if b then
			jaddstr("+")
			gs_strint(jdest,b.value)
		fi

	when jaddroffirst then
		jadditem("&.")
		jevalx(a)

	when jtypestr then
		jadditem("TYPESTR(")
		jevalx(a)
		jadditem(")")

!	when jcvlineno, jcvfilename, jcvmodulename then
	when jcvfilename, jcvmodulename then
		jaddstr("$")
		jaddstr(jtagnames[p.tag]+1)

	when jbitfield then
		jevalx(a)
		jaddstr(".")
		jaddstr(bitfieldnames[p.bitopindex])

	when jfmtitem then
		jevalx(a)
		jaddstr(":")
		jevalx(b)

	when jsyscall then
		jaddstr(sysfnnames[p.fnindex]+3)
		jaddstr("(")
		if a then jevalx(a) fi
		jaddstr(")")
	when jincr then
		jaddstr("incr ")
		jevalx(a)
	when jstrinclude then
		jaddstr("strinclude ")
		jevalx(a)

	else
		CPL jtagnames[p.tag]
		gerror("CAN'T DO JEVAL",p)
	end
end

proc jadditem(ichar s)=
	gs_additem(jdest,s)
end

proc jaddstr(ichar s)=
	gs_str(jdest,s)
end

global func strmode(int m,expand=1)ichar=
	static [4096]char str
	istrmode(m,expand,&.str)
	return &.str
end

global func strmode2(int m,expand=1)ichar=
	static [4096]char str
	istrmode(m,expand,&.str)
	return &.str
end

global proc istrmode(int m,expand=1,ichar dest)=
	symbol d,q
	int needcomma,i,target,mbase,n
	strbuffer sxx
	ref strbuffer xx:=&sxx
	ref strbuffer sdim
	[100]char strdim
	ichar prefix
	typenamerec tn

	if m<0 then
		strcpy(dest,"*")
		tn:=typenames[-m]

!		if tn.defb=nil then			!assume typeof
!			strcat(dest,"typeof(")
!			strcat(dest,tn.defa.name)
!			strcat(dest,")")
!	    else
			if tn.defa then
				strcat(dest,tn.defa.name)
				strcat(dest,".")
			fi
			strcat(dest,tn.def.name)
!		fi
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
			fprint @dest,"@[#<#>",&.strdim,M
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

	pp:=pcm_allocnfz(procrec.bytes)

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
!	pp:=pcm_alloc(procrec.bytes)
	pp:=pcm_allocnfz(procrec.bytes)

	if staticlist=nil then
		staticlist:=staticlistx:=pp
	else
		staticlistx.nextproc:=pp
		staticlistx:=pp
	fi

	pp.def:=d
end

global proc addexpconst(symbol d)=
	ref procrec pp
	pp:=pcm_allocnfz(procrec.bytes)

	if constlist=nil then
		constlist:=constlistx:=pp
	else
		constlistx.nextproc:=pp
		constlistx:=pp
	fi
	pp.def:=d
end

global func typename(int m)ichar=
	if m>=0 then
		return ttname[m]
	fi
	return typenames[-m].def.name

end

global func allocunitrec:ref unitrec=
	ref unitrec p

	++nunits
	nunitsmem+:=unitrec.bytes

	if remainingunits-- then
		p:=unitheapptr
		++unitheapptr
		p.pos:=lx.pos
		p.moduleno:=currmoduleno
		p.subprogno:=moduletosub[currmoduleno]
		return p
	fi

!need first or new heap
	p:=unitheapptr:=pcm_alloc(unitheapsize*unitrec.bytes)

	memset(p,0,unitheapsize*unitrec.bytes)
	remainingunits:=unitheapsize-1
	++unitheapptr
	p.pos:=lx.pos

	p.moduleno:=currmoduleno
	p.subprogno:=moduletosub[currmoduleno]

	return p
end

global func createdupldef(symbol owner,symptr, int id)symbol=
!create new proc entry
!symptr is the generic st entry for proc's name
	symbol p

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

global func createnewmoduledef(symbol owner,symptr, int id=moduleid)symbol=
	return createdupldef(owner,symptr,id)
end

global func duplunit(unit p,int lineno=0)unit=
	unit q
	if p=nil then return nil fi

	q:=createunit0(p.tag)

	q^:=p^
	q.nextunit:=nil
	for i to jsubs[q.tag] do
		q.abc[i]:=duplunit(q.abc[i])
	od

	return q
end

global func isconstunit(unit a)int=
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

global func getalignment(int m)int=
!return alignment needed for type m, as 1,2,4,8
	int a

	case ttbasetype[m]
	when tarray then
		return getalignment(tttarget[m])
	when trecord then
		a:=ttnamedef[m].maxalign
		if a=0 then
CPL "GAL0"
 a:=8 fi
		return a
	elsif ttisblock[m] then
		return 8
	esac

	a:=ttsize[m]
	case a
	when 1,2,4,8 then
		return a
	when 0 then
		return 8
	esac
	cpl Strmode(m)
	gerror("GETALIGN SIZE NOT 1248")

	return 0
end

global proc addlistunit(unit &ulist,&ulistx,unit p)=
!add unit p to unit structure ulist,^ulistx  which can be null
	if ulist=nil then		!first
		ulist:=ulistx:=p
	else
		ulistx.nextunit:=p
	fi
	ulistx:=p			!update end-of-list pointer
end

global func storemode(symbol owner, int m, i32 &pmode)int =
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

global func gettypebase(int m)int=
	case ttbasetype[m]
	when ti8,ti16,ti32 then ti64
	when tu8,tu16,tu32 then ti64

	when tr32 then tr64

	when tc8 then tc64
	else
		m
	esac
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

	if f=nil then
		CPL "ATL ERROR",FILENAME; return fi

	do
		c:=fgetc(f)
		exit when c=c_eof
		fputc(c,logdest)
	od
	fclose(f)
end

global func getprocretmodes(unit p)symbol=
!p must be a call unit, for a proc with multiple values; at least one expected
!however, here it only populates retmodes with the available types
	unit a

	if p.tag<>jcall then txerror("multass/need multfn") fi
	a:=p.a

	case a.tag
	when jname then
		return a.def
	else
		return ttnamedef[tttarget[a.mode]]
	esac
end

global func gettclmode(int t)int u=
	u:=stdtcl[ttbasetype[t]]

	if u=tpblock then
		case ttsize[t]
		when 8 then u:=tpu64
		when 4 then u:=tpu32
		when 2 then u:=tpu16
		when 1 then u:=tpu8
		esac
	fi
	return u
end

=== mm_libsources_dummy.m 0 0 33/40 ===
global const fsyslibs = 0

global proc loadbuiltins=
end
=== mm_modules.m 0 0 34/40 ===
ichar fileext="m"

global func loadsp(ichar filename, int mainsub=0)isubprog sp=
!source = nil:  load lead module and dependencies from given sourcefile
!source <> nil: source code is given directly. filename can give a name
! to that source text, or if nil, and internal name is applied

	const maxmods=250
	const maxsubs=250
	[maxmods]ichar modnames
	[maxmods]symbol aliases
	[maxmods]ichar paths
	[maxsubs]ichar subnames
	[maxsubs]ichar subpaths
	int nmods:=0, nsubs:=0, hdrcode
	int firstmod, lastmod, issyslib:=0
	imodule pm
	symbol d, stalias
	ichar path, name, ext, file2
	byte proj:=0, sepheader:=0

!CPL "LOADSP", =EXTRACTBASEFILE(FILENAME), =SYSLIBNAME

	if eqstring(extractbasefile(filename), syslibname) then
		issyslib:=1
	fi

	ext:=extractext(filename)
	if not eqstring(ext, "m") then fileext:=pcm_copyheapstring(ext) fi

	pm:=loadmodule(filename, issyslib)

	if pm=nil then
		loaderror("Can't load lead module: ", filename)
	fi
	path:=pm.file.path

	for i to nsubprogs do
		if eqstring(pm.name, subprogs[i].name) then
			loaderror("Subprog already loaded: ", sp.name)
		fi
	od

!reader header info
	startlex(pm.file)
	lex()
	skipsemi()

	if lx.symbol=kprojectsym then
		proj:=1
		lexchecksymbol(eqsym)
		lex()
	fi

	do
		skipsemi()
		case lx.symbol
		when kheadersym then
			hdrcode:=lx.subcode
			lex()
			case hdrcode
			when hdr_module then
				checksymbol(namesym)
				name:=lx.symptr.name

				if not eqstring(name, pm.name) then
					if nmods>=maxmods then loaderror("Too many modules in header") fi
					modnames[++nmods]:=name
					paths[nmods]:=path
					aliases[nmods]:=nil

				fi
				if nextlx.symbol=namesym and eqstring(nextlx.symptr.name,"as") then
					lex()
					lex()
					if lx.symbol=namesym then
						stalias:=lx.symptr
						lex()
					else
						checksymbol(stringconstsym)
						stalias:=addnamestr(lx.svalue)
					fi
					aliases[nmods]:=stalias
				fi

			when hdr_import then
				checksymbol(namesym)
				if nsubs>=maxsubs then loaderror("Too many imports in header") fi
				subnames[++nsubs]:=lx.symptr.name
				subpaths[nsubs]:=path

			when hdr_linkdll then
				checksymbol(namesym)
				addlib(lx.symptr.name)

			when hdr_sourcepath then
				checksymbol(stringconstsym)
				unless loadedfromma then			!ignore paths for .ma
					path:=pcm_copyheapstring(lx.svalue)
				end
!CPL "SET PATH", PATH

			else
				loaderror("Hdr cmd not ready")
			esac
			lex()

		when semisym then
		else
			exit
		esac
	od

	if proj then
		checkend(kendsym, kprojectsym)
	fi
	skipsemi()
	if lx.symbol=eofsym then
		sepheader:=1
	fi

!process nested imports
	for i to nsubs do
		if eqstring(subnames[i],pm.name) then loaderror("Importing self") fi
!		loadsp(getmodulefilename(path, subnames[i]))
		loadsp(getmodulefilename(subpaths[i], subnames[i]))
	od

!create new subprog entry
	if nsubprogs>=maxsubprog then loaderror("Too many subprogs") fi
	sp:=pcm_allocz(subprogrec.bytes)
	subprogs[++nsubprogs]:=sp
	sp.subprogno:=nsubprogs

	if mainsub then
!		loadsyslib()
		mainsubprogno:=nsubprogs
	fi

	firstmod:=nmodules+1
	lastmod:=firstmod+nmods
	if lastmod>maxmodule then loaderror("Too many modules") fi
	nmodules:=lastmod
	pm.subprogno:=nsubprogs
	pm.islead:=1
	pm.moduleno:=firstmod
	pm.stmodule:=d:=createdupldef(stprogram,addnamestr(pm.name),moduleid)
	d.moduleno:=firstmod
	d.subprogno:=nsubprogs

	moduletosub[firstmod]:=nsubprogs

	sp.name:=pm.name
	sp.firstmodule:=firstmod

	sp.mainmodule:=0

	sp.lastmodule:=lastmod
	sp.issyslib:=issyslib

!create new set of modules[] entries and load those other modules
!create stmodule entries for each module
	modules[firstmod]:=pm

	for i to nmods do
		pm:=loadmodule(getmodulefilename(paths[i], modnames[i], issyslib), issyslib)
		stalias:=aliases[i]
		if not pm then
			loaderror("Can't load: ",modnames[i])
		fi
		modules[firstmod+i]:=pm
		pm.stmodule:=d:=createdupldef(stprogram,addnamestr(pm.name),moduleid)
		pm.subprogno:=nsubprogs
		
		if stalias then
			pm.stmacro:=getduplnameptr(stprogram, stalias, macroid)
			adddef(stprogram, pm.stmacro)
			pm.stmacro.paramlist:=nil
			pm.stmacro.code:=createname(d)
		fi

		d.moduleno:=pm.moduleno:=firstmod+i
		d.subprogno:=nsubprogs
		moduletosub[d.moduleno]:=nsubprogs

		for j to nmodules when eqstring(modules[i].name, pm.name) do
			serror_s("Dupl mod name:", pm.name)
		od
	od

	return sp
end

global func loadmodule(ichar filespec, int issyslib=0)imodule pm=
	ifile pf

!CPL "LOADMOD",FILESPEC, =ISSYSLIB
	pf:=loadsourcefile(filespec, issyslib)
	return nil when pf=nil

	pm:=pcm_allocz(modulerec.bytes)

	pm.name:=pf.name
	pm.file:=pf
	pm.fileno:=pf.fileno
	pm.issyslib:=issyslib

	return pm
end

global func loadsourcefile(ichar filespec, int issyslib=0)ifile pf=
	ichar s,filename
	[300]char str

	filename:=extractfile(filespec)

!CPL "LSF1", =FILENAME, =NSOURCEFILES, =ISSYSLIB

!look for file already loaded, or preloaded due to built-in syslib or .ma file:
	for i to nsourcefiles do
!CPL "LOOP", I, FILENAME, SOURCES[I].FILENAME, SOURCES[I].ISSYSLIB
		if eqstring(filename, sources[i].filename) and sources[i].issyslib=issyslib then
			return sources[i]
		fi
	od

	pf:=newsourcefile()

	pf.filespec:=pcm_copyheapstring(filespec)
	pf.path:=pcm_copyheapstring(extractpath(filespec))
	pf.name:=pcm_copyheapstring(extractbasefile(filespec))
	pf.filename:=pcm_copyheapstring(filename)
	pf.issyslib:=issyslib
	pf.fileno:=nsourcefiles
!CPL "LSF", FILESPEC


	s:=cast(readfile(filespec))			!will overallocate by a few bytes
	if not s then				!unexpected error
		return nil
	fi
	pf.text:=s
	pf.size:=rfsize

	if passlevel=ma_pass then
		pf.dupl:=pcm_copyheapstring(s)
	fi

	(s+rfsize)^:=0				!replace etx,0 by 0,0 (effectively, just zero)
	return pf
end

func getmodulefilename(ichar path, name, int issyslib=0)ichar =
	static [300]char str

!need to sort out search path etc

	strcpy(str, path)
	strcat(str, name)
	strcat(str, ".")
	strcat(str, (issyslib|"m"|fileext))
	return str
end

global proc addlib(ichar libname)=
	for i to nlibfiles do
		if eqstring(libfiles[i],libname) then return fi
	od
	if nlibfiles>=maxlibfile then
		loaderror("Too many libs")
	fi
	libfiles[++nlibfiles]:=libname
end

proc loadsyslib=
	[300]char str
	ichar name
	byte fruntcl:=passlevel=runtcl_pass
	byte fgentcl:=passlevel=tcl_pass
!	byte flinux:=clinux or not os_iswindows()

	if dointlibs then				!bundled sys files
		str[1]:=0
	else
		strcpy(str, langhomedir)
	fi

	case msyslevel
	when 0 then
		return
	when 1 then
		name:=(ctarget or not os_iswindows()|"msysminc"|"msysmin")
	else				!full syslib
!CPL "FULLSYS"
		if os_iswindows() and not clinux then	!run on Windows
!CPL "WINDOWS", =CTARGET, FRUNTCL,=FGENTCL
			if ctarget then
				name:="msyswinc"
			elsif fruntcl or fgentcl then		!avoid modules with assem
				name:="msyswini"
			else
!				name:="msyswin"
				NAME:="MSYSWINI"				!TEMPORARY FOR TCL TARGET
			fi
		else									!on Linux, or generating C for Linux on Windows
!CPL "LINUX"
			name:="msyslinc"
		fi
	esac

	strcat(str, name)

	SYSLIBNAME:=PCM_COPYHEAPSTRING(STR)
!IF FVERBOSE>=2 THEN
!	CPL =SYSLIBNAME
!FI
	strcat(str, ".m")


!CPL "SYSLIB:",STR

	loadsp(str)
end

global proc loadproject(ichar file)=
	[300]char str
	ichar file2

	int tt:=clock()
	if dointlibs then
		loadbuiltins()
	fi

	loadsyslib()

!try .ma version of .m not present
	if not checkfile(file) then
		file2:=pcm_copyheapstring(changeext(file,"ma"))
		if checkfile(file2) then file:=file2 fi
	fi

	if eqstring(extractext(file),"ma") then
CPL "LOADING FROM MA FILE"
		loadmafile(file)
		loadedfromma:=1
		strcpy(str, changeext(file,"m"))			!assume lead module has same name as ma file
		file:=&.str
	fi

	loadsp(file, 1)

	addlib("msvcrt")
	if os_iswindows() then
		addlib("user32")
		addlib("gdi32")
		addlib("kernel32")
	end

	loadtime:=clock()-tt
end

func readfileline(ichar s)ichar =
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

func findnextlineheader(ichar s)ichar=
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

proc loadmafile(ichar filespec, ichar builtinstr=nil)=
!load ma file from disk
!unless filespec is nil, then direct from builtinstr
!return name of lead module
	ichar s,t
	[100]char name
	int sys,support
	ifile pf

	if filespec then
		s:=cast(readfile(filespec))
		if s=nil then							!file not found on disk
			loaderror("Can't find MA file ",filespec)
		fi
	else
		s:=builtinstr
	fi

!need to scan file pickuping the file headers, and populating sourctables

	s:=readfileline(s+3)
	readstr(name,'n')
	if not eqstring(name,"ma") then
		loaderror("MA: bad header")
	fi

	--s					!point to previous lf

	s:=findnextlineheader(s)

	do
		if s=nil then
			loaderror("Unexpected EOF in MA file")
			exit
		fi
		s:=readfileline(s)

		readstr(name,'n')
		read sys,support

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

		pf:=newsourcefile()

		pf.filename:=pf.filespec:=pcm_copyheapstring(name)
		pf.name:=pcm_copyheapstring(extractbasefile(name))
		pf.size:=t-s-3
		pf.text:=s
		pf.path:=pf.filespec:=""
		pf.issyslib:=sys
		pf.issupport:=support
		s:=t
	od
!
	for i to nsourcefiles do
		pf:=sources[i]
		(pf.text+pf.size)^:=0
	od
end


=== mm_name.m 0 0 35/40 ===
symbol currstproc
int allowmodname=0
int noexpand
int macrolevels

const maxmacroparams=50
[maxmacroparams]symbol macroparams
[maxmacroparams]symbol macroparamsgen
[maxmacroparams]unit macroargs
int nmacroparams
int nmacroargs

global proc rx_typetable=
	symbol d
!	symbol currproc

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
	int n,oldnoexpand,oldtag,useparams

	a:=p.a
	b:=p.b
	mmpos:=p.pos

	switch p.tag
	when jname then
		resolvename(owner,p)
		if P.TAG=JNAME AND p.def.nameid=macroid and not noexpand then
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
		oldtag:=p.tag

		if a.tag=jname then			!can expand possible macro if params not ready
			oldnoexpand:=noexpand; noexpand:=1
			rx_unit(owner,a)
			noexpand:=oldnoexpand
		else
			rx_unit(owner,a)
		fi

		rx_unitlist(owner,b)

		if a.tag=jname then
			d:=a.def
			case d.nameid
			when typeid then		!change to type conversion
				p.tag:=jconvert
				storemode(owner,d.mode,p.convmode)
				p.a:=b
				if b.nextunit then
					p.a:=createunit1(jmakelist,b)
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

				if useparams and p.tag<>jcall then
					insertunit(p,oldtag)
					p.b:=b					!note b may be nil
				FI

			esac
		fi

	when jandl, jorl then
		rx_unit(owner,a)
		rx_unit(owner,b)
		if not isbooltag[a.tag] then insertunit(a,jistruel) fi
		if not isbooltag[b.tag] then insertunit(b,jistruel) fi

	when jistruel then
	doistruel:
		rx_unit(owner,a)

		if isbooltag[a.tag] then
			deleteunit(p,a)
		fi

	when jnotl then
		rx_unit(owner,a)
		case a.tag
		when jnotl then
			deleteunit(p,a)
			p.tag:=jistruel
			a:=p.a
			goto doistruel

		when jistruel then
			a.tag:=jisfalsel
			deleteunit(p,a)
			a:=p.a
		when jisfalsel then
			a.tag:=jistruel
			deleteunit(p,a)
			a:=p.a
		elsif not isbooltag[a.tag] then
			p.tag:=jisfalsel
			a:=p.a
		esac

	else
doabc:
		for i to jsubs[p.tag] do
			rx_unitlist(owner,p.abc[i])
		od
	end switch
end

global func rx_module(int n)int=
	currmoduleno:=n

	rx_passdef(stprogram,modules[n].stmodule)

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

global func resolvetopname(symbol owner,stnewname,int moduleno,allowmod)symbol =
!stnewname points to a symrec with generic nullid
!This is a top-level name (left-most name of any dotted sequence, or standalone name)

!Search through all the duplicate symrecs (all names with identical names have symrecs that
!are linked together, always starting with a nullid symrec) looking for the best match

!moduleno is the module where the currently generic name is encountered
!(derived from a unit if in an expression, or an STREC if a type in a declaration)

	int extcount, subprogno
	symbol p,q, powner,extdef,moddef
	[10]symbol ambiglist

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

		case powner.nameid
		when moduleid then							!candidate is file-scope item
			if powner.moduleno=moduleno then		!same module
				return p
			elsif p.scope then	!matches an external module
				if powner.subprogno=subprogno or		!within same subprog
					 p.scope=program_scope or
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
				if subprogno=moduletosub[p.moduleno] then
					moddef:=p
				else
					for i to nsubprogs do
						if eqstring(p.name, subprogs[i].name) then
							p.issubprog:=1				!in case not yet set
							moddef:=p
							exit
						fi
					od
				fi
			when macroid then
				return p

			esac

		esac
	od

	if allowmod and moddef then
		return moddef
	fi

	if extdef then
		if extcount>1 then
			if not eqstring(extdef.owner.name, "mclib") then
				for i:=1 to extcount do
					extdef:=ambiglist[i]
					println i,extdef.owner.name,namenames[extdef.owner.nameid]
				od
				if not eqstring(extdef.owner.name, "mclib") then
					rxerror_s("Ambiguous ext name: #",extdef.name)
				fi
			fi
		fi
		return extdef
	fi
	return nil
end

global proc resolvename(symbol owner, unit p)=
!p is a name tag inside given owner
!resolve name
!report error if unresolved, unless mode is not void. Then an unresolved
!name is added as a frame (assumes this is a proc)

	symbol d,e
	int moduleno, mode,islet

	d:=p.def
	moduleno:=p.moduleno

	if d.nameid<>nullid then			!assume already resolved
		return
	fi

	e:=resolvetopname(owner,d,moduleno,allowmodname)

	if not e then
		islet:=0
		mode:=tvoid
		case p.avcode
		when 'I', 'T', 'S' then mode:=ti64; islet:=1
		when 'L','A' then mode:=tany
		esac

		if mode=tvoid then
			[300]CHAR STR
			STRCPY(STR, D.NAME)
			CONVUCSTRING(STR)
			rxerror_s("tcl:Undefined: #",STR,p)
		else
			e:=addframevar(owner,d,moduleno,mode)
			e.pos:=p.pos
			e.islet:=islet
		fi
	fi

!	if e.used<255 then ++e.used fi
	e.used:=1

	p.def:=e
end

global func finddupl(symbol d, pdupl)symbol=
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

global func finddupl_sub(symbol d, pdupl)symbol=
!version of finddupl where d is a subprog
	int subprogno

	if pdupl.nameid<>nullid then		!assume already resolved
		return pdupl
	fi
	pdupl:=pdupl.nextdupl
	subprogno:=d.subprogno

	while pdupl do
		if pdupl.owner.subprogno=subprogno then
			return pdupl
		fi
		pdupl:=pdupl.nextdupl
	od
	return nil
end

proc resolvedot(symbol owner,unit p)=
	unit lhs,rhs
	symbol d,e,t
	int m,moduleno,subprogno,oldallowmod

	moduleno:=p.moduleno
	subprogno:=p.subprogno
	lhs:=p.a
	rhs:=p.b
	e:=rhs.def				!p.b will be a name type (could perhaps be stored as p.def)

	oldallowmod:=allowmodname
	allowmodname:=lhs.tag=jname
	rx_unit(owner,lhs)
	allowmodname:=oldallowmod
	d:=lhs.def

	case lhs.tag
	when jname then
		case d.nameid
		when moduleid, typeid, procid, typeid then

			if d.nameid=moduleid and d.subprogno<>subprogno then
				dosubprogid
			fi

			e:=finddupl(d,e)

			if e then
				if d.nameid=moduleid then
					if e.subprogno<>subprogno then
						if e.scope<program_scope AND NOT E.ISIMPORT then
							rxerror_s("Need export to import '#'",e.name)
						fi
					elsif e.moduleno<>moduleno then
						if not e.scope then
							rxerror_s("Need global to import '#'",e.name)
						fi
					fi
				fi
domodule:
				p.tag:=jname			!convert to dot to name
				p.a:=p.b:=nil
				p.def:=e
				case e.nameid
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
						rxerror("2:Record expected")
					esac
				od
			else
				rxerror("Record expected")
			esac
			t:=ttnamedef[m]

			e:=finddupl(t,e)
			if e then
				p.b.def:=e
			else
				rxerror_s("Not a field: #",rhs.def.name)
			fi
		when subprogid then
dosubprogid:
			e:=finddupl_sub(d,e)
			if e then
				if e.subprogno<>subprogno then
					if e.scope<program_scope AND NOT E.ISIMPORT then
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
		end unless
	esac
end

proc fixmode(ref typenamerec p)=
!p refers to a negative mode that is a typename index
!fix that up if possible
	ref i32 pmode
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

	fi

	if e and e.nameid=typeid then
		pmode^:=e.mode

	else
		rxerror_s("2:Can't resolve tentative type: #",d.name)
	fi
end

global proc fixusertypes=
	ref typenamerec p
	int npasses,notresolved
	symbol d

	npasses:=0
	repeat
		++npasses
		notresolved:=0

		for i to ntypenames do
			p:=&typenames[i]

			if p.pmode^<0 then
				mmpos:=typenamepos[i].pos
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

func addframevar(symbol owner, d, int moduleno, mode)symbol=
!owner should be a proc; d is a generic st entry
!add framewith the name of d and given mode to the proc
	symbol e
	e:=getduplnameptr(owner,d,frameid)
	storemode(owner,mode,e.mode)
	adddef(owner,e)
	return e
end

func copylistunit(unit p)unit=
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

func copyunit(unit p)unit=
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
	for i to jsubs[q.tag] do
		q.abc[i]:=copylistunit(q.abc[i])
	od

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

	pm:=d.paramlist
	nmacroparams:=0
	while pm do
		if nmacroparams>=maxmacroparams then
			rxerror("macro param overflow")
		fi
		macroparams[++nmacroparams]:=pm
		macroparamsgen[nmacroparams]:=pm.firstdupl

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
=== mm_parse.m 0 0 36/40 ===
!M Language Parserxxx

int intabledata=0		!1 means reading table data line; $ gives tabledataname
int inreadprint=0
int inparamlist=0
int inrecordbody=0
int inimportmodule=0
int labelseen=0
ichar tabledataname=nil

const maxprocstack=10
[maxprocstack]symbol procstack
int nprocstack=0

uflagsrec unionstring, unionpend
symbol unionlastvar=nil
symbol dretvar			!part of read-proc: nil, or symptr of retval variable

int varattribs=0

const maxdollarstack=10
[maxdollarstack]unit dollarstack		!used for a[$]
int ndollar=0
int insiderecord=0
int insidedllimport=0

const maxforloops=10
[maxforloops]symbol forindexvars
int nforloops

global func parsemodule(imodule pm)int=
	symbol owner

	initparser()

	currmoduleno:=pm.moduleno

	stmodule:=pm.stmodule

	currproc:=stmodule

	startlex(pm.file)
	owner:=stmodule

	lex()
!

!CPL "PARSE", PM.NAME


!!=========================================
!int t:=os_clock()
!int ntokens:=0
!CPL "******************** LEX TEST ****************"
!
!!	repeat
!!		lex()
!!		++ntokens
!!!PS("TOKEN")
!!	until lx.symbol=eofsym
!
!	repeat
!		lexreadtoken()
!!PSNEXT("HELLO")
!		++ntokens
!	until nextlx.symbol=eofsym
!
!!CPL =NMODULES
!
!t:=os_clock()-t
!
!CPL "LEX TIME=", t
!CPL =ntokens
!
!STOP
!!=========================================
!
	readmoduledefs(owner)
	return 1
end

global proc readmoduledefs(symbol owner) =
!first symbol has been read
	int globalflag

	globalflag:=module_scope

	do
		switch lx.symbol
		when kglobalsym then
			if globalflag then serror("global global?") fi
			globalflag:=lx.subcode

!			if globalflag=export_scope and stmodule.subprogno<>1 then
			if globalflag=export_scope and stmodule.subprogno<>nsubprogs then
				globalflag:=program_scope
			fi

			lex()

		when kprocsym, kfuncsym then	!todo
			readprocdef(owner, globalflag)
			globalflag:=module_scope

		when stdtypesym, krefsym, kicharsym, lsqsym, kslicesym then
dovar:
			readvardef(owner, globalflag, 0, staticid, 0)
			globalflag:=module_scope

		when kletsym then
			lex()
			readvardef(owner, globalflag, 0, staticid, kletsym)
			globalflag:=module_scope

		when kimportmodulesym then
			readimportmodule(owner)

		when ktypesym then
			readtypedef(owner, globalflag)
			globalflag:=module_scope

		when kconstsym then
			readconstdef(owner, globalflag)
			globalflag:=module_scope

		when krecordsym then
			readclassdef(owner, globalflag)
			globalflag:=module_scope

		when ktabledatasym then
			readtabledef(owner, globalflag)
			globalflag:=module_scope

		when semisym then
			lex()

		when eofsym then
			exit

		when kmacrosym then
			readmacrodef(owner, globalflag)
			globalflag:=module_scope

		when kprojectsym then
			repeat
				lex()
			until lx.symbol in [kendsym, eofsym]
			checkend(kendsym, kprojectsym)

		when kheadersym then
			repeat
				lex()
			until lx.symbol=semisym

		when dotsym then
			SERROR("MODULE/DOT")
		when namesym then
			if istypestarter() then
				goto dovar
			fi
			goto doexec

		else
doexec:
		serror("Code outside a func")
		end switch
	od
end

proc initparser=

	unless nullunit then
		nullunit:=createunit0(jnull)
	end unless

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

global func makeblock(unit p)unit=
	if p and p.tag=jblock then return p fi
	return createunit1(jblock, p)
end

proc checkequals=
!check that "=" is current symbol
	if lx.symbol<>eqsym then
		serror("""="" expected")
	fi
end

func getcurrline:int=
	return lx.pos
end

func checkbegin(int fbrack)int=
!look for ( or [ or begin, return close symbol expected
!positioned at this opening symbol
!fbrack=1 to allow left "("
	int closesym

	skipsemi()

	if lx.symbol=lbracksym and fbrack then
		closesym:=rbracksym
		lex()
	else
		closesym:=kendsym
	fi
	return closesym
end

proc checkbeginend(int closesym, kwd, startline=0)=
!look for ) or ] or end [kwd] depending on closesym
!positioned at this symbol; exit at following symbol
	skipsemi()
!	if closesym=rbracksym or closesym=rcurlysym then
	if closesym=rbracksym then
		checksymbollex(closesym)
!		lex()
	else
		checkend(closesym, kwd, startline:startline)
	fi
end

global proc checkend(int endsym, endkwd1, endkwd2=0, startline=0)=
!at terminator symbol such as ), eof or 'end'
!check it matches what is expected
!endsym is symbol expected to match
!'end' can have optional keyword following; if present, it must match endkeyword
!Some loop ends (indicated by endkeyword=kforsym, etc) can be also be terminated with 'od'
!endsym should be lbracksym or kendsym
	[100]char str

	skipsemi()

!exit pointing to current symbol (to 'end', keyword after 'end', or ')')
	if endsym=lx.symbol=rbracksym then
		return
	fi

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
!	elsif lx.symbol<>semisym then
!		error
	fi
end

func readvardef(symbol owner, int scope=0, isstatic=0, varid=staticid, k)unit=
!positioned at symbol following 'mut' or 'let', which will at the first symbol of
!the type, or at the first name being defined if there is no type
!k is the keyword symbol used (let/mut), or set to 0 if no keyword has been used, 
!then mut is assumed

!read vars inside module or proc
!isglobal must be 0 for procs
!isstatic must be 1 for modules
!varid must be frameid[let]/staticid[let] for procs, otherwise staticid[let]

	unit ulist, ulistx, p
	int nvars, m, initcode
	symbol stname

	ulist:=ulistx:=nil

	if istypestarter() then
		m:=readtypespec(owner)
	else
		serror("Readvar?")
	fi

	nvars:=0
	while lx.symbol=namesym do

		++nvars
		stname:=getduplnameptr(owner, lx.symptr, varid)

		stname.scope:=scope

		stname.isstatic:=isstatic

		stname.islet:=(k=kletsym)
		if varid=dllvarid then
			stname.isimport:=1
		fi

		adddef(owner, stname)
		if varid=staticid and owner.nameid<>procid then
!CPL "MM/ADDSTATIC", STNAME.NAME, OWNER.NAME
			addstatic(stname)
		fi

		lex()

		storemode(owner, m, stname.mode)

		if lx.symbol in [assignsym, eqsym] then

!			initcode:=case lx.symbol when eqsym then 1 when assignsym then 2 else 3 esac
			case lx.symbol
			when eqsym then initcode:=1
			when assignsym then initcode:=2
			else initcode:=3
			esac
			stname.used:=1

			if lx.symbol<>eqsym then
				if varid=staticid then
					serror("Non-variants can't use :=")
					if owner.nameid=procid then
						serror("Can't use := for statics inside procs")
					fi
					
				fi
			else
				if varid=frameid then
					serror("Need 'static' for '='")
					addstatic(stname)
				fi
			fi
			lex()

			stname.code:=readunit()

			stname.equals:=initcode
			if varid=frameid then
				p:=createunit2(jassign, createname(stname), stname.code)
				p.initlet:=1
				addlistunit(ulist, ulistx, p)
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

proc readconstdef(symbol owner, int scope=0)=
!at 'const' symbol
	int nconsts, deft, m
	symbol stname

	lex()

	nconsts:=0

	if istypestarter() then
		deft:=readtypespec(owner)
	else
		deft:=tauto
	fi

	while lx.symbol=namesym do
		stname:=getduplnameptr(owner, lx.symptr, constid)

		lex()

		checkequals()
		lex()
		stname.code:=readconstexpr(1)

		m:=deft

		storemode(owner, m, stname.mode)
		++nconsts

		stname.scope:=scope

		adddef(owner, stname)
		if scope=export_scope and stname.name^<>'$' then
			addexpconst(stname)
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

func readlbrack:unit=
!positioned at "("
!termsym is rbracksym
!read one of the following:
! (x)		simple expression
! ()		list with no elements
! (x, )		list with one element
! (x, x, ...)		list
! (x|x|x])		if then else fi
! (x|x, ... |x])	select then else end

!return positioned at symbol following closing ")"
!listtag is jmakelist or jmakearray if 'array' was used

	unit ulist, ulistx, p, q, r, plower
	int oldirp, length, usecomma

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
		checksymbollex(colonsym)
!		lex()

	elsif lx.symbol=intconstsym and nextlx.symbol=colonsym then
		plower:=createconstunit(lx.value, lx.subcode)
!		plower.istrueconst:=1
		lex()
		lex()

	elsif symboloptypes[lx.symbol]=bin_op and nextlx.symbol=rbracksym then	!operator constant
		p:=createunit0(joperator)
		p.opcindex:=lx.subcode
		lex()
		lex()
		return p
	elsif symboloptypes[lx.symbol]=bin_op and nextlx.symbol=assignsym then	!operator:= constant
		p:=createunit0(joperator)
		p.tclop:=symbolgentoops[lx.symbol]
		lex()			!read :=
		lexchecksymbol(rbracksym)
		lex()
		return p
	fi

!check symbol after "("
	case lx.symbol
	when rbracksym then			!empty list
		lex()
		p:=createunit0(jmakelist)
		p.b:=plower
		p.length:=0
		return p
	else					!assume normal expression follows
		p:=readunit()
	esac

!check symbol after "(expr"
	case lx.symbol
	when rbracksym then			!simple (x) expression
		lex()

		return p

	when commasym then			!separate by comma or implicit newline
		usecomma:=1
		if nextlx.symbol=rbracksym then		!means one-element list
			lex()
			lex()
			p:=createunit1(jmakelist, p)
			p.length:=1
			p.b:=plower
			return p
		fi
docomma:						!entry from implicit newline
		length:=1

!must be regular list
		ulist:=ulistx:=p

		if usecomma then
			repeat
				lex()							!skip comma
				if lx.symbol=rbracksym then		!allow , ) to end list
					exit
				fi
				if lx.symbol=commasym then
					serror(", , null expr not allowed")
				fi
				addlistunit(ulist, ulistx, readunit())
				++length
				skipsemi()
			until lx.symbol<>commasym
		else

			repeat
				skipsemi()
				if lx.symbol=rbracksym then		!allow , ) to end list
					exit
				fi
				if lx.symbol=commasym then
					serror(", , null expr not allowed")
				fi
				addlistunit(ulist, ulistx, readunit())
				++length
			until lx.symbol<>semisym
		fi

		checksymbollex(rbracksym)
!		lex()
		p:=createunit1(jmakelist, ulist)
		p.length:=length
		p.b:=plower
		return p

	when barsym then			!ifx/selectx expression; p is selector expression
		lex()
		q:=readunit()
		case lx.symbol
		when barsym then		!(a|b|c)
			lex()
			r:=readsunit()
			checksymbollex(rbracksym)
!			lex()
			p:=createunit3(jif, fixcond(p), q, r)
			p.compactif:=1
			return p
		when rbracksym then
			lex()
			p:=createunit3(jif, fixcond(p), q, nil)
			p.compactif:=1
			return p
		esac

!assume selectx expression
		addlistunit(ulist, ulistx, q)	!start with one-element list
		checksymbol(commasym)
		if nextlx.symbol<>barsym then		!(n|a, | using one-element list; not useful but allow it...
			repeat
				lex()				!skip comma
				addlistunit(ulist, ulistx, readunit())
			until lx.symbol<>commasym
			checksymbol(barsym)
		else
			lex()					!skip |
		fi
		lex()
		r:=readunit()
		checksymbollex(rbracksym)
!		lex()
		return createunit3(jselect, p, ulist, r)

	when semisym then
		if lx.subcode=1 then
			usecomma:=0
			goto docomma
		fi
		ulist:=ulistx:=p
		repeat
			skipsemi()
			if lx.symbol=rbracksym then
				exit
			fi
			addlistunit(ulist, ulistx, readunit())
!			skipsemi()						!allow a, b, c;) (works better with a, b, c\ followed by comment on next line followed by ")")
		until lx.symbol<>semisym
		checksymbollex(rbracksym)
!		lex()

		return makeblock(ulist)


	else
		serror("(x ...")
	esac
	return nil
end

proc addlistparam(ref symbol ulist, ulistx, symbol p)=
!add unit p to unit structure ulist, ^ulistx  which can be null
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx^.nextparam:=p
	fi
	ulistx^:=p			!update end-of-list pointer
end

func readcast:unit=
!also reads standalone type value
!t<>tvoid means already has ty[e
	unit p
	int opc, t

	t:=readtypespec(currproc)

	case lx.symbol
	when rbracksym then
		p:=createunit0(jtypeconst)
		p.mode:=ttype
		p.value:=t
		return p

	when atsym then
		opc:=jtypepun
		lex()
	when dotsym then			!allow T.type, but also just T (followed by . which
								!might be T.min etc)
		if nextlx.symbol=ktypesym then
			lex()
			p:=createunit0(jtypeconst)
			p.value:=t
			p.mode:=ttype
			lex()
		else					!leave dot to be processed by caller
			p:=createunit0(jtypeconst)
			p.value:=t
		fi
		return p
	else
		opc:=jconvert
	esac

	checksymbollex(lbracksym)
!	lex()
	p:=readunit()
	checksymbollex(rbracksym)
!	lex()

	p:=createunit1(opc, p)
	storemode(currproc, t, p.convmode)
	return p
end

func readopc:unit=
!op sym seen just before a term
	unit p, q, r
	int tag, opc, firstsym

	firstsym:=lx.symbol

	case lx.symbol
	when mathsopsym then
		tag:=junary
		opc:=lx.subcode
	when maths2opsym then
		tag:=jbin
		opc:=lx.subcode
	else
		tag:=junary
		opc:=symbolgenops[firstsym]
	esac

	lex()
	case firstsym
	when addsym then			!ignore +
		return readterm2()
	when subsym then			!convert minus to negate
		opc:=kneg
	when minsym, maxsym, maths2opsym, 
iandsym, iorsym, ixorsym then
		p:=readterm2()

		if p.tag=jmakelist then
			if p.length<>2 then serror("Needs (x, y)") fi
			q:=p.a
			r:=q.nextunit
			q.nextunit:=nil
			p:=createunit2(jbin, q, r)
			p.tclop:=opc
			return p
		else		!assume single tclopnd
			SERROR("READOPC/SINGLE OPND?")
			return createunit1(opc, p)

		fi
	else
		if symboloptypes[firstsym]=bin_op then
			serror("Can't be used as unary op")
		fi

	esac

	if lx.symbol=assignsym then	!op:=, not normally allowed inside expressions
		lex()
		tag:=junaryto
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

	p:=createunit1(tag, q:=readterm2())

	p.tclop:=opc

	if q.tag=jmakelist then
		serror("Too many opnds")
	fi

	return p
end

func readcompilervar:unit=
	[100]char str
	rsystemtime tm
	static []ichar monthnames=("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
	unit p
	imodule currmodule:=modules[currmoduleno]

	switch lx.subcode
	when jcvnil then
		p:=createconstunit(0, tref)
		lex()
		return p

	when jcvpi then
!		p:=createconstunit(i64@(3.14159'265358'979'3238'4626'433'832), treal)
		p:=createconstunit(i64@(pi), treal)
		lex()
		return p

	when jcvinfinity then
		p:=createconstunit(i64@(infinity), treal)
		lex()
		return p

	when jcvlineno then
!		tc_gen(kloadimm, getlineno(lx.pos)
		p:=createconstunit(getlineno(lx.pos), ti64)
!		p:=createunit0(jcvlineno)
		lex()
		return p

	when jcvstrlineno then
		getstrint(getlineno(lx.pos), &.str)

	when jcvmodulename then
		strcpy(str, stmodule.name)

	when jcvfilename then
		strcpy(str, sources[currmodule.fileno].filespec)

	when jcvfunc then
		strcpy(&.str, currproc.name)

	when jcvdate then
		os_getsystime(&tm)
		fprint @&.str, "#-#-#", tm.day, monthnames[tm.month], tm.year:"4"

	when jcvtime then
		os_getsystime(&tm)
		fprint @&.str, "#:#:#", tm.hour:"z2", tm.minute:"z2", tm.second:"z2"

	when jcvversion then
		strcpy(&.str, "Compiler:M6.4")

	when jcvtrue, jcvfalse then
		p:=createconstunit(lx.subcode=jcvtrue, tbool64)
		lex()
		return p
	
	else
		serror_s("compiler var not impl: #", jtagnames[lx.subcode])
	end switch
	lex()

	return createstringconstunit(pcm_copyheapstring(&.str), -1)
end

func readcastx:unit=
!explicit cast using syntax:
! cast(expr)
! cast(expr, type)
! cast@(expr, type)
!at 'cast'
	int opc, m
	unit p

	lex()
	opc:=jconvert
	if lx.symbol=atsym then
		opc:=jtypepun
		lex()
	fi
	checksymbollex(lbracksym)
!	lex()
	m:=tvoid
	p:=readunit()
	if lx.symbol<>commasym then
		if opc=jtypepun then serror("@ type missing") fi
		opc:=jautocast
	else
		lex()
		m:=readtypespec(currproc)
	fi
	checksymbollex(rbracksym)
!	lex()

	p:=createunit1(opc, p)
	storemode(currproc, m, p.convmode)

	return p
end

global proc checksymbol(int symbol)=
	[100]char str

	if lx.symbol<>symbol then
		fprint @&.str, "# expected, not #", symbolnames[symbol], symbolnames[lx.symbol]
		serror(&.str)
	fi
end

global proc lexchecksymbol(int symbol)=
	lex()
	checksymbol(symbol)
end

global proc checksymbollex(int symbol)=
	checksymbol(symbol)
	lex()
end

global func readtypespec(symbol owner, int typedefx=0)int=
!at initial symbol of a type, or where type is expected
!read simple type (which will have a single name) or a more elaborate type-spec
!returns a moderec handle
!typedefx is not a def, but either:
! moderec	Used when called from readtypedef. This is then filled in with the
!		details of the new mode, and the new version is returned
! nil		Called from outside readtypedef; then just returns a new moderec

!If the first symbol is not a stdtype, then it is assumed to be a usertype
!For stdtypes, I might implement :N and *N width-specifiers, as an alternative to just
!using i16 etc
	symbol d, e
	int t, kwd, sltype, w
	unit x, pupper, plx
	unit dim, length
	const maxdim=30
	[maxdim]unit dims
	int ndims, i, n, k

	case lx.symbol
	when lsqsym then		!array bounds
arraybounds:
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
				when rsqsym, commasym then			![n]
				when colonsym then				!a:n
					lex()
					if not (lx.symbol=commasym or lx.symbol=rsqsym) then	!lower:length
						length:=readunit()
						dim:=createunit2(jkeyvalue, dim, length)
					else													!lower:
						dim:=createunit1(jkeyvalue, dim)
					fi
				esac
			fi
			if ndims>=maxdim then serror("Too many array dims") fi
			dims[++ndims]:=dim
			exit when lx.symbol<>commasym
			lex()
		od
		inreadprint:=0
		checksymbollex(rsqsym)
!		lex()
		t:=readtypespec(owner)

		for i:=ndims downto 1 do
			t:=createarraymode(owner, t, dims[i], (i=1|typedefx|0))
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
			t:=newtypename(d, lx.symptr)
			lex()
		else
			t:=newtypename(nil, d)
		fi

	when krecordsym, kstructsym then
		serror("Use 'record name =' syntax")

	when kunionsym then
		serror("Top-level union not allowed")

	when krefsym then		!ref T
	retry:

		lex()
		if lx.symbol=ktosym then lex() fi

		case lx.symbol
		when kprocsym, kfuncsym then	!func pointer being created
			t:=readrefproc(owner, typedefx)

		when stdtypesym then
			case lx.subcode
			when tc8 then
				t:=trefchar
				if typedefx then tttarget[typedefx]:=tc8 fi
			else
				goto readtarget
			esac

			lex()

		when kvoidsym then
			lex()
			t:=tvoid
			gottarget
		else						!assume normal type
readtarget:
			t:=readtypespec(owner)
gottarget:
			t:=createrefmode(owner, t, typedefx)
		esac

	when kicharsym then
		if lx.subcode=tc8 then
			t:=trefchar
		else
			t:=tref
		fi
		if typedefx then tttarget[typedefx]:=lx.subcode fi
		lex()

	when kslicesym then
		t:=readslicetype(owner, lx.subcode, typedefx)

	else
		serror("Bad type starter")
	esac

	if typedefx then			!assume a simple alias
		ttbasetype[typedefx]:=ttbasetype[t]
	fi

	return t
end

func readslicetype(symbol owner, int slicetype, typedefx)int=
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
	t:=readtypespec(owner, typedefx)

	return createslicemode(owner, slicetype, t, plower, typedefx)
end

func readslist(int iscall=0, donulls)unit=
!read comma-separated list of expressions
!positioned at first symbol of first expression
! it might be | or )
!
!donulls=1 means empty expressions are allowed (just comma or terminator, which
!result in null units
!return with symbol at terminating symbol: 1st non comma and is that a unit starter
!iscall=1 when called to read a func-call parameter list; then key:value pairs
!are treated as keyword arguments
!eg: (a, b, c	)
!eg: (a		!
	unit ulist, ulistx
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
				addlistunit(ulist, ulistx, createunit0(jnull))
			else
				serror("null comma expr not allowed")
			fi
			lex()
		when rbracksym then
			if donulls then
				addlistunit(ulist, ulistx, nullunit)
			fi
			exit
		else
			addlistunit(ulist, ulistx, readunit())
			if lx.symbol in [commasym, semisym] then
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

func readindex(unit p, int dot)unit=
!at '['; dot=0/1 for a[]/a.[]
!syntax is:
![x] or [x, ...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
	unit q, plower, pupper

	lex()

	if not dot then
		case lx.symbol
		when rsqsym then
	fullslice:
			lex()
			plower:=createunit1(junary, duplunit(p))
			plower.tclop:=kklwb
			pupper:=createunit1(junary, duplunit(p))
			pupper.tclop:=kkupb
			p:=createunit2(jslice, p, createunit2(jmakerange, plower, pupper))
			return p
		when rangesym, colonsym then
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

		if q.tag=jmakerange then		!convert into a discrete slice
			p:=createunit2((dot|jdotslice|jslice), p, q)
		else
			p:=createunit2((dot|jdotindex|jindex), p, q)
		fi

		exit when lx.symbol<>commasym
		lex()
	od
	checksymbollex(rsqsym)
!	lex()
	return p
end

func readdotsuffix(unit p)unit=
!at '.' symbol
!read any modifiers for term currently in p
!multiple .terms can be present
	unit q, r, p2

	while lx.symbol=dotsym do
		lex()
		case lx.symbol
		when lsqsym then
			p:=readindex(p, 1)
		when namesym then
			p:=createunit2(jdot, p, createname(lx.symptr))
			lex()
		when propsym then
			if lx.subcode=kkbounds then
				q:=createunit1(jprop, duplunit(p))
				r:=createunit1(jprop, duplunit(p))
				if p.tag=jtypeconst then
					q.propcode:=kkminval
					r.propcode:=kkmaxval
				else
					q.propcode:=kklwb
					r.propcode:=kkupb
				fi

				p2:=createunit2(jmakerange, q, r)
				deleteunit(p, p2)
			else
	doprop:
				p:=createunit1(jprop, p)
				p.tclop:=lx.subcode
			fi
			lex()

		when bitfieldsym then
			p:=createunit1(jbitfield, p)
			p.bfcode:=lx.subcode
			lex()
		when ktypesym then			!.type, convert to .gettype
			case p.tag
			when jtypeconst then			!int.type=>int

			else
				SERROR("RDS:TYPEOF")
!				p:=createunit1(jtypeof, p)
			esac
			lex()

		when maxsym then
			lx.subcode:=kkmaxval
			goto doprop

		when minsym then
			lx.subcode:=kkminval
			goto doprop
		when stdtypesym then
			if p.tag=jtypeconst and lx.subcode=trange then
				q:=createunit2(jmakerange, 
					createunit1(junary, p), 
					createunit1(junary, p))
				q.a.propcode:=kkminval
				q.b.propcode:=kkmaxval
			else
				error
			fi
			lex()
			p:=q



		else
	error:
			serror("Unknown dot suffix")
		esac
	od
	return p
end

func readconstexpr(int needconst=1)unit=
	return readunit()
end

func readconstint:int=
!read expression that must yield a constant int value *now*; return value
	i64 x

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

proc readprocdef(symbol procowner, int scope)=
!at 'proc' etc symbol; read proc def or declaration
!syntax:
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
	int kwd, startline, closesym, shortfun
	symbol stproc, stname

	kwd:=lx.symbol
	shortfun:=lx.subcode=1
	nforloops:=0

	stproc:=readprocdecl(procowner, scope)
	checkequals()

	lex()

	startline:=getcurrline()

	if not shortfun then
		closesym:=checkbegin(0)
	fi

	pushproc(stproc)
	nextavindex:=0

	IF DRETVAR THEN
		stname:=getduplnameptr(stproc, dretvar, frameid)
		storemode(procowner, stproc.mode, stname.mode)
		adddef(stproc, stname)
	fi

	addtoproclist(stproc)

	if shortfun then
		stproc.code:=readunit()
		checksymbollex(semisym)
	else
		stproc.code:=readsunit()
		checkbeginend(closesym, kwd, startline)
	fi

	stproc.code:=makeblock(stproc.code)

	popproc()
end

global func readprocdecl(symbol procowner, int scope)symbol=
!at 'proc'  or 'func' 
!read proc declaration only, so exit at "=" or ";" symbol
!syntax:
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
!return st entry of proc, and positioned at '=' or semi

	int kwd, varparams, nparams, nretvalues, isthreaded
	int subprogno
	[maxtuplesize]int retmodes
	imodule ms
	isubprog ps

	ichar metadata, truename
	symbol pequiv, stproc, owner, paramlist, nameptr

	kwd:=lx.symbol				!remember keyword
	isthreaded:=lx.subcode=2

	pequiv:=nil
	metadata:=""
	truename:=nil
	varparams:=0

	lex()

	if lx.symbol=stringconstsym then		!assume dll truename
		truename:=pcm_copyheapstring(lx.svalue)
		convlcstring(lx.svalue)
		lx.symptr:=addnamestr(lx.svalue)
	else
		checksymbol(namesym)
	fi

	nameptr:=lx.symptr

	stproc:=getduplnameptr(procowner, nameptr, (insidedllimport|dllprocid|procid))
	if insidedllimport then scope:=subprog_scope fi
	stproc.isthreaded:=isthreaded

	if truename then
		stproc.truename:=truename
	fi

	adddef(procowner, stproc)
	if stproc.nameid=dllprocid then
		stproc.isimport:=1
	fi

	owner:=stproc
	pushproc(stproc)

	lex()
	if lx.symbol=mulsym then
		stproc.ishandler:=1
		lex()
	fi

	paramlist:=nil
	retmodes[1]:=tvoid
	nparams:=0
	nretvalues:=0

	nretvalues:=0
	if lx.symbol=lbracksym then		!possible params
		lex()
		if lx.symbol<>rbracksym then
			paramlist:=readparams(procowner, stproc, varparams, nparams)
			checksymbol(rbracksym)
		fi
		lex()

		if lx.symbol=colonsym or lx.symbol=sendtosym then
			lex()
			nretvalues:=readreturntype(owner, retmodes)
		elsif typestarterset[lx.symbol] or lx.symbol=namesym then
			nretvalues:=readreturntype(owner, retmodes)
		fi
	elsif lx.symbol=colonsym or lx.symbol=sendtosym then
		lex()
		nretvalues:=readreturntype(owner, retmodes)
	fi

	dretvar:=nil
	if nretvalues=1 then
		if lx.symbol=namesym then
			dretvar:=lx.symptr
			lex()
		fi
	fi

	unless nretvalues or (kwd<>kfuncsym) then		!func: no result given
		serror("Function needs ret type")
	end unless

	if nretvalues and (kwd<>kfuncsym) then		!proc: result given
		serror("Proc can't return value")
	fi

	stproc.paramlist:=paramlist
	stproc.nretvalues:=nretvalues

	case nretvalues
	when 0 then
		stproc.mode:=tvoid
	when 1 then
		storemode(procowner, retmodes[1], stproc.mode)
	else
		stproc.mode:=createtuplemode(procowner, retmodes, nretvalues, 0)
	esac

	if lx.symbol=atsym then			!equivalence
		SERROR("READPROCDEF @")
		lexchecksymbol(namesym)
		lex()
		stproc.atvar:=1
	fi

	stproc.code:=nil

	stproc.scope:=scope
	stproc.varparams:=varparams

	if procowner=stmodule then
		if stproc.namelen=5 and eqstring(stproc.name, "start") then
			modules[stmodule.moduleno].ststart:=stproc
			stproc.scope:=subprog_scope
			dosigcheck
		elsif stproc.namelen=4 and eqstring(stproc.name, "main") then
			ms:=modules[stmodule.moduleno]
			ps:=subprogs[stmodule.subprogno]

			if ps.mainmodule then serror("More than one main() in SP") fi
			ps.mainmodule:=stmodule.moduleno
			ms.stmain:=stproc

			if ps.subprogno=mainsubprogno then
				stproc.scope:=export_scope
dosigcheck:
				if stproc.paramlist or stproc.mode<>tvoid then
					serror("Wrong 'main/start' sig")
				fi

			fi
		fi
	fi

	popproc()

	return stproc
end

func readparams(symbol procowner, owner, int &varparams, &nparams)symbol=			!READPARAMS
!positioned at first symbol after '('; this is not ')'
!read list of params, return that list
!syntax is a list of names and/or types
!each param can optionally be followed by a default value
!finish pointing at ")"
	symbol stlist, stlistx, stname
	int parammode, pmode, m, isoptional, types

	stlist:=stlistx:=nil
	pmode:=tvoid
	nparams:=0
	parammode:=byval_param
	types:=0

	if lx.symbol=namesym and nextlx.symbol in [commasym, rbracksym] then
		types:=1
	fi

	do										!expect type of name at start of loop
		parammode:=byval_param
		isoptional:=0

		if types or istypestarter() then				!assume new mode
			pmode:=readtypespec(procowner)
gotmode:

			if nparams=0 and lx.symbol in [commasym, rbracksym] then
				do
					[32]char str
					++nparams
					str[1]:='$'; str[2]:=0
					strcat(str, strint(nparams))
					stname:=getduplnameptr(owner, addnamestr(&.str), paramid)
					adddef(owner, stname)

					storemode(owner, pmode, stname.mode)
					stname.parammode:=parammode
					addlistparam(&stlist, &stlistx, stname)

					case lx.symbol
					when rbracksym then
						exit
					esac

					checksymbollex(commasym)
!					lex()
					if lx.symbol=ellipsissym then
						varparams:=nparams		!no. of fixed params
						lex()
						exit
					fi

					pmode:=readtypespec(procowner)
				od
				return stlist
			fi

		elsif pmode=tvoid then
			serror("Type expected")
		fi

		case lx.symbol
		when addrsym then
			parammode:=byref_param
			lex()
			if lx.symbol=colonsym then lex() fi
		when ellipsissym then
			varparams:=nparams
			lex()
			return stlist
		esac

		checksymbol(namesym)
		++nparams
		stname:=getduplnameptr(owner, lx.symptr, paramid)
		adddef(owner, stname)
		lex()

		if parammode=byref_param then
			m:=createrefmode(procowner, pmode)
		else
			m:=pmode
		fi

		storemode(owner, m, stname.mode)
		stname.parammode:=parammode
		stname.optional:=isoptional
		addlistparam(&stlist, &stlistx, stname)

		case lx.symbol
		when assignsym, eqsym then
			lex()
			stname.code:=readunit()
			stname.equals:=1
			stname.optional:=1
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

func readcondsuffix(unit p)unit=
!p is a unit just read
!positioned at following symbol
!check whether a conditional suffix follows, and return p wrapped in a conditional if so
! ... if cond
! ... when cond
! ... unless cond
	unit q

	case lx.symbol
	when kwhensym then
		lex()
		return createunit2(jif, fixcond(readunit()), createunit1(jblock, p))
	when kunlesssym then
		lex()
		q:=createunit1(jnotl, fixcond(readunit()))
		q.tclop:=knot
		return createunit2(jif, q, createunit1(jblock, p))
	else
		return p
	esac
end

func readif:unit=
!at 'if'
	int pos1, kwd
	unit clist, clistx, plist, plistx, pelse, p

	pos1:=lx.pos
	kwd:=lx.symbol			!in case coming from elsecase etc
	lex()
	skipsemi()

	clist:=clistx:=plist:=plistx:=pelse:=nil

	if lx.symbol=kelsifsym then
		lex()
	fi
	nextif

	repeat
		lex()
nextif:
		addlistunit(clist, clistx, fixcond(readsunit()))

		skipsemi()
		checksymbollex(kthensym)

		if lx.symbol=colonsym then
			if clist=clistx and kwd=kifsym then
				lex()
				p:=createunit3(jif, clist, readunit(), nil)
				p.pos:=pos1
				return p
			else
				serror("then: not allowed")
			fi
		fi

		addlistunit(plist, plistx, readsunit())
		skipsemi()

	until lx.symbol<>kelsifsym

	case lx.symbol
	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()
		checkend(kendsym, kwd, 0)
	when kelsecasesym, kelseswitchsym then
		lx.symbol:=kwd
		pelse:=makeblock(readswitchcase())
	else
		checkend(kendsym, kwd, 0)
	esac

	p:=createunit3(jif, clist, plist, pelse)
	p.pos:=pos1
	return p
end

func readgoto(int gototag=jgoto)unit=
	lex()

	return readcondsuffix(createunit1(gototag, readunit()))
end

func readunless:unit=
	int pos
	unit pcond, pthen, pelse, p, q

	pos:=lx.pos
	lex()
	pcond:=fixcond(readsunit())
	checksymbollex(kthensym)
!	lex()

	pthen:=readsunit()

	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
	else			!assume simple if-then
		PELSE:=NIL
	fi
	checkend(kendsym, kunlesssym)
	p:=createunit3(jif, q:=createunit1(jnotl, pcond), pthen, pelse)
	q.tclop:=knot
	p.pos:=pos
	return p
end

func readswitchcase:unit=
	int pos1, kwd, opc, pos2, rangeused, nwhen
	unit pexpr, pwhenlist, pwhenlistx, pwhen, pwhenx, pelse, p, pthen, pwhenthen, pjump

	pos1:=lx.pos
	kwd:=lx.symbol			!remember kcasesym etc

	opc:=lx.subcode			!pick up tag: kcase etc
	pjump:=nil
!
	lex()

	skipsemi()

	if opc=jdoswitchx then
		checksymbollex(lbracksym)
		pjump:=readunit()
		checksymbollex(rbracksym)
		currproc.hasdoswx:=1
	fi

	if lx.symbol=kwhensym then
		if kwd=kswitchsym then
			serror("switch expr missing")
		fi
		pexpr:=nil
	else
		pexpr:=readsunit()			!index expression
		pexpr.nextunit:=pjump		!for doswitchx
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
			if p.tag=jmakerange then rangeused:=1 fi
			addlistunit(pwhen, pwhenx, p)
			if lx.symbol<>commasym then exit fi
			lex()
		od
		if lx.symbol<>sendtosym then
			checksymbol(kthensym)
		fi
		lex()
		pthen:=readsunit()
		pwhenthen:=createunit2(jwhenthen, pwhen, pthen)
		pwhenthen.pos:=pos2
		addlistunit(pwhenlist, pwhenlistx, pwhenthen)
	od

	if opc=jswitch and not rangeused then
		if nwhen<=8 then
			opc:=jcase
		fi
	fi

	case lx.symbol
	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()

		checkend(kendsym, kwd)
	when kelsifsym then
		lx.symbol:=kwd
		pelse:=makeblock(readif())
	when kelsecasesym, kelseswitchsym then
		lx.symbol:=kwd
		pelse:=makeblock(readswitchcase())
	else
		PELSE:=NIL
		checkend(kendsym, kwd)
	esac

	p:=createunit3(opc, pexpr, pwhenlist, pelse)
	p.pos:=pos1

	return p
end

func readstop:unit=
	unit p
	int i
	lex()
	if exprstarter[lx.symbol] then
		p:=createunit1(jstop, readunit())
	else
		p:=createunit0(jstop)
	fi
	return readcondsuffix(p)
end

func readreturn:unit=
	unit p, q

	lex()
	if exprstarter[lx.symbol] then
		q:=readunit()
		p:=createunit1(jreturn, q)
		p.length:=1
	else
		p:=createunit0(jreturn)
		p.length:=0
	fi

	return readcondsuffix(p)
end

func readdo:unit=
	unit p
	int pos

	pos:=lx.pos
	lex()
	p:=readsunit()
	checkend(kendsym, kdosym)
	p:=createunit1(jdo, p)
	p.pos:=pos
	return p
end

func readto:unit=
	int pos, id
	unit p, pcount, pbody

	pos:=lx.pos
	lex()

	pcount:=readunit()

	checksymbollex(kdosym)
	pbody:=readsunit()
	checkend(kendsym, ktosym, kdosym)
	id:=frameid
	if currproc.nameid<>procid then id:=staticid fi

!	p:=createunit3(jto, pcount, pbody, createname(getavname(currproc, id)))
	p:=createunit2(jto, pcount, pbody)
	p.pos:=pos
	return p
end

func readwhile:unit=
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

	checksymbollex(kdosym)
!	lex()
	pbody:=readsunit()

	checkend(kendsym, kwhilesym, kdosym)

	p:=createunit3(jwhile, pcond, pbody, pincr)
	p.pos:=pos

	return p
end

func readrepeat:unit=
	int pos
	unit pbody, pcond, p

	pos:=lx.pos
	lex()
	pbody:=readsunit()
	checksymbollex(kuntilsym)
!	lex()
	pcond:=fixcond(readunit())
	p:=createunit2(jrepeat, pbody, pcond)
	p.pos:=pos

	return p
end

func readloopcontrol:unit=
	int opc
	unit p

	opc:=lx.subcode

	lex()
	if lx.symbol=namesym and eqstring(lx.symptr.name, "all") then
		lex()
		p:=createunit1(opc, createconstunit(0, tint))

	elsif exprstarter[lx.symbol] then
		p:=createunit1(opc, readconstexpr(1))
	else
		p:=createunit1(opc, createconstunit(1, tint))
	fi
	return readcondsuffix(p)
end

func readprint:unit=
	int oldinreadprint, opc, isfprint, fshowname
	unit pformat, pdev, printlist, printlistx, p, q
	ref strbuffer expr

	ichar s

	oldinreadprint:=inreadprint
	inreadprint:=1
	opc:=lx.subcode

	case opc
	when jfprint, jfprintln then
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

	if not exprstarter[lx.symbol] then
		goto finish
	fi

	do
		case lx.symbol
		when commasym then		!assume extra comma, meaning nogap
			addlistunit(printlist, printlistx, createunit0(jnogap))
		when dollarsym then		!assume extra comma, meaning nogap
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

				addlistunit(printlist, printlistx, q:=createstringconstunit(s, expr.length))
			fi
			addlistunit(printlist, printlistx, p)
		esac
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish:
	inreadprint:=oldinreadprint
	if opc=jprint and printlist=nil then
		serror("No print items")
	fi
	if opc=jfprint and printlist=nil and pformat=nil then
		serror("No print items")
	fi

	if isfprint then
		if pformat=nil then
			serror("No fmt str")
		fi
		return createunit3(opc, pdev, pformat, printlist)
	else
		return createunit2(opc, pdev, printlist)
	fi
end

func readread:unit=
	int oldinreadprint, opc
	unit pformat, pdev, readlist, readlistx, p, pread

	oldinreadprint:=inreadprint
	inreadprint:=1
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
		if lx.symbol=commasym then lex() fi
	fi

	if opc=jreadln then
		addlistunit(readlist, readlistx, createunit1(jreadln, pdev))
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

		pread:=createunit1(jread, pformat)

!

		p:=createunit2(jassign, p, pread)

		addlistunit(readlist, readlistx, p)
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish:
	inreadprint:=oldinreadprint
	if opc=jread and readlist=nil then
		serror("No read items")
	fi

	return createunit1(jblock, readlist)
end

func readfor:unit=
!on 'for'; syntax is:
! for [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/od
! for var[, var] in/inrev expr [when expr] do stmts [else stmts] end/od *FORALL*
! for var in/inrev expr.bounds [when expr] do stmts [else stmts] end/od
! for var in/inrev <rangeexpr> [when expr] do stmts [else stmts] end/od

!AV codes:
!	I	loop index, always i64; will be 'i' (declared or not declared) or autovar
!	L	forall local variable; will be 'x' (declared or not declared); type is variable

	int pos, opc
	unit pindex, plocal				!for index; for index, local
	unit pfrom, pto, pstep, ptoinit	!for INDEX:=FROM to/downto TO [by STEP]/ INDEX in FROM..TO
	unit plist, passign				!for INDEX in/inrev LIST (also LIST.BOUNDS)
	unit pcond, pbody, pelse
	unit p

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

	opc:=jforup
	pstep:=nil
	pcond:=nil

	if lx.symbol in [insym, inrevsym] then				!assume forall
		if lx.symbol=jinrev then
			opc:=jfordown				!tentative; may be changed to forall
		fi
		lex()

		plist:=readunit()

		if plist.tag=jmakerange then
			pfrom:=plist.a
			pto:=plist.b
		else
			opc:=(opc=jforup|jforall|jforallrev)
			pfrom:=getrangelwbunit(duplunit(plist))
			pto:=getrangeupbunit(duplunit(plist))
		fi

	else
		if lx.symbol=assignsym then
			lex()
			pfrom:=readunit()
		else
			pfrom:=createconstunit(1, tint)
		fi
		checksymbol(ktosym)
		opc:=(lx.subcode=1|jfordown|jforup)
		lex()
		pto:=readunit()

		if lx.symbol=kbysym then
			lex()
			pstep:=readconstexpr(0)
			if pstep.tag=jconst then
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
	checksymbollex(kdosym)
!	lex()
	pbody:=readsunit()
	pelse:=nil

	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
	fi
	checkend(kendsym, kforsym, kdosym)

!deal with complex limit
!problem: autovar for STEP only created when there is an autovar for TO

	if pcond<>nil then
		pbody:=makeblock(createunit2(jif, pcond, pbody))
	fi
	pbody.nextunit:=pelse

!forup/down layout
!	a:	pindex/ptoinit
!	b:	pfrom/pto/pstep
!	c:	pbody/pelse

!forall/rev layout
!	a:	pindex/plocal/pfrom/pto
!	b:	plist/passign
!	c:	pbody/pelse

	case opc
	when jforup, jfordown then
		if plocal then serror("for i, x?") fi
		pindex.avcode:='I'
!		if pto.tag not in [jconst, jname] then
!			plocal:=createname(getavname(currproc))
!			plocal.avcode:='I'
!			ptoinit:=createunit2(jassign, plocal, pto)
!			pindex.nextunit:=ptoinit
!			pto:=plocal
!		fi

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

		passign:=createunit2(jassign, duplunit(plocal), 
					createunit2(jindex, duplunit(plist), duplunit(pindex)))
		plist.nextunit:=passign

		p:=createunit3(opc, pindex, plist, pbody)

	esac

	p.pos:=pos
	--nforloops
	return p
end

func readname:unit p=
	p:=readterm2()
	if p.tag<>jname then serror("Name expected") fi
	return p
end

global proc readtypedef(symbol owner, int scope=0)=
!at 'type' symbol
	symbol sttype, stname
	int t, m

	lexchecksymbol(namesym)
	stname:=lx.symptr

	lex()
	checkequals()
	lex()

	sttype:=getduplnameptr(owner, stname, typeid)
	adddef(owner, sttype)
	m:=createusertype(sttype)
	ttusercat[m]:=1

	t:=readtypespec(sttype, m)		!should return filled-in version of m

	sttype.scope:=scope
	storemode(owner, t, sttype.mode)

	if t>=0 then
		if ttisinteger[t]+ttisreal[t] then
			tttarget[m]:=t
		elsif ttisref[t] then
		elsecase ttbasetype[t]
		when tarray then
		when tslice then
!		when tslice, tvector, tflex then
		when trecord then
		else
			tttarget[m]:=t
		fi
	else
		storemode(owner, t, tttarget[m])
	fi

	if t>=0 then
		copyttvalues(m, t)
	else
		ttbasetype[m]:=tpending
	fi
end

global proc readrecordfields(symbol owner, int m)=
!positioned at just after type m has been read
!read vars inside struct for one line of struct body
	int nvars, offset
	symbol stname, stbitfield

	nvars:=0
	while lx.symbol=namesym do

		stname:=getduplnameptr(owner, lx.symptr, fieldid)
		storemode(owner, m, stname.mode)
		++nvars

		if unionpend.ulength then
			unionstr_copy(&stname.uflags, &unionpend)
			unionstr_concat(&unionstring, &unionpend)
			unionstr_clear(&unionpend)
		else
			unionstr_clear(&stname.uflags)
		fi
		unionlastvar:=stname			!filled in from outside with 'E' codes

		adddef(owner, stname)

		lex()

		case lx.symbol
		when atsym then
			lex()
			stname.atfield:=1
			stname.equivfield:=readequivfield(owner)
			if lx.symbol=addsym then
				lex()
				offset:=readconstint()
				if offset>stname.equivoffset.max then serror("Offset>255") fi
				stname.equivoffset:=offset
			fi

		when colonsym then				!read bitfields
!format is int : (a:1, b:3, c:2)
			lexchecksymbol(lbracksym)

			repeat
				lexchecksymbol(namesym)
				stbitfield:=getduplnameptr(owner, lx.symptr, fieldid)
				stbitfield.mode:=tbitfield
				adddef(owner, stbitfield)

				stbitfield.atfield:=1
				stbitfield.equivfield:=stname

				lexchecksymbol(colonsym)
				lexchecksymbol(intconstsym)
				stbitfield.bitfieldwidth:=lx.value
				lex()

			until lx.symbol<>commasym
			checksymbollex(rbracksym)
!			lex()

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

global proc readtabledef(symbol owner, int scope=0)=
!at 'tabledata' symbol
	int i, ncols, nrows, enums, nextenumvalue, firstval, lastval, startline, closesym
	int ltype
	symbol stvar, stenum, stgen
	const maxcols=20
	[maxcols]symbol varnameptrs
	[maxcols]int varlisttypes
	[maxcols]unit plist, plistx
	const maxrows=500
	[maxrows]int enumvalues

	enums:=lx.subcode				! means enumdata
	lex()

	tabledataname:=nil

	if lx.symbol=lbracksym then		!tabledata(...) read enum type
		if not enums then serror("Use 'enumdata'") fi
		enums:=1
		lex()
		checksymbollex(rbracksym)
!		lex()
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
		if ncols>0 then
			checksymbollex(lbracksym)
!			lex()
		fi
		if ++nrows>maxrows then
			serror("tabledata:too many rows")
		fi

		if enums then
			checksymbol(namesym)
			stgen:=lx.symptr				!generic symbol entry
			tabledataname:=stgen.name		!allow to be picked up by $ lx.symbol
			lex()
			if lx.symbol=eqsym then
				if nrows<>1 then serror("enum=x, 1st row only") fi
				lex()
				nextenumvalue:=readconstint()
			fi
			enumvalues[nrows]:=nextenumvalue

			stenum:=getduplnameptr(owner, stgen, constid)
			stenum.mode:=tint
			stenum.code:=createconstunit(nextenumvalue, tint)
			stenum.scope:=scope
			adddef(owner, stenum)
			if scope=export_scope then
				addexpconst(stenum)
			fi

			if nrows=1 then firstval:=nextenumvalue fi
			lastval:=nextenumvalue

			++nextenumvalue
			if ncols then				!comma always expected
				checksymbollex(commasym)		!check it
!				lex()
			fi
		fi

		for i:=1 to ncols do
			addlistunit(plist[i], plistx[i], readunit())
			if i=ncols then
				checksymbollex(rbracksym)
			else
				checksymbollex(commasym)
			fi
!			lex()
		od

		if lx.symbol<>commasym then exit fi
		lex()					!should be ( for next entry
		if lx.symbol=closesym then exit fi		!allow trailing comma on last entry
	od

	intabledata:=0

	skipsemi()
	checkbeginend(closesym, ktabledatasym, startline)

!Here, I have:

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

		stvar:=getduplnameptr(owner, varnameptrs[i], staticid)
		stvar.code:=createunit1(jmakelist, plist[i])
		stvar.code.length:=nrows
		stvar.istabdata:=1

		storemode(owner, varlisttypes[i], stvar.mode)
		stvar.scope:=scope

		adddef(owner, stvar)
		addstatic(stvar)
	od
end

global proc readclassdef(symbol owner, int scope)=
!at 'class' symbol
!read enough of the class to be able to generate export data
	int kwd, baseclass, m, startline, closesym, mrec, isrecord, align
	symbol nameptr, sttype

	kwd:=lx.symbol
	isrecord:=kwd=krecordsym

	lexchecksymbol(namesym)
	nameptr:=lx.symptr

	lex()
	baseclass:=0
	if lx.symbol=lbracksym then
		lex()
		baseclass:=readtypespec(owner)
		checksymbollex(rbracksym)
!		lex()
	fi

	checkequals()
	lex()

	align:=0
	if lx.symbol=atsym then
!		if lx.subcode=0 then
!SERROR("= @ N")
!!			lex()
!!			align:=readconstint()
!		else
			lex()
!		fi
		align:=1
	fi

	sttype:=getduplnameptr(owner, nameptr, typeid)
	adddef(owner, sttype)
	m:=createusertype(sttype)

	mrec:=createrecordmode(owner, m)
	storemode(owner, mrec, sttype.mode)

	storemode(owner, baseclass, sttype.baseclass)
	sttype.align:=align

	closesym:=checkbegin(1)

	startline:=getcurrline()

	readclassbody(sttype, kwd)

	checkbeginend(closesym, kwd, startline)

	sttype.scope:=scope
end

proc readclassbody(symbol owner, int classkwd)=
!at first symbol of a class or record body
!read fields, constants, types, methods.
	int kwd, t, lbcount:=0

	unionstr_clear(&unionstring)
	unionstr_clear(&unionpend)

	docase lx.symbol
	when kconstsym then
		readconstdef(owner, 0)
	when kfuncsym, kprocsym then
		kwd:=lx.symbol

		if owner.isimport then
			readprocdecl(owner, 0)
		else
			readprocdef(owner, 0)
		fi
	when krecordsym then
		readclassdef(owner, 0)

	when ktypesym then
		readtypedef(owner)
	when eofsym then
		serror("Class eof?")
		exit
	when semisym then
		lex()

	when ktabledatasym then
		readtabledef(owner, 0)

	when kmacrosym then
		readmacrodef(owner, 0)

	when kstructsym, kunionsym then
		unionstr_append(&unionpend, (lx.symbol=kstructsym|'S'|'U'))
		unionlastvar:=nil
		lex()
		if lx.symbol=lbracksym then ++lbcount; lex() fi
	when kendsym, rbracksym then
		if unionstring.ulength then
			if lx.symbol=rbracksym and lbcount then
				lex()
				--lbcount
			else
				checkend(kendsym, (unionstr_last(&unionstring)='S'|kstructsym|kunionsym))
			fi
			if unionlastvar=nil or unionpend.ulength then
				serror("Empty union group")
			fi
			case unionstr_last(&unionlastvar.uflags)
			when 'E', '*' then
			else
				unionstr_append(&unionlastvar.uflags, '*')
			esac
			unionstr_append(&unionlastvar.uflags, 'E')
			unionstring.ulength--
		else
			exit
		fi

	when kvarsym then

		lex()
		if istypestarter() then
	readmut:
			++insiderecord
			t:=readtypespec(owner)
			--insiderecord
		else
			t:=tauto
		fi
		readrecordfields(owner, t)

	when kletsym then
		serror("Let not allowed")

	else
		if istypestarter() then
			goto readmut
!		serror("record:need var")
		else
			exit
		fi
	end docase

	if lbcount then serror("LB?") fi

end

proc readimportmodule(symbol owner)=
!at 'importmodule' symbol
	int isnew, startline, closesym
	symbol stname, stname0

	if insidedllimport then serror("nested importdll") fi
!	libtype:=lx.subcode

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

	for i to nlibfiles do
		if eqstring(libfiles[i], stname.name) then
!			stname:=libtable[i]
			isnew:=0
			exit
		fi
	od

	if isnew then			!new
		addlib(stname.name)
	fi

	startline:=getcurrline()
	closesym:=checkbegin(0)

	insidedllimport:=1

	readimportbody(owner)

	insidedllimport:=0

	checkbeginend(closesym, kimportmodulesym, startline)

end

proc readimportbody(symbol owner)=
!positioned at first symbol of statement (which can be empty)
!return knode containing statement, or nil if not found (at 'end etc)
	int pos
	symbol d

	pos:=lx.pos

	do
		skipsemi()
		case lx.symbol
		when kprocsym, kfuncsym then
doproc:
			d:=readprocdecl(owner, 0)
			if ndllproctable>=maxdllproc then
				serror("Too many dll procs")
			fi
			dllproctable[++ndllproctable]:=d

		when ktypesym then
			readtypedef(owner, subprog_scope)

		when kconstsym then
			readconstdef(owner, subprog_scope)

		when krecordsym then
			readclassdef(owner, subprog_scope)

		when kvarsym then
			lex()
			readvardef(owner, subprog_scope, 0, dllvarid, kvarsym)

		when stdtypesym, namesym, krefsym, kicharsym, lsqsym, kslicesym then
			readvardef(owner, subprog_scope, 0, dllvarid, 0)

		when eofsym then
			exit

		when kendsym then
			exit
		else
			PS("symbol")
			serror("Not allowed in importmodule")
		esac
	od
end

func readequivfield(symbol owner)symbol=
!reading a class or struct body, where owner is that class/struct entry
!positioned at symbol following '@', should be name of an existing field
	symbol p, d

	checksymbol(namesym)
	d:=lx.symptr
	lex()

	p:=owner.deflist
	while p do
		if eqstring(p.name, d.name) then
			return p
		fi

		p:=p.nextdef
	od
	cpl d.name
	serror("Can't find @ field")
	return nil
end

func readrefproc(symbol owner, int typedefx)int=
!'ref' was seen, now positioned at 'proc' 'func' or 'method'
!read proc params and any result, return a complete ref proc spec
	int kwd, prettype, m, varparams, nparams
	[4]int retmodes
	symbol paramlist, stproc
	int rettype2, rettype3, nretvalues
	ichar name

	kwd:=lx.symbol				!remember whether proc or func
	
	lex()

	paramlist:=nil
	prettype:=tvoid
	nretvalues:=0
	varparams:=0

!need to create suitable holding typename in advance
	name:=nextautotype()
	stproc:=getduplnameptr(stmodule, addnamestr(name), typeid)
	adddef(stmodule, stproc)
	retmodes[1]:=tvoid

	if kwd=kfuncsym then
		if lx.symbol=lbracksym then		!possible params
			lex()
			if lx.symbol<>rbracksym then
				paramlist:=readparams(owner, stproc, varparams, nparams)
				checksymbol(rbracksym)
			fi
			lex()
			if lx.symbol=colonsym or lx.symbol=sendtosym then
				lex()
				nretvalues:=readreturntype(stproc, retmodes)
			elsif typestarterset[lx.symbol] or lx.symbol=namesym then
				nretvalues:=readreturntype(stproc, retmodes)
			fi
		elsif lx.symbol=colonsym or lx.symbol=sendtosym then
			lex()
			nretvalues:=readreturntype(stproc, retmodes)
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
				paramlist:=readparams(owner, stproc, varparams, nparams)
				checksymbol(rbracksym)
			fi
			lex()
		fi
		if typestarterset[lx.symbol] or lx.symbol=colonsym or lx.symbol=sendtosym then
			serror("proc can't have ret value")
		fi
	fi

	m:=createrefprocmode(owner, stproc, paramlist, kwd, prettype, typedefx)

	storemode(owner, retmodes[1], stproc.mode)
	stproc.nretvalues:=nretvalues

	ttnamedef[m]:=stproc

	stproc.varparams:=varparams

	return m
end

proc pushproc(symbol p)=
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

func readreturntype(symbol owner, []int &retmodes)int=
!read 1..maxtuplesize return types as part of func decl
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

func readset:unit=
!positioned at "["
	int length, nkeyvalues, oldirp
	unit p, ulist, ulistx

	lex()					!first symbol of first expression

	case lx.symbol
	when rsqsym then		!empty set, same as 0
		lex()
		return createunit1(jmakeset, nil)
	esac

	length:=0
	nkeyvalues:=0

	ulist:=ulistx:=nil

	do
		oldirp:=inreadprint
		inreadprint:=0
		p:=readunit()
		inreadprint:=oldirp
		if p.tag=jkeyvalue then ++nkeyvalues fi
		++length

		addlistunit(ulist, ulistx, p)

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
		skipsemi()						!allow a, b, c;]
	od
	lex()

	if nkeyvalues then
		if length>nkeyvalues then serror("dict: mixed elements") fi
		p:=createunit1(jmakedict, ulist)
	else
		p:=createunit1(jmakeset, ulist)
	fi
	p.length:=length
	return p
end

func istypestarter:int=
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

global func readunit:unit p=
	unit pt
	int pos

	pt:=nil
	pos:=lx.pos
	pt:=readterm2()

	if jisexpr[pt.tag]=0 then
		return pt
	fi

	if endsexpr[lx.symbol] then
		return pt
	fi

	if lx.symbol=assignsym then
		lex()
		p:=readterm2()
		if endsexpr[lx.symbol] then
			p:=createunit2(jassign, pt, p)
			p.pos:=pos
			return p
		fi
		p:=createunit2(jassign, pt, readassignment(p))
	else
		p:=readassignment(pt)
		p.pos:=pos
	fi

	while lx.symbol=pipesym do
		lex()
		p:=createunit2(jcall, readassignment(), p)
	od

	return p
end

func readassignment(unit pt=nil)unit p=
	int pos, opc
	unit q

	p:=readorterms(pt)

	if (opc:=lx.symbol) = assignsym then
		pos:=lx.pos
		lex()
		q:=readassignment(nil)
		p:=createunit2(jassign, p, q)
		p.pos:=pos
	fi
	return p
end

func readorterms(unit pt=nil)unit p=
	int pos

	p:=readandterms(pt)

	while lx.symbol=orlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			SERROR("OR:=")
		fi

		p:=createunit2(jorl, p, readandterms())
		p.pos:=pos
	od

	return p
end

func readandterms(unit pt=nil)unit p=
	int pos

	p:=readcmpterms(pt)

	while lx.symbol=andlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			SERROR("AND:=")
		fi

		p:=createunit2(jandl, p, readcmpterms())
		p.pos:=pos
	od

	return p
end

func readcmpterms(unit pt=nil)unit p=
	int pos, opc, n
	unit ulist, ulistx, q
	[4]byte genops

	p:=readinterms(pt)

	if lx.symbol not in [eqsym, cmpsym] then
		return p
	fi

	ulist:=ulistx:=p
	p:=createunit1(jcmpchain, p)
	n:=0				!n counts tclopnd after the first
	clear genops

	docase lx.symbol
	when eqsym, cmpsym then
		++n
		if n>genops.len then serror("cmpchain: Too many items") fi
		genops[n]:=lx.subcode

		pos:=lx.pos
		lex()

		q:=readinterms()
		addlistunit(ulist, ulistx, q)
		q.pos:=pos
	else
		exit
	end docase

	if n=1 then
		p.tag:=jcmp
		q:=p.a
		p.tclcond:=genops[1]
		p.b:=q.nextunit
		q.nextunit:=nil
	else
		p.cmpgenop:=genops
	fi

	return p
end

func readinterms(unit pt=nil)unit p=
	int pos, opc
	p:=readrangeterm(pt)

	docase lx.symbol
	when insym, notinsym then
		opc:=lx.subcode

		pos:=lx.pos
		lex()

		p:=createunit2(jin, p, readrangeterm())
		p.inv:=opc
		p.pos:=pos
	else
		exit
	end docase

	return p
end

func readrangeterm(unit pt=nil)unit p=
	int pos, opc
	p:=readaddterms(pt)

	if lx.symbol=rangesym then
		pos:=lx.pos
		lex()
		p:=createunit2(jmakerange, p, readaddterms())
		p.pos:=pos
	fi

	return p
end

func readaddterms(unit pt=nil)unit p=
	int pos, sym, tag, genop

	p:=readmulterms(pt)

	docase sym:=lx.symbol
	when addsym, subsym, iandsym, iorsym, ixorsym, minsym, maxsym then
		pos:=lx.pos
		genop:=lx.subcode
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto, p, readassignment())
			p.tclop:=symbolgentoops[sym]
			p.pos:=pos
			exit
		fi

		p:=createunit2(jbin, p, readmulterms())
		p.tclop:=symbolgenops[sym]
		p.pos:=pos
	else
		exit
	end docase

	return p
end

func readmulterms(unit pt=nil)unit p=
	int pos, sym

	p:=readpowerterms(pt)

	docase sym:=lx.symbol
	when mulsym, divsym, idivsym, iremsym, shlsym, shrsym, idivremsym then
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto, p, readassignment())
			p.tclop:=symbolgentoops[sym]
			p.pos:=pos
			exit
		fi

		p:=createunit2(jbin, p, readpowerterms())
		p.tclop:=symbolgenops[sym]
		p.pos:=pos
	else
		exit
	end docase

	return p
end

func readpowerterms(unit p=nil)unit=
	int pos

	if p=nil then
		p:=readterm2()
	fi

	while lx.symbol=powersym do
		pos:=lx.pos
		lex()
		p:=createunit2(jbin, p, readpowerterms())
		p.tclop:=kpower
		p.pos:=pos
	od

	return p
end

func readterm2:unit=
	unit p, q, r
	ref char pbyte
	u64 a
	int oldipl, opc, oldinrp, pos, shift, t

	pos:=lx.pos

	p:=readterm()

	docase lx.symbol
	when lbracksym then
		lex()
		oldinrp:=inreadprint
		inreadprint:=0
		q:=readslist(1, 1)
		checksymbollex(rbracksym)
!		lex()
		if p.tag=jsyscall then
			p.a:=q
		else
			p:=createunit2(jcall, p, q)
		fi
		inreadprint:=oldinrp
		p:=readcondsuffix(p)

	when ptrsym then
		p:=createunit1(jptr, p)
		lex()

	when lsqsym then
		p:=readindex(p, 0)

	when dotsym then
		p:=readdotsuffix(p)

	when colonsym then
		if inreadprint then exit fi
		lex()
		q:=readunit()
		p:=createunit2((inparamlist|jkeyword|jkeyvalue), p, q)

	when incrsym then
		case lx.subcode
		when kincrto then opc:=kloadincr
		when kdecrto then opc:=kloaddecr
		esac
		lex()
		p:=createunit1(jincr, p)
		p.tclop:=opc

	when lcurlysym then
		serror("X{...} not ready")
	else
		exit
	end docase

	p.pos:=pos

	return p
end

func readterm:unit=
	unit p, q, r
	u64 a
	int opc, pos, length
	byte strtype
	ichar s
	[32]u64 cstr

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

	when intconstsym, realconstsym then
		p:=createconstunit(lx.value, lx.subcode)
!		p.istrueconst:=1
		lex()

	when stringconstsym then
		p:=createstringconstunit(lx.svalue, lx.slength)
		p.strtype:=lx.subcode			!0/1/2 = str/bindata/strdata
		lex()

	when charconstsym then
		length:=lx.slength-1
		if length>8 then serror("Char const too long") fi
		a:=0
		if length then
			memcpy(&a, lx.svalue, length)
		fi
		p:=createconstunit(a, tc64)
		lex()

	when lbracksym then
		p:=readlbrack()

	when stdtypesym, krefsym, kicharsym then
!CPL "RT CAST"
		p:=readcast()

	when addsym, subsym, minsym, maxsym, abssym, inotsym, 
iandsym, iorsym, ixorsym, 
		mathsopsym, sqrtsym, sqrsym, maths2opsym, signsym then
		p:=readopc()

	when notlsym then
		if nextlx.symbol=assignsym then
			p:=readopc()
		else
			lex()
			p:=createunit1(jnotl, readterm2())
			p.tclop:=knot
		fi

	when istruelsym then
		if nextlx.symbol=assignsym then
			p:=readopc()
		else
			lex()
			p:=createunit1(jistruel, readterm2())
		fi

	when lsqsym then
		p:=readset()

	when incrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(jincr, readterm2())
		p.tclop:=opc

	when addrsym, daddrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(opc, readterm2())
		if p.a.tag=jcall then
			if p.a.b then
				serror("Params not allowed")
			fi
			p.a:=p.a.a			!lose the call
		fi

	when anddotsym then
		lex()
		p:=createunit1(jaddroffirst, readterm2())

	when compilervarsym then
		p:=readcompilervar()

	when dollarsym then
		if intabledata then
			if lx.subcode=1 then			!need char type
				cstr[1]:=0
				strcpy(cast(&cstr), tabledataname)
				p:=createconstunit(cstr[1], tu64)
			else
				s:=tabledataname
				if nextlx.symbol=addsym then
					lex()
					lex()
					checksymbol(intconstsym)
					s+:=lx.value
				fi
				p:=createstringconstunit(s, -1)
			fi
		else
			if ndollar<=0 then
				serror("[$] No array")
			fi
			p:=createunit1(junary, dollarstack[ndollar])
			p.propcode:=kkupb
		fi
		lex()

	when kcastsym then
		p:=readcastx()

	when kclampsym then
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbollex(commasym)
!		lex()
		q:=readunit()
		if lx.symbol=rbracksym and q.tag=jmakerange then
			r:=q.b
			q:=q.a
		else
			checksymbollex(commasym)
!			lex()
			r:=readunit()
			checksymbol(rbracksym)
		fi
		lex()

		q:=createunit2(jbin, p, q)
		q.tclop:=kmax
		p:=createunit2(jbin, q, r)
		p.tclop:=kmin

	when kgotosym then
		p:=readgoto()

	when kifsym then
		p:=readif()

	when kunlesssym then
		p:=readunless()

	when kcasesym, kdocasesym, kswitchsym, kdoswitchsym then
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

	when kswapsym then			!swap using func syntax
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbollex(commasym)
!		lex()
		q:=readunit()
		checksymbollex(rbracksym)
!		lex()
		p:=createunit2(jswap, p, q)

	when kevalsym then
		lex()
		p:=createunit1(jeval, readunit())

	when ksyscallsym then
		p:=createunit0(jsyscall)
		p.fnindex:=lx.subcode
		lex()

	when kstrincludesym then
		strtype:=lx.subcode
		lex()
		p:=createunit1(jstrinclude, readterm2())
		p.strtype:=strtype

	when kclearsym then
		lex()
		p:=createunit1(jclear, readterm2())

	when lcurlysym then
		serror("{...} not ready")

	when kslicesym then
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbollex(commasym)
		q:=readunit()
		checksymbollex(rbracksym)
		p:=createunit2(jmakeslice, p, q)

	else
DOELSE:
		cpl symbolnames[lx.symbol], =LX.SYMBOL, ISTYPESTARTER()
		serror("readterm?")
	end switch

	p.pos:=pos
	return p
end

proc readmacrodef(symbol owner, int scope)=
!positioned at 'macro'
!read expression macro-definition; global=1 if to be exported
!int kwd, varparams, try_level, prettype, nparams, rettype2, rettype3, nretvalues
!ichar metadata, truename
!symbol pequiv, stproc, owner, paramlist, nameptr

	symbol nameptr, stmacro, paramlist, paramlistx, stname

	lexchecksymbol(namesym)

	nameptr:=lx.symptr
	stmacro:=getduplnameptr(owner, nameptr, macroid)
	adddef(owner, stmacro)

	owner:=stmacro

	lex()

	paramlist:=paramlistx:=nil

	if lx.symbol=lbracksym then			!may have parameters
		lex()
		if lx.symbol<>rbracksym then
			do
				case lx.symbol
				when namesym then
					stname:=getduplnameptr(owner, lx.symptr, macroparamid)
					adddef(owner, stname)
					addlistparam(&paramlist, &paramlistx, stname)

					lex()
					if lx.symbol=rbracksym then
						exit
					fi
					checksymbollex(commasym)
!					lex()
				else
					serror("macro def params")
				esac
			od
		fi
		lex()						!skip )
	fi
	stmacro.paramlist:=paramlist
	stmacro.scope:=scope

	checkequals()
	lex()
	stmacro.code:=readunit()
end

func readrecase:unit=
	lex()
	if lx.symbol=kelsesym then
		lex()
		return createunit0(jrecase)
	else
		return createunit1(jrecase, readunit())
	fi
end

func fixcond(unit p)unit=
	checknotempty(p)
	if not isbooltag[p.tag] then
		insertunit(p, jistruel)
!		p.convcode:=kktoboolt
	fi
	return p
end

func readsunit(int inwhile=0)unit=
	int pos, m, sym, opc
	unit ulist, ulistx, p, q, r
	symbol stname

	pos:=lx.pos
	ulist:=ulistx:=nil

	repeat
		while lx.symbol=semisym do
			lex()
		od
		switch lx.symbol
		when kstaticsym then
			lex()
			if lx.symbol in [kletsym, kvarsym] then
				opc:=lx.symbol
				lex()
			else
!			opc:=kmutsym
				opc:=0
			fi
			readvardef(currproc, 0, 1, staticid, opc)

		when kprocsym, kfuncsym then
			readprocdef(currproc, 0)

		when stdtypesym, krefsym, kicharsym, kslicesym, lsqsym then
			if nextlx.symbol in [lbracksym, atsym, dotsym] then		!is a cast etc
				goto doexec
			else
				sym:=0
				goto dovar
			fi

		when kvarsym, kletsym then
			sym:=lx.symbol
			lex()
	dovar:
			q:=readvardef(currproc, 0, 0, frameid, sym)
			while q do								!initialised decls involve code
				r:=q.nextunit						!unlink from this block first
				q.nextunit:=nil
				addlistunit(ulist, ulistx, q)		!add one by-one
				q:=r
			od

		when ktypesym then
			readtypedef(currproc, 0)

		when kconstsym then
			readconstdef(currproc, 0)

		when krecordsym then
			readclassdef(currproc, 0)

		when kmacrosym then
			readmacrodef(currproc, 0)

		when ktabledatasym then
			readtabledef(currproc, 0)

		when eofsym then
			cpl currproc.name
			serror("Unexpected EOF in proc")

!these are needed to check for an empty sunit preceding
		when rbracksym, kthensym, kelsifsym, kelsesym, kuntilsym, kwhensym, 
				kelsecasesym, kelseswitchsym, kendsym then
			exit
!
		when namesym then
			case nextlx.symbol
			when colonsym then
				p:=createunit0(jlabeldef)
				stname:=getduplnameptr(currproc, lx.symptr, labelid)
				adddef(currproc, stname)
				p.def:=stname
				lex()
				lx.symbol:=semisym
				addlistunit(ulist, ulistx, p)
			when namesym then
				sym:=kvarsym
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

!		when questionsym then
!			lx.symbol:=semisym
		when questionsym then
			p:=createunit1(jsourceline, createstringconstunit(lx.svalue, -1))
			LX.SYMBOL:=SEMISYM
			doexec3


		else							!assume a statement
	doexec:
			p:=readunit()
	doexec2:
			if p.tag=jname and lx.symbol=namesym then
				serror("Possibly var/let needed")
			fi
	doexec3:
			addlistunit(ulist, ulistx, p)
			if lx.symbol=kdosym then
				exit
			fi

		end switch
	until lx.symbol<>semisym

	case lx.symbol
	when rbracksym, kthensym, kelsifsym, kelsesym, kuntilsym, kwhensym, kdosym, 
		kelsecasesym, kelseswitchsym, kendsym, commasym, 
		barsym then
	else
		serror("Readsunit: "";"" expected, or bad unit starter")
	esac

	if ulist=nil or ulist.nextunit then
		return createunit1(jblock, ulist)
	else
		return ulist
	fi
end

func readbxdata:unit p =
!EXPERIMENTAL CODE TO SPEED UP BYTE ARRAY INITS
!This assumes a sequence of intconsts, but needs to backtrack
!and general a normal makelist if any non-intconsts are seen
!This backtracking is not present.

!at '(', and initialising a byte-array
!this test assumes all values are intconst ones, 
!and creates a data-string object
!	int curralloc:=16, n:=0
	int curralloc:=4, n:=0
	ref byte q, r, qnew
!CPL "READBXDATA"

	p:=nil
	q:=r:=pcm_alloc(curralloc)

	do
		lex()
		skipsemi()
		if lx.symbol<>intconstsym then
			exit
		fi

		if n=curralloc then
			curralloc*:=2
			qnew:=pcm_alloc(curralloc)
			memcpy(qnew, q, n)
			r:=qnew+(r-q)
			pcm_free(q, n)
			q:=qnew
		fi

		r++^:=lx.value
		++n

		lex()
		if lx.symbol<>commasym then
			exit
		fi
	od
	checksymbol(rbracksym)
	lex()

	p:=createstringconstunit(q, n)
	p.strtype:=1

	p
end

proc checknotempty(unit p)=
	if p=nil or p.tag=jblock and p.a=nil then
		serror("Empty sunit")
	fi
end
=== mm_support.m 0 0 37/40 ===
global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

global func newsourcefile:ifile pf=
	pf:=pcm_allocz(filerec.bytes)
	if nsourcefiles>=maxsourcefile then loaderror("Too many sources") fi
	sources[++nsourcefiles]:=pf
	pf.fileno:=nsourcefiles
	pf
end

global proc mcerror(ichar mess)=
	println "MC Error:",mess

	stop 1
end

global proc serror_gen(ichar mess)=
	showdivider('*')
	println "Syntax Error:",MESS

	showerrorsource(lx.pos, currproc)

	println mess

	stopcompiler(sources[lx.fileno].filespec,getlineno(lx.pos))
end

proc showdivider(c64 ch)=
	to 87 do
		print ch
	od
	println
end

proc showerrorsource(int pos, symbol stproc=nil)=
	int fileno:=getfileno(pos), lineoffset
	ichar errorline,s

	fprintln "    Line:     #",getlineno(pos)
	if stproc and stproc.nameid=procid then
		fprintln "    Function: #()", stproc.name
	fi
	fprintln "    Module:   # (#)", sources[fileno].name,sources[fileno].filespec
	showdivider('-')

	s:=errorline:=getsourceline(pos)
	lineoffset:=getsourcepos(pos)-errorline

	to 6 do print " " od
	while s^ not in [10,0] do
		print s++^
	od
	println
	s:=errorline
	to 6 do print " " od
	to lineoffset do
		if s^=9 then print '\t' else print ' ' fi
		++s
	od
	println "^"
	showdivider('-')
end

global proc stopcompiler(ichar filename,int lineno)=
	filehandle f
	f:=fopen("$error.tmp","w")
	println @f,filename,lineno
	fclose(f)
CPL "PRESS key"; OS_GETCH()
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
	int pos

	if p then
CPL "P.POS"
		pos:=p.pos
	else
		pos:=mmpos
	fi

	showdivider('*')
	case pass
	when 'N' then println "RX Name Error: "
	when 'T' then println "TX Type Error: "
	when 'G' then println "GX Code Gen Error: "
	when 'A' then println "AX Code Gen Error: "
	esac

	showerrorsource(pos, currproc)

	println mess

	stopcompiler(sources[getfileno(pos)].filespec,getlineno(pos))
end

global proc rxerror(ichar mess,unit p=nil)=
	error_gen('N',mess,p)
end

global proc gerror(ichar mess,unit p=nil)=
	error_gen('G',mess,p)
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

	println "On line",getlineno(lx.pos),"in file",sources[lx.fileno].filespec

	println
	println "**** Lex Error:",mess,"****"
	println

	stopcompiler(sources[lx.fileno].filespec,getlineno(lx.pos))
end

global proc lxerror(ichar mess)=
	lxerror_gen(mess)
end

global proc loaderror(ichar mess,mess2="",mess3="")=
	println "Load Error:",mess,mess2,mess3
	println "Stopping"
	stop 1
end

global proc gs_additem(ref strbuffer dest,ichar s)=
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

global func isalphanum(int c)int=
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
		bitsize:=stdsize[i]*8

		switch bitsize
		when 0 then
			size:=0
		when 1,2,4 then
			size:=1
		else
			size:=bitsize/8
		end switch

		ttsize[i]:=size

		case i
		when ti8,ti16,ti32,ti64 then
			ttsigned[i]:=1
			ttisinteger[i]:=1
		when tu8, tu16, tu32, tu64, tc8, tc64 then
			ttisinteger[i]:=1
		when tr32, tr64 then
			ttisreal[i]:=1
		when tref, trefchar, trefbit then
			ttisref[i]:=1
		esac

!		if stdcat[i]=intcat and size<8 then
		if ttisinteger[i] and size<8 then
			ttisshort[i]:=1
		fi

		ttlower[i]:=1

		if i in [trecord, trange, tarray, tslice] then
			ttisblock[i]:=1
		fi

	od

	ttbasetype[trefchar]:=tref
	tttarget[trefchar]:=tc8

	ntypes:=tlast-1

	trefproc:=createrefmode(nil,tproc,0)
	treflabel:=createrefmode(nil,tlabel,0)
end

global func getsupportfile(ichar filename, ext="", path="")ifile =
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
	ifile pfile

!CPL "GETSUPP1", FILENAME, =PATH

	file:=filename

	if fverbose=3 then
		fprintln "Get file:# (ext:#) (path:#)",filename,ext, path
	fi

	if ext^ then
		strcpy(filespec,addext(filename,ext))
		file:=&.filespec
	fi

	if loadedfromma then
		file:=pcm_copyheapstring(extractfile(file))
	fi	

	for i to nsourcefiles do
		if eqstring(file, sources[i].filename) and not sources[i].issyslib then
			return sources[i]
		fi
	od

	if not isabspath(file) then
		strcpy(filespec2,path)
		strcat(filespec2,file)
		file:=&.filespec2
	fi

	if fverbose=3 and fileno then
		println "Checkfile:",file
	fi

!CPL =FILE
!CPL =FILENAME

	if file=nil or not checkfile(file) then
		loaderror("Can't find file: ",file)
	fi

	pfile:=loadsourcefile(file)
	if fverbose=3 and pfile then
		println "Found:",file
	fi

	pfile.issupport:=1
	return pfile
end

func isabspath(ichar filespec)int=
	ichar path:=extractpath(filespec)
	if path^ in ['\\','/'] or path^<>0 and (path+1)^=':' then	!absolute path
		return 1
	fi
	return 0
end

global proc initbblib=
	for i:=1 to D_typestarterset.len do typestarterset[D_typestarterset[i]]:=1 od
end

global func getfileno(word pos)int fileno=
	fileno:=pos.[24..31]
!
!CPL =FILENO
!CPL =POS.[0..23]

	if fileno<1 or fileno>nsourcefiles then
!		RETURN 1
		abortprogram("No file no")
	fi
	return fileno
end

global func getlineno(word pos)int=
	ichar source := getsourcestart(pos)
	ichar sline:=getsourceline(pos)
	ichar s:=sline
	int lineno:=1

	while s>=source do
		if s^=10 then ++lineno fi
		--s
	od

	return lineno
end

func getsourceline(word pos)ichar=
	ichar source := getsourcestart(pos)
	ichar s :=  getsourcepos(pos)

	while s>source and s^<>10 do --s od
	if s^=10 then ++s fi

	return s
end

func getsourcestart(word pos)ichar=
	return sources[getfileno(pos)].text
end

func getsourcepos(word pos)ichar=
	return sources[getfileno(pos)].text+pos.[0..23]
end

global func mgetsourceinfo(int pos, ichar &filename, &sourceline)int=
	int lineno

	lineno:=getlineno(pos)
	sourceline:=getsourcestart(pos)
	filename:=sources[getfileno(pos)].filespec

	lineno
end


global proc do_writema(ichar inpfile)=
	[300]char filename
	[maxsourcefile]int sflist
	filehandle f
	int offset, nfiles, fileno
	ifile pf

	return unless passlevel=ma_pass

	strcpy(filename, changeext(inpfile, "ma"))

!first build a table of source files to be o/p
	nfiles:=0

	for i to nsourcefiles when not sources[i].issyslib do
		sflist[++nfiles]:=i
	od

	if nfiles=0 then loaderror("MA: no files") fi

	f:=fopen(filename,"wb")
	if not f then loaderror("Can't create MA file ",filename) fi

	if fverbose then
		println "Writing ",filename
	fi
	fprintln @f,"=== MA # ===",nfiles

	for i to nfiles do
		pf:=sources[sflist[i]]

		fprintln @f,"=== # # # #/# ===",
			pf.filename,
			pf.issyslib,
			pf.issupport,
			i,nfiles

		offset:=getfilepos(f)
		writerandom(f,cast(pf.dupl),offset,pf.size)
	od

	println @f,"=== END ==="

	for i to nfiles do
		pf:=sources[sflist[i]]
		println @f,i,pf.filename, pf. issyslib, pf.issupport
	od

	fclose(f)
	stop
end

global proc do_getinfo(ichar filename)=
	filehandle f
	ichar fs
	imodule pm

	if passlevel=getst_pass then
		f:=fopen(fs:=changeext(filename,"list"),"wb")
		if f then
			println "Writing",fs
			getst(f,stprogram)
			fclose(f)
		fi
	fi

	if passlevel=getproj_pass then
		f:=fopen(fs:=changeext(filename,"proj"),"wb")
		if f then
			println "Writing",fs
			for i to nmodules do
				pm:=modules[i]
				println @f,pm.name:"16jl", subprogs[pm.subprogno].name:"16jl",
					pm.file.filespec:"q",
					pm.issyslib
			od

			fclose(f)
		fi
	fi
end

proc getst(filehandle f, symbol d)=
	symbol q

	getstrec(f,d)

	q:=d.deflist

	while q, q:=q.nextdef do
		getst(f,q)
	od
end

proc getstrec(filehandle f, symbol d)=
	ichar name

	case d.nameid
	when procid, dllprocid, typeid, constid, staticid,
		 macroid, dllvarid then
	else
		return
	esac

	if d.owner and d.owner.nameid<>moduleid then
		return									!only module-level names
	fi

	print @f, subprogs[moduletosub[d.moduleno]].name:"10jl",$

	print @f,d.owner.name:"12jl",$
	print @f,d.name:"18jl",$

	case d.nameid
	when procid then
		name:=(d.mode|"funcid"|"procid")
	when dllprocid then
		name:=(d.mode|"dllfuncid"|"dllprocid")
	else
		name:=namenames[d.nameid]
	esac

	print @f,name:"10jl"

	print @f,getlineno(d.pos):"5",$

	case d.scope
	when module_scope then name:="Module"
	when subprog_scope then name:="Subprog"
	when program_scope then name:="Program"
	else name:="Export"				!assume export scope
	esac

	print @f, name,$

	if d.isimport then
		print @f,"Import "
	fi

	print @f,strmode(d.mode):"10jlq",$
	print @f,sources[modules[d.moduleno].fileno].filespec:"q"
	println @f

end
=== mm_tables.m 0 0 38/40 ===
!include "mm_types.m"

global enumdata  [0:]ichar stdnames,
        [0:]byte stdsize,
        [0:]byte stdtcl =

!    type         name       bits     tcl
    (tvoid=0,     "void",       0,    tpvoid),

    (tr64,        "r64",        8,    tpr64),
    (tr32,        "r32",        4,    tpr32),
    (ti64,        "i64",        8,    tpi64),
    (tu64,        "u64",        8,    tpu64),
    (tc64,        "c64",        8,    tpu64),

    (tbool64,     "bool64",     8,    tpu64),

    (tref,        "ref",        8,    tpu64),
    (trecord,     "rec",        0,    tpblock),
    (trange,      "range",     16,    tpblock),

    (tarray,      "array",       0,   tpblock),
    (tslice,      "slice",      16,   tpblock),

    (tc8,         "c8",          1,   tpu8),
    (tbool8,      "b8",          1,   tpu8),
    (ti8,         "i8",          1,   tpi8),
    (ti16,        "i16",         2,   tpi16),
    (ti32,        "i32",         4,   tpi32),
    (tu8,         "u8",          1,   tpu8),
    (tu16,        "u16",         2,   tpu16),
    (tu32,        "u32",         4,   tpu32),

    (trefchar,    "ichar",       8,   tpu64),
    (trefbit,     "refbit",     16,   tpu64),

    (tauto,       "auto",        0,   tpu64),
    (tany,        "any",         0,   tpu64),
    (tproc,       "proc",        0,   tpu64),
    (tlabel,      "label",       0,   tpu64),
    (ttype,       "type",        8,   tpu64),
    (tbitfield,   "bitfl",       8,   tpu64),
    (ttuple,      "tuple",       0,   tpu64),
    (tpending,    "pend",        0,   tpu64),
    (tblock,      "block",       8,   tpblock),

    (tlast,       "last ",       0,   tpvoid),
end

global const tuser	= tlast

global const tint	= ti64
global const tword	= tu64
global const treal	= tr64
global const tbool	= tbool64

global const tfirstnum	= tr64
global const tlastnum	= tc64

global const tfirstshort	= tc8
global const tlastshort		= tu32

global const maxtuplesize = 4

global int trefproc
global int treflabel



global enumdata []ichar sysfnnames, []byte sysfnparams, []byte sysfnres =
	(sf_init,				$,	0,	0),
	(sf_print_startfile,	$,	0,	0),
	(sf_print_startstr,		$,	0,	0),
	(sf_print_startptr,		$,	0,	0),
	(sf_print_startcon,		$,	0,	0),
	(sf_print_setfmt,		$,	0,	0),
	(sf_print_nogap,		$,	0,	0),
	(sf_print_space,		$,	0,	0),
	(sf_print_i64,			$,	0,	0),
	(sf_print_i64_nf,		$,	0,	0),
	(sf_print_u64,			$,	0,	0),
	(sf_print_r64,			$,	0,	0),
	(sf_print_r32,			$,	0,	0),
	(sf_print_str,			$,	0,	0),
	(sf_print_str_nf,		$,	0,	0),
	(sf_print_strsl,		$,	0,	0),
	(sf_print_ptr,			$,	0,	0),
	(sf_print_ptr_nf,		$,	0,	0),
	(sf_print_c8,			$,	0,	0),
	(sf_print_bool,			$,	0,	0),
!	(sf_print_var,			$,	0,	0),
	(sf_print_newline,		$,	0,	0),
	(sf_print_end,			$,	0,	0),
	(sf_read_i64,			$,	0,	0),
	(sf_read_r64,			$,	0,	0),
	(sf_read_str,			$,	0,	0),
	(sf_read_fileline,		$,	0,	0),
	(sf_read_strline,		$,	0,	0),
	(sf_read_conline,		$,	0,	0),

	(sf_getnprocs,			$,	0,	1),		!access funcs
	(sf_getprocname,		$,	0,	1),
	(sf_getprocaddr,		$,	0,	1),

	(sf_power_i64,			$,	0,	1),
	(sf_unimpl,				$,	0,	1),

end
!
global [sysfnnames.len]symbol sysfnhandlers

!global [sysfnnames.len]int sysfnproclabels

!global int mmpos
!global byte fshowpst


!!---
global enumdata [0:]ichar jtagnames,
				   [0:]byte jsubs, [0:]byte jisexpr, [0:]byte jsolo =
!Basic units; these don't follow normal rules of params needing to be units or lists
!jisexpr=1/2 when unit returns a value; 1 means unary, 2 binary op,
! 3 means returns a value, but is not a unary or binary op
!jsolo = 1 means unit is allowed standalone without its value being used

!a,b,c are unitrec refs, which can be a single unit, or a linked-list chain
!(usually in forward order)
!	L means .a/b/c pointing to a unitlist; L can be nil for an empty list
!	u means .a/b/c pointing to a single unit
!	u/nil means can be nil

![a=u] means a is a unit/list, or is nil

	(jnone=0,		$,	0,		0,	0), ! For tagname lookups when tag is zero
	(jconst,		$,	0,		3,	0), ! value/etc=value, typeno=type code
	(jnull,			$,	0,		3,	0), ! Place holder unit: means 'param no present' when used where a param is expected
	(jvoidvar,		$,	0,		3,	0), ! create void variant
	(jname,			$,	0,		3,	0), ! def=nameptr
!	(jname,			$,	0,		3,	1), ! def=nameptr
	(jnamelv,		$,	0,		3,	0), ! def=nameptr
	(jblock,		$,	1,		0,	1), ! a=L
	(jdecimal,		$,	0,		3,	0), ! svalue=str, slength
	(jstrinclude,	$,	1,		3,	0), !
	(jsourceline,	$,	1,		3,	0), !

!Logical Operators

	(jandl,			$,	2,		2,	0), ! A B	This group are for conditional expressions (no result)
	(jorl,			$,	2,		2,	0), ! A B

	(jnotl,			$,	1,		1,	0), ! a
	(jistruel,		$,	1,		1,	0), ! a
	(jisfalsel,		$,	1,		1,	0), ! a

!Expressions and Operators

	(jmakelist,		$,	2,		3,	0), ! a=L, b=[u], length=N; element list/lower bound expr
	(jmakerange,	$,	2,		3,	0), ! A B
	(jmakeset,		$,	1,		3,	0), ! a=L, length=N
	(jmakedict,		$,	1,		3,	0), !
	(jmakeslice,	$,	2,		3,	0), !
	(jreturnmult,	$,	1,		0,	0), !

	(jkeyword,		$,	1,		3,	0), ! def=st entry
	(jkeyvalue,		$,	2,		3,	0), ! A B
	(jassign,		$,	2,		3,	1), ! A B a := x
	(jassignmm,		$,	2,		3,	1), ! A B (a,b,c) := (x,y,z)
	(jassignms,		$,	2,		3,	1), ! A B (a,b,c) := x
	(jassignmdrem,	$,	2,		3,	1), ! A B (a,b) := x divrem y
!	(jcallfn,		$,	2,		3,	1), ! A B
	(jcall,			$,	2,		3,	1), ! A B

	(jcmp,			$,	2,		2,	0), ! A B
	(jcmpchain,		$,	2,		1,	0), ! A B
	(jbin,			$,	2,		2,	0), ! A B
	(junary,		$,	2,		1,	0), ! A B
	(jprop,			$,	2,		1,	0), ! A B
	(jbinto,		$,	2,		2,	0), ! A B
	(junaryto,		$,	1,		1,	0), ! A B
	(jincr,			$,	1,		3,	0), ! a	++a
	(jin,			$,	2,		2,	0), ! A B

	(jinrev,		$,	2,		2,	0), ! A B
	(jinrange,		$,	2,		2,	0), ! A B
	(jinset,		$,	2,		2,	0), ! A B

	(jstringz,		$,	0,		3,	0), ! A B

	(jindex,		$,	2,		3,	0), ! A B		a[b]
	(jindexlv,		$,	2,		3,	0), ! A B		a[b]
	(jslice,		$,	2,		3,	0), ! A B		a[b.a..b.b]
!	(jnewslice,		$,	2,		3,	0), ! A B		slice(a,b)

	(jdot,			$,	2,		3,	0), ! A B opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(jdotlv,		$,	2,		3,	0), ! A B opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(jdotindex,		$,	2,		3,	0), ! A B		a[b]
	(jdotslice,		$,	2,		3,	0), ! A B		a[b]
!	(janddotindex,	$,	2,		3,	0), ! A B		a[b]

	(jptr,			$,	1,		3,	0), ! a		a^
	(jptrlv,		$, 	1,		3,	0), ! a		a^
	(jaddrof,		$,	2,		3,	0), ! a		&a
	(jaddroffirst,	$,	1,		3,	0), ! a		&a
	(jdaddrvv,		$,	1,		3,	0), ! a		&&a
!	(jdaddrtv,		$,	1,		3,	0), ! a		&&a (from jdaddrvv)
	(jconvert,		$,	1,		3,	0), ! typeno=T a		T(a)			T
	(jshorten,		$,	1,		3,	0), !
	(jautocast,		$,	1,		3,	0), ! typeno=T a		T(a)			T
	(jtypepun,		$,	1,		3,	0), ! typeno=T a		T@(a)			T
	(jtypeconst,	$,	0,		3,	0), ! typeno=T			typeconst(T)
	(joperator,		$,	0,		3,	0), ! opcode=opc
	(jupper,		$,	1,		3,	0), ! a		$					T

	(jbitwidth,		$,	1,		1,	0), ! a
	(jbytesize,		$,	1,		1,	0), ! a
	(jtypestr,		$,	0,		1,	0), ! a
!	(jsliceptr,		$,	0,		1,	0), ! a
	(jbitfield,		$,	1,		3,	0), ! a

	(jminvalue,		$,	1,		3,	0), ! a
	(jmaxvalue,		$,	1,		3,	0), ! a

!Translator Variables

	(jcvlineno,		$,	0,		3,	0), !
	(jcvstrlineno,	$,	0,		3,	0), ! 
	(jcvmodulename,	$,	0,		3,	0), ! 
	(jcvfilename,	$,	0,		3,	0), ! 
	(jcvfunc,	$,	0,		3,	0), ! 
	(jcvdate,		$,	0,		3,	0), ! 
	(jcvtime,		$,	0,		3,	0), ! 
	(jcvversion,	$,	0,		3,	0), ! 
	(jcvtypename,	$,	0,		3,	0), ! 
!	(jcvtargetbits,	$,	0,		3,	0), ! 
!	(jcvtargetsize,	$,	0,		3,	0), ! 
!	(jcvtargetcode,	$,	0,		3,	0), ! 
	(jcvnil,		$,	0,		3,	0), ! 
	(jcvpi,			$,	0,		3,	0), ! 
	(jcvinfinity,	$,	0,		3,	0), ! 
	(jcvtrue,		$,	0,		3,	0), ! 
	(jcvfalse,		$,	0,		3,	0), ! 

	(jwhenthen,		$,	2,		0,	0), ! a=L b=u
	(jfmtitem,		$,	2,		3,	0), ! A B  x/fmtstr
	(jnogap,		$,	0,		3,	0), ! 
	(jspace,		$,	0,		3,	0), ! 

!Statements

!	(jcallproc,		$,	2,		0,	1), ! a=fn b=L, length
	(jreturn,		$,	1,		0,	0), ! a=x/nil
	(jsyscall,		$,	1,		3,	1), ! a=x or nil

!	(jassign,		$,	0,		3,	0), ! A B
	(jto,			$,	3,		0,	0), ! a=N, b=body, c=tempvar/nil, def=name
	(jif,			$,	3,		3,	1), ! condcode a=then b=else
	(jforup,		$,	3,		0,	0), ! 
	(jfordown,		$,	3,		0,	0), !
	(jforall,		$,	3,		0,	0), !
	(jforallrev,	$,	3,		0,	0), !
	(jwhile,		$,	3,		0,	1), ! a=x b=u
	(jrepeat,		$,	2,		0,	1), ! a=u b=x
	(jgoto,			$,	1,		0,	1), ! a=x
	(jlabeldef,		$,	0,		0,	0), ! def=nameptr
	(jredo,			$,	0,		0,	1), ! [a=x]
	(jnext,			$,	0,		0,	1), ! [a=x]
	(jexit,			$,	0,		0,	1), ! [a=x]
	(jdo,			$,	1,		0,	1), ! [a=u
	(jcase,			$,	3,		3,	1), ! a=x b=L [c=else]		L is series of whenthen pairs
	(jdocase,		$,	3,		0,	1), ! a=x b=L [c=else]
	(jswitch,		$,	3,		3,	1), ! a=x b=L [c=else]
	(jdoswitch,		$,	3,		0,	1), ! a=x b=L [c=else]
	(jdoswitchu,	$,	3,		0,	1), ! a=x b=L [c=else]
	(jdoswitchx,	$,	3,		0,	1), ! a=x b=L [c=else]
	(jswap,			$,	2,		0,	1), ! A B
	(jselect,		$,	3,		3,	1), ! Not implemented
	(jrecase,		$,	1,		0,	0), ! Not implemented
!	(jrecaseelse,	$,	0,		0,	0), ! Not implemented

	(jprint,		$,	2,		0,	1), ! [a=dev] b=L
	(jprintln,		$,	2,		0,	1), ! [a=dev] b=L
	(jfprint,		$,	3,		0,	1), ! [a=dev] b=fmtstr c=L
	(jfprintln,		$,	3,		0,	1), ! [a=dev] b=fmtstr c=L
!	(jsprint,		$,	2,		0,	0), !         b=L 
!	(jsfprint,		$,	2,		0,	0), !         b=L
	(jread,			$,	2,		0,	1), ! [a=dev] b=L
	(jreadln,		$,	2,		0,	1), ! [a=dev] b=L
!	(jsread,		$,	2,		0,	0), ! [a=dev] b=L
!	(jsreadln,		$,	2,		0,	0), ! [a=dev] b=L
	(jstop,			$,	1,		0,	0), ! [a=x]
	(jeval,			$,	1,		3,	1), ! "
!	(jstack,		$,	1,		0,	0), ! "
!	(junstack,		$,	1,		0,	0), ! "
	(jclear,		$,	1,		1,	1), ! "

!	(jdummy,		$,	0,		3,	0)
end

global enumdata []ichar bitfieldnames=
	(bf_msb,		$),
	(bf_lsb,		$),
	(bf_msbit,		$),
	(bf_lsbit,		$),
	(bf_msw,		$),
	(bf_lsw,		$),
	(bf_odd,		$),
	(bf_even,		$),
end

global enumdata [0:]ichar optypenames =
	(no_op=0,		$),
	(bin_op,		$),
	(mon_op,		$),
	(prop_op,		$),
end

!!---
global enumdata []ichar symbolnames,
					[]byte symboloptypes,
					[]byte symbolgenops,
					[]byte symbolgentoops,
					[]byte symbolopprios,
					[]byte exprstarter =
!First half are basic tokens returned by lexreadtoken()
	(dotsym,			".",		0,			0,	0,	0,	0),		! "."
	(anddotsym,			"&.",		0,			0,	0,	0,	1),		! "&."
	(commasym,			",",		0,			0,	0,	0,	0),		! ","
	(semisym,			";",		0,			0,	0,	0,	0),		! ";"
	(colonsym,			":",		0,			0,	0,	0,	0),		! ":"
	(assignsym,			":=",		bin_op,		0,	0,	1,	0),		! :=
	(sendtosym,			"=>",		0,			0,	0,	0,	0),		! =>
	(pipesym,			"->",		0,			0,	0,	0,	0),		! ->
	(lbracksym,			"(",		0,			0,	0,	0,	1),		! (
	(rbracksym,			")",		0,			0,	0,	0,	0),		! )
	(lsqsym,			"[",		0,			0,	0,	0,	1),		! [
	(rsqsym,			"]",		0,			0,	0,	0,	0),		! ]
	(lcurlysym,			"{",		0,			0,	0,	0,	0),		! {
	(rcurlysym,			"}",		0,			0,	0,	0,	0),		! }
	(ptrsym,			"^",		0,			0,	0,	0,	1),		! ^
	(barsym,			"|",		0,			0,	0,	0,	0),		! |
!	(dbarsym,			"||",		0,			0,	0,	0,	0),		! ||
	(atsym,				"@",		0,			0,	0,	0,	0),		! @
!	(datsym,			"@@",		0,			0,	0,	0,	0),		! @@
	(questionsym,		"?",		0,			0,	0,	0,	0),		! ?
	(addrsym,			"&",		0,			0,	0,	0,	1),		! &
	(daddrsym,			"&&",		0,			0,	0,	0,	0),		! &&
!	(curlsym,			"~",		0,			0,	0,	0,	0),		! ~
	(rangesym,			"..",		bin_op,		0,	0,	5,	0),		! ..
	(ellipsissym,		"...",		0,			0,	0,	0,	0),		! ...
	(hashsym,			"#",		0,			0,			0,			0,	0),		! #

!	(opsym,				$,		0,	0,	0,	0,	0),		! Any operator or property tag (use sets to distinguish)

	(addsym,			"+",		bin_op,		kadd,		kaddto,		4,	1),
	(subsym,			"-",		bin_op,		ksub,		ksubto,		4,	1),
	(mulsym,			"*",		bin_op,		kmul,		kmulto,		3,	0),
	(divsym,			"/",		bin_op,		kdiv,		kdivto,		3,	0),
	(idivsym,			"%",		bin_op,		kidiv,		kidivto,	3,	0),
	(iremsym,			"rem",		bin_op,		kirem,		kiremto,	3,	0),
	(idivremsym,		"divrem",	bin_op,		kidivrem,	0,			3,	0),
	(iandsym,			"iand",		bin_op,		kbitand,	kbitandto,	4,	0),
	(iorsym,			"ior",		bin_op,		kbitor,		kbitorto,	4,	0),
	(ixorsym,			"ixor",		bin_op,		kbitxor,	kbitxorto,	4,	0),
	(shlsym,			"<<",		bin_op,		kshl,		kshlto,		3,	0),
	(shrsym,			">>",		bin_op,		kshr,		kshrto,		3,	0),
	(minsym,			"min",		bin_op,		kmin,		kminto,		4,	1),
	(maxsym,			"max",		bin_op,		kmax,		kmaxto,		4,	1),
	(andlsym,			"and",		bin_op,		0,			0,			7,	0),
	(orlsym,			"or",		bin_op,		0,			0,			8,	0),
	(xorlsym,			"xor",		bin_op,		0,			0,			8,	0),

	(eqsym,				"=",		bin_op,		0,			0,			6,	1),
	(cmpsym,			"cmp",		bin_op,		0,			0,			6,	1),
	(powersym,			"**",		bin_op,		kpower,		0,			2,	0),
	(insym,				"in",		bin_op,		0,			0,			6,	0),
	(notinsym,			"notin",	bin_op,		0,			0,			6,	0),
	(inrevsym,			"inrev",	0,			0,			0,			0,	0),

	(notlsym,			"not",		mon_op,		knot,		knotto,		0,	1),
	(istruelsym,		"istrue",	mon_op,		0,			ktoboolto,	0,	1),
	(inotsym,			"inot",		mon_op,		kbitnot,	kbitnotto,	0,	1),
	(abssym,			"abs",		mon_op,		kabs,		kabsto,		0,	1),
	(signsym,			"sign",		mon_op,		ksign,		0,			0,	1),
	(sqrtsym,			"sqrt",		mon_op,		ksqrt,		0,			0,	1),
	(sqrsym,			"sqr",		mon_op,		ksqr,		0,			0,	1),

	(propsym,			$,			prop_op,		0,			0,			0,	0),
	(mathsopsym,		$,			0,	0,	0,	0,	1),		! sin etc
	(maths2opsym,		$,			0,	0,	0,	0,	1),		! atan2 etc

	(bitfieldsym,		$,			0,	0,	0,	0,	0),		! Special bit selections
	(eolsym,			$,			0,	0,	0,	0,	0),		! End of line
	(eofsym,			$,			0,	0,	0,	0,	0),		! Eof seen
	(rawxnamesym,		$,			0,	0,	0,	0,	0),		! unassigned name, case-sensitive, that is never a reserved word
	(incrsym,			$,			0,	0,	0,	0,	1),		! 1/2 = ++/--; later may add +2 for x++/x--
	(intconstsym,		$,			0,	0,	0,	0,	1),		! 123 32 bits signed
	(realconstsym,		$,			0,	0,	0,	0,	1),		! 123.4 64 bits
	(charconstsym,		$,			0,	0,	0,	0,	1),		! 'A' or 'ABCD'
	(stringconstsym,	$,			0,	0,	0,	0,	1),		! "ABC"

!Second half are tokens that can be yielded after a name lookup:
	(unitnamesym,		$,			0,	0,	0,	0,	0),		! 
	(namesym,			$,			0,	0,	0,	0,	1),		! identifier symbol
	(kincludesym,		$,			0,	0,	0,	0,	0),		! INCLUDE
	(kstrincludesym,	$,			0,	0,	0,	0,	1),		! SINCLUDE/BINCLUDE
	(regsym,			$,			0,	0,	0,	0,	0),		! x64 registers
	(xregsym,			$,			0,	0,	0,	0,	0),		! XMM registers
	(fregsym,			$,			0,	0,	0,	0,	0),		! ST registers
	(mregsym,			$,			0,	0,	0,	0,	0),		! MMX registers
	(jmpccsym,			$,			0,	0,	0,	0,	0),		! 
	(setccsym,			$,			0,	0,	0,	0,	0),		! 
	(movccsym,			$,			0,	0,	0,	0,	0),		! 
	(segnamesym,		$,			0,	0,	0,	0,	0),		! 
	(asmopcodesym,		$,			0,	0,	0,	0,	0),		! MOV etc

	(stdtypesym,		$,			0,	0,	0,	0,	1),		! INT, CHAR etc
	(kicharsym,			$,			0,	0,	0,	0,	1),		! ICHAR IVOID
	(kifsym,			$,			0,	0,	0,	0,	1),		! 
	(kthensym,			$,			0,	0,	0,	0,	0),		! 
	(kelsifsym,			$,			0,	0,	0,	0,	0),		! 
	(kelsesym,			$,			0,	0,	0,	0,	0),		! 
	(kelsecasesym,		$,			0,	0,	0,	0,	0),		! 
	(kelseswitchsym,	$,			0,	0,	0,	0,	0),		! 
	(kendsym,			$,			0,	0,	0,	0,	0),		! 
	(kunlesssym,		$,			0,	0,	0,	0,	0),		! 
	(kcasesym,			$,			0,	0,	0,	0,	1),		! CASE
	(kdocasesym,		$,			0,	0,	0,	0,	0),		! DOCASE
	(krecasesym,		$,			0,	0,	0,	0,	0),		! RECASE
	(kwhensym,			$,			0,	0,	0,	0,	0),		! 
	(kforsym,			$,			0,	0,	0,	0,	0),		! FOR
	(ktosym,			$,			0,	0,	0,	0,	0),		! TO/DOWNTO
	(kbysym,			$,			0,	0,	0,	0,	0),		! 
	(kdosym,			$,			0,	0,	0,	0,	0),		! 
	(kwhilesym,			$,			0,	0,	0,	0,	0),		! 
	(krepeatsym,		$,			0,	0,	0,	0,	0),		! 
	(kuntilsym,			$,			0,	0,	0,	0,	0),		! 
	(kreturnsym,		$,			0,	0,	0,	0,	0),		! 
	(kstopsym,			$,			0,	0,	0,	0,	0),		! 
	(kloopsym,			$,			0,	0,	0,	0,	0),		! EXIT/NEXT/LOOP/REDO/RESTART
	(kgotosym,			$,			0,	0,	0,	0,	0),		! GO/GOTO
	(kswitchsym,		$,			0,	0,	0,	0,	0),		! SWITCH
	(kdoswitchsym,		$,			0,	0,	0,	0,	0),		! DOSWITCH
	(kprintsym,			$,			0,	0,	0,	0,	0),		! PRINT/PRINTLN/FPRINT/FPRINTLN
	(kreadsym,			$,			0,	0,	0,	0,	0),		! READ/READLN
	(kprocsym,			$,			0,	0,	0,	0,	0),		! PROC
	(kfuncsym,		$,			0,	0,	0,	0,	0),		! FUNCTION
	(klabelsym,			$,			0,	0,	0,	0,	0),		! LABEL
	(krecordsym,		$,			0,	0,	0,	0,	0),		! RECORD
	(kstructsym,		$,			0,	0,	0,	0,	0),		! STRUCT
	(kunionsym,			$,			0,	0,	0,	0,	0),		! UNION
	(kimportmodulesym,	$,			0,	0,	0,	0,	0),		! IMPORTDLL/IMPORTMODULE
	(kprojectsym,		$,			0,	0,	0,	0,	0),		! PROJECT
	(ktypesym,			$,			0,	0,	0,	0,	0),		! TYPE
	(krefsym,			$,			0,	0,	0,	0,	1),		! REF
	(kvoidsym,			$,			0,	0,	0,	0,	1),		! VOID
	(kvarsym,			$,			0,	0,	0,	0,	0),		! MUT
	(kletsym,			$,			0,	0,	0,	0,	0),		! LET
	(kslicesym,			$,			0,	0,	0,	0,	0),		! SLICE/SLICE2D
	(kmacrosym,			$,			0,	0,	0,	0,	0),		! MACRO
!	(koperatorsym,		$,			0,	0,	0,	0,	0),		! OPERATOR
	(kconstsym,			$,			0,	0,	0,	0,	0),		! 
	(kclearsym,			$,			0,	0,	0,	0,	0),		! CLEAR
	(kheadersym,		$,			0,	0,	0,	0,	0),		! MODULE
	(kglobalsym,		$,			0,	0,	0,	0,	0),		! global
	(kstaticsym,		$,			0,	0,	0,	0,	0),		! STATIC

	(kcastsym,			$,			0,	0,	0,	0,	1),		! CAST
	(compilervarsym,	$,			0,	0,	0,	0,	1),		! $lineno etc
	(dollarsym,			$,			0,	0,	0,	0,	1),		! to be used for current array upperbound; also tabledata names
	(kevalsym,			$,			0,	0,	0,	0,	0),		! EVAL
	(ktabledatasym,		$,			0,	0,	0,	0,	0),		! tabledata
	(kclampsym,			$,			0,	0,	0,	0,	1),			! CLAMP
	(kswapsym,			$,			0,	0,	0,	0,	0),		! SWAP
	(ksyscallsym,		$,			0,	0,	0,	0,	1),		! $getprocname etc
end

global enumdata []ichar headerdirnames =
	(hdr_module,		$),
	(hdr_import,		$),
	(hdr_sourcepath,	$),
	(hdr_linkdll,		$),
end

global enumdata [0:]ichar scopenames=
	(Module_scope=0,	"Local"), ! 		!module
	(subprog_scope,		"Global"), ! 		!inter-subprog
	(program_scope,		"Program"), ! 		!inter-module
	(export_scope,		"Export"), ! 		!inter-program
end

global enumdata =
	million_unit,
	billion_unit,
end

global enumdata [0:]ichar parammodenames=
	(byval_param=0,		"Byval "),
	(byref_param,		"Byref "),
end

global enumdata [0:]ichar namenames, [0:]byte name2pid =
	(nullid=0,		$,		0),				!Not assigned
	(programid,		$,		0),				!Main root
	(subprogid,		$,		0),
	(moduleid,		$,		program_id),	!Current or imported module
	(dllmoduleid,	$,		0),				!
	(typeid,		$,		0),				!Type name in type, proc or module
	(procid,		$,		proc_id),		!Proc/method/func/op name
	(dllprocid,		$,		import_id),		!Dll Proc/func name
	(dllvarid,		$,		0),				!Dll variable name
	(constid,		$,		0),				!Named constant in type, proc or module
	(staticid,		$,		static_id),		!Static in type or proc or module
	(frameid,		$,		local_id),		!Local var
	(paramid,		$,		param_id),		!Local param
	(fieldid,		$,		0),				!Field of Record or Class
	(labelid,		$,		label_id),		!Label name in proc only
	(macroid,		$,		0),				!Name of macro
	(macroparamid,	$,		0),				!Macro formal parameter name
	(linkid,		$,		0),				!Name in class defined in a base class
end

global enumdata []ichar propnames =
	(kksliceptr,	$),
	(kklen,			$),
	(kklwb,			$),
	(kkupb,			$),
	(kkbounds,		$),
	(kkbitwidth,	$),
	(kkbytesize,	$),
	(kktypestr,		$),
	(kkminval,		$),
	(kkmaxval,		$),
end

!!---
global tabledata []ichar stnames, []byte stsymbols, []i16 stsubcodes=

	("if",			kifsym,			jif),
	("then",		kthensym,		0),
	("elsif",		kelsifsym,		jif),
	("else",		kelsesym,		0),
	("dummyelse",	kelsesym,		1),
	("elsecase",	kelsecasesym,	jcase),
	("elseswitch",	kelseswitchsym,	jswitch),
	("case",		kcasesym,		jcase),
	("docase",		kdocasesym,		jdocase),
	("recase",		krecasesym,		jrecase),
	("when",		kwhensym,		0),
	("for",			kforsym,		0),
	("to",			ktosym,			0),
	("downto",		ktosym,			1),
	("by",			kbysym,			0),
	("do",			kdosym,			0),
	("end",			kendsym,		0),
	("while",		kwhilesym,		0),
	("repeat",		krepeatsym,		0),
	("until",		kuntilsym,		0),
	("return",		kreturnsym,		0),
	("stop",		kstopsym,		0),
	("redoloop",	kloopsym,		jredo),
	("nextloop",	kloopsym,		jnext),
	("exit",		kloopsym,		jexit),
	("goto",		kgotosym,		0),
	("switch",		kswitchsym,		jswitch),
	("doswitch",	kdoswitchsym,	jdoswitch),
	("doswitchu",	kdoswitchsym,	jdoswitchu),
	("doswitchx",	kdoswitchsym,	jdoswitchx),
	("tabledata",	ktabledatasym,	0),
	("enumdata",	ktabledatasym,	1),
	("clamp",		kclampsym,		0),
	("eval",		kevalsym,		0),
	("print",		kprintsym,		jprint),
	("println",		kprintsym,		jprintln),
	("fprint",		kprintsym,		jfprint),
	("fprintln",	kprintsym,		jfprintln),
!	("sprint",		ksprintsym,		jsprint),
!	("sfprint",		ksprintsym,		jsfprint),

	("cp",			kprintsym,		jprint),
	("cpl",			kprintsym,		jprintln),

	("read",		kreadsym,		jread),
	("readln",		kreadsym,		jreadln),
	("cast",		kcastsym,		jconvert),

	("function",	kfuncsym,	0),
	("func",		kfuncsym,	0),
	("proc",		kprocsym,		0),
	("fun",			kfuncsym,	1),
	("threadedproc",kprocsym,		2),

	("type",		ktypesym,		0),
	("record",		krecordsym,		0),
	("struct",		kstructsym,		0),
	("union",		kunionsym,		0),
	("ref",			krefsym,		0),
	("var",			kvarsym,		0),
	("let",			kletsym,		0),

	("include",		kincludesym,	0),
	("binclude",	kstrincludesym,	'B'),
	("sinclude",	kstrincludesym,	'S'),
	("strinclude",	kstrincludesym,	'S'),

	("macro",		kmacrosym,		0),

	("static",		kstaticsym,		0),
	
	("const",		kconstsym,		0),

	("$getnprocs",		ksyscallsym,	sf_getnprocs),
	("$getprocname",	ksyscallsym,	sf_getprocname),
	("$getprocaddr",	ksyscallsym,	sf_getprocaddr),

	("importdll",	kimportmodulesym,	0),
	("project",		kprojectsym,		0),
	("unless",		kunlesssym,			0),

	("global",		kglobalsym,		subprog_scope),
	("export",		kglobalsym,		export_scope),

	("swap",		kswapsym,		0),

	("void",		kvoidsym,		0),
	("int",			stdtypesym,		tint),
	("word",		stdtypesym,		tword),
	("real",		stdtypesym,		treal),

	("ichar",		kicharsym,		tc8),
	("ivoid",		kicharsym,		tvoid),

	("i8",			stdtypesym,		ti8),
	("i16",			stdtypesym,		ti16),
	("i32",			stdtypesym,		ti32),
	("i64",			stdtypesym,		ti64),

	("r32",			stdtypesym,		tr32),
	("r64",			stdtypesym,		tr64),

	("byte",		stdtypesym,		tu8),
	("u8",			stdtypesym,		tu8),
	("u16",			stdtypesym,		tu16),
	("u32",			stdtypesym,		tu32),
	("u64",			stdtypesym,		tu64),

!	("word8",		stdtypesym,		tu8),
!	("u16",		stdtypesym,		tu16),
!	("u32",		stdtypesym,		tu32),
!	("u64",		stdtypesym,		tu64),

	("char",		stdtypesym,		tc8),
	("c8",			stdtypesym,		tc8),
!	("char8",		stdtypesym,		tc8),
	("c64",			stdtypesym,		tc64),
!	("c64",		stdtypesym,		tc64),

	("bool64",		stdtypesym,		tbool64),
	("bool",		stdtypesym,		tbool64),
	("bool8",		stdtypesym,		tbool8),

	("label",		stdtypesym,		tlabel),

	("slice",		kslicesym,		tslice),

	("million",		unitnamesym,	million_unit),
	("billion",		unitnamesym,	billion_unit),

	("$lineno",		compilervarsym,	jcvlineno),
	("$strlineno",	compilervarsym,	jcvstrlineno),
	("$filename",	compilervarsym,	jcvfilename),
	("$modulename",	compilervarsym,	jcvmodulename),
	("$function",	compilervarsym,	jcvfunc),
	("$date",		compilervarsym,	jcvdate),
	("$time",		compilervarsym,	jcvtime),
	("$version",	compilervarsym,	jcvversion),
	("$typename",	compilervarsym,	jcvtypename),
!	("$targetbits",	compilervarsym,	jcvtargetbits),
!	("$targetsize",	compilervarsym,	jcvtargetsize),
!	("$targetname",	compilervarsym,	jcvtargetname),
!	("$targetcode",	compilervarsym,	jcvtargetcode),
	("nil",			compilervarsym,	jcvnil),
	("pi",			compilervarsym,	jcvpi),
	("true",		compilervarsym,	jcvtrue),
	("false",		compilervarsym,	jcvfalse),
	("infinity",	compilervarsym,	jcvinfinity),
	("$",			dollarsym,		0),

	("and",			andlsym,		0),
	("or",			orlsym,			0),
	("xor",			xorlsym,		0),
	("iand",		iandsym,		0),
	("ior",			iorsym,			0),
	("ixor",		ixorsym,		0),
!	("in",			insym,			kkin),
!	("notin",		notinsym,		kknotin),
	("in",			insym,			0),
	("notin",		notinsym,		1),
	("inrev",		inrevsym,		0),
	("rem",			iremsym,		0),
	("divrem",		idivremsym,		0),
	("min",			minsym,			0),
	("max",			maxsym,			0),

	("not",			notlsym,		0),
	("inot",		inotsym,		0),
	("istrue",		istruelsym,		0),
	("abs",			abssym,			kabs),
!	("$neg",		negsym,			0),
!	("byteswap",	byteswapsym,	0),

	("sqr",			sqrsym,			0),
	("sqrt",		sqrtsym,		0),
	("sign",		signsym,		0),

	("sin",			mathsopsym,		ksin),
	("cos",			mathsopsym,		kcos),
	("tan",			mathsopsym,		ktan),
	("asin",		mathsopsym,		kasin),
	("acos",		mathsopsym,		kacos),
	("atan",		mathsopsym,		katan),
	("log",			mathsopsym,		klog),
	("log10",		mathsopsym,		klog10),
	("exp",			mathsopsym,		kexp),
	("round",		mathsopsym,		kround),
	("floor",		mathsopsym,		kfloor),
	("ceil",		mathsopsym,		kceil),

	("atan2",		maths2opsym,	katan2),
	("fmod",		maths2opsym,	kfmod),

	("sliceptr",	propsym,		kksliceptr),
	("len",			propsym,		kklen),
	("lwb",			propsym,		kklwb),
	("upb",			propsym,		kkupb),
	("bounds",		propsym,		kkbounds),
	("bitwidth",	propsym,		kkbitwidth),
	("bytes",		propsym,		kkbytesize),
	("typestr",		propsym,		kktypestr),

	("msb",			bitfieldsym,	bf_msb),
	("lsb",			bitfieldsym,	bf_lsb),
	("msbit",		bitfieldsym,	bf_msbit),
	("lsbit",		bitfieldsym,	bf_lsbit),
	("msw",			bitfieldsym,	bf_msw),
	("lsw",			bitfieldsym,	bf_lsw),
	("odd",			bitfieldsym,	bf_odd),
	("even",		bitfieldsym,	bf_even),

	("fi",			kendsym,		kifsym),
	("esac",		kendsym,		kcasesym),
	("od",			kendsym,		kdosym),

	("$caligned",	atsym,			1),
	("clear",		kclearsym,		0),

	("module",		kheadersym,		hdr_module),
	("import",		kheadersym,		hdr_import),
	("$sourcepath",	kheadersym,		hdr_sourcepath),
	("linkdll",		kheadersym,		hdr_linkdll),
end

global enumdata [0:]ichar convnames, [0:]byte convtotcl =
	(kkerror=0,     $,		0),
!	(kktypepun,     $,		0),
	(kkfloat,       $,		kfloat),
	(kkfix,         $,		kfix),
	(kktruncate,    $,		ktruncate),
	(kkwiden,       $,		kwiden),
	(kkfwiden,      $,		kfwiden),
	(kkfnarrow,     $,		kfnarrow),
	(kksoftconv,    $,		0),
	(kktoboolt,     $,		ktoboolt),
	(kkharderr,     $,		0),
	(kksofttrun,    $,		0),
	(kkichar2sl,    $,		0),
	(kkax2slice,    $,		0),
	(kkcx2ichar,    $,		0),
end

global []int D_typestarterset= (stdtypesym,lsqsym,krefsym,krecordsym,
		kicharsym, kslicesym)

global [tr64..tc64, tr64..tc64]i16 softconvtable = (
!To: r64			r32			i64			u64			c64				 From:
	(kksoftconv,	kkfnarrow,	kkfix,		kkfix,		kkfix),			!r64
	(kkfwiden,		kksoftconv,	kkfix,		kkfix,		kkfix),			!r32
	(kkfloat,		kkfloat,	kksoftconv,	kksoftconv,	kksoftconv),	!i64
	(kkfloat,		kkfloat,	kksoftconv,	kksoftconv,	kksoftconv),	!u64
	(kkfloat,		kkfloat,	kksoftconv,	kksoftconv,	kksoftconv))	!c64

global [symbolnames.lwb..symbolnames.upb]byte endsexpr
global []byte exprendsymbols=(rbracksym,rsqsym,kthensym,kelsifsym,
			kelsesym, kuntilsym, kdosym, kendsym, commasym, barsym,
			semisym, ktosym)

global [jtagnames.lwb..jtagnames.upb]byte isbooltag

proc start=
	int genop, s,t, a, specop

	for i to exprendsymbols.len do
		endsexpr[exprendsymbols[i]]:=1
	od

	isbooltag[jcmp]:=1
	isbooltag[jcmpchain]:=1
	isbooltag[jandl]:=1
	isbooltag[jorl]:=1
	isbooltag[jnotl]:=1
	isbooltag[jistruel]:=1
	isbooltag[jisfalsel]:=1
	isbooltag[jinrange]:=1
	isbooltag[jinset]:=1
end

=== mm_type.m 0 0 39/40 ===
const nolv=0
const needlv=1

global const maxparams=100
const maxfields=200
int countedfields
int inassem
int inidata

!proc tpass(unit p, int t=tany, lv=nolv, hard=0)=
proc tpass(unit p, int t=tany, lv=nolv)=
	symbol d
	unit a,b,c, q
	int oldmmpos,m,nparams,paramtype,restype,amode
	static int depth

	if p=nil then return fi
	if depth=100 then
		txerror("TX looping detected")
	fi
	++depth

	oldmmpos:=mmpos

!CPL "TPASS------------------------", JTAGNAMES[P.TAG]

	mmpos:=p.pos

	a:=p.a
	b:=p.b
	c:=p.c

	p.resultflag:=t<>tvoid

	switch p.tag
	when jname then
		tx_name(p,t,lv)
	when jconst, jdecimal then

	when jtypeconst then
		p.mode:=ti64

	when jbytesize, jbitwidth then
		tpass(a)
		p.mode:=ti64

	when jbin, jcmp then
		tx_bin(p,a,b)

	when jin then
		tx_in(p,a,b)

	when junary then
		tx_unary(p,a)

	when jprop then
		tx_prop(p,a)

	when jbinto then
		tx_binto(p,a,b)

	when junaryto then
		tx_unaryto(p,a)

	when jassign then
		tx_assign(p,a,b,t)

	when jaddrof then
		if a.tag=jptr then
			deleteunit(p,a)
			deleteunit(p,p.a)
!			tpass(p,t,lv,hard)
			tpass(p,t,lv)
		else
			tpasslv(a)
			p.mode:=createrefmode(nil,a.mode)
		fi

	when jaddroffirst then
		tx_addroffirst(p,a,t)

	when jif then
		tx_if(p,a,b,c,t,lv)

	when jindex then
		tx_index(p,a,b,t,lv)

	when jptr then
		tx_ptr(p,a,t,lv)

	when jcall then
		tx_callproc(p,a,b,t)

	when jdot then
		tx_dot(p,a,b,lv)

	when jandl, jorl then
		tx_andl(p,a,b)

	when jnotl, jistruel, jisfalsel then
		tx_notl(p,a)

	when jconvert then
		tx_convert(p,a,1)

	when jtypepun then
		tx_typepun(p,a)

	when jincr then
		tx_incrto(p,a,t)

	when jmakerange then
		tx_makerange(p,a,b)

	when jswap then
		tx_swap(p,a,b)

	when jselect then
		tx_select(p,a,b,c,t,lv)

	when jswitch, jdoswitch, jdoswitchu, jdoswitchx then
		tx_switch(p,a,b,c,t,lv)

	when jcase, jdocase then
		tx_case(p,a,b,c,t,lv)

	when jdotindex, jdotslice then
		tx_dotindex(p,a,b,lv)

	when jslice then
		tx_slice(p,a,b)

	when jblock then
		tx_block(p,a,t,lv)

	when jeval then
		tpass(a,tany)

	when jdo then
		tpass(a,tvoid)

	when jreturn then
		tx_return(p,a,t)

	when jprint,jprintln,jfprint,jfprintln then

		tx_unitlist(a)
		fixchararray(a)

		while b do
			if b.tag=jfmtitem then
				tpass(c:=b.a)
				tpass(b.b,trefchar)
			else
				tpass(c:=b)
			fi
			fixchararray(c)
			b:=b.nextunit
		od
		tx_unitlist(p.c)

	when jforup, jfordown then
		tx_for(a,b,c)

	when jforall, jforallrev then
		tx_forall(a,b,c)

	when jto then
		tpass(a,ti64)
		tpass(b,tvoid)
		tpass(c,ti64)		!when autovar present

	when jautocast then
		tpass(a)
		if t=tany then txerror("cast() needs type") fi
		coerceunit(a,t,1)
		deleteunit(p,a)

	when jmakelist then
		tx_makelist(p,a,t,lv)

	when jstop then
		tpass(a,ti64)

	when jexit,jredo, jnext then
		tx_exit(p,a)

	when jgoto then
		tx_goto(p,a)

	when jlabeldef then

	when jwhile then
		tpass(a,tbool)
		if iscondtrue(a) then
			p.tag:=jdo
			p.a:=b
		elsif iscondfalse(a) then
			p.tag:=jnull
		fi
		tpass(b,tvoid)
		tpass(c,tvoid)

	when jrepeat then
		tpass(a,tvoid)
		tpass(b)
		if iscondtrue(b) or iscondfalse(b) then txerror("repeat/const cond") fi

	when jnogap, jspace then

	when jtypestr then
		tpass(a)
		if a.tag=jtypeconst then
			m:=a.value
		else
			tpass(a)
			m:=a.mode
		fi
		p.tag:=jconst
		p.mode:=trefchar
		p.a:=nil
		p.svalue:=pcm_copyheapstring(strmode(m,0))
		p.slength:=strlen(p.svalue)+1
		p.isastring:=1

	when jfmtitem then
		tpass(a)
		tpass(b)

	when jreadln then
		tpass(a)

	when jread then
		if a then
			tpass(a,tc64)
		fi
		if ttisinteger[t] or ttisreal[t] then
			t:=gettypebase(t)
		fi
		p.mode:=t
	when jrecase then
		if a then
			tpass(a,ti64)
			if a.tag<>jconst then
				txerror("recase must be const")
			fi
		fi

	when jcvfilename,jcvmodulename then
		p.mode:=trefchar

	when jbitfield then
		tx_bitfield(p,a,lv)

	when jsyscall then
		restype:=tvoid
		paramtype:=tvoid
		case p.fnindex
		when sf_getnprocs then restype:=ti64
		when sf_getprocname then paramtype:=ti64; restype:=trefchar
		when sf_getprocaddr then paramtype:=ti64; restype:=tref 
		esac

		if paramtype<>tvoid then
			if a=nil then txerror("sys: arg missing") fi
			tpass(a,paramtype)
			if a.nextunit then txerror("sys: too many args") fi
		elsif a then txerror("sys: too many args")
		fi

		p.mode:=restype

	when jcmpchain then
		tx_cmpchain(p,a)

	when jclear then
		tpasslv(a)
		case ttbasetype[a.mode]
		when trecord, tarray then
!CPL "CLEAR BLOCK"
		else
			txerror("Clear scalar?")
		esac


	when jshorten then

	when jstrinclude then
		tx_strinclude(p,a)

	when jmakeslice then
		tx_makeslice(p,a,b,t)

	when jmakeset then
		tx_makeset(p,a,t)

	when jsourceline then

	else
		CPL "TXUNIT: CAN'T DO:",jtagnames[p.tag]
	doelse:

		for i to jsubs[p.tag] do
			tx_unitlist(p.abc[i],t)
		od
	end switch

	tevaluate(p)

	case p.tag
	when jmakelist, jreturn then
	else
		if t<>tany and t<>tvoid and p.mode<>t then		!does not already match
!			coerceunit(p,t, hard)			!apply soft conversion
			coerceunit(p,t)			!apply soft conversion
		fi
	esac

	IF T=TVOID THEN
		CASE P.TAG
		WHEN JCONST, JBIN, JUNARY, JCMP THEN
!			TXERROR("Eval needed")
		WHEN JNAME THEN
			unless ttisref[p.mode] and tttarget[p.mode]=tlabel then
!				TXERROR("Eval needed2")
			end

		esac
	fi

	mmpos:=oldmmpos
	--depth
end

global proc tx_allprocs=
	ref procrec pp
	unit pcode

	pp:=proclist
	while pp do
		currproc:=pp.def
		pcode:=currproc.code

	    tpass(pcode,(currproc.nretvalues>1|ttuple|currproc.mode))

		case ttbasetype[currproc.mode]
		when tvoid then		!PROC
		when ttuple then	!MULT FN
		else				!REGULAR FN
			if pcode.tag<>jreturn then
!			if NOT CTARGET AND pcode.tag<>jreturn then
				insertunit(pcode,jreturn)
				pcode.mode:=currproc.mode
				pcode.resultflag:=1
			fi
		esac

		pp:=pp.nextproc
	od
end

proc tx_block(unit p,a, int t,lv)=
	while a and a.nextunit do
		tpass(a,tvoid)
		a:=a.nextunit
	od
	if a then
		tpass(a,t,lv)
		p.mode:=(t<>tvoid|a.mode|tvoid)
	fi
end

global proc tx_typetable=
	symbol d

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


	mmpos:=ttlineno[m]
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
		ttnamedef[m]:=ttnamedef[target]

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

!CPL "SETARRAYSIZE"

	pdim:=ttdimexpr[m]

	if pdim then
		a:=pdim.a
		b:=pdim.b
		rx_unit(ttowner[m],pdim)

		case pdim.tag
		when jmakerange then
			tpass(a)
			tpass(b)
			lower:=getconstint(a)
			length:=getconstint(b)-lower+1
		when jkeyvalue then
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

	if length<0 then txerror("Neg length") fi
	ttdimexpr[m]:=nil

	ttlower[m]:=lower
	ttlength[m]:=length
!CPL "SAS", LENGTH

	target:=tttarget[m]
	setmodesize(target)
	elemsize:=ttsize[tttarget[m]]
	ttsize[m]:=size:=length*elemsize
	ttsizeset[m]:=1

!CPL "=>",LENGTH

	checkblocktype(m)
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

global func tx_module(int n)int=
	modulerec m
	symbol d
	int globalflag,status

	currmoduleno:=n

	tx_passdef(modules[n].stmodule)

	return 1
end

global proc tx_passdef(symbol p)=
	symbol d
	int oldmmpos
	unit q

	if p.txdone then
		return
	fi

	oldmmpos:=mmpos
	mmpos:=p.pos

	d:=p.deflist
	while d do
		tx_passdef(d)
		d:=d.nextdef
	od

	q:=p.code

	case p.nameid
	when procid then
		currproc:=nil
		currproc:=nil
	when constid then
		tx_namedconst(p)
	when staticid, frameid, paramid then
		tx_namedef(p)
	esac

	p.txdone:=1
	mmpos:=oldmmpos
end

proc tx_unitlist(unit p, int t=tany, lv=nolv)=
	while p do
		tpass(p,t)
		p:=p.nextunit
	od
end

proc tx_namedef(symbol d)=
	int m,mold,inidataold, LENGTH
	unit dcode,pequiv

UNIT OLDDCODE

	if d.circflag then
		txerror("Circular reference detected")
	fi
	if d.txdone then return fi

	m:=d.mode
	setmodesize(m)

	dcode:=d.code

	d.circflag:=1

	if d.atvar then
		pequiv:=d.equivvar
		if pequiv.tag=jaddrof then deleteunit(pequiv,pequiv.a) fi
		if pequiv.tag<>jname then
			txerror("@name needed")
		fi
		tpass(pequiv)
	fi



	if dcode and d.nameid<>frameid then
		mold:=m
		m:=gettypebase(m)

		if ttbasetype[m]=tslice and dcode.tag=jconst and dcode.mode=trefchar then
			tpass(dcode,trefchar)
		else
			inidataold:=inidata
			inidata:=1
			tpass(dcode,m)
			inidata:=inidataold
		fi
		d.circflag:=0
		d.txdone:=1
		if ttbasetype[m]=tarray and ttlength[m]=0 then
			d.mode:=dcode.mode
		fi

		if mold<>m then
			if ttisinteger[m] and ttisshort[mold] then
				insertunit(d.code,jshorten)
				d.code.mode:=mold
			elsif mold=tr32 then
				d.code.mode:=mold
			fi
		fi

		if d.nameid=staticid then
			checkconstexpr(d.code)
		fi

	elsif dcode and d.nameid=frameid and ttbasetype[m]=tarray and ttlength[m]=0 then
		LENGTH:=-1
		IF DCODE.TAG=JMAKELIST THEN
			LENGTH:=DCODE.LENGTH
		FI

		tpass(dcode,m)
		d.mode:=dcode.mode
		d.circflag:=0
		d.txdone:=1

!this is bodge to get correct array size when it depends on data. Since it's
!done via an AV which is copied, but dimensions of that seem to be set later on.
!Length is set directly from the makelist construct
		if ttlength[m]=0 and length then
			ttlength[m]:=length
			ttsize[m]:=length*ttsize[tttarget[m]]
		fi

	else
		d.circflag:=0
		d.txdone:=1
	fi
end

global proc tx_namedconst(symbol d)=
	int m

	if d.circflag then
		txerror("Circular const reference detected")
	fi

	unit q
	if d.txdone then return fi
	q:=d.code

	m:=d.mode

	d.circflag:=1
	tpass(q,(m=tauto|tany|m))

	d.circflag:=0
	checkconstexpr(q)
	if m=tauto then
		d.mode:=q.mode
	fi

	case ttbasetype[d.mode]
	when tref then
		if d.mode<>trefchar then
			txerror("Bad const type")
		fi
	esac

	d.txdone:=1
end

proc checkconstexpr(unit p)=
!check whether p is const expr
	unit q
	int pmode

	case p.tag
	when jconst, jtypeconst then
		return
	when jmakelist then
		q:=p.a
		while q do
			checkconstexpr(q)
			q:=q.nextunit
		od

	when jconvert then
		if ttbasetype[p.a.mode]=tref then
			p.a.mode:=p.mode
			deleteunit(p,p.a)
		else
			goto error
		fi

	when jshorten then
		checkconstexpr(p.a)

	when jaddrof, jaddroffirst then
		case p.a.tag
		when jname then
		else
			goto error
		esac

	when jname then
		if p.def.nameid=fieldid then return fi
		if p.def.nameid=procid then return fi
		if p.def.nameid=labelid then return fi
		error
	else
	error:
		println =jtagnames[p.tag],STRMODE(P.MODE)
		PRINTUNIT(P)
		txerror("Getconstexpr: not const")
	esac
end

func getconstint(unit q)i64=
	checkconstexpr(q)

	if ttisinteger[q.mode] or q.tag=jtypeconst then
		return q.value
	elsif ttisreal[q.mode] then
		return q.xvalue
	else
		cpl strmode(q.mode)
		txerror("Getconstint: not i32/64")
	fi
	return 0
end

proc makenewconst(unit p,i64 x,int t=tvoid)=
!modify p (usually a binop, monop, convert op etc) to a new const unit
!p will usually already have the result mode
!the x value will do for int/word/real

	p.tag:=jconst
	p.a:=p.b:=nil
	p.value:=x
	p.isconst:=1
	if t<>tvoid then
		p.mode:=t
	fi
end

proc tx_name(unit p,int t,lv)=
	symbol d
	int oldmmpos
	unit pcode
	oldmmpos:=mmpos

IF P.TXCOUNT THEN
RETURN
FI
++P.TXCOUNT

	d:=p.def
	mmpos:=d.pos

	case d.nameid
	when constid then			!note: currently, rxpass converts names to constants

		if lv then txerror("&const") fi

		tx_namedconst(d)
		pcode:=d.code

		p.tag:=jconst
		p.def:=nil
		p.a:=nil
	    p.c:=nil

		if pcode.tag=jconvert then		!assume c_soft
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
!			println D.NAME,=LV,D.ISLET
			txerror_s("Can't use 'let' as lvalue: ",d.name)
		fi

		tx_namedef(d)

		if not inassem then
			p.mode:=d.mode
			if d.parammode=byref_param then
IF NOT P.INSPTR THEN
++P.INSPTR
				insertunit(p, jptr)
				p.mode:=tttarget[d.mode]
			fi
FI
			twiden(p,lv)
		else
			p.mode:=trefchar
		fi

	when procid,dllprocid then

		p.mode:=trefproc	!use generic refproc mode (yields return type of actual proc mode
				!after a call op, or actual refproc in other context. Don't use actual
				!refproc here, to avoid generating thousands of ref proc modes, one
				!for each call, that will never be needed

	when labelid then
		p.mode:=treflabel

	when moduleid then
		txerror_s("Module name can't be used on it's own: #",d.name)

	when fieldid then
		p.tag:=jconst
		p.def:=nil
		p.a:=nil
	    p.c:=nil

		p.value:=d.offset

		p.mode:=ti64
		p.isconst:=1


	when typeid then
		p.tag:=jtypeconst
		p.value:=d.mode
		p.mode:=ti64

	when dllvarid then
		if d.code then
			txerror("Can't init dllvar")
		fi
		p.mode:=d.mode

	else
		mmpos:=p.pos
		txerror_ss("TNAME? # #",namenames[d.nameid],d.name)
	esac
	mmpos:=oldmmpos

end

proc tx_bin(unit p,a,b)=
!deal with most binary ops
	unit q
	int amode,bmode,abase,bbase,cmode, resmode, relop, simpleset

	tpass(a)
	tpass(b)
	amode:=a.mode
	bmode:=b.mode

	case p.tclop
	when kadd then
		if dobinnumx(p,a,b) then return fi
		if ttisref[amode] then
			if ttisref[bmode] and a.isastring and b.isastring then
				combinestrings(p)
				return
			fi
			if isnum(bmode) then
				coerceunit(b,ti64)
				p.tclop:=kaddpx
				p.mode:=amode
				return
			fi
		fi

	when ksub then
		if dobinnumx(p,a,b) then return fi
		if ttisref[amode] then
			if ttisref[bmode] then
				if comparemodes(amode, bmode) then
					p.tclop:=ksubp
					p.mode:=ti64
					return
				else
					txerror("ref-ref: not compat")
				fi
			fi
			if isnum(bmode) then
				coerceunit(b,ti64)
				p.tclop:=ksubpx
				p.mode:=amode
				return
			fi

		fi

	when kmul then
		if dobinnumx(p,a,b) then return fi
		if ttisref[amode] then
			if a.isastring and ttisinteger[b.mode] and b.tag=jconst then
				mulstrings(p)
				return
			fi
		fi


	when kdiv then
		if isnumi(amode) and isnumi(bmode) then p.tclop:=kidiv; goto doidiv fi
		if dobinnumf(p,a,b) then return fi
		if isnum(amode) and isnum(bmode) then
			p.mode:=tr64
			coerceunit(a,tr64)
			coerceunit(b,tr64)
			return
		fi

	when kidiv, kirem, kidivrem, kbitand, kbitor, kbitxor then
doidiv:
		if dobinnumi(p,a,b) then return fi

	when kmin, kmax then
		if dobinnumx(p,a,b) then return fi

	when kpower then
		if dobinnumx(p,a,b) then return fi

	when kfmod, katan2 then
		coerceunit(a,tr64)
		coerceunit(b,tr64)
		p.mode:=tr64
		return

	when kshl, kshr then
		if isnumi(amode) then
			coerceunit(b,ti64)
			p.mode:=amode
			return
		fi

	elsif p.tclcond then
		if dobinnumx(p,a,b) then
			p.mode:=tbool
			return
		fi
		p.mode:=tbool
		if ttisref[amode] and ttisref[bmode] then
			if not comparemodes(amode, bmode) then
				txerror("Cmp ref/ref not compat")
			fi
			return
		fi
		if p.tclcond in [eq_cc, ne_cc] then
			if comparemodes(amode, bmode) then
				return
			fi
		fi

	else
		txerror("txbin?")
	esac

cpl tclnames[p.tclop]
	TXERROR_SS("BIN/CAN'T RESOLVE MODES",strmode(amode),strmode2(bmode))
end

proc tx_binto(unit p,a,b)=
	int abase, bbase, amode,bmode, opc

	tpasslv(a)
	tpass(b)

	amode:=a.mode
	bmode:=b.mode

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if p.tclop=kdivto and ttisinteger[abase] then
		p.tclop:=kidivto
	fi

	p.mode:=tvoid

	case p.tclop
	when kaddto then				!ref+ref not allowed; or ref+int (later refchar+refchar)
		if abase=tref and bbase=tref then
			txerror("to:ref+ref")
		fi
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.tclop:=kaddpxto
			return
		fi
	when ksubto then				!ref-int
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.tclop:=ksubpxto
			return
		fi
	when kshlto, kshrto, kbitandto, kbitorto, kbitxorto then
		coerceunit(b,ti64)
		return
	esac

	if isnum(abase) and isnum(bbase) then	!num op num
		coerceunit(b,abase)

	elsif ttisshort[abase] and isnum(bbase) then
		coerceunit(b,abase)

	else
		if not comparemodes(amode,bmode) then
			txerror_ss("BIN: modes not compatible: # #",strmode(amode),strmode(bmode))
		fi
	fi
end

func getdominantmode(int amode,bmode)int=
	int abase,bbase

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if isnum(abase) and isnum(bbase) then
		return min(abase,bbase)
	fi
	if not comparemodes(amode, bmode) then
		txerror("Getdom: no dominant mode")
	fi
	return amode
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

		p.cmptclmode[i]:=gettclmode(u)
	od

	p.mode:=ti64
!	p.mode:=tbool
end

proc tx_callproc (unit p,a,pargs,int t)=
!deal with both callproc and callfn (perhaps calldll too)
	unit q
	symbol d,e,pm
	[maxparams]symbol paramlist
	[maxparams]unit arglist,newarglist
	int nparams,i,j,k,nargs,m,kwdused,qm, ismproc
	ichar name

	tpass(a)

	nargs:=nparams:=0
	ismproc:=0

	retry:

	case a.tag
	when jname then
		d:=a.def

		if d.nameid in [procid, dllprocid] then
			ismproc:=d.nameid=procid
getparams:
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
				insertunit(a,jptr)
				a.mode:=tttarget[a.mode]
			od
			goto dorefproc
		fi

	when jif,jselect,jblock then
		TXERROR("Can't do ifx/func")

	else
	dorefproc:
		if a.tag=jdot then
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

	p.mode:=d.mode				!type returned by func (will be void for procs)

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
		case q.tag
		when jkeyword then
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

		else
!doregparam:
			if kwdused then
				txerror("Normal param follows kwd")
			fi
			if k>=nparams then
				cpl =k, =nparams
				txerror("Too many params supplied")
			fi
			newarglist[++k]:=(q.tag=jnull|nil|q)
		esac
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
				newarglist[i]:=duplunit(pm.code,p.pos)
			else
				newarglist[i]:=createconstunit(0,ti64)
			fi
		fi
	od

!final pass: do type-pass on each param, and apply any conversion
!I also need to build a new argument list for the call unit
	unit ulist:=nil, ulistx:=nil

	for i to nparams do
		pm:=paramlist[i]
		q:=newarglist[i]

		if pm.parammode=byref_param then
			tpass(q,m:=tttarget[pm.mode],needlv)
			qm:=q.mode

			if not comparemodes(qm,m) then
				txerror_ss("&param: type mismatch",strmode(qm), strmode(m))
			fi

!			UNLESS CTARGET AND Q.TAG=JCONVERT THEN

				insertunit(q,jaddrof)
				q.mode:=pm.mode
!			ELSE
!				Q.TAG:=JADDROF
!				Q.MODE:=PM.MODE
!			END

		else
			tpass(q,pm.mode)
		fi

		addlistunit(ulist, ulistx, q)
		q.nextunit:=nil
	od
	p.b:=ulist
end

proc tx_unary(unit p,a)=
	int opc,size,amode,mbase,tmax,x,xhigh, resmode

	tpass(a)

	amode:=a.mode
	resmode:=amode

	case p.tclop
	when katan, klog, klog10, kexp, ksqrt,ksin,kcos,ktan, kasin, kacos,
			kfloor, kceil then
		coerceunit(a,tr64)
		resmode:=tr64

	when kneg, kabs, ksqr then
		txerror("not num") when not isnum(amode)

	when kbitnot, knot, ktoboolt then
		txerror("toboolt") when not isint(amode)

!	when ksliceptr then
!		tx_sliceptr(p,a)
!		return
	when ksign then
		resmode:=ti64

	ELSE
		CPL "TTT", TCLNAMES[P.TCLOP]
	esac

	p.mode:=resmode
end

proc tx_prop(unit p,a)=
	int opc,size,amode,mbase,tmax,x,xhigh, resmode

	tpass(a)

	amode:=a.mode
	resmode:=amode

	case p.propcode
	when kklwb, kkupb, kklen, kkbounds, kksliceptr then
		do_bounds(p,a)
		return

	when kkbytesize,kkbitwidth then
		size:=ttsize[(a.tag=jtypeconst|a.value|a.mode)]*(p.propcode=kkbytesize|1|8)
		makenewconst(p,size)
		resmode:=ti64

	when kkminval, kkmaxval then
		resmode:=ti64
		if a.tag=jtypeconst then
			mbase:=ttbasetype[a.value]
		else
			mbase:=ttbasetype[a.mode]
		fi

		if p.propcode=kkminval then
			case mbase
			when ti8 then x:=-128
			when ti16 then x:=-32768
			when ti32 then x:=-2_147_483_648
			when ti64 then x:=i64.min
			when tu8,tu16,tu32,tu64,tc8,tc64 then x:=0
			else
 	           txerror_s("Can't do minvalue on #",strmode(mbase))
			esac
		else
			case mbase
			when ti8 then x:=127
			when ti16 then x:=32767
			when ti32 then x:=2_147_483_647
			when ti64 then x:=0x7fff'ffff'ffff'ffff
			when tu8,tc8 then x:=255
			when tu16 then x:=65535
			when tu32 then x:=4294967295
			when tu64 then x:=0; --x; resmode:=tu64
			else
				txerror_s("Can't do maxvalue on #",strmode(mbase))
			esac
		fi
		p.tag:=jconst
		p.a:=nil
		p.value:=x
		p.isconst:=1

	when kktypestr then
		p.tag:=jconst
		if a.tag=jtypeconst then
			amode:=a.value
		else
			amode:=a.mode
		fi

		p.mode:=trefchar
		p.svalue:=pcm_copyheapstring(strmode(amode))
		p.isastring:=1
		p.length:=strlen(p.svalue)
		return

	ELSE
		CPL "PROP", TCLNAMES[P.TCLOP]
	esac

	p.mode:=resmode
end

proc tx_unaryto(unit p,a)=
	tpasslv(a)

	case p.tclop
	when kbitnotto, knotto, ktoboolto then
		txerror("Not int") when not isint(a.mode)
	esac

	p.mode:=tvoid
end

proc tx_if(unit p,pcond,plist,pelse, int t,lv) =
	unit pc:=pcond, pl:=plist
	int u

	u:=tvoid
	if t<>tany then u:=t fi

	while pc, (pc:=pc.nextunit; pl:=pl.nextunit) do
		tpass(pc)
		tpass(pl,t,lv)

		if t=tany then
			if u=tvoid then
				u:=pl.mode
			elsif lv then
				if not comparemodes(u, pl.mode) then
					txerror("IF/LV?")
				fi
			else
				u:=getdominantmode(u,pl.mode)
			fi
		fi
	od

	if t<>tvoid and pelse=nil then
		txerror("else needed")
	fi
	tpass(pelse,t,lv)
	if t=tany then
		if lv then
			if not comparemodes(u, pelse.mode) then
				txerror("IF/LV2?")
			else
				u:=getdominantmode(u,pelse.mode)
			fi
		fi
	fi

	if t<>tvoid then
		pl:=plist
		while pl, pl:=pl.nextunit do
			if t=tany then
				coerceunit(pl,u)
			fi
		od
		if t=tany then
			coerceunit(pelse,u)
		fi
		p.mode:=u
	fi

	if pcond.nextunit=plist.nextunit=nil then
		if iscondtrue(pcond) then		!branch b only
			deleteunit(p,plist)
		elsif iscondfalse(pcond) then	!branch c only
			if pelse=nil then
				pelse:=createunit0(jblock)
			fi
			deleteunit(p,pelse)
		fi
	fi
end

proc tx_incrto(unit p,a,int t)=
	tpasslv(a)

	if t<>tvoid then
		case p.tclop
		when kincrto then p.tclop:=kincrload
		when kdecrto then p.tclop:=kdecrload
		esac
		p.mode:=a.mode
	else				!a++ a-- to ++a --a
		case p.tclop
		when kloadincr then p.tclop:=kincrto
		when kloaddecr then p.tclop:=kdecrto
		esac
		p.mode:=tvoid
	fi

	twiden(p,0)
end

proc tx_for(unit pindex,pfrom,pbody)=
	unit pto, pstep, plocal, plist
	int u

	pto:=pfrom.nextunit
	pstep:=pto.nextunit

	tpass(pindex)
	if pindex.tag<>jname then
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

	tpass(a,ti64)
	tpass(b,ti64)

	amode:=a.mode
	bmode:=b.mode

	coerceunit(a,ti64)
	coerceunit(b,ti64)
	p.mode:=trange
end

proc tx_ptr(unit p,a,int t,lv)=
	symbol d

	tpass(a)

	case ttbasetype[a.mode]
	when tvoid then
		txerror("Deref Void")
	when tref then
		p.mode:=tttarget[a.mode]

	when tslice then
		CPL "DEREF SLICE"
	else
		txerror("PTR: need ref T")
	esac

	twiden(p,lv)
end

proc setrecordsize(int m)=
	[maxfields+8]symbol fieldlist
	int i,nfields,indent,nrfields,size,index, maxalign
	symbol d,e
	ref char flags
	const ss='S', ee='E'
	int flag
	static int depth


	if ttsize[m] then return fi
	if ++depth>10 then serror("Recursive record?") fi

	d:=ttnamedef[m]
	e:=d.deflist
	nfields:=0

	fieldlist[++nfields]:=symbol(ss)

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
				fieldlist[++nfields]:=symbol(flag)
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
					fieldlist[++nfields]:=symbol(ee)
				else
					exit
				esac
			od
		fi

		e:=e.nextdef
	od

	fieldlist[++nfields]:=symbol(ee)
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

	checkblocktype(m)
	--depth
end

proc checkblocktype(int m)=
	case ttsize[m]
	when 1,2,4,8 then
		ttisblock[m]:=0
	esac
end

proc scanrecord(int state,ref[]symbol fields, int &index, &isize, offset, calign, &maxalign)=
 	symbol e,f,ea
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

			elsif f.atfield then
				bitoffset:=0
				e:=f.equivfield
				fieldsize:=0
				f.offset:=e.offset+f.equivoffset
			else
				bitoffset:=0
				if state='S' then ++countedfields fi
				fieldsize:=ttsize[f.mode]
				if calign then
					alignment:=getalignment(f.mode)
!CPL "CALIGN", =FIELDSIZE, =ALIGNMENT, =MAXALIGN, =STRMODE(F.MODE), =TTSIZE[F.MODE]
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

func roundoffset(int offset, alignment)int=
	int mask

	if alignment=1 then return offset fi
	mask:=alignment-1
	while offset iand mask do ++offset od

	return offset
end

proc tx_convert(unit p,a,int hard=0)=
	case a.tag
	when jmakelist then
		tx_makelist(a,a.a,p.convmode,nolv)
	else
!CPL "TX CONVERT"
		tpass(a)
		coerceunit(a,p.convmode,hard)
!!NEW:
!		tpass(a, p.convmode, hard:hard)
!!		coerceunit(a,p.convmode,hard)
	esac
	deleteunit(p,a)			!get rid of this convert (may be replaced by new convert unit)
end

proc tx_makelist(unit p,a, int t,lv)=
	int alength,tlength,elemtype,newt, i, nfields,isconst, m
	unit q,b
	symbol e

	alength:=p.length
	newt:=0
	isconst:=1

	tlength:=ttlength[t]

	if tlength then
		if alength<tlength then
			txerror_ss("Too few elements",strint(alength), strint(tlength))
		elsif alength>tlength then
			txerror_ss("Too many elements",strint(alength), strint(tlength))
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

			unless q.tag=jconst then isconst:=0 end
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

				unless q.tag=jconst then isconst:=0 end
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
		p.resultflag:=1

	when tslice then
CPL "TSLICE"

	else
		txerror_s("Unknown makelist type: #",strmode(t))
	esac

	p.isconst:=isconst

	tpass(p.b,ti64)				!lower


IF P.TAG<>JMAKESLICE THEN

	if not inidata and isconst then
		e:=getavname(currproc,staticid)
		e.mode:=t
		addstatic(e)
		q:=createunit0(jnone)
		q^:=p^
		e.code:=q
		p.tag:=jname
		p.def:=e
	fi
FI
end

proc tx_makeslicefromlist(unit p,a, int t)=
	CPL "MAKESLICE/TX"

	TXERROR("MAKESLICE FROM LIST NOT READY")
end

proc tx_makeslice(unit p, a,b, int t)=
	CPL "MAKESLICE/TX"
	tpass(a)

	if ttbasetype[a.mode]<>tref then txerror("slice init not ref") fi
	if tttarget[a.mode]<>tvoid then
		if not comparemodes(a.mode,createrefmode(nil,tTtarget[t])) then
			txerror("slice/ptr mismatch")
		fi
	fi

	tpass(b,ti64)
	p.mode:=t
CPL "MKSLICE2"
	p.resultflag:=1
end

proc tx_makeset(unit p,a, int t)=
	p.isconst:=1

	while a, a:=a.nextunit do
		tpass(a)

		if not a.isconst then
			p.isconst:=0
		fi
	od

	p.mode:=tvoid
end

proc tx_dot(unit p,a,b,int lv)=
	int recmode,recbasemode,i,j,newtag,tmode
	unit q,pindex
	symbol d,dequiv

	tpass(a)			!lhs, yeields ref array type

	recmode:=a.mode

	recbasemode:=ttbasetype[recmode]

	while recbasemode=tref do
		tmode:=tttarget[recmode]
		insertunit(a,jptr)
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
			newtag:=jdotindex
		else						!bit slice
			pindex:=createunit2(jmakerange,createconstunit(i,ti64),createconstunit(j,ti64))
			pindex.mode:=trange
			pindex.a.resultflag:=1
			pindex.b.resultflag:=1
			newtag:=jdotslice
		fi

		p.mode:=b.mode
		twiden(p,lv)
		insertunit(p,newtag)
		p.mode:=tu64
		p.b:=pindex
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

func resolvefield(symbol d, int m)symbol=
	symbol e,t

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
	tpass(a,tbool)
	tpass(b,tbool)

	p.mode:=tbool
end

proc convintconst(unit p,i64 x)=
!convert unit p into int const x
	p.tag:=jconst
	p.mode:=ti64
	p.a:=p.b:=p.c:=nil
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
	int av, bv

	tpasslv(a)
	tpasslv(b)

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

	if p.tag=jdocase and lv then gerror("&docase") fi

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
			if w.tag=jmakerange then
				unless ttisinteger[amode] then txerror("case: need int index") end
			else
				if amode=tany then
						if not isbooltag[w.tag] then
							TXERROR("CASE/BOOL?")
							insertunit(w,jistruel)
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
	p.mode:=tbool
end

proc tx_typepun(unit p,a)=
	int smode,tmode

	case a.tag
	when jmakelist then
		TXERROR("TYPEPUN/LIST")
	else
		tpass(a)

		smode:=ttbasetype[a.mode]
		tmode:=ttbasetype[p.convmode]

!CPL =STRMODE(SMODE)
!CPL =STRMODE(TMODE)

		case smode
		when tr64 then
			if tmode in [ti64, tu64] then
			else
				error
			fi
		when ti64, tu64 then
			case tmode
			when tr64 then
			when tr32 then
			else
				error
			esac

		when tr32 then
!CPL "SMODE=TR32", STRMODE(TMODE)
			case tmode
			when ti32 then tmode:=ti64			!will be widened to 64 bits
			when tu32 then tmode:=tu64
			else
error:			txerror("Typepun: invalid")
			esac
		esac

		p.mode:=tmode

!CPL "------",STRMODE(P.MODE), STRMODE(P.CONVMODE), strmode(a.mode)
!PRINTUNIT(P)
	esac
end

proc tx_exit(unit p,a)=
	if a=nil then return fi
	tpass(a,ti64)
	if a.tag<>jconst then
		txerror("exit/etc not const")
	fi
	p.index:=a.value
	p.a:=nil
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

	if p.tag=jdoswitch and lv then gerror("&doswitch") fi

	if p.tag=jdoswitchx then
		tpass(a)
		tpass(a.nextunit)
		if ttbasetype[a.mode]<>tref then txerror("not ref") fi
	else
		tpass(a,ti64)
	fi

	memset(&valueset,0,valueset.bytes)
	u:=tvoid

	wt:=b
	while wt do

		w:=wt.a
		while w do
			tpass(w)

			if not isconstunit(w) then
				PRINTUNIT(W)
				txerror("Switch not constant")
			fi

			case ttbasetype[w.mode]
			when trange then			!assume makerange
				ax:=w.a.value
				bx:=w.b.value
	dorange:
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
				if w.tag<>jconst then
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
	if a.tag=jname then
		a.addroffirst:=1
	fi
	p.mode:=m
end

proc tx_return(unit p,a, int t)=
 	int m,nvalues,nret,i
	ref[]i32 pmult
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

	if a.tag=jmakelist then
		a.tag:=jreturnmult
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
		P.RESULTFLAG:=1

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
		if ttisreal[a.mode] then
			insertunit(a,jtypepun)
			a.mode:=a.convmode:=tu64
			a.resultflag:=1

		else
			txerror("a.[i]: not int/str value")
		fi
	fi

	tpass(b)			!index

	case ttbasetype[b.mode]
	when trange then
		i:=b.a
		j:=b.b
		if i.tag=j.tag=jconst then
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

proc twiden(unit p, int lv)=
!intended for widening narrow types for memory access nodes Name, Index, Dot, Ptr.
!But will also be used to generally apply
	int m,u,mbase

	mbase:=ttbasetype[m:=p.mode]

	if mbase=tvoid then return fi		!nothing to widen (error?)
	if lv then return fi				!lv, keep memory mode as dest

	if not ttisshort[mbase] then return fi	!no widening needed

	case p.tag
	when jname, jptr, jindex, jdot, jcall, jincr then
		insertunit(p, jconvert)
		p.convcode:=kkwiden
		p.convmode:=m
		p.mode:=gettypebase(m)
	else
		PRINTUNIT(P)
		txerror_s("widen? #",jtagnames[p.tag])
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
	insertunit(p,jslice)


	if p.a.tag=jconst then
	else
		b:=duplunit(p.a)
		insertunit(b,junary)
		prange:=createunit2(jmakerange,createconstunit(1,ti64),b)

		prange.mode:=trange
		p.b:=prange
	fi

	p.mode:=slicemode
end

proc tx_bitfield(unit p,a,int lv)=
	int i,j,bitsize,topbit
	unit r

	tpass(a,,lv)

	if not ttisinteger[a.mode] and not ttisref[a.mode] then
		if ttisreal[a.mode] then
			insertunit(a,jtypepun)
			a.mode:=a.convmode:=tu64
			a.resultflag:=1
		else
			txerror("Int/ref needed")
		fi
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
		p.tag:=jdotindex
		p.b:=createconstunit(i,ti64)
		p.resultflag:=1
		p.b.resultflag:=1

		if p.bitopindex=bf_even then
			p.mode:=tu64
			addnotl(p)
		fi

	else
		r:=createunit2(jmakerange,createconstunit(i,ti64),createconstunit(j,ti64))
		r.a.resultflag:=1
		r.b.resultflag:=1
		r.mode:=trange
		p.tag:=jdotslice
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

		insertunit(a,jptr)
		a.mode:=tmode

		abasemode:=ttbasetype[a.mode]
	od

end

proc tmethodcall(unit p, pdot, pargs)=
	int mrec
	unit prec, pfield, pfunc
	symbol d,e

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

proc do_bounds(unit p,a) =
	int m,mbase,opc,lower,upper

	deref(a)

	m:=a.mode
	if a.tag=jtypeconst then m:=a.value fi

	mbase:=ttbasetype[m]
	p.mode:=ti64

	case p.tclop
	when kklwb then
		case mbase
		when tarray,tslice then
			convintconst(p,ttlower[m])
			return
		else
error:
			txerror_s("lwb/upb/len?",strmode(m))
		esac

	when kkupb then
		case mbase
		when tarray then
			convintconst(p,ttlower[m]+ttlength[m]-1)
		when tslice then
			p.tag:=junary			!code gen needs to look at type, and use .propcode
		else
			goto error
		esac

	when kklen then
		case mbase
		when tarray then
			convintconst(p,ttlength[m])
		when tslice then
			p.tag:=junary
!			p.tclop:=klen
		else
			goto error
		esac
	when kkbounds then
		p.mode:=trange
		case mbase
		when tarray then
			p.range_lower:=ttlower[m]
			p.range_upper:=p.range_lower+ttlength[m]-1
			p.tag:=jconst
			p.a:=p.b:=p.c:=nil
			p.isconst:=1
			return

		when tslice then
		else
			goto error
		esac
	when kksliceptr then
		if mbase<>tslice then txerror("Not slice") fi
		p.tag:=junary

	esac
end

proc addnotl(unit p)=
	insertunit(p,jnotl)
	p.mode:=tbool
	p.tclop:=knot
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
	when jmakerange then
		a:=p.a
		b:=p.b
		if ttsize[a.mode]<=8 then			!const range only for 32-bits
			tevaluate(a)
			tevaluate(b)
			if a.tag=jconst and b.tag=jconst then
				p.isconst:=a.isconst iand b.isconst
			fi
		fi

	when jaddrof then
!		IF NOT CTARGET THEN
			a:=p.a

			pname:=addrdotindex(a, offset)

			if pname then
				deleteunit(a,pname)
				if p.b=nil then
					p.b:=createconstunit(offset,ti64)
					p.b.resultflag:=1
				else 
					p.b.value+:=offset
				fi
			fi
!		FI
	fi

end

func addrdotindex(unit p, int &offset)unit q=
	int axmode

	case p.tag
	when jdot then
		if p.a.tag=jname then
			offset:=p.offset
			return p.a
		else
			q:=addrdotindex(p.a,offset)
			offset+:=p.offset
			return q
		fi
	when jindex then
		axmode:=p.a.mode
		if p.b.tag=jconst then
			if p.a.tag=jname then
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
	i64 a,b,c,offset
	real x,y,z
	unit lhs, rhs

	lhs:=p.a
	rhs:=p.b

	unless lhs.tag=rhs.tag=jconst then
!		if lhs.tag=jaddrof and rhs.tag=jconst AND P.TCLOP=KADDREFX then		!ASSUME ADD/SUBREFX
		if lhs.tag=jaddrof and rhs.tag=jconst then		!ASSUME ADD/SUBREFX
			if lhs.a.tag=jname then			!reduce addrof(a)+k => addrof(a,k)
				offset:=rhs.value*ttsize[tttarget[lhs.mode]]
				if p.tclop=ksubpx then
					offset:=-offset
				fi
				if lhs.b=nil then
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

		case p.tclop
		when kadd then c:=a+b
		when ksub then c:=a-b
		when kmul then c:=a*b
		when kidiv then
			if b=0 then txerror("x/0") fi
			c:=a/b
		when kirem then
			if b=0 then txerror("x rem 0") fi
			c:=a rem b
		when kshl then c:=a<<b

!		when keq then c:=a=b
!		when kne then c:=a<>b
!		when klt then c:=a<b
!		when kle then c:=a<=b
!		when kge then c:=a>=b
!		when kgt then c:=a>b

		when kbitand then c:=a iand b
		when kbitor then c:=a ior b
		when kpower then c:=a ** b
		else
			return
		end

	when tr64,tr32 then

		case p.tclop
		when kadd then z:=x+y
		when ksub then z:=x-y
		when kmul then z:=x*y
!		when kdiv then z:=x/y
		when kpower then z:=x**y

		else
			return
		end
	else
		return
	esac
!
	if ttisreal[p.mode] then
		makenewconst(p,i64@(z))
	else
		makenewconst(p,c)
	fi
end

proc tevalmonop(unit p)=
	i64 a,b,c
	real x,z

	unless p.a.tag=jconst then
		return
	end

	a:=p.a.value
	x:=p.a.xvalue

	case p.mode
	when ti64, tu64 then

		if p.tag in [jistruel, jisfalsel] then dobool fi

		case p.tclop
		when kneg then c:=-a

!		when ktoboolt then
!
!CPL "EVALMONO/XXTOBOOLT1"
!
! c:=istrue a; p.mode:=tbool
		when knot then c:=not a; p.mode:=tbool
		when kbitnot then c:=inot a
		when kabs then c:=abs a

		else
			return
		esac
	when tr64, tr32 then
		case p.tclop
		when kneg then z:=-x
		when katan then z:=atan(x)
		when ksqrt then z:=sqrt(x)

		else
			return
		esac

	when tbool then

dobool:
		case p.tag
		when jistruel then c:=istrue a; p.mode:=tbool
		when jisfalsel then c:=not a; p.mode:=tbool
		elsecase p.tclop
		when ktoboolt then c:=istrue a; p.mode:=tbool
		when knot then c:=not a; p.mode:=tbool
		esac
	else
		return
	esac

	if ttisreal[p.mode] then
		makenewconst(p,i64@(z))
	else
		makenewconst(p,c)
	fi
end

func iscondtrue(unit p)int =
	p.tag=jconst and p.value<>0
end

func iscondfalse(unit p)int =
	p.tag=jconst and p.value=0
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
	int alen:=a.slength
	int blen:=b.slength
	int clen, needterm
	byte atype:=a.strtype, btype:=b.strtype, ctype
	ichar s

	if atype=btype='B' then
		needterm:=0
		ctype:='B'
	elsif atype='B' or btype='B' then
		txerror("Mixed str+bin strings")
	else					!both are string/strdata
		--alen				!lose zero terminator
		--blen

		needterm:=1
		if atype='S' or btype='S' then		!either strdata then both are
			ctype:='S'
		else
			ctype:=0
		fi
	fi
	clen:=alen+blen

	if blen=0 then
		deleteunit(p,a)
		return
	elsif alen=0 then
		deleteunit(p,b)
		return
	fi

	s:=pcm_alloc(clen+needterm)
	memcpy(s,a.svalue,alen)
	memcpy(s+alen,b.svalue,blen)
	if needterm then
		(s+clen)^:=0
	fi

	deleteunit(p,a)
	p.slength:=clen+needterm
	p.svalue:=s
	p.strtype:=atype
end

proc mulstrings(unit p)=
!p is (add, a, b) where a and b are string constants.
	unit a:=p.a, b:=p.b
	int alen:=a.slength
	int scale:=b.value
	int clen, needterm
	byte atype:=a.strtype, ctype
	ichar s, t

	--alen				!lose zero terminator

	needterm:=1
	if atype='S' then needterm:=1 fi

	clen:=alen*scale
	if scale<1 or clen<1 or clen>100000 or alen<1 then txerror("mulstr") fi

	t:=s:=pcm_alloc(clen+needterm)
	to scale do
		memcpy(t,a.svalue,alen)
		t+:=alen
	od
	if needterm then
		(s+clen)^:=0
	fi

	deleteunit(p,a)
	p.slength:=clen+needterm
	p.svalue:=s
	p.strtype:=atype
end

proc tx_strinclude(unit p,a)=
	int fileno
	ifile pf

	tpass(a)
	if a.tag<>jconst or not a.isastring then
		txerror("strincl/not string")
	fi

!CPL "TX STRINCLUDE", A.SVALUE, CURRPROC.NAME

	fileno:=modules[p.moduleno].fileno

	pf:=getsupportfile(a.svalue,path:sources[fileno].path)

	a.svalue:=pf.text
	a.slength:=pf.size+1
	a.strtype:=p.strtype

	if a.strtype='B' then				!string
		--a.slength						!there will already be zero-terminator
	fi
!
!CPL "DONE STRINCL",A.STRTYPE
	deleteunit(p,a)
end

proc coerceunit(unit p, int t, hard=0)=
	int opc, s:=p.mode, n

	if t=tvoid or s=t then return fi
	if s=tvoid then
		txerror("Void expression/return value missing")
	fi

	if s=t then return fi

	int sbase:=ttbasetype[s]
	int tbase:=ttbasetype[t]

	opc:=kkerror
	int starg:=tttarget[s]
	int ttarg:=tttarget[t]

	if s=trefchar then sbase:=trefchar fi
	if t=trefchar then tbase:=trefchar fi

	if sbase in tfirstnum..tlastnum then
		if tbase in tfirstnum..tlastnum then
			opc:=softconvtable[sbase,tbase]
		elsecase tbase
		when tref, trefchar then
			opc:=kksoftconv
checkhard:
			if not hard then opc:=kkharderr fi
		elsif tbase in tfirstshort..tlastshort then
			if ttisinteger[sbase] then
				if not hard then				!needed for idata init
					opc:=kksofttrun
				else
					opc:=kktruncate
				fi
			fi
		elsecase tbase
		when tbool then
			opc:=kktoboolt
		when ttype then
			opc:=kksoftconv
		fi

	elsecase sbase
	when tbool then
		if tbase in [ti64, tu64] then
			opc:=kksoftconv
		fi

	when tref then
		case tbase
		when ti64, tu64 then
			opc:=kksoftconv
			checkhard
		when tref then
			if starg=tvoid or ttarg=tvoid then			!at least one is ref void
				opc:=kksoftconv
			else
checkref:
				opc:=kksoftconv
				if not comparemodes(s,t) then
					checkhard
				fi
			fi
		when trefchar then
			checkref
		when tbool then
			opc:=kktoboolt
		end

	when trefchar then
		case tbase
		when ti64,tu64 then
			opc:=kksoftconv
			checkhard
		when tref then
			if comparemodes(s,t) or hard then
				opc:=kksoftconv
			else
				opc:=kkharderr
			fi
		when tbool then
			opc:=kktoboolt
		when tslice then
!			if ttarg not in [tc8, tu8] then
			if ttarg in [tc8, tu8] then
				opc:=kkichar2sl
			fi
		when tarray then
			if p.tag=jconst and p.strtype then
				opc:=kksoftconv
				n:=ttlength[t]
				if n=0 then
					ttlength[t]:=p.slength/ttsize[tttarget[p.mode]]
					ttsize[t]:=p.slength
				else
					txerror("Array not empty")
				fi
			fi

		end

	when tarray then
		case tbase
		when tarray then
			if comparemodes(s,t) then
				opc:=kksoftconv
			fi
		when tslice then
			if comparemodes(starg, ttarg) then
				opc:=kkax2slice
			fi

		when trefchar then
			if starg in [tc8, tu8] then
				opc:=kkcx2ichar
			fi
		when tref then
			if ttarg=tvoid then
				opc:=kkcx2ichar
			fi
		esac

	when tslice then
		case tbase
		when tslice then
			if comparemodes(s,t) then
				opc:=kksoftconv
			fi
		when tref then
			if ttarg=tvoid or comparemodes(starg, ttarg) then
GERROR("COERCE/SLICEPTR")
!				opc:=ksliceptr
			fi

		esac

	when ttype then
		if tbase<=tlastnum then
			opc:=kksoftconv

		fi
	fi

	applyconversion(p,s,t,opc)
end

proc applyconversion(unit p, int s,t, opc)=
!deal with conversion op applied to p:
! do nothing
! report error
! insert special node
! attempt compile-time conversion
! insert convert node
! set p's mode etc

	case opc
	when kkerror then
		txerror_ss("Can't do conversion: # => #",strmode(s),strmode2(t))

	when kkharderr then
		txerror_ss("Need explicit cast: # => #",strmode(s),strmode2(t))

	when kksoftconv then
		p.mode:=t
		return
	when kksofttrun then
		if tevalconvert(p,s,t,opc) then
			return
		fi
		insertunit(p,jshorten)
		p.mode:=t			!don't use the short target mode
		return

	when kkax2slice then
		insertunit(p,jslice)
		p.mode:=t
		return
	when kkichar2sl then
		tstringslice(p,t)
		return

	when kkcx2ichar then
		insertunit(p,jaddroffirst)
		p.mode:=trefchar
		return
	esac

	if tevalconvert(p,s,t,opc) then		!try and apply it directly
		return
	fi

!have to add an explict conversion node
	insertunit(p, jconvert)
	p.tclop:=opc

	p.convmode:=s
	p.resultflag:=1

!???
	if ttisshort[t] then
		p.convmode:=t
		t:=gettypebase(t)
	fi

	p.mode:=t
end

proc checkmodes(int s,t)=
	if not comparemodes(s,t) then
		txerror_ss("Type-compare error: # <-> #",strmode(s), strmode2(t))
	fi
end

func comparemodes(int s,t)int=
!return 1 if modes s,t are compatible. That is, ref s/ref t would be interchangeable.
!a direct compare may be false because refs/arrays but be constructed at
!different times
	int sbase, tbase, starg, ttarg
	symbol d,e

	if s=t then return 1 fi

	sbase:=ttbasetype[s]
	tbase:=ttbasetype[t]
	starg:=tttarget[s]
	ttarg:=tttarget[t]

	if sbase=tbase then
		case sbase
		when tref then
			if starg=tvoid or ttarg=tvoid then
				return 1
			fi
			return comparemodes(starg,ttarg)

		when tarray then
			if not comparemodes(starg, ttarg) then return 0 fi
			if ttlength[s]=ttlength[t] or ttlength[s]=0 or ttlength[t]=0 then
				return 1
			fi
		when tslice then
			return comparemodes(starg, ttarg)

		when tproc then
			d:=ttnamedef[s]
			e:=ttnamedef[t]
			if d and e then
				if not comparemodes(d.mode,e.mode) then return 0 fi
				if d.paramlist=nil and e.paramlist=nil then return 1 fi
			fi
		esac

	elsif sbase=tc8 and tbase=tu8 or sbase=tu8 and tbase=tc8 then
		return 1
	else
!else needs complex param/result-matching
!...
	fi
	return 0
end

func tevalconvert(unit p,int s,t,opc)int=
!conversion op opc to convert from s to t is about to be applied to be
!try and do that at compile time to avoid adding a runtime conversion
!return 1 if it could apply it, 0 if it couldn't
!caller should have already evaluated p to reduce constants etc
	real x,z
	int a,c,sbase,tbase
!
	if p.tag<>jconst then
		return 0
	fi
	a:=p.value
	x:=p.xvalue

	case pr(s,    t)
	when pr(ti64, tr64), pr(ti64, tr32) then
		z:=a

	when pr(tr64, ti64) then
		c:=x

	when pr(tr64, tr32) then
		Z:=X

	when pr(ti64, tu8) then
		c:=byte(a)
	when pr(ti64, ti16) then
		c:=i16(a)

	else
		if ttisinteger[s] and ttisinteger[t] and ttsize[s]=ttsize[t] then
			c:=a
		else
			sbase:=ttbasetype[s]
			tbase:=ttbasetype[t]
			if sbase=tbase then return 1 fi
			return 0
		fi
	esac

	if ttisreal[t] then
		makenewconst(p,i64@(z),t)

	else
		makenewconst(p,c,t)
	fi

	return 1
end

proc tx_assign(unit p,a,b,int t)=
	int m,mm,needres:=t<>tvoid
	symbol d

	case a.tag
	when jmakelist then
		if b.tag=jmakelist then
			if needres then txerror("Mult assign has no result") fi
			tx_assignmultmult(p,a,b)
		else
			tx_assignmultscalar(p,a,b,t)
		fi
		return
	when jdotindex, jdotslice then
		tx_dotindex(a,a.a,a.b,needlv)
		tpass(b,a.mode)
		p.mode:=ti64
		return
	esac

	if a.tag=jname and a.def.islet and p.initlet then
		tpass(a)
	else
		tpasslv(a)
	fi
	m:=a.mode

	a.resultflag:=needres

	if ttbasetype[m]=tslice and b.tag=jmakelist then
		tx_makeslicefromlist(b,b.a,m)
		p.mode:=m

	else
		if b.tclop in [kidiv, kirem] then		!CAN'T JUST OVERRIDE MODE
			tpass(b)
		elsif b.tag=jread then
			tpass(b,m)
		else
			mm:=m
			if ttisshort[m] then
				mm:=gettypebase(m)
			fi
			case b.tag
			when jautocast then
				tpass(b,mm)
			when jmakelist then
				tpass(b,m)
			else
				tpass(b,mm)
			esac
			p.mode:=mm


!Eliminate widening when lhs is not wider than rhs (and when an Widen conversion is used
!which implies that rhs is < 8 bytes)
			STATIC INT NN

			if b.tag=jconvert and b.convcode=kkwiden and
				 ttsize[a.mode]<=ttsize[b.convmode] and not needres then
				DELETEUNIT(B, B.A)
			fi

		fi
	fi
end

proc tx_assignmultmult(unit pp,a,b)=
!mult:=mult
	unit p,q,lhs,rhs

	pp.tag:=jassignmm

	if a.length<>b.length then
		txerror("Mult assign: count mismatch")
	fi
	if a.length=0 then
		txerror("Invalid assignment")
	fi
	rhs:=b.a
	lhs:=a.a

	p:=lhs
	while p, p:=p.nextunit do
		tpasslv(p)
	od

	p:=lhs

	q:=rhs
	while q, (p:=p.nextunit; q:=q.nextunit) do
		tpass(q,p.mode)
	od
end

proc tx_assignmultscalar(unit pp,a,b,int t)=
!assign 'scalar' to mult LHS, but it might be a tuple type or be an expandable one
	unit p,q, alist:=a.a
	int nretmodes,i, alength:=a.length
	ref[]i32 pmult
	symbol d				!point to def containing return mode info

	nretmodes:=0
	pp.tag:=jassignms

	tpass(b,tany)

	case ttbasetype[b.mode]
	when ttuple then
		d:=getprocretmodes(b)
		nretmodes:=d.nretvalues

		if ttbasetype[d.mode]<>ttuple then txerror("Not a tuple") fi

		if alength>nretmodes then
			txerror("mult ass/mult returns don't agree in number")
		fi
		if nretmodes<=1 then
			txerror("mult ass rhs needs fn yielding 2+ values")
		fi

		p:=alist
		pmult:=ttmult[d.mode]
		i:=1

		while p, p:=p.nextunit do
			tpasslv(p,pmult[i++])
		od
	when tslice then
		if alength<>2 then txerror("(a,b):=slice") fi
		tpasslv(alist,createrefmode(nil, tttarget[b.mode]))
		tpasslv(alist.nextunit,ti64)

	when trange then
	when trecord then

	elsif b.tag=jbin and b.tclop=kidivrem then
		if alength<>2 then txerror("(a,b):=divrem") fi
		tpasslv(alist,b.mode)
		tpasslv(alist.nextunit,b.mode)
		pp.tag:=jassignmdrem

	else
		txerror_s("Can't expand to mult values:",strmode(b.mode))
	esac

	pp.mode:=t
end

proc tpasslv(unit p, int t=tany)=
!process p as lvalue, but require it to be of type t
!however no conversion is done (not allowed); only a compare is done
	tpass(p,,needlv)
	if t not in [tany, tvoid] then
		if not comparemodes(p.mode, t) then
			txerror_ss("PassLV type mismatch: #:=#",strmode(p.mode), strmode2(t))
		fi
	fi
end

func dobinnumx(unit p,a,b)int=
!Try and apply this to binary tclopnds:
!	NUMX	NUMX	DOM
!a and b have already been processed, but not coerced to any type yet

	int amode:=a.mode, bmode:=b.mode, cmode

	if isnum(amode) and isnum(bmode) then
		p.mode:=cmode:=min(amode, bmode)
		coerceunit(a,cmode)
		coerceunit(b,cmode)
		return 1
	fi

	if isnum(amode) and isbool(bmode) then
		p.mode:=amode
		coerceunit(b,amode)
		return 1
	elsif isbool(amode) and isnum(bmode) then
		p.mode:=bmode
		coerceunit(a,bmode)
		return 1
	fi


	return 0
end

func dobinnumf(unit p,a,b)int=
!Try and apply this to binary tclopnds:
!	NUMF	NUMF	DOM
	int amode:=a.mode, bmode:=b.mode, cmode

!	if amode=ti64 then coerceunit(a, tr64); amode:=tr64 fi
!	if bmode=ti64 then coerceunit(b, tr64); bmode:=tr64 fi

	if isnumf(amode) and isnumf(bmode) then
		p.mode:=cmode:=min(amode, bmode)
		coerceunit(a,cmode)
		coerceunit(b,cmode)
		return 1
	fi
	return 0
end

func dobinnumi(unit p,a,b)int=
!Try and apply this to binary tclopnds:
!	NUMI	NUMI	DOM
	int amode:=a.mode, bmode:=b.mode, cmode

	if isnumi(amode) and isnumi(bmode) then
		p.mode:=cmode:=min(amode, bmode)
		coerceunit(a,cmode)
		coerceunit(b,cmode)
		return 1
	fi
	return 0
end

func tx_in(unit p,a,b)int=
	int simpleset, amode, bmode
	unit q

	tpass(a)
	tpass(b)
	amode:=a.mode
	bmode:=b.mode

	coerceunit(a,ti64)

	simpleset:=1
	if b.tag=jmakeset then
		q:=b.a
		while q, q:=q.nextunit do
			if not ttisinteger[q.mode] then
				simpleset:=0
				exit
			fi
		od
	fi

	if isnum(a.mode) and b.tag in [jmakerange, jmakeset] and simpleset then
		p.tag:=(b.tag=jmakerange|jinrange|jinset)
	else
		txerror("doin")
	fi
	p.mode:=tbool

!	if p.tclop=kknotin then
	if p.inv then
		addnotl(p)
	fi
	return 1
end
=== mm_help.txt 0 1 40/40 ===
M Compiler for 64-bit Windows

Normal use:           Compiles lead module prog.m to:

    mm      prog      prog.exe (default)
    mm -r   prog      in-memory native code then execute
    mm -i   prog      in-memory IL then interpret

    mm -exe prog      prog.exe
    mm -dll prog      prog.dll
    mm -obj prog      prog.obj
    mm -a   prog      prog.asm
    mm -n   prog      prog.nasm
    mm -mx  prog      prog.mx
    mm -p   prog      prog.pcl (textual IL)
    mm -ma   prog     prog.ma (single amalgamated source file)

Other options:

    -ext              Used std headers external to compiler
    -opt              Optimise native code
    -out:file         Name output file (extension can be added)
    -rip              Use RIP address modes
    -himem            Generate PIC code (automatic with -obj/-dll)
    @file             Read files and options from a file
=== END ===
1 mm.m 0 0
2 tcl.m 0 0
3 tc_api.m 0 0
4 tc_decls.m 0 0
5 tc_diags.m 0 0
6 tc_tables.m 0 0
7 mc_decls_x.m 0 0
8 mc_lib_x.m 0 0
9 mc_asm_x.m 0 0
10 mc_gen_xb.m 0 0
11 mc_aux_xb.m 0 0
12 mc_conv_xb.m 0 0
13 mc_temp_xb.m 0 0
14 mc_objdecls.m 0 0
15 mc_genss.m 0 0
16 mc_writeexe.m 0 0
17 mc_writess.m 0 0
18 mc_disasm.m 0 0
19 mx_decls.m 0 0
20 mx_run.m 0 0
21 mx_lib.m 0 0
22 mx_write.m 0 0
23 mx_show.m 0 0
24 mm_cli.m 0 0
25 mm_gentcl.m 0 0
26 mm_libtcl.m 0 0
27 mm_blocktcl.m 0 0
28 mm_decls.m 0 0
29 mm_diags.m 0 0
30 mm_export_dummy.m 0 0
31 mm_lex.m 0 0
32 mm_lib.m 0 0
33 mm_libsources_dummy.m 0 0
34 mm_modules.m 0 0
35 mm_name.m 0 0
36 mm_parse.m 0 0
37 mm_support.m 0 0
38 mm_tables.m 0 0
39 mm_type.m 0 0
40 mm_help.txt 0 1
