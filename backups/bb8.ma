=== MA 74 ===
=== mc.m 0 0 1/74 ===
!project =

	module mm_decls

	module mm_cli				! Command line interface

	module mm_lex				! Lexer, produces tokens
	module mm_parse				! Parser, produces ST, TT and AST1
	module mm_name				! Name resolution, AST1 to AST2
	module mm_type				! Type analysis, AST2 to AST3

	module mm_diags				! diagnostics
!	module mm_diags_dummy
!
	module mm_export_dummy		! Write exports files

!Embedded SYSLIB sources
!	module mm_libsources		!Embedded Syslib sources
	module mm_libsources_dummy
!
	module mm_modules			! Module handling

	module mm_support			! Source file handling

	module mm_tables			! Enumerations
	module mm_lib				! Support functions

!AST to PCL IL
	module mm_genc				! ST/AST -> PST/PCL
!	module mm_blockc			! AST -> PCL
	module mm_libc				!

!PCL Support Library

	module pcl_decls			! Data structures, vars, enums and tables
	module pcl_api				! PCL-generating API and support routines
	module pcl_diags			! Dump PCL code by function
!!
!!!!PCL Interpreter
!	module pcl_run				! Fixups and dispatch loop
!
!
!!PCL->MCL Inbetween modules
!	module mcl_cdecls			! Data structures, vars, enums, and tables
!	module mcl_decls			! Data structures, vars, enums, and tables
!	module mcl_cgen
!	module mcl_caux
!	module mcl_clib
!	module mcl_genaux			! Other support routines
!	module mcl_stack			! PCL stack management (and some diags)
!	module mcl_block			! PCL to MCL per functions
!	module mcl_blockaux			! Support routines for PCL to MCL
!!
!!!MCL Support Library
!	module mcx_decls			! Data structures, vars, enums, and tables
!	module mcx_asm				! Dump generated MCL as ASM source code
!
!	module mcx_lib				! Low-level MCL-generating API and other support
!
!	module mcx_pedecls			! For PE format + some extra records
!	module mcx_optim			! Peephole MCL Optimiser
!
!	module mcx_gas				! Dump generated MCL as GAS source code
!
!	module mcx_genss			! MCL to SS binary code and data
!	module mcx_exe				!
!
!!MCL Run In Memory
!	module mcr_decls
!	module mcr_run
!	module mcr_lib
!	module mcr_write
!



!MCL Uncomment these lines when no MCL modules presen

!global const pclpresent=0
global const pclpresent=1
!global const mclpresent=0
!global const ctarget=0
!global const assemtype=0
!global proc genmcl=end
!global fun writeasm:^void = nil


global const clpass=1
=== mclib.m 0 0 2/74 ===
export type filehandle=ref void

importdll $cstd=
	func malloc		(u64)ref void
	func realloc	(ref void, word)ref void
	proc free		(ref void)
	proc memset		(ref void, i32, word)
	proc memcpy		(ref void, ref void, word)
	proc memmove		(ref void, ref void, word)
	func clock		:i32
	func ftell		(filehandle)i32
	func fseek		(filehandle, i32, i32)i32
	func fread		(ref void, word, word, filehandle)word
	func fwrite		(ref void, word, word, filehandle)word
	func getc		(filehandle)i32
	func ungetc		(i32, filehandle)i32
	func fopen		(ichar a, b="rb")filehandle
	func fclose		(filehandle)i32
	func fgets		(ichar, int, filehandle)ichar
	func remove		(ichar)i32
	func rename		(ichar, ichar)i32
	func getchar	:i32
	proc putchar	(i32)
	proc setbuf		(filehandle, ref byte)

	func strlen		(ichar)int
	func strcpy		(ichar, ichar)ichar
	func strcmp		(ichar, ichar)i32
	func strncmp	(ichar, ichar, word)i32
	func strncpy	(ichar, ichar, word)word
	func memcmp		(ref void, ref void, word)i32
	func strcat		(ichar, ichar)ichar
	func tolower	(i32)i32
	func toupper	(i32)i32
	func isalpha	(i32)i32
	func isupper	(i32)i32
	func islower	(i32)i32
	func isalnum	(i32)i32
	func isspace	(i32)i32
	func strstr		(ichar, ichar)ichar
	func atol		(ichar)int
	func atoi		(ichar)i32
	func strtod		(ichar,ref ref char)r64
	func _strdup	(ichar)ichar

	func puts		(ichar)i32
	func printf		(ichar, ...)i32

	func sprintf	(ichar, ichar, ...)i32

	func sscanf		(ichar, ichar, ...)i32
	func scanf		(ichar, ...)i32

	func rand		:i32
	proc srand		(u32)
	func system		(ichar)i32

	func fgetc		(filehandle)i32
	func fputc		(i32,  filehandle)i32
	func fprintf	(filehandle, ichar, ...)i32
	func fputs		(ichar,  filehandle)i32
	func feof		(filehandle)i32
	func getch		:i32
	func _getch		:i32
	func kbhit		:i32
	func _mkdir		(ichar)i32
	func mkdir		(ichar)i32
	func strchr		(ichar,i32)ichar

	func _setmode	(i32,i32)i32

	proc _exit		(i32)
	proc "exit"		(i32)
!	proc `exit		(i32)
	func pow		(real,real)real

	func `sin 		(real)real
	func `cos		(real)real
	func `tan		(real)real
	func `asin		(real)real
	func `acos		(real)real
	func `atan 		(real)real
	func `log		(real)real
	func `log10		(real)real
	func `exp		(real)real
	func `floor		(real)real
	func `ceil		(real)real

	proc  qsort   	(ref void, u64, u64, ref proc)

end

export macro strdup=_strdup

importdll $cstdextra=
	func __getmainargs	(ref i32, ref void, ref void, int, ref void)i32
end

export const c_eof		=-1
export const seek_set	= 0
export const seek_curr	= 1
export const seek_end	= 2
=== mcl_block.m 0 0 3/74 ===
!global const fshowinlinepcl=1
global const fshowinlinepcl=0

!global const fshowopndstack=1
global const fshowopndstack=0

!global const docalltrace=1
!global const docalltrace=0

GLOBAL INT DEBUG

!global int frameoffset, paramoffset
global int framebytes

[6]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
[6]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

global [pclnames.bounds]^proc(pcl) px_handlertable

!global macro ispint(m) = pstdint[m]
!global macro ispfloat(m) = stdfloat[m]
!global macro ispwide(m) = stdwide[m]
!
global proc convertpcl(pcl p)=
	mcl lastmcl
!	pcl q
!
!	PRINTLN "    CONV",PCLNAMES[P.OPCODE], CURRFUNC.NAME

!	CPL STROPNDSTACK()

	doshowpcl(p) when fshowinlinepcl

	pmode:=p.mode
	lastmcl:=mccodex

	px_handlertable[p.opcode]^(p)

	oldregset:=regset

	if mccodex<>lastmcl then				!don't update is no mcl generated
		mccodex.freedset := regset ixor pclset
		lastmcl:=mccodex
	end


	regset:=pclset
	showopndstack() when fshowopndstack and currpcl.opcode not in [klabel, kcomment]

!clear all work regs including ones holding active temps
end

proc px_nop*(pcl p) =
!nop is sometimes used to replace deleted instructions like useless jumps

!	unimpl(p)
end

proc px_load*(pcl p) =
	pushpcl(p)
end

proc px_store*(pcl p) =

	storeopnd(p, zz)
	poppcl()
end

proc px_iload*(pcl p) =
! Z' := Z^
	mclopnd ax, px
	pcl nextpcl
	int reg

	if pmode<>tblock then
			px:=getopnd_ind(zz)
		nextpcl:=currpcl.next

		if nextpcl.opcode=kwiden and not nextpcl.istrunc then
!MCOMMENT("ILOAD/WIDEN")
			ax:=gwrm(nextpcl.mode, getsharereg(nextpcl.mode, px))

			genmc(ploadop[nextpcl.mode2], ax, px)
			setnewzz(nextpcl.mode, ax.reg)
			currpcl:=nextpcl
		else
			ax:=gwrm(pmode, getsharereg(pmode, px))
			genmc(m_mov, ax, px)
			setnewzz(pmode, ax.reg)
		end

	else
		px:=getopnd_ind_simp(zz)
		ax:=gwrm(tu64, px.reg)
		dolea(ax, px)
	end
end

proc px_iloadx*(pcl p) =
! Z' := (Y + Z*s + d)^
	pcl z, nextpcl
	mclopnd ax, bx, px, fx

	px:=do_addrmode(p)

	if pmode=tblock then
		ax:=gwrm(tu64, px.reg)
		dolea(ax, px)
		poppcl()
		setnewzz(tu64, ax.reg)

	else
		nextpcl:=currpcl.next

		if nextpcl.opcode=kwiden and not nextpcl.istrunc then
!MCOMMENT("ILOAD/WIDEN2")
			ax:=gwrm(nextpcl.mode, getsharereg(nextpcl.mode, px))

			genmc(ploadop[nextpcl.mode2], ax, px)
			poppcl()
			setnewzz(nextpcl.mode, ax.reg)
			currpcl:=nextpcl
		else
			ax:=gwrm(pmode, getsharereg(pmode, px))

			genmc(m_mov, ax, px)
			poppcl()
			setnewzz(pmode, ax.reg)
		end

	end
end

proc px_istore*(pcl p) =
! Y^ := Z
	mclopnd bx, px
	int reg

	bx:=loadopnd(yy, pmode)				!rhs to store into lhs

	if reg:=pclstack[zz].reg then
		px:=mgenireg(reg, pmode)
	else
		px:=getopnd_ind(zz, pmode, mem:1)
	end

	if pmode=tblock then
		px:=makesimpleaddr(px)
		bx:=makeopndind(bx, tu64)

		copyblock(px, bx, p.size)

	else
		genmc(m_mov, px, bx)
	end

	poppcl()
	poppcl()
end

proc px_istorex*(pcl p) =
! (Y + Z*s + d)^ := X
	mclopnd ax, cx, px
	pcl z

	cx:=loadopnd(xx, pmode)			!rhs
	px:=do_addrmode(p)

	if pmode=tblock then
		px:=makesimpleaddr(px)
		cx:=makeopndind(cx, tu64)
		copyblock(px, cx, p.size)

	else
		genmc(m_mov, px, cx)

	end

	poppcl()
	poppcl()
	poppcl()
end

proc px_storem*(pcl p) =
	mclopnd ax, bx, px
	pcl z
	psymbol dblock

MERROR("STOREM NOT COMPLETE")

	if p.size<>16 then merror("Storem not 16") fi		!only Y/Z for now

	dblock:=newblocktemp(16)

	px:=mgenmem(dblock)
!
	bx:=loadopnd(zz, tu64)
!
	genmc(m_mov, applyoffset(px, 8), bx)
	poppcl()
!
	bx:=loadopnd(zz, tu64)
	genmc(m_mov, px, bx)

	genmc(m_lea, mgenreg(bx.reg, tu64), px)
	setnewzz(tu64, bx.reg)
end

proc px_dupl*(pcl p) =
!	unimpl(p)
	duplpcl()
end

proc px_double*(pcl p) =
	if ncalldepth then
		duplpcl()
	else
		++pclstack[noperands].count
	end
end

proc px_swapstk*(pcl p) =
! (Z', Y') := (Z, Y)
	swapopnds(noperands-p.x+1, noperands-p.y+1)
end

proc px_unload*(pcl p) =
	poppcl()
end

proc px_type*(pcl p) =
	unimpl(p)
end

proc px_callp*(pcl p) =
	int nargs, nregargs, slots, isptr:=0, shadow:=0

	int blockret:=callblockret[ncalldepth]

	nargs:=p.nargs+blockret
	nregargs:=min(nargs, 4)

	if p.opcode in [kicallp, kicallf] then
		isptr:=1
	end

	do_pushlowargs(nregargs, p.nvariadics, isptr)

	slots:=0
	if nargs<=4 then
		if mstackdepth then
			slots+:=4
			pushslots(4)					!shadowspace
			SLOTS+:=CALLALIGN[NCALLDEPTH]
		else
			localshadow:=1
		end

	else
		slots:=nargs+callalign[ncalldepth]
		pushslots(4)						!shadowspace
	end

	if isptr then
		genmc(m_call, loadopnd(zz, tu64))
		poppcl()
	else
		genmc(m_call, genmemaddr(p.def))
	end

	to nregargs-BLOCKRET do
!CPL "CALL/POP", =NOPERANDS, =PCLSET:"H"
!CPL STROPNDSTACK()
		poppcl()
	end

	if slots then
		popslots(slots)
	end

	if pmode then
		do_getretvalue(p)
	end

	--ncalldepth
end

proc px_retproc*(pcl p) =
	if mclprocentry=mccodex then		!empty body: add dummy mcl op
		mcomment("---")				!injection of entry code goes wrong otherwise
	end
end

proc px_retfn*(pcl p) =
	mclopnd ax, bx

	if pmode=tblock then
		bx:=genireg(r0)								!r0 points to local block value
		regset.[r0]:=1
		ax:=gwrm()
		genmc(m_mov, ax, genmem(blockretname))
		ax:=genireg(ax.reg)
		copyblock(ax, bx, p.size)
		genmc(m_mov, mgenreg(r0, tu64), mgenmem(blockretname))
	end

	px_retproc(p)
end

proc px_jump*(pcl p) =
	int labno:=p.labelno
	pcl q:=p.next

	while q.opcode=kcomment do q:=q.next end
	case q.opcode
	when klabel then
		if q.labelno=labno then return end
		q:=q.next
		if q.opcode=klabel and q.labelno=labno then return end
	when kjump then
		q.opcode:=knop
	end case

	genmc(m_jmp, genlabel(labno))
end

proc px_ijump*(pcl p) =
	genmc(m_jmp, getopnd(zz, tu64))
	poppcl()
end

proc px_jumpcc*(pcl p) =
! goto L when Y c Z; p=1: Z':=Y (b=0/1)
	int mcond
	mclopnd ax, bx, lx
	pcl z

	mcond:=ucondcodes[p.condcode]
	lx:=genlabel(p.labelno)

	if pmode=tblock then
MERROR("JUMPCC/BLOCK")
!		addimm(p.size)
!		swapopnds(1, 3)
!		domaths(nil, "memcmp*", 3)
!		genmc(m_cmp, mgenreg(r0, tp32), mgenint(0))
!		genmc_cond(m_jmpcc, mcond, lx)

	else

		ax:=loadopnd(yy)

		if ispint(pmode) then
			if (z:=isimmint(zz)) and z.value=0 and p.condcode in [eq_cc, ne_cc] then
				genmc(m_test, ax, ax)
			else
				bx:=getopnd(zz)
				if pstdsigned[pmode] then
					mcond:=scondcodes[p.condcode]
				end
				genmc(m_cmp, ax, bx)
			end
		else
			bx:=getopnd(zz)
!MCOMM("A")
			genmc(m_comiss, ax, bx)
		end

		genmc_cond(m_jmpcc, mcond, lx)
		poppcl()

		unless p.popone then
			poppcl()
		end
	end
end

proc px_jumpt*(pcl p) =
! goto L when Z is true
	do_jumptruefalse(p, nz_cond)
end

proc px_jumpf*(pcl p) =
! goto L when Z is false
	do_jumptruefalse(p, z_cond)
end

proc px_jumpret*(pcl p) =
! goto L, common return point; deal with any ret value on stack

	if pmode=tvoid or noperands=0 then
		merror("Jumpret/no-arg")
	end

	loadparam(zz, pmode, (pstdfloat[pmode]|xr0|r0))
	poppcl()

	px_jump(p)
end

proc px_jumpretm*(pcl p) =
	int n, reg

	n:=zz-p.nargs+1
	to p.nargs do
		loadopnd(n, pclstack[n].mode)
		++n
	od

	reg:=r0
	to p.nargs do
		if reg=r3 and ispint(pclstack[zz].mode) then reg:=r10 fi
		movetoreg(reg)
		poppcl()
		++reg
	od

	px_jump(p)
end

proc px_setcc*(pcl p) =
! Z' := Y cc Z
	int cond
	mclopnd ax, bx, cx

	AX:=LOADOPND(YY, PMODE)
	bx:=getopnd(zz, pmode)
	cond:=ucondcodes[p.condcode]

	if pmode=tblock then
		merror("setcc/block")

	elsif ispint(pmode) then
		if pstdsigned[pmode] then
			cond:=scondcodes[p.condcode]
		end
		genmc(m_cmp, ax, bx)
		cx:=changeopndsize(ax, 1)

	else
		genmc(m_comiss, ax, bx)

		cx:=gwrm(tu8)
		setnewzz(ti64, cx.reg)
		swapopnds(yy, zz)
	end

	genmc_cond(m_setcc, cond, cx)
	genmc(m_movzx, changeopndsize(cx, 4), cx)

	poppcl()
end

proc px_iswap*(pcl p) =
	mclopnd ax, bx

	mclopnd px:=getopnd_ind(yy, mem:1)
	mclopnd qx:=getopnd_ind(zz, mem:1)

	ax:=gwrm()
	bx:=gwrm()

	if pmode<>tblock then
		genmc(m_mov, ax, px)
		genmc(m_mov, bx, qx)
		genmc(m_mov, qx, ax)
		genmc(m_mov, px, bx)
	else
		merror("swap/block")
	end

	poppcl()
	poppcl()
end

proc px_clear*(pcl p) =
	mclopnd ax

	ax:=getopnd_ind_simp(zz, tu64)

	clearblock(ax, p.size)
	poppcl()
end

proc px_add*(pcl p) =
	do_addsub(p, m_inc, m_add, m_addss)
end

proc px_sub*(pcl p) =
	do_addsub(p, m_dec, m_sub, m_subss)
end

proc px_mul*(pcl p) =
! Z' := Y * Z
	mclopnd ax, bx
	pcl z

	ax:=loadopnd(yy)

	if pstdint[pmode] then
		if z:=isimmint(zz) then
			mulimm(ax, z.value)

		else

!			bx:=getopnd(zz)
			bx:=loadopnd(zz)
			genmc(m_imul2, ax, bx)
		end

	else
		bx:=getopnd(zz)
		genmc(m_mulss, ax, bx)
	end

	poppcl()
end

proc px_div*(pcl p) =
	mclopnd ax, bx

	ax:=loadopnd(yy)
	bx:=getopnd(zz)
	genmc(m_divss, ax, bx)
	poppcl()
end

proc px_idiv*(pcl p) =
! Z' := Y % Z
	do_divrem(p, issigned:pstdsigned[pmode], isdiv:1)
end

proc px_irem*(pcl p) =
! Z' := Y rem Z
	do_divrem(p, issigned:pstdsigned[pmode], isdiv:0)
end

proc px_idivrem*(pcl p) =
! Z' := divrem(Y, Z)
	do_divrem(p, issigned:pstdsigned[pmode], isdiv:2)
end

proc px_bitand*(pcl p) =
! Z' := Y iand Z
	do_bitwise(p, m_and)
end

proc px_bitor*(pcl p) =
! Z' := Y ior Z
	do_bitwise(p, m_or)
end

proc px_bitxor*(pcl p) =
! Z' := Y ixor Z
	do_bitwise(p, m_xor)
end

proc px_shl*(pcl p) =
! Z' := Y << Z
	do_shift(p, m_shl)
end

proc px_shr*(pcl p) =
! Z' := Y >> Z
	do_shift(p, (pstdsigned[pmode]|m_sar|m_shr))
end

proc px_min*(pcl p) =
! Z' := min(Y, Z)
	if ispint(pmode) then
		do_max_int((pstdsigned[pmode]|gt_cond|gtu_cond))
	else
		do_max_float(m_minss)
	end
end

proc px_max*(pcl p) =
! Z' := max(Y, Z)
	if ispint(pmode) then
		do_max_int((pstdsigned[pmode]|lt_cond|ltu_cond))
	else
		do_max_float(m_maxss)
	end
end

proc px_addpx*(pcl p) =
! Z' := Y + Z*s + d
	mclopnd ax, cx

	cx:=do_addrmode(p)

	if regvarset.[cx.reg] then
		ax:=gwrm(tu64)
	else
		ax:=gwrm(tu64, cx.reg)
	end

	dolea(ax, cx)
	poppcl()

	setnewzz(tu64, ax.reg)
end

proc px_neg*(pcl p) =
! Z' := -Z
	mclopnd ax

	ax:=loadopnd(zz)

	if ispint(pmode) then
		genmc(m_neg, ax)
	else
		do_negreal(ax, pmode)
	end
end

proc px_abs*(pcl p) =
! Z' := abs Z
	mclopnd ax, lx

	ax:=loadopnd(zz)

	if ispint(pmode) then
		genmc(m_cmp, ax, mgenint(0, pmode))

		genmc_cond(m_jmpcc, ge_cond, lx:=genlabel(++mlabelno))
		genmc(m_neg, ax)
		genmc(m_label, lx)

	else
		do_absreal(ax, pmode)
	end
end

proc px_bitnot*(pcl p) =
	mclopnd ax
	ax:=loadopnd(zz)
	genmc(m_not, ax)
end

proc px_not*(pcl p) =
	mclopnd ax
	ax:=loadopnd(zz)
	genmc(m_xor, changeopndsize(ax,1), mgenint(1, tu8))
end

proc px_toboolt*(pcl p) =
! Z' := istrue Z
	mclopnd ax, bx, cx
	byte pmode2:=p.mode2

	ax:=loadopnd(zz, pmode2)

	if ispfloat(pmode2) then
		bx:=gwrm(pmode2)
		cx:=gwrm(tu8)
		genmc(m_xorps, bx, bx)
		genmc(m_comiss, ax, bx)

		genmc_cond(m_setcc, (p.opcode=ktoboolt|ne_cond|eq_cond), cx)
		genmc(m_movzx, changeopndsize(cx, 4), cx)		!4 works for u32/u64
		setnewzz(pmode, cx.reg)

	else
		genmc(m_test, ax, ax)
		genmc_cond(m_setcc, (p.opcode=ktoboolt|ne_cond|eq_cond), bx:=changeopndsize(ax, 1))
		genmc(m_movzx, changeopndsize(ax, 4), bx)
		pclstack[xx].mode:=pmode
	end
end

proc px_sqrt*(pcl p) =
	mclopnd ax

	ax:=loadopnd(zz)
	genmc(m_sqrtss, ax, ax)
end

proc px_maths*(pcl p) =
	do_callrts(p, mathsnames[p.mathsop], 1)
end

proc px_maths2*(pcl p) =
	swapopnds(yy,zz)
	do_callrts(p, mathsnames[p.mathsop], 2)
end

proc px_power*(pcl p) =
! Z' := Y ** Z
	mclopnd ax, bx
	psymbol d

	if ispint(pmode) then
		d:=findhostfn(kpower)
		unless d then merror("$power?") end
		swapopnds(yy, zz)
		do_callrts(p, nil, 2, d)
	else
		swapopnds(yy, zz)
		do_callrts(p, "pow", 2)
	end
end

proc px_incrload*(pcl p) =
! Z' := (Z +:= n)^
	do_incrload(p, m_inc, m_add)
end

proc px_decrload*(pcl p) =
! Z' := (Z -:= n)^
	do_incrload(p, m_dec, m_sub)
end

proc px_loadincr*(pcl p) =
! Z' := Z++^ (difficult to express step)
	do_loadincr(p, m_inc, m_add)
end

proc px_loaddecr*(pcl p) =
! Z' := Z--^
	do_loadincr(p, m_dec, m_sub)
end

proc px_fix*(pcl p) =
! Z' := cast(Z, t) Real u to int t
	mclopnd fx, ax
!
	fx:=loadopnd(zz, p.mode2)
	pushpcl_reg(pmode)

	ax:=getopnd(zz, pstdmin[pmode])
	genmc(m_cvttss2si+ispwide(p.mode2), ax, fx)

	swapopnds(yy, zz)
	poppcl()

	setnewzz(pmode, ax.reg)
end

proc px_float*(pcl p) =
! Z' := cast(Z, t) Int u to real t
	mclopnd ax, fx
	int lab, lab2
	byte pmode2:=p.mode2

	ax:=loadopnd(zz, pmode2)

	if pstdsize[pmode2]<4 then merror("float/short") end

	if pstdsigned[pmode2] then
		pushpcl_reg(pmode)
		fx:=getopnd(zz)

		genmc(m_cvtsi2ss+ispwide(pmode), fx, ax)
		swapopnds(yy, zz)

	elsif pmode2=tu64 then								!u64 to r32/r64
		pushpcl_reg(tr64)								!convert to r64 in all cases

		fx:=getopnd(zz, tr64)

		lab:=mcreatefwdlabel()
		lab2:=mcreatefwdlabel()

		genmc(m_cmp, ax, genint(0, ax.size))					!range of +ve i64?
		genmc_cond(m_jmpcc, lt_cond, genlabel(lab))
		genmc(m_cvtsi2sd, fx, ax)						!number is +ve i64
		genmc(m_jmp, genlabel(lab2))

		mdefinefwdlabel(lab)

		if not labmask63 then
			labmask63:=++mlabelno
			laboffset64:=++mlabelno
		end
		genmc(m_and, ax, genlabelmem(labmask63))		!clear top bit of u64 (subtract 2**63)
		genmc(m_cvtsi2sd, fx, ax)						!now in +ve i64 range
		genmc(m_addsd, fx, genlabelmem(laboffset64))	!add back 2**63 as float

		mdefinefwdlabel(lab2)							!done conv to r64
reduce:
		if pmode=tr32 then								!for r64, reduce down
			genmc(m_cvtsd2ss, changeopndsize(fx, 4), fx)
			pclstack[zz].mode:=tr32
		end

		swapopnds(yy, zz)								!bring old int value to top
	else												!u32 to r32/r64
		pushpcl_reg(tr64)								!convert to r64 in all cases

		fx:=getopnd(zz, tr64)
		ax:=changeopndsize(ax, 8)						!eg A0 to D0

		genmc(m_cvtsi2sd, fx, ax)						!u64 (always in range) to r64

		goto reduce

	end

	poppcl()
end

proc px_typepun*(pcl p) =
! Z' := t(u!(Z^))
	mclopnd ax, bx

	bx:=loadopnd(zz, p.mode2)
	ax:=gwrm()
	genmc(m_mov, ax, changeopndsize(bx, ax.size))

	setnewzz(pmode, ax.reg)
end

proc px_widen*(pcl p) =
! Z' := cast(Z, t) Mask to width of u, but type is widened to t
	mclopnd ax, bx

	if pmode=tu64 and p.mode2=tu32 then
		ax:=loadopnd(zz, tu32)
		if mccodex.opcode<>m_mov then
			genmc(m_mov, ax, ax)			!sets upper half to zero, just in case
		end
	else
		bx:=getopnd(zz, p.mode2)
		ax:=gwrm()
		genmc((pstdsigned[p.mode2]|m_movsx|m_movzx), ax, bx)
		setnewzz(pmode, ax.reg)
	end
end

proc px_fwiden*(pcl p) =
	mclopnd fx
	fx:=loadopnd(zz, p.mode2)
	genmc(m_cvtss2sd, changeopndsize(fx, 8), fx)
	pclstack[zz].mode:=tr64
end

proc px_fnarrow*(pcl p) =
! Z' := cast(Z, t) r64 to r32
	mclopnd fx
	fx:=loadopnd(zz, p.mode2)
	genmc(m_cvtsd2ss, changeopndsize(fx, 4), fx)
	pclstack[zz].mode:=tr32
end

proc px_startmx*(pcl p) =
	saveopnds()
end

proc px_resetmx*(pcl p) =
! -
	movetoreg((pstdfloat[pmode]|xr0|r0))

	if p.opcode=kresetmx then
		poppcl()
	end

	PCLSET:=0
	FOR I TO NOPERANDS DO
		IF PCLSTACK[I].REG THEN
			PCLSET.[PCLSTACK[I].REG]:=1
		FI
	OD
end

proc px_label*(pcl p) =
!	genmc(m_label, genlabel(p.labelno))
	genmc_lab(p.labelno)
end

proc px_setcall*(pcl p) =
	saveopnds()

	if ncalldepth>=maxcalldepth then
		merror("Too many nested calls")
	end

	++ncalldepth

	if p.nargs<=4 then
		callalign[ncalldepth]:=mstackdepth.odd
	else
		callalign[ncalldepth]:=p.nargs.odd ixor mstackdepth.odd
	end

	callblockret[ncalldepth]:=pmode=tblock
	callblocksize[ncalldepth]:=p.size

	if callalign[ncalldepth] then
		pushslots(1)
	end
end

proc px_setarg*(pcl p) =
	int n

	n:=p.x+callblockret[ncalldepth]

	if n>4 then
		pushopnd(zz, pmode)
	elsif pmode=tblock then			!need to record its size
		callargsize[ncalldepth, n]:=p.size
	end
end

proc px_loadall*(pcl p) =
	unimpl(p)
end

proc px_eval*(pcl p) =
	loadopnd(zz)
	poppcl()
end

proc px_comment*(pcl p) =
	mcomment(p.svalue)
!	unimpl(p)
end

proc px_data*(pcl p)=
!should only have one label operand
	genmc(m_dq, genlabel(p.labelno))
end

=== mcl_blockaux.m 0 0 4/74 ===

global func scaleindex(mclopnd ax, int scale)int=
!when scale is 1/2/3/4, return scale unchanged
!anything else, scale value in ax, return 1
	int n
	if scale in [1,2,4,8] then return scale end

	mulimm(ax,scale)
	return 1
end

global proc mulimm(mclopnd ax, int n)=
!multiply operand in ax (a simple reg) by constant n
!will try efficient method if possible, otherwise use normal multiply 
	int shifts,m
	mclopnd bx

	case n
	when 0 then
		clearreg(ax)
		return
	when 1 then
		return
	when -1 then
		genmc(m_neg, ax)
		return
	end case

	shifts:=0
	m:=n

	while m.even do
		m>>:=1
		++shifts
	end

	if shifts then
		genmc(m_shl, ax, mgenint(shifts))
	end

	case m
	when 1 then
		return
	when 3, 5, 9 then
		genmc(m_lea, ax, genindex(areg: ax.reg, ireg:ax.reg, scale:m-1))
	else						!mul needed anyway; forget the shift
		if shifts then
			mccodex.opcode:=m_imul2
			mccodex.b:=genint(n)
		else
!			genmc(m_imul2, ax, genint(n))

			bx:=gwrm((ax.size=4|ti32|ti64))
			genmc(m_mov, bx, genint(n))
			genmc(m_imul2, ax, bx)

		end
	end case

end

global func isimmint(int n)pcl p=
	if pclstack[n].code then
		p:=pclopnds[n]
		if p.optype=int_opnd then
			return p
		end
	end
	nil
end

global func ismemaddr(int n)int=
	if pclstack[n].code and pclopnds[n].opndtype=memaddr_opnd then return 1 end
	return 0
end

global proc do_pushlowargs(int nargs, nvariadics=0, isptr=0)=
!nargs=0 to 4 /operands/, not using more than 4 slots
!load args to D10-13/X0-3
!does not do anything with the stack at all
! Params are categorised as follows:
! Variadic:
!   float:  load to both D and X registers
!   other:  load to D register only
! Normal/non-variadic:
!   float:  load to X register
!   other:  load to D register
	mclopnd ax
	int j,k, nextireg, nextxreg, mode, imode, blockret
	psymbol dblock

	if nargs=0 then return end

	blockret:=callblockret[ncalldepth]

	nextireg:=r10
	nextxreg:=xr0

	k:=0
	for i:=noperands downto noperands-nargs+1 do
		++k						!counts params from 1

		if k=1 and blockret then
			dblock:=newblocktemp(callblocksize[ncalldepth])
			dblock.used:=1
			genmc(m_lea, mgenreg(r10), mgenmem(dblock))

		else

			j:=i-isptr+BLOCKRET

			mode:=pclstack[j].mode

			case mode
			when tblock then
				ax:=loadparam(j, mode, nextireg)
!				copyblockarg(ax, callargsize[ncalldepth,k], k)

			when tr64, tr32 then
				loadparam(j, mode, nextxreg)
				if nvariadics and k>=nvariadics then			!variadic floats go to both regs

!I need to move xmm reg to int reg
					imode:=(mode=tr32|tu32|tu64)
					genmc(m_mov, mgenreg(nextireg, imode), mgenreg(nextxreg, mode))
				end
			else
doint:
				loadparam(j, mode, nextireg)
			end case
		end

		++nextireg
		++nextxreg
	end
end

global proc do_getretvalue(pcl p)=
	int reg,xreg,i,n, m
	pcl q
	[10]int modes

	if p.next.opcode=ktype then
		n:=0
		while q:=p; p:=p.next; p.opcode=ktype do
			modes[++n]:=p.mode
		end
		currpcl:=q

		for i:=n downto 1 do 
			m:=modes[i]
			pushpcl_reg(m, (pstdfloat[m]|multxregs[i]|multregs[i]))
		end

	else
		pushpcl_reg(p.mode, (pstdfloat[p.mode]|xr0|r0))

	end
end

global func newblocktemp(int size)psymbol=
	[16]char str
	psymbol d

	if nblocktemps>=maxblocktemps then
		merror("Too many block temps")
	end
	++nblocktemps

	fprint @str,"$B#",nblocktemps

	d:=pmakesymbol(str, frameid)
	d.mode:=tblock
	d.size:=size
	d.used:=1
!	d.nextlocal:=currfunc.nextlocal

	d.nextlocal:=currfunc.nextlocal
	currfunc.nextlocal:=d

	blockdefs[nblocktemps]:=d
	d
end

global proc do_jumptruefalse(pcl p, int cond)=
	mclopnd ax, bx

	ax:=loadopnd(zz)

	if ispint(pmode) then
		genmc(m_test, ax, ax)

	else
		bx:=gwrm()
		genmc(m_xorps, bx, bx)
		genmc(m_comiss, ax, bx)
	end

	genmc_cond(m_jmpcc, cond, genlabel(p.labelno))

	poppcl()
end

global proc do_incrload(pcl p, int incrop, addop)=
	mclopnd ax, mx
	int reg
	pcsrec ps

	mx:=getopnd_ind(zz, mem:1)

	reg:=gwri(getmemreg(mx))					!for dest

	ax:=mgenreg(reg, pmode)

	if p.stepx=1 then
		genmc(incrop, mx)
	else
		genmc(addop, mx, mgenint(p.stepx))
	end

	genmc(m_mov, ax, mx)
!
!now replace ax opnd with new value
	clear ps

	ps.reg:=reg
	ps.mode:=pmode
	ps.count:=1
	pclstack[zz]:=ps

	pclopnds[zz]:=nil

	PCLSET.[REG]:=1

end

global proc do_loadincr(pcl p, int incrop, addop)=
	mclopnd ax, mx

	mx:=getopnd_ind(zz, mem:1)
!	mx:=getopnd_ind(zz, mem:0)

	pushpcl_reg(pmode)			!to hold loaded value

	ax:=getopnd(zz)

	genmc(m_mov, ax, mx)

	if p.stepx=1 then
		genmc(incrop, mx)
	else
		genmc(addop, mx, mgenint(p.stepx))
	end

	swapopnds(yy, zz)
	poppcl()
end

global proc do_bitwise(pcl p, int opc)=
	mclopnd ax, bx

	ax:=loadopnd(yy)
	bx:=getopnd(zz)

	genmc(opc, ax, bx)

	poppcl()
end

global proc do_shift(pcl p, int opc)=
	mclopnd ax, cx
	pcl y
	pcsrec ps:=pclstack[zz]

	ax:=loadopnd(yy)
	y:=pclopnds[zz]

	if ps.code and y.opndtype=int_opnd then
		if y.value then
			genmc(opc, ax, mgenint(y.value))
		fi
	else
		genmc(m_push, mgenreg(r10)) when r10used
		cx:=loadparam(zz, tu8, r10)
		genmc(opc, ax, cx)
		genmc(m_pop, mgenreg(r10)) when r10used
	end
	poppcl()
end

global proc do_max_int(int cond)=
	mclopnd ax, bx, ax2, bx2

	ax:=loadopnd(yy)
	bx:=loadopnd(zz)

	if ax.size<>bx.size then MERROR("MAX/SIZE") fi
	if ax.size=1 then
		ax2:=changeopndsize(ax,2)	
		bx2:=changeopndsize(bx,2)	
		genmc(ploadop[pmode], ax2, ax)
		genmc(ploadop[pmode], bx2, bx)
		ax:=ax2
		bx:=bx2
	fi

!CPL =AX.SIZE, =BX.SIZE

	genmc(m_cmp, ax, bx)
	genmc_cond(m_cmovcc, cond, ax, bx)

	poppcl()
end

global proc do_max_float(int opc)=
	mclopnd ax, bx
	ax:=loadopnd(yy)
	bx:=getopnd(zz)
	genmc(opc, ax, bx)
	poppcl()
end

global proc do_divrem(pcl p, int issigned, isdiv)=
!isdiv = 0/1/2 = rem/div/divrem
! Z' := Y % Z
	mclopnd ax, bx, px
	pcl q
	int opc, n, shifts
	byte fdivto:=0
	int locyy:=yy, loczz:=zz

	ax:=loadopnd(locyy, pmode)

	q:=isimmint(loczz)

	if q and isdiv=1 then
		n:=q.value
		case n
		when 0 then
			merror("Divide by zero")
		when 1 then
			poppcl()
			return
		else
			shifts:=ispoweroftwo(n)
			if shifts AND NOT FDIVTO then
				genmc((issigned|m_sar|m_shr), ax, genint(shifts, pmode))
				poppcl()
				return
			end
		end case
	fi 

	bx:=loadopnd(loczz)

	saverdx()
	fixdivopnds(locyy, loczz)
	bx:=loadopnd(loczz)			!in case regs have changed

	if issigned then
		opc:=
			case pstdsize[pmode]
			when 8 then	m_cqo
			when 4 then	m_cdq
			when 2 then	m_cwd
			else merror("div/u8"); 0
			end case
		genmc(opc)

		opc:=m_idiv
	else
		clearreg(mgenreg(r11))
		opc:=m_div
	end

	genmc(opc, bx)

	case isdiv
	when 0 then				!rem
		genmc(m_xchg, mgenreg(r0, pmode), mgenreg(r11, pmode))

	when 2 then				!divrem
		genmc(m_xchg, bx, mgenreg(r11, pmode))			!rem replace y-operand
		swapopndregs(r1)						!make sure it is in r1
		swapopnds(locyy, loczz)

	end case

	restorerdx()

	if fdivto then
		bx:=gwrm(ti64)
		genmc(m_pop, bx)
		genmc(m_mov, makeopndind(bx, pmode), getopnd(locyy))
		poppcl()
	end

	if isdiv<>2 then
		poppcl()
	end

end

proc fixdivopnds(int locyy, loczz)=
!two div operands exist as the top two operands, which will be
!in registers
!the div op requires that x is in d0, and y in any other register
!d11 also needs to be free, which will be the case is reg allocs only
!go up to d9, and d10/d11/12/13 are in use for win64 parameter passing
	int regx, regy, zop
	mclopnd bx, ax

	regx:=pclstack[locyy].reg
	regy:=pclstack[loczz].reg

	if regx=r0 then			!regy will be OK
		return
	end

	bx:=getopnd(locyy, tu64)
	ax:=getopnd(loczz, tu64)

	if regy=r0 then			!need to swap then
		genmc(m_xchg, bx, ax)
		swapopnds(locyy, loczz)		!switch operands
		return
	end

!neither x nor y in r0
	if regset.[r0]=0 then	!d0 not in use
		genmc(m_xchg, mgenreg(r0), bx)
		regset.[regx]:=0				!switch registers for yy

		pclstack[locyy].reg:=r0
		regset.[r0]:=1

		return
	end

!need to move current occupier of r0
	for zop:=noperands downto 1 do
		if pclstack[zop].reg=r0 then exit end
	else
		return
	end

!zop is the operand number that happens to be using r0
	genmc(m_xchg, mgenreg(r0), getopnd(locyy, tu64))	
	swap(pclstack[locyy].reg, pclstack[zop].reg)		!switch registers
end

proc saverdx=
	genmc(m_push, mgenreg(r11)) when r11used
end

proc restorerdx=
	genmc(m_pop, mgenreg(r11)) when r11used
end

global func do_addrmode*(pcl p)mclopnd px =
!Top two stack elements are an array (yy) and index (zz)
!Return a operand which provdes the address mode to access the element,
!for either reading or writing
!The address mode will use 0, 1 or 2 registers. The registers may be 1 or 2
!associated with the pcl operands, or may be regvars.
!If for reading, caller will need to make their own arrangements for a dest reg.
!When Xb has to be loaded into a register anyway, then the caller can make use
!of that

	mclopnd ax,bx
	int scale, extra,offset, reg,regix
	psymbol d
	pcl q
	pcsrec ys:=pclstack[yy], zs:=pclstack[zz]

	scale:=p.scale
	extra:=p.extra

	q:=isimmint(zz)
	if q then
		offset:=q.value*scale+extra	!for imm offset
	end

	px:=nil

	if regvarset.[ys.reg] then				!(if not reg, will be 0)
		if regvarset.[zs.reg] then
			reg:=zs.reg
			regix:=scaleregvar(reg, scale)
			px:=genindex(areg:ys.reg, ireg:regix, offset:extra, scale:scale)

		elsif q then						!regvar/imm
			px:=genindex(areg:ys.reg, offset:offset)
		else								!regvar/any
			scale:=scaleindex(bx:=loadopnd(zz, zs.mode), scale)
			px:=genindex(areg:ys.reg, ireg:bx.reg, scale:scale, offset:extra)
		end

	elsif ismemaddr(yy) then
		d:=pclopnds[yy].def
		if d.nameid=staticid and highmem=2 or 
			D.NAMEID=PARAMID AND d.mode=tblock  then
			skip
		end

		if regvarset.[zs.reg] then			!memaddr/regvar
			reg:=zs.reg
			regix:=scaleregvar(reg, scale)
			px:=genindex(ireg:regix, def:d, offset:extra, scale:scale)

		elsif q then			!memaddr/imm
			px:=genindex(def:d, offset:offset)
		else							!memaddr/any
			scale:=scaleindex(bx:=loadopnd(zz, ti64), scale)
			px:=genindex(ireg:bx.reg, def:d, offset:extra, scale:scale)
		end
	else								!
skip:
		ax:=loadopnd(yy, tu64)

		if zs.reg then			!any/regvar
			regix:=scaleregvar(zs.reg, scale)
			px:=genindex(areg:ax.reg, ireg:regix, offset:extra, scale:scale)

		elsif q then						!any/imm	
			px:=genindex(areg:ax.reg, offset:offset)
		else
			scale:=scaleindex(bx:=loadopnd(zz, tu64), scale)
			px:=genindex(areg:ax.reg, ireg:bx.reg, scale:scale, offset:extra)
		end
	end

	px.size:=p.size

	return px
end

function scaleregvar(int reg, &scale)int=
!When scale is 1/2/3/4, return reg (a regvar) and scale unchanged;
!otherwise set up a new register for operand n
!Copy reg to it, and scale. Return new reg, and set scale to 1
	int regix
	mclopnd ax

	if scale in [1,2,4,8] then return reg end

	regix:=gwri()
	ax:=mgenreg(regix)

	IF SCALE=16 THEN
		genmc(m_lea, ax, genindex(ireg:reg, areg:reg, scale:1))
		scale:=8

	ELSE
		genmc(m_mov,ax, genreg(reg))
		mulimm(ax,scale)
		scale:=1
	FI

	pcsrec ps
	clear ps

	ps.reg:=regix
	ps.mode:=ti64

	return regix
end

global proc dolea(mclopnd ax, px)=
!do 'lea ax, px`, but suppress in cases like 'lea d0,[d0]'
	unless px.regix=px.valtype=px.offset=0 and px.reg=ax.reg then
		genmc(m_lea, ax, px)
	end
end

global proc clearblock(mclopnd ax, int n)=
!ax is the operand with the address of memory to be cleared
!generate code to clear n bytes

	mclopnd rx, rcount
	int nwords, lab, oddbytes, offset, workreg, countreg

	oddbytes:=n rem 8		!will be zero, or 1..7

	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of u64s (ie. octobytes)

	rx:=gwrm(tu64)
	clearreg(rx)

	offset:=0

	if 1<=nwords<=8 then		!use unrolled code (no loop)
		ax:=changeopndsize(ax, 8)

		to nwords do
			genmc(m_mov, applyoffset(ax, offset), rx)
			offset+:=8
		end

	elsif nwords<>0 then		!use a loop

!SPLIT INTO xx VERSIONS:
! NWORDS IS A MULTIPLE OF 4, so can write 4 words at a time, in 1/4 of iterations
! Or do one word at a time like now.
! nword is a multiple of 4 happens when N is a multiple of 32 bytes, which will
! always be the case for power-of-two sizes of 32 bytes or more. 32/64 may already
! be done without a loop. So non-part-unrolled version only really for odd array or
! struct sizes, such as [100]char.

		if nwords iand 3 then		!not 4n

			rcount:=genreg(countreg:=gwri())
			lab:=++mlabelno

			ax:=makesimpleaddr(ax)

			genmc(m_mov, rcount, genint(nwords))
			genmc(m_label, genlabel(lab))
			genmc(m_mov, ax, rx)

			genmc(m_add, genreg(ax.reg), genint(8))

			genmc(m_dec, rcount)
			genmc_cond(m_jmpcc, ne_cond, genlabel(lab))

			offset:=0
		else
			rcount:=genreg(countreg:=gwri())
			lab:=++mlabelno

			ax:=makesimpleaddr(ax)
			genmc(m_mov, rcount, genint(nwords/4))
			genmc(m_label, genlabel(lab))

			for i to 4 do
				genmc(m_mov, applyoffset(ax, offset), rx)
				offset+:=8
			end

			genmc(m_add, genreg(ax.reg), genint(targetsize*4))

			genmc(m_dec, rcount)
			genmc_cond(m_jmpcc, ne_cond, genlabel(lab))

			offset:=0
		end
	end

	if oddbytes then
		n:=oddbytes						!1..7

		if n>=4 then
			rx:=changeopndsize(rx, 4)
			genmc(m_mov, applyoffset(ax, offset, 4), rx)
			n-:=4
			offset+:=4
		end
		if n>=2 then
			rx:=changeopndsize(rx, 2)
			genmc(m_mov, applyoffset(ax, offset, 2), rx)
			n-:=2
			offset+:=2
		end
		if n=1 then
			rx:=changeopndsize(rx, 1)
			genmc(m_mov, applyoffset(ax, offset, 1), rx)
		end
	end
end

global proc do_negreal(mclopnd ax, int mode)=
	if ispwide(pmode) then
		if not labneg64 then labneg64:=mcreatefwdlabel() end
		genmc(m_xorpd, ax, genlabelmem(labneg64))
	else
		if not labneg32 then labneg32:=mcreatefwdlabel() end
		genmc(m_xorps, ax, genlabelmem(labneg32))
	end
end

global proc do_absreal(mclopnd ax, int mode)=
	if ispwide(pmode) then
		if not lababs64 then lababs64:=mcreatefwdlabel() end
		genmc(m_andpd, ax, genlabelmem(lababs64))
	else
		if not lababs32 then lababs32:=mcreatefwdlabel() end
		genmc(m_andps, ax, genlabelmem(lababs32))
	end
end

global proc do_callrts(pcl p, ichar opname, int nargs, psymbol d=nil)=
	int slots

	saveopnds(nargs)
	slots:=0

	if mstackdepth.odd then
		pushslots(1)
		slots:=1
	end

	do_pushlowargs(nargs)

	if mstackdepth then
		slots+:=4
		pushslots(4)					!shadowspace
	else
		localshadow:=1
	end

	if opname then
		genmc(m_call, genextname(opname))
	else
		genmc(m_call, genmemaddr(d))
	end

	to nargs do
		poppcl()
	end

	if slots then
		popslots(slots)
	end

	do_getretvalue(p)
end

global func findhostfn(int opc)psymbol=
!called from pcl/mcl backend. opc refers to a PCL op

	case opc
	when kpower then			!assume for i64
!		getsysfnhandler(sf_power_i64)
		findsyslibfunc("m$power_i64")

	else
		nil
	end case
end

func checkregvarls(int n, pcl p)int=
!n is a pushed pcl operand number
!p is the following pcl op
!return 1 when n is a register and p is store op to a var with same register
	pcsrec ps := pclstack[n]
	return 0 when p.opcode<>kstore
	return 0 unless ps.reg
!CPL =PS.REG, P.DEF.NAME, P.DEF.REG
	if p.opndtype=mem_opnd and p.def.reg = ps.reg then
		return 1
	end
	return 0	
end

global proc do_addsub(pcl p, int incop, addop, faddop) =
	mclopnd ax, bx
	pcl z
	byte pop2:=0

	if pstdint[p.mode] then

		if checkregvarls(yy, p.next) then		!optimise load-store of regvar
			ax:=getopnd(yy)
			currpcl:=p.next
			pop2:=1
		else
			ax:=loadopnd(yy)
		fi

		if (z:=isimmint(zz)) and z.value=1 then
			genmc(incop, ax)
		else
			bx:=getopnd(zz)
			genmc(addop, ax, bx)
		end
	else
		ax:=loadopnd(yy)
		bx:=getopnd(zz)
		genmc(faddop, ax, bx)
	end

	poppcl()
	if pop2 then poppcl() end
end
=== mcl_caux.m 0 0 5/74 ===
!hello

int coffseta

global const maxfuncsig=100
global [maxfuncsig]byte fsnargs
global [maxfuncsig]byte fsrettype
global [maxfuncsig, maxcallarg]byte fsargs
global int nfuncsigs

global proc dobinop(pcl p, ichar opstr)=
	if p.opcode=kidiv then
		ccstr("   if (")
		cctpcast(p)
		ccstack(zz)
		ccstrline(") == 0) {puts((u64)""Divide by zero""); exit(1);}");
	fi

	cctab()
	cctpcast(p)
	ccstack(yy, p)
	ccstr(") ")
	ccstr(opstr)
	ccstr("= ")
	cctpcast(p)
	ccstack(zz, p)
	ccrb()
	popcc()
	ccsemi()
end

global proc dounaryop(pcl p, ichar opstr)=
	cctab()
	cctpcast(p)
	ccstack(zz, p)
	ccstr(") = ")
	ccstr(opstr)
	cctpcast(p)
	ccstack(zz, p)
	ccrb()
	ccsemi()
end

global proc domathsop(pcl p, ichar opstr)=
	cctab()
	cctpcast(p)
	ccstack(zz, p)
	ccstr(") = ")
	ccstr(opstr)
	ccstr("(")
	cctpcast(p)
	ccstack(zz, p)
	ccstrline("));")
end

global proc domaths2op(pcl p, ichar opstr)=
	cctab()
	cctpcast(p)
	ccstack(yy)
	ccstr(") = ")
	ccstr(opstr)
	ccstr("(")
	cctpcast(p)
	ccstack(yy)
	ccstr("), ")
	cctpcast(p)
	ccstack(zz)
	ccstrline("));")
	popcc()
end

global proc dobinopto(pcl p, ichar opstr, int scale=0)=
	cctab()
	ccstr("*")
	cccastptrm(p.mode)
	ccstack(zz)
	ccstr(") ")
	ccstr(opstr)
	ccstr("= ")
	cctpcast(p)
	ccstack(yy)
	ccrb()
	if scale>1 then
		ccstr("*")
		ccint(scale)
	fi
	ccsemi()
	popcc()
	popcc()
end

global proc doabs(pcl p)=
	cctab()
	ccstr("if (")
	cctpcast(p)
	ccstack(zz, p)
	ccstr(") < 0) ")
	cctpcast(p)
	ccstack(zz, p)
	ccstr(") = -")
	cctpcast(p)
	ccstack(zz, p)
	ccrb()
	ccsemi()
end

global proc dojumpcc(pcl p)=
	cctab()
	ccstr("if (")
	cctpcast(p)
	ccstack(yy, p)
	ccrb()
	ccstr(getcondstr(p.condcode))
	cctpcast(p)
	ccstack(zz, p)
	ccstr(")) goto ")
	cclabel(p.labelno)
	popcc()
	unless p.popone then
		popcc()
	end

	ccsemi()
end

global proc dojumptf(pcl p)=
	cctab()
	ccstr("if (")
	if p.opcode=kjumpf then ccstr("!") fi
	cctpcast(p)
	ccstack(zz, p)
	ccstr(")) goto ")
	cclabel(p.labelno)
	popcc()

	ccsemi()
end

global proc doprocdef(pcl pc)=
	psymbol d:=pc.def, e
	int needcomma, nstack
	pcl spc

	localmax:=noperands:=0
	nlocaltemps:=0
!	clear blockflags

!CPL "PROC:",D.NAME

	genprocsig(d, " {")


!	ccstr("	u64 $[")
!	ccint(maxoperands)
!	ccstrline("];")
!
	coffseta:=cdest.length
!	cccomment("R DECLS GO HERE")
!	nstack:=8
!
!
!	ccstr("    u64 ")
!	for i to nstack do
!		ccstr("R")
!		ccint(i-1)
!		if i<nstack then ccstr(", ") fi
!	od
!	ccsemi()
!PDECLEN:=CDEST.LENGTH-PDECOFFSET

	e:=d.nextlocal
	while e, e:=e.nextlocal do
		cctab()
		ccstrmode(e.mode, e.size)
		ccstr(" ")
		ccname(e.name)
		ccstrline(";")
	od

	if d.chasstatics then
CCCOMMENT("PROC LOCAL STATICS GO HERE")
!CPL =PCSTART
		spc:=pcstart
		while spc.opcode<>kendprog, ++spc do
			if spc.opcode=kistatic then
				e:=spc.def
				if e.cprocowner=d then
					cctab()
					spc:=genistaticvar(spc,1)-1
				fi
			fi
		od

	fi

end

global proc doendproc(pcl pc)=
	[32]char str
	int coffsetb, t

	if noperands then
		CCCOMMENT(ADDSTR("******** STACK NOT EMPTY:", STRINT(NOPERANDS)))
!		CPL "STACK NOT EMPTY:",CURRFUNC.NAME
		CPL "STACK NOT EMPTY:",CURRFUNC
	fi

!CPL =LOCALMAX

	if localmax then
		coffsetb:=cdest.length
		ccstr("    u64 ")
		for i to localmax do
			ccstr("R")
			ccint(i)
			if i<localmax then ccstr(", ") fi
		od
		ccstr("; ")

		for i to nlocaltemps do
			t:=ltblockno[i]
			ccstrmode(tpblock, blocksizes[t])
			fprint @str, " R#_B#; ", ltstackno[i], t
			ccstr(str)
		od	
		ccstrline("")

!		for t to nblocktypes do
!			cpl t,blockflags[t]
!
!			fprint @str, "R#_B#", n-1, t
!		fi
!		ccstr(str)



!		od

!ccstr("NBLOCKTYPE:")
!ccint(nblocktypes)
!ccstrline("")
		ccswap(coffseta, coffsetb)

	fi
end

global proc docall(pcl p, int isfunc, isptr)=
	int nargs:=p.nargs, ftype
	int dest:=zz-nargs+1-isptr		!point to right-most arg which is dest for funcs
!	int npop:=nargs-isfunc+isptr		!stack slots to pop
	PCL PC

	cctab()

	if isfunc then					!need dest

		cctpcast(p)
		ccstack(dest, p)
		ccstr(") = ")
	fi

	if isptr then
		ftype:=getfstype(p, nargs)
		ccstr("((F")
		ccint(ftype)
		ccstr(")")

		ccstack(zz, p)				!func ptr is from top of stack
		ccstr(")")
		popcc()
	else

		ccname(p.def.name)
	fi
	ccstr("(")

	for i to nargs do
		pc:=callpcmode[ncalldepth, i]
		if pc then
			cctpcast(callpcmode[ncalldepth, i])
			ccstack(zz-i+1, pc)
			ccrb()
		else
			ccstack(zz-i+1)
		fi

		if i<nargs then ccstr(", ") fi
	od
	ccstr(")")
	ccsemi()

!CPL =ZZ
!CPL =DEST
!
!CPL "CALL/NPOP", =NARGS, =ISFUNC, =ISPTR

	to nargs-isfunc do
!CPL "//POP"
		popcc()
	od

	if nargs=0 and isfunc then
		pushcc()
	fi
end

global proc dofor(pcl pc)=
	int step:=pc.stepx
	if pc.opcode=kfordown then -:=step fi

	cctab()
	ccopnd(pc+1)
	ccstr(" += ")
	ccint(step)
	ccstr("; if (")
	ccopnd(pc+1)
	ccstr((step>0|" <= "|" >= "))
	ccopnd(pc+2)
	ccstr(") goto ")
	cclabel(pc.labelno)
	ccsemi()

end

global proc gentypes(pcl pc)=
	int t, ntypes

!	cccomment("TYPES")
	cccomment("***** Types *****")
!	ccstrline("/* Types */")

	while pc.opcode<>kendprog, ++pc do
		gentype(pc.mode, pc.size)
	od

	psymbol d:=psymboltable, e
	while d, d:=d.next do
		if d.id=proc_id then

			e:=d.nextlocal
			while e, e:=e.nextlocal do
				gentype(e.mode, e.size)
			od
		fi
	od

	ccstrline("")
end

proc gentype(int mode, size)=
	static [8]ichar typenames=("u8", "u16","","u32","","","","u64")
	int ntypes, t

	if mode=tpblock then
		ntypes:=nblocktypes
		t:=findblocktype(size, 1)
		if nblocktypes>ntypes then			!new type
			ccstr("struct $B")
			ccint(t)
			ccstr(" {")
			ccstr(typenames[blockelemsizes[t]])
			ccstr(" a[")
			ccint(blocklengths[t])	
			ccstr("];};   // mem:")
			ccint(size)
			ccsemi()
		fi
	fi
end

global proc genvars=
	cccomment("***** Variables *****")

	psymbol d:=psymboltable
	while d, d:=d.next do
		if d.id=static_id and not d.cprocowner then
			genzstaticvar(d)
		fi
	od
	ccstrline("")
end

global proc genimports=
	cccomment("***** Imported Functions *****")
	psymbol d:=psymboltable

	while d, d:=d.next do
		if d.id=import_id then
			genprocsig(d)
		fi
	od
	ccstrline("")
end

proc genprocsig(psymbol p, ichar term=";")=
	psymbol e

	if p.imported then
		ccstr("extern ")
	elsif not p.exported then
		ccstr("static ")
	fi

	ccstrmode(p.mode, p.size)
	ccstr(" ")
	ccname(p.name)
	ccstr("(")

	e:=p.nextparam
	while e, e:=e.nextparam do
		ccstrmode(e.mode, e.size)
		ccstr(" ")
		ccstr(e.name)
		if e.nextparam or p.varparams then
			ccstr(", ")
		fi
	od

	if p.varparams then
		ccstr("...")
	fi
	ccstr(")")
	ccstrline(term)
end

global func genistaticvar(pcl pc, int inproc)pcl=
!pc points to istatic op (which may be out-of-line, if called for local proc statics)
	psymbol d:=pc.def

	if d.imported then
		ccstr("extern ")
	elsif not d.exported then
		ccstr("static ")
	fi

	ccstrmode(d.mode, d.size)
	ccstr(" ")
	ccname(d.name)

	if (pc+1).opcode<>kdata then
		ccsemi()
		return pc+1
	fi


	ccstr(" = ")

	if d.mode=tpblock then
!		ccstrline("{")
		ccstrline("{{")
	fi

!CPL "=================GENISTATICVAR",D.NAME, STRPMODE(D.MODE, D.SIZE),"//",STRPMODE(PC.MODE, PC.SIZE)

	genidata(pc, inproc)
!
!	CCSTR("<")
!	CCSTR(PCLNAMES[(PC+1).OPCODE])
!	CCSTR(">")
!	CCSTRLINE("")

	repeat ++pc until pc.opcode<>kdata

	if d.mode=tpblock then
		if inproc then ccstr("    ") fi
!		ccstr("}")
		ccstr("}}")
	fi

	ccsemi()
	pc
end

proc genidata(pcl pc, int inproc)=
	pcl qc
	byte hasaddr:=0, is64bits:=1

	qc:=pc+1
	while qc.opcode=kdata, ++qc do
		case qc.opndtype
		when string_opnd, mem_opnd, memaddr_opnd, label_opnd then
			hasaddr:=1
		esac

!CPL =QC.SIZE

		if qc.size<>8 then is64bits:=0 fi

	od

!CPL =HASADDR, =IS64BITS

	if hasaddr or is64bits then
		genaddrdata(pc, inproc)
	else
		genconstdata(pc, inproc)
	fi

end

proc genaddrdata(pcl pc, int inproc)=
!data contains addresses. Assume single or multiple u64 elements
	int t

!CPL "GENADDRDATA", PCLNAMES[PC.OPCODE]

	if pc.mode<>tpblock then
		if pc.size<>8 then
			merror("genaddr/not u64")
		fi
!		ccstr("<Single Addr>")
		gendatax(pc+1, 1)
	
	else
		t:=findblocktype(pc.size, 0)
		if blockelemsizes[t]<>8 then
			merror("genaddr/not []u64")
		fi

!		ccstrline("<Mult Addr/mixed>")

		++pc
		do
			if inproc then cctab() fi
			gendatax(pc)
			++pc
			if pc.opcode=kdata then
				ccstrline(",")
			else
				exit
			fi
		od

	fi
end

proc genconstdata(pcl pc, int inproc)=
!data contains only constant 1/2/4/8 byte elements, may be mixed
!dest will be 1/2/4/8 byte elements, all the same

	pcl pca 
	ref byte q8, r
	ref u16 q16 @ q8
	ref u32 q32 @ q8
	ref u64 q64 @ q8
	int t, n, elemsize

!CPL "GENCONSTDATA", PCLNAMES[PC.OPCODE]!,pc.def.name
!CPL =STRPMODE(PC.MODE, PC.SIZE)
 
	q8:=r:=pcm_allocz(pc.size)

	pca:=pc+1
	repeat
		r:=gendatac(pca, r)
		++pca
	until pca.opcode<>kdata

!now need to dump data at q into 1/2/4/8-byte values

	if pc.mode=tpblock then
		t:=findblocktype(pc.size, 0)
		IF T=0 THEN MERROR("CAN'T FIND BLOCK") FI

		n:=blocklengths[t]
		elemsize:=blockelemsizes[t]
	else
		n:=1
		elemsize:=pc.size
	fi

!CPL "-------GENCONST",=N, =ELEMSIZE

	for i to n do
		case elemsize
		when 1 then ccint(q8++^)
		when 2 then ccint(q16++^)
		when 4 then ccint(q32++^)
		else		ccint(q64++^)
		esac

		if n>1 and i<n then
			ccstrline(",")
			if inproc then cctab() fi
		fi

	od
!CPL $LINENO
end

func gendatac(pcl pc, ref byte q)ref byte=
!generate constant data to buffer at q
!return pointer to next location

	case pc.opndtype
	when mem_opnd, memaddr_opnd, label_opnd, string_opnd then
		merror("gendatac/not const")
	esac

	if pc.mode=tpblock then			!block data
		memcpy(q, pc.svalue, pc.size)
	else							!simple
		memcpy(q, &pc.value, pc.size)
	fi

	q+pc.size	
end

proc gendatax(pcl pc, int dataone=0)=
!directly generate data sequence of u64 or addr items to C dest

!	case pc.opndtype
!	when mem_opnd then
!		ccstr(pc.def.name)
!
!	when memaddr_opnd then
!		ccstr("&")
!		ccstr(pc.def.name)
!!	when label_opnd then
!	when int_opnd then
!		ccint(pc.value)
!
!!	when real_opnd then
!!	when r32_opnd then
!	when string_opnd then
!!		ccstr(pc.svalue)
!		ccopnd(pc)
!
!!	when realimm_opnd then
!!	when realimm32_opnd then
!!	when data_opnd then
!	else
!		ccstr(addstr("gendata64:", opndnames[pc.opndtype]))
!	esac
!CCSTR("GDX")

	ccopnd(pc, dataone)

end

global proc genzstaticvar(psymbol d)=
	if d.imported then
		ccstr("extern ")
	elsif not d.exported then
		ccstr("static ")
	fi

	ccstrmode(d.mode, d.size)
	ccstr(" ")
	ccname(d.name)
	ccsemi()
end

global proc genprocdecls=
	cccomment("***** Function Declarations *****")
	psymbol d:=psymboltable
	while d, d:=d.next do
		if d.id=proc_id then
			genprocsig(d)
		fi
	od
	ccstrline("")
end

global proc genmain=
	return when not entryproc
!
	ccstrline("")
	ccstrline("int main(int nargs, char** args) {")
	if pfullsys then
		ccstrline("    msysc_$getcommands(nargs, (u64)args, 0);")
	fi

	ccstr("    ")
	ccname(entryproc.name)
	ccstrline("();\n}\n")
end

global proc pushcc=
	if noperands>=200 then
		cerror("Too many operands?")
	fi
	++noperands
	localmax max:=noperands
end

global proc genloadopnd(pcl pc)=
	[512]char str
	int length

	pushcc()

	cctab()

	case pc.opndtype
	when int_opnd then
		ccstack(zz, pc)
		ccstr(" = ")
		if pc.value=i64.min then
			ccstr("0x8000000000000000")
		else
			ccint(pc.value)
		fi

	when real_opnd, r32_opnd, realimm_opnd, realimm32_opnd then
		cctpcast(pc)
		ccstack(zz, pc)
		if pc.xvalue=infinity then
			print @str,") = (1.0/0.0)"
		else
			fprint @str,") = #", pc.xvalue:"e18.18"
		fi
		ccstr(str)

!	when real32_opnd then
!		sx:=pc.xvalue
!		ccint(u32@(sx))
!
!		fprint @str," /* # */", pc.xvalue:"e8"
!		ccstr(str)

	when string_opnd then
		ccstack(zz, pc)
		ccstr(" = ")
		ccstr("tou64(")
		if (length:=strlen(pc.svalue))<str.len/2 then
			ccstr("""")
			convertstring(pc.svalue,&.str)
			ccstr(str)
			ccstr(""")")
		else
			cclongstr(pc.svalue, length)
			ccstr(")")
		fi

	when mem_opnd then
		cctpcast(pc)
		ccstack(zz, pc)
		ccstr(") = ")
		ccname(pc.def.name)

	when memaddr_opnd then
		ccstack(zz, pc)
		ccstr(" = (u64)&")
		ccname(pc.def.name)

	when label_opnd then
		ccstack(zz, pc)
		ccstr(" = (u64)&&")
		cclabel(pc.labelno)
!
!	when data_opnd then
!		ccstr("<StrData")

	else
!CPL OPNDNAMES[PC.OPNDTYPE]
!		ccstr("<PCLOPND?>")
		CCSTR("<LOADOPND:")
		CCSTR(OPNDNAMES[PC.OPNDTYPE])
		CCSTR(">")
	esac

	ccsemi()
end

global proc genstoreopnd(pcl pc)=
	if noperands<1 then
!		cerror("Opnd stack underflow")
CPL "UNDERFLOW"
CCSTR("UNDERFLOW")
NOPERANDS:=1

	fi

	cctab()

	if pc.opndtype<>mem_opnd then merror("Store?") fi

	ccname(pc.def.name)
	ccstr(" = ")

	cctpcast(pc)
	ccstack(zz, pc)
	ccrb()
	ccsemi()

	popcc()
end

global proc doswap(pcl pc)=
	int mode:=pc.mode
	cctab()
	ccstr("{")
	ccstrmode(mode, pc.size)
	ccstr(" temp; temp = *")
	cccastptr(pc)
	ccstack(yy, pc)
	ccstr("); *")

	cccastptr(pc)
	ccstack(yy, pc)
	ccstr(") = *")
	cccastptr(pc)
	ccstack(zz, pc)
	ccstr("); *")

	cccastptr(pc)
	ccstack(zz, pc)
	ccstrline(") = temp; }")
	popcc()
	popcc()

end

global proc doincrload(pcl pc, ichar opstr)=
	cctab()
	cctpcast(pc)
	ccstack(zz)
	ccstr(") = *(")
	cccastptr(pc)
	ccstack(zz)
	ccstr("))")
	ccstr(opstr)
	ccint(pc.stepx)
	ccsemi()
end

global proc doloadincr(pcl pc, ichar opstr)=
	cctab()
	cctpcast(pc)
	localmax max:=zz+1
	ccstack(zz+1)
	ccstr(") = *")
	cccastptr(pc)
	ccstack(zz)
	ccstr("); *(")

	cccastptr(pc)
	ccstack(zz)
	ccstr("))")
	ccstr(opstr)
	ccint(pc.stepx)
	ccstr("; ")

	cctpcast(pc)
	ccstack(zz)
	ccstr(") = ")
	cctpcast(pc)
	ccstack(zz+1)
	ccstrline(");")

end

global func doswitchstmt(pcl pc)pcl =
	const maxlabels = 1000				!matches do_switch in front-end
	[maxlabels]u32 labtable				!list of unique labels
	[maxlabels]u32 indextable			!indices into labtable
	int swbase, n
	pcl pctab
	int labelse, lab, nlabels, index

	swbase:=pc.minlab
	n:=pc.maxlab-swbase+1
	pctab:=pc+3
	labelse:=(pc+1).labelno
	nlabels:=0							!no. unique labels

!	cpl "SWITCH", =SWBASE, =N, =SWBASE, =LABELSE, =pclnames[pctab.opcode], pctab.labelno

!build labtable/indextable

	for i to n do
		lab:=pctab.labelno
		for k to nlabels do
			if labtable[k]=lab then			!already seen
				index:=k
				exit
			fi
		else
			labtable[++nlabels]:=lab
			index:=nlabels
		od

		indextable[i]:=index
		++pctab
	od

!CPL "LABS:"
!FOR I TO NLABELS DO
!	CPL I,LABTABLE[I]
!OD
!CPL "INDICES:"
!FOR I TO N DO
!	CPL I,INDEXTABLE[I], INDEXTABLE[I]+SWBASE-1
!OD

	cctab()
	ccstr("switch (asi64(")
	ccstack(zz)
	ccstrline(")) {")


	for k to nlabels do
		cctab()

		for i to n when indextable[i]=k do
			ccstr("case ")
			ccint(i+swbase-1)
			ccstr(": ")
		od

		ccstr("goto ")
		cclabel(labtable[k])
		ccsemi()
	od

	cctab()
	ccstr("default: goto ")
	cclabel(labelse)
	ccsemi()

	ccstrline("    };")


!	MERROR("SWITCH")

	CCCOMMENT("SWITCH")


	popcc()

	pc+n+4
end

func getfstype(pcl pc, int nargs)int=
	[maxcallarg]byte args
	int rettype
	pcl p

	for i to nargs do
		p:=callpcmode[ncalldepth, i]
		args[i]:=p.mode
		if p.mode=tpblock then merror("getfs/block") fi
	od
	rettype:=pc.mode
	if rettype=tpblock then merror("getfs/blockr") fi

!now search for existing sig that matches
	for i to nfuncsigs when fsrettype[i]=rettype and fsnargs[i]=nargs do
		for k to nargs do
			if fsargs[i, k]<>args[k] then		!mismatch
				exit
			fi
		else
			return i
		od
	od

!new unique funcsig

	if nfuncsigs>=maxfuncsig then merror("Too many fs") fi
	++nfuncsigs

	fsrettype[nfuncsigs]:=rettype
	fsnargs[nfuncsigs]:=nargs
	for i to nargs do
		fsargs[nfuncsigs, i]:=args[i]
	od

	nfuncsigs
end

global proc genfuncsigs(int coffsetf)=
	int coffsetg:=cdest.length, nargs

	cccomment("Function Ptr Types:")
	for i to nfuncsigs do
		ccstr("typedef ")
		ccstrmode(fsrettype[i])
		ccstr(" (*F")
		ccint(i)
		ccstr(")(")
		nargs:=fsnargs[i]
		for k to nargs do
			ccstrmode(fsargs[i,k])
			if k<nargs then ccstr(", ") fi
		od
		ccstrline(");")
	od 
	ccstrline("")
!	ccstrline("FUNTAB1")
!	ccstrline("FUNTAB2")
!	ccstrline("FUNTAB3")

	ccswap(coffsetf, coffsetg)

end

global proc cclongstr(ichar s, int length)=
	if longstring then
		pcm_free(longstring,longstringlen)
	fi
	longstringlen:=length*2
	longstring:=pcm_alloc(longstringlen)
	longstring^:='"'
!	longstring^:='<'
	length:=convertstring(s, longstring+1)
	(longstring+length+1)^:='"'
	(longstring+length+2)^:=0

	clineptr^:=0
	gs_str(cdest,&.clinebuffer)
	ccinitline()
	gs_str(cdest,longstring)
end

global func geninitdoswx(pcl pc)pcl =
	int jtlabel
	pcl q, pcjumptab
	byte first:=1

!	CCSTRLINE("DOSWX INIT")
	++pc						!point to load #jumptab
	jtlabel:=pc.labelno

	pcjumptab:=pc+1
	repeat
		++pcjumptab
	until pcjumptab.opcode=klabel and pcjumptab.labelno=jtlabel
	++PCJUMPTAB

!CPL "FOUND PC JT:", PCJUMPTAB, PCLNAMES[PCJUMPTAB.OPCODE]

!Need to output local doswitchx jumptable
	ccstr("    static void* $jumptable[] = {")

	while pcjumptab.opcode=kswlabel do
		if not first then ccstr(", ") else first:=0 fi
		ccstr("&&L")
		ccint(pcjumptab.labelno)
		++pcjumptab
	od
	ccstrline("};")

!do the load here, using this special jumptable, and skip the instr
	pushcc()
	ccstrline("    R1 = (u64)$jumptable;")

	pc
end
!
=== mcl_cdecls.m 0 0 6/74 ===
!hello

global int assemtype='AA'
global const mclpresent=1

global proc genss=end
global proc writeexe(ichar s, int x)=end
global proc writemcx(ichar s)=end
global proc runlibfile(ichar s, int x)=end

global func writeasm:^strbuffer=

CPL "WRITEASM(C)"

 CDEST
end
=== mcl_cgen.m 0 0 7/74 ===
!const showpclcode=1
const showpclcode=0

!global const maxoperands=20

global const maxcalldepth=16
global const maxcallarg=20
global [maxcalldepth]byte callalign		!pending 1-slot alignment for syscalls
global [maxcalldepth]byte callblockret	!1 if fnc returns a block
global [maxcalldepth]u32 callblocksize	!size of any returned block
global [maxcalldepth, maxcallarg]pcl callpcmode			!get mode/size
global int ncalldepth

global int noperands

global macro zz = noperands
global macro yy = noperands-1
global macro xx = noperands-2
global macro ww = noperands-3

pcl currpcl, nextpcl

global int localmax				!max operands in this function

!the following is a list of unique stackno/blocktype combinations used in this function

global const maxlocaltemps = 100		!max local block temps
global int nlocaltemps					!how many unique ones used in this function
global [maxlocaltemps]i16 ltstackno		!stack number for this temp
global [maxlocaltemps]i16 ltblockno		!block type code


!global const int maxcallargs=20
!global const int maxnestedcall=10
!global [maxnestedcall, maxcallargs]byte argmodes
!global [maxnestedcall, maxcallargs]byte argstack
!global [maxnestedcall, maxcallargs]u32  argsizes
!global int nccalldepth

int seqno

global proc genmcl=
	pcl pc
	int coffsetf
	symbol d

!CPL "GEN LINEAR C", ENTRYPROC

	filehandle f

	gs_init(cdest)
	ccinitline()
!
	cccomment("Generated C")

	cccomment("End of C Code")


	d:=stlinear
	while d, d:=d.nextlinear do
		case d.nameid
		when staticid then
			CCSTR("STATIC: ")
			CCSTRLINE(D.NAME)
!			p:=getpsymbol(d)
!			if not d.equivvar then
!				paddsymbol(p)
!				dostaticvar(d, p)
!			fi
!
		when procid then
			CCSTR("PROC: ")
			CCSTRLINE(D.NAME)
!			p:=getpsymbol(d)
!			paddsymbol(p)
!
!			addlocals(d, p)
!
!			doprocdef(d, p)

		when dllprocid then
			CCSTR("IMPORT PROC: ")
			CCSTRLINE(D.NAME)
!			p:=getpsymbol(d)
!			paddsymbol(p)

		end
	end

end

=== mcl_clib.m 0 0 8/74 ===
global const cbufferlen=4096				!needs to be same size as exprbuffer
global [cbufferlen]char clinebuffer
global ref char clineptr, clineend

export const ctarget=1
export ichar asmext="c"

strbuffer sbuffer
global ref strbuffer cdest=&sbuffer

const maxblocktypes=200
global [maxblocktypes]int blocksizes		!total bytes size
global [maxblocktypes]int blocklengths		!array length
global [maxblocktypes]int blockelemsizes	!array element size
!global [maxblocktypes]byte blockflags		!1 during a proc means a block stack slot needed
global int nblocktypes

global proc cccomment(ichar s=nil)=
!comment that is appended to current line
	ccstr("// ")
	ccstrline(s)
end

global proc ccblank=
	ccsendline()
end

global proc cclinecomment(ichar s)=
	cccomment(s)
end

global proc ccchar(int c)=
	clineptr^:=c
	++clineptr
end

global proc cctab=
	clineptr++^:='\t'
end

global proc ccstr(ref char s)=
	while s^ do
		clineptr++^:=s++^
	od
end

global proc ccname(ref char s)=
	int c

	while c:=s++^ do
		if c='.' then
			clineptr++^:='_'
		else
			clineptr++^:=c
		fi
	od
end

global function xxstrname(ichar s)ichar=
	static [256]char str
	int c
	ichar t:=str

	while c:=s++^ do
		if c='.' then
			t++^:='_'
		else
			t++^:=c
		fi
	od
	t^:=0
	return str
end


global proc ccstrline(ichar cstr)=
	ccstr(cstr)
	ccsendline()
end

global proc ccsemi=
	ccchar(';')

!	ccstr("   // ")
!	ccint(noperands)

	ccsendline()
end

global proc ccstrsemi(ichar cstr)=
	ccstr(cstr)
	ccchar(';')
	ccsendline()
end

global proc ccsendline=
	clineptr^:=0
	gs_strln(cdest,&.clinebuffer)
	ccinitline()
end

global proc ccint(int a)=
	ccstr(strint(a))
end

global proc ccinitline=
	clineptr:=&.clinebuffer
	clineend:=clineptr+cbufferlen-1
end

global proc ccopnd(pcl p, int dataone=0)=
	[512]char str
	int length, t
	r32 sx
	ref u64 q

	t:=p.mode

!CPL "CCOPND", OPNDNAMES[P.OPNDTYPE]
!CCSTR("<CCOPND>")


	case p.opndtype
	when int_opnd then
		ccint(p.value)
	when real_opnd then
!CCSTR("<CCOPND/REAL>")
		if dataone then
			print @str,p.xvalue:"e18.18"
		else
			ccint(u64@(p.xvalue))
			fprint @str," /* # */", p.xvalue:"e16"
		fi
		ccstr(str)

!	when real32_opnd then
!		sx:=p.xvalue
!		ccint(u32@(sx))
!
!		fprint @str," /* # */", p.xvalue:"e8"
!		ccstr(str)

	when string_opnd then
		ccstr("(u64)")
		if (length:=strlen(p.svalue))<str.len/2 then
			ccstr("""")
			convertstring(p.svalue,&.str)
			ccstr(str)
			ccstr("""")
		else
!			cclongstr(p.svalue, length)
			CCSTR("CCOPND)")
		fi

	when mem_opnd then
		ccname(p.def.name)

	when memaddr_opnd then
		ccstr("(u64)&")
		ccname(p.def.name)

	when label_opnd then
		cclabel(p.labelno)

	when data_opnd then
		if p.size=8 then			!suspect only 64-bit data strs get through to here
			q:=cast(p.svalue)
			ccint(q^)
		else
			CPL "CCOPND/STRDATA"
			ccstr("<StrData>")
		fi

	else
		CCSTR("<CCOPNDXXX:")
		CCSTR(OPNDNAMES[P.OPNDTYPE])
		CCSTR(">")
	esac
end

global function getaddrmode(int scale, offset)ichar=
!Xb and Ya on stack (2 and 1) should be a pointer, and an offset
!return an address mode like (Xb+Ya*scale+offset), with casts to byte pointers etc
	static [256]char str

	fprint @str,"((char*)S#+S#*#",yy, zz, scale

	if offset>0 then
		strcat(str,"+")
		strcat(str,strint(offset))
	elsif offset<0 then
		strcat(str,strint(offset))
	fi
	strcat(str,")")
	return str
end

global proc cclabel(int labelno)=
	ccstr("L")
	ccint(labelno)
end

global proc cerror(ichar mess, param=nil)=
	print "C Gen error:",mess
	if param then
		print ":",param
	fi

!	println " seq:",ppseqno
	println
	stop 1
end

global proc popcc=
	if noperands<=0 then
CCSTRLINE("\N//POPCC ON EMPTY STACK")
	else
		--noperands
	fi
end

export func getassemstr:ref strbuffer=
	return cdest
end

global func getcondstr(int cc)ichar=
	case cc
	when eq_cc then " == "
	when ne_cc then " != "
	when lt_cc then " < "
	when le_cc then " <= "
	when ge_cc then " >= "
	else            " > "
	esac
end

global proc ccstack(int n, pcl pc=nil)=
	[32]char str
	int t

	if n<1 then
CPL "UNDERFLOW",N
		ccstr("$UNDERFLOW")
	elsif pc=nil or pc.size in [0, 1,2,4,8] then
		ccstr("R")
		ccint(n)

	else		
!		t:=findblocktype(pc.size, 1)
		t:=findblocktype(pc.size, 0)
		addlocaltemp(n, t)
!		blockflags[t]:=1
		fprint @str, "R#_B#", n, t
		ccstr(str)
	fi
end

global proc cctpcast(pcl pc)=
!type punning cast
!caller must provide closing ")" after operand
	if pc.mode<>tblock then
		ccstr("as")
		ccstrmode(pc.mode, pc.size)
	fi
	ccstr("(")
end


!global proc ccconv(pcl pc)=
!!type conversion cast
!!caller must provide closing ")" after operand
!	if pc.mode<>tpblock then
!		ccstr("to")
!		ccstrmode(pc.mode, pc.size)
!		ccstr("(")
!	fi
!end
!
global proc cctpcastm(int mode)=
!type punning cast
!	if pc.mode<>tpblock then
	ccstr("as")
	ccstrmode(mode)
	ccstr("(")
end

global proc cccastm(int mode)=
	ccstr("to")
	ccstrmode(mode)
	ccstr("(")
end

global proc cccastptr(pcl pc)=
	if pc.mode<>tblock then
		ccstr("to")
		ccstrmode(pc.mode)
		ccstr("p(")
	else
!		ccstr("*(")
		ccstr("(")
		ccstrmode(pc.mode, pc.size)
		ccstr("*)(")
	fi
end

global proc cccastptrm(int mode)=
IF MODE=TBLOCK THEN MERROR("CASTPTRM/BLOCK") FI
	ccstr("to")
	ccstrmode(mode)
	ccstr("p(")
end

global proc ccstrmode(int mode, int size=0)=
!write only c version  of mode (may depend on header defining these types
	int t

	case mode
	when tblock then
		t:=findblocktype(size, 0)
		if t=0 then
!CPL =SIZE
!CPL =NBLOCKTYPES
!FOR I TO NBLOCKTYPES DO
!	CPL "	",I,=BLOCKSIZES[I], =BLOCKLENGTHS[I],=BLOCKELEMSIZES[I]
!OD
!CPL "CAN'T FIND BLOCK TYPE", SIZE
!CCSTR("<CAN'T FIND BLOCK TYPE>")
!STOP
!RETURN

			merror("Can't find block type")
		fi
		ccstr("struct $B")
		ccint(t)
	when tvoid then
		ccstr("void")
	else
		ccstr(strpmode(mode))
	esac
end

global func findblocktype(int size, add=0)int=
!add=1 to create new block type if not found
	int elemsize

!CPL "FINDBLOCK1", =SIZE
	if size=0 then size:=1 fi

	elemsize:=1

	if size iand 7 = 0 then
		elemsize:=8
	elsif size iand 3 = 0 then
		elemsize:=4
	elsif size iand 1 = 0 then
		elemsize:=2
	fi

!CPL =ELEMSIZE

	for i to nblocktypes do
		if blocksizes[i]=size then
			return i
		fi
	od

	if add then
		newblocktype(size, elemsize)
	else
		0
	fi
end

global func newblocktype(int size, elemsize)int =
!only call when block of that size does not exist
	if nblocktypes>=maxblocktypes then
		merror("Too many blocks")
	fi
	++nblocktypes
	blocksizes[nblocktypes]:=size
	blocklengths[nblocktypes]:=size/elemsize
	blockelemsizes[nblocktypes]:=elemsize

!CPL "**********NEW BLOCK:",SIZE, ELEMSIZE

	nblocktypes
end

global proc doshowpcl(pcl p)=
	[1256]char str

	case p.opcode
	when kdata then
	else
		strcpy(&.str,"                       ")
		strcat(&.str,strpclstr(p, str.len))
		cccomment(pcm_copyheapstring(str))
	esac
end

global proc cceq=
	ccstr(" = ")
end

global proc ccrb=
	ccstr(")")
end

global proc ccinsert(int offseta, ichar s)=
!writes s to cdest at B (current end), but then moves it to given offset A
	int length
	int offsetb, size
	ichar destptr, buffer

	destptr:=cdest.strptr
	offsetb:=cdest.length
	size:=offsetb-offseta			!bytes to move
	length:=strlen(s)
	buffer:=pcm_alloc(size)

	if size=0 then merror("CCINS") fi

	memcpy(buffer, destptr+offseta, size)					!copy body to buffer

	gs_str(cdest, s)			!write temporarily to end 

	memcpy(destptr+offseta, destptr+offsetb, length)		!copy new str to A
	memcpy(destptr+offseta+length, buffer, size)			!copy body back, but further up
end

global proc ccswap(int offseta, offsetb)=
!Cdest buffer contains text A from offseta to offsetb-1, and text B
!from offsetb to end of buffer
!Swap text A and text B:
! Copy A to buffer
! Copy B to offseta
! Copy A in buffer to offseta+B.len

	int alen, blen
	ichar destptr, buffer

	destptr:=cdest.strptr

	alen:=offsetb-offseta
	blen:=cdest.length-offsetb

	if alen=0 or blen=0 then merror("CCINS") fi

	buffer:=pcm_alloc(alen)

	memcpy(buffer, destptr+offseta, alen)				!A to buffer
	memcpy(destptr+offseta, destptr+offsetb, blen)		!B to A's old offset
	memcpy(destptr+offseta+blen, buffer, alen)			!A to A's old offset + B.len

	pcm_free(buffer, alen)
end

global func addlocaltemp(int stackno, blockno)int=
	for i to nlocaltemps do
		if ltstackno[i]=stackno and ltblockno[i]=blockno then
			return i
		fi
	od
	++nlocaltemps
	if nlocaltemps>=maxlocaltemps then merror("Too many localtemps") fi
	ltstackno[nlocaltemps]:=stackno
	ltblockno[nlocaltemps]:=blockno
	nlocaltemps
end

global proc merror(ichar mess,ichar param="")=
	int lineno
	ichar filename, sourceline

!	if igetmsourceinfo then
!		lineno:=igetmsourceinfo(mmpos, filename, sourceline)
!		CPL =LINENO
!		CPL =FILENAME
!	else
CPL "NO LINE INFO"
		lineno:=0
		filename:="?"
!	end

!	if currfunc then
!		println "Proc:", currfunc.name
!	end

	fprintln "MCL Error: # (#) on Line: # in #",mess,param, lineno, filename
OS_GETCH()
	stop 1
!	pcerrorstop(filename, lineno)
end

=== mcl_decls.m 0 0 9/74 ===
global const maxoperands=16
!global [maxoperands]pclopndrec pstack
global [maxoperands]pcl	   pclopnds
global [maxoperands]pcsrec pclstack

!The following are reset per proc and augmented as it is processed
global [maxoperands]byte pcltempflags		!1 means a temp uses local storage
global [maxoperands]int pcltempoffsets	!for final fixups
global int npcltemps

global int noperands						!number of pcl operands, including wide

global macro zz = noperands
global macro yy = noperands-1
global macro xx = noperands-2
global macro ww = noperands-3

global int mstackdepth						!hw stack size (pcl operands, + extra for wide, + padding)

global const u64 invertbytes = 0x0101'0101'0101'0101

global const maxcalldepth=32
global [maxcalldepth]byte callalign		!pending 1-slot alignment for syscalls
global [maxcalldepth]byte callblockret	!1 if fnc returns a block
global [maxcalldepth]u32 callblocksize	!size of any returned block
global [maxcalldepth,4]u32 callargsize	!size of any block pushed in low args
global int ncalldepth

!global symbol currfunc
global psymbol blockretname
!global int mstackdepth						!hw stack size (pcl operands, + extra for wide, + padding)
global byte localshadow			!1 if local, proc-wide shadow space used for a call

global byte r10used				!these may be set in pass2 when occupied by params
global byte r11used

global const maxblocktemps=50
global [maxblocktemps]psymbol blockdefs
global int nblocktemps

global []int multregs=(r0,r1,r2,r10,r11,r12)
global []int multxregs=(r0,r1,r2,r3,r4,r5)

const maxworkreg = 8
const maxworkxreg = 8

!global u64 baseworkset				!default initial working set program-wide
!global u64 workset					!'1' bits mean available as workreg in this proc
global [maxworkreg]byte workregs		!contains set of work regs eg (r0 r1 r2 ...)
global [maxworkxreg]byte workxregs	!contains set of work xregs eg (xr4 ...)
global u64 regset					!'1' bits mean workreg currently in use
global u64 oldregset				!'1' bits mean workreg currently in use
global u64 usedset					!accumulates '1' bits from regset to show used regs in proc
global u64 regvarset				!'1' means holds a regvar
global u64 pclset					!'1' means reg hold pcl operand

global int nregvars, nxregvars		!how many are available from r9-r3, or xr15-xr8 etc

global const nbaseworkregs = 3		!program-wide minimums
global const nbaseworkxregs = 5

global int nworkregs				!no. of int workregs (base + extra)
global int nworkxregs				!no. of real workregs (base + possible extra)

!global [stdnames.bounds]byte ploadopx
global [stdnames.bounds]byte ploadop

proc start=
	for i in ploadop.bounds do ploadop[i]:=m_nop end

	ploadop[tu8]:=ploadop[tu16]:=ploadop[tu32]:=m_movzx
	ploadop[ti8]:=ploadop[ti16]:=ploadop[ti32]:=m_movsx
	ploadop[tr32]:=m_movd
	ploadop[tr64]:=m_movq
	ploadop[tu64]:=ploadop[ti64]:=m_mov
end

global pcl currpcl

global int pmode

global macro ispint(m) = pstdint[m]
global macro ispfloat(m) = pstdfloat[m]
global macro ispwide(m) = pstdwide[m]

=== mcl_gen.m 0 0 10/74 ===
!macro mdivider = mcomment("-"*40)

!INT PCLLENGTH
!INT NPROCS

global proc genmcl=
	^strbuffer asmstr
	psymbol d
	int tt

	return when mcldone

	tt:=os_clock()
!CPL "GENMCL"

	inithandlers()
	mclinit()
!CPL "GENMCL", $LINENO

	mcomment("Generated ASM")
!RETURN

	dolibfiles()
!CPL "GENMCL", $LINENO

!CPL "GENMCL", $LINENO
!CPL =STATICLIST
	d:=psymboltable
!CPL "GENMCL", $LINENO

	while d, d:=d.next do
!CPL "GENMCL", $LINENO, D.NAME
		if d.nameid=staticid then
!CPL "GENMCL", $LINENO, D.NAME
			dostaticvar(d)
!CPL "GENMCL", $LINENO
		fi
	end

!CPL "GENMCL", $LINENO
	mcomment("")

!import list not needed for AA; only for NASM

!	for i to ndllproctable do
!		gendllproc(dllproctable[i])
!	end

!CPL =PROCLIST
	d:=psymboltable

	while d, d:=d.next do
		if d.nameid=procid then
			genprocmcl(d)
		fi
	end
!CPL "DONE PROCS"

	genrealtable()
	genabsneg()
	genstringtable()
!
	genmc(m_endx)					!need as buffer in optimiser
	genmc(m_endx)

	mcldone:=1

	mcltime:=os_clock()-tt

	if mccode.opcode=m_nop then
		mccode:=mccode.nextmcl
	fi

!CPL "AFTER MCL:", MCLTIME

!CPL $LINENO


end

global proc genprocmcl(psymbol d) =
	mcl mstart

	currfunc:=d
	setsegment('C',1)

	genmc(m_procstart, genmemaddr(currfunc))
	genmc(m_labelname, genmemaddr(currfunc))
!CPL "GENPROC", CURRFUNC.NAME, CURRFUNC.ISMAIN, CURRFUNC.ISENTRY
!CPL "GENPROC", CURRFUNC.NAME, CURRFUNC.ISENTRY


	mcomment("?>>")
	mclprocentry:=mccodex

!	if currfunc.isentry and currfunc.nextparam and not pdcc then	!assume 2 args
!		fixmain()
!	end

	do_proccode_a()						!initialise offsets etc

	mstart:=mccodex

	currpcl:=d.pccode
	noperands:=0

	while currpcl, currpcl:=currpcl.next do
		convertpcl(currpcl)				!currpcl may be stepped in handler
		exit when currpcl=nil
	end

	if mclprocentry=mccodex then		!empty body: add dummy mcl op
		mcomment("---")					!injection of entry code goes wrong otherwise
	end

	do_proccode_b(mstart)				!do entry code (inject into start)
	do_proccode_c()						!do exit code

	genmc(m_procend)					!also tells optimiser when to stop

	if foptimise=2 then
		peephole(mstart)
	end

	if noperands then
		println currfunc.name,,": PCL stack not empty"
		mcomment("PCL stack not empty")
	end

PCLSET.[0]:=0
IF PCLSET THEN
CPL CURRFUNC.NAME, "PCLSET NOT EMPTY"
FI

!CPL "DONE", D.NAME
	currfunc:=nil

end

proc dolibfiles =
	mcomment("Lib files go here")
	mcomment("")
end

proc dostaticvar(psymbol d) =
	pcl pc

	return when d.isimport

!CPL "STATIC VAR", D.NAME

	if d.name^='$' and (d.name+1)^='$' then
		setsegment('C', 8)
	else
		setsegment((d.pccode|'I'|'Z'), getalignment(d.mode))
	end

	genmc(m_labelname, genmemaddr(d))

	if d.pccode then
!MCOMMENT("IDATA GOES HERE")
!		genidata(d.code)
		pc:=d.pccode

		while pc, pc:=pc.next do
			dodata(pc)
		end

	elsif d.size then
		genmc(m_resb, genint(d.size))
	end
end

proc dodata(pcl p) =
! Constant data. For block types, there can be multiple C values
	mclopnd ax
	int opc

	if p.mode=tblock then
		do_blockdata(p)
		return
	fi

	case p.opndtype
	when int_opnd then
		ax:=mgenint(p.value)
	when real_opnd then
		ax:=mgenrealimm(p.xvalue, p.mode)

	when string_opnd then
		ax:=genlabel(getstringindex(p.svalue, p.slength))

	when memaddr_opnd then
		ax:=genmemaddr(p.def)
		ax.offset:=p.extra
!CPL "SETTING OFFSET", AX.OFFSET, MSTROPND(AX)
	when label_opnd then
		ax:=genlabel(p.labelno)

	else
		merror("db/dq optype? #", opndnames[p.opndtype])
	esac

	case p.size
	when 1 then opc:=m_db
	when 2 then opc:=m_dw
	when 4 then opc:=m_dd
	when 8 then opc:=m_dq
	else
CPL =P.SIZE, =STRPMODE(P.MODE)
		merror("DATA/not 1248")
	esac
!
	genmc(opc,ax)

end

global proc do_blockdata(pcl p) =
	ref byte s
	ref u64 d
	int n,nqwords,nwords,r

	n:=p.size
	return when n=0

	nwords:=n/8

	d:=cast(p.svalue)
	to nwords do
		genmc(m_dq, mgenint(d++^))
	end

	r:=n-nwords*8
	if r then
		genstring_db(cast(d), r, 'B')
	fi
	MCOMMENT("ENDDATA")

end

proc inithandlers=
	static byte initdone=0
	ichar name, s
	int n

	if initdone then return end

	n:=$getnprocs()

	for i to n do
		name:=$getprocname(i)
		if eqbytes(name,"px_",3) then
			for k in pclnames.bounds do
				s:=pclnames[k]
				if s^='k' then ++s fi				!some are kload, others just store
				if eqstring(s,name+3) then
					px_handlertable[k]:=$getprocaddr(i)
					exit
				end
			else
				merror("Invalid handler name:",name)
			end
		end
	end

	static [,2]byte dupltable = (

!mapping           =>
		(ktoboolf, 		ktoboolt),

		(kcallf,		kcallp),
		(kicallp,		kcallp),
		(kicallf,		kcallp),

		(kendmx,		kresetmx),

!		(kidivto,		kidiv),
!		(kiremto,		kirem)
		)

	for i to dupltable.len do
		px_handlertable[dupltable[i,1]]:=px_handlertable[dupltable[i,2]]
	end

	for i in px_handlertable.bounds do
		if not px_handlertable[i] then
			px_handlertable[i]:=cast(&unimpl)
		end
	end

	initdone:=1
end

global proc doshowpcl(pcl p)=
	[1256]char str

!*!	return unless fshowinlinepcl

!	return when p.opcode in [kcomment, klabel]
	return when p.opcode in [kcomment, klabel, ksetcall, ksetarg]

	strcpy(str,"               ")
	strcat(str,strpclstr(p, str.len))
	mcomment(PCM_COPYHEAPSTRING(str))
end

global proc unimpl(pcl p)=
	[100]char str
	fprint @str, "Unimpl: # (#)", pclnames[p.opcode], strmode(pmode)
	CPL STR
	mcomment(pcm_copyheapstring(str))
end
=== mcl_genaux.m 0 0 11/74 ===


!global int nsaveregs, nsavefregs		!number of integer/float non-vols to be saved
!global int nspilled						!spilled int/float registers

global int framesize					!counts bytes in stack frame
!global int framebytes, frameoffset, paramoffset
!global int paramstackoffset
!global int paramspilloffset

psymbol dblockarg

int nsavedregs, nsavedxregs
!global int retindex

global [20]psymbol nametable
global int nnametable

record inforec=
	byte nargs			!regvar-eligible int args
	byte nxargs			!regvar-eligible float args
	byte nlocals		!regvar-eligible int locals
	byte nxlocals		!regvar-eligible int locals

	byte isleaf			!1 when there are no calls (explicit or implicit)
	byte maxargs		!maxargs either incoming or (via calls/rts) outgoing args
	byte hasblocks		!1 if code involves any blocks other than 16-byte ones
	byte spare			!1 if code involves any blocks other than 16-byte ones
end

inforec info
INT PCLCOUNT


global proc do_proccode_a=
	psymbol d
	int reg, xreg, n, r, npregs, retmode, nextreg, nextxreg
	int nargregs, nargxregs, spare, nextargreg, nextargxreg

!	clear pcltempflags
	npcltemps:=0

	nblocktemps:=0
	r10used:=r11used:=0
	usedset:=0
	regset:=usedset:=regvarset:=0

	pclset:=0
	nsavedregs:=nsavedxregs:=0
	nregvars:=nxregvars:=0

	nworkregs:=nbaseworkregs
	nworkxregs:=nbaseworkxregs

!MCOMMENT("PROCA")
	retmode:=currfunc.mode

	if retmode=tblock then
!MERROR("RETBLOCK")
		d:=pmakesymbol("$1x", paramid)
		d.mode:=currfunc.mode
		d.size:=currfunc.size
		d.used:=1
		d.nextparam:=currfunc.nextparam
		currfunc.nextparam:=d
		++currfunc.nparams
		blockretname:=d
	end

	retindex:=createfwdlabel()

!set up inforec; done after any extra arg for block returns has been done

!for now, add extra 2 int workregs manually
!IF DEBUG THEN
!CPL "PROCA***************"
!FI
	SCANPROCINFO()

	if foptimise=0 then		!allow 5 workregs always
!		clear info				!set .isleaf to 0 etc
		workregs[++nworkregs]:=r3
		workregs[++nworkregs]:=r4
		return
	end

!optimising for regvars
!	scanprocinfo()

!IF DEBUG THEN
!CPL CURRFUNC.NAME,,":"
!CPL =PCLCOUNT
!CPL =INFO.NARGS, =INFO.NXARGS
!CPL =INFO.NLOCALS, =INFO.NXLOCALS
!CPL =INFO.ISLEAF
!CPL =INFO.MAXARGS
!CPL =INFO.HASBLOCKS
!CPL =FOPTIMISE
!END
!set up extra 1 or 2 workregs
!INFO.HASBLOCKS:=1
!IF DEBUG THEN CPL $LINENO,":", INFO.MAXARGS FI


	if info.hasblocks then			!need 2 extra
!IF DEBUG THEN CPL $LINENO,":", INFO.MAXARGS FI
		nworkregs:=5
		if info.maxargs<=2 then		!both in pregs
			workregs[4]:=r13
			workregs[5]:=r12
			nregvars:=7
		else
user3r4:
			workregs[4]:=r3
			workregs[5]:=r4
			nregvars:=5
		end
	else							!only one extra needed
!IF DEBUG THEN CPL $LINENO,":", INFO.MAXARGS FI
		nworkregs:=4
		if info.maxargs<=3 then
!CPL CURRFUNC.NAME,"USING R13"
			workregs[4]:=r13
			nregvars:=7
		else
			workregs[4]:=r3
			nregvars:=6
		end
	end
!IF DEBUG THEN CPL $LINENO,":", INFO.MAXARGS FI

	nxregvars:=12-nworkxregs
!IF DEBUG THEN CPL $LINENO,":", INFO.MAXARGS FI

!IF DEBUG THEN
!CPL CURRFUNC.NAME, =NWORKREGS, =NREGVARS, =NWORKXREGS, =NXREGVARS, =INFO.MAXARGS
!FI

!Allocate registers to params and locals

!	nextreg:=r9
	nextreg:=r9-nregvars+1
	nextxreg:=xr15
	nextargreg:=r10-1				!for leaf
	nextargxreg:=xr0-1				!for leaf

	nargregs:=2

	nargregs:=info.nargs			! 0-4
	spare:=nregvars-info.nlocals		! will be negative, or 0-nregvars
	if nargregs in 3..4 then
		if spare<3 then
			nargregs:=2
		elsif spare=3 then			!else allow all 3/4 argregs
			nargregs:=3
		end
	end

	nargxregs:=2					!just stick to first 2 for now

!CPL CURRFUNC.NAME:":"
!CPL $,=INFO.NARGS
!CPL $,=INFO.NLOCALS
!CPL $,=NREGVARS
!CPL $,=SPARE
!CPL $,=NARGREGS, =NREGVARS-NARGREGS

!IF EQSTRING(currfunc.name, "mandel") then	CPL "MANDEL SEEN"; NARGREGS:=0 end
!IF EQSTRING(currfunc.name, "mandel") then	CPL "MANDEL SEEN"; INFO.ISLEAF:=0 end
!IF EQSTRING(currfunc.name, "create_fractal") then	CPL "CREATEF SEEN"; NARGREGS:=0 end

!scan params first
	d:=currfunc.nextparam

	while d, d:=d.nextparam do
		++nextargxreg
		++nextargreg

		if d.regcand and nargregs then
			if info.isleaf then
				regvarset.[nextargreg]:=1
				if nextargreg=r10 then r10used:=1 end
				if nextargreg=r11 then r11used:=1 end
				d.reg:=nextargreg
			else
				regvarset.[nextreg]:=1
				usedset.[nextreg]:=1
				d.reg:=nextreg

!					--nextreg
				++nextreg
				--nregvars
				--nargregs
			end

		elsif d.xregcand and nxregvars then
			if info.isleaf then
				regvarset.[nextargxreg]:=1
				d.reg:=nextargxreg
			else
				regvarset.[nextxreg]:=1
				usedset.[nextxreg]:=1
				d.reg:=nextxreg

				--nextxreg
				--nxregvars
				--nargxregs
			end

		end
	end
!CPL "DONE PM"

	d:=currfunc.nextlocal
	while d, d:=d.nextlocal do
		if d.regcand and nregvars then
			regvarset.[nextreg]:=1
			usedset.[nextreg]:=1
			d.reg:=nextreg

!			--nextreg
			++nextreg

			--nregvars

		elsif d.xregcand and nxregvars then
			regvarset.[nextxreg]:=1
			usedset.[nextxreg]:=1
			d.reg:=nextxreg

			--nextxreg
			--nxregvars

		end
!CPL =D.NAME, D.REG
	end

end

global proc do_proccode_b(mcl mstart)=
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
	mclopnd ax, bx
	psymbol d
	[100]char str, newname
	int r, n, rr
	u64 oldusedset
	mcl mm

	setmclentry(mclprocentry)
!CPL "PROCB1", CURRFUNC.NAME, USEDSET.[RFRAME], USEDSET


	framesize:=0
	dblockarg:=nil

	for r in r3..r9    when usedset.[r] do ++nsavedregs end
	for r in xr6..xr15 when usedset.[r] do ++nsavedxregs end

	nsavedbytes:=(nsavedregs+nsavedxregs)*8


!allocate offsets to args, and set defines

	paramoffset:=16+nsavedbytes		!between top of stackframe and 1st param is fp/retaddr/saved

	oldusedset:=usedset				!need to ignore rframe used in defines

	d:=currfunc.nextparam
	while d, d:=d.nextparam do
		if d.reg then
			paramoffset+:=8
			defreg(d)
		else
			d.offset:=paramoffset
			paramoffset+:=8
			genmc_def(m_define, d)
		end
	end

	d:=currfunc.nextlocal
	while d, d:=d.nextlocal do
		if d.reg then
			defreg(d)
		else
			framesize+:=roundsizetg(d.size)
			d.offset:=-framesize
			genmc_def(m_define, d)
		end
	end

!	for i to maxoperands when pcltempflags[i] do
	for i to npcltemps do
		framesize+:=8
		pcltempoffsets[i]:=-framesize
		genmc(m_definetemp, genname(gettempname(currfunc,i)), genint(-framesize))
	end







!CPL =FRAMESIZE, $LINENO

	usedset:=oldusedset

!CPL =INFO.ISLEAF
	if not info.isleaf then
		framesize+:=32									!shadow space
	end
	int adjust:=0
!CPL =FRAMESIZE, $LINENO

	if not usedset.[rframe] then					!rframe not pushed
		adjust:=8
	end

!CPL =ADJUST


	unless (framesize+nsavedbytes+adjust+8) iand 8 then			!keep stack frame 16-byte aligned
		UNLESS INFO.ISLEAF THEN
			framesize+:=8
		END
	end
!CPL =FRAMESIZE, $LINENO

!CPL =FRAMESIZE

	savevolregs(nsavedregs, nsavedxregs)

!generate stack entry code proper:

!CPL "PROCB2", CURRFUNC.NAME, USEDSET.[RFRAME], USEDSET

	IF USEDSET.[RFRAME] THEN
		genmc(m_push, dframeopnd)
		genmc(m_mov, dframeopnd, dstackopnd)
	FI
	pushstack(framesize)

!spill any args to shadow space
	spillparams()

	resetmclentry()

!FIXUP UP ALL MCL TEMP OPNDS WITH FINAL OFFSETS
!....
	return unless npcltemps

	mm:=mstart
	while mm, mm:=mm.nextmcl do
		ax:=mm.a
		if ax and ax.valtype=temp_val then
			ax.offset:=pcltempoffsets[ax.tempno]
		end

		ax:=mm.b
		if ax and ax.valtype=temp_val then
			ax.offset:=pcltempoffsets[ax.tempno]
		end

	end
end

global proc do_proccode_c=
	int offset
	mclopnd ax, bx

!	genmc(m_label, genlabel(retindex))
	genmc_lab(retindex)

	popstack(framesize)
	IF USEDSET.[RFRAME] THEN
		genmc(m_pop, dframeopnd)
	FI
	restorevolregs(nsavedregs, nsavedxregs)

	genmc(m_ret)
end

proc start=
	int r

!initialise program-wide workreg maps
!They will be augmented on a per-function basis
	r:=r0
	for i to nbaseworkregs do
		workregs[i]:=r++
	end

	r:=xr4
	for i to nbaseworkxregs do
		workxregs[i]:=r++
	end
end

proc spillparams=
	psymbol d
	mclopnd ax
	int offset:=16, regoffset:=0, xregoffset, firstoffset, mode

	regoffset:=0

	d:=currfunc.nextparam

	if currfunc.cvariadic then				!C proc def using ...
!CPL =CURRFUNC.NPARAMS, =D.OFFSET

!MERROR("SPILLPARAMS/VARIADIC")
		firstoffset:=d.offset				!param offsets may be pushed up

		for i:=currfunc.nparams to 3 do				!0-based; if nparams=2, loops over 2..3 as 0..1 are normal
			ax:=genindex(areg:rframe, size:8, offset:i*8+firstoffset)
			genmc(m_mov, ax, genreg(i+r10))
		end
	end
!
	while d, d:=d.nextparam do
		if regoffset>3 then exit end

!		if d.used or regoffset=0 then
		if d.used then
			mode:=d.mode
			if not d.reg then
				ax:=genindex(areg:rframe, size:8, offset:d.offset)
				case mode
				when tr64 then
					genmc(m_movq, ax, genreg(regoffset+xr0))
				when tr32 then
					genmc(m_movd, changeopndsize(ax, 4), genxreg(regoffset+xr0, 4))
				else
					genmc(m_mov, ax, genreg(regoffset+r10))
				end case
			elsif d.reg>=xr4 then			!xregvar, not in-situ param
					genmc(m_movq, genxreg(d.reg), genxreg(regoffset+xr0))

			elsif d.reg<=r9 then			!regvar, not in-situ param
				genmc(m_mov, genreg(d.reg), genreg(regoffset+r10))
			end
		end

		offset+:=8
		++regoffset
	end
end

proc savevolregs(int nregs, nxregs)=
	int reg
	mclopnd ax

	if nregs then
		for r in r3..r9 when usedset.[r] do
			genmc(m_push, genreg(r))
			exit unless --nregs
		end
	end

	if nxregs then
		ax:=genreg(r0)
		for r in xr6..xr15 when usedset.[r] do
			genmc(m_movq, ax, genreg(r))
			genmc(m_push, ax)
			exit unless --nxregs
		end
	end
end

proc restorevolregs(int nregs, nxregs)=
	mclopnd ax

	if nxregs then
		ax:=genreg(r10)
		for r:=xr15 downto xr6 when usedset.[r] do
			genmc(m_pop, ax)
			genmc(m_movq, genreg(r), ax)
			exit unless --nxregs
		end
	end

	if nregs then
		for r:=r9 downto r3 when usedset.[r] do
			genmc(m_pop, genreg(r))
			exit unless --nregs
		end
	end
end

proc gendq(int a)=
	genmc_int(m_dq, a)
end

global proc genabsneg=
	if lababs32+lababs64+labneg32+labneg64 then
		setsegment('I', 16)
	end

	if lababs32 then
		mcomment("lababs32")
		genmc_label(m_label, lababs32)
		gendq(0x7FFF'FFFF'7FFF'FFFF)
		gendq(0x7FFF'FFFF'7FFF'FFFF)
	end
	if lababs64 then
		mcomment("lababs64")
		genmc_label(m_label, lababs64)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
	end

	if labneg32 then
		mcomment("labneg32")
		genmc_label(m_label, labneg32)
		gendq(0x8000'0000'8000'0000)
		gendq(0x8000'0000'8000'0000)
	end
	if labneg64 then
		mcomment("labneg64")
		genmc_label(m_label, labneg64)
		gendq(0x8000'0000'0000'0000)
		gendq(0x8000'0000'0000'0000)
	end

	if labzero then
		mcomment("labzero")
		genmc_label(m_label, labzero)
		gendq(0)
	end

	if labmask63 then
		mcomment("mask63/offset64")
		genmc_label(m_label, labmask63)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		genmc_label(m_label, laboffset64)
		gendq(0x43E0'0000'0000'0000)
	end
end

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

!proc setmclentryf(mcl p)=
!!temporarily set mcl insertion before p
!
!	mcf_oldmccodex:=mccodex
!	mccodex:=p
!	mcf_lastmcl:=p.lastmcl
!	mcf_nextmcl:=p.nextmcl
!end
!
!func resetmclentryf:mcl pnew =
!!restore mcl insertion point to normal
!!restireturn mcl instruction that followed	
!	mccodex.lastmcl:=mcf_lastmcl
!	mccodex.nextmcl:=mcf_nextmcl
!	pnew:=mccodex
!	mccodex:=mcf_oldmccodex
!	pnew
!end

global proc copyblock(mclopnd ax,bx, int n)=
!ax, bx refer to memory; do ax:=bx for n bytes

	mclopnd rx, rcount
	int nwords, lab, oddbytes, offset, workreg, countreg, axreg

	if n=16 then
		rx:=genreg(gwrx())
		genmc(m_movdqu, rx, bx)
		genmc(m_movdqu, ax, rx)
		return
	end

	oddbytes:=n rem 8		!will be zero, or 1..7
	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of u64s (ie. octobytes)

	rx:=genreg(gwri())		!work reg

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
		end

	elsif nwords then			!use a loop
		rcount:=genreg(gwri())
		lab:=++mlabelno

		ax.size:=8

		genmc(m_mov, rcount, mgenint(nwords))
		genmc(m_label, genlabel(lab))
		genmc(m_mov, rx, bx)
		genmc(m_mov, ax, rx)

		genmc(m_add, mgenreg(ax.reg), mgenint(targetsize))
		genmc(m_add, mgenreg(bx.reg), mgenint(targetsize))

		genmc(m_dec, rcount)
		genmc_cond(m_jmpcc, ne_cond, genlabel(lab))

		offset:=0
	end

	if oddbytes then
		n:=oddbytes						!1..7

		if n>=4 then
			rx:=changeopndsize(rx, 4)
			genmc(m_mov, rx, applyoffset(bx, offset, 4))
			genmc(m_mov, applyoffset(ax, offset, 4), rx)
			n-:=4
			offset+:=4
		end
		if n>=2 then
			rx:=changeopndsize(rx, 2)
			genmc(m_mov, rx, applyoffset(bx, offset, 2))
			genmc(m_mov, applyoffset(ax, offset, 2), rx)
			n-:=2
			offset+:=2
		end
		if n=1 then
			rx:=changeopndsize(rx, 1)
			genmc(m_mov, rx, applyoffset(bx, offset, 1))
			genmc(m_mov, applyoffset(ax, offset, 1), rx)
		end
	end
end

proc scanprocinfo=
	pcl pc
	psymbol d
	int nargs, nparams, m

	clear info
	info.isleaf:=1
	pclcount:=0

	pc:=currfunc.pccode

!CPL "SCANPROCINFO", CURRFUNC.NAME

	while pc, pc:=pc.next do
		++pclcount

		if pc.mode=tblock and pc.size<>16 then info.hasblocks:=1 end

		nargs:=pclargs[pc.opcode]
		if nargs then
			info.isleaf:=0
			if nargs=9 then nargs:=pc.nargs fi		!is a call
			info.maxargs max:= nargs
		end

		if pc.optype=memaddr_opnd then
			unless pc.opcode=kload and pc.inplace then
				pc.def.addrof:=1
			end
		end

	end

!need to potential regvars in 4 categories: int/float params/locals

	d:=currfunc.nextparam
	nparams:=0

	while d, d:=d.nextparam do
		if nparams<4 and d.used and not d.addrof then
			m:=d.mode
			if pstdint[m] then
				++info.nargs
				d.regcand:=1
			elsif pstdfloat[m] then
				++info.nxargs
				d.xregcand:=1
			end
		end
		++nparams
	end
	currfunc.nparams:=nparams
	d:=currfunc.nextlocal

	while d, d:=d.nextlocal do
		if d.used and not d.addrof then
			m:=d.mode
			if pstdint[m] then
				++info.nlocals
				d.regcand:=1
			elsif pstdfloat[m] then
				++info.nxlocals
				d.xregcand:=1
			end
		end
	end
!	d.nlocals:=nlocals

	info.maxargs max:=nparams

	if currfunc.cvariadic then			!args must all be spilled
		info.nargs:=info.nxargs:=0
	end

END

proc defreg(psymbol d) =
	int rr
	rr:=d.reg
	d.reg:=0
	genmc(m_definereg, genmem(d), mgenreg(rr, ttbasetype[d.mode]))
	d.reg:=rr
end
=== mcl_stack.m 0 0 12/74 ===
!PCL Operand Stack 

global func getopnd(int n, mode=pmode, reg=rnone)mclopnd ax =
!get access mode for operand n
	int oldreg, size
	mclopnd bx
	psymbol d

	pcl p:=pclopnds[n]
	pcsrec ps:=pclstack[n]

	size:=stdsize[mode]

!CPL "GETOPND", =PS.REG
	if ps.reg then
		oldreg:=ps.reg
		if reg and oldreg<>reg then
			ax:=genreg(reg, size)
			bx:=genreg(oldreg, size)
			REGSET.[OLDREG]:=0
			PCLSET.[OLDREG]:=0
			PCLSET.[REG]:=1
			genmc(m_mov, ax, bx)
		else
			ax:=genreg(oldreg, size)
		end

	elsif ps.temp then
		ax:=mgentemp(ps.temp, mode)

	elsecase p.optype

	when mem_opnd then
		d:=p.def
		if mode=tblock and d.nameid<>paramid then
			mode:=tu64
			recase memaddr_opnd
		else
			ax:=genmem(p.def, size)
		end

	when memaddr_opnd then
		d:=p.def
		if d.nameid=paramid and d.mode=tblock then		!pcl mode will be u64
			ax:=genmem(d, size)
		else
			ax:=gwrm(mode, reg)
			genmc(m_lea, ax, genmem(d, size))
		end

	when int_opnd then

		case size
		when 2 then
			p.value iand:=0xFFFF
		when 4 then
			p.value iand:=0xFFFF'FFFF
		end case

		bx:=genint(p.value, size)
		if p.value in i32.bounds then			!keep as immediate
			ax:=bx
		else
			ax:=gwrm(mode, reg)
			genmc(m_mov, ax, bx)
		end

	when real_opnd then
		ax:=genrealmem(p.xvalue, size)

	when string_opnd then
		ax:=gwrm(tu64, reg)
		genmc(m_lea, ax, genlabelmem(getstringindex(p.svalue, p.slength)))

	when label_opnd then
		ax:=gwrm(tu64, reg)

		genmc(m_lea, ax, genlabelmem(p.labelno))
!
	else
error:
		merror("getopnd", opndnames[p.optype])
	end

	if ax.mode=a_reg then		!ensure correctly set to reg
		pclstack[n].reg:=ax.reg
		pclstack[n].temp:=0
		pclstack[n].code:=0
		pclset.[ax.reg]:=1
	end

	ax
end

global func loadopnd(int n, mode=pmode, reg=rnone)mclopnd ax =
!must return with value in register. But it can be in-situ if it is a regvar
!and no newreg is specified
	pcl p:=pclopnds[n]
	pcsrec ps
	psymbol d
	int reg0:=reg

	int size:=stdsize[mode]
	mclopnd bx

!CPL "LOAD1"
	ax:=getopnd(n, mode, reg)
!CPL "LOAD2"

	ps:=pclstack[n]
	if ps.reg then
		if regvarset.[ps.reg] then		!needs copying
			finish
		end
		return ax
	end

	if reg=rnone then
		if pstdfloat[mode] then
			bx:=genxreg(reg:=gwrx(), size)
		else
			bx:=genreg(reg:=gwri(), size)
		end
	else
		bx:=genreg(reg, size)
	end

	if ps.temp then
		genmc(m_mov, bx, ax)
		pcltempflags[ps.temp]:=0

	elsecase p.optype
	when mem_opnd, real_opnd  then
		genmc(m_mov, bx, ax)

	when int_opnd then
		if ax.value=0 then
			clearreg(bx)
		else
			genmc(m_mov, bx, ax)
		end


	when memaddr_opnd then
		d:=p.def
		if d.nameid=paramid and d.mode=tblock then		!pcl mode will be u64
			ax:=genmem(d, size)
			genmc(m_mov, bx, ax)
		else
			bx:=gwrm(mode, reg)
			genmc(m_lea, bx, genmem(d, size))
		end

	when string_opnd then
!		ax:=genreg(getworkireg())
!		genmc(m_lea, ax, genlabelmem(getstringindex(p.svalue)))
	else
error:
		merror("loadopndr", opndnames[p.optype])
	end

	pclstack[n].reg:=reg
	pclstack[n].temp:=0
	pclstack[n].code:=0
	pclset.[reg]:=1

	ax:=bx
	int newreg

finish:
	reg:=ax.reg

!don't need a value if the reg is regvar belong to an in-situ argument
!other code will have taken care of this (mainly to do with using CL or RDX)

!	if regvarset.[reg] and reg not in r10..r13 and reg not in xr0..xr3 then			!need to copy elsewhere
	if regvarset.[reg] and reg<>reg0 then			!need to copy elsewhere
!	if regvarset.[reg]  then			!need to copy elsewhere
		bx:=gwrm(mode)
		newreg:=bx.reg

		genmc(m_mov, bx, ax)
	
		pclset.[reg]:=0
		pclset.[newreg]:=1

		pclstack[n].reg:=newreg
		bx
	else
		ax
	end
end

global func loadparam(int n, mode=pmode, reg)mclopnd ax =
	loadopnd(n, mode, reg)
end

global proc pushpcl(pcl pc)=
!Push a inline operand from pcl code to pcs
!addrof is 1 to apply & to name units, creating a memaddr opnd
	int n
	pcl p
	pcsrec ps
	psymbol d
!
	if noperands>=maxoperands then
		merror("PCL stack overflow")
	end

	n:=++noperands

!	if pc.optype=mem_opnd and (reg:=pc.def.reg) then
	if pc.optype=mem_opnd then
		d:=pc.def

		if d.reg then
			ps.reg:=d.reg
			ps.temp:=ps.code:=0
			finish
		end
	end

	pclopnds[n]:=pc
	ps.code:=1
	ps.reg:=ps.temp:=0

finish:

	ps.mode:=pc.mode
	ps.count:=1

	pclstack[n]:=ps
!CPL "PUSHPCL", PCLSTACK[N].REG
end

global proc pushopnd(int n, mode)=
!Push opnd n to hardware stack then pop it from pcl stack
!The hardware stack is popped on return from a call

	mclopnd ax, bx
	pcl p:=pclopnds[n]
	pcsrec ps:=pclstack[n]

	if mode=tvoid then mode:=p.mode end

!First look for operands that can be directly pushed without using a register
	if ps.code then
		case p.optype
		when mem_opnd then
			if p.size=8 then
				ax:=genmem(p.def, p.size)
				pushit
			end

		when int_opnd then
			if p.value in i32.bounds then		!fits in d32 offset
!				ax:=genint(p.value, 4)
!				ax:=genint(p.value IAND 0XFFFFFFFF, 4)
				ax:=genint(p.value, TI64)
				pushit
			end

		when real_opnd then
			if p.size=8 then
				ax:=genrealmem(p.xvalue)
				pushit
			end
		end case
	end

!!need to go via register

	ax:=loadopnd(n, mode)

	if ax.reg>=xr0 then			!float register
		bx:=ax
!		ax:=genreg(getworkireg(), p.size)
		ax:=gwrm((ax.size=4|tu32|tu64))
		genmc(m_mov, ax, bx)
	end

pushit:
	genmc(m_push, changeopndsize(ax,8))
	poppcl()
	++mstackdepth

end

global proc pushpcl_reg(int mode, reg=rnone)=
!Push a new, empty pcs slot located in given register
	int n
	pcsrec ps

	if noperands>=maxoperands then
		merror("PCL stack overflow")
	end

	if reg=rnone then reg:=gwr(mode) end

	n:=++noperands

	ps.all:=0
	ps.reg:=reg
	ps.count:=1
	ps.mode:=mode
	pclstack[n]:=ps

	regset.[reg]:=1
	pclset.[reg]:=1

end

global proc poppcl=
	pcsrec ps


	if noperands<=0 then merror("poppcl/underflow") end
	ps:=pclstack[noperands]

	if ps.count>1 then
		--pclstack[noperands].count
		return
	end

	if ps.temp then
CPL "FREEING TEMP ON POPPCL"
		pcltempflags[ps.temp]:=0
	fi

	pclset.[ps.reg]:=0		!will clear bit 0 if no reg used

	--noperands
end

global proc duplpcl=
!ensure zz is in a register, duplicate into a new register
	int mode:=pclstack[zz].mode

	loadopnd(zz, mode)							!get zz to reg
	pushpcl_reg(mode)							!create new zz opnd, old is now yy

	genmc(m_mov, getopnd(zz, mode), getopnd(yy, mode))	!copy old to new
end

global func gwri(int r=rnone)int =
	if r then return r end

	to 10 do
		for i to nworkregs do
			r:=workregs[i]
			if regset.[r]=0 then
				regset.[r]:=1
				usedset ior:=regset
				return r
			end
		end
		savenextopnd()
	end
	merror("No more work regs")
	0
end

global func gwrx(int r=rnone)int=
	if r then return r end

	for i to nworkxregs do
		r:=workxregs[i]
		if regset.[r]=0 then
			regset.[r]:=1
!			usedset.[r]:=1
			usedset ior:=regset
			return r
		end
	end
	merror("No more work xregs")
	0
end

global func gwr(int mode=pmode, reg=rnone)int =
	if regvarset.[reg] then
		reg:=rnone
	end

	if pstdfloat[mode] then
		gwrx(reg)
	else
		gwri(reg)
	end
end

global func gwrm(int mode=pmode, reg=rnone)mclopnd =

	if reg=rframe then reg:=rnone end

	mgenreg(gwr(mode, reg), mode)
end

global proc saveopnd(int n, allregs=1)=
!if operand is in a volatile register, then save it in a temp
!allregs=1 to save both A and B regs (vol/nonval), which can include P regs if
!used as workregs; this is to save pne reg=opnd to a temp to free up a register
!allregs=0 to limit to A regs (possibly some P regs) only; normall for CALLs
!in order to preserve non-vol regs, as call will preserve the rest

!NOTE: operands that are unlikely to be unchanged from their value in
!pclrec, could be revert to unit_loc. Eg. immediates, addresses, or any
!variable that is immutable

	int reg, size, temp
	mclopnd tx
	pcsrec ps:=pclstack[n]

	reg:=ps.reg

	if reg=rnone or regvarset.[reg] then return end

	size:=stdsize[ps.mode]

	IF PS.COUNT>1 THEN
CPL "SAVEOPND/COUNT>1"
	END

	temp:=getnexttemp()

	if pstdint[ps.mode] then
		if allregs or reg not in r3..r9 then
			genmc(m_mov, gentemp(temp, size), genreg(reg, size))
		end
	else
		if allregs or reg in xr0..xr5 then
			genmc(m_mov, gentemp(temp, size), genxreg(reg, size))
		end
	end
	regset.[reg]:=0
	pclset.[reg]:=0

	ps.reg:=ps.code:=0
	ps.temp:=temp
	pclstack[n]:=ps
end
!
global proc saveopnds(int n=0)=
!save all operands other than top n
!assume this is to do with calls

	for i:=1 to noperands-n do
		saveopnd(i,0)
	end
end

global proc savenextopnd=
!starting from the first loaded, look for and save first reg-based opnd
!this does A/B/P regs if used
	int reg

	for i:=1 to noperands do
		reg:=pclstack[i].reg

		if reg and not regvarset.[reg] and pstdint[pclstack[i].mode] then
			saveopnd(i,1)
			return
		end
	end
end

global proc movetoreg(int newreg)=
!move top of stack (assumed to be in reg) to newreg
!assumes integer reg
	int oldreg, size
	pcsrec ps

	loadopnd(zz, pclstack[zz].mode)

retry:

	ps:=pclstack[zz]
	oldreg:=ps.reg
	size:=stdsize[ps.mode]

	if oldreg=newreg then
		return
	end

	if pstdfloat[ps.mode] then
		if regset.[newreg] then
			MERROR("MOVE TO REG: XREG IN USE")
		end
	elsif regset.[newreg] then
		for i to noperands do
			if pstdint[ps.mode] and pclstack[i].reg=newreg then
				swapopnds(i,zz)
				genmc(m_xchg, genreg(oldreg), genreg(newreg))
				retry
			end
		end
	end

	genmc(m_mov, genreg(newreg, size), genreg(oldreg, size))

	pclstack[zz].reg:=newreg

	regset.[oldreg]:=0
	regset.[newreg]:=1
	pclset.[oldreg]:=0
	pclset.[newreg]:=1
end

global proc swapopnds(int m,n)=
!exchange pcl stack operands
	pclrec t

	swap(pclopnds[m], pclopnds[n])
	swap(pclstack[m], pclstack[n])
end

global proc setnewzz(int mode, reg)=
!some value has been into into given register
!create replace pcl[zz] with that new operand
!assume pclcount was 1 and stays at 1
	pcsrec ps

	ps:=pclstack[zz]

IF REGVARSET.[REG] THEN
MERROR("SETNEWZZ/REGVAR")
FI

	pclset.[ps.reg]:=0

	ps.reg:=reg
IF PS.TEMP THEN CPL "SETNEWZZ WAS TEMP" FI
	ps.temp:=ps.code:=0
	ps.mode:=mode
	pclset.[reg]:=1

	pclstack[zz]:=ps
end

global func stropndstack(int indent=0)ichar=
	static [768]char str
	[512]char str2
	ichar s:=str, t
	pcl p
	pcsrec ps
	const w=24

	strcpy(s, "")

	for i to noperands do
		p:=pclopnds[i]
		ps:=pclstack[i]

		if i=1 then
			strcat(s, " "*(w))
		else
			strcat(s, ";"+" "*w)
		end
		strcat(s, strint(i,"2"))
		strcat(s, " ")
		strcat(s, (noperands-i+1|"Z:", "Y:", "X:", "W:"|"  "))

		if ps.reg then
			strcpy(str2, strreg(ps.reg))

		elsif ps.temp then
			print @str2, "T",,i

		elsecase p.opndtype
		when int_opnd then
			print @str2, p.value

		when real_opnd then
			print @str2, p.xvalue

		when mem_opnd then
domem:
			print @str2, p.def.name

		when memaddr_opnd then
			print @str2, "&",,p.def.name

		when string_opnd then
			if p.slength<10 then
				strcpy(str2, """")
				newconvertstring(p.svalue, &str2[2], p.slength)
				strcat(str2, """")
			else
				strcpy(str2, """<string>""")
			end

		else
			print @str2, "<",,opndnames[p.opndtype],,">"
		end

		to 12-strlen(str2) do
			strcat(str2, " ")
		end
		strcat(s, str2)

		if ps.count>1 then
			strcat(s, "*")
			strcat(s, strint(ps.count))
			strcat(s, " ")
		else
			strcat(s, "   ")
		end


		strcat(s, strmode(ps.mode))
!		strcat(s, " ")
		strcat(s, "\n")
	end

	u64 rset

	for i in 1..2 do
		if i=1 then
			rset:=oldregset
			strcat(s, (noperands|";"|""))
			strcat(s, " "*w+" (")
		else
			rset:=regset
			strcat(s, ";"+" "*w+" (")
		end
		for r:=r0 to xr15 when i not in r10..xr3  do
			strcat(s, (rset.[r]|strreg(r)|"-"))
			if regvarset.[r] then
				strcat(s, "*")
			end
			if pclset.[r] then
				strcat(s, "p")
			end
			strcat(s, " ")

		end
		strcat(s,")\n")
	end
	return s
end

global proc showopndstack=
	mcomment(stropndstack(1))
end

global proc storeopnd(pcl p, int n)=
!store opnd n (usually zz) to p which is always mem_opnd
	psymbol d
	mclopnd ax, bx
!
	d := p.def

	bx := loadopnd(n, p.mode)
!
	if p.mode=tblock then
		ax:=genreg(gwri())
		genmc(m_lea, ax, genmem(d))
		ax:=genireg(ax.reg)

		bx:=genireg(bx.reg)
		copyblock(ax, bx, p.size)

	else
		bx:=changeopndsize(bx, p.size)
		if d.reg then
			ax:=mgenreg(d.reg, p.mode)
		else
			ax:=genmem(d, p.size)
		end
		if ax.reg<>bx.reg then
			genmc(m_mov, ax, bx)
		end
	end
end

global proc freeworkregs=
!copy of code in convert pcl
	regset:=pclset
end

global func getopnd_ind(int n=noperands, mode=pmode, mem=0)mclopnd=
!Get access mode to operand which is to be used as a pointer elsewhere
!So it needs first to in a register, if not already
!mem=1 if allowed to return memory address mode, like [abc] or [dfp+abc]
	pcl p:=pclopnds[n]
	pcsrec ps:=pclstack[n]
	psymbol d

!optimise for memaddr
	if mem and ps.code and p.opndtype=memaddr_opnd then
		d:=p.def
		unless d.nameid=paramid and ttbasetype[d.mode]=tblock then
			return mgenmem(d, mode)
		end
	end

	unless ps.reg then
		loadopnd(n, tu64)			!ps may be out of date
	end

	return mgenireg(pclstack[n].reg, mode)
end

global func makeopndind(mclopnd a, int mode=pmode)mclopnd=
	mclopnd b

	if a.mode<>a_reg then
		merror("makeopndind")
	end

	return mgenireg(a.reg, mode)
end

global proc swapopndregs(int reg2)=
!top operand is in a register. Need to switch its reg with whatever is occupying
!reg2
!Note that swapondregs is best assumed to invalidate all existing mclopnds that
!refer to registers, as stuff if moved aound
!Also invalided are workregs that might use reg2, even if no mclopnd exists for it

	if not ispint(pclstack[zz].mode) then merror("SOR1") end

!assume int regs

	int reg1:=pclstack[zz].reg

	if reg1=reg2 then return end

	for i:=noperands-1 downto 1 do
		if pclstack[i].reg=reg2 then
			swap(pclstack[zz].reg, pclstack[i].reg)
			return
		end
	else
!pcl op not found that occupies reg2, so it is assumed to be a work register
!that is no longer needed. If it /is/ needed

		regset.[reg1]:=0				!make available (although done for next pcl op anyway)
		pclstack[zz].reg:=reg2
		pclset.[reg2]:=1
!		merror("swapopndregs/reg not found")
	end
end

global func getopnd_ind_simp(int n=noperands, mode=ti64)mclopnd ax=
!version of getopnd_ind which always returns [reg]
	ax:=loadopnd(n, mode)
	return mgenireg(ax.reg, mode)
end

global func getsharereg(int mode, mclopnd ax)int=
!if ax includes reg/regix, then try and use them
!return 0 if not reg available or not possibe
	byte reg:=ax.reg, regix:=ax.regix

	if ispfloat(mode) then return 0 end

!	if reg and (workset.[reg] or reg in r10..r13) then			!not a regvar
	if reg and not regvarset.[reg] then			!not a regvar
		return reg
	elsif regix and not regvarset.[regix] then
		return regix
	end

	return 0
end

global func getmemreg(mclopnd px)int reg=
!px is a memory addr mode like [d], [R], [R+d] etc. but the latter can also include Rframe
!return any main register used provided it is not Rframe. It does not use .regix
	if px.reg=rframe then
		rnone
	else
		px.reg				!will be rnone if not in use
	end
end

global func makesimpleaddr(mclopnd ax)mclopnd bx=
!assume ax is an ireg, but need a simple one with areg set but not ireg
	int newreg, reg, regix

	if ax.mode<>a_mem then merror("MSA") end

	reg:=ax.reg
	regix:=ax.regix
	if reg=rframe then reg:=rnone end

	if reg and regix=rnone and not regvarset.[reg] then		!already simple
		return ax
	end

!need a new register: try and reuse existing one
	if reg and not regvarset.[reg] then
		newreg:=reg
	elsif regix and not regvarset.[regix] then
		newreg:=regix
	else
		newreg:=gwri()
	end

	bx:=mgenireg(newreg)

	genmc(m_lea, genreg(newreg), ax)
	return bx
end
=== mcr_decls.m 0 0 13/74 ===
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

	^byte codeptr		! executable code block (includes thunk/addr table)
	^byte idataptr		! initialised data block

	^[]mcxreloc	reloctable		! table of reloc entries
	^[]ichar		dllnames		! base names of imported dll files (no extension)
	^[]ichar		libnames		! base names of imported mcx files (no extension)
	^[]ichar		importnames		! names of imported symbols
	^[]ichar		exports			! names of exported symbols
	^[]byte		exportsegs		! segment where each is located
	^[]u64		exportoffsets	! offsets within each segment

	u64 entryoffset					! offset within code block where execution will start
									! value of 0xFFFFFFFF (is u32 in file) means not set

!The next section is filled in after loading

	^byte zdataptr				! zeroed data block
	int codexsize					! bytes in thunk/addr tables that follow code
	^[]u64		exportaddr		! fully fixed-up addresses of exported symbols (not in file)
	^[]i16		importxreftable	! map symbol index to global one

	ichar			filespec		!full path
	ichar			libname			!base name of library
	^byte		entryaddr		!start address (left at nil when entryoffset not set)
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
global [maxlibs]^librec	libtable
global [maxlibs]byte		librelocated		!1 when relocated
global [maxlibs]byte		libinitdone			!1 when entry point called
global int nlibs

!Global import tables

global [maxsymbols]ichar	symbolnametable	! Name of symbol
global [maxsymbols]byte		symboldefined	! 1 when fully resolved with address
global [maxsymbols]^void	symboladdress	! Abs address
global [maxsymbols]i16	symbollibindex	! Lib index where defined
global [maxsymbols]byte		symboldllindex	! DLL index of library where found
global int nsymbols

export int nsymimports=0, nsymexports=0
=== mcr_lib.m 0 0 14/74 ===
global enumdata [0:]ichar rsegmentnames =
	(no_seg=0,		$),
	(code_rseg,		$),
	(idata_rseg,		$),
	(zdata_rseg,		$),
	(rodata_rseg,	$),
	(impdata_rseg,	$),
end

global func readlibfile(ichar filespec, ^byte p)^librec plib=
!p points to an MCB block; scan that into an MCU descriptor (librec)

	librec lib
	u64 sig
	int dir,n,tablesize
	^byte q

	clear lib

	sig:=readu32(p)
	if sig<>mcxsig then
		println "Bad sig - not MCX file"
		stop 1
	end

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
		end

	when libs_dir then
		lib.nlibs:=n:=readu32(p)
		lib.libnames:=pcm_alloc(ichar.bytes*n)
		for i to n do
			lib.libnames[i]:=readstring(p)
		end
	when importsymbols_dir then
		lib.nimports:=n:=readu32(p)
		lib.importnames:=pcm_alloc(ichar.bytes*n)
		for i to n do
			lib.importnames[i]:=readstring(p)
		end

	when exportsymbols_dir then
		lib.nexports:=n:=readu32(p)
		lib.exports:=pcm_alloc(ichar.bytes*n)
		for i to n do
			lib.exports[i]:=readstring(p)
		end

	when exportsegs_dir then
		n:=readu32(p)
		lib.exportsegs:=pcm_alloc(n)
		for i to n do
			lib.exportsegs[i]:=readbyte(p)
		end

	when exportoffsets_dir then
		n:=readu32(p)
		lib.exportoffsets:=pcm_alloc(u64.bytes*n)
		for i to n do
			lib.exportoffsets[i]:=readu32(p)
		end

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

func readbyte(^byte &p)int=
	return p++^
end
!
func readu32(^byte &p)u64 x=
	x:=^u32(p)^
	p+:=4
	x
end

func readstring(^byte &p)ichar s=
	s:=pcm_copyheapstring(p)

	while (++p)^ do end
	++p

	return s
end

global proc alloclibdata(^librec lib)=
	int tablesize, n
	^byte p

	lib.zdataptr:=pcm_allocz(lib.zdatasize)
!CPL "RUN/MX: NO ALLOCZ"
!	lib.zdataptr:=pcm_alloc(lib.zdatasize)

	tablesize:=lib.nimports*16			!add in thunk table+address table
	n:=lib.codesize

	p:=os_allocexecmem(n+tablesize)		!MUST BE EXECUTABLE MEMORY
	if p=nil then
		error("Can't alloc code memory")
	end
	memcpy(p, lib.codeptr, n)

	memset(p+n, 0, tablesize)
!	memset(p+n, 0xAA, tablesize)

	lib.codeptr:=p
	lib.codexsize:=tablesize

	lib.exportaddr:=pcm_alloc(u64.bytes*lib.nexports)
	lib.importxreftable:=pcm_alloc(i16.bytes*lib.nimports)

!	if lib.entryoffset<>0xFFFF'FFFF then
	if lib.entryoffset<>-1 then
		lib.entryaddr:=lib.codeptr+lib.entryoffset
	end
end

global proc error(ichar mess, param="")=
	if param^ then
		fprintln mess,param
	else
		println mess
	end
	println "Aborting"
	stop 1
end

global proc loadmemmcu(^librec lib)=
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
	end
end

global func findlib(ichar name)int n=
!find an existing library existing

	for i to nlibs do
		if eqstring(name,libnametable[i]) then return i end
	end
	return 0
end

global func mxaddlib(ichar name)int n=
!add a new lib slot with given name
	if nlibs>=maxlibs then 
		error("Too many libs")
	end

	libnametable[++nlibs]:=name
	return nlibs
end

!export proc fixuplib(^librec lib)=
global proc fixuplib(^librec lib)=
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
		end
		dllinsttable[i]:=inst
    end
end

func finddllsymbol(ichar name, int &dllindex)^void p=
!look up symbol in any of the DLLs
!return address, or void if not found
!dllindex is set to dll where it was found

	dllindex:=0
	for i to ndlllibs do
		p:=os_getdllprocaddr(dllinsttable[i], name)
		if p then
			dllindex:=i
			return p
		end
	end

	return nil
end

proc checksymbols=
	int dllindex,undef:=0
	^void p

	for i to nsymbols when not symboldefined[i] do
		p:=finddllsymbol(symbolnametable[i], dllindex)
		if p then
			symboladdress[i]:=p
			symboldllindex[i]:=dllindex
			symboldefined[i]:=1
		else
			println "Undef",symbolnametable[i]
			++undef
		end
	end

	if undef then
		error("Symbols Undefined")
	end
end

proc dorelocations=
	for i to nlibs when not librelocated[i] do
		reloclib(libtable[i])
	end
end

proc reloclib(^librec lib)=
	int index, targetoffset
	ichar name
	^byte p
	^byte q
	^u64 qaddr		!to import address table
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
		(^u32(p)^:=cast(qaddr))
		p+:=4

		index:=lib.importxreftable[i]
		qaddr++^:=cast(symboladdress[index])

	end

!Now do the actual relocations
	for i to lib.nrelocs do
		r:=lib.reloctable[i]
		case r.segment
		when code_rseg then p:=lib.codeptr+r.offset
		when idata_rseg then p:=lib.idataptr+r.offset
		when zdata_rseg then p:=lib.zdataptr+r.offset
		end case

		case r.reloctype
		when locabs32_rel then
			targetoffset:=^u32(p)^
			case r.targetsegment
			when code_rseg then
				(^u32(p)^ := cast(lib.codeptr+targetoffset))
			when idata_rseg then
				(^u32(p)^ := cast(lib.idataptr+targetoffset))
			when zdata_rseg then
				(^u32(p)^ := cast(lib.zdataptr+targetoffset))
			end case

		when locabs64_rel then
			targetoffset:=^u32(p)^
			case r.targetsegment
			when code_rseg then
				(^u64(p)^ := cast(lib.codeptr+targetoffset))
			when idata_rseg then
				(^u64(p)^ := cast(lib.idataptr+targetoffset))
			when zdata_rseg then
				(^u64(p)^ := cast(lib.zdataptr+targetoffset))
			end case

		when impabs64_rel then

			index:=lib.importxreftable[r.stindex]			!global index
			(^u64(p)^+:=cast(symboladdress[index],u64))

		when impabs32_rel then
			index:=lib.importxreftable[r.stindex]			!global index
			(^u32(p)^+:=cast(symboladdress[index],u64))

		when imprel32_rel then
			if r.segment<>code_rseg then error("imprel32?") end
			index:=r.stindex								!local index
			q:=lib.codeptr+lib.codesize+(index-1)*8

			(^u32(p)^ := q-(p+4))	!offset to thunk entry
		end case

	end

	librelocated[lib.libno]:=1

end

global proc loadimports(^librec plib)=
! load imported libs
! do first fixup pass which sets up tables adds imports/exports to global table
! This is done per libs and can be called on imported sub-libs

	^librec qlib
	ichar name

	for i to plib.nlibs do
		dosublib(plib.libnames[i])
	end

	alloclibdata(plib)
	dosymbols(plib)
end

proc dosublib(ichar name)=
	^librec qlib
	int n:=findlib(name)

	if not n then									!not already loaded
		n:=mxaddlib(name)
		println "Loading sublib", name
		qlib:=loadlibfile(addext(name,"ml"),n)		!get mcu
		loadimports(qlib)						!recursive call
	end
end

global func loadlibfile(ichar filename, int libno)^librec plib=
!read mcb file into memory, process it into a new librec
	^byte p

	p:=readmxfile(filename)
	if p=nil then
		error("Can't find #",filename)
	end

	plib:=readlibfile(filename,p)
	plib.libno:=libno
	libtable[libno]:=plib	
end

proc dosymbols(^librec lib)=
!Add any dll libs to global table (libs already done)
!Then deal with imported and exported symbols

	int ix, libx, dllx
	^byte baseaddr

	for i to lib.ndlllibs do
		adddll(lib.dllnames[i])
	end

	for i to lib.nimports do
		ix:=addsymbol(lib.importnames[i])
		lib.importxreftable[i]:=ix
	end

	for i to lib.nexports do
		ix:=addsymbol(lib.exports[i])
		if symboldefined[ix] then
			CPL "Dupl symbol:",lib.exports[i]
			NEXTLOOP
		end
		symboldefined[ix]:=1

		case lib.exportsegs[i]
		when code_rseg then baseaddr:=lib.codeptr
		when idata_rseg then baseaddr:=lib.idataptr
		when zdata_rseg then baseaddr:=lib.zdataptr
		else baseaddr:=nil
		end case

		symboladdress[ix]:=cast(baseaddr+lib.exportoffsets[i])
		symbollibindex[ix]:=lib.libno

	end
end

func readmxfile(ichar filename)^byte p=
!read in mx/ml file into an mcb block, add end_dir byte at the end just in case
!return pointer to mcb block

	p:=readfile(filename)
	return nil when p=nil
	(p+rfsize)^:=end_dir		!add eof-marker

	return p
end

proc adddll(ichar name)=
	for i to ndlllibs do
		if eqstring(name,dllnametable[i]) then return end
	end

	if ndlllibs>=maxdlls then 
		error("Too many DLLs")
	end

	dllnametable[++ndlllibs]:=name
end

func addsymbol(ichar name)int=
	for i to nsymbols do
		if eqstring(name,symbolnametable[i]) then return i end
	end

	if nsymbols>=maxsymbols then 
		error("Too many Imports")
	end

	symbolnametable[++nsymbols]:=name
	return nsymbols
end

proc setspecialglobals(int cmdskip)=
!adjust cmdparams visible to application by setting $cmdskip flag
!	for i to nsymbols when symbolnametable[i]^='$' do
	for i to nsymbols do
		if eqstring(symbolnametable[i],"msys.$cmdskip") or
			eqstring(symbolnametable[i],"$cmdskip") then

			(^byte(symboladdress[i])^:=cmdskip)
!			(^byte(symboladdress[i])^:=0)
		end
	end
end

global proc runprogram(^librec lib, int cmdskip=0)=
	^proc fnptr
	int libno:=lib.libno

	for i to nlibs when i<>libno and not libinitdone[i] do
		calllibinit(libtable[i])
	end

	if lib.entryaddr=nil then
		error("No entry point found")
	end

!CPL "RUNPROG", =CMDSKIP
	setspecialglobals(cmdskip)

	fnptr:=cast(lib.entryaddr)

	fnptr()

	libinitdone[libno]:=1
end

global proc calllibinit(^librec lib)=
	^proc fnptr
	int libno:=lib.libno

	if lib.entryaddr then
		fnptr:=cast(lib.entryaddr)
		fnptr()
	end
	libinitdone[lib.libno]:=1
end

!global func findsymbol(ichar name)^void=
!
!	for i to nsymbols do
!		if eqstring(symbolnametable[i], name) then
!			return symboladdress[i]
!		end
!	end
!	return nil
!end

!global func loadmx(ichar filename)^librec plib=
!!load mx/ml into mcu then scan for other imported libraries
!	int newlib
!	ichar name
!
!	name:=pcm_copyheapstring(convlcstring(extractbasefile(filename)))
!	checknew(name,filename)
!
!	newlib:=mxaddlib(name)
!
!	plib:=loadlibfile(filename,newlib)
!
!	loadimports(plib)
!	return plib
!end

!global func loadmemmcb(ichar filename, ^byte p)^librec plib=
!!read from mcb block in memory
!!'filename' is just an identifying string
!
!	int newlib
!	ichar name
!
!	name:=pcm_copyheapstring(convlcstring(extractbasefile(filename)))
!	checknew(name,filename)
!
!	newlib:=mxaddlib(name)
!	plib:=readlibfile(filename,p)
!	plib.libno:=newlib
!	libtable[newlib]:=plib	
!
!	loadimports(plib)
!	return plib
!end

=== mcr_run.m 0 0 15/74 ===
!Translate SS data directly into MCU block, then try and run that

global func writememlib(ichar filename)^librec plib=
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
	for i to nlibfiles when libfiles[i]^<>'$' do
!		if libtypes[i]='D' then ++ndlls else ++nlibs end
		++ndlls
	end

	lib.ndlllibs:=ndlls
	lib.nlibs:=nlibs

	lib.dllnames:=pcm_alloc(ichar.bytes*ndlls)
	lib.libnames:=pcm_alloc(ichar.bytes*nlibs)

	k:=0
!	for i to nlibfiles when libfiles[i]^<>'$' and libtypes[i]='D' do
	for i to nlibfiles when libfiles[i]^<>'$' do
		lib.dllnames[++k]:=libfiles[i]
	end

!	k:=0
!	for i to nlibfiles when libfiles[i]^<>'$' and libtypes[i]='L' do
!		lib.libnames[++k]:=libfiles[i]
!	end

	addsymbols(&lib)
	plib:=pcm_allocnfz(librec.bytes)
	memcpy(plib, &lib, librec.bytes)	

	return plib
end

proc roundsegment(^dbuffer p, int align, value)=
	int length:=bufferlength(p)
	int newlength:=roundtoblock(length, align)

	buffercheck(p, align)

	to newlength-length do
		p.pcurr++^:=value
	end
end

proc writerelocs(^librec lib)=
	^relocrec oldr
	mcxreloc newr
	int n, k
	psymbol d
	^u64 baseptr64
	^u32 baseptr32@baseptr64

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
				if d.isimport then
					newr.stindex:=d.importindex
					newr.reloctype:=imprel32_rel
				else
					axerror("rel32/rel not imported")
				end
			when addr32_rel, addr64_rel then
				if d.isimport then
					newr.reloctype:=(oldr.reloctype=addr32_rel|impabs32_rel|impabs64_rel)
					newr.stindex:=d.importindex
				else
					if oldr.reloctype=addr32_rel then
						newr.reloctype:=locabs32_rel
					else
						newr.reloctype:=locabs64_rel
					end
					newr.targetsegment:=d.segment
				end
			else
				axerror("reloc?")
			end case

			lib.reloctable[++k]:=newr

		end
	end
end

proc addsymbols(^librec lib)=
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
	for i to ss_nsymbols when ss_symboltable[i].importindex do
		d:=ss_symboltable[i]
!CPL "ADDSYM", D.NAME
		lib.importnames[++k]:=d.name
	end

	k:=0
	for i to ss_nsymbols do
		d:=ss_symboltable[i]
!CPL =D.NAME, =D.EXPORTINDEX, =D.ISENTRY, SCOPENAMES[D.SCOPE]
		if d.exportindex then
			if d.isentry then
				stentry:=d
			end
			lib.exports[++k]:=d.name
			lib.exportsegs[k]:=d.segment
			lib.exportoffsets[k]:=d.offset
		end
	end

!CPL =STENTRY

	if stentry then
		lib.entryoffset:=stentry.offset
	else
		lib.entryoffset:=-1
CPL "NO MAIN FOUND", LIB.ENTRYOFFSET, LIB.ENTRYADDR
	end
end

global proc countsymbols=
	psymbol d
	for i:=1 to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.isexport then d.exportindex:=++nsymexports end
		if d.isimport then d.importindex:=++nsymimports end
	end
end

global proc runlibfile(ichar filename, int cmdskip)=
!LOADERROR("RUNLIBFILE")

	^librec plib

!CPL "RUN LIB", =CMDSKIP

	plib:=writememlib(filename)

	loadmemmcu(plib)
	fixuplib(plib)

!	if fshowmx then
!		LOADERROR("SHOWMX missing")
!!		initlogfile()
!!		showlibs()
!!		closelogfile()
!	else
		runprogram(plib, cmdskip)
!	end
end

=== mcr_write.m 0 0 16/74 ===
!Translate SS data directly into MCB block, then write as mx/ml file

^dbuffer dest

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
	for i to nlibfiles when libfiles[i]^<>'$' do
		++ndlls
	end

	genbyte(dlls_dir)
	genu32(ndlls)
!	for i to nplibfiles when libfiles[i]^<>'$' and libtypes[i]='D' do
	for i to nlibfiles when libfiles[i]^<>'$' do
		genstring(libfiles[i])
	end

	writesymbols()

	genbyte(end_dir)

	writefile(filename, dest.pstart, dest.pcurr-dest.pstart)
end

global proc writerelocs=
	^relocrec oldr
	mcxreloc newr
	int n,count
	psymbol d
	^u64 baseptr64
	^u32 baseptr32@baseptr64

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
				if d.isimport then
					newr.stindex:=d.importindex
					newr.reloctype:=imprel32_rel
				else
					axerror("rel32/rel not imported")
				end
			when addr32_rel, addr64_rel then
				if d.isimport then
					newr.reloctype:=(oldr.reloctype=addr32_rel|impabs32_rel|impabs64_rel)
					newr.stindex:=d.importindex
				else
					if oldr.reloctype=addr32_rel then
						newr.reloctype:=locabs32_rel
					else
						newr.reloctype:=locabs64_rel
					end
					newr.targetsegment:=d.segment
				end
			else
				axerror("reloc?")
			end case

			genblock(&newr, newr.bytes)

		end
	end
end

proc writesymbols=
	psymbol d
	int n
	ichar name

	genbyte(importsymbols_dir)
	genu32(nsymimports)

	for i to ss_nsymbols when ss_symboltable[i].importindex do
		d:=ss_symboltable[i]
		genstring(d.name)
	end

	genbyte(exportsymbols_dir)
	genu32(nsymexports)

	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.exportindex then
!			if eqstring(d.name, "main") then
			if d.isentry then
				entrypoint:=d
			end
			genstring(d.name)
		end
	end

	genbyte(exportsegs_dir)
	genu32(nsymexports)
	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.exportindex then
			genbyte(d.segment)
		end
	end

	genbyte(exportoffsets_dir)
	genu32(nsymexports)
	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.exportindex then
			genu32(d.offset)
		end
	end

	genbyte(entry_dir)		!must be present; writes 0xFFFFFFFF when no entry point
	if entrypoint then
		genu32(entrypoint.offset)
	else
		genu32(0xFFFF'FFFF)
	end
end

proc roundsegment(^dbuffer p, int align, value)=
	int length:=bufferlength(p)
	int newlength:=roundtoblock(length, align)

	buffercheck(p, align)

	to newlength-length do
		p.pcurr++^:=value
	end
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

proc genblock(^void p, int length)=
	buffercheck(dest,length)
	memcpy(dest.pcurr, p, length)
	dest.pcurr+:=length
end
=== mcx_asm.m 0 0 17/74 ===
!const fshowseq=1
const fshowseq=0

!const showsizes=1
const showsizes=0

!const showfreed=1
const showfreed=0

[r0..xr15]psymbol regvars		!nil, or strec when it uses that reg

global int assemtype='AA'

strbuffer sbuffer
global ^strbuffer pdest=&sbuffer

global func writeasm:^strbuffer=
!write all mcl code in system by scanning all procs
!mcl code is only stored per-proc
	psymbol d, e
	^mclrec m
	[32]char str2, str3
	int i

	gs_init(pdest)
!
	asmstr(";   mm8\n")

	case highmem
	when 1 then asmstr("    $userip\n")
	when 2 then asmstr("    $highmem\n")
	end case

	m:=mccode
	i:=1
	while m do
		writemcl(i, m)
		++i
		m:=m.nextmcl
	end
	return pdest
end

proc writemcl(int index, ^mclrec mcl)=
	if mcl.opcode=m_comment and mcl.a.svalue^='?' then
	else
		strmcl(mcl)
!CPL "//"
		gs_line(pdest)
	end
end

global proc strmcl(^mclrec mcl)=
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

!if fshowseq then
!	asmstr(strint(mcl.seqno, "z5"))
!	asmstr(" ")
!end
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
		end
		return
	when m_endx then
		return

	when m_labelname then				!label name will be complete and will have colon(s)
		d:=a.def
		case a.valtype
		when def_val then
			asmstr(getdispname(d))
		when string_val then
			asmstr(a.svalue)
			return
		else
			merror("strmcl/lab")
		end case

		asmstr(":")

		if d.isexport then
			if not fcff then
				asmstr("\n`")
				asmstr(getbasename(d.name))
				asmstr("::")
			else
				asmstr(":")
			fi
		end

!ASMSTR(" ")
!ASMSTR(STRINT(INT(D), "H"))

		return

	when m_label then
		if a.valtype=label_val then
			fprint @str, "L#:", a.value
		else
			recase m_labelname
		end
		asmstr(str)
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
		asmstr(getregname(b.reg, b.size))
		return

	when m_definetemp then
		asmstr("    ")
		asmstr(mstropnd(a))
!		asmstr(gettempname(currfunc, a.tempno))
		asmstr(" = ")
		asmint(b.value)
		return

	when m_asciiz then
		asmstr("    db ")
		asmstr(mstropnd(a))
		asmstr(", 0")
		return

	when m_ascii then
		asmstr("    db ")
		asmstr(mstropnd(a))
		return

	end case

	case opcode
	when m_jmpcc then
		print @opcname, "j", ,asmcondnames[cond]

	when m_setcc then
		print @opcname, "set", ,asmcondnames[cond]

	when m_cmovcc then
		print @opcname, "cmov", ,asmcondnames[cond]

	when m_and then
		strcpy(opcname, "and")
	when m_or then
		strcpy(opcname, "or")
	when m_xor then
		strcpy(opcname, "xor")
	when m_not then
		strcpy(opcname, "not")

	ELSIF OPCODE>M_HALT THEN
		STRCPY(OPCNAME, STRINT(OPCODE))

	else
		strcpy(opcname, mclnames[opcode]+2)
	end case

!	ipadstr(opcname, (opcode=m_dq|4|10), " ")
	ipadstr(opcname, 10, " ")
	ipadstr(str, 4)

	strcat(str, opcname)

	asmstr(str)

	if a and b then		!2 operands
		sizepref:=needsizeprefix(opcode, a, b)
!
		asmopnd(a, sizepref)
		asmstr(", 	")
		asmopnd(b, sizepref)

		if mcl.c then
			asmstr(", ")
			asmstr(strint(mcl.c))
		end

	elsif a and a.mode then								!1 operand
		if opcode=m_call then
			asmopnd(a, 0, opcode)
		else
			asmopnd(a, 1, opcode)
		end
	end

	if showsizes then
		if a then
			asmstr("  ; ")
			asmstr(strint(a.size))
			if b then
				asmstr("/")
				asmstr(strint(b.size))
			end
		end
	end

	if fshowseq then
		asmstr(" ; ")
		asmstr(strint(mcl.seqno, "z5"))
	end

	if showfreed then
		asmstr(" ; Freed:(")
		for r in r0..r13 when mcl.freedset.[r] do
			asmstr(strreg(r))
			asmstr(" ")
		end
		asmstr(") ")
		ASMSTR(STRINT(INT(MCL),"h"))
	end

!IF A AND A.VALTYPE=TEMP_VAL THEN ASMSTR("! AOFFSET:"); ASMINT(A.OFFSET) FI
!IF B AND B.VALTYPE=TEMP_VAL THEN ASMSTR("! BOFFSET:"); ASMINT(B.OFFSET) FI


end

global func strmclstr(^mclrec m)ichar=
	gs_init(pdest)
	strmcl(m)
	return pdest.strptr
end

global func mstropnd(mclopnd a, int sizeprefix=0, opcode=0)ichar=
	static [512]char str
	[128]char str2
	ichar plus, t
	int offset, tc

!RETURN "<OPND>"

	str[1]:=0

	case a.mode
	when a_reg then
		return strreg(a.reg, a.size)

	when a_imm then
!CPL "OPND/IMM", VALTYPENAMES[A.VALTYPE]
		if opcode=m_dq and a.valtype=int_val then
!			if a.value in 0..9 then
				strcat(str, strint(a.value))
!			else
!				strcat(str, "0x")
!				strcat(str, strword(a.value, "H"))
!			end
		else
			strcpy(str, strvalue(a))
		end

	when a_mem then
!CPL "OPND/MEM", VALTYPENAMES[A.VALTYPE]
		strcat(str, getsizeprefix(a.size, sizeprefix))
		strcat(str, "[")

		plus:=""
		if a.reg then
			strcat(str, strreg(a.reg, 8))
			plus:=" + "
		end
		if a.regix then
			strcat(str, plus)
			strcat(str, strreg(a.regix, 8))
			plus:=" + "

			if a.scale>1 then
				strcat(str, "*")
				strcat(str, strint(a.scale))
			end
		end

		if a.valtype in [def_val, label_val, temp_val] then
			if plus^ then
				strcat(str, plus)
			end
			strcat(str, strvalue(a))
	    elsif offset:=a.offset then
			print @str2, offset:" + "
			strcat(str, str2)
		end
		strcat(str, "]")

	else
		println "BAD OPND", A.MODE, =A
		return "<BAD OPND>"
	end case

	return str
end

global func strvalue(mclopnd a)ichar=
	static [512]char str
	[128]char str2
	psymbol def
	i64 value, offset, length
	ichar ss

	def:=a.def
	value:=a.value

	strcpy(str, "")

	case a.valtype
	when def_val then
		strcat(str, getdispname(def))

	addoffset:
		if offset:=a.offset then
			print @str2, (offset>0|"+"|""), ,offset
			strcat(str, str2)
		end

	when int_val then
		strcat(str, strint(value))

	when real_val then
		if a.size=8 then
			print @str, a.xvalue:"20.20"
		else
			print @str, r32(a.xvalue):"20.20"
		end

!	when realmem_val then
!		strcat(str, "M")
!		strcat(str, strreal(a.xvalue))

	when string_val then
		strcat(str, """")
		strcat(str, a.svalue)
		strcat(str, """")

	when name_val then
		strcat(str, a.svalue)

	when label_val then
		strcat(str, "L")
		strcat(str, strint(a.labelno))
		goto addoffset

	when temp_val then
!CPL "STRVAL/TEMP", A.TEMPNO
		return gettempname(currfunc, a.tempno)

	else
		merror("Stropnd?")
	end case

	return str
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
!	end

	size2:=size
	if size2>16 then
		size2:=17
	end

	case reg
	when rnone then return "-"
	when rframe then rs:="fp"
	when rstack then rs:="sp"
	elsif reg>=xr0 then
		print @str, "XMM", ,reg-xr0
		return str

	else
		getstrint(reg-r0, str2)
		rs:=str2
	end case

	print @str, prefix[size2], ,rs
	return str
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
		fprint @str, "##R.#", (fshortnames|""|"`"), (pstdfloat[ttbasetype[d.mode]]|"X"|""),
			 (fshortnames|d.name|getfullname(d))
		return str
	end

	if fshortnames then
		return getbasename(d.name)
	else
		return getfullname(d, backtick:1)
	end
end 

global func gettempname(psymbol d, int n)ichar=
	static [128]char str

	if fshortnames or d=nil then
		print @str, "T", ,n
	else
		fprint @str, "#.$T#", getdispname(d), n
	end
	str
end

global func strreg(int reg, size=8)ichar=
	psymbol d

	d:=regvars[reg]

	if d and stdsize[ttbasetype[d.mode]]=size then
		return getdispname(d)
	end
	getregname(reg, size)
end

global func needsizeprefix(int opcode, mclopnd a, b)int=
	case opcode
	when m_movsx, m_movzx, m_cvtsi2ss, m_cvtsi2sd then
		return 1

	when m_cvtss2si, m_cvtsd2si, m_cvttss2si, m_cvttsd2si then
		return 1
	when m_shl, m_shr, m_sar then
		if a.mode=a_mem then return 1 end
		return 0
	end case

	if a.mode=a_reg or b.mode=a_reg then
		return 0
	end
	return 1
end

global func getsizeprefix(int size, enable=0)ichar=
	static []ichar table=("byte ", "u16 ", "", "u32 ", "","","", "u64 ")
	if not enable or size not in 1..8 then
		""
	else
		table[size]
	end
end

=== mcx_decls.m 0 0 18/74 ===
 

global type mclopnd = ^mclopndrec

!global record mclopndrec =
global record mclopndrec = $caligned
!	^pstrec labeldef	!nil, or handle of strec for label
	union
		psymbol def
		i64 value		!immediate value
		r64 xvalue		!immediate or memory real (depends on .mode)
		ichar svalue	!immediate string
		int labelno
		int sysfn
		struct
			i32 tempno
			byte lasttemp		!set to 1 if .islast applied to the temp
		end
	end

	u32 size			! usually one of 1/2/4/8, but can also be string/data length
	i32 offset			! additional offset to memory operands
	byte scale			! should be 1/2/4/8
	byte mode			!a_reg, a_imm, a_mem etc
	byte valtype		!int_val etc

	byte reg			!0, or main register
	byte regix			!0, or index register
end

!value types associated with a_imm or a_mem modes (not all combos valid):
global enumdata [0:]ichar valtypenames =
	(no_val=0,		$),		!no operand
	(int_val,		$),		!immediate int
	(real_val,		$),		!immediate real (as data) or mem (code)
	(string_val,	$),		!immediate (data) or label (code)
	(def_val,		$),		!var/proc name as imm or mem
	(label_val,		$),		!label index; imm/mem
	(name_val,		$),		!immediate string must be output as an unquoted name
	(temp_val,		$),		!index of pclopnd temp (later becomes ptr to descriptor?)
	(data_val,		$),		!data string; always immediate
end

global type mcl = ^mclrec

global record mclrec = $caligned
	^mclrec lastmcl, nextmcl
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

	u64 freedset				!1 if a register was freed

!	union
!		[r0..r15]byte regfreed		!1 indicates work-register freed after this instr
!		pair regfreedpr
!	end
end

global enumdata []ichar mclnames, []byte mclnopnds, []byte mclcodes, []byte mclauto =

	(m_procstart,		$,		0,		0,		0),
	(m_procend,			$,		0,		0,		0),
	(m_comment,			$,		0,		0,		0),
!	(m_blank,			$,		0,		0,		0),
!	(m_deleted,			$,		0,		0,		0),
	(m_labelname,		$,		0,		0,		0),
	(m_define,			$,		0,		0,		0),
	(m_definereg,		$,		0,		0,		0),
	(m_definetemp,		$,		0,		0,		0),
	(m_trace,			$,		0,		0,		0),
	(m_endx,			$,		0,		0,		0),

	(m_label,			$,		1,		0,		0),
	(m_nop,				$,		0,		0x90,	0),
!	(m_param,			$,		1,		0,		0),
!	(m_assembly,		$,		1,		0,		0),
!	(m_proc,			$,		1,		0,		0),

	(m_mov,				$,		2,		0,		0),
	(m_push,			$,		1,		0,		0),
	(m_pop,				$,		1,		0,		0),
	(m_lea,				$,		2,		0,		0),
	(m_cmovcc,			$,		2,		0,		0),

	(m_movd,			$,		2,		0,		2),
	(m_movq,			$,		2,		0,		0),

	(m_movsx,			$,		2,		0,		0),
	(m_movzx,			$,		2,		0,		0),
	(m_movsxd,			$,		2,		0,		0),

	(m_call,			$,		1,		0xE8,	0),
	(m_ret,				$,		0,		0xC3,	0),
	(m_leave,			$,		0,		0xC9,	0),
	(m_retn,			$,		1,		0,		0),

	(m_jmp,				$,		1,		0xE9,	0),
	(m_jmpcc,			$,		1,		0,		0),
	(m_xchg,			$,		2,		0,		0),

	(m_adc,				$,		2,		2,		0),
	(m_sbb,				$,		2,		3,		0),
	(m_imul,			$,		1,		5,		0),
	(m_mul,				$,		1,		4,		0),
	(m_imul2,			$,		2,		0,		0),
	(m_imul3,			$,		3,		0,		0),

	(m_idiv,			$,		1,		7,		0),
	(m_div,				$,		1,		6,		0),

	(m_and,				$,		2,		0x04,	0),
	(m_or,				$,		2,		0x01,	0),
	(m_xor,				$,		2,		0x06,	0),
	(m_test,			$,		2,		0,		0),

	(m_cmp,				$,		2,		0x07,	0),

	(m_shl,				$,		2,		0x04,	0),
	(m_sar,				$,		2,		0x07,	0),
	(m_shr,				$,		2,		0x05,	0),
	(m_rol,				$,		2,		0x00,	0),
	(m_ror,				$,		2,		0x01,	0),
	(m_rcl,				$,		2,		0x02,	0),
	(m_rcr,				$,		2,		0x03,	0),

	(m_neg,				$,		1,		3,		0),
	(m_not,				$,		1,		2,		0),

	(m_inc,				$,		1,		0,		0),
	(m_dec,				$,		1,		1,		0),

	(m_cbw,				$,		0,		0,		0),
	(m_cwd,				$,		0,		0,		0),
	(m_cdq,				$,		0,		0,		0),
	(m_cqo,				$,		0,		0,		0),
	(m_setcc,			$,		1,		0,		0),

	(m_bsf,				$,		2,		0xBC,	0),
	(m_bsr,				$,		2,		0xBD,	0),

	(m_shld,			$,		2,		0xA4,	0),
	(m_shrd,			$,		2,		0xAC,	0),

	(m_sqrtss,			$,		2,		0x51,	2),
	(m_sqrtsd,			$,		2,		0x51,	0),

	(m_add,				$,		2,		0,		1),
	(m_addss,			$,		2,		0x58,	2),
	(m_addsd,			$,		2,		0x58,	0),

	(m_sub,				$,		2,		5,		1),
	(m_subss,			$,		2,		0x5C,	2),
	(m_subsd,			$,		2,		0x5C,	0),

	(m_mulss,			$,		2,		0x59,	2),
	(m_mulsd,			$,		2,		0x59,	0),

	(m_divss,			$,		2,		0x5E,	2),
	(m_divsd,			$,		2,		0x5E,	0),

	(m_comiss,			$,		2,		0,		2),
	(m_comisd,			$,		2,		0x2F,	0),
	(m_ucomisd,			$,		2,		0x2E,	0),

	(m_xorps,			$,		2,		0x57,	2),
	(m_xorpd,			$,		2,		0x57,	0),

	(m_andps,			$,		2,		0x54,	2),
	(m_andpd,			$,		2,		0x54,	0),

	(m_pxor,			$,		2,		0xEF,	0),
	(m_pand,			$,		2,		0xDB,	0),

	(m_cvtss2si,		$,		2,		0,		0),
	(m_cvtsd2si,		$,		2,		0,		0),

	(m_cvttss2si,		$,		2,		0,		0),
	(m_cvttsd2si,		$,		2,		0,		0),

	(m_cvtsi2ss,		$,		2,		0,		0),
	(m_cvtsi2sd,		$,		2,		0,		0),

	(m_cvtsd2ss,		$,		2,		0,		0),
	(m_cvtss2sd,		$,		2,		0,		0),

	(m_movdqa,			$,		2,		0x66,	0),
	(m_movdqu,			$,		2,		0xF3,	0),

	(m_pcmpistri,		$,		3,		0x63,	0),
	(m_pcmpistrm,		$,		3,		0x62,	0),

	(m_fld,				$,		1,		0,		0),
	(m_fst,				$,		1,		2,		0),
	(m_fstp,			$,		1,		3,		0),

	(m_fild,			$,		1,		0,		0),
	(m_fist,			$,		1,		2,		0),
	(m_fistp,			$,		1,		3,		0),

	(m_fadd,			$,		0,		0xC1,	0),
	(m_fsub,			$,		0,		0xE9,	0),
	(m_fmul,			$,		0,		0xC9,	0),
	(m_fdiv,			$,		0,		0xF9,	0),
	(m_fsqrt,			$,		0,		0xFA,	0),
	(m_fsin,			$,		0,		0xFE,	0),
	(m_fcos,			$,		0,		0xFF,	0),
	(m_fsincos,			$,		0,		0xFB,	0),
	(m_fptan,			$,		0,		0xF2,	0),
	(m_fpatan,			$,		0,		0xF3,	0),
	(m_fabs,			$,		0,		0xE1,	0),
	(m_fchs,			$,		0,		0xE0,	0),

	(m_minss,			$,		2,		0x5D,	2),
	(m_minsd,			$,		2,		0x5D,	0),
	(m_maxss,			$,		2,		0x5F,	2),
	(m_maxsd,			$,		2,		0x5F,	0),

	(m_db,				$,		1,		0,		0),
	(m_dw,				$,		1,		0,		0),
	(m_dd,				$,		1,		0,		0),
	(m_dq,				$,		1,		0,		0),
	(m_ascii,			$,		1,		0,		0),
	(m_asciiz,			$,		1,		0,		0),
!	(m_ddoffset,		$,		1,		0,		0),

!	(m_segment,			$,		1,		0,		0),
	(m_isegment,		$,		0,		0,		0),
	(m_zsegment,		$,		0,		0,		0),
	(m_csegment,		$,		0,		0,		0),

	(m_align,			$,		1,		0,		0),
	(m_resb,			$,		1,		1,		0),
	(m_resw,			$,		1,		2,		0),
	(m_resd,			$,		1,		4,		0),
	(m_resq,			$,		1,		8,		0),

	(m_xlat,			$,		0,		0xD7,	0),
	(m_loopnz,			$,		1,		0xE0,	0),
	(m_loopz,			$,		1,		0xE1,	0),
	(m_loopcx,			$,		1,		0xE2,	0),
	(m_jecxz,			$,		1,		0xE3,	0),
	(m_jrcxz,			$,		1,		0xE3,	0),

	(m_cmpsb,			$,		0,		0,		0),
	(m_cmpsw,			$,		0,		0,		0),
	(m_cmpsd,			$,		0,		0,		0),
	(m_cmpsq,			$,		0,		0,		0),

	(m_rdtsc,			$,		0,		0x31,	0),
	(m_popcnt,			$,		2,		0,		0),
	(m_bswap,			$,		1,		0,		0),

	(m_finit,			$,		0,		0,		0),

	(m_fldz,			$,		0,		0xEE,	0),
	(m_fld1,			$,		0,		0xE8,	0),
	(m_fldpi,			$,		0,		0xEB,	0),
	(m_fld2t,			$,		0,		0xE9,	0),
	(m_fld2e,			$,		0,		0xEA,	0),
	(m_fldlg2,			$,		0,		0xEC,	0),
	(m_fldln2,			$,		0,		0xED,	0),

	(m_cpuid,			$,		0,		0,		0),

	(m_halt,			$,		0,		0xF4,	0),
end

global enumdata [0:]ichar regnames, [0:]byte regcodes =
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

global const rframe = r14
global const rstack = r15

global enumdata [0:]ichar condnames, [0:]ichar asmcondnames,
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

global enumdata [0:]ichar segmentnames =
	(no_seg=0,		$),
	(code_seg,		$),
	(idata_seg,		$),
	(zdata_seg,		$),
	(rodata_seg,	$),
	(impdata_seg,	$),
end

global enumdata [0:]ichar reftypenames =
	(extern_ref=0,		$),		!is external
	(fwd_ref,			$),		!not yet reached
	(back_ref,			$),		!has been reached
end

global enumdata [0:]ichar opndnames_ma =
	(a_none=0,	$),
	(a_reg,		$),		! Ri
	(a_imm,		$),		! d including def name, label etc
	(a_mem,		$),		! any memory modes: [d], [R], [R*4+R2+d+imm] etc
end

global int lababs32, lababs64
global int labneg32, labneg64
global int labmask63, laboffset64
global int labzero
global int kk0used=0

global ^mclrec mccode, mccodex		!genmc adds to this linked list

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
	^constrec nextconst
	int labelno
	int slength
end

global ^constrec cstringlist
!global ^constrec vstringlist
global ^constrec creallist
global ^constrec cr32list

global int lab_funcnametable
global int lab_funcaddrtable
!global int lab_funcnprocs

global record relocrec =			!informal version
	^relocrec nextreloc
	int reloctype
	int offset
	int stindex
end

!global record fwdrec =
!	^fwdrec nextfwd
!	i32 offset
!	i16 reltype
!	i16 seg
!end


!record used for expanding buffers. Expansion is not automatic: buffercheck(n)
!is needed at strategic points to ensure that are at least n bytes left
global record dbuffer =
	^byte pstart
	union
		^byte pcurr
		^u16 pcurr16
		^u32 pcurr32
		^u64 pcurr64
	end
	^byte pend
	int alloc
end

global int ss_zdatalen
global ^dbuffer ss_zdata			!used for error checking only (should be empty at end)
global ^dbuffer ss_idata
global ^dbuffer ss_code

global ^relocrec ss_idatarelocs
global ^relocrec ss_coderelocs

global int ss_nidatarelocs
global int ss_ncoderelocs

global const init_ss_symbols=32768				!globaled to coff
global ^[]psymbol ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize

global ^[]psymbol labeldeftable

global int aaseqno
global int aapos

global [1..8]byte regmodes=(tu8, tu16, 0, tu32, 0,0,0, tu64)

global ^mclrec mclprocentry
global ^mclrec mce_oldmccodex, mce_lastmcl, mce_nextmcl		!used by reset/setmclentry
!global ^mclrec mcf_oldmccodex, mcf_lastmcl, mcf_nextmcl		!used by reset/setmclentry for frame setup

!global byte fpcheckunusedlocals

global record riprec =
	^riprec next
	u32 offset			!within code segment, offset of d32 field
	i32 immsize			!0,1,4 bytes of trailing imm field
end

global ^riprec riplist
global byte mcldone
global byte ssdone
global byte exedone

!global const ctarget=0


global const mclpresent=1
=== mcx_exe.m 0 0 19/74 ===
!Create .exe file from SS-data (code, data, reloc and psymbol tables)

[maxlibfile]i64 libinsttable
[maxlibfile]ichar libinstnames
[maxlibfile]int libnotable			!index into dlltable

global const zsect=3
global const dsect=2
global const csect=1
global const isect=4

record basereloc =
	^basereloc nextitem
	u32 address				!virtual address
	i32 reloctype
end

^basereloc basereloclist
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
!ref[]i64 thunktable				!point into code segment
int fileiatoffset
int fileiatsize
psymbol stentrypoint				!psymbol to be the entry point
psymbol stentrypoint2
!psymbol stentrypoint3

const maxsection = 10
global [maxsection]sectionrec sectiontable
global int nsections

!^byte importdir				!allowed section data for import directort in .idata

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

^byte datastart
^byte dataptr
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
	^byte codeaddr				!mem address of start of code seg
	^u32 offsetptr

!CPL =NLIBFILES
!FOR I TO NLIBFILES DO
!	CPL I, LIBFILES[I]
!OD



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

	if highmem then
		^riprec pr

		pr:=riplist
		while pr, pr:=pr.next do
			offsetptr:=^u32(codeaddr+pr.offset)
			offset:=getripoffset(pr.offset, offsetptr^-imagebase, pr.immsize)
			offsetptr^:=offset	
		end
	end
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
	end
	writepadding(sectiontable[1].rawoffset)
	for i to nsections do
		writesectiondata(&sectiontable[i])
	end

	if fcodesize>=2 then
		println "EXE size:  ", dataptr-datastart:"10s,jr"
		println
	end

	if writefile(outfile,datastart,dataptr-datastart)=0 then
		println "Error writing exe file (possibly still running)"
		stop 1
	end
end

proc loadlibs=
!load library instances
	int i
	i64 hinst
	ichar file
	[300]char filename

	for i to nlibfiles when libfiles[i]^<>'$' do
		strcpy(filename, libfiles[i])
		hinst:=os_getdllinst(filename)
		if hinst=0 then
			cpl "File:",filename
			axerror("Can't load search lib")
		end
		libinsttable[i]:=hinst
		libinstnames[i]:=pcm_copyheapstring(filename)
	end
end

global proc initsectiontable=
!set up the section table

	sectiontable[csect].name:=".text"
	sectiontable[csect].segtype:=code_seg
	sectiontable[csect].data:=ss_code
	sectiontable[csect].virtsize:=bufferlength(ss_code)

!CPL =BUFFERLENGTH(SS_CODE)
!CPL =BUFFERLENGTH(SS_IDATA)
!CPL =BUFFERLENGTH(SS_ZDATA)
!

	if fcodesize in [1,3] then
		println "Code size: ", bufferlength(ss_code):"10s,jr","bytes"
		if fcodesize=3 then
			println "Idata size:", bufferlength(ss_idata):"10s,jr"
			println "Code+Idata:", bufferlength(ss_code)+bufferlength(ss_idata):"10s,jr"
			println "Zdata size:", ss_zdatalen:"10s,jr"
		end
	end

	if bufferlength(ss_idata)=0 then
		addqword (ss_idata,0)
	end

	sectiontable[dsect].name:=".data"
	sectiontable[dsect].segtype:=idata_seg
	sectiontable[dsect].data:=ss_idata

	sectiontable[dsect].virtsize:=bufferlength(ss_idata)
	sectiontable[dsect].rawsize:=roundtoblock(sectiontable[dsect].virtsize,filealign)
	sectiontable[dsect].nrelocs:=ss_nidatarelocs
	sectiontable[dsect].relocs:=ss_idatarelocs

	if ss_zdatalen=0 then
		ss_zdatalen:=16
	end

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
	^char s,name2
	[256]char str
	[256]char str2
	int i

	name2:=nil

	reenter:
	s:=name
	libno:=0

	while s^ do
		if s^='.' then			!assume lib.name
			memcpy(str,name,s-name)
			str[s-name+1]:=0
			strcat(str,".dll")

			for i:=1 to ndlls do
				if eqstring(str,dlltable[i].name) then
					libno:=i
					++dlltable[libno].nprocs
					return (name2|name2|s+1)
				end
			end
			if ndlls>=maxlibs then axerror("Too many libs") end
			libno:=++ndlls

			dlltable[libno].name:=pcm_copyheapstring(str)
			dlltable[libno].nprocs:=1
			return (name2|name2|s+1)
		end

		++s
	end

!do explicit search
	int n

	for i:=1 to nlibfiles when libinsttable[i] do
		if os_getdllprocaddr(libinsttable[i],name) then
			n:=i
			exit				!don't need the actual address; just whether it exists
		end
	else
		CPL NAME
		axerror("Can't find external function")
	end

!found in search lib n
	if libno:=libnotable[n] then			!already added this library
		++dlltable[libno].nprocs
		return name
	end

!first use of this lib
	strcpy(str, libfiles[n])
	strcat(str,".dll")
	if ndlls>=maxlibs then axerror("2:Too many libs") end
	libno:=++ndlls

	dlltable[libno].name:=pcm_copyheapstring(str)
	dlltable[libno].nprocs:=1
	libnotable[n]:=libno

	return name
end

proc scanst=
!scan psymbol table and build dll and imports list
!this version assumes dlls are encoded into the name of each import
!(otherwise, it means requiring a list of dlls and loading/searching for
!the names: doing real linker work.)

	int i,libno
	psymbol d
	ichar name, libname, basename

	for i:=1 to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.isimport then
			if nimports>=maximports then axerror("genexe: Too many imports") end
			++nimports
			name:=d.name
			name:=extractlibname(name,libno,1)
			importtable[nimports].libno:=libno			!0 if no lib
			importtable[nimports].name:=name				!original, or 2nd part of lib.name
			importtable[nimports].def:=d

			d.importindex:=nimports
		elsif d.isexport then
			basename:=getbasename(d.name)
			if userentrypoint then
				if eqstring(basename,userentrypoint) then
					stentrypoint:=d
				end
			else
!				if eqstring(basename,"main") and not isdll then
				if d.isentry and not isdll then
					stentrypoint:=d
				end
			end

			if nexports>=maxexports then axerror("gendll: Too many exports") end
			++nexports

			exporttable[nexports].def:=d
			exporttable[nexports].name:=getbasename(d.name)
		end
	end
end

proc relocdata(^sectionrec s)=
	^sectionrec u
	^relocrec r
	^byte p
	^u32 p32
	^u64 p64
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
			if not d.isimport then
				axerror("rel32/not imported")
			end
			(^u32(p+r.offset)^:=thunkoffset-r.offset-4)
!
		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d.isimport then
				(^u32(p+r.offset)^:=imagebase+thunkoffset+sectiontable[csect].virtoffset)
			else
				u:=nil
				case d.segment
				when zdata_seg then u:=&sectiontable[zsect]
				when idata_seg then u:=&sectiontable[dsect]
				when code_seg then u:=&sectiontable[csect]
				else
					PRINTLN D.NAME,D.SEGMENT
					AXERROR("RELOCDATA/SEG?")

				end case
					p32:=cast(p+r.offset)
					if r.reloctype=addr32_rel then
						p32^:=p32^+u.virtoffset+imagebase
					else
						p64:=cast(P32)
						p64^:=p64^+u.virtoffset+imagebase
					end
			end
		else
			cpl relocnames[r.reloctype]
			axerror("Can't do this rel type")
		end case

		r:=r.nextreloc
	end

end

proc getbaserelocs(^sectionrec s)=
!	^sectionrec u
	^relocrec r
	^byte p
	psymbol d
	int index

	p:=bufferelemptr(s.data,0)
	r:=s.relocs

	while r do
		d:=ss_symboltable[r.stindex]

		case r.reloctype
		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d.isimport then
			else

IF R.RELOCTYPE=ADDR32_REL THEN
!PRINTLN "??BASE RELOC",(D.SEGMENT=CODE_SEG|"CODE"|"DATA"),(R.RELOCTYPE=ADDR32_REL|"ADDR32"|"ADDR64")
ELSE
				newbasereloc(s.virtoffset+r.offset, r.reloctype)
FI

			end
		end case

		r:=r.nextreloc
	end

end

proc writerecordx(^void r, int length)=
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
	end

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
	end

	if stentrypoint=nil then
		if userentrypoint then
			cpl userentrypoint
			axerror("User entry point not found")
		else
			if not isdll then
				axerror("Entry point not found: main")
			end
		end
	else
		header.entrypoint:=sectiontable[csect].virtoffset+stentrypoint.offset
	end

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
	end

	header.iat.virtualaddr:=fileiatoffset
	header.iat.size:=fileiatsize

	writerecordx(&header,header.bytes)

end

proc writesectionheader(^sectionrec s)=
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
	end case
	writerecordx(&sheader,sheader.bytes)
end

proc writesectiondata(^sectionrec s)=
	case s.segtype
	when impdata_seg then
		writerecordx(s.bytedata,s.virtsize)		!rest of section will be zeros
		if s.rawsize>s.virtsize then
			dataptr+:=(s.rawsize-s.virtsize)
		end

	when zdata_seg then					!nothing goes to disk
!		dataptr+:=s.rawsize
	else
		writerecordx(bufferelemptr(s.data,0),s.rawsize)
	end case
end

proc writeexporttable(^byte pstart)=
	const maxexports=2000
	[maxexports]int sortindex
	^exportdirrec phdr := cast(pstart)
	^u32 paddrtable
	^u32 pnametable
	^u16 pordtable
	^char pdllname
	^char pnames
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
	end

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
	end
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
	end

	return size
end

proc newbasereloc(int addr, reltype)=
	^basereloc p

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
	^basereloc p

	baseaddr:=0x1000
	nbaseblocks:=0

	repeat
		nextblock:=baseaddr+0x1000
		if nbaseblocks>=maxbaseblock then axerror("Too many blocks") end
		++nbaseblocks
		blockbases[nbaseblocks]:=baseaddr
		blockcounts[nbaseblocks]:=0


		p:=basereloclist
		while p do
			addr:=p.address
			if addr>=baseaddr and addr<nextblock then
!				println "	",addr:"h",addr-baseaddr:"h", relocnames[p.reloctype]
				++blockcounts[nbaseblocks]
			end

			p:=p.nextitem
		end

		baseaddr:=nextblock
	until baseaddr>maxrelocaddr

	for i to nbaseblocks when blockcounts[i] do
!	for i to nbaseblocks do
		if blockcounts[i].odd then
			++blockcounts[i]
			++blockpadding[i]
		end
		blockbytes[i]:=blockcounts[i]*2+8
		basetablesize+:=blockbytes[i]
	end
end

proc writebasereloctable(^byte pstart)=
	
	^u32 p32
	^u16 p16
	int baseaddr,addr,nextblock
	^basereloc q

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
			end
!
			q:=q.nextitem
		end
		if blockpadding[i] then p16++^:=0 end

		p32:=cast(p16)

	end
end

proc sortexports([]int &sortindex)=
!Sort exporttable by name. This is done by building a set of sorted indices into
!sortindex
	psymbol d,e

!First, store 1..nexports into sortindex
	for i to nexports do
		sortindex[i]:=i
	end

!do bubble sort for now
	repeat
		int swapped:=0
		for i:=1 to nexports-1 do

			d:=exporttable[sortindex[i]].def
			e:=exporttable[sortindex[i+1]].def

			if strcmp(getbasename(d.name), getbasename(e.name))>0 then
				swapped:=1
				swap(sortindex[i], sortindex[i+1])
			end
		end
	until not swapped

end

func getsectionno(int segment)int=
	case segment
	when zdata_seg then zsect
	when idata_seg then dsect
	when code_seg then csect
	else axerror("GSN"); 0
	end case
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
	^byte pcode
	codesize:=sectiontable[csect].virtsize
	pcode:=bufferelemptr(ss_code,codesize)
	while codesize iand 7 do pcode++^:=0x90; ++codesize end
	thunkoffset:=codesize
	codesize+:=nimports*8

	sectiontable[csect].virtsize:=codesize
	sectiontable[csect].rawsize:=roundtoblock(codesize,filealign)

!have to actually add the extra memory now.
	buffercheck(ss_code, codesize-thunkoffset+16)		!just ensure it's there for now

	for i:=1 to nsections do
		if sectiontable[i].segtype<>zdata_seg then
			sectiontable[i].rawoffset:=fileoffset
		end
		if sectiontable[i].segtype<>zdata_seg then
			fileoffset:=roundtoblock(fileoffset+sectiontable[i].virtsize,filealign)
		end
		sectiontable[i].virtoffset:=imageoffset

		if sectiontable[i].segtype=impdata_seg then
			diroffset:=imageoffset
			impdirno:=i
		end

		imageoffset:=roundtoblock(imageoffset+sectiontable[i].virtsize,sectionalign)
	end

	if isdll then
		getbaserelocs(&sectiontable[csect])
		getbaserelocs(&sectiontable[dsect])
	end

!Work out offsets within import directory
!assume dll/imports have been set up
!diroffset starts off as virtual offset of start of impdata section

	diroffset+:=(ndlls+1)*importdirrec.bytes			!need blank entry as terminator

!diroffset now points to import name table
!usual arrangements is for all import name table, followed by all import addr tables

	for i to ndlls do
		dlltable[i].nametableoffset:=diroffset				!data will be filled in later
		diroffset+:=(dlltable[i].nprocs+1)*8
	end
	fileiatoffset:=diroffset
	for i to ndlls do
		dlltable[i].addrtableoffset:=diroffset				!data will be filled in later
		diroffset+:=(dlltable[i].nprocs+1)*8
	end
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
	end

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
	end

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
	end

	offset:=diroffset-dirstartoffset

!offset contains now the overall size of the import directory
!diroffset contains is the overall size of the image

!finish off last section data, and compute final file and image sizes
	sectiontable[impdirno].virtsize:=offset
	sectiontable[impdirno].rawsize:=roundtoblock(offset,filealign)
	filesize:=roundtoblock(fileoffset+offset,filealign)

	imagesize:=roundtoblock(imageoffset+(diroffset-dirstartoffset),sectionalign)

	^byte pimpdir

	pimpdir:=sectiontable[impdirno].bytedata:=pcm_allocz(offset)

!prepare the thunk area in the code segment
	^importdirrec pdir
	^i64 paddr,pname
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
		end
	end

!Fill in the hint/name table
	^byte phint
	^u32 pextra

	for i to nimports do
		phint:=pimpdir+importtable[i].hintnameoffset-dirstartoffset
		phint+:=2					!leave hint as 0
		strcpy(cast(phint),importtable[i].name)
	end

!same for lib names (no hint here, but re-use phint anyway)
	int xxx
	xxx:=dirstartoffset
	for i to ndlls do
!CPL "DLL", I,NDLLS
		pextra:=cast(pimpdir+dlltable[i].dllextraoffset-dirstartoffset)
		for j to dlltable[i].nprocs do
!CPL "  PROC", J, DLLTABLE[I].NAME, IMPORTDIRREC.BYTES
			pextra^:=xxx
			++pextra
		end
		xxx+:=importdirrec.bytes
		phint:=pimpdir+dlltable[i].dllnameoffset-dirstartoffset
		strcpy(cast(phint),dlltable[i].name)
	end

	if isdll then
		writeexporttable(^byte(pimpdir)+exportdiroffset)
		writebasereloctable(^byte(pimpdir)+blockdiroffset)
	end

!write the thunk table
	^byte thunkptr,codebase
	int thunkaddr
	thunkptr:=bufferelemptr(ss_code,thunkoffset)
	codebase:=bufferelemptr(ss_code,0)

	for i to nimports do
		importtable[i].thunkoffset:=thunkptr-codebase
		if highmem=0 then
			thunkptr++^:=0x48
			thunkptr++^:=0xFF
			thunkptr++^:=0x24
			thunkptr++^:=0x25
			thunkaddr:=imagebase+importtable[i].iatoffset
			(^i32(thunkptr)^:=thunkaddr)
			thunkptr+:=4
		else					!use rip mode

			thunkptr++^:=0x48
			thunkptr++^:=0xFF
			thunkptr++^:=0x25
			thunkaddr:=imagebase+importtable[i].iatoffset
			(^i32(thunkptr)^:=getripoffset(int(thunkptr-codebase),thunkaddr-imagebase))
			thunkptr+:=4
			thunkptr++^:=0x90
		end
	end
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

=== mcx_gas.m 0 0 20/74 ===
const fasmformat=2			!gas
!const fasmformat=1			!nasm
!const fasmformat=0			!aa

global int assemtype='GAS'

!export ichar asmext="s"
!export ichar asmext="asm"

strbuffer sbuffer
global ^strbuffer pdest=&sbuffer

[8, r0..r15]ichar nregnames = (
	("al", "r10b", "r11b", "dil", "bl", "sil", "r12b", "r13b", "r14b", "r15b", "cl", "dl", "r8b", "r9b", "bpl", "spl"), 	! 1
	("ax", "r10w", "r11w", "di", "bx", "si", "r12w", "r13w", "r14w", "r15w", "cx", "dx", "r8w", "r9w", "bp", "sp"), 	! 2
	(nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil), 	! 3
	("eax", "r10d", "r11d", "edi", "ebx", "esi", "r12d", "r13d", "r14d", "r15d", "ecx", "edx", "r8d", "r9d", "ebp", "esp"), 	! 4
	(nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil), 	! 5
	(nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil), 	! 6
	(nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil), 	! 7
	("rax", "r10", "r11", "rdi", "rbx", "rsi", "r12", "r13", "r14", "r15", "rcx", "rdx", "r8", "r9", "rbp", "rsp"), 	! 8
)

byte currseg

global func writeasm:^strbuffer=
!write all mcl code in system by scanning all procs
!mcl code is only stored per-proc
	psymbol d,e
	^mclrec m
	[32]char str2,str3

!CPL "WRITE GAS",=PHIGHMEM


	gs_init(pdest)
!	asmstr("# GAS VERSION\n")

	asmstr("    .code64\n")
	asmstr("    .intel_syntax prefix\n")
	asmstr("\n")

!do globals and externs
	d:=psymboltable

	while d, d:=d.next do
		if d.isexport then
			asmstr("    .global ")
			asmstr(getbasename(d.name))
			asmstr("\n")
		end
	end
	asmstr("\n")

	m:=mccode
	while m do
		writemcl(m)
		m:=m.nextmcl
	end

CPL "DONE CODE"
	byte first:=1
	d:=psymboltable

	while d, d:=d.next do
		if d.nameid=procid and d.isexport then
			if first then
				first:=0
				asmstr("    .section .drectve\n")
			end
			asmstr("    .ascii "" -export:\\""")
			asmstr(d.name)
			asmstr("\\""""\n")
		end
	end
	asmstr("\n")

	return pdest
end

global proc strmcl(^mclrec mcl)=
	static [512]char str
	[128]char opcname
	mclopnd a,b
	int opcode,cond,sizepref
	ichar s,comment
	psymbol d

	opcode:=mcl.opcode


	cond:=mcl.cond
	a:=mcl.a
	b:=mcl.b
	comment:=nil

!CPL =MCLNAMES[OPCODE]

	case opcode
	when m_procstart then
		asmstr("# Proc ")
		asmstr(a.def.name)
		currfunc:=a.def

		return

	when m_procend then
		asmstr("# End ")
		currfunc:=nil

		return

	when m_comment then
		asmchar('# ')
		asmstr(a.svalue)
		return

	when m_labelname then				!label name will be complete and will have colon(s)
		d:=a.def
		case a.valtype
		when def_val then
			asmstr(getdispname(d))
		when string_val then
			asmstr(a.svalue)
			return
		else
			merror("strmcl/lab")
		end case

		asmstr(":")

!CPL "LABEL", D.NAME, SCOPENAMES[D.SCOPE]
		if d.isexport then
				asmstr("\n")
				asmstr(getbasename(d.name))
!				asmstr("::")
				asmstr(":")
!			end
		end

		return

	when m_label then
!		fprint @str,"L#:",a.value
!		asmstr(str)
!		return
		if a.valtype=label_val then
			fprint @str,"L#:",a.value
		else
			recase m_labelname
		end
		asmstr(str)
		return

	when m_define then
		asmstr("    .set ")
		d:=a.def
		asmstr(getdispname(d))
		asmstr(", ")
		asmint(d.offset)
		return

	when m_definetemp then
		asmstr("    .set ")
		asmstr(a.svalue)
		asmstr(", ")
		asmopnd(b)
		return

!	when m_definereg then
!		d:=a.def
!		asmstr("   %define ")
!		asmstr(getdispname(d))
!		asmstr(" ")
!
!		case b.mode
!		when a_reg then
!			asmstr(strreg(b.reg, b.size))
!
!		else
!!			asmstr(getxregname(b.reg, b.size))
!			asmstr(strxreg(b.reg, b.size))
!		end case
!		return

		return

	when m_csegment then asmstr("    .text"); currseg:=code_seg; return
	when m_isegment then asmstr("    .data"); currseg:=idata_seg; return
	when m_zsegment then asmstr("    .bss"); currseg:=zdata_seg; return

	end case

	case opcode
	when m_jmpcc then
		print @opcname,"j",,asmcondnames[cond]

	when m_setcc then
		print @opcname,"set",,asmcondnames[cond]

	when m_cmovcc then
		print @opcname,"cmov",,asmcondnames[cond]

	when m_imul2 then
		strcpy(opcname,"imul")

	when m_movzx then
		if a.size=8 and b.size=4 then
			mcl.a:=a:=changeopndsize(a, 4)
			opcode:=m_mov
		end
		recase else

	when m_movsx then
		if a.size=8 and b.size=4 then
			strcpy(opcname, "movsxd")
		else
			recase else
		end

	when m_movd then
		if isxreg(a) and isxreg(b) then		!
			opcode:=m_movq
		end

		recase else

    when m_mov then
        if a.mode=a_reg and b.mode=a_imm and b.valtype=int_val and b.value not in i32.bounds then
            mcl.opcode:=m_movq
        end
		recase else

	when m_align then
		strcpy(opcname, ".align")
!		if currseg=zdata_seg then
!			strcpy(opcname, "alignb")
!		else
!			recase else
!		end
	when m_resb then
		strcpy(opcname, ".space")

	when m_db then
		strcpy(opcname, ".byte")
	when m_dw then
		strcpy(opcname, ".word")
	when m_dd then
		strcpy(opcname, ".long")
	when m_dq then
		strcpy(opcname, ".quad")
	when m_ascii then
		strcpy(opcname, ".ascii")


	when m_endx then
		return

	ELSIF OPCODE>M_HALT THEN
		STRCPY(OPCNAME,STRINT(OPCODE))

	else
		strcpy(opcname,mclnames[opcode]+2)
	end case

	ipadstr(opcname,10," ")

	if not fasmformat then
		if a and b then
			fprint @str,"  #/#",a.size,b.size
		elsif a then
			fprint @str,"  #",a.size
		else
			strcpy(str,"  ")
		end
	else
		strcpy(str,"  ")
	end

	ipadstr(str,4)

	strcat(str,opcname)

	asmstr(str)

	if a and b then		!2 operands
		sizepref:=needsizeprefix(opcode,a,b)
!
		asmopnd(a,sizepref)
		asmstr(",	")
		asmopnd(b,sizepref)

		if mcl.c then
			asmstr(",")
			asmstr(strint(mcl.c))
		end

!		ASMSTR("; ")
!		ASMSTR(strint(a.size))
!		ASMSTR(" ")
!		ASMSTR(strint(b.size))
!		ASMSTR(" #")
!		ASMSTR(STRINT(MCL.SEQNO))
!!
	elsif a and a.mode then								!1 operand
		if opcode=m_call then
			asmopnd(a,0,opcode)
		else
			asmopnd(a,1,opcode)
		end
	end

!ASMSTR("	#"); ASMSTR(STRINT(MCL.SEQNO))

end

global func strmclstr(^mclrec m)ichar=
	gs_init(pdest)
	strmcl(m)
	return pdest.strptr
end

global func mstropnd(mclopnd a,int sizeprefix=0,opcode=0)ichar=
	static [512]char str
	[128]char str2
	ichar plus,t
	int offset,tc

	str[1]:=0

	case a.mode
	when a_reg then
		return strreg(a.reg, a.size)

	when a_imm then
		if opcode=m_dq and a.valtype=int_val then
			if a.value in 0..9 then
				strcat(str,strint(a.value))
			else
				strcat(str,"0x")
				strcat(str,strword(a.value,"H"))
			end
		else
			strcpy(str,strvalue(a))
		end

	when a_mem then
!		case a.valtype
!		when intimm_val then
!			strcpy(str,strint(a.value))
!		when realimm_val then
!			strcpy(str,strreal(a.xvalue))
!		when realmem_val then
!			fprint @str,"M#",a.xvalue
!		end case

		strcat(str,getsizeprefix(a.size,sizeprefix))
		strcat(str,"[")

		plus:=""
		if a.reg then
			strcat(str,strreg(a.reg,8))
			plus:=" + "
		end
		if a.regix then
			strcat(str,plus)
			strcat(str,strreg(a.regix,8))
			plus:=" + "

			if a.scale>1 then
				strcat(str,"*")
				strcat(str,strint(a.scale))
			end
		end

		if a.valtype in [def_val,label_val, temp_val] then
			IF A.REG=A.REGIX=RNONE AND HIGHMEM THEN
				STRCAT(STR, "%rip+")
			end
			if plus^ then
				strcat(str,plus)
			end
			strcat(str,strvalue(a))
	    elsif offset:=a.offset then
			print @str2,offset:" + "
			strcat(str,str2)
		end
		strcat(str,"]")

	else
		println "BAD OPND",A.MODE
		return "<BAD OPND>"
	end case

	return str
end

global func strvalue(mclopnd a)ichar=
	static [512]char str
	[128]char str2
	psymbol def
	i64 value,offset,length
	ichar ss

	def:=a.def
	value:=a.value

	strcpy(str,"")

	case a.valtype
	when def_val then
		strcat(str,getdispname(def))

	addoffset:
		if offset:=a.offset then
			print @str2,(offset>0|"+"|""),,offset
			strcat(str,str2)
		end

	when int_val then
		strcat(str,strint(value))
!		strcat(str,strint(value, "h"))

	when real_val then
		print @str,a.xvalue:"20.20"

!	when realmem_val then
!		strcat(str,"M")
!		strcat(str,strreal(a.xvalue))
!
	when string_val then
		strcat(str,"""")
		strcat(str,a.svalue)
		strcat(str,"""")

	when name_val then
		strcat(str,a.svalue)

	when label_val then
		strcat(str,"L")
		strcat(str,strint(a.labelno))
		goto addoffset

	when temp_val then
		return gettempname(currfunc,a.tempno)

	else
		merror("Stropnd?")
	end case

	return str

end

proc writemcl(^mclrec mcl)=

	case mcl.opcode
!	when m_define, m_definereg then
	when m_definereg then
	else
		strmcl(mcl)
		gs_line(pdest)
	end case
end

global proc asmopnd(mclopnd a,int sizeprefix=0,opcode=0)=
	asmstr(mstropnd(a,sizeprefix,opcode))
end

global func getxregname(int reg,size=8)ichar=
	static [32]char str

	if reg=rnone then return "-" end

!	if fasmformat then
		print @str,"%XMM",,reg-xr0
!	else
!		print @str,(size=8|"DX"|"SX"),,reg-xr0
!	end
	return str
end

proc asmstr(ichar s)=
	gs_str(pdest,s)
end

proc asmchar(int c)=
	gs_char(pdest,c)
end

proc asmint(int a)=
	asmstr(strint(a))
end

global func getdispname(psymbol d)ichar=
	static [256]char str

!	if d.reg then
!		fprint @str,"#.#","R", d.name
!		return str
!	end
!
!	if fpshortnames then
!		return d.name
!	end

	return getfullname(d)
!	strcpy(str, getfullname(d))
!	if d.id=static_id then
!!IF D.IMPORTED THEN CPL "STATIC", D.NAME, D.IMPORTED FI
!		strcat(str,".")
!	end
!	return str
!	return "FULL"

end 

global func gettempname(psymbol d, int n)ichar=
	static [128]char str

!CPL "TEMP:", D.NAME,D.OFFSET

	if fshortnames then
		print @str,"T",,n
	else
		fprint @str,"#.$T#",getdispname(d),n
	end
	str
end

global func strreg(int reg, size=8)ichar=
	static [16]char str

	if reg>=xr0 then
		return strxreg(reg, size)
	end

	strcpy(str, "%")
	strcat(str, nregnames[size, reg])

	str
end

func strxreg(int reg, size=8)ichar=
	psymbol d

!	d:=checkregvar(reg,1)

!	if size=8 and d then
!		return getdispname(d)
!	else
		return getxregname(reg,size)
!	end
end

global func needsizeprefix(int opcode,mclopnd a,b)int=
	case opcode
	when m_movsx, m_movzx, m_cvtsi2ss, m_cvtsi2sd then
		return 1

	when m_cvtss2si,m_cvtsd2si, m_cvttss2si,m_cvttsd2si then
		return 1
	when m_shl, m_shr, m_sar then
		if a.mode=a_mem then return 1 end
		return 0
	end case

	if a.mode=a_reg or b.mode=a_reg then
		return 0
	end
	return 1
end

global func getsizeprefix(int size,enable=0)ichar=
	if not enable then return "" end
	case size
	when 1 then return "byte ptr"
	when 2 then return "word ptr"
	when 4 then return "dword ptr"
	when 8 then return "qword ptr"
	end case
	return ""
end

func checkregvar(int reg, ispfloat)psymbol d=
	RETURN NIL
end

=== mcx_genexe.m 0 0 21/74 ===
!HELLO
=== mcx_genss.m 0 0 22/74 ===
const wmask = 2x1000				!1 means 64-bit operand size
const rmask = 2x0100				!extends mod/rm reg field
const xmask = 2x0010				!extends sib index field
const bmask = 2x0001				!extends mod/rm r/m field, also sib base field

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

int currseg=0
^dbuffer currdata				!copy of ss_idata or ss_code
^relocrec currrelocs
int nrelocs

[r0..r15]byte ishighreg				!high regs have 0x40 (see start)

^MCLREC CURRMCL
^riprec ripentry

macro genbyte(x) = currdata.pcurr++^:=x

macro makemodrm(mode,opc,rm) = mode<<6+opc<<3+rm

global proc genss(int obj=0)=
	int index
	^mclrec m

	return when ssdone


!CPL "GENSS",$LINENO

	sstime:=os_clock()

	initlib(mlabelno)
!CPL "INITLAB 50"
!	initlib(50)

!CPL "GENSS",$LINENO
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

!CPL "SS1", OS_CLOCK()-SSTIME

!CPL "GENSS",$LINENO
	while m do
!CPL "GENSS",$LINENO
		doinstr(m,++index)
!CPL "GENSS",$LINENO
		m:=m.nextmcl
	end
!CPL "DONE GENSS LOOP"
!CPL "SS2", OS_CLOCK()-SSTIME

	switchseg(0)					!update ss_currrelocs etc

	if bufferlength(ss_zdata) then
		axerror("Zdata contains code or data")
	end

	if obj then					!do fixups needed for .obj files
		^riprec pr			!(exe module does its own fixups)
		^byte codeaddr
		^u32 offsetptr

		codeaddr:=bufferelemptr(ss_code, 0)
			pr:=riplist
			while pr, pr:=pr.next do
				offsetptr:=^u32(codeaddr+pr.offset)
				offsetptr^-:=pr.immsize
		end
	end
!CPL "SS3", OS_CLOCK()-SSTIME

	ssdone:=1
	sstime:=os_clock()-sstime

end

proc doinstr(^mclrec m,int index)=
	mclopnd a,b
	psymbol d,e
	int x,offset,shortjmp,n

!UNLESS M.OPCODE=M_COMMENT THEN
!CPL "DOINSTR",MCLNAMES[M.OPCODE], M.SEQNO, (CURRFUNC|CURRFUNC.NAME|"--")
!!STOP WHEN OS_GETCH()=27
!END

	if currdata.pend-currdata.pcurr<1024 then
		bufferexpand(currdata)
	end

	rex:=sizeoverride:=addroverride:=f2override:=f3override:=nowmask:=usesizeb:=0

	a:=m.a
	b:=m.b

	aaseqno:=m.seqno
	aapos:=m.mpos
	ripentry:=nil
	CURRMCL:=M

	switch m.opcode
	when m_procstart then
		currfunc:=M.A.DEF
!CPL "PROC", CURRASMPROC.NAME

	when m_procend then
	when m_define then

	when m_definereg then
	when m_definetemp then

	when m_labelname then

		case a.valtype
		when string_val then
		when def_val then
			d:=a.def
			d.reftype:=back_ref
			d.segment:=currseg
			d.offset:=getcurrdatalen(6)

			if d.isexport then
				getstindex(d)
			end

			dofwdrefs(d)
		end case

	when m_label then

		if a.valtype=def_val then			!named label (probably from assembler)
			d:=a.def
		else
			d:=labeldeftable[a.labelno]
		end
	
		d.reftype:=back_ref
		d.segment:=currseg
		d.offset:=getcurrdatalen(6)

		if d.isexport then
			getstindex(d)
		end

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
			end
		else
			shortjmp:=checkshortjump(m,d)
			if not shortjmp then
				genbyte(0x0F)
				genbyte(0x80+m.cond)
				genrel32(a)
			else
				genbyte(0x70+m.cond)
				genrel8(a)
			end
		end

	when m_db, m_ascii then
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
		if a.mode<>a_imm then axerror("retn?") end
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
				to n do genbyte(0x90) end
			when idata_seg then
				to n do genbyte(0) end
			else
				ss_zdatalen+:=n
			end case
		
		else
			axerror("resb?")
		end

	when m_align then
		if a.mode=a_imm then
			x:=a.value
			if x<1 or x>16384 then axerror("align2") end
			buffercheck(currdata, x)
			if currseg<>zdata_seg then
				while bufferlength(currdata) rem x do genbyte((currseg=code_seg|0x90|0)) end
			else
				while ss_zdatalen rem x do	++ss_zdatalen end
			end
		else
			axerror("align?")
		end

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
		println "*** Can't do opcode",mclnames[m.opcode],"line",aaseqno,=M.OPCODE,=M_HALT
	CPL
	CPL
	AXERROR("STOPPING")
!	end switch
	end

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
	^char s
	i64 x
	int length

	if size=0 then size:=a.size end

	case a.valtype
	when string_val then
		s:=a.svalue
		length:=strlen(s)
		if length>100 then
			buffercheck(currdata,max(1024,length+1))
		end
		while s^ do
			genbyte(s++^)
		end
		return
	WHEN NAME_VAL THEN
		PRINTLN "GENSS/NAME OPND"
	end case

	if getdef(a) and size<=2 then
		axerror("8/16-BIT RELOC")
	end

	case size
	when 1 then
		genbyte(a.value)
	when 2 then
		genword(a.value)
	when 4 then
		case a.valtype
		when int_val then
			gendword(a.value)
		when real_val then
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
		end case

	when 8 then
		case a.valtype
		when int_val then
			genqword(a.value)
		when real_val then
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
		end case

	end case
end

proc addrelocitem(int reloctype, psymbol d)=
	^relocrec r
	int stindex, adjust

!CPL "ADDRECLOCITEM", D.NAME
	stindex:=getstindex(d)

	adjust:=4
	if reloctype=addr64_rel then adjust:=8 end

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
!CPL "GETSTX", D.NAME, =D.EQUIVVAR, =D
	if d.stindex=0 then
		if ss_nsymbols>=ss_symboltablesize then
			extendsymboltable()
		end
		d.stindex:=++ss_nsymbols

		ss_symboltable[d.stindex]:=d

		if d.segment=0 then
!			if d.imported then
			if d.isimport then
				d.segment:=code_seg
			end
		end

	end
	return d.stindex
end

proc genrel32(mclopnd a)=
!used by call/longjmp/ddoffset
	psymbol d

!CPL "GENREL32", MSTROPND(A)
	d:=getdef(a)

	if d=nil then				!constant
		gendword(a.value)
		return
	end

	case d.reftype
	when back_ref then
		if d.segment<>currseg then
			axerror("Rel label across segments")			!might be Ok if treated as external?
		end
		gendword(d.offset-(getcurrdatalen(2)+4)+a.offset)
	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(3),rel32_rel)
		gendword(a.offset)
	else								!external symbol
		gendword(a.offset)		!this is probably just zero
		addrelocitem(rel32_rel,d)
	end case
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
				if not d.isimport then
					d.reftype:=fwd_ref
				end
			end

			return d
		end case
	end
	if dneeded then				!must return a non-nil value
		println opndnames_ma[a.mode],valtypenames[a.valtype]
		axerror("getdef/no def")
	end
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
		if d.nameid in [frameid, paramid] then
			gendword(d.offset+a.offset)
		else
			gendword(a.offset)
			addrelocitem(addr32_rel,d)
		end

	else								!external symbol
		gendword(a.offset)					!this is probably just zero
		addrelocitem(addr32_rel,d)
	end case
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
		if d.nameid in [frameid, paramid] then
			genqword(d.offset+a.offset)
		else
			genqword(a.offset)
			addrelocitem(addr64_rel,d)
		end

	else								!external symbol
		genqword(a.offset)				!this is probably just zero
		addrelocitem(addr64_rel,d)
	end case
end

func getrel32(psymbol d,int offset)int=
!get rel difference between offset in this segment, and label d
	if d.reftype=back_ref then					!defined earlier in this segment
		if d.segment<>currseg then
			axerror("Rel label across segments2")
		end
		return d.offset-(offset+1)
	else
		return i32.max
	end
end

proc dofwdrefs(psymbol d)=
!label d has been encountered
!update any fwd refs
!assume inside same offset, at least for rel-32 which only works in text segment
	^fwdrec f
	int offset, seg
	^byte p8
	^i32 p32
	^i64 p64
	^dbuffer data

	if d.fwdrefs=nil then return end
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
			when zdata_seg then axerror("Fwd ^in zdata")
			when idata_seg then data:=ss_idata
			end case

			p32:=bufferelemptr(data,offset)
			if f.reltype=addr32_rel then
				p32^:=p32^+d.offset
			else
				p64:=cast(p32)
				p64^:=p64^+d.offset
			end
		when rel8_rel then
			p8:=bufferelemptr(currdata,offset)
			p8^:=d.offset-offset-1
		else
			CPL RELOCNAMES[F.RELTYPE],D.NAME
			AXERROR("DOFWDREFS/CAN'T DO RELTYPE")
		end case

		f:=f.nextfwd
	end
end

proc genrex=
	if f2override then genbyte(0xF2) end
	if f3override then genbyte(0xF3) end
	if sizeoverride then genbyte(0x66) end
	if addroverride then genbyte(0x67) end

	if nowmask then rex.[wbit]:=0 end

	if rex then genbyte(rex iand 15+0x40) end
end

func isbytesized(i64 x)int=
	return -128<=x<=127
end

func isdwordsized(i64 x)int=
	return i32.min<=x<=i32.max
end

proc genamode(mclopnd a, amoderec am)=
	psymbol d
	^riprec pr

	genbyte(am.modrm)

	if am.usesib=1 then
		genbyte(am.sib)
	end

!CPL "GENAMODExx", VALTYPENAMES[A.VALTYPE], =A.TEMPNO, =AM.OFFSET,=AM.DISPSIZE
!AMODENMES[A.MODE, =AM.
	case am.dispsize			!disp bytes
	when 0 then
	when 1 then
!CPL "HERE:", =AM.OFFSET
!CPL "GENAMODE/1", VALTYPENAMES[A.VALTYPE], =A.TEMPNO, =AM.OFFSET
		genbyte(am.offset)
	when 4 then
		if am.usesib=-1 then
			pr:=pcm_alloc(riprec.bytes)
			pr.next:=riplist
			pr.offset:=currdata.pcurr-currdata.pstart
			ripentry:=riplist:=pr
		end
		case a.mode
		when a_mem then
			case a.valtype
			when def_val, label_val then
				genabs32(a)
			when no_val, temp_val then
!CPL "GENAMODE", VALTYPENAMES[A.VALTYPE], =A.TEMPNO, =AM.OFFSET
				gendword(am.offset)
			else
				axerror("genam/3")
			end case
		else
			CPL OPNDNAMES_MA[A.MODE]
			axerror("GENAMODE/MODE?")
		end case
	else
		axerror("genamode size 2/8")
	end case
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
	end case
end

func getdispsize(mclopnd a, i32 &offset)int=
!look at imm/mem displacement, and return (0,1 or 4) and offset
!0 is returned when no disp is needed (no labeldef and offset is zero)
!unless mand=1 then 1 is returned
	psymbol d

	d:=getdef(a)
	offset:=a.offset

	if d then
		if d.nameid in [frameid, paramid] then
			offset+:=d.offset
		else
			return 4
		end
	end

	if offset then
		return (isbytesized(offset)|1|4)
	else
		return 0
	end
end

proc checkhighreg(mclopnd a)=
	if a.mode=a_reg then
		rex ior:=ishighreg[a.reg]
	end
end

proc do_loop(mclopnd a,int opc)=
	int offset

	offset:=getrel32(getdef(a,1),getcurrdatalen(9)+1)
	if offset<0 then			!backjump
		if offset<-126 then
			axerror("loop jmp out of range")
		end
		genbyte(opc)
		genbyte(offset)
	else
		axerror("Can't do loopxx fwd jump")
	end
end

proc do_jcxz(mclopnd a,int opsize)=
	int offset

	offset:=getrel32(getdef(a,1),getcurrdatalen(10)+1)
	if offset<0 then			!backjump
		if offset<-126 then
			axerror("jcxz jmp out of range")
		end
		if opsize=4 then genbyte(0x67) end
		genbyte(0xE3)
		genbyte(offset)
	else
		axerror("Can't do jcxz fwd jump")
	end
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
		end case

		genxrm(0xFF, 2, a)

	end case
end

proc do_jmp(mclopnd a,^mclrec m)=
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
			end

			if not shortjmp then
				genbyte(0xE9)
				genrel32(a)
			else
				genbyte(0xEB)
				genrel8(a)
			end
		end
	else				!indirect jump
		case a.size
		when 0 then a.size:=8
		when 1,2,4 then
			axerror("jmp[]size")
		end case

		genxrm(0xFF, 4, a)
	end case

end

func getcurrdatalen(int id)int=
!I think that zdata-seg is only likely when id=6

	if currseg=zdata_seg then
		return ss_zdatalen
	end
	return bufferlength(currdata)
end

proc do_cmovcc(int cond, mclopnd a,b)=
	if a.size<>b.size and b.size then
		axerror("1:Opnd size mismatch")
	end
	if a.size=1 then axerror("cmov/byte") end

	genrrm(0x0F'40+cond, a, b)
end

proc do_fmem(mclopnd a, int freal, code)=
!do fld/fild/fst/fstp/fist,fistp
!freal=1 for fld/etc, 0 for fild etc
!code is middle 3 bits of 2nd byte: 0=load, 2=store, 3=store+pop
	int am, regcode, mf

	if a.mode<>a_mem then
		axerror("fmem/not mem")
	end

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
			end case
		else
			CPL "SIZE=",A.SIZE
			axerror("fmem size")
		end case
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
			end case
		else
			axerror("fmem int size")
		end case
	end
	
	genxrm(0xD9+mf<<1, code, a)
end

proc genrel8(mclopnd a)=
!a is a known fwd reference, and expected to be <=127 bytes
	psymbol d

	d:=getdef(a,1)

	if d.reftype=fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(3),rel8_rel)
		genbyte(0)
	else								!external symbol
		axerror("genrel8")
	end
end

func checkshortjump(^mclrec m,psymbol d)int=
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
				if a.labelno=d.labelno then return 1 end
			when def_val then
				if a.def=d then return 1 end
			end case

		when m_comment, m_endx then
		when m_resb then
			return 0
		else
			++n
		end case
		m:=m.nextmcl
	end

	return 0
end

func addfwdref(^fwdrec p, int offset, reltype, seg=0)^fwdrec=
	^fwdrec q

!	q:=pcm_alloc(fwdrec.bytes)
	q:=pcm_allocnfz(fwdrec.bytes)
	q.nextfwd:=p
	q.offset:=offset
	q.reltype:=reltype
	q.seg:=seg
	return q
end

proc switchseg(int newseg)=
	if newseg=currseg then return end

	case currseg						!reloc linked list roots must be updated
	when code_seg then
		ss_coderelocs:=currrelocs
		ss_ncoderelocs:=nrelocs
	when idata_seg then
		ss_idatarelocs:=currrelocs
		ss_nidatarelocs:=nrelocs
	end case

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
	end case							!else 0, done at end to update linked lists

end

proc do_popcnt(mclopnd a,b)=
	int am, regcode

	if b.mode=a_mem then
		if b.size=0 then b.size:=8 end
	end

	f3override:=1
	genrrm(0x0F'B8, a, b)
end

proc do_bsf(mclopnd a,b, int opc)=
	int am, regcode

	if b.mode=a_mem then
		if b.size=0 then b.size:=8 end
	end
	if a.size<>b.size then axerror("bsf size") end

	genrrm(0x0F<<8+opc, a, b)
end

proc extendsymboltable=
	^[]psymbol oldsymboltable
	int oldsymboltablesize

	oldsymboltablesize:=ss_symboltablesize
	oldsymboltable:=ss_symboltable

	ss_symboltablesize*:=2

	ss_symboltable:=pcm_alloc(^void.bytes*ss_symboltablesize)

	for i:=1 to ss_nsymbols do
		ss_symboltable[i]:=oldsymboltable[i]
	end

	pcm_free(oldsymboltable,^void.bytes*oldsymboltablesize)
end

global proc initlib(int nlabels)=
	[256]char str
	psymbol d

	ss_symboltable:=pcm_alloc(init_ss_symbols*^void.bytes)
	ss_symboltablesize:=init_ss_symbols
	ss_nsymbols:=0
	labeldeftable:=pcm_alloc(nlabels*^void.bytes)

	for i to nlabels do
		d:=labeldeftable[i]:=pcm_allocnfz(strec.bytes)

!CPL "SETTING LAB",I,"TO",D
		d.labelno:=i
		fprint @str,"l#",i
		d.name:=pcm_copyheapstring(str)
		d.reftype:=fwd_ref
	end
!
end

global func buffercreate(int size=1024)^dbuffer=
	^dbuffer a

	a:=pcm_alloc(dbuffer.bytes)

	a.alloc:=size
	a.pstart:=a.pcurr:=pcm_alloc(a.alloc)
	a.pend:=a.pstart+a.alloc
	return a
end

proc bufferexpand(^dbuffer a)=
	int newalloc,usedbytes
	^byte p

	newalloc:=a.alloc*2
	usedbytes:=a.pcurr-a.pstart

	if usedbytes>a.alloc then
		println "dbuffer error"
		stop
	end

	p:=pcm_alloc(newalloc)
	memcpy(p,a.pstart,usedbytes)
	a.pstart:=p
	a.pcurr:=p+usedbytes
	a.alloc:=newalloc
	a.pend:=p+newalloc
end

global proc buffercheck(^dbuffer a,int n=1024)=
	while a.pend-a.pcurr<n do
		bufferexpand(a)
	end
end

global func bufferlength(^dbuffer a)int=
	return a.pcurr-a.pstart
end

global func bufferelemptr(^dbuffer a, int offset)^void=
	return a.pstart+offset
end

global proc addword(^dbuffer a, int x)=
	a.pcurr16^:=x
	++(a.pcurr16)
end

global proc adddword(^dbuffer a, int x)=
	a.pcurr32^:=x
	++(a.pcurr32)
end

global proc addqword(^dbuffer a, i64 x)=
	a.pcurr64^:=x
	++(a.pcurr64)
end

proc genxrm(int opcode, code, mclopnd b)=
!deal with /d instructions, where code = 0..7
	amoderec am

	setopsize(b)

	am:=genrm(0, code, b)

	case currmcl.opcode
	when m_push, m_pop then rex.[wbit]:=0
	end case


!	if opbytes[2] then genbyte(opbytes[2]) fi		!extra opcodes will not be 0
!	if opbytes[1] then genbyte(opbytes[1]) end
	if opcode.[16..23] then genbyte(opcode.[16..23]) end


	genrex()
	if opcode.[8..15] then genbyte(opcode.[8..15]) end

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
	ref byte p
	int length

!	checkhighreg(a)
	if a.mode=a_reg and a.reg<xr0 then rex ior:=ishighreg[a.reg] end

	setopsize(a)

	if usesizeb then				!wmask comes from b
		rex.[wbit]:=0
		if b.size=8 then rex ior:=wmask end
	end

	am:=genrm(a.reg, 0, b)

!CPL =AM.modrm, am.sib, am.usesib, am.dispsize, am.offset

	if opcode.[16..23] then genbyte(opcode.[16..23]) end
	genrex()
	if opcode.[8..15] then genbyte(opcode.[8..15]) end

!cpl =OPCODE:"H"

	genbyte(opcode)
	genamode(b,am)
end

func getregcode(int reg, int mask, isxreg=0)int regcode=
!convert m-register code (1 to 16/20) to hardware code (0..7 plus any rex bits)
!mask is the rex bit to set for high registers
!isxreg is 1 for float registers, where I don't need to do the usual mapping

	if not isxreg then
		regcode:=regcodes[reg]
	else
		regcode:=reg-xr0			!xr1 is 1; xmm0 is 0
	end

	if regcode>=8 then
		regcode-:=8
		rex ior:=mask
	end
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
	end case
end

func genrm(int reg, opc, mclopnd b)amoderec=
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

	if reg then				!else use opc as-is
!		opc:=getregcode(reg, rmask, isxreg)
		opc:=getregcode(reg, rmask, reg>=xr0)
!CPL =OPC
	end

	case b.mode
	when a_reg then			!modrm can only ^to a single register
		rm:=getregcode(b.reg, bmask, b.reg>=xr0)
		rex ior:=ishighreg[b.reg]

!CPL =OPC, =RM, =REX

		am.modrm:=makemodrm(3,opc,rm)
		return am

	when a_mem then
		ismem:=1
		case b.valtype
		when def_val then
			if b.def.nameid=staticid then ismem:=2 end
		when real_val then ismem:=2
		when label_val then ismem:=2
		end case

	else
		axerror("genrm not mem")
	end case

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
		end

		rm:=base:=getregcode(reg, bmask)

		if rm<>4 then
			if rm=5 and am.dispsize=0 then
				mode:=1; am.dispsize:=1
			end
			index:=0
		else
			index:=4				!means no index
			scale:=1				!force sib

		end
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
		if regix=rstack then axerror("Scaled rstack?") end

	else									!assume regix used; optional reg and disp
		am.dispsize:=getdispsize(b, am.offset)
		if am.dispsize then
			mode:=(am.dispsize=1|1|2)
		end
		rm:=4

		scale:=(b.scale|b.scale|1)
!CP "SCAD"
		if reg=0 then
			base:=5
		else
			if reg in [rframe,r7] and am.dispsize=0 then
				mode:=1; am.dispsize:=1
			end
			base:=getregcode(reg, bmask)
		end

		if regix=0 then
			index:=4
		else
			index:=getregcode(regix, xmask)
			if not reg then
				am.dispsize:=4
			end
			if regix=rstack and scale>1 then axerror("Can't scale rstack") end
		end
	end

	if scale then
		am.sib:=scaletable[scale]<<6 + index<<3 + base
		am.usesib:=1
	end

	if am.dispsize=4 and ismem then
		if reg or regix then
			if highmem=2 AND ISMEM=2 then
				CPL "Addr32 can't use RIP, line",aaseqno,STRMCLSTR(CURRMCL)
			end
		elsif highmem then
			am.usesib:=-1
			mode:=0
			rm:=5
		end
	end

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
				if a.size<4 then axerror("add imm/size") end
				genxrm(0x81, code, a)
				genopnd(b,4)
				return
			end

			x:=b.value
			dispsize:=1
			if a.size=1 then
				opc:=0x80
!				if x not in -128..127 then axerror("Exceeding i8 range") end
				checkimmrange(x,1)
				if x not in -128..255 then axerror("Exceeding i8/u8 range") end
			elsif -128<=x<=127 then
				opc:=0x83
			else
				checkimmrange(x,4)
				opc:=0x81
				dispsize:=(a.size=2|2|4)
			end

			genxrm(opc, code, a)

			case dispsize
			when 1 then genbyte(x)
			when 2 then genword(x)
			when 4 then gendword(x)
			end case
			fixrip(dispsize)

		else
			axerror("ADD reg,???")
		end case

	when a_mem then
		case b.mode
		when a_reg then
			opc:=code<<3 ior (b.size=1|0x00|0x01)
			genrrm(opc, b, a)

		when a_imm then
			goto doregimm
		else
			axerror("ADD mem,???")
		end case

	else
	cpl opndnames_ma[code],=CODE
		axerror("1:Can't add to this opnd")
	end case
end

proc do_mov(mclopnd a,b)=
	int regcode, opc, dispsize
	i64 value
	psymbol d:=getdef(b)

	case a.mode
	when a_reg then
		case b.mode
		when a_reg, a_mem then
			if a.size<>b.size and b.size then axerror("2:Opnd size mismatch") end

			genrrm((a.size=1|0x8A|0x8B), a, b)

		when a_imm then
			value:=b.value

			regcode:=getregcode(a.reg, bmask)
			setopsize(a)
			if d and a.size<=2 then axerror("mov imm?") end

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
						CPL value,^void(value)
						axerror("1:exceeding u32 value")
					end
doreg32:
					genrex()
					genbyte(0xB8+regcode)
					gendword(value)
				end

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
					end
!there might be short form for negative values that fit into 32 bits, using other opcs
!but ignore that for now
					rex ior:=wmask
					genrex()
					genbyte(0xB8+regcode)
					genqword(value)
				end

			end case

		else
			axerror("MOV REG/??")
		end case
	when a_mem then
		case b.mode
		when a_reg then

			if a.size=0 then a.size:=b.size end
			if a.size<>b.size and a.size then axerror("3:Opnd size mismatch") end
			genrrm((b.size=1|0x88|0x89), b, a)

		when a_imm then
			value:=b.value

			if a.size=0 then a.size:=1 end
			if d and a.size<=2 then axerror("mov imm?") end
			setopsize(a)
			opc:=(a.size=1|0xC6|0xC7)

			if not d then checkimmrange(value, a.size) end

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
			end case
			fixrip(dispsize)	
		else
			axerror("MOV MEM/?")
		end case
	else
		axerror("MOV ?/..")
	end case
end

proc do_push(mclopnd a)=
	int code

	if a.size=0 then a.size:=8 end

	case a.mode
	when a_reg then
		if a.size<>8 then axerror("pushreg not 64-bit") end
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
		end

	when a_mem then
		if a.size<>8 then axerror("push not 64-bit") end
		genxrm(0xFF, 6, a)

	else
		axerror("push opnd?")
	end case
end

proc do_pop(mclopnd a)=
	int code

	if a.size=0 then a.size:=8 end

	case a.mode
	when a_reg then
		if a.size<>8 then axerror("popreg not 64-bit") end
		code:=getregcode(a.reg, bmask)
		genrex()
		genbyte(0x58+code)

	when a_mem then
		if a.size<>8 then axerror("pop not 64-bit") end
		genxrm(0x8F, 0, a)
	else
		axerror("pop opnd?")
	end case
end

proc do_inc(mclopnd a,int code)=
!inc/dec

	case a.mode
	when a_reg, a_mem then
		genxrm((a.size=1|0xFE|0xFF), code, a)
	else
		axerror("inc/opnd?")
	end case
end

proc do_neg(mclopnd a,int code)=
!neg/not/mul/imul/div/idiv
	case a.mode
	when a_reg, a_mem then
		genxrm((a.size=1|0xF6|0xF7), code, a)
	else
		axerror("neg/div/etc opnd?")
	end case
end

proc do_lea(mclopnd a,b)=
	int regcode, am

	unless a.mode=a_reg and b.mode=a_mem then
		axerror("LEA not reg/mem")
	end

	if a.size<4 then
		CPL =A.SIZE
 axerror("LEA size error") end
	genrrm(0x8D, a, b)
end

proc do_movsx(mclopnd a,b,int opc)=
!opc=B6 for movzx, and BE for movsx
	int am, regcode

	if a.mode<>a_reg then axerror("movsx not reg") end

	if a.size=8 and b.size=4 then
		if opc=0xBE then
			do_movsxd(a,b)
		else						!movsx 4->8 bytes, do normal move 4->4
			a:=regtable[a.reg,4]
			do_mov(a,b)
		end
		return
	end

	if a.size=1 or a.size<=b.size then axerror("movsx size error") end
	if opc=0xB6 and b.size=4 then axerror("movsx 4=>8 bytes?") end

	case b.mode
	when a_reg then
	when a_mem then
		if b.size=0 then axerror("movsx need size prefix") end
		if b.size=8 then axerror("movsx size 8") end
	else
		axerror("movsx not reg/mem")
	end case

	genrrm(0x0F<<8+(b.size=1|opc|opc+1), a, b)
end

proc do_exch(mclopnd a,b)=
	int regcode, am

	if a.mode=a_reg and b.mode=a_reg and (a.reg=r0 or b.reg=r0) and a.size<>1 then		!simple r0/reg
		if a.reg<>r0 then				!get a to be r0
			swap(a,b)
		end
		if a.size<>b.size then axerror("exch size") end

		setopsize(a)
		regcode:=getregcode(b.reg, bmask)
		genrex()
		genbyte(0x90+regcode)
		return
	end

	if a.mode=a_mem then swap(a,b) end

	unless a.mode=a_reg and (b.mode=a_reg or b.mode=a_mem) then axerror("exch opnds") end
	if b.size=0 and b.mode=a_mem then b.size:=a.size end
	if a.size<>b.size then axerror("exch size") end

	genrrm((a.size=1|0x86|0x87), a, b)
end

proc do_movsxd(mclopnd a,b)=
	int regcode, am

	if b.mode=a_mem and b.size=0 then b.size:=4 end

	if a.size<>8 or b.size>4 then axerror("movsxd size") end

	if a.mode<>a_reg or (b.mode<>a_reg and b.mode<>a_mem) then
		axerror("movsxd opnds")
	end

	genrrm(0x63, a, b)
end

proc do_imul2(mclopnd a,b)=
	int regcode, am, opc, dispsize
	i64 value

	if a.mode<>a_reg then
		axerror("imul2 opnds")
	end
	if b.size=0 then b.size:=a.size end
	if a.size=1 then axerror("imul2 byte") end

	case b.mode
	when a_reg,a_mem then
		if a.size<>b.size then axerror("imul2 size") end

		genrrm(0x0F'AF, a, b)

	when a_imm then						!imul reg1,reg2,imm but implemented as imul reg,imm
		if getdef(b) then axerror("mul/label") end
		value:=b.value

		if -128<=value<=127 then
			opc:=0x6B
		else
			opc:=0x69
		end

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
		end
		fixrip(dispsize)
	else
		axerror("imul2 opnds")
	end case
end

proc do_shift(mclopnd a,b,int code)=
	int w,opc,needdisp

	if a.mode<>a_reg and a.mode<>a_mem then axerror("shift opnds1?") end
	if getdef(b) then axerror("shift/label") end
	w:=(a.size=1|0|1)
	needdisp:=0

	case b.mode
	when a_imm then
		if b.value=1 then
			opc:=0xD0+w
		else
			opc:=0xC0+w
			needdisp:=1
		end
	when a_reg then
		if b.reg<>r10 or b.size<>1 then axerror("cl or b10 needed") end
		opc:=0xD2+w
	else
		axerror("shift opnds2?")
	end case

	genxrm(opc, code, a)

	if needdisp then genbyte(b.value); fixrip(1) end
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
		end case

	elsif (a.mode=a_reg or a.mode=a_mem) and b.mode=a_imm then
		genxrm((a.size=1|0xF6|0xF7), 0, a)

		case a.size
		when 1 then
			genbyte(value)
		when 2 then
			genword(value)
		else
			gendword(value)
		end case
		fixrip(a.size)

	elsif a.mode in [a_reg, a_mem] and b.mode=a_reg then
domemreg:
		genrrm((a.size=1|0x84|0x85), a, b)

	elsif a.mode=a_reg and b.mode=a_mem then
		swap(a,b)
		goto domemreg
	else
		axerror("test opnds")
	end

end

proc do_setcc(int cond, mclopnd b)=
!a is cond
!b is byte reg/mem

	if b.mode not in [a_reg, a_mem] or b.size>1 then axerror("setcc opnd/size") end

	genxrm(0x0F'90+cond, 0, b)
end

proc checksize(mclopnd a, int size1=0, size2=0)=
	if a.size=0 then axerror("Need size") end
	if size1 and a.size not in [size1,size2] then
		CPL =A.SIZE
		axerror("Wrong size")
	end
end

proc do_arithxmm(mclopnd a,b,int prefix,opc)=
	if not isxreg(a) and checkxregmem(b) then
		axerror("arithxmm opnds")
	end

	if prefix then genbyte(prefix) end
	genrrm(0x0F<<8+opc, a, b)
end

proc do_logicxmm(mclopnd a,b,int opc,size)=
	int am, regcode

	if not isxreg(a) and checkxregmem(b) then
		axerror("logicxmm opnds")
	end

	if size=8 then genbyte(0x66) end

	genrrm(0x0F<<8+opc, a, b)
end

proc do_convertfloat(mclopnd a,b,int prefix)=
!cvtss2sd and cvtsd2ss
	int am, regcode

	if not isxreg(a) and checkxregmem(b) then
!	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("convertfloat opnds")
	end
	genbyte(prefix)
	nowmask:=1
	genrrm(0x0F'5A, a,b)
end

proc do_fix(mclopnd a,b,int prefix,opc)=
	int am, regcode

	if not isreg(a) or not checkxregmem(b) then
		axerror("fix opnds")
	end

	checksize(a, 4, 8)
	
	b.size:=(prefix=0xF3|4|8)

	genbyte(prefix)
	genrrm(0x0F<<8+opc, a, b)
end

proc do_float(mclopnd a,b,int prefix)=
!cvtss2si and cvtsd2si
	if not isxreg or not isreg(b) and not ismem(b) then
		axerror("float opnds")
	end

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

!CPL "MOV XMM", ISREG(A), ISREG(B)
!CPL "MOV XMM", ISXREG(A), ISXREG(B)
!
!CPL =MSTROPND(A)
!CPL =MSTROPND(B)

	case a.mode
	when a_reg then
!CPL $LINENO
		if isreg(a) then
!CPL $LINENO
			if isxreg(b) then
				if a.size<>size then axerror("1:movdq size") end
				b.size:=a.size

				sizeoverride:=1
				genrrm(0x0F'7E, b, a)
	
			else
				axerror("movdq reg,?")
			end
		else
!CPL $LINENO
			if isreg(b) then
				a.size:=b.size
				if b.size<>size then axerror("3:movdq size") end
				sizeoverride:=1
				genrrm(0x0F'6E, a, b)

			elsif isxreg(b) then
				a.size:=b.size
				f3override:=1
				genrrm(0x0F'7E, a, b)

			elsif ismem(b) then
!CPL $LINENO
				if b.size=0 then b.size:=a.size end
!			if b.size<>size then axerror("31:movdq size") end
				if b.size<>size then axerror("31:movdq size") end
!
				if size=4 then
					sizeoverride:=1
					nowmask:=1
					genrrm(0x0F'6E, a, b)

				else
					f3override:=1
					nowmask:=1
					genrrm(0x0F'7E, a, b)
!CPL $LINENO
				end

			else
				axerror("movdq xreg,?")
			end
		end
	when a_mem then
		if isxreg(b) then
			if a.size and a.size<>size then axerror("5:movdq size") end

			sizeoverride:=1
			genrrm((size=4|0x0F'7E|0x0F'D6), b,a)

		else
			axerror("movdq mem,?")
		end

	else
		axerror("movdq opnds")
	end case
end

proc fixrip(int dispsize)=
	^byte codeaddr
	^u32 offsetptr

	if not ripentry then return end

	case dispsize
	when 0 then return
	when 1,2,4 then
	else
CPL =DISPSIZE
		axerror("fixrip disp?")
	end case
	ripentry.immsize:=dispsize
end

proc do_bswap(mclopnd a)=
	int code
	if not isreg(a) or a.size<4 then axerror("bswap reg>") end

	setopsize(a)

	code:=getregcode(a.reg, bmask)

	genrex()
	genbyte(0x0F)
	genbyte(0xC8 + code)
end

proc do_movdqx(mclopnd a, b, int prefix)=
	prefix:=prefix<<16 + 0x0F<<8

	if a.size=0 then a.size:=16 end
	if b.size=0 then b.size:=16 end

	if a.mode=a_mem then
		genrrm(prefix+0x7F, b, a)
	else
		genrrm(prefix+0x6F, a, b)
	end
end

proc do_dshift(mclopnd a, b, int c, opc)=

	if a.size=0 then a.size:=b.size end
	if a.size<>b.size or a.size<=1 then axerror("dshift/size") end

	sizeoverride:=0
	genrrm(0x0F<<8+opc, b, a)
	genbyte(c)
end

func checkxregmem(mclopnd a)int=
!return 1 if a is either an xreg, or mem
	if a.mode=a_reg and a.reg>=xr0 or a.mode=a_mem then
		1
	else
		0
	end
end
=== mcx_lib.m 0 0 23/74 ===
const fuseregtable=1
!const fuseregtable=0

global const targetsize=8

!global int mclseqno
global int mclseqno
global int NMCLOPND

[-1..10]mclopnd smallinttable
![20]psymbol nametable
!int nnametable

global macro isframex(d) = (d.nameid in [frameid, paramid])

global proc mclinit(int bypass=0)=
	mclopnd a
	int r, s

	if mclrec.bytes>64 then ABORTPROGRAM("MCLREC>64B") end

	for r:=r0 to r15 do
		regtable[r, 1]:=genreg0(r, 1)
		regtable[r, 2]:=genreg0(r, 2)
		regtable[r, 4]:=genreg0(r, 4)
		regtable[r, 8]:=genreg0(r, 8)
	end

	for i in frameregtable.bounds do
		a:=newmclopnd()
		a.mode:=a_mem
		a.reg:=rframe
		a.size:=8
		a.offset:=i
		frameregtable[i]:=a
	end

	dframeopnd:=genreg(rframe, 8)
	dstackopnd:=genreg(rstack, 8)

	initmcdest()

	setsegment('C')

	lab_funcnametable:=0
	lab_funcaddrtable:=0

!bypass is used when directly using mcl api (eg. from an external assembler)
!then genmcl(), called from pcl functions, is a no-op
	if bypass then
		mcldone:=1
	end
end

proc start=
	for i in smallinttable.bounds do
		smallinttable[i]:=genint0(i, 8)
	end
end

global proc initmcdest=
!reset mccode/mccodex
!called should have saved any values from last linked list 
	mccode:=mccodex:=nil

!add dummy op at start, to avoid having to check every time whether it is
!the first op
	mccode:=pcm_allocnfz(mclrec.bytes)
	mccode.opcode:=m_nop

	mccodex:=mccode

!	clear rtsproclabels
end

EXPORT proc genmc(int opcode, mclopnd a=nil, b=nil)=		!used in do_mcl/assem in host
	^mclrec m, oldm

	m:=pcm_allocnfz(mclrec.bytes)

	m.opcode:=opcode
	m.seqno:=++mclseqno
	m.mpos:=mmpos

	if a then						!try and detect use of rframe
		m.a:=a
		usedset.[a.reg]:=1		!assume will not be .regix
		if b then
			m.b:=b
			usedset.[b.reg]:=1
		end
	end

	case opcode
	when m_mov then				!change to movd/q if needed
		if a.reg>=xr0 or (b and b.reg>=xr0) then
			m.opcode:=(a.size=8|m_movq|m_movd)
		end
	elsecase mclauto[opcode]
	when 1 then					!add/addss/addsd etc
		if a.mode=a_reg and a.reg>=xr0 then
			++m.opcode			!addss etc
			if a.size=8 then
				++m.opcode
			end
		elsif b.mode=a_reg and b.reg>=xr0 then
			++m.opcode			!addss etc
			if b.size=8 then
				++m.opcode
			end
		end

	when 2 then					!sqrtss/sqrtsd etc
		if a.mode=a_reg and a.reg>=xr0 and a.size=8 then
			++m.opcode			!sqrtsd etc
		elsif b and b.mode=a_reg and b.reg>=xr0 and b.size=8 then
			++m.opcode			!sqrtsd etcc
		end

	end case

	m.lastmcl:=mccodex
	mccodex.nextmcl:=m
	mccodex:=m
end

EXPORT proc genmc_lab(int labelno)=
	^mclrec m, oldm
	mclopnd a

	m:=pcm_allocnfz(mclrec.bytes)

	m.opcode:=m_label
	m.seqno:=++mclseqno
	m.mpos:=mmpos

	a:=newmclopnd()
	a.mode:=a_imm

	a.value:=labelno
	a.valtype:=label_val
	a.size:=8

	m.a:=a

	m.lastmcl:=mccodex
	mccodex.nextmcl:=m
	mccodex:=m
end

global proc genmc_cond(int opcode, cond, mclopnd a=nil, b=nil)=
	genmc(opcode, a, b)
	mccodex.cond:=cond
end

global proc genmc_label(int opcode, labelno)=
	genmc(opcode, genlabel(labelno))
end

global proc genmc_string(int opcode, ichar s)=
!as genmc but uses a single immediate string operand
	genmc(opcode, genstring(s))
end

global proc genmc_def(int opcode,  psymbol d)=
!as genmc but uses a single immediate string operand
	genmc(opcode, genmem(d))
end

!global proc genmc_defaddr(int opcode,  psymbol d)=
!!as genmc but uses a single immediate string operand
!	genmc(opcode, genmemaddr(d))
!end

global proc genmc_int(int opcode,  int a)=
!as genmc but uses a single immediate string operand
	genmc(opcode, genint(a))
end

global proc genmc_name(int opcode,  ichar name)=
!as genmc but uses a single immediate string operand
	genmc(opcode, genname(name))
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

EXPORT func genindex(int areg=0, ireg=0, scale=1, offset=0, size=0, labno=0, psymbol def=nil)mclopnd=
!construct a mem address mode
	mclopnd a
	a:=newmclopnd()

	a.mode:=a_mem
	a.reg:=areg

!	if areg=rframe or ireg=rframe then usedset.[rframe]:=1 end

	a.regix:=ireg
	a.scale:=scale
	a.size:=size

	a.offset:=offset

	if labno then
		a.value:=labno
		a.valtype:=label_val
	elsif def then
		a.def:=def
!		++def.nrefs
		a.valtype:=def_val
		if isframex(def) then
			a.reg:=rframe
!			usedset.[rframe]:=1
		end
	end

	return a
end

global proc mcomment(ichar s)=
!if not debugmode then return end
!	if s=nil or s^=0 then
!		genmc(m_blank)
!	else
		genmc_string(m_comment, s)
!	end
end

global func genstring(ichar s, int length=-1)mclopnd=
	mclopnd a
	a:=newmclopnd()
	a.mode:=a_imm

	if length<0 then
		length:=strlen(s)
	end

	a.svalue:=pcm_alloc(length+1)
	memcpy(a.svalue, s, length)
	(a.svalue+length)^:=0

	a.valtype:=string_val
	a.size:=8
	return a
end

!global func gendata(^byte p, int length)mclopnd=
!	mclopnd a
!	a:=newmclopnd()
!	a.mode:=a_imm
!	a.svalue:=pcm_copyheapstringn(p, length)
!
!	a.valtype:=data_val
!	a.size:=length
!	return a
!end

global func genname(ichar s)mclopnd=
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
		end case
		if mccodex and mccodex.opcode in [m_isegment, m_zsegment, m_csegment] then
			mccodex.opcode:=opc
		else
			genmc(opc)
		end

		currsegment:=seg
	end

	if align>1 then
		if mccodex.opcode=m_align then
			oldalign:=mccodex.a.value
			if oldalign>=align then return end
		end
		genmc(m_align, genint(align))
	end
end

global func changeopndsize(mclopnd a, int size)mclopnd=
	mclopnd b

	if a.size<>size then
		if a.reg<xr0 and a.mode=a_reg then
			b:=regtable[a.reg, size]
		else
			b:=duplopnd(a)
			b.size:=size
		end
		return b
	end
	return a
end

global func applyoffset(mclopnd a, int offset, int size=0)mclopnd=
!astr is an asm operand
!add possible byte offset
	mclopnd b

	if offset=0 and size=0 then
		return a
	end
	b:=duplopnd(a)
	b.offset+:=offset
	if size then
		b.size:=size
	end

	return b
end

global func genlabel(int x)mclopnd a=
!x is a label index
!generate immediate operand containing label
	a:=newmclopnd()
	a.mode:=a_imm

!	if x=0 then x:=++mlabelno end
	a.value:=x
	a.valtype:=label_val
	a.size:=8

	return a
end

global func genlabelmem(int x)mclopnd a=
!x is a label index
!generate immediate operand containing label

	a:=genlabel(x)
	a.mode:=a_mem
	return a
end

EXPORT func genmemaddr(psymbol d)mclopnd=
	mclopnd a

!*!	d.addrof:=1
!	++d.nrefs

	a:=newmclopnd()
	a.mode:=a_imm

	a.def:=d
!	++d.nrefs
	a.valtype:=def_val
	a.size:=8

	return a
end

global func genint(i64 x, int size=8)mclopnd a=

	if x in -1..10 and size=8 then
		return smallinttable[x]
	end

	a:=newmclopnd()
	a.mode:=a_imm

	case size
	when 1 then x iand:=255
	when 2 then x iand:=65535
	when 4 then x iand:=0xffff'ffff
	end case

	a.value:=x
	a.valtype:=int_val
	a.size:=size

	return a
end

global func genint0(i64 x, int size=8)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_imm

	a.value:=x
	a.valtype:=int_val
	a.size:=size

	return a
end

global func genrealmem(r64 x, int size=8)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_mem

	if size=8 then
		a.value:=getrealindex(x)
	else
		a.value:=getr32index(x)

	end
	a.valtype:=label_val
	a.size:=size
	return a
end

global func genrealimm(r64 x, int size=8)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_imm
	a.xvalue:=x
	a.valtype:=real_val
	a.size:=size
	return a
end

global func genmem(psymbol d, int size=0)mclopnd a=
	int reg

	if size=0 then
		size:=min(ttsize[d.mode], 8)
	end

!	IF D.MX and size=d.mx.size then
!!CPL "REUSE D.MX"
!		return d.mx
!	end

	if d.reg then
		return genreg(d.reg, size)
	end

	reg:=rnone
	if isframex(d) then
!		if not foptim and (int(d.offset) in -128..64) and ttsize[d.mode]=8 then
!			return frameregtable[d.offset]
!		end

		reg:=rframe
!		usedset.[rframe]:=1

	end

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.def:=d
!	++d.nrefs
	a.valtype:=def_val

	if size then
		a.size:=size
	else
		a.size:=min(ttsize[d.mode], 8)
	end
	if a.size=0 then a.size:=8 end

!	IF D.MX=NIL THEN D.MX:=A FI

	return a
end

global func genreg0(int reg, size=8)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_reg
	a.reg:=reg
	a.size:=size

IF SIZE=0 THEN MERROR("1:SIZE=0") FI
	return a
end

EXPORT func genxreg(int reg, size=8)mclopnd=
	mclopnd a

!	if xreg=rnone then xreg:=++currxregno end
	a:=newmclopnd()

	a.mode:=a_reg
	a.reg:=reg
	a.size:=size
	return a
end

global func genreg(int reg, int size=8)mclopnd a =

!	if stdfloat[mode] and reg>=xr0 then
	if reg>=xr0 then
		genxreg(reg, size)
	else
		if fuseregtable then
			return regtable[reg, size]
		end
		return genreg0(reg, size)
	end
end

!global func genregi(int reg, mode=ti64)mclopnd a =
!!	if fuseregtable then
!!		return regtable[reg, stdsize[mode]]
!!	end
!	return genreg0(reg, stdsize[mode])
!end

global func genireg(int reg, size=8, offset=0)mclopnd=
	mclopnd a

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.size:=size
	a.offset:=offset

	return a
end

global func roundsizetg(int size)int=
!make sure size is round up to next multiple of targetsize (4 or 8)
	if size iand 7=0 then return size end
	return size+(8-(size iand 7))
end

!global proc merroropnd(ichar mess, int opndtype)=
!	fprintln "MCL Opnd not supported: # (#) [#]", mess, opndnames_ma[opndtype]
!	PRINTLN
!	STOP 1
!!	stopcompiler(sourcefilepaths[mmpos>>24], mmpos iand 16777215)
!end

global func mcreatefwdlabel:int =
	return ++mlabelno
end

global proc mdefinefwdlabel(int lab) =
!	genmc(m_label, genlabel(lab))
	genmc_lab(lab)
end

global func mdefinelabel:int =
!	genmc(m_label, genlabel(++mlabelno))
	genmc_lab(++mlabelno)
	mlabelno
end

global func genextname(ichar s)mclopnd=
	[64]char str
	psymbol d
	static [20]psymbol table
	static int ntable

	strcpy(str, s)
!	str[strlen(s)]:=0			!lose final *

	d:=findnamesym(str)

	if not d then
		d:=pcm_allocnfz(strec.bytes)

		d.name:=pcm_copyheapstring(str)
		d.nameid:=dllprocid
		d.isimport:=1
		addnamesym(d)
	end

	return genmemaddr(d)
end

!global func getprimreg(mclopnd ax)int =
!!get primary reg value; only one should be active
!!return 0 if no regs
!!//error if both regs are active
!
!	if ax.reg then
!!		if ax.regix then merror("getprim?") end
!		ax.reg
!	else
!		ax.regix	!0 if no regs used
!	end
!end

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
		genmc(m_sub, dstackopnd, genint(n))
	end
end

global proc popstack(int n)=
	if n then
		genmc(m_add, dstackopnd, genint(n))
	end
end

global func getstringindex(ichar s, int length)int=
	if s=nil then			!assume nil
		kk0used:=++mlabelno
		return kk0used
	end

	if cstringlist and length=cstringlist.slength and eqbytes(cstringlist.svalue, s, length) then
		return cstringlist.labelno
	end

	return addconst(cstringlist, cast(s), length)
end

global func addconst(^constrec &clist, int value=0, length=0)int=
	^constrec p
	p:=pcm_allocnfz(constrec.bytes)
	p.value:=value
	p.slength:=length
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
		end
	end
	return 0
end

global proc axerror(ichar mess)=
	CPL "AX ERROR:", mess, "AASEQ:", aaseqno
	CPL
	STOP 1

end

global func findnamesym(ichar s)psymbol d=
!search for s in cache of named psymbols

	for i to nnametable do
		if eqstring(s, nametable[i].name) then
			return nametable[i]
		end
	end
	nil
end

global proc addnamesym(psymbol d)=
!add new name psymbol, which should be unique

	if nnametable<nametable.len then
		nametable[++nnametable]:=d
	else
		merror("Ext nametab overflow")
	end
end

global proc clearreg(mclopnd ax)=
	if ax.size=8 then
		ax:=changeopndsize(ax, 4)
	end
	genmc(m_xor, ax, ax)
end

global proc genstringtable=
	^constrec p

	return unless cstringlist

	mcomment("String Table")

	setsegment('I', 8)

!	if kk0used then
!		genmc(m_label, genlabel(kk0used))
!		gendb(0)
!	end

	p:=cstringlist
	while p, p:=p.nextconst do
		genmc_label(m_label, p.labelno)
		genstring_db(p.svalue, p.slength, strtype:1)
!		genstring_db(p.svalue, strtype:0)
	end
end

global proc mcomm(ichar s, t="", u="")=
	[256]char str
	print @str, s, t, u
	mcomment(pcm_copyheapstring(str))
end

global proc genstring_db(ichar s, int length, strtype)=
!string table generated in ax pass, so is just text
!this is target-specific, so should really be moved
!strtype should be zero for a normal string, then a zero-terminator is added.
	int i, c, seqlen
	^char seq

	if length=-1 then
		length:=strlen(s)
	end

	if length=0 then
		gendb(0)
		return
	end

	seqlen:=0

	to length do
		c:=s++^
!		if c<32 or c>=127 or c='\"' then
		if c<32 or c>=127 or c in ['\"', '\\'] then
			if seqlen then
				gendbstring(seq, seqlen)
				seqlen:=0
			end
			gendb(c)
		else
			if seqlen=0 then
				seqlen:=1
				seq:=s-1
			else
				++seqlen
			end
		end
	end
	if seqlen then
		gendbstring(seq,seqlen)
	end
	if strtype=0 then
		gendb(0)
	end
end

proc gendbstring(ichar s, int length)=
!string is printable, and doesn't include double quotes
	genmc(m_ascii, genstring(s, length))
end

proc gendb(int a)=
	genmc(m_db,genint(a))
end

global func getnexttemp:int=
!pcl temps are used to spill pcl operands from a register
!they will always be 64 bits

	for i to npcltemps do
		if pcltempflags[i]=0 then			!available
			pcltempflags[i]:=1
			return i
		end
	end

	if npcltemps>=maxoperands then merror("Too many temps") end
	++npcltemps
	pcltempflags[npcltemps]:=1

	npcltemps
end

global func gentemp(int temp, size)mclopnd a=
!pcl temps are used to spill pcl operands from a register
!they will always be 64 bits
	int reg

!look for an existing, freed temp slot
!	if pcltempopnds[temp] then
!		return changeopndsize(pcltempopnds[temp], size)
!	fi

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=rframe
!	usedset.[rframe]:=1
	a.valtype:=temp_val
	a.size:=size
	a.tempno:=temp

A.OFFSET:=9999

!	pcltempopnds[temp]:=a

	return a
end

global proc genrealtable=
	^constrec p

	return unless creallist or cr32list

	mcomment("Real Table")
	setsegment('I',8)
	p:=creallist
	while p, p:=p.nextconst do
		genmc(m_label,genlabel(p.labelno))

!		if p.xvalue=infinity then
!			genmc(m_dq, genint(u64@(p.xvalue)))
!		else
!			genmc(m_dq, genrealimm(p.xvalue, 8))
!		end
		if p.xvalue=infinity then
			genmc(m_dq, mgenint(u64@(p.xvalue)))
		else
			genmc(m_dq, mgenint(u64@(p.xvalue)))
		end
	end

	mcomment("Real32 Table")
	p:=cr32list
	p:=cr32list
	while p, p:=p.nextconst do
		genmc(m_label, genlabel(p.labelno))
!		if p.xvalue=infinity then
			genmc(m_dd, genint(i32@(r32(p.xvalue))))
!		else
!			genmc(m_dd, mgenrealimm(p.xvalue, tr32))
!		end

	end
end

global proc merror(ichar mess,ichar param="")=
	int lineno
	ichar filename, sourceline

!	if igetmsourceinfo then
!		lineno:=igetmsourceinfo(mmpos, filename, sourceline)
!		CPL =LINENO
!		CPL =FILENAME
!	else
CPL "NO LINE INFO"
		lineno:=0
		filename:="?"
!	end

!	if currfunc then
!		println "Proc:", currfunc.name
!	end

	fprintln "MCL Error: # (#) on Line: # in #",mess,param, lineno, filename
OS_GETCH()
	stop 1
!	pcerrorstop(filename, lineno)
end

!global func getbasename(ichar s)ichar t=
!	t:=s+strlen(s)-1
!	while t>s and (t-1)^<>'.' do
!		--t
!	end
!
!	return t
!end

!global func gentemp(int n, mode)mclopnd a=
!!pcl temps are used to spill pcl operands from a register
!!they will always be 64 bits
!	mgentemp(n, stdsize[mode])
!end

global func mgenint(i64 x, int mode=ti64)mclopnd a=
	genint(x, ttsize[mode])
end

!global func mgenrealmem(r64 x, int mode=tr64)mclopnd a=
!	genrealmem(x, ttsize[mode])
!end

global func mgenrealimm(r64 x, int mode=tr64)mclopnd a=
	genrealimm(x, ttsize[mode])
end

global func mgenmem(psymbol d, int mode=tvoid)mclopnd a=
	genmem(d, ttsize[mode])
end

global func mgenreg(int reg, int mode=tu64)mclopnd a =
	genreg(reg, ttsize[mode])
end

global func mgenireg(int reg, mode=tu64, offset=0)mclopnd=
	genireg(reg, ttsize[mode])
end

global func mgentemp(int n, mode)mclopnd a=
	gentemp(n, ttsize[mode])
end

global func isreg(mclopnd b)int=
	return b.mode=a_reg and b.reg<xr0
end

global func isxreg(mclopnd b)int=
	return b.mode=a_reg and b.reg>=xr0
end

global func ismem(mclopnd b)int=
	return b.mode=a_mem
end

=== mcx_optim.m 0 0 24/74 ===
!This version does a proc at a time
!Caller provides mcl ptr to near start of proc code just after entry code

global proc peephole(mcl m)=
	mcl m2,m3
	int lab1,lab2

	if foptimise<2 then return end
!CPL "PEEP", CURRFUNC.NAME

	while m, m:=m.nextmcl do 
		if m.opcode=m_procend then
			exit
		end


!CPL "PEEPLOOP", MCLNAMES[M.OPCODE], currfunc.name
		m2:=m.nextmcl
!CPL =M2
		m3:=m2.nextmcl
!CPL =M3
		case m.opcode
!		when m_procend then
!			exit

		when m_mov then
!CPL "MOV", MCLNAMES[M2.OPCODE]

			case m2.opcode
			when m_mov then					!mov/mov
!CPL "MOV/MOV?", ISREG0(M.A), M.A=M2.B, ENDR0(M2)
				if isreg0(m.a) and m.a=m2.b and endr0(m2) then		!mov r0,x/mov x,r0
!CPL "MOV/MOV1"
					if isreg10(m2.a) then
					elsif isreg(m2.a) or isreg(m.b) then				!one x is a reg
						m.a:=m2.a
						deletemcl(m2)
					end
				elsif isreg0(m.a) and m.a=m2.b and isreg10(m2.a) and m3.opcode=m_call and
						endr0(m3) then
!CPL "MOV/MOV2"
					m.a:=m2.a
					deletemcl(m2)
				end
			when m_test then				!mov/test
				if isreg0(m.a) and m.a=m2.a=m2.b and isreg(m.b) and endr0(m3) then		!mov r0,x/test r0,r0
!CPL "MOV/TEST"
					m.opcode:=m_test
					m.a:=m.b
					m:=deletemcl(m2)
				end
			when m_cmp then					!mov r0, reg/cmp r0,x
				if isreg0(m.a) and m.a=m2.a and isreg(m.b) and endr0(m3) then
!CPL "MOV/CMP"
					m.opcode:=m_cmp
					m.a:=m.b
					m.b:=m2.b
					deletemcl(m2)
				end
			when m_add, m_sub then
				if isreg(m.a) and m.a=m2.a and isreg(m.b) and isconst(m2.b) and m.a.size=8 then
!CPL "MOV/ADD"
					m.opcode:=m_lea
					m.b:=genindex(areg:m.b.reg, offset: (m2.opcode=m_add|m2.b.value|-m2.b.value))
					deletemcl(m2)
				end
			when m_inc, m_dec then
				if isreg(m.a) and m.a=m2.a and isreg(m.b) AND M.A.SIZE=8 then
!CPL "MOV/INC"
					m.opcode:=m_lea
					m.b:=genindex(areg:m.b.reg, offset: (m2.opcode=m_inc|1|-1))
					deletemcl(m2)
				end
			when m_jmp then
				if isreg0(m.a) and isreg0(m2.a) then
!CPL "MOV/JMP"
					m.opcode:=m_jmp
					m.a:=m.b
					m.b:=nil
					deletemcl(m2)
				end
			end case

		when m_and then
			if m2.opcode=m_test then				!and r0../test r0,r0 -> and r0.. only
				if isreg0(m.a) and m.a=m2.a=m2.b and endr0(m3) then
					m:=deletemcl(m2)
				end
			end
		when m_xor then
			if m2.opcode=m_mov then					!xor r0,r0; mov reg, r0
				if isreg0(m.a) and m.a=m.b and isreg(m2.a) and isreg0(m2.b) and endr0(m2) then
!CPL "XOR"
					m.a:=m.b:=m2.a
					m:=deletemcl(m2)
				end
			end

		when m_jmpcc then
			if m2.opcode=m_jmp and m3.opcode=m_label and m.a.labelno=m3.a.labelno and endr0(m) then
!CPL "JMPCC"
				m.cond:=asmrevcond[m.cond]
				m.a:=m2.a
				m:=deletemcl(m2)

			end

!		when m_add then
!			if m2.opcode in [m_add, m_sub] then
!				if isreg(m.a) and m.a=m2.a and isreg(m.b) and isconst(m2.b) then
!STATIC INT AA
!!					m.opcode:=m_lea
!!					m.b:=mgenindex(areg:m.b.reg, offset: (m2.opcode=m_add|m2.b.value|-m2.b.value))
!!					deletemcl(m2)
!CPL "ADD/ADD/SUB NN",++AA
!				end
!			end
!
!!!		when m_jmp then			!this uses more bytes than it saves, when self-hosting
!!!			if m.a.mode=a_imm and m2.opcode=m_label and m.a.labelno=m2.a.labelno then
!!!				m:=deletemcl(m)
!!			FI

		end case
	end

end

func isreg(mclopnd a)int=
	return a.mode=a_reg
end

func isreg0(mclopnd a)int=
	if not a then return 0 end
	if a.mode=a_reg and a.reg=r0 then return 1 end
	return 0
end

func isreg10(mclopnd a)int=
	if not a then return 0 end
	if a.mode=a_reg and a.reg=r10 then return 1 end
	return 0
end

!func isreg00(^mclrec m)int=
!	if isreg(m.a) and m.a=m.b then return 1 end
!	0
!end

func isconst(mclopnd a)int=
	if not a then return 0 end
	if a.mode=a_imm and a.valtype=int_val then
		return 1
	end
	return 0
end

!func sameoperand(mclopnd a,b)int=
!	return memcmp(a,b,mclopndrec.bytes)=0
!end
!
!func sameregopnd(mclopnd a,b)int=
!!check if same register operand
!	unless a.mode=b.mode=a_reg then return 0 end
!	return a.reg=b.reg
!end

!func deletemcl(^mclrec p, ichar comment=nil)^mclrec =
func deletemcl(^mclrec p)^mclrec =
!delete p; return following instr
	^mclrec a,b

	a:=p.lastmcl
	b:=p.nextmcl
	if a=nil or b=nil then merror("delmcl?") end

!	if comment then
!		p.opcode:=m_comment
!		p.a:=mgenstring(pcm_copyheapstring(comment))
!	else
		a.nextmcl:=b
		b.lastmcl:=a
!	end

	b
end

func endr0(^mclrec m)int=
	return m.freedset.[r0]
end
=== mcx_pedecls.m 0 0 25/74 ===
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
		^dbuffer data		!copy of ss_zdata etc
		^byte bytedata		!added later, eg, import dir block
	end
	ichar name					!name like ".bss" as it will be in obj/exe file
	int segtype					!code_seg etc
	int rawsize					!in file
	int rawoffset				!offset in exe file
	int virtsize				!in image
	int virtoffset				!offset from imagebase
	^relocrec relocs			!for idata/code: reloc info needs to be processed
	int nrelocs					!
end

global record importrec = 				!details about all imported symbols
	psymbol def					!full st entry
	int libno					!which dll lib this belongs to
	ichar name					!name of symbol (extracted from lib.name if needed)
	int hintnameoffset			!voffset of hint/name entry in impdir section
	int iatoffset				!voffset of IAT entry
	int thunkoffset				!offset within code section of thunk entry
end

global record exportrec = 		!details about all exported symbols
	psymbol def					!full st entry
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
=== mi.m 0 0 26/74 ===
!project =
	module mm_cli

!	module mm_genc
!	module mm_libc
!	module mm_blockc

	module mm_genpcl
	module mm_libpcl
	module mm_blockpcl

	module mm_assem
!	module mm_assem_dummy
!	module mm_assemaux
	module mm_assemaux_dummy
	module mm_decls

!	module mm_diags
	module mm_diags_dummy
!
	module mm_export_dummy
!	module mm_exportq
!	module mm_exportm

	module mm_lex
	module mm_lib

	module mm_libsources
!	module mm_libsources_dummy
!
	module mm_modules
	module mm_name
	module mm_parse

	module mm_support
	module mm_tables
	module mm_type

	$sourcepath "c:/px/"
!	$sourcepath "c:/xxx/"
!	import pcl
	import pclint
!	import pclmin
!	import pclrunx

!	$sourcepath "c:/qx/"
!	import qc

!end

proc main=
	main2()
end

=== mlib.m 0 0 27/74 ===
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

!GLOBAL REF VOID ALLOCBASE

byte pcm_setup=0

int show=0

global int memtotal=0
export i64 smallmemtotal=0
global int smallmemobjs=0
global int maxmemtotal=0

!store all allocated pointers
const int maxmemalloc=(mem_check|500000|2)
[maxmemalloc+1]^i32 memalloctable
[maxmemalloc+1]i32 memallocsize

const pcheapsize=1048576*2
^byte pcheapstart
^byte pcheapend			!points to first address past heap
^byte pcheapptr

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

export [0:9]^word freelist

export record strbuffer =
	ichar strptr
	i32 length
	i32 allocated
end

export enumdata [0:]ichar pmnames=
	(pm_end=0,		$),
	(pm_option,		$),
	(pm_sourcefile,	$),
	(pm_libfile,	$),
	(pm_colon,		$),
	(pm_extra,		$),
end

[2]int seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)

!PROC START=
!CPL "MLIB START"
!END


export function pcm_alloc(int n)^void =
	^byte p

	if not pcm_setup then
		pcm_init()
	end

	if n>maxblocksize then			!large block allocation
		alloccode:=pcm_getac(n)
		allocbytes:=allocupper[alloccode]

		p:=allocmem(allocbytes)
		if not p then
			abortprogram("pcm_alloc failure")
		end

		return p
	end

	alloccode:=sizeindextable[n]		!Size code := 0,1,2 etc for 0, 16, 32 etc
	allocbytes:=allocupper[alloccode]

!	smallmemtotal+:=allocbytes

	if p:=^byte(freelist[alloccode]) then		!Items of this block size available
		freelist[alloccode]:=^word(int((freelist[alloccode])^))

		return p
	end

!No items in freelists: allocate new space in this heap block
	p:=pcheapptr				!Create item at start of remaining pool in heap block
	pcheapptr+:=allocbytes			!Shrink remaining pool

	if pcheapptr>=pcheapend then		!Overflows?
		p:=pcm_newblock(allocbytes)		!Create new heap block, and allocate from start of that
		return p
	end

!IF MDEBUG THEN CPL "SMALLALLOC2", =PCHEAPPTR, =P FI
	return p
end

export proc pcm_free(^void p,int n) =
!n can be the actual size requested it does not need to be the allocated size
	int acode

	return when n=0 or p=nil

	if n>maxblocksize then		!large block
		free(p)
	else
		acode:=sizeindextable[n]		!Size code := 0,1,2 etc for 0, 16, 32 etc
		cast(p,^word)^:=word(int(freelist[acode]))
		freelist[acode]:=p
	end
end

export proc pcm_freeac(^void p,int alloc) =
	pcm_free(p,allocupper[alloc])
end

export proc pcm_clearmem(^void p,int n) =
	memset(p,0,n)
end

export proc pcm_init =
!set up sizeindextable too
	int j, k
	i64 size
	const limit=1<<33

	alloccode:=0
	if pcm_setup then
		return
	end

	pcm_newblock(0)

	for i to maxblocksize do	!table converts eg. 78 to 4 (4th of 16,32,64,128)
		j:=1
		k:=16
		while i>k do
			k:=k<<1
			++j
		end
		sizeindextable[i]:=j
	end

	allocupper[1]:=16
	size:=16

	for i:=2 to 27 do
		size*:=2
		allocupper[i]:=size
		if size>=threshold then
				k:=i
			exit
		end
	end

	for i:=k+1 to allocupper.upb do
		size+:=alloc_step
		if size<limit then
			allocupper[i]:=size
			maxmemory:=size
		else
			maxalloccode:=i-1
			exit
		end
		
	end
	pcm_setup:=1
end

export function pcm_getac(int size)int =
! convert linear blocksize from 0..approx 2GB to 8-bit allocation code

!sizeindextable scales values from 0 to 2048 to allocation code 0 to 9

	if size<=maxblocksize then
		return sizeindextable[size]		!size 0 to 2KB
	end

	size:=(size+255)>>8					!scale by 256

!now same sizetable can be used for 2KB to 512KB (288 to 2KB)

	if size<=maxblocksize then
		return sizeindextable[size]+8
	end

!sizetable now used for 512KB to 128MB (to 2KB)
	size:=(size+63)>>6					!scale by 256

	if size<=maxblocksize then
		return sizeindextable[size]+14
	end

!size>2048, which means it had been over 128MB.
	size:=(size-2048+2047)/2048+22
	return size
end

export function pcm_newblock(int itemsize)^void=
!create new heap block (can be first)
!also optionally allocate small item at start
!return pointer to this item (and to the heap block)
	static int totalheapsize
	^byte p

	totalheapsize+:=pcheapsize
	alloccode:=0
	p:=allocmem(pcheapsize)	!can't free this block until appl terminates
	if p=nil then
		abortprogram("Can't alloc pc heap")
	end
	memset(p,0,pcheapsize)

	pcheapptr:=p
	pcheapend:=p+pcheapsize

	if pcheapstart=nil then		!this is first block
		pcheapstart:=p
	end
	pcheapptr+:=itemsize
	return ^u32(p)
end

export function pcm_round(int n)int =
!for any size n, return actual number of bytes that would be allocated
	static [0:maxblockindex+1]i32 allocbytes=(0,16,32,64,128,256,512,1024,2048)

	if n>maxblocksize then
		return n
	else
		return allocbytes[sizeindextable[n]]
	end
end

export function pcm_allocz(int n)^void =
	^void p
	p:=pcm_alloc(n)

	memset(p,0,n)
	return p
end

export function pcm_copyheapstring(^char s)^char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
	^char q
	int n
	if s=nil then return nil end

	n:=strlen(s)+1
	q:=pcm_alloc(n)
	memcpy(q,s,n)
	return q
end

export function pcm_copyheapstringn(^char s,int n)^char =
	^char q
	if s=nil then return nil end

	q:=pcm_alloc(n+1)
	memcpy(q,s,n)
	(q+n)^:=0
	return q
end

export function pcm_copyheapblock(^char s, int length)^char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
	^char q
	if length=0 then return nil end

	q:=pcm_alloc(length)
	memcpy(q,s,length)
	return q
end

export function allocmem(int n)^void =
	^void p

	p:=malloc(n)
	if p then
		return p
	end
	println n,memtotal
	abortprogram("Alloc mem failure")
	return nil
end

global function reallocmem(^void p,int n)^void =
	p:=realloc(p,n)
	return p when p
	println n
	abortprogram("Realloc mem failure")
	return nil
end

export proc abortprogram(^char s) =
	println s
	print   "ABORTING: Press key..."
!os_getch()
	stop 5
end

export function getfilesize(filehandle handlex)int=
	u32 p,size

	p:=ftell(handlex)		!current position
	fseek(handlex,0,2)		!get to eof
	size:=ftell(handlex)		!size in bytes
	fseek(handlex,p,seek_set)	!restore position
	return size
end

export proc readrandom(filehandle handlex, ^byte memx, int offset, size) =
	int a
	fseek(handlex,offset,seek_set)
	a:=fread(memx,1,size,handlex)			!assign so as to remove gcc warning
end

export function writerandom(filehandle handlex, ^byte memx, int offset,size)int =
	fseek(handlex,offset,seek_set)
	return fwrite(memx,1,size,handlex)
end

export function setfilepos(filehandle file,int offset)int=
	return fseek(file,offset,0)
end

export function getfilepos(filehandle file)int=
	return ftell(file)
end

export function readfile(^char filename)^byte =
	filehandle f
	int size
	^byte m,p

	f:=fopen(filename,"rb")
	if f=nil then
		return nil
	end
	rfsize:=size:=getfilesize(f)

	m:=malloc(size+2)		!allow space for etx/zeof etc

	if m=nil then
		return nil
	end

	readrandom(f,m,0,size)
	p:=m+size			!point to following byte
	(^u16(p)^:=0)	!add two zero bytes

	fclose(f)
	return m
end

export function writefile(^char filename,^byte data,int size)int =
	filehandle f
	int n

	f:=fopen(filename,"wb")
	if f=nil then
		return 0
	end

	n:=writerandom(f,data,0,size)
	fclose(f)
	return n
end

export function checkfile(^char file)int=
	filehandle f
	if f:=fopen(file,"rb") then
		fclose(f)
		return 1
	end
	return 0
end

export proc readlinen(filehandle handlex,^char buffer,int size) =
!size>2
	int ch
	^char p
	int n
	byte crseen

	if handlex=nil then
		handlex:=filehandle(os_getstdin())
	end
	if handlex=nil then
		n:=0
		p:=buffer
		do
			ch:=getchar()
			if ch=13 or ch=10 or ch=-1 then
				p^:=0
				return
			end
			p++^:=ch
			++n
			if n>=(size-2) then
				p^:=0
				return
			end
		end
	end

	buffer^:=0
	if fgets(buffer,size-2,handlex)=nil then
		return
	end

	n:=strlen(buffer)
	if n=0 then
		return
	end

	p:=buffer+n-1		!point to last char
	crseen:=0
	while (p>=buffer and (p^=13 or p^=10)) do
		if p^=13 or p^=10 then crseen:=1 end
		p--^ :=0
	end

!NOTE: this check doesn't work when a line simply doesn't end with cr-lf

	if not crseen and (n+4>size) then
		cpl size,n
		abortprogram("line too long")
	end
end

export proc iconvlcn(^char s,int n) =
	to n do
		s^:=tolower(s^)
		++s
	end
end

export proc iconvucn(^char s,int n) =
	to n do
		s^:=toupper(s^)
		++s
	end
end

export function convlcstring(^char s)ichar s0=
	s0:=s
	while (s^) do
		s^:=tolower(s^)
		++s
	end
	s0
end

export function convucstring(^char s)ichar s0=
	s0:=s
	while (s^) do
		s^:=toupper(s^)
		++s
	end
	s0
end

export function changeext(^char s,newext)ichar=
!whether filespec has an extension or not, change it to newext
!newext should start with "."
!return new string (locally stored static string, so must be used before calling again)
	static [260]char newfile
	[32]char newext2
	^char sext
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
	end case


	sext:=extractext(s,1)			!include "." when it is only extension

	case sext^
	when 0 then						!no extension not even "."
		strcat(&newfile[1],&newext2[1])
	when '.' then						!no extension not even "."
		strcat(&newfile[1],&newext2[2])
	else							!has extension
		n:=sext-s-2			!n is number of chars before the "."
		strcpy(&newfile[1]+n+1,&newext2[1])
	end case

	return &newfile[1]
end

export function extractext(^char s,int period=0)ichar=
!if filespec s has an extension, then return pointer to it otherwise return ""
!if s ends with ".", then returns "."
	^char t,u

	t:=extractfile(s)

	if t^=0 then			!s contains no filename
		return ""
	end

!t contains filename+ext
	u:=t+strlen(t)-1		!u points to last char of t

	while u>=t do
		if u^='.' then		!start extension found
			if (u+1)^=0 then		!null extension
				return (period|"."|"")
			end
			return u+1			!return last part of filename as extension exclude the dot
		end
		--u
	end
	return ""			!no extension seen
end

export function extractpath(^char s)ichar=
	static [0:260]char str
	^char t
	int n

	t:=s+strlen(s)-1		!t points to last char

	while (t>=s) do
		case t^
		when '\\','/',':' then		!path separator or drive letter terminator assume no extension
			n:=t-s+1			!n is number of chars in path, which includes rightmost / or \ or :
			memcpy(str,s,n)
			str[n]:=0
			return str
		end case
		--t
	end
	return ""			!no path found
end

export function extractfile(^char s)ichar=
	^char t

	t:=extractpath(s)

	if t^=0 then			!s contains no path
		return s
	end

	return s+strlen(t)		!point to last part of s that contains the file
	end

export function extractbasefile(^char s)ichar=
	static [0:100]char str
	^char f,e
	int n,flen

	f:=extractfile(s)
	flen:=strlen(f)
	if flen=0 then		!s contains no path
		return ""
	end
	e:=extractext(f,0)

	if e^ then			!not null extension
		n:=flen-strlen(e)-1
		memcpy(&str,f,n)
		str[n]:=0
		return str
	end
	if (f+flen-1)^='.' then
		memcpy(&str,f,flen-1)
		str[flen-1]:=0
		return str
	end
	return f
end

export function addext(^char s,^char newext)ichar=
!when filespec has no extension of its own, add newext
	^char sext

	sext:=extractext(s,1)

	if sext^=0 then						!no extension not even "."
		return changeext(s,newext)
	end

	return s							!has own extension; use that
end

export function pcm_alloc32:^void =
	^byte p

	allocbytes:=32
!	smallmemtotal+:=32

	if p:=^byte(freelist[2]) then		!Items of this block size available
		freelist[2]:=^word(int((freelist[2])^))
		return p
	end

!No items in freelists: allocate new space in this heap block
	return pcm_alloc(32)
end

export proc pcm_free32(^void p) =
!n can be the actual size requested it does not need to be the allocated size

!	smallmemtotal-:=32

	cast(p,^word)^:=word(int(freelist[2]))
	freelist[2]:=p
end

export proc outbyte(filehandle f,int x)=
	fwrite(&x,1,1,f)
end

export proc outu16(filehandle f,word x)=
	fwrite(&x,2,1,f)
end

export proc outu32(filehandle f,word x)=
	fwrite(&x,4,1,f)
end

export proc outu64(filehandle f,u64 x)=
	fwrite(&x,8,1,f)
end

export proc outstring(filehandle f, ichar s)=
	fwrite(s,strlen(s)+1,1,f)
end

export proc outblock(filehandle f, ^void p, int n)=
	fwrite(p,n,1,f)
end

export function myeof(filehandle f)int=
	int c

	c:=fgetc(f)
	if c=c_eof then return 1 end
	ungetc(c,f)
	return 0
end

export proc strbuffer_add(^strbuffer dest, ichar s, int n=-1)=
	int newlen,oldlen
	ichar newptr

!	IF N=0 THEN CPL "N=0" FI

	if n=-1 then
		n:=strlen(s)
	end

	oldlen:=dest.length

	if oldlen=0 then				!first string
		dest.strptr:=pcm_alloc(n+1)
		dest.allocated:=allocbytes
		dest.length:=n				!length always excludes terminator
		memcpy(dest.strptr,s,n)
		(dest.strptr+n)^:=0
		return
	end

	newlen:=oldlen+n
	if newlen+1>dest.allocated then
		newptr:=pcm_alloc(newlen+1)
		memcpy(newptr,dest.strptr,oldlen)
		dest.strptr:=newptr
		dest.allocated:=allocbytes
	end

	memcpy(dest.strptr+oldlen,s,n)
	(dest.strptr+newlen)^:=0

	dest.length:=newlen
end

export proc gs_init(^strbuffer dest)=
	pcm_clearmem(dest,strbuffer.bytes)
end

export proc gs_free(^strbuffer dest)=
	if dest.allocated then
		pcm_free(dest.strptr,dest.allocated)
	end
end

export proc gs_str(^strbuffer dest,ichar s)=
	strbuffer_add(dest,s)
end

export proc gs_char(^strbuffer dest,int c)=
	[16]char s

	s[1]:=c
	s[2]:=0

	strbuffer_add(dest,s,1)
end

export proc gs_strn(^strbuffer dest,ichar s,int length)=
	strbuffer_add(dest,s,length)
end

export proc gs_strvar(^strbuffer dest,s)=
	strbuffer_add(dest,s.strptr)
end

export proc gs_strint(^strbuffer dest,i64 a)=
	strbuffer_add(dest,strint(a))
end

export proc gs_strln(^strbuffer dest,ichar s)=
	gs_str(dest,s)
	gs_line(dest)
end

export proc gs_strsp(^strbuffer dest,ichar s)=
	gs_str(dest,s)
	gs_str(dest," ")
end

export proc gs_line(^strbuffer dest)=
!	strbuffer_add(dest,"\w")
	strbuffer_add(dest,"\n")
end

export function gs_getcol(^strbuffer dest)int=
	return dest.length
end

export proc gs_leftstr(^strbuffer dest, ichar s, int w, padch=' ')=
	int col,i,n,slen
	[2560]char str
	col:=dest.length
	strcpy(str,s)
	slen:=strlen(s)
	n:=w-slen
	if n>0 then
		for i:=1 to n do
			str[slen+i]:=padch
		end
		str[slen+n+1]:=0
	end
	gs_str(dest,str)
end

export proc gs_leftint(^strbuffer dest, int a, int w, padch=' ')=
	gs_leftstr(dest,strint(a),w,padch)
end

export proc gs_padto(^strbuffer dest,int col, ch=' ')=
	int n
	[2560]char str

	n:=col-dest.length
	if n<=0 then return end
	for i:=1 to n do
		str[i]:=ch
	end
	str[n+1]:=0
	gs_str(dest,str)
end

export proc gs_println(^strbuffer dest,filehandle f=nil)=
	if dest.length=0 then return end
	(dest.strptr+dest.length)^:=0

	if f=nil then
		println dest.strptr,,"\c"
	else
		println @f,dest.strptr,,"\c"
	end
end

export function nextcmdparamnew(int &paramno, ichar &name, &value, ichar defext=nil)int=
	static int infile=0
	static ichar filestart=nil
	static ichar fileptr=nil
	static byte colonseen=0
	^char q
	ichar item,fileext
	int length
	static [300]char str

	reenter:
	value:=nil
	name:=nil

	if infile then
		if readnextfileitem(fileptr,item)=0 then		!eof
			free(filestart)								!file allocated via malloc
			infile:=0
			goto reenter
		end
	else
		if paramno>ncmdparams then
			return pm_end
		end
		item:=cmdparams[paramno]
		++paramno

		length:=strlen(item)

		if item^='@' then		!@ file
			filestart:=fileptr:=readfile(item+1)
			if filestart=nil then
				println "Can't open",item
				stop 7
			end
			infile:=1
			goto reenter
		end

		if item^=':' then
			colonseen:=1
			return pm_colon
		end
	end

	value:=nil
	if item^='-' then
		name:=item+(colonseen|0|1)
		q:=strchr(item,':')
		if not q then
			q:=strchr(item,'=')
		end
		if q then
			value:=q+1
			q^:=0
		end
		return (colonseen|pm_extra|pm_option)
	end

	fileext:=extractext(item,0)
	name:=item

	if fileext^=0 then							!no extension
		strcpy(str,name)
		if defext and not colonseen then
			name:=addext(str,defext)				!try .c
		end
!	elsif eqstring(fileext,"dll") then
	elsif eqstring(fileext,"dll") or eqstring(fileext,"mcx") then
		return (colonseen|pm_extra|pm_libfile)
	end
	return (colonseen|pm_extra|pm_sourcefile)
end

function readnextfileitem(ichar &fileptr,&item)int=
	^char p,pstart,pend
	int n
	static [256]char str

	p:=fileptr

	reenter:
	do
		case p^
		when ' ','\t',13,10 then	!skip white space
			++p
		when 26,0 then				!eof
			return 0
		else
			exit
		end case
	end

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

		end docase
	end case


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
				if p^=',' then ++p end
				exit
			end case
			++p
		end
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
			end case
			++p
		end
	end case

	n:=pend-pstart
	if n>=str.len then
		println "@file item too long"
		stop 9
	end
	memcpy(str,pstart,n)
	str[n+1]:=0
	item:=str
	fileptr:=p

	return 1
end

export proc ipadstr(^char s,int width,^char padchar=" ")=
	int n

	n:=strlen(s)
	to width-n do
		strcat(s,padchar)
	end
end

export function padstr(^char s,int width,^char padchar=" ")ichar=
	static [256]char str

	strcpy(str,s)
	ipadstr(str,width,padchar)
	return str
end

export function chr(int c)ichar=
	static [8]char str

	str[1]:=c
	str[2]:=0
	return str
end

export function cmpstring(ichar s,t)int=
	int res
	if (res:=strcmp(s,t))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	end
end

export function cmpstringn(ichar s,t,int n)int=
	int res
	if (res:=strncmp(s,t,n))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	end
end

export function eqstring(ichar s,t)int=
	return strcmp(s,t)=0
end

export function cmpbytes(^void p,q,int n)int=
	int res
	if (res:=memcmp(p,q,n))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	end
end

export function eqbytes(^void p,q,int n)int=
	return memcmp(p,q,n)=0
end

export proc mseed(u64 a,b=0)=
	seed[1]:=a
	if b then
		seed[2]:=b
	else
		seed[2] ixor:=a
	end
end

export function mrandom:word =
!return pure 64-bit word value, 0 to 2**64-1
!(cast result for signed value)
!	u64 x,y
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
	end
	return (mrandomp() rem span)+a
end

export function mrandomreal:real x=
!positive random real value from 0 to just under (but not including) 1.0
	repeat x:=mrandomp()/9223372036854775808.0 until x<>1.0
	return x
end

export function mrandomreal1:real=
!positive random real value from 0 to 1.0 inclusive
	return mrandomp()/9223372036854775807.0
end

export function readline:ichar=
	readln
	return rd_buffer
end

export function findfunction(ichar name)^void=
	for i to $getnprocs() do
		if eqstring($getprocname(i),name) then
			return $getprocaddr(i)
		end
	end
	return nil
end

export function roundtoblock(int n,align)int=
!round up n until it is a multiple of filealign (which is a power of two)
!return aligned value. Returns original if already aligned
	if n iand (align-1)=0 then return n end
	return n+(align-(n iand (align-1)))
end

export function pcm_allocnfz(int n)^void =
!non-freeing allocator for small objects
!n should be a multiple of 8 bytes, but is rounded up here if needed
	^byte p

!make n a multiple of 8
	if n iand 7 then
		n:=n+(8-(n iand 7))
	end

	p:=pcheapptr					!Create item at start of remaining pool in heap block
	pcheapptr+:=n					!Shrink remaining pool

	if pcheapptr>=pcheapend then	!Overflows?
		p:=pcm_newblock(n)			!Create new heap block, and allocate from start of that
	end

	return p
end

!export proc freddy=
!	PRINTLN "FREDDY"
!end
=== mlinux.m 0 0 28/74 ===
!import clib
!import mlib
!
!importlib cstd=
!	clang proc     sleep	(word32)
!end

record termios =
	i32 c_iflag
	i32 c_oflag
	i32 c_cflag
	i32 c_lflag
	char c_line
	[32]char c_cc				!at offset 17
	[3]byte filler
	i32 c_ispeed				!at offset 52
	i32 c_ospeed
end

!importdll dlstuff=
importdll msvcrt=
	func dlopen			(ichar, i32)ref void
	func dlsym			(ref void, ichar)ref void
	func tcgetattr		(i32, ref termios) i32
	func tcsetattr		(i32, i32, ref termios) i32
	func gettimeofday	(ref timeval, ref void) i32
	func gmtime_r  	   (ref i64, ref tm_rec) ref void
	proc stdin
	proc stdout
end
 
record timeval =
	i64 tv_sec
	i64 tv_usec
end

record tm_rec =
	i32 tm_sec
	i32 tm_min
	i32 tm_hour
	i32 tm_mday

	i32 tm_mon
	i32 tm_year
	i32 tm_wday
	i32 tm_yday
	i32 tm_isdst
	[20]byte padding
end

!this record is used by some apps, so these fields must be present
export record rsystemtime =
	i32 year
	i32 month
	i32 dayofweek
	i32 day
	i32 hour
	i32 minute
	i32 second
	int milliseconds
end

int init_flag=0


export proc os_init=
	init_flag:=1
end

export func os_execwait(ichar cmdline,int newconsole=0,ichar workdir=nil)int =
	return system(cmdline)
end

export func os_execcmd(ichar cmdline, int newconsole)int =
	return system(cmdline)
end

export func os_getch:int=
	const ICANON  = 2
	const ECHO    = 8
	const TCSANOW = 0
	const ISIG    = 1

	termios old,new
	char ch

	tcgetattr(0,&old)
	new:=old
	new.c_lflag iand:=inot ICANON
	new.c_lflag iand:=inot ECHO
	new.c_lflag iand:=inot ISIG

	tcsetattr(0,TCSANOW,&new)

	ch:=getchar()

	tcsetattr(0,TCSANOW,&old)

	return ch
end

export func os_kbhit:int=
	abortprogram("kbhit")
	return 0
end

export proc os_flushkeys=
	abortprogram("flushkeys")
end

export func os_getconsolein:ref void=
	return nil
end

export func os_getconsoleout:ref void=
	return nil
end

export func os_proginstance:ref void=
	abortprogram("PROGINST")
	return nil
end

export func os_getdllinst(ichar name)u64=
	const RTLD_LAZY=1
	ref void h

	h:=dlopen(name,RTLD_LAZY)

	if h=nil then
		if strcmp(name,"msvcrt")=0 then			!might be linux
			h:=dlopen("libc.so.6",RTLD_LAZY);
		fi
	fi

	return cast(h)
end

export func os_getdllprocaddr(int hlib,ichar name)ref void=
	ref void fnaddr

	if hlib=0 then
		return nil
	fi

	fnaddr:=dlsym(cast(int(hlib)), name)
	return fnaddr
end

export proc os_initwindows=
end

export func os_getchx:int=
	abortprogram("getchx")
	return 0
end

export func os_getos=>ichar=
!	if $targetbits=32 then
!		return "L32"
!	else
		return "L64"
!	fi
end

export func os_gethostsize=>int=
	return 64
end

export func os_iswindows:int=
	return 0
end

export func os_shellexec(ichar opc, file)int=
	abortprogram("SHELL EXEC")
	return 0
end

export proc  os_sleep(int a)=
!*!	sleep(a)
end

export func os_getstdin:filehandle =
	ref filehandle pf:=cast(stdin)
	return pf^
end

export func os_getstdout:filehandle =
	ref filehandle pf:=cast(stdout)
	return pf^
end

export func os_gethostname:ichar=
!	abortprogram("gethostname")
	return ""
end

export func os_getmpath:ichar=
!	abortprogram("getmpath")
	return ""
end

export proc os_exitprocess(int x)=
	stop
!	_exit(0)
!	ExitProcess(x)
end

export func os_clock:i64=
	if os_iswindows() then
		return clock()
	else
		return clock()/1000
	fi
end

export func os_ticks:i64=
	return clock()
end

export func os_getclockspersec:i64=
	return (os_iswindows()|1000|1000'000)
end

export proc os_setmesshandler(ref void addr)=
	abortprogram("SETMESSHANDLER")
!	wndproc_callbackfn:=addr
end

export func os_hpcounter:i64=
	return 1
end

export func os_hpfrequency:i64=
	return 1
end

export func os_filelastwritetime(ichar filename)i64=
	return 0
end

export proc os_getsystime(ref rsystemtime tm)=
	timeval tv
	tm_rec tmr


	gettimeofday(&tv, nil)
	gmtime_r(&tv.tv_sec, &tmr)

	tm.year := tmr.tm_year + 1900
	tm.month := tmr.tm_mon + 1
	tm.dayofweek := tmr.tm_wday + 1
	tm.day := tmr.tm_mday
	tm.hour := tmr.tm_hour
	tm.minute := tmr.tm_min
	tm.second := tmr.tm_sec
	tm.milliseconds := tv.tv_usec/1000
tm.month:=1			!avoid crashing the M compiler
end

export proc os_peek=
end

export func  os_allocexecmem(int n)ref byte=
	abortprogram("No allocexec")
	nil
end

export func dirlist(ichar filespec, ref[]ichar dest, int capacity, t=1)int=
	0
end
=== mm.m 0 0 29/74 ===
!project =

	module mm_decls

	module mm_cli				! Command line interface

	module mm_lex				! Lexer, produces tokens
	module mm_parse				! Parser, produces ST, TT and AST1
	module mm_name				! Name resolution, AST1 to AST2
	module mm_type				! Type analysis, AST2 to AST3

	module mm_diags				! diagnostics
!	module mm_diags_dummy
!
!	module mm_export_dummy		! Write exports files

!Embedded SYSLIB sources
	module mm_libsources		!Embedded Syslib sources
!	module mm_libsources_dummy
!
	module mm_modules			! Module handling

	module mm_support			! Source file handling

	module mm_tables			! Enumerations
	module mm_lib				! Support functions

!AST to PCL IL
	module mm_genpcl			! ST/AST -> PST/PCL
	module mm_blockpcl			! AST -> PCL

!PCL Support Library

	module pcl_decls			! Data structures, vars, enums and tables
	module pcl_api				! PCL-generating API and support routines
	module pcl_diags			! Dump PCL code by function
!!
!!!!PCL Interpreter
!	module pcl_run				! Fixups and dispatch loop
!
!
!!PCL->MCL Inbetween modules
	module mcl_decls			! Data structures, vars, enums, and tables
	module mcl_gen				! ST/PCL to MCL AND some support
	module mcl_genaux			! Other support routines
	module mcl_stack			! PCL stack management (and some diags)
	module mcl_block			! PCL to MCL per functions
	module mcl_blockaux			! Support routines for PCL to MCL
!
!!MCL Support Library
	module mcx_decls			! Data structures, vars, enums, and tables
	module mcx_asm				! Dump generated MCL as ASM source code

	module mcx_lib				! Low-level MCL-generating API and other support

	module mcx_pedecls			! For PE format + some extra records
	module mcx_optim			! Peephole MCL Optimiser

!	module mcx_gas				! Dump generated MCL as GAS source code
!
	module mcx_genss			! MCL to SS binary code and data
	module mcx_exe				!
!!
!!MCL Run In Memory
	module mcr_decls
	module mcr_run
	module mcr_lib
	module mcr_write




global const pclpresent=1

global proc writeexports(ichar basefile, modulename)=end


!global const fsyslibs=0

!global proc printunit(ref unitrec p,int level=0,ichar prefix="*",filehandle dev=nil)=end
!
!global proc showlogfile=CPL "NO DIAGS MODULE" end
!
!global proc showast(ichar filename)=end
!
!global proc printsymbol(ref tokenrec lp)=end
!
!global proc showtimings=end
!
!global proc loadbuiltins=end
=== mmi.m 0 0 30/74 ===
!project =
	module mm_cli

	module mm_blockpcl

!	module mm_assem
	module mm_assem_dummy
	module mm_assemaux_dummy
	module mm_decls

	module mm_diags_dummy

	module mm_exportm

	module mm_genpcl

	module mm_lex
	module mm_lib

	module mm_libpcl

!	module mm_libsources
	module mm_libsources_dummy

	module mm_modules
	module mm_name
	module mm_parse

	module mm_support
	module mm_tables
	module mm_type

!	$sourcepath "c:/px/"
!	import pcl
	import pclint
!	import pclmin
!	import pclrunx

!end

proc main=
	main2()
end

=== mmp.m 0 0 31/74 ===
!project =

	module mm_decls

	module mm_cli				! Command line interface

	module mm_lex				! Lexer, produces tokens
	module mm_parse				! Parser, produces ST, TT and AST1
	module mm_name				! Name resolution, AST1 to AST2
	module mm_type				! Type analysis, AST2 to AST3

	module mm_diags				! diagnostics
!	module mm_diags_dummy
!
!	module mm_export_dummy		! Write exports files

!Embedded SYSLIB sources
	module mm_libsources		!Embedded Syslib sources
!	module mm_libsources_dummy
!
	module mm_modules			! Module handling

	module mm_support			! Source file handling

	module mm_tables			! Enumerations
	module mm_lib				! Support functions

!AST to PCL IL
	module mm_genpcl			! ST/AST -> PST/PCL
	module mm_blockpcl			! AST -> PCL

!PCL Support Library

	module pcl_decls			! Data structures, vars, enums and tables
	module pcl_api				! PCL-generating API and support routines
	module pcl_diags			! Dump PCL code by function
!!
!!!!PCL Interpreter
!	module pcl_run				! Fixups and dispatch loop
!
!
!!PCL->MCL Inbetween modules
	module mcl_decls			! Data structures, vars, enums, and tables
	module mcl_gen				! ST/PCL to MCL AND some support
	module mcl_genaux			! Other support routines
	module mcl_stack			! PCL stack management (and some diags)
	module mcl_block			! PCL to MCL per functions
	module mcl_blockaux			! Support routines for PCL to MCL
!
!!MCL Support Library
	module mcx_decls			! Data structures, vars, enums, and tables
	module mcx_asm				! Dump generated MCL as ASM source code

	module mcx_lib				! Low-level MCL-generating API and other support

	module mcx_pedecls			! For PE format + some extra records
	module mcx_optim			! Peephole MCL Optimiser

!	module mcx_gas				! Dump generated MCL as GAS source code
!
	module mcx_genss			! MCL to SS binary code and data
	module mcx_exe				!
!!
!!MCL Run In Memory
	module mcr_decls
	module mcr_run
	module mcr_lib
	module mcr_write




global const pclpresent=1

global proc writeexports(ichar basefile, modulename)=end


!global const fsyslibs=0

!global proc printunit(ref unitrec p,int level=0,ichar prefix="*",filehandle dev=nil)=end
!
!global proc showlogfile=CPL "NO DIAGS MODULE" end
!
!global proc showast(ichar filename)=end
!
!global proc printsymbol(ref tokenrec lp)=end
!
!global proc showtimings=end
!
!global proc loadbuiltins=end
=== mm_blockpcl.m 0 0 32/74 ===
!blockpcl

const dodotchains=1
!const dodotchains=0

const maxnestedloops	= 50

const maxparams=100

const maxswitchrange=500
const maxcases=maxswitchrange

const maxcasedepth=20
[maxcasedepth]unit casestmt
[maxcasedepth]int caseelse
int casedepth

int maxreg=0

global macro getmemmode_m(p) = (p.memmode|p.memmode|p.mode)
macro evallv(p) = evalref(p)
macro evalunitx(p, isref) = (isref|evalref(p)|evalunit(p))
macro evalblock(p) = evalunit(p)

global proc evalunit(unit p)=
!p is a single executable unitrec; not a list or const
!should be called via execblock() to executate a code item in a unitrec
	unit a, b, c

	if p=nil then return end

!CPL "EVALUNIT", JTAGNAMES[P.TAG]
	mmpos:=p.pos

	a:=p.a
	b:=p.b
	c:=p.c

	switch p.tag
	when jconst         then do_const(p)
	when jnull          then
	when jname          then do_name(p)
	when jblock then
		do_block(p)
	when jcall          then
		do_callproc(p, a, b)
	when jreturn        then do_return(p, a)
	when jreturnmult    then do_returnmult(p, a)
	when jassign        then do_assign(p, a, b)
	when jassignms      then do_assignms(a, b)
	when jassignmm      then do_assignmm(a, b)
	when jassignmdrem   then do_assignmdrem(a, b)
	when jto            then do_to(p, a, b)
	when jif            then do_if(p, a, b, c, 0)
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
	when jcase          then do_case(p, a, b, c, 0, 0)
	when jdocase        then do_case(p, a, b, c, 1, 0)
	when jswitch, jdoswitch, jdoswitchu, jdoswitchx then
		do_switch(p, a, b, c)
	when jrecase        then do_recase(p, a)
	when jswap          then do_swap(p, a, b)
	when jselect        then do_select(p, a, b, c, 0)
	when jprint, jprintln then
		do_print(p, a, b)
	when jfprint, jfprintln then
		do_print(p, a, b)
	when jread	        then do_read(p, a)
	when jreadln        then do_readln(a)
	when jstop          then do_stop(p, a)
	when jeval          then
		evalunit(a)
		pgen(keval)
		setmode_u(a)

	when jandl          then do_andl(p, a, b)
	when jorl           then do_orl(p, a, b)
	when jcmp           then do_setcc(p, a, b)
	when jcmpchain      then do_setccchain(p, a)

	when jbin           then do_bin(p, a, b)
	when jindex         then do_index(p, a, b)

	when jdotindex      then do_dotindex(p, a, b)
	when jdotslice      then do_dotslice(p, a, b)
	when jdot           then do_dot(p)
	when jptr           then do_ptr(p, a)
	when jaddrof        then evalref(a, b)

!	when jconvert       then do_convert(p, a)
	when jwiden         then do_convert(p, kwiden)
	when jfwiden        then do_convert(p, kfwiden)
	when jfnarrow       then do_convert(p, kfnarrow)
	when jfloat         then do_convert(p, kfloat)
	when jfix           then do_convert(p, kfix)
!	when jtruncate      then do_convert(p, ktruncate)
	when jtruncate      then
		do_convert(p, kwiden)
		pccurr.istrunc:=1			!stop it being eliminated in some contexts

	when jtoboolt       then do_convert(p, ktoboolt)

	when jtypepun       then do_typepun(p, a)
	when jshorten       then do_shorten(p, a)
	when jtypeconst     then do_typeconst(p)

	when junary         then do_unary(p, a)

	when jnotl          then do_notl(p, a)
	when jistruel       then do_istruel(p, a)
	when jisfalsel      then do_isfalsel(p, a)

	when jincr          then
		if p.pclop in [kincrto, kdecrto] then
			do_incr(p, a)
		else
			do_incrload(p, a)
		end
!
	when jbinto         then do_binto(p, a, b)
!
	when junaryto       then do_unaryto(p, a)
!
	when jsyscall then
		do_syscall(p, a)

	when jclear         then do_empty(p, a)

	when jslice then
		do_slice(p, a, b)

	else
		GERROR_S("UNSUPPORTED TAG ", JTAGNAMES[P.TAG])
		return
	end switch

	if p.mode<>tvoid and not p.resultflag then
		case p.tag
		when jassign, jcall, jsyscall then

		else
IF NOT JSOLO[P.TAG] THEN
PRINTUNIT(P)
GERROR(ADDSTR("NOT ALLOWED BY ITSELF:", JTAGNAMES[P.TAG]))
FI

			pgen(kunload)
			setmode_u(p)
		end case
	end
end

proc evalref(unit p, q=nil)=
	unit a, b, c
	a:=p.a
	b:=p.b
	c:=p.c
	mmpos:=p.pos

	case p.tag
	when jname then
		genpushmemaddr_d(p.def)
		setmodeword()
		if q then					!addrof may have optional byte offset
			genpushint(q.value)
			pgenix(kaddpx)
			setmode(tu8)
		end

	when jindex then
		do_indexref(a, b)

	when jdot then
		do_dotref(p)

	when jptr then
		evalunit(p.a)

	else
		case p.tag
		when jif then
			do_if(p, a, b, c, 1)
!		when jselect then
!			do_select(p, a, b, c, 1)
!		when jswitch then
!			do_switch(p, a, b, c, 1)
!		when jcase then
!			do_case(p, a, b, c, 0, 1)
!		elsif ttisblock[p.mode] then
!			evalunit(p)

		else
			PRINTUNIT(P)
			gerror("evalref")
		end case
	end case
end

proc evalarray(unit p)=
	case ttbasetype[p.mode]
	when tslice then
		evalref(p)
		pgen(kiload)

		setmodeword()
	elsif p.mode=trefchar then
		evalunit(p)
	else
		evalref(p)
	end case

end

proc do_block(unit p)=
	unit a:=p.a

	while a do
		evalunit(a)
		a:=a.nextunit
	end
end

proc genjumpcond(int opc, unit p, int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
	unit q, r, s
	int lab2, i

	q:=p.a
	r:=p.b

	case p.tag
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
		end case

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
		end case

	when jnotl, jisfalsel then
		case opc
		when kjumpf then
			genjumpcond(kjumpt, q, lab)
		when kjumpt then
			genjumpcond(kjumpf, q, lab)
		end case

	when jistruel then
		genjumpcond(opc, q, lab)

	when jblock then
		while q and q.nextunit do
			evalunit(q)
			q:=q.nextunit
		end
		genjumpcond(opc, q, lab)

	when jcmp then

		gcomparejump(opc, p.condcode, q, r, lab)

	when jinrange then
		evalunit(q)

		if opc=kjumpt then
			lab2:=createfwdlabel()
			evalunit(r.a)
			pgencond(kjumpcc, lt_cc, pgenlabel(lab2))
			setmode_u(q)
			pccurr.popone:=1
			evalunit(r.b)
			pgencond(kjumpcc, le_cc, pgenlabel(lab))
			setmode_u(q)
			definefwdlabel(lab2)
		else
			evalunit(r.a)
			pgencond(kjumpcc, lt_cc, pgenlabel(lab))
			setmode_u(q)
			pccurr.popone:=1
			evalunit(r.b)
			pgencond(kjumpcc, gt_cc, pgenlabel(lab))
			setmode_u(q)
		end

	when jinset then
		s:=r.a
		if s=nil then
			gerror("empty set")
		end

		if opc=kjumpf then
			lab2:=createfwdlabel()
			evalunit(q)

			while s do
				evalunit(s)
				s:=s.nextunit
				if s then
					pgencond(kjumpcc, eq_cc, pgenlabel(lab2))
					pccurr.popone:=1
				else
					pgencond(kjumpcc, ne_cc, pgenlabel(lab))
				end
				setmode_u(q)
			end
			definefwdlabel(lab2)
		else
			evalunit(q)

			while s, s:=s.nextunit do
				evalunit(s)
				pgencond(kjumpcc, eq_cc, pgenlabel(lab))
				setmode_u(q)
				if s.nextunit then pccurr.popone:=1 end
			end
		end

	when jcmpchain then
		r:=q.nextunit
		i:=1
		evalunit(q)
		if opc=kjumpf then
			while r do
				evalunit(r)
				if r.nextunit then
					pgenxy(kswapstk, 1, 2)

					pgencond(kjumpcc, reversecond_order(reversecond(p.cmpgenop[i])), pgenlabel(lab))

					pccurr.popone:=1
				else
					pgencond(kjumpcc, reversecond(p.cmpgenop[i]), pgenlabel(lab))
				end

				setmode_u(q)
				++i
				q:=r
				r:=r.nextunit
			end
		
		else
			lab2:=createfwdlabel()
			while r do
				evalunit(r)
				if r.nextunit then
					pgenxy(kswapstk, 1, 2)
					pgencond(kjumpcc, reversecond_order(reversecond(p.cmpgenop[i])), pgenlabel(lab2))
					pccurr.popone:=1
				else
					pgencond(kjumpcc, p.cmpgenop[i], pgenlabel(lab))
				end
				setmode_u(q)
				++i
				q:=r
				r:=r.nextunit
			end
			definefwdlabel(lab2)
		end
	else			!other, single expression
		evalunit(p)
		pgen(opc, pgenlabel(lab))
!*!		if ttisblock[p.mode] then gerror("jumpt/f") end
		setmode(p.mode)
	end
end

proc gcomparejump(int jumpopc, int cond, unit lhs, rhs, int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	if jumpopc=kjumpf then			!need to reverse condition
		cond:=reversecond(cond)		!eqop => neop, etc
	end

	evalunit(lhs)
	evalunit(rhs)

	pgencond(kjumpcc, cond, pgenlabel(lab))
	setmode_u(lhs)
end

proc genjumpl(int lab)=
!generate unconditional jump to label
	pgen(kjump, pgenlabel(lab))
end

proc unimpl(ichar mess)=
	gerror_s("Unimplemented: #", mess)
end

proc do_const(unit p) =
	int mode:=p.mode

	if ttisinteger[mode] or mode=tbool then
		genpushint(p.value)
	elsif ttisreal[mode] then
		genpushreal(p.xvalue, getpclmode(mode))

	elsif ttisref[mode] then
		if p.isastring then
			if p.strtype='B' then gerror("1:B-str?") end

			genpushstring(p.svalue, p.slength)
		else
			genpushint(p.value)
		end
	else
		gerror("do_const")
	end
	setmode(mode)
end

proc do_name(unit p)=
	symbol d

	d:=p.def
	case d.nameid
	when procid, dllprocid then
		genpushmemaddr_d(d)
		setmodeword()
	when labelid then
		if d.labelno=0 then
			d.labelno:=++mlabelno
		end
		if p.resultflag then		!get label address
			pgen(kload, pgenlabel(d.labelno))
			setmodeword()
		else
			pgen(kjump, pgenlabel(d.labelno))
			p.mode:=tvoid
			p.resultflag:=0
		end

	when fieldid then
		genpushint(d.offset)
		!setmodeint()

	else
		genpushmem_d(d)
		setmode(d.mode)

	end case
end

proc do_stop(unit p, a) =
	gencallimp("exit", 0, (a|a|pzero))
end

proc do_andl(unit p, a, b) =
	int labfalse, labend

	pgen(kstartmx)

	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpf, a, labfalse)
	genjumpcond(kjumpf, b, labfalse)

	genpushint(1)
	pgen(kresetmx)
	!setmodeint()

	genjumpl(labend)

	definefwdlabel(labfalse)
	genpushint(0)
	pgen(kendmx)
	!setmodeint()

	definefwdlabel(labend)
end

proc do_orl(unit p, a, b) =
	int labtrue, labfalse, labend

	pgen(kstartmx)
	labtrue:=createfwdlabel()
	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpt, a, labtrue)
	genjumpcond(kjumpf, b, labfalse)

	definefwdlabel(labtrue)
	genpushint(1)
	pgen(kresetmx)
	!setmodeint()
	genjumpl(labend)

	definefwdlabel(labfalse)
	genpushint(0)
	pgen(kendmx)
	!setmodeint()

	definefwdlabel(labend)
end

proc do_notl(unit p, a) =
	evalunit(a)
	pgen(p.pclop)
	!setmodeint()
end

proc do_istruel(unit p, a) =
	evalunit(a)
!	if a.mode=tbool then
!		return
!	end
	pgen(ktoboolt)
	setmode_u(p)
	setmode2(a.mode)
end

proc do_isfalsel(unit p, a) =
	evalunit(a)
!	if a.mode=tbool then
!		return
!	end
	pgen(ktoboolf)
	setmode_u(p)
	setmode2(a.mode)
end

proc do_typepun(unit p, a) =
	evalunit(a)

	if a.tag=jname then
		a.def.addrof:=1
	end

!CPL =STRMODE(P.MODE)
!CPL =STRMODE(P.OLDMODE)
!CPL =STRMODE(A.MODE)

	if a.mode=p.mode then return end
	pgen(ktypepun)
	setmode(p.mode)
	setmode2(a.mode)
end

proc do_shorten(unit p, a) =
	evalunit(a)
end

proc do_assign(unit p, a, b) =
!fstore=1 when result is needed

	if a.tag=jname and not a.def.used then
		RETURN
	FI

	case b.tag
	when jmakelist then					!a:=(x, y, z)
		if not p.resultflag then
			do_assignblock(p, a, b)		!(avoids pushing/popping block data)
			return
		end

!	when jslice then					!a:=b[i..j] etc
!		if a.tag=jname then
!			doslice(b, b.a, b.b, a.def, p.resultflag)
!			return
!		end
	end case

	case a.tag
	when jindex then
		do_storeindex(p, a.a, a.b, b)
		return
	when jslice then
		GERROR("ASS/SLICE")

	when jdot then
		do_storedot(a, a.b, b)
		return

	when jdotindex then
		do_storebit(a.a, a.b, b, p.resultflag)
		return
	when jdotslice then
		do_storebf(a.a, a.b.a, a.b.b, b, p.resultflag)
		return
	end case

	evalunit(b)
	if p.resultflag then
		pgen(kdouble)
	end

	case a.tag
	when jname then
		if a.def.nameid in [procid, dllprocid, labelid] then GERROR("Assign to proc?") end
		pgen(kstore, genmem_u(a))

	when jptr then
		evalref(a)
		pgen(kistore)
!		setmode_u(a)

!	when jdotslice then
!		evalref(a.a)
!		evalunit(a.b.a)
!		evalunit(a.b.b)
!		pgen(kstorebf)
!		setmode_u(a.a)
!		return
!
	when jif then
		do_if(a, a.a, a.b, a.c, 1)
		pgen(kistore)
!		setmode_u(a)

	else
		cpl jtagnames[a.tag]
		gerror("Can't assign")
	end case

	setmode_u(a)
end

proc do_bin(unit p, a, b) =
	int size, n

	evalunit(a)

	if p.pclop=ksubpx then
		size:=ttsize[tttarget[a.mode]]
		if b.tag=jconst then
			genpushint(b.value*size)
		else
			evalunit(b)
			genpushint(size)
			pgen(kmul)
		fi
		pgen(ksub)
		finish
	end

	evalunit(b)

	case p.pclop
	when kaddpx then
		pgenix(p.pclop, ttsize[tttarget[a.mode]])
	when ksub then
		pgen(p.pclop)
		if ttbasetype[a.mode]=tref then
			size:=ttsize[tttarget[a.mode]]
			if size>1 then
				n:=ispoweroftwo(size)
				if n then
					genpushint(n)
					pgen(kshr)
					setmode(tu64)
					return
				else
					merror("sub/ref not power of two")
				fi
			end
		fi
	else
		pgen(p.pclop)
		case p.pclop
		when kmaths2 then
			pccurr.mathsop:=p.mathsop
		end case
	end
finish:
	setmode_u(p)
end

proc do_setcc(unit p, a, b) =
	evalunit(a)
	evalunit(b)
	pgencond(ksetcc, p.condcode)
	setmode_u(a)
end

proc do_setccchain(unit p, q) =
	int lab1, lab2, i, cond
	unit r

	lab1:=createfwdlabel()
	lab2:=createfwdlabel()

	r:=q.nextunit
	i:=1

	pgen(kstartmx)

	evalunit(q)
	while r do
		evalunit(r)
		cond:=reversecond(p.cmpgenop[i])
		if r.nextunit then
			pgenxy(kswapstk, 1, 2)
			cond:=reversecond_order(cond)
		end

		pgencond(kjumpcc, cond, pgenlabel(lab1))
		if r.nextunit then pccurr.popone:=1 end

		setmode_u(q)
		++i
		q:=r
		r:=r.nextunit
	end

	genpushint(1)
	pgen(kresetmx)
	!setmodeint()
	pgen(kjump, pgenlabel(lab2))

	definefwdlabel(lab1)
	genpushint(0)
	pgen(kendmx)
	!setmodeint()
	definefwdlabel(lab2)
end

proc do_binto(unit p, a, b)=
	int scale
!CPL "BINTO", PCLNAMES[P.PCLOP]

	case p.pclop
	when kaddpx, ksubpx then
		loadlhs(a)
		do_setinplace()

		scale:=ttsize[tttarget[a.mode]]

		if scale>1 and b.tag=jconst then
			genpushint(scale*b.value)
		else
			evalunit(b)
			if scale>1 then
				genpushint(scale)
				pgen(kmul)
			end
		end

		pgen((p.pclop=kaddpx|kadd|ksub))
		setmode_u(a)

		storelhs(a)


	else
		loadlhs(a)
	do_setinplace()
		evalunit(b)

		pgen(p.pclop)
		setmode_u(a)

		storelhs(a)
	end
end

proc do_unary(unit p, a) =
	int adj
!
	if ttbasetype[a.mode]=tslice then
		evalref(a)
		case p.propcode
		when kklen, kkupb then
			genpushint(8)
			pgenix(kiloadx, 1, 0)
			!setmodeint()
			if p.propcode=kkupb then
				adj:=ttlower[a.mode]-1
				if adj then
					genpushint(adj)
					pgen(kadd)
					!setmodeint()
				end
			end

		when kksliceptr then
			pgen(kiload)
!			pgen(kload, pgenint(0))
!			genpushint(0)
!			pgenix(kiloadx)
			setmodeword()

		end case

		return
	end

	case p.pclop
	when ksqr then
		evalunit(a)
		pgen(kdupl)
		pgen(kmul)
	when ksign then
		gencallsysfn((pstdint[ttbasetype[a.mode]]|sf_sign_i64|sf_sign_r64), a)

	else
		evalunit(a)
		pgen(p.pclop)

		if p.pclop=kmaths then
			pccurr.mathsop:=p.mathsop
		end
	end

	setmode_u(a)
end

proc do_unaryto(unit p, a)=
	loadlhs(a)
	do_setinplace()
	pgen(p.pclop)
	setmode_u(a)
	storelhs(a)
end

proc do_ptr(unit p, a)=
	evalunit(a)
	pgen(kiload)
	setmode_u(p)
end

proc do_labeldef(unit p)=
	symbol d
	[256]char str

	d:=p.def
	if d.labelno=0 then
		d.labelno:=++mlabelno
	end

	strcpy(str, d.name)
	strcat(str, ":")
	pcomment(str)

!PCOMMENT("LABDEF")
!	pgen(klabeldef, genmemaddr_d(d))

	pgen(klabel, pgenlabel(d.labelno))
end

proc do_goto(unit a)=
	symbol d

	if a.tag=jname and a.def.nameid=labelid then
		d:=a.def
		if d.labelno=0 then
			d.labelno:=++mlabelno
		end
		pgen(kjump, pgenlabel(d.labelno))
	else
		evalunit(a)
		pgen(kijump)
	end
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
	unit cvar
	int lab_b, lab_c, lab_d, count

	cvar:=p.c

	a.mode:=ti64

	evalunit(a)
	pgen(kstore, genmem_u(cvar))
	!setmodeint()

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	stacklooplabels(lab_b, lab_c, lab_d)

!check for count being nonzero
	if a.tag<>jconst then			!assume const limit is non-zero
		evalunit(cvar)
		evalunit(pzero)

		pgencond(kjumpcc, le_cc, pgenlabel(lab_d))
		!setmodeint()

	else
		count:=a.value
		if count<=0 then
			genjumpl(lab_d)
		end
	end

	definefwdlabel(lab_b)
	evalblock(b)			!main body

	definefwdlabel(lab_c)

	pgen(kload, genmem_u(cvar))
!	pgen(kload, pgenint(1))
	genpushint(1)
	pgen(ksub)
	pgen(kdouble, genmem_u(cvar))
	pgen(kstore, genmem_u(cvar))
	pgen(kjumpt, pgenlabel(lab_b))

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
	end

	stacklooplabels(lab_b, lab_c, lab_d)

	genjumpl(lab_incr)		!direct to condition code which is at the end

	definefwdlabel(lab_b)

	evalblock(pbody)

	definefwdlabel(lab_c)

	if pincr then
		evalblock(pincr)
		definefwdlabel(lab_incr)
	end

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

	index:=p.loopindex
	if index=0 then index:=loopindex end

	n:=findlooplabel(k, index)
	if n=0 then
		gerror("Bad exit/loop index", p)
	else
		genjumpl(n)
	end
end

proc do_if(unit p, pcond, plist, pelse, int isref) =
	int labend, i, lab2, ismult

	labend:=createfwdlabel()
	ismult:=p.mode<>tvoid

	i:=0
	if ismult then pgen(kstartmx) end

	while pcond, (pcond:=pcond.nextunit; plist:=plist.nextunit) do
		++i
		lab2:=createfwdlabel()

		docond(kjumpf, pcond, lab2)

		evalunitx(plist, isref)
		if ismult then pgen(kresetmx); setmode_u(p) end

		if pcond.nextunit or pelse then
			genjumpl(labend)
		end
		definefwdlabel(lab2)
	end

	if pelse then
		evalunitx(pelse, isref)
		if ismult then pgen(kendmx); setmode_u(p) end
	end
	definefwdlabel(labend)
end

proc do_return(unit p, a) =
	if a then
		evalunit(a)

		pgen(kjumpret, pgenlabel(retindex))
		setmode_u(a)
	else
		genjumpl(retindex)
	end
end

proc do_returnmult(unit p, a) =
	[maxparams]unit params
	unit q
	int nparams

	q:=a
	nparams:=0
	while q do
		if nparams>=maxparams then gerror("Mult?") end
		params[++nparams]:=q
		q:=q.nextunit
	end

	for i:=nparams downto 1 do
		evalunit(params[i])
	end

!need individual setret codes (not sure about the order)
	pgen(kjumpretm, pgenlabel(retindex))
	psetnargs(nparams)
	p.resultflag:=1
end

proc do_callproc(unit p, a, b) =
	[maxparams]unit paramlist
	[maxparams]i8 argattr
	int nparams, isptr, nvariadics, nret, isfn
	int iparams, fparams
	int nfixedparams
	symbol d, e
	^[]i32 pmult
	unit q

	isptr:=0
!	isfn:=p.tag=jcallfn

	case a.tag
	when jname then
		d:=a.def

	when jptr then
		d:=ttnamedef[a.mode]
		isptr:=1
	else
		gerror("call/not ptr")
	end case

	isfn:=d.mode<>tvoid

	nparams:=0
	nvariadics:=0

	q:=b
	nfixedparams:=0
	e:=d.deflist
	while e, e:=e.nextdef do
		if e.nameid=paramid then ++nfixedparams end
	end

	q:=b
	while q, q:=q.nextunit do
		if nparams>=maxparams then gerror("maxparams") end
		paramlist[++nparams]:=q

		if d.varparams and nparams>=nfixedparams and nparams<=4 and nvariadics=0 then
			nvariadics:=nparams
		end
	end

	pgen(ksetcall)
	setmode_u(p)
	pccurr.nargs:=nparams

	iparams:=fparams:=0

	for i to nparams do
		q:=paramlist[i]
		argattr[i]:=0
		if q.mode in [tr32, tr64] then
			if ++fparams>8 then argattr[i]:=2 end
		else
			if ++iparams>8 then argattr[i]:=2 end
		end
	end

	if fparams>8 and iparams>8 then
		gerror("Mixed stack args")
	end
	iparams:=max(fparams, iparams)-8		!no. of stack args

	for i:=nparams downto 1 do
		if iparams.odd and argattr[i] then
			argattr[i]:=1					!change first/rightmost '2' to '1'
			iparams:=0
		end
		q:=paramlist[i]
		evalunit(q)

		if nvariadics and i>=nvariadics and pccurr.mode=tr32 then
			pgen(kfwiden)
			pccurr.size:=8
			pccurr.mode:=tr64
			pccurr.mode2:=tr32

			pgen(ksetarg)
			setmode(tr64)
		else
			pgen(ksetarg)
			setmode_u(q)
		end

		pccurr.x:=i
		pccurr.y:=argattr[i]
	end

	if not isptr then
		pgen((isfn|kcallf|kcallp), genmemaddr_d(d))
	else
		evalunit(a.a)
		pgen((isfn|kicallf|kicallp))
	end

	pccurr.nargs:=nparams
    pccurr.nvariadics:=nvariadics

	if isfn then
		setmode_u(p)
	end

	if d.nretvalues>1 and isfn then
		nret:=d.nretvalues
		pmult:=ttmult[d.mode]

		for i to nret do
			pgen(ktype)
			setmode(pmult[i])
		end
	end

	if isfn and not p.resultflag then
		pgen(kunload)
		setmode_u(p)
	end

end

proc do_print(unit p, a, b) =
	unit q, r, fmt
	int m, fn, needprintend

	if a then
		needprintend:=1
		if ttbasetype[a.mode]<>tref then gerror("@dev no ref") end
		case ttbasetype[tttarget[a.mode]]
		when tvoid then
			gencallsysproc(sf_print_startfile, a)
		when tc8 then
			gencallsysproc(sf_print_startstr, a)
		when tref then
			gencallsysproc(sf_print_startptr, a)
		else
			gerror("@dev?")
		end case
	else
		needprintend:=1
		gencallsysproc(sf_print_startcon)
	end

	q:=b

	case p.tag
	when jfprint, jfprintln then
		if ttbasetype[q.mode]<>tref or ttbasetype[tttarget[q.mode]]<>tc8 then
			gerror("string expected")
		end
		gencallsysproc(sf_print_setfmt, q)
		q:=p.c
	end case

	while q do
		case q.tag
		when jfmtitem then
			fmt:=q.b
			r:=q.a
			m:=r.mode
		when jnogap then
			gencallsysproc(sf_print_nogap)
			q:=q.nextunit
			nextloop
		when jspace then
			gencallsysproc(sf_print_space)
			q:=q.nextunit
			nextloop
		else
			fmt:=nil
			r:=q
			m:=q.mode
		end case

		case ttbasetype[m]
		when ti64 then
			fn:=sf_print_i64
			if not fmt then fn:=sf_print_i64_nf end
		when tu64 then
			fn:=sf_print_u64
		when tr32 then
			fn:=sf_print_r32
		when tr64 then
			fn:=sf_print_r64
		when tref then
			if tttarget[m]=tc8 or tttarget[m]=tarray and tttarget[tttarget[m]]=tc8 then
				fn:=sf_print_str
				if not fmt then fn:=sf_print_str_nf end
			else
				fn:=sf_print_ptr
				if not fmt then fn:=sf_print_ptr_nf end
			end
		when tbool then
			fn:=sf_print_bool
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
			end

		when tc64 then
			fn:=sf_print_c8

		else
			PRINTLN STRMODE(M), STRMODE(TTBASETYPE[M])
			gerror_s("PRINT/T=#", strmode(m))
		end case

		case fn
		when sf_print_i64_nf, sf_print_str_nf, sf_print_ptr_nf then
			gencallsysproc(fn, r)
		else
			gencallsysproc(fn, r, (fmt|fmt|pzero))
		end case

		q:=q.nextunit
	end

	case p.tag
	when jprintln, jfprintln then
		gencallsysproc(sf_print_newline)
	end case
	if needprintend then
		gencallsysproc(sf_print_end)
	end
end

proc do_incr(unit p, a) =

	loadlhs(a)

	if ttisref[a.mode] then
		genpushint(ttsize[tttarget[a.mode]])
	else
		genpushint(1)
	fi

	pgen((p.pclop=kincrto|kadd|ksub))

	storelhs(a)
end

proc setincrstep(int m)=
	pccurr.stepx:=1

	if ttisref[m] then
		pccurr.stepx:=ttsize[tttarget[m]]
	end
end

proc do_incrload(unit p, a) =
	evallv(a)
	do_setinplace()
	pgen(p.pclop)
	setmode_u(a)
	setincrstep(a.mode)
end

proc do_for(unit p, pindex, pfrom, pbody, int down) =
!Structure:
!	Forup/to
!		pindex -> [ptoinit]
!		pfrom -> pto -> [pstep]
!		pbody -> [pelse]
!When pto is complex, then pto refers to an AV variable, and ptoinit contains code
!to evaluate the complex pto, and assign it to that AV

	unit pto, pstep, pelse, px, ptoinit
	int lab_b, lab_c, lab_d, lab_e
	int a, b, stepx

	pto:=pfrom.nextunit
	pstep:=pto.nextunit
	pelse:=pbody.nextunit
	ptoinit:=pindex.nextunit

	if pto.tag=jptr then
		px:=pto.a
		symbol d
		if px.tag=jname and (d:=px.def).nameid=paramid and
			 d.byref then
			gerror("Possibly using &param as for-loop limit")
		end
	end

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pelse then
		lab_e:=createfwdlabel()
	else
		lab_e:=lab_d
	end

	stacklooplabels(lab_b, lab_c, lab_d)

!now start generating code
	evalunit(pfrom)
	pgen(kstore, genmem_u(pindex))
	setmode_u(pindex)

	if ptoinit then			!using temp for limit
		ptoinit.resultflag:=0
		evalunit(ptoinit)
	end

	if pfrom.tag=jconst and pto.tag=jconst then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			pgen(kjump, pgenlabel(lab_e))
		end
	else
		if pfrom.tag=jconst then				!reverse condition; compare mem:imm
			evalunit(pto)
			evalunit(pfrom)
			pgencond(kjumpcc, (down|gt_cc|lt_cc), pgenlabel(lab_e))
		else
			evalunit(pindex)
			evalunit(pto)
			pgencond(kjumpcc, (down|lt_cc|gt_cc), pgenlabel(lab_e))
		end
		setmode_u(pindex)
	end

	definefwdlabel(lab_b)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	if pstep then
		if pstep.tag<>jconst then
			gerror("for/step non-const not ready")
		end
		stepx:=pstep.value
		if stepx<=0 then
			gerror("Bad for-step")
		end
	else
		stepx:=1
	end

	pgen(kload, genmem_u(pindex))			!load i
	pgen(kdouble)
!	pgen(kload, pgenint(stepx))				!load stepx
	genpushint(stepx)
	pgen((down|ksub|kadd))					!add/sub
	pgen(kstore, genmem_u(pindex))			!store (copy still on stack)

	if pto.tag=jconst then
!		pgen(kload, pgenint(pto.value))
		genpushint(pto.value)
	else
		pgen(kload, genmem_u(pto))
	fi
	pgencond(kjumpcc, (down|ge_cc|le_cc), pgenlabel(lab_b))

	if pelse then
		definefwdlabel(lab_e)
		evalblock(pelse)
	end

	definefwdlabel(lab_d)
	--loopindex
end

proc do_forall(unit p, pindex, plist, pbody, int down) =
!Structure:
!	forall
!		pindex -> plocal -> pfrom -> pto
!		plist -> passign
!		pbody -> [pelse]

	unit plocal, pfrom, pto, pelse, passign
	int lab_b, lab_c, lab_d, lab_e
	int a, b
	symbol dto

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
	end

	stacklooplabels(lab_b, lab_c, lab_d)

!now start generating code

	evalunit(pfrom)
	pgen(kstore, genmem_u(pindex))
	setmode_u(pindex)

	if pto.tag not in [jconst, jname] then
		evalunit(pto)
		dto:=getavname(currproc)
		pgen(kstore, genmem_d(dto))
		!setmodeint()
		pto:=createname(dto)
		pto.mode:=dto.mode
		pto.resultflag:=1
	end

	if pfrom.tag=jconst and pto.tag=jconst then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			pgen(kjump, pgenlabel(lab_e))
		end
	else
		if pfrom.tag=jconst then				!reverse condition; compare mem:imm
			evalunit(pto)
			evalunit(pfrom)
			pgencond(kjumpcc, (down|gt_cc|lt_cc), pgenlabel(lab_e))
		else
			evalunit(pfrom)
			evalunit(pto)
			pgencond(kjumpcc, (down|lt_cc|gt_cc), pgenlabel(lab_e))
		end
		setmode_u(pindex)
	end

	definefwdlabel(lab_b)

!need to generate assignment to local
	passign.resultflag:=0
	evalunit(passign)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	pgen(kload, genmem_u(pindex))			!load i
	pgen(kdouble)
!	pgen(kload, pgenint(1))					!load stepx
	genpushint(1)
	pgen((down|ksub|kadd))					!add/sub
	pgen(kstore, genmem_u(pindex))			!store (copy still on stack)

	case pto.tag
	when jconst then
!		pgen(kload, pgenint(pto.value))
		genpushint(pto.value)
	when jname then
		pgen(kload, genmem_u(pto))
	else
		PGEN(KLOAD, GENMEM_D(DTO))
	esac
	pgencond(kjumpcc, (down|ge_cc|le_cc), pgenlabel(lab_b))

	if pelse then
		definefwdlabel(lab_e)
		evalblock(pelse)
	end

	definefwdlabel(lab_d)
	--loopindex
end

proc do_convert(unit p, int opc) =

	evalunit(p.a)
	pgen(opc)
	setmode(p.mode)
	setmode2(p.oldmode)
end

proc do_swap(unit p, a, b) =
	evallv(a)
	do_setinplace()
	evallv(b)
	do_setinplace()
	pgen(kiswap)
	setmode_u(a)
end

global func checkdotchain(unit p, &pname)int=
!return accumulated offset of this and nested dot-expressions, 
!or -1 when offsets cannot be combined
	int offset

	case p.tag
	when jdot then
		offset:=checkdotchain(p.a, pname)
		return p.offset+offset

	else							!anything else, is the start expression
		pname:=p
		return 0
	end case
	return 0
end

proc do_dotref(unit pdot) =
	int imode:=createrefmode(nil, pdot.mode, 0)
	int offset
	unit a, pname


	a:=pdot.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a, pname)
		offset+:=pdot.offset
		a:=pname
	else
		offset:=pdot.offset
	end

	evalref(a)

	if offset then
		genpushint(offset)
		pgenix(kaddpx)
	end
	setmode(imode)
end

proc do_dot(unit pdot) =
	int offset
	unit a, pname

	a:=pdot.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a, pname)
		offset+:=pdot.offset
		a:=pname
	else
		offset:=pdot.offset
	end

	evalref(a)
	genpushint(offset)
	pgenix(kiloadx, 1)
	setmode_u(pdot)
end

proc do_storedot(unit pdot, pfield, rhs) =
	int offset
	unit a, pname

	evalunit(rhs)
	if pdot.resultflag then
		pgen(kdouble)
	end

	a:=pdot.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a, pname)
		offset+:=pdot.offset
		a:=pname
	else
		offset:=pdot.offset
	end

	evalref(a)
!	pgen(kload, pgenint(offset))
	genpushint(offset)
	!setmodeint()

	pgenix(kistorex, 1)

	setmode_u(pdot)
end

proc do_index(unit p, parray, pindex) =
	int addoffset, scale, offset

	addoffset:=getindexoffset(parray, pindex)

	evalarray(parray)
	scale:=ttsize[tttarget[parray.mode]]
	offset:=-ttlower[parray.mode]*scale + addoffset*scale

	evalunit(pindex)

	pgenix(kiloadx, scale, offset)
	setmode_u(p)
end

proc do_storeindex(unit p, parray, pindex, rhs) =
	int addoffset, scale
	addoffset:=getindexoffset(parray, pindex)

	evalunit(rhs)
	if p.resultflag then
		pgen(kdouble)
	end

	evalarray(parray)
	evalunit(pindex)

	scale:=ttsize[tttarget[parray.mode]]
	pgenix(kistorex, scale, -ttlower[parray.mode]*scale+addoffset*scale)
	setmode_u(p.a)
end

proc do_indexref(unit parray, pindex) =
	int addoffset, scale
	addoffset:=getindexoffset(parray, pindex)

	evalarray(parray)
	evalunit(pindex)

	scale:=ttsize[tttarget[parray.mode]]
	pgenix(kaddpx, scale, -ttlower[parray.mode]*scale+addoffset*scale)
	setmode(tttarget[parray.mode])
end

func getindexoffset(unit parray, &pindex)int offset=
!convert index like [i+3] to [i], returning the +3 etc as a separate offset
	int addoffset:=0

	if pindex.tag=jbin and pindex.pclop in [kadd, ksub] then
		if pindex.b.tag=jconst then		!incorporate const offset into lwb adjustment
			addoffset:=(pindex.pclop=kadd|pindex.b.value|-pindex.b.value)
			pindex:=pindex.a
		end
	end
	return addoffset
end

proc do_switch(unit p, pindex, pwhenthen, pelse, int isref=0) =
!'looptype' is set up here:
! 0 = switch	normal switch (range-checked)
! 1 = doswitch	looping switch (range-checked)
! 2 = doswitchu	looping switch via computed goto/indexed (both non-range-checked)
! 3 = doswitchx	looping switch via computed goto/labels

	const maxlabels = 1000
	int minlab, maxlab, n, iscomplex, i
	int lab_a, lab_d, labjump, elselab, labstmt, ax, bx, ismult, mode
	byte looptype
	[0..maxlabels]pcl labels
	unit w, wt, pjump, pnext
	pcl psetup, pc, px, pw
	symbol djump

	case p.tag
	when jswitch then
		looptype:=0
	when jdoswitch then
dodosw:
		looptype:=1
	when jdoswitchu then
!		if ctarget then dodosw fi			
		looptype:=2
	else
		looptype:=3
	end case

	ismult:=p.mode<>tvoid and looptype=0

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
				end
			when jconst then		!assume int
				ax:=bx:=w.value
				goto dorange
			else
				gerror_s("Switch when2: not const: #", strexpr(w).strptr)
			end case
			w:=w.nextunit
		end
		wt:=wt.nextunit
	end

	n:=maxlab-minlab+1
	if n>maxlabels then
		gerror("Switch too big")
	end

	if looptype then
		lab_a:=definelabel()
		lab_d:=createfwdlabel()
		stacklooplabels(lab_a, lab_a, lab_d)
	else
		lab_d:=createfwdlabel()
	end

	labjump:=createfwdlabel()
	elselab:=createfwdlabel()

	if ismult then pgen(kstartmx) end

	if looptype=3 then		!need to initialise pointer to JT
		pjump:=pindex.nextunit
		if pjump.tag<>jname then gerror("doswx not name") end
		djump:=pjump.def
		if ttbasetype[djump.mode]<>tref then gerror("doswx not ref") end

!these two instrs to be moved to kinitdswx later
		pw:=pccurr							!last op before the two new ops
		pgen(kload, pgenlabel(labjump))
		setmodeword()
		px:=pccurr							!px=pw.next = 1st of two ops

		pgen(kstore, genmem_u(pjump))
		setmodeword()

		if pcldoswx=nil then
			gerror("doswx in main?")
		end

!Before:  A B PCLDOSWX C D ...  W X Y				!X Y are load/store above
!After:   A B PCLDOSWX X Y D ...  W

		pc:=pcldoswx.next					!point to C
		pcldoswx.next:=px
		px.next.next:=pc
		pw.next:=nil
		pccurr:=pw

	end

	evalunit(pindex)

	if looptype<>3 then

		doswitchdispatch(labjump, elselab, minlab, maxlab, looptype<2)

	else
		pgen(kijump)
	end
	setmodeword()

	definefwdlabel(labjump)

	for i:=minlab to maxlab do			!fill with else labels first
		pgen(kdata, pgenlabel(elselab))
		labels[i]:=pccurr
	end

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
			end case
			for i:=ax to bx do
				labels[i].labelno:=labstmt
			end
			w:=w.nextunit
		end

		evalunitx(wt.b, isref)
		if ismult then pgen(kresetmx); setmode_u(p) end

		case looptype
		when 0 then
			genjumpl(lab_d)
		when 1 then
			genjumpl(lab_a)
		when 2 then
			evalunit(pindex)
			doswitchdispatch(labjump, elselab, minlab, maxlab, 0)
!			pgenxy(opc, minlab, maxlab, pgenlabel(labjump))
			!setmodeint()
		else
			evalunit(pindex)
			pgen(kijump)
			setmodeword()
		end case

		wt:=wt.nextunit
	end

	definefwdlabel(elselab)
	if pelse then
		evalunitx(pelse, isref)
		if ismult then pgen(kendmx); setmode_u(p) end
	end

	if looptype then
		case looptype
		when 1 then
			genjumpl(lab_a)
		when 2 then
			evalunit(pindex)
			doswitchdispatch(labjump, elselab, minlab, maxlab, 0)
		else
			evalunit(pindex)
			pgen(kijump)
			setmodeword()
		end case
		--loopindex
	end

	definefwdlabel(lab_d)
end

proc doswitchdispatch(int labjump, elselab, minlab, maxlab, rangecheck) =
!control expr has been pushed
!generate jump dispatch code
!rangecheck is 1 for switch/doswitch, 0 for doswitchu
	int oldmode

	if pccurr.size<8 then
		oldmode:=pccurr.mode
		pgen(kwiden)
		pccurr.mode2:=oldmode
	fi

	if minlab then			!make relative to zero
!		pgen(kload, pgenint(minlab))
		genpushint(minlab)
		pgen(ksub)
	end

	if rangecheck then		!switch/doswitch need range-checking
!		pgen(kload, pgenint(maxlab-minlab+1))
		genpushint(maxlab-minlab+1)
		pgencond(kjumpcc, ge_cc, pgenlabel(elselab))
		setmodeword()					!needs to be unsigned
		pccurr.popone:=1				!keep on stack if not jumping
	end

	pgen(kload, pgenlabel(labjump))		!index, label
	pgenxy(kswapstk, 1, 2)				!label, index
	pgenix(kiloadx, 8, 0)				!elements are 8 bytes; no offset
	pgen(kijump)

end

proc do_select(unit p, a, b, c, int isref) =
	const maxlabels=256
	[maxlabels]pcl labels
	int labend, labjump, n, i, elselab, labstmt, ismult
	unit q

	ismult:=p.mode<>tvoid and p.resultflag

	q:=b
	n:=0
	while q do
		if n>=maxlabels then gerror("selectx: too many labels") end
		++n
		q:=q.nextunit
	end

	labend:=createfwdlabel()
	labjump:=createfwdlabel()
	elselab:=createfwdlabel()

	if ismult then pgen(kstartmx) end
	evalunit(a)

	doswitchdispatch(labjump, elselab, 1, n, 1)

	definefwdlabel(labjump)

	q:=b
	i:=0
	for i:=1 to n do
		pgen(kdata, pgenlabel(elselab))
		setmodeword()
		labels[i]:=pccurr
	end

	q:=b
	i:=0
	while q do
		labstmt:=definelabel()
		++i
		labels[i].labelno:=labstmt
		evalunitx(q, isref)
		if ismult then pgen(kresetmx); setmode_u(p) end
		genjumpl(labend)
		q:=q.nextunit
	end

	definefwdlabel(elselab)

	evalunitx(c, isref)
	if ismult then pgen(kendmx); setmode_u(p) end

	definefwdlabel(labend)
end

proc do_case(unit p, pindex, pwhenthen, pelse, int loopsw, isref) =
	const maxcase=500
	[maxcase]int labtable
	[maxcase]unit unittable
	int ncases, ismult, a, b

	int lab_abc, lab_d, labelse
	unit w, wt, plower, pupper

	loopsw:=p.tag=jdocase

	if pindex=nil then
		GERROR("EMPTY CASE NOT DONE")
	end

	ismult:=p.mode<>tvoid and not loopsw

	if loopsw then
		lab_abc:=definelabel()		!start of loop
		lab_d:=createfwdlabel()	!end of case/end of loop
		stacklooplabels(lab_abc, lab_abc, lab_d)
	else
		lab_d:=createfwdlabel()	!end of case/end of loop
	end

	if ismult then pgen(kstartmx) end

	ncases:=0

	if casedepth>=maxcasedepth then
		gerror("case nested too deeply")
	end
	casestmt[++casedepth]:=p

	if pwhenthen=nil then
		if ismult then gerror("case") end
		goto skip
	end

	evalunit(pindex)

!CPL "INCR CASED", CASEDEPTH

	wt:=pwhenthen

	while wt do
		w:=wt.a
		if ncases>=maxcase then
			gerror("too many cases")
		end
		labtable[++ncases]:=createfwdlabel()
		unittable[ncases]:=wt.b

		while w do
			evalunit(w)
			pgencond(kjumpcc, eq_cc, pgenlabel(w.whenlabel:=labtable[ncases]))
			if w.nextunit or wt.nextunit then
				pccurr.popone:=1
			end
			setmode_u(w)
			w:=w.nextunit
		end

		wt:=wt.nextunit
	end

skip:
	labelse:=createfwdlabel()
	caseelse[casedepth]:=labelse
	genjumpl(labelse)

	for i:=1 to ncases do
		definefwdlabel(labtable[i])
		evalunitx(unittable[i], isref)
		if ismult then pgen(kresetmx); setmode_u(p) end

		if loopsw then
			genjumpl(lab_abc)
		else
			genjumpl(lab_d)
		end
	end

	definefwdlabel(labelse)

	if pelse then
		evalunitx(pelse, isref)
		if ismult then pgen(kendmx); setmode_u(p) end
	end

	if loopsw then
		genjumpl(lab_abc)
		definefwdlabel(lab_d)
		--loopindex
	else
		definefwdlabel(lab_d)
	end

	--casedepth
end

proc do_dotindex(unit p, a, b) =
	evalunit(a)
	evalunit(b)

	pgen(kshr)
	setmodeword()

	genpushint(1)
	pgen(kbitand)
	setmodeword()
end

proc do_dotslice(unit p, a, b) =
	u64 mask
	int i, j

	evalunit(a)

	if b.a.tag=b.b.tag=jconst then
		i:=b.a.value
		j:=b.b.value
		if i>j then swap(i, j) end

		if j=63 then			!signed field includes sign bit; assume i>0
			genpushint(i)
			pgen(kshr)
		else
			if i then
				genpushint(i)
				pgen(kshr)
				setmodeword()
			end
			mask:=inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
			genpushint(mask)
			pgen(kbitand)
			setmodeword()
		fi
	else
		GERROR("DOTSLICE/VAR")
	end
end

proc do_read(unit p, a) =
	int m

	m:=p.mode

	if a=nil then
		a:=pzero
	end

	if ttisinteger[m] then
		gencallsysfn(sf_read_i64, a)
	elsif ttisreal[m] and ttsize[m]=8 then
		gencallsysfn(sf_read_r64, a)
	elsif m=trefchar then
		gencallsysfn(sf_read_str, a)
	else
CPL =STRMODE(M)
		GERROR("CAN'T READ THIS ITEM")
	end
	setmode_u(p)
end

proc do_readln(unit a) =
	if a then
		if ttbasetype[a.mode]<>tref then gerror("@dev no ref") end

		case ttbasetype[tttarget[a.mode]]
		when tvoid then
			gencallsysproc(sf_read_fileline, a)
		when tu8, tc8 then
			gencallsysproc(sf_read_strline, a)
		else
			gerror("rd@dev?")
		end case
	else
		gencallsysproc(sf_read_conline)
	end
end

proc docond(int opc, unit p, int lab)=
	genjumpcond(opc, p, lab)
end

proc do_syscall(unit p, a)=

	setfunctab()

	case p.fnindex
	when sf_getnprocs then
		pgen(kload, pgenmem(pnprocs))

	when sf_getprocname then
		pgen(kload, pgenmemaddr(pprocname))
		setmodeword()
		evalunit(a)
		pgenix(kiloadx, 8, -8)

	when sf_getprocaddr then
		pgen(kload, pgenmemaddr(pprocaddr))
		setmodeword()
		evalunit(a)
		pgenix(kiloadx, 8, -8)

	else
		PCOMMENT("SYSCALL/GENERIC")
	end case
	!setmodeint()
end

proc do_slice(unit p, a, b) =
!generate separate code for (ptr, length) parts

	if b=nil then
		evalarray(a)
		if a.tag=jconst then			!assume string
			genpushint(strlen(a.svalue))
		else
			genpushint(ttlength[a.mode])
		end

	else
!worth checking for const bounds? Both must be const to make it worthwhile
		do_indexref(a, b.a)
		if b.a.tag=b.b.tag=jconst then
			genpushint(b.b.value-b.a.value+1)
		else
			evalunit(b.b)
			evalunit(b.a)
			pgen(ksub)
			!setmodeint()
			genpushint(1)
			pgen(kadd)
		end
		!setmodeint()

	end

	pgen(kstorem); setmode(tslice)
end

proc do_assignblock(unit p, a, b) =
!fstore=1 when result is needed
!method used is:
! load ^to lhs
! load ^to rhs
! do block xfer, not using the stack

	if b.tag=jmakelist then
		if ttbasetype[a.mode]=tarray then
			do_assignarray(a, b)
		else
			do_assignrecord(a, b)
		end
	else
		GERROR("ASSIGN BLOCK")
	end
end

proc do_assignarray(unit a, b)=
	unit passign, pindex, pconst, q
	int index

	if ttbasetype[tttarget[a.mode]]=tc8 then
		gerror("Assignment not suitable for []char type")
	end

	pconst:=createconstunit(1, ti64)
	pindex:=createunit2(jindex, a, pconst)
	passign:=createunit2(jassign, pindex, b.a)
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
	end

end

proc do_assignrecord(unit a, b)=
	unit passign, pdot, pfield, q
	int m, fieldtype
	symbol d, e

	pfield:=createunit0(jname)
	pdot:=createunit2(jdot, a, pfield)
	passign:=createunit2(jassign, pdot, b.a)
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
		end
		e:=e.nextdef
	end
end

proc pushrhs(unit a)=
	if a=nil then return end
	pushrhs(a.nextunit)
	evalunit(a)
end

proc do_assignms(unit a, b)=
	int nlhs, nrhs
	symbol d

	nlhs:=a.length

	case b.tag
	when jcall then
		evalunit(b)
		if b.a.tag<>jname then
			gerror("multassign from fn: not simple fn")
		end
		d:=b.a.def
		nrhs:=d.nretvalues

		a:=a.a					!point to elements of makelist
	elsif ttbasetype[b.mode]=tslice then
GERROR("DECONSTR SLICE NOT READY")
	else
		gerror("(a, b):=x; var only")
	end case

	poptomult(a)

	if nrhs>nlhs then
		d:=getprocretmodes(b)

		for i:=nlhs+1 to nrhs do
			pgen(kunload)
			setmode(ttmult[d.mode, i])
		end
	end
end

proc do_assignmm(unit a, b)=
!(a, b, c):=(x, y, z)
	pushrhs(b.a)			!push rhs elements in right-to-left order
	pgen(kloadall)
	poptomult(a.a)
end

proc do_assignmdrem(unit a, b)=
!(a, b):=x divrem y
	evalunit(b)
	poptomult(a.a)
end

proc poptomult(unit a)=
!a is a linked list; assign n popped values to each element in turn 
	repeat
		case a.tag
		when jname then
			pgen(kstore, genmem_u(a))
		when jindex, jslice, jdot then
			evalref(a)
			pgen(kistore)

		when jptr then
			evalunit(a.a)
			pgen(kistore)

		when jif, jcase, jswitch, jselect then
			evalref(a)

			pgen(kistore)

!		when jdotindex then
!CPL "POP TO MULT/DOTINDEX"
!			evalref(a.a)
!			evalunit(a.b)
!			pgen(kstorebit)
!
		else
			cpl jtagnames[a.tag]
			gerror("Bad mult assign element")
		end case

		setmode_u(a)

		a:=a.nextunit
	until a=nil
end

proc do_recase(unit p, a)=
	unit q, wt, w
	int destlab, casevalue

!CPL "DO_RECASE", CASEDEPTH

	if casedepth=0 then
		gerror("recase outside case stmt")
	end

	if a then
		casevalue:=a.value
	else				!a=null means goto else
		genjumpl(caseelse[casedepth])
	end

	q:=casestmt[casedepth]

	destlab:=0

	wt:=q.b
	while wt do
		w:=wt.a
		while w do
			if w.tag=jconst and ttisinteger[w.mode] and w.value=casevalue then
				destlab:=w.whenlabel
				exit all
			end
			w:=w.nextunit
		end
		wt:=wt.nextunit
	end

	if destlab=0 then
		genjumpl(caseelse[casedepth])
	else
		genjumpl(destlab)
	end
end

proc do_empty(unit p, a)=
	evallv(a)
	pgen(kclear)
	setmode_u(a)
end

proc do_typeconst(unit p)=
	genpushint(p.value)
end

proc do_setinplace=
	if pccurr.opcode=kload and pccurr.opndtype=memaddr_opnd then
		pccurr.inplace:=1
	end
end

proc do_storebit(unit lhs, pindex, rhs, int dupl)=
!Do lhs.[index] := rhs, or A.[i] := x
	byte oldmode, x
	u64 mask
	int shift

!IF LHS.TAG<>JNAME THEN
!
!	evalunit(rhs)			!rhs (x)
!
!	evalref(lhs)			!lhs (A)
!	evalunit(pindex)		!index (i)
!	pgen(kstorebit)
!	setmode_u(lhs)
!
!	RETURN
!END
	
!CPL "SIMPLE LHS"
!	IF DUPL THEN GERROR("STORBIT/DUPL") FI
	getbfdupl(rhs, dupl)

	if rhs.tag=jconst then
		x:=rhs.value iand 1				!x is 0 or 1
	else
		x:=2							!means variable in rhs
	end	

	loadlhs(lhs)

!	if lhs.tag=jname then				!simple dest
!		pgen(kload, genmem_u(lhs))
!	else
!!CPL "STOREBIT/PX"
!		evalref(lhs)					!load ptr: P
!!		pgen(kdouble)					!P P
!		pgen(kdupl)					!P P
!		pgen(kiload)					!P lhs value
!	end
!	setmode_u(lhs)

!value of lhs has been loaded
!now load or evaluate the mask

	if pindex.tag=jconst then			!index known here
		shift:=pindex.value
		mask:=(shift|1<<shift|1)
		if x<>1 then
			mask := inot mask
		end
		genpushint(mask)
	else								!index is a variable
		genpushint(1)
		evalunit(pindex)
		pgen(kshl)
		if x<>1 then
			pgen(kbitnot)
		end

	end
!lhs value and suitable mask have been pushed
	case x
	when 0 then							!store zero bt
		pgen(kbitand)
	when 1 then							!store 1 bit
		pgen(kbitor)
	else								!x is variable; assume in 0..1
		pgen(kbitand)					!clear bit first
		evalunit(rhs)
		evalunit(pindex)
		pgen(kshl)
		pgen(kbitor)
	end

!done; now store result
	storelhs(lhs)
!	if lhs.tag=jname then				!simple dest
!		pgen(kstore, genmem_u(lhs))
!	else
!		pgenxy(kswapstk, 1, 2)			!get P to top
!		pgen(kistore)					!store new value
!	end
!	setmode_u(lhs)
end

proc loadlhs(unit p)=
!Load the lhs of ADDTO/STOREBF et
!For simple name, load the value directly
!For anything else, load a reference, duplicate it, and load the value from
!the top pointer.
!Stack will have (Ptr, value); caller will store new value via that pointer
	if p.tag=jname then				!simple dest
		pgen(kload, genmem_u(p))
	else
		evalref(p)					!load ptr: P
		pgen(kdupl)					!P P
		pgen(kiload)					!P lhs value
	end
	setmode_u(p)
end

proc storelhs(unit p)=
!store value on stack to p
!p is a name, or more complex; in latter case, use and consume the pointer
!set up via loadlhd
	if p.tag=jname then				!simple dest
		pgen(kstore, genmem_u(p))
	else
		pgenxy(kswapstk, 1, 2)			!get P to top
		pgen(kistore)					!store new value
	end
	setmode_u(p)
end

proc getbfdupl(unit p, int dupl)=
!p is rhs of storebit/bf
!when dupl is 1, push an extra copy of p, but this is only viable
!(due to side-effects) for simple p
	return unless dupl
	if p.tag not in [jconst, jname] then
		gerror("Storebit/bf dupl?")
	fi
	evalunit(p)
end

proc do_storebf(unit lhs, ix, iy, rhs, int dupl)=
	int i, j

	getbfdupl(rhs, dupl)
	unless ix.tag = iy.tag = jconst then
		gerror("Storebf ij not const")
	end

	i:=ix.value
	j:=iy.value
	if i>j then swap(i, j) end

	loadlhs(lhs)

	genpushword(inot((inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i))
	pgen(kbitand)

	evalunit(rhs)
	if i then
		genpushint(i)
		pgen(kshl)
	fi

	pgen(kbitor)

	storelhs(lhs)
end
=== mm_cli.m 0 0 33/74 ===

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
	(clang_pass,	"c"),			! .c

	(pcl_pass,		"pcl"),			! .pcl (?)
	(mcl_pass,		"asm"),			! .asm
	(asm_pass,		"asm"),			! .asm
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
	(pcl_sw,		"p",			pcl_pass),
	(mcl_sw,		"mcl",			mcl_pass),
	(asm_sw,		"a",			asm_pass),
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
	(opt0_sw,		"o0",			0),
	(opt1_sw,		"o1",			1),
	(opt2_sw,		"o2",			2),
!	(opt3_sw,		"o3",			3),

!diagnostic outputs
	(ast1_sw,		"ast1",			0),
	(ast2_sw,		"ast2",			0),
	(ast3_sw,		"ast3",			0),
	(showpcl_sw,	"showpcl",		0),
	(showc_sw,		"showc",		0),
	(showasm_sw,	"showasm",		0),
	(st_sw,			"st",			0),
	(stflat_sw,		"stflat",		0),
	(types_sw,		"types",		0),
	(showmodules_sw,"modules",		0),
	(pst_sw,		"pst",			0),

	(shortnames_sw,	"shortnames",	0),


	(time_sw,		"time",			0),
	(v_sw,			"v",			2),
	(vv_sw,			"vv",			3),
	(quiet_sw,		"q",			0),

	(csize_sw,		"cs",			1),
	(esize_sw,		"es",			2),
	(size_sw,		"ss",			3),

	(help_sw,		"h",			0),
	(help2_sw,		"help",			0),
	(ext_sw,		"ext",			0),
	(out_sw,		"o",			0),
	(outpath_sw,	"outpath",		0),
	(unused_sw,		"unused",		0),

	(norip_sw,		"norip",		0),
	(himem_sw,		"himem",		2),

	(decons_sw,		"decons",		0),

!	(debug_sw,		"debug",		1),
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
global int compiletime

global proc main=
!STOP
!proc main=
	unit p,q,r
	int m,fileno,ntokens,t,tt

!cpl "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
!
!CPL =STREC.BYTES
!CPL =PSTREC.BYTES
!CPL =MCLREC.BYTES
!CPL =unitrec.bytes
!CPL =MCLOPNDREC.bytes
!CPL =JTAGNAMES.LEN
!CPL =pclrec.bytes
!CPL =PCLNAMES.LEN
!CPL =kdata
!CPL =kcomment

!for s in pclnames do
!	fprintln "proc px_#*(pcl p) =", s
!	println "\tunimpl(p)"
!	println "end"
!	println
!end

!for s in jtagnames do
!	fprintln "\twhen j# then", s
!	fprintln "\t\ttx := do_#(p)", s
!	println
!end
!
	startclock:=os_clock()
	initdata()

	getinputoptions()

!CPL =FREGOPTIM
!CPL =FPEEPHOLE
!CPL =HIGHMEM

	showcompilemess()

! Do early passes common to all options

	loadproject(inputfile)

	do_parse()
	do_name()
	do_type()

! Special outputs can be done at this point
	do_writema(inputfile)	when passlevel=ma_pass			! terminates
	do_getinfo(inputfile)	when passlevel in [getst_pass, getproj_pass]		! terminates
	do_writeexports()		when passlevel=dll_pass

!CPL =PASSNAMES[PASSLEVEL]

	case passlevel
	when clang_pass then
		do_clang()
	when pcl_pass then
		do_genpcl()

	when mcl_pass, asm_pass then
!CPL $LINENO
		do_genmcl()
!CPL $LINENO

	when obj_pass then
		do_writeobj()	

	when exe_pass then
		do_writeexe()	

	when dll_pass then
		do_writedll()	

	when mx_pass then
		do_writemx()	

	when run_pass then
		do_run()

	end case

	showsurveys()

!CPL $LINENO
	do_outputs()
!CPL $LINENO
	showlogfile()

!CPL $LINENO
	showtimings() when fshowtiming

	if fverbose=3 then println "Finished." end

end

proc showcompilemess=
	if fverbose>=1 and not msfile then
		fprint "Compiling # to #",inputfile,changeext(outfile,passnames[passlevel])
!		print $,fregoptim, fpeephole
		println
	end
end

proc do_parse=
!	if fverbose=3 then println "PARSE" end

	return unless passlevel>=parse_pass

	int tt:=clock()

	for i to nmodules do
		parsemodule(modules[i])
	end
	parsetime:=clock()-tt

	if passlevel>=fixup_pass then
!		if fverbose=3 then println "FIXUP" end
		fixusertypes()
	end

	fixstartprocs()
!
	if fshowast1 then showast("AST1") end
end

proc do_name=
!	if fverbose=3 then println "NAME" end
	return unless passlevel>=name_pass

	int tt:=clock()
	rx_typetable()

	for i:=2 to nmodules do
		rx_module(i)
	end
	rx_module(1)
	resolvetime:=clock()-tt

	if fshowast2 then showast("AST2") end
end

proc do_type=
!	if fverbose=3 then println "TYPE" end

	return unless passlevel>=type_pass
	int tt:=clock()
	tx_typetable()

	for i:=1 to nmodules do
		tx_module(i)
	end
	tx_allprocs()
	typetime:=clock()-tt
	if fshowast3 then showast("AST3") end
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
	if assemtype='GAS' then highmem:=2 end



end

proc getinputoptions=
	int paramno,pmtype,sw,extlen
	ichar name,value,ext
	[300]char filespec

!	if tc_useruntcl then
!		passlevel:=runtcl_pass
!		fverbose:=0
!	end
	paramno:=1

	if eqstring(extractfile(os_gethostname()),"ms.exe") then
		msfile:=1
		fverbose:=0
		do_option(run_sw, "")
	end

	while pmtype:=nextcmdparamnew(paramno,name,value,"m") do
		case pmtype
		when pm_option then

			convlcstring(name)
			for sw to optionnames.len do
				if eqstring(name,optionnames[sw]) then
					do_option(sw,value,paramno)
					exit
				end
			else
				println "Unknown option:",name
				stop 99
			end
		when pm_sourcefile then
			if inputfile then
				loaderror("Specify one lead module only")
			end
			convlcstring(name)
			inputfile:=pcm_copyheapstring(name)

!CPL =PASSNAMES[PASSLEVEL]

			if passlevel= run_pass then
				cmdskip:=paramno-1+$CMDSKIP
!CPL "EXITG1"
				exit
			end

		when pm_libfile then
			loaderror("Lib files go in module headers")
		else
			loaderror("Invalid params")
		end case

	end

	if passlevel=0 then
		passlevel:=exe_pass
		outext:="exe"
	end

	case passlevel
	when obj_pass, dll_pass then
		highmem:=2
	when mcl_pass then
!*!		if assemtype='NASM' then highmem:=2 end
	when mx_pass, run_pass then
		highmem:=0
	end case

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
		end

		if destfilepath then
			strcpy(filespec,destfilepath)
			strcat(extractfile(filespec), outfile)
			outfile:=pcm_copyheapstring(filespec)	
		end
	end

	ext:=extractext(inputfile)
	extlen:=strlen(ext)
	strcpy(filespec, changeext(cmdparams[0],ext))
	convlcstring(filespec)
	if eqstring(filespec, inputfile) and passlevel=exe_pass then
		strcpy(&filespec[1]+strlen(filespec)-extlen-1, "2.m")
		outfile:=pcm_copyheapstring(filespec)
		println "New dest=",outfile
	end

!*!	tcl_setflags(highmem:highmem, shortnames:fshortnames)
!*!	tcl_cmdskip(cmdskip)
!*!	if msyslevel=2 then pfullsys:=1 end

end

proc do_option(int sw, ichar value, int paramno=0)=
	static byte outused, outpathused
	byte newpass

!CPL "DOOPTION", OPTIONNAMES[SW], PASSLEVEL

	if sw in ma_sw..run_sw then
		newpass:=optionvalues[sw]
		if passlevel and newpass<>passlevel then
			loaderror("Conflicting pass:", optionnames[sw])
		end
		passlevel:=newpass
		outext:=passnames[sw]

		return
	end

	if sw in ast1_sw..pst_sw then
		fshowdiags:=1
	end

	if sw in noopt_sw..opt2_sw then
		foptimise:=optionvalues[sw]
	end

	case sw
	when ast1_sw then fshowast1:=1
	when ast2_sw then fshowast2:=1
	when ast3_sw then fshowast3:=1
	when showasm_sw then fshowasm:=1
	when showpcl_sw then fshowpcl:=1
	when showc_sw then fshowc:=1
	when st_sw then fshowst:=1
	when pst_sw then fshowpst:=1
	when stflat_sw then fshowstflat:=1
	when types_sw then fshowtypes:=1
	when showmodules_sw then fshowmodules:=1

	when clinux_sw then clinux:=1

	when sys_sw, minsys_sw, nosys_sw then msyslevel:=optionvalues[sw]

	when time_sw then fshowtiming:=1
	when v_sw, vv_sw, quiet_sw then fverbose:=optionvalues[sw]
	when csize_sw, esize_sw, size_sw then fcodesize:=optionvalues[sw]

	when help_sw, help2_sw then showhelp(); stop
	when ext_sw then dointlibs:=0

	when out_sw then
		if outpathused then loaderror("mixed out/path") end
		destfilename:=pcm_copyheapstring(value)
		outused:=1

	when outpath_sw then
		if outused then loaderror("mixed out/path") end
		if (value+strlen(value)-1)^ not in ['\\','/'] then
			loaderror("Path needs to end with \\ or /")
		end
		destfilepath:=pcm_copyheapstring(value)
		outpathused:=1

	when unused_sw then fcheckunusedlocals:=1

	when shortnames_sw then fshortnames:=1

	when norip_sw, himem_sw then highmem:=optionvalues[sw]

	when decons_sw then fdeconsma:=1

!	WHEN DEBUG_SW THEN DEBUG:=1


	end case

end

proc showcaption=
	println "M Compiler 8.1", $date, $time, "Opt:",foptimise
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
		end
	end


	for i to nmodules do
		ms:=modules[i]
		if ms.ststart then
			subproghasstart[ms.subprogno]:=1
		end
	end

	for i to nmodules do
		ms:=modules[i]
		if ms.ststart=nil then
			s:=ms.subprogno
			if subproghasstart[s] and subprogs[s].mainmodule=i then
				ms.ststart:=addstartproc(ms.stmodule,"start", program_scope,i)
!				ms.ststart.pclinfo:=pcm_allocnfz(pclinforec.bytes)
			end
		end

	end
end

func addstartproc(symbol owner, ichar name, int scope,moduleno)symbol stproc=
	stproc:=getduplnameptr(owner,addnamestr(name),procid)
	stproc.scope:=scope
	stproc.moduleno:=moduleno
	stproc.subprogno:=moduletosub[moduleno]
	stproc.code:=makeblock(nil)
	adddef(owner,stproc)
	addlinear(stproc)

	return stproc
end

proc showsurveys=

!CPL =NALLPCL
!CPL =NSPECIAL

!CPL =NALLGENRRM
!CPL =NABREG
!CPL =NOPCBREG
!cpl =STLINEAR
!SYMBOL D
!D:=STLINEAR
!
!WHILE D, D:=D.NEXTLINEAR DO
!!	IF D.NAMEID=PROCID THEN
!	IF D.NAMEID=STATICID THEN
!		IF NOT D.USED THEN
!			CPL D.NAME, =D.USED
!		FI
!	FI
!
!OD
!

end

proc do_genpcl=
!CPL "GENPCL/////"
	if not pclpresent then loaderror("No PCL") end
	genpcl()
end

proc do_genmcl=

	if not mclpresent then loaderror("No MCL") end

	genpcl()

!CPL "SS", $lineno
	genmcl()
!CPL "SS", $lineno
end

proc do_writeobj=
	CPL "WRITEOBJ NOT READY"
end

proc do_writeexe=
	^strbuffer ss

	genpcl()
	genmcl()
	
	genss()

	writeexe(changeext(outfile, "exe"), 0)
!	CPL "WRITEEXE NOT READY"
end

proc do_writedll=
	^strbuffer ss

	genpcl()
	genmcl()
	
	genss()

!CPL "WRITEDLL:"
!os_getch()
	writeexe(changeext(outfile, "dll"), 1)
!	CPL "WRITEEXE NOT READY"
end

proc do_writemx=
!	CPL "WRITEMX NOT READY"
	genpcl()
	genmcl()
	
	genss()

	writemcx(changeext(outfile, "mx"))
!	CPL "WRITEEXE NOT READY"
end

proc dowritepcl=
	ichar filename
	^strbuffer pclstr
	filehandle f

	pclstr:=writeallpcl()

	filename:="PCL"

	if fverbose>=2 then println "Writing PCL" end

	f:=fopen(filename, "w")
	gs_println(pclstr, f)
	fclose(f)

	gs_free(pclstr)
end

proc dowritepst=
	ichar filename
	^strbuffer pststr
	filehandle f

!CPL "WRITING PST"
	pststr:=writepst()

	filename:="PST"

	if fverbose>=2 then println "Writing PST" end

	f:=fopen(filename, "w")
	gs_println(pststr, f)
	fclose(f)

	gs_free(pststr)
end

proc dowriteasm=
	ichar filename
	^strbuffer asmstr
	filehandle f

!CPL "-----------WRITE ASM1"

!	if assemtype='NASM' then
!		phighmem:=2
!	end

	asmstr:=writeasm()

!CPL =ASMSTR

	STOP when asmstr=nil

!	filename:=changeext(outfile, (ctarget|"c"|"asm"))
	filename:=changeext(outfile, "asm")

!	if fverbose>=2 then
		println "Writing ASM", filename
!	end

	f:=fopen(filename, "w")
	gs_println(asmstr, f)
	fclose(f)

	gs_free(asmstr)
end

proc do_run=
!this to run native code, not interpreter
!	loaderror("No RUN")

	genpcl()

	genmcl()
	genss()
	runlibfile("dummy", cmdskip)

end

proc do_outputs=
!Any diagnostic or requested textual outputs
!These will be written to a dedicated file here
!If appearing in a composite log file, then they are appended ther
!Some stuff like 'showproject info' is only done there, direct to logfile
!CPL "DOOUT------------", =FSHOWPCL, =passlevel, =PCL_PASS

	if fshowpcl and passlevel>=pcl_pass then
		dowritepcl()
	end

!'mcl'/'fshowasm' is also used for C target from backend, so adjust file exts
!CPL =PASSNAMES[PASSLEVEL]

	if fshowasm and passlevel>=mcl_pass or passlevel=asm_pass then
		CPL "WRITEMCL"
		dowriteasm()
	end

	if fshowpst and passlevel>=pcl_pass then
		dowritepst()

	end

!	if fshowast3 then showast("AST3") end
!	if fshowss and passlevel>=obj_pass then
!
!	end

end

proc do_clang=
!	ichar filename
!	filehandle f
!
!	genc(inputfile)
!
!CPL =FSHOWDIAGS
!
!!CPL "-----------WRITE ASM1"
!
!!	if assemtype='NASM' then
!!		phighmem:=2
!!	end
!
!!	cstrasmstr:=writeasm()
!!
!!!CPL =ASMSTR
!!
!!	STOP when asmstr=nil
!!
!!!	filename:=changeext(outfile, (ctarget|"c"|"asm"))
!	filename:=changeext(outfile, "c")
!!
!!	if fverbose>=2 then
!		println "Writing C", filename
!!!	end
!!
!	f:=fopen(filename, "w")
!	gs_println(cdest, f)
!	fclose(f)
!!
!!	gs_free(asmstr)
!!
!
!!	stop
!
end
=== mm_decls.m 0 0 34/74 ===
global const maxmodule=400
global const maxsubprog=30
global const maxlibfile=50
global const maxsourcefile=300

global type symbol		= ^strec
global type unit  		= ^unitrec
global type imodule   	= ^modulerec
global type ifile   	= ^filerec
global type isubprog  	= ^subprogrec

global macro pr(a,b)	= (a<<16 ior b)

global record tokenrec =
	byte symbol
	byte subcode
	u16 slength				!string length; includes any zero term
	u32 pos: (sourceoffset:24, fileno:8)

	union
		^strec symptr		!pointer to symbol table entry for name
		i64 value				!64-bit int
		real xvalue				!64-bit float
		u64 uvalue			!64-bit word
		ichar svalue			!pointer to string or charconst (not terminated)
	end
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
	^i32 pmode
end

global record posrec=
	u32 pos: (sourceoffset:24, fileno:8)
end

global record uflagsrec =
	[7]byte	codes
	byte	ulength
end

!global record strec = $caligned
global record strec =
	ichar name
	^strec owner
	^strec deflist
	^strec deflistx			!point to last in deflist

	^strec nextdef
	^strec firstdupl			!point to generic version
	^strec nextdupl

	^strec nextlinear			!next linear symbol (user identifiers only)

	psymbol pdef

!	^fwdrec fwdrefs

!	MCLOPND MX

	unit code					!vars with init data or proc bodies

	i32 mode					!modes must be i32 to allow fixup later
	byte namelen
	byte symbol
	byte subcode
	byte nameid					!procid etc

	union
		i32 lexindex			!for certain tokens
		i32 fnindex				!dll proc: index into table
		i32 labelno				!named labels: labelno
	end
	i32 offset

	u32 pos: (
		sourceoffset:24,		!char offset within source file
		fileno:8)				!sourcefile no

	u16 flags: (
		isstatic:1,
		hasdoswx:1,				!uses doswitchx in proc
		txdone:1,				!tx-processing flags
		circflag:1,

		islet:1,				!let-name initialised
		ishandler:1,			!proc is a handler (proc F*())

		atfield:1,				!field def has @ x (.equivfield will be set too, but that is shared)

		ADDROF:1,				!TEMP ADDITIONS FOR MCL/PCL BACKEND
		ISENTRY:1,

		regcand:1,				!1 when either could be a candidate for a regvar,
		xregcand:1,				! if there were available registers

		used:1,
		cvariadic:1,			!used in C front end; always 0 here
		isimport:1)				!for dllproc/dllvar

	byte moduleno				!module in which this name is defined
	byte subprogno				!enclosing sub-prog

	unit equivvar				!nil, or reference to @ var, as an expression

	u32 size					!used in backend, when set to basic 'tblock'

	union
		struct					!when a proc or dllproc
			ichar truename		!for imported name only
			^strec paramlist

			byte nretvalues		!func: number of return values (0 for proc)
			byte varparams		!0 or number fixed params for variadic imports
[6]BYTE DUMMY
		end

		struct					!when a record or record field
			^strec equivfield	!@ x used in record field defs; ^to x
			uflagsrec uflags
			byte bitfieldwidth	!width of bitfield in record
			byte caligned		!caligned used
			byte bitoffset		!0..31 for bitfields in records
			byte equivoffset
	byte maxalign				!for record types (doesn't fit above)
		end

		struct					!when a param name
			^strec nextparam
			byte byref			!0/1 = byvalue/byref
			byte optional		!0 or 1	
		end
	end

!	^pclinforec pclinfo

!	byte used					!1 if var used at least once

	byte scope					!module_scope etc

!temporary, added for mcl backend which now uses same strec
!	byte reg
!	byte reftype
!	byte segment
!	i16 stindex
!	i16 importindex
!	i16 exportindex
!	u32 size
end

global record unitrec = $caligned
!global record unitrec =
	unit nextunit

	byte tag					!jtag number
	byte resultflag				!1 when the result of this unit is needed; 0=void or discarded
	byte insptr					!tx-processing flags
	byte txcount

	u32 pos: (
		sourceoffset:24,		!char offset in source file
		fileno:8)				!sourcefileno

	i32 mode
	i32 oldmode					!convert/typepun: original mode (usually that of any subnode)

	byte moduleno
	byte subprogno
	byte flags: (
		initlet:1,				!1 for an assignment that initialises a let
		isconst:1)				!1 for jconst, and jmakerange with const range

	union
		byte pclop				!kadd etc, for jbin, incr etc
		byte propcode			!kklen etc
		byte inv				!notin
!		byte convcode			!kkfix etc
		byte bfcode				!bf_even etc
		byte cvindex			!cv_lineno etc, compile var index
		byte fnindex			!sf_print etc
	end
	union
		byte mathsop			!maths_sin etc
		byte condcode			!eq_cc etc
	end

!	byte pmode					!
	byte avcode
	[2]byte SPARE

!Rest of unit is fields A, B, C. Not all will be used
!Jsubs[tag] says how many (0-3) will be sub-nodes
!The rest may be used for other purposes depending on jtag

	union
		unit	a				!subnode
		symbol	def				!jname
		i64		value			!jconst
		r64		xvalue			!jconst
		ichar	svalue			!jconst: string const or data-string
		i64		range_lower		!jmakerange only
	end

	union
		unit	b				!subnode
		i64		range_upper		!jmakerange only
	end

	union
		unit	c				!subnode

		struct					!const string
			u32  slength		!includes any zero term
			byte isastring
			char strtype		!0/'B'/'S' = normal / bindata / strdata
		end

		u32 length			!makelist: number of elements

		struct					!cmpchain
			[8]byte cmpgenop
		end

		u32 offset				!jdot
!		byte avcode				!jname for/autovars: 'I','T','S' = index/to/step autovars

		i32 loopindex			!jexit etc
		i32 whenlabel			!label associated with expr, for recase

	end
	[3]unit abc @ a				!alias a/b/c with an array
end

global record modulerec=
	ichar	name				!module name and base filename
	ifile	file
	i16		moduleno			!useful if using pointer to a source rec
	i16		subprogno
	i16		fileno
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

global symbol stprogram				!root into the symbol table
global symbol stmodule				!main module
global symbol stlinear, stlinearx	!linear list of selected identifiers (eg. statics, procs)
global int currmoduleno				!used when compiling modules
global byte loadedfromma			!1 if source/support files are in loaded .ma file

global tokenrec lx					!provides access to current token data
global tokenrec nextlx				!provides access to next token

global [0..maxlibfile]ichar libfiles

global int mainsubprogno		!index of main subprog (eg. may be before/after syslib)

!global const int maxtype=6'000
global const int maxtype=16'000

global int ntypes

global [0..maxtype]symbol	ttnamedef
global [0..maxtype]symbol	ttowner			!for ttlowerexpr/rtlengthexpr

global [0..maxtype]i32		ttbasetype		!basetype
global [0..maxtype]ichar	ttname

global [0..maxtype]u32		ttsize
global [0..maxtype]byte		ttsizeset
global [0..maxtype]i32		ttlower 		!.lbound (default 1)
global [0..maxtype]i32		ttlength 		!elements in array/record/tuple
global [0..maxtype]^[]i32	ttmult 			!ttlength elements in tuple

global [0..maxtype]unit		ttdimexpr		!length, lower:length, or lower..upper

global [0..maxtype]i32		tttarget 		!for array/^types
global [0..maxtype]i32		ttlineno

global [0..maxtype]byte		ttsigned		!is i8 i16 i32 i64
global [0..maxtype]byte		ttisreal		!is r32 r64
global [0..maxtype]byte		ttisinteger		!is i8..i64/u8..u64/c8..c64
global [0..maxtype]byte		ttisshort		!is i8/i16/i32/u8/u16/u32/c8/c16
global [0..maxtype]byte		ttisref			!is a pointer

!global const int maxtypename=4'000
global const int maxtypename=8'000
!global const int maxtypename=38'000
!global const int maxtypename=380'000

global [0..maxtypename]typenamerec typenames
global [0..maxtypename]posrec typenamepos
global int ntypenames

global [0..symbolnames.upb]byte typestarterset

global symbol currproc

global int headermode=0

!global symbol proclist,proclistx			!linked list of all procs
!global symbol staticlist,staticlistx		!linked list of all static
!global symbol constlist,constlistx		!linked list of all export consts

global unit nullunit

global const maxdllproc=1000

global int ndllproctable
global [maxdllproc]symbol dllproctable

global int fverbose=1		!1=normal, 0=less verbose, 2/3 = more verbose
global int fcodesize=0		!0/1/2/2 = quite/show code size/exe/all

global byte msyslevel=2		!0/1/2 = none/min/normal

global byte fshowtiming
!global byte fshowss
global byte fshowasm
global byte fshowpcl
global byte fshowc
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
global byte fdeconsma=0

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

!global const langhomedir	= "C:/mx/"
global const langhomedir	= "C:/bx/"
!global const langhomedir	= "C:/bx2/"

global const langhelpfile	= "mm_help.txt"

!global byte ctarget=0

global int pcltime
global int mcltime
global int sstime
global int exetime

global byte foptimise = 2

global byte fcff = 0

global int mmpos

global int mlabelno

global const reducelabels=1
!global const reducelabels=0

GLOBAL PCL PPP
=== mm_diags.m 0 0 35/74 ===
int currlineno
int currfileno

strbuffer sbuffer
^strbuffer dest=&sbuffer

strbuffer v
^strbuffer ds=&v

const tab1="\t"
const tab2="\t\t"

const tabstr="|--"


!const fshowsymbols=1
const fshowsymbols=0

global proc printst(filehandle f, ^strec p, int level=0)=
	^strec q

	printstrec(f, p, level)

	q:=p.deflist

	while q<>nil do
		printst(f, q, level+1)
		q:=q.nextdef
	end
end

proc printstrec(filehandle f, ^strec p, int level)=
	strec dd
	^byte q
	int col, offset, n
	const tabstr="    "
	[256]char str

	gs_init(ds)

	print @str, p
	dsstr(str)
	dsstr(" ")

	offset:=0
	to level do
		dsstr(tabstr)
		offset+:=4
	end
	dsstr(":")

	gs_leftstr(ds, p.name, 28-offset, '-')
	gs_leftstr(ds, namenames[p.nameid], 12, '.')

	col:=gs_getcol(ds)
	dd:=p^


	dsstr("[")
	dsstr(scopenames[p.scope])
	dsstr(" ")
	if p.isimport then
		dsstr("Imp ")
	end

	if dd.isstatic then
		dsstr("Stat")
	end

	if dd.nameid=paramid and dd.byref then
		dsstr("byref")
	end

	if dd.caligned then
		dsstr(" $caligned")
	end
	if dd.maxalign then
		dsstr(" maxalign:")
		dsint(dd.maxalign)
		dsstr(" ")
	end
	if dd.optional then
		dsstr("Opt ")
	end
	if dd.varparams then
		dsstr("Var:")
		dsint(dd.varparams)
		dsstr(" ")
	end

	if dd.moduleno then
		if dd.nameid<>subprogid then
			print @str, "Modno#",,dd.moduleno
		else
			print @str, "Subno#",,dd.subprogno
		end
		dsstr(str)
	end

	if dd.used then
		dsstr("U ")
	end

!	if dd.isthreaded then
!		dsstr("Threaded ")
!	end
!

	dsstr("]")
	gs_padto(ds, col+10, '=')

	if p.owner then
		fprint @str, "(#)", p.owner.name
		gs_leftstr(ds, str, 18, '-')
	else
		gs_leftstr(ds, "()", 18, '-')
	end

	case p.mode
	when tvoid then
		dsstr("Void ")
	else
		GS_STRINT(DS, P.MODE)
		GS_STR(DS, ":")

		dsstr(strmode(p.mode))
		dsstr(" ")
	end case

	case p.nameid
	when fieldid, paramid then
		dsstr(" Offset:")
		dsint(p.offset)
		if p.mode=tbitfield then
			dsstr(" Bitoffset:")
			dsint(p.bitoffset)
			dsstr(":")
			dsint(p.bitfieldwidth)
		end

		sprintf(str, "%.*s", int(p.uflags.ulength), &p.uflags.codes)
		print @str, p.uflags.ulength:"v", ichar(&p.uflags.codes):".*"
		dsstr(" UFLAGS:")
		dsstr(str)
		dsstr("-")
		dsint(p.uflags.ulength)

		if p.code then
			dsstr("/:=")
			gs_strvar(ds, strexpr(p.code))
		end

	when procid then

		dsstr("Index:")
		dsint(p.fnindex)

		dsstr(" Nret:")
		dsint(p.nretvalues)

	when dllprocid then
		dsstr("Index/PCaddr:")
		dsint(p.fnindex)
		if p.truename then
			dsstr(" Truename:")
			dsstr(p.truename)
		end

	when staticid then
		if p.code then
			dsstr("=")
			gs_strvar(ds, strexpr(p.code))
		end

	when frameid then
		if p.code then
			dsstr(":=")
			gs_strvar(ds, strexpr(p.code))
		end

	when constid then
		dsstr("Const:")
		gs_strvar(ds, strexpr(p.code))

!	when enumid then
!		dsstr("Enum:")
!		dsint(p.index)
!	when dllmoduleid then
!		dsstr("DLL#:")
!		dsint(p.dllindex)
	end case

	if p.atfield then
		dsstr(" @")
		dsstr(p.equivfield.name)
		dsstr(" +")
		dsint(p.equivoffset)
	end
	if p.equivvar then
		gs_strvar(ds, strexpr(p.equivvar))
	end

!dsstr(" Module# ")
!dsint(p.moduleno)
!
!	dsstr(" Lineno: ???")
!dsint(p.lineno iand 16777215)

	gs_println(ds, f)

	case p.nameid
	when constid, frameid, staticid, macroid then
		if p.code then
			printunit(p.code, dev:f)
		end
	end case
end

global proc printstflat(filehandle f)=
symbol p
println @f, "GLOBAL SYMBOL TABLE:"

for i:=0 to hashtable.upb-1 do
	p:=hashtable[i]
	if p=nil then nextloop end

!	IF P.NEXTDUPL=NIL THEN NEXTLOOP FI

	case p.symbol
	when namesym then
		println @f, i:"5", p, p.name, symbolnames[p.symbol],,":",,namenames[p.nameid]
		p:=p.nextdupl
		while p do
			print @f, "     ", p, p.name, symbolnames[p.symbol],,":",,namenames[p.nameid]
			if p.owner then
				fprint @f, " (From #:#)", p.owner.name, namenames[p.owner.nameid]
			end

			println @f

			p:=p.nextdupl
		end
	end case
end
end

global proc printcode(filehandle f, ichar caption)=
	symbol p

	p:=stlinear
	while p, p:=p.nextlinear do
		if p.nameid=procid then
			print @f, p.name,,"=", (p.scope|"Sub", "Prog", "Exp"|"Mod")
			if p.owner.nameid=typeid then
				print @f, " in record", p.owner.name
			end
			println @f
			printunit(p.code, 0, 1, dev:f)
			println @f
		end
	end
end

global proc printunit(^unitrec p, int level=0, number=0, filehandle dev=nil)=
!p is a tagrec
	^unitrec q
	^strec d
	int t
	ichar idname
	i64 a
	r32 x32
	static int cmpchain=0

	if p=nil then
		return
	end

	if p.pos then
		currlineno:=getlineno(p.pos)
		currfileno:=p.fileno
	end

!	print @dev, p, ":"
	print @dev, getprefix(level, number, p)

	idname:=jtagnames[p.tag]
	print @dev, idname,,": "

	case p.tag
	when jname then
		d:=p.def

		print @dev, d.name
!		print @dev, d.name, namenames[d.nameid]

!		if d.code then
!			print @dev, " {",,jtagnames[d.code.tag],,"}"
!		end

!		print @dev, " ",,getdottedname(d)!, q
!		print @dev, (p.dottedname|" {Dotted}"|"")

		if p.avcode then
			print @dev, " AV:", p.c:"c"
		end

!		print @dev, " Moduleno:", p.moduleno
!
!!		if p.avcode then print @dev, " AV:", char(p.avcode) end
!		if p.avcode then print @dev, " AV:", char(p.avcode), $ end

	when jlabeldef then
		println @dev, p.def.name, p.def.labelno

	when jconst then
		t:=p.mode
		a:=p.value
		if t=trefchar then
			if p.slength>256 then
				print @dev, """",,"1:(longSTR)", """ *",,p.slength
			elsif p.slength then
				print @dev, """",,p.svalue,,""" *",,p.slength
			else
				print @dev, """"""
			end

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
			end
		when tbool then
			print @dev, (p.value|"True"|"False")
		when tarray then
			print @dev, "<ARRAY>", =P.STRTYPE, =P.SLENGTH
		else
			println =typename(t), typename(ttbasetype[t])
			PRINT @DEV, "<PRINTUNIT BAD CONST PROBABLY VOID"
		end
		print @dev, " ",,typename(t)
		if p.isastring then
!			print @dev, " <isstr>"
			fprint @dev, " <isstr>(#)", p.strtype
		end

	when jtypeconst then
		print @dev, typename(p.value)

	when jbitfield then
		print @dev, bitfieldnames[p.bfcode]+3

	when jconvert, jtypepun, jtruncate, jfix, jfloat, jfwiden, jfwiden, jfnarrow then
		print @dev, " From mode:", strmode(p.oldmode)

	when jmakelist then
		print @dev, "Len:", p.length

	when jdot then
		print @dev, "Offset:", p.offset

	when jindex, jptr then

	when jexit, jredo, jnext then
		print @dev, "#",,p.loopindex

	when jsyscall then
		print @dev, sysfnnames[p.fnindex]+3

	when joperator then
		print @dev, pclnames[p.pclop]

!	when jmakeset then
	when jcmpchain then
		for i to p.cmpgenop.len do
			if p.cmpgenop[i]=0 then exit end
			print @dev, ccnames[p.cmpgenop[i]],," "
		end
	end case

	if p.isconst then
		print @dev, " Is const"
!	else
!		print @dev, " Not const"
	end

	case p.tag
	when jbin, jbinto, junary, junaryto, jincr then
		if p.pclop then
			fprint @dev, " <#>", pclnames[p.pclop]
			if p.pclop in [kmaths, kmaths2] then
				fprint @dev, " (#)", mathsnames[p.mathsop]
			end
		else
			fprint @dev, " no-op"
		end
	when jprop then
		fprint @dev, " Prop<#>", propnames[p.propcode]
	when jcmp then
		fprint @dev, " <#>", ccnames[p.condcode]
	end case


	println @dev

	for i to jsubs[p.tag] do
		printunitlist(dev, p.abc[i], level+1, i)
	end
end

proc printunitlist(filehandle dev, ^unitrec p, int level=0, number=0)=
	if p=nil then return end

	while p do
		printunit(p, level, number, dev)
		p:=p.nextunit
	end
end

func getprefix(int level, number, ^unitrec p)ichar=
!combine any lineno info with indent string, return string to be output at start of a line
	static [1024]char str
	[1024]char indentstr
	[16384]char modestr
	ichar isexpr

	indentstr[1]:=0
	if level>10 then level:=10 end

	to level do
		strcat(indentstr, tabstr)
	end

	isexpr:="S"
	if jisexpr[p.tag] then isexpr:="E" end

	case p.tag
	when jif, jswitch, jcase, jselect then
		if p.mode=tvoid then
			isexpr:="S"
		end
	end case

!	fprint @modestr, "# #:#", isexpr, (p.resultflag|"RES"|"---"), strmode(p.mode)
	fprint @modestr, "# #:#", isexpr, (p.resultflag|"RES"|"---"), strmode(p.mode)
	modestr[256]:=0

	strcat(modestr, "-----------------------------")
	modestr[17]:=' '
	modestr[18]:=0

	str[1]:=0
	strcpy(str, getlineinfok())
	strcat(str, modestr)
	strcat(str, indentstr)
	strcat(str, strint(number))
!	if prefix^ then
		strcat(str, " ")
!	end

	return str
end

func getlineinfok:ichar=			!GETLINEINFO
	static [40]char str

	fprint @str, "# # ", CURRFILENO:"Z2", currlineno:"z4"
	return str
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
			for i to ttlength[m] do print @f, strmode(ttmult[m, i]),," " end
			println @f
		end
		println @f, tab, "Signed:", ttsigned[m]
		println @f, tab, "Isreal:", ttisreal[m]
		println @f, tab, "Isinteger:", ttisinteger[m]
		println @f, tab, "Isshort:", ttisshort[m]
		println @f, tab, "Isref:", ttisref[m]
		println @f
	end
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
		end
		ps:=subprogs[moduletosub[i]]

			isfirst:=ps.firstmodule=i
			ismain:=ps.mainmodule=i

			if isfirst and ismain then s:="hm"
			elsif isfirst then s:="h "
			elsif ismain then s:="m "
			else s:="  " 
			end

			print @dev, tab, i:"2", s, 
			pm.name:"16jl", "Sys:", pm.issyslib, 
			"Sub:", subprogs[pm.subprogno].name, "Fileno:", pm.fileno

		if pm.stmacro then
			print @dev, " Alias:", pm.stmacro.name
		end
		if pm.stmain then
			print @dev, $, pm.stmain.name, ":", scopenames[pm.stmain.scope], pm.stmain
		end
		if pm.ststart then
			print @dev, $, pm.ststart.name, ":", scopenames[pm.ststart.scope], pm.ststart
		end

		println @dev
	end
	println @dev

	println @dev, "Subprograms", nsubprogs, =mainsubprogno
	for i to nsubprogs do
		ps:=subprogs[i]
		println @dev, tab, i, ps.name, "Sys:", ps.issyslib!, =PS.STSUBPROG

		if ps.firstmodule then
			print @dev, tab, tab
			for j:=ps.firstmodule to ps.lastmodule do
				print @dev, $, modules[j].name, "(", MODULES[J].STSUBPROG, ")"
			end
			println @dev
		end
	end
	println @dev

	println @dev, "Sourcefiles", nsourcefiles
	ifile pf
	for i to nsourcefiles do
		pf:=sources[i]
		fprintln @dev, "  #:  Name=# File=# Path=# Spec=# Size=#", 
			i:"2", pf.name:"jl16", pf.filename:"jl18", pf.path:"20jl", pf.filespec:"30jl", pf.size:"7"
	end
	println @dev

	println @dev, "Link files", nlibfiles
	for i to nlibfiles do
		println @dev, tab, libfiles[i]:"16jl"
	end
	println @dev
end

global proc showlogfile=
	[256]char str
	filehandle logdev
	int size
	^strbuffer ss

!CPL "SHOWLOG1"
	return unless fshowdiags

!CPL "SHOWLOG2"
	logdev:=fopen(logfile, "w")

	if fshowmodules then showprojectinfo(logdev) end

	if fshowasm and passlevel>=mcl_pass then
		println @logdev, "PROC ASSEMBLY"
		addtolog(changeext(outfile, "asm"), logdev)
	end

	if fshowpcl and passlevel>=pcl_pass then
		println @logdev, "PROC PCL"
		addtolog("PCL", logdev)
	end

	if fshowc and passlevel=clang_pass then
		println "PROC CLANG"
		addtolog(changeext(outfile, "c"), logdev)
	fi


	if fshowast3 and passlevel>=type_pass then addtolog("AST3", logdev) end
	if fshowast2 and passlevel>=name_pass then addtolog("AST2", logdev) end
	if fshowast1 and passlevel>=parse_pass then addtolog("AST1", logdev) end

	if fshowpst and passlevel>=pcl_pass then
		println @logdev, "PROC PST"
		addtolog("PST", logdev)
	end

	if fshowst then
		showsttree("SYMBOL TABLE", logdev)
	end
	if fshowstflat then
		showstflat("FLAT SYMBOL TABLE", logdev)
	end
!
	if fshowtypes then
		printmodelist(logdev)
	end

	size:=getfilesize(logdev)
	fclose(logdev)

	if size then
CPL "PRESS KEY..."; if OS_GETCH()=27 then stop end
		print @str, "\\m\\ed.bat ", logfile

		if checkfile("mm.m") then
			os_execwait(str, 0, nil)
		else
			println "Diagnostic outputs written to", logfile
		end
	end
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

	symbol d:=stlinear

	while d, d:=d.nextlinear do
		fprintln @f, "#	#.# (#) Mod:", d, d.owner.name, d.name:"20jl", namenames[d.nameid], 
			d.moduleno
	end
	println @f, "End\n"

	println @f, "DLL Proc List:"
	for i to ndllproctable do
		d:=dllproctable[i]
		fprintln @f, "#	#.# (#) Mod:", d, d.owner.name, d.name:"20jl", namenames[d.nameid], 
			d.moduleno
	end
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

global proc printsymbol(^tokenrec lp)=
	tokenrec l
	l:=lp^

	printf("%-18s", symbolnames[l.symbol])

	switch l.symbol
	when namesym then
		printstrn(l.symptr.name, l.symptr.namelen)

		if l.subcode then
			fprint " [#]", symbolnames[l.subcode]
		end

	when intconstsym then
		case l.subcode
		when tint then print l.value, "int"
		when tword then print l.uvalue, "word"
		else print l.value
		end case

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
		print symbolnames[l.symbol], =L.SUBCODE
!	elsif l.subcode then
!	ELSE
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
	showtime("PCL:", 		pcltime)
	showtime("MCL:", 		mcltime)
	showtime("SS:", 			sstime)
	showtime("EXE:", 		exetime)
	println "-----------------------------"
	showtime("Total:", 		compiletime)
end

proc dsstr(ichar s)=
	gs_str(ds, s)
end

proc dsint(int a)=
	gs_strint(ds, a)
end

=== mm_exportm.m 0 0 36/74 ===
strbuffer sbuffer
ref strbuffer dest=&sbuffer

const expscope=export_scope

global proc writeexports(ichar basefile, modulename)=
	ref strec d
	ref procrec pp
	filehandle f
	[300]char filespec
	ichar outfile
	
	strcpy(filespec,changeext(basefile,""))
	strcat(filespec,"_lib.m")
	outfile:=pcm_copyheapstring(filespec)

	println "Writing exports file to",outfile

	gs_init(dest)
	wxstr("importdll ")
	wxstr(modulename)
	wxstrln(" =")

	for i:=tuser to ntypes do
		d:=ttnamedef[i]
		if d.scope=export_scope and d.name^<>'$' then

			case ttbasetype[i]
			when trecord then
				exportrecord(d)
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
		d:=pp.def
		if d.scope=export_scope then
			exportstatic(d)
		fi
	od
	if staticlist then wxline() fi

	pp:=constlist
	while pp, pp:=pp.nextproc do
		d:=pp.def
		if d.scope=export_scope then
			exportconst(d)
		fi
	od
	if constlist then wxline() fi

	pp:=proclist
	while pp, pp:=pp.nextproc do
		d:=pp.def
		if d.scope=export_scope then
			exportproc(d)
		fi
	od

	wxstrln("end")

	f:=fopen(outfile,"wb")
	gs_println(dest,f)
	fclose(f)
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
	jevalx2(dest,d.code)
	wxline()
end

proc exportproc(ref strec d)=
	ref strec e
	int currmode,needcomma

	wxstr("    ")
	wxstr((d.mode=tvoid|"proc "|"func "))
	wxstr(d.name)
	wxstr("(")

	e:=d.deflist
	needcomma:=0
	currmode:=tvoid

	while e do
		if e.nameid=paramid then
			if needcomma then wxstr(",") fi
			if e.parammode<>byref_param then
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
				if ttisref[e.mode] and e.code.tag=jconst and e.code.value=0 then
					wxstr("nil")
				else
					jevalx2(dest,e.code)
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
=== mm_exportq.m 0 0 37/74 ===
strbuffer sbuffer
ref strbuffer dest=&sbuffer

const expscope=export_scope

global proc writeexports(ichar basefile, modulename)=
	ref strec d
	ref procrec pp
	filehandle f
	ichar outfile

	outfile:=pcm_copyheapstring(changeext(basefile,"q"))

	println "Writing exports file to",outfile

	gs_init(dest)
	for i:=tuser to ntypes do
		d:=ttnamedef[i]
		if d.scope=export_scope and d.name^<>'$' then

			case ttbasetype[i]
			when trecord then
				exportrecord(d)
			else
				wxstr("    global type ")
				wxstr(d.name)
				wxstr(" = ")
				wxstr(strmode(d.mode,0))
				wxline()
			esac
		fi
	od

	pp:=staticlist
	while pp, pp:=pp.nextproc do
		d:=pp.def
		if d.scope=export_scope then
!CPL =D.NAME
!			if not d.istabdata then gerror("Exporting statics to Q") fi
			exportstatic(d)
		fi
	od
	if staticlist then wxline() fi

	pp:=constlist
	while pp, pp:=pp.nextproc do
		d:=pp.def
		if d.scope=export_scope then
			exportconst(d)
		fi
	od
	if constlist then wxline() fi

!	wxstr((passlevel=dll_pass|"importdll "|"importlib "))
	wxstr("importdll ")
!	wxstr("importlib ")
	wxstr(modulename)
	wxstrln(" =")

	pp:=proclist
	while pp, pp:=pp.nextproc do
		d:=pp.def
		if d.scope=export_scope then
			exportproc(d)
		fi
	od

	wxstrln("end importdll")

	f:=fopen(outfile,"wb")
	gs_println(dest,f)
	fclose(f)
end

proc exportstatic(ref strec d)=
	unit p:=d.code, q
	[1024]char str

	return when d.code=nil

	return when p.tag<>jmakelist


!CPL "EXPORTS",P.LENGTH,"LOWER:",TTLOWER[P.MODE]
!PRINTUNIT(P)

	wxstr("global var ")
!	wxmode(d.mode)
!	wxstr(" ")
	wxstr(d.name)
	wxstr(" = (")
	wxstr(strint(ttlower[p.mode]))
	wxstrln(":\\")

	q:=p.a
	for i to p.length do
		wxstr("    ")
		case q.tag
		when jconst then
			case q.mode
			when ti64,tu8 then
				wxstr(strint(q.value))
			when trefchar then
				if strlen(q.svalue)>str.len/2 then gerror("expstr") fi
				wxstr("""")
				convertstring(q.svalue, str)
				wxstr(str)
				wxstr("""")
			ELSE
CPL =STRMODE(Q.MODE)
				GERROR("EXPLIST TYPE?")
			esac
			wxstrln(",")
		else
			gerror("explist?")
		esac
		q:=q.nextunit
	od
	wxstrln(")")
	wxline()
end

proc exportconst(ref strec d)=
	wxstr("global const ")
!	wxmode(d.mode)
!	wxstr(" ")
	wxstr(d.name)
	wxstr(" = ")
	jevalx2(dest,d.code)
	wxline()
end

proc exportproc(ref strec d)=
	ref strec e
	int currmode,needcomma

	wxstr("    ")
	wxstr((d.mode=tvoid|"proc "|"func "))
	wxstr(d.name)
	wxstr("(")

	e:=d.deflist
	needcomma:=0
	currmode:=tvoid

	while e do
		if e.nameid=paramid then
			if needcomma then wxstr(",") fi
			if e.parammode<>byref_param then
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
				if ttisref[e.mode] and e.code.tag=jconst and e.code.value=0 then
					wxstr("nil")
				else
					jevalx2(dest,e.code)
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

	wxstr("global type ")
	wxstr(d.name)
	wxstr(" = struct ")
	wxline()

	indent:=1

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

	wxstrln("end")
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
=== mm_genaux.m 0 0 38/74 ===
global int nsaveregs, nsavefregs		!number of integer/float non-vols to be saved
global int nspilled						!spilled int/float registers

global int framesize					!counts bytes in stack frame
!global int framebytes, frameoffset, paramoffset
global int paramstackoffset
global int paramspilloffset

symbol dblockarg

int nsavedregs, nsavedxregs
global int retindex

global proc do_proccode_a=
!	nextworkreg:=r0						!first 3 are volatile
!	nextworkxreg:=xr4					!first 2 are volatile
!
!	highworkreg:=nextworkreg			!assume workreg will be used
!	highworkxreg:=nextworkxreg
!
	retindex:=createfwdlabel()
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
	symbol d
	[100]char str, newname
	int r, n

!CPL $LINENO

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

	if ttbasetype[currfunc.mode]=tblock then	!need to inject extra parameter
		GERROR("PROCB/BLOCKRET")
!		dblockarg:=tc_makesymbol("$block", param_id)
!		dblockarg.nextparam:=currfunc.nextparam
!		dblockarg.mode:=tblock
!		dblockarg.size:=currfunc.size
!
!		currfunc.nextparam:=dblockarg
!		++currfunc.nparams
	fi
!CPL "PROC B2", =CURRFUNC.NAME, =CURRFUNC.NPARAMS, STRPMODE(CURRFUNC.MODE, CURRFUNC.SIZE)

!IF NSAVEDBYTES THEN
!	CPL "Saving:", nsavedregs, nsavedxregs,"in", currfunc.name
!FI

	paramoffset:=16+nsavedbytes		!between top of stackframe and 1st param is fp/retaddr/saved

	d:=currfunc.deflist
	while d, d:=d.nextdef do
		case d.nameid
		when paramid then
			d.offset:=paramoffset
			paramoffset+:=8
	!		if d.used then
				genmc_def(m_define, d)
!			elsif pcheckunusedlocals then
!				println "Unused param:", d.name, "in", currfunc.name
!			fi

		when frameid then
!			if not d.used then
!				if pcheckunusedlocals then
!					println "Unused local:", d.name, "in", currfunc.name
!				fi
!				nextloop
!			fi

			size:=stdsize[ttbasetype[d.mode]]
			if d.mode=tblock then
				size:=ttsize[d.mode]
			fi

			if d.equivvar then
				MERROR("PCODEB/@")
			else
				framesize+:=roundsizetg(size)
				d.offset:=-framesize
				genmc_def(m_define, d)
			fi
		esac
	od

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

	genmc(m_label, mgenlabel(retindex))

!	if dblockarg then
!!		MCOMMENT("BLOCK RETURN COPY NEEDED")
!!D0 has address of block to be copied
!!It needs to be returned on D0 after copying
!MCOMMENT("BLOCKRET1")
!
!		ax:=mgenireg(r0)
!		bx:=mgenreg(r1)
!		genmc(m_mov, bx, mgenmem(dblockarg))
!		nextworkreg:=r2
!		copyblock(mgenireg(r1), ax, dblockarg.size)		!does not affect r0
!		genmc(m_xchg,  mgenreg(r0), bx)
!
!MCOMMENT("BLOCKRET2")
!
!	fi

	popstack(framesize)
	genmc(m_pop, dframeopnd)
	restorevolregs(nsavedregs, nsavedxregs)

	genmc(m_ret)
end

proc spillparams=
	symbol d
	mclopnd ax
	int offset:=16, regoffset:=0, xregoffset, firstoffset

	regoffset:=0

	d:=currfunc.deflist

!	if currfunc.variadic then				!C proc def using ...
!		firstoffset:=d.offset				!param offsets may be pushed up
!
!		for i:=currfunc.nparams to 3 do				!0-based; if nparams=2, loops over 2..3 as 0..1 are normal
!			ax:=mgenindex(areg:rframe, size:8, offset:i*8+firstoffset)
!			genmc(m_mov, ax, mgenreg(i+r10))
!		od
!	fi
!
	while d, d:=d.nextdef do
		if d.nameid=paramid then
			if regoffset>3 then exit fi

			if d.used or regoffset=0 then
				ax:=mgenindex(areg:rframe, size:8, offset:d.offset)
				case d.mode
				when tr64 then
					genmc(m_movq, ax, mgenxreg(regoffset+xr0))
				when tr32 then
					genmc(m_movd, changeopndsize(ax,4), mgenxreg(regoffset+xr0))
				else
					genmc(m_mov, ax, mgenreg(regoffset+r10))
				esac
			fi

			offset+:=8
			++regoffset
		fi
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

global func createfwdlabel:int =
	return ++mlabelno
end

global func definelabel:int =
	pc_gen(klabel, mgenlabel(++mlabelno))
	return mlabelno
end

global proc definefwdlabel(int lab) =
	pc_gen(klabel, mgenlabel(lab))
end

global func mdefinelabel:int =
	genmc(m_label, mgenlabel(++mlabelno))
	return mlabelno
end

global proc mdefinefwdlabel(int lab) =
	genmc(m_label, mgenlabel(lab))
end

global func getpclmode(int t)int u=
	u:=stdpcl[ttbasetype[t]]

	if u=tblock then
		case ttsize[t]
		when 8 then u:=tu64
		when 4 then u:=tu32
		when 2 then u:=tu16
		when 1 then u:=tu8
		esac
	fi
	return u
end

global func addstr(ichar s, t)ichar=
	static [256]char str
	strcpy(str, s)
	strcat(str, t)
	str
end

=== mm_genmcl.m 0 0 39/74 ===
macro mdivider = mcomment("-"*40)
macro divider = pc_comment("-"*40)

global pcl pcldoswx

const maxnestedloops	= 50

global [maxnestedloops,4]int loopstack
global int loopindex							!current level of nested loop/switch blocks

unitrec zero_unit
global unit pzero=&zero_unit

INT PCLLENGTH
INT NPROCS

global proc genmcl=
	ref strbuffer asmstr
	ref procrec pp
	symbol d
	int tt

	return when mcldone

	tt:=clock()

!*!	inithandlers()
	mclinit()

	mcomment("Generated ASM")

	dolibfiles()

	pp:=staticlist
	while pp do
		d:=pp.def
!CPL "DOSTATIC",D, D.NAME
		dostaticvar(d)
		pp:=pp.nextproc
	od

	mcomment("")

!import list not needed for AA; only for NASM

!	for i to ndllproctable do
!		gendllproc(dllproctable[i])
!	od

	pp:=proclist
	while pp do
		d:=pp.def
		genprocmcl(d)
		pp:=pp.nextproc
	od

	genrealtable()
	genabsneg()
	genstringtable()

	genmc(m_endx)					!need as buffer in optimiser
	genmc(m_endx)

	if fpeephole then
!*!		peephole()
	fi

	mcldone:=1

	mcltime:=os_clock()-tt
!CPL $LINENO


end

global proc genpcl=
!generate pcl code for each function
	ref procrec pp
	symbol d
	int tt:=clock()

	pp:=proclist
	while pp do
		d:=pp.def
		genprocpcl(d)
!++NPROCS
!PCLLENGTH+:=D.PCLINFO.LENGTH

		pp:=pp.nextproc
	od

!CPL =NPROCS
!CPL =PCLLENGTH
!CPL real (PCLLENGTH)/nprocs
!CPL

	pcltime:=clock()-tt
end

proc start=
	zero_unit.tag:=jconst
	zero_unit.mode:=ti64
	zero_unit.value:=0
	zero_unit.resultflag:=1
end

proc dolibfiles =
	mcomment("Lib files go here")
	mcomment("")
end

proc dostaticvar(symbol d) =

	return when d.isimport or d.equivvar

	if d.scope = program_scope and d.name^='$' then
		if eqstring(d.name,"$cmdskip") then
			d.scope:=export_scope				!export from mlib subprog
		fi
	fi

	setsegment((d.code|'I'|'Z'), getalignment(d.mode))
	genmc(m_labelname, mgenmemaddr(d))

	if d.code then
		genidata(d.code)
	else
		genmc(m_resb, mgenint(ttsize[d.mode]))
	fi
end

proc genidata(unit p)=
	[2000]byte data
	int t, tbase, offset
	byte allbytes, nbytes
	unit q, a
	symbol d
	ref char s
	mclopnd dx

	t:=p.mode
!CPL "GENIDATA",JTAGNAMES[P.TAG], STRMODE(T)
!PRINTUNIT(P)

	mmpos:=p.pos
	tbase:=ttbasetype[t]
	a:=p.a
!RETURN

	case p.tag
	when jconst then
		case tbase
		when tref then				!ref or string
			if t=trefchar and p.isastring then
				gerror("idata/empty str?") unless p.svalue
				if p.strtype='B' then gerror("1:B-str?") fi
				genmc(m_dq, mgenlabel(getstringindex(p.svalue)))
			else
				gendataint(p.value)
			fi

		when tr64 then
			genmc(m_dq, mgenrealimm(p.xvalue, tr64))

		when tr32 then
			genmc(m_dd, mgenrealimm(p.xvalue, tr32))

		when tarray then			!should be data string
			if p.strtype=0 then gerror("idata/array/not blockdata") fi
			doblockdata(p.svalue, p.slength)

		else						!assume integer
			gendataint(p.value, t)

		esac

	when jmakelist then
		q:=a

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
			doblockdata(cast(&data), nbytes)
		else
			q:=a
			while q, q:=q.nextunit do
				genidata(q)
			od
		fi

	when jname then
		d:=p.def
		offset:=0
doname:
		case d.nameid
		when staticid, procid, dllprocid then
			dx:=applyoffset(mgenmemaddr(d), offset)

		when labelid then
			if d.index=0 then d.index:=++mlabelno fi
			dx:=mgenlabel(d.index)
		else

			MCOMMENT("Idata &frameXXX")
!			gerror("Idata &frameXXX")
		esac
		genmc(m_dq, dx)

	when jconvert then
		genidata(p.a)

	when jshorten then
		gendataint(a.value, t)

	when jaddrof then
		if a.tag<>jname then recase else fi
		offset:=0
		if p.b then offset:=p.b.value fi
		d:=a.def
		doname

	else
		gerror_s("IDATA: ", jtagnames[p.tag], p)

	esac
end

proc gendataint(int a, mode=ti64)=
	static []byte opctable =(m_db, m_dw, 0,0, m_dd, 0,0, m_dq)

	genmc(opctable[ttsize[mode]], mgenint(a))
end

!proc doblockdata(unit p) =
proc doblockdata(ref byte s, int n) =
!p contains jconst with array type; treat as data-string

	ref u64 d:=cast(s)
	int nwords, r

	return when n=0

	nwords:=n/8

	to nwords do
		genmc(m_dq, mgenint(d++^))
	od

	r:=n-nwords*8
	if r then
		s:=cast(d)
		to r do
			genmc(m_db, mgenint(s++^))
		od
	fi
	MCOMMENT("ENDDATA")

end

proc genprocmcl(symbol d) =

	currfunc:=d
	setsegment('C',1)

	genmc(m_procstart, mgenmemaddr(currfunc))
	genmc(m_labelname, mgenmemaddr(currfunc))

	mcomment("?>>")
	mclprocentry:=mccodex

	do_proccode_a()						!initialise offsets etc

	mcomment("?>>")
	mclprocentry:=mccodex

	mdivider()

	MCOMMENT("CONVERT PCL BODY TO MCL")
!*!	pcltomcl(d)

	if mclprocentry=mccodex then		!empty body: add dummy mcl op
		mcomment("---")					!injection of entry code goes wrong otherwise
	fi

	mdivider()

	do_proccode_b()						!do entry code (inject into start)
	do_proccode_c()						!do exit code

	genmc(m_procend)

	currfunc:=nil

end

proc genprocpcl (symbol p) =
	imodule ms
	byte ismain:=0

	ms:=modules[p.moduleno]
	pcldoswx:=nil
!	nblocktemps:=0

!-----------------
	currfunc:=p
	pcl_start()
	mmpos:=p.pos


	if p=ms.stmain and moduletosub[p.moduleno]=mainsubprogno then
		ismain:=1
		entryproc:=p
!		p.isentry:=1
		genmain(p)

	elsif p=ms.ststart then
		genstart(p)
	fi
!------------------


	retindex:=createfwdlabel()

	divider()

	if p.hasdoswx then
		pc_gen(kinitdswx)			!this op needed by C?
		pcldoswx:=pccurr			!code to be injected later after this instr
	fi

!	pc_comment("<EVALBLOCk>")

!CPL "PROC",P.NAME, P.OWNER.NAME, P.CODE
	evalunit(p.code)
!CPL "PROC",$LINENO

	if ismain then
		genpushint(0)
		pc_gen(kstop)
		pc_setmode(ti64)
	fi

	divider()
	definefwdlabel(retindex)
	genreturn()

	p.pclinfo.code:=pcl_end()
	p.pclinfo.length:=pclength
!	CPL P.NAME:"JL18", p.pclinfo.length:"5"

!	CPL P.PCLINFO.LENGTH:"5", P.NAME:"JL18"

!	scanpclstack(p, p.pclinfo.code)


end

proc genmain(symbol p)=
	symbol d
	for i to nsubprogs when i<>mainsubprogno do
		d:=modules[subprogs[i].mainmodule].ststart
		docallproc(d)
	od
	d:=modules[subprogs[mainsubprogno].mainmodule].ststart
	docallproc(d)

	entryproc:=p
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

proc docallproc(symbol d)=
!call a simple proc, eg. start(), with no args
	return unless d
	pc_gen(ksetcall)
	pc_setnargs(0)

	pc_gen(kcallp, genmemaddr_d(d))
end

global proc genreturn=
!assume returning from currproc
	case currproc.nretvalues
	when 0 then
		pc_gen(kretproc)
	when 1 then
		pc_gen(kretfn)
		pc_setmode(currproc.mode)

	else
		pc_genx(kretfn, currproc.nretvalues)
	esac
end

global func genmem_u(unit p)mclopnd=
	return mgenmem(p.def)
end

global func genmem_d(symbol d)mclopnd=
	return mgenmem(d)
end

global proc genpushmem_d(symbol d)=
	pc_gen(kload, mgenmem(d))
end

global func genmemaddr_d(symbol d)mclopnd=
	return mgenmemaddr(d)
end

global proc genpushmemaddr_d(symbol d)=
	pc_gen(kload, mgenmemaddr(d))
end

global proc genpushint(int a)=
	pc_gen(kload, mgenint(a))
	pc_setmode(ti64)
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

global proc genpushreal(real x, int mode)=
	pc_gen(kload, mgenrealmem(x, ttsize[mode]))
	pc_setmode(mode)
end

global proc genpushstring(ichar s)=
	pc_gen(kload, mgenstring(s))
	pc_setmode(tu64)
end

global proc stacklooplabels(int a,b,c)=
!don't check for loop depth as that has been done during parsing
	++loopindex
	if loopindex>maxnestedloops then
		gerror("Too many nested loops")
	fi

	loopstack[loopindex,1]:=a
	loopstack[loopindex,2]:=b
	loopstack[loopindex,3]:=c

end

global func findlooplabel(int k,n)int=
!k is 1,2,3 for label A,B,C
!n is a 1,2,3, according to loop nesting index
	int i

	i:=loopindex-(n-1)		!point to entry
	if i<1 or i>loopindex then gerror("Bad loop index") fi
	return loopstack[i,k]
end

global proc genpc_sysfn(int fnindex, unit a=nil,b=nil,c=nil)=
	genpc_sysproc(fnindex, a,b,c, 1)
end

global proc genpc_sysproc(int fnindex, unit a=nil,b=nil,c=nil, int asfunc=0)=
	int nargs:=0, opc
	symbol d
	pcl p
	opc:=0

	pc_gen(ksetcall)
	p:=pccurr

	pushsysarg(c, 3, nargs)
	pushsysarg(b, 2, nargs)
	pushsysarg(a, 1, nargs)
!
	p.nargs:=nargs

	d:=getsysfnhandler(fnindex)
	if d then
		pc_gen((asfunc|kcallf|kcallp), mgenmemaddr(d))
		pc_setnargs(nargs)
	else
!		pc_gen((asfunc|kcallf|kcallp), gennameaddr(sysfnnames[fnindex]+3))
		pc_gen((asfunc|kcallf|kcallp), mgenname(sysfnnames[fnindex]+3))
	fi
	pccurr.nargs:=nargs
end

global proc pushsysarg(unit p, int n, &nargs) =
!return 0 or 1 args pushed
	if p then
		evalunit(p)
		pc_gen(ksetarg)
		pc_setmode_u(p)
		pccurr.x:=n
		pccurr.y:=n			!ASSUMES ALL INTS; however this only important
							!for arm64, and only matters if more than 8 args
		++nargs
	fi
end

global func getsysfnhandler(int fn)symbol p=
	[300]char str
	int report

	if sysfnhandlers[fn] then
		return sysfnhandlers[fn]
	fi

	strcpy(str,"m$")
	strcat(str,sysfnnames[fn]+3)	!"sf_stop" => "m$stop"

	ref procrec pp:=proclist
	while pp, pp:=pp.nextproc do
		if eqstring(pp.def.name, str) then
			sysfnhandlers[fn]:=pp.def
			return pp.def
		fi
	od

!	report:=passlevel>asm_pass
	report:=1
	report:=0

	if report then
		println "Sysfn not found:",str
	fi
	if fn<>sf_unimpl then
		p:=getsysfnhandler(sf_unimpl)
		if p=nil and report then
			gerror("No m$unimpl")
		fi
		return p
	fi

	return nil
end

global proc setfunctab=
	if pnprocs=nil then
		pnprocs:=makesymbol("$nprocs", staticid)
!CPL "SET PNPROCS", PNPROCS
		pnprocs.mode:=tpi64
		pprocname:=makesymbol("$procname", staticid)
		pprocaddr:=makesymbol("$procaddr", staticid)
	fi
end

global func makesymbol(ichar s, int id)symbol d=
	d:=newstrec()
	d.name:=pcm_copyheapstring(s)

	d.nameid:=id

	addstatic(d)
	d
end

!proc scanpclstack(symbol d, pcl p)=
!!scan pcl sequence from a function d, and set the stack index for each
!	int sp:=0				!start with empty stack
!	int a, b				!popped and pushed stack opnds for each opcode
!	int opcode
!	int mxlevel:=0, spstart
!
!	while p, p:=p.next do
!		opcode:=p.opcode
!		a:=pclpop[opcode]
!		b:=pclpush[opcode]
!
!		if a=9 or b=9 then		!needs special analysis
!			case opcode
!			when kstartmx then
!				if mxlevel then cpl "Nested startmx",currfunc.name fi
!				++mxlevel
!				spstart:=sp
!			when kresetmx then
!				sp:=spstart
!			when kendmx then
!				--mxlevel
!			when kcallp, kcallf then
!				sp:=sp-p.nargs+b
!			when kicallp, kicallf then
!				sp:=sp-p.nargs-1+b
!!			when kjumpret then
!!				a:=b:=0
!			when kjumpcc then
!				sp:=sp-(p.popone|1|2)
!			ELSE
!				CPL "CAN'T DO",PCLNAMES[OPCODE]
!			esac
!		else
!			sp:=sp-a+b
!		fi
!		p.spindex:=sp
!	od
!
!	if sp then
!		println d.name," Stack not empty:", sp
!	fi
!
!end
=== mm_genpcl.m 0 0 40/74 ===
global const maxnestedloops	= 50

global [maxnestedloops, 4]int loopstack
global int loopindex							!current level of nested loop/switch blocks

!global psymbol pnprocs, pprocname, pprocaddr

unitrec zero_unit
global unit pzero=&zero_unit

global int retindex
global pcl pcldoswx

macro divider = pcomment("-"*40)

!PROC EVALUNIT(unit p)=
!	CPL "TEMP EVALUNIT"
!END


global proc genpcl=
!CPL "GENPCL"
	symbol d
	psymbol p

	int tt:=clock()

	dolibs()

	d:=stlinear
	while d, d:=d.nextlinear do
!CPL "GENPCL", D.NAME
		case d.nameid
		when staticid then
			p:=getpsymbol(d)
			if not d.equivvar then
				paddsymbol(p)
				dostaticvar(d, p)
			fi

		when procid then
			p:=getpsymbol(d)
			paddsymbol(p)

			addlocals(d, p)
			doprocdef(d, p)

		when dllprocid then
			p:=getpsymbol(d)
			paddsymbol(p)

		end
	end

	dofunctables()

	pcltime:=clock()-tt

!CPL "DONE"
end

proc dostaticvar(symbol d, psymbol p)=

	if d.isimport then return fi

	if d.scope = program_scope and d.name^='$' then
		if eqstring(d.name, "$cmdskip") then
			d.scope:=export_scope				!export from mlib subprog
			p.isexport:=1
		fi
	fi

	return unless d.code

!init data exists; generate kdata PCL sequence

	pcl_start()

	genidata(d.code)

	p.pccode:=pcl_end()


!	if d.atvar=1 then
!		return
!	elsif d.code then
!		pgen(kistatic, genmem_d(d))
!		setmode(d.mode)
!		psetalign(getalignment(d.mode))
!		genidata(d.code)
!	else
!dozstatic:
!		pgen(kzstatic, genmem_d(d))
!		setmode(d.mode)
!		psetalign(getalignment(d.mode))
!	fi

end

proc addlocals(symbol d, psymbol p)=
	symbol e
	psymbol q
	int nlocals:=0, nparams:=0
	[pmaxparams]psymbol params
	[100]psymbol locals

	e:=d.deflist
	while e, e:=e.nextdef do
!CPL =E.NAME
		case e.nameid
		when paramid then
			params[++nparams]:=getpsymbol(e)

		when frameid then
			NEXTLOOP WHEN E.EQUIVVAR

			if nlocals>=locals.len then gerror("Too many locals") fi
			locals[++nlocals]:=getpsymbol(e)

!		when staticid then
!CPL "LOCAL/STATIC", e.name
!!			q:=getpsymbol(e)
!CPL "LS2"
!!			paddsymbol(q)
!CPL "LS3"
!		else
		end
	end

	if nparams then
		p.nextparam:=q:=params[1]
		for i in 2..nparams do
			q.nextparam:=params[i]
			q:=params[i]
		end
	fi

!CPL "ADDLOCALS", D.NAME, P.NAME
	if nlocals then
		p.nextlocal:=q:=locals[1]
		for i in 2..nlocals do
			q.nextlocal:=locals[i]
			q:=locals[i]
		end
	fi
end

global func getpsymbol(symbol d)psymbol p =
	symbol e
	[256]char str
	[16]symbol chain
	int n

	return nil when d=nil

	return d.pdef when d.pdef

!CPL "GETPS", D.NAME

	if d.equivvar then
		getpsymbol(e:=getequivdef(d))
		d.pdef:=e.pdef
		return e.pdef
	fi

	case d.nameid
	when frameid, paramid then
		strcpy(str, d.name)
	elsif d.isimport then
		strcpy(str, (d.truename|d.truename|d.name))

	else
		e:=d
		n:=0
		repeat
			chain[++n]:=e
			e:=e.owner
		until e=nil or e.nameid=programid

		strcpy(str, chain[n].name)
		for i:=n-1 downto 1 do
			strcat(str, ".")
			if chain[i].truename then
				strcat(str, chain[i].truename)
			else
				strcat(str, chain[i].name)
			fi
		end
	end

	d.pdef:=p:=pmakesymbol(str, d.nameid)

	p.mode:=getpclmode(d.mode)
	p.size:=ttsize[d.mode]

	if d.scope=export_scope then p.isexport:=1 fi
	if d.nameid in [dllprocid, dllvarid] then p.isimport:=1 fi
	p.used:=d.used
!	p.labelno:=d.index
	p.ishandler:=d.ishandler
!	p.isthreaded:=d.isthreaded
!
	p.varparams:=d.varparams
	return p
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

global proc setmode(int mode)=
	psetmode(getpclmode(mode), ttsize[mode])
end

global proc setmode2(int mode)=
	psetmode2(getpclmode(mode))
end

global proc setmode_u(unit p)=
	int mode:=p.mode
	psetmode(getpclmode(mode), ttsize[mode])
end

global proc setmodeint=
	psetmode(ti64)
end

global proc setmodeword=
	psetmode(tu64)
end

global proc genpushint(int a)=
	pgen(kload, pgenint(a))
!	setmode(ti64)
end

global proc genpushword(int a)=
	pgen(kload, pgenint(a))
	setmode(tu64)
end

global proc genpushreal(real x, int mode)=
	pgen(kload, pgenrealmem(x))
	setmode(mode)
end

global func genmem_u(unit p)pcl=
	return pgenmem(getpsymbol(p.def))
end

global func genmem_d(symbol d)pcl=
	return pgenmem(getpsymbol(d))
end

global func genmemaddr_d(symbol d)pcl=
	return pgenmemaddr(getpsymbol(d))
end

global proc genpushmem_d(symbol d)=
	pgen(kload, pgenmem(getpsymbol(d)))
end

global proc genpushmemaddr_d(symbol d)=
	pgen(kload, pgenmemaddr(getpsymbol(d)))
end

global proc genpushstring(ichar s, int length)=
	pgen(kload, pgenstring(s, length))
	setmode(tu64)
end

proc genidata(unit p)=
	[2000]byte data
	int t, tbase, offset, nbytes
	byte allbytes
	unit q, a
	symbol d
	^char s

	t:=p.mode

!CPL "GENIDATA", JTAGNAMES[P.TAG], STRMODE(T)
	mmpos:=p.pos
	tbase:=ttbasetype[t]
	a:=p.a

	case p.tag
	when jconst then
		if ttisref[t] then
			if t=trefchar then
				if p.svalue then
!CPL "GID/CONST1", P.SVALUE, p.strtype
					if p.strtype='B' then gerror("1:B-str?") fi
					pgen(kdata, pgenstring(p.svalue, p.slength))
				else
					pgen(kdata, pgenint(0))
				fi
			else
				pgen(kdata, pgenint(p.value))
			fi
			setmode(ti64)
		elsif ttisreal[t] then
!			pgen(kdata, pgenrealimm(p.xvalue, getpclmode(t)))
			pgen(kdata, pgenrealmem(p.xvalue))
			setmode(t)

		elsif ttbasetype[t]=tarray then
			IF P.STRTYPE=0 THEN GERROR("IDATA/ARRAY/NOT BLOCKDATA") FI
!CPL "GID/CONST2", P.SVALUE, p.strtype
			pgen(kdata, pgendata(p.svalue, p.slength))		!sets block mode

		else						!assume int/word
			pgen(kdata, pgenint(p.value))
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
			pgen(kdata, pgendata(pcm_copyheapstringn(cast(&data), nbytes), nbytes))
		else
			q:=p.a
			while q, q:=q.nextunit do
				genidata(q)
			end
		fi

	when jname then
		d:=p.def
		offset:=0
doname:
		case d.nameid
		when staticid, procid, dllprocid then
			pgen(kdata, genmemaddr_d(d))
			if offset then
				psetscaleoff(1, offset)
			fi
!			if am='P' then
!				setmode(tu64)
!			else
				setmode(t)
!			fi
		when labelid then
			if d.labelno=0 then d.labelno:=++mlabelno fi
			pgen(kdata, pgenlabel(d.labelno))
			setmode(ti64)
		else
			gerror("Idata &frameXXX")
		esac
		return
	when jconvert then
		genidata(p.a)
	when jshorten then
		pgen(kdata, pgenint(p.a.value))
		setmode(t)

	when jaddrof then
		if a.tag<>jname then recase else end
		offset:=0
		if p.b then offset:=p.b.value end
		d:=a.def
		t:=tu64
		doname
	else
		gerror_s("IDATA: ", jtagnames[p.tag], p)

	esac
end

proc doprocdef(symbol d, psymbol p) =
	imodule ms
	byte ismain:=0

	ms:=modules[d.moduleno]
	pcldoswx:=nil

!-----------------
	currfunc:=p
	pcl_start()
	mmpos:=d.pos

	if d=ms.stmain and moduletosub[d.moduleno]=mainsubprogno then
		ismain:=1
		entryproc:=p
		p.isentry:=1
		genmain(d)

	elsif d=ms.ststart then
		genstart(d)
	fi
!------------------


	retindex:=createfwdlabel()

	divider()

	if d.hasdoswx then
!		pgen(kinitdswx)			!this op needed by C?
		pcldoswx:=pccurr			!code to be injected later after this instr
	fi

!	pcomment("<EVALBLOCk>")

	evalunit(d.code)
!CPL "PROC",$LINENO

	if ismain then
		gencallimp("exit", 0, pzero)
	fi

	divider()
	definefwdlabel(retindex)
	genreturn(d)

	p.pccode:=pcl_end()

!CPL "DONE genpcl", P.NAME, P.CODE, =p

end

proc genmain(symbol d)=
	symbol e
	for i to nsubprogs when i<>mainsubprogno do
		e:=modules[subprogs[i].mainmodule].ststart
		docallproc(e)
	end
	d:=modules[subprogs[mainsubprogno].mainmodule].ststart
	docallproc(d)
end

proc genstart(symbol d)=
	symbol e
	int lead:=0, m,s

	m:=d.moduleno
	s:=d.subprogno

	if s=mainsubprogno and d.moduleno=subprogs[s].mainmodule then
		LEAD:=1
	elsif d.moduleno=subprogs[s].firstmodule then
		LEAD:=2
	fi

	if lead then
		for i to nmodules when moduletosub[i]=s and i<>m do
			e:=modules[i].ststart
			docallproc(e)
		end
	fi
end

proc docallproc(symbol d)=
!call a simple proc, eg. start(), with no args
	return unless d
	pgen(ksetcall)
	psetnargs(0)

	pgen(kcallp, genmemaddr_d(d))
end

global proc genreturn(symbol d)=
!assume returning from currproc
!CPL "GENRET", CURRPROC.NRETVALUES, CURRPROC.NAME

	case d.nretvalues
	when 0 then
		pgen(kretproc)
	when 1 then
		pgen(kretfn)
		setmode(d.mode)

	else
		pgenx(kretfn, d.nretvalues)
	esac
end

global proc gencallsysfn(int fnindex, unit a=nil,b=nil,c=nil)=
	gencallsysproc(fnindex, a,b,c, 1)
end

global proc gencallsysproc(int fnindex, unit a=nil,b=nil,c=nil, int isfunc=0)=
	[128]char str
!	symbol d
	psymbol p

	if sysfnhandlers[fnindex] then
		p:=sysfnhandlers[fnindex]
	else
		strcpy(str,"m$")
		strcat(str,sysfnnames[fnindex]+3)	!"sf_stop" => "m$stop"

!		p:=findsyslibfunc(sysfnnames[fnindex]+3, passlevel>asm_pass)
		p:=findsyslibfunc(str, 0)

		if p=nil then gerror_s("No sysfn:", str) end
		sysfnhandlers[fnindex]:=p
	fi

	gencallaux(p, isfunc, a, b, c)
end

global proc pushsysarg(unit p, int n, &nargs) =
!return 0 or 1 args pushed
	if p then
		evalunit(p)
		pgen(ksetarg)
		setmode_u(p)
		pccurr.x:=n
		pccurr.y:=n			!ASSUMES ALL INTS; however this only important
							!for arm64, and only matters if more than 8 args
		++nargs
	end
end

global proc gencallimp(ichar name, int isfn=0, unit a=nil, b=nil)=
	gencallaux(pmakeextname(name), isfn, a, b)

end

global proc gencallaux(psymbol d, int isfn=0, unit a=nil, b=nil, c=nil)=
	int nargs:=0, opc
	pcl p
	opc:=0

	pgen(ksetcall)
	p:=pccurr

	pushsysarg(c, 3, nargs)
	pushsysarg(b, 2, nargs)
	pushsysarg(a, 1, nargs)
!
	p.nargs:=nargs

	pgen((isfn|kcallf|kcallp), pgenmemaddr(d))

	pccurr.nargs:=nargs
end

global func findsyslibfunc(ichar name, int report=0)psymbol p=
!Look for internal function (within program being compiled, which
!is normally within the syslib usually compiled with it), and return strec
!If not found, create an imported version with the same name, unless report=1
	symbol d

	d:=stlinear
	while d, d:=d.nextlinear do
		if d.nameid=procid then
			if eqstring(d.name, name) then
				return getpsymbol(d)
			end
		end
	end

	if not report then
		return pmakeextname(name)
	end

	return nil
end

proc dolibs=
	for i to nlibfiles when libfiles[i]^<>'$' do
		pc_addplib(libfiles[i])
	end
end

proc start=
	zero_unit.tag:=jconst
	zero_unit.mode:=ti64
	zero_unit.value:=0
	zero_unit.resultflag:=1
end

global proc stacklooplabels(int a,b,c)=
!don't check for loop depth as that has been done during parsing
	++loopindex
	if loopindex>maxnestedloops then
		gerror("Too many nested loops")
	end

	loopstack[loopindex,1]:=a
	loopstack[loopindex,2]:=b
	loopstack[loopindex,3]:=c

end

global func findlooplabel(int k,n)int=
!k is 1,2,3 for label A,B,C
!n is a 1,2,3, according to loop nesting index
	int i

	i:=loopindex-(n-1)		!point to entry
	if i<1 or i>loopindex then gerror("Bad loop index") end
	return loopstack[i,k]
end

=== mm_lex.m 0 0 41/74 ===
macro hashc(hsum,c)=hsum<<4-hsum+c
!macro hashw(hsum)=(hsum<<5-hsum)
macro hashw(hsum)=hsum

const maxstackdepth=20
[maxstackdepth]^char lxstart_stack
[maxstackdepth]^char lxsource_stack
[maxstackdepth]^char lxsptr_stack
[maxstackdepth]int lxfileno_stack
[maxstackdepth]tokenrec lxnextlx_stack
[maxstackdepth]byte lximport_stack
global int sourcelevel=0
global int lximport

const cr	= 13
!const lf	= 10
const lf	= '\n'
const tab	= 9

^char lxsource
^char lxstart
^char lxsptr
!int lxifcond

int lxfileno
global const hstsize	= 65536
!global const hstsize	= 65536*4
global const hstmask	= hstsize-1

global [0:hstsize]symbol hashtable
[0..255]byte namemap			!0/1/2 = other/name/name-upper

ichar u64maxstr="18446744073709551615"

global proc lex=
	int lena,lenb
	^char p

	lx:=nextlx				!grab that already read basic token
	lx.sourceoffset:=lxstart-lxsource

	docase lexreadtoken(); nextlx.symbol
	when eolsym then
		unless keepeol[lx.symbol] then
			nextlx.symbol:=semisym
			nextlx.subcode:=1
			exit
		end

	when kincludesym then
		doinclude()

	when namesym then
		case nextlx.subcode
		when unitnamesym then
			case lx.symbol
			when intconstsym then
				case nextlx.symptr.lexindex
				when million_unit then lx.value *:= 1 million
				when billion_unit then lx.value *:= 1 billion
				else
					lxerror("Can't do this unit index")
				end case
				lx.subcode:=setinttype(lx.value)
			when realconstsym then
				lxerror("Unit suffix after float not implem")
			else
				nextlx.symbol:=namesym
				exit
			end case

		else
			nextlx.symbol:=namesym
			exit
		end case

	when rawxnamesym then
		nextlx.symbol:=namesym
		exit

	when insym then
		if lx.symbol=notlsym then
			lx.symbol:=notinsym
			lx.subcode:=1
		else
			exit
		end

	else
		exit
	end docase

	nextlx.fileno:=lxfileno

end

global proc lexreadtoken=
!read next token into nextlx
	int c,hsum
	^char sptr, lxsvalue
	int length,commentseen
	^char p, q
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
			if lxsvalue+1=^char(lxsptr) then
				case c:=toupper(lxsvalue^)
				when  'F','R' then 
					readrawstring()
					return
				when  'S','B','A' then 
					readarraystring(c)
					return
				end case
			end
		end

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
			end case
		else
			--lxsptr
			readdec()
		end case
		return

	when '!','#' then			!comment to eol
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

!	when '#' then
!		nextlx.symbol:=hashsym
!		return

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
			end
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
			end
			return
		elsif lxsptr^ in '0'..'9' then			!real const: deal with this after the switch
			--lxsptr
LXERROR(".123 not done")
!			readrealnumber(nil,0,10)
			return
		else
			nextlx.symbol:=dotsym
			return
		end case

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
		end case
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
!		end
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
!		end
		return

!	when '?' then
!		p:=str; q:=lxsptr+1
!		while q^ not in [cr, lf, 0] do
!			p++^:=q++^
!		end
!		p^:=0
!
!		nextlx.svalue:=pcm_copyheapstring(str)
!		nextlx.symbol:=questionsym
!		return
!

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
		end
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
		end case
		return

	when '*' then
		if lxsptr^='*' then
			++lxsptr
			nextlx.symbol:=powersym
		else
			nextlx.symbol:=mulsym
		end
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
		end case
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
		end case
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
		end case
		return

	when '&' then
		nextlx.symbol:=addrsym
		nextlx.subcode:=jaddrof
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
		end

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
	end
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
		end
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

proc lookup(^char name, int length, hashindex)=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!in either case, lx.symptr set to entry where name was found, or will be stored in
	int wrapped, j
	symbol d

	j:=hashindex iand hstmask

	d:=hashtable[j]
	wrapped:=0

	do
		if d=nil then exit end

		if d.namelen=length and memcmp(d.name,name,length)=0 then	!match
			nextlx.symptr:=d
			nextlx.symbol:=d.symbol
			nextlx.subcode:=d.subcode
			return
		end

		if ++j>=hstsize then
			if wrapped then
				abortprogram("HASHTABLE FULL")
			end
			wrapped:=1
			j:=0
		end
		d:=hashtable[j]
	end

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

func lookupsys(^char name)int=
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
		end

		if ++j>=hstsize then
			if wrapped then
				abortprogram("SYS:HASHTABLE FULL")
			end
			wrapped:=1
			j:=0
		end
		lx.symptr:=hashtable[j]
	end

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

	if s^=0 then return 0 end

	hsum:=s++^

	do
		c:=s++^
		exit when c=0
		hsum:=hashc(hsum,c)
	end
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
			lx.symptr.lexindex:=stsubcodes[i]
			lx.symptr.subcode:=stsymbols[i]
			lx.symptr.symbol:=namesym		!masquerades as normal identifier
		else
			lx.symptr.subcode:=stsubcodes[i]
		end case
	end
end

proc doinclude=
	ichar file
	ifile pf

	lexreadtoken()
	if nextlx.symbol<>stringconstsym then lxerror("include: string expected") end
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

global func addnamestr(ichar name)^strec=
	tokenrec oldlx
	^strec symptr

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
	end
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
	end
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
	end
end

func setinttype(u64 a)int=
	if a<=u64(0x7FFF'FFFF'FFFF'FFFF) then
		return ti64
	else
		return tu64
	end
end

proc readrawxname=
	int c,hsum,length

	nextlx.svalue:=lxsptr
	hsum:=0

	while namemap[c:=lxsptr++^] do
		hsum:=hsum<<4-hsum+c
	end
	--lxsptr

	length:=lxsptr-nextlx.svalue

	if length=0 then
		lxerror("Bad ` name")
	end
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
	end

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
				if c>='A'  and c<='Z' then c+:=' ' end
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
						end
						++t
					end
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
					end
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
					lxerror_s("Unknown string escape: \\%s",str)
				end
			when '"','\'' then		!possible terminators
				if c=termchar then		!terminator char
					if s^=c then		!repeated, assume embedded term char
						++s
					else			!was end of string
						exit
					end
				end
HASESCAPE:=1
			when cr,lf,0 then
				lxerror("String not terminated")
			end case

			if pass=2 then
				t^:=c
			end
			++t

		end

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
			end

		else
			t^:=0
			lxsptr:=s
		end
	end
end

func readhexcode(^ref char s, int n, sp=0)int a=
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
		end

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
		end
	end
	a
end

func getutf8(int c, ^char s)int n =
!convert unicode char c to utf8 sequence at s, consisting of 1-4 bytes, and
!return the number of bytes. s will be zero-terminated
!On error, return zero
	[16]char str
	if s=nil then s:=str end

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
	end

	s^:=0
	n
end

proc readdec=
	int c
	^char dest, destend, pstart
	int islong, length
	[1024]char str
	word a

	islong:=0

	pstart:=lxsptr

	dest:=str
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
			end
			--lxsptr
			exit

		when '_','\'' then

		when 'b','B' then
			length:=dest-&str[1]
			if length>64 then lxerror("bin overflow") end
			dest:=str
			a:=0
			to length do
				if dest^>='2' then lxerror("bad bin digit") end
				a:=a*2+dest++^-'0'
			end
			finish

		else
			--lxsptr
			exit
		end

		if dest>=destend then lxerror("Numlit too long") end
	end
	length:=dest-&str[1]

	if length>20 or length=20 and strncmp(str,u64maxstr,20)>0 then
		nodecimal()
	end

finish:
	nextlx.symbol:=intconstsym
	nextlx.subcode:=setinttype(a)
	nextlx.value:=a
end

proc readhex=
	int c
	^char dest, destend, pstart
	int length
	[1024]byte str
	word a

	pstart:=lxsptr

	dest:=str
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

		when '.' then
			--lxsptr
			exit

		else
			--lxsptr
			exit
		end

		if dest>=destend then lxerror("Numlit too long") end
	end
	length:=dest-&str[1]

	if length>16 then
		LXERROR("MAKEDEC")
		return
	end

	nextlx.symbol:=intconstsym
	nextlx.subcode:=setinttype(a)
	nextlx.value:=a
end

proc readbin=
	int c
	^char dest, destend, pstart
	int length
	[1024]byte str
	word a

	pstart:=lxsptr

	dest:=str
	destend:=dest+str.len-10
	a:=0

	do
		case c:=lxsptr++^
		when '0', '1' then
			a:=a*2+c-'0'
			dest++^:=c

		when '_','\'' then

		when '.' then
			--lxsptr
			exit

		elsif c in '2'..'9' then
			lxerror("bin bad digit")
		else
			--lxsptr
			exit
		end case

		if dest>=destend then lxerror("bin overflow") end
	end
	length:=dest-&str[1]

	if length>64 then
		nodecimal()
	end

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

	dest:=str
	destend:=dest+str.len-100
	length:=negexpon:=dotseen:=expseen:=expon:=fractlen:=0

	do
		if (c:=lxsptr++^) in '0'..'9' then
			dest++^:=c
			++length
			if dotseen then ++fractlen end
		elsecase c
		when '.' then
			if dotseen then --lxsptr; exit end
			dotseen:=1
			dest++^:=c

		when 'e','E' then
			if expseen then lxerror("double expon") end
			expseen:=1
			dest++^:=c
			while lxsptr^=' ' do ++lxsptr end
			if lxsptr^ in ['+','-'] then
				if lxsptr^='-' then negexpon:=1 end
				dest++^:=lxsptr++^
			end

			expon:=0
			do
				if (c:=lxsptr++^) in '0'..'9' then
					expon:=expon*10+c-'0'
					dest++^:=c
					if dest>=destend then lxerror("expon?") end
				elsecase c
				when '_','\'' then
				else
					--lxsptr
					exit all
				end
			end

		when '_','\'' then

		else
			--lxsptr
			exit
		end

		if dest>=destend then lxerror("r64lit too long") end
	end
	dest^:=0

	if expseen and expon>=0 and not dotseen then		!read as integer
		a:=0
		for i to length do				!digits already range checked
			a:=a*10+str[i]-'0'
		end
		to expon do
			a:=a*10
		end
		nextlx.symbol:=intconstsym
		nextlx.subcode:=setinttype(a)
		nextlx.value:=a
		return
	end


!------------------------------------------------------------
! Fast way to convert for ordinary numbers (1e100 migt be slower!)
!------------------------------------------------------------
	if negexpon then expon:=-expon end
	expon-:=fractlen
	x:=0.0

	for i:=1 to length+dotseen do		!digits already range-checked
		c:=str[i]
		if c<>'.' then
			x:=x*10.0+c-'0'
		end
	end

	if expon>=0 then
		to expon do
			x*:=10.0
		end
	else
		to -expon do
			x/:=10.0
		end
	end

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
	lxerror("u64 const overflow")
end

proc start=
	for c in namemap.bounds do
		if c in 'a'..'z' or c in '0'..'9' or c in ['_','$'] then
			namemap[c]:=1
		elsif c in 'A'..'Z' then
			namemap[c]:=2				!upper case
		end
	end
end

=== mm_lib.m 0 0 42/74 ===
int autotypeno=0
global int nextavindex=0
int nextsvindex=0

strbuffer exprstrvar
^strbuffer exprstr=&exprstrvar

strbuffer sbuffer
global ^strbuffer dest=&sbuffer

^strbuffer jdest

!global ichar framevarname			!normally nil, set to frame var def to display in comment

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
		end
	end

	if owner.deflist=nil then			!first def
		owner.deflist:=p
	else
		owner.deflistx.nextdef:=p
	end

	owner.deflistx:=p
end

global func createname(symbol p)^unitrec=
	^unitrec u

	u:=allocunitrec(jname)
	u.def:=p

	return u
end

global func createunit0(int tag)^unitrec=
	^unitrec u

	u:=allocunitrec(tag)
	return u
end

global func createunit1(int tag, ^unitrec p)^unitrec=
	^unitrec u

	u:=allocunitrec(tag)
	u.a:=p
	return u
end

global func createunit2(int tag, ^unitrec p,q)^unitrec=
	^unitrec u

	u:=allocunitrec(tag)

	u.a:=p
	u.b:=q
	return u
end

global func createunit3(int tag, ^unitrec p,q,r)^unitrec=
	^unitrec u

	u:=allocunitrec(tag)
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

	q:=allocunitrec(jnull)
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

global func createconstunit(u64 a, int t)^unitrec=
	^unitrec u
	u:=allocunitrec(jconst)
	u.value:=a
	u.mode:=t

	u.isconst:=1
	return u
end

global func createstringconstunit(ichar s, int length)^unitrec=
	^unitrec u
	u:=allocunitrec(jconst)
	u.svalue:=s
	u.mode:=trefchar
	u.isastring:=1

	if length=-1 then
		u.slength:=strlen(s)+1
	else
		u.slength:=length
	end
	return u
end

global func newtypename(symbol a,b)int=
	if ntypenames>=maxtypename then
		serror("Too many type names")
	end
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
	end

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

global func getrangelwbunit(^unitrec p)^unitrec=
	if p.tag=jmakerange then
		return p.a
	else
		p:=createunit1(jprop,p)
		p.propcode:=kklwb
		return p
	end
end

global func getrangeupbunit(^unitrec p)^unitrec=
	if p.tag=jmakerange then
		return p.b
	else
		p:=createunit1(jprop,p)
		p.propcode:=kkupb
		return p
	end
end

global func createarraymode(symbol owner,int target,unit dimexpr, int typedefx)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
	int k,m

	if typedefx=0 then		!anon type
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	end

	ttbasetype[m]:=tarray
	ttlower[m]:=1
	ttdimexpr[m]:=dimexpr
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner

	return m
end

global func createarraymodek(symbol owner,int target,int lower,length, int typedefx)int=
!lower is lower bound of array
	int atype,m

	atype:=tarray

	if typedefx=0 then		!anon type
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	end

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

global func nextautotype:ichar=
	static [32]char str

	print @str,"$T",,++autotypeno
	return str
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
!		end
!	end
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	end

	ttbasetype[m]:=slicetype
	if dimexpr then
		ttdimexpr[m]:=dimexpr
	else
		ttlower[m]:=1
	end
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner

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
	end

	ttbasetype[m]:=tslice
	ttlower[m]:=lower
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner

	return m
end

global func createrefmode(symbol owner,int target,typedefx=0)int=
	int k,m
!	int a,b

	if typedefx=0 then		!anon type
		for k:=tlast to ntypes when ttisref[k] do
			if tttarget[k]=target then
				return k
			end
		end
!		FI
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	end

	storemode(owner,target,tttarget[m])
	ttbasetype[m]:=tref
	ttsize[m]:=ttsize[tref]
	ttisref[m]:=1

	return m
end

global func createrefprocmode(symbol owner,stproc, paramlist,int kwd, prettype,typedefx)int=
!create a ^proc mode; (can't create a proc mode by itself, as it's meaningless)
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
	end

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
end

!global func getdottedname(symbol p)ichar=
!!build full dotted name for st item p
!	static [256]char str
!	[256]char str2
!	symbol owner
!
!	strcpy(str,p.name)
!	owner:=p.owner
!	while owner and owner.nameid<>programid do
!		strcpy(str2,str)
!		strcpy(str,owner.name)
!		strcat(str,".")
!		strcat(str,str2)
!		owner:=owner.owner
!	end
!	return str
!end

global func getavname(symbol owner,int id=frameid)symbol=
!create auto-var name and return pointer to st entry
	symbol p
	[32]char str
	ichar name

	if id=frameid and owner.nameid<>procid then
		serror("Auto frame not in proc")
	end

	if id=frameid then
		print @str,"av_",,++nextavindex
	else
		print @str,"sv_",,++nextsvindex
	end

	name:=pcm_copyheapstring(str)
	addnamestr(name)

	p:=getduplnameptr(owner,addnamestr(name),id)
	p.used:=1

	p.mode:=tint

	adddef(owner,p)
	return p
end

global proc unionstr_clear(^uflagsrec u)=
	((^u64(u))^:=0)		!clear flags and length togetjer
end

global proc unionstr_append(^uflagsrec u, int c)=
	if u.ulength=(u.codes.len-1) then
		serror("Uflags overflow/a")
	end
	++u.ulength
	u.codes[u.ulength]:=c
end

global proc unionstr_concat(^uflagsrec u, v)=
	int ulen,vlen,i

	ulen:=u.ulength
	vlen:=v.ulength
	if ulen+vlen>u.codes.len then
		serror("Uflags overflow/c")
	end
	for i:=1 to vlen do
		u.codes[i+ulen]:=v.codes[i]
	end
	u.ulength:=ulen+vlen
end

global func unionstr_last(^uflagsrec u)int=
	if u.ulength then
		return u.codes[u.ulength]
	end
	return 0 
end

global proc unionstr_copy(^uflagsrec u,v)=
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
	end
	ttbasetype[m]:=trecord

	return m
end

global func createtuplemode(symbol owner,[]int &elements,int elementslen, typedefx)int=
	int m

	if typedefx=0 then
		m:=createusertype(owner)
	else
		m:=typedefx
	end
	ttbasetype[m]:=ttuple
	ttlength[m]:=elementslen
	ttmult[m]:=pcm_alloc(elementslen*i32.bytes)
	for i to elementslen do
		storemode(owner,elements[i],ttmult[m,i])
	end

	return m
end

global func strexpr(^unitrec p)^strbuffer=
!vx_makestring("",exprstr)
	gs_init(exprstr)

	jevalx2(exprstr,p)
	return exprstr
end

global proc jevalx2(^strbuffer dest, ^unitrec p)=			!JEVAL
	jdest:=dest
	jevalx(p)
end

global proc jevalx(^unitrec p)=			!JEVAL
!p represents an expression. It can be a unitrec only, not a list (lists only occur inside
!kmakelist and kmakeset units, which specially dealt with here)
!dest is a destination string. Special routines such as gs_additem() are used, which take care
!of separators so that successive alphanumeric items don't touch
	unit q,a,b
	[500]char str
	int length

	if p=nil then
		return
	end

	a:=p.a
	b:=p.b

	case p.tag
	when jconst then

		case ttbasetype[p.mode]
		when ti32,ti64,ti8,ti16 then
			getstrint(p.value,str)
		when tu32,tu64,tu8,tu16 then
			strcpy(str,strword(p.value))
		when tc8,tc64 then
			str[1]:=p.value
			str[0]:=0

		when treal,tr32 then
			print @str,p.xvalue
		when tref then
			if p.mode=trefchar and p.isastring then
				if p.slength>str.len/2 then
					strcpy(str,"LONGSTR)")
				else
					convertstring(p.svalue,str)
				end
				jadditem("""")
				jadditem(str)
				jadditem("""")
				return
			else
				print @str,^void(p.value)
			end
		else
			strcpy(STR,"<EVAL/CONST PROBABLY VOID>")
		end case
		jadditem(str)

	when jname then
		jadditem(p.def.name)

	when jbin,jcmp then

		strcpy(str,pclnames[p.pclop])
		jadditem("(")
		jevalx(a)
		jadditem(str)
		jevalx(b)
		jadditem(")")

	when junary, jistruel, jnotl then

		strcpy(str,pclnames[p.pclop])
		jadditem(str)
		jadditem("(")

		if a.tag=jtypeconst then
			jadditem(STRMODE(a.value))
		else
			jevalx(a)
		end
		jadditem(")")

	when jprop then

		strcpy(str,propnames[p.propcode])
		jadditem(str)
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
			if q then jadditem(",") end
		end
		jadditem(")")

	when jindex,jdotindex,jslice,jdotslice then
		jevalx(a)
		if p.tag=jdotindex or p.tag=jdotslice then
			jadditem(".")
		end
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
			if q then jadditem(",") end
		end
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

	when jconvert,jtypepun, jwiden then

		jadditem(strmode(p.oldmode))
		if p.tag=jtypepun then
			jadditem("@")
		end
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
	when jdim then
		jevalx(a)
		jadditem(":")
		if b then
			jevalx(p.b)
		else
			jaddstr("-")
		end

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
		end

!	when jaddroffirst then
!		jadditem("")
!		jevalx(a)

	when jtypestr then
		jadditem("TYPESTR(")
		jevalx(a)
		jadditem(")")

!	when jcvlineno, jcvfilename, jcvmodulename then
	when jcompilervar then
		jaddstr("$")
		jaddstr(cvnames[p.cvindex]+3)

	when jbitfield then
		jevalx(a)
		jaddstr(".")
		jaddstr(bitfieldnames[p.bfcode])

	when jfmtitem then
		jevalx(a)
		jaddstr(":")
		jevalx(b)

	when jsyscall then
		jaddstr(sysfnnames[p.fnindex]+3)
		jaddstr("(")
		if a then jevalx(a) end
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
	static [2048]char str
	istrmode(m,expand,str)
	return str
end

global func strmode2(int m,expand=1)ichar=
	static [2048]char str
	istrmode(m,expand,str)
	return str
end

global func strpmode(int m, size=0)ichar=
	static [2048]char str
	if m=tblock then
		print @str,"mem:",,size
	else
		istrmode(m,1,str)
	end
	str
end

global proc istrmode(int m,expand=1,ichar dest)=
	symbol d,q
	int needcomma,i,target,mbase,n
	strbuffer sxx
	^strbuffer xx:=&sxx
	^strbuffer sdim
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
			end
			strcat(dest,tn.def.name)
!		end
		return
	end

	if m<tlast and m<>tref then
		strcpy(dest,typename(m))
		return
	end

	case mbase:=ttbasetype[m]
	when tref then
		strcpy(dest,"^")
		target:=tttarget[m]
		if target>=0 and ttbasetype[target]=trecord then
			strcat(dest,typename(target))
		else
			istrmode(tttarget[m],0,dest+strlen(dest))
		end

	when tarray then
		if ttdimexpr[m] then
			gs_copytostr(strexpr(ttdimexpr[m]),strdim)
			fprint @dest,"@[#<#>",&strdim[1],m
		else
			if ttlength[m] then
				if ttlower[m]=1 then
					fprint @dest,"[#]",ttlength[m]+ttlower[m]-1
				else
					fprint @dest,"[#..#]",ttlower[m],ttlength[m]+ttlower[m]-1
				end
			else
				if ttlower[m]=1 then
					fprint @dest,"[]"
				else
					fprint @dest,"[#:]",ttlower[m]
				end
			end
		end
		istrmode(tttarget[m],0,dest+strlen(dest))

	when tslice then
		prefix:=stdnames[mbase]

		if ttdimexpr[m] then
			gs_copytostr(strexpr(ttdimexpr[m]),strdim)
			fprint @dest,"@#[#:]",prefix,&strdim[1]
		else
			if ttlower[m]=1 then
				strcpy(dest,prefix)
				strcat(dest,"[]")
			else
				fprint @dest,"#[#:]",prefix,ttlower[m]
			end
		end
		istrmode(tttarget[m],0,dest+strlen(dest))

	when trecord then
		if not expand then
			strcpy(dest,typename(m))
			return
		end
		strcpy(dest,"")
		if expand<>2 then
			strcat(dest,typename(ttbasetype[m]))
		end
		strcat(dest,"(")
		d:=ttnamedef[m]
		needcomma:=0

		q:=d.deflist

		while q, q:=q.nextdef do
			if needcomma then strcat(dest,",") end
			needcomma:=1
			istrmode(q.mode,0,dest+strlen(dest))
			strcat(dest," ")
			strcat(dest,q.name)
		end
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
			if needcomma then strcat(dest,",") end
			needcomma:=1
			istrmode(q.mode,0,dest+strlen(dest))
			strcat(dest," ")
			strcat(dest,q.name)
			q:=q.nextdef
		end
		strcat(dest,")")
		if d.mode<>tvoid then
			istrmode(d.mode,0,dest+strlen(dest))
		end

	when ttuple then
		strcpy(dest,"Tuple(")
		n:=ttlength[m]
		for i to n do
			istrmode(ttmult[m,i],0,dest+strlen(dest))
			if i<n then strcat(dest,",") end
		end

		strcat(dest,")")

	when tbitfield then
		strcpy(dest,"bitfield")

	elsif ttbasetype[m]<tlast then
		strcpy(dest,"Alias for:")
		istrmode(tttarget[m],0,dest+strlen(dest))

	else
		println typename(m),STRMODE(TTBASETYPE[M])
		mcerror("NEWSTRMODE")
	end case
end

!global proc addtoproclist(symbol d)=
!	^procrec pp
!
!	pp:=pcm_allocnfz(procrec.bytes)
!
!	if proclist=nil then
!		proclist:=proclistx:=pp
!	else
!		proclistx.nextproc:=pp
!		proclistx:=pp
!	end
!!
!	pp.def:=d
!end

global proc addlinear(symbol d)=
	if stlinear=nil then
		stlinear:=stlinearx:=d
	else
		stlinearx.nextlinear:=d
		stlinearx:=d
	end
end

!global proc addstatic(symbol d)=
!	^procrec pp
!
!!CPL "ADDSTATIC", D.NAME, D.OWNER.NAME
!
!!	pp:=pcm_alloc(procrec.bytes)
!	pp:=pcm_allocnfz(procrec.bytes)
!
!	if staticlist=nil then
!		staticlist:=staticlistx:=pp
!	else
!		staticlistx.nextproc:=pp
!		staticlistx:=pp
!	end
!
!	pp.def:=d
!end
!
!global proc addexpconst(symbol d)=
!	^procrec pp
!	pp:=pcm_allocnfz(procrec.bytes)
!
!	if constlist=nil then
!		constlist:=constlistx:=pp
!	else
!		constlistx.nextproc:=pp
!		constlistx:=pp
!	end
!	pp.def:=d
!end

global func typename(int m)ichar=
	if m>=0 then
		return ttname[m]
	end
	return typenames[-m].def.name

end

global func allocunitrec(int tag)^unitrec=
	^unitrec p

	p:=pcm_allocnfz(unitrec.bytes)
	p.tag:=tag
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
		end
	end

	return p
end

global func duplunit(unit p,int lineno=0)unit=
	unit q
	if p=nil then return nil end

	q:=createunit0(p.tag)

	q^:=p^
	q.nextunit:=nil
	for i to jsubs[q.tag] do
		q.abc[i]:=duplunit(q.abc[i])
	end

	return q
end

global func isconstunit(unit a)int=
	return a.isconst
end

global proc getownername(symbol d, ichar dest)=
	symbol owner

	owner:=d.owner

	if owner=nil or owner.nameid=programid then return end
	getownername(owner,dest)
	strcat(dest,owner.name)
	strcat(dest,".")
end

global func getalignment(int m)int=
!return alignment needed for type m, as 1,2,4,8
	int a

!CPL "GETALIGN", STRMODE(M), STRMODE(TTTARGET[M])

	case ttbasetype[m]
	when tarray then
		return getalignment(tttarget[m])
	when trecord then
		a:=ttnamedef[m].maxalign
		if a=0 then
CPL "GAL0"
			 a:=8
		 end
!CPL "  //RECORD/MAXALIGN=",A
		return a
	when tslice then
		return 8
	end case

	a:=ttsize[m]
	case a
	when 1,2,4,8 then
		return a
	when 0 then
		return 8
	end case
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
	end
	ulistx:=p			!update end-of-list pointer
end

global func storemode(symbol owner, int m, i32 &pmode)int =
	^typenamerec r

	if m>=0 then
		pmode:=m
		return m
	end

	r:=&typenames[-m]

	if r.pmode=nil then
		r.owner:=owner
		pmode:=m
		r.pmode:=&pmode

	IF R.PMODE=NIL THEN SERROR("PMODE=NIL") FI

		return m
	end

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
	end case
end

!global proc writegsfile(ichar filename, ^strbuffer d)=
!	filehandle f
!
!	f:=fopen(filename,"w")
!	gs_println(d,f)
!	fclose(f)
!end

global proc addtolog(ichar filename, filehandle logdest)=
	filehandle f
	int c

	f:=fopen(filename,"rb")

	if f=nil then
		CPL "ATL ERROR",FILENAME; return end

	do
		c:=fgetc(f)
		exit when c=c_eof
		fputc(c,logdest)
	end
	fclose(f)
end

global func getprocretmodes(unit p)symbol=
!p must be a call unit, for a proc with multiple values; at least one expected
!however, here it only populates retmodes with the available types
	unit a

	if p.tag<>jcall then txerror("multass/need multfn") end
	a:=p.a

	case a.tag
	when jname then
		return a.def
	else
		return ttnamedef[tttarget[a.mode]]
	end case
end

export func convertstring(ichar s, t)int=
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
!returns length of t
	int c
	ichar t0:=t
	[16]char str

	while c:=s++^ do
		case c
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
		when 7, 8, 26, 27 then
			t++^:='<'
			t++^:=c/10+'0'
			t++^:=(c rem 10)+'0'
			t++^:='>'
		elsif c in 32..126 then
			t++^:=c
		else
			t++^:='\\'
			t++^:='x'
			print @str, c:"z2h"
			t++^:=str[1]
			t++^:=str[2]
		end case
	end
	t^:=0

	return t-t0
end

global func getpclmode(int t)int u=
	u:=stdpcl[ttbasetype[t]]

	if u=tblock then
		case ttsize[t]
		when 8 then u:=tu64
		when 4 then u:=tu32
		when 2 then u:=tu16
		when 1 then u:=tu8
		end case
	end
	return u
end

!global func getfullname(symbol d, int backtick=0)ichar=
!!create fully qualified name into caller's dest buffer
!	static [256]char str
!	int n:=0
!	symbol e:=d
!	ichar name
!
!	str[1]:=0
!	if backtick then
!		strcpy(str, "`")
!	end
!
!	if d.isimport then
!		name:=(d.truename|d.truename|d.name)
!		if backtick then
!			strcat(str, name)
!			strcat(str, "*")
!		else
!			strcat(str, name)
!		end
!		return str
!	end
!
!	if d.equivvar then
!		d:=d.equivvar.def
!	end
!
!	if d.owner then
!		if d.owner.nameid=procid then
!			strcat(str, d.owner.owner.name)
!			strcat(str, ".")
!		end
!		strcat(str, d.owner.name)
!		strcat(str, ".")
!	end
!	strcat(str, d.name)
!end
!

=== mm_libsources.m 0 0 43/74 ===
global const fsyslibs = 1

global tabledata []ichar syslibnames, []ichar syslibtext =
	("msyswin.m",		strinclude "msyswin.m"),
	("msyswinc.m",		strinclude "msyswinc.m"),
	("msyswini.m",		strinclude "msyswini.m"),
	("msyslinc.m",		strinclude "msyslinc.m"),
	("msys.m",			strinclude "msys.m"),
	("msysc.m",			strinclude "msysc.m"),
	("msysmin.m",		strinclude "msysmin.m"),
	("mlib.m",			strinclude "mlib.m"),
	("mclib.m",			strinclude "mclib.m"),
	("mwindows.m",		strinclude "mwindows.m"),
	("mlinux.m",		strinclude "mlinux.m"),
	("mwindll.m",		strinclude "mwindll.m"),
	("mwindllc.m",		strinclude "mwindllc.m"),
end

global proc loadbuiltins=
!load built-in libs to sources[] list
	ifile pf
	ichar filename

	for i to syslibnames.len do
		filename:=syslibnames[i]
		pf:=newsourcefile()

		sources[nsourcefiles].name:=pcm_copyheapstring(extractbasefile(filename))
		sources[nsourcefiles].filename:=filename
		sources[nsourcefiles].text:=syslibtext[i]

		SOURCES[NSOURCEFILES].TEXT:=pcm_copyheapstring(syslibtext[i])


		sources[nsourcefiles].size:=strlen(syslibtext[i])
!		sources[nsourcefiles].path:="<Builtins>"
		sources[nsourcefiles].path:=""
		sources[nsourcefiles].filespec:=filename
		sources[nsourcefiles].issyslib:=1
		sources[nsourcefiles].issupport:=0
!CPL "LOADED BUILTIN", FILENAME
	end
end
=== mm_modules.m 0 0 44/74 ===
ichar fileext="m"

global func loadsp(ichar filename, int mainsub=0)isubprog sp=
!source = nil:  load lead module and dependencies from given sourcefile
!source <> nil: source code is given directly. filename can give a name
! to that source text, or if nil, and internal name is applied

	const maxmods=500
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
	end

	ext:=extractext(filename)
	if not eqstring(ext, "m") then fileext:=pcm_copyheapstring(ext) end

	pm:=loadmodule(filename, issyslib)

	if pm=nil then
		loaderror("Can't load lead module: ", filename)
	end
	path:=pm.file.path

	for i to nsubprogs do
		if eqstring(pm.name, subprogs[i].name) then
			loaderror("Subprog already loaded: ", sp.name)
		end
	end

!reader header info
	startlex(pm.file)
	lex()
	skipsemi()

	if lx.symbol=kprojectsym then
		proj:=1
		lexchecksymbol(eqsym)
		lex()
	end

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
					if nmods>=maxmods then loaderror("Too many modules in header") end
					modnames[++nmods]:=name
					paths[nmods]:=path
					aliases[nmods]:=nil

				end
				if nextlx.symbol=namesym and eqstring(nextlx.symptr.name,"as") then
					lex()
					lex()
					if lx.symbol=namesym then
						stalias:=lx.symptr
						lex()
					else
						checksymbol(stringconstsym)
						stalias:=addnamestr(lx.svalue)
					end
					aliases[nmods]:=stalias
				end

			when hdr_import then
				checksymbol(namesym)
				if nsubs>=maxsubs then loaderror("Too many imports in header") end
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
			end case
			lex()

		when semisym then
		else
			exit
		end case
	end

	if proj then
		checkend(kendsym, kprojectsym)
	end
	skipsemi()
	if lx.symbol=eofsym then
		sepheader:=1
	end

!process nested imports
	for i to nsubs do
		if eqstring(subnames[i],pm.name) then loaderror("Importing self") end
!		loadsp(getmodulefilename(path, subnames[i]))
		loadsp(getmodulefilename(subpaths[i], subnames[i]))
	end

!create new subprog entry
	if nsubprogs>=maxsubprog then loaderror("Too many subprogs") end
	sp:=pcm_allocz(subprogrec.bytes)
	subprogs[++nsubprogs]:=sp
	sp.subprogno:=nsubprogs

	if mainsub then
!		loadsyslib()
		mainsubprogno:=nsubprogs
	end

	firstmod:=nmodules+1
	lastmod:=firstmod+nmods
	if lastmod>maxmodule then loaderror("Too many modules") end
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
		end
		modules[firstmod+i]:=pm
		pm.stmodule:=d:=createdupldef(stprogram,addnamestr(pm.name),moduleid)
		pm.subprogno:=nsubprogs
		
		if stalias then
			pm.stmacro:=getduplnameptr(stprogram, stalias, macroid)
			adddef(stprogram, pm.stmacro)
			pm.stmacro.paramlist:=nil
			pm.stmacro.code:=createname(d)
		end

		d.moduleno:=pm.moduleno:=firstmod+i
		d.subprogno:=nsubprogs
		moduletosub[d.moduleno]:=nsubprogs

		for j to nmodules when eqstring(modules[i].name, pm.name) do
			serror_s("Dupl mod name:", pm.name)
		end
	end

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
		end
	end

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
	end
	pf.text:=s
	pf.size:=rfsize

	if passlevel=ma_pass then
		pf.dupl:=pcm_copyheapstring(s)
	end

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
		if eqstring(libfiles[i],libname) then return end
	end
	if nlibfiles>=maxlibfile then
		loaderror("Too many libs")
	end
	libfiles[++nlibfiles]:=libname
end

proc loadsyslib=
	[300]char str
	ichar name

	if dointlibs then				!bundled sys files
		str[1]:=0
	else
		strcpy(str, langhomedir)
	end
!CPL =MSYSLEVEL

	case msyslevel
	when 0 then
		return
	when 1 then
		name:=(not os_iswindows()|"msysminc"|"msysmin")
	else				!full syslib
!CPL "FULLSYS"
		if os_iswindows() and not clinux then	!run on Windows
!CPL "WINDOWS", =CTARGET
			name:="msyswin"
		else									!on Linux, or generating C for Linux on Windows
!CPL "LINUX"
			name:="msyslinc"
		end
	end case

	strcat(str, name)

	SYSLIBNAME:=PCM_COPYHEAPSTRING(STR)
IF FVERBOSE>=2 THEN
	CPL =SYSLIBNAME
FI
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
	end

	loadsyslib()

!try .ma version of .m not present
	if not checkfile(file) then
		file2:=pcm_copyheapstring(changeext(file,"ma"))
		if checkfile(file2) then file:=file2 end
	end

	if eqstring(extractext(file),"ma") then
		loadmafile(file)
		loadedfromma:=1
		strcpy(str, changeext(file,"m"))			!assume lead module has same name as ma file
		file:=str
	end

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
		end
	end docase

	t^:=0

	readln @&str[1]
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
		end
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
		end
	else
		s:=builtinstr
	end

!need to scan file pickuping the file headers, and populating sourctables

	s:=readfileline(s+3)
	readstr(name,'n')
	if not eqstring(name,"ma") then
		loaderror("MA: bad header")
	end

	--s					!point to previous lf

	s:=findnextlineheader(s)

	do
		if s=nil then
			loaderror("Unexpected EOF in MA file")
			exit
		end
		s:=readfileline(s)

		readstr(name,'n')
		read sys,support

		if eqstring(name,"end") then
			exit
		end
		if nsourcefiles>=maxsourcefile then
			loaderror("Too many files in MA")
		end

		t:=findnextlineheader(s)
		if t=nil then
			loaderror("MA error")
		end

		pf:=newsourcefile()

		pf.filename:=pf.filespec:=pcm_copyheapstring(name)
		pf.name:=pcm_copyheapstring(extractbasefile(name))
		pf.size:=t-s-3
		pf.text:=s
		pf.path:=pf.filespec:=""
		pf.issyslib:=sys
		pf.issupport:=support
		s:=t
	end
!
	if fdeconsma then
		println "Deconst MA to src/*.*"
		system("md src")
	end

	for i to nsourcefiles do
		pf:=sources[i]
		(pf.text+pf.size)^:=0

		if fdeconsma then
			strcpy(name, "src/")
			strcat(name, sources[i].filename)
			println "Writing", name
			writefile(name, sources[i].text, sources[i].size-1)
		end
	end

	if fdeconsma then
		stop
	end

end


=== mm_name.m 0 0 45/74 ===
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
		end
	end
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
		end

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
		end

		rx_unitlist(owner,b)

		if a.tag=jname then
			d:=a.def
			case d.nameid
			when typeid then		!change to type conversion
				p.tag:=jconvert
				storemode(owner,d.mode,p.oldmode)
				p.a:=b
				if b.nextunit then
					p.a:=createunit1(jmakelist,b)
					n:=0
					while b do
						++n
						b:=b.nextunit
					end
					p.a.length:=n
				end
			when macroid then
				++macrolevels
				if d.deflist then			!macro uses params
					expandmacro(p,a,b)
					b:=nil
					useparams:=0
				else						!macro has no params
					expandmacro(p,a,nil)
					useparams:=1
				end

				rx_unit(owner,p)
				--macrolevels

				if useparams and p.tag<>jcall then
					insertunit(p,oldtag)
					p.b:=b					!note b may be nil
				FI

			end case
		end

	when jandl, jorl then
		rx_unit(owner,a)
		rx_unit(owner,b)
		if not isbooltag[a.tag] then insertunit(a,jistruel) end
		if not isbooltag[b.tag] then insertunit(b,jistruel) end

	when jistruel then
	doistruel:
		rx_unit(owner,a)

		if isbooltag[a.tag] then
			deleteunit(p,a)
		end

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
		end case

	when jdotindex then
		rx_unit(owner, a)
		rx_unit(owner, b)
		if b.tag=jmakerange then
			p.tag:=jdotslice
		fi

	else
doabc:
		for i to jsubs[p.tag] do
			rx_unitlist(owner,p.abc[i])
		end
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
	end
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
		if p.equivvar then
			rx_unit(owner,p.equivvar)
		end
		if p.code then
			rx_unit(owner,p.code)
		end
	when typeid then
		rx_deflist(p,p.deflist)

	else
	end case
end

proc rx_unitlist(symbol owner, unit p)=
	while p do
		rx_unit(owner,p)
		p:=p.nextunit
	end
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
			end
		end
	end

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
					end
				end
			end

		when typeid then					!only for code inside a record def
			if powner=owner or powner=owner.owner then		!immediate match
				return p					!looks at 2 nested record levels only
			end

		when programid then					!p is a module
			case p.nameid
			when moduleid, subprogid then	!match a module/subprog name
				if subprogno=moduletosub[p.moduleno] then
					moddef:=p
				else
					for i to nsubprogs do
						if eqstring(p.name, subprogs[i].name) then
!							p.issubprog:=1				!in case not yet set
							moddef:=p
							exit
						end
					end
				end
			when macroid then
				return p

			end case

		end case
	end

	if allowmod and moddef then
		return moddef
	end

	if extdef then
		if extcount>1 then
			if not eqstring(extdef.owner.name, "mclib") then
				for i:=1 to extcount do
					extdef:=ambiglist[i]
					println i,extdef.owner.name,namenames[extdef.owner.nameid]
				end
				if not eqstring(extdef.owner.name, "mclib") then
					rxerror_s("Ambiguous ext name: #",extdef.name)
				end
			end
		end
		return extdef
	end
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
	end

	e:=resolvetopname(owner,d,moduleno,allowmodname)

	if not e then
		islet:=0
		mode:=tvoid
		case p.avcode
		when 'I', 'T', 'S' then mode:=ti64; islet:=1
		when 'L','A' then mode:=tany
		end case

		if mode=tvoid then
			[300]CHAR STR
			STRCPY(STR, D.NAME)
			CONVUCSTRING(STR)
			rxerror_s("tcl:Undefined: #",STR,p)
		else
			e:=addframevar(owner,d,moduleno,mode)
			e.pos:=p.pos
			e.islet:=islet
		end
	end

!	if e.used<255 then ++e.used end
	e.used:=1

	p.def:=e
end

global func finddupl(symbol d, pdupl)symbol=
!trying to resolve a field name, by scanning a dupllist headed by pdupl
!which ought to point to nullid entry
!d will be the owner of the matching entry

	if pdupl.nameid<>nullid then		!assume already resolved
		return pdupl
	end
	pdupl:=pdupl.nextdupl

	while pdupl do
		if pdupl.owner=d then
			return pdupl
		end
		pdupl:=pdupl.nextdupl
	end
	return nil
end

global func finddupl_sub(symbol d, pdupl)symbol=
!version of finddupl where d is a subprog
	int subprogno

	if pdupl.nameid<>nullid then		!assume already resolved
		return pdupl
	end
	pdupl:=pdupl.nextdupl
	subprogno:=d.subprogno

	while pdupl do
		if pdupl.owner.subprogno=subprogno then
			return pdupl
		end
		pdupl:=pdupl.nextdupl
	end
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
			end

			e:=finddupl(d,e)

			if e then
				if d.nameid=moduleid then
					if e.subprogno<>subprogno then
						if e.scope<program_scope AND NOT E.ISIMPORT then
							rxerror_s("Need export to import '#'",e.name)
						end
					elsif e.moduleno<>moduleno then
						if not e.scope then
							rxerror_s("Need global to import '#'",e.name)
						end
					end
				end
domodule:
				p.tag:=jname			!convert to dot to name
				p.a:=p.b:=nil
				p.def:=e
				case e.nameid
				when constid then
				end case
			else
				rxerror_s("Can't resolve .#",p.b.def.name,p)
			end

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
					end case
				end
			else
				rxerror("Record expected")
			end case
			t:=ttnamedef[m]

			e:=finddupl(t,e)
			if e then
				p.b.def:=e
			else
				rxerror_s("Not a field: #",rhs.def.name)
			end
		when subprogid then
dosubprogid:
			e:=finddupl_sub(d,e)
			if e then
				if e.subprogno<>subprogno then
					if e.scope<program_scope AND NOT E.ISIMPORT then
						rxerror_s("Need export to import '#'",e.name)
					end
				end
				goto domodule
			else
				rxerror_s("Can't resolve sub.#",p.b.def.name,p)
			end

		end case

	else
!Can't fully resolve at this time; leave to next pass
		unless e.nextdupl then
			rxerror_s("Not a field: #",e.name)
		end unless
	end case
end

proc fixmode(^typenamerec p)=
!p refers to a negative mode that is a typename index
!fix that up if possible
	^i32 pmode
	symbol a,d,e,f,owner
	int m,moduleno

	pmode:=p.pmode

	m:=-pmode^					!typename index

	d:=owner:=p.owner
	while d.nameid<>moduleid do d:=d.owner end
	moduleno:=d.moduleno

	a:=p.defa
	d:=p.defb

	if a=nil and d then			!simple type name V
		e:=resolvetopname(owner,d,moduleno,0)

	end

	if e and e.nameid=typeid then
		pmode^:=e.mode

	else
		rxerror_s("2:Can't resolve tentative type: #",d.name)
	end
end

global proc fixusertypes=
	^typenamerec p
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
				end
			end
		end

		if npasses>5 then
			println "Type phase errors - check these user types:"

			for i to ntypenames do
				p:=&typenames[i]

				if p.pmode^<0 then
					d:=p.defb
					if d=nil then d:=p.defa end
					println "	",d.name
				end
			end

			rxerror("Fixtypes: too many passes (cyclic ref?)")
		end

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
	end
	return plist
end

func copyunit(unit p)unit=
	unit q
	symbol d

	if p=nil then return nil end

!need to quickly check if a name unit is a macroparam

	if p.tag=jname then
		d:=p.def
		for i to nmacroparams do
			if macroparamsgen[i]=d then
				return copyunit(macroargs[i])
				exit
			end
		end
	end

	q:=createunit0(p.tag)

	q^:=p^
	q.nextunit:=nil
	for i to jsubs[q.tag] do
		q.abc[i]:=copylistunit(q.abc[i])
	end

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
	end

	d:=a.def

!First step: get list of macro formal parameters

	pm:=d.paramlist
	nmacroparams:=0
	while pm do
		if nmacroparams>=maxmacroparams then
			rxerror("macro param overflow")
		end
		macroparams[++nmacroparams]:=pm
		macroparamsgen[nmacroparams]:=pm.firstdupl

		pm:=pm.nextparam
	end

!now get macro args into a list
	nmacroargs:=0

	while b do
		if nmacroargs>=maxmacroparams then
			rxerror("macro arg overflow")
		end
		macroargs[++nmacroargs]:=b
		b:=b.nextunit
	end

	if nmacroargs<nmacroparams then
		PRINTLN =NMACROARGS, NMACROPARAMS
		rxerror("Too few macro args")
	end

	ignoreargs:=0
	if nmacroargs>0 and nmacroparams=0 then		!ignore extra params
		ignoreargs:=1
		nmacroargs:=nmacroparams:=0

	elsif nmacroargs>nmacroparams then
		rxerror("Too many macro args")
	end

	pnew:=copyunit(d.code)

	if not ignoreargs then				!normal expansion
		replaceunit(p,pnew)
	else								!keep call and paramlist; just replace fn name
		p.a:=pnew						!with expansion
	end
end

=== mm_parse.m 0 0 46/74 ===
!M Language Parserxxx

const fastexpr=1
!const fastexpr=0

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
			if globalflag then serror("global global?") end
			globalflag:=lx.subcode

!			if globalflag=export_scope and stmodule.subprogno<>1 then
			if globalflag=export_scope and stmodule.subprogno<>nsubprogs then
				globalflag:=program_scope
			end

			lex()

		when kprocsym, kfuncsym then	!todo
			readprocdef(owner, globalflag)
			globalflag:=module_scope

!		when stdtypesym, krefsym, kicharsym, lsqsym, kslicesym then
		when stdtypesym, krefsym, kicharsym, lsqsym, kslicesym, ptrsym then
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
			end
			goto doexec

		else
doexec:
		serror("Code outside a func")
		end switch
	end
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
	while lx.symbol=semisym do lex() end
end

global func makeblock(unit p)unit=
	if p and p.tag=jblock then return p end
	return createunit1(jblock, p)
end

proc checkequals=
!check that "=" is current symbol
	if lx.symbol<>eqsym then
		serror("""="" expected")
	end
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
	end
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
	end
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
	end

	if lx.symbol<>kendsym then
		serror("'End' expected")
	end

	if lx.subcode then
		if lx.subcode in [endkwd1, endkwd2] then
			lex()
			return
		else
error:
			strcpy(str, "Mismatched end ")
			if startline then
				fprint @(&str[1]+strlen(str)), " (from line #)", startline
			end
			serror(str)
		end
	end

!only end was used, so skip that now
	lex()

!now, should be semi, or possibly kwd1/2
	if lx.symbol in [endkwd1, endkwd2] then
		lex()
!	elsif lx.symbol<>semisym then
!		error
	end
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
	end

	nvars:=0
	while lx.symbol=namesym do

		++nvars
		stname:=getduplnameptr(owner, lx.symptr, varid)

		stname.scope:=scope

		stname.isstatic:=isstatic

		stname.islet:=(k=kletsym)
		if varid=dllvarid then
			stname.isimport:=1
		end

		adddef(owner, stname)
!		if varid=staticid and owner.nameid<>procid then
		if varid=staticid then
!CPL "MM/ADDSTATIC", STNAME.NAME, OWNER.NAME
			addlinear(stname)
		end

		lex()

		storemode(owner, m, stname.mode)

		if lx.symbol in [assignsym, eqsym] then

!			initcode:=case lx.symbol when eqsym then 1 when assignsym then 2 else 3 end case
			case lx.symbol
			when eqsym then initcode:=1
			when assignsym then initcode:=2
			else initcode:=3
			end case
			stname.used:=1

			if lx.symbol<>eqsym then
				if varid=staticid then
					serror("Non-variants can't use :=")
					if owner.nameid=procid then
						serror("Can't use := for statics inside procs")
					end
					
				end
			else
				if varid=frameid then
					serror("Need 'static' for '='")
				end
!				addstatic(stname)
			end
			lex()

			stname.code:=readunit()

			if varid=frameid then
				p:=createunit2(jassign, createname(stname), stname.code)
				p.initlet:=1
				addlistunit(ulist, ulistx, p)
			end

		elsif lx.symbol=atsym then
			if k=kletsym then serror("let@") end
			lex()
			stname.equivvar:=readunit()
		elsif k=kletsym then
			serror("let needs :=/=")
		end

		if lx.symbol<>commasym then
			exit
		end
		lex()
	end

	if nvars=0 then
		serror("No vars declared")
	end
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
	end

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
			addlinear(stname)
		end

		if lx.symbol<>commasym then
			exit
		end
		lex()
	end

	if nconsts=0 then
		serror("No consts declared")
	end

end

func readlbrack:unit=
!positioned at "("
!termsym is rbracksym
!read one of the following:
! (x)		simple expression
! ()		list with no elements
! (x, )		list with one element
! (x, x, ...)		list
! (x|x|x])		if then else end
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

	elsif binopset[lx.symbol] and nextlx.symbol=rbracksym then	!operator constant
!	elsif symboloptypes[lx.symbol]=bin_op and nextlx.symbol=rbracksym then	!operator constant
		p:=createunit0(joperator)
		p.pclop:=symbolgenops[lx.symbol]
		lex()
		lex()
		return p
	end

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
	end case

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
		end
docomma:						!entry from implicit newline
		length:=1

!must be regular list
		ulist:=ulistx:=p

		if usecomma then
			repeat
				lex()							!skip comma
				if lx.symbol=rbracksym then		!allow , ) to end list
					exit
				end
				if lx.symbol=commasym then
					serror(", , null expr not allowed")
				end
				addlistunit(ulist, ulistx, readunit())
				++length
				skipsemi()
			until lx.symbol<>commasym
		else

			repeat
				skipsemi()
				if lx.symbol=rbracksym then		!allow , ) to end list
					exit
				end
				if lx.symbol=commasym then
					serror(", , null expr not allowed")
				end
				addlistunit(ulist, ulistx, readunit())
				++length
			until lx.symbol<>semisym
		end

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
			return p
		when rbracksym then
			lex()
			p:=createunit3(jif, fixcond(p), q, nil)
			return p
		end case

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
		end
		lex()
		r:=readunit()
		checksymbollex(rbracksym)
!		lex()
		return createunit3(jselect, p, ulist, r)

	when semisym then
		if lx.subcode=1 then
			usecomma:=0
			goto docomma
		end
		ulist:=ulistx:=p
		repeat
			skipsemi()
			if lx.symbol=rbracksym then
				exit
			end
			addlistunit(ulist, ulistx, readunit())
!			skipsemi()						!allow a, b, c;) (works better with a, b, c\ followed by comment on next line followed by ")")
		until lx.symbol<>semisym
		checksymbollex(rbracksym)
!		lex()

		return makeblock(ulist)


	else
		serror("(x ...")
	end case
	return nil
end

proc addlistparam(^symbol ulist, ulistx, symbol p)=
!add unit p to unit structure ulist, ^ulistx  which can be null
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx^.nextparam:=p
	end
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
		p.mode:=ti64
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
			p.mode:=ti64
			lex()
		else					!leave dot to be processed by caller
			p:=createunit0(jtypeconst)
			p.value:=t
		end
		return p
	else
		opc:=jconvert
	end case

	checksymbollex(lbracksym)
!	lex()
	p:=readunit()
	checksymbollex(rbracksym)
!	lex()

	p:=createunit1(opc, p)
	storemode(currproc, t, p.oldmode)
	return p
end

func readopc:unit=
!op sym seen just before a term
	unit p, q, r
	int tag, opc, firstsym, mathsop

	firstsym:=lx.symbol

	case lx.symbol
	when mathsopsym then
		tag:=junary
		mathsop:=lx.subcode
		opc:=kmaths
	when maths2opsym then
		tag:=jbin
		mathsop:=lx.subcode
		opc:=kmaths2
	else
		tag:=junary
		opc:=symbolgenops[firstsym]
	end case

	lex()
	case firstsym
	when addsym then			!ignore +
		return readterm2()
	when subsym then			!convert minus to negate
		opc:=kneg
	when minsym, maxsym, maths2opsym, iandsym, iorsym, ixorsym then
		p:=readterm2()

		if p.tag=jmakelist then
			if p.length<>2 then serror("Needs (x, y)") end
			q:=p.a
			r:=q.nextunit
			q.nextunit:=nil
			p:=createunit2(jbin, q, r)
			p.pclop:=opc
			p.mathsop:=mathsop
			return p
		else		!assume single tclopnd
			SERROR("READOPC/SINGLE OPND?")
			return createunit1(opc, p)

		end
	else
		if binopset[firstsym] then
!		if symboloptypes[firstsym]=bin_op then
			serror("Can't be used as unary op")
		end

	end case

	if lx.symbol=assignsym then	!op:=, not normally allowed inside expressions
		lex()
		tag:=junaryto
		case firstsym
		when subsym then
			opc:=kneg
		else
			opc:=symbolgenops[firstsym]
			if opc=0 then
				serror("op:= not available")
			end
		end case
	end

	p:=createunit1(tag, q:=readterm2())

	p.pclop:=opc
	p.mathsop:=mathsop

	if q.tag=jmakelist then
		serror("Too many opnds")
	end

	return p
end

func readcompilervar:unit=
	[100]char str
	rsystemtime tm
	static []ichar monthnames=("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
	unit p
	imodule currmodule:=modules[currmoduleno]

	switch lx.subcode
	when cv_nil then
		p:=createconstunit(0, tref)
		lex()
		return p

	when cv_pi then
!		p:=createconstunit(i64@(3.14159'265358'979'3238'4626'433'832), treal)
		p:=createconstunit(i64@(pi), treal)
		lex()
		return p

	when cv_infinity then
		p:=createconstunit(i64@(infinity), treal)
		lex()
		return p

	when cv_lineno then
!		tc_gen(kloadimm, getlineno(lx.pos)
		p:=createconstunit(getlineno(lx.pos), ti64)
!		p:=createunit0(cv_lineno)
		lex()
		return p

	when cv_strlineno then
		getstrint(getlineno(lx.pos), str)

	when cv_modulename then
		strcpy(str, stmodule.name)

	when cv_filename then
		strcpy(str, sources[currmodule.fileno].filespec)

	when cv_func then
		strcpy(str, currproc.name)

	when cv_date then
		os_getsystime(&tm)
		fprint @str, "#-#-#", tm.day, monthnames[tm.month], tm.year:"4"

	when cv_time then
		os_getsystime(&tm)
		fprint @str, "#:#:#", tm.hour:"z2", tm.minute:"z2", tm.second:"z2"

	when cv_version then
		strcpy(str, "Compiler:M6.4")

	when cv_true, cv_false then
		p:=createconstunit(lx.subcode=cv_true, tbool64)
		lex()
		return p
	
	else
		serror_s("compiler var not impl: #", jtagnames[lx.subcode])
	end switch
	lex()

	return createstringconstunit(pcm_copyheapstring(str), -1)
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
	end
	checksymbollex(lbracksym)
!	lex()
	m:=tvoid
	p:=readunit()
	if lx.symbol<>commasym then
		if opc=jtypepun then serror("@ type missing") end
		opc:=jautocast
	else
		lex()
		m:=readtypespec(currproc)
	end
	checksymbollex(rbracksym)
!	lex()

	p:=createunit1(opc, p)
	storemode(currproc, m, p.oldmode)

	return p
end

global proc checksymbol(int symbol)=
	[100]char str

	if lx.symbol<>symbol then
		fprint @str, "# expected, not #", symbolnames[symbol], symbolnames[lx.symbol]
		serror(str)
	end
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
						dim:=createunit2(jdim, dim, length)
					else													!lower:
						dim:=createunit1(jdim, dim)
					end
				end case
			end
			if ndims>=maxdim then serror("Too many array dims") end
			dims[++ndims]:=dim
			exit when lx.symbol<>commasym
			lex()
		end
		inreadprint:=0
		checksymbollex(rsqsym)
!		lex()
		t:=readtypespec(owner)

		for i:=ndims downto 1 do
			t:=createarraymode(owner, t, dims[i], (i=1|typedefx|0))
		end
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
		end

	when krecordsym, kstructsym then
		serror("Use 'record name =' syntax")

	when kunionsym then
		serror("Top-level union not allowed")

	when krefsym, ptrsym then		!^T
	retry:

		lex()
		if lx.symbol=ktosym then lex() end

		case lx.symbol
		when kprocsym, kfuncsym then	!func pointer being created
			t:=readrefproc(owner, typedefx)

		when stdtypesym then
			case lx.subcode
			when tc8 then
				t:=trefchar
				if typedefx then tttarget[typedefx]:=tc8 end
			else
				goto readtarget
			end case

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
		end case

	when kicharsym then
		if lx.subcode=tc8 then
			t:=trefchar
		else
			t:=tref
		end
		if typedefx then tttarget[typedefx]:=lx.subcode end
		lex()

	when kslicesym then
		t:=readslicetype(owner, lx.subcode, typedefx)

	else
		serror("Bad type starter")
	end case

	if typedefx then			!assume a simple alias
		ttbasetype[typedefx]:=ttbasetype[t]
	end

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
	end
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
	end

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
			end
			lex()
		when rbracksym then
			if donulls then
				addlistunit(ulist, ulistx, nullunit)
			end
			exit
		else
			addlistunit(ulist, ulistx, readunit())
			if lx.symbol in [commasym, semisym] then
				lex()
				if lx.symbol=rbracksym then
					exit
				end
			else
				skipsemi()
				if lx.symbol=rbracksym then
					exit
				end
				serror("SLIST?")
			end
		end case
	end
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
			plower.pclop:=kklwb
			pupper:=createunit1(junary, duplunit(p))
			pupper.pclop:=kkupb
			p:=createunit2(jslice, p, createunit2(jmakerange, plower, pupper))
			return p
		when rangesym, colonsym then
			lexchecksymbol(rsqsym)
			goto fullslice
		end case
	end

	do
		if ndollar>=maxdollarstack then
			serror("Too many nested a[$]")
		end
		dollarstack[++ndollar]:=p
		q:=readunit()
		--ndollar

		if q.tag=jmakerange then		!convert into a discrete slice
			p:=createunit2((dot|jdotslice|jslice), p, q)
		else
			p:=createunit2((dot|jdotindex|jindex), p, q)
		end

		exit when lx.symbol<>commasym
		lex()
	end
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
				end

				p2:=createunit2(jmakerange, q, r)
				deleteunit(p, p2)
			else
	doprop:
				p:=createunit1(jprop, p)
				p.pclop:=lx.subcode
			end
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
			end case
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
			end
			lex()
			p:=q



		else
	error:
			serror("Unknown dot suffix")
		end case
	end
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
		end
	end

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

!	stproc.pclinfo:=pcm_allocnfz(pclinforec.bytes)

	lex()

	startline:=getcurrline()

	if not shortfun then
		closesym:=checkbegin(0)
	end

	pushproc(stproc)
	nextavindex:=0

	IF DRETVAR THEN
		stname:=getduplnameptr(stproc, dretvar, frameid)
		storemode(procowner, stproc.mode, stname.mode)
		adddef(stproc, stname)
	end

	addlinear(stproc)

	if shortfun then
		stproc.code:=readunit()
		checksymbollex(semisym)
	else
		stproc.code:=readsunit()
		checkbeginend(closesym, kwd, startline)
	end

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

	int kwd, varparams, nparams, nretvalues
	int subprogno
	[maxtuplesize]int retmodes
	imodule ms
	isubprog ps

	ichar metadata, truename
	symbol pequiv, stproc, owner, paramlist, nameptr

	kwd:=lx.symbol				!remember keyword

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
	end

	nameptr:=lx.symptr

	stproc:=getduplnameptr(procowner, nameptr, (insidedllimport|dllprocid|procid))
!	if insidedllimport then scope:=global_scope end
!	if insidedllimport then scope:=program_scope end

	if truename then
		stproc.truename:=truename
	end


	adddef(procowner, stproc)
	if stproc.nameid=dllprocid then
		stproc.isimport:=1
	end

	owner:=stproc
	pushproc(stproc)

	lex()
	if lx.symbol=mulsym then
		stproc.ishandler:=1
		lex()
	end

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
		end
		lex()

		if lx.symbol=colonsym or lx.symbol=sendtosym then
			lex()
			nretvalues:=readreturntype(owner, retmodes)
		elsif typestarterset[lx.symbol] or lx.symbol=namesym then
			nretvalues:=readreturntype(owner, retmodes)
		end
	elsif lx.symbol=colonsym or lx.symbol=sendtosym then
		lex()
		nretvalues:=readreturntype(owner, retmodes)
	end

	dretvar:=nil
	if nretvalues=1 then
		if lx.symbol=namesym then
			dretvar:=lx.symptr
			lex()
		end
	end

	unless nretvalues or (kwd<>kfuncsym) then		!func: no result given
		serror("Function needs ret type")
	end unless

	if nretvalues and (kwd<>kfuncsym) then		!proc: result given
		serror("Proc can't return value")
	end

	stproc.paramlist:=paramlist
	stproc.nretvalues:=nretvalues

	case nretvalues
	when 0 then
		stproc.mode:=tvoid
	when 1 then
		storemode(procowner, retmodes[1], stproc.mode)
	else
		stproc.mode:=createtuplemode(procowner, retmodes, nretvalues, 0)
	end case

	stproc.code:=nil

	stproc.scope:=scope
	stproc.varparams:=varparams

	if procowner=stmodule then
		if stproc.namelen=5 and eqstring(stproc.name, "start") then
			modules[stmodule.moduleno].ststart:=stproc
			stproc.scope:=global_scope
			dosigcheck
		elsif stproc.namelen=4 and eqstring(stproc.name, "main") then
			ms:=modules[stmodule.moduleno]
			ps:=subprogs[stmodule.subprogno]

			if ps.mainmodule then serror("More than one main() in SP") end
			ps.mainmodule:=stmodule.moduleno
			ms.stmain:=stproc

			if ps.subprogno=mainsubprogno then
				stproc.scope:=export_scope
dosigcheck:
				if stproc.paramlist or stproc.mode<>tvoid then
					serror("Wrong 'main/start' sig")
				end

			end
		end
	end

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
	int byref, pmode, m, isoptional, types

	stlist:=stlistx:=nil
	pmode:=tvoid
	nparams:=0
	types:=0

	if lx.symbol=namesym and nextlx.symbol in [commasym, rbracksym] then
		types:=1
	end

	do										!expect type of name at start of loop
		byref:=0
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
					stname:=getduplnameptr(owner, addnamestr(str), paramid)
					adddef(owner, stname)

					storemode(owner, pmode, stname.mode)
					stname.byref:=byref
					addlistparam(&stlist, &stlistx, stname)

					case lx.symbol
					when rbracksym then
						exit
					end case

					checksymbollex(commasym)
!					lex()
					if lx.symbol=ellipsissym then
						varparams:=nparams		!no. of fixed params
						lex()
						exit
					end

					pmode:=readtypespec(procowner)
				end
				return stlist
			end

		elsif pmode=tvoid then
			serror("Type expected")
		end

		case lx.symbol
		when addrsym then
			byref:=1
			lex()
			if lx.symbol=colonsym then lex() end
		when ellipsissym then
			varparams:=nparams
			lex()
			return stlist
		end case

		checksymbol(namesym)
		++nparams
		stname:=getduplnameptr(owner, lx.symptr, paramid)
		adddef(owner, stname)
		lex()

		if byref then
			m:=createrefmode(procowner, pmode)
		else
			m:=pmode
		end

		storemode(owner, m, stname.mode)
		stname.byref:=byref
		stname.optional:=isoptional
		addlistparam(&stlist, &stlistx, stname)

		case lx.symbol
		when assignsym, eqsym then
			lex()
			stname.code:=readunit()
			stname.optional:=1
		end case

		case lx.symbol
		when commasym then
			lex()
		when rbracksym then
			exit
		else
			serror("nameparams1")
		end case
	end

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
!		q.pclop:=knot
		return createunit2(jif, q, createunit1(jblock, p))
	else
		return p
	end case
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
	end
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
			end
		end

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
	end case

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
	end
	checkend(kendsym, kunlesssym)
	p:=createunit3(jif, q:=createunit1(jnotl, pcond), pthen, pelse)
!	q.pclop:=knot
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
	end

	if lx.symbol=kwhensym then
		if kwd=kswitchsym then
			serror("switch expr missing")
		end
		pexpr:=nil
	else
		pexpr:=readsunit()			!index expression
		pexpr.nextunit:=pjump		!for doswitchx
	end

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
			if p.tag=jmakerange then rangeused:=1 end
			addlistunit(pwhen, pwhenx, p)
			if lx.symbol<>commasym then exit end
			lex()
		end
		if lx.symbol<>sendtosym then
			checksymbol(kthensym)
		end
		lex()
		pthen:=readsunit()
		pwhenthen:=createunit2(jwhenthen, pwhen, pthen)
		pwhenthen.pos:=pos2
		addlistunit(pwhenlist, pwhenlistx, pwhenthen)
	end

	if opc=jswitch and not rangeused then
		if nwhen<=8 then
			opc:=jcase
		end
	end

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
	end case

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
	end
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
	end

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
	if currproc.nameid<>procid then id:=staticid end

	p:=createunit3(jto, pcount, pbody, createname(getavname(currproc, id)))
!	p:=createunit2(jto, pcount, pbody)
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
	end

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
	end
	return readcondsuffix(p)
end

func readprint:unit=
	int oldinreadprint, opc, isfprint, fshowname
	unit pformat, pdev, printlist, printlistx, p, q
	^strbuffer expr

	ichar s

	oldinreadprint:=inreadprint
	inreadprint:=1
	opc:=lx.subcode

	case opc
	when jfprint, jfprintln then
		isfprint:=1
	else
		isfprint:=0
	end case

	lex()

	printlist:=printlistx:=nil
	pformat:=pdev:=nil

	if lx.symbol=atsym then
		lex()
		pdev:=readunit()
		if lx.symbol=commasym then lex() else goto finish end
	end
	if isfprint then
		pformat:=readunit()
		if lx.symbol=commasym then lex() else goto finish end
	end

	if not exprstarter[lx.symbol] then
		goto finish
	end

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
			end

			p:=readunit()
			if lx.symbol=colonsym then
				lex()
				p:=createunit2(jfmtitem, p, readunit())
			end
			if fshowname then
				expr:=strexpr(p)
				strbuffer_add(expr, "=")
				s:=expr.strptr
				iconvucn(expr.strptr, expr.length)

				addlistunit(printlist, printlistx, q:=createstringconstunit(s, expr.length+1))
			end
			addlistunit(printlist, printlistx, p)
		end case
		if lx.symbol<>commasym then exit end
		lex()
	end

	finish:
	inreadprint:=oldinreadprint
	if opc=jprint and printlist=nil then
		serror("No print items")
	end
	if opc=jfprint and printlist=nil and pformat=nil then
		serror("No print items")
	end

	if isfprint then
		if pformat=nil then
			serror("No fmt str")
		end
		return createunit3(opc, pdev, pformat, printlist)
	else
		return createunit2(opc, pdev, printlist)
	end
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
		end
		lex()
		pdev:=readunit()
		if lx.symbol=commasym then lex() end
	end

	if opc=jreadln then
		addlistunit(readlist, readlistx, createunit1(jreadln, pdev))
	end

	if not exprstarter[lx.symbol] then
		goto finish
	end

	do
		p:=readunit()
		if lx.symbol=colonsym then
			lex()
			pformat:=readunit()
		else
			pformat:=nil
		end

		pread:=createunit1(jread, pformat)

!

		p:=createunit2(jassign, p, pread)

		addlistunit(readlist, readlistx, p)
		if lx.symbol<>commasym then exit end
		lex()
	end

	finish:
	inreadprint:=oldinreadprint
	if opc=jread and readlist=nil then
		serror("No read items")
	end

	return createunit1(jblock, readlist)
end

func readfor:unit=
!on 'for'; syntax is:
! for [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/end
! for var[, var] in/inrev expr [when expr] do stmts [else stmts] end/od *FORALL*
! for var in/inrev expr.bounds [when expr] do stmts [else stmts] end/end
! for var in/inrev <rangeexpr> [when expr] do stmts [else stmts] end/end

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
	end
	for i to nforloops do
		if forindexvars[i]=pindex.def then
			serror("Re-using nested loop index")
		end
	end
	forindexvars[++nforloops]:=pindex.def

	if lx.symbol=commasym then
		lex()
		plocal:=readname()
	end

	opc:=jforup
	pstep:=nil
	pcond:=nil

	if lx.symbol in [insym, inrevsym] then				!assume forall
		if lx.symbol=inrevsym then
			opc:=jfordown						!tentative; may be changed to forall
		end
		lex()

		plist:=readunit()

		if plist.tag=jmakerange then
			pfrom:=plist.a
			pto:=plist.b
		else
			opc:=(opc=jforup|jforall|jforallrev)
			pfrom:=getrangelwbunit(duplunit(plist))
			pto:=getrangeupbunit(duplunit(plist))
		end

	else
		if lx.symbol=assignsym then
			lex()
			pfrom:=readunit()
		else
			pfrom:=createconstunit(1, tint)
		end
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
				end
			end
		end
	end

	if lx.symbol=kwhensym then
		lex()
		pcond:=fixcond(readunit())
	end
	checksymbollex(kdosym)
!	lex()
	pbody:=readsunit()
	pelse:=nil

	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
	end
	checkend(kendsym, kforsym, kdosym)

!deal with complex limit
!problem: autovar for STEP only created when there is an autovar for TO

	if pcond<>nil then
		pbody:=makeblock(createunit2(jif, pcond, pbody))
	end
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
		if plocal then serror("for i, x?") end
		pindex.avcode:='I'
		if pto.tag not in [jconst, jname] then
			plocal:=createname(getavname(currproc))
			plocal.avcode:='I'
			ptoinit:=createunit2(jassign, plocal, pto)
			pindex.nextunit:=ptoinit
			pto:=plocal
		end

		pfrom.nextunit:=pto
		pto.nextunit:=pstep
		p:=createunit3(opc, pindex, pfrom, pbody)

	else										!assume forall/rev

		if plocal=nil then						!only for x
			plocal:=pindex
			pindex:=createname(getavname(currproc))
		end
		pindex.avcode:='I'
		plocal.avcode:='L'
		pindex.nextunit:=plocal
		plocal.nextunit:=pfrom
		pfrom.nextunit:=pto

		passign:=createunit2(jassign, duplunit(plocal), 
					createunit2(jindex, duplunit(plist), duplunit(pindex)))
		plist.nextunit:=passign

		p:=createunit3(opc, pindex, plist, pbody)

	end case

	p.pos:=pos
	--nforloops
	return p
end

func readname:unit p=
	p:=readterm2()
	if p.tag<>jname then serror("Name expected") end
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
		end
	else
		storemode(owner, t, tttarget[m])
	end

	if t>=0 then
		copyttvalues(m, t)
	else
		ttbasetype[m]:=tpending
	end
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
		end
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
				if offset>stname.equivoffset.max then serror("Offset>255") end
				stname.equivoffset:=offset
			end

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

		end case

		if lx.symbol<>commasym then
			exit
		end
		lex()
	end

	if nvars=0 then
		serror("No fields declared")
	end
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
		if not enums then serror("Use 'enumdata'") end
		enums:=1
		lex()
		checksymbollex(rbracksym)
!		lex()
	end


	nextenumvalue:=1
	nrows:=0			!number of data rows appearing
	ncols:=0			!number of data columns (varnames appearing)

!loop reading variable names
	while lx.symbol<>eqsym do
		ltype:=readtypespec(owner)
		checksymbol(namesym)
		if ++ncols>maxcols then
			serror("tabledata/too many columns")
		end
		varnameptrs[ncols]:=lx.symptr
		varlisttypes[ncols]:=ltype

		lex()
		if lx.symbol=commasym then
			lex()
		else
			exit
		end
	end

	lex()					!skip =

	skipsemi()
	startline:=getcurrline()
	closesym:=checkbegin(0)

	skipsemi()
	firstval:=lastval:=0

	for i:=1 to ncols do
		plist[i]:=plistx[i]:=nil
	end

	intabledata:=1
	do			!loop per row
		skipsemi()
		if ncols>0 then
			checksymbollex(lbracksym)
!			lex()
		end
		if ++nrows>maxrows then
			serror("tabledata:too many rows")
		end

		if enums then
			checksymbol(namesym)
			stgen:=lx.symptr				!generic symbol entry
			tabledataname:=stgen.name		!allow to be picked up by $ lx.symbol
			lex()
			if lx.symbol=eqsym then
				if nrows<>1 then serror("enum=x, 1st row only") end
				lex()
				nextenumvalue:=readconstint()
			end
			enumvalues[nrows]:=nextenumvalue

			stenum:=getduplnameptr(owner, stgen, constid)
			stenum.mode:=tint
			stenum.code:=createconstunit(nextenumvalue, tint)
			stenum.scope:=scope
			adddef(owner, stenum)
			if scope=export_scope then
				addlinear(stenum)
			end

			if nrows=1 then firstval:=nextenumvalue end
			lastval:=nextenumvalue

			++nextenumvalue
			if ncols then				!comma always expected
				checksymbollex(commasym)		!check it
!				lex()
			end
		end

		for i:=1 to ncols do
			addlistunit(plist[i], plistx[i], readunit())
			if i=ncols then
				checksymbollex(rbracksym)
			else
				checksymbollex(commasym)
			end
!			lex()
		end

		if lx.symbol<>commasym then exit end
		lex()					!should be ( for next entry
		if lx.symbol=closesym then exit fi		!allow trailing comma on last entry
	end

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

	if nrows=0 then serror("No table data") end

!for each variable, add a vardef initialised to the list
!add the decls for the vars

	for i:=1 to ncols do

		stvar:=getduplnameptr(owner, varnameptrs[i], staticid)
		stvar.code:=createunit1(jmakelist, plist[i])
		stvar.code.length:=nrows
!		stvar.istabdata:=1

		storemode(owner, varlisttypes[i], stvar.mode)
		stvar.scope:=scope

		adddef(owner, stvar)
		addlinear(stvar)
	end
end

global proc readclassdef(symbol owner, int scope)=
!at 'class' symbol
!read enough of the class to be able to generate export data
	int kwd, m, startline, closesym, mrec, isrecord, align
	symbol nameptr, sttype

	kwd:=lx.symbol
	isrecord:=kwd=krecordsym

	lexchecksymbol(namesym)
	nameptr:=lx.symptr

	lex()
	checkequals()
	lex()

	align:=0
	if lx.symbol=atsym and lx.subcode=1 then	!$caligned only
		lex()
		align:=1
	end

	sttype:=getduplnameptr(owner, nameptr, typeid)
	adddef(owner, sttype)
	m:=createusertype(sttype)

	mrec:=createrecordmode(owner, m)
	storemode(owner, mrec, sttype.mode)

!	storemode(owner, baseclass, sttype.baseclass)
	sttype.caligned:=align

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
		end
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
		if lx.symbol=lbracksym then ++lbcount; lex() end
	when kendsym, rbracksym then
		if unionstring.ulength then
			if lx.symbol=rbracksym and lbcount then
				lex()
				--lbcount
			else
				checkend(kendsym, (unionstr_last(&unionstring)='S'|kstructsym|kunionsym))
			end
			if unionlastvar=nil or unionpend.ulength then
				serror("Empty union group")
			end
			case unionstr_last(&unionlastvar.uflags)
			when 'E', '*' then
			else
				unionstr_append(&unionlastvar.uflags, '*')
			end case
			unionstr_append(&unionlastvar.uflags, 'E')
			unionstring.ulength--
		else
			exit
		end

	when kvarsym then

		lex()
		if istypestarter() then
	readmut:
			++insiderecord
			t:=readtypespec(owner)
			--insiderecord
		else
			t:=tauto
		end
		readrecordfields(owner, t)

	when kletsym then
		serror("Let not allowed")

	else
		if istypestarter() then
			goto readmut
!		serror("record:need var")
		else
			exit
		end
	end docase

	if lbcount then serror("LB?") end

end

proc readimportmodule(symbol owner)=
!at 'importmodule' symbol
	int isnew, startline, closesym
	symbol stname, stname0

	if insidedllimport then serror("nested importdll") end
!	libtype:=lx.subcode

	lex()
	if lx.symbol=stringconstsym then
		stname:=addnamestr(lx.svalue)
	else
		checksymbol(namesym)
		stname:=lx.symptr
	end

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
		end
	end

	if isnew then			!new
		addlib(stname.name)
	end

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
!	const scope = global_scope
	const scope = program_scope

	pos:=lx.pos

	do
		skipsemi()
		case lx.symbol
		when kprocsym, kfuncsym then
doproc:
			d:=readprocdecl(owner, program_scope)
			if ndllproctable>=maxdllproc then
				serror("Too many dll procs")
			end
			dllproctable[++ndllproctable]:=d
			addlinear(d)

		when ktypesym then
			readtypedef(owner, scope)

		when kconstsym then
			readconstdef(owner, scope)

		when krecordsym then
			readclassdef(owner, scope)

		when kvarsym then
			lex()
			readvardef(owner, scope, 0, dllvarid, kvarsym)

		when stdtypesym, namesym, krefsym, ptrsym, kicharsym, lsqsym, kslicesym then
			readvardef(owner, scope, 0, dllvarid, 0)

		when eofsym then
			exit

		when kendsym then
			exit
		else
			PS("symbol")
			serror("Not allowed in importmodule")
		end case
	end
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
		end

		p:=p.nextdef
	end
	cpl d.name
	serror("Can't find @ field")
	return nil
end

func readrefproc(symbol owner, int typedefx)int=
!'ref' was seen, now positioned at 'proc' 'func' or 'method'
!read proc params and any result, return a complete ^proc spec
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
			end
			lex()
			if lx.symbol=colonsym or lx.symbol=sendtosym then
				lex()
				nretvalues:=readreturntype(stproc, retmodes)
			elsif typestarterset[lx.symbol] or lx.symbol=namesym then
				nretvalues:=readreturntype(stproc, retmodes)
			end
		elsif lx.symbol=colonsym or lx.symbol=sendtosym then
			lex()
			nretvalues:=readreturntype(stproc, retmodes)
		end
		if nretvalues=0 then
			serror("Function needs return type")
		end

		if nretvalues and kwd=kprocsym then		!proc: result given
			serror("Proc can't return value")
		end
	else					!proc with no result
		if lx.symbol=lbracksym then		!possible params
			lex()
			if lx.symbol<>rbracksym then
				paramlist:=readparams(owner, stproc, varparams, nparams)
				checksymbol(rbracksym)
			end
			lex()
		end
		if typestarterset[lx.symbol] or lx.symbol=colonsym or lx.symbol=sendtosym then
			serror("proc can't have ret value")
		end
	end

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
	end
	procstack[++nprocstack]:=currproc
	currproc:=p
end

proc popproc=
	if nprocstack then
		currproc:=procstack[nprocstack--]
	else
		currproc:=stmodule
	end
end

func readreturntype(symbol owner, []int &retmodes)int=
!read 1..maxtuplesize return types as part of func decl
	int nretvalues

	retmodes[1]:=readtypespec(owner)
	nretvalues:=1
	while lx.symbol=commasym do
		if nretvalues>=maxtuplesize then
			serror("Too many return values")
		end
		lex()
		retmodes[++nretvalues]:=readtypespec(owner)
	end

	return nretvalues
end

func readset:unit=
!positioned at "["
	int length
	unit p, ulist, ulistx

	lex()					!first symbol of first expression

	case lx.symbol
	when rsqsym then		!empty set, same as 0
		lex()
		return createunit1(jmakeset, nil)
	end case

	length:=0

	ulist:=ulistx:=nil

	do
		p:=readunit()
		++length

		addlistunit(ulist, ulistx, p)

		case lx.symbol
		when commasym then
			lex()
			if lx.symbol=rsqsym then exit end
		when semisym then
			lexchecksymbol(rsqsym)
			exit
		when rsqsym then
			exit
		else
			serror("readset?")
		end case
		skipsemi()						!allow a, b, c;]
	end
	lex()

!	if nkeyvalues then
!		if length>nkeyvalues then serror("dict: mixed elements") end
!		p:=createunit1(jmakedict, ulist)
!	else
		p:=createunit1(jmakeset, ulist)
!	end
	p.length:=length
	return p
end

func istypestarter:int=
	if typestarterset[lx.symbol] then return 1 end
	if lx.symbol=namesym then				!name ...
		case nextlx.symbol
		when namesym then					!name name
			return 1
		when addrsym then
			return 1
		end case
	end
	return 0
end

global func readunit:unit p=
	unit pt
	int pos

	pos:=lx.pos
	pt:=readterm2()

	IF FASTEXPR THEN
		if jisexpr[pt.tag]=0 then
			return pt
		end

		if endsexpr[lx.symbol] then
			return pt
		end
	FI

	if lx.symbol=assignsym then
		lex()
		p:=readterm2()
		IF FASTEXPR THEN
			if endsexpr[lx.symbol] then
				p:=createunit2(jassign, pt, p)
				p.pos:=pos
				return p
			end
		FI
		p:=createunit2(jassign, pt, readassignment(p))
	else

		p:=readassignment(pt)
		p.pos:=pos
	end

	while lx.symbol=pipesym do
		lex()
		p:=createunit2(jcall, readassignment(), p)
	end

	return p
end

func readassignment(unit pt=nil)unit p=
	int pos
	unit q

	p:=readorterms(pt)

	if lx.symbol = assignsym then
		pos:=lx.pos
		lex()
		q:=readassignment(nil)
		p:=createunit2(jassign, p, q)
		p.pos:=pos
	end
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
		end

		p:=createunit2(jorl, p, readandterms())
		p.pos:=pos
	end

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
		end

		p:=createunit2(jandl, p, readcmpterms())
		p.pos:=pos
	end

	return p
end

func readcmpterms(unit pt=nil)unit p=
	int pos, opc, n
	unit ulist, ulistx, q
	[8]byte genops

	p:=readinterms(pt)

	if lx.symbol not in [eqsym, cmpsym] then
		return p
	end

	ulist:=ulistx:=p
	p:=createunit1(jcmpchain, p)
	n:=0				!n counts tclopnd after the first
	clear genops

	docase lx.symbol
	when eqsym, cmpsym then
		++n
		if n>genops.len then serror("cmpchain: Too many items") end
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
		p.condcode:=genops[1]
		p.b:=q.nextunit
		q.nextunit:=nil
	else
		p.cmpgenop:=genops
	end

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

		p:=createunit2(jinrange, p, readrangeterm())		!can change to inset later

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
	end

	return p
end

func readaddterms(unit pt=nil)unit p=
	int pos, tag, pclop

	p:=readmulterms(pt)

	while symboladdmul[lx.symbol]='A' do
		pclop:=symbolgenops[lx.symbol]
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto, p, readassignment())
			p.pclop:=pclop
			p.pos:=pos
			exit
		end

		p:=createunit2(jbin, p, readmulterms())
		p.pclop:=pclop
		p.pos:=pos
	end

	return p
end

func readmulterms(unit pt=nil)unit p=
	int pos, pclop

	p:=readpowerterms(pt)

	while symboladdmul[lx.symbol]='M' do
		pclop:=symbolgenops[lx.symbol]
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto, p, readassignment())
			p.pclop:=pclop
			p.pos:=pos
			exit
		end

		p:=createunit2(jbin, p, readpowerterms())
		p.pclop:=pclop
		p.pos:=pos
	end

	return p
end

func readpowerterms(unit p=nil)unit=
	int pos

	if p=nil then
		p:=readterm2()
	end

	while lx.symbol=powersym do
		pos:=lx.pos
		lex()
		p:=createunit2(jbin, p, readpowerterms())
		p.pclop:=kpower
		p.pos:=pos
	end

	return p
end

func readterm2:unit=
	unit p, q, r
	^char pbyte
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
		end
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
		if inreadprint then exit end
		lex()
		q:=readunit()

		if inparamlist then
			p:=createunit2(jkeyword, p, q)
		else
			serror("A:B?")
		end

	when incrsym then
		case lx.subcode
		when kincrto then opc:=kloadincr
		when kdecrto then opc:=kloaddecr
		end case
		lex()
		p:=createunit1(jincr, p)
		p.pclop:=opc

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
		end

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
		if length>8 then serror("Char const too long") end
		a:=0
		if length then
			memcpy(&a, lx.svalue, length)
		end
		p:=createconstunit(a, tc64)
		lex()

	when lbracksym then
		p:=readlbrack()

	when stdtypesym, krefsym, ptrsym, kicharsym then
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
!			p.pclop:=knot
		end

	when istruelsym then
		if nextlx.symbol=assignsym then
			p:=readopc()
		else
			lex()
			p:=createunit1(jistruel, readterm2())
		end

	when lsqsym then
		p:=readset()

	when incrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(jincr, readterm2())
		p.pclop:=opc

	when addrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(opc, readterm2())
		if p.a.tag=jcall then
			if p.a.b then
				serror("Params not allowed")
			end
			p.a:=p.a.a			!lose the call
		end

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
				end
				p:=createstringconstunit(s, -1)
			end
		else
			if ndollar<=0 then
				serror("[$] No array")
			end
			p:=createunit1(jprop, dollarstack[ndollar])
			p.propcode:=kkupb
		end
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
		end
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
CPL "MAKESLICE"
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
					end
					checksymbollex(commasym)
!					lex()
				else
					serror("macro def params")
				end case
			end
		end
		lex()						!skip )
	end
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
	end
end

func fixcond(unit p)unit=
	checknotempty(p)
	if not isbooltag[p.tag] then
		insertunit(p, jistruel)
!		p.convcode:=kktoboolt
	end
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
		end
		switch lx.symbol
		when kstaticsym then
			lex()
			if lx.symbol in [kletsym, kvarsym] then
				opc:=lx.symbol
				lex()
			else
!			opc:=kmutsym
				opc:=0
			end
			readvardef(currproc, 0, 1, staticid, opc)

		when kprocsym, kfuncsym then
			readprocdef(currproc, 0)

		when stdtypesym, krefsym, ptrsym, kicharsym, kslicesym, lsqsym then
			if nextlx.symbol in [lbracksym, atsym, dotsym] then		!is a cast etc
				goto doexec
			else
				sym:=0
				goto dovar
			end

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
			end

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
			end case
		when kdosym then				!u;u;u;do rather than u;u;u do
			if inwhile then
				exit
			end
			goto doexec

		when semisym then

		else							!assume a statement
	doexec:
			p:=readunit()
	doexec2:
			if p.tag=jname and lx.symbol=namesym then
				serror("Possibly var/let needed")
			end
	doexec3:
			addlistunit(ulist, ulistx, p)
			if lx.symbol=kdosym then
				exit
			end

		end switch
	until lx.symbol<>semisym

	case lx.symbol
	when rbracksym, kthensym, kelsifsym, kelsesym, kuntilsym, kwhensym, kdosym, 
		kelsecasesym, kelseswitchsym, kendsym, commasym, 
		barsym then
	else
		serror("Readsunit: "";"" expected, or bad unit starter")
	end case

	if ulist=nil or ulist.nextunit then
		return createunit1(jblock, ulist)
	else
		return ulist
	end
end

proc checknotempty(unit p)=
	if p=nil or p.tag=jblock and p.a=nil then
		serror("Empty sunit")
	end
end
=== mm_pclblock.m 0 0 47/74 ===
!blockpcl

const dodotchains=1
!const dodotchains=0

INT NNN

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

global symbol pnprocs, pprocname, pprocaddr


global macro getmemmode_m(p) = (p.memmode|p.memmode|p.mode)
macro evallv(p) = evalref(p)
macro evalunitx(p, isref) = (isref|evalref(p)|evalunit(p))
macro evalblock(p) = evalunit(p)

!global proc genpcl=
!!generate pcl code for each function
!	ref procrec pp
!	symbol d
!	int tt:=clock()
!
!	pp:=proclist
!	while pp do
!		d:=pp.def
!		genprocpcl(d)
!!++NPROCS
!!PCLLENGTH+:=D.PCLINFO.LENGTH
!
!		pp:=pp.nextproc
!	od
!
!!CPL =NPROCS
!!CPL =PCLLENGTH
!!CPL real (PCLLENGTH)/nprocs
!!CPL
!
!	pcltime:=clock()-tt
!end
!



global proc evalunit(unit p)=
!p is a single executable unitrec; not a list or const
!should be called via execblock() to executate a code item in a unitrec
	unit a, b, c

	if p=nil then return fi

!CPL "EVALUNIT", JTAGNAMES[P.TAG]


	mmpos:=p.pos

	a:=p.a
	b:=p.b
	c:=p.c

	switch p.tag
	when jconst         then do_const(p)
	when jnull          then
	when jname          then do_name(p)
	when jblock then
		do_block(p)
	when jcall          then
		do_callproc(p, a, b)
	when jreturn        then do_return(p, a)
	when jreturnmult    then do_returnmult(p, a)
	when jassign        then do_assign(p, a, b)
	when jassignms      then do_assignms(a, b)
	when jassignmm      then do_assignmm(a, b)
	when jassignmdrem   then do_assignmdrem(a, b)
	when jto            then do_to(p, a, b)
	when jif            then do_if(p, a, b, c, 0)
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
	when jcase          then do_case(p, a, b, c, 0, 0)
	when jdocase        then do_case(p, a, b, c, 1, 0)
	when jswitch, jdoswitch, jdoswitchu, jdoswitchx then
		do_switch(p, a, b, c)
	when jrecase        then do_recase(p, a)
	when jswap          then do_swap(p, a, b)
	when jselect        then do_select(p, a, b, c, 0)
	when jprint, jprintln then
		do_print(p, a, b)
	when jfprint, jfprintln then
		do_print(p, a, b)
	when jread	        then do_read(p, a)
	when jreadln        then do_readln(a)
	when jstop          then do_stop(p, a)
	when jeval          then
		evalunit(a)
		pc_gen(keval)
		pc_setmode_u(a)

	when jandl          then do_andl(p, a, b)
	when jorl           then do_orl(p, a, b)
	when jcmp           then do_setcc(p, a, b)
	when jcmpchain      then do_setccchain(p, a)

	when jbin           then do_bin(p, a, b)
	when jindex         then do_index(p, a, b)

	when jdotindex      then do_dotindex(p, a, b)
	when jdotslice      then do_dotslice(p, a, b)
	when jdot           then do_dot(p)
	when jptr           then do_ptr(p, a)
	when jaddrof        then evalref(a, b)

	when jconvert       then do_convert(p, a)
	when jtypepun       then do_typepun(p, a)
	when jshorten       then do_shorten(p, a)
	when jtypeconst     then do_typeconst(p)

	when junary         then do_unary(p, a)

	when jnotl          then do_notl(p, a)
	when jistruel       then do_istruel(p, a)
	when jisfalsel      then do_isfalsel(p, a)

	when jincr          then
		if p.pclop in [kincrto, kdecrto] then
			do_incr(p, a)
		else
			do_incrload(p, a)
		fi
!
	when jbinto         then do_binto(p, a, b)
!
	when junaryto       then do_unaryto(p, a)
!
	when jsyscall then
		do_syscall(p, a)

	when jclear         then do_empty(p, a)

	when jslice then
		do_slice(p, a, b)

	else
		GERROR_S("UNSUPPORTED TAG ", JTAGNAMES[P.TAG])
		return
	end switch

!CPL "  EVALUNIT2", JTAGNAMES[P.TAG]
!CPL "EVALU", JTAGNAMES[P.TAG], STRMODE(P.MODE), =P.RESULTFLAG

	if p.mode<>tvoid and not p.resultflag then
!CPL "RESULT CREATED BUT NOT USED"
		case p.tag
		when jassign, jcall, jsyscall then
!CPL "  EVALA"

		else
!CPL "  EVALB"
IF NOT JSOLO[P.TAG] THEN
PRINTUNIT(P)
GERROR(ADDSTR("NOT ALLOWED BY ITSELF:", JTAGNAMES[P.TAG]))
FI

			pc_gen(kunload)
			pc_setmode_u(p)
		esac
	fi
!CPL "  EVALUNIT3", JTAGNAMES[P.TAG]

end

proc evalref(unit p, q=nil)=
	unit a, b, c
	a:=p.a
	b:=p.b
	c:=p.c
	mmpos:=p.pos

	case p.tag
	when jname then
		genpushmemaddr_d(p.def)
		pc_setmode(tu64)
		if q then					!addrof may have optional byte offset
			genpushint(q.value)
			pc_genix(kaddpx)
			pc_setmode(tu8)
		fi

	when jindex then
		do_indexref(a, b)

	when jdot then
		do_dotref(p)

	when jptr then
		evalunit(p.a)

	else
		case p.tag
		when jif then
			do_if(p, a, b, c, 1)
!		when jselect then
!			do_select(p, a, b, c, 1)
!		when jswitch then
!			do_switch(p, a, b, c, 1)
!		when jcase then
!			do_case(p, a, b, c, 0, 1)
!		elsif ttisblock[p.mode] then
!			evalunit(p)

		else
			PRINTUNIT(P)
			gerror("evalref")
		esac
	esac
end

proc evalarray(unit p)=
	case ttbasetype[p.mode]
	when tslice then
		evalref(p)
		pc_gen(kiload)
		pc_setmode(tu64)
	elsif p.mode=trefchar then
		evalunit(p)
	else
		evalref(p)
	esac

end

proc do_block(unit p)=
	unit a:=p.a

	while a do
		evalunit(a)
		a:=a.nextunit
	od
end

proc genjumpcond(int opc, unit p, int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
	unit q, r, s
	int lab2, i

	q:=p.a
	r:=p.b

	case p.tag
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
			evalunit(q)
			q:=q.nextunit
		od
		genjumpcond(opc, q, lab)

	when jcmp then

		gcomparejump(opc, p.condcode, q, r, lab)

	when jinrange then
		evalunit(q)

		if opc=kjumpt then
			lab2:=createfwdlabel()
			evalunit(r.a)
			pc_gencond(kjumpcc, lt_cc, pgenlabel(lab2))
			pc_setmode_u(q)
			pccurr.popone:=1
			evalunit(r.b)
			pc_gencond(kjumpcc, le_cc, pgenlabel(lab))
			pc_setmode_u(q)
			definefwdlabel(lab2)
		else
			evalunit(r.a)
			pc_gencond(kjumpcc, lt_cc, pgenlabel(lab))
			pc_setmode_u(q)
			pccurr.popone:=1
			evalunit(r.b)
			pc_gencond(kjumpcc, gt_cc, pgenlabel(lab))
			pc_setmode_u(q)
		fi

	when jinset then
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
					pc_gencond(kjumpcc, eq_cc, pgenlabel(lab2))
					pccurr.popone:=1
				else
					pc_gencond(kjumpcc, ne_cc, pgenlabel(lab))
				fi
				pc_setmode_u(q)
			od
			definefwdlabel(lab2)
		else
			evalunit(q)

			while s, s:=s.nextunit do
				evalunit(s)
				pc_gencond(kjumpcc, eq_cc, pgenlabel(lab))
				pc_setmode_u(q)
				if s.nextunit then pccurr.popone:=1 fi
			od
		fi

	when jcmpchain then
		r:=q.nextunit
		i:=1
		evalunit(q)
		if opc=kjumpf then
			while r do
				evalunit(r)
				if r.nextunit then
					pc_genxy(kswapstk, 1, 2)

					pc_gencond(kjumpcc, reversecond_order(reversecond(p.cmpgenop[i])), pgenlabel(lab))

					pccurr.popone:=1
				else
					pc_gencond(kjumpcc, reversecond(p.cmpgenop[i]), pgenlabel(lab))
				fi

				pc_setmode_u(q)
				++i
				q:=r
				r:=r.nextunit
			od
		
		else
			lab2:=createfwdlabel()
			while r do
				evalunit(r)
				if r.nextunit then
					pc_genxy(kswapstk, 1, 2)
					pc_gencond(kjumpcc, reversecond_order(reversecond(p.cmpgenop[i])), pgenlabel(lab2))
					pccurr.popone:=1
				else
					pc_gencond(kjumpcc, p.cmpgenop[i], pgenlabel(lab))
				fi
				pc_setmode_u(q)
				++i
				q:=r
				r:=r.nextunit
			od
			definefwdlabel(lab2)
		fi
	else			!other, single expression
		evalunit(p)
		pc_gen(opc, pgenlabel(lab))
		if ttisblock[p.mode] then gerror("jumpt/f") fi
		pc_setmode(p.mode)
	end
end

proc gcomparejump(int jumpopc, int cond, unit lhs, rhs, int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	if jumpopc=kjumpf then			!need to reverse condition
		cond:=reversecond(cond)		!eqop => neop, etc
	fi

	evalunit(lhs)
	evalunit(rhs)

	pc_gencond(kjumpcc, cond, pgenlabel(lab))
	pc_setmode_u(lhs)
end

proc genjumpl(int lab)=
!generate unconditional jump to label
	pc_gen(kjump, pgenlabel(lab))
end

proc unimpl(ichar mess)=
	gerror_s("Unimplemented: #", mess)
end

proc do_const(unit p) =
	int mode:=p.mode

	if ttisinteger[mode] or mode=tbool then
		genpushint(p.value)
	elsif ttisreal[mode] then
		genpushreal(p.xvalue, getpclmode(mode))

	elsif ttisref[mode] then
		if p.isastring then
!CPL "doconst/CONST3", P.SVALUE, p.strtype
if p.strtype='B' then gerror("1:B-str?") fi

			genpushstring(p.svalue)
		else
			genpushint(p.value)
		fi
	else
		gerror("do_const")
	fi
	pc_setmode(mode)
end

proc do_name(unit p)=
	symbol d

	d:=p.def
	case d.nameid
	when procid, dllprocid then
		genpushmemaddr_d(d)
		pc_setmode(tu64)
	when labelid then
		if d.index=0 then
			d.index:=++mlabelno
		fi
		if p.resultflag then		!get label address
			pc_gen(kload, pgenlabel(d.index))
			pc_setmode(tu64)
		else
			pc_gen(kjump, pgenlabel(d.index))
			p.mode:=tvoid
			p.resultflag:=0
		fi

	when fieldid then
		genpushint(d.offset)
		pc_setmode(ti64)

	else
		genpushmem_d(d)
		pc_setmode(d.mode)

	esac
end

proc do_stop(unit p, a) =
	if a then
		evalunit(a)
	else
		pc_gen(kload, pgenint(0))
		pc_setmode(ti64)
	fi
	pc_gen(kstop)
end

proc do_andl(unit p, a, b) =
	int labfalse, labend

	pc_gen(kstartmx)

	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpf, a, labfalse)
	genjumpcond(kjumpf, b, labfalse)

	genpushint(1)
	pc_gen(kresetmx)
	pc_setmode(ti64)

	genjumpl(labend)

	definefwdlabel(labfalse)
	genpushint(0)
	pc_gen(kendmx)
	pc_setmode(ti64)

	definefwdlabel(labend)
end

proc do_orl(unit p, a, b) =
	int labtrue, labfalse, labend

	pc_gen(kstartmx)
	labtrue:=createfwdlabel()
	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpt, a, labtrue)
	genjumpcond(kjumpf, b, labfalse)

	definefwdlabel(labtrue)
	genpushint(1)
	pc_gen(kresetmx)
	pc_setmode(ti64)
	genjumpl(labend)

	definefwdlabel(labfalse)
	genpushint(0)
	pc_gen(kendmx)
	pc_setmode(ti64)

	definefwdlabel(labend)
end

proc do_notl(unit p, a) =
	evalunit(a)
	pc_gen(p.pclop)
	pc_setmode(ti64)
end

proc do_istruel(unit p, a) =
	evalunit(a)
!	if a.mode=tbool then
!		return
!	fi
	pc_gen(ktoboolt)
	pc_setmode_u(p)
	pc_setmode2(a.mode)
end

proc do_isfalsel(unit p, a) =
	evalunit(a)
!	if a.mode=tbool then
!		return
!	fi
	pc_gen(ktoboolf)
	pc_setmode_u(p)
	pc_setmode2(a.mode)
end

proc do_typepun(unit p, a) =
	evalunit(a)

	if a.tag=jname then
		a.def.addrof:=1
	fi

	if a.mode=p.mode then return fi
	pc_gen(ktypepun)
	pc_setmode(p.oldmode)
	pc_setmode2(a.mode)
end

proc do_shorten(unit p, a) =
	evalunit(a)
end

proc do_assign(unit p, a, b) =
!fstore=1 when result is needed

!cpl "ASSIGN", B.RESULTFLAG
!PRINTUNIT(P)

	if a.tag=jname and not a.def.used then
		RETURN
	FI

	case b.tag
	when jmakelist then					!a:=(x, y, z)
		if not p.resultflag then
			do_assignblock(p, a, b)		!(avoids pushing/popping block data)
			return
		fi

!	when jslice then					!a:=b[i..j] etc
!		if a.tag=jname then
!			doslice(b, b.a, b.b, a.def, p.resultflag)
!			return
!		fi
	esac

	case a.tag
	when jindex then
		do_storeindex(p, a.a, a.b, b)
		return
	when jslice then
GERROR("ASS/SLICE")

	when jdot then
		do_storedot(a, a.b, b)
		return
	esac

	evalunit(b)
	if p.resultflag then
		pc_gen(kdouble)
	fi

	case a.tag
	when jname then
		if a.def.nameid in [procid, dllprocid, labelid] then GERROR("Assign to proc?") fi

		pc_gen(kstore, genmem_u(a))

	when jptr then
		evalref(a)
		pc_gen(kistore)
		pc_setmode_u(a)

	when jdotindex then
		evalref(a.a)
		evalunit(a.b)
		pc_gen(kstorebit)
		pc_setmode_u(a.a)
		return
	when jdotslice then
		evalref(a.a)
		evalunit(a.b.a)
		evalunit(a.b.b)
		pc_gen(kstorebf)
		pc_setmode_u(a.a)
		return

	when jif then
		do_if(a, a.a, a.b, a.c, 1)
		pc_gen(kistore)
		pc_setmode_u(a)


	else
		cpl jtagnames[a.tag]
		gerror("Can't assign")
	esac

	pc_setmode_u(a)
end

proc do_bin(unit p, a, b) =
	evalunit(a)
	evalunit(b)

	if p.pclop in [kaddpx, ksubpx] then
		pc_genix(p.pclop, ttsize[tttarget[a.mode]])
	else
		pc_gen(p.pclop)
		if p.pclop=ksubp then
			pc_setscaleoff(ttsize[tttarget[a.mode]])
		fi
	fi

	pc_setmode_u(p)
end

proc do_setcc(unit p, a, b) =
	evalunit(a)
	evalunit(b)
	pc_gencond(ksetcc, p.condcode)
	pc_setmode_u(a)
end

proc do_setccchain(unit p, q) =
	int lab1, lab2, i, cond
	unit r

	lab1:=createfwdlabel()
	lab2:=createfwdlabel()

	r:=q.nextunit
	i:=1

	pc_gen(kstartmx)

	evalunit(q)
	while r do
		evalunit(r)
		cond:=reversecond(p.cmpgenop[i])
		if r.nextunit then
			pc_genxy(kswapstk, 1, 2)
			cond:=reversecond_order(cond)
		fi

		pc_gencond(kjumpcc, cond, pgenlabel(lab1))
		if r.nextunit then pccurr.popone:=1 fi

		pc_setmode_u(q)
		++i
		q:=r
		r:=r.nextunit
	od

	genpushint(1)
	pc_gen(kresetmx)
	pc_setmode(ti64)
	pc_gen(kjump, pgenlabel(lab2))

	definefwdlabel(lab1)
	genpushint(0)
	pc_gen(kendmx)
	pc_setmode(ti64)
	definefwdlabel(lab2)
end

proc do_binto(unit p, a, b)=
	evalunit(b)
	evallv(a)
	do_setinplace()

	pc_gen(getto_op(p.pclop))
	pc_setmode_u(a)

	if ttisref[a.mode] and ttisinteger[b.mode] then
		pc_setscaleoff(ttsize[tttarget[a.mode]])
	fi
end

proc do_unary(unit p, a) =
	int adj
!
	if ttbasetype[a.mode]=tslice then
		evalref(a)
		case p.propcode
		when kklen, kkupb then
			genpushint(8)
			pc_genix(kiloadx, 1, 0)
			pc_setmode(ti64)
			if p.propcode=kkupb then
				adj:=ttlower[a.mode]-1
				if adj then
					genpushint(adj)
					pc_gen(kadd)
					pc_setmode(ti64)
				fi
			fi

		when kksliceptr then
			pc_gen(kiload)
			pc_setmode(tu64)

		esac

		return
	fi

	evalunit(a)
	pc_gen(p.pclop)
	pc_setmode_u(a)
end

proc do_unaryto(unit p, a)=
	evallv(a)
	do_setinplace()

	pc_gen(getto_op(p.pclop))
	pc_setmode_u(a)
end

func getto_op(int opc)int=
	static [,2]byte table = (
	(kadd,       kaddto),
	(ksub,       ksubto),
	(kmul,       kmulto),
	(kdiv,       kdivto),
	(kidiv,      kidivto),
	(kirem,      kiremto),
	(kbitand,    kbitandto),
	(kbitor,     kbitorto),
	(kbitxor,    kbitxorto),
	(kshl,       kshlto),
	(kshr,       kshrto),
	(kmin,       kminto),
	(kmax,       kmaxto),
	(kaddpx,     kaddpxto),
	(ksubpx,     ksubpxto),

	(kneg,       knegto),
	(kabs,       kabsto),
	(kbitnot,    kbitnotto),
	(knot,       knotto))
!	(ktobool,    ktoboolto))

	for i to table.len do
		if opc=table[i,1] then
			return table[i,2]
		fi
	od

	gerror("No -to op")
	0
end


proc do_ptr(unit p, a)=
	evalunit(a)
	pc_gen(kiload)
	pc_setmode_u(p)
end

proc do_labeldef(unit p)=
	symbol d
	[256]char str

	d:=p.def
	if d.index=0 then
		d.index:=++mlabelno
	fi

	pc_gen(klabeldef, genmemaddr_d(d))

	pc_gen(klabel, pgenlabel(d.index))
end

proc do_goto(unit a)=
	symbol d

	if a.tag=jname and a.def.nameid=labelid then
		d:=a.def
		if d.index=0 then
			d.index:=++mlabelno
		fi
		pc_gen(kjump, pgenlabel(d.index))
	else
		evalunit(a)
		pc_gen(kijump)
	fi
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
	unit cvar
	int lab_b, lab_c, lab_d, count

	cvar:=p.c

	a.mode:=ti64

	evalunit(a)
	pc_gen(kstore, genmem_u(cvar))
	pc_setmode(ti64)

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	stacklooplabels(lab_b, lab_c, lab_d)

!check for count being nonzero
	if a.tag<>jconst then			!assume const limit is non-zero
		evalunit(cvar)
		evalunit(pzero)

		pc_gencond(kjumpcc, le_cc, pgenlabel(lab_d))
		pc_setmode(ti64)

	else
		count:=a.value
		if count<=0 then
			genjumpl(lab_d)
		fi
	fi

	definefwdlabel(lab_b)
	evalblock(b)			!main body

	definefwdlabel(lab_c)

	pc_gen(kto, pgenlabel(lab_b))
	pc_setmode(ti64)
	pc_gen(kopnd, genmem_u(cvar))
	pc_setmode(ti64)

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

	index:=p.loopindex
	if index=0 then index:=loopindex fi

	n:=findlooplabel(k, index)
	if n=0 then
		gerror("Bad exit/loop index", p)
	else
		genjumpl(n)
	fi
end

proc do_if(unit p, pcond, plist, pelse, int isref) =
	int labend, i, lab2, ismult

	labend:=createfwdlabel()
	ismult:=p.mode<>tvoid

	i:=0
	if ismult then pc_gen(kstartmx) fi

	while pcond, (pcond:=pcond.nextunit; plist:=plist.nextunit) do
		++i
		lab2:=createfwdlabel()

		docond(kjumpf, pcond, lab2)

		evalunitx(plist, isref)
		if ismult then pc_gen(kresetmx); pc_setmode_u(p) fi

		if pcond.nextunit or pelse then
			genjumpl(labend)
		fi
		definefwdlabel(lab2)
	od

	if pelse then
		evalunitx(pelse, isref)
		if ismult then pc_gen(kendmx); pc_setmode_u(p) fi
	fi
	definefwdlabel(labend)
end

proc do_return(unit p, a) =
	if a then
		evalunit(a)

		pc_gen(kjumpret, pgenlabel(retindex))
		pc_setmode_u(a)
	else
		genjumpl(retindex)
	fi
end

proc do_returnmult(unit p, a) =
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
	pc_gen(kjumpretm, pgenlabel(retindex))
	pc_setnargs(nparams)
	p.resultflag:=1
end

proc do_callproc(unit p, a, b) =
	[maxparams]unit paramlist
	[maxparams]i8 argattr
	int nparams, isptr, nvariadics, nret, isfn
	int iparams, fparams
	int nfixedparams
	symbol d, e
	ref[]i32 pmult
	unit q

	isptr:=0
!	isfn:=p.tag=jcallfn


	case a.tag
	when jname then
		d:=a.def

	when jptr then
		d:=ttnamedef[a.mode]
		isptr:=1
	else
		gerror("call/not ptr")
	esac

	isfn:=d.mode<>tvoid

	nparams:=0
	nvariadics:=0

	q:=b
	nfixedparams:=0
	e:=d.deflist
	while e, e:=e.nextdef do
		if e.nameid=paramid then ++nfixedparams fi
	od

	q:=b
	while q, q:=q.nextunit do
		if nparams>=maxparams then gerror("maxparams") fi
		paramlist[++nparams]:=q

		if d.varparams and nparams>=nfixedparams and nparams<=4 and nvariadics=0 then
			nvariadics:=nparams
		fi
	od

	pc_gen(ksetcall)
	pc_setmode_u(p)
	pccurr.nargs:=nparams

	iparams:=fparams:=0

	for i to nparams do
		q:=paramlist[i]
		argattr[i]:=0
		if q.mode in [tr32, tr64] then
			if ++fparams>8 then argattr[i]:=2 fi
		else
			if ++iparams>8 then argattr[i]:=2 fi
		fi
	od

	if fparams>8 and iparams>8 then
		gerror("Mixed stack args")
	fi
	iparams:=max(fparams, iparams)-8		!no. of stack args

	for i:=nparams downto 1 do
		if iparams.odd and argattr[i] then
			argattr[i]:=1					!change first/rightmost '2' to '1'
			iparams:=0
		fi
		q:=paramlist[i]
		evalunit(q)

		if nvariadics and i>=nvariadics and pccurr.mode=tr32 then
			pc_gen(kfwiden)
			pccurr.size:=8
			pccurr.mode:=tr64
			pccurr.mode2:=tr32

			pc_gen(ksetarg)
			pc_setmode(tr64)
		else
			pc_gen(ksetarg)
			pc_setmode_u(q)
		fi

		pccurr.x:=i
		pccurr.y:=argattr[i]
	od

	if not isptr then
		pc_gen((isfn|kcallf|kcallp), genmemaddr_d(d))
	else
		evalunit(a.a)
		pc_gen((isfn|kicallf|kicallp))
	fi

	pccurr.nargs:=nparams
    pccurr.nvariadics:=nvariadics

	if isfn then
		pc_setmode_u(p)
	fi

	if d.nretvalues>1 and isfn then
		nret:=d.nretvalues
		pmult:=ttmult[d.mode]

		for i to nret do
			pc_gen(ktype)
			pc_setmode(pmult[i])
		od
	fi

	if isfn and not p.resultflag then
		pc_gen(kunload)
		pc_setmode_u(p)
	fi

end

proc do_print(unit p, a, b) =
	unit q, r, fmt
	int m, fn, needprintend

	if a then
		needprintend:=1
		if ttbasetype[a.mode]<>tref then gerror("@dev no ref") fi
		case ttbasetype[tttarget[a.mode]]
		when tvoid then
			genpc_sysproc(sf_print_startfile, a)
		when tc8 then
			genpc_sysproc(sf_print_startstr, a)
		when tref then
			genpc_sysproc(sf_print_startptr, a)
		else
			gerror("@dev?")
		esac
	else
		needprintend:=1
		genpc_sysproc(sf_print_startcon)
	fi

	q:=b

	case p.tag
	when jfprint, jfprintln then
		if ttbasetype[q.mode]<>tref or ttbasetype[tttarget[q.mode]]<>tc8 then
			gerror("string expected")
		fi
		genpc_sysproc(sf_print_setfmt, q)
		q:=p.c
	esac

	while q do
		case q.tag
		when jfmtitem then
			fmt:=q.b
			r:=q.a
			m:=r.mode
		when jnogap then
			genpc_sysproc(sf_print_nogap)
			q:=q.nextunit
			nextloop
		when jspace then
			genpc_sysproc(sf_print_space)
			q:=q.nextunit
			nextloop
		else
			fmt:=nil
			r:=q
			m:=q.mode
		esac

		case ttbasetype[m]
		when ti64 then
			fn:=sf_print_i64
			if not fmt then fn:=sf_print_i64_nf fi
		when tu64 then
			fn:=sf_print_u64
		when tr32 then
			fn:=sf_print_r32
		when tr64 then
			fn:=sf_print_r64
		when tref then
			if tttarget[m]=tc8 or tttarget[m]=tarray and tttarget[tttarget[m]]=tc8 then
				fn:=sf_print_str
				if not fmt then fn:=sf_print_str_nf fi
			else
				fn:=sf_print_ptr
				if not fmt then fn:=sf_print_ptr_nf fi
			fi
		when tbool then
			fn:=sf_print_bool
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
			PRINTLN STRMODE(M), STRMODE(TTBASETYPE[M])
			gerror_s("PRINT/T=#", strmode(m))
		esac

		case fn
		when sf_print_i64_nf, sf_print_str_nf, sf_print_ptr_nf then
			genpc_sysproc(fn, r)
		else
			genpc_sysproc(fn, r, (fmt|fmt|pzero))
		esac

		q:=q.nextunit
	od

	case p.tag
	when jprintln, jfprintln then
		genpc_sysproc(sf_print_newline)
	esac
	if needprintend then
		genpc_sysproc(sf_print_end)
	fi
end

proc do_incr(unit p, a) =
	evallv(a)
	do_setinplace()
	pc_gen(p.pclop)
	pc_setmode_u(a)
	setincrstep(a.mode)
end

proc setincrstep(int m)=
	pccurr.stepx:=m

	if ttisref[m] then
		pccurr.stepx:=ttsize[tttarget[m]]
	fi
end

proc do_incrload(unit p, a) =
	evallv(a)
	do_setinplace()
	pc_gen(p.pclop)
	pc_setmode_u(a)
	setincrstep(a.mode)
end

proc do_for(unit p, pindex, pfrom, pbody, int down) =
!Structure:
!	Forup/to
!		pindex -> [ptoinit]
!		pfrom -> pto -> [pstep]
!		pbody -> [pelse]
!When pto is complex, then pto refers to an AV variable, and ptoinit contains code
!to evaluate the complex pto, and assign it to that AV

	unit pto, pstep, pelse, px, ptoinit
	int lab_b, lab_c, lab_d, lab_e
	int a, b, stepx

	pto:=pfrom.nextunit
	pstep:=pto.nextunit
	pelse:=pbody.nextunit
	ptoinit:=pindex.nextunit

	if pto.tag=jptr then
		px:=pto.a
		symbol d
		if px.tag=jname and (d:=px.def).nameid=paramid and
			 d.byref then
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
	evalunit(pfrom)
	pc_gen(kstore, genmem_u(pindex))
	pc_setmode_u(pindex)

	if ptoinit then			!using temp for limit
		ptoinit.resultflag:=0
		evalunit(ptoinit)
	fi

	if pfrom.tag=jconst and pto.tag=jconst then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			pc_gen(kjump, pgenlabel(lab_e))
		fi
	else
		if pfrom.tag=jconst then				!reverse condition; compare mem:imm
			evalunit(pto)
			evalunit(pfrom)
			pc_gencond(kjumpcc, (down|gt_cc|lt_cc), pgenlabel(lab_e))
		else
			evalunit(pindex)
			evalunit(pto)
			pc_gencond(kjumpcc, (down|lt_cc|gt_cc), pgenlabel(lab_e))
		fi
		pc_setmode_u(pindex)
	fi

	definefwdlabel(lab_b)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	if pstep then
		if pstep.tag<>jconst then
			gerror("for/step non-const not ready")
		fi
		stepx:=pstep.value
		if stepx<=0 then
			gerror("Bad for-step")
		fi
		pc_genx((down|kfordown|kforup), stepx, pgenlabel(lab_b))
		pc_setmode_u(pindex)
	else
		pc_genx((down|kfordown|kforup), 1, pgenlabel(lab_b))
		pc_setmode_u(pindex)
	fi

	pc_gen(kopnd, genmem_u(pindex))
	pc_setmode(ti64)

	case pto.tag
	when jconst then
		pc_gen(kopnd, pgenint(pto.value))
		pc_setmode(ti64)
	when jname then
		pc_gen(kopnd, genmem_u(pto))
		pc_setmode(ti64)
	esac

	if pelse then
		definefwdlabel(lab_e)
		evalblock(pelse)
	fi

	definefwdlabel(lab_d)
	--loopindex
end

proc do_forall(unit p, pindex, plist, pbody, int down) =
!Structure:
!	forall
!		pindex -> plocal -> pfrom -> pto
!		plist -> passign
!		pbody -> [pelse]

	unit plocal, pfrom, pto, pelse, passign
	int lab_b, lab_c, lab_d, lab_e
	int a, b
	symbol dto

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

	evalunit(pfrom)
	pc_gen(kstore, genmem_u(pindex))
	pc_setmode_u(pindex)

	if pto.tag not in [jconst, jname] then
		evalunit(pto)
		dto:=getavname(currproc)
		pc_gen(kstore, genmem_d(dto))
		pc_setmode(ti64)
		pto:=createname(dto)
		pto.mode:=dto.mode
		pto.resultflag:=1
	fi

	if pfrom.tag=jconst and pto.tag=jconst then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			pc_gen(kjump, pgenlabel(lab_e))
		fi
	else
		if pfrom.tag=jconst then				!reverse condition; compare mem:imm
			evalunit(pto)
			evalunit(pfrom)
			pc_gencond(kjumpcc, (down|gt_cc|lt_cc), pgenlabel(lab_e))
		else
			evalunit(pfrom)
			evalunit(pto)
			pc_gencond(kjumpcc, (down|lt_cc|gt_cc), pgenlabel(lab_e))
		fi
		pc_setmode_u(pindex)
	fi

	definefwdlabel(lab_b)

!need to generate assignment to local
	passign.resultflag:=0
	evalunit(passign)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	pc_genx((down|kfordown|kforup), 1, pgenlabel(lab_b))
	pc_setmode_u(pindex)

	pc_gen(kopnd, genmem_u(pindex))
	pc_setmode(ti64)
	case pto.tag
	when jconst then
		pc_gen(kopnd, pgenint(pto.value))
	when jname then
		pc_gen(kopnd, genmem_u(pto))
	else
		PC_GEN(KOPND, GENMEM_D(DTO))
!		gerror("forall/to: not const or name")
	esac
	pc_setmode(ti64)

	if pelse then
		definefwdlabel(lab_e)
		evalblock(pelse)
	fi

	definefwdlabel(lab_d)
	--loopindex
end

proc do_convert(unit p, a) =

	case p.convcode
	when kksoftconv then
		evalunit(a)
		return
	when kkerror then
		gerror("CONV/ERROR")

	else
		evalunit(a)
		pc_gen(convtopcl[p.convcode])
	esac
	pc_setmode_u(p)

	pc_setmode2(p.oldmode)
end

proc do_swap(unit p, a, b) =
	evallv(a)
	do_setinplace()
	evallv(b)
	do_setinplace()
	pc_gen(kiswap)
	pc_setmode_u(a)
end

global func checkdotchain(unit p, &pname)int=
!return accumulated offset of this and nested dot-expressions, 
!or -1 when offsets cannot be combined
	int offset

	case p.tag
	when jdot then
		offset:=checkdotchain(p.a, pname)
		return p.offset+offset

	else							!anything else, is the start expression
		pname:=p
		return 0
	esac
	return 0
end

proc do_dotref(unit pdot) =
	int imode:=createrefmode(nil, pdot.mode, 0)
	int offset
	unit a, pname


	a:=pdot.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a, pname)
		offset+:=pdot.offset
		a:=pname
	else
		offset:=pdot.offset
	fi

	evalref(a)

	if offset then
		genpushint(offset)
		pc_genix(kaddpx)
	fi
	pc_setmode(imode)
end

proc do_dot(unit pdot) =
	int offset
	unit a, pname

	a:=pdot.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a, pname)
		offset+:=pdot.offset
		a:=pname
	else
		offset:=pdot.offset
	fi

	evalref(a)
	pc_gen(kload, pgenint(offset))
	pc_setmode(ti64)

	pc_genix(kiloadx, 1)
	pc_setmode_u(pdot)
end

proc do_storedot(unit pdot, pfield, rhs) =
	int offset
	unit a, pname

	evalunit(rhs)
	if pdot.resultflag then
		pc_gen(kdouble)
	fi

	a:=pdot.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a, pname)
		offset+:=pdot.offset
		a:=pname
	else
		offset:=pdot.offset
	fi

	evalref(a)
	pc_gen(kload, pgenint(offset))
	pc_setmode(ti64)

	pc_genix(kistorex, 1)

	pc_setmode_u(pdot)
end

proc do_index(unit p, parray, pindex) =
	int addoffset, scale, offset


!GENCOMMENT("INDEX/ILOADNEXT/block")
!	if ttisblock[p.mode] then
!CPL "INDEX/BLOCK"
!		do_indexref(parray, pindex)
!
!		return
!	fi

	addoffset:=getindexoffset(parray, pindex)

	evalarray(parray)
	scale:=ttsize[tttarget[parray.mode]]
	offset:=-ttlower[parray.mode]*scale + addoffset*scale

	evalunit(pindex)
!GENCOMMENT("INDEX/ILOADNEXT")

	pc_genix(kiloadx, scale, offset)
	pc_setmode_u(p)
!GENCOMMENT("...INDEX/ILOADNEXT")
end

proc do_storeindex(unit p, parray, pindex, rhs) =
	int addoffset, scale
	addoffset:=getindexoffset(parray, pindex)

	evalunit(rhs)
	if p.resultflag then
		pc_gen(kdouble)
	fi

	evalarray(parray)
	evalunit(pindex)

	scale:=ttsize[tttarget[parray.mode]]
	pc_genix(kistorex, scale, -ttlower[parray.mode]*scale+addoffset*scale)
	pc_setmode_u(p.a)
end

proc do_indexref(unit parray, pindex) =
	int addoffset, scale
	addoffset:=getindexoffset(parray, pindex)

	evalarray(parray)
	evalunit(pindex)

	scale:=ttsize[tttarget[parray.mode]]
	pc_genix(kaddpx, scale, -ttlower[parray.mode]*scale+addoffset*scale)
	pc_setmode(tttarget[parray.mode])
end

func getindexoffset(unit parray, &pindex)int offset=
!convert index like [i+3] to [i], returning the +3 etc as a separate offset
	int addoffset:=0

	if pindex.tag=jbin and pindex.pclop in [kadd, ksub] then
		if pindex.b.tag=jconst then		!incorporate const offset into lwb adjustment
			addoffset:=(pindex.pclop=kadd|pindex.b.value|-pindex.b.value)
			pindex:=pindex.a
		fi
	fi
	return addoffset
end

proc do_switch(unit p, pindex, pwhenthen, pelse, int isref=0) =
!'looptype' is set up here:
! 0 = switch	normal switch (range-checked)
! 1 = doswitch	looping switch (range-checked)
! 2 = doswitchu	looping switch via computed goto/indexed (both non-range-checked)
! 3 = doswitchx	looping switch via computed goto/labels

	const maxlabels = 1000
	int minlab, maxlab, n, iscomplex, i
	int lab_a, lab_d, labjump, elselab, labstmt, ax, bx, ismult, mode
	byte looptype, opc
	[0..maxlabels]pcl labels
	unit w, wt, pjump, pnext
	pcl psetup, pc, px, pw
	symbol djump

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

	ismult:=p.mode<>tvoid and looptype=0

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

	if ismult then pc_gen(kstartmx) fi

	if looptype=3 then		!need to initialise pointer to JT
		pjump:=pindex.nextunit
		if pjump.tag<>jname then gerror("doswx not name") fi
		djump:=pjump.def
		if ttbasetype[djump.mode]<>tref then gerror("doswx not ref") fi

!these two instrs to be moved to kinitdswx later
!	PC_COMMENT("JT LOAD")
		pw:=pccurr							!last op before the two new ops
		pc_gen(kload, pgenlabel(labjump))
		pc_setmode(tu64)
		px:=pccurr							!px=pw.next = 1st of two ops

		pc_gen(kstore, genmem_u(pjump))
		pc_setmode(tu64)

		if pcldoswx=nil then
			gerror("doswx in main?")
		fi

!Before:  A B PCLDOSWX C D ...  W X Y				!X Y are load/store above
!After:   A B PCLDOSWX X Y D ...  W

		pc:=pcldoswx.next					!point to C
		pcldoswx.next:=px
		px.next.next:=pc
		pw.next:=nil
		pccurr:=pw

	fi

	evalunit(pindex)

	if looptype<>3 then
		pc_genxy(opc, minlab, maxlab, pgenlabel(labjump))
		pc_setmode(ti64)
		if looptype<2 then
			pc_gen(kopnd, pgenlabel(elselab))
			pc_setmode(ti64)						!dummy type (kopnd is general purpose)
		fi
	else
!PC_COMMENT("J1")
		pc_gen(kijump)
		pc_setmode(tu64)
	fi

	definefwdlabel(labjump)

	for i:=minlab to maxlab do			!fill with else labels first
		pc_gen(kswlabel, pgenlabel(elselab))
		labels[i]:=pccurr
	od
!*!	pc_gen(kendsw)


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
				labels[i].labelno:=labstmt
			od
			w:=w.nextunit
		od

		evalunitx(wt.b, isref)
		if ismult then pc_gen(kresetmx); pc_setmode_u(p) fi

		case looptype
		when 0 then
			genjumpl(lab_d)
		when 1 then
			genjumpl(lab_a)
		when 2 then
			evalunit(pindex)
			pc_genxy(opc, minlab, maxlab, pgenlabel(labjump))
			pc_setmode(ti64)
		else
			evalunit(pindex)
!PC_COMMENT("J2")
			pc_gen(kijump)
			pc_setmode(tu64)
		esac

		wt:=wt.nextunit
	od

	definefwdlabel(elselab)
	if pelse then
		evalunitx(pelse, isref)
		if ismult then pc_gen(kendmx); pc_setmode_u(p) fi
	fi

	if looptype then
		case looptype
		when 1 then
			genjumpl(lab_a)
		when 2 then
			evalunit(pindex)
			pc_genxy(opc, minlab, maxlab, pgenlabel(labjump))
			pc_setmode(ti64)
		else
			evalunit(pindex)
!PC_COMMENT("J3")
			pc_gen(kijump)
			pc_setmode(tu64)
		esac
		--loopindex
	fi

	definefwdlabel(lab_d)
end

proc do_select(unit p, a, b, c, int isref) =
	const maxlabels=256
	[maxlabels]pcl labels
	int labend, labjump, n, i, elselab, labstmt, ismult
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

	if ismult then pc_gen(kstartmx) fi
	evalunit(a)

	pc_genxy(kswitch, 1, n, pgenlabel(labjump))
	pc_setmode(ti64)
	pc_gen(kopnd, pgenlabel(elselab))
	pc_setmode(ti64)


	definefwdlabel(labjump)

	q:=b
	i:=0
	for i:=1 to n do
		pc_gen(kswlabel, pgenlabel(elselab))
		labels[i]:=pccurr
	od
!*!	pc_gen(kendsw)

	q:=b
	i:=0
	while q do
		labstmt:=definelabel()
		++i
		labels[i].labelno:=labstmt
		evalunitx(q, isref)
		if ismult then pc_gen(kresetmx); pc_setmode_u(p) fi
		genjumpl(labend)
		q:=q.nextunit
	od

	definefwdlabel(elselab)

	evalunitx(c, isref)
	if ismult then pc_gen(kendmx); pc_setmode_u(p) fi

	definefwdlabel(labend)
end

proc do_case(unit p, pindex, pwhenthen, pelse, int loopsw, isref) =
	const maxcase=500
	[maxcase]int labtable
	[maxcase]unit unittable
	int ncases, ismult, a, b

	int lab_abc, lab_d, labelse
	unit w, wt, plower, pupper

	loopsw:=p.tag=jdocase

	if pindex=nil then
		GERROR("EMPTY CASE NOT DONE")
	fi

	ismult:=p.mode<>tvoid and not loopsw

	if loopsw then
		lab_abc:=definelabel()		!start of loop
		lab_d:=createfwdlabel()	!end of case/end of loop
		stacklooplabels(lab_abc, lab_abc, lab_d)
	else
		lab_d:=createfwdlabel()	!end of case/end of loop
	fi

	if ismult then pc_gen(kstartmx) fi

	ncases:=0

	if casedepth>=maxcasedepth then
		gerror("case nested too deeply")
	fi
	casestmt[++casedepth]:=p

	if pwhenthen=nil then
		if ismult then gerror("case") fi
		goto skip
	fi

	evalunit(pindex)

!CPL "INCR CASED", CASEDEPTH

	wt:=pwhenthen

	while wt do
		w:=wt.a
		if ncases>=maxcase then
			gerror("too many cases")
		fi
		labtable[++ncases]:=createfwdlabel()
		unittable[ncases]:=wt.b

		while w do
!			if w.nextunit or wt.nextunit then pc_gen(kdouble) fi
!			if w.tag=jmakerange then
!				plower:=w.a
!				pupper:=w.b
!				unless plower.tag=pupper.tag=jconst then
!					gerror("case/var-range")
!				end
!
!
!
!GERROR("CASE/RANGE", W)
!			else
				evalunit(w)
!			fi
			pc_gencond(kjumpcc, eq_cc, pgenlabel(w.whenlabel:=labtable[ncases]))
			if w.nextunit or wt.nextunit then
				pccurr.popone:=1
			fi
			pc_setmode_u(w)
			w:=w.nextunit
		od

		wt:=wt.nextunit
	od

skip:
	labelse:=createfwdlabel()
	caseelse[casedepth]:=labelse
	genjumpl(labelse)

	for i:=1 to ncases do
		definefwdlabel(labtable[i])
		evalunitx(unittable[i], isref)
		if ismult then pc_gen(kresetmx); pc_setmode_u(p) fi

		if loopsw then
			genjumpl(lab_abc)
		else
			genjumpl(lab_d)
		fi
	od

	definefwdlabel(labelse)

	if pelse then
		evalunitx(pelse, isref)
		if ismult then pc_gen(kendmx); pc_setmode_u(p) fi
	fi

	if loopsw then
		genjumpl(lab_abc)
		definefwdlabel(lab_d)
		--loopindex
	else
		definefwdlabel(lab_d)
	fi

!IF CASEDEPTH=0 THEN
!GERROR("CASE - DEPTH 0?")
!FI

	--casedepth
!CPL "DECR CASED", CASEDEPTH
end

proc do_dotindex(unit p, a, b) =
	evalunit(a)
	evalunit(b)

	pc_gen(kloadbit)
	pc_setmode(ti64)
end

proc do_dotslice(unit p, a, b) =
	evalunit(a)
	evalunit(b.a)
	evalunit(b.b)

	pc_gen(kloadbf)
	pc_setmode(ti64)
end

proc do_read(unit p, a) =
	int m

	m:=p.mode

	if a=nil then
		a:=pzero
	fi

	if ttisinteger[m] then
		genpc_sysfn(sf_read_i64, a)
	elsif ttisreal[m] and ttsize[m]=8 then
		genpc_sysfn(sf_read_r64, a)
	elsif m=trefchar then
		genpc_sysfn(sf_read_str, a)
	else
CPL =STRMODE(M)
		GERROR("CAN'T READ THIS ITEM")
	fi
	pc_setmode_u(p)
end

proc do_readln(unit a) =
	if a then
		if ttbasetype[a.mode]<>tref then gerror("@dev no ref") fi

		case ttbasetype[tttarget[a.mode]]
		when tvoid then
			genpc_sysproc(sf_read_fileline, a)
		when tu8, tc8 then
			genpc_sysproc(sf_read_strline, a)
		else
			gerror("rd@dev?")
		esac
	else
		genpc_sysproc(sf_read_conline)
	fi
end

proc docond(int opc, unit p, int lab)=
	genjumpcond(opc, p, lab)
end

proc do_syscall(unit p, a)=

	setfunctab()

	case p.fnindex
	when sf_getnprocs then
		pc_gen(kload, pgenmem(pnprocs))

	when sf_getprocname then
		pc_gen(kload, pgenmemaddr(pprocname))
		pc_setmode(tu64)
		evalunit(a)
		pc_genix(kiloadx, 8, -8)

	when sf_getprocaddr then
		pc_gen(kload, pgenmemaddr(pprocaddr))
		pc_setmode(tu64)
		evalunit(a)
		pc_genix(kiloadx, 8, -8)

	else
		PC_COMMENT("SYSCALL/GENERIC")
	esac
	pc_setmode(ti64)
end

proc do_slice(unit p, a, b) =
!generate separate code for (ptr, length) parts

	if b=nil then
		evalarray(a)
		if a.tag=jconst then			!assume string
			genpushint(strlen(a.svalue))
		else
			genpushint(ttlength[a.mode])
		fi

	else
!worth checking for const bounds? Both must be const to make it worthwhile
		do_indexref(a, b.a)
		if b.a.tag=b.b.tag=jconst then
			genpushint(b.b.value-b.a.value+1)
		else
			evalunit(b.b)
			evalunit(b.a)
			pc_gen(ksub)
			pc_setmode(ti64)
			genpushint(1)
			pc_gen(kadd)
		fi
		pc_setmode(ti64)

	fi

	pc_gen(kstorem); pc_setmode(tslice)
end

proc do_assignblock(unit p, a, b) =
!fstore=1 when result is needed
!method used is:
! load ref to lhs
! load ref to rhs
! do block xfer, not using the stack

	if b.tag=jmakelist then
		if ttbasetype[a.mode]=tarray then
			do_assignarray(a, b)
		else
			do_assignrecord(a, b)
		fi
	else
		GERROR("ASSIGN BLOCK")
	fi
end

proc do_assignarray(unit a, b)=
	unit passign, pindex, pconst, q
	int index

	if ttbasetype[tttarget[a.mode]]=tc8 then
		gerror("Assignment not suitable for []char type")
	fi

	pconst:=createconstunit(1, ti64)
	pindex:=createunit2(jindex, a, pconst)
	passign:=createunit2(jassign, pindex, b.a)
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

proc do_assignrecord(unit a, b)=
	unit passign, pdot, pfield, q
	int m, fieldtype
	symbol d, e

	pfield:=createunit0(jname)
	pdot:=createunit2(jdot, a, pfield)
	passign:=createunit2(jassign, pdot, b.a)
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

proc do_assignms(unit a, b)=
	int nlhs, nrhs
	symbol d

	nlhs:=a.length

	case b.tag
	when jcall then
		evalunit(b)
		if b.a.tag<>jname then
			gerror("multassign from fn: not simple fn")
		fi
		d:=b.a.def
		nrhs:=d.nretvalues

		a:=a.a					!point to elements of makelist
	elsif ttbasetype[b.mode]=tslice then
GERROR("DECONSTR SLICE NOT READY")
	else
		gerror("(a, b):=x; var only")
	esac

	poptomult(a)

	if nrhs>nlhs then
		d:=getprocretmodes(b)

		for i:=nlhs+1 to nrhs do
			pc_gen(kunload)
			pc_setmode(ttmult[d.mode, i])
		od
	fi
end

proc do_assignmm(unit a, b)=
!(a, b, c):=(x, y, z)
	pushrhs(b.a)			!push rhs elements in right-to-left order
	pc_gen(kloadall)
	poptomult(a.a)
end

proc do_assignmdrem(unit a, b)=
!(a, b):=x divrem y
	evalunit(b)
	poptomult(a.a)
end

proc poptomult(unit a)=
!a is a linked list; assign n popped values to each element in turn 
	repeat
		case a.tag
		when jname then
			pc_gen(kstore, genmem_u(a))
		when jindex, jslice, jdot then
			evalref(a)
			pc_gen(kistore)

		when jptr then
			evalunit(a.a)
			pc_gen(kistore)

		when jif, jcase, jswitch, jselect then
			evalref(a)
			pc_gen(kistore)

		when jdotindex then
			evalref(a.a)
			evalunit(a.b)
			pc_gen(kstorebit)

		else
			cpl jtagnames[a.tag]
			gerror("Bad mult assign element")
		esac

		pc_setmode_u(a)

		a:=a.nextunit
	until a=nil
end

proc do_recase(unit p, a)=
	unit q, wt, w
	int destlab, casevalue

!CPL "DO_RECASE", CASEDEPTH

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

proc do_empty(unit p, a)=
	evallv(a)
	pc_gen(kclear)
	pc_setmode_u(a)
end

proc do_typeconst(unit p)=
	genpushint(p.value)
end

proc do_setinplace=
	if pccurr.opcode=kload and pccurr.opndtype=memaddr_opnd then
		pccurr.inplace:=1
	fi
end
=== mm_support.m 0 0 48/74 ===
global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

global func newsourcefile:ifile pf=
	pf:=pcm_allocz(filerec.bytes)
	if nsourcefiles>=maxsourcefile then loaderror("Too many sources") end
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
	end
	println
end

proc showerrorsource(int pos, symbol stproc=nil)=
	int fileno:=getfileno(pos), lineoffset
	ichar errorline,s

	fprintln "    Line:     #",getlineno(pos)
	if stproc and stproc.nameid=procid then
		fprintln "    Function: #()", stproc.name
	end
	fprintln "    Module:   # (#)", sources[fileno].name,sources[fileno].filespec
	showdivider('-')

	s:=errorline:=getsourceline(pos)
	lineoffset:=getsourcepos(pos)-errorline

	to 6 do print " " end
	while s^ not in [10,0] do
		print s++^
	end
	println
	s:=errorline
	to 6 do print " " end
	to lineoffset do
		if s^=9 then print '\t' else print ' ' end
		++s
	end
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
	fprint @str,mess,a
	serror_gen(str)
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
	end

	showdivider('*')
	case pass
	when 'N' then println "RX Name Error: "
	when 'T' then println "TX Type Error: "
	when 'G' then println "GX Code Gen Error: "
	when 'A' then println "AX Code Gen Error: "
	end case

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

!global proc gerroru(ichar mess,unit p)=
!	[256]char str
!	println @str, mess,,": Unsupported tag:", jtagnames[p.tag]
!	error_gen('G', str, p)
!end
!
!global proc gerrorop(ichar mess, unit p)=
!	[256]char str
!	println @str, mess,,": Unsupported op:", pclnames[p.pclop]
!	error_gen('G', str, p)
!end

global proc txerror(ichar mess,unit p=nil)=
	error_gen('T',mess,p)
end

global proc txerror_s(ichar mess,a,unit p=nil)=
	[256]char str
	fprint @str,mess,a
	error_gen('T',str,p)
end

global proc txerror_ss(ichar mess,a,b)=
	[256]char str
	fprint @str,mess,a,b
	error_gen('T',str)
end

global proc rxerror_s(ichar mess,a,unit p=nil)=
	[256]char str
	fprint @str,mess,a
	error_gen('N',str,p)
end

global proc gerror_s(ichar mess,s,^unitrec p=nil)=
	[256]char str

	fprint @str,mess,s
	error_gen('G',str,p)
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

global proc gs_additem(^strbuffer dest,ichar s)=
!like genstr, but ensure there is white space separation as needed from the last output
	ichar d
	int lastchar,nextchar

	d:=dest^.strptr

	if dest^.length then
		lastchar:=(d+dest^.length-1)^
		nextchar:=s^
		if isalphanum(lastchar) and isalphanum(nextchar) then
			strbuffer_add(dest," ")
		end
	end
	strbuffer_add(dest,s)
end

global proc gs_copytostr(^strbuffer source,^char s)=
	if source^.length then
		memcpy(s,source^.strptr,source^.length)
		(s+source^.length)^:=0
	else
		s^:=0
	end
end

global func isalphanum(int c)int=
	if c>='A' and c<='Z' or c>='a' and c<='z' or c>='0' and c<='9' then
		return 1
	end
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
		end case

!		if stdcat[i]=intcat and size<8 then
		if ttisinteger[i] and size<8 then
			ttisshort[i]:=1
		end

		ttlower[i]:=1

	end

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
	end

	if ext^ then
		strcpy(filespec,addext(filename,ext))
		file:=filespec
	end

	if loadedfromma then
		file:=pcm_copyheapstring(extractfile(file))
	fi	

	for i to nsourcefiles do
		if eqstring(file, sources[i].filename) and not sources[i].issyslib then
			return sources[i]
		end
	end

	if not isabspath(file) then
		strcpy(filespec2,path)
		strcat(filespec2,file)
		file:=filespec2
	end

	if fverbose=3 and fileno then
		println "Checkfile:",file
	end

!CPL =FILE
!CPL =FILENAME

	if file=nil or not checkfile(file) then
		loaderror("Can't find file: ",file)
	end

	pfile:=loadsourcefile(file)
	if fverbose=3 and pfile then
		println "Found:",file
	end

	pfile.issupport:=1
	return pfile
end

func isabspath(ichar filespec)int=
	ichar path:=extractpath(filespec)
	if path^ in ['\\','/'] or path^<>0 and (path+1)^=':' then	!absolute path
		return 1
	end
	return 0
end

global proc initbblib=
	for i:=1 to D_typestarterset.len do typestarterset[D_typestarterset[i]]:=1 end
end

global func getfileno(word pos)int fileno=
	fileno:=pos.[24..31]
!
!CPL =FILENO
!CPL =POS.[0..23]

	if fileno<1 or fileno>nsourcefiles then
!		RETURN 1
		abortprogram("No file no")
	end
	return fileno
end

global func getlineno(word pos)int=
	ichar source := getsourcestart(pos)
	ichar sline:=getsourceline(pos)
	ichar s:=sline
	int lineno:=1

	while s>=source do
		if s^=10 then ++lineno end
		--s
	end

	return lineno
end

func getsourceline(word pos)ichar=
	ichar source := getsourcestart(pos)
	ichar s :=  getsourcepos(pos)

	while s>source and s^<>10 do --s end
	if s^=10 then ++s end

	return s
end

func getsourcestart(word pos)ichar=
	return sources[getfileno(pos)].text
end

func getsourcepos(word pos)ichar=
	return sources[getfileno(pos)].text+pos.[0..23]
end

!global func mgetsourceinfo(int pos, ichar &filename, &sourceline)int=
!	int lineno
!
!	lineno:=getlineno(pos)
!	sourceline:=getsourcestart(pos)
!	filename:=sources[getfileno(pos)].filespec
!
!	lineno
!end
!

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
	end

	if nfiles=0 then loaderror("MA: no files") end

	f:=fopen(filename,"wb")
	if not f then loaderror("Can't create MA file ",filename) end

	if fverbose then
		println "Writing ",filename
	end
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
	end

	println @f,"=== END ==="

	for i to nfiles do
		pf:=sources[sflist[i]]
		println @f,i,pf.filename, pf. issyslib, pf.issupport
	end

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
		end
	end

	if passlevel=getproj_pass then
		f:=fopen(fs:=changeext(filename,"proj"),"wb")
		if f then
			println "Writing",fs
			for i to nmodules do
				pm:=modules[i]
				println @f,pm.name:"16jl", subprogs[pm.subprogno].name:"16jl",
					pm.file.filespec:"q",
					pm.issyslib
			end

			fclose(f)
		end
	end
end

proc getst(filehandle f, symbol d)=
	symbol q

	getstrec(f,d)

	q:=d.deflist

	while q, q:=q.nextdef do
		getst(f,q)
	end
end

proc getstrec(filehandle f, symbol d)=
	ichar name

	case d.nameid
	when procid, dllprocid, typeid, constid, staticid,
		 macroid, dllvarid then
	else
		return
	end case

	if d.owner and d.owner.nameid<>moduleid then
		return									!only module-level names
	end

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
	end case

	print @f,name:"10jl"

	print @f,getlineno(d.pos):"5",$

	case d.scope
	when module_scope then name:="Module"
	when global_scope then name:="Subprog"
	when program_scope then name:="Program"
	else name:="Export"				!assume export scope
	end case

	print @f, name,$

	if d.isimport then
		print @f,"Import "
	end

	print @f,strmode(d.mode):"10jlq",$
	print @f,sources[modules[d.moduleno].fileno].filespec:"q"
	println @f

end
=== mm_tables.m 0 0 49/74 ===
!include "mm_types.m"

!The first few entries must match those in pcl backend

global enumdata  [0:]ichar stdnames,
		[0:]byte stdsize,
		[0:]byte stdpcl =

!    type         name       bits     pcl
    (txvoid=0,    "void",       0,    tvoid),

    (txr64,       "r64",        8,    tr64),
    (txr32,       "r32",        4,    tr32),
    (txi64,       "i64",        8,    ti64),
    (txu64,       "u64",        8,    tu64),
    (txc64,       "c64",        8,    tu64),

    (txi8,        "i8",         1,    ti8),
    (txi16,       "i16",        2,    ti16),
    (txi32,       "i32",        4,    ti32),
    (txu8,        "u8",         1,    tu8),
    (txu16,       "u16",        2,    tu16),
    (txu32,       "u32",        4,    tu32),

    (txblock,     "block",      8,    tblock),

!    (tvoid=0,    "void",       0,    tvoid,	0,	0,	0,	0,	0),
!
!    (tr64,       "r64",         8,   tr64),
!    (tr32,       "r32",         4,   tr32),
!    (ti64,       "i64",         8,   ti64),
!    (tu64,       "u64",         8,   tu64),
!    (tc64,       "c64",         8,   tu64),
!
!    (ti8,        "i8",          1,   ti8),
!    (ti16,       "i16",         2,   ti16),
!    (ti32,       "i32",         4,   ti32),
!    (tu8,        "u8",          1,   tu8),
!    (tu16,       "u16",         2,   tu16),
!    (tu32,       "u32",         4,   tu32),
!
!    (tblock,     "block",       8,   tblock),
!    (tvector,    "vector",      8,   tvector),

!Above are also basic PCL types. Below are extras used by the front end

    (tc8,         "c8",          1,   tu8),
    (tbool8,      "b8",          1,   tu8),
    (tbool64,     "bool64",      8,   tu64),

    (tref,        "ref",         8,   tu64),
    (trecord,     "rec",         0,   tblock),
    (trange,      "range",      16,   tblock),

    (tarray,      "array",       0,   tblock),
    (tslice,      "slice",      16,   tblock),

    (trefchar,    "ichar",       8,   tu64),
    (trefbit,     "refbit",     16,   tu64),

    (tauto,       "auto",        0,   tu64),
    (tany,        "any",         0,   tu64),
    (tproc,       "proc",        0,   tu64),
    (tlabel,      "label",       0,   tu64),
    (tbitfield,   "bitfl",       8,   tu64),
    (ttuple,      "tuple",       0,   tu64),
    (tpending,    "pend",        0,   tu64),

    (tenum,       "enum",        0,   ti32),
    (tunion,      "union",       0,   tblock),

    (tlast,       "last ",       0,   tvoid),
end

global const tuser	= tlast

global const tint	= ti64
global const tword	= tu64
global const treal	= tr64
global const tbool	= tbool64

global const tfirstnum	= tr64
global const tlastnum	= tc64

global const maxtuplesize = 4

global int trefproc
global int treflabel
global int taddr = tu64			!for pcl use
!

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
	(sf_sign_i64,			$,	0,	1),
	(sf_sign_r64,			$,	0,	1),
	(sf_unimpl,				$,	0,	1),

end
!
global [sysfnnames.len]psymbol sysfnhandlers

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

!Key
!   A B C		Indicate single subnodes
!   A*			Etc, means a subnode can be a list
!   A+			Etc, means a list with one or two extra elements, so A2, A3

!                       N Expr  Solo
	(jnull=0,		$+1,	0,	3,	0), ! Place holder unit: means 'param no present' when used where a param is expected
	(jconst,		$+1,	0,	3,	0), ! int/real/string/range in .value/.xvalue/.svalue etc
	(jname,			$+1,	0,	3,	0), ! .def = ST entry
	(jblock,		$+1,	1,	0,	1), ! A*
	(jstrinclude,	$+1,	1,	3,	0), ! A; should be const string

!Logical Operators

	(jandl,			$+1,	2,	2,	0), ! A and B; A/B must be bools otherwise ISTRUE applied
	(jorl,			$+1,	2,	2,	0), ! A or B

	(jnotl,			$+1,	1,	1,	0), ! not A; A must be a bool (compiler generates jisfalsel if not)
	(jistruel,		$+1,	1,	1,	0), ! istrue A  zero/not-zero => 0/1
	(jisfalsel,		$+1,	1,	1,	0), ! isfalse A zero/not-zero => 1/0

!Expressions and Operators

	(jmakelist,		$+1,	2,	3,	0), ! (B: A*); B is lower bound; .length=# elements
	(jmakerange,	$+1,	2,	3,	0), ! A..B
	(jmakeset,		$+1,	1,	3,	0), ! [A*]; .length=# elements
	(jmakeslice,	$+1,	2,	3,	0), ! slice(A, B) A=ptr, B=length
	(jreturnmult,	$+1,	1,	0,	0), ! A*; uses .length; (set in TX from return/makelist

	(jkeyword,		$+1,	2,	3,	0), ! def=st entry
	(jdim,			$+1,	2,	3,	0), ! [A:] or [A:B] (set array dims by length)
	(jassign,		$+1,	2,	3,	1), ! A := B
	(jassignmm,		$+1,	2,	3,	1), ! (A*) := (B*); .length
	(jassignms,		$+1,	2,	3,	1), ! (A*) := B; .length
	(jassignmdrem,	$+1,	2,	3,	1), ! (A+) := B; B should be x divrem y 
	(jcall,			$+1,	2,	3,	1), ! A(B*)

	(jcmp,			$+1,	2,	2,	0), ! A cc B
	(jcmpchain,		$+1,	2,	1,	0), ! A* uses .cmpgenop/.cmpmode
	(jbin,			$+1,	2,	2,	0), ! A op B
	(junary,		$+1,	2,	1,	0), ! op A
	(jprop,			$+1,	2,	1,	0), ! prop A
	(jbinto,		$+1,	2,	2,	0), ! A op:= b
	(junaryto,		$+1,	1,	1,	0), ! op:= A
	(jincr,			$+1,	1,	3,	0), ! op A, A op

	(jinrange,		$+1,	2,	2,	0), ! A in B (B is range)
	(jinset,		$+1,	2,	2,	0), ! A in B (B is set)

	(jindex,		$+1,	2,	3,	0), ! A[B]
	(jslice,		$+1,	2,	3,	0), ! A[B..C]

	(jdot,			$+1,	2,	3,	0), ! A.B (B usu name); uses .offset in later stages
	(jdotindex,		$+1,	2,	3,	0), ! A.[B]
	(jdotslice,		$+1,	2,	3,	0), ! A.[B] (B must be jmakerange)

	(jptr,			$+1,	1,	3,	0), ! A^
	(jaddrof,		$+1,	2,	3,	0), ! &A

!NOTE conversion handling in the frontend needs to be reviewed, especially
!type punning

	(jconvert,		$+1,	1,	3,	0), ! Used internally; becomes specific mode in tx pass

	(jshorten,		$+1,	1,	3,	0), ! A; convert type of A to type of this node;
										! seems to be used in init data only; must be compile-time
	(jautocast,		$+1,	1,	3,	0), ! A (changed to jconvert by tx pass)
	(jtypepun,		$+1,	1,	3,	0), ! THIS NEEDS REVISING
	(jwiden,		$+1,	1,	3,	0), !
	(jfwiden,		$+1,	1,	3,	0), !
	(jfnarrow,		$+1,	1,	3,	0), !
	(jfix,			$+1,	1,	3,	0), !
	(jfloat,		$+1,	1,	3,	0), !
	(jtruncate,		$+1,	1,	3,	0), !
	(jtoboolt,		$+1,	1,	3,	0), !

	(jtypeconst,	$+1,	0,	3,	0), ! .value is the type code. The node itself has type i64
	(joperator,		$+1,	0,	3,	0), ! Uses .pclop

	(jbitwidth,		$+1,	1,	1,	0), ! A.bitwidth
	(jbytesize,		$+1,	1,	1,	0), ! A.bytes
	(jtypestr,		$+1,	0,	1,	0), ! A.typestr
	(jbitfield,		$+1,	1,	3,	0), ! A.odd etc (uses .bfcode)

	(jminvalue,		$+1,	1,	3,	0), ! A.min
	(jmaxvalue,		$+1,	1,	3,	0), ! A.max

	(jcompilervar,	$+1,	0,	3,	0), ! Uses .cvindex to denote compiler var)
	(jfmtitem,		$+1,	2,	3,	0), ! A:B within print items
	(jnogap,		$+1,	0,	3,	0), ! 
	(jspace,		$+1,	0,	3,	0), ! 

!Statements

	(jreturn,		$+1,	1,	0,	0), ! return [A]
	(jsyscall,		$+1,	1,	3,	1), ! FN(A*); .fnindex = sysfn no.

	(jto,			$+1,	3,	0,	0), ! to A do B end
	(jif,			$+1,	3,	3,	1), ! if A then B [else C] end
	(jforup,		$+1,	3,	0,	0), ! for A := B+     to B2 [by B3] do C+ [else C2] end
	(jfordown,		$+1,	3,	0,	0), ! for A := B+ downto B2 [by B3] do C+ [else C2] end
	(jforall,		$+1,	3,	0,	0), ! for[all] [i,]x in L do body [else e] end
									!    A1=i; A2=x; A3=L.lwb; A4=L.upb; B=L; B2={x:=L[I];C1=body; C2=E
	(jforallrev,	$+1,	3,	0,	0), ! Same but with inrev
	(jwhile,		$+1,	3,	0,	1), ! while A [,C] do B end
	(jrepeat,		$+1,	2,	0,	1), ! repeat A until B
	(jgoto,			$+1,	1,	0,	1), ! goto A
	(jlabeldef,		$+1,	0,	0,	0), ! A:
	(jexit,			$+1,	0,	0,	1), ! exit     .loopindex
	(jredo,			$+1,	0,	0,	1), ! redoloop .loopindex
	(jnext,			$+1,	0,	0,	1), ! nextloop .loopindex
	(jdo,			$+1,	1,	0,	1), ! do A end
	(jcase,			$+1,	3,	3,	1), ! case A <B* = whenthen chain> [else C] end
	(jdocase,		$+1,	3,	0,	1), ! Same as case
	(jwhenthen,		$+1,	2,	0,	0), ! when A* then B (part of internal case/switch)

	(jswitch,		$+1,	3,	3,	1), ! Same as case
	(jdoswitch,		$+1,	3,	0,	1), ! Same as case
	(jdoswitchu,	$+1,	3,	0,	1), ! Same as case
	(jdoswitchx,	$+1,	3,	0,	1), ! Same as case
	(jswap,			$+1,	2,	0,	1), ! swap(A, B)
	(jselect,		$+1,	3,	3,	1), ! (A | B* | C)
	(jrecase,		$+1,	1,	0,	0), ! recase A; must be const

	(jprint,		$+1,	2,	0,	1), ! print   [@A,] B*
	(jprintln,		$+1,	2,	0,	1), ! println [@A,] B*
	(jfprint,		$+1,	3,	0,	1), ! print   [@A,] B, C*   B is fmtstr
	(jfprintln,		$+1,	3,	0,	1), ! println [@A,] B, C*   B is fmtstr
	(jread,			$+1,	2,	0,	1), ! read A*
	(jreadln,		$+1,	2,	0,	1), ! readln [@A] (items are in separate jread node)
	(jstop,			$+1,	1,	0,	0), ! stop [A]
	(jeval,			$+1,	1,	3,	1), ! eval A
	(jclear,		$+1,	1,	1,	1), ! clear A
end

global const jfuncname = jname

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

global enumdata []ichar cvnames =
	(cv_lineno,		$),
	(cv_strlineno,	$),
	(cv_modulename,	$),
	(cv_filename,	$),
	(cv_func	,	$),
	(cv_date,		$),
	(cv_time,		$),
	(cv_version,	$),
	(cv_typename,	$),
	(cv_nil,		$),
	(cv_pi,			$),
	(cv_infinity,	$),
	(cv_true,		$),
	(cv_false,		$),
end

!!---
global enumdata []ichar symbolnames, []byte symbolgenops, []byte exprstarter,
 []byte symboladdmul =

!First half are basic tokens returned by lexreadtoken()
!                                genops       expr
	(dotsym,			".",		0,			0,	0),
	(commasym,			",",		0,			0,	0),
	(semisym,			";",		0,			0,	0),
	(colonsym,			":",		0,			0,	0),
	(sendtosym,			"=>",		0,			0,	0),
	(pipesym,			"->",		0,			0,	0),
	(lbracksym,			"(",		0,			1,	0),
	(rbracksym,			")",		0,			0,	0),
	(lsqsym,			"[",		0,			1,	0),
	(rsqsym,			"]",		0,			0,	0),
	(lcurlysym,			"{",		0,			0,	0),
	(rcurlysym,			"}",		0,			0,	0),
	(ptrsym,			"^",		0,			1,	0),
	(barsym,			"|",		0,			0,	0),
	(atsym,				"@",		0,			0,	0),
	(addrsym,			"&",		0,			1,	0),
	(ellipsissym,		"...",		0,			0,	0),

	(assignsym,			":=",		0,			0,	0),
	(rangesym,			"..",		0,			0,	0),
	(addsym,			"+",		kadd,		1,	'A'),
	(subsym,			"-",		ksub,		1,	'A'),
	(mulsym,			"*",		kmul,		0,	'M'),
	(divsym,			"/",		kdiv,		0,	'M'),
	(idivsym,			"%",		kidiv,		0,	'M'),
	(iremsym,			"rem",		kirem,		0,	'M'),
	(idivremsym,		"divrem",	kidivrem,	0,	'M'),
	(iandsym,			"iand",		kbitand,	0,	'A'),
	(iorsym,			"ior",		kbitor,		0,	'A'),
	(ixorsym,			"ixor",		kbitxor,	0,	'A'),
	(shlsym,			"<<",		kshl,		0,	'M'),
	(shrsym,			">>",		kshr,		0,	'M'),
	(minsym,			"in",		kmin,		1,	'A'),
	(maxsym,			"max",		kmax,		1,	'A'),
	(andlsym,			"and",		0,			0,	0),
	(orlsym,			"or",		0,			0,	0),
	(xorlsym,			"xor",		0,			0,	0),

	(eqsym,				"=",		0,			1,	0),
	(cmpsym,			"cmp",		0,			1,	0),
	(powersym,			"**",		kpower,		0,	0),
	(insym,				"in",		0,			0,	0),
	(notinsym,			"notin",	0,			0,	0),
	(inrevsym,			"inrev",	0,			0,	0),

	(notlsym,			"not",		knot,		1,	0),
	(istruelsym,		"istrue",	ktoboolt,	1,	0),
	(inotsym,			"inot",		kbitnot,	1,	0),
	(abssym,			"abs",		kabs,		1,	0),
	(signsym,			"sign",		ksign,		1,	0),
	(sqrtsym,			"sqrt",		ksqrt,		1,	0),
	(sqrsym,			"sqr",		ksqr,		1,	0),

	(propsym,			$,			0,			0,	0),
	(mathsopsym,		$,			0,			1,	0),		! sin etc
	(maths2opsym,		$,			0,			1,	0),		! atan2 etc

	(bitfieldsym,		$,			0,			0,	0),		! Special bit selections
	(eolsym,			$,			0,			0,	0),		! End of line
	(eofsym,			$,			0,			0,	0),		! Eof seen
	(rawxnamesym,		$,			0,			0,	0),		! unassigned name, case-sensitive, that is never a reserved word
	(incrsym,			$,			0,			1,	0),		! 1/2 = ++/--; later may add +2 for x++/x--
	(intconstsym,		$,			0,			1,	0),		! 123 32 bits signed
	(realconstsym,		$,			0,			1,	0),		! 123.4 64 bits
	(charconstsym,		$,			0,			1,	0),		! 'A' or 'ABCD'
	(stringconstsym,	$,			0,			1,	0),		! "ABC"

!Second half are tokens that can be yielded after a name lookup:
	(unitnamesym,		$,			0,			0,	0),		! 
	(namesym,			$,			0,			1,	0),		! identifier symbol
	(kincludesym,		$,			0,			0,	0),		! INCLUDE
	(kstrincludesym,	$,			0,			1,	0),		! SINCLUDE/BINCLUDE

	(stdtypesym,		$,			0,			1,	0),		! INT, CHAR etc
	(kicharsym,			$,			0,			1,	0),		! ICHAR IVOID
	(kifsym,			$,			0,			1,	0),		! 
	(kthensym,			$,			0,			0,	0),		! 
	(kelsifsym,			$,			0,			0,	0),		! 
	(kelsesym,			$,			0,			0,	0),		! 
	(kelsecasesym,		$,			0,			0,	0),		! 
	(kelseswitchsym,	$,			0,			0,	0),		! 
	(kendsym,			$,			0,			0,	0),		! 
	(kunlesssym,		$,			0,			0,	0),		! 
	(kcasesym,			$,			0,			1,	0),		! CASE
	(kdocasesym,		$,			0,			0,	0),		! DOCASE
	(krecasesym,		$,			0,			0,	0),		! RECASE
	(kwhensym,			$,			0,			0,	0),		! 
	(kforsym,			$,			0,			0,	0),		! FOR
	(ktosym,			$,			0,			0,	0),		! TO/DOWNTO
	(kbysym,			$,			0,			0,	0),		! 
	(kdosym,			$,			0,			0,	0),		! 
	(kwhilesym,			$,			0,			0,	0),		! 
	(krepeatsym,		$,			0,			0,	0),		! 
	(kuntilsym,			$,			0,			0,	0),		! 
	(kreturnsym,		$,			0,			0,	0),		! 
	(kstopsym,			$,			0,			0,	0),		! 
	(kloopsym,			$,			0,			0,	0),		! EXIT/NEXT/LOOP/REDO/RESTART
	(kgotosym,			$,			0,			0,	0),		! GO/GOTO
	(kswitchsym,		$,			0,			0,	0),		! SWITCH
	(kdoswitchsym,		$,			0,			0,	0),		! DOSWITCH
	(kprintsym,			$,			0,			0,	0),		! PRINT/PRINTLN/FPRINT/FPRINTLN
	(kreadsym,			$,			0,			0,	0),		! READ/READLN
	(kprocsym,			$,			0,			0,	0),		! PROC
	(kfuncsym,			$,			0,			0,	0),		! FUNCTION
	(klabelsym,			$,			0,			0,	0),		! LABEL
	(krecordsym,		$,			0,			0,	0),		! RECORD
	(kstructsym,		$,			0,			0,	0),		! STRUCT
	(kunionsym,			$,			0,			0,	0),		! UNION
	(kimportmodulesym,	$,			0,			0,	0),		! IMPORTDLL/IMPORTMODULE
	(kprojectsym,		$,			0,			0,	0),		! PROJECT
	(ktypesym,			$,			0,			0,	0),		! TYPE
	(krefsym,			$,			0,			1,	0),		! REF
	(kvoidsym,			$,			0,			1,	0),		! VOID
	(kvarsym,			$,			0,			0,	0),		! MUT
	(kletsym,			$,			0,			0,	0),		! LET
	(kslicesym,			$,			0,			0,	0),		! SLICE/SLICE2D
	(kmacrosym,			$,			0,			0,	0),		! MACRO
	(kconstsym,			$,			0,			0,	0),		! 
	(kclearsym,			$,			0,			0,	0),		! CLEAR
	(kheadersym,		$,			0,			0,	0),		! MODULE
	(kglobalsym,		$,			0,			0,	0),		! global
	(kstaticsym,		$,			0,			0,	0),		! STATIC

	(kcastsym,			$,			0,			1,	0),		! CAST
	(compilervarsym,	$,			0,			1,	0),		! $lineno etc
	(dollarsym,			$,			0,			1,	0),		! to be used for current array upperbound; also tabledata names
	(kevalsym,			$,			0,			0,	0),		! EVAL
	(ktabledatasym,		$,			0,			0,	0),		! tabledata
	(kclampsym,			$,			0,			1,	0),			! CLAMP
	(kswapsym,			$,			0,			0,	0),		! SWAP
	(ksyscallsym,		$,			0,			1,	0),		! $getprocname etc
end

global enumdata []ichar headerdirnames =
	(hdr_module,		$),
	(hdr_import,		$),
	(hdr_sourcepath,	$),
	(hdr_linkdll,		$),
end

global enumdata [0:]ichar scopenames=
	(Module_scope=0,	"Module"), 		!local to this module
	(global_scope,		"Global"),		!share with modules in SP
	(program_scope,		"Program"),		!(export/non-top SP) shared with other SPs
	(export_scope,		"Export"), 		!(export/top SP) exported from program/DLL

	(imported_scope,	"CImport"), 	!(for c front-end)
end

global enumdata =
	million_unit,
	billion_unit,
end

global enumdata [0:]ichar namenames=
	(nullid=0,		"null"),		!Not assigned
	(programid,		"program"),		!Main root
	(subprogid,		"subprog"),
	(moduleid,		"module"),		!Current or imported module
	(dllmoduleid,	"dllmodule"),	!
	(typeid,		"type"),		!Type name in type, proc or module
	(procid,		"proc"),		!Proc/method/func/op name
	(dllprocid,		"dllproc"),		!Dll Proc/func name
	(dllvarid,		"dllvar"),		!Dll variable name
	(constid,		"const"),		!Named constant in type, proc or module
	(staticid,		"static"),		!Static in type or proc or module
	(frameid,		"frame"),		!Local var
	(paramid,		"param"),		!Local param
	(fieldid,		"field"),		!Field of Record or Class
	(labelid,		"label"),		!Label name in proc only
	(macroid,		"macro"),		!Name of macro
	(macroparamid,	"macroparam"),	!Macro formal parameter name
	(linkid,		"link"),		!Name in class defined in a base class
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

	("cp",			kprintsym,		jprint),
	("cpl",			kprintsym,		jprintln),

	("read",		kreadsym,		jread),
	("readln",		kreadsym,		jreadln),
	("cast",		kcastsym,		jconvert),

	("function",	kfuncsym,	0),
	("func",		kfuncsym,	0),
	("proc",		kprocsym,		0),
	("fun",			kfuncsym,	1),

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

	("global",		kglobalsym,		global_scope),
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

	("char",		stdtypesym,		tc8),
	("c8",			stdtypesym,		tc8),
	("c64",			stdtypesym,		tc64),

	("bool64",		stdtypesym,		tbool64),
	("bool",		stdtypesym,		tbool64),
	("bool8",		stdtypesym,		tbool8),

	("label",		stdtypesym,		tlabel),

	("slice",		kslicesym,		tslice),

	("million",		unitnamesym,	million_unit),
	("billion",		unitnamesym,	billion_unit),

	("$lineno",		compilervarsym,	cv_lineno),
	("$strlineno",	compilervarsym,	cv_strlineno),
	("$filename",	compilervarsym,	cv_filename),
	("$modulename",	compilervarsym,	cv_modulename),
	("$function",	compilervarsym,	cv_func),
	("$date",		compilervarsym,	cv_date),
	("$time",		compilervarsym,	cv_time),
	("$version",	compilervarsym,	cv_version),
	("$typename",	compilervarsym,	cv_typename),
	("nil",			compilervarsym,	cv_nil),
	("pi",			compilervarsym,	cv_pi),
	("true",		compilervarsym,	cv_true),
	("false",		compilervarsym,	cv_false),
	("infinity",	compilervarsym,	cv_infinity),
	("$",			dollarsym,		0),

	("and",			andlsym,		0),
	("or",			orlsym,			0),
	("xor",			xorlsym,		0),
	("iand",		iandsym,		0),
	("ior",			iorsym,			0),
	("ixor",		ixorsym,		0),
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

	("sqr",			sqrsym,			0),
	("sqrt",		sqrtsym,		0),
	("sign",		signsym,		0),

	("sin",			mathsopsym,		maths_sin),
	("cos",			mathsopsym,		maths_cos),
	("tan",			mathsopsym,		maths_tan),
	("asin",		mathsopsym,		maths_asin),
	("acos",		mathsopsym,		maths_acos),
	("atan",		mathsopsym,		maths_atan),
	("log",			mathsopsym,		maths_log),
	("log10",		mathsopsym,		maths_log10),
	("exp",			mathsopsym,		maths_exp),
	("round",		mathsopsym,		maths_round),
	("floor",		mathsopsym,		maths_floor),
	("ceil",		mathsopsym,		maths_ceil),

	("atan2",		maths2opsym,	maths_atan2),
	("fmod",		maths2opsym,	maths_fmod),

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

global enumdata [0:]ichar convnames =
	(kkerror=0,     $),
	(kkfloat,       $),
	(kkfix,         $),
	(kktruncate,    $),
	(kkwiden,       $),
	(kkfwiden,      $),
	(kkfnarrow,     $),
	(kksoftconv,    $),
	(kktoboolt,     $),
	(kkharderr,     $),

	(kksofttrun,    $),
	(kkichar2sl,    $),
	(kkax2slice,    $),
	(kkcx2ichar,    $),
end

global []int D_typestarterset= (stdtypesym,lsqsym,krefsym,krecordsym,
		kicharsym, kslicesym, ptrsym)

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

global [jtagnames.bounds]byte isbooltag

global [symbolnames.bounds]byte binopset
global [symbolnames.bounds]byte keepeol

proc start=
	int genop, s,t, a, specop

	for i to exprendsymbols.len do
		endsexpr[exprendsymbols[i]]:=1
	end

	isbooltag[jcmp]:=1
	isbooltag[jcmpchain]:=1
	isbooltag[jandl]:=1
	isbooltag[jorl]:=1
	isbooltag[jnotl]:=1
	isbooltag[jistruel]:=1
	isbooltag[jisfalsel]:=1
	isbooltag[jinrange]:=1
	isbooltag[jinset]:=1

	for i in assignsym..notinsym do binopset[i]:=1 end

	for i in keepeol.bounds do
		if i in [commasym, lsqsym, lbracksym] or binopset[i] and 
				i not in [maxsym, minsym] then
			keepeol[i]:=1
		end
	end

end

=== mm_type.m 0 0 50/74 ===
!MACRO SETMODE(P, M) = P.MODE:=M

const nolv=0
const needlv=1

global const maxparams=100
const maxfields=200
int countedfields
int inidata

macro setmode(p, m) = p.mode:=m
macro setbool(p) = p.mode:=tbool

!proc tpass(unit p, int t=tany, lv=nolv, hard=0)=
proc tpass(unit p, int t=tany, lv=nolv)=
	symbol d
	unit a,b,c, q
	int oldmmpos,m,nparams,paramtype,restype,amode
	static int depth

	if p=nil then return end
	if depth=100 then
		txerror("TX looping detected")
	end
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

	when jconst then

	when jtypeconst then
		p.mode:=ti64

	when jbytesize, jbitwidth then
		tpass(a)
		p.mode:=ti64

	when jbin, jcmp then
		tx_bin(p,a,b)

	when jinrange then
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
!			setmode(p, createrefmode(nil,a.mode))
			p.mode:=createrefmode(nil,a.mode)
		end

!	when jaddroffirst then
!		tx_addroffirst(p,a,t)
!
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
			end
			fixchararray(c)
			b:=b.nextunit
		end
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
		if t=tany then txerror("cast() needs type") end
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
		end
		tpass(b,tvoid)
		tpass(c,tvoid)

	when jrepeat then
		tpass(a,tvoid)
		tpass(b)
		if iscondtrue(b) or iscondfalse(b) then txerror("repeat/const cond") end

	when jnogap, jspace then

	when jtypestr then
		tpass(a)
		if a.tag=jtypeconst then
			m:=a.value
		else
			tpass(a)
			m:=a.mode
		end
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
		end
		if ttisinteger[t] or ttisreal[t] then
			t:=gettypebase(t)
		end
		setmode(p, t)
	when jrecase then
		if a then
			tpass(a,ti64)
			if a.tag<>jconst then
				txerror("recase must be const")
			end
		end

	when jcompilervar then
		if p.cvindex in [cv_filename, cv_modulename] then
			setmode(p, trefchar)
		end

	when jbitfield then
		tx_bitfield(p,a,lv)

	when jsyscall then
		restype:=tvoid
		paramtype:=tvoid
		case p.fnindex
		when sf_getnprocs then restype:=ti64
		when sf_getprocname then paramtype:=ti64; restype:=trefchar
		when sf_getprocaddr then paramtype:=ti64; restype:=tref
		end case

		if paramtype<>tvoid then
			if a=nil then txerror("sys: arg missing") end
			tpass(a,paramtype)
			if a.nextunit then txerror("sys: too many args") end
		elsif a then txerror("sys: too many args")
		end

		setmode(p, restype)

	when jcmpchain then
		tx_cmpchain(p,a)

	when jclear then
		tpasslv(a)
		case ttbasetype[a.mode]
		when trecord, tarray then
!CPL "CLEAR BLOCK"
		else
			txerror("Clear scalar?")
		end case


	when jshorten then

	when jstrinclude then
		tx_strinclude(p,a)

	when jmakeslice then
		tx_makeslice(p,a,b,t)

	when jmakeset then
		tx_makeset(p,a,t)

	when joperator then
		p.mode:=ti64

	else
		CPL "TXUNIT: CAN'T DO:",jtagnames[p.tag]
	doelse:

		for i to jsubs[p.tag] do
			tx_unitlist(p.abc[i],t)
		end
	end switch

	tevaluate(p)

	case p.tag
	when jmakelist, jreturn then
	else
		if t<>tany and t<>tvoid and p.mode<>t then		!does not already match
!			coerceunit(p,t, hard)			!apply soft conversion
			coerceunit(p,t)			!apply soft conversion
		end
	end case

	IF T=TVOID THEN
		CASE P.TAG
		WHEN JCONST, JBIN, JUNARY, JCMP THEN
!			TXERROR("Eval needed")
		WHEN JNAME THEN
			unless ttisref[p.mode] and tttarget[p.mode]=tlabel then
!				TXERROR("Eval needed2")
			end

		end case
	end

!IF P.MODE AND NOT P.PMODE THEN
!CPL "PMODE NOT SET:"
!PRINTUNIT(P)
!FI


!CPL "TPASS/END", JTAGNAMES[P.TAG]

!P.PMODE:=GETPCLMODE(P.MODE)
	mmpos:=oldmmpos
	--depth
end

global proc tx_allprocs=
	symbol d
	unit pcode

	d:=stlinear
	while d, d:=d.nextlinear do
		if d.nameid=procid then
			currproc:=d
			pcode:=currproc.code

		    tpass(pcode,(currproc.nretvalues>1|ttuple|currproc.mode))

			case ttbasetype[currproc.mode]
			when tvoid then		!PROC
			when ttuple then	!MULT FN
			else				!REGULAR FN
				if pcode.tag<>jreturn then
	!			if NOT CTARGET AND pcode.tag<>jreturn then
					insertunit(pcode,jreturn)
					setmode(pcode, currproc.mode)
					pcode.resultflag:=1
				end
			end case
		end
	end

end

proc tx_block(unit p,a, int t,lv)=
	while a and a.nextunit do
		tpass(a,tvoid)
		a:=a.nextunit
	end
	if a then
		tpass(a,t,lv)
		setmode(p, (t<>tvoid|a.mode|tvoid))
	end
end

global proc tx_typetable=
	symbol d

	for i:=tuser to ntypes do
		if ttbasetype[i]=trecord then
			tx_passdef(d:=ttnamedef[i])
		end
		setmodesize(i)
	end
end

proc setmodesize(int m)=
	int size,target

	if ttsize[m] then return end


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
		end
		println "SIZE 0:",strmode(m),=m,=stdnames[ttbasetype[m]]
		println "Can't set mode size"
	end case
end

proc setarraysize(int m)=
	int lower,length,elemsize,target,size
	unit pdim,a,b

	if ttsizeset[m] then return end

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
		when jdim then
			tpass(a)
			lower:=getconstint(a)
			if b then
				tpass(b)
				length:=getconstint(b)
			else
				length:=0
			end
		else
			tpass(pdim)
			length:=getconstint(pdim)
			lower:=1
		end case
	else
		lower:=1
		length:=0
	end

	if length<0 then txerror("Neg length") end
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
end

proc setslicesize(int m)=
	unit pdim

	if ttsize[m] then return end

	pdim:=ttdimexpr[m]

	if pdim then
		rx_unit(ttowner[m],pdim)
		tpass(pdim)
		ttlower[m]:=getconstint(pdim)
		ttdimexpr[m]:=nil
	else
		ttlower[m]:=1
	end

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
	end

	oldmmpos:=mmpos
	mmpos:=p.pos

	d:=p.deflist
	while d do
		tx_passdef(d)
		d:=d.nextdef
	end

	q:=p.code

	case p.nameid
	when procid then
		currproc:=nil
		currproc:=nil
	when constid then
		tx_namedconst(p)
	when staticid, frameid, paramid then
		tx_namedef(p)
	end case

	p.txdone:=1
	mmpos:=oldmmpos
end

proc tx_unitlist(unit p, int t=tany, lv=nolv)=
	while p do
		tpass(p,t)
		p:=p.nextunit
	end
end

proc tx_namedef(symbol d)=
	int m,mold,inidataold, LENGTH
	unit dcode,pequiv

UNIT OLDDCODE

	if d.circflag then
		txerror("Circular reference detected")
	end
	if d.txdone then return end

	m:=d.mode
	setmodesize(m)

	dcode:=d.code

	d.circflag:=1

	if d.equivvar then
		pequiv:=d.equivvar
		if pequiv.tag=jaddrof then deleteunit(pequiv,pequiv.a) end
		if pequiv.tag<>jname then
			txerror("@name needed")
		end
		tpass(pequiv)
	end



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
		end
		d.circflag:=0
		d.txdone:=1
		if ttbasetype[m]=tarray and ttlength[m]=0 then
			d.mode:=dcode.mode
		end

		if mold<>m then
			if ttisinteger[m] and ttisshort[mold] then
				insertunit(d.code,jshorten)
!				d.code.mode:=mold
				setmode(d.code, mold)
			elsif mold=tr32 then
!				d.code.mode:=mold
				setmode(d.code, mold)
			end
		end

		if d.nameid=staticid then
			checkconstexpr(d.code)
		end

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
		end

	else
		d.circflag:=0
		d.txdone:=1
	end
end

global proc tx_namedconst(symbol d)=
	int m

	if d.circflag then
		txerror("Circular const reference detected")
	end

	unit q
	if d.txdone then return end
	q:=d.code

	m:=d.mode

	d.circflag:=1
	tpass(q,(m=tauto|tany|m))

	d.circflag:=0
	checkconstexpr(q)
	if m=tauto then
		d.mode:=q.mode
	end

	case ttbasetype[d.mode]
	when tref then
		if d.mode<>trefchar then
			txerror("Bad const type")
		end
	end case

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
		end

	when jconvert then
		if ttbasetype[p.a.mode]=tref then
			setmode(p.a, p.mode)
			deleteunit(p,p.a)
		else
			goto error
		end

	when jshorten then
		checkconstexpr(p.a)

!	when jaddrof, jaddroffirst then
	when jaddrof then
		case p.a.tag
		when jname then
		else
			goto error
		end case

	when jname then
		if p.def.nameid=fieldid then return end
		if p.def.nameid=procid then return end
		if p.def.nameid=labelid then return end
		error
	else
	error:
		println =jtagnames[p.tag],STRMODE(P.MODE)
		PRINTUNIT(P)
		txerror("Getconstexpr: not const")
	end case
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
	end
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
		setmode(p, t)
	end
end

proc tx_name(unit p,int t,lv)=
	symbol d
	int oldmmpos
	unit pcode
	oldmmpos:=mmpos

	return when p.txcount
	++p.txcount

	d:=p.def
	mmpos:=d.pos

!CPL "TXNAME", D.NAME

	case d.nameid
	when constid then			!note: currently, rxpass converts names to constants

		if lv then txerror("&const") end

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
		end

		p.slength:=pcode.slength

!		setmode(p, d.mode)
		p.mode:=d.mode

		p.isconst:=1
		p.isastring:=pcode.isastring

	when staticid,frameid,paramid then
		if d.islet and lv then
!			println D.NAME,=LV,D.ISLET
			txerror_s("Can't use 'let' as lvalue: ",d.name)
		end

		tx_namedef(d)

		setmode(p, d.mode)
		if d.byref then
			if not p.insptr then
				++p.insptr
				insertunit(p, jptr)
				setmode(p, tttarget[d.mode])
			end
		end
		twiden(p,lv)

	when procid,dllprocid then

		setmode(p, trefproc)	!use generic refproc mode (yields return type of actual proc mode
				!after a call op, or actual refproc in other context. Don't use actual
				!refproc here, to avoid generating thousands of ^proc modes, one
				!for each call, that will never be needed

	when labelid then
		if t=tvoid then			!assume standalone label; treat as goto
			insertunit(p, jgoto)
		else
			setmode(p, treflabel)
		end

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
		end
		setmode(p, d.mode)

	else
		mmpos:=p.pos
		txerror_ss("TNAME? # #",namenames[d.nameid],d.name)
	end case
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

	case p.pclop
	when kadd then
		if dobinnumx(p,a,b) then return end
		if ttisref[amode] then
			if ttisref[bmode] and a.isastring and b.isastring then
				combinestrings(p)
				return
			end
			if isnum(bmode) then
				coerceunit(b,ti64)
				p.pclop:=kaddpx
				setmode(p, amode)
				return
			end
		end

	when ksub then
		if dobinnumx(p,a,b) then return end
		if ttisref[amode] then
			if ttisref[bmode] then
				if comparemodes(amode, bmode) then
!					p.pclop:=ksubp
					p.mode:=ti64
					return
				else
					txerror("ref-ref: not compat")
				end
			end
			if isnum(bmode) then
				coerceunit(b,ti64)
				p.pclop:=ksubpx
				setmode(p, amode)
				return
			end

		end

	when kmul then
		if dobinnumx(p,a,b) then return end
		if ttisref[amode] then
			if a.isastring and ttisinteger[b.mode] and b.tag=jconst then
				mulstrings(p)
				return
			end
		end


	when kdiv then
		if isnumi(amode) and isnumi(bmode) then p.pclop:=kidiv; goto doidiv end
		if dobinnumf(p,a,b) then return end
		if isnum(amode) and isnum(bmode) then
			p.mode:=tr64
			coerceunit(a,tr64)
			coerceunit(b,tr64)
			return
		end

	when kidiv, kirem, kidivrem, kbitand, kbitor, kbitxor then
doidiv:
		if dobinnumi(p,a,b) then return end

	when kmin, kmax then
		if dobinnumx(p,a,b) then return end

	when kpower then
		if dobinnumx(p,a,b) then return end

!	when kfmod, katan2 then
	when kmaths2 then
		coerceunit(a,tr64)
		coerceunit(b,tr64)
		p.mode:=tr64
		return

	when kshl, kshr then
		if isnumi(amode) then
			coerceunit(b,ti64)
			setmode(p, amode)
			return
		end

	elsif p.condcode then
		if dobinnumx(p,a,b) then
			setbool(p)
			return
		end
		setbool(p)
		if ttisref[amode] and ttisref[bmode] then
			if not comparemodes(amode, bmode) then
				txerror("Cmp ref/^not compat")
			end
			return
		end
		if p.condcode in [eq_cc, ne_cc] then
			if comparemodes(amode, bmode) then
				return
			end
		end

	else
		txerror("txbin?")
	end case

cpl pclnames[p.pclop]
	CPL "BIN/CAN'T RESOLVE MODES",strmode(amode),strmode2(bmode)
!	TXERROR_SS("BIN/CAN'T RESOLVE MODES",strmode(amode),strmode2(bmode))
end

proc tx_binto(unit p,a,b)=
	int abase, bbase, amode,bmode, opc

	tpasslv(a)
	tpass(b)

	amode:=a.mode
	bmode:=b.mode

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if p.pclop=kdiv and ttisinteger[abase] then
		p.pclop:=kidiv
	end

	p.mode:=tvoid

	case p.pclop
	when kadd then				!ref+^not allowed; or ref+int (later refchar+refchar)
		if abase=tref and bbase=tref then
			txerror("to:ref+ref")
		end
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=kaddpx
			return
		end
	when ksub then				!ref-int
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=ksubpx
			return
		end
	when kshl, kshr, kbitand, kbitor, kbitxor then
		coerceunit(b,ti64)
		return
	end case

	if isnum(abase) and isnum(bbase) then	!num op num
		coerceunit(b,abase)

	elsif ttisshort[abase] and isnum(bbase) then
		coerceunit(b,abase)

	else
		if not comparemodes(amode,bmode) then
			txerror_ss("BIN: modes not compatible: # #",strmode(amode),strmode(bmode))
		end
	end
end

func getdominantmode(int amode,bmode)int=
	int abase,bbase

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if isnum(abase) and isnum(bbase) then
		return min(abase,bbase)
	end
	if not comparemodes(amode, bmode) then
		txerror("Getdom: no dominant mode")
	end
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
		end

		q:=q.nextunit
	end

	q:=a
	r:=a.nextunit
	while q do
		coerceunit(q,u)
		q:=q.nextunit
	end

	for i:=1 to p.cmpgenop.len do
		genop:=p.cmpgenop[i]
		if genop=0 then exit end
	end

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
					if nparams>=maxparams then txerror("Param overflow") end
					paramlist[++nparams]:=e
				end
				e:=e.nextdef
			end

		else					!assume fn ptr
			while ttbasetype[a.mode]=tref do
				insertunit(a,jptr)
				setmode(a, tttarget[a.mode])
			end
			goto dorefproc
		end

	when jif,jselect,jblock then
		TXERROR("Can't do ifx/func")

	else
	dorefproc:
		if a.tag=jdot then
			tmethodcall(p,a,pargs)
			a:=p.a
			pargs:=p.b
			goto retry
		end

		if ttbasetype[a.mode]<>tproc then
			txerror("Function pointer expected")
		end

		d:=ttnamedef[a.mode]

		if d=nil then txerror("Function expected") end
		goto getparams
	end case

	q:=pargs
	while q do
		if nargs>=maxparams then txerror("Param overflow") end
		arglist[++nargs]:=q
		q:=q.nextunit
	end

	setmode(p, d.mode)				!type returned by func (will be void for procs)

	if p.mode and t<>tvoid then
		twiden(p,nolv)
	end

	if d.varparams then
		for i to nargs do

			if i<=nparams then
				tpass(arglist[i],paramlist[i].mode)
			else
				tpass(arglist[i])
			end
		end
		return

	end

!I have formal params in paramlist, and actual args in arglist
!Now create new set of args in arglist, which maps any keyword parameters,
!while missing args (represented as nullunit) replaced with nil

!Create new set of actual parameters in params(), with optional/default values filled in
!and any conversions applied
	k:=0
	kwdused:=0
	for i to nparams do
		newarglist[i]:=nil
	end

	for i to nargs do
		q:=arglist[i]
		case q.tag
		when jkeyword then
			name:=q.a.def.name
			for j to nparams do
				if eqstring(paramlist[j].name,name) then
					exit
				end
			else
				txerror_s("Can't find kwd param: #",name)
			end

			if newarglist[j] then
				txerror_s("Kwd: # already used or was implicit",name)
			end
			newarglist[j]:=q.b
			kwdused:=1

		else
!doregparam:
			if kwdused then
				txerror("Normal param follows kwd")
			end
			if k>=nparams then
				cpl =k, =nparams
				txerror("Too many params supplied")
			end
			newarglist[++k]:=(q.tag=jnull|nil|q)
		end case
	end

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
			end
		end
	end

!final pass: do type-pass on each param, and apply any conversion
!I also need to build a new argument list for the call unit
	unit ulist:=nil, ulistx:=nil

	for i to nparams do
		pm:=paramlist[i]
		q:=newarglist[i]

		if pm.byref then
			tpass(q,m:=tttarget[pm.mode],needlv)
			qm:=q.mode

			if not comparemodes(qm,m) then
				txerror_ss("&param: type mismatch",strmode(qm), strmode(m))
			end

!			UNLESS CTARGET AND Q.TAG=JCONVERT THEN

				insertunit(q,jaddrof)
				setmode(q, pm.mode)
!			ELSE
!				Q.TAG:=JADDROF
!				Q.MODE:=PM.MODE
!			END

		else
			tpass(q,pm.mode)
		end

		addlistunit(ulist, ulistx, q)
		q.nextunit:=nil
	end
	p.b:=ulist
end

proc tx_unary(unit p,a)=
	int opc,size,amode,mbase,tmax,x,xhigh, resmode

	tpass(a)

	amode:=a.mode
	resmode:=amode

	case p.pclop
!	when katan, klog, klog10, kexp, ksqrt,ksin,kcos,ktan, kasin, kacos,
	when kmaths, ksqrt then
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
!		resmode:=ti64

	ELSE
		txerror_s("TXUNARY:",pclnames[p.pclop])
	end case

	setmode(p, resmode)
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
		end

		if p.propcode=kkminval then
			case mbase
			when ti8 then x:=-128
			when ti16 then x:=-32768
			when ti32 then x:=-2_147_483_648
			when ti64 then x:=i64.min
			when tu8,tu16,tu32,tu64,tc8,tc64 then x:=0
			else
 	           txerror_s("Can't do minvalue on #",strmode(mbase))
			end case
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
			end case
		end
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
		end

		setmode(p, trefchar)
		p.svalue:=pcm_copyheapstring(strmode(amode))
		p.isastring:=1
		p.length:=strlen(p.svalue)
		return

	ELSE
		txerror_s("Prop:", pclnames[p.propcode])
	end case

	setmode(p, resmode)
end

proc tx_unaryto(unit p,a)=
	tpasslv(a)

	case p.pclop
	when kbitnot, knot, ktoboolt then
		txerror("Not int") when not isint(a.mode)
	end case

	p.mode:=tvoid
end

proc tx_if(unit p,pcond,plist,pelse, int t,lv) =
	unit pc:=pcond, pl:=plist
	int u

	u:=tvoid
	if t<>tany then u:=t end

	while pc, (pc:=pc.nextunit; pl:=pl.nextunit) do
		tpass(pc)
		tpass(pl,t,lv)

		if t=tany then
			if u=tvoid then
				u:=pl.mode
			elsif lv then
				if not comparemodes(u, pl.mode) then
					txerror("IF/LV?")
				end
			else
				u:=getdominantmode(u,pl.mode)
			end
		end
	end

	if t<>tvoid and pelse=nil then
		txerror("else needed")
	end
	tpass(pelse,t,lv)
	if t=tany then
		if lv then
			if not comparemodes(u, pelse.mode) then
				txerror("IF/LV2?")
			else
				u:=getdominantmode(u,pelse.mode)
			end
		end
	end

	if t<>tvoid then
		pl:=plist
		while pl, pl:=pl.nextunit do
			if t=tany then
				coerceunit(pl,u)
			end
		end
		if t=tany then
			coerceunit(pelse,u)
		end
		setmode(p, u)
	end

	if pcond.nextunit=plist.nextunit=nil then
		if iscondtrue(pcond) then		!branch b only
			deleteunit(p,plist)
		elsif iscondfalse(pcond) then	!branch c only
			if pelse=nil then
				pelse:=createunit0(jblock)
			end
			deleteunit(p,pelse)
		end
	end
end

proc tx_incrto(unit p,a,int t)=
	tpasslv(a)

	if t<>tvoid then
		case p.pclop
		when kincrto then p.pclop:=kincrload
		when kdecrto then p.pclop:=kdecrload
		end case
		setmode(p, a.mode)
	else				!a++ a-- to ++a --a
		case p.pclop
		when kloadincr then p.pclop:=kincrto
		when kloaddecr then p.pclop:=kdecrto
		end case
		p.mode:=tvoid
	end

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
	end
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
	end case

	tpass(plocal)
	if plocal.mode=tany then
		setmode(plocal, elemtype)
		plocal.def.mode:=elemtype
	end

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
	end
	setmode(p, tttarget[amode])
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
	setmode(p, trange)
end

proc tx_ptr(unit p,a,int t,lv)=
	symbol d

	tpass(a)

	case ttbasetype[a.mode]
	when tvoid then
		txerror("De^Void")
	when tref then
		setmode(p, tttarget[a.mode])

	when tslice then
		CPL "DEREF SLICE"
	else
		txerror("PTR: need ^T")
	end case

	twiden(p,lv)
end

proc setrecordsize(int m)=
	[maxfields+8]symbol fieldlist
	int i,nfields,indent,nrfields,size,index, maxalign
	symbol d,e
	^char flags
	const ss='S', ee='E'
	int flag
	static int depth


	if ttsize[m] then return end
	if ++depth>10 then serror("Recursive record?") end

	d:=ttnamedef[m]
	e:=d.deflist
	nfields:=0

	fieldlist[++nfields]:=symbol(ss)

	while e do
		if e.nameid=fieldid then
			if nfields>=maxfields then
				gerror("srs:too many fields")
			end

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
				end case
			end
		end

		e:=e.nextdef
	end

	fieldlist[++nfields]:=symbol(ee)
	fieldlist[nfields+1]:=nil			!terminator

	countedfields:=0
	index:=2
	maxalign:=1
	scanrecord('S',&fieldlist,index,size,0, d.caligned, maxalign)

	if d.caligned then
		size:=roundoffset(size,maxalign)
		d.maxalign:=maxalign
	else
		d.maxalign:=1
		if size iand 7 = 0 then
			d.maxalign:=8
		elsif size iand 3 = 0 then
			d.maxalign:=4
		elsif size iand 1 = 0 then
			d.maxalign:=2
		end
	end

	ttsize[m]:=size
	ttlength[m]:=countedfields
	ttlower[m]:=1

	--depth
end

proc scanrecord(int state,ref[]symbol fields, int &index, &isize, offset, calign, &maxalign)=
 	symbol e,f,ea
	int size:=0,fieldsize,bitoffset:=0, alignment, newoffset

	while f:=fields^[index++] do
		case int(f)
		when 'S','U' then
			scanrecord(int(f),fields, index,fieldsize, offset, calign, maxalign)
		when 'E' then			!end of this nested block
			if state='U' then ++countedfields end
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
				end

			elsif f.atfield then
				bitoffset:=0
				e:=f.equivfield
				fieldsize:=0
				f.offset:=e.offset+f.equivoffset
			else
				bitoffset:=0
				if state='S' then ++countedfields end
				fieldsize:=ttsize[f.mode]
				if calign then
					alignment:=getalignment(f.mode)
!CPL "CALIGN", =FIELDSIZE, =ALIGNMENT, =MAXALIGN, =STRMODE(F.MODE), =TTSIZE[F.MODE]
					if alignment>maxalign then maxalign:=alignment end
					newoffset:=roundoffset(offset,alignment)
					size+:=newoffset-offset
				else
					newoffset:=offset
				end
				f.offset:=newoffset
				offset:=newoffset
			end
		end case
		if state='S' then
			offset+:=fieldsize
			size+:=fieldsize
		else
			size:=max(size,fieldsize)
		end
	end
end

func roundoffset(int offset, alignment)int=
	int mask

	if alignment=1 then return offset end
	mask:=alignment-1
	while offset iand mask do ++offset end

	return offset
end

proc tx_convert(unit p,a,int hard=0)=
	case a.tag
	when jmakelist then
		tx_makelist(a,a.a,p.oldmode,nolv)
	else
		tpass(a)
		coerceunit(a,p.oldmode,hard)
!!NEW:
!		tpass(a, p.oldmode, hard:hard)
!!		coerceunit(a,p.oldmode,hard)
	end case
	deleteunit(p,a)			!get rid of this convert (may be replaced by new convert unit)
end

proc tx_makelist(unit p,a, int t,lv)=
	int alength,tlength,elemtype,newt, i, nfields,isconst, m
	unit q,b
	symbol e

!CPL "MAKELIST"

	alength:=p.length
	newt:=0
	isconst:=1

	tlength:=ttlength[t]

	if tlength then
		if alength<tlength then
			txerror_ss("Too few elements",strint(alength), strint(tlength))
		elsif alength>tlength then
			txerror_ss("Too many elements",strint(alength), strint(tlength))
		end
	end

	case ttbasetype[t]
	when tarray then
		elemtype:=tttarget[t]
		if tlength=0 then
			newt:=createarraymodek(nil, elemtype, ttlower[t],alength,0)
		else
			newt:=t
		end
		q:=a
		while q do
			tpass(q,elemtype,lv)

			unless q.tag=jconst then isconst:=0 end
			q:=q.nextunit
		end

		setmode(p, newt)

	when trecord then
		e:=ttnamedef[t].deflist
		q:=a
		while q and e do
			if e.nameid=fieldid then 
				while e.mode=tbitfield do
					e:=e.nextdef
					if not e then exit end
				end

				tpass(q,e.mode,lv)

				unless q.tag=jconst then isconst:=0 end
				q:=q.nextunit
			end

			e:=e.nextdef
		end
		while e and (e.nameid<>fieldid or e.mode=tbitfield) do
			e:=e.nextdef
		end
		if q or e then
			txerror("Can't initialise unions")
		end
		setmode(p, t)
		p.resultflag:=1

	when tslice then
CPL "TSLICE"

	else
		txerror_s("Unknown makelist type: #",strmode(t))
	end case

	p.isconst:=isconst

	tpass(p.b,ti64)				!lower


IF P.TAG<>JMAKESLICE THEN

	if not inidata and isconst then
		e:=getavname(currproc,staticid)
		e.mode:=t
		addlinear(e)
		q:=createunit0(jnull)
		q^:=p^
		e.code:=q
		p.tag:=jname
		p.def:=e
	end
FI
end

proc tx_makeslicefromlist(unit p,a, int t)=
	CPL "MAKESLICE/TX"

	TXERROR("MAKESLICE FROM LIST NOT READY")
end

proc tx_makeslice(unit p, a,b, int t)=
	CPL "MAKESLICE/TX"
	tpass(a)

	if ttbasetype[a.mode]<>tref then txerror("slice init not ref") end
	if tttarget[a.mode]<>tvoid then
		if not comparemodes(a.mode,createrefmode(nil,tTtarget[t])) then
			txerror("slice/ptr mismatch")
		end
	end

	tpass(b,ti64)
	setmode(p, t)
CPL "MKSLICE2"
	p.resultflag:=1
end

proc tx_makeset(unit p,a, int t)=
	p.isconst:=1

	while a, a:=a.nextunit do
		tpass(a)

		if not a.isconst then
			p.isconst:=0
		end
	end

	setmode(p, tvoid)
end

proc tx_dot(unit p,a,b,int lv)=
	int recmode,recbasemode,i,j,newtag,tmode
	unit q,pindex
	symbol d,dequiv

	tpass(a)			!lhs, yeields ^array type

	recmode:=a.mode

	recbasemode:=ttbasetype[recmode]

	while recbasemode=tref do
		tmode:=tttarget[recmode]
		insertunit(a,jptr)
		recmode:=tmode
		setmode(a, tmode)
		recbasemode:=ttbasetype[recmode]
	end

	if ttbasetype[recmode]<>trecord then
		txerror("Bad record type")
	end

	d:=b.def

	if d.nameid=nullid then			!not resolved; lhs mode wasn't available
		d:=b.def:=resolvefield(d,recmode)
	end

	if d.mode=tbitfield then
		i:=d.bitoffset
		j:=i+d.bitfieldwidth-1
		dequiv:=d.equivfield

		b.def:=dequiv				!change from bitfield field to containing int
		setmode(b, dequiv.mode)
		p.offset:=d.offset

		if i=j then					!single bit
			pindex:=createconstunit(i,ti64)
			newtag:=jdotindex
		else						!bit slice
			pindex:=createunit2(jmakerange,createconstunit(i,ti64),createconstunit(j,ti64))
			setmode(pindex, trange)
			pindex.a.resultflag:=1
			pindex.b.resultflag:=1
			newtag:=jdotslice
		end

		setmode(p, b.mode)
		twiden(p,lv)
		insertunit(p,newtag)
		setmode(p, tu64)
		p.b:=pindex
		p.a.resultflag:=1
		p.b.resultflag:=1
		p.resultflag:=1

		return

	end

	setmode(b, d.mode)
	setmode(p, d.mode)

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
		end
	else
		txerror("4:record expected")
	end case
	t:=ttnamedef[m]

	e:=finddupl(t,d)
	if not e then
		txerror_s("Not a field: #",d.name)
	end
	return e
end

proc tx_andl(unit p,a,b)=
	tpass(a,tbool)
	tpass(b,tbool)
	setbool(p)
end

proc convintconst(unit p,i64 x)=
!convert unit p into int const x
	p.tag:=jconst
!CPL "CCI1", STRMODE(P.MODE)
	p.mode:=ti64
!	p.mode:=ti64
!CPL "CCI2", STRMODE(P.MODE)
	p.a:=p.b:=p.c:=nil
	p.value:=x
	p.isconst:=1
!CPL "CCI3", STRMODE(P.MODE)
end

!proc tx_sliceptr(unit p,a)=
!	int m,tmode
!
!	tpass(a)
!	m:=a.mode
!
!	case ttbasetype[m]
!	when tslice then
!	else
!		txerror_s("SLICEPTR #",strmode(m))
!	end case
!
!!for when ptr is to be pointer to the array
!	tmode:=createarraymodek(nil, tttarget[m], ttlower[m],0,0)
!
!!for when ptr is to be pointer to the array element (although either can be
!!cast to the other); although try alternate .sliceptr versions too
!!tmode:=tttarget[m]
!
!	setmode(p, createrefmode(nil,tmode))
!end

proc tx_swap(unit p,a,b)=
	int av, bv

	tpasslv(a)
	tpasslv(b)

	if not comparemodes(a.mode,b.mode) then
		txerror("SWAP: type mismatch")
	end

	setmode(p, tvoid)
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
		end

		q:=q.nextunit
	end

	tpass(c,t,lv)
	u:=getdominantmode(u,c.mode)

	q:=b
	while q do
		coerceunit(q,u)
		q:=q.nextunit
	end

	setmode(p, u)
end

proc tx_case(unit p,a,b,c, int t,lv)=
	int amode,u
	unit wt,w

	if p.tag=jdocase and lv then gerror("&docase") end

	tpass(a)

	if a=nil then
		amode:=tany
	else
		amode:=a.mode
	end

	if ttisinteger[amode] and ttsize[amode]<8 then
		coerceunit(a,tint)
		amode:=tint
	end
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
						end
				else
					coerceunit(w,amode)
				end
			end
			w:=w.nextunit
		end
		tpass(wt.b,t,lv)			!process block
		if t<>tvoid then
			if u then
				u:=getdominantmode(u,wt.b.mode)
			else
				u:=wt.b.mode
			end
		end
		wt:=wt.nextunit
	end

	if c then
		tpass(c,t,lv)
		if t=tany then
			u:=getdominantmode(u,c.mode)
		end
	elsif t<>tvoid then
		txerror("case needs else")
	end

	if t<>tvoid then
		setmode(p, u)
	else
		setmode(p, tvoid)
	end

end

proc tx_notl(unit p,a)=
	tpass(a)
	setbool(p)
end

proc tx_typepun(unit p,a)=
	int smode,tmode

	case a.tag
	when jmakelist then
		TXERROR("TYPEPUN/LIST")
	else
		tpass(a)

		smode:=ttbasetype[a.mode]
		tmode:=ttbasetype[p.oldmode]

!CPL =STRMODE(SMODE)
!CPL =STRMODE(TMODE)
!CPL =STRMODE(P.OLDMODE)

		case smode
		when tr64 then
			if tmode in [ti64, tu64] then
			else
				error
			end
		when ti64, tu64 then
			case tmode
			when tr64 then
			when tr32 then
			else
				error
			end case

		when tr32 then
!CPL "SMODE=TR32", STRMODE(TMODE)
			case tmode
			when ti32 then tmode:=ti64			!will be widened to 64 bits
			when tu32 then tmode:=tu64
			else
error:			txerror("Typepun: invalid")
			end case
		end case

		setmode(p, tmode)
		p.oldmode:=smode

!CPL "------",STRMODE(P.MODE), STRMODE(P.OLDMODE)!, strmode(a.mode)
!PRINTUNIT(P)
	end case
end

proc tx_exit(unit p,a)=
	if a=nil then return end
	tpass(a,ti64)
	if a.tag<>jconst then
		txerror("exit/etc not const")
	end
	p.loopindex:=a.value
	p.a:=nil
end

proc tx_goto(unit p,a)=
	int m

	tpass(a)
	m:=a.mode

	if ttbasetype[m]<>tref or ttbasetype[tttarget[m]]<>tlabel then
		txerror("goto: not label")
	end
end

proc tx_switch(unit p,a,b,c,int t,lv)=
	[0:2048]byte valueset
	unit wt, w
	int ax,bx,i,u

	if p.tag=jdoswitch and lv then gerror("&doswitch") end

	if p.tag=jdoswitchx then
		tpass(a)
		tpass(a.nextunit)
		if ttbasetype[a.mode]<>tref then txerror("not ref") end
	else
		tpass(a,ti64)
	end

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
			end

			case ttbasetype[w.mode]
			when trange then			!assume makerange
				ax:=w.a.value
				bx:=w.b.value
	dorange:
				for i:=ax to bx do
					if i<valueset.lwb or i>valueset.upb then
						txerror("switch: value out of range")
					end
					if valueset[i] then
						cpl i
						txerror("Duplicate switch value")
					end
					valueset[i]:=1
				end
			else
				coerceunit(w,ti64,0)
				tevaluate(w)
				if w.tag<>jconst then
					txerror("Switch value: not const int")
				end
				ax:=bx:=w.value
				goto dorange
			end case
			w:=w.nextunit
		end
		tpass(wt.b,t,lv)

		if t=tany then
			if u then
				u:=getdominantmode(u,wt.b.mode)
			else
				u:=wt.b.mode
			end
		end

		wt:=wt.nextunit
	end

	if c then
		tpass(c,t,lv)
		if t=tany then
			u:=getdominantmode(u,c.mode)
		end
	elsif t<>tvoid then
		txerror("switch needs else")
	end

	if t<>tvoid then
		w:=b.a
		while w do				!all elseif unots
			if t=tany then
				coerceunit(b.b,u)
			end
			setmode(w, b.b.mode)
			w:=w.nextunit
		end
		if t=tany then
			coerceunit(c,u)
			setmode(p, u)
		else
			setmode(p, t)
		end
	else
		setmode(p, tvoid)
	end
end

proc tx_return(unit p,a, int t)=
 	int m,nvalues,nret,i
	^[]i32 pmult
	unit q

	m:=currproc.mode
	nret:=currproc.nretvalues
	pmult:=ttmult[currproc.mode]

	if a=nil then
		if nret then
			txerror("return value(s) missing")
		end
		return
	elsif nret=0 then
		txerror("Superfluous return value")
	end

	if a.tag=jmakelist then
		a.tag:=jreturnmult
		if a.length<>nret then
			case ttbasetype[m]
			when trecord, tarray then
				txerror("return constructor not supported")
			else
				txerror("Wrong number of return values")
			end case
		end
		q:=a.a				!point to list of return values
		for i to nret do
			tpass(q,pmult[i])
			q:=q.nextunit
		end

		deleteunit(p,a)			!don't need return
		if t=tvoid then
			setmode(p, tvoid)
		else
			setmode(p, ttuple)
		end
		P.RESULTFLAG:=1

	else
		if nret>1 then txerror("RETERROR?") end
		tpass(a,m)

		if t=tvoid then					!regular out-of-line return
			setmode(p, tvoid)
		else
			deleteunit(p,a)
!			P.MODE:=A.MODE
		end
	end

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
			a.mode:=a.oldmode:=tu64
			a.resultflag:=1

		else
			txerror("a.[i]: not int/str value")
		end
	end

	tpass(b)			!index

	case ttbasetype[b.mode]
	when trange then
		i:=b.a
		j:=b.b
		if i.tag=j.tag=jconst then
			if i.value>j.value then
				swap(b.a,b.b)
			end
		end
	else					!assume simple index
		coerceunit(b,ti64)
	end case

	setmode(p, pmode)
end

proc tx_slice(unit p,a,b) =
!a[b], b is a rtange

	tpass(a)			!lhs
	tpass(b)			!will be a range

	if a.mode=trefchar then
		setmode(p, createslicemodek(currproc,tc8,1,0))
	else
		deref(a)
		case ttbasetype[a.mode]
		when tarray then
			setmode(p, createslicemodek(currproc,tttarget[a.mode],1, 0))

		when tslice then
			setmode(p, a.mode)

		else
			CPL =STRMODE(A.MODE)
			txerror("a[i..j]: not array")
		end case
	end
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
		insertunit(p, jwiden)
		p.oldmode:=m
		setmode(p, gettypebase(m))
	else
		PRINTUNIT(P)
		txerror_s("widen? #",jtagnames[p.tag])
	end case
end

proc tstringslice(unit p, int slicemode)=
!p is a string; insert conversions to turn it into a slice:
	unit a,b,prange
	int length

	if tttarget[slicemode]<>tc8 then
		txerror("Not char slice")
	end
!
	a:=p
	insertunit(p,jslice)


	if p.a.tag=jconst then
	else
		b:=duplunit(p.a)
		insertunit(b,junary)
		prange:=createunit2(jmakerange,createconstunit(1,ti64),b)

		setmode(prange, trange)
		p.b:=prange
	end

	setmode(p, slicemode)
end

proc tx_bitfield(unit p,a,int lv)=
	int i,j,bitsize,topbit
	unit r

	tpass(a,,lv)

	if not ttisinteger[a.mode] and not ttisref[a.mode] then
		if ttisreal[a.mode] then
			insertunit(a,jtypepun)
			a.mode:=a.oldmode:=tu64
			a.resultflag:=1
		else
			txerror("Int/^needed")
		end
	end

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
		end
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
	end case

	if i=j then			!single bit
		p.tag:=jdotindex
		p.b:=createconstunit(i,ti64)
		p.resultflag:=1
		p.b.resultflag:=1

		if p.bfcode=bf_even then
			setmode(p, tu64)
			addnotl(p)
		end

	else
		r:=createunit2(jmakerange,createconstunit(i,ti64),createconstunit(j,ti64))
		r.a.resultflag:=1
		r.b.resultflag:=1
		setmode(r, trange)
		p.tag:=jdotslice
		p.b:=r
	end

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
		setmode(a, tmode)

		abasemode:=ttbasetype[a.mode]
	end

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
	end

	pfunc:=createname(e)
	setmode(pfunc, e.mode)
	prec.nextunit:=pargs

	p.a:=pfunc
	p.b:=prec
end

proc do_bounds(unit p,a) =
	int m,mbase,opc,lower,upper

	deref(a)

	m:=a.mode
	if a.tag=jtypeconst then m:=a.value end

	mbase:=ttbasetype[m]
	p.mode:=ti64

	case p.pclop
	when kklwb then
		case mbase
		when tarray,tslice then
			convintconst(p,ttlower[m])
			return
		else
error:
			txerror_s("lwb/upb/len?",strmode(m))
		end case

	when kkupb then
		case mbase
		when tarray then
			convintconst(p,ttlower[m]+ttlength[m]-1)
		when tslice then
			p.tag:=junary			!code gen needs to look at type, and use .propcode
		else
			goto error
		end case

	when kklen then
		case mbase
		when tarray then
			convintconst(p,ttlength[m])
		when tslice then
			p.tag:=junary
!			p.pclop:=klen
		else
			goto error
		end case
	when kkbounds then
		setmode(p, trange)
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
		end case
	when kksliceptr then
		if mbase<>tslice then txerror("Not slice") end
		p.tag:=junary

	end case
end

proc addnotl(unit p)=
	insertunit(p,jnotl)
	setbool(p)
	p.pclop:=knot
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
			end
		end

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
				end
			end
!		FI
	end

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
		end
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
				end
				return q
			end
		else
			return nil
		end
	else
		return nil
	end case

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
				if p.pclop=ksubpx then
					offset:=-offset
				end
				if lhs.b=nil then
					lhs.b:=createconstunit(offset,ti64)
				else
					lhs.b.value+:=offset
				end
				deleteunit(p,lhs)
			end
		end
		return
	end

	if ttisreal[p.mode] then
		x:=p.a.xvalue
		y:=p.b.xvalue
	else
		a:=p.a.value
		b:=p.b.value
	end

	case p.mode
	when ti64, tu64 then

		case p.pclop
		when kadd then c:=a+b
		when ksub then c:=a-b
		when kmul then c:=a*b
		when kidiv then
			if b=0 then txerror("x/0") end
			c:=a/b
		when kirem then
			if b=0 then txerror("x rem 0") end
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

		case p.pclop
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
	end case
!
	if ttisreal[p.mode] then
		makenewconst(p,i64@(z))
	else
		makenewconst(p,c)
	end
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

		if p.tag in [jistruel, jisfalsel] then dobool end

		case p.pclop
		when kneg then c:=-a

!		when ktoboolt then
!
!CPL "EVALMONO/XXTOBOOLT1"
!
! c:=istrue a; p.mode:=tbool
		when knot then c:=not a; setbool(p)
		when kbitnot then c:=inot a
		when kabs then c:=abs a

		else
			return
		end case
	when tr64, tr32 then
		case p.pclop
		when kneg then z:=-x

!		when katan then z:=atan(x)
!		when ksqrt then z:=sqrt(x)

		else
			return
		end case

	when tbool then

dobool:
		case p.tag
		when jistruel then c:=istrue a; setbool(p)
		when jisfalsel then c:=not a; setbool(p)
		elsecase p.pclop
		when ktoboolt then c:=istrue a; setbool(p)
		when knot then c:=not a; setbool(p)
		end case
	else
		return
	end case

	if ttisreal[p.mode] then
		makenewconst(p,i64@(z))
	else
		makenewconst(p,c)
	end
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
	end
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
		end
	end
	clen:=alen+blen

	if blen=0 then
		deleteunit(p,a)
		return
	elsif alen=0 then
		deleteunit(p,b)
		return
	end

	s:=pcm_alloc(clen+needterm)
	memcpy(s,a.svalue,alen)
	memcpy(s+alen,b.svalue,blen)
	if needterm then
		(s+clen)^:=0
	end

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


!	if atype='S' or atype=0 then
	if atype<>'B' then
		--alen				!lose zero terminator
		needterm:=1
	else
		needterm:=0
	end

	clen:=alen*scale
	if scale<1 or clen<1 or clen>100000 or alen<1 then txerror("mulstr") end

	t:=s:=pcm_alloc(clen+needterm)
	to scale do
		memcpy(t,a.svalue,alen)
		t+:=alen
	end
	if needterm then
		(s+clen)^:=0
	end

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
	end

!CPL "TX STRINCLUDE", A.SVALUE, CURRPROC.NAME

	fileno:=modules[p.moduleno].fileno

	pf:=getsupportfile(a.svalue,path:sources[fileno].path)

	a.svalue:=pf.text
	a.slength:=pf.size+1
	a.strtype:=p.strtype

	if a.strtype='B' then				!string
		--a.slength						!there will already be zero-terminator
	end
!
!CPL "DONE STRINCL",A.STRTYPE
	deleteunit(p,a)
end

proc coerceunit(unit p, int t, hard=0)=
	int opc, s:=p.mode, n

	if t=tvoid or s=t then return end
	if s=tvoid then
		txerror("Void expression/return value missing")
	end

	if s=t then return end

	int sbase:=ttbasetype[s]
	int tbase:=ttbasetype[t]

	opc:=kkerror
	int starg:=tttarget[s]
	int ttarg:=tttarget[t]

	if s=trefchar then sbase:=trefchar end
	if t=trefchar then tbase:=trefchar end

	if sbase in tfirstnum..tlastnum then
		if tbase in tfirstnum..tlastnum then
			opc:=softconvtable[sbase,tbase]
		elsecase tbase
		when tref, trefchar then
			opc:=kksoftconv
checkhard:
			if not hard then opc:=kkharderr end
!		elsif tbase in tfirstshort..tlastshort then
		elsif stdsize[tbase] in 1..4 and tbase<>tr32 then
			if ttisinteger[sbase] then
				if not hard then				!needed for idata init
					opc:=kksofttrun
				else
					opc:=kktruncate
				end
			end
		elsecase tbase
		when tbool then
			opc:=kktoboolt
!		when ttype then
!			opc:=kksoftconv
		end

	elsecase sbase
	when tbool then
		if tbase in [ti64, tu64] then
			opc:=kksoftconv
		end

	when tref then
		case tbase
		when ti64, tu64 then
			opc:=kksoftconv
			checkhard
		when tref then
			if starg=tvoid or ttarg=tvoid then			!at least one is ^void
				opc:=kksoftconv
			else
checkref:
				opc:=kksoftconv
				if not comparemodes(s,t) then
					checkhard
				end
			end
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
			end
		when tbool then
			opc:=kktoboolt
		when tslice then
!			if ttarg not in [tc8, tu8] then
			if ttarg in [tc8, tu8] then
				opc:=kkichar2sl
			end
		when tarray then
			if p.tag=jconst and p.strtype then
				opc:=kksoftconv
				n:=ttlength[t]
				if n=0 then
					ttlength[t]:=p.slength/ttsize[tttarget[p.mode]]
					ttsize[t]:=p.slength
				else
!!					txerror("Array not empty")
					CPL("Array not empty")
				end
			end

		end

	when tarray then
		case tbase
		when tarray then
			if comparemodes(s,t) then
				opc:=kksoftconv
			end
		when tslice then
			if comparemodes(starg, ttarg) then
				opc:=kkax2slice
			end

		when trefchar then
			if starg in [tc8, tu8] then
!CPL =STRMODE(STARG), =STRMODE(TBASE)
!PRINTUNIT(P)
!

!TXERROR("KKCX2ICHAR:A")
				opc:=kkcx2ichar
			end
!RETURN
		when tref then
			if ttarg=tvoid then
				opc:=kkcx2ichar
!TXERROR("KKCX2ICHAR:B")
			end
		end case

	when tslice then
		case tbase
		when tslice then
			if comparemodes(s,t) then
				opc:=kksoftconv
			end
		when tref then
			if ttarg=tvoid or comparemodes(starg, ttarg) then
GERROR("COERCE/SLICEPTR")
!				opc:=ksliceptr
			end

		end case

!	when ttype then
!		if tbase<=tlastnum then
!			opc:=kksoftconv
!
!		end
	end

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
!		PRINTUNIT(P)

		txerror_ss("Can't do conversion: # => #",strmode(s),strmode2(t))

	when kkharderr then
		txerror_ss("Need explicit cast: # => #",strmode(s),strmode2(t))

	when kksoftconv then
		setmode(p, t)
		return
	when kksofttrun then
		if tevalconvert(p,s,t,opc) then
			return
		end
		insertunit(p,jshorten)
		setmode(p, t)			!don't use the short target mode
		return

	when kkax2slice then
		insertunit(p,jslice)
		setmode(p, t)
		return
	when kkichar2sl then
		tstringslice(p,t)
		return

	when kkcx2ichar then
!TXERROR("KKCX2ICHAR-C")
!		insertunit(p,jaddroffirst)
		insertunit(p,jaddrof)
		setmode(p, trefchar)
		return
	end case

	if tevalconvert(p,s,t,opc) then		!try and apply it directly
		return
	end

!have to add an explict conversion node
	int tag
	case opc
	when kkwiden then tag:=jwiden
	when kkfwiden then tag:=jfwiden
	when kkfnarrow then tag:=jfnarrow
	when kkfix then tag:=jfix
	when kkfloat then tag:=jfloat
	when kktruncate then tag:=jtruncate
	when kktoboolt then tag:=jtoboolt
	else
		txerror_s("applyconv? ",convnames[opc])
	end case

!	insertunit(p, jconvert)
!	p.pclop:=opc

	insertunit(p, tag)
	p.pclop:=opc

	p.oldmode:=s
	p.resultflag:=1

!???
	if ttisshort[t] then
		p.oldmode:=t
		t:=gettypebase(t)
	end

	setmode(p, t)
end

func comparemodes(int s,t)int=
!return 1 if modes s,t are compatible. That is, ^s/^t would be interchangeable.
!a direct compare may be false because refs/arrays but be constructed at
!different times
	int sbase, tbase, starg, ttarg
	symbol d,e

	if s=t then return 1 end

	sbase:=ttbasetype[s]
	tbase:=ttbasetype[t]
	starg:=tttarget[s]
	ttarg:=tttarget[t]

	if sbase=tbase then
		case sbase
		when tref then
			if starg=tvoid or ttarg=tvoid then
				return 1
			end
			return comparemodes(starg,ttarg)

		when tarray then
			if not comparemodes(starg, ttarg) then return 0 end
			if ttlength[s]=ttlength[t] or ttlength[s]=0 or ttlength[t]=0 then
				return 1
			end
		when tslice then
			return comparemodes(starg, ttarg)

		when tproc then
			d:=ttnamedef[s]
			e:=ttnamedef[t]
			if d and e then
				if not comparemodes(d.mode,e.mode) then return 0 end
				if d.paramlist=nil and e.paramlist=nil then return 1 end
			end
		end case

	elsif sbase=tc8 and tbase=tu8 or sbase=tu8 and tbase=tc8 then
		return 1
	else
!else needs complex param/result-matching
!...
	end
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
	end
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
			if sbase=tbase then return 1 end
			return 0
		end
	end case

	if ttisreal[t] then
		makenewconst(p,i64@(z),t)

	else
		makenewconst(p,c,t)
	end

	return 1
end

proc tx_assign(unit p,a,b,int t)=
	int m,mm,needres:=t<>tvoid
	symbol d

	case a.tag
	when jmakelist then
		if b.tag=jmakelist then
			if needres then txerror("Mult assign has no result") end
			tx_assignmultmult(p,a,b)
		else
			tx_assignmultscalar(p,a,b,t)
		end
		return
	when jdotindex, jdotslice then
		tx_dotindex(a,a.a,a.b,needlv)
		tpass(b,a.mode)
		p.mode:=ti64
		return
	end case

	if a.tag=jname and a.def.islet and p.initlet then
		tpass(a)
	else
		tpasslv(a)
	end
	m:=a.mode

	a.resultflag:=needres

	if ttbasetype[m]=tslice and b.tag=jmakelist then
		tx_makeslicefromlist(b,b.a,m)
		setmode(p, m)

	else
		if b.pclop in [kidiv, kirem] then		!CAN'T JUST OVERRIDE MODE
			tpass(b)
		elsif b.tag=jread then
			tpass(b,m)
		else
			mm:=m
			if ttisshort[m] then
				mm:=gettypebase(m)
			end
			case b.tag
			when jautocast then
				tpass(b,mm)
			when jmakelist then
				tpass(b,m)

			else
				tpass(b,mm)
			end case
			setmode(p, mm)


!Eliminate widening when lhs is not wider than rhs (and when an Widen conversion is used
!which implies that rhs is < 8 bytes)

!			if b.tag=jconvert and b.convcode=kkwiden and
			if b.tag=jwiden and
				 ttsize[a.mode]<=ttsize[b.oldmode] and not needres then
!CPL "DELETEUNIT/WIDEN/TX"
				DELETEUNIT(B, B.A)
			end

		end
	end
end

proc tx_assignmultmult(unit pp,a,b)=
!mult:=mult
	unit p,q,lhs,rhs

	pp.tag:=jassignmm

	if a.length<>b.length then
		txerror("Mult assign: count mismatch")
	end
	if a.length=0 then
		txerror("Invalid assignment")
	end
	rhs:=b.a
	lhs:=a.a

	p:=lhs
	while p, p:=p.nextunit do
		tpasslv(p)
	end

	p:=lhs

	q:=rhs
	while q, (p:=p.nextunit; q:=q.nextunit) do
		tpass(q,p.mode)
	end
end

proc tx_assignmultscalar(unit pp,a,b,int t)=
!assign 'scalar' to mult LHS, but it might be a tuple type or be an expandable one
	unit p,q, alist:=a.a
	int nretmodes,i, alength:=a.length
	^[]i32 pmult
	symbol d				!point to def containing return mode info

	nretmodes:=0
	pp.tag:=jassignms

	tpass(b,tany)

	case ttbasetype[b.mode]
	when ttuple then
		d:=getprocretmodes(b)
		nretmodes:=d.nretvalues

		if ttbasetype[d.mode]<>ttuple then txerror("Not a tuple") end

		if alength>nretmodes then
			txerror("mult ass/mult returns don't agree in number")
		end
		if nretmodes<=1 then
			txerror("mult ass rhs needs fn yielding 2+ values")
		end

		p:=alist
		pmult:=ttmult[d.mode]
		i:=1

		while p, p:=p.nextunit do
			tpasslv(p,pmult[i++])
		end
	when tslice then
		if alength<>2 then txerror("(a,b):=slice") end
		tpasslv(alist,createrefmode(nil, tttarget[b.mode]))
		tpasslv(alist.nextunit,ti64)

	when trange then
	when trecord then

	elsif b.tag=jbin and b.pclop=kidivrem then
		if alength<>2 then txerror("(a,b):=divrem") end
		tpasslv(alist,b.mode)
		tpasslv(alist.nextunit,b.mode)
		pp.tag:=jassignmdrem

	else
		txerror_s("Can't expand to mult values:",strmode(b.mode))
	end case

	setmode(pp, t)
end

proc tpasslv(unit p, int t=tany)=
!process p as lvalue, but require it to be of type t
!however no conversion is done (not allowed); only a compare is done
	tpass(p,,needlv)
	if t not in [tany, tvoid] then
		if not comparemodes(p.mode, t) then
			txerror_ss("PassLV type mismatch: #:=#",strmode(p.mode), strmode2(t))
		end
	end
end

func dobinnumx(unit p,a,b)int=
!Try and apply this to binary tclopnds:
!	NUMX	NUMX	DOM
!a and b have already been processed, but not coerced to any type yet

	int amode:=a.mode, bmode:=b.mode, cmode

	if isnum(amode) and isnum(bmode) then
		cmode:=min(amode, bmode)
		setmode(p, cmode)
		coerceunit(a,cmode)
		coerceunit(b,cmode)
		return 1
	end

	if isnum(amode) and isbool(bmode) then
		setmode(p, amode)
		coerceunit(b,amode)
		return 1
	elsif isbool(amode) and isnum(bmode) then
		setmode(p, bmode)
		coerceunit(a,bmode)
		return 1
	end


	return 0
end

func dobinnumf(unit p,a,b)int=
!Try and apply this to binary tclopnds:
!	NUMF	NUMF	DOM
	int amode:=a.mode, bmode:=b.mode, cmode

!	if amode=ti64 then coerceunit(a, tr64); amode:=tr64 end
!	if bmode=ti64 then coerceunit(b, tr64); bmode:=tr64 end

	if isnumf(amode) and isnumf(bmode) then
		cmode:=min(amode, bmode)
		setmode(p, cmode)
		coerceunit(a,cmode)
		coerceunit(b,cmode)
		return 1
	end
	return 0
end

func dobinnumi(unit p,a,b)int=
!Try and apply this to binary tclopnds:
!	NUMI	NUMI	DOM
	int amode:=a.mode, bmode:=b.mode, cmode

	if isnumi(amode) and isnumi(bmode) then
		cmode:=min(amode, bmode)
		setmode(p, cmode)
		coerceunit(a,cmode)
		coerceunit(b,cmode)
		return 1
	end
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
			end
		end

		p.tag:=jinset

	end

!CPL "TXIN:"
!PRINTUNIT(P)

	unless isnum(a.mode) and b.tag in [jmakerange, jmakeset] and simpleset then
		txerror("doin")
	end
!	setmode(p, tbool)
	setbool(p)

!	if p.pclop=kknotin then
	if p.inv then
		addnotl(p)
	end
	return 1
end

!proc setmode(unit p, int mode)=
!	int u
!	p.mode:=mode
!	u:=stdpcl[ttbasetype[mode]]
!
!	if u=tblock then
!		case ttsize[mode]
!		when 8 then u:=tu64
!		when 4 then u:=tu32
!		when 2 then u:=tu16
!		when 1 then u:=tu8
!		end case
!	end
!	p.pmode:=u
!end
!
=== mnoos.m 0 0 51/74 ===

export record rsystemtime =
	u16 year
	u16 month
	u16 dayofweek
	u16 day
	u16 hour
	u16 minute
	u16 second
	u16 milliseconds
end

export proc os_init=
end

export func os_execwait(ichar cmdline,int newconsole=0,ichar workdir=nil)int =
	0
end

export func os_execcmd(ichar cmdline, int newconsole=0)int =
	0
end

export func os_getch:int=
	0
end

export func os_kbhit:int=
	0
end

export func os_getdllinst(ichar name)u64=
	0
end

export func os_getdllprocaddr(int hinst,ichar name)ref void=
	nil
end

export proc os_initwindows=
end

export proc os_gxregisterclass(ichar classname)=
end

!global function mainwndproc (
!		wt_handle hwnd, wt_uint message, wt_wparam wParam, wt_lparam lParam)int=
!	0
!end

export proc os_setmesshandler(ref void addr)=
end

export func os_getchx:int=
	27
end

export func os_getos=>ichar=
	return "NOOS"
end

export func os_gethostsize=>int=
	return 64
end

export func os_shellexec(ichar opc, file)int=
	return system(file)
end

export proc os_sleep(int a)=
!	Sleep(a)
end

export func os_getstdin:filehandle =
	return fopen("con","rb")
end

export func os_getstdout:filehandle =
	return fopen("con","wb")
end

export func os_gethostname:ichar=
	"?"
end

export func os_getmpath:ichar=
!BART
!	return "C:\\m\\"
	return F"C:\m\" !ABC
!	return "C:@@@@\\m\\" !ABC
end

export func os_clock:i64=
	clock()
end

export func os_ticks:i64=
	clock()
end

export func os_iswindows:int=
	return 1
end

export proc os_getsystime(ref void)=
end

export proc os_peek=
end

export func os_allocexecmem(int n)ref byte=
	nil
end

export func dirlist(ichar filespec, ref[]ichar dest, int capacity, t=1)int=
	0
end

export func os_hpcounter:int a =
!return counter such that successive calls indicate duration in msec
	0
end

export func os_hpfreq:int a =
	0
end

=== ms.m 0 0 52/74 ===
!project =

	module mm_decls

	module mm_cli				! Command line interface

	module mm_lex				! Lexer, produces tokens
	module mm_parse				! Parser, produces ST, TT and AST1
	module mm_name				! Name resolution, AST1 to AST2
	module mm_type				! Type analysis, AST2 to AST3

!	module mm_diags				! diagnostics
!Embedded SYSLIB sources
!	module mm_libsources		!Embedded Syslib sources
!	module mm_libsources_dummy
!
	module mm_modules			! Module handling

	module mm_support			! Source file handling

	module mm_tables			! Enumerations
	module mm_lib				! Support functions

!AST to PCL IL
	module mm_genpcl			! ST/AST -> PST/PCL
	module mm_blockpcl			! AST -> PCL

!PCL Support Library

	module pcl_decls			! Data structures, vars, enums and tables
	module pcl_api				! PCL-generating API and support routines
	module pcl_diags			! Dump PCL code by function
!	module pcl_diags_dummy		! Dump PCL code by function
!!
!!!!PCL Interpreter
!	module pcl_run				! Fixups and dispatch loop
!
!
!!PCL->MCL Inbetween modules
	module mcl_decls			! Data structures, vars, enums, and tables
	module mcl_gen				! ST/PCL to MCL AND some support
	module mcl_genaux			! Other support routines
	module mcl_stack			! PCL stack management (and some diags)
	module mcl_block			! PCL to MCL per functions
	module mcl_blockaux			! Support routines for PCL to MCL
!
!!MCL Support Library
	module mcx_decls			! Data structures, vars, enums, and tables
	module mcx_asm				! Dump generated MCL as ASM source code

	module mcx_lib				! Low-level MCL-generating API and other support

	module mcx_pedecls			! For PE format + some extra records
	module mcx_optim			! Peephole MCL Optimiser

!	module mcx_gas				! Dump generated MCL as GAS source code
!
	module mcx_genss			! MCL to SS binary code and data
!	module mcx_exe				!
!!
!!MCL Run In Memory
	module mcr_decls
	module mcr_run
	module mcr_lib
	module mcr_write




!MCL Uncomment these lines when no MCL modules presen

!global const pclpresent=0
global const pclpresent=1
!global const mclpresent=0
global const ctarget=0
!global const assemtype=0
!global proc genmcl=end
!global fun writeasm:^void = nil
!global int assemtype

global proc genc(ichar s)=end

global const clpass=0

!global proc genmcl=end
!global proc genss=end
!global proc genexe=end
!global proc writemcx(ichar s)=end
!global proc writeexe(ichar s, int a)=end
global proc writeexe(ichar outfile, int dodll, ichar entrypoint=nil)= end
!global proc writeasm(ichar s, int a)=end
!global func writeasm:^strbuffer= nil end
!global proc runlibfile(ichar s, int a)=end
!global proc =end


global const fsyslibs=0

global proc printmodelist(filehandle f)= end

global proc printst(filehandle f,ref strec p,int level=0)= end

global proc printstflat(filehandle f)=end

global proc printcode(filehandle f,ichar caption)=end

global proc printunit(ref unitrec p,int level=0,ichar prefix="*",filehandle dev=nil)=end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=end

global proc showprojectinfo(filehandle dev)=end

global proc showlogfile=CPL "NO DIAGS MODULE" end

global proc showast(ichar filename)=end

global proc printsymbol(ref tokenrec lp)=end

global proc showtimings=end

global proc writeexports(ichar basefile, modulename)=end

global proc loadbuiltins=end
=== msys.m 0 0 53/74 ===
global record procinforec=
	u16			fnindex
	byte		rettype
	byte		nparams
	[12]byte	paramlist
end

!for print/read routines
!------------------------------------------
export record fmtrec=	! (default)
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
	char	charmode	! C,M (0)  0 or 'C' or 'M'	o/p int as int or single char or multi-char
	char	heapmode	! D (0)  'D' for str-functions, return ptr to heap string
	char	param		! Use int value for <fmtparam>
	byte	spare : (showtype:1, newline:1)
end

int fmtparam			!as set with :'V'

enumdata =
	std_io,file_io,str_io
end

const comma = ','

export int $cmdskip			!0 unless set by READMCX/etc

export int needgap			= 0
int outdev			= std_io
filehandle outchan	= nil
^char fmtstr 	= nil

const maxiostack=10
[maxiostack]filehandle	outchan_stack
[maxiostack]int			outdev_stack
[maxiostack]^char	fmtstr_stack
[maxiostack]byte		needgap_stack

[maxiostack]^char	ptr_stack		!this one doesn't need pushing, as each is pointed to from outchan
int niostack=0

[0:]char digits=s"0123456789ABCDEF"
const onesixty=360
fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,0,0)

!Read buffer vars
export const rd_buffersize = 16384	!total capacity of line buffer

export ^char rd_buffer		! point to start of read buffer
export int rd_length			! length of this line (as read by readln)
export ^char rd_pos			! current position it's up to (next read starts here)
export ^char rd_lastpos		! set by sread() just before reading used for reread()

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

proc start=
	i32 nargs
	int nargs64
	ref[]ichar args
	static [128]byte startupinfo			! 68 or 104 bytes
	int res

!res:=1234567
!res:=0x1234567
!
!PUTS("MSYS/START")

	res:=__getmainargs(&nargs,cast(&args),cast(&envstrings),0,cast(&startupinfo))
!	res:=__getmainargs(&nargs,cast(&args),nil,0,cast(&startupinfo))
	
	nsysparams:=nargs

	if nsysparams>maxparam then
		printf("Too many params\n")
		stop 50
	end

	nargs64:=nargs			!bug when using 32-bit limit when compiled with mm
	for i:=1 to nargs64 do
		sysparams[i]:=args[i]
	end
	
!assume nsysparams is >=1, since first is always the program name
	ncmdparams:=nsysparams-($cmdskip+1)
	cmdparams:=cast(&sysparams[$cmdskip+1])

	int j:=1
	nenvstrings:=0
	while envstrings[j] do
		++nenvstrings
		++j
	end
end

proc pushio=
	if niostack>=maxiostack then
		printf("Too many io levels\n")
		stop 53
	end
	++niostack
	outchan_stack[niostack]	:= outchan
	outdev_stack[niostack]	:= outdev
	fmtstr_stack[niostack]	:= fmtstr
	needgap_stack[niostack]	:= needgap
	needgap:=0
	fmtstr:=nil
	outchan:=nil
end

export proc m$print_startfile(^void dev)=
	pushio()
	outchan:=cast(dev)
	if dev then
		outdev:=file_io
	else
		outdev:=std_io
	end
	resetprintbuffer()
end

export proc m$print_startstr(^char s)=
	^ref char p
	pushio()

	ptr_stack[niostack]:=s
	p:=&ptr_stack[niostack]

	outchan:=cast(p)
	outdev:=str_io
end

export proc m$print_startptr(^ref char p)=
	pushio()

	outchan:=cast(p)
	outdev:=str_io
end

export proc m$print_startcon=
	pushio()
	outdev:=std_io
	resetprintbuffer()
end

export proc m$print_setfmt(^char format)=
	fmtstr:=format
end

export proc m$print_end=
	needgap:=0
	nextfmtchars(1)
	if niostack=1 and outdev in [std_io,file_io] then
		dumpprintbuffer()
	end

	if niostack=0 then return end
	outchan	:= outchan_stack[niostack]
	outdev	:= outdev_stack[niostack]
	fmtstr	:= fmtstr_stack[niostack]
	needgap	:= needgap_stack[niostack]

	--niostack
end

export proc m$print_ptr(u64 a,ichar fmtstyle=nil)=
	if fmtstyle=nil then
		fmtstyle:="z8H"
	end
	m$print_u64(a,fmtstyle)
end

export proc m$print_ptr_nf(u64 a)=
	m$print_ptr(a)
end

export proc m$print_i64(i64 a,ichar fmtstyle=nil)=
	[40]char s
	fmtrec fmt
	int n

!PUTS("PRINT "+$STRLINENO)

	nextfmtchars()
!PUTS("PRINT "+$STRLINENO)
	if fmtstyle=nil then
!PUTS("PRINT "+$STRLINENO)
		if a>=0 then
!PUTS("PRINT "+$STRLINENO)
			n:=u64tostr(a, s,10,0)
!PUTS("PRINT "+$STRLINENO)
		elsif a=i64.min then
			fmt:=defaultfmt
			dofmt

		else
			s[1]:='-'
			n:=u64tostr(-a,&s[2],10,0)+1
		end

		printstr_n(s,n)

	else
!PUTS("PRINT "+$STRLINENO)

		strtofmt(fmtstyle,-1,&fmt)
		if fmt.param='V' then
			fmtparam:=a
			needgap:=0
		else
dofmt:
			tostr_i64(a,&fmt)
		end
	end
	needgap:=1
end

export proc m$print_i64_nf(i64 a)=
	m$print_i64(a)
end

export proc m$print_bool(i64 a, ichar fmtstyle=nil)=
	if a then
		m$print_str("True",fmtstyle)
	else
		m$print_str("False",fmtstyle)
	end
end

export proc m$print_u64(u64 a,ichar fmtstyle=nil)=
	[40]char s
	fmtrec fmt

	nextfmtchars()
	if fmtstyle=nil then
		sprintf(s,"%llu",a)
		printstr(s)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_u64(a,&fmt)
	end
	needgap:=1
end

export proc m$print_r64(real x,ichar fmtstyle=nil)=
	[360]char s
	fmtrec fmt

	nextfmtchars()
	if fmtstyle=nil then
		sprintf(s,"%f",x)
		printstr(s)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_r64(x,&fmt)
	end

	needgap:=1
end

export proc m$print_r32(r32 x,ichar fmtstyle=nil)=
	[360]char s
	fmtrec fmt

	nextfmtchars()
	if fmtstyle=nil then
		sprintf(s,"%f",x)
		printstr(s)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_r64(x,&fmt)
	end

	needgap:=1
end

global proc m$print_c8(i64 a,ichar fmtstyle=nil)=
	[32]char s
!	int cc@s
	fmtrec fmt
	int n
	byte charmode:=0

	nextfmtchars()

	if fmtstyle then
		strtofmt(fmtstyle,-1, &fmt)
		charmode:=fmt.charmode
	end

	if charmode='M' then
		n:=domultichar(^char(&a), 8, s, &fmt)
!		n:=domultichar(^char(&a), 8, str, fmt)
	else						!assume 'C'
		(^int(&s)^):=a	
		s[9]:=0

		n:=getutfsize(s)			!isolate size of next char
	end

	printstr_n(s,n)

	needgap:=1
end

export proc m$print_str(ichar s, fmtstyle=nil)=
	nextfmtchars()

	if s=nil then
		printstr("<null>")
		return
	end

	fmtrec fmt
	if fmtstyle=nil then
		printstr(s)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_str(s,-1,&fmt)
	end
	needgap:=1
end

export proc m$print_strn(ichar s, int length, ichar fmtstyle=nil)=
	nextfmtchars()

	if s=nil then
		printstr("<null>")
		return
	end

	fmtrec fmt
	if fmtstyle=nil then
		printstr_n(s,length)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_str(s,length,&fmt)
	end
	needgap:=1
end

export proc m$print_str_nf(ichar s)=
	m$print_str(s)
end

export proc m$print_strsl(slice[]char s, ichar fmtstyle=nil)=
ABORTPROGRAM("PRTSL")
!	nextfmtchars()
!
!!	fmtrec fmt
!
!	if fmtstyle=nil then
!		printstr_n(cast(s.sliceptr),s.len)
!!		printstr_n(cast(ss.str),ss.length)
!	else
!		abortprogram("FORMATED PRINT SLICE NOT READY")
!!		strtofmt(fmtstyle,-1,&fmt)
!!		tostr_str(s,s.len,&fmt)
!	end
!	needgap:=1
end

export proc m$print_newline=
!PUTS("<NEWLINE>")
	needgap:=0
	nextfmtchars(1)
	printstr("\n")
end

export proc m$print_nogap=
	needgap:=0
end

export proc m$print_space=
	needgap:=0
	printstr(" ")
end

export proc printstr(ichar s)=
	printstr_n(s,strlen(s))
end

export proc printstr_n(ichar s,int n)=

!	return when n=0

!	if niostack=1 and outdev in [std_io,file_io] then
!!puts("ADDTO BUFF")
!		addtobuffer(s,n)
!	else
!printf("DUMPSTR %lld\n", n)
		dumpstr(s,n)
!	end
end

export proc printstrn_app(ichar s, int length, filehandle f=nil)=
if length then
	if f=nil then
		printf("%.*s",length,s)
	else
		fprintf(f,"%.*s",length,s)
	end
end
end

proc printchar(int ch)=
	[4]char str

	str[1]:=ch
	str[2]:=0
	printstr_n(str,1)
end

global proc nextfmtchars(int lastx=0)=
	char c
	^char pstart
	int n
	if not fmtstr then			!format not in use
		if needgap then
			printchar(' ')
		end
		needgap:=0
		return
	end

	pstart:=fmtstr
	n:=0

	do
		c:=fmtstr^
		case c
		when '#' then
			if lastx then
				goto skip
			end
			++fmtstr
			if n then
				printstr_n(pstart,n)
			end
			return
		when 0 then
			if n then
				printstr_n(pstart,n)
			elsif not lastx then
				printstr_n("|",1)
			end
			return
		when '~' then
			if n then
				printstr_n(pstart,n)
				n:=0
			end
			++fmtstr
			c:=fmtstr^
			if c then
				++fmtstr
				printchar(c)
			end
			pstart:=fmtstr
		else
	skip:
			++n
			++fmtstr
		end case
	end
end

export proc strtofmt(^char s,int slen,^fmtrec fmt) =		!PC_STRTOFMT
!convert format code string in s, to fmtrec at fmt^
!Format code is a string containing the following char codes (upper or lower when mostly)
!n	Width
!.n	Max width/precision
!A	Convert to upper case
!a	Convert to lower case
!B	Binary
!C	Show int as single n-bit (unicode) character
!D	Duplicate string returned via STRINT etc on heap
!E,F,G	Specify format for double (corresponds to C format codes)
!F
!G
!H	Hex
!JC	Justify centre
!JL	Justify left
!JR	Justify right
!M	Show int as multi-bit (unicode) character
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

	if s=nil then return end

	if slen=-1 then slen:=strlen(s) end

	memcpy(str,s,slen)		!convert s/slen to zero-terminated string
	str[slen]:=0
	s:=str

	wset:=0
	while s^ do
		c:=s^
		++s
		if c='A' then fmt.lettercase:='A'
		elsif c='a' then fmt.lettercase:='a'
		elseswitch toupper(c)
		when 'B' then fmt.base:=2
		when 'H' then fmt.base:=16
		when 'O' then fmt.base:=8
		when 'X' then
			base:=0
			do
				c:=s^
				if c in '0'..'9' then
					base:=base*10+c-'0'
					++s
				else
					exit
				end
			end
			if base in 2..16 then
				fmt.base:=base
			end

		when 'Q' then fmt.quotechar:='"'
		when 'J' then
			fmt.justify:=toupper(s^)
			if s^ then
				++s
			end
		when 'Z' then fmt.padchar:='0'
		when 'S' then
			fmt.sepchar:=s^
			if s^ then
				++s
			end
		when 'P' then
			fmt.padchar:=s^
			if s^ then
				++s
			end
		when 'T' then
			fmt.suffix:=s^
			if s^ then
				++s
			end
		when 'U' then fmt.usigned:='W'
		when 'E' then fmt.realfmt:='e'
		when 'F' then fmt.realfmt:='f'
		when 'G' then fmt.realfmt:='g'
		when 'D' then fmt.heapmode:='D'
		when 'C' then fmt.charmode:='C'
		when 'M' then fmt.charmode:='M'
		when 'V' then fmt.param:='V'
		when 'Y' then fmt.showtype:=1
		when 'N' then fmt.newline:=1
		elsecase c
		when '.' then
			wset:=1
		when comma,'_' then fmt.sepchar:=c
		when '+' then fmt.plus:='+'
		when '~' then fmt.quotechar:='~'
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
					end
					if c>='0' and c<='9' then
						++s
						n:=n*10+c-'0'
					else
						exit
					end
				end
gotwidth:
				if not wset then
					fmt.minwidth:=n
					wset:=1
				else
					fmt.precision:=n
				end
			end
		end
	end
end

function domultichar (^char p,int n,^char dest,^fmtrec fmt)int =
!there are n (4 or 8) chars at p.!
!There could be 0 to 4 or 8 printable chars converted to string at dest
	[0:20]char str
	^char q
	int nchars

	q:=str

	nchars:=n

	to n do
		if p^=0 then exit end
		q^:=p^
		++q
		++p
	end
	q^:=0

	return expandstr(str,dest,strlen(str),fmt)
end

export function expandstr(^char s,^char t,int n,^fmtrec fmt)int =		!EXPANDSTR
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
	end

	if fmt.justify='L' then	! left-justify
		strncpy(t,s,n)
		t+:=n
		for i:=1 to w-n do
			t^:=fmt.padchar
			++t
		end
		t^:=0
	elsif fmt.justify='R' then
		if fmt.padchar='0' and fmt.base and (s^='-' or s^='+') then ! need to move sign outside 
			t^:=s^
			++t
			to w-n do
				t^:=fmt.padchar
				++t
			end
			strncpy(t,s+1,n-1)
			(t+n-1)^:=0
		else
			to w-n do
				t^:=fmt.padchar
				++t
			end
			strncpy(t,s,n)
			(t+n)^:=0
		end

	else				! centre-justify?

		m:=(w-n+1)/2
		to m do
			t^:=fmt.padchar
			++t
		end
		strncpy(t,s,n)
		t+:=n
		to w-n-m do
			t^:=fmt.padchar
			++t
		end
		t^:=0

	end
	return w
end

export function u64tostr(u64 aa,^char s,word base,int sep)int =		!U64TOSTR
!convert 64-bit int a to string in s^
!base is number base, usually 10 but can be 2 or 16. Other bases allowed
!result when a=minint (will give "<minint>")
	[0:onesixty]char t
	u64 dd
	int i,j,k,g
	^char s0

	i:=0
	k:=0
	g:=(base=10|3|4)

	repeat
!		if base=10 then			!BUGGY FOR AA OVER I64.MAX
!			assem
!				mov		rcx, [aa]
!				mov		rax, rcx
!				mov		rdx, 7378697629483820647
!				imul	rdx
!				mov		rax, rdx
!				mov		rdx, rcx
!				sar		rdx, 63
!				sar		rax, 2
!				sub		rax, rdx
!				lea		rdx, [rax+rax*4]
!				add		rdx, rdx
!				sub		rcx, rdx
!				mov		[dd], rcx
!				mov		[aa], rax
!			end
!		else
			dd:=aa rem base
			aa:=aa/base
!		end

		t[++i]:=digits[dd]

!BUG in separator logic, doesn't work when leading zeros used, eg. printing
!out a full length binary
!so perhaps move this out to expandstr
		++k
		if sep and aa<>0 and k=g then
			t[++i]:=sep
			k:=0
		end
	until aa=0

	j:=i
	s0:=s
	while i do
		s^:=t[i--]
		++s
	end
	s^:=0

	return j
end

export function i64tostrfmt(i64 aa,^char s,^fmtrec fmt)int =
!a is signed 64-bit int/long, fmt is a ^to a filled-in fmtrec
!convert a to a string in s, according to fmt
!a basic conversion is done first,: the field manipulation is done
!signed=1 for int, 0 for u32 (fmt.unsigned forces ints to be treated as longs)
!returns length of s
	[0:onesixty]char str				! allow for binary with separators!
	int n, usigned
	const i64 mindint=0x8000'0000'0000'0000

	usigned:=0
	if fmt.usigned then
		usigned:=1
	end
	if aa=mindint and not usigned then		! minint

		str[0]:='-'
		n:=i64mintostr(&str[1],fmt.base,fmt.sepchar)+1

	else
		if (not usigned and aa<0) or fmt.plus then
			if aa<0 then
				aa:=-aa
				str[0]:='-'
			else
				str[0]:='+'
			end
			n:=u64tostr(aa,&str[1],fmt.base,fmt.sepchar)+1
		else
			n:=u64tostr(aa,str,fmt.base,fmt.sepchar)
		end
	end

	if fmt.suffix then
		str[n]:=fmt.suffix
		str[++n]:=0
	end

!str uses upper cases for hex/etc see if lc needed
	if (fmt.base>10 or fmt.suffix) and fmt.lettercase='a'	then	! need lower when
		convlcstring(str)
	end

!at this point, n is the str length including signs and suffix
	return expandstr(str,s,n,fmt)
end

export function u64tostrfmt(i64 aa,^char s,^fmtrec fmt)int =		!U64TOSTRFMT
!see i64tostrfmt
	[0:onesixty]char str				! allow for binary with separators!
	int n

	n:=u64tostr(aa,str,fmt.base,fmt.sepchar)

	if fmt.suffix then
		str[n]:=fmt.suffix
		str[++n]:=0
	end

!str uses upper cases for hex/etc see if lc needed
	if fmt.base>10 or fmt.suffix and fmt.lettercase='a'	then	! need lower when
!		convlcstring(str)
	end

!at this point, n is the str length including signs and suffix
	return expandstr(str,s,n,fmt)
end

export function i64mintostr(^char s,int base,int sep)int =		!I64MINTOSTR
!convert minint to string in s do not include minus sign
!return number of chars in string
	[0:onesixty]char t
	int i,j,k,g

	case base
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
	end case

	i:=strlen(&t[0])
	s+:=i
	if sep then
		s+:=j
	end
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
		end
	end
	return strlen(s)
end

export function strtostrfmt(^char s,^char t,int n,^fmtrec fmt)int =
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
	^char u,v
	[256]char str
	int w,nheap		! whether any heap storage is used  bytes allocated

	nheap:=0

	if fmt.quotechar or fmt.lettercase then		! need local copy
		if n<256 then
			u:=str
		else
			nheap:=n+3					! allow for quotes+terminator
			u:=pcm_alloc(nheap)
		end
		if fmt.quotechar then
			v:=u
			v^:=fmt.quotechar
			++v
			if n then
				strcpy(v,s)
				v+:=n
			end
			v^:=fmt.quotechar
			++v
			v^:=0
			n+:=2
		else
			memcpy(u,s,n)
		end
		case fmt.lettercase
		when 'a' then	! need lower when
			convlcstring(u)
		when 'A' then
			convucstring(u)
		end case
		s:=u
	end

	w:=fmt.minwidth
	if w>n then
		n:=expandstr(s,t,n,fmt)
	else
		memcpy(t,s,n)
	end
	if nheap then
		pcm_free(u,nheap)
	end
	return n
end

proc tostr_i64(i64 a, ^fmtrec fmt)=
	[360]char str
	int n

	case fmt.charmode
	when 0 then
		n:=i64tostrfmt(a,str,fmt)
	when 'M' then
		n:=domultichar(^char(&a),8,str,fmt)

	else						!assume 'C'
		m$print_c8(a, nil)
!		printchar(a)			!no other formatting allowed
		return
	end case

	printstr_n(str,n)
end

proc tostr_u64(u64 a, ^fmtrec fmt)=
	[360]char str
	int n

	case fmt.charmode
	when 'M' then
		n:=domultichar(^char(&a),8,str,fmt)

	when 'C' then
		m$print_c8(a, nil)
!		printchar(a)			!no other formatting allowed
		return

	else
		n:=u64tostrfmt(a,str,fmt)
	end case

	printstr_n(str,n)
end

proc tostr_r64(real x,^fmtrec fmt) =
	[360]char str,str2
	[0:10]char cfmt
	int n

	cfmt[0]:='%'

	if fmt.precision then
		cfmt[1]:='.'
		cfmt[2]:='*'
		cfmt[3]:=fmt.realfmt
		cfmt[4]:=0
		sprintf(str,cfmt,fmt.precision,x)
	else
		cfmt[1]:=fmt.realfmt
		cfmt[2]:=0
		sprintf(str,cfmt,x)
	end

!at this point, n is the str length including signs and suffix

	n:=strlen(str)		! current length

	if n<fmt.minwidth then
		n:=expandstr(str,str2,n,fmt)
		strcpy(str,str2)
	end

	printstr_n(str,n)
end

proc tostr_str(^char s, int oldlen, ^fmtrec fmt) =
	int newlen,n
	^char t

!try and work out size of formatted string
	if oldlen=-1 then
		oldlen:=strlen(s)
	end
	newlen:=oldlen

	if fmt.quotechar or fmt.minwidth>newlen or fmt.lettercase or fmt.precision then
		if fmt.quotechar then
			newlen+:=2
		end
		if fmt.minwidth>newlen then
			newlen:=fmt.minwidth
		end
		t:=pcm_alloc(newlen+1)
		n:=strtostrfmt(s,t,oldlen,fmt)
		if fmt.precision then
			n min:=fmt.precision
		end

		printstr_n(t,n)
		pcm_free(t,newlen+1)
	else
		printstr_n(s,oldlen)
	end
end

function getfmt(ichar fmtstyle)^fmtrec=
	static fmtrec fmt
	if fmtstyle then
		strtofmt(fmtstyle,-1,&fmt)
		return &fmt
	else
		return &defaultfmt
	end
end

export function strint(i64 a, ichar fmtstyle=nil)ichar=
	static [100]char str
	^fmtrec fmt

	m$print_startstr(str)
	tostr_i64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(str,fmt)
end

export proc getstrint(i64 a, ichar dest)=
	m$print_startstr(dest)
	tostr_i64(a,getfmt(nil))
	m$print_end()
end

export function strword(u64 a, ichar fmtstyle=nil)ichar=
	static [100]char str
	^fmtrec fmt

	m$print_startstr(str)
	tostr_u64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(str,fmt)
end

export function strreal(real a, ichar fmtstyle=nil)ichar=
	static [320]char str
	^fmtrec fmt

	m$print_startstr(str)
	tostr_r64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(str,fmt)
end

export function getstr(ichar s, ^fmtrec fmt)ichar=
	if fmt.heapmode then
		return pcm_copyheapstring(s)
	else
		return s
	end
end

proc initreadbuffer=
	if rd_buffer then return end
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

	if f=filehandle(1) then
ABORTPROGRAM("READ CMDLINE")
!		rd_buffer^:=0
!		p:=getcommandlinea()
!		repeat
!			++p
!		until p^ in [' ','\t',0]
!		strcpy(rd_buffer, p)
!		rd_length:=strlen(rd_buffer)
!		rd_pos:=rd_buffer
!		rd_lastpos:=nil
		return
	end

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
	end
	rd_length:=n
	rd_pos:=rd_buffer
	rd_lastpos:=nil
end

function readitem(int &itemlength)^char =
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
	^char p,s,itemstr
	char quotechar, c

	unless rd_buffer then 
		initreadbuffer()
	end unless

	s:=rd_pos

!scan string, eliminating leading white space
	while s^=' ' or s^=9 do
		++s
	end

	itemstr:=s
	rd_lastpos:=rd_pos:=s

	if s^=0 then
		termchar:=0
		itemlength:=0
		return s
	end

	quotechar:=0
	if s^='"' then
		quotechar:='"'
		++s
	elsif s^='\'' then
		quotechar:='\''
		++s
	end

!loop reading characters until separator or end reached
	p:=itemstr:=s

	while s^ do
		c:=s++^
		case c
		when ' ', 9, comma, '=' then		! separator
			if quotechar or p=s then			!can be considered part of name if inside quotes, or is only char
				goto normalchar
			end
			termchar:=c
			exit
		else
	normalchar:
			if c=quotechar then
				if s^=quotechar then	! embedded quote
					p^:=c
					++s
					++p
				else					! end of name
					termchar:=s^
					if termchar in [',', '='] then
						++s
						termchar:=s^
					end
					exit
				end
			else
				p^:=c
				++p
			end
		end case
	end

	if s^=0 then
		termchar:=0
	end
	itemlength:=p-itemstr				! actual length of token
	rd_pos:=s

	return itemstr
end

export function strtoint(ichar s,int length=-1, word base=10)i64=
!return point to next char after terminator (which can be just off length of string)
	byte signd
	u64 aa
	word c,d

	itemerror:=0

	if length=-1 then
		length:=strlen(s)
	end
!check for sign
	signd:=0
	if length and s^='-' then
		signd:=1; ++s; --length
	elsif length and s^='+' then
		++s; --length
	end

	aa:=0
	while length do
		c:=s++^
		--length
		if c in 'A'..'F' then d:=c-'A'+10
		elsif c in 'a'..'f' then d:=c-'a'+10
		elsif c in '0'..'9' then d:=c-'0'
		elsif c in ['_', '\''] then
			nextloop
		else
			itemerror:=1
			exit
		end

		if d>=base then
			itemerror:=1
			exit
		end
		aa:=aa*base+d
	end

	if signd then
		return -aa
	else
		return aa
	end
end

global function m$read_i64(int fmt=0)i64=
	^char s
	int length

	fmt:=toupper(fmt)

	case fmt
	when 'C' then
		rd_lastpos:=rd_pos
		if rd_pos^ then
			return rd_pos++^
		else
			return 0
		end
	when 'T' then
		return termchar
	when 'E' then
		return itemerror
	end case

	s:=readitem(length)

	case fmt
	when 0,'I' then
		return strtoint(s,length)
	when 'B' then
		return strtoint(s,length,2)
	when 'H' then
		return strtoint(s,length,16)
	end case
	return 0
end

global function m$read_r64(int fmt=0)real=
	[512]char str
	^char s
	int length
	i32 numlength
	real x

	s:=readitem(length)

	if length=0 or length>=str.len then		!assume not a real
		return 0.0
	end
	memcpy(str,s,length)
	str[length+1]:=0

	itemerror:=0

	if sscanf(str,"%lf%n", &x, &numlength)=0 or numlength<>length then
		x:=0.0
		itemerror:=1
	end

	return x
end

global proc m$read_str(^char dest, int destlen=0,fmt=0)=
	^char s
	int length

	itemerror:=0
	if fmt in ['L','l'] then
		s:=rd_pos
		length:=rd_buffer+rd_length-rd_pos

	else
		s:=readitem(length)

		if fmt in ['N','n'] then
			iconvlcn(s,length)
		end
	end

	if destlen>0 then
		if length>=destlen then
			length:=destlen-1
			itemerror:=1
		end
	end
	memcpy(dest,s,length)
	(dest+length)^:=0
end

export proc readstr(^char dest, int fmt=0,destlen=0)=
	m$read_str(dest,destlen,fmt)
end

export proc rereadln=
	rd_pos:=rd_buffer
	rd_lastpos:=rd_pos
end

export proc reread=
	rd_pos:=rd_lastpos
end

export function valint(ichar s, int fmt=0)i64=
	^char old_pos, old_lastpos
	i64 aa

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
	^char old_pos, old_lastpos
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

	^ref char p

	if outdev=str_io then
		p:=cast(outchan)
		if n then
			memcpy(p^,s,n)
			p^+:=n
		end
		p^^:=0
		return
	end

	return when n=0
	if fbuffer and n>=2 and outdev=std_io then
		--printptr				!point to last char
		if printptr^=10 then
			if (printptr-1)^=13 then		!crlf
				(printptr-1)^:=0
			else							!lf only
				printptr^:=0
			end
			puts(printbuffer)
			return
		end
	end

	case outdev
	when std_io then
		printf("%.*s",n,s)
	when file_io then
		fprintf(outchan,"%.*s",n,s)
	end case
end

proc dumpprintbuffer=
	if printlen then
		dumpstr(printbuffer,printlen,1)
	end

	resetprintbuffer()
end

proc resetprintbuffer=
	printptr:=printbuffer
	printlen:=0
end

proc addtobuffer(ichar s, int n)=
	if printlen+n>=(printbuffer.len-8) then
		dumpprintbuffer()
	end

	if n<printbuffer.len then
		memcpy(printptr,s,n)
		printptr+:=n
		printlen+:=n
		return
	end

	dumpstr(s, n)			!don't bother with buffer
end

global function m$power_i64(i64 a,n)i64=
	if n<0 then
		return 0
	elsif n=0 then
		return 1
	elsif n=1 then
		return a
	elsif (n iand 1)=0 then
		return m$power_i64(sqr a,n/2)
	else			!assume odd
		return m$power_i64(sqr a,(n-1)/2)*a
	end
end

func getutfsize(^char s)int =
!work out the size in bytes of the ascii or utf8 character that s points to
	int a

	a:=s++^

	if a=0 then						!end of string
		0
	elsif a.[7]=0 then				!ascii
		1
	elsif a.[7..5]=2x110 then
		2
	elsif a.[7..4]=2x1110 then
		3
	elsif a.[7..3]=2x11110 then
		4
	else							!error: just assume a byte of random binary
		1
	end
end

!export fun `fract(real x)real = fmod(x,1.0)
!export fun fraction(real x)real = fmod(x,1.0)

export fun m$sign_i64(int a)int = (a<0|-1| (a>0|1|0))
export func m$sign_r64(real x)real =
	if x<0 then return -1 end
	if x>0 then return 1 end
	0
end
=== msysmin.m 0 0 54/74 ===
!import clib
!export type filehandle=ref void

!importdll $cstd=
importdll msvcrt=
	func malloc	(u64)ref void
	proc free		(ref void)
!	func pow		(real,real)real
!
	func printf (ref char,...)i32
!	func fprintf (ref void,ref char,...)i32
	func puts (ref char)i32
	proc `exit(i32)
	func getchar	:i32
	proc memcpy		(ref void, ref void, word)
	proc memset		(ref void, i32, u64)
	func strlen		(ichar)u64
	func strcpy		(ichar,ichar)ichar
	func strcat		(ichar,ichar)ichar
	func strcmp		(ichar,ichar)i32

	func _strdup	(ichar)ichar
end

export macro strdup=_strdup

!export proc free(ref void) = end


int needgap

!proc start=
!	CPL "MIN/START"
!end


global proc m$print_startcon=
end

global proc m$print_end=
	needgap:=0
end

global proc m$print_ptr(u64 a,ichar fmtstyle=nil)=
!	nextfmtchars()
	printf("%p",a)
	needgap:=1
end

global proc m$print_ptr_nf(u64 a)=
	nextfmtchars()
	printf("%p",a)
	needgap:=1
end

global proc m$print_i64(i64 a,ichar fmtstyle=nil)=
	nextfmtchars()
	printf("%lld",a)
	needgap:=1
end

!global proc m$print_i128(i64 a,ichar fmtstyle=nil)=
!	puts("<128>")
!!	nextfmtchars()
!!	printf("%lld",a)
!!	needgap:=1
!end

global proc m$print_i64_nf(i64 a)=
!puts("PRINTI64_nf")
	nextfmtchars()
	printf("%lld",a)
	needgap:=1
end

global proc m$print_u64(u64 a,ichar fmtstyle=nil)=
	nextfmtchars()
	printf("%llu",a)
	needgap:=1
end

global proc m$print_r64(real x,ichar fmtstyle=nil)=
	nextfmtchars()
	printf("%f",x)
	needgap:=1
end

!global proc m$print_r32(real x,ichar fmtstyle=nil)=
global proc m$print_r32(r32 x,ichar fmtstyle=nil)=
	nextfmtchars()
	printf("%f",x)
	needgap:=1
end

!global proc m$print_c8(i64 a,ichar fmtstyle=nil)=
!	nextfmtchars()
!	printf("%c",a)
!	needgap:=1
!end

global proc m$print_str(ichar s, fmtstyle=nil)=
	nextfmtchars()
	printf("%s",s)
	needgap:=1
end

global proc m$print_str_nf(ichar s)=
	nextfmtchars()
	printf("%s",s)
	needgap:=1
end

global proc m$print_space=
	needgap:=0
	printf(" ")
end

global proc m$print_newline=
	needgap:=0
	printf("\n")
end

global proc m$unimpl=
	puts("Sysfn unimpl")
	stop 1
end

global proc m$print_nogap=
	needgap:=0
end

!global proc nextfmtchars(int lastx=0)=
global proc nextfmtchars=
	if needgap then
		printf(" ")
		needgap:=0
	fi
end

!global proc m$stop(int stopcode)=
!	`exit(stopcode)
!end
!
!global func strint(i64 a, ichar fmtstyle=nil)ichar=
!	return "?"
!end
!

!global function m$power_i64(i64 a,n)i64=
!	if n<0 then
!		return 0
!	elsif n=0 then
!		return 1
!	elsif n=1 then
!		return a
!	elsif (n iand 1)=0 then
!		return m$power_i64(sqr a,n/2)
!	else			!assume odd
!		return m$power_i64(sqr a,(n-1)/2)*a
!	fi
!end


=== msystemp.m 0 0 55/74 ===
!import clib
!export type filehandle=ref void

importdll $cstd=
	func malloc	(word64)ref void
	proc free		(ref void)
!	func pow		(real,real)real
!
	func printf (ref char,...)int32
!	func fprintf (ref void,ref char,...)int32
	func puts (ref char)int32
	proc `exit(int32)
	func getchar	:int32
	proc memcpy		(ref void, ref void, word)
	proc memset		(ref void, int32, u64)
	func strlen		(ichar)u64
end

!export proc free(ref void) = end


int needgap

global proc m$print_startcon=
end

global proc m$print_end=
	needgap:=0
end

!global proc m$print_ptr(u64 a,ichar fmtstyle=nil)=
!!	nextfmtchars()
!	printf("%p",a)
!	needgap:=1
!end

global proc m$print_ptr_nf(u64 a)=
	nextfmtchars()
	printf("%p",a)
	needgap:=1
end
!
!global proc m$print_i64(int64 a,ichar fmtstyle=nil)=
!	nextfmtchars()
!	printf("%lld",a)
!	needgap:=1
!end

!global proc m$print_i128(int64 a,ichar fmtstyle=nil)=
!	puts("<128>")
!!	nextfmtchars()
!!	printf("%lld",a)
!!	needgap:=1
!end

global proc m$print_i64_nf(int64 a)=
!puts("PRINTI64_nf")
	nextfmtchars()
	printf("%lld",a)
	needgap:=1
end

!global proc m$print_u64(word64 a,ichar fmtstyle=nil)=
!	nextfmtchars()
!	printf("%llu",a)
!	needgap:=1
!end
!
global proc m$print_r64(real x,ichar fmtstyle=nil)=
	nextfmtchars()
	printf("%f",x)
	needgap:=1
end

!global proc m$print_c8(int64 a,ichar fmtstyle=nil)=
!	nextfmtchars()
!	printf("%c",a)
!	needgap:=1
!end
!
!global proc m$print_str(ichar s, fmtstyle=nil)=
!	nextfmtchars()
!	printf("%s",s)
!	needgap:=1
!end

global proc m$print_str_nf(ichar s)=
	nextfmtchars()
	printf("%s",s)
	needgap:=1
end

global proc m$print_space=
	needgap:=0
	printf(" ")
end

global proc m$print_newline=
	needgap:=0
	printf("\n")
end

global proc m$unimpl=
	puts("Sysfn unimpl")
	stop 1
end

!global proc m$print_nogap=
!	needgap:=0
!end
!
!global proc nextfmtchars(int lastx=0)=
global proc nextfmtchars=
	if needgap then
		printf(" ")
		needgap:=0
	fi
end

!global proc m$stop(int stopcode)=
!	`exit(stopcode)
!end
!
!global func strint(int64 a, ichar fmtstyle=nil)ichar=
!	return "?"
!end
!
export proc billy=
	CPL "BILLY"
end
=== msyswin.m 0 0 56/74 ===
module msys
module mlib
module mclib
module mwindows
module mwindll

!proc start=
!	CPL "MSYSWIN/START"
!END
=== mwindll.m 0 0 57/74 ===
[]byte $$binary =	(
	0x55, 0x48, 0x8B, 0xEC, 0x48, 0x83, 0xEC, 0x20, 0x48, 0x89, 0x4D, 0x10, 
	0x48, 0x89, 0x55, 0x18, 0x4C, 0x89, 0x45, 0x20, 0x4C, 0x89, 0x4D, 0x28, 
	0x33, 0xC0, 0x48, 0x89, 0x45, 0xF0, 0x8B, 0x45, 0x20, 0x83, 0xE0, 0x01, 
	0x48, 0x85, 0xC0, 0x74, 0x0B, 0xB8, 0x08, 0x00, 0x00, 0x00, 0x48, 0x89, 
	0x45, 0xF0, 0x6A, 0x00, 0x48, 0x8B, 0x45, 0x20, 0x48, 0xC1, 0xE0, 0x03, 
	0x48, 0x01, 0x45, 0xF0, 0x48, 0x8B, 0x45, 0x20, 0x48, 0x89, 0x45, 0xE8, 
	0x48, 0x8B, 0x45, 0xE8, 0x48, 0x83, 0xF8, 0x01, 0x0F, 0x8C, 0x25, 0x00, 
	0x00, 0x00, 0x48, 0x8B, 0x45, 0x18, 0x4C, 0x8B, 0x55, 0xE8, 0x4A, 0x8B, 
	0x44, 0xD0, 0xF8, 0x48, 0x89, 0x45, 0xF8, 0xFF, 0x75, 0xF8, 0x48, 0x8B, 
	0x45, 0xE8, 0x48, 0xFF, 0xC8, 0x48, 0x89, 0x45, 0xE8, 0x48, 0x83, 0xF8, 
	0x01, 0x7D, 0xDB, 0x48, 0x8B, 0x0C, 0x24, 0xF3, 0x0F, 0x7E, 0x04, 0x24, 
	0x48, 0x8B, 0x54, 0x24, 0x08, 0xF3, 0x0F, 0x7E, 0x4C, 0x24, 0x08, 0x4C, 
	0x8B, 0x44, 0x24, 0x10, 0xF3, 0x0F, 0x7E, 0x54, 0x24, 0x10, 0x4C, 0x8B, 
	0x4C, 0x24, 0x18, 0xF3, 0x0F, 0x7E, 0x5C, 0x24, 0x18, 0x48, 0x8B, 0x45, 
	0x10, 0x48, 0xFF, 0xD0, 0x48, 0x03, 0x65, 0xF0, 0x44, 0x8A, 0x55, 0x28, 
	0x45, 0x22, 0xD2, 0x74, 0x05, 0x66, 0x48, 0x0F, 0x7E, 0xC0, 0x48, 0x89, 
	0x45, 0xF8, 0x48, 0x8B, 0x45, 0xF8, 0x48, 0x83, 0xC4, 0x20, 0x5D, 0xC3
	)


ref func (ref proc, ref[]i64, int, int)u64 icalldll


proc start=
	icalldll:=cast(os_allocexecmem($$binary.bytes))
	memcpy(icalldll, &$$binary, $$binary.bytes)
end

export function os_calldllfunction(
	ref proc fnaddr,
	int retcode, nargs,
	ref[]i64 args,
	ref[]byte argcodes)u64 =
	u64 a

	a:=icalldll^(fnaddr, cast(args), nargs, retcode='R')
end	
=== mwindows.m 0 0 58/74 ===
const wm_destroy=2

export type wt_word	= u16
export type wt_wordpm	= u32
export type wt_bool	= u32
export type wt_dword	= u32
export type wt_wchar	= u16
export type wt_wcharpm	= u32
export type wt_char	= byte
export type wt_ichar	= ref char
export type wt_ptr		= ref void
export type wt_wndproc	= ref proc
export type wt_handle	= ref void
export type wt_int		= i32
export type wt_uint	= u32
export type wt_long	= i32
export type wt_wparam	= word
export type wt_lparam	= word
export type wt_point	= rpoint

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
!	func "VirtualAlloc"(wt_ptr, dint,wt_dword,wt_dword)wt_ptr
	func "GetStdHandle"(wt_dword)wt_handle
	func "GetConsoleScreenBufferInfo"(wt_handle,wt_ptr)int
	func "SetConsoleCtrlHandler"(wt_wndproc,int)int
	func "SetConsoleMode"(wt_handle,wt_dword)int
	func "CreateProcessA"(wt_ichar,wt_ichar,wt_ptr,wt_ptr, int,
						wt_dword, wt_ptr,wt_ichar,wt_ptr,wt_ptr)int
	func "GetLastError":wt_dword
	func "WaitForSingleObject"(wt_handle,wt_dword)wt_dword
	func "GetExitCodeProcess"(wt_handle,wt_ptr)int
	func "CloseHandle"(wt_handle)int
	func "GetNumberOfConsoleInputEvents"(wt_handle,wt_ptr)int
	func "FlushConsoleInputBuffer"(wt_handle)int
	func "LoadLibraryA"(wt_ichar)wt_handle
!	func "GetProcAddress"(wt_handle,wt_ichar)wt_wndproc
	func "GetProcAddress"(wt_handle,wt_ichar)ref void
	func "LoadCursorA"(wt_handle,wt_ichar)wt_handle
	func "RegisterClassExA"(wt_ptr)wt_wordpm
	func "DefWindowProcA"(wt_handle,wt_uint,wt_wparam,wt_lparam)int
	func "ReadConsoleInputA"(wt_handle,wt_ptr,wt_dword,wt_ptr)int
	proc "Sleep"(wt_dword)
	func "GetModuleFileNameA"(wt_handle,wt_ichar,wt_dword)wt_dword

	proc "ExitProcess"(wt_uint)
	proc "PostQuitMessage"(wt_int)

	proc "MessageBoxA"(wt_int x=0,wt_ichar message, caption="Caption",wt_int y=0)

	func "QueryPerformanceCounter"(ref i64)wt_bool
	func "QueryPerformanceFrequency"(ref i64)wt_bool

	func "CreateFileA"(wt_ichar,wt_dword,wt_dword,wt_ptr,wt_dword,wt_dword,wt_handle)wt_handle
	func "GetFileTime"(wt_handle,wt_ptr,wt_ptr,wt_ptr)wt_bool

	proc "GetSystemTime"(ref rsystemtime)
	proc "GetLocalTime"(ref rsystemtime)

	func "GetTickCount64":u64
	func "PeekMessageA"		(ref void, ref wt_handle, wt_uint,wt_uint,wt_uint)wt_bool

	func "GetCommandLineA":ichar

	func "VirtualAlloc" (ref void, wt_dword, wt_dword, wt_dword)ref void
	func "VirtualProtect" (ref void, wt_dword, wt_dword, ref wt_dword)wt_bool

	func "WriteConsoleA" (ref void, ref void, i32, ref i32, ref void)wt_bool

	func "FindFirstFileA" (wt_ichar,ref rfinddata)wt_handle
	func "FindNextFileA"  (wt_handle, ref rfinddata)wt_bool
	func "FindClose"      (wt_handle)wt_bool

	func "MessageBeep"    (i32)wt_bool
	func "Beep"    (i32 freq, dur)wt_bool
end

record input_record = $caligned
	wt_word	eventtype
!	u16	padding
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

record rspoint=(i16 x,y)

record rsrect=
	i16 leftx,top,rightx,bottom
end

global record rpoint =
	wt_long x,y
end

record rconsole=
	rspoint size,pos
	u16 attributes
	rsrect window
	rspoint maxwindowsize
end

record rstartupinfo =
	wt_dword	size
	u32 dummy1
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
	u32 dummy2
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

EXPORT record rmsg =
	wt_handle	hwnd
	wt_uint		message
	u32		dummy1
	wt_wparam	wParam
	wt_lparam	lParam
	wt_dword	time
	u32		dummy2
	wt_point	pt
end

record rfiletime =
	wt_dword lowdatetime
	wt_dword highdatetime
end

record rfinddata =
	wt_dword	fileattributes
	rfiletime	creationtime
	rfiletime	lastaccesstime
	rfiletime	lastwritetime
	wt_dword	filesizehigh
	wt_dword	filesizelow
	wt_dword	reserved0
	wt_dword	reserved1
	[260]char	filename
	[14]char		altfilename
	wt_dword	obs1, obs2
	wt_word		obs3
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


export wt_handle hconsole, hconsolein

input_record lastkey, pendkey
int keypending			!whether pendkey contains a new key event detected by flushkbd

int hpfreq				!counts per msec


ref func (ref void)int wndproc_callbackfn=nil	!windows call-back: address of handler

int init_flag=0

export proc os_init=
!general initialisation
	hconsole:=GetStdHandle(u32(-11))
	hconsolein:=GetStdHandle(u32(-10))

	lastkey.repeatcount:=0
	keypending:=0

	SetConsoleCtrlHandler(nil,1)

	SetConsoleMode(hconsole,1 ior 2)

	init_flag:=1

end

export func os_execwait(ichar cmdline,int newconsole=0,ichar workdir=nil)int =
	wt_dword exitcode
	int status
	int cflags:=0

	rstartupinfo si
	rprocess_information xpi

	clear si
	clear xpi

	case newconsole
	when 0 then cflags := NORMAL_PRIORITY_CLASS
	when 1 then cflags := NORMAL_PRIORITY_CLASS ior CREATE_NEW_CONSOLE
	when 2 then cflags := NORMAL_PRIORITY_CLASS ior DETACHED_PROCESS
	esac

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
		printf("Winexec error: %lld\n",status)
		return -1
	fi

	WaitForSingleObject(xpi.process, 0xFFFF'FFFF)
	GetExitCodeProcess(xpi.process,&exitcode)

	CloseHandle(xpi.process)
	CloseHandle(xpi.thread)

	return exitcode
end

export func os_execcmd(ichar cmdline, int newconsole=0)int =
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

export func os_getch:int=
	int k

	k:=os_getchx() iand 255

	return k
end

export func os_kbhit:int=
	wt_dword count

	unless init_flag then os_init() end

	GetNumberOfConsoleInputEvents(hconsolein,&count)
	return count>1
end

export func os_getdllinst(ichar name)u64=
	wt_handle hinst

	hinst:=LoadLibraryA(name)
	return cast(hinst)
end

export func os_getdllprocaddr(int hinst,ichar name)ref void=
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
	r.style:=8 ior 32
	r.wndproc:=cast(&mainwndproc)
	r.instance:=nil

	r.icon:=nil
	r.cursor:=LoadCursorA(nil,ref void(idcarrow))
	r.background:=cast(15+1)
	r.menuname:=nil
	r.classname:=classname
	r.iconsm:=nil

	if RegisterClassExA(&r)=0 then
		printf("Regclass error: %lld %lld\n",classname,GetLastError())
		stop 1
	end
	registered:=1
end

!EXPORT REF RMSG MADDR

global function mainwndproc (
		wt_handle hwnd, wt_uint message, wt_wparam wParam, wt_lparam lParam)int=
	rmsg m
	int result
	static int count=0

!CPL "MAINWNDPROC",HWND

	m.hwnd:=hwnd
	m.message:=message
	m.wParam:=wParam
	m.lParam:=lParam
	m.pt.x:=0
	m.pt.y:=0
!MADDR:=&M
!CPL "MAINWNDPROC", =&M, =M.HWND
	
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

export proc os_setmesshandler(ref void addr)=
	wndproc_callbackfn:=addr
end

export func os_getchx:int=
!Q! function  os_getchx_c:int
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

export func os_getos=>ichar=
	return "W64"
end

export func os_gethostsize=>int=
	return 64
end

export func os_shellexec(ichar opc, file)int=
	return system(file)
end

export proc os_sleep(int a)=
	Sleep(a)
end

export func os_getstdin:filehandle =
	return fopen("con","rb")
end

export func os_getstdout:filehandle =
	return fopen("con","wb")
end

export func os_gethostname:ichar=
	static [300]char name
	static int n

	GetModuleFileNameA(nil,name,name.bytes)
	return name
end

export func os_getmpath:ichar=
!BART
!	return "C:\\m\\"
	return F"C:@@@@\m\" !ABC
!	return "C:@@@@\\m\\" !ABC
end

export func os_clock:i64=
!	return clock()
	return os_hpcounter()
end

export func os_ticks:i64=
	return GetTickCount64()
end

export func os_iswindows:int=
	return 1
end

export proc os_getsystime(ref rsystemtime tm)=
	GetLocalTime(tm)
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

export func os_allocexecmem(int n)ref byte=
	ref byte p
	u32 oldprot
	int status

	p := VirtualAlloc(nil, n, MEM_RESERVE ior MEM_COMMIT, PAGE_NOACCESS)
	if p = nil then return nil fi

	status := VirtualProtect(p, n, PAGE_EXECUTE_READWRITE, &oldprot)
	if status = 0 then return nil fi

	return p
end

export func dirlist(ichar filespec, ref[]ichar dest, int capacity, t=1)int=
!filespec is a filename (eg. "*.dwg") with possible drive/path; scan
!directory for all matching files:
! Store each file in dest array up to capacity
! Return:
!  -1:	capacity exceeded
!   N:  number of files found including 0 for no matching files

!t has this value
! +1  Include normal files only, no sub-directory names
! +2  Include directories
! +3  (+1 +2) Include all files including directories
! +4  Convert to lower case
	ref void hfind
	rfinddata file
	int nfiles:=0
	[300]char path
	[300]char fullfilename

	strcpy(path, extractpath(filespec))


	if (hfind:=findfirstfilea(filespec,&file))<>ref void(-1) then	!at least one file
		repeat
			if (file.fileattributes iand 16) then		!this is a directory
				if (t iand 2)=0 then nextloop fi		!no directories
			else						!this is a file
				if (t iand 1)=0 then nextloop fi
			fi
			if nfiles>=capacity then
				nfiles:=-1
				exit
			fi

			if (t iand 4) then				!to lower case
				convlcstring(file.filename)
!				convlcstring(file.filename)
			fi
			strcpy(fullfilename, path)
			strcat(fullfilename, file.filename)

			dest[++nfiles]:=pcm_copyheapstring(fullfilename)

		until not findnextfilea(hfind,&file)
		findclose(hfind)
	fi
	return nfiles
end

export func os_hpcounter:int a =
!return counter such that successive calls indicate duration in msec

	if hpfreq=0 then
		hpfreq:=os_hpfreq()/1000		!counts per msec
	fi

	QueryPerformanceCounter(&a)
	a/hpfreq
end

export func os_hpfreq:int a =
	QueryPerformanceFrequency(&a)
	a
end

=== pcl_api.m 0 0 59/74 ===
!EXPORT INT PCLSEQNO
int STSEQNO

global pcl pcstart			!start of pcl block
global pcl pccurr			!point to current pcl op
global pcl pcend			!point to last allocated pclrec

!byte pcfixed				!whether code is fixed up
global int pclength

global pcl pcmin, pcmax		!limits of pcl code generated for program

int initpcalloc=65536

MACRO ADDSPEC = if opcode>klast then ++NSPECIAL fi

const maxextnames=100
[maxextnames]ichar extnames
[maxextnames]psymbol extsymbols
int nextnames

global proc pgen(int opcode, pcl p=nil) =
	p:=newpcl(p, opcode)
end

global proc pgenx(int opcode, int x, pcl p=nil) =
	p:=newpcl(p, opcode)
	pccurr.x:=x
end

global func newpcl(pcl p, int opcode):pcl =

	if p=nil then
		p:=pcm_allocnfz(pclrec.bytes)
	fi
	p.opcode:=opcode

	if pclhastype[opcode] and opcode<>kdata then		!pcgendata can set block type
		p.mode:=ti64
		p.size:=8
	fi

	pccurr.next:=p
	pccurr:=p

	++pclength

	return pccurr
end

global func newopnd:pcl p =
	pcm_allocnfz(pclrec.bytes)
end

global proc pcl_start =
!reset tcstart/tccurr for new TCL sequence (new proc or new init data)
	pcstart:=pcm_allocnfz(pclrec.bytes)
	pcstart.opcode:=knop
	pccurr:=pcstart
!	pcseqno:=0
	pclength:=0

	pcmin min:=pcstart

end

global func pcl_end:pcl pc=
!Terminate sequence; sets pcstart to nil so that pcl cannot be generated
!outside proc bodies etc
!But caller should copy tcstart value, or use its value returned here

	pcmax max:=pccurr
	pc:=pcstart
	if pc.opcode=knop then
		pc.next
	else
		pc
	end
end

global proc psetmode(int m, size=0)=
	pccurr.mode:=m
	if size=0 then size:=pstdsize[m] fi
	pccurr.size:=size

	if pclhastype[pccurr.opcode]=2 then
		pccurr.mode2:=pccurr.mode
	end
end

global proc psetmode2(int m)=
	pccurr.mode2:=m
end

global proc pcomment(ichar s)=
!*!	return when fregoptim or fpeephole		!will get skipped anyway
!	RETURN WHEN DOREDUCE			!comments suppressed as they get in the way
!STATIC INT CCC
!CPL "COMMENT",++CCC
	pgen(kcomment, pgenstring(s))
end

global proc psetnargs(int n)=
	pccurr.nargs:=n
end

global proc pgenix(int opcode, scale=1, offset=0) =
!originally intended for combinations of ptr ops to be combined into
!previous ones, but that has now been dropped.
!Instead any such reductions will be done in a separate pass, much simpler
	pcl p:=newpcl(nil, opcode)
	p.scale:=scale
	p.extra:=offset
end

global proc pgencond(int opcode, cond, pcl p=nil) =
	p:=newpcl(p, opcode)
	p.condcode:=cond
end

global proc pgenxy(int opcode, int x, y, pcl p=nil) =
	p:=newpcl(p, opcode)
	p.x:=x
	p.y:=y
end

global proc psetscaleoff(int scale, offset:=0)=
	pccurr.scale:=scale
	pccurr.extra:=offset
end

global func pgenint(int a)pcl p=
	p:=newopnd()
	p.value:=a
	p.opndtype:=int_opnd
	return p
end

global func pgenrealmem(real x)pcl p=
	p:=newopnd()
	p.xvalue:=x
	p.opndtype:=real_opnd
	return p
end

!global func pgenrealimm(real x)pcl p=
!	p:=newpcl()
!	p.xvalue:=x
!	p.opndtype:=realimm_opnd
!	return p
!end

global func pgenstring(ichar s, int length=-1)pcl p=
	p:=newopnd()

	if length<0 then
		length:=strlen(s)+1
	end

	p.svalue:=pcm_copyheapstringn(s,length)		!will add an extra zero

	p.slength:=length
	p.opndtype:=string_opnd
	return p
end

global func pgenlabel(int a)pcl p=
	p:=newopnd()
	p.labelno:=a

	p.opndtype:=label_opnd
	return p
end

global func pgenmem(psymbol d)pcl p=

	p:=newopnd()

	p.def:=d

	p.opndtype:=mem_opnd
	return p
end

global func pgenmemaddr(psymbol d)pcl p=
	p:=newopnd()
	p.def:=d
	p.opndtype:=memaddr_opnd
	return p
end

global func pgenname(ichar s)pcl=
	return pgenmem(pmakesymbol(s))
end

!global func pgenextname(ichar s)pcl=
!	psymbol d
!
!	d:=addextname(s)
!
!!	return pgenmemaddr(pmakesymbol(s))
!	return pgenmemaddr(d)
!end
!
global func pmakeextname(ichar s)psymbol d=
	for i to nextnames do
		if eqstring(extnames[i], s) then
			return extsymbols[i]
		end
	end

	if nextnames >= extnames.upb then
		gerror("Too many extnames")
	end

	++nextnames
	extnames[nextnames]:=pcm_copyheapstring(s)

	d:=pmakesymbol(s)
	paddsymbol(d)

	d.nameid:=dllprocid
	d.isimport:=1
	extsymbols[nextnames]:=d
	return d
end


!global func pgennameaddr(ichar s)pcl=
!	return pgenmemaddr(pmakesymbol(s))
!end

global func addstr(ichar s, t)ichar=
	static [256]char str
	strcpy(str, s)
	strcat(str, t)
	str
end

global func newpstrec:psymbol p=

	p:=pcm_allocnfz(pstrec.bytes)
!
!	p.pos:=lx.pos
!	p.moduleno:=currmoduleno
!	p.subprogno:=moduletosub[currmoduleno]
	return p
end

global func pmakesymbol(ichar s, int id=nullid)psymbol d=
	d:=newpstrec()
	d.name:=pcm_copyheapstring(s)

	d.nameid:=id

	d.seqno:=++stseqno

!	if id=staticid then
!		paddsymbol(d)
!	end
	d
end

global func createfwdlabel:int =
	return ++mlabelno
end

global func definelabel:int =
	pgen(klabel, pgenlabel(++mlabelno))
	return mlabelno
end

global proc definefwdlabel(int lab) =
	pgen(klabel, pgenlabel(lab))
end

global proc paddsymbol(psymbol d)=
!CPL "ADDSYMBOL", D.NAME
	if psymboltable=nil then
		psymboltable:=psymboltablex:=d
	else
		psymboltablex.next:=d
		psymboltablex:=d
	fi
end

global func getfullname(psymbol d, int backtick=0)ichar=
!create fully qualified name into caller's dest buffer
	static [256]char str

	str[1]:=0
	if backtick then
		strcpy(str, "`")
	end

	if d.isimport then
		if backtick then
			strcat(str, d.name)
			strcat(str, "*")
		else
			strcat(str, d.name)
		end
		return str
	end

	if d.nameid in [frameid, paramid] then
		strcat(str, currfunc.name)
		strcat(str, ".")
	fi

	strcat(str, d.name)
end

export func pgendata(ref byte s, int length)pcl p=
	p:=newopnd()
	p.svalue:=s			! assume already saved on heap
	p.opndtype:=data_opnd
	p.mode:=tblock
	p.size:=length

	return p
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

global proc setfunctab=
	if pnprocs=nil then
		pnprocs:=pmakesymbol("$nprocs", staticid)
!CPL "SET PNPROCS", PNPROCS
		pnprocs.mode:=ti64
		pprocname:=pmakesymbol("$procname", staticid)
		pprocaddr:=pmakesymbol("$procaddr", staticid)

		paddsymbol(pnprocs)
		paddsymbol(pprocname)
		paddsymbol(pprocaddr)

	end
end

global proc dofunctables=
	const maxprocs=1000
	[maxprocs]psymbol proctable
	int nprocs:=0
	psymbol d

	d:=psymboltable
	while d, d:=d.next do
		if d.nameid=procid and d.ishandler then
			if nprocs>=maxprocs then gerror("PCL proctab overflow") fi
			proctable[++nprocs]:=d
		fi
	end

	if nprocs=0 and pnprocs=nil then
		pnprocs:=pmakesymbol("$nprocs", staticid)
		pnprocs.mode:=ti64
		goto finish
	fi

	setfunctab()

!CPL "SCANP", =PNPROCS, =PPROCADDR

!	pgen(kistatic, genmem(pprocaddr))
	pprocaddr.mode:=tblock
	pprocaddr.size:=nprocs*8

	pcl_start()
	for i to nprocs do
		pgen(kdata, pgenmemaddr(proctable[i]))
		psetmode(tu64)
	end
	pprocaddr.pccode:=pcl_end()

	pprocname.mode:=tblock
	pprocname.size:=nprocs*8

	pcl_start()
	for i to nprocs do
		pgen(kdata, pgenstring(getbasename(proctable[i].name)))
		psetmode(tu64)
	end
	pprocname.pccode:=pcl_end()

finish:
!	pgen(kistatic, genmem(pnprocs))
!	psetmode(ti64)
	pcl_start()
	pgen(kdata, pgenint(nprocs))
	psetmode(ti64)
	pnprocs.pccode:=pcl_end()
end

global func getbasename(ichar s)ichar t=
	t:=s+strlen(s)-1
	while t>s and (t-1)^<>'.' do
		--t
	end

	return t
end

export proc pc_addplib(ichar name)=
	if nplibfiles>=maxplibfile then gerror("Too many libs") fi

	plibfiles[++nplibfiles]:=pcm_copyheapstring(changeext(name, ""))
end

=== pcl_decls.m 0 0 60/74 ===
global enumdata  [0:]ichar pstdnames,
        [0:]byte pstdsize,
		[0:]byte pstdsigned,
		[0:]byte pstdint,
		[0:]byte pstdfloat,
		[0:]byte pstdwide,
		[0:]byte pstdmin =				!minimum width for int types

!Ordering must match tx-codes in front-end
!    type         name       bits     sign int flt  wide  min
    (tvoid=0,     "void",       0,    0,	0,	0,	0,	0),

    (tr64,        "r64",        8,    0,	0,	1,	1,	0),
    (tr32,        "r32",        4,    0,	0,	1,	0,	0),
    (ti64,        "i64",        8,    1,	1,	0,	0,	ti64),
    (tu64,        "u64",        8,    0,	1,	0,	0,	tu64),
    (tc64,        "c64",        8,    0,	1,	0,	0,	tu64),

    (ti8,         "i8",          1,   1,	1,	0,	0,	ti64),
    (ti16,        "i16",         2,   1,	1,	0,	0,	ti64),
    (ti32,        "i32",         4,   1,	1,	0,	0,	ti64),
    (tu8,         "u8",          1,   0,	1,	0,	0,	tu64),
    (tu16,        "u16",         2,   0,	1,	0,	0,	tu64),
    (tu32,        "u32",         4,   0,	1,	0,	0,	tu64),

    (tblock,      "block",       8,   0,	0,	0,	0,	0),
    (tvector,     "vector",      8,   0,	0,	0,	0,	0),
end

global type psymbol = ^pstrec

global record pstrec = $caligned
!global record pstrec = $caligned
!global record pstrec =
	ichar name
	psymbol next
	psymbol nextparam
	psymbol nextlocal

	union
		pcl pcdata				!for idata: point kdata sequence
		pcl pccode				!for procs: entry point to function body code
		^proc dlladdr		!for imported functions
		^void staddr			!var statics: address
		psymbol cprocowner		!C target:var statics: owner proc
	end
	^fwdrec fwdrefs			!fwd ref chain

	psymbol pdata

!	mclopnd mx

	byte nameid

	i32 offset

	byte isimport
	byte isexport				!only for proc_id/static_id
	byte mode
	byte isentry
	u32 size

	byte addrof
	byte reg
	byte atvar
	byte used
	byte cvariadic				!C variadic function

	byte reftype
	byte segment
	byte ishandler
	byte regcand
	byte xregcand
!	byte hasdot

	i16 stindex
	i16 importindex
	i16 exportindex
	i32 labelno

!	byte flags:(asmused:1, chasstatics:1, dllexport:1)

!	byte dllindex				!for dllproc: which dll in dlltable

	byte nretvalues				!function: number of return values (0 for proc)
	byte varparams				!0 or N; variadic params
	byte nparams

!	byte ismain					!1 if a proc to be part of func tables
!	byte SPARE

!----------------------------------

!	byte nparams
	byte variadic				!local function that uses ... variadic params (for C)
	i16 nlocals
	i16 impindex
	i16 expindex
	u32 seqno

end

global type pcl = ^pclrec

global record pclrec =
	pcl next

	byte opcode
	byte mode, mode2
	byte condcode						!for jumpcc/setcc

!	mclopnd a
	union
		i64		value
		r64		xvalue
		ichar	svalue			!also used for data
		int		labelno
		psymbol	def
	end

	u32 size

	union						!two 32-bit params used according to opcode
		struct
			i16 x				!common access to these 1/2 extra attribs
			i16 y
		end

		struct					! (x,y) pointer ops
			i16 scale			! scale factor for offset
			i16 extra			! extra constant byte offset, already scaled
		end

		struct					! (x,y) call/etc
			i16 nargs			! number of args
			union
				i16 nvariadics	!call: 0, or arg # that is first variadic
				i16 nrealargs	!setcall: 1 if whole call sequence is simple
			end
		end

		struct					! (x,y) switch
			i16 minlab
			i16 maxlab
		end

		struct					! defproc/retproc/retfn
			i16 paramslots		! stack usage as 8-byte slots
			i16 localslots
		end

		i32 stepx				! (x) always +ve fixed step size for forup/fordown; also INCR
		i32 align
		i32 popone				! (x) jumpcc: leave X on stack
		i32 istrunc				! (x) widen: doing the job of truncate
		i32 slicelwb			! (x) for .upb
		i32 inplace				! (x) for load, means &A operand is for inplace update
		i32 mathsop				! (x) for maths: maths_sin etc
		u32 slength				! string length: includes any terminator

	end

!	u16		lineno				!within function? Can use rel offset if func has full offset
	union
		byte	opndtype			!operand type (value, def etc)
		byte	optype				!alias used as pcl opnd
	end
	U16 SPARE
!	byte	reg						!when used as pcl opnd
!	byte	count
end

global type pclopnd = ^pcsrec

global record pcsrec =
	union
		struct
			byte mode				!mode (copy of pcl loadop to start)
			byte reg				!0: register holding operand
			byte temp				!0, or 1: pcl loadop in temp mem location
			byte code				!0, or 1: pclopnds[] element refers to PCL load instr
			byte count				!dupl count (zero to start)
!			byte regvar				!0, or 1 when reg is a regvar (copy of regvarset.[reg])

!			u32 psize				!copy of pcl load op size to start
		end
		u64 all						!use to restore whole record at once
	end
end

global record fwdrec =
	^fwdrec nextfwd
	i32 offset
	i16 reltype
	i16 seg
end

global enumdata [0:]ichar opndnames =
	(no_opnd=0,			$),
	(reg_opnd,			$),				!in reg (regvarset shows if regvar)

	(mem_opnd,			$),
	(memaddr_opnd,		$),
	(int_opnd,			$),
	(real_opnd,			$),				!will be [label]
	(string_opnd,		$),				!will be [label]
	(label_opnd,		$),				!will be the address of the label, so lea r,[label]

!	(temp_opnd,			$),

	(data_opnd,			$),
end

global enumdata [0:]ichar pclnames,
!enumdata [0:]ichar pclnames,
				[0:]byte pclhastype,			!where t or t/u are used (0/1/2)
				[0:]byte pclextra,				!uses .x or .x/.y
				[0:]byte pclargs =				!no. of call-args, incl ops that may use calls (9 = special)

!                       t  x          (attrs     )
	(knop=0,       $+1, 0, 0,  0),  ! (          ) 

	(kload,        $+1, 1, 1,  0),  ! (M L t i   ) Z' := M &M L &L 123 4.5 "abc"; i=1 for in-place ref
	(kiload,       $+1, 1, 0,  0),  ! (t         ) Z' := Z^
	(kiloadx,      $+1, 1, 2,  0),  ! (t d       ) Z' := (Y + Z*s + d)^

	(kstore,       $+1, 1, 0,  0),  ! (M t       ) M := Z
	(kistore,      $+1, 1, 0,  0),  ! (t         ) Z^ := Y
	(kistorex,     $+1, 1, 2,  0),  ! (t s d     ) (Y + Z*s + d)^ := X
	(kstorem,      $+1, 1, 0,  0),  ! (t         ) Z' :=(Y, Z) for mem:16
	(kiswap,       $+1, 1, 0,  0),  ! (t         ) swap(Y^,Z^)
	(kclear,       $+1, 1, 0,  0),  ! (t         ) Clear Z^

	(kcallp,       $+1, 0, 2,  9),  ! (M n v     ) Call &M with nargs, then pop args; v = varargs
	(kicallp,      $+1, 0, 2,  9),  ! (n v       ) Call Z with nargs, then pop args (a=n+1)
	(kretproc,     $+1, 0, 0,  0),  ! (          ) Return from proc
	(kcallf,       $+1, 1, 2,  9),  ! (M t n v   ) Call &M, then pop args, leave retval; v = varrgs
	(kicallf,      $+1, 1, 2,  9),  ! (t n v     ) Call Z, then pops args, leave retval (a=n+1)
	(kretfn,       $+1, 1, 0,  0),  ! (t         ) Return from func with Z=retval

	(kjump,        $+1, 0, 0,  0),  ! (L         ) goto L
	(kijump,       $+1, 1, 0,  0),  ! (          ) goto Z
	(kjumpcc,      $+1, 1, 1,  0),  ! (L t c p   ) goto L when Y c Z; p=1: Z':=Y (b=0/1)
	(kjumpt,       $+1, 1, 0,  0),  ! (L t       ) goto L when Z is true
	(kjumpf,       $+1, 1, 0,  0),  ! (L t       ) goto L when Z is false
	(kjumpret,     $+1, 1, 0,  0),  ! (L t       ) goto 0, common return point; deal with any ret value on stack
	(kjumpretm,    $+1, 1, 0,  0),  ! (L t n     ) goto 0, common return point; deal with any ret value on stack
	(ksetcc,       $+1, 1, 0,  0),  ! (t c       ) Z' := Y cc Z

	(kadd,         $+1, 1, 0,  0),  ! (t         ) Z' := Y + Z
	(ksub,         $+1, 1, 0,  0),  ! (t         ) Z' := Y - Z
	(kmul,         $+1, 1, 0,  0),  ! (t         ) Z' := Y * Z
	(kdiv,         $+1, 1, 0,  0),  ! (t         ) Z' := Y / Z
	(kidiv,        $+1, 1, 0,  0),  ! (t         ) Z' := Y % Z
	(kirem,        $+1, 1, 0,  0),  ! (t         ) Z' := Y rem Z
	(kidivrem,     $+1, 1, 0,  0),  ! (t         ) Z' := divrem(Y, Z)
	(kbitand,      $+1, 1, 0,  0),  ! (t         ) Z' := Y iand Z
	(kbitor,       $+1, 1, 0,  0),  ! (t         ) Z' := Y ior Z
	(kbitxor,      $+1, 1, 0,  0),  ! (t         ) Z' := Y ixor Z
	(kshl,         $+1, 1, 0,  0),  ! (t         ) Z' := Y << Z
	(kshr,         $+1, 1, 0,  0),  ! (t         ) Z' := Y >> Z
	(kmin,         $+1, 1, 0,  0),  ! (t         ) Z' := min(Y, Z)
	(kmax,         $+1, 1, 0,  0),  ! (t         ) Z' := max(Y, Z)
	(kaddpx,       $+1, 1, 2,  0),  ! (t s d     ) Z' := Y + Z*s + d
	(kpower,       $+1, 1, 0,  2),  ! (t         ) Z' := Y ** Z

	(kneg,         $+1, 1, 0,  0),  ! (t         ) Z' := -Z
	(kabs,         $+1, 1, 0,  0),  ! (t         ) Z' := abs Z
	(kbitnot,      $+1, 1, 0,  0),  ! (t         ) Z' := inot Z
	(knot,         $+1, 1, 0,  0),  ! (t         ) Z' := not Z
	(ktoboolt,     $+1, 2, 0,  0),  ! (t u       ) Z' := istrue Z; u is of type u; result is type t
	(ksqrt,        $+1, 1, 0,  0),  ! (t         ) Z' := sqrt Z
	(kmaths,       $+1, 1, 1,  1),  ! (t op      ) Z' := maths(Z)
	(kmaths2,      $+1, 1, 1,  2),  ! (t op      ) Z' := maths2(Y, Z)
	(ktoboolf,     $+1, 2, 0,  0),  ! (t u       ) Z' := not istrue Z

	(kincrload,    $+1, 1, 1,  0),  ! (t n       ) Z' := (Z +:= n)^
	(kdecrload,    $+1, 1, 1,  0),  ! (t n       ) Z' := (Z -:= n)^
	(kloadincr,    $+1, 1, 1,  0),  ! (t n       ) Z' := Z++^ (difficult to express step)
	(kloaddecr,    $+1, 1, 1,  0),  ! (t n       ) Z' := Z--^

	(ktypepun,     $+1, 2, 0,  0),  ! (t u       ) Z' := t(u@(Z^))
	(kfloat,       $+1, 2, 0,  0),  ! (t u       ) Z' := cast(Z,t) Int   to real t
	(kfix,         $+1, 2, 0,  0),  ! (t u       ) Z' := cast(Z,t) Real   to int t
	(kwiden,       $+1, 2, 0,  0),  ! (t u       ) Z' := cast(Z,t) Mask to width of u, but type is widened to t
	(kfwiden,      $+1, 2, 0,  0),  ! (t u       ) Z' := cast(Z,t) r32 to r64
	(kfnarrow,     $+1, 2, 0,  0),  ! (t u       ) Z' := cast(Z,t) r64 to r32

!Codes after this point are mostly directives (hints and pcl stack manipulation,
! labels, comments etc) rather than code-generating ops.
!'data' is mostly for data init, but is used also for switch jump-labels

	(kdata, 	   $+1, 1, 2,  0),  ! (          ) Mainly or IDATA; also for switch labels
	(kdupl,        $+1, 0, 0,  0),  ! (          ) Z' := Y' := Z
	(kdouble,      $+1, 0, 0,  0),  ! (          ) Count extra instance of Z
	(kswapstk,     $+1, 0, 2,  0),  ! (a b       ) Swap(stack(a, 0,  0), stack(b)); 1/2/3/4 = Z/Y/X/W
	(kunload,      $+1, 1, 0,  0),  ! (t         ) Pop stack

	(kstartmx,     $+1, 0, 0,  0),  ! (          ) -
	(kresetmx,     $+1, 1, 0,  0),  ! (t         ) -
	(kendmx,       $+1, 1, 0,  0),  ! (t         ) -

	(klabel,       $+1, 0, 0,  0),  ! (          ) ?


	(ksetcall,     $+1, 0, 1,  0),  ! (n s       ) n=args, s=1 for simple call
	(ksetarg,      $+1, 0, 2,  0),  ! (n1 n2     ) n1=arg no (LTR) n2=int or real arg no (maybe neg for real)

	(kloadall,     $+1, 0, 0,  0),  ! (          ) Used for multi-assignment to force loads of pending items

	(keval,        $+1, 0, 0,  0),  ! (          ) Evaluate Z [load to an actual register], then pop
	(kcomment,     $+1, 0, 0,  0),  ! (C         ) Comment C (a string)

!The codes after this point are used internally, but do not form part
!of the generated IL so no backend support is needed.
!('ktype' may be check by the backend when dealing with multiple returns)

	(ksubpx,       $+1, 1, 2,  0),  ! (t s d     ) Z' := Y - Z*s + s
	(ksqr,         $+1, 1, 0,  0),  ! (t         ) Z' := sqr Z

	(ksign,        $+1, 1, 0,  1),  ! (t         ) Z' := sign Z

	(kincrto,      $+1, 1, 1,  0),  ! (t n       ) Z^ +:= n
	(kdecrto,      $+1, 1, 1,  0),  ! (t n       ) Z^ -:= n

	(ktype,        $+1, 1, 0,  0),  ! (t         ) Define auxiliary type t

end

global const kerror = knop

global enumdata [0:]ichar ccnames, [0:]ichar ccshortnames =
	(no_cc=0,	"xx",	"?"),
	(eq_cc,		"eq",	" = "),
	(ne_cc,		"ne",	" <> "),
	(lt_cc,		"lt",	" < "),
	(le_cc,		"le",	" <= "),
	(ge_cc,		"ge",	" >= "),
	(gt_cc,		"gt",	" > "),
end

global enumdata [0:]ichar mathsnames =
	(none_m = 0,		$),
	(maths_sin,			$+6),
	(maths_cos,			$+6),
	(maths_tan,			$+6),
	(maths_asin,		$+6),
	(maths_acos,		$+6),
	(maths_atan,		$+6),
	(maths_log,			$+6),
	(maths_log10,		$+6),
	(maths_exp,			$+6),
	(maths_round,		$+6),
	(maths_ceil,		$+6),
	(maths_floor,		$+6),
	(maths_fract,		$+6),
	(maths_sign,		$+6),
	(maths_fmod,		$+6),
	(maths_atan2,		$+6),
end

global const maxplibfile=50
global [maxplibfile]ichar plibfiles
global [maxplibfile]u64 plibinst
global int nplibfiles

global psymbol entryproc		!entry point function

global psymbol currfunc
!global int retindex
!
!global const maxnestedloops	= 50
!
!global [maxnestedloops,4]int loopstack
!global int loopindex							!current level of nested loop/switch blocks
!
!unitrec zero_unit
!global unit pzero=&zero_unit

!proc start=
!	zero_unit.tag:=jconst
!	zero_unit.mode:=ti64
!	zero_unit.value:=0
!	zero_unit.resultflag:=1
!end

!global byte ssdone
!
!global int assemtype
!

global psymbol psymboltable, psymboltablex

global const pmaxparams = 30

global int pcmdskip
global psymbol pnprocs, pprocname, pprocaddr
=== pcl_diags.m 0 0 61/74 ===
!const fshowpclseqno=1
const fshowpclseqno=0

strbuffer sbuffer
^strbuffer dest=&sbuffer
int destlinestart

const tab1="    "
const tab2="        "

ref[]byte labeltab

int pclseqno

global ichar longstring					!used in stropnd
global int longstringlen

global proc strpcl(pcl p)=
	[256]char str
	int opcode, n,x,y
	psymbol d, e

	const showformatted=1

	opcode:=p.opcode

!	if fshowpclseqno then
!		psstr(string(pclseqno, "z5"))
!		psstr("  ")
!	end

!CPL "STRPCL", PCLNAMES[P.OPCODE]

!PSSTR("<PCL>")
!PSSTR(STRINT(INT(P),"h"))
!PSSTR( " ")

!PSSTR(OPNDNAMES[P.OPNDTYPE])

!psstr(strint(getlineno(p.pos),"4"))
!psstr(" ")

	case opcode
	when klabel then
		strlabel(p.labelno,1)

		IF P.POPONE THEN
			PSSTR(" NOT USED")
		FI

		return
!	when klabeldef then
!		psstr("! ")
!		psstr(p.def.name)
!		psstr(":")
!		return
	when kcomment then
		if p.svalue^ then
			psstr("!")
			psstr(p.svalue)
		ELSE
			PSSTR("! - - -")
		end
		return

	end case

	psstr(tab1)
skiptab:


	case opcode
	when kjumpcc then
		strcpy(str, "jump")
		strcat(str, ccnames[p.condcode])
!		if p.popone then
!			strcat(str, "/1")
!		end
!	when ksetcc then
!		strcpy(str, "set")
!		strcat(str, ccnames[p.condcode])
	when kmaths, kmaths2 then
!		strcpy(str, "maths.")
		strcpy(str, "m.")
		strcat(str, mathsnames[p.mathsop])
	else
		strcpy(str, pclnames[opcode])
	end case

	gs_leftstr(dest,str,9)

	if p.opndtype then
		psstr(" ")
		psopnd(p)
	end
	pstabto(40)
	psstr(" ")
!	strcat(str, " ")

	str[1]:=0
	if p.mode then

		strcat(str, strpmode(p.mode, p.size))

		if pclhastype[opcode]=2 then
			strcat(str, "/")
			strcat(str, strmode(p.mode2))
		end
		STRCAT(STR, " ")
	end
	gs_leftstr(dest,str,4)

	str[1]:=0
	n:=pclextra[opcode]
	if n then
		x:=p.x; y:=p.y
		if x or n=2 then			!don't show single 0 value
			strcat(str, "/")
			strcat(str, strint(p.x))
		end

		if n=2 and y then
			strcat(str, "/")
			strcat(str, strint(y))
		end
		STRCAT(STR, " ")
	fi	
	gs_leftstr(dest,str,5)

!	if p.opndtype then
!		psstr(" ")
!		psopnd(p)
!	end
!	pstabto(40)

	if fshowpclseqno then
		psstr("! ")
		psstr(strint(pclseqno,"z5"))
		psstr("  ")
	end



end

global func strpclstr(pcl p, int buffsize)ichar=
	gs_free(dest)
	gs_init(dest)
	destlinestart:=0
	strpcl(p)
	gs_char(dest,0)

	if dest.length>=buffsize then return "<BIGSTR>" end

	dest.strptr
end

global func writeallpcl:^strbuffer=
!write all pcl code in system by scanning all procs
!pcl code is only stored per-proc
	psymbol d

	labeltab:=pcm_allocz(mlabelno)			!indexed 1..labelno

	gs_init(dest)
	destlinestart:=dest.length

	gs_strln(dest, "!PROC PCL")

!CPL "WRITEALL", =PROCLIST
!	PC_COMMENT("HELLO")
!	PC_COMMENT("HELLO")
!	PC_COMMENT("HELLO")
!
	d:=psymboltable

	while d, d:=d.next do
		if d.nameid=procid and not d.isimport then
			currfunc:=d

			psprocsig(d)
			psprocbody(d)

			psstrline("End")
			psline()
		end
	end

	pcm_free(labeltab, mlabelno)

	return dest
end

proc psstr(ichar s)=
	gs_str(dest,s)
end

proc psstrline(ichar s)=
	gs_str(dest,s)
	gs_line(dest)
end

proc psline=
	gs_line(dest)
end

proc psint(int a)=
	gs_str(dest,strint(a))
end

proc psname(psymbol d)=
	gs_str(dest,getfullname(d))
end

proc psprocsig(psymbol d)=
	psymbol e
	byte comma:=0
	int lastmode:=tvoid, m, lastsize:=0, size

	psstr("Proc ")
	psstr(d.name)
	psstr("(")

	e:=d.nextparam

	while e, e:=e.nextparam do
		if comma then psstr(", ") end
		if e.mode<>lastmode then
			lastmode:=e.mode
			lastsize:=e.size
			psstr(strpmode(lastmode, lastsize))
			psstr(" ")
		end
		psstr(e.name)

		comma:=1
	end
	psstr(")")

	if d.mode then
		psstr(strpmode(d.mode, d.size))
	end
	psstrline(" =")


	e:=d.nextlocal
	comma:=0
	while e, e:=e.nextlocal do
!		if e.nameid in [frameid, staticid] then
		psstr(tab1)
		psstr(strpmode(e.mode, e.size))
		psstr(" ")
		psstrline(e.name)
		comma:=1
	end
	if comma then psline() end
end

proc psprocbody(psymbol d)=
	pcl p

	p:=d.pccode
	return unless p

!do first pass populating label table

	while p, p:=p.next do
		if p.opcode<>klabel then
			if p.opndtype=label_opnd then
				labeltab[p.labelno]:=1
			end
		end
	end

	p:=d.pccode
	destlinestart:=dest.length
	pclseqno:=0

	while p, p:=p.next do
		++pclseqno

!UNLESS P.OPCODE IN [KSETCALL, KSETARG] THEN
		strpcl(p)
		gs_line(dest)
!END
		destlinestart:=dest.length
	end

	psline()
end

global proc strlabel(int labelno,colon=0)=
	psstr("L")
	psint(labelno)
	if colon then
		psstr(":")
	end
	psstr(" ")
end

global proc pstabto(int n)=
	int col:=dest.length-destlinestart
	while n>col do psstr(" "); ++col end
end

proc psopnd(pcl p)=
	static[512]char str
!	static[32]char str
	int length
	psymbol d
	static ichar longstring

	if p=nil then
		return
	end

	case p.opndtype
	when int_opnd then
		psint(p.value)
	when real_opnd then
		if p.xvalue=infinity then
!			fprint @str,"0x#",word@(p.xvalue):"h"
			fprint @str,"infinity"
		else
			print @str,p.xvalue:"e16.16"
		end
		psstr(str)

	when string_opnd then
		length:=p.slength
		if length<str.len/2 then
			psstr("""")
			newconvertstring(p.svalue, str, length)

			psstr(str)
			psstr("""*")
			psint(p.slength)

		else

			if longstring then
				pcm_free(longstring,longstringlen)
			end
			longstringlen:=length*2
			longstring:=pcm_alloc(longstringlen)
			longstring^:='"'
			length:=convertstring(p.svalue, longstring+1)
			(longstring+length+1)^:='"'
			(longstring+length+2)^:=0
			PSSTR(LONGSTRING)
		end

	when mem_opnd then
		d:=p.def
		psstr(p.def.name)

	when memaddr_opnd then
		psstr("&")
		recase mem_opnd

	when label_opnd then
		fprint @str,"L# ",p.labelno
		psstr(str)

	when no_opnd then
		return

!	when data_opnd then
!		fprint @str,"<Data * # (#)>", p.size,p.svalue
!		psstr(str)
!
	else
		psstr("<PCLOPND?>")
	end case
end

EXPORT func newconvertstring(ichar s, t, int length)int=
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
!		when 0 then
!			t++^:='\\'
!			t++^:='x'
!			t++^:='0'
!			t++^:='0'
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
		end case
	end

	t^:=0

	return t-t0
end

global func writepst:ref strbuffer=
	psymbol d, e

	gs_init(dest)

	psstrline("PCL Libs")
	for i to nplibfiles do
		psstrline(plibfiles[i])
	end
	psline()

	psstrline("PROC PC Symbol table")
	psline()

	d:=psymboltable

!CPL =PSYMBOLTABLE

	while d, d:=d.next do
!CPL "WRITE PST LOOP", D.NAME, D, D.NEXT
			writepsymbol(d, "25jl")
!
			if d.nameid in [procid, dllprocid] and not d.isimport then
!CPL "PROC:", D.NAME
				e:=d.nextparam
				while e, e:=e.nextparam do
					psstr("    ")
					writepsymbol(e, "21jl")
				end

				e:=d.nextlocal
				while e, e:=e.nextlocal do
					psstr("    ")
					writepsymbol(e, "21jl")
				end
			fi
PSLINE()
!		fi
	end
!CPL "DONE PST"
	psline()

	return dest
end

proc writepsymbol(psymbol d, ichar fmt)=
	byte localfile:=0
	[256]char str

	print @str, d.seqno:"4", namenames[d.nameid]
	psstr(str)
	to 10-strlen(namenames[d.nameid]) do psstr(" ") end

	str[1]:=0

	print @str, d.name:fmt
	psstr(str)

	psstr(strpmode(d.mode, d.size))

	if d.nameid=procid then
		psstr(" Pm:")
		psint(d.nparams)
		psstr(" Loc:")
		psint(d.nlocals)
	fi

	if d.isexport then psstr(" Exp") fi
	if d.isimport then psstr(" Imp") fi
	if d.varparams then psstr(" Var:"); psint(d.varparams) fi
!*!	if d.reg then psstr(" "); psstr(regnames[d.reg]) fi
!	if d.hasdot then psstr(" Dot") fi
	if d.isentry then psstr(" ENTRY PT") fi

!	if d.nameid=procid then psstr(" .PCCODE ="); PSSTR(STRINT(CAST(D.PCCODE),"H")) fi
	if d.pccode and d.nameid<>procid then
		psstr(" .PCCODE ="); PSSTR(STRINT(CAST(D.PCCODE),"H"))
		psline()
		writepcldata(d.pccode)
	fi

	psline()
end

proc writepcldata(pcl p)=
	destlinestart:=dest.length
	pclseqno:=0

	while p, p:=p.next do
		strpcl(p)
		gs_line(dest)
		destlinestart:=dest.length
	end

	psline()
end
=== backup.bat 0 0 62/74 ===
del *.asm
del ?.exe
del *.zip
del *.log
del AST?
del MCL?
del PCL
del PST
del SS
del *.ml
del *.mx
del *.obj
del *.ppm
del *.pgm
del *.pcl
del *.tcl
rem del *.c
ren mm.exe mm.eee
ren mc.exe mc.eee
del *.exe
ren mm.eee mm.exe
ren mc.eee mc.exe

rem del *.list


call backupproject bx test\*.*
copy bxall.zip c:\test

=== backuphex.bat 0 0 63/74 ===
mm7 mmp -o:bb
upx bb.exe
qq \m\scripts\writehex bb.exe
copy bb.hex g:\langs\mmbinaries
del bb.exe
del bb.hex
=== demo.bat 0 0 64/74 ===
mm7 ms
tim ms ms ms ms ms ms ms ms ms ms ms ms ms ms ms ms ms ms ms ms ms hello
=== e.bat 0 0 65/74 ===
ed makema.q

=== putbb.bat 0 0 66/74 ===
\m\mm7 mmp
copy mmp.exe \m\bb.exe
=== r.bat 0 0 67/74 ===
qq makema fred
=== runasm.bat 0 0 68/74 ===
aa %1.asm && %1 \cx2\t.c
=== rungas.bat 0 0 69/74 ===
as %1.asm -o %1.obj && gcc %1.obj -o %1.exe && %1
=== runasm.pro 0 0 70/74 ===
run runasm.bat $
file $.asm
file runasm.bat
file msys.m
file msystemp.m
file n.m
=== runexe.pro 0 0 71/74 ===
runexe $
file kkk11
=== rungas.pro 0 0 72/74 ===
run rungas.bat $
file $.asm
file rungas.bat
file msys.m
file msystemp.m
=== runmx.pro 0 0 73/74 ===
runinfo

	runexe pc -mx $ && runmx $
!	runexe runmx $
!	runexe runmx -show $

	runparams a b c d e
end

modules
	mmproject runmx.m

	module runmx.m
	module runmxshow.m
	module mx_decls.m
	module mx_lib.m
end
=== mm_help.txt 0 0 74/74 ===
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
1 mc.m 0 0
2 mclib.m 0 0
3 mcl_block.m 0 0
4 mcl_blockaux.m 0 0
5 mcl_caux.m 0 0
6 mcl_cdecls.m 0 0
7 mcl_cgen.m 0 0
8 mcl_clib.m 0 0
9 mcl_decls.m 0 0
10 mcl_gen.m 0 0
11 mcl_genaux.m 0 0
12 mcl_stack.m 0 0
13 mcr_decls.m 0 0
14 mcr_lib.m 0 0
15 mcr_run.m 0 0
16 mcr_write.m 0 0
17 mcx_asm.m 0 0
18 mcx_decls.m 0 0
19 mcx_exe.m 0 0
20 mcx_gas.m 0 0
21 mcx_genexe.m 0 0
22 mcx_genss.m 0 0
23 mcx_lib.m 0 0
24 mcx_optim.m 0 0
25 mcx_pedecls.m 0 0
26 mi.m 0 0
27 mlib.m 0 0
28 mlinux.m 0 0
29 mm.m 0 0
30 mmi.m 0 0
31 mmp.m 0 0
32 mm_blockpcl.m 0 0
33 mm_cli.m 0 0
34 mm_decls.m 0 0
35 mm_diags.m 0 0
36 mm_exportm.m 0 0
37 mm_exportq.m 0 0
38 mm_genaux.m 0 0
39 mm_genmcl.m 0 0
40 mm_genpcl.m 0 0
41 mm_lex.m 0 0
42 mm_lib.m 0 0
43 mm_libsources.m 0 0
44 mm_modules.m 0 0
45 mm_name.m 0 0
46 mm_parse.m 0 0
47 mm_pclblock.m 0 0
48 mm_support.m 0 0
49 mm_tables.m 0 0
50 mm_type.m 0 0
51 mnoos.m 0 0
52 ms.m 0 0
53 msys.m 0 0
54 msysmin.m 0 0
55 msystemp.m 0 0
56 msyswin.m 0 0
57 mwindll.m 0 0
58 mwindows.m 0 0
59 pcl_api.m 0 0
60 pcl_decls.m 0 0
61 pcl_diags.m 0 0
62 backup.bat 0 0
63 backuphex.bat 0 0
64 demo.bat 0 0
65 e.bat 0 0
66 putbb.bat 0 0
67 r.bat 0 0
68 runasm.bat 0 0
69 rungas.bat 0 0
70 runasm.pro 0 0
71 runexe.pro 0 0
72 rungas.pro 0 0
73 runmx.pro 0 0
74 mm_help.txt 0 0
