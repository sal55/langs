import clib
import mlib
import oslib

import pc_decls
import pci_mcl
import pc_objdecls
import pc_tables

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
			CPL RELOCNAMES[F.RELTYPE]
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
!CPL "GENAMODE/MEM",VALTYPENAMES[A.VALTYPE],A.DEF
!CPL "GENAMODE/ABS32"
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
!				unless -0x8000'0000<=value<=0xFFFF'FFFFu then
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
!			gendword(value)
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
!	genrex()
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
		when m_comment, m_blank then
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

