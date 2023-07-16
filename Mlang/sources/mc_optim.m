import clib
import mlib
import* pci_core
import* pci_mcl


global proc peephole(ref mclrec m)=
	ref mclrec m2,m3,mtarget,lastmcl
	int lab1,lab2
	lastmcl:=nil

	if not fpeephole then return fi

	do
		m2:=m.nextmcl
		while m2 and m2.opcode in [m_comment, m_deleted] do m2:=m2.nextmcl od

		switch m.opcode
		when m_procstart then

		when m_procend then
			exit

		when m_jmp then
dojmp::
	GOTO SKIP

		when m_jmpcc then
			if m2.opcode<>m_jmp then goto dojmp fi
!jcc followed by jmp; detect jcc L1; jmp L2; L1: and replace with:
! jncc L2; <deleted>; L1
			lab1:=m.a.labelno
			m3:=m2.nextmcl
			if m3.opcode=m_labelx and m3.a.labelno=lab1 then
				m.a:=mgenlabel(m2.a.labelno)
				m.cond:=asmrevcond[m.cond]
				deletemcl(m2,102)
			fi

		when m_test then
			case lastmcl.opcode
			when m_andx, m_orx, m_xorx then
				if sameregopnd(m.a,m.b) and sameregopnd(m.a,lastmcl.a) then
					deletemcl(m,103)
				fi
			esac

		when m_movzx then
			if m.a.mode=a_reg and m.a.size=8 and m.b.size<4 then
				m.a:=changeopndsize(m.a,4)
			fi
		when m_mov then
			if isreg0(m.a) and isregopnd(m.b) then
				if isreg0(m2.b) and m2.regend[r0] AND M2.A.SIZE=8 then
					m2.b:=m.b
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

			if m.b.mode=a_reg and m2.opcode=m_mov and m2.a.mode=a_reg and
				m.b.reg=m2.a.reg and sameoperand(m.a,m2.b) then
				deletemcl(m2,141)
			fi

			if m.b.mode=a_reg and m2.opcode=m_mov and m2.a.mode=a_reg then
				 if m.a.mode=a_mem and sameoperand(m.a, m2.b) then		!mov [MEM1],Da; mov Db,[MEM1] => mov Db,Da
					m2.b:=mgenreg(m.b.reg)
				fi
			fi
		when m_xorx then
			if isreg0(m.a) and isreg0(m.b) then
				if isreg0(m2.b) and m2.regend[r0] then
					m2.b:=mgenint(0)
					deletemcl(m,110)
				fi
			fi

		when m_lea then
			if isreg0(m.a) and m2.opcode=m_mov then
				if isregopnd(m2.a) and isreg0(m2.b) and m2.regend[r0] then
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
	if a.mode<>a_reg then return 0 fi
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
	m.opcode:=m_deleted
end
