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
