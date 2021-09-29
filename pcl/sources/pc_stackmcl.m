import clib
import msys
import* pci_core
import* pci_mcl

global macro freereg(r) =
	(regset[r]:=0; mccodex.regend[r]:=1)

global proc resetopnds1=
!after pass1

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

	if not foptimise then			!else needed for pass 2 procentry
		inf_proccalls:=0
		inf_maxargs:=0
		inf_proclocals:=0
		inf_procxlocals:=0

		inf_leafproc:=0
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
!		merror("GENOPND? ",fmtnames[pclstack[index].fmt])
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

