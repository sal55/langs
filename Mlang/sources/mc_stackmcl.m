global macro freereg(r) =
	(regset[r]:=0; mccodex.regend[r]:=1)

global proc initpass1(pcl p)=
!done before pass1 at procdef
	symbol d:=p.def, e

	passno:=1

	clear regset
	clear xregset
	clear isregvar
	clear isxregvar

	clear pcltempflags
	r10used:=r11used:=r13used:=0

	mstackdepth:=0
	noperands:=0

	frameoffset:=paramoffset:=framebytes:=0
	nlocals:=nparams:=0
	usedparams:=usedxparams:=0
	nproccalls:=highargs:=0
	localshadow:=0
	assemused:=0
	highreg:=highxreg:=0


	nlocals:=nparams:=0
	clear pcltempflags

	if ttisblock[d.mode] then
		e:=getduplnameptr(d,addnamestr("$1x"),paramid)
		e.mode:=d.mode
		e.used:=1
		blockretname:=e
		paramdefs[++nparams]:=e
	fi

	e:=d.deflist

!framedefs not done until initpass1x, to allow for any late blockdefs
	while e, e:=e.nextdef do
		case e.nameid
		when paramid then
			if nparams>=maxparams then merror("Too many params") fi
			paramdefs[++nparams]:=e
		esac
	od
end

global proc initpass1x(pcl p)=
	symbol d:=p.def, e

	e:=d.deflist

	while e, e:=e.nextdef do
		case e.nameid
		when frameid then
			if not e.atvar then
				if e.used then
					if nlocals>=maxlocals then merror("Too many locals") fi
					localdefs[++nlocals]:=e
				fi

			fi
		esac
	od
end

global proc initpass2=
!done before pass2 at procdef
	int reg
	symbol d

	passno:=2

	clear regset
	clear xregset
	clear isregvar
	clear isxregvar

	clear pcltempflags
	r10used:=r11used:=r13used:=0

	mstackdepth:=0
	noperands:=0

	frameoffset:=paramoffset:=framebytes:=0

!high/xregs were set in pass1. They can be extended here by use as regvars
!It is assumed that in pass2, highreg will be no higher than it was in pass1.
!It might be lower, but I don't know that.


!do regvars assignments

	if leafproc then
		for i to nparams do
			exit when i>4
			d:=paramdefs[i]
			if d.used and not d.addrof and not d.noreg and d.nrefs then
				case ttcat[d.mode]
				when d64cat then
					d.reg:=reg:=r10+i-1
					isregvar[reg]:=1
					if reg=r10 then r10used:=1 fi
					if reg=r11 then r11used:=1 fi
					if reg=r13 then r13used:=1 fi
				when x64cat then
					d.reg:=reg:=r0+i-1
					isxregvar[d.reg]:=1
				esac
			fi
		od
	fi


	for i to nlocals do
		d:=localdefs[i]
		if not d.addrof and not d.noreg and d.nrefs and 
				ttbasetype[d.mode] not in [trecord, tarray] then
			case stdcat[getpclmode(d.mode)]
			when d64cat then
				if highreg>=r9 then next fi			!no more regs
				if highreg<r3 then
					highreg:=r3
				else
					++highreg
				fi
				d.reg:=highreg
				isregvar[highreg]:=1
			when x64cat then
				if highxreg>=r15 then next fi			!no more regs
				if highxreg<r6 then
					highxreg:=r6
				else
					++highxreg
				fi
				d.reg:=highxreg
				isxregvar[highxreg]:=1
			esac
		fi
	od

	if not leafproc then
		for i to nparams do
			exit when i>4
			d:=paramdefs[i]
			if d.used and not d.addrof and not d.noreg and d.nrefs then
				case ttcat[d.mode]
				when d64cat then
					if highreg>=r9 then next fi			!no more regs
					if highreg<r3 then
						highreg:=r3
					else
						++highreg
					fi
					d.reg:=highreg
					isregvar[highreg]:=1
				esac
			fi
		od
	fi
end

global proc checkopnds=
!after endproc on pass1 or pass2
	symbol d

	if mstackdepth then
		println passno,"HW stack not empty",currproc.name,=mstackdepth
		MSTACKDEPTH:=0

!		MERROR("reset:mstackdepth?")
	fi
	if noperands then
		println passno,"Reset:pcl stack not empty:",currproc.name,=noperands
	fi

	for i in regset.bounds do
		if regset[i] or xregset[i] then
			println passno,"Reset: reg flag set",currproc.name,REGSET[I],XREGSET[I],=I
			exit
		fi
	od
end

proc newopnd(int loc=reg_loc)=
	if noperands>=maxoperands then
		merror("PCL stack overflow")
	fi
	++noperands
	pclstack[noperands]:=pclstackzero
	pclstack[noperands].loc:=loc
	pclstack[noperands].cat:=d64cat
	pclstack[noperands].count:=1
	pclvals[noperands].value:=0
end

global proc duploperand=
!assume 64-bit operand
	int reg
	ref pclstackrec pc
	mcloperand ax
	
	checkloaded(xa)			!simplest if in a register

	++noperands
IF NOPERANDS>MAXOPERANDS THEN MERROR("OV1") FI

	pclstack[noperands]:=pclstack[noperands-1]
	pclvals[noperands]:=pclvals[noperands-1]

	pc:=&pclstack[noperands]
!!There is now a simple duplicate; but it will need more work depending
!!on the current format

	case pc.loc
	when reg_loc then			!need to be physically duplicated
		reg:=getnextreg()
		pclstack[ya].reg:=reg
		genmc(m_mov, ax:=mgenreg(reg), mgenreg(pclstack[xb].reg))

	when xreg_loc then			!need to be physically duplicated
		reg:=getnextxreg()
		pclstack[xa].reg:=reg
		if pc.cat=x64cat then
			genmc(m_movq, mgenxreg(reg), mgenxreg(pclstack[xb].reg))
		else
			genmc(m_movd, mgenxreg(reg,4), mgenxreg(pclstack[xb].reg,4))
		fi
	esac
end

global proc addlabel(int lab,offset=0)=
	newopnd(label_loc)
	pclvals[xa].labno:=lab
	pclvals[xa].offset:=offset
end

global proc addreg0(int reg)=
!turn return value in r0 into a new pclstack operand
!(modified for mult regs)
	newopnd(reg_loc)
	pclstack[xa].reg:=reg
	if regset[reg] then
		merror("addreg0/reg in use")
	fi
	regset[reg]:=1
end

global proc addxreg0(int reg,size=8)=
!turn return value in x0 into a new pclstack operand
	newopnd(xreg_loc)
	pclstack[xa].reg:=reg
	pclstack[xa].cat:=(size=8|x64cat|x32cat)
	if xregset[reg] then merror("addxreg0/reg in use") fi
	xregset[reg]:=1
end

global proc addreg_d64=
!create new pcl opnd in any d64 reg
	newopnd(reg_loc)
	pclstack[xa].reg:=getnextreg()
end

global proc addreg_x64=
	newopnd(xreg_loc)
	pclstack[xa].reg:=getnextxreg()
	pclstack[xa].cat:=x64cat
end

global proc addreg_x32=
	newopnd(xreg_loc)
	pclstack[xa].reg:=getnextxreg()
	pclstack[xa].cat:=x32cat
end

global proc loadparam(int n=noperands, reg)=
!load pcl opnd n into given register
	int oldreg, value
	mcloperand ax
	ref pclstackrec pc:=&pclstack[n]
	ref pclvaluerec pcv:=&pclvals[n]

	ax:=mgenreg(reg)
	oldreg:=pc.reg

	case pc.loc
	when reg_loc, regvar_loc then
		genmc(m_mov, ax, mgenreg(oldreg))

	when xreg_loc, xregvar_loc then
		if pc.cat=x32cat then
			genmc(m_movd, changeopndsize(ax,4), mgenxreg(oldreg))
		else
			genmc(m_movq, ax, mgenxreg(oldreg))
		fi

	when immd64_loc then
		value:=pcv.value
		if value=0 then
			ax:=mgenreg(reg,4)
			genmc(m_xorx, ax,ax)
		else
			genmc(m_mov, ax, mgenint(pcv.value))
		fi
	when immx64_loc then
		genmc(m_mov, ax, mgenlabelmem(pcv.r64index))

	when immx32_loc then
		genmc(m_mov, ax, mgenlabelmem(pcv.r32index))

	when string_loc then
		genmc(m_mov,ax, mgenlabel(pcv.strindex))

	when mem_loc then
		genmc(m_mov,ax,mgenmem(pcv.def))

	when memaddr_loc then
		genmc(m_lea,ax,mgenmem(pcv.def))

	when temp_loc then
		genmc(m_mov,ax,mgentemp(n))

	else
		CPL "LOADPARAM:",locnames[pc.loc]
		MGENCOMMENT("****LOADPARAM??")
		MERROR("LOADPARAM??",locnames[pc.loc])
	esac
end

global proc loadxparam(int n=noperands, reg)=
	mcloperand ax
	ref pclstackrec pc:=&pclstack[n]
	ref pclvaluerec pcv:=&pclvals[n]

	ax:=mgenxreg(reg)

	case pc.loc
	when reg_loc, regvar_loc then
		genmc(m_movq, ax, mgenreg(pc.reg))

	when xreg_loc, xregvar_loc then
		if pc.cat=x32cat then
			genmc(m_movd, ax, mgenxreg(pc.reg))
		else
			genmc(m_movq, ax, mgenxreg(pc.reg))
		fi
	when immx64_loc then
		genmc(m_movq, ax, mgenlabelmem(pcv.r64index))

	when immx32_loc then
		genmc(m_movd, ax, mgenlabelmem(pcv.r32index))

	when temp_loc then
		if pc.cat=x32cat then
			genmc(m_movd, ax, mgentemp(n))
		else
			genmc(m_movq, ax, mgentemp(n))
		fi

	else
		CPL "??LOADXPARAM",N,NOPERANDS
		MGENCOMMENT("****LOADXPARAM??")
		MERROR("LOADXPARAM??",locnames[pc.loc])
	esac
end

global function getopnd(int n=noperands,size=8)mcloperand ax=
!get an access mode for pcl opnd n, which means a register operand, memory, 
! immediate etc
!It can mean loading to a register if opnd is currently on stack, or float imm, etc
!In that case, its loc will be updated
!getopnd should be called from top-of-stack down, in case they are currently
!on the hardware stack, which must be popped in sequence
!Any regvars stay in their regs, but this means they can't be modified

!int, float, or low half of wide
	int reg, value
	ref pclstackrec pc:=&pclstack[n]
	ref pclvaluerec pcv:=&pclvals[n]

	case pc.loc
	when reg_loc then
		return mgenreg(pc.reg,size)

	when regvar_loc then
		return mgenregvar(pcv.def)

	when xreg_loc then
		return mgenxreg(pc.reg,size)
!
	when xregvar_loc then
		return mgenxregvar(pcv.def)

	when temp_loc then
		return mgentemp(n)

	when immd64_loc then
		value:=pcv.value
		if int32.minvalue<=value<=int32.maxvalue then
			return mgenint(value)
		fi
		ax:=mgenreg(getnextreg())
		genmc(m_mov, ax, mgenint(value))
		pc.reg:=ax.reg
		pc.loc:=reg_loc
		return ax

	when immx64_loc then
		return mgenlabelmem(pcv.r64index)

	when immx32_loc then
		return mgenlabelmem(pcv.r32index)

	when memaddr_loc then
		reg:=getnextreg()
		ax:=mgenreg(reg)
		genmc(m_lea,ax,mgenmem(pcv.def))
		pc.reg:=ax.reg
		pc.loc:=reg_loc
		return ax

	when mem_loc then
		return mgenmem(pcv.def)

	when string_loc then
!		return mgenlabel(getstringindex(pcv.svalue))
		return mgenlabel(pcv.strindex)

	when label_loc then
		MGENCOMMENT("GENLABELMEM")
		return mgenlabelmem(pcv.labno)

	else
		merror("GETOPND? ",locnames[pc.loc])
	esac
!
	return nil
end

global function loadopnd(int n=noperands, size=8)mcloperand =
!ensure operand is loaded to a register, either reg/xreg depending on category
!regvars must be loaded to a general register (eg. to do ops on it)

	mcloperand ax,bx
	int reg
	ref pclstackrec pc:=&pclstack[n]
	ref pclvaluerec pcv:=&pclvals[n]

!special handling for regvars, which are already in regs
	case pc.loc
	when regvar_loc then
		genmc(m_mov, ax:=mgenreg(reg:=getnextreg(),size), mgenreg(pc.reg,size))
		pc.loc:=reg_loc
		pc.reg:=reg
		return ax
	when xregvar_loc then
		if pc.cat=x64cat then
			genmc(m_movq, ax:=mgenxreg(reg:=getnextxreg()), mgenxreg(pc.reg))
			pc.loc:=xreg_loc
			pc.reg:=reg
			return ax
		else
			merror("loadopnd/xreg32")
		fi
	WHEN LABEL_LOC THEN
		reg:=getnextreg()

		if pc.ilabel then
			bx:=mgenlabelmem(pcv.labno)
		else
			bx:=mgenlabel(pcv.labno)
		fi

		genmc(m_mov, ax:=mgenreg(reg,size), bx)
		pc.reg:=reg
		pc.loc:=reg_loc
		return ax
	esac

!get the access mode
	ax:=getopnd(n,size)
!if the current loc is now in a reg, then done
	if pc.loc in [reg_loc, xreg_loc] then return ax fi

!opnd not loaded; get it inoto a register via ax if needed
	case pc.cat
	when d64cat then
		reg:=getnextreg()
		if pc.loc=immd64_loc and pcv.value=0 and not noxorclear then
			ax:=mgenreg(reg,4)
			genmc(m_xorx,ax,ax)
			bx:=mgenreg(reg,size)
		else
			genmc(m_mov, bx:=mgenreg(reg,size), changeopndsize(ax,size))
		fi
		pc.reg:=reg
		pc.loc:=reg_loc
	when x64cat then
		genmc(m_movq, bx:=mgenxreg(reg:=getnextxreg()), ax)
		pc.reg:=reg
		pc.loc:=xreg_loc
	when x32cat then
		genmc(m_movd, bx:=mgenxreg(reg:=getnextxreg()), ax)
		pc.reg:=reg
		pc.loc:=xreg_loc
	else
		merror("Loadopnd:",catnames[pc.cat])
	esac

	return bx
end

global proc checkloaded(int n=noperands)=
	if pclstack[n].loc in [reg_loc, xreg_loc] then
		return
	fi
	loadopnd(n)
end

global proc checkallloaded=
	for i to noperands do
		case pclstack[i].loc
		when immd64_loc, immx64_loc, immx32_loc, memaddr_loc then
		else
			checkloaded(i)
		esac
	od
end

global function getopnd_ind(int index=noperands,size=8)mcloperand=
!int, float, or low half of wide
!
	case pclstack[index].loc
	when reg_loc then
		return mgenireg(pclstack[index].reg,size)
	esac

	loadopnd(index)

	return getopnd_ind(index,size)
end

global function getnextreg:int=
	to 16 do
		for r:=r0 to regmax do
			if regset[r]=0 then
				regset[r]:=1
				highreg max:=r
				return r
			fi
		od

!all regs occupied; need to free one
		savenextopnd()
	od
	merror("GNR")
	0
end

!global function getnextxreg(int firstop=1)int=
global function getnextxreg:int=
	int reg,firstreg

	to 16 do
		for r:=r4 to xregmax do
			if xregset[r]=0 then
				xregset[r]:=1
				highxreg max:=r
				return r
			fi
		od
	!all regs occupied; need to free one
		savenextxopnd()
	od
	merror("GNXR")
	0
end

global proc freexreg(int xr)=
	xregset[xr]:=0
end

global proc pushopnd(int n)=
!make sure operand n is on the hw stack; caller must know that all
!previous pclstack operands are already on the stack
	ref pclstackrec pc:=&pclstack[n]
	ref pclvaluerec pcv:=&pclvals[n]
	mcloperand ax

	case pc.loc
	when reg_loc then
		genmc(m_push, mgenreg(pc.reg))
		freereg(pc.reg)

	when regvar_loc then
		genmc(m_push, mgenreg(pc.reg))

	when xreg_loc then
		if r13used then merror("2:R13 in use") fi
		genmc(m_movq,mgenreg(r13), mgenxreg(pc.reg))
		genmc(m_push, mgenreg(r13))
		freexreg(pc.reg)

	when immd64_loc then
		genmc(m_push, mgenint(pcv.value))

	when immx64_loc then
		genmc(m_push, mgenlabelmem(pcv.r64index))

	when immx32_loc then
		if r13used then merror("4:R13 in use") fi
		genmc(m_mov, mgenreg(r13,4), mgenlabelmem(pcv.r32index))
		genmc(m_push, mgenreg(r13))

	when string_loc then
		genmc(m_push, mgenlabel(pcv.strindex))

	when memaddr_loc then
		if r13used then merror("3:R13 in use") fi
		genmc(m_lea, mgenreg(r13), mgenmem(pcv.def))
		genmc(m_push, mgenreg(r13))

	when mem_loc then
		genmc(m_push, mgenmem(pcv.def))

	else
		merror("Can't push opnd: #",locnames[pc.loc])
	esac

	delopnd()
	++mstackdepth
end

global proc popargs(int nargslots)=
!pop pcl opnds which were args to a call
!however, nargslots is the number of slots used, not actual args, since
!a wide argument was counted as two slots
	int cat

	while nargslots>0 do
		cat:=pclstack[xa].cat
		poparg()
		--nargslots
	od
end

global proc poparg=

	case pclstack[xa].loc
	when reg_loc then
		freereg(pclstack[xa].reg)
	when xreg_loc then freexreg(pclstack[xa].reg)
	when immd64_loc, string_loc, memaddr_loc, mem_loc,
			immx64_loc, immx32_loc then
	when regvar_loc then
	when xregvar_loc then
	else
		merror("poparg? #",locnames[pclstack[xa].loc])
	esac
	--noperands
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

global proc saveopnd(int n, allregs=1)=
!if operand is in a volatile register, then save it in a temp
!allregs=1 to save both A and B regs (vol/nonval), which can include P regs if
!used as workregs; this is to save pne reg=opnd to a temp to free up a register
!allregs=0 to limit to A regs (possibly some P regs) only; normall for CALLs
!in order to preserve non-vol regs, as call will preserve the rest

	int reg
	mcloperand tx

	case pclstack[n].loc
	when reg_loc then
		reg:=pclstack[n].reg
		if allregs or reg not in r3..r9 then
			genmc(m_mov, tx:=mgentemp(n), mgenreg(reg))
			pclstack[n].loc:=temp_loc
			pclvals[n].mopnd:=tx
			freereg(reg)
		fi

	when xreg_loc then
		reg:=pclstack[n].reg
		if allregs or reg in r0..r5 then
			if pclstack[n].cat=x64cat then
				genmc(m_movq, mgentemp(n), mgenxreg(reg))
			else
				genmc(m_movd, mgentemp(n), mgenxreg(reg,4))
			fi
			pclstack[n].loc:=temp_loc
			freexreg(reg)
		fi
	when regvar_loc, xregvar_loc then

	when immd64_loc, memaddr_loc, mem_loc, immx64_loc, immx32_loc,
		temp_loc, string_loc then

	else
		merror("Can't save opnd: #",locnames[pclstack[n].loc])
	esac

end

global proc saveopnds(int n=0)=
!save all operands other than top n
!assume this is to do with calls
	for i:=1 to noperands-n do
		saveopnd(i,0)
	od
end

global proc savenextopnd=
!starting from the first loaded, look for and save first reg-based opnd
!this does A/B/P regs if used
	for i:=1 to noperands do
		if pclstack[i].loc=reg_loc then
			saveopnd(i,1)
			return
		fi
	od
end

global proc savenextxopnd=
!as savenextopnd but only does AX/BX/PX regs 
	for i:=1 to noperands do
		if pclstack[i].loc=xreg_loc then
			saveopnd(i,1)
			return
		fi
	od
end

global proc movetoreg(int newreg)=
	int oldreg

	loadopnd()
	oldreg:=pclstack[xa].reg

	if oldreg=newreg then
		return
	fi

	if regset[newreg] then
!			println "movereg/reg in use"
!			mgencomment("movereg/reg in use")
			merror("movereg/reg in use")
	fi
	genmc(m_mov, mgenreg(newreg), mgenreg(oldreg))
	freereg(oldreg)
	pclstack[xa].reg:=newreg
	pclstack[xa].loc:=reg_loc
	regset[newreg]:=1
	if newreg>=r10 then highreg max:=newreg fi
end
!
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

	pclvaluerec u
	u:=pclvals[m]
	pclvals[m]:=pclvals[n]
	pclvals[n]:=u

!	swap(pclvals[m], pclvals[n])

end

global proc swapopndregs(int reg2)=
!top operand is in a register. Need to switch its reg with whatever is occupying
!reg2

	int reg1:=pclstack[xa].reg

	for i:=noperands-1 downto 1 do
		if pclstack[i].loc=reg_loc and pclstack[i].reg=reg2 then
			swap(pclstack[xa].reg, pclstack[xb].reg)
			return
		fi
	else
		CPL CURRPROC.NAME
		merror("swapopndregs/reg not found")
	od
end

global proc addmem(pcl p)=
	int reg
	symbol d:=p.def
	mcloperand ax

	case p.pcat
	when d64cat then
		if d.reg then
			newopnd(regvar_loc)
			pclstack[xa].reg:=d.reg
		else
			newopnd(mem_loc)
		fi
		pclvals[xa].def:=d

	when x64cat then
		newopnd(xreg_loc)
		pclstack[xa].reg:=reg:=getnextxreg()
		pclstack[xa].cat:=x64cat
		genmc(m_movq,mgenxreg(reg),mgenmem(d))
	when x32cat then
		newopnd(xreg_loc)
		pclstack[xa].cat:=x32cat
		pclstack[xa].reg:=reg:=getnextxreg()
		genmc(m_movd,mgenxreg(reg),mgenmem(d))

	when blockcat then
		newopnd(reg_loc)
		pclstack[xa].reg:=reg:=getnextreg()
		genmc(m_lea,mgenreg(reg),mgenmem(d))

	when shortcat then
		newopnd(reg_loc)
		pclstack[xa].reg:=reg:=getnextreg()
		ax:=getopnd(xa)
		genmc((ttsigned[p.pmode]|m_movsx|m_movzx), ax, mgenmem(d))
		return

	else
		merror("ADDMEM?",CATNAMES[P.PCAT])
	esac
end

global proc addmemaddr(symbol d)=
	newopnd(memaddr_loc)
	pclvals[xa].def:=d
!	d.addrof:=1
end

global proc addimm(u64 a)=
	newopnd(immd64_loc)
	pclvals[xa].value:=a
end

global proc addimmx64(int r64index)=
	newopnd(immx64_loc)
	pclvals[xa].r64index:=r64index
	pclstack[xa].cat:=x64cat
end

global proc addimmx32(int r32index)=
	newopnd(immx32_loc)
	pclvals[xa].r32index:=r32index
	pclstack[xa].cat:=x32cat
end

global proc addstr(int strindex)=
	newopnd(string_loc)
	pclvals[xa].strindex:=strindex
end

global proc delopnd=
	int reg
	ref pclstackrec pc

	if noperands<=0 then
		MGENCOMMENT("****DELND/UNDERFLOW")
		RETURN
	fi

	pc:=&pclstack[noperands]

	if pc.count>1 then
		--pc.count
		return
	fi

	case pc.loc
	when reg_loc, regvar_loc then
		reg:=pc.reg

		freereg(reg)
	when xreg_loc, xregvar_loc then
		freexreg(pc.reg)
	when immd64_loc, memaddr_loc, immx64_loc, immx32_loc, mem_loc,
		regvar_loc, xregvar_loc, string_loc, temp_loc, label_loc then
	else
		merror("delopnd: can't do xreg etc",locnames[pc.loc])
	esac	

	--noperands
end

