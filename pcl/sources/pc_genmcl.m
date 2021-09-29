import clib
import mlib
import* pci_core
import* pci_mcl

!const fshowpcl=1
const fshowpcl=0
!const fshowopndstack=1
const fshowopndstack=0

!const fshowbothmcl=1
const fshowbothmcl=0

pcl currpcl
[0..klast]ref proc(pcl) px_handlertable

[0..5]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
[0..5]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

[]int multregs=(r0,r1,r2,r10,r11,r12)
[]int multxregs=(r0,r1,r2,r3,r4,r5)

global proc genmcl(int optim)=

	if mcldone then return fi

	inithandlers()
	optimflag:=optim

	mclinit()

	currpcl:=pcstart
	mlabelno:=labelno
	mseqno:=0

	repeat
		convertpcl(currpcl)
		++currpcl
	until currpcl.opcode=kendprogram

	genabsneg()
	genstringtable()
	genrealtable()
	genrtsproctable()

	genfunctiontable()
	genmc(m_nop)
	genmc(m_nop)

	mcldone:=1
end

proc convertpcl(pcl p)=
	[1256]char str
	ichar ss
	int m

	if fshowpcl  then
		case p.opcode
		when klabel, kcomment, klocal, kprocdef, kprocentry,
			kretproc, kendproc, kparam, kextproc, kextparam, kendextproc then
		else
				strcpy(&.str,"                       ")
				strcat(&.str,pclnames[p.opcode])
				mgencomment(&.str)
		esac
	fi

	mseqno:=p.seqno
	px_handlertable[p.opcode]^(p)

	if fshowopndstack then
		case p.opcode
		when klabel, kcomment, klocal, kprocdef, kprocentry,
			kretproc, kendproc, kparam, kextproc, kextparam, kendextproc then
		else
			showopndstack()
		esac
	fi
end

proc inithandlers=
	static byte initdone=0
	ichar name
	int n

	if initdone then return fi

	n:=$get_nprocs()

	for i to n do
		name:=$get_procname(i)
		if eqbytes(name,"px_",3) then
			for k in pclnames.bounds do
				if eqstring(pclnames[k]+1,name+3) then
					px_handlertable[k]:=$get_procaddr(i)
					exit
				fi
			else
				gerrorc("Invalid handler name:",name)
			od
		fi
	od

	for i in px_handlertable.bounds do
		if not px_handlertable[i] then
			px_handlertable[i]:=cast(&unimpl)
		fi
	od

	initdone:=1
end

proc unimpl(pcl p)=
!doesn't need a handler, but used as default handler for all opcodes
!for which its pc-handler doesn't exist
	[300]char str

	print @str,"Unimplemented Opcode:",pclnames[p.opcode]
	mgencomment(str)
!	gerrorc("Unimplemented Opcode:",pclnames[p.opcode])
	println "Unimplemented Opcode:",pclnames[p.opcode]
end

global proc gerrorc(ichar mess, param=nil)=
	print "MCL Gen error:",mess
	if param then
		print ":",param
	fi

!	println " on line:",lxlineno
	stop 1
end

proc px_zero(pcl p)=
	unimpl(p)
end

proc px_nop(pcl p)=
	unimpl(p)
end

proc px_stop(pcl p)=
	psymbol d

	loadparam(1,r10)

	d:=pcl_makesymbol("exit")
	d.isimported:=1
	genmc(m_call, mgenmemaddr(d))

	delopnd()
end

proc px_comment(pcl p)=
	mgencomment(p.svalue)
end

proc px_importdll(pcl p)=
	unimpl(p)
end

proc px_istatic(pcl p)=
	setsegment('I',p.align)
	genmc(m_labelname,mgenmemaddr(p.def))
end

proc px_zstatic(pcl p)=
	psymbol d

	d:=p.def
	setsegment('Z',p.align)
	genmc(m_labelname,mgenmemaddr(d))

	genmc(m_resb,mgenint(p.size))
end

proc px_equiv(pcl p)=
	unimpl(p)
end

proc px_procdef(pcl p)=
	ichar name

	procdefpcl:=currpcl
	procdef:=currpcl.def

	name:=procdef.name

	setsegment('C',16)
	genmc(m_labelname,mgenmemaddr(procdef))

	nlocals:=nparams:=0
end

proc px_procentry(pcl p)=
	int np, regoffset, offset, dreg, xreg, nregparams, nspill,hasequiv
	mcloperand ax
	psymbol d

	framebytes:=0
	frameoffset:=0
	paramoffset:=0
	needstackframe:=0
	ndsaveregs:=nxsaveregs:=0			!not of if b=non-vol regs to be spilled
	ndsavepush:=0
	nregparams:=nspill:=0
	needshadow48:=0			!duplicate caller's shadow space
	needshadow32:=0			!local shadow space
	hasequiv:=0

	if inf_assem then skip fi

	if passno=2 and inf_leafproc then	!no point in pass1 as no info avialable
		dreg:=r10			!next available dreg
		xreg:=r0			!next available xreg

		for i to nparams do
			if i>4 then exit fi
			d:=paramdefs[i]
			case pcat[d.pcldef.mode]
			when d64cat then
				if not d.addrof and not d.noreg and d.nrefs then
					d.reg:=dreg
					isregvar[dreg]:=1
					if dreg=r10 then inf_r10used:=1 fi
					if dreg=r11 then inf_r11used:=1 fi
					if dreg=r13 then inf_r13used:=1 fi
					++nregparams
				fi
			when x64cat then
				if not d.addrof and d.nrefs then
					d.reg:=xreg
					isxregvar[dreg]:=1
					++nregparams
				fi
			esac
			++dreg
			++xreg
		od
	fi

	if passno=2 then		!no point in pass1 as no info avialable
		dreg:=r9			!next available dreg
		xreg:=r15			!next available xreg
		for i to nlocals do
			d:=localdefs[i]
			case pcat[d.pcldef.mode]
			when d64cat then
				if not d.addrof and not d.noreg and d.nrefs then
					if dreg<=inf_highreg or dreg<r3 then next fi
					dsaveregs[++ndsaveregs]:=dreg
					d.reg:=dreg
					isregvar[dreg]:=1
					--dreg
				fi
			when x64cat then
				if not d.addrof and d.nrefs and not d.noreg then
!				if not d.addrof and d.nrefs then
					if xreg<=inf_highxreg or xreg<r6 then next fi
					xsaveregs[++nxsaveregs]:=xreg
					d.reg:=xreg
					isxregvar[dreg]:=1
					--xreg
				fi
			esac
		od

!see if any params not regvars can use spare nonvol regs
		if not inf_leafproc then
			for i to nparams do
				if i>4 then exit fi
				d:=paramdefs[i]
				case pcat[d.pcldef.mode]
				when d64cat then
					if not d.addrof and d.nrefs and not d.noreg then
						if dreg<=inf_highreg or dreg<r3 then next fi
						dsaveregs[++ndsaveregs]:=dreg
						d.reg:=dreg
						isregvar[dreg]:=1
						--dreg
						++nregparams
					fi
				when x64cat then
					if not d.addrof and d.nrefs and not d.noreg then
						if xreg<=inf_highxreg or xreg<r6 then next fi
						xsaveregs[++nxsaveregs]:=xreg
						d.reg:=xreg
						isxregvar[dreg]:=1
						--xreg
						++nregparams
					fi
				esac
			od
		fi

	fi
skip::

	for i to nparams do
		d:=paramdefs[i]

		if not d.reg then			!not a regvar
			if i>1 and pcat[d.pcldef.mode]=widecat and paramdefs[i-1]=d then
			else
				d.offset:=paramoffset+16
				genmc(m_define, mgenname(getfullname(d)), mgenint(d.offset))
			fi

		elsif pcat[d.pcldef.mode]=d64cat then
			genmc(m_definereg, mgenname(getfullname(d)), mgenreg(d.reg))
		else
			genmc(m_definereg, mgenname(getfullname(d)), mgenxreg(d.reg))
		fi
		paramoffset+:=8
	od

	for i:=r3 to inf_highreg do		!add any non-vol regs
		dsaveregs[++ndsaveregs]:=i
	od

	for i:=r6 to inf_highxreg do		!add any non-vol xregs
		xsaveregs[++nxsaveregs]:=i
	od


!Decided class of proc entry/exit code:
!(1) Full (always a stack frame, nonvols not pushed)
!(2) Pushed nonvols, uses stack frame
!(3) Pushed nonvols, no stack frame

	if nparams>4 then
		needstackframe:=1
		nspill:=4-nregparams
	else
		ndsavepush:=ndsaveregs
		ndsaveregs:=0
		nspill:=nparams-nregparams
		if nspill then needstackframe:=1 fi
!		if nspill and nthen needstackframe:=1 fi
!		if ndsavepush then
!			if nspill then
!				needstackframe:=1
!				needshadow48:=1
!			fi
!		fi
	fi

	for i to nlocals do
		d:=localdefs[i]
		if d.atvar then
			hasequiv:=1
        elsif not d.reg then
			frameoffset-:=roundsizetg(d.pcldef.size)
			d.offset:=frameoffset
			genmc(m_define, mgenname(getfullname(d)), mgenint(d.offset))
		elsif pcat[d.pcldef.mode]=d64cat then
			genmc(m_definereg, mgenname(getfullname(d)), mgenreg(d.reg))
		else
			genmc(m_definereg, mgenname(getfullname(d)), mgenxreg(d.reg))
		fi
	od

	if hasequiv then
		for i to nlocals do
			d:=localdefs[i]

			if d.atvar then
MERROR("D.ATVAR NOT READY")
!				d.offset:=d.equivvar.def.offset
!				if d.reg then merror("@ on reg var") fi
!				genmc(m_define, mgenname(getfullname(d)),
!					 mgenname(getfullname(d.equivvar.def)))
			fi
		od
	fi

	frameoffset-:=ndsaveregs*8			!non-vol reg spill area
	dsaveoffset:=frameoffset
	frameoffset-:=nxsaveregs*8
	xsaveoffset:=frameoffset

	framebytes:=-frameoffset

	if (nlocals or nparams) and procdef.isthreaded then
		merror("params/locals in threaded?")
	fi
	if framebytes then needstackframe:=1 fi	!may already be set

	while framebytes iand 15 do ++framebytes od	!multiple of 16

	if needstackframe and ndsavepush.odd then framebytes+:=8 fi	!alignment

	if needstackframe and not inf_leafproc then
		framebytes +:= 32
	fi

	if needstackframe and ndsavepush then needshadow48:=1 fi

!start to generate code
	if not needstackframe and not inf_leafproc and not procdef.isthreaded then
		needshadow32:=(ndsavepush.odd | 32 | 40)
	fi

!IF EQSTRING(PROCDEF.NAME,"start") then
!
!mgeninfos("***NAME***",PROCDEF.NAME)
!mgeninfo("NPARAMS",nparams)
!mgeninfo("NDSAVE",ndsaveregs)
!mgeninfo("NXSAVE",nxsaveregs)
!mgeninfo("NDPUSH",ndsavepush)
!mgeninfo("NSPILL",nspill)
!mgeninfo("NREGPARAMS",nregparams)
!mgeninfo("SHADOW48",needshadow48)
!mgeninfo("FRAMEBYTES",framebytes)
!mgeninfo("SHADOW32",needshadow32)
!mgeninfo("NEEDFRAME",needstackframe)
!mgeninfo("LEAFPROC",inf_leafproc)
!mgeninfo("ISTHREADED",procdef.isthreaded)
!mgeninfo("ASSEM USED",inf_assem)
!fi

	for i to ndsavepush do
		genmc(m_push, mgenreg(dsaveregs[i]))
	od

	if needshadow48 then			!create new shadow space to spill params
!MGENCOMMENT("NEED SHADOW SPACE")
		pushstack(48)
	fi

	if needstackframe then
		genmc(m_push, dframeopnd)
		genmc(m_mov, dframeopnd, dstackopnd)
		if framebytes then
			pushstack(framebytes)
		fi
	elsif needshadow32 then
!MGENCOMMENT("PUSH SHADOW32")
		pushstack(needshadow32)
	fi

!SAVE D3..D9
	offset:=dsaveoffset
	for i to ndsaveregs do
		genmc(m_mov, mgenindex(areg:rframe, size:8, offset:offset),
			mgenreg(dsaveregs[i]))
		offset+:=8
	od

	offset:=xsaveoffset
	for i to nxsaveregs do
		genmc(m_movq, mgenindex(areg:rframe, size:8, offset:offset),
			mgenxreg(xsaveregs[i]))
		offset+:=8
	od

	offset:=16
	regoffset:=0
	for i to nparams do
		if regoffset>3 then exit fi
		d:=paramdefs[i]
		IF NOT D.REG THEN
			ax:=mgenindex(areg:rframe, size:8, offset:offset)
!			ax:=mgenindex(areg:rframe, size:d.size, offset:offset)
			case pcat[d.pcldef.mode]
			when x64cat then
				genmc(m_movq, ax, mgenxreg(r0+regoffset))
			when x32cat then
				genmc(m_movd, changeopndsize(ax,4), mgenxreg(r0+regoffset))
!			when widecat then
!				genmc(m_mov, ax, mgenreg(regoffset+r10))
!				offset+:=8
!				++regoffset
!				genmc(m_mov, ax, mgenreg(regoffset+r10))
			else
				genmc(m_mov, ax, mgenreg(regoffset+r10))
			esac
		elsif d.reg then			!may use non-vol regs
			case pcat[d.pcldef.mode]
			when x64cat then
				if d.reg<>r0+regoffset then
					genmc(m_movq, mgenxreg(d.reg), mgenxreg(r0+regoffset))
				fi
!			when x32cat then
!				genmc(m_movd, ax, mgenxreg(r0+regoffset))
!			when widecat then
!				genmc(m_mov, ax, mgenreg(regoffset+r10))
!				offset+:=8
!				++regoffset
!				genmc(m_mov, ax, mgenreg(regoffset+r10))
			when d64cat then
				if d.reg<>r10+regoffset then
					genmc(m_mov, mgenreg(d.reg), mgenreg(regoffset+r10))
				fi
			esac

		fi
		offset+:=8
		++regoffset
	od
end

proc px_endproc(pcl p)=
	genmc(m_procend)
end

proc px_endprogram(pcl p)=
	unimpl(p)
end

proc px_extproc(pcl p)=
!	unimpl(p)
end

proc px_extparam(pcl p)=
!	unimpl(p)
end

proc px_extvariadics(pcl p)=
!	unimpl(p)
end

proc px_endextproc(pcl p)=
!	unimpl(p)
end

proc px_local(pcl p)=
	if nlocals>=maxlocals then merror("Too many locals") fi
	++nlocals
	case pcat[p.mode]
	when d64cat then
		++inf_proclocals
	when x64cat then
		++inf_procxlocals
	esac
	localdefs[nlocals]:=p.def
end

proc px_param(pcl p)=
	if nparams>=maxparams then merror("Too many params") fi
	++nparams
	paramdefs[nparams]:=p.def
	if pcat[p.mode]=widecat then
		++nparams
		paramdefs[nparams]:=p.def
	fi
end

proc px_label(pcl p)=
	genmc(m_label, mgenlabel(p.labelno))
end

proc px_labelname(pcl p)=
	unimpl(p)
end

proc px_push(pcl p)=
	case p.opndtype
	when mem_opnd then
		addmem(p)
	when memaddr_opnd then
		addmemaddr(p.def)
	when int_opnd then
		addint(p.value)
	when real_opnd then
		addreal(p.xvalue)
	when real32_opnd then
		addreal32(p.xvalue32)
	when string_opnd then
		addstring(p.svalue)
	when int128_opnd then
		addint128(cast(&p.value128))
	else
		merror("push",opndnames[p.opndtype])
	esac
end

proc px_pop(pcl p)=
	mcloperand ax,bx

	case p.opndtype
	when mem_opnd then
		case pcat[p.mode]
		when d64cat then
!load any defered memtypes
			for i to noperands do
				case pclstack[i].loc
				when stack_loc then		!should not be any more
					exit
				when mem_loc then
					if pclstack[i].def=p.def then
						genopnd_ld(i)
					fi
				esac
			od

			genmc(m_mov, mgenmem(p.def), genopnd_ld(xa))
		when x64cat then
			genmc(m_movq, mgenmem(p.def), genopnd_ld(xa))
		when x32cat then
			genmc(m_movd, mgenmem(p.def), genopnd_ld(xa))
		when widecat then
			genmc(m_mov, mgenmem(p.def), genopnd_ld(xa))
			if p.opcode<>kstore then delopnd() fi
			genmc(m_mov, mgenmemhigh(p.def), genopnd_ld(xa))
			if p.opcode<>kstore then delopnd() fi
			return
		when shortcat then
			genmc(m_mov, mgenmem(p.def), genopnd_ld(xa,p.size))

		when blockcat then
			bx:=genopnd_ind()
			addmemaddr(p.def)
			ax:=genopnd_ind()

			copyblock(ax,bx,p.size)

			delopnd()
		else
!			merrort("POPMEM",p.mode)
			CPL "POPMEM",p.mode
		esac
	else
		merroropnd("POP",p.opndtype)
	esac

	if p.opcode<>kstore then
		delopnd()
	fi
end

proc px_store(pcl p)=
	px_pop(p)
end

proc px_pushnc(pcl p)=
	unimpl(p)
end

proc px_opnd(pcl p)=
	unimpl(p)
end

proc px_type(pcl p)=
	unimpl(p)
end

proc px_pushptroff(pcl p)=
	mcloperand ax,bx,cx,fx
	int m

	m:=p.mode

	cx:=do_addrmode(p)

	if pclstack[2].loc<>reg_loc then
		pclstack[2].fmt:=reg_d64			!reg not needed to load addr, but
		pclstack[2].loc:=reg_loc			!need to prepare it for result
		pclstack[2].reg:=getnextreg()		!(although wasted for floats)
	fi
	ax:=genopnd(xb)

!here, ax is a suitable dest reg (not used for float dest), cx is the access mode

	case pcat[m]
	when d64cat then
		genmc(m_mov, ax, cx)

	when x64cat then
!need to turn ax into a float reg
		addreg_x64()
		swapopnds(1,3)
		fx:=genopnd(xc)

		genmc(m_movq, fx, cx)
		delopnd()

	when x32cat then
!need to turn ax into a float reg
		addreg_x32()
		swapopnds(1,3)
		fx:=genopnd(xc)

		genmc(m_movd, fx, changeopndsize(cx,4))
		delopnd()

	when shortcat then
		cx.size:=p.size
		genmc((psigned[m]|m_movsx|m_movzx), ax, cx)

	when widecat then
		bx:=genopnd_d64()
		swapopnds(1,2)
		swapopnds(2,3)
		genmc(m_mov, bx, applyoffset(cx,8,8))
		genmc(m_mov, ax, changeopndsize(cx,8))
		delopnd()
		setwideopnd()
		return
	when blockcat then
		genmc(m_lea, ax, cx)

	else
		merrort("pushptroff",m)
	esac	

	delopnd()
end

proc px_popptroff(pcl p)=
	mcloperand ax,bx,cx,px
	int m

	m:=p.mode

	px:=do_addrmode(p)
	cx:=genopnd_ld(xc)

	case pcat[m]
	when d64cat then
		genmc(m_mov, px,cx)

	when x64cat then
		genmc(m_movq, px,cx)

	when x32cat then
		genmc(m_movd, changeopndsize(px,4),cx)

	when shortcat then
		px.size:=p.size
		genmc(m_mov, px,changeopndsize(cx,p.size))

	when widecat then
		genmc(m_mov, changeopndsize(px,8),cx)
		genmc(m_mov, applyoffset(px,8,8),genopnd_ld(xc+1))

	when blockcat then
		copyblock(px,makeopndind(cx),p.size)

	else
		merrort("popptroff ",m)
	esac	

	delopnd()
	delopnd()
	if p.opcode=kpopptroff then
		delopnd()
		if pcat[m]=widecat then
			delopnd()
		fi
	fi
end

proc px_storeptroff(pcl p)=
	px_popptroff(p)
end

proc px_pushptr(pcl p)=
	mcloperand ax,px,cx,fx,bx
	int m

	m:=p.mode
	if isregvaropnd(xa) and pcat[m]<>blockcat then
		cx:=mgenireg(pclstack[1].reg)
		ax:=makeregopnd(xa)
	elsif pclstack[xa].fmt=imm_memaddr then
		cx:=mgenmem(pclstack[1].def)
		ax:=makeregopnd(1)
	else
		ax:=genopnd_ld()
		cx:=makeopndind(ax)
	fi

	case pcat[m]
	when d64cat then
		genmc(m_mov, ax, cx)

	when shortcat then
		genmc((psigned[m]|m_movsx|m_movzx), ax, changeopndsize(cx,p.size))

	when x64cat then
		addreg_x64()
		swapopnds(1,2)
		fx:=genopnd(xb)

		genmc(m_movq, fx, cx)
		delopnd()

	when x32cat then
		addreg_x32()
		swapopnds(1,2)
		fx:=genopnd(xb)

		genmc(m_movd, fx, changeopndsize(cx,4))
		delopnd()

	when widecat then
		bx:=genopnd_d64()
		genmc(m_mov, bx, cx)
		genmc(m_mov, ax, applyoffset(cx,8))
		setwideopnd()

	when blockcat then		!nothing further needed

	else

		MGENCOMMENT("****PUSHPTR")
!		merror("pushptr ",stdnames[m])
	esac	

end

proc px_popptr(pcl p)=
	mcloperand ax,bx,cx,px
	int m

	m:=p.mode
	bx:=genopnd_ld(xb)
	if isregvaropnd(xa) and pcat[m]<>blockcat then
		ax:=mgenireg(pclstack[1].reg)
	else
		ax:=genopnd_ind(ya)
	fi

	case pcat[m]
	when d64cat then
		genmc(m_mov, ax,bx)

	when shortcat then
		genmc(m_mov, changeopndsize(ax,psize[m]),changeopndsize(bx,psize[m]))

	when x64cat then
		genmc(m_movq, ax,bx)

	when x32cat then
		genmc(m_movd, changeopndsize(ax,4),bx)

	when widecat then
		genmc(m_mov, ax,bx)
		genmc(m_mov, applyoffset(ax,8),genopnd_ld(xb+1))

	when blockcat then
		copyblock(ax,makeopndind(bx),p.size)

	else
		merrort("popptr ",m)
	esac	

	delopnd()
	if p.opcode=kpopptr then
		delopnd()
		if pcat[m]=widecat then
			delopnd()
		fi
	fi

end

proc px_storeptr(pcl p)=
	px_popptr(p)
end

proc px_dotindex(pcl p)=
	mcloperand ax
	int i

	if pclstack[1].fmt<>imm_d64 then
		merror("dotix i not imm")
	fi

	ax:=genopnd_ld(xb)
	i:=pclstack[1].value

	if i then
		genmc(m_shr, ax, mgenint(i))
	fi
	genmc(m_andx, changeopndsize(ax,4), mgenint(1))

	delopnd()
end

proc px_popdotindex(pcl p)=
	mcloperand ax,bx,cx,rx,mx
	int i,size,cxfmt,rhs,axoffset

	if pclstack[3].fmt=imm_d64 then
		rhs:=pclstack[3].value
		cx:=nil
	else
		cx:=genopnd_ld(xc)
	fi

	if pclstack[1].fmt<>imm_d64 then
		merror("popdotix i not imm")
	fi
	i:=pclstack[1].value
	size:=p.size

	axoffset:=xb

	addreg_d64()
	rx:=genopnd()
	addreg_d64()
	mx:=genopnd()

!	if pclfmt[axindex]=imm_memaddr then
!		genmc(m_mov, mgenmem(pcldef[axindex]))
!	else
		ax:=genopnd_ind(axoffset+2,size:size)
		genmc((size=8|m_mov|m_movzx),rx,ax)
!	fi


!	genmc(m_mov,mx,mgenint(1<<i))
	genmc(m_mov,mx,mgenint(inot(1<<i)))
!	genmc(m_notx,mx)
	genmc(m_andx,rx,mx)

	if cx then
		if i then genmc(m_shl, cx, mgenint(i)) fi
		genmc(m_orx, rx, cx)
	elsif rhs<>0 then
		genmc(m_orx, rx, mgenint(1<<i))
	fi

!	if pclfmt[axindex]=imm_memaddr then
!		genmc(m_mov, mgenmem(pcldef[axindex]), rx)
!	else
		genmc(m_mov,ax,changeopndsize(rx,size))
!	fi

	delopnd()			!mx
	delopnd()			!rx
	delopnd()			!bx/index
	delopnd()			!addr
	if p.opcode=kpopdotindex then
		delopnd()		!value being stored
	fi
end

proc px_storedotindex(pcl p)=
	px_popdotindex(p)
end

proc px_dotslice(pcl p)=
	mcloperand ax,mx,mx4
	int i,j
	word mask

	if pclstack[yb].fmt<>imm_d64 or pclstack[za].fmt<>imm_d64 then
		merror("dotslice i/j not imm")
	fi

	ax:=genopnd_ld(xc)
	i:=pclstack[yb].value
	j:=pclstack[za].value

	if i then
		genmc(m_shr, ax, mgenint(i))
	fi

	mask:=inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
	if mask<=word(int32.maxvalue) then			!use immediate
		genmc(m_andx, ax, mgenint(mask))
	else
		mx:=makeregopnd(yb)
		genmc(m_mov, mx, mgenint(mask))
		genmc(m_andx, ax, mx)
	fi

	delopnd()
	delopnd()
end

proc px_popdotslice(pcl p)=
	mcloperand ax,rx,mx,mx4,dx
	int i,j,size
	word mask

	if pclstack[yb].fmt<>imm_d64 or pclstack[za].fmt<>imm_d64 then
		merror("popdotslice i/j not imm")
	fi

	dx:=genopnd_ld(wd)

	size:=p.size
	ax:=genopnd_ind(xc,size:size)

	i:=pclstack[yb].value
	j:=pclstack[za].value

	mx:=makeregopnd(yb)
	rx:=makeregopnd(za)

	loadtoreg(rx,ax,p.mode)

	mask:=inot((inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i)

!	mask:=inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
	genmc(m_mov, mx, mgenint(mask))

	if i then
		genmc(m_shl, dx, mgenint(i))
	fi

	genmc(m_andx, rx, mx)
	genmc(m_orx, rx, dx)

	storefromreg(ax,rx,size)

	delopnd()			!j
	delopnd()			!i
	delopnd()			!A
	if p.opcode=kpopdotslice then
		delopnd()		!x
	fi

end

proc px_storedotslice(pcl p)=
	px_popdotslice(p)
end

proc px_popstack(pcl p)=
	delopnd()
end

proc px_eval(pcl p)=
	if pclstack[1].wide='L' then
		genopnd_ld(ya)
		genopnd_ld(xb)
		delopnd()
		delopnd()
	else
		genopnd_ld(xa)
		delopnd()
	fi
end

proc px_callproc(pcl p)=
	int nslots, nargs:=callargs[ncalldepth]

	nslots:=do_pushparams(p,0)

	genmc(m_call, mgenmemaddr(p.def))

	to nargs do
		poparg()
	od
	popslots(nslots)
end

proc px_callprocptr(pcl p)=
	int nslots, nargs:=callargs[ncalldepth]
	nslots:=do_pushparams(p,1)

	genmc(m_call, genopnd_ld(xa))

	delopnd()			!the ptr

	to nargs do
		poparg()
	od
	popslots(nslots)
end

proc px_retproc(pcl p)=
	int offset

	offset:=dsaveoffset
	for i to ndsaveregs do
		genmc(m_mov, mgenreg(dsaveregs[i]),
			mgenindex(areg:rframe, size:8, offset:offset))
		offset+:=8
	od

	offset:=xsaveoffset
	for i to nxsaveregs do
		genmc(m_movq, mgenxreg(xsaveregs[i]),
			mgenindex(areg:rframe, size:8, offset:offset))
		offset+:=8
	od

!	if framebytes then
!		genmc(m_add, dstackopnd, mgenint(framebytes))
!	fi
!	if framebytes or parambytes then
!		genmc(m_pop, dframeopnd)
!	fi

	if needstackframe then
		if framebytes then
			genmc(m_add, dstackopnd, mgenint(framebytes))
		fi
		genmc(m_pop, dframeopnd)
	elsif needshadow32 then
		popstack(needshadow32)
	fi

	if needshadow48 then
		popstack(48)
	fi
	for i:=ndsavepush downto 1 do
		genmc(m_pop, mgenreg(dsaveregs[i]))
	od

!	if ndsavepush.odd then
!		genmc(m_pop, mgenreg(dsaveregs[1]))
!!		popstack(8)
!	fi
!
	genmc(m_ret)
end

proc px_callfn(pcl p)=
	px_callproc(p)

	dogetretvalue(p)
end

proc px_callfnptr(pcl p)=
	px_callprocptr(p)

	dogetretvalue(p)
end

proc px_retfn(pcl p)=
	px_retproc(p)
end

proc px_jump(pcl p)=
	genmc(m_jmp, mgenlabel(p.labelno))
end

proc px_jumpptr(pcl p)=
	unimpl(p)
end

proc px_jumpeq(pcl p)=
	dojumpcc(p)
end

proc px_jumpne(pcl p)=
	dojumpcc(p)
end

proc px_jumplt(pcl p)=
	dojumpcc(p)
end

proc px_jumple(pcl p)=
	dojumpcc(p)
end

proc px_jumpge(pcl p)=
	dojumpcc(p)
end

proc px_jumpgt(pcl p)=
	dojumpcc(p)
end

proc px_jumptrue(pcl p)=
	dojumptruefalse(p,nz_cond)
end

proc px_jumpfalse(pcl p)=
	dojumptruefalse(p,z_cond)
end

proc px_jumpinrange(pcl p)=
	mcloperand ax,bx,cx,lx,nolx
	int nolab

	ax:=genopnd_ld(xc)
	bx:=genopnd(yb)
	cx:=genopnd(za)

	lx:=mgenlabel(p.labelno)

	genmc(m_cmp, ax,bx)

	nolx:=mgenlabel(nolab:=mcreatefwdlabel())
	genmc_cond(m_jmpcc, (psigned[p.mode]|lt_cond|ltu_cond),nolx)
	genmc(m_cmp, ax, cx)
	genmc_cond(m_jmpcc, (psigned[p.mode]|le_cond|leu_cond),lx)
	mdefinefwdlabel(nolab)

	delopnd()
	delopnd()
	delopnd()
end

proc px_jumpnotinrange(pcl p)=
	mcloperand ax,bx,cx,lx

	ax:=genopnd_ld(xc)
	bx:=genopnd(yb)
	cx:=genopnd(za)

	lx:=mgenlabel(p.labelno)

	genmc(m_cmp, ax,bx)

	genmc_cond(m_jmpcc, (psigned[p.mode]|lt_cond|ltu_cond),lx)
	genmc(m_cmp, ax, cx)
	genmc_cond(m_jmpcc, (psigned[p.mode]|gt_cond|gtu_cond),lx)

	delopnd()
	delopnd()
	delopnd()
end

proc px_setjumpeq(pcl p)=
	genmc(m_cmp,genopnd_ld(xb),genopnd(ya))

	genmc_cond(m_jmpcc, eq_cond, mgenlabel(p.labelno))
	delopnd()
end

proc px_setjumpeqx(pcl p)=
	genmc(m_cmp,genopnd_ld(xb),genopnd(ya))

	genmc_cond(m_jmpcc, eq_cond, mgenlabel(p.labelno))
	delopnd()
	delopnd()
end

proc px_setjumpne(pcl p)=
	genmc(m_cmp,genopnd(xb),genopnd(ya))

	genmc_cond(m_jmpcc, ne_cond, mgenlabel(p.labelno))
	delopnd()
	delopnd()
end

proc px_seteq(pcl p)=
	dosetcc(p)
end

proc px_setne(pcl p)=
	dosetcc(p)
end

proc px_setlt(pcl p)=
	dosetcc(p)
end

proc px_setle(pcl p)=
	dosetcc(p)
end

proc px_setge(pcl p)=
	dosetcc(p)
end

proc px_setgt(pcl p)=
	dosetcc(p)
end

proc px_casejumpeq(pcl p)=
	genmc(m_cmp, genopnd_ld(xb), genopnd(ya))
	genmc_cond(m_jmpcc, eq_cond, mgenlabel(p.labelno))
	delopnd()
end

proc px_selecteq(pcl p)=
	doselectcc(p)
end

proc px_selectne(pcl p)=
	doselectcc(p)
end

proc px_selectlt(pcl p)=
	doselectcc(p)
end

proc px_selectle(pcl p)=
	doselectcc(p)
end

proc px_selectge(pcl p)=
	doselectcc(p)
end

proc px_selectgt(pcl p)=
	doselectcc(p)
end

proc px_selecttrue(pcl p)=
	mcloperand ax,bx,cx,dx

	if pcat[p.mode]<>d64cat then merrort("selecttrue",p.mode) fi

	ax:=genopnd_ld(xa)
!	genmc(m_andx, ax,ax)
	genmc(m_test, ax,ax)

	noxorclear:=1
	dx:=genopnd_ld(yb)
	cx:=genopnd_ld(xc)
!	dx:=genopnd_ld(xc)
!	cx:=genopnd_ld(yb)
	noxorclear:=0
	genmc_cond(m_cmovcc, z_cond, cx,dx)

	delopnd()
	delopnd()
end

proc px_to(pcl p)=
	pcl q
	mcloperand ax

	q:=currpcl:=p+1

	ax:=mgenmem(q.def)
	genmc(m_dec, ax)
	genmc_cond(m_jmpcc, nz_cond, mgenlabel(p.labelno))
end

proc px_forup(pcl p)=
	do_for(p, m_inc, m_add, le_cond)
end

proc px_fordown(pcl p)=
	do_for(p, m_dec, m_sub, ge_cond)
end

proc px_swap(pcl p)=
	mcloperand ax,bx

	mcloperand px:=genopnd_ind(xb,p.size)
	mcloperand qx:=genopnd_ind(ya,p.size)

	ax:=mgenreg(getnextreg(),p.size)
	bx:=mgenreg(getnextreg(),p.size)

	case pcat[p.mode]
	when d64cat,shortcat then
		genmc(m_mov, ax, px)
		genmc(m_mov, bx, qx)
		genmc(m_mov, qx, ax)
		genmc(m_mov, px, bx)

	else
		merrort("swap",p.mode)
	esac

	freereg(ax.reg)
	freereg(bx.reg)

	delopnd()
	delopnd()
end

proc px_makeslice(pcl p)=
	setwideopnd()
end

proc px_switch(pcl p)=
	int minlab, maxlab, jumplab, elselab
	mcloperand ax

	minlab:=p.minlab
	maxlab:=p.maxlab
	jumplab:=p.labelno
	currpcl:=p+1
	elselab:=currpcl.labelno

	ax:=genopnd_ld(xa)
	if minlab<>0 then
		genmc(m_sub,ax,mgenint(minlab))
	fi
	genmc(m_cmp,ax,mgenint(maxlab-minlab+1))
	genmc_cond(m_jmpcc,geu_cond,mgenlabel(elselab))
	genmc(m_jmp, mgenindex(ireg:ax.reg,scale:8,labno:jumplab))

	delopnd()

	setsegment('I')
end

proc px_switchlabel(pcl p)=
	genmc(m_dq, mgenlabel(p.labelno))
end

proc px_endswitch(pcl p)=
	setsegment('C')
end

proc px_clear(pcl p)=
	mcloperand ax

	ax:=genopnd_ind()
	clearblock(ax,p.size)
	delopnd()
end

proc px_csegment(pcl p)=
	unimpl(p)
end

proc px_isegment(pcl p)=
	unimpl(p)
end

proc px_zsegment(pcl p)=
	unimpl(p)
end

proc px_rosegment(pcl p)=
	unimpl(p)
end

proc px_data(pcl p)=
	mcloperand ax
	int opc

	case p.opndtype
	when int_opnd then
		ax:=mgenint(p.value)
	when real_opnd,real32_opnd then
		ax:=mgenrealimm(p.xvalue,p.size)
	when string_opnd then
		 ax:=mgenlabel(getstringindex(p.svalue))

	when memaddr_opnd then
		ax:=mgenmemaddr(p.def)
		ax.offset:=p.extra
	else
		merror("db/dq optype? #", opndnames[p.opndtype])
	esac

	case p.size
	when 1 then opc:=m_db
	when 2 then opc:=m_dw
	when 4 then opc:=m_dd
	when 8 then opc:=m_dq
	else
		merror("DATA/not 1248")
	esac

	genmc(opc,ax)
end

proc px_db(pcl p)=
	unimpl(p)
end

proc px_dw(pcl p)=
	unimpl(p)
end

proc px_dd(pcl p)=
	unimpl(p)
end

proc px_dq(pcl p)=
	unimpl(p)
end

proc px_dstring(pcl p)=
	unimpl(p)
end

proc px_dstringz(pcl p)=
	unimpl(p)
end

proc px_reserve(pcl p)=
	unimpl(p)
end

proc px_assem(pcl p)=
	unimpl(p)
end

proc px_add(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xb)
		if pclstack[1].fmt=imm_d64 and pclstack[1].value=1 then
			genmc(m_inc, ax)
		else
			bx:=genopnd(ya)
			genmc(m_add,ax,bx)
		fi
	when x64cat then
		dobin_float(m_addsd)
	when x32cat then
		dobin_float(m_addss)
	when widecat then
		dobin_i128(p,m_add, m_adc)
		return
	else
		merrort("add:",p.mode)
	esac
	delopnd()
end

proc px_sub(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xb)
		if pclstack[1].fmt=imm_d64 and pclstack[1].value=1 then
			genmc(m_dec, ax)
		else
			bx:=genopnd(ya)
			genmc(m_sub,ax,bx)
		fi
	when x64cat then
		dobin_float(m_subsd)
	when x32cat then
		dobin_float(m_subss)
	when widecat then
		dobin_i128(p,m_sub, m_sbb)
		return
	else
		merrort("sub:",p.mode)
	esac
	delopnd()
end

proc px_mul(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xb)
		if  pclstack[1].fmt=imm_d64 then
			mulimm(ax,pclstack[1].value)
			delopnd()
			return
		fi

		bx:=genopnd(ya)
		genmc(m_imul2,ax,bx)

	when x64cat then
		dobin_float(m_mulsd)
	when x32cat then
		dobin_float(m_mulss)
	when widecat then
		do_syscall(rts_mul_i128,4,widecat)
		return
	else
		merrort("mul:",p.mode)
	esac
	delopnd()
end

proc px_div(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when x64cat then
		dobin_float(m_divsd)
	when x32cat then
		dobin_float(m_divss)
!	when widecat then
	else
		merrort("div:",p.mode)
	esac
	delopnd()
end

proc px_idiv(pcl p)=
	case p.mode
	when tpi64 then
		do_divrem(p, issigned:1, isdiv:1)
	when tpu64 then
		do_divrem(p, issigned:0, isdiv:1)
	when tpi128 then
		do_syscall(rts_div_i128,4,widecat)
	when tpu128 then
!		do_syscall(rts_div_u128,4,widecat)
		do_syscall(rts_div_i128,4,widecat)
	else
		merrort("idiv:",p.mode)
	esac
end

proc px_irem(pcl p)=
	case p.mode
	when tpi64 then
		do_divrem(p, issigned:1, isdiv:0)
	when tpu64 then
		do_divrem(p, issigned:0, isdiv:0)
	else
		merrort("irem:",p.mode)
	esac
end

proc px_iand(pcl p)=
	dobitwise(p, m_andx)
end

proc px_ior(pcl p)=
	dobitwise(p, m_orx)
end

proc px_ixor(pcl p)=
	dobitwise(p, m_xorx)
end

proc px_shl(pcl p)=
	case pcat[p.mode]
	when d64cat then
		do_shift(p,m_shl)
	else
		merrort("shl:",p.mode)
	esac
end

proc px_shr(pcl p)=
	case p.mode
	when tpi64 then
		do_shift(p,m_sar)
	when tpu64 then
		do_shift(p,m_shr)
	else
		merrort("shr:",p.mode)
	esac
end

proc px_in(pcl p)=
	unimpl(p)
end

proc px_notin(pcl p)=
	unimpl(p)
end

proc px_min(pcl p)=
	case p.mode
	when tpi64 then
		domax_int(gt_cond)
	when tpu64 then
		domax_int(gtu_cond)
	when tpr64 then
		domax_float(m_minsd)
	when tpr32 then
		domax_float(m_minss)
	else
		merrort("min:",p.mode)
	esac
end

proc px_max(pcl p)=
	case p.mode
	when tpi64 then
		domax_int(lt_cond)
	when tpu64 then
		domax_int(ltu_cond)
	when tpr64 then
		domax_float(m_maxsd)
	when tpr32 then
		domax_float(m_maxss)
	else
		merrort("max:",p.mode)
	esac
end

proc px_eq(pcl p)=
	unimpl(p)
end

proc px_ne(pcl p)=
	unimpl(p)
end

proc px_lt(pcl p)=
	unimpl(p)
end

proc px_le(pcl p)=
	unimpl(p)
end

proc px_ge(pcl p)=
	unimpl(p)
end

proc px_gt(pcl p)=
	unimpl(p)
end

proc px_same(pcl p)=
	unimpl(p)
end

proc px_andl(pcl p)=
	unimpl(p)
end

proc px_orl(pcl p)=
	unimpl(p)
end

proc px_addrefoff(pcl p)=
	mcloperand ax,cx

	cx:=do_addrmode(p)

	if pclstack[2].loc<>reg_loc then
		pclstack[2].fmt:=reg_d64			!reg not needed to load addr, but
		pclstack[2].loc:=reg_loc			!need to prepare it for result
		pclstack[2].reg:=getnextreg()		!(although wasted for floats)
	fi
	ax:=genopnd(xb)

	genmc(m_lea, ax, cx)
	delopnd()
end

proc px_subref(pcl p)=
	mcloperand ax,bx
	int n

	ax:=genopnd_ld(xb)
!	bx:=genopnd_ld(ya)
	bx:=genopnd(ya)
	genmc(m_sub,ax,bx)

	if p.scale>1 then
		n:=ispoweroftwo(p.scale)
		if n then
			genmc(m_shr, ax, mgenint(n))
		else
			MERROR("SUB/REF NOT POWER OF TWO")
		fi
	fi

	delopnd()
end

proc px_subrefoff(pcl p)=
	int scale, extra, offset
	mcloperand ax,bx

	scale:=p.scale
	extra:=p.extra

	ax:=genopnd_ld(xb)

	if pclstack[1].fmt=imm_d64 then
		genmc(m_sub, ax, mgenint(pclstack[1].value*scale+extra))
	else
		bx:=genopnd_ld(xa)
		scale:=scaleindex(bx,scale)
		if scale>1 then
			mulimm(bx,scale)
		fi
		genmc(m_sub, ax, bx)
		if extra then
MERROR("SUBREF/EXTRA")
!			genmc(m_add, ax, mgenint(extra))
		fi
	fi
	delopnd()
end

proc px_neg(pcl p)=
	mcloperand ax

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_neg,ax)

	when x64cat then
		if not labneg64 then labneg64:=mcreatefwdlabel() fi
		genmc(m_xorpd,genopnd_ld(xa),mgenlabelmem(labneg64))
	when x32cat then
		if not labneg32 then labneg32:=mcreatefwdlabel() fi
		genmc(m_xorps,genopnd_ld(xa),mgenlabelmem(labneg32))

	when widecat then
		mcloperand axlow, axhigh, bxlow, bxhigh
		axlow:=genopnd_ld()
		axhigh:=genopnd_ld(2)

		bxhigh:=genopnd_d64()
		bxlow:=genopnd_d64()

		genmc(m_xorx,bxlow,bxlow)
		genmc(m_xorx,bxhigh,bxhigh)
		genmc(m_sub,bxlow,axlow)
		genmc(m_sbb,bxhigh,axhigh)

		swapopnds(1,3)
		swapopnds(2,4)

		delopnd()
		delopnd()
	else
		merrort("neg",p.mode)
	esac
end

proc px_abs(pcl p)=
	mcloperand ax,lx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_cmp, ax, mgenint(0))

		genmc_cond(m_jmpcc, ge_cond, lx:=mgenlabel(++mlabelno))
		genmc(m_neg,ax)
		genmc(m_label, lx)

!
	when x64cat then
		if not labneg64 then labneg64:=mcreatefwdlabel() fi
		genmc(m_xorpd,genopnd_ld(xa),mgenlabelmem(labneg64))
	when x32cat then
		if not labneg32 then labneg32:=mcreatefwdlabel() fi
		genmc(m_xorps,genopnd_ld(xa),mgenlabelmem(labneg32))
	else
		merrort("abs",p.mode)
	esac
end

proc px_inot(pcl p)=
	mcloperand ax

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_notx,ax)

	else
		merrort("inot",p.mode)
	esac
end

proc px_notl(pcl p)=
	mcloperand ax

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_xorx,ax,mgenint(1))

	else
		merrort("notl",p.mode)
	esac
end

proc px_istruel(pcl p)=
	mcloperand ax, bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_test, ax,ax)
		genmc_cond(m_setcc, ne_cond, bx:=changeopndsize(ax,1))
		genmc(m_movzx, changeopndsize(ax,4),bx)

	else
		merrort("istruel",p.mode)
	esac
end

proc px_sqr(pcl p)=
	mcloperand ax

	ax:=genopnd_ld(xa)

	case pcat[p.mode]
	when d64cat then
		genmc(m_imul2,ax,ax)
!
	when x64cat then
		genmc(m_mulsd,ax,ax)
	when x32cat then
		genmc(m_mulss,ax,ax)
	else
		merrort("sqr",p.mode)
	esac
end

proc px_sqrt(pcl p)=
	mcloperand ax

	ax:=genopnd_ld(xa)

	case pcat[p.mode]
	when x64cat then
		genmc(m_sqrtsd,ax,ax)
	when x32cat then
		genmc(m_sqrtss,ax,ax)
	else
		merrort("sqrt",p.mode)
	esac
end

proc px_sin(pcl p)=
	domaths(p,"sin*")
end

proc px_cos(pcl p)=
	domaths(p,"cos*")
end

proc px_tan(pcl p)=
	domaths(p,"tan*")
end

proc px_asin(pcl p)=
	domaths(p,"asin*")
end

proc px_acos(pcl p)=
	domaths(p,"acos*")
end

proc px_atan(pcl p)=
	domaths(p,"atan*")
end

proc px_ln(pcl p)=
	domaths(p,"log*")
end

proc px_log(pcl p)=
	domaths(p,"log10*")
end

proc px_exp(pcl p)=
	domaths(p,"exp*")
end

proc px_round(pcl p)=
	domaths(p,"round*")
end

proc px_floor(pcl p)=
	domaths(p,"floor*")
end

proc px_ceil(pcl p)=
	domaths(p,"ceil*")
end

proc px_fract(pcl p)=
	unimpl(p)
end

proc px_sign(pcl p)=
	unimpl(p)
end

proc px_atan2(pcl p)=
	unimpl(p)
end

proc px_power(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then
		swapopnds(1,2)
		do_syscall(rts_power_i64,2,d64cat)

!	when x64cat then
!		dobin_float(m_mulsd)
!	when x32cat then
!		dobin_float(m_mulss)
!	when widecat then
!!		do_syscall(rts_mul_i128,4,widecat)
!!		delopnd()
!!		delopnd()
		return
	else
		merrort("power:",p.mode)
	esac
	delopnd()
end

proc px_fmod(pcl p)=
	unimpl(p)
end

proc px_incr(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doincr(p,m_inc, m_add)
	else
		merrort("incr", p.mode)
	esac
end

proc px_decr(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doincr(p,m_dec, m_sub)
	else
		merrort("decr", p.mode)
	esac
end

proc px_incrload(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doincrload(p,m_inc, m_add)
	else
		merrort("incrload", p.mode)
	esac
end

proc px_decrload(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doincrload(p,m_dec, m_sub)
	else
		merrort("decrload", p.mode)
	esac
end

proc px_loadincr(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doloadincr(p,m_inc, m_add)
	else
		merrort("loadincr", p.mode)
	esac
end

proc px_loaddecr(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doloadincr(p,m_dec, m_sub)
	else
		merrort("loaddecr", p.mode)
	esac
end

proc px_addto(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat,shortcat then
		dobinto_int(p,m_add)
	when x64cat then
		dobinto_float(p,m_addsd)
	when x32cat then
		dobinto_float32(p,m_addss)
!	when shortcat then

!	when widecat then
	else
		merrort("addto:",p.mode)
	esac
end

proc px_subto(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat,shortcat then
		dobinto_int(p,m_sub)
	when x64cat then
		dobinto_float(p,m_subsd)
	when x32cat then
		dobinto_float32(p,m_subss)
!	when widecat then
	else
		merrort("subto:",p.mode)
	esac
end

proc px_multo(pcl p)=
	mcloperand ax,bx,cx

	case pcat[p.mode]
	when d64cat then
		addreg_d64()
		ax:=genopnd_ind(xc)
		bx:=genopnd(yb)
		cx:=genopnd(za)

		genmc(m_mov, cx,ax)

		if  pclstack[2].fmt=imm_d64 then
			mulimm(cx, pclstack[2].value)
		else
			genmc(m_imul2, cx,bx)
		fi
		genmc(m_mov, ax,cx)

		delopnd()
		delopnd()
		delopnd()
	when x64cat then
		dobinto_float(p,m_mulsd)
	when x32cat then
		dobinto_float32(p,m_mulss)
!	when widecat then
	else
		merrort("multo:",p.mode)
	esac
end

proc px_divto(pcl p)=
	mcloperand ax,bx,cx

	case pcat[p.mode]
	when x64cat then
		dobinto_float(p,m_divsd)
	when x32cat then
		dobinto_float32(p,m_divss)
!	when widecat then
	else
		merrort("divto:",p.mode)
	esac
end

proc px_idivto(pcl p)=
	unimpl(p)
end

proc px_iremto(pcl p)=
	unimpl(p)
end

proc px_iandto(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat,shortcat then
		dobinto_int(p,m_andx)
!	when widecat then
	else
		merrort("iandto:",p.mode)
	esac
end

proc px_iorto(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat,shortcat then
		dobinto_int(p,m_orx)
!	when widecat then
	else
		merrort("iorto:",p.mode)
	esac
end

proc px_ixorto(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat,shortcat then
		dobinto_int(p,m_xorx)
!	when widecat then
	else
		merrort("ixorto:",p.mode)
	esac
end

proc px_shlto(pcl p)=
	case p.mode
	when tpi64,tpu64 then
		do_shiftnto(p,m_shl)
	else
		merrort("shlto:",p.mode)
	esac
end

proc px_shrto(pcl p)=
	case p.mode
	when tpi64 then
		do_shiftnto(p,m_sar)
	when tpu64 then
		do_shiftnto(p,m_shr)
	else
		merrort("shrto:",p.mode)
	esac
end

proc px_minto(pcl p)=
	case p.mode
	when tpi64 then
		domaxto_int(le_cond)
	when tpu64 then
		domaxto_int(leu_cond)
	when tpr64 then
		domaxto_r64(leu_cond)
	when tpr32 then
		domaxto_r32(leu_cond)
!	when tpi128 then
	else
		merrort("minto:",p.mode)
	esac
end

proc px_maxto(pcl p)=
	case p.mode
	when tpi64 then
		domaxto_int(ge_cond)
	when tpu64 then
		domaxto_int(geu_cond)
	when tpr64 then
		domaxto_r64(geu_cond)
	when tpr32 then
		domaxto_r32(geu_cond)
!	when tpi128 then
	else
		merrort("maxto:",p.mode)
	esac
end

proc px_andlto(pcl p)=
	unimpl(p)
end

proc px_orlto(pcl p)=
	unimpl(p)
end

proc px_addrefoffto(pcl p)=
	int scale, extra,offset
!
	scale:=p.scale
	extra:=p.extra
	offset:=pclstack[1].value*scale+extra	!in case imm_d64

	mcloperand ax,bx,rx
	int reg,size

	if ismemaddr(xb) then
		ax:=mgenmem(pclstack[2].def)
		reg:=getnextreg()
		rx:=mgenreg(reg)

		genmc(m_mov, rx, ax)

		if pclstack[1].fmt=imm_d64 then
			genmc(m_add,rx,mgenint(offset))
		else
			bx:=genopnd_ld(ya)
			mulimm(bx,scale)
			genmc(m_add,rx,bx)
		fi

		genmc(m_mov, ax,rx)
		freereg(reg)
	else
		ax:=genopnd_ind(xb)
		if pclstack[1].fmt=imm_d64 then
			genmc(m_add,ax,mgenint(offset))
		else
			bx:=genopnd_ld(ya)
			mulimm(bx,scale)
			genmc(m_add,ax,bx)
		fi
	fi
	delopnd()
	delopnd()
end

proc px_subrefoffto(pcl p)=
	int scale, extra
	mcloperand ax,bx

	scale:=p.scale
	extra:=p.extra

	ax:=genopnd_ind(xb)

	if pclstack[1].fmt=imm_d64 then
		genmc(m_sub, ax, mgenint(pclstack[1].value*scale+extra))
	else
		bx:=genopnd_ld(xa)
		scale:=scaleindex(bx,scale)
		if scale>1 then
			mulimm(bx,scale)
		fi
		genmc(m_sub, ax, bx)
		if extra then
MERROR("SUBTOREF/EXTRA")
!			genmc(m_sub, ax, mgenint(extra))
		fi
	fi

	delopnd()
	delopnd()
end

proc px_negto(pcl p)=
	unimpl(p)
end

proc px_absto(pcl p)=
	unimpl(p)
end

proc px_inotto(pcl p)=
	unimpl(p)
end

proc px_notlto(pcl p)=
	unimpl(p)
end

proc px_istruelto(pcl p)=
	unimpl(p)
end

proc px_typepun(pcl p)=
	mcloperand ax,bx,cx

	bx:=genopnd_ld(xa)

	case pcat[p.mode]
	when d64cat then
		case pclstack[1].fmt
		when xreg_x64 then
			addreg_d64()
			ax:=genopnd(xa)
            genmc(m_movq,ax,bx)
			swapopnds(1,2)
			delopnd()
		when reg_d64 then
		else
			goto error
		esac

	when x64cat then
		case pclstack[1].fmt
		when reg_d64 then
			addreg_x64()
			ax:=genopnd(xa)
            genmc(m_movq,ax,bx)
			swapopnds(1,2)
			delopnd()
		else
			goto error
		esac
	when shortcat then
		case pclstack[1].fmt
		when xreg_x32 then
			addreg_d64()
			ax:=genopnd(xa)
			cx:=changeopndsize(ax,4)
            genmc(m_movd, cx,bx)
			swapopnds(1,2)
			delopnd()

			genmc((psigned[p.mode]|m_movsx|m_movzx),ax,cx)
		when reg_d64 then

		else
			goto error
		esac

	else
error::
CPL =PSTDNAMES[P.MODE]
CPL =FMTNAMES[PCLSTACK[1].FMT]
CPL "TYPEPUN"
!MGENCOMMENT("TYPEPUN")
		MERROR("TYPEPUN")
	esac
end

proc px_softconv(pcl p)=
	unimpl(p)
end

proc px_widen(pcl p)=
	unimpl(p)
end

proc px_narrow(pcl p)=
	unimpl(p)
end

proc px_float(pcl p)=
	mcloperand ax,fx
	int lab,lab2

	ax:=genopnd_ld(xa)

	case p.oldmode
	when tpi64 then
		addreg_x64()
		fx:=genopnd(xa)
		genmc(m_cvtsi2sd, fx, ax)
		swapopnds(1,2)
	when tpu64 then
		addreg_x64()
		fx:=genopnd(xa)

		lab:=mcreatefwdlabel()
		lab2:=mcreatefwdlabel()
		genmc(m_cmp, ax, mgenint(0))
		genmc_cond(m_jmpcc, lt_cond, mgenlabel(lab))
		genmc(m_cvtsi2sd, fx, ax)
		genmc(m_jmp, mgenlabel(lab2))

		mdefinefwdlabel(lab)
		if not labmask63 then
			labmask63:=++mlabelno
			laboffset64:=++mlabelno
		fi
		genmc(m_andx,ax, mgenlabelmem(labmask63))
		genmc(m_cvtsi2sd, fx, ax)
		genmc(m_addsd, fx, mgenlabelmem(laboffset64))
		mdefinefwdlabel(lab2)
		swapopnds(1,2)

	else
		merrort("float",p.mode)
	esac

	delopnd()
end

proc px_fix(pcl p)=
	mcloperand fx,ax
	int newmode:=p.mode, oldmode:=p.oldmode

	case pcat[newmode]
	when d64cat then
		fx:=genopnd_ld(xa)
		addreg_d64()
		ax:=genopnd(xa)
		genmc((oldmode=tpr64|m_cvttsd2si|m_cvttss2si), ax, fx)
		swapopnds(1,2)
		delopnd()

	else
		merrort("fix->",newmode)
	esac
end

proc px_truncate(pcl p)=
	mcloperand ax
	int mask

	case psize[p.truncmode]
	when 1 then mask:=255
	when 2 then mask:=65535
	when 4 then mask:=0xFFFF'FFFF
	esac

	ax:=genopnd_ld(xa)
	genmc(m_andx, ax, mgenint(mask))

	genmc((psigned[p.truncmode]|m_movsx|m_movzx), ax, changeopndsize(ax,psize[p.truncmode]))
end

proc px_fwiden(pcl p)=
	mcloperand fx
	fx:=genopnd_ld()
	genmc(m_cvtss2sd, fx,fx)
	pclstack[1].fmt:=xreg_x64
end

proc px_fnarrow(pcl p)=
	mcloperand ax:=genopnd_ld(xa)
	genmc(m_cvtsd2ss, ax,ax)
	pclstack[1].fmt:=xreg_x32
end

proc px_softtruncw(pcl p)=
	mcloperand ax,bx,bx2,lx

	ax:=genopnd_ld()
	swapopnds(1,2)
	delopnd()
	unsetwideopnd()
end

proc px_widenw(pcl p)=
	mcloperand ax,bx,bx2,lx

	ax:=genopnd_ld()
	bx:=genopnd_d64()
	bx2:=changeopndsize(bx,4)

	swapopnds(1,2)

	if psigned[p.mode] then
		genmc(m_xorx,bx2,bx2)
		genmc(m_cmp,ax,mgenint(0))
		genmc_cond(m_jmpcc, ge_cond, lx:=mgenlabel(++mlabelno))
		genmc(m_notx, bx)
		genmc(m_label,lx)
	else
		genmc(m_xorx,bx2,bx2)
	fi

	setwideopnd()
end

proc px_len(pcl p)=
	delopnd()
	unsetwideopnd()
end

proc px_lwb(pcl p)=
	unimpl(p)
end

proc px_upb(pcl p)=
	unimpl(p)
end

proc px_bounds(pcl p)=
	unimpl(p)
end

proc px_lenstr(pcl p)=
	unimpl(p)
end

proc px_bitwidth(pcl p)=
	unimpl(p)
end

proc px_bytesize(pcl p)=
	unimpl(p)
end

proc px_bytes(pcl p)=
	unimpl(p)
end

proc px_minvalue(pcl p)=
	unimpl(p)
end

proc px_maxvalue(pcl p)=
	unimpl(p)
end

proc px_typestr(pcl p)=
	unimpl(p)
end

proc px_error(pcl p)=
	unimpl(p)
end

proc px_arraytoslice(pcl p)=
	unimpl(p)
end

proc px_ichartoslice(pcl p)=
	unimpl(p)
end

proc px_softtruncshort(pcl p)=
	unimpl(p)
end

proc px_charaxtoichar(pcl p)=
	unimpl(p)
end

proc px_sliceptr(pcl p)=
	swapopnds(1,2)
	delopnd()
	unsetwideopnd()
end

proc px_startmult(pcl p)=
	pushallopnds()
end

proc px_resetmult(pcl p)=
	if pclstack[1].float then
		MERROR("RESETMULT/XREG")
	else
		movetoreg(r0)
	fi

	if p.opcode=kresetmult then
		delopnd()
	fi
end

proc px_endmult(pcl p)=
	px_resetmult(p)
end

proc px_setret(pcl p)=
	do_setret(r0,r0)

	regset[r0]:=0
	xregset[r0]:=0
end

proc px_setretmult(pcl p)=
	int k,wide

	k:=0

	for i:=1 to p.nret do
		++k
		wide:=pclstack[1].wide
		do_setret(multregs[k],multxregs[k])
!		if wide then
!			++k
!			do_setret(multregs[k],multxregs[k])
!		fi
	od

	for i:=1 to k do
		regset[multregs[i]]:=xregset[multxregs[i]]:=0
	od
end

proc px_setargs(pcl p)=
	int nslots,shadow,align,nargs,opcode,nvars

	if p then
		nargs:=p.nargs
		nvars:=p.nvariadics
		opcode:=p.opcode
	else
		nargs:=sa_nargs		!set via global
		nvars:=0
		opcode:=0
	fi

	nslots:=0			!total slots to be recovered after a call
	shadow:=0			!whether 4-slot shadow space to be created
	align:=0			!whether stack alignment fix needed

	case opcode
	when ksetargs then
		saveallopnds()		!get latest mstackdepth
	else
		saveallopnds(nargs+1)
	esac

	if nargs<=4 then					!no pushed args needed
		if mstackdepth=0 then
		else
			shadow:=1
			align:=mstackdepth.odd
			nslots:=4
		fi
	else								!some pushed params
		shadow:=1
		nslots:=nargs
		align:=(mstackdepth+nslots).odd
	fi

	nslots+:=align
	if align then
		if opcode=ksetargs then		!normal
			pushslots(1)
			align:=0
		fi								!else leave to be stored in callalign
	fi

	if ncalldepth>=maxcalldepth then
		merror("Too many nested calls")
	fi
	++ncalldepth
	IF NCALLDEPTH<1 THEN
		ABORTPROGRAM("CALLDEPTH?")
	FI

	callargs[ncalldepth]:=nargs
	callnvars[ncalldepth]:=nvars
	callslots[ncalldepth]:=nslots
	callshadow[ncalldepth]:=shadow
	callalign[ncalldepth]:=align
end

proc px_duplstack(pcl p)=
	mcloperand ax

	if pcat[p.mode]>=widecat then
		merrort("duplstack",p.mode)
	fi

	duploperand()
end

proc px_swapstack(pcl p)=
	swapopnds(1,p.x+1)		!the x-attr is 0-based; need 1-based
end

proc px_getnprocs(pcl p)=
	dosetfntable()
	addlabel(lab_funcnprocs)
end

proc px_getprocname(pcl p)=
	mcloperand ax

	dosetfntable()
	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_mov, ax, mgenindex(ireg:ax.reg,scale:8,offset:-8,labno:lab_funcnametable))
	else
		merrort("gpn",p.mode)
	esac
end

proc px_getprocaddr(pcl p)=
	mcloperand ax

	dosetfntable()
	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_mov, ax, mgenindex(ireg:ax.reg,scale:8,offset:-8,labno:lab_funcaddrtable))
	else
		merrort("gpa",p.mode)
	esac
end

proc px_last(pcl p)=
	unimpl(p)
end

proc dobin_float(int opc)=
	mcloperand ax,bx

	ax:=genopnd_ld(xb)
	bx:=genopnd(ya)

	genmc(opc,ax,bx)
end

function do_pushparams(pcl p, int isptr)int=
!isptr=1 when top pcl operand is the function pointer for indirect calls

	int pushedslots, nparams, nvars, nargs

	if p then
		nargs:=callargs[ncalldepth]
		nvars:=callnvars[ncalldepth]
	else
		nargs:=sa_nargs; isptr:=0
		nvars:=0
	fi

!CPL "DOPUSHPARAMS", =NCALLDEPTH, =NARGS, =CALLSLOTS[NCALLDEPTH],=CALLARGS[NCALLDEPTH]


	if nargs>inf_maxargs and nargs<=4 then inf_maxargs:=nargs fi
	nparams:=nargs

	if nparams>4 then
		pushallopnds(isptr+4+1)
	fi

!low params are 'pushed' after high params
!this allows r13 to be used as a scratch register

	do_pushlowparams(nparams,nvars,isptr)

	if callshadow[ncalldepth] then
		pushslots(callalign[ncalldepth]+4)
	fi

	pushedslots:=callslots[ncalldepth]
	--ncalldepth
	return pushedslots
end

proc do_pushlowparams(int nparams, nvariadics=0, isptr=0)=
!nparams=0 to 4 (if more than 4 in total, then nparams must be 4 here)
!load params to D10-13/X0-3
!does not do anything with the stack at all
! Params are categorised as follows:
! Variadic:
!   float:  load to both D and X registers
!   other:  load to D register only
! Normal/non-variadic:
!   float:  load to X register
!   other:  load to D register

	int ireg, xreg, j

	if nparams=0 then return fi
	nparams min:=4

	for i to nparams do
		j:=i+isptr
		ireg:=r10+i-1
		xreg:=xr0+i-1

		if pclstack[j].float then
			unless nvariadics and i>=nvariadics then ireg:=0 end
		else
			xreg:=0
		fi

		if ireg then loadparam(j,ireg) fi
		if xreg then loadxparam(j,xreg) fi
	od
end

proc do_for(pcl p, int incop, addop, cond)=
	pcl q,r
	mcloperand ax,bx,cx,dx,mx
	int reg

	q:=p+1
	r:=currpcl:=q+1

	mx:=mgenmem(q.def)

	if q.def.reg then
		if p.stepx=1 then
			genmc(incop, mx)
		else
			genmc(addop, mx, mgenint(p.stepx))
		fi
		ax:=mx
	else
		ax:=mgenreg(getnextreg())
		genmc(m_mov, ax,mx)
		if p.stepx=1 then
			genmc(incop, ax)
		else
			genmc(addop, ax, mgenint(p.stepx))
		fi
		genmc(m_mov, mx, ax)
	fi

	if r.opndtype=int_opnd then
		bx:=mgenint(r.value)
	else
		bx:=mgenmem(r.def)
	fi

	genmc(m_cmp, ax, bx)
	freereg(ax.reg)
!

	genmc_cond(m_jmpcc, cond, mgenlabel(p.labelno))
end

proc dojumpcc(pcl p)=
	int m:=p.mode, cond
	mcloperand ax,bx

	if p.mode in [tpi128,tpu128] then
		dojumpcc_i128(p)
		return
	fi

	ax:=genopnd_ld(xb)
	bx:=genopnd(ya)

	case pcat[m]
	when d64cat then
		if psigned[m] then
			cond:=scondcodes[p.opcode-kjumpeq]
		else
			cond:=ucondcodes[p.opcode-kjumpeq]
		fi
		genmc(m_cmp,ax,bx)

	when x32cat then
		cond:=ucondcodes[p.opcode-kjumpeq]
		genmc(m_comiss,ax,bx)

	when x64cat then
		cond:=ucondcodes[p.opcode-kjumpeq]
		genmc(m_comisd,ax,bx)
	else
		merrort("jumpcc:",p.mode)
	esac

	genmc_cond(m_jmpcc,cond, mgenlabel(p.labelno))
	delopnd()
	delopnd()
end

proc dosetcc(pcl p)=
	int m:=p.mode, cond
	mcloperand ax,bx

!	if p.mode in [tpi128,tpu128] then
!		dojumpcc_i128(p)
!		return
!	fi

	ax:=genopnd_ld(xb)
	bx:=genopnd(ya)

	case pcat[m]
	when d64cat then
		if psigned[m] then
			cond:=scondcodes[p.opcode-kseteq]
		else
			cond:=ucondcodes[p.opcode-kseteq]
		fi
		genmc(m_cmp,ax,bx)

	when x32cat then
		cond:=ucondcodes[p.opcode-kseteq]
		genmc(m_comiss,ax,bx)

	when x64cat then
		cond:=ucondcodes[p.opcode-kseteq]
		genmc(m_comisd,ax,bx)
	else
		merrort("setcc:",p.mode)
	esac

	genmc_cond(m_setcc,cond, bx:=changeopndsize(ax,1))
	genmc(m_movzx, changeopndsize(ax,4), bx)
	delopnd()
end

proc do_setretfloat(int destreg)=
	int currreg
	mcloperand ax,rx

	rx:=mgenxreg(destreg)

	ax:=genopnd_ld(1)
	currreg:=ax.reg

	case pclstack[1].loc
	when xreg_loc then
		if currreg<>destreg then

			if regset[destreg] then
				merror("setretfloat/dest in use")
			else
				genmc(m_movq, rx, ax)
				xregset[destreg]:=1
			fi
		fi
	else
		merror("setretf?")
	esac
	delopnd()		!assume next is a jump to return point

end

proc do_setret(int destreg,destxreg)=
!make sure top-of-stack is in nth register for multi-value return
!for normal returns, n will be 1
!nth value must be in d0/d1/d2, or x0/x1/x2
!Value might not be on the stack
!prior registers not available. Current value will not be in previous
!regs as they will have been moved out

	int currreg
	mcloperand ax,rx

	if pclstack[1].float then
		do_setretfloat(destxreg)
		return
	fi

	rx:=mgenreg(destreg)

	ax:=genopnd_ld(1)
	currreg:=ax.reg

	case pclstack[1].loc
	when reg_loc then
		if currreg<>destreg then

			if regset[destreg] then
				swapopndregs(destreg)
				genmc(m_xchg, rx, ax)
			else
				genmc(m_mov, rx, ax)
			fi
		fi
	else
		merror("setret?")
	esac
	delopnd()						!assume next is a jump to return point
	regset[destreg]:=1
	mccodex.regend[destreg]:=0			!d0 will not be freed
end

proc dogetretvalue(pcl p)=
	int reg,xreg,i,n
	[10]int cats

	if (p+1).opcode=ktype then
		n:=0
		while (++p).opcode=ktype do
			cats[++n]:=pcat[p.mode]
		od
		currpcl:=p-1

		for i:=n downto 1 do 
			case cats[i]
			when shortcat, widecat then
				merror("Short/wide mulret type")
			esac

			dogetretvalue_n(multregs[i],multxregs[i], cats[i])
		od

	else
		dogetretvalue_n(r0,r0,pcat[p.mode])
		if pcat[p.mode]=shortcat then
			genmc((psigned[p.mode]|m_movsx|m_movzx), mgenreg(r0),mgenreg(r0,p.size))
		fi
	fi
end

proc dogetretvalue_n(int reg,xreg,cat)=

	case cat
	when d64cat,shortcat then
		addreg0(reg)
	when x64cat then
		addxreg0(xreg,xreg_x64)
	when x32cat then
		addxreg0(xreg,xreg_x32)
	when widecat then
		addwidereg0(reg)
!	when shortcat then
!		genmc((ttisint[p.mode]|m_movsx|m_movzx), mgenreg(r0),mgenreg(r0,p.size))
	else
CPL PSTDNAMES[CAT]
		merror("getretval/n?")
	esac
end

proc do_shift(pcl p, int opc)=
	mcloperand ax
	ax:=genopnd_ld(xb)

	if pclstack[1].fmt=imm_d64 then
		genmc(opc, ax, mgenint(pclstack[1].value))
	else
		if inf_r10used then merror("shift:cl in use") fi
		loadparam(reg:r10)
		genmc(opc,ax, mgenreg(r10,1))
	fi
	delopnd()
end

proc mulimm(mcloperand ax, int n)=
!multiply operand in ax (a simple reg) by constant n
!will try efficient method if possible, otherwise use normal multiply 
	int shifts,m

	case n
	when 0 then
		genmc(m_xorx, ax,ax)
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

proc dojumptruefalse(pcl p, int cond)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_test, ax,ax)
		genmc_cond(m_jmpcc, cond, mgenlabel(p.labelno))
	else
		merrort("jumptrue/false",p.mode)
	esac
	delopnd()
end

proc dobitwise(pcl p, int opc)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xb)
		bx:=genopnd(ya)
		genmc(opc,ax,bx)
	when widecat then
		dobin_i128(p,opc,opc)
		return
	else
		merrort("bitwise:",p.mode)
	esac
	delopnd()
end

function do_addrmode(pcl p)mcloperand px =
!Top two stack elements are an array (xb) and index (ya)
!Return a operand which provdes the address mode to access the element,
!for either reading or writing
!The address mode will use 0, 1 or 2 registers. The registers may be 1 or 2
!associated with the pcl operands, or may be regvars.
!If for reading, caller will need to make their own arrangements for a dest reg.
!When Xb has to be loaded into a register anyway, then the caller can make use
!of that

	mcloperand ax,bx
	int m, scale, extra,offset, reg,regix
	psymbol d

	scale:=p.scale
	extra:=p.extra
	offset:=pclstack[1].value*scale+extra	!for imm offset

	m:=p.mode

	px:=nil

	if isregvaropnd(xb) then
		if isregvaropnd(ya) then			!regvar/regvar
			reg:=pclstack[1].reg
			regix:=scaleregvar(reg,scale,1)
			px:=mgenindex(areg:pclstack[2].reg,ireg:regix, offset:extra, scale:scale)

		elsif isimm64(ya) then			!regvar/imm
			px:=mgenindex(areg:pclstack[2].reg, offset:offset)
		else							!regvar/any
			scale:=scaleindex(bx:=genopnd_ld(ya),scale)
			px:=mgenindex(areg:pclstack[2].reg, ireg:bx.reg, scale:scale, offset:extra)
		fi
	elsif ismemaddr(xb) then
		d:=pclstack[2].def
		if isregvaropnd(ya) then			!memaddr/regvar
			reg:=pclstack[1].reg
			regix:=scaleregvar(reg,scale,1)
			px:=mgenindex(ireg:regix, def:d, offset:extra, scale:scale)

		elsif isimm64(ya) then			!memaddr/imm
			px:=mgenindex(def:d, offset:offset)
		else							!memaddr/any
			scale:=scaleindex(bx:=genopnd_ld(ya),scale)
			px:=mgenindex(ireg:bx.reg, def:d, offset:extra, scale:scale)
		fi
	else								!
		ax:=genopnd_ld(xb)
		if isregvaropnd(ya) then			!any/regvar
			reg:=pclstack[1].reg
			regix:=scaleregvar(reg,scale,1)
			px:=mgenindex(areg:ax.reg, ireg:regix, offset:extra, scale:scale)
		elsif isimm64(ya) then			!any/imm
			px:=mgenindex(areg:ax.reg, offset:offset)
		else							!any/any
			scale:=scaleindex(bx:=genopnd_ld(ya),scale)
			px:=mgenindex(areg:ax.reg, ireg:bx.reg, scale:scale, offset:extra)

		fi
	fi

!CPL "DAM",PX.SIZE,P.SIZE
	if px.size=0 then px.size:=p.size fi
	return px
end

function scaleregvar(int reg, &scale, n)int=
!When scale is 1/2/3/4, return reg (a regvar) and scale unchanged;
!otherwise set up a new register for operand n
!Copy reg to it, and scale. Return new reg, and set scale to 1
	int regix
	mcloperand ax

	if scale in [1,2,4,8] then return reg fi

	regix:=getnextreg()
	ax:=mgenreg(regix)
	genmc(m_mov,ax, mgenreg(reg))

	pclstack[n].fmt:=reg_d64
	pclstack[n].loc:=reg_loc
	pclstack[n].reg:=regix
	mulimm(ax,scale)
!	genmc(m_imul2, ax, mgenint(scale))
	scale:=1

	return regix
end

function scaleindex(mcloperand ax, int scale)int=
!when scale is 1/2/3/4, return scale unchanged
!anything else, scale value in ax, return 1
	int n
	if scale in [1,2,4,8] then return scale fi
	mulimm(ax,scale)
!
!	n:=ispoweroftwo(scale)
!	if n then
!		genmc(m_shl, ax, mgenint(n))
!	else
!		genmc(m_imul2, ax, mgenint(scale))
!	fi
	return 1
end

function makeregopnd(int n)mcloperand ax=
!turn given pcl operand, which does not occupy a register,
!make it into register operand. Note that other characteristics, such
!as value/def for imm/mem/memaddr, are not affected
!offset = xa, yb etc

	pclstack[n].fmt:=reg_d64
	pclstack[n].loc:=reg_loc
	pclstack[n].reg:=getnextreg()

	return genopnd(n)
end

proc dobinto_int(pcl p, int opc)=
	mcloperand ax,bx,rx
	int reg,size

	size:=p.size

	if size=8 and ismemaddr(xb) then
		ax:=mgenmem(pclstack[2].def)
		reg:=getnextreg()
		rx:=mgenreg(reg)
		genmc(m_mov, rx, ax)
		bx:=genopnd(ya)
		genmc(opc,rx,bx)
		genmc(m_mov, ax,rx)
		freereg(reg)
	else
		ax:=genopnd_ind(xb,size:size)
		bx:=genopnd_ld(ya,size)

		genmc(opc,ax,bx)
	fi
	delopnd()
	delopnd()
end

proc dobinto_float(pcl p, int opc)=
	mcloperand ax,bx,cx

	addreg_x64()
	ax:=genopnd_ind(xc)
	bx:=genopnd(yb)
	cx:=genopnd(za)

	genmc(m_movq, cx,ax)
	genmc(opc, cx,bx)
	genmc(m_movq, ax,cx)

	delopnd()
	delopnd()
	delopnd()
end

proc dobinto_float32(pcl p, int opc)=
	mcloperand ax,bx,cx

	addreg_x32()
	ax:=genopnd_ind(xc,4)
	bx:=genopnd(yb)
	cx:=genopnd(za)

	genmc(m_movd, cx,ax)
	genmc(opc, cx,bx)
	genmc(m_movd, ax,cx)

	delopnd()
	delopnd()
	delopnd()
end

proc doselectcc(pcl p)=
	mcloperand ax,bx, cx,dx
	int cond

	if pcat[p.mode]<>d64cat then merrort("selectcc",p.mode) fi

	ax:=genopnd_ld(xb)
!	bx:=genopnd_ld(ya)
	bx:=genopnd(ya)
	genmc(m_cmp,ax,bx)

	noxorclear:=1
	dx:=genopnd_ld(xc)
	cx:=genopnd_ld(wd)
	noxorclear:=0
	if psigned[p.mode] then
		cond:=scondcodes[p.opcode-kselecteq]
	else
		cond:=ucondcodes[p.opcode-kselecteq]
	fi
	genmc_cond(m_cmovcc, reversemcond(cond), cx,dx)
	delopnd()
	delopnd()
	delopnd()
end

function reversemcond(int cond)int=

	case cond
	when z_cond then return nz_cond
	when nz_cond then return nz_cond

	when lt_cond then return ge_cond
	when le_cond then return gt_cond
	when ge_cond then return lt_cond
	when gt_cond then return le_cond

	when ltu_cond then return geu_cond
	when leu_cond then return gtu_cond
	when geu_cond then return ltu_cond
	when gtu_cond then return geu_cond
	esac
	return 0
end

proc do_divrem(pcl p, int issigned, isdiv)=
	int opc, n, shifts

	loadopnd(2)

	if isdiv and pclstack[1].fmt=imm_d64 then
		n:=pclstack[1].value
		case n
		when 0 then
			merror("Divide by zero")
		when 1 then
			delopnd()
			return
		else
			shifts:=ispoweroftwo(n)
			if shifts then
				genmc((issigned|m_sar|m_shr), genopnd(xb), mgenint(shifts))
				delopnd()
				return
			fi
		esac
	fi 

	loadopnd(1)
	saverdx()
	fixdivopnds()

	if issigned then
		genmc(m_cqo)
		opc:=m_idiv
	else
		genmc(m_xorx, mgenreg(r11),mgenreg(r11))
		opc:=m_div
	fi

	genmc(opc, genopnd(ya,p.size))

	if not isdiv then
		genmc(m_xchg,mgenreg(r0),mgenreg(r11))
	fi
	restorerdx()

	delopnd()

end

proc fixdivopnds=
!two div operands exist as the top two operands, which will be
!in registers
!the div op requires that x is in d0, and y in any other register
!d11 also needs to be free, which will be the case is reg allocs only
!go up to d9, and d10/d11/12/13 are in use for win64 parameter passing
	int regx,regy,zop

	regx:=pclstack[2].reg
	regy:=pclstack[1].reg

	if regx=r0 then			!regy will be OK
		return
	fi
	if regy=r0 then			!need to swap then
		genmc(m_xchg,genopnd(xb),genopnd(ya))
		swapopnds(1,2)		!switch operands
		return
	fi

!neither x nor y in r0
	if regset[r0]=0 then	!d0 not in use
		genmc(m_xchg,mgenreg(r0),genopnd(xb))
		regset[regx]:=0
		pclstack[2].reg:=r0
		regset[r0]:=1
		return
	fi

!need to move current occupier of r0
!	for zop:=1 to noperands do
	for zop:=noperands downto 1 do
		if pclstack[zop].loc=reg_loc and pclstack[zop].reg=r0 then exit fi
	od

!zop is the operand number that happens to be using r0
	genmc(m_xchg,mgenreg(r0),genopnd(xb))	
	swap(pclstack[2].reg,pclstack[zop].reg)		!switch registers

end

proc saverdx=
	if inf_r11used then
		genmc(m_push, mgenreg(r11))
	fi
end

proc restorerdx=
	if inf_r11used then
		genmc(m_pop, mgenreg(r11))
	fi
end

proc doincr(pcl p, int incrop, addop)=
	if p.stepx=1 then
		if ismemaddr(xa) then
			genmc(incrop, mgenmem(pclstack[1].def))
		else
			genmc(incrop, genopnd_ind(xa))
		fi
	else
		if ismemaddr(xa) then
			genmc(addop, mgenmem(pclstack[1].def), mgenint(p.stepx))
		else
			genmc(addop, genopnd_ind(xa), mgenint(p.stepx))
		fi
	fi
	delopnd()
end

proc doincrload(pcl p, int incrop, addop)=
	mcloperand ax, mx

	if ismemaddr(xa) then
		mx:=mgenmem(pclstack[1].def)
		ax:=makeregopnd(xa)
	else
		mx:=genopnd_ind(xa,p.size)
		ax:=genopnd(xa)
	fi

	if p.stepx=1 then
		genmc(incrop, mx)
	else
		genmc(addop, mx, mgenint(p.stepx))
	fi

	if pcat[p.mode]=shortcat then
		genmc((psigned[p.mode]|m_movsx|m_movzx), ax, mx)
	else
		genmc(m_mov, ax,mx)
	fi
end

proc doloadincr(pcl p, int incrop, addop)=
	mcloperand ax,mx

	if ismemaddr(xa) then
		mx:=mgenmem(pclstack[1].def)
	else
		mx:=genopnd_ind(xa,p.size)
	fi

	addreg_d64()
	ax:=genopnd()

	if pcat[p.mode]=shortcat then
		genmc((psigned[p.mode]|m_movsx|m_movzx), ax, mx)
	else
		genmc(m_mov, ax,mx)
	fi

	if p.stepx=1 then
		genmc(incrop,mx)
	else
		genmc(addop,mx, mgenint(p.stepx))
	fi

	swapopnds(1,2)
	delopnd()
end

proc dobin_i128(pcl p, int opc1, opc2)=
	mcloperand axl,axh, bxl, bxh

	axl:=genopnd_ld(3)
	axh:=genopnd_ld(4)
	bxl:=genopnd(1)
	bxh:=genopnd(2)

	genmc(opc1, axl, bxl)
	genmc(opc2, axh, bxh)

	delopnd()
	delopnd()
end

proc dojumpcc_i128(pcl p)=
	mcloperand axlow,axhigh,bxlow,bxhigh, cx,dx, lxtrue, lxfalse

	if p.mode=tpu128 and p.opcode not in [kjumpeq, kjumpne] then
		dojumpcc_u128(p)
		return
	fi

	lxtrue:=mgenlabel(p.labelno)

	axhigh:=genopnd_ld(4)
	axlow:=genopnd_ld(3)
	bxhigh:=genopnd(2)
	bxlow:=genopnd(1)

	case p.opcode
	when kjumpeq then
		genmc(m_cmp,axlow,bxlow)
		genmc_cond(m_jmpcc,ne_cond,lxfalse:=mgenlabel())
		genmc(m_cmp,axhigh,bxhigh)
		genmc_cond(m_jmpcc,eq_cond,lxtrue)
		genmc(m_label,lxfalse)
	when kjumpne then
		genmc(m_cmp,axlow,bxlow)
		genmc_cond(m_jmpcc,ne_cond,lxtrue)
		genmc(m_cmp,axhigh,bxhigh)
		genmc_cond(m_jmpcc,ne_cond,lxtrue)
	else
		genmc(m_sub, axlow,bxlow)
		genmc(m_sbb, axhigh,bxhigh)

		genmc(m_cmp,axhigh, zero_opnd)
		if psigned[p.mode] then
			case p.opcode
			when kjumplt then
				genmc_cond(m_jmpcc, lt_cond, lxtrue)
			when kjumple then
				genmc_cond(m_jmpcc, lt_cond, lxtrue)
				genmc(m_orx,axlow,axhigh)
				genmc_cond(m_jmpcc, eq_cond, lxtrue)
			when kjumpgt then
				genmc_cond(m_jmpcc, lt_cond, lxfalse:=mgenlabel())
				genmc(m_orx,axlow,axhigh)
				genmc_cond(m_jmpcc, ne_cond, lxtrue)
				genmc(m_label,lxfalse)
			when kjumpge then
				genmc_cond(m_jmpcc, ge_cond, lxtrue)
			esac
		else
			merrort("jumpcc/rel",p.mode)
		fi
	esac

	delopnd()
	delopnd()
	delopnd()
	delopnd()

end

proc dojumpcc_u128(ref pclrec p) =
!handled relops (not eq/ne)
	mcloperand lxtrue,lxfalse, ax1,bx1,ax2,bx2
	int cond1,cond2,cond3

	case p.opcode
	when kjumpgt then
		cond1:=gtu_cond
		cond2:=ltu_cond
		cond3:=gtu_cond
	when kjumpge then
		cond1:=gtu_cond
		cond2:=ltu_cond
		cond3:=geu_cond
	when kjumplt then
		cond1:=ltu_cond
		cond2:=gtu_cond
		cond3:=ltu_cond
	when kjumple then
		cond1:=ltu_cond
		cond2:=gtu_cond
		cond3:=leu_cond
	else
		MERROR("JCC/U128")
	esac

	lxtrue:=mgenlabel(p.labelno)
	lxfalse:=mgenlabel(++mlabelno)

	ax2:=genopnd_ld(4)
	ax1:=genopnd_ld(3)
	bx2:=genopnd(2)
	bx1:=genopnd(1)

	genmc(m_cmp,ax2,bx2)
	genmc_cond(m_jmpcc, cond1, lxtrue)
	genmc_cond(m_jmpcc, cond2, lxfalse)
	genmc(m_cmp,ax1,bx1)
	genmc_cond(m_jmpcc, cond3, lxtrue)

	genmc(m_label,lxfalse)

	delopnd()
	delopnd()
	delopnd()
	delopnd()
end

proc do_syscall(int fnindex, nargs, retcat)=
!retcat = 0, d64cat, x64cat, x32cat, widecat

	int nslots

	sa_nargs:=nargs
	px_setargs(nil)

	nslots:=do_pushparams(nil,0)

	genmc(m_call, mgenlabel(getrtsproclabel(fnindex)))

!	mccodex.a.mode:=a_mem
	mccodex.a.size:=8

	to sa_nargs do
		poparg()
	od
	popslots(nslots)

	getretvalue_bycat(retcat)
end

proc getretvalue_bycat(int cat)=
	case cat
	when 0 then
		return
	when d64cat then
		addreg0(r0)
	when x64cat then
		addxreg0(r0,xreg_x64)
	when x32cat then
		addxreg0(r0,xreg_x32)
	when widecat then
		addwidereg0(r0)
	else
		merror("getval bycat")
	esac
end

proc px_pushlabel(pcl p)=
	if p.opndtype<>label_opnd then merror("pushlabel") fi
!CPL "ADD LABEL..."
	addlabeladdr(p.labelno)
end

proc do_shiftnto(pcl p,int opc)=
!shift opc=shl/shr/sar, when both operands are on the stack
!first operand is address of dest
	mcloperand px

	px:=genopnd_ind(xb)

	if pclstack[1].fmt=imm_d64 then
		genmc(opc, px, mgenint(pclstack[1].value))
	else
		if inf_r10used then merror("shiftto:cl in use") fi
		loadparam(1,r10)
		genmc(opc, px, mgenreg(r10,1))
	fi

	delopnd()
	delopnd()
end

proc domax_float(int opc)=
	mcloperand ax,bx
	ax:=genopnd_ld(xb)
	bx:=genopnd(ya)
	genmc(opc,ax,bx)
	delopnd()
end

proc domax_int(int cond)=
	mcloperand ax,bx

	ax:=genopnd_ld(xb)
	bx:=genopnd_ld(ya)

	genmc(m_cmp, ax, bx)
	genmc_cond(m_cmovcc, cond, ax, bx)

	delopnd()
end

proc domaxto_r64(int cond)=
	mcloperand px,ax,bx,lx
	int lab

	px:=genopnd_ind(xb)
	bx:=genopnd_ld(ya)
	addreg_x64()
	ax:=genopnd(xa)

	genmc(m_movq, ax, px)

	genmc(m_comisd, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=mgenlabel(lab))
	genmc(m_movq, px,bx)
	genmc(m_label, lx)
	delopnd()
	delopnd()
	delopnd()
end

proc domaxto_r32(int cond)=
	mcloperand px,ax,bx,lx
	int lab

	px:=genopnd_ind(xb)
	bx:=genopnd_ld(ya)
	addreg_x32()
	ax:=genopnd(xa)

	genmc(m_movd, ax, px)

	genmc(m_comiss, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=mgenlabel(lab))
	genmc(m_movd, px,bx)
	genmc(m_label, lx)
	delopnd()
	delopnd()
	delopnd()
end

proc domaxto_int(int cond)=
	mcloperand ax,bx,lx
	int lab

	ax:=genopnd_ind(xb)
	bx:=genopnd_ld(ya)

	genmc(m_cmp, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=mgenlabel(lab))
	genmc(m_mov, ax,bx)
	genmc(m_label, lx)
	delopnd()
	delopnd()
end

proc dosetfntable=
	if lab_funcnametable=0 then
		lab_funcnametable:=++mlabelno
		lab_funcaddrtable:=++mlabelno
		lab_funcnprocs:=++mlabelno
	fi
end

proc loadtoreg(mcloperand rx, ax, int m)=
	if pcat[m]=d64cat then
		genmc(m_mov, rx, ax)
	elsif psigned[m] then
		genmc(m_movsx, rx, ax)
	else
		genmc(m_movzx, rx, ax)
	fi
end

proc storefromreg(mcloperand ax, rx, int size)=
	genmc(m_mov, ax, changeopndsize(rx,size))
end

proc domaths(pcl p, ichar opname)=
	int nslots
	p.nargs:=1

	px_setargs(p)

	nslots:=do_pushparams(p,0)

	genmc(m_call, mgenextname(opname))

	poparg()

	popslots(nslots)

	dogetretvalue(p)
end
