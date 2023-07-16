!const fshowpcl=1
!const fshowopndstack=1
const fshowpcl=0
const fshowopndstack=0

ref mclrec mclprocentry
ref mclrec mce_oldmccodex, mce_nextmcl		!used by reset/setmclentry

pcl currpcl
[0..klast]ref proc(pcl) px_handlertable

[0..5]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
[0..5]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

[]int multregs=(r0,r1,r2,r10,r11,r12)
[]int multxregs=(r0,r1,r2,r3,r4,r5)

global proc genmcl=

	if mcldone then return fi

	inithandlers()

	mclinit()

!	for i to nlibfiles when libfiles[i]^<>'$' do
!		genpc((libtypes[i]='D'|kimportdll|kimportlib), genpc_name(libfiles[i]))
!	od

	currpcl:=pcstart

	repeat
		convertpcl(currpcl)
		++currpcl
	until currpcl.opcode=kendprogram

	genabsneg()
	genstringtable()
 
	genrealtable()


	genfunctiontable()
	genmc(m_nop)
	mcldone:=1
end

proc convertpcl(pcl p)=
	[1256]char str
	ichar ss
	int m

	if fshowpcl  then
		case p.opcode
		when klabel, kcomment, kprocdef, kthreadedproc,
			kretproc, kendproc then
		else
				strcpy(&.str,"                       ")
				strcat(str, strint(getlineno(p.pos)))
				strcat(str, " ")
				strcat(&.str,strpclstr(p))
				strcat(str, " (")
				strcat(str, sourcefilenames[getfileno(p.pos)])
				strcat(str, " )")

				mgencomment(&.str)
		esac
	fi

	mlineno:=p.pos
	px_handlertable[p.opcode]^(p)

	if fshowopndstack then
		case p.opcode
		when klabel, kcomment, kprocdef, kthreadedproc,
			kretproc, kendproc then
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

	static [,2]byte dupltable = (
		(kjumpne, 		kjumpeq)
		(kjumplt, 		kjumpeq)
		(kjumple, 		kjumpeq)
		(kjumpge, 		kjumpeq)
		(kjumpgt, 		kjumpeq)

		(ksetne, 		kseteq)
		(ksetlt, 		kseteq)
		(ksetle, 		kseteq)
		(ksetge, 		kseteq)
		(ksetgt, 		kseteq)
!
		(kcallf,		kcallp)
		(kicallp,		kcallp)
		(kicallf,		kcallp)
		(kthreadedproc,	kprocdef)
		)


	for i to dupltable.len do
		px_handlertable[dupltable[i,1]]:=px_handlertable[dupltable[i,2]]
	end

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
	println "Unimplemented Opcode:",pclnames[p.opcode]
end

global proc gerrorc(ichar mess, param=nil)=
	print "MCL Gen error:",mess
	if param then
		print ":",param
	fi

	stop 1
end

proc px_zero*(pcl p)=
	unimpl(p)
end

proc px_nop*(pcl p)=
!	unimpl(p)
end

proc px_stop*(pcl p)=
	symbol d

	loadparam(xa,r10)

	d:=pcl_makesymbol("exit")
	d.isimport:=1
	genmc(m_call, mgenmemaddr(d))
	localshadow:=1
	highargs max:=1

	delopnd()
end

proc px_comment*(pcl p)=
	mgencomment(p.svalue)
end

proc px_istatic*(pcl p)=
	setsegment('I',p.align)
	genmc(m_labelname,mgenmemaddr(p.def))
end

proc px_zstatic*(pcl p)=
	symbol d

	d:=p.def
	setsegment('Z',p.align)
	genmc(m_labelname,mgenmemaddr(d))

	genmc(m_resb,mgenint(p.psize))
end

proc px_procdef*(pcl p)=
!Things that are remembered:

!PCLPROCDEF:	PCL op for kprocdef: used to repeat PASS2 pass for optimising
!				Note will normally skip back to following op, as below is for PASS1 only

!MCLPROCENTRY:	MCL op for dummy op (or anything that will work), used to insert
!				proc entry ops during do_procentry()

	passno:=1

	pclprocdef:=currpcl
	currproc:=currpcl.def

	setsegment('C',1)

	mgencomment("DUMMY")

	genmc(m_procstart,mgenmemaddr(currproc))
	genmc(m_labelname,mgenmemaddr(currproc))

	initpass1(currpcl)

!create dummy mcl op at which to insert hang proc-entry code onto later
	mgencomment(">>")
	mclprocentry:=mccodex

end

proc px_retproc*(pcl p)=
	int offset

	if passno=1 then
		do_procentry1(p)
		do_procexit1()
	else
		do_procentry2(p)
		do_procexit2()
	fi

end

proc px_endproc*(pcl p)=

	if passno=1 then
		do_endproc1(p)
	else
		do_endproc2(p)
	fi

!	mgeninfos("High reg:  ",getregname(highreg))
!	mgeninfos("High xreg: ",getxregname(highxreg))
!	mgeninfo("Bspill: ",	bspill)
!	mgeninfo("Bxspill: ",	bxspill)
!	mgeninfo ("Calls:     ",nproccalls)
!	mgeninfos("Leaf func: ",(leafproc|"Yes"|"No"))
!	mgeninfos("Local shadow: ",(localshadow|"Yes"|"No"))
!	mgeninfo ("Max args:  ",highargs)
end

proc px_endprogram*(pcl p)=
	unimpl(p)
end

proc px_label*(pcl p)=
	genmc(m_labelx, mgenlabel(p.labelno))
end

proc px_load*(pcl p)=
	case p.opndtype
	when mem_opnd then
		addmem(p)
	when memaddr_opnd then
		addmemaddr(p.def)
	when int_opnd then
		addimm(p.value)
	when real_opnd then
!		addimmx64(p.xvalue)
		addimmx64(p.r64index)
	when real32_opnd then
!		addimmx32(p.xvalue32)
		addimmx32(p.r32index)
	when string_opnd then
!		addstr(p.svalue)
		addstr(p.strindex)
	else
		merror("Load",opndnames[p.opndtype])
	esac
end

proc px_store*(pcl p)=
	mcloperand ax,bx

	checkallloaded()

	case p.opndtype
	when mem_opnd then
		case p.pcat
		when d64cat then
			genmc(m_mov, mgenmem(p.def), loadopnd(xa))

		when x64cat then
			genmc(m_movq, mgenmem(p.def), loadopnd(xa))
		when x32cat then
			genmc(m_movd, mgenmem(p.def), loadopnd(xa,4))
		when shortcat then
			genmc(m_mov, mgenmem(p.def), loadopnd(xa,p.psize))

		when blockcat then
			bx:=getopnd_ind()
			addmemaddr(p.def)
			ax:=getopnd_ind()

			copyblock(ax,bx,p.psize)

			delopnd()
		else
			merrort("POPMEM",p.pmode)
		esac
	else
		merroropnd("POP",p.opndtype)
	esac

	delopnd()
end

proc px_pushhw*(pcl p)=
	pushopnd(noperands)
end

proc px_iloadx*(pcl p)=
	mcloperand ax,bx,cx,fx

	cx:=do_addrmode(p)

	if pclstack[xb].loc<>reg_loc then
		pclstack[xb].loc:=reg_loc			!need to prepare it for result
		pclstack[xb].reg:=getnextreg()		!(although wasted for floats)
		pclstack[xb].cat:=d64cat
	fi
	ax:=getopnd(xb)

!here, ax is a suitable dest reg (not used for float dest), cx is the access mode

	case p.pcat
	when d64cat then
		genmc(m_mov, ax, cx)

	when x64cat then
!need to turn ax into a float reg
		addreg_x64()
		swapopnds(xc, ya)
		fx:=getopnd(xc)

		genmc(m_movq, fx, cx)
		delopnd()

	when x32cat then
!need to turn ax into a float reg
		addreg_x32()
		swapopnds(xc,ya)
		fx:=getopnd(xc)

		genmc(m_movd, fx, changeopndsize(cx,4))
		delopnd()

	when shortcat then
		cx.size:=p.psize
		genmc((ttsigned[p.pmode]|m_movsx|m_movzx), ax, cx)

	when blockcat then
		genmc(m_lea, ax, cx)
	else
		merrort("pushptroff",p.pmode)
	esac	

	delopnd()
end

proc px_istorex*(pcl p)=
	mcloperand ax,bx,cx,px

	px:=do_addrmode(p)
	cx:=loadopnd(xc)

	case p.pcat
	when d64cat then
		genmc(m_mov, px,cx)

	when x64cat then
		genmc(m_movq, px,cx)

	when x32cat then
		genmc(m_movd, changeopndsize(px,4),cx)

	when shortcat then
		px.size:=p.psize
		genmc(m_mov, px,changeopndsize(cx,p.psize))

	when blockcat then
		copyblock(px,makeopndind(cx),p.psize)

	else
		merrort("istorex ",p.pmode)
	esac	

	delopnd()
	delopnd()
	delopnd()
end

proc px_iload*(pcl p)=
	mcloperand ax,px,cx,fx,bx
	int m

	m:=p.pmode
	if isregvaropnd(xa) and p.pcat<>blockcat then
		cx:=mgenireg(pclstack[xa].reg)
		ax:=makeregopnd(xa)
	elsif pclstack[xa].loc=memaddr_loc then
		cx:=mgenmem(pclvals[xa].def)
		ax:=makeregopnd(xa)
	else
		ax:=loadopnd()
		cx:=makeopndind(ax)
	fi

	case stdcat[m]
	when d64cat then
		genmc(m_mov, ax, cx)

	when shortcat then
		genmc((ttsigned[m]|m_movsx|m_movzx), ax, changeopndsize(cx,p.psize))

	when x64cat then
		addreg_x64()
		swapopnds(xb,ya)
		fx:=getopnd(xb)

		genmc(m_movq, fx, cx)
		delopnd()

	when x32cat then
		addreg_x32()
		swapopnds(xb,ya)
		fx:=getopnd(xb)

		genmc(m_movd, fx, changeopndsize(cx,4))
		delopnd()

	when blockcat then		!nothing further needed

	else
		merror("pushptr ",stdnames[m])
	esac	

end

proc px_istore*(pcl p)=
	mcloperand ax,bx,cx,px
	int opc

	bx:=loadopnd(xb)

	if isregvaropnd(xa) and p.pcat<>blockcat then
		ax:=mgenireg(pclstack[xa].reg)
	else
		ax:=getopnd_ind(ya)
	fi

	case p.pcat
	when d64cat then
		genmc(m_mov, ax,bx)

	when shortcat then
		genmc(m_mov, changeopndsize(ax,ttsize[p.pmode]),changeopndsize(bx,ttsize[p.pmode]))

	when x64cat then
		genmc(m_movq, ax,bx)

	when x32cat then
		genmc(m_movd, changeopndsize(ax,4),bx)

	when blockcat then
		copyblock(ax,makeopndind(bx),p.psize)

	else
		merror("istore ",catnames[p.pcat])
	esac	

	delopnd()
	delopnd()
end

proc px_loadbit*(pcl p)=
	mcloperand ax
	int i

	if pclstack[xa].loc<>immd64_loc then
		merror("dotix i not imm")
	fi

	ax:=loadopnd(xb)
	i:=pclvals[xa].value

	if i then
		genmc(m_shr, ax, mgenint(i))
	fi
	genmc(m_andx, changeopndsize(ax,4), mgenint(1))

	delopnd()
end

proc px_storebit*(pcl p)=
	mcloperand ax,bx,cx,rx,mx
	int i,size,cxfmt,rhs,axoffset

	if pclstack[xc].loc=immd64_loc then
		rhs:=pclvals[xc].value
		cx:=nil
	else
		cx:=loadopnd(xc)
	fi

	if pclstack[xa].loc<>immd64_loc then
		merror("popdotix i not imm")
	fi
	i:=pclvals[xa].value
	size:=p.psize


	axoffset:=xb

	addreg_d64()
	rx:=getopnd()
	addreg_d64()
	mx:=getopnd()

!	if pclfmt[axindex]=imm_memaddr then
!		genmc(m_mov, mgenmem(pcldef[axindex]))
!	else
		ax:=getopnd_ind(xb-2,size:size)
		genmc((size=8|m_mov|m_movzx),rx,ax)
!	fi


	genmc(m_mov,mx,mgenint(inot(1<<i)))
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
	delopnd()		!value being stored
end

proc px_loadbf*(pcl p)=
	mcloperand ax,mx,mx4
	int i,j
	word mask

	if pclstack[yb].loc<>immd64_loc or pclstack[za].loc<>immd64_loc then
		merror("dotslice i/j not imm")
	fi

	ax:=loadopnd(xc)
	i:=pclvals[yb].value
	j:=pclvals[za].value

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

proc px_storebf*(pcl p)=
	mcloperand ax,rx,mx,mx4,dx
	int i,j,size
	word mask

	if pclstack[yb].loc<>immd64_loc or pclstack[za].loc<>immd64_loc then
		merror("popdotslice i/j not imm")
	fi

	dx:=loadopnd(wd)

	size:=p.psize
	ax:=getopnd_ind(xc,size:size)

	i:=pclvals[yb].value
	j:=pclvals[za].value

	mx:=makeregopnd(yb)
	rx:=makeregopnd(za)

	loadtoreg(rx,ax,p.pmode)

	mask:=inot((inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i)

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
	delopnd()		!x

end

proc px_unload*(pcl p)=
	delopnd()
end

proc px_eval*(pcl p)=
	loadopnd(xa)
	delopnd()
end

proc px_callp*(pcl p)=
	int nregargs:=min(p.nargs,4), slots, isptr:=0, shadow:=0

	if p.opcode in [kicallp, kicallf] then
		isptr:=1
	fi

	highargs max:=p.nargs

	do_pushlowargs(nregargs, p.nvariadics, isptr)

	slots:=0
	if p.nargs<=4 then
		if mstackdepth then
			slots+:=4
			pushslots(4)					!shadowspace
		else
			localshadow:=1
		fi

	else
		slots:=p.nargs+callalign[ncalldepth]
		pushslots(4)						!shadowspace
	fi

	if isptr then
		genmc(m_call, loadopnd(xa))
		delopnd()
	else
		genmc(m_call, mgenmemaddr(p.def))
	fi

	to nregargs do
		delopnd()
	od

	if slots then
		popslots(slots)
	fi

	if p.pmode then
		dogetretvalue(p)
	fi

	--ncalldepth
end

proc px_retfn*(pcl p)=
	mcloperand ax,bx

	if p.pcat=blockcat then
		genmc(m_mov, mgenreg(r1), mgenmem(blockretname))
		ax:=mgenireg(r0)
		bx:=mgenireg(r1)
		regset[r0]:=1
		regset[r1]:=1
		copyblock(bx, ax, p.psize)

		regset[r0]:=0
		regset[r1]:=0
		genmc(m_mov, mgenreg(r0), mgenmem(blockretname))
	fi

	px_retproc(p)
end

proc px_jump*(pcl p)=
	int labno:=p.labelno
	pcl q:=p+1

	while q.opcode=kcomment do ++q od
	case q.opcode
	when klabel then
		if q.labelno=labno then return fi
		++q
		if q.opcode=klabel and q.labelno=labno then return fi
	when kjump then
		q.opcode:=knop
	esac

	genmc(m_jmp, mgenlabel(labno))
end

proc px_jumpptr*(pcl p)=
	genmc(m_jmp, getopnd(xa))
	delopnd()
end

proc px_jumpeq*(pcl p)=
	dojumpcc(p)
end

proc px_jumptrue*(pcl p)=
	dojumptruefalse(p,nz_cond)
end

proc px_jumpfalse*(pcl p)=
	dojumptruefalse(p,z_cond)
end

proc px_seteq*(pcl p)=
	dosetcc(p)
end

proc px_casejumpeq*(pcl p)=
	mcloperand ax,bx

	case p.pcat
	when d64cat then
		genmc(m_cmp, loadopnd(xb), getopnd(ya))
		genmc_cond(m_jmpcc, eq_cond, mgenlabel(p.labelno))
		delopnd()

	else
		merrort("casejumpeq",p.pmode)
	esac
end

proc px_to*(pcl p)=
	pcl q
	mcloperand ax

	q:=currpcl:=p+1

	ax:=mgenmem(q.def)
	genmc(m_dec, ax)
	genmc_cond(m_jmpcc, nz_cond, mgenlabel(p.labelno))
end

proc px_forup*(pcl p)=
	do_for(p, m_inc, m_add, le_cond)
end

proc px_fordown*(pcl p)=
	do_for(p, m_dec, m_sub, ge_cond)
end

proc px_swap*(pcl p)=
	mcloperand ax,bx

	mcloperand px:=getopnd_ind(xb,p.psize)
	mcloperand qx:=getopnd_ind(ya,p.psize)

	ax:=mgenreg(getnextreg(),p.psize)
	bx:=mgenreg(getnextreg(),p.psize)

	case p.pcat
	when d64cat,shortcat then
		genmc(m_mov, ax, px)
		genmc(m_mov, bx, qx)
		genmc(m_mov, qx, ax)
		genmc(m_mov, px, bx)

	else
		merrort("swap",p.pmode)
	esac

	freereg(ax.reg)
	freereg(bx.reg)

	delopnd()
	delopnd()
end

proc px_storeslice*(pcl p)=
	mcloperand ax, bx

	addmemaddr(p.def)
	ax:=getopnd_ind()

	bx:=loadopnd(xc)
	genmc(m_mov, ax, bx)
	bx:=loadopnd(yb)
	genmc(m_mov, applyoffset(ax,8), bx)
	delopnd()
	delopnd()
	delopnd()
	if p.opcode=kstoresliced then
		addmemaddr(p.def)
	fi
end

proc px_storesliced*(pcl p)=
	px_storeslice(p)
end

proc px_switch*(pcl p)=
	int minlab, maxlab, jumplab, elselab
	mcloperand ax

	minlab:=p.minlab
	maxlab:=p.maxlab
	jumplab:=p.labelno
	currpcl:=p+1
	elselab:=currpcl.labelno

	ax:=loadopnd(xa)
	if minlab<>0 then
		genmc(m_sub,ax,mgenint(minlab))
	fi
	genmc(m_cmp,ax,mgenint(maxlab-minlab+1))
	genmc_cond(m_jmpcc,geu_cond,mgenlabel(elselab))
	genmc(m_jmp, mgenindex(ireg:ax.reg,scale:8,labno:jumplab))

	delopnd()

	setsegment('I')
end

proc px_switchu*(pcl p)=
	int minlab, maxlab, jumplab, elselab
	mcloperand ax

	minlab:=p.minlab
	maxlab:=p.maxlab
	jumplab:=p.labelno
	currpcl:=p+1
	elselab:=currpcl.labelno

	ax:=loadopnd(xa)

	genmc(m_jmp, mgenindex(ireg:ax.reg,scale:8,labno:jumplab, offset:-minlab*8))

	delopnd()
end

proc px_switchlabel*(pcl p)=
	genmc(m_dq, mgenlabel(p.labelno))
end

proc px_endswitch*(pcl p)=
	setsegment('C')
end

proc px_clear*(pcl p)=
	mcloperand ax

	ax:=getopnd_ind()

	clearblock(ax,p.psize)
	delopnd()
end

proc do_data(pcl p)=
	mcloperand ax
	int opc

	case p.opndtype
	when int_opnd then
		ax:=mgenint(p.value)
	when real_opnd, realimm_opnd then
		ax:=mgenrealimm(p.xvalue,8)
	when real32_opnd then
		ax:=mgenrealimm(p.xvalue32,4)

	when string_opnd then
		ax:=mgenlabel(p.strindex)

	when memaddr_opnd then
		ax:=mgenmemaddr(p.def)
		ax.offset:=p.extra
	when label_opnd then
		ax:=mgenlabel(p.labelno)

	else
		merror("db/dq optype? #", opndnames[p.opndtype])
	esac

	case p.psize
	when 1 then opc:=m_db
	when 2 then opc:=m_dw
	when 4 then opc:=m_dd
	when 8 then opc:=m_dq
	else
		merror("DATA/not 1248")
	esac
!
	genmc(opc,ax)
end

proc px_db*(pcl p)=
	p.pmode:=tu8
	p.psize:=1
	do_data(p)
end

proc px_dw*(pcl p)=
	p.pmode:=tu16
	p.psize:=2
	do_data(p)
end

proc px_dd*(pcl p)=
	p.pmode:=tu32
	p.psize:=4
	do_data(p)
end

proc px_dq*(pcl p)=
	p.pmode:=tu64
	p.psize:=8
	do_data(p)
end

proc px_assem*(pcl p)=
	domcl_assem(p.asmcode)
	if p.pmode then
		dogetretvalue(p)
	fi
end

proc px_add*(pcl p)=
	mcloperand ax,bx

	case p.pcat
	when d64cat then
		ax:=loadopnd(xb)
		bx:=getopnd(ya)
		genmc(m_add,ax,bx)

	when x64cat then
		dobin_float(m_addsd)
	when x32cat then
		dobin_float(m_addss)
	else
		merrort("add:",p.pmode)
	esac
	delopnd()
end

proc px_sub*(pcl p)=
	mcloperand ax,bx

	case p.pcat
	when d64cat then
		ax:=loadopnd(xb)
		if pclstack[xa].loc=immd64_loc and pclvals[xa].value=1 then
			genmc(m_dec, ax)
		else
			bx:=getopnd(ya)
			genmc(m_sub,ax,bx)
		fi
	when x64cat then
		dobin_float(m_subsd)
	when x32cat then
		dobin_float(m_subss)
	else
		merrort("sub:",p.pmode)
	esac
	delopnd()
end

proc px_mul*(pcl p)=
	mcloperand ax,bx

	case p.pcat
	when d64cat then
		ax:=loadopnd(xb)
		if  pclstack[xa].loc=immd64_loc then
			mulimm(ax,pclvals[xa].value)
			delopnd()
			return
		fi

		bx:=getopnd(ya)
		genmc(m_imul2,ax,bx)

	when x64cat then
		dobin_float(m_mulsd)
	when x32cat then
		dobin_float(m_mulss)
	else
		merrort("mul:",p.pmode)
	esac
	delopnd()
end

proc px_div*(pcl p)=
	mcloperand ax,bx

	case p.pcat
	when x64cat then
		dobin_float(m_divsd)
	when x32cat then
		dobin_float(m_divss)
	else
		merrort("div:",p.pmode)
	esac
	delopnd()
end

proc px_idiv*(pcl p)=
	case p.pmode
	when ti64 then
		do_divrem(p, issigned:1, isdiv:1)
	when tu64 then
		do_divrem(p, issigned:0, isdiv:1)
	else
		merrort("idiv:",p.pmode)
	esac
end

proc px_irem*(pcl p)=
	case p.pmode
	when ti64 then
		do_divrem(p, issigned:1, isdiv:0)
	when tu64 then
		do_divrem(p, issigned:0, isdiv:0)
	else
		merrort("irem:",p.pmode)
	esac
end

proc px_iand*(pcl p)=
	dobitwise(p, m_andx)
end

proc px_ior*(pcl p)=
	dobitwise(p, m_orx)
end

proc px_ixor*(pcl p)=
	dobitwise(p, m_xorx)
end

proc px_shl*(pcl p)=
	case p.pcat
	when d64cat then
		do_shift(p,m_shl)
	else
		merrort("shl:",p.pmode)
	esac
end

proc px_shr*(pcl p)=
	case p.pmode
	when ti64 then
		do_shift(p,m_sar)
	when tu64 then
		do_shift(p,m_shr)
	else
		merrort("shr:",p.pmode)
	esac
end

proc px_min*(pcl p)=
	case p.pmode
	when ti64 then
		domax_int(gt_cond)
	when tu64 then
		domax_int(gtu_cond)
	when tr64 then
		domax_float(m_minsd)
	when tr32 then
		domax_float(m_minss)
	else
		merrort("min:",p.pmode)
	esac
end

proc px_max*(pcl p)=
	case p.pmode
	when ti64 then
		domax_int(lt_cond)
	when tu64 then
		domax_int(ltu_cond)
	when tr64 then
		domax_float(m_maxsd)
	when tr32 then
		domax_float(m_maxss)
	else
		merrort("max:",p.pmode)
	esac
end

proc px_addrefx*(pcl p)=
	mcloperand ax,cx

	cx:=do_addrmode(p)

	if pclstack[xb].loc<>reg_loc then
		pclstack[xb].loc:=reg_loc			!need to prepare it for result
		pclstack[xb].cat:=d64cat
		pclstack[xb].reg:=getnextreg()		!(although wasted for floats)
	fi
	ax:=getopnd(xb)

	genmc(m_lea, ax, cx)
	delopnd()
end

proc px_subref*(pcl p)=
	mcloperand ax,bx
	int n

	ax:=loadopnd(xb)
!	bx:=loadopnd(ya)
	bx:=getopnd(ya)
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

proc px_subrefx*(pcl p)=
	int scale, extra, offset
	mcloperand ax,bx

	scale:=p.scale
	extra:=p.extra

	ax:=loadopnd(xb)

	if pclstack[xa].loc=immd64_loc then
		genmc(m_sub, ax, mgenint(pclvals[xa].value*scale+extra))
	else
		bx:=loadopnd(xa)
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

proc px_neg*(pcl p)=
	mcloperand ax

	case p.pcat
	when d64cat then
		ax:=loadopnd(xa)
		genmc(m_neg,ax)

	when x64cat then
		if not labneg64 then labneg64:=mcreatefwdlabel() fi
		genmc(m_xorpd,loadopnd(xa),mgenlabelmem(labneg64))
	when x32cat then
		if not labneg32 then labneg32:=mcreatefwdlabel() fi
		genmc(m_xorps,loadopnd(xa),mgenlabelmem(labneg32))
	else
		merrort("neg",p.pmode)
	esac
end

proc px_abs*(pcl p)=
	mcloperand ax,lx

	case p.pcat
	when d64cat then
		ax:=loadopnd(xa)
		genmc(m_cmp, ax, mgenint(0))

		genmc_cond(m_jmpcc, ge_cond, lx:=mgenlabel(++mlabelno))
		genmc(m_neg,ax)
		genmc(m_labelx, lx)

	when x64cat then
		if not lababs64 then lababs64:=mcreatefwdlabel() fi
		genmc(m_andpd,loadopnd(xa),mgenlabelmem(lababs64))
	when x32cat then
		if not lababs32 then lababs32:=mcreatefwdlabel() fi
		genmc(m_andps,loadopnd(xa),mgenlabelmem(lababs32))
	else
		merrort("abs",p.pmode)
	esac
end

proc px_inot*(pcl p)=
	mcloperand ax

	case p.pcat
	when d64cat then
		ax:=loadopnd(xa)
		genmc(m_notx,ax)

	else
		merrort("inot",p.pmode)
	esac
end

proc px_notl*(pcl p)=
	mcloperand ax

	case p.pcat
	when d64cat then
		ax:=loadopnd(xa)
		genmc(m_xorx,changeopndsize(ax,1),mgenint(1))

	else
		merrort("notl",p.pmode)
	esac
end

proc px_istruel*(pcl p)=
	mcloperand ax, bx

!PRINTLN "ISTRUE",CATNAMES[P.PCAT]

	case p.pcat
	when d64cat then
		ax:=loadopnd(xa)
		genmc(m_test, ax,ax)
		genmc_cond(m_setcc, ne_cond, bx:=changeopndsize(ax,1))
		genmc(m_movzx, changeopndsize(ax,4),bx)
!	when x64cat then

	else
		merrort("istruel",p.pmode)
	esac
end

proc px_sqr*(pcl p)=
	mcloperand ax

	ax:=loadopnd(xa)

	case p.pcat
	when d64cat then
		genmc(m_imul2,ax,ax)
!
	when x64cat then
		genmc(m_mulsd,ax,ax)
	when x32cat then
		genmc(m_mulss,ax,ax)
	else
		merrort("sqr",p.pmode)
	esac
end

proc px_sqrt*(pcl p)=
	mcloperand ax

	ax:=loadopnd(xa)

	case p.pcat
	when x64cat then
		genmc(m_sqrtsd,ax,ax)
	when x32cat then
		genmc(m_sqrtss,ax,ax)
	else
		merrort("sqrt",p.pmode)
	esac
end

proc px_sin*(pcl p)=
	domaths(p,"sin*")
end

proc px_cos*(pcl p)=
	domaths(p,"cos*")
end

proc px_tan*(pcl p)=
	domaths(p,"tan*")
end

proc px_asin*(pcl p)=
	domaths(p,"asin*")
end

proc px_acos*(pcl p)=
	domaths(p,"acos*")
end

proc px_atan*(pcl p)=
	domaths(p,"atan*")
end

proc px_ln*(pcl p)=
	domaths(p,"log*")
end

proc px_log*(pcl p)=
	domaths(p,"log10*")
end

proc px_exp*(pcl p)=
	domaths(p,"exp*")
end

proc px_round*(pcl p)=
	domaths(p,"round*")
end

proc px_floor*(pcl p)=
	domaths(p,"floor*")
end

proc px_ceil*(pcl p)=
	domaths(p,"ceil*")
end

proc px_atan2*(pcl p)=
	domaths(p, "atan2",2)
end

proc px_power*(pcl p)=
	mcloperand ax,bx

	case p.pcat
	when d64cat then

		swapopnds(1,2)
		do_syscall(sf_power_i64,2,d64cat)

	when x64cat then
		swapopnds(1,2)
		domaths(p,"pow*",2)
	else
		merrort("power:",p.pmode)
	esac
end

proc px_fmod*(pcl p)=
	unimpl(p)
end

proc px_incr*(pcl p)=
	case p.pcat
	when d64cat,shortcat then
		doincr(p,m_inc, m_add)
	else
		merrort("incr", p.pmode)
	esac
end

proc px_decr*(pcl p)=
	case p.pcat
	when d64cat,shortcat then
		doincr(p,m_dec, m_sub)
	else
		merrort("decr", p.pmode)
	esac
end

proc px_incrload*(pcl p)=
	case p.pcat
	when d64cat,shortcat then
		doincrload(p,m_inc, m_add)
	else
		merrort("incrload", p.pmode)
	esac
end

proc px_decrload*(pcl p)=
	case p.pcat
	when d64cat,shortcat then
		doincrload(p,m_dec, m_sub)
	else
		merrort("decrload", p.pmode)
	esac
end

proc px_loadincr*(pcl p)=
	case p.pcat
	when d64cat,shortcat then
		doloadincr(p,m_inc, m_add)
	else
		merrort("loadincr", p.pmode)
	esac
end

proc px_loaddecr*(pcl p)=
	case p.pcat
	when d64cat,shortcat then
		doloadincr(p,m_dec, m_sub)
	else
		merrort("loaddecr", p.pmode)
	esac
end

proc px_addto*(pcl p)=
	mcloperand ax,bx

	case p.pcat
	when d64cat,shortcat then
		dobinto_int(p,m_add)
	when x64cat then
		dobinto_float(p,m_addsd)
	when x32cat then
		dobinto_float32(p,m_addss)
	else
		merrort("addto:",p.pmode)
	esac
end

proc px_subto*(pcl p)=
	mcloperand ax,bx

	case p.pcat
	when d64cat,shortcat then
		dobinto_int(p,m_sub)
	when x64cat then
		dobinto_float(p,m_subsd)
	when x32cat then
		dobinto_float32(p,m_subss)
	else
		merrort("subto:",p.pmode)
	esac
end

proc px_multo*(pcl p)=
	mcloperand ax,bx,cx

	case p.pcat
	when d64cat then
		addreg_d64()
		ax:=getopnd_ind(xc)
		bx:=getopnd(yb)
		cx:=getopnd(za)

		genmc(m_mov, cx,ax)

		if  pclstack[xb].loc=immd64_loc then
			mulimm(cx, pclvals[xb].value)
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
	else
		merrort("multo:",p.pmode)
	esac
end

proc px_divto*(pcl p)=
	mcloperand ax,bx,cx

	case p.pcat
	when x64cat then
		dobinto_float(p,m_divsd)
	when x32cat then
		dobinto_float32(p,m_divss)
	else
		merrort("divto:",p.pmode)
	esac
end

proc px_iandto*(pcl p)=
	mcloperand ax,bx

	case p.pcat
	when d64cat,shortcat then
		dobinto_int(p,m_andx)
	else
		merrort("iandto:",p.pmode)
	esac
end

proc px_iorto*(pcl p)=
	mcloperand ax,bx

	case p.pcat
	when d64cat,shortcat then
		dobinto_int(p,m_orx)
	else
		merrort("iorto:",p.pmode)
	esac
end

proc px_ixorto*(pcl p)=
	mcloperand ax,bx

	case p.pcat
	when d64cat,shortcat then
		dobinto_int(p,m_xorx)
	else
		merrort("ixorto:",p.pmode)
	esac
end

proc px_shlto*(pcl p)=
	case p.pmode
	when ti64,tu64 then
		do_shiftnto(p,m_shl)
	else
		merrort("shlto:",p.pmode)
	esac
end

proc px_shrto*(pcl p)=
	case p.pmode
	when ti64 then
		do_shiftnto(p,m_sar)
	when tu64 then
		do_shiftnto(p,m_shr)
	else
		merrort("shrto:",p.pmode)
	esac
end

proc px_minto*(pcl p)=
	case p.pmode
	when ti64 then
		domaxto_int(le_cond)
	when tu64 then
		domaxto_int(leu_cond)
	when tr64 then
		domaxto_r64(leu_cond)
	when tr32 then
		domaxto_r32(leu_cond)
	else
		merrort("minto:",p.pmode)
	esac
end

proc px_maxto*(pcl p)=
	case p.pmode
	when ti64 then
		domaxto_int(ge_cond)
	when tu64 then
		domaxto_int(geu_cond)
	when tr64 then
		domaxto_r64(geu_cond)
	when tr32 then
		domaxto_r32(geu_cond)
	else
		merrort("maxto:",p.pmode)
	esac
end

proc px_addrefxto*(pcl p)=
	int scale, extra,offset
!
	scale:=p.scale
	extra:=p.extra
	offset:=pclvals[xa].value*scale+extra	!in case imm_d64

	mcloperand ax,bx,rx
	int reg,size

	if ismemaddr(xb) then
		ax:=mgenmem(pclvals[xb].def)
		reg:=getnextreg()
		rx:=mgenreg(reg)

		genmc(m_mov, rx, ax)

		if pclstack[xa].loc=immd64_loc then
			genmc(m_add,rx,mgenint(offset))
		else
			bx:=loadopnd(ya)
			mulimm(bx,scale)
			genmc(m_add,rx,bx)
		fi

		genmc(m_mov, ax,rx)
		freereg(reg)
	else
		ax:=getopnd_ind(xb)
		if pclstack[xa].loc=immd64_loc then
			genmc(m_add,ax,mgenint(offset))
		else
			bx:=loadopnd(ya)
			mulimm(bx,scale)
			genmc(m_add,ax,bx)
		fi
	fi
	delopnd()
	delopnd()
end

proc px_subrefxto*(pcl p)=
	int scale, extra
	mcloperand ax,bx

	scale:=p.scale
	extra:=p.extra

	ax:=getopnd_ind(xb)

	if pclstack[xa].loc=immd64_loc then
		genmc(m_sub, ax, mgenint(pclvals[xa].value*scale+extra))
	else
		bx:=loadopnd(xa)
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

proc px_typepun*(pcl p)=
	mcloperand ax,bx,cx

	bx:=loadopnd(xa)

	case p.pcat
	when d64cat then
		case pclstack[xa].loc
		when xreg_loc then
			addreg_d64()
			ax:=getopnd(xa)
            genmc(m_movq,ax,bx)
			swapopnds(xb,ya)
			delopnd()
		when reg_loc then
		else
			goto error
		esac

	when x64cat then
		case pclstack[xa].loc
		when reg_loc then
			addreg_x64()
			ax:=getopnd(xa)
            genmc(m_movq,ax,bx)
			swapopnds(xb,ya)
			delopnd()
		else
			goto error
		esac
	when shortcat then
		case pclstack[xa].loc
		when xreg_loc then
			addreg_d64()
			ax:=getopnd(xa)
			cx:=changeopndsize(ax,4)
            genmc(m_movd, cx,bx)
			swapopnds(xb,ya)
			delopnd()

			genmc((ttsigned[p.pmode]|m_movsx|m_movzx),ax,cx)
		when reg_loc then

		else
			goto error
		esac

	else
error::
		merrort("TYPEPUN",p.pmode)
	esac

	pclstack[xa].cat:=p.pcat
end

proc px_float*(pcl p)=
	mcloperand ax,fx
	int lab,lab2

	ax:=loadopnd(xa)

	case pr(p.oldmode, p.pmode)
	when pr(ti64, tr64) then
		addreg_x64()
		fx:=getopnd(xa)
		genmc(m_cvtsi2sd, fx, ax)
		swapopnds(xb,ya)
	when pr(ti64, tr32) then
		addreg_x32()
		fx:=getopnd(xa,4)
		genmc(m_cvtsi2ss, fx, changeopndsize(ax,4))
		swapopnds(xb,ya)
	when pr(tu64, tr64) then
		addreg_x64()
		fx:=getopnd(xa)

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
		swapopnds(xb,ya)

	else
		merrort("float",p.pmode)
	esac

	delopnd()
end

proc px_fix*(pcl p)=
	mcloperand fx,ax
	int newmode:=p.pmode, oldmode:=p.oldmode

	case stdcat[newmode]
	when d64cat then
		fx:=loadopnd(xa)
		addreg_d64()
		ax:=getopnd(xa)
		genmc((oldmode=tr64|m_cvttsd2si|m_cvttss2si), ax, fx)
		swapopnds(xb,ya)
		delopnd()

	else
		merrort("fix->",newmode)
	esac
end

proc px_truncate*(pcl p)=
	mcloperand ax
	int mask

	case ttsize[p.truncmode]
	when 1 then mask:=255
	when 2 then mask:=65535
	when 4 then mask:=0xFFFF'FFFF
	esac

	ax:=loadopnd(xa)
	genmc(m_andx, ax, mgenint(mask))

	genmc((ttsigned[p.truncmode]|m_movsx|m_movzx), ax, changeopndsize(ax,ttsize[p.truncmode]))
end

proc px_fwiden*(pcl p)=
	mcloperand fx
	fx:=loadopnd()
	genmc(m_cvtss2sd, fx,fx)
	pclstack[xa].loc:=xreg_loc
	pclstack[xa].cat:=x64cat
end

proc px_fnarrow*(pcl p)=
	mcloperand ax:=loadopnd(xa)
	genmc(m_cvtsd2ss, ax,ax)
!	pclstack[xa].fmt:=xreg_x32
	pclstack[xa].loc:=xreg_loc
	pclstack[xa].cat:=x32cat
end

proc px_len*(pcl p)=
	mcloperand ax

	case p.pcat
	when blockcat then			!assume slice
		ax:=getopnd()
		genmc(m_mov, mgenreg(ax.reg), applyoffset(getopnd_ind(),8))
		pclstack[xa].loc:=reg_loc
		pclstack[xa].cat:=d64cat
	else
		merrort("len",p.pmode)
	esac
end

proc px_upb*(pcl p)=
	MCERROR("UPB")
end

proc px_bounds*(pcl p)=
	merrort("bounds",p.pmode)
end

proc px_sliceptr*(pcl p)=
	mcloperand ax

	ax:=getopnd()
	genmc(m_mov, mgenreg(ax.reg), getopnd_ind())
	pclstack[xa].loc:=reg_loc
	pclstack[xa].cat:=d64cat

end

proc px_startmx*(pcl p)=
	saveopnds()
end

proc px_resetmx*(pcl p)=
	case pclstack[xa].cat
	when x64cat, x32cat then
		merror("RESETMULT/XREG")
	esac

	movetoreg(r0)

	if p.opcode=kresetmx then
		delopnd()
	fi
end

proc px_endmx*(pcl p)=
	px_resetmx(p)
end

proc px_setret*(pcl p)=
	do_setret(r0,r0)

	regset[r0]:=0
	xregset[r0]:=0
end

proc px_setretmult*(pcl p)=
	int k,wide

	k:=0

	for i:=1 to p.nret do
		++k
		do_setret(multregs[k],multxregs[k])
	od

	for i:=1 to k do
		regset[multregs[i]]:=xregset[multxregs[i]]:=0
	od
end

proc px_setcall*(pcl p)=
	saveopnds()

	if ncalldepth>=maxcalldepth then
		merror("Too many nested calls")
	fi

	++ncalldepth

	if p.nargs<=4 then
		callalign[ncalldepth]:=mstackdepth.odd
	else
		callalign[ncalldepth]:=p.nargs.odd ixor mstackdepth.odd
	fi

	if callalign[ncalldepth] then
		pushslots(1)
	fi
end

proc px_setarg*(pcl p)=
	if p.x>4 then
		pushopnd(xa)
	fi
end

proc px_dupl*(pcl p)=
	if ncalldepth then
		duploperand()
	else
		++pclstack[noperands].count
	fi

end

proc px_swapopnds*(pcl p)=
	swapopnds(xb,ya)
end

proc px_copyblock*(pcl p)=
	mcloperand ax, bx

	ax:=getopnd_ind()				!old block
	addmem(p)
	bx:=getopnd_ind()				!new block

	copyblock(bx,ax,p.psize,1)

	swapopnds(xb,ya)

	delopnd()
end

proc px_getnprocs*(pcl p)=
	mcloperand bx
	dosetfntable()
	addlabel(lab_funcnprocs)
	pclstack[xa].ilabel:=1
end

proc px_getprocname*(pcl p)=
	mcloperand ax

	dosetfntable()
	case p.pcat
	when d64cat then
		ax:=loadopnd(xa)
		genmc(m_mov, ax, mgenindex(ireg:ax.reg,scale:8,offset:-8,labno:lab_funcnametable))
	else
		merrort("gpn",p.pmode)
	esac
end

proc px_getprocaddr*(pcl p)=
	mcloperand ax

	dosetfntable()
	case p.pcat
	when d64cat then
		ax:=loadopnd(xa)
		genmc(m_mov, ax, mgenindex(ireg:ax.reg,scale:8,offset:-8,labno:lab_funcaddrtable))
	else
		merrort("gpa",p.pmode)
	esac
end

proc dobin_float(int opc)=
	mcloperand ax,bx

	ax:=loadopnd(xb)
	bx:=getopnd(ya)

	genmc(opc,ax,bx)
end

proc do_pushlowargs(int nargs, nvariadics=0, isptr=0)=
!nargs=0 to 4 /operands/, not using more than 4 slots (for 2 wides, nargs=2)
!load args to D10-13/X0-3
!does not do anything with the stack at all
! Params are categorised as follows:
! Variadic:
!   float:  load to both D and X registers
!   other:  load to D register only
! Normal/non-variadic:
!   float:  load to X register
!   other:  load to D register

	int ireg, xreg, j,k, nextireg, nextxreg

	if nargs=0 then return fi

	highargs max:=nargs

	nextireg:=r10
	nextxreg:=xr0

	k:=0
	for i:=noperands downto noperands-nargs+1 do
		++k						!counts params from 1
		j:=i-isptr
		ireg:=nextireg
		xreg:=nextxreg

		if pclstack[j].cat in [x64cat,x32cat] then
			unless nvariadics and k>=nvariadics then ireg:=0 end
		else
			xreg:=0
		fi

		if ireg then loadparam(j,ireg) fi
		if xreg then loadxparam(j,xreg) fi

		++nextireg
		++nextxreg
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
	int cond, offset
	mcloperand ax,bx

	ax:=loadopnd(xb)
	bx:=getopnd(ya)
	offset:=p.opcode-kjumpeq

	case p.pcat
	when d64cat then
		if ttsigned[p.pmode] then
			cond:=scondcodes[offset]
		else
			cond:=ucondcodes[offset]
		fi
		genmc(m_cmp,ax,bx)

	when x32cat then
		cond:=ucondcodes[offset]
		genmc(m_comiss,ax,bx)

	when x64cat then
		cond:=ucondcodes[offset]
		genmc(m_comisd,ax,bx)
	else
		merrort("jumpcc:",p.pmode)
	esac

	genmc_cond(m_jmpcc,cond, mgenlabel(p.labelno))
	delopnd()
	unless p.popone then
		delopnd()
	end
end

proc dosetcc(pcl p)=
	int cond, offset,isfloat
	mcloperand ax,bx,cx
	ref pclstackrec pc

	ax:=loadopnd(xb)
	bx:=getopnd(ya)
	isfloat:=0
	offset:=p.opcode-kseteq			!0..5

	case p.pcat
	when d64cat then
		if ttsigned[p.pmode] then
			cond:=scondcodes[offset]
		else
			cond:=ucondcodes[offset]
		fi
		genmc(m_cmp,ax,bx)

	when x32cat then
		cond:=ucondcodes[offset]
		genmc(m_comiss,ax,bx)
		isfloat:=1

	when x64cat then
		cond:=ucondcodes[offset]
		genmc(m_comisd,ax,bx)
		isfloat:=1

	else
		merrort("setcc:",p.pmode)
	esac

	if isfloat then
		cx:=mgenreg(getnextreg(),1)
		pc:=&pclstack[xb]
		if pc.loc=xreg_loc then
			freexreg(pc.reg)
		fi
		pc.loc:=reg_loc
		pc.reg:=cx.reg
		pc.cat:=d64cat

	else
		cx:=changeopndsize(ax,1)
	fi

	genmc_cond(m_setcc,cond, cx)
	genmc(m_movzx, changeopndsize(cx,4), cx)

	if isfloat then
		freereg(cx.reg)
	fi

	delopnd()
end
!
proc do_setretfloat(int destreg)=
	int currreg
	mcloperand ax,rx

	rx:=mgenxreg(destreg)

	ax:=loadopnd(xa)
	currreg:=ax.reg

	case pclstack[xa].loc
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
		merror("setretf?", locnames[pclstack[xa].loc])
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

	if pclstack[xa].cat in [x64cat,x32cat] then
		do_setretfloat(destxreg)
		return
	fi

	rx:=mgenreg(destreg)

	ax:=loadopnd(xa)
	currreg:=ax.reg

	case pclstack[xa].loc
	when reg_loc then
		if currreg<>destreg then

			if regset[destreg] then
				swapopndregs(destreg)
				genmc(m_xchg, rx, ax)
			else
				genmc(m_mov, rx, ax)
			fi
		fi
	when regvar_loc then
		genmc(m_mov, rx, ax)
		pclstack[xa].loc:=reg_loc
		pclstack[xa].reg:=destreg
		regset[destreg]:=1

	else
		PRINTLN =LOCNAMES[PCLSTACK[NOPERANDS].LOC]
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
			cats[++n]:=p.pcat
		od
		currpcl:=p-1

		for i:=n downto 1 do 
			case cats[i]
			when shortcat then
				merror("Short/wide mulret type")
			esac

			dogetretvalue_n(multregs[i],multxregs[i], cats[i])
		od

	else
		dogetretvalue_n(r0,r0,p.pcat)
		if p.pcat=shortcat then
			genmc((ttsigned[p.pmode]|m_movsx|m_movzx), mgenreg(r0),mgenreg(r0,p.psize))
		fi
	fi
end

proc dogetretvalue_n(int reg,xreg,cat)=

	case cat
	when d64cat,shortcat,blockcat then
		ADDREG0(REG)
	when x64cat then
		addxreg0(xreg,8)
	when x32cat then
		addxreg0(xreg,4)
	else
		merror("getretval/n?",catnames[cat])
	esac
end

proc do_shift(pcl p, int opc)=
	mcloperand ax
	ax:=loadopnd(xb)

	if pclstack[xa].loc=immd64_loc then
		genmc(opc, ax, mgenint(pclvals[xa].value))
	else
		if r10used then
			genmc(m_push, mgenreg(r10))
		fi
		loadparam(reg:r10)
		genmc(opc,ax, mgenreg(r10,1))
		if r10used then
			genmc(m_pop, mgenreg(r10))
		fi
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

	case p.pcat
	when d64cat then
		ax:=loadopnd(xa)
dotestint::
		genmc(m_test, ax,ax)
		genmc_cond(m_jmpcc, cond, mgenlabel(p.labelno))
	else
		merrort("jumptrue/false",p.pmode)
	esac
	delopnd()
end

proc dobitwise(pcl p, int opc)=
	mcloperand ax,bx

	case p.pcat
	when d64cat then
		ax:=loadopnd(xb)
		bx:=getopnd(ya)
!		bx:=loadopnd(ya)
		genmc(opc,ax,bx)
	else
		merrort("bitwise:",p.pmode)
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
	symbol d

	scale:=p.scale
	extra:=p.extra
	offset:=pclvals[xa].value*scale+extra	!for imm offset

	m:=p.pmode

	px:=nil

	if isregvaropnd(xb) then
		if isregvaropnd(ya) then			!regvar/regvar
			reg:=pclstack[xa].reg
			regix:=scaleregvar(reg,scale,xa)
			px:=mgenindex(areg:pclstack[xb].reg,ireg:regix, offset:extra, scale:scale)

		elsif isimm64(ya) then			!regvar/imm
			px:=mgenindex(areg:pclstack[xb].reg, offset:offset)
		else							!regvar/any
			scale:=scaleindex(bx:=loadopnd(ya),scale)
			px:=mgenindex(areg:pclstack[xb].reg, ireg:bx.reg, scale:scale, offset:extra)
		fi
	elsif ismemaddr(xb) then
		d:=pclvals[xb].def
		if isregvaropnd(ya) then			!memaddr/regvar
			reg:=pclstack[xa].reg
			regix:=scaleregvar(reg,scale,xa)
			px:=mgenindex(ireg:regix, def:d, offset:extra, scale:scale)

		elsif isimm64(ya) then			!memaddr/imm
			px:=mgenindex(def:d, offset:offset)
		else							!memaddr/any
			scale:=scaleindex(bx:=loadopnd(ya),scale)
			px:=mgenindex(ireg:bx.reg, def:d, offset:extra, scale:scale)
		fi
	else								!
		ax:=loadopnd(xb)

		if isregvaropnd(ya) then			!any/regvar
			reg:=pclstack[xa].reg
			regix:=scaleregvar(reg,scale,xa)
			px:=mgenindex(areg:ax.reg, ireg:regix, offset:extra, scale:scale)
		elsif isimm64(ya) then			!any/imm
			px:=mgenindex(areg:ax.reg, offset:offset)
		else
			scale:=scaleindex(bx:=loadopnd(ya),scale)
			px:=mgenindex(areg:ax.reg, ireg:bx.reg, scale:scale, offset:extra)

		fi
	fi

	if px.size=0 then px.size:=p.psize fi
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

	pclstack[n].loc:=reg_loc
	pclstack[n].reg:=regix
	pclstack[n].cat:=d64cat
	mulimm(ax,scale)
	scale:=1

	return regix
end

function scaleindex(mcloperand ax, int scale)int=
!when scale is 1/2/3/4, return scale unchanged
!anything else, scale value in ax, return 1
	int n
	if scale in [1,2,4,8] then return scale fi
	mulimm(ax,scale)
	return 1
end

function makeregopnd(int n)mcloperand ax=
!turn given pcl operand, which does not occupy a register,
!make it into register operand. Note that other characteristics, such
!as value/def for imm/mem/memaddr, are not affected
!offset = xa, yb etc

	pclstack[n].loc:=reg_loc
	pclstack[n].reg:=getnextreg()

	return getopnd(n)
end

proc dobinto_int(pcl p, int opc)=
	mcloperand ax,bx,rx
	int reg,size

	size:=p.psize

	if size=8 and ismemaddr(xb) then
		ax:=mgenmem(pclvals[xb].def)
		reg:=getnextreg()
		rx:=mgenreg(reg)
		genmc(m_mov, rx, ax)
		bx:=getopnd(ya)
		genmc(opc,rx,bx)
		genmc(m_mov, ax,rx)
		freereg(reg)
	else
		ax:=getopnd_ind(xb,size:size)
		bx:=loadopnd(ya,size)

		genmc(opc,ax,bx)
	fi
	delopnd()
	delopnd()
end

proc dobinto_float(pcl p, int opc)=
	mcloperand ax,bx,cx

	addreg_x64()
	ax:=getopnd_ind(xc)
	bx:=getopnd(yb)
	cx:=getopnd(za)

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
	ax:=getopnd_ind(xc,4)
	bx:=getopnd(yb)
	cx:=getopnd(za)

	genmc(m_movd, cx,ax)
	genmc(opc, cx,bx)
	genmc(m_movd, ax,cx)

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

	checkloaded(xb)

	if isdiv and pclstack[xa].loc=immd64_loc then
		n:=pclvals[xa].value
		case n
		when 0 then
			merror("Divide by zero")
		when 1 then
			delopnd()
			return
		else
			shifts:=ispoweroftwo(n)
			if shifts then
				genmc((issigned|m_sar|m_shr), getopnd(xb), mgenint(shifts))
				delopnd()
				return
			fi
		esac
	fi 

	checkloaded(ya)
	saverdx()
	fixdivopnds()

	if issigned then
		genmc(m_cqo)
		opc:=m_idiv
	else
		genmc(m_xorx, mgenreg(r11),mgenreg(r11))
		opc:=m_div
	fi

	genmc(opc, getopnd(ya,p.psize))

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

	regx:=pclstack[xb].reg
	regy:=pclstack[xa].reg

	if regx=r0 then			!regy will be OK
		return
	fi
	if regy=r0 then			!need to swap then
		genmc(m_xchg,getopnd(xb),getopnd(ya))
		swapopnds(xb,ya)		!switch operands
		return
	fi

!neither x nor y in r0
	if regset[r0]=0 then	!d0 not in use
		genmc(m_xchg,mgenreg(r0),getopnd(xb))
		regset[regx]:=0
		pclstack[xb].reg:=r0
		regset[r0]:=1
		return
	fi

!need to move current occupier of r0
	for zop:=noperands downto 1 do
		if pclstack[zop].loc=reg_loc and pclstack[zop].reg=r0 then exit fi
	od

!zop is the operand number that happens to be using r0
	genmc(m_xchg,mgenreg(r0),getopnd(xb))	
	swap(pclstack[xb].reg,pclstack[zop].reg)		!switch registers

end

proc saverdx=
	if r11used then
		genmc(m_push, mgenreg(r11))
	fi
end

proc restorerdx=
	if r11used then
		genmc(m_pop, mgenreg(r11))
	fi
end

proc doincr(pcl p, int incrop, addop)=
	if p.stepx=1 then
		if ismemaddr(xa) then
			genmc(incrop, mgenmem(pclvals[xa].def))
		else
			genmc(incrop, getopnd_ind(xa))
		fi
	else
		if ismemaddr(xa) then
			genmc(addop, mgenmem(pclvals[xa].def), mgenint(p.stepx))
		else
			genmc(addop, getopnd_ind(xa), mgenint(p.stepx))
		fi
	fi
	delopnd()
end

proc doincrload(pcl p, int incrop, addop)=
	mcloperand ax, mx

	if ismemaddr(xa) then
		mx:=mgenmem(pclvals[xa].def)
		ax:=makeregopnd(xa)
	else
		mx:=getopnd_ind(xa,p.psize)
		ax:=getopnd(xa)
	fi

	if p.stepx=1 then
		genmc(incrop, mx)
	else
		genmc(addop, mx, mgenint(p.stepx))
	fi

	if p.pcat=shortcat then
		genmc((ttsigned[p.pmode]|m_movsx|m_movzx), ax, mx)
	else
		genmc(m_mov, ax,mx)
	fi
end

proc doloadincr(pcl p, int incrop, addop)=
	mcloperand ax,mx

	if ismemaddr(xa) then
		mx:=mgenmem(pclvals[xa].def)
	else
		mx:=getopnd_ind(xa,p.psize)
	fi

	addreg_d64()
	ax:=getopnd()

	if p.pcat=shortcat then
		genmc((ttsigned[p.pmode]|m_movsx|m_movzx), ax, mx)
	else
		genmc(m_mov, ax,mx)
	fi

	if p.stepx=1 then
		genmc(incrop,mx)
	else
		genmc(addop,mx, mgenint(p.stepx))
	fi

	swapopnds(xb,ya)
	delopnd()
end

global proc do_syscall(int fnindex, nargs, retcat)=
!retcat = 0, d64cat, x64cat, x32cat, widecat
	int slots

	saveopnds(nargs)
	slots:=0

	if mstackdepth.odd then
		pushslots(1)
		slots:=1
	fi

	highargs max:=nargs
	do_pushlowargs(nargs)

	if mstackdepth then
		slots+:=4
		pushslots(4)					!shadowspace
	else
		localshadow:=1
	fi

	genmc(m_call, getsyscallname(fnindex))

	to nargs do
		delopnd()
	od

	if slots then
		popslots(slots)
	fi

	getretvalue_bycat(retcat)
end

proc getretvalue_bycat(int cat)=
	case cat
	when 0 then
		return
	when d64cat then
		addreg0(r0)
	when x64cat then
		addxreg0(r0,8)
	when x32cat then
		addxreg0(r0,4)
	else
		merror("getval bycat")
	esac
end

proc px_loadlabel*(pcl p)=
	addlabel(p.labelno)
end

proc do_shiftnto(pcl p,int opc)=
!shift opc=shl/shr/sar, when both operands are on the stack
!first operand is address of dest
	mcloperand px

	px:=getopnd_ind(xb)

	if pclstack[xa].loc=immd64_loc then
		genmc(opc, px, mgenint(pclvals[xa].value))
	else
		if r10used then merror("shiftto:cl in use") fi
		loadparam(xa,r10)
		genmc(opc, px, mgenreg(r10,1))
	fi

	delopnd()
	delopnd()
end

proc domax_float(int opc)=
	mcloperand ax,bx
	ax:=loadopnd(xb)
	bx:=getopnd(ya)
	genmc(opc,ax,bx)
	delopnd()
end

proc domax_int(int cond)=
	mcloperand ax,bx

	ax:=loadopnd(xb)
	bx:=loadopnd(ya)

	genmc(m_cmp, ax, bx)
	genmc_cond(m_cmovcc, cond, ax, bx)

	delopnd()
end

proc domaxto_r64(int cond)=
	mcloperand px,ax,bx,lx
	int lab

	px:=getopnd_ind(xb)
	bx:=loadopnd(ya)
	addreg_x64()
	ax:=getopnd(xa)

	genmc(m_movq, ax, px)

	genmc(m_comisd, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=mgenlabel(lab))
	genmc(m_movq, px,bx)
	genmc(m_labelx, lx)
	delopnd()
	delopnd()
	delopnd()
end

proc domaxto_r32(int cond)=
	mcloperand px,ax,bx,lx
	int lab

	px:=getopnd_ind(xb)
	bx:=loadopnd(ya)
	addreg_x32()
	ax:=getopnd(xa)

	genmc(m_movd, ax, px)

	genmc(m_comiss, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=mgenlabel(lab))
	genmc(m_movd, px,bx)
	genmc(m_labelx, lx)
	delopnd()
	delopnd()
	delopnd()
end

proc domaxto_int(int cond)=
	mcloperand ax,bx,lx
	int lab

	ax:=getopnd_ind(xb)
	bx:=loadopnd(ya)

	genmc(m_cmp, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=mgenlabel(lab))
	genmc(m_mov, ax,bx)
	genmc(m_labelx, lx)
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
	if stdcat[m]=d64cat then
		genmc(m_mov, rx, ax)
	elsif ttsigned[m] then
		genmc(m_movsx, rx, ax)
	else
		genmc(m_movzx, rx, ax)
	fi
end

proc storefromreg(mcloperand ax, rx, int size)=
	genmc(m_mov, ax, changeopndsize(rx,size))
end

proc domaths(pcl p, ichar opname, int nargs=1)=
	int slots

	saveopnds(nargs)
	slots:=0

	if mstackdepth.odd then
		pushslots(1)
		slots:=1
	fi

	highargs max:=nargs
	do_pushlowargs(nargs)

	if mstackdepth then
		slots+:=4
		pushslots(4)					!shadowspace
	else
		localshadow:=1
	fi

	genmc(m_call, mgenextname(opname))

	to nargs do
		delopnd()
	od

	if slots then
		popslots(slots)
	fi

	dogetretvalue(p)
end

function getsyscallname(int fnindex)mcloperand=
	symbol d:=getsysfnhandler(fnindex)

	if d=nil then
		return mgenname(sysfnnames[fnindex]+3)
	else
		return mgenmemaddr(d)
	fi
end

proc showgeninfo=
	mgeninfos("High reg:  ",getregname(highreg))
	mgeninfos("High xreg: ",getxregname(highxreg))
	mgeninfo("Bspill: ",	bspill)
	mgeninfo("Bxspill: ",	bxspill)
	mgeninfo ("Calls:     ",nproccalls)
	mgeninfos("Leaf func: ",(leafproc|"Yes"|"No"))
	mgeninfos("Local shadow: ",(localshadow|"Yes"|"No"))
	mgeninfo ("Max args:  ",highargs)
end

proc setmclentry(ref mclrec p)=
!temporarily set mcl insertion before p

	mce_oldmccodex:=mccodex
	mccodex:=p
	mce_nextmcl:=p.nextmcl
end

func resetmclentry:ref mclrec pnew =
!restore mcl insertion point to normal
!restireturn mcl instruction that followed	
	mccodex.nextmcl:=mce_nextmcl
	pnew:=mccodex
	mccodex:=mce_oldmccodex
	pnew
end

proc spillparams=
	symbol d
	mcloperand ax
	int offset:=16, regoffset:=0

	regoffset:=0

	for i to nparams do
		if regoffset>3 then exit fi
		d:=paramdefs[i]
		if d.used  then
			if not d.reg then
				ax:=mgenindex(areg:rframe, size:8, offset:d.offset)
				case ttbasetype[d.mode]
				when tr64 then
					genmc(m_movq, ax, mgenxreg(regoffset+xr0))
				when tr32 then
					genmc(m_movd, changeopndsize(ax,4), mgenxreg(regoffset+xr0))
				else
					genmc(m_mov, ax, mgenreg(regoffset+r10))
				esac
			elsif d.reg and d.reg<=r9 then		!move from pregs to bregs
				case ttbasetype[d.mode]
				when tr64 then
					genmc(m_movq, mgenxreg(d.reg), mgenxreg(regoffset+xr0))
				else
					genmc(m_mov, mgenreg(d.reg), mgenreg(regoffset+r10))
				esac
			fi
		fi

		offset+:=8
		++regoffset
	od

end

proc do_procentry1(pcl p)=
	int retmode, ntemps, hasequiv, offset
	mcloperand ax
	symbol d
	[100]char str, newname

	initpass1x(pclprocdef)

	SETMCLENTRY(MCLPROCENTRY)

	bspill:=bxspill:=bxspilloffset:=0
	if highreg>=r3 then bspill:=highreg-r2 fi		!no of regs d3..highreg
	if highxreg>=r6 then bxspill:=highxreg-r5 fi	!no of xregs x6..highxreg

	for i to nparams do
		d:=paramdefs[i]
		if not d.reg then			!not a regvar
			d.offset:=paramoffset+16+bspill*8
			genmc(m_define, mgenname(getdispname(d)), mgenint(d.offset))
		fi
		paramoffset+:=8
	od

	retmode:=currproc.mode

	for i to nlocals do
		d:=localdefs[i]

		if d.atvar then
			hasequiv:=1
        elsif not d.reg then
			frameoffset-:=roundsizetg(ttsize[d.mode])
			d.offset:=frameoffset
			genmc(m_define, mgenname(getdispname(d)), mgenint(d.offset))
		fi
	od

	ntemps:=0
	for i to maxoperands when pcltempflags[i] do
		++ntemps
		frameoffset-:=8
		ax:=pcltemps[i]
		ax.offset:=frameoffset
		genmc(m_define, mgenname(gettempname(currproc,i)), mgenint(ax.offset))
	od

	if bxspill then
		frameoffset-:=bxspill*8
		bxspilloffset:=frameoffset
	fi

	if currproc.isthreaded then
		if nlocals or nparams then merror("Threaded proc has locals/params") fi
		if ntemps then merror("Threaded proc has temps") fi
		if bspill or bxspill then merror("Threaded proc has spill regs") fi
		resetmclentry()
		return
	fi

	framebytes:=-frameoffset

	if bspill.odd then				!need an even number to keep stack alighnment correct
		unless framebytes iand 8 then
			framebytes+:=8
		end
	else
		if framebytes iand 8 then
			framebytes+:=8
		fi
	fi

	if localshadow then
		framebytes+:=32				!shadow space
	fi

!spill any bregs
	if bspill then
		for r:=r3 to highreg do
			genmc(m_push, mgenreg(r))
		od
	fi

	if framebytes or nparams then
		genmc(m_push, dframeopnd)
		genmc(m_mov, dframeopnd, dstackopnd)
		pushstack(framebytes)
	fi

	spillparams()

	if bxspill then
		offset:=bxspilloffset
		for r:=xr6 to highxreg do
			ax:=mgenindex(areg:rframe, size:8, offset:offset)
			offset+:=8
			genmc(m_movq, ax, mgenxreg(r))
		od
	fi
!	mgencomment("... END PROC ENTRY CODE")
	RESETMCLENTRY()
end

proc do_procexit1=
	mcloperand ax
	int offset

	if currproc.isthreaded then
		genmc(m_ret)				!in case no jump out exists
		return
	fi

	if bxspill then
!		MGENCOMMENT("UNSPILL XREGS:")
		offset:=bxspilloffset
		for r:=xr6 to highxreg do
			ax:=mgenindex(areg:rframe, size:8, offset:offset)
			offset+:=8
			genmc(m_movq, mgenxreg(r), ax)
		od
	fi

	if framebytes or nparams then
		popstack(framebytes)
		genmc(m_pop, dframeopnd)
	fi

	if bspill then
		for r:=highreg downto r3 do
			genmc(m_pop, mgenreg(r))
		od
	fi

	genmc(m_ret)
end

proc do_endproc1(pcl p)=
!Pass 1 just completed

	genmc(m_procend)
	checkopnds()

	if fregoptim and not assemused then		!then Pass 2
		initpass2()							!Also sets up regvars

		currpcl:=pclprocdef					!will skip to following op

		mccodex:=mclprocentry				!redo mcl code (also proc entry injection point)
		mccodex.nextmcl:=nil
	elsif fpeephole then					!one pass only
		peephole(mclprocentry)
	fi
end

proc do_procentry2(pcl p)=
	int retmode, ntemps, hasequiv, offset
	mcloperand ax
	symbol d
	[100]char str, newname

	SETMCLENTRY(MCLPROCENTRY)

	bspill:=bxspill:=bxspilloffset:=0
	if highreg>=r3 then bspill:=highreg-r2 fi		!no of regs d3..highreg
	if highxreg>=r6 then bxspill:=highxreg-r5 fi	!no of xregs x6..highxreg

	for i to nparams do
		d:=paramdefs[i]
		if not d.reg then			!not a regvar
			d.offset:=paramoffset+16+bspill*8
			genmc(m_define, mgenname(getdispname(d)), mgenint(d.offset))

		elsif stdcat[getpclmode(d.mode)]=d64cat then
			genmc(m_definereg, mgenname(getdispname(d)), mgenreg(d.reg))
		else
			genmc(m_definereg, mgenname(getdispname(d)), mgenxreg(d.reg))
		fi
		paramoffset+:=8
	od

	retmode:=currproc.mode

	for i to nlocals do
		d:=localdefs[i]
		if d.atvar then
			hasequiv:=1
        elsif not d.reg then
			frameoffset-:=roundsizetg(ttsize[d.mode])
			d.offset:=frameoffset
			genmc(m_define, mgenname(getdispname(d)), mgenint(d.offset))
		elsif stdcat[getpclmode(d.mode)]=d64cat then
			genmc(m_definereg, mgenname(getdispname(d)), mgenreg(d.reg))
		else
			genmc(m_definereg, mgenname(getdispname(d)), mgenxreg(d.reg))
		fi
	od
	ntemps:=0
	for i to maxoperands when pcltempflags[i] do
		++ntemps
		frameoffset-:=8
		ax:=pcltemps[i]
		ax.offset:=frameoffset
		genmc(m_define, mgenname(gettempname(currproc,i)), mgenint(ax.offset))
	od

	if bxspill then
		frameoffset-:=bxspill*8
		bxspilloffset:=frameoffset
	fi

	if currproc.isthreaded then
		if nlocals or nparams then merror("Threaded proc has locals/params") fi
		if ntemps then merror("Threaded proc has temps") fi
		if bspill or bxspill then merror("Threaded proc has spill regs") fi
		resetmclentry()
		return
	fi

	framebytes:=-frameoffset

	if bspill.odd then				!need an even number to keep stack alighnment correct
		unless framebytes iand 8 then
			framebytes+:=8
		end
	else
		if framebytes iand 8 then
			framebytes+:=8
		fi
	fi

	if localshadow then
		framebytes+:=32				!shadow space
	fi

!spill any bregs
	if bspill then
		for r:=r3 to highreg do
			genmc(m_push, mgenreg(r))
		od
	fi

	if framebytes or nparams then
		genmc(m_push, dframeopnd)
		genmc(m_mov, dframeopnd, dstackopnd)
		pushstack(framebytes)
	fi

	spillparams()

	if bxspill then
!		MGENCOMMENT("SPILL XREGS:")
		offset:=bxspilloffset
		for r:=xr6 to highxreg do
			ax:=mgenindex(areg:rframe, size:8, offset:offset)
			offset+:=8
			genmc(m_movq, ax, mgenxreg(r))
		od
	fi

!	mgencomment("... END PROC ENTRY CODE")
	RESETMCLENTRY()
end

proc do_procexit2=
	mcloperand ax
	int offset

	if currproc.isthreaded then
		genmc(m_ret)				!in case no jump out exists
		return
	fi

	if bxspill then
!		MGENCOMMENT("UNSPILL XREGS:")
		offset:=bxspilloffset
		for r:=xr6 to highxreg do
			ax:=mgenindex(areg:rframe, size:8, offset:offset)
			offset+:=8
			genmc(m_movq, mgenxreg(r), ax)
		od
	fi

	if framebytes or nparams then
		popstack(framebytes)
		genmc(m_pop, dframeopnd)
	fi

	if bspill then
		for r:=highreg downto r3 do
			genmc(m_pop, mgenreg(r))
		od
	fi

	genmc(m_ret)
end

proc do_endproc2(pcl p)=
!Pass 2 just completed

!	showgeninfo()

	genmc(m_procend)
	checkopnds()

	if fpeephole then
		peephole(mclprocentry)
	fi

	passno:=1
end

proc px_loadall*(pcl p)=
	checkallloaded()
end
