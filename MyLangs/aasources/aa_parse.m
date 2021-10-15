import* aa_common

ref strec exprlabeldef
int64 exprvalue
int exprtype

global proc readmodule(int moduleno)=
	ref strec symptr
	int sym

	initsourcefile(moduletable[moduleno].source)

	lxsymbol:=eolsym

	genmc(m_segment,genint(code_seg))

	while lxsymbol=eolsym do

		lex()

		switch lxsymbol
		when kopcodesym then
			readinstr()

		when namesym then
			symptr:=lxsymptr
			lex()
			sym:=lxsymbol
			case sym
			when eqsym then
				lex()
				case lxsymbol
				when kregsym then
					createregalias(symptr,lxsymptr.subcode, lxsymptr.regsize)
					lex()
				when kxregsym then
					createxregalias(symptr,lxsymptr.subcode)
					lex()
				else
					createnamedconst(symptr,readexpression())
				esac

			when colonsym,dcolonsym then
				createlabel(symptr,(sym=colonsym|localsym|exportedsym))
				genmc(m_label, genlab(symptr))
				symptr.reftype:=fwd_ref
				lxsymbol:=eolsym
				redo
			else
				println symptr.name
				serror("colon expected after label")
			esac

		when fwdlocalsym then
			symptr:=lxsymptr
			lex()
			case lxsymbol
			when eqsym then
				serror_s("Redefining label as const: %s",symptr.name)
			when colonsym,dcolonsym then
				symptr.fwdrefs:=nil
				genmc(m_label, genlab(symptr))
				symptr.symbol:=(lxsymbol=colonsym|localsym|exportedsym)
				symptr.reftype:=fwd_ref
				lxsymbol:=eolsym
				redo
			else
				serror("Instruction expected")
			esac

		when importedsym then
			serror_s("Defining imported name: %s",symptr.name)
		when localsym, exportedsym then
			serror("Redefining symbol")
		when namedconstsym then
			serror_s("2:Const redefined: %s",symptr.name)

		when kjmpccsym then
			readcondinstr(m_jmpcc)

		when ksetccsym then
			readcondinstr(m_setcc)

		when kmovccsym then
			readcondinstr(m_cmovcc)

		when eolsym then			!blank or comment line
		when eofsym then
			return
		else
			println "Unknown symbol (possibly redefining regvar):",symbolnames[lxsymbol]
		end switch
	od
	serror("EOL expected")
end

global proc checkundefined=
	int i
	ref strec d

	d:=modulenamelist
	while d do
		if d.symbol=fwdlocalsym then
			println "Undefined:",padstr(d.name,20)
			++nundefined
		fi
		d:=d.nextdef
	od
end

proc checksymbol(int symbol)=
	[265]char str

	if lxsymbol<>symbol then
		fprint @&.str,"# expected not #",symbolnames[symbol],symbolnames[lxsymbol]

		serror(&.str)
	fi
end

proc readinstr=
!deal with opcode symbol
	int opcode
	ref opndrec a,b,c

	opcode:=lxsubcode

	lex()

	switch opcode
	when m_db, m_dw, m_dd, m_dq, m_ddoffset then
		do
			if lxsymbol=stringconstsym then
				a:=genstrimm(lxsvalue)
				lex()
				genmc(opcode,a)
			else
				a:=readoperand()
				genmc(opcode,a)
			fi
			if lxsymbol=commasym then
				lex()
			else
				exit
			fi
		od
	when m_segment then
		checksymbol(ksegnamesym)
		genmc(m_segment,genint(lxsubcode))
		lex()

	when m_isegment then
		genmc(m_segment, genint(idata_seg))
	when m_zsegment then
		genmc(m_segment, genint(zdata_seg))
	when m_csegment then
		genmc(m_segment, genint(code_seg))

	when m_imul3 then
		a:=readoperand()
		checksymbol(commasym)
		lex()
		b:=readoperand()
		checksymbol(commasym)
		lex()
		c:=readoperand()
		SERROR("IMUL3 CAN'T DO 3 OPNDS")

	when m_pcmpistri,m_pcmpistrm then
		a:=readoperand()
		checksymbol(commasym)
		lex()
		b:=readoperand()
		checksymbol(commasym)
		lex()
		c:=readoperand()
		if c.mode<>a_imm then serror("pcmpistr/not int") fi
		genmc(opcode,a,b)
		mccodex.c:=c.value

	when m_proc then
		while lxsymbol<>eolsym do lex() od
	else
		a:=b:=nil
		if lxsymbol<>eolsym then
			a:=readoperand()
			if lxsymbol=commasym then
				lex()
				b:=readoperand()
			fi
		fi 

		genmc(opcode,a,b)
	end

end

proc readcondinstr(int opc)=
	ref opndrec a,b

	a:=genint(lxsubcode)
	lex()
	b:=readoperand()

	if lxsymbol=commasym and opc=m_cmovcc then		!ignore dest
		genmc(m_param,b)							!store extra param as separate instr

		lex()
		b:=readoperand()
	fi

	genmc(opc,a,b)
end

function readoperand:ref opndrec=
!position at start of operand
!read reg, expression or complex operand, and return an opndrec
	ref opndrec p
	int size

	switch lxsymbol
	when kregsym then
		p:=regtable[lxsubcode, lxsymptr.regsize]
		lex()
		return p
	when lsqsym then
		lex()
		return readaddrmode(0)
	when kxregsym then
		p:=genxreg(lxsubcode)
		lex()
		return p
	when kprefixsym then
		size:=lxsubcode
		lex()
		checksymbol(lsqsym)
		lex()
		return readaddrmode(size)

	else
		return readexpression()
	end
	return nil
end

function readexpression:ref opndrec=
	ref strec labelx
	int64 valuex
	int typex

	readterm()

	docase lxsymbol
	when addsym then
		labelx:=exprlabeldef
		valuex:=exprvalue
		typex:=exprtype
		lex()
		readterm()
		if exprlabeldef then serror("+label?") fi
		exprlabeldef:=labelx
		if typex or exprtype then serror("add real") fi
		exprvalue+:=valuex

	when subsym then
		labelx:=exprlabeldef
		valuex:=exprvalue
		typex:=exprtype
		lex()
		readterm()
		if exprlabeldef then serror("+label?") fi
		exprlabeldef:=labelx
		if typex or exprtype then serror("sub real") fi
		exprvalue:=valuex-exprvalue
	when mulsym then
		labelx:=exprlabeldef
		valuex:=exprvalue
		typex:=exprtype
		lex()
		readterm()
		if exprlabeldef then serror("+label?") fi
		exprlabeldef:=labelx
		if typex or exprtype then serror("add real") fi
		exprvalue*:=valuex

	else
		exit
	end

	return genimm_expr(exprlabeldef,exprvalue,exprtype)
end

proc readterm=
!read term into exprlabeldef/exprvalue
	ref strec symptr
	real x
	exprlabeldef:=nil
	exprvalue:=0
	exprtype:=0

	switch lxsymbol
	when fwdlocalsym, localsym, exportedsym then
		exprlabeldef:=lxsymptr
		lex()
		if lxsymbol=mulsym then		!is extern name
			serror("* applied to non-extern label or applied inconsistently")
		fi

	when importedsym then
		exprlabeldef:=lxsymptr
		lex()
		if lxsymbol<>mulsym then		!is extern name
			serror("* missing or applied inconsistently")
		fi
		lex()

	when namedconstsym then
		exprlabeldef:=lxsymptr.expr.labeldef
		exprvalue:=lxsymptr.expr.value
		exprtype:=lxsymptr.expr.valtype

!		p:=lxsymptr.value
		lex()
	when namesym then
		symptr:=lxsymptr
		exprlabeldef:=symptr
		lex()
		if lxsymbol=mulsym then		!is extern name
			createlabel(symptr,importedsym)
			lex()
		else
			createlabel(symptr,fwdlocalsym)
		fi

	when intconstsym then
		exprvalue:=lxvalue
		lex()
	when realconstsym then
		exprvalue:=int64@(lxxvalue)
		exprtype:='R'
		lex()

	when subsym then
		lex()
		readterm()
		if not exprlabeldef then
			if not exprtype then
				exprvalue:=-exprvalue
			else
				x:=-(real@(exprvalue))
				exprvalue:=int64@(x)
			fi
		else
			serror("neg/label")
		fi
	when addsym then
		lex()
		readterm()

	else
		serror("READTERM")
	end
end

proc readreg(int &reg,&regsize,&scale)=
!positioned at reg symbol
!read R or R*n for address mode
!return (reg, regsize, scale); scale is 0 when no *n

	reg:=lxsubcode
	regsize:=lxsymptr.regsize
	lex()
	if lxsymbol=mulsym then
		lex()
		checksymbol(intconstsym)
		case lxvalue
		when 1,2,4,8 then
		else
			serror("*n must be 1,2,4,8")
		esac
		scale:=lxvalue
		lex()
	else
		scale:=0
	fi
end

function readaddrmode(int size)ref opndrec=
![" just seen, positioned at next symbol
!syntax: [Reg+Reg*scale+expr], all items optional, but at least one must be present
!read optional reg, index reg, scale factor, label, and offset
!size is 0, or is 1,2,4,8 when an override was used
	int reg,regsize,scale,regix, addrsize, regixsize, scaleix
	ref opndrec x
	ref opndrec p

	reg:=regix:=0
	regsize:=regixsize:=0
	scale:=scaleix:=0
	x:=nil

	if lxsymbol=kregsym then
		readreg(reg,regsize,scale)
!	n:=1
		case lxsymbol
		when addsym then
			lex()
			if lxsymbol=kregsym then
				readreg(regix,regixsize,scaleix)

				case lxsymbol
				when addsym,subsym then
					x:=readexpression()
				esac

			else
				x:=readexpression()
			fi
		when subsym then
			x:=readexpression()
		esac
	else
		x:=readexpression()
	fi

	if scale and scaleix then serror("Two *N scales") fi
!if reg=0 and regix=0 and 0 then	serror("Empty address mode") fi
	checksymbol(rsqsym)
	lex()

	if scale and not scaleix then
		swap(reg,regix)
		swap(regsize,regixsize)
		swap(scale,scaleix)
	fi
	if scaleix=0 then scaleix:=1 fi

	if regsize and regixsize and regsize<>regixsize then serror("Addr reg size mismatch") fi

	p:=genindex(areg:reg, ireg:regix, scale:scaleix, x:x, size:size,
		addrsize:(regsize=4 or regixsize=4|4|8))
	return p
end
