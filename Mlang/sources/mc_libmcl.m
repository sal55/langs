const fasmformat=1
!const fasmformat=0

const fuseregtable=1
!const fuseregtable=0

const targetsize=8

global int fshowmsource=0

int mclseqno

global macro isframex(d) = (d.nameid in [frameid, paramid])

global proc mclinit=
	mcloperand a
	int r,s

	for r:=r0 to r15 do
		regtable[r,1]:=mgenreg0(r,1)
		regtable[r,2]:=mgenreg0(r,2)
		regtable[r,4]:=mgenreg0(r,4)
		regtable[r,8]:=mgenreg0(r,8)
	od

	for i in frameregtable.bounds do
		a:=newmclopnd()
		a.mode:=a_mem
		a.reg:=rframe
		a.size:=8
		a.offset:=i
		frameregtable[i]:=a
	end

	dframeopnd:=mgenreg(rframe,8)
	dstackopnd:=mgenreg(rstack,8)

	initmcdest()

	setsegment('C')

lab_funcnametable:=0
	lab_funcaddrtable:=0

end

global proc initmcdest=
!reset mccode/mccodex
!called should have saved any values from last linked list 
	mccode:=mccodex:=nil
!	clear rtsproclabels
end

export proc genmc(int opcode, mcloperand a=nil,b=nil)=
	ref mclrec m, oldm
	int labno

	m:=pcm_allocz(mclrec.bytes)
	m.opcode:=opcode
	m.seqno:=++mclseqno

	m.a:=a
	m.b:=b

	case opcode
	when m_call then
		++nproccalls

	when m_lea then
		if b and b.valtype=def_val then
			b.def.addrof:=1
		fi
	when m_labelx then
		labno:=a.labelno

	esac

	if mccode then
		mccodex.nextmcl:=m
		mccodex:=m
	else
		mccode:=mccodex:=m
	fi
end

global proc genmc_cond(int opcode, cond, mcloperand a=nil,b=nil)=
	genmc(opcode,a,b)
	mccodex.cond:=cond
end

global proc genmc_str(int opcode,ichar s)=
!as genmc but uses a single immediate string operand
	genmc(opcode,mgenstring(s))
end

function newmclopnd:mcloperand a=
	a:=pcm_allocz(mclopndrec.bytes)
	return a
end

global function duplopnd(mcloperand a)mcloperand=
	mcloperand b
	b:=pcm_alloc(mclopndrec.bytes)
	b^:=a^
	return b
end

export function mgenxreg(int xreg,size=8)mcloperand=
	mcloperand a

!	if xreg=rnone then xreg:=++currxregno fi
	a:=newmclopnd()

	a.mode:=a_xreg
	a.reg:=xreg
	a.size:=size
	return a
end

export function mgenindex(int areg=0,ireg=0,scale=1,offset=0,size=0, labno=0, symbol def=nil)mcloperand=
!construct a mem address mode
	mcloperand a
	a:=newmclopnd()

	a.mode:=a_mem
	a.reg:=areg

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
		fi
	fi

	return a
end

global function getmclstr:ref strbuffer=
!write all mcl code in system by scanning all procs
!mcl code is only stored per-proc
	symbol d,e
	ref mclrec m
	[32]char str2,str3
	int i

	gs_init(dest)

	for i to nlibfiles when libfiles[i]^<>'$' do
		asmstr("          ")
		asmstr((libtypes[i]='D'|"importdll "|"importlib "))
		asmstr(libfiles[i])
		gs_line(dest)
	od

	m:=mccode
	i:=1
	while m do
		writemcl(i,m)
		++i
		m:=m.nextmcl
	od

	return dest
end

global proc mgencomment(ichar s)=
!if not debugmode then return fi
	if s=nil or s^=0 then
		genmc(m_blank)
	else
		genmc_str(m_comment,s)
	fi
end

global function mgenstring(ichar s,int length=-1)mcloperand=
	mcloperand a
	a:=newmclopnd()
	a.mode:=a_imm
	if length<0 then
		length:=strlen(s)
	fi
	a.svalue:=pcm_alloc(length+1)
	memcpy(a.svalue,s,length)
	(a.svalue+length)^:=0

	a.valtype:=stringimm_val
	a.size:=8
	return a
end

global function mgenname(ichar s)mcloperand=
	[64]char str
	mcloperand a
	a:=newmclopnd()
	a.mode:=a_imm
	a.svalue:=pcm_copyheapstring(s)
	a.valtype:=name_val
	a.size:=8

	return a
end

proc writemcl(int index,ref mclrec mcl)=

	case mcl.opcode
	when m_deleted then
	else
		strmcl(mcl)
		gs_line(dest)
	esac
end

global proc strmcl(ref mclrec mcl)=
	static [512]char str
	[128]char opcname
	mcloperand a,b
	int opcode,cond,sizepref
	ichar s,comment
	symbol d

	opcode:=mcl.opcode

	cond:=mcl.cond
	a:=mcl.a
	b:=mcl.b
	comment:=nil

	case opcode
	when m_procstart then
		asmstr(";Proc ")
		asmstr(a.def.name)
		currasmproc:=a.def

		return

	when m_procend then
		asmstr(";End ")
		currasmproc:=nil

		return

	when m_blank then
		return
	when m_comment then
		asmchar(';')
		asmstr(a.svalue)
		GOTO DOCOMMENTS
		return
	when m_deleted then
		asmstr("; <deleted>")
		GOTO DOCOMMENTS
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

		if d.scope=export_scope then
			asmstr("\n")
			asmstr(d.name)
			asmstr("::")
		fi
		return

	when m_labelx then
		fprint @&.str,"L#:",a.value
		asmstr(&.str)
		return

	when m_define then
		asmstr("          ")
		asmstr(a.svalue)
		asmstr(" = ")
		asmopnd(b)
		return

	when m_definereg then
		asmstr("          ")
		asmstr(a.svalue)
		asmstr(" = ")

		case b.mode
		when a_reg then
			asmstr(getregname(b.reg, b.size))
		else
			asmstr(getxregname(b.reg, b.size))
		esac
		return

	esac

	case opcode
	when m_jmpcc then
		print @&.opcname,"j",,asmcondnames[cond]

	when m_setcc then
		print @&.opcname,"set",,asmcondnames[cond]

	when m_cmovcc then
		print @&.opcname,"cmov",,asmcondnames[cond]

	when m_call then
		strcpy(&.opcname,"call")
	when m_andx then
		strcpy(&.opcname,"and")
	when m_orx then
		strcpy(&.opcname,"or")
	when m_xorx then
		strcpy(&.opcname,"xor")
	when m_notx then
		strcpy(&.opcname,"not")

	ELSIF OPCODE>M_HALT THEN
		STRCPY(&.OPCNAME,STRINT(OPCODE))

	else
		strcpy(&.opcname,mclnames[opcode]+2)
	esac
	ipadstr(&.opcname,10," ")

	if not fasmformat then
		if a and b then
			fprint @&.str,"  #/#",a.size,b.size
		elsif a then
			fprint @&.str,"  #",a.size
		else
			strcpy(&.str,"  ")
		fi
	else
		strcpy(&.str,"  ")
	fi

	ipadstr(&.str,10)

	strcat(&.str,&.opcname)

	asmstr(&.str)

	if a and b then		!2 operands
		sizepref:=needsizeprefix(opcode,a,b)
!
		asmopnd(a,sizepref)
		asmstr(",	")
		asmopnd(b,sizepref)

!		ASMSTR("; ")
!		ASMSTR(strint(a.size))
!		ASMSTR(" ")
!		ASMSTR(strint(b.size))

	elsif a and a.mode then								!1 operand
		if opcode=m_call then
			asmopnd(a,0)
		else
			asmopnd(a,1)
		fi
	fi

DOCOMMENTS::

end

global proc asmopnd(mcloperand a,int sizeprefix=0,debug=0)=
	asmstr(stropnd(a,sizeprefix,debug))
end

global proc setsegment(int seg,align=1)=
!seg is 'D', 'Z', 'C', 'R' for data, zdata, code, rdata
	int opc,oldalign

	if seg<>currsegment then
		case seg
		when 'I' then opc:=m_isegment
		when 'Z' then opc:=m_zsegment
		when 'C' then opc:=m_csegment
		when 'R' then MERROR("CAN'T DO RODATA SEG")
		else
			MERROR("BAD SEG CODE")
		esac
		if mccodex and mccodex.opcode in [m_isegment,m_zsegment,m_csegment] then
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
		genmc(m_align,mgenint(align))
	fi
end

global function getsizeprefix(int size,enable=0)ichar=
	if not enable then return "" fi
	case size
	when 1 then return "byte "
	when 2 then return "word16 "
	when 4 then return "word32 "
	when 8 then return "word64 "
	esac
	return ""
end

global function needsizeprefix(int opcode,mcloperand a,b)int=
	case opcode
	when m_movsx, m_movzx, m_cvtsi2ss, m_cvtsi2sd then
		return 1

	when m_cvtss2si,m_cvtsd2si, m_cvttss2si,m_cvttsd2si then
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

global function changeopndsize(mcloperand a,int size)mcloperand=
	mcloperand b

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

global function makeopndind(mcloperand a,int size=0)mcloperand=
	mcloperand b

	if a.mode<>a_reg then
		merror("makeopndind")
	fi

	return mgenireg(a.reg,size)
end

global function applyoffset(mcloperand a,int offset,int size=0)mcloperand=
!astr is an asm operand
!add possible byte offset
	mcloperand b

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

export function mgenint(int64 x,int size=8)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_imm

	a.value:=x
	a.valtype:=intimm_val
	a.size:=size

	return a
end

global function mgenrealmem(real64 x,int size=8)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_mem
	if size=8 then
		a.value:=getrealindex(x)
	else
		a.value:=getreal32index(x)
	fi
	a.valtype:=label_val
	a.size:=size
	return a
end

global function mgenrealimm(real64 x,int size=8)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_imm
	a.xvalue:=x
	a.valtype:=realimm_val
	a.size:=size
	return a
end

export function mgenlabel(int x=0)mcloperand a=
!x is a label index
!generate immediate operand containing label
	a:=newmclopnd()
	a.mode:=a_imm

	if x=0 then x:=++mlabelno fi
	a.value:=x
	a.valtype:=label_val

	return a
end

global function mgenlabelmem(int x)mcloperand a=
!x is a label index
!generate immediate operand containing label

	a:=mgenlabel(x)
	a.mode:=a_mem
	return a
end

global function mgenregvar(symbol d)mcloperand a=
	a:=mgenreg(d.reg,8)
	isregvar[d.reg]:=1

	return a
end

global function mgenxregvar(symbol d)mcloperand a=
	a:=mgenxreg(d.reg)
	isxregvar[d.reg]:=1

	return a
end

global function mgenmem(symbol d)mcloperand a=
	int reg

	if d.reg then
		if ttisreal[d.mode] then
			return mgenxregvar(d)
		else
			return mgenregvar(d)
		fi
	fi

	reg:=rnone
	if isframex(d) then
!		if not foptim and (int(d.offset) in -128..64) and ttsize[d.mode]=8 then
!			return frameregtable[d.offset]
!		fi

		reg:=rframe
	fi

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.def:=d
	++d.nrefs
	a.valtype:=def_val

	a.size:=min(ttsize[d.mode],8)

	return a
end

export function mgenmemaddr(symbol d)mcloperand=
	mcloperand a

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

export function mgenreg(int reg,size=8)mcloperand=
	if fuseregtable then
		return regtable[reg,size]
	fi
	return mgenreg0(reg,size)
end

global function mgenreg0(int reg,size=8)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_reg
	a.reg:=reg
	a.size:=size
	return a
end

global function mgenireg(int reg,size=8,offset=0)mcloperand=
	mcloperand a

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.size:=size
	a.offset:=offset

	return a
end

global func mgentemp(int n)mcloperand a=
	int reg, size

	if pcltempflags[n] then			!already in use
		return pcltemps[n]
	fi

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=rframe
	a.valtype:=temp_val
	a.size:=8
	a.tempno:=n

	pcltemps[n]:=a
	pcltempflags[n]:=1

	return a
end

global function roundsizetg(int size)int=
!make sure size is round up to next multiple of targetsize (4 or 8)
	while size iand (targetsize-1) do ++size od
	return size
end

global function getregname(int reg,size=8)ichar=
	static [1..17]ichar prefix=("B","W","","A","","","","D","","","","","","","","Q","N")
	static [32]char str
	[16]char str2
	ichar rs
	int size2

	size2:=size
	if size2>16 then
		size2:=17
	FI

	case reg
	when rnone then return "-"
	when rframe then rs:="frame"
	when rstack then rs:="stack"
	else
		getstrint(reg-r0,&.str2)
		rs:=&.str2
	esac

	print @&.str,prefix[size2],,rs
	return &.str
end

global function getxregname(int reg,size=8)ichar=
	static [32]char str

	if reg=rnone then return "-" fi

	if fasmformat then
		print @&.str,"XMM",,reg-xr0
	else
		print @&.str,(size=8|"DX"|"SX"),,reg-xr0
	fi
	return &.str
end

global function sameoperand(mcloperand a,b)int=
	return memcmp(a,b,mclopndrec.bytes)=0
end

global function sameregopnd(mcloperand a,b)int=
!check if same register operand
	unless a.mode=b.mode=a_reg then return 0 end
	return a.reg=b.reg
end

global function getstringindex(ichar s)int=
	if s=nil then			!assume nil
		kk0used:=++mlabelno
		return kk0used
	fi

	if cstringlist and eqstring(cstringlist.svalue,s) then
		return cstringlist.labelno
	fi

	return addconst(cstringlist, cast(s))
end

global function addconst(ref constrec &clist, int value)int=
	ref constrec p
	p:=pcm_allocz(constrec.bytes)
	p.value:=value
!CPL "ADDCONST",MLABELNO+1
	p.labelno:=++mlabelno
	p.nextconst:=clist
	clist:=p
	return mlabelno
end

global function getrealindex(real x)int=
	return addconst(creallist,cast@(x,int))
end

global function getreal32index(real x)int=
	return addconst(creal32list,cast@(x,int))
end

proc asmstr(ichar s)=
	gs_str(dest,s)
end

proc asmchar(int c)=
	gs_char(dest,c)
end

global function getdispname(symbol d)ichar=
	static [256]char str

	ichar name:=getfullname(d)

	if d.reg then
		fprint @str,"#.#",(d.isfloat|"X"|"R"), name
		return str
	fi

	if d.truename and d.isimport then
		strcpy(str,"`")
		strcat(str,d.truename)
		strcat(str,"*")
	elsif d.isimport then
		strcpy(str,name)
		strcat(str,"*")
	else		
		return name
	fi

	return str

end 

global function gettempname(symbol d, int n)ichar=
	static [128]char str

	fprint @str,"#.$P#",getdispname(d),n
	str
end

export proc merror(ichar mess,ichar param="")=
	fprintln "MCL Error: # (#) on Line: # in #",mess,param,
!	fprintln "MCL Error: # (#) [#]",mess,param,
		getlineno(mlineno), sourcefilenames[getfileno(mlineno)]
!	PRINTLN
!	STOP 1
	stopcompiler(sourcefilespecs[getfileno(mlineno)],getlineno(mlineno))
end

export proc merrort(ichar mess,int t)=
	[300]char str
	fprint @str, "MCL Type not supported for (#)",mess
	merror(str, ttname[t])
!	fprintln "MCL Type not supported: # (#) [#]",mess,ttname[t]
!	PRINTLN
!	STOP 1
!	stopcompiler(sourcefilepaths[getfilenpmlineno>>24],mlineno iand 16777215)
end

global proc merroropnd(ichar mess,int opndtype)=
	fprintln "MCL Opnd not supported: # (#) [#]",mess,opndnames[opndtype]
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mlineno>>24],mlineno iand 16777215)
end

global proc genstringtable=
	ref constrec p

	return unless cstringlist

	mgencomment("String Table")

	setsegment('I',8)
!	setsegment('C',8)

	if kk0used then
		genmc(m_labelx,mgenlabel(kk0used))
		gendb(0)
	fi

	p:=cstringlist
	while p, p:=p.nextconst do
		genmc(m_labelx,mgenlabel(p.labelno))
		genstring(p.svalue,1)
	od
end

global proc genstring(ichar s, int doterm)=
!string table generated in ax pass, so is just text
!this is target-specific, so should really be moved
	int i, c, seqlen, length
	ref char seq

	length:=strlen(s)

	if length=0 then
		gendb(0)
		return
	fi

	seqlen:=0

	to length do
		c:=s++^
		if c<32 or c>=127 or c='\"' then
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
	if doterm then
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

proc gendq(int a)=
	genmc(m_dq,mgenint(a))
end

global proc genrealtable=
	ref constrec p

	return unless creallist or creal32list

	mgencomment("Real Table")
	setsegment('I',8)
	p:=creallist
	while p, p:=p.nextconst do
		genmc(m_labelx,mgenlabel(p.labelno))

		if p.xvalue=infinity then
			genmc(m_dq, mgenint(u64@(p.xvalue)))
		else
			genmc(m_dq, mgenrealimm(p.xvalue,8))
		fi
	od

	mgencomment("Real32 Table")
	p:=creal32list
	while p, p:=p.nextconst do
		genmc(m_labelx,mgenlabel(p.labelno))
		if p.xvalue=infinity then
			genmc(m_dd, mgenint(int@(real32(p.xvalue))))
		else
			genmc(m_dd, mgenrealimm(p.xvalue,4))
		fi

	od
end

global proc genabsneg=
	setsegment('I',16)

	if lababs32 then
		mgencomment("lababs32")
		genmc(m_labelx,mgenlabel(lababs32))
		gendq(0x7FFF'FFFF'7FFF'FFFF)
		gendq(0x7FFF'FFFF'7FFF'FFFF)
	fi
	if lababs64 then
		mgencomment("lababs64")
		genmc(m_labelx,mgenlabel(lababs64))
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
	fi

	if labneg32 then
		mgencomment("labneg32")
		genmc(m_labelx,mgenlabel(labneg32))
		gendq(0x8000'0000'8000'0000)
		gendq(0x8000'0000'8000'0000)
	fi
	if labneg64 then
		mgencomment("labneg64")
		genmc(m_labelx,mgenlabel(labneg64))
		gendq(0x8000'0000'0000'0000)
		gendq(0x8000'0000'0000'0000)
	fi

	if labzero then
		mgencomment("labzero")
		genmc(m_labelx,mgenlabel(labzero))
		gendq(0)
	fi

	if labmask63 then
		mgencomment("mask63/offset64")
		genmc(m_labelx,mgenlabel(labmask63))
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		genmc(m_labelx,mgenlabel(laboffset64))
		gendq(0x43E0'0000'0000'0000)
	fi
end

global function mcreatefwdlabel:int =
	return ++mlabelno
end

global proc mdefinefwdlabel(int lab) =
	genmc(m_labelx,mgenlabel(lab))
end

global function stropnd(mcloperand a,int sizeprefix=0,debug=0)ichar=
	static [512]char str
	[128]char str2
	ichar plus,t
	int offset,tc

	str[1]:=0

	case a.mode
	when a_reg then
		return strreg(a.reg, a.size)

	when a_imm then
		strcpy(&.str,strvalue(a))

	when a_mem then
		case a.valtype
		when intimm_val then
			strcpy(&.str,strint(a.value))
		when realimm_val then
			strcpy(&.str,strreal(a.xvalue))
		when realmem_val then
			fprint @&.str,"M#",a.xvalue
		esac

		strcat(&.str,getsizeprefix(a.size,sizeprefix))
		strcat(&.str,"[")

		plus:=""
		if a.reg then
			strcat(&.str,strreg(a.reg,8))
			plus:="+"
		fi
		if a.regix then
			strcat(&.str,plus)
			strcat(&.str,strreg(a.regix,8))
			plus:="+"
			if a.scale>1 then
				strcat(&.str,"*")
				strcat(&.str,strint(a.scale))
			fi
		fi

		if a.valtype in [def_val,label_val, temp_val] then
			if plus^='+' then
				strcat(&.str,plus)
			fi
			strcat(&.str,strvalue(a))
	    elsif offset:=a.offset then
			print @&.str2,offset:"+"
			strcat(&.str,&.str2)
		fi
		strcat(&.str,"]")

	when a_xreg then
		return strxreg(a.reg,a.size)

	else
		println "BAD OPND",A.MODE
		return "<BAD OPND>"
	esac

	return &.str
end

function strreg(int reg, size=8)ichar=
	symbol d

	d:=checkregvar(reg,0)

	if size=8 and d then
		return getdispname(d)
	else
		getregname(reg,size)
	fi
end

function checkregvar(int reg, isfloat)symbol d=
	RETURN NIL
end

function strxreg(int reg, size=8)ichar=
	symbol d

	d:=checkregvar(reg,1)

	if size=8 and d then
		return getdispname(d)
	else
		return getxregname(reg,size)
	fi
end

global function strvalue(mcloperand a)ichar=
	static [512]char str
	[128]char str2
	symbol def
	int64 value,offset,length
	ichar ss

	def:=a.def
	value:=a.value

	strcpy(&.str,"")

	case a.valtype
	when def_val then
		strcat(&.str,getdispname(def))

	addoffset::
		if offset:=a.offset then
			print @&.str2,(offset>0|"+"|""),,offset
			strcat(&.str,&.str2)
		fi

	when intimm_val then
		strcat(&.str,strint(value))

	when realimm_val then
		print @&.str,a.xvalue:"20.20"

	when realmem_val then
		strcat(&.str,"M")
		strcat(&.str,strreal(a.xvalue))

	when stringimm_val then
		strcat(&.str,"""")
		strcat(&.str,a.svalue)
		strcat(&.str,"""")

	when name_val then
		strcat(&.str,a.svalue)

	when label_val then
		strcat(&.str,"L")
		strcat(&.str,strint(a.labelno))
		goto addoffset

	when temp_val then
		return gettempname(currasmproc,a.tempno)

	else
		merror("Stropnd?")
	esac

	return &.str

end

global function ismemaddr(int n)int=
	if pclstack[n].loc=memaddr_loc then return 1 fi
	return 0
end

global function isimm64(int n)int=
	if pclstack[n].loc=immd64_loc then return 1 fi
	return 0
end

global function isregvaropnd(int n)int=
	if pclstack[n].loc=regvar_loc then return 1 fi
	return 0
end

global proc copyblock(mcloperand ax,bx, int n, savedest=1)=
!ax,bx refer to memory; do ax:=bx for n bytes
!savedest=1 to ensure that the value in ax register is not modified

	mcloperand rx, rcount
	int nwords,lab,oddbytes,offset,workreg, countreg, axreg

	oddbytes:=n rem 8		!will be zero, or 1..7
	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of word64s (ie. octobytes)

	rx:=mgenreg(workreg:=getnextreg())		!work reg

	offset:=0

	if 1<=nwords<=4 then		!use unrolled code (no loop)
		ax:=changeopndsize(ax,targetsize)
		bx:=changeopndsize(bx,targetsize)

		to nwords do
			genmc(m_mov,rx,applyoffset(bx,offset))
			genmc(m_mov,applyoffset(ax,offset),rx)
			offset+:=8
		od

	elsif nwords<>0 then		!use a loop
		rcount:=mgenreg(countreg:=getnextreg())	!count
		lab:=++mlabelno
		if savedest then
			axreg:=ax.reg
			genmc(m_push, mgenreg(axreg))
		fi

		ax:=makesimpleaddr(ax)
		bx:=makesimpleaddr(bx)
		AX.SIZE:=8

		genmc(m_mov,rcount,mgenint(nwords))
		genmc(m_labelx,mgenlabel(lab))
		genmc(m_mov,rx,bx)
		genmc(m_mov,ax,rx)

		genmc(m_add,mgenreg(ax.reg),mgenint(targetsize))
		genmc(m_add,mgenreg(bx.reg),mgenint(targetsize))

		genmc(m_dec,rcount)
		genmc_cond(m_jmpcc,ne_cond,mgenlabel(lab))
		if savedest then
			genmc(m_pop, mgenreg(axreg))
		fi

		offset:=0
		freereg(countreg)
	fi

	if oddbytes then
		n:=oddbytes						!1..7

		if n>=4 then
			rx:=changeopndsize(rx,4)
			genmc(m_mov,rx,applyoffset(bx,offset,4))
			genmc(m_mov,applyoffset(ax,offset,4),rx)
			n-:=4
			offset+:=4
		fi
		if n>=2 then
			rx:=changeopndsize(rx,2)
			genmc(m_mov,rx,applyoffset(bx,offset,2))
			genmc(m_mov,applyoffset(ax,offset,2),rx)
			n-:=2
			offset+:=2
		fi
		if n=1 then
			rx:=changeopndsize(rx,1)
			genmc(m_mov,rx,applyoffset(bx,offset,1))
			genmc(m_mov,applyoffset(ax,offset,1),rx)
		fi
	fi

	freereg(workreg)
end

function makesimpleaddr(mcloperand ax)mcloperand bx=
!assume ax is an ireg, but need a simple one with areg set but not ireg
	int newreg

	if ax.reg and not ax.regix then return ax fi
	newreg:=(ax.reg | ax.reg | (ax.regix | ax.regix | getnextreg()))
	bx:=mgenireg(newreg)

	genmc(m_lea, mgenreg(newreg), ax)
	return bx
end

global proc clearblock(mcloperand ax, int n)=
!ax is the operand with the address of memory to be cleared
!generate code to clear n bytes

	mcloperand rx, rcount
	int nwords,lab,oddbytes,offset,workreg, countreg

	oddbytes:=n rem 8		!will be zero, or 1..7
	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of word64s (ie. octobytes)

	rx:=mgenreg(workreg:=getnextreg())		!work reg
	genmc(m_xorx,rx,rx)

	offset:=0

	if 1<=nwords<=8 then		!use unrolled code (no loop)
		ax:=changeopndsize(ax,targetsize)

		to nwords do
			genmc(m_mov,applyoffset(ax,offset),rx)
			offset+:=8
		od

	elsif nwords<>0 then		!use a loop

!SPLIT INTO TWO VERSIONS:
! NWORDS IS A MULTIPLE OF 4, so can write 4 words at a time, in 1/4 of iterations
! Or do one word at a time like now.
! nword is a multiple of 4 happens when N is a multiple of 32 bytes, which will
! always be the case for power-of-two sizes of 32 bytes or more. 32/64 may already
! be done without a loop. So non-part-unrolled version only really for odd array or
! struct sizes, such as [100]char.

		if nwords iand 3 then		!not 4n

			rcount:=mgenreg(countreg:=getnextreg())	!count
			lab:=++mlabelno

			ax:=makesimpleaddr(ax)

			genmc(m_mov,rcount,mgenint(nwords))
			genmc(m_labelx,mgenlabel(lab))
			genmc(m_mov,ax,rx)

			genmc(m_add,mgenreg(ax.reg),mgenint(targetsize))

			genmc(m_dec,rcount)
			genmc_cond(m_jmpcc,ne_cond,mgenlabel(lab))

			offset:=0
			freereg(countreg)
		else
			rcount:=mgenreg(countreg:=getnextreg())	!count
			lab:=++mlabelno

			ax:=makesimpleaddr(ax)
			genmc(m_mov,rcount,mgenint(nwords/4))
			genmc(m_labelx,mgenlabel(lab))

			for i to 4 do
				genmc(m_mov,applyoffset(ax,offset),rx)
				offset+:=8
			od

			genmc(m_add,mgenreg(ax.reg),mgenint(targetsize*4))

			genmc(m_dec,rcount)
			genmc_cond(m_jmpcc,ne_cond,mgenlabel(lab))

			offset:=0
			freereg(countreg)
		fi
	fi

	if oddbytes then
		n:=oddbytes						!1..7

		if n>=4 then
			rx:=changeopndsize(rx,4)
			genmc(m_mov,applyoffset(ax,offset,4),rx)
			n-:=4
			offset+:=4
		fi
		if n>=2 then
			rx:=changeopndsize(rx,2)
			genmc(m_mov,applyoffset(ax,offset,2),rx)
			n-:=2
			offset+:=2
		fi
		if n=1 then
			rx:=changeopndsize(rx,1)
			genmc(m_mov,applyoffset(ax,offset,1),rx)
		fi
	fi

	freereg(workreg)
end

global proc genfunctiontable=
	[256]char str
	ichar s,t
	pcl currpcl
	int firststringlab,nextlab,nprocs

	if lab_funcaddrtable=0 then return fi
	mgencomment("Function Table")
	nprocs:=0

	setsegment('C',16)
	genmc(m_labelx, mgenlabel(lab_funcaddrtable))
	currpcl:=pcstart
	repeat
		if currpcl.opcode in [kprocdef,kthreadedproc] then
IF FFUNTAB OR CURRPCL.DEF.ISHANDLER THEN
			genmc(m_dq,mgenmemaddr(currpcl.def))
			++nprocs
FI
		fi
		++currpcl
	until currpcl.opcode=kendprogram

	firststringlab:=0

	genmc(m_labelx, mgenlabel(lab_funcnametable))
	currpcl:=pcstart
	repeat
!		if currpcl.opcode=kprocdef then
		if currpcl.opcode in [kprocdef,kthreadedproc] then
IF FFUNTAB OR CURRPCL.DEF.ISHANDLER THEN
			if firststringlab=0 then
				firststringlab:=nextlab:=++mlabelno
			else
				nextlab:=++mlabelno
			fi

			genmc(m_dq,mgenlabel(nextlab))
fi
		fi
		++currpcl
	until currpcl.opcode=kendprogram

	nextlab:=firststringlab
	currpcl:=pcstart
	repeat
		if currpcl.opcode in [kprocdef,kthreadedproc] then
IF FFUNTAB OR CURRPCL.DEF.ISHANDLER THEN
			genmc(m_labelx,mgenlabel(nextlab))
			s:=currpcl.def.name
			t:=s

			while s^ do
				if s^='.' then
					t:=s+1
				fi
				++s
			od
			genstring(t,1)
			++nextlab
FI
		fi
		++currpcl
	until currpcl.opcode=kendprogram

	genmc(m_labelx, mgenlabel(lab_funcnprocs))
	genmc(m_dq, mgenint(nprocs))
end

global function mgenextname(ichar s)mcloperand=
	[64]char str
	symbol d

	strcpy(&.str,s)
	str[strlen(s)]:=0

	d:=pcm_allocz(strec.bytes)

	d.name:=pcm_copyheapstring(&.str)
	d.isimport:=1

	return mgenmemaddr(d)
end

global proc mgeninfo(ichar s, int value)=
	[256]char str
	fprint @&.str,"# #",s:"15jl",value
	genmc_str(m_comment,&.str)
end

global proc mgeninfos(ichar s, svalue)=
	[256]char str
	fprint @&.str,"# #",s:"15jl",svalue
	genmc_str(m_comment,&.str)
end

global proc domcl_assem(unit pcode)=
	return when not pcode or pcode.tag<>jassem

	assemused:=1

	genmc(pcode.asmopcode, genasmopnd(pcode.a),genasmopnd(pcode.b))
	mccodex.cond:=pcode.cond

	case pcode.asmopcode
	when m_pcmpistri,m_pcmpistrm then
		if pcode.c=nil or pcode.c.tag<>jconst then gerror("pcmpistr/no imm") fi
		mccodex.c:=pcode.c.value

	esac

end

function genasmopnd(unit p)mcloperand ax=
	symbol d
	int offset,labno
	unit a				!expr: nil/name/const/(add name, const)
	unit x,y
	symbol e

	if p=nil then return nil fi

	case p.tag
	when jassemreg then
		ax:=mgenreg(p.reg,p.regsize)

	when jconst then
		ax:=mgenint(p.value)

	when jassemmem then
		a:=p.a
		d:=nil
		offset:=labno:=0

		if a then
			case a.tag
			when jconst then
				offset:=a.value
			when jname then
				d:=a.def
				if d.nameid=labelid then
					labno:=fixasmlabel(d)
					d:=nil
				fi
			when jbin then
				x:=a.a
				y:=a.b
				if x.tag=jname and y.tag=jconst then
					d:=x.def
					if d.nameid=labelid then
						labno:=fixasmlabel(d)
						d:=nil
					fi
				else
					goto error
				fi
				offset:=(a.pclop in [kadd,kaddrefx]|y.value|-y.value)
			when junary then
				if a.pclop<>kneg then merror("assume/unary") fi
				unless a.a.tag=jconst then gerror("-name") end
				offset:=-a.a.value
			when jsyscall then
MERROR("ASSEM/SYSFN?")
!				labno:=getsysfnlabel(a.opcode)

			else
error::
				cpl jtagnames[a.tag]
				gerror("Can't do memexpr")
			esac
		fi
		ax:=mgenindex(areg:p.reg, ireg:p.regix, scale:p.scale, size:ttsize[p.prefixmode],
			offset:offset, labno:labno, def:d)

	when jname then
		d:=p.def
		if d.nameid=labelid then
			labno:=fixasmlabel(d)
			ax:=mgenlabel(labno)
		else
			ax:=mgenmemaddr(d)
		fi

	when jassemxreg then
		ax:=mgenxreg(p.reg)
	when jbin then				!assume add/sub
		x:=p.a
		y:=p.b
		if x.tag=jname and y.tag=jconst then
			d:=x.def
			offset:=(p.pclop in [kadd,kaddrefx]|y.value|-y.value)
			if d.nameid=labelid then
				labno:=fixasmlabel(d)
				ax:=mgenlabel(labno)
			else
				ax:=mgenmemaddr(d)
			fi
			ax.offset:=offset
		else
			gerror("ax:imm/add")
		fi
	else
		cpl jtagnames[p.tag]
		gerror("genasmopnd?")
	esac

	return ax

end

function fixasmlabel(symbol d)int=
!d.labelno contains the label number that is passed to PCL
!PCL maintains a labelmap[] array to convert such labels to renumbered labels
!Do that translation here, and return that new label
!Note: mapped label is stored as negative value to indicate it's been done
!Will return +ve mapped label

	if d.labelno=0 then
		gerror("FIXASMLABEL: zero")
	fi
	return d.labelno
end

