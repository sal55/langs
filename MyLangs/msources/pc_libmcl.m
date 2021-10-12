import clib
import msys
import mlib
import* pci_core
import* pci_mcl

const fasmformat=1
!const fasmformat=0

const fuseregtable=1
!const fuseregtable=0

const targetsize=8

global int fshowmsource=0

global macro isframex(d) = (d.pclop in [klocal, kparam])

[r1..rstack]mcloperand rd

global proc mclinit=
	mcloperand a
	int r,s

	for r:=r0 to r15 do
		regtable[r,1]:=mgenreg0(r,1)
		regtable[r,2]:=mgenreg0(r,2)
		regtable[r,4]:=mgenreg0(r,4)
		regtable[r,8]:=mgenreg0(r,8)
		regtable[r,16]:=mgenreg0(r,16)

		rd[r]:=regtable[r,8]
	od

	zero_opnd:=mgenint0(0)

	for i:=0 to maxsmallint do
		smallinttable[i]:=mgenint0(i)
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

	stringtable:=pcm_alloc(ref void.bytes*initstringsize)
	stringlabtable:=pcm_alloc(int32.bytes*initstringsize)
	realtable:=pcm_alloc(real.bytes*initrealsize)
	reallabtable:=pcm_alloc(int32.bytes*initrealsize)

	nstrings:=0
	nreals:=0

	stringtablesize:=initstringsize
	realtablesize:=initrealsize

	pclstack:=cast(&pclopndstack[maxoperands])

	lab_funcnametable:=0
	lab_funcaddrtable:=0

end

global proc initmcdest=
!reset mccode/mccodex
!called should have saved any values from last linked list 
	mccode:=mccodex:=nil
	clear rtsproclabels
end

global proc genmc(int opcode, mcloperand a=nil,b=nil)=
	ref mclrec m, oldm
	int labno

	m:=pcm_allocz(mclrec.bytes)
	m.opcode:=opcode

	m.a:=a
	m.b:=b

	case opcode
	when m_call then
		++inf_proccalls

	when m_lea then
		if b and b.valtype=def_val then
			b.def.addrof:=1
		fi
	when m_label then
		labno:=a.labelno
		if labno>maxlabelno then
	CPL =LABNO, MAXLABELNO
			merror("Too many labels")
		fi
		labeltable[labno]:=m

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

global function mgenxreg(int xreg,size=8)mcloperand=
	mcloperand a

!	if xreg=rnone then xreg:=++currxregno fi
	a:=newmclopnd()

	a.mode:=a_xreg
	a.reg:=xreg
	a.size:=size
	return a
end

global function mgenindex(int areg=0,ireg=0,scale=1,offset=0,size=0, labno=0, psymbol def=nil)mcloperand=
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
	psymbol d,e
	ref mclrec m
	[32]char str2,str3
	int i

	gs_init(dest)
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
	psymbol d

	opcode:=mcl.opcode

	cond:=mcl.cond
	a:=mcl.a
	b:=mcl.b
	comment:=nil

	case opcode
	when m_procstart then
!CPL "PROCSTART"
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
!ASMSTR("LABNAME/")
		case a.valtype
		when def_val then
			asmstr(getfullname(d))
		when stringimm_val then
			asmstr(a.svalue)
			return
		else
			merror("strmcl/lab")
		esac

		asmstr(":")

		if d.isexported then
			asmstr("\n")
			asmstr(getbasename(d.name))
			asmstr("::")
		fi
		return

	when m_label then
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
			asmstr(fgetregname(b.reg, b.size))
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

	elsif a and a.mode then								!1 operand
		if opcode=m_call then
			asmopnd(a,0)
		else
			asmopnd(a,1)
		fi
	fi

DOCOMMENTS::
!IF PRODMODE THEN
!	RETURN
!FI
!	asmstr(" !")
!
!	if comment then
!!		asmstr("	!")
!		asmstr(comment)
!	fi

!	IF MCL.COMMENT THEN
!		ASMSTR(" ")
!		ASMSTR(MCL.COMMENT)
!	FI
!
!	for i in mcl.regend.bounds do
!		if mcl.regend[i] then
!			asmstr(" Free:")
!			asmstr(getregname(i))
!		fi
!	od

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
	when 16 then return "word128 "
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

global function mgenint(int64 x,int size=8)mcloperand=
	if x in 0..maxsmallint and size=8 then
		return smallinttable[x]
	fi

	return mgenint0(x,size)
end

global function mgenint0(int64 x,int size=8)mcloperand a=
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
	a.value:=getrealindex(x,size)
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

global function mgenlabel(int x=0)mcloperand a=
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

global function mgenregvar(psymbol d)mcloperand a=
	a:=mgenreg(d.reg,8)
	isregvar[d.reg]:=1

	return a
end

global function mgenxregvar(psymbol d)mcloperand a=
	a:=mgenxreg(d.reg)
	isxregvar[d.reg]:=1

	return a
end

global function mgenmem(psymbol d)mcloperand a=
	pcl p:=d.pcldef
	int reg

	if d.reg then
		if pfloat[p.mode] then
			return mgenxregvar(d)
		else
			return mgenregvar(d)
		fi
	fi

	reg:=rnone
	if isframex(d) then
		if not optimflag and (int(d.offset) in -128..64) and p.size=8 then
			return frameregtable[d.offset]
		fi

		reg:=rframe
	fi

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.def:=d
	++d.nrefs
	a.valtype:=def_val

	a.size:=min(p.size,8)

	return a
end

global function mgenmemhigh(psymbol d)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_mem

	if isframex(d) then
		a.reg:=rframe
	fi
	++d.nrefs
	a.def:=d
	a.valtype:=def_val
	a.offset:=8
	a.size:=8

	return a
end

global function mgenmemaddr(psymbol d)mcloperand=
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

global function mgenreg(int reg,size=8)mcloperand=
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

global function fgetregname(int reg,size=8)ichar=
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

	if nstrings>=stringtablesize then
		extendstringtable()
	fi

	if nstrings and eqstring(stringtable[nstrings],s) then
		return stringlabtable[nstrings]
	fi

	stringtable[++nstrings]:=s
	stringlabtable[nstrings]:=++mlabelno

	return mlabelno
end

global function getrealindex(real x,int size)int=
	if nreals>=realtablesize then
		extendrealtable()
	fi

	realtable[++nreals]:=x
	++mlabelno
	reallabtable[nreals]:=(size=8|mlabelno|-mlabelno)
	return mlabelno
end

proc extendstringtable=
	ref[]ichar oldstringtable
	ref[]int32 oldstringlabtable
	int oldstringtablesize

	oldstringtablesize:=stringtablesize
	oldstringtable:=stringtable
	oldstringlabtable:=stringlabtable

	stringtablesize*:=2

	stringtable:=pcm_alloc(ichar.bytes*stringtablesize)
	stringlabtable:=pcm_alloc(int32.bytes*stringtablesize)

	for i:=1 to nstrings do
		stringtable[i]:=oldstringtable[i]
		stringlabtable[i]:=oldstringlabtable[i]
	od

	pcm_free(oldstringtable,ichar.bytes*oldstringtablesize)
	pcm_free(oldstringlabtable,int32.bytes*oldstringtablesize)
end

proc extendrealtable=
	ref[]real oldrealtable
	ref[]int32 oldreallabtable
	int oldrealtablesize

	oldrealtablesize:=realtablesize
	oldrealtable:=realtable
	oldreallabtable:=reallabtable

	realtablesize*:=2

	realtable:=pcm_alloc(real.bytes*realtablesize)
	reallabtable:=pcm_alloc(int32.bytes*realtablesize)

	for i:=1 to nreals do
		realtable[i]:=oldrealtable[i]
		reallabtable[i]:=oldreallabtable[i]
	od

	pcm_free(oldrealtable,real.bytes*oldrealtablesize)
	pcm_free(oldreallabtable,int32.bytes*oldrealtablesize)
end

proc asmstr(ichar s)=
	gs_str(dest,s)
end

proc asmchar(int c)=
	gs_char(dest,c)
end

global function getfullname(psymbol d)ichar=
	static [256]char str
	ichar name:=d.name

	if d.reg then
		fprint @str,"#.#",(d.isfloat|"X"|"R"), name
		return str
	fi

	if d.istruename and d.isimported then
		strcpy(str,"`")
		strcat(str,name)
		strcat(str,"*")
	elsif d.isimported then
		strcpy(str,name)
		strcat(str,"*")
	else		
		return name
	fi
!	esac
end 

global function getfulltempname(int tempno)ichar=
	RETURN "TEMP"
end 

global proc merror(ichar mess,ichar param="")=
!	fprintln "MCL Error: # (#) on Line: # in #",mess,param
	fprintln "MCL Error: # (#) [#]",mess,param,mseqno
!		mlineno iand 16777215, sourcefilenames[mlineno>>24]
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mlineno>>24],mlineno iand 16777215)
end

global proc merrort(ichar mess,int t)=
	fprintln "MCL Type not supported: # (#) [#]",mess,pstdnames[t], mseqno
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mlineno>>24],mlineno iand 16777215)
end

global proc merroropnd(ichar mess,int opndtype)=
	fprintln "MCL Opnd not supported: # (#) [#]",mess,opndnames[opndtype], mseqno
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mlineno>>24],mlineno iand 16777215)
end

global proc genstringtable=
	int col

	setsegment('I',8)

	if kk0used then
		genmc(m_label,mgenlabel(kk0used))
		gendb(0)
	fi
	return unless nstrings

	for i to nstrings do
		genmc(m_label,mgenlabel(stringlabtable[i]))
		genstring(stringtable[i],1)
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

proc gendqname(psymbol d)=
	genmc(m_dq,mgenmemaddr(d))
end

proc gendqlabel(int lab)=
	genmc(m_dq,mgenlabel(lab))
end

global proc genrealtable=
	real x

	return unless nreals

	mgencomment("Real Table")
	setsegment('I',8)

	for i to nreals do
		genmc(m_label,mgenlabel(abs(reallabtable[i])))
		x:=realtable[i]

		if reallabtable[i]>0 then
			genmc(m_dq, mgenrealimm(x,8))
		else
			genmc(m_dd, mgenrealimm(x,4))
		fi
	od
end

global proc genabsneg=
	setsegment('I',16)

	if lababs32 then
		mgencomment("lababs32")
		genmc(m_label,mgenlabel(lababs32))
		gendq(0x7FFF'FFFF'7FFF'FFFF)
		gendq(0x7FFF'FFFF'7FFF'FFFF)
	fi
	if lababs64 then
		mgencomment("lababs64")
		genmc(m_label,mgenlabel(lababs64))
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
	fi

	if labneg32 then
		mgencomment("labneg32")
		genmc(m_label,mgenlabel(labneg32))
		gendq(0x8000'0000'8000'0000)
		gendq(0x8000'0000'8000'0000)
	fi
	if labneg64 then
		mgencomment("labneg64")
		genmc(m_label,mgenlabel(labneg64))
		gendq(0x8000'0000'0000'0000)
		gendq(0x8000'0000'0000'0000)
	fi

	if labzero then
		mgencomment("labzero")
		genmc(m_label,mgenlabel(labzero))
		gendq(0)
	fi

	if labmask63 then
		mgencomment("mask63/offset64")
		genmc(m_label,mgenlabel(labmask63))
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		genmc(m_label,mgenlabel(laboffset64))
		gendq(0x43E0'0000'0000'0000)
	fi
end

global function mdefinelabel:int =
	genmc(m_label,mgenlabel(++mlabelno))
	return mlabelno
end

global function mcreatefwdlabel:int =
	return ++mlabelno
end

global proc mdefinefwdlabel(int lab) =
	genmc(m_label,mgenlabel(lab))
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
	psymbol d

	d:=checkregvar(reg,0)

	if size=8 and d then
		return getfullname(d)
	else
		getregname(reg,size)
	fi
end

function checkregvar(int reg, isfloat)psymbol d=
	pcl p
!CPL "CHECKREGVAR",CURRASMPROC!.NAME
	if currasmproc=nil then return nil fi
	p:=currasmproc.pcldef+1

	while p.opcode<>kendproc, ++p do
		if p.opcode in [klocal, kparam] then
			d:=p.def
			if d.reg=reg then
!CPL D.NAME,=PSTDNAMES[P.MODE],=D.REG, REG
				if isfloat and pfloat[p.mode] then return d fi
				if not isfloat and not pfloat[p.mode] then return d fi
			fi
		fi
	od
!	od
	return nil
end

function strxreg(int reg, size=8)ichar=
	psymbol d

	d:=checkregvar(reg,1)

	if size=8 and d then
		return getfullname(d)
	else
		return fgetregname(reg,size)
	fi
end

global function strvalue(mcloperand a)ichar=
	static [512]char str
	[128]char str2
	psymbol def
	int64 value,offset,length
	ichar ss

	def:=a.def
	value:=a.value

	strcpy(&.str,"")

	case a.valtype
	when def_val then
		strcat(&.str,getfullname(def))

	addoffset::
		if offset:=a.offset then
			print @&.str2,(offset>0|"+"|""),,offset
			strcat(&.str,&.str2)
		fi

	when temp_val then
		strcat(&.str,getfulltempname(a.tempno))

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

	when syscall_val then
		strcat(&.str,"XXX")

	when label_val then
		strcat(&.str,"L")
		strcat(&.str,strint(a.labelno))
		goto addoffset

!	else
	esac

	return &.str

end

global proc writegsfile(ichar filename, ref strbuffer d)=
	filehandle f

	f:=fopen(filename,"w")
	gs_println(d,f)
	fclose(f)
end

global function ismemaddr(int n)int=
	if pclstack[n].fmt=imm_memaddr then return 1 fi
	return 0
end

global function isimm64(int n)int=
	if pclstack[n].fmt=imm_d64 then return 1 fi
	return 0
end

global function isregvaropnd(int n)int=
	if pclstack[n].loc=regvar_loc then return 1 fi
	return 0
end

global proc copyblock(mcloperand ax,bx, int n)=
	mcloperand rx, rcount
	int nwords,lab,oddbytes,offset,workreg, countreg


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

		ax:=makesimpleaddr(ax)
		bx:=makesimpleaddr(bx)

		genmc(m_mov,rcount,mgenint(nwords))
		genmc(m_label,mgenlabel(lab))
		genmc(m_mov,rx,bx)
		genmc(m_mov,ax,rx)

		genmc(m_add,mgenreg(ax.reg),mgenint(targetsize))
		genmc(m_add,mgenreg(bx.reg),mgenint(targetsize))

		genmc(m_dec,rcount)
		genmc_cond(m_jmpcc,ne_cond,mgenlabel(lab))

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

global function ispoweroftwo(int64 x)int=
!when x is a power of two, and is at least 2, then return the power (ie. equiv number of shifts)
!otherwise return zero when x is negative, 0, 1, not a power of two, or more than 2**31
	int64 a
	int n

	a:=1
	n:=0
	to 60 do
		++n
		a:=a<<1
		if a=x then
			return n
		fi
	od
	return 0
end

global function getrtsproclabel(int fnindex)int=
	if rtsproclabels[fnindex]=0 then
		rtsproclabels[fnindex]:=++mlabelno
		return mlabelno
	fi
	return rtsproclabels[fnindex]
end

global proc genrtsproctable=
	[256]char str
	int proclab
	psymbol d

	setsegment('C',16)

	for i to rtsnames.len when rtsproclabels[i] do
		print @str,"Generating code for",rtsnames[i]

		mgencomment(str)
		genmc(m_label, mgenlabel(rtsproclabels[i]))

		if not rtsproctable[i] then

			case i
			when rts_mul_i128 then
				genrts_mul_i128()
			when rts_div_i128 then
				genrts_div_i128()
			else
				cpl rtsnames[i]
				merror("RTS fn not defined")
!				mgencomment("Code not ready")
			esac
			mgencomment("")
		else
			genmc(m_jmp, mgenmemaddr(rtsproctable[i]))
		fi
	od
end

proc genrts_mul_i128=
	mgencomment("<code for mul-i128>")

!a1=D10
!a2=D11 => D9
!b1=D12
!b2=D13
	genrtsentry()
	genmc(m_push, rd[r6])
	genmc(m_push, rd[r7])
	genmc(m_push, rd[r9])

	genmc(m_mov, rd[r9],rd[r11])

	genmc(m_mov, rd[r0],rd[r10])			!a1
	genmc(m_imul2, rd[r0],rd[r13])			!*b2
	genmc(m_mov, rd[r6],rd[r0])				!=>d6

	genmc(m_mov, rd[r0],rd[r9])				!a2
	genmc(m_imul2, rd[r0],rd[r12])			!*b1
	genmc(m_mov, rd[r7],rd[r0])				!=>d7

	genmc(m_mov, rd[r0],rd[r10])			!a1
	genmc(m_imul, rd[r12])					!*b1; d0=low word of a1*b1; d11=high word
	genmc(m_add, rd[r11],rd[r6])			!+ a1*b2<<64
	genmc(m_add, rd[r11],rd[r7])			!+ a2*b1<<64
	genmc(m_mov, rd[r1],rd[r11])			!

	genmc(m_pop, rd[r9])
	genmc(m_pop, rd[r7])
	genmc(m_pop, rd[r6])


	genrtsexit()
end

proc genrts_div_i128=
	mgencomment("<code for div-i128>")

!a1=D10
!a2=D11 => D3
!b1=D12
!b2=D13

	genrtsentry()
	genmc(m_push, rd[r3])

	genmc(m_mov, rd[r3],rd[r11])

	genmc(m_mov, rd[r0], rd[r3])		!a2
	genmc(m_xorx, rd[r11], rd[r11])
	genmc(m_div, rd[r12])				!a2/b1
	genmc(m_mov, rd[r1], rd[r0])		!=> c2
	genmc(m_mul, rd[r12])				!c2*b
	genmc(m_sub, rd[r3], rd[r0])		!a2-:=c2*b

	genmc(m_mov, rd[r0], rd[r10])
	genmc(m_mov, rd[r11], rd[r3])		!a2:a1
	genmc(m_div, rd[r12])				!/b1

	genmc(m_pop, rd[r3])

	genrtsexit()
end

!proc genrts_div_i128=
!	mgencomment("<code for div-i128>")
!
!!a1=D10
!!a2=D11 => D9
!!b1=D12
!!b2=D13
!
!	genrtsentry()
!	genmc(m_push, rd[r6])
!	genmc(m_push, rd[r9])
!
!	genmc(m_mov, rd[r9],rd[r11])
!
!	genmc(m_mov, rd[r0], rd[r9])		!a2
!	genmc(m_xorx, rd[r11], rd[r11])
!	genmc(m_div, rd[r12])				!a2/b1
!	genmc(m_mov, rd[r6], rd[r0])		!=> c2
!	genmc(m_mul, rd[r12])				!c2*b
!	genmc(m_sub, rd[r9], rd[r0])		!a2-:=c2*b
!
!	genmc(m_mov, rd[r0], rd[r10])
!	genmc(m_mov, rd[r11], rd[r9])		!a2:a1
!	genmc(m_div, rd[r12])				!/b1
!	genmc(m_mov, rd[r1], rd[r6])
!
!	genmc(m_pop, rd[r9])
!	genmc(m_pop, rd[r6])
!
!	genrtsexit()
!end

proc genrtsentry=
	genmc(m_push, dframeopnd)
	genmc(m_mov, dframeopnd, dstackopnd)
	genmc(m_sub, dstackopnd, mgenint(32))
end

proc genrtsexit=
	genmc(m_add, dstackopnd, mgenint(32))
	genmc(m_pop, dframeopnd)
	genmc(m_ret)
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
			genmc(m_label,mgenlabel(lab))
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
			genmc(m_label,mgenlabel(lab))

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
	genmc(m_label, mgenlabel(lab_funcaddrtable))
	currpcl:=pcstart
	repeat
!		if currpcl.opcode=kprocdef then
		if currpcl.opcode in [kprocdef,kthreadedproc] then
			genmc(m_dq,mgenmemaddr(currpcl.def))
			++nprocs
		fi
		++currpcl
	until currpcl.opcode=kendprogram

	firststringlab:=0

	genmc(m_label, mgenlabel(lab_funcnametable))
	currpcl:=pcstart
	repeat
!		if currpcl.opcode=kprocdef then
		if currpcl.opcode in [kprocdef,kthreadedproc] then
			if firststringlab=0 then
				firststringlab:=nextlab:=++mlabelno
			else
				nextlab:=++mlabelno
			fi

			genmc(m_dq,mgenlabel(nextlab))
		fi
		++currpcl
	until currpcl.opcode=kendprogram

	nextlab:=firststringlab
	currpcl:=pcstart
	repeat
!		if currpcl.opcode=kprocdef then
		if currpcl.opcode in [kprocdef,kthreadedproc] then
			genmc(m_label,mgenlabel(nextlab))
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
		fi
		++currpcl
	until currpcl.opcode=kendprogram

	genmc(m_label, mgenlabel(lab_funcnprocs))
	genmc(m_dq, mgenint(nprocs))
end

global function mgenextname(ichar s)mcloperand=
	[64]char str
	psymbol d

	strcpy(&.str,s)
	str[strlen(s)]:=0

	d:=pcm_allocz(pstrec.bytes)
!	
	d.name:=pcm_copyheapstring(&.str)
	d.isimported:=1

	return mgenmemaddr(d)
end

export proc pcl_setasmhandler(ref void fnaddr)=
	hostasmhandler:=fnaddr
end

global proc mgeninfo(ichar s, int value)=
	[256]char str
	fprint @&.str,"# #",s,value
	genmc_str(m_comment,&.str)
end

global proc mgeninfos(ichar s, svalue)=
	[256]char str
	fprint @&.str,"# #",s,svalue
	genmc_str(m_comment,&.str)
end

