import clib
import msys
import mlib
import ax_tables
import ax_decls
import ax_lex

const ptrsize=8

fwdrec dummy1
!valuerec dummy2

global tabledata() [0:]ichar opndnames =
	(a_none=0,	$),
	(a_reg,		$),
	(a_imm,		$),
 	(a_mem,		$),		!any memory modes: [d], [R], [R*4+R2+d+imm] etc
 	(a_cond,	$),		!a condition code for jcc/setcc
	(a_xreg,	$),		!xmm register
	(a_string,	$),		!immediate string (for comments)
end

!global type opndrec = record			!24 bytes
!	ref strec labeldef	!nil, or handle of strec for label
!	union
!		int64 value		!const value/extra offset/cond code/string for comments
!		real64 xvalue	!floating point value
!		ref char svalue
!	end
!	byte mode		!a_reg etc, low level operand details
!	byte size		!byte size of operand: 1,2,4,8
!	byte reg		!0, or main register
!	byte regix		!0, or index register
!
!	byte scale		!0, or scale factor for regix
!	byte addrsize	!4 or 8 for a_mem when regs are involved
!	byte valtype	!0 (no value or int) or 'R'/'S'
!	byte spare2
!end

global record mclrec =		!64 bytes
	ref mclrec nextmcl
	ref opndrec a,b
	word16 opcode
	word16 c
	int lineno
end

!!record used for expanding buffers. Expansion is not automatic: buffercheck(n)
!!is needed at strategic points to ensure that are at least n bytes left
!global type dbuffer = record
!	ref byte pstart
!	union
!		ref byte pcurr
!		ref word16 pcurr16
!		ref word32 pcurr32
!		ref word64 pcurr64
!	end
!	ref byte pend
!	int alloc
!end

global int currsegment=0		!

global opndrec dstackopnd
global opndrec dframeopnd

global int labelno=0
global ref opndrec zero_opnd=nil

global ref mclrec mccode, mccodex

strbuffer destv
global ref strbuffer dest=&destv

global [r0..r19, 1..8]ref opndrec regtable

global proc initlib=
zero_opnd:=genint(0)

int reg,size

for reg:=r0 to r15 do
	for size:=1 to 8 do
		case size
		when 1,2,4,8 then
			regtable[reg,size]:=genreg0(reg,size)
		esac
	od
od	
for reg:=r16 to r19 do
	regtable[reg,1]:=genreg0(reg,1)
od	

ss_symboltable:=pcm_alloc(init_ss_symbols*ref void.bytes)
ss_symboltablesize:=init_ss_symbols
ss_nsymbols:=0

end

global proc genmc(int opcode,ref opndrec a=nil,b=nil)=	!GENMC
ref mclrec m
int nopnds

!	m:=pcm_allocz(mclrec.bytes)
!	m:=pcm_allocz(mclrec.bytes)
	m:=pcm_alloc(mclrec.bytes)
++NMCLASM

	m^.nextmcl:=nil

!CPL "SET MLINENO",symbolnames[lxsymbol],mclnames[opcode]
	if lxsymbol=eolsym then
		m^.lineno:=lxlineno-1
	else
		m^.lineno:=lxlineno
	fi

	m^.opcode:=opcode

	nopnds:=(a=nil|0|(b=nil|1|2))
	if nopnds=2 and opcode in [m_pcmpistri,m_pcmpistrm] then nopnds:=3 fi

	if nopnds<mclnopnds[opcode] then
		serror("Too few operands")
	elsif nopnds>mclnopnds[opcode] then
		serror("Too many operands")
	fi

	m^.a:=a
	m^.b:=b

	if mccode then
		mccodex^.nextmcl:=m
		mccodex:=m
	else
		mccode:=mccodex:=m
	fi
end

global proc genmcstr(int opcode,ichar s)=	!GENMCSTR
!as genmc but uses a single immediate string operand

genmc(opcode,genstrimm(s))
end

function newopnd(int mode)ref opndrec=
ref opndrec a

++NMCLOPNDSASM

!a:=pcm_allocz(opndrec.bytes)
a:=pcm_allocz(opndrec.bytes)
a^.mode:=mode
return a
end

global function genxreg(int xreg)ref opndrec=		!GENXREG
ref opndrec a

a:=newopnd(a_xreg)
a^.reg:=xreg
a^.size:=16
return a
end

global function genindex(int areg=0,ireg=0,scale=1,ref opndrec x=nil,int size=0,addrsize=8)ref opndrec=		!GENINDEX
!construct a mem address mode
ref opndrec a

if x then							!existing operand filled in with value
!	a:=genmem_expr(x)				!fill in label and/or offset
	a:=x
	x^.mode:=a_mem
else
	a:=newopnd(a_mem)
fi

a^.reg:=areg
a^.regix:=ireg
a^.scale:=scale
a^.size:=size
a^.addrsize:=addrsize
return a
end

global function writemclblock:ref strbuffer=		!WRITEMCLBLOCK
int i
ref mclrec m

gs_init(dest)

gs_strln(dest,"MC CODE")

m:=mccode
i:=1

while m do
	writemcl(i,m)
	m:=m^.nextmcl
	++i
od
return dest			!only used when initstr=1, otherwise caller ignores
end

global proc gencomment(ichar s=nil)=			!GENCOMMENT
if s=nil then
	genmc(m_blank)
else
	genmcstr(m_comment,s)
fi
end

global function genstrimm(ichar s)ref opndrec=			!GENSTRIMM
ref opndrec a
a:=newopnd(a_string)
a^.svalue:=s
return a
end

function getsizetag(int size)ichar=			!GETSIZETAG
case size
when 1 then return "b"
when 2 then return "h"
when 4 then return "w"
when 8 then return "d"
esac
GERROR("GETSIZETAG?")
!return tostr(size)
return nil
end

proc writemcl(int index,ref mclrec mcl)=			!WRITEMCL
[512]char mclstr
[512]char str
ichar semi

strcpy(&.mclstr,strmcl(mcl))
if mclstr[1]=0 then return fi

case mcl^.opcode
when m_comment then
	semi:=";"
else
	semi:=" "
esac

!sprintf(&.str,"%03d %04d ",semi,index, mcl^.lineno)
print @&.str,semi:"z3",index:"z4",," "!, mcl^.lineno

gs_str(dest,&.str)
gs_strln(dest,&.mclstr)
end

global function strmcl(ref mclrec mcl)ichar=			!STRMCL
static [512]char str
[128]char str2
int opcode,sizepref

opcode:=mcl^.opcode

case opcode
when m_assem then
	return mcl^.a^.svalue
when m_blank then
	return ""
when m_comment then
!	if fshowcomments then
		strcpy(&.str,";")
		strcat(&.str,mcl^.a^.svalue)
		return &.str
!	fi
!when m_bsource then
!	strcpy(&str,";")
!	strcat(&str,mcl^.a.svalue)

!when m_labelname then
!	strcpy(&str,mcl^.a.svalue)
!	strcat(&str,":")
!	return &str

when m_label then
	strcpy(&.str,mcl^.a^.labeldef^.name)
	strcat(&.str,":")
	return &.str

esac

strcpy(&.str,"		")

case opcode
when m_jmpcc then
	strcat(&.str,"j")
	strcat(&.str,condnames[mcl^.a^.value])

when m_setcc then
	strcat(&.str,"set")
	strcat(&.str,condnames[mcl^.a^.value])
when m_cmovcc then
	strcat(&.str,"cmov")
	strcat(&.str,condnames[mcl^.a^.value])
else
	strcat(&.str,mclnames[opcode]+2)
esac

ipadstr(&.str,12)

!s+:=tab+tab+leftstr(opcname,10)

if mcl^.a and mcl^.b then		!2 operands
	sizepref:=needsizeprefix(mcl^.opcode,mcl^.a,mcl^.b)

	strcat(&.str,stropnd(mcl^.a,sizepref))
	strcat(&.str,",	")
	strcat(&.str,stropnd(mcl^.b,sizepref))

elsif mcl^.a then								!1 operand
	if mcl^.opcode=m_call then
		strcat(&.str,stropnd(mcl^.a,0))
	else
		strcat(&.str,stropnd(mcl^.a,1))
	fi
!else
!	opnds:=""
fi

case opcode
when m_pcmpistri,m_pcmpistrm then
	fprint @&.str2,", #",mcl.c
	strcat(&.str,&.str2)
esac

!s+:=opnds

return &.str
end

global function stropnd(ref opndrec a,int sizeprefix=0)ichar=			!STROPND
static [256]char str
ichar plus,s
int64 value
ref strec d

case a^.mode
when a_reg then
	return getregname(a^.reg,a^.size)
when a_imm then
!	return STRVALUE(A^.LABELDEF,A^.VALUE)
	d:=a^.labeldef
	value:=a^.value
	if d then
		if d^.symbol=namedconstsym then
			return inttostr(d^.expr^.value)
		fi

!		s:=d^.name
		s:=GETFULLNAME(d)

		if value then
			if value>0 then
				strcpy(&.str,s)
				strcat(&.str,"+")
				strcat(&.str,inttostr(value))
			else
				strcpy(&.str,s)
				strcat(&.str,inttostr(value))
			fi
			return &.str
		else
			strcpy(&.str,s)
			return &.str
!			return s
		fi
	fi
	if a^.valtype=0 then
		return inttostr(value)
	else
		return realtostr(real@(value))
	fi

when a_mem then
	str[1]:=0
	strcat(&.str,getsizeprefix(a^.size,sizeprefix))
	strcat(&.str,"[")
	plus:=""

	if a^.reg then
		strcat(&.str,getregname(a^.reg,a^.addrsize))
		plus:="+"
	fi

	if a^.regix then
		strcat(&.str,plus)
		strcat(&.str,getregname(a^.regix,a^.addrsize))
		plus:="+"
		if a^.scale>1 then
			strcat(&.str,"*")
			strcat(&.str,inttostr(a^.scale))
		fi
	fi

	if a^.labeldef then
		strcat(&.str,plus)
		strcat(&.str,strdef(a^.labeldef))
		plus:="+"
	fi

	if a^.value>0 then
		strcat(&.str,plus)
		strcat(&.str,inttostr(a^.value))
	elsif a^.value<0 then
		strcat(&.str,inttostr(a^.value))
	fi

	strcat(&.str,"]")
when a_string then
	if strlen(a^.svalue)>=str.len then
!		sprintf(&.str,"\"%s\"","<Long string>")
		print @&.str,"""<Long string>"""
	else
!		sprintf(&.str,"\"%s\"",a^.svalue)
		print @&.str,"""",,a.svalue,,""""
	fi

when a_cond then
	return opndnames[a^.value]

when a_xreg then
	return xgetregname(a^.reg)

else
	return "<BAD OPND>"
esac

return &.str
end

function strdef(ref strec def)ichar=			!STRDEF
if def^.symbol=namedconstsym then
	return inttostr(def^.expr^.value)
fi
return getfullname(def)
end

global proc setsegment(int seg)=		!SETSEGMENT
!seg is 'D', 'Z', 'C', 'R' for data, zdata, code, rdata
if seg=currsegment then
	return
fi
case seg
when 'D' then genmcstr(m_segment,".data")
when 'Z' then genmcstr(m_segment,".bss")
when 'C' then genmcstr(m_segment,".text")
when 'R' then genmcstr(m_segment,".rodata")
esac
currsegment:=seg
!currzdataalign:=curridataalign:=0
end

function getsizeprefix(int size,enable=0)ichar=		!GETSIZEPREFIX
if not enable then return "" fi
case size
when 1 then return "byte "
when 2 then return "word "
when 4 then return "dword "
when 8 then return "qword "
!when 0 then return "<no size> "
when 0 then return ""
esac
return "N:"
end

function needsizeprefix(int opcode,ref opndrec a,b)int=		!NEEDSIZEPREFIX

case opcode
when m_movsx,m_movzx then
	return 1
when m_cvtsi2ss,m_cvtsi2sd then
	return 1
esac

if a^.mode=a_reg or a^.mode=a_xreg or b^.mode=a_reg or b^.mode=a_xreg then
	return 0
fi
return 1
end

global function genimm_expr(ref strec d, int64 value, int t, size=4)ref opndrec=
!generate immediate operand
!x is valuerec from an expression, or direct strec (for label) or int
ref opndrec a

a:=newopnd(a_imm)
a^.size:=size

a^.labeldef:=d
a^.value:=value
a^.valtype:=t

return a
end

global function genint(int64 x,int size=4)ref opndrec=
!generate immediate operand
!x is valuerec from an expression, or direct strec (for label) or int
ref opndrec a

!IF X=0 THEN ++NZEROS FI

a:=newopnd(a_imm)
a^.size:=size
a^.value:=x

return a
end

global function genlab(ref strec d,int size=4)ref opndrec=
!generate immediate operand
!x is valuerec from an expression, or direct strec (for label) or int
ref opndrec a

a:=newopnd(a_imm)
a^.size:=size
a^.labeldef:=d

return a
end

global function genmem(ref strec d,int size=4)ref opndrec=
!simple memory operand without registers
ref opndrec a

a:=genlab(d,size)
a^.mode:=a_mem
return a
end

global function genreg0(int reg,size=4)ref opndrec=	!GENREG

ref opndrec a
a:=newopnd(a_reg)
a^.reg:=reg
a^.size:=size
return a
end

global function getfullname(ref strec d)ichar=
static [256]char str
ichar ms

ms:=""
if d^.basedef then
	ms:=d^.basedef^.name
fi

!sprintf(&.str,"<%s : #%d &:%8p SYM:%.*s M:%s>",
!	d^.name,d^.moduleno,d,
!	strlen(symbolnames[d^.symbol])-3,symbolnames[d^.symbol],
!	ms)

fprint @&.str,"<# : ## &:# SYM:## M:#>",
	d^.name,"#",d^.moduleno,d:"8",
	strlen(symbolnames[d^.symbol])-3:"v",symbolnames[d^.symbol]:".*", ms

return &.str
return d^.name
end

global function getregname(int reg,size=4)ichar=
ichar prefix,rs
static [32]char str

case reg
when rnone then return "-"
when rframe then rs:="frame"
when rstack then rs:="stack"
!when r16..r19 then
!	rs:=(reg-r15|"0H","1H","10H","11H"|"?")

else
	rs:=inttostr(reg-r0)
esac

case size
when 1 then prefix:="B"
when 2 then prefix:="W"
when 4 then prefix:="A"
else prefix:="D"
esac

strcpy(&.str,prefix)
strcat(&.str,rs)
return &.str
end

global function xgetregname(int reg)ichar=
static [16]char str

!sprintf(&.str,"xmm%d",reg-r0)
print @&.str,"xmm",,reg-r0

return &.str
end

global proc printst(filehandle f)=
ref strec r
int count,i

r:=modulenamelist
while r do
	printstrec(f,r)
	r:=r^.nextdef
od

end

global proc printstrec(filehandle f,ref strec d)=
const w=16

case d^.symbol
when fwdlocalsym, localsym, exportedsym then
	println @f,"Label:       ",padstr(d^.name,w),(d^.scope=fwd_ref|"U"|"-"),
		symbolnames[d^.symbol],,"\T",,
	padstr((d^.segment|segmentnames[d^.segment]|"no seg"),12),
		d^.offset, d^.fwdrefs
when importedsym then
	println @f,"Label:       ",padstr(d^.name,w),"EXTERN"

when namedconstsym then
	println @f,"Named const: ",padstr(d^.name,w),"=",stropnd(d^.expr)
else
	println @f,"??"
esac
end

global proc adddef(ref strec d)=
d^.nextdef:=modulenamelist
modulenamelist:=d
end

global proc addimport(ref strec d)=
ref stlistrec p

p:=pcm_alloc(stlistrec.bytes)
p^.def:=d
p^.nextitem:=globalimportlist
globalimportlist:=p
end

global proc createlabel(ref strec symptr,int symbol)=
!symptr is a generic st entry
symptr^.symbol:=symbol
symptr^.stindex:=0
symptr^.moduleno:=currmoduleno
adddef(symptr)
end

global proc createnamedconst(ref strec symptr,ref opndrec expr)=
symptr^.symbol:=namedconstsym
symptr^.expr:=expr
!symptr^.stindex:=0
!symptr^.moduleno:=currmoduleno
adddef(symptr)
end

global proc gerror(ichar mess)=
println "SS code gen error:",mess
println "On line:", alineno
println
stop 1
end

global proc serror(ichar mess)=
println "Syntax error: '",,mess,,"' on line",lxlineno,moduletable[currmoduleno].name
stop 1
end

global proc serror_s(ichar mess, param)=
[256]char str
sprintf(&.str,mess, param)
serror(&.str)
end

function inttostr(int64 a)ichar=
static [64]char str

!sprintf(&.str,"%lld",a)
getstrint(a,&.str)
return &.str
end

function realtostr(real a)ichar=
static [64]char str
!sprintf(&.str,"%f",a)
strcpy(&.str,strreal(a))
return &.str
end

global function buffercreate(int size=1024)ref dbuffer=
ref dbuffer a

a:=pcm_alloc(dbuffer.bytes)

a^.alloc:=size
a^.pstart:=a^.pcurr:=pcm_alloc(a^.alloc)
a^.pend:=a^.pstart+a^.alloc
return a
end

proc bufferexpand(ref dbuffer a)=
int newalloc,usedbytes
ref byte p

newalloc:=a^.alloc*2
usedbytes:=a^.pcurr-a^.pstart

if usedbytes>a^.alloc then
	println "dbuffer error"
	stop
fi

p:=pcm_alloc(newalloc)
memcpy(p,a^.pstart,usedbytes)
a^.pstart:=p
a^.pcurr:=p+usedbytes
a^.alloc:=newalloc
a^.pend:=p+newalloc
end

!global proc buffercheck(ref dbuffer a,int n=1024)=
global proc buffercheck(ref dbuffer a,int n=1024)=
while a^.pend-a^.pcurr<n do
	bufferexpand(a)
od
end

global function bufferlength(ref dbuffer a)int=
return a^.pcurr-a^.pstart
end

global function bufferelemptr(ref dbuffer a, int offset)ref void=
!IF OFFSET>=BUFFERLENGTH(A) THEN
!	GERROR("BUFFERELEMPTE/OVERFLOW")
!FI

return a^.pstart+offset
end

global proc addbyte(ref dbuffer a, int x)=
a^.pcurr^:=x
++a^.pcurr
end

global proc addword(ref dbuffer a, int x)=
a^.pcurr16^:=x
++a^.pcurr16
end

global proc adddword(ref dbuffer a, int x)=
a^.pcurr32^:=x
++a^.pcurr32
end

global proc addqword(ref dbuffer a, int64 x)=
a^.pcurr64^:=x
++a^.pcurr64
end

global proc printmodulesymbols(filehandle f)=
[256]char str
ref strec d,e

	println @f,"MODULE SYMBOLS IN",moduletable[currmoduleno].name

	d:=modulenamelist

	while d do
		print @f,"   ",,padstr(d^.name,14),padstr(symbolnames[d^.symbol],12)

!		sprintf(&.str,"|| %6d %6d %8X",d^.htfirstindex,d^.htindex,d)

!		fprint @&.str,"|| # # #",d.htfirstindex:"6",d^.htindex:"6",d:"8H"
!		print @f,&.str

		fprint @f,"|| # # #",d.htfirstindex:"6",d^.htindex:"6",d:"8H"

		e:=dupltable[d^.htfirstindex]
		if e then
			print @f,"||"
			while e do
				print @f,"(",,e^.name,,")"
				e:=e^.nextdupl
			od
		fi
		println @f," BASE:",(d^.basedef|d^.basedef^.name|""),d^.basedef
		d:=d^.nextdef
	od
	println @f
end

global proc printimportsymbols(filehandle f)=
ref strec d,e
ref stlistrec p

	println @f,"GLOBAL IMPORT TABLE",globalimportlist

	p:=globalimportlist

	while p do
		d:=p^.def
		print @f,"   ",,padstr(d^.name,14),padstr(symbolnames[d^.symbol],12)
		println @f,=d^.offset,reftypenames[d^.reftype],ref void(d)
		p:=p^.nextitem
	od
	println @f
end

global proc printdupltable(filehandle f)=
[256]char str
ref strec d,e
ref stlistrec p
int i

println @f,"DUPL TABLE"

for i:=0 to dupltable.upb when dupltable[i] do
	d:=dupltable[i]

	print @f,"	",d^.htfirstindex,,":"
	while d do
!		sprintf(&.str,"(%6d %s (%s) %8X) ",d^.htindex,d^.name,
!				moduletable[d^.moduleno].name,d)
!		print @f,&.str

		fprint @&.str,"(# # (#) #) ",d.htindex:"6",d.name,
				moduletable[d.moduleno].name,d:"8H"

		d:=d^.nextdupl
	od
	println @f
od
println @f
end

