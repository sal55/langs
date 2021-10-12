import msys
import mlib
import clib
import oslib

import mm_decls
import mm_tables
import mm_support
import mm_lex
import mm_lib

import* mm_pcl

int currlineno
int currfileno

strbuffer sbuffer
ref strbuffer dest=&sbuffer
int destlinestart

global proc printoverloads(filehandle f)=
	ref overloadrec p

	println @f,"OVERLOADS"
	for i to overloadtable.upb do
		p:=overloadtable[i]
		if p then
!			println @f,"Overloads for",jtagnames[i],P
			while p do
				if p.bmode then
					fprint @f,"operator (#)(#,#)#",
						jtagnames[i]+2,strmode(p.amode), strmode(p.bmode), strmode(p.rmode)
				else
					fprint @f,"operator (#)(#)#",
						jtagnames[i]+2,strmode(p.amode), strmode(p.rmode)
				fi
				if p.fncode then
					print @f,"=",p.fncode,strexpr(p.fncode).strptr
				fi
				println @f
				p:=p.nextoverload
			od
			println @f
		fi
	od

end

global proc printst(filehandle f,ref strec p,int level=0)=	!PRINTST
ref strec q

if p.symbol<>namesym then
	mcerror("PRINTST not name")
fi

printstrec(f,p,level)

q:=p.deflist

while q<>nil do
	printst(f,q,level+1)
	q:=q.nextdef
od
end

proc printstrec(filehandle f,ref strec p,int level)=		!PRINTSTREC
strec dd
ref byte q
strbuffer v
ref strbuffer d:=&v
int col,offset,n
const tabstr="    "
[256]char str

gs_init(d)

offset:=0
to level do
	gs_str(d,tabstr)
	offset+:=4
od
gs_str(d,":")

gs_leftstr(d,p.name,28-offset,'-')
gs_leftstr(d,namenames[p.nameid],12,'.')

col:=gs_getcol(d)
dd:=p^


gs_str(d,"[")
if p.isimport then
	gs_str(d,"Imp ")
else
	gs_str(d,(p.isglobal|"Prog ","Exp ", "ExpQ "|"Loc "))
fi

if dd.isstatic then
	gs_str(d,"Stat")
fi
if dd.fflang then
	gs_strsp(d,fflangnames[dd.fflang])
fi

if dd.nameid=paramid and dd.parammode then
	gs_str(d,parammodenames[dd.parammode])
fi

if dd.align then
	gs_str(d,"@@")
	gs_strint(d,dd.align)
	gs_str(d," maxalign:")
	gs_strint(d,dd.maxalign)
	gs_str(d," ")
fi
if dd.optional then
	gs_str(d,"Opt ")
fi
if dd.varparams then
	gs_str(d,"Var:")
	gs_strint(d,dd.varparams)
	gs_str(d," ")
fi
if dd.moduleno then
	print @&.str,"Modno#",,dd.moduleno
	gs_str(d,&.str)
fi

if dd.used then
	gs_str(d,"U ")
fi

if dd.isthreaded then
	gs_str(d,"Threaded ")
fi


gs_str(d,"]")
gs_padto(d,col+10,'=')

if p.owner then
	fprint @&.str,"(#)",p.owner.name
	gs_leftstr(d,&.str,18,'-')
else
	gs_leftstr(d,"()",18,'-')
fi

case p.mode
when tvoid then
	gs_str(d,"Void ")
else
	GS_STRINT(D,P.MODE)
	GS_STR(D,":")

	gs_str(d,strmode(p.mode))
	gs_str(d," ")
esac

case p.nameid
when fieldid,paramid then
	gs_str(d," Offset:")
	gs_strint(d,p.offset)
	if p.mode=tbitfield then
		gs_str(d," Bitoffset:")
		gs_strint(d,p.bitoffset)
		gs_str(d,":")
		gs_strint(d,p.bitfieldwidth)
	fi

!	sprintf(&.str,"%.*s",int(p.uflags.ulength),&p.uflags.codes)
	print @&.str,p.uflags.ulength:"v",ichar(&p.uflags.codes):".*"
	gs_str(d," UFLAGS:")
	gs_str(d,&.str)
	gs_str(d,"-")
	gs_strint(d,p.uflags.ulength)

	if p.code then
		gs_str(d,"/:=")
		gs_strvar(d,strexpr(p.code))
	fi

	if p.nameid=paramid and p.variadic then
		gs_str(d,"...")
	fi
when genfieldid then
	gs_str(d,"Index:")
	gs_strint(d,p.offset)

when procid,genprocid then

	gs_str(d,"Index:")
	gs_strint(d,p.index)

	gs_str(d," Nret:")
	gs_strint(d,p.nretvalues)

when dllprocid then
	gs_str(d,"Index/PCaddr:")
	gs_strint(d,p.index)
	if p.truename then
		gs_str(d," Truename:")
		gs_str(d,p.truename)
	fi

when staticid then
	if p.code then
		gs_str(d,"=")
		gs_strvar(d,strexpr(p.code))
	fi

when frameid then
	if p.code then
		gs_str(d,":=")
		gs_strvar(d,strexpr(p.code))
	fi

when constid then
	gs_str(d,"Const:")
	gs_strvar(d,strexpr(p.code))

when typeid then
	if p.baseclass then
		gs_str(d,"Baseclass:")
		GS_STR(D,"<HAS BASECLASS>")
	fi
when enumid then
	gs_str(d,"Enum:")
	gs_strint(d,p.index)
when dllmoduleid then
	gs_str(d,"DLL#:")
	gs_strint(d,p.dllindex)
esac

if p.atfield then
	gs_str(d," @")
	gs_str(d,p.equivfield.name)
fi
if p.atvar then
	gs_strvar(d,strexpr(p.equivvar))
fi

gs_str(d," Module# ")
gs_strint(d,p.moduleno)

gs_str(d," Lineno:")
gs_strint(d,p.lineno iand 16777215)

gs_println(d,f)

case p.nameid
when constid,frameid,staticid,macroid then
	if p.code then
		printunit(p.code,dev:f)
	fi
esac
end

global proc printstflat(filehandle f)=
int i
ref strec p
println @f,"GLOBAL SYMBOL TABLE:"

for i:=0 to hashtable.upb-1 do
!cpl i
!	if hashtable[i].name and hashtable[i].symbol=namesym then
	p:=&hashtable[i]
	if p.name then
		case p.symbol
		when namesym then
			println @f,i,p,":",p.name,symbolnames[p.symbol],namenames[p.nameid]
!			if p.symbol=lexmacronamesym then
!!				lx:=p.macrotoken
!				println @f,"			",p.macrovalue
!			fi
			p:=p.nextdupl
			while p do
				println @f,"	",p,p.name,symbolnames[p.symbol],namenames[p.nameid],
					"(From",(p.owner|p.owner.name|"-"),,")"
				p:=p.nextdupl
			od
		esac
	fi
od
end

global proc printcode(filehandle f,ichar caption)=
ref strec p
ref procrec pp

pp:=proclist
while pp do
	p:=pp.def

	print @f,p.name,,"=",(p.isglobal|"Global","Export","ExportQ"|"Local")
	if p.owner.nameid=typeid then
		print @f," in record",p.owner.name
	fi
	println @f
	printunit(p.code,,"1",dev:f)
	println @f
	pp:=pp.nextproc
od
end

global proc printunit(ref unitrec p,int level=0,ichar prefix="*",filehandle dev=nil)=		!PRINTUNIT
!p is a tagrec
ref unitrec q
ref strec d
int t
ichar idname
int64 a
real32 x32
static int cmpchain=0

if p=nil then
	return
fi

if p.lineno then
	currlineno:=p.lineno
	currfileno:=p.fileno
fi

print @dev,p,":"
print @dev,getprefix(level,prefix,p)

idname:=jtagnames[p.tag]+2
print @dev,idname,,": "

case p.tag
when j_name then
	d:=p.def

	print @dev,d.name,namenames[d.nameid]

	if d.code then
		print @dev," {",,jtagnames[d.code.tag],,"}"
	fi

	print @dev," ",,getdottedname(d)!,q
	print @dev,(p.dottedname|" {Dotted}"|"")

	if p.c then
		print @dev," Lastcall:",p.c
	fi

	if p.addroffirst then
		print @dev," Addroffirst."
	fi

	print @dev," Moduleno:",p.moduleno

	if p.avcode then print @dev," AV:",char(p.avcode) fi

PRINT @DEV,=P.INDEX


when j_labeldef then
	println @dev,p.def.name

when j_const then
	t:=p.mode
	a:=p.value
	if t=trefchar then
		if p.slength>256 then
			print @dev,"""",,"(LONGSTR)",""" *",,p.slength
		elsif p.slength then
			print @dev,"""",,p.svalue,,""" *",,p.slength
		else
			print @dev,""""""
		fi

	elsecase ttbasetype[t]
	when ti64,ti32,ti16,ti8 then print @dev,int64(a)
	when tu64,tu32,tu16,tu8 then print @dev,word64(a)
	when tc64,tc8,tc16 then print @dev,chr(a)

!	when ti32,ti64,ti8,ti16 then
!		print @dev,p.value
!	when tu32,tu64,tu8,tu16 then
!		print @dev,p.uvalue
	when tr32 then
		x32:=p.xvalue
		print @dev,real64(x32)
	when tr64 then
		print @dev,p.xvalue
	when tref then
		if p.value then
			print @dev,"#",,p.value,P.SLENGTH
		else
			print @dev,"NIL"
		fi
	when ti128 then
		print @dev,p.value128
	when tu128 then
		print @dev,p.uvalue128
	else
		println =typename(t),typename(ttbasetype[t])
		PRINT @DEV,"<PRINTUNIT BAD CONST PROBABLY VOID"
	fi
	print @dev," ",,typename(t)
	if p.isastring then
		print @dev," <isstr>"
	fi

	if p.whenlabel then
		print @dev," *L",,p.whenlabel
	fi

when j_decimal then
	print @dev,p.svalue,"Len:",p.slength

when j_typeconst then
	print @dev,typename(p.mode),typename(p.value)

!when j_operator then
!	print @dev,jtagnames[p.opcode]+2

when j_bitfield then
	print @dev,bitfieldnames[p.bfcode]+3

when j_convert,j_typepun then
!	print @dev,pclnames[p.pclop]," Convmode:",strmode(p.convmode)
	print @dev," Convmode:",strmode(p.convmode)

when j_makelist,j_multexpr then
	print @dev,"Len:",p.length," Makeax:",p.makearray

when j_dot then
	print @dev,"Offset:",p.offset

when j_index, j_ptr then

when j_exit,j_redo,j_restart,j_next then
	print @dev,"#",,p.index

when j_syscall then
	print @dev,sysfnnames[p.fnindex]+6

when j_assem then
!	print @dev,mclnames[p.index]+2
!	if p.index in [m_jmpcc, m_setcc, m_cmovcc] then
!		print @dev," ",condnames[p.cond],=P.COND
!	fi

when j_assemreg then
!	print @dev,regnames[p.reg],"size:",p.regsize

when j_assemxreg then
!	print @dev,xmmregnames[p.reg]

when j_assemmem then
!	ichar plus
!	plus:=""
!	if p.prefixmode then print @dev,strmode(p.prefixmode) fi
!	print @dev,"["
!	if p.reg then 
!		print @dev,regnames[p.reg]
!		plus:="+"
!	fi
!	if p.regix then 
!		print @dev,plus,,regnames[p.regix]
!!		plus:="+"
!	fi
!	if p.scale>1 then
!		print @dev,"*",,p.scale
!	fi
!	print @dev,"]"
!
!when j_unary, j_bin, j_unaryto, j_binto, j_cmp then
!	if p.opindex then
!		print @dev,pclnames[p.opindex]
!	else
!		print @dev,pclnames[p.pclop]
!	fi

when j_makeset then
!	if p.isconst then
!		print @dev,p.range_lower,,"..",,p.range_upper
!	fi
when j_cmpchain then
	for i to p.cmpgenop.len do
		if p.cmpgenop[i]=0 then exit fi
		print @dev,pclnames[p.cmpgenop[i]],," "
	od
esac

case p.tag
when j_name, j_ptr, j_index, j_dot,j_callproc, j_callfn, j_assign then
	if p.memmode=tvoid then
!		print @dev," LVALUE"
	else
		print @dev," WIDEN FROM:",strmode(p.memmode)
	fi
esac

if p.isconst then
	print @dev," Is const"
fi

case p.tag
when j_bin, j_binto, j_unary, j_unaryto, j_cmp, j_incr, j_convert,
	j_andl, j_orl, j_notl, j_istruel then
	if p.pclop then
		fprint @dev," Pcl<#:#>",pclnames[p.pclop],pstdnames[p.pclmode]
	else
		fprint @dev," no-op"
	fi
esac


println @dev

if p.hasa then printunitlist(dev,p.a,level+1,"1") fi
if p.hasb then printunitlist(dev,p.b,level+1,"2") fi
if p.hasc then printunitlist(dev,p.c,level+1,"3") fi
end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=		!PRINTUNIT
if p=nil then return fi

while p do
	printunit(p,level,prefix,dev)
	p:=p.nextunit
od
end

function getprefix(int level,ichar prefix,ref unitrec p)ichar=		!GETPREFIX
!combine any lineno info with indent string, return string to be output at start of a line
static [1024]char str
[1024]char indentstr
[16384]char modestr
ichar isexpr

indentstr[1]:=0
if level>10 then level:=10 fi

to level do
	strcat(&.indentstr,"- ")
od

isexpr:="S"
if jisexpr[p.tag] then isexpr:="E" fi

case p.tag
when j_if, j_switch, j_case, j_select then
	if p.mode=tvoid then
		isexpr:="S"
	fi
esac

fprint @&.modestr,"# #:#",isexpr,(p.resultflag|"RES"|"---"),strmode(p.mode)
modestr[256]:=0

strcat(&.modestr,"-----------------------------")
modestr[17]:=' '
modestr[18]:=0

strcpy(&.str,getlineinfok())
strcat(&.str,&.modestr)
strcat(&.str,&.indentstr)
strcat(&.str,prefix)
if prefix^ then
	strcat(&.str," ")
fi

return &.str
end

function getlineinfok:ichar=			!GETLINEINFO
static [40]char str

fprint @&.str,"# # ",CURRFILENO:"Z2",currlineno:"z4"
return &.str
end

global proc printmodelist(filehandle f)=		!PRINTMODELIST
int mbase
static ichar tab="\t"

println @f,"MODELIST",ntypes

for m:=0 to ntypes do
!for m:=tlast to ntypes do
	println @f,m:"4",strmode(m)
	mbase:=ttbasetype[m]

	println @f,tab,"Basetype:",mbase,strmode(mbase)
	println @f,tab,"ttname:",ttname[m]
	println @f,tab,"ttnamedef:",ttnamedef[m],(ttnamedef[m]|ttnamedef[m].name|"-")
	println @f,tab,"Target:",strmode(tttarget[m])
	println @f,tab,"Code:",stdcodes[mbase]:"c"
	println @f,tab,"Size:",ttsize[m],"Sizeset",ttsizeset[m]
	fprintln @f,"# Bounds: #..#  Length:#",tab,ttlower[m],ttlower[m]+ttlength[m]-1,ttlength[m]
	if mbase=ttuple then
		print @f,tab,"Mult:"
		for i to ttlength[m] do print @f,strmode(ttmult[m,i]),," " od
		println @f
	fi
	println @f,tab,"Keytype:",strmode(ttkeytype[m])
	println @f,tab,"Isint:",ttisint[m]
	println @f,tab,"Isword:",ttisword[m]
	println @f,tab,"Isreal:",ttisreal[m]
	println @f,tab,"Isinteger:",ttisinteger[m]
	println @f,tab,"Isallnum:",ttisallnum[m]
	println @f,tab,"Ismainnum:",ttismainnum[m]
	println @f,tab,"Isshort:",ttisshort[m]
	println @f,tab,"Isref:",ttisref[m]
	println @f
od

end

