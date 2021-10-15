import msys
import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables
import cc_lex

global int autotypeno=0
int currlineno
global int nextafindex=0

strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar

const int unitheapsize=50000
ref unitrec unitheapptr=nil
int remainingunits=0

function newstrec:ref strec=
	ref strec p
	p:=pcm_alloc(strec.bytes)
	memset(p,0,strec.bytes)
!	clear p^

	p.lineno:=lx.lineno+int(lx.fileno)<<24

	p.attribs.ax_moduleno:=currmoduleno
	return p
end

global proc initcclib=

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
	attribrec attrs
	ref byte q
	strbuffer v
	ref strbuffer d:=&v
	int col,offset
	const tabstr="    "
	[256]char str
	int scope
	ref paramrec pm

	gs_init(d)

	offset:=0
	to level do
		gs_str(d,tabstr)
		offset+:=4
	od
	gs_str(d,":")

	if p.blockno then
		print @&.str,p.name,,".",,p.blockno

		gs_leftstr(d,&.str,28-offset,'-')
	else
		gs_leftstr(d,p.name,28-offset,'-')
	fi
	gs_leftstr(d,namenames[p.nameid],12,'.')
	col:=gs_getcol(d)
	attrs:=p.attribs

	gs_str(d,"[")

	gs_str(d,scopenames[p.scope])
	gs_str(d," ")

	if attrs.ax_static then
		gs_str(d,"Stat")
	fi
	if attrs.ax_align then
		gs_str(d,"@@")
		gs_strint(d,attrs.ax_align)
		gs_str(d," ")
	fi
	if attrs.ax_varparams then
		gs_str(d,"Var ")
	fi
	if attrs.ax_used then
		gs_str(d,"Used ")
	fi
	if attrs.ax_forward then
		gs_str(d,"Fwd ")
	fi
	if attrs.ax_frame then
		gs_str(d,"Frm ")
	fi
	if attrs.ax_autovar then
		gs_str(d,"AV ")
	fi
	if attrs.ax_nparams then
		fprint @&.str,"Pm:# ",attrs.ax_nparams

		gs_str(d,&.str)
	fi
	if attrs.ax_moduleno then
		fprint @&.str,"M# ",attrs.ax_moduleno
		gs_str(d,&.str)
	fi
	if attrs.ax_equals then
		gs_str(d,"= ")
	fi
	gs_str(d,"]")
	gs_padto(d,col+10,'=')

	if p.owner then
		fprint @&.str,"(#)",p.owner.name
		gs_leftstr(d,&.str,18,' ')
	else
		gs_leftstr(d,"()",18,' ')
	fi

	case p.mode
	when tvoid then
		gs_str(d,"Void ")
	else
		gs_strsp(d,Strmode(p.mode))
	esac

	case p.nameid
	when fieldid then
		gs_str(d,"Offset:")
		gs_strint(d,p.offset)

	when frameid,paramid then
		if p.code then
			gs_str(d,"=")
			gs_strvar(d,strexpr(p.code))
		fi
		gs_str(d," Offset: ")
		gs_strint(d,p.offset)

	when procid then

		gs_str(d,"Index:")
		gs_strint(d,p.index)
		gs_str(d," Address:")
		print @&.str,ref void(p.address)
		gs_str(d,&.str)
		if p.attribs.ax_callback then
			gs_str(d,"<callback fn>")
		fi

	when enumid then
		gs_str(d,"Enum:")
		gs_strint(d,p.index)

	when staticid then
		if p.code then
			gs_str(d,"=")
			gs_strvar(d,strexpr(p.code))
		fi
		gs_str(d,"STATIC********")
	esac

	gs_str(d," ")

	gs_str(d,"Lineno:")
	gs_strint(d,p.lineno iand 16777215)
	gs_str(d," ")
	gs_str(d,sourcefilenames[p.lineno>>24])

	if p.nameid=procid then
		gs_line(d)
		pm:=p.paramlist
		while pm do
			gs_str(d,"		Param: ")
			gs_leftstr(d,(pm.def|pm.def.name|"Anon"),10,'-')
			gs_str(d,pmflagnames[pm.flags])
			gs_str(d," Mode:")
			gs_str(d,strmode(pm.mode))

			gs_line(d)
			pm:=pm.nextparam
		od
	fi

	gs_str(d," MODE:")
	gs_strint(d,p.mode)

	gs_println(d,f)

	if p.code then
		case p.nameid
		when frameid,staticid then
			printunit(f,p.code,-3)
		esac
	fi

end

global proc printstflat(filehandle f)=
	int i
	ref strec p
	ref tokenrec lx
	println @f,"GLOBAL SYMBOL TABLE:"

	for i:=0 to hstsize-1 do
		p:=hashtable^[i]
		if p.name then
			case p.symbol
			when namesym then
				println @f,i,p,":",getstname(p),symbolnames[p.symbol],namenames[p.nameid]
				p:=p.nextdupl
				while p do
					print   @f,"	",p,getstname(p),symbolnames[p.symbol],namenames[p.nameid],
						p.prevdupl
					println @f,"(From",(p.owner|getstname(p.owner)|"-"),,")"
					p:=p.nextdupl
				od
			esac
		fi
	od
end

global function createname(ref strec p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()

	u.tag:=j_name
	u.def:=p
	u.simple:=1

	return u
end

global function createunit0(int tag)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=tag
	return u
end

global function createunit1(int tag, ref unitrec p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=tag
	u.a:=p
	return u
end

global function createunit2(int tag, ref unitrec p,q)ref unitrec=
	ref unitrec u

	u:=allocunitrec()

	u.tag:=tag
	u.a:=p
	u.b:=q
	return u
end

global function createunit3(int tag, ref unitrec p,q,r)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=tag
	u.a:=p
	u.b:=q
	u.c:=r
	return u
end

global function createconstunit(word64 a, int t)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=j_const
	u.value:=a
	u.mode:=t
	u.simple:=1
	return u
end

global function createstringconstunit(ichar s, int length)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=j_const
	u.svalue:=s
	u.mode:=trefchar
	if length=-1 then
		u.slength:=strlen(s)
	else
		u.slength:=length
	fi
	u.isstrconst:=1
	u.simple:=1
	return u
end

global function createwstringconstunit(ref word16 s, int length)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=j_const
	u.wsvalue:=s
	u.mode:=trefwchar
	u.wslength:=length
	u.iswstrconst:=1
	u.simple:=1
	return u
end

global function getoptocode(int opc)int=		!GETOPTOCODE
!opc is kadd etc
!return matching kaddto, etc
	static [0:jtagnames.len]int16 opctotable
	int n,opcto,i
	[20]char str

	opcto:=opctotable[opc]
	if opcto then return opcto fi				!find

!assume memoising table not filled in for this opc

	strcpy(&.str,jtagnames[opc])					!"add" etc
	strcat(&.str,"to")							!"addto" etc

	for i:=0 to jtagnames.upb do
		if eqstring(jtagnames[i],&.str) then
			opctotable[opc]:=i
			return i
		fi
	od

	cpl jtagnames[opc]
	serror("Can't find -to version")
	return 0
end

global function getconstvalue(ref unitrec p,int ID=0)int64=	!GETCONSTVALUE
!extract value from kconst
	if p and p.tag=j_const then
		return p.value
	fi
	serror("GCV Not constant")
	return 0
end

global function nextautotype:ichar=
	static [32]char str

	print @&.str,"$T",,++autotypeno
	return &.str
end

global function createconstmode(int m)int=
!create const version of mode m
	int newm
	if ttconst[m] then return m fi
	if ttconsttype[m] then return ttconsttype[m] fi
	newm:=copymode(m)
	ttconsttype[m]:=newm
	ttconst[newm]:=1

	ttconsttype[newm]:=m			!use consttype to point back as well as forwards

	return newm
end

global function createrefmode(int m)int=
!create ref version of mode m (including when m is already a ref)
	int newm

	if ttreftype[m] then

	 return ttreftype[m] fi
	newm:=createnewmode(tref)
	ttreftype[m]:=newm
	tttarget[newm]:=m
	ttisref[newm]:=1
	return newm
end

global function createprocmode(int m, ref paramrec pm)int=
!create proc mode with return type
	int newm

	newm:=createnewmode(tproc)
	ttparams[newm]:=pm
	tttarget[newm]:=m
	return newm
end

global function createarraymode(int m, length)int=
!create array of mode m (including when m is already a ref)
	int newm

	IF NTYPES>10000 THEN CPL =NTYPES FI

	for i to ntypes do
		if ttbasetype[i]=tarray and tttarget[i]=m and ttlength[i]=length then
!CPL "ALREADY EXISTS"
			return i
		fi
	od


	newm:=createnewmode(tarray)
	tttarget[newm]:=m
	ttlength[newm]:=length
	ttsize[newm]:=length*ttsize[m]

	return newm
end

global function createenummode(ref strec e)int=
	int newm
	newm:=createnewmode(tenum)
	ttnamedef[newm]:=e

	return newm
end

global function createstructmode(ref strec s,int smode)int=
	int newm
	newm:=createnewmode(smode)
	ttnamedef[newm]:=s

	return newm
end

global proc setnameptr(ref unitrec p)=		!SETNAMEPTR
!p is a just created j_...def unit which has a nameptr in the .a parameter
!set up an xref from the strec back to the -def unit
!Set up a pointer in the associated strec that points back to q

	p.def.code:=p
end

global proc printcode_all(filehandle f,ichar caption)=
	int i
	ref strec p

	for i:=1 to nmodules do
		printcode(f,caption,i)
	od
end

global proc printcode(filehandle f,ichar caption,int n)=
	int i
	ref strec p

	p:=moduletable[n].stmodule.deflist

	println @f, caption, "MODULE:",moduletable[n].name

	while p do
		case p.nameid
		when procid then
			if p.code then
				println @f,p.name,,"=",scopenames[p.scope]
				printunit(f,p.code,,"1")
				println @f
			fi
		esac
		p:=p.nextdef
	od
end

global proc printunit(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=		!PRINTUNIT
!p is a tagrec
	ref unitrec q
	ref strec d
	int t,n,lincr
	ichar idname
	ref caserec pc

	if p=nil then
		return
	fi

	if p.tag>=j_dummy then
		println "print unit: bad tag",p.tag
!	os_getch()
		stop 30
	fi

	if p.lineno then
		currlineno:=p.lineno
	fi

	lincr:=1
	if level<0 then
		lincr:=-1
		print @dev,"             "
	fi

	print @dev,getprefix(abs(level),prefix,p)
	idname:=jtagnames[p.tag]
	if idname^='j' then idname+:=2 fi

	print @dev,idname,,": "

	case p.tag
	when j_name, j_funcname then
		d:=p.def

		print @dev,d.name,namenames[d.nameid]

		if d.code then
			print @dev," {",,jtagnames[d.code.tag],,"}"
		fi

		print @dev," ",,getdottedname(d)!,q

		if p.c then
			print @dev," Lastcall:",p.c
		fi

	when j_tempdecl, j_decl, j_goto then

		d:=p.def
		print @dev,d.name,namenames[d.nameid]

		println @dev
		printunit(dev,d.code,level+lincr,"1")
		return

	when j_goto then

		d:=p.def
		print @dev,d.name,namenames[d.nameid]

	when j_labelstmt then
		print @dev,p.def.name!,"+ LABELED STATEMENT"

	when j_casestmt then
		print @dev,"Index:",p.index

	when j_const then
		t:=p.mode
		if t=trefchar then
			if not p.isstrconst then
				goto doref
			fi
	dostring::
			if p.slength>256 then
				print @dev,"""",,"(LONGSTR)",""" *",,p.slength
			else
				print @dev,"""",,p.svalue,,""" *",,p.slength
			fi
		elsif t=trefwchar then
			if not p.iswstrconst then
				goto doref
			fi
			print @dev,"""",,"(WSTRING)",""" *",,p.wslength
		elsif t>=tschar and t<=tsllong then
			print @dev,p.value
		elsif t>=tuchar and t<=tullong then
			print @dev,p.uvalue
		elsif isrealcc(t) then
			print @dev,p.xvalue
		elsif ttbasetype[t]=tref then
			if p.isstrconst then
				goto dostring
			fi
	doref::
			print @dev,ref void(p.value)
		elsif ttbasetype[t]=tarray then
			if p.isstrconst then
				goto dostring
			fi
			serror("PRINTUNIT/CONST/aRRAY")
		else
			cpl typename(t)
			serror("PRINTUNIT BAD CONST")
		fi
		print @dev," ",,Strmode(t)
		if p.isstrconst then print @dev,"<STRCONST>" fi
		if p.iswstrconst then print @dev,"<WSTRCONST>" fi

	when j_convert then
		print @dev,convnames[p.opcode]
		print @dev," "
		if p.convmem then print @dev,"Mem:" fi
		print @dev,typename(p.a.mode)
		print @dev," => "
		if p.convtomem then print @dev,"Mem:" fi
		print @dev,typename(p.mode)

	when j_scale then
		print @dev,"Scale:",p.scale

	when j_addptr,j_subptr then
		print @dev,"Ptrscale:",p.ptrscale

	when j_switch then
		pc:=p.nextcase
		n:=0
		while pc do ++n; pc:=pc.nextcase od

		print @dev,p.nextcase,n

	when j_callfn then
		print @dev," Aparams:",p.aparams

	when j_ptr then

	when j_dot then
		print @dev," Offset:",p.offset

	esac

	if p.alength then print @dev," ALENGTH=",p.alength fi

	println @dev

	printunitlist(dev,p.a,level+lincr,"1")
	printunitlist(dev,p.b,level+lincr,"2")
	if p.tag<>j_block then					!.c is used to point to last element
		printunitlist(dev,p.c,level+lincr,"3")
	fi
end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=		!PRINTUNIT
	if p=nil then return fi

	while p do
		printunit(dev,p,level,prefix)
		p:=p.nextunit
	od
end

function getprefix(int level,ichar prefix,ref unitrec p)ichar=		!GETPREFIX
!combine any lineno info with indent string, return string to be output at start of a line
	static [512]char str
	[512]char indentstr
	ichar modestr
	int length

	indentstr[1]:=0
	if level>10 then level:=10 fi

	strcpy(&.indentstr,"-----------------------")

	modestr:=Strmode(p.mode,0)
	length:=strlen(modestr)
	if length<strlen(&.indentstr) then
		memcpy(&.indentstr,modestr,length)
	else
		strcpy(&.indentstr,modestr)
	fi

	to level do
		strcat(&.indentstr,"|---")
	od

	strcpy(&.str,getlineinfok())
	strcat(&.str,&.indentstr)
	strcat(&.str,prefix)
	if prefix^ then
		strcat(&.str," ")
	fi

	return &.str
end

global function getdottedname(ref strec p)ichar=		!GETDOTTEDNAME
!build full dotted name for st item p
	static [256]char str
	[256]char str2
	ref strec owner

	strcpy(&.str,p.name)
	owner:=p.owner
	while owner and owner.nameid<>programid do
		strcpy(&.str2,&.str)
		strcpy(&.str,owner.name)
		strcat(&.str,".")
		strcat(&.str,&.str2)
		owner:=owner.owner
	od
	if p.blockno then
		print @&.str2,".",,p.blockno
		strcat(&.str,&.str2)
	fi
	return &.str
end

function getlineinfok:ichar=			!GETLINEINFO
	static [40]char str

	fprint @&.str,"# ",currlineno:"z5"
	return &.str
end

global function getautofieldname:ref strec=
!create auto-field name and return pointer to st entry
	[32]char str
	ichar name

	print @&.str,"$F",,++nextafindex

	name:=pcm_copyheapstring(&.str)
	return addnamestr(name)
end

global proc convertstring(ichar s, t,int length=-1)=		!CONVERTSTRING
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
	int c
	[20]char str
	ichar t0

	if length=-1 then
		length:=strlen(s)
	fi

	t0:=t

	to length do
		c:=s++^
		switch c
		when '"' then
			t++^:='\\'
			t++^:='"'
		when '\'' then
			t++^:='\\'
			t++^:='\''
		when 10 then
			t++^:='\\'
			t++^:='n'
		when 13 then
			t++^:='\\'
			t++^:='r'
		when 9 then
			t++^:='\\'
			t++^:='t'
		when '\\' then
			t++^:='\\'
			t++^:='\\'
		when 7 then
			t++^:='\\'
			t++^:='a'
		when 8 then
			t++^:='\\'
			t++^:='b'
		when 12 then
			t++^:='\\'
			t++^:='f'
		when 11 then
			t++^:='\\'
			t++^:='v'
		else
			if c<32 or c>=127 then
				fprint @&.str,"\\#o",c:"z3"
				t++^:=str[1]
				t++^:=str[2]
				t++^:=str[3]
				t++^:=str[4]
			else
				t++^:=c
			fi
		endswitch
	od
	t^:=0
end

global function strexpr(ref unitrec p)ref strbuffer=		!STREXPR
!vx_makestring("",exprstr)
	gs_init(exprstr)

	jeval(exprstr,p)
	return exprstr
end

proc jeval(ref strbuffer dest, ref unitrec p)=			!JEVAL
!p represents an expression. It can be a unitrec only, not a list (lists only occur inside
!kmakelist and kmakeset units, which specially dealt with here)
!dest is a destination string. Special routines such as gs_additem() are used, which take care
!of separators so that successive alphanumeric items don't touch
	ref unitrec q
	[16000]char str
	int lb,t

	case p.tag
	when j_const then
		if (t:=p.mode)=trefchar then
			if p.slength=0 then goto doref fi		!might be initialised to something else
			if not p.isstrconst then goto doref fi		!might be initialised to something else
			if p.slength>str.len/2 then
				strcpy(&.str,"LONGSTR)")
			else
				convertstring(p.svalue,&.str)
			fi
			gs_additem(dest,"""")
			gs_additem(dest,&.str)
			gs_additem(dest,"""")
			return
		elsif t>=tschar and t<=tsllong then
			getstrint(p.value, &.str)

		elsif t>=tuchar and t<=tullong then
			strcpy(&.str,strword(p.uvalue))

		elsif t=tdouble or t=tfloat then
			strcpy(&.str,strreal(p.xvalue))
		else
			case ttbasetype[p.mode]
			when tref then
	doref::
				print @&.str,ref void(p.svalue)
			when tarray then
				strcpy(&.str,"ARRAY")
			else
				CPL typename(p.mode)
				nxerror("EVAL/CONST",p)
			esac
		fi
		gs_additem(dest,&.str)

	when j_name then
		gs_additem(dest,p.def.name)

	when j_funcname then
		gs_str(dest,"&")
		gs_additem(dest,p.def.name)

	when j_andl,j_orl,j_andand,j_eq,j_ne,j_lt,j_le,j_gt,j_ge,j_add,j_sub,j_mul,j_div,
			j_rem,j_iand,j_ior,j_ixor,j_shl,j_shr,
			j_addto,j_subto,j_multo,j_divto,
			j_remto,j_iandto,j_iorto,j_ixorto,j_shlto,j_shrto 	then

		strcpy(&.str,getopcjname(p.tag))
		gs_additem(dest,"(")
		jeval(dest,p.a)
		gs_additem(dest,&.str)
		jeval(dest,p.b)
		gs_additem(dest,")")

	when j_neg,j_abs,j_inot,j_notl,j_istruel then

		strcpy(&.str,getopcjname(p.tag))
		gs_additem(dest,&.str)
		gs_additem(dest,"(")
		jeval(dest,p.a)
		gs_additem(dest,")")

	when j_callfn then
		jeval(dest,p.a)
		gs_additem(dest,"(")

		q:=p.b
		while q do
			jeval(dest,q)
			q:=q.nextunit
			if q then gs_additem(dest,",") fi
		od
		gs_additem(dest,")")

	when j_dot then
		jeval(dest,p.a)
		gs_additem(dest,".")
		GS_STR(DEST,"???")

	when j_idot then
		jeval(dest,p.a)
		gs_additem(dest,"->")
		jeval(dest,p.b)

	when j_makelist,j_exprlist then
		lb:=p.tag=j_exprlist
		gs_additem(dest,(lb|"("|"{"))

		q:=p.a
		while q do
			jeval(dest,q)
			q:=q.nextunit
			if q then gs_additem(dest,",") fi
		od
		gs_additem(dest,(lb|")"|"}"))

	when j_assign then
		jeval(dest,p.a)
		gs_additem(dest,"=")
		jeval(dest,p.b)

	when j_ifx then
		jeval(dest,p.a)
		gs_additem(dest,"?")
		jeval(dest,p.b)
		gs_additem(dest,":")
		jeval(dest,p.c)

	when j_convert then

		gs_additem(dest,Strmode(p.mode))
		gs_additem(dest,"(")
		jeval(dest,p.a)
		gs_additem(dest,")")

	when j_ptr then
		gs_additem(dest,"*(")
		jeval(dest,p.a)
		if p.b then
			gs_additem(dest,"+")
			jeval(dest,p.b)
		fi
		gs_additem(dest,")")

	when j_block then
		gs_additem(dest,"<JBLOCK>")

	when j_preincr then
		gs_additem(dest,"++")
		jeval(dest,p.a)

	when j_predecr then
		gs_additem(dest,"--")
		jeval(dest,p.a)

	when j_postincr then
		jeval(dest,p.a)
		gs_additem(dest,"++")

	when j_postdecr then
		jeval(dest,p.a)
		gs_additem(dest,"--")


	when j_null then
		gs_str(dest,"<nullunit>")

	when j_scale then
		gs_str(dest,"scale((")
		jeval(dest,p.a)
		if p.scale>0 then
			gs_str(dest,")*")
			gs_strint(dest,p.scale)
		else
			gs_str(dest,")/")
			gs_strint(dest,-p.scale)
		fi
		gs_str(dest,")")
	when j_addptr then
		gs_str(dest,"(")
		jeval(dest,p.a)
		gs_str(dest,"+")
		jeval(dest,p.b)
		gs_str(dest,")")

	when j_widenmem then
		jeval(dest,p.a)


	else
		CPL JTAGNAMES[P.TAG]
		gs_str(dest,"<CAN'T DO JEVAL>")
	end
end

global function getopcjname(int opc)ichar=		!GETOPCJNAME
!op is a kcode representing an operator
!return the name as it might appear in J code
!caller must check for differences specific to the target
	static [20]char str
	ichar name,s

	name:=jtagnames[opc]
	s:=strchr(name,' ')
	if s then
		memcpy(&.str,name,s-name)
		str[s-name+1]:=0
		return &.str
	else
		return name
	fi
end

global function Strmode(int m,expand=1)ichar=		!STRMODE
	static [16384]char str

	istrmode(m,expand,&.str)

	return &.str
end

global function Strmode2(int m,expand=1)ichar=		!STRMODE
	static [16384]char str

	istrmode(m,expand,&.str)

	return &.str
end

global proc istrmode(int m,expand=1,ichar dest)=		!ISTRMODE
	ref strec d,q
	int value,needcomma,x,i,target,t,n
	strbuffer sxx
	ref strbuffer xx:=&sxx
	ref strbuffer sdim,slength
	[100]char strdim,strlength
	ref paramrec pm

	if m<tlast then
		strcpy(dest,typename(m))
		return
	fi

	t:=ttbasetype[m]
	case t
	when tref then
		if ttconst[m] then
			strcpy(dest,"const ref ")
		else
			strcpy(dest,"ref ")
		fi
		target:=tttarget[m]
		if target>=0 and ttbasetype[tttarget[m]]=tstruct then
			strcat(dest,typename(tttarget[m]))
		else
			istrmode(tttarget[m],0,dest+strlen(dest))
		fi
	when tarray then
		if ttlength[m] then
			fprint @dest,"[#]",ttlength[m]
		else
			strcpy(dest,"[]")
		fi
		istrmode(tttarget[m],0,dest+strlen(dest))

	when tenum then
		strcpy(dest,"enum ")
		strcat(dest,typename(m))

	when tstruct,tunion then
		if not expand then
			strcpy(dest,typename(m))
			return
		fi

		strcpy(dest,typename(ttbasetype[m]))
		strcat(dest,"(")
		d:=ttnamedef[m]
		needcomma:=0

		q:=d.deflist
		while q do
			if needcomma then strcat(dest,",") fi
			needcomma:=1
			istrmode(q.mode,0,dest+strlen(dest))
			strcat(dest," ")
			strcat(dest,q.name)
			q:=q.nextdef
		od
		strcat(dest,")")

	when tvoid then			!must be a usertype that is not defined (as normal voids checked above)
		strcpy(dest,typename(m))

	when tproc then
		strcpy(dest,"proc[PM](")
		pm:=ttparams[m]
		n:=pm.nparams
		for i to n do
			istrmode(pm.mode,0,dest+strlen(dest))
			if i<>n then
				strcat(dest,",")
			fi
			pm:=pm.nextparam
		od
		strcat(dest,")")
		istrmode(tttarget[m],0,dest+strlen(dest))

	elsif t<tlast then
		strcpy(dest,typename(m))
		return
	else
		CPL typename(m)
		mcerror("NEWSTRMODE")
	esac
end

global function countunits(ref unitrec p)int=
	int n
	n:=0
	while p do
		++n
		p:=p.nextunit
	od
	return n
end

proc purgesymbol(ref strec p,prev,int del)=
!unlink and (when del=1) recover memory from st entry p
!p is removed from deflist chain of its owner, and from the dupllist chain
!prev is nil when p is the first entry in its owner's deflist, or points
!to the previous still existing entry in the chain. It is necessary so that
!the .nextdef link can be maintained, of either prev or owner
	ref strec q

	case p.nameid
	when fieldid then			!needed for genfieldtables
		return
	esac

!Unlink child symbols
	purgesymbollist(p.deflist,0,del)

!unlink from deflist and continue deflist around it
	if prev then
		prev.nextdef:=p.nextdef
	else
		p.owner.deflist:=p.nextdef
	fi

!now need to unlink from dupllist. Here, the .prevdupl field will always
!be valid, pointing to the generic entry if necessary (that can't be unlinked
!here)

	q:=p.prevdupl
	q.nextdupl:=p.nextdupl

!Now delete the entry
	if del then
		pcm_free(p,strec.bytes)
	fi
end

global proc purgesymbollist(ref strec p,int ismodule, del)=
!scan the list in p (the deflist of an owner symbol)
!and unlink a symbol when ismodule is 0 or it's not global
!when del=1, then also recover the memory used
!ismodule should be 1 for a module, then the global flag is checked

	ref strec q,prev

	serror("PURGESYMBOL")

end

global proc purgeprocs(ref strec p, int del)=
!scan procs in the list p, and remove frame vars

	while p do
		if p.nameid=procid then
			purgeproc(p,del)
		fi
		p:=p.nextdef
	od
end

global proc purgeproc(ref strec p, int del)=
!scan procs in the list p, and remove frame vars
	ref strec q,prev,r

!NOTE: THIS CAN'T BE USED AT THE MINUTE, AS THE STRECS COMPRISING THE
!FRAME VARS CONTAIN FRAME OFFSETS NEEDED BY THE CODE GENERATOR.
!POSSIBLY, ENTRIES CAN BE UNLINKED INSTEAD, BUT CAN STILL BE POINTED
!TO BY REFERENCES WITHIN THE BYTE-CODE 

	q:=p.deflist
	prev:=nil
	while q do
		r:=q.nextdef
		if q.nameid=frameid then
			purgesymbol(q,prev,del)
		else
			prev:=q
		fi
		q:=r
	od
end

global proc printmodelist(filehandle f)=		!PRINTMODELIST
	const wtypeno	= 4
	const wname		= 13
	const wbasetype	= 13
	const wbitsize	= 3
	const wtarget	= 14
	const wlength	= 4
	const wsize		= 5
	const wconst	= 3
	const wrest		= 3
	const wvolatile	= 3
	const wused		= 3
	const wconsttype= 5
	const wreftype	= 5
	const wnamedef	= 8
	const wmode		= 32
	[256]char str
	ichar mstr
	strbuffer destv
	ref strbuffer dest := &destv
	int m

	println @f,"MODELIST",ntypes

	gs_init(dest)

	gs_leftstr(dest,"#",wtypeno)
	gs_leftstr(dest,"Name",wname)
	gs_leftstr(dest,"Base",wbasetype)
	gs_leftstr(dest,"Bit",wbitsize)
	gs_leftstr(dest,"Target",wtarget)
	gs_leftstr(dest,"Len",wlength)
	gs_leftstr(dest,"Size",wsize)
	gs_leftstr(dest,"C",wconst)
	gs_leftstr(dest,"R",wrest)
	gs_leftstr(dest,"V",wvolatile)
	gs_leftstr(dest,"@Cnst",wconsttype)
	gs_leftstr(dest,"@Ref",wreftype)
	gs_leftstr(dest,"Tag",wnamedef)
	gs_leftstr(dest,"Mode",wmode)
	gs_println(dest,f)


	for m:=0 to ntypes do
		gs_init(dest)
		gs_leftint(dest,m,wtypeno)
		gs_leftstr(dest,typename(m),wname)
		gs_leftstr(dest,typename(ttbasetype[m]),wbasetype)
		gs_leftint(dest,ttbitwidth[m],wbitsize)

		if tttarget[m] then
			gs_leftint(dest,tttarget[m],3)
			gs_leftstr(dest,typename(tttarget[m]),wtarget-3)
		else
			gs_leftstr(dest,"-",wtarget)
		fi

		case ttbasetype[m]
		when tarray,tstruct,tunion then
			gs_leftint(dest,ttlength[m],wlength)
		else
			gs_leftstr(dest,"",wlength)
		esac

		gs_leftint(dest,ttsize[m],wsize)
		gs_leftint(dest,ttconst[m],wconst)
		gs_leftint(dest,ttrestrict[m],wrest)
		gs_leftint(dest,ttvolatile[m],wvolatile)

		gs_leftint(dest,ttconsttype[m],wconsttype)
		gs_leftint(dest,ttreftype[m],wreftype)

		if ttnamedef[m] then
			gs_leftstr(dest,ttnamedef[m].name,wnamedef)
		else
			gs_leftstr(dest,"-",wnamedef)
		fi

		mstr:=Strmode(m)
		if strlen(mstr)<16 then
			gs_str(dest,mstr)
		else
			gs_println(dest,f)
			gs_init(dest)
			gs_str(dest,"		")
			gs_str(dest,mstr)
		fi
		gs_println(dest,f)
	od

	println @f
end

global function typename(int m)ichar=
	int basem
	static [300]char str

	basem:=ttbasetype[m]
	case basem
	when tstruct,tunion then
		strcpy(&.str,(basem=tstruct|"struct "|"union "))
		if ttnamedef[m] then
			strcat(&.str,ttnamedef[m].name)
		fi
		return &.str
	when tarray then
		return "<array>"
	when tenum then
		if ttnamedef[m] then
			return ttnamedef[m].name
		fi
		return "<enum>"
	else
		if ttconst[m] then
			strcpy(&.str,"const ")
			strcat(&.str,stdtypenames[basem])
			return &.str
		fi
		return stdtypenames[basem]
	esac
	return ""
end

global function allocunitrec:ref unitrec=
	ref unitrec p
	ref int64 q
	int nwords

	if remainingunits-- then
		p:=unitheapptr
		++unitheapptr
		p.lineno:=lx.lineno

		if lx.fileno<=255 then
			p.fileno:=lx.fileno
		fi
		return p
	fi

!need first or new heap
	p:=unitheapptr:=pcm_alloc(unitheapsize*unitrec.bytes)

	memset(p,0,unitheapsize*unitrec.bytes)
	remainingunits:=unitheapsize-1
	++unitheapptr
	p.lineno:=lx.lineno
	if lx.fileno<=255 then
		p.fileno:=lx.fileno
	fi
	return p
end

function copymode(int m)int=
	if ntypes>=maxtype then
		serror("Too many types")
	fi
	++ntypes

!copy fields that won't already be zero
	ttnamedef[ntypes]:=ttnamedef[m]
	ttbasetype[ntypes]:=ttbasetype[m]
	ttlength[ntypes]:=ttlength[m]
	ttconst[ntypes]:=ttconst[m]
	ttrestrict[ntypes]:=ttrestrict[m]
	ttvolatile[ntypes]:=ttvolatile[m]
	ttusertype[ntypes]:=ttusertype[m]
	ttsize[ntypes]:=ttsize[m]
	ttbitwidth[ntypes]:=ttbitwidth[m]
	tttarget[ntypes]:=tttarget[m]
	ttparams[ntypes]:=ttparams[m]
	ttisref[ntypes]:=ttisref[m]

	return ntypes
end

function createnewmode(int m)int=
!create new type unitialised except for given basetype m

	if ntypes>=maxtype then
	CPL =STRMODE(M)
		serror("Too many types/cnm")
	fi
	++ntypes

!leave length, const etc all zero
!copy basic size info from basetype

	ttbasetype[ntypes]:=m
	ttsize[ntypes]:=ttsize[m]
	ttbitwidth[ntypes]:=ttbitwidth[m]

	return ntypes
end

global proc addlistunit(ref ref unitrec ulist,ulistx,ref unitrec p)=
!add strec p to end of linked list headed by ulist. ulistx^ is current end of list
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nextunit:=p
	fi
	p.nextunit:=nil

	ulistx^:=p			!update end-of-list pointer
end

global proc addlistdef(ref ref strec ulist,ulistx,ref strec p)=
!add strec p to end of linked list headed by ulist. ulistx^ is current end of list
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nextdef:=p
	fi
	p.nextdef:=nil

	ulistx^:=p			!update end-of-list pointer
end

global proc addlistparam(ref ref paramrec ulist,ulistx,ref paramrec p)=
!add paramrec p to end of linked list headed by ulist. ulistx^ is current end of list
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nextparam:=p
	fi
	p.nextparam:=nil

	ulistx^:=p			!update end-of-list pointer
end

global proc checksymbol(int symbol)=
	[256]char str

	if lx.symbol<>symbol then
		fprint @&.str,"# expected, not #",symbolnames[symbol],symbolnames[lx.symbol]

	if lx.symbol=namesym then
		strcat(&.str," \"")
		strcat(&.str,getstname(lx.symptr))
		strcat(&.str,"\"")
	fi
		serror(&.str)
	fi
end

global proc skipsymbol(int symbol)=
	if lx.symbol<>symbol then checksymbol(symbol) fi
	lex()
end

global proc inittypetables=
	int i,j,size,bitsize,s,t,u

!Initialise type tt-tables from std types first all fields initially zero

	for i:=0 to tlast-1 do
		ttbasetype[i]:=i

		bitsize:=stdtypewidths[i]
		size:=bitsize/8

		ttsize[i]:=size
		ttbitwidth[i]:=bitsize

	od
	ntypes:=tlast-1

	trefchar:=createrefmode(tschar)

	trefwchar:=createrefmode(tushort)

!do dominant table
	for i:=1 to dominantsetuptable.len do
		s:=dominantsetuptable[i,1]
		t:=dominantsetuptable[i,2]
		u:=dominantsetuptable[i,3]
		dominantmode[s,t]:=u
	od

!do conversion table
	for i:=1 to convsetuptable.len do
		s:=convsetuptable[i,1]
		t:=convsetuptable[i,2]
		u:=convsetuptable[i,3]
		conversionops[s,t]:=u
	od
	ntypesreset:=ntypes
end

global function createdupldef(ref strec owner,symptr, int id)ref strec=
!create new proc entry
!symptr is the generic st entry for proc's name
	ref strec p,q

	p:=newstrec()

	p.name:=symptr.name
	p.namelen:=symptr.namelen
	p.symbol:=namesym
	p.owner:=owner
	p.nameid:=id
	p.namespace:=namespaces[id]
	if q:=symptr.nextdupl then			!1st in dupl list
		q.prevdupl:=p
	fi
	p.nextdupl:=q
	p.prevdupl:=symptr
	symptr.nextdupl:=p

	if owner then
		if owner.deflist=nil then			!first def
			owner.deflist:=owner.deflistx:=p
		else
			owner.deflistx.nextdef:=p
			owner.deflistx:=p
		fi
	fi

	return p
end

global function createnewmoduledef(ref strec owner,symptr)ref strec=
	ref strec p,q

	p:=createdupldef(owner,symptr,moduleid)
	return p
end

global function createnewproc(ref strec owner,symptr)ref strec=
!create new proc entry
!symptr is the generic st entry for proc's name
	ref strec p,q

	p:=createdupldef(owner,symptr,procid)

	q:=p
	while q:=q.nextdupl do
		if q.owner=owner then
			cpl q.name,"in",owner.name
			serror("Dupl proc name")
		fi
!	q:=q.nextdupl
	od

	return p
end

global function resolvename(ref strec owner, symptr, int ns, blockno)ref strec=
!symptr is a generic st entry for a name
!owner is the st entry where the name has been encountered (the current
! module, or a function)
!ns is code of the namespace that is being searched
!blockno is zero, if searched at file scope, or non-zero if searching
!from inside a function. Then, it will be the current block number
!where the name has been encountered
!Search the symbol table (usually the dupl list for symptr) for
!any instance of the name which matches in owner, matches the
!namespace, and is within the blockno hierarchy
!return symptr of the st entry when resolved, or nil if not resolved
	int nsblock
	ref strec d

	if symptr.nameid>macroid then
		return symptr
	fi

	if ns=ns_labels then
		return resolvelabel(owner,symptr)
	fi

	if blockno and blockcounts[blockno]=0 then blockno:=blockowner[blockno] fi

	do							!loop for each block level
		nsblock:=ns<<16 ior blockno
		d:=symptr				!reset dupl list
		while d:=d.nextdupl do
			if owner.nameid=procid  and d.owner<>owner and d.owner.nameid=procid then
				exit
			fi
			if d.owner=owner and d.nsblock=nsblock then
				return d
			fi
		od

		if blockno=0 then
			case owner.nameid
			when procid then			!was in function, now search filescope
					!(THIS MIGHT BE NEEDED FOR PARAM-SCOPES where block number is zero)
				owner:=stmodule
				redo
			when structtagid then		!was in struct; now try owner (proc/module/other struct)
				owner:=owner.owner
				if owner=nil then		!not sure if possible, but just in case...
					return nil
				fi
			else
				return nil
			esac
		elsif (blockno:=blockowner[blockno])=0 then		!try next block level
			owner:=stmodule				!block 0 means outside outer block, so switch to module scope
		fi

	od

	return nil
end

global function resolvelabel(ref strec owner, symptr)ref strec=
	ref strec d
	d:=symptr				!reset dupl list
	while d:=d.nextdupl do
		if owner.nameid=procid  and d.owner<>owner and d.owner.nameid=procid then
			exit
		fi

		if d.owner=owner and d.namespace=ns_labels then
			return d
		fi
	od

	return nil
end

global function checkdupl(ref strec owner, symptr, int ns, blockno)ref strec=
!Same params as resolvename.
!But here, search only current scope level to see if something of the
!same name, and in the same namespace, already exists
!Returns nil, if such a name was not found, or a symptr to it
!A returned symbol might be of a different nameid, but that would
!be an error usually as you can't have two identical names in the same namespace.
!Some kinds of names can have repeated declarations in the same scope
	int nsblock
	ref strec d

	d:=symptr

	nsblock:=ns<<16 ior blockno

	while d:=d.nextdupl do
		if d.owner=owner and d.nsblock=nsblock then
			return d
		fi
	od

	return nil
end

global function checkdupl_inproc(ref strec owner, symptr, int ns, blockno)ref strec=
!special version of checkdupl
!assumes that dupl list starts at last proc

	int nsblock
	ref strec d

	d:=symptr

	nsblock:=ns<<16 ior blockno

	while (d:=d.nextdupl) and d.owner=owner do
		if d.nsblock=nsblock then
			return d
		fi
	od

	return nil
end

global function getalignment(int m)int=
!return alignment needed for type m, as 1,2,4,8
	int a

	case ttbasetype[m]
	when tarray then
		return getalignment(tttarget[m])
	when tstruct,tunion then
		a:=ttnamedef[m].attribs.ax_align
		if a=0 then
			RETURN 16
		fi
		return a
	esac

	a:=ttsize[m]
	case a
	when 1,2,4,8 then
		return a
	esac
	cpl Strmode(m),A
	serror("GETALIGN SIZE NOT 1248")

	return 0
end

global function isexported(ref strec d)int=
	if d.nameid=procid then
		if d.code and (d.scope=imported_scope or d.scope=exported_scope) then
			return 1
		fi
	else
		if d.scope=exported_scope then
			return 1
		fi
	fi
	return 0
end

global function isimported(ref strec d)int=
	if d.nameid=procid then
		if d.code=nil and (d.scope=imported_scope or d.scope=exported_scope) then
			return 1
		fi
	else
		if d.scope=imported_scope then
			return 1
		fi
	fi
	return 0
end

global function isstructunion(int m)int=
	case ttbasetype[m]
	when tstruct,tunion then
		case ttsize[m]
		when 1,2,4,8 then
		else
		 return 1
		esac
	esac
	return 0
end

global function getstname(ref strec d)ichar=
	static [256]char name
	memcpy(&.name,d.name,d.namelen)
	name[d.namelen+1]:=0
	return &.name
end

global function isrealcc(int m)int=
	m:=ttbasetype[m]
	return tfirstreal<=m<=tlastreal
end

global function isintcc(int m)int=
	m:=ttbasetype[m]
	return tfirstint<=m<=tlastint
end
