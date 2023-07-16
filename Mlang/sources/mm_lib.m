int autotypeno=0
global int nextavindex=0
int nextsvindex=0

strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar

const int unitheapsize=32768
ref unitrec unitheapptr=nil
int remainingunits=0

strbuffer sbuffer
global ref strbuffer dest=&sbuffer

global ichar framevarname			!normally nil, set to frame var def to display in comment

global macro isnum(m) = (m in tfirstnum..tlastnum)
global macro isnumx(m) = (m in tfirstnum..tlastnum)
global macro isnumf(m) = (m in [tr64, tr32])
global macro isnumi(m) = (m in [ti64, tu64, tc64])

global function newstrec:symbol=
	symbol p
	p:=pcm_alloc(strec.bytes)
	clear p^

	p.pos:=lx.pos
	p.moduleno:=currmoduleno
	p.subprogno:=moduletosub[currmoduleno]
	return p
end

global function getduplnameptr(symbol owner,symptr,int id)symbol=
!create duplicate stentry
!owner is the owner ST
!symptr points to the current generic entry for the name (nameid=0)
!id is the desired nameid
!new entry is created, and added to the dupl chain for this name
!return pointer to new strec; this can be stored directly in a -def unit
!but such nameptrs are not allowed elsewhere; it must be wrapped in a knameunit
	symbol p,q

	p:=newstrec()

	p.name:=symptr.name
	p.namelen:=symptr.namelen
	p.symbol:=namesym
	p.owner:=owner
	p.nameid:=id

	p.nextdupl:=symptr.nextdupl
	p.firstdupl:=symptr
	symptr.nextdupl:=p

	return p
end

global proc adddef(symbol owner,p)=
!add new st def p, to existing deflist of owner
!assumes p already has a .owner link to owner, but is not yet part of owner's deflist
!pgeneric points to the 'generic' entry for this name in the main hash table.
!this is needed as it contains the head of the dupl list for this name (linking
!all instances of this name).
!Usually the dupl list is checked to ensure that there are no existing names
!with this same owner. (pgeneric can be nil to avoid this check.)
!ASSUMES THAT P IS LAST THING ADDED TO HEAD OF DUPLLIST (just after PGENERIC)
	symbol q

	if q:=p.nextdupl then
		if q.owner=owner then
			cpl q.name,"in",owner.name
			serror("Duplicate name")
		fi
	fi

	if owner.deflist=nil then			!first def
		owner.deflist:=p
	else
		owner.deflistx.nextdef:=p
	fi

	owner.deflistx:=p
end

global function createname(symbol p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=jname
	u.def:=p

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

global proc insertunit(unit p,int tag)=		!INSERTUNIT
!wrap extra unit around p, with given tag
!p itself is effectively changed
	unit q,nextunit
	int mode

	q:=allocunitrec()
	q^:=p^
	mode:=q.mode
	nextunit:=q.nextunit
	q.nextunit:=nil

	clear p^

	p.tag:=tag
	p.pos:=q.pos
	p.a:=q
	p.mode:=mode
	p.nextunit:=nextunit
	p.resultflag:=q.resultflag
end

global proc deleteunit(unit p,q)=
!delete p, replace by q, so that what was addressed by p now contains q
	unit r:=p.nextunit
	p^:=q^
	p.nextunit:=r
end

global function createconstunit(word64 a, int t)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=jconst
	u.value:=a
	u.mode:=t

	u.isconst:=1
	return u
end

global function createstringconstunit(ichar s, int length)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=jconst
	u.svalue:=s
	u.mode:=trefchar
	u.isastring:=1
	if length=-1 then
		u.slength:=strlen(s)
	else
		u.slength:=length
	fi
	return u
end

global function newtypename(symbol a,b)int=
	if ntypenames>=maxtypename then
		serror("Too many type names")
	fi
	++ntypenames
	typenames[ntypenames].defa:=a		!leave .owner/.pmode to be filled in
	typenames[ntypenames].defb:=b		!used type's mode is used

	typenamepos[ntypenames].pos:=lx.pos

	return -ntypenames
end

global function createusertype(symbol stname)int=
!create new, named user type
	if ntypes>=maxtype then
	cpl ntypes,stname.name
		serror("Too many types")
	fi

	++ntypes
	ttname[ntypes]:=stname.name

	ttnamedef[ntypes]:=stname
	ttbasetype[ntypes]:=tvoid
	ttlineno[ntypes]:=lx.pos

	stname.mode:=ntypes

	return ntypes
end

global function createusertypefromstr(ichar name)int=
!create new, named user type
	symbol stname

	stname:=getduplnameptr(stmodule,addnamestr(name),typeid)
	return createusertype(stname)
end

global function getrangelwbunit(ref unitrec p)ref unitrec=				!GETRANGELWB
	if p.tag=jmakerange then
		return p.a
	else
		p:=createunit1(junary,p)
		p.pclop:=klwb
		return p
	fi
end

global function getrangeupbunit(ref unitrec p)ref unitrec=				!GETRANGEUPB
	if p.tag=jmakerange then
		return p.b
	else
		p:=createunit1(junary,p)
		p.pclop:=kupb
		return p
	fi
end

global function createarraymode(symbol owner,int target,unit dimexpr, int typedefx)int=		!CREATEARRAYMODE
!lower is lower bound of array
!length is length, unless lx<>nil!
	int k,m

	if typedefx=0 then		!anon type
		for k:=tlast to ntypes do
			if ttusercat[k]=0 and ttbasetype[k]=tarray and tttarget[k]=target and
					sameunit(dimexpr, ttdimexpr[k],owner, ttowner[k]) then
				return k
			fi
		od
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=tarray
	ttlower[m]:=1
	ttdimexpr[m]:=dimexpr
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner

	ttcat[m]:=blockcat
	ttisblock[m]:=1

	return m
end

function sameunit(unit p,q, symbol powner=nil, qowner=nil)int=
!p are q are units just parses; no name resolving or type checking
!do a simple check to see if they are the same unit
	symbol d,e

	if p=q then return 1 fi
	if p=nil or q=nil then return 0 fi

	if p.tag<>q.tag then return 0 fi

	case p.tag
	when jconst then
		return p.value=q.value
	when jmakerange,jkeyvalue then
		return sameunit(p.a, q.a) and sameunit(p.b, q.b)
	when jname then
		if p.def=q.def and powner=qowner then
			return 1
		fi
	esac

	return 0

end

global function createarraymodek(symbol owner,int target,int lower,length, int typedefx)int=		!CREATEARRAYMODE
!lower is lower bound of array
	int atype,k,m

	atype:=tarray

	if typedefx=0 then		!anon type
!	for k:=tlast to ntypes do
!		if ttusercat[k]=0 and ttbasetype[k]=atype and tttarget[k]=target and \
!			ttlower[k]=lower and ttlength[k]=length then
!			return k
!		fi
!	od
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=atype
	ttlower[m]:=lower
	ttlength[m]:=length
	IF TARGET<0 THEN
		SERROR("CREATEARRAYMODEK/TARGET NOT RESOLVED")
	FI
	ttsize[m]:=length*ttsize[target]

	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner
	ttcat[m]:=blockcat
	ttisblock[m]:=1

	return m
end

global function nextautotype:ichar=
	static [32]char str

	print @&.str,"$T",,++autotypeno
	return &.str
end

global function createslicemode(symbol owner,int slicetype,target,unit dimexpr, int typedefx=0)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
	int k,m

	if typedefx=0 then		!anon type
!	for k:=tlast to ntypes do
!		if ttusercat[k]=0 and ttbasetype[k]=atype and tttarget[k]=target and \
!			ttlower[k]=lower and ttlength[k]=length then
!			return k
!		fi
!	od
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=slicetype
	if dimexpr then
		ttdimexpr[m]:=dimexpr
	else
		ttlower[m]:=1
	fi
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner
	ttcat[m]:=blockcat
	ttisblock[m]:=1

	return m
end

global function createslicemodek(symbol owner,int target,lower, int typedefx=0)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
	int k,m

	if typedefx=0 then		!anon type
!	for k:=tlast to ntypes do
!		if ttusercat[k]=0 and ttbasetype[k]=atype and tttarget[k]=target and \
!			ttlower[k]=lower and ttlength[k]=length then
!			return k
!		fi
!	od
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=tslice
	ttlower[m]:=lower
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner
!	ttcat[m]:=stdcat[slicetype]
	ttcat[m]:=blockcat
	ttisblock[m]:=1

	return m
end

global function createrefmode(symbol owner,int target,typedefx=0)int=		!CREATEREFPACKMODE
	int k,m
!	int a,b

	if typedefx=0 then		!anon type
		for k:=tlast to ntypes when ttisref[k] do
			if tttarget[k]=target then
				return k
			fi
		od
!		FI
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	storemode(owner,target,tttarget[m])
	ttbasetype[m]:=tref
	ttsize[m]:=ttsize[tref]
	ttisref[m]:=1
	ttcat[m]:=d64cat

	return m
end

global function createrefprocmode(symbol owner,stproc, paramlist,int kwd, prettype,typedefx)int=		!CREATEREFPROCMODE
!create a ref proc mode; (can't create a proc mode by itself, as it's meaningless)
	int m, mproc

	mproc:=createusertype(stproc)
	stproc.paramlist:=paramlist

	stproc.mode:=prettype
	ttbasetype[mproc]:=tproc

!don't bother looking for similar proc sig; each one is unique
	if typedefx=0 then		!anon type
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	tttarget[m]:=mproc
	ttbasetype[m]:=tref

	ttsize[m]:=ttsize[tref]
	ttisref[m]:=1
	ttcat[m]:=d64cat

	return m
end

global proc copyttvalues(int dest, source)=
	ttsigned[dest]		:= ttsigned[source]
	ttisreal[dest]		:= ttisreal[source]
	ttisinteger[dest]	:= ttisinteger[source]
	ttisshort[dest]		:= ttisshort[source]
	ttisref[dest]		:= ttisref[source]
	ttcat[dest]			:= ttcat[source]
	ttisblock[dest]		:= ttisblock[source]
end

global function getdottedname(symbol p)ichar=		!GETDOTTEDNAME
!build full dotted name for st item p
	static [256]char str
	[256]char str2
	symbol owner

	strcpy(&.str,p.name)
	owner:=p.owner
	while owner and owner.nameid<>programid do
		strcpy(&.str2,&.str)
		strcpy(&.str,owner.name)
		strcat(&.str,".")
		strcat(&.str,&.str2)
		owner:=owner.owner
	od
	return &.str
end

global function getavname(symbol owner,int id=frameid)symbol=
!create auto-var name and return pointer to st entry
	symbol p
	[32]char str
	ichar name

	if id=frameid and owner.nameid<>procid then
		serror("Auto frame not in proc")
	fi

	if id=frameid then
		print @&.str,"av_",,++nextavindex
	else
		print @&.str,"sv_",,++nextsvindex
	fi

	name:=pcm_copyheapstring(&.str)
	addnamestr(name)

	p:=getduplnameptr(owner,addnamestr(name),id)
	p.used:=1

	p.mode:=tint

	adddef(owner,p)
	return p
end

global proc unionstr_clear(ref uflagsrec u)=
	((ref word64(u))^:=0)		!clear flags and length togetjer
end

global proc unionstr_append(ref uflagsrec u, int c)=
	if u.ulength=(u.codes.len-1) then
		serror("Uflags overflow/a")
	fi
	++u.ulength
	u.codes[u.ulength]:=c
end

global proc unionstr_concat(ref uflagsrec u, v)=
	int ulen,vlen,i

	ulen:=u.ulength
	vlen:=v.ulength
	if ulen+vlen>u.codes.len then
		serror("Uflags overflow/c")
	fi
	for i:=1 to vlen do
		u.codes[i+ulen]:=v.codes[i]
	od
	u.ulength:=ulen+vlen
end

global function unionstr_last(ref uflagsrec u)int=
	if u.ulength then
		return u.codes[u.ulength]
	fi
	return 0 
end

global proc unionstr_copy(ref uflagsrec u,v)=
	memcpy(u,v,uflagsrec.bytes)
end

global function createrecordmode(symbol owner,int typedefx)int=	!CREATERECORDMODE
!typedef is nil, or an empty moderec belonging to a user type
!owner is an strec for the name def::
! * user-supplied name belonging to the typedef (same as typedef.namedef)
! * user-supplied optional name from a stand-alone enum typespec
! * auto-generated name
	int m

	if typedefx=0 then
		m:=createusertype(owner)
	else
		m:=typedefx
	fi
	ttbasetype[m]:=trecord
	ttusercat[m]:=1
	ttcat[m]:=blockcat
	ttisblock[m]:=1

	return m
end

global function createtuplemode(symbol owner,[]int &elements,int elementslen, typedefx)int=
	int m

	if typedefx=0 then
		m:=createusertype(owner)
	else
		m:=typedefx
	fi
	ttbasetype[m]:=ttuple
	ttusercat[m]:=1
	ttlength[m]:=elementslen
	ttmult[m]:=pcm_alloc(elementslen*int32.bytes)
	for i to elementslen do
		storemode(owner,elements[i],ttmult[m,i])
	od

	return m
end

global function convertstring(ichar s, t)int=
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
!returns length of t
	int c
	ichar t0:=t

	while c:=s++^ do
		switch c
		when '"' then
			t++^:='\\'
			t++^:='"'
		when 10 then
			t++^:='\\'
			t++^:='n'
		when 13 then
			t++^:='\\'
			t++^:='c'
		when 9 then
			t++^:='\\'
			t++^:='t'
		when '\\' then
			t++^:='\\'
			t++^:='\\'
		when 7,8,26,27 then
			t++^:='<'
			t++^:=c/10+'0'
			t++^:=(c rem 10)+'0'
			t++^:='>'
		else
			t++^:=c
		end switch
	od
	t^:=0

	return t-t0
end

global function strexpr(ref unitrec p)ref strbuffer=		!STREXPR
!vx_makestring("",exprstr)
	gs_init(exprstr)

	jevalx(exprstr,p)
	return exprstr
end

global proc jevalx(ref strbuffer dest, ref unitrec p)=			!JEVAL
!p represents an expression. It can be a unitrec only, not a list (lists only occur inside
!kmakelist and kmakeset units, which specially dealt with here)
!dest is a destination string. Special routines such as gs_additem() are used, which take care
!of separators so that successive alphanumeric items don't touch
	unit q,a,b
	[500]char str

	if p=nil then
		return
	fi

	a:=p.a
	b:=p.b

	switch p.tag
	when jconst then

		case ttbasetype[p.mode]
		when ti32,ti64,ti8,ti16 then
			getstrint(p.value,&.str)
		when tu32,tu64,tu8,tu16 then
			strcpy(&.str,strword(p.uvalue))
		when tc8,tc64 then
			str[1]:=p.uvalue
			str[0]:=0

		when treal,tr32 then
			print @&.str,p.xvalue
		when tref then
			if p.mode=trefchar and p.isastring then
				if p.slength>str.len/2 then
					strcpy(&.str,"LONGSTR)")
				else
					convertstring(p.svalue,&.str)
				fi
				gs_additem(dest,"""")
				gs_additem(dest,&.str)
				gs_additem(dest,"""")
				return
			else
				print @&.str,ref void(p.value)
			fi
		else
			strcpy(&.STR,"<EVAL/CONST PROBABLY VOID>")
		esac
		gs_additem(dest,&.str)

	when jname then
		gs_additem(dest,p.def.name)

	when jbin,jcmp then

		strcpy(&.str,pclnames[p.pclop])
		gs_additem(dest,"(")
		jevalx(dest,a)
		gs_additem(dest,&.str)
		jevalx(dest,b)
		gs_additem(dest,")")

	when junary, jistruel, jnotl then

		strcpy(&.str,pclnames[p.pclop])
		gs_additem(dest,&.str)
		gs_additem(dest,"(")

		if a.tag=jtypeconst then
			gs_additem(dest,STRMODE(a.value))
		else
			jevalx(dest,a)
		fi
		gs_additem(dest,")")

	when jcallfn,jcallproc then
		jevalx(dest,a)
		gs_additem(dest,"(")

		q:=b
		while q do
			jevalx(dest,q)
			q:=q.nextunit
			if q then gs_additem(dest,",") fi
		od
		gs_additem(dest,")")

	when jindex,jdotindex,jslice,jdotslice then
		jevalx(dest,a)
		if p.tag=jdotindex or p.tag=jdotslice then
			gs_additem(dest,".")
		fi
		gs_additem(dest,"[")
		jevalx(dest,b)
		gs_additem(dest,"]")

	when jdot then
		jevalx(dest,a)
		gs_additem(dest,".")
		jevalx(dest,b)

	when jmakelist then
		gs_additem(dest,"(")

		q:=a
		while q do
			jevalx(dest,q)
			q:=q.nextunit
			if q then gs_additem(dest,",") fi
		od
		gs_additem(dest,")")

	when jmakerange then
		gs_additem(dest,"(")
		jevalx(dest,a)
		gs_additem(dest,"..")
		jevalx(dest,b)
		gs_additem(dest,")")

	when jassign then
		jevalx(dest,a)
		gs_additem(dest,":=")
		jevalx(dest,b)

	when jif then
		gs_additem(dest,"(")
		jevalx(dest,a)
		gs_additem(dest,"|")
		jevalx(dest,b)
		gs_additem(dest,"|")
		jevalx(dest,p.c)
		gs_additem(dest,")")

	when jtypeconst then
		gs_additem(dest,strmode(p.mode))

	when jconvert,jtypepun then

		gs_additem(dest,strmode(p.convmode))
		if p.tag=jtypepun then
			gs_additem(dest,"@")
		fi
		gs_additem(dest,"(")
		jevalx(dest,a)
		gs_additem(dest,")")

	when jshorten then

		gs_additem(dest,"shorten(")
		jevalx(dest,a)
		gs_additem(dest,")")
	when jautocast then

		gs_additem(dest,"cast(")
		jevalx(dest,a)
		gs_additem(dest,")")
	when jkeyvalue then
		jevalx(dest,a)
		gs_additem(dest,":")
		if b then
			jevalx(dest,p.b)
		else
			gs_str(dest,"-")
		fi

	when jptr then
		jevalx(dest,a)
		gs_additem(dest,"^")

	when jclamp then
		gs_additem(dest,"(")
		jevalx(dest,a)
		gs_additem(dest,",")
		jevalx(dest,b)
		gs_additem(dest,",")
		jevalx(dest,p.c)
		gs_additem(dest,")")

	when jblock then
		gs_additem(dest,"<JBLOCK>")

	when jnull then
		gs_str(dest,"<nullunit>")

	when jaddrof then
		gs_additem(dest,"&")
		jevalx(dest,a)
		if b then
			gs_str(dest,"+")
			gs_strint(dest,b.value)
		fi

	when jaddroffirst then
		gs_additem(dest,"&.")
		jevalx(dest,a)

	when jtypestr then
		gs_additem(dest,"TYPESTR(")
		jevalx(dest,a)
		gs_additem(dest,")")

	when jcvlineno, jcvfilename, jcvmodulename then
		gs_str(dest,"$")
		gs_str(dest,jtagnames[p.tag]+1)

	when jbitfield then
		jevalx(dest,a)
		gs_str(dest,".")
		gs_str(dest,bitfieldnames[p.bitopindex])

	when jfmtitem then
		jevalx(dest,a)
		gs_str(dest,":")
		jevalx(dest,b)

	when jtypeof then
		gs_str(dest,"typeof(")
		jevalx(dest,a)
		gs_str(dest,")")

	when jsyscall then
		gs_str(dest,sysfnnames[p.fnindex]+3)
		gs_str(dest,"(")
		if a then jevalx(dest,a) fi
		gs_str(dest,")")
	when jincr then
		gs_str(dest,"incr ")
		jevalx(dest,a)
	when jstrinclude then
		gs_str(dest,"newstrinclude ")
		jevalx(dest,a)

	else
		CPL jtagnames[p.tag]
		gerror("CAN'T DO JEVAL",p)
	end
end

global function strmode(int m,expand=1)ichar=		!STRMODE
	static [4096]char str
	istrmode(m,expand,&.str)
	return &.str
end

global function strmode2(int m,expand=1)ichar=		!STRMODE
	static [4096]char str
	istrmode(m,expand,&.str)
	return &.str
end

global proc istrmode(int m,expand=1,ichar dest)=		!ISTRMODE
	symbol d,q,e
	int value,needcomma,x,i,target,mbase,n
	strbuffer sxx
	ref strbuffer xx:=&sxx
	ref strbuffer sdim,slength
	[100]char strdim
	ichar prefix
	typenamerec tn

	if m<0 then
		strcpy(dest,"*")
		tn:=typenames[-m]

		if tn.defb=nil then			!assume typeof
			strcat(dest,"typeof(")
			strcat(dest,tn.defa.name)
			strcat(dest,")")
	    else
			if tn.defa then
				strcat(dest,tn.defa.name)
				strcat(dest,".")
			fi
			strcat(dest,tn.def.name)
		fi
		return
	fi

	if m<tlast and m<>tref then
		strcpy(dest,typename(m))
		return
	fi

	case mbase:=ttbasetype[m]
	when tref then
		strcpy(dest,"ref ")
		target:=tttarget[m]
		if target>=0 and ttbasetype[target]=trecord then
			strcat(dest,typename(target))
		else
			istrmode(tttarget[m],0,dest+strlen(dest))
		fi

	when tarray then
		if ttdimexpr[m] then
			gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
			fprint @dest,"@[#]",&.strdim
		else
			if ttlength[m] then
				if ttlower[m]=1 then
					fprint @dest,"[#]",ttlength[m]+ttlower[m]-1
				else
					fprint @dest,"[#..#]",ttlower[m],ttlength[m]+ttlower[m]-1
				fi
			else
				if ttlower[m]=1 then
					fprint @dest,"[]"
				else
					fprint @dest,"[#:]",ttlower[m]
				fi
			fi
		fi
		istrmode(tttarget[m],0,dest+strlen(dest))

	when tslice then
		prefix:=stdnames[mbase]

		if ttdimexpr[m] then
			gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
			fprint @dest,"@#[#:]",prefix,&.strdim
		else
			if ttlower[m]=1 then
				strcpy(dest,prefix)
				strcat(dest,"[]")
			else
				fprint @dest,"#[#:]",prefix,ttlower[m]
			fi
		fi
		istrmode(tttarget[m],0,dest+strlen(dest))

	when trecord then
		if not expand then
			strcpy(dest,typename(m))
			return
		fi
		strcpy(dest,"")
		if expand<>2 then
			strcat(dest,typename(ttbasetype[m]))
		fi
		strcat(dest,"(")
		d:=ttnamedef[m]
		needcomma:=0

		q:=d.deflist

		while q, q:=q.nextdef do
			if needcomma then strcat(dest,",") fi
			needcomma:=1
			istrmode(q.mode,0,dest+strlen(dest))
			strcat(dest," ")
			strcat(dest,q.name)
		od
		strcat(dest,")")

	when tvoid then			!must be a usertype that is not defined (as normal voids checked above)
		strcpy(dest,"void")

	when tuser then
		strcpy(dest,typename(m))
	when tproc then

		d:=ttnamedef[m]

		strcpy(dest,"proc(")
		q:=d.paramlist
		needcomma:=0
		while q<>nil do
			if needcomma then strcat(dest,",") fi
			needcomma:=1
			istrmode(q.mode,0,dest+strlen(dest))
			strcat(dest," ")
			strcat(dest,q.name)
			q:=q.nextdef
		od
		strcat(dest,")")
		if d.mode<>tvoid then
			istrmode(d.mode,0,dest+strlen(dest))
		fi

	when ttuple then
		strcpy(dest,"Tuple(")
		n:=ttlength[m]
		for i to n do
			istrmode(ttmult[m,i],0,dest+strlen(dest))
			if i<n then strcat(dest,",") fi
		od

		strcat(dest,")")

	when tbitfield then
		strcpy(dest,"bitfield")

	elsif ttbasetype[m]<tlast then
		strcpy(dest,"Alias for:")
		istrmode(tttarget[m],0,dest+strlen(dest))

	else
		println typename(m),STRMODE(TTBASETYPE[M])
		mcerror("NEWSTRMODE")
	esac
end

global proc addtoproclist(symbol d)=
	ref procrec pp


	pp:=pcm_allocz(procrec.bytes)

	if proclist=nil then
		proclist:=proclistx:=pp
	else
		proclistx.nextproc:=pp
		proclistx:=pp
	fi
!
	pp.def:=d
end

global proc addstatic(symbol d)=
	ref procrec pp
	pp:=pcm_alloc(procrec.bytes)

	if staticlist=nil then
		staticlist:=staticlistx:=pp
	else
		staticlistx.nextproc:=pp
		staticlistx:=pp
	fi

	pp.def:=d
end

global proc addexpconst(symbol d)=
	ref procrec pp
	pp:=pcm_alloc(procrec.bytes)

	if constlist=nil then
		constlist:=constlistx:=pp
	else
		constlistx.nextproc:=pp
		constlistx:=pp
	fi
	pp.def:=d
end

global function typename(int m)ichar=
	if m>=0 then
		return ttname[m]
	fi
	return typenames[-m].def.name

end

global function allocunitrec:ref unitrec=
	ref unitrec p
	ref int64 q
	int nwords

	++nunits

	if remainingunits-- then
		p:=unitheapptr
		++unitheapptr
		p.pos:=lx.pos
		p.moduleno:=currmoduleno
		p.subprogno:=moduletosub[currmoduleno]
		return p
	fi

!need first or new heap
	p:=unitheapptr:=pcm_alloc(unitheapsize*unitrec.bytes)

	memset(p,0,unitheapsize*unitrec.bytes)
	remainingunits:=unitheapsize-1
	++unitheapptr
	p.pos:=lx.pos

	p.moduleno:=currmoduleno
	p.subprogno:=moduletosub[currmoduleno]
	return p
end

global function createdupldef(symbol owner,symptr, int id)symbol=
!create new proc entry
!symptr is the generic st entry for proc's name
	symbol p,q

	p:=newstrec()

	p.name:=symptr.name
	p.namelen:=symptr.namelen
	p.symbol:=namesym
	p.owner:=owner
	p.nameid:=id

	p.nextdupl:=symptr.nextdupl
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

global function createnewmoduledef(symbol owner,symptr, int id=moduleid)symbol=
	return createdupldef(owner,symptr,id)
end

global function duplunit(unit p,int lineno=0)unit=
	unit q
	if p=nil then return nil fi

	q:=createunit0(p.tag)

	q^:=p^
	q.nextunit:=nil
	for i to jsubs[q.tag] do
		q.abc[i]:=duplunit(q.abc[i])
	od

	return q
end

global function checkblockreturn(unit p)int=
!p is any statement
!check p, or the last statement of a block, or the last statement of any
!nested block, a return, or is a unit yielding some value (as explicit return
!statement not needed)
! return 1/0 for return/not return
	unit e,wt
	int m,res

	if p=nil then return 0 fi

	m:=p.mode

	case p.tag
	when jreturn then			!that's an easy one...
		return 1
	when jstop then
		return 1
	when jif then
		e:=p.b
		while e, e:=e.nextunit do
			if not checkblockreturn(e) then return 0 fi
		od

		return checkblockreturn(p.c)		!all branches must have a return

	when jblock then
		e:=p.a
		if e then
			while e and e.nextunit do
				e:=e.nextunit
			od
			return checkblockreturn(e)
		fi

	when jcase, jswitch, jdocase, jdoswitch then
		wt:=p.b
		while wt do
			if not checkblockreturn(wt.b) then
				return 0
			fi

			wt:=wt.nextunit
		od

		return checkblockreturn(p.c)		!else

	when jassem then						!assume yes
		return 1
	esac

	if jisexpr[p.tag] and m<>tvoid then
		return 1							!any non-void expr allowed as return value
	else
		return 0
	fi
end

global function isconstunit(unit a)int=
	return a.isconst
end

global proc getownername(symbol d, ichar dest)=
	symbol owner

	owner:=d.owner

	if owner=nil or owner.nameid=programid then return fi
	getownername(owner,dest)
	strcat(dest,owner.name)
	strcat(dest,".")
end

global function getalignment(int m)int=
!return alignment needed for type m, as 1,2,4,8
	int a

	case ttbasetype[m]
	when tarray then
		return getalignment(tttarget[m])
	when trecord then
		return 8
	elsif ttisblock[m] then
		return 8
	esac

	a:=ttsize[m]
	case a
	when 1,2,4,8 then
		return a
	when 0 then
		return 8
	esac
	cpl Strmode(m)
	gerror("GETALIGN SIZE NOT 1248")

	return 0
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

global proc addlistunit(unit &ulist,&ulistx,unit p)=
!add unit p to unit structure ulist,^ulistx  which can be null
	if ulist=nil then		!first
		ulist:=ulistx:=p
	else
		ulistx.nextunit:=p
	fi
	ulistx:=p			!update end-of-list pointer
end

global function storemode(symbol owner, int m, int32 &pmode)int =
	ref typenamerec r

	if m>=0 then
		pmode:=m
		return m
	fi

	r:=&typenames[-m]

	if r.pmode=nil then
		r.owner:=owner
		pmode:=m
		r.pmode:=&pmode

	IF R.PMODE=NIL THEN SERROR("PMODE=NIL") FI

		return m
	fi

!Already one instance of this mode; need a new slot
	m:=newtypename(r.defa, r.defb)
	r:=&typenames[-m]

	r.owner:=owner
	pmode:=m
	r.pmode:=&pmode
	return m
end

global function gettypebase(int m)int=
	switch ttbasetype[m]
	when ti8,ti16,ti32 then ti64
!	when tu8,tu16,tu32 then tu64
	when tu8,tu16,tu32 then ti64

	when tr32 then tr64

	when tc8 then tc64
	else
		m
	end switch
end

global proc writegsfile(ichar filename, ref strbuffer d)=
	filehandle f

	f:=fopen(filename,"w")
	gs_println(d,f)
	fclose(f)
end

global proc addtolog(ichar filename, filehandle logdest)=
	filehandle f
	int c

	f:=fopen(filename,"rb")

	if f=nil then return fi

	do
		c:=fgetc(f)
		exit when c=c_eof
		fputc(c,logdest)
	od
	fclose(f)
end

global function getprocretmodes(unit p)symbol=
!p must be a call unit, for a proc with multiple values; at least one expected
!however, here it only populates retmodes with the available types
	symbol d
	unit a

	if p.tag<>jcallfn then txerror("multass/need multfn") fi
!	if p.tag not in [jcallfn,jcallproc] then txerror("multass/need multfn") fi
	a:=p.a

	case a.tag
	when jname then
		return a.def
	else
		return ttnamedef[tttarget[a.mode]]
	esac
end

global function getmemmode(unit p)int =
	if p.memmode then
		return p.memmode
	fi
	return p.mode
end

global function getpclmode(int t)int u=
	u:=ttbasetype[t]
	case u
	when tc64 then u:=tu64
	when tc8 then u:=tu8
	when trecord, tarray then
		if not ttisblock[t] then
			case ttsize[t]
			when 8 then u:=tu64
			when 4 then u:=tu32
			when 2 then u:=tu16
			else u:=tu8
			esac
		fi
	esac
	return u
end

global function getfullname(symbol d)ichar=
!create fully qualified name into caller's dest buffer
	static [128]char str
	[16]symbol chain
	int n:=0
	symbol e:=d

	if d.isimport then
		return d.name
	fi

	repeat
		chain[++n]:=e
		e:=e.owner
	until e=nil or e.nameid=programid

	strcpy(str,chain[n].name)
	for i:=n-1 downto 1 do
		strcat(str,".")
		strcat(str,chain[i].name)
	od

	return str
end

global func addstring(ichar s,t)ichar =
	static[256]char str
	strcpy(str,s)
	strcat(str,t)
	str
end
