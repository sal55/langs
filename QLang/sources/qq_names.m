!Symbol table handling

int sdsize, sdoffset
int sdaligned
int sdlevel
int sdmode
int sdnfields
int sdmaxalign
const int maxstructdepth=10
[maxstructdepth]byte sdunion		!1 if union model 0 for normal offset calc
[maxstructdepth]int sdmaxsize		!accumulate max size of union

global function addglobalname(ichar name)symbol=
!generic name encountered in namedef op. Convert to symbol reference
!will always return a generic strec, either existing, or just created
	lexrec oldlx
	symbol d

	oldlx:=nextlx

	lookup(name,strlen(name),gethashvaluez(name))

	d:=nextlx.symptr
	nextlx:=oldlx
	return d
end

function newstrec:symbol=
	symbol p
	p:=pcm_alloc(strec.bytes)
	memset(p,0,strec.bytes)

	return p
end

global function addsymbol(symbol owner,d, int id, isglobal)symbol e=
!d should be a generic symbol (or if nil, then when name is provided
!to create a suitable generic symbol0
!create a dedicated strec for it, link it in to the dupl chain of the generic
!version, and insert it a child of owner
	symbol f

	e:=newstrec()
	e.name:=d.name
	e.namelen:=d.namelen
	e.owner:=owner
	e.nameid:=id
	e.isframe:=id=frameid or id=paramid

!CPL "ADDSYM",=D.NAME
	if currmodule then
		e.moduleno:=currmodule.moduleno
	fi

	e.firstdupl:=d
	e.isglobal:=isglobal

!IF OWNER.NAMEID<>PROCID THEN
	return e when not owner			!not linked in to anything

IF OWNER.NAMEID NOT IN [PROCID, ANONPROCID] THEN
	e.nextdupl:=d.nextdupl
	d.nextdupl:=e


	if e.nextdupl and e.nextdupl.owner=owner then
		cpl e.name,"in",owner.name
		serror("AS:Duplicate name")
	fi
else
	f:=owner.deflist
	while f do
		if f.firstdupl=e.firstdupl then
			cpl e.name,"in",owner.name
			serror("AS2:Duplicate name")
		fi
		f:=f.nextdef
	od
fi

	if owner.deflist=nil then			!first def
		owner.deflist:=e
	else
		owner.deflistx.nextdef:=e
	fi
	owner.deflistx:=e

	return e
end

global proc addproc(symbol d)=
	ref procrec p

	p:=pcm_allocz(procrec.bytes)
	p.def:=d

	if proclist=nil then
		proclist:=p
	else
		proclistx.nextproc:=p
	fi
	proclistx:=p
	++nproclist
end

!global function createstroot(ichar name)symbol d=
!	d:=newstrec()
!	d.name:=pcm_copyheapstring(name)
!	d.namelen:=strlen(name)
!	d.nameid:=programid
!
!	return d
!end

global function newusertypex(ref strec d,e=nil)int=
	int i

	if nuserxtypes>=maxuserxtype then
		serror("Too many external user types")
	fi
	++nuserxtypes
	ttnamedefx[nuserxtypes]:=d
!	ttnamedefx2[nuserxtypes]:=e

	ttxmoduleno[nuserxtypes]:=stcurrmodule.moduleno
	return -nuserxtypes
end

global function resolvedottedname(symbol owner, d)symbol e=
!d should be generic

	e:=d.nextdupl
	while e and e.owner<>owner do
		e:=e.nextdupl
	od

	return e
end

global proc addgenfield(symbol d)=
	int index
	symbol dgen
	ref genfieldrec g


	dgen:=d.firstdupl
	index:=dgen.genfieldindex

!CPL "ADDGENFIELD",D.NAME,namenames[d.nameid],=INDEX

	if index=0 then			!first field with this name
		if ngenfields>=maxgenfield then
			pcerror("Too many genfields")
		fi
		dgen.genfieldindex:=index:=++ngenfields
	fi

	g:=pcm_alloc(genfieldrec.bytes)
	g.def:=d
	g.nextdef:=genfieldtable[index]
	genfieldtable[index]:=g
end

!global function addusertype(symbol d)int=
!!d is the name of a new user type; the details have been set up inside it
!!but now create the actual type
!
!	if ntypes>=maxtype then pcerror("Too many types") fi
!
!	++ntypes
!	d.mode:=ntypes
!	ttnamedef[ntypes]:=d
!	ttname[ntypes]:=d.name
!
!	return ntypes
!
!end

global function makereftype(int target, symbol owner=nil)int=
!owner <> nil means used for new type so cannot reuse existing ref

	int newtype

	if owner=nil then
		for i:=tlast+1 to ntypes do
			if ttbasetype[i]=trefpack and tttarget[i]=target then
				return i
			fi
		od
	fi

	newtype:=addanontype()
	ttbasetype[newtype]:=trefpack

	storemode(stcurrproc,target,&tttarget[newtype])

	ttsize[newtype]:=8
	ttbitwidth[newtype]:=64
!	ttcat[newtype]:=refcat
	return newtype
end

global function makeaxtype(int target, unit plower, plength)int=
	int newtype,length

	newtype:=addanontype()

	ttbasetype[newtype]:=tvector
	storemode(stcurrproc, target, &tttarget[newtype])

	ttlower[newtype]:=1
	ttlengthexpr[newtype]:=plength
	ttlowerexpr[newtype]:=plower
!	ttcat[newtype]:=blockcat			!may be adjusted later
!	ttsize[newtype]:=length*ttsize[target]



	return newtype
end

global function makeslicetype(int target)int=
	int newtype,length

	newtype:=addanontype()



	ttbasetype[newtype]:=tslice
	storemode(stcurrproc, target, &tttarget[newtype])

	ttlower[newtype]:=1
	ttsize[newtype]:=16

!CPL "SLICE",=STRMODE(TTBASETYPE[NEWTYPE])

	return newtype
end

!global function makestrtype(int m, width)int=
global function makestrtype(int m, unit pwidth)int=
	int newtype

	newtype:=addanontype()
	ttbasetype[newtype]:=m
!	ttispacked[newtype]:=1
!	ttlength[newtype]:=width
	ttlengthexpr[newtype]:=pwidth
	ttlower[newtype]:=1
	ttowner[newtype]:=stcurrproc
!	ttsize[newtype]:=width
	return newtype
end

global function addanontype:int=
!d is the name of a new user type; the details have been set up inside it
!but now create the actual type
	[32]char str

	if ntypes>=maxtype then pcerror("Too many types") fi

	++ntypes
!CPL "ADDANONTYPE",NTYPES
	print @str,"$T",,ntypes

!CPL "ADDANON TYPE", STCURRPROC.NAME
	ttname[ntypes]:=pcm_copyheapstring(str)
	ttowner[ntypes]:=stcurrproc

	return ntypes

end

global proc createusertype(symbol d, int m)=
	storemode(stcurrproc,m,&d.mode)

	if m>tlast and ttnamedef[m]=nil then
		ttnamedef[m]:=d
		ttname[m]:=d.name
		ttowner[m]:=d.owner
!		ttcat[m]:=gettypecat(m)
	fi
end

!global function roundoffset(int offset, alignment)int=
!	int mask
!
!	if alignment=1 then return offset fi
!	mask:=alignment-1
!	while offset iand mask do ++offset od
!
!	return offset
!end

global function getalignment(int m)int=
!return alignment needed for type m, as 1,2,4,8
	int a

	case ttbasetype[m]
	when tvector then
		return getalignment(tttarget[m])
!	when tpstruct then
	when tstruct then
!		return ttnamedef[m].structmaxalign
	esac

	a:=ttsize[m]
	case a
	when 1,2,4,8 then
		return a
	esac
	cpl ttname[m],a
	pcerror("Getalign not 1248")

	return 0
end

global proc duplfield(ref strec p,q)=
!p is strec of an existing field, const etc
!q is a newly created strec with the same id and name
!copy the relevant fields of p to q

	if p.code then
		serror("DUPLFIELD")
	fi
	q.atfield:=p.atfield
	q.index:=p.index
	q.fieldoffset:=p.fieldoffset
end

proc writesig(symbol d, filehandle dev)=
	symbol e
	int n
	fprint @dev, "# #(", (d.misfunc|"function"|"proc"), d.name

	e:=d.deflist
!CPL "PARAMS",D.NPARAMS
	n:=0
	while e, e:=e.nextdef do
		if e.nameid=paramid then
			++n
			if e.moptional and e.code then
				fprint @dev,"#=#", e.name, strexpr(e.code).strptr
			elsif e.moptional then
				print @dev,"?",,e.name
			else
				print @dev,e.name
			fi

			if n<d.nparams then
				print @dev,", "
			fi
		fi
	od

	fprintln @dev,")	[#]", d.owner.name

end

global function createdupldef(symbol owner,symptr, int id)symbol=
!create new proc entry
!symptr is the generic st entry for proc's name
	symbol p,q

	p:=newstrec()

	p.name:=symptr.name
	p.namelen:=symptr.namelen
	p.symbolcode:=namesym
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
