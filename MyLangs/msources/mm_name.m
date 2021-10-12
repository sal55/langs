import mlib
import clib

import mm_decls
import mm_tables
import mm_support
import mm_lib
import mm_diags
import* mm_pcl

ref strec currstproc
int allowmodname=0
int noexpand, noassem
int macrolevels

const maxmacroparams=50
[maxmacroparams]ref strec macroparams
[maxmacroparams]ref strec macroparamsgen
[maxmacroparams]unit macroargs
int nmacroparams
int nmacroargs

global proc rx_typetable=
	ref strec d

	for i:=tuser to ntypes do
		if ttbasetype[i]=trecord then
			d:=ttnamedef[i]
			if d.baseclass then
				do_baseclass(d)
			fi
		fi
	od
end

global proc rx_unit(ref strec owner, unit p)=
	ref strec d
	unit a,b
	int n,oldnoexpand,oldnoassem,oldtag,useparams

	a:=p.a
	b:=p.b
	mlineno:=p.pos

	switch p.tag
	when j_name then
		resolvename(owner,p)
		if P.TAG=J_NAME AND p.def.nameid=macroid and not noexpand then
			++macrolevels
			expandmacro(p,p,nil)
			rx_unit(owner,p)
			--macrolevels
		fi

	when j_keyword then
		rx_unit(owner,b)		!do param value only

	when j_dot then
		if b.tag=j_name then
			d:=resolvetopname(owner,b.def,b.moduleno,fmodule:0,fdoambig:0)
		fi
		resolvedot(owner,p)

	when j_callproc, j_callfn then
		oldtag:=p.tag

		if a.tag=j_name then			!can expand possible macro if params not ready
			oldnoexpand:=noexpand; noexpand:=1
			rx_unit(owner,a)
			noexpand:=oldnoexpand
		else
			rx_unit(owner,a)
		fi

		rx_unitlist(owner,b)

		if a.tag=j_name then
			d:=a.def
			case d.nameid
			when typeid then		!change to type conversion
				p.tag:=j_convert
				storemode(owner,d.mode,p.convmode)
				p.a:=b
				p.b:=nil; p.hasb:=0
				if b.nextunit then
					p.a:=createunit1(j_makelist,b)
					n:=0
					while b do
						++n
						b:=b.nextunit
					od
					p.a.length:=n
				fi
			when macroid then
				++macrolevels
				if d.deflist then			!macro uses params
					expandmacro(p,a,b)
					b:=nil
					useparams:=0
				else						!macro has no params
					expandmacro(p,a,nil)
					useparams:=1
				fi

				rx_unit(owner,p)
				--macrolevels

				if useparams and p.tag not in [j_callproc, j_callfn] then
					insertunit(p,oldtag)
					p.b:=b					!note b may be nil
					p.hasb:=1
				FI

			else
				if d.mode=tvoid then
					p.tag:=j_callproc
				fi
			esac
		fi

	when j_andl, j_orl then
		rx_unit(owner,a)
		rx_unit(owner,b)
		if not isboolunit(a) then insertunit(a,j_istruel); a.pclop:=kistruel fi
		if not isboolunit(b) then insertunit(b,j_istruel); b.pclop:=kistruel fi

	when j_istruel then
	doistruel::
		rx_unit(owner,a)

		if isboolunit(a) then
			deleteunit(p,a)
		fi
		goto doabc

	when j_notl then
		rx_unit(owner,a)
		if a.tag=j_notl then
			deleteunit(p,a)
			p.tag:=j_istruel
			p.pclop:=kistruel
			a:=p.a
			goto doistruel
		fi
		if not isboolunit(a) then
			insertunit(a,j_istruel); a.pclop:=kistruel
			a:=p.a
		fi
		goto doabc

	when j_assemmacro then
		resolvename(owner,a)
		if not noexpand then
			++macrolevels
			oldnoassem:=noassem
			noassem:=1
			expandmacro(p,a,b)
			noassem:=oldnoassem
			rx_unit(owner,p)
			--macrolevels
		fi

	else
doabc::
		if p.hasa then rx_unitlist(owner,a) fi
		if p.hasb then rx_unitlist(owner,b) fi
		if p.hasc then rx_unitlist(owner,p.c) fi
	endswitch
end

global function rx_module(int n)int=
	modulerec m
	ref strec stmodule, d
	int globalflag,status

	currmoduleno:=n

	rx_passdef(stprogram,moduletable[n].stmodule)

	return 1
end

global proc rx_deflist(ref strec owner,p)=
	ref strec pstart:=p
	while p do
		rx_passdef(owner,p)
		p:=p.nextdef
	od
end

global proc rx_passdef(ref strec owner,p)=
	ref strec d

	case p.nameid
	when moduleid,dllmoduleid then
		rx_deflist(p,p.deflist)

	when procid then
		rx_deflist(p,p.deflist)
		currstproc:=p
		rx_unit(p,p.code)
		currstproc:=nil

	when dllprocid then
		rx_deflist(p,p.deflist)

	when constid,staticid,frameid,paramid then
		if p.atvar then
			rx_unit(owner,p.equivvar)
		fi
		if p.code then
			rx_unit(owner,p.code)
		fi
	when typeid then
		rx_deflist(p,p.deflist)

	else
	esac
end

proc rx_unitlist(ref strec owner, unit p)=
	while p do
		rx_unit(owner,p)
		p:=p.nextunit
	od
end

global function resolvetopname(ref strec owner,stnewname,int moduleno,
	fmodule,fdoambig=1)ref strec=
!stnewname points to a symrec with nullid
!This is a top-level name (left-most name of any dotted sequence, or standalone name)
!Search through all the duplicate symrecs (all names with identical names have symrecs that
!are linked together, always starting with a nullid symrec) looking for the best match
!moduleno is the module where the currently generic name is encountered
!(derived from a unit if in an expression, or an STREC if a type in a declaration)

	int i,m,extcount,modno
	ref strec p,powner,d,e,dlldef,extdef,moddef,extmod,q
	[10]ref strec ambiglist

	if owner.nameid=procid then
		q:=owner.deflist
		while q do
			if q.firstdupl=stnewname then		!use that match
				return q
			fi
			q:=q.nextdef
		od
	fi

	p:=stnewname.nextdupl

	extcount:=0
	extmod:=dlldef:=extdef:=moddef:=nil

	while p do						!for each possibe st entry of the same name

		powner:=p.owner			!the owner of that entry

		switch powner.nameid
		when procid then
			if powner=owner then			!immediate match
				return p
			fi
		when moduleid then			!p is file-scope item
			if powner.moduleno=moduleno then		!same module
				if owner.nameid=moduleid then	!immediate match
					return p
				fi
				moddef:=p			!take note, but continue searching (in case proc etc)
			elsif moduletable[moduleno].importmap[powner.moduleno] then
				if p.isglobal then
									!matches an external module imported by this name's module
					++extcount			!if an ext match is closest, there can only be one
					extdef:=p
					if extcount<ambiglist.len then
						ambiglist[extcount]:=extdef
					fi
				fi
			fi
		when dllmoduleid then
			modno:=powner.owner.moduleno
			if modno=moduleno or moduletable[moduleno].importmap[modno] then
				dlldef:=p
			fi

		when typeid then
			if powner=owner then			!immediate match
				return p
			fi
			if powner=owner.owner then
				return p
			fi
		when programid then					!p is a module
			if p.nameid=moduleid then		!match a module name
				if p.moduleno=moduleno then
					if fmodule then
						return p			!immediate match (unless proc but that would have
					fi						!matched by now
				else						!ext module
					extmod:=p				!keep it in reserve
				fi
			fi
		endswitch

		p:=p.nextdupl
	od

	if moddef then				!go with that first
		return moddef
	fi
	if extdef then
		if extcount>1 and fdoambig then
			for i:=1 to extcount do
				extdef:=ambiglist[i]
				println i,extdef.owner.name,namenames[extdef.owner.nameid]
			od
			rxerror_s("Ambiguous ext name: #",extdef.name)
		fi
		return extdef
	fi
	if extmod then return extmod fi
	return dlldef				!will be nil when no match
end

global proc resolvename(ref strec owner, unit p)=
!p is a name tag inside given owner
!resolve name
!report error if unresolved, unless mode is not void. Then an unresolved
!name is added as a frame (assumes this is a proc)

	ref strec d,e
	unit q
	int moduleno, mode

	d:=p.def
	moduleno:=p.moduleno

	if d.nameid<>nullid then			!assume already resolved
		return
	fi

	e:=resolvetopname(owner,d,moduleno,allowmodname)
	if not e then
		mode:=tvoid
		case p.avcode
		when 'I', 'T', 'S' then mode:=ti64
		when 'L','A' then mode:=tany
		esac

		if mode=tvoid then
			rxerror_s("Undefined: #",d.name,p)
		else
			e:=addframevar(owner,d,moduleno,mode)
			e.lineno:=p.lineno
			if mode<>tany then e.islet:=1 fi
		fi
	fi

	e.used:=1

	if e.nameid=paramid and e.parammode=out_param then
		p.tag:=j_ptr
		p.a:=createname(e)
		p.hasa:=1; p.hasb:=p.hasc:=0
	else
		p.def:=e			!update link in kcode
	fi

end

global function finddupl(ref strec d, pdupl)ref strec=
!trying to resolve a field name, by scanning a dupllist headed by pdupl
!which ought to point to nullid entry
!d will be the owner of the matching entry

	if pdupl.nameid<>nullid then		!assume already resolved
		return pdupl
	fi
	pdupl:=pdupl.nextdupl

	while pdupl do
		if pdupl.owner=d then
			return pdupl
		fi
		pdupl:=pdupl.nextdupl
	od
	return nil
end

proc resolvedot(ref strec owner,unit p)=
	unit lhs,rhs
	ref strec d,e,t,f
	int m

	lhs:=p.a
	rhs:=p.b
	e:=rhs.def				!p.b will be a name type (could perhaps be stored as p.def)

	rx_unit(owner,lhs)

	case lhs.tag
	when j_name then
		d:=lhs.def
		case d.nameid
		when moduleid,typeid,procid,typeid,dllmoduleid then
			e:=finddupl(d,e)
			if e then
				p.tag:=j_name			!convert to dot to name
				p.a:=p.b:=nil
				p.hasa:=p.hasb:=0
				p.def:=e
				case e.nameid
				when enumid then
				when constid then
				esac
			else
				rxerror_s("Can't resolve .#",p.b.def.name,p)
			fi

		when frameid, staticid, paramid then		!.x applied to normal var
			m:=d.mode
			case ttbasetype[m]
			when trecord then
			when tref then
				do
					m:=tttarget[m]
					case ttbasetype[m]
					when trecord then
						exit
					when tref then
					else
						rxerror("2:record expected")
					esac
				od
			else
				rxerror("record expected")
			esac
			t:=ttnamedef[m]

			e:=finddupl(t,e)
			if e then
				p.b.def:=e
			else
				rxerror_s("Not a field: #",rhs.def.name)
			fi
		esac

	else
!Can't fully resolve at this time; leave to next pass
		unless e.nextdupl then
			rxerror_s("Not a field: #",e.name)
		endunless
	esac
end

proc fixmode(ref typenamerec p)=
!p refers to a negative mode that is a typename index
!fix that up if possible
	ref int32 pmode
	ref strec a,d,e,f,owner
	int m,moduleno

	pmode:=p.pmode

	m:=-pmode^					!typename index

	d:=owner:=p.owner
	while d.nameid<>moduleid do d:=d.owner od
	moduleno:=d.moduleno

	a:=p.defa
	d:=p.defb

	if a=nil and d then			!simple type name V
		e:=resolvetopname(owner,d,moduleno,0)

	elsif d=nil and a then		!typeno
		rxerror("Fixmode can't do typeof yet")
	else						!assume a.d type reference
		e:=resolvetopname(owner,a,moduleno,0)
		if e then
			f:=e.deflist
			e:=nil
			while f do
				if f.nameid=typeid and f.firstdupl=d then
					e:=f
					exit
				fi
				f:=f.nextdef
			od

		fi

	fi

	if e and e.nameid=typeid then
		pmode^:=e.mode

	else
		rxerror_s("2:Can't resolve tentative type: #",d.name)
	fi
end

global proc fixusertypes=
	ref typenamerec p
	int npasses,notresolved,m,zerosizes
	ref strec d

	npasses:=0
	repeat
		++npasses
		notresolved:=0

		for i to ntypenames do
			p:=&typenames[i]

			if p.pmode^<0 then
				mlineno:=typenamepos[i].pos
				fixmode(p)
				if p.pmode^<0 then
					++notresolved
				fi
			fi
		od

		if npasses>5 then
			println "Type phase errors - check these user types:"

			for i to ntypenames do
				p:=&typenames[i]

				if p.pmode^<0 then
					d:=p.defb
					if d=nil then d:=p.defa fi
					println "	",d.name
				fi
			od

			rxerror("Fixtypes: too many passes (cyclic ref?)")
		fi

	until notresolved=0
end

global proc fixblockparams=
!make sure all block params are changed to references

	ref procrec pp
	ref strec d,e

	RETURN

	pp:=proclist
	while pp do
		d:=pp.def
		e:=d.deflist
		while e do
			if e.nameid=paramid then
				if ttbasetype[e.mode] in [trecord, tarray] then
					e.parammode:=out_param
					e.mode:=createrefmode(d,e.mode,0)
				fi
			fi
			e:=e.nextdef
		od
	
		pp:=pp.nextproc
	od
end

global function resolve_equiv_name(ref strec owner,p)ref strec=
!@p or @p+offset used for a field offset
!owner is record type of which it should be a member
!currently, p is at strec that might be null
!return matching fieldid name
	if p.nameid=fieldid then
		return p
	fi

	RXERROR("RESOLVE EQUIV FIELD/COMPLEX")

	return nil
end

function addframevar(ref strec owner, d, int moduleno, mode)ref strec=
!owner should be a proc; d is a generic st entry
!add framewith the name of d and given mode to the proc
	ref strec e
	e:=getduplnameptr(owner,d,frameid)
	storemode(owner,mode,e.mode)
	adddef(owner,e)
	return e
end

function copylistunit(unit p)unit=
	unit q

	unit plist,plistx
	plist:=plistx:=nil
	while p do
		q:=copyunit(p)
		addlistunit(&plist,&plistx,q)
		p:=p.nextunit
	od
	return plist
end

function copyunit(unit p)unit=
	unit q
	ref strec d

	if p=nil then return nil fi

!need to quickly check if a name unit is a macroparam

	if p.tag=j_name then
		d:=p.def
		for i to nmacroparams do
			if macroparamsgen[i]=d then
				return copyunit(macroargs[i])
				exit
			fi
		od
	fi

	q:=createunit0(p.tag)

	q^:=p^
	q.nextunit:=nil
	if q.hasa then q.a:=copylistunit(q.a); q.hasa:=1 fi
	if q.hasb then q.b:=copylistunit(q.b); q.hasb:=1 fi
	if q.hasc then q.c:=copylistunit(q.c); q.hasc:=1 fi

	return q
end

proc replaceunit(unit p,q)=
!replace p with q, keeping same address of p, and same next pointer
!original contents discarded
	unit pnext
	pnext:=p.nextunit
	p^:=q^
	p.nextunit:=pnext
end

proc expandmacro(unit p, a, b)=
!a is a macro name unit, b is a macro parameter list (rx-processed), which
!can be nil
!p is either the call-unit as this may originally have been, or the same as a::
!M => (NAME)
!M(A,B) => (CALL NAME (A,B))
!Need to replace M or M(A,B) with the duplicated AST from a.code.
!When a name appears in the AST which is a local macroparam name, then that is
!replaced by the corresponding argument from B;
!The new code replaces P (either CALL or NAME)
!Supplied args may be more than macro takes (error) or may be less (error,
!or allow default arg values to be specified)
	ref strec d,pm
	unit pnew
	int ignoreargs

	if macrolevels>10 then
		rxerror("Too many macro levels (recursive macro?)")
	fi

	d:=a.def

!First step: get list of macro formal parameters

	pm:=d.paramlist
	nmacroparams:=0
	while pm do
		if nmacroparams>=maxmacroparams then
			rxerror("macro param overflow")
		fi
		macroparams[++nmacroparams]:=pm
		macroparamsgen[nmacroparams]:=pm.nulldef
		pm:=pm.nextparam
	od

!now get macro args into a list
	nmacroargs:=0

	while b do
		if nmacroargs>=maxmacroparams then
			rxerror("macro arg overflow")
		fi
		macroargs[++nmacroargs]:=b
		b:=b.nextunit
	od

	if nmacroargs<nmacroparams then
		PRINTLN =NMACROARGS, NMACROPARAMS
		rxerror("Too few macro args")
	fi

	ignoreargs:=0
	if nmacroargs>0 and nmacroparams=0 then		!ignore extra params
		ignoreargs:=1
		nmacroargs:=nmacroparams:=0

	elsif nmacroargs>nmacroparams then
		rxerror("Too many macro args")
	fi

	pnew:=copyunit(d.code)

	if not ignoreargs then				!normal expansion
		replaceunit(p,pnew)
	else								!keep call and paramlist; just replace fn name
		p.a:=pnew						!with expansion
	fi
end

proc duplfield(ref strec owner,p,q)=
!p is strec of an existing field, const etc
!q is a newly created strec with the same id and name
!copy the relevant fields of p to q

	if p.code then
		serror("DUPLFIELD")
	fi

!Need to copy whatever are relevant attributes

	q.atfield:=p.atfield
	q.flags:=p.flags

	q.uflags:=p.uflags		!for .uflags
	storemode(owner,p.mode,q.mode)
end

proc do_baseclass(ref strec p)=
!p is class name, which has a baseclass, do the copying necessary for
!inheriting fields
	ref strec d,e,newd,dbase
	int normalexit

	dbase:=ttnamedef[p.baseclass]
	d:=dbase.deflist

	while d do				!for each element of base class
		e:=p.deflist

		normalexit:=1
		while e do			!for each element of new class
			if eqstring(d.name,e.name) then
				normalexit:=0
				exit
			fi
			e:=e.nextdef
		od
		if normalexit then
!duplicate d in this class; keep it simple for now
!(procs will need a more elaborate duplication, and really needs to share code)
			case d.nameid
			when procid,linkid then
				newd:=getduplnameptr(p,d,linkid)
				newd.equivfield:=d
			else
				newd:=getduplnameptr(p,d,d.nameid)
				duplfield(p.owner,d,newd)
			esac
			adddef(p,newd)
		fi
		d:=d.nextdef
	od
end
