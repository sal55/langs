import msys
import mlib
import clib
import oslib

import mm_decls
import mm_tables
import mm_support
import mm_lib
import mm_name
import mm_diags
import* mm_pcl

const nolv=0
const needlv=1

const maxparams=100
const maxfields=200
int countedfields
int inassem

global proc $init=
	inittypetables()
end

global proc tx_allprocs=
	ref procrec pp
	unit pcode

	pp:=proclist
	while pp do
		currproc:=pp.def
		pcode:=currproc.code

		if ttisshort[currproc.mode] then
			mlineno:=currproc.pos
			txerror("proc short ret type")
		 fi

	    tpass(pcode,(currproc.nretvalues>1|ttuple|currproc.mode))

		case ttbasetype[currproc.mode]
		when tvoid then		!PROC
		when ttuple then	!MULT FN
		else				!REGULAR FN
			if pcode.tag<>j_return then
				insertunit(pcode,j_return)
				pcode.mode:=currproc.mode
				pcode.resultflag:=1
			fi
		esac

		pp:=pp.nextproc
	od
end

proc tpass(unit p, int t=tany, lv=nolv)=
	ref strec d
	unit a,b,c
	int oldmlineno,m,nparams,paramtype,restype

	if p=nil then return fi

	oldmlineno:=mlineno

	mlineno:=p.pos

	a:=p.a
	b:=p.b

	p.resultflag:=t<>tvoid

	switch p.tag
	when j_name then
		tx_name(p,t,lv)
	when j_const, j_decimal then

	when j_typeconst then
		p.mode:=ti64

	when j_bytesize, j_bitwidth then
		tx_bytesize(p,a)

	when j_bin then
		tx_bin(p,a,b)

	when j_unary then
		tx_unary(p,a)

	when j_binto then
		tx_binto(p,a,b)

	when j_unaryto then
		tx_unaryto(p,a)

	when j_assign,j_deepcopy then
		tx_assign(p,a,b,t)

	when j_multexpr then
		while a do
			tpass(a)
			a:=a.nextunit
		od

	when j_cmp then
		tx_cmp(p,a,b)

	when j_addrof then
		if a.tag=j_ptr then
			deleteunit(p,a)
			deleteunit(p,p.a)
			tpass(p,t)
		else
			tpass(a,,needlv)
			p.mode:=createrefmode(nil,a.mode)
		fi

	when j_addroffirst then
		tx_addroffirst(p,a,t)

	when j_if then
		tx_if(p,a,b,p.c,t,lv)

	when j_longif then
		tx_longif(p,a,b,t,lv)

	when j_index then
		tx_index(p,a,b,t,lv)

	when j_ptr then
		tx_ptr(p,a,t,lv)

	when j_callproc, j_callfn then
		tx_callproc(p,a,b,t)

	when j_dot then
		tx_dot(p,a,b,lv)

	when j_andl, j_orl, j_xorl then
		tx_andl(p,a,b)

	when j_notl then
		tx_notl(p,a)

	when j_istruel then
		tx_istruel(p,a)

	when j_convert then
		tx_convert(p,a,1)

	when j_typepun then
		tx_typepun(p,a)

	when j_sliceptr then
		tx_sliceptr(p,a)

	when j_incr then
		tx_incrto(p,a,t)

	when j_makerange then
		tx_makerange(p,a,b)

	when j_makeset then
		tx_makeset(p,a,t)

	when j_swap then
		tx_swap(p,a,b)

	when j_select then
		tx_select(p,a,b,p.c,t,lv)

	when j_switch, j_doswitch then
		tx_switch(p,a,b,p.c,t,lv)

	when j_case, j_docase then
		tx_case(p,a,b,p.c,t,lv)

	when j_exprlist then
		tx_exprlist(p,a,t)

	when j_dotindex, j_dotslice, j_anddotindex then
		tx_dotindex(p,a,b,lv)

	when j_slice then
		tx_slice(p,a,b)

	when j_block,j_stmtblock then
		tx_block(p,a,t,lv)

	when j_eval then
		tpass(a,tany)

	when j_do then
		tpass(a,tvoid)

	when j_return then
		tx_return(p,a,t)

	when j_print,j_println,j_fprint,j_fprintln then

		tx_unitlist(a)
		fixchararray(a)

		while b do
			if b.tag=j_fmtitem then
				tpass(c:=b.a)
				tpass(b.b,trefchar)
			else
				tpass(c:=b)
			fi
			fixchararray(c)
			b:=b.nextunit
		od
		tx_unitlist(p.c)

	when j_forup, j_fordown then
		tx_for(a,b,p.c)

	when j_forall, j_forallrev then
		tx_forall(a,b,p.c)

	when j_to then
		tpass(a,ti64)
		tpass(b,tvoid)
		tpass(p.c,ti64)		!when autovar present

	when j_autocast then
		tpass(a)
		if t=tany then txerror("cast() needs type") fi
		coerceunit(a,t,1)
		deleteunit(p,a)

	when j_makelist then
		tx_makelist(p,a,t,lv)

	when j_stop then
		tpass(a,ti64)

	when j_exit,j_redo, j_restart, j_next then
		tx_exit(p,a)

	when j_goto then
		tx_goto(p,a)

	when j_labeldef then

	when j_while then

		tcond(a)
		if iscondtrue(a) then
			p.tag:=j_do
			p.a:=b
			p.hasb:=0
		elsif iscondfalse(a) then
			p.tag:=j_null
			p.hasa:=p.hasb:=0
		fi
		tpass(b,tvoid)
		tpass(p.c,tvoid)

	when j_repeat then
		tpass(a,tvoid)
		tcond(b)
		if iscondtrue(b) or iscondfalse(b) then txerror("repeat/const cond") fi

	when j_nogap, j_space then

	when j_assem then
		if t<>tvoid then
			p.mode:=t
		fi

		inassem:=1
		tx_unitlist(a)
		tx_unitlist(b)
		tx_unitlist(p.c)
		inassem:=0

	when j_assemreg,j_assemxreg then
	when j_assemmem then
		tpass(a)

	when j_typeof then
		tpass(a)
		if a.tag=j_typeconst then
			p.value:=a.value
		else
			p.value:=a.mode
		fi
		p.tag:=j_typeconst
		p.mode:=ti64
		p.hasa:=0

	when j_typestr then
		tpass(a)
	CPL "TYPESTR",STRMODE(A.MODE)
		if a.tag=j_typeconst then
			m:=a.value
		else
			tpass(a)
			m:=a.mode
		fi
		p.tag:=j_const
		p.mode:=trefchar
		p.a:=nil; p.hasa:=0
		p.svalue:=pcm_copyheapstring(strmode(m,0))
		p.slength:=strlen(p.svalue)
		p.isastring:=1

	when j_fmtitem then
		tpass(a)
		tpass(b)

	when j_readln then
		tpass(a)

	when j_read then
		if a then
			tpass(a,tc64)
		fi
		if ttisallnum[t] then
			t:=gettypebase(t)
		fi
		p.mode:=t
	when j_recase then
		if a then
			tpass(a,ti64)
			if a.tag<>j_const then
				txerror("recase must be const")
			fi
		fi

	when j_cvlineno then
		p.mode:=ti64
	when j_cvfilename,j_cvmodulename then
		p.mode:=trefchar

	when j_bitfield then
		tx_bitfield(p,a,lv)

	when j_syscall then
		restype:=tvoid
		paramtype:=tvoid
		case p.fnindex
		when sysfn_getnprocs then restype:=ti64
		when sysfn_getprocname then paramtype:=ti64; restype:=trefchar; 
		when sysfn_getprocaddr then paramtype:=ti64; restype:=tref; 
		esac

		if paramtype<>tvoid then
			if a=nil then txerror("sys: arg missing") fi
			tpass(a,paramtype)
			if a.nextunit then txerror("sys: too many args") fi
		elsif a then txerror("sys: too many args")
		fi

		p.mode:=restype

	when j_cmpchain then
		tx_cmpchain(p,a)

	when j_empty then
		tpass(a,,needlv)

	when j_shorten then

	when j_strinclude then
		tx_strinclude(p,a)

	else
		CPL "TXUNIT: CAN'T DO:",jtagnames[p.tag]
	doelse::

		if p.hasa then tx_unitlist(a,t) fi
		if p.hasb then tx_unitlist(b,t) fi
		if p.hasc then tx_unitlist(p.c,t) fi
	endswitch

	tevaluate(p)

	case p.tag
	when j_makelist, j_return then
	else
		if t<>tany and t<>tvoid and p.mode<>t then		!does not already match
			coerceunit(p,t)			!apply soft conversion
		fi
	esac
!
	IF T=TVOID THEN
		CASE P.TAG
		WHEN J_CONST, J_BIN, j_UNARY, J_CMP THEN
			TXERROR("Eval needed")
		WHEN J_NAME THEN
			unless ttisref[p.mode] and tttarget[p.mode]=tlabel then
				TXERROR("Eval needed2")
			end

		esac
	fi

	mlineno:=oldmlineno
end

proc tx_block(unit p,a, int t,lv)=
	while a and a.nextunit do
		tpass(a,tvoid)
		a:=a.nextunit
	od
	if a then
		tx_unitlist(a,t,lv)
		p.mode:=(t<>tvoid|a.mode|tvoid)
	fi
end

global proc tx_typetable=
	ref strec d

	for i:=tuser to ntypes do
		if ttbasetype[i]=trecord then
			tx_passdef(d:=ttnamedef[i])
		fi
		setmodesize(i)
	od
end

proc setmodesize(int m)=
	int size,target

	if ttsize[m] then return fi

	mlineno:=ttlineno[m]
	case ttbasetype[m]
	when tarray then
		setarraysize(m)
	when trecord then
		setrecordsize(m)
	when tvoid,tproc then
	when tslice then
		setslicesize(m)
	when tauto then
		TXERROR("SETMODESIZE/AUTO?")
	when tany then

	when tpending then
		target:=tttarget[m]
		setmodesize(target)

		ttbasetype[m]:=ttbasetype[target]
		ttsize[m]:=ttsize[target]
		ttlower[m]:=ttlower[target]
		ttlength[m]:=ttlength[target]
		ttnamedef[m]:=ttnamedef[target]

	when tenum then
		ttsize[m]:=8
	when ttuple then

	else
		if size:=ttsize[ttbasetype[m]] then
			ttsize[m]:=size
			return
		fi
		println "SIZE 0:",strmode(m),=m,=stdnames[ttbasetype[m]]
		println "Can't set mode size"
	esac
end

proc setarraysize(int m)=
	int lower,length,elemsize,target,size
	unit pdim,a,b

	if ttsizeset[m] then return fi

	pdim:=ttdimexpr[m]

	if pdim then
		a:=pdim.a
		b:=pdim.b
		rx_unit(ttowner[m],pdim)

		case pdim.tag
		when j_makerange then
			tpass(a)
			tpass(b)
			lower:=getconstint(a)
			length:=getconstint(b)-lower+1
		when j_keyvalue then
			tpass(a)
			lower:=getconstint(a)
			if b then
				tpass(b)
				length:=getconstint(b)
			else
				length:=0
			fi
		else
			tpass(pdim)
			length:=getconstint(pdim)
			lower:=1
		esac
	else
		lower:=1
		length:=0
	fi

	ttdimexpr[m]:=nil

	ttlower[m]:=lower
	ttlength[m]:=length

	target:=tttarget[m]
	setmodesize(target)
	elemsize:=ttsize[tttarget[m]]
	ttsize[m]:=size:=length*elemsize
	ttsizeset[m]:=1
end

proc setslicesize(int m)=
	unit pdim

	if ttsize[m] then return fi

	pdim:=ttdimexpr[m]

	if pdim then
		rx_unit(ttowner[m],pdim)
		tpass(pdim)
		ttlower[m]:=getconstint(pdim)
		ttdimexpr[m]:=nil
	else
		ttlower[m]:=1
	fi

	setmodesize(tttarget[m])
	ttsize[m]:=ttsize[tslice]
end

proc tcond(unit p)=
	unit a,b

	a:=p.a
	b:=p.b

	tpass(p)
end

global function tx_module(int n)int=
	modulerec m
	ref strec stmodule, d
	int globalflag,status

	currmoduleno:=n

	tx_passdef(moduletable[n].stmodule)

	return 1
end

global proc tx_passdef(ref strec p)=
	ref strec d
	int oldmlineno
	unit q

	if p.txdone then
		return
	fi

	oldmlineno:=mlineno
	mlineno:=p.pos

	d:=p.deflist
	while d do
		tx_passdef(d)
		d:=d.nextdef
	od

	q:=p.code

	case p.nameid
	when procid then
		currproc:=nil
	when constid,enumid then
		tx_namedconst(p)
	when staticid, frameid, paramid then
		tx_namedef(p)
	esac

	p.txdone:=1
	mlineno:=oldmlineno
end

proc tx_unitlist(unit p, int t=tany, lv=nolv)=
	while p do
		tpass(p,t)
		p:=p.nextunit
	od
end

proc tx_namedef(ref strec d)=
	int m,mold
	unit dcode,pequiv

	m:=d.mode
	setmodesize(m)

	if d.circflag then
		txerror("Circular reference detected")
	fi
	if d.txdone then return fi
	dcode:=d.code

	d.circflag:=1

	if d.atvar then
		pequiv:=d.equivvar
		if pequiv.tag=j_addrof then deleteunit(pequiv,pequiv.a) fi
		if pequiv.tag<>j_name then
			txerror("@name needed")
		fi
		tpass(pequiv)
	fi

	if dcode and d.nameid<>frameid then
		mold:=m
		m:=gettypebase(m)

		if ttbasetype[m]=tslice and dcode.tag=j_const and dcode.mode=trefchar then
			tpass(dcode,trefchar)
		else
			tpass(dcode,m)
		fi
		d.circflag:=0
		d.txdone:=1
		if ttbasetype[m]=tarray and ttlength[m]=0 then
			d.mode:=dcode.mode
		fi

		if mold<>m then
			if ttisinteger[m] and ttisshort[mold] then
				insertunit(d.code,j_shorten)
				d.code.mode:=mold
			elsif mold=tr32 then
				d.code.mode:=mold
			fi
		fi

		if d.nameid=staticid then
			checkconstexpr(d.code)
		fi

	elsif dcode and d.nameid=frameid and ttbasetype[m]=tarray and ttlength[m]=0 then
		tpass(dcode,m)
		d.mode:=dcode.mode
		d.circflag:=0
		d.txdone:=1

	else
		d.circflag:=0
		d.txdone:=1
	fi
end

global proc tx_namedconst(ref strec d)=
	int m

	if d.circflag then
		txerror("Circular const reference detected")
	fi

	unit q
	if d.txdone then return fi
	q:=d.code

	m:=d.mode

	d.circflag:=1
	tx_expr(q,(m=tauto|tany|m))

	d.circflag:=0
	checkconstexpr(q)
	if m=tauto then
		d.mode:=q.mode
	fi

	d.txdone:=1
end

proc tx_expr(unit p, int t=tany)=
	tpass(p,t)
end

proc checkconstexpr(unit p)=
!check whether p is const expr
	unit q
	int pmode

	case p.tag
	when j_const then
		return
	when j_makelist then
		q:=p.a
		while q do
			checkconstexpr(q)
			q:=q.nextunit
		od

	when j_convert then

		if ttbasetype[p.a.mode]=tref then
			if tttarget[p.a.mode]=tvoid then
				p.a.mode:=p.mode
				deleteunit(p,p.a)
			else
				goto error
			fi
		fi
		goto error

	when j_shorten then
		checkconstexpr(p.a)

	when j_addrof, j_addroffirst then
		case p.a.tag
		when j_name then
		else
			goto error
		esac

	when j_name then
		if p.def.nameid=fieldid then return fi
		error
	else
	error::
		println jtagnames[p.tag],STRMODE(P.MODE)
	PRINTUNIT(P)
		txerror("Getconstexpr: not const")
	esac
end

function getconstint(unit q, int t=tany)int64=
	checkconstexpr(q)

	if ttisinteger[q.mode] then
		if ttsize[q.mode]=16 then
			GERROR("GETCONSTINT/128")
		fi
		return q.value
	elsif ttisreal[q.mode] then
		return q.xvalue
	else
		cpl strmode(q.mode)
		txerror("Getconstint: not int32/64")
	fi
	return 0
end

proc makenewconst(unit p,int64 x,int t=tvoid)=
!modify p (usually a binop, monop, convert op etc) to a new const unit
!p will usually already have the result mode
!the x value will do for int/word/real

	p.tag:=j_const
	p.a:=p.b:=nil
	p.hasa:=p.hasb:=0
	p.value:=x
	p.isconst:=1
	if t<>tvoid then
		p.mode:=t
	fi
end

proc tx_name(unit p,int t,lv)=
	ref strec d
	int oldmlineno
	unit pcode
	oldmlineno:=mlineno

	d:=p.def
	mlineno:=d.pos

	switch d.nameid
	when constid,enumid then			!note: currently, rxpass converts names to constants

		if lv then txerror("&const") fi

		tx_namedconst(d)
		pcode:=d.code

		p.tag:=j_const
		p.def:=nil
		p.a:=nil
	    p.c:=nil
		p.hasa:=p.hasc:=0

		if pcode.tag=j_convert then		!assume c_soft
			p.value:=pcode.a.value

		else
			p.value:=pcode.value
		fi

		p.slength:=pcode.slength
		p.mode:=d.mode
		p.isconst:=1
		p.isastring:=pcode.isastring

	when staticid,frameid,paramid then

	if d.islet and lv then
		println D.NAME,=LV,D.ISLET
		txerror("Can't use 'let' as lvalue")
	fi

		tx_namedef(d)

		if not inassem then
			p.mode:=d.mode
			twiden(p,lv)
		else
			p.mode:=trefchar
		fi

	when procid,dllprocid then

		p.mode:=trefproc	!use generic refproc mode (yields return type of actual proc mode
				!after a call op, or actual refproc in other context. Don't use actual
				!refproc here, to avoid generating thousands of ref proc modes, one
				!for each call, that will never be needed

	when labelid,blockid then
		p.mode:=treflabel

	when moduleid then
		txerror_s("Module name can't be used on it's own: #",d.name)

	when fieldid then
		p.tag:=j_const
		p.def:=nil
		p.a:=nil
	    p.c:=nil
		p.hasa:=p.hasc:=0

		p.value:=d.offset

		p.mode:=ti64
		p.isconst:=1


	when typeid then
		p.tag:=j_typeconst
		p.value:=d.mode
		p.mode:=ti64

	when dllvarid then
		if d.code then
			txerror("Can't init dllvar")
		fi
	p.mode:=d.mode

	else
		MLINENO:=P.POS
		CPL NAMENAMES[D.NAMEID]
		txerror_ss("TNAME? # #",namenames[d.nameid],d.name)
	endswitch
	mlineno:=oldmlineno

end

proc tx_bin(unit p,a,b)=
!deal with most binary ops
	int amode,bmode,abase,bbase,cmode, relop

	tpass(a)
	tpass(b)

	amode:=a.mode
	bmode:=b.mode

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]
	relop:=0

!deal with possible asymmetric ops

	case p.pclop
	when kadd then				!ref+ref not allowed; or ref+int (later refchar+refchar)
		if abase=tref and bbase=tref then
			if a.isastring and b.isastring then
				combinestrings(p)
				return
			fi

			txerror("ref+ref")
		fi
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=kaddrefoff
			p.mode:=amode
			return
		fi
	when ksub then				!ref-int or ref-ref
		if abase=tref and bbase=tref then
			if not comparemodes(amode, bmode) then
				txerror("ref-ref: not compat")
			fi
			p.pclop:=ksubref
			p.mode:=ti64
			return
		elsif abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=ksubrefoff
			p.mode:=amode
			return
		fi
	when kshl, kshr then
		coerceunit(b,ti64)
		p.mode:=amode
		return
	when keq, kne, klt, kle, kge, kgt then
		if abase=bbase=tref then
			p.mode:=ti64	
			return
		fi
		if p.pclop not in [keq, kne] then
			relop:=1
		fi

	when kin, knotin then
		if not isnum(abase) then txerror("IN lhs not int") fi
		case b.tag
		when j_makerange,j_makeset then
			p.tag:=(b.tag=j_makerange|j_inrange|j_inset)
			p.mode:=ti64
			if p.pclop=knotin then
				addnotl(p)
			fi
			return
		else
			txerror("IN ?")
		esac

	esac

	if isnum(abase) and isnum(bbase) then	!num op num

		if relop and ttsize[abase]<16 then

			if abase=tu64 and bbase<>tu64 then
				if b.tag=j_const and b.istrueconst and b.value.[63]=0 then
					bbase:=b.mode:=tu64
				fi
			elsif abase<>tu64 and bbase=tu64 then
				if a.tag=j_const and a.istrueconst and a.value.[63]=0 then
					abase:=a.mode:=tu64
				fi
			fi

			if abase=tu64 and bbase<>tu64 or abase<>tu64 and bbase=tu64 then
				txerror("Mixed sign")
			fi
		fi

		if typerank[abase]>=typerank[bbase] then
			cmode:=abase
		else
			cmode:=bbase
		fi

		if p.pclop=kdiv and ttisinteger[cmode] then
			p.pclop:=kidiv
		fi

		coerceunit(a,cmode)
		coerceunit(b,cmode)

	else
		if not comparemodes(amode,bmode) then
			txerror_ss("BIN: modes not compatible: # #",strmode(amode),strmode2(bmode))
		fi
		cmode:=amode
		p.pclmode:=getpclmode(getnewbase(abase))
	fi

	if intresult[p.pclop] or p.pclop=ksubrefoff then
		cmode:=ti64
	fi

	p.mode:=cmode
end

proc tx_binto(unit p,a,b)=
	int abase, bbase, amode,bmode, opc

	tpass(a,,needlv)
	tpass(b)

	amode:=a.mode
	bmode:=b.mode

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if p.pclop=kdivto and ttisinteger[abase] then
		p.pclop:=kidivto
	fi

	p.mode:=tvoid

	case p.pclop
	when kaddto then				!ref+ref not allowed; or ref+int (later refchar+refchar)
		if abase=tref and bbase=tref then
			txerror("to:ref+ref")
		fi
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=kaddrefoffto
			return
		fi
	when ksubto then				!ref-int
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=ksubrefoffto
			return
		fi
	when kshlto, kshrto then
		coerceunit(b,ti64)
		p.pclmode:=getpclmode(abase)
		return
	esac

	if isnum(abase) and isnum(bbase) then	!num op num
		p.pclmode:=getpclmode(abase)
		coerceunit(b,abase)

	elsif ttisshort[abase] and isnum(bbase) then
		p.pclmode:=getpclmode(abase)
		coerceunit(b,abase)

	else
		if not comparemodes(amode,bmode) then
			txerror_ss("BIN: modes not compatible: # #",strmode(amode),strmode(bmode))
		fi
		p.pclmode:=getpclmode(getnewbase(amode))
	fi
end

function getdominantmode(int amode,bmode)int=
	int abase,bbase

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if abase<=tlastnum and bbase<=tlastnum then	!num op num
		if typerank[abase]>=typerank[bbase] then
			return abase
		else
			return bbase
		fi

	else
		if not comparemodes(amode,bmode) then
			txerror("Getdom: no dominant mode")
		fi
		return amode
	fi
end

function getdominantmodepp(unit a,b)int=
	int amode:=a.mode, bmode:=b.mode
	int abase,bbase

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if abase<=tlastnum and bbase<=tlastnum then	!num op num
		if typerank[abase]>=typerank[bbase] then
			return abase
		else
			return bbase
		fi

	else
		if not comparemodes(amode,bmode) then
			txerror("Getdom: no dominant mode")
		fi
		return amode
	fi
end

proc tx_cmp(unit p,a,b)=
	int abase,bbase,atype,btype,u,v

	tx_bin(p,a,b)
end

proc tx_cmpchain(unit p,a)=
	int u,genop
	unit q,r

	q:=a
	while q do
		tpass(q,tany)

		if q=a then
			u:=q.mode
		else
			u:=getdominantmode(u,q.mode)
		fi

		q:=q.nextunit
	od

	q:=a
	r:=a.nextunit
	while q do
		coerceunit(q,u)
		q:=q.nextunit
	od

	for i:=1 to p.cmpgenop.len do
		genop:=p.cmpgenop[i]
		if genop=0 then exit fi

		p.cmppclmode[i]:=getpclmode(u)
	od

	p.mode:=ti64
end

proc tx_callproc (unit p,a,pargs,int t)=
!deal with both callproc and callfn (perhaps calldll too)
	unit q
	ref strec d,e,pm
	[maxparams]ref strec paramlist
	[maxparams]unit arglist,newarglist
	int nparams,i,j,k,nargs,m,kwdused,qm
	ichar name

	tpass(a)

	nargs:=nparams:=0

	retry::

	case a.tag
	when j_name then
		d:=a.def

		if d.nameid in [procid, dllprocid] then
	getparams::
			e:=d.deflist
			while e do
				if e.nameid=paramid then
					if nparams>=maxparams then txerror("Param overflow") fi
					paramlist[++nparams]:=e
				fi
				e:=e.nextdef
			od

		else					!assume fn ptr
			while ttbasetype[a.mode]=tref do
				insertunit(a,j_ptr)
				a.mode:=tttarget[a.mode]
			od
			goto dorefproc
		fi

	when j_if,j_select then
		TXERROR("Can't do ifx/function")

	else
	dorefproc::
		if a.tag=j_dot then
			tmethodcall(p,a,pargs)
			a:=p.a
			pargs:=p.b
			goto retry
		fi

		if ttbasetype[a.mode]<>tproc then
			txerror("Function pointer expected")
		fi

		d:=ttnamedef[a.mode]

		if d=nil then txerror("Function expected") fi
		goto getparams
	esac

	q:=pargs
	while q do
		if nargs>=maxparams then txerror("Param overflow") fi
		arglist[++nargs]:=q
		q:=q.nextunit
	od

	p.mode:=d.mode				!type returned by function (will be void for procs)

	if p.mode=tvoid and p.tag=j_callfn then
		p.tag:=j_callproc
	fi

	if p.mode and t<>tvoid then
		twiden(p,nolv)
	fi

	if d.varparams then
		for i to nargs do

			if i<=nparams then
				tpass(arglist[i],paramlist[i].mode)
			else
				tpass(arglist[i])
			fi
		od
		if t=tvoid then
			p.tag:=j_callproc
		fi
		return

	fi

!I have formal params in paramlist, and actual args in arglist
!Now create new set of args in arglist, which maps any keyword parameters,
!while missing args (represented as nullunit) replaced with nil

!Create new set of actual parameters in params(), with optional/default values filled in
!and any conversions applied
	k:=0
	kwdused:=0
	for i to nparams do
		newarglist[i]:=nil
	od

	for i to nargs do
		q:=arglist[i]
		switch q.tag
		when j_keyword then
			name:=q.a.def.name
			for j to nparams do
				if eqstring(paramlist[j].name,name) then
					exit
				fi
			else
				txerror_s("Can't find kwd param: #",name)
			od

			if newarglist[j] then
				txerror_s("Kwd: # already used or was implicit",name)
			fi
			newarglist[j]:=q.b
			kwdused:=1

		when j_null then			!missing param
			if kwdused then
				txerror("Normal param follows kwd")
			fi
			q:=nil
			goto doregparam
		else
	doregparam::
			if kwdused then
				txerror("Normal param follows kwd")
			fi
			if k>=nparams then
				cpl =k, =nparams
				txerror("Too many params supplied")
			fi
			newarglist[++k]:=q
		endswitch
	od

!scan params, and fill in optional/default params as needed

	for i to nparams do
		q:=newarglist[i]			!will be nil of not supplied
		pm:=paramlist[i]			!formal param (an st entry)
		if q=nil then
			unless pm.optional then
				txerror_s("Param not optional: #",strint(i))
			end
			if pm.code then		!provide default value
				newarglist[i]:=duplunit(pm.code,p.lineno)
			else
				newarglist[i]:=createconstunit(0,ti64)
			fi
		fi
	od

!final pass: do type-pass on each param, and apply any conversion
!I also need to build a new argument list for the call unit
	unit ulist:=nil, ulistx

	for i to nparams do
		pm:=paramlist[i]
		q:=newarglist[i]

		if pm.parammode=out_param then
			tpass(q,,needlv)
			m:=tttarget[pm.mode]
			qm:=q.mode

			if not comparemodes(qm,m) then
				cpl =strmode(qm)
				cpl =strmode(m)
				txerror("&param: type mismatch")
			fi

			insertunit(q,j_addrof)
			q.mode:=createrefmode(nil,qm)

		else
			tpass(q,pm.mode)
		fi

		if ulist=nil then
			ulist:=q
		else
			ulistx.nextunit:=q
		fi
		ulistx:=q
		q.nextunit:=nil
	od
	p.b:=ulist

	if t=tvoid then
		p.tag:=j_callproc
	fi

end

proc tx_unary(unit p,a)=
	int opc,size,amode,mbase,tmax,x,xhigh

	tpass(a)
	amode:=a.mode

	switch p.pclop
	when klwb, kupb, klen, kbounds then
		do_bounds(p,a)
		return
	when kbytesize,kbitwidth then
		size:=ttsize[(a.tag=j_typeconst|a.value|amode)]*(p.pclop=kbytesize|1|8)
		makenewconst(p,size)
		p.mode:=ti64
		return
	when kminvalue, kmaxvalue then
		tmax:=ti64
		if a.tag=j_typeconst then
			mbase:=ttbasetype[a.value]
		else
			mbase:=ttbasetype[getmemmode(a)]
		fi

		if p.pclop=kminvalue then
			case mbase
			when ti8 then x:=-128
			when ti16 then x:=-32768
			when ti32 then x:=-2_147_483_648
			when ti64 then x:=int64.minvalue
			when ti128 then
				xhigh:=0x8000'0000'0000'0000
				x:=0
				tmax:=ti128
			when tu128 then
				x:=xhigh:=0
			when tu8,tu16,tu32,tu64,tu128,tc8,tc16,tc64 then x:=0
			else
 	           txerror_s("Can't do minvalue on #",strmode(mbase))
			esac
		else
			case mbase
			when ti8 then x:=127
			when ti16 then x:=32767
			when ti32 then x:=2_147_483_647
			when ti64 then x:=0x7fff'ffff'ffff'ffff
			when ti128 then
				x:=0xFFFF'FFFF'FFFF'FFFF
				xhigh:=0x7FFF'FFFF'FFFF'FFFF
				tmax:=ti128
			when tu8,tc8 then x:=255
			when tu16,tc16 then x:=65535
			when tu32 then x:=4294967295
			when tu64 then x:=0; --x; tmax:=tu64
			when tu128 then
				x:=0xFFFF'FFFF'FFFF'FFFF
				xhigh:=0xFFFF'FFFF'FFFF'FFFF
				tmax:=tu128
			else
				txerror_s("Can't do maxvalue on #",strmode(mbase))
			esac
		fi
		p.tag:=j_const
		p.a:=nil; p.hasa:=0
		p.value:=x
	    p.high128:=xhigh
		p.mode:=tmax
		p.isconst:=1
		return
	when katan, kln, kexp then
		if ttisinteger[amode] then coerceunit(a,amode:=tr64) fi
	when ksin,kcos,ktan, kasin, kacos, ksqrt then
		coerceunit(a,amode:=tr64)
	when ktypestr then
		p.tag:=j_const
		if a.tag=j_typeconst then
			amode:=a.value
		else
			amode:=getmemmode(a)
		fi

		p.mode:=trefchar
		p.hasa:=0
		p.svalue:=pcm_copyheapstring(strmode(amode))
		p.isastring:=1
		p.length:=strlen(p.svalue)
		return
	endswitch

	p.pclmode:=getpclmode(getnewbase(amode))
	p.mode:=amode
end

proc tx_unaryto(unit p,a)=
	int abase, amode, opc

	tpass(a,,needlv)

	amode:=a.mode

	p.pclmode:=getpclmode(amode)
	p.mode:=tvoid
end

proc tx_if(unit p,a,b,c,int t,lv) =
	int u

	tcond(a)

!process both branches even if one will never be executed (for typechecking etc)
	tpass(b,t,lv)
	if t<>tvoid and not c then
		txerror("if needs else")
	fi
	tpass(c,t,lv)

	if t=tany then			!unknown types (eg. print)
		u:=getdominantmodepp(b,c)
		coerceunit(b,u)
		coerceunit(c,u)
		p.mode:=u
	else				!know exactly what type needed
		p.mode:=t
	fi

	if iscondtrue(a) then		!branch b only
		deleteunit(p,b)
	elsif iscondfalse(a) then	!branch c only
		if c=nil then
			c:=createunit0(j_block)
		fi
		deleteunit(p,c)
	fi

end

proc tx_longif(unit p,a,b,int t,lv) =
	unit q,r
	int u

	u:=tvoid

	q:=a
	while q do				!all elseif unots
		tcond(q.a)
		r:=q.b
		tpass(r,t,lv)

		if t=tany then
			if u=tvoid then
				u:=r.mode
			else
				u:=getdominantmode(u,r.mode)
			fi
		fi

		q:=q.nextunit
	od

	if t<>tvoid and b=nil then
		txerror("longif needs else")
	fi
	tpass(b,t,lv)

	if t=tany then
		u:=getdominantmode(u,b.mode)
	fi

	if t<>tvoid then
		q:=a
		while q do				!all elseif unots
			if t=tany then
				coerceunit(q.b,u)
			fi
			q.mode:=q.b.mode
			q:=q.nextunit
		od
		if t=tany then
			coerceunit(b,u)
		fi
		p.mode:=b.mode
	fi
end

proc tx_incrto(unit p,a,int t)=
	tpass(a,,needlv)

	if t<>tvoid then
		case p.pclop
		when kincr then p.pclop:=kincrload
		when kdecr then p.pclop:=kdecrload
		esac
		p.mode:=gettypebase(a.mode)
	else				!a++ a-- to ++a --a
		case p.pclop
		when kloadincr then p.pclop:=kincr
		when kloaddecr then p.pclop:=kdecr
		esac
		p.mode:=tvoid
	fi

	p.pclmode:=getpclmode(a.mode)

	twiden(p,0)
end

proc tx_for(unit pindex,pfrom,pbody)=
	unit pto, pstep, plocal, plist
	int u

	pto:=pfrom.nextunit
	pstep:=pto.nextunit

	tpass(pindex)
	if pindex.tag<>j_name then
		txerror("Loop index not a variable")
	fi
	u:=pindex.mode
	tpass(pindex.nextunit)

	tpass(pfrom,u)
	tpass(pto,u)
	tpass(pstep,u)

	tpass(pbody,tvoid)
	tpass(pbody.nextunit,tvoid)	!optional else
end

proc tx_forall(unit pindex,plist,pbody)=
	unit plocal,pfrom,pto,passign
	int u,mlist,elemtype

	plocal:=pindex.nextunit
	pfrom:=plocal.nextunit
	pto:=pfrom.nextunit
	passign:=plist.nextunit

	tpass(pindex,ti64)
	tpass(pfrom,ti64)
	tpass(pto,ti64)

	tpass(plist)
	mlist:=plist.mode

	case ttbasetype[mlist]
	when tarray then
		elemtype:=tttarget[mlist]
	when tslice then
		elemtype:=tttarget[mlist]
	else
		txerror("forall/can't iterate")
	esac

	tpass(plocal)
	if plocal.mode=tany then
		plocal.mode:=elemtype
		plocal.def.mode:=elemtype
	fi

	tpass(passign)

	tpass(pbody,tvoid)
	tpass(pbody.nextunit,tvoid)	!optional else
end

proc tx_index(unit p,a,b,int t,lv) =
!p is an index unit
!a is an array, b is an index
!t is the needed type for the element being indexed
	int amode,emode,pmode,tmode,tbasemode

	tpass(a,,lv)
	deref(a,t<>tvoid)
	amode:=a.mode

	tpass(b,ti64)			!index

	if ttbasetype[amode] not in [tarray, tslice] then
		txerror_s("Can't index: #",strmode(amode))
	fi
	p.mode:=tttarget[amode]
	twiden(p,lv)
end

proc tx_makerange(unit p,a,b)=
	int amode,bmode

	tpass(a)
	tpass(b)

	amode:=a.mode
	bmode:=b.mode

	if not ttisinteger[amode] or not ttisinteger[bmode] then
		txerror("range not int")
	fi

	if ttisint[amode] then
		coerceunit(a,ti64)
		coerceunit(b,ti64)
	else
		coerceunit(a,tu64)
		coerceunit(b,tu64)
	fi
	p.mode:=trange
end

proc tx_makeset(unit p,a, int t)=
	int x,y,isconst
	int64 lower,upper
	ref void pvoid

	if t=tvoid then
		txerror("open(var) set type")
	fi

	lower:=2 billion
	upper:=-2 billion

	isconst:=1

	while a do
		tpass(a)

		if not a.isconst then
			isconst:=0
		else
			case a.tag
			when j_makerange then
				lower min:=a.a.value
				upper max:=a.b.value
			when j_const then
				coerceunit(a,ti64)
				lower min:=y:=a.value
				upper max:=y:=a.value
			esac
		fi
		a:=a.nextunit
	od

	p.isconst:=isconst
end

proc tx_ptr(unit p,a,int t,lv)=
	ref strec d

	tpass(a)

	case ttbasetype[a.mode]
	when tvoid then
		txerror("Deref Void")
	when tref then
		p.mode:=tttarget[a.mode]

	when tslice then
		txerror("Can't deref slice")
	else
		txerror("PTR: need ref T")
	esac

	twiden(p,lv)
end

proc setrecordsize(int m)=
	[maxfields+8]ref strec fieldlist
	int i,nfields,indent,nrfields,size,index, maxalign
	ref strec d,e
	ref char flags
	const ss='S', ee='E'
	int flag

	if ttsize[m] then return fi

	d:=ttnamedef[m]
	e:=d.deflist
	nfields:=0

	fieldlist[++nfields]:=ref strec@(ss)

	while e do
		if e.nameid=fieldid then
			if nfields>=maxfields then
				gerror("srs:too many fields")
			fi

			setmodesize(e.mode)
			flags:=cast(&e.uflags)
			docase flags^
			when 'S', 'U' then
				flag:=flags^
				fieldlist[++nfields]:=ref strec@(flag)
				++flags
			else
				exit
			end docase

			fieldlist[++nfields]:=e

			do
				flag:=flags++^
				case flag
				when '*'  then
				when 'E' then
					fieldlist[++nfields]:=ref strec@(ee)
				else
					exit
				esac
			od
		fi

		e:=e.nextdef
	od

	fieldlist[++nfields]:=ref strec@(ee)
	fieldlist[nfields+1]:=nil			!terminator

	countedfields:=0
	index:=2
	maxalign:=1
	scanrecord('S',&fieldlist,index,size,0, d.align, maxalign)

	if d.align then
		size:=roundoffset(size,maxalign)
		d.maxalign:=maxalign
	else
		d.maxalign:=1
	fi

	ttsize[m]:=size
	ttlength[m]:=countedfields
	ttlower[m]:=1
end

proc scanrecord(int state,ref[]ref strec fields, int &index, &isize, offset, calign, &maxalign)=
 	ref strec e,f,ea
	int size:=0,fieldsize,bitoffset:=0, alignment, newoffset

	while f:=fields^[index++] do
		case int(f)
		when 'S','U' then
			scanrecord(int(f),fields, index,fieldsize, offset, calign, maxalign)
		when 'E' then			!end of this nested block
			if state='U' then ++countedfields fi
			isize:=size
			return
		else
			if f.mode=tbitfield then
				fieldsize:=0
				ea:=f.equivfield
				f.offset:=ea.offset
				f.bitoffset:=bitoffset
				bitoffset+:=f.bitfieldwidth
				if bitoffset>ttsize[f.equivfield.mode]*8 then
					txerror("Bit fields overflow type")
				fi

			elsif f.atvar then
				bitoffset:=0
				e:=f.equivfield
				fieldsize:=0
				ea:=resolve_equiv_name(f.owner,e)
				f.offset:=ea.offset
			else
				bitoffset:=0
				if state='S' then ++countedfields fi
				fieldsize:=ttsize[f.mode]
				if calign then
					alignment:=getalignment(f.mode)
					if alignment>maxalign then maxalign:=alignment fi
					newoffset:=roundoffset(offset,alignment)
					size+:=newoffset-offset
				else
					newoffset:=offset
				fi
				f.offset:=newoffset
				offset:=newoffset
			fi
		esac
		if state='S' then
			offset+:=fieldsize
			size+:=fieldsize
		else
			size:=max(size,fieldsize)
		fi
	od
end

function roundoffset(int offset, alignment)int=
	int mask

	if alignment=1 then return offset fi
	mask:=alignment-1
	while offset iand mask do ++offset od

	return offset
end

global function getalignment(int m)int=
!return alignment needed for type m, as 1,2,4,8
	int a

	case ttbasetype[m]
	when tarray then
		return getalignment(tttarget[m])
	when trecord then
		return ttnamedef[m].maxalign
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

proc tx_convert(unit p,a,int hard=0)=
	if a.tag=j_makelist then
		tx_makelist(a,a.a,p.convmode,nolv)
	else
		tpass(a)
		coerceunit(a,p.convmode,hard)
	fi
	deleteunit(p,a)			!get rid of this convert (may be replaced by new convert unit)
end

proc tx_makelist(unit p,a, int t,lv)=
	int alength,tlength,elemtype,newt, i, nfields,isconst, m
	unit q,b
	ref strec e

	alength:=p.length
	newt:=0
	isconst:=1

	tlength:=ttlength[t]

	if tlength then
		if alength<tlength then
			CPL =alength, =tlength
			txerror("Too few elements")
		elsif alength>tlength then
			CPL =alength, =tlength
			txerror("Too many elements")
		fi
	fi

	case ttbasetype[t]
	when tarray then
		elemtype:=tttarget[t]
		if tlength=0 then
			newt:=createarraymodek(nil, elemtype, ttlower[t],alength,0)
		else
			newt:=t
		fi
		q:=a
		while q do
			tpass(q,elemtype,lv)

			unless q.tag=j_const then isconst:=0 end
			q:=q.nextunit
		od

		p.mode:=newt

	when trecord then
		e:=ttnamedef[t].deflist
		q:=a
		while q and e do
			if e.nameid=fieldid then 
				while e.mode=tbitfield do
					e:=e.nextdef
					if not e then exit fi
				od

				tpass(q,e.mode,lv)
				unless q.tag=j_const then isconst:=0 end
				q:=q.nextunit
			fi

			e:=e.nextdef
		od
		while e and (e.nameid<>fieldid or e.mode=tbitfield) do
			e:=e.nextdef
		od
		if q or e then
			txerror("Can't initialise unions")
		fi
		p.mode:=t
	when tslice then
		if a=nil or (b:=a.nextunit; b=nil) or b.nextunit then
			txerror("bad slice init")
		fi
		p.b:=b
		p.hasb:=1
		a.nextunit:=nil
		tpass(a,,lv)
		if ttbasetype[a.mode]<>tref then txerror("slice init not ref") fi
		if tttarget[a.mode]<>tvoid then
			if not comparemodes(tttarget[a.mode],tttarget[t]) then
				txerror("slice/ptr mismatch")
			fi
		fi

		tpass(b,ti64)
		p.mode:=t
		p.tag:=j_makeslice
		p.resultflag:=1

	when tvoid then
		q:=a
		if p.makearray then
			if q=nil then txerror("array()?") fi
			tpass(q,,lv)
			m:=q.mode
			q:=q.nextunit
		else
			TXERROR("MAKELIST1")
		fi

		while q do
			tpass(q,m,lv)
			unless q.tag=j_const then isconst:=0 end
			q:=q.nextunit
		od

	else
		txerror_s("Unknown makelist type: #",strmode(t))
	esac

	p.isconst:=isconst

	tpass(p.b,ti64)

end

proc tx_dot(unit p,a,b,int lv)=
	int recmode,recbasemode,i,j,newtag,tmode
	unit q,pindex
	ref strec d,dequiv

	tpass(a)			!lhs, yeields ref array type

	recmode:=a.mode

	recbasemode:=ttbasetype[recmode]

	while recbasemode=tref do
		tmode:=tttarget[recmode]
		insertunit(a,j_ptr)
		recmode:=a.mode:=tmode
		recbasemode:=ttbasetype[recmode]
	od

	if ttbasetype[recmode]<>trecord then
		txerror("Bad record type")
	fi

	d:=b.def

	if d.nameid=nullid then			!not resolved; lhs mode wasn't available
		d:=b.def:=resolvefield(d,recmode)
	fi

	if d.mode=tbitfield then
		i:=d.bitoffset
		j:=i+d.bitfieldwidth-1
		dequiv:=d.equivfield
		b.def:=dequiv				!change from bitfield field to containing int
		b.mode:=dequiv.mode
		p.offset:=d.offset

		if i=j then					!single bit
			pindex:=createconstunit(i,ti64)
			newtag:=j_dotindex
		else						!bit slice
			pindex:=createunit2(j_makerange,createconstunit(i,ti64),createconstunit(j,ti64))
			pindex.mode:=trange
			pindex.a.resultflag:=1
			pindex.b.resultflag:=1
			newtag:=j_dotslice
		fi

		p.mode:=b.mode
		twiden(p,lv)
		insertunit(p,newtag)
		p.mode:=tu64
		p.b:=pindex
		p.hasb:=1
		p.a.resultflag:=1
		p.b.resultflag:=1
		p.resultflag:=1

		return

	fi

	b.mode:=d.mode
	p.mode:=d.mode

	p.offset:=d.offset
	twiden(p,lv)
end

function resolvefield(ref strec d, int m)ref strec=
	ref strec e,t

	case ttbasetype[m]
	when trecord then
	when tref then
		m:=tttarget[m]
		if ttbasetype[m]<>trecord then
			txerror("3:record expected")
		fi
	else
		txerror("4:record expected")
	esac
	t:=ttnamedef[m]

	e:=finddupl(t,d)
	if not e then
		txerror_s("Not a field: #",d.name)
	fi
	return e
end

proc tx_andl(unit p,a,b)=
	tpass(a)
	tpass(b)

	p.mode:=ti64

!assume both aren't const, as that would be evaluated
	if iscondfalse(a) or iscondfalse(b) then
		makenewconst(p,0,ti64)
	elsif iscondtrue(a) then
		deleteunit(p,b)
	elsif iscondtrue(b) then
		deleteunit(p,a)
	fi

end

proc convintconst(unit p,int64 x)=				!CONVINTCONST
!convert unit p into int const x
	p.tag:=j_const
	p.mode:=ti64
	p.a:=p.b:=p.c:=nil
	p.hasa:=p.hasb:=p.hasc:=0
	p.value:=x
	p.isconst:=1
end

proc tx_sliceptr(unit p,a)=
	int m,tmode

	tpass(a)
	m:=a.mode

	case ttbasetype[m]
	when tslice then
	else
		txerror_s("SLICEPTR #",strmode(m))
	esac

!for when ptr is to be pointer to the array
	tmode:=createarraymodek(nil, tttarget[m], ttlower[m],0,0)

!for when ptr is to be pointer to the array element (although either can be
!cast to the other); although try alternate .sliceptr versions too
!tmode:=tttarget[m]

	p.mode:=createrefmode(nil,tmode)
end

proc tx_swap(unit p,a,b)=
	tpass(a,,needlv)
	tpass(b,,needlv)

	if not comparemodes(a.mode,b.mode) then
		txerror("SWAP: type mismatch")
	fi

	p.mode:=tvoid
end

proc tx_select(unit p,a,b,c, int t,lv)=
	int i,u
	unit q

	tpass(a,ti64)

	q:=b
	while q do
		tpass(q,t,lv)
		if q=b then
			u:=q.mode
		else
			u:=getdominantmode(u,q.mode)
		fi

		q:=q.nextunit
	od

	tpass(c,t,lv)
	u:=getdominantmode(u,c.mode)

	q:=b
	while q do
		coerceunit(q,u)
		q:=q.nextunit
	od

	p.mode:=u
end

proc tx_case(unit p,a,b,c, int t,lv)=
	int amode,u
	unit wt,w

	if p.tag=j_docase and lv then gerror("&docase") fi

	tpass(a)

	if a=nil then
		amode:=tany
	else
		amode:=a.mode
	fi

	if ttisinteger[amode] and ttsize[amode]<8 then
		coerceunit(a,tint)
		amode:=tint
	fi
	u:=tvoid

	wt:=b
	while wt do				!whenthen chain
		w:=wt.a
		while w do				!each expr between when...then
			tpass(w)
			if w.tag=j_makerange then
				unless ttisinteger[amode] then txerror("case: need int index") end
			else
				if amode=tany then
						if not isboolunit(w) then
							TXERROR("CASE/BOOL?")
							insertunit(w,j_istruel)
						fi
				else
					coerceunit(w,amode)
				fi
			fi
			w:=w.nextunit
		od
		tpass(wt.b,t,lv)			!process block
		if t<>tvoid then
			if u then
				u:=getdominantmode(u,wt.b.mode)
			else
				u:=wt.b.mode
			fi
		fi
		wt:=wt.nextunit
	od

	if c then
		tpass(c,t,lv)
		if t=tany then
			u:=getdominantmode(u,c.mode)
		fi
	elsif t<>tvoid then
		txerror("case needs else")
	fi

	if t<>tvoid then
		p.mode:=u
	else
		p.mode:=tvoid
	fi

end

proc tx_notl(unit p,a)=
	tpass(a)
	p.mode:=ti64
end

proc tx_istruel(unit p,a)=
	int abase

	tpass(a)

	if isboolunit(a) then
		deleteunit(p,a)
		return
	fi

	abase:=ttbasetype[a.mode]
	if abase=tref then abase:=ti64 fi

	p.mode:=ti64
	p.pclmode:=getpclmode(abase)
end

proc tx_typepun(unit p,a)=
	int smode
	case a.tag
	when j_makelist then
		TXERROR("TYPEPUN/LIST")
	else
		tpass(a)

		smode:=getmemmode(a)

		if ttsize[smode]<ttsize[p.convmode] then
			txerror("Typepun: sizes must match")
		fi

		p.mode:=gettypebase(p.convmode)
	esac
end

proc tx_bytesize(unit p,a)=
	tpass(a)
	p.mode:=ti64
end

proc tx_exit(unit p,a)=
	if a=nil then return fi
	tpass(a,ti64)
	if a.tag<>j_const then
		txerror("exit/etc not const")
	fi
	p.index:=a.value
	p.a:=nil
	p.hasa:=0
end

proc tx_goto(unit p,a)=
	int m

	tpass(a)
	m:=a.mode

	if ttbasetype[m]<>tref or ttbasetype[tttarget[m]]<>tlabel then
		txerror("goto: not label")
	fi
end

proc tx_switch(unit p,a,b,c,int t,lv)=
	[0:2048]byte valueset
	unit wt, w
	int ax,bx,i,u

	if p.tag=j_doswitch and lv then gerror("&doswitch") fi

	tpass(a,ti64)

	memset(&valueset,0,valueset.bytes)
	u:=tvoid

	wt:=b
	while wt do

		w:=wt.a
		while w do
			tpass(w)

			if not isconstunit(w) then txerror("Switch not constant") fi

			case ttbasetype[w.mode]
			when trange then			!assume makerange
				ax:=w.a.value
				bx:=w.b.value
	dorange::
				for i:=ax to bx do
					if i<valueset.lwb or i>valueset.upb then
						txerror("switch: value out of range")
					fi
					if valueset[i] then
						cpl i
						txerror("Duplicate switch value")
					fi
					valueset[i]:=1
				od
			else
				coerceunit(w,ti64,0)
				tevaluate(w)
				if w.tag<>j_const then
					txerror("Switch value: not const int")
				fi
				ax:=bx:=w.value
				goto dorange
			esac
			w:=w.nextunit
		od
		tpass(wt.b,t,lv)

		if t=tany then
			if u then
				u:=getdominantmode(u,wt.b.mode)
			else
				u:=wt.b.mode
			fi
		fi

		wt:=wt.nextunit
	od

	if c then
		tpass(c,t,lv)
		if t=tany then
			u:=getdominantmode(u,c.mode)
		fi
	elsif t<>tvoid then
		txerror("switch needs else")
	fi

	if t<>tvoid then
		w:=b.a
		while w do				!all elseif unots
			if t=tany then
				coerceunit(b.b,u)
			fi
			w.mode:=b.b.mode
			w:=w.nextunit
		od
		if t=tany then
			coerceunit(c,u)
			p.mode:=u
		else
			p.mode:=t
		fi
	else
		p.mode:=tvoid
	fi
end

proc tx_addroffirst(unit p,a,int t)=
!&.x maps to &x[x.lwb]
	int m

	tpass(a)
	m:=a.mode
	if ttbasetype[m]<>tarray then
		txerror("&. ref[] expected")
	fi

	m:=createrefmode(nil,tttarget[m])
	if a.tag=j_name then
		a.addroffirst:=1
	fi
	p.mode:=m
end

proc tx_return(unit p,a, int t)=
 	int m,nvalues,nret,i
	ref[]int32 pmult
	unit q

	m:=currproc.mode
	nret:=currproc.nretvalues
	pmult:=ttmult[currproc.mode]

	if a=nil then
		if nret then
			txerror("return value(s) missing")
		fi
		return
	elsif nret=0 then
		txerror("Superfluous return value")
	fi

	if a.tag=j_makelist then
		a.tag:=j_returnmult
		if a.length<>nret then
			case ttbasetype[m]
			when trecord, tarray then
				txerror("return constructor not supported")
			else
				txerror("Wrong number of return values")
			esac
		fi
		q:=a.a				!point to list of return values
		for i to nret do
			tpass(q,pmult[i])
			q:=q.nextunit
		od

		deleteunit(p,a)			!don't need return
		if t=tvoid then
			p.mode:=tvoid
		else
			p.mode:=ttuple
		fi

	else
		if nret>1 then txerror("RETERROR?") fi
		tpass(a,m)

		if t=tvoid then					!regular out-of-line return
			p.mode:=tvoid
		else
			deleteunit(p,a)
!			P.MODE:=A.MODE
		fi
	fi

	IF TTISSHORT[P.MODE] THEN TXERROR("SHORT RET TYPE") FI
end

proc tx_dotindex(unit p,a,b,int lv) =
!a.[b], a is an int
	int pmode
	unit i,j

	tpass(a,,lv)			!lhs

	pmode:=tu64

	if not ttisinteger[a.mode] then
		txerror("a.[i]: not int/str value")
	fi

	tpass(b)			!index

	case ttbasetype[b.mode]
	when trange then
		i:=b.a
		j:=b.b
		if i.tag=j.tag=j_const then
			if i.value>j.value then
				swap(b.a,b.b)
			fi
		fi
	else					!assume simple index
		coerceunit(b,ti64)
	esac

	p.mode:=pmode
end

proc tx_slice(unit p,a,b) =
!a[b], b is a rtange

	tpass(a)			!lhs
	tpass(b)			!will be a range

	if a.mode=trefchar then
		p.mode:=createslicemodek(currproc,tc8,1,0)
	else
		deref(a)
		case ttbasetype[a.mode]
		when tarray then
			p.mode:=createslicemodek(currproc,tttarget[a.mode],1, 0)

		when tslice then
			p.mode:=a.mode

		else
			CPL =STRMODE(A.MODE)
			txerror("a[i..j]: not array")
		esac
	fi
end

proc tx_assign(unit p,a,b,int t)=
	int m,mm
	ref strec d

	case a.tag
	when j_makelist then
		tx_multassign(a,b)
	when j_dotindex, j_dotslice then
		tx_dotindex(a,a.a,a.b,needlv)
		tpass(b,a.mode)
		p.mode:=ti64
	else
		if a.tag=j_name and a.def.islet and p.initlet then
			tpass(a)
		else
			tpass(a,,needlv)
		fi
		m:=a.mode

		a.resultflag:=t<>tvoid

		if ttbasetype[m]=tslice and b.tag=j_makelist then
			tx_makelist(b,b.a,m,0)

		elsif ttisshort[m] and t<>tvoid then
			p.memmode:=m
			p.mode:=gettypebase(m)
			tpass(b,p.mode)

		else
			if b.pclop in [kidiv, kirem] then		!CAN'T JUST OVERRIDE MODE
				tpass(b)
			elsif b.tag=j_read then
				tpass(b,m)
			else
				mm:=m
				if ttisshort[m] then
					mm:=gettypebase(m)
				fi
				case b.tag
				when j_autocast then
					tpass(b,mm)
				when j_makelist then
					tpass(b,m)
				else
					tpass(b)
				esac
				if ttbasetype[b.mode]=ttuple then
					d:=getprocretmodes(b)
					coerceunit(a,ttmult[d.mode,1])
					p.mode:=a.mode
				else
					coerceunit(b,mm)
					p.mode:=mm
				fi
			fi
		fi
	esac
end

proc tx_multassign(unit a,b)=
!a is a multexpr; b might be multexpr, or a function with multiple returns
	unit p,q,lhs,rhs
	int nretmodes,i
	ref[]int32 pmult
	ref strec d				!point to def containing return mode info

	nretmodes:=0

	if b.tag<>j_makelist then

		tpass(b)
		d:=getprocretmodes(b)
		nretmodes:=d.nretvalues

		if ttbasetype[d.mode]<>ttuple then txerror("Not a tuple") fi

		if a.length>nretmodes then
			txerror("mult ass/mult returns don't agree in number")
		fi
		if nretmodes<=1 then
			txerror("mult ass rhs needs fn yielding 2+ values")
		fi

		p:=a.a
		pmult:=ttmult[d.mode]
		i:=1

		while p do
			tpass(p,,needlv)
			if p.mode<>pmult[i++] then
				txerror("mult ass/mult fn needs exact type match")
			fi
			p:=p.nextunit
		od
		return
	fi

	if a.length<>b.length then
		txerror("Mult assign: count mismatch")
	fi
	if a.length=0 then
		txerror("Invalid assignment")
	fi
	rhs:=b.a
	lhs:=a.a

	p:=lhs
	while p do
		tpass(p,,needlv)
		p:=p.nextunit
	od

	p:=lhs

	q:=rhs
	while q do
		tpass(q,p.mode)
		p:=p.nextunit
		q:=q.nextunit
	od
end

proc tx_exprlist(unit p,a,int t)=
	unit q

	q:=a
	while q and q.nextunit do
		tpass(q)
		q:=q.nextunit
	od

!q points to last expr
	tpass(q,t)
	p.mode:=q.mode
end

proc twiden(unit p, int lv)=
!intended for widening narrow types for memory access nodes Name, Index, Dot, Ptr.
!But will also be used to generally apply
	int m,u,mbase

	mbase:=ttbasetype[m:=p.mode]

	if mbase=tvoid then return fi		!nothing to widen (error?)
	if lv then return fi				!lv, keep memory mode as dest

	if not ttisshort[mbase] then return fi	!no widening needed
	case p.tag
	when j_name, j_ptr, j_index, j_dot then
			p.memmode:=m				!non-void marks this as non-lv too
			p.mode:=gettypebase(m)
	when j_callproc,j_callfn then
		p.memmode:=m
		p.mode:=gettypebase(m)
	else
		PRINTUNIT(P)
		txerror_s("widen? #",jtagnames[p.tag])
	esac
end

proc removeaddrof(unit p)=
!p is a lhs of dot operator used for flex/var
!will need to remove any addrof that has been applied
	if p=nil then return fi
	case p.tag
	when j_addrof then
		deleteunit(p,p.a)
	when j_if then
		removeaddrof(p.b)
		removeaddrof(p.c)
	else
		txerror("dot/flex: complex record expr, can't remove &")
	esac

end

proc tstringslice(unit p, int slicemode)=
!p is a string; insert conversions to turn it into a slice:
	unit a,b,prange
	int length

	if tttarget[slicemode]<>tc8 then
		txerror("Not char slice")
	fi
!
	a:=p
	insertunit(p,j_slice)


	if p.a.tag=j_const then
	else
		b:=duplunit(p.a)
		insertunit(b,j_unary)
		prange:=createunit2(j_makerange,createconstunit(1,ti64),b)

		prange.mode:=trange
		p.b:=prange
		p.hasb:=1
	fi

	p.mode:=slicemode
end

proc tx_bitfield(unit p,a,int lv)=
	int i,j,bitsize,topbit
	unit r

	tpass(a,,lv)

	if not ttisinteger[a.mode] and not ttisref[a.mode] then
		txerror("Int/ref needed")
	fi

	bitsize:=ttsize[ttbasetype[a.mode]]*8
	topbit:=bitsize-1

	case p.bfcode
	when bf_lsb then
		i:=0; j:=7

	when bf_msb then
		j:=topbit
		i:=topbit-7

	when bf_lsbit then
		i:=j:=0

	when bf_odd,bf_even then
		if lv then
			txerror("Can't assign")
		fi
		i:=j:=0

	when bf_msbit then
		i:=j:=topbit

	when bf_lsw then
		i:=0
		j:=bitsize/2-1

	when bf_msw then
		i:=bitsize/2
		j:=topbit
	else
		CPL P.BFCODE
		TXERROR("BITFIELD")
	esac

	if i=j then			!single bit
		p.tag:=j_dotindex
		p.b:=createconstunit(i,ti64)
		p.hasb:=1
		p.resultflag:=1
		p.b.resultflag:=1

		if p.bitopindex=bf_even then
			p.mode:=tu64
			addnotl(p)
		fi

	else
		r:=createunit2(j_makerange,createconstunit(i,ti64),createconstunit(j,ti64))
		r.a.resultflag:=1
		r.b.resultflag:=1
		r.mode:=trange
		p.tag:=j_dotslice
		p.hasb:=1
		p.b:=r
	fi

	p.mode:=tu64
end

proc deref(unit a, int needres=1)=
!a is a unit that needs to be dereferenced because it's about to used as:
! a[i]
! a[i..j]
! a.lwb, a.upb, a.len
!Ie in an array context
	int abasemode, tmode

	abasemode:=ttbasetype[a.mode]

	while abasemode=tref do
		tmode:=tttarget[a.mode]

		insertunit(a,j_ptr)
		a.mode:=tmode

		abasemode:=ttbasetype[a.mode]
	od

end

proc tmethodcall(unit p, pdot, pargs)=
	int mrec
	unit prec, pfield, pfunc
	ref strec d,e

	prec:=pdot.a
	pfield:=pdot.b
	mrec:=prec.mode
	d:=pfield.def

	e:=resolvefield(d,mrec)

	if e=nil then
		txerror_s("Can't resolve method:",d.name)
	fi

	pfunc:=createname(e)
	pfunc.mode:=e.mode
	prec.nextunit:=pargs

	p.a:=pfunc
	p.b:=prec
end

function softconvert(int s,t,hard=0)int=
!see what implicits are needed t convert s to t
!return value will be:
!	kerror		No implicit conversions exist
!	op_softconv		No conversion needed (eg. i64 to u64, or ref void to ref T)
!	op_widen_...	Etc, an actual op index

	int sbase, tbase

	if s=t then return ksoftconv fi

	sbase:=ttbasetype[s]
	tbase:=ttbasetype[t]

	if sbase<=tlastnum and tbase<=tlastnum then		!both numeric
		return softconvtable[sbase,tbase]
	fi

	if sbase=tbase=tref then
		if s=tref or t=tref then						!at least one is ref void
			return ksoftconv
		fi												!ref T/ref U
		return (comparemodes(s,t)|ksoftconv|kerror)
	fi

	if sbase=tarray and t=trefchar and tttarget[s]=tc8 then
		return kcharaxtoichar
	fi

	if not hard and sbase<=tlastnum and ttisshort[tbase] then
		return ksofttruncshort		!soft truncate needed for idata
	fi

!a few other coercions to be added later, such as refchar to slice/string
	if sbase=tbase=tarray then
		if comparemodes(s,t) then
			return ksoftconv
		fi
	fi

	if sbase=tarray and tbase=tslice then
		if not comparemodes(tttarget[s],tttarget[t]) then
			txerror("Bad array to slice")
		fi
		return karraytoslice
	fi
	if s=trefchar and tbase=tslice then
		if tttarget[t] not in [tu8,tc8] then
			txerror("Bad string to slice")
		fi

		return kichartoslice
	fi
	if sbase=tslice and tbase=tslice then
		if comparemodes(s,t) then
			return ksoftconv
		fi
	fi

	return kerror
end

function comparemodes(int s,t)int=
!return 1 if modes s,t are compatible. That is, ref s/ref t would be interchangeable.
!a direct compare may be false because refs/arrays but be constructed at
!different times
	int sbase, tbase, starg, ttarg
	ref strec d,e

	if s=t then return 1 fi

	sbase:=ttbasetype[s]
	tbase:=ttbasetype[t]

	case sbase
	when tref then
		starg:=tttarget[s]
		ttarg:=tttarget[t]
		if starg=tvoid or ttarg=tvoid then
			return 1
		fi

		return comparemodes(tttarget[s],tttarget[t])

	when tarray then
		if comparemodes(tttarget[s],tttarget[t]) and (ttlength[s]=ttlength[t] or
			ttlength[s]=0 or ttlength[t]=0) then
			return 1
		fi
	when tslice then
		return comparemodes(tttarget[s],tttarget[t])

	when tproc then
		d:=ttnamedef[s]
		e:=ttnamedef[t]
		if d and e then
			if not comparemodes(d^.mode,e^.mode) then return 0 fi
			if d^.paramlist=nil and e^.paramlist=nil then return 1 fi
		fi
	elsif sbase=tc8 and tbase=tu8 or sbase=tu8 and tbase=tc8 then
		return 1
	else
!else needs complex param/result-matching
!...
	esac
	return 0
end

function hardconvert(int s,t)int=
!an explicit conversion from s to t has been requested
!see if it is possible, and return an approrpiate conversion
	int opc, sbase, tbase
	int sint, tint, sref, tref

	opc:=softconvert(s,t,1)				!implicit conversion anyway?
	if opc<>kerror then				!yes
		return opc
	fi

	if s=tvoid then						!assume t<>tvoid, as that is tested above
		txerror("Non-void type expected")
	fi

	sbase:=ttbasetype[s]
	tbase:=ttbasetype[t]
	sref:=ttisref[s]
	tref:=ttisref[t]
	sint:=ttisinteger[s]
	tint:=ttisinteger[t]

	if sref and tint or sint and tref or sref and tref then
		return ksoftconv
	elsif sbase=tenum and tint or sint and tbase=tenum then
		return ksoftconv
	elsif sint and ttisshort[tbase] then
		return (ttsize[sbase]=16|ksofttruncw|ktruncate)
	fi

	return kerror
end

proc applyconversion(unit p, int s,t, opc)=
	int cmpop
!
	case opc
	when kerror then
		txerror("No conversion possible")
	when ksoftconv then
		p.mode:=t
		return
	when ksofttruncshort then
		insertunit(p,j_shorten)
		p.mode:=t			!don't use the short target mode
		return

	when karraytoslice then
		insertunit(p,j_slice)
		p.mode:=t
		return
	when kichartoslice then
		tstringslice(p,t)
		return

	when kcharaxtoichar then
		insertunit(p,j_addroffirst)
		p.mode:=trefchar
		return

	esac

	if tevalconvert(p,s,t,opc) then		!try and apply it directly
		return
	fi

!have to add an explict conversion node
	insertunit(p, j_convert)
	p.pclop:=opc

	p.convmode:=s
	p.resultflag:=1

	if ttisshort[t] then
		p.convmode:=t
		t:=gettypebase(t)
	fi

	p.mode:=t
end

proc coerceunit(unit p, int t, hard=0)=
	int opc,s

	s:=p.mode

	if t=tvoid or s=t then return fi
	if s=tvoid and t<>tvoid then
		txerror("Void type in expression/return value missing")
	fi

	if hard then
		opc:=hardconvert(s,t)
	else
		opc:=softconvert(s,t)
	fi

	if opc=kerror then
		println strmode(s),"=>",strmode(t)
		if not hard and hardconvert(s,t)<>kerror then
			txerror("Need explicit conversion")
		else
			txerror("Can't do conversion")
		fi
	fi

	applyconversion(p,s,t,opc)
end

function tevalconvert(unit p,int s,t,opc)int=
!conversion op opc to convert from s to t is about to be applied to be
!try and do that at compile time to avoid adding a runtime conversion
!return 1 if it could apply it, 0 if it couldn't
!caller should have already evaluated p to reduce constants etc
	real x,z
	int a,c
	int128 aa
!
	if p.tag<>j_const then
		return 0
	fi

	case pr(s,    t)
	when pr(ti64, tr64) then
		z:=p.value

	when pr(tr64, ti64) then
		c:=p.xvalue

	when pr(tr64, tr32) then
		z:=real32(p.xvalue)
	else
		return 0
	esac

	if ttisreal[t] then
		makenewconst(p,int64@(z),t)

	else
		makenewconst(p,c,t)
	fi

	return 1
end

proc do_bounds(unit p,a) =
	int m,mbase,opc,lower,upper

	deref(a)

	m:=a.mode
	if a.tag=j_typeconst then m:=a.value fi

	mbase:=ttbasetype[m]
	p.mode:=ti64

	case p.pclop
	when klwb then
		case mbase
		when tarray,tslice then
			convintconst(p,ttlower[m])
			return
		else
error::
			txerror("lwb/upb/len?")
		esac

	when kupb then
		case mbase
		when tarray then
			convintconst(p,ttlower[m]+ttlength[m]-1)
		when tslice then
			p.pclop:=kupb
		else
			goto error
		esac

	when klen then
		case mbase
		when tarray then
			convintconst(p,ttlength[m])
		when tslice then
			p.pclop:=klen
		else
			goto error
		esac
	when kbounds then
		p.mode:=trange
		case mbase
		when tarray then
			p.range_lower:=ttlower[m]
			p.range_upper:=p.range_lower+ttlength[m]-1
			p.tag:=j_const
			p.a:=p.b:=p.c:=nil
			p.hasa:=p.hasb:=p.hasc:=0
			p.isconst:=1
			return

		when tslice then
			p.pclop:=kbounds
		when ti32 then
			convintconst(p,int32.max-int32.min+1)
			return
		else
			goto error
		esac
	esac
end

proc addnotl(unit p)=
	insertunit(p,j_notl)
	p.mode:=ti64
	p.pclop:=knotl
end

proc tevaluate(unit p)=
	unit a,b,pname
	int offset

	int tag:=p.tag

	if jisexpr[tag]=2 then
		tevalbinop(p)

	elsif jisexpr[tag]=1 then
		tevalmonop(p)

	elsecase tag
	when j_makerange then
		a:=p.a
		b:=p.b
		if ttsize[a.mode]<=8 then			!const range only for 32-bits
			tevaluate(a)
			tevaluate(b)
			if a.tag=j_const and b.tag=j_const then
				p.isconst:=a.isconst iand b.isconst
			fi
		fi

	when j_addrof then
		a:=p.a

		pname:=addrdotindex(a, offset)

		if pname then
			deleteunit(a,pname)
			if p.b=nil then
				p.hasb:=1
				p.b:=createconstunit(offset,ti64)
			else 
				p.b.value+:=offset
			fi
		fi
	fi

end

function addrdotindex(unit p, int &offset)unit q=
	int axmode

	case p.tag
	when j_dot then
		if p.a.tag=j_name then
			offset:=p.offset
			return p.a
		else
			q:=addrdotindex(p.a,offset)
			offset+:=p.offset
			return q
		fi
	when j_index then
		axmode:=p.a.mode
		if p.b.tag=j_const then
			if p.a.tag=j_name then
				offset:=(p.b.value-ttlower[axmode])*ttsize[tttarget[axmode]]
				return p.a
			else
				q:=addrdotindex(p.a,offset)
				if q then
					offset+:=(p.b.value-ttlower[axmode])*ttsize[tttarget[axmode]]
				fi
				return q
			fi
		else
			return nil
		fi
	else
		return nil
	esac

end

proc tevalbinop(unit p)=
	int64 a,b,c,offset
	real x,y,z
	unit lhs, rhs

	lhs:=p.a
	rhs:=p.b

	unless lhs.tag=rhs.tag=j_const then
		if lhs.tag=j_addrof and rhs.tag=j_const then
			if lhs.a.tag=j_name then			!reduce addrof(a)+k => addrof(a,k)
				offset:=rhs.value*ttsize[tttarget[lhs.mode]]
				if lhs.b=nil then
					lhs.hasb:=1
					lhs.b:=createconstunit(offset,ti64)
				else
					lhs.b.value+:=offset
				fi
				deleteunit(p,lhs)
			fi
		fi
		return
	end

	if ttisreal[p.mode] then
		x:=p.a.xvalue
		y:=p.b.xvalue
	else
		a:=p.a.value
		b:=p.b.value
	fi

	case p.mode
	when ti64, tu64 then

		switch p.pclop
		when kadd then c:=a+b

		when ksub then c:=a-b

		when kmul then c:=a*b

		when kidiv then c:=a/b

		when kshl then c:=a<<b

		when keq then c:=a=b

		when kne then c:=a<>b

		when klt then c:=a<b

		when kle then c:=a<=b

		when kge then c:=a>=b

		when kgt then c:=a>b

		when kandl then c:=a and b

		when korl then c:=a or b

		when kiand then c:=a iand b
		when kior then c:=a ior b
		else
			return
		end

	when tr64,tr32 then

		switch p.pclop
		when kadd then z:=x+y

		when ksub then z:=x-y

		when kmul then z:=x*y
		when kdiv then z:=x/y

		else
			return
		end
	else
		return
	esac
!
	if ttisreal[p.mode] then
		makenewconst(p,int64@(z))
	else
		makenewconst(p,c)
	fi
end

proc tevalmonop(unit p)=
	int64 a,b,c
	real x,z
	ref int128 q

	unless p.a.tag=j_const then
		return
	end

	a:=p.a.value
	x:=p.a.xvalue

	case p.mode
	when ti64, tu64 then

		switch p.pclop
		when kneg then c:=-a

		when kistruel then c:=istrue a

		when knotl then c:=not a
		when kinot then c:=inot a
		when kabs then c:=abs a

		else
			return
		end switch
	when tr64, tr32 then
		switch p.pclop
		when kneg then z:=-x
		when katan then z:=atan(x)

		else
			return
		end switch
	else
		return
	esac

	if ttisreal[p.mode] then
		makenewconst(p,int64@(z))
	else
		makenewconst(p,c)
	fi
end

function getnewbase(int m)int=
	m:=ttbasetype[m]
	case m
	when tarray, trecord then
		return tblock
	esac
	return m
end

function iscondtrue(unit p)int =
	if p.tag=j_const and p.value<>0 then 1 else 0 fi
end

function iscondfalse(unit p)int =
	if p.tag=j_const and p.value=0 then 1 else 0 fi
end

proc fixchararray(unit a)=
!turn []char into ichar at certain points
	if a and ttbasetype[a.mode]=tarray and tttarget[a.mode]=tc8 then
		coerceunit(a,trefchar,0)
	fi
end

proc combinestrings(unit p)=
!p is (add, a, b) where a and b are string constants.
	unit a:=p.a, b:=p.b
	int alen:=a.length
	int blen:=b.length
	int clen:=alen+blen
	ichar s

	if blen=0 then
		deleteunit(p,a)
		return
	elsif alen=0 then
		deleteunit(p,b)
		return
	fi

	s:=pcm_alloc(clen+1)
	memcpy(s,a.svalue,alen)
	memcpy(s+alen,b.svalue,blen)
	(s+clen)^:=0

	deleteunit(p,a)
	p.length:=clen
	p.svalue:=s

end

proc tx_strinclude(unit p,a)=
	int fileno

	tpass(a)
	if a.tag<>j_const or not a.isastring then
		txerror("strincl/not string")
	fi
	fileno:=getsupportfile(a.svalue)
	a.svalue:=sourcefiletext[fileno]
	a.slength:=sourcefilesizes[fileno]
!
	deleteunit(p,a)
end
