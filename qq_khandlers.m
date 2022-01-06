!HLL bytecode handlers

global macro getopnda = (pcptr+1)^
global macro getopndb = (pcptr+2)^
global macro getopndc = (pcptr+3)^
global macro getopndd = (pcptr+4)^

!step pcptr to next bytecode, skipping n operands
global macro skip(n) = pcptr:=pcptr+(n+1)


var ref int paramdefretloc
var int insiderecorddef

!const doretcheck=1
const doretcheck=0

global var [0..pclnames.upb]ref proc khandlertable

global proc initkhandlers=
	ichar name
	static int handlersdone=0

	if handlersdone then return fi

	for i to $get_nprocs() do
		name:=$get_procname(i)
		if eqbytes(name,"k_",2) then
			for k:=0 to pclnames.upb do
				if eqstring(name+2,pclnames[k]+1) then		!skip "k_" and "k"
					khandlertable[k]:=$get_procaddr(i)
					exit
				fi
			else
				pcerror_s("Unknown khandler",name)
			od
		fi
	od

	for i in khandlertable.bounds when khandlertable[i]=nil do
		khandlertable[i]:=cast(kunimpl)
	od

	handlersdone:=1
end

proc kunimpl=
	if hasbytecodes then
		pcerror_s("Unimplemented:",pclnames[pcptr^])
	else
		pcerror("Unimplemented (use -fdebug to see opcode)")
	fi
end

global proc k_pushci=
	++sptr
	sptr.tagx:=tint
	sptr.value:=getopnda
	skip(1)
end

global proc k_pushcu=
	++sptr
	sptr.tagx:=tword
	sptr.value:=getopnda
	skip(1)
end

global proc k_pushnil=
	++sptr
	sptr.tagx:=trefpack
	sptr.uref.elemtag:=tpvoid
	sptr.uref.ptr:=nil
	++pcptr
end

global proc k_pushcs=
	++sptr

	sptr.tagx:=tstring ior hasrefmask
	sptr.objptr:=cast(getopnda)
	++sptr.objptr.refcount
	skip(1)
end

global proc k_pushcr=
	++sptr
	sptr.tagx:=treal
	sptr.xvalue:=real@(getopnda)
	skip(1)
end

global proc k_stop=
	stopped:=1
end

global proc k_stoprunproc=
	stopped:=1
end

global proc k_pushm=
	++sptr
	sptr^:=variant(getopnda)^
	var_share(sptr)

	skip(1)
end

global proc k_pushf=
	++sptr
	sptr^:=variant(frameptr+getopnda)^
	var_share(sptr)

	skip(1)
end

global proc k_pushmref=
	++sptr
	sptr.tagx:=trefvar
	sptr.varptr:=variant(getopnda)

	skip(1)
end

global proc k_pushfref=
	++sptr
	sptr.tagx:=trefvar
	sptr.varptr:=variant(frameptr+getopnda)

	skip(1)
end

global proc k_popm=
	variant p

	p:=variant(getopnda)
	var_unshare(p)
	p^:=sptr^				!transfer reference
	--sptr

	skip(1)
end

global proc k_storem=
	variant p

	p:=variant(getopnda)
	var_share(sptr)
	var_unshare(p)
	p^:=sptr^				!transfer reference

	skip(1)
end

global proc k_zpopm=
	(variant(getopnda))^:=sptr^				!transfer reference

	--sptr
	skip(1)
end

global proc k_popf=
	variant p

	p:=variant(frameptr+getopnda)
	var_unshare(p)
	p^:=sptr^				!transfer reference
	--sptr

	skip(1)
end

global proc k_storef=
	variant p

	p:=variant(frameptr+getopnda)
	var_share(sptr)
	var_unshare(p)
	p^:=sptr^				!transfer reference

	skip(1)
end

global proc k_zpopf=
	variant p

	p:=variant(frameptr+getopnda)
	p^:=sptr^				!transfer reference
	--sptr

	skip(1)
end

global proc k_popretval=
	variant p

	p:=variant(frameptr+getopnda)
	p^:=sptr^				!transfer reference
	--sptr

	skip(1)
end

global proc k_tom=
	variant p

	p:=cast(getopndb)

	--p.value

	if p.value then
		pcptr:=cast(getopnda)
	else
		skip(2)
	fi
end

global proc k_tof=
	variant p

	p:=cast(frameptr+getopndb)

	--p.value

	if p.value then
		pcptr:=cast(getopnda)
	else
		skip(2)
	fi
end

global proc k_add=
	variant y:=sptr--
	varrec x:=sptr^

	var_add(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_sub=
	variant y:=sptr--
	varrec x:=sptr^

	var_sub(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_mul=
	variant y:=sptr--
	varrec x:=sptr^

	var_mul(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_div=
	variant y:=sptr--
	varrec x:=sptr^

	var_div(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_idiv=
	variant y:=sptr--
	varrec x:=sptr^

	var_idiv(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_irem=
	variant y:=sptr--
	varrec x:=sptr^

	var_irem(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_iand=
	variant y:=sptr--
	varrec x:=sptr^

	var_iand(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_ior=
	variant y:=sptr--
	varrec x:=sptr^

	var_ior(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_ixor=
	variant y:=sptr--
	varrec x:=sptr^

	var_ixor(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_shl=
	variant y:=sptr--
	varrec x:=sptr^

	var_shl(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_shr=
	variant y:=sptr--
	varrec x:=sptr^

	var_shr(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_sqr=
	case sptr.tag
	when tint then
		sptr.value:=sqr(sptr.value)
	when treal then
		sptr.xvalue:=sqr(sptr.xvalue)
	else
		pcustype("Sqr",sptr)
	esac

	++pcptr
end

global proc k_sign=
	case sptr.tag
	when tint then
		sptr.value:=(sptr.value<0|-1|(sptr.value>0|1|0))
	when treal then
		sptr.tag:=tint
		sptr.value:=(sptr.xvalue<0|-1|(sptr.xvalue>0|1|0))
!	when tdecimal then
	else
		pcustype("Sign",sptr)
	esac

	++pcptr
end

global proc k_sqrt=	{domaths(ksqrt)}
global proc k_sin=	{domaths(ksin)}
global proc k_cos=	{domaths(kcos)}
global proc k_tan=	{domaths(ktan)}
global proc k_asin=	{domaths(kasin)}
global proc k_acos=	{domaths(kacos)}
global proc k_atan=	{domaths(katan)}
global proc k_ln=	{domaths(kln)}
global proc k_log=	{domaths(klog)}
global proc k_lg=	{domaths(klg)}
global proc k_exp=	{domaths(kexp)}
global proc k_round=	{domaths(kround)}
global proc k_floor=	{domaths(kfloor)}
global proc k_ceil=		{domaths(kceil)}
global proc k_fract=	{domaths(kfract)}

global proc k_neg=
	varrec x:=sptr^

	var_neg(sptr)
	var_unshare(&x)

	++pcptr
end

global proc k_negto=
	variant px:=sptr--

	if not var_negto(px) then
		var_inplace_unary(px, cast(var_neg))
	end

	++pcptr
end

global proc k_absto=
	variant px:=sptr--

	if not var_absto(px) then
		var_inplace_unary(px, cast(var_abs))
	end

	++pcptr
end

global proc k_inotto=
	variant px:=sptr--

	if not var_inotto(px) then
		var_inplace_unary(px, cast(var_inot))
	end

	++pcptr
end

!global proc k_notlto=
!	variant px:=sptr--
!
!	if not var_notlto(px) then
!		var_inplace_unary(px, cast(var_notl))
!	end
!
!	++pcptr
!end

global proc k_atan2=
	pcerror("ATAN2 NOT READY")
end

global proc k_fmod=
	pcerror("FMOD NOT READY")
end


global proc k_abs=
	varrec x:=sptr^

	var_abs(sptr)
	var_unshare(&x)

	++pcptr
end

global proc k_inot=
	varrec x:=sptr^

	var_inot(sptr)
	var_unshare(&x)

	++pcptr
end

global proc k_istruel=
	int res

	res:=var_istruel(sptr)
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res

	++pcptr
end

global proc k_notl=
	int res

	res:=not var_istruel(sptr)
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res

	++pcptr
end

global proc k_jumpeq=
	variant y:=sptr--
	variant x:=sptr--

	if var_equal(x,y) then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)
end

global proc k_jumpne=
	variant x,y

	y:=sptr--
	x:=sptr--

	if not var_equal(x,y) then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)
end

global proc k_jumplt=
	variant x,y

	y:=sptr
	x:=sptr-1

	sptr-:=2

	if var_compare(x,y)<0 then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)
end

global proc k_jumple=
	variant x,y

	y:=sptr--
	x:=sptr--

	if var_compare(x,y)<=0 then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)
end

global proc k_jumpge=
	variant x,y

	y:=sptr
	x:=sptr-1

	sptr-:=2
	if var_compare(x,y)>=0 then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)

end

global proc k_jumpgt=
	variant x,y

	y:=sptr--
	x:=sptr--

	if var_compare(x,y)>0 then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)
end

global proc k_jumpfalse=
	variant x:=sptr--

	if not var_istruel(x) then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)

end

global proc k_jumptrue=
	variant x:=sptr--

	if var_istruel(x) then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
end

global proc k_incrtom=
	variant p
!
	p:=variant(getopnda)
	case p.tag
	when tint then
		++p.value
  	when trefvar then
		++p.varptr
	when trefpack then
		p.uref.ptr+:=ttsize[p.uref.elemtag]
	when treal then
		p.xvalue+:=1

	else
		pcustype("incrtom",p)
	end
	skip(1)
end

global proc k_incrtof=
	variant p

	p:=variant(frameptr+getopnda)
	case p.tag
	when tint,tword then
		++p.value
  	when trefvar then
		++p.varptr
	when trefpack then
		p.uref.ptr+:=ttsize[p.uref.elemtag]
	when treal then
		p.xvalue+:=1
	else
		pcustype("incrtof",p)
	esac
	skip(1)
end

global proc k_decrtom=
	variant p

	p:=variant(getopnda)
	case p.tag
	when tint,tword then
		--p.value
  	when trefvar then
		--p.varptr
	when trefpack then
		p.value-:=ttsize[p.uref.elemtag]
	when treal then
		p.xvalue-:=1
	else
		pcustype("decrtom",p)
	esac
	skip(1)
end

global proc k_decrtof=
	variant p

	p:=variant(frameptr+getopnda)
	case p.tag
	when tint,tword then
		--p.value
	when treal then
		p.xvalue-:=1
  	when trefvar then
		--p.varptr
	when trefpack then
		p.uref.ptr-:=ttsize[p.uref.elemtag]
  	else
		pcustype("decrtof",p)
	esac
	skip(1)
end

global proc k_incrload=
	varrec v

	v:=sptr^
	k_incrptr()
	var_loadptr(&v,++sptr)
end

global proc k_loadincr=
	varrec v

	v:=sptr^
	var_loadptr(sptr,sptr)
	++sptr
	sptr^:=v

	k_incrptr()
end

global proc k_decrload=
	varrec v

	v:=sptr^
	k_decrptr()
	var_loadptr(&v,++sptr)
end

global proc k_loaddecr=
	varrec v

	v:=sptr^
	var_loadptr(sptr,sptr)
	++sptr
	sptr^:=v

	k_decrptr()
end

global proc k_incrptr=
	variant p

	p:=sptr--

	switch p.tag
	when trefvar then			!increment what ptr points to
		p:=p.varptr
		switch p.tag
		when tint,tword then
			++p.value
		when trefvar then			!incr the pointer
			++p.varptr
		when trefpack then			!incr the pointer
			p.uref.ptr+:=ttsize[p.uref.elemtag]
		when treal then
			p.xvalue+:=1
		else
			pcustype("incrptr/refvar",p)
		endswitch
	when trefpack then			!incr the packed type pointed to
		switch p.uref.elemtag
		when tpu8,tpi8 then
			++(p.uref.ptr)^
		when tpu16,tpi16 then
			++(p.uref.ptr)^
		else
			pcustype_t("incrptr/ref",p.uref.elemtag)
		endswitch

	else
		pcustype("incrptr",p)
	endswitch
	++pcptr
end

global proc k_decrptr=
	variant p

	p:=sptr--

	switch p.tag
	when trefvar then			!increment what ptr points to
		p:=p.varptr
		switch p.tag
		when tint,tword then
			--p.value
		when trefvar then			!incr the pointer
			--p.varptr
		when trefpack then			!incr the pointer
			p.uref.ptr-:=ttsize[p.uref.elemtag]
		when treal then
			p.xvalue-:=1
		else
			pcustype("incrptr/refvar",p)
		endswitch
	when trefpack then			!incr the packed type pointed to
		switch p.uref.elemtag
		when tpu8,tpi8 then
			--(p.uref.ptr)^
		when tpu16,tpi16 then
			--(p.uref.ptr)^
		else
			pcustype_t("incrptr/ref",p.uref.elemtag)
		endswitch

	else
		pcustype("incrptr",p)
	endswitch
	++pcptr
end

global proc k_pushvoid=
	++sptr
	sptr.tagx:=tvoid
	++pcptr
end

global proc k_callproc=
	const countinterval=100
	static int count=countinterval

	if --count=0 then
		count:=countinterval
		os_peek()
	fi

	if sptr>=stacklimit then
		pcerror("Stack Overflow")
	fi

	++sptr
	sptr.tagx:=treturn
	sptr.uret.retaddr := pcptr+3

	sptr.uret.frameptr_low := word32@(frameptr)
	frameptr:=cast(sptr)

	pcptr:=cast(getopnda)

end

global proc k_callptr=
	symbol d

	if sptr.tag<>tsymbol then
!		pcustype("Probably undefined function:",sptr)
		pcerror("Probably undefined function")
!		pcustype("callptr not symbol",sptr)
	fi
	d:=sptr.def

!CPL "CALLPTR",D.NAME,NAMENAMES[D.NAMEID]

!check. no. of params
	if d.nparams<>getopnda then
		pcerror_s("Callptr: wrong # params; need:",strint(d.nparams))
	fi

	sptr.tagx:=treturn
	sptr.uret.retaddr := pcptr+3

	sptr.uret.frameptr_low := word32@(frameptr)
	frameptr:=cast(sptr)

	pcptr:=cast(d.pcaddress)
end

global proc k_procentry =
	to getopnda do
		++sptr
		sptr.tagx:=tvoid
	od
	skip(1)
end

global proc k_return=
	int nargs

	if doretcheck then
		if sptr.tag<>treturn then
			pcerror_s("Not treturn:",ttname[sptr.tag])
		fi
	fi
	nargs:=getopnda

	pcptr:=sptr.uret.retaddr

	(ref int32(&frameptr))^:=sptr.uret.frameptr_low

	--sptr

	to nargs do
		var_unshare(sptr)
		--sptr
	od
end

global proc k_return0=
	int nargs

	if doretcheck then
		if sptr.tag<>treturn then
			pcerror_s("Not treturn:",ttname[sptr.tag])
		fi
	fi

	pcptr:=sptr.uret.retaddr

	(ref int32(&frameptr))^:=sptr.uret.frameptr_low

	--sptr
end

global proc k_unshare=
	to getopnda do
		var_unshare(sptr)
		--sptr
	od
	skip(1)
end

global proc k_unshare1=
	var_unshare(sptr)
	--sptr
	++pcptr
end

global proc k_formci=
	variant p

	p:=cast(getopndb)

	++p.value

	if p.value<=getopndc then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

global proc k_forfci=
	variant p

	p:=cast(frameptr+getopndb)

	++p.value

	if p.value<=getopndc then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

global proc k_fordmci=
	variant p

	p:=cast(getopndb)

	--p.value

	if p.value>=getopndc then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

global proc k_fordfci=
	variant p

	p:=cast(frameptr+getopndb)

	--p.value

	if p.value>=getopndc then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

global proc k_formm=
	variant p,q

	p:=cast(getopndb)
	q:=cast(getopndc)

	++p.value

	if p.value<=q.value then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

global proc k_fordmm=
	variant p,q

	p:=cast(getopndb)
	q:=cast(getopndc)

	--p.value

	if p.value>=q.value then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

global proc k_forff=
	variant p,q

	p:=cast(frameptr+getopndb)
	q:=cast(frameptr+getopndc)

	++p.value

	if p.value<=q.value then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

global proc k_fordff=
	variant p,q

	p:=cast(frameptr+getopndb)
	q:=cast(frameptr+getopndc)

	--p.value

	if p.value>=q.value then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

global proc k_comment=
	skip(1)
end

global proc k_makelist=
	variant x,y
	int n

	n:=getopnda

	x:=sptr-n+1			!start of data
	sptr:=x

	var_make_list(x,sptr,n,getopndb)
	sptr.objptr.mutable:=0

	skip(2)
end

global proc k_makedict=
	variant x
	int n

	n:=getopnda

	x:=sptr-n*2+1			!start of data

	var_make_dict(x,x,n)
	sptr:=x

	skip(1)
end

global proc k_makeset=
	variant x
	int n

	n:=getopnda

	x:=sptr-n+1			!start of data

	var_make_set(x,x,n)
	sptr:=x
	sptr.objptr.mutable:=0

	skip(1)
end

global proc k_makerecord=
	variant x,y
	int n

	n:=getopnda

	x:=sptr-n+1				!start of data

	var_make_record(x,x,n,getopndb)
	sptr:=x
	sptr.objptr.mutable:=0

	skip(2)
end

global proc k_makestruct=
	variant x,y
	int n

	n:=getopnda

	x:=sptr-n+1				!start of data

	var_make_struct(x,x,n,getopndb)
	sptr:=x
	sptr.objptr.mutable:=0

	skip(2)
end

global proc k_makearray=
	variant x
	int n

	n:=getopndb

	x:=sptr-n+1				!start of data

	var_make_array(x,x,getopnda, n, getopndc, getopndd)
	sptr:=x
	sptr.objptr.mutable:=0

	skip(4)
end

global proc k_makebits=
	variant x
	int n

	n:=getopndb

	x:=sptr-n+1				!start of data

	var_make_bits(x,x,getopnda, n, getopndc, getopndd)
	sptr:=x
	sptr.objptr.mutable:=0

	skip(4)
end

global proc k_index=
!x[y]
	variant y,z
	varrec x

	y:=sptr--
	x:=sptr^

	case y.tag
	when tint then
		var_getix(sptr,y.value)
	when trange then
		var_getslice(sptr,y.range_lower,y.range_upper)
	else
		pcmxtypes("Index",&x,y)
	esac

	var_unshare(&x)

	++pcptr
end

global proc k_popindex=
!y[z]:=x
	variant x,y,z

	z:=sptr--		!index
	y:=sptr--		!list etc
	x:=sptr--		!value to store

	case z.tag
	when tint then
		var_putix(y, z.value, x)
		var_unshare(y)
	when trange then
		var_putslice(y, z.range_lower, z.range_upper, x)
		var_unshare(x)
		var_unshare(y)
	else
		pcmxtypes("Popindex",y,z)
	esac


	++pcptr
end

global proc k_indexref=
!&x[y]
	variant y,p
	varrec x

	y:=sptr--
	x:=sptr^

	case y.tag
	when tint then
		var_getixref(sptr, y.value)
	else
		pcmxtypes("Indexref",sptr,y)
	esac

	var_unshare(&x)
	++pcptr
end

global proc k_keyindex=
!x{y}
	variant d,k,p,def

	def:=sptr--			!def is any default value to be used
	k:=sptr--			!k is the key
	d:=sptr				!d is the dict

	if d^.tag<>tdict then
		pcustype("dict{}",d)
	fi

	p:=var_finddictitem(d,k,0)
	var_unshare(d)
	var_unshare(k)

	if p then			!found
		sptr^:=p^
		var_unshare(def)
	else
		sptr^:=def^			!use given default value when not found
	fi
	++pcptr
end

global proc k_popkeyindex=
!y[z]:=x
	variant d,k,p,x

	k:=sptr--			!k is the key
	d:=sptr--			!d is the dict
	x:=sptr--			!value to pop

	if d.tag<>tdict then
		pcustype("dict{}:=",d)
	fi

	p:=var_finddictitem(d,k,1)

	if p.tag<>tvoid then
		var_unshare(p)
	fi
	p^:=x^

	var_unshare(d)
	var_unshare(k)

	++pcptr
end

global proc k_keyindexref=
!y[z]:=x
	variant d,k,p,x

	k:=sptr--			!k is the key
	d:=sptr				!d is the dict

	if d.tag<>tdict then
		pcustype("&dict{}",d)
	fi

	p:=var_finddictitem(d,k,0)
	if p=nil then
		pcerror("&dict{} not found")
	fi
	var_share(p)
	var_unshare(k)
	var_unshare(d)

	sptr.tagx:=trefvar
	sptr.varptr:=p

	++pcptr
end

global proc k_dot=
	symbol d
	variant p
	ref byte q
	int rectype
	varrec v

	case sptr.tag
	when trecord, tstruct then
	else
		pcerror("dot/not record")
	esac
	rectype:=sptr.usertag

	d:=resolvefield(getopnda, rectype)

	case d.nameid
	when fieldid then
		p:=sptr.objptr.urec.varptr+d.fieldoffset/varsize
		var_share(p)
		var_unshare(sptr)
		sptr^:=p^

	when structfieldid then
		var_loadpacked(sptr.objptr.ustruct.ptr+d.fieldoffset, d.mode, &v, nil)
		var_unshare(sptr)
		sptr^:=v

    when procid then
		sptr.tagx:=tsymbol
		sptr.def:=d

    when linkid then
		sptr.tagx:=tsymbol
		sptr.def:=d.alias

	else
		pcerror_s("DOT: can't do this fieldtype:",namenames[d.nameid])
	esac

	skip(1)
end

global proc k_dotref=
	symbol d
	variant p
	ref byte q
	int rectype

	rectype:=sptr.usertag

	d:=resolvefield(getopnda, rectype)

	case d.nameid
	when fieldid then
		p:=sptr.objptr.urec.varptr+d.fieldoffset/varsize
!Possible bug when sptr is a transient value which is now freed
!But you wouldn't normally use as an lvalue
		var_unshare(sptr)

		sptr.tagx:=trefvar
		sptr.varptr:=P

	when structfieldid then
		q:=sptr.objptr.ustruct.ptr+d.fieldoffset
		var_unshare(sptr)
		sptr.tagx:=trefpack
		sptr.uref.ptr:=q
		sptr.uref.elemtag:=d.mode

	else
		pcerror_s("DOTREF: can't do this fieldtype:",namenames[d.nameid])
	esac

	skip(1)
end

global proc k_popdot=
	symbol d
	variant p,x,y

	x:=sptr--
	y:=sptr--

	case x.tag
	when trecord, tstruct then
	else
		pcerror("popdot/not record")
	esac

	d:=resolvefield(getopnda, x.usertag)

	IF NOT X.HASREF THEN PCERROR("POPDOT") FI

	if not x.objptr.mutable then
		pcerror("Not mutable")
	fi

	case d.nameid
	when fieldid then
		p:=x.objptr.urec.varptr+d.fieldoffset/varsize
		var_unshare(p)
		p^:=y^				!transfer
		var_unshare(x)

	when structfieldid then
!		var_loadpacked(sptr.objptr.ustruct.ptr+d.fieldoffset, d.mode, &v, nil)
		var_storepacked(x.objptr.ustruct.ptr+d.fieldoffset, y, d.mode)
		var_unshare(x)

	else
		pcerror_s("POPDOT: can't do this fieldtype:",namenames[d.nameid])
	esac

	skip(1)
end

global proc k_dotindex=
!x.[y]
	variant y,z
	varrec x


	y:=sptr--
	x:=sptr^

	case y.tag
	when tint then
		var_getdotix(sptr,y.value)
	when trange then
		var_getdotslice(sptr,y.range_lower,y.range_upper)
	else
		pcmxtypes("Dotindex",&x,y)
	esac

	var_unshare(&x)

	++pcptr
end

global proc k_dotindexref=
!x.[y]
	variant y,p
	varrec x

	y:=sptr--
	x:=sptr^

	case y.tag
	when tint then
		var_getdotixref(sptr, y.value)
	when trange then
		var_getdotsliceref(sptr, y.range_lower,y.range_upper)
	else
		pcmxtypes("Dotindexref",sptr,y)
	esac

	var_unshare(&x)
	++pcptr
end

global proc k_popdotindex=
!y[z]:=x
	variant x,y,z,py

	z:=sptr--		!index
	y:=sptr--		!ref to int, string etc
	x:=sptr--		!value to store

	case z.tag
	when tint then
		var_putdotix(y, z.value, x)
		var_unshare(y)
	when trange then
		var_putdotslice(y, z.range_lower, z.range_upper, x)
		var_unshare(x)
		var_unshare(y)
	else
		pcmxtypes("Popdotindex",y,z)
	esac


	++pcptr
end

global proc k_len=
	variant x:=sptr
	object p:=x.objptr
	int n

	case x.tag
	when tlist,trecord,tarray,tdict,tbits then
		n:=p.ulist.length
	when tstring then
		n:=p.ustr.length
	when trecord, tcarray, tstruct then
		n:=ttlength[x.usertag]
	when tset then
		n:=p.uset.length
	when trange then
		n:=x.range_upper-x.range_lower+1
	when tdecimal then
		n:=obj_len_dec(p)
	else
		pcustype("Len",x)
	esac

	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

global proc k_upb=
	variant x:=sptr
	object p:=x.objptr
	int n

	case x.tag
	when tlist,tdict then
		n:=p.ulist.length+p.ulist.lower-1
	when tstring then
		n:=p.ustr.length
	when tarray then
		n:=p.uarray.length+p.uarray.lower-1
	when tbits then
		n:=p.ubits.length+p.ubits.lower-1
	when trecord, tstruct then
		n:=ttlength[x.usertag]

	when tcarray then
		n:=ttlength[x.usertag]+ttlower[x.usertag]-1

	when tset then
		n:=p.uset.length-1
	when trange then
		n:=x.range_upper

	else
		pcustype("Upb",x)
	esac

	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

global proc k_lwb=
	variant x:=sptr
	object p:=x.objptr
	int n

	case x.tag
	when tlist then
		n:=p.ulist.lower
	when tstring,tdict then
		n:=1
	when tarray then
		n:=p.uarray.lower
	when tbits then
		n:=p.ubits.lower
	when trecord,tstruct then
		n:=1
	when tcarray then
		n:=ttlower[x.usertag]
	when tset then
		n:=0
	when trange then
		n:=x.range_lower
	else
		pcustype("Lwb",x)
	esac

	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

global proc k_bounds=
	int a,b,m
	object p

	m:=sptr.tag
	p:=sptr.objptr

	case m
	when tlist,tdict then
		a:=p.ulist.lower
		b:=p.ulist.length+a-1
	when tarray then
		a:=p.uarray.lower
		b:=p.uarray.length+a-1
	when tbits then
		a:=p.ubits.lower
		b:=p.ubits.length+a-1
	when tstring then
		a:=1
		b:=p.ustr.length
	when trange then
		a:=sptr.range_lower
		b:=sptr.range_upper
	when tstruct,trecord then
		a:=1
		b:=ttlength[sptr.usertag]
	when tcarray then
		a:=ttlower[sptr.usertag]
		b:=ttlength[sptr.usertag]

	when tset then
		a:=0
		b:=p.uset.length-1

	else
		pcustype("Bounds",sptr)
	esac

	var_unshare(sptr)
	sptr.tagx:=trange
	sptr.range_lower:=a
	sptr.range_upper:=b

	++pcptr
end

global proc k_boundsx=
	int a,b,m
	object p

	m:=sptr.tag
	p:=sptr.objptr

	case m
	when tlist,tdict then
		a:=p.ulist.lower
		b:=p.ulist.length+a-1
	when tarray then
		a:=p.uarray.lower
		b:=p.uarray.length+a-1

	when tbits then
		a:=p.ubits.lower
		b:=p.ubits.length+a-1

	when tstring then
		a:=1
		b:=p.ustr.length
	when trange then
		a:=sptr.range_lower
		b:=sptr.range_upper
	when tstruct,trecord then
		a:=1
		b:=ttlength[sptr.usertag]
	when tcarray then
		a:=ttlower[sptr.usertag]
		b:=ttlength[sptr.usertag]
	when tset then
		a:=0
		b:=p.uset.length-1

	else
		pcustype("Boundsx",sptr)
	esac

	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=a

	++sptr
	sptr.tagx:=tint
	sptr.value:=b

	++pcptr
end

global proc k_dictitems=
	int n

	case sptr.tag
	when tdict then
		n:=sptr.objptr.udict.dictitems
	when tdecimal then
		n:=sptr.objptr.udec.length
	else
		pcustype("Dictitems/digits",sptr)
	esac
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

global proc k_append=
	variant y:=sptr--
	varrec x:=sptr^

	var_append(sptr,y)
	var_unshare(&x)

	++pcptr
end

global proc k_concat=
	variant y:=sptr--
	varrec x:=sptr^

	var_concat(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

global proc k_appendto=
!x append:= y
	variant px,x,y

	y:=sptr--
	px:=sptr--

	case px.tag
	when trefvar then
		var_appendto(px.varptr,y)
	else
		pcustype("Appendto",px)
	esac
	++pcptr
end

global proc k_concatto=
!x append:= y
	variant px,x,y

	y:=sptr--
	px:=sptr--

	case px.tag
	when trefvar then
		var_concatto(px.varptr,y)
		var_unshare(y)
	else
		pcustype("Concatto",px)
	esac
	++pcptr
end

global proc k_addto=
!x +:= y
	variant y:=sptr--
	variant px:=sptr--

	if not var_addto(px, y) then
		var_inplace(px,y, cast(var_add), cast(var_addmixed))
	end

	var_unshare(y)
	++pcptr
end

global proc k_subto=
!x -:= y
	variant y:=sptr--
	variant px:=sptr--

	if not var_subto(px, y) then
		var_inplace(px,y, cast(var_sub), cast(var_submixed))
	end

	var_unshare(y)
	++pcptr
end

global proc k_multo=
!x *:= y
	variant y:=sptr--
	variant px:=sptr--

	if not var_multo(px, y) then
		var_inplace(px,y, cast(var_mul), cast(var_mulmixed))
	end

	var_unshare(y)
	++pcptr
end

global proc k_divto=
!x /:= y
	variant y:=sptr--
	variant px:=sptr--

	if not var_divto(px, y) then
		var_inplace(px,y, cast(var_div), cast(var_divmixed))
	end

	var_unshare(y)
	++pcptr
end

global proc k_idivto=
!px^ %:= y
	variant y:=sptr--
	variant px:=sptr--

	if not var_idivto(px, y) then
		var_inplace(px,y, cast(var_idiv), cast(var_idivmixed))
	end

	var_unshare(y)
	++pcptr
end

global proc k_iandto=
!px^ iand:= y
	variant y:=sptr--
	variant px:=sptr--

	if not var_iandto(px, y) then
		var_inplace(px,y, cast(var_iand), cast(var_iandmixed))
	end

	var_unshare(y)
	++pcptr
end

global proc k_iorto=
!px^ ior:= y
	variant y:=sptr--
	variant px:=sptr--

	if not var_iorto(px, y) then
		var_inplace(px,y, cast(var_ior), cast(var_iormixed))
	end

	var_unshare(y)
	++pcptr
end

global proc k_ixorto=
!px^ ixor:= y
	variant y:=sptr--
	variant px:=sptr--

	if not var_ixorto(px, y) then
		var_inplace(px,y, cast(var_ixor), cast(var_ixormixed))
	end

	var_unshare(y)
	++pcptr
end

global proc k_shlto=
!x <<:= y
	variant y:=sptr--
	variant px:=sptr--

	if not var_shlto(px, y) then
		var_inplace(px,y, cast(var_shl), cast(var_shlmixed))
	end

	var_unshare(y)
	++pcptr
end

global proc k_shrto=
!x >>:= y
	variant y:=sptr--
	variant px:=sptr--

	if not var_shrto(px, y) then
		var_inplace(px,y, cast(var_shr), cast(var_shrmixed))
	end

	var_unshare(y)
	++pcptr
end

global proc k_copy=
	varrec x

	if sptr.hasref then
		x:=sptr^
		var_duplu(sptr)
		var_unshareu(&x)
	fi

	++pcptr
end

global proc k_dupl=
	++sptr
	sptr^:=(sptr-1)^
	var_share(sptr)
	++pcptr
end

global proc k_makerange=
	variant x,y

	y:=sptr--
	x:=sptr

	if x.tag<>tint or y.tag<>tint then
		pcerror("makerange/not int")
	fi

	sptr.tagx:=trange
	sptr.range_upper:=y.value
	sptr.range_lower:=x.value

	++pcptr
end

global proc k_makerangelen=
	variant x,y

	y:=sptr--
	x:=sptr

	if x.tag<>tint or y.tag<>tint then
		pcerror("makerangelen/not int")
	fi

	sptr.tagx:=trange
	sptr.range_upper:=x.value+y.value-1
	sptr.range_lower:=x.value

	++pcptr
end

global proc k_makedecimal=
	varrec x
	object p

	x:=sptr^

	if x.tag<>tstring then pcerror("Not str") fi
	p:=x.objptr
	if p.ustr.length=0 then pcerror("Null str") fi

	var_make_dec_str(p.ustr.strptr, p.ustr.length, sptr)

	var_unshare(&x)

	++pcptr
end

function resolvefield(int index, rectype)symbol d=
!index is a start point in the genfieldtable
!scan the linked list looking for a field/structfield/method etc whose
!owner type matches rectype
	ref genfieldrec g

	if index=0 then pcerror("Not a field") fi

	g:=genfieldtable[index]

	while g do
		d:=g.def
!CPL =D.NAME,NAMENAMES[D.NAMEID], D.OWNER.NAME,TTNAME[RECTYPE]

		if d.owner.mode=rectype then return d fi
		g:=g.nextdef
	od

	pcerror_s("Can't resolve field:",d.name)
	return nil
end

global proc k_pushptr=
	variant p

	p:=sptr

	case p.tag
	when trefvar then
		sptr^:=p.varptr^

	when trefpack then
		var_loadpacked(p.uref.ptr,p.uref.elemtag, sptr, nil)

	when trefbit then
		var_loadbit(p.uref.ptr, p.uref.bitoffset, p.uref.elemtag, p.uref.bitlength, sptr)

!	when tsymbol then

	else
		pcustype("Pushptr",p)
	esac

	var_share(sptr)

	++pcptr	
end

global proc k_popptr=
	variant p,x,y

	p:=sptr--
	x:=sptr--

	case p.tag
	when trefvar then
		var_unshare(p.varptr)
		p.varptr^:=x^
	when trefpack then
		var_storepacked(p.uref.ptr,x,p.uref.elemtag)
	when trefbit then
		var_storebit(p.uref.ptr, p.uref.bitoffset, x, p.uref.elemtag, p.uref.bitlength)

	else
		pcustype("Popptr",p)
	esac

	++pcptr	
end
!
global proc k_islist=
	istype(tlist)
end

global proc k_isarray=
	istype(tarray)
end

global proc k_isstring=
	istype(tstring)
end

global proc k_isrecord=
	istype(trecord)
end

global proc k_swap=
	[1024]byte tempbuffer
	variant x,y
	varrec v
	int xt,yt,s,t,n
	ref byte p,q
	int a

	x:=sptr--
	y:=sptr--

	if x.tag<>y.tag then
		pcerror("Swap mismatch")
	fi

	case x.tag
	when trefvar then
		v:=x.varptr^
		x.varptr^:=y.varptr^
		y.varptr^:=v

!		swap(x.varptr^, y.varptr^)		!block swaps not implemented!
	when trefpack then
		s:=x.uref.elemtag
		t:=y.uref.elemtag
		if s<>t then goto swaperror fi
		n:=ttsize[s]
		case n
		when 1 then
			p:=x.uref.ptr
			q:=y.uref.ptr
			a:=p^
			p^:=q^
			q^:=a
		elsif ttsize[s]<=tempbuffer.bytes then
			memcpy(&tempbuffer,x.uref.ptr,n)
			memcpy(x.uref.ptr,y.uref.ptr,n)
			memcpy(y.uref.ptr,&tempbuffer,n)
		else
			goto swaperror
		esac

	else
swaperror::
		pcmxtypes("Swap",x,y)
	esac

	++pcptr
end

global proc k_jumptesteq=
!jump to L when x=y
! x<>y: keep x on the stack, skip
! x=y:  pop both jump
	variant x,y
	int xt,yt,res

	y:=sptr--
	x:=sptr
	xt:=x.tag
	yt:=y.tag

	if xt<>yt then
		case pr(xt,yt)
		when pr(tint,trange) then
			if x.value not in y.range_lower..y.range_upper then
				skip(1)
				return
			fi
		when pr(tint,tset) then
			if not var_in_set(x,y) then
				skip(1)
				return
			fi
		esac
		var_unshare(x)
		var_unshare(y)
		--sptr
		pcptr:=cast(getopnda)
		return
	fi

	res:=var_equal(x,y)
	var_unshare(y)
	if res then
		var_unshare(x)
		--sptr
		pcptr:=cast(getopnda)
		return
	fi

	skip(1)
end

global proc k_jumptestne=
!jump to L when x=y
! x<>y: keep x on the stack, skip
! x=y:  pop both jump
	variant x,y
	int xt,yt,res

	y:=sptr--
	x:=sptr
	xt:=x.tag
	yt:=y.tag

	if xt<>yt then
		case pr(xt,yt)
		when pr(tint,trange) then
			if x.value in y.range_lower..y.range_upper then
				--sptr
				skip(1)
				return
			fi
		when pr(tint,tset) then
			if var_in_set(x,y) then
				--sptr
				skip(1)
				return
			fi
		esac

		var_unshare(y)
!		--sptr
		pcptr:=cast(getopnda)
		return

	fi

	res:=var_equal(x,y)
	var_unshare(y)
	if not res then
		pcptr:=cast(getopnda)
		return
	fi
	var_unshare(x)
	--sptr

	skip(1)
end

global proc k_jump=
	pcptr:=cast(getopnda)
end

global proc k_jumpptr=
	symbol d

	if sptr.tag<>tsymbol then
		pcerror("symbol expected")
	fi
	d:=cast(sptr.def)
	++sptr
	if d.nameid<>labelid then
		pcerror("label expected")
	fi
	if not d.procfixed then
		d.pcaddress:=moduletable[d.moduleno].pcstart+d.labelno
		d.procfixed:=1
	fi

	pcptr:=d.pcaddress
end

global proc k_incr=
	case sptr.tag
	when tint then
		++sptr.value
	else
		pcustype("incr",sptr)
	esac

	++pcptr
end

global proc k_decr=
	case sptr.tag
	when tint then
		--sptr.value
	else
		pcustype("decr",sptr)
	esac

	++pcptr
end

global proc k_chr=
	var [8]char str

	if sptr.tag=tint then
		var_makechar(sptr.value,sptr)
!!		if sptr.uvalue>=128 then pcerror("chr:not ASCII") fi
!		if sptr.uvalue>255 then pcerror("chr:not ASCII") fi
!		str[1]:=sptr.value
!		str[2]:=0
!		var_make_stringn(&.str,1,sptr,1)
	else
		pcustype("CHR",sptr)
	fi
	++pcptr
end

global proc k_asc=
	int c

	if sptr.tag=tstring then
		if sptr.objptr.ustr.length then
			c:=sptr^.objptr.ustr.strptr^
		else
			c:=0
		fi
		var_unshareu(sptr)
		sptr.tagx:=tint
		sptr.value:=c
	else
		pcustype("ASC",sptr)
	fi

	++pcptr
end

global proc k_pusht=
	++sptr
	sptr.tagx:=ttype
	sptr.value:=getopnda
	skip(1)
end

global proc k_type=
	int t:=sptr.tag
	case t
	when trecord, tstruct, tcarray then
		t:=sptr.usertag
	esac

	var_unshare(sptr)
	sptr.tagx:=ttype
	sptr.value:=t
	++pcptr
end

global proc k_basetype=
	int t:=sptr.tag
	var_unshare(sptr)
	sptr.tagx:=ttype
	sptr.value:=t
	++pcptr
end

global proc k_elemtype=
	int t:=sptr.tag

	case t
	when tarray,tbits,tcarray then
		t:=sptr.objptr.uarray.elemtype
	when trefpack, trefvar, trefbit then
		t:=sptr.uref.elemtag
	when tset then
		t:=tpu1
	else
		pcustype_t("elemtype",t)
	esac

	var_unshare(sptr)
	sptr.tagx:=ttype
	sptr.value:=t
	++pcptr
end

global proc k_nop={++pcptr}

global proc k_modulecall=
	symbol d:=cast(getopnda)
	int moduleno:=d.moduleno

	++sptr
	sptr.tagx:=treturn
	sptr.uret.retaddr := pcptr+2

	pcptr:=moduletable[moduleno].pcstart
end

global proc k_modulereturn=
	pcptr:=sptr.uret.retaddr
	--sptr
end

global proc k_maxvalue=
	int64 a

	if sptr.tag=ttype then sptr.tag:=sptr.value fi

	case sptr.tag
	when tpu8 then a:=255
	when tpu16 then a:=65536
	when tpu32 then a:=0xFFFF'FFFF
	when tpu64,tword then a:=0xFFFF'FFFF'FFFF'FFFF
	when tpi8 then a:=127
	when tpi16 then a:=32767
	when tpi32 then a:=0x7FFF'FFFF
	when tpi64,tint then a:=0x7FFF'FFFF'FFFF'FFFF
	else
		pcustype("MAXVALUE",sptr)
	esac
	sptr.tagx:=tint
	sptr.value:=a

	++pcptr

end

global proc k_minvalue=
	int64 a

	if sptr.tag=ttype then sptr.tag:=sptr.value fi

	case sptr.tag
	when tword,tpu8,tpu16,tpu32,tpu64 then a:=0
	when tpi8 then a:=-128
	when tpi16 then a:=-32768
	when tpi32 then a:=-0x8000'0000
	when tint,tpi64 then a:=-0x8000'0000'0000'0000
!	when tbignum then a:=-0x8000'0000'0000'0000
	else
		pcustype("MINVALUE",sptr)
	esac
	sptr^.tagx:=tint
	sptr^.value:=a

	++pcptr
end

global proc k_callhost=
	callhostfunction(getopnda)
	skip(1)
end

global proc k_expand=
	variant dest
	int n
	
	n:=getopnda
	dest:=sptr+n-1

	var_expand(sptr,dest,n)
	sptr:=dest

	skip(1)
end

global proc k_pushsymbol=
	symbol d:=cast(getopnda)
	++sptr
	sptr.tagx:=tsymbol
	sptr.def:=cast(getopnda)

	skip(1)
end

global proc k_eq=
	variant x,y
	int res

	y:=sptr
	x:=--sptr

	res:=var_equal(x,y)
	var_unshare(x)
	var_unshare(y)

	sptr.tagx:=tint
	sptr.value:=res

	++pcptr
end

global proc k_ne=
	variant x,y
	int res

	y:=sptr
	x:=--sptr

	res:=not var_equal(x,y)
	var_unshare(x)
	var_unshare(y)

	sptr.tagx:=tint
	sptr.value:=res

	++pcptr
end

global proc k_lt={do_cmp(klt)}
global proc k_le={do_cmp(kle)}
global proc k_ge={do_cmp(kge)}
global proc k_gt={do_cmp(kgt)}

proc do_cmp(int opc)=
	variant x,y
	int res

	y:=sptr
	x:=--sptr

	res:=var_compare(x,y)
	var_unshare(x)
	var_unshare(y)

	sptr.tagx:=tint

	case opc
	when klt then sptr.value:=res<0
	when kle then sptr.value:=res<=0
	when kge then sptr.value:=res>=0
	else sptr.value:=res>0
	esac

	++pcptr
end

global proc k_calldll=
	symbol d:=cast(getopnda)
	int nargs:=getopndb, restype:=getopndc
	variant p

	calldll(d, sptr-nargs+1, sptr-nargs, nargs)

	sptr-:=nargs

!	to nargs do
!		--sptr
!	od
	skip(3)
end

global proc k_in=
	variant x,y
	int n

	y:=sptr
	x:=--sptr

	n:=var_in(x,y)
	var_unshare(x)
	var_unshare(y)

	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

global proc k_notin=
	variant x,y
	int n

	y:=sptr
	x:=--sptr

	n:=not var_in(x,y)
	var_unshare(x)
	var_unshare(y)

	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

global proc k_convrefpack =
	variant a
	int tag,elemtype
	ref void p
	object pa

	switch sptr.tag
	when trefvar then
		a:=sptr.varptr

		pa:=a.objptr
		switch a.tag
		when tint,tword,trefpack then
			p:=&a.value
			elemtype:=tpi64
		when treal then
			p:=&a.value
			elemtype:=tpr64
		when tarray then
			p:=pa.uarray.ptr
			elemtype:=pa.uarray.elemtype
		when tbits then
			sptr.uref.ptr:=pa.ubits.ptr
			sptr.uref.bitoffset:=pa.ubits.indexoffset*ttbitwidth[pa.ubits.elemtype]
			sptr.uref.bitlength:=0
			sptr.tagx:=trefbit
			sptr.uref.elemtag:=pa.ubits.elemtype
			++pcptr
			return

		when tset then
			sptr.uref.ptr:=pa.uset.ptr
			sptr.uref.bitoffset:=0
			sptr.uref.bitlength:=0
			sptr.tagx:=trefbit
			sptr.uref.elemtag:=tpu1
			++pcptr
			return

		when tstring then
			p:=pa.ustr.strptr
			elemtype:=tpu8
			if p=nil then
				p:=""
			fi
		when tstruct then
			p:=pa.ustruct.ptr
			elemtype:=a.usertag
		when tcarray then
			p:=pa.uarray.ptr
			elemtype:=a.usertag
		when tdecimal then
			p:=pa.udec.num
			elemtype:=tpi32

		else
			pcustype("Getrefpack1",a)
		end switch
	when trefpack,trefbit then
		++pcptr
		return

	else
		pcustype("Getrefpack2",sptr)
	endswitch
done::

	sptr.tagx:=trefpack
	sptr.uref.ptr:=p
	sptr.uref.elemtag:=elemtype

	++pcptr
end

global proc k_isdef=
	int res:=sptr.tag<>tvoid
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res
	++pcptr
end

global proc k_isvoid=
	int res:=sptr.tag=tvoid
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res
	++pcptr
end

global proc k_isint=
	istype(tint, tword)
end

global proc k_isnumber=
	int res

	if sptr.tag in [tint,treal,tdecimal,tword] then
		res:=1
	else
		res:=0
	fi
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res
	++pcptr
end

global proc k_ismutable=
	int res

	if sptr.hasref then
		res:=sptr.objptr.mutable
!	elsif sptr.tag=symbol then
!		res
!		res:=1
	else
		res:=1
	fi
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res
	++pcptr
end

global proc k_isreal=
	istype(treal)
end

global proc k_isrange=
	istype(trange)
end

global proc k_isset=
	istype(tset)
end

global proc k_ispointer=
	istype(trefvar,trefpack)
end

proc istype(int t1, t2=tvoid)=
!replace tos with 1 when tos has type t1 or, when t2 is not 0, t2; else tos:=0
	int res, t:=sptr.tag

	res:=t=t1
	if not res and t2 then
		res:=t=t2
	fi
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res
	++pcptr
end

global proc k_convert=
	varrec x
	int t

	t:=getopnda

	if sptr.tag<>t then
		x:=sptr^
		var_convert(&x,t,sptr)
		var_unshare(&x)
	fi

	skip(1)
end

global proc k_switch=
	int index,n,lower

	n:=getopnda
	lower:=getopndb

	case sptr.tag
	when tint,ttype then
	else
		pcerror_s("switch not int",ttname[sptr.tag])
	esac
	index:=sptr.value-lower		!now 0-based index
	--sptr

	if u32(index)>=u32(n) then			!out of range
		pcptr:=ref int((pcptr+n*2+4)^)
	else					!in range
		pcptr:=ref int((pcptr+index*2+4)^) 	!+3 for sw cmd + 1 to label part of (kjumptable,label) pair
	fi
end

global proc k_bytesize=
	int n,t,usert
	object p:=sptr.objptr

	t:=sptr.tag

	case t
	when ttype then
		t:=sptr.value
	when tstruct, trecord, tcarray then
		t:=sptr.usertag
	esac

!t is usertag for structs etc, or base tag
	case t
	when tarray then
		n:=p.uarray.length*ttsize[p.uarray.elemtype]
	when tset then
		n:=getbitssize(p.uset.length,tpu1)
	when tstring then
		n:=p.ustr.length
	when tbits then
		n:=bits_bytesize(p)
	when tlist then
		n:=p.ulist.length*varsize
	when trecord, tstruct, tcarray then
		n:=ttsize[t]	
	when tdecimal then
		n:=p.udec.length
		if n then
			n:=n*decelemsize
		fi
	else
		n:=ttsize[t]
	esac

	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

global proc k_bitwidth=
	if sptr.tag=ttype then
		sptr.value:=ttbitwidth[sptr.value]
	elsif ttbitwidth[sptr.tag] then
		sptr.value:=ttbitwidth[sptr.tag]
	else
		pcerror("bitwidth")
	fi

	sptr.tagx:=tint

	++pcptr
end

global proc k_min=
	variant x,y

	y:=sptr--
	x:=sptr

!	if x.tag=y.tag=tint then
!		if y.value<x.value then
!			sptr.value:=y.value
!		fi
!	elsif var_compare(x,y)<0 then		!x is smaller
	if var_compare(x,y)<0 then		!x is smaller
		var_unshare(y)
	else
		var_unshare(x)
		sptr^:=y^
	fi

	++pcptr
end


global proc k_max=
	variant x,y

	y:=sptr--
	x:=sptr

	if var_compare(x,y)>=0 then		!x is bigger
		var_unshare(y)
	else
		var_unshare(x)
		sptr^:=y^
	fi

	++pcptr
end

global proc k_addsp=
	sptr-:=getopnda
	skip(1)
end

global proc k_pushtry=
	(++sptr)^.tagx:=texception
	sptr.refptr:=ref byte(getopnda)
	sptr.uexcept.frameoffset:=frameptr-ref byte(sptr)		!byte offset
	sptr.uexcept.exceptiontype:=getopndb
	sptr.uexcept.nexceptions:=getopndc
	skip(3)
end

global proc k_raise=
	if sptr.tag<>tint then
		pcerror("Raise: not Int on stack [not proceeding direct to RAISE]")
	fi
	pcptr:=raiseexception(sptr.value)				!will unwind stack and set pcptr to address of exception code
end

global proc k_isequal=
	variant x,y
	int res

	y:=sptr--
	x:=sptr

	if x.hasref and y.hasref and x.objptr=y.objptr then
		res:=1
	else
		res:=0
	fi

	var_unshare(x)
	var_unshare(y)
	sptr.tagx:=tint
	sptr.value:=res

	++pcptr
end

global proc k_minto=
!x min:= y
	variant y:=sptr--
	variant px:=sptr--

	if not var_minto(px, y) then
		var_inplace(px,y, cast(var_min), cast(var_minmixed))
	end

	var_unshare(y)
	++pcptr
end

global proc k_maxto=
!x max:= y
	variant y:=sptr--
	variant px:=sptr--

	if not var_maxto(px, y) then
		var_inplace(px,y, cast(var_max), cast(var_maxmixed))
	end

	var_unshare(y)
	++pcptr
end

global proc k_power=
	variant y:=sptr--
	varrec x:=sptr^

	var_power(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

proc domaths(int opcode)=
	switch sptr.tag
	when tint then
		sptr.tagx:=treal
		sptr.xvalue:=getmaths(opcode,sptr.value)

	when treal then
		sptr.xvalue:=getmaths(opcode,sptr.xvalue)

	else
		pcustype("Maths:",sptr)
	end switch
	++pcptr
end

function getmaths(int opcode, real x)real=
	switch opcode
	when ksqrt then return sqrt(x)
	when ksin then return sin(x)
	when kcos then return cos(x)
	when ktan then return tan(x)
	when kasin then return asin(x)
	when kacos then return acos(x)
	when katan then return atan(x)
	when kln then return ln(x)
	when klog then return log(x)
!	when klg then return lg(x)
	when kexp then return exp(x)
	when kround then
		if x>=0.0 then
			return floor(x+0.5)
		else
			return ceil(x-0.5)
		fi

	when kfloor then
		return floor(x)
	when kceil then
		x:=ceil(x)
		if x=0.0 then x:=0.0 FI
		return ceil(x)

!
!	when kfract then return fract(x)
	else
		pcerror_s("Maths",pclnames[opcode])
	end
	return 0.0
end

global proc k_typepun=
	sptr.tagx:=getopnda
	skip(1)
end

global proc k_andlto=
!px^ iand:= y
	variant y:=sptr--
	variant px:=sptr--
	variant x:=px.varptr

	if px.tag<>trefvar or x.tag<>tint then pcerror("andlto") fi

	x.value iand:=var_istruel(y)
	var_unshare(y)

	++pcptr
end

global proc k_orlto=
!px^ iand:= y
	variant y:=sptr--
	variant px:=sptr--
	variant x:=px.varptr

	if px.tag<>trefvar or x.tag<>tint then pcerror("orlto") fi

	x.value ior:=var_istruel(y)
	var_unshare(y)

	++pcptr
end

global proc k_notlto=
!px^ iand:= y
	variant px:=sptr--
	variant x:=px.varptr

	if px.tag<>trefvar or x.tag<>tint then pcerror("notlto") fi

	x.value ixor:=1

	++pcptr
end

global proc k_pushoperator=
	++sptr
	sptr.tagx:=toperator
	sptr.value:=getopnda
	skip(1)
end

global proc k_maps=
	k_mapss()
end

global proc k_mapss=
	static [10]int codeseq

	int nargs

	case sptr.tag
	when toperator then
		codeseq[1]:=cast(cmdmap[sptr.value])
		--sptr
		codeseq[2]:=(pcptr+1)^			!copy jump lab which follows the applyop
		codeseq[3]:=(pcptr+2)^			!include the dest label
	when tsymbol then
		nargs:=(cmdmap[pcptr^]=cmdmap[kmaps] |1|2)

!I need to push 2-3 stack entries down to make room a return value slot
		for i:=0 downto -(nargs+1) do				!0/1 or 0/1/2
			(sptr+i+1)^:=(sptr+i)^
		od
		(sptr-nargs).tagx:=tvoid
		++sptr
!
		codeseq[1]:=cast(cmdmap[kcallptr])
		codeseq[2]:=nargs
		codeseq[3]:=0
		codeseq[4]:=(pcptr+1)^			!copy jump lab which follows the applyop
		codeseq[5]:=(pcptr+2)^			!include the dest label

	else
		pcerror("Apply:no op")
	esac
	pcptr:=&codeseq[1]				!pass control this short sequence
end

global proc k_idivrem=
	PCERROR("IDIVREM")
end

global proc k_odd=
	case sptr.tag
	when tint,tword then
		sptr.value:=sptr.value.odd
	else
		pcustype("Odd",sptr)
	esac
	sptr.tagx:=tint
	++pcptr
end

global proc k_even=
	case sptr.tag
	when tint,tword then
		sptr.value:=sptr.value.even
	else
		pcustype("Even",sptr)
	esac
	sptr.tagx:=tint
	++pcptr
end

