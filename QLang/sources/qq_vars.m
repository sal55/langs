!Var-routines are usually called from bytecode handlers, either directly on indirectly

!Rules for dealing with variant params are:

!* Input variants are usually never freed; leave that to the caller

!* A dest variant is assumed to be ready to write into

!* Where the same variant is both input and output, assume it can be overwritten
!  without needing to free prior contents (after extracting any necessary info!)

!* If a variant needs to be stored or copied, it should be explicitly shared

!* Variants in memory (not params, but could be part of a complex data structure
!  headed by a param), need to be unshared before overwriting, or shared if reused

!* For SOME var-functions (eg. the variant param for appending), it is assumed that the
!  function will consume the value, ie. transfer ownership, eg. to append to
!  a list. Where it doesn't (eg. append to a string), then the function should unshare.
!  (The alternate to share anyway, and get the caller to unshare, is little less efficient) 

!* Some Var functions are called recursively from others (eg. var_equal on lists),
!  then it is especially important the function does not share or unshare params
!  unless it knows what it's doing

!global macro var_share(x) = var_shareu(x) when x.hasref
global macro var_share(x) = if x.hasref then ++x.objptr.refcount fi
global macro var_unshare(x) = var_unshareu(x) when x.hasref
global macro var_dupl(x) = var_duplu(x) when x.hasref
global macro var_shareu(x) = ++x.objptr.refcount

objrec zeroobj

global proc var_unshareu(variant p)=
	if --p.objptr.refcount<=0 then
		var_free(p)
	fi
end

global proc obj_shareu(object p)=
	++p.refcount
end

global function void_new:variant p =
	p:=pcm_alloc(varrec.bytes)
	p.tagx:=tvoid
	return p
end

global function obj_new:object p=
	P:=PCM_ALLOC(32)
	P^:=ZEROOBJ

	p.refcount:=1
	return p
end

global function var_getintvalue(variant p)int =
! return int value from variant, which should be a numeric type
	case p.tag
	when tint,ttype then
		return p.value
	when treal then
		return p.xvalue
	else
		pcustype("getintvalue",p)
	esac
	return 0
end

global proc var_fromobj(int tag,object p, variant dest)=
	dest.tagx:=tag ior hasrefmask
	dest.objptr:=p
end

global proc var_free(variant a)=
	varrec v
	object q:=a.objptr

	case q.objtype
	when normal_obj then
		case a.tag
		when tlist then
			obj_free_list(q)
		when trecord then
			obj_free_record(q)
		when tstring then
			obj_free_string(q)
		when tarray then
			obj_free_array(q)
		when tvector then
			obj_free_vector(q)
		when tbits then
			obj_free_bits(q,a.tag)
		when tstruct then
			obj_free_struct(q)
		when tdict then
			obj_free_dict(q)
		when tset then
			obj_free_set(q)
		when tdecimal then
			obj_free_dec(q)
		else
!			pcustype_t("free", a.tag)
			pcustype("free", a)
		esac
	when slice_obj then
		v.tagx:=a.tag
		v.objptr:=q.objptr2
		var_unshareu(&v)
		pcm_free32(q)
	else
		pcm_free32(q)
	esac
end

global proc var_duplu(variant a)=
	case a.tag
	when tstring then
		var_dupl_string(a)
	when tset then
		var_dupl_set(a)
	when tlist then
		var_dupl_list(a)
	when tdict then
		var_dupl_dict(a)
	when tarray then
		var_dupl_array(a)
	when tvector then
		var_dupl_vector(a)
	when tbits then
		var_dupl_bits(a)
	when trecord then
		var_dupl_record(a)
	when tstruct then
		var_dupl_struct(a)
	when tdecimal then
		var_dupl_dec(a)
	else
		pcustype_t("dupl", a.tag)
	esac
end

global proc var_neg(variant a)=
	case a.tag
	when tint then
		a.value:=-a.value
	when treal then
		a.xvalue:=-a.xvalue
	when tdecimal then
		var_dupl_dec(a)
		var_neg_dec(a)
	when tset then
		var_dupl_set(a)
		var_inotto_set(a)
	else
		pcustype_t("neg", a.tag)
	esac
end

global proc var_abs(variant a)=
	case a.tag
	when tint then
		a.value:=abs a.value
	when treal then
		a.xvalue:= abs a.xvalue
	when tdecimal then
		var_dupl_dec(a)
		var_abs_dec(a)
	else
		pcustype_t("abs", a.tag)
	esac
end

global proc var_inot(variant a)=
	case a.tag
	when tint then
		a.value:=inot a.value
	when tset then
		var_dupl_set(a)
		var_inotto_set(a)
	else
		pcustype_t("inot", a.tag)
	esac
end

global function var_istruel(variant a)int=
	case a.tag
	when tint, trefvar, trefpack, trefbit, ttype, tsymbol then
		return istrue a.value
	when treal then
		return (a.xvalue<>0|1|0)
!	when tdecimal then
!		return var_istruel_decimal(a)
	when tstring,tlist,tarray,tbits,tvector then
		return a.objptr.length<>0
	when tset then
		return a.objptr.length<>0
!		return var_istruel_string(a)
!	when tlist then
!		return var_istruel_list(a)
!	when tarray then
!		return var_istruel_array(a)
	when trecord, tstruct then
		return 1
!		return var_istruel_record(a)
!	when tstruct then
!		return var_istruel_struct(a)
!	when tbits then
!		return var_istruel_bits(a)
!	when tdict then
!		return var_istruel_dict(a)
	when tdecimal then
		return not bn_iszero(a.objptr)
	when tvoid then
		return 0
	else
		pcustype_t("istruel", a.tag)
	esac
	return 0
end

!global function var_negto(variant p)int=
!	variant a:=p.varptr
!
!	if p.tag<>trefvar then
!		return 0
!	fi
!	case a.tag
!	when tint then
!		a.value:=-a.value
!	when treal then
!		a.xvalue:=-a.xvalue
!!	when tdecimal then
!!		var_negto_decimal(a)
!	when tset then
!		var_inotto_set(a)
!	else
!		pcustype_t("negto", a.tag)
!	esac
!	return 1
!end
!
!global function var_absto(variant p)int=
!	variant a:=p.varptr
!
!	if p.tag<>trefvar then
!		return 0
!	fi
!	case a.tag
!	when tint then
!		a.value:= abs a.value
!	when treal then
!		a.xvalue:= abs a.xvalue
!!	when tdecimal then
!!		var_absto_decimal(a)
!	else
!		pcustype_t("absto", a.tag)
!	esac
!	return 1
!end
!
!global function var_inotto(variant p)int=
!	variant a:=p.varptr
!
!	if p.tag<>trefvar then
!		return 0
!	fi
!	case a.tag
!	when tint then
!		a.value:=inot a.value
!	when tset then
!		var_inotto_set(a)
!	else
!		pcustype_t("inotto", a.tag)
!	esac
!	return 1
!end

global proc var_add(variant a, b)=
	if a.tag<>b.tag then
		var_addmixed(a,b)
		return
	fi

	case a.tag
	when tint then
		a.value+:=b.value
	when treal then
		a.xvalue+:=b.xvalue
	when tdecimal then
		var_add_dec(a,b)
	when tstring then
		var_add_string(a,b)
	when tset then
		var_dupl_set(a)
		var_iorto_set(a,b)
	else
		pcustype_t("add", a.tag)
	esac
end

global proc var_addmixed(variant a, b)=
	int newtag:=a.tag

	int tt

	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		newtag:=treal
		a.xvalue:=a.value+b.xvalue
	when pr(treal,		tint)     then
		a.xvalue+:=b.value
!	when pr(tint,		tword)    then
!		a.value+:=b.value
!	when pr(tword,		tint)     then
!		newtag:=tint
!		a.value+:=b.value
	when pr(tint,tdecimal),
		 pr(treal,tdecimal) then
		newtag:=tdecimal
		var_add_dec(dectemp(a),b)
		freedectemp()
	when pr(tdecimal,tint),
		 pr(tdecimal,treal) then
		var_add_dec(a,dectemp(b))

	when pr(trefpack,	tint) then
		if a.ptr=nil then pcerror("Nil+x") fi
		a.ptr+:=ttsize[a.elemtag]*b.value
	else
		pcmxtypes("Addmixed",a,b)
	esac

	a.tag:=newtag
end

global function var_addto(variant p,b)int=
!p^+:=b
!handles case where p is a refvar and types are mostly matched
	variant a:=p.varptr
	int newtag

	return 0 when p.tag<>trefvar
	newtag:=a.tag

	if a.tag<>b.tag then
		if newtag=tstring and b.tag=tint then
			var_addto_string_ch(a,b.value)
			return 1
		fi
		return 0

	fi

	switch a.tag
	when tint then
		a.value+:=b.value
	when treal then
		a.xvalue+:=b.xvalue
!	when tdecimal then
!		var_addto_decimal(a,b)
	when tstring then
		var_addto_string(a,b)
	when tset then
		var_iorto_set(a,b)
	else
		return 0
	end switch

	a.tag:=newtag

	return 1
end


global proc var_sub(variant a, b)=
	ref byte p,q
	int elemsize,x

	if a.tag<>b.tag then
		var_submixed(a,b)
		return
	fi
	case a.tag
	when tint then
		a.value-:=b.value
	when treal then
		a.xvalue-:=b.xvalue
	when tdecimal then
		var_sub_dec(a,b)
	when trefpack then
		p:=a.ptr
		q:=b.ptr
		case elemsize:=ttsize[a.elemtag]
		when 1 then x:=p-q
		when 2 then x:=(p-q)>>1
		when 4 then x:=(p-q)>>2
		else x:=(p-q)/elemsize
		esac
		a.tagx:=tint
		a.value:=x
!		var_sub_refpack(a,b)
!	when tset then
!		var_sub_set(a,b)
	else
		pcustype_t("sub", a.tag)
	esac
end

global proc var_submixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		newtag:=treal
		a.xvalue:=a.value-b.xvalue
		
	when pr(treal,		tint)     then
		a.xvalue-:=b.value
	when pr(tint,tdecimal),
		 pr(treal,tdecimal) then
		newtag:=tdecimal
		var_sub_dec(dectemp(a),b)
		freedectemp()
	when pr(tdecimal,tint),
		 pr(tdecimal,treal) then
		var_sub_dec(a,dectemp(b))


!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	when pr(trefpack,	tint) then
		a.ptr-:=ttsize[a.elemtag]*b.value
	else
		pcmxtypes("Submixed",a,b)
	esac

	a.tag:=newtag
end

global proc var_mul(variant a, b)=
	if a.tag<>b.tag then
		var_mulmixed(a,b)
		return
	fi
	case a.tag
	when tint then
		a.value*:=b.value
	when treal then
		a.xvalue*:=b.xvalue
	when tdecimal then
		var_mul_dec(a,b)
	when tset then
		var_dupl_set(a)
		var_iandto_set(a,b)
	else
		pcustype_t("mul", a.tag)
	esac
end

global proc var_mulmixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		newtag:=treal
		a.xvalue:=a.value*b.xvalue
		
	when pr(treal,		tint)     then
		a.xvalue*:=b.value
	when pr(tint,tdecimal),
		 pr(treal,tdecimal) then
		newtag:=tdecimal
		var_mul_dec(dectemp(a),b)
		freedectemp()
	when pr(tdecimal,tint),
		 pr(tdecimal,treal) then
		var_mul_dec(a,dectemp(b))

	when pr(tstring, tint) then
		var_mul_string(a,b.value)
	when pr(tlist, tint) then
		var_mul_list(a,b.value)
	else
		pcmxtypes("Mulmixed",a,b)
	esac

	a.tag:=newtag
end

global proc var_div(variant a, b)=
	if a.tag<>b.tag then
		var_divmixed(a,b)
		return
	fi
	case a.tag
	when tint then
		a.tagx:=treal
		a.xvalue:=real(a.value)/b.value
!		var_div_int(a,b)
	when treal then
		a.xvalue/:=b.xvalue
	when tdecimal then
		var_div_dec(a,b)
	else
		pcustype_t("div", a.tag)
	esac
end

global proc var_divmixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		newtag:=treal
		a.xvalue:=a.value/b.xvalue
	when pr(treal,		tint)     then
		a.xvalue/:=b.value
!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	else
		pcmxtypes("Divmixed",a,b)
	esac

	a.tag:=newtag
end

global proc var_idiv(variant a, b)=
	if a.tag<>b.tag then
		pcerror("idivmixed")
		return
	fi
	case a.tag
	when tint then
!CPL "/////////////IDIV",A.VALUE,B.VALUE
		if b.value then
			a.value:=a.value/b.value
		else
			pcerror("Divide by 0")
		fi
!		var_idiv_int(a,b)
	when tdecimal then
		var_idiv_dec(a,b)
	else
		pcustype_t("idiv", a.tag)
	esac
end

!global proc var_idivmixed(variant a, b)=
!	int newtag:=a.tag
!	case pr(a.tag,		b.tag)
!!	when pr(tint,		tdecimal) then
!!		pcerror("int/dec")
!!	when pr(tdecimal,	tint)     then
!!		pcerror("dec/int")
!!	when pr(treal,		tdecimal) then
!!		pcerror("real/dec")
!!	when pr(tdecimal,	treal)    then
!!		pcerror("dec/real")
!	else
!		pcmxtypes("Idivmixed",a,b)
!	esac
!
!	a.tag:=newtag
!end

global proc var_irem(variant a, b)=
	if a.tag<>b.tag then
		pcerror("iremmixed")
		return
	fi

	case a.tag
	when tint then
		a.value:=a.value rem b.value

!		var_irem_int(a,b)
	when tdecimal then
		var_irem_dec(a,b)
	else
		pcustype_t("irem", a.tag)
	esac
end

!global proc var_iremmixed(variant a, b)=
!	int newtag:=a.tag
!	case pr(a.tag,		b.tag)
!!	when pr(tint,		tdecimal) then
!!		pcerror("int/dec")
!!	when pr(tdecimal,	tint)     then
!!		pcerror("dec/int")
!!	when pr(treal,		tdecimal) then
!!		pcerror("real/dec")
!!	when pr(tdecimal,	treal)    then
!!		pcerror("dec/real")
!	else
!		pcmxtypes("Iremmixed",a,b)
!	esac
!
!	a.tag:=newtag
!end

global proc var_iand(variant a, b)=
	if a.tag<>b.tag then
		pcerror("iand mixed")
		return
	fi
	case a.tag
	when tint then
		a.value iand:= b.value
	when tset then
		var_dupl_set(a)
		var_iandto_set(a,b)
	else
		pcustype_t("iand", a.tag)
	esac
end

global proc var_ior(variant a, b)=
	if a.tag<>b.tag then
		pcerror("ior mixed")
!		var_iormixed(a,b)
		return
	fi
	case a.tag
	when tint then
		a.value ior:=b.value

	when tset then
		var_dupl_set(a)
		var_iorto_set(a,b)
	else
		pcustype_t("ior", a.tag)
	esac
end

global proc var_ixor(variant a, b)=
	if a.tag<>b.tag then
		pcerror("ixor mixed")
!		var_ixormixed(a,b)
		return
	fi
	case a.tag
	when tint then
		a.value ixor :=b.value
	when tset then
		var_dupl_set(a)
		var_ixorto_set(a,b)
	else
		pcustype_t("ixor", a.tag)
	esac
end

global proc var_shl(variant a, b)=
	if a.tag<>b.tag then
		pcerror("ishl mixed")
!		var_shlmixed(a,b)
		return
	fi
	case a.tag
	when tint then
		a.value<<:=b.value

!		var_shl_int(a,b)
!	when tdecimal then
!		var_shl_decimal(a,b)
	else
		pcustype_t("shl", a.tag)
	esac
end

global proc var_shr(variant a, b)=
	if a.tag<>b.tag then
		pcerror("ishr mixed")
!		var_shrmixed(a,b)
		return
	fi
	case a.tag
	when tint then
		a.value>>:=b.value
!	when tint, tword then
!		var_shr_int(a,b)
!	when tdecimal then
!		var_shr_decimal(a,b)
	else
		pcustype_t("shr", a.tag)
	esac
end

global function var_in(variant a, b)int n=
	case pr(a.tag,b.tag)
	when pr(tint, tset) then
		return var_in_set(a,b)
	when pr(tint, trange) then
		return (a.value in b.range_lower..b.range_upper|1|0)

	elsecase b.tag
	when tlist, tstring, tarray, tvector then
		n:=var_inx(a,b)
		return (n<>i64.min|1|0)
	else
		pcmxtypes("in", a,b)
	esac
	return 0
end

global function var_inx(variant a, b)int n=
	case pr(a.tag,b.tag)
	when pr(tstring,tstring) then
		return var_inx_string(a,b)
	when pr(tint, tarray), pr(treal, tarray) then
		return var_inx_array(a,b, tvoid)
	when pr(tint, tvector), pr(treal, tvector) then
		return var_inx_array(a,b, b.objptr.usertag)
	elsecase b.tag
	when tlist then
		return var_inx_list(a,b)
	else
		pcmxtypes("inx", a,b)
	esac
	return 0
end

global function var_equal(variant a,b)int=
!can be called when a/b have same tags, or were mixed and have been
!converted, but also they haven't been checked.
	if a.tag<>b.tag then
		return var_equalmixed(a,b)
	fi

	case a.tag
	when tint, trefvar, trefpack, ttype, tsymbol then
		return a.value=b.value
!	when trefpack, trefbit then
!		return var_equal_refpack(a, b)
	when treal then
		return (a.xvalue=b.xvalue|1|0)
	when tdecimal then
		return var_equal_dec(a, b)
	when tstring then
		return var_equal_string(a, b)
	when tset then
		return var_equal_set(a, b)
	when tlist then
		return var_equal_list(a, b)
	when tdict then
		return var_equal_dict(a, b)
	when tarray, tvector then
		return var_equal_array(a, b)
	when tbits then
		return var_equal_bits(a, b)
	when trecord then
		return var_equal_record(a, b)
	when tstruct then
		return var_equal_struct(a, b)
	else
		pcustype_t("equal", a.tag)
	esac
	return 0
end

global function var_equalmixed(variant a, b)int=
	int result

	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		return (a.value=b.xvalue|1|0)
		
	when pr(treal,		tint)     then
		return (a.xvalue=b.value|1|0)
!		a.value+:=b.value
!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	when pr(tint,tdecimal),
		 pr(treal,tdecimal) then
		result:=var_equal_dec(dectemp(a),b)
		freedectemp()
		return result
	when pr(tdecimal,tint),
		 pr(tdecimal,treal) then
		return var_equal_dec(a,dectemp(b))

	else
		return 0
!		pcmxtypes("equalmixed",a,b)
	esac

	return 0
end

global function var_compare(variant a,b)int=
	if a.tag<>b.tag then
		return var_comparemixed(a,b)
	fi

	case a.tag
	when tint, trefpack then
		return (a.value<b.value|-1|(a.value>b.value|1|0))
	when treal then
		return (a.xvalue<b.xvalue|-1|(a.xvalue>b.xvalue|1|0))
	when tdecimal then
		return var_compare_dec(a,b)
	when tstring then
		return var_compare_string(a,b)
	else
		pcustype_t("compare", a.tag)
	esac
	return 0
end

global function var_comparemixed(variant a, b)int=
	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		return (a.value<b.xvalue|-1|(a.value>b.xvalue|1|0))
!		
	when pr(treal,		tint)     then
		return (a.xvalue<b.value|-1|(a.xvalue>b.value|1|0))
!	when pr(tint,		tword)    then
!		a.value+:=b.value
!	when pr(tword,		tint)     then
!		a.value+:=b.value
!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	else
		pcmxtypes("comparemixed",a,b)
	esac

	return 0
end


global proc var_concat(variant a,b)=
	if a.tag<>b.tag then
		pcmxtypes("Concat",a,b)
	fi

	case a.tag
	when tstring then
		var_add_string(a,b)
	when tlist then
		var_dupl_list(a)
		var_concatto_list(a,b)
	when tarray then
		var_dupl_array(a)
		var_concatto_array(a,b)
!	when tarray then
!		var_concatto_array(a,b)
	else
		pcustype_t("concat", a.tag)
	esac
end

global proc var_append(variant a,b)=
!be is expected to be consumed here

	if a.tag<>b.tag then
		case a.tag
		when tlist then dolist
		when tarray then doarray
		when tbits then dobits
		esac
		error
	elseswitch a.tag
	when tstring then
		var_add_string(a,b)
		var_unshareu(b)		!caller expects ownership to be xfered
	when tlist then
dolist:
		var_dupl_list(a)
		var_appendto_list(a,b)
	when tarray then
doarray:
		var_dupl_array(a)
		var_appendto_array(a,b)
	when tbits then
dobits:
		var_dupl_bits(a)
		var_appendto_bits(a,b)
	else
error:
		pcustype_t("append", a.tag)
	end
end

global proc var_min(variant a,b)=
	if a.tag<>b.tag then
		pcerror("VARMIN")
		return
	fi

	if var_compare(a,b)<0 then		!x is smaller
		var_unshare(b)
	else
		var_unshare(a)
		a^:=b^
	fi
end

global proc var_max(variant a,b)=
	if a.tag<>b.tag then
		pcerror("VARMAX")
!		var_maxmixed(a,b)
		return
	fi

	if var_compare(a,b)>=0 then		!x is bigger
		var_unshare(b)
	else
		var_unshare(a)
		a^:=b^
	fi
end

global function var_concatto(variant a,b)int=
!	return 0
	if a.tag<>b.tag then pcerror("concatto/mixed") fi
	case a.tag
	when tstring then
		var_addto_string(a,b)
	when tlist then
		var_concatto_list(a,b)
	when tarray then
		var_concatto_array(a,b)
	else
		pcustype("concat",a)
	esac
	return 1
end

global function var_appendto(variant a,b)int=
!return 0
	if a.tag<>b.tag then
		case a.tag
		when tlist then dolist
		when tarray then doarray
		when tbits then dobits
		else
			pcerror("appendto/mixed")
		esac
	fi

	case a.tag
	when tstring then
		var_addto_string(a,b)
		var_unshareu(b)		!caller expects ownership to be xfered
	when tlist then
dolist:
		var_appendto_list(a,b)
	when tarray then
doarray:
		var_appendto_array(a,b)
	when tbits then
dobits:
		var_appendto_bits(a,b)
	else
		pcustype("append",a)
		return 0
	esac
	return 1
end

global proc var_getix(variant a, int index)=
	case a.tag
	when tstring then
		var_getix_string(a,index)
	when tlist, tdict then
		var_getix_list(a,index)
	when tarray,tvector then
		var_getix_array(a,index)
	when tbits then
		var_getix_bits(a,index)
	when tset then
		var_getix_set(a,index)
	when trecord then
		var_getix_record(a,index)
	when trange then
		if index in a.range_lower..a.range_upper then
			a.tagx:=tint
			a.value:=index
		else
			pcerror("range/bounds")
		fi

!	when tstruct then
!		var_getix_struct(a,index)
	else
		pcustype_t("getix", a.tag)
	esac
end

global proc var_putix(variant a, int index, variant x)=
# comment

	case a.tag
	when tstring then
		var_putix_string(a,index,x)
		var_unshareu(x)
	when tlist then
		var_putix_list(a,index,x)
	when tarray,tvector then
		var_putix_array(a,index,x)
	when tbits then
		var_putix_bits(a,index,x)
	when tset then
		var_putix_set(a,index,x)
	when trecord then
		var_putix_record(a,index,x)
!	when tstruct then
!		var_putix_struct(a,index,x)
	else
		pcustype_t("putix", a.tag)
	esac
end

global proc var_getixref(variant a, int index)=
	case a.tag
	when tstring then
		var_getixref_string(a,index)
	when tlist then
		var_getixref_list(a,index)
	when tarray,tvector then
		var_getixref_array(a,index)
	when tbits then
		var_getixref_bits(a,index)
	when tset then
		var_getixref_set(a,index)
	when trecord then
		var_getixref_record(a,index,a)
!	when tstruct then
!		var_getixref_struct(a,index)
	else
		pcustype_t("getixref", a.tag)
	esac
end

global proc var_getslice(variant a, int i,j)=
	case a.tag
	when tstring then
		var_getslice_string(a,i,j)
	when tlist then
		var_getslice_list(a,i,j)
	when tarray then
		var_getslice_array(a,i,j)
	when tbits then
		var_getslice_bits(a,i,j)
	else
		pcustype_t("getslice", a.tag)
	esac
end

global proc var_putslice(variant a, int i,j, variant x)=
	if a.tag<>x.tag then
		pcerror("putslice: not compatible")
	fi

	case a.tag
	when tstring then
		var_putslice_string(a,i,j,x)
	when tlist then
		var_putslice_list(a,i,j,x)
!	when tarray then
!		var_putslice_array(a,i,j,x)
!	when tbits then
!		var_putslice_bits(a,i,j,x)
	else
		pcustype_t("putslice", a.tag)
	esac
end

global proc var_getdotix(variant a, int index)=
	case a.tag
	when tint then
		if index not in 0..63 then
			pcerror("int.[int] bounds")
		fi
		a.value:=(a.value>>index) iand 1
	when tstring then
		var_getdotix_string(a,index)
	when trecord then
		var_getix_record(a,index)
!	when tstruct then
!		var_getdotix_struct(a,index)
	else
		pcustype_t("getdotix", a.tag)
	esac
end

global proc var_putdotix(variant p, int index, variant x)=
	variant a

	if p.tag=trefvar then
		a:=p.varptr

		case a.tag
		when tint then
			if index not in 0..63 then
				pcerror("int.[int]:= bounds")
			fi
			var_storebit(cast(&a.value),index,x,tu1,1)

		when tstring then
			var_putdotix_string(a,index,x)
		when trecord then
			var_putix_record(a,index,x)
!	when tstruct then
!		var_putdotix_struct(a,index,x)
		else
			pcustype("putdotix", a)
		esac
	else
		pcustype("putdotix",p)
	fi
end

global proc var_getdotixref(variant p, int index)=
	variant a

	if p.tag=trefvar then
		a:=p.varptr

		case a.tag
		when tint then
			if index not in 0..63 then
				pcerror("&int.[int] bounds")
			fi
			p.ptr:=cast(&a.value)
			p.tagx:=trefbit
			p.elemtag:=tu1
			p.bitoffset:=index
			p.bitlength:=1
		when tstring then
			var_getdotixref_string(a,index,p)
		when trecord then
			var_getixref_record(a,index,p)
!		when tstruct then
!			var_getdotixref_struct(a,index)
		else
			pcustype_t("getdotixref", a.tag)
		esac
	else
		pcustype("not refvar",p)
	fi
end

global proc var_getdotslice(variant a, int i,j)=
	case a.tag
	when tint then

		if i>j then swap(i,j) fi
		if i<0 or j>63 then pcerror("int.[slice] bounds") fi
		a.value:=(a.value>>i)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))

!		var_getdotslice_int(a,i,j)
	when tstring then
		var_getslice_string(a,i,j)
	else
		pcustype_t("getdotslice", a.tag)
	esac
end

global proc var_putdotslice(variant p, int i,j, variant x)=
	variant a

	if p.tag=trefvar then
		a:=p.varptr

		case a.tag
		when tint then
			if i>j then swap(i,j) fi
			if i<0 or j>63 then pcerror("int.[slice]:= bounds") fi
			var_storebit(cast(&a.value), i,x,tu1,j-i+1)
		when tstring then
			var_putslice_string(a,i,j,x)
		else
			pcustype("putdotslice", a)
		esac
	else
		pcustype("not ref",p)
	fi
end

global proc var_getdotsliceref(variant p, int i,j)=
	variant a

	if p.tag=trefvar then
		a:=p.varptr

		case a.tag
		when tint then
			if i>j then swap(i,j) fi
			if i<0 or j>63 then
				pcerror("&int.[slice] bounds")
			fi
			p.ptr:=cast(&a.value)
			p.tagx:=trefbit
			p.elemtag:=tu1
			p.bitoffset:=i
			p.bitlength:=j-i+1
!			var_getdotsliceref_int(a,i,j)
		else
			pcustype("getdotsliceref", a)
		esac
	else
		pcustype("not ref",p)
	fi
end

global proc var_expand(variant a, dest, int m)=
!expand object at a to maximum m entries, starting at dest
!(dest may overlap a, since usually this is done on the stack)
!arrays, records are expanded the first m entries. If there are 
!fewer, then padded with void entries
!ranges expand to two integers
!minimum m will be 2.
	variant b,c
	object p
	ref char s
	int n,length

!CPL "EXPAND",=M,TTNAME[A.TAG]

	if m<2 then pcerror("Expand: LHS too few") fi

	case a.tag
	when tlist then
		p:=a.objptr
		length:=p.length
dolist:
		b:=dest
		c:=p.varptr
		n:=1

		to m do
			if n>length then
				dest.tagx:=tvoid
			else
				dest^:=c^
				var_share(dest)
				++c
			fi
			++n
			--dest
		od

	when trange then			!expand to two ints
		dest.tagx:=tint
		dest.value:=a.range_lower
		--dest
		dest.tagx:=tint
		dest.value:=a.range_upper
		to m-2 do
			--dest
			dest.tagx:=tvoid
		od

	when tstring then
		var_expand_string(a, dest, m)
!
	when trecord then
		p:=a.objptr
		length:=ttlength[p.usertag]
		goto dolist

	when tarray, tvector then
		var_expand_array(a, dest, m)

	else
		pcustype("expand",a)
	esac
end

!global function var_minto(variant p,b)int=
!	variant a:=p.varptr
!	int newtag
!
!	if p.tag<>trefvar then
!		return 0
!	fi
!	if newtag:=a.tag<>b.tag then
!		return 0
!	fi
!
!	case a.tag
!	when tint then
!		a.value min:=b.value
!!	when treal then
!!		a.xvalue min:=b.xvalue
!!!		var_addto_real(a,b)
!	when tdecimal then
!		if var_compare_dec(a,b)>0 then
!			var_shareu(b)
!			var_unshareu(a)
!			a^:=b^
!		fi
!
!!!	when tstring then
!!!		var_addto_string(a,b)
!	when tstring then
!		if var_compare_string(a,b)>0 then		!b is smaller
!			var_shareu(b)
!			var_unshareu(a)
!			a^:=b^
!		fi
!
!!!	when tset then
!!!		var_addto_set(a,b)
!	else
!		return 0
!	esac
!	return 1
!end
!
!global function var_maxto(variant p,b)int=
!	variant a:=p.varptr
!	int newtag
!
!	if p.tag<>trefvar then
!		return 0
!	fi
!	if newtag:=a.tag<>b.tag then
!		return 0
!	fi
!
!	case a.tag
!	when tint then
!		a.value max:=b.value
!!	when treal then
!!		a.xvalue min:=b.xvalue
!!!		var_addto_real(a,b)
!	when tdecimal then
!		if var_compare_dec(a,b)<0 then
!			var_shareu(b)
!			var_unshareu(a)
!			a^:=b^
!		fi
!!!	when tdecimal then
!!!		var_addto_decimal(a,b)
!!!	when tstring then
!!!		var_addto_string(a,b)
!	when tstring then
!		if var_compare_string(a,b)<0 then		!b is bigger
!			var_shareu(b)
!			var_unshareu(a)
!			a^:=b^
!		fi
!
!!!	when tset then
!!!		var_addto_set(a,b)
!	else
!		return 0
!	esac
!	return 1
!end
!
global proc var_inplace(variant px,y, ref proc(variant,variant) fnadd, fnaddmixed=nil)=
	varrec x
	varrec z

	var_loadptr(px,&x)
!z:=x

	if x.tag=y.tag then
		fnadd^(&x,y)
	elsif fnaddmixed then
		fnaddmixed^(&x,y)
	else
!		if u64(fnadd)=u64(var_add) and x.tag=tstring and y.tag=tint then
!			var_addto_string_ch(&x,y.value)
!		else
!
!			pcerror("Inplace mixed")
			pcmxtypes("Inplace mixed",&x,y)
!		fi
	fi

!var_unshare(&z)
	var_storeptr(px,&x)
end

global proc var_inplace_unary(variant px, ref proc(variant) fnneg)=
	varrec x

!CPL "INPLACE UNARY"

	var_loadptr(px,&x)
	fnneg^(&x)
	var_storeptr(px,&x)
end

global proc var_loadptr(variant x,y)=
!y:=x^

	case x.tag
	when trefvar then
		y^:=(x.varptr)^
		if y.hasref then
			++y.objptr.refcount
		fi

	when trefpack then
		var_loadpacked(x.ptr,x.elemtag,y,nil)

	when trefbit then
		var_loadbit(x.ptr, x.bitoffset, x.elemtag, x.bitlength, y)

	else
		pcustype("var_loadptr",x)
	esac
end

global proc var_storeptr(variant p,q)=
!p^:=q
	variant dest
	variant pptr,qptr
	varrec v
	int i,n,etag
	int poffset,qoffset,bitwidthx
	ref byte pp,qq
	int aa,bb

	case p.tag
	when trefvar then
		dest:=p.varptr
		var_unshare(dest)
!		var_share(q)
		dest^:=q^

	when trefpack then
		var_storepacked(ref byte(p.ptr),q,p.elemtag)
!		var_unshare(q)

	when trefbit then
		var_storebit(p.ptr,p.bitoffset,q,p.elemtag,p.bitlength)

	else
		pcustype("var_popptr",p)
	esac
end

global proc var_loadbit(ref byte p,int shift,t,bitlength,variant dest) =
!t is tu1/tu2/tu4 load bitfield from p^ at given bit offset, to dest
	ref word pd
	word mask

	dest.tagx:=tint
	case (t)
	when tu1 then
		if bitlength=0 then
			dest.value:=not not (p^ iand (1<<shift))
		else
			pd:=cast(p)
			mask:=0xFFFF'FFFF'FFFF'FFFE
			case bitlength
			when 1 then
			when 64 then
				mask:=0
			else
				mask<<:=bitlength-1
			esac
			dest.value:=(pd^>>shift) iand (inot mask)
		fi

	when tu2 then
		dest.value:=(p^ iand (3<<shift))>>shift
	when tu4 then
		dest.value:=(p^ iand (15<<shift))>>shift
	else
		pcustype_t("loadbit",t)
	esac

end

global proc var_storebit(ref byte p,int shift,variant q,int t,bitlength) =
!t is tu1/tu2/tu4 store bitfield to p^ at given bit offset, from dest
!shift will be 0,1,2,3,4,5,6,7
	ref word pd
	byte bb
	word mask1,mask2,newvalue

	if q.tag<>tint then
		pcerror("storebit not int")
	fi

	case (t)
	when tu1 then
		if bitlength=0 then
			p^:=(p^ iand inot(1<<shift)) ior ((q.value iand 1)<<shift)
		else
			pd:=cast(p)
			mask1:=0xFFFF'FFFF'FFFF'FFFE
			case bitlength
			when 1 then
			when 64 then
				mask1:=0
			else
				mask1<<:=bitlength-1
			esac

			mask1 :=inot mask1

			if shift then
				mask1<<:=shift
			fi

			mask2:=inot mask1
			newvalue:=q.value
			if shift then
				newvalue<<:=shift
			fi
			pd^:=(pd^ iand mask2) ior (newvalue iand mask1)
		fi

	when tu2 then
		p^:=(p^ iand inot(3<<shift)) ior ((q.value iand 3)<<shift)
	when tu4 then
		p^:=(p^ iand inot(15<<shift)) ior ((q.value iand 15)<<shift)
	else
		pcustype_t("storebit",t)
	esac
end

global proc var_convert(variant x, int t, variant dest)=
!convert x to type t and store new value in dest
!dest will be different location from x
	int s,tbase
	i64 aa
	varrec bn

	dest^:=x^

	s:=x.tag
!	if s=t and s<tlist then		!same type
	if s=t then		!same type
		return 							!Note: heap types such as arrays must match on elemtypes too
	fi
	tbase:=t

	dest.tag:=t			!assume works, so pre-set tag

	case s
	when tint then
		case tbase
		when tint then			!no changes needed
		when treal then
			dest.xvalue:=x.value
		when tdecimal then
			var_make_dec_int(sptr.value,dest)
!		elsif ttbasetype[t]=tenum then
!			dest.tag:=tenum
!			dest.elemtag:=t

		else
			pcustype_t("conv int=>",t)
		esac

	when treal then
		case tbase
		when tint then
			dest.value:=x.xvalue
		else
			pcustype_t("conv real=>",t)
		esac

	when trefpack,trefvar,trefbit then
		case ttbasetype[tbase]
		when tint then
		when trefpack then
			dest.tag:=trefpack
			dest.elemtag:=tttarget[t]
		else
			pcustype_t("conv ptr=>",t)
		esac
	when tstring then
		case tbase
		when tlist then
			var_convert_string_list(x,t,dest)

		when tdecimal then
			var_make_dec_str(x.objptr.strptr, x.objptr.length, dest)
		when tstring then
		else
			pcustype_t("string=>",t)
		esac

	when ttype then
		if tbase<>tint then
			pcustype_t("type=>",t)
		fi

	when tdecimal then
		case (tbase)
		when tint then
			aa:=var_convert_dec_int(x)
			dest.tagx:=tint
			dest.value:=aa

		else
			pcustype_t("decimal=>",t)
		esac

!	when tenum then
!		case tbase
!		when tint then			!no changes needed
!			dest.tagx:=tint
!		when tenum then
!			dest.elemtag:=x.elemtag
!		else
!			pcustype_t("conv enum=>",t)
!		esac

	else
		pcmxtypestt("Convert s.t",s,t)
	esac

end

global function var_gethashvalue(variant p)int=
	int hsum,csum,c,n,i,result
	ref char s,s0
	object q

	case p.tag
	when tstring then
		n:=p.objptr.length
		if not n then return 0 fi
		hsum:=0
		s:=p.objptr.strptr
		to n do
			c:=s++^
			hsum:=(hsum<<4-hsum) +c
		od
		result:=hsum<<5-hsum

		return result iand 0x7FFF'FFFF'FFFF'FFFF		!keep positive

	when tint,treal,trange then
		return p.value
	when tdecimal then
		q:=p.objptr
		if q.length=0 then
			return 0
		else
			return q.num[0]
		fi
	else
		pcustype("Can't hash:",p)
	esac
	return 0
end

global proc var_objtovar(int tag, object p, variant q)=
	q.tagx:=tag ior hasrefmask
	q.objptr:=p
end

global proc var_putdotix_intint(variant a, int index, variant b)=
!a.[index]:=b
!a, b are both ints
	word x:=a.value
	word y:=b.value

	if index not in 0..63 then
		pcerror("int.[int]:= bounds")
	fi

	a.value:=x iand inot (1<<index) ior y<<index
end

global proc var_power(variant a, b)=
	if a.tag<>b.tag then
		var_powermixed(a,b)
		return
	fi

	case a.tag
	when tint then
		a.value:=a.value**b.value
	when treal then
!		a.xvalue:=a.xvalue**b.xvalue
		a.xvalue:=pow(a.xvalue,b.xvalue)
	when tdecimal then
		var_power_dec(a,var_convert_dec_int(b))
	else
		pcustype_t("power", a.tag)
	esac
end

global proc var_powermixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		newtag:=treal
		a.xvalue:=pow(a.value,b.xvalue)
		
	when pr(treal,		tint)     then
		a.xvalue:=pow(a.xvalue,b.value)

	when pr(tdecimal, tint) then
		var_power_dec(a,b.value)

	else
		pcmxtypes("Powermixed",a,b)
	esac

	a.tag:=newtag
end

