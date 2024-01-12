global proc var_empty_array(int tag, elemtype, lower, variant dest)=
	dest.objptr:=obj_newarray(elemtype,lower, 0)
	dest.tagx:=tag ior hasrefmask
!	dest.usertag:=usertag
end

global proc obj_free_array(object p)=
	if p.length then
		pcm_free(p.ptr, p.alloc64*ttsize[p.elemtag])
	fi

	pcm_free32(p)
end

global proc obj_free_vector(object p)=
	if p.length then
		pcm_free(p.ptr,ttsize[p.usertag])
	fi

	pcm_free32(p)
end

global proc var_make_array(variant a, dest, int lower, n, axtype, elemtype) =
!create a list of n vars starting from a in reverse order (a is the last)
!put the result in dest (note this will be the last the n vars)
!axtype = array, elemtype = void/T
!axtype = vector basetype, elemtype = T

	object p
	ref byte q
	int m

	if axtype<>tarray then				!vector: built-in length and elemtype
		m:=ttlength[axtype]
		if n<>m then
			println =n,=m
			pcerror("Too few/too many elements")
		fi

	elsif n then
		if elemtype=tvoid then			!array: n>0: no elemtype override
			case (a+n-1).tag
			when tint then elemtype:=ti64
			when treal then elemtype:=tr64
			else
				elemtype:=ti64
			esac
		fi

	elsif elemtype=tvoid then			!array: n=0: no elemtype override
		elemtype:=ti64
	fi

	p:=obj_newarray(elemtype,lower,n)
	q:=p.ptr

	to n do
		var_storepacked(q,a,elemtype)
		q+:=ttsize[elemtype]
		++a
	od

	if axtype=tarray then
		dest.tagx:=tarray ior hasrefmask
	else
		dest.tagx:=tvector ior hasrefmask
		p.usertag:=axtype
	fi
	dest.objptr:=p
end

global function obj_newarray(int elemtype, lower,length)object p=
!create a packed array with element-type t, given length and lower bound.
!it will be initialised to zeros

	ref byte q
	int elemsize

	p:=obj_new()
	p.mutable:=1
	if lower in 0..1 then
		p.lower:=lower
	else
		pcerror("Lwb not 0/1")
	fi
	p.length:=length
	p.objtype:=normal_obj
	p.elemtag:=elemtype
	elemsize:=ttsize[elemtype]

	if length then
		p.ptr:=pcm_allocz(length*elemsize)
		p.alloc64:=allocbytes/elemsize
	fi

	return p
end

global function obj_newarray_u(int usertag)object p=
!create a packed array with element-type t, given length and lower bound.
!it will be initialised to zeros

	ref byte q
	int elemsize

	p:=obj_new()
	p.mutable:=1
	p.objtype:=normal_obj
	p.usertag:=usertag
	elemsize:=ttsize[tttarget[usertag]]

	if ttlength[usertag] then
!		p.ptr:=pcm_allocz(ttlength[usertag]*elemsize)
		p.ptr:=pcm_allocz(ttsize[usertag])
		p.alloc64:=allocbytes/elemsize
	fi

	return p
end

global proc var_getix_array(variant a, int index)=
!a is a list, b is an int; return a[b] into a, which will be on the stack usually
	varrec v
	object p
	int elemtype,length

	v:=a^
	p:=a.objptr

	if v.tag=tvector then
		length:=ttlength[p.usertag]
		index-:=ttlower[p.usertag]
		elemtype:=tttarget[p.usertag]
	else
		length:=p.length
		elemtype:=p.elemtag
		index-:=p.lower
	fi

	if u64(index)>=u64(length) then
		pcerror("ax[int] bounds")
	fi

	if elemtype=tu8 then
		a.tagx:=tint
		a.value:=(p.ptr+index)^
	else
		var_loadpacked(p.ptr+index*ttsize[elemtype],elemtype, a)
	fi

!	var_unshare(&v)
end

global proc var_putix_array(variant a, int index, variant x)=
!a[index]:=x
	varrec v
	object p
	int elemtype, length, lower

	v:=a^
	p:=v.objptr

	if v.tag=tvector then
		length:=ttlength[p.usertag]
		lower:=ttlower[p.usertag]
		elemtype:=tttarget[p.usertag]
	else
		length:=p.length
		lower:=p.lower
		elemtype:=p.elemtag
	fi

	index-:=lower

	if u64(index)>=u64(length) then
		if index<0 then
			pcerror("lwb")
		elsif index=length then
			if v.tag=tvector then
				pcerror("Can't append user type")
			fi
			obj_append_array(p,x)
		else
			pcerror("ax[i]:=x bounds")
		fi
	fi

	if elemtype=tu8 then
		if x.tag<>tint then pcerror("rhs not int") fi
		a.tagx:=tint
		(p.ptr+index)^:=x.value
	else
		var_storepacked(p.ptr+index*ttsize[elemtype],x,elemtype)
	fi
end

global proc var_getixref_array(variant a, int index)=
!a[index]:=x
	varrec v
	object p
	int elemtype, length, lower

	v:=a^
	p:=v.objptr

	if v.tag=tvector then
		length:=ttlength[p.usertag]
		lower:=ttlower[p.usertag]
		elemtype:=tttarget[p.usertag]
	else
		length:=p.length
		lower:=p.lower
		elemtype:=p.elemtag
	fi

	index-:=lower

	if u64(index)>=u64(length) then
		if index<0 then
			pcerror("lwb")
		else
			if u64(index)=u64(length) then
PCERROR("PUTIXREF NEEDS IAPPEND")
!				var_iappendarray(a,nil)
				p:=a.objptr
			else
				pcerror("ax[i]:=x bounds")
			fi
		fi
	fi

	a.tagx:=trefpack
	a.elemtag:=elemtype
	a.ptr:=p.ptr+index*ttsize[elemtype]
end

proc obj_append_array(object a, variant x)=
!do in-place append of b to list a
	int n
	ref byte q

	if a.objtype<>normal_obj then
		pcerror("Can't extend slice")
	fi

	if not a.mutable then
		pcnotmut()
	fi

	n:=a.length+1			!new length

	if n>a.alloc64 then		!need more space
		obj_resize_array(a,n)
	else
		a.length:=n
	fi

	q:=a.ptr+(n-1)*ttsize[a.elemtag]

	var_storepacked(cast(q), x, a.elemtag)
end

global proc var_appendto_array(variant a, x)=
	obj_append_array(a.objptr,x)
end

global proc obj_resize_array(object p,int n)=
	ref byte q
	int elemsize

	elemsize:=ttsize[p.elemtag]

	if n<=p.alloc64 then
		p.length:=n
	else
		q:=pcm_alloc(n*elemsize)
		if p.length then
			memcpy(q,p.ptr,p.length*elemsize)
			pcm_free(p.ptr,p.alloc64*elemsize)
		fi
		p.ptr:=q
		p.length:=n
		p.alloc64:=allocbytes/elemsize
	fi
end

global proc var_dupl_array(variant a)=
	object p,q
	int elemsize

	p:=a.objptr
	q:=obj_newarray(p.elemtag, p.lower, p.length)
	a.objptr:=q

	if p.length then
		memcpy(q.ptr, p.ptr,
			p.length*ttsize[p.elemtag])
	fi
end

global proc var_dupl_vector(variant a)=
	object p,q
	int elemsize,length

	p:=a.objptr
	length:=ttlength[p.usertag]
	q:=obj_newarray_u(p.usertag)
	a.objptr:=q

	if length then
		memcpy(q.ptr, p.ptr,ttsize[p.usertag])
	fi
end

global function var_equal_array(variant a,b)int=
	object p:=a.objptr
	object q:=b.objptr
	int length,elemsize:=p.elemtag

	if p.elemtag<>q.elemtag then
		return 0
	fi
	length:=p.length

	if length<>q.length then
		return 0
	fi
	if length=0 then return 1 fi

	return eqbytes(p.ptr, q.ptr, ttsize[p.elemtag]*length)
end

global proc var_concatto_array(variant a,b)=
!do in-place append of b to array a
!both a,b must be arrays
!a must own its data
	ref byte d
	int n,alen,blen,newlen,oldbytes,newbytes,elemsize
	variant v
	object pa,pb

	pa:=a.objptr
	pb:=b.objptr

	if not pa.mutable then
		pcnotmut()
	fi

	if pa.elemtag<>pb.elemtag then
		pcerror("concat/not compat")
	fi
	elemsize:=ttsize[pa.elemtag]

	alen:=pa.length
	blen:=pb.length

	if alen=0 then					!concat to empty array
		if blen then				!copy b to a (else leave a as empty)
			obj_resize_array(pa,blen)
			d:=pa.ptr
			memcpy(d,pb.ptr,blen*elemsize)
		fi
	elsif blen then					!neither array is empty (else leave a unchanged)
		newlen:=alen+blen
		obj_resize_array(pa,newlen)
		d:=pa.ptr+alen*elemsize
		memcpy(d,pb.ptr,blen*elemsize)
	fi
end

global proc var_getslice_array(variant a, int i,j)=
	int alower,elemsize
	object p,q

	p:=a.objptr

	alower:=p.lower
	elemsize:=ttsize[p.elemtag]

	if i<alower or j>p.length+alower-1 or i>j then
		pcerror("array/slice bounds")
	fi

	q:=obj_new()

	q.objtype:=slice_obj
	q.mutable:=p.mutable
	q.lower:=1
	q.ptr:=p.ptr+(i-alower)*elemsize
	q.elemtag:=p.elemtag

	case p.objtype
	when slice_obj then				!slice of a slice!
		q.objptr2:=p.objptr2		!link to original
		obj_shareu(q.objptr2)

	when extslice_obj then
		q.objptr2:=nil
		q.objtype:=extslice_obj
	else
		q.objptr2:=p				!link to original
		++p.refcount
	esac

	q.length:=j-i+1
	a.objptr:=q
end

!global proc var_putslice_array(variant a, int i,j, variant x)=
!!insert a substring into a
!	ref byte r,s
!	object p,q
!	int length,sublength,elemsize
!
!	if a.tag=tvector then
!		pcerror("userax/putslice")
!	fi
!	p:=a.objptr
!	if not p.mutable then pcnotmut() fi
!	length:=p.length
!
!	if i<1 or j>p.length or i>j then
!		pcerror("array/slice bounds")
!	fi
!
!	sublength:=j-i+1
!
!	q:=x.objptr
!	if q.length<sublength then
!		pcerror("substr too short")
!	fi
!	if p.elemtag<>q.elemtag then
!		pcerror("Not compat")
!	fi
!	elemsize:=ttsize[p.elemtag]
!
!	r:=p.ptr+(i-1)*elemsize
!	s:=q.ptr
!	memcpy(r,s,sublength*elemsize)
!end

function u8inarray(byte a,object p)int=
!look for byte value a within array
!return index of first matching value, or lowerbound-1 (ie. 0 for 1-based arrays)
	int i
	ref byte q

	i:=p.lower
	q:=p.ptr

	to p.length do
		if q^=a then
			return i
		fi
		++q
		++i
	od
	return i64.min
end

global function u16inarray(word16 a,object p)int=
	int i
	ref word16 q

	i:=p.lower
	q:=cast(p.ptr)

	to p.length do
		if q^=a then
			return i
		fi
		++q
		++i
	od
	return i64.min
end

global function u32inarray(word32 a,object p)int=
	int i
	ref word32 q

	i:=p.lower
	q:=cast(p.ptr)

	to p.length do
		if q^=a then
			return i
		fi
		++q
		++i
	od
	return i64.min
end

global function u64inarray(word64 a,object p)int=
	int i
	ref word64 q

	i:=p.lower
	q:=cast(p.ptr)

	to p.length do
		if q^=a then
			return i
		fi
		++q
		++i
	od
	return i64.min
end

global function var_inx_array(variant a,b, int usertag)int n =
!a will be int/real/word
	object q:=b.objptr
	int elemtag

	if usertag then				!vector
		elemtag:=tttarget[usertag]
	else
		elemtag:=q.elemtag
	fi

	case elemtag
	when ti8,tu8 then
		n:=u8inarray(a.value,q)
	when ti16,tu16 then
		n:=u16inarray(a.value,q)
	when ti32,tu32 then
		n:=u32inarray(a.value,q)
	when ti64,tu64 then
		n:=u64inarray(a.value,q)
	else
		pcustype("x in array",b)
	esac
	return n
end

global proc var_expand_array(variant p, dest, int m)=
	ref byte q
	int i,n,elemtype,length
	object pa

	pa:=p.objptr

	if p.tag=tarray then
		length:=pa.length
		elemtype:=pa.elemtag
	else
		length:=ttlength[pa.usertag]
		elemtype:=tttarget[pa.usertag]
	fi

	q:=pa.ptr

	n:=1

	to m do
		if n>length then
			dest.tagx:=tvoid
		else
			var_loadpacked(q,elemtype,dest,nil)
			q+:=ttsize[elemtype]
		fi
		++n
		--dest
	od
end
