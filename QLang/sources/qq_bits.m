global proc obj_free_bits(object p, int tag)=
	if p.length then
		pcm_free(p.ptr, getbitssize(p.alloc64, p.elemtag))
	fi

	pcm_free32(p)
end

global proc var_make_bits(variant a, dest, int lower, n, bxtype, elemtype) =
!create a list of n vars starting from a in reverse order (a is the last)
!put the result in dest (note this will be the last the n vars)
	object p
	ref byte q
	int bitwidthx,offset

	p:=obj_newbits(elemtype,lower,n)
	q:=p.ptr

	bitwidthx:=ttbitwidth[elemtype]
	offset:=0

	to n do
		var_storebit(q,offset,a,elemtype,bitwidthx)
		offset+:=bitwidthx
		if offset>=8 then
			++q
			offset:=0
		fi
		++a
	od

	dest.tagx:=bxtype ior hasrefmask
	dest.objptr:=p
end

global function obj_newbits(int elemtype, lower,length)object p=
!create a packed array with element-type t, given length and lower bound.
!it will be initialised to zeros

	ref byte q
	int nbits,bitwidthx,nbytes

	p:=obj_new()
	p.mutable:=1
	p.lower:=lower
	p.length:=length
	p.objtype:=normal_obj
	p.elemtag:=elemtype

	if length then
		nbytes:=getbitssize(length, elemtype)
		p.ptr:=pcm_allocz(nbytes)
		p.alloc64:=allocbytes*(8/ttbitwidth[elemtype])
	fi

	return p
end

global proc var_getix_bits(variant a, int index)=
!a is a list, b is an int; return a[b] into a, which will be on the stack usually
	object p
	ref byte q
	int elemtype,offset,shift

	p:=a.objptr
	elemtype:=p.elemtag
	index-:=p.lower

	if u64(index)>=u64(p.length) then
		pcerror("ax[int] bounds")
	fi
	q:=p.ptr
	a.tagx:=tint

	index+:=p.indexoffset

	case p.elemtag
	when tu1 then
		a.value:=not not ((q+index>>3)^ iand (1<<(index iand 7)))
	when tu2 then
		shift:=(index iand 3)*2
		a.value:=((q+index>>2)^ iand (3<<shift))>>shift
	when tu4 then
		shift:=(index iand 1)*4
		a.value:=((q+index>>1)^ iand (15<<shift))>>shift
	else
		pcustype_t("bitix",p.elemtag)
	end
end

global proc var_putix_bits(variant a, int index, variant x)=
!a[index]:=x
	object p
	ref byte q
	int elemtype, newoffset

	p:=a.objptr
	elemtype:=p.elemtag

	index-:=p.lower

	if u64(index)>=u64(p.length) then
		if index<0 then
			pcerror("lwb")
		elsif index=p.length then
			obj_append_bits(p,x)
		else
			pcerror("bx[i]:=x bounds")
		fi
	fi

	q:=getindexoffset(p.ptr, p.indexoffset, index, elemtype, newoffset)
	var_storebit(q, newoffset*ttbitwidth[elemtype],x,elemtype,0)
end

global proc var_getixref_bits(variant a, int index)=
!a[index]:=x
	varrec v
	object p
	ref byte q
	int offset, newoffset
	int elemtype

	p:=a.objptr
	elemtype:=p.elemtag
	index-:=p.lower

	if u64(index)>=u64(p.length) then
!
		pcerror("&bx[i] bounds")
	fi

	q:=getindexoffset(p.ptr, p.indexoffset, index, elemtype, newoffset)

	a.tagx:=trefbit
	a.elemtag:=elemtype
	a.ptr:=q
	a.bitoffset:=newoffset*ttbitwidth[elemtype]
end

function getindexoffset(ref byte p, int offset, index, t, &newoffset)ref byte=
!p, with intra-byte offset 'offset', forms a bit pointer to bit-type t
!step it by 'index' elements, and return a new byte-byte, and new offset

	index+:=offset

	case t
	when tu1 then
		p+:=index>>3				!add number of whole bytes
		newoffset:=index iand 7
	when tu2 then
		p+:=index>>2
		newoffset:=index iand 3
	when tu4 then
		index+:=offset>>2
		p+:=index>>1
		newoffset:=index iand 1
	end

	return p
end

proc obj_append_bits(object a, variant x)=
!do in-place append of b to list a
	int n, newoffset, elemtype
	ref byte q

	if a.objtype<>normal_obj then
		pcerror("Can't extend slice")
	fi

	if not a.mutable then
		pcnotmut()
	fi

	n:=a.length+1			!new length
	elemtype:=a.elemtag

	if n>a.alloc64 then		!need more space
		obj_resize_bits(a,n)
	else
		a.length:=n
	fi

	q:=getindexoffset(a.ptr, a.indexoffset, n-a.lower, elemtype, newoffset)
	var_storebit(q,newoffset*ttbitwidth[elemtype],x,elemtype, 0)
end

global proc var_appendto_bits(variant a, x)=
	obj_append_bits(a.objptr,x)
end

global proc obj_resize_bits(object p,int n)=
	ref byte q
	int newsize,elemtype

	elemtype:=p.elemtag

	if n<=p.alloc64 then
		p.length:=n
	else
		newsize:=getbitssize(n,elemtype)
		q:=pcm_alloc(newsize)
		if p.length then
			memcpy(q,p.ptr, bits_bytesize(p))
			pcm_free(p.ptr, getbitssize(p.alloc64, elemtype))
		fi
		p.ptr:=q
		p.length:=n
		p.alloc64:=allocbytes*(8/ttbitwidth[elemtype])
	fi
end

global proc var_dupl_bits(variant a)=
	object p,q
	int elemsize

	p:=a.objptr

	q:=obj_newbits(p.elemtag, p.lower, p.length)
	q.indexoffset:=p.indexoffset

	a.objptr:=q

	if p.length then
		memcpy(q.ptr, p.ptr, bits_bytesize(p))
	fi
end

global function var_equal_bits(variant a,b)int=
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

	return eqbytes(p.ptr, q.ptr, bits_bytesize(p))
end

global proc var_concatto_bits(variant a,b)=
!do in-place append of b to array a
!both a,b must be arrays
!a must own its data
	ref byte d
	int n,alen,blen,newlen,oldbytes,newbytes,elemsize
	variant v
	object pa,pb

PCERROR_S("VAR/BITS/NOT READY",$FUNCTION)
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
!global proc obj_resize_bits(object p,int n)=
			obj_resize_bits(pa,blen)
			d:=pa.ptr
			memcpy(d,pb.ptr,blen*elemsize)
		fi
	elsif blen then					!neither array is empty (else leave a unchanged)
		newlen:=alen+blen
!		array_resize(pa,newlen)
		obj_resize_bits(pa,newlen)
		d:=pa.ptr+alen*elemsize
		memcpy(d,pb.ptr,blen*elemsize)
	fi
end

global proc var_getslice_bits(variant a, int i,j)=
	int alower,elemtype,newoffset
	object p,q

	p:=a.objptr

	alower:=p.lower
	elemtype:=p.elemtag

	if i<alower or j>p.length+alower-1 or i>j then
		pcerror("bits/slice bounds")
	fi

	q:=obj_new()

	q.objtype:=slice_obj
	q.mutable:=p.mutable
	q.lower:=1
	q.elemtag:=elemtype

	q.ptr:=getindexoffset(p.ptr, p.indexoffset, i-alower, elemtype, newoffset)
	q.indexoffset:=newoffset

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

!global proc var_putslice_bits(variant a, int i,j, variant x)=
!!insert a substring into a
!	ref byte pp,qq
!	object p,q
!	int length,sublength,elemtype,offsetp,offsetq,bitwidthx
!	varrec v
!
!	p:=a.objptr
!	if not p.mutable then pcnotmut() fi
!	length:=p.length
!	elemtype:=p.elemtag
!
!	if i<1 or j>p.length or i>j then
!		pcerror("bits/slice bounds")
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
!
!	bitwidthx:=ttbitwidth[elemtype]
!
!	pp:=getindexoffset(p.ptr, p.indexoffset, i-p.lower, elemtype, offsetp)
!	qq:=getindexoffset(q.ptr, q.indexoffset, 0, elemtype, offsetq)
!	offsetq*:=bitwidthx
!	offsetq*:=bitwidthx
!
!	to sublength do
!		var_loadbit(qq, offsetq, elemtype,0, &v)
!		var_storebit(pp, offsetp, &v, elemtype,0)
!		offsetp+:=bitwidthx
!		if offsetp>=8 then ++pp; offsetp:=0 fi
!		offsetq+:=bitwidthx
!		if offsetq>=8 then ++qq; offsetq:=0 fi
!	od	
!end

global function bits_bytesize(object p)int=
!return how many bytes are used by the object
!should be bits, but set should work; also array?

	return getbitssize(p.length, p.elemtag)
end

global function getbitssize(int n, t)int=
!return bytesize of n bit elements of type t
!will round to whole number of 64-bit values, but expressed as bytes
	int nbits:=n*ttbitwidth[t]
	return ((nbits-1)/64+1)*8			!bytes required in 64-bit blocks
end
