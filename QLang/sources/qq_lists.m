global object emptylist

proc start=
	emptylist:=obj_new()
	emptylist.lower16:=1
	emptylist.objtype:=normal_obj
end

global proc var_empty_list(int lower, variant dest) =
	object p
	dest.objptr:=obj_newlist(0, lower)
	dest.tagx:=tlist ior hasrefmask
end

global proc var_make_list(variant a, dest, int n, lower) =
!create a list of n vars starting from a in reverse order (a is the last)
!put the result in dest (note this will be the last/first of the n vars)
	object p
	variant b

	p:=obj_newlist(n, lower)

	b:=p.varptr

	if n and a then
		to n do
			b^:=a^				!assume list initialised to void
			++a
			++b
		od
	fi							!else leave as voids if not empty

	dest.tagx:=tlist ior hasrefmask
	dest.objptr:=p
end

global function obj_newlist(int n, lower, variant defval=nil)object p=
	variant a

	p:=obj_new()
	p.mutable:=1
	if lower not in -32768..32767 then pcerror("List LWB not 16-bit") fi
	p.lower16:=lower
	p.length:=n
	p.objtype:=normal_obj

	if n then
		p.varptr:=a:=pcm_alloc(n*varrec.bytes)
		p.alloc64:=allocbytes/varrec.bytes

		if defval and defval.tag<>tvoid then
			to n do
				var_share(defval)
				a^:=defval^
				++a
			od
		else
			to n do
				a.tagx:=tvoid
				++a
			od
		fi
	fi

	return p
end

global proc obj_free_list(object p)=
	variant q
	varrec v

	q:=p.varptr

	to p.length do
		var_unshare(q)
		++q
	od
	if p.length then
		pcm_free(p.varptr,p.alloc64*varrec.bytes)
	fi

	pcm_free32(p)
end

global proc var_getix_list(variant a, int index)=
!put result into a (which will be on the stack)
	variant p
	object q
	word offset
	int lower

	q:=a.objptr

	lower:=q.lower16

	offset:=index-lower
	if offset>=word(q.length) then
		pcerror("getlist[int] bounds")
	fi

	a^:=(q.varptr+offset)^
	var_share(a)
end

global proc var_getslice_list(variant a, int i,j)=
	varrec v,v2
	int alower
	object p,q

	p:=a.objptr

!CPL "LIST SLICE",=I,=J,=P.ULIST.LENGTH,=P.ULIST.VARPTR

	alower:=p.lower16

	if i<alower or j>p.length+alower-1 or i>j then
		pcerror("list/slice bounds")
	fi

	q:=obj_new()

	v.objptr:=q
	q.objtype:=slice_obj
	q.mutable:=p.mutable
	q.lower16:=1
	q.varptr:=p.varptr+i-alower

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
!		var_shareu(a)
	esac

	q.length:=j-i+1
	a.objptr:=q
!	var_unshare(a)
end

global proc var_getixref_list(variant a, int index)=
	variant p
	object q
	word offset
	varrec v

	q:=a.objptr

	offset:=index-q.lower16

	if offset>=word(q.length) then
		if int(offset)<0 then
			pcerror("&list[int] lwb")
		elsif offset=q.length then
			if q.objtype<>normal_obj then pcerror("Can't extend slice/ext") fi
			v.tagx:=tvoid
			obj_append_list(q,&v)
!			return
		else
			pcerror("putlist[int] bounds")
		fi
	fi

	p:=q.varptr+offset
!	var_unshare(a)			!a should not disappear; rvalues can't have & applied

	a.tagx:=trefvar
	a.varptr:=p
end

global proc var_putix_list(variant a, int index, variant x)=
	variant dest
	object q
	word offset
	int lower

	q:=a.objptr

	if not q.mutable then pcnotmut() fi

	offset:=index-q.lower16

	if offset>=word(q.length) then
		if int(offset)<0 then
			pcerror("putlist[int] lwb")
		elsif offset=q.length then
			if q.objtype<>normal_obj then pcerror("Can't extend slice/ext") fi
			obj_append_list(q,x)
			return
		else
			pcerror("putlist[int] bounds")
		fi
	fi

	dest:=q.varptr+offset
	var_unshare(dest)
	dest^:=x^				!xfer ownership	
end

global proc var_putslice_list(variant a, int i,j, variant x)=
!insert a substring into a
	variant r,s
	object p,q
	int length,sublength

	p:=a.objptr
	if not p.mutable then pcnotmut() fi
	length:=p.length

	if i<1 or j>p.length or i>j then
		pcerror("list/slice bounds")
	fi
	sublength:=j-i+1

	q:=x.objptr
	if q.length<sublength then
		pcerror("substr too short")
	fi

	r:=p.varptr+i-1
	s:=q.varptr
	to sublength do
		r^:=s^
		var_share(r)
		++r
		++s
	od

end

proc obj_append_list(object a, variant x)=
!do in-place append of b to list a
	int n

	if a.objtype<>normal_obj then
		pcerror("Can't extend slice")
	fi

	if not a.mutable then
		pcnotmut()
	fi

	n:=a.length+1			!new length

	if n>a.alloc64 then		!need more space
		obj_resize_list(a,n)
	else
		a.length:=n
	fi

	if x then
		(a.varptr+n-1)^:=x^		!transfers ownership
	fi

end

global proc obj_resize_list(object p,int n)=
	variant q
	word32 allocated

	if n<=p.alloc64 then
		p.length:=n
	else
		q:=pcm_alloc(n*varrec.bytes)
!		p.alloc64:=allocbytes/varrec.bytes
		allocated:=allocbytes/varrec.bytes
		if p.length then
			memcpy(q,p.varptr,p.length*varsize)
			pcm_free(p.varptr, p.alloc64*varsize)
		fi
		p.varptr:=q
		p.length:=n
		p.alloc64:=allocated
	fi
end

global proc var_appendto_list(variant a, x)=
!a is a list (was a pointer)
	obj_append_list(a.objptr,x)
end

global proc var_dupl_list(variant a)=
	object p,q
	variant plist, qlist

	p:=a.objptr
	q:=obj_new()
	q^:=p^
	q.refcount:=1
	q.mutable:=1
	q.objtype:=normal_obj

	a.objptr:=q

	if q.length=0 then return fi

	qlist:=q.varptr:=pcm_alloc(p.length*varrec.bytes)
	q.alloc64:=allocbytes/varrec.bytes	!probably same as p.alloc64	
	plist:=p.varptr

	to q.length do
		qlist^:=plist^
		if qlist.tag=trecord then
			var_share(qlist)
		else
			var_dupl(qlist)
		fi
		++qlist
		++plist
	od
end

global proc var_mul_list(variant p, int m)=
	int oldlength, newlength, n
	object q:=p.objptr, r
	variant a,b

	oldlength:=q.length
	newlength:=oldlength*m

	if oldlength=0 then return fi

	if newlength<0 then
		pcerror("list*int <0")
	elsif newlength=0 then
		p.objptr:=obj_newlist(0,q.lower16)
		return
	fi

	r:=obj_newlist(newlength, q.lower16)
	a:=r.varptr
	b:=q.varptr
	n:=0

	to newlength do
		a^:=b^
		var_share(a)
		++a
		if oldlength>1 then
			++b
			if ++n=oldlength then
				b:=q.varptr
				n:=0
			fi
		fi
	od

	p.objptr:=r
end

global function var_equal_list(variant x,y)int =
!return 1 if lists in x,y are equal, otherwise 0
	int xlen,ylen,res
	object px,py
	variant a,b

	px:=x.objptr
	py:=y.objptr

	if px=py then return 1 fi			!same object

	xlen:=px.length
	ylen:=py.length

	if xlen<>ylen then return 0 fi		!unequal lengths

	if xlen=0 then return 1 fi			!both empty

	a:=px.varptr
	b:=py.varptr

	to xlen do
		if var_equal(a,b)=0 then return 0 fi	!at least one mismatch
		++a
		++b

	od

	return 1
end

global proc var_concatto_list(variant a,b)=
!do in-place append of b to list a
!both a,b must be lists
!a must own its data
	variant newptr,c,d
	int n,alen,blen,newlen,oldbytes,newbytes
	variant v
	object pa,pb

	pa:=a.objptr

	if not pa.mutable then
		pcnotmut()
	fi

	pb:=b.objptr

	alen:=pa.length
	blen:=pb.length

	if alen=0 then					!concat to empty list
		if blen then				!copy b to a (else leave a as empty)
!global proc obj_resize_list(object p,int n)=
			obj_resize_list(pa,blen)
			d:=pa.varptr
			memcpy(d,pb.varptr,blen*varsize)
			to blen do
				var_share(d)
				++d
			od
		fi
	elsif blen then					!neither list is empty (else leave a unchanged)
		newlen:=alen+blen
!		list_resize(pa,newlen)
		obj_resize_list(pa,newlen)
		d:=pa.varptr+alen
		memcpy(d,pb.varptr,blen*varsize)
		to blen do
			var_share(d)
			++d
		od
	fi
end

global function var_inx_list(variant a,b)int =
	int n:=b.objptr.length
	int lowerm1:=b.objptr.lower16-1
	variant x:=b.objptr.varptr

	for i to n do
		if var_equal(a,x)=1 then
			return i+lowerm1
		fi
		++x
	od
	return i64.min
end
