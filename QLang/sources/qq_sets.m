global proc obj_free_set(object p)=
	if p.length then
		pcm_free(p.ptr, getbitssize(p.alloc64, tu1))
	fi
	pcm_free32(p)
end

global proc var_dupl_set(variant a)=
	object p:=a.objptr
	object q
	int nbytes, nbits:=p.length

	q:=obj_newset(nbits)	

	if nbits then
		memcpy(q.ptr, p.ptr, getbitssize(nbits, tu1))
	fi

	a.objptr:=q
end

global function var_equal_set(variant x,y)int=
	int xbytes:=getsetbytes(x)
	int ybytes:=getsetbytes(y)
	if xbytes<>ybytes then return 0 fi

	return eqbytes(x.objptr.ptr, y.objptr.ptr, xbytes)
end

function getsetbytes(variant x)int=
	int nbits:=x.objptr.length
	if nbits then
		if nbits iand 7 then
			return nbits/8+1
		else
			return nbits/8
		fi
	else
		return 0
	fi
end

global proc var_make_set(variant data, dest, int n) =
! data points to n vars in a block (on the stack, but the caller takes care of that)
! These will be in reverse order, but it doesn't matter for sets.
! dest points to the place to put the resulting set.
! Note: dest will likely correspond to the last data element, so do not override until done.

	variant q
	ref byte p
	int top,a,b,i,j,t,size
	byte alloc
	object s
	static int count=0

	if n=0 then
		var_emptyset(dest)
		return
	fi

!First scan to work out size of set
	top:=0
	q:=data

	to n do
		switch q.tag		!scan items, which should be ranges or integers
		when trange then

			a:=q.range_lower
			b:=q.range_upper
		when tint then
			a:=q.value
			if a<0 then
				a:=-a-1
				if a>top then
					top:=a
				fi
				nextloop
			fi
			b:=a
		
		else			!assume numeric value of some sort
			b:=a:=var_getintvalue(q)

		end switch
		if a<0 or b<0 then
			pcerror("Neg range element")
		fi

		top max:=a
		top max:=b
		++q
	od

!CPL "MS1",=N,TOP
	s:=obj_newset(top+1)
!CPL "MS2",N

!Second scan to store elements
	q:=data
	to n do
!CPL "LOOP"
		switch q.tag
		when trange then
			a:=q.range_lower
			b:=q.range_upper
			if a>b then
				swap(a,b)
!				t:=a; a:=b; b:=t
			fi

		when tint then
			b:=a:=q.value
			if a<0 then nextloop end		!ignore set-length control
		else
			b:=a:=var_getintvalue(q)
		end switch

!CPL "LOOP",A,B
!		for j:=a to b do
!			setelem(cast(s.ptr),j)
!		od
		setelemblock(cast(s.ptr),a,b)
		++q
	od
!CPL "MS3"

	var_objtovar(tset,s,dest)
end

global function obj_newset(int length)object p=
!create a packed array with element-type t, given length and lower bound.
!it will be initialised to zeros

	ref byte q
	int nbits,nbytes

	p:=obj_new()
	p.mutable:=1
	p.length:=length

	nbytes := ((length-1)/64+1)*8		!bytes required in 64-bit blocks

	if length then
		p.ptr := pcm_alloc(nbytes)              !(turns total allocated in 'allocbytes')
		p.alloc64:=word64(allocbytes)*8
		pcm_clearmem(p.ptr,allocbytes)
	else
		p.ptr:=nil
	fi

	return p
end

global proc var_emptyset(variant dest)=
	var_objtovar(tset,obj_newset(0),dest)
end

global proc var_getix_set(variant a, int index)=
!a is a list, b is an int; return a[b] into a, which will be on the stack usually
	object p

	p:=a.objptr

	if u64(index)>=u64(p.length) then
		pcerror("set[int] bounds")
	fi

	a.tagx:=tint
	a.value:=not not ((p.ptr+index>>3)^ iand (1<<(index iand 7)))
end

global proc var_putix_set(variant a, int index, variant x)=
!a[index]:=x
	object p
	ref byte q
	int newoffset

	p:=a.objptr
	if not p.mutable then pcnotmut() fi

	if u64(index)>=u64(p.length) then
		if index<0 then
			pcerror("lwb")
		else
			pcerror("set[i]:=x bounds")
		fi
	fi

	q:=getoffset(p.ptr, index, newoffset)

	var_storebit(q, newoffset,x,tu1,0)
end

global proc var_getixref_set(variant a, int index)=
!a[index]:=x
	varrec v
	object p
	ref byte q
	int offset, newoffset

	p:=a.objptr
	if not p.mutable then pcnotmut() fi

	if u64(index)>=u64(p.length) then
		pcerror("&set[i] bounds")
	fi

	q:=getoffset(p.ptr, index,  newoffset)

	a.tagx:=trefbit
	a.elemtag:=tu1
	a.ptr:=q
	a.bitoffset:=newoffset
end

function getoffset(ref byte p, int index, &newoffset)ref byte=
!p, with intra-byte offset 'offset', forms a bit pointer to bit-type t
!step it by 'index' elements, and return a new byte-byte, and new offset

	p+:=index>>3				!add number of whole bytes
	newoffset:=index iand 7

	return p
end

global function var_in_set(variant a,b)int =
	int i:=a.value,m
	static [0:]byte masks=(1,2,4,8,16,32,64,128)
	object p

	p:=b.objptr

	if u64(i)>=u64(p.length) then
		return 0
	fi

	if	(p.ptr+i>>3)^ iand masks[i iand 7] then
		return 1
	else
		return 0
	fi
end

global proc iresizeset(variant p,int n)=
!make sure set x has at least n elements, extending as needed
!this is done in-place, so caller must ensure p can be modified
!x should also be a cc_owner type (as it makes use of .alloc)
	object pp

	pp:=p.objptr

	if pp.length>=n then		!already large enough
		return
	fi

	obj_resize_set(pp,n)
end

global proc obj_resize_set(object p,int n)=
	ref byte q
	int newsize,elemtype

	elemtype:=p.elemtag

	if n<=p.alloc64 then
		p.length:=n
	else
!CPL "RESIZE"
		newsize:=getbitssize(n,tu1)
		q:=pcm_allocz(newsize)
		if p.length then
			memcpy(q,p.ptr, getbitssize(p.length,tu1))
			pcm_free(p.ptr, getbitssize(p.alloc64, tu1))
		fi
		p.ptr:=q
		p.length:=n
		p.alloc64:=allocbytes*8
	fi
end

global proc iorsetbits(ref int p,q,int n)=
	to (n-1)/64+1 do
		p++^ ior:= q++^
	od
end

global proc ixorsetbits(ref int p,q,int n)=
	to (n-1)/64+1 do
		p++^ ixor:= q++^
	od
end

global proc iandsetbits(ref word p,q,int n)=
	to (n-1)/64+1 do
		p++^ iand:= q++^
	od
end

global proc inotsetbits(ref word p,int n)=
	to (n-1)/64+1 do
		p^ :=inot p^
		++p
	od
end

global proc var_iorto_set(variant x,y) =
!x,y are on the stack, and usually dest coincides with x
!add/ior set x to y
	int xlen,ylen
	int n,i
	ref int p
	object px,py
	ref byte pp

	px:=x.objptr
	py:=y.objptr

	xlen:=px.length
	ylen:=py.length

	if ylen=0 then			!return x unchanged
	elsif xlen=0 then		!return y
		x^:=y^
		var_dupl_set(x)
	else
		px:=x.objptr

		iresizeset(x,ylen)		!make sure x is at least as big as y

		iorsetbits(cast(px.ptr),cast(py.ptr),ylen)

!		var_unshare(y)
	fi
end

global proc var_iandto_set(variant x,y) =
!x,y are on the stack, and usually dest coincides with x
!add/ior set x to y
	int xlen,ylen
	int n,i
	ref int p
	object px,py
	ref byte pp

	px:=x.objptr
	py:=y.objptr

	xlen:=px.length
	ylen:=py.length

	if ylen=0 then				!return empty set
		var_emptyset(x)
	elsif xlen=0 then			!return x unchanged
	else						!x iand:= y
		px:=x.objptr

		iresizeset(x,ylen)		!make sure x is at least as big as y

		iandsetbits(cast(px.ptr),cast(py.ptr),ylen)
	fi
end

global proc var_ixorto_set(variant x,y) =
!x,y are on the stack, and usually dest coincides with x
!add/ior set x to y
	int xlen,ylen
	int n,i
	ref int p
	object px,py
	ref byte pp

	px:=x.objptr
	py:=y.objptr

	xlen:=px.length
	ylen:=py.length

	if ylen=0 then				!return x unchanged
		var_emptyset(x)
	elsif xlen=0 then			!return y
		x^:=y^
		var_dupl_set(x)
	else						!x iand:= y
		px:=x.objptr

		iresizeset(x,ylen)		!make sure x is at least as big as y

		ixorsetbits(cast(px.ptr),cast(py.ptr),ylen)
	fi
end

global proc var_inotto_set(variant x) =
!inot:=x
	int xlen
	object px,py

	px:=x.objptr

	xlen:=px.length

	if xlen then				!lease return x unchanged as []
		inotsetbits(cast(px.ptr),xlen)
	fi
end

