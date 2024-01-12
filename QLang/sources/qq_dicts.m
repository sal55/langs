global proc var_make_dict(variant a, dest, int n) =
!create a list of n vars starting from a in reverse order (a is the last)
!put the result in dest (note this will be the last/first of the n vars)
	object p
	variant b
	varrec v

	p:=obj_new_dict(n)

!CPL "NEWDICT",N,P.LENGTH
	b:=p.varptr
	v.tagx:=tdict+hasrefmask
	v.objptr:=p

	to n do
		adddictitem(&v,a,a+1)
		a+:=2
	od
	p.dictitems:=n

	dest^:=v
end

global function obj_new_dict(int n)object p=
	int m

	m:=max(16,nextpoweroftwo(n*2))		!list has 2n entries, min 16, rounded up to 2**x

	p:=obj_newlist(m,1,nil)
	p.dictitems:=0

	return p
end

global proc obj_free_dict(object p,int internal=0)=
!internal=1 means called from expanddict; free only elements, not descriptor
	variant q
	varrec v

	q:=p.varptr
	to p.length do
		var_unshare(q)
		++q
	od
	if p.length then
		pcm_free(p.varptr,p.alloc32*varrec.bytes)
	fi

	if not internal then
		pcm_free32(p)
	fi
end

global proc var_dupl_dict(variant a)=
	object p,q
	variant plist, qlist

	p:=a.objptr
	q:=obj_new_dict(p.dictitems)
	q^:=p^
	q.refcount:=1
	q.mutable:=1

	a.objptr:=q

	if q.length=0 then return fi

	qlist:=q.varptr:=pcm_alloc(p.length*varrec.bytes)
	q.alloc32:=allocbytes/varrec.bytes	!probably same as p.alloc32	
	plist:=p.varptr

	to q.length do
		qlist^:=plist^
		var_dupl(qlist)
		++qlist
		++plist
	od
end

global function var_equal_dict(variant x,y)int =
!return 1 if lists in x,y are equal, otherwise 0
	int xlen,ylen,res
	object px,py
	variant a,b

PCERROR("EQUALDICT")
!	px:=x.objptr
!	py:=y.objptr
!
!	xlen:=px.length
!	ylen:=py.length
!
!	if xlen<>ylen then return 0 fi		!unequal lengths
!
!	if xlen=0 then return 1 fi			!both empty
!
!	a:=px.varptr
!	b:=py.varptr
!
!	to xlen do
!		if var_equal(a,b)=0 then return 0 fi	!at least one mismatch
!		++a
!		++b
!
!	od
!
	return 1
end

global function var_finddictitem(variant vd, variant p,int doins)variant=
!look for key p in dict d
!when key is found:    will return a pointer to the value
!when key not found:
!   doins=1:     Will insert the key and a void value, and return a pointer to the value
!   doins=0:     Will return nil

	int hash,index,size,keytag,wrapped,limit
	int64 keyvalue
	variant q
	object pa,qa,d

	retry:
	d:=vd.objptr

	size:=d.length/2

	index:=(var_gethashvalue(p) iand (size-1))		!0-based index

	q:=d.varptr+index*2							!point to key of key/value pair
	wrapped:=0
	keytag:=p.tag
	keyvalue:=p.value							!when int
	pa:=p.objptr								!when string

	do
		if q.tag=tvoid then					!unused entry; not found
			exit

		elsif q.tag=keytag then
			case keytag
			when tint,treal,trange then
				if q.value=keyvalue then
					++q
					var_share(q)
					return q
				fi
			when tstring then
				qa:=q.objptr
				if pa.length=qa.length then	!match on length at least
					if memcmp(pa.strptr,qa.strptr,pa.length)=0 then
						++q
						var_share(q)
						return q
					fi
				fi
			esac
		fi

!no match
		++index
		q+:=2
		if index>=size then
			if wrapped then					!shouldn't happen if dict was properly expanded
				pcerror("DICT FULL?")
			fi
			wrapped:=1
			index:=0
			q:=d.varptr
		fi
	od

!exit when not found
	if doins then
		limit:=size*3/4
		if d.dictitems>=limit then
			expanddict(vd)
			goto retry
		fi
		q^:=p^
		var_share(q)
		++(d.dictitems)
		return q+1							!point to entry; leave value as void
	else
		return nil
	fi
end

proc expanddict(variant vd)=
!double the size of the dict
	int n,m,i,j,k,oldrefcount
	object d,e
	objrec temp
	variant p,q,r
	varrec ev
	static byte inuse

	if inuse then
		pcerror("expanddict?")
	fi
	inuse:=1

	d:=vd.objptr

	n:=d.alloc32			!nos of keys and values (all slots)
	m:=n/2					!number of dict slots

	p:=d.varptr							!old data

	e:=obj_new_dict(m*2)
!	e:=obj_new_dict(m*4)

	var_objtovar(tdict,e,&ev)

	q:=p

	for i:=1 to m do
		if q.tag<>tvoid then
			r:=var_finddictitem(&ev,q,1)
			++q
			r^:=q++^					!transfer ownership of data
VAR_SHARE(R)

		else
			q+:=2
		fi
	od

	obj_free_dict(d,1)				!get rid of old dict
	oldrefcount:=d.refcount
	d^:=e^							!use new dict, but at same address

	pcm_free(e,e.bytes)

	d.refcount:=oldrefcount

	inuse:=0
end

proc adddictitem(variant d, p, q)=
!d is a dict, p:q are akey:value pair to be added to it
	object da
	variant r

	da:=d.objptr

	if da.length=0 then				!cannot be empty
		pcerror("NULL DICT")
	fi

	r:=var_finddictitem(d,p,1)

	var_share(q)
	var_unshare(r)			!overwrite any existing value
	r^:=q^
end
