global proc var_make_record(variant a, dest, int n, rectype) =
!create a list of n vars starting from a in reverse order (a is the last)
!put the result in dest (note this will be the last the n vars)
	object p
	variant b
	int m

	p:=obj_new_record(rectype,nil)

	b:=p.varptr

	m:=ttlength[rectype]

	if n<m then
		pcerror("Too few elements")
	elsif n>m then
		println =n,=m
		pcerror("Too many elements")
	fi

	to n do
		b^:=a^				!assume list initialised to void
		++a
		++b
	od

	dest.tagx:=trecord ior hasrefmask
	p.usertag:=rectype

!CPL "MAKEREC",TTNAME[RECTYPE],TTNAME[TTBASETYPE[RECTYPE]]

	dest.objptr:=p
end

global function obj_new_record(int m, variant defval)object p=
	variant a
	int n

	p:=obj_new()
	p.mutable:=1
!	p.lower32:=1
	n:=ttlength[m]
!	p.length:=n
	p.objtype:=normal_obj

	if n then
		p.varptr:=a:=pcm_alloc(n*varrec.bytes)

!CPL "NEWREC",TTNAME[M],=DEFVAL
!IF DEFVAL THEN CPL =TTNAME[DEFVAL.TAG],=DEFVAL.VALUE FI

		if defval and defval.tag<>tvoid then
			a:=p.varptr
			to n do
				a^:=defval^
				var_share(a)
				++a
			od
		else
			to n do
				a.tagx:=tint
				a.value:=0
				++a
			od
		fi
	fi

	return p
end

global proc obj_free_record(object p)=
	variant q

	q:=p.varptr
	to p.length do
		var_unshare(q)
		++q
	od
	if p.length then
		pcm_free(p.varptr,p.length*varrec.bytes)
	fi

	pcm_free32(p)
end

global proc var_dupl_record(variant a)=
	object p,q
	variant plist, qlist
	int length

	p:=a.objptr
	q:=obj_new()
	q^:=p^
	q.refcount:=1
	q.mutable:=1

	a.objptr:=q
	length:=ttlength[p.usertag]

	if length=0 then return fi

	qlist:=q.varptr:=pcm_alloc(length*varrec.bytes)
	plist:=p.varptr

	to length do
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

global function var_equal_record(variant x,y)int =
!return 1 if x and y are of same records with identical field values, else 0
	int xlen,ylen,res
	object px,py
	variant a,b


	px:=x.objptr
	py:=y.objptr
	if px.usertag<>py.usertag then return 0 fi

	if px=py then
		return 1
	fi

	a:=px.varptr
	b:=py.varptr

	to ttlength[px.usertag] do
		if var_equal(a,b)=0 then return 0 fi	!at least one mismatch
		++a
		++b

	od

	return 1
end

global proc var_getix_record(variant a, int index)=
!put result into a (which will be on the stack)
	object q
	word offset

	q:=a.objptr

	offset:=index-1
	if offset>=word(ttlength[q.usertag]) then
		pcerror("record[int] bounds")
	fi

	a^:=(q.varptr+offset)^
	var_share(a)
end

global proc var_putix_record(variant a, int index, variant x)=
	variant dest
	object q
	word offset

	q:=a.objptr

	if not q.mutable then pcnotmut() fi

	offset:=index-1
	if offset>=word(ttlength[q.usertag]) then
		pcerror("rec[int] bounds")
	fi

	dest:=q.varptr+offset
	var_unshare(dest)
	dest^:=x^				!xfer ownership	
!	var_share(dest)
end

global proc var_getixref_record(variant a, int index, variant dest)=
	variant p
	object q
	word offset

	q:=a.objptr

	offset:=index-1
	if offset>=word(q.length) then
		pcerror("^rec[int] bounds")
	fi

	p:=q.varptr+offset

	dest.tagx:=trefvar
	dest.varptr:=p
end

