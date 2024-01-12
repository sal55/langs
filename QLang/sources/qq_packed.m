global proc var_loadpacked(ref void p,int t,variant dest, object ownerobj=nil) =
! p is a direct pointer to a packed type of type t.
! Extract target and store in varrec dest, which should have been freed.
!ownerobj is nil, or points to an array obj of which an element is being accessed
!this is mainly for arrays of structs
	int length
	variant q,r
	ref int pp
	object s
	ref char ss

	dest.tagx:=tint

	switch ttbasetype[t]
	when ti8 then
!		dest.tagx:=tint
		dest.value:=ref i8(p)^

	when ti16 then
!		dest.tagx:=tint
		dest.value:=ref i16(p)^

	when ti32 then
!		dest.tagx:=tint
		dest.value:=ref int32(p)^

	when ti64 then
!		dest.tagx:=tint
		dest.value:=ref i64(p)^

	when tu8 then
!		dest.tagx:=tint
		dest.value:=ref byte(p)^

	when tu16 then
!		dest.tagx:=tint
		dest.value:=ref u16(p)^

	when tu32 then
!		dest.tagx:=tint		!BETTER I64
		dest.value:=ref u32(p)^

	when tu64 then
		dest.tagx:=tint
		dest.uvalue:=ref u64(p)^

	when tr64 then
		dest.tagx:=treal
		dest.xvalue:=ref r64(p)^

	when tr32 then
		dest.tagx:=treal
		dest.xvalue:=ref r32(p)^

	when tpackstrc then
		dest.tagx:=tstring ior hasrefmask
		length:=ttlength[t]
		if length>=2 then		!normal fixed string
			length:=getfslength(p,length)
		else				!assume string basetype: char target (length can be 0)
			length:=1
		fi
!		s:=make_strslicexobj(p,length)
		s:=obj_make_strslicexobj(p,length)
		dest.objptr:=s

	when tpackstrz then		!zero-terminated string
		dest.tagx:=tstring ior hasrefmask
		ss:=p
		to ttlength[t] do
			exit when ss^=0
			++ss
		od

		s:=obj_make_strslicexobj(p,ss-ref char(p))
		dest.objptr:=s

	elsecase ttbasetype[t]
	when trefpack then
		dest.tagx:=trefpack
		dest.ptr:=cast(ref i64(p)^)
		dest.elemtag:=tttarget[t]

	when tstruct then
		s:=obj_new()
		s.mutable:=1
		s.ptr:=p
!	dostruct:
		dest.objptr:=s
		dest.tagx:=tstruct ior hasrefmask
		s.usertag:=t
		if ownerobj then
			s.objtype:=slice_obj
			s.objptr2:=ownerobj
			++ownerobj.refcount
		else
			s.objtype:=extslice_obj
		fi
	when tvector then
!global function obj_newarray(int elemtype, lower,length)object p=
		s:=obj_newarray(tttarget[t],ttlower[t],ttlength[t])
		s.mutable:=1
		s.ptr:=p
		dest.objptr:=s
		dest.tagx:=tvector ior hasrefmask
		s.usertag:=t
		if ownerobj then
			s.objtype:=slice_obj
			s.objptr2:=ownerobj
			++ownerobj.refcount
		else
			s.objtype:=extslice_obj
		fi
	else
		pcmxtypestt("loadpacked",ttbasetype[t],t)
	end switch
end

global proc var_storepacked(ref byte p,variant q,int t) =
!p points directly to a packed value of type t, which is to receive a value currently
!in variant q

	int plength,qlength
	int s,sbase,tbase
	object qa

	s:=sbase:=q.tag		!storing coercible sbase type to fixed type tbase
	tbase:=ttbasetype[t]

	switch sbase
	when tint, trefpack then
		switch tbase
!		switch t
		when ti8,tu8 then
			(ref byte(p)^):=q.value
			return
		when ti16,tu16 then
			(ref u16(p)^):=q.value
			return
		when ti32,tu32 then
			(ref int32(p)^):=q.value
			return
		when ti64,tu64,trefpack then
			(ref i64(p)^):=q.value
			return
		when tr32 then
			(ref r32(p)^):=q.value
			return
		when tr64 then
			(ref r64(p)^):=q.value
			return
		end switch

	when treal then
		switch tbase
		when ti32,tu32 then
			(ref int32(p)^):=q.xvalue
			return
		when ti64,tu64 then
			(ref int64(p)^):=q.xvalue
			return
		when tr32 then
		(ref r32(p)^):=q.xvalue
			return
		when tr64 then
			(ref r64(p)^):=q.xvalue
			return
		when ti16,tu16 then
			(ref int16(p)^):=q.xvalue
			return
		end switch

	when tstring then
		qa:=q.objptr
		plength:=ttlength[t]
		qlength:=qa.length
		switch tbase
!		when tstring then			!ref string assumed here to mean special 1-char string
!			if t=tbase then			!if basetype, then means special 1-char string
!				if qlength<>1 then
!					pcerror("Str not len 1")
!				fi
!				(ref char(p)^):=ref char(qa.strptr)^
!				return
!			fi
!			if qlength>plength then		!truncate
!				qlength:=plength
!			fi
!			memcpy(p,qa.strptr,qlength)		!copy the number of chars provided
!			setfslength(cast(p),plength,qlength)
!			return
!
		when tpackstrc then			!ref string assumed here to mean special 1-char string
			if t=tbase then			!if basetype, then means special 1-char string
				if qlength<>1 then
					pcerror("Str not len 1")
				fi
				(ref char(p)^):=ref char(qa.strptr)^
				return
			fi
			if qlength>plength then		!truncate
				qlength:=plength
			fi
			memcpy(p,qa.strptr,qlength)		!copy the number of chars provided
			setfslength(cast(p),plength,qlength)
			return

		when tpackstrz then
			if qlength>=plength then			!truncate as needed; no teminator to allow space for terminator
				memcpy(p,qa.strptr,plength)		!copy the number of chars provided
				(ref byte(p)+plength-1)^:=0			!zero terminator

			else
				memcpy(p,qa.strptr,qlength)		!copy the number of chars provided
				(ref byte(p)+qlength)^:=0			!zero terminator
			fi

			return

		end switch

	when tstruct then
		s:=q.objptr.usertag
		if s<>t then
			pcmxtypestt("spack struct",s,t)
		fi
		memcpy(p,q.objptr.ptr,ttsize[t])
		return

	when tvector then
		s:=q.objptr.usertag
		if s<>t then				!not direct match: check whether compatible
				pcmxtypestt("spack array",s,t)
		fi
		memcpy(p,q.objptr.ptr,ttsize[t])
		return

	end switch

	pcmxtypestt("storepacked (source->dest)",s,t)
end

proc setfslength(ref char s,int m,n) =		!SETFSLENGTH
!set up lengthcode of fixed string starting at s, of maximum length m, with actual length n
!a,b are the last two chars of the fixed string:
!a b
!0,N	Length is N
!0,0	Length is 0 (special case of 0,N)
!X,0	Length is M-1
!X,Y	Length is M
!NOTE: this only works up for m in 2..256, and the string can't contain zero bytes

	if m=n then		!no length needed (x,y)
	elsif n=m-1 then	!n=m-1, use (x,0)
		(s+m-1)^:=0
	else			!n<=m-2, so encode length at end of string (0,0) or (0,n)
		(s+m-2)^:=0		!
		(s+m-1)^:=n		!store count n (n can be zero)
	fi
end

global function getfslength(ref char s,int m)int =		!GETFSLENGTH
!s points to a packed string encoded with length at it's end. m is the max length (m>=2)
!return the actual encoded length (see setfslength for encoding scheme)
	s+:=m-1			!point to last char

	if (s-1)^=0 then		!(0,n) length is n
		return s^
	elsif s^=0 then		!(x,0) length is m-1
		return m-1
	else				!(x,y) length is m
		return m
	fi
end

global proc var_make_struct(variant a, dest, int n, rectype) =
!create a list of n vars starting from a in reverse order (a is the last)
!put the result in dest (note this will be the last the n vars)
	symbol d
	ref symbol r
	object p
	variant b
	int m
	ref byte q

	p:=obj_new_struct(rectype)

	b:=p.varptr

	m:=ttlength[rectype]
	d:=ttnamedef[rectype]
	r:=d.topfieldlist

	if n<m then
		pcerror("Too few elements")
	elsif n>m then
		println =n,=m
		pcerror("Too many elements")
	fi

	q:=p.ptr

	to n do
		var_storepacked(q, a, r.mode)
		q+:=ttsize[r.mode]
		++r
		++a
	od

	dest.tagx:=tstruct ior hasrefmask
	p.usertag:=rectype
	dest.objptr:=p
end

global function obj_new_struct(int m)object p=
	int size

	p:=obj_new()
	p.mutable:=1
	p.usertag:=m

	size:=ttsize[m]
	if size then
		p.ptr:=pcm_allocz(size)
	fi

	return p
end

global proc var_dupl_struct(variant a)=
	object p,q
	int size

	p:=a.objptr
!
	size:=ttsize[p.usertag]
	q:=obj_new_struct(p.usertag)
	a.objptr:=q

	memcpy(q.ptr, p.ptr, size)
end

global proc obj_free_struct(object p)=
	pcm_free(p.ptr, ttsize[p.usertag])
	pcm_free32(p)
end

global function var_equal_struct(variant x,y)int=
!assume tags match

	return eqbytes(x.objptr.ptr, y.objptr.ptr, ttsize[x.tag])
end

global proc var_getix_struct(variant a, int index)=
!a is a list, b is an int; return a[b] into a, which will be on the stack usually
	symbol d
	ref symbol r
	varrec v
	object p
	int elemtype

	v:=a^
	p:=a.objptr

	if index<1 or index>ttlength[a.tag] then
		pcerror("struct[int] bounds")
	fi

	d:=ttnamedef[p.usertag]
	r:=(d.topfieldlist+index-1)

	var_loadpacked(p.ptr+r.fieldoffset, r.mode, a)
end
