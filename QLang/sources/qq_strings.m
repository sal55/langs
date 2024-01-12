global object emptystring

proc start=
	emptystring:=obj_new()
	emptystring.refcount:=1
	emptystring.objtype:=normal_obj
end

global proc var_empty_string(variant dest, int mutable=0)=
	dest.tagx:=tstring ior hasrefmask
	if not mutable then
		dest.objptr:=emptystring
		++emptystring.refcount
	else
		dest.objptr:=obj_make_stringn(nil, 0,1)
	fi
end

global proc var_make_string(ichar s, variant dest, int mutable=0)=
!CPL "MAKESTR"
	dest.tagx:=tstring ior hasrefmask
	dest.objptr:=obj_make_string(s,mutable)
end

global proc var_make_stringn(ichar s, int length, variant dest, int mutable=0)=
!CPL "MAKESTRN",LENGTH
	dest.tagx:=tstring ior hasrefmask
	dest.objptr:=obj_make_stringn(s,length,mutable)
end

global function obj_new_string(int n)object p=
	p:=obj_new()

!CPL "NEWSTR:",=P,=N
	p.mutable:=1
	p.length:=n
	p.objtype:=normal_obj

	if n then
		p.strptr:=pcm_alloc(n)
		p.alloc64:=allocbytes
	fi

	return p
end

global function obj_make_string(ichar s,int mutable=0)object p=
	int n

	p:=obj_new_string(n:=strlen(s))
	p.mutable:=mutable

	if n then
		memcpy(p.strptr,s,n)
	fi
	return p
end

global function obj_make_stringn(ichar s,int length,mutable=0)object p=
!when s=nil, then the string data is not initialised

	p:=obj_new_string(length)
	p.mutable:=mutable

	if length then
		if s then
			memcpy(p.strptr,s,length)
		else
			memset(p.strptr,0,length)
		fi

	fi
	return p
end

global proc obj_free_string(object p)=
	if p.length then
		pcm_free(p.strptr,p.alloc64)
	fi

	pcm_free32(p)
end

global proc var_dupl_string(variant a)=
	object p,q

	p:=a.objptr
	q:=obj_new_string(p.length)
	a.objptr:=q

	if q.length then
		memcpy(q.strptr,p.strptr,q.length)
	fi
end

global proc var_getix_string(variant a, int index)=
!put result into a (which will be on the stack)
	object q

	q:=a.objptr

	if word(index-1)>=word(q.length) then
		pcerror("getstring[int] bounds")
	fi

!	var_unshareu(a)
	stringslice(a,index,index,a)
end

global proc var_getixref_string(variant a, int index)=
!put result into a (which will be on the stack)
	object q

	q:=a.objptr

	if word(index-1)>=word(q.length) then
		pcerror("getixref[int] bounds")
	fi

	a.tagx:=trefpack
	a.elemtag:=tu8
	a.ptr:=cast(q.strptr+index-1)
end

global proc var_getdotix_string(variant a, int index)=
!put result into a (which will be on the stack)
	object q

	q:=a.objptr

	if word(index-1)>=word(q.length) then
		pcerror("x.[] bounds")
	fi

	a.tagx:=tint
	a.value:=(q.strptr+index-1)^
end

global proc var_getdotixref_string(variant a, int index,variant dest)=
	object q

	q:=a.objptr

	--index

	if word(index)>=word(q.length) then
		pcerror("x.[] bounds")
	fi

	dest.tagx:=trefpack
	dest.elemtag:=tu8
	dest.ptr:=q.strptr+index
end

global proc var_getslice_string(variant a, int i,j)=
	object p:=a.objptr

	if i<1 or j>p.length or i>j then
		pcerror("string/slice bounds")
	fi

	stringslice(a,i,j,a)
end

proc stringslice(variant a, int i,j, variant dest)=
	object p,q

	p:=a.objptr

	q:=obj_new()
	q.mutable:=p.mutable
	q.length:=j-i+1
	q.objtype:=slice_obj

	case p.objtype
	when slice_obj then				!slice of a slice!
		q.objptr2:=p.objptr2		!link to original
		++q.objptr2.refcount
	when extslice_obj then
		q.objptr2:=nil
		q.objtype:=extslice_obj
	else
		++p.refcount
		q.objptr2:=p				!link to original
	esac
	q.strptr:=p.strptr+i-1

	dest.tagx:=a.tagx
	dest.objptr:=q
end

global proc var_putix_string(variant a, int index, variant x)=
	ichar s
	object p,q
	int length

	p:=a.objptr
	if not p.mutable then pcnotmut() fi
	length:=p.length

	if index not in 1..length then
		if index=length+1 then
			var_addto_string(a,x)
			return
		else
			pcerror("putstring[int] bounds")
		fi
	fi

	s:=p.strptr+index-1
	if x.tag<>tstring then
		pcerror("s[i]:= not str")
	fi
	q:=x.objptr
	if q.length=0 then pcerror("s[i]:=""""") fi
	s^:=q.strptr^
end

global proc var_putslice_string(variant a, int i,j, variant x)=
!insert a substring into a
	ichar s
	object p,q
	int length,sublength

	p:=a.objptr
	if not p.mutable then pcnotmut() fi
	length:=p.length

	if i<1 or j>p.length or i>j then
		pcerror("string/slice bounds")
	fi
	sublength:=j-i+1

	s:=p.strptr+i-1
	if x.tag<>tstring then
		pcerror("s[i..j]:= not str")
	fi
	q:=x.objptr
	if q.length<sublength then
		pcerror("substr too short")
	fi
	memcpy(s,q.strptr, sublength)
end

global proc var_putdotix_string(variant a, int index, variant x)=
	ichar s
	object p,q
	int length,ch

	if x.tag<>tint then
		pcerror("s.[i]:= not int")
	fi
	ch:=x.value

	p:=a.objptr
	if not p.mutable then pcnotmut() fi
	length:=p.length

	if index not in 1..length then
		if index=length+1 then
			var_addto_string_ch(a,ch)
			return
		else
			pcerror("str.[int] bounds")
		fi
	fi

	(p.strptr+index-1)^:=ch
end

global proc obj_resize_string(object p,int n)=
	ichar s
	int oldalloc

	if n<=p.alloc64 then
		p.length:=n
	else
		oldalloc:=p.alloc64
		s:=pcm_alloc(n)
		p.alloc64:=allocbytes
		if p.length then
			memcpy(s,p.strptr,p.length)

			pcm_free(p.strptr, oldalloc)
		fi

		p.strptr:=s
		p.length:=n
	fi
end

global proc var_add_string(variant a,b)=
!a':=a+b; original a is preserved, just that new result is into a
	object p:=a.objptr
	object q:=b.objptr
	object r

	int alen:=p.length
	int blen:=q.length
	int newlen

	if blen=0 then
		var_shareu(a)
		return
	elsif alen=0 then
		var_make_stringn(q.strptr,blen,a,1)
		return
	fi

	newlen:=alen+blen
	r:=obj_new_string(newlen)
	memcpy(r.strptr, p.strptr, alen)
	memcpy(r.strptr+alen, q.strptr, blen)

	a.objptr:=r
end

global proc var_addto_string(variant a,b)=
!a+:=b; inplace add
!a is normally subject of a refvar, so not shared

	object p:=a.objptr
	object q:=b.objptr

!CPL "ADDTO/STR"

	int alen:=p.length
	int blen:=q.length
	int newlen

	if not p.mutable then
		pcnotmut()
	fi

	if blen=0 then
		return
	elsif alen=0 then			!copy b over and share
		var_unshareu(a)
		a^:=b^
		var_duplu(a)
		return
	fi

!CP "AS "
	newlen:=alen+blen
	obj_resize_string(p,newlen)
	memcpy(p.strptr+alen, q.strptr, blen)
end

global proc var_addto_string_ch(variant a,int ch)=
!a+:=ch; inplace add
!a is normally subject of a refvar, so not shared
	object p:=a.objptr
	int alen:=p.length, n
	[32]char str
	ichar s

	if not p.mutable then
		pcnotmut()
	FI

	obj_resize_string(p,alen+1)
	(p.strptr+alen)^:=ch
end

!global function u64tostr(u64 aa,ref char s,word base,int sep)int =		!U64TOSTR

global function var_equal_string(variant x,y)int =
!return 1 if strings in x,y are equal, otherwise 0
	int n,res
	object px,py

	px:=x.objptr
	py:=y.objptr
	if px=py then return 1 fi

	n:=px.length

	if n<>py.length then
		0				!unequal lengths
	elsif n=0 then
		1				!same zero length
	else
!		res:=cmpstringn(px.strptr,py.strptr,n)=0
		eqbytes(px.strptr,py.strptr,n)
	fi
end

global function var_compare_string(variant x,y)int =
!return -1/0/+1
	int res
	object px,py

	px:=x.objptr
	py:=y.objptr

	res:=cmpstring_len(px.strptr, py.strptr, px.length, py.length)
	return res
end

function cmpstring_len(ref char s,t,int slen,tlen)int =
!compare the given strings with these lengths, and return -1,0,1

	if slen=0 then
		if tlen=0 then
			return 0		!empty:empty
		else
			return -1		!empty:str
		fi
	elsif tlen=0 then	!str:empty
		return 1
	else
		if slen=tlen then
			if slen=1 then
				if s^<t^ then return -1
				elsif s^>t^ then return 1
				else
					return 0
				fi
			fi
			return cmpstringn(s,t,slen)
		else
			return cmpstring(convtostringz(s,slen),convtostringz(t,tlen))
		fi
	fi
end

global function var_inx_string(variant x,y)int =
!return start index of string x in y, or 0
	int xlen,ylen,result,i,j,k
	ref char sx, sy
	object px,py

	px:=x.objptr
	py:=y.objptr

	xlen:=px.length
	ylen:=py.length

	if xlen=0 or ylen=0 then		!at least one is empty
		return i64.min
	fi

	k:=ylen-xlen
	for i:=0 to k do			!all start positions
		sx:=px.strptr
		sy:=py.strptr+i
		for j:=1 to xlen do			!all chars in y
			if sx^<>sy^  then
				goto nextpos
			fi
			++sx; ++sy
		od
		return i+1
nextpos:
	od
	return i64.min
!	return 0
end

global proc var_iconvcase(variant a,b,int upper)=
!do in-place conversion of string in a^ to lower or upper (upper=0/1).
!a points directly to a varrec to be modified
!b is optional if supplied, gives number of chars to convert at left of string
!
	int i,n
	ref char s
	object pa

	pa:=a.objptr

	if b.tag>tvoid then		!allow void param to be regarded as missing one
		n:=var_getintvalue(b)
	else
		n:=pa.length			!default is the whole length of the string
	fi

	if a.tag<>tstring then
		pcerror("convcase/notstr")
	fi

	if n<0 then
		pcerror("CONVCASE N<0")
	fi

	if n=0 then
		return
	fi

	if n>pa.length then
	cpl =n,pa.length
		pcerror("convcase/N?")
	fi
	s:=pa.strptr

	if upper then
		to n do
			s^:=toupper(s^)
			++s
		od
	else
		to n do
			s^:=tolower(s^)
			++s
		od
	fi
end

global proc var_makestrslicexobj(ichar s, int length, variant dest)=
	dest.tagx:=tstring ior hasrefmask
	dest.objptr:=obj_make_strslicexobj(s,length)
end

global function obj_make_strslicexobj(ichar s, int length)object=
!s is an existing non-allocated or external string
!create a special string slice object, which for now has the format:
! .objtype=extslice, but .objptr2=0
!length can be 0, then s can be nil or ""
!
	object p

	if length=0 then s:=nil fi

	p:=obj_new()
	p.strptr:=s
	p.mutable:=1
	p.length:=length
	p.objtype:=extslice_obj		!.objptr2 will be zero
	return p
end

function var_asc(variant a)int=
	object p
	if a.tag<>tstring then pcerror("Asc:not str") fi
	p:=a.objptr
	if p.length<1 then pcerror("Asc:empty") fi

	return p.strptr^
end

global proc var_new_string(variant a, b, dest)=
	int length:=var_getintvalue(a)
	int ch

	if length<0 then pcerror("Length<0") fi

	var_make_stringn(nil,length,dest)

	case b.tag
	when tint then
		ch:=b.value
	when tstring then
		ch:=var_asc(b)
	when tvoid then
		ch:=' '
	else
		pcerror("Not int/str")
	esac

	if length then
		memset(dest.objptr.strptr,ch,length)
	fi
end

global proc var_new_stringn(int length, variant dest)=
	if length<0 then pcerror("Length<0") fi

	var_make_stringn(nil,length,dest)
end

global proc var_mul_string(variant a, int m)=
!a:=a*m
!a has been copied, so no need to unshare it

	int i,oldlen,newlen
	ref char newptr,p
	varrec v
	object pa,s

	if m<0 then
		pcerror("neg str mul")

	elsif m=0 then		!result is empty str
		var_empty_string(a)
		return

	elsif m=1 then		!leave a unchanged
		var_shareu(a)	!
		return

	else				!multiply non-null string by m
		pa:=a.objptr
		oldlen:=pa.length
		if oldlen then			!not empty string
			newlen:=oldlen*m

			v.objptr:=obj_new_string(newlen)
			v.tagx:=tstring ior hasrefmask

			p:=v.objptr.strptr
			if oldlen=1 then
				memset(p,pa.strptr^,m)
			else
				to m do
					memcpy(p,pa.strptr,oldlen)
					p+:=oldlen
				od
			fi
			a^:=v
		else				!was empty string: copy to v
			var_empty_string(a)
			return
		fi
	fi
end

global proc var_convert_string_list(variant a, int t, variant dest)=
	object p:=a.objptr
	variant q
	int length:=p.length
	ichar s

	var_make_list(nil, dest, length, 1)
	q:=dest.objptr.varptr
	s:=p.strptr

	to length do
		var_make_stringn(s,1, q,1)
		++s
		++q
	od
end

global proc var_expand_string(variant a, dest, int m)=
	variant b,c
	object p
	ref char s
	int n

	p:=a.objptr
	b:=dest
	s:=p.strptr
	n:=1

	to m do
		if n>p.length then
			var_empty_string(dest)
		else
			var_make_stringn(s,1, dest,1)
			++s
		fi
		++n
		--+dest
	od
end

global proc var_makechar(int ch,variant dest)=
	varrec v
	[8]char str
	object p

	if ch not in 0..255 then
		pcerror("chr range")
	fi

	p:=chrtable[ch]
	if p=nil then			!create single-char constant
		str[1]:=ch
		str[2]:=0
		var_make_stringn(str,1,&v,0)
				chrtable[ch]:=p:=v.objptr
	fi
	++p.refcount

	dest.tagx:=tstring ior hasrefmask
	dest.objptr:=p
end
