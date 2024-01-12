
record dimrec=(mut int lbound, upper, length)

type hostproc0=ref proc
type hostproc1=ref proc(variant a)
type hostproc2=ref proc(variant a,b)
type hostproc3=ref proc(variant a,b,c)
type hostproc4=ref proc(variant a,b,c,d)
type hostproc5=ref proc(variant a,b,c,d,e)

ref[]symbol procrefs				!linear arrays set up from proclists

!record overloadrec=
!	int optype, optype2
!	ref int pchandler
!	ref overloadrec nextrec
!end
!
!ref overloadrec tostr_list			!list of user overloads for tostr
!ref overloadrec convert_list

const noparamtag=tvoid
const nodefault=-999999

!global [0..hostfnnames.upb]ref proc hosttable

global proc callhostfunction(int hostfn) =
	ref proc fnaddr
	int nparams,isfn
	object p

!	fnaddr:=hosttable[hostfn]
	fnaddr:=hosthandlers[hostfn]
	nparams:=hostnparams[hostfn]
	isfn:=hostisfn[hostfn]

!CPL "CALL HOST",HOSTFNNAMES[HOSTFN]

	if fnaddr=nil then
		pcerror_s("Hostfn not implemented:",hostfnnames[hostfn])
	fi

	case nparams+isfn
	when 0 then
		hostproc0(fnaddr)^()
	when 1 then
		hostproc1(fnaddr)^(sptr)
	when 2 then
		hostproc2(fnaddr)^(sptr,sptr-1)
	when 3 then
		hostproc3(fnaddr)^(sptr,sptr-1,sptr-2)
	when 4 then
		hostproc4(fnaddr)^(sptr,sptr-1,sptr-2,sptr-3)
	when 5 then
		hostproc5(fnaddr)^(sptr,sptr-1,sptr-2,sptr-3,sptr-4)
	else
		pcerror("callhost/proc")
	esac

	to nparams do
		var_unshare(sptr) when sptr.hasref
		--sptr
	od
end

!global proc inithostlib=
!
!	ichar name
!	int n:=$get_nprocs()
!
!	for i to n do
!		name:=$get_procname(i)
!		if eqbytes(name,"pch_",4) then		!(should be OK with v short fn names)
!			for k:=0 to hostfnnames.upb do
!				if eqstring(name+4,hostfnnames[k]+2) then		!skip "pch_" and "h_"
!					hosttable[k]:=$get_procaddr(i)
!					exit
!				fi
!			else
!				loaderror("Unknown hostfn",name)
!			od
!		fi
!	od
!end

global proc pch_leftstr(variant a, b, c, result)=
	int n,length,padchar
	ref char s
	object pa

	padchar:=' '
	case c.tag
	when tvoid then
	when tstring then
		if c.objptr.length=1 then
			padchar:=c.objptr.strptr^
		else
			pcerror("left/padx")
		fi
	when tint then
		padchar:=c.value
	else
		pcerror("left/pad?")
	esac

	case b.tag
	when tvoid then
		n:=1
	when tint then
		n:=b.value
	else
		pcerror("left:bad n")
	esac
	if a.tag<>tstring then
		pcerror("left:not str")
	fi

	pa:=a.objptr
	length:=pa.length
	s:=pa.strptr

	if n=0 then
		var_empty_string(result,1)
		return
	fi

	result.tagx:=tstring ior hasrefmask
	if n>0 then			!leftmost n chars
		if n<=length then
			leftstring(a,n,result)
		else				!n>length
			padstring_right(a,n,padchar,result)
		fi
	else					!left chars chars excluding rightmost n
		n:=-n
		if n<length then
			leftstring(a,length-n,result)
		else
			var_empty_string(result,1)
		fi
	fi
end

global proc pch_rightstr(variant a, b, c, result)=
	int n,length,padchar
	ref char s
	object pa

	padchar:=' '
	case c.tag
	when tvoid then
	when tstring then
		if c.objptr.length=1 then
			padchar:=c.objptr.strptr^
		else
			pcerror("right/padx")
		fi
	when tint then
		padchar:=c.value
	else
		pcerror("right/pad?")
	esac

	case b.tag
	when tvoid then
		n:=1
	when tint then
		n:=b.value
	else
		pcerror("right:bad n")
	esac

	pa:=a.objptr
	if a.tag<>tstring then
		pcerror("right:not str")
	fi

	length:=pa.length
	s:=pa.strptr

	result.tagx:=tstring ior hasrefmask

	if n=0 then
		var_empty_string(result,1)
		return
	fi

	if n>0 then			!rightmost n chars
		if n<=length then
			rightstring(a,n,result)
		else				!n>length
			padstring_left(a,n,padchar,result)
		fi
	else					!right chars chars excluding leftmost n
		n:=-n
		if n<length then
			rightstring(a,length-n,result)
		else
			var_empty_string(result,1)
		fi
	fi
end

global proc pch_convlc(variant a, b, result)=
	checkparam(a,tstring)
	result^:=a^
	++result.objptr.refcount
	var_duplu(result)
	var_iconvcase(result,b,0)
end

global proc pch_convuc(variant a, b, result)=
	checkparam(a,tstring)
	result^:=a^
	++result.objptr.refcount
	var_dupl(result) when result.hasref
	var_iconvcase(result,b,1)
end

global proc pch_waitkey(variant result)=
	result.tagx:=tint
	result.value:=os_getch()
end

global proc pch_execwait(variant a, b, c, result)=
	ref char workdir
	int flag
	object pa

	checkparam(a,tstring)
	pa:=a.objptr

	flag:=checkparam(b,tint,0)

	if c.tag=tvoid then
		workdir:=nil
	else
		checkparam(c,tstring)
		workdir:=convtostringz(c.objptr.strptr,c.objptr.length)
	fi
	result.tagx:=tint
	result.value:=os_execwait(convtostringz(pa.strptr,pa.length),flag,workdir)
end

global proc pch_execcmd(variant a, b, c, result)=
	ref char workdir
	int flag
	object pa

	checkparam(a,tstring)
	pa:=a.objptr

	flag:=checkparam(b,tint,0)

	if c.tag=tvoid then
		workdir:=nil
	else
		checkparam(c,tstring)
		workdir:=convtostringz(c.objptr.strptr,c.objptr.length)
	fi
	result.tagx:=tint
	result.value:=os_execcmd(convtostringz(pa.strptr,pa.length),flag)
end

global proc pch_makestr(variant a, b, result)=
	int n

	case a.tag
	when trefpack then
	when tint then
	else
		pcerror("makestr")
	esac

	n:=var_getintvalue(b)

	RESULT.TAGX:=TSTRING IOR HASREFMASK

	RESULT.OBJPTR:=obj_make_strslicexobj(cast(a.ptr),n)
end

global proc pch_makeref(variant a,b,result) =
	ref byte ptr

	case (ttbasetype[a.tag])
	when trefvar,trefpack,tint then
		ptr:=a.ptr
	when tstring,tarray,tlist,tset then
		ptr:=a.objptr.ptr
	else
		pcerror("makeref")
	esac

	result.tagx:=trefpack
	result.ptr:=ptr
	result.elemtag:=var_getintvalue(b)

	case result.elemtag
	when tu1,tu2,tu4 then
		result.tag:=trefbit
		result.bitoffset:=0
		result.bitlength:=0
	esac
end

global proc pch_getcmdparam(variant a, result)=
!a=	void:	return number of cmd params following program name
!a= -2:		return name of invoked interpreter
!a= -1:		return name of .q program when run conventionally
!a= 1..N:   return name of n'th cmd param

	int n
	ref char s

	if a.tag=noparamtag then		!return number of cmds
		result.tagx:=tint
		result.value:=nqparams
		return
	fi

	n:=var_getintvalue(a)
	if n not in 1..nqparams then pcerror("getcmdpm") fi

	var_make_string(qparamtable[n],result)
end

global proc pch_clock(variant result)=
	result.tagx:=tint
	result.value:=os_clock()
end

global proc pch_allocexec(variant a, result)=
	int n
	ref byte p

	n:=var_getintvalue(a)
	p:=os_allocexecmem(n)

	result.tagx:=trefpack
	result.ptr:=p
	result.elemtag:=tu8
end

global proc pch_runnative(variant a, b, result)=
	int n
	ref func(int)int fnptr

	if a.tag<>trefpack then pcerror("runnative?") fi
	fnptr:=cast(a.ptr)

	result.value:=fnptr(b.value)
	result.tagx:=tint
end

global proc pch_setlwb(variant a, b)=
	int n
	object p

	if not a.hasref then error fi
	p:=a.objptr
	if not p.mutable then pcnotmut() fi
	n:=checkparam(b,tint)

	case a.tag
	when tlist then
		if n not in i16.min..i16.max then pcerror("lwb not i16") fi
		p.lower16:=n
	when tarray, tbits then
		if n not in 0..1 then pcerror("lwb not 0/1") fi
		p.lower:=n
	else
error:
		pcerror("Can't set lwb")
	esac
end

global proc pch_ticks(variant result)=
	result.tagx:=tint
	result.value:=os_ticks()
end

global proc pch_sleep(variant a)=
	checkparam(a,tint)
	os_sleep(a.value)
end

global proc pch_random(variant a, result)=
! a=0		Result is pure int
! a=1		Result is 0.0 to 0.9999999...
! a=n		Result is 0 to n-1
! a=x..y	Result is x to y inclusive

	int n,x

	result.tagx:=tint			!assume int result (can be real too)

	if a.tag=trange then
		x:=mrandomrange(a.range_lower, a.range_upper)
	else
		checkparam(a,tint)
		n:=a.value
		if n>1 then					!0 to n-1
			x:=mrandomint(n)
		elsif n=0 then				!pure rand
			x:=mrandom()
		elsif n=1 then				!0.0 to 0.99999999
			result.tagx:=treal
			result.xvalue:=mrandomreal()
			return
		else
			mseed(-n)
			x:=0
		fi
	fi
	result.value:=x
end
!
global proc pch_system(variant a,result) =		!PCH_SYSTEM
	checkparam(a,tstring)
	result.tagx:=tint
	result.value:=system(convtostringz(a.objptr.strptr,a.objptr.length))
end

global proc pch_$getparam(variant a, result)=
	checkparam(a,tint)

	result^:=variant(frameptr-a.value*varsize)^		!param 1/2/3... = offset 16/32/48... (varsize=16)
	if result.hasref then
		++result.objptr.refcount
	fi
end

function checkparam(variant p,int tag,defaultx=nodefault)int64=
!check out a host param, usually for ints
!void:	return default value (assuming int needed), unless default=nodefault
!		then it's an error
!=tag:	return value
!other:	error

	case p.tag
	when tvoid then
		if defaultx=nodefault then
			pcerror("Missing host param")
		fi
		return defaultx
	when tag then
		return p.value
	esac

	if tag=tint then
		case p.tag
		when treal then
			return p.xvalue
		esac
	fi

	cpl ttname[p.tag]
	pcerror("Host param wrong type")
	return 0
end

proc leftstring(variant a, int n, variant result)=
!a is an existing string on varstack, which could have be cc_copy or cc_owner
!This can be ""
!return slice of left n chars (n<=length, but n is never zero) in result
!When a is a copy, then returns a view into a, otherwise it will create a new
!string
	object p

!NOTE can create slice here

	var_make_stringn(a.objptr.strptr,n,result,1)
!var_makestringn(ichar s, int length, variant dest, int mutable)=
end

proc rightstring(variant a, int n, variant result)=
!a is an existing string on varstack, which could have be cc_copy or cc_owner
!This can be ""
!return slice of right n chars (n<=length, but n is never zero) in result
!When a is a copy, then returns a view into a, otherwise it will create a new
!string
	object p

!NOTE can create slice here
!pc_makestring(a.objptr.strptr+(a.objptr.length-n),n,result)
	var_make_stringn(a.objptr.strptr+(a.objptr.length-n),n,result,1)
end

proc padstring_right(variant a,int n, fillchar, variant result)=
!a is a string (can be "")
!create a new string of n chars of which the first a.length are from a,
!and the rest are filled with <fillchar>
!n>length always
	ref char s
	int length

	length:=a.objptr.length

	var_new_stringn(n,result)
	s:=result.objptr.strptr

	if length then
		memcpy(s,a.objptr.strptr,length)
		s+:=length
	fi
	to n-length do
		s^:=fillchar
		++s
	od
end

proc padstring_left(variant a,int n, fillchar, variant result)=
!a is a string (can be "")
!create a new string of n chars of which the last a.length are from a,
!and the rest are filled on the left with <fillchar>
!n>length always
	ref char s
	int length,padlen

	length:=a.objptr.length
	padlen:=n-length

	var_make_stringn(nil,n,result,0)

	s:=result.objptr.strptr
	s+:=padlen

	if length then
		memcpy(s,a.objptr.strptr,length)
	fi
	to padlen do
		--s
		s^:=fillchar
	od
end

proc getbounds(variant p,ref dimrec dims,int lower) =
! extract length or bounds from p, and return in dims
! p will be an int, range, or other value coerceable to int
! lower is default lower bound
	int n

	if not p then
		pcerror("New: no bounds")
	fi

	case p.tag
	when noparamtag then
		dims.lbound:=lower
		dims.upper:=0
		dims.length:=0
	when trange then
		dims.lbound:=p.range_lower
		dims.upper:=p.range_upper
		dims.length:=p.range_upper-p.range_lower+1
		if dims.length<0 then
			dims.length:=0
			dims.upper:=dims.lbound-1
		fi
	else
		n:=var_getintvalue(p)
		dims.lbound:=lower
		dims.upper:=dims.length:=n
	esac
end

global proc pch_new(variant a, b, c, d, result)=
	varrec v
	int i,t,nbytes,ival,nwords,nbits,offset,elemtype,n, usertag
	dimrec dims
	variant qvar
	ref int64 qint
	ref byte qbyte
	ref byte ptr
	object p

	t:=var_getintvalue(a)

	if t<0 or t>ntypes then
		pcustype_t("New:bad type",t)
	fi
	v.tagx:=t ior hasrefmask
	usertag:=0

	switch ttbasetype[t]
	when tstring then
		var_new_string(b,c, result)
		return

	when tlist then
		getbounds(b,&dims,1)
		p:=obj_newlist(dims.length,dims.lbound,c)

		v.objptr:=p

	when tarray then
		elemtype:=var_getintvalue(b)
		getbounds(c,&dims,1)
		if elemtype>=tu1 and elemtype<=tu4 then
			v.tag:=t:=tbits
			goto dobits2
		fi
!
		p:=obj_newarray(elemtype, dims.lbound, dims.length)

doarray2:
		v.objptr:=p

		if dims.length then
			if d and d.tag<>tvoid then		!initial value supplied
				qbyte:=p.ptr
				to dims.length do
					var_storepacked(qbyte,d,elemtype)
					qbyte+:=ttsize[elemtype]
				od
			fi
		fi
!
	when tvector then
		usertag:=t
		v.tag:=tvector
		elemtype:=tttarget[t]
		dims.length:=ttlength[t]
		dims.lbound:=ttlower[t]
		dims.upper:=dims.length+dims.lbound-1

		d:=b					!any init value: move to d
		p:=obj_newarray_u(t)
!CPL =TTNAME[T], TTNAME[TTBASETYPE[T]]
		goto doarray2
!
	when tbits then
		elemtype:=var_getintvalue(b)
		if elemtype not in tu1..tu4 then
			pcerror("new: bad bits elem")
		fi
		getbounds(c,&dims,1)
dobits2:				!entry point from arrays, when element is bit type

		p:=obj_newbits(elemtype,dims.lbound,dims.length)

		v.objptr:=p

		if dims.length then
			if d and d.tag<>tvoid then		!initial value supplied
				qbyte:=p.ptr

				offset:=0
				to dims.length do
					var_storebit(qbyte,offset,d,elemtype,0)
					offset+:=ttbitwidth[elemtype]
					if offset>=8 then
						offset:=0
						++qbyte
					fi
				od
			fi
		fi
!
	when tset then
		getbounds(b,&dims,0)

		if dims.lbound<0 then
			pcerror("new:set:lwb")
		fi
		if dims.lbound<>0 then
			dims.lbound:=0
			dims.length:=dims.upper+1
		fi

		p:=obj_newset(dims.length)
		v.objptr:=p
!
	when trecord then
		p:=obj_new_record(t,b)
		var_fromobj(t,p,&v)
		v.tag:=trecord
		usertag:=t

	when tstruct then

		p:=obj_new_struct(t)

		var_objtovar(t,p,&v)
		v.tag:=tstruct
		usertag:=t

		if b and b.tag<>tvoid then
			pcerror("New: struct init")
		fi

	when tint,treal,trefvar then
		v.value:=0
		v.hasref:=0
		if b and b.tag<>tvoid then
			pcerror("NEW(int/value)")
		fi

	when tdict then
		getbounds(b,&dims,1)
		if dims.lbound<>1 then
			pcerror("new:dict:lwb")
		fi
		p:=obj_new_dict(dims.length)
		v.objptr:=p

	when tdecimal then
		var_empty_dec(result)
		return

	else
		pcustype_t("new",t)
	end
finish:

	if usertag then
		v.objptr.usertag:=usertag
	fi

	result^:=v

end

global proc pch_gethostname(variant result) =
	static [256]char name

	strcpy(name,os_gethostname())

	var_make_string(name,result)
end

global proc pch_getprogname(variant result) =
	static [256]char name

	strcpy(name,inputfile)

	var_make_string(name,result)
end

global proc pch_$test(variant a, b, c, result)=
!OBJECT P
!P:=A.OBJPTR

!CPL "$TEST:",=A, =A.OBJPTR
!!
RESULT.TAGX:=TINT
RESULT.VALUE:=A.VALUE+B.VALUE+C.VALUE

end

global proc pch_$test2(variant a, result)=
!PPP:=A.OBJPTR
	RESULT.TAGX:=TVOID
end

global proc pch_$refcount(variant a, result)=
	result.tagx:=tint
	if a.hasref then
		result.value:=a.objptr.refcount
	else
		result.value:=0
	fi
end

global proc pch_testkey(variant result)=
	result.tagx:=tint
	result.value:=os_kbhit()
end

global proc pch_getos(variant result)=
	var_make_string(os_getos(),result)
end

global proc pch_setmesshandler(variant fn)=
	if fn.tag<>tsymbol or fn.def.nameid<>procid then
		pcerror("Not proc ref")
	fi
	pcl_callbackfn:=cast(fn.def.pcaddress)
	os_setmesshandler(&runproc_m)
end

global proc pch_$smallmemtotal(variant result)=
	result.tagx:=tint
	result.value:=smallmemtotal/varsize
end

global proc pch_$id(variant a, result)=
	result.tagx:=tint
	result.value:=a.value
end

global proc pch_iswindows(variant result)=
	result.tagx:=tint
	result.value:=os_iswindows()
end

global proc pch_$setdebug(variant a)=
	checkparam(a,tint)

	CPL "SETDEBUG................."
	fdebug:=a.value
end

global proc pch_copy(variant a, dest)=
	dest^:=a^
	var_dupl(dest)
end

global proc pch_gethash(variant a,result) =		!PCH_GETHASH
!convert a to hash value
	result.tagx:=tint
	result.value:=var_gethashvalue(a)
end

global proc pch_makeempty(variant a,result)=
	object p
	int t

	t:=ttbasetype[a.tag]
	if t=ttype then
		t:=a.value
	fi

	p:=a.objptr

	case t
	when tlist then
		var_empty_list(p.lower16,result)
		return

	when tstring then
		p:=emptystring
		++p.refcount
	when tarray then
		var_empty_array(t, p.elemtag, p.lower,result)
		return

!	when tvector then
!		var_empty_array(t, tttarget[p.usertag], 1,result)
!		return

	else
		pcustype_t("makeempty?",t)
	esac

	result.tagx:=t ior hasrefmask
	result.objptr:=p
end

global proc pch_$infinity(variant dest)=
	var_setinf(dest)
end

global proc pch_$nan(variant dest)=
	var_setnan(dest)
end

global proc setcmdparam(int index, ichar s)=
!build cmd params for pc program, or set size (usually smaller) when s is nil
	if s=nil then
		nqparams:=index
	elsif index<=maxqparam then
		qparamtable[index]:=pcm_copyheapstring(s)
		nqparams max:=index
	fi
end

global proc pch_$nprocs(variant result)=
	result.tagx:=tint
	result.value:=nproclist
end

proc initprocrefs=
	ref procrec pp
	static int oldnprocs

!	if procrefs and nn=nproclist then
!IF NN<>NPROCLIST THEN
!PCERROR("NPROCLIST HAS CHANGED")
!FI
! return fi
	if oldnprocs=nproclist then
		return
	fi


!CPL "INITPROCREFS",NPROCLIST

	procrefs:=pcm_alloc(nproclist*procrefs[1].bytes)

	pp:=proclist

	for i to nproclist do
		procrefs[i]:=pp.def
		pp:=pp.nextproc
	od
	oldnprocs:=nproclist

!NN:=NPROCLIST
end

global proc pch_$procname(variant a, result)=
	int n:=checkparam(a,tint)

!CPL "PROCNAME",NPROCLIST

	initprocrefs()

	var_make_string(procrefs[n].name, result)
end

global proc pch_$procref(variant a, result)=
	int n:=checkparam(a,tint)
	ref procrec pp

	initprocrefs()

	result.tagx:=tsymbol
	result.def:=cast(procrefs[n])
end
