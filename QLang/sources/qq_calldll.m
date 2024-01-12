global proc calldll(symbol d, variant args, result, int nargs)=
	symbol e
	const maxparams=100
	[maxparams]int arglist
	int n, retcode, retval,fnindex,libindex
	word dllinst
	ref proc fnaddr
	ichar name

	if nargs>maxparams then pcerror("Too many dll args") fi

	e:=d.deflist
	n:=0

	for i to nargs do
		if e=nil then		!need to check for variadic
			if d.mvarparams then
				arglist[i]:=vartopacked(args,nil)
				++args
			else
				pcerror("Too many dll args")
			fi

		else
			arglist[i]:=vartopacked(args,e)
			++args
			e:=e.nextdef
		fi
	od

	if d.mode in [tr64] then
		retcode:='R'
	else
		retcode:='I'
	fi

	fnaddr:=getlibprocaddr(d)

	retval:=os_calldllfunction(fnaddr, retcode, nargs,&arglist, nil)

	if d.mode then
		packedtovar(retval, d.mode, result)
	fi
end

func getlibprocaddr(symbol d)ref proc fnaddr=
	fnaddr:=dllprocaddr[d.index]

	if fnaddr=nil then
		fnaddr:=loaddllfunction(d)
	fi

	fnaddr
end

function vartopacked(variant p, symbol d)word=
!convert variant to packed type matching d.mode that will fit into a word
!when d is nil, means variadic arg
	int s:=p.tag, t
	object a

	if d=nil then				!variadic
		case s
		when tstring then
			a:=p.objptr
!			return word@(convtostringz(a.strptr,a.length))
			return word(convtostringz(a.strptr,a.length))
		when tint,treal,trefpack then
			return p.value
		else
			pcerror("Bad variadic param")
		esac
	fi

	t:=d.mode

	case ttbasetype[t]
	when ti64, tu64, ti32, tu32, ti16, tu16 then
		case s
		when tint, trefpack,trefvar then
			return p.value
		when treal then
			return int(p.xvalue)
		else
error:
			fprintln "'#' should be '#' (param # #)", strmode(s,1), strmode(t),d.name, d.index
			pcerror("DLL: wrong param type")
		esac

	when tr64 then
		case s
		when tint then
			return word@(real(p.value))
		when treal then
			return word@(p.xvalue)
		else
			error
		esac
	when tstringz then
		case s
		when tstring then
			a:=p.objptr
!			return word@(convtostringz(a.strptr,a.length))
			return word(convtostringz(a.strptr,a.length))
		when trefpack then
!			return word@(p.ptr)
			return word(p.ptr)
		else
			error
		esac
	when trefpack then
		case s
		when trefpack then
			return word(p.ptr)
		when tarray, tvector then
			return word(p.objptr.ptr)
		else
			error
		esac
	when tslice then
		return u64(ref byte(p.objptr)+8)		!point to (ptr, length) section

	else
		pcmxtypestt("DLL params:",s,t)
	end

	return 0

end

proc packedtovar(word retval, int t, variant dest)=
!convert packed value retval of type t to variant
!assume variant is ready to receive value (eg. is void)
	int tbase:=ttbasetype[t]
	object a

	case tbase
	when tvoid then
	when tr64 then
		dest.tagx:=treal
		dest.xvalue:=real@(retval)
	when tr32 then
		PCERROR("dll/r32ret")
	when ti64,tu64 then
		dest.tagx:=tint
		dest.value:=retval
	when ti32 then
		dest.tagx:=tint
		dest.value:=int32(retval)
	when tu32 then
		dest.tagx:=tint
		dest.value:=word32(retval)
	when ti16 then
		dest.tagx:=tint
		dest.value:=int16(retval)
	when tu16 then
		dest.tagx:=tint
		dest.value:=word16(retval)
	when trefpack then
		dest.tagx:=trefpack
		dest.ptr:=cast(retval)
		dest.elemtag:=tttarget[t]
	when tstringz then
		var_make_string(cast(retval), dest)

	else
		pcerror_s("Rettype not supported:",ttname[t])
	esac
end

function loaddllfunction(symbol d)ref proc=
	int fnindex,libindex
	word dllinst
	ref proc fnaddr
	ichar name

	fnindex:=d.index
	fnaddr:=dllprocaddr[fnindex]
	return fnaddr when fnaddr

	libindex:=dllproclibindex[fnindex]
	dllinst:=dllinsttable[libindex]

	if dllinst=0 then
		dllinst:=os_getdllinst(libtable[libindex].name)
		if dllinst=0 then
			pcerror_s("Can't load DLL:",libtable[libindex].name)
		fi
		dllinsttable[libindex]:=dllinst
	fi

	name:=(d.truename|d.truename|d.name)
!CPL "LOADING DLL FN",NAME
	fnaddr:=os_getdllprocaddr(dllinst,name)

	if fnaddr=nil then
		pcerror_s("Can't find DLL func:",name)
	fi
!CPL "LOADED DLL FUNC",NAME,"AT", FNADDR
	dllprocaddr[fnindex]:=fnaddr

	return fnaddr
end

