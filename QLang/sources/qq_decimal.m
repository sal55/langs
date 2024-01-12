const digitwidth   = 9
const digitbase	= 1000000000
const digitfmt	 = "%09d"
const mdigitfmt	 = "z9"

const digitmax	 = digitbase-1

global type elemtype = int32
global const decelemsize = elemtype.bytes

record constrec =
	int64 value
	object bnvalue
	ref constrec nextconst
end

!special values for bignum types
enumdata [0:]ichar fpnames =
	(zero_type = 0,		$),
	(normal_type,		$),
	(inf_type,			$),
	(nan_type,			$),
end

!recognised combinations of bignum types (bintypes)
enumdata =
	nn_types,	 	  ! both numbers (non-zero)
	zz_types,	 	  ! both zero
	ii_types,	 	  ! both infinity
	xx_types,	 	  ! one or both is nan

	nz_types,	 	  ! number/zero
	ni_types,	 	  ! number/infinity

	zn_types,	 	  ! zero/number
	in_types,	 	  ! infinity/number

	zi_types,	 	  ! zero/infinity
	iz_types	 	  ! infinity/zero
end

const maxprec	   = 10 million
!int currprec	   = 100/digitwidth
!int currprec	   = 300/digitwidth

int currprec	   = 500/digitwidth
!int currprec	   = 100'000'000/digitwidth
!!int currprec	   = 1000

int stblz	 	 	 !global set by smalltobig

ref constrec constlist=nil		!use linked list of constant values
global int decstrsize
varrec vtemp					!as used in free/dectemp

macro bn_free(x) = obj_free_dec(x)

global proc obj_free_dec(object p)=
	if p.length then
		pcm_free(p.num, p.length*decelemsize)
	fi

	pcm_free32(p)
end

global proc var_dupl_dec(variant a)=
!NOTE: because decimals are immutable, there may never be any need
!to duplicate a decimal; it can always be shared
!But I've done it now so...

	object p,q
	int size

	q:=a.objptr
	p:=obj_new()
	p.length:=q.length
	p.expon:=q.expon
	p.neg:=q.neg
	p.numtype:=q.numtype

	size:=q.length*decelemsize

	if size then
		p.num:=pcm_alloc(size)
		memcpy(p.num,q.num,size)
	fi

	a.objptr:=p
end

global proc var_empty_dec(variant dest)=
	dest.tagx:=tdecimal+hasrefmask
	dest.objptr:=makebignum(0)
end

global proc var_make_dec_str(ichar s, int length, variant dest)=
	dest.tagx:=tdecimal ior hasrefmask
	dest.objptr:=bn_makestr(s,length)
end

global proc var_make_dec_int(int a, variant dest)=
	dest.tagx:=tdecimal ior hasrefmask
	dest.objptr:=bn_makeint(a)
end

function badnumber:object c=
	c:=makebignum(0)
	c.numtype:=nan_type
	return c
end

function bn_makestr(ichar s, int length=0)object=
	ichar t,u,oldt
	int tlength
	int neg,dpindex,expon,nonzeros,talloc,dpseen
	int leadingzeros, trailingzeros,zerosafterdp
	int d,n,wd,dp,wdp,w,d2,na,nb, c
	object a

	if length=0 then
		length:=strlen(s)
	fi
	if length<=0 then
		return badnumber()
	fi

	t:=malloc(length+1)
	memcpy(t,s,length)
	(t+length)^:=0
	oldt:=t
	tlength:=length+1
	s:=t

	talloc:=length+1+10	 	!allow for extending last wdigit group

	neg:=0
	case s^
	when '+' then ++s
	when '-' then neg:=1; ++s
	esac

	t:=u:=pcm_alloc(talloc)	  !accummulate sig digits into t
	dpindex:=-1
	dpseen:=zerosafterdp:=0
	nonzeros:=0
	leadingzeros:=trailingzeros:=0
	expon:=0

!CPL =S

	do
		if (c:=s^) in '1'..'9' then
			u++^:=s++^
			trailingzeros:=0
			nonzeros:=1
		elsecase c
		when '0' then
			if nonzeros then
		 	   ++trailingzeros
		 	   u++^:=s++^
			else
		 	   ++leadingzeros
		 	   if dpseen then
		 	 	  ++zerosafterdp
		 	   fi
		 	   ++s
			fi
		when '_', '\'', '`', ' ',13,10 then
			++s
		when '.' then
			if dpseen or dpindex>=0 then return badnumber() fi
			if nonzeros then
		 	   dpindex:=u-t
			else
		 	   dpseen:=1
			fi
!	   trailingzeros:=0
			++s
		when 0 then
			exit
		when 'e','E' then
			expon:=readexpon(s+1)
			exit
		else
			return badnumber()
		end
	end

	u^:=0
	length:=u-t	 	 	   !new length of extracted digits
	if dpindex<0 then
		if dpseen then
	 	   dpindex:=-zerosafterdp
		else
	 	   dpindex:=length
		fi
	fi
	length-:=trailingzeros	  !adjust precision to ignore trailing zeros
	(t+length)^:=0

	if length=0 then
!		return obj_make_dec_int(0)
		return bn_makeint(0)
	fi

	d:=dpindex-1+expon
	n:=length
	dp:=0
	na:=1
	nb:=n-na

	w:=digitwidth

	if d>=0 then
		wd:=d/w
		wdp:=d rem w
	else
		d2:=abs(d+1)
		wd:=-(d2/w+1)
		wdp:=w-1-(d2 rem w)
	fi

	na:=wdp+1
	nb:=max(n-na,0)
	while nb rem w do ++nb od
	length:=nb/w+1
	u:=t+n
	to na+nb-n do
		u++^:='0'
	od
	n:=na+nb
	(t+n)^:=0

	a:=makebignum(length)
	a.neg:=neg
	a.expon:=wd
	u:=t
	a.num[0]:=strvaln(u,na)
	u+:=na
	
	for i:=1 to length-1 do
		a.num[i]:=strvaln(u,w)
		u+:=w
	od

	pcm_free(t,talloc)
	free(oldt)

	return a
end

function readexpon(ichar s)int=
!s points just after 'e' or 'E'
	int neg, expon, c
	neg:=expon:=0

	case s^
	when '+' then ++s
	when '-' then neg:=1; ++s
	esac

	do
		if (c:=s^) in '0'..'9' then
			expon:=expon*10+(s^-'0')
			++s
		elsecase c
		when '_', '\'', '`', ' ' then
			++s
		when 0 then
			exit
		else
			pcerror("make expon?")
		end
	end

	return (neg|-expon|expon)
end

function bn_makeint(int x)object a=
	[256]char str

	if x=0 then
		a:=makebignum(0)
	elsif x in 0..digitmax then
		a:=makebignum(1)
		a.num[0]:=x
	elsif -x in 0..digitmax then
		a:=makebignum(1)
		a.num[0]:=-x
		a.neg:=1
	else
		print @str,x
		a:=bn_makestr(str)
	fi

	return a
end

global function var_tostr_dec(variant a,int fmt)ichar=
	return obj_tostr_dec(a.objptr,fmt)
end

function obj_tostr_dec(object a,int fmt=0)ichar=
	int expon,upper
	ichar s,t

	t:=nil
	if a=nil then
		t:="<void>"
	else
		case a.numtype
		when zero_type then t:=(fmt='E' or fmt='F'|"0.0"|"0")
!		when inf_type then t:=(a.neg|"<-infinity>"|"<infinity>")
		when inf_type then t:=(a.neg|"-Infinity"|"Infinity")
		when nan_type then t:="<NaN>"
		esac
	fi

	if t then
		s:=pcm_alloc(decstrsize:=strlen(t)+1)
		strcpy(s,t)
		return s
	fi

	if fmt=0 or fmt='A' then
		if bn_isint(a) and (a.expon-a.length)*digitwidth<60 then
	 	   fmt:='I'
		elsif abs(a.expon*digitwidth)<60 then
	 	   fmt:='F'
		else
	 	   fmt:='E'
		fi
	fi

	if fmt='E' then
		s:=tostring_scient(a)
	else
		s:=tostring_float(a,fmt)
	fi
	return s
end

function tostring_scient(object a)ichar=
!a is an actual number
	ichar s,t
	int expon,nchars,n,shift
	int64 x,scale

	nchars:=3

	expon:=a.expon*digitwidth

	x:=a.num[0]
	scale:=1
	shift:=0
	while x>=10 do
		x:=x/10
		scale*:=10
		++expon
		++shift
	od

	nchars:=a.length*digitwidth+16	 !allow for 1., and exponent

	s:=t:=pcm_alloc(decstrsize:=nchars)

	if a.neg then
		t++^:='-'
	fi

	print @t,x,,"."
	t+:=strlen(t)

	if shift then
		print @t, shift:"v",,a.num[0]-x*scale:"z*"
		t+:=strlen(t)
	fi

	for i to a.length-1 do
		print @t,a.num[i]:mdigitfmt
		t+:=strlen(t)
	od

	while (t-1)^='0' and (t-2)^<>'.' do
		--t
	od

	print @t,"e",,expon
	t+:=strlen(t)
	t^:=0

	return s
end

function tostring_float(object a,int fmt)ichar=
!a is an actual number (not zero, infinity etc)
	int expon,upper,nchars,w,prel,n,showdot
	ichar s,t

	expon:=a.expon
	upper:=a.length-1

	if fmt='I' and bn_isint(a) then
		showdot:=0
	else
		showdot:=1
	fi

	w:=digitwidth
	nchars:=3	 	 	 !sign and trailing .0
	if expon<0 then
		nchars+:=abs(expon-1)*w
	fi
	nchars+:=a.length*w
	if expon-upper>0 then
		nchars+:=(expon-upper)*w
	fi
	nchars+:=8	 	 		!margin

!   s:=t:=bn_alloc(nchars)
	s:=t:=pcm_alloc(decstrsize:=nchars)
	
	if a.neg then
		t++^:='-'
	fi

	prel:=0
	if expon<0 then
		prel:=1
		t++^:='0'
		t++^:='.'
		to abs(expon)-1 do
	 	   to digitwidth do
	 	 	  t++^:='0'
	 	   od
		od
	fi

	for i:=0 to upper do
		print @t, a.num[i]:(i>0 or prel|mdigitfmt|"")
		t+:=strlen(t)
		

		if expon=i and i<upper and showdot then
	 	   t++^:='.'
		fi
	od

	to expon-upper do
!print "+"
		to digitwidth do
	 	   t++^:='0'
		od
	od
	if expon>=upper and showdot then
		t++^:='.'
		t++^:='0'
	fi

	t^:=0
	return s
end

function strvaln(ref char s,int n)int=	  !STRVALN
!convert first n chars of s to int value and return result will fit into 32 bits
	int a

	a:=0
	to n do
		if s^<>'_' then
	 	   a:=a*10+s^-'0'
		fi
		++s
	od
	return a
end

function bn_isint(object a)int =
	return a.length<=a.expon+1
end

global function obj_len_dec(object a)int=
!return number of digits in integer a
!CPL "OBLLENDEC",=BN_ISINT(A),=BN_ISZERO(A),=A.UDEC.LENGTH,FPNAMES[A.UDEC.NUMTYPE],=A.UDEC.NUMTYPE

RETURN BN_GETPREC(A)

	if not bn_isint(a) then
		return 0
	fi
	if BN_iszero(a) then
		return 1
	fi

	return strlen(strint(a.num[0]))+a.expon*digitwidth
end

global function bn_iszero(object a)int=
	return a.numtype=zero_type
end

global function var_equal_dec(variant a,b)int=
	return bn_equal(a.objptr, b.objptr)
end

global proc var_add_dec(variant a,b)=
	object dest:=bn_init()

	bn_add(dest, a.objptr,b.objptr)
	a.objptr:=dest
end

global proc var_sub_dec(variant a,b)=
	object dest:=bn_init()

	bn_sub(dest, a.objptr,b.objptr)
	a.objptr:=dest
end

global proc var_mul_dec(variant a,b)=
	object dest:=bn_init()

	bn_mul(dest, a.objptr,b.objptr)
	a.objptr:=dest
end

global proc var_div_dec(variant a,b)=
	object dest:=bn_init()

	bn_div(dest, a.objptr,b.objptr)
	a.objptr:=dest
end

global proc var_idiv_dec(variant a,b)=
	object dest:=bn_init()

	bn_idiv(dest, a.objptr,b.objptr)
	a.objptr:=dest
end

global proc var_irem_dec(variant a,b)=
	object dest:=bn_init()

	bn_irem(dest, a.objptr,b.objptr)
	a.objptr:=dest
end

global proc var_neg_dec(variant a)=
!-:=a
!do in place neg; caller should have duplicated value

	bn_negto(a.objptr)
end

global proc var_abs_dec(variant a)=
!abs:=a
!do in place abs; caller should have duplicated value

	bn_absto(a.objptr)
end

global function var_compare_dec(variant a,b)int=
	return bn_cmp(a.objptr, b.objptr)
end

function bn_cmp(object a,b)int=
	object d
	int neg

	if bn_equal(a,b) then
		return 0
	fi

	d:=bn_init()
	bn_sub(d,a,b)
	neg:=d.neg
	bn_free(d)
	return (neg|-1|1)
end

function bn_equal(object a,b)int=
	if a.numtype<>normal_type and a.numtype=b.numtype then
		return a.neg=b.neg
	fi

	if a.length<>b.length or 
	   a.numtype<>b.numtype or 
	   a.neg<>b.neg or 
	   a.expon<>b.expon then
		return 0
	fi

	if a.length=0 then return 1 fi

	return eqbytes(a.num,b.num,a.length*decelemsize)
end

function bn_add(object dest,a,b)int=
	int nega,negb

	case getbintype(a,b)
	when nn_types then
	when zz_types then
		bn_setzero(dest)
		return 1
	when nz_types then
		bn_dupl(dest,a)
		return 1
	when zn_types then
		bn_dupl(dest,b)
		return 1
	else
		bn_setnan(dest)
		return 0
	esac

	nega:=a.neg
	negb:=b.neg

	if not nega and not negb then	   !both positive
		bn_addu(dest,a,b)
	elsif nega and negb then	 	   !both negative
		bn_addu(dest,a,b)
		bn_negto(dest)
	elsif not nega and negb then		!a positive, b negative
		bn_subu(dest,a,b)
	else
		bn_subu(dest,b,a)	 	 	 !a negative, b positive
	fi

	return 1
end

function bn_sub(object dest,a,b)int=
	int nega,negb

	case getbintype(a,b)
	when nn_types then
	when zz_types then
		bn_setzero(dest)
		return 1
	when nz_types then
		bn_dupl(dest,a)
		return 1
	when zn_types then
		bn_dupl(dest,b)
		bn_negto(dest)
		return 1
	else
		bn_setnan(dest)
		return 0
	esac

	nega:=a.neg
	negb:=b.neg

	if not nega and not negb then	   !both positive
		bn_subu(dest,a,b)
	elsif nega and negb then	 	   !both negative
		bn_subu(dest,b,a)
	elsif not nega and negb then		!a positive, b negative
		bn_addu(dest,a,b)
	else	 	 	 	 	 	   !a negative, b positive
		bn_subu(dest,b,a)
	fi

	return 1
end

proc bn_addu(object dest,a,b)=
	int preca, precb, precc
	int uppera,upperb,upperc, offset, carry,expona,exponb
	int dc
	word j
	ref[0:]elemtype pa,pb
	ref elemtype pax,pbx
	ref elemtype c,c2

	if a.expon<b.expon then	   !A has definite smaller magnitude
		swap(a,b)	 	 	 	!make sure A is always bigger or (approx) equal
	fi

	expona:=a.expon
	exponb:=b.expon
	preca:=a.length
	precb:=b.length

	offset:=expona-exponb	 	  !for indexing B elements shift to match A
	uppera:=preca-1
	upperb:=precb-1

	if uppera>(upperb+offset) then  !A defines overall precision; B contained within A
		upperc:=uppera
	else	 	 	 	 		!B extends overall precision
		upperc:=upperb+offset
	fi
	precc:=upperc+1

	c:=makesmallnum(precc)	 	 !no space for carry
	carry:=0
	pa:=a.num
	pb:=b.num

	for i:=upperc downto 0 do	 	  !do the add, starting from ls digit

		j:=i-offset	 	 	 	  !index of A/C in terms of B
		if i<=uppera and j<=word(upperb) then
	 	   dc:=pa^[i]+pb^[j]+carry
		elsif i<=uppera then
	 	   dc:=pa^[i]+carry
		elsif j<=word(upperb) then
	 	   dc:=pb^[j]+carry
		else
	 	   dc:=carry
		fi

		if dc>=digitbase then
	 	   carry:=1
	 	   (c+i)^:=dc-digitbase
		else
	 	   (c+i)^:=dc
	 	   carry:=0
		fi
	od

	if carry then
		c2:=makesmallnum(precc+1)
		c2^:=carry
		memcpy(c2+1,c,precc*decelemsize)
		freesmall(c,precc)
		c:=c2
		++precc
	fi

	smalltobig(dest,c,precc,precc)

	dest.expon:=expona+carry
end

proc bn_subu(object dest,a,b)=
	int preca, precb, precc
	int uppera,upperb,upperc, offset, carry, expona
	int da,db,dc, isneg, z, newprec,diff
	word j
	ref[0:]elemtype pa,pb
	ref elemtype c

!can only do subtract when a>=b; do some basic checks
	isneg:=0
	if a.expon<b.expon then	   !A has definite smaller magnitude
		swap(a,b)	 	 	 	!make sure A is always bigger or (approx) equal
		isneg:=1
	fi

!know that a>=b, and that isneg might be true
retry:
	expona:=a.expon
	preca:=a.length
	precb:=b.length

	offset:=expona-b.expon	 	!for indexing B elements shift to match A
	uppera:=preca-1
	upperb:=precb-1

	if uppera>(upperb+offset) then  !A defines overall precision; B contained within A
		upperc:=uppera
	else	 	 	 	 		!B extends overall precision
		upperc:=upperb+offset
	fi
	precc:=upperc+1

	c:=makesmallnum(precc)
	carry:=0
	pa:=a.num
	pb:=b.num

	for i:=upperc downto 0 do	 	  !do the add, starting from ls digit
		j:=i-offset	 	 	 	  !index of A/C in terms of B
		if i<=uppera and j<=word(upperb) then

	 	   diff:=pa^[i]-pb^[j]-carry
		elsif i<=uppera then
	 	   diff:=pa^[i]-carry
		elsif j<=word(upperb) then
	 	   diff:=-pb^[j]-carry
		else
	 	   diff:=-carry
		fi

		if diff<0 then
	 	   carry:=1
	 	   (c+i)^:=diff+digitbase
		else
	 	   (c+i)^:=diff
	 	   carry:=0
		fi
		
	od

	if carry then
		if isneg then	 	  !already swapped
	 	   pcerror("SUBU/CARRY")
		fi
		swap(a,b)
		isneg:=1
		freesmall(c,precc)
		goto retry
	fi

	smalltobig(dest,c,precc,precc)
	dest.neg:=isneg
	dest.expon:=expona-stblz

end

function makebignum(int length)object=
!ndigits=0 to create a zero value
!these are wide digits
	object a

!	a:=pcm_alloc(objrec.bytes)
	a:=obj_new()
	if length then
		a.num:=pcm_alloc(length*decelemsize)
		a.numtype:=normal_type
	else
		a.num:=nil
		a.numtype:=zero_type
	fi
	a.length:=length

	a.expon:=0
	a.neg:=0

!CPL "MAKE BIG NUM IS ZERO",A.UDEC.EXPON
	return a
end

function makesmallnum(int length)ref elemtype=
	return pcm_alloc(length*decelemsize)
end

function smalltobig(object c, ref elemtype a, int length,alloc,offset=0)object =
!copy numeric data from smallnum into new object
!also normalises by removing trailing zeros and leading zeros
!sets up expon with assumption that sequence represents an int
!will also free alloc elemente of a, provided memory is not reused
!offset is to be added to a, when a doesn't point to original allocation

	ref elemtype p
	int leadingzeros, trailingzeros, nonzeros, newlength

	bn_setzero(c)

	p:=a
	leadingzeros:=trailingzeros:=nonzeros:=0
	to length do
		if p++^ then
	 	   nonzeros:=1
	 	   trailingzeros:=0
		else
	 	   if nonzeros then
	 	 	  ++trailingzeros
	 	   else
	 	 	  ++leadingzeros
	 	   fi
		fi
	od

	stblz:=leadingzeros

	if nonzeros then

		newlength:=length-trailingzeros-leadingzeros

		if newlength=length=alloc then	 	 !can use data in a directly
	 	   c.num:=cast(a)
		else
	 	   c.num:=cast(makesmallnum(newlength))
	 	   memcpy(c.num,a+leadingzeros,newlength*decelemsize)
	 	   freesmall(a+offset,alloc)
		fi
		c.length:=newlength
		c.numtype:=normal_type
		c.expon:=length-1-leadingzeros	 		!include trailing zeros, but not leading ones?
	elsif alloc then	 	 	 	 	 	 	  !result stays at zero
		freesmall(a+offset,alloc)
	fi

	return c
end

proc freesmall(ref elemtype p, int length)=
	pcm_free(p,length*decelemsize)
end

global function bn_init()object=
	object a

	a:=makebignum(0)
	return a
end

proc bn_setzero(object a)=
!clear digit memory only; clear descriptor to a zero number
	if a then
		if a.num then
	 	   freesmall(cast(a.num),a.length)
		fi
		a.num:=nil
		a.length:=0
		a.neg:=0
		a.expon:=0
		a.numtype:=zero_type
	fi
end

proc bn_move(object a,b)=
#move contents of b to a. Original value of a is cleared; b becomes zero

	bn_setzero(a)
	a.bignumdescr:=b.bignumdescr
	clear b.bignumdescr
end

proc bn_dupl(object a,b)=
#copy contents of b to a. Each copy is independent
	object c
	int size

	c:=bn_init()
	c^:=b^
	if c.length then
		c.num:=cast(makesmallnum(size:=c.length))
		memcpy(c.num,b.num, size*decelemsize)
	fi
	bn_move(a,c)
	bn_free(c)
end

proc bn_setinf(object dest) =
	bn_setzero(dest)
	dest.numtype:=inf_type
end

proc bn_setnan(object dest) =
	bn_setzero(dest)
	dest.numtype:=nan_type
end

global proc var_setnan(variant dest) =
	dest.tagx:=tdecimal+hasrefmask
	dest.objptr:=makebignum(0)
	bn_setnan(dest.objptr)
end

global proc var_setinf(variant dest) =
	dest.tagx:=tdecimal+hasrefmask
	dest.objptr:=makebignum(0)
	bn_setinf(dest.objptr)
end

function getbintype(object a,b)int=
!return bintype code for combination of a and b
	int atype:=a.numtype, btype:=b.numtype

	if atype=nan_type or btype=nan_type then
		return xx_types
	fi

	case atype
	when normal_type then
		case btype
		when normal_type then
	 	   return nn_types
		when zero_type then
	 	   return nz_types
		else
	 	   return ni_types
		esac
	when zero_type then
		case btype
		when normal_type then
	 	   return zn_types
		when zero_type then
	 	   return zz_types
		else
	 	   return zi_types
		esac
	else
		case btype
		when normal_type then
	 	   return in_types
		when zero_type then
	 	   return iz_types
		else
	 	   return ii_types
		esac
	esac

end

proc bn_negto(object a)=
	if not bn_iszero(a) then
		a.neg:=not a.neg
	fi
end

proc bn_absto(object a)=
	a.neg:=0
end

function bn_mul(object dest,a,b)int=
	int neg

	case getbintype(a,b)
	when nn_types then
	when zz_types,nz_types,zn_types then
		bn_setzero(dest)
		return 1
	else
		bn_setnan(dest)
		return 0
	esac

	neg:=a.neg<>b.neg
	bn_mulu(dest,a,b)
	if neg then	 !different signs
		bn_negto(dest)
	fi
	return 1
end

function bn_mulp(object dest,a,b, int prec)int=
	int res:=bn_mul(dest,a,b)
	if res then
		bn_setprec(dest,(prec=0|currprec|prec))
	fi
	return res
end

proc bn_mulu(object dest, a,b) =
!unsigned multiply, c:=a*b
!general scheme A1/B1 are least significant words
!x is total overflows (product overflow and carry) from previous column

!(A4 A3 A2 A1) * (B3 B2 B1)
!
!0	 0	 x	 A4.B1 A3.B1 A2.B1 A1.B1
!0	 x	 A4.B2 A3.B2 A2.B2 A1.B2 0
!x	 A4.B3 A3.B3 A2.B3 A1.B3 0	 0

	int uppera, upperb, upperc
	int precc,expona,exponb
	int ax,bx,cx		!indices within a,b,c
	int i,cx1, nc2, pd,pr
	i64 p,carry,x
	object d
	ref elemtype c
	i64 pdquot,pdrem

	expona:=a.expon
	exponb:=b.expon
	uppera:=a.length-1
	upperb:=b.length-1

	precc:=uppera+upperb+2
	nc2:=precc

!++NMULT

	c:=makesmallnum(nc2)
	memset(c,0,precc*decelemsize)
!   c.expon:=a.expon+b.expon+1
	cx:=precc-1

	for bx:=upperb downto 0 do
		carry:=0

		cx1:=cx
		for ax:=uppera downto 0 do
			p:=i64((a.num[ax]))*i64((b.num[bx]))+carry

			pd:=p/digitbase

!			assem
!				mov rdx,[p]
!				mov rcx, 19342813113834067
!				shr rdx,9
!				mov rax,rdx
!				mul rcx
!				xor eax,eax
!				shr rdx,11
!				mov [pd],rdx
!			end
!			pd:=p/digitbase

			pr:=p-pd*digitbase
!
!			x:=int64((c+cx1)^)+p rem digitbase
			x:=int64((c+cx1)^)+pr

			if x>digitmax then
				carry := pd+1
				(c+cx1--)^ := x-digitbase
			else
				carry:=pd
				(c+cx1--)^:=x
			fi

		od
		(c+cx1)^:=carry
		--cx	 	 	  !for next row, start at next column in dest
	od

	smalltobig(dest,c,precc,nc2)
	dest.expon:=expona+exponb+1-stblz

end

function smallmulto(ref elemtype p,q, int plen, m)int=
!multiply object sequence p inplace, by single digit m
!return new length (will be plen or plen+1, unless result is zero)
!p must be long enough to store the extra digit

	ref elemtype pp,qq
	int carry,d

	case m
	when 0 then
		p^:=0
		return 1
	when 1 then
		memcpy(p,q,plen*decelemsize)
		return plen
	esac

	pp:=p+plen-1
	qq:=q+plen-1
	carry:=0

	to plen do
		d:=int64(qq^)*m+carry
		pp^:=d rem digitbase
		carry:=d/digitbase
		--qq
		--pp
	od

	if carry then	 	 	 !need extra digit
		pp:=p+plen
		to plen do
	 	   pp^:=(pp-1)^
	 	   --pp
		od
		pp^:=carry
		++plen
	fi

	return plen
end

function bn_div(object dest,a,b,int prec=0)int=
	int neg

	case getbintype(a,b)
	when nn_types then
	when zn_types then
		bn_setzero(dest)
		return 1
	when zz_types,nz_types then
		bn_setinf(dest)
		return 0
	else
		bn_setnan(dest)
		return 0
	esac

	neg:=a.neg<>b.neg

	bn_fdivu(dest,a,b,prec)

	if neg then
		bn_negto(dest)
	fi
	return 1
end

function bn_idiv(object dest,a,b)int=
	int neg
	case getbintype(a,b)
	when nn_types then
	when zn_types then
		bn_setzero(dest)
		return 1
	when zz_types,nz_types then
		bn_setinf(dest)
		return 0
	else
		bn_setnan(dest)
		return 0
	esac

	neg:=a.neg<>b.neg
	bn_idivu(dest,a,b)
	if neg then
		bn_negto(dest)
	fi
	return 1
end

function bn_idivrem(object dest,rm,a,b)int=
	int nega,negb

	case getbintype(a,b)
	when nn_types then
	when zn_types then
		bn_setzero(dest)
		bn_setzero(rm)
		return 1
	when zz_types,nz_types then
		bn_setinf(dest)
		bn_setzero(rm)
		return 0
	else
		bn_setnan(dest)
		return 0
	esac

	nega:=a.neg
	negb:=b.neg
	bn_idivu(dest,a,b,rm)
	if nega<>negb then	  !different signs
		bn_negto(dest)
	fi
	if nega then bn_negto(rm) fi
	return 1
end

function bn_irem(object dest,a,b)int=
	object rm,d
	int nega

	case getbintype(a,b)
	when nn_types then
	when zn_types then
		bn_dupl(dest,b)
		return 1
	when zz_types,nz_types then
		bn_setinf(dest)
		bn_setzero(dest)
		return 0
	else
		bn_setnan(dest)
		return 0
	esac

	nega:=a.neg
	d:=bn_init()
	bn_idivu(d,a,b,dest)
	if nega then bn_negto(dest) fi
	bn_free(d)
	return 1
end

proc bn_idivu(object dest,a,b,rm=nil)=
#neither a nor b are zero; both are positive
#integer divide

	ref elemtype c,x,e
	int expona, exponb, badjust, exponc
	int na,nb,nc,nx,ne,nx2,ne2, cx,nupper
	int uppera, upperb, upperc
	int n, k, nexta
	int64 xx,y
	ref elemtype pa,pb

	na:=a.length
	nb:=b.length
	expona:=a.expon
	exponb:=b.expon
	badjust:=exponb+1-nb

	if na>expona+1 or nb>exponb+1 then
		pcerror("idivu:a or b not int")
	fi
	nc:=expona+1

	if expona<exponb then
		bn_setzero(dest)
		if  rm then
	 	   bn_dupl(rm,a)
		fi
		return
	fi

	uppera:=na-1
	upperb:=nb-1
	upperc:=nc-1
	pa:=cast(a.num)
	pb:=cast(b.num)	 	   !p is not zero, and all digits of interest are present

!x is the moving and changing window into a that b is divided into get next digit of result
!use a permanently allocated smallnum, 1 digit wider than b
	n:=nb	 	 	 	!n is also how many digits of a we're into so far
	x:=makesmallnum(nx2:=n+1)	   !allow one extra digit
	nx:=n	 	 	 	 	   !current x size
	nupper:=nc-badjust

	for i:=0 to upperb do
		if i<=uppera then
	 	   (x+i)^:=(pa+i)^
		else
	 	   (x+i)^:=0
		fi
	od

	c:=makesmallnum(nc)
	cx:=0

	do
		k:=smalldiv(x,pb,nx,nb)

		(c+cx++)^:=k
		if n>=nupper then	 	 	 	!finished with A 
	 	   exit
		fi

		nexta:=(n>uppera|0|(pa+n)^)
		++n
		if nx=1 and x^=0 then
	 	   x^:=nexta	 	 	 !x is 1 digit long
		else
	 	   (x+nx)^:=nexta	 	 !next digit from a
	 	   ++nx
		fi
	od

	if rm and exponb<nb then		!no trailing zeros in b
		smalltobig(rm,x,nx,nx2)
	else
		freesmall(x,nx2)
	fi

	if cx=1 and c^=0 then
		freesmall(c,nc)
		bn_setzero(dest)
		if rm then
	 	   bn_dupl(rm,a)
		fi
		return
	fi

	if c^=0 and cx>=2 then	 	 	!leading zero (may not need cx check)
		smalltobig(dest,c+1,cx-1,nc,-1)
	else
		smalltobig(dest,c,cx,nc)
	fi
!   freesmall(c,nc)

	if rm and exponb>=nb then	 	  !has trailing zeros so natural rem doesn't work
		object d
		d:=bn_init()
		bn_mulu(d,b,dest)
		bn_subu(rm,a,d)
		bn_free(d)
	fi

end

proc bn_fdivu(object dest,a,b,int precision)=
!neither a nor b are zero; both are positive
!integer divide

	ref elemtype c,x,e
	int expona, exponb, badjust, exponc
	int na,nb,nc,nx,ne,nx2,ne2, cx,nupper,nc2
	int uppera, upperb, upperc
	int n, k, nexta
	int64 xx,y
	ref elemtype pa,pb

	na:=a.length
	nb:=b.length
	expona:=a.expon
	exponb:=b.expon

	if precision then
		precision:=((precision-1)/digitwidth+1)	 	!must be multiple of digitwidth
	else
		precision:=currprec
	fi
	nc:=precision

	uppera:=na-1
	upperb:=nb-1
	upperc:=nc-1
	pa:=cast(a.num)
	pb:=cast(b.num)	 	   !p is not zero, and all digits of interest are present

!x is the moving and changing window into a that b is divided into get next digit of result
!use a permanently allocated smallnum, 1 digit wider than b
	n:=nb	 	 	 	!n is also how many digits of a we're into so far
	x:=makesmallnum(nx2:=n+1)	   !allow one extra digit
	nx:=n	 	 	 	 	   !current x size

	for i:=0 to upperb do
		if i<=uppera then
	 	   (x+i)^:=(pa+i)^
		else
	 	   (x+i)^:=0
		fi
	od

	c:=makesmallnum(nc2:=nc+1)
	cx:=0

	do
		k:=smalldiv(x,pb,nx,nb)

		(c+cx++)^:=k

		if cx>nc then	 	 	 !reached given precision
	 	   exit
		fi

		nexta:=(n>uppera|0|(pa+n)^)
		++n
		if nx=1 and x^=0 then
	 	   x^:=nexta	 	 	 !x is 1 digit long
		else
	 	   (x+nx)^:=nexta	 	 !next digit from a
	 	   ++nx
		fi
	od

	freesmall(x,nx2)

	if cx=1 and c^=0 then
		freesmall(c,nc2)
		bn_setzero(dest)
		return
	fi

	if c^=0 and cx>=2 then	 	 	!leading zero (may not need cx check)
		smalltobig(dest,c+1,cx-1,nc2,-1)
		dest.expon:=expona-exponb-1
	else
		smalltobig(dest,c,cx,nc2)
		dest.expon:=expona-exponb
	fi
!   freesmall(c,nc2)
end

function smalldiv(ref elemtype x, b, int &xlen, nb)int =
!x,b are smallnums: arrays of elements, of the exact lengths given
!x is same length as b, or at most one element longer
!(x can also be smaller, but then result is just 0)
!return integer x/b as machine word type 0..digitmax
!when digits are 0..9, then result of x/b is always going to be 0 to 9.

	int k,count
	int64 xx,y
	elemtype xi,bi
	ref elemtype e
	int esize,ne,nx

	nx:=xlen
	k:=0
	count:=0
	e:=makesmallnum(esize:=(nb+1))

	do
		if nx<nb then	 	 	 !completed this k
	 	   exit
		elsif nx>nb then	 	   !x will be at most 1 digit wider than b
	 	   xx:=int64(x^)*digitbase+int64((x+1)^)
	 	   y:=xx/(b^+1)
		else	 	 	 	 	 	   !x,b are same length
	 	   if x^>=(b^+1) then
	 	 	  y:=x^/(b^+1)
	 	   else
	 	 	  y:=1
	 	 	  for i:=0 to nb-1 do
	 	 	 	 xi:=(x+i)^
	 	 	 	 bi:=(b+i)^
	 	 	 	 if xi<bi then
	 	 	 	 	y:=0
	 	 	 	 	exit all
	 	 	 	 elsif xi>bi then
	 	 	 	 	exit
	 	 	 	 fi
	 	 	  od

	 	   fi
		fi
		k+:=y
		if y>1 then
	 	   ne:=smallmulto(e,b,nb,y)
	 	   nx:=smallsubto(x,e,nx,ne)
		elsif y then
	 	   nx:=smallsubto(x,b,nx,nb)
		else
	 	   pcerror("smalldiv:Y=0")
		fi
	od

	freesmall(e,esize)
	xlen:=nx	 	 	 	 !return modified x, and new length of x
	return k
end

function smallsubto(ref elemtype p,q, int plen, qlen)int=
!subtract q from p, return new length. New p will be moved up if smaller
!p>=q, and plen>=qlen
	ref elemtype pp,qq
	int carry,diff,z

	pp:=p+plen-1
	qq:=q+qlen-1
	carry:=0
	z:=0	 	 	 	 !leading zeros

	to plen do
		if qq>=q then
	 	   diff:=pp^-qq^-carry
	 	   --qq
		else
	 	   diff:=pp^-carry
		fi

		if diff<0 then
	 	   carry:=1
	 	   pp^:=diff+digitbase
		else
	 	   pp^:=diff
	 	   carry:=0
		fi
		if pp^ then
	 	   z:=0
		else
	 	   ++z
		fi
		--pp
	od
	if carry then pcerror("SSUBTO/CARRY?") fi

	if z=plen then --z fi	 	  !result is zero, needs at least one digit

	if z then
		plen-:=z
		pp:=p
		qq:=p+z
		to plen do
	 	   pp++^:=qq++^
		od
	fi

	return plen
end

function bn_getprec(object a)int=
	return a.length*digitwidth
end

proc bn_setprec(object a,int prec)=
	int oldlength,newlength
	object c

	if a.numtype<>normal_type then
		return
	fi

	if prec<1 or prec>maxprec then
		return
	fi

!prec is digit count, not words
	prec:=((prec-1)/digitwidth+1)*digitwidth		!must be multiple of digitwidth

!prec should be rounded up as needed to next multiple of digitwith
	newlength:=prec/digitwidth	 	 	 	   !no. words

	oldlength:=a.length

	if oldlength<=newlength then
		return
	fi

	c:=makebignum(newlength)
	c.neg:=a.neg
	c.expon:=a.expon

	for i:=0 to newlength-1 do
		if i<oldlength then
	 	   c.num[i]:=a.num[i]
		else
	 	   c.num[i]:=0
		fi
	od

	bn_move(a,c)
	bn_free(c)
end

function bn_getglobalprec:int=
	return currprec*digitwidth
end

proc bn_setglobalprec(int prec)=
	currprec:=((prec-1)/digitwidth+1)
end

function bn_makefloat(real64 x)object =
	object a
	[2048]char str

!	sprintf(&.str,"%.15g",x)

	print @str, x:".15g"

	return bn_makestr(&.str)
end

global function dectemp(variant a)variant=
!turn int/real into temporary 
	vtemp.tagx:=tdecimal+hasrefmask

	case a.tag
	when tint then
		vtemp.objptr:=bn_makeint(a.value)
	when treal then
		vtemp.objptr:=bn_makefloat(a.xvalue)
	else
		pcerror("dectemp")
	esac
	a^:=vtemp
	return a
end

global proc freedectemp=
	bn_free(vtemp.objptr)
end

proc bn_ipower(object d, a,int64 n)=
#return a**b for bigints
	object e,f

	if n<0 then
		bn_setzero(d)

	elsif n=0 then
		bn_move(d,bn_makeint(1))

	elsif n=1 then
		bn_dupl(d,a)
!
	elsif (n iand 1)=0 then
		e:=bn_init()
		bn_mulu(e,a,a)
		bn_ipower(d,e,n/2)
		bn_free(e)	  

	else	 	   !assume odd
		e:=bn_init()
		f:=bn_init()
		bn_mulu(e,a,a)
		bn_ipower(f,e,(n-1)/2)
		bn_mulu(d,a,f)
		bn_free(e)
		bn_free(f)

	fi
end

global proc var_power_dec(variant a, int n)=
	object dest:=bn_init()
	bn_ipower(dest,a.objptr,n)
	a.objptr:=dest
end

global function var_convert_dec_int(variant a)int=
!CPL "CONVINT"
	return bn_toint(a.objptr)
end

function bn_toint(object a)int64=
	int64 x
	if not bn_isint(a) then
		pcerror("dec-float->int not ready")
		return 0
	fi
	if bn_iszero(a) then
		return 0
	fi

	x:=0
!	if a.expon>2 or a.expon=2 and a.num[2]>9 then
!		pcerror("dec->int overflow")
!	fi

	for i:=0 to a.length-1 do
		x:=x*digitbase+a.num[i]
	od

	for i:=a.length to a.expon do
		x*:=digitbase
	od

	if a.neg then
		return -x
	else
		return x
	fi
end

