import msys
import mlib
import clib
import oslib

import mm_decls
import mm_tables
import mm_support
import mm_lib

import* mm_pcl

macro hashc(hsum,c)=hsum<<4-hsum+c
macro hashw(hsum)=(hsum<<5-hsum)

const maxstackdepth=20
[maxstackdepth]ref char lxstart_stack
[maxstackdepth]ref char lxsptr_stack
[maxstackdepth]int lxfileno_stack
[maxstackdepth]int lxlineno_stack
[maxstackdepth]byte isfile_stack
int sourcelevel=0

const cr	= 13
const lf	= 10
const tab	= 9

ref char lxstart
ref char lxsptr
int lxifcond
int longsuffix			!for real nos

int lxfileno
global const hstsize	= 32768
global const hstmask	= hstsize-1

global [0:hstsize]strec hashtable
global [0:hstsize]word hashkeys
symbol hashtablelast

global int astringlength

byte prescanmode=0

[]ichar maxnumlist=(
	"",					!1
	"1111111111111111111111111111111111111111111111111111111111111111",   	!2
	"11112220022122120101211020120210210211220",                          	!3
	"33333333333333333333333333333333",                                   	!4
	"2214220303114400424121122430",                                       	!5
	"3520522010102100444244423",                                          	!6
	"45012021522523134134601",                                            	!7
	"1777777777777777777777",                                             	!8
	"145808576354216723756",                                              	!9
	"18446744073709551615",                                               	!10
	"335500516A429071284",                                                	!11
	"839365134A2A240713",                                                 	!12
	"219505A9511A867B72",                                                 	!13
	"8681049ADB03DB171",                                                  	!14
	"2C1D56B648C6CD110",                                                  	!15
	"FFFFFFFFFFFFFFFF")                                                   	!16
[maxnumlist.len]int maxnumlen

global proc lex=
	int lena,lenb
	ref char p

	lx:=nextlx				!grab that already read basic token
reenter::
	lexreadtoken()
reenter2::

	switch nextlx.symbol
	when kcasesym,kswitchsym,kdocasesym,kdoswitchsym,kforsym,
			kdosym,ktosym,kprocsym,kfunctionsym,kimportmodulesym,kunlesssym,
			krecordsym,kstructsym,kunionsym,ktypesym,kwhilesym,kclasssym,
			ktrysym,ktabledatasym,kassemsym,kifsym then

		if lx.symbol=kendsym then
			if lx.subcode then lxerror("end if if?") fi
			lx.subcode:=nextlx.symbol
			reenter
		fi

	when eolsym then
		if lx.symbol in [commasym, lsqsym, lbracksym] then !ignore eol
			reenter
		elsif symboloptypes[lx.symbol]=bin_op and not assemmode and 
			lx.symbol not in [maxsym, minsym] then
			reenter
		fi
		nextlx.symbol:=semisym

	when stringconstsym then
		if lx.symbol=stringconstsym then
			lena:=strlen(lx.svalue)
			lenb:=strlen(nextlx.svalue)
			p:=pcm_alloc(lena+lenb+1)
			memcpy(p,lx.svalue,lena)
			memcpy(p+lena,nextlx.svalue,lenb)
			(p+lena+lenb)^:=0
			lx.svalue:=p
		fi
	when ksourcedirsym then
		if not dolexdirective(nextlx.subcode) then		!skip symbol
			reenter
		fi

	when namesym then
		if nextlx.subcode=unitnamesym then
			case lx.symbol
			when intconstsym then
				if lx.subcode in [ti128,tu128] then
					lxerror("No suffix on i128/u128")
				fi
				case nextlx.symptr.index
				when million_unit then lx.value *:= 1 million
				when billion_unit then lx.value *:= 1 billion
				when thousand_unit then lx.value *:= 1 thousand
				when kilo_unit then lx.value *:= 1024
				when mega_unit then lx.value *:= 1048576
				when giga_unit then lx.value *:= (1048576*1024)
				else
					lxerror("Can't do this unit index")
				esac
				lx.subcode:=setinttype(lx.value)
				reenter
			when realconstsym then
				lxerror("Unit suffix after float not implem")
			else
				nextlx.symbol:=namesym
			esac
		fi
	when machinetypesym then
		case nextlx.subcode
		when 'I','i' then nextlx.subcode:=ti64
		when 'W','w' then nextlx.subcode:=tu64
		esac
		nextlx.symbol:=stdtypesym

	when rawxnamesym then
		nextlx.symbol:=namesym

	when insym then
		if lx.symbol=notlsym then
			lx.symbol:=notinsym
			lx.subcode:=knotin
			reenter
		fi
	when eqsym then
		if lx.symbol=notlsym then
			lx.symbol:=cmpsym
			lx.subcode:=kne
			reenter
		fi
	end switch

	nextlx.pos :=nextlx.pos ior lxfileno<<24
end

global proc lexreadtoken=
!read next token into nextlx
	int c,hsum,commentseen,hashindex,length
	ref char pstart,pnext,p,ss,lxsvalue

	nextlx.subcode:=0

	doswitch lxsptr++^
	when 'a'..'z','_','$',0x80..0xEE, 0xF0..0xFF then
		lxsvalue:=lxsptr-1
	doname::
		hsum:=lxsvalue^

		doswitch c:=lxsptr++^
		when 'a'..'z','0'..'9','_','$',0x80..0xEE, 0xF0..0xFF then
			hsum:=hashc(hsum,c)
		when 'A'..'Z' then
			(lxsptr-1)^:=c+' '
			hsum:=hashc(hsum,c+' ')
		when '"' then
			--lxsptr
			if lxsvalue+1=ref char(lxsptr) then
				case lxsvalue^
				when  'F','f','R','r' then 
					readrawstring()
					return
				when  'A','a','Z','z' then 
					readarraystring(lxsvalue^)
					return
				esac
			fi
			exit
		else
			--lxsptr
			exit
		end doswitch

		lookup(lxsvalue, lxsptr-lxsvalue, hashw(hsum))
		return

	when 'A'..'Z' then
		lxsvalue:=lxsptr-1
		lxsvalue^+:=32
		goto doname

	when '0'..'9' then
		c:=(lxsptr-1)^
		case lxsptr^
		when ' ',')',cr,',','|' then		!assume single digit decimal
			nextlx.symbol:=intconstsym
			nextlx.subcode:=tint
			nextlx.value:=c-'0'
		when 'x','X' then
			case c
			when '0' then		!0x
				++lxsptr
				readnumber(16)
			when '1' then
				lxerror("Bad base")
			else				!other base 2..9
				++lxsptr
				readnumber(c-'0')
			esac
		elsif c='1' and lxsptr^ in '0'..'6' and (lxsptr+1)^ in ['x','X'] then
			int base:=lxsptr^+(10-'0')
			lxsptr+:=2
			readnumber(base)

		else
			--lxsptr
			readdecimalnumber()
		esac
		return

	when '!' then			!comment to eol
	docomment::
		doswitch c:=lxsptr++^
		when 13 then
			++lxsptr
			exit
		when 10 then
			exit
		when 0 then
			--lxsptr
			exit
		end
		++nextlx.pos
		nextlx.symbol:=eolsym
		return

	when '#' then			!docstring to eol
		lxsvalue:=cast(lxsptr)

		doswitch c:=lxsptr++^
		when 13,10,0 then			!leave eol for next symbol
			--lxsptr
			exit
		end

		length:=lxsptr-cast(lxsvalue,ref char)
		nextlx.symbol:=docstringsym
		nextlx.svalue:=pcm_copyheapstringn(lxsvalue,length)
		return

	when '\\' then			!line continuation

!two stages::
! 1: read chars until any eol chars (unless further '\' seen)
! 2: read until non-white space
		commentseen:=0
		doswitch lxsptr++^			!read until end of this line
		when cr then
			++nextlx.pos
			++lxsptr				!skip lf
			exit
		when lf then
			++nextlx.pos
			exit
		when 0 then
			nextlx.symbol:=eofsym
			--lxsptr
			return
		when ' ',tab then
		when '!' then
			commentseen:=1
		else
			if not commentseen then
				lxerror("\\ not followed by eol")
			fi
	enddoswitch
!eol seen: now skip 0 or more further eol chars, plus any white space (ie. multiple blank lines)

		doswitch lxsptr++^
		when cr then
			++nextlx.pos
			++lxsptr				!skip lf
		when lf then
			++nextlx.pos
		when ' ',tab then
		else
			--lxsptr
			exit
		enddoswitch

	when '{' then
		nextlx.symbol:=lcurlysym
		return

	when '}' then
		nextlx.symbol:=rcurlysym
		return

	when '.' then
		switch lxsptr^
		when '.' then				!.. or ...
			++lxsptr
			if lxsptr^='.' then
				++lxsptr
				nextlx.symbol:=ellipsissym
			else
				nextlx.symbol:=rangesym
				nextlx.subcode:=j_makerange		!helps treat as opsym which all have k-code as subcode
			fi
			return
		when '0'..'9' then			!real const: deal with this after the switch
			--lxsptr
			readrealnumber(nil,0,10)
			return
		else
			nextlx.symbol:=dotsym
			return
		endswitch

	when ',' then
		nextlx.symbol:=commasym
		return

	when ';' then
		nextlx.symbol:=semisym
		return

	when ':' then
		switch lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=assignsym
			nextlx.subcode:=j_assign		!helps treat as opsym which all have k-code as subcode
		when ':' then
			++lxsptr
			case lxsptr^
			when '=' then
				++lxsptr
				nextlx.symbol:=deepcopysym
				nextlx.subcode:=j_deepcopy
			else
				nextlx.symbol:=dcolonsym
			esac
		else
			nextlx.symbol:=colonsym
		endswitch
		return

	when '(' then
		nextlx.symbol:=lbracksym
		return

	when ')' then
		nextlx.symbol:=rbracksym
		return

	when '[' then
		nextlx.symbol:=lsqsym
		return

	when ']' then
		nextlx.symbol:=rsqsym
		return

	when '|' then
		if lxsptr^='|' then
			++lxsptr
			nextlx.symbol:=dbarsym
		else
			nextlx.symbol:=barsym
		fi
		return

	when '^' then
		nextlx.symbol:=ptrsym
		return

	when '@' then
		if lxsptr^='@' then
			++lxsptr
			nextlx.symbol:=datsym
		else
			nextlx.symbol:=atsym
		fi
		return

	when '?' then
		nextlx.symbol:=questionsym
		return

	when '~' then
		nextlx.symbol:=curlsym
		return

	when '+' then
		nextlx.symbol:=addsym
		if lxsptr^='+' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=kincr
			return
		fi
		return

	when '-' then
		nextlx.symbol:=subsym
		if lxsptr^='-' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=kdecr
			return
		fi
		return

	when '*' then
		if lxsptr^='*' then
			++lxsptr
			nextlx.symbol:=powersym
		else
			nextlx.symbol:=mulsym
		fi
		return

	when '/' then
		nextlx.symbol:=divsym
		return

	when '%' then
		nextlx.symbol:=idivsym
		return

	when '=' then
		case lxsptr^
		when '>' then
			nextlx.symbol:=sendtosym
			++lxsptr
		when '=' then
			nextlx.symbol:=samesym
			++lxsptr
		else
			nextlx.symbol:=eqsym
			nextlx.subcode:=keq
		esac
		return

	when '<' then
		nextlx.symbol:=cmpsym
		switch lxsptr^
		when '=' then
			++lxsptr
			nextlx.subcode:=kle
		when '>' then
			++lxsptr
			nextlx.subcode:=kne
		when '<' then
			++lxsptr
			nextlx.symbol:=shlsym
		else
			nextlx.subcode:=klt
		endswitch
		return

	when '>' then
		nextlx.symbol:=cmpsym
		switch lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=cmpsym
			nextlx.subcode:=kge
		when '>' then
			++lxsptr
			nextlx.symbol:=shrsym
		else
			nextlx.symbol:=cmpsym
			nextlx.subcode:=kgt
		endswitch
		return

	when '&' then
		case lxsptr^
!			when '&' then
!			++lxsptr
!			nextlx.symbol:=opsym
!			nextlx.subcode:=j_andand
		when '.' then
			++lxsptr
			nextlx.symbol:=anddotsym
			nextlx.subcode:=0
		else
			nextlx.symbol:=addrsym
			nextlx.subcode:=j_addrof
		esac
		return

	when '\'' then
		lxreadstring('\'')
		return

	when '"' then
		lxreadstring('"')
		return

	when '`' then
		readrawxname()
		return

	when ' ',tab then

	when cr then
		++lxsptr				!skip lf
		++nextlx.pos
		nextlx.symbol:=eolsym
		return
	when lf then			!only lfs not preceded by cr
		++nextlx.pos
		nextlx.symbol:=eolsym
		return

	when 0 then
		if sourcelevel then
			unstacksource()
		else
			nextlx.symbol:=eofsym
			--lxsptr
			return
		fi

	when 0xEF then			!BOM
		lxsptr+:=2

	else
		nextlx.symbol:=errorsym
		return

	end doswitch

end

proc readnumber(int base)=
!lxsptr positioned at first digit of number (could be separator)
!base is 2 to 10, or 16
	ref char pstart,dest
	int c
	ref char p

	dest:=pstart:=lxsptr

	if base=10 then
		doswitch c:=lxsptr++^
		when '0'..'9' then
			dest++^:=c
		when '_','\'','`' then
		else
			--lxsptr
			exit
		end doswitch
	else
		dest:=scannumber(base)
		c:=lxsptr^
	fi

	switch c			!terminator character
	when '.' then		!possible real number
		if (lxsptr+1)^<>'.' then

			readrealnumber(pstart,dest-pstart,base)
			return
		fi
	when 'e','E' then
		if base<15 then
			readrealnumber(pstart,dest-pstart,base)
			return
		fi
	when 'p','P' then
		if base>=15 then
			readrealnumber(pstart,dest-pstart,base)
			return
		fi
	end switch

	stringtonumber(pstart,dest-pstart,base)
end

proc readdecimalnumber=
!lxsptr positioned at first digit of number (could be separator)
!base is 2 to 10, or 16
	ref char pstart,dest
	int c,n,base,suffix
	ref char p

	dest:=pstart:=lxsptr
	suffix:=0

	doswitch c:=lxsptr++^
	when '0'..'9' then
		dest++^:=c
	when '_','\'','`' then
	else
		--lxsptr
		exit
	end doswitch

	switch c			!terminator character
	when '.' then		!possible real number
		if (lxsptr+1)^<>'.' then

			readrealnumber(pstart,dest-pstart,10)
			return
		fi
	when 'e','E' then
		readrealnumber(pstart,dest-pstart,10)
		return
	when 'b','B' then
		++lxsptr
		n:=dest-pstart
		p:=pstart
		to n do
			if p^<'0' or p^>'1' then
				lxerror("1101B: bad digit")
			fi
			++p
		od
		stringtonumber(pstart,n,2)
		return
	end switch

	stringtodecimalnumber(pstart,dest-pstart,suffix)
end

proc readrealnumber(ichar intstart, int intlen, base)=
!'e' or '.' has been encountered, possibly after a string of digits
!intstart points to int prefix, or is nil
!lxsptr still points at '.', 'e' or 'E' (might be 'p' or 'P' for hex base)
!read entire numbers, convert to real value in lx.xvalue
	ref char fractstart,ss
	int fractlen,expon,i,c,n
	real basex,x
	const maxrealdigits=500
	[maxrealdigits]char realstr
	[32]char str

	fractstart:=nil
	fractlen:=0
	expon:=0
	longsuffix:=0

	if lxsptr^='.' then		!read
		fractstart:=++lxsptr
		fractlen:=scannumber(base)-fractstart
	fi

	case lxsptr^
	when 'e','E' then
		if base<15 then
			++lxsptr
			expon:=readexponent(base)
		fi
	when 'p','P' then
		if base>=15 then
			++lxsptr
			expon:=readexponent(base)
		fi
	when 'l','L' then
		if longsuffix then lxerror("LL?") fi
		longsuffix:='L'
		++lxsptr

	esac

	if longsuffix='L' then
		ss:=pcm_alloc(intlen+fractlen+16)		!add ".", "e", exponent, 0 terminator
		memcpy(ss,intstart,intlen)
		memcpy(ss+intlen,".",1)
		memcpy(ss+intlen+1,fractstart,fractlen)
		memcpy(ss+intlen+fractlen+1,"e",1)
		getstrint(expon,&.str)
		memcpy(ss+intlen+fractlen+2,&.str,strlen(&.str)+1)

		nextlx.symbol:=decimalconstsym
		nextlx.svalue:=ss
		return
	fi

	if intlen+fractlen>maxrealdigits then
		lxerror("Real too long")
	fi
	if intlen then
		memcpy(&realstr,intstart,intlen)
	fi
	if fractlen then
		memcpy(&realstr[1]+intlen,fractstart,fractlen)
	fi

	if base=10 then
		x:=readrealbest(intlen,fractlen,expon,&.realstr)
	else
		basex:=base
		expon-:=fractlen
		x:=0.0
		for i:=1 to intlen+fractlen do		!digits already range-checked
			c:=realstr[i]
			if c>='0' and c<='9' then
				x:=x*basex+c-'0'
			elsif c>'a' then
				x:=x*basex+c-'a'+10
			else
				x:=x*basex+c-'A'+10
			fi
		od

		if expon>=0 then
			to expon do
				x*:=basex
			od
		else
			to -expon do
				x/:=basex
			od
		fi
	fi

	nextlx.symbol:=realconstsym
	nextlx.subcode:=treal
	nextlx.xvalue:=x
end

function readrealbest(int intlen,fractlen,expon, ichar realstr)real=
	[32]char expstr

	(realstr+intlen+fractlen)^:=0
	expon-:=fractlen

	print @&.expstr,"e",,expon
	strcat(realstr,&.expstr)
	return strtod(realstr,nil)
end

function readexponent(int base)int=
!positioned just after 'e' etc
!read exponent, which can have optional + or -, and return actual exponent value
	ref char numstart,numend
	int expon,length,neg

	neg:=0
	case lxsptr^
	when '+' then ++lxsptr
	when '-' then ++lxsptr; neg:=1
	esac

	numstart:=lxsptr
	length:=scannumber(base)-numstart

	if length=0 then
		lxerror("Bad expon")
	fi

	stringtonumber(numstart, length, base)
	return (neg|-lx.value|lx.value)
end

global proc printsymbol(ref tokenrec lp)=
	tokenrec l
	l:=lp^

	printf("%-18s",symbolnames[l.symbol])

	case l.symbol
	when namesym then
		printstrn(l.symptr.name,l.symptr.namelen)

		if l.subcode then
			fprint " [#]",symbolnames[l.subcode]
		fi

	when intconstsym then
		case l.subcode
		when tint then print l.value,"int"
		when tword then print l.uvalue,"word"
		else print l.value
		esac

	when realconstsym then
		print l.xvalue

	when stringconstsym then
		print """"
		printstr(l.svalue)
		print """",strlen(l.svalue)
	when charconstsym then
		print "'"
		printstr(l.svalue)
		print "'"
	when decimalconstsym then
		printstr(l.svalue)
		print "L"
	when assignsym,addrsym,ptrsym,deepcopysym,rangesym,
		andlsym,orlsym,eqsym,cmpsym,addsym,subsym,
		mulsym,divsym,idivsym,iremsym,iandsym,iorsym,ixorsym,shlsym,shrsym,
		minsym,maxsym,powersym,samesym then
		print symbolnames[l.symbol]
	elsif l.subcode then
		fprint "SUBCODE:",l.subcode
!	fprint "#",symbolnames[l.subcode]
	end

	println

end

proc stringtonumber(ichar s, int length, base)=
!convert decimal number s to an i64 value
!s contains only digits
!for hex, then a..f and A..F have been converted to '9'+1 to '9'+6
	int64 a
	word64 b
	int c

!trim leading zeros, which make it difficult to do a string match with maxstr
	while length>=2 and s^='0' do		!trim leading zeros
		++s
		--length
	od

	nextlx.symbol:=intconstsym

	if length>maxnumlen[base] or 
			(length=maxnumlen[base] and strncmp(s,maxnumlist[base],length)>0) then
		if base<>16 then
			lxerror("longint const")

		else
			if length>32 or 
				(length=32 and strncmp(s,"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",32)>0) then
				lxerror("longint const")

			else						!greater than 64 bits, up to 128 bits

				if length=32 and strncmp(s,"7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",32)>0 then
					nextlx.subcode:=tu128
				else
					nextlx.subcode:=ti128
				fi

				nextlx.pvalue128:=stringtonumber128(s,length,16)
			fi
		fi
		return
	fi

	a:=0

	if base<=10 then
		to length do
			a:=a*base+s++^-'0'
		od
	else
		to length do
			c:=s++^
			if c>='a' then
				a:=a*base+c-'a'+10
			elsif c>='A' then
				a:=a*base+c-'A'+10
			else
				a:=a*base+c-'0'
			fi
		od
	fi

	nextlx.value:=a

	nextlx.subcode:=setinttype(a)
end

proc stringtodecimalnumber(ichar s, int length,suffix=0)=
	int64 a
	word64 b
	int c

!trim leading zeros, which make it difficult to do a string match with maxstr
	while length>=2 and s^='0' do		!trim leading zeros
		++s
		--length
	od

	nextlx.symbol:=intconstsym

	if length>20 or 
			(length=20 and strncmp(s,"18446744073709551615",20)>0) or suffix then

		if length>39 or 
			(length=39 and strncmp(s,"340282366920938463463374607431768211455",39)>0) then
			if suffix='W' then
				lxerror("-W overflows 128 bits")
			fi
	dolongint::
			nextlx.symbol:=decimalconstsym
			nextlx.svalue:=pcm_copyheapstring(s)
		else						!greater than 64 bits, up to 128 bits

			if suffix='L' then goto dolongint fi

			if (length=39 and strncmp(s,"170141183460469231731687303715884105727",39)>0) then
				nextlx.subcode:=tu128
			else
				nextlx.subcode:=ti128
			fi

			nextlx.pvalue128:=stringtonumber128(s,length,10)
		fi
		return
	fi

	a:=0

	to length do
		a:=a*10+s++^-'0'
	od

	nextlx.value:=a

	nextlx.subcode:=setinttype(a)
end

global proc lexsetup=
!do one-time setup::
! clear the hash table and populated it with reserved words
! do maxnum support and such
	int i!,n
	static int n

	for i to maxnumlist.len do
		maxnumlen[i]:=strlen(maxnumlist[i])
	od

	inithashtable()
end

global proc printstrn(ichar s, int length)=
	if length then
		print length:"v",s:".*"
	fi
end

function scannumber(int base)ref char=
!lxsptr is at possible first digit of number sequence
!scan digits until non-digit
!return pointer to next char after compacted sequence
!sequence can be updated in-place (to close gaps caused by separators)
!start of sequence will be at lxsptr
	ref char dest
	int c

	dest:=lxsptr

	doswitch c:=lxsptr++^
	when '0'..'9' then
		dest++^:=c
		if c>='0'+base then
			lxerror("Digit out of range")
		fi
	when 'A'..'D','F','a'..'d','f' then
		if 11<=base<=16 then		!NEEDS TO CHECK LIMITS FOR BASES 10..15
			dest++^:=c
		else
			--lxsptr
			exit
		fi
	when 'E','e' then
		if base<15 then
			--lxsptr
			exit
		else
			dest++^:=c
		fi

	when '_','\'','`' then
	when 'l','L' then
		longsuffix:='L'
		exit

	else
		--lxsptr
		exit
	end doswitch
	return dest
end

proc readrawstring=
!positioned at " of F"
!read raw string
	ichar dest
	int c

	nextlx.symbol:=stringconstsym
	nextlx.svalue:=++lxsptr

	dest:=lxsptr				!form string into same buffer

	doswitch c:=lxsptr++^
	when '"' then
		if lxsptr^='"' then		!repeated, assume embedded term char
			dest++^:='"'
			++lxsptr
		else			!was end of string
			(lxsptr-1)^:=0
			exit
		fi
	when cr,lf,0 then
		lxerror("Raw string not terminated")
		--lxsptr
		exit
	else
		dest++^:=c
	enddoswitch
end

proc lookup(ref char name, int length, hashindex0)=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!in either case, lx.symptr set to entry where name was found, or will be stored in
	int wrapped, hashindex,INDEX,n
	symbol d

	hashindex:=hashindex0 iand hstmask

	d:=&hashtable[hashindex]
	wrapped:=0

	do
		if (n:=d.namelen)=length and memcmp(d.name,name,n)=0 then	!match
			nextlx.symptr:=d
			nextlx.symbol:=d.symbol
			nextlx.subcode:=d.subcode
			return
		elsif n=0 then
			exit
		fi
		++d
		if d>=hashtablelast then
			if wrapped then
				abortprogram("HASHTABLE FULL")
			fi
			wrapped:=1
			d:=&hashtable[0]
		fi
	od

!exit when not found; new name will go in entry pointed to by lxsymptr

	d.name:=pcm_copyheapstringn(name,length)
	d.namelen:=length
	d.symbol:=namesym

	nextlx.symptr:=d
	nextlx.symbol:=d.symbol
!	nextlx.subcode:=d.subcode
end

function lookupsys(ref char name)int=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!return 1 (found) or 0 (not found)
!in either case, lx.symptr set to entry where name was found, or will be stored in
	int j, wrapped, hashvalue

	j:=gethashvaluez(name) iand hstmask

	lx.symptr:=&hashtable[j]
	wrapped:=0

	do
		if lx.symptr.namelen=0 then
			exit
		elsif eqstring(lx.symptr.name,name) then	!match
			cpl name
			lxerror("sys dupl name?")
		fi

		++lx.symptr
		if ++j>=hstsize then
			if wrapped then
				abortprogram("SYS:HASHTABLE FULL")
			fi
			wrapped:=1
			lx.symptr:=&hashtable[0]
			j:=0
		fi
	od

!exit when not found; new name will go in entry pointed to by lxsymptr
	lx.symptr.name:=name				!assume can be shared (stored in a table)
	lx.symptr.namelen:=strlen(name)
	lx.symptr.symbol:=namesym			!usually replaced with actual symbol details

	return 0
end

function gethashvaluez(ichar s)int=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!ASSUMES S is lower-case, as conversion not done
	int c,hsum

	if s^=0 then return 0 fi

	hsum:=s++^

	do
		c:=s++^
		exit when c=0
		hsum:=hashc(hsum,c)
	od
	return hashw(hsum)
end

proc inithashtable=
!populate hashtable with standard symbols
	int i
	memset(&hashtable,0,hashtable.bytes)
	hashtablelast:=&hashtable[hstsize-1]

	for i:=1 to stnames.len do
		lookupsys(stnames[i])

		lx.symptr.symbol:=stsymbols[i]

		case stsymbols[i]
		when unitnamesym then
			lx.symptr.index:=stsubcodes[i]
			lx.symptr.subcode:=unitnamesym
			lx.symptr.symbol:=namesym		!masquerades as normal identifier
		else
			lx.symptr.subcode:=stsubcodes[i]
		esac
	od
end

global proc printhashtable=
	println "Hashtable:"

	for i:=0 to hstsize-1 do
		if hashtable[i].namelen then
			println i,hashtable[i].name,symbolnames[hashtable[i].symbol]
		fi
	od
end

global proc addreservedword(ichar name,int symbol,subcode, regsize=0)=

	lookupsys(name)

	lx.symptr.symbol:=namesym
	lx.symptr.subcode:=symbol
	lx.symptr.index:=subcode
	lx.symptr.regsize:=regsize
end

function dolexdirective(int index)int=
!return 1: returns a new symbol
!return 0: symbol has been absorbed; caller needs to read a new symbol
	ref strec symptr
	ref char p
	ichar file
	int i,lastsymbol,cond,fileno,length
	[256]char str

	case index
	when strincludedir,binincludedir then
		lexreadtoken()
		if nextlx.symbol<>stringconstsym then
			lxerror("strincl: string expected")
		else
			file:=nextlx.svalue
		fi

		fileno:=getsupportfile(file)
		nextlx.svalue:=sourcefiletext[fileno]
		astringlength:=length:=sourcefilesizes[fileno]

		nextlx.symbol:=(index=strincludedir|stringconstsym|astringconstsym)
		nextlx.subcode:='A'			!for use when an astring
		(nextlx.svalue+length)^:=0			!sometimes .length is not used (eg. in newstringobj())
		return 1						!so get it right. Don't need the etx

	when includedir then
		lexreadtoken()
		if nextlx.symbol<>stringconstsym then lxerror("include: string expected") fi
		file:=nextlx.svalue
		convlcstring(file)
		file:=addext(file,".m")		!add in extension if not present; assume same as source

		stacksourcefile(file)
		return 0

	when defineunitdir then
		LXERROR("DEFINE UNIT NOT DONE")

	when cclibdir then
		do
!		if ncclibs>=maxcclibs then lxerror("Too many cc libs") fi
			lexreadtoken()
			case lx.symbol
			when stringconstsym then
				addcclib(lx.svalue)
			when namesym then
				addcclib(lx.symptr.name)
			else
				lxerror("cclib/not str/name")
			esac

			lexreadtoken()
			if lx.symbol<>commasym then exit fi
		od
		return 0


	else
		cpl sourcedirnames[index]
		lxerror("Directive not implemented")
	esac
	return 0
end

proc lexreadline=
!read lex chars until eol
!returns with lxsptr pointing to what follows (crlf, etc)
!caller should remember lxsptr as start of text
!processing of next symbol deals with line counting

	doswitch lxsptr^
	when cr,lf then
		return
	when 0 then
		--lxsptr
		return
	else
		++lxsptr
	enddoswitch
END

global proc startlex(ichar caption,int fileno)=
!s is a 0-terminated source string representing perhaps
!an entire file.
!Initial lex vars so that it is possible to start reading tokens from it
!(This lex system doesn't deal with include files so there no nested sourcefiles.
!There are only macro expansions which are dealt with locally.)

	lxsptr:=sourcefiletext[fileno]

	lxfileno:=fileno
	nextlx.pos:=1

	nextlx.symbol:=semisym
	nextlx.subcode:=0
end

global function addnamestr(ichar name)ref strec=
	tokenrec oldlx
	ref strec symptr

	oldlx:=nextlx
	lookup(name,strlen(name), gethashvaluez(name))
	symptr:=nextlx.symptr
	nextlx:=oldlx

	return symptr
end

global proc ps(ichar caption)=
!print "PS:",,caption,,":"
	print caption,,": "
	printsymbol(&lx)
end

global proc psnext(ichar caption)=
!print "PS:",,caption,,":"
	print caption,,": "
	printsymbol(&nextlx)
end

proc stacksourcefile(ichar file,int ismainmodule=0)=
	int fileno
	ichar basefile,sptr,path

	fileno:=getsupportfile(file)

	stacksource(sourcefiletext[fileno],fileno,1)
end

proc stacksource(ichar sptr,int fileno,isfile)=
!introduce new source level for macros or include files
!not used for main source

	if sourcelevel>=maxstackdepth then
		lxerror("Include file/macro overflow")
	fi
	++sourcelevel
	lxstart_stack[sourcelevel]:=lxstart
	lxsptr_stack[sourcelevel]:=lxsptr
	lxfileno_stack[sourcelevel]:=lxfileno
	lxlineno_stack[sourcelevel]:=nextlx.pos
	isfile_stack[sourcelevel]:=isfile

	lxstart:=lxsptr:=sptr
	nextlx.pos:=1
	lxfileno:=fileno
end

proc unstacksource=
	if sourcelevel>0 then			!check that some source is stacked
		lxstart:=lxstart_stack[sourcelevel]
		lxsptr:=lxsptr_stack[sourcelevel]
		nextlx.pos:=lxlineno_stack[sourcelevel]
		lxfileno:=lxfileno_stack[sourcelevel]
		--sourcelevel
	fi
end

proc readarraystring(int prefix)=
	++lxsptr
	lxreadstring('"')
	nextlx.symbol:=astringconstsym
	nextlx.subcode:=toupper(prefix)
	astringlength:=strlen(nextlx.svalue)
end

function stringtonumber128(ichar s, int length,base)ref int128=
	int128 aa
	int c,d
	aa:=0
	to length do
		aa:=aa*base
		c:=(s++)^

		if c>='a' then
			d:=c-'a'+10
		elsif c>='A' then
			d:=c-'A'+10
		else
			d:=c-'0'
		fi

		aa:=aa+d
	od

	ref int128 p
	p:=pcm_alloc(int128.bytes)
	p^:=aa

	return p
end

function setinttype(word64 a)int=
	if a<=u64(0x7FFF'FFFF'FFFF'FFFF) then
		return ti64
	else
		return tu64
	fi
end

proc readrawxname=
	int c,hsum,length

	nextlx.svalue:=lxsptr
	hsum:=0

	doswitch c:=lxsptr++^
	when 'A'..'Z','a'..'z','0'..'9','_','$',128..255then
		hsum:=hashc(hsum,c)
	else
		--lxsptr
		exit
	end doswitch

	length:=lxsptr-nextlx.svalue

	if length=0 then
		lxerror("Bad ` name")
	fi
	lookup(lx.svalue,length, hashw(hsum))
	nextlx.symbol:=rawxnamesym

	return
end

proc lxerror_s(ichar mess,s)=
	lxerror(mess)
end

proc lxreadstring(int termchar)=
!start from char just after " or ' (termchar will be " or ')

	ichar s,t
	int c, d, length, hasescape
	[8]char str

	if termchar='"' then
		nextlx.symbol:=stringconstsym
	else
		nextlx.symbol:=charconstsym
		nextlx.subcode:=tint
	fi

	s:=lxsptr

!do a first pass that terminates length of final string
	length:=0
	hasescape:=0

	doswitch c:=lxsptr++^
	when '\\' then			!escape char
		c:=lxsptr^
		if c in 'A'..'Z' then c+:=' ' fi
		++lxsptr
		hasescape:=1

		switch c
		when 'a','b','c','r','f','l','n','s','t','v','y','z','0','"','q','\\','\'' then
			++length
		when 'w' then
			++length
		when 'x' then	!2-digit hex code follows
			lxsptr+:=2
			++length
		else
			lxerror("Bad str escape")
		endswitch
	when '"','\'' then		!possible terminators
		if c=termchar then		!terminator char
			if lxsptr^=c then		!repeated, assume embedded term char
				hasescape:=1
				++lxsptr
				++length
			else			!was end of string
				exit
			fi
		else
			++length
		fi
	when cr,lf,0 then
		lxerror("String not terminated")
	else
		++length
	end doswitch

	if length=0 then
		nextlx.svalue:=""
		return
	elsif not hasescape then
		nextlx.svalue:=pcm_copyheapstringn(s,length)
		return
	fi

!need to copy string to dest and expand the escape codes

	nextlx.svalue:=t:=pcm_alloc(length+1)

	do
		switch c:=s++^
		when '\\' then			!escape char
			c:=s^
			if c>='A'  and c<='Z' then c+:=' ' fi
			++s
			switch c
			when 'a' then			!bell ('alert')
				c:=7
			when 'b' then			!backspace
				c:=8
			when 'c','r' then		!carriage return
					c:=cr
			when 'e' then			!end-of-text
				c:=26
			when 'f' then			!formfeed
				c:=12
			when 'l','n' then		!linefeed, or linux/c-style newline
				c:=lf
			when 's' then			!eScape
				c:=27
			when 't' then			!tab
				c:=9
!			when 'u' then			!reserved for unicode, like \x but with 4 hex digits
			when 'v' then			!vertical tab
				c:=11
			when 'w' then			!windows-style cr-lf
				t++^:=cr
				c:=lf
			when 'x' then	!2-digit hex code follows
				c:=0
				to 2 do
					case d:=s++^
					when 'A','B','C','D','E','F' then
						c:=c*16+d-'A'+10
					when 'a','b','c','d','e','f' then
						c:=c*16+d-'a'+10
					when '0','1','2','3','4','5','6','7','8','9' then
						c:=c*16+d-'0'
					else
						lxerror("Bad \\x code")
					esac
				od
			when 'y' then			!CCI/SM backwards tab
				c:=16
			when 'z','0' then		!null (not fully supported in code)
				c:=0
			when '"','Q' then		!embedded double quote
				c:='"'
			when '\\' then
				c:='\\'
			when '\'' then			!embedded single quote
				c:='\''
			else
				str[1]:=c; str[2]:=0
				lxerror_s("Unknown string escape: \\%s",&.str)
			end
		when '"','\'' then		!possible terminators
			if c=termchar then		!terminator char
				if s^=c then		!repeated, assume embedded term char
					++s
				else			!was end of string
					exit
				fi
			fi
		when cr,lf,0 then
			lxerror("String not terminated")
		endswitch

		t++^:=c
	od

	t^:=0
end
