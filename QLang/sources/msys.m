global record procinforec=
	word16		fnindex
	byte		rettype
	byte		nparams
	[12]byte	paramlist
end

!for print/read routines
!------------------------------------------
export record fmtrec=	! (default)
	byte	minwidth	! n (0)   min field width (0 if not used or don't care)
	i8		precision	! .n (0)   number of decimals/significant figures/max width
	byte	base		! B,H or Xn (10)  2 to 16

	char	quotechar	! Qc (0)   0 or '"' or c
	char	padchar		! Pc, Z (' ')
	char	realfmt		! E,F,G ('f') 'e' or 'f' or 'g'

	char	plus		! (0)   0 or '+'
	char	sepchar		! Sc (0)   0 or ',' or c placed every 3 (base=10) or 4 digits
	char	lettercase	! A,a ('A') 'A' or 'a'
	char	justify		! JL, JR, JC ('R') 'L' or 'R' or 'C'?
	char	suffix		! Tc (0)   0 or 'B' or 'H' or c
	char	usigned		! W (0)   0 or 'W' force unsigned o/p for ints (eg. for hex display)
	char	charmode	! C,D (0)  0 or 'C' or 'D'	o/p int as int or single char or double/multi-char
	char	heapmode	! M (0)  'M' for str-functions, return ptr tp heap string
	char	param		! Use int value for <fmtparam>
	byte	spare : (showtype:1, newline:1)
end

int fmtparam			!as set with :'V'

enumdata =
	std_io,file_io,str_io
end

const comma = ','

export int $cmdskip			!0 unless set by READMCX/etc

export int needgap			= 0
int outdev			= std_io
filehandle outchan	= nil
ref char fmtstr 	= nil

const maxiostack=10
[maxiostack]filehandle	outchan_stack
[maxiostack]int			outdev_stack
[maxiostack]ref char	fmtstr_stack
[maxiostack]byte		needgap_stack

[maxiostack]ref char	ptr_stack		!this one doesn't need pushing, as each is pointed to from outchan
int niostack=0

[0:]char digits=A"0123456789ABCDEF"
const onesixty=360
fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,0,0)

!Read buffer vars
export const rd_buffersize = 16384	!total capacity of line buffer

export ref char rd_buffer		! point to start of read buffer
export int rd_length			! length of this line (as read by readln)
export ref char rd_pos			! current position it's up to (next read starts here)
export ref char rd_lastpos		! set by sread() just before reading used for reread()

int termchar			! terminator char set by readxxx()
int itemerror			!	set by some read functions, eg for reals

[4096]char printbuffer
ichar printptr
int printlen

!------------------------------------------

const maxparam=128
export int nsysparams
export int ncmdparams
export int nenvstrings
export [maxparam]ichar sysparams
!export ref[]ichar cmdparams
export ref[0:]ichar cmdparams
export ref[]ichar envstrings
!export [maxparam]ichar envstrings

const maxcallback=8
[0..maxcallback,8]word64 callbackstack
int ncallbacks=0

proc start=
	int32 nargs
	int nargs64
	ref[]ichar args
	static [128]byte startupinfo			! 68 or 104 bytes
	int res

	res:=__getmainargs(&nargs,cast(&args),cast(&envstrings),0,cast(&startupinfo))
	
	nsysparams:=nargs
	
	if nsysparams>maxparam then
		printf("Too many params\n")
		stop 50
	fi

	nargs64:=nargs			!bug when using 32-bit limit when compiled with mm
	for i:=1 to nargs64 do
		sysparams[i]:=args[i]
	od
	
!assume nsysparams is >=1, since first is always the program name
	ncmdparams:=nsysparams-($cmdskip+1)
	cmdparams:=cast(&sysparams[$cmdskip+1])

	int j:=1
	nenvstrings:=0
	while envstrings[j] do
		++nenvstrings
		++j
	od
end

proc pushio=
	if niostack>=maxiostack then
		printf("Too many io levels\n")
		stop 53
	fi
	++niostack
	outchan_stack[niostack]	:= outchan
	outdev_stack[niostack]	:= outdev
	fmtstr_stack[niostack]	:= fmtstr
	needgap_stack[niostack]	:= needgap
	needgap:=0
	fmtstr:=nil
	outchan:=nil
end

export proc m$print_startfile(ref void dev)=
	pushio()
	outchan:=cast(dev)
	if dev then
		outdev:=file_io
	else
		outdev:=std_io
	fi
	resetprintbuffer()
end

export proc m$print_startstr(ref char s)=
	ref ref char p
	pushio()

	ptr_stack[niostack]:=s
	p:=&ptr_stack[niostack]

	outchan:=cast(p)
	outdev:=str_io
end

export proc m$print_startptr(ref ref char p)=
	pushio()

	outchan:=cast(p)
	outdev:=str_io
end

export proc m$print_startcon=
	pushio()
	outdev:=std_io
	resetprintbuffer()
end

export proc m$print_setfmt(ref char format)=
	fmtstr:=format
end

export proc m$print_end=
	needgap:=0
	nextfmtchars(1)
	if niostack=1 and outdev in [std_io,file_io] then
		dumpprintbuffer()
	fi

	if niostack=0 then return fi
	outchan	:= outchan_stack[niostack]
	outdev	:= outdev_stack[niostack]
	fmtstr	:= fmtstr_stack[niostack]
	needgap	:= needgap_stack[niostack]

	--niostack
end

export proc m$print_ptr(u64 a,ichar fmtstyle=nil)=
	[20]char s

	if fmtstyle=nil then
		fmtstyle:="z8H"
	fi
	m$print_u64(a,fmtstyle)
end

export proc m$print_ptr_nf(u64 a)=
	m$print_ptr(a)
end

export proc m$print_i64(int64 a,ichar fmtstyle=nil)=
	[40]char s
	fmtrec fmt
	int n

	nextfmtchars()
	if fmtstyle=nil then
		if a>=0 then
			n:=u64tostr(a,&.s,10,0)
		elsif a=i64.min then
			fmt:=defaultfmt
			dofmt

		else
			s[1]:='-'
			n:=u64tostr(-a,&s[2],10,0)+1
		fi

		printstr_n(&.s,n)

	else

		strtofmt(fmtstyle,-1,&fmt)
		if fmt.param='V' then
			fmtparam:=a
			needgap:=0
		else
dofmt:
			tostr_i64(a,&fmt)
		fi
	fi
	needgap:=1
end

export proc m$print_i64_nf(int64 a)=
	m$print_i64(a)
end

export proc m$print_bool(int64 a, ichar fmtstyle=nil)=
	if a then
		m$print_str("True",fmtstyle)
	else
		m$print_str("False",fmtstyle)
	fi
end

export proc m$print_u64(word64 a,ichar fmtstyle=nil)=
	[40]char s
	fmtrec fmt

	nextfmtchars()
	if fmtstyle=nil then
		sprintf(&.s,"%llu",a)
		printstr(&.s)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_u64(a,&fmt)
	fi
	needgap:=1
end

export proc m$print_r64(real x,ichar fmtstyle=nil)=
	[360]char s
	fmtrec fmt

	nextfmtchars()
	if fmtstyle=nil then
		sprintf(&.s,"%f",x)
		printstr(&.s)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_r64(x,&fmt)
	fi

	needgap:=1
end

export proc m$print_r32(real32 x,ichar fmtstyle=nil)=
	m$print_r64(x,fmtstyle)
end

global proc m$print_c8(int64 a,ichar fmtstyle=nil)=
	[40]char s
	fmtrec fmt
	int n

	nextfmtchars()

	s[1]:=a
	s[2]:=0
	printstr(&.s)
	needgap:=1
end

export proc m$print_str(ichar s, fmtstyle=nil)=
	nextfmtchars()

	if s=nil then
		printstr("<null>")
		return
	fi

	fmtrec fmt
	if fmtstyle=nil then
		printstr(s)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_str(s,-1,&fmt)
	fi
	needgap:=1
end

export proc m$print_strn(ichar s, int length, ichar fmtstyle=nil)=
	nextfmtchars()

	if s=nil then
		printstr("<null>")
		return
	fi

	fmtrec fmt
	if fmtstyle=nil then
		printstr_n(s,length)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_str(s,length,&fmt)
	fi
	needgap:=1
end

export proc m$print_str_nf(ichar s)=
	m$print_str(s)
end

export proc m$print_strsl(slice[]char s, ichar fmtstyle=nil)=
	nextfmtchars()
	fmtrec fmt
!	if fmtstyle=nil then
!		printstr_n(cast(s.sliceptr),s.len)
!	else
		abortprogram("FORMATED PRINT SLICE NOT READY")
!		strtofmt(fmtstyle,-1,&fmt)
!		tostr_str(s,s.len,&fmt)
!	fi
	needgap:=1
end

export proc m$print_newline=
	needgap:=0
	nextfmtchars(1)
	printstr("\w")
end

export proc m$print_nogap=
	needgap:=0
end

export proc m$print_space=
	needgap:=0
	printstr(" ")
end

export proc printstr(ichar s)=
	printstr_n(s,strlen(s))
end

export proc printstr_n(ichar s,int n)=
	ref ref char p

	return when n=0

	if niostack=1 and outdev in [std_io,file_io] then
		addtobuffer(s,n)
	else
		dumpstr(s,n)
	fi
end

export proc printstrn_app(ichar s, int length, filehandle f=nil)=
if length then
	if f=nil then
		printf("%.*s",length,s)
	else
		fprintf(f,"%.*s",length,s)
	fi
fi
end

proc printchar(int ch)=
	ref ref char p
	[4]char str

	str[1]:=ch
	str[0]:=ch
	printstr_n(str,1)
end

global proc nextfmtchars(int lastx=0)=
	char c
	ref char pstart
	int n
	if not fmtstr then			!format not in use
		if needgap then
			printchar(' ')
		fi
		needgap:=0
		return
	fi

	pstart:=fmtstr
	n:=0

	do
		c:=fmtstr^
		case c
		when '#' then
			if lastx then
				goto skip
			fi
			++fmtstr
			if n then
				printstr_n(pstart,n)
			fi
			return
		when 0 then
			if n then
				printstr_n(pstart,n)
			elsif not lastx then
				printstr_n("|",1)
			fi
			return
		when '~' then
			if n then
				printstr_n(pstart,n)
				n:=0
			fi
			++fmtstr
			c:=fmtstr^
			if c then
				++fmtstr
				printchar(c)
			fi
			pstart:=fmtstr
		else
	skip:
			++n
			++fmtstr
		esac
	od
end

export proc strtofmt(ref char s,int slen,ref fmtrec fmt) =		!PC_STRTOFMT
!convert format code string in s, to fmtrec at fmt^
!Format code is a string containing the following char codes (upper or lower when mostly)
!n	Width
!.n	Max width/precision
!A	Convert to upper when
!a	Convert to lower when
!B	Binary
!C	Show int as single n-bit (unicode) character
!D	Show int as multi-bit (unicode) character
!E,F,G	Specify format for double (corresponds to C format codes)
!F
!G
!H	Hex
!JC	Justify centre
!JL	Justify left
!JR	Justify right
!M	HEAPMODE???
!O	Octal
!Pc	Use padding char c
!Q	Add double quotes around string (and deal with embedded quotes)
!'	Add single quotes around string (and deal with embedded quotes)
!Sc	Use separator char c between every 3 or 4 digits
!Tc	Use terminator char c (typically B or H)
!U	Show ints as unsigned
!V	For ints, don't display: store value as parameter for subsequent '*'
!W	Unsigned
!Xn	Use base n (n is hex 0 to F)
!Z	Use "0" padding
!+	Always have + or - in front of integers
!~	Quote char is ~
!*	Same as n but uses parameter set with :'V' on previous int

	int c, base
	byte wset
	int n
	[0:100]char str

	fmt^:=defaultfmt

	if s=nil then return fi

	if slen=-1 then slen:=strlen(s) fi

	memcpy(&.str,s,slen)		!convert s/slen to zero-terminated string
	str[slen]:=0
	s:=&.str

	wset:=0
	while s^ do
		c:=s^
		++s
		if c='A' then fmt.lettercase:='A'
		elsif c='a' then fmt.lettercase:='a'
		elseswitch toupper(c)
		when 'B' then fmt.base:=2
		when 'H' then fmt.base:=16
		when 'O' then fmt.base:=8
		when 'X' then
			base:=0
			do
				c:=s^
				if c in '0'..'9' then
					base:=base*10+c-'0'
					++s
				else
					exit
				fi
			od
			if base in 2..16 then
				fmt.base:=base
			fi

		when 'Q' then fmt.quotechar:='"'
		when 'J' then
			fmt.justify:=toupper(s^)
			if s^ then
				++s
			fi
		when 'Z' then fmt.padchar:='0'
		when 'S' then
			fmt.sepchar:=s^
			if s^ then
				++s
			fi
		when 'P' then
			fmt.padchar:=s^
			if s^ then
				++s
			fi
		when 'T' then
			fmt.suffix:=s^
			if s^ then
				++s
			fi
		when 'U' then fmt.usigned:='W'
		when 'E' then fmt.realfmt:='e'
		when 'F' then fmt.realfmt:='f'
		when 'G' then fmt.realfmt:='g'
		when 'D' then fmt.charmode:='D'
		when 'C' then fmt.charmode:='C'
		when 'M' then fmt.heapmode:='M'
		when 'V' then fmt.param:='V'
		when 'Y' then fmt.showtype:=1
		when 'N' then fmt.newline:=1
		elsecase c
		when '.' then
			wset:=1
		when comma,'_' then fmt.sepchar:=c
		when '+' then fmt.plus:='+'
		when '~' then fmt.quotechar:='~'
		when '*' then
			n:=fmtparam
			goto gotwidth
		else
			if c>='0' and c<='9' then
				n:=c-'0'
				do
					c:=s^
					if s^=0 then
						exit
					fi
					if c>='0' and c<='9' then
						++s
						n:=n*10+c-'0'
					else
						exit
					fi
				od
gotwidth:
				if not wset then
					fmt.minwidth:=n
					wset:=1
				else
					fmt.precision:=n
				fi
			fi
		fi
	od
end

function domultichar (ref char p,int n,ref char dest,ref fmtrec fmt)int =
!there are n (4 or 8) chars at p.!
!There could be 0 to 4 or 8 printable chars converted to string at dest
	[0:20]char str
	ref char q
	int i,nchars

	q:=&.str

	nchars:=n

	to n do
		if p^=0 then exit fi
		q^:=p^
		++q
		++p
	od
	q^:=0

	return expandstr(&.str,dest,strlen(&.str),fmt)
end

export function expandstr(ref char s,ref char t,int n,ref fmtrec fmt)int =		!EXPANDSTR
!s contains a partly stringified value.
!widen s if necessary, according to fmt, and copy result to t
!n is current length of s
!note) = for non-numeric strings, fmt.base should be set to 0, to avoid moving
!a leading +/- when right-justifying with '0' padding.
!t MUST be big enough for the expanded string; caller must take care of this
!result will be zero-terminated, for use in this module

	int i,w,m

!check to see if result is acceptable as it is
	w:=fmt.minwidth
	if w=0 or w<=n then		! allow str to be longer than minwidth
		strncpy(t,s,n)
		(t+n)^:=0
		return n
	fi

	if fmt.justify='L' then	! left-justify
		strncpy(t,s,n)
		t+:=n
		for i:=1 to w-n do
			t^:=fmt.padchar
			++t
		od
		t^:=0
	elsif fmt.justify='R' then
		if fmt.padchar='0' and fmt.base and (s^='-' or s^='+') then ! need to move sign outside 
			t^:=s^
			++t
			to w-n do
				t^:=fmt.padchar
				++t
			od
			strncpy(t,s+1,n-1)
			(t+n-1)^:=0
		else
			to w-n do
				t^:=fmt.padchar
				++t
			od
			strncpy(t,s,n)
			(t+n)^:=0
		fi

	else				! centre-justify?

		m:=(w-n+1)/2
		to m do
			t^:=fmt.padchar
			++t
		od
		strncpy(t,s,n)
		t+:=n
		to w-n-m do
			t^:=fmt.padchar
			++t
		od
		t^:=0

	fi
	return w
end

export function u64tostr(u64 aa,ref char s,word base,int sep)int =		!U64TOSTR
!convert 64-bit int a to string in s^
!base is number base, usually 10 but can be 2 or 16. Other bases allowed
!result when a=minint (will give "<minint>")
	[0:onesixty]char t
	u64 dd
	int i,j,k,g
	int cc
	int dummy
	ref char s0

	i:=0
	k:=0
	g:=(base=10|3|4)

	repeat
		if base=10 then
			assem
				mov		rcx, [aa]
				mov		rax, rcx
				mov		rdx, 7378697629483820647
				imul	rdx
				mov		rax, rdx
				mov		rdx, rcx
				sar		rdx, 63
				sar		rax, 2
				sub		rax, rdx
				lea		rdx, [rax+rax*4]
				add		rdx, rdx
				sub		rcx, rdx
				mov		[dd], rcx
				mov		[aa], rax
			end
		else
			dd:=aa rem base
			aa:=aa/base
		fi

		t[++i]:=digits[dd]

!BUG in separator logic, doesn't work when leading zeros used, eg. printing
!out a full length binary
!so perhaps move this out to expandstr
		++k
		if sep and aa<>0 and k=g then
			t[++i]:=sep
			k:=0
		fi
	until aa=0

	j:=i
	s0:=s
	while i do
		s^:=t[i--]
		++s
	od
	s^:=0

	return j
end

export function i64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =
!a is signed 64-bit int/long, fmt is a ref to a filled-in fmtrec
!convert a to a string in s, according to fmt
!a basic conversion is done first,: the field manipulation is done
!signed=1 for int, 0 for u32 (fmt.unsigned forces ints to be treated as longs)
!returns length of s
	[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w,usigned
	const i64 mindint=0x8000'0000'0000'0000

	usigned:=0
	if fmt.usigned then
		usigned:=1
	fi
	if aa=mindint and not usigned then		! minint

		str[0]:='-'
		n:=i64mintostr(&str[1],fmt.base,fmt.sepchar)+1

	else
		if (not usigned and aa<-0) or fmt.plus then
			if aa<0 then
				aa:=-aa
				str[0]:='-'
			else
				str[0]:='+'
			fi
			n:=u64tostr(aa,&str[1],fmt.base,fmt.sepchar)+1
		else
			n:=u64tostr(aa,&.str,fmt.base,fmt.sepchar)
		fi
	fi

	if fmt.suffix then
		str[n]:=fmt.suffix
		str[++n]:=0
	fi

!str uses upper cases for hex/etc see if lc needed
	if (fmt.base>10 or fmt.suffix) and fmt.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

export function u64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =		!U64TOSTRFMT
!see i64tostrfmt
	[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w

	n:=u64tostr(aa,&.str,fmt.base,fmt.sepchar)

	if fmt.suffix then
		str[n]:=fmt.suffix
		str[++n]:=0
	fi

!str uses upper cases for hex/etc see if lc needed
	if fmt.base>10 or fmt.suffix and fmt.lettercase='a'	then	! need lower when
!		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

export function i64mintostr(ref char s,int base,int sep)int =		!I64MINTOSTR
!convert minint to string in s do not include minus sign
!return number of chars in string
	[0:onesixty]char t
	int i,j,k,g,neg

	case base
	when 10 then
		strcpy(&t[0],"9223372036854775808")
		j:=3
	when 16 then
		strcpy(&t[0],"8000000000000000")
		j:=1
	when 2 then
		strcpy(&t[0],"1000000000000000000000000000000000000000000000000000000000000000")
		j:=7
	else
		strcpy(&t[0],"<mindint>")
	esac

	i:=strlen(&t[0])
	s+:=i
	if sep then
		s+:=j
	fi
	s^:=0

	k:=0
	g:=(base=10|3|4)

	while i do
		--s
		s^:=t[i-- -1]
		if sep and i and ++k=g then
			--s
			s^:=sep
			k:=0
		fi
	od
	return strlen(s)
end

export function strtostrfmt(ref char s,ref char t,int n,ref fmtrec fmt)int =
!s is a string process according to fmtrec fmt^, and return result in t
!caller should check whether any changes are required to s (now it can just use s), but this
!check is done here anyway (with a simple copy to t)
!n is current length of s
!return length of t
!Three processing stages:
!1 Basic input string s
!2 Additions or mods: quotes, suffix, when conversion
!3 Width adjustment
!1 is detected here, 2 is done here, 3 is done by expandstr
	ref char u,v
	[256]char str
	int w,nheap		! whether any heap storage is used  bytes allocated

	nheap:=0

	if fmt.quotechar or fmt.lettercase then		! need local copy
		if n<256 then
			u:=&.str
		else
			nheap:=n+3					! allow for quotes+terminator
			u:=pcm_alloc(nheap)
		fi
		if fmt.quotechar then
			v:=u
			v^:=fmt.quotechar
			++v
			if n then
				strcpy(v,s)
				v+:=n
			fi
			v^:=fmt.quotechar
			++v
			v^:=0
			n+:=2
		else
			memcpy(u,s,n)
		fi
		case fmt.lettercase
		when 'a' then	! need lower when
			convlcstring(u)
		when 'A' then
			convucstring(u)
		esac
		s:=u
	fi

	w:=fmt.minwidth
	if w>n then
		n:=expandstr(s,t,n,fmt)
	else
		memcpy(t,s,n)
	fi
	if nheap then
		pcm_free(u,nheap)
	fi
	return n
end

proc tostr_i64(int64 a, ref fmtrec fmt)=
	[360]char str
	int n

	case fmt.charmode
	when 0 then
		n:=i64tostrfmt(a,&.str,fmt)
	when 'D','d' then
		n:=domultichar(ref char(&a),8,&.str,fmt)

	else						!assume 'C'
		printchar(a)			!no other formatting allowed
		return
	esac

	printstr_n(&.str,n)
end

proc tostr_u64(word64 a, ref fmtrec fmt)=
	[360]char str
	int n

	case fmt.charmode
	when 'D','d' then
		n:=domultichar(ref char(&a),8,&.str,fmt)

	when 'C','c' then
		printchar(a)			!no other formatting allowed
		return

	else
		n:=u64tostrfmt(a,&.str,fmt)
	esac

	printstr_n(&.str,n)
end

proc tostr_r64(real x,ref fmtrec fmt) =
	[360]char str,str2
	[0:10]char cfmt
	int n

	cfmt[0]:='%'

	if fmt.precision then
		cfmt[1]:='.'
		cfmt[2]:='*'
		cfmt[3]:=fmt.realfmt
		cfmt[4]:=0
		sprintf(&.str,&.cfmt,fmt.precision,x)
	else
		cfmt[1]:=fmt.realfmt
		cfmt[2]:=0
		sprintf(&.str,&.cfmt,x)
	fi

!at this point, n is the str length including signs and suffix

	n:=strlen(&.str)		! current length

	if n<fmt.minwidth then
		n:=expandstr(&.str,&.str2,n,fmt)
		strcpy(&.str,&.str2)
	fi

	printstr_n(&.str,n)
end

proc tostr_str(ref char s, int oldlen, ref fmtrec fmt) =
	int newlen,n
	ref char t

!try and work out size of formatted string
	if oldlen=-1 then
		oldlen:=strlen(s)
	fi
	newlen:=oldlen

	if fmt.quotechar or fmt.minwidth>newlen or fmt.lettercase or fmt.precision then
		if fmt.quotechar then
			newlen+:=2
		fi
		if fmt.minwidth>newlen then
			newlen:=fmt.minwidth
		fi
		t:=pcm_alloc(newlen+1)
		n:=strtostrfmt(s,t,oldlen,fmt)
		if fmt.precision then
			n min:=fmt.precision
		fi

		printstr_n(t,n)
		pcm_free(t,newlen+1)
	else
		printstr_n(s,oldlen)
	fi
end

function getfmt(ichar fmtstyle)ref fmtrec=
	static fmtrec fmt
	if fmtstyle then
		strtofmt(fmtstyle,-1,&fmt)
		return &fmt
	else
		return &defaultfmt
	fi
end

export function strint(int64 a, ichar fmtstyle=nil)ichar=
	static [100]char str
	ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_i64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

export proc getstrint(int64 a, ichar dest)=
	m$print_startstr(dest)
	tostr_i64(a,getfmt(nil))
	m$print_end()
end

export function strword(word64 a, ichar fmtstyle=nil)ichar=
	static [100]char str
	ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_u64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

export function strreal(real a, ichar fmtstyle=nil)ichar=
	static [320]char str
	ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_r64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

export function getstr(ichar s, ref fmtrec fmt)ichar=
	if fmt.heapmode then
		return pcm_copyheapstring(s)
	else
		return s
	fi
end

proc initreadbuffer=
	if rd_buffer then return fi
	rd_buffer:=pcm_alloc(rd_buffersize)
	rd_buffer^:=0
	rd_pos:=rd_lastpos:=rd_buffer
end

global proc m$read_conline=
	initreadbuffer()

	readlinen(nil,rd_buffer,rd_buffersize)

	rd_length:=strlen(rd_buffer)
	rd_pos:=rd_buffer
	rd_lastpos:=nil
end

global proc m$read_fileline(filehandle f)=
	ichar p
	initreadbuffer()

	if f=filehandle(1) then
ABORTPROGRAM("READ CMDLINE")
!		rd_buffer^:=0
!		p:=getcommandlinea()
!		repeat
!			++p
!		until p^ in [' ','\t',0]
!		strcpy(rd_buffer, p)
!		rd_length:=strlen(rd_buffer)
!		rd_pos:=rd_buffer
!		rd_lastpos:=nil
		return
	fi

	readlinen(f,rd_buffer,rd_buffersize)

	rd_length:=strlen(rd_buffer)
	rd_pos:=rd_buffer
	rd_lastpos:=nil
end

global proc m$read_strline(ichar s)=
	int n

	initreadbuffer()
	n:=strlen(s)

	if n<rd_buffersize then
		strcpy(rd_buffer,s)
	else
		memcpy(rd_buffer,s,rd_buffersize-1)
		(rd_buffer+rd_buffersize-1)^:=0
	fi
	rd_length:=n
	rd_pos:=rd_buffer
	rd_lastpos:=nil
end

function readitem(int &itemlength)ref char =
!read next item from rd_buffer
!identify a substring that can contain a name, int, real, string or filename
!return updated position of s that points past the item and past the immediate
!terminator 
!information about the read item is returned in itemstr, which points to
!the start of the item, and in itemlength. Item excludes any surrounding whitespace
!Item can be quoted, then the item points inside the quotes
!Any embedded quotes are removed, and the characters moved up. The item will
!be that reduced subsequence
!NOTE THAT THIS IS DESTRUCTIVE. On reread, the input will be different.
!I can mitigate this by adding spaces between the end of the item, and the next item,
!overwriting also the terminator. But this won't restore the line if one of the next
!reads is literal, using 'L' or 'C' codes.
	ref char p,s,itemstr
	char quotechar, c

	unless rd_buffer then 
		initreadbuffer()
	end unless

	s:=rd_pos

!scan string, eliminating leading white space
	while s^=' ' or s^=9 do
		++s
	od

	itemstr:=s
	rd_lastpos:=rd_pos:=s

	if s^=0 then
		termchar:=0
		itemlength:=0
		return s
	fi

	quotechar:=0
	if s^='"' then
		quotechar:='"'
		++s
	elsif s^='\'' then
		quotechar:='\''
		++s
	fi

!loop reading characters until separator or end reached
	p:=itemstr:=s

	while s^ do
		c:=s++^
		case c
		when ' ', 9, comma, '=' then		! separator
			if quotechar or p=s then			!can be considered part of name if inside quotes, or is only char
				goto normalchar
			fi
			termchar:=c
			exit
		else
	normalchar:
			if c=quotechar then
				if s^=quotechar then	! embedded quote
					p^:=c
					++s
					++p
				else					! end of name
					termchar:=s^
					if termchar=',' or termchar='=' then
						++s
						termchar:=s^
					fi
					exit
				fi
			else
				p^:=c
				++p
			fi
		esac
	od

	if s^=0 then
		termchar:=0
	fi
	itemlength:=p-itemstr				! actual length of token
	rd_pos:=s

	return itemstr
end

export function strtoint(ichar s,int length=-1, word base=10)int64=
!return point to next char after terminator (which can be just off length of string)
	byte signd
	word64 aa
	word c,d

	itemerror:=0

	if length=-1 then
		length:=strlen(s)
	fi
!check for sign
	signd:=0
	if length and s^='-' then
		signd:=1; ++s; --length
	elsif length and s^='+' then
		++s; --length
	fi

	aa:=0
	while length do
		c:=s++^
		--length
		if c in 'A'..'F' then d:=c-'A'+10
		elsif c in 'a'..'f' then d:=c-'a'+10
		elsif c in '0'..'9' then d:=c-'0'
		elsif c in ['_', '\''] then
			nextloop
		else
			itemerror:=1
			exit
		fi

		if d>=base then
			itemerror:=1
			exit
		fi
		aa:=aa*base+d
	od

	if signd then
		return -aa
	else
		return aa
	fi
end

global function m$read_i64(int fmt=0)int64=
	ref char s
	int length,c
	int64 aa

	case fmt
	when 'C','c' then
		rd_lastpos:=rd_pos
		if rd_pos^ then
			return rd_pos++^
		else
			return 0
		fi
	when 'T','t' then
		return termchar
	when 'E','e' then
		return itemerror
	esac

	s:=readitem(length)

	case fmt
	when 0,'I','i' then
		return strtoint(s,length)
	when 'B','b' then
		return strtoint(s,length,2)
	when 'H','h' then
		return strtoint(s,length,16)
	esac
	return 0
end

global function m$read_r64(int fmt=0)real=
	[512]char str
	ref char s
	int length
	int32 numlength
	real x

	s:=readitem(length)

	if length=0 or length>=str.len then		!assume not a real
		return 0.0
	fi
	memcpy(&.str,s,length)
	str[length+1]:=0

	itemerror:=0

	if sscanf(&.str,"%lf%n", &x, &numlength)=0 or numlength<>length then
		x:=0.0
		itemerror:=1
	fi

	return x
end

global proc m$read_str(ref char dest, int destlen=0,fmt=0)=
	ref char s
	int length,numlength
	real x

	itemerror:=0
	if fmt='L' or fmt='l' then
		s:=rd_pos
		length:=rd_buffer+rd_length-rd_pos

	else
		s:=readitem(length)

		if fmt='N' or fmt='n' then
			iconvlcn(s,length)
		fi
	fi

	if destlen>0 then
		if length>=destlen then
			length:=destlen-1
			itemerror:=1
		fi
	fi
	memcpy(dest,s,length)
	(dest+length)^:=0
end

export proc readstr(ref char dest, int fmt=0,destlen=0)=
	m$read_str(dest,destlen,fmt)
end

export proc rereadln=
	rd_pos:=rd_buffer
	rd_lastpos:=rd_pos
end

export proc reread=
	rd_pos:=rd_lastpos
end

export function valint(ichar s, int fmt=0)int64=
	ref char old_pos, old_lastpos
	int64 aa

	initreadbuffer()
	old_pos:=rd_pos
	old_lastpos:=rd_lastpos

	rd_pos:=s
	aa:=m$read_i64(fmt)
	rd_pos:=old_pos
	rd_lastpos:=old_lastpos
	return aa
end

export function valreal(ichar s)real=
	ref char old_pos, old_lastpos
	real x

	initreadbuffer()
	old_pos:=rd_pos
	old_lastpos:=rd_lastpos

	rd_pos:=s
	x:=m$read_r64()
	rd_pos:=old_pos
	rd_lastpos:=old_lastpos
	return x
end

proc mclunimpl(ichar mess)=
	printf("MCL-UNIMPL: %s\n",mess)
	stop 1
end

proc dumpstr(ichar s, int n, fbuffer=0)=
!fbuffer=1 when outputting contents of buffer

	ref ref char p

	return when n=0

	if outdev=str_io then
		p:=cast(outchan)
		memcpy(p^,s,n)
		p^+:=n
		p^^:=0
		return
	fi

	if fbuffer and n>=2 and outdev=std_io then
		--printptr				!point to last char
		if printptr^=10 then
			if (printptr-1)^=13 then		!crlf
				(printptr-1)^:=0
			else							!lf only
				printptr^:=0
			fi
			puts(printbuffer)
			return
		fi
	fi

	case outdev
	when std_io then
		printf("%.*s",n,s)
	when file_io then
		fprintf(outchan,"%.*s",n,s)
	esac
end

proc dumpprintbuffer=
	if printlen then
		dumpstr(&.printbuffer,printlen,1)
	fi

	resetprintbuffer()
end

proc resetprintbuffer=
	printptr:=&.printbuffer
	printlen:=0
end

proc addtobuffer(ichar s, int n)=
	if printlen+n>=(printbuffer.len-8) then
		dumpprintbuffer()
	fi

	if n<printbuffer.len then
		memcpy(printptr,s,n)
		printptr+:=n
		printlen+:=n
		return
	fi

	dumpstr(s, n)			!don't bother with buffer
end

global function m$power_i64(int64 a,n)int64=
	if n<0 then
		return 0
	elsif n=0 then
		return 1
	elsif n=1 then
		return a
	elsif (n iand 1)=0 then
		return m$power_i64(sqr a,n/2)
	else			!assume odd
		return m$power_i64(sqr a,(n-1)/2)*a
	fi
end
