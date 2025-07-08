=== MA 34 ===
=== msyswin.m 0 0 1/34 ===
module msys
module mlib
module mclib
module mwindows
module mwindll

!proc start=
!	CPL "MSYSWIN/START"
!END
=== msys.m 0 0 2/34 ===
global record procinforec=
	u16			fnindex
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
	char	charmode	! C,M (0)  0 or 'C' or 'M'	o/p int as int or single char or multi-char
	char	heapmode	! D (0)  'D' for str-functions, return ptr to heap string
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

[0:]char digits=s"0123456789ABCDEF"
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

proc start=
	i32 nargs
	int nargs64
	ref[]ichar args
	static [128]byte startupinfo			! 68 or 104 bytes
	int res

!res:=1234567
!res:=0x1234567
!
!CPL "MSYS/START"

	res:=__getmainargs(&nargs,cast(&args),cast(&envstrings),0,cast(&startupinfo))
!	res:=__getmainargs(&nargs,cast(&args),nil,0,cast(&startupinfo))
	
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
	if fmtstyle=nil then
		fmtstyle:="z8H"
	fi
	m$print_u64(a,fmtstyle)
end

export proc m$print_ptr_nf(u64 a)=
	m$print_ptr(a)
end

export proc m$print_i64(i64 a,ichar fmtstyle=nil)=
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

export proc m$print_i64_nf(i64 a)=
	m$print_i64(a)
end

export proc m$print_bool(i64 a, ichar fmtstyle=nil)=
	if a then
		m$print_str("True",fmtstyle)
	else
		m$print_str("False",fmtstyle)
	fi
end

export proc m$print_u64(u64 a,ichar fmtstyle=nil)=
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

export proc m$print_r32(r32 x,ichar fmtstyle=nil)=
	m$print_r64(x,fmtstyle)
end

global proc m$print_c8(i64 a,ichar fmtstyle=nil)=
	[32]char s
!	int cc@s
	fmtrec fmt
	int n
	byte charmode:=0

	nextfmtchars()

	if fmtstyle then
		strtofmt(fmtstyle,-1, &fmt)
		charmode:=fmt.charmode
	fi

	if charmode='M' then
		n:=domultichar(ref char(&a), 8, &.s, &fmt)
!		n:=domultichar(ref char(&a), 8, &.str, fmt)
	else						!assume 'C'
		(ref int(&s)^):=a	
		s[9]:=0

		n:=getutfsize(s)			!isolate size of next char
	fi

	printstr_n(s,n)

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
ABORTPROGRAM("PRTSL")
!	nextfmtchars()
!
!!	fmtrec fmt
!
!	if fmtstyle=nil then
!		printstr_n(cast(s.sliceptr),s.len)
!!		printstr_n(cast(ss.str),ss.length)
!	else
!		abortprogram("FORMATED PRINT SLICE NOT READY")
!!		strtofmt(fmtstyle,-1,&fmt)
!!		tostr_str(s,s.len,&fmt)
!	fi
!	needgap:=1
end

export proc m$print_newline=
!PUTS("<NEWLINE>")
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

!	return when n=0

!	if niostack=1 and outdev in [std_io,file_io] then
!!puts("ADDTO BUFF")
!		addtobuffer(s,n)
!	else
!printf("DUMPSTR %lld\n", n)
		dumpstr(s,n)
!	fi
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
	[4]char str

	str[1]:=ch
	str[2]:=0
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
!A	Convert to upper case
!a	Convert to lower case
!B	Binary
!C	Show int as single n-bit (unicode) character
!D	Duplicate string returned via STRINT etc on heap
!E,F,G	Specify format for double (corresponds to C format codes)
!F
!G
!H	Hex
!JC	Justify centre
!JL	Justify left
!JR	Justify right
!M	Show int as multi-bit (unicode) character
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
		when 'D' then fmt.heapmode:='D'
		when 'C' then fmt.charmode:='C'
		when 'M' then fmt.charmode:='M'
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
	int nchars

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
	ref char s0

	i:=0
	k:=0
	g:=(base=10|3|4)

	repeat
!		if base=10 then			!BUGGY FOR AA OVER I64.MAX
!			assem
!				mov		rcx, [aa]
!				mov		rax, rcx
!				mov		rdx, 7378697629483820647
!				imul	rdx
!				mov		rax, rdx
!				mov		rdx, rcx
!				sar		rdx, 63
!				sar		rax, 2
!				sub		rax, rdx
!				lea		rdx, [rax+rax*4]
!				add		rdx, rdx
!				sub		rcx, rdx
!				mov		[dd], rcx
!				mov		[aa], rax
!			end
!		else
			dd:=aa rem base
			aa:=aa/base
!		fi

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
	int n, usigned
	const i64 mindint=0x8000'0000'0000'0000

	usigned:=0
	if fmt.usigned then
		usigned:=1
	fi
	if aa=mindint and not usigned then		! minint

		str[0]:='-'
		n:=i64mintostr(&str[1],fmt.base,fmt.sepchar)+1

	else
		if (not usigned and aa<0) or fmt.plus then
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
	int n

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
	int i,j,k,g

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

proc tostr_i64(i64 a, ref fmtrec fmt)=
	[360]char str
	int n

	case fmt.charmode
	when 0 then
		n:=i64tostrfmt(a,&.str,fmt)
	when 'M' then
		n:=domultichar(ref char(&a),8,&.str,fmt)

	else						!assume 'C'
		m$print_c8(a, nil)
!		printchar(a)			!no other formatting allowed
		return
	esac

	printstr_n(&.str,n)
end

proc tostr_u64(u64 a, ref fmtrec fmt)=
	[360]char str
	int n

	case fmt.charmode
	when 'M' then
		n:=domultichar(ref char(&a),8,&.str,fmt)

	when 'C' then
		m$print_c8(a, nil)
!		printchar(a)			!no other formatting allowed
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

export function strint(i64 a, ichar fmtstyle=nil)ichar=
	static [100]char str
	ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_i64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

export proc getstrint(i64 a, ichar dest)=
	m$print_startstr(dest)
	tostr_i64(a,getfmt(nil))
	m$print_end()
end

export function strword(u64 a, ichar fmtstyle=nil)ichar=
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
					if termchar in [',', '='] then
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

export function strtoint(ichar s,int length=-1, word base=10)i64=
!return point to next char after terminator (which can be just off length of string)
	byte signd
	u64 aa
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

global function m$read_i64(int fmt=0)i64=
	ref char s
	int length

	fmt:=toupper(fmt)

	case fmt
	when 'C' then
		rd_lastpos:=rd_pos
		if rd_pos^ then
			return rd_pos++^
		else
			return 0
		fi
	when 'T' then
		return termchar
	when 'E' then
		return itemerror
	esac

	s:=readitem(length)

	case fmt
	when 0,'I' then
		return strtoint(s,length)
	when 'B' then
		return strtoint(s,length,2)
	when 'H' then
		return strtoint(s,length,16)
	esac
	return 0
end

global function m$read_r64(int fmt=0)real=
	[512]char str
	ref char s
	int length
	i32 numlength
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
	int length

	itemerror:=0
	if fmt in ['L','l'] then
		s:=rd_pos
		length:=rd_buffer+rd_length-rd_pos

	else
		s:=readitem(length)

		if fmt in ['N','n'] then
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

export function valint(ichar s, int fmt=0)i64=
	ref char old_pos, old_lastpos
	i64 aa

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

	if outdev=str_io then
		p:=cast(outchan)
		if n then
			memcpy(p^,s,n)
			p^+:=n
		fi
		p^^:=0
		return
	fi

	return when n=0
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

global function m$power_i64(i64 a,n)i64=
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

func getutfsize(ref char s)int =
!work out the size in bytes of the ascii or utf8 character that s points to
	int a

	a:=s++^

	if a=0 then						!end of string
		0
	elsif a.[7]=0 then				!ascii
		1
	elsif a.[7..5]=2x110 then
		2
	elsif a.[7..4]=2x1110 then
		3
	elsif a.[7..3]=2x11110 then
		4
	else							!error: just assume a byte of random binary
		1
	fi
end

!export fun `fract(real x)real = fmod(x,1.0)
!export fun fraction(real x)real = fmod(x,1.0)

export fun m$sign_i64(int a)int = (a<0|-1| (a>0|1|0))
export func m$sign_r64(real x)real =
	if x<0 then return -1 fi
	if x>0 then return 1 fi
	0
end
=== mlib.m 0 0 3/34 ===
!const mem_check=1
const mem_check=0

global [0..300]u64 allocupper
global int alloccode				!set by heapalloc
export int allocbytes				!set by heapalloc
export int fdebug=0
export int rfsize

const threshold=1<<25
const alloc_step=1<<25
word maxmemory
int  maxalloccode

!GLOBAL REF VOID ALLOCBASE

byte pcm_setup=0

int show=0

global int memtotal=0
export i64 smallmemtotal=0
global int smallmemobjs=0
global int maxmemtotal=0

!store all allocated pointers
const int maxmemalloc=(mem_check|500000|2)
[maxmemalloc+1]ref i32 memalloctable
[maxmemalloc+1]i32 memallocsize

const pcheapsize=1048576*2
ref byte pcheapstart
ref byte pcheapend			!points to first address past heap
ref byte pcheapptr

const int maxblockindex = 8 		!2048
export const int maxblocksize = 2048
export const int $maxblocksizexx = 2048

[0:maxblocksize+1]byte sizeindextable	!convert byte size to block index 1..maxblockindex

const int size16   = 1			!the various index codes
const int size32   = 2
const int size64   = 3
const int size128  = 4
const int size256  = 5
const int size512  = 6
const int size1024 = 7
const int size2048 = 8

export [0:9]ref word freelist

export record strbuffer =
	ichar strptr
	i32 length
	i32 allocated
end

export enumdata [0:]ichar pmnames=
	(pm_end=0,		$),
	(pm_option,		$),
	(pm_sourcefile,	$),
	(pm_libfile,	$),
	(pm_colon,		$),
	(pm_extra,		$),
end

[2]int seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)

!PROC START=
!CPL "MLIB START"
!END


export function pcm_alloc(int n)ref void =
	ref byte p


	if not pcm_setup then
		pcm_init()
	fi

!GOTO DOLARGE

	if n>maxblocksize then			!large block allocation
!DOLARGE:
		alloccode:=pcm_getac(n)
		allocbytes:=allocupper[alloccode]

		p:=allocmem(allocbytes)
		if not p then
			abortprogram("pcm_alloc failure")
		fi

		return p
	fi

!CPL "DOSMALL"

	alloccode:=sizeindextable[n]		!Size code := 0,1,2 etc for 0, 16, 32 etc
	allocbytes:=allocupper[alloccode]
!	smallmemtotal+:=allocbytes

	if p:=ref byte(freelist[alloccode]) then		!Items of this block size available
		freelist[alloccode]:=ref word(int((freelist[alloccode])^))

		return p
	fi

!No items in freelists: allocate new space in this heap block
	p:=pcheapptr				!Create item at start of remaining pool in heap block
	pcheapptr+:=allocbytes			!Shrink remaining pool

	if pcheapptr>=pcheapend then		!Overflows?
		p:=pcm_newblock(allocbytes)		!Create new heap block, and allocate from start of that
		return p
	fi

	return p
end

export proc pcm_free(ref void p,int n) =
!n can be the actual size requested it does not need to be the allocated size
	int acode

	return when n=0 or p=nil

	if n>maxblocksize then		!large block
		free(p)
	else
		acode:=sizeindextable[n]		!Size code := 0,1,2 etc for 0, 16, 32 etc
		cast(p,ref word)^:=word(int(freelist[acode]))
		freelist[acode]:=p
	fi
end

export proc pcm_freeac(ref void p,int alloc) =
	pcm_free(p,allocupper[alloc])
end

export proc pcm_clearmem(ref void p,int n) =
	memset(p,0,n)
end

export proc pcm_init =
!set up sizeindextable too
	int j, k
	i64 size
	const limit=1<<33

	alloccode:=0
	if pcm_setup then
		return
	fi

	pcm_newblock(0)

	for i to maxblocksize do	!table converts eg. 78 to 4 (4th of 16,32,64,128)
		j:=1
		k:=16
		while i>k do
			k:=k<<1
			++j
		od
		sizeindextable[i]:=j
	od

	allocupper[1]:=16
	size:=16

	for i:=2 to 27 do
		size*:=2
		allocupper[i]:=size
		if size>=threshold then
				k:=i
			exit
		fi
	od

	for i:=k+1 to allocupper.upb do
		size+:=alloc_step
		if size<limit then
			allocupper[i]:=size
			maxmemory:=size
		else
			maxalloccode:=i-1
			exit
		fi
		
	od
	pcm_setup:=1
end

export function pcm_getac(int size)int =
! convert linear blocksize from 0..approx 2GB to 8-bit allocation code

!sizeindextable scales values from 0 to 2048 to allocation code 0 to 9

	if size<=maxblocksize then
		return sizeindextable[size]		!size 0 to 2KB
	fi

	size:=(size+255)>>8					!scale by 256

!now same sizetable can be used for 2KB to 512KB (288 to 2KB)

	if size<=maxblocksize then
		return sizeindextable[size]+8
	fi

!sizetable now used for 512KB to 128MB (to 2KB)
	size:=(size+63)>>6					!scale by 256

	if size<=maxblocksize then
		return sizeindextable[size]+14
	fi

!size>2048, which means it had been over 128MB.
	size:=(size-2048+2047)/2048+22
	return size
end

export function pcm_newblock(int itemsize)ref void=
!create new heap block (can be first)
!also optionally allocate small item at start
!return pointer to this item (and to the heap block)
	static int totalheapsize
	ref byte p

	totalheapsize+:=pcheapsize
	alloccode:=0
	p:=allocmem(pcheapsize)	!can't free this block until appl terminates
	if p=nil then
		abortprogram("Can't alloc pc heap")
	fi
	memset(p,0,pcheapsize)

	pcheapptr:=p
	pcheapend:=p+pcheapsize

	if pcheapstart=nil then		!this is first block
		pcheapstart:=p
	fi
	pcheapptr+:=itemsize
	return ref u32(p)
end

export function pcm_round(int n)int =
!for any size n, return actual number of bytes that would be allocated
	static [0:maxblockindex+1]i32 allocbytes=(0,16,32,64,128,256,512,1024,2048)

	if n>maxblocksize then
		return n
	else
		return allocbytes[sizeindextable[n]]
	fi
end

export function pcm_allocz(int n)ref void =
	ref void p
	p:=pcm_alloc(n)

	memset(p,0,n)
	return p
end

export function pcm_copyheapstring(ref char s)ref char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
	ref char q
	int n
	if s=nil then return nil fi

	n:=strlen(s)+1
	q:=pcm_alloc(n)
	memcpy(q,s,n)
	return q
end

export function pcm_copyheapstringn(ref char s,int n)ref char =
	ref char q
	if s=nil then return nil fi

	q:=pcm_alloc(n+1)
	memcpy(q,s,n)
	(q+n)^:=0
	return q
end

export function pcm_copyheapblock(ref char s, int length)ref char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
	ref char q
	if length=0 then return nil fi

	q:=pcm_alloc(length)
	memcpy(q,s,length)
	return q
end

export function allocmem(int n)ref void =
	ref void p

	p:=malloc(n)
	if p then
		return p
	fi
	println n,memtotal
	abortprogram("Alloc mem failure")
	return nil
end

global function reallocmem(ref void p,int n)ref void =
	p:=realloc(p,n)
	return p when p
	println n
	abortprogram("Realloc mem failure")
	return nil
end

export proc abortprogram(ref char s) =
	println s
	print   "ABORTING: Press key..."
!os_getch()
	stop 5
end

export function getfilesize(filehandle handlex)int=
	u32 p,size

	p:=ftell(handlex)		!current position
	fseek(handlex,0,2)		!get to eof
	size:=ftell(handlex)		!size in bytes
	fseek(handlex,p,seek_set)	!restore position
	return size
end

export proc readrandom(filehandle handlex, ref byte memx, int offset, size) =
	int a
	fseek(handlex,offset,seek_set)
	a:=fread(memx,1,size,handlex)			!assign so as to remove gcc warning
end

export function writerandom(filehandle handlex, ref byte memx, int offset,size)int =
	fseek(handlex,offset,seek_set)
	return fwrite(memx,1,size,handlex)
end

export function setfilepos(filehandle file,int offset)int=
	return fseek(file,offset,0)
end

export function getfilepos(filehandle file)int=
	return ftell(file)
end

export function readfile(ref char filename)ref byte =
	filehandle f
	int size
	ref byte m,p

	f:=fopen(filename,"rb")
	if f=nil then
		return nil
	fi
	rfsize:=size:=getfilesize(f)

	m:=malloc(size+2)		!allow space for etx/zeof etc

	if m=nil then
		return nil
	fi

	readrandom(f,m,0,size)
	p:=m+size			!point to following byte
	(ref u16(p)^:=0)	!add two zero bytes

	fclose(f)
	return m
end

export function writefile(ref char filename,ref byte data,int size)int =
	filehandle f
	int n

	f:=fopen(filename,"wb")
	if f=nil then
		return 0
	fi

	n:=writerandom(f,data,0,size)
	fclose(f)
	return n
end

export function checkfile(ref char file)int=
	filehandle f
	if f:=fopen(file,"rb") then
		fclose(f)
		return 1
	fi
	return 0
end

export proc readlinen(filehandle handlex,ref char buffer,int size) =
!size>2
	int ch
	ref char p
	int n
	byte crseen

	if handlex=nil then
		handlex:=filehandle(os_getstdin())
	fi
	if handlex=nil then
		n:=0
		p:=buffer
		do
			ch:=getchar()
			if ch=13 or ch=10 or ch=-1 then
				p^:=0
				return
			fi
			p++^:=ch
			++n
			if n>=(size-2) then
				p^:=0
				return
			fi
		od
	fi

	buffer^:=0
	if fgets(buffer,size-2,handlex)=nil then
		return
	fi

	n:=strlen(buffer)
	if n=0 then
		return
	fi

	p:=buffer+n-1		!point to last char
	crseen:=0
	while (p>=buffer and (p^=13 or p^=10)) do
		if p^=13 or p^=10 then crseen:=1 fi
		p--^ :=0
	od

!NOTE: this check doesn't work when a line simply doesn't end with cr-lf

	if not crseen and (n+4>size) then
		cpl size,n
		abortprogram("line too long")
	fi
end

export proc iconvlcn(ref char s,int n) =
	to n do
		s^:=tolower(s^)
		++s
	od
end

export proc iconvucn(ref char s,int n) =
	to n do
		s^:=toupper(s^)
		++s
	od
end

export function convlcstring(ref char s)ichar s0=
	s0:=s
	while (s^) do
		s^:=tolower(s^)
		++s
	od
	s0
end

export function convucstring(ref char s)ichar s0=
	s0:=s
	while (s^) do
		s^:=toupper(s^)
		++s
	od
	s0
end

export function changeext(ref char s,newext)ichar=
!whether filespec has an extension or not, change it to newext
!newext should start with "."
!return new string (locally stored static string, so must be used before calling again)
	static [260]char newfile
	[32]char newext2
	ref char sext
	int n

	strcpy(&newfile[1],s)

	case newext^
	when 0 then
		newext2[1]:=0
		newext2[2]:=0
	when '.' then
		strcpy(&newext2[1],newext)
	else
		strcpy(&newext2[1],".")
		strcat(&newext2[1],newext)
	esac


	sext:=extractext(s,1)			!include "." when it is only extension

	case sext^
	when 0 then						!no extension not even "."
		strcat(&newfile[1],&newext2[1])
	when '.' then						!no extension not even "."
		strcat(&newfile[1],&newext2[2])
	else							!has extension
		n:=sext-s-2			!n is number of chars before the "."
		strcpy(&newfile[1]+n+1,&newext2[1])
	esac

	return &newfile[1]
end

export function extractext(ref char s,int period=0)ichar=
!if filespec s has an extension, then return pointer to it otherwise return ""
!if s ends with ".", then returns "."
	ref char t,u

	t:=extractfile(s)

	if t^=0 then			!s contains no filename
		return ""
	fi

!t contains filename+ext
	u:=t+strlen(t)-1		!u points to last char of t

	while u>=t do
		if u^='.' then		!start extension found
			if (u+1)^=0 then		!null extension
				return (period|"."|"")
			fi
			return u+1			!return last part of filename as extension exclude the dot
		fi
		--u
	od
	return ""			!no extension seen
end

export function extractpath(ref char s)ichar=
	static [0:260]char str
	ref char t
	int n

	t:=s+strlen(s)-1		!t points to last char

	while (t>=s) do
		case t^
		when '\\','/',':' then		!path separator or drive letter terminator assume no extension
			n:=t-s+1			!n is number of chars in path, which includes rightmost / or \ or :
			memcpy(&.str,s,n)
			str[n]:=0
			return &.str
		esac
		--t
	od
	return ""			!no path found
end

export function extractfile(ref char s)ichar=
	ref char t

	t:=extractpath(s)

	if t^=0 then			!s contains no path
		return s
	fi

	return s+strlen(t)		!point to last part of s that contains the file
	end

export function extractbasefile(ref char s)ichar=
	static [0:100]char str
	ref char f,e
	int n,flen

	f:=extractfile(s)
	flen:=strlen(f)
	if flen=0 then		!s contains no path
		return ""
	fi
	e:=extractext(f,0)

	if e^ then			!not null extension
		n:=flen-strlen(e)-1
		memcpy(&str,f,n)
		str[n]:=0
		return &.str
	fi
	if (f+flen-1)^='.' then
		memcpy(&str,f,flen-1)
		str[flen-1]:=0
		return &.str
	fi
	return f
end

export function addext(ref char s,ref char newext)ichar=
!when filespec has no extension of its own, add newext
	ref char sext

	sext:=extractext(s,1)

	if sext^=0 then						!no extension not even "."
		return changeext(s,newext)
	fi

	return s							!has own extension; use that
end

export function pcm_alloc32:ref void =
	ref byte p

	allocbytes:=32
!	smallmemtotal+:=32

	if p:=ref byte(freelist[2]) then		!Items of this block size available
		freelist[2]:=ref word(int((freelist[2])^))
		return p
	fi

!No items in freelists: allocate new space in this heap block
	return pcm_alloc(32)
end

export proc pcm_free32(ref void p) =
!n can be the actual size requested it does not need to be the allocated size

!	smallmemtotal-:=32

	cast(p,ref word)^:=word(int(freelist[2]))
	freelist[2]:=p
end

export proc outbyte(filehandle f,int x)=
	fwrite(&x,1,1,f)
end

export proc outu16(filehandle f,word x)=
	fwrite(&x,2,1,f)
end

export proc outu32(filehandle f,word x)=
	fwrite(&x,4,1,f)
end

export proc outu64(filehandle f,u64 x)=
	fwrite(&x,8,1,f)
end

export proc outstring(filehandle f, ichar s)=
	fwrite(s,strlen(s)+1,1,f)
end

export proc outblock(filehandle f, ref void p, int n)=
	fwrite(p,n,1,f)
end

export function myeof(filehandle f)int=
	int c

	c:=fgetc(f)
	if c=c_eof then return 1 fi
	ungetc(c,f)
	return 0
end

export proc strbuffer_add(ref strbuffer dest, ichar s, int n=-1)=
	int newlen,oldlen
	ichar newptr

!	IF N=0 THEN CPL "N=0" FI

	if n=-1 then
		n:=strlen(s)
	fi

	oldlen:=dest.length

	if oldlen=0 then				!first string
		dest.strptr:=pcm_alloc(n+1)
		dest.allocated:=allocbytes
		dest.length:=n				!length always excludes terminator
		memcpy(dest.strptr,s,n)
		(dest.strptr+n)^:=0
		return
	fi

	newlen:=oldlen+n
	if newlen+1>dest.allocated then
		newptr:=pcm_alloc(newlen+1)
		memcpy(newptr,dest.strptr,oldlen)
		dest.strptr:=newptr
		dest.allocated:=allocbytes
	fi

	memcpy(dest.strptr+oldlen,s,n)
	(dest.strptr+newlen)^:=0

	dest.length:=newlen
end

export proc gs_init(ref strbuffer dest)=
	pcm_clearmem(dest,strbuffer.bytes)
end

export proc gs_free(ref strbuffer dest)=
	if dest.allocated then
		pcm_free(dest.strptr,dest.allocated)
	fi
end

export proc gs_str(ref strbuffer dest,ichar s)=
	strbuffer_add(dest,s)
end

export proc gs_char(ref strbuffer dest,int c)=
	[16]char s

	s[1]:=c
	s[2]:=0

	strbuffer_add(dest,&.s,1)
end

export proc gs_strn(ref strbuffer dest,ichar s,int length)=
	strbuffer_add(dest,s,length)
end

export proc gs_strvar(ref strbuffer dest,s)=
	strbuffer_add(dest,s.strptr)
end

export proc gs_strint(ref strbuffer dest,i64 a)=
	strbuffer_add(dest,strint(a))
end

export proc gs_strln(ref strbuffer dest,ichar s)=
	gs_str(dest,s)
	gs_line(dest)
end

export proc gs_strsp(ref strbuffer dest,ichar s)=
	gs_str(dest,s)
	gs_str(dest," ")
end

export proc gs_line(ref strbuffer dest)=
!	strbuffer_add(dest,"\w")
	strbuffer_add(dest,"\n")
end

export function gs_getcol(ref strbuffer dest)int=
	return dest.length
end

export proc gs_leftstr(ref strbuffer dest, ichar s, int w, padch=' ')=
	int col,i,n,slen
	[2560]char str
	col:=dest.length
	strcpy(&.str,s)
	slen:=strlen(s)
	n:=w-slen
	if n>0 then
		for i:=1 to n do
			str[slen+i]:=padch
		od
		str[slen+n+1]:=0
	fi
	gs_str(dest,&.str)
end

export proc gs_leftint(ref strbuffer dest, int a, int w, padch=' ')=
	gs_leftstr(dest,strint(a),w,padch)
end

export proc gs_padto(ref strbuffer dest,int col, ch=' ')=
	int n
	[2560]char str

	n:=col-dest.length
	if n<=0 then return fi
	for i:=1 to n do
		str[i]:=ch
	od
	str[n+1]:=0
	gs_str(dest,&.str)
end

export proc gs_println(ref strbuffer dest,filehandle f=nil)=
	if dest.length=0 then return fi
	(dest.strptr+dest.length)^:=0

	if f=nil then
		println dest.strptr,,"\c"
	else
		println @f,dest.strptr,,"\c"
	fi
end

export function nextcmdparamnew(int &paramno, ichar &name, &value, ichar defext=nil)int=
	static int infile=0
	static ichar filestart=nil
	static ichar fileptr=nil
	static byte colonseen=0
	ref char q
	ichar item,fileext
	int length
	static [300]char str

	reenter:
	value:=nil
	name:=nil

	if infile then
		if readnextfileitem(fileptr,item)=0 then		!eof
			free(filestart)								!file allocated via malloc
			infile:=0
			goto reenter
		fi
	else
		if paramno>ncmdparams then
			return pm_end
		fi
		item:=cmdparams[paramno]
		++paramno

		length:=strlen(item)

		if item^='@' then		!@ file
			filestart:=fileptr:=readfile(item+1)
			if filestart=nil then
				println "Can't open",item
				stop 7
			fi
			infile:=1
			goto reenter
		fi

		if item^=':' then
			colonseen:=1
			return pm_colon
		fi
	fi

	value:=nil
	if item^='-' then
		name:=item+(colonseen|0|1)
		q:=strchr(item,':')
		if not q then
			q:=strchr(item,'=')
		fi
		if q then
			value:=q+1
			q^:=0
		fi
		return (colonseen|pm_extra|pm_option)
	fi

	fileext:=extractext(item,0)
	name:=item

	if fileext^=0 then							!no extension
		strcpy(&.str,name)
		if defext and not colonseen then
			name:=addext(&.str,defext)				!try .c
		fi
!	elsif eqstring(fileext,"dll") then
	elsif eqstring(fileext,"dll") or eqstring(fileext,"mcx") then
		return (colonseen|pm_extra|pm_libfile)
	fi
	return (colonseen|pm_extra|pm_sourcefile)
end

function readnextfileitem(ichar &fileptr,&item)int=
	ref char p,pstart,pend
	int n
	static [256]char str

	p:=fileptr

	reenter:
	do
		case p^
		when ' ','\t',13,10 then	!skip white space
			++p
		when 26,0 then				!eof
			return 0
		else
			exit
		esac
	od

	case p^
	when '!', '#' then			!comment
		++p
		docase p++^
		when 10 then
			goto reenter
		when 26,0 then
			fileptr:=p-1
			return 0
		else

		end docase
	esac


	case p^
	when '"' then				!read until closing "
		pstart:=++p
		do
			case p^
			when 0,26 then
				println "Unexpected EOF in @file"
				stop 8
			when '"' then
				pend:=p++
				if p^=',' then ++p fi
				exit
			esac
			++p
		od
	else
		pstart:=p
		do
			case p^
			when 0,26 then
				pend:=p
				exit
			when ' ','\t',',',13,10 then
				pend:=p++
				exit
			esac
			++p
		od
	esac

	n:=pend-pstart
	if n>=str.len then
		println "@file item too long"
		stop 9
	fi
	memcpy(&.str,pstart,n)
	str[n+1]:=0
	item:=&.str
	fileptr:=p

	return 1
end

export proc ipadstr(ref char s,int width,ref char padchar=" ")=
	int n

	n:=strlen(s)
	to width-n do
		strcat(s,padchar)
	od
end

export function padstr(ref char s,int width,ref char padchar=" ")ichar=
	static [256]char str

	strcpy(&.str,s)
	ipadstr(&.str,width,padchar)
	return &.str
end

export function chr(int c)ichar=
	static [8]char str

	str[1]:=c
	str[2]:=0
	return &.str
end

export function cmpstring(ichar s,t)int=
	int res
	if (res:=strcmp(s,t))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

export function cmpstringn(ichar s,t,int n)int=
	int res
	if (res:=strncmp(s,t,n))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

export function eqstring(ichar s,t)int=
	return strcmp(s,t)=0
end

export function cmpbytes(ref void p,q,int n)int=
	int res
	if (res:=memcmp(p,q,n))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

export function eqbytes(ref void p,q,int n)int=
	return memcmp(p,q,n)=0
end

export proc mseed(u64 a,b=0)=
	seed[1]:=a
	if b then
		seed[2]:=b
	else
		seed[2] ixor:=a
	fi
end

export function mrandom:word =
!return pure 64-bit word value, 0 to 2**64-1
!(cast result for signed value)
!	u64 x,y
	int x,y
	x:=seed[1]
	y:=seed[2]
	seed[1]:=y
	x ixor:=(x<<23)
	seed[2]:= x ixor y ixor (x>>17) ixor (y>>26)
	return seed[2]+y
end

export function mrandomp:int =
!pure 64-bit int value, positive only, 0 to 2**63-1
	return mrandom() iand 0x7FFF'FFFF'FFFF'FFFF
end

export function mrandomint(int n)int=
!positive random int value from 0 to n-1
	return mrandomp() rem n
end

export function mrandomrange(int a,b)int=
!random int value from a to b inclusive
!span extent must be 1 to 2**63-1
	int span
	span:=b-a+1
	if span<=0 then
		return 0
	fi
	return (mrandomp() rem span)+a
end

export function mrandomreal:real x=
!positive random real value from 0 to just under (but not including) 1.0
	repeat x:=mrandomp()/9223372036854775808.0 until x<>1.0
	return x
end

export function mrandomreal1:real=
!positive random real value from 0 to 1.0 inclusive
	return mrandomp()/9223372036854775807.0
end

export function readline:ichar=
	readln
	return rd_buffer
end

export function findfunction(ichar name)ref void=
	for i to $getnprocs() do
		if eqstring($getprocname(i),name) then
			return $getprocaddr(i)
		fi
	od
	return nil
end

export function roundtoblock(int n,align)int=
!round up n until it is a multiple of filealign (which is a power of two)
!return aligned value. Returns original if already aligned
	if n iand (align-1)=0 then return n fi
	return n+(align-(n iand (align-1)))
end

export function pcm_allocnfz(int n)ref void =
!non-freeing allocator for small objects
!n should be a multiple of 8 bytes, but is rounded up here if needed
	ref byte p

!make n a multiple of 8
	if n iand 7 then
		n:=n+(8-(n iand 7))
	fi

	p:=pcheapptr					!Create item at start of remaining pool in heap block
	pcheapptr+:=n					!Shrink remaining pool

	if pcheapptr>=pcheapend then	!Overflows?
		p:=pcm_newblock(n)			!Create new heap block, and allocate from start of that
	fi

	return p
end

!export proc freddy=
!	PRINTLN "FREDDY"
!end
=== mclib.m 0 0 4/34 ===
export type filehandle=ref void

importdll $cstd=
	func malloc		(u64)ref void
	func realloc	(ref void, word)ref void
	proc free		(ref void)
	proc memset		(ref void, i32, word)
	proc memcpy		(ref void, ref void, word)
	proc memmove		(ref void, ref void, word)
	func clock		:i32
	func ftell		(filehandle)i32
	func fseek		(filehandle, i32, i32)i32
	func fread		(ref void, word, word, filehandle)word
	func fwrite		(ref void, word, word, filehandle)word
	func getc		(filehandle)i32
	func ungetc		(i32, filehandle)i32
	func fopen		(ichar a, b="rb")filehandle
	func fclose		(filehandle)i32
	func fgets		(ichar, int, filehandle)ichar
	func remove		(ichar)i32
	func rename		(ichar, ichar)i32
	func getchar	:i32
	proc putchar	(i32)
	proc setbuf		(filehandle, ref byte)

	func strlen		(ichar)int
	func strcpy		(ichar, ichar)ichar
	func strcmp		(ichar, ichar)i32
	func strncmp	(ichar, ichar, word)i32
	func strncpy	(ichar, ichar, word)word
	func memcmp		(ref void, ref void, word)i32
	func strcat		(ichar, ichar)ichar
	func tolower	(i32)i32
	func toupper	(i32)i32
	func isalpha	(i32)i32
	func isupper	(i32)i32
	func islower	(i32)i32
	func isalnum	(i32)i32
	func isspace	(i32)i32
	func strstr		(ichar, ichar)ichar
	func atol		(ichar)int
	func atoi		(ichar)i32
	func strtod		(ichar,ref ref char)r64
	func _strdup	(ichar)ichar

	func puts		(ichar)i32
	func printf		(ichar, ...)i32

	func sprintf	(ichar, ichar, ...)i32

	func sscanf		(ichar, ichar, ...)i32
	func scanf		(ichar, ...)i32

	func rand		:i32
	proc srand		(u32)
	func system		(ichar)i32

	func fgetc		(filehandle)i32
	func fputc		(i32,  filehandle)i32
	func fprintf	(filehandle, ichar, ...)i32
	func fputs		(ichar,  filehandle)i32
	func feof		(filehandle)i32
	func getch		:i32
	func _getch		:i32
	func kbhit		:i32
	func _mkdir		(ichar)i32
	func mkdir		(ichar)i32
	func strchr		(ichar,i32)ichar

	func _setmode	(i32,i32)i32

	proc _exit		(i32)
	proc "exit"		(i32)
!	proc `exit		(i32)
	func pow		(real,real)real

	func `sin 		(real)real
	func `cos		(real)real
	func `tan		(real)real
	func `asin		(real)real
	func `acos		(real)real
	func `atan 		(real)real
	func `log		(real)real
	func `log10		(real)real
	func `exp		(real)real
	func `floor		(real)real
	func `ceil		(real)real

	proc  qsort   	(ref void, u64, u64, ref proc)

end

export macro strdup=_strdup

importdll $cstdextra=
	func __getmainargs	(ref i32, ref void, ref void, int, ref void)i32
end

export const c_eof		=-1
export const seek_set	= 0
export const seek_curr	= 1
export const seek_end	= 2
=== mwindows.m 0 0 5/34 ===
const wm_destroy=2

export type wt_word	= u16
export type wt_wordpm	= u32
export type wt_bool	= u32
export type wt_dword	= u32
export type wt_wchar	= u16
export type wt_wcharpm	= u32
export type wt_char	= byte
export type wt_ichar	= ref char
export type wt_ptr		= ref void
export type wt_wndproc	= ref proc
export type wt_handle	= ref void
export type wt_int		= i32
export type wt_uint	= u32
export type wt_long	= i32
export type wt_wparam	= word
export type wt_lparam	= word
export type wt_point	= rpoint

export record rsystemtime =
	wt_word year
	wt_word month
	wt_word dayofweek
	wt_word day
	wt_word hour
	wt_word minute
	wt_word second
	wt_word milliseconds
end

importdll $windowsdlls=
!	func "VirtualAlloc"(wt_ptr, dint,wt_dword,wt_dword)wt_ptr
	func "GetStdHandle"(wt_dword)wt_handle
	func "GetConsoleScreenBufferInfo"(wt_handle,wt_ptr)int
	func "SetConsoleCtrlHandler"(wt_wndproc,int)int
	func "SetConsoleMode"(wt_handle,wt_dword)int
	func "CreateProcessA"(wt_ichar,wt_ichar,wt_ptr,wt_ptr, int,
						wt_dword, wt_ptr,wt_ichar,wt_ptr,wt_ptr)int
	func "GetLastError":wt_dword
	func "WaitForSingleObject"(wt_handle,wt_dword)wt_dword
	func "GetExitCodeProcess"(wt_handle,wt_ptr)int
	func "CloseHandle"(wt_handle)int
	func "GetNumberOfConsoleInputEvents"(wt_handle,wt_ptr)int
	func "FlushConsoleInputBuffer"(wt_handle)int
	func "LoadLibraryA"(wt_ichar)wt_handle
!	func "GetProcAddress"(wt_handle,wt_ichar)wt_wndproc
	func "GetProcAddress"(wt_handle,wt_ichar)ref void
	func "LoadCursorA"(wt_handle,wt_ichar)wt_handle
	func "RegisterClassExA"(wt_ptr)wt_wordpm
	func "DefWindowProcA"(wt_handle,wt_uint,wt_wparam,wt_lparam)int
	func "ReadConsoleInputA"(wt_handle,wt_ptr,wt_dword,wt_ptr)int
	proc "Sleep"(wt_dword)
	func "GetModuleFileNameA"(wt_handle,wt_ichar,wt_dword)wt_dword

	proc "ExitProcess"(wt_uint)
	proc "PostQuitMessage"(wt_int)

	proc "MessageBoxA"(wt_int x=0,wt_ichar message, caption="Caption",wt_int y=0)

	func "QueryPerformanceCounter"(ref i64)wt_bool
	func "QueryPerformanceFrequency"(ref i64)wt_bool

	func "CreateFileA"(wt_ichar,wt_dword,wt_dword,wt_ptr,wt_dword,wt_dword,wt_handle)wt_handle
	func "GetFileTime"(wt_handle,wt_ptr,wt_ptr,wt_ptr)wt_bool

	proc "GetSystemTime"(ref rsystemtime)
	proc "GetLocalTime"(ref rsystemtime)

	func "GetTickCount64":u64
	func "PeekMessageA"		(ref void, ref wt_handle, wt_uint,wt_uint,wt_uint)wt_bool

	func "GetCommandLineA":ichar

	func "VirtualAlloc" (ref void, wt_dword, wt_dword, wt_dword)ref void
	func "VirtualProtect" (ref void, wt_dword, wt_dword, ref wt_dword)wt_bool

	func "WriteConsoleA" (ref void, ref void, i32, ref i32, ref void)wt_bool

	func "FindFirstFileA" (wt_ichar,ref rfinddata)wt_handle
	func "FindNextFileA"  (wt_handle, ref rfinddata)wt_bool
	func "FindClose"      (wt_handle)wt_bool

	func "MessageBeep"    (i32)wt_bool
	func "Beep"    (i32 freq, dur)wt_bool
end

record input_record = $caligned
	wt_word	eventtype
!	u16	padding
		wt_bool	keydown			!key event record (was inside 'Event' union in win32)
		wt_word	repeatcount
		wt_word	virtualkeycode
		wt_word	virtualscancode
		union
			wt_word unicodechar
			wt_char asciichar
		end
		wt_dword controlkeystate
end

record rspoint=(i16 x,y)

record rsrect=
	i16 leftx,top,rightx,bottom
end

global record rpoint =
	wt_long x,y
end

record rconsole=
	rspoint size,pos
	u16 attributes
	rsrect window
	rspoint maxwindowsize
end

record rstartupinfo =
	wt_dword	size
	u32 dummy1
	wt_ichar	reserved
	wt_ichar	desktop
	wt_ichar	title
	wt_dword	x
	wt_dword	y
	wt_dword	xsize
	wt_dword	ysize
	wt_dword	xcountchars
	wt_dword	ycountchars
	wt_dword	fillattribute
	wt_dword	flags
	wt_word		showwindow
	wt_word		reserved2
	u32 dummy2
	wt_ptr		reserved4
	wt_handle	stdinput
	wt_handle	stdoutput
	wt_handle	stderror
end

record rprocess_information =
	wt_handle process
	wt_handle thread
	wt_dword processid
	wt_dword threadid
end

record rwndclassex =
	wt_uint		size
	wt_uint		style
	wt_wndproc	wndproc
	wt_int		clsextra
	wt_int		wndextra
	wt_handle	instance
	wt_handle	icon
	wt_handle	cursor
	wt_handle	background
	wt_ichar	menuname
	wt_ichar	classname
	wt_handle	iconsm
end

global record rmsg =
	wt_handle	hwnd
	wt_uint		message
	u32		dummy1
	wt_wparam	wParam
	wt_lparam	lParam
	wt_dword	time
	u32		dummy2
	wt_point	pt
end

record rfiletime =
	wt_dword lowdatetime
	wt_dword highdatetime
end

record rfinddata =
	wt_dword	fileattributes
	rfiletime	creationtime
	rfiletime	lastaccesstime
	rfiletime	lastwritetime
	wt_dword	filesizehigh
	wt_dword	filesizelow
	wt_dword	reserved0
	wt_dword	reserved1
	[260]char	filename
	[14]char		altfilename
	wt_dword	obs1, obs2
	wt_word		obs3
end

const NORMAL_PRIORITY_CLASS=32
const CREATE_NEW_CONSOLE=16
const DETACHED_PROCESS=16

const MEM_COMMIT				= 4096
const MEM_RESERVE				= 8192
const PAGE_EXECUTE				= 16
const PAGE_EXECUTE_READ			= 32
const PAGE_EXECUTE_READWRITE	= 64
const PAGE_NOACCESS				= 1


export wt_handle hconsole, hconsolein

input_record lastkey, pendkey
int keypending			!whether pendkey contains a new key event detected by flushkbd

int hpfreq				!counts per msec


ref func (ref void)int wndproc_callbackfn=nil	!windows call-back: address of handler

int init_flag=0

export proc os_init=
!general initialisation
	hconsole:=GetStdHandle(u32(-11))
	hconsolein:=GetStdHandle(u32(-10))

	lastkey.repeatcount:=0
	keypending:=0

	SetConsoleCtrlHandler(nil,1)

	SetConsoleMode(hconsole,1 ior 2)

	init_flag:=1

end

export func os_execwait(ichar cmdline,int newconsole=0,ichar workdir=nil)int =
	wt_dword exitcode
	int status
	int cflags:=0

	rstartupinfo si
	rprocess_information xpi

	clear si
	clear xpi

	case newconsole
	when 0 then cflags := NORMAL_PRIORITY_CLASS
	when 1 then cflags := NORMAL_PRIORITY_CLASS ior CREATE_NEW_CONSOLE
	when 2 then cflags := NORMAL_PRIORITY_CLASS ior DETACHED_PROCESS
	esac

	si.size := rstartupinfo.bytes

	status:=CreateProcessA(
		nil,
		cmdline,
		nil,

		nil,
		1,
		cflags,

		nil,
		nil,
		&si,
		&xpi )

	if status=0 then		!fails
		status:=GetLastError()
		printf("Winexec error: %lld\n",status)
		return -1
	fi

	WaitForSingleObject(xpi.process, 0xFFFF'FFFF)
	GetExitCodeProcess(xpi.process,&exitcode)

	CloseHandle(xpi.process)
	CloseHandle(xpi.thread)

	return exitcode
end

export func os_execcmd(ichar cmdline, int newconsole=0)int =
	rstartupinfo si
	rprocess_information xpi

	clear si
	clear xpi

	si.size := rstartupinfo.bytes

	CreateProcessA( nil,
		cmdline,
		nil,
		nil,
		1,
		NORMAL_PRIORITY_CLASS ior (newconsole|CREATE_NEW_CONSOLE|0),
		nil,
		nil,
		&si,
		&xpi )

	CloseHandle(xpi.process)
	CloseHandle(xpi.thread)

	return 1
end

export func os_getch:int=
	int k

	k:=os_getchx() iand 255

	return k
end

export func os_kbhit:int=
	wt_dword count

	unless init_flag then os_init() end

	GetNumberOfConsoleInputEvents(hconsolein,&count)
	return count>1
end

export func os_getdllinst(ichar name)u64=
	wt_handle hinst

	hinst:=LoadLibraryA(name)
	return cast(hinst)
end

export func os_getdllprocaddr(int hinst,ichar name)ref void=
	return GetProcAddress(cast(hinst),name)
end

export proc os_initwindows=
	os_init()
	os_gxregisterclass("pcc001")
end

export proc os_gxregisterclass(ichar classname)=
	const idcarrow=32512
	rwndclassex r
	static byte registered

	if registered then
		return
	fi

	clear r

	r.size:=r.bytes
	r.style:=8 ior 32
	r.wndproc:=cast(&mainwndproc)
	r.instance:=nil

	r.icon:=nil
	r.cursor:=LoadCursorA(nil,ref void(idcarrow))
	r.background:=cast(15+1)
	r.menuname:=nil
	r.classname:=classname
	r.iconsm:=nil

	if RegisterClassExA(&r)=0 then
		printf("Regclass error: %lld %lld\n",classname,GetLastError())
		stop 1
	end
	registered:=1
end

global function mainwndproc (
		wt_handle hwnd, wt_uint message, wt_wparam wParam, wt_lparam lParam)int=
	rmsg m
	int result
	static int count=0

!CPL "MAINWNDPROC",HWND

	m.hwnd:=hwnd
	m.message:=message
	m.wParam:=wParam
	m.lParam:=lParam
	m.pt.x:=0
	m.pt.y:=0
	
	if (wndproc_callbackfn) then
		result:=(wndproc_callbackfn^)(&m)
	else
		result:=0
	fi

	if m.message=wm_destroy then
		return 0
	fi

	if not result then
		return DefWindowProcA(hwnd,message,wParam,lParam)
	else
		return 0
	fi
end

export proc os_setmesshandler(ref void addr)=
	wndproc_callbackfn:=addr
end

export func os_getchx:int=
!Q! function  os_getchx_c:int
!return a 32-bit value containing:
! 15..B0:	char code
! 23..16	virtual keycode
! 31..24	shift flags (.[24]=shift, .[25]=ctrl, .[26]=alt, .[27]=capslock)
	const rightaltmask	= 1
	const leftaltmask	= 2
	const leftctrlmask	= 8
	const rightctrlmask	= 4
	const shiftmask		= 16
	const capsmask		= 128
	const scrollmask	= 64
	int count
	int charcode,keyshift,keycode
	int altdown,ctrldown,shiftdown,capslock

!os_init() unless init_flag
	unless init_flag then os_init() end

	if keypending then
		lastkey:=pendkey
		keypending:=0
	else
		if lastkey.repeatcount=0 then
			repeat
				count:=0
				ReadConsoleInputA(hconsolein,&lastkey,1,&count)
			until (lastkey.eventtype=1 and lastkey.keydown=1)
		fi
	fi

!set shift flags

	altdown		:= ((lastkey.controlkeystate iand (leftaltmask ior rightaltmask))|1|0)
	ctrldown	:= ((lastkey.controlkeystate iand (leftctrlmask ior rightctrlmask))|1|0)
	shiftdown	:= ((lastkey.controlkeystate iand shiftmask)|1|0)
	capslock	:= ((lastkey.controlkeystate iand capsmask)|1|0)

	--lastkey.repeatcount		!count this key out

	charcode:=lastkey.asciichar
	keycode:=lastkey.virtualkeycode iand 255

	if charcode<0 then
		if charcode<-128 then
			charcode:=0
		else
			charcode+:=256
		fi
	fi

!for keycodes in range 186 to 223, which are all stand-alone punctuation keys, I might
!wish to set charcode to the appropriate printed char code (currently charcode will be
!zero, and keyboard handlers need to detect keycodes such as vkequals)
!....

	if altdown and ctrldown and charcode=166 then
		altdown:=ctrldown:=0
	else
		if altdown or ctrldown then
			charcode:=0
			if keycode>='A' and keycode<= 'Z' then
				charcode:=keycode-'@'
			fi
		fi
	fi

	keyshift:=capslock<<3 ior altdown<<2 ior ctrldown<<1 ior shiftdown

	return keyshift<<24 ior keycode<<16 ior charcode
end

export func os_getos=>ichar=
	return "W64"
end

export func os_gethostsize=>int=
	return 64
end

export func os_shellexec(ichar opc, file)int=
	return system(file)
end

export proc os_sleep(int a)=
	Sleep(a)
end

export func os_getstdin:filehandle =
	return fopen("con","rb")
end

export func os_getstdout:filehandle =
	return fopen("con","wb")
end

export func os_gethostname:ichar=
	static [300]char name
	static int n

	GetModuleFileNameA(nil,&.name,name.bytes)
	return &.name
end

export func os_getmpath:ichar=
!BART
!	return "C:\\m\\"
	return F"C:@@@@\m\" !ABC
!	return "C:@@@@\\m\\" !ABC
end

export func os_clock:i64=
!	return clock()
	return os_hpcounter()
end

export func os_ticks:i64=
	return GetTickCount64()
end

export func os_iswindows:int=
	return 1
end

export proc os_getsystime(ref rsystemtime tm)=
	GetLocalTime(tm)
end

export proc os_peek=
	int ticks
	static int lastticks
	[100]byte m
	ticks:=GetTickCount64()
	if ticks-lastticks>=1000 then
		lastticks:=ticks
		PeekMessageA(&m,nil,0,0,0)
	fi
end

export func os_allocexecmem(int n)ref byte=
	ref byte p
	u32 oldprot
	int status

	p := VirtualAlloc(nil, n, MEM_RESERVE ior MEM_COMMIT, PAGE_NOACCESS)
	if p = nil then return nil fi

	status := VirtualProtect(p, n, PAGE_EXECUTE_READWRITE, &oldprot)
	if status = 0 then return nil fi

	return p
end

export func dirlist(ichar filespec, ref[]ichar dest, int capacity, t=1)int=
!filespec is a filename (eg. "*.dwg") with possible drive/path; scan
!directory for all matching files:
! Store each file in dest array up to capacity
! Return:
!  -1:	capacity exceeded
!   N:  number of files found including 0 for no matching files

!t has this value
! +1  Include normal files only, no sub-directory names
! +2  Include directories
! +3  (+1 +2) Include all files including directories
! +4  Convert to lower case
	ref void hfind
	rfinddata file
	int nfiles:=0
	[300]char path
	[300]char fullfilename

	strcpy(path, extractpath(filespec))


	if (hfind:=findfirstfilea(filespec,&file))<>ref void(-1) then	!at least one file
		repeat
			if (file.fileattributes iand 16) then		!this is a directory
				if (t iand 2)=0 then nextloop fi		!no directories
			else						!this is a file
				if (t iand 1)=0 then nextloop fi
			fi
			if nfiles>=capacity then
				nfiles:=-1
				exit
			fi

			if (t iand 4) then				!to lower case
				convlcstring(file.filename)
!				convlcstring(&.file.filename)
			fi
			strcpy(fullfilename, path)
			strcat(fullfilename, file.filename)

			dest[++nfiles]:=pcm_copyheapstring(fullfilename)

		until not findnextfilea(hfind,&file)
		findclose(hfind)
	fi
	return nfiles
end

export func os_hpcounter:int a =
!return counter such that successive calls indicate duration in msec

	if hpfreq=0 then
		hpfreq:=os_hpfreq()/1000		!counts per msec
	fi

	QueryPerformanceCounter(&a)
	a/hpfreq
end

export func os_hpfreq:int a =
	QueryPerformanceFrequency(&a)
	a
end

=== mwindll.m 0 0 6/34 ===
export function os_calldllfunction(
	ref proc fnaddr,
	int retcode, nargs,
	ref[]i64 args,
	ref[]byte argcodes)u64 =

	u64 a
	r64 x
	int nextra := 0, pushedbytes

!Stack is 16-byte aligned at this point

CPL "NO CALLDLL"
STOP

	if nargs<4 then
		nextra:=4-nargs			!need at least 4 slots for shadow space
	elsif nargs.odd then		!need one more for a 16-byte-aligned stack
		nextra:=1
	fi

	pushedbytes:=(nextra+nargs)*8

	to nextra do
!		asm push 0
	od

	for i:=nargs downto 1 do
		a:=args[i]				!get generic 64-bit value to push
!		asm push u64 [a]
	od

! blindly load first 4 args to both int/float regs, whether used or not,
! and assuming calling a variadic function whether it is or not

!	assem
!		mov D10,   [Dstack]
!		movq XMM0, [Dstack]
!		mov D11,   [Dstack+8]
!		movq XMM1, [Dstack+8]
!		mov D12,   [Dstack+16]
!		movq XMM2, [Dstack+16]
!		mov D13,   [Dstack+24]
!		movq XMM3, [Dstack+24]
!	end

	if retcode='I' then
		a:=(ref func:i64(fnaddr))^()
!		asm add Dstack,[pushedbytes]
		return a

	else
		x:=(ref func:r64(fnaddr))^()
!		asm add Dstack,[pushedbytes]
		return u64@(x)			!(type-punning cast)

	fi
end	
=== mm.m 0 0 7/34 ===
!project =
	module mm_cli

	module mm_gentcl
	module mm_libtcl
	module mm_blocktcl

	MODULE DUMMY
	MODULE TC_API
	MODULE TC_DECLS
	MODULE TC_DIAGS
	MODULE TC_TABLES

	module mm_decls

	module mm_diags
!	module mm_diags_dummy
!
	module mm_export_dummy
!	module mm_exportq
!	module mm_exportm

	module mm_lex
	module mm_lib

!	module mm_libsources
	module mm_libsources_dummy
!
	module mm_modules
	module mm_name
	module mm_parse

	module mm_support
	module mm_tables
	module mm_type

	module mc_decls
	module mc_genmcl
!	module mc_auxmcl
	module mc_libmcl
	module mc_objdecls
	module mc_writeasm


!	$sourcepath "c:/px/"
!	$sourcepath "c:/xxx/"
!	import pcl
!	import pclmin
!	import pclrunx

!	$sourcepath "c:/qx/"
!	import qc

!end

!global type int8	= i8
!global type int16	= i16
!global type int32	= i32
!global type int64	= i64
!
!global type word8	= u8
!global type word16	= u16
!global type word32	= u32
!global type word64	= u64
!
!global type char64	= c64
!
!global type real32	= r32
!global type real64	= r64


proc main=
	main2()
end

=== mm_cli.m 0 0 8/34 ===

global ichar syslibname=""

!macro SHOW(m) = println m
macro SHOW(m) = eval 0

!main production options; passnames are also file extensions for outputs

global enumdata []ichar passnames =
!								Output (when this is the final step)
	(ma_pass,		"ma"),			! .ma     These are are special
	(getst_pass,	"list"),		! .list
	(getproj_pass,	"proj"),		! .prog

	(pcl_pass,		"pcl"),			! .pcl
	(runpcl_pass,	"(int)"),		! interpret
	(mcl_pass,		"asm"),			! .asm
	(obj_pass,		"obj"),			! .obj (via .asm and aa)
	(dll_pass,		"dll"),			! .dll
	(exe_pass,		"exe"),			! .exe
	(mx_pass,		"mx"),			! .mx
	(run_pass,		"(run)"),		! run in-memory
end

!options used in debugging
global enumdata []ichar dpassnames =
	(dload_pass,	$),
	(dparse_pass,	$),
	(dfixup_pass,	$),
	(dname_pass,	$),
	(dtype_pass,	$),
	(dpcl_pass,		$),
	(dmcl_pass,		$),		!all-inclusive up to this point (includes all prev passes)
	(dss_pass,		$),		!only one of these 3 will be done
end

enumdata []ichar optionnames, []byte optionvalues =

!special outputs
	(ma_sw,			"ma",			ma_pass),
	(getst_sw,		"getst",		getst_pass),
	(getproj_sw,	"getproj",		getproj_pass),

!normal production outputs
	(pcl_sw,		"p",			pcl_pass),
	(runpcl_sw,		"i",			runpcl_pass),
	(asm_sw,		"a",			mcl_pass),
	(nasm_sw,		"nasm",			mcl_pass),
	(obj_sw,		"obj",			obj_pass),
	(dll_sw,		"dll",			dll_pass),
	(dll2_sw,		"d",			dll_pass),
	(exe_sw,		"exe",			exe_pass),		!default
	(mx_sw,			"mx",			mx_pass),
	(run_sw,		"r",			run_pass),		!default with ms.exe

!debugging options; these set debug mode; above are production mode
	(dload_sw,		"dload",		dload_pass),
	(dparse_sw,		"dparse",		dparse_pass),
	(dfixup_sw,		"dfixup",		dfixup_pass),
	(dname_sw,		"dname",		dname_pass),
	(dtype_sw,		"dtype",		dtype_pass),
	(dpcl_sw,		"dpcl",			dpcl_pass),
	(dmcl_sw,		"dmcl",			dmcl_pass),
	(dss_sw,		"dss",			dss_pass),

	(sys_sw,		"sys",			2),
	(minsys_sw,		"min",			1),
	(nosys_sw,		"nosys",		0),
	(clinux_sw,		"linux",		0),

	(noopt_sw,		"no",			0),
	(nopeephole_sw,	"nopeep",		0),
	(noregoptim_sw,	"noregs",		0),

!diagnostic outputs, only relevant for debug mode
	(ast1_sw,		"ast1",			0),
	(ast2_sw,		"ast2",			0),
	(ast3_sw,		"ast3",			0),
	(showc_sw,		"showc",		0),
	(showpcl_sw,	"showpcl",		0),
	(showasm_sw,	"showasm",		0),
	(st_sw,			"st",			0),
	(stflat_sw,		"stflat",		0),
	(types_sw,		"types",		0),
	(showss_sw,		"showss",		0),
	(showmodules_sw,"modules",		0),
	(shortnames_sw,	"shortnames",	0),

	(pst_sw,		"pst",			0),

	(time_sw,		"time",			0),
	(v_sw,			"v",			2),
	(vv_sw,			"vv",			3),
	(quiet_sw,		"q",			0),
	(csize_sw,		"cs",			1),
	(size_sw,		"ss",			2),
	(help_sw,		"h",			0),
	(help2_sw,		"help",			0),
	(ext_sw,		"ext",			0),
	(out_sw,		"o",			0),
	(outpath_sw,	"outpath",		0),
	(unused_sw,		"unused",		0),

	(norip_sw,		"norip",		0),
	(himem_sw,		"himem",		2),
end


byte msfile

global const logfile="mx.log"

ichar outext=""				!for reporting of primary o/p file

global int startclock, endclock
global int cmdskip

global ichar inputfile

global int loadtime
global int parsetime
global int resolvetime
global int typetime
global int ctime
global int pcltime
global int compiletime

global proc main2=
!STOP
!proc main=
	unit p,q,r
	int m,fileno,ntokens,t,tt

cpl "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

CPL =PCLNAMES.LEN
CPL =Pstrec.bytes
CPL =PCLREC.bytes
!CPL =Pstrec.len

!for s in pclnames do
!	fprintln "	when k# then",s:"8jl"
!	println "		unimpl(pc)"
!	println
!
!od
!STOP

!TESTLIBS()
!STOP

!CPL "TEST VERSION"
!CPL STREC.BYTES
!CPL PCLREC.BYTES

	startclock:=os_clock()
PSTARTCLOCK:=STARTCLOCK
	initdata()

	getinputoptions()


!CPL =PASSNAMES[PASSLEVEL],=PASSLEVEL
!CPL =CTARGET
!CPL =DPASSNAMES[DPASSLEVEL]
!!CPL =CTARGET
!CPL =CLINUX
!CPL =MSYSLEVEL


	if prodmode then
		production_compiler()
	else
		debug_compiler()
	fi

	if fverbose=3 then println "Finished." fi
end

proc debug_compiler=

	println "DEBUG COMPILATION"
!CPL "DEBUG",$LINENO

	loadproject(inputfile)
!CPL "DEBUG",$LINENO

	IF DPASSLEVEL=DPCL_PASS THEN PASSLEVEL:=PCL_PASS		!enable diags
	ELSIF DPASSLEVEL=DMCL_PASS THEN PASSLEVEL:=MCL_PASS
	FI

!CPL "DEBUG",$LINENO
	do_parse(fshowast1) when dpasslevel>=dparse_pass
!CPL "DEBUG",$LINENO

	do_name(fshowast2) when dpasslevel>=dname_pass

!CPL "DEBUG",$LINENO
	do_type(fshowast3) when dpasslevel>=dtype_pass
!CPL "DEBUG",$LINENO

	do_genpcl(fshowpcl or fshowpst) when dpasslevel>=dpcl_pass

!CPL "DEBUG",$LINENO
	do_genmcl(fshowasm) when dpasslevel>=dmcl_pass

	pcl_genss() when dpasslevel>=dss_pass

SHOWSURVEYS()

	showtimings() when fshowtiming

	showlogfile()

end

proc production_compiler=
!	println "Production:"
	showcompilemess()

! Do early passes common to all options

!BEEP(2000,20)
CPL "PRODUCTION"

	loadproject(inputfile)

	do_parse()
	do_name()
	do_type()

! Special outputs can be done at this point
	do_writema(inputfile)	when passlevel=ma_pass			! terminates
	do_getinfo(inputfile)	when passlevel in [getst_pass, getproj_pass]		! terminates
	do_writeexports()		when passlevel=dll_pass

	do_genpcl(passlevel=pcl_pass)

	pcl_runpcl() when passlevel=runpcl_pass				!terminates

! Deal with chosen output kind

!CPL PASSNAMES[PASSLEVEL]

	if passlevel>=mcl_pass then
		do_genmcl(passlevel=mcl_pass)
!		do_genmcl(passlevel=mcl_pass)

		case passlevel
		when obj_pass then
			pcl_writeobj(changeext(outfile, "obj"))	

		when exe_pass then
			pcl_writeexe(changeext(outfile, "exe"))	

		when dll_pass then
			pcl_writedll(changeext(outfile, "dll"))	

		when mx_pass then
			pcl_writemx(changeext(outfile, "mx"))	

		when run_pass then
			pcl_exec()
!			do_genss()
!			do_run()

		esac
	fi
SHOWSURVEYS()

!CPL =NIF
!CPL =NWHEN
!CPL =NUNLESS
!CPL =NALLSYMS

!CPL =NUNITS
!CPL =PCLSEQNO
!CPL =MCLSEQNO
!CPL =NMCLOPND

!CPL =NALLCALLS
!CPL =NUSESTACK
!CPL =NUSEMIXEDSTACK

!CPL UNITREC.BYTES
!CPL PCLREC.BYTES
!CPL MCLREC.BYTES
!CPL MCLOPNDREC.BYTES
!
	showtimings() when fshowtiming
!	showtimings() when fshowtiming
!	showtimings() when fshowtiming
end

proc showcompilemess=
	if fverbose>=1 and not msfile then
		fprintln "Compiling # to #",inputfile,changeext(outfile,(ctarget|"c"|passnames[passlevel]))
	fi
end

proc do_parse(int flog=0)=
!	if fverbose=3 then println "PARSE" fi

	int tt:=clock()

!CPL "PARSE",$LINENO

	for i to nmodules do
!CPL "PARSE",$LINENO,MODULES[I].NAME
		parsemodule(modules[i])
	od
!CPL "PARSE",$LINENO
	parsetime:=clock()-tt

	if prodmode or dpasslevel>=dfixup_pass then
!		if fverbose=3 then println "FIXUP" fi
		fixusertypes()
	fi
!CPL "PARSE",$LINENO

	fixstartprocs()
!CPL "PARSE",$LINENO
!
	if flog then showast("AST1") fi
!CPL "PARSE",$LINENO
end

proc do_name(int flog=0)=
!	if fverbose=3 then println "NAME" fi

	int tt:=clock()
	rx_typetable()

	for i:=2 to nmodules do
		rx_module(i)
	od
	rx_module(1)
	resolvetime:=clock()-tt

	if flog then showast("AST2") fi
end

proc do_type(int flog=0)=
!	if fverbose=3 then println "TYPE" fi

	int tt:=clock()
	tx_typetable()

	for i:=1 to nmodules do
		tx_module(i)
	od
	tx_allprocs()
	typetime:=clock()-tt

	if flog then showast("AST3") fi
end

proc do_genc=
!	if fverbose=3 then println "GENPCL" fi
	int tt:=clock()
	ichar file:=pcm_copyheapstring(changeext(outfile,"c"))

!CPL "CALL CODEGEN CLANG..."
	codegen_il(file)
!CPL "DONE"

	ctime:=clock()-tt

!	println "WRITE C FILE:", changeext(outfile, "c"),"..."
	println "WRITE C FILE:", file, DEST.LENGTH
	writegsfile(file, dest)
end

proc do_genpcl(int flog=0)=
!	if fverbose=3 then println "GENPCL" fi

	int tt:=clock()

CPL "GENPCL-------------"

	codegen_il(nil)

	pcltime:=clock()-tt

!CPL =FREGOPTIM, =FPEEPHOLE

!	pcl_reducetest() when fregoptim or fpeephole
!	pcltime:=clock()-tt

	if flog then
		if fshowpcl or passlevel=pcl_pass then
!CPL "NOT WRITING PCL"
			pcl_writepcl(changeext(outfile, "pcl"))
		fi

		if fshowpst and passlevel=pcl_pass then		!for mcl+ it is o/p later
			pcl_writepst("PSYMTAB")
		fi

	fi

end

proc do_genmcl(int flog=0)=
!	if fverbose=3 then println "GENMCL" fi

	int tt:=clock()

	pcl_genmcl()
	mcltime:=clock()-tt

!CPL "GENMCL", =OUTEXT

	if flog then
!CPL "MCL1"
		pcl_writeasm(changeext(outfile, (ctarget|"c"|"asm")))
!CPL "MCL2"
	fi

	if fshowpst and passlevel>pcl_pass then
		pcl_writepst("PSYMTAB")
	fi

end

proc initdata=
	imodule pm
	ifile pf

	pcm_init()
	lexsetup()
	init_tt_tables()
	initbblib()

	pm:=pcm_allocz(modulerec.bytes)

	pm.name:="PROGRAM"

	stprogram:=createdupldef(nil,addnamestr("$prog"),programid)
	pm.stmodule:=stprogram
	modules[0]:=pm

	igetmsourceinfo:=cast(mgetsourceinfo)

!	idomcl_assem:=cast(domcl_assem)
	igethostfn:=cast(findhostfn)

	REMOVE("PSYMTAB")

end

proc getinputoptions=
	int paramno,pmtype,sw,extlen
	ichar name,value,ext
	[300]char filespec

	if pc_userunpcl then
		passlevel:=runpcl_pass
		prodmode:=1
		fverbose:=0
	fi
	paramno:=1

	if eqstring(extractfile(os_gethostname()),"ms.exe") then
		msfile:=1
		fverbose:=0
		do_option(run_sw, "")
	fi

	while pmtype:=nextcmdparamnew(paramno,name,value,"m") do
		case pmtype
		when pm_option then

			convlcstring(name)
			for sw to optionnames.len do
				if eqstring(name,optionnames[sw]) then
					do_option(sw,value,paramno)
					exit
				fi
			else
				println "Unknown option:",name
				stop 99
			od
		when pm_sourcefile then
			if inputfile then
				loaderror("Specify one lead module only")
			fi
			convlcstring(name)
			inputfile:=pcm_copyheapstring(name)

!CPL =PASSNAMES[PASSLEVEL]

			if passlevel in [run_pass, runpcl_pass] then
				cmdskip:=paramno-1+$CMDSKIP
!CPL "EXITG1"
				exit
			fi

		when pm_libfile then
			loaderror("Lib files go in module headers")
		else
			loaderror("Invalid params")
		esac

	od

	if prodmode=debugmode=0 then
		if not ctarget then
			passlevel:=exe_pass
			outext:="exe"
		else
			passlevel:=mcl_pass
			outext:="c"
		fi
		prodmode:=1
	fi

	case passlevel
	when obj_pass, dll_pass then
		highmem:=2
	when mcl_pass then
		if assemtype='NASM' then highmem:=2 fi
	when mx_pass, run_pass then
		highmem:=0
	esac

	if inputfile=nil then
		showcaption()
		println "Usage:"
		println "   ",cmdparams[0]," prog[.m]  Compile prog.m to prog.exe"
		println "   ",cmdparams[0]," -h           Show all options"
		stop

	else
!default output
		outfile:=pcm_copyheapstring(inputfile)

		if destfilename then
			outfile:=destfilename
		fi

		if destfilepath then
			strcpy(&.filespec,destfilepath)
			strcat(extractfile(&.filespec), outfile)
			outfile:=pcm_copyheapstring(&.filespec)	
		fi
	fi

	ext:=extractext(inputfile)
	extlen:=strlen(ext)
	strcpy(filespec, changeext(cmdparams[0],ext))
	convlcstring(filespec)
	if eqstring(filespec, inputfile) and passlevel=exe_pass then
		strcpy(&.filespec+strlen(filespec)-extlen-1, "2.m")
		outfile:=pcm_copyheapstring(filespec)
		println "New dest=",outfile
	fi

	pcl_setflags(highmem:highmem, shortnames:fshortnames)
	pcl_cmdskip(cmdskip)
	if msyslevel=2 then pfullsys:=1 fi

end

proc do_option(int sw, ichar value, int paramno=0)=
	static byte outused, outpathused

!CPL "DOOPTION", OPTIONNAMES[SW], PASSLEVEL

	if sw in ma_sw..run_sw then
		if prodmode then loaderror("dupl prod option:",OPTIONNAMES[SW]) fi
		passlevel:=optionvalues[sw]
		prodmode:=1
		outext:=passnames[sw]

		case sw
		when asm_sw then
			if assemtype<>'AA' or sw=nasm_sw and assemtype<>'NASM' then
				loaderror("Wrong WRITEASM")
			fi
		when runpcl_sw then			!in case occurs at end
			cmdskip:=paramno-1+$CMDSKIP
		esac

		return

	elsif sw in dload_sw..dss_sw then
		if debugmode then loaderror("dupl debug option") fi
		dpasslevel:=sw-dload_sw+1
		debugmode:=1
		return
	fi	

	case sw
	when ast1_sw then fshowast1:=1
	when ast2_sw then fshowast2:=1
	when ast3_sw then fshowast3:=1
	when showpcl_sw then fshowpcl:=1
	when showc_sw then fshowc:=1
	when showasm_sw then fshowasm:=1
	when st_sw then fshowst:=1
	when stflat_sw then fshowstflat:=1
	when pst_sw then fshowpst:=1
	when types_sw then fshowtypes:=1
	when showss_sw then fshowss:=1
	when showmodules_sw then fshowmodules:=1
	when clinux_sw then clinux:=1

	when sys_sw, minsys_sw, nosys_sw then msyslevel:=optionvalues[sw]

	when noopt_sw then fpeephole:=fregoptim:=0
	when nopeephole_sw then fpeephole:=0
	when noregoptim_sw then fregoptim:=0

	when time_sw then fshowtiming:=1
	when v_sw, vv_sw, quiet_sw then fverbose:=optionvalues[sw]
	when csize_sw, size_sw then pverbose:=optionvalues[sw]

	when help_sw, help2_sw then showhelp(); stop
	when ext_sw then dointlibs:=0

	when out_sw then
		if outpathused then loaderror("mixed out/path") fi
		destfilename:=pcm_copyheapstring(value)
		outused:=1

	when outpath_sw then
		if outused then loaderror("mixed out/path") fi
		if (value+strlen(value)-1)^ not in ['\\','/'] then
			loaderror("Path needs to end with \\ or /")
		fi
		destfilepath:=pcm_copyheapstring(value)
		outpathused:=1

	when unused_sw then fcheckunusedlocals:=1

	when shortnames_sw then fshortnames:=1

	when norip_sw, himem_sw then highmem:=optionvalues[sw]

	end case

end

proc showcaption=
	println "M Compiler [M8.0]", $date, $time
end

global proc showhelp=
	println strinclude(langhelpfile)
end

proc do_writeexports=
	[300]char str

	strcpy(str, extractbasefile(outfile))
	writeexports(outfile, changeext(str, "dll"))
!	stop
end

func getoutfilename(ichar file,ext)ichar=
	return pcm_copyheapstring(changeext(file,ext))
end

proc fixstartprocs=
!make sure each module has a start proc
!make sure the lead module has a main proc
	imodule ms
	isubprog ps
	symbol d
	unit p, q
	int s

	for i to nsubprogs do
		ps:=subprogs[i]
		if ps.mainmodule=0 then
			ps.mainmodule:=ps.firstmodule
		fi
	od


	for i to nmodules do
		ms:=modules[i]
		if ms.ststart then
			subproghasstart[ms.subprogno]:=1
		fi
	od

	for i to nmodules do
		ms:=modules[i]
		if ms.ststart=nil then
			s:=ms.subprogno
			if subproghasstart[s] and subprogs[s].mainmodule=i then
				ms.ststart:=addstartproc(ms.stmodule,"start", program_scope,i)
			fi
		fi

	od
end

func addstartproc(symbol owner, ichar name, int scope,moduleno)symbol stproc=
	stproc:=getduplnameptr(owner,addnamestr(name),procid)
	stproc.scope:=scope
	stproc.moduleno:=moduleno
	stproc.subprogno:=moduletosub[moduleno]
	stproc.code:=makeblock(nil)
	adddef(owner,stproc)
	addtoproclist(stproc)

	return stproc
end

PROC SHOWSURVEYS=
!CPL =NALLGENPC
!CPL =NALLGENPCHIST[0]
!CPL =NALLGENPCHIST[1]
!CPL =NALLGENPCHIST[2]
!CPL =NALLGENPCHIST[3]
!CPL =NALLGENPC1
END
=== mm_gentcl.m 0 0 9/34 ===

global int retindex
global int initstaticsindex
global pcl pcldoswx

const maxnestedloops	= 50

global [maxnestedloops,4]int loopstack
global int loopindex							!current level of nested loop/switch blocks

unitrec zero_unit
global unit pzero=&zero_unit

int nvarlocals, nvarparams

macro divider = pc_comment("------------------------")

global proc codegen_il(ichar dummy)=
	symbol d
	ref procrec pp

	return when pcldone

CPL "CODEGEN/IL"
!RETURN
	dolibs()
!CPL $LINENO

	pp:=staticlist
	while pp do
		d:=pp.def
!CPL $LINENO, D.NAME
		dostaticvar(d)
!CPL $LINENO, D.NAME
		pp:=pp.nextproc
	od

!CPL $LINENO

	for i to ndllproctable do
		gendllproc(dllproctable[i])
	od

!CPL $LINENO
	pp:=proclist
	while pp do
		d:=pp.def
!CPL "GENPCL/PROC", D.NAME
		genprocdef(currproc:=d)
!CHECKPROC("GENPCL/LIST", D.PDEF)
		pp:=pp.nextproc
	od
!CPL $LINENO

!CHECKCOMM("GENPCLX")

!	scanprocs()

	pcldone:=1
CPL "END CG"


end

proc genprocdef (symbol d) =
	imodule ms
	psymbol p, q
	symbol e

	ms:=modules[d.moduleno]
	pcldoswx:=nil

	p:=getpsymbol(d)	

	pc_addproc(p)
	pc_currfunc(p)

!CPL $LINENO

!cpl D.NAME
	e:=d.deflist
!CPL $LINENO

	while e, e:=e.nextdef do
		q:=getpsymbol(e)
!CPL =Q.NAME, Q.ATVAR, E.ATVAR
		nextloop when e.atvar

		case e.nameid
		when paramid then
!CPL "	PARAM", D.NAME, E.NAME
			pc_addparam(q)
		when frameid then
!CPL "	FRAME", D.NAME, E.NAME
			pc_addlocal(q)
		when staticid then
			pc_addstatic(q)
			if not d.atvar then
				do_idata(e)
			fi
		esac
	od

!CPL $LINENO


!	if p=ms.stmain and moduletosub[p.moduleno]=mainsubprogno then
!		genmaindef(p)
!		return
!	elsif p=ms.ststart then
!		genstartdef(p)
!		return
!	fi
	
	pcl_start()
	mmpos:=d.pos
!CPL $LINENO

	retindex:=createfwdlabel()

	divider()

	if d.hasdoswx then
!		pc_gen0(knop)
		pcldoswx:=pccurr
!		pc_gen0(knop)
	fi

!PC_GEN0(KNOP)

	evalunit(d.code)

	divider()

	definefwdlabel(retindex)

	p.code:=pcl_end()
	pc_currfunc(nil)
end

proc gendllproc(symbol p)=
	symbol e

!CPL "GENDLLPROC", P.NAME
	pc_setimport(getpsymbol(p))

	e:=p.deflist
	while e, e:=e.nextdef do
		pc_addparam(getpsymbol(e))
	od
	pc_setimport(nil)

end

proc dolibs=
	for i to nlibfiles when libfiles[i]^<>'$' do
		pc_addplib(libfiles[i])
	od
end

proc dostaticvar(symbol d)=
	psymbol p

!CPL "DOSTATIC", D.NAME
	if d.isimport then return fi
!
	if d.scope = program_scope and d.name^='$' then
		if eqstring(d.name,"$cmdskip") then
			d.scope:=export_scope				!export from mlib subprog
		fi
	fi

	p:=getpsymbol(d)
	pc_addstatic(p)

	if d.atvar=1 then
		return
	else
		do_idata(d)
	fi
end

proc do_idata(symbol d)=
	pcl pc

	return unless d.code

	pcl_start()
	mmpos:=d.pos

	genidata(d.code)

	pc:=pcl_end()
	if pc.opcode=knop then pc:=pc.next fi

	d.pdef.code:=pc
end

proc genidata(unit p,int doterm=1, am='A',offset=0)=
	[2000]byte data
	int t,tbase
	byte allbytes, nbytes
	unit q,a
	symbol d
	ref char s

!CPL "GENIDATA", P, JTAGNAMES[P.TAG]

	t:=p.mode
	mmpos:=p.pos
	tbase:=ttbasetype[t]

	case p.tag
	when jconst then
		if ttisref[t] then
			if t=trefchar then
				if p.svalue then
!CPL "GID/CONST1", P.SVALUE, p.strtype
					if p.strtype='B' then gerror("1:B-str?") fi
					pc_gen1(kdata,pc_genstring(p.svalue))
				else
					pc_gen1(kdata,pc_genint(0))
				fi
			else
				pc_gen1(kdata,pc_genint(p.value))
			fi
			setmode(ti64)
		elsif ttisreal[t] then
			if tbase=tr64 then
				pc_gen1(kdata, pc_genreal(p.xvalue))
			else
				pc_gen1(kdata, pc_genr32(p.xvalue))
			fi
			setmode(t)

		elsif ttbasetype[t]=tarray then
			IF P.STRTYPE=0 THEN GERROR("IDATA/ARRAY/NOT BLOCKDATA") FI
!CPL "GID/CONST2", P.SVALUE, p.strtype
			pc_gen1(kdata, pc_gendata(p.svalue, p.slength))
!CPL "JERE", STRPMODE(PC
!SETMODE(TI64)

		else						!assume int/word
			pc_gen1(kdata, pc_genint(p.value))
			setmode_u(p)
		fi

	when jmakelist then
		q:=p.a

		allbytes:=1
		nbytes:=0
		while q, q:=q.nextunit do
			if q.tag=jconst and q.mode=tu8 and nbytes<data.len then
				data[++nbytes]:=q.value
			else
				allbytes:=0
				exit
			end
		end

		if allbytes and nbytes then		!was all byte constants, not in data[1..nbytes]
			pc_gen1(kdata, pc_gendata(pcm_copyheapstringn(cast(&data), nbytes), nbytes))

		else
			q:=p.a
			while q, q:=q.nextunit do
				genidata(q)
			od
		fi


	when jname then
		d:=p.def
		case d.nameid
		when staticid,procid,dllprocid then
			pc_gen1(kdata, genmemaddr_d(d))
			if offset then
				pccurr.extra:=offset
			fi
			if am='P' then
				setmode(tu64)
			else
				setmode(t)
			fi
		when labelid then
			if d.index=0 then d.index:=++mlabelno fi
			pc_gen1(kdata, pc_genlabel(d.index))
			setmode(ti64)
		else
			gerror("Idata &frameXXX")
		esac
		return
	when jconvert then
		genidata(p.a)
	when jshorten then
		pc_gen1(kdata,pc_genint(p.a.value))
		setmode(t)

	when jaddrof,jaddroffirst then
		genidata(p.a,am:'P',offset:(p.b|p.b.value|0))
	else
		gerror_s("IDATA: ",jtagnames[p.tag],p)

	esac
end

global func genmem_u(unit p)pclopnd=
	pc_genmem(getpsymbol(p.def))
end

global func genmem_d(symbol d)pclopnd=
	pc_genmem(getpsymbol(d))
end

!global proc genpushmem_d(symbol d)=
!	pc_gen1(kload,pc_genmem(getpsymbol(d)))
!end

global func genmemaddr_d(symbol d)pclopnd=
	pc_genmemaddr(getpsymbol(d))
end

global func genmemaddr_u(unit p)pclopnd=
	return pc_genmemaddr(getpsymbol(p.def))
end

!global proc genpushmemaddr_d(symbol d)=
!	pc_gen(kload,pc_genmemaddr(getpsymbol(d)))
!end

global func definelabel:int =
!	pc_gen(klabel,pc_genlabel(++mlabelno))
	pc_gen1(klabel, pc_genlabel(++mlabelno))
	return mlabelno
end

global func createfwdlabel:int =
	return ++mlabelno
end

global proc definefwdlabel(int lab) =
	pc_gen1(klabel, pc_genlabel(lab))
end

!global proc pc_genreturn=
!!assume returning from currproc
!	case currproc.nretvalues
!	when 0 then
!		pc_gen(kretproc)
!	when 1 then
!		pc_gen(kretfn)
!		setmode(currproc.mode)
!
!	else
!		pc_genx(kretfn,currproc.nretvalues)
!	esac
!end

global func reversecond(int cc)int=
!reverse conditional operator
	case cc
	when eq_cc then cc:=ne_cc
	when ne_cc then cc:=eq_cc
	when lt_cc then cc:=ge_cc
	when le_cc then cc:=gt_cc
	when ge_cc then cc:=lt_cc
	when gt_cc then cc:=le_cc
	esac

	return cc
end

global func reversecond_order(int cc)int=
	case cc
	when eq_cc then cc:=eq_cc
	when ne_cc then cc:=ne_cc
	when lt_cc then cc:=gt_cc
	when le_cc then cc:=ge_cc
	when ge_cc then cc:=le_cc
	when gt_cc then cc:=lt_cc
	esac

	return cc
end

global proc stacklooplabels(int a,b,c)=
!don't check for loop depth as that has been done during parsing
	++loopindex
	if loopindex>maxnestedloops then
		gerror("Too many nested loops")
	fi

	loopstack[loopindex,1]:=a
	loopstack[loopindex,2]:=b
	loopstack[loopindex,3]:=c

end

global func findlooplabel(int k,n)int=
!k is 1,2,3 for label A,B,C
!n is a 1,2,3, according to loop nesting index
	int i

	i:=loopindex-(n-1)		!point to entry
	if i<1 or i>loopindex then gerror("Bad loop index") fi
	return loopstack[i,k]
end

global proc gensysfn(int fnindex, pclopnd a=nil,b=nil,c=nil)=
	gensysproc(fnindex, a,b,c, 1)
end

global proc gensysproc(int fnindex, pclopnd a=nil,b=nil,c=nil, int asfunc=0)=
	pclopnd fx, tx
	int nargs:=0, m
	symbol d

	if a then ++nargs fi
	if b then ++nargs fi
	if c then ++nargs fi

	extparamopnds[1]:=a	
	extparamopnds[2]:=b	
	extparamopnds[3]:=c	


	if asfunc then
		if a then
			m:=a.mode
		else
			m:=tpvoid
		fi
		tx:=extretopnds[1]:=pc_gentemp(m)
	fi

!	d:=getsysfnhandler(fnindex)

	D:=NIL
	if d then
		fx:=pc_genmemaddr(getpsymbol(d))
	else
		fx:=pc_gennameaddr(sysfnnames[fnindex]+3)
	fi

	pc_gen_call(fx, asfunc, nargs)
end

!global proc pushsysarg(unit p, int n, &nargs) =
!!return 0 or 1 args pushed
!	if p then
!		evalunit(p)
!		pc_gen(ksetarg)
!		setmode_u(p)
!		pccurr.x:=n
!		pccurr.y:=n			!ASSUMES ALL INTS; however this only important
!							!for arm64, and only matters if more than 8 args
!		++nargs
!	fi
!end
!
proc start=
	zero_unit.tag:=jconst
	zero_unit.mode:=ti64
	zero_unit.value:=0
	zero_unit.resultflag:=1
end

global func getsysfnhandler(int fn)symbol p=
	[300]char str
	int report

	if sysfnhandlers[fn] then
		return sysfnhandlers[fn]
	fi

	strcpy(str,"m$")
	strcat(str,sysfnnames[fn]+3)	!"sf_stop" => "m$stop"

	ref procrec pp:=proclist
	while pp, pp:=pp.nextproc do
		if eqstring(pp.def.name, str) then
			sysfnhandlers[fn]:=pp.def
			return pp.def
		fi
	od

!	report:=passlevel>asm_pass
	report:=1
	report:=0

	if report then
		println "Sysfn not found:",&.str
	fi
	if fn<>sf_unimpl then
		p:=getsysfnhandler(sf_unimpl)
		if p=nil and report then
			gerror("No m$unimpl")
		fi
		return p
	fi

	return nil
end

global func findhostfn(int opc)psymbol=
!called from pcl/mcl backend. opc refers to a PCL op

	case opc
	when kpower then			!assume for i64
		getpsymbol(getsysfnhandler(sf_power_i64))

	else
		nil
	esac
end

!global proc genpushint(int a)=
!	pc_gen(kload, pc_genint(a))
!	setmode(ti64)
!end
!
!global proc genpushreal(real x, int mode)=
!	pc_gen(kload,pc_genreal(x, getpclmode(mode)))
!	setmode(mode)
!end
!
!global proc genpushstring(ichar s)=
!	pc_gen(kload,pc_genstring(s))
!	setmode(tu64)
!end

!proc genmaindef(symbol p)=
!	symbol d
!
!	mmpos:=p.pos
!	doprocdef(p,1)
!
!	retindex:=createfwdlabel()
!	for i to nsubprogs when i<>mainsubprogno do
!		d:=modules[subprogs[i].mainmodule].ststart
!		docallproc(d)
!	od
!	d:=modules[subprogs[mainsubprogno].mainmodule].ststart
!	docallproc(d)
!
!	divider()
!	evalunit(p.code)
!	divider()
!
!	definefwdlabel(retindex)
!
!	pc_gen(kload, pc_genint(0))
!	setmode(ti64)
!	pc_gen(kstop)
!	pc_genreturn()
!
!	pc_endproc()
!end

!proc genstartdef(symbol p)=
!	symbol d
!	int lead:=0, m,s
!
!	m:=p.moduleno
!	s:=p.subprogno
!
!	if s=mainsubprogno and p.moduleno=subprogs[s].mainmodule then
!		LEAD:=1
!	elsif p.moduleno=subprogs[s].firstmodule then
!		LEAD:=2
!	fi
!
!	mmpos:=p.pos
!	doprocdef(p)
!
!	retindex:=createfwdlabel()
!
!	if lead then
!		for i to nmodules when moduletosub[i]=s and i<>m do
!			d:=modules[i].ststart
!			docallproc(d)
!		od
!	fi
!
!	divider()
!	evalunit(p.code)
!	divider()
!
!	definefwdlabel(retindex)
!
!	pc_genreturn()
!
!	pc_endproc()
!!	pc_comment("")
!end

proc initstaticvar(symbol d)=
	if d.code then
!*!		evalunit(d.code)
	fi
!*!	pc_gen(kstore,pc_genmem_d(d))
end

!proc docallproc(symbol d)=
!!call a simple proc, eg. start(), with no args
!	return unless d
!	pc_gen(ksetcall)
!	pc_setnargs(0)
!
!	pc_gen(kcallp, pc_genmemaddr_d(d))
!end
!
!proc scanprocs=
!	const maxprocs=1000
!	[maxprocs]psymbol proctable
!	pcl currpcl
!	int nprocs:=0
!
!	currpcl:=pcstart
!
!	repeat
!		if currpcl.opcode in [kproc,ktcproc] and currpcl.def.ishandler then
!			if nprocs>=maxprocs then gerror("PCL proctab overflow") fi
!			proctable[++nprocs]:=currpcl.def
!		fi
!		++currpcl
!	until currpcl>pccurr
!
!	if nprocs=0 and pnprocs=nil then
!		pnprocs:=pc_makesymbol("$nprocs", static_id)
!
!		pnprocs.mode:=tpi64
!!CPL "++++++++", =PNPROCS, PNPROCS.NAME
!
!		goto finish
!	fi
!
!	setfunctab()
!
!!CPL "SCANP", =PNPROCS, =PPROCADDR
!
!	pc_gen(kistatic, pc_genmem(pprocaddr))
!	pccurr.mode:=tpblock
!	pccurr.size:=nprocs*8
!	pprocaddr.mode:=tpblock
!	pprocaddr.size:=pccurr.size
!
!	for i to nprocs do
!		pc_gen(kdata, pc_genmemaddr(proctable[i]))
!		setmode(tu64)
!	od
!
!	pc_gen(kistatic, pc_genmem(pprocname))
!	pccurr.mode:=tpblock
!	pccurr.size:=nprocs*8
!	pprocname.mode:=tpblock
!	pprocname.size:=pccurr.size
!
!	for i to nprocs do
!		pc_gen(kdata, pc_genstring(getbasename(proctable[i].name)))
!		setmode(tu64)
!	od
!
!finish:
!	pc_gen(kistatic, pc_genmem(pnprocs))
!	setmode(ti64)
!	pc_gen(kdata, pc_genint(nprocs))
!	setmode(ti64)
!end

!global proc setfunctab=
!	if pnprocs=nil then
!		pnprocs:=pc_makesymbol("$nprocs", static_id)
!!CPL "SET PNPROCS", PNPROCS
!		pnprocs.mode:=tpi64
!		pprocname:=pc_makesymbol("$procname", static_id)
!		pprocaddr:=pc_makesymbol("$procaddr", static_id)
!	fi
!end

!global func gendest(pclopnd dx, unit p)pclopnd=
!	if dx then return dx fi
!	return pc_gentemp(getpclmode(p.mode), ttsize[p.mode])
!end
!
!global func gendestm(pclopnd dx, int m=tu64)pclopnd=
!	if dx then return dx fi
!	return pc_gentemp(getpclmode(m), ttsize[m])
!end
!
=== mm_libtcl.m 0 0 10/34 ===
global int nreturnvalues

global [maxtuplesize]pclopnd extretopnds		!temps to hold func results
global [maxparam]pclopnd extparamopnds

global func getpsymbol(symbol d)psymbol p=
	symbol e
	[256]char str
	[16]symbol chain
	int n

	return nil when d=nil

	if d.pdef then return d.pdef fi

	if d.atvar and d.equivvar then
		getpsymbol(e:=getequivdef(d))
		e.pdef.atvar:=1
		d.pdef:=e.pdef
		return e.pdef
	fi

	if d.nameid in [frameid, paramid] or d.isimport then
		strcpy(str, (d.truename|d.truename|d.name))
	else
		e:=d
		n:=0
		repeat
			chain[++n]:=e
			e:=e.owner
		until e=nil or e.nameid=programid

		strcpy(str,chain[n].name)
		for i:=n-1 downto 1 do
			strcat(str,".")
			if chain[i].truename then
				strcat(str,chain[i].truename)
			else
				strcat(str,chain[i].name)
			fi
		od
	fi

	d.pdef:=p:=pc_makesymbol(str, name2pid[d.nameid])

	p.mode:=getpclmode(d.mode)

	p.size:=ttsize[d.mode]

!	if d.owner and d.owner.owner then
!		p.owner:=getpsymbol(d.owner)
!	fi

	if d.scope=export_scope then p.exported:=1 fi
	if d.nameid in [dllprocid, dllvarid] then p.imported:=1 fi
	p.used:=d.used
	p.labelno:=d.index
	p.ishandler:=d.ishandler
	p.isthreaded:=d.isthreaded

	p.varparams:=d.varparams
	p.align:=getalignment(d.mode)		!mainly for vars, but no harm for procs etc

	e:=d.owner
	if ctarget and d.nameid=staticid and e and e.nameid=procid and d.code then
!CPL "GETPS/STATIC VAR", D.NAME, E.PDEF.CHASSTATICS
		p.cprocowner:=e.pdef
		e.pdef.chasstatics:=1
!		p.pcdata:=cast(123456)
	fi

	return p
end

global proc setmode(int mode)=
	pc_setmode(getpclmode(mode), ttsize[mode])
end

global proc setmode2(int mode)=
	pc_setmode2(getpclmode(mode))
end

global proc setmode_u(unit p)=
	int mode:=p.mode

	pc_setmode(getpclmode(mode), ttsize[mode])
end

func getequivdef(symbol d)symbol=
!assume that d.atvar/d.equivvar are set
	unit p

	p:=d.equivvar
	case p.tag
	when jname then
		p.def
	when jconvert then
		p.a.def			!assume points to name
	else
		gerror("geteqv")
		nil
	esac
end

global func gendest(pclopnd dx, unit p)pclopnd=
	if dx then return dx fi
	return pc_gentemp(getpclmode(p.mode), ttsize[p.mode])
end

global func gendestm(pclopnd dx, int m=tu64)pclopnd=
	if dx then return dx fi
	return pc_gentemp(getpclmode(m), ttsize[m])
end

global func makeind(pclopnd a, unit q, int m)pclopnd p=
	pc_makeind(a, q, getpclmode(m), ttsize[m])
end

global func makeindlv(pclopnd a, unit q, int m, size=0)pclopnd p=
	pc_makeindlv(a, q, getpclmode(m), ttsize[m])
end
=== mm_blocktcl.m 0 0 11/34 ===
!dummy

const freduce=0
!const freduce=1

!const kjumpt = 1		!pseudo ops used for conditional jump logic
!const kjumpf = 0

const maxnestedloops	= 50

![maxnestedloops, 4]int loopstack
!int loopindex							!current level of nested loop/switch blocks

!const maxparams=100

const maxswitchrange=500
const maxcases=maxswitchrange

const maxcasedepth=20
[maxcasedepth]unit casestmt
[maxcasedepth]int caseelse
int casedepth

ref[]int sw_labeltable			!set from do-switch
ref[]int sw_valuetable
int sw_lower
int sw_ncases					!1..n for serial switch; 0 for simple
byte sw_defaultseen				!starts at 0, set to 1 when default: seen
int sw_defaultlabel
int sw_breaklabel

int maxreg=0

!global macro getmemmode_m(p) = (p.memmode|p.memmode|p.mode)

global func evalunit(unit p, pclopnd dx=nil)pclopnd tx=
!p is a single executable unitrec; not a list or const
!should be called via execblock() to executate a code item in a unitrec
	unit a, b, c
	symbol d
	[128]char str

	if p=nil then return nil fi
	mmpos:=p.pos

	a:=p.a
	b:=p.b
	c:=p.c

	tx:=nil

!CPL "EVALUNIT", JTAGNAMES[P.TAG], =DX
!PRINTUNIT(P)

	switch p.tag
	when jconst         then tx:=do_const(p)
	when jname          then tx:=do_name(p)
	when jblock then
		tx:=do_block(p, dx)

!	when jdecimal       then do_decimal(p, a, b)
	when jcall          then tx:=do_callproc(p, a, b, dx)
	when jreturn        then
		tx:=do_return(p, a)

	when jreturnmult    then
!		tx:=do_returnmult(p, a)
		tx:=do_returnmult(p, a)

	when jassign        then tx:=do_assign(p,a,b,dx)
	when jassignmm      then do_assignmm(p,a,b)
	when jassignms      then do_assignms(p,a,b)
	when jto            then do_to(p, a, b)
	when jif            then tx:=do_if(p, a, b, c, dx, 0)
	when jforup         then do_for(p, a, b, c, 0)
	when jfordown       then do_for(p, a, b, c, 1)
	when jforall        then do_forall(p, a, b, c, 0)
	when jforallrev     then do_forall(p, a, b, c, 1)
	when jwhile         then do_while(p, a, b, c)
	when jrepeat        then do_repeat(p, a, b)
	when jgoto          then do_goto(a)
	when jlabeldef      then do_labeldef(p)
	when jredo          then do_exit(p, 1)
	when jnext          then do_exit(p, 2)
	when jexit          then do_exit(p, 3)
	when jdo            then do_do(p, a, b)
	when jcase          then tx:=do_case(p, a, b, c, dx, 0, 0)
	when jdocase        then do_case(p, a, b, c, nil, 1, 0)
	when jswitch, jdoswitch, jdoswitchu, jdoswitchx then
		do_switch(p, a, b, c, dx, 0)
	when jrecase        then do_recase(p, a)
	when jswap          then do_swap(p, a, b)
	when jselect        then tx:=do_select(p, a, b, c, dx, 0)
	when jprint, jprintln then
		do_print(p, a, b)
	when jfprint, jfprintln then
		do_print(p, a, b)
	when jread	         then tx:=do_read(p, a, dx)
	when jreadln        then do_readln(a)
!	when jsread         then do_sread(p, a, b)
!	when jsreadln       then do_sreadln(p, a, b)
	when jstop          then do_stop(p, a)
!	when jtry           then do_try(p, a, b)
!	when jexcept        then do_except(p, a, b)
!	when jyield         then do_yield(p, a, b)
!	when jraise         then do_raise(p, a, b)
	when jeval          then tx:=do_eval(a, dx)
!		tx:=gendest(dx, a)
!		pc_gen(kmove, tx, evalunit(a))
!		setmode_u(a)
!		pccurr.x:=p.index
!
!		pc_gen1(keval, tx)
!		pccurr.x:=p.index
!		setmode_u(a)
!
	when jandl          then tx:=do_andl(p, a, b, dx)
	when jorl          then tx:=do_orl(p, a, b, dx)
!
!	when jmakerange     then
PC_COMMENT("MAKERANGE")
!TX:=GENINT(9999)


	when jcmp then
		tx:=do_setcc(p, a, b, dx)
	when jcmpchain then
		tx:=do_setccchain(p, a, dx)

	when jbin then
           tx:=do_bin(p, a, b, dx)

	when jindex         then tx:=do_index(p, a, b, dx)
	when jslice         then tx:=do_slice(p, a, b, dx)
	when jdotindex      then tx:=do_dotindex(p, a, b, dx)
	when jdotslice      then tx:=do_dotslice(p, a, b, dx)
	when jdot           then tx:=do_dot(p, dx)
	when jptr           then tx:=do_ptr(p, a, b)
	when jaddrof        then tx:=evalref(a, dx)
	when jaddroffirst   then tx:=evalref(a, dx)
	when jconvert       then tx:=do_convert(p, a, dx)
	when jtypepun       then
		tx:=do_typepun(p, a, dx)

	when junary then
		tx:=do_unary(p, a, dx)

	when jnotl          then tx:=do_notl(p, a, dx)
	when jistruel       then tx:=do_istruel(p, a, dx)
	when jisfalsel       then tx:=do_isfalsel(p, a, dx)

!	when jsliceptr      then do_sliceptr(p, a)
!
	when jincr          then
		if p.pclop in [kincrto, kdecrto] then
			do_incr(p, a)
		else
			tx:=do_incrload(p, a, dx)
		fi
!
	when jbinto then
		do_binto(p, a, b)
!
	when junaryto then
		do_unaryto(p, a)
!
	when jsyscall then
		tx:=do_syscall(p, a, dx)
!
	when jshorten then
		tx:=evalunit(a, dx)

	when jclear then
		pc_gen1(kclear, evalref(a))
		setmode_u(a)

	when jcvlineno then
		tx:=pc_genint(getlineno(p.pos))

	when jsourceline then
 		pc_comment(" ")
 		pc_comment(p.a.svalue)

	else

		GERROR_S("EVALUNIT NOT IMPL:",JTAGNAMES[P.TAG])
!		fprint @str, "Unimplemented: #", jtagnames[p.tag]
!		pc_comment(str)
!		pc_comment(jtagnames[p.tag])
		return nil

!		gerror_s("UNSUPPORTED TAG: #", JTAGNAMES[P.TAG])
	end switch

!CPL "EVALU", JTAGNAMES[P.TAG], STRMODE(P.MODE),=P.RESULTFLAG,=DX,=TX
!	tx:=doresult(p, dx, tx, 0)

	if p.mode<>tvoid and not p.resultflag AND TX then
		case p.tag
		when jassign, jcall, jsyscall then
		else
			if not jsolo[p.tag] then
				printunit(p)
				gerror_s("Not allowed by itself:", jtagnames[p.tag])
			fi

!			pc_gen(kunload)
!			setmode_u(p)
	!CPL "HERE-----------"
			PC_GEN1(KEVAL, TX)
			tx:=nil
		esac
!		IF TX THEN GERROR("RES NOT USED BUT TX NOT NIL") FI

	fi

	if dx and tx and dx<>tx then
		pc_gen2(kmove, dx, tx)
		if tx.optype=memaddr_opnd then
			setmode(tu64)
		else
			setmode_u(p)
		fi
		tx:=dx
	fi

	return tx
end

func evalref(unit p, pclopnd dx=nil)pclopnd tx=
	unit a, b, c
	a:=p.a
	b:=p.b
	c:=p.c

!CPL "EVALREF", JTAGNAMES[P.TAG], =DX
	switch p.tag
	when jname then
!		pc_gen(kgetaddr, tx:=gendest(dx, p), genmem_d(p.def))
		tx:=genmemaddr_d(p.def)

	when jindex then
		tx:=do_indexref(p, a, b, dx)

	when jdot then
		tx:=do_dotref(p, dx)

	when jptr then
		tx:=evalunit(p.a, dx)

	else
		case p.tag
		when jif then
			tx:=do_if(p, a, b, c, dx, 1)
!CPL =TX, STRMODE(TX.MODE), =DX

		elsif ttisblock[p.mode] then
			tx:=evalunit(p)
		else
			PRINTUNIT(P)
			gerror("evalref")
		esac
	end switch

!	if dx and tx and dx<>tx then
!		GERROR("EVALREF: DX<>TX")
!	fi
	if dx and tx and dx<>tx then
		pc_gen2(kmove, dx, tx)
		if tx.optype=memaddr_opnd then
			setmode(tu64)
		else
			setmode_u(p)
		fi
		tx:=dx
	fi

!CPL "AFTER EVALREF"
!	tx:=doresult(p, dx, tx, 1)


	return tx

end

global func evalunitx(unit p, pclopnd dx=nil, int isref)pclopnd tx=
!call either evalunit (isref=0) or evalref(isref=1)
	if isref then
		tx:=evalref(p, dx)
	else
		tx:=evalunit(p, dx)
	fi
	return tx
end

global func evalblock(unit p, pclopnd dx=nil)pclopnd tx=
	tx:=evalunit(p, dx)
	return tx
end

func evallv(unit p, pclopnd dx=nil)pclopnd tx=
	tx:=makeindlv(evalref(p, dx), p, p.mode)
	return tx
end

func do_block(unit p, pclopnd dx)pclopnd tx=
	unit a:=p.a

	while a and a.nextunit do
		evalunit(a)
		a:=a.nextunit
	od
	if a then
		tx:=evalunit(a, dx)
		return tx
	fi
	return nil
end

proc docond(int opc, unit p, int lab)=
	genjumpcond(opc, p, lab)
end

proc genjumpcond(int opc, unit p, int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
	unit q, r, s
	int lab2, i
	pclopnd sx, qx, rx

	q:=p.a
	r:=p.b

	switch p.tag
	when jandl then
		case opc
		when kjumpf then
			genjumpcond(kjumpf, q, lab)
			genjumpcond(kjumpf, r, lab)
		when kjumpt then
			lab2:=createfwdlabel()
			genjumpcond(kjumpf, q, lab2)
			genjumpcond(kjumpt, r, lab)
			definefwdlabel(lab2)
		esac

	when jorl then
		case opc
		when kjumpf then
			lab2:=createfwdlabel()
			genjumpcond(kjumpt, q, lab2)
			genjumpcond(kjumpf, r, lab)
			definefwdlabel(lab2)
		when kjumpt then
			genjumpcond(kjumpt, q, lab)
			genjumpcond(kjumpt, r, lab)
		esac

	when jnotl, jisfalsel then
		case opc
		when kjumpf then
			genjumpcond(kjumpt, q, lab)
		when kjumpt then
			genjumpcond(kjumpf, q, lab)
		esac

	when jistruel then
		genjumpcond(opc, q, lab)

	when jblock then
		while q and q.nextunit do
			evalunit(q)
			q:=q.nextunit
		od
		genjumpcond(opc, q, lab)

	when jcmp then
		gcomparejump(opc, p.pclcond, q, r, lab)

	when jinrange then
		pc_gen4((opc=kjumpf|kjumpout|kjumpin), pc_genlabel(lab), 
				evalunit(q), evalunit(r.a), evalunit(r.b))
		setmode_u(q)

	when jinset then
		s:=r.a
		if s=nil then
			gerror("empty set")
		fi

		if opc=kjumpf then
			lab2:=createfwdlabel()
			qx:=evalunit(q)

			while s do
				sx:=evalunit(s)
				s:=s.nextunit
				if s then
					pc_sharetemp(qx)
					pc_gen3(kjumpsete, pc_genlabel(lab2), qx, sx)
				else
					pc_gen3(kjumpsetn, pc_genlabel(lab), qx, sx)
				fi
				setmode_u(q)
			od
			definefwdlabel(lab2)
		else
			qx:=evalunit(q)

			while s do
				sx:=evalunit(s)
				pc_sharetemp(qx)
				pc_gen3(kjumpsete, pc_genlabel(lab), qx, sx)
				setmode_u(q)
				s:=s.nextunit
			od
			pc_unsharetemp(qx)
			
		fi

	when jcmpchain then
		r:=q.nextunit
		qx:=evalunit(q)
		i:=1

		if opc=kjumpf then
			while r do
				rx:=evalunit(r)
				if r.nextunit then
					pc_sharetemp(rx)
				fi
				pc_gen_cond(kjumpcc, reversecond(p.cmpgenop[i]), pc_genlabel(lab), qx, rx)

				setmode_u(q)
				++i
				qx:=rx
				r:=r.nextunit
			od
		
		else
			lab2:=createfwdlabel()
			while r do
				rx:=evalunit(r)
				if r.nextunit then
					pc_sharetemp(rx)
					pc_gen_cond(kjumpcc, reversecond(p.cmpgenop[i]), pc_genlabel(lab2), qx, rx)
				else
					pc_gen_cond(kjumpcc, p.cmpgenop[i], pc_genlabel(lab), qx, rx)
				fi
				setmode_u(q)
				++i
				qx:=rx
				r:=r.nextunit
			od
			definefwdlabel(lab2)
		fi
	else			!other, single expression
			pc_gen2(opc, pc_genlabel(lab), evalunit(p))
			setmode_u(p)
	end switch
end

proc gcomparejump(int jumpopc, int cond, unit lhs, rhs, int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode

	if jumpopc=kjumpf then			!need to reverse condition
		cond:=reversecond(cond)			!eqop => neop, etc
	fi

	pc_gen_cond(kjumpcc, cond, pc_genlabel(lab), evalunit(lhs), evalunit(rhs))
	setmode_u(lhs)
end

proc genjumpl(int lab)=
!generate unconditional jump to label
	pc_gen1(kjump, pc_genlabel(lab))
end

proc unimpl(ichar mess)=
	gerror_s("Unimplemented: #", mess)
end

func do_const(unit p)pclopnd =
	int mode:=p.mode

!PRINTUNIT(P)

	if ttisinteger[mode] then
		return pc_genint(p.value)
	elsif ttisreal[mode] then
		if ttsize[mode]=4 then
			return pc_genr32(p.xvalue)
		else
			return pc_genreal(p.xvalue)
		fi

	elsif ttisref[mode] then
		if p.isastring then
			return pc_genstring(p.svalue)
		else
			return pc_genint(p.value)
		fi
	elsif mode=tbool64 then
		return pc_genint(p.value)
	else

CPL =STRMODE(P.MODE)
		gerror("do_const")
	fi
	nil
end

proc do_null(unit p, a, b) =
	unimpl("do_null")
end

func do_name(unit p)pclopnd q=
	symbol d

	d:=p.def
	q:=nil

	case d.nameid
	when procid, dllprocid then
		q:=genmemaddr_d(d)

	when labelid then
		if d.index=0 then
			d.index:=++mlabelno
		fi
		if p.resultflag then
			q:=pc_genlabel(d.index)
		else
			genjumpl(d.index)
		fi

	else
		q:=genmem_d(d)

	esac
	return q
end

proc do_stop(unit p, a) =
	pc_gen1(kstop, (a|evalunit(a)|pc_genint(0)))
end

func do_andl(unit p, a, b, pclopnd dx)pclopnd tx =
	int labfalse, labend

	if p.resultflag then
		tx:=gendest(dx, a)
	else
		tx:=pc_gentemp(ti64)
	fi

	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpf, a, labfalse)
	genjumpcond(kjumpf, b, labfalse)

	pc_gen2(kmove, tx, pc_genint(1))
	setmode(ti64)

	genjumpl(labend)

	definefwdlabel(labfalse)
	pc_gen2(kmove, tx, pc_genint(0))
	setmode(ti64)

	definefwdlabel(labend)
	return tx
end

func do_orl(unit p, a, b, pclopnd dx)pclopnd tx =
	int labtrue, labfalse, labend

	if p.resultflag then
		tx:=gendest(dx, a)
	else
		tx:=pc_gentemp(ti64)
	fi

	labtrue:=createfwdlabel()
	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpt, a, labtrue)
	genjumpcond(kjumpf, b, labfalse)

	definefwdlabel(labtrue)
	pc_gen2(kmove, tx, pc_genint(1))
	setmode(ti64)
	genjumpl(labend)

	definefwdlabel(labfalse)
	pc_gen2(kmove, tx, pc_genint(0))
	setmode(ti64)

	definefwdlabel(labend)

	return tx
end

func do_notl(unit p, a, pclopnd dx)pclopnd tx =
	pclopnd ax

	ax:=evalunit(a)
	pc_gen2(knot, tx:=gendest(dx, p), ax)
	setmode(ti64)
	return tx
end

func do_istruel(unit p, a, pclopnd dx=nil)pclopnd tx =
	pc_gen2(ktoboolt, tx:=gendestm(dx, ti64), evalunit(a))
	setmode(ti64)
	return tx
end

func do_isfalsel(unit p, a, pclopnd dx=nil)pclopnd tx =
	pc_gen2(ktoboolf, tx:=gendestm(dx, ti64), evalunit(a))
	setmode(ti64)
	return tx
end

func do_typepun(unit p, a, pclopnd dx)pclopnd tx =
	int s:=ttbasetype[p.mode]
	int t:=ttbasetype[a.mode]

	if s=t or ttisinteger[s] and ttisinteger[t] then

!	if ttsize[p.mode]=ttsize[a.mode] then
!	if p.mode=a.mode or ttbase[p.mode]=ttbase[a.mode] then
		tx:=evalunit(a, dx)
!		tx.mode:=p.convmode
!		setopndmode(tx, p.convmode)
	else
!CPL "TYPEPUN", STRMODE(P.MODE), STRMODE(A.MODE)
!CPL "TYPEPUN2", STRMODE(TTBASE[P.MODE]), STRMODE(TTBASE[A.MODE])
		pc_gen2(ktypepun, tx:=gendest(dx, p), evalunit(a))
!PRINTUNIT(P)

		setmode_u(p)
		setmode2(a.mode)
	fi
	return tx
end

global func islogical(unit p)int=			!ISLOGICAL
!return 1 if p is known to have a logical value
	case p.tag
	when jistruel, jnotl, jandl, jorl then
		return 1
	esac
	return 0
end

func do_assign(unit p, a, b, pclopnd dx)pclopnd =
!fstore=1 when result is needed
	pclopnd lhs, rhs, ax
	pcl pold
	unit c
	symbol d
	int offset

!deal with list constructs on either side
!CPL "\NASSIGN...", =DX

	if b.tag=jmakelist then
		if p.resultflag then gerror("Share x:=(a, b, c)") fi
		if ttbasetype[a.mode]=tarray then
			do_assignarray(a, b)
		else
			do_assignrecord(a, b)
		fi
		return nil
	fi

!Special handling for index/slice/dot
	rhs:=evalunit(b)
	if p.resultflag then
		pc_sharetemp(rhs)
	fi

	case a.tag
	when jindex then
		do_storeindex(p, a.a, a.b, rhs)
		finish
	when jslice then
GERROR("ASS/SLICE")
		finish
	when jdot then
		do_storedot(a, a.b, rhs)
		finish
	esac

	switch a.tag
	when jname then
		pc_gen2(kmove, genmem_u(a), rhs)

	when jptr then
		ax:=evalunit(a.a, dx)
		pold:=pccurr
		pc_gen_ix(kstorepx, ax, pc_genint(0), rhs)
		checkaddpx_store(pold, 101)

	when jdotindex then

		pc_gen3(kstorebit, evallv(a.a), evalunit(a.b), rhs)

	when jdotslice then
		pc_gen4(kstorebf, evallv(a.a), evalunit(a.b.a), evalunit(a.b.b), rhs)

	else
		cpl jtagnames[a.tag]
		gerror("Can't assign")
	end switch

	setmode_u(a)

!	setmemmode2(a, b)

finish:

	return rhs
end

!FUNCTION FF:PCLOPND P=
!	STATIC INT I=1;
!	RETURN GENINT(I++)
!END	

func do_bin(unit p, a, b, pclopnd dx)pclopnd tx =
	pclopnd ax, bx
	pcl pold

	ax:=evalunit(a)
	bx:=evalunit(b)
	tx:=gendest(dx, a)

	if p.pclop=kaddpx then
		pold:=pccurr
		pc_gen_ix(kaddpx, tx, ax, bx, ttsize[tttarget[a.mode]])
		checkaddpx(pold, 102)
	else


		pc_gen3(p.pclop, tx, ax, bx)
		case p.pclop
		when ksubpx, ksubp then
			pccurr.scale:=ttsize[tttarget[a.mode]]
!		when kadd then
!			checkadd()
		esac

	fi

	setmode_u(p)

	return tx
end

func do_setcc(unit p, a, b, pclopnd dx)pclopnd tx =
!	pc_gen_cond(ksetcc, p.condcode, tx:=gendest(dx, a), evalunit(a), evalunit(b))
	pc_gen_cond(ksetcc, p.pclcond, tx:=gendest(dx, a), evalunit(a), evalunit(b))
	setmode_u(a)
	return tx
end

func do_setccchain(unit p, q, pclopnd dx)pclopnd tx =
	int lab1, lab2, i
	unit r
	pclopnd qx, rx

	tx:=gendest(dx, p)

	lab1:=createfwdlabel()
	lab2:=createfwdlabel()

	r:=q.nextunit
	qx:=evalunit(q)
	i:=1
	while r do
		rx:=evalunit(r)
		pc_gen_cond(kjumpcc, reversecond(p.cmpgenop[i]), pc_genlabel(lab1), qx, rx)

		setmode_u(q)
		++i
		qx:=rx
		r:=r.nextunit
	od

	pc_gen2(kmove, tx, pc_genint(1))
	setmode(ti64)
	pc_gen1(kjump, pc_genlabel(lab2))
	definefwdlabel(lab1)
	pc_gen2(kmove, tx, pc_genint(0))
	setmode(ti64)
	definefwdlabel(lab2)

	return tx
end

proc do_binto(unit p, a, b)=
	pclopnd tx

	if a.tag=jname then
		pc_gen2(p.pclop, evalunit(a), evalunit(b))
	else
		pc_gen2(p.pclop, evallv(a), evalunit(b))
	fi
	setmode_u(a)

	if ttisref[a.mode] and ttisinteger[b.mode] then
		pccurr.scale:=ttsize[tttarget[a.mode]]
	fi
end

func do_unary(unit p, a, pclopnd dx)pclopnd tx =
	pc_gen2(p.pclop, tx:=gendest(dx, a), evalunit(a))
	setmode_u(p)
	return tx
end

proc do_unaryto(unit p, a)=
	pclopnd tx
	if a.tag=jname then
		pc_gen1(p.pclop, evalunit(a))
	else
		pc_gen1(p.pclop, evallv(a))
	fi
	!setmode_u(a)
end

func do_ptr(unit p, a, b)pclopnd tx=
	tx:=makeind(evalunit(a), p, tttarget[a.mode])
!	fixshort(tx)

	return tx
end

proc do_labeldef(unit p)=
	symbol d

	d:=p.def
	if d.index=0 then
		d.index:=++mlabelno
	fi
	pc_gen2(klabel, pc_genlabel(d.index), genmem_d(d))
end

proc do_goto(unit a)=
	symbol d

	case a.tag
	when jname then
		d:=a.def
		if d.index=0 then
			d.index:=++mlabelno
		fi
		pc_gen1(kjump, pc_genlabel(d.index))

	else
		gerror("goto ptr?")
	esac
end

proc do_do(unit p, a, b) =
	int lab_abc, lab_d

	lab_abc:=definelabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_abc, lab_abc, lab_d)

	evalblock(a)

	genjumpl(lab_abc)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_to(unit p, a, b) =
	unit avar
	int lab_b, lab_c, lab_d, count
	pclopnd cx

	cx:=pc_gentemp(ti64)

	a.mode:=ti64

	pc_gen2(kmove, cx, evalunit(a))
	setmode(ti64)

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	stacklooplabels(lab_b, lab_c, lab_d)

!check for count being nonzero
	if a.tag<>jconst then			!assume const limit is non-zero
		pc_sharetemp(cx)
		pc_gen_cond(kjumpcc, le_cc, pc_genlabel(lab_d), cx, pc_genint(0))
		setmode(ti64)

	else
		count:=a.value
		if count<=0 then
			genjumpl(lab_d)
		fi
	fi

	definefwdlabel(lab_b)
	evalblock(b)			!main body

	definefwdlabel(lab_c)

	pc_gen2(kto, pc_genlabel(lab_b), cx)
	setmode(ti64)

	definefwdlabel(lab_d)
	--loopindex
end

proc do_while(unit p, pcond, pbody, pincr) =
	int lab_b, lab_c, lab_d, lab_incr

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pincr then
		lab_incr:=createfwdlabel()
	else
		lab_incr:=lab_c
	fi

	stacklooplabels(lab_b, lab_c, lab_d)

	genjumpl(lab_incr)		!direct to condition code which is at the end

	definefwdlabel(lab_b)

	evalblock(pbody)

	definefwdlabel(lab_c)

	if pincr then
		evalblock(pincr)
		definefwdlabel(lab_incr)
	fi

	docond(kjumpt, pcond, lab_b)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_repeat(unit p, a, b) =
	int lab_ab, lab_c, lab_d

	lab_ab:=definelabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_ab, lab_c, lab_d)

	evalblock(a)

	definefwdlabel(lab_c)

	unless b.tag=jconst and b.value=0 then
		docond(kjumpf, b, lab_ab)
	end

	definefwdlabel(lab_d)
	--loopindex
end

proc do_exit(unit p, int k) =
	int n, index

	index:=p.index
	if index=0 then index:=loopindex fi

	n:=findlooplabel(k, index)
	if n=0 then
		gerror("Bad exit/loop index", p)
	else
		genjumpl(n)
	fi
end

func do_if(unit p, pcond, plist, pelse, pclopnd dx, int isref)pclopnd tx =
	int labend, i, lab2, ismult

	labend:=createfwdlabel()
!	ismult:=p.mode<>tvoid

	if p.resultflag then
!CPL "DO IF RES"
		tx:=gendest(dx, p)
	else
		tx:=nil
	fi

	i:=0
!	if ismult then pc_gen(kstartmult) fi

	while pcond, (pcond:=pcond.nextunit; plist:=plist.nextunit) do
		++i
		lab2:=createfwdlabel()

		docond(kjumpf, pcond, lab2)

		evalunitx(plist, tx, isref)
!		if ismult then pc_gen(kresetmult) fi

		if pcond.nextunit or pelse then
			genjumpl(labend)
		fi
		definefwdlabel(lab2)
	od

	if pelse then
		evalunitx(pelse, tx, isref)
!		if ismult then pc_gen(kendmult) fi
	fi
	definefwdlabel(labend)
	return tx
end

func do_longif(unit p, a, b, pclopnd dx, int isref)pclopnd tx =
	int labend, i, lab2, ismult
	unit pcond

	labend:=createfwdlabel()

	pcond:=a
	i:=0

	if p.resultflag then
		tx:=gendest(dx, p)
	else
		tx:=nil
	fi

	while pcond do
		++i
		lab2:=createfwdlabel()

		docond(kjumpf, pcond.a, lab2)

		evalunitx(pcond.b, tx, isref)

		if pcond.nextunit or b then
			genjumpl(labend)
		fi
		definefwdlabel(lab2)
		pcond:=pcond.nextunit
	od

	if b then
		evalunitx(b, tx, isref)
	fi
	definefwdlabel(labend)

	return tx
end

func do_return(unit p, a)pclopnd tx =
	if a then
!		evalunit(a, retax)
!		pc_gen(kretfn, retax)
!CPL "DORET"
		pc_gen1(kretfn, tx:=evalunit(a))
		setmode_u(a)
	else
		pc_gen0(kretproc)
	fi
	tx
!	genjumpl(retindex)
end

func do_returnmult(unit p, a)pclopnd tx =
	[10]pclopnd results
	clear results
!GERROR("RET MUL")

	if nreturnvalues>maxtuplesize then gerror("Too many ret values") fi

	for i to nreturnvalues do
		results[i]:=evalunit(a)
		a:=a.nextunit
	od

!really needs a dedicated gen routine.

	IF NRETURNVALUES>4 THEN GERROR("RETMULT>3") fi

	pc_gen3(kretmult, results[1], results[2], results[3])

!CPL =TX
	results[1]
!	genjumpl(retindex)
end

func do_callproc(unit p, a, b, pclopnd dx)pclopnd tx =
	[maxparam]pclopnd paramopnds
!	[maxparam]int parammodes
	[maxparam]unit paramlist
	unit c
	int nret, nparams, NUSED, ffi, i
	symbol d
	pclopnd ax

	case a.tag
	when jname then
		d:=a.def
	when jptr then
		d:=ttnamedef[a.mode]

	else
		gerror("call/not ptr")
	esac

	ffi:=d.nameid=dllprocid

	tx:=nil
	nparams:=0

!	if ttisblock[d.mode] and not p.resultflag then
!		gerror("Can't discard block ret value")
!	fi

	c:=b
	while c, c:=c.nextunit do
		if nparams>=maxparam then gerror("call:too many params") fi
		paramlist[++nparams]:=c
	od

!need to store arg operands locally, as there may be nested called
	for i:=nparams downto 1 do				!normal RTL eval order (need for temp allocs when compiling tx/mm)
!	for i:=1 to nparams do					!new LRT eval order
		paramopnds[i]:=evalunit(paramlist[i])
!		parammodes[i]:=paramlist[i].mode
	od

!copy local args to global table
	for i to nparams do
		extparamopnds[i]:=paramopnds[i]

		if ffi and i>=d.varparams and i<=4 then
			extparamopnds[i].isvariadic:=1			!whether params pushed as variadic
		fi
	od

!CPL =D.NRETVALUES, =DX, =TX

	ax:=evalref(a)
	nret:=d.nretvalues
!	if not p.resultflag and nret=1 then nret:=0 fi
	if not p.resultflag then nret:=0 fi

	case nret
	when 0 then					!proc
		pc_gen_call(ax, 0, nparams)

	when 1 then					!func
		tx:=extretopnds[1]:=gendestm(dx, p.mode)
!CPL "CALLFN/1", STROPND(EXTRETOPNDS[1]
		pc_gen_call(ax, 1, nparams)
		setmode_u(p)
!		setmemmode(p)

	else						!mult func
!ASSUME that all return values are used. Zero return values are check above.
!For 1 <= nused < nret, I need to find a way to get the information. Possibly
!it can be the value of .result flag. For now, assume NUSED=NRET
		NUSED:=NRET

		for i to nused do
			extretopnds[i]:=pc_gentemp(ttmult[d.mode, i])
		od

!CPL "CALLMULT", NUSED
		pc_gen_call(ax, nused, nparams)
	esac

!	COUNTARGS(TCCURR)
!*!	currproc.maxargs:=max(currproc.maxargs, nparams)
!*!	++currproc.ncalls

	pccurr.ffi:=ffi

	if ttisblock[p.mode] and not p.resultflag then	!???
		return nil
	else
		return tx
	fi
end

proc do_print(unit p, a, b) =
	pclopnd ax, fx
	unit q, r
	int m, fn

	if a then
		ax:=evalunit(a)

		if ttbasetype[a.mode]<>tref then gerror("@dev no ref") fi

		case ttbasetype[tttarget[a.mode]]
		when tvoid then
			gensysproc(sf_print_startfile, ax)
		when tc8 then
			gensysproc(sf_print_startstr, ax)
		when tref then
			gensysproc(sf_print_startptr, ax)
		else
			gerror("@dev?")
		esac
	else
		gensysproc(sf_print_startcon)
	fi

	q:=b

	case p.tag
	when jfprint, jfprintln then
		if ttbasetype[q.mode]<>tref or ttbasetype[tttarget[q.mode]]<>tc8 then
			gerror("string expected")
		fi
		evalunit(q)
		gensysproc(sf_print_setfmt, evalunit(q))
		q:=p.c
	esac

	while q do
		case q.tag
		when jfmtitem then
			fx:=evalunit(q.b)
			r:=q.a
			m:=r.mode
		when jnogap then
			gensysproc(sf_print_nogap)
			q:=q.nextunit
			nextloop
		when jspace then
			gensysproc(sf_print_space)
			q:=q.nextunit
			nextloop
		else
			fx:=pc_genint(0)
			r:=q
			m:=q.mode
		esac

		switch ttbasetype[m]
		when ti64 then
			fn:=sf_print_i64
		when tu64 then
			fn:=sf_print_u64
		when tr32 then
			fn:=sf_print_r32
		when tr64 then
			fn:=sf_print_r64
		when tref then
			if tttarget[m]=tc8 or tttarget[m]=tarray and tttarget[tttarget[m]]=tc8 then
				fn:=sf_print_str
			else
				fn:=sf_print_ptr
			fi
		when tarray then
			GERROR("PRINTARRAY")
			q:=q.nextunit
		when trecord then
			GERROR("PRINTRECORD")
		when tslice then
			if tttarget[m]=tc8 then
				fn:=sf_print_strsl
			else
				gerror("PRINTSLICE")
			fi

		when tc64 then
			fn:=sf_print_c8

		else
			gerror_s("PRINT/T=#", strmode(m))
		end switch

		ax:=evalunit(r)

		gensysproc(fn, ax, fx)
		q:=q.nextunit
	od

	case p.tag
	when jprintln, jfprintln then
		gensysproc(sf_print_newline)
	esac
	gensysproc(sf_print_end)

end

proc do_incr(unit p, a) =
	pclopnd tx

	if a.tag=jname then
		pc_gen1(p.pclop, genmem_u(a))
	else
		pc_gen1(p.pclop, evallv(a))
	fi

	setmode_u(a)
	setincrstep(a.mode)
end

proc setincrstep(int m)=
	pccurr.step:=1

	if ttisref[m] then
		pccurr.step:=ttsize[tttarget[m]]
	fi
end

func do_incrload(unit p, a, pclopnd dx)pclopnd =
	pclopnd tx
	int opc

	tx:=gendest(dx, a)

	if a.tag=jname then
		pc_gen2(p.pclop, tx, genmem_u(a))
	else
		pc_gen2(p.pclop, tx, evallv(a))
	fi
	setmode_u(a)
	setincrstep(a.mode)
	return tx
end

proc do_for(unit p, pindex, pfrom, pbody, int down) =
	unit pto, pstep, pelse, px, plimit
	int lab_b, lab_c, lab_d, lab_e
	int a, b, step
	pclopnd qindex, qfrom, qto

	pto:=pfrom.nextunit
!PRINTUNIT(PTO)
	pstep:=pto.nextunit
	pelse:=pbody.nextunit

	if pto.tag=jptr then
		px:=pto.a
		symbol d
		if px.tag=jname and (d:=px.def).nameid=paramid and
			 d.parammode=byref_param then
			gerror("Possibly using &param as for-loop limit")
		fi
	fi

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pelse then
		lab_e:=createfwdlabel()
	else
		lab_e:=lab_d
	fi

	stacklooplabels(lab_b, lab_c, lab_d)

!now start generating code
	qindex:=evalunit(pindex)
	qfrom:=evalunit(pfrom)
	qto:=evalunit(pto)

	pc_gen2(kmove, qindex, qfrom)
	setmode_u(pindex)

	if pfrom.tag=jconst and pto.tag=jconst then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			pc_gen1(kjump, pc_genlabel(lab_e))
		fi
	else
		pc_sharetemp(qto)
		if pfrom.tag=jconst then				!reverse condition; compare mem:imm
			pc_gen_cond(kjumpcc, (down|gt_cc|lt_cc), pc_genlabel(lab_e), qto, qfrom)
		else
			pc_gen_cond(kjumpcc, (down|lt_cc|gt_cc), pc_genlabel(lab_e), qindex, qto)
		fi
		setmode_u(pindex)
	fi

	definefwdlabel(lab_b)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	if pstep then
		if pstep.tag<>jconst then
			gerror("for/step non-const not ready")
		fi
		step:=pstep.value
		if step<=0 then
			gerror("Bad for-step")
		fi
		pc_gen3((down|kfordown|kforup), pc_genlabel(lab_b), qindex, qto)
		pccurr.index:=step
	else
		pc_gen3((down|kfordown|kforup), pc_genlabel(lab_b), qindex, qto)
		pccurr.index:=1
	fi
	setmode_u(pindex)

	if pelse then
		definefwdlabel(lab_e)
		evalblock(pelse)
	fi

	definefwdlabel(lab_d)
	--loopindex
end

proc do_forall(unit p, pindex, plist, pbody, int down) =
	unit plocal, pfrom, pto, pelse, px, plimit, passign
	int lab_b, lab_c, lab_d, lab_e
	int a, b, step
	pclopnd qindex, qlocal, qfrom, qto

	plocal:=pindex.nextunit
	pfrom:=plocal.nextunit
	pto:=pfrom.nextunit
	passign:=plist.nextunit
	pelse:=pbody.nextunit

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pelse then
		lab_e:=createfwdlabel()
	else
		lab_e:=lab_d
	fi

	stacklooplabels(lab_b, lab_c, lab_d)

!now start generating code
	qindex:=evalunit(pindex)
	qlocal:=evalunit(plocal)
	qfrom:=evalunit(pfrom)
	qto:=evalunit(pto)

	pc_gen2(kmove, qindex, qfrom)
	setmode_u(pindex)

	if pfrom.tag=jconst and pto.tag=jconst then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			pc_gen1(kjump, pc_genlabel(lab_e))
		fi
	else
		if pfrom.tag=jconst then				!reverse condition; compare mem:imm
			pc_gen_cond(kjumpcc, (down|gt_cc|lt_cc), pc_genlabel(lab_e), qto, qfrom)
		else
			pc_gen_cond(kjumpcc, (down|lt_cc|gt_cc), pc_genlabel(lab_e), qindex, qto)
		fi
		setmode_u(pindex)
	fi

	definefwdlabel(lab_b)

	evalunit(passign)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	pc_gen3((down|kfordown|kforup), pc_genlabel(lab_b), qindex, qto)
	pccurr.index:=1
	setmode_u(pindex)

	if pelse then
		definefwdlabel(lab_e)
		evalblock(pelse)
	fi

	definefwdlabel(lab_d)
	--loopindex
end

func do_convert(unit p, a, pclopnd dx)pclopnd tx =
	int opc


	case p.convcode
	when kksoftconv then
		tx:=evalunit(a)
	when kkerror then
		gerror("CONV/ERROR")
	else
		pc_gen2(convtopcl[p.convcode], tx:=gendest(dx, p), evalunit(a))
		setmode_u(p)
		setmode2(p.convmode)
!		if p.pclop=ktruncate then
!			pccurr.truncmode:=p.convmode
!		fi
	esac

	return tx
end

func do_dot(unit pdot, pclopnd dx)pclopnd tx =
	unit a
	int offset
	pcl pold
	pclopnd ax

	a:=nil
	offset:=getdotoffset(pdot, a)

	ax:=evalref(a)
	pold:=pccurr

	pc_gen_ix(kloadpx, tx:=gendest(dx, pdot), ax, nil, 1, offset)
	checkaddpx(pold, 103)

!	fixshort(tx)

	setmode_u(pdot)

	return tx
end

func do_dotref(unit pdot, pclopnd dx)pclopnd tx =
	unit a
	int offset
	pcl pold
	pclopnd ax

	a:=nil
	offset:=getdotoffset(pdot, a)

	ax:=evalref(a)
	pold:=pccurr
	pc_gen_ix(kaddpx, tx:=gendestm(dx), ax, nil, 1, offset)
	setmode(tu64)
	checkaddpx(pold, 104)

	return tx
end

proc do_storedot(unit pdot, pfield, pclopnd rhs) =
	unit a
	int offset
	pcl pold
	pclopnd ax

	a:=nil
	offset:=getdotoffset(pdot, a)
!CPL "STOREDOT"

	ax:=evalref(a)
	pold:=pccurr

	pc_gen_ix(kstorepx, ax, pc_genint(0), rhs, 1, offset)
	checkaddpx_store(pold, 105)

	setmode_u(pdot)
end

func do_index(unit p, parray, pindex, pclopnd dx)pclopnd tx =
	int addoffset, scale
	pcl pold
	pclopnd ax, bx

	addoffset:=getindexoffset(pindex)

	scale:=ttsize[tttarget[parray.mode]]

	ax:=evalarray(parray)
	bx:=evalunit(pindex)

	pold:=pccurr
	pc_gen_ix(kloadpx, tx:=gendest(dx, p), ax, bx, 
		scale, -ttlower[parray.mode]*scale + addoffset*scale)
	checkaddpx(pold, 106)

	setmode_u(p)

	return tx
end

func evalarray(unit p)pclopnd tx=
	pclopnd px
	case ttbasetype[p.mode]
	when tslice then
		px:=evalref(p)
		pc_gen2(kloadpx, tx:=pc_gentemp(tpi64), px)
		setmode(tu64)
		tx

	elsif p.mode=trefchar then
		evalunit(p)
	else
		evalref(p)
	esac
end

proc do_storeindex(unit p, parray, pindex, pclopnd rhs) =
	pclopnd px
	int addoffset, scale, emode
	pcl pold
	pclopnd ax, bx

	addoffset:=getindexoffset(pindex)

	scale:=ttsize[emode:=tttarget[parray.mode]]

	ax:=evalarray(parray)
	bx:=evalunit(pindex)
	pold:=pccurr
	pc_gen_ix(kstorepx, ax, bx, rhs, 
		scale, -ttlower[parray.mode]*scale + addoffset*scale)
	checkaddpx_store(pold, 107)
!
!CPL =STRMODE(PINDEX.MODE)
!CPL "P:"; PRINTUNIT(P)
!CPL "PARRAY:"; PRINTUNIT(PARRAY)
!CPL "PINDEX:"; PRINTUNIT(PINDEX)

	setmode(emode)
end

func do_indexref(unit p, parray, pindex, pclopnd dx)pclopnd tx =
	int addoffset, scale
	pcl pold
	pclopnd ax, bx

	addoffset:=getindexoffset(pindex)

	scale:=ttsize[tttarget[parray.mode]]

	ax:=evalarray(parray)
	bx:=evalunit(pindex)

	pold:=pccurr
	pc_gen_ix(kaddpx, tx:=gendestm(dx, tref), ax, bx,
		scale, -ttlower[parray.mode]*scale+addoffset*scale)

	checkaddpx(pold, 108)
	setmode(tu64)

	return tx
end

func getindexoffset(unit &pindex)int offset=
!convert index like [i+3] to [i], returning the +3 etc as a separate offset
	int addoffset:=0

	if pindex.tag=jbin and pindex.pclop in [kadd, ksub] then

!	case pindex.tag
!	when jadd, jsub then
		if pindex.b.tag=jconst then		!incorporate const offset into lwb adjustment
			addoffset:=(pindex.pclop=kadd|pindex.b.value|-pindex.b.value)
			pindex:=pindex.a
		fi
	fi
	return addoffset
end

func do_switch(unit p, pindex, pwhenthen, pelse, pclopnd dx, int isref)pclopnd tx =
!'looptype' is set up here:
! 0 = switch	normal switch (range-checked)
! 1 = doswitch	looping switch (range-checked)
! 2 = doswitchu	looping switch via computed goto/indexed (both non-range-checked)
! 3 = doswitchx	looping switch via computed goto/labels

	const maxlabels = 1000
	int minlab, maxlab, n, iscomplex, i
	int lab_a, lab_b, lab_d, labjump, elselab, labstmt, ax, bx, ismult

	pcl pprev, p1
	symbol djump
	byte looptype, opc

	[0..maxlabels]pcl labels
	unit w, wt, pjump

	case p.tag
	when jswitch then
		looptype:=0; opc:=kswitch
	when jdoswitch then
dodosw:
		looptype:=1; opc:=kswitch
	when jdoswitchu then
		if ctarget then dodosw fi			
		looptype:=2; opc:=kswitchu
	else
		looptype:=3
	esac

	if p.resultflag then
		tx:=gendest(dx, p)
	else
		tx:=nil
	fi

	minlab:=1000000
	maxlab:=-1000000		!highest index seen

	n:=0				!no. different values
	iscomplex:=0			!whether complex switch

	wt:=pwhenthen
	while wt do
		w:=wt.a
		while w do		!for each when expression
			case w.tag
			when jmakerange then
				ax:=w.a.value
				bx:=w.b.value
	dorange:
				for i:=ax to bx do
					minlab := min(i, minlab)
					maxlab := max(i, maxlab)
				od
			when jconst then		!assume int
				ax:=bx:=w.value
				goto dorange
			else
				gerror_s("Switch when2: not const: #", strexpr(w).strptr)
			esac
			w:=w.nextunit
		od
		wt:=wt.nextunit
	od

	n:=maxlab-minlab+1
	if n>maxlabels then
		gerror("Switch too big")
	fi

	if looptype then
		lab_a:=definelabel()
		lab_d:=createfwdlabel()
		stacklooplabels(lab_a, lab_a, lab_d)
	else
		lab_d:=createfwdlabel()
	fi

	labjump:=createfwdlabel()
	elselab:=createfwdlabel()

	if looptype=3 then		!need to initialise pointer to JT
		pjump:=pindex.nextunit
		if pjump.tag<>jname then gerror("doswx not name") fi
		djump:=pjump.def
		if ttbasetype[djump.mode]<>tref then gerror("doswx not ref") fi

		pprev:=pccurr
		pc_gen2(kmove, genmem_u(pjump), pc_genlabel(labjump))
		setmode(tu64)

		if pcldoswx=nil then
			gerror("doswx in main?")
		fi

!move just-create <move> op to just before instr following pcldosw

		p1:=pcldoswx.next
		pcldoswx.next:=pccurr
		pccurr.next:=p1
		pccurr:=pprev
		pprev.next:=nil	

	fi

	if looptype<>3 then
		doswx_dispatch(opc, pindex, labjump, elselab, minlab, maxlab)

	else
		pc_gen1(kijump, evalunit(pindex))
		setmode(tu64)
	fi

	definefwdlabel(labjump)

	for i:=minlab to maxlab do			!fill with else labels first
		pc_gen1(kswlabel, pc_genlabel(elselab))
		labels[i]:=pccurr
	od

!scan when statements again, o/p statements

	wt:=pwhenthen
	while wt do
		labstmt:=definelabel()
		w:=wt.a
		while w do
			case w.tag
			when jmakerange then
				ax:=w.a.value
				bx:=w.b.value
			when jconst then
					ax:=bx:=int(w.value)
			esac
			for i:=ax to bx do
				labels[i].a.labelno:=labstmt
			od
			w:=w.nextunit
		od

		evalunitx(wt.b, tx, isref)

!		genjumpl((loopsw|lab_a|lab_d))
		case looptype
		when 0 then
			genjumpl(lab_d)
		when 1 then
			genjumpl(lab_a)
		when 2 then
			doswx_dispatch(opc, pindex, labjump, elselab, minlab, maxlab)
		else
			pc_gen1(kijump, evalunit(pindex))
			setmode(tu64)
		esac

		wt:=wt.nextunit
	od

	definefwdlabel(elselab)
	if pelse then
		evalunitx(pelse, tx, isref)
	fi

	if looptype then
		case looptype
		when 1 then
			genjumpl(lab_a)
		when 2 then
			doswx_dispatch(opc, pindex, labjump, elselab, minlab, maxlab)
		else
			pc_gen1(kijump, evalunit(pindex))
			setmode(tu64)
		esac
		--loopindex
	fi

	definefwdlabel(lab_d)

	return tx
end

proc doswx_dispatch(int opc, unit pindex, int labjump, elselab, minlab, maxlab) =
	pc_gen3(opc, pc_genlabel(labjump), pc_genlabel(elselab), evalunit(pindex))
	setmode_u(pindex)
	pccurr.minlab:=minlab
	pccurr.maxlab:=maxlab
end

func do_select(unit p, a, b, c, pclopnd dx, int isref)pclopnd tx =
	const maxlabels=256
	[maxlabels]pcl labels
	int labend, labjump, n, i, elselab, labstmt
	unit q

	if p.resultflag then
		tx:=gendest(dx, p)
	else
		tx:=nil
	fi

	q:=b
	n:=0
	while q do
		if n>=maxlabels then gerror("selectx: too many labels") fi
		++n
		q:=q.nextunit
	od

	labend:=createfwdlabel()
	labjump:=createfwdlabel()
	elselab:=createfwdlabel()

	pc_gen3(kswitch, pc_genlabel(labjump), pc_genlabel(elselab), evalunit(a))
	setmode_u(a)

	pccurr.minlab:=1
	pccurr.maxlab:=n

	definefwdlabel(labjump)

	q:=b
	i:=0
	for i:=1 to n do
		pc_gen1(kswlabel, pc_genlabel(elselab))
		labels[i]:=pccurr
	od

	q:=b
	i:=0
	while q do
		labstmt:=definelabel()
		++i
		labels[i].a.labelno:=labstmt
		evalunitx(q, tx, isref)
		genjumpl(labend)
		q:=q.nextunit
	od

	definefwdlabel(elselab)

	evalunitx(c, tx, isref)

	definefwdlabel(labend)
	return tx
end

func do_case(unit p, pindex, pwhenthen, pelse, pclopnd dx, int loopsw, isref)pclopnd tx =
	const maxcase=256
	[maxcase]int labtable
	[maxcase]unit unittable
	int ncases, opc

	int lab_abc, lab_d, fmult, labnextwhen, labstmtstart, labelse, count
	unit w, wt
	pclopnd cx

	if p.resultflag then
		tx:=gendest(dx, p)
	else
		tx:=nil
	fi

	if pindex=nil then
		GERROR("EMPTY CASE NOT DONE")
	fi

	if loopsw then
		lab_abc:=definelabel()		!start of loop
		lab_d:=createfwdlabel()	!end of case/end of loop
		stacklooplabels(lab_abc, lab_abc, lab_d)
	else
		lab_d:=createfwdlabel()	!end of case/end of loop
	fi

	cx:=evalunit(pindex)
!	setmulttemp(cx)

	if casedepth>=maxcasedepth then
		gerror("case nested too deeply")
	fi
	casestmt[++casedepth]:=p

	ncases:=0
	count:=0
	wt:=pwhenthen

	while wt do
		w:=wt.a
		if ncases>=maxcase then
			gerror("too many cases")
		fi
		labtable[++ncases]:=createfwdlabel()
		unittable[ncases]:=wt.b

		while w, w:=w.nextunit do
			++count
			pc_sharetemp(cx)
			pc_gen_cond(kjumpcc, eq_cc, pc_genlabel(w.whenlabel:=labtable[ncases]), cx, evalunit(w))
			setmode_u(w)
		od

		wt:=wt.nextunit
	od

	pc_unsharetemp(cx)

	labelse:=createfwdlabel()
	caseelse[casedepth]:=labelse
	genjumpl(labelse)

	for i:=1 to ncases do
		definefwdlabel(labtable[i])
		evalunitx(unittable[i], tx, isref)

		if loopsw then
			genjumpl(lab_abc)
		else
			genjumpl(lab_d)
		fi
	od

	definefwdlabel(labelse)

	if pelse then
		evalunitx(pelse, tx, isref)
	fi

	if loopsw then
		genjumpl(lab_abc)
		definefwdlabel(lab_d)
		--loopindex
	else
		definefwdlabel(lab_d)
	fi

	--casedepth

	return tx
end

proc do_recase(unit p,a)=
	unit q,wt,w
	int destlab,casevalue

!CPL "DO_RECASE",CASEDEPTH

	if casedepth=0 then
		gerror("recase outside case stmt")
	fi

	if a then
		casevalue:=a.value
	else				!a=null means goto else
		genjumpl(caseelse[casedepth])
	fi

	q:=casestmt[casedepth]

	destlab:=0

	wt:=q.b
	while wt do
		w:=wt.a
		while w do
			if w.tag=jconst and ttisinteger[w.mode] and w.value=casevalue then
				destlab:=w.whenlabel
				exit all
			fi
			w:=w.nextunit
		od
		wt:=wt.nextunit
	od

	if destlab=0 then
		genjumpl(caseelse[casedepth])
	else
		genjumpl(destlab)
	fi
end

proc do_swap(unit p, a, b) =
	if a.tag=b.tag=jname then
		pc_gen2(kiswap, evalunit(a), evalunit(b))
	else
		pc_gen2(kiswap, evallv(a), evallv(b))
	fi

	setmode_u(a)
end

func do_dotindex(unit p, a, b, pclopnd dx)pclopnd tx =
	pc_gen3(kloadbit, tx:=gendest(dx, p), evalunit(a), evalunit(b))
	setmode(ti64)
	return tx
end

func do_dotslice(unit p, a, b, pclopnd dx)pclopnd tx =
	pclopnd ax, bx
	ax:=evalunit(a)
	bx:=evalunit(b.a)

!	pc_gen1(ksetopndd, evalunit(b.b))
	!setmode(ti64)
	pc_gen4(kloadbf, tx:=gendest(dx, p), ax, bx, evalunit(b.b))
	setmode(ti64)
	return tx
end

func do_eval(unit a, pclopnd dx)pclopnd tx =
	tx:=gendest(dx, a)
	pc_gen2(kmove, tx, evalunit(a))
	setmode_u(a)

	pc_gen1(keval, tx)
	setmode_u(a)
	tx
end

func do_read(unit p, a, pclopnd dx)pclopnd tx =
	pclopnd fx
	int m

	if a then			!format
		fx:=evalunit(a)
	else
		fx:=pc_genint(0)
	fi

	m:=p.mode

	if ttisinteger[m] then
		gensysfn(sf_read_i64, tx:=gendest(dx, p), fx)
	elsif ttisreal[m] and ttsize[m]=8 then
		gensysfn(sf_read_r64, tx:=gendest(dx, p), fx)
	elsif m=trefchar then
		gensysfn(sf_read_str, tx:=gendest(dx, p), fx)
	else
		GERROR("CAN'T READ THIS ITEM")
	fi
	!setmode_u(p)

	return tx
end

proc do_readln(unit a) =
	if a then
		if ttbasetype[a.mode]<>tref then gerror("@dev no ref") fi

		case ttbasetype[tttarget[a.mode]]
		when tvoid then
			gensysproc(sf_read_fileline, evalunit(a))
		when tu8, tc8 then
			gensysproc(sf_read_strline, evalunit(a))
		else
			gerror("rd@dev?")
		esac
	else
		gensysproc(sf_read_conline)
	fi
end

func do_slice(unit p, pslice, prange, pclopnd dx)pclopnd tx =
GERROR("SLICE")
!	int addoffset
!
!	addoffset:=getindexoffset(prange.a)
!
!!	pc_gen1(ksetopndd, evalunit(prange.b))
!	pc_gen4(kgetslice, tx:=gendest(dx, p), evalref(pslice), evalunit(prange.a), 
!		evalunit(prange.b))
!
!	!setmode(getmemmode(p))
!	pccurr.scale:=ttsize[tttarget[pslice.mode]]
!
!	pccurr.extra:=-ttlower[pslice.mode]*pccurr.scale + addoffset*pccurr.scale
	return tx
end

func do_syscall(unit p, a, pclopnd dx)pclopnd tx=
	ichar name

	tx:=gendest(dx, p)

	case p.fnindex
	when sf_getnprocs then
		pc_gen2(kmove, tx, pc_genname("$nprocs"))

	when sf_getprocname then
		name:="$procname"
doprocname:
		pc_gen_ix(kloadpx, tx, pc_genname(name), evalunit(a), 8, -8)

	when sf_getprocaddr then
		name:="$procaddr"
		doprocname
	else
		PC_COMMENT("SYSCALL")
	esac

	setmode_u(p)

	return tx
end

func getdotoffset(unit p, &pname)int=
!return accumulated offset of this and nested dot-expressions, 
!also returns the terminal dot unit to be evaluated
	int offset, axmode

	case p.tag
	when jdot then
		offset:=getdotoffset(p.a, pname)
		return p.offset+offset

	else							!anything else, is the start expression
		pname:=p
		return p.offset
	esac
end

proc moveopnd(pcl p, q, int a, b)=
	if q.abc[b]=nil then
		p.abc[a]:=pc_genint(0)
		p.isfirst.[a]:=0
		p.islast.[a]:=0
		return
	fi

	p.abc[a]:=q.abc[b]

!	p.isfirst.[a]:=q.isfirst.[b]

	if q.isfirst.[b] then
		p.isfirst.[a]:=1
	else
		p.isfirst.[a]:=0
	fi

	if q.islast.[b] then
		p.islast.[a]:=1
	else
		p.islast.[a]:=0
	fi
	q.isfirst.[b]:=0
	q.islast.[b]:=0
	dummy()
end

global proc dummy()=
end

global proc checkaddpx(pcl p, int id=0)=
!addpx/loadpx has just been generated; see if it can be combined with previous addpx
!assume it looks like this:
! p  T1 := bp  + cp*sp + extrap        (p)
! q  T2 := T1 +  cq*sq + extraq        (q:=pccurr)
!For this to work, p must be addpx, q is addpx/loadpx, and cq must be int-opnd or
! be missing; T1 must only be used in these 2 ops

	pcl q:=pccurr

	return unless freduce

	return unless p.opcode=kaddpx
	return unless q.c=nil or q.c.optype=int_opnd
	return unless p.a=q.b
	return unless p.isfirst.[1]=q.islast.[2]=1

	p.a:=q.a							!move T2 over to P
	p.isfirst.[1]:=q.isfirst.[1]		!copy T2 isfirst flag
	if q.c then							!I think that .c is optional
		p.extra +:= q.c.value*q.scale
	fi
	p.extra +:= q.extra
	p.opcode := q.opcode				!move opcode in case loadpx

	pccurr:=p							!discard new pcl op
end

global proc checkaddpx_store(pcl p, int id=0)=
!storepx has just been generated; see if it can be combined with previous addpx
!assume it looks like this:
! p  T1 := bp  + cp*sp + extrap        (pccurr-1)
! q  (T1 +  bq*sq + extraq)^ := c       (tcurr)

!For this to work, p must be addpx, q is storepx, and bq must be int-opnd or
! be missing; T1 must only be used in these 2 ops

	return unless freduce

	pcl q:=pccurr

	return unless p.opcode=kaddpx
	return unless q.b=nil or q.b.optype=int_opnd
	return unless p.a=q.a
	return unless p.isfirst.[1]=q.islast.[1]=1

	if q.b then							!I think that .c is optional
		p.extra +:= q.b.value*q.scale
	fi
	p.extra +:= q.extra
	p.opcode := kstorepx				!addpx becomes storepx

	moveopnd(p, p, 1, 2)
	moveopnd(p, p, 2, 3)
	moveopnd(p, q, 3, 3)

	--pccurr							!discard new pcl op
end

proc do_assignmm(unit p, a, b) =
!fstore=1 when result is needed
	[50]pclopnd temps
	int n
	pclopnd ax, bx
	unit q

	q:=b.a
	n:=0

	while q, q:=q.nextunit do
		++n
		if n>temps.upb then gerror("assignmm?") fi

		bx:=evalunit(q)
		if bx.optype<>mem_opnd then
			ax:=bx
		else						!need to copy value to allow swap/rotate
			ax:=gendest(nil, q)
			pc_gen2(kmove, ax, bx)
			setmode_u(q)
		fi
		temps[n]:=ax

	od

	q:=a.a
	n:=0
	while q, q:=q.nextunit do
		++n
		pc_gen2(kmove, evallv(q), temps[n])
		setmode_u(q)
	od

end

proc do_assignms(unit p, a, b) =
!fstore=1 when result is needed
!	[50]pclopnd temps
	int nlhs, nret
	pclopnd ax, bx
	unit q
	pcl pcall

	bx:=evalunit(b)
	pcall:=pccurr

	if pcall.opcode<>kcall then
		gerror("assignms not call")
	fi
	nret:=pcall.nret
	nlhs:=a.length

	if nlhs<>nret then gerror("ass/ms N?") fi

	q:=a.a
	for i to nret do
		pc_gen2(kmove, evallv(q), pcall.abc[i+1])
		q:=q.nextunit
	od

end

proc do_assignarray(unit a, b)=
!a is an array type; b is makelist
	unit q
	int scale
	pcl pold
	pclopnd ax, bx

!	if ttbasetype[tttarget[a.mode]]=tc8 then
!		gerror("Assignment not suitable for []char type")
!	fi

	q:=b.a
	for i to b.length do
		scale:=ttsize[tttarget[a.mode]]

		ax:=evalarray(a)
		bx:=evalunit(q)

		pold:=pccurr
		pc_gen_ix(kstorepx, ax, pc_genint(i), bx,
			scale, -ttlower[a.mode]*scale)
		checkaddpx_store(pold, 110)
		setmode(tttarget[a.mode])

		q:=q.nextunit
	od
end

proc do_assignrecord(unit a, b)=
!a is a record, b is makelist

	unit q
	int m, fieldtype
	symbol d, e
	pcl pold
	pclopnd ax, bx

PC_COMMENT("ASSIGNRECORD")

!
!	pfield:=createunit0(jname)
!	pdot:=createunit2(jdot, a, pfield)
!	passign:=createunit2(jassign, pdot, b.a)
!	passign.mode:=pdot.mode:=tttarget[a.mode]
!
	m:=a.mode
	d:=ttnamedef[m]

	e:=d.deflist
	q:=b.a
	while e, e:=e.nextdef do
		if e.nameid=fieldid and e.mode<>tbitfield then
			ax:=evallv(a)
			bx:=evalunit(q)
			pold:=pccurr
			pc_gen_ix(kstorepx, ax, pc_genint(0), bx, 1, e.offset)
			checkaddpx_store(pold, 111)
			setmode(e.mode)

			q:=q.nextunit
		fi
	od
end

=== dummy.m 0 0 12/34 ===
global int pstartclock
global int mcltime

global proc pcl_genss= end
global proc pcl_runpcl= end
global proc pcl_writeobj(ichar filename)= end
global proc pcl_writeexe(ichar filename)= end
global proc pcl_writedll(ichar filename)= end
global proc pcl_writemx(ichar filename)= end
global proc pcl_writemcl= end
global proc pcl_exec= end
export proc pcl_setflags(int highmem=-1, verbose=-1, shortnames=-1) = end
export proc pcl_cmdskip(int a)=end


global ref func (int pos, ichar &filename, &sourceline)int igetmsourceinfo
global ref proc (ref void) idomcl_assem
global ref func (ref void)int icheckasmlabel
global ref func (int)psymbol igethostfn

global byte fregoptim
global byte fpeephole
global byte pc_userunpcl=0
!global byte pfullsys
export byte pverbose

export int assemtype='AA'

export int mmpos

=== tc_api.m 0 0 13/34 ===
int STSEQNO

export pcl pcstart			!start of pcl block
export pcl pccurr			!point to current pcl op
export pcl pcend			!point to last allocated pclrec
global int pcalloc			!number of pclrecs allocated
byte pcfixed				!whether code is fixed up
global int pcseqno
int pcneedfntable			!whether kgetnprocs etc are used

const pcelemsize = pclrec.bytes
global const pclbasesize = pclrec.bytes - 3*pclopnd.bytes	!base size excludes a,b,c fields

export int mlabelno
export byte phighmem
export byte pfullsys
global byte fpshortnames

global const maxtemp=32
global [maxtemp]int tempsizes			!0..max size (for blocks)
global [maxtemp]byte tempused			!1 means in use
global [maxtemp]u16 tempcount			!how many reads are left

global int currtempno, ntemps

const maxsmallint=64
[0..maxsmallint]pclopnd smallintoperands

global ichar longstring					!used in stropnd
global int longstringlen

!---------------------------------------------------

proc start=
	for i:=0 to maxsmallint do
		smallintoperands[i]:=pc_genint0(i)
	od
end

export proc pcl_start =
!reset pcstart/pccurr for new PCL sequence (new proc or new init data)
	pcstart:=pcm_alloc(pclrec.bytes)
	pccurr:=pcstart
end

export func pcl_end:pcl pc=
!Terminate sequence; sets pcstart to nil so that pcl cannot be generated
!outside proc bodies etc
!But caller should copy pcstart value, or use its value returned here

	pc:=pcstart
!	pc_gen(knop)
!	pcstart:=pccurr:=nil
	pc
end

export func pcl_writepcl(ichar filename=nil)ichar=
	ref strbuffer d

!PCLERROR("WRITEPCL")
!""
!
	d:=writeallpcl("caption")

	if filename then
		if pverbose then println "Writing PCL",filename fi
		writefile(filename, d.strptr, d.length)
		""
	else
		d.strptr
	fi
end

export func pcl_writepst(ichar filename=nil)ichar=
	ref strbuffer d
!PCLERROR("WRITEPST")
!""


	d:=writepst()

	if filename then
		if pverbose then println "Writing PST",filename fi
		writefile(filename, d.strptr, d.length)
		""
	else
		d.strptr
	fi
end

global proc perror(ichar mess)=
	perror_s(mess, nil)
end

global proc perror_s(ichar mess, param=nil)=
	print "PCL error:",mess
	if param then
		print ":",param
	fi

	stop 1
end

global proc pclerror(ichar mess)=
	println "PCL Error:", mess
	println
	stop 1
end

export func pc_makesymbol(ichar s, int id)psymbol d=
!Create a new st entry
!local/param/null-id names are not linked to psymbol table
!all others become part of main ST
!Only local/param have .owner set to currfunc

	d:=pcm_allocnfz(pstrec.bytes)
	d.name:=pcm_copyheapstring(s)
	d.seqno:=++stseqno

	case id
	when import_id then
		d.imported:=1
	when export_id then
		d.exported:=1
		id:=proc_id
	esac

	d.id:=id
!
!	if id in [local_id, param_id] then
!!*!		d.owner:=currfunc
!	elsif id then
!		pc_addsymbol(d)
!	fi
!
	d
end

export proc pc_addproc(psymbol d)=
	if pproctable=nil then
		pproctable:=pproctablex:=d
	else
		pproctablex.next:=d
		pproctablex:=d
	fi
end

export proc pc_addparam(psymbol d)=
	psymbol p

	pclerror("No proc") unless currfunc

	p:=currfunc.nextparam

	if p=nil then
		currfunc.nextparam:=d
	else
		while p.nextparam do p:=p.nextparam od		!look for last
		p.nextparam:=d
	fi
	++currfunc.nparams
end

export proc pc_addlocal(psymbol d)=
	psymbol p

	pclerror("No proc") unless currfunc

	p:=currfunc.nextlocal

	if p=nil then
		currfunc.nextlocal:=d
	else
		while p.nextlocal do p:=p.nextlocal od		!look for last
		p.nextlocal:=d
	fi
	++currfunc.nlocals
end

export proc pc_addstatic(psymbol d)=
!add to global static if outside a function, or to current function

	psymbol p

	if currfunc=nil then
		if pstatictable=nil then
			pstatictable:=pstatictablex:=d
		else
			pstatictablex.next:=d
			pstatictablex:=d
		fi
	else

		p:=currfunc.nextstatic

		if p=nil then
			currfunc.nextstatic:=d
		else
			while p.nextstatic do p:=p.nextstatic od		!look for last
			p.nextstatic:=d
		fi
!		++currfunc.nstatics
	fi
end

global func newpcl(int opcode, nopnds)pcl p=

	p:=pcm_allocnfz(nopnds*pclopnd.bytes+pclbasesize)

!CPL "NEWPCL", =NOPNDS, =PCLOPND.BYTES, =PCLBASESIZE

	pccurr.next:=p
	pccurr:=p

	pccurr.opcode:=opcode
	pccurr.nopnds:=nopnds
	pccurr.pos:=mmpos
	pccurr.seqno:=++pcseqno

!CPL "NEWPCL", PCLNAMES[OPCODE],=PCCURR, PCSEQNO
	return pccurr
end

global func newopnd:pclopnd=
	pcm_allocnfz(opndrec.bytes)
end

!global proc pc_gen(int opcode, pclopnd a=nil, b=nil, c=nil)=
!	pcl p
!!	[3]pclopnd abc
!	int n
!
!	if pcstart=nil then GERROR("PCSTART NOT SET") FI
!
!	if c then n:=3
!	elsif b then n:=2
!	elsif a then n:=1
!	else n:=0
!	fi
!
!!IF N=1 THEN
!!CPL PCLNAMES[OPCODE]
!!FI
!
!++NALLGENPC
!++NALLGENPCHIST[N]
!
!	p:=newpcl(opcode,n)
!
!	p.abc[1]:=a
!	p.abc[2]:=b
!	p.abc[3]:=c
!
!	for i to n do
!		addtemp(p, p.abc[i], i)
!	od
!end

global proc pc_gen0(int opcode)=
	newpcl(opcode, 0)
end

global proc pc_gen1(int opcode, pclopnd a)=
	pcl p

	p:=newpcl(opcode, 1)

	p.a:=a

	if a.optype in [temp_opnd, tempptr_opnd] then
		addtemp(p, a, 1)
	fi
end

global proc pc_gen2(int opcode, pclopnd a, b)=
	pcl p

	p:=newpcl(opcode, 2)

	p.a:=a
	p.b:=b

	addtemp(p, a, 1)
	addtemp(p, b, 2)
end

global proc pc_gen3(int opcode, pclopnd a, b, c)=
	pcl p

	p:=newpcl(opcode, 3)

	p.a:=a
	p.b:=b
	p.c:=c

	addtemp(p, a, 1)
	addtemp(p, b, 2)
	addtemp(p, c, 3)
end

!global proc pc_gen1(int opcode, pclopnd a)=
!!can be called for one-operand when know it is not a temp
!	pcl p
!
!!IF A.OPTYPE IN [TEMP_OPND, TEMPPTR_OPND] THEN
!!CPL =PCLNAMES[OPCODE]
!!GERROR("GEN1/TEMP")
!!FI
!
!	p:=newpcl(opcode, 1)
!
!	p.a:=a
!end

global proc pc_gen4(int opcode, pclopnd a,b,c,d)=
!global proc pc_gen(int opcode, pclopnd a=nil, b=nil, c=nil, d=nil, E=NIL, F=NIL)=
	pcl p

	p:=newpcl(opcode,4)

	p.a:=a
	p.b:=b
	p.c:=c
	p.abc[4]:=d

	for i to 4 do
		addtemp(p, p.abc[i], i)
	od
end

global proc pc_gen_ix(int opcode, pclopnd a, b, c, int scale=1, offset=0) =

!	IF A=NIL THEN A:=GENINT(0) FI
	IF B=NIL THEN B:=PC_GENINT(0) FI
	IF C=NIL THEN C:=PC_GENINT(0) FI
!INT OLDOP:=PCCURR.OPCODE
	pc_gen3(opcode, a,b,c)

!CPL "GENPC/IX", PCLNAMES[OLDOP], PCLNAMES[OPCODE]

	pccurr.scale:=scale
	pccurr.extra:=offset
end

global proc pc_gen_call(pclopnd fn, int nret, nargs)=
!nret is number of ret opnds in extretopnds
!nargs is the number of args opnds in extparamopnds
	pcl p
	pclopnd x
	int argoffset

	p:=newpcl(kcall, nret+nargs+1)

	p.abc[1]:=fn
	p.nret:=nret
	p.nargs:=nargs
	p.argoffset:=argoffset:=nret+1

	addtemp(p, fn, 1)

	for i to nret do
		p.abc[i+1]:=x:=extretopnds[i]			!offset past .a
		addtemp_ret(p, x, i+1)
	od

	for i to nargs do
		p.abc[i+argoffset]:=x:=extparamopnds[i]
		addtemp(p, x, i+argoffset)
	od

end

global proc pc_gen_cond(int opcode, cond, pclopnd a, b, c)=
	pc_gen3(opcode,a,b,c)
	pccurr.cond:=cond
end

global func pc_genint(int a)pclopnd p=
	if a in 0..maxsmallint then
		return smallintoperands[a]
	fi
	p:=newopnd()
	p.value:=a
	p.optype:=int_opnd
	p.mode:=tpi64
	p.size:=8
	return p
end

global func pc_genint0(int a)pclopnd p=
	p:=newopnd()
	p.value:=a
	p.optype:=int_opnd
	p.mode:=tpi64
	p.size:=8
	return p
end

global func pc_genreal(real x)pclopnd p=
	p:=newopnd()
	p.xvalue:=x
	p.optype:=real_opnd
	p.mode:=tpr64
	p.size:=8
	return p
end

global func pc_genr32(real x)pclopnd p=
	p:=newopnd()
	p.xvalue32:=x
	p.optype:=r32_opnd
	p.mode:=tpr32
	p.size:=4
	return p
end

global func pc_genstring(ichar s)pclopnd p=
	p:=newopnd()
	p.svalue:=pcm_copyheapstring(s)
	p.svalue:=s
	p.optype:=string_opnd
	p.mode:=tpu64
	p.size:=8
	return p
end

global function pc_gendata(ref byte s, int length)pclopnd p=
	p:=newopnd()
	p.svalue:=s			! assume already saved on heap
	p.optype:=data_opnd
	p.mode:=tpblock
	p.size:=length
	return p
end

global func pc_genlabel(int labelno)pclopnd p=
	p:=newopnd()
	p.labelno:=labelno
	p.optype:=label_opnd
	return p
end

global func pc_genmem(psymbol d)pclopnd p=
	p:=newopnd()
	p.def:=d
	p.optype:=mem_opnd
	p.mode:=d.mode
	p.size:=d.size
	return p
end

global func pc_genmemaddr(psymbol d)pclopnd p=
	p:=newopnd()
	p.def:=d
	p.optype:=memaddr_opnd
	p.mode:=d.mode
	p.size:=d.size
	return p
end

global func pc_genname(ichar s)pclopnd p=
	return pc_genmem(pc_makesymbol(s, misc_id))
end

global func pc_gennameaddr(ichar s)pclopnd p=
	return pc_genmemaddr(pc_makesymbol(s, misc_id))
end

proc addtemp(pcl p, pclopnd a, int n=0)=
!n=1,2,3 for n'th pclopnd in a normal pcl op
!n=0 for use from kcall, where the params are not used conventionally
	int temp

!	if a=nil then return fi

	if a.optype not in [temp_opnd, tempptr_opnd] then
		return
	fi

	temp:=a.tempno

	if n=1 and a.optype=temp_opnd and pclwrite[p.opcode] then
		if tempcount[temp]=0 then
			p.isfirst.[n]:=1
		fi
		tempcount[temp]:=1

	else

		--tempcount[temp]
		if tempcount[temp]=0 then
			tempused[temp]:=0
			if n then
				p.islast.[n]:=1
			fi
		elsif tempcount[temp]<0 then
		fi

	fi
end

global func getfullname(psymbol d, int backtick=0)ichar=
!create fully qualified name into caller's dest buffer
	static [256]char str
	int n:=0
	psymbol e:=d

!	if fpshortnames then return d.name fi

	str[1]:=0
	if backtick then
		strcpy(str, "`")
	fi

	if d.imported then
		if backtick then
			strcat(str, d.name)
			strcat(str, "*")
		else
			strcat(str, d.name)
		fi
		return str
	fi

	if d.id in [local_id, param_id] then
!		strcat(str, d.owner.name)
!		strcat(str, ".")
		strcat(str, d.name)
		return str
	fi

	if backtick then
		strcat(str, d.name)
	else
		return d.name
	fi
end

export func strpmode(int mode, size=0)ichar=
	static [32]char str

	strcpy(str, "")

	case mode
	when tpblock then
		strcpy(str, "mem:")
		strcat(str, strint(size))
		str
	when tpvoid then
		"---"
	else
		pstdnames[mode]
	esac
end

export proc pc_setmode(int m, size=0)=
	pccurr.mode:=m

	if size then
		pccurr.size:=size
	else
		pccurr.size:=psize[pccurr.mode]
	fi

	if pclhastype[pccurr.opcode]=2 then
		pccurr.mode2:=pccurr.mode
	fi
end

export proc pc_setmode2(int m)=
	pccurr.mode2:=m
end

export proc pc_setimport(psymbol d)=
!allow the use of pc_addlocal
!use d=nil when done

	currfunc:=d
end

export proc pc_comment(ichar s)=
	return when fregoptim or fpeephole		!will get skipped anyway
	pc_gen1(kcomment, pc_genstring(s))
end

global proc pc_currfunc(psymbol d)=
	currfunc:=d
end

export proc pc_addplib(ichar name)=
	if nplibfiles>=maxplibfile then perror("Too many libs") fi

!CPL "ADDPLIB",NAME

!	plibfiles[++nplibfiles]:=pcm_copyheapstring(name)
	plibfiles[++nplibfiles]:=pcm_copyheapstring(changeext(name,""))
end

global func pc_makeind(pclopnd a, unit q, int m, size=8)pclopnd p=
	pcl pold

	p:=newopnd()
	if a=nil then gerror("MAKEIND A=0") fi
	p^:=a^
!PC_COMMENT(ADDSTR("MAKEIND1:", OPNDNAMES[P.OPTYPE]))
	case p.optype

	when memaddr_opnd then
		p.optype:=mem_opnd

	when temp_opnd, mem_opnd then

!	else							!everything else needs explicit loadptr to a new temp
!									!note will not detect invalid ops like floats or strings

!IF P.OPTYPE=TEMPPTR_OPND THEN
!	CPL "MAKEIND/TEMPPTR"
!FI
		pold:=pccurr
		pc_gen_ix(kloadpx, p:=pc_gentemp(tu64),a, pc_genint(0))
		checkaddpx(pold)
		setmode(m)

	else
		gerror("makeind?")
	esac

	p.mode:=m
	p.size:=size

!	setopndmode(p,m)
!PC_COMMENT(ADDSTR("MAKEIND2:", OPNDNAMES[P.OPTYPE]))

	return p
end

global func pc_makeindlv(pclopnd a, unit q, int m, size=0)pclopnd p=
	p:=newopnd()
	if a=nil then gerror("MAKEIND A=0") fi

	p^:=a^

	case p.optype
	when memaddr_opnd then
		p.optype:=mem_opnd
!	when mem_opnd then
!		p.optype:=memptr_opnd

	when temp_opnd then
		p.optype:=tempptr_opnd

!	when tempptr_opnd, memptr_opnd then
!	when memptr_opnd then
	when mem_opnd then

		pc_gen2(kmove,p:=pc_gentemp(tu64),a)
		setmode(tu64)
		p:=pc_makeindlv(p,q, m)

		return p

	else
		gerror("makeindlv?")

	esac
	p.mode:=m
	p.size:=size

	return p
end

global proc pc_sharetemp(pclopnd tx)=
	if tx and tx.optype in [temp_opnd,tempptr_opnd] then
!CPL "SHARE", TX.TEMPNO
		++tempcount[tx.tempno]
	fi
end

global proc pc_unsharetemp(pclopnd tx)=
	if tx and tx.optype in [temp_opnd,tempptr_opnd] then
!CPL "UNSHARE", TX.TEMPNO
		--tempcount[tx.tempno]
		if tempcount[tx.tempno]=0 then
!PC_COMMENT(addstr("Temp unshared to zero: T",strint(tx.tempno)))
			tempused[tx.tempno]:=0
		fi
	fi
end

global proc pc_setmulttemp(pclopnd tx)=
	if tx and tx.optype in [temp_opnd,tempptr_opnd] then
		tempcount[tx.tempno]:=9999
	fi
end

global proc pc_freemulttemp(pclopnd tx)=
	if tx and tx.optype in [temp_opnd,tempptr_opnd] then
		tempcount[tx.tempno]:=0
	fi
end

!global func gendest(pclopnd dx, unit p)pclopnd=
!	if dx then return dx fi
!	return pc_gentemp(p.mode)
!end
!
!global func gendestm(pclopnd dx, int m=tu64)pclopnd=
!	if dx then return dx fi
!	return pc_gentemp(m)
!end

global func pc_gentemp(int m, size=8)pclopnd p=
	int n
	p:=newopnd()

!CPL "GENTEMP", STRMODE(M)

	for i to ntemps do
		if not tempused[i] then
			n:=i
			exit
		fi
	else
		if currtempno>=maxtemp then
			gerror("Too many temps")
		fi
		n:=++currtempno
	
		if n>ntemps then
			ntemps:=n
			tempsizes[n]:=0
		fi
	od

	p.tempno:=n
	p.optype:=temp_opnd
	p.mode:=m
	p.size:=size

!	COUNTTEMP(N, P.MODE)
!	currproc.ntemps:=max(currproc.ntemps, ntemps)

IF P.MODE=TBLOCK THEN
	tempsizes[n] max:=size
FI

	tempused[n]:=1
	tempcount[n]:=0

	return p
end

global proc pc_freetemp(int temp)=
	tempused[temp]:=0
end

global proc pc_addblocktemp(int temp, size)=
	tempsizes[temp]:=size
end

global proc addtemp_ret(pcl p, pclopnd a, int n)=
!n=2..nret-1 for calls
	int temp

	if a.optype<>temp_opnd then
		return
	fi

	temp:=a.tempno

	if tempcount[temp]=0 then
		p.isfirst.[n]:=1
	fi
	tempcount[temp]:=1
	dummy()
end

export func convertstring(ichar s, t)int=
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
!returns length of t
	int c
	ichar t0:=t
	[16]char str

	while c:=s++^ do
		case c
		when '"' then
			t++^:='\\'
			t++^:='"'
		when 10 then
			t++^:='\\'
			t++^:='n'
		when 13 then
			t++^:='\\'
			t++^:='r'
		when 9 then
			t++^:='\\'
			t++^:='t'
		when '\\' then
			t++^:='\\'
			t++^:='\\'
		when 7,8,26,27 then
			t++^:='<'
			t++^:=c/10+'0'
			t++^:=(c rem 10)+'0'
			t++^:='>'
		elsif c in 32..126 then
			t++^:=c
		else
			t++^:='\\'
			t++^:='x'
			print @str,c:"z2h"
			t++^:=str[1]
			t++^:=str[2]
		esac
	od
	t^:=0

	return t-t0
end

EXPORT proc merror(ichar mess,ichar param="")=
	int lineno
	ichar filename, sourceline

	if igetmsourceinfo then
		lineno:=igetmsourceinfo(mmpos, filename, sourceline)
		CPL =LINENO
		CPL =FILENAME
	else
		lineno:=0
		filename:="?"
	fi

	if currfunc then
		println "Proc:", currfunc.name
	fi

	fprintln "MCL Error: # (#) on Line: # in #, PCL:#",mess,param, lineno, filename, pcseqno

	pcerrorstop(filename, lineno)
end

global proc pcerrorstop(ichar filename,int lineno)=
	filehandle f
	f:=fopen("$error.tmp","w")
	println @f,filename,lineno
	println
	fclose(f)
	stop 1
end

export func getbasename(ichar s)ichar t=
	t:=s+strlen(s)-1
	while t>s and (t-1)^<>'.' do
		--t
	od

	return t
end

export proc pcl_genmcl=
	genmcl()
end


=== tc_decls.m 0 0 14/34 ===

global type psymbol = ref pstrec

!global record pstrec = $caligned
global record pstrec =
	ichar name
	psymbol next				!proc or global static
	psymbol nextparam
	psymbol nextlocal
	psymbol nextstatic
	pcl code					!proc body; istatic init data

!	psymbol owner
!	psymbol generic				!locals/params: version in global ST

!	ref procinforec info		!procs: info to help codegen	

	union
!		pcl pcaddr				!for procs: entry point to func
		ref proc dlladdr		!for imported funcs
		ivoid staddr			!var statics: address
		psymbol cprocowner		!C target:var statics: owner proc
	end
	ref fwdrec fwdrefs			!fwd ref chain

	byte id
	byte opcode					!for opcode_rw
	byte subcode				!for jumpcc_rw/setcc_rw/type_rw
	byte nrefs
	i32 offset

	byte mode
	byte isentry
	byte nretvalues				!func: number of return values (0 for proc)
	byte varparams				!0 or N; variadic params

	byte dllindex				!for dllproc: which dll in dlltable
	byte reg
	byte reftype
	byte segment

	i32 labelno
	u32 seqno

	u32 size
	i16 stindex
	i16 importindex

	i16 nlocals
	i16 impindex
	i16 expindex
	u16 flags:(chasstatics:1, addrof:1, atvar:1, used:1,
				imported:1, exported:1, isthreaded:1, ishandler:1,
				ismain:1, variadic:1)

	byte scope
	byte nparams
	byte align					!for variables

end

global type pcl = ref pclrec

global record pclrec =
	pcl next

	union						!two 32-bit params used according to opcode
		struct					!pointer ops
			i32 scale			!scale factor for offset
			i32 extra			!extra constant byte offset, already scaled
		end
		struct					!call/etc
			i32 nargs			!number of args set via setparam
			i32 nret			!0/1/2 for call proc/func/mult
		end
		struct					!switch
			i32 minlab
			i32 maxlab
		end

!following single values set by pc_gen_n or pc_gen_cond or pc_gen_op
		i32 index				!general sequential index for setparam/temp etc
		i32 fnindex			!sysfn index number
		i32 cond				!pcl condition code for jumpcc etc
		i32 step				!always +ve fixed step size for forup/fordown; also INCR
		i32 truncmode			!convert/truncate: truncated mode

		struct
			i32 x				!common access to these two params
			i32 y
		end
	end
	u32 pos:(sourceoffset:24, fileno:8)
	u32 size

	byte mode
	byte mode2
	byte opcode
	byte flags:(isglobal:1, isvariadic:1,ffi:1)

	byte nopnds
	byte argoffset				!So that p.abc[i+offset] accesses i'th argument
!	[2]byte SPARE
!	U16 RECSIZE
	U16 SEQNO

	u32 isfirst				!indexed by bit
	u32 islast
	
!only allocated up to here; the rest depends on number of pclopnds

	union
		struct
			pclopnd a,b,c					!only present for correct .nopnds
		end
		[]pclopnd abc
!		[-1:]pclopnd args				!args [1] corresponds with abc[3]
	end
end

global type pclopnd = ref opndrec

global record opndrec =
	union
		i64 value
		r64 xvalue
		r32 xvalue32
		ichar svalue
		psymbol def
		struct
			byte tempno
			byte reg
		end

		int labelno
		unit asmcode
	end
	u32 size
	byte optype
	byte mode
	byte flags:(isvariadic:1, isbinary:1, isstring:1)
	byte spare1
end

export record fwdrec =
	ref fwdrec nextfwd
	i32 offset
	i16 reltype
	i16 seg
end

global byte pcldone, mcldone, ssdone, objdone, exedone

global psymbol pstatictable, pstatictablex
global psymbol pproctable, pproctablex

global psymbol currprog
export psymbol currfunc
global psymbol blockretname
global psymbol entryproc		!entry point func

global const maxparam=100

global const maxplibfile=50
global [maxplibfile]ichar plibfiles
global [maxplibfile]u64 plibinst
global int nplibfiles

strbuffer sbuffer
global ref strbuffer pdest=&sbuffer

EXPORT ICHAR $PMODULENAME
=== tc_diags.m 0 0 15/34 ===
int currlineno
int currfileno

PCL LASTPCL

strbuffer sbuffer
ref strbuffer dest=&sbuffer
int destlinestart

!const tab1="  "
!const tab2="    "
const tab1="  "
!const tab2=tab1+tab1

const pclindent = 1

!const fshowsymbols=1
const fshowsymbols=0

ref[]byte labeltab

global proc strpcl(pcl p, int inline=0)=
!inline=1 when generating inline comments for strmcl
	int opcode, nopnds
	pclopnd a,b,c
	int ntypes, defused

	const showformatted=1
	opcode:=p.opcode
!CPL "----STRPCL",PCLNAMES[OPCODE], CURRFUNC.NAME

	nopnds:=p.nopnds
	ntypes:=pclhastype[opcode]

	a:=b:=c:=nil

	if nopnds then
		a:=p.a
		if nopnds>1 then
			b:=p.b
			if nopnds>2 then
				c:=p.c
			fi
		fi
	fi

	case opcode
	when klabel then
		strlabel(a.labelno,1)
		return
	when kcomment then
		psstr("!")
		psstr(a.svalue)
		return
!
	esac

	psstr(tab1)

!PSSTR(STRINT(INT(P),"H"))
!PSSTR(" ")
!PSSTR(STRINT(P.SEQNO,"4Z"))
!PSSTR(" ")

	defused:=0
	if not showformatted then
		goto default
	fi

	switch opcode
	when kmove then
		psopnd(a)
		psassign()
		psopnd(b)

	when klabel then

	when kjump then
		psstr("goto ")
		psopnd(a)

	when kijump then
		psstr("goto ")
		psopnd(a)
		psstr("^")

	when kadd..kfmod then
!IF A.OPTYPE<>TEMP_OPND THEN PSSTR("TTTADD/NONTEMP ") FI
		psopnd(a)
		psassign()
		psbinop(p.opcode,b,c)

	when kneg..ktypepun then
		psopnd(a)
		psassign()
		psmonop(p.opcode,b)

	when kaddto..ksubpxto then
		psbinop(p.opcode,a,b)

	when knegto..ktoboolto then
		psmonop(p.opcode,a)

	when kjumpcc then
		psstr("if ")
		psopnd(b)
		psstr(ccshortnames[p.cond])
		psopnd(c)
		psstr(" then goto ")
		psopnd(a)

	when ksetcc then
		psopnd(a)
		psassign()
		psopnd(b)
		psstr(ccshortnames[p.cond])
		psopnd(c)

	when kjumpf then
		psstr("if not ")
		psopnd(b)
		psstr(" then goto ")
		psopnd(a)

	when kjumpt then
		psstr("if ")
		psopnd(b)
		psstr(" then goto ")
		psopnd(a)

	when kjumpin, kjumpout then
		psstr("if ")
		psopnd(b)
		psstr((opcode=kjumpin|" in "|" not in "))
		psopnd(c)
		psstr(" .. ")
		psopnd(p.abc[4])
		psstr(" then goto ")
		psopnd(a)

!	when kjumpsete, kjumpsetn then
!		psstr("if ")
!		psopnd(b)
!		psstr((opcode=kjumpsete|" = "|" <> "))
!		psopnd(c)
!		psstr(" then goto ")
!		psopnd(a)

	when kforup, kfordown then
		psopnd(b)
		psstr((opcode=kforup|" +:= "|" -:= "))
		psint(p.step)

		psstr("; if ")
		psopnd(b)
		psstr((opcode=kforup|" <= "|" >= "))
		psopnd(c)
		psstr(" then goto ")
		psopnd(a)

	when kto then
		psopnd(b)
		psstr("--; if ")
		psopnd(b)
		psstr(" then goto ")
		psopnd(a)

	when kaddpx, kloadpx then
!IF OPCODE=KLOADPX AND A.OPTYPE<>TEMP_OPND THEN PSSTR("TTTLOADPX/NONTEMP ") FI
		psopnd(a)
		psstr(" := ")
		psptr(b, c, p.scale, p.extra)
		if opcode=kloadpx then
			psstr("^")
		fi

	when kstorepx then
		psptr(a, b, p.scale, p.extra)
		psstr("^ := ")
		psopnd(c)

!	when ksysproc, ksysprocx then
!		psstr(sysfnnames[p.fnindex]+3)
!		psstr("(")
!		for i to p.nopnds do
!			psopnd(p.abc[i])
!			if i<p.nopnds then psstr(",") fi
!		od
!		psstr(")")
!
!	when ksysfn,ksysfnx then
!		psopnd(a)
!		psstr(" := ")
!		psstr(sysfnnames[p.fnindex]+3)
!		psstr("(")
!		if p.nopnds>1 then
!			psopnd(b)
!			if p.nopnds=3 then
!				psstr(",")
!				psopnd(c)
!			fi
!		fi
!		psstr(")")
!
	when kiswap then
		psstr("swap(")
		psopnd(a)
		psstr(",")
		psopnd(b)
		psstr(")")

!	when kblocktemp then
!		psstr("B")
!		psint(p.index)

	when kswitch, kswitchu then
		psstr((opcode=kswitch|"switch "|"switchu "))
		psopnd(c)
		psstr(" (")
		psopnd(a)
		psopnd(b)
		psint(p.minlab)
		psstr(":")
		psint(p.maxlab)
		psstr(")")

	when kcall then
		for i to p.nret do
			psopnd(p.abc[i+1])
			if i<p.nret then psstr(", ") fi
		od
		if p.nret then psstr(" := ") fi

		psopnd(a)
		psstr("(")
		for i to p.nargs do
			psopnd(p.abc[i+p.argoffset])
			if i<p.nargs then psstr(", ") fi
		od
		psstr(")")

	when kincrto, kdecrto then
		psopnd(a)
		if p.step=1 then
			psstr((opcode=kincrto|"++"|"--"))
		else
			psstr((opcode=kincrto|"+:="|"-:="))
			psint(p.step)
		fi

	when kstop then
		psstr("stop ")
		psopnd(a)

	when kclear, kretfn, kretproc, kswlabel, kdata,
			kretmult, keval then
		goto default

	when kloadbit, kloadbf then
		psopnd(a)
		psassign()
		psopnd(b)
		psstr(".[")
		psopnd(c)
		if opcode=kloadbf then
			psstr("..")
			psopnd(p.abc[4])
		fi
		psstr("]")

	when kstorebit then
		psopnd(a)
		psstr(".[")
		psopnd(b)
		psstr("]")
		psassign()
		psopnd(c)

	when kstorebf then
		psopnd(a)
		psstr(".[")
		psopnd(b)
		psstr("..")
		psopnd(c)
		psstr("]")
		psassign()
		psopnd(p.abc[4])

	when kincrload, kdecrload then
		psopnd(a)
		psassign()
		psstr((opcode=kincrload|"++"|"--"))
		psopnd(b)

	when kloadincr, kloaddecr then
		psopnd(a)
		psassign()
		psopnd(b)
		psstr((opcode=kloadincr|"++"|"--"))

	else
		PSSTR("@@ ")				!may need attention
default:
!CPL "DEFAULT"
		psstr(pclnames[opcode])
		psstr(" ")
		defused:=1
		for i to nopnds do
			psopnd(p.abc[i])
			psstr(" ")
		od
	end switch

	if inline then
		PSTABTO(30)
!		psstr("   ")
	else
		PSTABTO(40)
	fi

	case ntypes
	when 1, 2 then
		psmode(p.mode, p.size)
		if ntypes=2 and p.mode<>p.mode2 then
			psstr("/")
			psmode(p.mode2)
		fi
	when 3 then
		if a then
PSINT(NOPNDS)
			psstr("(")
			for i to nopnds do
				a:=p.abc[i]
				if i>1 then psstr(",") fi
				if a.mode then
					psmode(a.mode, a.size)
				else
					psstr("---")
				fi
			od
			psstr(")")
		fi
		psstr(" ")
	else
		psstr("---")
	esac

	if inline then
		psstr(" ")
	else
		PSTABTO(56)
	fi

	gs_leftstr(dest,pclnames[opcode],9)
return when inline
	PSSTR("|")
!CPL $LINENO,A,=P.NOPNDS
!RETURN

!if not showformatted then
	if defused and (p.x or p.y) then
		psstr(" X:")
		psint(p.x)
		psstr(" Y:")
		psstr(" Y:")
		psint(p.Y)
	fi

	if p.isglobal then psstr(" Isglobal") fi
	if p.isvariadic then psstr(" Isvariadic") fi
!
!IF OPCODE=KCALL THEN
!	PSSTR(" NRET:"); PSINT(P.NRET)
!	PSSTR(" NARGS:"); PSINT(P.NARGS)
!	PSSTR(" ARGOFF:"); PSINT(P.ARGOFFSET)
!FI

!	PSTABTO(80)
	for i to nopnds do
		a:=p.abc[i]
		if a and a.optype in [temp_opnd, tempptr_opnd] then
			if p.isfirst.[i] then
!				psstr(" (")
				psstr("<")
				psstr(gettempnamex(a))
				psstr(": ")
			fi
			if p.islast.[i] then
				psstr(":")
				psstr(gettempnamex(a))
				psstr(">")
!				psstr(")")
			fi
		fi
	od
!CPL $LINENO

!	PSSTR(" ")
!	PSINT(INT(P.A)); PSSTR(" ")
!	PSINT(INT(P.B)); PSSTR(" ")
!	PSINT(INT(P.C))

end

global function stropnd(pclopnd p)ichar=
	[maxparam]pclopnd paramopnds
	static[512]char str
	[4]char str2
	psymbol d
	ref byte q

!RETURN "<OPND>"

	if p=nil then
		return "-"
	fi

	case p.optype
	when int_opnd then
		return strint(p.value)

	when real_opnd then
		return strreal(p.xvalue)
	when r32_opnd then
		return strreal(p.xvalue32)

	when string_opnd then
		if int(strlen(p.svalue))<str.len/2 then
			strcpy(str,"""")
			convertstring(p.svalue,&.str+1)
			strcat(str,"""")
		else
			return "<Long str>"
		fi

	when metastring_opnd then
		fprint @str,"{#}",p.svalue

	when mem_opnd then
		print @str,p.def.name

	when memaddr_opnd then
		fprint @str,"&#",p.def.name

	when temp_opnd then
		strcpy(str,gettempnamex(p))

	when tempptr_opnd then
		fprint @str,"#^",gettempnamex(p)

	when label_opnd then
		fprint @str,"L# ",p.labelno

	when assem_opnd then
		return "<Assem>"

	when data_opnd then
		q:=p.svalue
		if p.isstring then
			print @str, "S<"
			to min(p.size,40) do
				str2[1]:=q^; str2[2]:=0
				strcat(str, str2)
				++q
			od
		else						!binary, or normal non-data-string data
!		if p.isbinary then
			print @str, "B<"
			to min(p.size,10) do
				strcat(str, strint(q^,"Hz2"))
				strcat(str, " ")
				++q
			od
!		else
!			STRCPY(STR, "<NOT S OR B>")
		fi
		strcat(str, ">")

	else
		return "<PCLOPND?>"
	esac

	return str
end

global function strpclstr(pcl p)ichar =
	gs_init(dest)

	destlinestart:=1
	strpcl(p,1)

	return dest.strptr
end

global proc strlabel(int labelno,colon=0)=
	psstr("L")
	psint(labelno)
	if colon then
		psstr(":")
	fi
	psstr(" ")
end

global func writeallpcl(ichar caption)ref strbuffer=
!write all pcl code in system by scanning all procs
!pcl code is only stored per-proc
	pcl p
	psymbol d,e
	pclopnd a

	gs_str(dest,"PROC ")
	gs_strln(dest,caption)
	gs_strln(dest,"!DATA ------------------------------------------------------")

!GS_STRLN(DEST,"<WRITEPSTALLPCL TO BE REVISED>")

!CHECKCOMM("ALL1")


	labeltab:=pcm_allocz(mlabelno)			!indexed 1..labelno

!!scan code looking for unused labels, or rather, used ones
!
!	p:=pcstart.next
!
!	while p, p:=p.next do
!		if p.opcode<>klabel then
!			for i to p.nopnds do
!				a:=p.abc[i]
!				if a.optype=label_opnd then
!					labeltab[a.labelno]:=1
!				fi
!			od
!		fi
!	od
!

	d:=pstatictable

	while d, d:=d.next do
		if d.id=static_id then
			psstr("var ")
			psstr(strpmode(d.mode, d.size))
			psstr(" ")
			psstr(d.name)

			if d.code then
				psstr(" = ")
				psdata(d.code)
			else
				psline()
			fi
		fi
	od
	psline()

	gs_strln(dest,"!CODE ------------------------------------------------------")
	d:=pproctable

	while d, d:=d.next do
		currfunc:=d
		if d.id=proc_id then
			psprocsig(d)

			psprocbody(d)

			psstrline("End")
			psline()

		fi
	od
	psline()


!	p:=pcstart.next
!	while p, p:=p.next do
!		writepcl(p)
!		destlinestart:=dest.length
!	od

!	gs_strln(dest,"------------------------------------------------------")
!
	pcm_free(labeltab, mlabelno)
!
	return dest
end

proc writepcl(pcl p)=
!	gs_leftint(dest,p.lineno,4)
!	gs_str(dest,"--")

	unless p.opcode=klabel and not labeltab[p.abc[1].labelno] then
		strpcl(p)
		gs_line(dest)
		psstrline("") when p.opcode=keval
	end
end

proc psopnd(pclopnd p)=
	psstr(stropnd(p))
end

proc psbinop(int opc,pclopnd a=nil,b=nil)=
	tabledata []byte opcodes, []ichar opnames =
		(kadd,		"+"),
		(ksub,		"-"),
		(kmul,		"*"),
		(kdiv,		"/"),
		(kidiv,		"%"),
		(kirem,		"rem"),
		(kbitand,	"iand"),
		(kbitor,	"ior"),
		(kbitxor,	"ixor"),
		(kshl,		"<<"),
		(kshr,		">>"),
!		(kand,		"and"),
!		(kor,		"or"),
!		(kaddpx,	"+(pi)"),
		(ksubpx,	"-(pi)"),
		(ksubp,		"-(pp)"),
		(kpower,	"**"),
		(kaddto,	"+:="),
		(ksubto,	"-:="),
		(kmulto,	"*:="),
		(kdivto,	"/:="),
		(kidivto,	"%:="),
		(kiremto,	"rem:="),
		(kbitandto,	"iand:="),
		(kbitorto,	"ior:="),
		(kbitxorto,	"ixor:="),
		(kshlto,	"<<:="),
		(kshrto,	">>:="),
		(kmaxto,	"max:="),
		(kminto,	"min:="),
!		(kandto,	"and:="),
!		(korto,		"or:="),
	end

	for i to opcodes.len do
		if opc=opcodes[i] then
			psopnd(a)
			psstr(" ")
			psstr(opnames[i])
			psstr(" ")
			psopnd(b)
			return
		fi
	od

	psstr(pclnames[opc])	
	psstr("(")
	psopnd(a)
	psstr(",")
	psopnd(b)
	psstr(")")
end

proc psmonop(int opc,pclopnd a)=
	tabledata []byte opcodes, []ichar opnames =
		(kneg,		"-"),
		(kbitnot,	"inot "),
		(knot,		"not "),
		(knegto,	"-:="),
		(kabsto,	"abs:="),
		(kbitnotto,	"inot:="),
		(knotto,	"not:="),
		(ktoboolto,	"istrue:="),
	end

	for i to opcodes.len do
		if opc=opcodes[i] then
			psstr(opnames[i])
			psopnd(a)
			return
		fi
	od

	psstr(pclnames[opc])	
	psstr("(")
	psopnd(a)
	psstr(")")
end

proc psassign=
	gs_str(dest," := ")
end

global proc psmode(int mode, size=0) =
	psstr(strpmode(mode, size))
end

proc psprocsig(psymbol d)=
	psymbol e
	byte comma:=0
	int lastmode:=tvoid, m, lastsize, size

	psstr("Proc ")
	psstr(d.name)
	psstr("(")

	e:=d.nextparam

	while e, e:=e.nextparam do
		if comma then psstr(", ") fi
		if e.mode<>lastmode and e.size<>lastsize then
			lastmode:=e.mode
			lastsize:=e.size
			psstr(strpmode(lastmode, lastsize))
			psstr(" ")
		fi
		psstr(e.name)

		comma:=1
	od
	psstr(")")
	if d.mode then
		psstr(strpmode(d.mode, d.size))
	fi
!	psstr(":")
	psstrline(" =")

	comma:=0
	e:=d.nextlocal
	while e, e:=e.nextlocal do
		if comma then psline() fi
		psstr(tab1)
		psstr(strpmode(e.mode, e.size))
		psstr(" ")
		psstr(e.name)
		comma:=1
	od
	if comma then psline() fi
	if d.nextlocal then psline() fi
end

proc psdata(pcl p)=
	pclopnd a
	byte mult:=istrue p.next

	if mult then psstrline("(") fi

	while p, p:=p.next do
		a:=p.a
		if mult then psstr("    ") fi
!		psstr(pclnames[p.opcode])
!		psstr(" ")
		psstr(stropnd(a))
		psstr(" ")

		psmode(a.mode, a.size)
		if p.next then psstr(",") fi
		psline()
	od

	if mult then psstrline(")") fi
end

proc psprocbody(psymbol d)=
	pcl p
	pclopnd a

	p:=d.code

	return unless p

!do first pass populating label table

	while p, p:=p.next do
		if p.opcode<>klabel then
			for i to p.nopnds do
				a:=p.abc[i]
				if a.optype=label_opnd then
					labeltab[a.labelno]:=1
				fi
			od
		fi
	od
!
	p:=d.code.next				!skip nop

	while p, p:=p.next do
		writepcl(p)
		destlinestart:=dest.length
	od

end

proc psptr(pclopnd a, b, int scale, offset)=
!CPL "PSPTR",A,B
	psstr("(")
	psopnd(a)

!	if b then
!!		if b.optype=int_opnd then
!!			offset+:=b.value*scale
!!		else
!			psstr(" + ")
!			psopnd(b)
!!			if scale>1 then
!				psstr("*")
!				psint(scale)
!!			fi
!!		fi
!	fi

	if b then
		if b.optype=int_opnd then
			offset+:=b.value*scale
		else
			psstr(" + ")
			psopnd(b)
			if scale>1 then
				psstr("*")
				psint(scale)
			fi
		fi
	fi

	if offset>0 then
		psstr(" + ")
		psint(offset)

	elsif offset<0 then
		psstr(" - ")
		psint(-offset)
	fi
	psstr(")")
end

global function gettempnamex(pclopnd ax)ichar =
!return Tn, Mn, Fn, Bn

	int temp
	char prefix
	static [32]char str

	temp:=ax.tempno
	print @str,"T",,temp

	return &.str
end

global proc psstr(ichar s)=
	gs_str(dest,s)
end

global proc psstrline(ichar s)=
	gs_str(dest,s)
	gs_line(dest)
end

global proc psline=
	gs_line(dest)
end

global proc psint(int a)=
	gs_str(dest,strint(a))
end

global proc psname(psymbol d)=
	gs_str(dest,getfullname(d))
end

global proc pstabto(int n)=
	int col:=dest.length-destlinestart
	while n>col do psstr(" "); ++col od
end

global func writepst:ref strbuffer=
	gs_init(dest)

	gs_strln(dest,"------------------------------------------------------")
	writepst2("PROC PST Global Static Table", pstatictable)
	psline()
	writepst2("PROC PST Global Proc Table", pproctable)
	psline()

	return dest
end

global proc writepst2(ichar caption, psymbol d)=
	int i:=0, j
	psymbol e

	psstrline(caption)
	psline()

	while d, d:=d.next do
!PSSTR(STRINT(INT(D),"H"))
!PSSTR(" ")
		writepsymbol(d, "25jl")

		if d.id in [proc_id, import_id] then
			e:=d.nextparam
			j:=0
			while e, e:=e.nextparam do
				psstr("    ")
				writepsymbol(e, "21jl")
			od
			e:=d.nextlocal
			j:=0
			while e, e:=e.nextlocal do
				psstr("    ")
				writepsymbol(e, "21jl")
			od
			e:=d.nextstatic
			j:=0
			while e, e:=e.nextstatic do
				psstr("    ")
				writepsymbol(e, "21jl")
			od
		fi
!PSLINE()
	od
	psline()
end

proc writepsymbol(psymbol d, ichar fmt)=
	byte localfile:=0
	[256]char str

	print @str, d.seqno:"4", idnames[d.id]
	psstr(str)
	to 8-strlen(idnames[d.id]) do psstr(" ") od

	str[1]:=0

	print @str, d.name:fmt
	psstr(str)

	psstr(strpmode(d.mode, d.size))

	if d.id=proc_id then
		psstr(" Pm:")
		psint(d.nparams)
		psstr(" Loc:")
		psint(d.nlocals)
	fi

	if d.exported then psstr(" Exp") fi
	if d.imported then psstr(" Imp") fi
	if d.varparams then psstr(" Var:"); psint(d.varparams) fi
	if d.isthreaded then psstr(" TC") fi
!*!	if d.reg then psstr(" "); psstr(regnames[d.reg]) fi
!	if d.hasdot then psstr(" Dot") fi
	if d.isentry then psstr(" ENTRY PT") fi

!	if d.id=proc_id then psstr(" .PCADDR ="); PSSTR(STRINT(CAST(D.PCADDR),"H")) fi

!	if d.owner then
!		psstr(" (")
!		psint(d.owner.seqno)
!		psstr(" ")
!		psstr(d.owner.name)
!		psstr(")")
!	fi	

	if ctarget and d.id=static_id and d.cprocowner then
		psstr(" (Proc:")
		psstr(d.cprocowner.name)
		psstr(") (D:")
!		psint(cast(d.pcdata))
!*!		psstr(strint(cast(d.pcdata),"H"))
		psstr(")")
	fi
	if ctarget and d.id=proc_id and d.chasstatics then
		psstr(" Has statics")
!		psint(d.chasstatics)
	fi

	psline()
end
=== tc_tables.m 0 0 16/34 ===
!type system

export enumdata \
		[0:]ichar pstdnames,
		[0:]byte psize,

		[0:]byte psigned,

		[0:]byte pint,
		[0:]byte pfloat,

		[0:]byte pmin,						!promoted type when min width applies
		[0:]byte piwrb =					!int/word/real/block

	(tpvoid=0,    "void",    	0,	0, 0,0,	tpvoid,		tpvoid),

	(tpr32,       "r32",    	4,	0, 0,1,	tpr32,		tpr32),
	(tpr64,       "r64",    	8,	0, 0,1,	tpr64,		tpr64),

	(tpu8,        "u8",      	1,	0, 1,0,	tpu32,		tpu64),
	(tpu16,       "u16",    	2,	0, 1,0,	tpu32,		tpu64),
	(tpu32,       "u32",    	4,	0, 1,0,	tpu32,		tpu64),
	(tpu64,       "u64",    	8,	0, 1,0,	tpu64,		tpu64),

	(tpi8,        "i8",      	1,	1, 1,0,	tpi32,		tpi64),
	(tpi16,       "i16",    	2,	1, 1,0,	tpi32,		tpi64),
	(tpi32,       "i32",    	4,	1, 1,0,	tpi32,		tpi64),
	(tpi64,       "i64",    	8,	1, 1,0,	tpi64,		tpi64),

	(tpblock,     "mem",   		0,	0, 0,0,	tpblock,	tpvoid),
	(tpvector,    "vec",   		0,	0, 0,0,	tpvector,	tpvoid),

	(tplast,      "$last",   	0,	0, 0,0,	0,			0),


end

global const tpref = tpu64

global enumdata [0:]ichar opndnames =
	(no_opnd=0,			$),

	(mem_opnd,			$),
	(temp_opnd,			$),
	(tempptr_opnd,		$),
	(memaddr_opnd,		$),

	(int_opnd,			$),
	(real_opnd,			$),
	(r32_opnd,		$),

	(realimm_opnd,		$),
	(realimm32_opnd,	$),

	(string_opnd,		$),
	(label_opnd,		$),
	(data_opnd,			$),

	(assem_opnd,		$),
	(metastring_opnd,	$),
end

!pclhastype:
! 0 		no type info
! 1			pcl.mode (t)
! 2			pcl.mode & pcl.mode2 (t & u)
! 3			Uses arg types only (eg. CALL)

global enumdata [0:]ichar pclnames, [0:]byte pclwrite, [0:]byte pclhastype =

!TCL opcodes
! T	lvalue			T3
! M	lvalue			x, T (x is static/local/proc)
! P lvalue			x, T3^

! a b c d			Rvalues: T3, x, &x, 123 4.56 "ABC" L123
! D					Data Rvalue: x, &x, 123 4.56 "ABC" L123 <datastr>

! L Label index		Labels
! d symbol			M (ST entry)

!** means opcode needs a 4th pclopnd; this needs specialing handling for temps

!                    Wr  Types       (a b c)
	(knop=0,	$+1,  0,  0),  !     (- - -)
	(kcomment,	$+1,  0,  0),  !     (a - -)
  
	(kmove,		$+1,  1,  1),  !     (M b -)	M := b
	(keval,		$+1,  0,  1),  !     (a - -)
  
	(kaddpx,	$+1,  1,  1),  ! s,n (T b c)	T := b + c*s + n
	(kloadpx,	$+1,  1,  1),  ! s,n (T b c)	T :=(b + c*s + n)^
	(kstorepx,	$+1,  0,  1),  ! s,n (b c r)	(a + b*s + n)^ := c
  
	(kcall,		$+1,  0,  3),  ! r,n (a- - -)	(Fn [T ...] [r ...]) r=nret, n=nargs
	(kretproc,	$+1,  0,  0),  !     (- - -)	return
	(kretfn,	$+1,  0,  1),  !     (a - -)	return a
	(kretmult,	$+1,  0,  2),  ! n   (a ...)	return n values

	(kjump,		$+1,  0,  0),  !     (L - -)	goto L
	(kjumpcc,	$+1,  0,  1),  ! cc  (L b c)	goto L when b cc c
	(kjumpt,	$+1,  0,  1),  !     (L b -)	goto L when istrue(b)
	(kjumpf,	$+1,  0,  1),  !     (L b -)	goto L when not istrue(b)
	(kijump,	$+1,  0,  1),  !     (a - -)	goto a
	(ksetcc,	$+1,  1,  1),  ! cc  (T b c)	T := b cc c
  
	(kto,		$+1,  0,  1),  !     (L b -)	--b; goto L when b<>0
	(kforup,	$+1,  0,  1),  ! n   (L b c)	b+:=n; goto L when b <= c
	(kfordown,	$+1,  0,  1),  ! n   (L b c)	b-:=n; goto L when b >= c

	(kiswap,	$+1,  0,  1),  !     (P P -)	swap(P, P)
  
	(kadd,		$+1,  1,  1),  !     (T b c)	T := b + c
	(ksub,		$+1,  1,  1),  !     (T b c)
	(kmul,		$+1,  1,  1),  !     (T b c)
	(kdiv,		$+1,  1,  1),  !     (T b c)
	(kidiv,		$+1,  1,  1),  !     (T b c)
	(kirem,		$+1,  1,  1),  !     (T b c)
	(kidivrem,	$+1,  1,  1),  !     (T b c)
	(kbitand,	$+1,  1,  1),  !     (T b c)
	(kbitor,	$+1,  1,  1),  !     (T b c)
	(kbitxor,	$+1,  1,  1),  !     (T b c)
	(kshl,		$+1,  1,  1),  !     (T b c)
	(kshr,		$+1,  1,  1),  !     (T b c)
	(kmin,		$+1,  1,  1),  !     (T b c)
	(kmax,		$+1,  1,  1),  !     (T b c)
  
	(ksubpx,	$+1,  1,  1),  !     (T b c)
	(ksubp,		$+1,  1,  1),  !     (T b c)
	(katan2,	$+1,  1,  1),  !     (T b c)
	(kpower,	$+1,  1,  1),  !     (T b c)
	(kfmod,		$+1,  1,  1),  !     (T b c)
  
	(kneg,		$+1,  1,  1),  !     (T b -)	T := -b
	(kabs,		$+1,  1,  1),  !     (T b -)
	(kbitnot,	$+1,  1,  1),  !     (T b -)
	(knot,		$+1,  1,  1),  !     (T b -)
	(ktoboolt,	$+1,  1,  1),  !     (T b -)
	(ktoboolf,	$+1,  1,  1),  !     (T b -)
  
	(ksqr,		$+1,  1,  1),  !     (T b -)
  
	(ksqrt,		$+1,  1,  1),  !     (T b -)
	(ksin,		$+1,  1,  1),  !     (T b -)
	(kcos,		$+1,  1,  1),  !     (T b -)
	(ktan,		$+1,  1,  1),  !     (T b -)
	(kasin,		$+1,  1,  1),  !     (T b -)
	(kacos,		$+1,  1,  1),  !     (T b -)
	(katan,		$+1,  1,  1),  !     (T b -)
  
	(klog,		$+1,  1,  1),  !     (T b -)
	(klog10,	$+1,  1,  1),  !     (T b -)
	(kexp,		$+1,  1,  1),  !     (T b -)
	(kround,	$+1,  1,  1),  !     (T b -)
	(kceil,		$+1,  1,  1),  !     (T b -)
	(kfloor,	$+1,  1,  1),  !     (T b -)
	(kfract,	$+1,  1,  1),  !     (T b -)
	(ksign,		$+1,  1,  1),  !     (T b -)
  
	(kfloat,    $+1,  1,  2),  !     (T b -)
	(kfix,		$+1,  1,  2),  !     (T b -)
	(ktruncate,	$+1,  1,  2),  !     (T b -)
	(kfwiden,	$+1,  1,  2),  !     (T b -)
	(kfnarrow,	$+1,  1,  2),  !     (T b -)
	(kwiden,	$+1,  1,  2),  !     (T b -)
  
	(ktypepun,	$+1,  1,  2),  !     (T b -)
  
	(kaddto,	$+1,  0,  1),  !     (P b -)
	(ksubto,	$+1,  0,  1),  !     (P b -)
	(kmulto,	$+1,  0,  1),  !     (P b -)
	(kdivto,	$+1,  0,  1),  !     (P b -)
	(kidivto,	$+1,  0,  1),  !     (P b -)
	(kiremto,	$+1,  0,  1),  !     (P b -)
	(kbitandto,	$+1,  0,  1),  !     (P b -)
	(kbitorto,	$+1,  0,  1),  !     (P b -)
	(kbitxorto,	$+1,  0,  1),  !     (P b -)
	(kshlto,	$+1,  0,  2),  !     (P b -)
	(kshrto,	$+1,  0,  2),  !     (P b -)
	(kminto,	$+1,  0,  1),  !     (P b -)
	(kmaxto,	$+1,  0,  1),  !     (P b -)
	(kaddpxto,	$+1,  0,  1),  !     (P b -)
	(ksubpxto,	$+1,  0,  1),  !     (P b -)
  
	(knegto,	$+1,  0,  1),  !     (P - -)
	(kabsto,	$+1,  0,  1),  !     (P - -)
	(kbitnotto,	$+1,  0,  1),  !     (P - -)
	(knotto,	$+1,  0,  1),  !     (P - -)
	(ktoboolto,	$+1,  1,  1),  !     (T b -)
  
	(kincrto,	$+1,  0,  1),  !     (P - -)	++a
	(kdecrto,	$+1,  0,  1),  !     (P - -)	--a
	(kincrload,	$+1,  1,  1),  !     (P - -)	a:=++b
	(kdecrload,	$+1,  1,  1),  !     (P - -)	a:=--b
	(kloadincr,	$+1,  1,  1),  !     (P - -)	a:=b++
	(kloaddecr,	$+1,  1,  1),  !     (P - -)	a:=b--
  
	(kswitch,	$+1,  0,  1),  ! n   (b c r)	switch a (b elements, index from c) n=0/1: normal/unchecked
	(kswitchu,	$+1,  0,  1),  ! n   (b c r)	switch a (b elements, index from c) n=0/1: normal/unchecked
	(kswlabel,	$+1,  0,  0),  !     (L - -)	label for switch jump table
	(kendsw,	$+1,  0,  0),  !     (L - -)	label for switch jump table
  
!	(kproc,		$+1,  0,  0),  !     (d - -)
	(kstop,		$+1,  0,  0),  !
	(klabel,	$+1,  0,  0),  !     (L - -)
  
	(kdata,		$+1,  0,  1),  !
  
	(kloadbit,	$+1,  1,  1),  !     (T b c)	T := b.[c]
	(kloadbf,	$+1,  1,  1),  !     (T b c d)	T := b.[c..d]
	(kstorebit,	$+1,  0,  1),  !	 (P b c)	P.[b] := c
	(kstorebf,	$+1,  0,  1),  !     (P b c d)	P.[b..c] := d
  
	(kjumpin,	$+1,  0,  1),  ! **
	(kjumpout,	$+1,  0,  1),  ! **
	(kjumpsete,	$+1,  0,  1),  !
	(kjumpsetn,	$+1,  0,  1),  !
  
	(kclear,	$+1,  0,  1),  !
  
	(klast,		$+1,  0,  0),  !

end

global enumdata [0:]ichar ccnames, [0:]ichar ccshortnames =
	(no_cc=0,	"xx",	"?"),
	(eq_cc,		"eq",	" = "),
	(ne_cc,		"ne",	" <> "),
	(lt_cc,		"lt",	" < "),
	(le_cc,		"le",	" <= "),
	(ge_cc,		"ge",	" >= "),
	(gt_cc,		"gt",	" > "),
end

export enumdata [0:]ichar idnames
	(null_id=0,		"--"),			!Not set (used for overall program name)
	(import_id,		"Import"),		!Imported symbol (proc or static)
	(proc_id,		"Proc"),		!Local proc
	(static_id,		"Static"),		!Local static
	(local_id,		"Local"),		!Function local var
	(param_id,		"Param"),		!Function param
	(label_id,		"Label"),		!Used in assembly
	(export_id,		"Export"),		!Used by makesymbol, is converted to proc_id/.exported
	(misc_id,		"Misc"),		!?
	(program_id,	"Program"),		!?
end

=== mm_decls.m 0 0 17/34 ===
global const maxmodule=300
global const maxsubprog=30
global const maxlibfile=50
global const maxsourcefile=300

global type symbol		= ref strec
global type unit  		= ref unitrec
global type imodule   	= ref modulerec
global type ifile   	= ref filerec
global type isubprog  	= ref subprogrec

global macro pr(a,b)	= (a<<16 ior b)

global record tokenrec =
	byte symbol
	byte subcode
	u16 slength				!string length; includes any zero term
	u32 pos: (sourceoffset:24, fileno:8)

	union
		ref strec symptr		!pointer to symbol table entry for name
		i64 value				!64-bit int
		real xvalue				!64-bit float
		u64 uvalue			!64-bit word
		ichar svalue			!pointer to string or charconst (not terminated)
	end
end

global record procrec =
	symbol def
	ref procrec nextproc
end

global record typenamerec=
	symbol owner			!owner of scope where typename was encountered
							!moduleno required by resolvetypename can be derived from owner
!A/B used as follows
!  nil B			Simple typename B
!  A   B			Dotted pair A.B
!  A   nil          I think represents a typeof(x) where x is a name
	symbol defa
	union
		symbol defb
		symbol def
	end
	ref i32 pmode
end

global record posrec=
	u32 pos: (sourceoffset:24, fileno:8)
end

global record uflagsrec =
	[7]byte	codes
	byte	ulength
end

global record strec = $caligned
	ichar name
	ref strec owner
	ref strec deflist
	ref strec deflistx
	ref strec nextdef
	ref strec nextdupl
	ref strec firstdupl			!point to generic version
!	union
	psymbol pdef			!pcl st version
!		unit pdoswx				!doswitchx used for ctarget
!	end

	unit code			!var idata/proc body/taggedunion tag value/etc

	i32 mode
	byte namelen
	byte symbol
	byte nameid
	byte subcode

	i32 index				!misc; eg. holds label numbers
	i32 offset

	u32 pos: (sourceoffset:24, fileno:8)
	u16 flags: (
		isstatic:1,
		hasdoswx:1,
		txdone:1,
		circflag:1,

		islet:1,
		addrof:1,
!		noreg:1,
		ishandler:1,

		atfield:1,
		atvar:1,
		istabdata:1,			!mark parallel enum/tabdata arrays

		issubprog:1,			!set in resolvetopname: module is also a subprog

		isimport:1)

	byte moduleno
	byte subprogno

	unit equivvar

	struct				!when a proc
		ichar truename			!for imported name only
		ref strec paramlist

		byte dllindex			!for dllproc: which dll in dlltable

		byte nretvalues			!func: number of return values (0 for proc)
		byte varparams			!0 or 1; variadic params in B and FF
		byte isthreaded			!0 or 1; variadic params in B and FF
	end

	struct						!when a record or record field
		ref strec equivfield
		uflagsrec uflags
		i32 baseclass
		byte bitfieldwidth		!width of bitfield in record
		byte align				!0, 2, 4, 8, 16 or 255 (auto-align)
		byte bitoffset			!0..31 for bitfields in records
		byte equivoffset
	end

	struct				!when a param name
		ref strec nextparam
		byte parammode			!0=byval_param, in_param, byref_param
		byte optional			!0 or 1	
		byte variadic			!variadic parameter for B code
		byte dummy3				!variadic parameter for B code
	end

	i16 regsize
	i16 maxalign		!for record types (doesn't fit above)
	u16 used

	byte scope
	byte equals			!for vars/params: 1/2/3 means =/:=/::= used

end


global record unitrec =
	byte tag				!jcode tag number
	byte insptr
	byte txcount
	byte spare
	u32 pos: (sourceoffset:24, fileno:8)

	unit nextunit

	union
		struct
			union
				unit	a
				symbol	def
				symbol	labeldef
				i64	value
				u64	uvalue
				r64	xvalue
				ichar	svalue
				i64	range_lower
			end

			union
				unit	b
				i64	range_upper
			end

			union
				unit	c
				[4]i16	cmppclmode
			end
		end
		[3]unit abc
	end

	union						!misc stuff depends on tag
		struct					!const string
			u32 slength			!includes any zero term
			byte isastring
			char strtype		!0/'B'/'S' = normal / bindata / strdata
		end

		struct					!name
			byte dottedname		!for jname: 1=resolved from fully qualified dotted seq
			byte avcode			!jname for/autovars: 'I','T','S' = index/to/step autovars
		end

		union					!asssemreg/xreg/mem
			struct
				byte reg
				byte regix
				byte scale
				byte prefixmode

				byte regsize
				byte cond
				byte spare2,spare3
				byte compactif	!for jif, 1 is using (a|b|c)
			end
			u64 reginfo
		end

		union					!for makelist
			u32 length		!number of elements
			byte makearray		!1 for makelist to create array-var not list-var
		end
		byte addroffirst	!1 for jnameaddr when derived from &.name

		u32 offset			!for jdot
		i32 whenlabel			!label no associated with when expr; for recase
		i32 swapvar			!for j-swap: 1 when swapping var:ref

		struct
			union
				i16 bitopindex	!
				i16 opcindex		!operator nodes
				i16 fnindex		!sf_add_var etc
!				i16 condcode		!pcl_eq etc; for jeq etc
				i16 bfcode
			end
		end
		i32 index
		[4]byte cmpgenop			!cmpchain: up to 8 genops
	end

	i32 mode
	union
		i32 convmode	!convert/typepun: source/target(?) mode (will be widened to give unit mode)
!		i32 memmode	!name/ptr/index/dot: void=LVALUE; non-void=RVALUE
		i32 elemmode	!for jnew/newvar
	end

	byte moduleno
	byte subprogno
	byte initlet		!1 for an assignment that initialises a let
	byte isconst		!1 for jconst, and jmakerange with const range

	byte resultflag		!1 when the result of this unit is needed; 0=void or discarded
	union
		byte pclop			!generic operator for jbin, incr etc
		byte propcode		!kklen etc
		byte inv			!notin
		byte convcode		!kkfix etc
	end
	byte istrueconst	!1 for actual "123" etc, not result of reduction
	byte pclcond		!eq_cc etc
end

global record modulerec=
	ichar	name				!module name and base filename
	ifile	file
	i16	moduleno			!useful if using pointer to a source rec
	i16	subprogno
	i16	fileno
	byte	issyslib
	byte	islead				!1 if lead module in sp

	union
		symbol	stmodule
		symbol	def
	end

	symbol	stsubprog
	symbol	stmacro

	symbol	ststart				!nil, or st entry of start()
	symbol	stmain				!nil, or st entry of main()
end

global record filerec=
	ichar	name				!module name and base filename
	ichar	filename			!base filename + extension
	ichar	path				!path where file resides
	ichar	filespec			!full file path
	ichar	text				!pointer to source text, 0-terminated
	ichar	dupl				!for ma files
	int		size				!source file size includes terminator

	byte	issyslib			!1 if a system module
	byte	issupport			!1 if a support file (strinclude); MAY BE STORED ELSEWHERE
	byte	compiled			!1 if compiled
	byte	islead				!1 if lead module in sp

	i16	subprogno
	i16	moduleno			!0, or moduleno

	i16	fileno				!refers to self
	i16	spare

end

global record subprogrec =
	ichar name
	i16 firstmodule			!will be header module or same as mainmodule if no header
	i16 mainmodule			!0, or module containing 'main'
	i16 lastmodule			!always first..lastmodule
!	i16 compiled				!1 if compiled
	byte flags:(compiled:1, issyslib:1)
	byte subprogno
end

global [0..maxmodule]imodule	modules
global [0..maxmodule]byte		moduletosub				!convert module no to subprog no
global [0..maxsubprog]isubprog	subprogs
global [0..maxsourcefile]ifile	sources
global [0..maxsubprog]byte		subproghasstart

global int nmodules
global int nsubprogs
global int nsourcefiles
global int nlibfiles

global symbol stprogram		!root into the symbol table
global symbol stmodule		!main module
global int currmoduleno				!used when compiling modules
global byte loadedfromma	!1 if source/support files are in loaded .ma file

global tokenrec lx				!provides access to current token data
global tokenrec nextlx			!provides access to next token

global [0..maxlibfile]ichar libfiles

global int mainsubprogno		!index of main subprog (eg. may be before/after syslib)

!global const int maxtype=6'000
global const int maxtype=16'000

global int ntypes

global [0..maxtype]symbol		ttnamedef
global [0..maxtype]symbol		ttowner			!for ttlowerexpr/rtlengthexpr

global [0..maxtype]i32		ttbasetype		!basetype
global [0..maxtype]ichar		ttname

global [0..maxtype]u32		ttsize
global [0..maxtype]byte			ttsizeset
global [0..maxtype]i32		ttlower 		!.lbound (default 1)
global [0..maxtype]i32		ttlength 		!elements in array/record/tuple
global [0..maxtype]ref[]i32	ttmult 			!ttlength elements in tuple

global [0..maxtype]unit			ttdimexpr		!length, lower:length, or lower..upper

global [0..maxtype]i32		tttarget 		!for array/ref types
global [0..maxtype]byte			ttusercat
global [0..maxtype]i32		ttlineno

global [0..maxtype]byte			ttsigned		!is i8 i16 i32 i64
global [0..maxtype]byte			ttisreal		!is r32 r64
global [0..maxtype]byte			ttisinteger		!is i8..i64/u8..u64/c8..c64
global [0..maxtype]byte			ttisshort		!is i8/i16/i32/u8/u16/u32/c8/c16
global [0..maxtype]byte			ttisref			!is a pointer

global [0..maxtype]byte			ttisblock		!is a variant

!global const int maxtypename=4'000
!global const int maxtypename=8'000
global const int maxtypename=38'000

global [0..maxtypename]typenamerec typenames
global [0..maxtypename]posrec typenamepos
global int ntypenames

global [0..symbolnames.upb]byte typestarterset

global symbol currproc

global int headermode=0

global ref procrec proclist,proclistx			!linked list of all procs
global ref procrec staticlist,staticlistx		!linked list of all static
global ref procrec constlist,constlistx		!linked list of all export consts

global unit nullunit

global const maxdllproc=1000

global int ndllproctable
global [maxdllproc]symbol dllproctable

global int fverbose=1		!1=normal, 0=less verbose, 2/3 = more verbose

global byte msyslevel=2		!0/1/2 = none/min/normal
global byte mvarlib=0		!0/1 = none/yes
global byte fvarnames=0		!display of names in asm/mcl

global byte fshowtiming
global byte fshowss
global byte fshowc
global byte fshowpcl
global byte fshowasm
global byte fshowast1
global byte fshowast2
global byte fshowast3
global byte fshowst
global byte fshowpst
global byte fshowstflat
global byte fshowtypes
global byte fshowmodules
global byte fcheckunusedlocals=0

global byte highmem=1			!enable rip by default
global byte clinux				!1 when clang_pass targeting linux

global byte dointlibs=fsyslibs

!passlevel used for compiler debug only
global int passlevel=0
global int dpasslevel=0
global int prodmode=0
global int debugmode=0
global int libmode=0					!1 means eventual ML/LIB target
global int fshortnames					!mcl/asm display

global ichar outfile					!one of the following two
global ichar destfilename				!nil, or override outfile
global ichar destfilepath				!nil, or set path of outfile

global int nunits
global int nunitsmem

!global const langhomedir	= "C:/mx/"
global const langhomedir	= "C:/bx/"

global const langhelpfile	= "mm_help.txt"

!GLOBAL INT NALLCALLS
!GLOBAL INT NUSESTACK
!GLOBAL INT NUSEMIXEDSTACK

!GLOBAL INT NGENINT
!GLOBAL INT NGENSMALLINT
=== mm_diags.m 0 0 18/34 ===
int currlineno
int currfileno

strbuffer sbuffer
ref strbuffer dest=&sbuffer

const tab1="\t"
const tab2="\t\t"

!const fshowsymbols=1
const fshowsymbols=0

global proc printst(filehandle f, ref strec p, int level=0)=
	ref strec q

	printstrec(f, p, level)

	q:=p.deflist

	while q<>nil do
		printst(f, q, level+1)
		q:=q.nextdef
	od
end

proc printstrec(filehandle f, ref strec p, int level)=
	strec dd
	ref byte q
	strbuffer v
	ref strbuffer d:=&v
	int col, offset, n
	const tabstr="    "
	[256]char str

	gs_init(d)

	print @str, p
	gs_str(d, str)
	gs_str(d, " ")

	offset:=0
	to level do
		gs_str(d, tabstr)
		offset+:=4
	od
	gs_str(d, ":")

	gs_leftstr(d, p.name, 28-offset, '-')
	gs_leftstr(d, namenames[p.nameid], 12, '.')

	col:=gs_getcol(d)
	dd:=p^


	gs_str(d, "[")
	if p.isimport then
		gs_str(d, "Imp ")
	else
		gs_str(d, SCOPENAMES[P.SCOPE])
		gs_str(d, " ")
	fi

	if dd.isstatic then
		gs_str(d, "Stat")
	fi

	if dd.nameid=paramid and dd.parammode then
		gs_str(d, parammodenames[dd.parammode])
	fi

	if dd.align then
		gs_str(d, "@@")
		gs_strint(d, dd.align)
		gs_str(d, " maxalign:")
		gs_strint(d, dd.maxalign)
		gs_str(d, " ")
	fi
	if dd.optional then
		gs_str(d, "Opt ")
	fi
	if dd.varparams then
		gs_str(d, "Var:")
		gs_strint(d, dd.varparams)
		gs_str(d, " ")
	fi

	if dd.moduleno then
		if dd.nameid<>subprogid then
			print @&.str, "Modno#",,dd.moduleno
		else
			print @&.str, "Subno#",,dd.subprogno
		fi
		gs_str(d, &.str)
	fi

	if dd.used then
		gs_str(d, "U ")
	fi

	if dd.isthreaded then
		gs_str(d, "Threaded ")
	fi


	gs_str(d, "]")
	gs_padto(d, col+10, '=')

	if p.owner then
		fprint @&.str, "(#)", p.owner.name
		gs_leftstr(d, &.str, 18, '-')
	else
		gs_leftstr(d, "()", 18, '-')
	fi

	case p.mode
	when tvoid then
		gs_str(d, "Void ")
	else
		GS_STRINT(D, P.MODE)
		GS_STR(D, ":")

		gs_str(d, strmode(p.mode))
		gs_str(d, " ")
	esac

	case p.nameid
	when fieldid, paramid then
		gs_str(d, " Offset:")
		gs_strint(d, p.offset)
		if p.mode=tbitfield then
			gs_str(d, " Bitoffset:")
			gs_strint(d, p.bitoffset)
			gs_str(d, ":")
			gs_strint(d, p.bitfieldwidth)
		fi

		sprintf(&.str, "%.*s", int(p.uflags.ulength), &p.uflags.codes)
		print @&.str, p.uflags.ulength:"v", ichar(&p.uflags.codes):".*"
		gs_str(d, " UFLAGS:")
		gs_str(d, &.str)
		gs_str(d, "-")
		gs_strint(d, p.uflags.ulength)

		if p.code then
			gs_str(d, "/:=")
			gs_strvar(d, strexpr(p.code))
		fi

		if p.nameid=paramid and p.variadic then
			gs_str(d, "...")
		fi
	when procid then

		gs_str(d, "Index:")
		gs_strint(d, p.index)

		gs_str(d, " Nret:")
		gs_strint(d, p.nretvalues)

	when dllprocid then
		gs_str(d, "Index/PCaddr:")
		gs_strint(d, p.index)
		if p.truename then
			gs_str(d, " Truename:")
			gs_str(d, p.truename)
		fi

	when staticid then
		if p.code then
			gs_str(d, "=")
			gs_strvar(d, strexpr(p.code))
		fi

	when frameid then
		if p.code then
			gs_str(d, ":=")
			gs_strvar(d, strexpr(p.code))
		fi

	when constid then
		gs_str(d, "Const:")
		gs_strvar(d, strexpr(p.code))

	when typeid then
		if p.baseclass then
			gs_str(d, "Baseclass:")
			GS_STR(D, "<HAS BASECLASS>")
		fi
!	when enumid then
!		gs_str(d, "Enum:")
!		gs_strint(d, p.index)
!	when dllmoduleid then
!		gs_str(d, "DLL#:")
!		gs_strint(d, p.dllindex)
	esac

	if p.atfield then
		gs_str(d, " @")
		gs_str(d, p.equivfield.name)
		gs_str(d, " +")
		gs_strint(d, p.equivoffset)
	fi
	if p.atvar then
		gs_strvar(d, strexpr(p.equivvar))
	fi

!gs_str(d, " Module# ")
!gs_strint(d, p.moduleno)
!
	gs_str(d, " Lineno: ???")
!gs_strint(d, p.lineno iand 16777215)

	gs_println(d, f)

	case p.nameid
	when constid, frameid, staticid, macroid then
		if p.code then
			printunit(p.code, dev:f)
		fi
	esac
end

global proc printstflat(filehandle f)=
symbol p
println @f, "GLOBAL SYMBOL TABLE:"

for i:=0 to hashtable.upb-1 do
	p:=hashtable[i]
	if p=nil then nextloop fi

!	IF P.NEXTDUPL=NIL THEN NEXTLOOP FI

	case p.symbol
	when namesym then
		println @f, i:"5", p, p.name, symbolnames[p.symbol],,":",,namenames[p.nameid]
		p:=p.nextdupl
		while p do
			print @f, "     ", p, p.name, symbolnames[p.symbol],,":",,namenames[p.nameid]
			if p.owner then
				fprint @f, " (From #:#)", p.owner.name, namenames[p.owner.nameid]
			fi

			println @f

			p:=p.nextdupl
		od
	esac
od
end

global proc printcode(filehandle f, ichar caption)=
ref strec p
ref procrec pp

pp:=proclist

while pp do
	p:=pp.def

	print @f, p.name,,"=", (p.scope|"Sub", "Prog", "Exp"|"Mod")
	if p.owner.nameid=typeid then
		print @f, " in record", p.owner.name
	fi
	println @f
	printunit(p.code, 0, 1, dev:f)
	println @f
	pp:=pp.nextproc
od
end

global proc printunit(ref unitrec p, int level=0, number=0, filehandle dev=nil)=
!p is a tagrec
	ref unitrec q
	ref strec d
	int t
	ichar idname
	i64 a
	r32 x32
	static int cmpchain=0

	if p=nil then
		return
	fi

	if p.pos then
		currlineno:=getlineno(p.pos)
		currfileno:=p.fileno
	fi

!	print @dev, p, ":"
	print @dev, getprefix(level, number, p)

	idname:=jtagnames[p.tag]
	print @dev, idname,,": "

	case p.tag
	when jname then
		d:=p.def

		print @dev, d.name, namenames[d.nameid]

		if d.code then
			print @dev, " {",,jtagnames[d.code.tag],,"}"
		fi

		print @dev, " ",,getdottedname(d)!, q
		print @dev, (p.dottedname|" {Dotted}"|"")

		if p.c then
			print @dev, " Lastcall:", p.c
		fi

		if p.addroffirst then
			print @dev, " Addroffirst."
		fi

		print @dev, " Moduleno:", p.moduleno

!		if p.avcode then print @dev, " AV:", char(p.avcode) fi
		if p.avcode then print @dev, " AV:", char(p.avcode), $ fi

	PRINT @DEV, =P.INDEX


	when jlabeldef then
		println @dev, p.def.name

	when jconst then
		t:=p.mode
		a:=p.value
		if t=trefchar then
			if p.slength>256 then
				print @dev, """",,"(LONGSTR)", """ *",,p.slength
			elsif p.slength then
				print @dev, """",,p.svalue,,""" *",,p.slength
			else
				print @dev, """"""
			fi

		elsecase ttbasetype[t]
		when ti64, ti32, ti16, ti8 then print @dev, i64(a)
		when tu64, tu32, tu16, tu8 then print @dev, u64(a)
		when tc64, tc8 then print @dev, chr(a)

		when tr32, tr64 then
			print @dev, p.xvalue
		when tref then
			if p.value then
				print @dev, "#",,p.value, P.SLENGTH
			else
				print @dev, "NIL"
			fi
		when tbool then
			print @dev, (p.value|"True"|"False")
		when tarray then
			print @dev, "<ARRAY>", =P.STRTYPE, =P.SLENGTH
		else
			println =typename(t), typename(ttbasetype[t])
			PRINT @DEV, "<PRINTUNIT BAD CONST PROBABLY VOID"
		fi
		print @dev, " ",,typename(t)
		if p.isastring then
!			print @dev, " <isstr>"
			fprint @dev, " <isstr>(#)", p.strtype
		fi

		if p.whenlabel then
			print @dev, " *L",,p.whenlabel
		fi

	when jdecimal then
		print @dev, p.svalue, "Len:", p.slength

	when jtypeconst then
		print @dev, typename(p.mode), typename(p.value)

	when jbitfield then
		print @dev, bitfieldnames[p.bfcode]+3

	when jconvert, jtypepun then
		print @dev, " Convmode:", strmode(p.convmode)

	when jmakelist then
		print @dev, "Len:", p.length, " Makeax:", p.makearray

	when jdot then
		print @dev, "Offset:", p.offset

	when jindex, jptr then

	when jexit, jredo, jnext then
		print @dev, "#",,p.index

	when jsyscall then
		print @dev, sysfnnames[p.fnindex]+3

	when jmakeset then
	when jcmpchain then
		for i to p.cmpgenop.len do
			if p.cmpgenop[i]=0 then exit fi
			print @dev, ccnames[p.cmpgenop[i]],," "
		od
	esac

	if p.isconst then
		print @dev, " Is const"
	else
		print @dev, " Not const"
	fi

	case p.tag
	when jbin, jbinto, junary, junaryto, jincr, 
		jandl, jorl, jnotl, jistruel then
		if p.pclop then
			fprint @dev, " Pcl<#>", pclnames[p.pclop]
		else
			fprint @dev, " no-op"
		fi
	when jprop then
		fprint @dev, " Prop<#>", propnames[p.propcode]
	when jconvert then
		fprint @dev, " Conv<#>", convnames[p.convcode]
	when jcmp then
		fprint @dev, " Pclcond<#>", ccnames[p.pclcond]
	esac


	println @dev

	for i to jsubs[p.tag] do
		printunitlist(dev, p.abc[i], level+1, i)
	od
end

proc printunitlist(filehandle dev, ref unitrec p, int level=0, number=0)=
	if p=nil then return fi

	while p do
		printunit(p, level, number, dev)
		p:=p.nextunit
	od
end

func getprefix(int level, number, ref unitrec p)ichar=
!combine any lineno info with indent string, return string to be output at start of a line
	static [1024]char str
	[1024]char indentstr
	[16384]char modestr
	ichar isexpr

	indentstr[1]:=0
	if level>10 then level:=10 fi

	to level do
		strcat(&.indentstr, "- ")
	od

	isexpr:="S"
	if jisexpr[p.tag] then isexpr:="E" fi

	case p.tag
	when jif, jswitch, jcase, jselect then
		if p.mode=tvoid then
			isexpr:="S"
		fi
	esac

	fprint @&.modestr, "# #:#", isexpr, (p.resultflag|"RES"|"---"), strmode(p.mode)
	modestr[256]:=0

	strcat(&.modestr, "-----------------------------")
	modestr[17]:=' '
	modestr[18]:=0

	strcpy(&.str, getlineinfok())
	strcat(&.str, &.modestr)
	strcat(&.str, &.indentstr)
	strcat(&.str, strint(number))
!	if prefix^ then
		strcat(&.str, " ")
!	fi

	return &.str
end

func getlineinfok:ichar=			!GETLINEINFO
	static [40]char str

	fprint @&.str, "# # ", CURRFILENO:"Z2", currlineno:"z4"
	return &.str
end

global proc printmodelist(filehandle f)=
	int mbase
	static ichar tab="\t"

!	PRINTLN @F, =NTYPENAMES
!	FOR I TO NTYPENAMES DO
!		PRINTLN @F, I, TYPENAMES[I].DEF.NAME
!	OD
!	PRINTLN @F
!
	println @f, "MODELIST", ntypes

	for m:=0 to ntypes do
		println @f, m:"4", strmode(m)
		mbase:=ttbasetype[m]

		println @f, tab, "Basetype:", mbase, strmode(mbase)
		println @f, tab, "ttname:", ttname[m]
		println @f, tab, "ttnamedef:", ttnamedef[m], (ttnamedef[m]|ttnamedef[m].name|"-")
		println @f, tab, "Target:", strmode(tttarget[m])
		println @f, tab, "Size:", ttsize[m], "Sizeset", ttsizeset[m]
		fprintln @f, "# Bounds: #..#  Length:#", tab, ttlower[m], ttlower[m]+ttlength[m]-1, ttlength[m]
		if mbase=ttuple then
			print @f, tab, "Mult:"
			for i to ttlength[m] do print @f, strmode(ttmult[m, i]),," " od
			println @f
		fi
		println @f, tab, "Signed:", ttsigned[m]
		println @f, tab, "Isreal:", ttisreal[m]
		println @f, tab, "Isinteger:", ttisinteger[m]
		println @f, tab, "Isshort:", ttisshort[m]
		println @f, tab, "Isref:", ttisref[m]
		println @f, tab, "Isblock:", ttisblock[m]
		println @f
	od
end

global proc showprojectinfo(filehandle dev)=
	imodule pm
	isubprog ps
	static ichar tab="    "
	ichar s
	byte isfirst, ismain

	println @dev, "Project Structure:"
	println @dev, "---------------------------------------"
	println @dev, "Modules", nmodules
	for i to nmodules do
		pm:=modules[i]

		if i>1 and pm.subprogno<>modules[i-1].subprogno then
			println @dev
		fi
		ps:=subprogs[moduletosub[i]]

			isfirst:=ps.firstmodule=i
			ismain:=ps.mainmodule=i

			if isfirst and ismain then s:="hm"
			elsif isfirst then s:="h "
			elsif ismain then s:="m "
			else s:="  " 
			fi

			print @dev, tab, i:"2", s, 
			pm.name:"16jl", "Sys:", pm.issyslib, 
			"Sub:", subprogs[pm.subprogno].name, "Fileno:", pm.fileno

		if pm.stmacro then
			print @dev, " Alias:", pm.stmacro.name
		fi
		if pm.stmain then
			print @dev, $, pm.stmain.name, ":", scopenames[pm.stmain.scope], pm.stmain
		fi
		if pm.ststart then
			print @dev, $, pm.ststart.name, ":", scopenames[pm.ststart.scope], pm.ststart
		fi

		println @dev
	od
	println @dev

	println @dev, "Subprograms", nsubprogs, =mainsubprogno
	for i to nsubprogs do
		ps:=subprogs[i]
		println @dev, tab, i, ps.name, "Sys:", ps.issyslib!, =PS.STSUBPROG

		if ps.firstmodule then
			print @dev, tab, tab
			for j:=ps.firstmodule to ps.lastmodule do
				print @dev, $, modules[j].name, "(", MODULES[J].STSUBPROG, ")"
			od
			println @dev
		fi
	od
	println @dev

	println @dev, "Sourcefiles", nsourcefiles
	ifile pf
	for i to nsourcefiles do
		pf:=sources[i]
		fprintln @dev, "  #:  Name=# File=# Path=# Spec=# Size=#", 
			i:"2", pf.name:"jl16", pf.filename:"jl18", pf.path:"20jl", pf.filespec:"30jl", pf.size:"7"
	od
	println @dev

	println @dev, "Link files", nlibfiles
	for i to nlibfiles do
		println @dev, tab, libfiles[i]:"16jl"
	od
	println @dev
end

global proc showlogfile=
	[256]char str
	filehandle logdev
	int size
	ref strbuffer ss

	if not debugmode then return fi

	logdev:=fopen(logfile, "w")

	if fshowmodules then showprojectinfo(logdev) fi

	if fshowasm and dpasslevel>=dmcl_pass then
		if ctarget then
			println @logdev, "PROC CLANG"
			addtolog(changeext(outfile, "c"), logdev)
		else
			println @logdev, "PROC ASSEMBLY"
			addtolog(changeext(outfile, "asm"), logdev)
		fi
	fi

	if fshowpcl and dpasslevel>=dpcl_pass then
		addtolog(changeext(outfile, "pcl"), logdev)
	fi
!	if fshowc and dpasslevel>=dclang_pass then
!		addtolog(changeext(outfile, "c"), logdev)
!	fi
	if fshowpst and dpasslevel>=dpcl_pass then
		addtolog("PSYMTAB", logdev)
	fi

	if fshowast3 and dpasslevel>=dtype_pass then addtolog("AST3", logdev) fi
	if fshowast2 and dpasslevel>=dname_pass then addtolog("AST2", logdev) fi
	if fshowast1 and dpasslevel>=dparse_pass then addtolog("AST1", logdev) fi

	if fshowst then
		showsttree("SYMBOL TABLE", logdev)
	fi
	if fshowstflat then
		showstflat("FLAT SYMBOL TABLE", logdev)
	fi
!
	if fshowtypes then
		printmodelist(logdev)
	fi

	size:=getfilesize(logdev)
	fclose(logdev)

	if size then
CPL "PRESS KEY..."; if OS_GETCH()=27 then stop fi
		print @&.str, "\\m\\ed.bat ", logfile

		if checkfile("mm.m") then
			os_execwait(&.str, 0, nil)
		else
			println "Diagnostic outputs written to", logfile
		fi
	fi
end

proc showstflat(ichar caption, filehandle f)=
	println @f, "PROC", caption
	printstflat(f)
	println @f
end

proc showsttree(ichar caption, filehandle f)=
	println @f, "PROC", caption
	printst(f, stprogram)
	println @f

	println @f, "Proc List:"
	ref procrec pp:=proclist
	while pp do
		symbol d:=pp.def
		fprintln @f, "#	#.# (#) Mod:", d, d.owner.name, d.name:"20jl", namenames[d.nameid], 
			d.moduleno
		pp:=pp.nextproc
	od
	println @f, "End\n"

	println @f, "DLL Proc List:"
	for i to ndllproctable do
		d:=dllproctable[i]
		fprintln @f, "#	#.# (#) Mod:", d, d.owner.name, d.name:"20jl", namenames[d.nameid], 
			d.moduleno
	od
	println @f, "End\n"
end

global proc showast(ichar filename)=
	filehandle f

	f:=fopen(filename, "w")
	return unless f

	println @f, "PROC", filename
	printcode(f, "")
	println @f
	fclose(f)
end

global proc printsymbol(ref tokenrec lp)=
	tokenrec l
	l:=lp^

	printf("%-18s", symbolnames[l.symbol])

	switch l.symbol
	when namesym then
		printstrn(l.symptr.name, l.symptr.namelen)

		if l.subcode then
			fprint " [#]", symbolnames[l.subcode]
		fi

	when intconstsym then
		case l.subcode
		when tint then print l.value, "int"
		when tword then print l.uvalue, "word"
		else print l.value
		esac

	when realconstsym then
		print l.xvalue

	when stringconstsym then
		print """"
		printstr(l.svalue)
		print """", strlen(l.svalue)

	when charconstsym then
		print "'"
		printstr(l.svalue)
		print "'"

	when assignsym, addrsym, ptrsym, rangesym, 
		andlsym, orlsym, eqsym, cmpsym, addsym, subsym, 
		mulsym, divsym, idivsym, iremsym, iandsym, iorsym, ixorsym, shlsym, shrsym, 
		minsym, maxsym, powersym then
		print symbolnames[l.symbol]
	elsif l.subcode then
		fprint "SUBCODE:", l.subcode
!	fprint "#", symbolnames[l.subcode]
	end

	println $, =lx.fileno

end

proc showtime(ichar caption, int t)=
	fprintln "# # ms # %", caption:"12jl", t:"5", (t*100.0)/compiletime:"5.1jr"
end

global proc showtimings=
	endclock:=os_clock()
	compiletime:=endclock-startclock
!
	showtime("Load:", 		loadtime)
	showtime("Parse:", 		parsetime)
	showtime("Resolve:", 	resolvetime)
	showtime("Type:", 		typetime)
	showtime("PCL:", 		pcltime)
	showtime("MCL:", 		mcltime)
!	showtime("SS:", 			sstime)
!	showtime("EXE:", 		exetime)
	println "-----------------------------"
	showtime("Total:", 		compiletime)
end

=== mm_export_dummy.m 0 0 19/34 ===
!hello

global proc writeexports(ichar basefile, modulename)=
end
=== mm_lex.m 0 0 20/34 ===
macro hashc(hsum,c)=hsum<<4-hsum+c
!macro hashw(hsum)=(hsum<<5-hsum)
macro hashw(hsum)=hsum

const maxstackdepth=20
[maxstackdepth]ref char lxstart_stack
[maxstackdepth]ref char lxsource_stack
[maxstackdepth]ref char lxsptr_stack
[maxstackdepth]int lxfileno_stack
[maxstackdepth]tokenrec lxnextlx_stack
[maxstackdepth]byte lximport_stack
global int sourcelevel=0
global int lximport

const cr	= 13
const lf	= 10
const tab	= 9

ref char lxsource
ref char lxstart
ref char lxsptr
int lxifcond

int lxfileno
global const hstsize	= 65536
!global const hstsize	= 65536*4
global const hstmask	= hstsize-1

global [0:hstsize]symbol hashtable
[0..255]byte namemap			!0/1/2 = other/name/name-upper

ichar u64maxstr="18446744073709551615"

global proc lex=
	int lena,lenb
	ref char p

	lx:=nextlx				!grab that already read basic token
	lx.sourceoffset:=lxstart-lxsource

	docase lexreadtoken(); nextlx.symbol
	when eolsym then
		if lx.symbol in [commasym, lsqsym, lbracksym] or
			symboloptypes[lx.symbol]=bin_op and 
			lx.symbol not in [maxsym, minsym] then
		else
			nextlx.symbol:=semisym
			nextlx.subcode:=1
			EXIT
		fi

	when kincludesym then
		doinclude()

	when namesym then
		case nextlx.subcode
		when unitnamesym then
			case lx.symbol
			when intconstsym then
				case nextlx.symptr.index
				when million_unit then lx.value *:= 1 million
				when billion_unit then lx.value *:= 1 billion
				else
					lxerror("Can't do this unit index")
				esac
				lx.subcode:=setinttype(lx.value)
			when realconstsym then
				lxerror("Unit suffix after float not implem")
			else
				nextlx.symbol:=namesym
				exit
			esac

		else
			nextlx.symbol:=namesym
			exit
		esac

	when rawxnamesym then
		nextlx.symbol:=namesym
		exit

	when insym then
		if lx.symbol=notlsym then
			lx.symbol:=notinsym
			lx.subcode:=1
		else
			exit
		fi

	else
		exit
	end docase

	nextlx.fileno:=lxfileno

end

global proc lexreadtoken=
!read next token into nextlx
	int c,hsum
	ref char sptr, lxsvalue
	int length,commentseen
	ref char p, q
	byte instr
	[256]char str

	nextlx.subcode:=0

	doswitch lxstart:=lxsptr; lxsptr++^
	when 'a'..'z','_','$' then
		lxsvalue:=lxsptr-1
	doname:
		hsum:=lxsvalue^

		sptr:=lxsptr

		docase namemap[c:=sptr++^]
		when 1 then
			hsum:=hsum<<4-hsum+c
		when 2 then
			(sptr-1)^:=c+' '
			hsum:=hsum<<4-hsum+c+' '
		else
			lxsptr:=sptr-1
			exit
		end docase

		if c='"' then
			if lxsvalue+1=ref char(lxsptr) then
				case c:=toupper(lxsvalue^)
				when  'F','R' then 
					readrawstring()
					return
				when  'S','B','A' then 
					readarraystring(c)
					return
				esac
			fi
		fi

		lookup(lxsvalue, lxsptr-lxsvalue, hashw(hsum))

		return

	when 'A'..'Z' then
		lxsvalue:=lxsptr-1
		lxsvalue^+:=32
		goto doname

	when '0'..'9' then
		lxstart:=lxsptr-1
		case lxsptr^
		when ')',cr,',',' ' then		!assume single digit decimal
			nextlx.symbol:=intconstsym
			nextlx.subcode:=tint
			nextlx.value:=lxstart^-'0'
		when 'x','X' then
			case lxstart^
			when '0' then		!0x
				++lxsptr
				readhex()
			when '2' then
				++lxsptr
				readbin()
			else
				lxerror("Bad base")
			esac
		else
			--lxsptr
			readdec()
		esac
		return

	when '!' then			!comment to eol
docomment:
		docase c:=lxsptr++^
		when 13 then
			++lxsptr
			exit
		when 10 then
			exit
		when 0 then
			--lxsptr
			exit
		end
		nextlx.symbol:=eolsym
		return

	when '#' then
		nextlx.symbol:=hashsym
		return

	when '\\' then			!line continuation

!two stages:
! 1: read chars until any eol chars (unless further '\' seen)
! 2: read until non-white space
		commentseen:=0
		docase lxsptr++^			!read until end of this line
		when cr then
!			++nextlx.pos
			++lxsptr				!skip lf
			exit
		when lf then
!			++nextlx.pos
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
		end docase
!eol seen: now skip 0 or more further eol chars, plus any white space (ie. multiple blank lines)

		docase lxsptr++^
		when cr then
			++lxsptr				!skip lf
		when lf then
		when ' ',tab then
		else
			--lxsptr
			exit
		end docase

	when '{' then
		nextlx.symbol:=lcurlysym
		return

	when '}' then
		nextlx.symbol:=rcurlysym
		return

	when '.' then
		case lxsptr^
		when '.' then				!.. or ...
			++lxsptr
			if lxsptr^='.' then
				++lxsptr
				nextlx.symbol:=ellipsissym
			else
				nextlx.symbol:=rangesym
				nextlx.subcode:=jmakerange		!helps treat as opsym which all have k-code as subcode
			fi
			return
		elsif lxsptr^ in '0'..'9' then			!real const: deal with this after the switch
			--lxsptr
LXERROR(".123 not done")
!			readrealnumber(nil,0,10)
			return
		else
			nextlx.symbol:=dotsym
			return
		esac

	when ',' then
		nextlx.symbol:=commasym
		return

	when ';' then
		nextlx.symbol:=semisym
		return

	when ':' then
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=assignsym
			nextlx.subcode:=jassign		!helps treat as opsym which all have k-code as subcode
		else
			nextlx.symbol:=colonsym
		esac
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
!		if lxsptr^='|' then
!			++lxsptr
!			nextlx.symbol:=dbarsym
!		else
			nextlx.symbol:=barsym
!		fi
		return

	when '^' then
		nextlx.symbol:=ptrsym
		return

	when '@' then
!		if lxsptr^='@' then
!			++lxsptr
!			nextlx.symbol:=datsym
!		else
			nextlx.symbol:=atsym
!		fi
		return

	when '?' then
		p:=str; q:=lxsptr+1
		while q^ not in [cr, lf, 0] do
			p++^:=q++^
		od
		p^:=0

		nextlx.svalue:=pcm_copyheapstring(str)
		nextlx.symbol:=questionsym
		return


	when '~' then
!		nextlx.symbol:=curlsym
!		return

	when '+' then
		nextlx.symbol:=addsym
		if lxsptr^='+' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=kincrto
			return
		fi
		return

	when '-' then
		nextlx.symbol:=subsym
		case lxsptr^
		when '-' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=kdecrto
			return
		when '>' then
			++lxsptr
			nextlx.symbol:=pipesym
			return
		esac
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
		else
			nextlx.symbol:=eqsym
			nextlx.subcode:=eq_cc
		esac
		return

	when '<' then
		nextlx.symbol:=cmpsym
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.subcode:=le_cc
		when '>' then
			++lxsptr
			nextlx.subcode:=ne_cc
		when '<' then
			++lxsptr
			nextlx.symbol:=shlsym
		else
			nextlx.subcode:=lt_cc
		esac
		return

	when '>' then
		nextlx.symbol:=cmpsym
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=cmpsym
			nextlx.subcode:=ge_cc
		when '>' then
			++lxsptr
			nextlx.symbol:=shrsym
		else
			nextlx.symbol:=cmpsym
			nextlx.subcode:=gt_cc
		esac
		return

	when '&' then
		case lxsptr^
			when '&' then
			++lxsptr
			nextlx.symbol:=daddrsym
			nextlx.subcode:=jdaddrvv
		when '.' then
			++lxsptr
			nextlx.symbol:=anddotsym
			nextlx.subcode:=0
		else
			nextlx.symbol:=addrsym
			nextlx.subcode:=jaddrof
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
		nextlx.symbol:=eolsym
		return
	when lf then			!only lfs not preceded by cr
		nextlx.symbol:=eolsym
		return

	when 0 then
		if sourcelevel then
			unstacksource()
			RETURN
		else
			nextlx.symbol:=eofsym
			--lxsptr
			return
		fi

	else
		lxerror("Unknown char")
!		nextlx.symbol:=errorsym
		return

	end doswitch

end

global proc lexsetup=
!do one-time setup:
! clear the hash table and populated it with reserved words

	inithashtable()
end

global proc printstrn(ichar s, int length)=
	if length then
		print length:"v",s:".*"
	fi
end

proc readrawstring=
!positioned at " of F"
!read raw string
	ichar dest
	int c

	nextlx.symbol:=stringconstsym
	nextlx.svalue:=++lxsptr

	dest:=lxsptr				!form string into same buffer

	docase c:=lxsptr++^
	when '"' then
		if lxsptr^='"' then		!repeated, assume embedded term char
			dest++^:='"'
			++lxsptr
		else			!was end of string
!			(lxsptr-1)^:=0
			exit
		fi
	when cr,lf,0 then
		lxerror("Raw string not terminated")
		--lxsptr
		exit
	else
		dest++^:=c
	end docase
	nextlx.slength:=lxsptr-nextlx.svalue
	nextlx.svalue:=pcm_copyheapstringn(nextlx.svalue, nextlx.slength)
end

proc lookup(ref char name, int length, hashindex)=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!in either case, lx.symptr set to entry where name was found, or will be stored in
	int wrapped, j
	symbol d

	j:=hashindex iand hstmask

	d:=hashtable[j]
	wrapped:=0

	do
		if d=nil then exit fi

!		if (n:=d.namelen)=length and memcmp(d.name,name,n)=0 then	!match
		if d.namelen=length and memcmp(d.name,name,length)=0 then	!match
			nextlx.symptr:=d
			nextlx.symbol:=d.symbol
			nextlx.subcode:=d.subcode
			return
		fi

		if ++j>=hstsize then
			if wrapped then
				abortprogram("HASHTABLE FULL")
			fi
			wrapped:=1
			j:=0
		fi
		d:=hashtable[j]
	od

!exit when not found; new name will go in entry pointed to by lxsymptr

	d:=pcm_allocnfz(strec.bytes)

	hashtable[j]:=d

	d.name:=pcm_copyheapstringn(name,length)
	d.namelen:=length
	d.symbol:=namesym

	nextlx.symptr:=d
	nextlx.symbol:=d.symbol
!	nextlx.subcode:=d.subcode
end

func lookupsys(ref char name)int=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!return 1 (found) or 0 (not found)
!in either case, lx.symptr set to entry where name was found, or will be stored in
	int j, wrapped, hashvalue

	j:=gethashvaluez(name) iand hstmask

	lx.symptr:=hashtable[j]
	wrapped:=0

	do
		if lx.symptr=nil then
			exit
		elsif eqstring(lx.symptr.name,name) then	!match
			println "Lex dupl name:",name
			stop 1 
!			lxerror("sys dupl name?")
		fi

		if ++j>=hstsize then
			if wrapped then
				abortprogram("SYS:HASHTABLE FULL")
			fi
			wrapped:=1
			j:=0
		fi
		lx.symptr:=hashtable[j]
	od

!exit when not found; new name will go in entry pointed to by lxsymptr
	lx.symptr:=pcm_allocnfz(strec.bytes)
	hashtable[j]:=lx.symptr

	lx.symptr.name:=name				!assume can be shared (stored in a table)
	lx.symptr.namelen:=strlen(name)
	lx.symptr.symbol:=namesym			!usually replaced with actual symbol details

	return 0
end

func gethashvaluez(ichar s)int=
!get identical hash func to that calculated by lexreadtoken
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


	for i:=1 to stnames.len do
		lookupsys(stnames[i])

		lx.symptr.symbol:=stsymbols[i]

		case stsymbols[i]
		when unitnamesym then
			lx.symptr.index:=stsubcodes[i]
			lx.symptr.subcode:=stsymbols[i]
			lx.symptr.symbol:=namesym		!masquerades as normal identifier
		else
			lx.symptr.subcode:=stsubcodes[i]
		esac
	od
end

global proc printhashtable=
	println "Hashtable:"

!	for i:=0 to hstsize-1 do
!		if hashtable[i] then
!		fi
!	od
end

global proc addreservedword(ichar name,int symbol,subcode, regsize=0)=
	lookupsys(name)

	lx.symptr.symbol:=namesym
	lx.symptr.subcode:=symbol
	lx.symptr.index:=subcode

	lx.symptr.regsize:=regsize
end

proc doinclude=
	ichar file
	ifile pf

	lexreadtoken()
	if nextlx.symbol<>stringconstsym then lxerror("include: string expected") fi
	file:=nextlx.svalue
	convlcstring(file)
	file:=addext(file,"m")		!add in extension if not present; assume same as source

	pf:=getsupportfile(file, path:sources[lxfileno].path)
	lexreadtoken()
	stacksource(pf.fileno)
end

global proc startlex(ifile file)=
!start processing one of the file in sourcefile tables as source code
!assume it is a complete header or module

	lxsource:=lxsptr:=file.text

	nextlx.pos:=0
	lxfileno:=file.fileno

	nextlx.symbol:=semisym
	nextlx.subcode:=0
end

global func addnamestr(ichar name)ref strec=
	tokenrec oldlx
	ref strec symptr

	oldlx:=nextlx
	lookup(name,strlen(name), gethashvaluez(name))
	symptr:=nextlx.symptr
	nextlx:=oldlx

	return symptr
end

global proc ps(ichar caption)=
	print "PS:",caption,,": "
	printsymbol(&lx)
end

global proc psnext(ichar caption)=
	print caption,,": "
	printsymbol(&nextlx)
end

global proc psx(ichar caption)=
	print caption,,": "
	printsymbol(&lx)
	print "	"
	printsymbol(&nextlx)
end

global proc stacksource(int fileno, isimport=0)=
!introduce new source level for macros or include files
!not used for main source

	if sourcelevel>=maxstackdepth then
		lxerror("Include file/macro overflow")
	fi
	++sourcelevel
	lxstart_stack[sourcelevel]:=lxstart
	lxsource_stack[sourcelevel]:=lxsource
	lxsptr_stack[sourcelevel]:=lxsptr
	lxfileno_stack[sourcelevel]:=lxfileno
	lxnextlx_stack[sourcelevel]:=nextlx
	lximport_stack[sourcelevel]:=lximport
	lximport:=isimport

	lxsource:=lxsptr:=sources[fileno].text

	nextlx.pos:=0
	lxfileno:=fileno

	nextlx.symbol:=semisym
	nextlx.subcode:=0
end

global proc unstacksource=
	if sourcelevel>0 then			!check that some source is stacked
		lxstart:=lxstart_stack[sourcelevel]
		lxsource:=lxsource_stack[sourcelevel]
		lxsptr:=lxsptr_stack[sourcelevel]
		nextlx:=lxnextlx_stack[sourcelevel]
		lxfileno:=lxfileno_stack[sourcelevel]
		lximport:=lximport_stack[sourcelevel]

		--sourcelevel
	fi
end

proc readarraystring(int prefix)=
	++lxsptr
	lxreadstring('"')

	if prefix='S' then
		nextlx.subcode:='S'
!CPL "RAX/STR"
	else
		--NEXTLX.SLENGTH
		nextlx.subcode:='B'
!CPL "RAX/BIN"
	fi
end

func setinttype(u64 a)int=
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

	while namemap[c:=lxsptr++^] do
		hsum:=hsum<<4-hsum+c
	od
	--lxsptr

	length:=lxsptr-nextlx.svalue

	if length=0 then
		lxerror("Bad ` name")
	fi
	lookup(nextlx.svalue,length, hashw(hsum))
	nextlx.symbol:=rawxnamesym

	return
end

proc lxerror_s(ichar mess,s)=
	lxerror(mess)
end

proc lxreadstring(int termchar)=
!start from char just after " or ' (termchar will be " or ')

	ichar s,t
	int c, d, length, hasescape, a, n
	[8]char str

	if termchar='"' then
		nextlx.symbol:=stringconstsym
	else
		nextlx.symbol:=charconstsym
		nextlx.subcode:=tint
	fi

!do a first pass that terminates length of final string
	length:=0
	hasescape:=0
	t:=nil

	for pass:=1 to 2 do
		s:=lxsptr
		do
			case c:=s++^
			when '\\' then			!escape char
				hasescape:=1
				c:=s^
				if c>='A'  and c<='Z' then c+:=' ' fi
				++s
				case c
				when 'a' then			!bell ('alert')
					c:=7
				when 'b' then			!backspace
					c:=8
				when 'c','r' then		!carriage return
					c:=cr
				when 'e' then			!escape
					c:=27
				when 'f' then			!formfeed
					c:=12
				when 'h' then
					while s^ <> '\\' do
						c:=readhexcode(&s,2,1)
						if pass=2 then
							t^:=c
						fi
						++t
					od
					++s
					--t					!will overwrite last byte

				when 'l','n' then		!linefeed, or linux/c-style newline
					c:=lf
				when 't' then			!tab
					c:=9
				when 'u','v' then		!reserved for unicode, like \x but with 4 hex digits
					t +:= getutf8(readhexcode(&s, (c='u'|4|6)), (pass=2|t|nil))
					nextloop

				when 'w' then			!windows-style cr-lf
					if pass=2 then
						t^:=cr
					fi
					++t
					c:=lf
				when 'x' then	!2-digit hex code follows
					c:=readhexcode(&s,2)
				when 'y' then			!CCI/SM backwards tab
					c:=16
				when 'z' then			!null (not fully supported in code)
					c:=0
				elsecase c
				when '"' then			!embedded double quote
					c:='"'
				when '\\' then
					c:='\\'
				when '\'' then			!embedded single quote
					c:='\''
				when '0' then
					c:=0
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
HASESCAPE:=1
			when cr,lf,0 then
				lxerror("String not terminated")
			esac

			if pass=2 then
				t^:=c
			fi
			++t

		od

		if pass=1 then
			length:=int(t)
			nextlx.slength:=length+1
!CPL "LXREADSTRING", LENGTH, NEXTLX.SLENGTH
			if hasescape then
				nextlx.svalue:=t:=pcm_alloc(length+1)
			elsif length=0 then
				nextlx.svalue:=""
				lxsptr:=s
				return
			else
				nextlx.svalue:=pcm_copyheapstringn(lxsptr,length)
				lxsptr:=s

				return
			fi

		else
			t^:=0
			lxsptr:=s
		fi
	od
end

func readhexcode(ref ref char s, int n, sp=0)int a=
!read n hex digits from from char ptr, and step ptr in the caller
	int c
	a:=0
	for i to n do

		if sp and i.odd then
			repeat
				c:=(s^)++^
			until c<>' '
		else
			c:=(s^)++^
		fi

		if c in 'A'..'F' then
			a:=a*16+c-'A'+10
		elsif c in 'a'..'f' then
			a:=a*16+c-'a'+10
		elsif c in '0'..'9' then
			a:=a*16+c-'0'
!		elsif c='\\' then
!			--(s^)
!			exit
		else
			lxerror("Bad hex digit")
		fi
	od
	a
end

func getutf8(int c, ref char s)int n =
!convert unicode char c to utf8 sequence at s, consisting of 1-4 bytes, and
!return the number of bytes. s will be zero-terminated
!On error, return zero
	[16]char str
	if s=nil then s:=str fi

	if c<=0x7F then
		n:=1
		s++^:=c

	elsif c<=0x7FF then
		n:=2
		s++^:=2x110_00000 + c.[10..6]
		s++^:=2x10_000000 + c.[5..0]

	elsif c<=0xFFFF then
		n:=3
		s++^:=2x1110_0000 + c.[15..12]
		s++^:=2x10_000000 + c.[11..6]
		s++^:=2x10_000000 + c.[5..0]
	elsif c<=0x10FFFF then
		n:=4
		s++^:=2x11110_000 + c.[20..18]
		s++^:=2x10_000000 + c.[17..12]
		s++^:=2x10_000000 + c.[11..6]
		s++^:=2x10_000000 + c.[5..0]
	else
		n:=0
	fi

	s^:=0
	n
end

proc readdec=
	int c
	ref char dest, destend, pstart
	int islong, length
	[1024]char str
	word a

	islong:=0

	pstart:=lxsptr

	dest:=&.str
	destend:=dest+str.len-10
	a:=0

	do
		if (c:=lxsptr++^) in '0'..'9' then
			a:=a*10+c-'0'
			dest++^:=c
		elsecase c
		when 'e','E' then
			lxsptr:=pstart
			readreal()
			return
		when '.' then
			if lxsptr^<>'.' then
				lxsptr:=pstart
				readreal()
				return
			fi
			--lxsptr
			exit

		when '_','\'' then
		when 'l','L' then
			nodecimal()

		when 'b','B' then
			length:=dest-&.str
			if length>64 then lxerror("bin overflow") fi
			dest:=&.str
			a:=0
			to length do
				if dest^>='2' then lxerror("bad bin digit") fi
				a:=a*2+dest++^-'0'
			od
			finish

		else
			--lxsptr
			exit
		fi

		if dest>=destend then lxerror("Numlit too long") fi
	end
	length:=dest-&.str

	if length>20 or length=20 and strncmp(str,u64maxstr,20)>0 then
		nodecimal()
	fi

finish:
	nextlx.symbol:=intconstsym
	nextlx.subcode:=setinttype(a)
	nextlx.value:=a
end

proc readhex=
	int c
	ref char dest, destend, pstart
	int length
	[1024]byte str
	word a

	pstart:=lxsptr

	dest:=&.str
	destend:=dest+str.len-10
	a:=0

	do
		if (c:=lxsptr++^) in '0'..'9' then
			a:=a*16+c-'0'
			dest++^:=c

		elsif c in 'A'..'F' then
			dest++^:=c
			a:=a*16+c-'A'+10
		elsif c in 'a'..'f' then
			dest++^:=c-32
			a:=a*16+c-'a'+10

		elsecase c
		when '_','\'' then
		when 'l','L' then
			nodecimal()

		when '.' then
			--lxsptr
			exit

		else
			--lxsptr
			exit
		fi

		if dest>=destend then lxerror("Numlit too long") fi
	end
	length:=dest-&.str

	if length>16 then
		LXERROR("MAKEDEC")
		return
	fi

	nextlx.symbol:=intconstsym
	nextlx.subcode:=setinttype(a)
	nextlx.value:=a
end

proc readbin=
	int c
	ref char dest, destend, pstart
	int length
	[1024]byte str
	word a

	pstart:=lxsptr

	dest:=&.str
	destend:=dest+str.len-10
	a:=0

	do
		case c:=lxsptr++^
		when '0', '1' then
			a:=a*2+c-'0'
			dest++^:=c

		when '_','\'' then
		when 'l','L' then
			nodecimal()

		when '.' then
			--lxsptr
			exit

		elsif c in '2'..'9' then
			lxerror("bin bad digit")
		else
			--lxsptr
			exit
		esac

		if dest>=destend then lxerror("bin overflow") fi
	end
	length:=dest-&.str

	if length>64 then
		nodecimal()
	fi

	nextlx.symbol:=intconstsym
	nextlx.subcode:=setinttype(a)
	nextlx.value:=a
end

proc readreal=
!at '.', or had been in middle of int where . or e were seen, back at the start

	int c,negexpon,dotseen,length, fractlen, expon, expseen
	real x
	[1024]char str
	ichar dest, destend
	u64 a

	dest:=&.str
	destend:=dest+str.len-100
	length:=negexpon:=dotseen:=expseen:=expon:=fractlen:=0

	do
		if (c:=lxsptr++^) in '0'..'9' then
			dest++^:=c
			++length
			if dotseen then ++fractlen fi
		elsecase c
		when '.' then
			if dotseen then --lxsptr; exit fi
			dotseen:=1
			dest++^:=c

		when 'e','E' then
			if expseen then lxerror("double expon") fi
			expseen:=1
			dest++^:=c
			while lxsptr^=' ' do ++lxsptr od
			if lxsptr^ in ['+','-'] then
				if lxsptr^='-' then negexpon:=1 fi
				dest++^:=lxsptr++^
			fi

			expon:=0
			do
				if (c:=lxsptr++^) in '0'..'9' then
					expon:=expon*10+c-'0'
					dest++^:=c
					if dest>=destend then lxerror("expon?") fi
				elsecase c
				when '_','\'' then
				when 'l','L' then
					dest^:=0
					nodecimal()
					return
				else
					--lxsptr
					exit all
				fi
			end

		when '_','\'' then

		when 'l','L' then
			nodecimal()
			return
		else
			--lxsptr
			exit
		fi

		if dest>=destend then lxerror("r64lit too long") fi
	end
	dest^:=0

	if expseen and expon>=0 and not dotseen then		!read as integer
		a:=0
		for i to length do				!digits already range checked
			a:=a*10+str[i]-'0'
		od
		to expon do
			a:=a*10
		od
		nextlx.symbol:=intconstsym
		nextlx.subcode:=setinttype(a)
		nextlx.value:=a
		return
	fi


!------------------------------------------------------------
! Fast way to convert for ordinary numbers (1e100 migt be slower!)
!------------------------------------------------------------
	if negexpon then expon:=-expon fi
	expon-:=fractlen
	x:=0.0

	for i:=1 to length+dotseen do		!digits already range-checked
		c:=str[i]
		if c<>'.' then
			x:=x*10.0+c-'0'
		fi
	od

	if expon>=0 then
		to expon do
			x*:=10.0
		od
	else
		to -expon do
			x/:=10.0
		od
	fi

	nextlx.xvalue:=x
!------------------------------------------------------------
! Best way to covert: more accurate representation, but slower
!------------------------------------------------------------
!	nextlx.xvalue:=strtod(str,nil)
!------------------------------------------------------------

	nextlx.symbol:=realconstsym
	nextlx.subcode:=treal

!IF EXPSEEN AND NOT DOTSEEN THEN
!	CPL "READREAL NO DOT", X
!FI
end

proc nodecimal=
	lxerror("Decimal not ready")
end

proc start=
	for c in namemap.bounds do
		if c in 'a'..'z' or c in '0'..'9' or c in ['_','$'] then
			namemap[c]:=1
		elsif c in 'A'..'Z' then
			namemap[c]:=2				!upper case
		fi
	od
end

=== mm_lib.m 0 0 21/34 ===
int autotypeno=0
global int nextavindex=0
int nextsvindex=0

strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar

const int unitheapsize=32768
ref unitrec unitheapptr=nil
int remainingunits=0

strbuffer sbuffer
global ref strbuffer dest=&sbuffer

ref strbuffer jdest

global ichar framevarname			!normally nil, set to frame var def to display in comment

global macro isnum(m)  = m <= tlastnum
global macro isnumx(m) = m <= tlastnum
global macro isnumf(m) = m <= tr32
global macro isnumi(m) = (m in [ti64, tu64, tc64])
global macro isbool(m) = (m in [tbool8, tbool64])

global macro isint(m) = m>tr32

global func newstrec:symbol=
	symbol p

!	p:=pcm_alloc(strec.bytes)
!	clear p^

	p:=pcm_allocnfz(strec.bytes)

	p.pos:=lx.pos
	p.moduleno:=currmoduleno
	p.subprogno:=moduletosub[currmoduleno]
	return p
end

global func getduplnameptr(symbol owner,symptr,int id)symbol=
!create duplicate stentry
!owner is the owner ST
!symptr points to the current generic entry for the name (nameid=0)
!id is the desired nameid
!new entry is created, and added to the dupl chain for this name
!return pointer to new strec; this can be stored directly in a -def unit
!but such nameptrs are not allowed elsewhere; it must be wrapped in a knameunit
	symbol p

	p:=newstrec()

	p.name:=symptr.name
	p.namelen:=symptr.namelen
	p.symbol:=namesym
	p.owner:=owner
	p.nameid:=id

!IF ID NOT IN [FRAMEID, PARAMID] THEN
	p.nextdupl:=symptr.nextdupl
	symptr.nextdupl:=p
!FI
	p.firstdupl:=symptr

	return p
end

global proc adddef(symbol owner,p)=
!add new st def p, to existing deflist of owner
!assumes p already has a .owner link to owner, but is not yet part of owner's deflist
!pgeneric points to the 'generic' entry for this name in the main hash table.
!this is needed as it contains the head of the dupl list for this name (linking
!all instances of this name).
!Usually the dupl list is checked to ensure that there are no existing names
!with this same owner. (pgeneric can be nil to avoid this check.)
!ASSUMES THAT P IS LAST THING ADDED TO HEAD OF DUPLLIST (just after PGENERIC)
	symbol q

	if q:=p.nextdupl then
		if q.owner=owner then
			cpl q.name,"in",owner.name
			serror("Duplicate name")
		fi
	fi

	if owner.deflist=nil then			!first def
		owner.deflist:=p
	else
		owner.deflistx.nextdef:=p
	fi

	owner.deflistx:=p
end

global func createname(symbol p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=jname
	u.def:=p

	return u
end

global func createunit0(int tag)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=tag
	return u
end

global func createunit1(int tag, ref unitrec p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=tag
	u.a:=p
	return u
end

global func createunit2(int tag, ref unitrec p,q)ref unitrec=
	ref unitrec u

	u:=allocunitrec()

	u.tag:=tag
	u.a:=p
	u.b:=q
	return u
end

global func createunit3(int tag, ref unitrec p,q,r)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=tag
	u.a:=p
	u.b:=q
	u.c:=r
	return u
end

global proc insertunit(unit p,int tag)=
!wrap extra unit around p, with given tag
!p itself is effectively changed
	unit q,nextunit
	int mode

	q:=allocunitrec()
	q^:=p^
	mode:=q.mode
	nextunit:=q.nextunit
	q.nextunit:=nil

	clear p^

	p.tag:=tag
	p.pos:=q.pos
	p.a:=q
	p.mode:=mode
	p.nextunit:=nextunit
	p.resultflag:=q.resultflag
end

global proc deleteunit(unit p,q)=
!delete p, replace by q, so that what was addressed by p now contains q
	unit r:=p.nextunit
	p^:=q^
	p.nextunit:=r
end

global func createconstunit(u64 a, int t)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=jconst
	u.value:=a
	u.mode:=t

	u.isconst:=1
	return u
end

global func createstringconstunit(ichar s, int length)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=jconst
	u.svalue:=s
	u.mode:=trefchar
	u.isastring:=1

	if length=-1 then
		u.slength:=strlen(s)+1
	else
		u.slength:=length
	fi
	return u
end

global func newtypename(symbol a,b)int=
	if ntypenames>=maxtypename then
		serror("Too many type names")
	fi
	++ntypenames
	typenames[ntypenames].defa:=a		!leave .owner/.pmode to be filled in
	typenames[ntypenames].defb:=b		!used type's mode is used

	typenamepos[ntypenames].pos:=lx.pos

	return -ntypenames
end

global func createusertype(symbol stname)int=
!create new, named user type
	if ntypes>=maxtype then
	cpl ntypes,stname.name
		serror("Too many types")
	fi

	++ntypes
	ttname[ntypes]:=stname.name

	ttnamedef[ntypes]:=stname
	ttbasetype[ntypes]:=tvoid
	ttlineno[ntypes]:=lx.pos

	stname.mode:=ntypes

	return ntypes
end

global func createusertypefromstr(ichar name)int=
!create new, named user type
	symbol stname

	stname:=getduplnameptr(stmodule,addnamestr(name),typeid)
	return createusertype(stname)
end

global func getrangelwbunit(ref unitrec p)ref unitrec=
	if p.tag=jmakerange then
		return p.a
	else
		p:=createunit1(jprop,p)
		p.propcode:=kklwb
		return p
	fi
end

global func getrangeupbunit(ref unitrec p)ref unitrec=
	if p.tag=jmakerange then
		return p.b
	else
		p:=createunit1(jprop,p)
		p.propcode:=kkupb
		return p
	fi
end

global func createarraymode(symbol owner,int target,unit dimexpr, int typedefx)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
	int k,m

	if typedefx=0 then		!anon type
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=tarray
	ttlower[m]:=1
	ttdimexpr[m]:=dimexpr
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner

	ttisblock[m]:=1

	return m
end

func sameunit(unit p,q, symbol powner=nil, qowner=nil)int=
!p are q are units just parses; no name resolving or type checking
!do a simple check to see if they are the same unit
	if p=q then return 1 fi
	if p=nil or q=nil then return 0 fi

	if p.tag<>q.tag then return 0 fi

	case p.tag
	when jconst then
		return p.value=q.value
	when jmakerange,jkeyvalue then
		return sameunit(p.a, q.a) and sameunit(p.b, q.b)
	when jname then
		if p.def=q.def and powner=qowner then
			return 1
		fi
	esac

	return 0

end

global func createarraymodek(symbol owner,int target,int lower,length, int typedefx)int=
!lower is lower bound of array
	int atype,m

	atype:=tarray

	if typedefx=0 then		!anon type
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=atype
	ttlower[m]:=lower
	ttlength[m]:=length
	IF TARGET<0 THEN
		SERROR("CREATEARRAYMODEK/TARGET NOT RESOLVED")
	FI
	ttsize[m]:=length*ttsize[target]

	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner
	ttisblock[m]:=1

	return m
end

global func nextautotype:ichar=
	static [32]char str

	print @&.str,"$T",,++autotypeno
	return &.str
end

global func createslicemode(symbol owner,int slicetype,target,unit dimexpr, int typedefx=0)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
	int m

	if typedefx=0 then		!anon type
!	for k:=tlast to ntypes do
!		if ttusercat[k]=0 and ttbasetype[k]=atype and tttarget[k]=target and \
!			ttlower[k]=lower and ttlength[k]=length then
!			return k
!		fi
!	od
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=slicetype
	if dimexpr then
		ttdimexpr[m]:=dimexpr
	else
		ttlower[m]:=1
	fi
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner
	ttisblock[m]:=1

	return m
end

global func createslicemodek(symbol owner,int target,lower, int typedefx=0)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
	int m

	if typedefx=0 then		!anon type
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=tslice
	ttlower[m]:=lower
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner
	ttisblock[m]:=1

	return m
end

global func createrefmode(symbol owner,int target,typedefx=0)int=
	int k,m
!	int a,b

	if typedefx=0 then		!anon type
		for k:=tlast to ntypes when ttisref[k] do
			if tttarget[k]=target then
				return k
			fi
		od
!		FI
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	storemode(owner,target,tttarget[m])
	ttbasetype[m]:=tref
	ttsize[m]:=ttsize[tref]
	ttisref[m]:=1

	return m
end

global func createrefprocmode(symbol owner,stproc, paramlist,int kwd, prettype,typedefx)int=
!create a ref proc mode; (can't create a proc mode by itself, as it's meaningless)
	int m, mproc

	mproc:=createusertype(stproc)
	stproc.paramlist:=paramlist

	stproc.mode:=prettype
	ttbasetype[mproc]:=tproc

!don't bother looking for similar proc sig; each one is unique
	if typedefx=0 then		!anon type
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	tttarget[m]:=mproc
	ttbasetype[m]:=tref

	ttsize[m]:=ttsize[tref]
	ttisref[m]:=1

	return m
end

global proc copyttvalues(int dest, source)=
	ttsigned[dest]		:= ttsigned[source]
	ttisreal[dest]		:= ttisreal[source]
	ttisinteger[dest]	:= ttisinteger[source]
	ttisshort[dest]		:= ttisshort[source]
	ttisref[dest]		:= ttisref[source]
	ttisblock[dest]		:= ttisblock[source]
end

global func getdottedname(symbol p)ichar=
!build full dotted name for st item p
	static [256]char str
	[256]char str2
	symbol owner

	strcpy(&.str,p.name)
	owner:=p.owner
	while owner and owner.nameid<>programid do
		strcpy(&.str2,&.str)
		strcpy(&.str,owner.name)
		strcat(&.str,".")
		strcat(&.str,&.str2)
		owner:=owner.owner
	od
	return &.str
end

global func getavname(symbol owner,int id=frameid)symbol=
!create auto-var name and return pointer to st entry
	symbol p
	[32]char str
	ichar name

	if id=frameid and owner.nameid<>procid then
		serror("Auto frame not in proc")
	fi

	if id=frameid then
		print @&.str,"av_",,++nextavindex
	else
		print @&.str,"sv_",,++nextsvindex
	fi

	name:=pcm_copyheapstring(&.str)
	addnamestr(name)

	p:=getduplnameptr(owner,addnamestr(name),id)
	p.used:=1

	p.mode:=tint

	adddef(owner,p)
	return p
end

global proc unionstr_clear(ref uflagsrec u)=
	((ref u64(u))^:=0)		!clear flags and length togetjer
end

global proc unionstr_append(ref uflagsrec u, int c)=
	if u.ulength=(u.codes.len-1) then
		serror("Uflags overflow/a")
	fi
	++u.ulength
	u.codes[u.ulength]:=c
end

global proc unionstr_concat(ref uflagsrec u, v)=
	int ulen,vlen,i

	ulen:=u.ulength
	vlen:=v.ulength
	if ulen+vlen>u.codes.len then
		serror("Uflags overflow/c")
	fi
	for i:=1 to vlen do
		u.codes[i+ulen]:=v.codes[i]
	od
	u.ulength:=ulen+vlen
end

global func unionstr_last(ref uflagsrec u)int=
	if u.ulength then
		return u.codes[u.ulength]
	fi
	return 0 
end

global proc unionstr_copy(ref uflagsrec u,v)=
	memcpy(u,v,uflagsrec.bytes)
end

global func createrecordmode(symbol owner,int typedefx)int=
!typedef is nil, or an empty moderec belonging to a user type
!owner is an strec for the name def:
! * user-supplied name belonging to the typedef (same as typedef.namedef)
! * user-supplied optional name from a stand-alone enum typespec
! * auto-generated name
	int m

	if typedefx=0 then
		m:=createusertype(owner)
	else
		m:=typedefx
	fi
	ttbasetype[m]:=trecord
	ttusercat[m]:=1
	ttisblock[m]:=1

	return m
end

global func createtuplemode(symbol owner,[]int &elements,int elementslen, typedefx)int=
	int m

	if typedefx=0 then
		m:=createusertype(owner)
	else
		m:=typedefx
	fi
	ttbasetype[m]:=ttuple
	ttusercat[m]:=1
	ttlength[m]:=elementslen
	ttmult[m]:=pcm_alloc(elementslen*i32.bytes)
	for i to elementslen do
		storemode(owner,elements[i],ttmult[m,i])
	od

	return m
end

global func strexpr(ref unitrec p)ref strbuffer=
!vx_makestring("",exprstr)
	gs_init(exprstr)

	jevalx2(exprstr,p)
	return exprstr
end

global proc jevalx2(ref strbuffer dest, ref unitrec p)=			!JEVAL
	jdest:=dest
	jevalx(p)
end

global proc jevalx(ref unitrec p)=			!JEVAL
!p represents an expression. It can be a unitrec only, not a list (lists only occur inside
!kmakelist and kmakeset units, which specially dealt with here)
!dest is a destination string. Special routines such as gs_additem() are used, which take care
!of separators so that successive alphanumeric items don't touch
	unit q,a,b
	[500]char str

	if p=nil then
		return
	fi

	a:=p.a
	b:=p.b

	case p.tag
	when jconst then

		case ttbasetype[p.mode]
		when ti32,ti64,ti8,ti16 then
			getstrint(p.value,&.str)
		when tu32,tu64,tu8,tu16 then
			strcpy(&.str,strword(p.uvalue))
		when tc8,tc64 then
			str[1]:=p.uvalue
			str[0]:=0

		when treal,tr32 then
			print @&.str,p.xvalue
		when tref then
			if p.mode=trefchar and p.isastring then
				if p.slength>str.len/2 then
					strcpy(&.str,"LONGSTR)")
				else
					convertstring(p.svalue,&.str)
				fi
				jadditem("""")
				jadditem(&.str)
				jadditem("""")
				return
			else
				print @&.str,ref void(p.value)
			fi
		else
			strcpy(&.STR,"<EVAL/CONST PROBABLY VOID>")
		esac
		jadditem(&.str)

	when jname then
		jadditem(p.def.name)

	when jbin,jcmp then

		strcpy(&.str,pclnames[p.pclop])
		jadditem("(")
		jevalx(a)
		jadditem(&.str)
		jevalx(b)
		jadditem(")")

	when junary, jistruel, jnotl then

		strcpy(&.str,pclnames[p.pclop])
		jadditem(&.str)
		jadditem("(")

		if a.tag=jtypeconst then
			jadditem(STRMODE(a.value))
		else
			jevalx(a)
		fi
		jadditem(")")

	when jprop then

		strcpy(&.str,propnames[p.propcode])
		jadditem(&.str)
		jadditem("(")
		jevalx(a)
		jadditem(")")

	when jcall then
		jevalx(a)
		jadditem("(")

		q:=b
		while q do
			jevalx(q)
			q:=q.nextunit
			if q then jadditem(",") fi
		od
		jadditem(")")

	when jindex,jdotindex,jslice,jdotslice then
		jevalx(a)
		if p.tag=jdotindex or p.tag=jdotslice then
			jadditem(".")
		fi
		jadditem("[")
		jevalx(b)
		jadditem("]")

	when jdot then
		jevalx(a)
		jadditem(".")
		jevalx(b)

	when jmakelist then
		jadditem("(")

		q:=a
		while q do
			jevalx(q)
			q:=q.nextunit
			if q then jadditem(",") fi
		od
		jadditem(")")

	when jmakerange then
		jadditem("(")
		jevalx(a)
		jadditem("..")
		jevalx(b)
		jadditem(")")

	when jassign then
		jevalx(a)
		jadditem(":=")
		jevalx(b)

	when jif then
		jadditem("(")
		jevalx(a)
		jadditem("|")
		jevalx(b)
		jadditem("|")
		jevalx(p.c)
		jadditem(")")

	when jtypeconst then
		jadditem(strmode(p.mode))

	when jconvert,jtypepun then

		jadditem(strmode(p.convmode))
		if p.tag=jtypepun then
			jadditem("@")
		fi
		jadditem("(")
		jevalx(a)
		jadditem(")")

	when jshorten then

		jadditem("shorten(")
		jevalx(a)
		jadditem(")")
	when jautocast then

		jadditem("cast(")
		jevalx(a)
		jadditem(")")
	when jkeyvalue then
		jevalx(a)
		jadditem(":")
		if b then
			jevalx(p.b)
		else
			jaddstr("-")
		fi

	when jptr then
		jevalx(a)
		jadditem("^")

	when jblock then
		jadditem("<JBLOCK>")

	when jnull then
		jaddstr("<nullunit>")

	when jaddrof then
		jadditem("&")
		jevalx(a)
		if b then
			jaddstr("+")
			gs_strint(jdest,b.value)
		fi

	when jaddroffirst then
		jadditem("&.")
		jevalx(a)

	when jtypestr then
		jadditem("TYPESTR(")
		jevalx(a)
		jadditem(")")

!	when jcvlineno, jcvfilename, jcvmodulename then
	when jcvfilename, jcvmodulename then
		jaddstr("$")
		jaddstr(jtagnames[p.tag]+1)

	when jbitfield then
		jevalx(a)
		jaddstr(".")
		jaddstr(bitfieldnames[p.bitopindex])

	when jfmtitem then
		jevalx(a)
		jaddstr(":")
		jevalx(b)

	when jsyscall then
		jaddstr(sysfnnames[p.fnindex]+3)
		jaddstr("(")
		if a then jevalx(a) fi
		jaddstr(")")
	when jincr then
		jaddstr("incr ")
		jevalx(a)
	when jstrinclude then
		jaddstr("strinclude ")
		jevalx(a)

	else
		CPL jtagnames[p.tag]
		gerror("CAN'T DO JEVAL",p)
	end
end

proc jadditem(ichar s)=
	gs_additem(jdest,s)
end

proc jaddstr(ichar s)=
	gs_str(jdest,s)
end

global func strmode(int m,expand=1)ichar=
	static [4096]char str
	istrmode(m,expand,&.str)
	return &.str
end

global func strmode2(int m,expand=1)ichar=
	static [4096]char str
	istrmode(m,expand,&.str)
	return &.str
end

global proc istrmode(int m,expand=1,ichar dest)=
	symbol d,q
	int needcomma,i,target,mbase,n
	strbuffer sxx
	ref strbuffer xx:=&sxx
	ref strbuffer sdim
	[100]char strdim
	ichar prefix
	typenamerec tn

	if m<0 then
		strcpy(dest,"*")
		tn:=typenames[-m]

!		if tn.defb=nil then			!assume typeof
!			strcat(dest,"typeof(")
!			strcat(dest,tn.defa.name)
!			strcat(dest,")")
!	    else
			if tn.defa then
				strcat(dest,tn.defa.name)
				strcat(dest,".")
			fi
			strcat(dest,tn.def.name)
!		fi
		return
	fi

	if m<tlast and m<>tref then
		strcpy(dest,typename(m))
		return
	fi

	case mbase:=ttbasetype[m]
	when tref then
		strcpy(dest,"ref ")
		target:=tttarget[m]
		if target>=0 and ttbasetype[target]=trecord then
			strcat(dest,typename(target))
		else
			istrmode(tttarget[m],0,dest+strlen(dest))
		fi

	when tarray then
		if ttdimexpr[m] then
			gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
			fprint @dest,"@[#<#>",&.strdim,M
		else
			if ttlength[m] then
				if ttlower[m]=1 then
					fprint @dest,"[#]",ttlength[m]+ttlower[m]-1
				else
					fprint @dest,"[#..#]",ttlower[m],ttlength[m]+ttlower[m]-1
				fi
			else
				if ttlower[m]=1 then
					fprint @dest,"[]"
				else
					fprint @dest,"[#:]",ttlower[m]
				fi
			fi
		fi
		istrmode(tttarget[m],0,dest+strlen(dest))

	when tslice then
		prefix:=stdnames[mbase]

		if ttdimexpr[m] then
			gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
			fprint @dest,"@#[#:]",prefix,&.strdim
		else
			if ttlower[m]=1 then
				strcpy(dest,prefix)
				strcat(dest,"[]")
			else
				fprint @dest,"#[#:]",prefix,ttlower[m]
			fi
		fi
		istrmode(tttarget[m],0,dest+strlen(dest))

	when trecord then
		if not expand then
			strcpy(dest,typename(m))
			return
		fi
		strcpy(dest,"")
		if expand<>2 then
			strcat(dest,typename(ttbasetype[m]))
		fi
		strcat(dest,"(")
		d:=ttnamedef[m]
		needcomma:=0

		q:=d.deflist

		while q, q:=q.nextdef do
			if needcomma then strcat(dest,",") fi
			needcomma:=1
			istrmode(q.mode,0,dest+strlen(dest))
			strcat(dest," ")
			strcat(dest,q.name)
		od
		strcat(dest,")")

	when tvoid then			!must be a usertype that is not defined (as normal voids checked above)
		strcpy(dest,"void")

	when tuser then
		strcpy(dest,typename(m))
	when tproc then

		d:=ttnamedef[m]

		strcpy(dest,"proc(")
		q:=d.paramlist
		needcomma:=0
		while q<>nil do
			if needcomma then strcat(dest,",") fi
			needcomma:=1
			istrmode(q.mode,0,dest+strlen(dest))
			strcat(dest," ")
			strcat(dest,q.name)
			q:=q.nextdef
		od
		strcat(dest,")")
		if d.mode<>tvoid then
			istrmode(d.mode,0,dest+strlen(dest))
		fi

	when ttuple then
		strcpy(dest,"Tuple(")
		n:=ttlength[m]
		for i to n do
			istrmode(ttmult[m,i],0,dest+strlen(dest))
			if i<n then strcat(dest,",") fi
		od

		strcat(dest,")")

	when tbitfield then
		strcpy(dest,"bitfield")

	elsif ttbasetype[m]<tlast then
		strcpy(dest,"Alias for:")
		istrmode(tttarget[m],0,dest+strlen(dest))

	else
		println typename(m),STRMODE(TTBASETYPE[M])
		mcerror("NEWSTRMODE")
	esac
end

global proc addtoproclist(symbol d)=
	ref procrec pp

	pp:=pcm_allocnfz(procrec.bytes)

	if proclist=nil then
		proclist:=proclistx:=pp
	else
		proclistx.nextproc:=pp
		proclistx:=pp
	fi
!
	pp.def:=d
end

global proc addstatic(symbol d)=
	ref procrec pp
!	pp:=pcm_alloc(procrec.bytes)
	pp:=pcm_allocnfz(procrec.bytes)

	if staticlist=nil then
		staticlist:=staticlistx:=pp
	else
		staticlistx.nextproc:=pp
		staticlistx:=pp
	fi

	pp.def:=d
end

global proc addexpconst(symbol d)=
	ref procrec pp
	pp:=pcm_allocnfz(procrec.bytes)

	if constlist=nil then
		constlist:=constlistx:=pp
	else
		constlistx.nextproc:=pp
		constlistx:=pp
	fi
	pp.def:=d
end

global func typename(int m)ichar=
	if m>=0 then
		return ttname[m]
	fi
	return typenames[-m].def.name

end

global func allocunitrec:ref unitrec=
	ref unitrec p

	++nunits
	nunitsmem+:=unitrec.bytes

	if remainingunits-- then
		p:=unitheapptr
		++unitheapptr
		p.pos:=lx.pos
		p.moduleno:=currmoduleno
		p.subprogno:=moduletosub[currmoduleno]
		return p
	fi

!need first or new heap
	p:=unitheapptr:=pcm_alloc(unitheapsize*unitrec.bytes)

	memset(p,0,unitheapsize*unitrec.bytes)
	remainingunits:=unitheapsize-1
	++unitheapptr
	p.pos:=lx.pos

	p.moduleno:=currmoduleno
	p.subprogno:=moduletosub[currmoduleno]

	return p
end

global func createdupldef(symbol owner,symptr, int id)symbol=
!create new proc entry
!symptr is the generic st entry for proc's name
	symbol p

	p:=newstrec()

	p.name:=symptr.name
	p.namelen:=symptr.namelen
	p.symbol:=namesym
	p.owner:=owner
	p.nameid:=id

	p.nextdupl:=symptr.nextdupl
	symptr.nextdupl:=p

	if owner then
		if owner.deflist=nil then			!first def
			owner.deflist:=owner.deflistx:=p
		else
			owner.deflistx.nextdef:=p
			owner.deflistx:=p
		fi
	fi

	return p
end

global func createnewmoduledef(symbol owner,symptr, int id=moduleid)symbol=
	return createdupldef(owner,symptr,id)
end

global func duplunit(unit p,int lineno=0)unit=
	unit q
	if p=nil then return nil fi

	q:=createunit0(p.tag)

	q^:=p^
	q.nextunit:=nil
	for i to jsubs[q.tag] do
		q.abc[i]:=duplunit(q.abc[i])
	od

	return q
end

global func isconstunit(unit a)int=
	return a.isconst
end

global proc getownername(symbol d, ichar dest)=
	symbol owner

	owner:=d.owner

	if owner=nil or owner.nameid=programid then return fi
	getownername(owner,dest)
	strcat(dest,owner.name)
	strcat(dest,".")
end

global func getalignment(int m)int=
!return alignment needed for type m, as 1,2,4,8
	int a

	case ttbasetype[m]
	when tarray then
		return getalignment(tttarget[m])
	when trecord then
		a:=ttnamedef[m].maxalign
		if a=0 then
CPL "GAL0"
 a:=8 fi
		return a
	elsif ttisblock[m] then
		return 8
	esac

	a:=ttsize[m]
	case a
	when 1,2,4,8 then
		return a
	when 0 then
		return 8
	esac
	cpl Strmode(m)
	gerror("GETALIGN SIZE NOT 1248")

	return 0
end

global proc addlistunit(unit &ulist,&ulistx,unit p)=
!add unit p to unit structure ulist,^ulistx  which can be null
	if ulist=nil then		!first
		ulist:=ulistx:=p
	else
		ulistx.nextunit:=p
	fi
	ulistx:=p			!update end-of-list pointer
end

global func storemode(symbol owner, int m, i32 &pmode)int =
	ref typenamerec r

	if m>=0 then
		pmode:=m
		return m
	fi

	r:=&typenames[-m]

	if r.pmode=nil then
		r.owner:=owner
		pmode:=m
		r.pmode:=&pmode

	IF R.PMODE=NIL THEN SERROR("PMODE=NIL") FI

		return m
	fi

!Already one instance of this mode; need a new slot
	m:=newtypename(r.defa, r.defb)
	r:=&typenames[-m]

	r.owner:=owner
	pmode:=m
	r.pmode:=&pmode
	return m
end

global func gettypebase(int m)int=
	case ttbasetype[m]
	when ti8,ti16,ti32 then ti64
	when tu8,tu16,tu32 then ti64

	when tr32 then tr64

	when tc8 then tc64
	else
		m
	esac
end

global proc writegsfile(ichar filename, ref strbuffer d)=
	filehandle f

	f:=fopen(filename,"w")
	gs_println(d,f)
	fclose(f)
end

global proc addtolog(ichar filename, filehandle logdest)=
	filehandle f
	int c

	f:=fopen(filename,"rb")

	if f=nil then
		CPL "ATL ERROR",FILENAME; return fi

	do
		c:=fgetc(f)
		exit when c=c_eof
		fputc(c,logdest)
	od
	fclose(f)
end

global func getprocretmodes(unit p)symbol=
!p must be a call unit, for a proc with multiple values; at least one expected
!however, here it only populates retmodes with the available types
	unit a

	if p.tag<>jcall then txerror("multass/need multfn") fi
	a:=p.a

	case a.tag
	when jname then
		return a.def
	else
		return ttnamedef[tttarget[a.mode]]
	esac
end

global func getpclmode(int t)int u=
	u:=stdpcl[ttbasetype[t]]

	if u=tpblock then
		case ttsize[t]
		when 8 then u:=tpu64
		when 4 then u:=tpu32
		when 2 then u:=tpu16
		when 1 then u:=tpu8
		esac
	fi
	return u
end

=== mm_libsources_dummy.m 0 0 22/34 ===
global const fsyslibs = 0

global proc loadbuiltins=
end
=== mm_modules.m 0 0 23/34 ===
ichar fileext="m"

global func loadsp(ichar filename, int mainsub=0)isubprog sp=
!source = nil:  load lead module and dependencies from given sourcefile
!source <> nil: source code is given directly. filename can give a name
! to that source text, or if nil, and internal name is applied

	const maxmods=250
	const maxsubs=250
	[maxmods]ichar modnames
	[maxmods]symbol aliases
	[maxmods]ichar paths
	[maxsubs]ichar subnames
	[maxsubs]ichar subpaths
	int nmods:=0, nsubs:=0, hdrcode
	int firstmod, lastmod, issyslib:=0
	imodule pm
	symbol d, stalias
	ichar path, name, ext, file2
	byte proj:=0, sepheader:=0

!CPL "LOADSP", =EXTRACTBASEFILE(FILENAME), =SYSLIBNAME

	if eqstring(extractbasefile(filename), syslibname) then
		issyslib:=1
	fi

	ext:=extractext(filename)
	if not eqstring(ext, "m") then fileext:=pcm_copyheapstring(ext) fi

	pm:=loadmodule(filename, issyslib)

	if pm=nil then
		loaderror("Can't load lead module: ", filename)
	fi
	path:=pm.file.path

	for i to nsubprogs do
		if eqstring(pm.name, subprogs[i].name) then
			loaderror("Subprog already loaded: ", sp.name)
		fi
	od

!reader header info
	startlex(pm.file)
	lex()
	skipsemi()

	if lx.symbol=kprojectsym then
		proj:=1
		lexchecksymbol(eqsym)
		lex()
	fi

	do
		skipsemi()
		case lx.symbol
		when kheadersym then
			hdrcode:=lx.subcode
			lex()
			case hdrcode
			when hdr_module then
				checksymbol(namesym)
				name:=lx.symptr.name

				if not eqstring(name, pm.name) then
					if nmods>=maxmods then loaderror("Too many modules in header") fi
					modnames[++nmods]:=name
					paths[nmods]:=path
					aliases[nmods]:=nil

				fi
				if nextlx.symbol=namesym and eqstring(nextlx.symptr.name,"as") then
					lex()
					lex()
					if lx.symbol=namesym then
						stalias:=lx.symptr
						lex()
					else
						checksymbol(stringconstsym)
						stalias:=addnamestr(lx.svalue)
					fi
					aliases[nmods]:=stalias
				fi

			when hdr_import then
				checksymbol(namesym)
				if nsubs>=maxsubs then loaderror("Too many imports in header") fi
				subnames[++nsubs]:=lx.symptr.name
				subpaths[nsubs]:=path

			when hdr_linkdll then
				checksymbol(namesym)
				addlib(lx.symptr.name)

			when hdr_sourcepath then
				checksymbol(stringconstsym)
				unless loadedfromma then			!ignore paths for .ma
					path:=pcm_copyheapstring(lx.svalue)
				end
!CPL "SET PATH", PATH

			else
				loaderror("Hdr cmd not ready")
			esac
			lex()

		when semisym then
		else
			exit
		esac
	od

	if proj then
		checkend(kendsym, kprojectsym)
	fi
	skipsemi()
	if lx.symbol=eofsym then
		sepheader:=1
	fi

!process nested imports
	for i to nsubs do
		if eqstring(subnames[i],pm.name) then loaderror("Importing self") fi
!		loadsp(getmodulefilename(path, subnames[i]))
		loadsp(getmodulefilename(subpaths[i], subnames[i]))
	od

!create new subprog entry
	if nsubprogs>=maxsubprog then loaderror("Too many subprogs") fi
	sp:=pcm_allocz(subprogrec.bytes)
	subprogs[++nsubprogs]:=sp
	sp.subprogno:=nsubprogs

	if mainsub then
!		loadsyslib()
		mainsubprogno:=nsubprogs
	fi

	firstmod:=nmodules+1
	lastmod:=firstmod+nmods
	if lastmod>maxmodule then loaderror("Too many modules") fi
	nmodules:=lastmod
	pm.subprogno:=nsubprogs
	pm.islead:=1
	pm.moduleno:=firstmod
	pm.stmodule:=d:=createdupldef(stprogram,addnamestr(pm.name),moduleid)
	d.moduleno:=firstmod
	d.subprogno:=nsubprogs

	moduletosub[firstmod]:=nsubprogs

	sp.name:=pm.name
	sp.firstmodule:=firstmod

	sp.mainmodule:=0

	sp.lastmodule:=lastmod
	sp.issyslib:=issyslib

!create new set of modules[] entries and load those other modules
!create stmodule entries for each module
	modules[firstmod]:=pm

	for i to nmods do
		pm:=loadmodule(getmodulefilename(paths[i], modnames[i], issyslib), issyslib)
		stalias:=aliases[i]
		if not pm then
			loaderror("Can't load: ",modnames[i])
		fi
		modules[firstmod+i]:=pm
		pm.stmodule:=d:=createdupldef(stprogram,addnamestr(pm.name),moduleid)
		pm.subprogno:=nsubprogs
		
		if stalias then
			pm.stmacro:=getduplnameptr(stprogram, stalias, macroid)
			adddef(stprogram, pm.stmacro)
			pm.stmacro.paramlist:=nil
			pm.stmacro.code:=createname(d)
		fi

		d.moduleno:=pm.moduleno:=firstmod+i
		d.subprogno:=nsubprogs
		moduletosub[d.moduleno]:=nsubprogs

		for j to nmodules when eqstring(modules[i].name, pm.name) do
			serror_s("Dupl mod name:", pm.name)
		od
	od

	return sp
end

global func loadmodule(ichar filespec, int issyslib=0)imodule pm=
	ifile pf

!CPL "LOADMOD",FILESPEC, =ISSYSLIB
	pf:=loadsourcefile(filespec, issyslib)
	return nil when pf=nil

	pm:=pcm_allocz(modulerec.bytes)

	pm.name:=pf.name
	pm.file:=pf
	pm.fileno:=pf.fileno
	pm.issyslib:=issyslib

	return pm
end

global func loadsourcefile(ichar filespec, int issyslib=0)ifile pf=
	ichar s,filename
	[300]char str

	filename:=extractfile(filespec)

!CPL "LSF1", =FILENAME, =NSOURCEFILES, =ISSYSLIB

!look for file already loaded, or preloaded due to built-in syslib or .ma file:
	for i to nsourcefiles do
!CPL "LOOP", I, FILENAME, SOURCES[I].FILENAME, SOURCES[I].ISSYSLIB
		if eqstring(filename, sources[i].filename) and sources[i].issyslib=issyslib then
			return sources[i]
		fi
	od

	pf:=newsourcefile()

	pf.filespec:=pcm_copyheapstring(filespec)
	pf.path:=pcm_copyheapstring(extractpath(filespec))
	pf.name:=pcm_copyheapstring(extractbasefile(filespec))
	pf.filename:=pcm_copyheapstring(filename)
	pf.issyslib:=issyslib
	pf.fileno:=nsourcefiles
!CPL "LSF", FILESPEC


	s:=cast(readfile(filespec))			!will overallocate by a few bytes
	if not s then				!unexpected error
		return nil
	fi
	pf.text:=s
	pf.size:=rfsize

	if passlevel=ma_pass then
		pf.dupl:=pcm_copyheapstring(s)
	fi

	(s+rfsize)^:=0				!replace etx,0 by 0,0 (effectively, just zero)
	return pf
end

func getmodulefilename(ichar path, name, int issyslib=0)ichar =
	static [300]char str

!need to sort out search path etc

	strcpy(str, path)
	strcat(str, name)
	strcat(str, ".")
	strcat(str, (issyslib|"m"|fileext))
	return str
end

global proc addlib(ichar libname)=
	for i to nlibfiles do
		if eqstring(libfiles[i],libname) then return fi
	od
	if nlibfiles>=maxlibfile then
		loaderror("Too many libs")
	fi
	libfiles[++nlibfiles]:=libname
end

proc loadsyslib=
	[300]char str
	ichar name
	byte frunpcl:=passlevel=runpcl_pass
	byte fgenpcl:=passlevel=pcl_pass
!	byte flinux:=clinux or not os_iswindows()

!CPL "LSB", =DOINTLIBS

	if dointlibs then				!bundled sys files
		str[1]:=0
	else
		strcpy(str, langhomedir)
	fi

	case msyslevel
	when 0 then
		return
	when 1 then
		name:=(ctarget or not os_iswindows()|"msysminc"|"msysmin")
	else				!full syslib
!CPL "FULLSYS"
		if os_iswindows() and not clinux then	!run on Windows
!CPL "WINDOWS", =CTARGET, FRUNPCL
			if ctarget then
				name:="msyswinc"
			elsif frunpcl or fgenpcl then		!avoid modules with assem
				name:="msyswini"
			else
				name:="msyswin"
			fi
		else									!on Linux, or generating C for Linux on Windows
!CPL "LINUX"
			name:="msyslinc"
		fi
	esac

	strcat(str, name)

	SYSLIBNAME:=PCM_COPYHEAPSTRING(STR)
IF FVERBOSE>=2 THEN
	CPL =SYSLIBNAME
FI
	strcat(str, ".m")


!CPL "SYSLIB:",STR

	loadsp(str)
end

global proc loadproject(ichar file)=
	[300]char str
	ichar file2

	int tt:=clock()
	if dointlibs then
		loadbuiltins()
	fi

	loadsyslib()

!try .ma version of .m not present
	if not checkfile(file) then
		file2:=pcm_copyheapstring(changeext(file,"ma"))
		if checkfile(file2) then file:=file2 fi
	fi

	if eqstring(extractext(file),"ma") then
CPL "LOADING FROM MA FILE"
		loadmafile(file)
		loadedfromma:=1
		strcpy(str, changeext(file,"m"))			!assume lead module has same name as ma file
		file:=&.str
	fi

	loadsp(file, 1)

	addlib("msvcrt")
	if os_iswindows() then
		addlib("user32")
		addlib("gdi32")
		addlib("kernel32")
	end

	loadtime:=clock()-tt
end

func readfileline(ichar s)ichar =
!s points into a string representing an entire file, which should be 0-terminated
!read the line s until the next eol or eof, into a line buffer
!return a pointer to the next line
	[2048]char str
	ichar t:=str
	int n, c

	n:=0
	docase c:=s++^
	when 0 then
		--s
		exit
	when 10 then
		exit
	else
		if n<str.len then
			t++^:=c
		fi
	end docase

	t^:=0

	readln @&.str
	return s
end

func findnextlineheader(ichar s)ichar=
!starting from s, find next sequence of lf followed by ===
!return nil if eof found
!otherwise return pointer to just after the ===
	int c

	docase c:=s++^
	when 0 then
		return nil
	when 10 then
		if s^='=' and (s+1)^='=' and (s+2)^='=' then
			return s+3
		fi
	end docase

	return nil
end

proc loadmafile(ichar filespec, ichar builtinstr=nil)=
!load ma file from disk
!unless filespec is nil, then direct from builtinstr
!return name of lead module
	ichar s,t
	[100]char name
	int sys,support
	ifile pf

	if filespec then
		s:=cast(readfile(filespec))
		if s=nil then							!file not found on disk
			loaderror("Can't find MA file ",filespec)
		fi
	else
		s:=builtinstr
	fi

!need to scan file pickuping the file headers, and populating sourctables

	s:=readfileline(s+3)
	readstr(name,'n')
	if not eqstring(name,"ma") then
		loaderror("MA: bad header")
	fi

	--s					!point to previous lf

	s:=findnextlineheader(s)

	do
		if s=nil then
			loaderror("Unexpected EOF in MA file")
			exit
		fi
		s:=readfileline(s)

		readstr(name,'n')
		read sys,support

		if eqstring(name,"end") then
			exit
		fi
		if nsourcefiles>=maxsourcefile then
			loaderror("Too many files in MA")
		fi

		t:=findnextlineheader(s)
		if t=nil then
			loaderror("MA error")
		fi

		pf:=newsourcefile()

		pf.filename:=pf.filespec:=pcm_copyheapstring(name)
		pf.name:=pcm_copyheapstring(extractbasefile(name))
		pf.size:=t-s-3
		pf.text:=s
		pf.path:=pf.filespec:=""
		pf.issyslib:=sys
		pf.issupport:=support
		s:=t
	od
!
	for i to nsourcefiles do
		pf:=sources[i]
		(pf.text+pf.size)^:=0
	od
end


=== mm_name.m 0 0 24/34 ===
symbol currstproc
int allowmodname=0
int noexpand
int macrolevels

const maxmacroparams=50
[maxmacroparams]symbol macroparams
[maxmacroparams]symbol macroparamsgen
[maxmacroparams]unit macroargs
int nmacroparams
int nmacroargs

global proc rx_typetable=
	symbol d
!	symbol currproc

	for i:=tuser to ntypes do
		if ttbasetype[i]=trecord then
			d:=ttnamedef[i]
			if d.baseclass then
				do_baseclass(d)
			fi
		fi
	od
end

global proc rx_unit(symbol owner, unit p)=
	symbol d
	unit a,b
	int n,oldnoexpand,oldtag,useparams

	a:=p.a
	b:=p.b
	mmpos:=p.pos

	switch p.tag
	when jname then
		resolvename(owner,p)
		if P.TAG=JNAME AND p.def.nameid=macroid and not noexpand then
			++macrolevels
			expandmacro(p,p,nil)
			rx_unit(owner,p)
			--macrolevels
		fi

	when jkeyword then
		rx_unit(owner,b)		!do param value only

	when jdot then
		resolvedot(owner,p)

	when jcall then
		oldtag:=p.tag

		if a.tag=jname then			!can expand possible macro if params not ready
			oldnoexpand:=noexpand; noexpand:=1
			rx_unit(owner,a)
			noexpand:=oldnoexpand
		else
			rx_unit(owner,a)
		fi

		rx_unitlist(owner,b)

		if a.tag=jname then
			d:=a.def
			case d.nameid
			when typeid then		!change to type conversion
				p.tag:=jconvert
				storemode(owner,d.mode,p.convmode)
				p.a:=b
				if b.nextunit then
					p.a:=createunit1(jmakelist,b)
					n:=0
					while b do
						++n
						b:=b.nextunit
					od
					p.a.length:=n
				fi
			when macroid then
				++macrolevels
				if d.deflist then			!macro uses params
					expandmacro(p,a,b)
					b:=nil
					useparams:=0
				else						!macro has no params
					expandmacro(p,a,nil)
					useparams:=1
				fi

				rx_unit(owner,p)
				--macrolevels

				if useparams and p.tag<>jcall then
					insertunit(p,oldtag)
					p.b:=b					!note b may be nil
				FI

			esac
		fi

	when jandl, jorl then
		rx_unit(owner,a)
		rx_unit(owner,b)
		if not isbooltag[a.tag] then insertunit(a,jistruel) fi
		if not isbooltag[b.tag] then insertunit(b,jistruel) fi

	when jistruel then
	doistruel:
		rx_unit(owner,a)

		if isbooltag[a.tag] then
			deleteunit(p,a)
		fi

	when jnotl then
		rx_unit(owner,a)
		case a.tag
		when jnotl then
			deleteunit(p,a)
			p.tag:=jistruel
			a:=p.a
			goto doistruel

		when jistruel then
			a.tag:=jisfalsel
			deleteunit(p,a)
			a:=p.a
		when jisfalsel then
			a.tag:=jistruel
			deleteunit(p,a)
			a:=p.a
		elsif not isbooltag[a.tag] then
			p.tag:=jisfalsel
			a:=p.a
		esac

	else
doabc:
		for i to jsubs[p.tag] do
			rx_unitlist(owner,p.abc[i])
		od
	end switch
end

global func rx_module(int n)int=
	currmoduleno:=n

	rx_passdef(stprogram,modules[n].stmodule)

	return 1
end

global proc rx_deflist(symbol owner,p)=
	symbol pstart:=p
	while p do
		rx_passdef(owner,p)
		p:=p.nextdef
	od
end

global proc rx_passdef(symbol owner,p)=
	symbol d

	case p.nameid
	when moduleid,dllmoduleid then
		rx_deflist(p,p.deflist)

	when procid then
		rx_deflist(p,p.deflist)
		currstproc:=p
		rx_unit(p,p.code)
		currstproc:=nil

	when dllprocid then
		rx_deflist(p,p.deflist)

	when constid,staticid,frameid,paramid then
		if p.atvar then
			rx_unit(owner,p.equivvar)
		fi
		if p.code then
			rx_unit(owner,p.code)
		fi
	when typeid then
		rx_deflist(p,p.deflist)

	else
	esac
end

proc rx_unitlist(symbol owner, unit p)=
	while p do
		rx_unit(owner,p)
		p:=p.nextunit
	od
end

global func resolvetopname(symbol owner,stnewname,int moduleno,allowmod)symbol =
!stnewname points to a symrec with generic nullid
!This is a top-level name (left-most name of any dotted sequence, or standalone name)

!Search through all the duplicate symrecs (all names with identical names have symrecs that
!are linked together, always starting with a nullid symrec) looking for the best match

!moduleno is the module where the currently generic name is encountered
!(derived from a unit if in an expression, or an STREC if a type in a declaration)

	int extcount, subprogno
	symbol p,q, powner,extdef,moddef
	[10]symbol ambiglist

	if owner.nameid=procid then
		q:=owner.deflist
		while q, q:=q.nextdef do
			if q.firstdupl=stnewname then		!immediate match
				return q
			fi
		od
	fi

	p:=stnewname.nextdupl
	subprogno:=moduletosub[moduleno]

	extcount:=0
	extdef:=moddef:=nil

	while p, p:=p.nextdupl do						!p is next candidate
		powner:=p.owner								!the owner of that entry

		case powner.nameid
		when moduleid then							!candidate is file-scope item
			if powner.moduleno=moduleno then		!same module
				return p
			elsif p.scope then	!matches an external module
				if powner.subprogno=subprogno or		!within same subprog
					 p.scope=program_scope or
					 p.isimport then 				!visible outside subprog
					++extcount			!if an ext match is closest, there can only be one
					extdef:=p
					if extcount<ambiglist.len then
						ambiglist[extcount]:=extdef
					fi
				fi
			fi

		when typeid then					!only for code inside a record def
			if powner=owner or powner=owner.owner then		!immediate match
				return p					!looks at 2 nested record levels only
			fi

		when programid then					!p is a module
			case p.nameid
			when moduleid, subprogid then	!match a module/subprog name
				if subprogno=moduletosub[p.moduleno] then
					moddef:=p
				else
					for i to nsubprogs do
						if eqstring(p.name, subprogs[i].name) then
							p.issubprog:=1				!in case not yet set
							moddef:=p
							exit
						fi
					od
				fi
			when macroid then
				return p

			esac

		esac
	od

	if allowmod and moddef then
		return moddef
	fi

	if extdef then
		if extcount>1 then
			if not eqstring(extdef.owner.name, "mclib") then
				for i:=1 to extcount do
					extdef:=ambiglist[i]
					println i,extdef.owner.name,namenames[extdef.owner.nameid]
				od
				if not eqstring(extdef.owner.name, "mclib") then
					rxerror_s("Ambiguous ext name: #",extdef.name)
				fi
			fi
		fi
		return extdef
	fi
	return nil
end

global proc resolvename(symbol owner, unit p)=
!p is a name tag inside given owner
!resolve name
!report error if unresolved, unless mode is not void. Then an unresolved
!name is added as a frame (assumes this is a proc)

	symbol d,e
	int moduleno, mode,islet

	d:=p.def
	moduleno:=p.moduleno

	if d.nameid<>nullid then			!assume already resolved
		return
	fi

	e:=resolvetopname(owner,d,moduleno,allowmodname)

	if not e then
		islet:=0
		mode:=tvoid
		case p.avcode
		when 'I', 'T', 'S' then mode:=ti64; islet:=1
		when 'L','A' then mode:=tany
		esac

		if mode=tvoid then
			[300]CHAR STR
			STRCPY(STR, D.NAME)
			CONVUCSTRING(STR)
			rxerror_s("pcl:Undefined: #",STR,p)
		else
			e:=addframevar(owner,d,moduleno,mode)
			e.pos:=p.pos
			e.islet:=islet
		fi
	fi

	if e.used<255 then ++e.used fi

	p.def:=e
end

global func finddupl(symbol d, pdupl)symbol=
!trying to resolve a field name, by scanning a dupllist headed by pdupl
!which ought to point to nullid entry
!d will be the owner of the matching entry

	if pdupl.nameid<>nullid then		!assume already resolved
		return pdupl
	fi
	pdupl:=pdupl.nextdupl

	while pdupl do
		if pdupl.owner=d then
			return pdupl
		fi
		pdupl:=pdupl.nextdupl
	od
	return nil
end

global func finddupl_sub(symbol d, pdupl)symbol=
!version of finddupl where d is a subprog
	int subprogno

	if pdupl.nameid<>nullid then		!assume already resolved
		return pdupl
	fi
	pdupl:=pdupl.nextdupl
	subprogno:=d.subprogno

	while pdupl do
		if pdupl.owner.subprogno=subprogno then
			return pdupl
		fi
		pdupl:=pdupl.nextdupl
	od
	return nil
end

proc resolvedot(symbol owner,unit p)=
	unit lhs,rhs
	symbol d,e,t
	int m,moduleno,subprogno,oldallowmod

	moduleno:=p.moduleno
	subprogno:=p.subprogno
	lhs:=p.a
	rhs:=p.b
	e:=rhs.def				!p.b will be a name type (could perhaps be stored as p.def)

	oldallowmod:=allowmodname
	allowmodname:=lhs.tag=jname
	rx_unit(owner,lhs)
	allowmodname:=oldallowmod
	d:=lhs.def

	case lhs.tag
	when jname then
		case d.nameid
		when moduleid, typeid, procid, typeid then

			if d.nameid=moduleid and d.subprogno<>subprogno then
				dosubprogid
			fi

			e:=finddupl(d,e)

			if e then
				if d.nameid=moduleid then
					if e.subprogno<>subprogno then
						if e.scope<program_scope AND NOT E.ISIMPORT then
							rxerror_s("Need export to import '#'",e.name)
						fi
					elsif e.moduleno<>moduleno then
						if not e.scope then
							rxerror_s("Need global to import '#'",e.name)
						fi
					fi
				fi
domodule:
				p.tag:=jname			!convert to dot to name
				p.a:=p.b:=nil
				p.def:=e
				case e.nameid
				when constid then
				esac
			else
				rxerror_s("Can't resolve .#",p.b.def.name,p)
			fi

		when frameid, staticid, paramid then		!.x applied to normal var
			m:=d.mode
			case ttbasetype[m]
			when trecord then
			when tref then
				do
					m:=tttarget[m]
					case ttbasetype[m]
					when trecord then
						exit
					when tref then
					else
						rxerror("2:Record expected")
					esac
				od
			else
				rxerror("Record expected")
			esac
			t:=ttnamedef[m]

			e:=finddupl(t,e)
			if e then
				p.b.def:=e
			else
				rxerror_s("Not a field: #",rhs.def.name)
			fi
		when subprogid then
dosubprogid:
			e:=finddupl_sub(d,e)
			if e then
				if e.subprogno<>subprogno then
					if e.scope<program_scope AND NOT E.ISIMPORT then
						rxerror_s("Need export to import '#'",e.name)
					fi
				fi
				goto domodule
			else
				rxerror_s("Can't resolve sub.#",p.b.def.name,p)
			fi

		esac

	else
!Can't fully resolve at this time; leave to next pass
		unless e.nextdupl then
			rxerror_s("Not a field: #",e.name)
		end unless
	esac
end

proc fixmode(ref typenamerec p)=
!p refers to a negative mode that is a typename index
!fix that up if possible
	ref i32 pmode
	symbol a,d,e,f,owner
	int m,moduleno

	pmode:=p.pmode

	m:=-pmode^					!typename index

	d:=owner:=p.owner
	while d.nameid<>moduleid do d:=d.owner od
	moduleno:=d.moduleno

	a:=p.defa
	d:=p.defb

	if a=nil and d then			!simple type name V
		e:=resolvetopname(owner,d,moduleno,0)

	fi

	if e and e.nameid=typeid then
		pmode^:=e.mode

	else
		rxerror_s("2:Can't resolve tentative type: #",d.name)
	fi
end

global proc fixusertypes=
	ref typenamerec p
	int npasses,notresolved
	symbol d

	npasses:=0
	repeat
		++npasses
		notresolved:=0

		for i to ntypenames do
			p:=&typenames[i]

			if p.pmode^<0 then
				mmpos:=typenamepos[i].pos
				fixmode(p)
				if p.pmode^<0 then
					++notresolved
				fi
			fi
		od

		if npasses>5 then
			println "Type phase errors - check these user types:"

			for i to ntypenames do
				p:=&typenames[i]

				if p.pmode^<0 then
					d:=p.defb
					if d=nil then d:=p.defa fi
					println "	",d.name
				fi
			od

			rxerror("Fixtypes: too many passes (cyclic ref?)")
		fi

	until notresolved=0
end

func addframevar(symbol owner, d, int moduleno, mode)symbol=
!owner should be a proc; d is a generic st entry
!add framewith the name of d and given mode to the proc
	symbol e
	e:=getduplnameptr(owner,d,frameid)
	storemode(owner,mode,e.mode)
	adddef(owner,e)
	return e
end

func copylistunit(unit p)unit=
	unit q

	unit plist,plistx
	plist:=plistx:=nil
	while p do
		q:=copyunit(p)
		addlistunit(plist,plistx,q)
		p:=p.nextunit
	od
	return plist
end

func copyunit(unit p)unit=
	unit q
	symbol d

	if p=nil then return nil fi

!need to quickly check if a name unit is a macroparam

	if p.tag=jname then
		d:=p.def
		for i to nmacroparams do
			if macroparamsgen[i]=d then
				return copyunit(macroargs[i])
				exit
			fi
		od
	fi

	q:=createunit0(p.tag)

	q^:=p^
	q.nextunit:=nil
	for i to jsubs[q.tag] do
		q.abc[i]:=copylistunit(q.abc[i])
	od

	return q
end

proc replaceunit(unit p,q)=
!replace p with q, keeping same address of p, and same next pointer
!original contents discarded
	unit pnext
	pnext:=p.nextunit
	p^:=q^
	p.nextunit:=pnext
end

proc expandmacro(unit p, a, b)=
!a is a macro name unit, b is a macro parameter list (rx-processed), which
!can be nil
!p is either the call-unit as this may originally have been, or the same as a:
!M => (NAME)
!M(A,B) => (CALL NAME (A,B))
!Need to replace M or M(A,B) with the duplicated AST from a.code.
!When a name appears in the AST which is a local macroparam name, then that is
!replaced by the corresponding argument from B;
!The new code replaces P (either CALL or NAME)
!Supplied args may be more than macro takes (error) or may be less (error,
!or allow default arg values to be specified)
	symbol d,pm
	unit pnew
	int ignoreargs

	if macrolevels>10 then
		rxerror("Too many macro levels (recursive macro?)")
	fi

	d:=a.def

!First step: get list of macro formal parameters

	pm:=d.paramlist
	nmacroparams:=0
	while pm do
		if nmacroparams>=maxmacroparams then
			rxerror("macro param overflow")
		fi
		macroparams[++nmacroparams]:=pm
		macroparamsgen[nmacroparams]:=pm.firstdupl

		pm:=pm.nextparam
	od

!now get macro args into a list
	nmacroargs:=0

	while b do
		if nmacroargs>=maxmacroparams then
			rxerror("macro arg overflow")
		fi
		macroargs[++nmacroargs]:=b
		b:=b.nextunit
	od

	if nmacroargs<nmacroparams then
		PRINTLN =NMACROARGS, NMACROPARAMS
		rxerror("Too few macro args")
	fi

	ignoreargs:=0
	if nmacroargs>0 and nmacroparams=0 then		!ignore extra params
		ignoreargs:=1
		nmacroargs:=nmacroparams:=0

	elsif nmacroargs>nmacroparams then
		rxerror("Too many macro args")
	fi

	pnew:=copyunit(d.code)

	if not ignoreargs then				!normal expansion
		replaceunit(p,pnew)
	else								!keep call and paramlist; just replace fn name
		p.a:=pnew						!with expansion
	fi
end

proc duplfield(symbol owner,p,q)=
!p is strec of an existing field, const etc
!q is a newly created strec with the same id and name
!copy the relevant fields of p to q

	if p.code then
		serror("DUPLFIELD")
	fi

!Need to copy whatever are relevant attributes

	q.atfield:=p.atfield
	q.flags:=p.flags

	q.uflags:=p.uflags		!for .uflags
	storemode(owner,p.mode,q.mode)
end

proc do_baseclass(symbol p)=
!p is class name, which has a baseclass, do the copying necessary for
!inheriting fields
	symbol d,e,newd,dbase
	int normalexit

	dbase:=ttnamedef[p.baseclass]
	d:=dbase.deflist

	while d do				!for each element of base class
		e:=p.deflist

		normalexit:=1
		while e do			!for each element of new class
			if eqstring(d.name,e.name) then
				normalexit:=0
				exit
			fi
			e:=e.nextdef
		od
		if normalexit then
!duplicate d in this class; keep it simple for now
!(procs will need a more elaborate duplication, and really needs to share code)
			case d.nameid
			when procid,linkid then
				newd:=getduplnameptr(p,d,linkid)
				newd.equivfield:=d
			else
				newd:=getduplnameptr(p,d,d.nameid)
				duplfield(p.owner,d,newd)
			esac
			adddef(p,newd)
		fi
		d:=d.nextdef
	od
end
=== mm_parse.m 0 0 25/34 ===
!M Language Parserxxx

int intabledata=0		!1 means reading table data line; $ gives tabledataname
int inreadprint=0
int inparamlist=0
int inrecordbody=0
int inimportmodule=0
int labelseen=0
ichar tabledataname=nil

const maxprocstack=10
[maxprocstack]symbol procstack
int nprocstack=0

uflagsrec unionstring, unionpend
symbol unionlastvar=nil
symbol dretvar			!part of read-proc: nil, or symptr of retval variable

int varattribs=0

const maxdollarstack=10
[maxdollarstack]unit dollarstack		!used for a[$]
int ndollar=0
int insiderecord=0
int insidedllimport=0

const maxforloops=10
[maxforloops]symbol forindexvars
int nforloops

global func parsemodule(imodule pm)int=
	symbol owner

	initparser()

	currmoduleno:=pm.moduleno

	stmodule:=pm.stmodule

	currproc:=stmodule

	startlex(pm.file)
	owner:=stmodule

	lex()
!

!CPL "PARSE", PM.NAME


!!=========================================
!int t:=os_clock()
!int ntokens:=0
!CPL "******************** LEX TEST ****************"
!
!!	repeat
!!		lex()
!!		++ntokens
!!!PS("TOKEN")
!!	until lx.symbol=eofsym
!
!	repeat
!		lexreadtoken()
!!PSNEXT("HELLO")
!		++ntokens
!	until nextlx.symbol=eofsym
!
!!CPL =NMODULES
!
!t:=os_clock()-t
!
!CPL "LEX TIME=", t
!CPL =ntokens
!
!STOP
!!=========================================
!
	readmoduledefs(owner)
	return 1
end

global proc readmoduledefs(symbol owner) =
!first symbol has been read
	int globalflag

	globalflag:=module_scope

	do
		switch lx.symbol
		when kglobalsym then
			if globalflag then serror("global global?") fi
			globalflag:=lx.subcode

!			if globalflag=export_scope and stmodule.subprogno<>1 then
			if globalflag=export_scope and stmodule.subprogno<>nsubprogs then
				globalflag:=program_scope
			fi

			lex()

		when kprocsym, kfuncsym then	!todo
			readprocdef(owner, globalflag)
			globalflag:=module_scope

		when stdtypesym, krefsym, kicharsym, lsqsym, kslicesym then
dovar:
			readvardef(owner, globalflag, 0, staticid, 0)
			globalflag:=module_scope

		when kletsym then
			lex()
			readvardef(owner, globalflag, 0, staticid, kletsym)
			globalflag:=module_scope

		when kimportmodulesym then
			readimportmodule(owner)

		when ktypesym then
			readtypedef(owner, globalflag)
			globalflag:=module_scope

		when kconstsym then
			readconstdef(owner, globalflag)
			globalflag:=module_scope

		when krecordsym then
			readclassdef(owner, globalflag)
			globalflag:=module_scope

		when ktabledatasym then
			readtabledef(owner, globalflag)
			globalflag:=module_scope

		when semisym then
			lex()

		when eofsym then
			exit

		when kmacrosym then
			readmacrodef(owner, globalflag)
			globalflag:=module_scope

		when kprojectsym then
			repeat
				lex()
			until lx.symbol in [kendsym, eofsym]
			checkend(kendsym, kprojectsym)

		when kheadersym then
			repeat
				lex()
			until lx.symbol=semisym

		when dotsym then
			SERROR("MODULE/DOT")
		when namesym then
			if istypestarter() then
				goto dovar
			fi
			goto doexec

		else
doexec:
		serror("Code outside a func")
		end switch
	od
end

proc initparser=

	unless nullunit then
		nullunit:=createunit0(jnull)
	end unless

	currproc:=nil
	varattribs:=0

	intabledata:=0		!1 means reading table data line; $ gives tabledataname
	inreadprint:=0
	inparamlist:=0
	inrecordbody:=0
	inimportmodule:=0
	ichar tabledataname:=""
	labelseen:=0

	ndollar:=0
end

global proc skipsemi=
	while lx.symbol=semisym do lex() od
end

global func makeblock(unit p)unit=
	if p and p.tag=jblock then return p fi
	return createunit1(jblock, p)
end

proc checkequals=
!check that "=" is current symbol
	if lx.symbol<>eqsym then
		serror("""="" expected")
	fi
end

func getcurrline:int=
	return lx.pos
end

func checkbegin(int fbrack)int=
!look for ( or [ or begin, return close symbol expected
!positioned at this opening symbol
!fbrack=1 to allow left "("
	int closesym

	skipsemi()

	if lx.symbol=lbracksym and fbrack then
		closesym:=rbracksym
		lex()
	else
		closesym:=kendsym
	fi
	return closesym
end

proc checkbeginend(int closesym, kwd, startline=0)=
!look for ) or ] or end [kwd] depending on closesym
!positioned at this symbol; exit at following symbol
	skipsemi()
!	if closesym=rbracksym or closesym=rcurlysym then
	if closesym=rbracksym then
		checksymbollex(closesym)
!		lex()
	else
		checkend(closesym, kwd, startline:startline)
	fi
end

global proc checkend(int endsym, endkwd1, endkwd2=0, startline=0)=
!at terminator symbol such as ), eof or 'end'
!check it matches what is expected
!endsym is symbol expected to match
!'end' can have optional keyword following; if present, it must match endkeyword
!Some loop ends (indicated by endkeyword=kforsym, etc) can be also be terminated with 'od'
!endsym should be lbracksym or kendsym
	[100]char str

	skipsemi()

!exit pointing to current symbol (to 'end', keyword after 'end', or ')')
	if endsym=lx.symbol=rbracksym then
		return
	fi

	if lx.symbol<>kendsym then
		serror("'End' expected")
	fi

	if lx.subcode then
		if lx.subcode in [endkwd1, endkwd2] then
			lex()
			return
		else
error:
			strcpy(str, "Mismatched end ")
			if startline then
				fprint @(&.str+strlen(&.str)), " (from line #)", startline
			fi
			serror(&.str)
		fi
	fi

!only end was used, so skip that now
	lex()

!now, should be semi, or possibly kwd1/2
	if lx.symbol in [endkwd1, endkwd2] then
		lex()
!	elsif lx.symbol<>semisym then
!		error
	fi
end

func readvardef(symbol owner, int scope=0, isstatic=0, varid=staticid, k)unit=
!positioned at symbol following 'mut' or 'let', which will at the first symbol of
!the type, or at the first name being defined if there is no type
!k is the keyword symbol used (let/mut), or set to 0 if no keyword has been used, 
!then mut is assumed

!read vars inside module or proc
!isglobal must be 0 for procs
!isstatic must be 1 for modules
!varid must be frameid[let]/staticid[let] for procs, otherwise staticid[let]

	unit ulist, ulistx, p
	int nvars, m, initcode
	symbol stname

	ulist:=ulistx:=nil

	if istypestarter() then
		m:=readtypespec(owner)
	else
		serror("Readvar?")
	fi

	nvars:=0
	while lx.symbol=namesym do

		++nvars
		stname:=getduplnameptr(owner, lx.symptr, varid)

		stname.scope:=scope

		stname.isstatic:=isstatic

		stname.islet:=(k=kletsym)
		if varid=dllvarid then
			stname.isimport:=1
		fi

		adddef(owner, stname)
		if varid=staticid then
			addstatic(stname)
		fi

		lex()

		storemode(owner, m, stname.mode)

		if lx.symbol in [assignsym, eqsym] then

!			initcode:=case lx.symbol when eqsym then 1 when assignsym then 2 else 3 esac
			case lx.symbol
			when eqsym then initcode:=1
			when assignsym then initcode:=2
			else initcode:=3
			esac
			stname.used:=1

			if lx.symbol<>eqsym then
				if varid=staticid then
					serror("Non-variants can't use :=")
					if owner.nameid=procid then
						serror("Can't use := for statics inside procs")
					fi
					
				fi
			else
				if varid=frameid then
					serror("Need 'static' for '='")
					addstatic(stname)
				fi
			fi
			lex()

			stname.code:=readunit()

			stname.equals:=initcode
			if varid=frameid then
				p:=createunit2(jassign, createname(stname), stname.code)
				p.initlet:=1
				addlistunit(ulist, ulistx, p)
			fi

		elsif lx.symbol=atsym then
			if k=kletsym then serror("let@") fi
			lex()
			stname.atvar:=1
			stname.equivvar:=readunit()
		elsif k=kletsym then
			serror("let needs :=/=")
		fi

		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od

	if nvars=0 then
		serror("No vars declared")
	fi
	return ulist
end

proc readconstdef(symbol owner, int scope=0)=
!at 'const' symbol
	int nconsts, deft, m
	symbol stname

	lex()

	nconsts:=0

	if istypestarter() then
		deft:=readtypespec(owner)
	else
		deft:=tauto
	fi

	while lx.symbol=namesym do
		stname:=getduplnameptr(owner, lx.symptr, constid)

		lex()

		checkequals()
		lex()
		stname.code:=readconstexpr(1)

		m:=deft

		storemode(owner, m, stname.mode)
		++nconsts

		stname.scope:=scope

		adddef(owner, stname)
		if scope=export_scope and stname.name^<>'$' then
			addexpconst(stname)
		fi

		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od

	if nconsts=0 then
		serror("No consts declared")
	fi

end

func readlbrack:unit=
!positioned at "("
!termsym is rbracksym
!read one of the following:
! (x)		simple expression
! ()		list with no elements
! (x, )		list with one element
! (x, x, ...)		list
! (x|x|x])		if then else fi
! (x|x, ... |x])	select then else end

!return positioned at symbol following closing ")"
!listtag is jmakelist or jmakearray if 'array' was used

	unit ulist, ulistx, p, q, r, plower
	int oldirp, length, usecomma

	lex()					!first symbol of first expression
	ulist:=ulistx:=nil
	plower:=nil
	length:=0

	if lx.symbol=atsym then			!lwb override
		lex()
		oldirp:=inreadprint
		inreadprint:=1
		plower:=readunit()

		inreadprint:=oldirp
		checksymbollex(colonsym)
!		lex()

	elsif lx.symbol=intconstsym and nextlx.symbol=colonsym then
		plower:=createconstunit(lx.value, lx.subcode)
!		plower.istrueconst:=1
		lex()
		lex()

	elsif symboloptypes[lx.symbol]=bin_op and nextlx.symbol=rbracksym then	!operator constant
		p:=createunit0(joperator)
		p.opcindex:=lx.subcode
		lex()
		lex()
		return p
	elsif symboloptypes[lx.symbol]=bin_op and nextlx.symbol=assignsym then	!operator:= constant
		p:=createunit0(joperator)
		p.pclop:=symbolgentoops[lx.symbol]
		lex()			!read :=
		lexchecksymbol(rbracksym)
		lex()
		return p
	fi

!check symbol after "("
	case lx.symbol
	when rbracksym then			!empty list
		lex()
		p:=createunit0(jmakelist)
		p.b:=plower
		p.length:=0
		return p
	else					!assume normal expression follows
		p:=readunit()
	esac

!check symbol after "(expr"
	case lx.symbol
	when rbracksym then			!simple (x) expression
		lex()

		return p

	when commasym then			!separate by comma or implicit newline
		usecomma:=1
		if nextlx.symbol=rbracksym then		!means one-element list
			lex()
			lex()
			p:=createunit1(jmakelist, p)
			p.length:=1
			p.b:=plower
			return p
		fi
docomma:						!entry from implicit newline
		length:=1

!must be regular list
		ulist:=ulistx:=p

		if usecomma then
			repeat
				lex()							!skip comma
				if lx.symbol=rbracksym then		!allow , ) to end list
					exit
				fi
				if lx.symbol=commasym then
					serror(", , null expr not allowed")
				fi
				addlistunit(ulist, ulistx, readunit())
				++length
				skipsemi()
			until lx.symbol<>commasym
		else

			repeat
				skipsemi()
				if lx.symbol=rbracksym then		!allow , ) to end list
					exit
				fi
				if lx.symbol=commasym then
					serror(", , null expr not allowed")
				fi
				addlistunit(ulist, ulistx, readunit())
				++length
			until lx.symbol<>semisym
		fi

		checksymbollex(rbracksym)
!		lex()
		p:=createunit1(jmakelist, ulist)
		p.length:=length
		p.b:=plower
		return p

	when barsym then			!ifx/selectx expression; p is selector expression
		lex()
		q:=readunit()
		case lx.symbol
		when barsym then		!(a|b|c)
			lex()
			r:=readsunit()
			checksymbollex(rbracksym)
!			lex()
			p:=createunit3(jif, fixcond(p), q, r)
			p.compactif:=1
			return p
		when rbracksym then
			lex()
			p:=createunit3(jif, fixcond(p), q, nil)
			p.compactif:=1
			return p
		esac

!assume selectx expression
		addlistunit(ulist, ulistx, q)	!start with one-element list
		checksymbol(commasym)
		if nextlx.symbol<>barsym then		!(n|a, | using one-element list; not useful but allow it...
			repeat
				lex()				!skip comma
				addlistunit(ulist, ulistx, readunit())
			until lx.symbol<>commasym
			checksymbol(barsym)
		else
			lex()					!skip |
		fi
		lex()
		r:=readunit()
		checksymbollex(rbracksym)
!		lex()
		return createunit3(jselect, p, ulist, r)

	when semisym then
		if lx.subcode=1 then
			usecomma:=0
			goto docomma
		fi
		ulist:=ulistx:=p
		repeat
			skipsemi()
			if lx.symbol=rbracksym then
				exit
			fi
			addlistunit(ulist, ulistx, readunit())
!			skipsemi()						!allow a, b, c;) (works better with a, b, c\ followed by comment on next line followed by ")")
		until lx.symbol<>semisym
		checksymbollex(rbracksym)
!		lex()

		return makeblock(ulist)


	else
		serror("(x ...")
	esac
	return nil
end

proc addlistparam(ref symbol ulist, ulistx, symbol p)=
!add unit p to unit structure ulist, ^ulistx  which can be null
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx^.nextparam:=p
	fi
	ulistx^:=p			!update end-of-list pointer
end

func readcast:unit=
!also reads standalone type value
!t<>tvoid means already has ty[e
	unit p
	int opc, t

	t:=readtypespec(currproc)

	case lx.symbol
	when rbracksym then
		p:=createunit0(jtypeconst)
		p.mode:=ttype
		p.value:=t
		return p

	when atsym then
		opc:=jtypepun
		lex()
	when dotsym then			!allow T.type, but also just T (followed by . which
								!might be T.min etc)
		if nextlx.symbol=ktypesym then
			lex()
			p:=createunit0(jtypeconst)
			p.value:=t
			p.mode:=ttype
			lex()
		else					!leave dot to be processed by caller
			p:=createunit0(jtypeconst)
			p.value:=t
		fi
		return p
	else
		opc:=jconvert
	esac

	checksymbollex(lbracksym)
!	lex()
	p:=readunit()
	checksymbollex(rbracksym)
!	lex()

	p:=createunit1(opc, p)
	storemode(currproc, t, p.convmode)
	return p
end

func readopc:unit=
!op sym seen just before a term
	unit p, q, r
	int tag, opc, firstsym

	firstsym:=lx.symbol

	case lx.symbol
	when mathsopsym then
		tag:=junary
		opc:=lx.subcode
	when maths2opsym then
		tag:=jbin
		opc:=lx.subcode
	else
		tag:=junary
		opc:=symbolgenops[firstsym]
	esac

	lex()
	case firstsym
	when addsym then			!ignore +
		return readterm2()
	when subsym then			!convert minus to negate
		opc:=kneg
	when minsym, maxsym, maths2opsym, 
iandsym, iorsym, ixorsym then
		p:=readterm2()

		if p.tag=jmakelist then
			if p.length<>2 then serror("Needs (x, y)") fi
			q:=p.a
			r:=q.nextunit
			q.nextunit:=nil
			p:=createunit2(jbin, q, r)
			p.pclop:=opc
			return p
		else		!assume single pclopnd
			SERROR("READOPC/SINGLE OPND?")
			return createunit1(opc, p)

		fi
	else
		if symboloptypes[firstsym]=bin_op then
			serror("Can't be used as unary op")
		fi

	esac

	if lx.symbol=assignsym then	!op:=, not normally allowed inside expressions
		lex()
		tag:=junaryto
		case firstsym
		when subsym then
			opc:=knegto
		else
			opc:=symbolgentoops[firstsym]
			if opc=0 then
				serror("op:= not available")
			fi
		esac
	fi

	p:=createunit1(tag, q:=readterm2())

	p.pclop:=opc

	if q.tag=jmakelist then
		serror("Too many opnds")
	fi

	return p
end

func readcompilervar:unit=
	[100]char str
	rsystemtime tm
	static []ichar monthnames=("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
	unit p
	imodule currmodule:=modules[currmoduleno]

	switch lx.subcode
	when jcvnil then
		p:=createconstunit(0, tref)
		lex()
		return p

	when jcvpi then
!		p:=createconstunit(i64@(3.14159'265358'979'3238'4626'433'832), treal)
		p:=createconstunit(i64@(pi), treal)
		lex()
		return p

	when jcvinfinity then
		p:=createconstunit(i64@(infinity), treal)
		lex()
		return p

	when jcvlineno then
!		pc_gen(kloadimm, getlineno(lx.pos)
		p:=createconstunit(getlineno(lx.pos), ti64)
!		p:=createunit0(jcvlineno)
		lex()
		return p

	when jcvstrlineno then
		getstrint(getlineno(lx.pos), &.str)

	when jcvmodulename then
		strcpy(str, stmodule.name)

	when jcvfilename then
		strcpy(str, sources[currmodule.fileno].filespec)

	when jcvfunc then
		strcpy(&.str, currproc.name)

	when jcvdate then
		os_getsystime(&tm)
		fprint @&.str, "#-#-#", tm.day, monthnames[tm.month], tm.year:"4"

	when jcvtime then
		os_getsystime(&tm)
		fprint @&.str, "#:#:#", tm.hour:"z2", tm.minute:"z2", tm.second:"z2"

	when jcvversion then
		strcpy(&.str, "Compiler:M6.4")

	when jcvtrue, jcvfalse then
		p:=createconstunit(lx.subcode=jcvtrue, tbool64)
		lex()
		return p
	
	else
		serror_s("compiler var not impl: #", jtagnames[lx.subcode])
	end switch
	lex()

	return createstringconstunit(pcm_copyheapstring(&.str), -1)
end

func readcastx:unit=
!explicit cast using syntax:
! cast(expr)
! cast(expr, type)
! cast@(expr, type)
!at 'cast'
	int opc, m
	unit p

	lex()
	opc:=jconvert
	if lx.symbol=atsym then
		opc:=jtypepun
		lex()
	fi
	checksymbollex(lbracksym)
!	lex()
	m:=tvoid
	p:=readunit()
	if lx.symbol<>commasym then
		if opc=jtypepun then serror("@ type missing") fi
		opc:=jautocast
	else
		lex()
		m:=readtypespec(currproc)
	fi
	checksymbollex(rbracksym)
!	lex()

	p:=createunit1(opc, p)
	storemode(currproc, m, p.convmode)

	return p
end

global proc checksymbol(int symbol)=
	[100]char str

	if lx.symbol<>symbol then
		fprint @&.str, "# expected, not #", symbolnames[symbol], symbolnames[lx.symbol]
		serror(&.str)
	fi
end

global proc lexchecksymbol(int symbol)=
	lex()
	checksymbol(symbol)
end

global proc checksymbollex(int symbol)=
	checksymbol(symbol)
	lex()
end

global func readtypespec(symbol owner, int typedefx=0)int=
!at initial symbol of a type, or where type is expected
!read simple type (which will have a single name) or a more elaborate type-spec
!returns a moderec handle
!typedefx is not a def, but either:
! moderec	Used when called from readtypedef. This is then filled in with the
!		details of the new mode, and the new version is returned
! nil		Called from outside readtypedef; then just returns a new moderec

!If the first symbol is not a stdtype, then it is assumed to be a usertype
!For stdtypes, I might implement :N and *N width-specifiers, as an alternative to just
!using i16 etc
	symbol d, e
	int t, kwd, sltype, w
	unit x, pupper, plx
	unit dim, length
	const maxdim=30
	[maxdim]unit dims
	int ndims, i, n, k

	case lx.symbol
	when lsqsym then		!array bounds
arraybounds:
		lex()

		ndims:=0
		inreadprint:=1
		do
			length:=nil				!both bounds unspecified
			if lx.symbol=rsqsym or lx.symbol=commasym then		![]
				dim:=nil
			else
				dim:=readunit()
				case lx.symbol
				when rsqsym, commasym then			![n]
				when colonsym then				!a:n
					lex()
					if not (lx.symbol=commasym or lx.symbol=rsqsym) then	!lower:length
						length:=readunit()
						dim:=createunit2(jkeyvalue, dim, length)
					else													!lower:
						dim:=createunit1(jkeyvalue, dim)
					fi
				esac
			fi
			if ndims>=maxdim then serror("Too many array dims") fi
			dims[++ndims]:=dim
			exit when lx.symbol<>commasym
			lex()
		od
		inreadprint:=0
		checksymbollex(rsqsym)
!		lex()
		t:=readtypespec(owner)

		for i:=ndims downto 1 do
			t:=createarraymode(owner, t, dims[i], (i=1|typedefx|0))
		od
		return t

	when stdtypesym then
		t:=lx.subcode
		lex()

	when namesym then
		d:=lx.symptr
		lex()

		if lx.symbol=dotsym then
			lexchecksymbol(namesym)
			t:=newtypename(d, lx.symptr)
			lex()
		else
			t:=newtypename(nil, d)
		fi

	when krecordsym, kstructsym then
		serror("Use 'record name =' syntax")

	when kunionsym then
		serror("Top-level union not allowed")

	when krefsym then		!ref T
	retry:

		lex()
		if lx.symbol=ktosym then lex() fi

		case lx.symbol
		when kprocsym, kfuncsym then	!func pointer being created
			t:=readrefproc(owner, typedefx)

		when stdtypesym then
			case lx.subcode
			when tc8 then
				t:=trefchar
				if typedefx then tttarget[typedefx]:=tc8 fi
			else
				goto readtarget
			esac

			lex()

		when kvoidsym then
			lex()
			t:=tvoid
			gottarget
		else						!assume normal type
readtarget:
			t:=readtypespec(owner)
gottarget:
			t:=createrefmode(owner, t, typedefx)
		esac

	when kicharsym then
		if lx.subcode=tc8 then
			t:=trefchar
		else
			t:=tref
		fi
		if typedefx then tttarget[typedefx]:=lx.subcode fi
		lex()

	when kslicesym then
		t:=readslicetype(owner, lx.subcode, typedefx)

	else
		serror("Bad type starter")
	esac

	if typedefx then			!assume a simple alias
		ttbasetype[typedefx]:=ttbasetype[t]
	fi

	return t
end

func readslicetype(symbol owner, int slicetype, typedefx)int=
!positioned at 'slice'
!dim is nil, or lower-bound expression
	unit plower
	int t

	lexchecksymbol(lsqsym)
	lex()
	if lx.symbol<>rsqsym then
		inreadprint:=1
		plower:=readunit()
		inreadprint:=0
		checksymbol(colonsym)
		lexchecksymbol(rsqsym)
	else
		plower:=nil
	fi
	lex()
	t:=readtypespec(owner, typedefx)

	return createslicemode(owner, slicetype, t, plower, typedefx)
end

func readslist(int iscall=0, donulls)unit=
!read comma-separated list of expressions
!positioned at first symbol of first expression
! it might be | or )
!
!donulls=1 means empty expressions are allowed (just comma or terminator, which
!result in null units
!return with symbol at terminating symbol: 1st non comma and is that a unit starter
!iscall=1 when called to read a func-call parameter list; then key:value pairs
!are treated as keyword arguments
!eg: (a, b, c	)
!eg: (a		!
	unit ulist, ulistx
	int oldinparamlist

	ulist:=ulistx:=nil

	skipsemi()
	if lx.symbol=rbracksym then		!empty list
		return ulist
	fi

	oldinparamlist:=inparamlist
	inparamlist:=iscall

	do
		skipsemi()
		case lx.symbol
		when commasym then
			if donulls then
				addlistunit(ulist, ulistx, createunit0(jnull))
			else
				serror("null comma expr not allowed")
			fi
			lex()
		when rbracksym then
			if donulls then
				addlistunit(ulist, ulistx, nullunit)
			fi
			exit
		else
			addlistunit(ulist, ulistx, readunit())
			if lx.symbol in [commasym, semisym] then
				lex()
				if lx.symbol=rbracksym then
					exit
				fi
			else
				skipsemi()
				if lx.symbol=rbracksym then
					exit
				fi
				serror("SLIST?")
			fi
		esac
	od
	inparamlist:=oldinparamlist

	return ulist
end

func readindex(unit p, int dot)unit=
!at '['; dot=0/1 for a[]/a.[]
!syntax is:
![x] or [x, ...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
	unit q, plower, pupper

	lex()

	if not dot then
		case lx.symbol
		when rsqsym then
	fullslice:
			lex()
			plower:=createunit1(junary, duplunit(p))
			plower.pclop:=kklwb
			pupper:=createunit1(junary, duplunit(p))
			pupper.pclop:=kkupb
			p:=createunit2(jslice, p, createunit2(jmakerange, plower, pupper))
			return p
		when rangesym, colonsym then
			lexchecksymbol(rsqsym)
			goto fullslice
		esac
	fi

	do
		if ndollar>=maxdollarstack then
			serror("Too many nested a[$]")
		fi
		dollarstack[++ndollar]:=p
		q:=readunit()
		--ndollar

		if q.tag=jmakerange then		!convert into a discrete slice
			p:=createunit2((dot|jdotslice|jslice), p, q)
		else
			p:=createunit2((dot|jdotindex|jindex), p, q)
		fi

		exit when lx.symbol<>commasym
		lex()
	od
	checksymbollex(rsqsym)
!	lex()
	return p
end

func readdotsuffix(unit p)unit=
!at '.' symbol
!read any modifiers for term currently in p
!multiple .terms can be present
	unit q, r, p2

	while lx.symbol=dotsym do
		lex()
		case lx.symbol
		when lsqsym then
			p:=readindex(p, 1)
		when namesym then
			p:=createunit2(jdot, p, createname(lx.symptr))
			lex()
		when propsym then
			if lx.subcode=kkbounds then
				q:=createunit1(jprop, duplunit(p))
				r:=createunit1(jprop, duplunit(p))
				if p.tag=jtypeconst then
					q.propcode:=kkminval
					r.propcode:=kkmaxval
				else
					q.propcode:=kklwb
					r.propcode:=kkupb
				fi

				p2:=createunit2(jmakerange, q, r)
				deleteunit(p, p2)
			else
	doprop:
				p:=createunit1(jprop, p)
				p.pclop:=lx.subcode
			fi
			lex()

		when bitfieldsym then
			p:=createunit1(jbitfield, p)
			p.bfcode:=lx.subcode
			lex()
		when ktypesym then			!.type, convert to .gettype
			case p.tag
			when jtypeconst then			!int.type=>int

			else
				SERROR("RDS:TYPEOF")
!				p:=createunit1(jtypeof, p)
			esac
			lex()

		when maxsym then
			lx.subcode:=kkmaxval
			goto doprop

		when minsym then
			lx.subcode:=kkminval
			goto doprop
		when stdtypesym then
			if p.tag=jtypeconst and lx.subcode=trange then
				q:=createunit2(jmakerange, 
					createunit1(junary, p), 
					createunit1(junary, p))
				q.a.propcode:=kkminval
				q.b.propcode:=kkmaxval
			else
				error
			fi
			lex()
			p:=q



		else
	error:
			serror("Unknown dot suffix")
		esac
	od
	return p
end

func readconstexpr(int needconst=1)unit=
	return readunit()
end

func readconstint:int=
!read expression that must yield a constant int value *now*; return value
	i64 x

!keep it simple for now
	if lx.symbol=intconstsym then
		x:=lx.value
		lex()
		return x
	elsif lx.symbol=subsym then
		lex()
		if lx.symbol=intconstsym then
			x:=lx.value
			lex()
			return -x
		fi
	fi

!later can only arbitrary expressions, provided they can be evaluated in this pass
	serror("Can't do complex expr")
	return 0
end

proc readprocdef(symbol procowner, int scope)=
!at 'proc' etc symbol; read proc def or declaration
!syntax:
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
	int kwd, startline, closesym, shortfun
	symbol stproc, stname

	kwd:=lx.symbol
	shortfun:=lx.subcode=1
	nforloops:=0

	stproc:=readprocdecl(procowner, scope)
	checkequals()

	lex()

	startline:=getcurrline()

	if not shortfun then
		closesym:=checkbegin(0)
	fi

	pushproc(stproc)
	nextavindex:=0

	IF DRETVAR THEN
		stname:=getduplnameptr(stproc, dretvar, frameid)
		storemode(procowner, stproc.mode, stname.mode)
		adddef(stproc, stname)
	fi

	addtoproclist(stproc)

	if shortfun then
		stproc.code:=readunit()
		checksymbollex(semisym)
	else
		stproc.code:=readsunit()
		checkbeginend(closesym, kwd, startline)
	fi

	stproc.code:=makeblock(stproc.code)

	popproc()
end

global func readprocdecl(symbol procowner, int scope)symbol=
!at 'proc'  or 'func' 
!read proc declaration only, so exit at "=" or ";" symbol
!syntax:
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
!return st entry of proc, and positioned at '=' or semi

	int kwd, varparams, nparams, nretvalues, isthreaded
	int subprogno
	[maxtuplesize]int retmodes
	imodule ms
	isubprog ps

	ichar metadata, truename
	symbol pequiv, stproc, owner, paramlist, nameptr

	kwd:=lx.symbol				!remember keyword
	isthreaded:=lx.subcode=2

	pequiv:=nil
	metadata:=""
	truename:=nil
	varparams:=0

	lex()

	if lx.symbol=stringconstsym then		!assume dll truename
		truename:=pcm_copyheapstring(lx.svalue)
		convlcstring(lx.svalue)
		lx.symptr:=addnamestr(lx.svalue)
	else
		checksymbol(namesym)
	fi

	nameptr:=lx.symptr

	stproc:=getduplnameptr(procowner, nameptr, (insidedllimport|dllprocid|procid))
	if insidedllimport then scope:=subprog_scope fi
	stproc.isthreaded:=isthreaded

	if truename then
		stproc.truename:=truename
	fi

	adddef(procowner, stproc)
	if stproc.nameid=dllprocid then
		stproc.isimport:=1
	fi

	owner:=stproc
	pushproc(stproc)

	lex()
	if lx.symbol=mulsym then
		stproc.ishandler:=1
		lex()
	fi

	paramlist:=nil
	retmodes[1]:=tvoid
	nparams:=0
	nretvalues:=0

	nretvalues:=0
	if lx.symbol=lbracksym then		!possible params
		lex()
		if lx.symbol<>rbracksym then
			paramlist:=readparams(procowner, stproc, varparams, nparams)
			checksymbol(rbracksym)
		fi
		lex()

		if lx.symbol=colonsym or lx.symbol=sendtosym then
			lex()
			nretvalues:=readreturntype(owner, retmodes)
		elsif typestarterset[lx.symbol] or lx.symbol=namesym then
			nretvalues:=readreturntype(owner, retmodes)
		fi
	elsif lx.symbol=colonsym or lx.symbol=sendtosym then
		lex()
		nretvalues:=readreturntype(owner, retmodes)
	fi

	dretvar:=nil
	if nretvalues=1 then
		if lx.symbol=namesym then
			dretvar:=lx.symptr
			lex()
		fi
	fi

	unless nretvalues or (kwd<>kfuncsym) then		!func: no result given
		serror("Function needs ret type")
	end unless

	if nretvalues and (kwd<>kfuncsym) then		!proc: result given
		serror("Proc can't return value")
	fi

	stproc.paramlist:=paramlist
	stproc.nretvalues:=nretvalues

	case nretvalues
	when 0 then
		stproc.mode:=tvoid
	when 1 then
		storemode(procowner, retmodes[1], stproc.mode)
	else
		stproc.mode:=createtuplemode(procowner, retmodes, nretvalues, 0)
	esac

	if lx.symbol=atsym then			!equivalence
		SERROR("READPROCDEF @")
		lexchecksymbol(namesym)
		lex()
		stproc.atvar:=1
	fi

	stproc.code:=nil

	stproc.scope:=scope
	stproc.varparams:=varparams

	if procowner=stmodule then
		if stproc.namelen=5 and eqstring(stproc.name, "start") then
			modules[stmodule.moduleno].ststart:=stproc
			stproc.scope:=subprog_scope
			dosigcheck
		elsif stproc.namelen=4 and eqstring(stproc.name, "main") then
			ms:=modules[stmodule.moduleno]
			ps:=subprogs[stmodule.subprogno]

			if ps.mainmodule then serror("More than one main() in SP") fi
			ps.mainmodule:=stmodule.moduleno
			ms.stmain:=stproc

			if ps.subprogno=mainsubprogno then
				stproc.scope:=export_scope
dosigcheck:
				if stproc.paramlist or stproc.mode<>tvoid then
					serror("Wrong 'main/start' sig")
				fi

			fi
		fi
	fi

	popproc()

	return stproc
end

func readparams(symbol procowner, owner, int &varparams, &nparams)symbol=			!READPARAMS
!positioned at first symbol after '('; this is not ')'
!read list of params, return that list
!syntax is a list of names and/or types
!each param can optionally be followed by a default value
!finish pointing at ")"
	symbol stlist, stlistx, stname
	int parammode, pmode, m, isoptional, types

	stlist:=stlistx:=nil
	pmode:=tvoid
	nparams:=0
	parammode:=byval_param
	types:=0

	if lx.symbol=namesym and nextlx.symbol in [commasym, rbracksym] then
		types:=1
	fi

	do										!expect type of name at start of loop
		parammode:=byval_param
		isoptional:=0

		if types or istypestarter() then				!assume new mode
			pmode:=readtypespec(procowner)
gotmode:

			if nparams=0 and lx.symbol in [commasym, rbracksym] then
				do
					[32]char str
					++nparams
					str[1]:='$'; str[2]:=0
					strcat(str, strint(nparams))
					stname:=getduplnameptr(owner, addnamestr(&.str), paramid)
					adddef(owner, stname)

					storemode(owner, pmode, stname.mode)
					stname.parammode:=parammode
					addlistparam(&stlist, &stlistx, stname)

					case lx.symbol
					when rbracksym then
						exit
					esac

					checksymbollex(commasym)
!					lex()
					if lx.symbol=ellipsissym then
						varparams:=nparams
						lex()
						exit
					fi

					pmode:=readtypespec(procowner)
				od
				return stlist
			fi

		elsif pmode=tvoid then
			serror("Type expected")
		fi

		case lx.symbol
		when addrsym then
			parammode:=byref_param
			lex()
			if lx.symbol=colonsym then lex() fi
		when ellipsissym then
			varparams:=nparams
			lex()
			return stlist
		esac

		checksymbol(namesym)
		++nparams
		stname:=getduplnameptr(owner, lx.symptr, paramid)
		adddef(owner, stname)
		lex()

		if parammode=byref_param then
			m:=createrefmode(procowner, pmode)
		else
			m:=pmode
		fi

		storemode(owner, m, stname.mode)
		stname.parammode:=parammode
		stname.optional:=isoptional
		addlistparam(&stlist, &stlistx, stname)

		case lx.symbol
		when assignsym, eqsym then
			lex()
			stname.code:=readunit()
			stname.equals:=1
			stname.optional:=1
		esac

		case lx.symbol
		when commasym then
			lex()
		when rbracksym then
			exit
		else
			serror("nameparams1")
		esac
	od

return stlist
end

func readcondsuffix(unit p)unit=
!p is a unit just read
!positioned at following symbol
!check whether a conditional suffix follows, and return p wrapped in a conditional if so
! ... if cond
! ... when cond
! ... unless cond
	unit q

	case lx.symbol
	when kwhensym then
		lex()
		return createunit2(jif, fixcond(readunit()), createunit1(jblock, p))
	when kunlesssym then
		lex()
		q:=createunit1(jnotl, fixcond(readunit()))
		q.pclop:=knot
		return createunit2(jif, q, createunit1(jblock, p))
	else
		return p
	esac
end

func readif:unit=
!at 'if'
	int pos1, kwd
	unit clist, clistx, plist, plistx, pelse, p

	pos1:=lx.pos
	kwd:=lx.symbol			!in case coming from elsecase etc
	lex()
	skipsemi()

	clist:=clistx:=plist:=plistx:=pelse:=nil

	if lx.symbol=kelsifsym then
		lex()
	fi
	nextif

	repeat
		lex()
nextif:
		addlistunit(clist, clistx, fixcond(readsunit()))

		skipsemi()
		checksymbollex(kthensym)

		if lx.symbol=colonsym then
			if clist=clistx and kwd=kifsym then
				lex()
				p:=createunit3(jif, clist, readunit(), nil)
				p.pos:=pos1
				return p
			else
				serror("then: not allowed")
			fi
		fi

		addlistunit(plist, plistx, readsunit())
		skipsemi()

	until lx.symbol<>kelsifsym

	case lx.symbol
	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()
		checkend(kendsym, kwd, 0)
	when kelsecasesym, kelseswitchsym then
		lx.symbol:=kwd
		pelse:=makeblock(readswitchcase())
	else
		checkend(kendsym, kwd, 0)
	esac

	p:=createunit3(jif, clist, plist, pelse)
	p.pos:=pos1
	return p
end

func readgoto(int gototag=jgoto)unit=
	lex()

	return readcondsuffix(createunit1(gototag, readunit()))
end

func readunless:unit=
	int pos
	unit pcond, pthen, pelse, p, q

	pos:=lx.pos
	lex()
	pcond:=fixcond(readsunit())
	checksymbollex(kthensym)
!	lex()

	pthen:=readsunit()

	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
	else			!assume simple if-then
		PELSE:=NIL
	fi
	checkend(kendsym, kunlesssym)
	p:=createunit3(jif, q:=createunit1(jnotl, pcond), pthen, pelse)
	q.pclop:=knot
	p.pos:=pos
	return p
end

func readswitchcase:unit=
	int pos1, kwd, opc, pos2, rangeused, nwhen
	unit pexpr, pwhenlist, pwhenlistx, pwhen, pwhenx, pelse, p, pthen, pwhenthen, pjump

	pos1:=lx.pos
	kwd:=lx.symbol			!remember kcasesym etc

	opc:=lx.subcode			!pick up tag: kcase etc
	pjump:=nil
!
	lex()

	skipsemi()

	if opc=jdoswitchx then
		checksymbollex(lbracksym)
		pjump:=readunit()
		checksymbollex(rbracksym)
		currproc.hasdoswx:=1
	fi

	if lx.symbol=kwhensym then
		if kwd=kswitchsym then
			serror("switch expr missing")
		fi
		pexpr:=nil
	else
		pexpr:=readsunit()			!index expression
		pexpr.nextunit:=pjump		!for doswitchx
	fi

	pwhenlist:=pwhenlistx:=nil
	rangeused:=0
	nwhen:=0

	skipsemi()
	while lx.symbol=kwhensym do	!read list of when-then pairs
		pos2:=lx.pos
		lex()
		pwhen:=pwhenx:=nil
		do
			p:=readunit()
			++nwhen
			p.pos:=pos2
			if p.tag=jmakerange then rangeused:=1 fi
			addlistunit(pwhen, pwhenx, p)
			if lx.symbol<>commasym then exit fi
			lex()
		od
		if lx.symbol<>sendtosym then
			checksymbol(kthensym)
		fi
		lex()
		pthen:=readsunit()
		pwhenthen:=createunit2(jwhenthen, pwhen, pthen)
		pwhenthen.pos:=pos2
		addlistunit(pwhenlist, pwhenlistx, pwhenthen)
	od

	if opc=jswitch and not rangeused then
		if nwhen<=8 then
			opc:=jcase
		fi
	fi

	case lx.symbol
	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()

		checkend(kendsym, kwd)
	when kelsifsym then
		lx.symbol:=kwd
		pelse:=makeblock(readif())
	when kelsecasesym, kelseswitchsym then
		lx.symbol:=kwd
		pelse:=makeblock(readswitchcase())
	else
		PELSE:=NIL
		checkend(kendsym, kwd)
	esac

	p:=createunit3(opc, pexpr, pwhenlist, pelse)
	p.pos:=pos1

	return p
end

func readstop:unit=
	unit p
	int i
	lex()
	if exprstarter[lx.symbol] then
		p:=createunit1(jstop, readunit())
	else
		p:=createunit0(jstop)
	fi
	return readcondsuffix(p)
end

func readreturn:unit=
	unit p, q

	lex()
	if exprstarter[lx.symbol] then
		q:=readunit()
		p:=createunit1(jreturn, q)
		p.length:=1
	else
		p:=createunit0(jreturn)
		p.length:=0
	fi

	return readcondsuffix(p)
end

func readdo:unit=
	unit p
	int pos

	pos:=lx.pos
	lex()
	p:=readsunit()
	checkend(kendsym, kdosym)
	p:=createunit1(jdo, p)
	p.pos:=pos
	return p
end

func readto:unit=
	int pos, id
	unit p, pcount, pbody

	pos:=lx.pos
	lex()

	pcount:=readunit()

	checksymbollex(kdosym)
	pbody:=readsunit()
	checkend(kendsym, ktosym, kdosym)
	id:=frameid
	if currproc.nameid<>procid then id:=staticid fi

!	p:=createunit3(jto, pcount, pbody, createname(getavname(currproc, id)))
	p:=createunit2(jto, pcount, pbody)
	p.pos:=pos
	return p
end

func readwhile:unit=
	int pos
	unit pcond, pbody, pincr, p
	pos:=lx.pos
	lex()


	pcond:=fixcond(readsunit(1))
	pincr:=nil

	if lx.symbol=commasym then
		lex()
		pincr:=readsunit(1)
	fi

	checksymbollex(kdosym)
!	lex()
	pbody:=readsunit()

	checkend(kendsym, kwhilesym, kdosym)

	p:=createunit3(jwhile, pcond, pbody, pincr)
	p.pos:=pos

	return p
end

func readrepeat:unit=
	int pos
	unit pbody, pcond, p

	pos:=lx.pos
	lex()
	pbody:=readsunit()
	checksymbollex(kuntilsym)
!	lex()
	pcond:=fixcond(readunit())
	p:=createunit2(jrepeat, pbody, pcond)
	p.pos:=pos

	return p
end

func readloopcontrol:unit=
	int opc
	unit p

	opc:=lx.subcode

	lex()
	if lx.symbol=namesym and eqstring(lx.symptr.name, "all") then
		lex()
		p:=createunit1(opc, createconstunit(0, tint))

	elsif exprstarter[lx.symbol] then
		p:=createunit1(opc, readconstexpr(1))
	else
		p:=createunit1(opc, createconstunit(1, tint))
	fi
	return readcondsuffix(p)
end

func readprint:unit=
	int oldinreadprint, opc, isfprint, fshowname
	unit pformat, pdev, printlist, printlistx, p, q
	ref strbuffer expr

	ichar s

	oldinreadprint:=inreadprint
	inreadprint:=1
	opc:=lx.subcode

	case opc
	when jfprint, jfprintln then
		isfprint:=1
	else
		isfprint:=0
	esac

	lex()

	printlist:=printlistx:=nil
	pformat:=pdev:=nil

	if lx.symbol=atsym then
		lex()
		pdev:=readunit()
		if lx.symbol=commasym then lex() else goto finish fi
	fi
	if isfprint then
		pformat:=readunit()
		if lx.symbol=commasym then lex() else goto finish fi
	fi

	if not exprstarter[lx.symbol] then
		goto finish
	fi

	do
		case lx.symbol
		when commasym then		!assume extra comma, meaning nogap
			addlistunit(printlist, printlistx, createunit0(jnogap))
		when dollarsym then		!assume extra comma, meaning nogap
			addlistunit(printlist, printlistx, createunit0(jspace))
			lex()

		else

			fshowname:=0
			if lx.symbol=eqsym then
				fshowname:=1
				lex()
			fi

			p:=readunit()
			if lx.symbol=colonsym then
				lex()
				p:=createunit2(jfmtitem, p, readunit())
			fi
			if fshowname then
				expr:=strexpr(p)
				strbuffer_add(expr, "=")
				s:=expr.strptr
				iconvucn(expr.strptr, expr.length)

				addlistunit(printlist, printlistx, q:=createstringconstunit(s, expr.length))
			fi
			addlistunit(printlist, printlistx, p)
		esac
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish:
	inreadprint:=oldinreadprint
	if opc=jprint and printlist=nil then
		serror("No print items")
	fi
	if opc=jfprint and printlist=nil and pformat=nil then
		serror("No print items")
	fi

	if isfprint then
		if pformat=nil then
			serror("No fmt str")
		fi
		return createunit3(opc, pdev, pformat, printlist)
	else
		return createunit2(opc, pdev, printlist)
	fi
end

func readread:unit=
	int oldinreadprint, opc
	unit pformat, pdev, readlist, readlistx, p, pread

	oldinreadprint:=inreadprint
	inreadprint:=1
	opc:=lx.subcode
	lex()

	readlist:=readlistx:=nil
	pformat:=pdev:=nil

	if lx.symbol=atsym then
		if opc=jread then
			serror("@ on read")
		fi
		lex()
		pdev:=readunit()
		if lx.symbol=commasym then lex() fi
	fi

	if opc=jreadln then
		addlistunit(readlist, readlistx, createunit1(jreadln, pdev))
	fi

	if not exprstarter[lx.symbol] then
		goto finish
	fi

	do
		p:=readunit()
		if lx.symbol=colonsym then
			lex()
			pformat:=readunit()
		else
			pformat:=nil
		fi

		pread:=createunit1(jread, pformat)

!

		p:=createunit2(jassign, p, pread)

		addlistunit(readlist, readlistx, p)
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish:
	inreadprint:=oldinreadprint
	if opc=jread and readlist=nil then
		serror("No read items")
	fi

	return createunit1(jblock, readlist)
end

func readfor:unit=
!on 'for'; syntax is:
! for [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/od
! for var[, var] in/inrev expr [when expr] do stmts [else stmts] end/od *FORALL*
! for var in/inrev expr.bounds [when expr] do stmts [else stmts] end/od
! for var in/inrev <rangeexpr> [when expr] do stmts [else stmts] end/od

!AV codes:
!	I	loop index, always i64; will be 'i' (declared or not declared) or autovar
!	L	forall local variable; will be 'x' (declared or not declared); type is variable

	int pos, opc
	unit pindex, plocal				!for index; for index, local
	unit pfrom, pto, pstep, ptoinit	!for INDEX:=FROM to/downto TO [by STEP]/ INDEX in FROM..TO
	unit plist, passign				!for INDEX in/inrev LIST (also LIST.BOUNDS)
	unit pcond, pbody, pelse
	unit p

	pos:=lx.pos
	lex()						!skip 'for' kwd

	plocal:=nil
	ptoinit:=nil
	pindex:=readname()

	if nforloops>=maxforloops then
		serror("Too many for-loops")
	fi
	for i to nforloops do
		if forindexvars[i]=pindex.def then
			serror("Re-using nested loop index")
		fi
	od
	forindexvars[++nforloops]:=pindex.def

	if lx.symbol=commasym then
		lex()
		plocal:=readname()
	fi

	opc:=jforup
	pstep:=nil
	pcond:=nil

	if lx.symbol in [insym, inrevsym] then				!assume forall
		if lx.symbol=jinrev then
			opc:=jfordown				!tentative; may be changed to forall
		fi
		lex()

		plist:=readunit()

		if plist.tag=jmakerange then
			pfrom:=plist.a
			pto:=plist.b
		else
			opc:=(opc=jforup|jforall|jforallrev)
			pfrom:=getrangelwbunit(duplunit(plist))
			pto:=getrangeupbunit(duplunit(plist))
		fi

	else
		if lx.symbol=assignsym then
			lex()
			pfrom:=readunit()
		else
			pfrom:=createconstunit(1, tint)
		fi
		checksymbol(ktosym)
		opc:=(lx.subcode=1|jfordown|jforup)
		lex()
		pto:=readunit()

		if lx.symbol=kbysym then
			lex()
			pstep:=readconstexpr(0)
			if pstep.tag=jconst then
				if pstep.value=1 then		!by 1
					pstep:=nil
				fi
			fi
		fi
	fi

	if lx.symbol=kwhensym then
		lex()
		pcond:=fixcond(readunit())
	fi
	checksymbollex(kdosym)
!	lex()
	pbody:=readsunit()
	pelse:=nil

	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
	fi
	checkend(kendsym, kforsym, kdosym)

!deal with complex limit
!problem: autovar for STEP only created when there is an autovar for TO

	if pcond<>nil then
		pbody:=makeblock(createunit2(jif, pcond, pbody))
	fi
	pbody.nextunit:=pelse

!forup/down layout
!	a:	pindex/ptoinit
!	b:	pfrom/pto/pstep
!	c:	pbody/pelse

!forall/rev layout
!	a:	pindex/plocal/pfrom/pto
!	b:	plist/passign
!	c:	pbody/pelse

	case opc
	when jforup, jfordown then
		if plocal then serror("for i, x?") fi
		pindex.avcode:='I'
!		if pto.tag not in [jconst, jname] then
!			plocal:=createname(getavname(currproc))
!			plocal.avcode:='I'
!			ptoinit:=createunit2(jassign, plocal, pto)
!			pindex.nextunit:=ptoinit
!			pto:=plocal
!		fi

		pfrom.nextunit:=pto
		pto.nextunit:=pstep
		p:=createunit3(opc, pindex, pfrom, pbody)

	else										!assume forall/rev

		if plocal=nil then						!only for x
			plocal:=pindex
			pindex:=createname(getavname(currproc))
		fi
		pindex.avcode:='I'
		plocal.avcode:='L'
		pindex.nextunit:=plocal
		plocal.nextunit:=pfrom
		pfrom.nextunit:=pto

		passign:=createunit2(jassign, duplunit(plocal), 
					createunit2(jindex, duplunit(plist), duplunit(pindex)))
		plist.nextunit:=passign

		p:=createunit3(opc, pindex, plist, pbody)

	esac

	p.pos:=pos
	--nforloops
	return p
end

func readname:unit p=
	p:=readterm2()
	if p.tag<>jname then serror("Name expected") fi
	return p
end

global proc readtypedef(symbol owner, int scope=0)=
!at 'type' symbol
	symbol sttype, stname
	int t, m

	lexchecksymbol(namesym)
	stname:=lx.symptr

	lex()
	checkequals()
	lex()

	sttype:=getduplnameptr(owner, stname, typeid)
	adddef(owner, sttype)
	m:=createusertype(sttype)
	ttusercat[m]:=1

	t:=readtypespec(sttype, m)		!should return filled-in version of m

	sttype.scope:=scope
	storemode(owner, t, sttype.mode)

	if t>=0 then
		if ttisinteger[t]+ttisreal[t] then
			tttarget[m]:=t
		elsif ttisref[t] then
		elsecase ttbasetype[t]
		when tarray then
		when tslice then
!		when tslice, tvector, tflex then
		when trecord then
		else
			tttarget[m]:=t
		fi
	else
		storemode(owner, t, tttarget[m])
	fi

	if t>=0 then
		copyttvalues(m, t)
	else
		ttbasetype[m]:=tpending
	fi
end

global proc readrecordfields(symbol owner, int m)=
!positioned at just after type m has been read
!read vars inside struct for one line of struct body
	int nvars, offset
	symbol stname, stbitfield

	nvars:=0
	while lx.symbol=namesym do

		stname:=getduplnameptr(owner, lx.symptr, fieldid)
		storemode(owner, m, stname.mode)
		++nvars

		if unionpend.ulength then
			unionstr_copy(&stname.uflags, &unionpend)
			unionstr_concat(&unionstring, &unionpend)
			unionstr_clear(&unionpend)
		else
			unionstr_clear(&stname.uflags)
		fi
		unionlastvar:=stname			!filled in from outside with 'E' codes

		adddef(owner, stname)

		lex()

		case lx.symbol
		when atsym then
			lex()
			stname.atfield:=1
			stname.equivfield:=readequivfield(owner)
			if lx.symbol=addsym then
				lex()
				offset:=readconstint()
				if offset>stname.equivoffset.max then serror("Offset>255") fi
				stname.equivoffset:=offset
			fi

		when colonsym then				!read bitfields
!format is int : (a:1, b:3, c:2)
			lexchecksymbol(lbracksym)

			repeat
				lexchecksymbol(namesym)
				stbitfield:=getduplnameptr(owner, lx.symptr, fieldid)
				stbitfield.mode:=tbitfield
				adddef(owner, stbitfield)

				stbitfield.atfield:=1
				stbitfield.equivfield:=stname

				lexchecksymbol(colonsym)
				lexchecksymbol(intconstsym)
				stbitfield.bitfieldwidth:=lx.value
				lex()

			until lx.symbol<>commasym
			checksymbollex(rbracksym)
!			lex()

		esac

		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od

	if nvars=0 then
		serror("No fields declared")
	fi
end

global proc readtabledef(symbol owner, int scope=0)=
!at 'tabledata' symbol
	int i, ncols, nrows, enums, nextenumvalue, firstval, lastval, startline, closesym
	int ltype
	symbol stvar, stenum, stgen
	const maxcols=20
	[maxcols]symbol varnameptrs
	[maxcols]int varlisttypes
	[maxcols]unit plist, plistx
	const maxrows=500
	[maxrows]int enumvalues

	enums:=lx.subcode				! means enumdata
	lex()

	tabledataname:=nil

	if lx.symbol=lbracksym then		!tabledata(...) read enum type
		if not enums then serror("Use 'enumdata'") fi
		enums:=1
		lex()
		checksymbollex(rbracksym)
!		lex()
	fi


	nextenumvalue:=1
	nrows:=0			!number of data rows appearing
	ncols:=0			!number of data columns (varnames appearing)

!loop reading variable names
	while lx.symbol<>eqsym do
		ltype:=readtypespec(owner)
		checksymbol(namesym)
		if ++ncols>maxcols then
			serror("tabledata/too many columns")
		fi
		varnameptrs[ncols]:=lx.symptr
		varlisttypes[ncols]:=ltype

		lex()
		if lx.symbol=commasym then
			lex()
		else
			exit
		fi
	od

	lex()					!skip =

	skipsemi()
	startline:=getcurrline()
	closesym:=checkbegin(0)

	skipsemi()
	firstval:=lastval:=0

	for i:=1 to ncols do
		plist[i]:=plistx[i]:=nil
	od

	intabledata:=1
	do			!loop per row
		skipsemi()
		if ncols>0 then
			checksymbollex(lbracksym)
!			lex()
		fi
		if ++nrows>maxrows then
			serror("tabledata:too many rows")
		fi

		if enums then
			checksymbol(namesym)
			stgen:=lx.symptr				!generic symbol entry
			tabledataname:=stgen.name		!allow to be picked up by $ lx.symbol
			lex()
			if lx.symbol=eqsym then
				if nrows<>1 then serror("enum=x, 1st row only") fi
				lex()
				nextenumvalue:=readconstint()
			fi
			enumvalues[nrows]:=nextenumvalue

			stenum:=getduplnameptr(owner, stgen, constid)
			stenum.mode:=tint
			stenum.code:=createconstunit(nextenumvalue, tint)
			stenum.scope:=scope
			adddef(owner, stenum)
			if scope=export_scope then
				addexpconst(stenum)
			fi

			if nrows=1 then firstval:=nextenumvalue fi
			lastval:=nextenumvalue

			++nextenumvalue
			if ncols then				!comma always expected
				checksymbollex(commasym)		!check it
!				lex()
			fi
		fi

		for i:=1 to ncols do
			addlistunit(plist[i], plistx[i], readunit())
			if i=ncols then
				checksymbollex(rbracksym)
			else
				checksymbollex(commasym)
			fi
!			lex()
		od

		if lx.symbol<>commasym then exit fi
		lex()					!should be ( for next entry
		if lx.symbol=closesym then exit fi		!allow trailing comma on last entry
	od

	intabledata:=0

	skipsemi()
	checkbeginend(closesym, ktabledatasym, startline)

!Here, I have:

!enum				1 means enum 0th column present, 0 means not
!ncols				0, or number of actual expression columns
!nrows				Number of data rows
!enumtypename			"", or enum user type name to be created for enum names/values

!varnameptrs[1..ncols]		!Names of the list variables ([]strec]
!varlisttypes[1..ncols]		!Type of each list (or 0 if not provided)
!varelemttypes[1..ncols]	!Type of each element (or 0 if not provided)
!plist[1..ncols]		Each entry is a list of expression units for the column

!enumnames[1..nrows]	When enum=1, list of names/values in 0th column
!enumvalues[1..nrows]

	if nrows=0 then serror("No table data") fi

!for each variable, add a vardef initialised to the list
!add the decls for the vars

	for i:=1 to ncols do

		stvar:=getduplnameptr(owner, varnameptrs[i], staticid)
		stvar.code:=createunit1(jmakelist, plist[i])
		stvar.code.length:=nrows
		stvar.istabdata:=1

		storemode(owner, varlisttypes[i], stvar.mode)
		stvar.scope:=scope

		adddef(owner, stvar)
		addstatic(stvar)
	od
end

global proc readclassdef(symbol owner, int scope)=
!at 'class' symbol
!read enough of the class to be able to generate export data
	int kwd, baseclass, m, startline, closesym, mrec, isrecord, align
	symbol nameptr, sttype

	kwd:=lx.symbol
	isrecord:=kwd=krecordsym

	lexchecksymbol(namesym)
	nameptr:=lx.symptr

	lex()
	baseclass:=0
	if lx.symbol=lbracksym then
		lex()
		baseclass:=readtypespec(owner)
		checksymbollex(rbracksym)
!		lex()
	fi

	checkequals()
	lex()

	align:=0
	if lx.symbol=atsym then
!		if lx.subcode=0 then
!SERROR("= @ N")
!!			lex()
!!			align:=readconstint()
!		else
			lex()
!		fi
		align:=1
	fi

	sttype:=getduplnameptr(owner, nameptr, typeid)
	adddef(owner, sttype)
	m:=createusertype(sttype)

	mrec:=createrecordmode(owner, m)
	storemode(owner, mrec, sttype.mode)

	storemode(owner, baseclass, sttype.baseclass)
	sttype.align:=align

	closesym:=checkbegin(1)

	startline:=getcurrline()

	readclassbody(sttype, kwd)

	checkbeginend(closesym, kwd, startline)

	sttype.scope:=scope
end

proc readclassbody(symbol owner, int classkwd)=
!at first symbol of a class or record body
!read fields, constants, types, methods.
	int kwd, t, lbcount:=0

	unionstr_clear(&unionstring)
	unionstr_clear(&unionpend)

	docase lx.symbol
	when kconstsym then
		readconstdef(owner, 0)
	when kfuncsym, kprocsym then
		kwd:=lx.symbol

		if owner.isimport then
			readprocdecl(owner, 0)
		else
			readprocdef(owner, 0)
		fi
	when krecordsym then
		readclassdef(owner, 0)

	when ktypesym then
		readtypedef(owner)
	when eofsym then
		serror("Class eof?")
		exit
	when semisym then
		lex()

	when ktabledatasym then
		readtabledef(owner, 0)

	when kmacrosym then
		readmacrodef(owner, 0)

	when kstructsym, kunionsym then
		unionstr_append(&unionpend, (lx.symbol=kstructsym|'S'|'U'))
		unionlastvar:=nil
		lex()
		if lx.symbol=lbracksym then ++lbcount; lex() fi
	when kendsym, rbracksym then
		if unionstring.ulength then
			if lx.symbol=rbracksym and lbcount then
				lex()
				--lbcount
			else
				checkend(kendsym, (unionstr_last(&unionstring)='S'|kstructsym|kunionsym))
			fi
			if unionlastvar=nil or unionpend.ulength then
				serror("Empty union group")
			fi
			case unionstr_last(&unionlastvar.uflags)
			when 'E', '*' then
			else
				unionstr_append(&unionlastvar.uflags, '*')
			esac
			unionstr_append(&unionlastvar.uflags, 'E')
			unionstring.ulength--
		else
			exit
		fi

	when kvarsym then

		lex()
		if istypestarter() then
	readmut:
			++insiderecord
			t:=readtypespec(owner)
			--insiderecord
		else
			t:=tauto
		fi
		readrecordfields(owner, t)

	when kletsym then
		serror("Let not allowed")

	else
		if istypestarter() then
			goto readmut
!		serror("record:need var")
		else
			exit
		fi
	end docase

	if lbcount then serror("LB?") fi

end

proc readimportmodule(symbol owner)=
!at 'importmodule' symbol
	int isnew, startline, closesym
	symbol stname, stname0

	if insidedllimport then serror("nested importdll") fi
!	libtype:=lx.subcode

	lex()
	if lx.symbol=stringconstsym then
		stname:=addnamestr(lx.svalue)
	else
		checksymbol(namesym)
		stname:=lx.symptr
	fi

	lex()
	checkequals()
	lex()

!stname points to a nullid symbol
!check whether this module already exists

	isnew:=1

	for i to nlibfiles do
		if eqstring(libfiles[i], stname.name) then
!			stname:=libtable[i]
			isnew:=0
			exit
		fi
	od

	if isnew then			!new
		addlib(stname.name)
	fi

	startline:=getcurrline()
	closesym:=checkbegin(0)

	insidedllimport:=1

	readimportbody(owner)

	insidedllimport:=0

	checkbeginend(closesym, kimportmodulesym, startline)

end

proc readimportbody(symbol owner)=
!positioned at first symbol of statement (which can be empty)
!return knode containing statement, or nil if not found (at 'end etc)
	int pos
	symbol d

	pos:=lx.pos

	do
		skipsemi()
		case lx.symbol
		when kprocsym, kfuncsym then
doproc:
			d:=readprocdecl(owner, 0)
			if ndllproctable>=maxdllproc then
				serror("Too many dll procs")
			fi
			dllproctable[++ndllproctable]:=d

		when ktypesym then
			readtypedef(owner, subprog_scope)

		when kconstsym then
			readconstdef(owner, subprog_scope)

		when krecordsym then
			readclassdef(owner, subprog_scope)

		when kvarsym then
			lex()
			readvardef(owner, subprog_scope, 0, dllvarid, kvarsym)

		when stdtypesym, namesym, krefsym, kicharsym, lsqsym, kslicesym then
			readvardef(owner, subprog_scope, 0, dllvarid, 0)

		when eofsym then
			exit

		when kendsym then
			exit
		else
			PS("symbol")
			serror("Not allowed in importmodule")
		esac
	od
end

func readequivfield(symbol owner)symbol=
!reading a class or struct body, where owner is that class/struct entry
!positioned at symbol following '@', should be name of an existing field
	symbol p, d

	checksymbol(namesym)
	d:=lx.symptr
	lex()

	p:=owner.deflist
	while p do
		if eqstring(p.name, d.name) then
			return p
		fi

		p:=p.nextdef
	od
	cpl d.name
	serror("Can't find @ field")
	return nil
end

func readrefproc(symbol owner, int typedefx)int=
!'ref' was seen, now positioned at 'proc' 'func' or 'method'
!read proc params and any result, return a complete ref proc spec
	int kwd, prettype, m, varparams, nparams
	[4]int retmodes
	symbol paramlist, stproc
	int rettype2, rettype3, nretvalues
	ichar name

	kwd:=lx.symbol				!remember whether proc or func
	
	lex()

	paramlist:=nil
	prettype:=tvoid
	nretvalues:=0
	varparams:=0

!need to create suitable holding typename in advance
	name:=nextautotype()
	stproc:=getduplnameptr(stmodule, addnamestr(name), typeid)
	adddef(stmodule, stproc)
	retmodes[1]:=tvoid

	if kwd=kfuncsym then
		if lx.symbol=lbracksym then		!possible params
			lex()
			if lx.symbol<>rbracksym then
				paramlist:=readparams(owner, stproc, varparams, nparams)
				checksymbol(rbracksym)
			fi
			lex()
			if lx.symbol=colonsym or lx.symbol=sendtosym then
				lex()
				nretvalues:=readreturntype(stproc, retmodes)
			elsif typestarterset[lx.symbol] or lx.symbol=namesym then
				nretvalues:=readreturntype(stproc, retmodes)
			fi
		elsif lx.symbol=colonsym or lx.symbol=sendtosym then
			lex()
			nretvalues:=readreturntype(stproc, retmodes)
		fi
		if nretvalues=0 then
			serror("Function needs return type")
		end

		if nretvalues and kwd=kprocsym then		!proc: result given
			serror("Proc can't return value")
		fi
	else					!proc with no result
		if lx.symbol=lbracksym then		!possible params
			lex()
			if lx.symbol<>rbracksym then
				paramlist:=readparams(owner, stproc, varparams, nparams)
				checksymbol(rbracksym)
			fi
			lex()
		fi
		if typestarterset[lx.symbol] or lx.symbol=colonsym or lx.symbol=sendtosym then
			serror("proc can't have ret value")
		fi
	fi

	m:=createrefprocmode(owner, stproc, paramlist, kwd, prettype, typedefx)

	storemode(owner, retmodes[1], stproc.mode)
	stproc.nretvalues:=nretvalues

	ttnamedef[m]:=stproc

	stproc.varparams:=varparams

	return m
end

proc pushproc(symbol p)=
	if nprocstack>=maxprocstack then
		serror("Too many nested proc")
	fi
	procstack[++nprocstack]:=currproc
	currproc:=p
end

proc popproc=
	if nprocstack then
		currproc:=procstack[nprocstack--]
	else
		currproc:=stmodule
	fi
end

func readreturntype(symbol owner, []int &retmodes)int=
!read 1..maxtuplesize return types as part of func decl
	int nretvalues

	retmodes[1]:=readtypespec(owner)
	nretvalues:=1
	while lx.symbol=commasym do
		if nretvalues>=maxtuplesize then
			serror("Too many return values")
		fi
		lex()
		retmodes[++nretvalues]:=readtypespec(owner)
	od

	return nretvalues
end

func readset:unit=
!positioned at "["
	int length, nkeyvalues, oldirp
	unit p, ulist, ulistx

	lex()					!first symbol of first expression

	case lx.symbol
	when rsqsym then		!empty set, same as 0
		lex()
		return createunit1(jmakeset, nil)
	esac

	length:=0
	nkeyvalues:=0

	ulist:=ulistx:=nil

	do
		oldirp:=inreadprint
		inreadprint:=0
		p:=readunit()
		inreadprint:=oldirp
		if p.tag=jkeyvalue then ++nkeyvalues fi
		++length

		addlistunit(ulist, ulistx, p)

		case lx.symbol
		when commasym then
			lex()
			if lx.symbol=rsqsym then exit fi
		when semisym then
			lexchecksymbol(rsqsym)
			exit
		when rsqsym then
			exit
		else
			serror("readset?")
		esac
		skipsemi()						!allow a, b, c;]
	od
	lex()

	if nkeyvalues then
		if length>nkeyvalues then serror("dict: mixed elements") fi
		p:=createunit1(jmakedict, ulist)
	else
		p:=createunit1(jmakeset, ulist)
	fi
	p.length:=length
	return p
end

func istypestarter:int=
	if typestarterset[lx.symbol] then return 1 fi
	if lx.symbol=namesym then				!name ...
		case nextlx.symbol
		when namesym then					!name name
			return 1
		when addrsym then
			return 1
		esac
	fi
	return 0
end

global func readunit:unit p=
	unit pt
	int pos

	pt:=nil
	pos:=lx.pos
	pt:=readterm2()

	if jisexpr[pt.tag]=0 then
		return pt
	fi

	if endsexpr[lx.symbol] then
		return pt
	fi

	if lx.symbol=assignsym then
		lex()
		p:=readterm2()
		if endsexpr[lx.symbol] then
			p:=createunit2(jassign, pt, p)
			p.pos:=pos
			return p
		fi
		p:=createunit2(jassign, pt, readassignment(p))
	else
		p:=readassignment(pt)
		p.pos:=pos
	fi

	while lx.symbol=pipesym do
		lex()
		p:=createunit2(jcall, readassignment(), p)
	od

	return p
end

func readassignment(unit pt=nil)unit p=
	int pos, opc
	unit q

	p:=readorterms(pt)

	if (opc:=lx.symbol) = assignsym then
		pos:=lx.pos
		lex()
		q:=readassignment(nil)
		p:=createunit2(jassign, p, q)
		p.pos:=pos
	fi
	return p
end

func readorterms(unit pt=nil)unit p=
	int pos

	p:=readandterms(pt)

	while lx.symbol=orlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			SERROR("OR:=")
		fi

		p:=createunit2(jorl, p, readandterms())
		p.pos:=pos
	od

	return p
end

func readandterms(unit pt=nil)unit p=
	int pos

	p:=readcmpterms(pt)

	while lx.symbol=andlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			SERROR("AND:=")
		fi

		p:=createunit2(jandl, p, readcmpterms())
		p.pos:=pos
	od

	return p
end

func readcmpterms(unit pt=nil)unit p=
	int pos, opc, n
	unit ulist, ulistx, q
	[4]byte genops

	p:=readinterms(pt)

	if lx.symbol not in [eqsym, cmpsym] then
		return p
	fi

	ulist:=ulistx:=p
	p:=createunit1(jcmpchain, p)
	n:=0				!n counts pclopnd after the first
	clear genops

	docase lx.symbol
	when eqsym, cmpsym then
		++n
		if n>genops.len then serror("cmpchain: Too many items") fi
		genops[n]:=lx.subcode

		pos:=lx.pos
		lex()

		q:=readinterms()
		addlistunit(ulist, ulistx, q)
		q.pos:=pos
	else
		exit
	end docase

	if n=1 then
		p.tag:=jcmp
		q:=p.a
		p.pclcond:=genops[1]
		p.b:=q.nextunit
		q.nextunit:=nil
	else
		p.cmpgenop:=genops
	fi

	return p
end

func readinterms(unit pt=nil)unit p=
	int pos, opc
	p:=readrangeterm(pt)

	docase lx.symbol
	when insym, notinsym then
		opc:=lx.subcode

		pos:=lx.pos
		lex()

		p:=createunit2(jin, p, readrangeterm())
		p.inv:=opc
		p.pos:=pos
	else
		exit
	end docase

	return p
end

func readrangeterm(unit pt=nil)unit p=
	int pos, opc
	p:=readaddterms(pt)

	if lx.symbol=rangesym then
		pos:=lx.pos
		lex()
		p:=createunit2(jmakerange, p, readaddterms())
		p.pos:=pos
	fi

	return p
end

func readaddterms(unit pt=nil)unit p=
	int pos, sym, tag, genop

	p:=readmulterms(pt)

	docase sym:=lx.symbol
	when addsym, subsym, iandsym, iorsym, ixorsym, minsym, maxsym then
		pos:=lx.pos
		genop:=lx.subcode
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto, p, readassignment())
			p.pclop:=symbolgentoops[sym]
			p.pos:=pos
			exit
		fi

		p:=createunit2(jbin, p, readmulterms())
		p.pclop:=symbolgenops[sym]
		p.pos:=pos
	else
		exit
	end docase

	return p
end

func readmulterms(unit pt=nil)unit p=
	int pos, sym

	p:=readpowerterms(pt)

	docase sym:=lx.symbol
	when mulsym, divsym, idivsym, iremsym, shlsym, shrsym, idivremsym then
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto, p, readassignment())
			p.pclop:=symbolgentoops[sym]
			p.pos:=pos
			exit
		fi

		p:=createunit2(jbin, p, readpowerterms())
		p.pclop:=symbolgenops[sym]
		p.pos:=pos
	else
		exit
	end docase

	return p
end

func readpowerterms(unit p=nil)unit=
	int pos

	if p=nil then
		p:=readterm2()
	fi

	while lx.symbol=powersym do
		pos:=lx.pos
		lex()
		p:=createunit2(jbin, p, readpowerterms())
		p.pclop:=kpower
		p.pos:=pos
	od

	return p
end

func readterm2:unit=
	unit p, q, r
	ref char pbyte
	u64 a
	int oldipl, opc, oldinrp, pos, shift, t

	pos:=lx.pos

	p:=readterm()

	docase lx.symbol
	when lbracksym then
		lex()
		oldinrp:=inreadprint
		inreadprint:=0
		q:=readslist(1, 1)
		checksymbollex(rbracksym)
!		lex()
		if p.tag=jsyscall then
			p.a:=q
		else
			p:=createunit2(jcall, p, q)
		fi
		inreadprint:=oldinrp
		p:=readcondsuffix(p)

	when ptrsym then
		p:=createunit1(jptr, p)
		lex()

	when lsqsym then
		p:=readindex(p, 0)

	when dotsym then
		p:=readdotsuffix(p)

	when colonsym then
		if inreadprint then exit fi
		lex()
		q:=readunit()
		p:=createunit2((inparamlist|jkeyword|jkeyvalue), p, q)

	when incrsym then
		case lx.subcode
		when kincrto then opc:=kloadincr
		when kdecrto then opc:=kloaddecr
		esac
		lex()
		p:=createunit1(jincr, p)
		p.pclop:=opc

	when lcurlysym then
		serror("X{...} not ready")
	else
		exit
	end docase

	p.pos:=pos

	return p
end

func readterm:unit=
	unit p, q, r
	u64 a
	int opc, pos, length
	byte strtype
	ichar s
	[32]u64 cstr

	pos:=lx.pos

	switch lx.symbol
	when namesym then
		if nextlx.symbol=atsym then		!type-punning with user type
			p:=readcast()
		else
			p:=createname(lx.symptr)
			p.pos:=lx.pos
			lex()
		fi

	when intconstsym, realconstsym then
		p:=createconstunit(lx.value, lx.subcode)
!		p.istrueconst:=1
		lex()

	when stringconstsym then
		p:=createstringconstunit(lx.svalue, lx.slength)
		p.strtype:=lx.subcode			!0/1/2 = str/bindata/strdata
		lex()

	when charconstsym then
		length:=lx.slength-1
		if length>8 then serror("Char const too long") fi
		a:=0
		if length then
			memcpy(&a, lx.svalue, length)
		fi
		p:=createconstunit(a, tc64)
		lex()

	when lbracksym then
		p:=readlbrack()

	when stdtypesym, krefsym, kicharsym then
!CPL "RT CAST"
		p:=readcast()

	when addsym, subsym, minsym, maxsym, abssym, inotsym, 
iandsym, iorsym, ixorsym, 
		mathsopsym, sqrtsym, sqrsym, maths2opsym, signsym then
		p:=readopc()

	when notlsym then
		if nextlx.symbol=assignsym then
			p:=readopc()
		else
			lex()
			p:=createunit1(jnotl, readterm2())
			p.pclop:=knot
		fi

	when istruelsym then
		if nextlx.symbol=assignsym then
			p:=readopc()
		else
			lex()
			p:=createunit1(jistruel, readterm2())
		fi

	when lsqsym then
		p:=readset()

	when incrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(jincr, readterm2())
		p.pclop:=opc

	when addrsym, daddrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(opc, readterm2())
		if p.a.tag=jcall then
			if p.a.b then
				serror("Params not allowed")
			fi
			p.a:=p.a.a			!lose the call
		fi

	when anddotsym then
		lex()
		p:=createunit1(jaddroffirst, readterm2())

	when compilervarsym then
		p:=readcompilervar()

	when dollarsym then
		if intabledata then
			if lx.subcode=1 then			!need char type
				cstr[1]:=0
				strcpy(cast(&cstr), tabledataname)
				p:=createconstunit(cstr[1], tu64)
			else
				s:=tabledataname
				if nextlx.symbol=addsym then
					lex()
					lex()
					checksymbol(intconstsym)
					s+:=lx.value
				fi
				p:=createstringconstunit(s, -1)
			fi
		else
			if ndollar<=0 then
				serror("[$] No array")
			fi
			p:=createunit1(junary, dollarstack[ndollar])
			p.propcode:=kkupb
		fi
		lex()

	when kcastsym then
		p:=readcastx()

	when kclampsym then
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbollex(commasym)
!		lex()
		q:=readunit()
		if lx.symbol=rbracksym and q.tag=jmakerange then
			r:=q.b
			q:=q.a
		else
			checksymbollex(commasym)
!			lex()
			r:=readunit()
			checksymbol(rbracksym)
		fi
		lex()

		q:=createunit2(jbin, p, q)
		q.pclop:=kmax
		p:=createunit2(jbin, q, r)
		p.pclop:=kmin

	when kgotosym then
		p:=readgoto()

	when kifsym then
		p:=readif()

	when kunlesssym then
		p:=readunless()

	when kcasesym, kdocasesym, kswitchsym, kdoswitchsym then
		p:=readswitchcase()

	when krecasesym then
		p:=readrecase()

	when kforsym then
		p:=readfor()

	when ktosym then
		p:=readto()

	when kdosym then
		p:=readdo()

	when kwhilesym then
		p:=readwhile()

	when krepeatsym then
		p:=readrepeat()

	when kloopsym then
		p:=readloopcontrol()

	when kreturnsym then
		p:=readreturn()

	when kstopsym then
		p:=readstop()

	when kprintsym then
		p:=readprint()

	when kreadsym then
		p:=readread()

	when kswapsym then			!swap using func syntax
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbollex(commasym)
!		lex()
		q:=readunit()
		checksymbollex(rbracksym)
!		lex()
		p:=createunit2(jswap, p, q)

	when kevalsym then
		lex()
		p:=createunit1(jeval, readunit())

	when ksyscallsym then
		p:=createunit0(jsyscall)
		p.fnindex:=lx.subcode
		lex()

	when kstrincludesym then
		strtype:=lx.subcode
		lex()
		p:=createunit1(jstrinclude, readterm2())
		p.strtype:=strtype

	when kclearsym then
		lex()
		p:=createunit1(jclear, readterm2())

	when lcurlysym then
		serror("{...} not ready")

	when kslicesym then
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbollex(commasym)
		q:=readunit()
		checksymbollex(rbracksym)
		p:=createunit2(jmakeslice, p, q)

	else
DOELSE:
		cpl symbolnames[lx.symbol], =LX.SYMBOL, ISTYPESTARTER()
		serror("readterm?")
	end switch

	p.pos:=pos
	return p
end

proc readmacrodef(symbol owner, int scope)=
!positioned at 'macro'
!read expression macro-definition; global=1 if to be exported
!int kwd, varparams, try_level, prettype, nparams, rettype2, rettype3, nretvalues
!ichar metadata, truename
!symbol pequiv, stproc, owner, paramlist, nameptr

	symbol nameptr, stmacro, paramlist, paramlistx, stname

	lexchecksymbol(namesym)

	nameptr:=lx.symptr
	stmacro:=getduplnameptr(owner, nameptr, macroid)
	adddef(owner, stmacro)

	owner:=stmacro

	lex()

	paramlist:=paramlistx:=nil

	if lx.symbol=lbracksym then			!may have parameters
		lex()
		if lx.symbol<>rbracksym then
			do
				case lx.symbol
				when namesym then
					stname:=getduplnameptr(owner, lx.symptr, macroparamid)
					adddef(owner, stname)
					addlistparam(&paramlist, &paramlistx, stname)

					lex()
					if lx.symbol=rbracksym then
						exit
					fi
					checksymbollex(commasym)
!					lex()
				else
					serror("macro def params")
				esac
			od
		fi
		lex()						!skip )
	fi
	stmacro.paramlist:=paramlist
	stmacro.scope:=scope

	checkequals()
	lex()
	stmacro.code:=readunit()
end

func readrecase:unit=
	lex()
	if lx.symbol=kelsesym then
		lex()
		return createunit0(jrecase)
	else
		return createunit1(jrecase, readunit())
	fi
end

func fixcond(unit p)unit=
	checknotempty(p)
	if not isbooltag[p.tag] then
		insertunit(p, jistruel)
!		p.convcode:=kktoboolt
	fi
	return p
end

func readsunit(int inwhile=0)unit=
	int pos, m, sym, opc
	unit ulist, ulistx, p, q, r
	symbol stname

	pos:=lx.pos
	ulist:=ulistx:=nil

	repeat
		while lx.symbol=semisym do
			lex()
		od
		switch lx.symbol
		when kstaticsym then
			lex()
			if lx.symbol in [kletsym, kvarsym] then
				opc:=lx.symbol
				lex()
			else
!			opc:=kmutsym
				opc:=0
			fi
			readvardef(currproc, 0, 1, staticid, opc)

		when kprocsym, kfuncsym then
			readprocdef(currproc, 0)

		when stdtypesym, krefsym, kicharsym, kslicesym, lsqsym then
			if nextlx.symbol in [lbracksym, atsym, dotsym] then		!is a cast etc
				goto doexec
			else
				sym:=0
				goto dovar
			fi

		when kvarsym, kletsym then
			sym:=lx.symbol
			lex()
	dovar:
			q:=readvardef(currproc, 0, 0, frameid, sym)
			while q do								!initialised decls involve code
				r:=q.nextunit						!unlink from this block first
				q.nextunit:=nil
				addlistunit(ulist, ulistx, q)		!add one by-one
				q:=r
			od

		when ktypesym then
			readtypedef(currproc, 0)

		when kconstsym then
			readconstdef(currproc, 0)

		when krecordsym then
			readclassdef(currproc, 0)

		when kmacrosym then
			readmacrodef(currproc, 0)

		when ktabledatasym then
			readtabledef(currproc, 0)

		when eofsym then
			cpl currproc.name
			serror("Unexpected EOF in proc")

!these are needed to check for an empty sunit preceding
		when rbracksym, kthensym, kelsifsym, kelsesym, kuntilsym, kwhensym, 
				kelsecasesym, kelseswitchsym, kendsym then
			exit
!
		when namesym then
			case nextlx.symbol
			when colonsym then
				p:=createunit0(jlabeldef)
				stname:=getduplnameptr(currproc, lx.symptr, labelid)
				adddef(currproc, stname)
				p.def:=stname
				lex()
				lx.symbol:=semisym
				addlistunit(ulist, ulistx, p)
			when namesym then
				sym:=kvarsym
				goto dovar

			goto doexec

			else
				goto doexec
			esac
		when kdosym then				!u;u;u;do rather than u;u;u do
			if inwhile then
				exit
			fi
			goto doexec

		when semisym then

!		when questionsym then
!			lx.symbol:=semisym
		when questionsym then
			p:=createunit1(jsourceline, createstringconstunit(lx.svalue, -1))
			LX.SYMBOL:=SEMISYM
			doexec3


		else							!assume a statement
	doexec:
			p:=readunit()
	doexec2:
			if p.tag=jname and lx.symbol=namesym then
				serror("Possibly var/let needed")
			fi
	doexec3:
			addlistunit(ulist, ulistx, p)
			if lx.symbol=kdosym then
				exit
			fi

		end switch
	until lx.symbol<>semisym

	case lx.symbol
	when rbracksym, kthensym, kelsifsym, kelsesym, kuntilsym, kwhensym, kdosym, 
		kelsecasesym, kelseswitchsym, kendsym, commasym, 
		barsym then
	else
		serror("Readsunit: "";"" expected, or bad unit starter")
	esac

	if ulist=nil or ulist.nextunit then
		return createunit1(jblock, ulist)
	else
		return ulist
	fi
end

func readbxdata:unit p =
!EXPERIMENTAL CODE TO SPEED UP BYTE ARRAY INITS
!This assumes a sequence of intconsts, but needs to backtrack
!and general a normal makelist if any non-intconsts are seen
!This backtracking is not present.

!at '(', and initialising a byte-array
!this test assumes all values are intconst ones, 
!and creates a data-string object
!	int curralloc:=16, n:=0
	int curralloc:=4, n:=0
	ref byte q, r, qnew
!CPL "READBXDATA"

	p:=nil
	q:=r:=pcm_alloc(curralloc)

	do
		lex()
		skipsemi()
		if lx.symbol<>intconstsym then
			exit
		fi

		if n=curralloc then
			curralloc*:=2
			qnew:=pcm_alloc(curralloc)
			memcpy(qnew, q, n)
			r:=qnew+(r-q)
			pcm_free(q, n)
			q:=qnew
		fi

		r++^:=lx.value
		++n

		lex()
		if lx.symbol<>commasym then
			exit
		fi
	od
	checksymbol(rbracksym)
	lex()

	p:=createstringconstunit(q, n)
	p.strtype:=1

	p
end

proc checknotempty(unit p)=
	if p=nil or p.tag=jblock and p.a=nil then
		serror("Empty sunit")
	fi
end
=== mm_support.m 0 0 26/34 ===
global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

global func newsourcefile:ifile pf=
	pf:=pcm_allocz(filerec.bytes)
	if nsourcefiles>=maxsourcefile then loaderror("Too many sources") fi
	sources[++nsourcefiles]:=pf
	pf.fileno:=nsourcefiles
	pf
end

global proc mcerror(ichar mess)=
	println "MC Error:",mess

	stop 1
end

global proc serror_gen(ichar mess)=
	showdivider('*')
	println "Syntax Error:",MESS

	showerrorsource(lx.pos, currproc)

	println mess

	stopcompiler(sources[lx.fileno].filespec,getlineno(lx.pos))
end

proc showdivider(c64 ch)=
	to 87 do
		print ch
	od
	println
end

proc showerrorsource(int pos, symbol stproc=nil)=
	int fileno:=getfileno(pos), lineoffset
	ichar errorline,s

	fprintln "    Line:     #",getlineno(pos)
	if stproc and stproc.nameid=procid then
		fprintln "    Function: #()", stproc.name
	fi
	fprintln "    Module:   # (#)", sources[fileno].name,sources[fileno].filespec
	showdivider('-')

	s:=errorline:=getsourceline(pos)
	lineoffset:=getsourcepos(pos)-errorline

	to 6 do print " " od
	while s^ not in [10,0] do
		print s++^
	od
	println
	s:=errorline
	to 6 do print " " od
	to lineoffset do
		if s^=9 then print '\t' else print ' ' fi
		++s
	od
	println "^"
	showdivider('-')
end

global proc stopcompiler(ichar filename,int lineno)=
	filehandle f
	f:=fopen("$error.tmp","w")
	println @f,filename,lineno
	fclose(f)
CPL "PRESS key"; OS_GETCH()
	println
	println
	stop 1
end

global proc serror(ichar mess)=

	serror_gen(mess)
end

global proc serror_s(ichar mess,a)=
	[256]char str
	fprint @&.str,mess,a
	serror_gen(&.str)
end

global proc error_gen(int pass,ichar mess,unit p=nil)=
!general error handling for passes name, type and code gen
!pass='N' 'T' or 'G'
	int pos

	if p then
CPL "P.POS"
		pos:=p.pos
	else
CPL "MMPOS"
		pos:=mmpos
	fi

CPL =GETFILENO(POS)

	showdivider('*')
	case pass
	when 'N' then println "RX Name Error: "
	when 'T' then println "TX Type Error: "
	when 'G' then println "GX Code Gen Error: "
	when 'A' then println "AX Code Gen Error: "
	esac

	showerrorsource(pos, currproc)

	println mess

	stopcompiler(sources[getfileno(pos)].filespec,getlineno(pos))
end

global proc rxerror(ichar mess,unit p=nil)=
	error_gen('N',mess,p)
end

global proc gerror(ichar mess,unit p=nil)=
	error_gen('G',mess,p)
end

global proc txerror(ichar mess,unit p=nil)=
	error_gen('T',mess,p)
end

global proc txerror_s(ichar mess,a,unit p=nil)=
	[256]char str
	fprint @&.str,mess,a
	error_gen('T',&.str,p)
end

global proc txerror_ss(ichar mess,a,b)=
	[256]char str
	fprint @&.str,mess,a,b
	error_gen('T',&.str)
end

global proc rxerror_s(ichar mess,a,unit p=nil)=
	[256]char str
	fprint @&.str,mess,a
	error_gen('N',&.str,p)
end

global proc gerror_s(ichar mess,s,ref unitrec p=nil)=
	[256]char str

	fprint @&.str,mess,s
	error_gen('G',&.str,p)
end

global proc lxerror_gen(ichar mess)=

	println "On line",getlineno(lx.pos),"in file",sources[lx.fileno].filespec

	println
	println "**** Lex Error:",mess,"****"
	println

	stopcompiler(sources[lx.fileno].filespec,getlineno(lx.pos))
end

global proc lxerror(ichar mess)=
	lxerror_gen(mess)
end

global proc loaderror(ichar mess,mess2="",mess3="")=
	println "Load Error:",mess,mess2,mess3
	println "Stopping"
	stop 1
end

global proc gs_additem(ref strbuffer dest,ichar s)=
!like genstr, but ensure there is white space separation as needed from the last output
	ichar d
	int lastchar,nextchar

	d:=dest^.strptr

	if dest^.length then
		lastchar:=(d+dest^.length-1)^
		nextchar:=s^
		if isalphanum(lastchar) and isalphanum(nextchar) then
			strbuffer_add(dest," ")
		fi
	fi
	strbuffer_add(dest,s)
end

global proc gs_copytostr(ref strbuffer source,ref char s)=
	if source^.length then
		memcpy(s,source^.strptr,source^.length)
		(s+source^.length)^:=0
	else
		s^:=0
	fi
end

global func isalphanum(int c)int=
	if c>='A' and c<='Z' or c>='a' and c<='z' or c>='0' and c<='9' then
		return 1
	fi
	return 0
end

global proc init_tt_tables=
	int i,size,bitsize
	int s,t,u,v

!Initialise type tt-tables from std types first all fields initially zero

	for i:=0 to tlast-1 do

		ttname[i]:=stdnames[i]
		ttbasetype[i]:=i
		bitsize:=stdsize[i]*8

		switch bitsize
		when 0 then
			size:=0
		when 1,2,4 then
			size:=1
		else
			size:=bitsize/8
		end switch

		ttsize[i]:=size

		case i
		when ti8,ti16,ti32,ti64 then
			ttsigned[i]:=1
			ttisinteger[i]:=1
		when tu8, tu16, tu32, tu64, tc8, tc64 then
			ttisinteger[i]:=1
		when tr32, tr64 then
			ttisreal[i]:=1
		when tref, trefchar, trefbit then
			ttisref[i]:=1
		esac

!		if stdcat[i]=intcat and size<8 then
		if ttisinteger[i] and size<8 then
			ttisshort[i]:=1
		fi

		ttlower[i]:=1

		if i in [trecord, trange, tarray, tslice] then
			ttisblock[i]:=1
		fi

	od

	ttbasetype[trefchar]:=tref
	tttarget[trefchar]:=tc8

	ntypes:=tlast-1

	trefproc:=createrefmode(nil,tproc,0)
	treflabel:=createrefmode(nil,tlabel,0)
end

global func getsupportfile(ichar filename, ext="", path="")ifile =
!filename is a rel/abs/base filespec (rel path, abs path or none)
!syslib=1: look first inside INT list
!syslib=0 or not found in int (or -EXT mode0):
!	fbundled; load from list of bundled files from .ma fle
!	filename has rel path/no path: appl to given path
!	filename has abs path: use as is
!look up filename at that final location only (not multiple places)
!issupport=0 for a support file; helps with bundled files where there may
!be duplicates

	[300]char filespec,filespec2
	ichar file
	int fileno
	ifile pfile

!CPL "GETSUPP1", FILENAME, =PATH

	file:=filename

	if fverbose=3 then
		fprintln "Get file:# (ext:#) (path:#)",filename,ext, path
	fi

	if ext^ then
		strcpy(filespec,addext(filename,ext))
		file:=&.filespec
	fi

	if loadedfromma then
		file:=pcm_copyheapstring(extractfile(file))
	fi	

	for i to nsourcefiles do
		if eqstring(file, sources[i].filename) and not sources[i].issyslib then
			return sources[i]
		fi
	od

	if not isabspath(file) then
		strcpy(filespec2,path)
		strcat(filespec2,file)
		file:=&.filespec2
	fi

	if fverbose=3 and fileno then
		println "Checkfile:",file
	fi

!CPL =FILE
!CPL =FILENAME

	if file=nil or not checkfile(file) then
		loaderror("Can't find file: ",file)
	fi

	pfile:=loadsourcefile(file)
	if fverbose=3 and pfile then
		println "Found:",file
	fi

	pfile.issupport:=1
	return pfile
end

func isabspath(ichar filespec)int=
	ichar path:=extractpath(filespec)
	if path^ in ['\\','/'] or path^<>0 and (path+1)^=':' then	!absolute path
		return 1
	fi
	return 0
end

global proc initbblib=
	for i:=1 to D_typestarterset.len do typestarterset[D_typestarterset[i]]:=1 od
end

global func getfileno(word pos)int fileno=
	fileno:=pos.[24..31]
!
!CPL =FILENO
!CPL =POS.[0..23]

	if fileno<1 or fileno>nsourcefiles then
!		RETURN 1
		abortprogram("No file no")
	fi
	return fileno
end

global func getlineno(word pos)int=
	ichar source := getsourcestart(pos)
	ichar sline:=getsourceline(pos)
	ichar s:=sline
	int lineno:=1

	while s>=source do
		if s^=10 then ++lineno fi
		--s
	od

	return lineno
end

func getsourceline(word pos)ichar=
	ichar source := getsourcestart(pos)
	ichar s :=  getsourcepos(pos)

	while s>source and s^<>10 do --s od
	if s^=10 then ++s fi

	return s
end

func getsourcestart(word pos)ichar=
	return sources[getfileno(pos)].text
end

func getsourcepos(word pos)ichar=
	return sources[getfileno(pos)].text+pos.[0..23]
end

global func mgetsourceinfo(int pos, ichar &filename, &sourceline)int=
	int lineno

	lineno:=getlineno(pos)
	sourceline:=getsourcestart(pos)
	filename:=sources[getfileno(pos)].filespec

	lineno
end


global proc do_writema(ichar inpfile)=
	[300]char filename
	[maxsourcefile]int sflist
	filehandle f
	int offset, nfiles, fileno
	ifile pf

	return unless passlevel=ma_pass

	strcpy(filename, changeext(inpfile, "ma"))

!first build a table of source files to be o/p
	nfiles:=0

	for i to nsourcefiles when not sources[i].issyslib do
		sflist[++nfiles]:=i
	od

	if nfiles=0 then loaderror("MA: no files") fi

	f:=fopen(filename,"wb")
	if not f then loaderror("Can't create MA file ",filename) fi

	if fverbose then
		println "Writing ",filename
	fi
	fprintln @f,"=== MA # ===",nfiles

	for i to nfiles do
		pf:=sources[sflist[i]]

		fprintln @f,"=== # # # #/# ===",
			pf.filename,
			pf.issyslib,
			pf.issupport,
			i,nfiles

		offset:=getfilepos(f)
		writerandom(f,cast(pf.dupl),offset,pf.size)
	od

	println @f,"=== END ==="

	for i to nfiles do
		pf:=sources[sflist[i]]
		println @f,i,pf.filename, pf. issyslib, pf.issupport
	od

	fclose(f)
	stop
end

global proc do_getinfo(ichar filename)=
	filehandle f
	ichar fs
	imodule pm

	if passlevel=getst_pass then
		f:=fopen(fs:=changeext(filename,"list"),"wb")
		if f then
			println "Writing",fs
			getst(f,stprogram)
			fclose(f)
		fi
	fi

	if passlevel=getproj_pass then
		f:=fopen(fs:=changeext(filename,"proj"),"wb")
		if f then
			println "Writing",fs
			for i to nmodules do
				pm:=modules[i]
				println @f,pm.name:"16jl", subprogs[pm.subprogno].name:"16jl",
					pm.file.filespec:"q",
					pm.issyslib
			od

			fclose(f)
		fi
	fi
end

proc getst(filehandle f, symbol d)=
	symbol q

	getstrec(f,d)

	q:=d.deflist

	while q, q:=q.nextdef do
		getst(f,q)
	od
end

proc getstrec(filehandle f, symbol d)=
	ichar name

	case d.nameid
	when procid, dllprocid, typeid, constid, staticid,
		 macroid, dllvarid then
	else
		return
	esac

	if d.owner and d.owner.nameid<>moduleid then
		return									!only module-level names
	fi

	print @f, subprogs[moduletosub[d.moduleno]].name:"10jl",$

	print @f,d.owner.name:"12jl",$
	print @f,d.name:"18jl",$

	case d.nameid
	when procid then
		name:=(d.mode|"funcid"|"procid")
	when dllprocid then
		name:=(d.mode|"dllfuncid"|"dllprocid")
	else
		name:=namenames[d.nameid]
	esac

	print @f,name:"10jl"

	print @f,getlineno(d.pos):"5",$

	case d.scope
	when module_scope then name:="Module"
	when subprog_scope then name:="Subprog"
	when program_scope then name:="Program"
	else name:="Export"				!assume export scope
	esac

	print @f, name,$

	if d.isimport then
		print @f,"Import "
	fi

	print @f,strmode(d.mode):"10jlq",$
	print @f,sources[modules[d.moduleno].fileno].filespec:"q"
	println @f

end
=== mm_tables.m 0 0 27/34 ===
!include "mm_types.m"

global enumdata  [0:]ichar stdnames,
        [0:]byte stdsize,
        [0:]byte stdpcl =

!    type         name       bits     pcl
    (tvoid=0,     "void",       0,    tpvoid),

    (tr64,        "r64",        8,    tpr64),
    (tr32,        "r32",        4,    tpr32),
    (ti64,        "i64",        8,    tpi64),
    (tu64,        "u64",        8,    tpu64),
    (tc64,        "c64",        8,    tpu64),

    (tbool64,     "bool64",     8,    tu64),

    (tref,        "ref",        8,    tpu64),
    (trecord,     "rec",        0,    tpblock),
    (trange,      "range",     16,    tpblock),

    (tarray,      "array",       0,   tpblock),
    (tslice,      "slice",      16,   tpblock),

    (tc8,         "c8",          1,   tpu8),
    (tbool8,      "b8",          1,   tpu8),
    (ti8,         "i8",          1,   tpi8),
    (ti16,        "i16",         2,   tpi16),
    (ti32,        "i32",         4,   tpi32),
    (tu8,         "u8",          1,   tpu8),
    (tu16,        "u16",         2,   tpu16),
    (tu32,        "u32",         4,   tpu32),

    (trefchar,    "ichar",       8,   tpu64),
    (trefbit,     "refbit",     16,   tpu64),

    (tauto,       "auto",        0,   tpu64),
    (tany,        "any",         0,   tpu64),
    (tproc,       "proc",        0,   tpu64),
    (tlabel,      "label",       0,   tpu64),
    (ttype,       "type",        8,   tpu64),
    (tbitfield,   "bitfl",       8,   tpu64),
    (ttuple,      "tuple",       0,   tpu64),
    (tpending,    "pend",        0,   tpu64),
    (tblock,      "block",       8,   tpblock),

    (tlast,       "last ",       0,   tpvoid),
end

global const tuser	= tlast

global const tint	= ti64
global const tword	= tu64
global const treal	= tr64
global const tbool	= tbool64

global const tfirstnum	= tr64
global const tlastnum	= tc64

global const tfirstshort	= tc8
global const tlastshort		= tu32

global const maxtuplesize = 4

global int trefproc
global int treflabel



global enumdata []ichar sysfnnames, []byte sysfnparams, []byte sysfnres =
	(sf_init,				$,	0,	0),
	(sf_print_startfile,	$,	0,	0),
	(sf_print_startstr,		$,	0,	0),
	(sf_print_startptr,		$,	0,	0),
	(sf_print_startcon,		$,	0,	0),
	(sf_print_setfmt,		$,	0,	0),
	(sf_print_nogap,		$,	0,	0),
	(sf_print_space,		$,	0,	0),
	(sf_print_i64,			$,	0,	0),
	(sf_print_i64_nf,		$,	0,	0),
	(sf_print_u64,			$,	0,	0),
	(sf_print_r64,			$,	0,	0),
	(sf_print_r32,			$,	0,	0),
	(sf_print_str,			$,	0,	0),
	(sf_print_str_nf,		$,	0,	0),
	(sf_print_strsl,		$,	0,	0),
	(sf_print_ptr,			$,	0,	0),
	(sf_print_ptr_nf,		$,	0,	0),
	(sf_print_c8,			$,	0,	0),
	(sf_print_bool,			$,	0,	0),
!	(sf_print_var,			$,	0,	0),
	(sf_print_newline,		$,	0,	0),
	(sf_print_end,			$,	0,	0),
	(sf_read_i64,			$,	0,	0),
	(sf_read_r64,			$,	0,	0),
	(sf_read_str,			$,	0,	0),
	(sf_read_fileline,		$,	0,	0),
	(sf_read_strline,		$,	0,	0),
	(sf_read_conline,		$,	0,	0),

	(sf_getnprocs,			$,	0,	1),		!access funcs
	(sf_getprocname,		$,	0,	1),
	(sf_getprocaddr,		$,	0,	1),

	(sf_power_i64,			$,	0,	1),
	(sf_unimpl,				$,	0,	1),

end
!
global [sysfnnames.len]symbol sysfnhandlers

!global [sysfnnames.len]int sysfnproclabels

!global int mmpos
!global byte fshowpst


!!---
global enumdata [0:]ichar jtagnames,
				   [0:]byte jsubs, [0:]byte jisexpr, [0:]byte jsolo =
!Basic units; these don't follow normal rules of params needing to be units or lists
!jisexpr=1/2 when unit returns a value; 1 means unary, 2 binary op,
! 3 means returns a value, but is not a unary or binary op
!jsolo = 1 means unit is allowed standalone without its value being used

!a,b,c are unitrec refs, which can be a single unit, or a linked-list chain
!(usually in forward order)
!	L means .a/b/c pointing to a unitlist; L can be nil for an empty list
!	u means .a/b/c pointing to a single unit
!	u/nil means can be nil

![a=u] means a is a unit/list, or is nil

	(jnone=0,		$,	0,		0,	0), ! For tagname lookups when tag is zero
	(jconst,		$,	0,		3,	0), ! value/etc=value, typeno=type code
	(jnull,			$,	0,		3,	0), ! Place holder unit: means 'param no present' when used where a param is expected
	(jvoidvar,		$,	0,		3,	0), ! create void variant
	(jname,			$,	0,		3,	0), ! def=nameptr
!	(jname,			$,	0,		3,	1), ! def=nameptr
	(jnamelv,		$,	0,		3,	0), ! def=nameptr
	(jblock,		$,	1,		0,	1), ! a=L
	(jdecimal,		$,	0,		3,	0), ! svalue=str, slength
	(jstrinclude,	$,	1,		3,	0), !
	(jsourceline,	$,	1,		3,	0), !

!Logical Operators

	(jandl,			$,	2,		2,	0), ! A B	This group are for conditional expressions (no result)
	(jorl,			$,	2,		2,	0), ! A B

	(jnotl,			$,	1,		1,	0), ! a
	(jistruel,		$,	1,		1,	0), ! a
	(jisfalsel,		$,	1,		1,	0), ! a

!Expressions and Operators

	(jmakelist,		$,	2,		3,	0), ! a=L, b=[u], length=N; element list/lower bound expr
	(jmakerange,	$,	2,		3,	0), ! A B
	(jmakeset,		$,	1,		3,	0), ! a=L, length=N
	(jmakedict,		$,	1,		3,	0), !
	(jmakeslice,	$,	2,		3,	0), !
	(jreturnmult,	$,	0,		3,	0), !

	(jkeyword,		$,	1,		3,	0), ! def=st entry
	(jkeyvalue,		$,	2,		3,	0), ! A B
	(jassign,		$,	2,		3,	1), ! A B a := x
	(jassignmm,		$,	2,		3,	1), ! A B (a,b,c) := (x,y,z)
	(jassignms,		$,	2,		3,	1), ! A B (a,b,c) := x
	(jassignmdrem,	$,	2,		3,	1), ! A B (a,b) := x divrem y
!	(jcallfn,		$,	2,		3,	1), ! A B
	(jcall,			$,	2,		3,	1), ! A B

	(jcmp,			$,	2,		2,	0), ! A B
	(jcmpchain,		$,	2,		1,	0), ! A B
	(jbin,			$,	2,		2,	0), ! A B
	(junary,		$,	2,		1,	0), ! A B
	(jprop,			$,	2,		1,	0), ! A B
	(jbinto,		$,	2,		2,	0), ! A B
	(junaryto,		$,	1,		1,	0), ! A B
	(jincr,			$,	1,		3,	0), ! a	++a
	(jin,			$,	2,		2,	0), ! A B

	(jinrev,		$,	2,		2,	0), ! A B
	(jinrange,		$,	2,		2,	0), ! A B
	(jinset,		$,	2,		2,	0), ! A B

	(jstringz,		$,	0,		3,	0), ! A B

	(jindex,		$,	2,		3,	0), ! A B		a[b]
	(jindexlv,		$,	2,		3,	0), ! A B		a[b]
	(jslice,		$,	2,		3,	0), ! A B		a[b.a..b.b]
!	(jnewslice,		$,	2,		3,	0), ! A B		slice(a,b)

	(jdot,			$,	2,		3,	0), ! A B opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(jdotlv,		$,	2,		3,	0), ! A B opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(jdotindex,		$,	2,		3,	0), ! A B		a[b]
	(jdotslice,		$,	2,		3,	0), ! A B		a[b]
!	(janddotindex,	$,	2,		3,	0), ! A B		a[b]

	(jptr,			$,	1,		3,	0), ! a		a^
	(jptrlv,		$, 	1,		3,	0), ! a		a^
	(jaddrof,		$,	1,		3,	0), ! a		&a
	(jaddroffirst,	$,	1,		3,	0), ! a		&a
	(jdaddrvv,		$,	1,		3,	0), ! a		&&a
!	(jdaddrtv,		$,	1,		3,	0), ! a		&&a (from jdaddrvv)
	(jconvert,		$,	1,		3,	0), ! typeno=T a		T(a)			T
	(jshorten,		$,	1,		3,	0), !
	(jautocast,		$,	1,		3,	0), ! typeno=T a		T(a)			T
	(jtypepun,		$,	1,		3,	0), ! typeno=T a		T@(a)			T
	(jtypeconst,	$,	0,		3,	0), ! typeno=T			typeconst(T)
	(joperator,		$,	0,		3,	0), ! opcode=opc
	(jupper,		$,	1,		3,	0), ! a		$					T

	(jbitwidth,		$,	1,		1,	0), ! a
	(jbytesize,		$,	1,		1,	0), ! a
	(jtypestr,		$,	0,		1,	0), ! a
!	(jsliceptr,		$,	0,		1,	0), ! a
	(jbitfield,		$,	1,		3,	0), ! a

	(jminvalue,		$,	1,		3,	0), ! a
	(jmaxvalue,		$,	1,		3,	0), ! a

!Translator Variables

	(jcvlineno,		$,	0,		3,	0), !
	(jcvstrlineno,	$,	0,		3,	0), ! 
	(jcvmodulename,	$,	0,		3,	0), ! 
	(jcvfilename,	$,	0,		3,	0), ! 
	(jcvfunc,	$,	0,		3,	0), ! 
	(jcvdate,		$,	0,		3,	0), ! 
	(jcvtime,		$,	0,		3,	0), ! 
	(jcvversion,	$,	0,		3,	0), ! 
	(jcvtypename,	$,	0,		3,	0), ! 
!	(jcvtargetbits,	$,	0,		3,	0), ! 
!	(jcvtargetsize,	$,	0,		3,	0), ! 
!	(jcvtargetcode,	$,	0,		3,	0), ! 
	(jcvnil,		$,	0,		3,	0), ! 
	(jcvpi,			$,	0,		3,	0), ! 
	(jcvinfinity,	$,	0,		3,	0), ! 
	(jcvtrue,		$,	0,		3,	0), ! 
	(jcvfalse,		$,	0,		3,	0), ! 

	(jwhenthen,		$,	2,		0,	0), ! a=L b=u
	(jfmtitem,		$,	2,		3,	0), ! A B  x/fmtstr
	(jnogap,		$,	0,		3,	0), ! 
	(jspace,		$,	0,		3,	0), ! 

!Statements

!	(jcallproc,		$,	2,		0,	1), ! a=fn b=L, length
	(jreturn,		$,	1,		0,	0), ! a=x/nil
	(jsyscall,		$,	1,		3,	1), ! a=x or nil

!	(jassign,		$,	0,		3,	0), ! A B
	(jto,			$,	3,		0,	0), ! a=N, b=body, c=tempvar/nil, def=name
	(jif,			$,	3,		3,	1), ! condcode a=then b=else
	(jforup,		$,	3,		0,	0), ! 
	(jfordown,		$,	3,		0,	0), !
	(jforall,		$,	3,		0,	0), !
	(jforallrev,	$,	3,		0,	0), !
	(jwhile,		$,	3,		0,	1), ! a=x b=u
	(jrepeat,		$,	2,		0,	1), ! a=u b=x
	(jgoto,			$,	1,		0,	1), ! a=x
	(jlabeldef,		$,	0,		0,	0), ! def=nameptr
	(jredo,			$,	0,		0,	1), ! [a=x]
	(jnext,			$,	0,		0,	1), ! [a=x]
	(jexit,			$,	0,		0,	1), ! [a=x]
	(jdo,			$,	1,		0,	1), ! [a=u
	(jcase,			$,	3,		3,	1), ! a=x b=L [c=else]		L is series of whenthen pairs
	(jdocase,		$,	3,		0,	1), ! a=x b=L [c=else]
	(jswitch,		$,	3,		3,	1), ! a=x b=L [c=else]
	(jdoswitch,		$,	3,		0,	1), ! a=x b=L [c=else]
	(jdoswitchu,	$,	3,		0,	1), ! a=x b=L [c=else]
	(jdoswitchx,	$,	3,		0,	1), ! a=x b=L [c=else]
	(jswap,			$,	2,		0,	1), ! A B
	(jselect,		$,	3,		3,	1), ! Not implemented
	(jrecase,		$,	1,		0,	0), ! Not implemented
!	(jrecaseelse,	$,	0,		0,	0), ! Not implemented

	(jprint,		$,	2,		0,	1), ! [a=dev] b=L
	(jprintln,		$,	2,		0,	1), ! [a=dev] b=L
	(jfprint,		$,	3,		0,	1), ! [a=dev] b=fmtstr c=L
	(jfprintln,		$,	3,		0,	1), ! [a=dev] b=fmtstr c=L
!	(jsprint,		$,	2,		0,	0), !         b=L 
!	(jsfprint,		$,	2,		0,	0), !         b=L
	(jread,			$,	2,		0,	1), ! [a=dev] b=L
	(jreadln,		$,	2,		0,	1), ! [a=dev] b=L
!	(jsread,		$,	2,		0,	0), ! [a=dev] b=L
!	(jsreadln,		$,	2,		0,	0), ! [a=dev] b=L
	(jstop,			$,	1,		0,	0), ! [a=x]
	(jeval,			$,	1,		3,	1), ! "
!	(jstack,		$,	1,		0,	0), ! "
!	(junstack,		$,	1,		0,	0), ! "
	(jclear,		$,	1,		1,	1), ! "

!	(jdummy,		$,	0,		3,	0)
end

global enumdata []ichar bitfieldnames=
	(bf_msb,		$),
	(bf_lsb,		$),
	(bf_msbit,		$),
	(bf_lsbit,		$),
	(bf_msw,		$),
	(bf_lsw,		$),
	(bf_odd,		$),
	(bf_even,		$),
end

global enumdata [0:]ichar optypenames =
	(no_op=0,		$),
	(bin_op,		$),
	(mon_op,		$),
	(prop_op,		$),
end

!!---
global enumdata []ichar symbolnames,
					[]byte symboloptypes,
					[]byte symbolgenops,
					[]byte symbolgentoops,
					[]byte symbolopprios,
					[]byte exprstarter =
!First half are basic tokens returned by lexreadtoken()
	(dotsym,			".",		0,			0,	0,	0,	0),		! "."
	(anddotsym,			"&.",		0,			0,	0,	0,	1),		! "&."
	(commasym,			",",		0,			0,	0,	0,	0),		! ","
	(semisym,			";",		0,			0,	0,	0,	0),		! ";"
	(colonsym,			":",		0,			0,	0,	0,	0),		! ":"
	(assignsym,			":=",		bin_op,		0,	0,	1,	0),		! :=
	(sendtosym,			"=>",		0,			0,	0,	0,	0),		! =>
	(pipesym,			"->",		0,			0,	0,	0,	0),		! ->
	(lbracksym,			"(",		0,			0,	0,	0,	1),		! (
	(rbracksym,			")",		0,			0,	0,	0,	0),		! )
	(lsqsym,			"[",		0,			0,	0,	0,	1),		! [
	(rsqsym,			"]",		0,			0,	0,	0,	0),		! ]
	(lcurlysym,			"{",		0,			0,	0,	0,	0),		! {
	(rcurlysym,			"}",		0,			0,	0,	0,	0),		! }
	(ptrsym,			"^",		0,			0,	0,	0,	1),		! ^
	(barsym,			"|",		0,			0,	0,	0,	0),		! |
!	(dbarsym,			"||",		0,			0,	0,	0,	0),		! ||
	(atsym,				"@",		0,			0,	0,	0,	0),		! @
!	(datsym,			"@@",		0,			0,	0,	0,	0),		! @@
	(questionsym,		"?",		0,			0,	0,	0,	0),		! ?
	(addrsym,			"&",		0,			0,	0,	0,	1),		! &
	(daddrsym,			"&&",		0,			0,	0,	0,	0),		! &&
!	(curlsym,			"~",		0,			0,	0,	0,	0),		! ~
	(rangesym,			"..",		bin_op,		0,	0,	5,	0),		! ..
	(ellipsissym,		"...",		0,			0,	0,	0,	0),		! ...
	(hashsym,			"#",		0,			0,			0,			0,	0),		! #

!	(opsym,				$,		0,	0,	0,	0,	0),		! Any operator or property tag (use sets to distinguish)

	(addsym,			"+",		bin_op,		kadd,		kaddto,		4,	1),
	(subsym,			"-",		bin_op,		ksub,		ksubto,		4,	1),
	(mulsym,			"*",		bin_op,		kmul,		kmulto,		3,	0),
	(divsym,			"/",		bin_op,		kdiv,		kdivto,		3,	0),
	(idivsym,			"%",		bin_op,		kidiv,		kidivto,	3,	0),
	(iremsym,			"rem",		bin_op,		kirem,		kiremto,	3,	0),
	(idivremsym,		"divrem",	bin_op,		kidivrem,	0,			3,	0),
	(iandsym,			"iand",		bin_op,		kbitand,	kbitandto,	4,	0),
	(iorsym,			"ior",		bin_op,		kbitor,		kbitorto,	4,	0),
	(ixorsym,			"ixor",		bin_op,		kbitxor,	kbitxorto,	4,	0),
	(shlsym,			"<<",		bin_op,		kshl,		kshlto,		3,	0),
	(shrsym,			">>",		bin_op,		kshr,		kshrto,		3,	0),
	(minsym,			"min",		bin_op,		kmin,		kminto,		4,	1),
	(maxsym,			"max",		bin_op,		kmax,		kmaxto,		4,	1),
	(andlsym,			"and",		bin_op,		0,			0,			7,	0),
	(orlsym,			"or",		bin_op,		0,			0,			8,	0),
	(xorlsym,			"xor",		bin_op,		0,			0,			8,	0),

	(eqsym,				"=",		bin_op,		0,			0,			6,	1),
	(cmpsym,			"cmp",		bin_op,		0,			0,			6,	1),
	(powersym,			"**",		bin_op,		kpower,		0,			2,	0),
	(insym,				"in",		bin_op,		0,			0,			6,	0),
	(notinsym,			"notin",	bin_op,		0,			0,			6,	0),
	(inrevsym,			"inrev",	0,			0,			0,			0,	0),

	(notlsym,			"not",		mon_op,		knot,		knotto,		0,	1),
	(istruelsym,		"istrue",	mon_op,		0,			0,	0,	1),
	(inotsym,			"inot",		mon_op,		kbitnot,	kbitnotto,	0,	1),
	(abssym,			"abs",		mon_op,		kabs,		kabsto,		0,	1),
	(signsym,			"sign",		mon_op,		ksign,		0,			0,	1),
	(sqrtsym,			"sqrt",		mon_op,		ksqrt,		0,			0,	1),
	(sqrsym,			"sqr",		mon_op,		ksqr,		0,			0,	1),

	(propsym,			$,			prop_op,		0,			0,			0,	0),
	(mathsopsym,		$,			0,	0,	0,	0,	1),		! sin etc
	(maths2opsym,		$,			0,	0,	0,	0,	1),		! atan2 etc

	(bitfieldsym,		$,			0,	0,	0,	0,	0),		! Special bit selections
	(eolsym,			$,			0,	0,	0,	0,	0),		! End of line
	(eofsym,			$,			0,	0,	0,	0,	0),		! Eof seen
	(rawxnamesym,		$,			0,	0,	0,	0,	0),		! unassigned name, case-sensitive, that is never a reserved word
	(incrsym,			$,			0,	0,	0,	0,	1),		! 1/2 = ++/--; later may add +2 for x++/x--
	(intconstsym,		$,			0,	0,	0,	0,	1),		! 123 32 bits signed
	(realconstsym,		$,			0,	0,	0,	0,	1),		! 123.4 64 bits
	(charconstsym,		$,			0,	0,	0,	0,	1),		! 'A' or 'ABCD'
	(stringconstsym,	$,			0,	0,	0,	0,	1),		! "ABC"

!Second half are tokens that can be yielded after a name lookup:
	(unitnamesym,		$,			0,	0,	0,	0,	0),		! 
	(namesym,			$,			0,	0,	0,	0,	1),		! identifier symbol
	(kincludesym,		$,			0,	0,	0,	0,	0),		! INCLUDE
	(kstrincludesym,	$,			0,	0,	0,	0,	1),		! SINCLUDE/BINCLUDE
	(regsym,			$,			0,	0,	0,	0,	0),		! x64 registers
	(xregsym,			$,			0,	0,	0,	0,	0),		! XMM registers
	(fregsym,			$,			0,	0,	0,	0,	0),		! ST registers
	(mregsym,			$,			0,	0,	0,	0,	0),		! MMX registers
	(jmpccsym,			$,			0,	0,	0,	0,	0),		! 
	(setccsym,			$,			0,	0,	0,	0,	0),		! 
	(movccsym,			$,			0,	0,	0,	0,	0),		! 
	(segnamesym,		$,			0,	0,	0,	0,	0),		! 
	(asmopcodesym,		$,			0,	0,	0,	0,	0),		! MOV etc

	(stdtypesym,		$,			0,	0,	0,	0,	1),		! INT, CHAR etc
	(kicharsym,			$,			0,	0,	0,	0,	1),		! ICHAR IVOID
	(kifsym,			$,			0,	0,	0,	0,	1),		! 
	(kthensym,			$,			0,	0,	0,	0,	0),		! 
	(kelsifsym,			$,			0,	0,	0,	0,	0),		! 
	(kelsesym,			$,			0,	0,	0,	0,	0),		! 
	(kelsecasesym,		$,			0,	0,	0,	0,	0),		! 
	(kelseswitchsym,	$,			0,	0,	0,	0,	0),		! 
	(kendsym,			$,			0,	0,	0,	0,	0),		! 
	(kunlesssym,		$,			0,	0,	0,	0,	0),		! 
	(kcasesym,			$,			0,	0,	0,	0,	1),		! CASE
	(kdocasesym,		$,			0,	0,	0,	0,	0),		! DOCASE
	(krecasesym,		$,			0,	0,	0,	0,	0),		! RECASE
	(kwhensym,			$,			0,	0,	0,	0,	0),		! 
	(kforsym,			$,			0,	0,	0,	0,	0),		! FOR
	(ktosym,			$,			0,	0,	0,	0,	0),		! TO/DOWNTO
	(kbysym,			$,			0,	0,	0,	0,	0),		! 
	(kdosym,			$,			0,	0,	0,	0,	0),		! 
	(kwhilesym,			$,			0,	0,	0,	0,	0),		! 
	(krepeatsym,		$,			0,	0,	0,	0,	0),		! 
	(kuntilsym,			$,			0,	0,	0,	0,	0),		! 
	(kreturnsym,		$,			0,	0,	0,	0,	0),		! 
	(kstopsym,			$,			0,	0,	0,	0,	0),		! 
	(kloopsym,			$,			0,	0,	0,	0,	0),		! EXIT/NEXT/LOOP/REDO/RESTART
	(kgotosym,			$,			0,	0,	0,	0,	0),		! GO/GOTO
	(kswitchsym,		$,			0,	0,	0,	0,	0),		! SWITCH
	(kdoswitchsym,		$,			0,	0,	0,	0,	0),		! DOSWITCH
	(kprintsym,			$,			0,	0,	0,	0,	0),		! PRINT/PRINTLN/FPRINT/FPRINTLN
	(kreadsym,			$,			0,	0,	0,	0,	0),		! READ/READLN
	(kprocsym,			$,			0,	0,	0,	0,	0),		! PROC
	(kfuncsym,		$,			0,	0,	0,	0,	0),		! FUNCTION
	(klabelsym,			$,			0,	0,	0,	0,	0),		! LABEL
	(krecordsym,		$,			0,	0,	0,	0,	0),		! RECORD
	(kstructsym,		$,			0,	0,	0,	0,	0),		! STRUCT
	(kunionsym,			$,			0,	0,	0,	0,	0),		! UNION
	(kimportmodulesym,	$,			0,	0,	0,	0,	0),		! IMPORTDLL/IMPORTMODULE
	(kprojectsym,		$,			0,	0,	0,	0,	0),		! PROJECT
	(ktypesym,			$,			0,	0,	0,	0,	0),		! TYPE
	(krefsym,			$,			0,	0,	0,	0,	1),		! REF
	(kvoidsym,			$,			0,	0,	0,	0,	1),		! VOID
	(kvarsym,			$,			0,	0,	0,	0,	0),		! MUT
	(kletsym,			$,			0,	0,	0,	0,	0),		! LET
	(kslicesym,			$,			0,	0,	0,	0,	0),		! SLICE/SLICE2D
	(kmacrosym,			$,			0,	0,	0,	0,	0),		! MACRO
!	(koperatorsym,		$,			0,	0,	0,	0,	0),		! OPERATOR
	(kconstsym,			$,			0,	0,	0,	0,	0),		! 
	(kclearsym,			$,			0,	0,	0,	0,	0),		! CLEAR
	(kheadersym,		$,			0,	0,	0,	0,	0),		! MODULE
	(kglobalsym,		$,			0,	0,	0,	0,	0),		! global
	(kstaticsym,		$,			0,	0,	0,	0,	0),		! STATIC

	(kcastsym,			$,			0,	0,	0,	0,	1),		! CAST
	(compilervarsym,	$,			0,	0,	0,	0,	1),		! $lineno etc
	(dollarsym,			$,			0,	0,	0,	0,	1),		! to be used for current array upperbound; also tabledata names
	(kevalsym,			$,			0,	0,	0,	0,	0),		! EVAL
	(ktabledatasym,		$,			0,	0,	0,	0,	0),		! tabledata
	(kclampsym,			$,			0,	0,	0,	0,	1),			! CLAMP
	(kswapsym,			$,			0,	0,	0,	0,	0),		! SWAP
	(ksyscallsym,		$,			0,	0,	0,	0,	1),		! $getprocname etc
end

global enumdata []ichar headerdirnames =
	(hdr_module,		$),
	(hdr_import,		$),
	(hdr_sourcepath,	$),
	(hdr_linkdll,		$),
end

global enumdata [0:]ichar scopenames=
	(Module_scope=0,	"Local"), ! 		!module
	(subprog_scope,		"Global"), ! 		!inter-subprog
	(program_scope,		"Program"), ! 		!inter-module
	(export_scope,		"Export"), ! 		!inter-program
end

global enumdata =
	million_unit,
	billion_unit,
end

global enumdata [0:]ichar parammodenames=
	(byval_param=0,		"Byval "),
	(byref_param,		"Byref "),
end

global enumdata [0:]ichar namenames, [0:]byte name2pid =
	(nullid=0,		$,		0),				!Not assigned
	(programid,		$,		0),				!Main root
	(subprogid,		$,		0),
	(moduleid,		$,		program_id),	!Current or imported module
	(dllmoduleid,	$,		0),				!
	(typeid,		$,		0),				!Type name in type, proc or module
	(procid,		$,		proc_id),		!Proc/method/func/op name
	(dllprocid,		$,		import_id),		!Dll Proc/func name
	(dllvarid,		$,		0),				!Dll variable name
	(constid,		$,		0),				!Named constant in type, proc or module
	(staticid,		$,		static_id),		!Static in type or proc or module
	(frameid,		$,		local_id),		!Local var
	(paramid,		$,		param_id),		!Local param
	(fieldid,		$,		0),				!Field of Record or Class
	(labelid,		$,		label_id),		!Label name in proc only
	(macroid,		$,		0),				!Name of macro
	(macroparamid,	$,		0),				!Macro formal parameter name
	(linkid,		$,		0),				!Name in class defined in a base class
end

global enumdata []ichar propnames =
	(kksliceptr,	$),
	(kklen,			$),
	(kklwb,			$),
	(kkupb,			$),
	(kkbounds,		$),
	(kkbitwidth,	$),
	(kkbytesize,	$),
	(kktypestr,		$),
	(kkminval,		$),
	(kkmaxval,		$),
end

!!---
global tabledata []ichar stnames, []byte stsymbols, []i16 stsubcodes=

	("if",			kifsym,			jif),
	("then",		kthensym,		0),
	("elsif",		kelsifsym,		jif),
	("else",		kelsesym,		0),
	("dummyelse",	kelsesym,		1),
	("elsecase",	kelsecasesym,	jcase),
	("elseswitch",	kelseswitchsym,	jswitch),
	("case",		kcasesym,		jcase),
	("docase",		kdocasesym,		jdocase),
	("recase",		krecasesym,		jrecase),
	("when",		kwhensym,		0),
	("for",			kforsym,		0),
	("to",			ktosym,			0),
	("downto",		ktosym,			1),
	("by",			kbysym,			0),
	("do",			kdosym,			0),
	("end",			kendsym,		0),
	("while",		kwhilesym,		0),
	("repeat",		krepeatsym,		0),
	("until",		kuntilsym,		0),
	("return",		kreturnsym,		0),
	("stop",		kstopsym,		0),
	("redoloop",	kloopsym,		jredo),
	("nextloop",	kloopsym,		jnext),
	("exit",		kloopsym,		jexit),
	("goto",		kgotosym,		0),
	("switch",		kswitchsym,		jswitch),
	("doswitch",	kdoswitchsym,	jdoswitch),
	("doswitchu",	kdoswitchsym,	jdoswitchu),
	("doswitchx",	kdoswitchsym,	jdoswitchx),
	("tabledata",	ktabledatasym,	0),
	("enumdata",	ktabledatasym,	1),
	("clamp",		kclampsym,		0),
	("eval",		kevalsym,		0),
	("print",		kprintsym,		jprint),
	("println",		kprintsym,		jprintln),
	("fprint",		kprintsym,		jfprint),
	("fprintln",	kprintsym,		jfprintln),
!	("sprint",		ksprintsym,		jsprint),
!	("sfprint",		ksprintsym,		jsfprint),

	("cp",			kprintsym,		jprint),
	("cpl",			kprintsym,		jprintln),

	("read",		kreadsym,		jread),
	("readln",		kreadsym,		jreadln),
	("cast",		kcastsym,		jconvert),

	("function",	kfuncsym,	0),
	("func",		kfuncsym,	0),
	("proc",		kprocsym,		0),
	("fun",			kfuncsym,	1),
	("threadedproc",kprocsym,		2),

	("type",		ktypesym,		0),
	("record",		krecordsym,		0),
	("struct",		kstructsym,		0),
	("union",		kunionsym,		0),
	("ref",			krefsym,		0),
	("var",			kvarsym,		0),
	("let",			kletsym,		0),

	("include",		kincludesym,	0),
	("binclude",	kstrincludesym,	'B'),
	("sinclude",	kstrincludesym,	'S'),
	("strinclude",	kstrincludesym,	'S'),

	("macro",		kmacrosym,		0),

	("static",		kstaticsym,		0),
	
	("const",		kconstsym,		0),

	("$getnprocs",		ksyscallsym,	sf_getnprocs),
	("$getprocname",	ksyscallsym,	sf_getprocname),
	("$getprocaddr",	ksyscallsym,	sf_getprocaddr),

	("importdll",	kimportmodulesym,	0),
	("project",		kprojectsym,		0),
	("unless",		kunlesssym,			0),

	("global",		kglobalsym,		subprog_scope),
	("export",		kglobalsym,		export_scope),

	("swap",		kswapsym,		0),

	("void",		kvoidsym,		0),
	("int",			stdtypesym,		tint),
	("word",		stdtypesym,		tword),
	("real",		stdtypesym,		treal),

	("ichar",		kicharsym,		tc8),
	("ivoid",		kicharsym,		tvoid),

	("i8",			stdtypesym,		ti8),
	("i16",			stdtypesym,		ti16),
	("i32",			stdtypesym,		ti32),
	("i64",			stdtypesym,		ti64),

	("r32",			stdtypesym,		tr32),
	("r64",			stdtypesym,		tr64),

	("byte",		stdtypesym,		tu8),
	("u8",			stdtypesym,		tu8),
	("u16",			stdtypesym,		tu16),
	("u32",			stdtypesym,		tu32),
	("u64",			stdtypesym,		tu64),

!	("word8",		stdtypesym,		tu8),
!	("u16",		stdtypesym,		tu16),
!	("u32",		stdtypesym,		tu32),
!	("u64",		stdtypesym,		tu64),

	("char",		stdtypesym,		tc8),
	("c8",			stdtypesym,		tc8),
!	("char8",		stdtypesym,		tc8),
	("c64",			stdtypesym,		tc64),
!	("c64",		stdtypesym,		tc64),

	("bool64",		stdtypesym,		tbool64),
	("bool",		stdtypesym,		tbool64),
	("bool8",		stdtypesym,		tbool8),

	("label",		stdtypesym,		tlabel),

	("slice",		kslicesym,		tslice),

	("million",		unitnamesym,	million_unit),
	("billion",		unitnamesym,	billion_unit),

	("$lineno",		compilervarsym,	jcvlineno),
	("$strlineno",	compilervarsym,	jcvstrlineno),
	("$filename",	compilervarsym,	jcvfilename),
	("$modulename",	compilervarsym,	jcvmodulename),
	("$function",	compilervarsym,	jcvfunc),
	("$date",		compilervarsym,	jcvdate),
	("$time",		compilervarsym,	jcvtime),
	("$version",	compilervarsym,	jcvversion),
	("$typename",	compilervarsym,	jcvtypename),
!	("$targetbits",	compilervarsym,	jcvtargetbits),
!	("$targetsize",	compilervarsym,	jcvtargetsize),
!	("$targetname",	compilervarsym,	jcvtargetname),
!	("$targetcode",	compilervarsym,	jcvtargetcode),
	("nil",			compilervarsym,	jcvnil),
	("pi",			compilervarsym,	jcvpi),
	("true",		compilervarsym,	jcvtrue),
	("false",		compilervarsym,	jcvfalse),
	("infinity",	compilervarsym,	jcvinfinity),
	("$",			dollarsym,		0),

	("and",			andlsym,		0),
	("or",			orlsym,			0),
	("xor",			xorlsym,		0),
	("iand",		iandsym,		0),
	("ior",			iorsym,			0),
	("ixor",		ixorsym,		0),
!	("in",			insym,			kkin),
!	("notin",		notinsym,		kknotin),
	("in",			insym,			0),
	("notin",		notinsym,		1),
	("inrev",		inrevsym,		0),
	("rem",			iremsym,		0),
	("divrem",		idivremsym,		0),
	("min",			minsym,			0),
	("max",			maxsym,			0),

	("not",			notlsym,		0),
	("inot",		inotsym,		0),
	("istrue",		istruelsym,		0),
	("abs",			abssym,			kabs),
!	("$neg",		negsym,			0),
!	("byteswap",	byteswapsym,	0),

	("sqr",			sqrsym,			0),
	("sqrt",		sqrtsym,		0),
	("sign",		signsym,		0),

	("sin",			mathsopsym,		ksin),
	("cos",			mathsopsym,		kcos),
	("tan",			mathsopsym,		ktan),
	("asin",		mathsopsym,		kasin),
	("acos",		mathsopsym,		kacos),
	("atan",		mathsopsym,		katan),
	("log",			mathsopsym,		klog),
	("log10",		mathsopsym,		klog10),
	("exp",			mathsopsym,		kexp),
	("round",		mathsopsym,		kround),
	("floor",		mathsopsym,		kfloor),
	("ceil",		mathsopsym,		kceil),

	("atan2",		maths2opsym,	katan2),
	("fmod",		maths2opsym,	kfmod),

	("sliceptr",	propsym,		kksliceptr),
	("len",			propsym,		kklen),
	("lwb",			propsym,		kklwb),
	("upb",			propsym,		kkupb),
	("bounds",		propsym,		kkbounds),
	("bitwidth",	propsym,		kkbitwidth),
	("bytes",		propsym,		kkbytesize),
	("typestr",		propsym,		kktypestr),

	("msb",			bitfieldsym,	bf_msb),
	("lsb",			bitfieldsym,	bf_lsb),
	("msbit",		bitfieldsym,	bf_msbit),
	("lsbit",		bitfieldsym,	bf_lsbit),
	("msw",			bitfieldsym,	bf_msw),
	("lsw",			bitfieldsym,	bf_lsw),
	("odd",			bitfieldsym,	bf_odd),
	("even",		bitfieldsym,	bf_even),

	("fi",			kendsym,		kifsym),
	("esac",		kendsym,		kcasesym),
	("od",			kendsym,		kdosym),

	("$caligned",	atsym,			1),
	("clear",		kclearsym,		0),

	("module",		kheadersym,		hdr_module),
	("import",		kheadersym,		hdr_import),
	("$sourcepath",	kheadersym,		hdr_sourcepath),
	("linkdll",		kheadersym,		hdr_linkdll),
end

global enumdata [0:]ichar convnames, [0:]byte convtopcl =
	(kkerror=0,     $,		0),
!	(kktypepun,     $,		0),
	(kkfloat,       $,		kfloat),
	(kkfix,         $,		kfix),
	(kktruncate,    $,		ktruncate),
	(kkwiden,       $,		kwiden),
	(kkfwiden,      $,		kfwiden),
	(kkfnarrow,     $,		kfnarrow),
	(kksoftconv,    $,		0),
	(kktoboolt,     $,		ktoboolt),
	(kkharderr,     $,		0),
	(kksofttrun,    $,		0),
	(kkichar2sl,    $,		0),
	(kkax2slice,    $,		0),
	(kkcx2ichar,    $,		0),
end

global []int D_typestarterset= (stdtypesym,lsqsym,krefsym,krecordsym,
		kicharsym, kslicesym)

global [tr64..tc64, tr64..tc64]i16 softconvtable = (
!To: r64			r32			i64			u64			c64				 From:
	(kksoftconv,	kkfnarrow,	kkfix,		kkfix,		kkfix),			!r64
	(kkfwiden,		kksoftconv,	kkfix,		kkfix,		kkfix),			!r32
	(kkfloat,		kkfloat,	kksoftconv,	kksoftconv,	kksoftconv),	!i64
	(kkfloat,		kkfloat,	kksoftconv,	kksoftconv,	kksoftconv),	!u64
	(kkfloat,		kkfloat,	kksoftconv,	kksoftconv,	kksoftconv))	!c64

global [symbolnames.lwb..symbolnames.upb]byte endsexpr
global []byte exprendsymbols=(rbracksym,rsqsym,kthensym,kelsifsym,
			kelsesym, kuntilsym, kdosym, kendsym, commasym, barsym,
			semisym, ktosym)

global [jtagnames.lwb..jtagnames.upb]byte isbooltag

proc start=
	int genop, s,t, a, specop

	for i to exprendsymbols.len do
		endsexpr[exprendsymbols[i]]:=1
	od

	isbooltag[jcmp]:=1
	isbooltag[jcmpchain]:=1
	isbooltag[jandl]:=1
	isbooltag[jorl]:=1
	isbooltag[jnotl]:=1
	isbooltag[jistruel]:=1
	isbooltag[jisfalsel]:=1
	isbooltag[jinrange]:=1
	isbooltag[jinset]:=1
end

=== mm_type.m 0 0 28/34 ===
const nolv=0
const needlv=1

const maxparams=100
const maxfields=200
int countedfields
int inassem
int inidata

!proc tpass(unit p, int t=tany, lv=nolv, hard=0)=
proc tpass(unit p, int t=tany, lv=nolv)=
	symbol d
	unit a,b,c, q
	int oldmmpos,m,nparams,paramtype,restype,amode
	static int depth

	if p=nil then return fi
	if depth=100 then
		txerror("TX looping detected")
	fi
	++depth

	oldmmpos:=mmpos

!CPL "TPASS------------------------", JTAGNAMES[P.TAG]

	mmpos:=p.pos

	a:=p.a
	b:=p.b
	c:=p.c

	p.resultflag:=t<>tvoid

	switch p.tag
	when jname then
		tx_name(p,t,lv)
	when jconst, jdecimal then

	when jtypeconst then
		p.mode:=ti64

	when jbytesize, jbitwidth then
		tpass(a)
		p.mode:=ti64

	when jbin, jcmp then
		tx_bin(p,a,b)

	when jin then
		tx_in(p,a,b)

	when junary then
		tx_unary(p,a)

	when jprop then
		tx_prop(p,a)

	when jbinto then
		tx_binto(p,a,b)

	when junaryto then
		tx_unaryto(p,a)

	when jassign then
		tx_assign(p,a,b,t)

	when jaddrof then
		if a.tag=jptr then
			deleteunit(p,a)
			deleteunit(p,p.a)
!			tpass(p,t,lv,hard)
			tpass(p,t,lv)
		else
			tpasslv(a)
			p.mode:=createrefmode(nil,a.mode)
		fi

	when jaddroffirst then
		tx_addroffirst(p,a,t)

	when jif then
		tx_if(p,a,b,c,t,lv)

	when jindex then
		tx_index(p,a,b,t,lv)

	when jptr then
		tx_ptr(p,a,t,lv)

	when jcall then
		tx_callproc(p,a,b,t)

	when jdot then
		tx_dot(p,a,b,lv)

	when jandl, jorl then
		tx_andl(p,a,b)

	when jnotl, jistruel, jisfalsel then
		tx_notl(p,a)

	when jconvert then
		tx_convert(p,a,1)

	when jtypepun then
		tx_typepun(p,a)

	when jincr then
		tx_incrto(p,a,t)

	when jmakerange then
		tx_makerange(p,a,b)

	when jswap then
		tx_swap(p,a,b)

	when jselect then
		tx_select(p,a,b,c,t,lv)

	when jswitch, jdoswitch, jdoswitchu, jdoswitchx then
		tx_switch(p,a,b,c,t,lv)

	when jcase, jdocase then
		tx_case(p,a,b,c,t,lv)

	when jdotindex, jdotslice then
		tx_dotindex(p,a,b,lv)

	when jslice then
		tx_slice(p,a,b)

	when jblock then
		tx_block(p,a,t,lv)

	when jeval then
		tpass(a,tany)

	when jdo then
		tpass(a,tvoid)

	when jreturn then
		tx_return(p,a,t)

	when jprint,jprintln,jfprint,jfprintln then

		tx_unitlist(a)
		fixchararray(a)

		while b do
			if b.tag=jfmtitem then
				tpass(c:=b.a)
				tpass(b.b,trefchar)
			else
				tpass(c:=b)
			fi
			fixchararray(c)
			b:=b.nextunit
		od
		tx_unitlist(p.c)

	when jforup, jfordown then
		tx_for(a,b,c)

	when jforall, jforallrev then
		tx_forall(a,b,c)

	when jto then
		tpass(a,ti64)
		tpass(b,tvoid)
		tpass(c,ti64)		!when autovar present

	when jautocast then
		tpass(a)
		if t=tany then txerror("cast() needs type") fi
		coerceunit(a,t,1)
		deleteunit(p,a)

	when jmakelist then
		tx_makelist(p,a,t,lv)

	when jstop then
		tpass(a,ti64)

	when jexit,jredo, jnext then
		tx_exit(p,a)

	when jgoto then
		tx_goto(p,a)

	when jlabeldef then

	when jwhile then
		tpass(a,tbool)
		if iscondtrue(a) then
			p.tag:=jdo
			p.a:=b
		elsif iscondfalse(a) then
			p.tag:=jnull
		fi
		tpass(b,tvoid)
		tpass(c,tvoid)

	when jrepeat then
		tpass(a,tvoid)
		tpass(b)
		if iscondtrue(b) or iscondfalse(b) then txerror("repeat/const cond") fi

	when jnogap, jspace then

	when jtypestr then
		tpass(a)
		if a.tag=jtypeconst then
			m:=a.value
		else
			tpass(a)
			m:=a.mode
		fi
		p.tag:=jconst
		p.mode:=trefchar
		p.a:=nil
		p.svalue:=pcm_copyheapstring(strmode(m,0))
		p.slength:=strlen(p.svalue)+1
		p.isastring:=1

	when jfmtitem then
		tpass(a)
		tpass(b)

	when jreadln then
		tpass(a)

	when jread then
		if a then
			tpass(a,tc64)
		fi
		if ttisinteger[t] or ttisreal[t] then
			t:=gettypebase(t)
		fi
		p.mode:=t
	when jrecase then
		if a then
			tpass(a,ti64)
			if a.tag<>jconst then
				txerror("recase must be const")
			fi
		fi

	when jcvfilename,jcvmodulename then
		p.mode:=trefchar

	when jbitfield then
		tx_bitfield(p,a,lv)

	when jsyscall then
		restype:=tvoid
		paramtype:=tvoid
		case p.fnindex
		when sf_getnprocs then restype:=ti64
		when sf_getprocname then paramtype:=ti64; restype:=trefchar
		when sf_getprocaddr then paramtype:=ti64; restype:=tref 
		esac

		if paramtype<>tvoid then
			if a=nil then txerror("sys: arg missing") fi
			tpass(a,paramtype)
			if a.nextunit then txerror("sys: too many args") fi
		elsif a then txerror("sys: too many args")
		fi

		p.mode:=restype

	when jcmpchain then
		tx_cmpchain(p,a)

	when jclear then
		tpasslv(a)
		case ttbasetype[a.mode]
		when trecord, tarray then
!CPL "CLEAR BLOCK"
		else
			txerror("Clear scalar?")
		esac


	when jshorten then

	when jstrinclude then
		tx_strinclude(p,a)

	when jmakeslice then
		tx_makeslice(p,a,b,t)

	when jmakeset then
		tx_makeset(p,a,t)

	when jsourceline then

	else
		CPL "TXUNIT: CAN'T DO:",jtagnames[p.tag]
	doelse:

		for i to jsubs[p.tag] do
			tx_unitlist(p.abc[i],t)
		od
	end switch

	tevaluate(p)

	case p.tag
	when jmakelist, jreturn then
	else
		if t<>tany and t<>tvoid and p.mode<>t then		!does not already match
!			coerceunit(p,t, hard)			!apply soft conversion
			coerceunit(p,t)			!apply soft conversion
		fi
	esac

	IF T=TVOID THEN
		CASE P.TAG
		WHEN JCONST, JBIN, JUNARY, JCMP THEN
!			TXERROR("Eval needed")
		WHEN JNAME THEN
			unless ttisref[p.mode] and tttarget[p.mode]=tlabel then
!				TXERROR("Eval needed2")
			end

		esac
	fi

	mmpos:=oldmmpos
	--depth
end

global proc tx_allprocs=
	ref procrec pp
	unit pcode

	pp:=proclist
	while pp do
		currproc:=pp.def
		pcode:=currproc.code

	    tpass(pcode,(currproc.nretvalues>1|ttuple|currproc.mode))

		case ttbasetype[currproc.mode]
		when tvoid then		!PROC
		when ttuple then	!MULT FN
		else				!REGULAR FN
			if pcode.tag<>jreturn then
!			if NOT CTARGET AND pcode.tag<>jreturn then
				insertunit(pcode,jreturn)
				pcode.mode:=currproc.mode
				pcode.resultflag:=1
			fi
		esac

		pp:=pp.nextproc
	od
end

proc tx_block(unit p,a, int t,lv)=
	while a and a.nextunit do
		tpass(a,tvoid)
		a:=a.nextunit
	od
	if a then
		tpass(a,t,lv)
		p.mode:=(t<>tvoid|a.mode|tvoid)
	fi
end

global proc tx_typetable=
	symbol d

	for i:=tuser to ntypes do
		if ttbasetype[i]=trecord then
			tx_passdef(d:=ttnamedef[i])
		fi
		setmodesize(i)
	od
end

proc setmodesize(int m)=
	int size,target

	if ttsize[m] then return fi


	mmpos:=ttlineno[m]
	case ttbasetype[m]
	when tarray then
		setarraysize(m)
	when trecord then
		setrecordsize(m)
	when tvoid,tproc then
	when tslice then
		setslicesize(m)
	when tauto then
		TXERROR("SETMODESIZE/AUTO?")
	when tany then

	when tpending then
		target:=tttarget[m]
		setmodesize(target)

		ttbasetype[m]:=ttbasetype[target]
		ttsize[m]:=ttsize[target]
		ttlower[m]:=ttlower[target]
		ttlength[m]:=ttlength[target]
		ttnamedef[m]:=ttnamedef[target]

	when ttuple then

	else
		if size:=ttsize[ttbasetype[m]] then
			ttsize[m]:=size
			return
		fi
		println "SIZE 0:",strmode(m),=m,=stdnames[ttbasetype[m]]
		println "Can't set mode size"
	esac
end

proc setarraysize(int m)=
	int lower,length,elemsize,target,size
	unit pdim,a,b

	if ttsizeset[m] then return fi

	pdim:=ttdimexpr[m]

	if pdim then
		a:=pdim.a
		b:=pdim.b
		rx_unit(ttowner[m],pdim)

		case pdim.tag
		when jmakerange then
			tpass(a)
			tpass(b)
			lower:=getconstint(a)
			length:=getconstint(b)-lower+1
		when jkeyvalue then
			tpass(a)
			lower:=getconstint(a)
			if b then
				tpass(b)
				length:=getconstint(b)
			else
				length:=0
			fi
		else
			tpass(pdim)
			length:=getconstint(pdim)
			lower:=1
		esac
	else
		lower:=1
		length:=0
	fi

	if length<0 then txerror("Neg length") fi
	ttdimexpr[m]:=nil

	ttlower[m]:=lower
	ttlength[m]:=length

	target:=tttarget[m]
	setmodesize(target)
	elemsize:=ttsize[tttarget[m]]
	ttsize[m]:=size:=length*elemsize
	ttsizeset[m]:=1

	checkblocktype(m)
end

proc setslicesize(int m)=
	unit pdim

	if ttsize[m] then return fi

	pdim:=ttdimexpr[m]

	if pdim then
		rx_unit(ttowner[m],pdim)
		tpass(pdim)
		ttlower[m]:=getconstint(pdim)
		ttdimexpr[m]:=nil
	else
		ttlower[m]:=1
	fi

	setmodesize(tttarget[m])
	ttsize[m]:=ttsize[tslice]
end

global func tx_module(int n)int=
	modulerec m
	symbol d
	int globalflag,status

	currmoduleno:=n

	tx_passdef(modules[n].stmodule)

	return 1
end

global proc tx_passdef(symbol p)=
	symbol d
	int oldmmpos
	unit q

	if p.txdone then
		return
	fi

	oldmmpos:=mmpos
	mmpos:=p.pos

	d:=p.deflist
	while d do
		tx_passdef(d)
		d:=d.nextdef
	od

	q:=p.code

	case p.nameid
	when procid then
		currproc:=nil
		currproc:=nil
	when constid then
		tx_namedconst(p)
	when staticid, frameid, paramid then
		tx_namedef(p)
	esac

	p.txdone:=1
	mmpos:=oldmmpos
end

proc tx_unitlist(unit p, int t=tany, lv=nolv)=
	while p do
		tpass(p,t)
		p:=p.nextunit
	od
end

proc tx_namedef(symbol d)=
	int m,mold,inidataold
	unit dcode,pequiv

	if d.circflag then
		txerror("Circular reference detected")
	fi
	if d.txdone then return fi

	m:=d.mode
	setmodesize(m)

	dcode:=d.code

	d.circflag:=1

	if d.atvar then
		pequiv:=d.equivvar
		if pequiv.tag=jaddrof then deleteunit(pequiv,pequiv.a) fi
		if pequiv.tag<>jname then
			txerror("@name needed")
		fi
		tpass(pequiv)
	fi

	if dcode and d.nameid<>frameid then
		mold:=m
		m:=gettypebase(m)

		if ttbasetype[m]=tslice and dcode.tag=jconst and dcode.mode=trefchar then
			tpass(dcode,trefchar)
		else
			inidataold:=inidata
			inidata:=1
			tpass(dcode,m)
			inidata:=inidataold
		fi
		d.circflag:=0
		d.txdone:=1
		if ttbasetype[m]=tarray and ttlength[m]=0 then
			d.mode:=dcode.mode
		fi

		if mold<>m then
			if ttisinteger[m] and ttisshort[mold] then
				insertunit(d.code,jshorten)
				d.code.mode:=mold
			elsif mold=tr32 then
				d.code.mode:=mold
			fi
		fi

		if d.nameid=staticid then
			checkconstexpr(d.code)
		fi

	elsif dcode and d.nameid=frameid and ttbasetype[m]=tarray and ttlength[m]=0 then
		tpass(dcode,m)
		d.mode:=dcode.mode
		d.circflag:=0
		d.txdone:=1

	else
		d.circflag:=0
		d.txdone:=1
	fi
end

global proc tx_namedconst(symbol d)=
	int m

	if d.circflag then
		txerror("Circular const reference detected")
	fi

	unit q
	if d.txdone then return fi
	q:=d.code

	m:=d.mode

	d.circflag:=1
	tpass(q,(m=tauto|tany|m))

	d.circflag:=0
	checkconstexpr(q)
	if m=tauto then
		d.mode:=q.mode
	fi

	case ttbasetype[d.mode]
	when tref then
		if d.mode<>trefchar then
			txerror("Bad const type")
		fi
	esac

	d.txdone:=1
end

proc checkconstexpr(unit p)=
!check whether p is const expr
	unit q
	int pmode

	case p.tag
	when jconst, jtypeconst then
		return
	when jmakelist then
		q:=p.a
		while q do
			checkconstexpr(q)
			q:=q.nextunit
		od

	when jconvert then
		if ttbasetype[p.a.mode]=tref then
			p.a.mode:=p.mode
			deleteunit(p,p.a)
		else
			goto error
		fi

	when jshorten then
		checkconstexpr(p.a)

	when jaddrof, jaddroffirst then
		case p.a.tag
		when jname then
		else
			goto error
		esac

	when jname then
		if p.def.nameid=fieldid then return fi
		if p.def.nameid=procid then return fi
		if p.def.nameid=labelid then return fi
		error
	else
	error:
		println =jtagnames[p.tag],STRMODE(P.MODE)
		PRINTUNIT(P)
		txerror("Getconstexpr: not const")
	esac
end

func getconstint(unit q)i64=
	checkconstexpr(q)

	if ttisinteger[q.mode] or q.tag=jtypeconst then
		return q.value
	elsif ttisreal[q.mode] then
		return q.xvalue
	else
		cpl strmode(q.mode)
		txerror("Getconstint: not i32/64")
	fi
	return 0
end

proc makenewconst(unit p,i64 x,int t=tvoid)=
!modify p (usually a binop, monop, convert op etc) to a new const unit
!p will usually already have the result mode
!the x value will do for int/word/real

	p.tag:=jconst
	p.a:=p.b:=nil
	p.value:=x
	p.isconst:=1
	if t<>tvoid then
		p.mode:=t
	fi
end

proc tx_name(unit p,int t,lv)=
	symbol d
	int oldmmpos
	unit pcode
	oldmmpos:=mmpos

IF P.TXCOUNT THEN
RETURN
FI
++P.TXCOUNT

	d:=p.def
	mmpos:=d.pos

	case d.nameid
	when constid then			!note: currently, rxpass converts names to constants

		if lv then txerror("&const") fi

		tx_namedconst(d)
		pcode:=d.code

		p.tag:=jconst
		p.def:=nil
		p.a:=nil
	    p.c:=nil

		if pcode.tag=jconvert then		!assume c_soft
			p.value:=pcode.a.value

		else
			p.value:=pcode.value
		fi

		p.slength:=pcode.slength
		p.mode:=d.mode
		p.isconst:=1
		p.isastring:=pcode.isastring

	when staticid,frameid,paramid then
		if d.islet and lv then
!			println D.NAME,=LV,D.ISLET
			txerror_s("Can't use 'let' as lvalue: ",d.name)
		fi

		tx_namedef(d)

		if not inassem then
			p.mode:=d.mode
			if d.parammode=byref_param then
IF NOT P.INSPTR THEN
++P.INSPTR
				insertunit(p, jptr)
				p.mode:=tttarget[d.mode]
			fi
FI
			twiden(p,lv)
		else
			p.mode:=trefchar
		fi

	when procid,dllprocid then

		p.mode:=trefproc	!use generic refproc mode (yields return type of actual proc mode
				!after a call op, or actual refproc in other context. Don't use actual
				!refproc here, to avoid generating thousands of ref proc modes, one
				!for each call, that will never be needed

	when labelid then
		p.mode:=treflabel

	when moduleid then
		txerror_s("Module name can't be used on it's own: #",d.name)

	when fieldid then
		p.tag:=jconst
		p.def:=nil
		p.a:=nil
	    p.c:=nil

		p.value:=d.offset

		p.mode:=ti64
		p.isconst:=1


	when typeid then
		p.tag:=jtypeconst
		p.value:=d.mode
		p.mode:=ti64

	when dllvarid then
		if d.code then
			txerror("Can't init dllvar")
		fi
		p.mode:=d.mode

	else
		mmpos:=p.pos
		txerror_ss("TNAME? # #",namenames[d.nameid],d.name)
	esac
	mmpos:=oldmmpos

end

proc tx_bin(unit p,a,b)=
!deal with most binary ops
	unit q
	int amode,bmode,abase,bbase,cmode, resmode, relop, simpleset

	tpass(a)
	tpass(b)
	amode:=a.mode
	bmode:=b.mode

	case p.pclop
	when kadd then
		if dobinnumx(p,a,b) then return fi
		if ttisref[amode] then
			if ttisref[bmode] and a.isastring and b.isastring then
				combinestrings(p)
				return
			fi
			if isnum(bmode) then
				coerceunit(b,ti64)
				p.pclop:=kaddpx
				p.mode:=amode
				return
			fi
		fi

	when ksub then
		if dobinnumx(p,a,b) then return fi
		if ttisref[amode] then
			if ttisref[bmode] then
				if comparemodes(amode, bmode) then
					p.pclop:=ksubp
					p.mode:=ti64
					return
				else
					txerror("ref-ref: not compat")
				fi
			fi
			if isnum(bmode) then
				coerceunit(b,ti64)
				p.pclop:=ksubpx
				p.mode:=amode
				return
			fi

		fi

	when kmul then
		if dobinnumx(p,a,b) then return fi
		if ttisref[amode] then
			if a.isastring and ttisinteger[b.mode] and b.tag=jconst then
				mulstrings(p)
				return
			fi
		fi


	when kdiv then
		if isnumi(amode) and isnumi(bmode) then p.pclop:=kidiv; goto doidiv fi
		if dobinnumf(p,a,b) then return fi
		if isnum(amode) and isnum(bmode) then
			p.mode:=tr64
			coerceunit(a,tr64)
			coerceunit(b,tr64)
			return
		fi

	when kidiv, kirem, kidivrem, kbitand, kbitor, kbitxor then
doidiv:
		if dobinnumi(p,a,b) then return fi

	when kmin, kmax then
		if dobinnumx(p,a,b) then return fi

	when kpower then
		if dobinnumx(p,a,b) then return fi

	when kfmod, katan2 then
		coerceunit(a,tr64)
		coerceunit(b,tr64)
		p.mode:=tr64
		return

	when kshl, kshr then
		if isnumi(amode) then
			coerceunit(b,ti64)
			p.mode:=amode
			return
		fi

	elsif p.pclcond then
		if dobinnumx(p,a,b) then
			p.mode:=tbool
			return
		fi
		p.mode:=tbool
		if ttisref[amode] and ttisref[bmode] then
			if not comparemodes(amode, bmode) then
				txerror("Cmp ref/ref not compat")
			fi
			return
		fi
		if p.pclcond in [eq_cc, ne_cc] then
			if comparemodes(amode, bmode) then
				return
			fi
		fi

	else
		txerror("txbin?")
	esac

cpl pclnames[p.pclop]
	TXERROR_SS("BIN/CAN'T RESOLVE MODES",strmode(amode),strmode2(bmode))
end

proc tx_binto(unit p,a,b)=
	int abase, bbase, amode,bmode, opc

	tpasslv(a)
	tpass(b)

	amode:=a.mode
	bmode:=b.mode

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if p.pclop=kdivto and ttisinteger[abase] then
		p.pclop:=kidivto
	fi

	p.mode:=tvoid

	case p.pclop
	when kaddto then				!ref+ref not allowed; or ref+int (later refchar+refchar)
		if abase=tref and bbase=tref then
			txerror("to:ref+ref")
		fi
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=kaddpxto
			return
		fi
	when ksubto then				!ref-int
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=ksubpxto
			return
		fi
	when kshlto, kshrto, kbitandto, kbitorto, kbitxorto then
		coerceunit(b,ti64)
		return
	esac

	if isnum(abase) and isnum(bbase) then	!num op num
		coerceunit(b,abase)

	elsif ttisshort[abase] and isnum(bbase) then
		coerceunit(b,abase)

	else
		if not comparemodes(amode,bmode) then
			txerror_ss("BIN: modes not compatible: # #",strmode(amode),strmode(bmode))
		fi
	fi
end

func getdominantmode(int amode,bmode)int=
	int abase,bbase

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if isnum(abase) and isnum(bbase) then
		return min(abase,bbase)
	fi
	if not comparemodes(amode, bmode) then
		txerror("Getdom: no dominant mode")
	fi
	return amode
end

proc tx_cmpchain(unit p,a)=
	int u,genop
	unit q,r

	q:=a
	while q do
		tpass(q,tany)

		if q=a then
			u:=q.mode
		else
			u:=getdominantmode(u,q.mode)
		fi

		q:=q.nextunit
	od

	q:=a
	r:=a.nextunit
	while q do
		coerceunit(q,u)
		q:=q.nextunit
	od

	for i:=1 to p.cmpgenop.len do
		genop:=p.cmpgenop[i]
		if genop=0 then exit fi

		p.cmppclmode[i]:=getpclmode(u)
	od

	p.mode:=ti64
!	p.mode:=tbool
end

proc tx_callproc (unit p,a,pargs,int t)=
!deal with both callproc and callfn (perhaps calldll too)
	unit q
	symbol d,e,pm
	[maxparams]symbol paramlist
	[maxparams]unit arglist,newarglist
	int nparams,i,j,k,nargs,m,kwdused,qm, ismproc
	ichar name

	tpass(a)

	nargs:=nparams:=0
	ismproc:=0

	retry:

	case a.tag
	when jname then
		d:=a.def

		if d.nameid in [procid, dllprocid] then
			ismproc:=d.nameid=procid
getparams:
			e:=d.deflist
			while e do
				if e.nameid=paramid then
					if nparams>=maxparams then txerror("Param overflow") fi
					paramlist[++nparams]:=e
				fi
				e:=e.nextdef
			od

		else					!assume fn ptr
			while ttbasetype[a.mode]=tref do
				insertunit(a,jptr)
				a.mode:=tttarget[a.mode]
			od
			goto dorefproc
		fi

	when jif,jselect,jblock then
		TXERROR("Can't do ifx/func")

	else
	dorefproc:
		if a.tag=jdot then
			tmethodcall(p,a,pargs)
			a:=p.a
			pargs:=p.b
			goto retry
		fi

		if ttbasetype[a.mode]<>tproc then
			txerror("Function pointer expected")
		fi

		d:=ttnamedef[a.mode]

		if d=nil then txerror("Function expected") fi
		goto getparams
	esac

	q:=pargs
	while q do
		if nargs>=maxparams then txerror("Param overflow") fi
		arglist[++nargs]:=q
		q:=q.nextunit
	od

	p.mode:=d.mode				!type returned by func (will be void for procs)

	if p.mode and t<>tvoid then
		twiden(p,nolv)
	fi

	if d.varparams then
		for i to nargs do

			if i<=nparams then
				tpass(arglist[i],paramlist[i].mode)
			else
				tpass(arglist[i])
			fi
		od
		return

	fi

!I have formal params in paramlist, and actual args in arglist
!Now create new set of args in arglist, which maps any keyword parameters,
!while missing args (represented as nullunit) replaced with nil

!Create new set of actual parameters in params(), with optional/default values filled in
!and any conversions applied
	k:=0
	kwdused:=0
	for i to nparams do
		newarglist[i]:=nil
	od

	for i to nargs do
		q:=arglist[i]
		case q.tag
		when jkeyword then
			name:=q.a.def.name
			for j to nparams do
				if eqstring(paramlist[j].name,name) then
					exit
				fi
			else
				txerror_s("Can't find kwd param: #",name)
			od

			if newarglist[j] then
				txerror_s("Kwd: # already used or was implicit",name)
			fi
			newarglist[j]:=q.b
			kwdused:=1

		else
!doregparam:
			if kwdused then
				txerror("Normal param follows kwd")
			fi
			if k>=nparams then
				cpl =k, =nparams
				txerror("Too many params supplied")
			fi
			newarglist[++k]:=(q.tag=jnull|nil|q)
		esac
	od

!scan params, and fill in optional/default params as needed

	for i to nparams do
		q:=newarglist[i]			!will be nil of not supplied
		pm:=paramlist[i]			!formal param (an st entry)
		if q=nil then
			unless pm.optional then
				txerror_s("Param not optional: #",strint(i))
			end
			if pm.code then		!provide default value
				newarglist[i]:=duplunit(pm.code,p.pos)
			else
				newarglist[i]:=createconstunit(0,ti64)
			fi
		fi
	od

!final pass: do type-pass on each param, and apply any conversion
!I also need to build a new argument list for the call unit
	unit ulist:=nil, ulistx:=nil

	for i to nparams do
		pm:=paramlist[i]
		q:=newarglist[i]

		if pm.parammode=byref_param then
			tpass(q,m:=tttarget[pm.mode],needlv)
			qm:=q.mode

			if not comparemodes(qm,m) then
				txerror_ss("&param: type mismatch",strmode(qm), strmode(m))
			fi

!			UNLESS CTARGET AND Q.TAG=JCONVERT THEN

				insertunit(q,jaddrof)
				q.mode:=pm.mode
!			ELSE
!				Q.TAG:=JADDROF
!				Q.MODE:=PM.MODE
!			END

		else
			tpass(q,pm.mode)
		fi

		addlistunit(ulist, ulistx, q)
		q.nextunit:=nil
	od
	p.b:=ulist
end

proc tx_unary(unit p,a)=
	int opc,size,amode,mbase,tmax,x,xhigh, resmode

	tpass(a)

	amode:=a.mode
	resmode:=amode

	case p.pclop
	when katan, klog, klog10, kexp, ksqrt,ksin,kcos,ktan, kasin, kacos,
			kfloor, kceil then
		coerceunit(a,tr64)
		resmode:=tr64

	when kneg, kabs, ksqr then
		txerror("not num") when not isnum(amode)

	when kbitnot, knot, ktoboolt then
		txerror("toboolt") when not isint(amode)

!	when ksliceptr then
!		tx_sliceptr(p,a)
!		return
	when ksign then
		resmode:=ti64

	ELSE
		CPL "TTT", PCLNAMES[P.PCLOP]
	esac

	p.mode:=resmode
end

proc tx_prop(unit p,a)=
	int opc,size,amode,mbase,tmax,x,xhigh, resmode

	tpass(a)

	amode:=a.mode
	resmode:=amode

	case p.propcode
	when kklwb, kkupb, kklen, kkbounds, kksliceptr then
		do_bounds(p,a)
		return

	when kkbytesize,kkbitwidth then
		size:=ttsize[(a.tag=jtypeconst|a.value|a.mode)]*(p.propcode=kkbytesize|1|8)
		makenewconst(p,size)
		resmode:=ti64

	when kkminval, kkmaxval then
		resmode:=ti64
		if a.tag=jtypeconst then
			mbase:=ttbasetype[a.value]
		else
			mbase:=ttbasetype[a.mode]
		fi

		if p.propcode=kkminval then
			case mbase
			when ti8 then x:=-128
			when ti16 then x:=-32768
			when ti32 then x:=-2_147_483_648
			when ti64 then x:=i64.min
			when tu8,tu16,tu32,tu64,tc8,tc64 then x:=0
			else
 	           txerror_s("Can't do minvalue on #",strmode(mbase))
			esac
		else
			case mbase
			when ti8 then x:=127
			when ti16 then x:=32767
			when ti32 then x:=2_147_483_647
			when ti64 then x:=0x7fff'ffff'ffff'ffff
			when tu8,tc8 then x:=255
			when tu16 then x:=65535
			when tu32 then x:=4294967295
			when tu64 then x:=0; --x; resmode:=tu64
			else
				txerror_s("Can't do maxvalue on #",strmode(mbase))
			esac
		fi
		p.tag:=jconst
		p.a:=nil
		p.value:=x
		p.isconst:=1

	when kktypestr then
		p.tag:=jconst
		if a.tag=jtypeconst then
			amode:=a.value
		else
			amode:=a.mode
		fi

		p.mode:=trefchar
		p.svalue:=pcm_copyheapstring(strmode(amode))
		p.isastring:=1
		p.length:=strlen(p.svalue)
		return

	ELSE
		CPL "PROP", PCLNAMES[P.PCLOP]
	esac

	p.mode:=resmode
end

proc tx_unaryto(unit p,a)=
	tpasslv(a)

	case p.pclop
	when kbitnotto, knotto, ktoboolto then
		txerror("Not int") when not isint(a.mode)
	esac

	p.mode:=tvoid
end

proc tx_if(unit p,pcond,plist,pelse, int t,lv) =
	unit pc:=pcond, pl:=plist
	int u

	u:=tvoid
	if t<>tany then u:=t fi

	while pc, (pc:=pc.nextunit; pl:=pl.nextunit) do
		tpass(pc)
		tpass(pl,t,lv)

		if t=tany then
			if u=tvoid then
				u:=pl.mode
			elsif lv then
				if not comparemodes(u, pl.mode) then
					txerror("IF/LV?")
				fi
			else
				u:=getdominantmode(u,pl.mode)
			fi
		fi
	od

	if t<>tvoid and pelse=nil then
		txerror("else needed")
	fi
	tpass(pelse,t,lv)
	if t=tany then
		if lv then
			if not comparemodes(u, pelse.mode) then
				txerror("IF/LV2?")
			else
				u:=getdominantmode(u,pelse.mode)
			fi
		fi
	fi

	if t<>tvoid then
		pl:=plist
		while pl, pl:=pl.nextunit do
			if t=tany then
				coerceunit(pl,u)
			fi
		od
		if t=tany then
			coerceunit(pelse,u)
		fi
		p.mode:=u
	fi

	if pcond.nextunit=plist.nextunit=nil then
		if iscondtrue(pcond) then		!branch b only
			deleteunit(p,plist)
		elsif iscondfalse(pcond) then	!branch c only
			if pelse=nil then
				pelse:=createunit0(jblock)
			fi
			deleteunit(p,pelse)
		fi
	fi
end

proc tx_incrto(unit p,a,int t)=
	tpasslv(a)

	if t<>tvoid then
		case p.pclop
		when kincrto then p.pclop:=kincrload
		when kdecrto then p.pclop:=kdecrload
		esac
		p.mode:=a.mode
	else				!a++ a-- to ++a --a
		case p.pclop
		when kloadincr then p.pclop:=kincrto
		when kloaddecr then p.pclop:=kdecrto
		esac
		p.mode:=tvoid
	fi

	twiden(p,0)
end

proc tx_for(unit pindex,pfrom,pbody)=
	unit pto, pstep, plocal, plist
	int u

	pto:=pfrom.nextunit
	pstep:=pto.nextunit

	tpass(pindex)
	if pindex.tag<>jname then
		txerror("Loop index not a variable")
	fi
	u:=pindex.mode
	tpass(pindex.nextunit)

	tpass(pfrom,u)
	tpass(pto,u)

	tpass(pstep,u)

	tpass(pbody,tvoid)
	tpass(pbody.nextunit,tvoid)	!optional else
end

proc tx_forall(unit pindex,plist,pbody)=
	unit plocal,pfrom,pto,passign
	int u,mlist,elemtype

	plocal:=pindex.nextunit
	pfrom:=plocal.nextunit
	pto:=pfrom.nextunit
	passign:=plist.nextunit

	tpass(pindex,ti64)
	tpass(pfrom,ti64)
	tpass(pto,ti64)

	tpass(plist)
	mlist:=plist.mode

	case ttbasetype[mlist]
	when tarray then
		elemtype:=tttarget[mlist]
	when tslice then
		elemtype:=tttarget[mlist]
	else
		txerror("forall/can't iterate")
	esac

	tpass(plocal)
	if plocal.mode=tany then
		plocal.mode:=elemtype
		plocal.def.mode:=elemtype
	fi

	tpass(passign)

	tpass(pbody,tvoid)
	tpass(pbody.nextunit,tvoid)	!optional else
end

proc tx_index(unit p,a,b,int t,lv) =
!p is an index unit
!a is an array, b is an index
!t is the needed type for the element being indexed
	int amode,emode,pmode,tmode,tbasemode

	tpass(a,,lv)
	deref(a,t<>tvoid)
	amode:=a.mode

	tpass(b,ti64)			!index

	if ttbasetype[amode] not in [tarray, tslice] then
		txerror_s("Can't index: #",strmode(amode))
	fi
	p.mode:=tttarget[amode]
	twiden(p,lv)
end

proc tx_makerange(unit p,a,b)=
	int amode,bmode

	tpass(a,ti64)
	tpass(b,ti64)

	amode:=a.mode
	bmode:=b.mode

	coerceunit(a,ti64)
	coerceunit(b,ti64)
	p.mode:=trange
end

proc tx_ptr(unit p,a,int t,lv)=
	symbol d

	tpass(a)

	case ttbasetype[a.mode]
	when tvoid then
		txerror("Deref Void")
	when tref then
		p.mode:=tttarget[a.mode]

	when tslice then
		CPL "DEREF SLICE"
	else
		txerror("PTR: need ref T")
	esac

	twiden(p,lv)
end

proc setrecordsize(int m)=
	[maxfields+8]symbol fieldlist
	int i,nfields,indent,nrfields,size,index, maxalign
	symbol d,e
	ref char flags
	const ss='S', ee='E'
	int flag
	static int depth


	if ttsize[m] then return fi
	if ++depth>10 then serror("Recursive record?") fi

	d:=ttnamedef[m]
	e:=d.deflist
	nfields:=0

	fieldlist[++nfields]:=symbol(ss)

	while e do
		if e.nameid=fieldid then
			if nfields>=maxfields then
				gerror("srs:too many fields")
			fi

			setmodesize(e.mode)
			flags:=cast(&e.uflags)
			docase flags^
			when 'S', 'U' then
				flag:=flags^
				fieldlist[++nfields]:=symbol(flag)
				++flags
			else
				exit
			end docase

			fieldlist[++nfields]:=e

			do
				flag:=flags++^
				case flag
				when '*'  then
				when 'E' then
					fieldlist[++nfields]:=symbol(ee)
				else
					exit
				esac
			od
		fi

		e:=e.nextdef
	od

	fieldlist[++nfields]:=symbol(ee)
	fieldlist[nfields+1]:=nil			!terminator

	countedfields:=0
	index:=2
	maxalign:=1
	scanrecord('S',&fieldlist,index,size,0, d.align, maxalign)

	if d.align then
		size:=roundoffset(size,maxalign)
		d.maxalign:=maxalign
	else
		d.maxalign:=1
	fi

	ttsize[m]:=size
	ttlength[m]:=countedfields
	ttlower[m]:=1

	checkblocktype(m)
	--depth
end

proc checkblocktype(int m)=
	case ttsize[m]
	when 1,2,4,8 then
		ttisblock[m]:=0
	esac
end

proc scanrecord(int state,ref[]symbol fields, int &index, &isize, offset, calign, &maxalign)=
 	symbol e,f,ea
	int size:=0,fieldsize,bitoffset:=0, alignment, newoffset

	while f:=fields^[index++] do
		case int(f)
		when 'S','U' then
			scanrecord(int(f),fields, index,fieldsize, offset, calign, maxalign)
		when 'E' then			!end of this nested block
			if state='U' then ++countedfields fi
			isize:=size
			return
		else
			if f.mode=tbitfield then
				fieldsize:=0
				ea:=f.equivfield
				f.offset:=ea.offset
				f.bitoffset:=bitoffset
				bitoffset+:=f.bitfieldwidth
				if bitoffset>ttsize[f.equivfield.mode]*8 then
					txerror("Bit fields overflow type")
				fi

			elsif f.atfield then
				bitoffset:=0
				e:=f.equivfield
				fieldsize:=0
				f.offset:=e.offset+f.equivoffset
			else
				bitoffset:=0
				if state='S' then ++countedfields fi
				fieldsize:=ttsize[f.mode]
				if calign then
					alignment:=getalignment(f.mode)
!CPL "CALIGN", =FIELDSIZE, =ALIGNMENT, =MAXALIGN, =STRMODE(F.MODE), =TTSIZE[F.MODE]
					if alignment>maxalign then maxalign:=alignment fi
					newoffset:=roundoffset(offset,alignment)
					size+:=newoffset-offset
				else
					newoffset:=offset
				fi
				f.offset:=newoffset
				offset:=newoffset
			fi
		esac
		if state='S' then
			offset+:=fieldsize
			size+:=fieldsize
		else
			size:=max(size,fieldsize)
		fi
	od
end

func roundoffset(int offset, alignment)int=
	int mask

	if alignment=1 then return offset fi
	mask:=alignment-1
	while offset iand mask do ++offset od

	return offset
end

proc tx_convert(unit p,a,int hard=0)=
	case a.tag
	when jmakelist then
		tx_makelist(a,a.a,p.convmode,nolv)
	else
!CPL "TX CONVERT"
		tpass(a)
		coerceunit(a,p.convmode,hard)
!!NEW:
!		tpass(a, p.convmode, hard:hard)
!!		coerceunit(a,p.convmode,hard)
	esac
	deleteunit(p,a)			!get rid of this convert (may be replaced by new convert unit)
end

proc tx_makelist(unit p,a, int t,lv)=
	int alength,tlength,elemtype,newt, i, nfields,isconst, m
	unit q,b
	symbol e

	alength:=p.length
	newt:=0
	isconst:=1

	tlength:=ttlength[t]

	if tlength then
		if alength<tlength then
			txerror_ss("Too few elements",strint(alength), strint(tlength))
		elsif alength>tlength then
			txerror_ss("Too many elements",strint(alength), strint(tlength))
		fi
	fi

	case ttbasetype[t]
	when tarray then
		elemtype:=tttarget[t]
		if tlength=0 then
			newt:=createarraymodek(nil, elemtype, ttlower[t],alength,0)
		else
			newt:=t
		fi
		q:=a
		while q do
			tpass(q,elemtype,lv)

			unless q.tag=jconst then isconst:=0 end
			q:=q.nextunit
		od

		p.mode:=newt

	when trecord then
		e:=ttnamedef[t].deflist
		q:=a
		while q and e do
			if e.nameid=fieldid then 
				while e.mode=tbitfield do
					e:=e.nextdef
					if not e then exit fi
				od

				tpass(q,e.mode,lv)

				unless q.tag=jconst then isconst:=0 end
				q:=q.nextunit
			fi

			e:=e.nextdef
		od
		while e and (e.nameid<>fieldid or e.mode=tbitfield) do
			e:=e.nextdef
		od
		if q or e then
			txerror("Can't initialise unions")
		fi
		p.mode:=t
		p.resultflag:=1

	when tslice then
CPL "TSLICE"

	else
		txerror_s("Unknown makelist type: #",strmode(t))
	esac

	p.isconst:=isconst

	tpass(p.b,ti64)				!lower


IF P.TAG<>JMAKESLICE THEN

	if not inidata and isconst then
		e:=getavname(currproc,staticid)
		e.mode:=t
		addstatic(e)
		q:=createunit0(jnone)
		q^:=p^
		e.code:=q
		p.tag:=jname
		p.def:=e
	fi
FI
end

proc tx_makeslicefromlist(unit p,a, int t)=
	CPL "MAKESLICE/TX"

	TXERROR("MAKESLICE FROM LIST NOT READY")
end

proc tx_makeslice(unit p, a,b, int t)=
	CPL "MAKESLICE/TX"
	tpass(a)

	if ttbasetype[a.mode]<>tref then txerror("slice init not ref") fi
	if tttarget[a.mode]<>tvoid then
		if not comparemodes(a.mode,createrefmode(nil,tTtarget[t])) then
			txerror("slice/ptr mismatch")
		fi
	fi

	tpass(b,ti64)
	p.mode:=t
CPL "MKSLICE2"
	p.resultflag:=1
end

proc tx_makeset(unit p,a, int t)=
	p.isconst:=1

	while a, a:=a.nextunit do
		tpass(a)

		if not a.isconst then
			p.isconst:=0
		fi
	od

	p.mode:=tvoid
end

proc tx_dot(unit p,a,b,int lv)=
	int recmode,recbasemode,i,j,newtag,tmode
	unit q,pindex
	symbol d,dequiv

	tpass(a)			!lhs, yeields ref array type

	recmode:=a.mode

	recbasemode:=ttbasetype[recmode]

	while recbasemode=tref do
		tmode:=tttarget[recmode]
		insertunit(a,jptr)
		recmode:=a.mode:=tmode
		recbasemode:=ttbasetype[recmode]
	od

	if ttbasetype[recmode]<>trecord then
		txerror("Bad record type")
	fi

	d:=b.def

	if d.nameid=nullid then			!not resolved; lhs mode wasn't available
		d:=b.def:=resolvefield(d,recmode)
	fi

	if d.mode=tbitfield then
		i:=d.bitoffset
		j:=i+d.bitfieldwidth-1
		dequiv:=d.equivfield

		b.def:=dequiv				!change from bitfield field to containing int
		b.mode:=dequiv.mode
		p.offset:=d.offset

		if i=j then					!single bit
			pindex:=createconstunit(i,ti64)
			newtag:=jdotindex
		else						!bit slice
			pindex:=createunit2(jmakerange,createconstunit(i,ti64),createconstunit(j,ti64))
			pindex.mode:=trange
			pindex.a.resultflag:=1
			pindex.b.resultflag:=1
			newtag:=jdotslice
		fi

		p.mode:=b.mode
		twiden(p,lv)
		insertunit(p,newtag)
		p.mode:=tu64
		p.b:=pindex
		p.a.resultflag:=1
		p.b.resultflag:=1
		p.resultflag:=1

		return

	fi

	b.mode:=d.mode
	p.mode:=d.mode

	p.offset:=d.offset
	twiden(p,lv)
end

func resolvefield(symbol d, int m)symbol=
	symbol e,t

	case ttbasetype[m]
	when trecord then
	when tref then
		m:=tttarget[m]
		if ttbasetype[m]<>trecord then
			txerror("3:record expected")
		fi
	else
		txerror("4:record expected")
	esac
	t:=ttnamedef[m]

	e:=finddupl(t,d)
	if not e then
		txerror_s("Not a field: #",d.name)
	fi
	return e
end

proc tx_andl(unit p,a,b)=
	tpass(a,tbool)
	tpass(b,tbool)

	p.mode:=tbool
end

proc convintconst(unit p,i64 x)=
!convert unit p into int const x
	p.tag:=jconst
	p.mode:=ti64
	p.a:=p.b:=p.c:=nil
	p.value:=x
	p.isconst:=1
end

proc tx_sliceptr(unit p,a)=
	int m,tmode

	tpass(a)
	m:=a.mode

	case ttbasetype[m]
	when tslice then
	else
		txerror_s("SLICEPTR #",strmode(m))
	esac

!for when ptr is to be pointer to the array
	tmode:=createarraymodek(nil, tttarget[m], ttlower[m],0,0)

!for when ptr is to be pointer to the array element (although either can be
!cast to the other); although try alternate .sliceptr versions too
!tmode:=tttarget[m]

	p.mode:=createrefmode(nil,tmode)
end

proc tx_swap(unit p,a,b)=
	int av, bv

	tpasslv(a)
	tpasslv(b)

	if not comparemodes(a.mode,b.mode) then
		txerror("SWAP: type mismatch")
	fi

	p.mode:=tvoid
end

proc tx_select(unit p,a,b,c, int t,lv)=
	int i,u
	unit q

	tpass(a,ti64)

	q:=b
	while q do
		tpass(q,t,lv)
		if q=b then
			u:=q.mode
		else
			u:=getdominantmode(u,q.mode)
		fi

		q:=q.nextunit
	od

	tpass(c,t,lv)
	u:=getdominantmode(u,c.mode)

	q:=b
	while q do
		coerceunit(q,u)
		q:=q.nextunit
	od

	p.mode:=u
end

proc tx_case(unit p,a,b,c, int t,lv)=
	int amode,u
	unit wt,w

	if p.tag=jdocase and lv then gerror("&docase") fi

	tpass(a)

	if a=nil then
		amode:=tany
	else
		amode:=a.mode
	fi

	if ttisinteger[amode] and ttsize[amode]<8 then
		coerceunit(a,tint)
		amode:=tint
	fi
	u:=tvoid

	wt:=b
	while wt do				!whenthen chain
		w:=wt.a
		while w do				!each expr between when...then
			tpass(w)
			if w.tag=jmakerange then
				unless ttisinteger[amode] then txerror("case: need int index") end
			else
				if amode=tany then
						if not isbooltag[w.tag] then
							TXERROR("CASE/BOOL?")
							insertunit(w,jistruel)
						fi
				else
					coerceunit(w,amode)
				fi
			fi
			w:=w.nextunit
		od
		tpass(wt.b,t,lv)			!process block
		if t<>tvoid then
			if u then
				u:=getdominantmode(u,wt.b.mode)
			else
				u:=wt.b.mode
			fi
		fi
		wt:=wt.nextunit
	od

	if c then
		tpass(c,t,lv)
		if t=tany then
			u:=getdominantmode(u,c.mode)
		fi
	elsif t<>tvoid then
		txerror("case needs else")
	fi

	if t<>tvoid then
		p.mode:=u
	else
		p.mode:=tvoid
	fi

end

proc tx_notl(unit p,a)=
	tpass(a)
	p.mode:=tbool
end

proc tx_typepun(unit p,a)=
	int smode,tmode
	case a.tag
	when jmakelist then
		TXERROR("TYPEPUN/LIST")
	else
		tpass(a)

		smode:=ttbasetype[a.mode]
		tmode:=ttbasetype[p.convmode]

		unless ttisreal[smode] and ttisinteger[tmode] or
			ttisinteger[smode] and ttisreal[tmode] then
			txerror("Invalid type-punning; only real<->int")
		end
		IF TMODE IN [TI32, TU32] THEN TMODE:=TI64 FI
		p.mode:=tmode
	esac
end

proc tx_exit(unit p,a)=
	if a=nil then return fi
	tpass(a,ti64)
	if a.tag<>jconst then
		txerror("exit/etc not const")
	fi
	p.index:=a.value
	p.a:=nil
end

proc tx_goto(unit p,a)=
	int m

	tpass(a)
	m:=a.mode

	if ttbasetype[m]<>tref or ttbasetype[tttarget[m]]<>tlabel then
		txerror("goto: not label")
	fi
end

proc tx_switch(unit p,a,b,c,int t,lv)=
	[0:2048]byte valueset
	unit wt, w
	int ax,bx,i,u

	if p.tag=jdoswitch and lv then gerror("&doswitch") fi

	if p.tag=jdoswitchx then
		tpass(a)
		tpass(a.nextunit)
		if ttbasetype[a.mode]<>tref then txerror("not ref") fi
	else
		tpass(a,ti64)
	fi

	memset(&valueset,0,valueset.bytes)
	u:=tvoid

	wt:=b
	while wt do

		w:=wt.a
		while w do
			tpass(w)

			if not isconstunit(w) then
				PRINTUNIT(W)
				txerror("Switch not constant")
			fi

			case ttbasetype[w.mode]
			when trange then			!assume makerange
				ax:=w.a.value
				bx:=w.b.value
	dorange:
				for i:=ax to bx do
					if i<valueset.lwb or i>valueset.upb then
						txerror("switch: value out of range")
					fi
					if valueset[i] then
						cpl i
						txerror("Duplicate switch value")
					fi
					valueset[i]:=1
				od
			else
				coerceunit(w,ti64,0)
				tevaluate(w)
				if w.tag<>jconst then
					txerror("Switch value: not const int")
				fi
				ax:=bx:=w.value
				goto dorange
			esac
			w:=w.nextunit
		od
		tpass(wt.b,t,lv)

		if t=tany then
			if u then
				u:=getdominantmode(u,wt.b.mode)
			else
				u:=wt.b.mode
			fi
		fi

		wt:=wt.nextunit
	od

	if c then
		tpass(c,t,lv)
		if t=tany then
			u:=getdominantmode(u,c.mode)
		fi
	elsif t<>tvoid then
		txerror("switch needs else")
	fi

	if t<>tvoid then
		w:=b.a
		while w do				!all elseif unots
			if t=tany then
				coerceunit(b.b,u)
			fi
			w.mode:=b.b.mode
			w:=w.nextunit
		od
		if t=tany then
			coerceunit(c,u)
			p.mode:=u
		else
			p.mode:=t
		fi
	else
		p.mode:=tvoid
	fi
end

proc tx_addroffirst(unit p,a,int t)=
!&.x maps to &x[x.lwb]
	int m

	tpass(a)

	m:=a.mode
	if ttbasetype[m]<>tarray then
		txerror("&. ref[] expected")
	fi

	m:=createrefmode(nil,tttarget[m])
	if a.tag=jname then
		a.addroffirst:=1
	fi
	p.mode:=m
end

proc tx_return(unit p,a, int t)=
 	int m,nvalues,nret,i
	ref[]i32 pmult
	unit q

	m:=currproc.mode
	nret:=currproc.nretvalues
	pmult:=ttmult[currproc.mode]

	if a=nil then
		if nret then
			txerror("return value(s) missing")
		fi
		return
	elsif nret=0 then
		txerror("Superfluous return value")
	fi

	if a.tag=jmakelist then
		a.tag:=jreturnmult
		if a.length<>nret then
			case ttbasetype[m]
			when trecord, tarray then
				txerror("return constructor not supported")
			else
				txerror("Wrong number of return values")
			esac
		fi
		q:=a.a				!point to list of return values
		for i to nret do
			tpass(q,pmult[i])
			q:=q.nextunit
		od

		deleteunit(p,a)			!don't need return
		if t=tvoid then
			p.mode:=tvoid
		else
			p.mode:=ttuple
		fi

	else
		if nret>1 then txerror("RETERROR?") fi
		tpass(a,m)

		if t=tvoid then					!regular out-of-line return
			p.mode:=tvoid
		else
			deleteunit(p,a)
!			P.MODE:=A.MODE
		fi
	fi

	IF TTISSHORT[P.MODE] THEN TXERROR("SHORT RET TYPE") FI
end

proc tx_dotindex(unit p,a,b,int lv) =
!a.[b], a is an int
	int pmode
	unit i,j

	tpass(a,,lv)			!lhs

	pmode:=tu64

	if not ttisinteger[a.mode] then
		if ttisreal[a.mode] then
			insertunit(a,jtypepun)
			a.mode:=a.convmode:=tu64
			a.resultflag:=1

		else
			txerror("a.[i]: not int/str value")
		fi
	fi

	tpass(b)			!index

	case ttbasetype[b.mode]
	when trange then
		i:=b.a
		j:=b.b
		if i.tag=j.tag=jconst then
			if i.value>j.value then
				swap(b.a,b.b)
			fi
		fi
	else					!assume simple index
		coerceunit(b,ti64)
	esac

	p.mode:=pmode
end

proc tx_slice(unit p,a,b) =
!a[b], b is a rtange

	tpass(a)			!lhs
	tpass(b)			!will be a range

	if a.mode=trefchar then
		p.mode:=createslicemodek(currproc,tc8,1,0)
	else
		deref(a)
		case ttbasetype[a.mode]
		when tarray then
			p.mode:=createslicemodek(currproc,tttarget[a.mode],1, 0)

		when tslice then
			p.mode:=a.mode

		else
			CPL =STRMODE(A.MODE)
			txerror("a[i..j]: not array")
		esac
	fi
end

proc twiden(unit p, int lv)=
!intended for widening narrow types for memory access nodes Name, Index, Dot, Ptr.
!But will also be used to generally apply
	int m,u,mbase

	mbase:=ttbasetype[m:=p.mode]

	if mbase=tvoid then return fi		!nothing to widen (error?)
	if lv then return fi				!lv, keep memory mode as dest

	if not ttisshort[mbase] then return fi	!no widening needed

	case p.tag
	when jname, jptr, jindex, jdot, jcall, jincr then
		insertunit(p, jconvert)
		p.convcode:=kkwiden
		p.convmode:=m
		p.mode:=gettypebase(m)
	else
		PRINTUNIT(P)
		txerror_s("widen? #",jtagnames[p.tag])
	esac
end

proc tstringslice(unit p, int slicemode)=
!p is a string; insert conversions to turn it into a slice:
	unit a,b,prange
	int length

	if tttarget[slicemode]<>tc8 then
		txerror("Not char slice")
	fi
!
	a:=p
	insertunit(p,jslice)


	if p.a.tag=jconst then
	else
		b:=duplunit(p.a)
		insertunit(b,junary)
		prange:=createunit2(jmakerange,createconstunit(1,ti64),b)

		prange.mode:=trange
		p.b:=prange
	fi

	p.mode:=slicemode
end

proc tx_bitfield(unit p,a,int lv)=
	int i,j,bitsize,topbit
	unit r

	tpass(a,,lv)

	if not ttisinteger[a.mode] and not ttisref[a.mode] then
		if ttisreal[a.mode] then
			insertunit(a,jtypepun)
			a.mode:=a.convmode:=tu64
			a.resultflag:=1
		else
			txerror("Int/ref needed")
		fi
	fi

	bitsize:=ttsize[ttbasetype[a.mode]]*8
	topbit:=bitsize-1

	case p.bfcode
	when bf_lsb then
		i:=0; j:=7

	when bf_msb then
		j:=topbit
		i:=topbit-7

	when bf_lsbit then
		i:=j:=0

	when bf_odd,bf_even then
		if lv then
			txerror("Can't assign")
		fi
		i:=j:=0

	when bf_msbit then
		i:=j:=topbit

	when bf_lsw then
		i:=0
		j:=bitsize/2-1

	when bf_msw then
		i:=bitsize/2
		j:=topbit
	else
		CPL P.BFCODE
		TXERROR("BITFIELD")
	esac

	if i=j then			!single bit
		p.tag:=jdotindex
		p.b:=createconstunit(i,ti64)
		p.resultflag:=1
		p.b.resultflag:=1

		if p.bitopindex=bf_even then
			p.mode:=tu64
			addnotl(p)
		fi

	else
		r:=createunit2(jmakerange,createconstunit(i,ti64),createconstunit(j,ti64))
		r.a.resultflag:=1
		r.b.resultflag:=1
		r.mode:=trange
		p.tag:=jdotslice
		p.b:=r
	fi

	p.mode:=tu64
end

proc deref(unit a, int needres=1)=
!a is a unit that needs to be dereferenced because it's about to used as:
! a[i]
! a[i..j]
! a.lwb, a.upb, a.len
!Ie in an array context
	int abasemode, tmode

	abasemode:=ttbasetype[a.mode]

	while abasemode=tref do
		tmode:=tttarget[a.mode]

		insertunit(a,jptr)
		a.mode:=tmode

		abasemode:=ttbasetype[a.mode]
	od

end

proc tmethodcall(unit p, pdot, pargs)=
	int mrec
	unit prec, pfield, pfunc
	symbol d,e

	prec:=pdot.a
	pfield:=pdot.b
	mrec:=prec.mode
	d:=pfield.def

	e:=resolvefield(d,mrec)

	if e=nil then
		txerror_s("Can't resolve method:",d.name)
	fi

	pfunc:=createname(e)
	pfunc.mode:=e.mode
	prec.nextunit:=pargs

	p.a:=pfunc
	p.b:=prec
end

proc do_bounds(unit p,a) =
	int m,mbase,opc,lower,upper

	deref(a)

	m:=a.mode
	if a.tag=jtypeconst then m:=a.value fi

	mbase:=ttbasetype[m]
	p.mode:=ti64

	case p.pclop
	when kklwb then
		case mbase
		when tarray,tslice then
			convintconst(p,ttlower[m])
			return
		else
error:
			txerror_s("lwb/upb/len?",strmode(m))
		esac

	when kkupb then
		case mbase
		when tarray then
			convintconst(p,ttlower[m]+ttlength[m]-1)
		when tslice then
			p.tag:=junary			!code gen needs to look at type, and use .propcode
		else
			goto error
		esac

	when kklen then
		case mbase
		when tarray then
			convintconst(p,ttlength[m])
		when tslice then
			p.tag:=junary
!			p.pclop:=klen
		else
			goto error
		esac
	when kkbounds then
		p.mode:=trange
		case mbase
		when tarray then
			p.range_lower:=ttlower[m]
			p.range_upper:=p.range_lower+ttlength[m]-1
			p.tag:=jconst
			p.a:=p.b:=p.c:=nil
			p.isconst:=1
			return

		when tslice then
		else
			goto error
		esac
	when kksliceptr then
		if mbase<>tslice then txerror("Not slice") fi
		p.tag:=junary

	esac
end

proc addnotl(unit p)=
	insertunit(p,jnotl)
	p.mode:=tbool
	p.pclop:=knot
end

proc tevaluate(unit p)=
	unit a,b,pname
	int offset

	int tag:=p.tag

	if jisexpr[tag]=2 then
		tevalbinop(p)

	elsif jisexpr[tag]=1 then
		tevalmonop(p)

	elsecase tag
	when jmakerange then
		a:=p.a
		b:=p.b
		if ttsize[a.mode]<=8 then			!const range only for 32-bits
			tevaluate(a)
			tevaluate(b)
			if a.tag=jconst and b.tag=jconst then
				p.isconst:=a.isconst iand b.isconst
			fi
		fi

	when jaddrof then
!		IF NOT CTARGET THEN
			a:=p.a

			pname:=addrdotindex(a, offset)

			if pname then
				deleteunit(a,pname)
				if p.b=nil then
					p.b:=createconstunit(offset,ti64)
				else 
					p.b.value+:=offset
				fi
			fi
!		FI
	fi

end

func addrdotindex(unit p, int &offset)unit q=
	int axmode

	case p.tag
	when jdot then
		if p.a.tag=jname then
			offset:=p.offset
			return p.a
		else
			q:=addrdotindex(p.a,offset)
			offset+:=p.offset
			return q
		fi
	when jindex then
		axmode:=p.a.mode
		if p.b.tag=jconst then
			if p.a.tag=jname then
				offset:=(p.b.value-ttlower[axmode])*ttsize[tttarget[axmode]]
				return p.a
			else
				q:=addrdotindex(p.a,offset)
				if q then
					offset+:=(p.b.value-ttlower[axmode])*ttsize[tttarget[axmode]]
				fi
				return q
			fi
		else
			return nil
		fi
	else
		return nil
	esac

end

proc tevalbinop(unit p)=
	i64 a,b,c,offset
	real x,y,z
	unit lhs, rhs

	lhs:=p.a
	rhs:=p.b

	unless lhs.tag=rhs.tag=jconst then
!		if lhs.tag=jaddrof and rhs.tag=jconst AND P.PCLOP=KADDREFX then		!ASSUME ADD/SUBREFX
		if lhs.tag=jaddrof and rhs.tag=jconst then		!ASSUME ADD/SUBREFX
			if lhs.a.tag=jname then			!reduce addrof(a)+k => addrof(a,k)
				offset:=rhs.value*ttsize[tttarget[lhs.mode]]
				if p.pclop=ksubpx then
					offset:=-offset
				fi
				if lhs.b=nil then
					lhs.b:=createconstunit(offset,ti64)
				else
					lhs.b.value+:=offset
				fi
				deleteunit(p,lhs)
			fi
		fi
		return
	end

	if ttisreal[p.mode] then
		x:=p.a.xvalue
		y:=p.b.xvalue
	else
		a:=p.a.value
		b:=p.b.value
	fi

	case p.mode
	when ti64, tu64 then

		case p.pclop
		when kadd then c:=a+b
		when ksub then c:=a-b
		when kmul then c:=a*b
		when kidiv then
			if b=0 then txerror("x/0") fi
			c:=a/b
		when kirem then
			if b=0 then txerror("x rem 0") fi
			c:=a rem b
		when kshl then c:=a<<b

!		when keq then c:=a=b
!		when kne then c:=a<>b
!		when klt then c:=a<b
!		when kle then c:=a<=b
!		when kge then c:=a>=b
!		when kgt then c:=a>b

		when kbitand then c:=a iand b
		when kbitor then c:=a ior b
		when kpower then c:=a ** b
		else
			return
		end

	when tr64,tr32 then

		case p.pclop
		when kadd then z:=x+y
		when ksub then z:=x-y
		when kmul then z:=x*y
!		when kdiv then z:=x/y
		when kpower then z:=x**y

		else
			return
		end
	else
		return
	esac
!
	if ttisreal[p.mode] then
		makenewconst(p,i64@(z))
	else
		makenewconst(p,c)
	fi
end

proc tevalmonop(unit p)=
	i64 a,b,c
	real x,z

	unless p.a.tag=jconst then
		return
	end

	a:=p.a.value
	x:=p.a.xvalue

	case p.mode
	when ti64, tu64 then

		if p.tag in [jistruel, jisfalsel] then dobool fi

		case p.pclop
		when kneg then c:=-a

!		when ktoboolt then
!
!CPL "EVALMONO/XXTOBOOLT1"
!
! c:=istrue a; p.mode:=tbool
		when knot then c:=not a; p.mode:=tbool
		when kbitnot then c:=inot a
		when kabs then c:=abs a

		else
			return
		esac
	when tr64, tr32 then
		case p.pclop
		when kneg then z:=-x
		when katan then z:=atan(x)
		when ksqrt then z:=sqrt(x)

		else
			return
		esac

	when tbool then

dobool:
		case p.tag
		when jistruel then c:=istrue a; p.mode:=tbool
		when jisfalsel then c:=not a; p.mode:=tbool
		elsecase p.pclop
		when ktoboolt then c:=istrue a; p.mode:=tbool
		when knot then c:=not a; p.mode:=tbool
		esac
	else
		return
	esac

	if ttisreal[p.mode] then
		makenewconst(p,i64@(z))
	else
		makenewconst(p,c)
	fi
end

func iscondtrue(unit p)int =
	p.tag=jconst and p.value<>0
end

func iscondfalse(unit p)int =
	p.tag=jconst and p.value=0
end

proc fixchararray(unit a)=
!turn []char into ichar at certain points
	if a and ttbasetype[a.mode]=tarray and tttarget[a.mode]=tc8 then
		coerceunit(a,trefchar,0)
	fi
end

proc combinestrings(unit p)=
!p is (add, a, b) where a and b are string constants.
	unit a:=p.a, b:=p.b
	int alen:=a.slength
	int blen:=b.slength
	int clen, needterm
	byte atype:=a.strtype, btype:=b.strtype, ctype
	ichar s

	if atype=btype='B' then
		needterm:=0
		ctype:='B'
	elsif atype='B' or btype='B' then
		txerror("Mixed str+bin strings")
	else					!both are string/strdata
		--alen				!lose zero terminator
		--blen

		needterm:=1
		if atype='S' or btype='S' then		!either strdata then both are
			ctype:='S'
		else
			ctype:=0
		fi
	fi
	clen:=alen+blen

	if blen=0 then
		deleteunit(p,a)
		return
	elsif alen=0 then
		deleteunit(p,b)
		return
	fi

	s:=pcm_alloc(clen+needterm)
	memcpy(s,a.svalue,alen)
	memcpy(s+alen,b.svalue,blen)
	if needterm then
		(s+clen)^:=0
	fi

	deleteunit(p,a)
	p.slength:=clen+needterm
	p.svalue:=s
	p.strtype:=atype
end

proc mulstrings(unit p)=
!p is (add, a, b) where a and b are string constants.
	unit a:=p.a, b:=p.b
	int alen:=a.slength
	int scale:=b.value
	int clen, needterm
	byte atype:=a.strtype, ctype
	ichar s, t

	--alen				!lose zero terminator

	needterm:=1
	if atype='S' then needterm:=1 fi

	clen:=alen*scale
	if scale<1 or clen<1 or clen>100000 or alen<1 then txerror("mulstr") fi

	t:=s:=pcm_alloc(clen+needterm)
	to scale do
		memcpy(t,a.svalue,alen)
		t+:=alen
	od
	if needterm then
		(s+clen)^:=0
	fi

	deleteunit(p,a)
	p.slength:=clen+needterm
	p.svalue:=s
	p.strtype:=atype
end

proc tx_strinclude(unit p,a)=
	int fileno
	ifile pf

	tpass(a)
	if a.tag<>jconst or not a.isastring then
		txerror("strincl/not string")
	fi

!CPL "TX STRINCLUDE", A.SVALUE, CURRPROC.NAME

	fileno:=modules[p.moduleno].fileno

	pf:=getsupportfile(a.svalue,path:sources[fileno].path)

	a.svalue:=pf.text
	a.slength:=pf.size+1
	a.strtype:=p.strtype

	if a.strtype='B' then				!string
		--a.slength						!there will already be zero-terminator
	fi
!
!CPL "DONE STRINCL",A.STRTYPE
	deleteunit(p,a)
end

proc coerceunit(unit p, int t, hard=0)=
	int opc, s:=p.mode, n

	if t=tvoid or s=t then return fi
	if s=tvoid then
		txerror("Void expression/return value missing")
	fi

	if s=t then return fi

	int sbase:=ttbasetype[s]
	int tbase:=ttbasetype[t]

	opc:=kkerror
	int starg:=tttarget[s]
	int ttarg:=tttarget[t]

	if s=trefchar then sbase:=trefchar fi
	if t=trefchar then tbase:=trefchar fi

	if sbase in tfirstnum..tlastnum then
		if tbase in tfirstnum..tlastnum then
			opc:=softconvtable[sbase,tbase]
		elsecase tbase
		when tref, trefchar then
			opc:=kksoftconv
checkhard:
			if not hard then opc:=kkharderr fi
		elsif tbase in tfirstshort..tlastshort then
			if ttisinteger[sbase] then
				if not hard then				!needed for idata init
					opc:=kksofttrun
				else
					opc:=kktruncate
				fi
			fi
		elsecase tbase
		when tbool then
			opc:=kktoboolt
		when ttype then
			opc:=kksoftconv
		fi

	elsecase sbase
	when tbool then
		if tbase in [ti64, tu64] then
			opc:=kksoftconv
		fi

	when tref then
		case tbase
		when ti64, tu64 then
			opc:=kksoftconv
			checkhard
		when tref then
			if starg=tvoid or ttarg=tvoid then			!at least one is ref void
				opc:=kksoftconv
			else
checkref:
				opc:=kksoftconv
				if not comparemodes(s,t) then
					checkhard
				fi
			fi
		when trefchar then
			checkref
		when tbool then
			opc:=kktoboolt
		end

	when trefchar then
		case tbase
		when ti64,tu64 then
			opc:=kksoftconv
			checkhard
		when tref then
			if comparemodes(s,t) or hard then
				opc:=kksoftconv
			else
				opc:=kkharderr
			fi
		when tbool then
			opc:=kktoboolt
		when tslice then
!			if ttarg not in [tc8, tu8] then
			if ttarg in [tc8, tu8] then
				opc:=kkichar2sl
			fi
		when tarray then
			if p.tag=jconst and p.strtype then
				opc:=kksoftconv
				n:=ttlength[t]
				if n=0 then
					ttlength[t]:=p.slength/ttsize[tttarget[p.mode]]
					ttsize[t]:=p.slength
				else
					txerror("Array not empty")
				fi
			fi

		end

	when tarray then
		case tbase
		when tarray then
			if comparemodes(s,t) then
				opc:=kksoftconv
			fi
		when tslice then
			if comparemodes(starg, ttarg) then
				opc:=kkax2slice
			fi

		when trefchar then
			if starg in [tc8, tu8] then
				opc:=kkcx2ichar
			fi
		when tref then
			if ttarg=tvoid then
				opc:=kkcx2ichar
			fi
		esac

	when tslice then
		case tbase
		when tslice then
			if comparemodes(s,t) then
				opc:=kksoftconv
			fi
		when tref then
			if ttarg=tvoid or comparemodes(starg, ttarg) then
GERROR("COERCE/SLICEPTR")
!				opc:=ksliceptr
			fi

		esac

	when ttype then
		if tbase<=tlastnum then
			opc:=kksoftconv

		fi
	fi

	applyconversion(p,s,t,opc)
end

proc applyconversion(unit p, int s,t, opc)=
!deal with conversion op applied to p:
! do nothing
! report error
! insert special node
! attempt compile-time conversion
! insert convert node
! set p's mode etc

	case opc
	when kkerror then
		txerror_ss("Can't do conversion: # => #",strmode(s),strmode2(t))

	when kkharderr then
		txerror_ss("Need explicit cast: # => #",strmode(s),strmode2(t))

	when kksoftconv then
		p.mode:=t
		return
	when kksofttrun then
		if tevalconvert(p,s,t,opc) then
			return
		fi
		insertunit(p,jshorten)
		p.mode:=t			!don't use the short target mode
		return

	when kkax2slice then
		insertunit(p,jslice)
		p.mode:=t
		return
	when kkichar2sl then
		tstringslice(p,t)
		return

	when kkcx2ichar then
		insertunit(p,jaddroffirst)
		p.mode:=trefchar
		return
	esac

	if tevalconvert(p,s,t,opc) then		!try and apply it directly
		return
	fi

!have to add an explict conversion node
	insertunit(p, jconvert)
	p.pclop:=opc

	p.convmode:=s
	p.resultflag:=1

!???
	if ttisshort[t] then
		p.convmode:=t
		t:=gettypebase(t)
	fi

	p.mode:=t
end

proc checkmodes(int s,t)=
	if not comparemodes(s,t) then
		txerror_ss("Type-compare error: # <-> #",strmode(s), strmode2(t))
	fi
end

func comparemodes(int s,t)int=
!return 1 if modes s,t are compatible. That is, ref s/ref t would be interchangeable.
!a direct compare may be false because refs/arrays but be constructed at
!different times
	int sbase, tbase, starg, ttarg
	symbol d,e

	if s=t then return 1 fi

	sbase:=ttbasetype[s]
	tbase:=ttbasetype[t]
	starg:=tttarget[s]
	ttarg:=tttarget[t]

	if sbase=tbase then
		case sbase
		when tref then
			if starg=tvoid or ttarg=tvoid then
				return 1
			fi
			return comparemodes(starg,ttarg)

		when tarray then
			if not comparemodes(starg, ttarg) then return 0 fi
			if ttlength[s]=ttlength[t] or ttlength[s]=0 or ttlength[t]=0 then
				return 1
			fi
		when tslice then
			return comparemodes(starg, ttarg)

		when tproc then
			d:=ttnamedef[s]
			e:=ttnamedef[t]
			if d and e then
				if not comparemodes(d.mode,e.mode) then return 0 fi
				if d.paramlist=nil and e.paramlist=nil then return 1 fi
			fi
		esac

	elsif sbase=tc8 and tbase=tu8 or sbase=tu8 and tbase=tc8 then
		return 1
	else
!else needs complex param/result-matching
!...
	fi
	return 0
end

func tevalconvert(unit p,int s,t,opc)int=
!conversion op opc to convert from s to t is about to be applied to be
!try and do that at compile time to avoid adding a runtime conversion
!return 1 if it could apply it, 0 if it couldn't
!caller should have already evaluated p to reduce constants etc
	real x,z
	int a,c,sbase,tbase
!
	if p.tag<>jconst then
		return 0
	fi
	a:=p.value
	x:=p.xvalue

	case pr(s,    t)
	when pr(ti64, tr64), pr(ti64, tr32) then
		z:=a

	when pr(tr64, ti64) then
		c:=x

	when pr(tr64, tr32) then
		Z:=X

	when pr(ti64, tu8) then
		c:=byte(a)
	when pr(ti64, ti16) then
		c:=i16(a)

	else
		if ttisinteger[s] and ttisinteger[t] and ttsize[s]=ttsize[t] then
			c:=a
		else
			sbase:=ttbasetype[s]
			tbase:=ttbasetype[t]
			if sbase=tbase then return 1 fi
			return 0
		fi
	esac

	if ttisreal[t] then
		makenewconst(p,i64@(z),t)

	else
		makenewconst(p,c,t)
	fi

	return 1
end

proc tx_assign(unit p,a,b,int t)=
	int m,mm,needres:=t<>tvoid
	symbol d

	case a.tag
	when jmakelist then
		if b.tag=jmakelist then
			if needres then txerror("Mult assign has no result") fi
			tx_assignmultmult(p,a,b)
		else
			tx_assignmultscalar(p,a,b,t)
		fi
		return
	when jdotindex, jdotslice then
		tx_dotindex(a,a.a,a.b,needlv)
		tpass(b,a.mode)
		p.mode:=ti64
		return
	esac

	if a.tag=jname and a.def.islet and p.initlet then
		tpass(a)
	else
		tpasslv(a)
	fi
	m:=a.mode

	a.resultflag:=needres

	if ttbasetype[m]=tslice and b.tag=jmakelist then
		tx_makeslicefromlist(b,b.a,m)
		p.mode:=m

	else
		if b.pclop in [kidiv, kirem] then		!CAN'T JUST OVERRIDE MODE
			tpass(b)
		elsif b.tag=jread then
			tpass(b,m)
		else
			mm:=m
			if ttisshort[m] then
				mm:=gettypebase(m)
			fi
			case b.tag
			when jautocast then
				tpass(b,mm)
			when jmakelist then
				tpass(b,m)
			else
				tpass(b,mm)
			esac
			p.mode:=mm


!Eliminate widening when lhs is not wider than rhs (and when an Widen conversion is used
!which implies that rhs is < 8 bytes)
			STATIC INT NN

			if b.tag=jconvert and b.convcode=kkwiden and
				 ttsize[a.mode]<=ttsize[b.convmode] and not needres then
				DELETEUNIT(B, B.A)
			fi

		fi
	fi
end

proc tx_assignmultmult(unit pp,a,b)=
!mult:=mult
	unit p,q,lhs,rhs

	pp.tag:=jassignmm

	if a.length<>b.length then
		txerror("Mult assign: count mismatch")
	fi
	if a.length=0 then
		txerror("Invalid assignment")
	fi
	rhs:=b.a
	lhs:=a.a

	p:=lhs
	while p, p:=p.nextunit do
		tpasslv(p)
	od

	p:=lhs

	q:=rhs
	while q, (p:=p.nextunit; q:=q.nextunit) do
		tpass(q,p.mode)
	od
end

proc tx_assignmultscalar(unit pp,a,b,int t)=
!assign 'scalar' to mult LHS, but it might be a tuple type or be an expandable one
	unit p,q, alist:=a.a
	int nretmodes,i, alength:=a.length
	ref[]i32 pmult
	symbol d				!point to def containing return mode info

	nretmodes:=0
	pp.tag:=jassignms

	tpass(b,tany)

	case ttbasetype[b.mode]
	when ttuple then
		d:=getprocretmodes(b)
		nretmodes:=d.nretvalues

		if ttbasetype[d.mode]<>ttuple then txerror("Not a tuple") fi

		if alength>nretmodes then
			txerror("mult ass/mult returns don't agree in number")
		fi
		if nretmodes<=1 then
			txerror("mult ass rhs needs fn yielding 2+ values")
		fi

		p:=alist
		pmult:=ttmult[d.mode]
		i:=1

		while p, p:=p.nextunit do
			tpasslv(p,pmult[i++])
		od
	when tslice then
		if alength<>2 then txerror("(a,b):=slice") fi
		tpasslv(alist,createrefmode(nil, tttarget[b.mode]))
		tpasslv(alist.nextunit,ti64)

	when trange then
	when trecord then

	elsif b.tag=jbin and b.pclop=kidivrem then
		if alength<>2 then txerror("(a,b):=divrem") fi
		tpasslv(alist,b.mode)
		tpasslv(alist.nextunit,b.mode)
		pp.tag:=jassignmdrem

	else
		txerror_s("Can't expand to mult values:",strmode(b.mode))
	esac

	pp.mode:=t
end

proc tpasslv(unit p, int t=tany)=
!process p as lvalue, but require it to be of type t
!however no conversion is done (not allowed); only a compare is done
	tpass(p,,needlv)
	if t not in [tany, tvoid] then
		if not comparemodes(p.mode, t) then
			txerror_ss("PassLV type mismatch: #:=#",strmode(p.mode), strmode2(t))
		fi
	fi
end

func dobinnumx(unit p,a,b)int=
!Try and apply this to binary pclopnds:
!	NUMX	NUMX	DOM
!a and b have already been processed, but not coerced to any type yet

	int amode:=a.mode, bmode:=b.mode, cmode

	if isnum(amode) and isnum(bmode) then
		p.mode:=cmode:=min(amode, bmode)
		coerceunit(a,cmode)
		coerceunit(b,cmode)
		return 1
	fi

	if isnum(amode) and isbool(bmode) then
		p.mode:=amode
		coerceunit(b,amode)
		return 1
	elsif isbool(amode) and isnum(bmode) then
		p.mode:=bmode
		coerceunit(a,bmode)
		return 1
	fi


	return 0
end

func dobinnumf(unit p,a,b)int=
!Try and apply this to binary pclopnds:
!	NUMF	NUMF	DOM
	int amode:=a.mode, bmode:=b.mode, cmode

!	if amode=ti64 then coerceunit(a, tr64); amode:=tr64 fi
!	if bmode=ti64 then coerceunit(b, tr64); bmode:=tr64 fi

	if isnumf(amode) and isnumf(bmode) then
		p.mode:=cmode:=min(amode, bmode)
		coerceunit(a,cmode)
		coerceunit(b,cmode)
		return 1
	fi
	return 0
end

func dobinnumi(unit p,a,b)int=
!Try and apply this to binary pclopnds:
!	NUMI	NUMI	DOM
	int amode:=a.mode, bmode:=b.mode, cmode

	if isnumi(amode) and isnumi(bmode) then
		p.mode:=cmode:=min(amode, bmode)
		coerceunit(a,cmode)
		coerceunit(b,cmode)
		return 1
	fi
	return 0
end

func tx_in(unit p,a,b)int=
	int simpleset, amode, bmode
	unit q

	tpass(a)
	tpass(b)
	amode:=a.mode
	bmode:=b.mode

	coerceunit(a,ti64)

	simpleset:=1
	if b.tag=jmakeset then
		q:=b.a
		while q, q:=q.nextunit do
			if not ttisinteger[q.mode] then
				simpleset:=0
				exit
			fi
		od
	fi

	if isnum(a.mode) and b.tag in [jmakerange, jmakeset] and simpleset then
		p.tag:=(b.tag=jmakerange|jinrange|jinset)
	else
		txerror("doin")
	fi
	p.mode:=tbool

!	if p.pclop=kknotin then
	if p.inv then
		addnotl(p)
	fi
	return 1
end
=== mc_decls.m 0 0 29/34 ===

export record regopnd =
	byte reg
	byte size
end

!export record mclrec = $caligned
export record mclrec =
	mcl lastmcl, nextmcl
	union
		struct
			byte a, b, c
		end
		[3]byte regs
	end
	union
		struct
			byte asize, bsize, csize
		end
		[3]byte sizes
	end

	byte opcode
	byte regmode				!which of r/r,r etc (see rm_mode enums)

	union
		i64 value
		r64 xvalue
		ichar svalue
		int labelno
		psymbol def
	end
	i32	offset					!additional label offset
	byte regext					!LSL ASR UXxx SXxx opcode used to shift/ext reg
	byte shift					!shift amount for regext
	byte flags: (excl:1, hash:1, inside:1, lo12:1)		!"!", "#", or label inside []
	byte valtype				!which of value/xvalue...def are in use

	u32 seqno
	u32 mpos

	union
		u16 suffix				!opcode extension (eg. 'SB'
		byte condcode			!cond for either Bcc suffix, or as final operand
	end
	byte nregs					!number of reg opnds, 0 to 3

end

export type mcl = ref mclrec

export enumdata [0:]ichar valtypenames =
	(no_val=0,		$),		!no operand
	(intimm_val,	$),		!immediate int
	(realimm_val,	$),		!immediate real (mainly for dq etc)
	(realmem_val,	$),		!indirect real (for movq etc)
	(stringimm_val,	$),		!immediate string, for comments, or address of string etc
	(def_val,		$),		!var/proc name
	(label_val,		$),		!label index
!	(labelind_val,	$),		!label index
	(name_val,		$),		!immediate string must be output as an unquoted name
	(temp_val,		$),		!index of pclopnd temp (later becomes ptr to descriptor?)
!	(syscall_val,	$),		!
end

export enumdata []ichar mclnames =

	(m_procstart,	$),
	(m_procend,		$),
	(m_comment,		$),
!	(m_blank,		$),
!	(m_deleted,		$),
	(m_labelname,	$),
	(m_define,		$),
	(m_definereg,	$),
	(m_trace,		$),
	(m_endx,		$),

	(m_label,		$),
	(m_nop,			$),

	(m_db,			"m_.byte"),
	(m_dw,			"m_.half"),
	(m_dd,			"m_.word"),
	(m_dq,			"m_.xword"),
	(m_ascii,		"m_.ascii"),
	(m_asciiz,		"m_.asciz"),

	(m_isegment,	"m_.data"),
	(m_zsegment,	"m_.data"),
	(m_csegment,	"m_.text"),

	(m_align,		$),
	(m_resb,		$),
	(m_resw,		$),
	(m_resd,		$),
	(m_resq,		$),

	(m_add,			$),
	(m_adr,			$),
	(m_adrp,		$),
	(m_and,			$),
	(m_asr,			$),
	(m_b,			$),
	(m_bcc,			$),
	(m_bl,			$),
	(m_blr,			$),
	(m_br,			$),
	(m_cmn,			$),
	(m_cmp,			$),
	(m_csel,		$),
	(m_cset,		$),
	(m_eor,			$),

	(m_fabs,		$),
	(m_fadd,		$),
	(m_fcmp,		$),
	(m_fcmpe,		$),
	(m_fcvt,		$),
	(m_fcvtzs,		$),
	(m_fdiv,		$),
	(m_fmov,		$),
	(m_fmul,		$),
	(m_fneg,		$),
	(m_fsub,		$),


	(m_ldp,			$),
	(m_ldr,			$),
	(m_lsl,			$),
	(m_lsr,			$),
	(m_mov,			$),
	(m_movi,		$),
	(m_movk,		$),
	(m_mul,			$),
	(m_mvn,			$),
	(m_neg,			$),
	(m_negs,		$),
	(m_orr,			$),
	(m_ret,			$),
	(m_ror,			$),
	(m_scvtf,		$),
	(m_sdiv,		$),
	(m_smull,		$),
	(m_stp,			$),
	(m_str,			$),
	(m_sub,			$),
	(m_sxtb,		$),
	(m_sxth,		$),
	(m_sxtw,		$),
	(m_udiv,		$),
	(m_umulh,		$),
	(m_umull,		$),
	(m_uxtw,		$),

	(m_dotzero,		"m_.zero"),
	(m_push,		$),
	(m_pop,			$),

end

export enumdata [0:]ichar regnames =
	(rnone=0,	$),			! means no register/non used

	(r0,		$),			!integer regs (X/W in asm)
	(r1,		$),
	(r2,		$),
	(r3,		$),
	(r4,		$),
	(r5,		$),
	(r6,		$),
	(r7,		$),
	(r8,		$),
	(r9,		$),
	(r10,		$),
	(r11,		$),
	(r12,		$),
	(r13,		$),
	(r14,		$),
	(r15,		$),
	(r16,		$),
	(r17,		$),
	(r18,		$),
	(r19,		$),
	(r20,		$),
	(r21,		$),
	(r22,		$),
	(r23,		$),
	(r24,		$),
	(r25,		$),
	(r26,		$),
	(r27,		$),
	(r28,		$),
	(r29,		$),			!also rframe
	(r30,		$),			!also rlink

	(rstack,	$),			!special regs
	(rzero,		$),

	(v0,		$),			!float regs (D/S/H/B in asm)
	(v1,		$),
	(v2,		$),
	(v3,		$),
	(v4,		$),
	(v5,		$),
	(v6,		$),
	(v7,		$),
	(v8,		$),
	(v9,		$),
	(v10,		$),
	(v11,		$),
	(v12,		$),
	(v13,		$),
	(v14,		$),
	(v15,		$),
	(v16,		$),
	(v17,		$),
	(v18,		$),
	(v19,		$),
	(v20,		$),
	(v21,		$),
	(v22,		$),
	(v23,		$),
	(v24,		$),
	(v25,		$),
	(v26,		$),
	(v27,		$),
	(v28,		$),
	(v29,		$),
	(v30,		$),
	(v31,		$),
end

export const rframe = r29
export const rlink  = r30

export const rfirst = r0
export const rlast = v31

export enumdata [0:]ichar condnames, [0:]int asmrevcond =

	(eq_cond=0,	$, 	ne_cond),
	(ne_cond, 	$, 	eq_cond),

	(cs_cond, 	$, 	cc_cond),		!carry set / unsigned >= (also hs)
	(cc_cond, 	$, 	cs_cond),		!carry clear / unsigned < (or lo)

	(mi_cond, 	$, 	pl_cond),		!minus
	(pl_cond, 	$, 	mi_cond),		!plus (>= 0)

	(vs_cond, 	$, 	vc_cond),		!signed overflow (v set)
	(vc_cond, 	$, 	vs_cond),		!no signed overflow (v clear)

	(hi_cond, 	$, 	ls_cond),		!unsigned >
	(ls_cond, 	$, 	hi_cond),		!unsigned <=

	(ge_cond, 	$, 	lt_cond),		!signed >=
	(lt_cond, 	$, 	ge_cond),		!signed <

	(gt_cond, 	$, 	le_cond),		!signed >
	(le_cond, 	$, 	gt_cond),		!signed <=

	(al_cond, 	$, 	nv_cond),		!always
	(nv_cond, 	$, 	al_cond),		!always
end

export enumdata [0:]ichar segmentnames =
	(no_seg=0,		$),
	(code_seg,		$),
	(idata_seg,		$),
	(zdata_seg,		$),
	(rodata_seg,	$),
	(impdata_seg,	$),
end

export enumdata [0:]ichar reftypenames =
	(extern_ref=0,		$),		!is external
	(fwd_ref,			$),		!not yet reached
	(back_ref,			$),		!has been reached
end

!reg-operand pattern
export enumdata [0:]ichar rmnames =
	(rm_none=0,	$),			! no regs used
	(rm_reg,	$),			! R [,R [,R]]  Use .nregs for count
	(rm_rm,		$),			! R, [R]
	(rm_rrm,	$),			! R, R, [R]
	(rm_rmm,	$),			! R, [R, R]
end

global enumdata [0:]ichar pmcnames =
	(pmc_ignore=0,	"Ignore"),
	(pmc_stack,		"Stack"),
	(pmc_reg,		"Reg"),
	(pmc_spill,		"Spill"),
	(pmc_move,		"Move"),
end	

global enumdata [0:]ichar locnames =
	(pcl_loc=0,	"pend"),				!operand still in pcl instruction
	(reg_loc,	"reg"),					!is in register (look at mode for reg/xreg)
	(regvar_loc,"regvar"),				!lives in register (look at mode for reg/xreg)
	(temp_loc,	"temp"),				!overflow to temporary
end

global [rfirst..rlast]byte regset		!register in-use flags: 0/1: free/in-use
!global [rfirst..rlast]byte isregvar		!1 if reg var and in-use

global const workrega  = r9,  workregb  = r15
global const workxrega = v16, workxregb = v31

global const regvara  = r19,  regvarb  = r28
global const xregvara  = v8,  xregvarb  = v15

global int tempreg						!helper reg: r7/r8 or rnone

!global record pair =
!	u64 low, high
!end

!global pair regsetpr @ regset
!global pair isregvarpr @ isregvar
global const u64 invertbytes = 0x0101'0101'0101'0101

!global [rfirst..rlast]byte usedregs		!1 means used during proc

global byte noxorclear					!1 to suppress xor optimisation

global macro zz = noperands
global macro yy = noperands-1
global macro xx = noperands-2
global macro ww = noperands-3

global const maxcalldepth=16
!global [maxcalldepth]byte callalign		!pending 1-slot alignment for syscalls
global [maxcalldepth]byte callblockret	!1 if fnc returns a block
global [maxcalldepth]u32 callblocksize	!size of any returned block
global [maxcalldepth,8]u32 callargsize	!size of any block pushed in low args
global [maxcalldepth]byte callpending	!opnd number waiting for a paired opnd for push

global int ncalldepth

global int lababs32, lababs64
global int labneg32, labneg64
global int labmask63, laboffset64
global int labzero
global int kk0used=0

export mcl mccode, mccodex		!genmc adds to this linked list

global int currsegment=0

global regopnd dstackopnd
global regopnd dframeopnd
global regopnd noreg

global record constrec =
	union
		int value
		real xvalue
		ichar svalue
	end
	ref constrec nextconst
	int labelno
end

global ref constrec cstringlist
global ref constrec vstringlist
global ref constrec creallist
global ref constrec cr32list

global psymbol currasmproc

global int lab_funcnametable
global int lab_funcaddrtable
global int lab_funcnprocs

export record relocrec =			!informal version
	ref relocrec nextreloc
	int reloctype
	int offset
	int stindex
end

!record used for expanding buffers. Expansion is not automatic: buffercheck(n)
!is needed at strategic points to ensure that are at least n bytes left
export record dbuffer =
	ref byte pstart
	union
		ref byte pcurr
		ref u16 pcurr16
		ref u32 pcurr32
		ref u64 pcurr64
	end
	ref byte pend
	int alloc
end

global int ss_zdatalen
global ref dbuffer ss_zdata			!used for error checking only (should be empty at end)
global ref dbuffer ss_idata
global ref dbuffer ss_code
global ref relocrec ss_idatarelocs
global ref relocrec ss_coderelocs
global int ss_nidatarelocs
global int ss_ncoderelocs

!const max_ss_symbols=32768				!exported to coff
global const init_ss_symbols=32768				!exported to coff
!global const init_ss_symbols=16384
global ref []psymbol ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize

global ref[]psymbol labeldeftable

global int aaseqno

!The following are highly dependent on the ordering of the base types being:
! r32 r64 ints... block ..., with r32 having value 1
!They assume mode is not void, and for ispfloat, is not a block

global macro ispwide(m)  = m - 1
global macro ispfloat(m) = m <= tpr64
global macro ispint(m)   = m > tpr64	!when block type is not expected

EXPORT [1..8]byte regmodes=(tpu8, tpu16, 0, tpu32, 0,0,0, tpu64)

global byte pmode
global pcl currpcl

global mcl mclprocentry
global mcl mce_oldmccodex, mce_lastmcl, mce_nextmcl		!used by reset/setmclentry
global mcl mcf_oldmccodex, mcf_lastmcl, mcf_nextmcl		!used by reset/setmclentry for frame setup

!global byte fpshortnames
global byte fpcheckunusedlocals
!export byte phighmem

global record riprec =
	ref riprec next
	u32 offset			!within code segment, offset of d32 field
	i32 immsize			!0,1,4 bytes of trailing imm field
end

!global record fwdrec =
!	ref fwdrec nextfwd
!	i32 offset
!	i16 reltype
!	i16 seg
!end

global ref riprec riplist

!export ref proc (ref void) idomcl_assem
!export ref func (ref void)int icheckasmlabel
!export ref func (int)psymbol igethostfn

global const maxblocktemps=50
global [maxblocktemps]psymbol blockdefs
global int nblocktemps


!global [pstdnames.bounds]byte ploadopx
!
!global [pstdnames.bounds]byte ploadop
!
proc start=
!	for i in ploadop.bounds do ploadop[i]:=m_nop od
!
!	ploadop[tpu8]:=ploadop[tpu16]:=ploadop[tpu32]:=m_movzx
!	ploadop[tpi8]:=ploadop[tpi16]:=ploadop[tpi32]:=m_movsx
!	ploadop[tpr32]:=m_movd
!	ploadop[tpr64]:=m_movq
!	ploadop[tpu64]:=ploadop[tpi64]:=m_mov
end

=== mc_genmcl.m 0 0 30/34 ===
!const fshowpcl=1
const fshowpcl=0

!global const docalltrace=1
!global const docalltrace=0

GLOBAL INT DEBUG

!global int framebytes, frameoffset, paramoffset

[pclnames.bounds]ref proc(pcl) px_handlertable

[6]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
![6]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

global proc genmcl(ichar dummy=nil)=

!CPL "DOING GENMCL"
	return when mcldone

!CPL "DOING GENMCL2"

	IF FSHOWPCL THEN CPL "********* ASM HAS PCL INFO *********" FI

!CPL =CURRFUNC

	int tt:=os_clock()

	inithandlers()
!	mclinit()
	initmcdest()

	GENMC(M_NOP)
	GENMC(M_MOV)

	currpcl:=pcstart

!	while currpcl do
!		convertpcl(currpcl)
!
!		currpcl:=currpcl.next
!
!	od
!
CPL "DONE CONVERTPCL"
!
!	genrealtable()
!!CPL $LINENO
!	genabsneg()
!*!	genstringtable()
!!CPL $LINENO

	genmc(m_endx)					!need as buffer in optimiser
	genmc(m_endx)

	mcldone:=1

	mcltime:=os_clock()-tt
!CPL $LINENO

end

!FUNC CHECKFPUSED(MCLOPND A)int=
!	RETURN 0 WHEN A=NIL
!	if a.reg=rframe or a.regix=rframe then return 1 fi
!	0
!END
!
proc convertpcl(pcl p)=

!RETURN WHEN P.OPCODE IN [KCOMMENT]
!CPL "    CONV",PCLNAMES[P.OPCODE]

	doshowpcl(p) when fshowpcl

	pmode:=p.mode
	currpcl:=p
	mmpos:=p.pos

	pcseqno:=p.seqno

	px_handlertable[p.opcode]^(p)

!	[r0..r15]byte OLDREGSET
!	pair oldregsetpr @ oldregset
!	OLDREGSET:=REGSET
	clear regset				!clear work reg flags

	int reg
!
!	for i to noperands do
!		reg:=pclreg[i]
!		if reg then				!reset work reg occupied by a pcl opnd
!			regset[reg]:=1
!		fi
!	od

end

global proc initmcdest=
!reset mccode/mccodex
!called should have saved any values from last linked list 
	mccode:=mccodex:=nil
!	clear rtsproclabels
end

proc inithandlers=
	static byte initdone=0
	ichar name, s
	int n

	if initdone then return fi

	n:=$getnprocs()

	for i to n do
		name:=$getprocname(i)
		if eqbytes(name,"px_",3) then
			for k in pclnames.bounds do
				s:=pclnames[k]
				if s^='k' then ++s fi				!some are kload, others just store
				if eqstring(s,name+3) then
					px_handlertable[k]:=$getprocaddr(i)
					exit
				fi
			else
				merror("Invalid handler name:",name)
			od
		fi
	od

	static [,2]byte dupltable = (
!
!!mapping           =>
!		(ktoboolf, 		ktoboolt)
!
!		(kcallf,		kcallp)
!		(kicallp,		kcallp)
!		(kicallf,		kcallp)
!
!		(kendmx,		kresetmx)
!		(ktcproc,		kproc)
!
!		(kidivto,		kidiv)
!		(kiremto,		kirem)
		)


	for i to dupltable.len do
		px_handlertable[dupltable[i,1]]:=px_handlertable[dupltable[i,2]]
	end

	for i in px_handlertable.bounds do
		if not px_handlertable[i] then
			px_handlertable[i]:=cast(&unimpl)
		fi
	od

	initdone:=1
end

proc unimpl(pcl p)=
	[100]char str
	fprint @str, "Unimpl: # (#)", pclnames[p.opcode], strpmode(pmode)
!	CPL STR
	mgencomment(pcm_copyheapstring(str))
end

proc doshowpcl(pcl p)=
	[1256]char str

	return unless fshowpcl

	case p.opcode
	when kretproc, kretfn then
	else
		strcpy(&.str,"                       ")
		strcat(&.str,strpclstr(p))
		mgencomment(PCM_COPYHEAPSTRING(&.str))
	esac
end

proc px_nop*(pcl p) =
! ?
!*!	unimpl(p)
end

proc px_comment*(pcl p) =
! Comment C (a string)
!	unimpl(p)
end

proc px_label*(pcl p) =
	genmc(m_label)
	mgenlabel(p.a.labelno)
end

proc px_jump*(pcl p) =
	int labno:=p.a.labelno
!	pcl q:=p.

!	while q.opcode=kcomment do ++q od
!	case q.opcode
!	when klabel then
!		if q.labelno=labno then return fi
!		++q
!		if q.opcode=klabel and q.labelno=labno then return fi
!	when kjump then
!		q.opcode:=knop
!	esac
!
	genmc_label(m_b, labno)
end

!proc px_stop*(pcl p) =
!! Stop Z
!
!	loadopnd(zz,tpu64, r0)
!
!	genmc(m_bl)
!	mgenname("exit")
!
!	poppcl()
!end

export func pcl_writeasm(ichar filename=nil, int atype='AA')ichar=
	ref strbuffer asmstr
	filehandle f

!	if assemtype<>atype then
!		pclerror("Wrong ASM Module")
!	fi

!	if assemtype='NASM' then
!		phighmem:=2
!	fi

	genmcl()

	asmstr:=getassemstr()

	if filename then
		if pverbose then println "Writing", filename fi

		f:=fopen(filename,"w")
		gs_println(asmstr, f)
		fclose(f)

		gs_free(asmstr)
		nil
	else
		asmstr.strptr
	fi
end

=== mc_libmcl.m 0 0 31/34 ===
const fuseregtable=1
!const fuseregtable=0

global const targetsize=8

export const ctarget=0

!global int mclseqno
EXPORT int mclseqno
EXPORT int NMCLOPND

[20]psymbol nametable
int nnametable

global macro isframex(d) = (d.id in [local_id, param_id])

!global macro mcomm = mgencomment

export proc mclinit(int bypass=0)=
	int r,s

	if mclrec.bytes>64 then ABORTPROGRAM("MCLREC>64B") fi

	initmcdest()

	setsegment('C')

	lab_funcnametable:=0
	lab_funcaddrtable:=0

!bypass is used when directly using mcl api (eg. from an external assembler)
!then genmcl(), called from pcl functions, is a no-op
	if bypass then
!*!		mcldone:=1
	fi
end

global proc initmcdest=
!reset mccode/mccodex
!called should have saved any values from last linked list 
	mccode:=mccodex:=nil
!	clear rtsproclabels
end

global proc genmc(int opcode)=				!used in do_mcl/assem in host
	mcl m, oldm
	int labno

	m:=pcm_allocnfz(mclrec.bytes)

	m.opcode:=opcode
	m.seqno:=++mclseqno
	m.mpos:=mmpos

!CPL "GENMC", MCLNAMES[OPCODE]

	if mccode then
		m.lastmcl:=mccodex
		mccodex.nextmcl:=m
		mccodex:=m
	else
		mccode:=mccodex:=m
	fi
!CPL "GENMC", MCLNAMES[OPCODE], MCCODE, MCCODEX
end

global proc genmc_reg(int opcode, int a, b=rnone, c=rnone)=
!assume 1-3 reg args provided
	int nregs, rm

	genmc(opcode)

	mccodex.a:=a
	mccodex.b:=b
	mccodex.c:=c

	mccodex.asize:=8

	nregs:=1
	rm:=rm_reg
	if b then
		nregs:=2
		mccodex.bsize:=8
		if c then
			nregs:=3
			mccodex.csize:=8
		fi
	fi

	mccodex.nregs:=nregs
	mccodex.regmode:=rm

end

global proc genmc_rm(int opcode, int a, b)=
	genmc(opcode)

	mccodex.a:=a
	mccodex.b:=b
	mccodex.asize:=8
	mccodex.bsize:=8
	mccodex.nregs:=2
	mccodex.regmode:=rm_rm
end

global proc genmc_rrm(int opcode, int a, b, c)=
	genmc(opcode)

	mccodex.a:=a
	mccodex.b:=b
	mccodex.c:=c
	mccodex.asize:=8
	mccodex.bsize:=8
	mccodex.csize:=8
	mccodex.nregs:=3
	mccodex.regmode:=rm_rrm
end

global proc genmc_rmm(int opcode, int a, b, c)=
	genmc_rrm(opcode, a,b,c)
	mccodex.regmode:=rm_rmm
end

global proc genmc_cond(int opcode, int cond)=
	genmc(opcode)
	mccodex.condcode:=cond
end

global proc genmc_label(int opcode, int labelno)=
	genmc(opcode)
	mgenlabel(labelno)
end

global proc genmc_condlabel(int opcode, int cond, labelno)=
	genmc(opcode)
	mccodex.condcode:=cond
	mgenlabel(labelno)
end

global proc genmc_int(int opcode, int value)=
	genmc(opcode)
	mgenint(value)
end

global proc genmc_string(int opcode, ichar svalue)=
	genmc(opcode)
	mgenstring(svalue)
end

global proc genmc_name(int opcode, ichar svalue)=
	genmc(opcode)
	mgenname(svalue)
end

global proc genmc_def(int opcode, psymbol d)=
	genmc(opcode)
	mgendef(d)
end

global proc msetsize(int size)=
	mccodex.asize:=size
	mccodex.bsize:=size
	mccodex.csize:=size
end

global proc msetsizea(int size)=
	mccodex.asize:=size
end

global proc msetsizeb(int size)=
	mccodex.bsize:=size
end

global proc msetsizec(int size)=
	mccodex.csize:=size
end

global proc msuffix(int suffix)=
	mccodex.suffix:=suffix
end

global proc mregext(int opc, shift)=
	mccodex.regext:=opc
	mccodex.shift:=shift
end

global proc minside=
	mccodex.inside:=1
end

global proc mexcl=
	mccodex.excl:=1
end

global proc mlo12=
	mccodex.lo12:=1
end

global proc mhash=
	mccodex.hash:=1
end

global proc mgenint(int value)=
	mccodex.value:=value
	mccodex.valtype:=intimm_val
end

global proc mgenrealimm(real xvalue, int mode=tpr64)=
	mccodex.xvalue:=xvalue
	mccodex.valtype:=realimm_val
end

global proc mgenlabel(int labelno)=
	mccodex.labelno:=labelno
	mccodex.valtype:=label_val
end

global proc moffset(int offset)=
	mccodex.offset:=offset
end

global proc mgencomment(ichar s)=
	genmc(m_comment)
	mgenstring(s)
end

global proc mcomm(ichar s, t="", u="")=
	[256]char str
	print @str, s,t,u
	mgencomment(pcm_copyheapstring(str))
end

global proc mgenstring(ichar s)=
	mccodex.svalue:=pcm_copyheapstring(s)
	mccodex.valtype:=stringimm_val
end

!global proc mgenmemaddr(psymbol d)=
!	mccodex.def:=d
!	mccodex.valtype:=memaddr_val
!end

global proc mgendef(psymbol d)=
	mccodex.def:=d
	mccodex.valtype:=def_val
end

global proc mgenname(ichar s)=
	mccodex.svalue:=s
	mccodex.valtype:=name_val
end

!global func mgenname(ichar s)regopnd=
!	[64]char str
!	regopnd a
!	a:=newregopnd()
!	a.mode:=a_imm
!	a.svalue:=pcm_copyheapstring(s)
!	a.valtype:=name_val
!	a.size:=8
!
!	return a
!end
!
global proc setsegment(int seg,align=1)=
!!seg is 'D', 'Z', 'C', 'R' for data, zdata, code, rdata
	int opc,oldalign

	if seg<>currsegment then
		case seg
		when 'I' then opc:=m_isegment
		when 'Z' then opc:=m_zsegment
		when 'C' then opc:=m_csegment
		when 'R' then MERROR("CAN'T DO RODATA SEG")
		else
			MERROR("BAD SEG CODE")
		esac
		if mccodex and mccodex.opcode in [m_isegment,m_zsegment,m_csegment] then
			mccodex.opcode:=opc
		else
			genmc(opc)
		fi

		currsegment:=seg
	fi

	if align>1 then
		if mccodex.opcode=m_align then
			oldalign:=mccodex.value
			if oldalign>=align then return fi
		fi
!*!		genmc(m_align,mgenint(align))
	fi
end

!global func applyoffset(regopnd a,int offset,int size=0)regopnd=
!!astr is an asm operand
!!add possible byte offset
!	regopnd b
!
!	if offset=0 and size=0 then
!		return a
!	fi
!	b:=duplopnd(a)
!	b.offset+:=offset
!	if size then
!		b.size:=size
!	fi
!
!	return b
!end
!
!export func mgenint(i64 x,int mode=tpi64)regopnd a=
!	int size:=psize[mode]
!
!	if x in -1..10 and size=8 then
!		return smallinttable[x]
!	fi
!
!	a:=newregopnd()
!	a.mode:=a_imm
!
!	a.value:=x
!	a.valtype:=intimm_val
!	a.size:=size
!
!	return a
!end
!
!global func mgenint0(i64 x,int size=8)regopnd a=
!	a:=newregopnd()
!	a.mode:=a_imm
!
!	a.value:=x
!	a.valtype:=intimm_val
!	a.size:=size
!
!	return a
!end
!
!global func mgenrealmem(r64 x,int mode=tpr64)regopnd a=
!	a:=newregopnd()
!	a.mode:=a_mem
!	if ispwide(mode) then
!		a.value:=getrealindex(x)
!	else
!		a.value:=getr32index(x)
!	fi
!	a.valtype:=label_val
!	a.size:=psize[mode]
!	return a
!end
!
!export func mgenrealimm(r64 x,int mode=tpr64)regopnd a=
!	a:=newregopnd()
!	a.mode:=a_imm
!	a.xvalue:=x
!	a.valtype:=realimm_val
!	a.size:=psize[mode]
!	return a
!end
!
!EXPORT func mgenlabel(int x=0)regopnd a=
!!x is a label index
!!generate immediate operand containing label
!	a:=newregopnd()
!	a.mode:=a_imm
!
!	if x=0 then x:=++mlabelno fi
!	a.value:=x
!	a.valtype:=label_val
!
!	return a
!end
!
!global func mgenlabelmem(int x)regopnd a=
!!x is a label index
!!generate immediate operand containing label
!
!	a:=mgenlabel(x)
!	a.mode:=a_mem
!	return a
!end
!
!export func mgenmem(psymbol d, int mode=tpu64)regopnd a=
!	int reg
!
!	if d.reg then
!		if pfloat[d.mode] then
!			return mgenxregvar(d)
!		else
!			return mgenregvar(d, mode)
!		fi
!	fi
!
!	reg:=rnone
!	if isframex(d) then
!!		if not foptim and (int(d.offset) in -128..64) and ttsize[d.mode]=8 then
!!			return frameregtable[d.offset]
!!		fi
!
!		reg:=rframe
!		usedregs[rframe]:=1
!
!	fi
!
!	a:=newregopnd()
!	a.mode:=a_mem
!	a.reg:=reg
!	a.def:=d
!	++d.nrefs
!	a.valtype:=def_val
!
!	if mode then
!		a.size:=psize[mode]
!	else
!		a.size:=min(d.size,8)
!	fi
!
!	return a
!end
!
!EXPORT func mgenmemaddr(psymbol d)regopnd=
!	regopnd a
!
!	d.addrof:=1
!	++d.nrefs
!
!	a:=newregopnd()
!	a.mode:=a_imm
!
!	a.def:=d
!	++d.nrefs
!	a.valtype:=def_val
!	a.size:=8
!
!	return a
!end

export func mgenreg(int reg, size)regopnd a =
!reg is r0/r30/v0/v31
	a.reg:=reg
	a.size:=size

	a
end

global func roundsizetg(int size)int=
!make sure size is round up to next multiple of targetsize (4 or 8)
	if size iand 7=0 then return size fi
	return size+(8-(size iand 7))
end

global proc merroropnd(ichar mess,int opndtype)=
	fprintln "MCL Opnd not supported: # (#) [#]",mess,opndnames[opndtype]
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mmpos>>24],mmpos iand 16777215)
end

global func mcreatefwdlabel:int =
	return ++mlabelno
end

global proc mdefinefwdlabel(int lab) =
!*!	genmc(m_label,mgenlabel(lab))
end

!global func mgenextname(ichar s)regopnd=
!	[64]char str
!	psymbol d
!	static [20]psymbol table
!	static int ntable
!
!	strcpy(&.str,s)
!	str[strlen(s)]:=0			!lose final *
!
!	d:=findnamesym(str)
!
!	if not d then
!		d:=pcm_allocnfz(pstrec.bytes)
!
!		d.name:=pcm_copyheapstring(&.str)
!		d.id:=import_id
!		d.imported:=1
!		addnamesym(d)
!	fi
!
!	return mgenmemaddr(d)
!end
!
!global func mgenregvar(psymbol d, int mode)regopnd a=
!	a:=mgenreg(d.reg, mode)
!!	isregvar[d.reg]:=1
!
!	return a
!end
!
!global func mgenxregvar(psymbol d)regopnd a=
!	a:=mgenxreg(d.reg)
!	isxregvar[d.reg]:=1
!
!	return a
!end

!global func getprimreg(regopnd ax)int =
!!get primary reg value; only one should be active
!!return 0 if no regs
!!//error if both regs are active
!
!	if ax.reg then
!!		if ax.regix then merror("getprim?") fi
!		ax.reg
!	else
!		ax.regix	!0 if no regs used
!	fi
!end

global proc pushslots(int nslots)=
!*!	pushstack(nslots*8)
!	mstackdepth+:=nslots
end

global proc popslots(int nslots)=
!*!	popstack(nslots*8)
!	mstackdepth-:=nslots
end

global proc pushstack(int n)=
	if n then
!*!		genmc(m_sub,dstackopnd,mgenint(n))
	fi
end

!global proc popstack(int n)=
!	if n then
!		genmc(m_add,dstackopnd,mgenint(n))
!	fi
!end
!
global func getstringindex(ichar s)int=
	if s=nil then			!assume nil
		kk0used:=++mlabelno
		return kk0used
	fi

	if cstringlist and eqstring(cstringlist.svalue,s) then
		return cstringlist.labelno
	fi

	return addconst(cstringlist, cast(s))
end

global func addconst(ref constrec &clist, int value)int=
	ref constrec p
	p:=pcm_allocnfz(constrec.bytes)
	p.value:=value
	p.labelno:=++mlabelno
	p.nextconst:=clist
	clist:=p
	return mlabelno
end

global func getrealindex(real x)int=
	return addconst(creallist,cast@(x,int))
end

global func getr32index(real x)int=
	return addconst(cr32list,cast@(x,int))
end

!global func ispoweroftwo(i64 x)int=
EXPORT func ispoweroftwo(i64 x)int=
!when x is a power of two, and is at least 2, then return the power (ie. equiv number of shifts)
!otherwise return zero when x is negative, 0, 1, not a power of two, or more than 2**31
	i64 a
	int n

	a:=1
	n:=0
	to 60 do
		++n
		a:=a<<1
		if a=x then
			return n
		fi
	od
	return 0
end

global proc axerror(ichar mess)=
	CPL "AX ERROR:", mess, "AASEQ:", aaseqno
	CPL
	STOP 1

end

!global func newblocktemp(int size)psymbol=
!	[16]char str
!	psymbol d
!
!	if nblocktemps>maxblocktemps then
!		merror("Too many block temps")
!	fi
!	++nblocktemps
!
!	fprint @str,"$B#",nblocktemps
!	d:=pc_makesymbol(str, misc_id)
!	d.mode:=tpblock
!	d.size:=size
!	d.used:=1
!	d.id:=local_id
!	d.nextlocal:=currfunc.nextlocal
! 	d.owner:=currfunc
!	currfunc.nextlocal:=d
!
!	blockdefs[nblocktemps]:=d
!	d
!end

global func findnamesym(ichar s)psymbol d=
!search for s in cache of named symbols

	for i to nnametable do
		if eqstring(s, nametable[i].name) then
			return nametable[i]
		fi
	od
	nil
end

global proc addnamesym(psymbol d)=
!add new name symbol, which should be unique

	if nnametable<nametable.len then
		nametable[++nnametable]:=d
	else
		merror("Ext nametab overflow")
	fi
end

!func mgenstringx(ichar s)regopnd=
!	mgenlabelmem(getstringindex(s))
!end
!
!global proc clearreg(regopnd ax)=
!	if ax.size=8 then
!		ax:=changeopndsize(ax,4)
!	fi
!	genmc(m_xorx, ax, ax)
!end
!

=== mc_objdecls.m 0 0 32/34 ===
global record imagefileheader =
	u16	machine
	u16	nsections
	u32	timedatestamp
	u32	symtaboffset
	u32	nsymbols
	u16	optheadersize
	u16	characteristics
end

global record imagedir =
	u32	virtualaddr
	u32	size
end

global record optionalheader =			!exe/dll only
	u16  magic
	byte     majorlv
	byte     minorlv
	u32 codesize
	u32 idatasize
	u32 zdatasize
	u32 entrypoint
	u32 codebase
!	u32 datebase		!32-bit exe files only
	u64	imagebase
	u32 sectionalignment
	u32 filealignment
	u16  majorosv
	u16  minorosv
	u16  majorimagev
	u16  minorimagev
	u16  majorssv
	u16  minorssv
	u32 win32version
	u32 imagesize
	u32 headerssize
	u32 checksum
	u16  subsystem
	u16  dllcharacteristics
	u64   stackreserve
	u64   stackcommit
	u64   heapreserve
	u64   heapcommit
	u32 loaderflags
	u32 rvadims
	imagedir exporttable
	imagedir importtable
	imagedir resourcetable
	imagedir exceptiontable
	imagedir certtable
	imagedir basereloctable
	imagedir debug
	imagedir architecture
	imagedir globalptr
	imagedir tlstable
	imagedir loadconfigtable
	imagedir boundimport
	imagedir iat
	imagedir delayimportdescr
	imagedir clrheader
	imagedir reserved
end

global record imagesectionheader =
	[8]char name
	union
		u32	physical_address
		u32	virtual_size
	end
	u32	virtual_address
	u32	rawdata_size
	u32	rawdata_offset
	u32	relocations_ptr
	u32	linenos_offset
	u16	nrelocs
	u16	nlinenos
	u32	characteristics
end

global record imagesymbol =
	union
		[8]char shortname
		struct
			u32	shortx
			u32	longx
		end
		u64 longname
	end
	u32	value
	i16	sectionno
	u16	symtype
	byte	storageclass
	byte	nauxsymbols
end

global record importdirrec =
	u32	implookuprva
	u32	timedatestamp
	u32	fwdchain
	u32	namerva
	u32	impaddressrva
end

global record coffrelocrec =
	i32	virtualaddr
	i32	stindex
	i16	reloctype
end

global enumdata [0:]ichar relocnames =
	(abs_rel = 0,	$),
	(addr64_rel,	$),
	(addr32_rel,	$),
	(addr32nb_rel,	$),
	(rel32_rel,		$),
	(rel321_rel,	$),
	(rel8_rel,		$),				!used within assembler only, not in coff format
end

global record auxsectionrec = 
	i32 length
	i16 nrelocs
	i16 nlines
	i32 checksum
	i16 sectionno
	i32 dummy
end

global record sectionrec =
	union
		ref dbuffer data		!copy of ss_zdata etc
		ref byte bytedata		!added later, eg, import dir block
	end
	ichar name					!name like ".bss" as it will be in obj/exe file
	int segtype					!code_seg etc
	int rawsize					!in file
	int rawoffset				!offset in exe file
	int virtsize				!in image
	int virtoffset				!offset from imagebase
	ref relocrec relocs			!for idata/code: reloc info needs to be processed
	int nrelocs					!
end

global record importrec = 				!details about all imported symbols
	psymbol def				!full st entry
	int libno					!which dll lib this belongs to
	ichar name					!name of symbol (extracted from lib.name if needed)
	int hintnameoffset			!voffset of hint/name entry in impdir section
	int iatoffset				!voffset of IAT entry
	int thunkoffset				!offset within code section of thunk entry
end

global record exportrec = 		!details about all exported symbols
	psymbol def				!full st entry
	ichar name					!name of symbol (extracted from lib.name if needed)
end

global record dllrec =					!all imported libraries
	ichar name					!name of library, including .dll
	int nprocs					!no. of imports which use this library
	int nametableoffset			!start of name table in impdir
	int addrtableoffset			!start of addr table (IAT)
	int dllnameoffset			!offset of name within impdir
	int dllextraoffset			!offset of mysterious region just before the name
end

global record exportdirrec =
	u32 exportflags
	u32 timedatestamp
	u16 majorversion
	u16 minorversion
	u32 namerva
	u32 ordinalbase
	u32 naddrtable
	u32 nnamepointers
	u32 expaddressrva
	u32 namepointerrva
	u32 ordtablerva
end
=== mc_writeasm.m 0 0 33/34 ===
!export int assemtype='AA'

!const fshowseq=1
const fshowseq=0

!const useintelregs=1
const useintelregs=0

!const showsizes=1
const showsizes=0

!const showfreed=1
const showfreed=0

!const fextendednames=1			!include module name in qualified names
const fextendednames=0

![8, r0..r15]ichar nregnames
!
[regnames.bounds]psymbol regvars		!nil, or strec when it uses that reg
![r0..r15]psymbol xregvars

proc writemcl(int index,mcl m)=

!	case m.opcode
	
!	if m.opcode=m_comment and m.a.svalue^='?' then
!	else
		strmcl(m)
		gs_line(pdest)
!	fi
!	esac
end

global proc strmcl(mcl m)=
	static [512]char str
	[128]char opcname
	[16]char ccstr
!	mclopnd a,b
	int opcode,cond,sizepref
	ichar s,comment
	psymbol d

	opcode:=m.opcode
	str[1]:=0

!	cond:=m.cond
!	a:=m.a
!	b:=m.b
	comment:=nil

!CPL "STRMCL", MCLNAMES[M.OPCODE]

	case opcode
	when m_procstart then
		d:=m.def
		asmstr("# Proc ")
		asmstr(d.name)

		currasmproc:=m.def
		clear regvars

		return

	when m_procend then
		asmstr("# End\n")
		currasmproc:=nil

		return

	when m_comment then
		asmstr("# ")
		asmstr(m.svalue)
		return
	when m_endx then
		return

	when m_labelname then				!label name will be complete and will have colon(s)
		case m.valtype
		when def_val then
			d:=m.def
			asmstr(getdispname(d))
			if d.id=proc_id and d.exported then
				asmstr(":\n")
				asmstr(getbasename(d.name))
			fi

		when stringimm_val, name_val then
			asmstr(m.svalue)
!			return
		else
			merror("strmcl/lab")
		esac

		asmstr(":")
		

		return

	when m_label then
		if m.valtype=label_val then
			fprint @str,"L#:",m.value
		else
			recase m_labelname
		fi
		asmstr(str)
		return

	when m_define then
		d:=m.def
		asmstr("    .set ")
		asmstr(getdispname(d))
		asmstr(", ")
!CPL "///",=D.OFFSET, D.NAME

		asmint(d.offset)
		return

	when m_definereg then
		d:=m.def
		asmstr("    ")
		asmstr(getdispname(d))
		regvars[d.reg]:=d

!		asmstr(a.svalue)
		asmstr(" .req ")

		asmstr(strreg(d.reg, d.size))

!		case b.mode
!		when a_reg then
!			asmstr(getregname(b.reg, b.size))
!
!		else
!			asmstr(getxregname(b.reg, b.size))
!		esac
		return
	esac

	strcpy(opcname, mclnames[opcode]+2)

	case opcode
	when m_bcc then
		strcpy(ccstr, condnames[m.condcode])
		ccstr[3]:=0				!first two letters only
CPL =M.CONDCODE, condnames[m.condcode]
		strcpy(&opcname[2], ccstr)

	when m_ldr, m_str then
		if m.suffix then
			opcname[4]:=m.suffix
			opcname[5]:=m.suffix>>8
			opcname[6]:=0
			
		fi
	esac

!	ipadstr(opcname,(opcode=m_dq|4|10)," ")
	ipadstr(opcname,10," ")

	ipadstr(str,4)

	strcat(str,opcname)

	asmstr(str)

	case m.regmode
	when rm_none then			!no registers involved
	when rm_reg then			!r,r,r
!		ASMSTR(" RRR")
		asmregopnds(m)
	else						!r with [r] combos
		asmregmemopnds(m)
	esac

	if m.valtype and not m.inside then		!additional non-reg opnd
		if m.regmode<>rm_none then
			asmstr(", ")
		fi
		if m.lo12 then
			asmstr(":lo12:")
		fi
		asmstr(strvalue(m))
	fi


!	asmreg(m.a, 1)
!	asmreg(m.b)
!	asmreg(m.c)


!IF FSHOWSEQ THEN ASMSTR("	#"); ASMSTR(STRINT(MCL.SEQNO)) FI
end

proc asmregopnds(mcl m)=
	for i to m.nregs do
		if i>1 then asmstr(", ") fi
		asmreg(m.regs[i], m.sizes[i])
	od

	if m.regext then
		asmstr(" ")
		asmstr(mclnames[m.regext])
		asmstr(" #")
		asmint(m.shift)
	fi

end

proc asmregmemopnds(mcl m)=

	for i to m.nregs do
		if i>1 then asmstr(", ") fi

		if m.regmode=rm_rrm then
			if i=3 then
				asmstr("[")
			fi
		elsif i=2 then
			asmstr("[")
		fi

		asmreg(m.regs[i], m.sizes[i])
	od

	if m.regext then
		asmstr(" ")
		asmstr(mclnames[m.regext])
		asmstr(" #")
		asmint(m.shift)
	fi

	if m.valtype and m.inside then
!CPL "///",=M.LO12, =M.VALTYPE; OS_GETCH()
!	if m.valtype then
		asmstr(", ")
		if m.lo12 then
			asmstr(":lo12:")
		fi
		asmstr(strvalue(m))
	fi

	asmstr("]")
	if m.excl then asmstr("!") fi
end

global func strmclstr(mcl m)ichar=
	gs_init(pdest)
	strmcl(m)
	return pdest.strptr
end

!global func mstropnd(mclopnd a,int sizeprefix=0,opcode=0)ichar=
!	static [512]char str
!	[128]char str2
!	ichar plus,t
!	int offset,tc
!
!"<STROPND>"

!	str[1]:=0
!
!	case a.mode
!	when a_reg then
!		return strreg(a.reg, a.size)
!
!	when a_imm then
!		if opcode=m_dq and a.valtype=intimm_val then
!			if a.value in 0..9 then
!				strcat(str,strint(a.value))
!			else
!				strcat(str,"0x")
!				strcat(str,strword(a.value,"H"))
!			fi
!		else
!			strcpy(str,strvalue(a))
!		fi
!
!	when a_mem then
!		case a.valtype
!		when intimm_val then
!			strcpy(str,strint(a.value))
!		when realimm_val then
!			strcpy(str,strreal(a.xvalue))
!		when realmem_val then
!			fprint @str,"M#",a.xvalue
!		esac
!
!		strcat(str,getsizeprefix(a.size,sizeprefix))
!		strcat(str,"[")
!
!		plus:=""
!		if a.reg then
!			strcat(str,strreg(a.reg,8))
!			plus:=" + "
!		fi
!		if a.regix then
!			strcat(str,plus)
!			strcat(str,strreg(a.regix,8))
!			plus:=" + "
!
!			if a.scale>1 then
!				strcat(str,"*")
!				strcat(str,strint(a.scale))
!			fi
!		fi
!
!		if a.valtype in [def_val,label_val, temp_val] then
!			if plus^ then
!				strcat(str,plus)
!			fi
!			strcat(str,strvalue(a))
!	    elsif offset:=a.offset then
!			print @str2,offset:" + "
!			strcat(str,str2)
!		fi
!		strcat(str,"]")
!
!	when a_xreg then
!		return strxreg(a.reg,a.size)
!
!	else
!		println "BAD OPND",A.MODE
!		return "<BAD OPND>"
!	esac
!
!	return str
!end

global func strvalue(mcl m)ichar=
	static [512]char str
	[128]char str2
	psymbol def
	i64 value,offset,length
	ichar ss
	static ichar longstring

!RETURN "<STRVAL>"
!RETURN VALTYPENAMES[M.VALTYPE]


	def:=m.def
	value:=m.value

	strcpy(str,"")

	case m.valtype
	when def_val then
		strcat(str,getdispname(def))

	addoffset:
		if offset:=m.offset then
			print @str2,(offset>0|"+"|""),,offset
			strcat(str,str2)
		fi

	when intimm_val then
		strcat(str,strint(value))
!STRCAT(STR, "0x")
!		strcat(str,strint(value,"H"))

	when realimm_val then
		print @str,m.xvalue:"20.20"

	when realmem_val then
		strcat(str,"M")
		strcat(str,strreal(m.xvalue))

	when stringimm_val then
!STRCAT(STR, "SIM")
!		strcat(str,"""")
!		strcat(str,m.svalue)
!		strcat(str,"""")

		if (length:=strlen(m.svalue))<str.len/2 then
			strcpy(str,"""")
			convertstring(m.svalue,&.str+1)
			strcat(str,"""")

		else

			if longstring then
				pcm_free(longstring,longstringlen)
			fi
			longstringlen:=length*2
			longstring:=pcm_alloc(longstringlen)
			longstring^:='"'
			length:=convertstring(m.svalue, longstring+1)
			(longstring+length+1)^:='"'
			(longstring+length+2)^:=0
			return longstring
		fi


	when name_val then
		strcat(str,m.svalue)

	when label_val then
		strcat(str,"L")
		strcat(str,strint(m.labelno))
		goto addoffset

!	when temp_val then
!		return gettempname(currasmproc,m.tempno)

	else
		merror("Stropnd?")
	esac

	return str
end

!global proc asmopnd(mclopnd a,int sizeprefix=0,opcode=0)=
!	asmstr(mstropnd(a,sizeprefix,opcode))
!end

proc asmstr(ichar s)=
	gs_str(pdest,s)
end

proc asmint(int a)=
	asmstr(strint(a))
end

proc asmchar(int c)=
	gs_char(pdest,c)
end

global func getdispname(psymbol d)ichar=
	static [256]char str

	if d.reg then

		IF FEXTENDEDNAMES THEN
			fprint @str,"#R.#.#", (pfloat[d.mode]|"X"|""), $PMODULENAME,(fpshortnames|d.name|getfullname(d))
		else
			fprint @str,"#R.#", (pfloat[d.mode]|"X"|""), (fpshortnames|d.name|getfullname(d))
		fi

		return str
	fi

	if fpshortnames then
		return d.name
	else
		return getfullname(d)
	fi

end 

global func gettempname(psymbol d, int n)ichar=
	static [128]char str

	if fpshortnames or d=nil then
		print @str,"T",,n
	else
		fprint @str,"#.$T#",getdispname(d),n
	fi
	str
end

!func strreg(int reg, size)ichar=
!	[8]char str
!	ichar prefix
!
!	case reg
!	when rstack then return "SP"
!	when rlink then return "LR"
!	elsif reg in r0..r30 then
!		fprint @str, "##", (size<=4 |"W"|"X"), reg-r0
!	elsif reg in v0..v31 then
!		case size
!		when 8 then prefix:="D"
!		when 4 then prefix:="S"
!		when 2 then prefix:="H"
!		else        prefix:="B"
!		esac
!		print @str, prefix,,reg-v0
!	else
!		return "r?"
!	esac
!
!	str
!end

global func strreg2(int reg, size=8)ichar=
	static [16]char str
	strcpy(str, strreg(reg, size))
	str
end

global func strreg(int reg, size=8)ichar=
	[8]char str
	ichar prefix
	psymbol d

	d:=regvars[reg]
!	D:=NIL

	if d and d.size=size then
		return getdispname(d)
	fi

	case reg
	when rstack then return "sp"
	when rlink then return "lr"
	when rframe then return "fp"
	elsif reg in r0..r30 then
		fprint @str, "##", (size<=4 |"w"|"x"), reg-r0
	elsif reg in v0..v31 then
		case size
		when 8 then prefix:="d"
		when 4 then prefix:="s"
		when 2 then prefix:="h"
		else        prefix:="b"
		esac
		print @str, prefix,,reg-v0
	else
		return "r?"
	esac

	str
end

export func getassemstr:ref strbuffer=
!write all mcl code in system by scanning all procs
!mcl code is only stored per-proc
	psymbol d,e
	mcl m
	[32]char str2,str3
	int i

CPL "GETASSEMSTR------------"

	gs_init(pdest)

	d:=pstatictable

	while d, d:=d.next do
!		if d.imported then
!			asmstr("    extern ")
!			asmstr(d.name)
!			asmstr("\n")
!		fi
		if d.exported then
			asmstr("    global ")
			asmstr(getbasename(d.name))
			asmstr("\n")
		fi
	od
	asmstr("\n")

MGENCOMMENT("<LOCAL STATICS NOT DONE>")

	m:=mccode
	i:=1
	while m do
!CPL "ASMSTR LOOP:", M, MCLNAMES[M.OPCODE]
		writemcl(i,m)
		++i
		m:=m.nextmcl
	od

!CPL "DONE ASM", PDEST.LENGTH

	return pdest
end

proc asmreg(int reg, size)=
	asmstr(strreg(reg, size))
end
=== mm_help.txt 0 1 34/34 ===
M Compiler for 64-bit Windows

Normal use:           Compiles lead module prog.m to:

    mm      prog      prog.exe (default)
    mm -r   prog      in-memory native code then execute
    mm -i   prog      in-memory IL then interpret

    mm -exe prog      prog.exe
    mm -dll prog      prog.dll
    mm -obj prog      prog.obj
    mm -a   prog      prog.asm
    mm -n   prog      prog.nasm
    mm -mx  prog      prog.mx
    mm -p   prog      prog.pcl (textual IL)
    mm -ma   prog     prog.ma (single amalgamated source file)

Other options:

    -ext              Used std headers external to compiler
    -opt              Optimise native code
    -out:file         Name output file (extension can be added)
    -rip              Use RIP address modes
    -himem            Generate PIC code (automatic with -obj/-dll)
    @file             Read files and options from a file
=== END ===
1 msyswin.m 0 0
2 msys.m 0 0
3 mlib.m 0 0
4 mclib.m 0 0
5 mwindows.m 0 0
6 mwindll.m 0 0
7 mm.m 0 0
8 mm_cli.m 0 0
9 mm_gentcl.m 0 0
10 mm_libtcl.m 0 0
11 mm_blocktcl.m 0 0
12 dummy.m 0 0
13 tc_api.m 0 0
14 tc_decls.m 0 0
15 tc_diags.m 0 0
16 tc_tables.m 0 0
17 mm_decls.m 0 0
18 mm_diags.m 0 0
19 mm_export_dummy.m 0 0
20 mm_lex.m 0 0
21 mm_lib.m 0 0
22 mm_libsources_dummy.m 0 0
23 mm_modules.m 0 0
24 mm_name.m 0 0
25 mm_parse.m 0 0
26 mm_support.m 0 0
27 mm_tables.m 0 0
28 mm_type.m 0 0
29 mc_decls.m 0 0
30 mc_genmcl.m 0 0
31 mc_libmcl.m 0 0
32 mc_objdecls.m 0 0
33 mc_writeasm.m 0 0
34 mm_help.txt 0 1
