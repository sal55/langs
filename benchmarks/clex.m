!C low-level lexer
![early, incomplete, experimental version now used as benchmark.
! for a proper C lexer, see cc_lex.m module of cc project]

import clib
import msys
import mlib
import oslib

!const diagmode=1
const diagmode=0

global tabledata() []ichar symbolnames=
	(error_sym,			$),		! Lex error
	(dot_sym,			$),		! "."
	(comma_sym,			$),		! ","
	(semi_sym,			$),		! ";"
	(question_sym,		$),		! "?"

	(colon_sym,			$),		! ":"
	(assign_sym,		$),		! =
	(lbrack_sym,		$),		! (
	(rbrack_sym,		$),		! )
	(lsq_sym,			$),		! [
	(rsq_sym,			$),		! ]
	(lcurly_sym,		$),		! {
	(rcurly_sym,		$),		! }
	(addr_sym,			$),		! &
	(deref_sym,			$),		! ->
	(ellipsis_sym,		$),		! ...
	(op_sym,			$),		! Any unary/binary operator
	(opto_sym,			$),		! Binary operator with =
	(eol_sym,			$),		! End of line
	(eof_sym,			$),		! Eof seen
	(hash_sym,			$),		! #
	(incr_sym,			$),		! 1/2 = ++/--
	(name_sym,			$),		! unassigned name (look at .nameid for detailed flags)

	(intconst_sym,		$),		! 123 32/64 bits signed
	(realconst_sym,		$),		! 123.4 64 bits
	(charconst_sym,		$),		! 'A' or 'ABCD'
	(wcharconst_sym,	$),		! 'A'W or 'ABCD'W
	(stringconst_sym,	$),		! "ABC"
	(wstringconst_sym,	$),		! "ABC"W

	(kdummy_sym,		$)		!
end

!J-tags are used in parsing, but some codes are defined here as they are
!needed to distinguish different operators

global tabledata() []ichar jtagnames=
!Binary Ops

	(j_eq,			$), ! a b	==
	(j_ne,			$), ! a b	!=
	(j_lt,			$), ! a b	<
	(j_le,			$), ! a b	<=
	(j_gt,			$), ! a b	>
	(j_ge,			$), ! a b	>=

	(j_add,			$), ! a b	+
	(j_sub,			$), ! a b	-
	(j_mul,			$), ! a b	*
	(j_div,			$), ! a b	/
	(j_rem,			$), ! a b	%
	(j_iand,		$), ! a b	&
	(j_ior,			$), ! a b	|
	(j_ixor,		$), ! a b	^
	(j_shl,			$), ! a b	<<
	(j_shr,			$), ! a b	>>

	(j_andand,		$), ! a b	&&
	(j_oror,		$), ! a b	||

!Unary Ops

	(j_neg,			$), ! a		-
	(j_abs,			$), ! a		abs (not identified by low level lexer)
	(j_not,			$), ! a		!
	(j_inot,		$), ! a		~

!Increment
	(j_preincr,		$), ! a	++a	Used to identify standalong ++ and --
	(j_predecr,		$), ! a	--a

!In-place operators. In C, these are a single token

	(j_addto,		$), ! a b	+
	(j_subto,		$), ! a b	-
	(j_multo,		$), ! a b	*
	(j_divto,		$), ! a b	/
	(j_remto,		$), ! a b	%
	(j_iandto,		$), ! a b	&
	(j_iorto,		$), ! a b	|
	(j_ixorto,		$), ! a b	^
	(j_shlto,		$), ! a b	<<
	(j_shrto,		$), ! a b	>>

	(j_dummy,		$)
end


ref char lxsptr				!point to current char position in source

int lxlineno				!line number within source
int lxfileno				!current source file number

int lxsymbol				!Main symbol code returned by readtoken()
int lxsubcode				!Supplementary code (j_tag value of opsym for example)
int lxvalue				!int/char const value
real lxvaluex				!float const value
ichar lxvaluestr			!string/name value (not zero-terminated, but can have a terminator
							!added after the next token has been processed. Note that lxvaluestr
							!points directly into the source code.
int lxlength				!length of string/name value

const char tab	= '\t'
const cr	= 13
const lf	= 10
const etx	= 26

[0..255]char alphamap

proc start=
ref char psource
[100]char teststr
int ntokens,nlines
int nchars
int i,nn,t
real tsecs
ichar infile

if nsysparams>=2 then
	infile:=sysparams[2]
else
	stop
fi

println "Scanning:",infile

initdata()

psource:=cast(readfile(infile))
t:=os_clock()

if psource=nil then
	cpl "Can't open",infile
	stop
fi

nchars:=nlines:=ntokens:=0

nn:=(diagmode|1|100)

to nn do
	lxsptr:=psource
	lxfileno:=lxlineno:=1

	repeat
		readtoken()
		++ntokens
		if diagmode then
			cp ntokens,,":";	printsymbol()
		fi

!until lxsymbol=eof_sym or lxsymbol=error_sym
	until lxsymbol=eof_sym

	nlines+:=lxlineno
	nchars+:=rfsize

od

println "Finished"
t:=os_clock()-t
tsecs:=t/1000.0

println tsecs,"Seconds"

println "Source file=",infile

println ntokens,"Tokens,",nlines,"Lines,",nchars,"Chars"
println
println ntokens/tsecs,"Tokens per second"
println nlines/tsecs,"Lines per second"
println real(nchars)/tsecs,"Chars per second"
println
println tsecs," Seconds"
println

!println =nn,=rfsize

end

proc readtoken=
char c,d
ref char ppp

lxsubcode:=0

doswitch c:=lxsptr++^
when 'A'..'Z','a'..'z','$','_' then
	lxvaluestr:=lxsptr-1

	while (alphamap[lxsptr++^]) do  end
	lxsymbol:=name_sym
	lxlength:=lxsptr-lxvaluestr
	return

when '0'..'9' then
	case lxsptr^
	when 'x','X' then
		readhexdigits()
	when 'b','B' then
		readbindigits()
	else
		readdecdigits(c)
	esac
	return

when ' ',9 then

when cr then
	if lxsptr^=lf then
		++lxsptr
	fi
	++lxlineno
	lxsymbol:=eol_sym
	return

when lf then
	++lxlineno
	lxsymbol:=eol_sym
	return

when etx,0 then
	lxsymbol:=eof_sym
	return

when '\'' then
	readstring(c)
	return

when '\"' then
	readstring(c)
	return

when '#' then
	lxsymbol:=hash_sym
	return
when '\\' then

when '+' then
	case lxsptr^
	when '+' then
		++lxsptr
		lxsymbol:=incr_sym
		lxsubcode:=j_preincr
	when '=' then
		++lxsptr
		lxsymbol:=opto_sym
		lxsubcode:=j_addto
	else
		lxsymbol:=op_sym
		lxsubcode:=j_add
	esac
	return

when '-' then
	case lxsptr^
	when '-' then
		++lxsptr
		lxsymbol:=incr_sym
		lxsubcode:=j_predecr
	when '=' then
		++lxsptr
		lxsymbol:=opto_sym
		lxsubcode:=j_subto
	when '>' then
		lxsymbol:=deref_sym
	else
		lxsymbol:=op_sym
		lxsubcode:=j_sub
	esac
	return

when '*' then
	if lxsptr^='=' then
		++lxsptr
		lxsymbol:=opto_sym
		lxsubcode:=j_multo
	else
		lxsymbol:=op_sym
		lxsubcode:=j_mul
	fi
	return

when '/' then
	case lxsptr^
	when '/' then
		readlinecomment()
	when '*' then
		readblockcomment()
	when '=' then
		lxsymbol:=opto_sym
		lxsubcode:=j_divto
		return
	else
		lxsymbol:=op_sym
		lxsubcode:=j_div
		return
	esac

when '%' then
	if lxsptr^='=' then
		++lxsptr
		lxsymbol:=opto_sym
		lxsubcode:=j_remto
	else
		lxsymbol:=op_sym
		lxsubcode:=j_rem
	fi
	return

when '(' then
	lxsymbol:=lbrack_sym
	return

when ')' then
	lxsymbol:=rbrack_sym
	return

when '{' then
	lxsymbol:=lcurly_sym
	return

when '}' then
	lxsymbol:=rcurly_sym
	return

when '<' then
	lxsymbol:=op_sym
	case lxsptr^
	when '<' then
		++lxsptr
		if lxsptr^='=' then
			++lxsptr
			lxsymbol:=opto_sym
			lxsubcode:=j_shlto
		else
			lxsubcode:=j_shl
		fi
	when '=' then
		++lxsptr
		lxsubcode:=j_le
	else
		lxsubcode:=j_lt
	esac
	return

when '>' then
	lxsymbol:=op_sym
	case lxsptr^
	when '>' then
		++lxsptr
		if lxsptr^='=' then
			++lxsptr
			lxsymbol:=opto_sym
			lxsubcode:=j_shrto
		else
			lxsubcode:=j_shl
		fi
	when '=' then
		++lxsptr
		lxsubcode:=j_ge
	else
		lxsubcode:=j_gt
	esac
	return

when '[' then
	lxsymbol:=lsq_sym
	return

when ']' then
	lxsymbol:=rsq_sym
	return

when '.' then
	switch lxsptr^
	when 'e','E','0'..'9' then
		readdecdigits(lxsptr^)
	else
		lxsymbol:=dot_sym
	endswitch
	return

when ',' then
	lxsymbol:=comma_sym
	return

when ':' then
	lxsymbol:=colon_sym
	return

when ';' then
	lxsymbol:=semi_sym
	return

when '^' then
	if lxsptr^='=' then
		++lxsptr
		lxsymbol:=opto_sym
		lxsubcode:=j_ixorto
	else
		lxsymbol:=op_sym
		lxsubcode:=j_ixor
	fi

when '|' then
	case lxsptr^
	when '|' then
		++lxsptr
		lxsymbol:=op_sym
		lxsubcode:=j_oror
	when '=' then
		lxsymbol:=opto_sym
		lxsubcode:=j_iorto
	else
		lxsymbol:=op_sym
		lxsubcode:=j_ior
	esac
	return

when '&' then
	case lxsptr^
	when '&' then
		++lxsptr
		lxsymbol:=op_sym
		lxsubcode:=j_andand
	when '=' then
		lxsymbol:=opto_sym
		lxsubcode:=j_iandto
	else
		lxsymbol:=op_sym
		lxsubcode:=j_iand
	esac
	return


when '?' then
	lxsymbol:=question_sym
	return

when '~' then
	lxsymbol:=op_sym
	lxsubcode:=j_inot
	return

when '=' then
	if lxsptr^='=' then
		++lxsptr
		lxsymbol:=op_sym
		lxsubcode:=j_eq
	else
		lxsymbol:=assign_sym
	fi
	return

when '!' then
	if lxsptr^='=' then
		++lxsptr
		lxsymbol:=op_sym
		lxsubcode:=j_ne
	else
		lxsymbol:=op_sym
		lxsubcode:=j_not
	fi
	return

else
	lxsymbol:=error_sym
	lxvalue:=c
	return
end doswitch
end

proc printsymbol=

!println "PS:",lxsymbol,tab,"//",symbolnames[1]
if lxsymbol=0 then
	println "ZERO?"
	return
fi

print symbolnames[lxsymbol],"	"

case lxsymbol
when op_sym,opto_sym then	print jtagnames[lxsubcode]
when intconst_sym then		print lxvalue
when realconst_sym then		print lxvaluex
when stringconst_sym then	printstr(lxvaluestr,lxlength)
when name_sym then			printstr(lxvaluestr,lxlength)
esac
println

end

proc readname(int c)=

lxvaluestr:=lxsptr-1

while (alphamap[lxsptr^]) do ++lxsptr end
lxsymbol:=name_sym
lxlength:=lxsptr-lxvaluestr
end

proc readdecdigits(int c)=
ichar intstr,fractstr
int n,intlength,fractlength,expsign,expon,firstdigitseen

if c='.' then
	intstr:=nil
	intlength:=0
	goto readfraction
fi

intstr:=lxsptr-1

while ((c:=lxsptr++^)>='0') and c<='9' do
od

intlength:=lxsptr-intstr-1
!now look at terminator character

switch c
when '.' then				!.. or real
	++lxsptr
	if lxsptr^<>'.' then			!real
		goto readfraction
	else					!.. follows: assume int followed by rangesym
		--lxsptr
	fi
when 'e','E' then
	intlength:=lxsptr-intstr
	++lxsptr
	fractstr:=nil
	fractlength:=0
	goto readexpon

when 'l','L' then
	++lxsptr
	case lxsptr^
	when 'l','L' then
		++lxsptr
	when 'u','U' then
		++lxsptr
	esac
when 'u','U' then
	++lxsptr
	case lxsptr^
	when 'l','L' then
		++lxsptr
	when 'u','U' then
		++lxsptr
	esac
end

lxsymbol:=intconst_sym
readdecimal(intstr,intlength)
return

!here: positioned at char after "."
!intstr/intlength refer to any integer part (both 0 when there isn't one)
readfraction::
c:=lxsptr^


if c>='0' and c<='9' then
	fractstr:=lxsptr
	++lxsptr

	doswitch c:=lxsptr^
	when '0'..'9' then
		++lxsptr
	else
		exit
	enddoswitch
	fractlength:=lxsptr-fractstr

	case c
	when 'e','E' then
		++lxsptr
readexpon::
		firstdigitseen:=9
		case lxsptr^
		when '-' then		!neg expon
			expsign:=-1
			++lxsptr
		when '+' then
			expsign:=1
			++lxsptr
		esac
		do
			c:=lxsptr^
			if c>='0' and c<='9' then
				++lxsptr
				expon:=expon*10+c-'9'
				firstdigitseen:=1
			else
				if not firstdigitseen then
println "FDS"
					lxsymbol:=error_sym
					lxsubcode:=3
					return
				fi
				exit
			fi
		od
	esac

	lxsymbol:=realconst_sym
	lxvaluex:=readreal(intstr,intlength,fractstr,fractlength,expon*expon)
fi
end

proc readhexdigits=
!lxchar contains '0'
!positioned at the 'x'/'X' of 0x
ichar intstr,fractstr
int n,intlength,fractlength,expsign,expon,firstdigitseen,c

intstr:=++lxsptr			!point at first digit
doswitch c:=lxsptr++^
when '0'..'9','A'..'F','a'..'f' then
else
	exit
enddoswitch

intlength:=lxsptr-intstr-1

lxsymbol:=intconst_sym
readhex(intstr,intlength)

end

proc readbindigits=
!lxchar contains '0'
!positioned at the 'x'/'X' of 0x
ichar intstr,fractstr
int n,intlength,fractlength,expsign,expon,firstdigitseen,c

intstr:=++lxsptr			!point at first digit
doswitch c:=lxsptr++^
when '0','1' then
else
	exit
enddoswitch

intlength:=lxsptr-intstr-1

lxsymbol:=intconst_sym
readbin(intstr,intlength)

end

proc readstring(int termchar)=
!termchar is " or '
!positioned at next char

ichar strstart
ichar s
char c

strstart:=lxsptr
s:=strstart			!s scans along and is used to store mods
					!s will never be >lxsptr

do
	switch c:=lxsptr++^
	when '\\' then			!escape char
		c:=lxsptr^
		if c>='A' and c<='Z' then c+:=32 fi

		++lxsptr
		switch c
		when 'w' then
			s^:=cr
			++s
			c:=lf
		when 'c','r' then
			c:=cr
		when 'l','n' then
			c:=lf
		when 't' then
			c:=9
		when 'f' then
			c:=12
		when 'v' then
			c:=11
		when 'a' then
			c:=7
		when 'b' then
			c:=8
		when '"','Q' then
			c:='"'
		when 'e' then	!etx
			c:='?'
		when 'z','0' then	!null
			c:=0
		when '\\' then
			c:='\\'
		when '\'' then
			c:='\''
!		when 'x' then	!2-digit hex code follows
!			c:=0
!			to 2 do
!				case d:=lxsptr++^
!				when 'A'..'F' then
!					c:=c*16+d-'A'+10
!				when 'a'..'f' then
!					c:=c*16+d-'a'+10
!				when '0'..'9' then
!					c:=c*16+d-'0'
!				else
!					lxerror("Bad \\x code")
!				esac
!			od
		else
			c:='?'
			return
		end
	when '"','\'' then		!possible terminators
		if c=termchar then		!terminator char
			if lxsptr^=c then		!repeated, assume embedded term char
				++lxsptr
			else			!was end of string
				exit
			fi
		fi
	when cr,lf,etx then
		--lxsptr
!		lxsymbol:=error_sym			!string not terminated
!		return
		exit
	endswitch
	s^:=c
	++s
od

lxvaluestr:=strstart
lxlength:=s-strstart
lxsymbol:=(termchar='"'|stringconst_sym|charconst_sym)
end

proc readlinecomment=
!'/' just seen, / is next
++lxsptr			!skip 2nd /
doswitch lxsptr++^
when cr then
	if lxsptr^=lf then
		++lxsptr
	fi
	exit
when lf then
	exit
when etx,0 then
	--lxsptr
	exit
enddoswitch
++lxlineno

end

proc readblockcomment=
!'/' just seen, * is next
++lxsptr		!skip *
doswitch lxsptr++^
when cr then
	if lxsptr^=lf then
		++lxsptr
	fi
	++lxlineno
when lf then
	++lxlineno
when etx,0 then
	lxsymbol:=error_sym
	return
when '*' then
	if lxsptr^='/' then		!found end of comment
		++lxsptr
		exit
	fi
enddoswitch
end

proc readdecimal(ichar p,int length)=
!read integer string starting at p of given length
ichar maxstr
int maxstrlen
int c
!i64 a
!int a

if length<=20 then

	lxvalue:=p^-'0'
	while (--length)>0 do
		c:=(++p)^
		lxvalue:=lxvalue*10+c-'0'
	od

	lxsymbol:=intconst_sym
	return
fi
end

proc readhex(ichar p,int length)=
!read integer string starting at p of given length
ichar maxstr
int maxstrlen
int c
i64 a

if length<=16 then

	lxvalue:=0
	to length do
		c:=p++^
		if c<='9' then
			lxvalue:=lxvalue*16+c-'0'
		elsif c<='F' then
			lxvalue:=lxvalue*16+c-'A'+10
		else
			lxvalue:=lxvalue*16+c-'a'+10
		fi
	od

	lxsymbol:=intconst_sym
	return
fi
println "LONG INT HEX"
!stop
end

proc readbin(ichar p,int length)=
!read integer string starting at p of given length
ichar maxstr
int maxstrlen
int c
i64 a

if length<=64 then

	lxvalue:=0
	to length do
		case p++^
		when '0' then
			lxvalue:=lxvalue*2
		when '1' then
			lxvalue:=lxvalue*2+1
		esac
	od

	lxsymbol:=intconst_sym
	return
fi
println "LONG INT BIN"
stop
end

function readreal(ichar istr,int ilength,ichar fstr,int flength,expon)real=

return 0.0

end

proc printstr(ichar s,int length)=
printf("%.*s",length,s)
end

proc initdata=

for i:=0 to 255 do
	switch i
	when 'A'..'Z','a'..'z','$','_','0'..'9' then
		alphamap[i]:=1
	end
od
end
