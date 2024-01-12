const etx	= 26
const cr	= 13
const lf	= 10
const tab	= 9

ref char lxsource		!start of module
ref char lxstart		!start of this token
ref char lxsptr
int lxifcond
int longsuffix			!for real nos
int lxfileno

const hstsize	= 32768
!const hstsize	= 65536
const hstmask	= hstsize-1

int nextlxlength
global int lxlength

global [0:hstsize]strec hashtable
symbol hashtablelast

ichar u64maxstr="18446744073709551615"

[0..255]byte namemap			!0/1/2 = other/name/name-upper

global proc lexreadtoken=
!read next token into nextlx
int c,csum,hsum,commentseen
ref char pstart,pnext,p,ss

	nextlx.subcode:=0

	doswitch lxstart:=lxsptr; lxsptr++^
	when 'a'..'e','g'..'z','$','_' then
	dolower:
		nextlx.svalue:=lxsptr-1
	doname:
		hsum:=nextlx.svalue^

		docase namemap[c:=lxsptr++^]
		when 1 then
			hsum:=hsum<<4-hsum+c
		when 2 then
			(lxsptr-1)^:=c+' '
			hsum:=hsum<<4-hsum+c+' '
		else
			--lxsptr
			exit
		end docase

		lookup(nextlx.svalue, lxsptr-nextlx.svalue, (hsum<<5-hsum) iand hstmask)

		return

	when 'A'..'E','G'..'Z' then
	doupper:
		nextlx.svalue:=lxsptr-1
		nextlx.svalue^+:=32
		goto doname

	when 'f' then
		if lxsptr^<>'"' then
			goto dolower
		fi
		readrawstring()
		return

	when 'F' then
		if lxsptr^<>'"' then
			goto doupper
		fi
		readrawstring()
		return

	when '0'..'9' then
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

	when '!', '#' then			!comment to eol
	docomment:

		docase c:=lxsptr++^
		when 13 then
			++lxsptr
			exit
		when 10 then
			exit
		when etx,0 then
			--lxsptr
			exit
		end
		nextlx.symbol:=eolsym
		return

	when '\\' then			!line continuation

!two stages:
! 1: read chars until any eol chars (unless further '\' seen)
! 2: read until non-white space
		commentseen:=0
		docase lxsptr++^			!read until end of this line
		when cr then
			++lxsptr				!skip lf
			++nalllines
			exit
		when lf then
!		++nextlx.pos
			++nalllines
			exit
		when etx,0 then
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
		end
!eol seen: now skip 0 or more further eol chars, plus any white space (ie. multiple blank lines)

		docase lxsptr++^
		when cr then
			++lxsptr				!skip lf
			++nalllines
		when lf then
			++nalllines
		when ' ',tab then
		else
			--lxsptr
			exit
		end

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
			readreal()
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
			nextlx.subcode:=jassign			!helps treat as opsym which all have k-code as subcode
		when ':' then
			++lxsptr
			case lxsptr^
			when '=' then
				++lxsptr
				nextlx.symbol:=deepcopysym
				nextlx.subcode:=jdeepcopy
			else
				nextlx.symbol:=dcolonsym
			esac
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
		nextlx.symbol:=barsym
		return

	when '^' then
		nextlx.symbol:=ptrsym
		nextlx.subcode:=jptrto
		return

	when '@' then
		nextlx.symbol:=atsym
		return

	when '?' then
		nextlx.symbol:=questionsym
		return

!	when '~' then
!		nextlx.symbol:=curlsym
!		return

	when '+' then
		nextlx.symbol:=addsym
		if lxsptr^='+' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=jincrload
			return
		else
			nextlx.subcode:=jadd
		fi
		return

	when '-' then
		nextlx.symbol:=subsym
		case lxsptr^
		when '-' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=jdecrload
			return
		when '>' then
			++lxsptr
			nextlx.symbol:=pipesym
		else
			nextlx.subcode:=jsub
		esac
		return

	when '*' then
		nextlx.symbol:=mulsym
		if lxsptr^='*' then
			++lxsptr
			nextlx.symbol:=powersym
			nextlx.subcode:=jpower
		else
			nextlx.subcode:=jmul
		fi
		return

	when '/' then
		nextlx.symbol:=divsym
		nextlx.subcode:=jdiv
		return

	when '%' then
		nextlx.symbol:=idivsym
		nextlx.subcode:=jidiv
		return

	when '=' then
		case lxsptr^
		when '>' then
			nextlx.symbol:=sendtosym
			++lxsptr
		when '=' then
			nextlx.symbol:=isequalsym
			nextlx.subcode:=jisequal

			++lxsptr
		else
			nextlx.symbol:=eqsym
			nextlx.subcode:=jeq
		esac
		return

	when '<' then
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=lesym
			nextlx.subcode:=jle
		when '>' then
			++lxsptr
			nextlx.symbol:=nesym
			nextlx.subcode:=jne
		when '<' then
			++lxsptr
			nextlx.symbol:=shlsym
			nextlx.subcode:=jshl
		else
			nextlx.symbol:=ltsym
			nextlx.subcode:=jlt
		esac
		return

	when '>' then
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=gesym
			nextlx.subcode:=jge
		when '>' then
			++lxsptr
			nextlx.symbol:=shrsym
			nextlx.subcode:=jshr
		else
			nextlx.symbol:=gtsym
			nextlx.subcode:=jgt
		esac
		return

	when '&' then
		case lxsptr^
		when '&' then
			++lxsptr
			nextlx.symbol:=daddrsym
			nextlx.subcode:=jconcat
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
		++nalllines
		nextlx.symbol:=eolsym
		return
	when lf then			!only lfs not preceded by cr
		nextlx.symbol:=eolsym
		++nalllines
		return

	when etx,0 then
		nextlx.symbol:=eofsym
		--lxsptr
		return

	else
		nextlx.symbol:=errorsym
		nextlx.value:=c
		return

	end doswitch
!end switch
!od

end

proc lxreadstring(int termchar)=
!read string inplace: new string, with expanded control characters,
!is stored on top of original string in the source
!new string is same length or shorter
!on entry, lxsptr points to char after " or '

	ichar dest,pstart
	int c,d,length,hasescape
	[8]char str

	if termchar='"' then
		nextlx.symbol:=stringconstsym
	else
		nextlx.symbol:=charconstsym
		nextlx.subcode:=tint
	fi

	pstart:=lxsptr

	length:=0
	hasescape:=0

	docase c:=lxsptr++^
	when '\\' then			!escape char
		c:=lxsptr^
		if c in 'A'..'Z' then c+:=' ' fi
		++lxsptr
		hasescape:=1

		case c
		when 'w' then
			length+:=2
		when 'x' then	!2-digit hex code follows
			lxsptr+:=2
			++length
		else				!assume valid escape (bad ones detected in next pass)
			++length
		esac

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
	end docase

	nextlxlength:=length

	if length=0 then
		nextlx.svalue:=""
		return
	elsif not hasescape then
		nextlx.svalue:=pcm_copyheapstringn(pstart,length)
		return
	fi

	nextlx.svalue:=dest:=pcm_alloc(length+1)

	do
		case c:=pstart++^
		when '\\' then			!escape char
			c:=pstart^
			if c>='A'  and c<='Z' then c+:=' ' fi
			++pstart
			switch c
			when 'a' then			!bell ('alert')
				c:=7
			when 'b' then			!backspace
				c:=8
			when 'c','r' then		!carriage return
					c:=cr
			when 'e' then			!end-of-text
CPL "<E> ETX SEEN"
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
				dest++^:=cr
				c:=lf
			when 'x' then	!2-digit hex code follows
				c:=0
				to 2 do
					if (d:=pstart++^) in 'A'..'F' then
						c:=c*16+d-'A'+10
					elsif d in 'a'..'f' then
						c:=c*16+d-'a'+10
					elsif d in '0'..'9' then
						c:=c*16+d-'0'
					else
						lxerror("Bad \\x code")
					fi
				od
			when 'y' then			!CCI/SM backwards tab
				c:=16
			when 'z' then		!null (not fully supported in code)
				c:=0
			elsecase c
			when '"','Q' then		!embedded double quote
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
				if pstart^=c then		!repeated, assume embedded term char
					++pstart
				else			!was end of string
					exit
				fi
			fi
		when cr,lf,etx,0 then
			lxerror("String not terminated")
		esac

		dest++^:=c
	od
	(nextlx.svalue+nextlxlength)^:=0
end

global proc lexinit=
!do one-time setup:
! clear the hash table and populated it with reserved words
! do maxnum support and such
	int i!,n
	static int n

	memset(&hashtable,0,hashtable.bytes)
	hashtablelast:=&hashtable[hstsize-1]

	inithashtable()
end

proc readrawstring=
!positioned at " of F"
!read raw string
	ichar pstart
	int length

	nextlx.symbol:=stringconstsym

	pstart:=++lxsptr
	length:=0

	docase lxsptr++^
	when '"' then
		exit
	when cr,lf,0 then
		lxerror("Raw string not terminated")
		--lxsptr
		exit
	else
		++length
	end

	nextlxlength:=length

	nextlx.svalue:=pcm_copyheapstringn(pstart,length)
end

global function lookup(ichar name, int length, hashindex)int=
!lookup rawnamesym with details in nextlx
!hash value already worked out in nextlxhashvalue
!return 1 (found) or 0 (not found)
!in either case, nextlx.symptr set to entry where name was found, or will be stored in
	int j,wrapped,n
	symbol d
	ref char s

	d:=&hashtable[hashindex]
	wrapped:=0

	do
		if (n:=d.namelen)=length and memcmp(d.name,name,n)=0 then	!match
			nextlx.symptr:=d
			nextlx.symbol:=d.symbolcode
			nextlx.subcode:=d.subcode
			return 1
		elsif n=0 then
			exit
		fi

		if ++d>hashtablelast then
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
	d.symbolcode:=namesym

	nextlx.symptr:=d
	nextlx.symbol:=d.symbolcode
	nextlx.subcode:=d.subcode

	return 0
end

global function gethashvaluez(ichar s)int=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!ASSUMES S is lower-case, as conversion not done
	int c,hsum

	if s^=0 then return 0 fi

	hsum:=s++^

	do
		c:=s++^
		exit when c=0
		hsum:=hsum<<4-hsum + c
	od
	return (hsum<<5-hsum) iand hstmask
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

proc inithashtable=
!populate hashtable with standard symbols
	int i
	ichar name

	for i:=1 to stnames.len do
		addstname(stnames[i], stsymbols[i], stsubcodes[i])
	od

	for i to hostfnnames.upb when not hostinternal[i] do
		name:=hostfnnames[i]+2				!skip 'h_'
		addstname(name, khostfnsym, i)

	od
end

proc addstname(ichar name, int symbol, subcode)=
	if lookup(name,strlen(name),gethashvaluez(name)) then
		println name
		abortprogram("Dupl ST entry")
	fi

	nextlx.symptr.symbolcode:=symbol
	nextlx.symptr.subcode:=subcode
end

global proc startlex(ifile pm)=
!	if not fwriteqa then
!		lxsource:=lxsptr:=pm.text
!	else
		lxsource:=lxsptr:=pcm_copyheapstring(pm.text)
!	fi
	lxfileno:=pm.moduleno

	nextlx.symbol:=semisym
	nextlx.subcode:=0
	nextlx.moduleno:=pm.moduleno
end

global function addnamestr(ichar name)symbol=
	lexrec oldlx
	symbol symptr

	oldlx:=nextlx

	nextlxlength:=strlen(name)
	nextlx.svalue:=pcm_alloc(nextlxlength+1)
	memcpy(nextlx.svalue,name,nextlxlength+1)
	lookup(nextlx.svalue, nextlxlength, gethashvaluez(name))
	symptr:=nextlx.symptr

	nextlx:=oldlx

	return symptr
end

global proc ps(ichar caption)=
	print caption,,":::"
	printsymbol(&lx)
end

global proc psnext(ichar caption)=
	print "	",,caption,,":##"
	printsymbol(&nextlx)
end

global proc lex=
!return next token in lx, using lexreadtoken but working a token ahead.
!static int lastline=0
	int lineno,n,dir,namelen
	ref char p
	symbol symptr

	lx:=nextlx				!grab that already read basic token

	lxlength:=nextlxlength
	lx.sourceoffset:=lxstart-lxsource

	reenter:

	lexreadtoken()			!read new token for next time around
	reenter2:

	case nextlx.symbol
	when unitnamesym then					!might be user identifier (points to generic entry)
		case lx.symbol
		when intconstsym then
			case nextlx.symptr.subcode
			when million_unit then lx.value *:= 1 million
			when billion_unit then lx.value *:= 1 billion
			when thousand_unit then lx.value *:= 1000
			else
				lxerror("Can't do this unit index")
			esac
			lx.subcode:=tint
			goto reenter
		when realconstsym then
			lxerror("unit symbol after float?")
		else
			nextlx.symbol:=namesym				!convert to actual identifier
		esac

	when sysconstsym then					!ST ENTRY LIMITED TO 16 bits signed
		case nextlx.subcode
		when con_const then
			nextlx.symbol:=intconstsym
			nextlx.value:=0
			nextlx.subcode:=tint
		when pi_const then
			nextlx.symbol:=realconstsym
			nextlx.xvalue:=pi
			nextlx.subcode:=treal
		when tab_const then
			nextlx.symbol:=stringconstsym
			nextlx.svalue:="\t"
			nextlxlength:=1
		when true_const then
			nextlx.symbol:=intconstsym
			nextlx.value:=1
			nextlx.subcode:=tint
		when false_const then
			nextlx.symbol:=intconstsym
			nextlx.value:=0
			nextlx.subcode:=tint

		else
			lxerror("sysconst?")
		esac

	when eolsym then

		case lx.symbol
		when commasym, lsqsym, lbracksym, !ignore eol
			 assignsym,semisym then

			lexreadtoken()
			goto reenter2

			goto reenter
		elsif binopset[lx.symbol] and 	lx.symbol not in [maxsym, minsym] then
			lexreadtoken()
			goto reenter2

		esac
		nextlx.symbol:=semisym

	when insym then
		if lx.symbol=notlsym then
			lx.symbol:=notinsym
			lx.subcode:=jnotin
			goto reenter
		fi
	esac

end

global proc lxerror_s(ichar mess,a)=
	[256]char str
	fprint @str,mess,a
	lxerror(&.str)
end

proc makedecimal(ichar s, int length,base)=
!create a decimal number token

	if base<>10 then
		LXERROR("MAKEDECIMAL/16/2")
	fi

	nextlx.symbol:=decimalconstsym
	nextlx.subcode:=tdecimal
	nextlx.svalue:=pcm_copyheapstringn(s,length)
	nextlxlength:=length
end

proc readdec=
	int c
	ref char dest, destend, pstart
	int islong, length
	[1024]byte str
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
			dest^:=0
			makedecimal(&.str,dest-&.str,10)
			return

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

	if length>20 or length=20 and strncmp(&.str,u64maxstr,20)>0 then
		makedecimal(&.str,length,10)
		return
	fi

finish:
	nextlx.symbol:=intconstsym
	nextlx.subcode:=tint
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
			dest^:=0
			makedecimal(&.str,dest-&.str,16)
			return

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
		makedecimal(&.str,length,16)
		return
	fi

	nextlx.symbol:=intconstsym
	nextlx.subcode:=tint
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
			dest^:=0
			makedecimal(&.str,dest-&.str,2)
			return

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
		makedecimal(&.str,length,2)
		return
	fi

	nextlx.symbol:=intconstsym
	nextlx.subcode:=tint
	nextlx.value:=a
end

proc readreal=
!at '.', or had been in middle of int where . or e were seen, back at the start

	int c,n,negexpon,dotseen,length, fractlen, expon, expseen
	real x
	[1024]char str
	ichar dest, destend, pexpon

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
					makedecimal(&.str,dest-&.str,10)
					return
				else
					--lxsptr
					exit all
				fi
			end

		when '_','\'' then

		when 'l','L' then
			makedecimal(&.str,dest-&.str,10)
			return
		else
			--lxsptr
			exit
		fi

		if dest>=destend then lxerror("r64lit too long") fi
	end
	dest^:=0

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
end

proc readrawxname=
	int c,hsum,length

	nextlx.svalue:=lxsptr
	hsum:=0

	while namemap[c:=lxsptr++^] do
		hsum:=hsum<<4-hsum+c
	od
	--lxsptr

	lookup(nextlx.svalue, lxsptr-nextlx.svalue, (hsum<<5-hsum) iand hstmask)

	return
end
