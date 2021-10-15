!Tokeniser Module
import* aa_common

macro testmode=0

const etx = 26
const cr  = 13
const lf  = 10

!the following returned by updated by lexreadtoken()

global int lxsymbol		!* main symbol kind
global int lxsubcode	!* for some symbols, which specific keyword

global int64 lxvalue
global real64 lxxvalue
global ichar lxsvalue
global int lxlength
int lxhashvalue

global ref byte lxsptr		!@ points to next char in source
ref byte lxstart		!@ start of source code for this file
global ref strec lxsymptr		!set by lookuplex()

[0..255]char alphamap
[0..255]char digitmap
[0..255]char commentmap

global proc lex=
!lowest level lex() function, reads names, numbers etc but does no lookups or translations
!returns results in lx-vars. Current source pointer should be in lxsptr
	int i, c, d, hsum, length
	ref byte pstart

	lxsubcode:=0

	doswitch c:=lxsptr++^
	when 'a'..'z','$','_','.' then
		pstart:=lxsptr-1		!point to start of name in source buffer
	doname::
		hsum:=pstart^

		doswitch c:=lxsptr++^
		when 'a'..'z','0'..'9','_','$','.' then
			hsum:=hsum<<4-hsum+c
		when 'A'..'Z' then
			(lxsptr-1)^:=c+32
			hsum:=hsum<<4-hsum+c+' '
		else
			--lxsptr
			exit
		end

		lxlength:=lxsptr-pstart
		lxhashvalue:=hsum<<5 -hsum

		if lookuplex(cast(pstart),lxlength) then
			if lxsymptr.ksymbol then			!keywords take priority here
				lxsymbol:=lxsymptr.ksymbol
				lxsubcode:=lxsymptr.subcode
			else
				lxsymbol:=lxsymptr.symbol
			fi
		else
			lxsymbol:=namesym
		fi

		return

	when 'A'..'Z' then
		pstart:=lxsptr-1
		c:=pstart^:=pstart^+32
		goto doname

	when '0'..'9' then
		readnumber(c)
		return

	when '`' then
		pstart:=lxsptr		!point to start of name in source buffer
		hsum:=0

		doswitch c:=lxsptr^
		when 'A'..'Z','a'..'z','0'..'9','_','$','.' then
			++lxsptr
			hsum:=hsum<<4-hsum+c
		else
			exit
		end

		lxsymbol:=namesym
		if pstart=lxsptr then
			lxerror("NULL ` name")
		fi
		lxlength:=lxsptr-pstart
		lxhashvalue:=hsum<<5-hsum

		if lookuplex(cast(pstart),lxlength) then
			lxsymbol:=lxsymptr.symbol			!can't be a keyword
			if lxsymbol=0 then					!assume was a keyword; use as name
				lxsymbol:=lxsymptr.symbol:=namesym
			fi
		fi
		return

	when '!',';','#' then			!comment to eol

		while commentmap[lxsptr++^] do od

		if (lxsptr-1)^=0 then --lxsptr fi
!
		++lxlineno

		lxsymbol:=eolsym
		return

	when ',' then
		lxsymbol:=commasym
		return

	when ':' then
		if lxsptr^=':' then
			lxsymbol:=dcolonsym
			++lxsptr
		else
			lxsymbol:=colonsym
		fi
		return

	when '[' then
		lxsymbol:=lsqsym
		return

	when ']' then
		lxsymbol:=rsqsym
		return

	when '+' then
		lxsymbol:=addsym
		return

	when '-' then
		lxsymbol:=subsym
		return

	when '*' then
		lxsymbol:=mulsym
		return

	when '=' then
		lxsymbol:=eqsym
		return

	when '\'' then
		pstart:=lxsptr

		do
			switch lxsptr++^
			when '\'' then
				exit
			when cr,lf then
				lxerror("String not terminated")
			endswitch
		od
		length:=lxsptr-pstart-1
		lxvalue:=0
		for i:=length downto 1 do
			lxvalue:=lxvalue<<8+(pstart+i-1)^
		od
		lxsymbol:=intconstsym
		return

	when '"' then
		pstart:=lxsptr

		do
			switch lxsptr++^
			when '"' then
				lxsvalue:=cast(pstart)
				lxlength:=lxsptr-pstart-1
				(lxsvalue+lxlength)^:=0
				lxsymbol:=stringconstsym
				return
			when cr,lf,etx,0 then
				lxerror("String not terminated")
			endswitch
		od

	when ' ',9 then

	when cr then			!lf expected to follow

	when lf then
		++lxlineno
		lxsymbol:=eolsym
		return

	when 0,etx then
		lxsymbol:=eofsym
		--lxsptr
		return
	else
		lxsymbol:=errorsym
		lxvalue:=c
		return

	end doswitch
end

global proc initlex=
	lxsubcode:=0
	lxsymbol:=errorsym

	lxlineno:=0

	int i
	for i:=0 to 255 do
		switch i
		when 'A'..'Z','a'..'z','$','_','0'..'9' then
			alphamap[i]:=1
		end
		switch i
		when '0'..'9' then
			digitmap[i]:=1
		end
		commentmap[i]:=1
	od

	commentmap[0]:=0
	commentmap[lf]:=0

	inithashtable()
end

proc readreal(ref[]char s,int slen, intlen,exponseen)=
!intstr is a string containing all digits, before and after decimal point
!intlen=0:  no decimal point, so fractional part is empty
!intlen<>0: length of integer part
!expon=1:   e/E was last char, so need to read exponent first
!expon=0:   No e/E seen, so no exponent
	int i,fractlen,expon,exponsign,c,digs
	int64 x

	if intlen=0 or intlen=slen then
		fractlen:=0
	else
		fractlen:=slen-intlen
	fi

	expon:=0
	exponsign:=0

	if exponseen then
		case c:=lxsptr++^
		when '+' then
		when '-' then
			exponsign:=1
		else
			--lxsptr
		esac

		digs:=0
		doswitch c:=lxsptr++^
		when '0'..'9' then
			expon:=expon*10+c-'0'
			++digs
		else
			--lxsptr
			exit
		end
		if digs=0 then
			lxerror("Exponent error")
		fi
		if exponsign then expon:=-expon fi
	fi

	expon:=expon-fractlen

	lxxvalue:=0.0

	for i:=1 to slen do
		c:=s^[i]
		lxxvalue:=lxxvalue*10.0+(c-'0')
	od

	if expon>0 then
		to expon do
			lxxvalue:=lxxvalue*10.0
		od
	elsif expon<0 then
		to -expon do
			lxxvalue:=lxxvalue/10.0
		od
	fi

	lxsymbol:=realconstsym
end

proc readnumber(int c)=
!A digit c 0..9 has just been read. Numeric formats are::
!1234
!0x1234
!2x1101
!Nx....		possible
	[256]char str
	int i,d,intlen,slen

	d:=lxsptr^
	case d
	when 'x','X' then			!other base
		case c
		when '0' then			!hex
			++lxsptr
			readhex()
			return
		when '2' then			!binary
			++lxsptr
			readbinary()
			return
		else
			cpl c
			lxerror("Base not supported")
		esac
	esac

!assume decimal
	str[1]:=c
	slen:=1
	intlen:=0

	doswitch c:=lxsptr++^
	when '0'..'9' then
		str[++slen]:=c
	when '_','\'','`' then
	when '.' then
		intlen:=slen
	when 'e','E' then
		readreal(&str,slen,intlen,1)
		return
	else
		--lxsptr
		exit
	end

	if intlen then
		readreal(&str,slen,intlen,0)
		return
	fi

	if slen>20 or slen=20 and cmpstring(&.str,"18446744073709551615")>0 then
		lxerror("Overflow in 64-bit value")
	fi

	lxsymbol:=intconstsym

	lxvalue:=0
	for i:=1 to slen do
		lxvalue:=lxvalue*10+str[i]-'0'
	od
end

proc readbinary=
!positioned at start of binary seq; 0 chars read yet
	int ndigs

	ndigs:=0
	lxvalue:=0
	doswitch lxsptr++^
	when '0' then
		lxvalue:=lxvalue*2
		++ndigs
	when '1' then
		lxvalue:=lxvalue*2+1
		++ndigs
	when '2'..'9' then
		lxerror("Bad binary digit")
	when '_','\'','`' then
	else
		--lxsptr
		exit
	end

	if ndigs=0 then
		lxerror("No bin digits")
	elsif ndigs>64 then
		lxerror("Overflow in binary number")
	fi
	lxsymbol:=intconstsym
end

proc readhex=
!positioned at start of hex seq; 0 chars read yet
	int ndigs,c

	ndigs:=0
	lxvalue:=0
	doswitch c:=lxsptr++^
	when '0'..'9' then
		lxvalue:=lxvalue*16+c-'0'
		++ndigs
	when 'A'..'F' then
		lxvalue:=lxvalue*16+(c-'A'+10)
		++ndigs
	when 'a'..'f' then
		lxvalue:=lxvalue*16+(c-'a'+10)
		++ndigs
	when '_','\'','`' then
	else
		--lxsptr
		exit
	end

	if ndigs=0 then
		lxerror("No hex digits")
	elsif ndigs>16 then
		lxerror("Overflow in hex number")
	fi
	lxsymbol:=intconstsym
end

global proc ps(ichar caption)=
	PRINT CAPTION,":"
	PRINTSYMBOL()
end

global proc printsymbol(filehandle dev=nil)=
	[256]char str

	strcpy(&.str,symbolnames[lxsymbol])
	str[strlen(&.str)-2]:=0

	print @dev,&.str
	to 14-strlen(&.str) do print @dev," " od

	case lxsymbol
	when namesym then

		print @dev,lxsymptr.name

	when intconstsym then
		print @dev, lxvalue
	when realconstsym then
		print @dev, lxxvalue
	when stringconstsym then
		print @dev,"""",,lxsvalue,,""""!,,"end"
	when errorsym then
		print @dev,lxvalue
	else
		print @dev,symbolnames[lxsymbol]
		if lxsubcode then
			print " ",,lxsubcode
		fi

	end

	println @dev
end

proc clearhashtable=
!if defined in zdata, then will already be all zeros
!for i:=1 to hashtable.upb do
!	lexhashtable[i]:=void
!od
end

proc inithashtable=
!initialise hash table from kwddata
	[32]char str
	int i

	if hstsize>65536 then
!limit in place because of 16-bit-wide strec fields like .htindex
!	lxerror("hash table limited to 64K entries")
	fi

	clearhashtable()

	for i to mclnames.len do
		addreservedword(mclnames[i]+2,kopcodesym,i)
	od

	for i to dregnames.len do
		addreservedword(dregnames[i],kregsym,regindices[i])
		lxsymptr.regsize:=regsizes[i]
	od

	for i to xregnames.len do
		addreservedword(xregnames[i],kxregsym,i)
	od

	for i to fregnames.len do
		addreservedword(fregnames[i],kfregsym,i)
	od

	for i to mregnames.len do
		addreservedword(mregnames[i],kmregsym,i)
	od

	for i to jmpccnames.len do
		addreservedword(jmpccnames[i],kjmpccsym,jmpcccodes[i])
	od

	for i to setccnames.len do
		addreservedword(setccnames[i],ksetccsym,setcccodes[i])
	od

	for i to cmovccnames.len do
		addreservedword(cmovccnames[i],kmovccsym,cmovcccodes[i])
	od

	for i to prefixnames.len do
		addreservedword(prefixnames[i],kprefixsym,prefixsizes[i])
	od

	for i to segmentnames.len do
		strcpy(&.str,segmentnames[i])
		str[strlen(&.str)-3]:=0
		addreservedword(pcm_copyheapstring(&.str),ksegnamesym,i)
	od

	addreservedword("aframe",kregsym,r14); lxsymptr.regsize:=4
	addreservedword("dframe",kregsym,r14); lxsymptr.regsize:=8
	addreservedword("astack",kregsym,r15); lxsymptr.regsize:=4
	addreservedword("dstack",kregsym,r15); lxsymptr.regsize:=8
	addreservedword("dprog",kregsym,r8); lxsymptr.regsize:=8
	addreservedword("dsptr",kregsym,r9); lxsymptr.regsize:=8
end

proc addreservedword(ichar name,int symbol,subcode)=
	lxhashvalue:=gethashvalue(name)
	if lookuplex(name,0) then
		cpl =name
		lxerror("DUPL NAME")
	fi

	lxsymptr.symbol:=0
	lxsymptr.ksymbol:=symbol
	lxsymptr.subcode:=subcode
end

global proc printhashtable(filehandle devx,ichar caption)=
	ref strec r
	int count,i

	println @devx,caption,":"
	count:=0
	for i:=0 to lexhashtable.upb do
		r:=lexhashtable[i]
		if R AND r.name then
			count+:=1

		fi
	od
	println @devx,count," items in table",hstsize
end

function lookuplex(ichar name,int length=0)int=
!name is either in an existing table (for reserved words; length=0)
!or is in the source code (so is not zero-terminated; length is actual length)
!look for name in lexhashtable
!sets lxsymptr to entry in table, either of existing entry, or a new one
!returns 1/0 if found/not found (ie. old or new name)
	ref strec e

	int j,wrapped,insource,firstj

	insource:=length
	if length=0 then
		length:=strlen(name)
	fi

	firstj:=j:=(lxhashvalue iand hstmask)		!j=initial hash index

	wrapped:=0

	do
		lxsymptr:=lexhashtable[j]
		if lxsymptr=nil then				!unused entry, not found
			exit
		fi

		if lxsymptr.namelen=length and memcmp(lxsymptr.name,name,length)=0 then			!match
			return 1
		fi

		if ++j>hstsize then		!wraparound
			if wrapped then
				println "???????HASHTABLE FULL",hstsize,lxlineno
				stop 1
			fi
			wrapped:=1
			j:=1
		fi
	od

!name not found
	if insource then
		name:=makestring(name,length)
	fi

	if lxsymptr=nil then
		lxsymptr:=pcm_allocz(strec.bytes)
		lexhashtable[j]:=lxsymptr
	fi

	lxsymptr.name:=name
	lxsymptr.namelen:=length
	lxsymptr.symbol:=namesym
	lxsymptr.ksymbol:=0
	lxsymptr.htindex:=j
	lxsymptr.htfirstindex:=firstj
	lxsymptr.moduleno:=currmoduleno
	return 0
end

global proc initsourcefile(ichar source)=
	lxstart:=lxsptr:=cast(source)
	lxlineno:=1
end

global function addnamestr(ichar name)ref strec=
!add a new name to the symbol table
!return symptr to new (or existing) generic name
	lxhashvalue:=gethashvalue(name)
	lookuplex(pcm_copyheapstring(name),0)
	return lxsymptr
end

global proc lxerror(ichar m)=			!LXERROR

	fprintln "\w\w Lexical Error\n*** # *** on line #",m,lxlineno

	stop 1
end

global function gethashvalue(ichar s)int=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!ASSUMES S is lower-case, as conversion not done
	int c,hsum

	if s^=0 then return 0 fi

	hsum:=s++^

	do
		c:=s++^
		exit when c=0
		hsum:=hsum<<4-hsum+c
	od
	return hsum<<5-hsum
end

global proc skiptoeol=
!read lex tokens until eol and consume it
!return entire line as string
!note, exit with lxsptr pointing at the cr (or lf) char
	repeat
		lex()
	until lxsymbol=eolsym or lxsymbol=eofsym
END

function makestring(ichar p,int length)ref char=
!turn counted/non-terminated string from any source, into independent heap string
	ref char s

	s:=pcm_alloc(length+1)
	memcpy(s,p,length)
	(s+length)^:=0
	return s
end
