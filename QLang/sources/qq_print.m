!Vars for i/o
!Makes use of stdio/fileio/strio/windio as used by Q system
global  int mindev		!one of stdio/fileio/strio/windio
global  int moutdev
global  ref int minchan		!actual file handles
global  filehandle moutchan
global  varrec minvar		!strio: vars to be used as source or dest
global  varrec moutvar		!str: used for sprint(=string) and @&.string (=refvar)

!I/O Constants: print/read i/o channels
global const std_io	= 0		!console i/o
global const file_io	= 1		!uses file channel inchan or outchan
global const str_io	= 2		!uses string instr^ or outstr^
global const wind_io	= 3		!uses window inwind^ or outwind^
global const istr_io	= 4		!used by pcx interpreter

const maxoclevel=6
[0:maxoclevel]int32			moutdevstack
[0:maxoclevel]filehandle	moutchanstack
[0:maxoclevel]varrec		moutvarstack
[0:maxoclevel]byte			mgapstack
[0:maxoclevel]ref char		mfmtstrstack
[0:maxoclevel]ref char		mfmtcurrstack
int noclevels

const maxstrlen=256
const comma=','
const onesixty=1024

global  ichar mfmtstr		!used for format string is nil (no fmt string) or points to fmt string
global  ichar mfmtcurr	!point to next char to use in fmtstr
!global  fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,(0,0))
global  fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,0, 0)
byte mgapneeded

!const minkb_size=16384		! start size of kb buffer
const minkb_size=1048576		! start size of kb buffer
ref char kb_start			! point to start of read buffer
ref char kb_pos				! current position it's up to (next read starts here)
ref char kb_lastpos			! set by sread() just before reading used for reread()
int kb_size					! total available length of entire read buffer (which is not zero-terminated)
int kb_linelength			! length of this current line (set by readln)
int kb_length				! length of current contents of buffer (can be zero)
							! use kb_length-(kb_pos-kb_start) for length starting from kb_pos
int kb_lastlength			! used with kb_lastpos to remember start of last read item
char termchar				! terminator char set by readxxx()
int itemerror				!	set by some read functions, eg for reals

global filehandle testfilech	!non-zero means contains handle for test file o/p

const maxlistdepth=4
int listdepth=0				!recursive nesting levels for lists/records

![0:]char digits=a"0123456789ABCDEF"

global proc pch_print(variant p, fmt=nil)=
	varrec v
	variant q
!	object a
	ref char s
	varrec emptyfmt

	if fmt=nil then
		fmt:=&emptyfmt
		emptyfmt.tagx:=tvoid
	fi

	if mfmtstr=nil then
		if mgapneeded then
			printstr_n(" ",1)
		else
			mgapneeded:=1
		fi
	else
		printnextfmtchars(0)
	fi

	listdepth:=0
	pch_tostr(p,fmt,&v)
	printstr_n(v.objptr.strptr,v.objptr.length)

	var_unshare(&v)
end

global proc pch_print_nf(variant p)=
	pch_print(p, nil)
end

global proc pch_printnogap=
	mgapneeded:=0
end

global proc pch_println=
	if mfmtstr then
		printnextfmtchars(1)
	fi
	mgapneeded:=0
	printstr_n("\n",-1)
end

global proc pch_reread=
	kb_pos:=kb_lastpos
	kb_length:=kb_lastlength
end

global proc pch_rereadln=
	kb_pos:=kb_start
	kb_length:=kb_linelength
end

global proc pch_startprint(variant p)=
	object s

	case ++noclevels
	when 0, 1 then		! no action needed

	when maxoclevel+1 then		! overflow
		prterror("print #x overflow")
	else
		moutdevstack[noclevels-1]:=moutdev
		moutchanstack[noclevels-1]:=cast(moutchan)
		moutvarstack[noclevels-1]:=moutvar
		mfmtstrstack[noclevels-1]:=mfmtstr
		mfmtcurrstack[noclevels-1]:=mfmtcurr
		mgapstack[noclevels-1]:=mgapneeded
	end

	mfmtstr:=nil
	mfmtcurr:=nil

	if p=nil then
		goto doconsole
	fi
	case p.tag
	when tint then
		switch p.value
		when 0 then
	doconsole:
			moutdev:=std_io
			moutchan:=nil
		
		when 1 then			! special sprint string
			moutdev:=str_io
			moutchan:=nil
			moutvar.tagx:=tstring ior hasrefmask

			s:=obj_new()
			s.mutable:=1
			moutvar.objptr:=s

		when 2 then
			if testfilech=nil then
				prterror("@2: file not open")
			fi
			moutdev:=file_io
			moutchan:=testfilech
		
		else
			moutdev:=file_io
			moutchan:=cast(filehandle(p.value))
		end

	when trefvar then
		p:=p.varptr
		case p.tag
		when tstring then
			moutdev:=istr_io
			moutchan:=nil
			moutvar.tagx:=trefvar
			moutvar.varptr:=p
		
		else
			PRINTLN ttname[p.tag]
			prterror("Print@^?")
		end

	else
		case p.tag
		when trecord, tstruct then		! check for specific records
			moutdev:=std_io
		else
			PRINTLN ttname[p.tag]
			prterror("Can't do startprint...")
		end
	end

	mgapneeded:=0
end

global proc pch_startprintcon=
	varrec v

	v.tagx:=tint
	v.value:=0
	pch_startprint(&v)
end

global proc pch_endprint=
	variant p

	if mfmtstr then
		printnextfmtchars(1)
	fi
	case moutdev
	when istr_io then
		p:=moutvar.varptr
	end

	if mfmtstr<>nil then
		pcm_free(mfmtstr,strlen(mfmtstr)+1)
	fi

	if --noclevels=-1 then
		prterror("resetoc??")
	fi

	if noclevels=0 then
		moutdev:=std_io

	else			! exit from higher nesting level
		moutdev:=moutdevstack[noclevels]
		moutchan:=cast(moutchanstack[noclevels])
		moutvar:=moutvarstack[noclevels]
		mgapneeded:=mgapstack[noclevels]
		mfmtstr:=mfmtstrstack[noclevels]
		mfmtcurr:=mfmtcurrstack[noclevels]
	fi
	mgapneeded:=0
end

global proc pch_strstartprint* =
	varrec p

	p.tagx:=tint
	p.value:=1
	pch_startprint(&p)		! do equivalent of @1
end

global proc pch_strendprint(variant dest) =
	if mfmtstr then
		printnextfmtchars(1)
	fi
	if moutdev<>str_io then
		prterror("STRENDPRT/NOT STR")
	fi

	dest^:=moutvar						!transfer ownership
	moutvar.tagx:=tvoid

	pch_endprint()
end

global proc pch_printspace=
	mgapneeded:=0
	print " "
end

global proc pch_readln(variant dev) =
!note: generally, at least one spare given should be left at the of the buffer.
!(readline zero-terminates the input line anyway)
!Sometimes C-functions might be called directly, and a zero-terminator is added (eg. readreal/sscanf)
	filehandle ch
	int length
	object pdev

	if kb_start=nil then
		kb_start:=pcm_alloc(minkb_size)
		kb_size:=minkb_size
		kb_lastpos:=kb_start
		kb_pos:=kb_start
		kb_length:=0
		kb_lastlength:=0
		kb_linelength:=0
	fi

	case dev.tag
	when tvoid then
doconsole:
		readlinen(nil,kb_start,kb_size)	! reads as zero-terminated
		kb_length:=strlen(kb_start)

	when tint then
		case dev.value
		when 0 then
			goto doconsole
		when 1 then
			if testfilech=nil then
				prterror("R@2: file not open")
			fi
			ch:=cast(testfilech)

		else
			ch:=filehandle(dev.value)
		end
!		pc_readlinen(cast(ch),kb_start,kb_size)			! reads as zero-terminated
		readlinen(cast(ch),kb_start,kb_size)			! reads as zero-terminated
		kb_length:=strlen(kb_start)

	when tstring then
		pdev:=dev.objptr
		length:=pdev.length
		if length=0 then
			kb_length:=0
			kb_start^:=0
		elsif length>=kb_size then
			prterror("KB overflow")
		else
			kb_length:=length
			memcpy(kb_start,pdev.strptr,length)
		fi
	else
		pcustype("readln@",dev)
	end

	kb_pos:=kb_start
	kb_lastpos:=kb_pos
	kb_linelength:=kb_length
end

global proc pch_sread(variant fmt,variant dest) =
	int fmtcode
	char c

!pc_cfree(dest)
	fmtcode:=getreadfmtcode(fmt)
	kb_lastpos:=kb_pos
	kb_lastlength:=kb_length

	case fmtcode
	when 'I' then
		stepkbpos(readint(kb_pos,kb_length,dest,0))

	when 'R' then
		stepkbpos(readreal(kb_pos,kb_length,dest))

	when 'N' then
		stepkbpos(readname(kb_pos,kb_length,dest))

	when 'S' then
		stepkbpos(readstring(kb_pos,kb_length,dest))

	when 'H' then
		stepkbpos(readhex(kb_pos,kb_length,dest))

	when 'B' then
		stepkbpos(readbin(kb_pos,kb_length,dest))

	when 'A' then
		stepkbpos(readany(kb_pos,kb_length,dest))

	when 'L' then
		if kb_length=0 then
			var_empty_string(dest)
		else
			var_make_stringn(kb_pos,kb_length,dest,1)
			kb_pos+:=kb_length
			kb_length:=0
		fi

	when 'C' then
		if kb_length=0 then
			var_empty_string(dest)
		else
			termchar:=kb_pos^
	dochar:
			dest.tagx:=tint
			dest.value:=termchar
			++kb_pos
			--kb_length
		fi

	when 'Z' then			! last terminator!
		dest.tagx:=tint
		dest.value:=termchar

	when 'E' then
		dest.tagx:=tint
		dest.value:=itemerror
	when 'D' then
		stepkbpos(readint(kb_pos,kb_length,dest,1))

	else
		prterror("SREAD/FMT?")
	end
end

global proc pch_sreadln(variant dev, variant dest) =
	pch_readln(dev)
	var_make_stringn(kb_start,kb_length,dest,mutable:1)
end

function readname(ref char s,int length,variant dest)ref char =
	ref char send
	ref char itemstr
	int itemlength
	send:=readitem(s,length,itemstr,itemlength)
	var_make_stringn(itemstr,itemlength,dest,1)

	iconvlcn(dest.objptr.strptr,dest.objptr.length)
	return send
end

function readstring(ref char s,int length,variant dest)ref char =
	ref char send
	ref char itemstr
	int itemlength
	send:=readitem(s,length,itemstr,itemlength)
	var_make_stringn(itemstr,itemlength,dest,1)
	return send
end

function readint(ref char sold,int length,variant dest, int dodec)ref char =
!return point to next char after terminator (which can be just off length of string)
	ref char p,s				! s points to ^str
	ref char send
	ref char itemstr
	int itemlength,numlength

	send:=readitem(sold,length,s,itemlength)

	strtoint(s,itemlength,dest,dodec)

	return send
end

function readhex(ref char sold,int length,variant dest)ref char =
	[0:maxstrlen]char str		! local copy
	ref char p,s			! s points to ^str
	byte res
	i64 aa
	int a,t,nalloc
	char c

	if length=0 then
		dest.tagx:=tint
		dest.value:=0
		termchar:=0
		return sold
	fi

!copy to buffer first skip leading spaces, and any sign
	while (length and (sold^=' ' or sold^=9)) do
		++sold; --length
	od

	if length<=maxstrlen then	! use local buffer
		s:=&.str
		nalloc:=0
	else
		nalloc:=length+1
		s:=pcm_alloc(nalloc)
	fi

	p:=s				! p points to next char available
	while (length) do
		c:=toupper(sold^); ++sold; --length
		if c>='0' and c<='9' then
			p^:=c
			++p
		elsif c>='A' and c<='F' then
			p^:=c
			++p
		elsif c='_' then
		else
			termchar:=c
			exit
		fi
	od
	p^:=0				! use zero terminator for local string
	length:=p-s			! length of s

! try and work out type
	if length<=16 then
		t:=tint
	else
		t:=tdecimal
	fi
	p:=s
	case t
	when tint then
		aa:=0
		do
			c:=p^; ++p
			if c=0 then
				exit
			fi
			if c<'A' then			! assume digit '0'..'9'
				aa:=aa*16+c-'0'
			else				! assume letter 'A'..'F'
				aa:=aa*16+(c-'A')+10
			fi
		od
		dest.tagx:=tint
		dest.value:=aa
	else
		prterror("Readhex/long")
	end

	if nalloc then
		pcm_free(s,nalloc)
	fi

	return sold
end

function readbin(ref char sold,int length,variant dest)ref char =
	[0:maxstrlen]char str		! local copy
	ref char p,s			! s points to ^str
	byte res
	i64 aa
	int a,t,nalloc
	char c

	if length=0 then
		dest.tagx:=tint
		dest.value:=0
		termchar:=0
		return sold
	fi

!copy to buffer first skip leading spaces, and any sign
	while (length and (sold^=' ' or sold^=9)) do
		++sold; --length
	od

	if length<=maxstrlen then	! use local buffer
		s:=&.str
		nalloc:=0
	else
		nalloc:=length+1
		s:=pcm_alloc(nalloc)
	fi

	p:=s				! p points to next char available
	while (length) do
		c:=toupper(sold^); ++sold; --length
		if c>='0' and c<='1' then
			p^:=c
			++p
		elsif c='_' then
		else
			termchar:=c
			exit
		fi
	od
	p^:=0				! use zero terminator for local string
	length:=p-s			! length of s

!try and work out type
	if length<=64 then
		t:=tint
	else
		t:=tdecimal
	fi

	p:=s
	case t
	when tint then
		aa:=0
		do
			c:=p^; ++p
			if c=0 then
				exit
			fi
			aa:=aa*2+c-'0'
		od
		dest.tagx:=tint
		dest.value:=aa

	else
!	bx_makeu_base(s,strlen(s),dest,2)
		prterror("Readbin/long")
	end

	if nalloc then
		pcm_free(s,nalloc)
	fi
	return sold
end

function readreal(ref char sold,int length,variant dest)ref char =
	[512]char str		! local copy
	real x
	ref char send
	ref char itemstr
	int itemlength,numlength

	send:=readitem(sold,length,itemstr,itemlength)
	strtoreal(itemstr,itemlength,dest)

	return send
end

global function getreadfmtcode(variant p)int =
!p is a variant  which should point to a string containing a read format code.
!return that code as an upper when char code, eg. 'I'
	char c

!if p=nil or p.tag=tvoid then
	if p=nil or p.tag=tvoid then
!	return 'I'
		return 'A'
	fi
	if p.tag<>tstring then
	CPL "P=%s",ttname[p.tag]
		prterror("Readfmt?")
	fi
	if p.objptr.length=0 then
!	return 'I'
		return 'A'
	fi

	c:=toupper(p.objptr.strptr^)

	case c
	when 'I', 'R', 'N', 'S', 'F', 'T', 'Z', 'C', 'L', 'H','B','A','E','D' then
		return c
	end

	prterror("Readfmt2?")
	return 0
end

proc stepkbpos(ref char s) =
!a readxxx function has been called with kb_pos/kb_length, and has returned s to point to
!the character after the terminator
!adjust kb_pos/kb_length to point to that position
	int newlen

	newlen:=s-kb_pos

	if newlen=0 then		! nothing read probably was at end of buffer
		return
	fi
	if newlen>=kb_length then	! at end of buffer
		kb_pos:=kb_pos+kb_length	! point to just past buffer (but should never be accessed when kb_length=0)
		kb_length:=0
	else
		kb_pos:=kb_pos+newlen
		kb_length-:=newlen
	fi
end

function readany(ref char sold,int length,variant dest)ref char =
!read item as int, real or string depending on content
!return point to next char after terminator (which can be just off length of string)
	[0:maxstrlen]char str			! local copy
	ref char p,s				! s points to ^str
	byte signd,res
	i64 aa
	int digits,expon,other
	int t,nalloc
	char c

	ref char send
	ref char itemstr
	int itemlength,numlength

	itemerror:=0

	send:=readitem(sold,length,s,itemlength)

!now analyse item
!ints consist only of 0123456789+-_'
!reals consist only of 0123456789+-Ee.

	p:=s
	digits:=expon:=other:=0

	to itemlength do
		switch p++^
		when '0'..'9','+','-','_' then digits:=1
		when 'E','e','.' then expon:=1
		else other:=1
		end
	od

	dest.tagx:=tint

	if other or itemlength=0 then
		dest.value:='STR'
		var_make_stringn(s,itemlength,dest,1)
	elsif expon then
		strtoreal(s,itemlength,dest)
	else
		strtoint(s,itemlength,dest,0)
	fi

	return send
end

function readitem(ref char s,int length,ref char &itemstr,int &itemlength)ref char =		!READSTRING
!s points into the line buffer
!length is number of chars remaining in buffer
!identify a substring that can contain a name, int, real, string or filename
!return updated position of s that points past the item and past the immediate
!terminator 
!information about the read item is returned in itemstr, which points to
!the start of the item, and in itemlength. Item excludes any surrounding whitespace
!Item can be quoted, then the item points inside the quotes
!Any embedded quotes are removed, and the characters moved up. The item will
!be that reduced subsequence
!Note that this is destructive. On reread, the input will be different.
!I can mitigate this by adding spaces between the end of the item, and the next item,
!overwriting also the terminator. But this won't restore the line if one of the next
!reads is literal, using 'L' or 'C' codes.
	ref char p
	char quotechar, c

!scan string, eliminating leading white space
	while (length and (s^=' ' or s^=9)) do
		++s; --length
	od

	itemstr:=s				!assume starts here

	if length=0 then		! No more chars left to read return null string
		termchar:=0
		itemlength:=0
		return s
	fi

	quotechar:=0			! Allow possible enclosing single or double quotes
	if s^='"' then
		quotechar:='"'
		++s
		--length
	elsif s^='\'' then
		quotechar:='\''
		++s
		--length
	fi

!loop reading characters until separator or end reached
	p:=itemstr:=s

	while length do
		c:=s++^; --length
		case c
		when ' ', 9, comma, '=', ';' then		! separator
			if quotechar or p=s then			!can be considered part of name if inside quotes, or is only char
				goto normalchar
			fi
			termchar:=c
			exit
		else
	normalchar:
			if c=quotechar then
				if length and s^=quotechar then	! embedded quote
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
		end
	od

	if length=0 then
		termchar:=0
	fi
	itemlength:=p-itemstr				! actual length of token

	return s
end

proc strtoreal(ichar s,int length,variant dest)=
	[512]char str		! local copy
	real x
	int32 numlength

	dest.tagx:=treal

	if length>=str.bytes or length=0 then		!assume not a real
		dest.xvalue:=0.0
		return
	fi
	memcpy(&.str,s,length)
	str[length+1]:=0

	itemerror:=0

	if sscanf(&.str,"%lf%n", &x, &numlength)=0 or numlength<>length then
		if numlength=length then x:=0.0 fi
		itemerror:=1
	fi

	dest.xvalue:=x
end

proc strtoint(ichar s,int length, variant dest, int dodec)=
!return point to next char after terminator (which can be just off length of string)
	[0:maxstrlen]char str			! local copy
	ref char p,q
	byte signd
	i64 aa
	int a,res,cat
	int t,nalloc
	char c

	itemerror:=0

	if length=0 then
		dest.tagx:=tint
		dest.value:=0
		return
	fi

!check for sign
	signd:=0
	if length and s^='-' then
		signd:=1; ++s; --length
	elsif length and s^='+' then
		++s; --length
	fi

	while s^='0' and length>1 do
		++s; --length
	od

	p:=q:=s				! p points to next char available

	while length do
		c:=q++^
		--length
		if c>='0' and c<='9' then
			p^:=c
			++p
		else
			if c='_' then
			else
				itemerror:=1
				exit
			fi
		fi
	od
	p^:=0				! use zero terminator for local string
	length:=p-s			! length of s

!classify magnitude of value as:
!'A' 0 to 2**63-1 or 0..9223372036854775807
!'B' 2**63 or 9223372036854775808
!'C' 2**63+1 to 2**64-1 or 9223372036854775809..18446744073709551615
!'D' 2**64 and over or 18446744073709551616 and over

	if length<=18 then
		cat:='A'
	elsif length=19 then
		case cmpstring(s,"9223372036854775808")
		when -1 then cat:='A'
		when 0 then cat:='B'
		else cat:='C'
		end
	elsif length=20 then
		if cmpstring(s,"18446744073709551615")<=0 then
			cat:='C'
		else
			cat:='D'
		fi
	else
		cat:='D'
	fi

	if dodec then cat:='D' fi

!now look at sign:
	if signd then
		case cat
		when 'B' then cat:='A'		!-922...808 can be int64
		when 'C' then cat:='D'		!needs longint
		end
	fi

!convert cat to type

	case cat
	when 'A' then t:=tint
!	when 'B','C' then t:=tword
	else t:=tdecimal
	end

	p:=s
	if t<>tdecimal then
		aa:=0
		do
			c:=p^; ++p
			if c=0 then
				exit
			fi
			aa:=aa*10+(c-'0')
		od
		if signd then
			aa:=-aa
		fi
		dest.tagx:=t
		dest.value:=aa

	else
		var_make_dec_str(s,length,dest)
	fi
end

proc printnextfmtchars(int lastx) =
!o/p chars from fmtstr until # or eos is encountered
	char c
	ref char pstart
	int n

	pstart:=mfmtcurr
	n:=0

	do
		c:=mfmtcurr^
		case c
		when '#' then
			if lastx then
				goto skip
			fi
			++mfmtcurr
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
			++mfmtcurr
			c:=mfmtcurr^
			if c then
				++mfmtcurr
				printstr_n(&c,1)
			fi
			pstart:=mfmtcurr
		else
skip:
			++n
			++mfmtcurr
		end
	od
end

global proc pch_setformat(variant p) =
	int n
	ref char s

	if p.tag<>tstring then
		prterror("(str)")
	fi
	if mfmtstr then
		prterror("Setfmt?")
	fi
	n:=p.objptr.length
	mfmtstr:=pcm_alloc(n+1)
	if n then
		memcpy(mfmtstr,p.objptr.strptr,n)
	fi
	s:=mfmtstr+n
	s^:=0

	mfmtcurr:=mfmtstr
end

global function pc_getfmt(variant p,ref fmtrec fmt)ref fmtrec=
!p is an optional fmt string to tostr and print
!turn into a proper format
!return pointer to a format, or to the default format is not supplied
!fmt points to a fmtrec in the caller to contain the processed format

if p=nil or p.tag=tvoid then
	return &defaultfmt
else
	if p.tag<>tstring then
		prterror("pc_getfmt/not str?")
	fi
	if p.objptr.strptr=nil then
		return &defaultfmt
	else
		strtofmt(p.objptr.strptr,p.objptr.length,fmt)
		return fmt
	fi
fi
end

global proc addstring(object p,ref char t,int n=-1) =
!p is a pointer to an object string data, initially with an empty string
!store string t to to p, or append to an existing string
!n is the length of the string (-1 if not known) =
	int oldlen,newlen,oldbytes,newbytes
	ref char newptr

	if n=0 or t^=0 then
		return
	fi
	if n<0 then
		n:=strlen(t)
	fi

	oldlen:=p.length

	if p.refcount=0 then	! assume a fixed buffer
		if oldlen=0 then		! first string
			memcpy(p.strptr,t,n)
			p.length:=n
		else				! append to existing string
			memcpy(p.strptr+oldlen,t,n)
			p.length:=oldlen+n
		fi
		return
	fi

	if oldlen=0 then		! first or only string
		p.strptr:=pcm_alloc(n)
		p.length:=n
		p.alloc64:=allocbytes
		memcpy(p.strptr,t,n)

	else				! append to existing string
		newlen:=oldlen+n
		oldbytes:=p.alloc64
		newbytes:=oldlen+n
		if newbytes<=oldbytes then 		! fits in current allocation
			memcpy(p.strptr+oldlen,t,n)
		else					! need new allocation
			newptr:=pcm_alloc(newbytes)
			memcpy(newptr,p.strptr,oldlen)	! existing chars
			memcpy(newptr+oldlen,t,n)		! add new chars
			p.alloc64:=allocbytes
			pcm_free(p.strptr,oldbytes)
			p.strptr:=newptr
		fi
		p.length:=newlen
	fi
end

proc domultichar (ref char p,int n,ref char dest,ref fmtrec fmt) =
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

	expandstr(str,dest,nchars,fmt)
end

proc printstr_n(ref char s,int n) =
!send string s to current m output device
!n is:
! -1:	s is zero-terminated; calculate length
! 0:	s is empty string (no output)
! >0:	n is length of string
	variant  p
	int x
	type fntype= ref function (filehandle f, ichar s, int i, ichar t)int

	if n=-1 then		! was stringz
		n:=strlen(s)
	fi

	if n=0 then
		return
	fi

	case moutdev
	when std_io then
		printstrn_app(s,n,nil)

	when file_io then
		printstrn_app(s,n,cast(moutchan))

	when str_io then
		addstring(moutvar.objptr,s,n)

	when istr_io then
		p:=moutvar.varptr
!CPL "PRINTSTR/STR",=MOUTVAR.OBJPTR
		if p.tag<>tstring then
			prterror("prtstrn1")
		fi
		addstring(moutvar.objptr,s,n)

	when wind_io then
		
	end
end

global proc pch_strtoval(variant p,variant fmt,variant dest) =
!p should be a string, fmt is nil, or contains a string format code for read
!convert string to value, then store in dest
	int fmtcode,length
	byte oldmutable
	object q
	[1024]char str
	ref char s:=&.str

	q:=p.objptr

	if q.length<str.len then
		memcpy(s,q.strptr,q.length)
		str[q.length+1]:=0
	else
		pcerror("STRTOVAL/string too long")
	fi

	fmtcode:=getreadfmtcode(fmt)
	if p.tag<>tstring then
		prterror("strval")
	fi
	length:=p.objptr.length

	case fmtcode
	when 'I' then
		readint(s,length,dest,0)
	when 'D' then
		readint(s,length,dest,1)
	when 'R' then
		readreal(s,length,dest)
	when 'N' then
		readname(s,length,dest)
	when 'S' then
		readstring(s,length,dest)
	when 'H' then
		readhex(s,length,dest)
	when 'B' then
		readbin(s,length,dest)
	when 'A' then
		readany(s,length,dest)
!
	else
		prterror("strval:fmt?")
	end
end

proc tostr_int(variant p,ref fmtrec fmt,object dest) =
	[0:onesixty]char str

	case fmt.charmode
	when 'M' then
		domultichar(ref char(&p.value),8,str,fmt)

	when 'C' then

		str[0]:=p.value
		str[1]:=0

	else
!		i64tostrfmt(p.value,str,fmt,0)
		i64tostrfmt(p.value,str,fmt)
	end

	if fmt.showtype then
		addstring(dest,"I:",2)
	fi

	addstring(dest,str,strlen(str))
end

proc tostr_real(variant p, ref fmtrec fmt, object dest) =
	[0:onesixty]char str,str2
	[10]char cfmt
	int n

	if fmt.precision then
		cfmt[1]:='.'
		cfmt[2]:='*'
		cfmt[3]:=fmt.realfmt
		cfmt[4]:=0
		print @str,fmt.precision:"v", p.xvalue:cfmt

	else
		print @str,p.xvalue:"fmt"
	fi

!at this point, n is the str length including signs and suffix
	n:=strlen(str)		! current length
	if n<fmt.minwidth then
		expandstr(str,str2,n,fmt)
		strcpy(str,str2)
	fi

	addstring(dest,str,strlen(str))
end

proc tostr_str(variant p, ref fmtrec fmt, object dest) =
	int oldlen,newlen
	ref char s
	[0:100]char str
	object q

!try and work out size of formatted string
	q:=p.objptr
	oldlen:=q.length
	newlen:=oldlen

	if fmt.quotechar or fmt.minwidth>newlen then
		if fmt.quotechar then
			newlen+:=2
		fi
		if fmt.minwidth>newlen then
			newlen:=fmt.minwidth
		fi
		s:=pcm_alloc(newlen+1)
		strtostrfmt(q.strptr,s,oldlen,fmt)
		addstring(dest,s,newlen)
		pcm_free(s,newlen+1)
	else
		addstring(dest,q.strptr,oldlen)
	fi
end

global proc pch_tostr(variant a, b, result)=
	fmtrec fmt
	ref fmtrec ifmt
	object p

	ifmt:=pc_getfmt(b,&fmt)

	p:=obj_new_string(0)

	listdepth:=0

	tostr(a,ifmt,p)

	result.tagx:=tstring ior hasrefmask
	result.objptr:=p
end

proc tostr_range(variant p, ref fmtrec fmt, object dest) =
	[0:onesixty]char str

!	i64tostrfmt(p.range_lower,str,fmt,0)
	i64tostrfmt(p.range_lower,str,fmt)
	strcat(str,"..")
	addstring(dest,str)
!	i64tostrfmt(p.range_upper,str,fmt,0)
	i64tostrfmt(p.range_upper,str,fmt)
	addstring(dest,str)
end

proc tostr_array(variant p, ref fmtrec fmt, object dest) =
	[0:onesixty]char str
	ref byte q
	int i,m,elemtype,a,b,lower, length
	varrec v
	object pa
	ref byte ptr

	m:=p.tag
	pa:=p.objptr

	if p.tag=tarray then
		length:=pa.length
		lower:=pa.lower
		elemtype:=pa.elemtag
	else
		length:=ttlength[pa.usertag]
		lower:=ttlower[pa.usertag]
		elemtype:=tttarget[pa.usertag]
	fi

	a:=lower
	b:=length+lower-1

	q:=pa.ptr

	if fmt.showtype then
		fprint @str,"#[#:#]A",ttname[m],lower,ttname[elemtype]
		addstring(dest,str)
	fi
	addstring(dest,"(")

	for i:=a to b do
		var_loadpacked(q,elemtype,&v,nil)
		q+:=ttsize[elemtype]
		tostr(&v,fmt,dest)
		if i<b then
			addstring(dest,",",1)
		fi
	od
	addstring(dest,")",1)
end

proc tostr_bits(variant p, ref fmtrec fmt, object dest) =
	[0:onesixty]char str
	ref byte q
	int i,m,elemtype,a,b,bitwidthx,offset
	varrec v
	object pa
	ref byte ptr

	m:=p.tag
	pa:=p.objptr

	a:=pa.lower16
	elemtype:=pa.elemtag
	b:=pa.length+a-1
	bitwidthx:=ttbitwidth[elemtype]
	offset:=pa.indexoffset*bitwidthx

	q:=pa.ptr

	if fmt.showtype then
		fprint @str,"#[#:#]A",ttname[m],pa.lower16,ttname[elemtype]
		addstring(dest,str)
	fi
	addstring(dest,"(")

	for i:=a to b do
		var_loadbit(q,offset,elemtype,0,&v)
		offset+:=bitwidthx
		if offset>=8 then
			offset:=0
			++q
		fi
		tostr(&v,fmt,dest)
		if i<b then
			addstring(dest,",",1)
		fi
	od
	addstring(dest,")",1)
end

proc tostr_struct(variant p, ref fmtrec fmt, object dest) =
!	[0:onesixty]char str
	ref byte q
	int i,m,nfields,needcomma
	varrec v
	object pa
	ref byte ptr
	symbol d
	ref symbol r

	pa:=p.objptr
	m:=pa.usertag

	d:=ttnamedef[m]

	r:=d.topfieldlist
	nfields:=ttlength[m]

	needcomma:=0
	addstring(dest,"(")

	for i to nfields do
		var_loadpacked(pa.ptr+r.fieldoffset, r.mode, &v, nil)
		if needcomma then
			addstring(dest,",")
		fi
		needcomma:=1

		tostr(&v,fmt,dest)
		++r
	od
	addstring(dest,")")
end

proc tostr_set(variant p,ref fmtrec fmt,object dest) =
	[0:onesixty]char str
	variant q
	int i,j,first
	varrec v
	object s

	if fmt=nil then
		fmt:=&defaultfmt
	fi

	addstring(dest,"[",1)

	s:=p.objptr

	first:=1

	i:=0
	while (i<s.length) do
		if testelem(cast(s.ptr),i) then	! element i included
			j:=i+1				! now search for end of this '1' block
			while (j<s.length and testelem(cast(s.ptr),j)) do
				++j
			od
			--j				! last '1' in group
			if not first then
				addstring(dest,",",1)
			fi
			first:=0
			if i=j then
				v.tagx:=tint
				v.value:=i
			else
				v.tagx:=trange
				v.range_lower:=i
				v.range_upper:=j
			fi
			tostr(&v,fmt,dest)
			i:=j+1
		else
			++i
		fi
	od
	addstring(dest,"]",1)
end

proc tostr_dict(variant p,ref fmtrec fmt,object dest) =
	[0:onesixty]char str
	variant q
	int i,length,needcomma:=0
	object pa

	if fmt=nil then
		fmt:=&defaultfmt
	fi
	addstring(dest,"[",-1)

	pa:=p.objptr
	q:=pa.varptr		!keys/value pairs

	length:=pa.length/2				!number of pairs

	for i:=length downto 1 do
		if q.tag=tvoid then
			q+:=2
			nextloop
		fi
		if needcomma then
			addstring(dest,",",1)
		fi
		needcomma:=1
		tostr(q,fmt,dest)
		q++
		addstring(dest,":",1)
		tostr(q,fmt,dest)
		q++
	od
	addstring(dest,"]",1)
end

proc tostr_decimal(variant p,ref fmtrec fmt,object dest) =
	ref char s

	s:=var_tostr_dec(p,0)
	addstring(dest,s,-1)
	pcm_free(s,decstrsize)
end

proc tostr(variant p, ref fmtrec fmt, object dest) =
	[1024]char str


!CPL "TOSTR",=STRMODE(P.TAG),=P.TAG

	case p.tag
	when tint then
		tostr_int(p, fmt, dest)

	when treal then
		tostr_real(p, fmt, dest)

	when tstring then
		tostr_str(p, fmt, dest)

	when trange then
		tostr_range(p, fmt, dest)

	when tlist, trecord then
		tostr_list(p, fmt, dest)

	when tarray,tvector then
		tostr_array(p, fmt, dest)

	when tbits then
		tostr_bits(p, fmt, dest)

	when tset then
		tostr_set(p, fmt, dest)

	when tstruct then
		tostr_struct(p, fmt, dest)

	when tdecimal then
		tostr_decimal(p, fmt, dest)

	when tdict then
		tostr_dict(p, fmt, dest)

	when tvoid then
		addstring(dest,"<Void>")

	when trefvar then
		if fmt.showtype then
			fprint @str,"#<#>:",ttname[p.tag],(p.varptr|ttname[p.varptr.tag]|"")
			addstring(dest,str)
		fi
showptr:
		if p.varptr=nil then
			addstring(dest,"nil")
		else
			addstring(dest,strint(cast(p.varptr),"H"))
		fi

	when trefpack then
		if fmt.showtype then
			fprint @str,"#<#>:",ttname[p.tag],(p.varptr|ttname[p.elemtag]|"")
			addstring(dest,str)
		fi
		showptr

	when trefbit then
		if fmt.showtype then
			fprint @str,"#<#>(#,#):",ttname[p.tag],(p.varptr|ttname[p.elemtag]|""),
				p.bitoffset,p.bitlength
			addstring(dest,str)
		fi
		showptr

	when tsymbol then
		if p.def then
			fprint @str,"<#:""#"">",namenames[p.def.nameid],p.def.name
			addstring(dest,str)
		else
			addstring(dest,"<nil>")
		fi
	when ttype then
		fprint @str,"#",ttname[p.value]!+(p.value<=tlast|1|0)
		addstring(dest,str)
	when toperator then
		fprint @str,"(#)", pclnames[p.value]+1
		addstring(dest,str)

!	when tenum then
!		addstring(dest,getenumname(p.elemtag, p.value))

	else
		pcustype("Tostr:",p)
	end
end

proc tostr_list(variant p, ref fmtrec fmt, object dest) =
	variant q
	int i,n
	char c
	object r

	++listdepth

	r:=p.objptr
	if r.refcount<0 or listdepth>maxlistdepth then
		addstring(dest,"...",3)
		--listdepth
		return
	fi

	r.refcount:=-r.refcount
	q:=r.varptr

	if p.tag=tlist then
		n:=p.objptr.length
	else
		n:=ttlength[r.usertag]
	fi

	if fmt.newline then
		to n do
			tostr(q,fmt,dest)
			addstring(dest,"\n",-1)
			++q
		od

	else
		addstring(dest,"(",1)
		for i:=n downto 1 do
			tostr(q,fmt,dest)
			++q
			if i<>1 then
				addstring(dest,",",1)
			fi
		od
		addstring(dest,")",1)
	fi
	r.refcount:=-r.refcount
	--listdepth
end
