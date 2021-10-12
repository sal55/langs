import msys
import clib
import oslib

!const mem_check=1
const mem_check=0

global [0..300]u64 allocupper
global int alloccode				!set by heapalloc
global int allocbytes				!set by heapalloc
global int fdebug=0
global int rfsize

const threshold=1<<25
const alloc_step=1<<25
word maxmemory
int  maxalloccode

GLOBAL REF VOID ALLOCBASE

byte pcm_setup=0

int show=0

global int memtotal=0
global int64 smallmemtotal=0
global int smallmemobjs=0
global int maxmemtotal=0

!store all allocated pointers
const int maxmemalloc=(mem_check|500000|2)
[maxmemalloc+1]ref int32 memalloctable
[maxmemalloc+1]int32 memallocsize

const pcheapsize=1048576*2
ref byte pcheapstart
ref byte pcheapend			!points to first address past heap
ref byte pcheapptr

const int maxblockindex = 8 		!2048
global const int maxblocksize = 2048

[0:maxblocksize+1]byte sizeindextable	!convert byte size to block index 1..maxblockindex

const int size16   = 1			!the various index codes
const int size32   = 2
const int size64   = 3
const int size128  = 4
const int size256  = 5
const int size512  = 6
const int size1024 = 7
const int size2048 = 8

GLOBAL [0:9]ref wordp freelist

global record strbuffer =
	ichar strptr
	int32 length
	int32 allocated
end

global tabledata() [0:]ichar pmnames=
	(pm_end=0,		$),
	(pm_option,		$),
	(pm_sourcefile,	$),
	(pm_libfile,	$),
	(pm_colon,		$),
	(pm_extra,		$),
end

![2]word seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)
[2]int seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)

global function pcm_alloc(int n)ref void =		!PCM_ALLOC
	ref byte p

	if not pcm_setup then
		pcm_init()
	fi

	if n>maxblocksize then			!large block allocation

		alloccode:=pcm_getac(n)
		allocbytes:=allocupper[alloccode]

		p:=allocmem(allocbytes)
		if not p then
			abortprogram("pcm_alloc failure")
		fi

		if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

		return p
	fi

	alloccode:=sizeindextable[n]		!Size code := 0,1,2 etc for 0, 16, 32 etc
	allocbytes:=allocupper[alloccode]
	smallmemtotal+:=allocbytes

	if p:=ref byte(freelist[alloccode]) then		!Items of this block size available
	if mem_check then addtomemalloc(ref int32(p),allocbytes) fi
		freelist[alloccode]:=ref wordp(int((freelist[alloccode])^))

		return p
	fi

!No items in freelists: allocate new space in this heap block
	p:=pcheapptr				!Create item at start of remaining pool in heap block
	pcheapptr+:=allocbytes			!Shrink remaining pool

	if pcheapptr>=pcheapend then		!Overflows?
		p:=pcm_newblock(allocbytes)		!Create new heap block, and allocate from start of that
		return p
	fi
	if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

	return p
end

global proc pcm_free(ref void p,int n) =		!PCM_FREE
!n can be the actual size requested it does not need to be the allocated size
	int acode

	if n=0 then return fi

	if n>maxblocksize then		!large block
		if mem_check then removefrommemalloc(p,n) fi

		free(p)
		return
	fi

	if p then
		acode:=sizeindextable[n]		!Size code := 0,1,2 etc for 0, 16, 32 etc

		smallmemtotal-:=allocupper[acode]

		if mem_check then removefrommemalloc(p,allocupper[acode]) fi

		cast(p,ref wordp)^:=wordp(int(freelist[acode]))
		freelist[acode]:=p
	fi
end

global proc pcm_freeac(ref void p,int alloc) =		!PCM_FREEAC
	pcm_free(p,allocupper[alloc])
end

global proc pcm_copymem4(ref void p,q,int n) =	!PCM_COPYMEM4
!copy n bytes of memory from q to p.
!the memory spaces used are multiples of 16 bytes, but n itself could be anything
!n can be zero, and need not be a multiple of 4 bytes

	memcpy(p,q,n)
end

global proc pcm_clearmem(ref void p,int n) =		!PCM_CLEARMEM
	memset(p,0,n)
end

global proc pcm_init =		!PCM_INIT
!set up sizeindextable too
	int j,k,k1,k2
	int64 size
	const limit=1<<33

	alloccode:=0
	if pcm_setup then
		return
	fi

	pcm_newblock(0)

!	ALLOCBASE:=PCHEAPPTR

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

global function pcm_getac(int size)int =		!PCM_GETAC
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

global function pcm_newblock(int itemsize)ref void=
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

	pcheapptr:=p
	pcheapend:=p+pcheapsize

	if pcheapstart=nil then		!this is first block
		pcheapstart:=p
	fi
	pcheapptr+:=itemsize
	return ref u32(p)
end

global function pcm_round(int n)int =		!PCM_ROUND
!for any size n, return actual number of bytes that would be allocated
	static [0:maxblockindex+1]int32 allocbytes=(0,16,32,64,128,256,512,1024,2048)

	if n>maxblocksize then
		return n
	else
		return allocbytes[sizeindextable[n]]
	fi
end

global function pcm_array(int n)int =		!PCM_ARRAY
!n bytes are needed for an array return the number of bytes to be actually allocated
	int m

	if n<=maxblocksize then	!automatic rounding up used for small heap
		return pcm_round(n)
	else				!devise some strategy probably doubling up.
		m:=2048
		while n>m do
			m<<:=1
		od
		return m
	fi

end

global proc pcm_printfreelist(int size,ref wordp p) =		!PCM_PRINTFREELIST
	println "Size: ",size
	while p do
		print " ",,p:"h"
		p:=ref wordp(int(p^))
	od
	puts("")
end

global proc pcm_diags(ref char caption) =		!PCM_DIAGS
	int m

	println "HEAP FREELISTS:",caption

	m:=16
	for i:=1 to 8 do
		pcm_printfreelist(m,freelist[i])
		m<<:=1
	od
end

global function pcm_allocz(int n)ref void =		!PCM_ALLOCZ
	ref void p
	p:=pcm_alloc(n)

	memset(p,0,n)
	return p
end

global function pcm_copyheapstring(ref char s)ref char =
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

global function pcm_copyheapstringn(ref char s,int n)ref char =
	ref char q
	if s=nil then return nil fi

	q:=pcm_alloc(n+1)
	memcpy(q,s,n)
	(q+n)^:=0
	return q
end

global function pcm_copyheapblock(ref char s, int length)ref char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
	ref char q
	if length=0 then return nil fi

	q:=pcm_alloc(length)
	memcpy(q,s,length)
	return q
end

proc addtomemalloc(ref int32 ptr,int size)=
!add ptr to allocated table

!CPL "***************ADD TO ALLOC:",ptr,size
	for i to maxmemalloc do
		if memalloctable[i]=ptr then
			CPL "ALLOC ERROR:",ptr,"ALREADY ALLOCATED\n\n\n"
			stop 2
		fi

		if memalloctable[i]=nil then		!unused entry
			memalloctable[i]:=ptr
			memallocsize[i]:=size
			return
		fi
	od
	CPL "MEMALLOCTABLE FULL\n\n\n\n"; os_getch()
	stop 3
end

proc removefrommemalloc(ref int32 ptr,int size)=
!remove ptr to allocated table

!CPL "------------------************REMOVE FROM ALLOC:",ptr,size

	for i to maxmemalloc do
		if memalloctable[i]=ptr then
			if memallocsize[i]<>size then
				CPL "REMOVE:FOUND",ptr,"IN MEMALLOCTABLE, FREESIZE=",size,", BUT STORED AS BLOCK SIZE:",memallocsize[i]
				abortprogram("MEMSIZE")
			fi
			memalloctable[i]:=nil
			return
		fi
	od
	CPL "CAN'T FIND",ptr,"IN MEMALLOCTABLE",size
	abortprogram("MEM")
	stop 4
end

global function allocmem(int n)ref void =		!ALLOCMEM
	ref void p

	p:=malloc(n)
	if p then
		return p
	fi
	println n,memtotal
	abortprogram("Alloc mem failure")
	return nil
end

global function reallocmem(ref void p,int n)ref void =		!REALLOCMEM
	p:=realloc(p,n)
	return p when p
	println n
	abortprogram("Realloc mem failure")
	return nil
end

global proc abortprogram(ref char s) =		!ABORTPROGRAM
	println s
	print   "ABORTING: Press key..."
!os_getch()
	stop 5
end

global function getfilesize(filehandle handlex)int=		!GETFILESIZE
	word32 p,size

	p:=ftell(handlex)		!current position
	fseek(handlex,0,2)		!get to eof
	size:=ftell(handlex)		!size in bytes
	fseek(handlex,p,seek_set)	!restore position
	return size
end

global proc readrandom(filehandle handlex, ref byte mem, int offset, size) =		!READRANDOM
	int a
	fseek(handlex,offset,seek_set)
	a:=fread(mem,1,size,handlex)			!assign so as to remove gcc warning
end

global function writerandom(filehandle handlex, ref byte mem, int offset,size)int =		!WRITERANDOM
	fseek(handlex,offset,seek_set)
	return fwrite(mem,1,size,handlex)
end

global function setfilepos(filehandle file,int offset)int=
	return fseek(file,offset,0)
end

global function getfilepos(filehandle file)int=
	return ftell(file)
end

global function readfile(ref char filename)ref byte =		!READFILE
	filehandle f
	int size
	ref byte m,p

	f:=fopen(filename,"rb")
	if f=nil then
		return nil
	fi
	rfsize:=size:=getfilesize(f)

	m:=malloc(size+4)		!allow space for etx/zeof etc

	if m=nil then
		return nil
	fi

	readrandom(f,m,0,size)
	p:=m+size			!point to following byte
	p^:=0
	(p+1)^:=26
	(p+2)^:=0			!allow use as string

	fclose(f)
	return m
end

global function writefile(ref char filename,ref byte data,int size)int =
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

global function checkfile(ref char file)int=		!CHECKFILE
	filehandle f
	if f:=fopen(file,"rb") then
		fclose(f)
		return 1
	fi
	return 0
end

global proc readlinen(filehandle handlex,ref char buffer,int size) =		!READLINEN
!size>2
	int ch
	ref char p
	int n
	[0:100]char buff
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

global proc iconvlcn(ref char s,int n) =		!ICONVLCN
	to n do
		s^:=tolower(s^)
		++s
	od
end

global proc iconvucn(ref char s,int n) =		!ICONVUCN
	to n do
		s^:=toupper(s^)
		++s
	od
end

global function convlcstring(ref char s)ichar s0=
	s0:=s
	while (s^) do
		s^:=tolower(s^)
		++s
	od
	s0
end

global function convucstring(ref char s)ichar s0=
	s0:=s
	while (s^) do
		s^:=toupper(s^)
		++s
	od
	s0
end

global function changeext(ref char s,newext)ichar=		!CHANGEEXT
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

global function extractext(ref char s,int period=0)ichar=		!EXTRACTEXT
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

global function extractpath(ref char s)ichar=		!EXTRACTPATH
	static [0:260]char str
	ref char t
	int n

	t:=s+strlen(s)-1		!t points to last char

	while (t>=s) do
		switch t^
		when '\\','/',':' then		!path separator or drive letter terminator assume no extension
			n:=t-s+1			!n is number of chars in path, which includes rightmost / or \ or :
			memcpy(&.str,s,n)
			str[n]:=0
			return &.str
		endswitch
		--t
	od
	return ""			!no path found
end

global function extractfile(ref char s)ichar=		!EXTRACTFILE
	ref char t

	t:=extractpath(s)

	if t^=0 then			!s contains no path
		return s
	fi

	return s+strlen(t)		!point to last part of s that contains the file
	end

	global function extractbasefile(ref char s)ichar=		!EXTRACTBASEFILE
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

global function addext(ref char s,ref char newext)ichar=		!ADDEXT
!when filespec has no extension of its own, add newext
	ref char sext

	sext:=extractext(s,1)

	if sext^=0 then						!no extension not even "."
		return changeext(s,newext)
	fi

	return s							!has own extension; use that
end

global function alloctable(int n, size)ref void =		!ALLOCTABLE
!Allocate table space for n elements, each of size <size>
!Allows for 1-based indexing, so allocates (n+1) elements
	ref void p

	p:=malloc((n+1)*size)

	if not p then
		abortprogram("Alloctable failure")
	fi
	return p
end

global function zalloctable(int n, size)ref void =		!ALLOCTABLE
!Allocate table space for n elements, each of size <size>
!Allows for 1-based indexing, so allocates (n+1) elements
	ref int p

	p:=alloctable(n,size)

	pcm_clearmem(p,(n+1)*size)
	return p
	end

global proc checkfreelists(ichar s)=
	ref wordp p,q
	int64 aa

	for i:=2 to 2 do
		p:=freelist[i]

		while p do
			aa:=int64(p)
			if aa>0xffff'FFFF or aa<100 then
				CPL s,"FREE LIST ERROR",i,p,q
!			os_getch(); stop 1
			fi
			q:=p
			p:=ref wordp(int(p^))
		od

	od
end

global function pcm_alloc32:ref void =		!PCM_ALLOC
	ref byte p
	allocbytes:=32
	smallmemtotal+:=32

	if p:=ref byte(freelist[2]) then		!Items of this block size available
		freelist[2]:=ref wordp(int((freelist[2])^))
		return p
	fi

!No items in freelists: allocate new space in this heap block
	return pcm_alloc(32)
end

global proc pcm_free32(ref void p) =
!n can be the actual size requested it does not need to be the allocated size

	smallmemtotal-:=32
	if mem_check then removefrommemalloc(p,32) fi

	cast(p,ref wordp)^:=wordp(int(freelist[2]))
	freelist[2]:=p
end

global proc outbyte(filehandle f,int x)=
	fwrite(&x,1,1,f)
end

global proc outword16(filehandle f,word x)=
	fwrite(&x,2,1,f)
end

global proc outword(filehandle f,word x)=
	fwrite(&x,4,1,f)
end

global proc outword64(filehandle f,word64 x)=
	fwrite(&x,8,1,f)
end

global function myeof(filehandle f)int=
	int c

	c:=fgetc(f)
	if c=c_eof then return 1 fi
	ungetc(c,f)
	return 0;
end

global function pcm_smallallocz(int n)ref void =
	ref byte p

	allocbytes:=allocupper[alloccode:=sizeindextable[n]]

!No items in freelists: allocate new space in this heap block
	p:=pcheapptr				!Create item at start of remaining pool in heap block
	pcheapptr+:=allocbytes			!Shrink remaining pool

	if pcheapptr>=pcheapend then		!Overflows?
		p:=pcm_newblock(allocbytes)		!Create new heap block, and allocate from start of that
		memset(p,0,n)
		return p
	fi

	memset(p,0,n)

	return p
end

global function pcm_smallalloc(int n)ref void =
	ref byte p

	allocbytes:=allocupper[alloccode:=sizeindextable[n]]

!No items in freelists: allocate new space in this heap block
	p:=pcheapptr				!Create item at start of remaining pool in heap block
	pcheapptr+:=allocbytes			!Shrink remaining pool

	if pcheapptr>=pcheapend then		!Overflows?
		p:=pcm_newblock(allocbytes)		!Create new heap block, and allocate from start of that
		return p
	fi

	return p
end

global proc strbuffer_add(ref strbuffer dest, ichar s, int n=-1)=
	int newlen,oldlen
	ichar newptr

	IF N=0 THEN CPL "N=0" FI

	if n=-1 then
		n:=strlen(s)
	fi

	oldlen:=dest^.length

	if oldlen=0 then				!first string
		dest^.strptr:=pcm_alloc(n+1)
		dest^.allocated:=allocbytes
		dest^.length:=n				!length always excludes terminator
		memcpy(dest^.strptr,s,n)
		(dest^.strptr+n)^:=0
		return
	fi

	newlen:=oldlen+n
	if newlen+1>dest^.allocated then
		newptr:=pcm_alloc(newlen+1)
		memcpy(newptr,dest^.strptr,oldlen)
		dest^.strptr:=newptr
		dest^.allocated:=allocbytes
	fi

	memcpy(dest^.strptr+oldlen,s,n)
	(dest^.strptr+newlen)^:=0

	dest^.length:=newlen
end

global proc gs_init(ref strbuffer dest)=			!INITGENSTR
	pcm_clearmem(dest,strbuffer.bytes)
end

global proc gs_free(ref strbuffer dest)=
	if dest^.allocated then
		pcm_free(dest^.strptr,dest^.allocated)
	fi
end

global proc gs_str(ref strbuffer dest,ichar s)=			!GENSTR
	strbuffer_add(dest,s)
end

global proc gs_char(ref strbuffer dest,int c)=
	[16]char s

	s[1]:=c
	s[2]:=0

	strbuffer_add(dest,&.s,1)
end

global proc gs_strn(ref strbuffer dest,ichar s,int length)=
	strbuffer_add(dest,s,length)
end

global proc gs_strvar(ref strbuffer dest,s)=			!GENSTR
	strbuffer_add(dest,s^.strptr)
end

global proc gs_strint(ref strbuffer dest,int64 a)=
	strbuffer_add(dest,strint(a))
end

global proc gs_strln(ref strbuffer dest,ichar s)=		!GENSTRLN
	gs_str(dest,s)
	gs_line(dest)
end

global proc gs_strsp(ref strbuffer dest,ichar s)=
	gs_str(dest,s)
	gs_str(dest," ")
end

global proc gs_line(ref strbuffer dest)=
	strbuffer_add(dest,"\w")
end

global function gs_getcol(ref strbuffer dest)int=
	return dest^.length
end

global proc gs_leftstr(ref strbuffer dest, ichar s, int w, padch=' ')=
	int col,i,n,slen
	[2560]char str
	col:=dest^.length
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

global proc gs_leftint(ref strbuffer dest, int a, int w, padch=' ')=
	gs_leftstr(dest,strint(a),w,padch)
end

global proc gs_padto(ref strbuffer dest,int col, ch=' ')=
	int n
	[2560]char str

	n:=col-dest^.length
	if n<=0 then return fi
	for i:=1 to n do
		str[i]:=ch
	od
	str[n+1]:=0
	gs_str(dest,&.str)
end

global proc gs_println(ref strbuffer dest,filehandle f=nil)=
	(dest.strptr+dest.length)^:=0

	if f=nil then
		println dest.strptr,,"\c"
	else
		println @f,dest.strptr,,"\c"
	fi
end

global function nextcmdparam(int &paramno, ichar &name, &value, ichar defext=nil)int=
	static int infile=0
	static ichar filestart=nil
	static ichar fileptr=nil
	static byte colonseen=0
	ref char q
	ichar item,fileext
	ichar rest
	int length
	static [300]char str

	reenter::
	value:=nil
	name:=nil

	if infile then
		if readnextfileitem(fileptr,item)=0 then		!eof
			free(filestart)								!file allocated via malloc
			infile:=0
			goto reenter
		fi
	else
		if paramno>nsysparams then
			return pm_end
		fi
		item:=sysparams[paramno]
		++paramno

		length:=strlen(item)

		if item^='@' then		!@ file
			filestart:=fileptr:=cast(readfile(item+1))
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
	elsif eqstring(fileext,"dll") then
		return (colonseen|pm_extra|pm_libfile)
	fi
	return (colonseen|pm_extra|pm_sourcefile)
end

function readnextfileitem(ichar &fileptr,&item)int=
	ref char p,pstart,pend
	int n
	static [256]char str

	p:=fileptr

	reenter::
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

		enddocase
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

global proc ipadstr(ref char s,int width,ref char padchar=" ")=
	int n

	n:=strlen(s)
	to width-n do
		strcat(s,padchar)
	od
end

global function padstr(ref char s,int width,ref char padchar=" ")ichar=
	static [256]char str

	strcpy(&.str,s)
	ipadstr(&.str,width,padchar)
	return &.str
end

global function chr(int c)ichar=
	static [8]char str

	str[1]:=c
	str[2]:=0
	return &.str
end

global function cmpstring(ichar s,t)int=
	int res
	if (res:=strcmp(s,t))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

global function cmpstringn(ichar s,t,int n)int=
	int res
	if (res:=strncmp(s,t,n))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

global function eqstring(ichar s,t)int=
	return strcmp(s,t)=0
end

global function cmpbytes(ref void p,q,int n)int=
	int res
	if (res:=memcmp(p,q,n))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

global function eqbytes(ref void p,q,int n)int=
	return memcmp(p,q,n)=0
end

global proc mseed(word64 a,b=0)=
	seed[1]:=a
	if b then
		seed[2]:=b
	else
		seed[2] ixor:=a
	fi
end

global function mrandom:word =
!return pure 64-bit word value, 0 to 2**64-1
!(cast result for signed value)
!	word64 x,y
	int x,y
	x:=seed[1]
	y:=seed[2]
	seed[1]:=y
	x ixor:=(x<<23)
	seed[2]:= x ixor y ixor (x>>17) ixor (y>>26)
	return seed[2]+y
end

global function mrandomp:int =
!pure 64-bit int value, positive only, 0 to 2**63-1
	return mrandom() iand 0x7FFF'FFFF'FFFF'FFFF
end

global function mrandomint(int n)int=
!positive random int value from 0 to n-1
	return mrandomp() rem n
end

global function mrandomrange(int a,b)int=
!random int value from a to b inclusive
!span extent must be 1 to 2**63-1
	int span
	span:=b-a+1
	if span<=0 then
		return 0
	fi
	return (mrandomp() rem span)+a
end

global function mrandomreal:real x=
!positive random real value from 0 to just under (but not including) 1.0
	repeat x:=mrandomp()/9223372036854775808.0 until x<>1.0
	return x
end

global function mrandomreal1:real=
!positive random real value from 0 to 1.0 inclusive
	return mrandomp()/9223372036854775807
end

global function checkpackfile:ref byte=
!find out if this executable contains extra packed files
!return 1 or 0

	int a,offset,i,size
	[100]char name
	[300]char exefile
	ref byte packexeptr			!for embedded pack files, contains pointer to in-memory version of this .exe file plus extras; else nil
	int packexesize				!byte size
	ref char packfilename
	int packfilesize
	ref byte packfileptr

!macro getfileint(data,offset)=(ref int32(data+offset))^
	macro getfileint(data,offset)=cast(data+offset,ref int32)^

	strcpy(&exefile[1],os_gethostname())
	println "Attempting to open",&exefile
	packexeptr:=readfile(&exefile[1])

	if not packexeptr then
		cpl "Can't open",&exefile,&packexeptr
		stop
	fi

	packexesize:=rfsize
	cpl "File read OK. Size",packexesize

	a:=getfileint(packexeptr,packexesize-int32.bytes)
	if a<>'PCAK' then
		free(packexeptr)
		packfileptr:=nil
		return nil
	fi

	offset:=getfileint(packexeptr,packexesize-int32.bytes*2)

	packfilename:=cast(packexeptr+offset)
	offset+:=strlen(packfilename)+1
	packfilesize:=getfileint(packexeptr,offset)
	packfileptr:=packexeptr+offset+int32.bytes

	return packfileptr
end

global function pcm_allocx:ref void =
	const n=32
	ref word p

	allocbytes:=32

	if p:=ref word(freelist[2]) then		!Items of this block size available
		freelist[2]:=ref wordp(int((freelist[2])^))

	else

!No items in freelists: allocate new space in this heap block
		p:=cast(pcheapptr)				!Create item at start of remaining pool in heap block
		pcheapptr+:=32			!Shrink remaining pool

		if pcheapptr>=pcheapend then		!Overflows?
			p:=pcm_newblock(32)		!Create new heap block, and allocate from start of that
		fi

		p^:=0
		(p+1)^:=0
		(p+2)^:=0
		(p+3)^:=0

		return p
	fi
end

global function readline:ichar=
	readln
	return rd_buffer
end

global function stralloc(ref void p)ichar=
	return strint(int(ref byte(p)-allocbase))
end

global function findfunction(ichar name)ref void=
	for i to $getnprocs() do
		if eqstring($getprocname(i),name) then
			return $getprocaddr(i)
		fi
	od
	return nil
end

