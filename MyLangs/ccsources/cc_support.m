import clib
import mlib
import oslib

import cc_decls
import cc_tables


global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

proc stopcompiler(ichar filename,int lineno)=
	filehandle f

	println
	println
	stop 1
end

global proc mcerror(ichar mess)=
println "MC Error:",mess
!os_getch()
stop 40
end

global proc serror(ichar mess)=
serror_gen(mess)
end

global proc serror_gen(ichar mess)=
if currproc then
	print "In function",currproc^.name,," "
fi

println "On line",lx.lineno,"in file",sourcefilepaths[lx.fileno],sourcefilenames[lx.fileno]
showmacrolineno()

println
println "**** Syntax Error:",mess,"****"
println

stopcompiler(sourcefilepaths[lx.fileno],lx.lineno)
end

global proc serror_ss(ichar mess,a,b)=
[256]char str
sprintf(&.str,mess,a,b)
serror_gen(&.str)
end

global proc serror_s(ichar mess,a)=
[256]char str
sprintf(&.str,mess,a)
serror_gen(&.str)
end

global proc terror_gen(ichar mess)=

if currproc then
	println "In function",currproc^.name
fi

println "Type error:",mess,"on line",lx.lineno,sourcefilepaths[lx.fileno]

showmacrolineno()

stopcompiler(sourcefilepaths[lx.fileno],lx.lineno)
end

global proc terror(ichar mess)=
terror_gen(mess)
end

global proc terror_s(ichar mess,a)=
[256]char str

sprintf(&.str,mess,a)
terror_gen(&.str)
end

global proc terror_ss(ichar mess,a,b)=
[256]char str

sprintf(&.str,mess,a,b)
terror_gen(&.str)
end

global proc gerror_gen(ichar mess,ref unitrec p=nil)=
int lineno,fileno

if p then
!CPL "GERROR/P GIVEN"
	lineno:=p^.lineno
	fileno:=p.fileno
else
	lineno:=clineno
	fileno:=lineno>>24
fi

if currproc then
	print "In function",currproc^.name,," "
fi

!CPL =LINENO,CLINENO, P.LINENO,P.FILENO
println "On line",lineno iand 16777215,"in file",sourcefilepaths[fileno]
println
println "**** Code Gen Error:",mess,"****"
stopcompiler(sourcefilepaths[fileno],lineno)
end


global proc gerror(ichar mess,ref unitrec p=nil)=
gerror_gen(mess,p)
end

global proc gerror_s(ichar mess,s,ref unitrec p=nil)=
[256]char str

sprintf(&.str,mess,s)
gerror_gen(&.str,p)
end

global proc nxerror(ichar mess,ref unitrec p=nil)=
int lineno

if p then
	lineno:=p^.lineno
else
	lineno:=0
fi
println "NX error:",mess,"on line",lineno,stmodule^.name
!os_getch()
stopcompiler(stmodule^.name,lineno)
end

global function testelem(ref[0:]byte p,int n)int =		!TESTELEM
!caller must check that n is in range
return ((p^[n>>3] iand bytemasks[n iand 7])|1|0)
end

global proc setelem(ref[0:]byte p,int n) =		!SETELEM
p^[n>>3] ior:= bytemasks[n iand 7]
end

global function nextpoweroftwo(int x)int=
!return next power of 2 >= x

if x=0 then return 0 fi

int a:=1
while a<x do
	a<<:=1
od
return a
end

global proc loaderror(ichar mess,mess2="")=
[512]char str

sprintf(&.str,mess,mess2)
println "Load Error:",&.str
println "Stopping"
stop 45
end

global function loadfromstdin(ichar file)int=
ichar s
[30000]CHAR SRC
ref char p
int n,c

if nsourcefiles>maxsourcefile then
	loaderror("Too many source files")
fi
++nsourcefiles
sourcefilepaths[nsourcefiles]:=pcm_copyheapstring(file)
sourcefilenames[nsourcefiles]:=pcm_copyheapstring(file)

println "Reading from stdin. Finish with Ctrl-Z:"
p:=&.src
n:=0
while (c:=getchar())<>c_eof do
	p++^:=c
	if ++n>=src.len then
		loaderror("stdin overflow")
	fi
od
!p++^:=26
p^:=0	

sourcefiletext[nsourcefiles]:=pcm_copyheapstring(&.src)
sourcefilesizes[nsourcefiles]:=strlen(&.src)
return nsourcefiles
end

global function loadsourcefile(ichar file,shortfile)int=
!file is a complete file spec of a file known to exist
!shortfile is the name as it might appear in an include statement; part- or fully-qualified
!return index into sourcefile tables
ichar s

if nsourcefiles>maxsourcefile then
	loaderror("Too many source files")
fi
++nsourcefiles
sourcefilepaths[nsourcefiles]:=pcm_copyheapstring(file)
sourcefilenames[nsourcefiles]:=pcm_copyheapstring(shortfile)

s:=cast(readfile(file))			!will overallocate by a few bytes
if not s then				!unexpected error
	loaderror("LSF can't load ",file)
fi

if flinesplicing then
!CPL "SPLICELINES",FILE
	s:=splicelines(s)
fi

!CPL "SETFILETEXT2",=NSOURCEFILES,REF VOID S,=RFSIZE
sourcefiletext[nsourcefiles]:=s
sourcefilesizes[nsourcefiles]:=rfsize
(s+rfsize)^:=0				!replace etx,0 by 0,0 (effectively, just zero)

return nsourcefiles
end

function splicelines(ichar s)ichar=
	ichar t,u

	t:=u:=pcm_alloc(strlen(s)+1)

	while s^ do
		if s^='\\' and (s+1)^=10 then s+:=2
		elsif s^='\\' and (s+1)^=13 and (s+2)^=10 then s+:=3
		else t++^ := s++^
		fi
		t^:=0
	od
	return u
end

global function loadbuiltin(ichar shortfile,hdrtext)int=
!loading build-in header with text at hdrtext
!Name of header is in 'file'.
ichar s

if nsourcefiles>maxsourcefile then
	loaderror("Too many source files")
fi
++nsourcefiles
sourcefilepaths[nsourcefiles]:="<builtin>"
sourcefilenames[nsourcefiles]:=pcm_copyheapstring(shortfile)

!source code may be written to (avoids doing that with names
!but may happen with real numbers); need to make writeable copy
!sourcefiletext[nsourcefiles]:=hdrtext
sourcefiletext[nsourcefiles]:=pcm_copyheapstring(hdrtext)
!CPL "SETFILETEXT3",=NSOURCEFILES,REF VOID SOURCEFILETEXT[NSOURCEFILES]

sourcefilesizes[nsourcefiles]:=strlen(hdrtext)
return nsourcefiles
end

proc gs_copytostr(ref strbuffer source,ref char s)=
if source^.length then
	memcpy(s,source^.strptr,source^.length)
	(s+source^.length)^:=0
else
	s^:=0
fi
end

global proc gs_additem(ref strbuffer dest,ichar s)=		!GENITEM
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

function isalphanum(int c)int=
if c>='A' and c<='Z' or c>='a' and c<='z' or c>='0' and c<='9' then
	return 1
fi
return 0
end

proc showmacrolineno=
if slineno then
	println "	(Last macro invoked near line",
		slineno,"in file",sourcefilenames[sfileno],,")"
fi
end
