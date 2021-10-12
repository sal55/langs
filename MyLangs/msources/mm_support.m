import clib
import msys
import mlib
import oslib

import mm_decls
import mm_lib
import mm_tables
import mm_libsources

global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

global function loadsourcefile(ichar filespec)int=
!file is a complete file spec of a file known to exist
!shortfile is the name as it might appear in an include statement; part- or fully-qualified
!return index into sourcefile tables
	ichar s,shortfile

	if nsourcefiles>maxsourcefile then
		loaderror("Too many source files")
	fi

	shortfile:=extractfile(filespec)

	++nsourcefiles
	sourcefilepaths[nsourcefiles]:=pcm_copyheapstring(filespec)
	sourcefilenames[nsourcefiles]:=pcm_copyheapstring(shortfile)

	s:=cast(readfile(filespec))			!will overallocate by a few bytes
	if not s then				!unexpected error
		loaderror("LSF can't load ",filespec)
	fi
	sourcefiletext[nsourcefiles]:=s

	if fwritema then
		mafiletext[nsourcefiles]:=pcm_copyheapstring(s)
	fi

	sourcefilesizes[nsourcefiles]:=rfsize
	(s+rfsize)^:=0				!replace etx,0 by 0,0 (effectively, just zero)
	return nsourcefiles
end

global function loadbuiltin(ichar shortfile, text)int=
!loading build-in header with given text
!Name of header is in 'file'.
	ichar s
	[128]char str

	if nsourcefiles>maxsourcefile then
		loaderror("Too many source files")
	fi
	++nsourcefiles
	fprint @&.str,"<Built-in: #>",shortfile

	sourcefilepaths[nsourcefiles]:=pcm_copyheapstring(&.str)
	sourcefilenames[nsourcefiles]:=pcm_copyheapstring(shortfile)

!source code may be written to (avoids doing that with names
!but may happen with real numbers); need to make writeable copy
!sourcefiletext[nsourcefiles]:=hdrtext
	sourcefiletext[nsourcefiles]:=pcm_copyheapstring(text)
	if fwritema then
		mafiletext[nsourcefiles]:=pcm_copyheapstring(text)
	fi

	sourcefilesizes[nsourcefiles]:=strlen(text)
	return nsourcefiles
end

global function loadbundledfile(ichar filespec,int support=0)int fileno=
!loading bundled file
!Name of header is in 'file'.
	ichar file
	int n,lastmatch

	file:=extractfile(filespec)

	for i to nmafiles do
		if eqstring(file,mafilenames[i]) and support=mafilesupport[i] then		!found
!		if eqstring(file,mafilenames[i]) then		!found
			fileno:=mafilefileno[i]
			if not fileno then					!cannot overflow sourcefiles; same limits?
				fileno:=++nsourcefiles
				mafilefileno[i]:=fileno

				sourcefilepaths[nsourcefiles]:=mafilenames[i]
				sourcefilenames[nsourcefiles]:=mafilenames[i]
				sourcefiletext[nsourcefiles]:=mafiletext[i]
				sourcefilesizes[nsourcefiles]:=mafilesizes[i]

				sourcefiletext[nsourcefiles]:=pcm_copyheapstring(mafiletext[i])

			fi
			return fileno
		fi
	od
!
	loaderror("Can't find bundled file: # #",filespec)
	return 0
end

global proc mcerror(ichar mess)=
	println "MC Error:",mess

	stop 1
end

global proc serror_gen(ichar mess)=

	if currproc and currproc^.nameid=procid then
		print "In function",currproc^.name,," "
	fi

	println "On line",lx.lineno,"in file",sourcefilepaths[lx.fileno],sourcefilenames[lx.fileno]

	println
	println "**** Syntax Error:",mess,"****"
	stopcompiler(sourcefilepaths[lx.fileno],lx.lineno)
end

global proc stopcompiler(ichar filename,int lineno)=
	filehandle f
	f:=fopen("$error.tmp","w")
	println @f,filename,lineno
	fclose(f)
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
	int lineno,fileno

	if p then
		fileno:=p.fileno
		lineno:=p.lineno
	else
		fileno:=mlineno>>24
		lineno:=mlineno iand 16777215
	fi

	if currproc and currproc^.nameid=procid then
		print "In function",currproc^.name,," "
	fi

	println "On line",lineno iand 16777215,"in file",sourcefilepaths[fileno]
	println
	case pass
	when 'N' then print "**** RX Name Error: "
	when 'T' then print "**** TX Type Error: "
	when 'G' then print "**** GX Code Gen Error: "
	when 'A' then print "**** 'AX Code Gen Error: "
	esac
	println mess

	os_getch()

	stopcompiler(sourcefilepaths[fileno],lineno iand 16777215)
end

global proc rxerror(ichar mess,unit p=nil)=
	error_gen('N',mess,p)
end

global proc gerror(ichar mess,unit p=nil)=
	error_gen('G',mess,p)
end

global proc axerror(ichar mess)=
	CPL =ALINENO
	error_gen('A',mess)
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

	println "On line",lx.lineno,"in file",sourcefilepaths[lx.fileno]

	println
	println "**** Lex Error:",mess,"****"
	println

	stopcompiler(sourcefilepaths[lx.fileno],lx.lineno)
end

global proc lxerror(ichar mess)=
	lxerror_gen(mess)
end

global proc loaderror(ichar mess,mess2="",mess3="")=
	[512]char str
	fprint @&.str,mess,mess2,mess3

	println "Load Error:",&.str
	println "Stopping"
	stop 1
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

global proc gs_copytostr(ref strbuffer source,ref char s)=
	if source^.length then
		memcpy(s,source^.strptr,source^.length)
		(s+source^.length)^:=0
	else
		s^:=0
	fi
end

global function isalphanum(int c)int=
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
		bitsize:=stdbits[i]

		switch bitsize
		when 0 then
			size:=0
		when 1,2,4 then
			size:=1
		else
			size:=bitsize/8
		endswitch

		ttsize[i]:=size

		case stdcodes[i]
		when 'I' then
			ttisint[i]:=1
			ttisinteger[i]:=1
		when 'U','C' then
			ttisword[i]:=1
			ttisinteger[i]:=1
		when 'R' then
			ttisreal[i]:=1
		when 'P','Q' then
			ttisref[i]:=1
		esac

		if ttisinteger[i] and size<8 then
			ttisshort[i]:=1
		fi

		if ttisinteger[i] or ttisreal[i] then
			ttisallnum[i]:=1
			if not ttisshort[i] then
				ttismainnum[i]:=1
			fi
		fi

		ttlower[i]:=1
	od

	ttbasetype[trefchar]:=tref
	tttarget[trefchar]:=tc8

	ntypes:=tlast-1
end

global proc addspecialtypes=
	trefproc:=createrefmode(nil,tproc,0)
	treflabel:=createrefmode(nil,tlabel,0)
end

function findfile(ichar filename)ichar=
!look for file by searching through search dirs
!return nil when not found, or the name of the sucessful filespec
!locate module within search paths
!return full filespec
	static [300]char filespec

	if fverbose=3 then
		println "Finding",filename
	fi

	for i:=nsearchdirs downto 1 do
		strcpy(&.filespec,searchdirs[i])
		strcat(&.filespec,filename)
		if fverbose=3 then
			println "	Checking:",&.filespec
		fi

		if checkfile(&.filespec) then
			if fverbose=3 then
					println "	Found:",&.filespec
			fi
			return &.filespec
		fi
	od

	return nil
end

global function getmainfile(ichar filename)int =
!locate and load lead module filename
	if fbundled then
		return loadbundledfile(filename)
	fi
	if not checkfile(filename) then
		loaderror("Can't find main module: ##",filename)
	fi
	return loadsourcefile(filename)
end

global function getmodulefile(ichar modulename, ownername, int xdflag)int =
	[300]char filename
	ichar file,libtext

	strcpy(&.filename,addext(modulename,(xdflag|"exp"|"m")))

	if fbundled then
		return loadbundledfile(&.filename)
	fi

	if dointlibs then
		libtext:=findstdlib(&.filename)
		if libtext then
			return loadbuiltin(&.filename,libtext)
		fi
	fi

	file:=findfile(&.filename)

	if file=nil then
		loaderror("Can't find import module: # imported in: #",&.filename,ownername)
	fi
	return loadsourcefile(file)
end

global function getsupportfile(ichar filename)int =
	ichar path,file
	int fileno

	if fbundled then
		return loadbundledfile(filename,1)
	fi

	path:=extractpath(filename)
	if path^ in ['\\','/'] or path^<>0 and (path+1)^=':' then	!absolute path
		file:=filename
	else
		file:=findfile(filename)
	fi

	if file=nil or not checkfile(file) then
		loaderror("Can't find include file: # #",filename)
	fi

	fileno:=loadsourcefile(file)
	issupportfile[fileno]:=1
	return fileno
end

global proc writemafile(ichar leadmodule,destfile)=
	[256]char filename
	filehandle f
	[maxsourcefile]int fileoffsets, headeroffsets
	int offset,nn,NEWOFFSET

	strcpy(&.filename, changeext(leadmodule,"ma"))

	if destfile then
		strcpy(&.filename,destfile)
	fi

	println "Writing MA File",&.filename

	f:=fopen(&.filename,"wb")
	if not f then loaderror("Can't create ma file #",&.filename) fi

	println @f,"mafile",nsourcefiles

	for i to nsourcefiles do
		print @f,i:"3",sourcefilenames[i]:"16jl",sourcefilesizes[i]:"7"
		headeroffsets[i]:=getfilepos(f)+1
		println @f,"           ",issupportfile[i]
	od

	for i to nsourcefiles do
		fprintln @f,"=== # #/# ===",sourcefilenames[i],i,nsourcefiles

		offset:=getfilepos(f)
		fileoffsets[i]:=offset
		nn:=writerandom(f,cast(mafiletext[i]),offset,sourcefilesizes[i])
	od

!Note: the first "=" of the "===" that follows each file may be replaced
!by a zero-terminator after the .ma is loaded
	println @f,"=== end ==="

	for i to nsourcefiles do
		setfilepos(f,headeroffsets[i])
		print @f,fileoffsets[i]:"8"
	od
!
	fclose(f)
end

global proc loadmafile=
	filehandle f
	[16]char kwd
	[256]char filename
	int index, size, offset, issupport

	f:=fopen(mafilename,"rb")
	if not f then
		loaderror("Can't open ##",mafilename)
	fi

	readln @f

	readstr(&.kwd,'n',kwd.len)
	if not eqstring(&.kwd,"mafile") then
		loaderror("Bad sig in ma file: # '#'",mafilename,&.kwd)
	fi

	read nmafiles

	for i to nmafiles do
		readln @f,index
		readstr(&.filename,'n',filename.len)
		read size, offset, issupport
		mafilenames[i]:=pcm_copyheapstring(&.filename)
		mafilesizes[i]:=size
		mafileoffsets[i]:=offset
		mafilefileno[i]:=0
		mafilesupport[i]:=issupport
	od
	fclose(f)

!Directory has been read. Now read whole file into memory, use directory
!to set up mafiletext values to each file, and add in terminator
	mafilesource:=cast(readfile(mafilename))
	if not mafilesource then loaderror("MA load?") fi

	for i to nmafiles do
		size:=mafilesizes[i]
		offset:=mafileoffsets[i]

		mafiletext[i]:=mafilesource+offset
		(mafilesource+offset+size)^:=0
	od
end

global function mapimport(ichar name)ichar=
	for i to nmodulemap do
		if eqstring(name,genericmodules[i]) then
			return actualmodules[i]
		fi
	od
	return name
end

global proc initbblib=
	for i:=1 to D_typestarterset.len do typestarterset[D_typestarterset[i]]:=1 od
end
