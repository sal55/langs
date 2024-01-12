
global func loadsp(ichar filename, source=nil)isubprog sp=
!source = nil:  load lead module and dependencies from given sourcefile
!source <> nil: source code is given directly. filename can give a name
! to that source text, or if nil, and internal name is applied

	const maxmods=100
	const maxsubs=100
	[maxmods]ichar modnames
	[maxsubs]ichar subnames
	int nmods:=0, nsubs:=0
	int firstmod, lastmod, issyslib:=0
	ifile pm
	symbol d
	[300]char path

!CPL "LOADSP",FILENAME

	if source then
		pm:=loadstring(filename, source)
		path[1]:=0
	else
		if eqstring(extractbasefile(filename), syslibname) then
			issyslib:=1
		fi

		pm:=loadsourcefile(filename, issyslib)
		if pm=nil then
			loaderror("Can't load lead module: #", filename)
		fi
		strcpy(path, pm.path)
	fi

	for i to nsubprogs do
		if eqstring(pm.name, subprogs[i].name) then
			loaderror("Subprog already loaded: #", sp.name)
		fi
	od

!reader header info
	startlex(pm)

	do
		lex()
		skipsemi()
		case lx.symbol
		when kmodulesym then
			lex()
			checksymbol(namesym)
			if not eqstring(lx.symptr.name, pm.name) then
				if nmods>=maxmods then loaderror("Too many modules in header") fi
				modnames[++nmods]:=lx.symptr.name
			fi

		when kimportsym then
			lex()
			checksymbol(namesym)
			if nsubs>=maxsubs then loaderror("Too many imports in header") fi
			subnames[++nsubs]:=lx.symptr.name

		when semisym then
		else
			exit
		esac
	od

!process nested imports
	for i to nsubs do
		if eqstring(subnames[i],pm.name) then loaderror("Importing self") fi
		compile_sp(getmodulefilename("", subnames[i]))				!recursive load
	od

!create new subprog entry
	if nsubprogs>=maxsubprog then loaderror("Too many subprogs") fi
	sp:=pcm_allocz(subprogrec.bytes)
	subprogs[++nsubprogs]:=sp
	sp.subprogno:=nsubprogs

	firstmod:=nmodules+1
	lastmod:=firstmod+nmods
	if lastmod>maxmodule then loaderror("Too many modules") fi
	nmodules:=lastmod
	pm.subprogno:=nsubprogs
	pm.islead:=1
	pm.moduleno:=firstmod
	pm.stmodule:=d:=createdupldef(stprogram,addnamestr(pm.name),moduleid)
	d.moduleno:=firstmod

	sp.name:=pm.name
	sp.path:=pm.path
	sp.filespec:=pm.filespec
	sp.firstmodule:=firstmod
	sp.lastmodule:=lastmod
	sp.issyslib:=issyslib

!create new set of modules[] entries and load those other modules
!create stmodule entries for each module
	modules[firstmod]:=pm

	for i to nmods do
		pm:=loadsourcefile(getmodulefilename(path, modnames[i]), issyslib)
		if not pm then
			loaderror("Can't load: ##",modnames[i])
		fi
		modules[firstmod+i]:=pm
		pm.stmodule:=d:=createdupldef(stprogram,addnamestr(pm.name),moduleid)
		pm.subprogno:=nsubprogs
		d.moduleno:=pm.moduleno:=firstmod+i
	od

	return sp
end

func getmodulefilename(ichar path, name)ichar =
	static [300]char str

!need to sort out search path etc

	strcpy(str, path)
	strcat(str, name)
	strcat(str, ".q")
	return str
end

global func loadsourcefile(ichar filespec, int issyslib=0)ifile pm=
	ichar s,basefilename
	[300]char str

	pm:=pcm_allocz(filerec.bytes)

	basefilename:=extractbasefile(filespec)


	pm.filespec:=pcm_copyheapstring(filespec)
	pm.path:=pcm_copyheapstring(extractpath(filespec))
	pm.name:=pcm_copyheapstring(basefilename)
	pm.issyslib:=issyslib

	if nqafiles and loadqafile(pm) then
		return pm
	fi

	if issyslib and usebundled then
		pm.issyslib:=issyslib
		if not loadsysmodule(pm) then
			loaderror("LS:Can't load syslib",filespec)
		fi
!CPL "LOADED",FILESPEC,"FROM BUNDLE"
		return pm
	fi

	s:=cast(readfile(filespec))			!will overallocate by a few bytes
	if not s then				!unexpected error
		return nil
	fi
!CPL "LOADED",FILESPEC,"FROM FILE"
	pm.text:=s

	pm.size:=rfsize

	(s+rfsize)^:=0				!replace etx,0 by 0,0 (effectively, just zero)
	return pm
end

global func loadstring(ichar name, source)ifile pm=
	[16]char str
	static int nextstrname=0

	if name=nil then
		print @str, "S$",,++nextstrname
		name:=pcm_copyheapstring(str)
	fi

	pm:=pcm_allocz(filerec.bytes)

	pm.filespec:="<string>"
	pm.path:=""
	pm.name:=name

	pm.text:=source

	pm.size:=strlen(source)
	return pm
end

function readfileline(ichar s)ichar =
!s points into a string representing an entire file, which should be 0-terminated
!read the line s until the next eol or eof, into a line buffer
!return a pointer to the next line
	array [2048]char str
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

function findnextlineheader(ichar s)ichar=
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

function loadqafile(ifile pm)int=
	ichar file
	[300]char filename

	strcpy(filename, extractfile(pm.filespec))
!	strcpy(filename, (pm.filespec))

	for i to nqafiles do
		if eqstring(filename,qafilenames[i]) then		!found
			pm.text:=qatext[i]
			pm.size:=qasize[i]
			return 1
		fi
	od
	return 0
end

global proc readqabundle=
	[100]char name
	ichar s, t
	int sys, support

	s:=extractext(inputfile)
	convlcstring(s)
	unless eqstring(s,"qa") then
		return
	end

!Input is a .qa file; load files into qa directory
	s:=readfile(inputfile)
	if s=nil then							!file not found on disk
		loaderror("Can't find QA file ##",inputfile)
	fi

!change inputfile from .qa to .q (later, change to suitable lead module within qa file)
	inputfile:=pcm_copyheapstring(changeext(inputfile,"q"))

	s:=readfileline(s+3)
	readstr(name,'n')
	if not eqstring(name,"qa") then
		loaderror("QA: bad header")
	fi

	--s					!point to previous lf

	s:=findnextlineheader(s)

	do
		if s=nil then
			loaderror("Unexpected EOF in QA file")
			exit
		fi
		s:=readfileline(s)
		readstr(name,'n')
		read sys,support
!		println "Found file",name
		if eqstring(name,"end") then
			exit
		fi
		if nqafiles>=maxqafile then
			loaderror("Too many QA files")
		fi

		t:=findnextlineheader(s)
		if t=nil then
			loaderror("QA error")
		fi

		++nqafiles

		qafilenames[nqafiles]:=pcm_copyheapstring(name)
		qasize[nqafiles]:=t-s-3
		qatext[nqafiles]:=s
		s:=t
	od
!
	for i to nqafiles do
		(qatext[i]+qasize[i])^:=0	
	od
end
