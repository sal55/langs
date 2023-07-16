global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

global function loadsourcefile(ichar filespec)int=
!file is a complete file spec of a file known to exist
!shortfile is the name as it might appear in an include statement; part- or fully-qualified
!return index into sourcefile tables
	ichar s,basefilename

	if nsourcefiles>maxsourcefile then
		loaderror("Too many source files")
	fi

	basefilename:=extractfile(filespec)

	++nsourcefiles
	sourcefilespecs[nsourcefiles]:=pcm_copyheapstring(filespec)
	sourcefilepaths[nsourcefiles]:=pcm_copyheapstring(extractpath(filespec))
	sourcefilenames[nsourcefiles]:=pcm_copyheapstring(basefilename)

	s:=cast(readfile(filespec))			!will overallocate by a few bytes
	if not s then				!unexpected error
		loaderror("LSF can't load ",filespec)
	fi
	sourcefiletext[nsourcefiles]:=s

	if fwritema then
		sourcefiledupl[nsourcefiles]:=pcm_copyheapstring(s)
	fi

	sourcefilesizes[nsourcefiles]:=rfsize
	(s+rfsize)^:=0				!replace etx,0 by 0,0 (effectively, just zero)
	return nsourcefiles
end

global function loadbuiltin(ichar shortfile, text)int=
!loading built-in file with given text, which has just been located,
!and add to the list of sourcefiles
	ichar s

	if nsourcefiles>maxsourcefile then
		loaderror("Too many source files")
	fi
	++nsourcefiles

	sourcefilepaths[nsourcefiles]:=""
	sourcefilespecs[nsourcefiles]:=sourcefilenames[nsourcefiles]:=pcm_copyheapstring(shortfile)
	sourcefilesys[nsourcefiles]:=1

!source code may be written to (avoids doing that with names
!but may happen with real numbers); need to make writeable copy
!sourcefiletext[nsourcefiles]:=hdrtext
	sourcefiletext[nsourcefiles]:=pcm_copyheapstring(text)
	if fwritema then
		sourcefiledupl[nsourcefiles]:=pcm_copyheapstring(text)
	fi

	sourcefilesizes[nsourcefiles]:=strlen(text)
	return nsourcefiles
end

function loadbundledfile(ichar filespec,int issyslib=0,support=0)int fileno=
!loading bundled file
!Name of header is in 'file'.
	ichar file

	file:=extractfile(filespec)

	for i to nsourcefiles do
		if eqstring(file,sourcefilenames[i]) and support=sourcefilesupport[i] then		!found
			return i
		fi
	od

	fileno:=findsyslib(file)
	if fileno then
		return fileno
	fi

	if not issyslib then
		loaderror("Can't find bundled file: ##",filespec)
	fi
	return 0
end

global proc mcerror(ichar mess)=
	println "MC Error:",mess

	stop 1
end

global proc serror_gen(ichar mess)=
	showdivider('*')
	println "Syntax Error:"

	showerrorsource(lx.pos, currproc)

	println mess

	stopcompiler(sourcefilespecs[lx.fileno],getlineno(lx.pos))
end

proc showdivider(char64 ch)=
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
	fprintln "    Module:   # (#)", sourcefilenames[fileno],sourcefilespecs[fileno]
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
		pos:=p.pos
	else
		pos:=mlineno
	fi

!	if currproc and currproc^.nameid=procid then
!		print "In function",currproc^.name,," "
!	fi

!!	println "On line",lineno iand 16777215,"in file",sourcefilepaths[fileno]
!	println "On line",lineno iand 16777215,"in file",sourcefilespecs[fileno]
!	println

	showdivider('*')
	case pass
	when 'N' then println "RX Name Error: "
	when 'T' then println "TX Type Error: "
	when 'G' then println "GX Code Gen Error: "
	when 'A' then println "AX Code Gen Error: "
	esac

!CPL "ERROR SOURCE:",POS.[0..23],POS.[24..31]
	showerrorsource(pos, currproc)
!CPL "DONE2"

	println mess

	stopcompiler(sourcefilespecs[getfileno(pos)],getlineno(pos))
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

	println "On line",getlineno(lx.pos),"in file",sourcefilespecs[lx.fileno]

	println
	println "**** Lex Error:",mess,"****"
	println

	stopcompiler(sourcefilespecs[lx.fileno],getlineno(lx.pos))
end

global proc lxerror(ichar mess)=
	lxerror_gen(mess)
end

global proc loaderror(ichar mess,mess2="",mess3="")=
	[512]char str
	if strchr(mess,'#') then
		fprint @str,mess,mess2,mess3
	else
		print @str,mess
	fi

	println "Load Error:",str
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

		ttisshort[i]:=stdcat[i]=shortcat

		ttlower[i]:=1

		ttcat[i]:=stdcat[i]
		ttisblock[i]:=stdcat[i]=blockcat

	od

	ttbasetype[trefchar]:=tref
	tttarget[trefchar]:=tc8

	ntypes:=tlast-1
end

global proc addspecialtypes=
	trefproc:=createrefmode(nil,tproc,0)
	treflabel:=createrefmode(nil,tlabel,0)
end

global function getsupportfile(ichar filename, ext="", path="",
	int issyslib=0, issupport=0)int =
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

	file:=filename

if fverbose=3 then
	fprintln "Get file:# (ext:#) (path:#)",filename,ext, path
fi

	if ext^ then
		strcpy(filespec,addext(filename,ext))
		file:=&.filespec
	fi

	if freadma then
		fileno:=loadbundledfile(file,issyslib, issupport)
		return fileno when fileno
	fi
	if issyslib and dointlibs then
		fileno:=findsyslib(file)
if fverbose=3 and fileno then
	fprintln "Found in syslib: #",sourcefilenames[fileno]
fi
		return fileno when fileno
	fi

	if not isabspath(file) then
		strcpy(filespec2,path)
		strcat(filespec2,file)
		file:=&.filespec2
	fi

if fverbose=3 and fileno then
	println "Checkfile:",file
fi
	if file=nil or not checkfile(file) then
		loaderror("Can't find file: # #",filename)
	fi


	fileno:=loadsourcefile(file)
if fverbose=3 and fileno then
	println "Found:",file
fi
	sourcefilesupport[fileno]:=issupport
	sourcefilesys[fileno]:=issyslib
	return fileno
end

function isabspath(ichar filespec)int=
	ichar path:=extractpath(filespec)
	if path^ in ['\\','/'] or path^<>0 and (path+1)^=':' then	!absolute path
		return 1
	fi
	return 0
end

global proc initbblib=
	for i:=1 to D_typestarterset.len do typestarterset[D_typestarterset[i]]:=1 od
end

global function getfileno(word pos)int fileno=
	fileno:=pos.[24..31]
	if fileno<1 or fileno>nsourcefiles then
RETURN 1
!		abortprogram("No file no")
	fi
	return fileno
end

global function getlineno(word pos)int=
	ichar source := getsourcestart(pos)
	ichar sline:=getsourceline(pos)
	ichar s:=sline
	int lineno:=1

	while s>source do
		if s^=10 then ++lineno fi
		--s
	od

	return lineno
end

function getsourceline(word pos)ichar=
	ichar source := getsourcestart(pos)
	ichar s :=  getsourcepos(pos)

	while s>source and s^<>10 do --s od
	if s^=10 then ++s fi

	return s
end

function getsourcestart(word pos)ichar=
	return sourcefiletext[getfileno(pos)]
end

function getsourcepos(word pos)ichar=
	return sourcefiletext[getfileno(pos)]+pos.[0..23]
end

global proc do_writema=
	[300]char filename
	[maxsourcefile]int sflist
	filehandle f
	int offset, nfiles, fileno

	if not fwritema then
		return
	fi
	strcpy(filename, changeext(sourcefilespecs[1],langextma))

!first build a table of source files to be o/p
	nfiles:=0

	for i to nsourcefiles do
		if sourcefilesys[i] and fwritema=1 then		!no syslibs
			next
		fi

		sflist[++nfiles]:=i
	od

	if nfiles=0 then loaderror(langextmauc+": no files") fi

	f:=fopen(filename,"wb")
	if not f then loaderror("Can't create "+langextmauc+" file #",filename) fi

	println "Writing ",filename
	fprintln @f,"=== "+langextmauc+" # ===",nfiles

	for i to nfiles do
		fileno:=sflist[i]

		fprintln @f,"=== # # # #/# ===",
			sourcefilenames[fileno],
			sourcefilesys[fileno],
			sourcefilesupport[fileno],
			i,nfiles

		offset:=getfilepos(f)
		writerandom(f,cast(sourcefiledupl[fileno]),offset,sourcefilesizes[fileno])
	od

	println @f,"=== END ==="

	for i to nfiles do
		fprintln @f,"# #",i,sourcefilenames[sflist[i]]
	od

	fclose(f)
	stop
end

