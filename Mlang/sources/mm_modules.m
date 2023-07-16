!int ssfile=0

ichar headerpathx	= ""
ichar altpathx		= ""
ichar importpathx	= ""
ichar subprogpath	= ""
int dirpos
int issyslib

[headervarnames.len]ichar headervars

macro mainmodule=headervars[hv_mainmodule]

global proc readprojectfile(ichar filename)=
	int fileno,headerdir, dir, oldsyslib, found
	ichar basefile, extension

	extension:=convlcstring(extractext(filename))

	found:=checkfile(filename)
	if not found and not eqstring(extension, langextma) then
		filename:=pcm_copyheapstring(changeext(filename,langextma))
		found:=checkfile(filename)
		if found then
			fprintln "(Building #)",filename
			extension:=langextma
		fi
	fi	

	if not found then
		loaderror("Can't find main module or project: ##",filename)
	fi

	if eqstring(extension,langextma) then
		filename:=loadmafile(filename)
	fi

	fileno:=getsupportfile(filename)

	basefile:=extractbasefile(sourcefilenames[fileno])

	initheadervars()

	headermode:=1
	headerdir:=0

	moduletable[0].name:="PROGRAM"
	moduletable[0].fileno:=0

	stprogram:=createdupldef(nil,addnamestr("$prog"),programid)
	moduletable[0].stmodule:=stprogram
!
	addfirstsubprogram(basefile,fileno)

	startlex(fileno)

	do
		lex()
		skipsemi()

		case lx.symbol
		when kheadersym then
			headerdir:=1
			dir:=lx.subcode
			dirpos:=lx.pos
			lex()
			case dir
			when hdr_module then
				readmoduledir()
				mainmodule:=""
			when hdr_sysmodule then
				oldsyslib:=issyslib
				issyslib:=1
				readmoduledir()
				issyslib:=oldsyslib
				mainmodule:=""
			when hdr_subprog then
				altpathx:=""
				issyslib:=0
				readsubprogram()
			when hdr_syssubprog then
				if importpathx^=0 then
					importpathx:=headervars[hv_devpath]
				fi
				issyslib:=1
				readsubprogram()
			when hdr_import then
				if lx.symbol=namesym and eqstring(lx.symptr.name,langlibname) then
!					recase hdr_sysimport
					$hdr_sysimport
				fi
				issyslib:=0
				altpathx:=""
				readimport()
			when hdr_minclude then
				readinclude()
			when hdr_sysimport then
$hdr_sysimport::
				if importpathx^=0 then
					importpathx:=headervars[hv_devpath]
				fi
				issyslib:=1
				altpathx:=""
				readimport()

			when hdr_altpath then
				altpathx:=fixpath(readvar())
			when hdr_importpath then
				importpathx:=fixpath(readvar())
				subprogpath:=(importpathx^|importpathx|headerpathx)

			when hdr_setvar then
				dosetvar()

			when hdr_showvar then
				doshowvar()

			when hdr_linkdll then
				addlib(readvar(),'D')
			when hdr_linklib then
				addlib(readvar(),'L')
			else
				loaderror("Hdr directive not ready:##",headerdirnames[dir])
			esac

			checksymbol(semisym)

		when semisym then
		when eofsym then
			exit
		else
			if sourcelevel and lximport then
				setmixedimport()
				unstacksource()
			else
				setmixedprogram(basefile)
				exit
			fi
		esac
	od


	if nmodules=0 then
		loaderror("No modules specified")
	fi

	addsyslib()

	addlib("msvcrt",'D')
	addlib("user32",'D')
	addlib("gdi32",'D')
	addlib("kernel32",'D')
end

proc initheadervars=
	for i to headervars.len do
		headervars[i]:=""
	od

	headervars[hv_devpath]:=langhomedir
	headervars[hv_mmpath]:=pcm_copyheapstring(extractpath(sysparams[1]))
	subprogpath:=headerpathx:=headervars[hv_hdrpath]:=pcm_copyheapstring(sourcefilepaths[1])
	headervars[hv_windows]:="1"
	mainmodule:="1"

end

proc readmoduledir=
!at symbol following 'module'
	ichar modulename, modulefilespec
	symbol stalias

	checksymbol(namesym)
	modulename:=modulefilespec:=pcm_copyheapstring(lx.symptr.name)
	convlcstring(modulename)
	stalias:=nil

	lex()
	if lx.symbol=namesym and eqstring(lx.symptr.name,"as") then
		lex()
		if lx.symbol=namesym then
			stalias:=lx.symptr
			lex()
		else
			stalias:=addnamestr(readvar())
		fi
	fi

	if checkwhen() then
		addmodule(modulename,stalias)
	fi
end

function checkwhen:int=
	int index

	return 1 when lx.symbol<>kwhensym

	lex()
	checksymbol(kheadervarsym)
	index:=lx.subcode
	lex()

	return eqstring(headervars[index],"1")
end

proc addmodule(ichar modulename, symbol stalias=nil)=
!	Add new module with given name (should be on the heap)
	ref modulerec pm
	ref subprogrec ps

	for i to nmodules do
		if eqstring(moduletable[i].name, modulename) then
			loaderror("Duplicate module name: # (Line:#)",modulename,strint(getlineno(dirpos)))
		fi
	od

	for i to nsubprogs do
		if eqstring(subprogtable[i].name, modulename) then
!			loaderror("Clashing subprog/module name: # (Line:#)",modulename,strint(dirline))
			loaderror("Clashing subprog/module name: # (Line:#)",modulename,strint(getlineno(dirpos)))
		fi
	od

	if nmodules>=maxmodule then
		loaderror("Too many modules",modulename)
	fi
	pm:=&moduletable[++nmodules]

	pm.name:=pcm_copyheapstring(modulename)
	pm.subprogno:=nsubprogs

	pm.stmodule:=stmodule:=createnewmoduledef(stprogram,addnamestr(modulename))

	pm.path:=(altpathx^|altpathx|subprogpath)
	pm.issyslib:=issyslib

	stmodule.moduleno:=nmodules
	stmodule.subprogno:=nsubprogs
	moduletosub[nmodules]:=nsubprogs

	ps:=&subprogtable[nsubprogs]

	if ps.firstmodule=0 then
		ps.firstmodule:=nmodules
	fi

	if stalias then

		pm.stmacro:=getduplnameptr(stprogram, stalias, macroid)
		adddef(stprogram, pm.stmacro)
		pm.stmacro.paramlist:=nil
		pm.stmacro.code:=createname(stmodule)
	fi
end

proc addsubprogram(ichar subprogname,int fileno)=
!	Add new subprogram with given name (should be on the heap)
	ref subprogrec ps

	if nsubprogs>=maxsubprog then
		loaderror("Too many subprograms",subprogname)
	fi

	for i to nsubprogs do
		if eqstring(subprogtable[i].name, subprogname) then
			loaderror("Duplicate subprog name: # (Line:#)",subprogname,strint(getlineno(dirpos)))
		fi
	od
	ps:=&subprogtable[++nsubprogs]

	ps.name:=pcm_copyheapstring(subprogname)

	subprogpath:=ps.path:=(importpathx^|importpathx|subprogpath)

	stsubprog:=createnewmoduledef(stprogram,addnamestr(subprogname),subprogid)
	stsubprog.subprogno:=nsubprogs
	ps.stsubprog:=stsubprog
!	stsubprog.subprogno:=nsubprogs
	ps.fileno:=fileno
	ps.issyslib:=issyslib
end

proc addfirstsubprogram(ichar progname, int fileno)=
	ref subprogrec ps

	nsubprogs:=1
	ps:=&subprogtable[1]
	ps.name:=pcm_copyheapstring(progname)
	ps.path:=headerpathx

	stsubprog:=createnewmoduledef(stprogram,addnamestr(progname),subprogid)
	stsubprog.subprogno:=1
	ps.stsubprog:=stsubprog
	ps.fileno:=fileno
	mainmoduleno:=1
end

proc readsubprogram=
	ichar subprogname, subprogfilespec

	checksymbol(namesym)
	subprogname:=subprogfilespec:=pcm_copyheapstring(lx.symptr.name)
	convlcstring(subprogname)

	lex()

	if lx.symbol=kwhensym then
		lex()
		lex()
	fi

	addsubprogram(subprogname,0)

end

proc readimport=
	ichar subprogname, path
	int fileno

	checksymbol(namesym)

	subprogname:=pcm_copyheapstring(lx.symptr.name)
	convlcstring(subprogname)

	lex()

	path:=(importpathx^|importpathx|subprogpath)

	fileno:=getsupportfile(subprogname,langext, path)
	addsubprogram(subprogname,fileno)

	stacksource(fileno)
end

proc readinclude=
	ichar name, path
	int fileno

	checksymbol(stringconstsym)
	name:=pcm_copyheapstring(lx.svalue)

	lex()

	fileno:=getsupportfile(name,langext, "")
	stacksource(fileno)
end

function readvar:ichar s=
	case lx.symbol
	when stringconstsym then
		s:=pcm_copyheapstring(lx.svalue)
	when kheadervarsym then
		s:=headervars[lx.subcode]
	when namesym then
		s:=lx.symptr.name
	else
		loaderror("readvar/bad expr")
		s:="?"
	esac
	lex()
	return s
end

function fixpath(ichar path)ichar=
	[300]char newpath
	int n:=strlen(path)
	if n=0 then return path fi
	if (path+n-1)^ in ['\\','/'] then
		return path
	fi
	strcpy(newpath,path)
	strcat(newpath,"\\")
	return pcm_copyheapstring(newpath)
end

proc dosetvar=
	int index

	checksymbol(kheadervarsym)
	index:=lx.subcode
	lex()
	checksymbol(eqsym)
	lex()
	headervars[index]:=readvar()
end

proc doshowvar=
	if lx.symbol=stringconstsym then
		println lx.svalue
	else
		checksymbol(kheadervarsym)
		println headervarnames[lx.subcode]+3,"=",headervars[lx.subcode]
	fi
	lex()
end

proc setmixedprogram(ichar basefile)=
	[100]char name
	int oldns

	print @name,"$",,basefile
	oldns:=nsubprogs
	nsubprogs:=1
	addmodule(name)
	nsubprogs:=oldns
	moduletable[nmodules].fileno:=1
	mainmoduleno:=subprogtable[1].firstmodule:=nmodules
end

proc setmixedimport=
	[100]char name

	print @name,"$",,subprogtable[nsubprogs].name
	addmodule(name)
	moduletable[nmodules].fileno:=subprogtable[nsubprogs].fileno
	subprogtable[nsubprogs].firstmodule:=nmodules
end

global proc loadmodules =
	ref modulerec pm

	for i to nmodules do
		pm:=&moduletable[i]
		loadmodule(pm)
	od
end

proc loadmodule(ref modulerec pm)=
	[300]char filespec
	ichar path

	if pm.fileno then
		return
	fi

	path:=pm.path
	if path^=0 and pm.issyslib then
		path:=f"c:\mx\"
	fi

	pm.fileno:=getsupportfile(pm.name, langext, path, issyslib:pm.issyslib)
end

proc addsyslib=
!add in syslib if mlib not already included

	if msyslevel=0 then return fi

	for i to nsubprogs do
		if eqstring(subprogtable[i].name,langname+"libx") then return fi
	od

	issyslib:=1
	importpathx:=headervars[hv_devpath]
	altpathx:=""
	if msyslevel=1 then
		addsubprogram(langname+"libtemp",0)
		addmodule(langname+"systemp")

		return
	fi

	addsubprogram(langname+"libx",0)
	addmodule(langname+"sys")
	addmodule(langname+"lib")

	addmodule(langname+"clib")
	addmodule(langname+"windows")

	addmodule(langname+"windll")
end

global proc addlib(ichar libname, int libtype='D')=
	for i to nlibfiles do
		if eqstring(libfiles[i],libname) then return fi
	od
	if nlibfiles>=maxlibfile then
		loaderror("Too many libs")
	fi
	libfiles[++nlibfiles]:=libname
	libtypes[nlibfiles]:=libtype
end

function readfileline(ichar s)ichar =
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

function loadmafile(ichar filespec, ichar builtinstr=nil)ichar=
!load ma file from disk
!unless filespec is nil, then direct from builtinstr
!return name of lead module
	ichar s,t
	[100]char name
	[300]char newfilespec
	int sys,support

	freadma:=1

	if filespec then
		s:=cast(readfile(filespec))
		if s=nil then							!file not found on disk
			loaderror("Can't find MA file ##",filespec)
		fi
		strcpy(newfilespec,extractpath(filespec))
	else
		s:=builtinstr
		newfilespec[1]:=0
	fi

!need to scan file pickuping the file headers, and populating sourctables

	s:=readfileline(s+3)
	readstr(name,'n')
	if not eqstring(name,langextma) then
		loaderror(langextmauc+": bad header")
	fi

	--s					!point to previous lf

	if nsourcefiles then
		loaderror(langextmauc+"/table not empty")
	fi

	s:=findnextlineheader(s)

	do
		if s=nil then
			loaderror("Unexpected EOF in "+langextmauc+" file")
			exit
		fi
		s:=readfileline(s)

		readstr(name,'n')
		read sys,support

!		println "Found file",name
		if eqstring(name,"end") then
			exit
		fi
		if nsourcefiles>=maxsourcefile then
			loaderror("Too many files in "+langextmauc)
		fi

		t:=findnextlineheader(s)
		if t=nil then
			loaderror("MA error")
		fi

		++nsourcefiles
		sourcefilenames[nsourcefiles]:=sourcefilespecs[nsourcefiles]:=pcm_copyheapstring(name)
		sourcefilesizes[nsourcefiles]:=t-s-3
		sourcefiletext[nsourcefiles]:=s
		sourcefilepaths[nsourcefiles]:=""
		sourcefilespecs[nsourcefiles]:=""
		sourcefilesys[nsourcefiles]:=sys
		sourcefilesupport[nsourcefiles]:=support
		s:=t
	od
!
	for i to nsourcefiles do
		(sourcefiletext[i]+sourcefilesizes[i])^:=0	
	od

!finally, set inputfile to the first file found
	strcat(newfilespec, sourcefilenames[1])
	return pcm_copyheapstring(newfilespec)
end

