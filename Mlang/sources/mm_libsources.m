const fsyslibs = 1

global tabledata []ichar syslibnames, []ichar libtext =
	("msys.m",			strinclude "msys.m"),
	("mlib.m",			strinclude "mlib.m"),
	("mclib.m",			strinclude "mclib.m"),
	("mwindows.m",		strinclude "mwindows.m"),
	("mwindll.m",		strinclude "mwindll.m"),
end

[syslibnames.len]byte syslibfileno

global function findsyslib(ichar filename)int=
!filename must be module name with .q extension
!return sourcefile no

	if not fsyslibs then return 0 fi

	filename:=extractfile(filename)		!remove any path

	for i to syslibnames.len do
		if eqstring(syslibnames[i],filename) then
			if syslibfileno[i] then
				return syslibfileno[i]
			fi

!add to sourcefiles
			if nsourcefiles>=maxsourcefile then loaderror("fsl: too many files") fi
			++nsourcefiles
			sourcefilenames[nsourcefiles]:=pcm_copyheapstring(filename)
			sourcefiletext[nsourcefiles]:=libtext[i]
			if fwritema then
				sourcefiledupl[nsourcefiles]:=pcm_copyheapstring(libtext[i])
			fi
			sourcefilesizes[nsourcefiles]:=strlen(libtext[i])
!			sourcefilelocs[nsourcefiles]:='PACK'
			sourcefilepaths[nsourcefiles]:=""
			sourcefilespecs[nsourcefiles]:=""
			sourcefilesys[nsourcefiles]:=1
			sourcefilesupport[nsourcefiles]:=0

			syslibfileno[i]:=nsourcefiles
			return nsourcefiles
		fi
	od

	return 0
end
