global const fsyslibs = 1
!global const fsyslibs = 0

global tabledata []ichar syslibnames,[]ichar libtext =
	("qlib.q",			(fsyslibs | strinclude "qlib.q" | "" )),
	("sysp.q",			(fsyslibs | strinclude "sysp.q" | "" )),
	("clibp.q",			(fsyslibs | strinclude "clibp.q" | "" )),
	("smlib.q",			(fsyslibs | strinclude "smlib.q" | "" )),
	("winapi.q",		(fsyslibs | strinclude "winapi.q" | "" )),
	("gxlib.q",			(fsyslibs | strinclude "gxlib.q" | "" )),
	("bmlib.q",			(fsyslibs | strinclude "bmlib.q" | "" )),
	("console.q",		(fsyslibs | strinclude "console.q" | "" )),
	("winconsts.q",		(fsyslibs | strinclude "winconsts.q" | "" )),
	("wingxlib.q",		(fsyslibs | strinclude "wingxlib.q" | "" )),
	("winmessages.q",	(fsyslibs | strinclude "winmessages.q" | "" )),
	("gxmisc.q",		(fsyslibs | strinclude "gxmisc.q" | "" )),
	("dates.q",			(fsyslibs | strinclude "dates.q" | "" )),
end

![syslibnames.len]byte syslibfileno

function findsyslib(ichar filename)ichar=
!filename must be module name with .q extension
!return source code, or nil

	for i to syslibnames.len do
		if eqstring(filename, syslibnames[i]) then
			return libtext[i]
		fi
	od

	nil
end

global func loadsysmodule(ifile pm)int=
!load the syslib specified within pm, then load it and fully populate pm
!return 0 if it fails
!for internal syslibs, the filename will have no path, but should have an extension
	ichar source

	source:=findsyslib(pm.filespec)

	if source then
		pm.text:=source
		pm.size:=strlen(source)
		return 1
	else
		return 0
	fi
end
