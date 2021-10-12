import mlib

global tabledata []ichar stdlibnames, []ichar stdlibtext =
	("msysp.m",			strinclude "msysp.m"),
	("mlibp.m",			strinclude "mlibp.m"),
	("mclib.m",			strinclude "mclib.m"),
	("mwindowsp.m",		strinclude "mwindowsp.m"),
	("mwindllp.m",		strinclude "mwindllp.m"),
end

global function findstdlib(ichar name)ichar=
	for i:=1 to stdlibnames.len do
		if eqstring(name,stdlibnames[i]) then
			return stdlibtext[i]
		fi
	od
	return nil
end

