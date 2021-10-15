!Built-in standard headers

import clib
import mlib

ichar bcclibstr = strinclude "bcclib.asm"

global function findheader(ichar name)ichar=
return nil
end

global proc writeheaders=
filehandle f
ichar ifile
int i
end

global function hasintlibs:int= {0}

global proc checkbcclib=
const libfile="bcclib.asm"
filehandle f

if not checkfile(libfile) then
	println "Writing",libfile
	f:=fopen(libfile,"wb")
	fwrite(bcclibstr,1,strlen(bcclibstr),f)
	fclose(f)
fi
end

global function getbcclib:ichar=
	ichar s
	int slen

	slen:=strlen(bcclibstr)
	s:=malloc(slen+1)
	memcpy(s,bcclibstr,slen+1)

	return s
end

global function isheaderfile(ichar file)int=
return 0
end
