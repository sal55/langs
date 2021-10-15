!Built-in standard headers

import clib
import mlib

ichar bcclibstr = strinclude "bcclib.asm"

ichar h_assert		=	strinclude "headers/assert.h"
ichar h_ctype		=	strinclude "headers/ctype.h"
ichar h_errno		=	strinclude "headers/errno.h"
ichar h_fenv		=	strinclude "headers/fenv.h"
ichar h_float		=	strinclude "headers/float.h"
ichar h_inttypes	=	strinclude "headers/inttypes.h"
ichar h_stdint		=	strinclude "headers/stdint.h"
ichar h_limits		=	strinclude "headers/limits.h"
ichar h_locale		=	strinclude "headers/locale.h"
ichar h__ansi		=	strinclude "headers/_ansi.h"
ichar h_math		=	strinclude "headers/math.h"
ichar h_setjmp		=	strinclude "headers/setjmp.h"
ichar h_signal		=	strinclude "headers/signal.h"
ichar h_stdarg		=	strinclude "headers/stdarg.h"
ichar h_stdbool		=	strinclude "headers/stdbool.h"
ichar h_stddef		=	strinclude "headers/stddef.h"
ichar h_stdio		=	strinclude "headers/stdio.h"
ichar h_stdlib		=	strinclude "headers/stdlib.h"
ichar h__syslist	=	strinclude "headers/_syslist.h"
ichar h_string		=	strinclude "headers/string.h"
ichar h_time		=	strinclude "headers/time.h"
ichar h_utime		=	strinclude "headers/utime.h"
ichar h_unistd		=	strinclude "headers/unistd.h"
ichar h_safelib		=	strinclude "headers/safelib.h"
ichar h_wchar		=	strinclude "headers/wchar.h"
ichar h_wctype		=	strinclude "headers/wctype.h"
ichar h_systypes	=	strinclude "headers/sys/types.h"
ichar h_sysstat		=	strinclude "headers/sys/stat.h"
ichar h_systimeb	=	strinclude "headers/sys/timeb.h"
ichar h_sysutime	=	strinclude "headers/sys/utime.h"
ichar h_memory		=	strinclude "headers/memory.h"
ichar h_windows		=	strinclude "headers/windows.h"
ichar h_fcntl		=	strinclude "headers/fcntl.h"
ichar h_io			=	strinclude "headers/io.h"
ichar h_direct		=	strinclude "headers/direct.h"
ichar h_process		=	strinclude "headers/process.h"
ichar h_malloc		=	strinclude "headers/malloc.h"
ichar h_bcc			=	strinclude "headers/bcc.h"
ichar h_conio		=	strinclude "headers/conio.h"
ichar h_winsock2	=	strinclude "headers/winsock2.h"
ichar h__mingw		=	strinclude "headers/_mingw.h"
!ichar h_shellapi	=	strinclude "headers/shellapi.h"
ichar h_windowsx	=	strinclude "headers/windowsx.h"

global tabledata []ichar stdhdrnames, []ref ichar stdhdrtext =
	("bcc.h",		&h_bcc),
	("assert.h",	&h_assert),
	("ctype.h",		&h_ctype),
	("errno.h",		&h_errno),
	("fenv.h",		&h_fenv),
	("float.h",		&h_float),
	("inttypes.h",	&h_inttypes),
	("stdint.h",	&h_stdint),
	("limits.h",	&h_limits),
	("locale.h",	&h_locale),
	("_ansi.h",		&h__ansi),
	("math.h",		&h_math),
	("setjmp.h",	&h_setjmp),
	("signal.h",	&h_signal),
	("stdarg.h",	&h_stdarg),
	("stdbool.h",	&h_stdbool),
	("stddef.h",	&h_stddef),
	("stdio.h",		&h_stdio),
	("stdlib.h",	&h_stdlib),
	("_syslist.h",	&h__syslist),
	("string.h",	&h_string),
	("time.h",		&h_time),
	("utime.h",		&h_utime),
	("unistd.h",	&h_unistd),
	("safelib.h",	&h_safelib),
	("wchar.h",		&h_wchar),
	("wctype.h",	&h_wctype),
	("sys/types.h",	&h_systypes),
	("sys/stat.h",	&h_sysstat),
	("sys/timeb.h",	&h_systimeb),
	("sys/utime.h",	&h_sysutime),
	("malloc.h",	&h_malloc),
	("windows.h",	&h_windows),
	("fcntl.h",		&h_fcntl),
	("io.h",		&h_io),
	("direct.h",	&h_direct),
	("process.h",	&h_process),
	("memory.h",	&h_memory),
	("conio.h",		&h_conio),
	("winsock2.h",	&h_winsock2),
	("_mingw.h",	&h__mingw),
!	("shellapi.h",	&h_shellapi),
	("windowsx.h",	&h_windowsx)
end

global function findheader(ichar name)ichar=
	int i
	[256]char newname
	ichar s,t

	if strchr(name,'\\') then
		s:=name; t:=&.newname
		while s^ do
			if s^='\\' then
				t++^:='/'
			else
				t++^:=s^
			fi
			++s
		od
		t^:=0
		name:=&.newname
	fi

	for i:=1 to stdhdrnames.len do
		if eqstring(name,stdhdrnames[i]) then
			return stdhdrtext[i]^
		fi
	od
	return nil
end

global proc writeheaders=
	filehandle f
	ichar ifile
	int i
	for i:=1 to stdhdrnames.len do
		ifile:=changeext(stdhdrnames[i],"hdr")
		println "Writing internal",stdhdrnames[i],"as",ifile
		f:=fopen(ifile,"wb")
		fwrite(stdhdrtext[i]^,1,strlen(stdhdrtext[i]^),f)
		fclose(f)
	od
end

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
	RETURN 0
	for i:=1 to stdhdrnames.len do
		if eqstring(stdhdrnames[i],file) then
			return 1
		fi
	od
	return 0
end

global function hasintlibs:int= {1}
