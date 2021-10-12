import clib
import mlib

const wm_destroy=2

type wt_word	= word16
type wt_wordpm	= word32
type wt_bool	= word32
type wt_dword	= word32
type wt_wchar	= word16
type wt_wcharpm	= word32
type wt_char	= byte
type wt_ichar	= ref char
type wt_ptr		= ref void
type wt_wndproc	= ref proc
type wt_handle	= ref void
type wt_int		= int32
type wt_uint	= word32
type wt_long	= int32
type wt_wparam	= wordm
type wt_lparam	= wordm
type wt_point	= rpoint

global record rsystemtime =
	wt_word year
	wt_word month
	wt_word dayofweek
	wt_word day
	wt_word hour
	wt_word minute
	wt_word second
	wt_word milliseconds
end

importdll $windowsdlls=
!	windows function "VirtualAlloc"(wt_ptr, dint,wt_dword,wt_dword)wt_ptr
	windows function "GetStdHandle"(wt_dword)wt_handle
	windows function "GetConsoleScreenBufferInfo"(wt_handle,wt_ptr)int
	windows function "SetConsoleCtrlHandler"(wt_wndproc,int)int
	windows function "SetConsoleMode"(wt_handle,wt_dword)int
	windows function "CreateProcessA"(wt_ichar,wt_ichar,wt_ptr,wt_ptr, int,
						wt_dword, wt_ptr,wt_ichar,wt_ptr,wt_ptr)int
	windows function "GetLastError":wt_dword
	windows function "WaitForSingleObject"(wt_handle,wt_dword)wt_dword
	windows function "GetExitCodeProcess"(wt_handle,wt_ptr)int
	windows function "CloseHandle"(wt_handle)int
	windows function "GetNumberOfConsoleInputEvents"(wt_handle,wt_ptr)int
	windows function "FlushConsoleInputBuffer"(wt_handle)int
	windows function "LoadLibraryA"(wt_ichar)wt_handle
!	windows function "GetProcAddress"(wt_handle,wt_ichar)wt_wndproc
	windows function "GetProcAddress"(wt_handle,wt_ichar)ref void
	windows function "LoadCursorA"(wt_handle,wt_ichar)wt_handle
	windows function "RegisterClassExA"(wt_ptr)wt_wordpm
	windows function "DefWindowProcA"(wt_handle,wt_uint,wt_wparam,wt_lparam)intm
	windows function "ReadConsoleInputA"(wt_handle,wt_ptr,wt_dword,wt_ptr)int
	windows proc     "Sleep"(wt_dword)
	windows function "GetModuleFileNameA"(wt_handle,wt_ichar,wt_dword)wt_dword

	windows proc     "ExitProcess"(wt_uint)
	windows proc	 "PostQuitMessage"(wt_int)

	windows proc	 "MessageBoxA"(wt_int x=0,wt_ichar message, caption="Caption",wt_int y=0)

	windows function "QueryPerformanceCounter"(ref int64)wt_bool
	windows function "QueryPerformanceFrequency"(ref int64)wt_bool

	windows function "CreateFileA"(wt_ichar,wt_dword,wt_dword,wt_ptr,wt_dword,wt_dword,wt_handle)wt_handle
	windows function "GetFileTime"(wt_handle,wt_ptr,wt_ptr,wt_ptr)wt_bool

	windows proc     "GetSystemTime"(ref rsystemtime)
	windows proc     "GetLocalTime"(ref rsystemtime)

	windows function "GetTickCount64":u64
	windows function "PeekMessageA"		(ref void, ref wt_handle, wt_uint,wt_uint,wt_uint)wt_bool

end

record input_record = $caligned
	wt_word	eventtype
!	word16	padding
		wt_bool	keydown			!key event record (was inside 'Event' union in win32)
		wt_word	repeatcount
		wt_word	virtualkeycode
		wt_word	virtualscancode
		union
			wt_word unicodechar
			wt_char asciichar
		end
		wt_dword controlkeystate
end

record rspoint=(int16 x,y)

record rsrect=
	int16 leftx,top,rightx,bottom
end

global record rpoint =
	wt_long x,y
end

record rconsole=
	rspoint size,pos
	word16 attributes
	rsrect window
	rspoint maxwindowsize
end

record rstartupinfo =
	wt_dword	size
	word32 dummy1
	wt_ichar	reserved
	wt_ichar	desktop
	wt_ichar	title
	wt_dword	x
	wt_dword	y
	wt_dword	xsize
	wt_dword	ysize
	wt_dword	xcountchars
	wt_dword	ycountchars
	wt_dword	fillattribute
	wt_dword	flags
	wt_word		showwindow
	wt_word		reserved2
	word32 dummy2
	wt_ptr		reserved4
	wt_handle	stdinput
	wt_handle	stdoutput
	wt_handle	stderror
end

record rprocess_information =
	wt_handle process
	wt_handle thread
	wt_dword processid
	wt_dword threadid
end

record rwndclassex =
	wt_uint		size
	wt_uint		style
	wt_wndproc	wndproc
	wt_int		clsextra
	wt_int		wndextra
	wt_handle	instance
	wt_handle	icon
	wt_handle	cursor
	wt_handle	background
	wt_ichar	menuname
	wt_ichar	classname
	wt_handle	iconsm
end

global record rmsg =
	wt_handle	hwnd
	wt_uint		message
	word32		dummy1
	wt_wparam	wParam
	wt_lparam	lParam
	wt_dword	time
	word32		dummy2
	wt_point	pt
end

const NORMAL_PRIORITY_CLASS=32
const CREATE_NEW_CONSOLE=16
const DETACHED_PROCESS=16

wt_handle hconsole, hconsolein

input_record lastkey, pendkey
int keypending			!whether pendkey contains a new key event detected by flushkbd

ref function(ref void)int wndproc_callbackfn=nil	!windows call-back: address of handler

int init_flag=0

global proc os_init=
	int i,count
	rconsole info

!general initialisation
	hconsole:=GetStdHandle(u32(-11))
	hconsolein:=GetStdHandle(u32(-10))

	lastkey.repeatcount:=0
	keypending:=0

	SetConsoleCtrlHandler(nil,1)

	SetConsoleMode(hconsole,1 ior 2)

	init_flag:=1

end

global function os_execwait(ichar cmdline,int newconsole=0,ichar workdir=nil)int =
	wt_dword exitcode
	int status
	int cflags:=0

	rstartupinfo si
	rprocess_information xpi

	clear si
	clear xpi

	switch newconsole
	when 0 then cflags := NORMAL_PRIORITY_CLASS
	when 1 then cflags := NORMAL_PRIORITY_CLASS ior CREATE_NEW_CONSOLE
	when 2 then cflags := NORMAL_PRIORITY_CLASS ior DETACHED_PROCESS
	endswitch

	si.size := rstartupinfo.bytes

	status:=CreateProcessA(
		nil,
		cmdline,
		nil,

		nil,
		1,
		cflags,

		nil,
		nil,
		&si,
		&xpi )

	if status=0 then		!fails
		status:=GetLastError()
		println "Winexec error:",status
		return -1
	fi

	WaitForSingleObject(xpi.process, 0xFFFF'FFFF)
	GetExitCodeProcess(xpi.process,&exitcode)

	CloseHandle(xpi.process)
	CloseHandle(xpi.thread)

	return exitcode
end

global function os_execcmd(ichar cmdline, int newconsole=0)int =
	wt_dword exitcode
	int i,j,k

	rstartupinfo si
	rprocess_information xpi

	clear si
	clear xpi

	si.size := rstartupinfo.bytes

	CreateProcessA( nil,
		cmdline,
		nil,
		nil,
		1,
		NORMAL_PRIORITY_CLASS ior (newconsole|CREATE_NEW_CONSOLE|0),
		nil,
		nil,
		&si,
		&xpi )

	CloseHandle(xpi.process)
	CloseHandle(xpi.thread)

	return 1
end

global function os_getch:int=
	int k

	k:=os_getchx() iand 255

	return k
	end

	global function os_kbhit:int=
	wt_dword count

	unless init_flag then os_init() end

	GetNumberOfConsoleInputEvents(hconsolein,&count)
	return count>1
end

global proc os_flushkeys=
	FlushConsoleInputBuffer(hconsolein)
end

global function os_getconsolein:ref void=
	return ref void(hconsolein)
end

global function os_getconsoleout:ref void=
	return ref void(hconsole)
end

global function os_proginstance:ref void=
	abortprogram("PROGINST")
	return nil
end

global function os_getdllinst(ichar name)u64=
	wt_handle hinst

	hinst:=LoadLibraryA(name)
	return cast(hinst)
end

global function os_getdllprocaddr(int hinst,ichar name)ref void=
	return GetProcAddress(cast(hinst),name)
end

global proc os_initwindows=
	os_init()
	os_gxregisterclass("pcc001")
end

global proc os_gxregisterclass(ichar classname)=
	const idcarrow=32512
	rwndclassex r
	static byte registered

	if registered then
		return
	fi

	clear r

	r.size:=r.bytes
	r.style:=8 ior 32		!CS_DBLCLKS | CS_OWNDC
	r.wndproc:=cast(&mainwndproc)
	r.instance:=nil

	r.icon:=nil		!loadicon(proginstance,"SCW32")
	r.cursor:=LoadCursorA(nil,ref void(idcarrow))		!IDC_ARROW)
	r.background:=cast(15+1)					!COLOR_BTNFACE+1
	r.menuname:=nil
	r.classname:=classname
	r.iconsm:=nil	!loadicon(proginstance,"SCW32")

	if RegisterClassExA(&r)=0 then
		println classname,GetLastError
		abortprogram("Registerclass error")
	end
	registered:=1
end

global callback function mainwndproc (
		wt_handle hwnd, wt_uint message, wt_wparam wParam, wt_lparam lParam)intm=
	rmsg m
	int i,result
	intm l
	static int count=0

	m.hwnd:=hwnd
	m.message:=message
	m.wParam:=wParam
	m.lParam:=lParam
	m.pt.x:=0
	m.pt.y:=0
	
	if (wndproc_callbackfn) then
		result:=(wndproc_callbackfn^)(&m)
	else
		result:=0
	fi

	if m.message=wm_destroy then
		return 0
	fi

	if not result then
		return DefWindowProcA(hwnd,message,wParam,lParam)
	else
		return 0
	fi
end

proc timerproc(wt_handle hwnd, int msg, id, time)=
	println "TIMERPROC"
end

GLOBAL PROC OS_TESTCALLBACK(ref void p)=
	IF WNDPROC_CALLBACKFN THEN
		(WNDPROC_CALLBACKFN)(P)
	ELSE
		ABORTPROGRAM("MESS HANDLER NOT DEFINED")
	FI
END

global proc os_setmesshandler(ref void addr)=
	wndproc_callbackfn:=addr
end

global function os_getchx:int=
!Q! function os_getchx_c:int
!return a 32-bit value containing:
! 15..B0:	char code
! 23..16	virtual keycode
! 31..24	shift flags (.[24]=shift, .[25]=ctrl, .[26]=alt, .[27]=capslock)
	const rightaltmask	= 1
	const leftaltmask	= 2
	const leftctrlmask	= 8
	const rightctrlmask	= 4
	const shiftmask		= 16
	const capsmask		= 128
	const scrollmask	= 64
	int count
	int charcode,keyshift,keycode
	int altdown,ctrldown,shiftdown,capslock

!os_init() unless init_flag
	unless init_flag then os_init() end

	if keypending then
		lastkey:=pendkey
		keypending:=0
	else
		if lastkey.repeatcount=0 then
			repeat
				count:=0
				ReadConsoleInputA(hconsolein,&lastkey,1,&count)
			until (lastkey.eventtype=1 and lastkey.keydown=1)
		fi
	fi

!set shift flags

	altdown		:= ((lastkey.controlkeystate iand (leftaltmask ior rightaltmask))|1|0)
	ctrldown	:= ((lastkey.controlkeystate iand (leftctrlmask ior rightctrlmask))|1|0)
	shiftdown	:= ((lastkey.controlkeystate iand shiftmask)|1|0)
	capslock	:= ((lastkey.controlkeystate iand capsmask)|1|0)

	--lastkey.repeatcount		!count this key out

	charcode:=lastkey.asciichar
	keycode:=lastkey.virtualkeycode iand 255

	if charcode<0 then
		if charcode<-128 then
			charcode:=0
		else
			charcode+:=256
		fi
	fi

!for keycodes in range 186 to 223, which are all stand-alone punctuation keys, I might
!wish to set charcode to the appropriate printed char code (currently charcode will be
!zero, and keyboard handlers need to detect keycodes such as vkequals)
!....

	if altdown and ctrldown and charcode=166 then
		altdown:=ctrldown:=0
	else
		if altdown or ctrldown then
			charcode:=0
			if keycode>='A' and keycode<= 'Z' then
				charcode:=keycode-'@'
			fi
		fi
	fi

	keyshift:=capslock<<3 ior altdown<<2 ior ctrldown<<1 ior shiftdown

	return keyshift<<24 ior keycode<<16 ior charcode
end

global function os_getos=>ichar=
	if $targetbits=32 then
		return "W32"
	else
		return "W64"
	fi
end

global function os_gethostsize=>int=
	return $targetbits
end

global function os_shellexec(ichar opc, file)int=
	return system(file)
end

global proc  os_sleep(int a)=
	Sleep(a)
end

global function os_getstdin:filehandle =
	return fopen("con","rb")
end

global function os_getstdout:filehandle =
	return fopen("con","wb")
end

global function os_gethostname:ichar=
	static [300]char name
	static int n

	GetModuleFileNameA(nil,&.name,name.bytes)
	return &.name
end

global function os_getmpath:ichar=
	return F"C:\m\"
end

global proc os_exitprocess(int x)=
	stop x
!	ExitProcess(x)
end

global function os_clock:int64=
	return clock()
end

global function os_ticks:int64=
	return GetTickCount64()
end

global function os_getclockspersec:int64=
	return 1000
end

global function os_iswindows:int=
	return 1
end

global function os_filelastwritetime(ichar filename)int64=
	wt_handle f;
	int64 ctime,atime,wtime;

	if filename=nil then				!used to test whether supported
		return 1
	fi

	f:=CreateFileA(filename,0x80000000,1,nil, 3,3,nil);
	if int64(f)=-1 then
		return 0
	fi

	GetFileTime(f,&ctime,&atime,&wtime);
	CloseHandle(f);

	return wtime;
end

global proc os_getsystime(ref rsystemtime tm)=
	GetLocalTime(tm)
end

global proc os_messagebox(ichar s,t)=
	messageboxa(0,s,t,0)
end

global function os_hpcounter:int64=
	int64 a

	queryperformancecounter(&a)
	return a

end

global function os_hpfrequency:int64=
	int64 a

	queryperformancefrequency(&a)
	return a

end

global proc os_peek=
	int ticks
	static int lastticks
	[100]byte m
	ticks:=GetTickCount64()
	if ticks-lastticks>=1000 then
		lastticks:=ticks
		PeekMessageA(&m,nil,0,0,0)
	fi
end
