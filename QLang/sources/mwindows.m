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
type wt_wparam	= word
type wt_lparam	= word
type wt_point	= rpoint

export record rsystemtime =
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
!	func "VirtualAlloc"(wt_ptr, dint,wt_dword,wt_dword)wt_ptr
	func "GetStdHandle"(wt_dword)wt_handle
	func "GetConsoleScreenBufferInfo"(wt_handle,wt_ptr)int
	func "SetConsoleCtrlHandler"(wt_wndproc,int)int
	func "SetConsoleMode"(wt_handle,wt_dword)int
	func "CreateProcessA"(wt_ichar,wt_ichar,wt_ptr,wt_ptr, int,
						wt_dword, wt_ptr,wt_ichar,wt_ptr,wt_ptr)int
	func "GetLastError":wt_dword
	func "WaitForSingleObject"(wt_handle,wt_dword)wt_dword
	func "GetExitCodeProcess"(wt_handle,wt_ptr)int
	func "CloseHandle"(wt_handle)int
	func "GetNumberOfConsoleInputEvents"(wt_handle,wt_ptr)int
	func "FlushConsoleInputBuffer"(wt_handle)int
	func "LoadLibraryA"(wt_ichar)wt_handle
!	func "GetProcAddress"(wt_handle,wt_ichar)wt_wndproc
	func "GetProcAddress"(wt_handle,wt_ichar)ref void
	func "LoadCursorA"(wt_handle,wt_ichar)wt_handle
	func "RegisterClassExA"(wt_ptr)wt_wordpm
	func "DefWindowProcA"(wt_handle,wt_uint,wt_wparam,wt_lparam)int
	func "ReadConsoleInputA"(wt_handle,wt_ptr,wt_dword,wt_ptr)int
	proc "Sleep"(wt_dword)
	func "GetModuleFileNameA"(wt_handle,wt_ichar,wt_dword)wt_dword

	proc "ExitProcess"(wt_uint)
	proc "PostQuitMessage"(wt_int)

	proc "MessageBoxA"(wt_int x=0,wt_ichar message, caption="Caption",wt_int y=0)

	func "QueryPerformanceCounter"(ref int64)wt_bool
	func "QueryPerformanceFrequency"(ref int64)wt_bool

	func "CreateFileA"(wt_ichar,wt_dword,wt_dword,wt_ptr,wt_dword,wt_dword,wt_handle)wt_handle
	func "GetFileTime"(wt_handle,wt_ptr,wt_ptr,wt_ptr)wt_bool

	proc "GetSystemTime"(ref rsystemtime)
	proc "GetLocalTime"(ref rsystemtime)

	func "GetTickCount64":u64
	func "PeekMessageA"		(ref void, ref wt_handle, wt_uint,wt_uint,wt_uint)wt_bool

	func "GetCommandLineA":ichar

	func "VirtualAlloc" (ref void, wt_dword, wt_dword, wt_dword)ref void
	func "VirtualProtect" (ref void, wt_dword, wt_dword, ref wt_dword)wt_bool

	func "WriteConsoleA" (ref void, ref void, i32, ref i32, ref void)wt_bool

	func "FindFirstFileA" (wt_ichar,ref rfinddata)wt_handle
	func "FindNextFileA"  (wt_handle, ref rfinddata)wt_bool
	func "FindClose"      (wt_handle)wt_bool
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

record rfiletime =
	wt_dword lowdatetime
	wt_dword highdatetime
end

record rfinddata =
	wt_dword	fileattributes
	rfiletime	creationtime
	rfiletime	lastaccesstime
	rfiletime	lastwritetime
	wt_dword	filesizehigh
	wt_dword	filesizelow
	wt_dword	reserved0
	wt_dword	reserved1
	[260]char	filename
	[14]char		altfilename
	wt_dword	obs1, obs2
	wt_word		obs3
end

const NORMAL_PRIORITY_CLASS=32
const CREATE_NEW_CONSOLE=16
const DETACHED_PROCESS=16

const MEM_COMMIT				= 4096
const MEM_RESERVE				= 8192
const PAGE_EXECUTE				= 16
const PAGE_EXECUTE_READ			= 32
const PAGE_EXECUTE_READWRITE	= 64
const PAGE_NOACCESS				= 1


export wt_handle hconsole, hconsolein

input_record lastkey, pendkey
int keypending			!whether pendkey contains a new key event detected by flushkbd

ref func (ref void)int wndproc_callbackfn=nil	!windows call-back: address of handler

int init_flag=0

export proc os_init=
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

export func os_execwait(ichar cmdline,int newconsole=0,ichar workdir=nil)int =
	wt_dword exitcode
	int status
	int cflags:=0

	rstartupinfo si
	rprocess_information xpi

	clear si
	clear xpi

	case newconsole
	when 0 then cflags := NORMAL_PRIORITY_CLASS
	when 1 then cflags := NORMAL_PRIORITY_CLASS ior CREATE_NEW_CONSOLE
	when 2 then cflags := NORMAL_PRIORITY_CLASS ior DETACHED_PROCESS
	esac

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
		printf("Winexec error: %lld\n",status)
		return -1
	fi

	WaitForSingleObject(xpi.process, 0xFFFF'FFFF)
	GetExitCodeProcess(xpi.process,&exitcode)

	CloseHandle(xpi.process)
	CloseHandle(xpi.thread)

	return exitcode
end

export func os_execcmd(ichar cmdline, int newconsole=0)int =
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

export func os_getch:int=
	int k

	k:=os_getchx() iand 255

	return k
end

export func os_kbhit:int=
	wt_dword count

	unless init_flag then os_init() end

	GetNumberOfConsoleInputEvents(hconsolein,&count)
	return count>1
end

export func os_getdllinst(ichar name)u64=
	wt_handle hinst

	hinst:=LoadLibraryA(name)
	return cast(hinst)
end

export func os_getdllprocaddr(int hinst,ichar name)ref void=
	return GetProcAddress(cast(hinst),name)
end

export proc os_initwindows=
	os_init()
	os_gxregisterclass("pcc001")
end

export proc os_gxregisterclass(ichar classname)=
	const idcarrow=32512
	rwndclassex r
	static byte registered

	if registered then
		return
	fi

	clear r

	r.size:=r.bytes
	r.style:=8 ior 32
	r.wndproc:=cast(&mainwndproc)
	r.instance:=nil

	r.icon:=nil
	r.cursor:=LoadCursorA(nil,ref void(idcarrow))
	r.background:=cast(15+1)
	r.menuname:=nil
	r.classname:=classname
	r.iconsm:=nil

	if RegisterClassExA(&r)=0 then
		printf("Regclass error: %lld %lld\n",classname,GetLastError())
		stop 1
	end
	registered:=1
end

global function mainwndproc (
		wt_handle hwnd, wt_uint message, wt_wparam wParam, wt_lparam lParam)int=
	rmsg m
	int i,result
	int l
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

export proc os_setmesshandler(ref void addr)=
	wndproc_callbackfn:=addr
end

export func os_getchx:int=
!Q! function  os_getchx_c:int
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

export func os_getos=>ichar=
	if $targetbits=32 then
		return "W32"
	else
		return "W64"
	fi
end

export func os_gethostsize=>int=
	return $targetbits
end

export func os_shellexec(ichar opc, file)int=
	return system(file)
end

export proc os_sleep(int a)=
	Sleep(a)
end

export func os_getstdin:filehandle =
	return fopen("con","rb")
end

export func os_getstdout:filehandle =
	return fopen("con","wb")
end

export func os_gethostname:ichar=
	static [300]char name
	static int n

	GetModuleFileNameA(nil,&.name,name.bytes)
	return &.name
end

export func os_getmpath:ichar=
	return F"C:\m\"
end

export func os_clock:int64=
	return clock()
end

export func os_ticks:int64=
	return GetTickCount64()
end

export func os_iswindows:int=
	return 1
end

export proc os_getsystime(ref rsystemtime tm)=
	GetLocalTime(tm)
end

export proc os_peek=
	int ticks
	static int lastticks
	array [100]byte m
	ticks:=GetTickCount64()
	if ticks-lastticks>=1000 then
		lastticks:=ticks
		PeekMessageA(&m,nil,0,0,0)
	fi
end

export func os_allocexecmem(int n)ref byte=
	ref byte p
	u32 oldprot
	int status

	p := VirtualAlloc(nil, n, MEM_RESERVE ior MEM_COMMIT, PAGE_NOACCESS);
	if p = nil then return nil fi

	status := VirtualProtect(p, n, PAGE_EXECUTE_READWRITE, &oldprot);
	if status = 0 then return nil fi

	return p
end

export func dirlist(ichar filespec, ref[]ichar dest, int capacity, t=1)int=
#filespec is a filename (eg. "*.dwg") with possible drive/path; scan
#directory for all matching files:
# Store each file in dest array up to capacity
# Return:
#  -1:	capacity exceeded
#   N:  number of files found including 0 for no matching files

#t has this value
# +1  Include normal files only, no sub-directory names
# +2  Include directories
# +3  (+1 +2) Include all files including directories
# +4  Convert to lower case
	ref void hfind
	rfinddata file
	int nfiles:=0
	[300]char path
	[300]char fullfilename

	strcpy(path, extractpath(filespec))


	if (hfind:=findfirstfilea(filespec,&file))<>ref void(-1) then	!at least one file
		repeat
			if (file.fileattributes iand 16) then		!this is a directory
				if (t iand 2)=0 then nextloop fi		!no directories
			else						!this is a file
				if (t iand 1)=0 then nextloop fi
			fi
			if nfiles>=capacity then
				nfiles:=-1
				exit
			fi

			if (t iand 4) then				!to lower case
				convlcstring(file.filename)
			fi
			strcpy(fullfilename, path)
			strcat(fullfilename, file.filename)

			dest[++nfiles]:=pcm_copyheapstring(fullfilename)

		until not findnextfilea(hfind,&file)
		findclose(hfind)
	fi
	return nfiles
end
