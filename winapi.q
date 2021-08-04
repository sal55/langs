!Winapi.q - define basic Win32 functions

global type wt_bool     = word32
global type wt_dword    = word32
global type wt_wchar    = word16
global type wt_char     = byte
global type wt_ichar    = cstring
global type wt_string   = cstring
global type wt_ptr      = ref byte
global type wt_wndproc  = word64

global type wt_handle   = ref void
global type wt_int      = int32
global type wt_uint     = word32
global type wt_long     = int32
global type wt_wparam   = word64
global type wt_lparam   = word64
global type wt_size     = word64

global type wt_wparam32 = word32
global type wt_lparam32 = word32
global type wt_handle32 = word32
global type wt_ptr32    = word32
global type wt_string32 = word32
global type wt_wndproc32    = word32

global type wt_wparam64 = word64
global type wt_lparam64 = word64
global type wt_handle64 = word64
global type wt_ptr64    = word64
global type wt_string64 = word64
global type wt_wndproc64= word64

global type wt_result   = word64
global type wt_intptr   = word64
global type wt_coord    = word32

global type ws_spoint= struct
    int16 x,y
end

global type ws_srect=struct
    int16 leftx,top, rightx,bottom
end

global type ws_charinfo=struct
    union
        wt_word unicodechar
        wt_char asciichar
    end union
    wt_word     attributes
end

global type ws_palette16=[0..15]int32

global type ws_console=struct
    ws_spoint size,pos
    wt_word attributes
    ws_srect window
    ws_spoint maxwindowsize
end

global type ws_consoleex=struct
    int32 recsize
    ws_spoint size,pos
    wt_word attributes
    ws_srect window
    ws_spoint maxwindowsize
    wt_word wpopup
    int32 fullscreen
    ws_palette16 palette
end

global type ws_keyevent = struct $caligned
    wt_word eventtype
        wt_bool keydown
        wt_word repeatcount
        wt_word virtualkeycode
        wt_word virtualscancode
        union
            wt_word unicodechar
            wt_char asciichar
        end
        wt_dword controlkeystate
end

global type ws_cursor=struct(int32 size,visible)

global var hconsole, hconsolein

global const stdoutputhandle=0xffff_fff5
global const stdinputhandle=0xfffffff6
global const stderrorputhandle=0xfffffff4
global const invalidhandlevalue=0xffffffff

global const maxpathlen=260

type spath=stringz*maxpathlen
type sshort=stringz*14
!
global type ws_filetime=struct
    int32 ftlow
    int32 fthigh
end

global type ws_finddata=struct
    int32       fileattributes
    ws_filetime creationtime
    ws_filetime lastaccesstime
    ws_filetime lastwritetime
    int32       filesizehigh
    int32       filesizelow
    int32       reserved0
    int32       reserved1
    spath       filename
    sshort      shortfilename
end

global type ws_systemtime = struct
    word16  year
    word16  month
    word16  dayofweek
    word16  day
    word16  hour
    word16  minute
    word16  second
    word16  milliseconds
end

global type ws_msg64 = struct $caligned
    ref void    hwnd
    int32       message
    int64       wparam
    int64       lparam
    int32       time
    int32       ptx
    int32       pty
end

global type ws_point = struct
    int32 x, y
end

global type ws_rect=struct      !rect record occupying 16 bytes
    union
        struct
            int32 leftx,top, rightx,bottom
        end
        struct
            union int32 x,x1 end
            union int32 y,y1 end
            int32 x2,y2
        end
    end
end

global type ws_logbrush = struct
    int32 lbstyle
    int32 lbcolour
    int32 lbhatch
end

global type ws_textmetrics = struct
    int32   height
    int32   ascent
    int32   descent
    int32   int32ernalleading
    int32   externalleading
    int32   avecharwidth
    int32   maxcharwidth
    int32   weight
    int32   overhang
    int32   digitizedaspectx
    int32   digitizedaspecty
    byte    firstchar
    byte    lastchar
    byte    defaultchar
    byte    breakchar
    byte    italic
    byte    underlined
    byte    struckout
    byte    pitchandfamily
    byte    charset
end

global type ws_bitmapv5header = struct
    int32   size
    int32   width
    int32   height
    word16  planes
    word16  bitcount
    int32   compression
    int32   sizeimage
    int32   xpelspermeter
    int32   ypelspermeter
    int32   clrused
    int32   clrimportant
    int32   redmask
    int32   greenmask
    int32   bluemask
    int32   alphamask
    int32   cstype
    [1..9]int32 endpoints
    int32   redgamma
    int32   greengamma
    int32   bluegamma
    int32   intent
    int32   profiledata
    int32   profilesize
    int32   reserved
end

global type ws_bitmapfileheader = struct
    wt_word     typex
    wt_dword    size
    wt_word     res1, res2
    wt_dword    offbits
end

global type ws_bitmapinfoheader = struct
    wt_dword    size
    wt_long     width
    wt_long     height
    wt_word     planes
    wt_word     bitcount
    wt_dword    compression
    wt_dword    sizeimage
    wt_long     xpelspermetre
    wt_long     ypelspermetre
    wt_dword    clrused
    wt_dword    clrimportant
end

global type ws_paintstruct = struct
    int64       hdc
    int32       erase
    ws_rect     paintrect
    int32       restore
    int32       incupdate
    [32]byte    rgbreserved
end

!32-BIT VERSION
global type ws_openfilename32 = struct
    wt_dword        structsize
    wt_handle32     owner
    wt_handle32     instance
    wt_string32     filter
    wt_string32     customfilter
    wt_dword        maxcustfilter
    wt_dword        filterindex
    wt_string32     file
    wt_dword        maxfile
    wt_string32     filetitle
    wt_dword        maxfiletitle
    wt_string32     initialdir
    wt_string32     title
    wt_dword        flags
    wt_word         fileoffset
    wt_word         fileextension
    wt_string32     defext
    wt_lparam32     custdata
    wt_wndproc32    hook
    wt_string32     templatename
    wt_ptr32        reserved1
    wt_dword        reserved2
    wt_dword        flagsex
end

!64-BIT VERSION
global type ws_openfilename64 = struct $caligned
    wt_dword        structsize
    wt_handle64     owner
    wt_handle64     instance
    wt_string64     filter
    wt_string64     customfilter
    wt_dword        maxcustfilter
    wt_dword        filterindex
    wt_string64     file
    wt_dword        maxfile
    wt_string64     filetitle
    wt_dword        maxfiletitle
    wt_string64     initialdir
    wt_string64     title
    wt_dword        flags
    wt_word         fileoffset
    wt_word         fileextension
    wt_string64     defext
    wt_lparam64     custdata
    wt_wndproc64    hook
    wt_string64     templatename
    wt_ptr64        reserved1
    wt_dword        reserved2
    wt_dword        flagsex
end

importdll kernel32=
    windows function    "GetLastError"                  :wt_dword
    windows function    "GetStdHandle"                  (wt_dword)wt_handle
    windows function    "WriteConsoleA" as writeconsole             (wt_handle,wt_string,wt_dword,wt_ptr,wt_ptr)wt_bool
    windows function    "SetConsoleCursorPosition"      (wt_handle,wt_coord)wt_bool
    windows function    "GetConsoleScreenBufferInfo"    (wt_handle,wt_ptr)wt_bool
    windows function    "SetConsoleMode"                (wt_handle,wt_dword)wt_bool
    windows function    "WriteConsoleOutputA" as writeconsoleoutput         (wt_handle,wt_ptr,wt_coord,wt_coord,wt_ptr)wt_bool

    windows function    "GetConsoleScreenBufferInfoEx"  (wt_handle,wt_ptr)wt_bool
    windows function    "SetConsoleScreenBufferInfoEx"  (wt_handle,wt_ptr)wt_bool

    windows function    "SetConsoleTextAttribute"       (wt_handle,wt_word)wt_bool
    windows function    "SetConsoleTitleA" as setconsoletitle               (wt_string)wt_bool
    windows function    "ReadConsoleInputA" as readconsoleinput         (wt_handle,wt_ptr,wt_dword,wt_ptr)wt_bool
    windows function    "PeekConsoleInputA"         (wt_handle,wt_ptr,wt_dword,wt_ptr)wt_bool
    windows function    "FlushConsoleInputBuffer"       (wt_handle)wt_bool
    windows function    "SetConsoleWindowInfo"          (wt_handle,wt_bool,wt_ptr)wt_bool
    windows function    "SetConsoleScreenBufferSize"    (wt_handle,wt_coord)wt_bool
    windows function    "GetConsoleCursorInfo"          (wt_handle,wt_ptr)wt_bool
    windows function    "SetConsoleCursorInfo"          (wt_handle,wt_ptr)wt_bool
    windows function    "GetNumberOfConsoleInputEvents"(wt_handle,wt_ptr)wt_bool

    windows function    "FindFirstFileA" as findfirstfile       (cstring,ref int32)int32
    windows function    "FindNextFileA"  as findnextfile            (int32,ref int32)int32
    windows function    "FindClose"                 (int32)int32
    windows function    "SetCurrentDirectoryA" as setcurrentdirectory   (cstring)int32
    windows function    "GetCurrentDirectoryA" as getcurrentdirectory   (int32,int32)int32
    windows function    "CreateDirectoryA" as createdirectory       (cstring,int32)int32
    windows function    "GetFileAttributesA"            (cstring)int32
    windows function    "GetModuleHandleA" as getmodulehandle       (wt_string)wt_handle
    windows function    "GetTickCount"                              :wt_dword
    windows function    "GlobalAlloc"                                   (wt_uint,wt_size)wt_handle
    windows function    "GlobalLock"                                    (wt_handle)wt_ptr
    windows function    "GlobalUnlock"                              (wt_handle)wt_bool
    windows function    "GlobalSize"                                    (wt_handle)wt_size

    windows function    "GetSystemTime"(ref byte)int32
    windows function    "Beep"                          (wt_dword, wt_dword)wt_bool
    windows function    "SetConsoleCP"                              (wt_uint)wt_bool
end

importdll user32=
    windows function    "CreateWindowExA" as createwindowex     (wt_dword, wt_string, wt_string, wt_dword, wt_int,wt_int,wt_int,wt_int,
                                                     wt_handle, wt_handle, wt_handle, wt_ptr)wt_handle

    windows function    "GetMessageA" as getmessage             (wt_ptr, wt_handle, wt_uint, wt_uint)wt_bool
    windows function    "TranslateMessage"                      (wt_ptr)wt_bool
    windows function    "DispatchMessageA" as dispatchmessage       (wt_ptr)wt_result
    windows function    "SetTimer"                              (wt_handle,wt_intptr,wt_uint,wt_ptr)wt_intptr
    windows function    "KillTimer"                             (wt_handle,wt_intptr)wt_bool
    windows function    "SystemParametersInfoA"                 (wt_uint,wt_uint,wt_ptr,wt_uint)wt_bool
    windows function    "GetSystemMetrics"                      (wt_int)wt_int
!   windows function    "CreateMenu"                                :int
    windows function    "AppendMenuA" as appendmenu             (wt_handle,wt_uint,wt_intptr,wt_string)wt_bool
    windows function    "GetDC"                                 (wt_handle)wt_handle
    windows function    "ReleaseDC"                             (wt_handle,wt_handle)wt_int

    windows function    "SendMessageA" as sendmessage               (wt_handle,wt_uint,wt_wparam,wt_lparam)wt_result
    windows function    "PostMessageA" as postmessage               (wt_handle,wt_uint,wt_wparam,wt_lparam)wt_bool
    windows function    "PeekMessageA" as peekmessage               (wt_ptr,wt_handle,wt_uint,wt_uint,wt_uint)wt_bool
    windows function    "BeginPaint"                                (wt_handle,wt_ptr)wt_handle
    windows function    "EndPaint"                              (wt_handle,wt_ptr)wt_bool
    windows proc    "PostQuitMessage"                   (wt_int)
    windows function    "LoadIconA" as loadicon                 (wt_handle,wt_string)wt_handle
    windows function    "LoadCursorA" as loadcursor             (wt_handle,wt_string)wt_handle
    windows function    "SetCursor"                             (wt_handle)wt_handle
    windows function    "DrawMenuBar"                               (wt_handle)wt_bool
    windows function    "GetSystemMenu"                         (wt_handle,wt_bool)wt_handle
    windows function    "CreateMenu"                                :wt_handle
    windows function    "CreatePopupMenu"                           :wt_handle
    windows function    "DestroyMenu"                               (wt_handle)wt_bool
    windows function    "CheckMenuItem"                         (wt_handle,wt_uint,wt_uint)wt_dword
    windows function    "EnableMenuItem"                            (wt_handle,wt_uint,wt_uint)wt_bool
    windows function    "GetSubMenu"                                (wt_handle,wt_int)wt_handle
    windows function    "GetMenuItemID"                         (wt_handle,wt_int)wt_uint
    windows function    "GetMenuItemCount"                      (wt_handle)wt_int
    windows function    "InsertMenuA" as insertmenu             (wt_handle,wt_uint,wt_uint,wt_intptr,wt_string)wt_bool
    windows function    "ModifyMenuA" as modifymenu             (wt_handle,wt_uint,wt_uint,wt_intptr,wt_string)wt_bool
    windows function    "RemoveMenu"                                (wt_handle,wt_uint,wt_uint)wt_bool
    windows function    "DeleteMenu"                                (wt_handle,wt_uint,wt_uint)wt_bool

    windows function    "DestroyWindow"                         (wt_handle)wt_bool
    windows function    "InvalidateRect"                            (wt_handle,wt_ptr,wt_bool)wt_bool
    windows function    "ValidateRect"                          (wt_handle,wt_ptr)wt_bool
    windows function    "ShowWindow"                                (wt_handle,wt_int)wt_bool
    windows function    "GetClassLongA" as getclassint          (wt_handle,wt_int)wt_word
    windows function    "SetClassLongA" as setclasslong         (wt_handle,wt_int,wt_dword)wt_word
    windows function    "SetWindowTextA" as setwindowtext           (wt_handle,wt_string)wt_bool
    windows function    "GetWindowTextA" as getwindowtext           (wt_handle,wt_string,wt_int)wt_int
    windows function    "GetWindowTextLengthA" as getwindowtextlength   (wt_handle)wt_int
    windows function    "GetKeyState"                               (wt_int)wt_word

!   windows function    "GetWindowLongPtrA" as getwindowlongptr (wt_handle,wt_int)int64
!   windows function    "SetWindowLongPtrA" as setwindowlongptr (wt_handle,wt_int,wt_int)int64
    windows function    "GetWindowLongA" as getwindowlongptr        (wt_handle,wt_int)int64
    windows function    "SetWindowLongA" as setwindowlongptr        (wt_handle,wt_int,int64)int64

    windows function    "GetClientRect"                         (wt_handle,wt_ptr)wt_bool
    windows function    "ClientToScreen"                            (wt_handle,wt_ptr)wt_bool
    windows function    "ScreenToClient"                            (wt_handle,wt_ptr)wt_bool
    windows function    "GetWindowRect"                         (wt_handle,wt_ptr)wt_bool
    windows function    "GetSysColor" as getsyscolour               (wt_int)wt_dword
    windows function    "GetScrollInfo"                         (wt_handle,wt_int,wt_ptr)wt_bool
    windows function    "GetMenu"                                   (wt_handle)wt_handle
    windows function    "SetMenu"                                   (wt_handle,wt_handle)wt_ptr
    windows function    "TrackPopupMenu"                            (wt_handle,wt_uint,wt_int,wt_int,wt_int,wt_handle,wt_ptr)wt_bool
    windows function    "GetMenuState"                          (wt_handle,wt_uint,wt_uint)wt_uint
    windows function    "MessageBoxA" \
                                (wt_handle a=nil,wt_string message, wt_string caption="Caption", wt_uint b=0)wt_int
    windows function    "OpenClipboard"                         (wt_handle)wt_bool
    windows function    "CloseClipboard"                            :wt_bool
    windows function    "EmptyClipboard"                            :wt_bool
    windows function    "GetClipboardData"                      (wt_uint)wt_handle
    windows function    "SetClipboardData"                      (wt_uint,wt_handle)wt_handle
    windows function    "MessageBeep"                           (wt_uint x=0)wt_bool
end

importdll gdi32=
    windows function    "Rectangle"                             (wt_handle,wt_int,wt_int,wt_int,wt_int)wt_bool
    windows function    "RoundRect"                             (wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int)wt_bool
    windows function    "Ellipse"                                   (wt_handle,wt_int,wt_int,wt_int,wt_int)wt_bool
    windows function    "Arc"                                       (wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int)wt_bool
    windows function    "Chord"                                 (wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int)wt_bool
    windows function    "Pie"                                       (wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int)wt_bool
    windows function    "Polygon"                                   (wt_handle,wt_handle,wt_int)wt_bool
    windows function    "TextOutA" as textout                       (wt_handle,wt_int,wt_int,wt_string,wt_int)wt_bool
    windows function    "TextOutW"                      (wt_handle,wt_int,wt_int,wt_ptr,wt_int)wt_bool
    windows function    "GetStockObject"                            (wt_int)wt_handle
    windows function    "SelectObject"                          (wt_handle,wt_handle)wt_handle
    windows function    "CreateDCA" as createdc                 (wt_string,wt_string,wt_string,wt_ptr)wt_handle
    windows function    "MoveToEx"                      (wt_handle a,wt_int b,wt_int c,wt_ptr d=nil)wt_bool
    windows function    "CreatePen"                             (wt_int,wt_int,wt_dword)wt_handle
    windows function    "CreateSolidBrush"                      (wt_dword)wt_handle
    windows function    "CreateBrushIndirect"                       (wt_ptr)wt_handle
    windows function    "LineTo"                                    (wt_handle,wt_int,wt_int)wt_bool
    windows function    "GetPixel"                              (wt_handle,wt_int,wt_int)wt_dword
    windows function    "SetPixel"                              (wt_handle,wt_int,wt_int,wt_dword)wt_dword
    windows function    "SetGraphicsMode"                           (wt_handle,wt_int)wt_int
    windows function    "CreateFontIndirectA" as createfontindirect (wt_ptr)wt_handle
    windows function    "CreateFontA" as createfont \
            (wt_int height, wt_int width=0, wt_int escapement=0, wt_int orientation=0, wt_int bold=0,
             wt_dword italic=0, wt_dword underline=0, wt_dword strikeout=0, wt_dword charset=0,
             wt_dword outprec=0, wt_dword clipprec=0, wt_dword quality=0, wt_dword pitch=0, wt_string facename)wt_handle
    windows function    "SaveDC"                                    (wt_handle)wt_int
    windows function    "GetTextMetricsA" as gettextmetrics     (wt_handle,wt_ptr)wt_bool
    windows function    "DeleteObject"                          (wt_handle)wt_bool
    windows function    "RestoreDC"                             (wt_handle,wt_int)wt_bool
    windows function    "GetTextExtentPoint32A" as gettextextentpoint32 (wt_handle,wt_string,wt_int,wt_ptr)wt_bool
    windows function    "GetObjectA" as getobject                   (wt_handle,wt_int,wt_ptr)wt_int
    windows function    "CreatePalette"                         (wt_ptr)wt_handle
    windows function    "GetWindowExtEx"                            (wt_handle,wt_ptr)wt_bool
    windows function    "CreateCompatibleBitmap"                    (wt_handle,wt_int,wt_int)wt_handle
    windows function    "SetBitmapBits"                         (wt_handle,wt_dword,wt_ptr)wt_long
    windows function    "SelectPalette"                         (wt_handle,wt_handle,wt_bool)wt_handle
    windows function    "RealizePalette"                            (wt_handle)wt_uint
    windows function    "SetDIBitsToDevice"                     (wt_handle,wt_int,wt_int,wt_dword,wt_dword,wt_int,wt_int,wt_uint,wt_uint,wt_ptr,wt_ptr,wt_uint)wt_int
    windows function    "StretchDIBits"                         (wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_ptr,wt_ptr,wt_uint,wt_dword)wt_int
    windows function    "SetStretchBltMode"                     (wt_handle,wt_int)wt_int
    windows function    "PatBlt"                                    (wt_handle,wt_int,wt_int,wt_int,wt_int,wt_dword)wt_bool
    windows function    "BitBlt"                                    (wt_handle,wt_int,wt_int,wt_int,wt_int,wt_handle,wt_int,wt_int,wt_dword)wt_bool
    windows function    "SetROP2"                                   (wt_handle,wt_int)wt_int
    windows function    "CreateCompatibleDC"                        (wt_handle)wt_handle
    windows function    "DeleteDC"                              (wt_handle)wt_bool
    windows function    "CreateBitmap"                          (wt_int,wt_int,wt_uint,wt_uint,wt_ptr)wt_handle
    windows function    "CreateBitmapIndirect"                  (wt_ptr)wt_handle
    windows function    "CreateDIBitmap"                            (wt_handle,wt_ptr,wt_dword,wt_ptr,wt_ptr,wt_uint)wt_handle
    windows function    "CreateDIBSection"                      (wt_handle,wt_ptr,wt_uint,wt_ptr,wt_handle,wt_dword)wt_handle
    windows function    "StretchBlt"                                (wt_handle,wt_int,wt_int, wt_int,wt_int,wt_handle, wt_int,wt_int,wt_int, wt_int,wt_dword)wt_bool
    windows function    "PlgBlt"                                (wt_handle,wt_ptr,wt_handle, wt_int,wt_int,wt_int,wt_int, wt_handle, wt_int,wt_int)wt_bool
    windows function    "SetTextColor"  as settextcolour            (wt_handle,wt_dword)wt_dword
    windows function    "SetTextAlign"                          (wt_handle,wt_uint)wt_uint
    windows function    "SetTextJustification"                  (wt_handle,wt_int,wt_int)wt_bool
    windows function    "SetBkColor"  as setbkcolour                (wt_handle,wt_dword)wt_dword
    windows function    "SetBkMode"                             (wt_handle,wt_int)wt_int
    windows function    "GetBkColor"  as getbkcolour                (wt_handle)wt_dword
    windows function    "GetBkMode"                             (wt_handle)wt_int
    windows function    "StartDocA" as startdoc                 (wt_handle,wt_ptr)wt_int
    windows function    "StartPage"                             (wt_handle)wt_int
    windows function    "EndPage"                                   (wt_handle)wt_int
    windows function    "EndDoc"                                    (wt_handle)wt_int
    windows function    "AbortDoc"                              (wt_handle)wt_int
    windows function    "GetViewportOrgEx"                      (wt_handle,wt_ptr)wt_bool
    windows function    "GetDIBits"                             (wt_handle,wt_handle,wt_uint,wt_uint,wt_ptr,wt_ptr,wt_uint)wt_int
    windows function    "GetDIBColorTable" as getdibcolourtable (wt_handle,wt_uint,wt_uint,wt_ptr)wt_uint
    windows function    "SetDIBColorTable" as setdibcolourtable (wt_handle,wt_uint,wt_uint,wt_ptr)wt_uint
    windows function    "GetTextAlign"                          (wt_handle)wt_uint
end

importdll comdlg32=
    windows function    "GetOpenFileNameA"                      (wt_ptr)wt_bool
    windows function    "GetSaveFileNameA"                      (wt_ptr)wt_bool
end
