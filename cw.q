import winmessages
import winconsts
import gxmisc
import winapi

global var hwapplic=nil
global var hwchild=nil
global var iswin32
global var screendc

global var nglobalfonts=0
global var fonttable::=()           ![]font handles
global var fontdimtable::=()        ![]rpoint (width,total line height)
global var fontvdimtable::=()       ![]rpoint (ascenders, descenders) 

proc start  =
    initdata()
end

proc initdata=
    iswin32:=(getos()="W32")
    screendc:=getdc(nil)

    fonttable:=(0,)*20
    fontdimtable:=(0,)*20
    fontvdimtable:=(0,)*20

    fonttable[1]:=getstockobject(17)    !default gui
    fonttable[2]:=getstockobject(13)    !system font
    fonttable[3]:=getstockobject(16)    !system fixed
    fonttable[4]:=getstockobject(10)    !oem fixed
    for i:=1 to 4 do
        fontdimtable[i]::=ws_point(0,0)
        fontvdimtable[i]::=ws_point(0,0)
    od
    nglobalfonts:=4
end

global proc wx_waitmess=
    windmsg:=new((iswin32|ws_msg32|ws_msg64))

    do
        if getmessage(&windmsg,nil,0,0)<>0 then
            w:=windmsg.hwnd
            if windmsg.message=wm_keydown and windmsg.wparam=27 then exit fi
            if windmsg.message=wm_timer then CPL "TIMER!!" fi
            translatemessage(&windmsg)
            dispatchmessage(&windmsg)
            if windmsg.message=wm_close then exit fi
        else
            exit
        fi
    od
end

global function wx_getw(hwnd)=
!return allwindow-index of window that has been stored into it
    n:=getwindowlongptr(hwnd, gwl_userdata)
    return n
end

global proc wx_setw(hwnd,index)=
!store mm window handle into win32 window
!index is .gindex (index into allwindows)
    setwindowlongptr(hwnd, gwl_userdata, index)
end

global function wx_gettextwidth(hdc,s)=
    size:=new(ws_point)
    gettextextentpoint32(hdc,s,s.len,&size)
    return size.x
end

global function wx_createpopup(?caption,?pos,?dim,?options,owner=nil)=
!wrapper around win32 createwindow
!return win32 handle to newly created window
    const gap=40
    const smallestwidth=150

    if options.isvoid then
 options:=[wf_caption:1,wf_border:wbs_resize]
    fi

    posx:=posy:=-1
    dimx:=640
    dimy:=480
    fcentre:=0
    fautopos:=0
    fmax:=fdesktop:=0

    if caption.isvoid then caption:="<No Caption>" fi

    if dim.defined then
        if dim.isstring and dim="max" then
            fmax:=1
        elsif dim.isstring and dim="desktop" then
            fdesktop:=1
        else
            dimx:=dim[1]
            dimy:=dim[2]
        fi
    fi

    if pos.isvoid or pos="cent" then
        fcentre:=1
    elsif pos="auto" then
        fautopos:=1
    elsif pos.defined and not pos.isstring then
        posx:=pos[1]
        posy:=pos[2]
    else                !check options?
        abort("gxcw bad pos")
    fi

    bstyle:=bxstyle:=0
    nocap:=0            !whether to suppress caption

    framex:=framey:=0

    case options{wf_border,wbs_resize}
    when wbs_none then      !no border
        nocap:=1
        framex:=0
        framey:=0
    when wbs_simple then        !single line
        nocap:=1
        bstyle:=ws_border
        framex:=1
        framey:=1
    when wbs_thick then     !thick line
        bstyle:=ws_dlgframe
        fixedframe:=0
        framex:=getsystemmetrics(sm_cxfixedframe)
        framey:=getsystemmetrics(sm_cyfixedframe)
    when wbs_resize then
        bstyle:=ws_sizebox
        framex:=getsystemmetrics(sm_cxsizeframe)
        framey:=getsystemmetrics(sm_cysizeframe)
    when wbs_sunken,wbs_sunken2 then        !sunken
        bstyle:=ws_dlgframe
        bxstyle:=ws_ex_clientedge
        framex:=5
        framey:=5
    when wbs_sunkenrs then
        bstyle:=ws_sizebox
        bxstyle:=ws_ex_clientedge
        framex:=6
        framey:=6
    esac

    capheight:=getsystemmetrics(sm_cycaption)
    mbheight:=getsystemmetrics(sm_cymenu)

    style:=0
    exstyle:=0

    if options{wf_show,1} then
        style ior:=ws_visible
    fi

    mxleft:=framex
    mxright:=framey
    mytop:=framey+capheight
    mybottom:=framey
    showstyle:=sw_shownormal

    hcwmenu:=nil
    if options{wf_menu,0}=1 then
        mytop+:=mbheight
        hcwmenu:=createmenu()
        appendmenu(hcwmenu,0,998,"fred")
    fi

    style ior:=ws_clipchildren

    if nocap or options{wf_caption,1}=0 then
        mytop-:=capheight
        style ior:=ws_popup
    fi

    if options{wf_iframe,0}=0 then
        if not fautopos then
            posx-:=mxleft
            posy-:=mytop
        fi
        dimx+:=mxleft+mxright
        dimy+:=mytop+mybottom
    fi

    if fcentre or options{wf_cent,0}=1 then
        fautopos:=0
        box:=new(ws_rect)
        systemparametersinfoa(spi_getworkarea,0,&box,0)
        posx:=box.rightx%2-dimx%2
        posy:=(box.bottom-box.top)%2-dimy%2+box.top
    fi

    if fmax or options{wf_max,0} then
        showstyle:=sw_maximize
        style ior:=ws_maximize
    fi


    if options{wf_minmax,1}=1 then
        style ior:=(ws_maximizebox ior ws_minimizebox)

    fi

    if options{wf_sysmenu,1}=1 then
        style ior:=ws_sysmenu
    fi

    if fautopos=0 and options{wf_clip,0}=1 then
        box:=new(ws_rect)
        systemparametersinfoa(spi_getworkarea,0,&box,0)

        if posx<box.leftx+gap then posx:=box.leftx+gap fi

        if posy<box.top+gap then posy:=box.top+gap fi
        dimxmin:=dimx max smallestwidth
        if posx+dimxmin>=box.rightx+gap then posx:=box.rightx-gap-dimxmin fi
        if posy+dimy>=box.bottom+gap then posy:=box.bottom-gap-dimy fi
    elsif fautopos then
        posx:=posy:=cw_usedefault
    fi

    if fdesktop or options{wf_desktop,0}=1 then
        box:=new(ws_rect)
        systemparametersinfoa(spi_getworkarea,0,&box,0)
        posx:=box.leftx
        posy:=box.top
        dimx:=box.rightx-box.leftx
        dimy:=box.bottom-box.top
    fi

    if options{wf_toolwind,0}=1 then
        exstyle ior:=ws_ex_toolwindow
    fi

    classname:="pcc001"

    STYLE IOR:=WS_VISIBLE

    style ior:=bstyle
    exstyle ior:=bxstyle

    hwnd:=createwindowex(
        exstyle,
        classname,
        caption,
        style,
        posx,posy,          !initial position and size
        dimx,dimy,
        owner,          !will be 0 for 1st window, other popups use hwapplic as owner
        hcwmenu,            !menu handle
        nil,    !proginstance,      !instance handle
        nil)            !creation params

    if hwnd=nil then
        e:=getlasterror()
        abort("wx:Can't create popup window "+tostr(e))
    fi
    return hwnd
end

global function wx_createcontrol(?pos,?dim,border=wbs_simple,owner)=
!wrapper around win32 createwindow
!return win32 handle to newly created window
    const gap=40
    const smallestwidth=150

    posx:=posy:=0
    dimx:=160
    dimy:=120

    if dim.defined then
        dimx:=dim[1]
        dimy:=dim[2]
    fi

    if pos.defined then
        posx:=pos[1]
        posy:=pos[2]
    fi

    bstyle:=bxstyle:=0

    case border
    when wbs_none then          !no border
    when wbs_simple then        !single line
        bstyle:=ws_border
    else
        pcerror("createcontrol/bad border "+wbsnames[border])
    esac

    style:=0
    exstyle:=0

    style ior:=ws_clipchildren

    classname:="pcc001"

    style ior:=ws_child
    style ior:=ws_visible

    style ior:=bstyle
    exstyle ior:=bxstyle

    hwnd:=createwindowex(
        exstyle,
        classname,
        nil,
        style,
        posx,posy,          !initial position and size
        dimx,dimy,
        owner,              !will be 0 for 1st window, other popups use hwapplic as owner
        nil,                !menu handle
        nil,
        nil)                !creation params

    if hwnd=0 then
        e:=getlasterror()
        abort("wx:Can't create child window "+tostr(e))
    fi

    return hwnd
end
