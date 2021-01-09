import oslib
import gxlib

var grid
var wgrid
var currx,curry     !position of zero square
var cols,rows

const bgndcol   = white

var offsetx,offsety
var hside, vside

const wsizex=600, wsizey=400
const margin=20
const maxside=80

proc start=
    defaultdata()

    makewindow("15 Puzzle")
    gxmenubar(wgrid,("201 &Reset","202 &Exit"))
    gxhandler(wgrid,mm_click,gridclickhandler)

    showgrid()

    do
        gxaskmess()

        case currmess.message
        when mm_close, mm_cancel then
            exit
        when mm_pick then
            makemove(currmess.a,currmess.b)
        when mm_command then
            case currmess.a
            when 201 then           !clear
                defaultdata()
                showgrid()

            when 202 then           !exit
                exit
            esac
        esac

    od
end

proc defaultdata=
    cols:=rows:=4

    hside:=maxside*cols
    vside:=maxside*rows

    grid::=(
        (1,2,3,4),
        (5,6,7,8),
        (9,10,11,12),
        (13,14,15,0))

    (currx,curry):=(4,4)
end

proc showgrid=
    gxfont(wgrid,10)

    for r:=1 to rows do
        for c:=1 to cols do
            showcell(grid[r,c],c,r)
        od
    od

    drawgrid()
end

proc drawgrid=
!display just the grid outline

    gxcolour(wgrid,getrgb(black))
    gxwidth(wgrid,2)
    gxrect(wgrid,offsetx,offsety,hside+1,vside+1)
    gxwidth(wgrid,0)

    for i:=1 to rows-1 do
        gxline(wgrid,offsetx,i*maxside+offsety,offsetx+hside,i*maxside+offsety)
    od
    for i:=1 to cols-1 do
        gxline(wgrid,i*maxside+offsetx,offsety,i*maxside+offsetx,offsety+vside)
    od

end

proc showcell(n, c,r)=
!display number Nat position (c,r)

    x:=(c-1)*maxside+offsetx
    y:=(r-1)*maxside+offsety

    gxfillrect(wgrid,x+1,y+1,maxside-1,maxside-1,getrgb(white),0)

    if n then
        gxtext(wgrid,tostr(n),x+(maxside*2)%(n<10|5|8),y+maxside%4+3)
    else
        x:=(c-1)*maxside+offsetx
        y:=(r-1)*maxside+offsety
        gxcolour(wgrid,getrgb(red))
        gxrect(wgrid,x+1,y+1,maxside-1,maxside-1)
        gxrect(wgrid,x+2,y+2,maxside-3,maxside-3)
    fi
end

function gridclickhandler(mess,w)=
    x:=mess.x-offsetx
    y:=mess.y-offsety

    if x<0 or x>=hside or y<0 or y>=vside then
        return thismess
    fi

    col:=x%maxside+1
    row:=y%maxside+1

    postmess(w,mm_pick,col,row)

    return skipmess         !else ignore message if not enabled
end

proc makewindow(caption)=
!create first-time window or re-calc dims when puzzle grid is resized
    const wm_size   = 7

    if not wgrid.defined then
        wgrid:= gxcreatewindow(caption:caption,dim:(wsizex,wsizey),pos:(10,100))
    fi

    gxdrawmode(wgrid,dm_screenmemory)
    gxclear(wgrid)

    calcdims()
    gxloadfont(10,"Arial","",(maxside*3)%6)
end

proc calcdims(?m,?s)=
    dimx:=wgrid.dimx
    dimy:=wgrid.dimy

    if not m.defined then m:=margin fi

    cx:=(dimx-m*2)/cols
    cy:=(dimy-m*2)/rows

    if not s.defined then s:=maxside fi
    side:=int(int(cx min cy) min s)
    hside:=side*cols
    vside:=side*rows

    offsetx:=(dimx-hside)%2
    offsety:=(dimy-vside)%2
end

proc error= {beep1()}

function getsquare(x,y) =
    if x in 1..cols and y in 1..rows then
        return grid[y,x]
    else
        return -1
    fi
end

proc makemove(x,y)=
    if getsquare(x,y)=0 then error(); return fi

    (cx,cy):=(x,y)
    if getsquare(x-1,y)=0 then
        cx:=x-1
    elsif getsquare(x+1,y)=0 then
        cx:=x+1
    elsif getsquare(x,y-1)=0 then
        cy:=y-1
    elsif getsquare(x,y+1)=0 then
        cy:=y+1
    else
        error()
        return
    fi

    swap(grid[y,x],grid[cy,cx])
    showgrid()
end
