import oslib
import gxlib
import smlib

var grid
var cols=4,rows=4
var buttons

proc start=
    initdata()

    sminit()

    A:=smdefblock(dim:(70,70),cells:(4,4),gap:0,dir:"V")
    smcreate("15 Puzzle",smmenusize())

    smblock(a)
    for y to 4 do for x to 4 do
        buttons[y,x]:=smcmd("X",200+y*10+x)
    od od

    showgrid()

    do
        gxaskmess()

        case currmess.message
        when mm_close, mm_cancel then
            exit
        when mm_command then
            x:=currmess.a rem 10
            y:=(currmess.a-200)%10
            makemove(x,y)
        esac

    od
end

proc initdata=
    buttons::=((0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0))

    grid::=(
        (1,2,3,4),
        (5,6,7,8),
        (9,10,11,12),
        (13,14,15,0))

    gxloadfont(10,"Arial","",40)
end

proc showgrid=
    for r:=1 to rows do
        for c:=1 to cols do
            btn:=buttons[c,r]
            n:=grid[c,r]
            gxclear(btn)
            if n then
                gxfont(btn,10)
                gxtext(btn,tostr(n),(n<10|20|10),10)
            fi
        od
    od
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
