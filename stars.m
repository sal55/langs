import clib
import msys
import oslib
import opengl

cclib opengl32,glu32,glut32

enum (normal = 0)
enum (weird = 1)
enum (streak = 0)
enum (circle = 1)

record starrec =
    int32 typex
    [0:2]real32 x
    [0:2]real32 y
    [0:2]real32 z
    real32 offsetx
    real32 offsety
    real32 offsetr
    real32 rotation
end

const maxstars=10000
const maxangles=600
const maxpos=10000

const maxwarp=200

global word32 doublebuffer
global int32 windw = 1800
global int32 windh = 1000
global word32 flag = normal
global int32 starcount = maxstars/2
global real32 speed = 2.000000
global int32 nitro = 0
global [0:maxstars]starrec stars
global [0:maxangles]real32 sintable

proc stopfn= stop end

global function sinx(real32 angle)real32 =
   return sintable[real(angle)]
end

global function cosx(real64 angle)real32 =
    return sintable[(int(angle)+(maxangles/4)) rem maxangles]
end

global proc newstar(int32 n, d) =
    if rand() rem 4=0 then
        stars[n].typex := circle
    else
        stars[n].typex := streak
    fi
    stars[n].x[0] := real32(rand() rem maxpos-maxpos/2)
    stars[n].y[0] := real32(rand() rem maxpos-maxpos/2)
    stars[n].z[0] := real32(rand() rem maxpos+d)
    stars[n].x[1] := stars[n].x[0]
    stars[n].y[1] := stars[n].y[0]
    stars[n].z[1] := stars[n].z[0]
    if rand() rem 4=0 and flag=weird then
        stars[n].offsetx := real32(rand() rem 100-100/2)
        stars[n].offsety := real32(rand() rem 100-100/2)
        stars[n].offsetr := real32(rand() rem 25-25/2)
    else
        stars[n].offsetx := 0.000000
        stars[n].offsety := 0.000000
        stars[n].offsetr := 0.000000
    fi
end

global proc rotatepoint(ref real32 x, y, real32 rotation) =
    real32 tmpx
    real32 tmpy

    tmpx := x^*cos(rotation)-y^*sin(rotation)
    tmpy := y^*cos(rotation)+x^*sin(rotation)
    x^ := tmpx
    y^ := tmpy
end

global proc movestars() =
    real32 offset
    int32 n

    offset := speed*60.000000
    n := 0

    while n<starcount do
        stars[n].x[1] := stars[n].x[0]
        stars[n].y[1] := stars[n].y[0]
        stars[n].z[1] := stars[n].z[0]
        stars[n].x[0] +:= stars[n].offsetx
        stars[n].y[0] +:= stars[n].offsety
        stars[n].z[0] -:= offset
        stars[n].rotation +:= stars[n].offsetr
        if stars[n].rotation>maxangles then
            stars[n].rotation := 0.000000
        fi
        n++
    od
end

global function starpoint(int32 n)word =
    real32 x0
    real32 y0

    x0 := stars[n].x[0]*windw/stars[n].z[0]
    y0 := stars[n].y[0]*windh/stars[n].z[0]
    rotatepoint(&x0,&y0,stars[n].rotation)
    x0 +:= windw/2.000000
    y0 +:= windh/2.000000
    if x0>=0.000000 and x0<windw and y0>=0.000000 and y0<windh then
        return gl_true
    else
        return gl_false
    fi
end

global proc showstar(int32 n) =
    real32 x0
    real32 y0
    real32 x1
    real32 y1
    real32 width
    int32 i
    real32 x
    real32 y

    x0 := stars[n].x[0]*windw/stars[n].z[0]
    y0 := stars[n].y[0]*windh/stars[n].z[0]

    rotatepoint(&x0,&y0,stars[n].rotation)
    x0 +:= windw/2.000000
    y0 +:= windh/2.000000
    if x0>=0.000000 and x0<windw and y0>=0.000000 and y0<windh then
      if stars[n].typex=streak then
            x1 := stars[n].x[1]*windw/stars[n].z[1]
            y1 := stars[n].y[1]*windh/stars[n].z[1]
            rotatepoint(&x1,&y1,stars[n].rotation)
            x1 +:= windw/2.000000
            y1 +:= windh/2.000000
            gllinewidth(maxpos/100.000000/stars[n].z[0]+1.000000)
            glcolor3f(1.000000,maxwarp-speed/maxwarp,maxwarp-speed/maxwarp)
            if abs(x0-x1)<1.000000 and abs(y0-y1)<1.000000 then
                glbegin(gl_points)
                glvertex2f(x0,y0)
                glend()
            else
                glbegin(gl_lines)
                glvertex2f(x0,y0)
                glvertex2f(x1,y1)
                glend()
            fi
        else
            width := maxpos/10.000000/stars[n].z[0]+1.000000
            glcolor3f(1.000000,0.000000,0.000000)
            glbegin(gl_polygon)
            i := 0

            while i<8 do
                x := x0+width*cosx(real32(i)*maxangles/8.000000)
                y := y0+width*sinx(real32(i)*maxangles/8.000000)
                glvertex2f(x,y)
                i++
            od
            glend()
        fi
    fi
end

global proc updatestars() =
    int32 n

    glclear(gl_color_buffer_bit)
    n := 0
    while n<starcount do
        if stars[n].z[0]>speed or stars[n].z[0]>0.000000 and speed<maxwarp then
            if starpoint(n)=gl_false then
                newstar(n,maxpos)
            fi
        else
            newstar(n,maxpos)
        fi
        n++
    od
end

global proc showstars() =
    int32 n

    glclear(gl_color_buffer_bit)
    n := 0
    while n<starcount do
        if stars[n].z[0]>speed or stars[n].z[0]>0.000000 and speed<maxwarp then
            showstar(n)
        fi
        n++
    od
end

proc initx =
    real64 angle
    int32 n

    srand(word32(clock()))
    n := 0
    while n<maxstars do
        newstar(n,100)
        n++
    od
    angle := 0.000000
    n := 0
    while n<=maxangles do
        sintable[n] := sin(angle)
        angle +:= pi/(maxangles/2.000000)
       n++
    od
    glclearcolor(0.000000,0.000000,0.000000,0.000000)
    gldisable(gl_dither)
end

global proc reshape(int32 width, height) =
    windw := width
    windh := height
    glviewport(0,0,windw,windh)
    glmatrixmode(gl_projection)
    glloadidentity()
    gluortho2d(-0.500000,windw+0.500000,-0.500000,windh+0.500000)
    glmatrixmode(gl_modelview)
end

proc key(byte key, int32 x, y) =
    switch key
    when ' ' then
        flag := (flag=normal|weird|normal)
    when 't' then
        nitro := 1
    when 27 then
        stop
    end switch
end

global proc idle() =
    movestars()
    updatestars()
    if nitro>0 then
        speed := real32(nitro/10)+1.000000
        if speed>maxwarp then
            speed := maxwarp
        fi
        if ++nitro>maxwarp*10 then
            nitro := -nitro
        fi
    else
        if nitro<0 then
            nitro++
            speed := real32(-nitro/10)+1.000000
            if speed>maxwarp then
                speed := maxwarp
            fi
        fi
    fi
    glutpostredisplay()
end

global proc display() =
    showstars()
    if doublebuffer then
        glutswapbuffers()
    else
        glflush()
    fi
end

global proc visible(int32 state) =
    if state=glut_visible then
        glutidlefunc(cast(idle))
    else
        glutidlefunc(nil)
    fi
end

proc args(int32 argc, ref[0:]ichar argv) =
    int32 i

    doublebuffer := gl_true
    i := 1
    while i<argc do
        if strcmp(argv[i],"-sb")=0 then
            doublebuffer := gl_false
        else
            if strcmp(argv[i],"-db")=0 then
                doublebuffer := gl_true
            fi
        fi
        i++
    od
end

proc start=
    word32 typex

    glutinitwindowsize(windw,windh)
    glutinit(cast(&nsysparams),cast(&sysparams))

    args(nsysparams,cast(&sysparams))
    typex := glut_rgb
    typex ior:= (doublebuffer|glut_double|glut_single)
    glutinitdisplaymode(typex)
    glutcreatewindow("Stars")
    initx()

    glutreshapefunc(cast(reshape))
    glutkeyboardfunc(cast(key))
    glutvisibilityfunc(cast(visible))
    glutdisplayfunc(cast(display))
    glutmainloop()
end
