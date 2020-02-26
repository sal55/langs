! C to M Converter

import clib
import oslib
import opengl

cclib opengl32,glu32,glut32
!cclib "GL", "GLU", "glut"

enum (NORMAL = 0)
enum (WEIRD = 1)
enum (STREAK = 0)
enum (CIRCLE = 1)

record starRec =
    int32 typex
    [0:2]real32 x
    [0:2]real32 y
    [0:2]real32 z
    real32 offsetX
    real32 offsetY
    real32 offsetR
    real32 rotation
end

!const maxstars=40000
!const maxangles=6000
!const maxpos=10'000
!const maxwarp=10

const maxstars=4000
const maxangles=600
const maxpos=10'000
const maxwarp=20

global word32 doubleBuffer
global int32 windW = 600
global int32 windH = 600
global word32 flag = NORMAL
global int32 starCount = MAXSTARS/2
global real32 speed = 2.000000
global int32 nitro = 0
global [0:MAXSTARS]starRec stars
global [0:MAXANGLES]real32 sinTable

callback proc stopfn= stop end

global function sinx(real32 angle)real32 =
   return sinTable[int32(angle)]
end

global function cosx(real32 angle)real32 =
    return sinTable[(int32(angle)+(MAXANGLES/4)) rem MAXANGLES]
end

global proc newstar(int32 n, d) =
    if rand() rem 4=0 then
        stars[n].typex := CIRCLE
    else
        stars[n].typex := STREAK
    fi
    stars[n].x[0] := real32(rand() rem MAXPOS-MAXPOS/2)
    stars[n].y[0] := real32(rand() rem MAXPOS-MAXPOS/2)
    stars[n].z[0] := real32(rand() rem MAXPOS+d)
    stars[n].x[1] := stars[n].x[0]
    stars[n].y[1] := stars[n].y[0]
    stars[n].z[1] := stars[n].z[0]
    if rand() rem 4=0 and flag=WEIRD then
        stars[n].offsetX := real32(rand() rem 100-100/2)
        stars[n].offsetY := real32(rand() rem 100-100/2)
        stars[n].offsetR := real32(rand() rem 25-25/2)
    else
        stars[n].offsetX := 0.000000
        stars[n].offsetY := 0.000000
        stars[n].offsetR := 0.000000
    fi
end

global proc rotatepoint(ref real32 x, y, real32 rotation) =
    real32 tmpX
    real32 tmpY

    tmpX := x^*cos(rotation)-y^*sin(rotation)
    tmpY := y^*cos(rotation)+x^*sin(rotation)
    x^ := tmpX
    y^ := tmpY
end

global proc movestars() =
    real32 offset
    int32 n

    offset := speed*60.000000
    n := 0

    while n<starCount do
        stars[n].x[1] := stars[n].x[0]
        stars[n].y[1] := stars[n].y[0]
        stars[n].z[1] := stars[n].z[0]
        stars[n].x[0] +:= stars[n].offsetX
        stars[n].y[0] +:= stars[n].offsetY
        stars[n].z[0] -:= offset
        stars[n].rotation +:= stars[n].offsetR
        if stars[n].rotation>MAXANGLES then
            stars[n].rotation := 0.000000
        fi
        n++
    od
end

global function starpoint(int32 n)word32 =
    real32 x0
    real32 y0

    x0 := stars[n].x[0]*windW/stars[n].z[0]
    y0 := stars[n].y[0]*windH/stars[n].z[0]
    RotatePoint(&x0,&y0,stars[n].rotation)
    x0 +:= windW/2.000000
    y0 +:= windH/2.000000
    if x0>=0.000000 and x0<windW and y0>=0.000000 and y0<windH then
        return GL_TRUE
    else
        return GL_FALSE
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

    x0 := stars[n].x[0]*windW/stars[n].z[0]
    y0 := stars[n].y[0]*windH/stars[n].z[0]

    RotatePoint(&x0,&y0,stars[n].rotation)
    x0 +:= windW/2.000000
    y0 +:= windH/2.000000
    if x0>=0.000000 and x0<windW and y0>=0.000000 and y0<windH then
      if stars[n].typex=STREAK then
            x1 := stars[n].x[1]*windW/stars[n].z[1]
            y1 := stars[n].y[1]*windH/stars[n].z[1]
            RotatePoint(&x1,&y1,stars[n].rotation)
            x1 +:= windW/2.000000
            y1 +:= windH/2.000000
            glLineWidth(MAXPOS/100.000000/stars[n].z[0]+1.000000)
            glColor3f(1.000000,MAXWARP-speed/MAXWARP,MAXWARP-speed/MAXWARP)
            if abs(x0-x1)<1.000000 and abs(y0-y1)<1.000000 then
                glBegin(GL_POINTS)
                glVertex2f(x0,y0)
                glEnd()
            else
                glBegin(GL_LINES)
                glVertex2f(x0,y0)
                glVertex2f(x1,y1)
                glEnd()
            fi
        else
            width := MAXPOS/10.000000/stars[n].z[0]+1.000000
            glColor3f(1.000000,0.000000,0.000000)
            glBegin(GL_POLYGON)
            i := 0

            while i<8 do
                x := x0+width*Cosx(real32(i)*MAXANGLES/8.000000)
                y := y0+width*Sinx(real32(i)*MAXANGLES/8.000000)
                glVertex2f(x,y)
                i++
            od
            glEnd()
        fi
    fi
end

global proc updatestars() =
    int32 n

    glClear(GL_COLOR_BUFFER_BIT)
    n := 0
    while n<starCount do
        if stars[n].z[0]>speed or stars[n].z[0]>0.000000 and speed<MAXWARP then
            if StarPoint(n)=GL_FALSE then
                NewStar(n,MAXPOS)
            fi
        else
            NewStar(n,MAXPOS)
        fi
        n++
    od
end

global proc showstars() =
    int32 n

    glClear(GL_COLOR_BUFFER_BIT)
    n := 0
    while n<starCount do
        if stars[n].z[0]>speed or stars[n].z[0]>0.000000 and speed<MAXWARP then
            ShowStar(n)
        fi
        n++
    od
end

proc initx =
    real64 angle
    int32 n

    srand(word32(clock()))
    n := 0
    while n<MAXSTARS do
        NewStar(n,100)
        n++
    od
    angle := 0.000000
    n := 0
    while n<=MAXANGLES do
        sinTable[n] := sin(angle)
        angle +:= PI/(MAXANGLES/2.000000)
       n++
    od
    glClearColor(0.000000,0.000000,0.000000,0.000000)
    glDisable(GL_DITHER)
end

callback global proc reshape(int32 width, height) =
    windW := width
    windH := height
    glViewport(0,0,windW,windH)
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    gluOrtho2D(-0.500000,windW+0.500000,-0.500000,windH+0.500000)
    glMatrixMode(GL_MODELVIEW)
end

callback proc key(byte key, int32 x, y) =
    switch key
    when ' ' then
        flag := (flag=NORMAL|WEIRD|NORMAL)
    when 't' then
        nitro := 1
    when 27 then
        stop
    end switch
end

global proc idle() =
    MoveStars()
    UpdateStars()
    if nitro>0 then
        speed := real32(nitro/10)+1.000000
        if speed>MAXWARP then
            speed := MAXWARP
        fi
        if ++nitro>MAXWARP*10 then
            nitro := -nitro
        fi
    else
        if nitro<0 then
            nitro++
            speed := real32(-nitro/10)+1.000000
            if speed>MAXWARP then
                speed := MAXWARP
            fi
        fi
    fi
    glutPostRedisplay()
end

callback global proc display() =
    ShowStars()
    if doubleBuffer then
        glutSwapBuffers()
    else
        glFlush()
    fi
end

callback global proc visible(int32 state) =
    if state=GLUT_VISIBLE then
        glutIdleFunc(cast(Idle))
    else
        glutIdleFunc(nil)
    fi
end

proc args(int32 argc, ref[0:]ichar argv) =
    int32 i

    doubleBuffer := GL_TRUE
    i := 1
    while i<argc do
        if strcmp(argv[i],"-sb")=0 then
            doubleBuffer := GL_FALSE
        else
            if strcmp(argv[i],"-db")=0 then
                doubleBuffer := GL_TRUE
            fi
        fi
        i++
    od
end

global function mainx(int32 argc, ref ref byte argv)int32 =
    word32 typex

    glutInitWindowSize(windW,windH)
!    glutInit_ATEXIT_HACK(&argc,argv)
    glutInit(&argc,argv)
    Args(argc,cast(argv))
    typex := GLUT_RGB
    typex ior:= (doubleBuffer|GLUT_DOUBLE|GLUT_SINGLE)
    glutInitDisplayMode(typex)
!    glutCreateWindow_ATEXIT_HACK("Stars")
    glutCreateWindow("Stars")
    Initx()

    glutReshapeFunc(cast(Reshape))
    glutKeyboardFunc(cast(Key))
    glutVisibilityFunc(cast(Visible))
    glutDisplayFunc(cast(Display))
    glutMainLoop()
    return 0
    return 0
end

proc start =
    stop mainx(nsysparams,cast(&sysparams))
end

