import clib

const imwidth  = 3072
const imheight = 2048

[0:imheight,0:imwidth]byte image

function mandel(real x,y, int max_iters)int=
    real a,b,a2
    a:=b:=0

    for i to max_iters do
        a2:=sqr(a)-sqr(b)+x
        b:=a*b*2.0+y
        a:=a2
        if sqr(a)+sqr(b)>=4.0 then
            return i-1
        fi
    od
    return max_iters
end

proc create_fractal(real min_x, max_x, min_y, max_y, int iters)=
    real pixel_size_x, pixel_size_y
    real i,r

    pixel_size_x := (max_x - min_x) / imwidth
    pixel_size_y := (max_y - min_y) / imheight
    i:=min_y

    for y:=0 to imheight-1 do
        i+:=pixel_size_y
        r:=min_x
        for x:=0 to imwidth-1 do
            r+:=pixel_size_x
            image[y,x]:=mandel(r,i,iters)
        od
    od
end

proc writepgm(ichar file,int maxpixel=255)=
    filehandle f := fopen(file,"w")
    
    println @f,"P2"
    println @f, imwidth, imheight
    println @f, maxpixel

    for y:=0 to imheight-1 do
        for x:=0 to imwidth-1 do
            print @f,image[y,x],," "
        od
        println @f
    od
    fclose(f)
end

proc start=
    const maxpixel=255
    int sum:=0

    create_fractal(-2.0, 1.0, -1.0, 1.0, maxpixel) 
    for y:=0 to imheight-1 do
        for x:=0 to imwidth-1 do
            sum+:=image[y,x]
        od
    od
    println sum

!Uncomment following line to create an image file
!   writepgm("test.pgm",maxpixel)
end
