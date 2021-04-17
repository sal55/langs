import files

const N=1024

proc start=
    f := createfile("mandel.pbm")

    println @f,"P4"
    println @f,N,N

    m := 2/N
    ba := 2<<(N rem 8+1)-1
    bb := 2<<(8-N rem 8)
    buf := new(array,byte)

    for y := 0 to N-1 do
        ci := y*m-1
        b := 1
        p := 0
        for x := 0 to N-1 do
            cr := x*m-1.5
            zr := cr
            zi := ci
            zrq := sqr cr
            ziq := sqr ci
            b+ := b

            to 49 do
                zi := zr*zi*2+ci
                zr := zrq-ziq+cr
                ziq := sqr zi
                zrq := sqr zr
                if zrq+ziq>4 then
                    ++b
                    exit
                end
            end

            if b>=256 then
                ++p
                buf[p] := 511-b
                b := 1
            end

        end

        if b<>1 then
            ++p
            buf[p] := (ba-b)*bb
        end

        writebytes(f,&buf,buf.len)
    end

    closefile(f)
end
