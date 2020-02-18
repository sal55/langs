!Example B source file
!Decode jpeg file as input and optionally display in window and/or
!write a ppm file
!Run as: pcc jpeg filename

!Simple jpeg decoder for colour images
!Supports 2x2, 2x1, 1x1 sub-sampling
!some algorithms derived from the C code in the book:
!# 'Basic Algorithms' by Malcolm McLean

import sys
import files
import clib
import gxlib
import bmlib

const showbm=1
const fwriteppm=0

const jpegerror=10
var debug=0

record stream=
    var ptr
    var ptrend
    var ptrstart
    var data

    var currbyte
    var currbit         !usually 7..0, bit which has just been read
end

record huffnode=
    var child0, child1
    var symbol
    var suppbits
end

record jpegrec=
    var width,height        !pixel size
    var ncomponents         !1..4 components (eg r,g,b)
    var framebytes          !width*height*framebytes
    var qtable              ![1..4,1..64] quantisation table
    var comptype            ![1..4]int 1=Y, 2=Cb, 3=Cr, 4=I, 5=Q
    var vsample, hsample    ![1..4]int sampling rate
    var usedc               ![1..4]int dc huffman index
    var useac               ![1..4]int ac huffman index
    var useq                ![1..4]int qtable index
    var dctable             ![1..4]table
    var actable             ![1..4]table
    var dri
end

var hdr

proc start=

    files:=cmd_getparams()

    case files.len
    when 0 then
        println "Decode JPEG file and write out PPM file"
        println "Usage:"
        println "     pcc jpeg filename[.jpg]"
        stop
    when 1 then
        file:=addext(files[1],"jpg")
    else
        println "Too many files"
    esac

    t:=ticks()

    println "Loading:",file
    try
        p:=jpegtest(file)
        if not p then
            cpl "Couldn't load",file
            stop
        fi
    except jpegerror then
        cpl "JPEG data error"
        stop
    end try

    println "Image:",hdr.width,"x",hdr.height
    println

    t:=ticks()-t

    println "time:",t

    if fwriteppm then
        writeppm(file,p,hdr)
    fi

    if showbm then
        bm:=makebm(p,hdr.width,hdr.height)

        dimx:=hdr.width
        dimy:=hdr.height
        scale:=1.0

        while dimx>1800 or dimy>1000 do
            dimx := dimx%2
            dimy := dimy%2
            scale /:=2.0
        od

        w:=gxcreatewindow(dim:(dimx,dimy))
        gxcopy(w,bm,scalex:scale)

        eventloop()
    fi

end

function makebm(data,w,h)=

    q:=data

    bm:=bmcreate(24,w,h)

    nbytes:=w*h*3
    dest:=makeref(bm.pixelptr,byte)

    to h do
        memcpy(dest,q,w*3)
        dest:=dest+bm.linebytes
        q:=q+w*3
    od

    return bm
end

function writeppm(file,data,hdr)=
    width:=hdr.width
    height:=hdr.height

    f:=createfile(file:=changeext(file,"ppm"))
    if not f then return 0 fi
    println "Writing",file

    print @f,"P6",,chr(10)
    print @f,width,height,,chr(10)
    print @f,255,,chr(10)

    buffer:=data

    q:=buffer
    linebytes:=width*3

    to height do
        p:=q                    !convert to bgr
        to width do
            swap(p^,(p+2)^)
            p:=p+3
        od

        q:=q+linebytes
    od

    writebytes(f,buffer,hdr.framebytes)
    free(buffer)

    return 1
end

proc initdata=

    hdr:=new(jpegrec,0)

    qt:=new(list,64,0)
    hdr.qtable::=(qt,qt,qt,qt)

    hdr.vsample::=hdr.hsample::=(0,0,0,0)
    hdr.usedc::=hdr.useac::=hdr.useq::=(0,0,0,0)

    hdr.comptype:=(0,0,0,0)
    hdr.dctable::=((),(),(),())
    hdr.actable::=((),(),(),())
    hdr.dri:=0

end

proc showheader(hdr)=
    println
    println "***** JPEG HEADER: *****"
    println =hdr.width,=hdr.height
    println =hdr.ncomponents
    println =hdr.framebytes
    println =hdr.qtable
    println =hdr.vsample
    println =hdr.hsample
    println =hdr.usedc,=hdr.useac,=hdr.useq
    println =hdr.dctable
    println =hdr.actable
end

proc showtree(tree,level=0)=
    if tree=nil then return fi
    println "   "*level,tree.symbol,tree.suppbits
    showtree(tree.child0,level+1)
    showtree(tree.child1,level+1)
end

proc initbitstream(fs)=
    fs.currbyte:=0
    fs.currbit:=0           !force new byte read on next nextbit
end

function nextbit(fs)=
    if fs.currbit=0 then
        fs.currbyte:=nextdatabyte(fs)
        fs.currbit:=8               !pick up .[7] below
    fi

    return fs.currbyte.[--fs.currbit]
end

function nextdatabyte(fs)=
    do
        if (c:=nextbyte(fs))<>0xFF then
            return c
        fi
        repeat
            c:=nextbyte(fs)
        until c<>0xFF
        if c=0 then return 0xFF fi
    od
    return 0
end

function getstream(filename)=
    fs:=new(stream)
    fs.data:=readbinfile(filename)
    if fs.data=0 then
        return 0
    fi
    fs.ptr:=makeref(&fs.data,byte)
    fs.ptrend:=makeref(&fs.data[$],byte)+1
    fs.ptrstart:=fs.ptr
    return fs
end

function nextbyte(fs)=
    if fs.ptr<fs.ptrend then
        c:=fs.ptr^
        ++fs.ptr
        return c
    fi
    PRINTLN "NEXTBYTE?"
    raise jpegerror         !shouldn't just run into eof
    return -1
end

function readword(fs)=
    bb:=nextbyte(fs)
    return bb<<8 ior nextbyte(fs)
end

proc readapp(fs,n,pref)=
!just read ffEn
    length:=readword(fs)-2
    ptr:=fs.ptr
    fs.ptr:=ptr+length
end

proc read_dht(fs)=
# Read and compute the huffman tables
    length:=readword(fs)-2

    while length>0 do
        codeswithlength::=()
        symbol::=()
        t:=nextbyte(fs)
        tabno:=t iand 15
        tabtype:=(t>>4) iand 15

!Read how many symbols of each length
        tot:=0
        for i:=1 to 16 do
            codeswithlength append:=nextbyte(fs)
            tot+:=codeswithlength[i]
        od

        for i:=1 to tot do
            symbol[i]:=nextbyte(fs)
        od

        tree:=buildhufftree(codeswithlength,symbol)

        if tabtype=0 then
            hdr.dctable[tabno+1]::=tree
        else
            hdr.actable[tabno+1]::=tree
        fi

        length -:=(tot+16+1)
    od
end

function buildhufftree(codelength,symbols)=
    tot:=0
    forall x in codelength do
        tot+:=x
    od

    nodes:=()
    to tot*2-1 do
        nodes append:=new(huffnode,0)
    od

    codes::=buildcanonical(codelength)

    buildtreerec(nodes,codes[1..$],symbols[1..$],tot,0)
    return nodes[1]
end

proc buildtreerec(nodes,code,symbol, n, bitx,level=0)=
    if n=0 then
        return
    fi

    first:=nodes[1]

    if n=1 then
        first.child0:=0
        first.child1:=0
        first.symbol:=symbol[1]
        first.suppbits:=0
        k:=code[1].len-bitx
        if k>0 then
            first.suppbits:=k
            bitx+:=k
        fi
        return
    fi

    for i2:=1 to n do
        if code[i2].[bitx+1]='1' then
            i:=i2
            exit
        fi
        i:=i2
    od
    first.child0:=nodes[2]
    first.child1:=nodes[2*i-1]
    first.symbol:=-1
    first.suppbits:=0

    buildtreerec(nodes[2..$],code,symbol,i-1,bitx+1, level+1)
    buildtreerec(nodes[2*i-1..$],code[i..$],symbol[i..$],n-i+1,bitx+1,level+1)
end

function buildcanonical(codelengths)=
    n:=0
    forall x in codelengths do
        n+:=x
    od

    a:=new(list,n)

    j:=1
    code:=0
    length:=1
    for i:=1 to 16 do
        to codelengths[i] do
            a[j]:=rightstr(tostr(code,"b"),length,"0")
            ++j
            code++
        od
        code <<:=1
        ++length
    od
    return a
end

proc read_dqt(fs)=

    lq:=readword(fs)-2
    q_table::=()
    while lq>0 do
        table::=()
        tq:=nextbyte(fs)
        pq:=tq>>4
        tq iand:=15
        --lq

        if pq=0 then
            to 64 do
                table append:=nextbyte(fs)
            od
        else
            to 64 do
                table append:=readword(fs)
            od
        fi
        lq-:=64
        hdr.qtable[tq+1]::=table
    od
end

proc read_sof(fs)=
    readword(fs)
    precision:=nextbyte(fs)
    if precision<>8 then
        PRINTLN "PRECISION"
        raise jpegerror
    fi

    hdr.height:=readword(fs)
    hdr.width:=readword(fs)
    hdr.ncomponents:=nextbyte(fs)
    hdr.framebytes:=hdr.width*hdr.height*hdr.ncomponents

    for i:=1 to hdr.ncomponents do
        hdr.comptype[i]:=nextbyte(fs)
        sampling:=nextbyte(fs)
        hdr.vsample[i]:=sampling iand 15
        hdr.hsample[i]:=sampling>>4
        hdr.useq[i]:=nextbyte(fs)+1
    od

end

proc read_sos(fs)=

    length:=readword(fs)-2
    ns:=nextbyte(fs)
    if ns<>hdr.ncomponents then
        PRINTLN "NCOMPS<>NS"
        raise jpegerror
    fi

    for i:=0 to ns-1 do
        hdr.comptype[i+1]:=nextbyte(fs)
        t:=nextbyte(fs)
        hdr.usedc[i+1]:=t>>4+1
        hdr.useac[i+1]:=(t iand 15)+1
    od

    length -:=ns*2+1

    while length-- do
        nextbyte(fs)
    od
end

proc read_eoi(fs)=
    if nextbyte(fs)<>0xFF then
        PRINTLN "EOI FF"
        raise jpegerror
    fi
    repeat
        c:=nextbyte(fs)
    until c<>0xFF
    if c<>0xD9 then
        PRINTLN "EOI D9"
        raise jpegerror
    fi
end

proc readmarker(fs)=
    nextbyte(fs)
    nextbyte(fs)
    initbitstream(fs)
end

function jpegtest(file)=
!returns pointer to memory block, or 0 if file couldn't be opened.
!info about the image is in the hdr global
!read/data errors in the file generate a 'jpegerror' exception

!CPL 201,=VKTOMESSTABLE
    initdata()
    pimage:=0

    fs:=getstream(file)
    if not fs then
        return 0
    fi
    N:=0

!CPL 202,=VKTOMESSTABLE
    do
        c:=nextbyte(fs)
!CPL 203,=VKTOMESSTABLE
        pref:="  "
        if c=0xFF then      !marker
            c:=nextbyte(fs)
            case c
            when 0xD8 then
                PRINTLN pref,"FFD8 SOI"
            when 0xE0 then
                PRINTLN pref,"FFE0 APP0"
            when 0xE0..0xEF then
                readapp(fs,c-0xE0,pref)
            when 0xC0 then
                PRINTLN pref,"FFC0 SOF0 baseline"
            read_sof(fs)
            when 0xC2 then
                PRINTLN pref,"FFC2 SOF2 progressive"
            when 0xC4 then
                PRINTLN pref,"FFC4 DHT"
                read_dht(fs)
            when 0xDB then
                PRINTLN pref,"FFDB DQT"
                read_dqt(fs)
            when 0xDD then
                PRINTLN pref,"FFDD DRI RST interval"
                readword(fs)                !skip length
                hdr.dri:=readword(fs)
            when 0xEE then
                PRINTLN pref,"FFEE COM comment"
            when 0xDA then
                PRINTLN pref,"FFDA SOS"
                read_sos(fs)
                pimage:=loadscan(fs,hdr)
                exit
            when 0xD9 then
                PRINTLN pref,"FFD9 EOI"
            when 0x0 then
                PRINTLN pref,"FF00 <embedded FF data>"
            when 0xFF then
                PRINTLN pref,"FFFF padding"
            else
                PRINTLN pref,"FF",,c:"2zh","Unknown marker"
            esac
        else
        fi
!CPL 209,=VKTOMESSTABLE
    od
    return pimage
end

function loadscan(fs,hdr)=
!Read image data following sos, from filestream handle fs,
!and using image params in hdr
!return a memory pointer to the image data
    initbitstream(fs)

    (vsample1,vsample2,vsample3):=hdr.vsample
    (hsample1,hsample2,hsample3):=hdr.hsample
    (comptype1,comptype2,comptype3):=hdr.comptype

    pimage:=nil
    case hdr.ncomponents
    when 1 then
        abort("loadmono")
    when 3 then
        if comptype1<>1 or comptype2<>2 or comptype3<>3 then
            abort("comptype?")
        fi
        if hsample2=vsample2=hsample3=vsample3 and (hsample1<=2 and vsample1<=2) then
            pimage:=loadcolour(fs,hdr,hsample1,vsample1)
        else
            println hsample1,vsample1,hsample2,vsample2,hsample3,vsample3
            ABORT("LOAD COLOUR/Unknown sampling")
        fi
    else
        abort("ncomp")
    esac

    return pimage
end

function tree_getsymbol(fs,node)=

    while (node.child0) do
        if nextbit(fs) then
            node := node.child1
        else
          node := node.child0
        fi
    od

    to node.suppbits do
        nextbit(fs)
    od

    return node.symbol
end

function getsymbol(fs,nbits)=
    if nbits = 0 then
        return 0
    fi

    answer := 0
    to nbits do
        answer <<:= 1
        answer ior:= nextbit(fs)
    od
    if answer iand (1 << (nbits-1)) = 0 then
        answer -:= (1 << nbits ) -1 
    fi 

    return answer
end

!proc unzigzag(&block)=
proc unzigzag(block)=
    var zigzag=(
    1,2,6,7,15,16,28,29,
    3,5,8,14,17,27,30,43,
    4,9,13,18,26,31,42,44,
    10,12,19,25,32,41,45,54,
    11,20,24,33,40,46,53,55,
    21,23,34,39,47,52,56,61,
    22,35,38,48,51,57,60,62,
    36,37,49,50,58,59,63,64)

    temp::=block
    for i:=1 to 64 do
        block[i]:=temp[zigzag[i]]
    od
end

proc idct8x8(block)=

    for i:=0 to 7 do
        j:=i*8+1
        fastidct8(
            block[j],
            block[j+1],
            block[j+2],
            block[j+3],
            block[j+4],
            block[j+5],
            block[j+6],
            block[j+7])
    od
    for i:=1 to 64 do
        block[i] >>:= 3
    od

    for i:=1 to 8 do
        fastidct8(
            block[i],
            block[i+8],
            block[i+16],
            block[i+24],
            block[i+32],
            block[i+40],
            block[i+48],
            block[i+56])
    od
end

proc fastidct8(&a1,&a2,&a3,&a4,&a5,&a6,&a7,&a8) =       ! FASTIDCT8X8
    const w1 = 2841
    const w2 = 2676
    const w3 = 2408
    const w5 = 1609
    const w6 = 1108
    const w7 = 565

    if (not ((x2 := a5 << 11) ior \
            (x3 := a7) ior \
            (x4 := a3) ior \
            (x5 := a2) ior \
            (x6 := a8) ior \
            (x7 := a6) ior \
            (x8 := a4))) then
        a1:=a2:=a3:=a4:=a5:=a6:=a7:=a8:=a1<<3
        return
    fi
    x1 := (a1 << 11) + 128

!first stage
    x9 := W7 * (x5 + x6)
    x5 := x9 + (W1 - W7) * x5
    x6 := x9 - (W1 + W7) * x6
    x9 := W3 * (x7 + x8)
    x7 := x9 - (W3 - W5) * x7
    x8 := x9 - (W3 + W5) * x8

!second stage
    x9 := x1 + x2
    x1 -:= x2
    x2 := W6 * (x4 + x3)
    x3 := x2 - (W2 + W6) * x3
    x4 := x2 + (W2 - W6) * x4
    x2 := x5 + x7
    x5 -:= x7
    x7 := x6 + x8
    x6 -:= x8

!third stage
    x8 := x9 + x4
    x9 -:= x4
    x4 := x1 + x3
    x1 -:= x3
    x3 := (181 * (x5 + x6) + 128) >> 8
    x5 := (181 * (x5 - x6) + 128) >> 8

!fourth stage
    a1 := (x8 + x2) >> 8
    a2 := (x4 + x3) >> 8
    a3 := (x1 + x5) >> 8
    a4 := (x9 + x7) >> 8
    a5 := (x9 - x7) >> 8
    a6 := (x1 - x5) >> 8
    a7 := (x4 - x3) >> 8
    a8 := (x8 - x2) >> 8
end

proc getblock(fs,dctree,actree,block)=

    nbits := tree_getsymbol(fs,dctree)
    nread:=0

    ++nread
    block[nread] := getsymbol(fs, nbits)

    repeat
        bb := tree_getsymbol(fs,actree)

        if bb = 0xF0 then
            if (nread > 48) then
                PRINTLN "GETB1"
                raise jpegerror
            fi
            to 16 do
                ++nread
                block[nread] := 0
            od
        fi
        zeroes := bb >> 4
        nbits := bb iand 15

        if nbits then
            if (nread + zeroes > 63) then
                raise jpegerror
            fi
            to zeroes do
                ++nread
                block[nread] := 0
            od
            ++nread
            block[nread] := getsymbol(fs, nbits)
            if (nread = 64) then
                return
            fi
        fi
    until not bb

    while nread<64 do
        ++nread
        block[nread] := 0
    od

end

proc readblock(fs,block, dctable,actable,qtable, &dc)=
    var zigzag=(
    1,2,6,7,15,16,28,29,
    3,5,8,14,17,27,30,43,
    4,9,13,18,26,31,42,44,
    10,12,19,25,32,41,45,54,
    11,20,24,33,40,46,53,55,
    21,23,34,39,47,52,56,61,
    22,35,38,48,51,57,60,62,
    36,37,49,50,58,59,63,64)
    static var temp=(0,)*64

    getblock(fs,dctable, actable,temp)

    temp[1]:=temp[1]+dc
    dc:=temp[1]

    for k:=1 to 64 do
        block[k]:=temp[zigzag[k]]*qtable[k]
    od
    idct8x8(block)
end

function loadcolour(fs,hdr,hoz,vert)=
!read yuv colour image data
!hoz/vert will be:
! 2 2     2x2 lum sampling compared with chroma
! 2 1     2x1 lum sampling
! 1 1     1x1 lum sampling
!return pointer to memory block containing image, or nil
!any data errors, corrupted file etc result in exception 'jpegerror'

    println "Read Colour:",hoz,"x",vert

    data:=malloc(hdr.framebytes)
    if data=nil then
        PRINTLN "MALLOC"
        raise jpegerror
    fi
    data:=makeref(data,byte)

    diffdc:=dcb:=dcr:=0

    nlum:=hoz*vert                          !number of lum blocks compared with cb or cr
    block:=(0,)*64
    lum::=(block,block,block,block)         !will use first nlum of these
    cr::=cb::=block

    dctable_lum := hdr.dctable[hdr.usedc[1]]
    actable_lum := hdr.actable[hdr.useac[1]]
    qtable_lum  := hdr.qtable[hdr.useq[1]]

    dctable_cb  := hdr.dctable[hdr.usedc[2]]
    actable_cb  := hdr.actable[hdr.useac[2]]
    qtable_cb   ::= hdr.qtable[hdr.useq[2]]

    dctable_cr  := hdr.dctable[hdr.usedc[3]]
    actable_cr  := hdr.actable[hdr.useac[3]]
    qtable_cr   ::= hdr.qtable[hdr.useq[3]]

    unzigzag(qtable_lum)
    unzigzag(qtable_cb)     !cb/cr might be the same table; use ::= above if doing this
    unzigzag(qtable_cr)

    count:=0
    y:=0

    while y<hdr.height do
        x:=0
        while x<hdr.width do
            if hdr.dri and (count rem hdr.dri)=0 and count>0 then
                readmarker(fs)
                diffdc:=0
                dcb:=dcr:=0
            fi

            for j:=1 to nlum do
                readblock(fs,lum[j],dctable_lum, actable_lum, qtable_lum, diffdc)
            od

            readblock(fs,cb,dctable_cb, actable_cb, qtable_cb, dcb)

            readblock(fs,cr,dctable_cr, actable_cr, qtable_cr, dcr)

            reconsblockcolour(lum[1],lum[2],lum[3],lum[4],cr,cb,data,x,y, hoz,vert)
            ++count
            x+:=hoz*8
        od
        y+:=vert*8
    od

    read_eoi(fs)
    return data
end

proc reconsblockcolour(lum1,lum2,lum3,lum4,cr,cb, data,x,y, hoz,vert)=

    width:=hdr.width
    height:=hdr.height

    ilim:=vert*8-1
    jlim:=hoz*8-1

    for i:=0 to ilim do
        if (yy:=y+i)>=height then exit fi
        p:=data+(yy*width+x)*3

        ii:=i%vert*8+1
        if i<8 then
            less8:=1
            ioffset:=i*8+1
        else
            less8:=0
            ioffset:=(i-8)*8+1
        fi

        for j:=0 to jlim do
            if x+j>=width then exit fi
            if less8 then
                if j<8 then
                    luminance := lum1[ioffset+j]%64+128
                else
                    luminance := lum2[ioffset+j-8]%64+128
                fi
            else
                if j<8 then
                    luminance := lum3[ioffset+j]%64+128
                else
                    luminance := lum4[ioffset+j-8]%64+128
                fi
            fi

            rr := cr[ii+j%2]
            bb := cb[ii+j%2]

            p++^ := clamp((bb*57%2048)+luminance, 0,255)                !blue
            p++^ := clamp(luminance - ((bb*11) + rr*23)%2048, 0,255)    !green
            p++^ := clamp(rr*45%2048+luminance, 0,255)                  !red
        od
    od
end
