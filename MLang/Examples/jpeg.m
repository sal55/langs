!simple jpeg decoder for colour images
!supports 2x2, 2x1, 1x1 sub-sampling
!Some algorithms derived from the C code in the book:
! 'Basic Algorithms' by Malcolm McLean

record stream =
    ref byte ptr
    ref byte ptrend
    ref byte ptrstart
    ref byte data

    int currbyte
    int currbit         !usually 0x80,0x40..0, bit which has just been read
end

record huffnode =
    ref huffnode child0, child1
    int symbol
    int suppbits
end

record jpeginforec =
    int width,height        !pixel size
    int ncomponents         !1..4 components (eg r,g,b)
    int framebytes          !width*height*framebytes
    [4,64]int qtable        ![1..4,1..64] quantisation table
    [4]int comptype         ![1..4]int 1=Y, 2=Cb, 3=Cr, 4=I, 5=Q
    [4]int vsample, hsample ![1..4]int sampling rate
    [4]int usedc            ![1..4]int dc huffman index
    [4]int useac            ![1..4]int ac huffman index
    [4]int useq             ![1..4]int qtable index
    [4]ref huffnode dctable ![1..4]table
    [4]ref huffnode actable ![1..4]table
    int dri
end

jpeginforec hdr

proc main=
    ref byte p
    ichar file
    int64 width,height

    file:=f"\jpeg\card2.jpg"

    println =file

    p:=loadjpeg(file,width,height)

    if p=nil then
        println "Couldn't load",file
        stop
    fi

    println =ref void(p)
    println =width,=height
    to 100 do
        print p^,$
        ++p
    od
    println
end

export function loadjpeg(ichar file, int64 &width, &height)ref byte =
!Load jpeg file into a memory buffer
!return a pointer to the image
!width/height info is returned via references in the parameter list
    ref byte p

    width:=height:=-1
    p:=nil

    p:=loadjpegfile(file)
    width:=hdr.width
    height:=hdr.height

    return p
end

export proc freejpeg(ref byte p)=
    free(p)
end

proc initdata=
    pcm_init()
    memset(&hdr,0,jpeginforec.bytes)
end

proc showtree(ref huffnode tree,int level=0)=
    static int seq=0

    if tree=nil then return fi
    print ++seq
    to level do
        print "    "
    od
    println tree.symbol,tree.suppbits
    showtree(tree.child0,level+1)
    showtree(tree.child1,level+1)
end

proc initbitstream(ref stream fs)=
    fs.currbyte:=0
    fs.currbit:=1          !force new byte read on next nextbit
end

function nextbit(ref stream fs)int=

    if fs.currbit=1 then
        fs.currbyte:=nextdatabyte(fs)
        fs.currbit:=0x100              !pick up .[7] below
    fi

    fs.currbit>>:=1
    return (fs.currbyte iand fs.currbit|1|0)
end

function nextdatabyte(ref stream fs)int=
    int c
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

function getstream(ichar filename)ref stream=
    ref stream fs
    fs:=jalloc(stream.bytes)
    fs.data:=readfile(filename)
    if fs.data=nil then
        println "JPEGDLL:Can't open",filename
        return nil
    fi
    fs.ptr:=fs.data
    fs.ptrend:=fs.ptr+rfsize
    fs.ptrstart:=fs.ptr
    return fs
end

function nextbyte(ref stream fs)int=
    int c

    if fs.ptr<fs.ptrend then
        c:=fs.ptr^
        ++fs.ptr
        return c
    fi
    abortjpeg("nextbyte")
    return -1
end

function readword(ref stream fs)int=
    int bb

    bb:=nextbyte(fs)
    return bb<<8 ior nextbyte(fs)
end

proc readapp(ref stream fs,int n)=
!just read ffEn
    ref byte ptr
    int length
    length:=readword(fs)-2
    ptr:=fs.ptr
    fs.ptr:=ptr+length
end

proc read_dht(ref stream fs)=
! Read and compute the huffman tables
    int length,tot,t,tabno,tabtype,i
    [16]int codeswithlength
    [256]int symbol
    ref huffnode tree

    length:=readword(fs)-2

    while length>0 do
        t:=nextbyte(fs)
        tabno:=t iand 15
        tabtype:=(t>>4) iand 15

!Read how many symbols of each length
        tot:=0
        for i:=1 to 16 do
            codeswithlength[i] :=nextbyte(fs)
            tot+:=codeswithlength[i]
        od

        for i:=1 to tot do
            symbol[i]:=nextbyte(fs)
        od

        tree:=buildhufftree(cast(&codeswithlength),cast(&symbol))

        if tabtype=0 then
            hdr.dctable[tabno+1]:=tree
        else
            hdr.actable[tabno+1]:=tree
        fi

        length -:=(tot+16+1)
    od
end

function buildhufftree(ref[]int codelength,symbols)ref huffnode=
    ref[]huffnode nodes
    ref[]ichar codes
    int tot,i

    tot:=0
    for i:=1 to 16 do
        tot+:=codelength^[i]
    od

    nodes:=jallocz(huffnode.bytes*(tot*2-1))

    codes:=buildcanonical(codelength)

    buildtreerec(cast(nodes),cast(&codes^[1]),cast(&symbols^[1]),tot,0)
    return cast(nodes)
end

proc buildtreerec(ref[]huffnode nodes,ref[]ref[]char code,ref[]int symbol, int n, bitx,level=0)=
    ref huffnode first
    int i,i2,k

    if n=0 then
        return
    fi

    first:=&nodes^[1]

    if n=1 then

        first.child0:=nil
        first.child1:=nil
        first.symbol:=symbol^[1]
        first.suppbits:=0
        k:=strlen(cast(code^[1]))-bitx
        if k>0 then
            first.suppbits:=k
            bitx+:=k
        fi
        return
    fi

    for i2:=1 to n do
        if code^[i2]^[bitx+1]='1' then
            i:=i2
            exit
        fi
        i:=i2
    od
    first.child0:=&nodes^[2]
    first.child1:=&nodes^[2*i-1]
    first.symbol:=-1
    first.suppbits:=0

    buildtreerec(cast(&nodes^[2]),code,symbol,i-1,bitx+1, level+1)
    buildtreerec(cast(&nodes^[2*i-1]),cast(&code^[i]),cast(&symbol^[i]),n-i+1,bitx+1,level+1)
end

function buildcanonical(ref[]int codelengths)ref[]ichar=
    int i,j,n,code,length
    ref[]ichar a

    n:=0
    for i:=1 to 16 do
        n+:=codelengths^[i]
    od

    a:=jallocz(n*ichar.bytes)

    j:=1
    code:=0
    length:=1
    for i:=1 to 16 do
        to codelengths^[i] do
            a^[j]:=pcm_copyheapstring(tostrbin(code,length))
            ++j
            code++
        od
        code <<:=1
        ++length
    od

    return a
end

function tostrbin(int a,length)ichar=
!convert a to a binary string, right-justified in a field of n characters, with
!leading zeros
!return pointer to static buffer containing result
    static [65]char result
    int i

    result[length+1]:=0
    for i:=length downto 1 do
        result[i]:='0'+(a iand 1)
        a >>:=1
    od
    return &.result
end

proc read_dqt(ref stream fs)=
    int lq,i,pq,tq

    lq:=readword(fs)-2
    while lq>0 do
        tq:=nextbyte(fs)
        pq:=tq>>4
        tq iand:=15
        ++tq            !make one-based
        --lq

        if pq=0 then
            for i:=1 to 64 do
                hdr.qtable[tq,i]:=nextbyte(fs)
            od
        else
            for i:=1 to 64 do
                hdr.qtable[tq,i]:=readword(fs)
            od
        fi
        lq-:=64
    od
end

proc read_sof(ref stream fs)=
    int precision,i,sampling

    readword(fs)
    precision:=nextbyte(fs)
    if precision<>8 then
        abortjpeg("PRECISION")
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

proc read_sos(ref stream fs)=
    int length,ns,i,t

    length:=readword(fs)-2
    ns:=nextbyte(fs)
    if ns<>hdr.ncomponents then
        abortjpeg("NCOMPS<>NS")
    fi

    for i:=1 to ns do
        hdr.comptype[i]:=nextbyte(fs)
        t:=nextbyte(fs)
        hdr.usedc[i]:=t>>4+1
        hdr.useac[i]:=(t iand 15)+1
    od

    length -:=ns*2+1

    while length-- do
        nextbyte(fs)
    od
end

proc read_eoi(ref stream fs)=
    int c

    if nextbyte(fs)<>0xFF then
        abortjpeg("EOI FF")
    fi

    repeat
        c:=nextbyte(fs)
    until c<>0xFF

    if c<>0xD9 then
        abortjpeg("EOI D9")
    fi
end

proc readmarker(ref stream fs)=
    nextbyte(fs)
    nextbyte(fs)
    initbitstream(fs)
end

function loadjpegfile(ichar file)ref byte=
!returns pointer to memory block, or 0 if file couldn't be opened.
!info about the image is in the hdr global
!read/data errors in the file generate a 'jpegerror' exception
    ref stream fs
    ref byte pimage
    int c,offset

    initdata()
    pimage:=nil

    fs:=getstream(file)
    if not fs then
        return nil
    fi

    do
        c:=nextbyte(fs)
        if c=0xFF then      !marker
            c:=nextbyte(fs)
            switch c
            when 0xD8 then
!           println offset,"FFD8 SOI"
            when 0xE0 then
!           println offset,"FFE0 APP0"
            when 0xE1..0xED,0xEF then
!           println offset,"FFEx APPx"
                readapp(fs,c-0xE0)
            when 0xC0 then
!           println offset,"FFC0 SOF0 baseline"
                read_sof(fs)
            when 0xC2 then
!           println offset,"FFC2 SOF2 progressive"
            when 0xC4 then
!           println offset,"FFC4 DHT"
                read_dht(fs)
            when 0xDB then
!           println offset,"FFDB DQT"
                read_dqt(fs)
            when 0xDD then
!           println offset,"FFDD DRI RST interval"
                readword(fs)                !skip length
                hdr.dri:=readword(fs)
            when 0xEE then
!           println offset,"FFEE COM comment"
            when 0xDA then
!           println offset,"FFDA SOS"
                read_sos(fs)
                pimage:=loadscan(fs)
                exit
            when 0xD9 then
!           println offset,"FFD9 EOI"
            when 0x0 then
!           println offset,"FF00 <embedded FF data>"
            when 0xFF then
!           println offset,"FFFF padding"
            else
!           println offset,"FF",,c:"2zh","Unknown marker"
            end
!   else
        fi
    od
    return pimage
end

function loadscan(ref stream fs)ref byte=
!Read image data following sos, from filestream handle fs,
!and using image params in hdr
!return a memory pointer to the image data
    ref byte pimage

    initbitstream(fs)

    pimage:=nil
    case hdr.ncomponents
    when 1 then
        abortjpeg("loadmono")
    when 3 then
        if hdr.comptype[1]<>1 or hdr.comptype[2]<>2 or hdr.comptype[3]<>3 then
            abortjpeg("comptype?")
        fi
        if hdr.hsample[2]=hdr.vsample[2]=hdr.hsample[3]=hdr.vsample[3] and \
            (hdr.hsample[1]<=2 and hdr.vsample[1]<=2) then
            pimage:=loadcolour(fs,hdr.hsample[1],hdr.vsample[1])
        else
            println hdr.hsample[1],hdr.vsample[1],hdr.hsample[2],hdr.vsample[2],hdr.hsample[3],hdr.vsample[3]
            abortjpeg("LOAD COLOUR/Unknown sampling")
        fi
    else
        abortjpeg("ncomp")
    esac

    return pimage
end

function tree_getsymbol(ref stream fs,ref huffnode node)int=

    while node.child0 do
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

function getsymbol(ref stream fs,int nbits)int=
    int a,b

    if nbits = 0 then
        return 0
    fi

    a := 0
    to nbits do
        a <<:= 1
        b:=nextbit(fs)
        a ior:= b
    od
    if a iand (1 << (nbits-1)) = 0 then
        a -:= (1 << nbits ) -1 
    fi 

    return a
end

proc unzigzag([64]int &block)=
    static []int zigzag=(
    1,2,6,7,15,16,28,29,
    3,5,8,14,17,27,30,43,
    4,9,13,18,26,31,42,44,
    10,12,19,25,32,41,45,54,
    11,20,24,33,40,46,53,55,
    21,23,34,39,47,52,56,61,
    22,35,38,48,51,57,60,62,
    36,37,49,50,58,59,63,64)
    [64]int temp

    temp:=block

    for i:=1 to 64 do
        block[i]:=temp[zigzag[i]]
    od
end

proc idct8x8([64]int block)=
    int j

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

proc fastidct8(int &a1,&a2,&a3,&a4,&a5,&a6,&a7,&a8) =       ! FASTIDCT8X8
    const w1 = 2841
    const w2 = 2676
    const w3 = 2408
    const w5 = 1609
    const w6 = 1108
    const w7 = 565
    int x1,x2,x3,x4,x5,x6,x7,x8,x9

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

    x9 := w7 * (x5 + x6)
    x5 := x9 + (w1 - w7) * x5
    x6 := x9 - (w1 + w7) * x6
    x9 := w3 * (x7 + x8)
    x7 := x9 - (w3 - w5) * x7
    x8 := x9 - (w3 + w5) * x8

    x9 := x1 + x2
    x1 -:= x2
    x2 := w6 * (x4 + x3)
    x3 := x2 - (w2 + w6) * x3
    x4 := x2 + (w2 - w6) * x4
    x2 := x5 + x7
    x5 -:= x7
    x7 := x6 + x8
    x6 -:= x8

    x8 := x9 + x4
    x9 -:= x4
    x4 := x1 + x3
    x1 -:= x3
    x3 := (181 * (x5 + x6) + 128) >> 8
    x5 := (181 * (x5 - x6) + 128) >> 8

    a1 := (x8 + x2) >> 8
    a2 := (x4 + x3) >> 8
    a3 := (x1 + x5) >> 8
    a4 := (x9 + x7) >> 8
    a5 := (x9 - x7) >> 8
    a6 := (x1 - x5) >> 8
    a7 := (x4 - x3) >> 8
    a8 := (x8 - x2) >> 8
end

proc getblock(ref stream fs,ref huffnode dctree,actree,[64]int block)=
    int nbits,nread,bb,zeroes

    nbits := tree_getsymbol(fs,dctree)
    nread:=0

    ++nread
    memset(&block,0,64*int.bytes)

    block[nread] := getsymbol(fs, nbits)

    repeat
        bb := tree_getsymbol(fs,actree)

        if bb = 0xF0 then
!       if (nread + 16 > 64) then
            if (nread > 48) then
                abortjpeg("GETB1")
            fi
            nread+:=16
        fi

        zeroes := bb >> 4
        nbits := bb iand 15

        if nbits then
            if (nread + zeroes > 63) then
                abortjpeg("nzeroes")
            fi
            nread+:=zeroes
            ++nread
            block[nread] := getsymbol(fs, nbits)
            if (nread = 64) then
                return
            fi
        fi
    until not bb
end

proc readblock(ref stream fs,[64]int &block, ref huffnode dctable,actable,
            ref[]int qtable, ref int dc)=
    int u

    getblock(fs,dctable, actable,block)
    block[1]:=block[1]+dc^

    dc^:=block[1]
    u:=hdr.useq[2]

    for k:=1 to 64 do
        block[k]*:=qtable^[k]
    od

    unzigzag(block)
    idct8x8(block)
end

function loadcolour(ref stream fs,int hoz,vert)ref byte=
!read yuv colour image data
!hoz/vert will be:
! 2 2     2x2 lum sampling compared with chroma
! 2 1     2x1 lum sampling compared with chroma
! 1 1     1x1 lum sampling compared with chroma
!return pointer to memory block containing image, or nil
    int x,y,nlum,u,k,count,i,j
    int diffdc,dcr,dcb
    ref byte data
    ref huffnode dctable_lum, actable_lum
    ref huffnode dctable_cb, actable_cb
    ref huffnode dctable_cr, actable_cr
    ref[]int qtable_lum, qtable_cb,qtable_cr

    [64]int cr,cb
    [4,64]int lum

    data:=jallocz(hdr.framebytes)

    diffdc:=dcb:=dcr:=0
    nlum:=hoz*vert                          !number of lum blocks compared with cb or cr

    dctable_lum := hdr.dctable[hdr.usedc[1]]
    actable_lum := hdr.actable[hdr.useac[1]]
    qtable_lum  := &hdr.qtable[hdr.useq[1]]

    dctable_cb  := hdr.dctable[hdr.usedc[2]]
    actable_cb  := hdr.actable[hdr.useac[2]]
    qtable_cb   := &hdr.qtable[hdr.useq[2]]

    dctable_cr  := hdr.dctable[hdr.usedc[3]]
    actable_cr  := hdr.actable[hdr.useac[3]]
    qtable_cr   := &hdr.qtable[hdr.useq[3]]

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
                readblock(fs,lum[j],dctable_lum, actable_lum, qtable_lum, &diffdc)
            od

            readblock(fs,cb,dctable_cb, actable_cb, qtable_cb, &dcb)
            readblock(fs,cr,dctable_cr, actable_cr, qtable_cr, &dcr)

            reconsblockcolour(&lum[1],&lum[2],&lum[3],&lum[4],&cr,&cb,data,x,y, hoz,vert)
            ++count
            x+:=hoz*8
        od
        y+:=vert*8
    od

    read_eoi(fs)
    return data
end

proc reconsblockcolour(ref[]int lum1,lum2,lum3,lum4,cr,cb, ref byte data,int x,y, hoz,vert)=
    int width,height,ilim,jlim,i,j,yy,ix,rr,bb,luminance
    ref byte p
    int red,blue,green
    int a,b

    width:=hdr.width
    height:=hdr.height

    ilim:=vert*8-1
    jlim:=hoz*8-1

    for i:=0 to ilim do
        if (yy:=y+i)>=height then next fi
        p:=data+(yy*width+x)*3

        for j:=0 to jlim do
            if x+j>=width then next fi
            if i<8 then
                if j<8 then
                    luminance := lum1^[i*8+j+1]/64+128
                else
                    luminance := lum2^[i*8+j-8+1]/64+128
                fi
            else
                if j<8 then
                    luminance := lum3^[(i-8)*8+j+1]/64+128
                else
                    luminance := lum4^[(i-8)*8+j-8+1]/64+128
                fi
            fi

            ix:=i/vert*8+j>>1+1
            rr := cr^[ix]
            bb := cb^[ix]
            p^ := clamp((bb*57/2048)+luminance, 0,255)              !blue
            ++p
            p^ := clamp(luminance - (bb*11 + rr*23)/2048, 0,255)    !green
            ++p
            p^ := clamp(rr*45/2048+luminance, 0,255)                    !red
            ++p
        od
    od
end

function jalloc(int n)ref void=
    ref void p

    p:=malloc(n)
    if p=nil then
        println "Jpeg malloc fails"
        stop
    fi
    return p
end

function jallocz(int n)ref void=
    ref void p
    p:=jalloc(n)
    memset(p,0,n)
    return p
end

proc abortjpeg(ichar mess)=
    println "Jpeg error:",mess
    stop
end
