! C to M Converter

!import clib

importdll dummy =
end

const NJ_USE_LIBC = 1
const NJ_USE_WIN32 = 0
const NJ_CHROMA_FILTER = 1
const W1 = 2841
const W2 = 2676
const W3 = 2408
const W5 = 1609
const W6 = 1108
const W7 = 565

enum (NJ_OK = 0)
enum (NJ_NO_JPEG = 0+1)
enum (NJ_UNSUPPORTED = 0+2)
enum (NJ_OUT_OF_MEM = 0+3)
enum (NJ_INTERNAL_ERR = 0+4)
enum (NJ_SYNTAX_ERROR = 0+5)
enum (__NJ_FINISHED = 0+6)
record _nj_code =
    byte bits
    byte code
end

record _nj_cmp =
    int32 cid
    int32 ssx
    int32 ssy
    int32 width
    int32 height
    int32 stride
    int32 qtsel
    int32 actabsel
    int32 dctabsel
    int32 dcpred
    ref byte pixels
end

record _nj_ctx =
    int32 error
    ref byte pos
    int32 size
    int32 length
    int32 width
    int32 height
    int32 mbwidth
    int32 mbheight
    int32 mbsizex
    int32 mbsizey
    int32 ncomp
    [3]nj_component_t comp
    int32 qtused
    int32 qtavail
    [4][64]byte qtab
    [4][65536]nj_vlc_code_t vlctab
    int32 buf
    int32 bufbits
    [64]int32 block
    int32 rstinterval
    ref byte rgb
end

nj_context_t nj
[64]byte njZZ = (0,1,8,16,9,2,3,10,17,24,32,25,18,11,4,5,12,19,26,33,40,48,41,34,27,20,13,6,7,14,21,28,35,42,49,56,57,50,43,36,29,22,15,23,30,37,44,51,58,59,52,45,38,31,39,46,53,60,61,54,47,55,62,63)

global proc njInit() =
    memset(&nj,0,88)
end

global function njDecode(ref void jpeg, int32 size)int32 =
    njDone()
    nj.pos := ref byte(jpeg)
    nj.size := size iand 2147483647
    if nj.size<2 then
        return NJ_NO_JPEG
    fi
    if nj.pos[0] ixor 255 ior nj.pos[1] ixor 216 then
        return NJ_NO_JPEG
    fi
    njSkip(2)
    while not nj.error do
        if nj.size<2 or nj.pos[0]<>255 then
            return NJ_SYNTAX_ERROR
        fi
        njSkip(2)
        switch nj.pos[-1]
        when 192 then
            njDecodeSOF()
        when 196 then
            njDecodeDHT()
        when 219 then
            njDecodeDQT()
        when 221 then
            njDecodeDRI()
        when 218 then
            njDecodeScan()
        when 254 then
            njSkipMarker()
        else
            if nj.pos[-1] iand 240=224 then
                njSkipMarker()
            else
                return NJ_UNSUPPORTED
            fi
        end switch
    od
    if nj.error<>__NJ_FINISHED then
        return nj.error
    fi
    nj.error := NJ_OK
    njConvert()
    return nj.error
end

global function njGetWidth()int32 =
    return nj.width
end

global function njGetHeight()int32 =
    return nj.height
end

global function njIsColor()int32 =
    return nj.ncomp<>1
end

global function njGetImage()ref byte =
    return (nj.ncomp=1|nj.comp[0].pixels|nj.rgb)
end

global function njGetImageSize()int32 =
    return nj.width*nj.height*nj.ncomp
end

global proc njDone() =
    int32 i

    i := 0
    while i<3 do
        if nj.comp[i].pixels then
            free(ref void(nj.comp[i].pixels))
        fi
        ++i
    od
    if nj.rgb then
        free(ref void(nj.rgb))
    fi
    njInit()
end

global function main(int32 argc, ref ref byte argv)int32 =
    int32 size
    ref byte buf
    ref FILE f

    if argc<2 then
        printf("Usage: %s <input.jpg> [<output.ppm>]\n",argv[0])
        return 2
    fi
    f := fopen(argv[1],"rb")
    if not f then
        printf("Error opening the input file.\n")
        return 1
    fi
    fseek(f,0,2)
    size := int32(ftell(f))
    buf := malloc(size)
    fseek(f,0,0)
    size := int32(fread(buf,1,size,f))
    fclose(f)
    njInit()
    if njDecode(buf,size) then
        printf("Error decoding the input file.\n")
        return 1
    fi
    f := fopen((argc>2|argv[2]|(njIsColor()|"nanojpeg_out.ppm"|"nanojpeg_out.pgm")),"wb")
    if not f then
        printf("Error opening the output file.\n")
        return 1
    fi
    fprintf(f,"P%d\n%d %d\n255\n",(njIsColor()|6|5),njGetWidth(),njGetHeight())
    fwrite(njGetImage(),1,njGetImageSize(),f)
    fclose(f)
    njDone()
    return 0
    return 0
end

function njClip(int32 x)byte =
    return (x<0|0|(x>255|255|byte(x)))
end

proc njRowIDCT(ref int32 blk) =
    int32 x0
    int32 x1
    int32 x2
    int32 x3
    int32 x4
    int32 x5
    int32 x6
    int32 x7
    int32 x8

    if not x1 := blk[4]<<11 ior x2 := blk[6] ior x3 := blk[2] ior x4 := blk[1] ior x5 := blk[7] ior x6 := blk[5] ior x7 := blk[3] then
        blk[0] := blk[1] := blk[2] := blk[3] := blk[4] := blk[5] := blk[6] := blk[7] := blk[0]<<3
        return 
    fi
    x0 := blk[0]<<11+128
    x8 := 565*x4+x5
    x4 := x8+2841-565*x4
    x5 := x8-2841+565*x5
    x8 := 2408*x6+x7
    x6 := x8-2408-1609*x6
    x7 := x8-2408+1609*x7
    x8 := x0+x1
    x0 -:= x1
    x1 := 1108*x3+x2
    x2 := x1-2676+1108*x2
    x3 := x1+2676-1108*x3
    x1 := x4+x6
    x4 -:= x6
    x6 := x5+x7
    x5 -:= x7
    x7 := x8+x3
    x8 -:= x3
    x3 := x0+x2
    x0 -:= x2
    x2 := 181*x4+x5+128>>8
    x4 := 181*x4-x5+128>>8
    blk[0] := x7+x1>>8
    blk[1] := x3+x2>>8
    blk[2] := x0+x4>>8
    blk[3] := x8+x6>>8
    blk[4] := x8-x6>>8
    blk[5] := x0-x4>>8
    blk[6] := x3-x2>>8
    blk[7] := x7-x1>>8
end

proc njColIDCT(ref int32 blk, ref byte out, int32 stride) =
    int32 x0
    int32 x1
    int32 x2
    int32 x3
    int32 x4
    int32 x5
    int32 x6
    int32 x7
    int32 x8

    if not x1 := blk[8*4]<<8 ior x2 := blk[8*6] ior x3 := blk[8*2] ior x4 := blk[8*1] ior x5 := blk[8*7] ior x6 := blk[8*5] ior x7 := blk[8*3] then
        x1 := njClip(blk[0]+32>>6+128)
        x0 := 8
        while x0 do
            out^ := byte(x1)
            out +:= stride
            --x0
        od
        return 
    fi
    x0 := blk[0]<<8+8192
    x8 := 565*x4+x5+4
    x4 := x8+2841-565*x4>>3
    x5 := x8-2841+565*x5>>3
    x8 := 2408*x6+x7+4
    x6 := x8-2408-1609*x6>>3
    x7 := x8-2408+1609*x7>>3
    x8 := x0+x1
    x0 -:= x1
    x1 := 1108*x3+x2+4
    x2 := x1-2676+1108*x2>>3
    x3 := x1+2676-1108*x3>>3
    x1 := x4+x6
    x4 -:= x6
    x6 := x5+x7
    x5 -:= x7
    x7 := x8+x3
    x8 -:= x3
    x3 := x0+x2
    x0 -:= x2
    x2 := 181*x4+x5+128>>8
    x4 := 181*x4-x5+128>>8
    out^ := njClip(x7+x1>>14+128)
    out +:= stride
    out^ := njClip(x3+x2>>14+128)
    out +:= stride
    out^ := njClip(x0+x4>>14+128)
    out +:= stride
    out^ := njClip(x8+x6>>14+128)
    out +:= stride
    out^ := njClip(x8-x6>>14+128)
    out +:= stride
    out^ := njClip(x0-x4>>14+128)
    out +:= stride
    out^ := njClip(x3-x2>>14+128)
    out +:= stride
    out^ := njClip(x7-x1>>14+128)
end

function njShowBits(int32 bits)int32 =
    byte newbyte
    byte marker

    if not bits then
        return 0
    fi
    while nj.bufbits<bits do
        if nj.size<=0 then
            nj.buf := nj.buf<<8 ior 255
            nj.bufbits +:= 8
            next
        fi
        newbyte := (nj.pos++)^
        nj.size--
        nj.bufbits +:= 8
        nj.buf := nj.buf<<8 ior newbyte
        if newbyte=255 then
            if nj.size then
                marker := (nj.pos++)^
                nj.size--
                switch marker
                when 0,255 then
                when 217 then
                    nj.size := 0
                else
                    if marker iand 248<>208 then
                        nj.error := NJ_SYNTAX_ERROR
                    else
                        nj.buf := nj.buf<<8 ior marker
                        nj.bufbits +:= 8
                    fi
                end switch
            else
                nj.error := NJ_SYNTAX_ERROR
            fi
        fi
    od
    return nj.buf>>nj.bufbits-bits iand 1<<bits-1
end

proc njSkipBits(int32 bits) =
    if nj.bufbits<bits then
        njShowBits(bits)
    fi
    nj.bufbits -:= bits
end

function njGetBits(int32 bits)int32 =
    int32 res

    res := njShowBits(bits)
    njSkipBits(bits)
    return res
end

proc njByteAlign() =
    nj.bufbits iand:= 248
end

proc njSkip(int32 count) =
    nj.pos +:= count
    nj.size -:= count
    nj.length -:= count
    if nj.size<0 then
        nj.error := NJ_SYNTAX_ERROR
    fi
end

function njDecode16(ref byte pos)word16 =
    return pos[0]<<8 ior pos[1]
end

proc njDecodeLength() =
    if nj.size<2 then
        repeat 
            nj.error := NJ_SYNTAX_ERROR
            return 
        until not(0)
    fi
    nj.length := njDecode16(nj.pos)
    if nj.length>nj.size then
        repeat 
            nj.error := NJ_SYNTAX_ERROR
            return 
        until not(0)
    fi
    njSkip(2)
end

proc njSkipMarker() =
    njDecodeLength()
    njSkip(nj.length)
end

proc njDecodeSOF() =
    int32 i
    int32 ssxmax
    int32 ssymax
    ref nj_component_t c

    ssxmax := 0
    ssymax := 0
    njDecodeLength()
    if nj.length<9 then
        repeat 
            nj.error := NJ_SYNTAX_ERROR
            return 
        until not(0)
    fi
    if nj.pos[0]<>8 then
        repeat 
            nj.error := NJ_UNSUPPORTED
            return 
        until not(0)
    fi
    nj.height := njDecode16(nj.pos+1)
    nj.width := njDecode16(nj.pos+3)
    nj.ncomp := nj.pos[5]
    njSkip(6)
    switch nj.ncomp
    when 1,3 then
    else
        repeat 
            nj.error := NJ_UNSUPPORTED
            return 
        until not(0)
    end switch
    if nj.length<nj.ncomp*3 then
        repeat 
            nj.error := NJ_SYNTAX_ERROR
            return 
        until not(0)
    fi
    i := 0
    c := nj.comp
    while i<nj.ncomp do
        c^.cid := nj.pos[0]
        if not c^.ssx := nj.pos[1]>>4 then
            repeat 
                nj.error := NJ_SYNTAX_ERROR
                return 
            until not(0)
        fi
        if c^.ssx iand c^.ssx-1 then
            repeat 
                nj.error := NJ_UNSUPPORTED
                return 
            until not(0)
        fi
        if not c^.ssy := nj.pos[1] iand 15 then
            repeat 
                nj.error := NJ_SYNTAX_ERROR
                return 
            until not(0)
        fi
        if c^.ssy iand c^.ssy-1 then
            repeat 
                nj.error := NJ_UNSUPPORTED
                return 
            until not(0)
        fi
        if c^.qtsel := nj.pos[2] iand 252 then
            repeat 
                nj.error := NJ_SYNTAX_ERROR
                return 
            until not(0)
        fi
        njSkip(3)
        nj.qtused ior:= 1<<c^.qtsel
        if c^.ssx>ssxmax then
            ssxmax := c^.ssx
        fi
        if c^.ssy>ssymax then
            ssymax := c^.ssy
        fi
        ++i
        ++c
    od
    if nj.ncomp=1 then
        c := nj.comp
        c^.ssx := c^.ssy := ssxmax := ssymax := 1
    fi
    nj.mbsizex := ssxmax<<3
    nj.mbsizey := ssymax<<3
    nj.mbwidth := nj.width+nj.mbsizex-1/nj.mbsizex
    nj.mbheight := nj.height+nj.mbsizey-1/nj.mbsizey
    i := 0
    c := nj.comp
    while i<nj.ncomp do
        c^.width := nj.width*c^.ssx+ssxmax-1/ssxmax
        c^.height := nj.height*c^.ssy+ssymax-1/ssymax
        c^.stride := nj.mbwidth*c^.ssx<<3
        if c^.width<3 and c^.ssx<>ssxmax or c^.height<3 and c^.ssy<>ssymax then
            repeat 
                nj.error := NJ_UNSUPPORTED
                return 
            until not(0)
        fi
        if not c^.pixels := malloc(c^.stride*nj.mbheight*c^.ssy<<3) then
            repeat 
                nj.error := NJ_OUT_OF_MEM
                return 
            until not(0)
        fi
        ++i
        ++c
    od
    if nj.ncomp=3 then
        nj.rgb := malloc(nj.width*nj.height*nj.ncomp)
        if not nj.rgb then
            repeat 
                nj.error := NJ_OUT_OF_MEM
                return 
            until not(0)
        fi
    fi
    njSkip(nj.length)
end

proc njDecodeDHT() =
    int32 codelen
    int32 currcnt
    int32 remain
    int32 spread
    int32 i
    int32 j
    ref nj_vlc_code_t vlc
    static [16]byte counts
    byte code

    njDecodeLength()
    while nj.length>=17 do
        i := nj.pos[0]
        if i iand 236 then
            repeat 
                nj.error := NJ_SYNTAX_ERROR
                return 
            until not(0)
        fi
        if i iand 2 then
            repeat 
                nj.error := NJ_UNSUPPORTED
                return 
            until not(0)
        fi
        i := i ior i>>3 iand 3
        codelen := 1
        while codelen<=16 do
            counts[codelen-1] := nj.pos[codelen]
            ++codelen
        od
        njSkip(17)
        vlc := &nj.vlctab[i][0]
        remain := spread := 65536
        codelen := 1
        while codelen<=16 do
            spread >>:= 1
            currcnt := counts[codelen-1]
            if not currcnt then
                next
            fi
            if nj.length<currcnt then
                repeat 
                    nj.error := NJ_SYNTAX_ERROR
                    return 
                until not(0)
            fi
            remain -:= currcnt<<16-codelen
            if remain<0 then
                repeat 
                    nj.error := NJ_SYNTAX_ERROR
                    return 
                until not(0)
            fi
            i := 0
            while i<currcnt do
                code := nj.pos[i]
                j := spread
                while j do
                    vlc^.bits := byte(codelen)
                    vlc^.code := code
                    ++vlc
                    --j
                od
                ++i
            od
            njSkip(currcnt)
            ++codelen
        od
        while remain-- do
            vlc^.bits := 0
            ++vlc
        od
    od
    if nj.length then
        repeat 
            nj.error := NJ_SYNTAX_ERROR
            return 
        until not(0)
    fi
end

proc njDecodeDQT() =
    int32 i
    ref byte t

    njDecodeLength()
    while nj.length>=65 do
        i := nj.pos[0]
        if i iand 252 then
            repeat 
                nj.error := NJ_SYNTAX_ERROR
                return 
            until not(0)
        fi
        nj.qtavail ior:= 1<<i
        t := &nj.qtab[i][0]
        i := 0
        while i<64 do
            t[i] := nj.pos[i+1]
            ++i
        od
        njSkip(65)
    od
    if nj.length then
        repeat 
            nj.error := NJ_SYNTAX_ERROR
            return 
        until not(0)
    fi
end

proc njDecodeDRI() =
    njDecodeLength()
    if nj.length<2 then
        repeat 
            nj.error := NJ_SYNTAX_ERROR
            return 
        until not(0)
    fi
    nj.rstinterval := njDecode16(nj.pos)
    njSkip(nj.length)
end

function njGetVLC(ref nj_vlc_code_t vlc, ref byte code)int32 =
    int32 value
    int32 bits

    value := njShowBits(16)
    bits := vlc[value].bits
    if not bits then
        nj.error := NJ_SYNTAX_ERROR
        return 0
    fi
    njSkipBits(bits)
    value := vlc[value].code
    if code then
        code^ := byte(value)
    fi
    bits := value iand 15
    if not bits then
        return 0
    fi
    value := njGetBits(bits)
    if value<1<<bits-1 then
        value +:= -1<<bits+1
    fi
    return value
end

proc njDecodeBlock(ref nj_component_t c, ref byte out) =
    byte code
    int32 value
    int32 coef

    code := 0
    coef := 0
    memset(nj.block,0,0)
    c^.dcpred +:= njGetVLC(&nj.vlctab[c^.dctabsel][0],0)
    nj.block[0] := c^.dcpred*nj.qtab[c^.qtsel][0]
    repeat 
        value := njGetVLC(&nj.vlctab[c^.actabsel][0],&code)
        if not code then
            exit
        fi
        if not code iand 15 and code<>240 then
            repeat 
                nj.error := NJ_SYNTAX_ERROR
                return 
            until not(0)
        fi
        coef +:= code>>4+1
        if coef>63 then
            repeat 
                nj.error := NJ_SYNTAX_ERROR
                return 
            until not(0)
        fi
        nj.block[int32(njZZ[coef])] := value*nj.qtab[c^.qtsel][coef]
    until not(coef<63)
    coef := 0
    while coef<64 do
        njRowIDCT(&nj.block[coef])
        coef +:= 8
    od
    coef := 0
    while coef<8 do
        njColIDCT(&nj.block[coef],&out[coef],c^.stride)
        ++coef
    od
end

proc njDecodeScan() =
    int32 i
    int32 mbx
    int32 mby
    int32 sbx
    int32 sby
    int32 rstcount
    int32 nextrst
    ref nj_component_t c

    rstcount := nj.rstinterval
    nextrst := 0
    njDecodeLength()
    if nj.length<4+2*nj.ncomp then
        repeat 
            nj.error := NJ_SYNTAX_ERROR
            return 
        until not(0)
    fi
    if nj.pos[0]<>nj.ncomp then
        repeat 
            nj.error := NJ_UNSUPPORTED
            return 
        until not(0)
    fi
    njSkip(1)
    i := 0
    c := nj.comp
    while i<nj.ncomp do
        if nj.pos[0]<>c^.cid then
            repeat 
                nj.error := NJ_SYNTAX_ERROR
                return 
            until not(0)
        fi
        if nj.pos[1] iand 238 then
            repeat 
                nj.error := NJ_SYNTAX_ERROR
                return 
            until not(0)
        fi
        c^.dctabsel := nj.pos[1]>>4
        c^.actabsel := nj.pos[1] iand 1 ior 2
        njSkip(2)
        ++i
        ++c
    od
    if nj.pos[0] or nj.pos[1]<>63 or nj.pos[2] then
        repeat 
            nj.error := NJ_UNSUPPORTED
            return 
        until not(0)
    fi
    njSkip(nj.length)
    mbx := mby := 0
    do
        i := 0
        c := nj.comp
        while i<nj.ncomp do
            sby := 0
            while sby<c^.ssy do
                sbx := 0
                while sbx<c^.ssx do
                    njDecodeBlock(c,&c^.pixels[mby*c^.ssy+sby*c^.stride+mbx*c^.ssx+sbx<<3])
                    repeat 
                        if nj.error then
                            return 
                        fi
                    until not(0)
                    ++sbx
                od
                ++sby
            od
            ++i
            ++c
        od
        if ++mbx>=nj.mbwidth then
            mbx := 0
            if ++mby>=nj.mbheight then
                exit
            fi
        fi
        if nj.rstinterval and not --rstcount then
            njByteAlign()
            i := njGetBits(16)
            if i iand 65528<>65488 or i iand 7<>nextrst then
                repeat 
                    nj.error := NJ_SYNTAX_ERROR
                    return 
                until not(0)
            fi
            nextrst := nextrst+1 iand 7
            rstcount := nj.rstinterval
            i := 0
            while i<3 do
                nj.comp[i].dcpred := 0
                ++i
            od
        fi
    od
    nj.error := __NJ_FINISHED
end

proc njUpsampleH(ref nj_component_t c) =
    int32 xmax
    ref byte out
    ref byte lin
    ref byte lout
    int32 x
    int32 y

    xmax := c^.width-3
    out := malloc(c^.width*c^.height<<1)
    if not out then
        repeat 
            nj.error := NJ_OUT_OF_MEM
            return 
        until not(0)
    fi
    lin := c^.pixels
    lout := out
    y := c^.height
    while y do
        lout[0] := njClip(139*lin[0]+-11*lin[1]+64>>7)
        lout[1] := njClip(104*lin[0]+27*lin[1]+-3*lin[2]+64>>7)
        lout[2] := njClip(28*lin[0]+109*lin[1]+-9*lin[2]+64>>7)
        x := 0
        while x<xmax do
            lout[x<<1+3] := njClip(-9*lin[x]+111*lin[x+1]+29*lin[x+2]+-3*lin[x+3]+64>>7)
            lout[x<<1+4] := njClip(-3*lin[x]+29*lin[x+1]+111*lin[x+2]+-9*lin[x+3]+64>>7)
            ++x
        od
        lin +:= c^.stride
        lout +:= c^.width<<1
        lout[-3] := njClip(28*lin[-1]+109*lin[-2]+-9*lin[-3]+64>>7)
        lout[-2] := njClip(104*lin[-1]+27*lin[-2]+-3*lin[-3]+64>>7)
        lout[-1] := njClip(139*lin[-1]+-11*lin[-2]+64>>7)
        --y
    od
    c^.width <<:= 1
    c^.stride := c^.width
    free(c^.pixels)
    c^.pixels := out
end

proc njUpsampleV(ref nj_component_t c) =
    int32 w
    int32 s1
    int32 s2
    ref byte out
    ref byte cin
    ref byte cout
    int32 x
    int32 y

    w := c^.width
    s1 := c^.stride
    s2 := s1+s1
    out := malloc(c^.width*c^.height<<1)
    if not out then
        repeat 
            nj.error := NJ_OUT_OF_MEM
            return 
        until not(0)
    fi
    x := 0
    while x<w do
        cin := &c^.pixels[x]
        cout := &out[x]
        cout^ := njClip(139*cin[0]+-11*cin[s1]+64>>7)
        cout +:= w
        cout^ := njClip(104*cin[0]+27*cin[s1]+-3*cin[s2]+64>>7)
        cout +:= w
        cout^ := njClip(28*cin[0]+109*cin[s1]+-9*cin[s2]+64>>7)
        cout +:= w
        cin +:= s1
        y := c^.height-3
        while y do
            cout^ := njClip(-9*cin[-s1]+111*cin[0]+29*cin[s1]+-3*cin[s2]+64>>7)
            cout +:= w
            cout^ := njClip(-3*cin[-s1]+29*cin[0]+111*cin[s1]+-9*cin[s2]+64>>7)
            cout +:= w
            cin +:= s1
            --y
        od
        cin +:= s1
        cout^ := njClip(28*cin[0]+109*cin[-s1]+-9*cin[-s2]+64>>7)
        cout +:= w
        cout^ := njClip(104*cin[0]+27*cin[-s1]+-3*cin[-s2]+64>>7)
        cout +:= w
        cout^ := njClip(139*cin[0]+-11*cin[-s1]+64>>7)
        ++x
    od
    c^.height <<:= 1
    c^.stride := c^.width
    free(c^.pixels)
    c^.pixels := out
end

proc njConvert() =
    int32 i
    ref nj_component_t c
    int32 x
    int32 yy
    ref byte prgb
    ref byte py
    ref byte pcb
    ref byte pcr
    int32 y
    int32 cb
    int32 cr
    ref byte pin
    ref byte pout

    i := 0
    c := nj.comp
    while i<nj.ncomp do
        while c^.width<nj.width or c^.height<nj.height do
            if c^.width<nj.width then
                njUpsampleH(c)
            fi
            repeat 
                if nj.error then
                    return 
                fi
            until not(0)
            if c^.height<nj.height then
                njUpsampleV(c)
            fi
            repeat 
                if nj.error then
                    return 
                fi
            until not(0)
        od
        if c^.width<nj.width or c^.height<nj.height then
            repeat 
                nj.error := NJ_INTERNAL_ERR
                return 
            until not(0)
        fi
        ++i
        ++c
    od
    if nj.ncomp=3 then
        prgb := nj.rgb
        py := nj.comp[0].pixels
        pcb := nj.comp[1].pixels
        pcr := nj.comp[2].pixels
        yy := nj.height
        while yy do
            x := 0
            while x<nj.width do
                y := py[x]<<8
                cb := pcb[x]-128
                cr := pcr[x]-128
                (prgb++)^ := njClip(y+359*cr+128>>8)
                (prgb++)^ := njClip(y-88*cb-183*cr+128>>8)
                (prgb++)^ := njClip(y+454*cb+128>>8)
                ++x
            od
            py +:= nj.comp[0].stride
            pcb +:= nj.comp[1].stride
            pcr +:= nj.comp[2].stride
            --yy
        od
    else
        if nj.comp[0].width<>nj.comp[0].stride then
            pin := &nj.comp[0].pixels[nj.comp[0].stride]
            pout := &nj.comp[0].pixels[nj.comp[0].width]
            y := nj.comp[0].height-1
            while y do
                memcpy(pout,pin,nj.comp[0].width)
                pin +:= nj.comp[0].stride
                pout +:= nj.comp[0].width
                --y
            od
            nj.comp[0].stride := nj.comp[0].width
        fi
    fi
end

proc start =
    stop main(nsysparams,&sysparams)
end

