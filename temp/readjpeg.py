#simple jpeg decoder for colour images
#supports 2x2, 2x1, 1x1 sub-sampling
#Some algorithms derived from the C code in the book:
# 'Basic Algorithms' by Malcolm McLean

import array
import copy

#file="card2"                  #2x2   2 Mpix
file="/jpeg/wtc-photo"         #2x2  87 Mpix

debug=0

class jpeginfoheader:
    pass

hdr=0
zigzag=0

class huffnode:
    def __init__(self):
        self.child0=0
        self.child1=0
        self.symbol=0
        self.suppbits=0

class stream():
    def __init__(self,a):
        self.data=a
        self.pos=0
        self.length=len(a)
        self.currbyte=0
        self.currbitmask=1

def initdata():
    global hdr,zigzag
    hdr=jpeginfoheader()

    hdr.width=0
    hdr.height=0
    hdr.framebytes=0
    hdr.ncomponents=0

    hdr.qtable=[[0,]*64,[0,]*64,[0,]*64,[0,]*64]
    hdr.vsample=[0,0,0,0]
    hdr.hsample=[0,0,0,0]
    hdr.usedc=[0,0,0,0]
    hdr.useac=[0,0,0,0]
    hdr.useq=[0,0,0,0]
    hdr.comptype=[0,0,0,0]
    hdr.dctable=[0,0,0,0]
    hdr.actable=[0,0,0,0]
    hdr.dri=0

    zigzag=[
        0, 1, 5, 6,14,15,27,28,
        2, 4, 7,13,16,26,29,42,
        3, 8,12,17,25,30,41,43,
        9,11,18,24,31,40,44,53,
       10,19,23,32,39,45,52,54,
       20,22,33,38,46,51,55,60,
       21,34,37,47,50,56,59,61,
       35,36,48,49,57,58,62,63]

def getfile(file):
    try:
        data=open(file,"rb").read()
    except:
        return 0
    data=array.array('B',data)
    fs=stream(data)
    return fs

def abortjpeg(mess):
    print ("Error:",mess)
    raise

def clamp(x,a,b):
    return max(min(x,b),a)

def initbitstream(fs):
    fs.currbyte=0
    fs.currbitmask=1

def nextbyte(fs):
    if fs.pos>=fs.length:
        print ("Reading past end of data")
#       abortjpeg("Reading past end of data")
        return -1
    bb=fs.data[fs.pos]
    fs.pos+=1
    return bb

def nextdatabyte(fs):
    while(1):
        c=nextbyte(fs)
        if c!=0xFF:
            return c
        while(c==0xFF):
            c=nextbyte(fs)
        if c==0:
            return 0xFF

def nextbit(fs):
    if fs.currbitmask==1:
        fs.currbyte=nextdatabyte(fs)
        fs.currbitmask=0x100
    fs.currbitmask >>=1
    return (fs.currbyte & fs.currbitmask)!=0

def readword(fs):
    bb=nextbyte(fs)
    return bb<<8 | nextbyte(fs)

def read_app(fs,n):
    length=readword(fs)-2
    fs.pos=fs.pos+length

def read_dqt(fs):
    length=readword(fs)-2
    while length>0:
        table=[]
        tq=nextbyte(fs)
        pq=tq>>4
        tq=tq & 15
        length-=1

        if pq==0:
            for i in range(64):
                table.append(nextbyte(fs))
        else:
            for i in range(64):
                table.append(readword(fs))
        length-=64
        hdr.qtable[tq]=table

def read_sof(fs):
    readword(fs)
    precision=nextbyte(fs)
    if precision!=8:
        abortjpeg("precision != 8")
    hdr.height=readword(fs)
    hdr.width=readword(fs)
    hdr.ncomponents=nextbyte(fs)
    hdr.framebytes=hdr.width*hdr.height*hdr.ncomponents

    for i in range(hdr.ncomponents):
        hdr.comptype[i]=nextbyte(fs)
        sampling=nextbyte(fs)
        hdr.vsample[i]=sampling & 15
        hdr.hsample[i]=sampling>>4
        hdr.useq[i]=nextbyte(fs)

def read_dht(fs):
    length=readword(fs)-2

    while length>0:
        codeswithlength=[]
        symbols=[]
        t=nextbyte(fs)
        tabno=t & 15
        tabtype = (t>>4) & 15

        tot=0
        for i in range(16):
            codeswithlength.append(nextbyte(fs))
            tot+=codeswithlength[i]

        for i in range(tot):
            symbols.append(nextbyte(fs))

        tree=buildhufftree(codeswithlength, symbols)

        if tabtype==0:
            hdr.dctable[tabno]=tree
        else:
            hdr.actable[tabno]=tree

        length-=(tot+17)

def buildhufftree(codelength,symbols):

    tot=sum(codelength)

    nodes=[]
    for i in range(tot*2-1):
        nodes.append(huffnode())

    codes=buildcanonical(codelength)

    buildtreerec(nodes,codes,symbols,tot,0)
    return nodes[0]

def buildtreerec(nodes,code,symbol,n,bitx):
    if n==0:
        return
    first=nodes[0]

    if n==1:
        first.child0=0
        first.child1=0
        first.symbol=symbol[0]
        first.suppbits-0
        k=len(code[0])-bitx
        if k>0:
            first.suppbits=k
            bitx+=k
        return

    for i2 in range(n):
        if code[i2][bitx]=='1':
            i=i2
            break
        i=i2            #?

    first.child0=nodes[1]
    first.child1=nodes[2*i]
    first.symbol=-1
    first.suppbits=0

    buildtreerec(nodes[1:],code,symbol,i,bitx+1)
    buildtreerec(nodes[2*i:],code[i:],symbol[i:],n-i,bitx+1)

def buildcanonical(codelengths):
    n=sum(codelengths)
    
    a=[]

    j=0
    code=0
    length=1
    for i in range(16):
        for k in range(codelengths[i]):
            a.append(tostrbin(code,length))
            code+=1
        code<<=1
        length+=1
    return a

def tostrbin(a,length):
    s=""
    for i in range(length):
        if a&1:
            s="1"+s
        else:
            s="0"+s
        a=a>>1
    return s

def readmarker(fs):
    nextbyte(fs)
    nextbyte(fs)
    initbitstream(fs)

def read_eoi(fs):
    c=nextbyte(fs)
    if c!=0xFF:
        abortjpeg("EOI FF")
    while c==0xFF:
        c=nextbyte(fs)
    if c!=0xD9:
        abortjpeg("EOI D9")

def read_sos(fs):
    length=readword(fs)-2
    ns=nextbyte(fs)
    if ns!=hdr.ncomponents:
        abortjpeg("ns!=hdr.ncomponents")
    for i in range(ns):
        hdr.comptype[i]=nextbyte(fs)
        t=nextbyte(fs)
        hdr.usedc[i]=t>>4
        hdr.useac[i]=t&15
    length-=ns*2+1

    while length>0:
        length-=1
        nextbyte(fs)

def loadscan(fs,hdr):
    print ("LOADSCAN",hdr.width,hdr.height)
    pimage=0

    initbitstream(fs)

    (vsample0,vsample1,vsample2,dummy)=hdr.vsample
    (hsample0,hsample1,hsample2,dummy)=hdr.hsample
    (comptype0,comptype1,comptype2,dummy)=hdr.comptype

    if hdr.ncomponents==1:
        abortjpeg("LOADMONO")
    elif hdr.ncomponents==3:
        if comptype0!=1 or comptype1!=2 or comptype2 != 3:
            abortjpeg("comptype?")
        if hsample1==vsample1==hsample2==vsample2 and (hsample0<=2 and vsample0<=2):
            pimage=loadcolour(fs,hdr,hsample0,vsample0)
        else:
            print (hsample0,vsample0,hsample1,vsample1,hsample2,vsample2)
            abortjpeg("Unknown sampling")
        
    else:
        abortjpeg("NCOMP")

    return pimage

def unzigzag(block):
    temp=copy.copy(block)
    for i in range(64):
        block[i]=temp[zigzag[i]]

def idct8x8(block):
    for i in range(8):
        j=i*8
        (   block[j],block[j+1],
            block[j+2],block[j+3],
            block[j+4],block[j+5],
            block[j+6],block[j+7]) = fastidct8(
                block[j],
                block[j+1],
                block[j+2],
                block[j+3],
                block[j+4],
                block[j+5],
                block[j+6],
                block[j+7])

    for i in range(64):
        block[i] >>= 3

    for i in range(8):
        (block[i],
        block[i+8],
        block[i+16],
        block[i+24],
        block[i+32],
        block[i+40],
        block[i+48],
        block[i+56])=fastidct8(
            block[i],
            block[i+8],
            block[i+16],
            block[i+24],
            block[i+32],
            block[i+40],
            block[i+48],
            block[i+56])

def fastidct8(a1,a2,a3,a4,a5,a6,a7,a8):
    w1 = 2841
    w2 = 2676
    w3 = 2408
    w5 = 1609
    w6 = 1108
    w7 = 565

    x2 = a5 << 11
    x3 = a7
    x4 = a3
    x5 = a2
    x6 = a8
    x7 = a6
    x8 = a4

    if not (x2 | x3 | x4 | x5 |x6 | x7 | x8):
        a1=a2=a3=a4=a5=a6=a7=a8=a1<<3
        return [a1,a2,a3,a4,a5,a6,a7,a8]
    x1 = (a1 << 11) + 128

    x9 = w7 * (x5 + x6)
    x5 = x9 + (w1 - w7) * x5
    x6 = x9 - (w1 + w7) * x6
    x9 = w3 * (x7 + x8)
    x7 = x9 - (w3 - w5) * x7
    x8 = x9 - (w3 + w5) * x8

    x9 = x1 + x2
    x1 -= x2
    x2 = w6 * (x4 + x3)
    x3 = x2 - (w2 + w6) * x3
    x4 = x2 + (w2 - w6) * x4
    x2 = x5 + x7
    x5 -= x7
    x7 = x6 + x8
    x6 -= x8

    x8 = x9 + x4
    x9 -= x4
    x4 = x1 + x3
    x1 -= x3
    x3 = (181 * (x5 + x6) + 128) >> 8
    x5 = (181 * (x5 - x6) + 128) >> 8

    #fourth stage
    a1 = (x8 + x2) >> 8
    a2 = (x4 + x3) >> 8
    a3 = (x1 + x5) >> 8
    a4 = (x9 + x7) >> 8
    a5 = (x9 - x7) >> 8
    a6 = (x1 - x5) >> 8
    a7 = (x4 - x3) >> 8
    a8 = (x8 - x2) >> 8
    return [a1,a2,a3,a4,a5,a6,a7,a8]

def tree_getsymbol(fs,node):
    while node.child0!=0:
        if nextbit(fs):
            node = node.child1
        else:
          node = node.child0

    for i in range(node.suppbits):
        nextbit(fs)

    return node.symbol

def getsymbol(fs,nbits):
    if nbits == 0:
        return 0

    a = 0
    for i in range(nbits):
        a <<= 1
        a |= nextbit(fs)

    if a & (1 << (nbits-1)) == 0:
        a -= (1 << nbits ) -1 

    return a

def getblock(fs, block, dctree, actree):
    nbits = tree_getsymbol(fs,dctree)
    nread=0

    block[nread] = getsymbol(fs, nbits)
    nread+=1

    while(1):
        bb = tree_getsymbol(fs,actree)
        if bb == 0xF0:
            if nread > 48:
                abortjpeg("GETBLOCK1")

            for i in range(16):
                block[nread] = 0
                nread+=1

        zeroes = bb >> 4
        nbits = bb & 15
        if nbits:
            if (nread + zeroes > 63):
                abortjpeg("GETBLOCK2")
            for i in range(zeroes):
                block[nread] = 0
                nread+=1
            block[nread] = getsymbol(fs, nbits)
            nread+=1
            if nread == 64:
                break
        if bb==0:
            break

    while nread<64:
        block[nread] = 0
        nread+=1

def readblock(fs, block, dctable, actable, qtable, dc):
    getblock(fs,block, dctable,actable)

    block[0]+=dc
    dc=block[0]

    for k in range(64):
        block[k]*=qtable[k]

    unzigzag(block)
    idct8x8(block)

    return dc

def loadcolour(fs,hdr,hoz,vert):
    print ("READCOLOUR",hoz,vert, hdr.width,hdr.height)

    data=bytearray(hdr.framebytes)
#   data=[0,]*hdr.framebytes
#   data=array.array('B',data)

    diffdc=dcb=dcr=0
    nlum=hoz*vert
    lum=[[0,]*64,[0,]*64,[0,]*64,[0,]*64]
    cr=[0,]*64
    cb=[0,]*64

    dctable_lum = hdr.dctable[hdr.usedc[0]]
    actable_lum = hdr.actable[hdr.useac[0]]
    qtable_lum  = hdr.qtable[hdr.useq[0]]

    dctable_cb  = hdr.dctable[hdr.usedc[1]]
    actable_cb  = hdr.actable[hdr.useac[1]]
    qtable_cb   = hdr.qtable[hdr.useq[1]]

    dctable_cr  = hdr.dctable[hdr.usedc[2]]
    actable_cr  = hdr.actable[hdr.useac[2]]
    qtable_cr   = hdr.qtable[hdr.useq[2]]

    count=0
    y=0

    while y<hdr.height:
        x=0
        while x<hdr.width:
            if hdr.dri and (count % hdr.dri)==0 and count>0:
                readmarker(fs)
                diffdc=dcb=dcr=0

            for j in range(nlum):
                diffdc=readblock(fs,lum[j],dctable_lum, actable_lum, qtable_lum, diffdc)

            dcb=readblock(fs,cb,dctable_cb, actable_cb, qtable_cb, dcb)
            dcr=readblock(fs,cr,dctable_cr, actable_cr, qtable_cr, dcr)

            reconsblockcolour(lum[0],lum[1],lum[2],lum[3],cr,cb,data,x,y, hoz,vert)
            count+=1
            x+=hoz*8
        y+=vert*8

    read_eoi(fs)
    return data

def reconsblockcolour(lum1,lum2,lum3,lum4,cr,cb, data,x,y, hoz,vert):

    width=hdr.width
    height=hdr.height

    ilim=vert*8
    jlim=hoz*8

    for i in range(ilim):
        yy=y+i
        if yy>=height:
            break
        ix=(yy*width+x)*3

        ii=i//vert*8
        if i<8:
            less8=1
            ioffset=i*8
        else:
            less8=0
            ioffset=(i-8)*8

        for j in range(jlim):
            if x+j>=width:
                break
            if less8:
                if j<8:
                    luminance = lum1[ioffset+j]//64+128
                else:
                    luminance = lum2[ioffset+j-8]//64+128
            else:
                if j<8:
                    luminance = lum3[ioffset+j]//64+128
                else:
                    luminance = lum4[ioffset+j-8]//64+128

            rr = cr[ii+j//2]
            bb = cb[ii+j//2]

            data[ix]    = clamp(rr*45//2048+luminance, 0,255)               #red
            ix+=1
            data[ix]    = clamp(luminance - ((bb*11) + rr*23)//2048, 0,255) #green
            ix+=1
            data[ix]    = clamp((bb*57//2048)+luminance, 0,255)             #blue
            ix+=1

def readjpegfile(file):

    initdata()

    fs=getfile(file)
    if fs==0:
        return 0
        exit(0)
    pimage=0

    while (1):
        c=nextbyte(fs)
#       print ("CC1",c)
        if c==0xFF:
            c=nextbyte(fs)
#           print ("CC2",c)
            if c==0xD8:
#               print ("FFD8 SOI")
                pass
            elif c==0xEE:
#               print ("FFEE COM comment")
                pass
            elif c>=0xE0 and c<=0xEF:
#               print ("FFEx APP")
                read_app(fs,c-0xE0)
            elif c==0xC0:
#               print ("FFC0 SOF0 baseline")
                read_sof(fs)
#           elif c==0xC2:
#               print ("FFC2 SOF2 progressive")
            elif c==0xC4:
#               print ("FFC4 DHT")
                read_dht(fs)
            elif c==0xDB:
#               print ("FFDB DQT")
                read_dqt(fs)
            elif c==0xDD:
#               print ("FFDD DRI RST interval")
                readword(fs)                #skip length
                hdr.dri=readword(fs)
            elif c==0xDA:
#               print ("FFDA SOS")
                read_sos(fs)
                pimage=loadscan(fs,hdr)
                break
#           elif c==0xD9:
#               print ("FFD9 EOI")
#           elif c==0x0:
#               print ("FF00 <embedded FF data>")
#           elif c==0xFF:
#               print ("FFFF padding")
            elif c==-1:
                break
#           else:
#               print ("FF",c,"Unknown marker")
        elif c==-1:
            break
    return pimage

def start():
#   print ("START")
#   exit(0)
    jpgfile=file+".jpg"
    data=readjpegfile(jpgfile)

    if data==0:
        print ("Can't load",jpgfile)
        exit(0)

    print("Done")
    exit(0)

#   f = open(file+".ppm", "wb")
#   s="P6\n%d %d\n255\n" % (hdr.width, hdr.height)
#   sbytes=array.array('B',list(map(ord,s)))
#   f.write(sbytes)
#   f.write(array.array('B',data))
#   f.write(data)
#   f.close()

start()
