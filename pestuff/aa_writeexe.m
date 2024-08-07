!Create .exe file from SS-data (code, data, reloc and symbol tables)
!Call order:
! initsectiontable()
! genexe()
! writeexe(filename)

[maxsearchlibs]int64 libinsttable
[maxsearchlibs]ichar libinstnames
[maxsearchlibs]int libnotable           !index into dlltable

global const zsect=3
global const dsect=2
global const csect=1
global const isect=4

record basereloc =
    ref basereloc nextitem
    word32 address              !virtual address
    int32 reloctype
end

ref basereloc basereloclist
int nbaserelocs
int maxrelocaddr
const maxbaseblock=500
[maxbaseblock]int blockbases
[maxbaseblock]int32 blockcounts
[maxbaseblock]int32 blockbytes
[maxbaseblock]byte blockpadding
int nbaseblocks
int basetablesize

const filealign = 512
const sectionalign = 4096
const exe_imagebase = 0x40'0000
const dll_imagebase = 0x1000'0000
global word imagebase

int imagesize
int filesize
ref[]int64 thunktable               !point into code segment
int fileiatoffset
int fileiatsize
ref strec stentrypoint              !symbol to be the entry point
ref strec stentrypoint2
ref strec stentrypoint3

const maxsection = 10
global [maxsection]sectionrec sectiontable
global int nsections

ref byte importdir              !allowed section data for import directort in .idata

!global const maximports = 3000
global const maximports = 1000
global [maximports]importrec importtable
global int nimports

global const maxexports = 3000
global [maxexports]exportrec exporttable
[maxexports]int sortindex
global int nexports
ichar dllfilename
int isdll

global const maxlibs = 50
global [maxlibs]dllrec dlltable
global int ndlls

global ref byte datastart
global ref byte dataptr
global ichar userentrypoint

int exportdirvirtaddr
int exportdirvirtsize
int exportdiroffset             !from start of imp dir

int blockdirvirtaddr
int blockdirvirtsize
int blockdiroffset

export proc genexe(ichar entrypoint, outfile, int dodll)=
!manipulate the ss data to fill in all the details needed for exe format
    int offset
    ref byte codeaddr               !mem address of start of code seg
    ref u32 offsetptr

    dllfilename:=outfile
    isdll:=dodll

    imagebase:=(isdll|dll_imagebase|exe_imagebase)

    userentrypoint:=entrypoint
    loadlibs()
    scanst()                !build dll/import tables

    getoffsets()
    relocdata(&sectiontable[csect],0)
    relocdata(&sectiontable[dsect],1)

    codeaddr:=bufferelemptr(sectiontable[csect].data, 0)

    if highmem then
        println "Doing RIP relocs..."

        ref riprec pr

        pr:=riplist
        while pr, pr:=pr.next do
            offsetptr:=ref u32(codeaddr+pr.offset)
            offset:=getripoffset(pr.offset, offsetptr^-imagebase, pr.immsize)
            offsetptr^:=offset

    
        od
    fi
end

export proc writeexe(ichar outfile, int dodll)=
    imagefileheader header
    optionalheader optheader
    int offset,i
    int64 aa

    dllfilename:=outfile
    isdll:=dodll

    datastart:=dataptr:=pcm_allocz(filesize)

    writedosstub()
    writepesig()
    writefileheader()
    writeoptheader()
    for i to nsections do
        writesectionheader(&sectiontable[i])
    od
    writepadding(sectiontable[1].rawoffset)
    for i to nsections do
        writesectiondata(&sectiontable[i])
    od

    if fverbose then
        PRINTLN "Writing file:",outfile
    fi

    if writefile(outfile,datastart,dataptr-datastart)=0 then
        println "Error writing exe file (possibly still running)"
        stop 1
    fi
end

proc loadlibs=
!load library instances
    int i
    int64 hinst
    ichar file
    [300]char filename

    for i to nsearchlibs do
        strcpy(&.filename,searchlibs[i])
        strcat(&.filename,".dll")
        hinst:=os_getdllinst(&.filename)
        if hinst=0 then
            println searchlibs[i]
            println &.FILENAME
            gerror("Can't load search lib")
        fi
        libinsttable[i]:=hinst
        libinstnames[i]:=pcm_copyheapstring(&.filename)
    od
end

export proc initsectiontable=
!set up the section table

    sectiontable[csect].name:=".text"
    sectiontable[csect].segtype:=code_seg
    sectiontable[csect].data:=ss_code
    sectiontable[csect].virtsize:=bufferlength(ss_code)

    if bufferlength(ss_idata)=0 then
        addqword (ss_idata,0)
    fi

    sectiontable[dsect].name:=".data"
    sectiontable[dsect].segtype:=idata_seg
    sectiontable[dsect].data:=ss_idata

    sectiontable[dsect].virtsize:=bufferlength(ss_idata)
    sectiontable[dsect].rawsize:=roundtoblock(sectiontable[dsect].virtsize,filealign)
    sectiontable[dsect].nrelocs:=ss_nidatarelocs
    sectiontable[dsect].relocs:=ss_idatarelocs

    if ss_zdatalen=0 then
        ss_zdatalen:=16
    fi

    sectiontable[zsect].name:=".bss"
    sectiontable[zsect].segtype:=zdata_seg
    sectiontable[zsect].virtsize:=ss_zdatalen

!note: rawsize will be recalculated later after thunk table is added
    sectiontable[csect].rawsize:=roundtoblock(sectiontable[csect].virtsize,filealign)
    sectiontable[csect].nrelocs:=ss_ncoderelocs
    sectiontable[csect].relocs:=ss_coderelocs

    sectiontable[isect].name:=".idata"
    sectiontable[isect].segtype:=impdata_seg
    sectiontable[isect].virtsize:=0
    sectiontable[isect].rawsize:=0

    nsections:=4
end

function extractlibname(ichar name, int &libno,moduleno)ichar=
!if name contains a dot, eg lib.abc, then set libno to index of "lib", and return "abc"
!otherwise return original name
    ref char s,name2
    [256]char str
    [256]char str2
    int i

    name2:=nil

    reenter:
    s:=name
    libno:=0

    while s^ do
        if s^='.' then          !assume lib.name
            memcpy(&.str,name,s-name)
            str[s-name+1]:=0
            strcat(&.str,".dll")

            for i:=1 to ndlls do
                if eqstring(&.str,dlltable[i].name) then
                    libno:=i
                    ++dlltable[libno].nprocs
                    return (name2|name2|s+1)
                fi
            od
            if ndlls>=maxlibs then gerror("Too many libs") fi
            libno:=++ndlls

            dlltable[libno].name:=pcm_copyheapstring(&.str)
            dlltable[libno].nprocs:=1
            return (name2|name2|s+1)
        fi

        ++s
    od

!do explicit search
    int n

    for i:=1 to nsearchlibs do
        if os_getdllprocaddr(libinsttable[i],name) then
            n:=i
            exit                !don't need the actual address; just whether it exists
        fi
    else
        println name,moduletable[moduleno].name
        gerror("Can't find external function")
    od

!found in search lib n
    if libno:=libnotable[n] then            !already added this library
        ++dlltable[libno].nprocs
        return name
    fi

!first use of this lib
    strcpy(&.str,searchlibs[n])
    strcat(&.str,".dll")
    if ndlls>=maxlibs then gerror("2:Too many libs") fi
    libno:=++ndlls

    dlltable[libno].name:=pcm_copyheapstring(&.str)
    dlltable[libno].nprocs:=1
    libnotable[n]:=libno

    return name
end

proc scanst=
!scan symbol table and build dll and imports list
!this version assumes dlls are encoded into the name of each import
!(otherwise, it means requiring a list of dlls and loading/searching for
!the names: doing real linker work.)

    int i,libno
    ref strec d
    ichar name, libname

    for i:=1 to ss_nsymbols do

        d:=ss_symboltable^[i]
        case d.symbol
        when importedsym then
            if nimports>=maximports then gerror("genexe: Too many imports") fi
            ++nimports

            name:=extractlibname(d.name,libno,d.moduleno)

            importtable[nimports].libno:=libno          !0 if no lib
            importtable[nimports].name:=name                !original, or 2nd part of lib.name
            importtable[nimports].def:=d

            d.importindex:=nimports
        when exportedsym then
            if userentrypoint then
                if eqstring(d.name,userentrypoint) then
                    stentrypoint:=d
                fi
            else
!               if eqstring(d.name,"main") and not isdll then
                if eqstring(d.name,"main") then
                    stentrypoint:=d
                elsif eqstring(d.name,"start") and not isdll then
                    stentrypoint2:=d
                elsif eqstring(d.name,"dllmain") and isdll then
                    stentrypoint:=d
                fi
            fi

            if stentrypoint and stentrypoint.segment<>code_seg then
                gerror("Entry point not in code seg")
            fi

            if nexports>=maxexports then gerror("gendll: Too many exports") fi
            ++nexports

            exporttable[nexports].def:=d
            exporttable[nexports].name:=d.name

        esac
    od
end

proc relocdata(ref sectionrec s,int isdata)=
    ref sectionrec u
    ref relocrec r
    ref byte p
    ref word32 p32
    ref word64 p64
    ref strec d
    word thunkoffset
    int offset,index,iatoffset

    p:=bufferelemptr(s.data,0)
    r:=s.relocs

    while r do
        d:=ss_symboltable^[r.stindex]
        index:=d.importindex                !into importtable
        thunkoffset:=importtable[index].thunkoffset

        case r.reloctype
        when rel32_rel then
            if d.symbol<>importedsym then
                gerror("rel32/not imported")
            fi
            (ref word32(p+r.offset)^:=int(thunkoffset)-r.offset-4)

        when addr32_rel, addr64_rel then                !for addr64, just leave top half zero
            if d.symbol=importedsym then

                (ref word32(p+r.offset)^:=imagebase+thunkoffset+sectiontable[csect].virtoffset)
            else
                case d.segment
                when zdata_seg then u:=&sectiontable[zsect]
                when idata_seg then u:=&sectiontable[dsect]
                when code_seg then u:=&sectiontable[csect]
                esac

                p32:=cast(p+r.offset)
                IF R.RELOCTYPE=ADDR32_REL THEN
                    p32^:=p32^+u.virtoffset+imagebase
                ELSE
                    p64:=cast(p32)
                    p64^:=p64^+u.virtoffset+imagebase
                FI
            fi
        else
            println relocnames[r.reloctype]
            gerror("Can't do this rel type")
        esac

        r:=r.nextreloc
    od
end

proc getbaserelocs(ref sectionrec s)=
    ref relocrec r
    ref byte p
    ref strec d
    int index

    p:=bufferelemptr(s.data,0)
    r:=s.relocs

    while r do
        d:=ss_symboltable^[r.stindex]

        case r.reloctype
        when addr32_rel, addr64_rel then                !for addr64, just leave top half zero
            if d.symbol=importedsym then
            else
                IF R.RELOCTYPE=ADDR32_REL THEN
                    !PRINTLN "??BASE RELOC",(D.SEGMENT=CODE_SEG|"CODE"|"DATA"),(R.RELOCTYPE=ADDR32_REL|"ADDR32"|"ADDR64")
                ELSE
                    !PRINTLN "DOING BASERELOC",D.NAME,SEGMENTNAMES[S.SEGTYPE]
                    newbasereloc(s.virtoffset+r.offset, r.reloctype)
                FI
            fi
        esac

        r:=r.nextreloc
    od

end

proc writerecordx(ref void r, int length)=
    memcpy(dataptr,r,length)
    dataptr+:=length
end

proc writedosstub=
!write 128-byte dos stub to dataptr
    static []byte stubdata = (
        0x4D, 0x5A, 0x90, 0x00, 0x03, 0x00, 0x00, 0x00, 
        0x04, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00, 
        0xB8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00, 
        0x0E, 0x1F, 0xBA, 0x0E, 0x00, 0xB4, 0x09, 0xCD, 
        0x21, 0xB8, 0x01, 0x4C, 0xCD, 0x21, 0x54, 0x68, 
        0x69, 0x73, 0x20, 0x70, 0x72, 0x6F, 0x67, 0x72, 
        0x61, 0x6D, 0x20, 0x63, 0x61, 0x6E, 0x6E, 0x6F, 
        0x74, 0x20, 0x62, 0x65, 0x20, 0x72, 0x75, 0x6E, 
        0x20, 0x69, 0x6E, 0x20, 0x44, 0x4F, 0x53, 0x20, 
        0x6D, 0x6F, 0x64, 0x65, 0x2E, 0x0D, 0x0D, 0x0A, 
        0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)

    writerecordx(&stubdata,stubdata.bytes)
end

proc writepesig=
    dataptr++^:='P'
    dataptr++^:='E'
    dataptr++^:=0
    dataptr++^:=0
end

proc writepadding(int offset)=
!offset is the next desired offset in the file
    dataptr:=datastart+offset           !data will have been cleared
end

proc writefileheader=
    imagefileheader header

    clear header

    header.machine:=0x8664
    header.nsections:=nsections
    header.optheadersize:=optionalheader.bytes
    header.characteristics:=0x22F
    if isdll then
        header.characteristics:=0x22E ior 0x2000
    fi

    writerecordx(&header,header.bytes)
end

proc writeoptheader=
    optionalheader header

    clear header

    header.magic:=0x20B
    header.majorlv:=1
    header.minorlv:=0
    header.codesize:=sectiontable[csect].rawsize
    header.idatasize:=sectiontable[dsect].rawsize+sectiontable[isect].rawsize
    header.zdatasize:=roundtoblock(sectiontable[zsect].virtsize,filealign)

    if stentrypoint=nil then
        stentrypoint:=stentrypoint2
        if stentrypoint=nil then
            stentrypoint:=stentrypoint3
            if stentrypoint then
                println "Using tertiary 'WinMain' entry point"
            fi
        fi
    fi
    if stentrypoint=nil then
        if userentrypoint then
            println userentrypoint
            gerror("User entry point not found")
        else
            if not isdll then
                gerror("Entry point not found: main or start")
            fi
        fi
    else
        header.entrypoint:=sectiontable[csect].virtoffset+stentrypoint.offset
    fi

    header.codebase:=sectionalign
    header.imagebase:=imagebase
    header.sectionalignment:=sectionalign
    header.filealignment:=filealign
    header.majorosv:=4
    header.minorosv:=0
    header.majorssv:=5
    header.minorssv:=2
    header.imagesize:=imagesize
    header.headerssize:=sectiontable[1].rawoffset
    header.subsystem:=3

    header.stackreserve:=4194304
    header.stackcommit:=2097152

    header.heapreserve:=1048576
    header.heapcommit:=4096
    header.rvadims:=16

    header.importtable.virtualaddr:=sectiontable[isect].virtoffset
    header.importtable.size:=sectiontable[isect].virtsize-exportdirvirtsize-blockdirvirtsize

    if isdll then
        header.dllcharacteristics:=0x40     !relocatable
        header.exporttable.virtualaddr:=exportdirvirtaddr
        header.exporttable.size:=exportdirvirtsize

        header.basereloctable.virtualaddr:=blockdirvirtaddr
        header.basereloctable.size:=blockdirvirtsize
    fi

    header.iat.virtualaddr:=fileiatoffset
    header.iat.size:=fileiatsize

    writerecordx(&header,header.bytes)

end

proc writesectionheader(ref sectionrec s)=
    imagesectionheader sheader

    memset(&sheader,0,sheader.bytes)
!   clear sheader

    strcpy(&sheader.name[1],s.name)
    sheader.virtual_size:=s.virtsize
    sheader.virtual_address:=s.virtoffset
    sheader.rawdata_offset:=s.rawoffset
    sheader.rawdata_size:=s.rawsize

    int64 aa
    case s.segtype
    when zdata_seg then
        sheader.characteristics:=0xC050'0080
    when idata_seg then
        sheader.characteristics:=0xC050'0040
    when code_seg then
        sheader.characteristics:=0x6050'0020
    when impdata_seg then
!       sheader.characteristics:=0xC030'0040
        sheader.characteristics:=0x4030'0040
    esac
    writerecordx(&sheader,sheader.bytes)
end

proc writesectiondata(ref sectionrec s)=

    case s.segtype
    when impdata_seg then
        writerecordx(s.bytedata,s.virtsize)     !rest of section will be zeros
        if s.rawsize>s.virtsize then
            dataptr+:=(s.rawsize-s.virtsize)
        fi

    when zdata_seg then                 !nothing goes to disk
!       dataptr+:=s.rawsize
    else
        writerecordx(bufferelemptr(s.data,0),s.rawsize)
    esac
end

proc getoffsets=
!apply file/image offsets to sectiontable
    int fileoffset, imageoffset,i,diroffset,impdirno,hinttableoffset,j,n
    int codesize,length,thunkoffset,offset,dirstartoffset

    fileoffset:=128+4+imagefileheader.bytes+optionalheader.bytes    !dosstub+sig
    fileoffset+:=imagesectionheader.bytes*nsections

    fileoffset:=roundtoblock(fileoffset,filealign)
    imageoffset:=4096

!Need to increase size of code segment to incorporate the thunk table
    ref byte pcode
    codesize:=sectiontable[csect].virtsize
    pcode:=bufferelemptr(ss_code,codesize)
    while codesize iand 7 do pcode++^:=0x90; ++codesize od
    thunkoffset:=codesize
    codesize+:=nimports*8

    sectiontable[csect].virtsize:=codesize
    sectiontable[csect].rawsize:=roundtoblock(codesize,filealign)

!have to actually add the extra memory now.
    buffercheck(ss_code, codesize-thunkoffset+16)       !just ensure it's there for now

    for i:=1 to nsections do
        if sectiontable[i].segtype<>zdata_seg then
            sectiontable[i].rawoffset:=fileoffset
        fi
        if sectiontable[i].segtype<>zdata_seg then
            fileoffset:=roundtoblock(fileoffset+sectiontable[i].virtsize,filealign)
        fi
        sectiontable[i].virtoffset:=imageoffset

        if sectiontable[i].segtype=impdata_seg then
            diroffset:=imageoffset
            impdirno:=i
        fi

        imageoffset:=roundtoblock(imageoffset+sectiontable[i].virtsize,sectionalign)
    od

    if isdll then
        getbaserelocs(&sectiontable[csect])
        getbaserelocs(&sectiontable[dsect])
    fi

!Work out offsets within import directory
!assume dll/imports have been set up
!diroffset starts off as virtual offset of start of impdata section

    diroffset+:=(ndlls+1)*importdirrec.bytes            !need blank entry as terminator

!diroffset now points to import name table
!usual arrangements is for all import name table, followed by all import addr tables

    for i to ndlls do
        dlltable[i].nametableoffset:=diroffset              !data will be filled in later
        diroffset+:=(dlltable[i].nprocs+1)*8
    od
    fileiatoffset:=diroffset
    for i to ndlls do
        dlltable[i].addrtableoffset:=diroffset              !data will be filled in later
        diroffset+:=(dlltable[i].nprocs+1)*8
    od
    fileiatsize:=diroffset-fileiatoffset

!diroffset now points to hint/name table, which is shared by all libs
!At this point, I need to write into an actual impdata segment, which doesn't
!exist yet. So I need to do a first pass over the import names to work out the size
    hinttableoffset:=diroffset
    for i to nimports do
        length:=strlen(importtable[i].name)+3
        if length.odd then ++length fi      !keep even
        importtable[i].hintnameoffset:=diroffset
        diroffset+:=length
    od

!need also space for the names of the libs

!need I think to get to next multiple of four
    diroffset:=roundtoblock(diroffset,4)

    for i to ndlls do
        length:=strlen(dlltable[i].name)+1
        if length.odd then ++length fi      !keep even
        dlltable[i].dllextraoffset:=diroffset
        diroffset+:=dlltable[i].nprocs*4        !space for back-links to dir entry
        dlltable[i].dllnameoffset:=diroffset
        diroffset+:=length
    od

    dirstartoffset:=sectiontable[impdirno].virtoffset

    if isdll then

        exportdirvirtaddr:=diroffset
        exportdiroffset:=diroffset-dirstartoffset
        exportdirvirtsize:=getexporttablesize()
        diroffset+:=exportdirvirtsize

        scanbaserelocs()

        blockdirvirtaddr:=diroffset
        blockdiroffset:=diroffset-dirstartoffset
        blockdirvirtsize:=basetablesize
        diroffset+:=blockdirvirtsize
    fi

    offset:=diroffset-dirstartoffset

!offset contains now the overall size of the import directory
!diroffset contains the overall size of the image

!finish off last section data, and compute final file and image sizes
    sectiontable[impdirno].virtsize:=offset
    sectiontable[impdirno].rawsize:=roundtoblock(offset,filealign)
    filesize:=roundtoblock(fileoffset+offset,filealign)

    imagesize:=roundtoblock(imageoffset+(diroffset-dirstartoffset),sectionalign)

    ref byte pimpdir

    pimpdir:=sectiontable[impdirno].bytedata:=pcm_allocz(offset)

!prepare the thunk area in the code segment
    ref importdirrec pdir
    ref int64 paddr,pname
    int iatoffset
    pdir:=cast(pimpdir)

!start fill in details within the import directory section
    for i:=1 to ndlls do
        pdir.implookuprva:=dlltable[i].nametableoffset
        pdir.impaddressrva:=dlltable[i].addrtableoffset
        pdir.namerva:=dlltable[i].dllnameoffset
        ++pdir

        iatoffset:=dlltable[i].addrtableoffset
        paddr:=cast(pimpdir+iatoffset-dirstartoffset)
        pname:=cast(pimpdir+dlltable[i].nametableoffset-dirstartoffset)
        for j to nimports when importtable[j].libno=i do
            pname^:=paddr^:=importtable[j].hintnameoffset
            importtable[j].iatoffset:=iatoffset
            iatoffset+:=8
            ++pname
            ++paddr
        od
    od

!Fill in the hint/name table
    ref byte phint
    ref word32 pextra

    for i to nimports do
        phint:=pimpdir+importtable[i].hintnameoffset-dirstartoffset
        phint+:=2                   !leave hint as 0
        strcpy(cast(phint),importtable[i].name)
    od
!same for lib names (no hint here, but re-use phint anyway)
    int xxx
    xxx:=dirstartoffset
    for i to ndlls do
        pextra:=cast(pimpdir+dlltable[i].dllextraoffset-dirstartoffset)
        for j to dlltable[i].nprocs do
            pextra^:=xxx
            ++pextra
        od
        xxx+:=importdirrec.bytes
        phint:=pimpdir+dlltable[i].dllnameoffset-dirstartoffset
        strcpy(cast(phint),dlltable[i].name)
    od

    if isdll then
        writeexporttable(ref byte(pimpdir)+exportdiroffset)
        writebasereloctable(ref byte(pimpdir)+blockdiroffset)
    fi

!write the thunk table
    ref byte thunkptr,codebase
    int thunkaddr
    thunkptr:=bufferelemptr(ss_code,thunkoffset)
    codebase:=bufferelemptr(ss_code,0)

    for i to nimports do
        importtable[i].thunkoffset:=thunkptr-codebase
        if highmem=0 then
            thunkptr++^:=0x48
            thunkptr++^:=0xFF
            thunkptr++^:=0x24
            thunkptr++^:=0x25
            thunkaddr:=imagebase+importtable[i].iatoffset
            (ref int32(thunkptr)^:=thunkaddr)
            thunkptr+:=4
        else                    !use rip mode

            thunkptr++^:=0x48
            thunkptr++^:=0xFF
            thunkptr++^:=0x25
            thunkaddr:=imagebase+importtable[i].iatoffset
            (ref int32(thunkptr)^:=getripoffset(int(thunkptr-codebase),thunkaddr-imagebase))
            thunkptr+:=4
            thunkptr++^:=0x90
        fi
    od
end

func getripoffset(int addr, dest, int extra=0)int=
!work out the rip offset for a d32 field at <addr>, to <dest>
!opbytes is the number of opcode bytes that precede the field
!addr is offset of d32 field with codeseg, from start of code segment
!dest is offset within image, relative to imagebase
!extra is 0/1/2/4 bytes of imm data that some instructions will have

    addr+:=sectiontable[csect].virtoffset       !now is offset rel to imagebase

    dest-(addr+4)-extra

end

function getsectionno(int segment)int=
    case segment
    when zdata_seg then zsect
    when idata_seg then dsect
    when code_seg then csect
    else gerror("GSN"); 0
    esac
end

proc writeexporttable(ref byte pstart)=
    ref exportdirrec phdr := cast(pstart)
    ref word32 paddrtable
    ref word32 pnametable
    ref word16 pordtable
    ref char pdllname
    ref char pnames
    int addrtableoffset
    int nametableoffset
    int ordtableoffset
    int dllnameoffset
    int namesoffset
    int virtoffset
    int sectionno
    ref strec d

    phdr.timedatestamp:=0x5f89f4f8

    phdr.ordinalbase:=1
    phdr.naddrtable:=nexports
    phdr.nnamepointers:=nexports

!these are offsets from the start of the export data, from the start of the export dir
    addrtableoffset:=exportdirrec.bytes
    nametableoffset:=addrtableoffset+nexports*4
    ordtableoffset:=nametableoffset+nexports*4
    dllnameoffset:=ordtableoffset+nexports*2
    namesoffset:=dllnameoffset+strlen(dllfilename)+1

!virtoffset must be added to all above basic offsets, before being written to the file 
    virtoffset:=sectiontable[isect].virtoffset+exportdiroffset

!work out pointers into memory to receive the data
    paddrtable:=cast(pstart+addrtableoffset)
    pnametable:=cast(pstart+nametableoffset)
    pordtable:=cast(pstart+ordtableoffset)
    pdllname:=cast(pstart+dllnameoffset)
    pnames:=cast(pstart+namesoffset)

!fill in rest of export dir
    phdr.namerva:=dllnameoffset+virtoffset
    phdr.expaddressrva:=addrtableoffset+virtoffset
    phdr.namepointerrva:=nametableoffset+virtoffset
    phdr.ordtablerva:=ordtableoffset+virtoffset

    strcpy(pdllname,dllfilename)

!address table
    if nexports>maxexports then
        gerror("Too many exports - can't sort")
    fi

    sortexports(sortindex)

    for i to nexports do
        d:=exporttable[sortindex[i]].def
        sectionno:=getsectionno(d.segment)

        strcpy(pnames,d.name)
        pnametable^:=namesoffset+virtoffset
        ++pnametable
        namesoffset+:=strlen(d.name)+1
        pnames+:=strlen(d.name)+1

        paddrtable^:=d.offset+sectiontable[sectionno].virtoffset
        ++paddrtable
        pordtable^:=i-1
        ++pordtable
    od
end

function getexporttablesize:int=
    int size

    size:=exportdirrec.bytes
    size+:=nexports*4           !address table entries
    size+:=nexports*4           !name pointers
    size+:=nexports*2           !ordinal table

    size+:=strlen(dllfilename)+1
    for i to nexports do
        size+:=strlen(exporttable[i].def.name)+1
    od

    return size
end

proc newbasereloc(int addr, reltype)=
    ref basereloc p

    p:=pcm_allocz(basereloc.bytes)
    p.address:=addr
    p.reloctype:=reltype

    p.nextitem:=basereloclist

    basereloclist:=p
    ++nbaserelocs
    maxrelocaddr max:=addr

end

proc scanbaserelocs=
!go through all the relocs and build the block tables, and work out overall size
!   int maxaddr:=maxrelocaddr+4096
    int baseaddr,addr,nextblock
    ref basereloc p

    baseaddr:=0x1000
    nbaseblocks:=0

    repeat
        nextblock:=baseaddr+0x1000
        if nbaseblocks>=maxbaseblock then gerror("Too many blocks") fi
        ++nbaseblocks
        blockbases[nbaseblocks]:=baseaddr
        blockcounts[nbaseblocks]:=0


        p:=basereloclist
        while p do
            addr:=p.address
            if addr>=baseaddr and addr<nextblock then
                ++blockcounts[nbaseblocks]
            fi

            p:=p.nextitem
        od

        baseaddr:=nextblock
    until baseaddr>maxrelocaddr

    for i to nbaseblocks when blockcounts[i] do
        if blockcounts[i] iand 1 then
            ++blockcounts[i]
            ++blockpadding[i]
        fi
        blockbytes[i]:=blockcounts[i]*2+8
        basetablesize+:=blockbytes[i]
    od
end

proc writebasereloctable(ref byte pstart)=
    
    ref word32 p32
    ref word16 p16
    int baseaddr,addr,nextblock
    ref basereloc q

    p32:=cast(pstart)

    for i to nbaseblocks when blockcounts[i] do
        p32^:=blockbases[i]
        ++p32
        p32^:=blockbytes[i]
        ++p32
        p16:=cast(p32)

        q:=basereloclist
        baseaddr:=blockbases[i]
        nextblock:=baseaddr+4096

        while q do
            addr:=q.address
            if addr>=baseaddr and addr<nextblock then
                IF Q.RELOCTYPE=ADDR64_REL THEN
                    p16^:=addr-baseaddr+(q.reloctype=addr32_rel|3|10)<<12
                    ++p16
                FI
            fi
!
            q:=q.nextitem
        od
        if blockpadding[i] then p16++^:=0 fi

        p32:=cast(p16)

    od
end

proc sortexports([]int &sortindex)=
!sort exporttable by name. This is done by building a set of sorted indices into
!sortindex
    ref strec d,e
!First, store 1..nexports into sortindex
    for i to nexports do
        sortindex[i]:=i
    od

!do bubble sort for now
    int swapped

    repeat
        swapped:=0
        for i:=1 to nexports-1 do

            d:=exporttable[sortindex[i]].def
            e:=exporttable[sortindex[i+1]].def

            if strcmp(d.name, e.name)>0 then
                swapped:=1
                swap(sortindex[i], sortindex[i+1])
            fi
        od
    until not swapped

end
