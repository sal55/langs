!Needs revising to match unlimited ss_symboltable size used for exe
!and also unlimited strings

int symtaboffset

ref byte datastart
ref byte dataptr

[0..13'000]imagesymbol symboltable

int nsymbols

int stoffset=0              !usually +7 to convert ss_symboltable indices to symboltable

const maxstring=5000
[maxstring]ichar stringtable
[maxstring]int stringlengths
int nextstringoffset=0
int nstrings=0

export proc writess(ichar outfile)=
    writecoff(outfile)
end

proc writerecord(ref void r, int length)=
    memcpy(dataptr,r,length)
    dataptr+:=length
end

proc writerelocs(ref relocrec r,int nrelocs)=
    static coffrelocrec s
    ref strec d

    return when nrelocs=0

    while r do
        case r.reloctype
        when addr32_rel, addr64_rel then        !change to section entry
            d:=ss_symboltable^[r.stindex]

            case d.segment
            when zdata_seg then s.stindex:=2
            when idata_seg then s.stindex:=4
            when code_seg then s.stindex:=6
            when 0 then                         !external; leave stindex pointing to symbol
                s.stindex:=r.stindex+stoffset
            else
                gerror("wrelocs/bad seg")
            esac

        else
            s.stindex:=r.stindex+stoffset
        esac

        s.reloctype:=r.reloctype
        case highmem
        when 0 then
        when 2 then
            IF R.RELOCTYPE=ADDR32_REL THEN
                S.RELOCTYPE:=REL32_REL
                R.RELOCTYPE:=REL32_REL
            FI
        else
            gerror("OBJ/highmem 1?")
        esac

        s.virtualaddr:=r.offset


        memcpy(dataptr,&s,s.bytes)
        dataptr+:=s.bytes

        r:=r.nextreloc
    od
end

proc writedata(ref dbuffer data)=
    memcpy(dataptr, bufferelemptr(data,0), bufferlength(data))
    dataptr+:=bufferlength(data)
end

proc writesymboltable=
    int i
    for i:=1 to nsymbols do
        writerecord(&symboltable[i],imagesymbol.bytes)
    od
end

proc writestringtable=
!should immediately follow symboltable
    ref int32 p
    int i,n

    p:=cast(dataptr)
    p^:=nextstringoffset
    dataptr+:=4

    for i to nstrings do
        n:=stringlengths[i]+1
        memcpy(dataptr,stringtable[i],n)
        dataptr+:=n
    od
end

function makesymbol(ichar name,int namelen=0, value=0, sectionno=0,symtype=0,storage=0,naux=0)ref imagesymbol=
    static imagesymbol r
    int length

    if namelen=0 then namelen:=strlen(name) fi

    if namelen<8 then
        strcpy(&r.shortname[1],name)
    elsif namelen=8 then
        memcpy(&r.shortname[1],name,namelen)
    else
        r.shortx:=0
        r.longx:=addstringentry(name,namelen)
    fi
    r.value:=value
    r.sectionno:=sectionno
    r.symtype:=symtype
    r.storageclass:=storage
    r.nauxsymbols:=naux
    return &r
end

proc addsymbol(ref imagesymbol r)=
    if nsymbols>=symboltable.upb then
        gerror("as:Too many symbols")
    fi
    memcpy(&symboltable[++nsymbols],r,imagesymbol.bytes)
end

proc initsymboltable(ichar filename)=
!add first few special symbols to coff symboltable

    nsymbols:=0

    addsymbol(makesymbol(".file",storage:103, sectionno:-2,naux:1))
    addsymbol(strtoaux(filename))

    addsymbol(makesymbol(".bss", storage:3, sectionno:1, naux:1))
    addsymbol(cast(sectiontoaux(nil, 0)))

    addsymbol(makesymbol(".data", storage:3, sectionno:2, naux:1))
    addsymbol(cast(sectiontoaux(ss_idata, ss_nidatarelocs)))

    addsymbol(makesymbol(".text", storage:3, sectionno:3, naux:1))
    addsymbol(cast(sectiontoaux(ss_code, ss_ncoderelocs)))
end

function strtoaux(ref char s)ref imagesymbol=
!turn string s into 18-byte imagesymbol record
    static imagesymbol r
    ref byte p:=cast(&r)
    int n

    memset(p,0,r.bytes)
!   clear p^

    n:=0
    while s^<>0 and n<r.bytes do
        p++^:=s++^
        ++n
    od

    return &r
end

function sectiontoaux(ref dbuffer data, int nrelocs)ref auxsectionrec=
!!turn segment into into aux section/reloc entry for symboltable
    static auxsectionrec r

    clear r

    if data=nil then            !zdata
        r.length:=ss_zdatalen
    else
        r.length:=bufferlength(data)

    fi
    r.nrelocs:=nrelocs
    return &r
end

function addstringentry(ichar s, int length)int=
!assume s is longer than 8 chars
!add string table entry, return offset to string, as it would be in the coff string table
!assume s in stable memory so doesn't need copying
    int offset

    offset:=nextstringoffset
    if nstrings>maxstring then
        gerror("W:too many strings")
    fi
    stringtable[++nstrings]:=s
    stringlengths[nstrings]:=length

    nextstringoffset+:=length+1

    return offset
end

proc convertsymboltable=
!scan ss_symboltable and generate coff symboltable equivalents
    ref strec s
    ichar name
    int i,sect, scope

    stoffset:=nsymbols-1

    nstrings:=0
    nextstringoffset:=4

    for i to ss_nsymbols do
        s:=ss_symboltable^[i]

        name:=s.name

        case s.segment
        when zdata_seg then sect:=1
        when idata_seg then sect:=2
        when code_seg then sect:=3
        else sect:=0
        esac

        case s.symbol
        when fwdlocalsym,localsym then
            scope:=3
        when importedsym,exportedsym then
            scope:=2
        else
            scope:=0
        esac

        addsymbol(makesymbol(s.name,s.namelen,sectionno:sect, storage:scope, value:s.offset))

    od
end

proc writecoff(ichar outfile)=
    imagefileheader header
    imagesectionheader zsection, isection, csection
    int offset
    int64 aa

    clear header
    clear zsection
    clear isection
    clear csection

    header.machine:=0x8664
    header.nsections:=3

    strcpy(&zsection.name[1],".bss")
    zsection.rawdata_size:=ss_zdatalen

    zsection.characteristics:=0xC040'0080

    if ss_nidatarelocs>=65536 or ss_ncoderelocs>=65536 then
        gerror("Too many relocs (exceeds 16-bit field)")
    fi

    strcpy(&isection.name[1],".data")
    isection.rawdata_size:=bufferlength(ss_idata)
    isection.nrelocs:=ss_nidatarelocs

    isection.characteristics:=0xC050'0040

    strcpy(&csection.name[1],".text")
    csection.rawdata_size:=bufferlength(ss_code)
    csection.nrelocs:=ss_ncoderelocs

    csection.characteristics:=0x6050'0020

    initsymboltable(outfile)

    convertsymboltable()

    offset:=imagefileheader.bytes

    offset+:=imagesectionheader.bytes*3

    if isection.nrelocs then
        isection.relocations_ptr:=offset
        offset+:=isection.nrelocs*coffrelocrec.bytes
    fi

    if csection.nrelocs then
        csection.relocations_ptr:=offset
        offset+:=csection.nrelocs*coffrelocrec.bytes
    fi

    isection.rawdata_offset:=offset
    offset+:=isection.rawdata_size

    csection.rawdata_offset:=offset
    offset+:=csection.rawdata_size

!create symbol table and string table

    header.symtaboffset:=offset
    offset+:=nsymbols*imagesymbol.bytes
    header.nsymbols:=nsymbols

    offset+:=nextstringoffset

!Allocate data block in memory for coff image
    datastart:=dataptr:=malloc(offset)

    writerecord(&header,header.bytes)
    writerecord(&zsection,zsection.bytes)

    writerecord(&isection,isection.bytes)
    writerecord(&csection,csection.bytes)
    writerelocs(ss_idatarelocs,ss_nidatarelocs)
    writerelocs(ss_coderelocs,ss_ncoderelocs)

    writedata(ss_idata)
    writedata(ss_code)

    writesymboltable()
    writestringtable()

    if fverbose then
        println "Writing file:",outfile
    fi
    writefile(outfile,datastart,dataptr-datastart)

end

