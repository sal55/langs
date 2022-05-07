!Display contents of PE+ format EXE/DLL file (not OBJ/COFF which also uses PE+)

module disasm
module genlib

type imagefileheader=struct
    wt_word  machine
    wt_word  nsections
    wt_dword timedatestamp
    wt_dword symtaboffset
    wt_dword nsymbols
    wt_word  optheadersize
    wt_word  characteristics
end

type imagedir=struct
    wt_dword virtualaddr
    wt_dword size
end

type optionalheader=struct          !exe/dll only
    wt_word  magic
    byte     majorlv
    byte     minorlv
    wt_dword codesize
    wt_dword idatasize
    wt_dword zdatasize
    wt_dword entrypoint
    wt_dword codebase
    word64  imagebase
    wt_dword sectionalignment
    wt_dword filealignment
    wt_word  majorosv
    wt_word  minorosv
    wt_word  majorimagev
    wt_word  minorimagev
    wt_word  majorssv
    wt_word  minorssv
    wt_dword win32version
    wt_dword imagesize
    wt_dword headerssize
    wt_dword checksum
    wt_word  subsystem
    wt_word  dllcharacteristics
    word64   stackreserve
    word64   stackcommit
    word64   heapreserve
    word64   heapcommit
    wt_dword loaderflags
    wt_dword rvadims
    imagedir exporttable
    imagedir importtable
    imagedir resourcetable
    imagedir exceptiontable
    imagedir certtable
    imagedir basereloctable
    imagedir debug
    imagedir architecture
    imagedir globalptr
    imagedir tlstable
    imagedir loadconfigtable
    imagedir boundimport
    imagedir iat
    imagedir delayimportdescr
    imagedir clrheader
    imagedir reserved
end

type imagesectionheader=struct
    stringz*8 name
    union
        wt_dword physical_address
        wt_dword virtual_size
    end
    wt_dword virtual_address
    wt_dword rawdata_size
    wt_dword rawdata_offset
    wt_dword relocations_ptr
    wt_dword linenos_offset
    wt_word  nrelocs
    wt_word  nlinenos
    wt_dword characteristics
end

type imagesymbol=struct
    union
        stringz*8 shortname
        struct
            wt_dword short
            wt_dword long
        end
        word64 longname
    end
    wt_dword value
    wt_word  sectionno
    wt_word  symtype
    byte     storageclass
    byte     nauxsymbols
end

type importdirrec = struct
    wt_dword implookuprva
    wt_dword timedatestamp
    wt_dword fwdchain
    wt_dword namerva
    wt_dword impaddressrva
end

type exportdirrec = struct
    wt_dword exportflags
    wt_dword timedatestamp
    wt_word majorversion
    wt_word minorversion
    wt_dword namerva
    wt_dword ordinalbase
    wt_dword naddrtable
    wt_dword nnamepointers
    wt_dword expaddressrva
    wt_dword namepointerrva
    wt_dword ordtablerva
end

type relocrec = struct
    int32 virtualaddr
    int32 stindex
    int16 reloctype
end

var coffptr
var sectionptr
var sectionoffset
var nsections

var optheaderoffset
var optheadersize
var opthdrptr

var symtaboffset
var symtabptr
var nsymbols
var stringtable
var stringindextable
var nstrings
var pedata

var reloctypenames = (0:"Abs","Addr64","Addr32","Add32NB","Rel32","Rel32_1")
const maxsymbol=1000
var symbolnames=(0:)

var storageclassnames=[0:"Null", 2:"Extern", 3:"Static",5:"Extern2"]

sub start=

    infile:=""
    showdata:=1

    if ncmdparams>=1 then
        (params,cmdswitches):=parsecmdparams(cmdparams[1..ncmdparams])
        if params then
            infile:=addext(params[1],"exe")
        fi

        if "hdr" in cmdswitches or "h" in cmdswitches then showdata:=0 fi
    fi

    if infile="" then
        println "No filename given"
        stop 1
    fi

    if not checkfile(infile) then
        infile:=changeext(infile,"dll")
    fi

    println "Dumping",infile

    pedata:=readblockfile(infile)      ! returns byte pointer to exe file contents

    if pedata=0 then
        println "Can't open",changeext(infile,"exe"),"nor",changeext(infile,"dll")
        stop 1
    fi

    initgenstr()

    if pedata^='M' and (pedata+1)^='Z' then     !exe file
        genstrln("Executable file "+infile)
        peoffset:=makeref(pedata+0x3c,int32)^
        coffoffset:=peoffset+4
        peptr:=makeref(pedata+peoffset,int32)
        genstrln("PE Sig:"+tostr(peptr^,"m"))
    else
        genstrln("Object file "+infile)
        coffoffset:=0
    fi

    coffptr:=makeref(pedata+coffoffset,imagefileheader)

    genstrln("Coff header:     "+tostr(coffptr^))

    genstrln("Machine:         "+tostr(coffptr^.machine,"h2"))
    genstrln("Nsections:       "+tostr(coffptr^.nsections,"h2"))
    genstrln("Timestamp:       "+tostr(coffptr^.timedatestamp,"h4"))
    genstrln("Symtab offset:   "+tostr(coffptr^.symtaboffset))
    genstrln("Nsymbols:        "+tostr(coffptr^.nsymbols))
    genstrln("Opt Hdr size:    "+tostr(coffptr^.optheadersize))
    genstrln("Characteristics: "+tostr(coffptr^.characteristics,"b"))
    genline()

    optheaderoffset:=coffoffset+imagefileheader.bytes
    optheadersize:=coffptr^.optheadersize
    opthdrptr:=makeref(pedata+optheaderoffset,optionalheader)

    sectionoffset:=optheaderoffset+optheadersize
    sectionptr:=makeref(pedata+sectionoffset,imagesectionheader)
    nsections:=coffptr^.nsections

    showoptheader()

    genstrln("Sectionoffset: "+tostr(sectionoffset))
    genstrln("Sectionptr   : "+tostr(sectionptr))
    genstrln("No Sections  : "+tostr(nsections))
    genline()

    symtaboffset:=coffptr^.symtaboffset
    symtabptr:=makeref(pedata+symtaboffset,imagesymbol)
    nsymbols:=coffptr^.nsymbols

    showsections()
    showsectionrelocs()

    showimportdir()

    showexportdir()

    if showdata then
        showbasereloctable()
        showsectiondata()
    fi

    writestrfile("kkk1",ttdeststr)
    execwait(f"\m\med.bat kkk1",1)
end

sub showsections=
    genstrln("proc Section Headers")

    p:=sectionptr
    for i:=1 to nsections do
        genstrln("Section "+tostr(i)+":")

        genstrln("  "+p^.name)
        genstrln("  Virtual size:  "+tostr(p^.virtual_size,"h"))
        genstrln("  Virtual addr:  "+tostr(p^.virtual_address,"h"))
        genstrln("  Rawdata size:  "+tostr(p^.rawdata_size,"H"))
        genstrln("  Nrelocs:       "+tostr(p^.nrelocs))
        genstrln("  Rawdat offset: "+tostr(p^.rawdata_offset,"H"))
        ++p
        genline()
    od
end

sub showsectiondata=
    genstrln("proc Section Data")

    p:=sectionptr
    for i:=1 to nsections do
        genstrln("Section "+tostr(i)+": "+p^.name)

        q:=makeref(pedata+p^.rawdata_offset,byte)
        n:=p^.rawdata_size
        if p^.name=".text" then
            codelines:=decodeblock(q,n)
            forall line in codelines do
                genstrln(line)
            od
        else
            k:=0
            offset:=0
            genstr(tostr(offset,"6zh")+" ")
            str::=""
            for col to n do
                bb:=q^
                genstr(tostr(bb,"z2h")+" ")

                if bb in 32..127 then
                    str+:=bb
                else
                    str+:="."
                fi

                ++offset
                if ++k=16 or col=n then
                    if k<16 then
                        to 16-k do
                            genstr("    ")
                        od
                    fi
                    genstr("[")
                    genstr(str)
                    genstr("]")
                    str::=""

                    genline()
                    genstr(tostr(offset,"6zh")+" ")
                    k:=0
                fi
                ++q
            od
            genline()
        fi

        ++p
        genline()
    od
end

sub showsectionrelocs=
    genstrln("proc Section Relocs")

    p:=sectionptr
    for i:=1 to nsections do
        genstrln("Section "+tostr(i)+": "+p^.name)

        q:=makeref(pedata+p^.relocations_ptr,relocrec)
        n:=p^.nrelocs
        genstrln("NRELOCS:"+TOSTR(N))
        for j to n do
            genstrln(sfprint("#: Type:# Virtualaddr:# Stindex:# (#)",
                j,reloctypenames[q^.reloctype]:"8",q^.virtualaddr:"6", q^.stindex:"4",
                symbolnames[q^.stindex]))
            ++q
        od
        genline()
        ++p
    od
end

sub showoptheader=
    genstrln("proc Optional Header")
    genstrln("Opt header size:  "+tostr(optheadersize))
    genstrln("Opt hdr rec size: "+tostr(optionalheader.bytes))

    p:=opthdrptr
    genline()
    genstrln("Magic:            "+tostr(p^.magic,"h"))
    genstrln("Link version:     "+tostr(p^.majorlv)+"."+tostr(p^.minorlv))
    genstrln("Code size:        "+tostr(p^.codesize))
    genstrln("Idata size:       "+tostr(p^.idatasize))
    genstrln("Zdata size:       "+tostr(p^.zdatasize))
    genstrln("Entry point:      "+tostr(p^.entrypoint)+" "+tostr(p^.entrypoint,"h")+ \
            " in data:"+tostr(p^.entrypoint-4096))

    genstrln("Code base:        "+tostr(p^.codebase))
    genstrln("Image base:       "+tostr(p^.imagebase)+" "+tostr(p^.imagebase,"h"))
    genstrln("Section align:    "+tostr(p^.sectionalignment))
    genstrln("File align:       "+tostr(p^.filealignment))


    genstrln("OS version:       "+tostr(p^.majorosv)+"."+tostr(p^.minorosv))
    genstrln("Image version:    "+tostr(p^.majorimagev)+"."+tostr(p^.minorimagev))
    genstrln("Subsys version:   "+tostr(p^.majorssv)+"."+tostr(p^.minorssv))
    genstrln("Win32 version:    "+tostr(p^.win32version))

    genstrln("Image size:       "+tostr(p^.imagesize))
    genstrln("Headers size:     "+tostr(p^.headerssize))
    genstrln("Checksum:         "+tostr(p^.checksum))
    genstrln("Subsystem:        "+tostr(p^.subsystem))
    genstrln("DLL chars:        "+tostr(p^.dllcharacteristics))
    genstrln("Stack reserve:    "+tostr(p^.stackreserve))
    genstrln("Stack commit:     "+tostr(p^.stackcommit))
    genstrln("Heap reserve:     "+tostr(p^.heapreserve))
    genstrln("Heap commit:      "+tostr(p^.heapcommit))
    genstrln("Loader flags:     "+tostr(p^.loaderflags))
    genstrln("RVA Dims:         "+tostr(p^.rvadims))
    genline()
    genstrln("Export table:     "+tostr(p^.exporttable.virtualaddr,"h")+" size:"+tostr(p^.exporttable.size))
    genstrln("Import table:     "+tostr(p^.importtable.virtualaddr,"h")+" size:"+tostr(p^.importtable.size))
    genstrln("Resource table:   "+tostr(p^.resourcetable.virtualaddr,"h")+" size:"+tostr(p^.resourcetable.size))
    genstrln("Exception table:  "+tostr(p^.exceptiontable.virtualaddr,"H")+" size:"+tostr(p^.exceptiontable.size))
    genstrln("Cert table:       "+tostr(p^.certtable))
    genstrln("Base reloc table: "+tostr(p^.basereloctable,"H"))
    genstrln("Debug:            "+tostr(p^.debug))
    genstrln("Architecture:     "+tostr(p^.architecture))
    genstrln("Global ptr:       "+tostr(p^.globalptr))
    genstrln("TLS table:        "+tostr(p^.tlstable))
    genstrln("Load config:      "+tostr(p^.loadconfigtable))
    genstrln("Bound import:     "+tostr(p^.boundimport))
    genstrln("IAT:              "+tostr(p^.iat,"h"))
    genstrln("Delayimp descr:   "+tostr(p^.delayimportdescr))
    genstrln("CLR Header:       "+tostr(p^.clrheader))
    genstrln("Reserved:         "+tostr(p^.reserved))

    genline()
end

fun getstring(p)=
!p is a ref byte pointing to zero-terminated string
!return the string
    s::=""
    while p^ do
        s+:=p++^
    od
    return s
end

fun getimportname(p)=
!p is a byte pointer to a 16-bit hint then zero-terminated name

    return (makeref(p,word16)^,getstring(p+2))
end

sub showlookuptable(p,rawoffset,virtaddr)=
!p points to a zero-terminated list of 64-bit values
!assume top bit of each is 0 (uses name not ordinal)


    while p^ do
        X:=INT(P^)
        IF X>10000 THEN
            phint:=makeref(pedata+rawoffset+((int(p^) iand 0xFFFF'FFFF)-virtaddr),byte)
            (hint,name):=getimportname(phint)
            genstrln("      Import:"+tostr(p^,"6h")+tostr(hint,"5h")+" "+name)
        FI
        ++p
    od
end

sub showimportdir=

    genstrln("Import Directory")
    genline()

    rawoffset:=virtaddr:=0
    ptr:=getidataptr(rawoffset,virtaddr)
    if ptr=nil then
        CPL "Can't find Import Directory"
        return
    fi

    p:=makeref(pedata+rawoffset,importdirrec)

    N:=0
    while p^.implookuprva do
        genstrln("Entry:  "+tostr(makeref(p,byte)-pedata-rawoffset+virtaddr,"h"))
        genstrln("  Lookup RVA:       "+tostr(p^.implookuprva,"h"))
        genstrln("  Time Date Stamp:  "+tostr(p^.timedatestamp,"h"))
        genstrln("  Fwd Chain:        "+tostr(p^.fwdchain,"h"))
        genstrln("  Name RVA:         "+tostr(p^.namerva,"h"))
        pname:=pedata+rawoffset+(p^.namerva-virtaddr)
        genstrln("  Name:             "+getstring(pname))
        genstrln("  Import Addr RVA:  "+tostr(p^.impaddressrva,"h"))

        ptable:=pedata+rawoffset+(p^.implookuprva-virtaddr)
        showlookuptable(makeref(ptable,word64), rawoffset,virtaddr)
        genline()
        ++N
        ++p
    od


    println N,"IMPORTDIRS"
!   genstrln("Entry (all zeros):  "+tostr(makeref(p,byte)-pedata-rawoffset+virtaddr,"h"))
!   ++p
!   genstrln("Next:               "+tostr(makeref(p,byte)-pedata-rawoffset+virtaddr,"h"))
    genline()

end

sub showexportdir=

    genline()
    genstrln("Export Directory")
    genline()

    rawoffset:=virtaddr:=0
    ptr:=getedataptr(rawoffset,virtaddr)

    if ptr=nil then
        CPL "Can't find Export Directory"
        return
    fi

    p:=makeref(pedata+rawoffset,exportdirrec)

    offset:=virtaddr-rawoffset

    pnameptr:=makeref(pedata+rawoffset+(p^.namepointerrva-virtaddr),word32)
    paddrptr:=makeref(pedata+rawoffset+(p^.expaddressrva-virtaddr),word32)
    pordptr:=makeref(pedata+rawoffset+(p^.ordtablerva-virtaddr),word16)

    a:=b:=0
    pcode:=findtable(".text",0,a,b)
    if pcode=nil then println "Can't find .text section"; return fi
    codevirtaddr:=pcode^.virtual_address
    codevirtsize:=pcode^.virtual_size

    to p^.nnamepointers do
        genstr(tostr(pordptr^,"4"))
        genstr(" ")
        ++pordptr
        (addr, ss):=fixexportaddr(paddrptr^)
        genstr(tostr(addr,"z8h"))
        genstr(" ")
        genstr(tostr(addr,"8"))
        genstr(" ")
        genstr(ss)
        ++paddrptr

        genstrln(" "+getstring(makeref(pedata+pnameptr^-offset,byte)))
        ++pnameptr
    od

    genline()
end

fun fixexportaddr(addr)=
!search sections to see where
    p:=sectionptr

    for i:=1 to nsections do
        rawoffset:=p^.rawdata_offset
        virtaddr:=p^.virtual_address
        virtsize:=p^.virtual_size

        a:=addr-virtaddr            !rel to start of section
        if a in 0..virtsize then
            return (a, (p^.name=".text"|"Fun"|"Var"))
        fi
        ++p
    od

    return (0,"?")

end

fun findtable(sectionname,tableaddr, &rawoffset, &virtaddr)=
    p:=sectionptr

    for i:=1 to nsections do
        if p^.name=sectionname then
            rawoffset:=p^.rawdata_offset
            virtaddr:=p^.virtual_address
            return p
        fi
        ++p
    od

!no dedicated table; look for section that might contain tableaddr
    p:=sectionptr
    idataptr:=nil

    for i:=1 to nsections do
        rawoffset:=p^.rawdata_offset
        virtaddr:=p^.virtual_address
        virtsize:=p^.virtual_size

        if tableaddr>=virtaddr and tableaddr<(virtaddr+virtsize) then
            xoffset:=tableaddr-virtaddr
            rawoffset+:=xoffset
            virtaddr+:=xoffset
            return p
        fi
        ++p
    od

    return nil
end

fun getedataptr(&rawoffset, &virtaddr)=
    return findtable(".edata",opthdrptr^.exporttable.virtualaddr,rawoffset,virtaddr)
end

fun getidataptr(&rawoffset, &virtaddr)=
    return findtable(".idata",opthdrptr^.importtable.virtualaddr,rawoffset,virtaddr)
end

sub showbasereloctable=
    p:=opthdrptr
    offset:=p^.basereloctable.virtualaddr
    size:=p^.basereloctable.size
    if offset=0 then return fi

    genstrln("Base Reloc Table "+tostr(size))

    rawoffset:=virtaddr:=0
    ptr:=findtable(".reloc",offset,rawoffset,virtaddr)

    p:=makeref(pedata+rawoffset,word32)


    repeat
        blocksize:=(p+1)^
        n:=(blocksize-8)%2
        genstrln("    Block "+tostr(p^,"h")+" Size:"+tostr(blocksize)+" N:"+tostr(n))

        base:=p^

!       println p^:"h", =blocksize
        q:=makeref(p+2,word16)
        to n do
            offset:=q^.[0..11]
            genstrln(tostr(q^>>12,"2")+":"+tostr(offset,"z4h")+" "+tostr(base+offset,"z6h")+\
                " "+tostr(base+offset-4096,"6"))
            ++q
        od
        genline()

        p:=makeref(makeref(p,byte)+blocksize,word32)
        size-:=blocksize
    until size<=0
end
