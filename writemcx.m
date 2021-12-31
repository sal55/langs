! Write MCX file (externally known as LIB file with .lib extension)

int nsymimports=0, nsymexports=0

global proc writemcx(ichar filename)=
    filehandle f
    int n

    ss_zdatalen:=roundtoblock(ss_zdatalen, 8)

    roundsegment(ss_code,8,0x90)
    roundsegment(ss_idata,8,0)

    f:=fopen(filename, "wb")
    outword32(f, mcxsig)

    outbyte(f, version_dir)
    outstring(f,"0.1234")

    scansymbols()
    writerelocs(f)

    outbyte(f, zdata_dir)
    outword32(f,ss_zdatalen)

    outbyte(f, code_dir)
    outword32(f, n:=bufferlength(ss_code))
    outblock(f, bufferelemptr(ss_code,0), n)

    outbyte(f, idata_dir)
    outword32(f, n:=bufferlength(ss_idata))

    outblock(f, bufferelemptr(ss_idata,0), n)

    int ndlls:=0, nlibs:=0
    for i to nplibfiles do
        if plibtypes[i]='D' then ++ndlls else ++nlibs fi
    od

    outbyte(f, dlls_dir)
    outword32(f, ndlls)
    for i to nplibfiles when plibtypes[i]='D' do
        outstring(f, plibfiles[i])
    od

    outbyte(f, libs_dir)
    outword32(f, nlibs)
    for i to nplibfiles when plibtypes[i]='L' do
        outstring(f, plibfiles[i])
    od

    writesymbols(f)

    outbyte(f,end_dir)
    fclose(f)
end

proc writerelocs(filehandle f)=
    ref relocrec oldr
    mcxreloc newr
    int n,count
    psymbol d
    ref u64 baseptr64
    ref u32 baseptr32@baseptr64

    outbyte(f, reloc_dir)
    outword32(f, n:=ss_nidatarelocs+ss_ncoderelocs)

    count:=0

    for i in code_seg..idata_seg do
        oldr:=(i=code_seg|ss_idatarelocs|ss_coderelocs)

        while oldr, oldr:=oldr.nextreloc do
            ++count
            clear newr

            newr.offset:=oldr.offset
            newr.segment:=(i=code_seg|idata_seg|code_seg)

            d:=ss_symboltable[oldr.stindex]

            case oldr.reloctype
            when rel32_rel then
                if d.isimported then
                    newr.stindex:=d.impindex
                    newr.reloctype:=imprel32_rel
                else
                    axerror("rel32/rel not imported")
                fi
            when addr32_rel, addr64_rel then
                if d.isimported then
                    newr.reloctype:=(oldr.reloctype=addr32_rel|impabs32_rel|impabs64_rel)
                    newr.stindex:=d.impindex
                else
                    if oldr.reloctype=addr32_rel then
                        newr.reloctype:=locabs32_rel
                    else
                        newr.reloctype:=locabs64_rel
                    fi
                    newr.targetsegment:=d.segment
                fi
            else
                axerror("reloc?")
            esac

            outblock(f, &newr, newr.bytes)

        od
    od
end

proc scansymbols=
    psymbol d
    for i:=1 to ss_nsymbols do
        d:=ss_symboltable[i]
        if d.isexported then d.expindex:=++nsymexports fi
        if d.isimported then d.impindex:=++nsymimports fi
    od
end

proc writesymbols(filehandle f)=
    psymbol d
    u64 epoffset:=-1
    int n
    ichar name

    outbyte(f, importsymbols_dir)
    outword32(f, nsymimports)

    for i to ss_nsymbols when ss_symboltable[i].impindex do
        outstring(f,ss_symboltable[i].name)
    od

    outbyte(f, exportsymbols_dir)
    outword32(f, nsymexports)

    for i to ss_nsymbols do
        d:=ss_symboltable[i]
        if d.expindex then
            name:=getbasename(d.name)
            if epoffset=-1 and (eqstring(name,"start") or eqstring(name,"main")) then
                epoffset:=d.offset
            fi
            outstring(f,name)
        fi
    od

    outbyte(f, exportsegs_dir)
    outword32(f, nsymexports)
    for i to ss_nsymbols do
        d:=ss_symboltable[i]
        if d.expindex then
            outbyte(f,d.segment)
        fi
    od

    outbyte(f, exportoffsets_dir)
    outword32(f, nsymexports)
    for i to ss_nsymbols do
        d:=ss_symboltable[i]
        if d.expindex then
            outword32(f,d.offset)
        fi
    od

    outbyte(f,entry_dir)        !must be present; writes 0xFFFFFFFF when no entry point
    outword32(f,epoffset)
end

proc roundsegment(ref dbuffer p, int align, value)=
    int length:=bufferlength(p)
    int newlength:=roundtoblock(length, align)

    buffercheck(p, align)

    to newlength-length do
        p.pcurr++^:=value
    od
end
