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
        if length.odd then ++length fi       !keep even
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
        pdir^.implookuprva:=dlltable[i].nametableoffset
        pdir^.impaddressrva:=dlltable[i].addrtableoffset
        pdir^.namerva:=dlltable[i].dllnameoffset
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
        thunkptr++^:=0x48
        thunkptr++^:=0xFF
        thunkptr++^:=0x24
        thunkptr++^:=0x25
        thunkaddr:=imagebase+importtable[i].iatoffset
        (ref int32(thunkptr)^:=thunkaddr)

        thunkptr+:=4
    od
end
