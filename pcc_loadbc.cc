#include "pcc_hdr.h"
#include "pcc_loadbc.cl"

#define zeof    255     !EOF marker

global function int load_bc_file(uchar* filename) =
    int s,t,tt;

    strcpy(mainmodulename,getmainmodule(filename));     !get basic module name

    pcl_init();

    //doztests(filename);
    if (!readbc(filename)) then
        return 0;
    fi

    pcl_init_ttmap();
    pcl_initusertypes();

    os_initdllmodules();
    os_initdllfunctions();
    os_initwindows();

//  s=tr64;
//  t=tlist;
//  tt=ttmap[s][t];
//  PRINTF("S=%s T=%s TTcode=%d TTname=%s\n",ttname[s],ttname[t],tt,ttmapnames[tt]);
//  EXIT(0);

    fixup_pcdata();
    return 1;
end

function byte* readbcfile(char *filename) {
! Read bc-file into memory, return pointer to start of image (null in case of error).
! Will add kendmodule opcode to the end, followed by zeof code for good measure
    byte *m;

    m:=readfile(filename);
    if (m=NULL || rfsize=0) then
        return NULL;
    fi
    *(m+rfsize):=0;     !kendmodule
    *(m+rfsize+1):=zeof;
    return m;
end

function int readbc(uchar* filename) {
//union {i32 a; i64 aa; r64 x; char *s;} u;
    byte *s;
    int t,length,version;
    int i,n,dir,a,b,c,d;
    uchar* str;
    typerec ttrec;
    fieldrec frec;
    linerec lrec;
    procrec prec;
    dllprocrec dprec;
    staticrec srec;
    varrec vrec;
    commentrec crec;
    genfieldrec grec;
    genfielddatarec gdrec;
    i64 aa;
    char file2[300];
    char name[100];

    s:=readbcfile(filename);
    if (s=NULL) then

!try from m directory
        if (filename[0]='/' || filename[0]='\\' || filename[1]=':') then    !includes full path (or assume so)
            return 0;
        fi
        strcpy(file2,"\\m\\");
        strcat(file2,filename);
        filename:=file2;
        s:=readbcfile(filename);
        if (s=NULL) then
            return 0;
        fi
    fi

    a:=*s++;
    b:=*s++;
    c:=*s++;
    d:=*s++;
    if (a<>'B' || b<>'C') then
        cpl "BC: bad sig"
        return 0;
    fi

    while (1) do
        dir:=readzint(&s);

        switch (dir) {
        when kkcompiler then
            str:=readzstring(&s,&length);

        when kkpclversion then
            version:=readzint(&s);
            if (version<>pclversion) then
                cpl "Module: %d Interpreter: %d\n",version,pclversion
                abortprogram("PCL Version mismatch");
            fi

        when kkmodulename then
            str:=readzstring(&s,&length);
        when kkmoduletable then
            nmodules:=readzint(&s);
            modulenames:=alloctable(nmodules,sizeof(uchar*));
            FOR(i,1,nmodules) do
                modulenames[i]:=pcm_copyheapstring(readzstring(&s,&length));
            od

        when kkmodulefiletable then
            n:=readzint(&s);        !should be same as nmodules
            modulefiles:=alloctable(n,sizeof(uchar*));
            FOR(i,1,n) do
                modulefiles[i]:=pcm_copyheapstring(readzstring(&s,&length));
            od

        when kkmoduleseq then
            n:=readzint(&s);
            FOR(i,1,n) do
                readzint(&s);
            od

        when kkdlltable then
            ndlllibs:=readzint(&s);
            dlltable:=alloctable(ndlllibs,sizeof(uchar*));
            FOR(i,1,ndlllibs) do
                dlltable[i]:=pcm_copyheapstring(readzstring(&s,&length));
            od

        when kksourcefiletable then
            nsourcefiles:=readzint(&s);
            sourcefiletable:=alloctable(nsourcefiles,sizeof(uchar*));
            FOR(i,1,nsourcefiles) do
                sourcefiletable[i]:=pcm_copyheapstring(readzstring(&s,&length));
            od

        when kkstringtable then
            nstringtable:=readzint(&s);
            stringtable:=alloctable(nstringtable,sizeof(uchar*));
            stringlengths:=alloctable(nstringtable,sizeof(int));
            FOR(i,1,nstringtable) do
                str:=readzstring(&s,&length);
                stringtable[i]:=pcm_copyheapstring(str);
                stringlengths[i]:=length;
            od

        when kkdinttable then
            ndinttable:=readzint(&s);
            dinttable:=alloctable(ndinttable,sizeof(i64));
            FOR(i,1,ndinttable) do
                dinttable[i]:=readzdint(&s);
            od

        when kkrealtable then
            nrealtable:=readzint(&s);
            realtable:=alloctable(nrealtable,sizeof(r64));
            FOR(i,1,nrealtable) do
                realtable[i]:=readzreal(&s);
            od

        when kkrangetable then
            nrangetable:=readzint(&s);
            rangetable:=alloctable(nrangetable,sizeof(i64));
            FOR(i,1,nrangetable) do
                aa:=readzdint(&s);
                rangetable[i]:=aa;
            od

        when kkstatictable then
            nstatics:=nstatictable:=readzint(&s);
            statictable:=alloctable(nstatictable,sizeof(staticrec));
            staticvars:=alloctable(nstatics,sizeof(varrec));
            FOR(i,1,nstatictable) do
                a:=readzint(&s);            //moduleno
                str:=readzstring(&s,&length);       //name
                c:=readzint(&s);            //hasinit
                d:=readzint(&s);            //initvalue
                srec.moduleno:=a;
                srec.name:=pcm_copyheapstring(str);
                statictable[i]:=srec;
                pcm_clearmem(&vrec,sizeof(vrec));   //set to void
                if (c) then
                    vrec.tag:=ti32;
                    vrec.value:=d;
                fi
                staticvars[i]:=vrec;
            od

        when kkproctable then
            nproctable:=readzint(&s);
            proctable:=alloctable(nproctable,sizeof(procrec));
            FOR(i,1,nproctable) do
                prec.moduleno:=readzint(&s);
                prec.name:=pcm_copyheapstring(readzstring(&s,&length));
                prec.pcindx:=readzint(&s);
                str:=readzstring(&s,&length);
                if (length) then
                    prec.metadata:=pcm_copyheapstring(str);
                else
                    prec.metadata:=NULL;
                fi
                proctable[i]:=prec;
            od

        when kkdllproctable then
            ndllproctable:=readzint(&s);
            dllproctable:=alloctable(ndllproctable,sizeof(dllprocrec));
            FOR(i,1,ndllproctable) do
                dprec.moduleno:=readzint(&s);
                dprec.name:=pcm_copyheapstring(readzstring(&s,&length));
                dprec.address:=0;
                dllproctable[i]:=dprec;
            od

        when kktypetable then           !global table in two parts: std types, plus these user types
            ntypetable:=readzint(&s);       !no. of user types across all modules
            typetable:=alloctable(ntypetable,sizeof(typerec));

!NOTES: (assume there are 41 std types)
!user types are numbered 42 to 41+N (where there are N=ntypetable user types across all modules)
!these are stored in positions 1..N in the typetable
!when copies to the TT[] tables, then locations 1..41 are already filled
!Locations 42+ are filled from entries 1+ in the typetables.
!The .typeno gives the true typecode (42+) of each entry

            FOR(i,1,ntypetable) do
                ttrec.moduleno:=readzint(&s);
                t:=ttrec.typeno:=readzint(&s);
                ttrec.name:=readzstring(&s,&length);
                ttrec.basetype:=readzint(&s);
                ttrec.target:=readzint(&s);
                ttrec.lbound:=readzint(&s);
                ttrec.length:=readzint(&s);
                ttrec.size:=readzint(&s);
                ttrec.nallfields:=readzint(&s);
                typetable[i]:=ttrec;
            od

        when kkfieldtable then
            nfieldtable:=readzint(&s);
            fieldtable:=alloctable(nfieldtable,sizeof(fieldrec));

            FOR(i,1,nfieldtable) do
                frec.rectype:=readzint(&s);
                frec.name:=readzstring(&s,&length);
                frec.fieldtype:=readzint(&s);
                frec.offset:=readzint(&s);
                fieldtable[i]:=frec;
            od

        when kklinetable then
            nlinetable:=readzint(&s);
            linetable:=alloctable(nlinetable,sizeof(linerec));
            FOR(i,1,nlinetable) do
                lrec.file:=readzint(&s);
                lrec.line:=readzint(&s);
                lrec.pcindx:=readzint(&s);
                linetable[i]:=lrec;
            od

        when kkcommenttable then
            ncommenttable:=readzint(&s);
            commenttable:=alloctable(ncommenttable,sizeof(commentrec));
            FOR(i,1,ncommenttable) do
                crec.comment:=pcm_copyheapstring(readzstring(&s,&length));
                crec.pcindx:=readzint(&s);
                commenttable[i]:=crec;
            od

        when kkpcdata then
            npcdata:=readzint(&s);
            pcdata:=alloctable(npcdata,sizeof(int));
            FOR(i,1,npcdata) do
                pcdata[i]:=readzint(&s);
            od

        when kkgenfieldtable then
            ngenfieldtable:=readzint(&s);
            genfieldtable:=alloctable(ngenfieldtable,sizeof(genfieldrec));
            FOR(i,1,ngenfieldtable) do
                grec.name:=pcm_copyheapstring(readzstring(&s,&length));
                grec.dataindex:=readzint(&s);
                grec.datalength:=readzint(&s);
                genfieldtable[i]:=grec;
            od

        when kkgenfielddata then
            ngenfielddata:=readzint(&s);
            genfielddata:=alloctable(ngenfielddata,sizeof(genfielddatarec));
            FOR(i,1,ngenfielddata) do
                gdrec.fieldindex:=readzint(&s);
                gdrec.recordtype:=readzint(&s);
                gdrec.fieldtype:=readzint(&s);
                gdrec.offset:=readzint(&s);
                genfielddata[i]:=gdrec;
            od

        when kkend then
            return 1;
        elsesw
            PRINTF("Unknown dir: %s\n",kkdirnames[dir]);
            return 0;
        endsw

    od

    return 1;
end

function uchar* getmainmodule(uchar* file)= !GETMAINMODULE
!similar to function of same name in load_pc_file()
!file should be the name of a .bc file
    static char str[100];

    strcpy(str,extractbasefile(file));      !base modulename
    convlcstring(str);
    return str;
end
