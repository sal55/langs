// Launch MX-format executable file

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <ctype.h>

typedef short           i16;
typedef int             i32;
typedef long long int   i64;

typedef unsigned char   byte;

typedef unsigned short          u16;
typedef unsigned int            u32;
typedef unsigned long long int  u64;

#define mcxsig ('M' + ('C'<<8) + ('X'<<16) + (26<<24))

enum {
    pad_dir = 0,
    version_dir,
    code_dir,
    idata_dir,
    zdata_dir,
    reloc_dir,
    dlls_dir,
    libs_dir,
    importsymbols_dir,
    exportsymbols_dir,
    exportsegs_dir,
    exportoffsets_dir,
    entry_dir,
    end_dir
};

enum {
    no_rel = 0,
    locabs32_rel,
    locabs64_rel,
    impabs32_rel,
    impabs64_rel,
    imprel32_rel
};

enum {
    code_seg = 1,
    idata_seg,
    zdata_seg,
    rodata_seg
};

typedef struct {
    u32 offset;
    union {
        u16 stindex;
        byte targetsegment;
    };
    byte segment;
    byte reloctype;
} mcxreloc;

typedef struct {
    char* version;

    i64 codesize;
    i64 idatasize;
    i64 zdatasize;

    i64 nrelocs;
    i64 ndlllibs;
    i64 nlibs;
    i64 nimports;
    i64 nexports;

    byte* codeptr;
    byte* idataptr;

    mcxreloc*   reloctable;
    char**      dllnames;
    char**      libnames;
    char**      importnames;
    char**      exports;
    byte*       exportsegs;
    u64*        exportoffsets;
    
    u64         entryoffset;

    byte*       zdataptr;
    i64         codexsize;
    u64*        exportaddr;
    i16*        importxreftable;

    char*       filespec;
    char*       libname;
    byte*       entryaddr;
    i64         libno;
} librec;

enum {maxdlls =     20};
enum {maxlibs =     20};
enum {maxsymbols =  3000};

extern char*    dllnametable[];
extern u64      dllinsttable[];
extern i64      ndlllibs;

extern char*    libnametable[];
extern librec*  libtable[];
extern byte     librelocated[];
extern byte     libinitdone[];
extern i64      nlibs;

extern char*    symbolnametable[];
extern byte     symboldefined[];
extern void*    symboladdress[];
extern i16      symbollibindex[];
extern i16      symboldllindex[];
extern i64      nsymbols;

extern char* addext(char*, char*);
extern char* extractbasefile(char*);
extern byte* readfile(char*);
int rfsize=0;
extern char* convlcstring(char*);

extern char* extractext(char*,int);
extern char* extractpath(char*);
extern char* extractfile(char*);
extern char* changeext(char*,char*);

extern u64 os_getdllinst(char*);
extern void* os_getdllprocaddr(u64, char*);
extern void* os_allocexecmem(u64);

extern u64 LoadLibraryA(char*);
extern void* GetProcAddress(u64, char*);

extern void* VirtualAlloc(void*, u32, u32, u32);
extern u32 VirtualProtect(void*, u32, u32, u32*);

enum {NORMAL_PRIORITY_CLASS=32};
enum {CREATE_NEW_CONSOLE=16};
enum {DETACHED_PROCESS=16};

enum {MEM_COMMIT                = 4096};
enum {MEM_RESERVE               = 8192};
enum {PAGE_EXECUTE              = 16};
enum {PAGE_EXECUTE_READ         = 32};
enum {PAGE_EXECUTE_READWRITE    = 64};
enum {PAGE_NOACCESS             = 1};


char*   dllnametable[maxdlls];
u64     dllinsttable[maxdlls];
i64     ndlllibs=0;

char*   libnametable[maxlibs];
librec* libtable[maxlibs];
byte    librelocated[maxlibs];
byte    libinitdone[maxlibs];
i64     nlibs=0;

char*   symbolnametable[maxsymbols];
byte    symboldefined[maxsymbols];
void*   symboladdress[maxsymbols];
i16     symbollibindex[maxsymbols];
i16     symboldllindex[maxsymbols];
i64     nsymbols=0;

void loadimports(librec* plib);
librec* loadlibfile(char* filename, int libno);
int cmdskip;

void error(char* mess, char* param) {
    printf("%s %s\n",mess,param);
    puts("Aborting");
    exit(1);
}

byte* readmxfile(char* filename) {
    byte* p;

    p=readfile(filename);
    if (p) {
        *(p+rfsize) = end_dir;
    }
    return p;
}

void calllibinit(librec* lib) {
    void (*fnptr)(void);

    if (lib->entryaddr) {
        fnptr=(void(*)(void))lib->entryaddr;
        (*fnptr)();
    }
    libinitdone[lib->libno]=1;
}

void setspecialglobals(int cmdskip) {
    for (int i=1; i<=nsymbols; ++i) if (*symbolnametable[i]=='$') {
        if (strcmp(symbolnametable[i], "$cmdskip")==0) {
            *(byte*)symboladdress[i] = cmdskip;
        }
    }
}

void runprogram(librec* lib, int cmdskip) {
    int libno=lib->libno;
    void (*fnptr)(void);

    for (int i=1; i<=nlibs; ++i) if (i!=libno && !libinitdone[i]) {
        calllibinit(libtable[i]);
    }

    if (lib->entryaddr==NULL) error("No entry point found","");

    setspecialglobals(cmdskip);

    fnptr = (void(*)(void))lib->entryaddr;

    (*fnptr)();

    libinitdone[libno]=1;
}

void* mallocz(i64 n) {
    void* p=malloc(n);
    memset(p,0,n);
    return p;
}

void reloclib(librec* lib) {
    int index, targetoffset;
    char* name;
    byte* p;
    byte* q;
    u64* qaddr;
    mcxreloc r;

    p=lib->codeptr+lib->codesize;
    qaddr=(u64*)(p+lib->nimports*sizeof(u64));

    for (int i=1; i<=lib->nimports; ++i) {
        name=lib->importnames[i];
        *p++=0x48;
        *p++=0xFF;
        *p++=0x24;
        *p++=0x25;
        *(u32*)p=(u32)(u64)qaddr;
        p+=4;

        index=lib->importxreftable[i];
        *qaddr++ = (u64)symboladdress[index];
    }

    for (int i=1; i<=lib->nrelocs; ++i) {
        r=lib->reloctable[i];
        if (r.segment==code_seg) p=lib->codeptr+r.offset;
        else if (r.segment==idata_seg) p=lib->idataptr+r.offset;
        else if (r.segment==zdata_seg) p=lib->zdataptr+r.offset;

        switch (r.reloctype) {
        case locabs32_rel:
            targetoffset=*(u32*)p;
            switch (r.targetsegment) {
                   case code_seg: *(u32*)p = (u32)(u64)(lib->codeptr+targetoffset);
            break; case idata_seg: *(u32*)p = (u32)(u64)(lib->idataptr+targetoffset);
            break; case zdata_seg: *(u32*)p = (u32)(u64)(lib->zdataptr+targetoffset);
            }

        break; case locabs64_rel:
            targetoffset=*(u32*)p;
            switch (r.targetsegment) {
                   case code_seg: *(u64*)p = (u64)(lib->codeptr+targetoffset);
            break; case idata_seg: *(u64*)p = (u64)(lib->idataptr+targetoffset);
            break; case zdata_seg: *(u64*)p = (u64)(lib->zdataptr+targetoffset);
            }

        break; case impabs64_rel:

            index=lib->importxreftable[r.stindex];
            *(u64*)p += (u64)symboladdress[index];

        break; case impabs32_rel:
            index=lib->importxreftable[r.stindex];
            *(u32*)p += (u32)(u64)symboladdress[index];

        break; case imprel32_rel:
            if (r.segment!=code_seg) error("imprel32?","");
            index=r.stindex;
            q=lib->codeptr+lib->codesize+(index-1)*8;
            *(u32*)p = q-(p+4);
        }

    }

    librelocated[lib->libno]=1;
}

void dorelocations(void) {
    for (int i=1; i<=nlibs; ++i) if (!librelocated[i]) {
        reloclib(libtable[i]);
    }
}

int findlib(char* name) {
    for (int i=1; i<=nlibs; ++i) {
        if (strcmp(name, libnametable[i])==0) return i;
    }
    return 0;
}

int addlib(char* name) {
    if (nlibs>maxlibs) error("Too many libs","");
    libnametable[++nlibs]=name;
    return nlibs;
}

void adddll(char* name) {

    for (int i=1; i<=ndlllibs; ++i) {
        if (strcmp(name, dllnametable[i])==0) return;
    }

    if (ndlllibs>=maxdlls) error("Too many libs","");
    dllnametable[++ndlllibs]=name;
}

int addsymbol(char* name) {
    for (int i=1; i<=nsymbols; ++i) {
        if (strcmp(name, symbolnametable[i])==0) return i;
    }

    if (nsymbols>maxsymbols) error("Too many imports","");
    symbolnametable[++nsymbols]=name;
    return nsymbols;
}

void* finddllsymbol(char* name, int* dllindex) {
    void* p;

    *dllindex = 0;
    for (int i=1; i<=ndlllibs; ++i) {
        p = os_getdllprocaddr(dllinsttable[i], name);
        if (p) {
            *dllindex=i;
            return p;
        }
    }
    return NULL;
}

void scansymbols(void) {
    int dllindex, undef=0;
    void* p;

    for (int i=1; i<=nsymbols; ++i) if (!symboldefined[i]) {
        p=finddllsymbol(symbolnametable[i], &dllindex);
        if (p) {
            symboladdress[i]=p;
            symboldllindex[i]=dllindex;
            symboldefined[i]=1;
        } else {
            printf("Undef: %s\n",symbolnametable[i]);
            ++undef;
        }

    }

    if (undef) {
        error("Symbols undefined","");
    }
}


char* readstring(byte** p) {
    char* s = strdup((char*)*p);
    while (*++(*p)) {}
    ++(*p);
    return s;
}

int readbyte(byte** p) {return *(*p)++;}

u64 readu32(byte** p) {
    u64 x = *(u32*)(*p);
    (*p)+=4;
    return x;
}

void loaddlls(void) {
    u64 inst;

    for (int i=1; i<=ndlllibs; ++i) if (!dllinsttable[i]) {
        inst=os_getdllinst(dllnametable[i]);
        if (inst==0) error("Can't find DLL:",dllnametable[i]);

        dllinsttable[i]=inst;
    }

}

void dosymbols(librec* lib) {
    int ix;
    byte* baseaddr;

    for (int i=1; i<=lib->ndlllibs; ++i)
        adddll(lib->dllnames[i]);

    for (int i=1; i<=lib->nimports; ++i)
        lib->importxreftable[i]=addsymbol(lib->importnames[i]);

    for (int i=1; i<=lib->nexports; ++i) {
        ix=addsymbol(lib->exports[i]);
        if (symboldefined[ix]) {
            printf("Dupl symbol: %s\n", lib->exports[i]);
            continue;
        }

        symboldefined[ix]=1;

        switch (lib->exportsegs[i]) {
        case code_seg: baseaddr=lib->codeptr; break;    
        case idata_seg: baseaddr=lib->idataptr; break;  
        case zdata_seg: baseaddr=lib->zdataptr; break;  
        default: baseaddr=NULL;
        }

        symboladdress[ix]=baseaddr+lib->exportoffsets[i];
        symbollibindex[ix]=lib->libno;
    }
}

void fixuplib(librec* lib) {
    loaddlls();
    scansymbols();
    dorelocations();
}

void alloclibdata(librec* lib) {
    int tablesize, n;
    byte* p;

    lib->zdataptr=mallocz(lib->zdatasize);

    tablesize=lib->nimports*16;
    n=lib->codesize;

    p=os_allocexecmem(n+tablesize);
    if (p==NULL) error("Can't alloc code memory","");

    memcpy(p, lib->codeptr, n);

    memset(p+n, 0, tablesize);

    lib->codeptr=p;
    lib->codexsize=tablesize;

    lib->exportaddr=malloc(sizeof(u64)*(lib->nexports+1));
    lib->importxreftable=malloc(sizeof(i16)*(lib->nimports+1));

    if (lib->entryoffset!=0xFFFFFFFF)
        lib->entryaddr=lib->codeptr+lib->entryoffset;
}

void dosublib(char* name) {
    librec* qlib;

    int n=findlib(name);

    if (n==0) {
        n=addlib(name);
        printf("Loading sublib: %s\n",name);
        qlib=loadlibfile(addext(name,"ml"),n);
        loadimports(qlib);
    }
}

void loadimports(librec* plib) {
    librec* qlib;
    char* name;

    for (int i=1; i<=plib->nlibs; ++i) {
        dosublib(plib->libnames[i]);
    }

    alloclibdata(plib);
    dosymbols(plib);
}

librec* readlibfile(char* filename, byte* p) {
    librec lib;
    u32 sig;
    int dir, n, tablesize;
    byte* pstart=p;

    memset(&lib, 0, sizeof(librec));

    sig=readu32(&p);
    if (sig!=mcxsig) error("Bad sig - not MX/ML file","");

    lib.filespec=strdup(filename);
    lib.libname=strdup(extractbasefile(filename));

    while (1) {
    switch (dir=readbyte(&p)) {
    case version_dir:
        lib.version=readstring(&p);

        break;

    case zdata_dir:
        lib.zdatasize=readu32(&p);
        lib.zdataptr=mallocz(lib.zdatasize);
        break;

    case idata_dir:
        lib.idatasize=n=readu32(&p);
        lib.idataptr=malloc(n);
        memcpy(lib.idataptr, p, n)  ;
        p+=n;
        break;

    case code_dir:
        lib.codesize=n=readu32(&p);
        lib.codeptr=p;
        p+=n;
        break;

    case dlls_dir:
        lib.ndlllibs=n=readu32(&p);
        lib.dllnames=malloc(sizeof(char*)*(n+1));
        for (int i=1; i<=n; ++i) {
            lib.dllnames[i]=readstring(&p);
        }
        break;

    case libs_dir:
        lib.nlibs=n=readu32(&p);
        lib.libnames=malloc(sizeof(char*)*(n+1));
        for (int i=1; i<=n; ++i) {
            lib.libnames[i]=readstring(&p);
        }
        break;

    case importsymbols_dir:
        lib.nimports=n=readu32(&p);
        lib.importnames=malloc(sizeof(char*)*(n+1));
        for (int i=1; i<=n; ++i) {
            lib.importnames[i]=readstring(&p);
        }
        break;

    case exportsymbols_dir:
        lib.nexports=n=readu32(&p);
        lib.exports=malloc(sizeof(char*)*(n+1));
        for (int i=1; i<=n; ++i) {
            lib.exports[i]=readstring(&p);
        }
        break;

    case exportsegs_dir:
        n=readu32(&p);
        lib.exportsegs=malloc(n);
        for (int i=1; i<=n; ++i) {
            lib.exportsegs[i]=readbyte(&p);
        }
        break;

    case exportoffsets_dir:
        n=readu32(&p);
        lib.exportoffsets=malloc(sizeof(u64)*(n+1));
        for (int i=1; i<=n; ++i) {
            lib.exportoffsets[i]=readu32(&p);
        }
        break;

    case reloc_dir:
        lib.nrelocs=n=readu32(&p);
        n=lib.nrelocs*sizeof(mcxreloc);
        lib.reloctable=malloc(n+sizeof(mcxreloc));
        memcpy(lib.reloctable+1, p, n);
        p+=n;
        break;

    case entry_dir:
        lib.entryoffset=readu32(&p);
        break;

    case end_dir:
        goto finish;

    case pad_dir:
        break;

    default:
//      error(("Unknown directive:",mcxdirnames[dir]);
//      error("Unknown directive:","<dirname>");
        printf("%d\n",dir);
        printf("%x\n",dir);
        error("Unknown directive:","");
        exit(1);
    }}
finish:;

    librec* plib=malloc(sizeof(librec));
    memcpy(plib, &lib, sizeof(librec));

    return plib;
}

librec* loadlibfile(char* filename, int libno) {
    byte* p;
    librec* plib;

    p=readmxfile(filename);
    if (p==NULL)
        error("Can't load:", filename);

    plib=readlibfile(filename, p);
    plib->libno=libno;
    libtable[libno]=plib;

    return plib;
}

librec* loadmx(char* filename) {
    librec* plib;
    int newlib;
    char* name;

    name=strdup(convlcstring(extractbasefile(filename)));

    newlib=addlib(name);

    plib=loadlibfile(filename, newlib);

    loadimports(plib);

    return plib;
}

int main(int nargs, char** args) {
    char* filename;
    librec* plib;

    if (nargs<2) {
        printf("Usage:\n    %s    filename[.mx]\n", args[0]);
        exit(1);
    }

    filename = strdup(addext(args[1],"mx"));
    cmdskip = 1;

    plib=loadmx(filename);
    fixuplib(plib);

    runprogram(plib, cmdskip);
}

char* extractpath(char* s) {
    static char str[260];
    char* t;
    int n;

    t=s+strlen(s)-1;

    while (t>=s) {
        switch (*t) {
        case '\\': case '/': case ':':
            n=t-s+1;
            memcpy(str,s,n);
            str[n]=0;
            return str;
        }
        --t;
    }
    return "";
}

char* extractfile(char* s) {
    char* t=extractpath(s);
    if (*t==0) return s;
    return s+strlen(t);
}

char* extractext(char* s, int period) {
    char* t;
    char* u;

    t=extractfile(s);

    if (*t==0) return "";

    u=t+strlen(t)-1;

    while (u>=t) {
        if (*u=='.') {
            if (*(u+1)==0)
                return (period?".":"");
            return u+1;
        }
        --u;
    }
    return "";
}

char* changeext(char* s, char* newext) {
    static char newfile[260];
    char newext2[32];
    char* sext;
    int n;

    strcpy(newfile,s);

    if (*newext==0) {
        newext2[0]=0;
        newext2[1]=0;
    } else if (*newext=='.') {
        strcpy(newext2, newext);
    } else {
        strcpy(newext2, ".");
        strcat(newext2, newext);
    }

    sext=extractext(s,1);

    if (*sext==0) {
        strcat(newfile, newext2);
    } else if (*sext=='.') {
        strcat(newfile, newext2+1);
    } else {
        n=sext-s-2;
        strcpy(newfile+n+1, newext2);
    }

    return newfile;
}

char* addext(char* s, char* newext) {
    char* sext;

    sext=extractext(s,1);
    if (*sext==0) return changeext(s, newext);

    return s;   
}

char* extractbasefile(char* s) {
    static char str[100];
    char* f;
    char* e;
    int n, flen;

    f=extractfile(s);
    flen=strlen(f);
    if (flen==0) return "";

    e=extractext(f,0);

    if (*e) {
        n=flen-strlen(e)-1;
        memcpy(str,f,n);
        str[n]=0;
        return str;
    }
    if (*(f+flen-1)=='.') {
        memcpy(str,f,flen-1);
        str[flen-1]=0;
        return str;
    }

    return f;
}

u64 getfilesize(FILE* f) {
    u64 p, size;

    p=ftell(f);
    fseek(f,0,2);
    size=ftell(f);
    fseek(f,p,SEEK_SET);
    return size;
}

byte* readfile(char* filename) {
    FILE* f;
    byte* m;
    byte* p;

    f=fopen(filename,"rb");
    if (f==NULL) return NULL;
    rfsize=getfilesize(f);

    m=malloc(rfsize+2);
    if (m==NULL) return NULL;


    fread(m, 1, rfsize, f);
    fclose(f);

    *(u16*)(m+rfsize) = 0;
    return m;
}

char* convlcstring(char* s) {
    char* t=s;
    while (*t) {
        *t=tolower(*t);
        ++t;
    }   
    return s;
}

u64 os_getdllinst(char* name) {
    return (u64)LoadLibraryA(name);
}

void* os_getdllprocaddr(u64 hinst, char* fnname) {
    return GetProcAddress(hinst, fnname);
}

void* os_allocexecmem(u64 n) {
    byte* p;
    u32 oldprot;
    int status;

    p=VirtualAlloc(NULL, n, MEM_RESERVE | MEM_COMMIT, PAGE_NOACCESS);
    if (p==NULL) return NULL;

    status=VirtualProtect(p, n, PAGE_EXECUTE_READWRITE, &oldprot);

    if (status==0) return NULL;

    return p;
}
