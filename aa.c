
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"

//#include <stdio.h>
//#include <stdlib.h>
//#include <ctype.h>
//#include <string.h>
//#include <math.h>
//#include <time.h>
//#include <sys/types.h>
//#include <sys/stat.h>

//#pragma pack(1)

typedef signed char         i8;
typedef short              i16;
typedef int                i32;
typedef long long          i64;

typedef unsigned char       u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
typedef unsigned long long u64;

typedef unsigned char     byte;

typedef float              r32;
typedef double             r64;

extern r64 sqrt(r64);
extern r64 fabs(r64);

typedef struct {void* ptr; u64 length;} Slice;

#define NULL ((void*)0)

#ifndef CALLBACK
#define CALLBACK
#endif

i64 m$llabs(i64 a){ return (a>=0?a:-a);}
#define m$infinity (1.0/0.0)


/* Forward Struct Declarations */
struct aa_mcxdecls$mcxreloc;
struct aa_mcxdecls$librec;
struct aa_decls$fwdrec;
struct aa_decls$opndrec;
struct aa_decls$strec;
struct aa_decls$relocrec;
struct aa_decls$dbuffer;
struct aa_decls$modulerec;
struct aa_decls$stlistrec;
struct aa_objdecls$imagefileheader;
struct aa_objdecls$imagedir;
struct aa_objdecls$optionalheader;
struct aa_objdecls$imagesectionheader;
struct aa_objdecls$imagesymbol;
struct aa_objdecls$importdirrec;
struct aa_objdecls$coffrelocrec;
struct aa_objdecls$auxsectionrec;
struct aa_objdecls$sectionrec;
struct aa_objdecls$importrec;
struct aa_objdecls$exportrec;
struct aa_objdecls$dllrec;
struct aa_objdecls$exportdirrec;
struct aa_writeexe$basereloc;
struct aa_lib$mclrec;
struct msysc$procinforec;
struct msysc$fmtrec;
struct mlib$strbuffer;
struct mwindows$rsystemtime;
struct mwindows$input_record;
struct mwindows$rspoint;
struct mwindows$rsrect;
struct mwindows$rpoint;
struct mwindows$rconsole;
struct mwindows$rstartupinfo;
struct mwindows$rprocess_information;
struct mwindows$rwndclassex;
struct mwindows$rmsg;

/* Struct Definitions */
struct aa_mcxdecls$mcxreloc {
    u32 offset;
    union {
        u16 stindex;
        byte targetsegment;
    };
    byte segment;
    byte reloctype;
};

struct aa_mcxdecls$librec {
    u8 *  version;
    i64 codesize;
    i64 idatasize;
    i64 zdatasize;
    i64 nrelocs;
    i64 ndlllibs;
    i64 nlibs;
    i64 nimports;
    i64 nexports;
    byte *  codeptr;
    byte *  idataptr;
    struct aa_mcxdecls$mcxreloc (*reloctable)[];
    u8 *(*dllnames)[];
    u8 *(*libnames)[];
    u8 *(*importnames)[];
    u8 *(*exports)[];
    byte (*exportsegs)[];
    u64 (*exportoffsets)[];
    u64 entryoffset;
    byte *  zdataptr;
    i64 codexsize;
    u64 (*exportaddr)[];
    i16 (*importxreftable)[];
    u8 *  filespec;
    u8 *  libname;
    byte *  entryaddr;
    i64 libno;
};

struct aa_decls$fwdrec {
    struct aa_decls$fwdrec* nextfwd;
    i32 offset;
    i16 reltype;
    i16 seg;
};

struct aa_decls$opndrec {
    struct aa_decls$strec *  labeldef;
    union {
        i64 value;
        r64 xvalue;
        u8 *  svalue;
    };
    byte mode;
    byte size;
    byte reg;
    byte regix;
    byte scale;
    byte addrsize;
    byte valtype;
    byte spare2;
};

struct aa_decls$strec {
    u8 *  name;
    struct aa_decls$fwdrec *  fwdrefs;
    struct aa_decls$opndrec *  expr;
    i32 offset;
    i32 stindex;
    i32 importindex;
    byte symbol;
    byte ksymbol;
    byte subcode;
    byte regsize;
    byte scope;
    byte reftype;
    byte segment;
    byte namelen;
    struct aa_decls$strec* basedef;
    struct aa_decls$strec* nextdef;
    struct aa_decls$strec* nextdupl;
    i32 moduleno;
    u32 htindex;
    u32 htfirstindex;
    u32 impindex;
    u32 expindex;
    byte spare[40];
};

struct aa_decls$relocrec {
    struct aa_decls$relocrec* nextreloc;
    i64 reloctype;
    i64 offset;
    i64 stindex;
};

struct aa_decls$dbuffer {
    byte *  pstart;
    union {
        byte *  pcurr;
        u16 *  pcurr16;
        u32 *  pcurr32;
        u64 *  pcurr64;
    };
    byte *  pend;
    i64 alloc;
};

struct aa_decls$modulerec {
    u8 *  filename;
    u8 *  name;
    u8 *  source;
};

struct aa_decls$stlistrec {
    struct aa_decls$strec *  def;
    struct aa_decls$stlistrec* nextitem;
};

struct aa_objdecls$imagefileheader {
    u16 machine;
    u16 nsections;
    u32 timedatestamp;
    u32 symtaboffset;
    u32 nsymbols;
    u16 optheadersize;
    u16 characteristics;
};

struct aa_objdecls$imagedir {
    u32 virtualaddr;
    u32 size;
};

struct aa_objdecls$optionalheader {
    u16 magic;
    byte majorlv;
    byte minorlv;
    u32 codesize;
    u32 idatasize;
    u32 zdatasize;
    u32 entrypoint;
    u32 codebase;
    u64 imagebase;
    u32 sectionalignment;
    u32 filealignment;
    u16 majorosv;
    u16 minorosv;
    u16 majorimagev;
    u16 minorimagev;
    u16 majorssv;
    u16 minorssv;
    u32 win32version;
    u32 imagesize;
    u32 headerssize;
    u32 checksum;
    u16 subsystem;
    u16 dllcharacteristics;
    u64 stackreserve;
    u64 stackcommit;
    u64 heapreserve;
    u64 heapcommit;
    u32 loaderflags;
    u32 rvadims;
    struct aa_objdecls$imagedir exporttable;
    struct aa_objdecls$imagedir importtable;
    struct aa_objdecls$imagedir resourcetable;
    struct aa_objdecls$imagedir exceptiontable;
    struct aa_objdecls$imagedir certtable;
    struct aa_objdecls$imagedir basereloctable;
    struct aa_objdecls$imagedir debug;
    struct aa_objdecls$imagedir architecture;
    struct aa_objdecls$imagedir globalptr;
    struct aa_objdecls$imagedir tlstable;
    struct aa_objdecls$imagedir loadconfigtable;
    struct aa_objdecls$imagedir boundimport;
    struct aa_objdecls$imagedir iat;
    struct aa_objdecls$imagedir delayimportdescr;
    struct aa_objdecls$imagedir clrheader;
    struct aa_objdecls$imagedir reserved;
};

struct aa_objdecls$imagesectionheader {
    u8 name[8];
    union {
        u32 physical_address;
        u32 virtual_size;
    };
    u32 virtual_address;
    u32 rawdata_size;
    u32 rawdata_offset;
    u32 relocations_ptr;
    u32 linenos_offset;
    u16 nrelocs;
    u16 nlinenos;
    u32 characteristics;
};

struct aa_objdecls$imagesymbol {
    union {
        u8 shortname[8];
        struct {
            u32 shortx;
            u32 longx;
        };
        u64 longname;
    };
    u32 value;
    i16 sectionno;
    u16 symtype;
    byte storageclass;
    byte nauxsymbols;
};

struct aa_objdecls$importdirrec {
    u32 implookuprva;
    u32 timedatestamp;
    u32 fwdchain;
    u32 namerva;
    u32 impaddressrva;
};

struct aa_objdecls$coffrelocrec {
    i32 virtualaddr;
    i32 stindex;
    i16 reloctype;
};

struct aa_objdecls$auxsectionrec {
    i32 length;
    i16 nrelocs;
    i16 nlines;
    i32 checksum;
    i16 sectionno;
    i32 dummy;
};

struct aa_objdecls$sectionrec {
    union {
        struct aa_decls$dbuffer *  data;
        byte *  bytedata;
    };
    u8 *  name;
    i64 segtype;
    i64 rawsize;
    i64 rawoffset;
    i64 virtsize;
    i64 virtoffset;
    struct aa_decls$relocrec *  relocs;
    i64 nrelocs;
};

struct aa_objdecls$importrec {
    struct aa_decls$strec *  def;
    i64 libno;
    u8 *  name;
    i64 hintnameoffset;
    i64 iatoffset;
    i64 thunkoffset;
};

struct aa_objdecls$exportrec {
    struct aa_decls$strec *  def;
    u8 *  name;
};

struct aa_objdecls$dllrec {
    u8 *  name;
    i64 nprocs;
    i64 nametableoffset;
    i64 addrtableoffset;
    i64 dllnameoffset;
    i64 dllextraoffset;
};

struct aa_objdecls$exportdirrec {
    u32 exportflags;
    u32 timedatestamp;
    u16 majorversion;
    u16 minorversion;
    u32 namerva;
    u32 ordinalbase;
    u32 naddrtable;
    u32 nnamepointers;
    u32 expaddressrva;
    u32 namepointerrva;
    u32 ordtablerva;
};

struct aa_writeexe$basereloc {
    struct aa_writeexe$basereloc* nextitem;
    u32 address;
    i32 reloctype;
};

struct aa_lib$mclrec {
    struct aa_lib$mclrec* nextmcl;
    struct aa_decls$opndrec *  a;
    struct aa_decls$opndrec *  b;
    u16 opcode;
    u16 c;
    u32 lineno;
};

struct msysc$procinforec {
    u16 fnindex;
    byte rettype;
    byte nparams;
    byte paramlist[12];
};

struct msysc$fmtrec {
    byte minwidth;
    i8 precision;
    byte base;
    u8 quotechar;
    u8 padchar;
    u8 realfmt;
    u8 plus;
    u8 sepchar;
    u8 lettercase;
    u8 justify;
    u8 suffix;
    u8 usigned;
    u8 charmode;
    u8 heapmode;
    u8 param;
    byte spare;
};

struct mlib$strbuffer {
    u8 *  strptr;
    i32 length;
    i32 allocated;
};

struct mwindows$rsystemtime {
    u16 year;
    u16 month;
    u16 dayofweek;
    u16 day;
    u16 hour;
    u16 minute;
    u16 second;
    u16 milliseconds;
};

#pragma pack(8)
struct mwindows$input_record {
    u16 eventtype;
    u32 keydown;
    u16 repeatcount;
    u16 virtualkeycode;
    u16 virtualscancode;
    union {
        u16 unicodechar;
        byte asciichar;
    };
    u32 controlkeystate;
};
#pragma pack(1)

struct mwindows$rspoint {
    i16 x;
    i16 y;
};

struct mwindows$rsrect {
    i16 leftx;
    i16 top;
    i16 rightx;
    i16 bottom;
};

struct mwindows$rpoint {
    i32 x;
    i32 y;
};

struct mwindows$rconsole {
    struct mwindows$rspoint size;
    struct mwindows$rspoint pos;
    u16 attributes;
    struct mwindows$rsrect window;
    struct mwindows$rspoint maxwindowsize;
};

struct mwindows$rstartupinfo {
    u32 size;
    u32 dummy1;
    u8 *  reserved;
    u8 *  desktop;
    u8 *  title;
    u32 x;
    u32 y;
    u32 xsize;
    u32 ysize;
    u32 xcountchars;
    u32 ycountchars;
    u32 fillattribute;
    u32 flags;
    u16 showwindow;
    u16 reserved2;
    u32 dummy2;
    void *  reserved4;
    void *  stdinput;
    void *  stdoutput;
    void *  stderror;
};

struct mwindows$rprocess_information {
    void *  process;
    void *  thread;
    u32 processid;
    u32 threadid;
};

struct mwindows$rwndclassex {
    u32 size;
    u32 style;
    void (*wndproc)(void);
    i32 clsextra;
    i32 wndextra;
    void *  instance;
    void *  icon;
    void *  cursor;
    void *  background;
    u8 *  menuname;
    u8 *  classname;
    void *  iconsm;
};

struct mwindows$rmsg {
    void *  hwnd;
    u32 message;
    u32 dummy1;
    u64 wparam;
    u64 lparam;
    u32 time;
    u32 dummy2;
    struct mwindows$rpoint pt;
};


/* PROCDECLS */
int main(int, char**, char**);
static void aacli$loadsourcefiles(void);
static void aacli$parsemodules(void);
static void aacli$fixopnd(struct aa_decls$opndrec *a);
static void aacli$initlogfile(void);
static void aacli$closelogfile(void);
static void aacli$initall(void);
static void aacli$lextest(u8 *file);
static void aacli$getinputoptions(void);
static void aacli$do_option(i64 sw,u8 *value);
static void aacli$showhelp(void);
static void aacli$showcaption(void);
static void aacli$loaderror(u8 *mess);
static void aacli$loaderror_s(u8 *mess,u8 *s);
static void aacli$addmodule(u8 *name);
static void aacli$addsearchlib(u8 *name);
static void aacli$addimportlib(u8 *name);
static void aacli$showmodules(void);
static struct aa_decls$strec *aacli$getemptyst(struct aa_decls$strec *d);
static struct aa_decls$strec *aacli$findduplname(struct aa_decls$strec *d);
static void aacli$adddupl(struct aa_decls$strec *d);
static void aacli$scanglobals(void);
static void aacli$resethashtable(void);
void aacli$start(void);
void aa_mcxdecls$start(void);
void aa_decls$start(void);
void aa_tables$start(void);
void aa_objdecls$start(void);
void aa_lex$lex(void);
void aa_lex$initlex(void);
static void aa_lex$readreal(u8 (*s)[],i64 slen,i64 intlen,i64 exponseen);
static void aa_lex$readnumber(i64 c);
static void aa_lex$readbinary(void);
static void aa_lex$readhex(void);
void aa_lex$ps(u8 *caption);
void aa_lex$printsymbol(void *dev);
static void aa_lex$clearhashtable(void);
static void aa_lex$inithashtable(void);
static void aa_lex$addreservedword(u8 *name,i64 symbol,i64 subcode);
void aa_lex$printhashtable(void *devx,u8 *caption);
static i64 aa_lex$lookuplex(u8 *name,i64 length);
void aa_lex$initsourcefile(u8 *source);
struct aa_decls$strec *aa_lex$addnamestr(u8 *name);
void aa_lex$lxerror(u8 *m);
i64 aa_lex$gethashvalue(u8 *s);
void aa_lex$skiptoeol(void);
static u8 *aa_lex$makestring(u8 *p,i64 length);
void aa_lex$start(void);
void aa_parse$readmodule(i64 moduleno);
void aa_parse$checkundefined(void);
static void aa_parse$checksymbol(i64 symbol);
static void aa_parse$readinstr(void);
static void aa_parse$readcondinstr(i64 opc);
static struct aa_decls$opndrec *aa_parse$readoperand(void);
static struct aa_decls$opndrec *aa_parse$readexpression(void);
static void aa_parse$readterm(void);
static void aa_parse$readreg(i64 *reg,i64 *regsize,i64 *scale);
static struct aa_decls$opndrec *aa_parse$readaddrmode(i64 size);
void aa_parse$start(void);
void aa_showss$writemcx(u8 *filename);
struct mlib$strbuffer *aa_showss$showssdata(void);
static void aa_showss$showsectiondata(struct aa_decls$dbuffer *d);
static void aa_showss$showsectioncode(struct aa_decls$dbuffer *d);
static void aa_showss$gs_value(u8 *caption,i64 value);
static void aa_showss$showsymboltable2(void);
static void aa_showss$showimporttable(void);
static void aa_showss$showsections(void);
static void aa_showss$writerelocs(void *f);
static void aa_showss$scansymbols(void);
static void aa_showss$writesymbols(void *f);
static void aa_showss$roundsegment(struct aa_decls$dbuffer *p,i64 align,i64 value);
static void aa_showss$showsectionrelocs2(u8 *caption,struct aa_decls$relocrec *relocs,i64 nrelocs);
void aa_showss$start(void);
void aa_writeobj$writess(u8 *outfile);
static void aa_writeobj$writerecord(void *r,i64 length);
static void aa_writeobj$writerelocs(struct aa_decls$relocrec *r,i64 nrelocs);
static void aa_writeobj$writedata(struct aa_decls$dbuffer *data);
static void aa_writeobj$writesymboltable(void);
static void aa_writeobj$writestringtable(void);
static struct aa_objdecls$imagesymbol *aa_writeobj$makesymbol(u8 *name,i64 namelen,i64 value,i64 sectionno,i64 symtype,i64 storage,i64 naux);
static void aa_writeobj$addsymbol(struct aa_objdecls$imagesymbol *r);
static void aa_writeobj$initsymboltable(u8 *filename);
static struct aa_objdecls$imagesymbol *aa_writeobj$strtoaux(u8 *s);
static struct aa_objdecls$auxsectionrec *aa_writeobj$sectiontoaux(struct aa_decls$dbuffer *data,i64 nrelocs);
static i64 aa_writeobj$addstringentry(u8 *s,i64 length);
static void aa_writeobj$convertsymboltable(void);
static void aa_writeobj$writecoff(u8 *outfile);
void aa_writeobj$start(void);
void aa_writeexe$writeexe(u8 *outfile,i64 dodll);
void aa_writeexe$genexe(u8 *entrypoint,u8 *outfile,i64 dodll);
static void aa_writeexe$loadlibs(void);
void aa_writeexe$initsectiontable(void);
static u8 *aa_writeexe$extractlibname(u8 *name,i64 *libno,i64 moduleno);
static void aa_writeexe$scanst(void);
static void aa_writeexe$relocdata(struct aa_objdecls$sectionrec *s);
static void aa_writeexe$getbaserelocs(struct aa_objdecls$sectionrec *s);
static void aa_writeexe$writerecordx(void *r,i64 length);
static void aa_writeexe$writedosstub(void);
static void aa_writeexe$writepesig(void);
static void aa_writeexe$writepadding(i64 offset);
static void aa_writeexe$writefileheader(void);
static void aa_writeexe$writeoptheader(void);
static void aa_writeexe$writesectionheader(struct aa_objdecls$sectionrec *s);
static void aa_writeexe$writesectiondata(struct aa_objdecls$sectionrec *s);
static void aa_writeexe$getoffsets(void);
static i64 aa_writeexe$getsectionno(i64 segment);
static void aa_writeexe$writeexporttable(byte *pstart);
static i64 aa_writeexe$getexporttablesize(void);
static void aa_writeexe$newbasereloc(i64 addr,i64 reltype);
static void aa_writeexe$scanbaserelocs(void);
static void aa_writeexe$writebasereloctable(byte *pstart);
static void aa_writeexe$sortexports(i64 (*sortindex)[]);
void aa_writeexe$start(void);
struct mlib$strbuffer *aa_writess$writessdata(i64 fexe);
static void aa_writess$showssdata(i64 fexe);
static void aa_writess$showsectiondata(struct aa_objdecls$sectionrec *d);
static void aa_writess$showsectioncode(struct aa_objdecls$sectionrec *p);
static void aa_writess$showsectionrelocs2(u8 *caption,struct aa_decls$relocrec *relocs,i64 nrelocs);
static void aa_writess$gs_value(u8 *caption,i64 value);
static void aa_writess$showsymboltable2(void);
static void aa_writess$showimporttable(void);
static void aa_writess$showsections(void);
void aa_writess$start(void);
u8 *aa_disasm$decodeinstr(byte **cptr,byte *baseaddr);
static void aa_disasm$decodetwobyteinstr(void);
static void aa_disasm$decodeaddr(i64 w);
static i64 aa_disasm$readbyte(void);
static i64 aa_disasm$readsbyte(void);
static u64 aa_disasm$readword16(void);
static i64 aa_disasm$readint16(void);
static u64 aa_disasm$readword32(void);
static i64 aa_disasm$readint32(void);
static i64 aa_disasm$readint64(void);
static i64 aa_disasm$getreg(i64 regcode,i64 upper);
u8 *aa_disasm$strreg(i64 reg,i64 opsize);
static u8 *aa_disasm$strfreg(i64 freg);
static void aa_disasm$printaddrmode(i64 xmm);
static void aa_disasm$genstr(u8 *s);
static void aa_disasm$genintd(i64 a);
static void aa_disasm$genhex(i64 a);
static i64 aa_disasm$readimm(void);
static i64 aa_disasm$readimm8(void);
static u8 *aa_disasm$strxmm(i64 reg);
static u8 *aa_disasm$strmmx(i64 reg);
static void aa_disasm$decode8087(i64 ttt);
static void aa_disasm$do87arith(u8 *opcstr,i64 ttt,i64 freg);
static void aa_disasm$do87mem(u8 *opcstr,i64 mf);
static void aa_disasm$getsil(i64 *reg);
static void aa_disasm$getsilx(i64 *reg);
void aa_disasm$start(void);
void aa_genss$genss(void);
static void aa_genss$doinstr(struct aa_lib$mclrec *m,i64 index);
static void aa_genss$genbyte(i64 x);
static void aa_genss$genword(i64 x);
static void aa_genss$gendword(i64 x);
static void aa_genss$genqword(i64 x);
static void aa_genss$genopnd(struct aa_decls$opndrec *a,i64 size);
static void aa_genss$addrelocitem(i64 reloctype,struct aa_decls$strec *d);
static i64 aa_genss$getstindex(struct aa_decls$strec *d);
static void aa_genss$genrel32(struct aa_decls$opndrec *a);
static void aa_genss$genabs32(struct aa_decls$opndrec *a);
static void aa_genss$genabs64(struct aa_decls$opndrec *a);
static i64 aa_genss$getrel32(struct aa_decls$strec *d,i64 offset);
static void aa_genss$dofwdrefs(struct aa_decls$strec *d);
static void aa_genss$genrex(void);
static i64 aa_genss$isbytesized(i64 x);
static i64 aa_genss$isdwordsized(i64 x);
static void aa_genss$do_push(struct aa_decls$opndrec *a);
static void aa_genss$do_pop(struct aa_decls$opndrec *a);
static void aa_genss$do_inc(struct aa_decls$opndrec *a,i64 code);
static void aa_genss$do_neg(struct aa_decls$opndrec *a,i64 code);
static void aa_genss$genamode(struct aa_decls$opndrec *a,i64 am);
static i64 aa_genss$makemodrm(i64 mode,i64 opc,i64 rm);
static void aa_genss$setopsize(struct aa_decls$opndrec *a);
static i64 aa_genss$getdispsize(struct aa_decls$opndrec *a,i64 mand);
static void aa_genss$genrmbyte(i64 mode,i64 opc,i64 rm);
static i64 aa_genss$makeam(i64 m,i64 s,i64 d);
static void aa_genss$do_lea(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b);
static void aa_genss$do_movsx(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 opc);
static void aa_genss$checkhighreg(struct aa_decls$opndrec *a);
static void aa_genss$do_exch(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b);
static void aa_genss$do_movsxd(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b);
static void aa_genss$do_imul2(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b);
static void aa_genss$do_shift(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 code);
static void aa_genss$do_test(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b);
static void aa_genss$do_loop(struct aa_decls$opndrec *a,i64 opc);
static void aa_genss$do_jcxz(struct aa_decls$opndrec *a,i64 opsize);
static void aa_genss$do_setcc(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b);
static void aa_genss$do_arithxmm(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 prefix,i64 opc);
static void aa_genss$do_logicxmm(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 opc,i64 size);
static void aa_genss$do_convertfloat(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 prefix);
static void aa_genss$do_fix(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 prefix,i64 opc);
static void aa_genss$do_float(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 prefix);
static void aa_genss$do_call(struct aa_decls$opndrec *a);
static void aa_genss$do_jmp(struct aa_decls$opndrec *a,struct aa_lib$mclrec *m);
static i64 aa_genss$getcurrdatalen(i64 id);
static void aa_genss$do_cmovcc(struct aa_decls$opndrec *c,struct aa_decls$opndrec *a,struct aa_decls$opndrec *b);
static void aa_genss$do_fmem(struct aa_decls$opndrec *a,i64 freal,i64 code);
static i64 aa_genss$getr32bits(r64 x);
static void aa_genss$genrel8(struct aa_decls$opndrec *a);
static i64 aa_genss$checkshortjump(struct aa_lib$mclrec *m,struct aa_decls$strec *d);
static struct aa_decls$fwdrec *aa_genss$addfwdref(struct aa_decls$fwdrec *p,i64 offset,i64 reltype,i64 seg);
static void aa_genss$switchseg(i64 newseg);
static void aa_genss$do_popcnt(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b);
static void aa_genss$do_bsf(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 opc);
static void aa_genss$extendsymboltable(void);
static void aa_genss$do_pcmpistri(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 c,i64 opc);
static void aa_genss$genxrm(i64 opcode,i64 code,struct aa_decls$opndrec *b);
static void aa_genss$genrrm(i64 opcode,struct aa_decls$opndrec *a,struct aa_decls$opndrec *b);
static i64 aa_genss$getregcode(i64 reg,i64 mask,i64 isxreg);
static void aa_genss$checkimmrange(i64 value,i64 size);
static i64 aa_genss$newgenrm(i64 reg,i64 opc,struct aa_decls$opndrec *b,i64 isxreg);
static void aa_genss$do_mov(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b);
static void aa_genss$do_arith(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 code);
static void aa_genss$do_movxmm(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 size);
static void aa_genss$checksize(struct aa_decls$opndrec *a,i64 size1,i64 size2);
void aa_genss$start(void);
void aa_lib$initlib(void);
void aa_lib$genmc(i64 opcode,struct aa_decls$opndrec *a,struct aa_decls$opndrec *b);
void aa_lib$genmcstr(i64 opcode,u8 *s);
static struct aa_decls$opndrec *aa_lib$newopnd(i64 mode);
struct aa_decls$opndrec *aa_lib$genxreg(i64 xreg);
struct aa_decls$opndrec *aa_lib$genindex(i64 areg,i64 ireg,i64 scale,struct aa_decls$opndrec *x,i64 size,i64 addrsize);
struct mlib$strbuffer *aa_lib$writemclblock(void);
void aa_lib$gencomment(u8 *s);
struct aa_decls$opndrec *aa_lib$genstrimm(u8 *s);
static u8 *aa_lib$getsizetag(i64 size);
static void aa_lib$writemcl(i64 index,struct aa_lib$mclrec *mcl);
u8 *aa_lib$strmcl(struct aa_lib$mclrec *mcl);
u8 *aa_lib$stropnd(struct aa_decls$opndrec *a,i64 sizeprefix);
static u8 *aa_lib$strdef(struct aa_decls$strec *def);
void aa_lib$setsegment(i64 seg);
static u8 *aa_lib$getsizeprefix(i64 size,i64 enable);
static i64 aa_lib$needsizeprefix(i64 opcode,struct aa_decls$opndrec *a,struct aa_decls$opndrec *b);
struct aa_decls$opndrec *aa_lib$genimm_expr(struct aa_decls$strec *d,i64 value,i64 t,i64 size);
struct aa_decls$opndrec *aa_lib$genint(i64 x,i64 size);
struct aa_decls$opndrec *aa_lib$genlab(struct aa_decls$strec *d,i64 size);
struct aa_decls$opndrec *aa_lib$genmem(struct aa_decls$strec *d,i64 size);
struct aa_decls$opndrec *aa_lib$genreg0(i64 reg,i64 size);
u8 *aa_lib$getfullname(struct aa_decls$strec *d);
u8 *aa_lib$getregname(i64 reg,i64 size);
u8 *aa_lib$xgetregname(i64 reg);
void aa_lib$printst(void *f);
void aa_lib$printstrec(void *f,struct aa_decls$strec *d);
void aa_lib$adddef(struct aa_decls$strec *d);
void aa_lib$addimport(struct aa_decls$strec *d);
void aa_lib$createlabel(struct aa_decls$strec *symptr,i64 symbol);
void aa_lib$createnamedconst(struct aa_decls$strec *symptr,struct aa_decls$opndrec *expr);
void aa_lib$createregalias(struct aa_decls$strec *symptr,i64 regindex,i64 regsize);
void aa_lib$createxregalias(struct aa_decls$strec *symptr,i64 regindex);
void aa_lib$gerror(u8 *mess);
void aa_lib$serror(u8 *mess);
void aa_lib$serror_s(u8 *mess,u8 *param);
static u8 *aa_lib$inttostr(i64 a);
static u8 *aa_lib$realtostr(r64 a);
struct aa_decls$dbuffer *aa_lib$buffercreate(i64 size);
static void aa_lib$bufferexpand(struct aa_decls$dbuffer *a);
void aa_lib$buffercheck(struct aa_decls$dbuffer *a,i64 n);
i64 aa_lib$bufferlength(struct aa_decls$dbuffer *a);
void *aa_lib$bufferelemptr(struct aa_decls$dbuffer *a,i64 offset);
void aa_lib$addbyte(struct aa_decls$dbuffer *a,i64 x);
void aa_lib$addword(struct aa_decls$dbuffer *a,i64 x);
void aa_lib$adddword(struct aa_decls$dbuffer *a,i64 x);
void aa_lib$addqword(struct aa_decls$dbuffer *a,i64 x);
void aa_lib$printmodulesymbols(void *f);
void aa_lib$printimportsymbols(void *f);
void aa_lib$printdupltable(void *f);
void aa_lib$start(void);
void msysc$m_init(i64 nargs,u8 *(*args)[],u8 *(*envstrings)[]);
i64 msysc$m_getdotindex(u64 a,i64 i);
u64 msysc$m_setdotindex(u64 a,i64 i,i64 x);
i64 msysc$m_getdotslice(u64 a,i64 i,i64 j);
u64 msysc$m_setdotslice(u64 a,i64 i,i64 j,u64 x);
i64 msysc$m_get_nprocs(void);
i64 msysc$m_get_nexports(void);
u8 *msysc$m_get_procname(i64 n);
void *msysc$m_get_procaddr(i64 n);
void *msysc$m_get_procexport(i64 n);
static void msysc$pushio(void);
void msysc$m_print_startfile(void *dev);
void msysc$m_print_startstr(u8 *s);
void msysc$m_print_startptr(u8 **p);
void msysc$m_print_startcon(void);
void msysc$m_print_setfmt(u8 *format);
void msysc$m_print_end(void);
void msysc$m_print_ptr(void *a,u8 *fmtstyle);
void msysc$m_print_i64(i64 a,u8 *fmtstyle);
void msysc$m_print_u64(u64 a,u8 *fmtstyle);
void msysc$m_print_r64(r64 x,u8 *fmtstyle);
void msysc$m_print_r32(r32 x,u8 *fmtstyle);
void msysc$m_print_c8(i64 a,u8 *fmtstyle);
void msysc$m_print_str(u8 *s,u8 *fmtstyle);
void msysc$m_print_newline(void);
void msysc$m_print_nogap(void);
void msysc$m_print_space(void);
void msysc$printstr(u8 *s);
void msysc$printstr_n(u8 *s,i64 n);
void msysc$printstrn_app(u8 *s,i64 length,void *f);
static u8 *msysc$makezstring(u8 *s,i64 n,u8 *local);
static void msysc$freezstring(u8 *t,i64 n);
static void msysc$printchar(i64 ch);
void msysc$nextfmtchars(i64 lastx);
void msysc$strtofmt(u8 *s,i64 slen,struct msysc$fmtrec *fmt);
static i64 msysc$domultichar(u8 *p,i64 n,u8 *dest,struct msysc$fmtrec *fmt);
static i64 msysc$expandstr(u8 *s,u8 *t,i64 n,struct msysc$fmtrec *fmt);
static i64 msysc$u64tostr(u64 aa,u8 *s,u64 base,i64 sep);
static i64 msysc$i64tostrfmt(i64 aa,u8 *s,struct msysc$fmtrec *fmt);
static i64 msysc$u64tostrfmt(i64 aa,u8 *s,struct msysc$fmtrec *fmt);
static i64 msysc$i64mintostr(u8 *s,i64 base,i64 sep);
static i64 msysc$strtostrfmt(u8 *s,u8 *t,i64 n,struct msysc$fmtrec *fmt);
static void msysc$tostr_i64(i64 a,struct msysc$fmtrec *fmt);
static void msysc$tostr_u64(u64 a,struct msysc$fmtrec *fmt);
static void msysc$tostr_r64(r64 x,struct msysc$fmtrec *fmt);
static void msysc$tostr_str(u8 *s,struct msysc$fmtrec *fmt);
static struct msysc$fmtrec *msysc$getfmt(u8 *fmtstyle);
u8 *msysc$strint(i64 a,u8 *fmtstyle);
void msysc$getstrint(i64 a,u8 *dest);
u8 *msysc$strword(u64 a,u8 *fmtstyle);
u8 *msysc$strreal(r64 a,u8 *fmtstyle);
static u8 *msysc$getstr(u8 *s,struct msysc$fmtrec *fmt);
static void msysc$initreadbuffer(void);
void msysc$m_read_conline(void);
void msysc$m_read_fileline(void *f);
void msysc$m_read_strline(u8 *s);
static u8 *msysc$readitem(i64 *itemlength);
i64 msysc$strtoint(u8 *s,i64 length,i64 base);
i64 msysc$m_read_i64(i64 fmt);
r64 msysc$m_read_r64(i64 fmt);
void msysc$m_read_str(u8 *dest,i64 destlen,i64 fmt);
void msysc$readstr(u8 *dest,i64 fmt,i64 destlen);
void msysc$rereadln(void);
void msysc$reread(void);
i64 msysc$valint(u8 *s,i64 fmt);
r64 msysc$valreal(u8 *s);
static void msysc$iconvlcn(u8 *s,i64 n);
static void msysc$iconvucn(u8 *s,i64 n);
static void msysc$convlcstring(u8 *s);
static void msysc$convucstring(u8 *s);
i64 msysc$m_power_i64(i64 n,i64 a);
void msysc$m_intoverflow(void);
void msysc$m_dotindex(u64 i,u64 a);
void msysc$m_dotslice(u64 j,u64 i,u64 a);
void msysc$m_popdotindex(u64 i,u64 *p,u64 x);
void msysc$m_popdotslice(u64 j,u64 i,u64 *p,u64 x);
i64 msysc$m_imin(i64 a,i64 b);
i64 msysc$m_imax(i64 a,i64 b);
r64 msysc$m_sign(r64 x);
r64 msysc$m_tp_i64tor64(i64 a);
i64 msysc$m_tp_r64toi64(r64 x);
i64 msysc$m_tp_reftoi64(void *p);
void *msysc$m_tp_i64toref(i64 a);
void msysc$start(void);
void *mlib$pcm_alloc(i64 n);
void mlib$pcm_free(void *p,i64 n);
void mlib$pcm_freeac(void *p,i64 alloc);
void mlib$pcm_clearmem(void *p,i64 n);
void mlib$pcm_init(void);
i64 mlib$pcm_getac(i64 size);
void *mlib$pcm_newblock(i64 itemsize);
i64 mlib$pcm_round(i64 n);
void *mlib$pcm_allocz(i64 n);
u8 *mlib$pcm_copyheapstring(u8 *s);
u8 *mlib$pcm_copyheapstringn(u8 *s,i64 n);
u8 *mlib$pcm_copyheapblock(u8 *s,i64 length);
void *mlib$allocmem(i64 n);
void *mlib$reallocmem(void *p,i64 n);
void mlib$abortprogram(u8 *s);
i64 mlib$getfilesize(void *handlex);
void mlib$readrandom(void *handlex,byte *mem,i64 offset,i64 size);
i64 mlib$writerandom(void *handlex,byte *mem,i64 offset,i64 size);
i64 mlib$setfilepos(void *file,i64 offset);
i64 mlib$getfilepos(void *file);
byte *mlib$readfile(u8 *filename);
i64 mlib$writefile(u8 *filename,byte *data,i64 size);
i64 mlib$checkfile(u8 *file);
void mlib$readlinen(void *handlex,u8 *buffer,i64 size);
void mlib$iconvlcn(u8 *s,i64 n);
void mlib$iconvucn(u8 *s,i64 n);
u8 *mlib$convlcstring(u8 *s);
u8 *mlib$convucstring(u8 *s);
u8 *mlib$changeext(u8 *s,u8 *newext);
u8 *mlib$extractext(u8 *s,i64 period);
u8 *mlib$extractpath(u8 *s);
u8 *mlib$extractfile(u8 *s);
u8 *mlib$extractbasefile(u8 *s);
u8 *mlib$addext(u8 *s,u8 *newext);
void *mlib$pcm_alloc32(void);
void mlib$pcm_free32(void *p);
void mlib$outbyte(void *f,i64 x);
void mlib$outword16(void *f,u64 x);
void mlib$outword32(void *f,u64 x);
void mlib$outword64(void *f,u64 x);
void mlib$outstring(void *f,u8 *s);
void mlib$outblock(void *f,void *p,i64 n);
i64 mlib$myeof(void *f);
void mlib$strbuffer_add(struct mlib$strbuffer *dest,u8 *s,i64 n);
void mlib$gs_init(struct mlib$strbuffer *dest);
void mlib$gs_free(struct mlib$strbuffer *dest);
void mlib$gs_str(struct mlib$strbuffer *dest,u8 *s);
void mlib$gs_char(struct mlib$strbuffer *dest,i64 c);
void mlib$gs_strn(struct mlib$strbuffer *dest,u8 *s,i64 length);
void mlib$gs_strvar(struct mlib$strbuffer *dest,struct mlib$strbuffer *s);
void mlib$gs_strint(struct mlib$strbuffer *dest,i64 a);
void mlib$gs_strln(struct mlib$strbuffer *dest,u8 *s);
void mlib$gs_strsp(struct mlib$strbuffer *dest,u8 *s);
void mlib$gs_line(struct mlib$strbuffer *dest);
i64 mlib$gs_getcol(struct mlib$strbuffer *dest);
void mlib$gs_leftstr(struct mlib$strbuffer *dest,u8 *s,i64 w,i64 padch);
void mlib$gs_leftint(struct mlib$strbuffer *dest,i64 a,i64 w,i64 padch);
void mlib$gs_padto(struct mlib$strbuffer *dest,i64 col,i64 ch);
void mlib$gs_println(struct mlib$strbuffer *dest,void *f);
i64 mlib$nextcmdparamnew(i64 *paramno,u8 **name,u8 **value,u8 *defext);
static i64 mlib$readnextfileitem(u8 **fileptr,u8 **item);
void mlib$ipadstr(u8 *s,i64 width,u8 *padchar);
u8 *mlib$padstr(u8 *s,i64 width,u8 *padchar);
u8 *mlib$chr(i64 c);
i64 mlib$cmpstring(u8 *s,u8 *t);
i64 mlib$cmpstringn(u8 *s,u8 *t,i64 n);
i64 mlib$eqstring(u8 *s,u8 *t);
i64 mlib$cmpbytes(void *p,void *q,i64 n);
i64 mlib$eqbytes(void *p,void *q,i64 n);
void mlib$mseed(u64 a,u64 b);
u64 mlib$mrandom(void);
i64 mlib$mrandomp(void);
i64 mlib$mrandomint(i64 n);
i64 mlib$mrandomrange(i64 a,i64 b);
r64 mlib$mrandomreal(void);
r64 mlib$mrandomreal1(void);
u8 *mlib$readline(void);
void *mlib$findfunction(u8 *name);
i64 mlib$roundtoblock(i64 n,i64 align);
void mlib$start(void);
extern void *malloc(u64 $1);
extern void *realloc(void *$1,u64 $2);
extern void free(void *$1);
extern void memset(void *$1,i32 $2,u64 $3);
extern void memcpy(void *$1,void *$2,u64 $3);
extern void memmove(void *$1,void *$2,u64 $3);
extern i32 clock(void);
extern i32 ftell(void *$1);
extern i32 fseek(void *$1,i32 $2,i32 $3);
extern u64 fread(void *$1,u64 $2,u64 $3,void *$4);
extern u64 fwrite(void *$1,u64 $2,u64 $3,void *$4);
extern i32 getc(void *$1);
extern i32 ungetc(i32 $1,void *$2);
extern void *fopen(u8 *a,u8 *b);
extern i32 fclose(void *$1);
extern u8 *fgets(u8 *$1,i64 $2,void *$3);
extern i32 remove(u8 *$1);
extern i32 rename(u8 *$1,u8 *$2);
extern i32 getchar(void);
extern void putchar(i32 $1);
extern void setbuf(void *$1,byte *$2);
extern i64 strlen(u8 *$1);
extern u8 *strcpy(u8 *$1,u8 *$2);
extern i32 strcmp(u8 *$1,u8 *$2);
extern i32 strncmp(u8 *$1,u8 *$2,u64 $3);
extern u64 strncpy(u8 *$1,u8 *$2,u64 $3);
extern i32 memcmp(void *$1,void *$2,u64 $3);
extern u8 *strcat(u8 *$1,u8 *$2);
extern i32 tolower(i32 $1);
extern i32 toupper(i32 $1);
extern i32 isalpha(i32 $1);
extern i32 isupper(i32 $1);
extern i32 islower(i32 $1);
extern i32 isalnum(i32 $1);
extern i32 isspace(i32 $1);
extern u8 *strstr(u8 *$1,u8 *$2);
extern i64 atol(u8 *$1);
extern i32 atoi(u8 *$1);
extern r64 strtod(u8 *$1,u8 **$2);
extern u8 *_strdup(u8 *$1);
extern i32 puts(u8 *$1);
extern i32 printf(u8 *$1,...);
extern i32 sprintf(u8 *$1,u8 *$2,...);
extern i32 sscanf(u8 *$1,u8 *$2,...);
extern i32 scanf(u8 *$1,...);
extern i32 rand(void);
extern void srand(u32 $1);
extern i32 system(u8 *$1);
extern i32 fgetc(void *$1);
extern i32 fputc(i32 $1,void *$2);
extern i32 fprintf(void *$1,u8 *$2,...);
extern i32 fputs(u8 *$1,void *$2);
extern i32 feof(void *$1);
extern i32 getch(void);
extern i32 _getch(void);
extern i32 kbhit(void);
extern i32 _mkdir(u8 *$1);
extern i32 mkdir(u8 *$1);
extern u8 *strchr(u8 *$1,i32 $2);
extern i32 _setmode(i32 $1,i32 $2);
extern void _exit(i32 $1);
extern void exit(i32 $1);
extern r64 pow(r64 $1,r64 $2);
extern r64 sin(r64 $1);
extern r64 cos(r64 $1);
extern r64 tan(r64 $1);
extern r64 asin(r64 $1);
extern r64 acos(r64 $1);
extern r64 atan(r64 $1);
extern r64 log(r64 $1);
extern r64 log10(r64 $1);
extern r64 exp(r64 $1);
extern r64 floor(r64 $1);
extern r64 ceil(r64 $1);
extern i64 llabs(i64 $1);
extern void qsort(void *$1,u64 $2,u64 $3,void (*$4)(void));
extern i32 __getmainargs(i32 *$1,void *$2,void *$3,i64 $4,void *$5);
void mclib$start(void);
extern void *GetStdHandle(u32 $1);
extern i64 GetConsoleScreenBufferInfo(void *$1,void *$2);
extern i64 SetConsoleCtrlHandler(void (*$1)(void),i64 $2);
extern i64 SetConsoleMode(void *$1,u32 $2);
extern i64 CreateProcessA(u8 *$1,u8 *$2,void *$3,void *$4,i64 $5,u32 $6,void *$7,u8 *$8,void *$9,void *$10);
extern u32 GetLastError(void);
extern u32 WaitForSingleObject(void *$1,u32 $2);
extern i64 GetExitCodeProcess(void *$1,void *$2);
extern i64 CloseHandle(void *$1);
extern i64 GetNumberOfConsoleInputEvents(void *$1,void *$2);
extern i64 FlushConsoleInputBuffer(void *$1);
extern void *LoadLibraryA(u8 *$1);
extern void *GetProcAddress(void *$1,u8 *$2);
extern void *LoadCursorA(void *$1,u8 *$2);
extern u32 RegisterClassExA(void *$1);
extern i64 DefWindowProcA(void *$1,u32 $2,u64 $3,u64 $4);
extern i64 ReadConsoleInputA(void *$1,void *$2,u32 $3,void *$4);
extern void Sleep(u32 $1);
extern u32 GetModuleFileNameA(void *$1,u8 *$2,u32 $3);
extern void ExitProcess(u32 $1);
extern void PostQuitMessage(i32 $1);
extern void MessageBoxA(i32 x,u8 *message,u8 *caption,i32 y);
extern u32 QueryPerformanceCounter(i64 *$1);
extern u32 QueryPerformanceFrequency(i64 *$1);
extern void *CreateFileA(u8 *$1,u32 $2,u32 $3,void *$4,u32 $5,u32 $6,void *$7);
extern u32 GetFileTime(void *$1,void *$2,void *$3,void *$4);
extern void GetSystemTime(struct mwindows$rsystemtime *$1);
extern void GetLocalTime(struct mwindows$rsystemtime *$1);
extern u64 GetTickCount64(void);
extern u32 PeekMessageA(void *$1,void **$2,u32 $3,u32 $4,u32 $5);
extern u8 *GetCommandLineA(void);
extern void *VirtualAlloc(void *$1,u32 $2,u32 $3,u32 $4);
extern u32 VirtualProtect(void *$1,u32 $2,u32 $3,u32 *$4);
void mwindows$os_init(void);
i64 mwindows$os_execwait(u8 *cmdline,i64 newconsole,u8 *workdir);
i64 mwindows$os_execcmd(u8 *cmdline,i64 newconsole);
i64 mwindows$os_getch(void);
i64 mwindows$os_kbhit(void);
u64 mwindows$os_getdllinst(u8 *name);
void *mwindows$os_getdllprocaddr(i64 hinst,u8 *name);
void mwindows$os_initwindows(void);
void mwindows$os_gxregisterclass(u8 *classname);
i64 mwindows$mainwndproc(void *hwnd,u32 message,u64 wparam,u64 lparam);
void mwindows$os_setmesshandler(void *addr);
i64 mwindows$os_getchx(void);
u8 *mwindows$os_getos(void);
i64 mwindows$os_gethostsize(void);
i64 mwindows$os_shellexec(u8 *opc,u8 *file);
void mwindows$os_sleep(i64 a);
void *mwindows$os_getstdin(void);
void *mwindows$os_getstdout(void);
u8 *mwindows$os_gethostname(void);
u8 *mwindows$os_getmpath(void);
i64 mwindows$os_clock(void);
i64 mwindows$os_ticks(void);
i64 mwindows$os_hptimer(void);
i64 mwindows$os_iswindows(void);
void mwindows$os_getsystime(struct mwindows$rsystemtime *tm);
void mwindows$os_peek(void);
byte *mwindows$os_allocexecmem(i64 n);
void mwindows$start(void);
u64 mwindllc$os_calldllfunction(void (*fnaddr)(void),i64 retcode,i64 nargs,i64 (*args)[],byte (*argcodes)[]);
u64 mwindllc$os_pushargs(u64 (*args)[],i64 nargs,i64 nextra,void (*fnaddr)(void),i64 isfloat);
static i64 mwindllc$calldll_cint(void (*fnaddr)(void),i64 (*params)[],i64 nparams);
static i64 mwindllc$calldll_creal(void (*fnaddr)(void),i64 (*params)[],i64 nparams);
void mwindllc$os_dummycall(r64 a,r64 b,r64 c,r64 d);
void mwindllc$start(void);

/* VARS */
static i64 aacli$logdest = (i64)0;
static byte aacli$fshowmcl;
static byte aacli$fshowss;
static byte aacli$fshowss2;
static byte aacli$fshowsx;
static byte aacli$fshowtiming;
static u8 *  aacli$optionnames[21] = {
    (byte*)"lex",
    (byte*)"parse",
    (byte*)"gen",
    (byte*)"obj",
    (byte*)"dll",
    (byte*)"exe",
    (byte*)"lib",
    (byte*)"run",
    (byte*)"mcl",
    (byte*)"ss",
    (byte*)"ss2",
    (byte*)"sx",
    (byte*)"time",
    (byte*)"s",
    (byte*)"d",
    (byte*)"v",
    (byte*)"q",
    (byte*)"help",
    (byte*)"out",
    (byte*)"main",
    (byte*)"start"
};
static i64 aacli$axlevel = (i64)6;
static u8 *  aacli$inputfile;
static u8 *  aacli$outputfile;
static u8 *  aa_mcxdecls$mcxdirnames[14] = {
    (byte*)"pad_dir",
    (byte*)"version_dir",
    (byte*)"code_dir",
    (byte*)"idata_dir",
    (byte*)"zdata_dir",
    (byte*)"reloc_dir",
    (byte*)"dlls_dir",
    (byte*)"libs_dir",
    (byte*)"importsymbols_dir",
    (byte*)"exportsymbols_dir",
    (byte*)"exportsegs_dir",
    (byte*)"exportoffsets_dir",
    (byte*)"entry_dir",
    (byte*)"end_dir"
};
static u8 *  aa_mcxdecls$mcxrelocnames[6] = {(byte*)"no_rel",(byte*)"locabs32",(byte*)"locabs64",(byte*)"impabs32",(byte*)"impabs64",(byte*)"imprel32"};
static u8 *  aa_mcxdecls$segmentnames[5] = {(byte*)"code",(byte*)"idata",(byte*)"zdata",(byte*)"rodata",(byte*)"impdata_seg"};
static u8 *  aa_mcxdecls$dllnametable[20];
static u64 aa_mcxdecls$dllinsttable[20];
static i64 aa_mcxdecls$ndlllibs;
static u8 *  aa_mcxdecls$libnametable[20];
static struct aa_mcxdecls$librec *  aa_mcxdecls$libtable[20];
static byte aa_mcxdecls$libdefined[20];
static i64 aa_mcxdecls$nlibs;
static u8 *  aa_mcxdecls$symbolnametable[3000];
static byte aa_mcxdecls$symboldefined[3000];
static void *  aa_mcxdecls$symboladdress[3000];
static i16 aa_mcxdecls$symbollibindex[3000];
static byte aa_mcxdecls$symboldllindex[3000];
static i64 aa_mcxdecls$nsymbols;
static i64 aa_decls$lxfileno = (i64)0;
static i64 aa_decls$lxlineno = (i64)0;
static i64 aa_decls$nsourcefiles = (i64)0;
static struct aa_decls$modulerec aa_decls$moduletable[2000];
static u8 *  aa_decls$searchlibs[30];
static u8 *  aa_decls$importlibs[30];
static i64 aa_decls$nmodules;
static i64 aa_decls$nsearchlibs;
static i64 aa_decls$nimportlibs;
static struct aa_decls$strec *  aa_decls$lexhashtable[65536];
static struct aa_decls$strec *  aa_decls$dupltable[65536];
static void *  aa_decls$logdev;
static i64 aa_decls$fverbose = (i64)0;
static i64 aa_decls$fquiet = (i64)0;
static i64 aa_decls$linecount = (i64)0;
static i64 aa_decls$nundefined = (i64)0;
static i64 aa_decls$alineno = (i64)0;
static i64 aa_decls$ss_zdatalen;
static struct aa_decls$dbuffer *  aa_decls$ss_zdata;
static struct aa_decls$dbuffer *  aa_decls$ss_idata;
static struct aa_decls$dbuffer *  aa_decls$ss_code;
static struct aa_decls$relocrec *  aa_decls$ss_idatarelocs;
static struct aa_decls$relocrec *  aa_decls$ss_coderelocs;
static i64 aa_decls$ss_nidatarelocs;
static i64 aa_decls$ss_ncoderelocs;
static struct aa_decls$strec *(*aa_decls$ss_symboltable)[];
static i64 aa_decls$ss_nsymbols;
static i64 aa_decls$ss_symboltablesize;
static struct aa_decls$stlistrec *  aa_decls$globalimportlist;
static struct aa_decls$strec *  aa_decls$modulenamelist;
static i64 aa_decls$currmoduleno;
static u8 *  aa_tables$symbolnames[35] = {
    (byte*)"errorsym",
    (byte*)"commasym",
    (byte*)"colonsym",
    (byte*)"dcolonsym",
    (byte*)"lsqsym",
    (byte*)"rsqsym",
    (byte*)"addsym",
    (byte*)"subsym",
    (byte*)"mulsym",
    (byte*)"eqsym",
    (byte*)"eolsym",
    (byte*)"eofsym",
    (byte*)"hashsym",
    (byte*)"intconstsym",
    (byte*)"realconstsym",
    (byte*)"stringconstsym",
    (byte*)"namesym",
    (byte*)"namedconstsym",
    (byte*)"fwdlocalsym",
    (byte*)"localsym",
    (byte*)"importedsym",
    (byte*)"exportedsym",
    (byte*)"kopcodesym",
    (byte*)"kregsym",
    (byte*)"kxregsym",
    (byte*)"kfregsym",
    (byte*)"kmregsym",
    (byte*)"kjmpccsym",
    (byte*)"ksetccsym",
    (byte*)"kmovccsym",
    (byte*)"kprefixsym",
    (byte*)"ksegnamesym",
    (byte*)"kimportlibsym",
    (byte*)"kimportdllsym",
    (byte*)"kdummysym"
};
static u8 *  aa_tables$mclnames[147] = {
    (byte*)"m_comment",
    (byte*)"m_blank",
    (byte*)"m_end",
    (byte*)"m_labelx",
    (byte*)"m_nop",
    (byte*)"m_param",
    (byte*)"m_assem",
    (byte*)"m_proc",
    (byte*)"m_mov",
    (byte*)"m_newmov",
    (byte*)"m_push",
    (byte*)"m_pop",
    (byte*)"m_lea",
    (byte*)"m_cmovcc",
    (byte*)"m_movd",
    (byte*)"m_movq",
    (byte*)"m_movsx",
    (byte*)"m_movzx",
    (byte*)"m_movsxd",
    (byte*)"m_call",
    (byte*)"m_ret",
    (byte*)"m_retn",
    (byte*)"m_leave",
    (byte*)"m_jmp",
    (byte*)"m_jmpcc",
    (byte*)"m_xchg",
    (byte*)"m_add",
    (byte*)"m_sub",
    (byte*)"m_adc",
    (byte*)"m_sbb",
    (byte*)"m_and",
    (byte*)"m_or",
    (byte*)"m_xor",
    (byte*)"m_newadd",
    (byte*)"m_newsub",
    (byte*)"m_test",
    (byte*)"m_imul",
    (byte*)"m_mul",
    (byte*)"m_imul2",
    (byte*)"m_imul3",
    (byte*)"m_idiv",
    (byte*)"m_div",
    (byte*)"m_cmp",
    (byte*)"m_shl",
    (byte*)"m_sar",
    (byte*)"m_shr",
    (byte*)"m_rol",
    (byte*)"m_ror",
    (byte*)"m_rcl",
    (byte*)"m_rcr",
    (byte*)"m_neg",
    (byte*)"m_not",
    (byte*)"m_inc",
    (byte*)"m_dec",
    (byte*)"m_cbw",
    (byte*)"m_cwd",
    (byte*)"m_cdq",
    (byte*)"m_cqo",
    (byte*)"m_setcc",
    (byte*)"m_bsf",
    (byte*)"m_bsr",
    (byte*)"m_sqrtsd",
    (byte*)"m_sqrtss",
    (byte*)"m_addss",
    (byte*)"m_subss",
    (byte*)"m_mulss",
    (byte*)"m_divss",
    (byte*)"m_addsd",
    (byte*)"m_subsd",
    (byte*)"m_mulsd",
    (byte*)"m_divsd",
    (byte*)"m_comiss",
    (byte*)"m_comisd",
    (byte*)"m_xorpd",
    (byte*)"m_xorps",
    (byte*)"m_andpd",
    (byte*)"m_andps",
    (byte*)"m_pxor",
    (byte*)"m_pand",
    (byte*)"m_cvtss2si",
    (byte*)"m_cvtsd2si",
    (byte*)"m_cvttss2si",
    (byte*)"m_cvttsd2si",
    (byte*)"m_cvtsi2ss",
    (byte*)"m_cvtsi2sd",
    (byte*)"m_cvtsd2ss",
    (byte*)"m_cvtss2sd",
    (byte*)"m_movdqa",
    (byte*)"m_movdqu",
    (byte*)"m_pcmpistri",
    (byte*)"m_pcmpistrm",
    (byte*)"m_fld",
    (byte*)"m_fst",
    (byte*)"m_fstp",
    (byte*)"m_fild",
    (byte*)"m_fist",
    (byte*)"m_fistp",
    (byte*)"m_fadd",
    (byte*)"m_fsub",
    (byte*)"m_fmul",
    (byte*)"m_fdiv",
    (byte*)"m_fsqrt",
    (byte*)"m_fsin",
    (byte*)"m_fcos",
    (byte*)"m_fsincos",
    (byte*)"m_fptan",
    (byte*)"m_fpatan",
    (byte*)"m_fabs",
    (byte*)"m_fchs",
    (byte*)"m_minss",
    (byte*)"m_maxss",
    (byte*)"m_minsd",
    (byte*)"m_maxsd",
    (byte*)"m_db",
    (byte*)"m_dw",
    (byte*)"m_dd",
    (byte*)"m_dq",
    (byte*)"m_segment",
    (byte*)"m_isegment",
    (byte*)"m_zsegment",
    (byte*)"m_csegment",
    (byte*)"m_align",
    (byte*)"m_resb",
    (byte*)"m_resw",
    (byte*)"m_resd",
    (byte*)"m_resq",
    (byte*)"m_xlat",
    (byte*)"m_loopnz",
    (byte*)"m_loopz",
    (byte*)"m_loopcx",
    (byte*)"m_jecxz",
    (byte*)"m_jrcxz",
    (byte*)"m_cmpsb",
    (byte*)"m_cmpsw",
    (byte*)"m_cmpsd",
    (byte*)"m_cmpsq",
    (byte*)"m_rdtsc",
    (byte*)"m_popcnt",
    (byte*)"m_finit",
    (byte*)"m_fldz",
    (byte*)"m_fld1",
    (byte*)"m_fldpi",
    (byte*)"m_fld2t",
    (byte*)"m_fld2e",
    (byte*)"m_fldlg2",
    (byte*)"m_fldln2",
    (byte*)"m_halt"
};
static byte aa_tables$mclnopnds[147] = {
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)2u,
    (u8)3u,
    (u8)1u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)3u,
    (u8)3u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)2u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u
};
static byte aa_tables$mclcodes[147] = {
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)144u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)232u,
    (u8)195u,
    (u8)0u,
    (u8)201u,
    (u8)233u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)5u,
    (u8)2u,
    (u8)3u,
    (u8)4u,
    (u8)1u,
    (u8)6u,
    (u8)0u,
    (u8)5u,
    (u8)0u,
    (u8)5u,
    (u8)4u,
    (u8)0u,
    (u8)0u,
    (u8)7u,
    (u8)6u,
    (u8)7u,
    (u8)4u,
    (u8)7u,
    (u8)5u,
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)3u,
    (u8)3u,
    (u8)2u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)188u,
    (u8)189u,
    (u8)81u,
    (u8)81u,
    (u8)88u,
    (u8)92u,
    (u8)89u,
    (u8)94u,
    (u8)88u,
    (u8)92u,
    (u8)89u,
    (u8)94u,
    (u8)0u,
    (u8)0u,
    (u8)87u,
    (u8)87u,
    (u8)84u,
    (u8)84u,
    (u8)239u,
    (u8)219u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)102u,
    (u8)243u,
    (u8)99u,
    (u8)98u,
    (u8)0u,
    (u8)2u,
    (u8)3u,
    (u8)0u,
    (u8)2u,
    (u8)3u,
    (u8)193u,
    (u8)233u,
    (u8)201u,
    (u8)249u,
    (u8)250u,
    (u8)254u,
    (u8)255u,
    (u8)251u,
    (u8)242u,
    (u8)243u,
    (u8)225u,
    (u8)224u,
    (u8)93u,
    (u8)95u,
    (u8)93u,
    (u8)95u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)4u,
    (u8)8u,
    (u8)215u,
    (u8)224u,
    (u8)225u,
    (u8)226u,
    (u8)227u,
    (u8)227u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)49u,
    (u8)0u,
    (u8)0u,
    (u8)238u,
    (u8)232u,
    (u8)235u,
    (u8)233u,
    (u8)234u,
    (u8)236u,
    (u8)237u,
    (u8)244u
};
static u8 *  aa_tables$regnames[21] = {
    (byte*)"rnone",
    (byte*)"r0",
    (byte*)"r1",
    (byte*)"r2",
    (byte*)"r3",
    (byte*)"r4",
    (byte*)"r5",
    (byte*)"r6",
    (byte*)"r7",
    (byte*)"r8",
    (byte*)"r9",
    (byte*)"r10",
    (byte*)"r11",
    (byte*)"r12",
    (byte*)"r13",
    (byte*)"r14",
    (byte*)"r15",
    (byte*)"r16",
    (byte*)"r17",
    (byte*)"r18",
    (byte*)"r19"
};
static byte aa_tables$regcodes[21] = {
    (u8)0u,
    (u8)0u,
    (u8)10u,
    (u8)11u,
    (u8)7u,
    (u8)3u,
    (u8)6u,
    (u8)12u,
    (u8)13u,
    (u8)14u,
    (u8)15u,
    (u8)1u,
    (u8)2u,
    (u8)8u,
    (u8)9u,
    (u8)5u,
    (u8)4u,
    (u8)4u,
    (u8)7u,
    (u8)5u,
    (u8)6u
};
static u8 *  aa_tables$dregnames[136] = {
    (byte*)"d0",
    (byte*)"d1",
    (byte*)"d2",
    (byte*)"d3",
    (byte*)"d4",
    (byte*)"d5",
    (byte*)"d6",
    (byte*)"d7",
    (byte*)"d8",
    (byte*)"d9",
    (byte*)"d10",
    (byte*)"d11",
    (byte*)"d12",
    (byte*)"d13",
    (byte*)"d14",
    (byte*)"d15",
    (byte*)"a0",
    (byte*)"a1",
    (byte*)"a2",
    (byte*)"a3",
    (byte*)"a4",
    (byte*)"a5",
    (byte*)"a6",
    (byte*)"a7",
    (byte*)"a8",
    (byte*)"a9",
    (byte*)"a10",
    (byte*)"a11",
    (byte*)"a12",
    (byte*)"a13",
    (byte*)"a14",
    (byte*)"a15",
    (byte*)"w0",
    (byte*)"w1",
    (byte*)"w2",
    (byte*)"w3",
    (byte*)"w4",
    (byte*)"w5",
    (byte*)"w6",
    (byte*)"w7",
    (byte*)"w8",
    (byte*)"w9",
    (byte*)"w10",
    (byte*)"w11",
    (byte*)"w12",
    (byte*)"w13",
    (byte*)"w14",
    (byte*)"w15",
    (byte*)"b0",
    (byte*)"b1",
    (byte*)"b2",
    (byte*)"b3",
    (byte*)"b4",
    (byte*)"b5",
    (byte*)"b6",
    (byte*)"b7",
    (byte*)"b8",
    (byte*)"b9",
    (byte*)"b10",
    (byte*)"b11",
    (byte*)"b12",
    (byte*)"b13",
    (byte*)"b14",
    (byte*)"b15",
    (byte*)"b16",
    (byte*)"b17",
    (byte*)"b18",
    (byte*)"b19",
    (byte*)"rax",
    (byte*)"rbx",
    (byte*)"rcx",
    (byte*)"rdx",
    (byte*)"rsi",
    (byte*)"rdi",
    (byte*)"rbp",
    (byte*)"rsp",
    (byte*)"r8",
    (byte*)"r9",
    (byte*)"r10",
    (byte*)"r11",
    (byte*)"r12",
    (byte*)"r13",
    (byte*)"r14",
    (byte*)"r15",
    (byte*)"eax",
    (byte*)"ebx",
    (byte*)"ecx",
    (byte*)"edx",
    (byte*)"esi",
    (byte*)"edi",
    (byte*)"ebp",
    (byte*)"esp",
    (byte*)"r8d",
    (byte*)"r9d",
    (byte*)"r10d",
    (byte*)"r11d",
    (byte*)"r12d",
    (byte*)"r13d",
    (byte*)"r14d",
    (byte*)"r15d",
    (byte*)"ax",
    (byte*)"bx",
    (byte*)"cx",
    (byte*)"dx",
    (byte*)"si",
    (byte*)"di",
    (byte*)"bp",
    (byte*)"sp",
    (byte*)"r8w",
    (byte*)"r9w",
    (byte*)"r10w",
    (byte*)"r11w",
    (byte*)"r12w",
    (byte*)"r13w",
    (byte*)"r14w",
    (byte*)"r15w",
    (byte*)"al",
    (byte*)"bl",
    (byte*)"cl",
    (byte*)"dl",
    (byte*)"ah",
    (byte*)"bh",
    (byte*)"ch",
    (byte*)"dh",
    (byte*)"sil",
    (byte*)"dil",
    (byte*)"bpl",
    (byte*)"spl",
    (byte*)"r8b",
    (byte*)"r9b",
    (byte*)"r10b",
    (byte*)"r11b",
    (byte*)"r12b",
    (byte*)"r13b",
    (byte*)"r14b",
    (byte*)"r15b"
};
static byte aa_tables$regsizes[136] = {
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u
};
static byte aa_tables$regindices[136] = {
    (u8)1u,
    (u8)2u,
    (u8)3u,
    (u8)4u,
    (u8)5u,
    (u8)6u,
    (u8)7u,
    (u8)8u,
    (u8)9u,
    (u8)10u,
    (u8)11u,
    (u8)12u,
    (u8)13u,
    (u8)14u,
    (u8)15u,
    (u8)16u,
    (u8)1u,
    (u8)2u,
    (u8)3u,
    (u8)4u,
    (u8)5u,
    (u8)6u,
    (u8)7u,
    (u8)8u,
    (u8)9u,
    (u8)10u,
    (u8)11u,
    (u8)12u,
    (u8)13u,
    (u8)14u,
    (u8)15u,
    (u8)16u,
    (u8)1u,
    (u8)2u,
    (u8)3u,
    (u8)4u,
    (u8)5u,
    (u8)6u,
    (u8)7u,
    (u8)8u,
    (u8)9u,
    (u8)10u,
    (u8)11u,
    (u8)12u,
    (u8)13u,
    (u8)14u,
    (u8)15u,
    (u8)16u,
    (u8)1u,
    (u8)2u,
    (u8)3u,
    (u8)4u,
    (u8)5u,
    (u8)6u,
    (u8)7u,
    (u8)8u,
    (u8)9u,
    (u8)10u,
    (u8)11u,
    (u8)12u,
    (u8)13u,
    (u8)14u,
    (u8)15u,
    (u8)16u,
    (u8)17u,
    (u8)18u,
    (u8)19u,
    (u8)20u,
    (u8)1u,
    (u8)5u,
    (u8)11u,
    (u8)12u,
    (u8)6u,
    (u8)4u,
    (u8)15u,
    (u8)16u,
    (u8)13u,
    (u8)14u,
    (u8)2u,
    (u8)3u,
    (u8)7u,
    (u8)8u,
    (u8)9u,
    (u8)10u,
    (u8)1u,
    (u8)5u,
    (u8)11u,
    (u8)12u,
    (u8)6u,
    (u8)4u,
    (u8)15u,
    (u8)16u,
    (u8)13u,
    (u8)14u,
    (u8)2u,
    (u8)3u,
    (u8)7u,
    (u8)8u,
    (u8)9u,
    (u8)10u,
    (u8)1u,
    (u8)5u,
    (u8)11u,
    (u8)12u,
    (u8)6u,
    (u8)4u,
    (u8)15u,
    (u8)16u,
    (u8)13u,
    (u8)14u,
    (u8)2u,
    (u8)3u,
    (u8)7u,
    (u8)8u,
    (u8)9u,
    (u8)10u,
    (u8)1u,
    (u8)5u,
    (u8)11u,
    (u8)12u,
    (u8)17u,
    (u8)18u,
    (u8)19u,
    (u8)20u,
    (u8)6u,
    (u8)4u,
    (u8)15u,
    (u8)16u,
    (u8)13u,
    (u8)14u,
    (u8)2u,
    (u8)3u,
    (u8)7u,
    (u8)8u,
    (u8)9u,
    (u8)10u
};
static u8 *  aa_tables$xregnames[16] = {
    (byte*)"xmm0",
    (byte*)"xmm1",
    (byte*)"xmm2",
    (byte*)"xmm3",
    (byte*)"xmm4",
    (byte*)"xmm5",
    (byte*)"xmm6",
    (byte*)"xmm7",
    (byte*)"xmm8",
    (byte*)"xmm9",
    (byte*)"xmm10",
    (byte*)"xmm11",
    (byte*)"xmm12",
    (byte*)"xmm13",
    (byte*)"xmm14",
    (byte*)"xmm15"
};
static u8 *  aa_tables$fregnames[8] = {(byte*)"st0",(byte*)"st1",(byte*)"st2",(byte*)"st3",(byte*)"st4",(byte*)"st5",(byte*)"st6",(byte*)"st7"};
static u8 *  aa_tables$mregnames[8] = {(byte*)"mmx0",(byte*)"mmx1",(byte*)"mmx2",(byte*)"mmx3",(byte*)"mmx4",(byte*)"mmx5",(byte*)"mmx6",(byte*)"mmx7"};
static u8 *  aa_tables$condnames[16] = {
    (byte*)"o",
    (byte*)"no",
    (byte*)"b",
    (byte*)"ae",
    (byte*)"z",
    (byte*)"nz",
    (byte*)"be",
    (byte*)"a",
    (byte*)"s",
    (byte*)"ns",
    (byte*)"p",
    (byte*)"np",
    (byte*)"l",
    (byte*)"ge",
    (byte*)"le",
    (byte*)"g"
};
static u8 *  aa_tables$jmpccnames[18] = {
    (byte*)"jo",
    (byte*)"jno",
    (byte*)"jb",
    (byte*)"jae",
    (byte*)"jz",
    (byte*)"jnz",
    (byte*)"jbe",
    (byte*)"ja",
    (byte*)"js",
    (byte*)"jns",
    (byte*)"jp",
    (byte*)"jnp",
    (byte*)"jl",
    (byte*)"jge",
    (byte*)"jle",
    (byte*)"jg",
    (byte*)"jc",
    (byte*)"jnc"
};
static byte aa_tables$jmpcccodes[18] = {
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)3u,
    (u8)4u,
    (u8)5u,
    (u8)6u,
    (u8)7u,
    (u8)8u,
    (u8)9u,
    (u8)10u,
    (u8)11u,
    (u8)12u,
    (u8)13u,
    (u8)14u,
    (u8)15u,
    (u8)2u,
    (u8)3u
};
static u8 *  aa_tables$setccnames[18] = {
    (byte*)"seto",
    (byte*)"setno",
    (byte*)"setb",
    (byte*)"setae",
    (byte*)"setz",
    (byte*)"setnz",
    (byte*)"setbe",
    (byte*)"seta",
    (byte*)"sets",
    (byte*)"setns",
    (byte*)"setp",
    (byte*)"setnp",
    (byte*)"setl",
    (byte*)"setge",
    (byte*)"setle",
    (byte*)"setg",
    (byte*)"setc",
    (byte*)"setnc"
};
static byte aa_tables$setcccodes[18] = {
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)3u,
    (u8)4u,
    (u8)5u,
    (u8)6u,
    (u8)7u,
    (u8)8u,
    (u8)9u,
    (u8)10u,
    (u8)11u,
    (u8)12u,
    (u8)13u,
    (u8)14u,
    (u8)15u,
    (u8)2u,
    (u8)3u
};
static u8 *  aa_tables$cmovccnames[18] = {
    (byte*)"cmovo",
    (byte*)"cmovno",
    (byte*)"cmovb",
    (byte*)"cmovae",
    (byte*)"cmovz",
    (byte*)"cmovnz",
    (byte*)"cmovbe",
    (byte*)"cmova",
    (byte*)"cmovs",
    (byte*)"cmovns",
    (byte*)"cmovp",
    (byte*)"cmovnp",
    (byte*)"cmovl",
    (byte*)"cmovge",
    (byte*)"cmovle",
    (byte*)"cmovg",
    (byte*)"cmovc",
    (byte*)"cmovnc"
};
static byte aa_tables$cmovcccodes[18] = {
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)3u,
    (u8)4u,
    (u8)5u,
    (u8)6u,
    (u8)7u,
    (u8)8u,
    (u8)9u,
    (u8)10u,
    (u8)11u,
    (u8)12u,
    (u8)13u,
    (u8)14u,
    (u8)15u,
    (u8)2u,
    (u8)3u
};
static u8 *  aa_tables$prefixnames[16] = {
    (byte*)"byte",
    (byte*)"u8",
    (byte*)"word",
    (byte*)"word16",
    (byte*)"u16",
    (byte*)"word32",
    (byte*)"dword",
    (byte*)"u32",
    (byte*)"word64",
    (byte*)"qword",
    (byte*)"u64",
    (byte*)"tword",
    (byte*)"word80",
    (byte*)"u80",
    (byte*)"word128",
    (byte*)"u128"
};
static byte aa_tables$prefixsizes[16] = {
    (u8)1u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)10u,
    (u8)10u,
    (u8)10u,
    (u8)16u,
    (u8)16u
};
static u8 *  aa_tables$reftypenames[3] = {(byte*)"extern_ref",(byte*)"fwd_ref",(byte*)"back_ref"};
static u8 *  aa_objdecls$relocnames[7] = {(byte*)"abs_rel",(byte*)"addr64_rel",(byte*)"addr32_rel",(byte*)"addr32nb_rel",(byte*)"rel32_rel",(byte*)"rel321_rel",(byte*)"rel8_rel"};
static u8 *  aa_objdecls$coffscopenames[3] = {(byte*)"cofflocal_scope",(byte*)"export_scope",(byte*)"import_scope"};
static i64 aa_lex$lxsymbol;
static i64 aa_lex$lxsubcode;
static i64 aa_lex$lxvalue;
static r64 aa_lex$lxxvalue;
static u8 *  aa_lex$lxsvalue;
static i64 aa_lex$lxlength;
static i64 aa_lex$lxhashvalue;
static byte *  aa_lex$lxsptr;
static byte *  aa_lex$lxstart;
static struct aa_decls$strec *  aa_lex$lxsymptr;
static u8 aa_lex$alphamap[256];
static u8 aa_lex$digitmap[256];
static u8 aa_lex$commentmap[256];
static struct aa_decls$strec *  aa_parse$exprlabeldef;
static i64 aa_parse$exprvalue;
static i64 aa_parse$exprtype;
static i64 aa_showss$nsymimports = (i64)0;
static i64 aa_showss$nsymexports = (i64)0;
static i64 aa_writeobj$symtaboffset;
static byte *  aa_writeobj$datastart;
static byte *  aa_writeobj$dataptr;
static struct aa_objdecls$imagesymbol aa_writeobj$symboltable[13001];
static i64 aa_writeobj$nsymbols;
static i64 aa_writeobj$stoffset = (i64)0;
static u8 *  aa_writeobj$stringtable[5000];
static i64 aa_writeobj$stringlengths[5000];
static i64 aa_writeobj$nextstringoffset = (i64)0;
static i64 aa_writeobj$nstrings = (i64)0;
static i64 aa_writeexe$libinsttable[30];
static u8 *  aa_writeexe$libinstnames[30];
static i64 aa_writeexe$libnotable[30];
static struct aa_writeexe$basereloc *  aa_writeexe$basereloclist;
static i64 aa_writeexe$nbaserelocs;
static i64 aa_writeexe$maxrelocaddr;
static i64 aa_writeexe$blockbases[500];
static i32 aa_writeexe$blockcounts[500];
static i32 aa_writeexe$blockbytes[500];
static byte aa_writeexe$blockpadding[500];
static i64 aa_writeexe$nbaseblocks;
static i64 aa_writeexe$basetablesize;
static u64 aa_writeexe$imagebase;
static i64 aa_writeexe$imagesize;
static i64 aa_writeexe$filesize;
static i64 (*aa_writeexe$thunktable)[];
static i64 aa_writeexe$fileiatoffset;
static i64 aa_writeexe$fileiatsize;
static struct aa_decls$strec *  aa_writeexe$stentrypoint;
static struct aa_decls$strec *  aa_writeexe$stentrypoint2;
static struct aa_decls$strec *  aa_writeexe$stentrypoint3;
static struct aa_objdecls$sectionrec aa_writeexe$sectiontable[10];
static i64 aa_writeexe$nsections;
static byte *  aa_writeexe$importdir;
static struct aa_objdecls$importrec aa_writeexe$importtable[1000];
static i64 aa_writeexe$nimports;
static struct aa_objdecls$exportrec aa_writeexe$exporttable[3000];
static i64 aa_writeexe$nexports;
static u8 *  aa_writeexe$dllfilename;
static i64 aa_writeexe$isdll;
static struct aa_objdecls$dllrec aa_writeexe$dlltable[50];
static i64 aa_writeexe$ndlls;
static byte *  aa_writeexe$datastart;
static byte *  aa_writeexe$dataptr;
static u8 *  aa_writeexe$userentrypoint;
static i64 aa_writeexe$exportdirvirtaddr;
static i64 aa_writeexe$exportdirvirtsize;
static i64 aa_writeexe$exportdiroffset;
static i64 aa_writeexe$blockdirvirtaddr;
static i64 aa_writeexe$blockdirvirtsize;
static i64 aa_writeexe$blockdiroffset;
static i64 aa_disasm$nmodules;
static i64 aa_disasm$xfchsmask_pd;
static u8 *  aa_disasm$opnames[8] = {(byte*)"add",(byte*)"or",(byte*)"adc",(byte*)"sbb",(byte*)"and",(byte*)"sub",(byte*)"xor",(byte*)"cmp"};
static u8 *  aa_disasm$condnames[16] = {
    (byte*)"o",
    (byte*)"no",
    (byte*)"b",
    (byte*)"ae",
    (byte*)"z",
    (byte*)"nz",
    (byte*)"be",
    (byte*)"a",
    (byte*)"s",
    (byte*)"ns",
    (byte*)"p",
    (byte*)"np",
    (byte*)"l",
    (byte*)"ge",
    (byte*)"le",
    (byte*)"g"
};
static u8 *  aa_disasm$addrmodenames[3] = {(byte*)"amreg",(byte*)"ammem",(byte*)"amrel"};
static i64 aa_disasm$rex;
static i64 aa_disasm$addrmode;
static i64 aa_disasm$rmreg;
static i64 aa_disasm$rmopc;
static i64 aa_disasm$ripmode;
static i64 aa_disasm$basereg;
static i64 aa_disasm$indexreg;
static i64 aa_disasm$scale;
static i64 aa_disasm$opsize;
static i64 aa_disasm$offset;
static i64 aa_disasm$offsetsize;
static i64 aa_disasm$sizeoverride;
static i64 aa_disasm$addroverride;
static i64 aa_disasm$f2override;
static i64 aa_disasm$f3override;
static u8 aa_disasm$deststr[256];
static u8 *  aa_disasm$destptr;
static byte *  aa_disasm$codeptr;
static byte aa_genss$rex;
static byte aa_genss$sizeoverride;
static byte aa_genss$addroverride;
static byte aa_genss$f2override;
static byte aa_genss$f3override;
static byte aa_genss$nowmask;
static byte aa_genss$usesizeb;
static struct aa_decls$opndrec *  aa_genss$extraparam;
static i64 aa_genss$currseg = (i64)0;
static struct aa_decls$dbuffer *  aa_genss$currdata;
static struct aa_decls$relocrec *  aa_genss$currrelocs;
static i64 aa_genss$nrelocs;
static struct aa_lib$mclrec *  aa_genss$currmcl;
static u8 *  aa_genss$opnames[8] = {(byte*)"add",(byte*)"or",(byte*)"adc",(byte*)"sbb",(byte*)"and",(byte*)"sub",(byte*)"xor",(byte*)"cmp"};
static u8 *  aa_lib$opndnames[7] = {(byte*)"a_none",(byte*)"a_reg",(byte*)"a_imm",(byte*)"a_mem",(byte*)"a_cond",(byte*)"a_xreg",(byte*)"a_string"};
static i64 aa_lib$currsegment = (i64)0;
static struct aa_decls$opndrec aa_lib$dstackopnd;
static struct aa_decls$opndrec aa_lib$dframeopnd;
static i64 aa_lib$labelno = (i64)0;
static struct aa_decls$opndrec *  aa_lib$zero_opnd = 0;
static struct aa_lib$mclrec *  aa_lib$mccode;
static struct aa_lib$mclrec *  aa_lib$mccodex;
static struct mlib$strbuffer aa_lib$destv;
static struct mlib$strbuffer *  aa_lib$dest = (struct mlib$strbuffer *)&aa_lib$destv;
static struct aa_decls$opndrec *  aa_lib$regtable[20][8];
static void *  msysc$_fnaddresses[]= {
    &main,
    &aacli$loadsourcefiles,
    &aacli$parsemodules,
    &aacli$fixopnd,
    &aacli$initlogfile,
    &aacli$closelogfile,
    &aacli$initall,
    &aacli$lextest,
    &aacli$getinputoptions,
    &aacli$do_option,
    &aacli$showhelp,
    &aacli$showcaption,
    &aacli$loaderror,
    &aacli$loaderror_s,
    &aacli$addmodule,
    &aacli$addsearchlib,
    &aacli$addimportlib,
    &aacli$showmodules,
    &aacli$getemptyst,
    &aacli$findduplname,
    &aacli$adddupl,
    &aacli$scanglobals,
    &aacli$resethashtable,
    &aacli$start,
    &aa_mcxdecls$start,
    &aa_decls$start,
    &aa_tables$start,
    &aa_objdecls$start,
    &aa_lex$lex,
    &aa_lex$initlex,
    &aa_lex$readreal,
    &aa_lex$readnumber,
    &aa_lex$readbinary,
    &aa_lex$readhex,
    &aa_lex$ps,
    &aa_lex$printsymbol,
    &aa_lex$clearhashtable,
    &aa_lex$inithashtable,
    &aa_lex$addreservedword,
    &aa_lex$printhashtable,
    &aa_lex$lookuplex,
    &aa_lex$initsourcefile,
    &aa_lex$addnamestr,
    &aa_lex$lxerror,
    &aa_lex$gethashvalue,
    &aa_lex$skiptoeol,
    &aa_lex$makestring,
    &aa_lex$start,
    &aa_parse$readmodule,
    &aa_parse$checkundefined,
    &aa_parse$checksymbol,
    &aa_parse$readinstr,
    &aa_parse$readcondinstr,
    &aa_parse$readoperand,
    &aa_parse$readexpression,
    &aa_parse$readterm,
    &aa_parse$readreg,
    &aa_parse$readaddrmode,
    &aa_parse$start,
    &aa_showss$writemcx,
    &aa_showss$showssdata,
    &aa_showss$showsectiondata,
    &aa_showss$showsectioncode,
    &aa_showss$gs_value,
    &aa_showss$showsymboltable2,
    &aa_showss$showimporttable,
    &aa_showss$showsections,
    &aa_showss$writerelocs,
    &aa_showss$scansymbols,
    &aa_showss$writesymbols,
    &aa_showss$roundsegment,
    &aa_showss$showsectionrelocs2,
    &aa_showss$start,
    &aa_writeobj$writess,
    &aa_writeobj$writerecord,
    &aa_writeobj$writerelocs,
    &aa_writeobj$writedata,
    &aa_writeobj$writesymboltable,
    &aa_writeobj$writestringtable,
    &aa_writeobj$makesymbol,
    &aa_writeobj$addsymbol,
    &aa_writeobj$initsymboltable,
    &aa_writeobj$strtoaux,
    &aa_writeobj$sectiontoaux,
    &aa_writeobj$addstringentry,
    &aa_writeobj$convertsymboltable,
    &aa_writeobj$writecoff,
    &aa_writeobj$start,
    &aa_writeexe$writeexe,
    &aa_writeexe$genexe,
    &aa_writeexe$loadlibs,
    &aa_writeexe$initsectiontable,
    &aa_writeexe$extractlibname,
    &aa_writeexe$scanst,
    &aa_writeexe$relocdata,
    &aa_writeexe$getbaserelocs,
    &aa_writeexe$writerecordx,
    &aa_writeexe$writedosstub,
    &aa_writeexe$writepesig,
    &aa_writeexe$writepadding,
    &aa_writeexe$writefileheader,
    &aa_writeexe$writeoptheader,
    &aa_writeexe$writesectionheader,
    &aa_writeexe$writesectiondata,
    &aa_writeexe$getoffsets,
    &aa_writeexe$getsectionno,
    &aa_writeexe$writeexporttable,
    &aa_writeexe$getexporttablesize,
    &aa_writeexe$newbasereloc,
    &aa_writeexe$scanbaserelocs,
    &aa_writeexe$writebasereloctable,
    &aa_writeexe$sortexports,
    &aa_writeexe$start,
    &aa_writess$writessdata,
    &aa_writess$showssdata,
    &aa_writess$showsectiondata,
    &aa_writess$showsectioncode,
    &aa_writess$showsectionrelocs2,
    &aa_writess$gs_value,
    &aa_writess$showsymboltable2,
    &aa_writess$showimporttable,
    &aa_writess$showsections,
    &aa_writess$start,
    &aa_disasm$decodeinstr,
    &aa_disasm$decodetwobyteinstr,
    &aa_disasm$decodeaddr,
    &aa_disasm$readbyte,
    &aa_disasm$readsbyte,
    &aa_disasm$readword16,
    &aa_disasm$readint16,
    &aa_disasm$readword32,
    &aa_disasm$readint32,
    &aa_disasm$readint64,
    &aa_disasm$getreg,
    &aa_disasm$strreg,
    &aa_disasm$strfreg,
    &aa_disasm$printaddrmode,
    &aa_disasm$genstr,
    &aa_disasm$genintd,
    &aa_disasm$genhex,
    &aa_disasm$readimm,
    &aa_disasm$readimm8,
    &aa_disasm$strxmm,
    &aa_disasm$strmmx,
    &aa_disasm$decode8087,
    &aa_disasm$do87arith,
    &aa_disasm$do87mem,
    &aa_disasm$getsil,
    &aa_disasm$getsilx,
    &aa_disasm$start,
    &aa_genss$genss,
    &aa_genss$doinstr,
    &aa_genss$genbyte,
    &aa_genss$genword,
    &aa_genss$gendword,
    &aa_genss$genqword,
    &aa_genss$genopnd,
    &aa_genss$addrelocitem,
    &aa_genss$getstindex,
    &aa_genss$genrel32,
    &aa_genss$genabs32,
    &aa_genss$genabs64,
    &aa_genss$getrel32,
    &aa_genss$dofwdrefs,
    &aa_genss$genrex,
    &aa_genss$isbytesized,
    &aa_genss$isdwordsized,
    &aa_genss$do_push,
    &aa_genss$do_pop,
    &aa_genss$do_inc,
    &aa_genss$do_neg,
    &aa_genss$genamode,
    &aa_genss$makemodrm,
    &aa_genss$setopsize,
    &aa_genss$getdispsize,
    &aa_genss$genrmbyte,
    &aa_genss$makeam,
    &aa_genss$do_lea,
    &aa_genss$do_movsx,
    &aa_genss$checkhighreg,
    &aa_genss$do_exch,
    &aa_genss$do_movsxd,
    &aa_genss$do_imul2,
    &aa_genss$do_shift,
    &aa_genss$do_test,
    &aa_genss$do_loop,
    &aa_genss$do_jcxz,
    &aa_genss$do_setcc,
    &aa_genss$do_arithxmm,
    &aa_genss$do_logicxmm,
    &aa_genss$do_convertfloat,
    &aa_genss$do_fix,
    &aa_genss$do_float,
    &aa_genss$do_call,
    &aa_genss$do_jmp,
    &aa_genss$getcurrdatalen,
    &aa_genss$do_cmovcc,
    &aa_genss$do_fmem,
    &aa_genss$getr32bits,
    &aa_genss$genrel8,
    &aa_genss$checkshortjump,
    &aa_genss$addfwdref,
    &aa_genss$switchseg,
    &aa_genss$do_popcnt,
    &aa_genss$do_bsf,
    &aa_genss$extendsymboltable,
    &aa_genss$do_pcmpistri,
    &aa_genss$genxrm,
    &aa_genss$genrrm,
    &aa_genss$getregcode,
    &aa_genss$checkimmrange,
    &aa_genss$newgenrm,
    &aa_genss$do_mov,
    &aa_genss$do_arith,
    &aa_genss$do_movxmm,
    &aa_genss$checksize,
    &aa_genss$start,
    &aa_lib$initlib,
    &aa_lib$genmc,
    &aa_lib$genmcstr,
    &aa_lib$newopnd,
    &aa_lib$genxreg,
    &aa_lib$genindex,
    &aa_lib$writemclblock,
    &aa_lib$gencomment,
    &aa_lib$genstrimm,
    &aa_lib$getsizetag,
    &aa_lib$writemcl,
    &aa_lib$strmcl,
    &aa_lib$stropnd,
    &aa_lib$strdef,
    &aa_lib$setsegment,
    &aa_lib$getsizeprefix,
    &aa_lib$needsizeprefix,
    &aa_lib$genimm_expr,
    &aa_lib$genint,
    &aa_lib$genlab,
    &aa_lib$genmem,
    &aa_lib$genreg0,
    &aa_lib$getfullname,
    &aa_lib$getregname,
    &aa_lib$xgetregname,
    &aa_lib$printst,
    &aa_lib$printstrec,
    &aa_lib$adddef,
    &aa_lib$addimport,
    &aa_lib$createlabel,
    &aa_lib$createnamedconst,
    &aa_lib$createregalias,
    &aa_lib$createxregalias,
    &aa_lib$gerror,
    &aa_lib$serror,
    &aa_lib$serror_s,
    &aa_lib$inttostr,
    &aa_lib$realtostr,
    &aa_lib$buffercreate,
    &aa_lib$bufferexpand,
    &aa_lib$buffercheck,
    &aa_lib$bufferlength,
    &aa_lib$bufferelemptr,
    &aa_lib$addbyte,
    &aa_lib$addword,
    &aa_lib$adddword,
    &aa_lib$addqword,
    &aa_lib$printmodulesymbols,
    &aa_lib$printimportsymbols,
    &aa_lib$printdupltable,
    &aa_lib$start,
    &msysc$m_init,
    &msysc$m_getdotindex,
    &msysc$m_setdotindex,
    &msysc$m_getdotslice,
    &msysc$m_setdotslice,
    &msysc$m_get_nprocs,
    &msysc$m_get_nexports,
    &msysc$m_get_procname,
    &msysc$m_get_procaddr,
    &msysc$m_get_procexport,
    &msysc$pushio,
    &msysc$m_print_startfile,
    &msysc$m_print_startstr,
    &msysc$m_print_startptr,
    &msysc$m_print_startcon,
    &msysc$m_print_setfmt,
    &msysc$m_print_end,
    &msysc$m_print_ptr,
    &msysc$m_print_i64,
    &msysc$m_print_u64,
    &msysc$m_print_r64,
    &msysc$m_print_r32,
    &msysc$m_print_c8,
    &msysc$m_print_str,
    &msysc$m_print_newline,
    &msysc$m_print_nogap,
    &msysc$m_print_space,
    &msysc$printstr,
    &msysc$printstr_n,
    &msysc$printstrn_app,
    &msysc$makezstring,
    &msysc$freezstring,
    &msysc$printchar,
    &msysc$nextfmtchars,
    &msysc$strtofmt,
    &msysc$domultichar,
    &msysc$expandstr,
    &msysc$u64tostr,
    &msysc$i64tostrfmt,
    &msysc$u64tostrfmt,
    &msysc$i64mintostr,
    &msysc$strtostrfmt,
    &msysc$tostr_i64,
    &msysc$tostr_u64,
    &msysc$tostr_r64,
    &msysc$tostr_str,
    &msysc$getfmt,
    &msysc$strint,
    &msysc$getstrint,
    &msysc$strword,
    &msysc$strreal,
    &msysc$getstr,
    &msysc$initreadbuffer,
    &msysc$m_read_conline,
    &msysc$m_read_fileline,
    &msysc$m_read_strline,
    &msysc$readitem,
    &msysc$strtoint,
    &msysc$m_read_i64,
    &msysc$m_read_r64,
    &msysc$m_read_str,
    &msysc$readstr,
    &msysc$rereadln,
    &msysc$reread,
    &msysc$valint,
    &msysc$valreal,
    &msysc$iconvlcn,
    &msysc$iconvucn,
    &msysc$convlcstring,
    &msysc$convucstring,
    &msysc$m_power_i64,
    &msysc$m_intoverflow,
    &msysc$m_dotindex,
    &msysc$m_dotslice,
    &msysc$m_popdotindex,
    &msysc$m_popdotslice,
    &msysc$m_imin,
    &msysc$m_imax,
    &msysc$m_sign,
    &msysc$m_tp_i64tor64,
    &msysc$m_tp_r64toi64,
    &msysc$m_tp_reftoi64,
    &msysc$m_tp_i64toref,
    &msysc$start,
    &mlib$pcm_alloc,
    &mlib$pcm_free,
    &mlib$pcm_freeac,
    &mlib$pcm_clearmem,
    &mlib$pcm_init,
    &mlib$pcm_getac,
    &mlib$pcm_newblock,
    &mlib$pcm_round,
    &mlib$pcm_allocz,
    &mlib$pcm_copyheapstring,
    &mlib$pcm_copyheapstringn,
    &mlib$pcm_copyheapblock,
    &mlib$allocmem,
    &mlib$reallocmem,
    &mlib$abortprogram,
    &mlib$getfilesize,
    &mlib$readrandom,
    &mlib$writerandom,
    &mlib$setfilepos,
    &mlib$getfilepos,
    &mlib$readfile,
    &mlib$writefile,
    &mlib$checkfile,
    &mlib$readlinen,
    &mlib$iconvlcn,
    &mlib$iconvucn,
    &mlib$convlcstring,
    &mlib$convucstring,
    &mlib$changeext,
    &mlib$extractext,
    &mlib$extractpath,
    &mlib$extractfile,
    &mlib$extractbasefile,
    &mlib$addext,
    &mlib$pcm_alloc32,
    &mlib$pcm_free32,
    &mlib$outbyte,
    &mlib$outword16,
    &mlib$outword32,
    &mlib$outword64,
    &mlib$outstring,
    &mlib$outblock,
    &mlib$myeof,
    &mlib$strbuffer_add,
    &mlib$gs_init,
    &mlib$gs_free,
    &mlib$gs_str,
    &mlib$gs_char,
    &mlib$gs_strn,
    &mlib$gs_strvar,
    &mlib$gs_strint,
    &mlib$gs_strln,
    &mlib$gs_strsp,
    &mlib$gs_line,
    &mlib$gs_getcol,
    &mlib$gs_leftstr,
    &mlib$gs_leftint,
    &mlib$gs_padto,
    &mlib$gs_println,
    &mlib$nextcmdparamnew,
    &mlib$readnextfileitem,
    &mlib$ipadstr,
    &mlib$padstr,
    &mlib$chr,
    &mlib$cmpstring,
    &mlib$cmpstringn,
    &mlib$eqstring,
    &mlib$cmpbytes,
    &mlib$eqbytes,
    &mlib$mseed,
    &mlib$mrandom,
    &mlib$mrandomp,
    &mlib$mrandomint,
    &mlib$mrandomrange,
    &mlib$mrandomreal,
    &mlib$mrandomreal1,
    &mlib$readline,
    &mlib$findfunction,
    &mlib$roundtoblock,
    &mlib$start,
    &mclib$start,
    &mwindows$os_init,
    &mwindows$os_execwait,
    &mwindows$os_execcmd,
    &mwindows$os_getch,
    &mwindows$os_kbhit,
    &mwindows$os_getdllinst,
    &mwindows$os_getdllprocaddr,
    &mwindows$os_initwindows,
    &mwindows$os_gxregisterclass,
    &mwindows$mainwndproc,
    &mwindows$os_setmesshandler,
    &mwindows$os_getchx,
    &mwindows$os_getos,
    &mwindows$os_gethostsize,
    &mwindows$os_shellexec,
    &mwindows$os_sleep,
    &mwindows$os_getstdin,
    &mwindows$os_getstdout,
    &mwindows$os_gethostname,
    &mwindows$os_getmpath,
    &mwindows$os_clock,
    &mwindows$os_ticks,
    &mwindows$os_hptimer,
    &mwindows$os_iswindows,
    &mwindows$os_getsystime,
    &mwindows$os_peek,
    &mwindows$os_allocexecmem,
    &mwindows$start,
    &mwindllc$os_calldllfunction,
    &mwindllc$os_pushargs,
    &mwindllc$calldll_cint,
    &mwindllc$calldll_creal,
    &mwindllc$os_dummycall,
    &mwindllc$start,
0};
static u8 *  msysc$_fnnames[]= {
    (byte*)"main",
    (byte*)"loadsourcefiles",
    (byte*)"parsemodules",
    (byte*)"fixopnd",
    (byte*)"initlogfile",
    (byte*)"closelogfile",
    (byte*)"initall",
    (byte*)"lextest",
    (byte*)"getinputoptions",
    (byte*)"do_option",
    (byte*)"showhelp",
    (byte*)"showcaption",
    (byte*)"loaderror",
    (byte*)"loaderror_s",
    (byte*)"addmodule",
    (byte*)"addsearchlib",
    (byte*)"addimportlib",
    (byte*)"showmodules",
    (byte*)"getemptyst",
    (byte*)"findduplname",
    (byte*)"adddupl",
    (byte*)"scanglobals",
    (byte*)"resethashtable",
    (byte*)"start",
    (byte*)"start",
    (byte*)"start",
    (byte*)"start",
    (byte*)"start",
    (byte*)"lex",
    (byte*)"initlex",
    (byte*)"readreal",
    (byte*)"readnumber",
    (byte*)"readbinary",
    (byte*)"readhex",
    (byte*)"ps",
    (byte*)"printsymbol",
    (byte*)"clearhashtable",
    (byte*)"inithashtable",
    (byte*)"addreservedword",
    (byte*)"printhashtable",
    (byte*)"lookuplex",
    (byte*)"initsourcefile",
    (byte*)"addnamestr",
    (byte*)"lxerror",
    (byte*)"gethashvalue",
    (byte*)"skiptoeol",
    (byte*)"makestring",
    (byte*)"start",
    (byte*)"readmodule",
    (byte*)"checkundefined",
    (byte*)"checksymbol",
    (byte*)"readinstr",
    (byte*)"readcondinstr",
    (byte*)"readoperand",
    (byte*)"readexpression",
    (byte*)"readterm",
    (byte*)"readreg",
    (byte*)"readaddrmode",
    (byte*)"start",
    (byte*)"writemcx",
    (byte*)"showssdata",
    (byte*)"showsectiondata",
    (byte*)"showsectioncode",
    (byte*)"gs_value",
    (byte*)"showsymboltable2",
    (byte*)"showimporttable",
    (byte*)"showsections",
    (byte*)"writerelocs",
    (byte*)"scansymbols",
    (byte*)"writesymbols",
    (byte*)"roundsegment",
    (byte*)"showsectionrelocs2",
    (byte*)"start",
    (byte*)"writess",
    (byte*)"writerecord",
    (byte*)"writerelocs",
    (byte*)"writedata",
    (byte*)"writesymboltable",
    (byte*)"writestringtable",
    (byte*)"makesymbol",
    (byte*)"addsymbol",
    (byte*)"initsymboltable",
    (byte*)"strtoaux",
    (byte*)"sectiontoaux",
    (byte*)"addstringentry",
    (byte*)"convertsymboltable",
    (byte*)"writecoff",
    (byte*)"start",
    (byte*)"writeexe",
    (byte*)"genexe",
    (byte*)"loadlibs",
    (byte*)"initsectiontable",
    (byte*)"extractlibname",
    (byte*)"scanst",
    (byte*)"relocdata",
    (byte*)"getbaserelocs",
    (byte*)"writerecordx",
    (byte*)"writedosstub",
    (byte*)"writepesig",
    (byte*)"writepadding",
    (byte*)"writefileheader",
    (byte*)"writeoptheader",
    (byte*)"writesectionheader",
    (byte*)"writesectiondata",
    (byte*)"getoffsets",
    (byte*)"getsectionno",
    (byte*)"writeexporttable",
    (byte*)"getexporttablesize",
    (byte*)"newbasereloc",
    (byte*)"scanbaserelocs",
    (byte*)"writebasereloctable",
    (byte*)"sortexports",
    (byte*)"start",
    (byte*)"writessdata",
    (byte*)"showssdata",
    (byte*)"showsectiondata",
    (byte*)"showsectioncode",
    (byte*)"showsectionrelocs2",
    (byte*)"gs_value",
    (byte*)"showsymboltable2",
    (byte*)"showimporttable",
    (byte*)"showsections",
    (byte*)"start",
    (byte*)"decodeinstr",
    (byte*)"decodetwobyteinstr",
    (byte*)"decodeaddr",
    (byte*)"readbyte",
    (byte*)"readsbyte",
    (byte*)"readword16",
    (byte*)"readint16",
    (byte*)"readword32",
    (byte*)"readint32",
    (byte*)"readint64",
    (byte*)"getreg",
    (byte*)"strreg",
    (byte*)"strfreg",
    (byte*)"printaddrmode",
    (byte*)"genstr",
    (byte*)"genintd",
    (byte*)"genhex",
    (byte*)"readimm",
    (byte*)"readimm8",
    (byte*)"strxmm",
    (byte*)"strmmx",
    (byte*)"decode8087",
    (byte*)"do87arith",
    (byte*)"do87mem",
    (byte*)"getsil",
    (byte*)"getsilx",
    (byte*)"start",
    (byte*)"genss",
    (byte*)"doinstr",
    (byte*)"genbyte",
    (byte*)"genword",
    (byte*)"gendword",
    (byte*)"genqword",
    (byte*)"genopnd",
    (byte*)"addrelocitem",
    (byte*)"getstindex",
    (byte*)"genrel32",
    (byte*)"genabs32",
    (byte*)"genabs64",
    (byte*)"getrel32",
    (byte*)"dofwdrefs",
    (byte*)"genrex",
    (byte*)"isbytesized",
    (byte*)"isdwordsized",
    (byte*)"do_push",
    (byte*)"do_pop",
    (byte*)"do_inc",
    (byte*)"do_neg",
    (byte*)"genamode",
    (byte*)"makemodrm",
    (byte*)"setopsize",
    (byte*)"getdispsize",
    (byte*)"genrmbyte",
    (byte*)"makeam",
    (byte*)"do_lea",
    (byte*)"do_movsx",
    (byte*)"checkhighreg",
    (byte*)"do_exch",
    (byte*)"do_movsxd",
    (byte*)"do_imul2",
    (byte*)"do_shift",
    (byte*)"do_test",
    (byte*)"do_loop",
    (byte*)"do_jcxz",
    (byte*)"do_setcc",
    (byte*)"do_arithxmm",
    (byte*)"do_logicxmm",
    (byte*)"do_convertfloat",
    (byte*)"do_fix",
    (byte*)"do_float",
    (byte*)"do_call",
    (byte*)"do_jmp",
    (byte*)"getcurrdatalen",
    (byte*)"do_cmovcc",
    (byte*)"do_fmem",
    (byte*)"getr32bits",
    (byte*)"genrel8",
    (byte*)"checkshortjump",
    (byte*)"addfwdref",
    (byte*)"switchseg",
    (byte*)"do_popcnt",
    (byte*)"do_bsf",
    (byte*)"extendsymboltable",
    (byte*)"do_pcmpistri",
    (byte*)"genxrm",
    (byte*)"genrrm",
    (byte*)"getregcode",
    (byte*)"checkimmrange",
    (byte*)"newgenrm",
    (byte*)"do_mov",
    (byte*)"do_arith",
    (byte*)"do_movxmm",
    (byte*)"checksize",
    (byte*)"start",
    (byte*)"initlib",
    (byte*)"genmc",
    (byte*)"genmcstr",
    (byte*)"newopnd",
    (byte*)"genxreg",
    (byte*)"genindex",
    (byte*)"writemclblock",
    (byte*)"gencomment",
    (byte*)"genstrimm",
    (byte*)"getsizetag",
    (byte*)"writemcl",
    (byte*)"strmcl",
    (byte*)"stropnd",
    (byte*)"strdef",
    (byte*)"setsegment",
    (byte*)"getsizeprefix",
    (byte*)"needsizeprefix",
    (byte*)"genimm_expr",
    (byte*)"genint",
    (byte*)"genlab",
    (byte*)"genmem",
    (byte*)"genreg0",
    (byte*)"getfullname",
    (byte*)"getregname",
    (byte*)"xgetregname",
    (byte*)"printst",
    (byte*)"printstrec",
    (byte*)"adddef",
    (byte*)"addimport",
    (byte*)"createlabel",
    (byte*)"createnamedconst",
    (byte*)"createregalias",
    (byte*)"createxregalias",
    (byte*)"gerror",
    (byte*)"serror",
    (byte*)"serror_s",
    (byte*)"inttostr",
    (byte*)"realtostr",
    (byte*)"buffercreate",
    (byte*)"bufferexpand",
    (byte*)"buffercheck",
    (byte*)"bufferlength",
    (byte*)"bufferelemptr",
    (byte*)"addbyte",
    (byte*)"addword",
    (byte*)"adddword",
    (byte*)"addqword",
    (byte*)"printmodulesymbols",
    (byte*)"printimportsymbols",
    (byte*)"printdupltable",
    (byte*)"start",
    (byte*)"m_init",
    (byte*)"m_getdotindex",
    (byte*)"m_setdotindex",
    (byte*)"m_getdotslice",
    (byte*)"m_setdotslice",
    (byte*)"m_get_nprocs",
    (byte*)"m_get_nexports",
    (byte*)"m_get_procname",
    (byte*)"m_get_procaddr",
    (byte*)"m_get_procexport",
    (byte*)"pushio",
    (byte*)"m_print_startfile",
    (byte*)"m_print_startstr",
    (byte*)"m_print_startptr",
    (byte*)"m_print_startcon",
    (byte*)"m_print_setfmt",
    (byte*)"m_print_end",
    (byte*)"m_print_ptr",
    (byte*)"m_print_i64",
    (byte*)"m_print_u64",
    (byte*)"m_print_r64",
    (byte*)"m_print_r32",
    (byte*)"m_print_c8",
    (byte*)"m_print_str",
    (byte*)"m_print_newline",
    (byte*)"m_print_nogap",
    (byte*)"m_print_space",
    (byte*)"printstr",
    (byte*)"printstr_n",
    (byte*)"printstrn_app",
    (byte*)"makezstring",
    (byte*)"freezstring",
    (byte*)"printchar",
    (byte*)"nextfmtchars",
    (byte*)"strtofmt",
    (byte*)"domultichar",
    (byte*)"expandstr",
    (byte*)"u64tostr",
    (byte*)"i64tostrfmt",
    (byte*)"u64tostrfmt",
    (byte*)"i64mintostr",
    (byte*)"strtostrfmt",
    (byte*)"tostr_i64",
    (byte*)"tostr_u64",
    (byte*)"tostr_r64",
    (byte*)"tostr_str",
    (byte*)"getfmt",
    (byte*)"strint",
    (byte*)"getstrint",
    (byte*)"strword",
    (byte*)"strreal",
    (byte*)"getstr",
    (byte*)"initreadbuffer",
    (byte*)"m_read_conline",
    (byte*)"m_read_fileline",
    (byte*)"m_read_strline",
    (byte*)"readitem",
    (byte*)"strtoint",
    (byte*)"m_read_i64",
    (byte*)"m_read_r64",
    (byte*)"m_read_str",
    (byte*)"readstr",
    (byte*)"rereadln",
    (byte*)"reread",
    (byte*)"valint",
    (byte*)"valreal",
    (byte*)"iconvlcn",
    (byte*)"iconvucn",
    (byte*)"convlcstring",
    (byte*)"convucstring",
    (byte*)"m_power_i64",
    (byte*)"m_intoverflow",
    (byte*)"m_dotindex",
    (byte*)"m_dotslice",
    (byte*)"m_popdotindex",
    (byte*)"m_popdotslice",
    (byte*)"m_imin",
    (byte*)"m_imax",
    (byte*)"m_sign",
    (byte*)"m_tp_i64tor64",
    (byte*)"m_tp_r64toi64",
    (byte*)"m_tp_reftoi64",
    (byte*)"m_tp_i64toref",
    (byte*)"start",
    (byte*)"pcm_alloc",
    (byte*)"pcm_free",
    (byte*)"pcm_freeac",
    (byte*)"pcm_clearmem",
    (byte*)"pcm_init",
    (byte*)"pcm_getac",
    (byte*)"pcm_newblock",
    (byte*)"pcm_round",
    (byte*)"pcm_allocz",
    (byte*)"pcm_copyheapstring",
    (byte*)"pcm_copyheapstringn",
    (byte*)"pcm_copyheapblock",
    (byte*)"allocmem",
    (byte*)"reallocmem",
    (byte*)"abortprogram",
    (byte*)"getfilesize",
    (byte*)"readrandom",
    (byte*)"writerandom",
    (byte*)"setfilepos",
    (byte*)"getfilepos",
    (byte*)"readfile",
    (byte*)"writefile",
    (byte*)"checkfile",
    (byte*)"readlinen",
    (byte*)"iconvlcn",
    (byte*)"iconvucn",
    (byte*)"convlcstring",
    (byte*)"convucstring",
    (byte*)"changeext",
    (byte*)"extractext",
    (byte*)"extractpath",
    (byte*)"extractfile",
    (byte*)"extractbasefile",
    (byte*)"addext",
    (byte*)"pcm_alloc32",
    (byte*)"pcm_free32",
    (byte*)"outbyte",
    (byte*)"outword16",
    (byte*)"outword32",
    (byte*)"outword64",
    (byte*)"outstring",
    (byte*)"outblock",
    (byte*)"myeof",
    (byte*)"strbuffer_add",
    (byte*)"gs_init",
    (byte*)"gs_free",
    (byte*)"gs_str",
    (byte*)"gs_char",
    (byte*)"gs_strn",
    (byte*)"gs_strvar",
    (byte*)"gs_strint",
    (byte*)"gs_strln",
    (byte*)"gs_strsp",
    (byte*)"gs_line",
    (byte*)"gs_getcol",
    (byte*)"gs_leftstr",
    (byte*)"gs_leftint",
    (byte*)"gs_padto",
    (byte*)"gs_println",
    (byte*)"nextcmdparamnew",
    (byte*)"readnextfileitem",
    (byte*)"ipadstr",
    (byte*)"padstr",
    (byte*)"chr",
    (byte*)"cmpstring",
    (byte*)"cmpstringn",
    (byte*)"eqstring",
    (byte*)"cmpbytes",
    (byte*)"eqbytes",
    (byte*)"mseed",
    (byte*)"mrandom",
    (byte*)"mrandomp",
    (byte*)"mrandomint",
    (byte*)"mrandomrange",
    (byte*)"mrandomreal",
    (byte*)"mrandomreal1",
    (byte*)"readline",
    (byte*)"findfunction",
    (byte*)"roundtoblock",
    (byte*)"start",
    (byte*)"start",
    (byte*)"os_init",
    (byte*)"os_execwait",
    (byte*)"os_execcmd",
    (byte*)"os_getch",
    (byte*)"os_kbhit",
    (byte*)"os_getdllinst",
    (byte*)"os_getdllprocaddr",
    (byte*)"os_initwindows",
    (byte*)"os_gxregisterclass",
    (byte*)"mainwndproc",
    (byte*)"os_setmesshandler",
    (byte*)"os_getchx",
    (byte*)"os_getos",
    (byte*)"os_gethostsize",
    (byte*)"os_shellexec",
    (byte*)"os_sleep",
    (byte*)"os_getstdin",
    (byte*)"os_getstdout",
    (byte*)"os_gethostname",
    (byte*)"os_getmpath",
    (byte*)"os_clock",
    (byte*)"os_ticks",
    (byte*)"os_hptimer",
    (byte*)"os_iswindows",
    (byte*)"os_getsystime",
    (byte*)"os_peek",
    (byte*)"os_allocexecmem",
    (byte*)"start",
    (byte*)"os_calldllfunction",
    (byte*)"os_pushargs",
    (byte*)"calldll_cint",
    (byte*)"calldll_creal",
    (byte*)"os_dummycall",
    (byte*)"start",
(byte*)""};
static i64 msysc$_fnnprocs=467;
static i64 msysc$_fnnexports;
static i64 msysc$fmtparam;
static i64 msysc$needgap = (i64)0;
static i64 msysc$outdev = (i64)1;
static void *  msysc$outchan = 0;
static u8 *  msysc$fmtstr = 0;
static void *  msysc$outchan_stack[10];
static i64 msysc$outdev_stack[10];
static u8 *  msysc$fmtstr_stack[10];
static byte msysc$needgap_stack[10];
static u8 *  msysc$ptr_stack[10];
static i64 msysc$niostack = (i64)0;
static u8 msysc$digits[16] = {
    (u8)(i64)48,
    (u8)(i64)49,
    (u8)(i64)50,
    (u8)(i64)51,
    (u8)(i64)52,
    (u8)(i64)53,
    (u8)(i64)54,
    (u8)(i64)55,
    (u8)(i64)56,
    (u8)(i64)57,
    (u8)(i64)65,
    (u8)(i64)66,
    (u8)(i64)67,
    (u8)(i64)68,
    (u8)(i64)69,
    (u8)(i64)70
};
static struct msysc$fmtrec msysc$defaultfmt = {
    (u8)0u,
    (i8)(i64)0,
    (u8)10u,
    (u8)(i64)0,
    (u8)' ',
    (u8)'f',
    (u8)(i64)0,
    (u8)(i64)0,
    (u8)(i64)0,
    (u8)'R',
    (u8)(i64)0,
    (u8)(i64)0,
    (u8)(i64)0,
    (u8)(i64)0,
    (u8)(i64)0,
    (u8)0u
};
static u8 *  msysc$rd_buffer;
static i64 msysc$rd_length;
static u8 *  msysc$rd_pos;
static u8 *  msysc$rd_lastpos;
static i64 msysc$termchar;
static i64 msysc$itemerror;
static i64 msysc$$cmdskip;
static i64 msysc$nsysparams;
static i64 msysc$ncmdparams;
static i64 msysc$nenvstrings;
static u8 *  msysc$sysparams[128];
static u8 *(*msysc$cmdparams)[];
static u8 *(*msysc$envstrings)[];
static u64 msysc$mask63 = (u64)9223372036854775807u;
static r64 msysc$offset64 = (double)9223372036854775800.;
static r64 msysc$offset32 = (double)9223372036854775800.;
static u64 mlib$allocupper[301];
static i64 mlib$alloccode;
static i64 mlib$allocbytes;
static i64 mlib$fdebug = (i64)0;
static i64 mlib$rfsize;
static u64 mlib$maxmemory;
static i64 mlib$maxalloccode;
static void *  mlib$allocbase;
static byte mlib$pcm_setup = (byte)(i64)0;
static i64 mlib$show = (i64)0;
static i64 mlib$memtotal = (i64)0;
static i64 mlib$smallmemtotal = (i64)0;
static i64 mlib$smallmemobjs = (i64)0;
static i64 mlib$maxmemtotal = (i64)0;
static i32 *  mlib$memalloctable[3];
static i32 mlib$memallocsize[3];
static byte *  mlib$pcheapstart;
static byte *  mlib$pcheapend;
static byte *  mlib$pcheapptr;
static byte mlib$sizeindextable[2049];
static u64 *  mlib$freelist[9];
static u8 *  mlib$pmnames[6] = {(byte*)"pm_end",(byte*)"pm_option",(byte*)"pm_sourcefile",(byte*)"pm_libfile",(byte*)"pm_colon",(byte*)"pm_extra"};
static u64 mlib$seed[2] = {(u64)2993073034246558322u,(u64)1617678968452121188u};
static void *  mwindows$hconsole;
static void *  mwindows$hconsolein;
static struct mwindows$input_record mwindows$lastkey;
static struct mwindows$input_record mwindows$pendkey;
static i64 mwindows$keypending;
static i64 (*mwindows$wndproc_callbackfn)(void *) = 0;
static i64 mwindows$init_flag = (i64)0;

/* PROCDEFS */
int main(int _nargs, char** _args, char** _envstrings) {
    msysc$m_init(_nargs, (void*)_args, (void*)_envstrings);

// call main-start() routines...
    msysc$start();
    aacli$start();

        u8 *  ext;
        struct mlib$strbuffer *  ss;
        i64 t;
    t = clock();
    aacli$initall();
    aacli$getinputoptions();
    aacli$inputfile = aa_decls$moduletable[((i64)1)-1].filename;
    aacli$initlogfile();
    if ((aacli$axlevel == (i64)1)) {
        if ((aa_decls$nmodules > (i64)1)) {
            aacli$loaderror((byte*)"lex test/multi files");
        }
;
        aacli$lextest(aacli$inputfile);
    }
    else {
        if ((aacli$outputfile == 0)) {
            aacli$outputfile = aacli$inputfile;
        }
;
        if ((aacli$axlevel==(i64)5)) {
            ext = (byte*)"dll";
        }
        else if ((aacli$axlevel==(i64)4)) {
            ext = (byte*)"obj";
        }
        else if ((aacli$axlevel==(i64)7) || (aacli$axlevel==(i64)8)) {
            ext = (byte*)"lib";
        }
        else {
            ext = (byte*)"exe";
        }
;
        aacli$outputfile = mlib$pcm_copyheapstring(mlib$changeext(aacli$outputfile,ext));
        if (!(!!(aa_decls$fquiet))) {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"Assembling",NULL);
            msysc$m_print_str(aacli$inputfile,NULL);
            msysc$m_print_str((byte*)"to",NULL);
            msysc$m_print_str(aacli$outputfile,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
;
        if (!!(aa_decls$fverbose)) {
            aacli$showcaption();
            msysc$m_print_startcon();
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
;
        aacli$loadsourcefiles();
        aacli$parsemodules();
        aa_genss$genss();
        if ((aacli$axlevel==(i64)4)) {
            if ((!!((i64)aacli$fshowss) || !!((i64)aacli$fshowsx))) {
                aa_writeexe$initsectiontable();
                ss = (struct mlib$strbuffer *)aa_writess$writessdata((i64)0);
                mlib$gs_println((struct mlib$strbuffer *)ss,aa_decls$logdev);
            }
;
            aa_writeobj$writess(aacli$outputfile);
        }
        else if ((aacli$axlevel==(i64)6) || (aacli$axlevel==(i64)5)) {
            aa_writeexe$initsectiontable();
            if (!!((i64)aacli$fshowss)) {
                ss = (struct mlib$strbuffer *)aa_writess$writessdata((i64)0);
                mlib$gs_println((struct mlib$strbuffer *)ss,aa_decls$logdev);
            }
;
            aa_writeexe$genexe(0,aacli$outputfile,(i64)(aacli$axlevel == (i64)5));
            if (!!((i64)aacli$fshowsx)) {
                ss = (struct mlib$strbuffer *)aa_writess$writessdata((i64)1);
                mlib$gs_println((struct mlib$strbuffer *)ss,aa_decls$logdev);
            }
;
            aa_writeexe$writeexe(aacli$outputfile,(i64)(aacli$axlevel == (i64)5));
        }
        else if ((aacli$axlevel==(i64)7) || (aacli$axlevel==(i64)8)) {
            aa_showss$writemcx(aacli$outputfile);
            if (!!((i64)aacli$fshowss2)) {
                ss = (struct mlib$strbuffer *)aa_showss$showssdata();
                mlib$gs_println((struct mlib$strbuffer *)ss,aa_decls$logdev);
            }
;
        }
;
        if (!!((i64)aacli$fshowmcl)) {
            ss = (struct mlib$strbuffer *)aa_lib$writemclblock();
            mlib$gs_println((struct mlib$strbuffer *)ss,aa_decls$logdev);
        }
;
    }
;
    if (!!((i64)aacli$fshowtiming)) {
        t = (clock() - t);
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Time",NULL);
        msysc$m_print_i64(t,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    aacli$closelogfile();
    exit((i64)0);
    return 0;
}

static void aacli$loadsourcefiles(void) {
        i64 i;
        u8 *  source;
    for (i=(i64)1;i<=aa_decls$nmodules;++i) {
L1 :;
        source = (u8 *)mlib$readfile(aa_decls$moduletable[(i)-1].filename);
        if ((source == 0)) {
            aacli$loaderror_s((byte*)"Can't load file: %s",aa_decls$moduletable[(i)-1].filename);
        }
;
        aa_decls$moduletable[(i)-1].source = source;
L2 :;
    }
L3 :;
    ;
}

static void aacli$parsemodules(void) {
        i64 i;
        struct aa_lib$mclrec *  m;
    for (i=(i64)1;i<=aa_decls$nmodules;++i) {
L4 :;
        aa_decls$currmoduleno = i;
        aa_decls$modulenamelist = 0;
        aa_parse$readmodule(i);
        aa_parse$checkundefined();
        if (!!(aa_decls$nundefined)) {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"Couldn't assemble - press key",NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            exit((i64)1);
        }
;
        aacli$scanglobals();
        if (!!((i64)aacli$fshowsx)) {
        }
;
        if ((i != aa_decls$nmodules)) {
            aacli$resethashtable();
        }
;
L5 :;
    }
L6 :;
    ;
    if (!!((i64)aacli$fshowsx)) {
    }
;
    m = (struct aa_lib$mclrec *)aa_lib$mccode;
    L7 :;
    while (!!(m)) {
        aacli$fixopnd((struct aa_decls$opndrec *)(*m).a);
        aacli$fixopnd((struct aa_decls$opndrec *)(*m).b);
        m = (struct aa_lib$mclrec *)(*m).nextmcl;
L8 :;
    }
L9 :;
    ;
}

static void aacli$fixopnd(struct aa_decls$opndrec *a) {
        struct aa_decls$strec *  d;
    if ((a == 0)) {
        return;
    }
;
    if (!!((*a).labeldef)) {
        d = (struct aa_decls$strec *)(*a).labeldef;
        if (!!((*d).basedef)) {
            (*a).labeldef = (struct aa_decls$strec *)(*d).basedef;
        }
;
    }
;
}

static void aacli$initlogfile(void) {
    if ((aacli$logdest==(i64)2)) {
        remove((byte*)"mx.log");
        aa_decls$logdev = fopen((byte*)"mx.log",(byte*)"w");
    }
    else if ((aacli$logdest==(i64)0) || (aacli$logdest==(i64)1)) {
        aa_decls$logdev = 0;
    }
;
}

static void aacli$closelogfile(void) {
        u8 str[512];
    if ((aacli$logdest == (i64)2)) {
        fclose(aa_decls$logdev);
        msysc$m_print_startstr(str);
        msysc$m_print_str((byte*)"\\m\\ed.bat",NULL);
        msysc$m_print_str((byte*)"mx.log",NULL);
        msysc$m_print_end();
        ;
        mwindows$os_execwait((u8 *)str,(i64)1,0);
    }
;
}

static void aacli$initall(void) {
    mlib$pcm_init();
    aa_lex$initlex();
    aa_lib$initlib();
}

static void aacli$lextest(u8 *file) {
    aacli$loadsourcefiles();
    aa_lex$initsourcefile(aa_decls$moduletable[((i64)1)-1].source);
    aa_lex$lxsymbol = (i64)11;
    L10 :;
    while ((aa_lex$lxsymbol != (i64)12)) {
        aa_lex$lex();
L11 :;
    }
L12 :;
    ;
}

static void aacli$getinputoptions(void) {
        i64 paramno;
        i64 pmtype;
        i64 sw;
        u8 *  name;
        u8 *  value;
    paramno = (i64)1;
    L13 :;
    while (!!((pmtype = mlib$nextcmdparamnew(&paramno,&name,&value,(byte*)".asm")))) {
        if ((pmtype==(i64)1)) {
            mlib$convlcstring(name);
            for (sw=(i64)1;sw<=(i64)21;++sw) {
L16 :;
                if (!!(mlib$eqstring(name,aacli$optionnames[(sw)-1]))) {
                    aacli$do_option(sw,value);
                    goto L18 ;
                }
;
L17 :;
            }
            {
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"Unknown option:",NULL);
                msysc$m_print_str(name,NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                exit((i64)1);
            }
L18 :;
            ;
        }
        else if ((pmtype==(i64)2)) {
            aacli$addmodule(name);
        }
        else if ((pmtype==(i64)3)) {
            aacli$addsearchlib(mlib$convlcstring(name));
        }
;
L14 :;
    }
L15 :;
    ;
    if (((aa_decls$nmodules == (i64)0) && (aa_decls$nsearchlibs == (i64)0))) {
        aacli$showcaption();
        msysc$m_print_startcon();
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Usage:",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"\t",NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((*msysc$cmdparams)[((i64)0)],NULL);
        msysc$m_print_str((byte*)"filename[.asm]           # Assemble filename.asm to filename.exe",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"\t",NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((*msysc$cmdparams)[((i64)0)],NULL);
        msysc$m_print_str((byte*)"-help                    # Show other options",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        exit((i64)1);
    }
;
    if ((((!!((i64)aacli$fshowss) || !!((i64)aacli$fshowsx)) || !!((i64)aacli$fshowss2)) || !!((i64)aacli$fshowmcl))) {
        if ((aacli$logdest == (i64)0)) {
            aacli$logdest = (i64)2;
        }
;
    }
;
    aacli$addsearchlib((byte*)"msvcrt");
    aacli$addsearchlib((byte*)"gdi32");
    aacli$addsearchlib((byte*)"user32");
    aacli$addsearchlib((byte*)"kernel32");
    if ((aa_decls$nmodules == (i64)0)) {
        aacli$loaderror((byte*)"No input files specified");
    }
;
}

static void aacli$do_option(i64 sw,u8 *value) {
    if ((sw==(i64)1) || (sw==(i64)2) || (sw==(i64)3) || (sw==(i64)4) || (sw==(i64)6) || (sw==(i64)5) || (sw==(i64)7) || (sw==(i64)8)) {
        aacli$axlevel = sw;
    }
    else if ((sw==(i64)9)) {
        aacli$fshowmcl = (i64)1;
    }
    else if ((sw==(i64)10)) {
        aacli$fshowss = (i64)1;
    }
    else if ((sw==(i64)11)) {
        aacli$fshowss2 = (i64)1;
    }
    else if ((sw==(i64)12)) {
        aacli$fshowsx = (i64)1;
    }
    else if ((sw==(i64)13)) {
        aacli$fshowtiming = (i64)1;
    }
    else if ((sw==(i64)14)) {
        aacli$logdest = (i64)1;
    }
    else if ((sw==(i64)15)) {
        aacli$logdest = (i64)2;
    }
    else if ((sw==(i64)16)) {
        aa_decls$fverbose = (i64)1;
    }
    else if ((sw==(i64)17)) {
        aa_decls$fquiet = (i64)1;
    }
    else if ((sw==(i64)18)) {
        aacli$showhelp();
    }
    else if ((sw==(i64)19)) {
        aacli$outputfile = mlib$pcm_copyheapstring(value);
    }
    else if ((sw==(i64)20)) {
    }
    else if ((sw==(i64)21)) {
    }
;
}

static void aacli$showhelp(void) {
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"'AA' Assembler-Linker for Win64\n\nAssembles ASM files written in a special syntax to OBJ or EXE or DLL format.\n\nUsage:\n\n    aa prog            Assemble prog.asm to prog.exe\n    aa prog -dll       Assemble prog.asm to prog.dll\n    aa prog -obj       Assemble prog.asm to prog.obj (needs ext. linker)\n\n    aa a b c           Assemble&link modules a.asm, b.asm, c.asm into a.exe\n\nOptions:\n\n    -out:file          Name output file (default is .exe applied to 1st module)\n    -exe               Generate executable (default)\n    -obj               Generate object file (one .obj file for multiple i/p files)\n    file.dll           Include library in list of DLLs to search\n\n    @file              Read options and files from @ file\n\nCan only link to external DLL libraries; not other .o/.obj/.lib/.a files.\n\nDLLs msvcrt.dll, user32.dll, gdi32.dll, user32.dll are automatically included.\n",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    exit((i64)1);
}

static void aacli$showcaption(void) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"AA[BX] Assembler/Linker",NULL);
    msysc$m_print_str((byte*)"1-Sep-2023",NULL);
    msysc$m_print_end();
    ;
}

static void aacli$loaderror(u8 *mess) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Error:",NULL);
    msysc$m_print_str(mess,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    exit((i64)1);
}

static void aacli$loaderror_s(u8 *mess,u8 *s) {
        u8 str[256];
    sprintf((u8 *)str,mess,s);
    aacli$loaderror((u8 *)str);
}

static void aacli$addmodule(u8 *name) {
    if ((aa_decls$nmodules >= (i64)2000)) {
        aacli$loaderror((byte*)"Too many modules");
    }
;
    ++(aa_decls$nmodules);
    aa_decls$moduletable[(aa_decls$nmodules)-1].filename = mlib$pcm_copyheapstring(name);
    aa_decls$moduletable[(aa_decls$nmodules)-1].name = mlib$pcm_copyheapstring(mlib$extractfile(name));
    aa_decls$moduletable[(aa_decls$nmodules)-1].source = (byte*)"<empty>";
}

static void aacli$addsearchlib(u8 *name) {
        i64 i;
    if (!!(mlib$eqstring(mlib$extractext(name,(i64)0),(byte*)"mcx"))) {
        aacli$addimportlib(name);
        return;
    }
;
    name = mlib$changeext(name,(byte*)"");
    for (i=(i64)1;i<=aa_decls$nsearchlibs;++i) {
L19 :;
        if (!!(mlib$eqstring(aa_decls$searchlibs[(i)-1],name))) {
            return;
        }
;
L20 :;
    }
L21 :;
    ;
    if ((aa_decls$nsearchlibs >= (i64)30)) {
        aacli$loaderror((byte*)"Too many DLLs");
    }
;
    ++(aa_decls$nsearchlibs);
    aa_decls$searchlibs[(aa_decls$nsearchlibs)-1] = mlib$pcm_copyheapstring(name);
}

static void aacli$addimportlib(u8 *name) {
        i64 i;
    name = mlib$changeext(name,(byte*)"");
    for (i=(i64)1;i<=aa_decls$nimportlibs;++i) {
L22 :;
        if (!!(mlib$eqstring(aa_decls$importlibs[(i)-1],name))) {
            return;
        }
;
L23 :;
    }
L24 :;
    ;
    if ((aa_decls$nimportlibs >= (i64)30)) {
        aacli$loaderror((byte*)"Too many LIBs");
    }
;
    ++(aa_decls$nimportlibs);
    aa_decls$importlibs[(aa_decls$nimportlibs)-1] = mlib$pcm_copyheapstring(name);
}

static void aacli$showmodules(void) {
        i64 i;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Modules:",NULL);
    msysc$m_print_i64(aa_decls$nmodules,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=aa_decls$nmodules;++i) {
L25 :;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"  ",NULL);
        msysc$m_print_i64(i,NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)":",NULL);
        msysc$m_print_str(mlib$padstr(aa_decls$moduletable[(i)-1].name,(i64)13,(byte*)" "),NULL);
        msysc$m_print_str(mlib$padstr(aa_decls$moduletable[(i)-1].filename,(i64)25,(byte*)" "),NULL);
        msysc$m_print_i64(strlen(aa_decls$moduletable[(i)-1].source),NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L26 :;
    }
L27 :;
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"DLL Libs:",NULL);
    msysc$m_print_i64(aa_decls$nsearchlibs,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=aa_decls$nsearchlibs;++i) {
L28 :;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"  ",NULL);
        msysc$m_print_i64(i,NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)":",NULL);
        msysc$m_print_str(aa_decls$searchlibs[(i)-1],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L29 :;
    }
L30 :;
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Import Libs:",NULL);
    msysc$m_print_i64(aa_decls$nimportlibs,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=aa_decls$nimportlibs;++i) {
L31 :;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"  ",NULL);
        msysc$m_print_i64(i,NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)":",NULL);
        msysc$m_print_str(aa_decls$importlibs[(i)-1],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L32 :;
    }
L33 :;
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

static struct aa_decls$strec *aacli$getemptyst(struct aa_decls$strec *d) {
        struct aa_decls$strec *  dnew;
    if (!!((i64)(*d).ksymbol)) {
        dnew = (struct aa_decls$strec *)mlib$pcm_allocz((i64)128);
        (*dnew).name = (*d).name;
        (*dnew).namelen = (i64)(*d).namelen;
        (*dnew).ksymbol = (i64)(*d).ksymbol;
        (*dnew).subcode = (i64)(*d).subcode;
        (*dnew).regsize = (i64)(*d).regsize;
        return (struct aa_decls$strec *)dnew;
    }
;
    return (struct aa_decls$strec *)0;
}

static struct aa_decls$strec *aacli$findduplname(struct aa_decls$strec *d) {
        struct aa_decls$strec *  e;
    if (!!((*d).basedef)) {
        return (struct aa_decls$strec *)(*d).basedef;
    }
;
    e = (struct aa_decls$strec *)aa_decls$dupltable[((i64)(*d).htfirstindex)];
    L34 :;
    while (!!(e)) {
        if ((((i64)(*d).namelen == (i64)(*e).namelen) && (memcmp((void *)(*d).name,(void *)(*e).name,(u64)(i64)(*d).namelen) == (i64)0))) {
            (*d).basedef = (struct aa_decls$strec *)e;
            return (struct aa_decls$strec *)e;
        }
;
        e = (struct aa_decls$strec *)(*e).nextdupl;
L35 :;
    }
L36 :;
    ;
    return (struct aa_decls$strec *)0;
}

static void aacli$adddupl(struct aa_decls$strec *d) {
    (*d).nextdupl = (struct aa_decls$strec *)aa_decls$dupltable[((i64)(*d).htfirstindex)];
    aa_decls$dupltable[((i64)(*d).htfirstindex)] = (struct aa_decls$strec *)d;
}

static void aacli$scanglobals(void) {
        struct aa_decls$strec *  d;
        struct aa_decls$strec *  e;
    d = (struct aa_decls$strec *)aa_decls$modulenamelist;
    L37 :;
    while (!!(d)) {
                {i64 $temp = (i64)(*d).symbol;
if (($temp==(i64)21)) {
            e = (struct aa_decls$strec *)aacli$findduplname((struct aa_decls$strec *)d);
            if (!!(e)) {
                                {i64 $temp = (i64)(*e).symbol;
if (($temp==(i64)21)) {
                }
                else if (($temp==(i64)22)) {
                    (*d).symbol = (i64)22;
                    (*d).reftype = ((*e).reftype = (i64)1);
                }
                };
            }
            else {
                aa_lib$addimport((struct aa_decls$strec *)d);
                aacli$adddupl((struct aa_decls$strec *)d);
            }
;
        }
        else if (($temp==(i64)22)) {
            e = (struct aa_decls$strec *)aacli$findduplname((struct aa_decls$strec *)d);
            if (!!(e)) {
                                {i64 $temp = (i64)(*e).symbol;
if (($temp==(i64)21)) {
                    (*e).symbol = (i64)22;
                    (*d).reftype = ((*e).reftype = (i64)1);
                }
                else if (($temp==(i64)22)) {
                    msysc$m_print_startcon();
                    msysc$m_print_str(aa_decls$moduletable[((i64)(*d).moduleno)-1].name,NULL);
                    msysc$m_print_str((*d).name,NULL);
                    msysc$m_print_i64((i64)(*d).htindex,NULL);
                    msysc$m_print_newline();
                    msysc$m_print_end();
                    ;
                    msysc$m_print_startcon();
                    msysc$m_print_str(aa_decls$moduletable[((i64)(*e).moduleno)-1].name,NULL);
                    msysc$m_print_str((*e).name,NULL);
                    msysc$m_print_i64((i64)(*e).htindex,NULL);
                    msysc$m_print_newline();
                    msysc$m_print_end();
                    ;
                    aa_lib$serror_s((byte*)"Multiply-defined global: %s",(*d).name);
                }
                };
            }
            else {
                e = d;
                aa_lib$addimport((struct aa_decls$strec *)d);
                aacli$adddupl((struct aa_decls$strec *)d);
            }
;
        }
        };
        d = (struct aa_decls$strec *)(*d).nextdef;
L38 :;
    }
L39 :;
    ;
}

static void aacli$resethashtable(void) {
        struct aa_decls$strec *  d;
    d = (struct aa_decls$strec *)aa_decls$modulenamelist;
    L40 :;
    while (!!(d)) {
        aa_decls$lexhashtable[((i64)(*d).htindex)] = (struct aa_decls$strec *)aacli$getemptyst((struct aa_decls$strec *)d);
        d = (struct aa_decls$strec *)(*d).nextdef;
L41 :;
    }
L42 :;
    ;
    aa_decls$modulenamelist = 0;
}

// START
void aacli$start(void) {
    aa_mcxdecls$start();
    aa_decls$start();
    aa_tables$start();
    aa_objdecls$start();
    aa_lex$start();
    aa_parse$start();
    aa_showss$start();
    aa_writeobj$start();
    aa_writeexe$start();
    aa_writess$start();
    aa_disasm$start();
    aa_genss$start();
    aa_lib$start();

}

// START
void aa_mcxdecls$start(void) {

}

// START
void aa_decls$start(void) {

}

// START
void aa_tables$start(void) {

}

// START
void aa_objdecls$start(void) {

}

void aa_lex$lex(void) {
        i64 i;
        i64 c;
        i64 hsum;
        i64 length;
        byte *  pstart;
    aa_lex$lxsubcode = (i64)0;
    L43 :;
    switch ((c = (i64)(*(aa_lex$lxsptr)++))) {
    case 97:;
    case 98:;
    case 99:;
    case 100:;
    case 101:;
    case 102:;
    case 103:;
    case 104:;
    case 105:;
    case 106:;
    case 107:;
    case 108:;
    case 109:;
    case 110:;
    case 111:;
    case 112:;
    case 113:;
    case 114:;
    case 115:;
    case 116:;
    case 117:;
    case 118:;
    case 119:;
    case 120:;
    case 121:;
    case 122:;
    case 36:;
    case 95:;
    case 46:;
        {
            pstart = (aa_lex$lxsptr - (i64)1);
            hsum = c;
            //doname:
L45 :;
;
            L46 :;
            switch ((c = (i64)(*(aa_lex$lxsptr)++))) {
            case 97:;
            case 98:;
            case 99:;
            case 100:;
            case 101:;
            case 102:;
            case 103:;
            case 104:;
            case 105:;
            case 106:;
            case 107:;
            case 108:;
            case 109:;
            case 110:;
            case 111:;
            case 112:;
            case 113:;
            case 114:;
            case 115:;
            case 116:;
            case 117:;
            case 118:;
            case 119:;
            case 120:;
            case 121:;
            case 122:;
            case 48:;
            case 49:;
            case 50:;
            case 51:;
            case 52:;
            case 53:;
            case 54:;
            case 55:;
            case 56:;
            case 57:;
            case 95:;
            case 36:;
            case 46:;
                {
                    hsum = (((hsum << (i64)4) - hsum) + c);
                }
                break;
            case 65:;
            case 66:;
            case 67:;
            case 68:;
            case 69:;
            case 70:;
            case 71:;
            case 72:;
            case 73:;
            case 74:;
            case 75:;
            case 76:;
            case 77:;
            case 78:;
            case 79:;
            case 80:;
            case 81:;
            case 82:;
            case 83:;
            case 84:;
            case 85:;
            case 86:;
            case 87:;
            case 88:;
            case 89:;
            case 90:;
                {
                    (*(aa_lex$lxsptr - (i64)1)) = (c + (i64)32);
                    hsum = ((((hsum << (i64)4) - hsum) + c) + (i64)32);
                }
                break;
            default: {
                --(aa_lex$lxsptr);
                goto L47 ;
            }
            } //SW
goto L46 ;
L47 :;
            ;
            aa_lex$lxlength = (aa_lex$lxsptr - pstart);
            aa_lex$lxhashvalue = ((hsum << (i64)5) - hsum);
            if (!!(aa_lex$lookuplex((u8 *)pstart,aa_lex$lxlength))) {
                if (!!((i64)(*aa_lex$lxsymptr).ksymbol)) {
                    aa_lex$lxsymbol = (i64)(*aa_lex$lxsymptr).ksymbol;
                    aa_lex$lxsubcode = (i64)(*aa_lex$lxsymptr).subcode;
                }
                else {
                    aa_lex$lxsymbol = (i64)(*aa_lex$lxsymptr).symbol;
                }
;
            }
            else {
                aa_lex$lxsymbol = (i64)17;
            }
;
            return;
        }
        break;
    case 65:;
    case 66:;
    case 67:;
    case 68:;
    case 69:;
    case 70:;
    case 71:;
    case 72:;
    case 73:;
    case 74:;
    case 75:;
    case 76:;
    case 77:;
    case 78:;
    case 79:;
    case 80:;
    case 81:;
    case 82:;
    case 83:;
    case 84:;
    case 85:;
    case 86:;
    case 87:;
    case 88:;
    case 89:;
    case 90:;
        {
            pstart = (aa_lex$lxsptr - (i64)1);
            hsum = ((*pstart) = (c + (i64)32));
            goto L45 ;
;
        }
        break;
    case 48:;
    case 49:;
    case 50:;
    case 51:;
    case 52:;
    case 53:;
    case 54:;
    case 55:;
    case 56:;
    case 57:;
        {
            aa_lex$readnumber(c);
            return;
        }
        break;
    case 96:;
        {
            pstart = aa_lex$lxsptr;
            hsum = (i64)0;
            L48 :;
            switch ((c = (i64)(*aa_lex$lxsptr))) {
            case 65:;
            case 66:;
            case 67:;
            case 68:;
            case 69:;
            case 70:;
            case 71:;
            case 72:;
            case 73:;
            case 74:;
            case 75:;
            case 76:;
            case 77:;
            case 78:;
            case 79:;
            case 80:;
            case 81:;
            case 82:;
            case 83:;
            case 84:;
            case 85:;
            case 86:;
            case 87:;
            case 88:;
            case 89:;
            case 90:;
            case 97:;
            case 98:;
            case 99:;
            case 100:;
            case 101:;
            case 102:;
            case 103:;
            case 104:;
            case 105:;
            case 106:;
            case 107:;
            case 108:;
            case 109:;
            case 110:;
            case 111:;
            case 112:;
            case 113:;
            case 114:;
            case 115:;
            case 116:;
            case 117:;
            case 118:;
            case 119:;
            case 120:;
            case 121:;
            case 122:;
            case 48:;
            case 49:;
            case 50:;
            case 51:;
            case 52:;
            case 53:;
            case 54:;
            case 55:;
            case 56:;
            case 57:;
            case 95:;
            case 36:;
            case 46:;
                {
                    ++(aa_lex$lxsptr);
                    hsum = (((hsum << (i64)4) - hsum) + c);
                }
                break;
            default: {
                goto L49 ;
            }
            } //SW
goto L48 ;
L49 :;
            ;
            aa_lex$lxsymbol = (i64)17;
            if ((pstart == aa_lex$lxsptr)) {
                aa_lex$lxerror((byte*)"NULL ` name");
            }
;
            aa_lex$lxlength = (aa_lex$lxsptr - pstart);
            aa_lex$lxhashvalue = ((hsum << (i64)5) - hsum);
            if (!!(aa_lex$lookuplex((u8 *)pstart,aa_lex$lxlength))) {
                aa_lex$lxsymbol = (i64)(*aa_lex$lxsymptr).symbol;
                if ((aa_lex$lxsymbol == (i64)0)) {
                    aa_lex$lxsymbol = ((*aa_lex$lxsymptr).symbol = (i64)17);
                }
;
            }
;
            return;
        }
        break;
    case 33:;
    case 59:;
    case 35:;
        {
            L50 :;
            while (!!((u64)aa_lex$commentmap[((i64)(*(aa_lex$lxsptr)++))])) {
L51 :;
            }
L52 :;
            ;
            if (((i64)(*(aa_lex$lxsptr - (i64)1)) == (i64)0)) {
                --(aa_lex$lxsptr);
            }
;
            ++(aa_decls$lxlineno);
            aa_lex$lxsymbol = (i64)11;
            return;
        }
        break;
    case 44:;
        {
            aa_lex$lxsymbol = (i64)2;
            return;
        }
        break;
    case 58:;
        {
            if (((i64)(*aa_lex$lxsptr) == (i64)58)) {
                aa_lex$lxsymbol = (i64)4;
                ++(aa_lex$lxsptr);
            }
            else {
                aa_lex$lxsymbol = (i64)3;
            }
;
            return;
        }
        break;
    case 91:;
        {
            aa_lex$lxsymbol = (i64)5;
            return;
        }
        break;
    case 93:;
        {
            aa_lex$lxsymbol = (i64)6;
            return;
        }
        break;
    case 43:;
        {
            aa_lex$lxsymbol = (i64)7;
            return;
        }
        break;
    case 45:;
        {
            aa_lex$lxsymbol = (i64)8;
            return;
        }
        break;
    case 42:;
        {
            aa_lex$lxsymbol = (i64)9;
            return;
        }
        break;
    case 61:;
        {
            aa_lex$lxsymbol = (i64)10;
            return;
        }
        break;
    case 39:;
        {
            pstart = aa_lex$lxsptr;
            L53 :;
            while (1) {
                switch ((i64)(*(aa_lex$lxsptr)++)) {
                case 39:;
                    {
                        goto L54 ;
                    }
                    break;
                case 13:;
                case 10:;
                    {
                        aa_lex$lxerror((byte*)"String not terminated");
                    }
                    break;
                } //SW
;
            }
L54 :;
            ;
            length = ((aa_lex$lxsptr - pstart) - (i64)1);
            aa_lex$lxvalue = (i64)0;
            for (i=length;i>=(i64)1;--i) {
L55 :;
                aa_lex$lxvalue = ((aa_lex$lxvalue << (i64)8) + (i64)(*((pstart + i) - (i64)1)));
L56 :;
            }
L57 :;
            ;
            aa_lex$lxsymbol = (i64)14;
            return;
        }
        break;
    case 34:;
        {
            pstart = aa_lex$lxsptr;
            L58 :;
            while (1) {
                switch ((i64)(*(aa_lex$lxsptr)++)) {
                case 34:;
                    {
                        aa_lex$lxsvalue = (u8 *)pstart;
                        aa_lex$lxlength = ((aa_lex$lxsptr - pstart) - (i64)1);
                        (*(aa_lex$lxsvalue + aa_lex$lxlength)) = (u64)0u;
                        aa_lex$lxsymbol = (i64)16;
                        return;
                    }
                    break;
                case 13:;
                case 10:;
                case 26:;
                case 0:;
                    {
                        aa_lex$lxerror((byte*)"String not terminated");
                    }
                    break;
                } //SW
;
            }
L59 :;
            ;
        }
        break;
    case 32:;
    case 9:;
        {
        }
        break;
    case 13:;
        {
        }
        break;
    case 10:;
        {
            ++(aa_decls$lxlineno);
            aa_lex$lxsymbol = (i64)11;
            return;
        }
        break;
    case 0:;
    case 26:;
        {
            aa_lex$lxsymbol = (i64)12;
            --(aa_lex$lxsptr);
            return;
        }
        break;
    default: {
        aa_lex$lxsymbol = (i64)1;
        aa_lex$lxvalue = c;
        return;
    }
    } //SW
goto L43 ;
L44 :;
    ;
}

void aa_lex$initlex(void) {
        i64 i;
    aa_lex$lxsubcode = (i64)0;
    aa_lex$lxsymbol = (i64)1;
    aa_decls$lxlineno = (i64)0;
    for (i=(i64)0;i<=(i64)255;++i) {
L60 :;
        switch (i) {
        case 65:;
        case 66:;
        case 67:;
        case 68:;
        case 69:;
        case 70:;
        case 71:;
        case 72:;
        case 73:;
        case 74:;
        case 75:;
        case 76:;
        case 77:;
        case 78:;
        case 79:;
        case 80:;
        case 81:;
        case 82:;
        case 83:;
        case 84:;
        case 85:;
        case 86:;
        case 87:;
        case 88:;
        case 89:;
        case 90:;
        case 97:;
        case 98:;
        case 99:;
        case 100:;
        case 101:;
        case 102:;
        case 103:;
        case 104:;
        case 105:;
        case 106:;
        case 107:;
        case 108:;
        case 109:;
        case 110:;
        case 111:;
        case 112:;
        case 113:;
        case 114:;
        case 115:;
        case 116:;
        case 117:;
        case 118:;
        case 119:;
        case 120:;
        case 121:;
        case 122:;
        case 36:;
        case 95:;
        case 48:;
        case 49:;
        case 50:;
        case 51:;
        case 52:;
        case 53:;
        case 54:;
        case 55:;
        case 56:;
        case 57:;
            {
                aa_lex$alphamap[(i)] = (u64)1u;
            }
            break;
        } //SW
;
        switch (i) {
        case 48:;
        case 49:;
        case 50:;
        case 51:;
        case 52:;
        case 53:;
        case 54:;
        case 55:;
        case 56:;
        case 57:;
            {
                aa_lex$digitmap[(i)] = (u64)1u;
            }
            break;
        } //SW
;
        aa_lex$commentmap[(i)] = (u64)1u;
L61 :;
    }
L62 :;
    ;
    aa_lex$commentmap[((i64)0)] = (u64)0u;
    aa_lex$commentmap[((i64)10)] = (u64)0u;
    aa_lex$inithashtable();
}

static void aa_lex$readreal(u8 (*s)[],i64 slen,i64 intlen,i64 exponseen) {
        i64 i;
        i64 fractlen;
        i64 expon;
        i64 exponsign;
        i64 c;
        i64 digs;
        i64 $av_1;
        i64 $av_2;
    if (((intlen == (i64)0) || (intlen == slen))) {
        fractlen = (i64)0;
    }
    else {
        fractlen = (slen - intlen);
    }
;
    expon = (i64)0;
    exponsign = (i64)0;
    if (!!(exponseen)) {
                {i64 $temp = (c = (i64)(*(aa_lex$lxsptr)++));
if (($temp==(i64)43)) {
        }
        else if (($temp==(i64)45)) {
            exponsign = (i64)1;
        }
        else {
            --(aa_lex$lxsptr);
        }
        };
        digs = (i64)0;
        L63 :;
        switch ((c = (i64)(*(aa_lex$lxsptr)++))) {
        case 48:;
        case 49:;
        case 50:;
        case 51:;
        case 52:;
        case 53:;
        case 54:;
        case 55:;
        case 56:;
        case 57:;
            {
                expon = (((expon * (i64)10) + c) - (i64)48);
                ++(digs);
            }
            break;
        default: {
            --(aa_lex$lxsptr);
            goto L64 ;
        }
        } //SW
goto L63 ;
L64 :;
        ;
        if ((digs == (i64)0)) {
            aa_lex$lxerror((byte*)"Exponent error");
        }
;
        if (!!(exponsign)) {
            expon = -(expon);
        }
;
    }
;
    expon = (expon - fractlen);
    aa_lex$lxxvalue = (double)0.;
    for (i=(i64)1;i<=slen;++i) {
L65 :;
        c = (i64)(u64)(*s)[(i)-1];
        aa_lex$lxxvalue = ((aa_lex$lxxvalue * (double)10.) + (r64)(c - (i64)48));
L66 :;
    }
L67 :;
    ;
    if ((expon > (i64)0)) {
        $av_1 = expon;
        while ($av_1-- > 0) {
L68 :;
            aa_lex$lxxvalue = (aa_lex$lxxvalue * (double)10.);
L69 :;
        }
L70 :;
        ;
    }
    else if ((expon < (i64)0)) {
        $av_2 = -(expon);
        while ($av_2-- > 0) {
L71 :;
            aa_lex$lxxvalue = (aa_lex$lxxvalue / (double)10.);
L72 :;
        }
L73 :;
        ;
    }
;
    aa_lex$lxsymbol = (i64)15;
}

static void aa_lex$readnumber(i64 c) {
        u8 str[256];
        i64 i;
        i64 d;
        i64 intlen;
        i64 slen;
    d = (i64)(*aa_lex$lxsptr);
    if ((d==(i64)120) || (d==(i64)88)) {
        if ((c==(i64)48)) {
            ++(aa_lex$lxsptr);
            aa_lex$readhex();
            return;
        }
        else if ((c==(i64)50)) {
            ++(aa_lex$lxsptr);
            aa_lex$readbinary();
            return;
        }
        else {
            msysc$m_print_startcon();
            msysc$m_print_i64(c,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            aa_lex$lxerror((byte*)"Base not supported");
        }
;
    }
;
    str[((i64)1)-1] = (u64)c;
    slen = (i64)1;
    intlen = (i64)0;
    L74 :;
    switch ((c = (i64)(*(aa_lex$lxsptr)++))) {
    case 48:;
    case 49:;
    case 50:;
    case 51:;
    case 52:;
    case 53:;
    case 54:;
    case 55:;
    case 56:;
    case 57:;
        {
            str[(++(slen))-1] = (u64)c;
        }
        break;
    case 95:;
    case 39:;
    case 96:;
        {
        }
        break;
    case 46:;
        {
            intlen = slen;
        }
        break;
    case 101:;
    case 69:;
        {
            aa_lex$readreal((u8 (*)[])&str,slen,intlen,(i64)1);
            return;
        }
        break;
    default: {
        --(aa_lex$lxsptr);
        goto L75 ;
    }
    } //SW
goto L74 ;
L75 :;
    ;
    if (!!(intlen)) {
        aa_lex$readreal((u8 (*)[])&str,slen,intlen,(i64)0);
        return;
    }
;
    if (((slen > (i64)20) || ((slen == (i64)20) && (mlib$cmpstring((u8 *)str,(byte*)"18446744073709551615") > (i64)0)))) {
        aa_lex$lxerror((byte*)"Overflow in 64-bit value");
    }
;
    aa_lex$lxsymbol = (i64)14;
    aa_lex$lxvalue = (i64)0;
    for (i=(i64)1;i<=slen;++i) {
L76 :;
        aa_lex$lxvalue = (((aa_lex$lxvalue * (i64)10) + (i64)(u64)str[(i)-1]) - (i64)48);
L77 :;
    }
L78 :;
    ;
}

static void aa_lex$readbinary(void) {
        i64 ndigs;
    ndigs = (i64)0;
    aa_lex$lxvalue = (i64)0;
    L79 :;
    switch ((i64)(*(aa_lex$lxsptr)++)) {
    case 48:;
        {
            aa_lex$lxvalue = (aa_lex$lxvalue * (i64)2);
            ++(ndigs);
        }
        break;
    case 49:;
        {
            aa_lex$lxvalue = ((aa_lex$lxvalue * (i64)2) + (i64)1);
            ++(ndigs);
        }
        break;
    case 50:;
    case 51:;
    case 52:;
    case 53:;
    case 54:;
    case 55:;
    case 56:;
    case 57:;
        {
            aa_lex$lxerror((byte*)"Bad binary digit");
        }
        break;
    case 95:;
    case 39:;
    case 96:;
        {
        }
        break;
    default: {
        --(aa_lex$lxsptr);
        goto L80 ;
    }
    } //SW
goto L79 ;
L80 :;
    ;
    if ((ndigs == (i64)0)) {
        aa_lex$lxerror((byte*)"No bin digits");
    }
    else if ((ndigs > (i64)64)) {
        aa_lex$lxerror((byte*)"Overflow in binary number");
    }
;
    aa_lex$lxsymbol = (i64)14;
}

static void aa_lex$readhex(void) {
        i64 ndigs;
        i64 c;
    ndigs = (i64)0;
    aa_lex$lxvalue = (i64)0;
    L81 :;
    switch ((c = (i64)(*(aa_lex$lxsptr)++))) {
    case 48:;
    case 49:;
    case 50:;
    case 51:;
    case 52:;
    case 53:;
    case 54:;
    case 55:;
    case 56:;
    case 57:;
        {
            aa_lex$lxvalue = (((aa_lex$lxvalue * (i64)16) + c) - (i64)48);
            ++(ndigs);
        }
        break;
    case 65:;
    case 66:;
    case 67:;
    case 68:;
    case 69:;
    case 70:;
        {
            aa_lex$lxvalue = ((aa_lex$lxvalue * (i64)16) + ((c - (i64)65) + (i64)10));
            ++(ndigs);
        }
        break;
    case 97:;
    case 98:;
    case 99:;
    case 100:;
    case 101:;
    case 102:;
        {
            aa_lex$lxvalue = ((aa_lex$lxvalue * (i64)16) + ((c - (i64)97) + (i64)10));
            ++(ndigs);
        }
        break;
    case 95:;
    case 39:;
    case 96:;
        {
        }
        break;
    default: {
        --(aa_lex$lxsptr);
        goto L82 ;
    }
    } //SW
goto L81 ;
L82 :;
    ;
    if ((ndigs == (i64)0)) {
        aa_lex$lxerror((byte*)"No hex digits");
    }
    else if ((ndigs > (i64)16)) {
        aa_lex$lxerror((byte*)"Overflow in hex number");
    }
;
    aa_lex$lxsymbol = (i64)14;
}

void aa_lex$ps(u8 *caption) {
    msysc$m_print_startcon();
    msysc$m_print_str(caption,NULL);
    msysc$m_print_str((byte*)":",NULL);
    msysc$m_print_end();
    ;
    aa_lex$printsymbol(0);
}

void aa_lex$printsymbol(void *dev) {
        u8 str[256];
        i64 $av_1;
    strcpy((u8 *)str,aa_tables$symbolnames[(aa_lex$lxsymbol)-1]);
    str[((strlen((u8 *)str) - (i64)2))-1] = (u64)0u;
    msysc$m_print_startfile(dev);
    msysc$m_print_str(str,NULL);
    msysc$m_print_end();
    ;
    $av_1 = ((i64)14 - strlen((u8 *)str));
    while ($av_1-- > 0) {
L83 :;
        msysc$m_print_startfile(dev);
        msysc$m_print_str((byte*)" ",NULL);
        msysc$m_print_end();
        ;
L84 :;
    }
L85 :;
    ;
    if ((aa_lex$lxsymbol==(i64)17)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_str((*aa_lex$lxsymptr).name,NULL);
        msysc$m_print_end();
        ;
    }
    else if ((aa_lex$lxsymbol==(i64)14)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_i64(aa_lex$lxvalue,NULL);
        msysc$m_print_end();
        ;
    }
    else if ((aa_lex$lxsymbol==(i64)15)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_r64(aa_lex$lxxvalue,NULL);
        msysc$m_print_end();
        ;
    }
    else if ((aa_lex$lxsymbol==(i64)16)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_str((byte*)"\"",NULL);
        msysc$m_print_nogap();
        msysc$m_print_str(aa_lex$lxsvalue,NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)"\"",NULL);
        msysc$m_print_end();
        ;
    }
    else if ((aa_lex$lxsymbol==(i64)1)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_i64(aa_lex$lxvalue,NULL);
        msysc$m_print_end();
        ;
    }
    else {
        msysc$m_print_startfile(dev);
        msysc$m_print_str(aa_tables$symbolnames[(aa_lex$lxsymbol)-1],NULL);
        msysc$m_print_end();
        ;
        if (!!(aa_lex$lxsubcode)) {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)" ",NULL);
            msysc$m_print_nogap();
            msysc$m_print_i64(aa_lex$lxsubcode,NULL);
            msysc$m_print_end();
            ;
        }
;
    }
;
    msysc$m_print_startfile(dev);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

static void aa_lex$clearhashtable(void) {
}

static void aa_lex$inithashtable(void) {
        i64 i;
    if (((i64)65536 > (i64)65536)) {
    }
;
    aa_lex$clearhashtable();
    for (i=(i64)1;i<=(i64)147;++i) {
L86 :;
        aa_lex$addreservedword((aa_tables$mclnames[(i)-1] + (i64)2),(i64)23,i);
L87 :;
    }
L88 :;
    ;
    for (i=(i64)1;i<=(i64)136;++i) {
L89 :;
        aa_lex$addreservedword(aa_tables$dregnames[(i)-1],(i64)24,(i64)aa_tables$regindices[(i)-1]);
        (*aa_lex$lxsymptr).regsize = (i64)aa_tables$regsizes[(i)-1];
L90 :;
    }
L91 :;
    ;
    for (i=(i64)1;i<=(i64)16;++i) {
L92 :;
        aa_lex$addreservedword(aa_tables$xregnames[(i)-1],(i64)25,i);
L93 :;
    }
L94 :;
    ;
    for (i=(i64)1;i<=(i64)8;++i) {
L95 :;
        aa_lex$addreservedword(aa_tables$fregnames[(i)-1],(i64)26,i);
L96 :;
    }
L97 :;
    ;
    for (i=(i64)1;i<=(i64)8;++i) {
L98 :;
        aa_lex$addreservedword(aa_tables$mregnames[(i)-1],(i64)27,i);
L99 :;
    }
L100 :;
    ;
    for (i=(i64)1;i<=(i64)18;++i) {
L101 :;
        aa_lex$addreservedword(aa_tables$jmpccnames[(i)-1],(i64)28,(i64)aa_tables$jmpcccodes[(i)-1]);
L102 :;
    }
L103 :;
    ;
    for (i=(i64)1;i<=(i64)18;++i) {
L104 :;
        aa_lex$addreservedword(aa_tables$setccnames[(i)-1],(i64)29,(i64)aa_tables$setcccodes[(i)-1]);
L105 :;
    }
L106 :;
    ;
    for (i=(i64)1;i<=(i64)18;++i) {
L107 :;
        aa_lex$addreservedword(aa_tables$cmovccnames[(i)-1],(i64)30,(i64)aa_tables$cmovcccodes[(i)-1]);
L108 :;
    }
L109 :;
    ;
    for (i=(i64)1;i<=(i64)16;++i) {
L110 :;
        aa_lex$addreservedword(aa_tables$prefixnames[(i)-1],(i64)31,(i64)aa_tables$prefixsizes[(i)-1]);
L111 :;
    }
L112 :;
    ;
    for (i=(i64)1;i<=(i64)5;++i) {
L113 :;
        aa_lex$addreservedword(aa_mcxdecls$segmentnames[(i)-1],(i64)32,i);
L114 :;
    }
L115 :;
    ;
    aa_lex$addreservedword((byte*)"aframe",(i64)24,(i64)15);
    (*aa_lex$lxsymptr).regsize = (i64)4;
    aa_lex$addreservedword((byte*)"dframe",(i64)24,(i64)15);
    (*aa_lex$lxsymptr).regsize = (i64)8;
    aa_lex$addreservedword((byte*)"astack",(i64)24,(i64)16);
    (*aa_lex$lxsymptr).regsize = (i64)4;
    aa_lex$addreservedword((byte*)"dstack",(i64)24,(i64)16);
    (*aa_lex$lxsymptr).regsize = (i64)8;
    aa_lex$addreservedword((byte*)"dprog",(i64)24,(i64)9);
    (*aa_lex$lxsymptr).regsize = (i64)8;
    aa_lex$addreservedword((byte*)"dsptr",(i64)24,(i64)10);
    (*aa_lex$lxsymptr).regsize = (i64)8;
    aa_lex$addreservedword((byte*)"importlib",(i64)33,(i64)0);
    aa_lex$addreservedword((byte*)"importdll",(i64)34,(i64)0);
}

static void aa_lex$addreservedword(u8 *name,i64 symbol,i64 subcode) {
    aa_lex$lxhashvalue = aa_lex$gethashvalue(name);
    if (!!(aa_lex$lookuplex(name,(i64)0))) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"NAME=",NULL);
        msysc$m_print_str(name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        aa_lex$lxerror((byte*)"DUPL NAME");
    }
;
    (*aa_lex$lxsymptr).symbol = (i64)0;
    (*aa_lex$lxsymptr).ksymbol = symbol;
    (*aa_lex$lxsymptr).subcode = subcode;
}

void aa_lex$printhashtable(void *devx,u8 *caption) {
        struct aa_decls$strec *  r;
        i64 count;
        i64 i;
    msysc$m_print_startfile(devx);
    msysc$m_print_str(caption,NULL);
    msysc$m_print_str((byte*)":",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    count = (i64)0;
    for (i=(i64)0;i<=(i64)65535;++i) {
L116 :;
        r = (struct aa_decls$strec *)aa_decls$lexhashtable[(i)];
        if ((!!(r) && !!((*r).name))) {
            count += (i64)1;
        }
;
L117 :;
    }
L118 :;
    ;
    msysc$m_print_startfile(devx);
    msysc$m_print_i64(count,NULL);
    msysc$m_print_str((byte*)" items in table",NULL);
    msysc$m_print_i64((i64)65536,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

static i64 aa_lex$lookuplex(u8 *name,i64 length) {
        i64 j;
        i64 wrapped;
        i64 insource;
        i64 firstj;
    insource = length;
    if ((length == (i64)0)) {
        length = strlen(name);
    }
;
    firstj = (j = (aa_lex$lxhashvalue & (i64)65535));
    wrapped = (i64)0;
    L119 :;
    while (1) {
        aa_lex$lxsymptr = (struct aa_decls$strec *)aa_decls$lexhashtable[(j)];
        if ((aa_lex$lxsymptr == 0)) {
            goto L120 ;
        }
;
        if ((((i64)(*aa_lex$lxsymptr).namelen == length) && (memcmp((void *)(*aa_lex$lxsymptr).name,(void *)name,(u64)length) == (i64)0))) {
            return (i64)1;
        }
;
        if ((++(j) > (i64)65536)) {
            if (!!(wrapped)) {
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"???????HASHTABLE FULL",NULL);
                msysc$m_print_i64((i64)65536,NULL);
                msysc$m_print_i64(aa_decls$lxlineno,NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                exit((i64)1);
            }
;
            wrapped = (i64)1;
            j = (i64)1;
        }
;
    }
L120 :;
    ;
    if (!!(insource)) {
        name = aa_lex$makestring(name,length);
    }
;
    if ((aa_lex$lxsymptr == 0)) {
        aa_lex$lxsymptr = (struct aa_decls$strec *)mlib$pcm_allocz((i64)128);
        aa_decls$lexhashtable[(j)] = (struct aa_decls$strec *)aa_lex$lxsymptr;
    }
;
    (*aa_lex$lxsymptr).name = name;
    (*aa_lex$lxsymptr).namelen = length;
    (*aa_lex$lxsymptr).symbol = (i64)17;
    (*aa_lex$lxsymptr).ksymbol = (i64)0;
    (*aa_lex$lxsymptr).htindex = j;
    (*aa_lex$lxsymptr).htfirstindex = firstj;
    (*aa_lex$lxsymptr).moduleno = aa_decls$currmoduleno;
    return (i64)0;
}

void aa_lex$initsourcefile(u8 *source) {
    aa_lex$lxstart = (aa_lex$lxsptr = (byte *)source);
    aa_decls$lxlineno = (i64)1;
}

struct aa_decls$strec *aa_lex$addnamestr(u8 *name) {
    aa_lex$lxhashvalue = aa_lex$gethashvalue(name);
    aa_lex$lookuplex(mlib$pcm_copyheapstring(name),(i64)0);
    return (struct aa_decls$strec *)aa_lex$lxsymptr;
}

void aa_lex$lxerror(u8 *m) {
    msysc$m_print_startcon();
    msysc$m_print_setfmt((byte*)"\r\n\r\n Lexical Error\n*** # *** on line #");
    msysc$m_print_str(m,NULL);
    msysc$m_print_i64(aa_decls$lxlineno,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    exit((i64)1);
}

i64 aa_lex$gethashvalue(u8 *s) {
        i64 c;
        i64 hsum;
    if (((i64)(u64)(*s) == (i64)0)) {
        return (i64)0;
    }
;
    hsum = (i64)(u64)(*(s)++);
    L121 :;
    while (1) {
        c = (i64)(u64)(*(s)++);
        if ((c == (i64)0)) {
            goto L122 ;
        }
;
        hsum = (((hsum << (i64)4) - hsum) + c);
    }
L122 :;
    ;
    return ((hsum << (i64)5) - hsum);
}

void aa_lex$skiptoeol(void) {
    L123 :;
    do {
        aa_lex$lex();
L124 :;
    }
    while (!((aa_lex$lxsymbol == (i64)11) || (aa_lex$lxsymbol == (i64)12)));
L125 :;
    ;
}

static u8 *aa_lex$makestring(u8 *p,i64 length) {
        u8 *  s;
    s = (u8 *)mlib$pcm_alloc((length + (i64)1));
    memcpy((void *)s,(void *)p,(u64)length);
    (*(s + length)) = (u64)0u;
    return s;
}

// START
void aa_lex$start(void) {

}

void aa_parse$readmodule(i64 moduleno) {
        struct aa_decls$strec *  symptr;
        i64 sym;
        i64 i;
    aa_lex$initsourcefile(aa_decls$moduletable[(moduleno)-1].source);
    aa_lex$lxsymbol = (i64)11;
    aa_lib$genmc((i64)118,(struct aa_decls$opndrec *)aa_lib$genint((i64)1,(i64)4),0);
    L126 :;
    while ((aa_lex$lxsymbol == (i64)11)) {
        aa_lex$lex();
        switch (aa_lex$lxsymbol) {
        case 23:;
            {
                aa_parse$readinstr();
            }
            break;
        case 17:;
            {
                symptr = (struct aa_decls$strec *)aa_lex$lxsymptr;
                aa_lex$lex();
                sym = aa_lex$lxsymbol;
                if ((sym==(i64)10)) {
                    aa_lex$lex();
                    if ((aa_lex$lxsymbol==(i64)24)) {
                        aa_lib$createregalias((struct aa_decls$strec *)symptr,(i64)(*aa_lex$lxsymptr).subcode,(i64)(*aa_lex$lxsymptr).regsize);
                        aa_lex$lex();
                    }
                    else if ((aa_lex$lxsymbol==(i64)25)) {
                        aa_lib$createxregalias((struct aa_decls$strec *)symptr,(i64)(*aa_lex$lxsymptr).subcode);
                        aa_lex$lex();
                    }
                    else {
                        aa_lib$createnamedconst((struct aa_decls$strec *)symptr,(struct aa_decls$opndrec *)aa_parse$readexpression());
                    }
;
                }
                else if ((sym==(i64)3) || (sym==(i64)4)) {
                    aa_lib$createlabel((struct aa_decls$strec *)symptr,((sym == (i64)3) ? (i64)20 : (i64)22));
                    aa_lib$genmc((i64)4,(struct aa_decls$opndrec *)aa_lib$genlab((struct aa_decls$strec *)symptr,(i64)4),0);
                    (*symptr).reftype = (i64)1;
                    aa_lex$lxsymbol = (i64)11;
                    goto L126 ;
                }
                else {
                    msysc$m_print_startcon();
                    msysc$m_print_str((*symptr).name,NULL);
                    msysc$m_print_newline();
                    msysc$m_print_end();
                    ;
                    aa_lib$serror((byte*)"colon expected after label");
                }
;
            }
            break;
        case 19:;
            {
                symptr = (struct aa_decls$strec *)aa_lex$lxsymptr;
                aa_lex$lex();
                if ((aa_lex$lxsymbol==(i64)10)) {
                    aa_lib$serror_s((byte*)"Redefining label as const: %s",(*symptr).name);
                }
                else if ((aa_lex$lxsymbol==(i64)3) || (aa_lex$lxsymbol==(i64)4)) {
                    (*symptr).fwdrefs = 0;
                    aa_lib$genmc((i64)4,(struct aa_decls$opndrec *)aa_lib$genlab((struct aa_decls$strec *)symptr,(i64)4),0);
                    (*symptr).symbol = ((aa_lex$lxsymbol == (i64)3) ? (i64)20 : (i64)22);
                    (*symptr).reftype = (i64)1;
                    aa_lex$lxsymbol = (i64)11;
                    goto L126 ;
                }
                else {
                    aa_lib$serror((byte*)"Instruction expected");
                }
;
            }
            break;
        case 21:;
            {
                aa_lib$serror_s((byte*)"Defining imported name: %s",(*symptr).name);
            }
            break;
        case 20:;
        case 22:;
            {
                aa_lib$serror((byte*)"Redefining symbol");
            }
            break;
        case 18:;
            {
                aa_lib$serror_s((byte*)"2:Const redefined: %s",(*symptr).name);
            }
            break;
        case 28:;
            {
                aa_parse$readcondinstr((i64)25);
            }
            break;
        case 29:;
            {
                aa_parse$readcondinstr((i64)59);
            }
            break;
        case 30:;
            {
                aa_parse$readcondinstr((i64)14);
            }
            break;
        case 11:;
            {
            }
            break;
        case 12:;
            {
                return;
            }
            break;
        case 33:;
            {
                aa_lex$lex();
                aa_parse$checksymbol((i64)17);
                if ((aa_decls$nimportlibs >= (i64)30)) {
                    aa_lib$serror((byte*)"Too many import libs");
                }
;
                for (i=(i64)1;i<=aa_decls$nimportlibs;++i) {
L129 :;
                    if (!!(mlib$eqstring(aa_decls$importlibs[(i)-1],(*aa_lex$lxsymptr).name))) {
                        goto L131 ;
                    }
;
L130 :;
                }
                {
                    aa_decls$importlibs[(++(aa_decls$nimportlibs))-1] = (*aa_lex$lxsymptr).name;
                }
L131 :;
                ;
                aa_lex$lex();
            }
            break;
        case 34:;
            {
                aa_lex$lex();
                aa_parse$checksymbol((i64)17);
                if ((aa_decls$nsearchlibs >= (i64)30)) {
                    aa_lib$serror((byte*)"Too many DLLs");
                }
;
                for (i=(i64)1;i<=aa_decls$nsearchlibs;++i) {
L132 :;
                    if (!!(mlib$eqstring(aa_decls$searchlibs[(i)-1],(*aa_lex$lxsymptr).name))) {
                        goto L134 ;
                    }
;
L133 :;
                }
                {
                    aa_decls$searchlibs[(++(aa_decls$nsearchlibs))-1] = (*aa_lex$lxsymptr).name;
                }
L134 :;
                ;
                aa_lex$lex();
            }
            break;
        default: {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"Unknown symbol (possibly redefining regvar):",NULL);
            msysc$m_print_str(aa_tables$symbolnames[(aa_lex$lxsymbol)-1],NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
        } //SW
;
L127 :;
    }
L128 :;
    ;
    aa_lib$serror((byte*)"EOL expected");
}

void aa_parse$checkundefined(void) {
        struct aa_decls$strec *  d;
    d = (struct aa_decls$strec *)aa_decls$modulenamelist;
    L135 :;
    while (!!(d)) {
        if (((i64)(*d).symbol == (i64)19)) {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"Undefined:",NULL);
            msysc$m_print_str(mlib$padstr((*d).name,(i64)20,(byte*)" "),NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            ++(aa_decls$nundefined);
        }
;
        d = (struct aa_decls$strec *)(*d).nextdef;
L136 :;
    }
L137 :;
    ;
}

static void aa_parse$checksymbol(i64 symbol) {
        u8 str[265];
    if ((aa_lex$lxsymbol != symbol)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"# expected not #");
        msysc$m_print_str(aa_tables$symbolnames[(symbol)-1],NULL);
        msysc$m_print_str(aa_tables$symbolnames[(aa_lex$lxsymbol)-1],NULL);
        msysc$m_print_end();
        ;
        aa_lib$serror((u8 *)str);
    }
;
}

static void aa_parse$readinstr(void) {
        i64 opcode;
        struct aa_decls$opndrec *  a;
        struct aa_decls$opndrec *  b;
        struct aa_decls$opndrec *  c;
    opcode = aa_lex$lxsubcode;
    aa_lex$lex();
    switch (opcode) {
    case 114:;
    case 115:;
    case 116:;
    case 117:;
        {
            L138 :;
            while (1) {
                if ((aa_lex$lxsymbol == (i64)16)) {
                    a = (struct aa_decls$opndrec *)aa_lib$genstrimm(aa_lex$lxsvalue);
                    aa_lex$lex();
                    aa_lib$genmc(opcode,(struct aa_decls$opndrec *)a,0);
                }
                else {
                    a = (struct aa_decls$opndrec *)aa_parse$readoperand();
                    aa_lib$genmc(opcode,(struct aa_decls$opndrec *)a,0);
                }
;
                if ((aa_lex$lxsymbol == (i64)2)) {
                    aa_lex$lex();
                }
                else {
                    goto L139 ;
                }
;
            }
L139 :;
            ;
        }
        break;
    case 118:;
        {
            aa_parse$checksymbol((i64)32);
            aa_lib$genmc((i64)118,(struct aa_decls$opndrec *)aa_lib$genint(aa_lex$lxsubcode,(i64)4),0);
            aa_lex$lex();
        }
        break;
    case 119:;
        {
            aa_lib$genmc((i64)118,(struct aa_decls$opndrec *)aa_lib$genint((i64)2,(i64)4),0);
        }
        break;
    case 120:;
        {
            aa_lib$genmc((i64)118,(struct aa_decls$opndrec *)aa_lib$genint((i64)3,(i64)4),0);
        }
        break;
    case 121:;
        {
            aa_lib$genmc((i64)118,(struct aa_decls$opndrec *)aa_lib$genint((i64)1,(i64)4),0);
        }
        break;
    case 40:;
        {
            a = (struct aa_decls$opndrec *)aa_parse$readoperand();
            aa_parse$checksymbol((i64)2);
            aa_lex$lex();
            b = (struct aa_decls$opndrec *)aa_parse$readoperand();
            aa_parse$checksymbol((i64)2);
            aa_lex$lex();
            c = (struct aa_decls$opndrec *)aa_parse$readoperand();
            aa_lib$serror((byte*)"IMUL3 CAN'T DO 3 OPNDS");
        }
        break;
    case 90:;
    case 91:;
        {
            a = (struct aa_decls$opndrec *)aa_parse$readoperand();
            aa_parse$checksymbol((i64)2);
            aa_lex$lex();
            b = (struct aa_decls$opndrec *)aa_parse$readoperand();
            aa_parse$checksymbol((i64)2);
            aa_lex$lex();
            c = (struct aa_decls$opndrec *)aa_parse$readoperand();
            if (((i64)(*c).mode != (i64)2)) {
                aa_lib$serror((byte*)"pcmpistr/not int");
            }
;
            aa_lib$genmc(opcode,(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
            (*aa_lib$mccodex).c = (*c).value;
        }
        break;
    case 8:;
        {
            L140 :;
            while ((aa_lex$lxsymbol != (i64)11)) {
                aa_lex$lex();
L141 :;
            }
L142 :;
            ;
        }
        break;
    default: {
        a = (b = 0);
        if ((aa_lex$lxsymbol != (i64)11)) {
            a = (struct aa_decls$opndrec *)aa_parse$readoperand();
            if ((aa_lex$lxsymbol == (i64)2)) {
                aa_lex$lex();
                b = (struct aa_decls$opndrec *)aa_parse$readoperand();
            }
;
        }
;
        aa_lib$genmc(opcode,(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
    }
    } //SW
;
}

static void aa_parse$readcondinstr(i64 opc) {
        struct aa_decls$opndrec *  a;
        struct aa_decls$opndrec *  b;
    a = (struct aa_decls$opndrec *)aa_lib$genint(aa_lex$lxsubcode,(i64)4);
    aa_lex$lex();
    b = (struct aa_decls$opndrec *)aa_parse$readoperand();
    if (((aa_lex$lxsymbol == (i64)2) && (opc == (i64)14))) {
        aa_lib$genmc((i64)6,(struct aa_decls$opndrec *)b,0);
        aa_lex$lex();
        b = (struct aa_decls$opndrec *)aa_parse$readoperand();
    }
;
    aa_lib$genmc(opc,(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
}

static struct aa_decls$opndrec *aa_parse$readoperand(void) {
        struct aa_decls$opndrec *  p;
        i64 size;
    switch (aa_lex$lxsymbol) {
    case 24:;
        {
            p = (struct aa_decls$opndrec *)aa_lib$regtable[(aa_lex$lxsubcode)-1][((i64)(*aa_lex$lxsymptr).regsize)-1];
            aa_lex$lex();
            return (struct aa_decls$opndrec *)p;
        }
        break;
    case 5:;
        {
            aa_lex$lex();
            return (struct aa_decls$opndrec *)aa_parse$readaddrmode((i64)0);
        }
        break;
    case 25:;
        {
            p = (struct aa_decls$opndrec *)aa_lib$genxreg(aa_lex$lxsubcode);
            aa_lex$lex();
            return (struct aa_decls$opndrec *)p;
        }
        break;
    case 31:;
        {
            size = aa_lex$lxsubcode;
            aa_lex$lex();
            aa_parse$checksymbol((i64)5);
            aa_lex$lex();
            return (struct aa_decls$opndrec *)aa_parse$readaddrmode(size);
        }
        break;
    default: {
        return (struct aa_decls$opndrec *)aa_parse$readexpression();
    }
    } //SW
;
    return (struct aa_decls$opndrec *)0;
}

static struct aa_decls$opndrec *aa_parse$readexpression(void) {
        struct aa_decls$strec *  labelx;
        i64 valuex;
        i64 typex;
    aa_parse$readterm();
    L143 :;
    if ((aa_lex$lxsymbol==(i64)7)) {
        labelx = (struct aa_decls$strec *)aa_parse$exprlabeldef;
        valuex = aa_parse$exprvalue;
        typex = aa_parse$exprtype;
        aa_lex$lex();
        aa_parse$readterm();
        if (!!(aa_parse$exprlabeldef)) {
            aa_lib$serror((byte*)"+label?");
        }
;
        aa_parse$exprlabeldef = (struct aa_decls$strec *)labelx;
        if ((!!(typex) || !!(aa_parse$exprtype))) {
            aa_lib$serror((byte*)"add real");
        }
;
        aa_parse$exprvalue += valuex;
    }
    else if ((aa_lex$lxsymbol==(i64)8)) {
        labelx = (struct aa_decls$strec *)aa_parse$exprlabeldef;
        valuex = aa_parse$exprvalue;
        typex = aa_parse$exprtype;
        aa_lex$lex();
        aa_parse$readterm();
        if (!!(aa_parse$exprlabeldef)) {
            aa_lib$serror((byte*)"+label?");
        }
;
        aa_parse$exprlabeldef = (struct aa_decls$strec *)labelx;
        if ((!!(typex) || !!(aa_parse$exprtype))) {
            aa_lib$serror((byte*)"sub real");
        }
;
        aa_parse$exprvalue = (valuex - aa_parse$exprvalue);
    }
    else if ((aa_lex$lxsymbol==(i64)9)) {
        labelx = (struct aa_decls$strec *)aa_parse$exprlabeldef;
        valuex = aa_parse$exprvalue;
        typex = aa_parse$exprtype;
        aa_lex$lex();
        aa_parse$readterm();
        if (!!(aa_parse$exprlabeldef)) {
            aa_lib$serror((byte*)"+label?");
        }
;
        aa_parse$exprlabeldef = (struct aa_decls$strec *)labelx;
        if ((!!(typex) || !!(aa_parse$exprtype))) {
            aa_lib$serror((byte*)"add real");
        }
;
        aa_parse$exprvalue *= valuex;
    }
    else {
        goto L144 ;
    }
goto L143 ;
L144 :;
    ;
    return (struct aa_decls$opndrec *)aa_lib$genimm_expr((struct aa_decls$strec *)aa_parse$exprlabeldef,aa_parse$exprvalue,aa_parse$exprtype,(i64)4);
}

static void aa_parse$readterm(void) {
        struct aa_decls$strec *  symptr;
        r64 x;
    aa_parse$exprlabeldef = 0;
    aa_parse$exprvalue = (i64)0;
    aa_parse$exprtype = (i64)0;
    switch (aa_lex$lxsymbol) {
    case 19:;
    case 20:;
    case 22:;
        {
            aa_parse$exprlabeldef = (struct aa_decls$strec *)aa_lex$lxsymptr;
            aa_lex$lex();
            if ((aa_lex$lxsymbol == (i64)9)) {
                aa_lib$serror((byte*)"* applied to non-extern label or applied inconsistently");
            }
;
        }
        break;
    case 21:;
        {
            aa_parse$exprlabeldef = (struct aa_decls$strec *)aa_lex$lxsymptr;
            aa_lex$lex();
            if ((aa_lex$lxsymbol != (i64)9)) {
                msysc$m_print_startcon();
                msysc$m_print_str((*aa_lex$lxsymptr).name,NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                aa_lib$serror((byte*)"* missing or applied inconsistently");
            }
;
            aa_lex$lex();
        }
        break;
    case 18:;
        {
            aa_parse$exprlabeldef = (struct aa_decls$strec *)(*(*aa_lex$lxsymptr).expr).labeldef;
            aa_parse$exprvalue = (*(*aa_lex$lxsymptr).expr).value;
            aa_parse$exprtype = (i64)(*(*aa_lex$lxsymptr).expr).valtype;
            aa_lex$lex();
        }
        break;
    case 17:;
        {
            symptr = (struct aa_decls$strec *)aa_lex$lxsymptr;
            aa_parse$exprlabeldef = (struct aa_decls$strec *)symptr;
            aa_lex$lex();
            if ((aa_lex$lxsymbol == (i64)9)) {
                aa_lib$createlabel((struct aa_decls$strec *)symptr,(i64)21);
                aa_lex$lex();
            }
            else {
                aa_lib$createlabel((struct aa_decls$strec *)symptr,(i64)19);
            }
;
        }
        break;
    case 14:;
        {
            aa_parse$exprvalue = aa_lex$lxvalue;
            aa_lex$lex();
        }
        break;
    case 15:;
        {
            aa_parse$exprvalue = *(i64*)&aa_lex$lxxvalue;
            aa_parse$exprtype = (i64)82;
            aa_lex$lex();
        }
        break;
    case 8:;
        {
            aa_lex$lex();
            aa_parse$readterm();
            if (!(!!(aa_parse$exprlabeldef))) {
                if (!(!!(aa_parse$exprtype))) {
                    aa_parse$exprvalue = -(aa_parse$exprvalue);
                }
                else {
                    x = -(*(r64*)&aa_parse$exprvalue);
                    aa_parse$exprvalue = *(i64*)&x;
                }
;
            }
            else {
                aa_lib$serror((byte*)"neg/label");
            }
;
        }
        break;
    case 7:;
        {
            aa_lex$lex();
            aa_parse$readterm();
        }
        break;
    default: {
        aa_lib$serror((byte*)"READTERM");
    }
    } //SW
;
}

static void aa_parse$readreg(i64 *reg,i64 *regsize,i64 *scale) {
    (*reg) = aa_lex$lxsubcode;
    (*regsize) = (i64)(*aa_lex$lxsymptr).regsize;
    aa_lex$lex();
    if ((aa_lex$lxsymbol == (i64)9)) {
        aa_lex$lex();
        aa_parse$checksymbol((i64)14);
        if ((aa_lex$lxvalue==(i64)1) || (aa_lex$lxvalue==(i64)2) || (aa_lex$lxvalue==(i64)4) || (aa_lex$lxvalue==(i64)8)) {
        }
        else {
            aa_lib$serror((byte*)"*n must be 1,2,4,8");
        }
;
        (*scale) = aa_lex$lxvalue;
        aa_lex$lex();
    }
    else {
        (*scale) = (i64)0;
    }
;
}

static struct aa_decls$opndrec *aa_parse$readaddrmode(i64 size) {
        i64 reg;
        i64 regsize;
        i64 scale;
        i64 regix;
        i64 regixsize;
        i64 scaleix;
        struct aa_decls$opndrec *  x;
        struct aa_decls$opndrec *  p;
    reg = (regix = (i64)0);
    regsize = (regixsize = (i64)0);
    scale = (scaleix = (i64)0);
    x = 0;
    if ((aa_lex$lxsymbol == (i64)24)) {
        aa_parse$readreg(&reg,&regsize,&scale);
        if ((aa_lex$lxsymbol==(i64)7)) {
            aa_lex$lex();
            if ((aa_lex$lxsymbol == (i64)24)) {
                aa_parse$readreg(&regix,&regixsize,&scaleix);
                if ((aa_lex$lxsymbol==(i64)7) || (aa_lex$lxsymbol==(i64)8)) {
                    x = (struct aa_decls$opndrec *)aa_parse$readexpression();
                }
;
            }
            else {
                x = (struct aa_decls$opndrec *)aa_parse$readexpression();
            }
;
        }
        else if ((aa_lex$lxsymbol==(i64)8)) {
            x = (struct aa_decls$opndrec *)aa_parse$readexpression();
        }
;
    }
    else {
        x = (struct aa_decls$opndrec *)aa_parse$readexpression();
    }
;
    if ((!!(scale) && !!(scaleix))) {
        aa_lib$serror((byte*)"Two *N scales");
    }
;
    aa_parse$checksymbol((i64)6);
    aa_lex$lex();
    if ((!!(scale) && !(!!(scaleix)))) {
        {i64 temp = reg; reg = regix; regix = temp; };
        {i64 temp = regsize; regsize = regixsize; regixsize = temp; };
        {i64 temp = scale; scale = scaleix; scaleix = temp; };
    }
;
    if ((scaleix == (i64)0)) {
        scaleix = (i64)1;
    }
;
    if (((!!(regsize) && !!(regixsize)) && (regsize != regixsize))) {
        aa_lib$serror((byte*)"Addr reg size mismatch");
    }
;
    p = (struct aa_decls$opndrec *)aa_lib$genindex(reg,regix,scaleix,(struct aa_decls$opndrec *)x,size,(((regsize == (i64)4) || (regixsize == (i64)4)) ? (i64)4 : (i64)8));
    return (struct aa_decls$opndrec *)p;
}

// START
void aa_parse$start(void) {

}

void aa_showss$writemcx(u8 *filename) {
        void *  f;
        i64 n;
        i64 i;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"WRITEMCX",NULL);
    msysc$m_print_str(filename,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    aa_decls$ss_zdatalen = mlib$roundtoblock(aa_decls$ss_zdatalen,(i64)8);
    aa_showss$roundsegment((struct aa_decls$dbuffer *)aa_decls$ss_code,(i64)8,(i64)144);
    aa_showss$roundsegment((struct aa_decls$dbuffer *)aa_decls$ss_idata,(i64)8,(i64)0);
    f = fopen(filename,(byte*)"wb");
    mlib$outword32(f,(u64)441992013u);
    mlib$outbyte(f,(i64)1);
    mlib$outstring(f,(byte*)"0.1234");
    aa_showss$scansymbols();
    aa_showss$writerelocs(f);
    mlib$outbyte(f,(i64)4);
    mlib$outword32(f,(u64)aa_decls$ss_zdatalen);
    mlib$outbyte(f,(i64)2);
    mlib$outword32(f,(u64)(n = aa_lib$bufferlength((struct aa_decls$dbuffer *)aa_decls$ss_code)));
    mlib$outblock(f,aa_lib$bufferelemptr((struct aa_decls$dbuffer *)aa_decls$ss_code,(i64)0),n);
    mlib$outbyte(f,(i64)3);
    mlib$outword32(f,(u64)(n = aa_lib$bufferlength((struct aa_decls$dbuffer *)aa_decls$ss_idata)));
    mlib$outblock(f,aa_lib$bufferelemptr((struct aa_decls$dbuffer *)aa_decls$ss_idata,(i64)0),n);
    mlib$outbyte(f,(i64)6);
    mlib$outword32(f,(u64)aa_decls$nsearchlibs);
    for (i=(i64)1;i<=aa_decls$nsearchlibs;++i) {
L145 :;
        mlib$outstring(f,aa_decls$searchlibs[(i)-1]);
L146 :;
    }
L147 :;
    ;
    mlib$outbyte(f,(i64)7);
    mlib$outword32(f,(u64)aa_decls$nimportlibs);
    for (i=(i64)1;i<=aa_decls$nimportlibs;++i) {
L148 :;
        mlib$outstring(f,aa_decls$importlibs[(i)-1]);
L149 :;
    }
L150 :;
    ;
    aa_showss$writesymbols(f);
    mlib$outbyte(f,(i64)13);
    fclose(f);
}

struct mlib$strbuffer *aa_showss$showssdata(void) {
    mlib$gs_init((struct mlib$strbuffer *)aa_lib$dest);
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"AFTER GENSS");
    aa_showss$showsections();
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    aa_showss$showsectionrelocs2((byte*)"Idata",(struct aa_decls$relocrec *)aa_decls$ss_idatarelocs,aa_decls$ss_nidatarelocs);
    aa_showss$showsectionrelocs2((byte*)"Code",(struct aa_decls$relocrec *)aa_decls$ss_coderelocs,aa_decls$ss_ncoderelocs);
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"proc Section Zdata: ");
    mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,aa_decls$ss_zdatalen);
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    aa_showss$showsectiondata((struct aa_decls$dbuffer *)aa_decls$ss_idata);
    aa_showss$showsectioncode((struct aa_decls$dbuffer *)aa_decls$ss_code);
    aa_showss$showsymboltable2();
    aa_showss$showimporttable();
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"END OF GENSS");
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    return (struct mlib$strbuffer *)aa_lib$dest;
}

static void aa_showss$showsectiondata(struct aa_decls$dbuffer *d) {
        i64 i;
        i64 k;
        i64 length;
        i64 bb;
        u8 str[128];
        u8 str2[128];
        byte *  p;
        byte *  baseaddr;
        i64 $av_1;
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"proc Section ");
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"Idata:");
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)" Size:");
    length = aa_lib$bufferlength((struct aa_decls$dbuffer *)d);
    mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,length);
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    k = (i64)0;
    p = (byte *)aa_lib$bufferelemptr((struct aa_decls$dbuffer *)d,(i64)0);
    str[((i64)1)-1] = (u64)0u;
    baseaddr = 0;
    msysc$m_print_startstr(str2);
    msysc$m_print_ptr(baseaddr,(byte*)"Z8H");
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)": ",NULL);
    msysc$m_print_end();
    ;
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str2);
    for (i=(i64)1;i<=length;++i) {
L151 :;
        bb = (i64)(*(p)++);
        msysc$m_print_startstr(str2);
        msysc$m_print_i64(bb,(byte*)"z2H");
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)" ",NULL);
        msysc$m_print_end();
        ;
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str2);
        if (((i64)32<=bb && bb<=(i64)127)) {
            str2[((i64)1)-1] = (u64)bb;
            str2[((i64)2)-1] = (u64)0u;
            strcat((u8 *)str,(u8 *)str2);
        }
        else {
            strcat((u8 *)str,(byte*)".");
        }
;
        if (((++(k) == (i64)16) || (i == length))) {
            if ((k < (i64)16)) {
                $av_1 = ((i64)16 - k);
                while ($av_1-- > 0) {
L154 :;
                    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"   ");
                    strcat((u8 *)str,(byte*)" ");
L155 :;
                }
L156 :;
                ;
            }
;
            mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"\t[");
            mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str);
            mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"]");
            k = (i64)0;
            str[((i64)1)-1] = (u64)0u;
            baseaddr += (i64)16;
            msysc$m_print_startstr(str2);
            msysc$m_print_ptr(baseaddr,(byte*)"z8h");
            msysc$m_print_nogap();
            msysc$m_print_str((byte*)": ",NULL);
            msysc$m_print_end();
            ;
            mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str2);
        }
;
L152 :;
    }
L153 :;
    ;
    if ((k == (i64)0)) {
        mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    }
;
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    if (!!(k)) {
        mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    }
;
}

static void aa_showss$showsectioncode(struct aa_decls$dbuffer *d) {
        byte *  codeptr;
        byte *  codeend;
        byte *  codestart;
        i64 length;
        i64 offset;
        u8 *  s;
        u8 str[16];
        byte *  baseaddr;
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"proc Section Code");
    length = aa_lib$bufferlength((struct aa_decls$dbuffer *)d);
    codestart = (codeptr = (byte *)aa_lib$bufferelemptr((struct aa_decls$dbuffer *)d,(i64)0));
    codeend = (codeptr + length);
    baseaddr = 0;
    L157 :;
    while ((codeptr < codeend)) {
        offset = (codeptr - codestart);
        s = aa_disasm$decodeinstr(&codeptr,(baseaddr + offset));
        if ((s == 0)) {
            goto L159 ;
        }
;
        msysc$m_print_startstr(str);
        msysc$m_print_i64(offset,(byte*)"4");
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)" ",NULL);
        msysc$m_print_end();
        ;
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str);
        mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,s);
L158 :;
    }
L159 :;
    ;
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
}

static void aa_showss$gs_value(u8 *caption,i64 value) {
        u8 str[256];
    strcpy((u8 *)str,caption);
    strcat((u8 *)str,(byte*)":");
    mlib$ipadstr((u8 *)str,(i64)20,(byte*)" ");
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str);
    msysc$m_print_startstr(str);
    msysc$m_print_setfmt((byte*)"0x# #");
    msysc$m_print_i64(value,(byte*)"H");
    msysc$m_print_i64(value,NULL);
    msysc$m_print_end();
    ;
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str);
}

static void aa_showss$showsymboltable2(void) {
        u8 str[300];
        struct aa_decls$strec *  d;
        i64 i;
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"Proc Symbol Table");
    for (i=(i64)1;i<=aa_decls$ss_nsymbols;++i) {
L160 :;
        d = (struct aa_decls$strec *)(*aa_decls$ss_symboltable)[(i)-1];
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"#: # # #");
        msysc$m_print_i64(i,NULL);
        msysc$m_print_str((*d).name,(byte*)"20jl");
        msysc$m_print_str(aa_tables$symbolnames[((i64)(*d).symbol)-1],(byte*)"15jl");
        msysc$m_print_str(aa_mcxdecls$segmentnames[((i64)(*d).segment)-1],NULL);
        msysc$m_print_end();
        ;
        mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,str);
L161 :;
    }
L162 :;
    ;
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
}

static void aa_showss$showimporttable(void) {
        u8 str[256];
        struct aa_objdecls$importrec p;
        i64 i;
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"Proc Dll List");
    for (i=(i64)1;i<=aa_decls$nsearchlibs;++i) {
L163 :;
        mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,i);
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)": ");
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,aa_decls$searchlibs[(i)-1]);
        mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
L164 :;
    }
L165 :;
    ;
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"Proc Lib List");
    for (i=(i64)1;i<=aa_decls$nimportlibs;++i) {
L166 :;
        mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,i);
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)": ");
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,aa_decls$importlibs[(i)-1]);
        mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
L167 :;
    }
L168 :;
    ;
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"Proc Import List");
    for (i=(i64)1;i<=aa_writeexe$nimports;++i) {
L169 :;
        p = aa_writeexe$importtable[(i)-1];
        mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,i);
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)": ");
        if (!!(p.libno)) {
            strcpy((u8 *)str,p.name);
            mlib$ipadstr((u8 *)str,(i64)16,(byte*)" ");
            mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str);
            mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)" (");
            mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,aa_writeexe$dlltable[(p.libno)-1].name);
            mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)")");
            aa_showss$gs_value((byte*)"\tIAT Offset        ",p.iatoffset);
            aa_showss$gs_value((byte*)"\tThunk Offset      ",p.thunkoffset);
            aa_showss$gs_value((byte*)"\tHint/Name Offset  ",p.hintnameoffset);
        }
        else {
            strcpy((u8 *)str,p.name);
            mlib$ipadstr((u8 *)str,(i64)20,(byte*)" ");
            mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str);
            mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)" (---)");
        }
;
L170 :;
    }
L171 :;
    ;
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
}

static void aa_showss$showsections(void) {
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"proc Sections");
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"Section Zdata: Size:");
    mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,aa_decls$ss_zdatalen);
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"Section Idata: Size:");
    mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,aa_lib$bufferlength((struct aa_decls$dbuffer *)aa_decls$ss_idata));
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"Section Code: Size:");
    mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,aa_lib$bufferlength((struct aa_decls$dbuffer *)aa_decls$ss_code));
}

static void aa_showss$writerelocs(void *f) {
        struct aa_decls$relocrec *  oldr;
        struct aa_mcxdecls$mcxreloc newr;
        i64 n;
        i64 count;
        struct aa_decls$strec *  d;
        u64 *  baseptr64;
        i64 i;
    mlib$outbyte(f,(i64)5);
    mlib$outword32(f,(u64)(n = (aa_decls$ss_nidatarelocs + aa_decls$ss_ncoderelocs)));
    count = (i64)0;
    for (i=(i64)1;i<=(i64)2;++i) {
L172 :;
        oldr = (struct aa_decls$relocrec *)((i == (i64)1) ? aa_decls$ss_idatarelocs : aa_decls$ss_coderelocs);
        L175 :;
        while (!!(oldr)) {
            ++(count);
            memset(&(newr),0,8);
            newr.offset = (*oldr).offset;
            newr.segment = ((i == (i64)1) ? (i64)2 : (i64)1);
            d = (struct aa_decls$strec *)(*aa_decls$ss_symboltable)[((*oldr).stindex)-1];
                        {i64 $temp = (*oldr).reloctype;
if (($temp==(i64)4)) {
                                {i64 $temp = (i64)(*d).symbol;
if (($temp==(i64)21)) {
                    newr.stindex = (i64)(*d).impindex;
                    newr.reloctype = (i64)5;
                }
                else {
                    aa_lib$gerror((byte*)"rel32/rel not imported");
                }
                };
            }
            else if (($temp==(i64)2) || ($temp==(i64)1)) {
                                {i64 $temp = (i64)(*d).symbol;
if (($temp==(i64)21)) {
                    newr.reloctype = (((*oldr).reloctype == (i64)2) ? (i64)3 : (i64)4);
                    newr.stindex = (i64)(*d).impindex;
                }
                else {
                    if (((*oldr).reloctype == (i64)2)) {
                        newr.reloctype = (i64)1;
                    }
                    else {
                        newr.reloctype = (i64)2;
                    }
;
                    newr.targetsegment = (i64)(*d).segment;
                }
                };
            }
            else {
                aa_lib$gerror((byte*)"reloc?");
            }
            };
            mlib$outblock(f,&newr,(i64)8);
L176 :;
            oldr = (struct aa_decls$relocrec *)(*oldr).nextreloc;
L178 :;
                    }
L177 :;
        ;
L173 :;
    }
L174 :;
    ;
}

static void aa_showss$scansymbols(void) {
        struct aa_decls$strec *  d;
        i64 i;
    for (i=(i64)1;i<=aa_decls$ss_nsymbols;++i) {
L179 :;
        d = (struct aa_decls$strec *)(*aa_decls$ss_symboltable)[(i)-1];
                {i64 $temp = (i64)(*d).symbol;
if (($temp==(i64)22)) {
            (*d).expindex = ++(aa_showss$nsymexports);
        }
        else if (($temp==(i64)21)) {
            (*d).impindex = ++(aa_showss$nsymimports);
        }
        };
L180 :;
    }
L181 :;
    ;
}

static void aa_showss$writesymbols(void *f) {
        struct aa_decls$strec *  d;
        u64 epoffset;
        i64 i;
    epoffset = (u64)18446744073709551615u;
    mlib$outbyte(f,(i64)8);
    mlib$outword32(f,(u64)aa_showss$nsymimports);
    for (i=(i64)1;i<=aa_decls$ss_nsymbols;++i) {
L182 :;
        if (!!((i64)(*(*aa_decls$ss_symboltable)[(i)-1]).impindex)) {
            mlib$outstring(f,(*(*aa_decls$ss_symboltable)[(i)-1]).name);
        }
;
L183 :;
    }
L184 :;
    ;
    mlib$outbyte(f,(i64)9);
    mlib$outword32(f,(u64)aa_showss$nsymexports);
    for (i=(i64)1;i<=aa_decls$ss_nsymbols;++i) {
L185 :;
        d = (struct aa_decls$strec *)(*aa_decls$ss_symboltable)[(i)-1];
        if (!!((i64)(*d).expindex)) {
            if ((((i64)epoffset == (i64)-1) && (!!(mlib$eqstring((*d).name,(byte*)"start")) || !!(mlib$eqstring((*d).name,(byte*)"main"))))) {
                epoffset = (u64)(i64)(*d).offset;
            }
;
            mlib$outstring(f,(*d).name);
        }
;
L186 :;
    }
L187 :;
    ;
    mlib$outbyte(f,(i64)10);
    mlib$outword32(f,(u64)aa_showss$nsymexports);
    for (i=(i64)1;i<=aa_decls$ss_nsymbols;++i) {
L188 :;
        d = (struct aa_decls$strec *)(*aa_decls$ss_symboltable)[(i)-1];
        if (!!((i64)(*d).expindex)) {
            mlib$outbyte(f,(i64)(*d).segment);
        }
;
L189 :;
    }
L190 :;
    ;
    mlib$outbyte(f,(i64)11);
    mlib$outword32(f,(u64)aa_showss$nsymexports);
    for (i=(i64)1;i<=aa_decls$ss_nsymbols;++i) {
L191 :;
        d = (struct aa_decls$strec *)(*aa_decls$ss_symboltable)[(i)-1];
        if (!!((i64)(*d).expindex)) {
            mlib$outword32(f,(u64)(i64)(*d).offset);
        }
;
L192 :;
    }
L193 :;
    ;
    mlib$outbyte(f,(i64)12);
    mlib$outword32(f,epoffset);
}

static void aa_showss$roundsegment(struct aa_decls$dbuffer *p,i64 align,i64 value) {
        i64 length;
        i64 newlength;
        i64 $av_1;
    length = aa_lib$bufferlength((struct aa_decls$dbuffer *)p);
    newlength = mlib$roundtoblock(length,align);
    aa_lib$buffercheck((struct aa_decls$dbuffer *)p,align);
    $av_1 = (newlength - length);
    while ($av_1-- > 0) {
L194 :;
        (*((*p).pcurr)++) = value;
L195 :;
    }
L196 :;
    ;
}

static void aa_showss$showsectionrelocs2(u8 *caption,struct aa_decls$relocrec *relocs,i64 nrelocs) {
        struct aa_decls$relocrec *  r;
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"proc Section Relocs: ");
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,caption);
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)" ");
    mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,nrelocs);
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    r = (struct aa_decls$relocrec *)relocs;
    L197 :;
    while (!!(r)) {
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"Reloc: ");
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,aa_objdecls$relocnames[((*r).reloctype)]);
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)" Offset: ");
        mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,(*r).offset);
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)" ST Index: ");
        mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,(*r).stindex);
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)" ");
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(*(*aa_decls$ss_symboltable)[((*r).stindex)-1]).name);
        mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
        r = (struct aa_decls$relocrec *)(*r).nextreloc;
L198 :;
    }
L199 :;
    ;
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
}

// START
void aa_showss$start(void) {

}

void aa_writeobj$writess(u8 *outfile) {
    aa_writeobj$writecoff(outfile);
}

static void aa_writeobj$writerecord(void *r,i64 length) {
    memcpy(aa_writeobj$dataptr,r,(u64)length);
    aa_writeobj$dataptr += length;
}

static void aa_writeobj$writerelocs(struct aa_decls$relocrec *r,i64 nrelocs) {
        static struct aa_objdecls$coffrelocrec s;
        struct aa_decls$strec *  d;
    if ((nrelocs == (i64)0)) {
        return;
    }
;
    L200 :;
    while (!!(r)) {
                {i64 $temp = (*r).reloctype;
if (($temp==(i64)2) || ($temp==(i64)1)) {
            d = (struct aa_decls$strec *)(*aa_decls$ss_symboltable)[((*r).stindex)-1];
                        {i64 $temp = (i64)(*d).segment;
if (($temp==(i64)3)) {
                s.stindex = (i64)2;
            }
            else if (($temp==(i64)2)) {
                s.stindex = (i64)4;
            }
            else if (($temp==(i64)1)) {
                s.stindex = (i64)6;
            }
            else if (($temp==(i64)0)) {
                s.stindex = ((*r).stindex + aa_writeobj$stoffset);
            }
            else {
                aa_lib$gerror((byte*)"wrelocs/bad seg");
            }
            };
        }
        else {
            s.stindex = ((*r).stindex + aa_writeobj$stoffset);
        }
        };
        s.reloctype = (*r).reloctype;
        s.virtualaddr = (*r).offset;
        memcpy(aa_writeobj$dataptr,&s,(u64)10u);
        aa_writeobj$dataptr += (i64)10;
        r = (struct aa_decls$relocrec *)(*r).nextreloc;
L201 :;
    }
L202 :;
    ;
}

static void aa_writeobj$writedata(struct aa_decls$dbuffer *data) {
    memcpy(aa_writeobj$dataptr,aa_lib$bufferelemptr((struct aa_decls$dbuffer *)data,(i64)0),(u64)aa_lib$bufferlength((struct aa_decls$dbuffer *)data));
    aa_writeobj$dataptr += aa_lib$bufferlength((struct aa_decls$dbuffer *)data);
}

static void aa_writeobj$writesymboltable(void) {
        i64 i;
    for (i=(i64)1;i<=aa_writeobj$nsymbols;++i) {
L203 :;
        aa_writeobj$writerecord(&aa_writeobj$symboltable[(i)],(i64)18);
L204 :;
    }
L205 :;
    ;
}

static void aa_writeobj$writestringtable(void) {
        i32 *  p;
        i64 i;
        i64 n;
    p = (i32 *)aa_writeobj$dataptr;
    (*p) = aa_writeobj$nextstringoffset;
    aa_writeobj$dataptr += (i64)4;
    for (i=(i64)1;i<=aa_writeobj$nstrings;++i) {
L206 :;
        n = (aa_writeobj$stringlengths[(i)-1] + (i64)1);
        memcpy(aa_writeobj$dataptr,(void *)aa_writeobj$stringtable[(i)-1],(u64)n);
        aa_writeobj$dataptr += n;
L207 :;
    }
L208 :;
    ;
}

static struct aa_objdecls$imagesymbol *aa_writeobj$makesymbol(u8 *name,i64 namelen,i64 value,i64 sectionno,i64 symtype,i64 storage,i64 naux) {
        static struct aa_objdecls$imagesymbol r;
    if ((namelen == (i64)0)) {
        namelen = strlen(name);
    }
;
    if ((namelen < (i64)8)) {
        strcpy((u8 *)&r.shortname[((i64)1)-1],name);
    }
    else if ((namelen == (i64)8)) {
        memcpy(&r.shortname[((i64)1)-1],(void *)name,(u64)namelen);
    }
    else {
        r.shortx = (i64)0;
        r.longx = aa_writeobj$addstringentry(name,namelen);
    }
;
    r.value = value;
    r.sectionno = sectionno;
    r.symtype = symtype;
    r.storageclass = storage;
    r.nauxsymbols = naux;
    return &r;
}

static void aa_writeobj$addsymbol(struct aa_objdecls$imagesymbol *r) {
    if ((aa_writeobj$nsymbols >= (i64)13000)) {
        aa_lib$gerror((byte*)"as:Too many symbols");
    }
;
    memcpy(&aa_writeobj$symboltable[(++(aa_writeobj$nsymbols))],r,(u64)8u);
}

static void aa_writeobj$initsymboltable(u8 *filename) {
    aa_writeobj$nsymbols = (i64)0;
    aa_writeobj$addsymbol((struct aa_objdecls$imagesymbol *)aa_writeobj$makesymbol((byte*)".file",(i64)0,(i64)0,(i64)-2,(i64)0,(i64)103,(i64)1));
    aa_writeobj$addsymbol((struct aa_objdecls$imagesymbol *)aa_writeobj$strtoaux(filename));
    aa_writeobj$addsymbol((struct aa_objdecls$imagesymbol *)aa_writeobj$makesymbol((byte*)".bss",(i64)0,(i64)0,(i64)1,(i64)0,(i64)3,(i64)1));
    aa_writeobj$addsymbol((struct aa_objdecls$imagesymbol *)aa_writeobj$sectiontoaux(0,(i64)0));
    aa_writeobj$addsymbol((struct aa_objdecls$imagesymbol *)aa_writeobj$makesymbol((byte*)".data",(i64)0,(i64)0,(i64)2,(i64)0,(i64)3,(i64)1));
    aa_writeobj$addsymbol((struct aa_objdecls$imagesymbol *)aa_writeobj$sectiontoaux((struct aa_decls$dbuffer *)aa_decls$ss_idata,aa_decls$ss_nidatarelocs));
    aa_writeobj$addsymbol((struct aa_objdecls$imagesymbol *)aa_writeobj$makesymbol((byte*)".text",(i64)0,(i64)0,(i64)3,(i64)0,(i64)3,(i64)1));
    aa_writeobj$addsymbol((struct aa_objdecls$imagesymbol *)aa_writeobj$sectiontoaux((struct aa_decls$dbuffer *)aa_decls$ss_code,aa_decls$ss_ncoderelocs));
}

static struct aa_objdecls$imagesymbol *aa_writeobj$strtoaux(u8 *s) {
        static struct aa_objdecls$imagesymbol r;
        byte *  p;
        i64 n;
    p = (byte *)&r;
    memset(p,(i32)(i64)0,(u64)18u);
    n = (i64)0;
    L209 :;
    while ((((i64)(u64)(*s) != (i64)0) && (n < (i64)18))) {
        (*(p)++) = (i64)(u64)(*(s)++);
        ++(n);
L210 :;
    }
L211 :;
    ;
    return (struct aa_objdecls$imagesymbol *)&r;
}

static struct aa_objdecls$auxsectionrec *aa_writeobj$sectiontoaux(struct aa_decls$dbuffer *data,i64 nrelocs) {
        static struct aa_objdecls$auxsectionrec r;
    memset(&(r),0,18);
    if ((data == 0)) {
        r.length = aa_decls$ss_zdatalen;
    }
    else {
        r.length = aa_lib$bufferlength((struct aa_decls$dbuffer *)data);
    }
;
    r.nrelocs = nrelocs;
    return &r;
}

static i64 aa_writeobj$addstringentry(u8 *s,i64 length) {
        i64 offset;
    offset = aa_writeobj$nextstringoffset;
    if ((aa_writeobj$nstrings > (i64)5000)) {
        aa_lib$gerror((byte*)"W:too many strings");
    }
;
    aa_writeobj$stringtable[(++(aa_writeobj$nstrings))-1] = s;
    aa_writeobj$stringlengths[(aa_writeobj$nstrings)-1] = length;
    aa_writeobj$nextstringoffset += (length + (i64)1);
    return offset;
}

static void aa_writeobj$convertsymboltable(void) {
        struct aa_decls$strec *  s;
        u8 *  name;
        i64 i;
        i64 sect;
        i64 scope;
    aa_writeobj$stoffset = (aa_writeobj$nsymbols - (i64)1);
    aa_writeobj$nstrings = (i64)0;
    aa_writeobj$nextstringoffset = (i64)4;
    for (i=(i64)1;i<=aa_decls$ss_nsymbols;++i) {
L212 :;
        s = (struct aa_decls$strec *)(*aa_decls$ss_symboltable)[(i)-1];
        name = (*s).name;
                {i64 $temp = (i64)(*s).segment;
if (($temp==(i64)3)) {
            sect = (i64)1;
        }
        else if (($temp==(i64)2)) {
            sect = (i64)2;
        }
        else if (($temp==(i64)1)) {
            sect = (i64)3;
        }
        else {
            sect = (i64)0;
        }
        };
                {i64 $temp = (i64)(*s).symbol;
if (($temp==(i64)19) || ($temp==(i64)20)) {
            scope = (i64)3;
        }
        else if (($temp==(i64)21) || ($temp==(i64)22)) {
            scope = (i64)2;
        }
        else {
            scope = (i64)0;
        }
        };
        aa_writeobj$addsymbol((struct aa_objdecls$imagesymbol *)aa_writeobj$makesymbol((*s).name,(i64)(*s).namelen,(i64)(*s).offset,sect,(i64)0,scope,(i64)0));
L213 :;
    }
L214 :;
    ;
}

static void aa_writeobj$writecoff(u8 *outfile) {
        struct aa_objdecls$imagefileheader header;
        struct aa_objdecls$imagesectionheader zsection;
        struct aa_objdecls$imagesectionheader isection;
        struct aa_objdecls$imagesectionheader csection;
        i64 offset;
    memset(&(header),0,20);
    memset(&(zsection),0,40);
    memset(&(isection),0,40);
    memset(&(csection),0,40);
    header.machine = (i64)34404;
    header.nsections = (i64)3;
    strcpy((u8 *)&zsection.name[((i64)1)-1],(byte*)".bss");
    zsection.rawdata_size = aa_decls$ss_zdatalen;
    zsection.characteristics = (i64)3225419904;
    if (((aa_decls$ss_nidatarelocs >= (i64)65536) || (aa_decls$ss_ncoderelocs >= (i64)65536))) {
        aa_lib$gerror((byte*)"Too many relocs (exceeds 16-bit field)");
    }
;
    strcpy((u8 *)&isection.name[((i64)1)-1],(byte*)".data");
    isection.rawdata_size = aa_lib$bufferlength((struct aa_decls$dbuffer *)aa_decls$ss_idata);
    isection.nrelocs = aa_decls$ss_nidatarelocs;
    isection.characteristics = (i64)3226468416;
    strcpy((u8 *)&csection.name[((i64)1)-1],(byte*)".text");
    csection.rawdata_size = aa_lib$bufferlength((struct aa_decls$dbuffer *)aa_decls$ss_code);
    csection.nrelocs = aa_decls$ss_ncoderelocs;
    csection.characteristics = (i64)1615855648;
    aa_writeobj$initsymboltable(outfile);
    aa_writeobj$convertsymboltable();
    offset = (i64)20;
    offset += (i64)120;
    if (!!((i64)isection.nrelocs)) {
        isection.relocations_ptr = offset;
        offset += ((i64)isection.nrelocs * (i64)10);
    }
;
    if (!!((i64)csection.nrelocs)) {
        csection.relocations_ptr = offset;
        offset += ((i64)csection.nrelocs * (i64)10);
    }
;
    isection.rawdata_offset = offset;
    offset += (i64)isection.rawdata_size;
    csection.rawdata_offset = offset;
    offset += (i64)csection.rawdata_size;
    header.symtaboffset = offset;
    offset += (aa_writeobj$nsymbols * (i64)18);
    header.nsymbols = aa_writeobj$nsymbols;
    offset += aa_writeobj$nextstringoffset;
    aa_writeobj$datastart = (aa_writeobj$dataptr = (byte *)malloc((u64)offset));
    aa_writeobj$writerecord(&header,(i64)20);
    aa_writeobj$writerecord(&zsection,(i64)40);
    aa_writeobj$writerecord(&isection,(i64)40);
    aa_writeobj$writerecord(&csection,(i64)40);
    aa_writeobj$writerelocs((struct aa_decls$relocrec *)aa_decls$ss_idatarelocs,aa_decls$ss_nidatarelocs);
    aa_writeobj$writerelocs((struct aa_decls$relocrec *)aa_decls$ss_coderelocs,aa_decls$ss_ncoderelocs);
    aa_writeobj$writedata((struct aa_decls$dbuffer *)aa_decls$ss_idata);
    aa_writeobj$writedata((struct aa_decls$dbuffer *)aa_decls$ss_code);
    aa_writeobj$writesymboltable();
    aa_writeobj$writestringtable();
    if (!!(aa_decls$fverbose)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Writing file:",NULL);
        msysc$m_print_str(outfile,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    mlib$writefile(outfile,aa_writeobj$datastart,(aa_writeobj$dataptr - aa_writeobj$datastart));
}

// START
void aa_writeobj$start(void) {

}

void aa_writeexe$writeexe(u8 *outfile,i64 dodll) {
        i64 i;
    aa_writeexe$dllfilename = outfile;
    aa_writeexe$isdll = dodll;
    aa_writeexe$datastart = (aa_writeexe$dataptr = (byte *)mlib$pcm_allocz(aa_writeexe$filesize));
    aa_writeexe$writedosstub();
    aa_writeexe$writepesig();
    aa_writeexe$writefileheader();
    aa_writeexe$writeoptheader();
    for (i=(i64)1;i<=aa_writeexe$nsections;++i) {
L215 :;
        aa_writeexe$writesectionheader((struct aa_objdecls$sectionrec *)&aa_writeexe$sectiontable[(i)-1]);
L216 :;
    }
L217 :;
    ;
    aa_writeexe$writepadding(aa_writeexe$sectiontable[((i64)1)-1].rawoffset);
    for (i=(i64)1;i<=aa_writeexe$nsections;++i) {
L218 :;
        aa_writeexe$writesectiondata((struct aa_objdecls$sectionrec *)&aa_writeexe$sectiontable[(i)-1]);
L219 :;
    }
L220 :;
    ;
    if (!!(aa_decls$fverbose)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Writing file:",NULL);
        msysc$m_print_str(outfile,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    if ((mlib$writefile(outfile,aa_writeexe$datastart,(aa_writeexe$dataptr - aa_writeexe$datastart)) == (i64)0)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Error writing exe file (possibly still running)",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        exit((i64)1);
    }
;
}

void aa_writeexe$genexe(u8 *entrypoint,u8 *outfile,i64 dodll) {
    aa_writeexe$dllfilename = outfile;
    aa_writeexe$isdll = dodll;
    aa_writeexe$imagebase = (u64)(!!(aa_writeexe$isdll) ? (i64)1713635328 : (i64)4194304);
    aa_writeexe$userentrypoint = entrypoint;
    aa_writeexe$loadlibs();
    aa_writeexe$scanst();
    aa_writeexe$getoffsets();
    aa_writeexe$relocdata(&aa_writeexe$sectiontable[((i64)1)-1]);
    aa_writeexe$relocdata(&aa_writeexe$sectiontable[((i64)2)-1]);
}

static void aa_writeexe$loadlibs(void) {
        i64 i;
        i64 hinst;
        u8 filename[300];
    for (i=(i64)1;i<=aa_decls$nsearchlibs;++i) {
L221 :;
        strcpy((u8 *)filename,aa_decls$searchlibs[(i)-1]);
        strcat((u8 *)filename,(byte*)".dll");
        hinst = (i64)mwindows$os_getdllinst((u8 *)filename);
        if ((hinst == (i64)0)) {
            msysc$m_print_startcon();
            msysc$m_print_str(aa_decls$searchlibs[(i)-1],NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            msysc$m_print_startcon();
            msysc$m_print_str(filename,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            aa_lib$gerror((byte*)"Can't load search lib");
        }
;
        aa_writeexe$libinsttable[(i)-1] = hinst;
        aa_writeexe$libinstnames[(i)-1] = mlib$pcm_copyheapstring((u8 *)filename);
L222 :;
    }
L223 :;
    ;
}

void aa_writeexe$initsectiontable(void) {
    aa_writeexe$sectiontable[((i64)1)-1].name = (byte*)".text";
    aa_writeexe$sectiontable[((i64)1)-1].segtype = (i64)1;
    aa_writeexe$sectiontable[((i64)1)-1].data = (struct aa_decls$dbuffer *)aa_decls$ss_code;
    aa_writeexe$sectiontable[((i64)1)-1].virtsize = aa_lib$bufferlength((struct aa_decls$dbuffer *)aa_decls$ss_code);
    if ((aa_lib$bufferlength((struct aa_decls$dbuffer *)aa_decls$ss_idata) == (i64)0)) {
        aa_lib$addqword((struct aa_decls$dbuffer *)aa_decls$ss_idata,(i64)0);
    }
;
    aa_writeexe$sectiontable[((i64)2)-1].name = (byte*)".data";
    aa_writeexe$sectiontable[((i64)2)-1].segtype = (i64)2;
    aa_writeexe$sectiontable[((i64)2)-1].data = (struct aa_decls$dbuffer *)aa_decls$ss_idata;
    aa_writeexe$sectiontable[((i64)2)-1].virtsize = aa_lib$bufferlength((struct aa_decls$dbuffer *)aa_decls$ss_idata);
    aa_writeexe$sectiontable[((i64)2)-1].rawsize = mlib$roundtoblock(aa_writeexe$sectiontable[((i64)2)-1].virtsize,(i64)512);
    aa_writeexe$sectiontable[((i64)2)-1].nrelocs = aa_decls$ss_nidatarelocs;
    aa_writeexe$sectiontable[((i64)2)-1].relocs = (struct aa_decls$relocrec *)aa_decls$ss_idatarelocs;
    if ((aa_decls$ss_zdatalen == (i64)0)) {
        aa_decls$ss_zdatalen = (i64)16;
    }
;
    aa_writeexe$sectiontable[((i64)3)-1].name = (byte*)".bss";
    aa_writeexe$sectiontable[((i64)3)-1].segtype = (i64)3;
    aa_writeexe$sectiontable[((i64)3)-1].virtsize = aa_decls$ss_zdatalen;
    aa_writeexe$sectiontable[((i64)1)-1].rawsize = mlib$roundtoblock(aa_writeexe$sectiontable[((i64)1)-1].virtsize,(i64)512);
    aa_writeexe$sectiontable[((i64)1)-1].nrelocs = aa_decls$ss_ncoderelocs;
    aa_writeexe$sectiontable[((i64)1)-1].relocs = (struct aa_decls$relocrec *)aa_decls$ss_coderelocs;
    aa_writeexe$sectiontable[((i64)4)-1].name = (byte*)".idata";
    aa_writeexe$sectiontable[((i64)4)-1].segtype = (i64)5;
    aa_writeexe$sectiontable[((i64)4)-1].virtsize = (i64)0;
    aa_writeexe$sectiontable[((i64)4)-1].rawsize = (i64)0;
    aa_writeexe$nsections = (i64)4;
}

static u8 *aa_writeexe$extractlibname(u8 *name,i64 *libno,i64 moduleno) {
        u8 *  s;
        u8 *  name2;
        u8 str[256];
        i64 i;
        i64 n;
    name2 = 0;
    //reenter:
L224 :;
;
    s = name;
    (*libno) = (i64)0;
    L225 :;
    while (!!((u64)(*s))) {
        if (((u64)(*s) == '.')) {
            memcpy(str,(void *)name,(u64)(s - name));
            str[(((s - name) + (i64)1))-1] = (u64)0u;
            strcat((u8 *)str,(byte*)".dll");
            for (i=(i64)1;i<=aa_writeexe$ndlls;++i) {
L228 :;
                if (!!(mlib$eqstring((u8 *)str,aa_writeexe$dlltable[(i)-1].name))) {
                    (*libno) = i;
                    ++(aa_writeexe$dlltable[((*libno))-1].nprocs);
                    return (!!(name2) ? name2 : (s + (i64)1));
                }
;
L229 :;
            }
L230 :;
            ;
            if ((aa_writeexe$ndlls >= (i64)50)) {
                aa_lib$gerror((byte*)"Too many libs");
            }
;
            (*libno) = ++(aa_writeexe$ndlls);
            aa_writeexe$dlltable[((*libno))-1].name = mlib$pcm_copyheapstring((u8 *)str);
            aa_writeexe$dlltable[((*libno))-1].nprocs = (i64)1;
            return (!!(name2) ? name2 : (s + (i64)1));
        }
;
        ++(s);
L226 :;
    }
L227 :;
    ;
    for (i=(i64)1;i<=aa_decls$nsearchlibs;++i) {
L231 :;
        if (!!(mwindows$os_getdllprocaddr(aa_writeexe$libinsttable[(i)-1],name))) {
            n = i;
            goto L233 ;
        }
;
L232 :;
    }
    {
        msysc$m_print_startcon();
        msysc$m_print_str(name,NULL);
        msysc$m_print_str(aa_decls$moduletable[(moduleno)-1].name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        aa_lib$gerror((byte*)"Can't find external function");
    }
L233 :;
    ;
    if (!!(((*libno) = aa_writeexe$libnotable[(n)-1]))) {
        ++(aa_writeexe$dlltable[((*libno))-1].nprocs);
        return name;
    }
;
    strcpy((u8 *)str,aa_decls$searchlibs[(n)-1]);
    strcat((u8 *)str,(byte*)".dll");
    if ((aa_writeexe$ndlls >= (i64)50)) {
        aa_lib$gerror((byte*)"2:Too many libs");
    }
;
    (*libno) = ++(aa_writeexe$ndlls);
    aa_writeexe$dlltable[((*libno))-1].name = mlib$pcm_copyheapstring((u8 *)str);
    aa_writeexe$dlltable[((*libno))-1].nprocs = (i64)1;
    aa_writeexe$libnotable[(n)-1] = (*libno);
    return name;
}

static void aa_writeexe$scanst(void) {
        i64 i;
        i64 libno;
        struct aa_decls$strec *  d;
        u8 *  name;
    for (i=(i64)1;i<=aa_decls$ss_nsymbols;++i) {
L234 :;
        d = (struct aa_decls$strec *)(*aa_decls$ss_symboltable)[(i)-1];
                {i64 $temp = (i64)(*d).symbol;
if (($temp==(i64)21)) {
            if ((aa_writeexe$nimports >= (i64)1000)) {
                aa_lib$gerror((byte*)"genexe: Too many imports");
            }
;
            ++(aa_writeexe$nimports);
            name = aa_writeexe$extractlibname((*d).name,&libno,(i64)(*d).moduleno);
            aa_writeexe$importtable[(aa_writeexe$nimports)-1].libno = libno;
            aa_writeexe$importtable[(aa_writeexe$nimports)-1].name = name;
            aa_writeexe$importtable[(aa_writeexe$nimports)-1].def = (struct aa_decls$strec *)d;
            (*d).importindex = aa_writeexe$nimports;
        }
        else if (($temp==(i64)22)) {
            if (!!(aa_writeexe$userentrypoint)) {
                if (!!(mlib$eqstring((*d).name,aa_writeexe$userentrypoint))) {
                    aa_writeexe$stentrypoint = (struct aa_decls$strec *)d;
                }
;
            }
            else {
                if ((!!(mlib$eqstring((*d).name,(byte*)"main")) && !(!!(aa_writeexe$isdll)))) {
                    aa_writeexe$stentrypoint = (struct aa_decls$strec *)d;
                }
                else if ((!!(mlib$eqstring((*d).name,(byte*)"start")) && !(!!(aa_writeexe$isdll)))) {
                    aa_writeexe$stentrypoint2 = (struct aa_decls$strec *)d;
                }
                else if ((!!(mlib$eqstring((*d).name,(byte*)"dllmain")) && !!(aa_writeexe$isdll))) {
                    aa_writeexe$stentrypoint = (struct aa_decls$strec *)d;
                }
;
            }
;
            if ((!!(aa_writeexe$stentrypoint) && ((i64)(*aa_writeexe$stentrypoint).segment != (i64)1))) {
                aa_lib$gerror((byte*)"Entry point not in code seg");
            }
;
            if ((aa_writeexe$nexports >= (i64)3000)) {
                aa_lib$gerror((byte*)"gendll: Too many exports");
            }
;
            ++(aa_writeexe$nexports);
            aa_writeexe$exporttable[(aa_writeexe$nexports)-1].def = (struct aa_decls$strec *)d;
            aa_writeexe$exporttable[(aa_writeexe$nexports)-1].name = (*d).name;
        }
        };
L235 :;
    }
L236 :;
    ;
}

static void aa_writeexe$relocdata(struct aa_objdecls$sectionrec *s) {
        struct aa_objdecls$sectionrec *  u;
        struct aa_decls$relocrec *  r;
        byte *  p;
        u32 *  p32;
        u64 *  p64;
        struct aa_decls$strec *  d;
        u64 thunkoffset;
        i64 index;
    p = (byte *)aa_lib$bufferelemptr((struct aa_decls$dbuffer *)(*s).data,(i64)0);
    r = (struct aa_decls$relocrec *)(*s).relocs;
    L237 :;
    while (!!(r)) {
        d = (struct aa_decls$strec *)(*aa_decls$ss_symboltable)[((*r).stindex)-1];
        index = (i64)(*d).importindex;
        thunkoffset = (u64)aa_writeexe$importtable[(index)-1].thunkoffset;
                {i64 $temp = (*r).reloctype;
if (($temp==(i64)4)) {
            if (((i64)(*d).symbol != (i64)21)) {
                aa_lib$gerror((byte*)"rel32/not imported");
            }
;
            (*(u32 *)(p + (*r).offset)) = (((i64)thunkoffset - (*r).offset) - (i64)4);
        }
        else if (($temp==(i64)2) || ($temp==(i64)1)) {
            if (((i64)(*d).symbol == (i64)21)) {
                (*(u32 *)(p + (*r).offset)) = ((i64)(aa_writeexe$imagebase + thunkoffset) + aa_writeexe$sectiontable[((i64)1)-1].virtoffset);
            }
            else {
                                {i64 $temp = (i64)(*d).segment;
if (($temp==(i64)3)) {
                    u = (struct aa_objdecls$sectionrec *)&aa_writeexe$sectiontable[((i64)3)-1];
                }
                else if (($temp==(i64)2)) {
                    u = (struct aa_objdecls$sectionrec *)&aa_writeexe$sectiontable[((i64)2)-1];
                }
                else if (($temp==(i64)1)) {
                    u = (struct aa_objdecls$sectionrec *)&aa_writeexe$sectiontable[((i64)1)-1];
                }
                };
                p32 = (u32 *)(p + (*r).offset);
                if (((*r).reloctype == (i64)2)) {
                    (*p32) = (((i64)(*p32) + (*u).virtoffset) + (i64)aa_writeexe$imagebase);
                }
                else {
                    p64 = (u64 *)p32;
                    (*p64) = (u64)(((i64)(*p64) + (*u).virtoffset) + (i64)aa_writeexe$imagebase);
                }
;
            }
;
        }
        else {
            msysc$m_print_startcon();
            msysc$m_print_str(aa_objdecls$relocnames[((*r).reloctype)],NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            aa_lib$gerror((byte*)"Can't do this rel type");
        }
        };
        r = (struct aa_decls$relocrec *)(*r).nextreloc;
L238 :;
    }
L239 :;
    ;
}

static void aa_writeexe$getbaserelocs(struct aa_objdecls$sectionrec *s) {
        struct aa_objdecls$sectionrec *  u;
        struct aa_decls$relocrec *  r;
        byte *  p;
        struct aa_decls$strec *  d;
    p = (byte *)aa_lib$bufferelemptr((struct aa_decls$dbuffer *)(*s).data,(i64)0);
    r = (struct aa_decls$relocrec *)(*s).relocs;
    L240 :;
    while (!!(r)) {
        d = (struct aa_decls$strec *)(*aa_decls$ss_symboltable)[((*r).stindex)-1];
                {i64 $temp = (*r).reloctype;
if (($temp==(i64)2) || ($temp==(i64)1)) {
            if (((i64)(*d).symbol == (i64)21)) {
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"BASERELOC/SKIP IMPORT",NULL);
                msysc$m_print_str((*d).name,NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
            }
            else {
                                {i64 $temp = (i64)(*d).segment;
if (($temp==(i64)3)) {
                    u = (struct aa_objdecls$sectionrec *)&aa_writeexe$sectiontable[((i64)3)-1];
                }
                else if (($temp==(i64)2)) {
                    u = (struct aa_objdecls$sectionrec *)&aa_writeexe$sectiontable[((i64)2)-1];
                }
                else if (($temp==(i64)1)) {
                    u = (struct aa_objdecls$sectionrec *)&aa_writeexe$sectiontable[((i64)1)-1];
                }
                };
                aa_writeexe$newbasereloc(((*u).virtoffset + (*r).offset),(*r).reloctype);
            }
;
        }
        };
        r = (struct aa_decls$relocrec *)(*r).nextreloc;
L241 :;
    }
L242 :;
    ;
}

static void aa_writeexe$writerecordx(void *r,i64 length) {
    memcpy(aa_writeexe$dataptr,r,(u64)length);
    aa_writeexe$dataptr += length;
}

static void aa_writeexe$writedosstub(void) {
        static byte stubdata[128] = {
    (u8)77u,
    (u8)90u,
    (u8)144u,
    (u8)0u,
    (u8)3u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)4u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)255u,
    (u8)255u,
    (u8)0u,
    (u8)0u,
    (u8)184u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)64u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)128u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)14u,
    (u8)31u,
    (u8)186u,
    (u8)14u,
    (u8)0u,
    (u8)180u,
    (u8)9u,
    (u8)205u,
    (u8)33u,
    (u8)184u,
    (u8)1u,
    (u8)76u,
    (u8)205u,
    (u8)33u,
    (u8)84u,
    (u8)104u,
    (u8)105u,
    (u8)115u,
    (u8)32u,
    (u8)112u,
    (u8)114u,
    (u8)111u,
    (u8)103u,
    (u8)114u,
    (u8)97u,
    (u8)109u,
    (u8)32u,
    (u8)99u,
    (u8)97u,
    (u8)110u,
    (u8)110u,
    (u8)111u,
    (u8)116u,
    (u8)32u,
    (u8)98u,
    (u8)101u,
    (u8)32u,
    (u8)114u,
    (u8)117u,
    (u8)110u,
    (u8)32u,
    (u8)105u,
    (u8)110u,
    (u8)32u,
    (u8)68u,
    (u8)79u,
    (u8)83u,
    (u8)32u,
    (u8)109u,
    (u8)111u,
    (u8)100u,
    (u8)101u,
    (u8)46u,
    (u8)13u,
    (u8)13u,
    (u8)10u,
    (u8)36u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u
};
    aa_writeexe$writerecordx(&stubdata,(i64)128);
}

static void aa_writeexe$writepesig(void) {
    (*(aa_writeexe$dataptr)++) = (i64)80;
    (*(aa_writeexe$dataptr)++) = (i64)69;
    (*(aa_writeexe$dataptr)++) = (i64)0;
    (*(aa_writeexe$dataptr)++) = (i64)0;
}

static void aa_writeexe$writepadding(i64 offset) {
    aa_writeexe$dataptr = (aa_writeexe$datastart + offset);
}

static void aa_writeexe$writefileheader(void) {
        struct aa_objdecls$imagefileheader header;
    memset(&header,(i32)(i64)0,(u64)20u);
    header.machine = (i64)34404;
    header.nsections = aa_writeexe$nsections;
    header.optheadersize = (i64)240;
    header.characteristics = (i64)559;
    if (!!(aa_writeexe$isdll)) {
        header.characteristics = (i64)8750;
    }
;
    aa_writeexe$writerecordx(&header,(i64)20);
}

static void aa_writeexe$writeoptheader(void) {
        struct aa_objdecls$optionalheader header;
    memset(&header,(i32)(i64)0,(u64)240u);
    header.magic = (i64)523;
    header.majorlv = (i64)1;
    header.minorlv = (i64)0;
    header.codesize = aa_writeexe$sectiontable[((i64)1)-1].rawsize;
    header.idatasize = (aa_writeexe$sectiontable[((i64)2)-1].rawsize + aa_writeexe$sectiontable[((i64)4)-1].rawsize);
    header.zdatasize = mlib$roundtoblock(aa_writeexe$sectiontable[((i64)3)-1].virtsize,(i64)512);
    if ((aa_writeexe$stentrypoint == 0)) {
        aa_writeexe$stentrypoint = (struct aa_decls$strec *)aa_writeexe$stentrypoint2;
        if ((aa_writeexe$stentrypoint == 0)) {
            aa_writeexe$stentrypoint = (struct aa_decls$strec *)aa_writeexe$stentrypoint3;
            if (!!(aa_writeexe$stentrypoint)) {
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"Using tertiary 'WinMain' entry point",NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
            }
;
        }
;
    }
;
    if ((aa_writeexe$stentrypoint == 0)) {
        if (!!(aa_writeexe$userentrypoint)) {
            msysc$m_print_startcon();
            msysc$m_print_str(aa_writeexe$userentrypoint,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            aa_lib$gerror((byte*)"User entry point not found");
        }
        else {
            if (!(!!(aa_writeexe$isdll))) {
                aa_lib$gerror((byte*)"Entry point not found: main or start");
            }
;
        }
;
    }
    else {
        header.entrypoint = (aa_writeexe$sectiontable[((i64)1)-1].virtoffset + (i64)(*aa_writeexe$stentrypoint).offset);
    }
;
    header.codebase = (i64)4096;
    header.imagebase = aa_writeexe$imagebase;
    header.sectionalignment = (i64)4096;
    header.filealignment = (i64)512;
    header.majorosv = (i64)4;
    header.minorosv = (i64)0;
    header.majorssv = (i64)5;
    header.minorssv = (i64)2;
    header.imagesize = aa_writeexe$imagesize;
    header.headerssize = aa_writeexe$sectiontable[((i64)1)-1].rawoffset;
    header.subsystem = (i64)3;
    header.stackreserve = (u64)4194304u;
    header.stackcommit = (u64)2097152u;
    header.heapreserve = (u64)1048576u;
    header.heapcommit = (u64)4096u;
    header.rvadims = (i64)16;
    header.importtable.virtualaddr = aa_writeexe$sectiontable[((i64)4)-1].virtoffset;
    header.importtable.size = ((aa_writeexe$sectiontable[((i64)4)-1].virtsize - aa_writeexe$exportdirvirtsize) - aa_writeexe$blockdirvirtsize);
    if (!!(aa_writeexe$isdll)) {
        header.dllcharacteristics = (i64)64;
        header.exporttable.virtualaddr = aa_writeexe$exportdirvirtaddr;
        header.exporttable.size = aa_writeexe$exportdirvirtsize;
        header.basereloctable.virtualaddr = aa_writeexe$blockdirvirtaddr;
        header.basereloctable.size = aa_writeexe$blockdirvirtsize;
    }
;
    header.iat.virtualaddr = aa_writeexe$fileiatoffset;
    header.iat.size = aa_writeexe$fileiatsize;
    aa_writeexe$writerecordx(&header,(i64)240);
}

static void aa_writeexe$writesectionheader(struct aa_objdecls$sectionrec *s) {
        struct aa_objdecls$imagesectionheader sheader;
    memset(&sheader,(i32)(i64)0,(u64)40u);
    strcpy((u8 *)&sheader.name[((i64)1)-1],(*s).name);
    sheader.virtual_size = (*s).virtsize;
    sheader.virtual_address = (*s).virtoffset;
    sheader.rawdata_offset = (*s).rawoffset;
    sheader.rawdata_size = (*s).rawsize;
        {i64 $temp = (*s).segtype;
if (($temp==(i64)3)) {
        sheader.characteristics = (i64)3226468480;
    }
    else if (($temp==(i64)2)) {
        sheader.characteristics = (i64)3226468416;
    }
    else if (($temp==(i64)1)) {
        sheader.characteristics = (i64)1615855648;
    }
    else if (($temp==(i64)5)) {
        sheader.characteristics = (i64)1076887616;
    }
    };
    aa_writeexe$writerecordx(&sheader,(i64)40);
}

static void aa_writeexe$writesectiondata(struct aa_objdecls$sectionrec *s) {
        {i64 $temp = (*s).segtype;
if (($temp==(i64)5)) {
        aa_writeexe$writerecordx((*s).bytedata,(*s).virtsize);
        if (((*s).rawsize > (*s).virtsize)) {
            aa_writeexe$dataptr += ((*s).rawsize - (*s).virtsize);
        }
;
    }
    else if (($temp==(i64)3)) {
    }
    else {
        aa_writeexe$writerecordx(aa_lib$bufferelemptr((struct aa_decls$dbuffer *)(*s).data,(i64)0),(*s).rawsize);
    }
    };
}

static void aa_writeexe$getoffsets(void) {
        i64 fileoffset;
        i64 imageoffset;
        i64 i;
        i64 diroffset;
        i64 impdirno;
        i64 hinttableoffset;
        i64 j;
        i64 codesize;
        i64 length;
        i64 thunkoffset;
        i64 offset;
        i64 dirstartoffset;
        byte *  pcode;
        byte *  pimpdir;
        struct aa_objdecls$importdirrec *  pdir;
        i64 *  paddr;
        i64 *  pname;
        i64 iatoffset;
        byte *  phint;
        u32 *  pextra;
        i64 xxx;
        i64 $av_1;
        byte *  thunkptr;
        byte *  codebase;
        i64 thunkaddr;
    fileoffset = (i64)392;
    fileoffset += ((i64)40 * aa_writeexe$nsections);
    fileoffset = mlib$roundtoblock(fileoffset,(i64)512);
    imageoffset = (i64)4096;
    codesize = aa_writeexe$sectiontable[((i64)1)-1].virtsize;
    pcode = (byte *)aa_lib$bufferelemptr((struct aa_decls$dbuffer *)aa_decls$ss_code,codesize);
    L243 :;
    while (!!((codesize & (i64)7))) {
        (*(pcode)++) = (i64)144;
        ++(codesize);
L244 :;
    }
L245 :;
    ;
    thunkoffset = codesize;
    codesize += (aa_writeexe$nimports * (i64)8);
    aa_writeexe$sectiontable[((i64)1)-1].virtsize = codesize;
    aa_writeexe$sectiontable[((i64)1)-1].rawsize = mlib$roundtoblock(codesize,(i64)512);
    aa_lib$buffercheck((struct aa_decls$dbuffer *)aa_decls$ss_code,((codesize - thunkoffset) + (i64)16));
    for (i=(i64)1;i<=aa_writeexe$nsections;++i) {
L246 :;
        if ((aa_writeexe$sectiontable[(i)-1].segtype != (i64)3)) {
            aa_writeexe$sectiontable[(i)-1].rawoffset = fileoffset;
        }
;
        if ((aa_writeexe$sectiontable[(i)-1].segtype != (i64)3)) {
            fileoffset = mlib$roundtoblock((fileoffset + aa_writeexe$sectiontable[(i)-1].virtsize),(i64)512);
        }
;
        aa_writeexe$sectiontable[(i)-1].virtoffset = imageoffset;
        if ((aa_writeexe$sectiontable[(i)-1].segtype == (i64)5)) {
            diroffset = imageoffset;
            impdirno = i;
        }
;
        imageoffset = mlib$roundtoblock((imageoffset + aa_writeexe$sectiontable[(i)-1].virtsize),(i64)4096);
L247 :;
    }
L248 :;
    ;
    if (!!(aa_writeexe$isdll)) {
        aa_writeexe$getbaserelocs((struct aa_objdecls$sectionrec *)&aa_writeexe$sectiontable[((i64)1)-1]);
        aa_writeexe$getbaserelocs((struct aa_objdecls$sectionrec *)&aa_writeexe$sectiontable[((i64)2)-1]);
    }
;
    diroffset += ((aa_writeexe$ndlls + (i64)1) * (i64)20);
    for (i=(i64)1;i<=aa_writeexe$ndlls;++i) {
L249 :;
        aa_writeexe$dlltable[(i)-1].nametableoffset = diroffset;
        diroffset += ((aa_writeexe$dlltable[(i)-1].nprocs + (i64)1) * (i64)8);
L250 :;
    }
L251 :;
    ;
    aa_writeexe$fileiatoffset = diroffset;
    for (i=(i64)1;i<=aa_writeexe$ndlls;++i) {
L252 :;
        aa_writeexe$dlltable[(i)-1].addrtableoffset = diroffset;
        diroffset += ((aa_writeexe$dlltable[(i)-1].nprocs + (i64)1) * (i64)8);
L253 :;
    }
L254 :;
    ;
    aa_writeexe$fileiatsize = (diroffset - aa_writeexe$fileiatoffset);
    hinttableoffset = diroffset;
    for (i=(i64)1;i<=aa_writeexe$nimports;++i) {
L255 :;
        length = (strlen(aa_writeexe$importtable[(i)-1].name) + (i64)3);
        if (!!((length & (i64)1))) {
            ++(length);
        }
;
        aa_writeexe$importtable[(i)-1].hintnameoffset = diroffset;
        diroffset += length;
L256 :;
    }
L257 :;
    ;
    diroffset = mlib$roundtoblock(diroffset,(i64)4);
    for (i=(i64)1;i<=aa_writeexe$ndlls;++i) {
L258 :;
        length = (strlen(aa_writeexe$dlltable[(i)-1].name) + (i64)1);
        if (!!(msysc$m_getdotindex(length,(i64)0))) {
            ++(length);
        }
;
        aa_writeexe$dlltable[(i)-1].dllextraoffset = diroffset;
        diroffset += (aa_writeexe$dlltable[(i)-1].nprocs * (i64)4);
        aa_writeexe$dlltable[(i)-1].dllnameoffset = diroffset;
        diroffset += length;
L259 :;
    }
L260 :;
    ;
    dirstartoffset = aa_writeexe$sectiontable[(impdirno)-1].virtoffset;
    if (!!(aa_writeexe$isdll)) {
        aa_writeexe$exportdirvirtaddr = diroffset;
        aa_writeexe$exportdiroffset = (diroffset - dirstartoffset);
        aa_writeexe$exportdirvirtsize = aa_writeexe$getexporttablesize();
        diroffset += aa_writeexe$exportdirvirtsize;
        aa_writeexe$scanbaserelocs();
        aa_writeexe$blockdirvirtaddr = diroffset;
        aa_writeexe$blockdiroffset = (diroffset - dirstartoffset);
        aa_writeexe$blockdirvirtsize = aa_writeexe$basetablesize;
        diroffset += aa_writeexe$blockdirvirtsize;
    }
;
    offset = (diroffset - dirstartoffset);
    aa_writeexe$sectiontable[(impdirno)-1].virtsize = offset;
    aa_writeexe$sectiontable[(impdirno)-1].rawsize = mlib$roundtoblock(offset,(i64)512);
    aa_writeexe$filesize = mlib$roundtoblock((fileoffset + offset),(i64)512);
    aa_writeexe$imagesize = mlib$roundtoblock((imageoffset + (diroffset - dirstartoffset)),(i64)4096);
    pimpdir = (aa_writeexe$sectiontable[(impdirno)-1].bytedata = (byte *)mlib$pcm_allocz(offset));
    pdir = (struct aa_objdecls$importdirrec *)pimpdir;
    for (i=(i64)1;i<=aa_writeexe$ndlls;++i) {
L261 :;
        (*pdir).implookuprva = aa_writeexe$dlltable[(i)-1].nametableoffset;
        (*pdir).impaddressrva = aa_writeexe$dlltable[(i)-1].addrtableoffset;
        (*pdir).namerva = aa_writeexe$dlltable[(i)-1].dllnameoffset;
        ++(pdir);
        iatoffset = aa_writeexe$dlltable[(i)-1].addrtableoffset;
        paddr = (i64 *)((pimpdir + iatoffset) - dirstartoffset);
        pname = (i64 *)((pimpdir + aa_writeexe$dlltable[(i)-1].nametableoffset) - dirstartoffset);
        for (j=(i64)1;j<=aa_writeexe$nimports;++j) {
L264 :;
            if ((aa_writeexe$importtable[(j)-1].libno == i)) {
                (*pname) = ((*paddr) = aa_writeexe$importtable[(j)-1].hintnameoffset);
                aa_writeexe$importtable[(j)-1].iatoffset = iatoffset;
                iatoffset += (i64)8;
                ++(pname);
                ++(paddr);
            }
;
L265 :;
        }
L266 :;
        ;
L262 :;
    }
L263 :;
    ;
    for (i=(i64)1;i<=aa_writeexe$nimports;++i) {
L267 :;
        phint = ((pimpdir + aa_writeexe$importtable[(i)-1].hintnameoffset) - dirstartoffset);
        phint += (i64)2;
        strcpy((u8 *)phint,aa_writeexe$importtable[(i)-1].name);
L268 :;
    }
L269 :;
    ;
    xxx = dirstartoffset;
    for (i=(i64)1;i<=aa_writeexe$ndlls;++i) {
L270 :;
        pextra = (u32 *)((pimpdir + aa_writeexe$dlltable[(i)-1].dllextraoffset) - dirstartoffset);
                ($av_1 = aa_writeexe$dlltable[(i)-1].nprocs);
        for (j=(i64)1;j<=$av_1;++j) {
L273 :;
            (*pextra) = xxx;
            ++(pextra);
L274 :;
        }
L275 :;
        ;
        xxx += (i64)20;
        phint = ((pimpdir + aa_writeexe$dlltable[(i)-1].dllnameoffset) - dirstartoffset);
        strcpy((u8 *)phint,aa_writeexe$dlltable[(i)-1].name);
L271 :;
    }
L272 :;
    ;
    if (!!(aa_writeexe$isdll)) {
        aa_writeexe$writeexporttable((pimpdir + aa_writeexe$exportdiroffset));
        aa_writeexe$writebasereloctable((pimpdir + aa_writeexe$blockdiroffset));
    }
;
    thunkptr = (byte *)aa_lib$bufferelemptr((struct aa_decls$dbuffer *)aa_decls$ss_code,thunkoffset);
    codebase = (byte *)aa_lib$bufferelemptr((struct aa_decls$dbuffer *)aa_decls$ss_code,(i64)0);
    for (i=(i64)1;i<=aa_writeexe$nimports;++i) {
L276 :;
        aa_writeexe$importtable[(i)-1].thunkoffset = (thunkptr - codebase);
        (*(thunkptr)++) = (i64)72;
        (*(thunkptr)++) = (i64)255;
        (*(thunkptr)++) = (i64)36;
        (*(thunkptr)++) = (i64)37;
        thunkaddr = ((i64)aa_writeexe$imagebase + aa_writeexe$importtable[(i)-1].iatoffset);
        (*(i32 *)thunkptr) = thunkaddr;
        thunkptr += (i64)4;
L277 :;
    }
L278 :;
    ;
}

static i64 aa_writeexe$getsectionno(i64 segment) {
    if ((segment==(i64)3)) {
        return (i64)3;
    }
    else if ((segment==(i64)2)) {
        return (i64)2;
    }
    else if ((segment==(i64)1)) {
        return (i64)1;
    }
    else {
        aa_lib$gerror((byte*)"GSN");
        return (i64)0;
    }
;
}

static void aa_writeexe$writeexporttable(byte *pstart) {
        i64 sortindex[3000];
        struct aa_objdecls$exportdirrec *  phdr;
        u32 *  paddrtable;
        u32 *  pnametable;
        u16 *  pordtable;
        u8 *  pdllname;
        u8 *  pnames;
        i64 addrtableoffset;
        i64 nametableoffset;
        i64 ordtableoffset;
        i64 dllnameoffset;
        i64 namesoffset;
        i64 virtoffset;
        i64 sectionno;
        struct aa_decls$strec *  d;
        i64 i;
    phdr = (struct aa_objdecls$exportdirrec *)pstart;
    (*phdr).timedatestamp = (i64)1602876664;
    (*phdr).ordinalbase = (i64)1;
    (*phdr).naddrtable = aa_writeexe$nexports;
    (*phdr).nnamepointers = aa_writeexe$nexports;
    addrtableoffset = (i64)40;
    nametableoffset = (addrtableoffset + (aa_writeexe$nexports * (i64)4));
    ordtableoffset = (nametableoffset + (aa_writeexe$nexports * (i64)4));
    dllnameoffset = (ordtableoffset + (aa_writeexe$nexports * (i64)2));
    namesoffset = ((dllnameoffset + strlen(aa_writeexe$dllfilename)) + (i64)1);
    virtoffset = (aa_writeexe$sectiontable[((i64)4)-1].virtoffset + aa_writeexe$exportdiroffset);
    paddrtable = (u32 *)(pstart + addrtableoffset);
    pnametable = (u32 *)(pstart + nametableoffset);
    pordtable = (u16 *)(pstart + ordtableoffset);
    pdllname = (u8 *)(pstart + dllnameoffset);
    pnames = (u8 *)(pstart + namesoffset);
    (*phdr).namerva = (dllnameoffset + virtoffset);
    (*phdr).expaddressrva = (addrtableoffset + virtoffset);
    (*phdr).namepointerrva = (nametableoffset + virtoffset);
    (*phdr).ordtablerva = (ordtableoffset + virtoffset);
    strcpy(pdllname,aa_writeexe$dllfilename);
    if ((aa_writeexe$nexports > (i64)3000)) {
        aa_lib$gerror((byte*)"Too many exports - can't sort");
    }
;
    aa_writeexe$sortexports(&sortindex);
    for (i=(i64)1;i<=aa_writeexe$nexports;++i) {
L279 :;
        d = (struct aa_decls$strec *)aa_writeexe$exporttable[(sortindex[(i)-1])-1].def;
        sectionno = aa_writeexe$getsectionno((i64)(*d).segment);
        strcpy(pnames,(*d).name);
        (*pnametable) = (namesoffset + virtoffset);
        ++(pnametable);
        namesoffset += (strlen((*d).name) + (i64)1);
        pnames += (strlen((*d).name) + (i64)1);
        (*paddrtable) = ((i64)(*d).offset + aa_writeexe$sectiontable[(sectionno)-1].virtoffset);
        ++(paddrtable);
        (*pordtable) = (i - (i64)1);
        ++(pordtable);
L280 :;
    }
L281 :;
    ;
}

static i64 aa_writeexe$getexporttablesize(void) {
        i64 size;
        i64 i;
    size = (i64)40;
    size += (aa_writeexe$nexports * (i64)4);
    size += (aa_writeexe$nexports * (i64)4);
    size += (aa_writeexe$nexports * (i64)2);
    size += (strlen(aa_writeexe$dllfilename) + (i64)1);
    for (i=(i64)1;i<=aa_writeexe$nexports;++i) {
L282 :;
        size += (strlen((*aa_writeexe$exporttable[(i)-1].def).name) + (i64)1);
L283 :;
    }
L284 :;
    ;
    return size;
}

static void aa_writeexe$newbasereloc(i64 addr,i64 reltype) {
        struct aa_writeexe$basereloc *  p;
    p = (struct aa_writeexe$basereloc *)mlib$pcm_allocz((i64)16);
    (*p).address = addr;
    (*p).reloctype = reltype;
    (*p).nextitem = (struct aa_writeexe$basereloc *)aa_writeexe$basereloclist;
    aa_writeexe$basereloclist = (struct aa_writeexe$basereloc *)p;
    ++(aa_writeexe$nbaserelocs);
    aa_writeexe$maxrelocaddr=(aa_writeexe$maxrelocaddr>addr?aa_writeexe$maxrelocaddr:addr);
;
}

static void aa_writeexe$scanbaserelocs(void) {
        i64 baseaddr;
        i64 addr;
        i64 nextblock;
        struct aa_writeexe$basereloc *  p;
        i64 i;
    baseaddr = (i64)4096;
    aa_writeexe$nbaseblocks = (i64)0;
    L285 :;
    do {
        nextblock = (baseaddr + (i64)4096);
        if ((aa_writeexe$nbaseblocks >= (i64)500)) {
            aa_lib$gerror((byte*)"Too many blocks");
        }
;
        ++(aa_writeexe$nbaseblocks);
        aa_writeexe$blockbases[(aa_writeexe$nbaseblocks)-1] = baseaddr;
        aa_writeexe$blockcounts[(aa_writeexe$nbaseblocks)-1] = (i64)0;
        p = (struct aa_writeexe$basereloc *)aa_writeexe$basereloclist;
        L288 :;
        while (!!(p)) {
            addr = (i64)(*p).address;
            if (((addr >= baseaddr) && (addr < nextblock))) {
                ++(aa_writeexe$blockcounts[(aa_writeexe$nbaseblocks)-1]);
            }
;
            p = (struct aa_writeexe$basereloc *)(*p).nextitem;
L289 :;
        }
L290 :;
        ;
        baseaddr = nextblock;
L286 :;
    }
    while (!(baseaddr > aa_writeexe$maxrelocaddr));
L287 :;
    ;
    for (i=(i64)1;i<=aa_writeexe$nbaseblocks;++i) {
L291 :;
        if (!!((i64)aa_writeexe$blockcounts[(i)-1])) {
            if (!!(((i64)aa_writeexe$blockcounts[(i)-1] & (i64)1))) {
                ++(aa_writeexe$blockcounts[(i)-1]);
                ++(aa_writeexe$blockpadding[(i)-1]);
            }
;
            aa_writeexe$blockbytes[(i)-1] = (((i64)aa_writeexe$blockcounts[(i)-1] * (i64)2) + (i64)8);
            aa_writeexe$basetablesize += (i64)aa_writeexe$blockbytes[(i)-1];
        }
;
L292 :;
    }
L293 :;
    ;
}

static void aa_writeexe$writebasereloctable(byte *pstart) {
        u32 *  p32;
        u16 *  p16;
        i64 baseaddr;
        i64 addr;
        i64 nextblock;
        struct aa_writeexe$basereloc *  q;
        i64 i;
    p32 = (u32 *)pstart;
    for (i=(i64)1;i<=aa_writeexe$nbaseblocks;++i) {
L294 :;
        if (!!((i64)aa_writeexe$blockcounts[(i)-1])) {
            (*p32) = aa_writeexe$blockbases[(i)-1];
            ++(p32);
            (*p32) = (i64)aa_writeexe$blockbytes[(i)-1];
            ++(p32);
            p16 = (u16 *)p32;
            q = (struct aa_writeexe$basereloc *)aa_writeexe$basereloclist;
            baseaddr = aa_writeexe$blockbases[(i)-1];
            nextblock = (baseaddr + (i64)4096);
            L297 :;
            while (!!(q)) {
                addr = (i64)(*q).address;
                if (((addr >= baseaddr) && (addr < nextblock))) {
                    (*p16) = ((addr - baseaddr) + ((((i64)(*q).reloctype == (i64)2) ? (i64)3 : (i64)10) << (i64)12));
                    ++(p16);
                }
;
                q = (struct aa_writeexe$basereloc *)(*q).nextitem;
L298 :;
            }
L299 :;
            ;
            if (!!((i64)aa_writeexe$blockpadding[(i)-1])) {
                (*(p16)++) = (i64)0;
            }
;
            p32 = (u32 *)p16;
        }
;
L295 :;
    }
L296 :;
    ;
}

static void aa_writeexe$sortexports(i64 (*sortindex)[]) {
        struct aa_decls$strec *  d;
        struct aa_decls$strec *  e;
        i64 swapped;
        i64 $av_1;
        i64 i;
    for (i=(i64)1;i<=aa_writeexe$nexports;++i) {
L300 :;
        (*sortindex)[(i)-1] = i;
L301 :;
    }
L302 :;
    ;
    L303 :;
    do {
        swapped = (i64)0;
                ($av_1 = (aa_writeexe$nexports - (i64)1));
        for (i=(i64)1;i<=$av_1;++i) {
L306 :;
            d = (struct aa_decls$strec *)aa_writeexe$exporttable[((*sortindex)[(i)-1])-1].def;
            e = (struct aa_decls$strec *)aa_writeexe$exporttable[((*sortindex)[((i + (i64)1))-1])-1].def;
            if ((strcmp((*d).name,(*e).name) > (i64)0)) {
                swapped = (i64)1;
                {i64 temp = (*sortindex)[(i)-1]; (*sortindex)[(i)-1] = (*sortindex)[((i + (i64)1))-1]; (*sortindex)[((i + (i64)1))-1] = temp; };
            }
;
L307 :;
        }
L308 :;
        ;
L304 :;
    }
    while (!!(!!(swapped)));
L305 :;
    ;
}

// START
void aa_writeexe$start(void) {

}

struct mlib$strbuffer *aa_writess$writessdata(i64 fexe) {
    mlib$gs_init((struct mlib$strbuffer *)aa_lib$dest);
    aa_writess$showssdata(fexe);
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    return (struct mlib$strbuffer *)aa_lib$dest;
}

static void aa_writess$showssdata(i64 fexe) {
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(!!(fexe) ? (byte*)"EXE FORMAT" : (byte*)"AFTER GENSS"));
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"proc Section Zdata: ");
    mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,aa_decls$ss_zdatalen);
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    aa_writess$showsectioncode((struct aa_objdecls$sectionrec *)&aa_writeexe$sectiontable[((i64)1)-1]);
    if (!!(fexe)) {
        aa_writess$showsectiondata((struct aa_objdecls$sectionrec *)&aa_writeexe$sectiontable[((i64)4)-1]);
    }
;
    aa_writess$showsymboltable2();
    aa_writess$showimporttable();
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"END OF GENSS");
}

static void aa_writess$showsectiondata(struct aa_objdecls$sectionrec *d) {
        i64 i;
        i64 k;
        i64 length;
        i64 bb;
        u8 str[128];
        u8 str2[128];
        byte *  p;
        byte *  baseaddr;
        i64 $av_1;
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"proc Section ");
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(*d).name);
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)" Size:");
    mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,(*d).virtsize);
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    k = (i64)0;
    if (((*d).segtype != (i64)5)) {
        p = (byte *)aa_lib$bufferelemptr((struct aa_decls$dbuffer *)(*d).data,(i64)0);
    }
    else {
        p = (*d).bytedata;
    }
;
    length = (*d).virtsize;
    str[((i64)1)-1] = (u64)0u;
    baseaddr = (byte *)((i64)aa_writeexe$imagebase + (*d).virtoffset);
    msysc$m_print_startstr(str2);
    msysc$m_print_ptr(baseaddr,(byte*)"Z8H");
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)": ",NULL);
    msysc$m_print_end();
    ;
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str2);
    for (i=(i64)1;i<=length;++i) {
L309 :;
        bb = (i64)(*(p)++);
        msysc$m_print_startstr(str2);
        msysc$m_print_i64(bb,(byte*)"z2H");
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)" ",NULL);
        msysc$m_print_end();
        ;
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str2);
        if (((i64)32<=bb && bb<=(i64)127)) {
            str2[((i64)1)-1] = (u64)bb;
            str2[((i64)2)-1] = (u64)0u;
            strcat((u8 *)str,(u8 *)str2);
        }
        else {
            strcat((u8 *)str,(byte*)".");
        }
;
        if (((++(k) == (i64)16) || (i == length))) {
            if ((k < (i64)16)) {
                $av_1 = ((i64)16 - k);
                while ($av_1-- > 0) {
L312 :;
                    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"   ");
                    strcat((u8 *)str,(byte*)" ");
L313 :;
                }
L314 :;
                ;
            }
;
            mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"\t[");
            mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str);
            mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"]");
            k = (i64)0;
            str[((i64)1)-1] = (u64)0u;
            baseaddr += (i64)16;
            msysc$m_print_startstr(str2);
            msysc$m_print_ptr(baseaddr,(byte*)"z8h");
            msysc$m_print_nogap();
            msysc$m_print_str((byte*)": ",NULL);
            msysc$m_print_end();
            ;
            mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str2);
        }
;
L310 :;
    }
L311 :;
    ;
    if ((k == (i64)0)) {
        mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    }
;
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    if (!!(k)) {
        mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    }
;
}

static void aa_writess$showsectioncode(struct aa_objdecls$sectionrec *p) {
        byte *  codeptr;
        byte *  codeend;
        byte *  codestart;
        i64 length;
        i64 offset;
        u8 *  s;
        u8 str[16];
        byte *  baseaddr;
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"proc Section Code");
    length = (*p).virtsize;
    codestart = (codeptr = (byte *)aa_lib$bufferelemptr((struct aa_decls$dbuffer *)(*p).data,(i64)0));
    codeend = (codeptr + length);
    baseaddr = (byte *)((i64)aa_writeexe$imagebase + (*p).virtoffset);
    L315 :;
    while ((codeptr < codeend)) {
        offset = (codeptr - codestart);
        s = aa_disasm$decodeinstr(&codeptr,(baseaddr + offset));
        if ((s == 0)) {
            goto L317 ;
        }
;
        msysc$m_print_startstr(str);
        msysc$m_print_i64(offset,(byte*)"4");
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)" ",NULL);
        msysc$m_print_end();
        ;
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str);
        mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,s);
L316 :;
    }
L317 :;
    ;
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
}

static void aa_writess$showsectionrelocs2(u8 *caption,struct aa_decls$relocrec *relocs,i64 nrelocs) {
        struct aa_decls$relocrec *  r;
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"proc Section Relocs: ");
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,caption);
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)" ");
    mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,nrelocs);
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    r = (struct aa_decls$relocrec *)relocs;
    L318 :;
    while (!!(r)) {
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"Reloc: ");
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,aa_objdecls$relocnames[((*r).reloctype)]);
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)" Offset: ");
        mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,(*r).offset);
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)" ST Index: ");
        mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,(*r).stindex);
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)" ");
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(*(*aa_decls$ss_symboltable)[((*r).stindex)-1]).name);
        mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
        r = (struct aa_decls$relocrec *)(*r).nextreloc;
L319 :;
    }
L320 :;
    ;
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
}

static void aa_writess$gs_value(u8 *caption,i64 value) {
        u8 str[256];
    strcpy((u8 *)str,caption);
    strcat((u8 *)str,(byte*)":");
    mlib$ipadstr((u8 *)str,(i64)20,(byte*)" ");
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str);
    msysc$m_print_startstr(str);
    msysc$m_print_setfmt((byte*)"0x# #");
    msysc$m_print_i64(value,(byte*)"H");
    msysc$m_print_i64(value,NULL);
    msysc$m_print_end();
    ;
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str);
}

static void aa_writess$showsymboltable2(void) {
        i64 i;
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"Proc Symbol Table");
    for (i=(i64)1;i<=aa_decls$ss_nsymbols;++i) {
L321 :;
        mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,i);
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)": ");
        mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(*(*aa_decls$ss_symboltable)[(i)-1]).name);
L322 :;
    }
L323 :;
    ;
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
}

static void aa_writess$showimporttable(void) {
        u8 str[256];
        struct aa_objdecls$importrec p;
        i64 i;
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"Proc Dll List");
    for (i=(i64)1;i<=aa_writeexe$ndlls;++i) {
L324 :;
        mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,i);
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)": ");
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,aa_writeexe$dlltable[(i)-1].name);
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)" ");
        mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,aa_writeexe$dlltable[(i)-1].nprocs);
        mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
        aa_writess$gs_value((byte*)"\t\tName Table Offset",aa_writeexe$dlltable[(i)-1].nametableoffset);
        aa_writess$gs_value((byte*)"\t\tAddr Table Offset",aa_writeexe$dlltable[(i)-1].addrtableoffset);
        aa_writess$gs_value((byte*)"\t\tDLL Name Offset  ",aa_writeexe$dlltable[(i)-1].dllnameoffset);
L325 :;
    }
L326 :;
    ;
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"Proc Import List");
    for (i=(i64)1;i<=aa_writeexe$nimports;++i) {
L327 :;
        p = aa_writeexe$importtable[(i)-1];
        mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,i);
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)": ");
        if (!!(p.libno)) {
            strcpy((u8 *)str,p.name);
            mlib$ipadstr((u8 *)str,(i64)16,(byte*)" ");
            mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str);
            mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)" (");
            mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,aa_writeexe$dlltable[(p.libno)-1].name);
            mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)")");
            aa_writess$gs_value((byte*)"\tIAT Offset        ",p.iatoffset);
            aa_writess$gs_value((byte*)"\tThunk Offset      ",p.thunkoffset);
            aa_writess$gs_value((byte*)"\tHint/Name Offset  ",p.hintnameoffset);
        }
        else {
            strcpy((u8 *)str,p.name);
            mlib$ipadstr((u8 *)str,(i64)20,(byte*)" ");
            mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str);
            mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)" (---)");
        }
;
L328 :;
    }
L329 :;
    ;
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
}

static void aa_writess$showsections(void) {
        struct aa_objdecls$sectionrec s;
        i64 i;
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"proc Section Headersxxx");
    mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
    for (i=(i64)1;i<=aa_writeexe$nsections;++i) {
L330 :;
        s = aa_writeexe$sectiontable[(i)-1];
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"Section ");
        mlib$gs_strint((struct mlib$strbuffer *)aa_lib$dest,i);
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)": ");
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,s.name);
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(byte*)"  (");
        mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,aa_mcxdecls$segmentnames[(s.segtype)-1]);
        mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)")");
        aa_writess$gs_value((byte*)"    Raw Offset",s.rawoffset);
        aa_writess$gs_value((byte*)"    Raw Size",s.rawsize);
        aa_writess$gs_value((byte*)"    Virtual Offset",s.virtoffset);
        aa_writess$gs_value((byte*)"    Virtual Size",s.virtsize);
        aa_writess$gs_value((byte*)"    Nrelocs",s.nrelocs);
        aa_writess$gs_value((byte*)"    Data",(i64)s.data);
        mlib$gs_line((struct mlib$strbuffer *)aa_lib$dest);
L331 :;
    }
L332 :;
    ;
}

// START
void aa_writess$start(void) {

}

u8 *aa_disasm$decodeinstr(byte **cptr,byte *baseaddr) {
        i64 n;
        i64 w;
        i64 opc;
        i64 reg;
        i64 op;
        byte *  pstart;
        static u8 str[256];
        u8 str2[128];
        i64 $av_1;
        i64 $av_2;
    aa_disasm$deststr[((i64)1)-1] = (u64)0u;
    pstart = (aa_disasm$codeptr = (*cptr));
    aa_disasm$rex = (i64)0;
    aa_disasm$opsize = (i64)1;
    aa_disasm$f2override = (aa_disasm$f3override = (aa_disasm$sizeoverride = (aa_disasm$addroverride = (i64)0)));
    aa_disasm$basereg = (aa_disasm$indexreg = (aa_disasm$offset = (i64)0));
    //retry:
L333 :;
;
    switch ((opc = (i64)(*(aa_disasm$codeptr)++))) {
    case 0:;
    case 1:;
    case 8:;
    case 9:;
    case 16:;
    case 17:;
    case 24:;
    case 25:;
    case 32:;
    case 33:;
    case 40:;
    case 41:;
    case 48:;
    case 49:;
    case 56:;
    case 57:;
        {
            op = (opc >> (i64)3);
            aa_disasm$decodeaddr((opc & (i64)1));
            aa_disasm$getsilx(&aa_disasm$basereg);
            aa_disasm$getsil(&aa_disasm$rmreg);
            aa_disasm$genstr(aa_disasm$opnames[(op)]);
            aa_disasm$printaddrmode((i64)0);
            aa_disasm$genstr((byte*)", ");
            aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,aa_disasm$opsize));
        }
        break;
    case 2:;
    case 3:;
    case 10:;
    case 11:;
    case 18:;
    case 19:;
    case 26:;
    case 27:;
    case 34:;
    case 35:;
    case 42:;
    case 43:;
    case 50:;
    case 51:;
    case 58:;
    case 59:;
        {
            op = (opc >> (i64)3);
            aa_disasm$decodeaddr((opc & (i64)1));
            aa_disasm$genstr(aa_disasm$opnames[(op)]);
            aa_disasm$genstr((byte*)" ");
            aa_disasm$getsil(&aa_disasm$rmreg);
            aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,aa_disasm$opsize));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$printaddrmode((i64)0);
        }
        break;
    case 4:;
    case 5:;
    case 12:;
    case 13:;
    case 20:;
    case 21:;
    case 28:;
    case 29:;
    case 36:;
    case 37:;
    case 44:;
    case 45:;
    case 52:;
    case 53:;
    case 60:;
    case 61:;
        {
            aa_disasm$genstr(aa_disasm$opnames[((opc >> (i64)3))]);
            aa_disasm$genstr((byte*)" ");
            if (!!((opc & (i64)1))) {
                aa_disasm$opsize = (i64)4;
                if (!!(aa_disasm$sizeoverride)) {
                    aa_disasm$opsize = (i64)2;
                }
;
                if (!!((aa_disasm$rex & (i64)8))) {
                    aa_disasm$opsize = (i64)8;
                }
;
            }
;
            aa_disasm$genstr(aa_disasm$strreg((i64)1,aa_disasm$opsize));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$genintd(aa_disasm$readimm());
        }
        break;
    case 15:;
        {
            aa_disasm$decodetwobyteinstr();
        }
        break;
    case 64:;
    case 65:;
    case 66:;
    case 67:;
    case 68:;
    case 69:;
    case 70:;
    case 71:;
    case 72:;
    case 73:;
    case 74:;
    case 75:;
    case 76:;
    case 77:;
    case 78:;
    case 79:;
        {
            aa_disasm$rex = opc;
            goto L333 ;
;
        }
        break;
    case 80:;
    case 81:;
    case 82:;
    case 83:;
    case 84:;
    case 85:;
    case 86:;
    case 87:;
        {
            reg = aa_disasm$getreg((opc & (i64)7),(aa_disasm$rex & (i64)1));
            aa_disasm$genstr((byte*)"push ");
            aa_disasm$genstr(aa_disasm$strreg(reg,(i64)8));
        }
        break;
    case 88:;
    case 89:;
    case 90:;
    case 91:;
    case 92:;
    case 93:;
    case 94:;
    case 95:;
        {
            reg = aa_disasm$getreg((opc & (i64)7),(aa_disasm$rex & (i64)1));
            aa_disasm$genstr((byte*)"pop ");
            aa_disasm$genstr(aa_disasm$strreg(reg,(i64)8));
        }
        break;
    case 99:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$genstr((byte*)"movsxd ");
            aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,aa_disasm$opsize));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$opsize = (i64)4;
            aa_disasm$printaddrmode((i64)0);
        }
        break;
    case 102:;
        {
            aa_disasm$sizeoverride = (i64)1;
            goto L333 ;
;
        }
        break;
    case 103:;
        {
            aa_disasm$addroverride = (i64)1;
            goto L333 ;
;
        }
        break;
    case 104:;
        {
            aa_disasm$genstr((byte*)"push ");
            if (!!(aa_disasm$sizeoverride)) {
                aa_disasm$genintd(aa_disasm$readint16());
            }
            else {
                aa_disasm$genintd(aa_disasm$readint32());
            }
;
        }
        break;
    case 106:;
        {
            aa_disasm$genstr((byte*)"push ");
            aa_disasm$genintd(aa_disasm$readsbyte());
        }
        break;
    case 105:;
    case 107:;
        {
            aa_disasm$decodeaddr((i64)1);
            if ((aa_disasm$basereg != aa_disasm$rmreg)) {
                aa_disasm$genstr((byte*)"imul3");
                aa_disasm$genstr((byte*)" ");
                aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,aa_disasm$opsize));
                aa_disasm$genstr((byte*)", ");
            }
            else {
                aa_disasm$genstr((byte*)"imul2");
            }
;
            aa_disasm$printaddrmode((i64)0);
            aa_disasm$genstr((byte*)", ");
            aa_disasm$opsize = (!!((opc & (i64)2)) ? (i64)1 : aa_disasm$opsize);
            aa_disasm$genintd(aa_disasm$readimm());
        }
        break;
    case 112:;
    case 113:;
    case 114:;
    case 115:;
    case 116:;
    case 117:;
    case 118:;
    case 119:;
    case 120:;
    case 121:;
    case 122:;
    case 123:;
    case 124:;
    case 125:;
    case 126:;
    case 127:;
        {
            aa_disasm$genstr((byte*)"j");
            aa_disasm$genstr(aa_disasm$condnames[((opc & (i64)15))]);
            aa_disasm$genstr((byte*)" ");
            aa_disasm$genintd(aa_disasm$readsbyte());
        }
        break;
    case 128:;
    case 129:;
    case 130:;
    case 131:;
        {
            aa_disasm$decodeaddr((opc & (i64)1));
            aa_disasm$genstr(aa_disasm$opnames[(aa_disasm$rmopc)]);
            aa_disasm$getsilx(&aa_disasm$basereg);
            aa_disasm$printaddrmode((i64)0);
            aa_disasm$genstr((byte*)", ");
            if ((opc != (i64)131)) {
                aa_disasm$genintd(aa_disasm$readimm());
            }
            else {
                aa_disasm$genintd(aa_disasm$readsbyte());
            }
;
        }
        break;
    case 132:;
    case 133:;
        {
            aa_disasm$decodeaddr((opc & (i64)1));
            aa_disasm$getsilx(&aa_disasm$basereg);
            aa_disasm$getsil(&aa_disasm$rmreg);
            aa_disasm$genstr((byte*)"test ");
            aa_disasm$printaddrmode((i64)0);
            aa_disasm$genstr((byte*)", ");
            aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,aa_disasm$opsize));
        }
        break;
    case 134:;
    case 135:;
        {
            aa_disasm$decodeaddr((opc & (i64)1));
            aa_disasm$genstr((byte*)"exch2 ");
            aa_disasm$getsilx(&aa_disasm$basereg);
            aa_disasm$getsil(&aa_disasm$rmreg);
            aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,aa_disasm$opsize));
            aa_disasm$genstr((byte*)",");
            aa_disasm$printaddrmode((i64)0);
        }
        break;
    case 136:;
    case 137:;
        {
            aa_disasm$decodeaddr((opc & (i64)1));
            aa_disasm$genstr((byte*)"mov");
            aa_disasm$getsilx(&aa_disasm$basereg);
            aa_disasm$getsil(&aa_disasm$rmreg);
            aa_disasm$printaddrmode((i64)0);
            aa_disasm$genstr((byte*)", ");
            aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,aa_disasm$opsize));
        }
        break;
    case 138:;
    case 139:;
        {
            aa_disasm$decodeaddr((opc & (i64)1));
            aa_disasm$genstr((byte*)"mov ");
            aa_disasm$getsilx(&aa_disasm$basereg);
            aa_disasm$getsil(&aa_disasm$rmreg);
            aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,aa_disasm$opsize));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$printaddrmode((i64)0);
        }
        break;
    case 141:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$genstr((byte*)"lea ");
            aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,aa_disasm$opsize));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$printaddrmode((i64)0);
        }
        break;
    case 143:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$opsize = (i64)1;
            aa_disasm$genstr((byte*)"pop");
            aa_disasm$printaddrmode((i64)0);
        }
        break;
    case 144:;
        {
            if (!!(aa_disasm$rex)) {
                goto L334 ;
;
            }
;
            aa_disasm$genstr((byte*)"nop");
        }
        break;
    case 145:;
    case 146:;
    case 147:;
    case 148:;
    case 149:;
    case 150:;
    case 151:;
        {
            //doexch:
L334 :;
;
            reg = ((opc & (i64)7) + (i64)1);
            if (!!((aa_disasm$rex & (i64)1))) {
                reg += (i64)8;
            }
;
            aa_disasm$opsize = (!!(aa_disasm$sizeoverride) ? (i64)2 : (i64)4);
            if (!!((aa_disasm$rex & (i64)8))) {
                aa_disasm$opsize = (i64)8;
            }
;
            aa_disasm$genstr((byte*)"xchg ");
            aa_disasm$genstr(aa_disasm$strreg((i64)1,aa_disasm$opsize));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$genstr(aa_disasm$strreg(reg,aa_disasm$opsize));
        }
        break;
    case 152:;
        {
            if (!!(aa_disasm$sizeoverride)) {
                aa_disasm$genstr((byte*)"cbw");
            }
            else {
                aa_disasm$genstr((byte*)"cbw???");
            }
;
        }
        break;
    case 153:;
        {
            if (!!(aa_disasm$sizeoverride)) {
                aa_disasm$genstr((byte*)"cwd");
            }
            else if (!!((aa_disasm$rex & (i64)8))) {
                aa_disasm$genstr((byte*)"cqo");
            }
            else {
                aa_disasm$genstr((byte*)"cdq");
            }
;
        }
        break;
    case 155:;
        {
            aa_disasm$genstr((byte*)"wait");
        }
        break;
    case 156:;
        {
            aa_disasm$genstr((byte*)"pushf");
        }
        break;
    case 157:;
        {
            aa_disasm$genstr((byte*)"popf");
        }
        break;
    case 158:;
        {
            aa_disasm$genstr((byte*)"sahf");
        }
        break;
    case 159:;
        {
            aa_disasm$genstr((byte*)"lahf");
        }
        break;
    case 164:;
    case 165:;
    case 166:;
    case 167:;
    case 170:;
    case 171:;
    case 172:;
    case 173:;
    case 174:;
    case 175:;
        {
            aa_disasm$genstr((((opc >> (i64)1) & (i64)7)==1?(byte*)"?":(((opc >> (i64)1) & (i64)7)==2?(byte*)"movs":(((opc >> (i64)1) & (i64)7)==3?(byte*)"cmps":(((opc >> (i64)1) & (i64)7)==4?(byte*)"?":(((opc >> (i64)1) & (i64)7)==5?(byte*)"stos":(((opc >> (i64)1) & (i64)7)==6?(byte*)"lods":(((opc >> (i64)1) & (i64)7)==7?(byte*)"scas":(byte*)"?"))))))));
            if (((opc & (i64)1) == (i64)0)) {
                aa_disasm$genstr((byte*)"b");
            }
            else {
                if (!!((aa_disasm$rex & (i64)8))) {
                    aa_disasm$genstr((byte*)"q");
                }
                else if (!!(aa_disasm$sizeoverride)) {
                    aa_disasm$genstr((byte*)"w");
                }
                else {
                    aa_disasm$genstr((byte*)"d");
                }
;
            }
;
        }
        break;
    case 168:;
    case 169:;
        {
            aa_disasm$genstr((byte*)"test ");
            if (!!((opc & (i64)1))) {
                aa_disasm$opsize = (!!(aa_disasm$sizeoverride) ? (i64)2 : (i64)4);
                if (!!((aa_disasm$rex & (i64)8))) {
                    aa_disasm$opsize = (i64)8;
                }
;
            }
;
            aa_disasm$genstr(aa_disasm$strreg((i64)1,aa_disasm$opsize));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$genintd(aa_disasm$readimm());
        }
        break;
    case 176:;
    case 177:;
    case 178:;
    case 179:;
    case 180:;
    case 181:;
    case 182:;
    case 183:;
    case 184:;
    case 185:;
    case 186:;
    case 187:;
    case 188:;
    case 189:;
    case 190:;
    case 191:;
        {
            reg = ((opc & (i64)7) + (i64)1);
            if (!!((aa_disasm$rex & (i64)1))) {
                reg += (i64)8;
            }
;
            if (!!((opc & (i64)8))) {
                aa_disasm$opsize = (!!(aa_disasm$sizeoverride) ? (i64)2 : (i64)4);
                if (!!((aa_disasm$rex & (i64)8))) {
                    aa_disasm$opsize = (i64)8;
                }
;
            }
;
            aa_disasm$genstr((byte*)"mov ");
            aa_disasm$getsil(&reg);
            aa_disasm$genstr(aa_disasm$strreg(reg,aa_disasm$opsize));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$genintd(aa_disasm$readimm8());
        }
        break;
    case 192:;
    case 193:;
    case 208:;
    case 209:;
    case 210:;
    case 211:;
        {
            aa_disasm$decodeaddr((opc & (i64)1));
            aa_disasm$getsilx(&aa_disasm$basereg);
            aa_disasm$genstr(((aa_disasm$rmopc + (i64)1)==1?(byte*)"rol":((aa_disasm$rmopc + (i64)1)==2?(byte*)"ror":((aa_disasm$rmopc + (i64)1)==3?(byte*)"rcl":((aa_disasm$rmopc + (i64)1)==4?(byte*)"rcr":((aa_disasm$rmopc + (i64)1)==5?(byte*)"shl":((aa_disasm$rmopc + (i64)1)==6?(byte*)"shr":((aa_disasm$rmopc + (i64)1)==7?(byte*)"?":((aa_disasm$rmopc + (i64)1)==8?(byte*)"sar":(byte*)"?")))))))));
            aa_disasm$printaddrmode((i64)0);
            if ((opc <= (i64)193)) {
                aa_disasm$genstr((byte*)", ");
                aa_disasm$genintd(aa_disasm$readbyte());
            }
            else {
                aa_disasm$genstr((!!((opc & (i64)2)) ? (byte*)", cl" : (byte*)", 1"));
            }
;
        }
        break;
    case 194:;
        {
            aa_disasm$genstr((byte*)"retn ");
            aa_disasm$genintd((i64)aa_disasm$readword16());
        }
        break;
    case 195:;
        {
            aa_disasm$genstr((byte*)"ret");
        }
        break;
    case 198:;
    case 199:;
        {
            aa_disasm$decodeaddr((opc & (i64)1));
            aa_disasm$genstr((byte*)"mov");
            aa_disasm$printaddrmode((i64)0);
            aa_disasm$genstr((byte*)", ");
            aa_disasm$genintd(aa_disasm$readimm());
        }
        break;
    case 215:;
        {
            aa_disasm$genstr((byte*)"xlat");
        }
        break;
    case 216:;
    case 217:;
    case 218:;
    case 219:;
    case 220:;
    case 221:;
    case 222:;
    case 223:;
        {
            aa_disasm$decode8087((opc & (i64)7));
        }
        break;
    case 224:;
        {
            aa_disasm$genstr((byte*)"loopnz ");
            aa_disasm$genintd(aa_disasm$readsbyte());
        }
        break;
    case 225:;
        {
            aa_disasm$genstr((byte*)"loopz ");
            aa_disasm$genintd(aa_disasm$readsbyte());
        }
        break;
    case 226:;
        {
            aa_disasm$genstr((byte*)"loop ");
            aa_disasm$genintd(aa_disasm$readsbyte());
        }
        break;
    case 227:;
        {
            if (!!(aa_disasm$addroverride)) {
                aa_disasm$genstr((byte*)"jecxz ");
            }
            else {
                aa_disasm$genstr((byte*)"jrcxz ");
            }
;
            aa_disasm$genintd(aa_disasm$readsbyte());
        }
        break;
    case 232:;
        {
            aa_disasm$genstr((byte*)"call ");
            aa_disasm$genintd(aa_disasm$readint32());
        }
        break;
    case 233:;
        {
            aa_disasm$genstr((byte*)"[4] jmp ");
            aa_disasm$genintd(aa_disasm$readint32());
        }
        break;
    case 235:;
        {
            aa_disasm$genstr((byte*)"jmp ");
            aa_disasm$genintd(aa_disasm$readsbyte());
        }
        break;
    case 242:;
        {
            if ((((i64)(*aa_disasm$codeptr) != (i64)15) && (((i64)(*aa_disasm$codeptr) < (i64)64) && ((i64)(*aa_disasm$codeptr) > (i64)79)))) {
                aa_disasm$genstr((byte*)"repne");
            }
            else {
                aa_disasm$f2override = (i64)1;
                goto L333 ;
;
            }
;
        }
        break;
    case 243:;
        {
            if ((((i64)(*aa_disasm$codeptr) != (i64)15) && (((i64)(*aa_disasm$codeptr) < (i64)64) && ((i64)(*aa_disasm$codeptr) > (i64)79)))) {
                aa_disasm$genstr((byte*)"repe");
            }
            else {
                aa_disasm$f3override = (i64)1;
                goto L333 ;
;
            }
;
        }
        break;
    case 244:;
        {
        }
        break;
    case 246:;
    case 247:;
        {
            aa_disasm$decodeaddr((opc & (i64)1));
            aa_disasm$getsilx(&aa_disasm$basereg);
            aa_disasm$genstr(((aa_disasm$rmopc + (i64)1)==1?(byte*)"test":((aa_disasm$rmopc + (i64)1)==2?(byte*)"?":((aa_disasm$rmopc + (i64)1)==3?(byte*)"not":((aa_disasm$rmopc + (i64)1)==4?(byte*)"neg":((aa_disasm$rmopc + (i64)1)==5?(byte*)"mul":((aa_disasm$rmopc + (i64)1)==6?(byte*)"imul":((aa_disasm$rmopc + (i64)1)==7?(byte*)"div":((aa_disasm$rmopc + (i64)1)==8?(byte*)"idiv":(byte*)"?")))))))));
            aa_disasm$printaddrmode((i64)0);
            if ((aa_disasm$rmopc == (i64)0)) {
                if ((aa_disasm$opsize == (i64)8)) {
                    aa_disasm$opsize = (i64)4;
                }
;
                aa_disasm$genstr((byte*)", ");
                aa_disasm$genintd(aa_disasm$readimm());
            }
;
        }
        break;
    case 254:;
        {
            w = (i64)0;
            goto L335 ;
;
        }
        break;
    case 255:;
        {
            w = (i64)1;
            //doff:
L335 :;
;
            aa_disasm$decodeaddr(w);
            if ((aa_disasm$rmopc==(i64)0)) {
                aa_disasm$getsilx(&aa_disasm$basereg);
                aa_disasm$genstr((byte*)"inc");
            }
            else if ((aa_disasm$rmopc==(i64)1)) {
                aa_disasm$getsilx(&aa_disasm$basereg);
                aa_disasm$genstr((byte*)"dec");
            }
            else if ((aa_disasm$rmopc==(i64)2)) {
                aa_disasm$opsize = (i64)8;
                aa_disasm$genstr((byte*)"icall");
            }
            else if ((aa_disasm$rmopc==(i64)4)) {
                aa_disasm$opsize = (i64)8;
                aa_disasm$genstr((byte*)"jmp");
            }
            else if ((aa_disasm$rmopc==(i64)6)) {
                aa_disasm$opsize = (i64)8;
                aa_disasm$genstr((byte*)"push");
            }
            else {
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"FFxx?",NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
            }
;
            aa_disasm$printaddrmode((i64)0);
        }
        break;
    default: {
        aa_disasm$genstr((byte*)"Unknown opcode: ");
        aa_disasm$genhex(opc);
    }
    } //SW
;
    msysc$m_print_startstr(str);
    msysc$m_print_ptr(baseaddr,(byte*)"z6h");
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)": ",NULL);
    msysc$m_print_end();
    ;
    n = (aa_disasm$codeptr - pstart);
    $av_1 = n;
    while ($av_1-- > 0) {
L336 :;
        msysc$m_print_startstr(str2);
        msysc$m_print_i64((i64)(*(pstart)++),(byte*)"z2H");
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)" ",NULL);
        msysc$m_print_end();
        ;
        strcat((u8 *)str,(u8 *)str2);
L337 :;
    }
L338 :;
    ;
    $av_2 = ((i64)14 - n);
    while ($av_2-- > 0) {
L339 :;
        strcat((u8 *)str,(byte*)"-- ");
L340 :;
    }
L341 :;
    ;
    strcat((u8 *)str,(u8 *)aa_disasm$deststr);
    (*cptr) = aa_disasm$codeptr;
    return (u8 *)str;
}

static void aa_disasm$decodetwobyteinstr(void) {
        i64 opc;
        i64 rhssize;
        i64 third;
        i64 imm;
        u8 *  opcstr;
    switch ((opc = (i64)(*(aa_disasm$codeptr)++))) {
    case 42:;
        {
            aa_disasm$decodeaddr((i64)1);
            if (!!(aa_disasm$f3override)) {
                aa_disasm$genstr((byte*)"cvtsi2ss ");
            }
            else {
                aa_disasm$genstr((byte*)"cvtsi2sd ");
            }
;
            aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$printaddrmode((i64)0);
        }
        break;
    case 44:;
        {
            aa_disasm$decodeaddr((i64)1);
            if (!!(aa_disasm$f3override)) {
                aa_disasm$genstr((byte*)"cvttss2si ");
                rhssize = (i64)4;
            }
            else {
                aa_disasm$genstr((byte*)"cvttsd2si ");
                rhssize = (i64)8;
            }
;
            if (!!((aa_disasm$rex & (i64)8))) {
                aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,(i64)8));
            }
            else {
                aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,(i64)4));
            }
;
            aa_disasm$genstr((byte*)", ");
            aa_disasm$opsize = rhssize;
            aa_disasm$printaddrmode((i64)1);
        }
        break;
    case 45:;
        {
            aa_disasm$decodeaddr((i64)1);
            if (!!(aa_disasm$f3override)) {
                aa_disasm$genstr((byte*)"cvtss2si ");
                rhssize = (i64)4;
            }
            else {
                aa_disasm$genstr((byte*)"cvtsd2si ");
                rhssize = (i64)8;
            }
;
            if (!!((aa_disasm$rex & (i64)8))) {
                aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,(i64)8));
            }
            else {
                aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,(i64)4));
            }
;
            aa_disasm$genstr((byte*)", ");
            aa_disasm$opsize = rhssize;
            aa_disasm$printaddrmode((i64)1);
        }
        break;
    case 47:;
        {
            aa_disasm$decodeaddr((i64)1);
            if (!!(aa_disasm$sizeoverride)) {
                aa_disasm$opsize = (i64)8;
                aa_disasm$genstr((byte*)"comisd ");
            }
            else {
                aa_disasm$opsize = (i64)4;
                aa_disasm$genstr((byte*)"comiss ");
            }
;
            aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$printaddrmode((i64)1);
        }
        break;
    case 58:;
        {
            third = (i64)(*(aa_disasm$codeptr)++);
            if ((third==(i64)99)) {
                aa_disasm$genstr((byte*)"pcmpistri ");
            }
            else if ((third==(i64)98)) {
                aa_disasm$genstr((byte*)"pcmpistrm ");
            }
            else {
                aa_disasm$genstr((byte*)"Unknown opcode 2-byte opcode: 0F ");
                aa_disasm$genhex(opc);
                return;
            }
;
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$printaddrmode((i64)1);
            aa_disasm$genstr((byte*)", ");
            imm = (i64)(*(aa_disasm$codeptr)++);
            aa_disasm$genintd(imm);
        }
        break;
    case 64:;
    case 65:;
    case 66:;
    case 67:;
    case 68:;
    case 69:;
    case 70:;
    case 71:;
    case 72:;
    case 73:;
    case 74:;
    case 75:;
    case 76:;
    case 77:;
    case 78:;
    case 79:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$genstr((byte*)"cmov");
            aa_disasm$genstr(aa_disasm$condnames[((opc & (i64)15))]);
            aa_disasm$genstr((byte*)" ");
            aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,aa_disasm$opsize));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$printaddrmode((i64)0);
        }
        break;
    case 81:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$opsize = (!!(aa_disasm$f3override) ? (i64)4 : (i64)8);
            aa_disasm$genstr(((aa_disasm$opsize == (i64)4) ? (byte*)"sqrtss " : (byte*)"sqrtsd "));
            aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$printaddrmode((i64)1);
        }
        break;
    case 84:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$genstr((!!(aa_disasm$sizeoverride) ? (byte*)"andpd " : (byte*)"andps "));
            aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$opsize = (!!(aa_disasm$sizeoverride) ? (i64)8 : (i64)4);
            aa_disasm$printaddrmode((i64)1);
        }
        break;
    case 87:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$genstr((!!(aa_disasm$sizeoverride) ? (byte*)"xorpd " : (byte*)"xorps "));
            aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$opsize = (!!(aa_disasm$sizeoverride) ? (i64)8 : (i64)4);
            aa_disasm$printaddrmode((i64)1);
        }
        break;
    case 88:;
        {
            opcstr = (byte*)"adds";
            //doarith:
L342 :;
;
            aa_disasm$genstr(opcstr);
            aa_disasm$decodeaddr((i64)1);
            if (!!(aa_disasm$f2override)) {
                aa_disasm$opsize = (i64)8;
                aa_disasm$genstr((byte*)"d ");
            }
            else {
                aa_disasm$opsize = (i64)4;
                aa_disasm$genstr((byte*)"s ");
            }
;
            aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$printaddrmode((i64)1);
        }
        break;
    case 89:;
        {
            opcstr = (byte*)"muls";
            goto L342 ;
;
        }
        break;
    case 90:;
        {
            aa_disasm$decodeaddr((i64)1);
            if (!!(aa_disasm$f3override)) {
                aa_disasm$genstr((byte*)"cvtss2sd ");
                rhssize = (i64)4;
            }
            else {
                aa_disasm$genstr((byte*)"cvtsd2ss ");
                rhssize = (i64)8;
            }
;
            aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$opsize = rhssize;
            aa_disasm$printaddrmode((i64)1);
        }
        break;
    case 92:;
        {
            opcstr = (byte*)"subs";
            goto L342 ;
;
        }
        break;
    case 93:;
        {
            opcstr = (byte*)"mins";
            goto L342 ;
;
        }
        break;
    case 94:;
        {
            opcstr = (byte*)"divs";
            goto L342 ;
;
        }
        break;
    case 95:;
        {
            opcstr = (byte*)"maxs";
            goto L342 ;
;
        }
        break;
    case 110:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$opsize = (!!((aa_disasm$rex & (i64)8)) ? (i64)8 : (i64)4);
            aa_disasm$genstr(((aa_disasm$opsize == (i64)4) ? (byte*)"movd " : (byte*)"movq "));
            if (!!(aa_disasm$sizeoverride)) {
                aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
            }
            else {
                aa_disasm$genstr(aa_disasm$strmmx(aa_disasm$rmreg));
            }
;
            aa_disasm$genstr((byte*)", ");
            aa_disasm$printaddrmode((i64)0);
        }
        break;
    case 111:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$opsize = (i64)16;
            if (!!(aa_disasm$sizeoverride)) {
                aa_disasm$genstr((byte*)"movdqa ");
            }
            else if (!!(aa_disasm$f3override)) {
                aa_disasm$genstr((byte*)"movdqu ");
            }
            else {
                aa_disasm$genstr((byte*)"No 66/F3 ");
            }
;
            aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$printaddrmode((i64)1);
        }
        break;
    case 126:;
        {
            aa_disasm$decodeaddr((i64)1);
            if (!!(aa_disasm$f3override)) {
                aa_disasm$opsize = (i64)8;
                aa_disasm$genstr((byte*)"movq ");
                aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
                aa_disasm$genstr((byte*)", ");
                aa_disasm$printaddrmode((i64)1);
            }
            else if (!!((aa_disasm$rex & (i64)8))) {
                aa_disasm$opsize = (i64)8;
                aa_disasm$genstr((byte*)"movq ");
                aa_disasm$printaddrmode((i64)0);
                aa_disasm$genstr((byte*)", ");
                aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
            }
            else {
                aa_disasm$opsize = (i64)4;
                aa_disasm$genstr((byte*)"movd ");
                aa_disasm$printaddrmode((i64)0);
                aa_disasm$genstr((byte*)", ");
                if (!!(aa_disasm$sizeoverride)) {
                    aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
                }
                else {
                    aa_disasm$genstr(aa_disasm$strmmx(aa_disasm$rmreg));
                }
;
            }
;
        }
        break;
    case 127:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$opsize = (i64)16;
            if (!!(aa_disasm$sizeoverride)) {
                aa_disasm$genstr((byte*)"movdqa ");
            }
            else if (!!(aa_disasm$f3override)) {
                aa_disasm$genstr((byte*)"movdqu ");
            }
            else {
                aa_disasm$genstr((byte*)"No 66/F3 ");
            }
;
            aa_disasm$printaddrmode((i64)1);
            aa_disasm$genstr((byte*)", ");
            aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
        }
        break;
    case 128:;
    case 129:;
    case 130:;
    case 131:;
    case 132:;
    case 133:;
    case 134:;
    case 135:;
    case 136:;
    case 137:;
    case 138:;
    case 139:;
    case 140:;
    case 141:;
    case 142:;
    case 143:;
        {
            aa_disasm$genstr((byte*)"[long] j");
            aa_disasm$genstr(aa_disasm$condnames[((opc & (i64)15))]);
            aa_disasm$genstr((byte*)" ");
            if (!!(aa_disasm$sizeoverride)) {
                aa_disasm$genintd(aa_disasm$readint16());
            }
            else {
                aa_disasm$genintd(aa_disasm$readint32());
            }
;
        }
        break;
    case 144:;
    case 145:;
    case 146:;
    case 147:;
    case 148:;
    case 149:;
    case 150:;
    case 151:;
    case 152:;
    case 153:;
    case 154:;
    case 155:;
    case 156:;
    case 157:;
    case 158:;
    case 159:;
        {
            aa_disasm$decodeaddr((i64)0);
            aa_disasm$genstr((byte*)"set");
            aa_disasm$genstr(aa_disasm$condnames[((opc & (i64)15))]);
            aa_disasm$genstr((byte*)" ");
            aa_disasm$getsilx(&aa_disasm$basereg);
            aa_disasm$printaddrmode((i64)0);
        }
        break;
    case 175:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$genstr((byte*)"imul ");
            aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,aa_disasm$opsize));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$printaddrmode((i64)0);
        }
        break;
    case 182:;
    case 183:;
    case 190:;
    case 191:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$genstr(((opc < (i64)190) ? (byte*)"movzx " : (byte*)"movsx "));
            aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,aa_disasm$opsize));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$opsize = (!!((opc & (i64)1)) ? (i64)2 : (i64)1);
            aa_disasm$printaddrmode((i64)0);
        }
        break;
    case 184:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$genstr((byte*)"popcnt ");
            aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,aa_disasm$opsize));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$printaddrmode((i64)0);
        }
        break;
    case 188:;
    case 189:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$genstr(((opc == (i64)188) ? (byte*)"bsf " : (byte*)"bsr "));
            aa_disasm$genstr(aa_disasm$strreg(aa_disasm$rmreg,aa_disasm$opsize));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$printaddrmode((i64)0);
        }
        break;
    case 214:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$opsize = (i64)8;
            aa_disasm$genstr((byte*)"movq ");
            aa_disasm$printaddrmode((i64)1);
            aa_disasm$genstr((byte*)",");
            aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
        }
        break;
    case 219:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$genstr((byte*)"pand ");
            aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$opsize = (i64)8;
            aa_disasm$printaddrmode((i64)1);
        }
        break;
    case 239:;
        {
            aa_disasm$decodeaddr((i64)1);
            aa_disasm$genstr((byte*)"pxor ");
            aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$rmreg));
            aa_disasm$genstr((byte*)", ");
            aa_disasm$opsize = (i64)8;
            aa_disasm$printaddrmode((i64)1);
        }
        break;
    default: {
        //error:
L343 :;
;
        aa_disasm$genstr((byte*)"Unknown opcode 2-byte opcode: 0F ");
        aa_disasm$genhex(opc);
    }
    } //SW
;
}

static void aa_disasm$decodeaddr(i64 w) {
        i64 modrm;
        i64 xxx;
        i64 mode;
        i64 sib;
        i64 rm;
    aa_disasm$basereg = (aa_disasm$indexreg = (i64)0);
    aa_disasm$scale = (i64)1;
    aa_disasm$offset = (i64)0;
    aa_disasm$ripmode = (i64)0;
    if (!!(w)) {
        aa_disasm$opsize = (!!(aa_disasm$sizeoverride) ? (i64)2 : (i64)4);
        if (!!((aa_disasm$rex & (i64)8))) {
            aa_disasm$opsize = (i64)8;
        }
;
    }
    else {
        aa_disasm$opsize = (i64)1;
    }
;
    modrm = (i64)(*(aa_disasm$codeptr)++);
    mode = (modrm >> (i64)6);
    xxx = ((modrm >> (i64)3) & (i64)7);
    rm = (modrm & (i64)7);
    if ((mode == (i64)3)) {
        aa_disasm$basereg = (rm + (i64)1);
        aa_disasm$addrmode = (i64)1;
    }
    else if ((rm != (i64)4)) {
        if (((mode == (i64)0) && (rm == (i64)5))) {
            aa_disasm$offset = aa_disasm$readint32();
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"RIP?",NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            aa_disasm$ripmode = (i64)1;
            aa_disasm$addrmode = (i64)2;
        }
        else {
            aa_disasm$basereg = (rm + (i64)1);
            aa_disasm$addrmode = (i64)2;
            if ((mode==(i64)1)) {
                aa_disasm$offset = aa_disasm$readsbyte();
            }
            else if ((mode==(i64)2)) {
                aa_disasm$offset = aa_disasm$readint32();
            }
;
        }
;
    }
    else {
        aa_disasm$addrmode = (i64)2;
        sib = aa_disasm$readbyte();
        aa_disasm$indexreg = (((sib >> (i64)3) & (i64)7) + (i64)1);
        aa_disasm$basereg = ((sib & (i64)7) + (i64)1);
        aa_disasm$scale = (((sib >> (i64)6) + (i64)1)==1?(i64)1:(((sib >> (i64)6) + (i64)1)==2?(i64)2:(((sib >> (i64)6) + (i64)1)==3?(i64)4:(((sib >> (i64)6) + (i64)1)==4?(i64)8:(i64)0))));
        if ((((mode == (i64)0) && (aa_disasm$basereg == (i64)6)) && (aa_disasm$indexreg == (i64)5))) {
            aa_disasm$indexreg = (aa_disasm$basereg = (i64)0);
            aa_disasm$offset = aa_disasm$readint32();
        }
        else if (((mode == (i64)0) && (aa_disasm$basereg == (i64)6))) {
            aa_disasm$basereg = (i64)0;
            aa_disasm$offset = aa_disasm$readint32();
        }
        else if (((mode == (i64)0) && (aa_disasm$indexreg == (i64)5))) {
            aa_disasm$indexreg = (i64)0;
        }
        else {
            if ((mode==(i64)1)) {
                aa_disasm$offset = aa_disasm$readsbyte();
            }
            else if ((mode==(i64)2)) {
                aa_disasm$offset = aa_disasm$readint32();
            }
;
            if ((aa_disasm$indexreg == (i64)5)) {
                aa_disasm$indexreg = (i64)0;
            }
;
        }
;
    }
;
    if ((!!(aa_disasm$basereg) && !!((aa_disasm$rex & (i64)1)))) {
        aa_disasm$basereg += (i64)8;
    }
;
    if ((!!(aa_disasm$indexreg) && !!((aa_disasm$rex & (i64)2)))) {
        aa_disasm$indexreg += (i64)8;
    }
;
    aa_disasm$rmreg = (xxx + (i64)1);
    if (!!((aa_disasm$rex & (i64)4))) {
        aa_disasm$rmreg += (i64)8;
    }
;
    aa_disasm$rmopc = xxx;
}

static i64 aa_disasm$readbyte(void) {
    return (i64)(*(aa_disasm$codeptr)++);
}

static i64 aa_disasm$readsbyte(void) {
    return (i64)(*(i8 *)(aa_disasm$codeptr)++);
}

static u64 aa_disasm$readword16(void) {
        u64 a;
    a = (u64)(i64)(*(u16 *)aa_disasm$codeptr);
    aa_disasm$codeptr += (i64)2;
    return a;
}

static i64 aa_disasm$readint16(void) {
        i64 a;
    a = (i64)(*(i16 *)aa_disasm$codeptr);
    aa_disasm$codeptr += (i64)2;
    return a;
}

static u64 aa_disasm$readword32(void) {
        u64 a;
    a = (u64)(i64)(*(u32 *)aa_disasm$codeptr);
    aa_disasm$codeptr += (i64)4;
    return a;
}

static i64 aa_disasm$readint32(void) {
        i64 a;
    a = (i64)(*(i32 *)aa_disasm$codeptr);
    aa_disasm$codeptr += (i64)4;
    return a;
}

static i64 aa_disasm$readint64(void) {
        i64 a;
    a = (*(i64 *)aa_disasm$codeptr);
    aa_disasm$codeptr += (i64)8;
    return a;
}

static i64 aa_disasm$getreg(i64 regcode,i64 upper) {
    if (!!(upper)) {
        return ((regcode + (i64)8) + (i64)1);
    }
;
    return (regcode + (i64)1);
}

u8 *aa_disasm$strreg(i64 reg,i64 opsize) {
        static u8 *  regnames8[20] = {
    (byte*)"al",
    (byte*)"cl",
    (byte*)"dl",
    (byte*)"bl",
    (byte*)"spl",
    (byte*)"bpl",
    (byte*)"sil",
    (byte*)"dil",
    (byte*)"r8b",
    (byte*)"r9b",
    (byte*)"r10b",
    (byte*)"r11b",
    (byte*)"r12b",
    (byte*)"r13b",
    (byte*)"r14b",
    (byte*)"r15b",
    (byte*)"ah",
    (byte*)"bh",
    (byte*)"ch",
    (byte*)"dh"
};
        static u8 *  regnames16[16] = {
    (byte*)"ax",
    (byte*)"cx",
    (byte*)"dx",
    (byte*)"bx",
    (byte*)"sp",
    (byte*)"bp",
    (byte*)"si",
    (byte*)"di",
    (byte*)"r8w",
    (byte*)"r9w",
    (byte*)"r10w",
    (byte*)"r11w",
    (byte*)"r12w",
    (byte*)"r13w",
    (byte*)"r14w",
    (byte*)"r15w"
};
        static u8 *  regnames32[16] = {
    (byte*)"eax",
    (byte*)"ecx",
    (byte*)"edx",
    (byte*)"ebx",
    (byte*)"esp",
    (byte*)"ebp",
    (byte*)"esi",
    (byte*)"edi",
    (byte*)"r8d",
    (byte*)"r9d",
    (byte*)"r10d",
    (byte*)"r11d",
    (byte*)"r12d",
    (byte*)"r13d",
    (byte*)"r14d",
    (byte*)"r15d"
};
        static u8 *  regnames64[16] = {
    (byte*)"rax",
    (byte*)"rcx",
    (byte*)"rdx",
    (byte*)"rbx",
    (byte*)"rsp",
    (byte*)"rbp",
    (byte*)"rsi",
    (byte*)"rdi",
    (byte*)"r8",
    (byte*)"r9",
    (byte*)"r10",
    (byte*)"r11",
    (byte*)"r12",
    (byte*)"r13",
    (byte*)"r14",
    (byte*)"r15"
};
        static u8 *  mregnames8[20] = {
    (byte*)"B0",
    (byte*)"B10",
    (byte*)"B11",
    (byte*)"B4",
    (byte*)"B15",
    (byte*)"B14",
    (byte*)"B5",
    (byte*)"B3",
    (byte*)"B12",
    (byte*)"B13",
    (byte*)"B1",
    (byte*)"B2",
    (byte*)"B6",
    (byte*)"B7",
    (byte*)"B8",
    (byte*)"B9",
    (byte*)"B16",
    (byte*)"B18",
    (byte*)"B19",
    (byte*)"B17"
};
        static u8 *  mregnames16[16] = {
    (byte*)"W0",
    (byte*)"W10",
    (byte*)"W11",
    (byte*)"W4",
    (byte*)"Wsp",
    (byte*)"Wbp",
    (byte*)"W5",
    (byte*)"W3",
    (byte*)"W12",
    (byte*)"W13",
    (byte*)"W1",
    (byte*)"W2",
    (byte*)"W6",
    (byte*)"W7",
    (byte*)"W8",
    (byte*)"W9"
};
        static u8 *  mregnames32[16] = {
    (byte*)"A0",
    (byte*)"A10",
    (byte*)"A11",
    (byte*)"A4",
    (byte*)"Astack",
    (byte*)"Aframe",
    (byte*)"A5",
    (byte*)"A3",
    (byte*)"A12",
    (byte*)"A13",
    (byte*)"A1",
    (byte*)"A2",
    (byte*)"A6",
    (byte*)"A7",
    (byte*)"A8",
    (byte*)"A9"
};
        static u8 *  mregnames64[16] = {
    (byte*)"D0",
    (byte*)"D10",
    (byte*)"D11",
    (byte*)"D4",
    (byte*)"Dstack",
    (byte*)"Dframe",
    (byte*)"D5",
    (byte*)"D3",
    (byte*)"D12",
    (byte*)"D13",
    (byte*)"D1",
    (byte*)"D2",
    (byte*)"D6",
    (byte*)"D7",
    (byte*)"D8",
    (byte*)"D9"
};
    if ((reg == (i64)0)) {
        return (byte*)"<>";
    }
;
    if ((u64)0u) {
        if ((opsize==(i64)1)) {
            return mregnames8[(reg)-1];
        }
        else if ((opsize==(i64)2)) {
            return mregnames16[(reg)-1];
        }
        else if ((opsize==(i64)4)) {
            return mregnames32[(reg)-1];
        }
        else if ((opsize==(i64)8)) {
            return mregnames64[(reg)-1];
        }
;
    }
    else {
        if ((opsize==(i64)1)) {
            return regnames8[(reg)-1];
        }
        else if ((opsize==(i64)2)) {
            return regnames16[(reg)-1];
        }
        else if ((opsize==(i64)4)) {
            return regnames32[(reg)-1];
        }
        else if ((opsize==(i64)8)) {
            return regnames64[(reg)-1];
        }
;
    }
;
    return (byte*)"";
}

static u8 *aa_disasm$strfreg(i64 freg) {
        static u8 *  fregnames[8] = {(byte*)"st0",(byte*)"st1",(byte*)"st2",(byte*)"st3",(byte*)"st4",(byte*)"st5",(byte*)"st6",(byte*)"st7"};
    return fregnames[(freg)-1];
}

static void aa_disasm$printaddrmode(i64 xmm) {
        u8 *  plus;
        i64 addrsize;
    aa_disasm$genstr((byte*)" ");
    if ((aa_disasm$addrmode==(i64)1)) {
        if (!!(xmm)) {
            aa_disasm$genstr(aa_disasm$strxmm(aa_disasm$basereg));
        }
        else {
            aa_disasm$getsilx(&aa_disasm$basereg);
            aa_disasm$genstr(aa_disasm$strreg(aa_disasm$basereg,aa_disasm$opsize));
        }
;
        return;
    }
;
    if ((aa_disasm$opsize==(i64)1)) {
        aa_disasm$genstr((byte*)"byte ");
    }
    else if ((aa_disasm$opsize==(i64)2)) {
        aa_disasm$genstr((byte*)"word ");
    }
    else if ((aa_disasm$opsize==(i64)4)) {
        aa_disasm$genstr((byte*)"dword ");
    }
    else if ((aa_disasm$opsize==(i64)8)) {
        aa_disasm$genstr((byte*)"qword ");
    }
    else if ((aa_disasm$opsize==(i64)10)) {
        aa_disasm$genstr((byte*)"tword ");
    }
    else if ((aa_disasm$opsize==(i64)16)) {
        aa_disasm$genstr((byte*)"oword ");
    }
    else {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"///OPSIZE",NULL);
        msysc$m_print_i64(aa_disasm$opsize,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    if (!!(aa_disasm$ripmode)) {
        aa_disasm$genstr((byte*)"rip:");
    }
;
    aa_disasm$genstr((byte*)"[");
    plus = (byte*)"";
    addrsize = (!!(aa_disasm$addroverride) ? (i64)4 : (i64)8);
    if (!!(aa_disasm$basereg)) {
        aa_disasm$genstr(aa_disasm$strreg(aa_disasm$basereg,addrsize));
        plus = (byte*)"+";
    }
;
    if (!!(aa_disasm$indexreg)) {
        aa_disasm$genstr(plus);
        aa_disasm$genstr(aa_disasm$strreg(aa_disasm$indexreg,addrsize));
        aa_disasm$genstr((byte*)"<INDEX>");
        if ((aa_disasm$scale > (i64)1)) {
            aa_disasm$genstr((byte*)"*");
            aa_disasm$genintd(aa_disasm$scale);
        }
;
        plus = (byte*)"+";
    }
;
    if ((!!(aa_disasm$offset) || ((aa_disasm$basereg == (i64)0) && (aa_disasm$indexreg == (i64)0)))) {
        if (((aa_disasm$basereg == (i64)0) && (aa_disasm$indexreg == (i64)0))) {
            aa_disasm$genhex(aa_disasm$offset);
        }
        else {
            if ((aa_disasm$offset > (i64)0)) {
                aa_disasm$genstr(plus);
            }
;
            aa_disasm$genintd(aa_disasm$offset);
        }
;
    }
;
    aa_disasm$genstr((byte*)"]");
}

static void aa_disasm$genstr(u8 *s) {
    strcat((u8 *)aa_disasm$deststr,s);
}

static void aa_disasm$genintd(i64 a) {
    aa_disasm$genstr(msysc$strint(a,0));
}

static void aa_disasm$genhex(i64 a) {
    aa_disasm$genstr(msysc$strint(a,(byte*)"h"));
}

static i64 aa_disasm$readimm(void) {
    if ((aa_disasm$opsize==(i64)1)) {
        return aa_disasm$readsbyte();
    }
    else if ((aa_disasm$opsize==(i64)2)) {
        return aa_disasm$readint16();
    }
    else if ((aa_disasm$opsize==(i64)4) || (aa_disasm$opsize==(i64)8)) {
        return aa_disasm$readint32();
    }
;
    return (i64)0;
}

static i64 aa_disasm$readimm8(void) {
    if ((aa_disasm$opsize < (i64)8)) {
        return aa_disasm$readimm();
    }
;
    return aa_disasm$readint64();
}

static u8 *aa_disasm$strxmm(i64 reg) {
        static u8 str[32];
    msysc$m_print_startstr(str);
    msysc$m_print_str((byte*)"xmm",NULL);
    msysc$m_print_nogap();
    msysc$m_print_i64((reg - (i64)1),NULL);
    msysc$m_print_end();
    ;
    return (u8 *)str;
}

static u8 *aa_disasm$strmmx(i64 reg) {
        static u8 str[32];
    msysc$m_print_startstr(str);
    msysc$m_print_str((byte*)"mmx",NULL);
    msysc$m_print_nogap();
    msysc$m_print_i64((reg - (i64)1),NULL);
    msysc$m_print_end();
    ;
    return (u8 *)str;
}

static void aa_disasm$decode8087(i64 ttt) {
        byte bb;
        i64 longopc;
        i64 freg;
        i64 shortopc;
    bb = (i64)(*(aa_disasm$codeptr)++);
    longopc = ((ttt << (i64)8) + (i64)bb);
    freg = (((i64)bb & (i64)7) + (i64)1);
    if ((longopc==(i64)1753)) {
        aa_disasm$genstr((byte*)"fcompp");
    }
    else if ((longopc==(i64)484)) {
        aa_disasm$genstr((byte*)"ftst");
    }
    else if ((longopc==(i64)485)) {
        aa_disasm$genstr((byte*)"fxam");
    }
    else if ((longopc==(i64)494)) {
        aa_disasm$genstr((byte*)"fldz");
    }
    else if ((longopc==(i64)488)) {
        aa_disasm$genstr((byte*)"fld1");
    }
    else if ((longopc==(i64)491)) {
        aa_disasm$genstr((byte*)"fldpi");
    }
    else if ((longopc==(i64)489)) {
        aa_disasm$genstr((byte*)"fldl2t");
    }
    else if ((longopc==(i64)490)) {
        aa_disasm$genstr((byte*)"fldl2e");
    }
    else if ((longopc==(i64)492)) {
        aa_disasm$genstr((byte*)"fldlg2");
    }
    else if ((longopc==(i64)493)) {
        aa_disasm$genstr((byte*)"fldln2");
    }
    else if ((longopc==(i64)506)) {
        aa_disasm$genstr((byte*)"fsqrt");
    }
    else if ((longopc==(i64)510)) {
        aa_disasm$genstr((byte*)"fsin");
    }
    else if ((longopc==(i64)511)) {
        aa_disasm$genstr((byte*)"fcos");
    }
    else if ((longopc==(i64)507)) {
        aa_disasm$genstr((byte*)"fsincos");
    }
    else if ((longopc==(i64)509)) {
        aa_disasm$genstr((byte*)"fscale");
    }
    else if ((longopc==(i64)504)) {
        aa_disasm$genstr((byte*)"fprem");
    }
    else if ((longopc==(i64)508)) {
        aa_disasm$genstr((byte*)"frndint");
    }
    else if ((longopc==(i64)500)) {
        aa_disasm$genstr((byte*)"fxtract");
    }
    else if ((longopc==(i64)481)) {
        aa_disasm$genstr((byte*)"fabs");
    }
    else if ((longopc==(i64)480)) {
        aa_disasm$genstr((byte*)"fchs");
    }
    else if ((longopc==(i64)498)) {
        aa_disasm$genstr((byte*)"fptan");
    }
    else if ((longopc==(i64)499)) {
        aa_disasm$genstr((byte*)"fpatan");
    }
    else if ((longopc==(i64)496)) {
        aa_disasm$genstr((byte*)"f2xm1");
    }
    else if ((longopc==(i64)497)) {
        aa_disasm$genstr((byte*)"fyl2x");
    }
    else if ((longopc==(i64)505)) {
        aa_disasm$genstr((byte*)"fyl2xp1");
    }
    else if ((longopc==(i64)995)) {
        aa_disasm$genstr((byte*)"finit");
    }
    else if ((longopc==(i64)992)) {
        aa_disasm$genstr((byte*)"feni");
    }
    else if ((longopc==(i64)993)) {
        aa_disasm$genstr((byte*)"fdisi");
    }
    else if ((longopc==(i64)994)) {
        aa_disasm$genstr((byte*)"fclex");
    }
    else if ((longopc==(i64)503)) {
        aa_disasm$genstr((byte*)"fincstp");
    }
    else if ((longopc==(i64)502)) {
        aa_disasm$genstr((byte*)"fdecstp");
    }
    else if ((longopc==(i64)464)) {
        aa_disasm$genstr((byte*)"fnop");
    }
    else {
                {i64 $temp = (longopc & (i64)2040);
if (($temp==(i64)448)) {
            aa_disasm$genstr((byte*)"fld ");
            aa_disasm$genstr(aa_disasm$strfreg(freg));
        }
        else if (($temp==(i64)1488)) {
            aa_disasm$genstr((byte*)"fst ");
            aa_disasm$genstr(aa_disasm$strfreg(freg));
        }
        else if (($temp==(i64)1496)) {
            aa_disasm$genstr((byte*)"fstp ");
            aa_disasm$genstr(aa_disasm$strfreg(freg));
        }
        else if (($temp==(i64)456)) {
            aa_disasm$genstr((byte*)"fxch ");
            aa_disasm$genstr(aa_disasm$strfreg(freg));
        }
        else if (($temp==(i64)208)) {
            aa_disasm$genstr((byte*)"fcom ");
            aa_disasm$genstr(aa_disasm$strfreg(freg));
        }
        else if (($temp==(i64)216)) {
            aa_disasm$genstr((byte*)"fcomp ");
            aa_disasm$genstr(aa_disasm$strfreg(freg));
        }
        else if (($temp==(i64)1472)) {
            aa_disasm$genstr((byte*)"ffree ");
            aa_disasm$genstr(aa_disasm$strfreg(freg));
        }
        else {
                        {i64 $temp = (longopc & (i64)504);
if (($temp==(i64)192)) {
                aa_disasm$do87arith((byte*)"fadd",ttt,freg);
            }
            else if (($temp==(i64)224)) {
                aa_disasm$do87arith((byte*)"fsub",ttt,freg);
            }
            else if (($temp==(i64)232)) {
                aa_disasm$do87arith((byte*)"fsubr",ttt,freg);
            }
            else if (($temp==(i64)200)) {
                aa_disasm$do87arith((byte*)"fmul",ttt,freg);
            }
            else if (($temp==(i64)240)) {
                aa_disasm$do87arith((byte*)"fdiv",ttt,freg);
            }
            else if (($temp==(i64)248)) {
                aa_disasm$do87arith((byte*)"fdivr",ttt,freg);
            }
            else {
                --(aa_disasm$codeptr);
                aa_disasm$decodeaddr((i64)0);
                shortopc = ((ttt << (i64)3) + aa_disasm$rmopc);
                if ((shortopc==(i64)61)) {
                    aa_disasm$do87mem((byte*)"fld",(i64)4);
                }
                else if ((shortopc==(i64)29)) {
                    aa_disasm$do87mem((byte*)"fld",(i64)5);
                }
                else if ((shortopc==(i64)60)) {
                    aa_disasm$do87mem((byte*)"fldbcd",(i64)-1);
                }
                else if ((shortopc==(i64)63)) {
                    aa_disasm$do87mem((byte*)"fstp",(i64)4);
                }
                else if ((shortopc==(i64)31)) {
                    aa_disasm$do87mem((byte*)"fstp",(i64)5);
                }
                else if ((shortopc==(i64)62)) {
                    aa_disasm$do87mem((byte*)"fstpbcd",(i64)-1);
                }
                else if ((shortopc==(i64)13)) {
                    aa_disasm$do87mem((byte*)"fldcw",(i64)-1);
                }
                else if ((shortopc==(i64)15)) {
                    aa_disasm$do87mem((byte*)"fstcw",(i64)-1);
                }
                else if ((shortopc==(i64)47)) {
                    aa_disasm$do87mem((byte*)"fstsw",(i64)-1);
                }
                else if ((shortopc==(i64)14)) {
                    aa_disasm$do87mem((byte*)"fstenv",(i64)-1);
                }
                else if ((shortopc==(i64)12)) {
                    aa_disasm$do87mem((byte*)"fldenv",(i64)-1);
                }
                else if ((shortopc==(i64)46)) {
                    aa_disasm$do87mem((byte*)"fsave",(i64)-1);
                }
                else if ((shortopc==(i64)44)) {
                    aa_disasm$do87mem((byte*)"frstor",(i64)-1);
                }
                else {
                                        {i64 $temp = (shortopc & (i64)15);
if (($temp==(i64)8)) {
                        aa_disasm$do87mem((byte*)"fld",(ttt >> (i64)1));
                    }
                    else if (($temp==(i64)10)) {
                        aa_disasm$do87mem((byte*)"fst",(ttt >> (i64)1));
                    }
                    else if (($temp==(i64)11)) {
                        aa_disasm$do87mem((byte*)"fstp",(ttt >> (i64)1));
                    }
                    else if (($temp==(i64)2)) {
                        aa_disasm$do87mem((byte*)"fcom",(ttt >> (i64)1));
                    }
                    else if (($temp==(i64)3)) {
                        aa_disasm$do87mem((byte*)"fcomp",(ttt >> (i64)1));
                    }
                    else if (($temp==(i64)0)) {
                        aa_disasm$do87mem((byte*)"fadd",(ttt >> (i64)1));
                    }
                    else if (($temp==(i64)4)) {
                        aa_disasm$do87mem((byte*)"fsub",(ttt >> (i64)1));
                    }
                    else if (($temp==(i64)5)) {
                        aa_disasm$do87mem((byte*)"fsubr",(ttt >> (i64)1));
                    }
                    else if (($temp==(i64)1)) {
                        aa_disasm$do87mem((byte*)"fmul",(ttt >> (i64)1));
                    }
                    else if (($temp==(i64)6)) {
                        aa_disasm$do87mem((byte*)"fdiv",(ttt >> (i64)1));
                    }
                    else if (($temp==(i64)7)) {
                        aa_disasm$do87mem((byte*)"fdivr",(ttt >> (i64)1));
                    }
                    else {
                        aa_disasm$genstr((byte*)"UNKNOWN x87 OPCODE");
                    }
                    };
                }
;
            }
            };
        }
        };
    }
;
}

static void aa_disasm$do87arith(u8 *opcstr,i64 ttt,i64 freg) {
        i64 d;
        i64 p;
    d = (ttt & (i64)4);
    p = (ttt & (i64)2);
    aa_disasm$genstr(opcstr);
    if (!!(p)) {
        aa_disasm$genstr((byte*)"p");
    }
;
    aa_disasm$genstr((byte*)" ");
    if ((d == (i64)0)) {
        aa_disasm$genstr((byte*)"st0, ");
        aa_disasm$genstr(aa_disasm$strfreg(freg));
    }
    else {
        aa_disasm$genstr(aa_disasm$strfreg(freg));
        aa_disasm$genstr((byte*)", st0");
    }
;
}

static void aa_disasm$do87mem(u8 *opcstr,i64 mf) {
    aa_disasm$genstr((byte*)"f");
    if ((mf==(i64)0)) {
        aa_disasm$opsize = (i64)4;
    }
    else if ((mf==(i64)1)) {
        aa_disasm$genstr((byte*)"i");
        aa_disasm$opsize = (i64)4;
    }
    else if ((mf==(i64)2)) {
        aa_disasm$opsize = (i64)8;
    }
    else if ((mf==(i64)3)) {
        aa_disasm$genstr((byte*)"i");
        aa_disasm$opsize = (i64)2;
    }
    else if ((mf==(i64)4)) {
        aa_disasm$genstr((byte*)"i");
        aa_disasm$opsize = (i64)8;
    }
    else if ((mf==(i64)5)) {
        aa_disasm$opsize = (i64)10;
    }
;
    aa_disasm$genstr((opcstr + (i64)1));
    aa_disasm$genstr((byte*)" ");
    aa_disasm$printaddrmode((i64)0);
}

static void aa_disasm$getsil(i64 *reg) {
    if (((((aa_disasm$opsize == (i64)1) && !(!!(aa_disasm$rex))) && ((*reg) >= (i64)5)) && ((*reg) <= (i64)8))) {
                {i64 $temp = (*reg);
if (($temp==(i64)5)) {
            (*reg) = (i64)17;
        }
        else if (($temp==(i64)6)) {
            (*reg) = (i64)19;
        }
        else if (($temp==(i64)7)) {
            (*reg) = (i64)20;
        }
        else if (($temp==(i64)8)) {
            (*reg) = (i64)18;
        }
        };
    }
;
}

static void aa_disasm$getsilx(i64 *reg) {
    if ((((((aa_disasm$addrmode == (i64)1) && (aa_disasm$opsize == (i64)1)) && (aa_disasm$rex == (i64)0)) && ((*reg) >= (i64)5)) && ((*reg) <= (i64)8))) {
                {i64 $temp = (*reg);
if (($temp==(i64)5)) {
            (*reg) = (i64)17;
        }
        else if (($temp==(i64)6)) {
            (*reg) = (i64)19;
        }
        else if (($temp==(i64)7)) {
            (*reg) = (i64)20;
        }
        else if (($temp==(i64)8)) {
            (*reg) = (i64)18;
        }
        };
    }
;
}

// START
void aa_disasm$start(void) {

}

void aa_genss$genss(void) {
        i64 index;
        struct aa_lib$mclrec *  m;
    aa_decls$ss_zdatalen = (i64)0;
    aa_decls$ss_zdata = (struct aa_decls$dbuffer *)aa_lib$buffercreate((i64)1024);
    aa_decls$ss_idata = (struct aa_decls$dbuffer *)aa_lib$buffercreate((i64)1024);
    aa_decls$ss_code = (struct aa_decls$dbuffer *)aa_lib$buffercreate((i64)1024);
    aa_decls$ss_idatarelocs = 0;
    aa_decls$ss_coderelocs = 0;
    aa_decls$ss_nsymbols = (i64)0;
    aa_genss$switchseg((i64)1);
    aa_decls$alineno = (i64)9999;
    aa_genss$extraparam = 0;
    m = (struct aa_lib$mclrec *)aa_lib$mccode;
    index = (i64)0;
    L344 :;
    while (!!(m)) {
        aa_decls$alineno = (i64)(*m).lineno;
        aa_genss$doinstr((struct aa_lib$mclrec *)m,++(index));
        m = (struct aa_lib$mclrec *)(*m).nextmcl;
L345 :;
    }
L346 :;
    ;
    aa_genss$switchseg((i64)0);
    if (!!(aa_lib$bufferlength((struct aa_decls$dbuffer *)aa_decls$ss_zdata))) {
        aa_lib$gerror((byte*)"Zdata contains code or data");
    }
;
}

static void aa_genss$doinstr(struct aa_lib$mclrec *m,i64 index) {
        struct aa_decls$opndrec *  a;
        struct aa_decls$opndrec *  b;
        struct aa_decls$strec *  d;
        i64 x;
        i64 offset;
        i64 shortjmp;
        i64 n;
        i64 $av_1;
        i64 $av_2;
    aa_genss$currmcl = (struct aa_lib$mclrec *)m;
    aa_lib$buffercheck((struct aa_decls$dbuffer *)aa_genss$currdata,(i64)1024);
    aa_genss$rex = (aa_genss$sizeoverride = (aa_genss$addroverride = (aa_genss$f2override = (aa_genss$f3override = (aa_genss$nowmask = (aa_genss$usesizeb = (i64)0))))));
    a = (struct aa_decls$opndrec *)(*m).a;
    b = (struct aa_decls$opndrec *)(*m).b;
    switch ((i64)(*m).opcode) {
    case 4:;
        {
            d = (struct aa_decls$strec *)(*a).labeldef;
            (*d).reftype = (i64)2;
            (*d).segment = aa_genss$currseg;
            (*d).offset = aa_genss$getcurrdatalen((i64)6);
            if (((i64)(*d).symbol == (i64)22)) {
                aa_genss$getstindex((struct aa_decls$strec *)d);
            }
;
            aa_genss$dofwdrefs((struct aa_decls$strec *)d);
        }
        break;
    case 20:;
        {
            aa_genss$do_call((struct aa_decls$opndrec *)a);
        }
        break;
    case 24:;
        {
            aa_genss$do_jmp((struct aa_decls$opndrec *)a,(struct aa_lib$mclrec *)m);
        }
        break;
    case 25:;
        {
            offset = aa_genss$getrel32((struct aa_decls$strec *)(*b).labeldef,(aa_genss$getcurrdatalen((i64)7) + (i64)1));
            if ((offset < (i64)0)) {
                if ((offset < (i64)-126)) {
                    aa_genss$genbyte((i64)15);
                    aa_genss$genbyte(((i64)128 + (*a).value));
                    aa_genss$gendword((offset - (i64)4));
                }
                else {
                    aa_genss$genbyte(((i64)112 + (*(*m).a).value));
                    aa_genss$genbyte(offset);
                }
;
            }
            else {
                shortjmp = aa_genss$checkshortjump((struct aa_lib$mclrec *)m,(struct aa_decls$strec *)(*b).labeldef);
                if (!(!!(shortjmp))) {
                    aa_genss$genbyte((i64)15);
                    aa_genss$genbyte(((i64)128 + (*a).value));
                    aa_genss$genrel32((struct aa_decls$opndrec *)b);
                }
                else {
                    aa_genss$genbyte(((i64)112 + (*a).value));
                    aa_genss$genrel8((struct aa_decls$opndrec *)b);
                }
;
            }
;
        }
        break;
    case 114:;
        {
            aa_genss$genopnd((struct aa_decls$opndrec *)a,(i64)1);
        }
        break;
    case 115:;
        {
            aa_genss$genopnd((struct aa_decls$opndrec *)a,(i64)2);
        }
        break;
    case 116:;
        {
            aa_genss$genopnd((struct aa_decls$opndrec *)a,(i64)4);
        }
        break;
    case 117:;
        {
            aa_genss$genopnd((struct aa_decls$opndrec *)a,(i64)8);
        }
        break;
    case 118:;
        {
            aa_genss$switchseg((*a).value);
        }
        break;
    case 5:;
    case 147:;
        {
            aa_genss$genbyte((i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
        }
        break;
    case 55:;
        {
            aa_genss$genbyte((i64)102);
            aa_genss$genbyte((i64)152);
        }
        break;
    case 56:;
        {
            aa_genss$genbyte((i64)102);
            aa_genss$genbyte((i64)153);
        }
        break;
    case 57:;
        {
            aa_genss$genbyte((i64)153);
        }
        break;
    case 58:;
        {
            aa_genss$genbyte((i64)72);
            aa_genss$genbyte((i64)153);
        }
        break;
    case 21:;
        {
            aa_genss$genbyte((i64)195);
        }
        break;
    case 23:;
        {
            aa_genss$genbyte((i64)201);
        }
        break;
    case 22:;
        {
            if (((i64)(*a).mode != (i64)2)) {
                aa_lib$gerror((byte*)"retn?");
            }
;
            aa_genss$genbyte((i64)194);
            aa_genss$genword((*a).value);
        }
        break;
    case 11:;
        {
            aa_genss$do_push((struct aa_decls$opndrec *)a);
        }
        break;
    case 12:;
        {
            aa_genss$do_pop((struct aa_decls$opndrec *)a);
        }
        break;
    case 53:;
    case 54:;
        {
            aa_genss$do_inc((struct aa_decls$opndrec *)a,(i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
        }
        break;
    case 51:;
    case 52:;
    case 38:;
    case 37:;
    case 42:;
    case 41:;
        {
            aa_genss$do_neg((struct aa_decls$opndrec *)a,(i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
        }
        break;
    case 27:;
    case 28:;
    case 31:;
    case 32:;
    case 33:;
    case 29:;
    case 30:;
    case 43:;
        {
            aa_genss$do_arith((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
        }
        break;
    case 9:;
        {
            aa_genss$do_mov((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
        }
        break;
    case 13:;
        {
            aa_genss$do_lea((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
        }
        break;
    case 17:;
        {
            aa_genss$do_movsx((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)190);
        }
        break;
    case 18:;
        {
            aa_genss$do_movsx((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)182);
        }
        break;
    case 19:;
        {
            aa_genss$do_movsxd((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
        }
        break;
    case 26:;
        {
            aa_genss$do_exch((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
        }
        break;
    case 39:;
        {
            aa_genss$do_imul2((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
        }
        break;
    case 123:;
    case 124:;
    case 125:;
    case 126:;
        {
            if (((i64)(*a).mode == (i64)2)) {
                n = ((*a).value * (i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
                aa_lib$buffercheck((struct aa_decls$dbuffer *)aa_genss$currdata,n);
                if ((aa_genss$currseg==(i64)1)) {
                    $av_1 = n;
                    while ($av_1-- > 0) {
L347 :;
                        aa_genss$genbyte((i64)144);
L348 :;
                    }
L349 :;
                    ;
                }
                else if ((aa_genss$currseg==(i64)2)) {
                    $av_2 = n;
                    while ($av_2-- > 0) {
L350 :;
                        aa_genss$genbyte((i64)0);
L351 :;
                    }
L352 :;
                    ;
                }
                else {
                    aa_decls$ss_zdatalen += n;
                }
;
            }
            else {
                aa_lib$gerror((byte*)"resb?");
            }
;
        }
        break;
    case 122:;
        {
            if (((i64)(*a).mode == (i64)2)) {
                x = (*a).value;
                if (((x < (i64)1) || (x > (i64)16384))) {
                    aa_lib$gerror((byte*)"align2");
                }
;
                if ((aa_genss$currseg != (i64)3)) {
                    L353 :;
                    while (!!((aa_lib$bufferlength((struct aa_decls$dbuffer *)aa_genss$currdata) % x))) {
                        aa_genss$genbyte(((aa_genss$currseg == (i64)1) ? (i64)144 : (i64)0));
L354 :;
                    }
L355 :;
                    ;
                }
                else {
                    L356 :;
                    while (!!((aa_decls$ss_zdatalen % x))) {
                        ++(aa_decls$ss_zdatalen);
L357 :;
                    }
L358 :;
                    ;
                }
;
            }
            else {
                aa_lib$gerror((byte*)"align?");
            }
;
        }
        break;
    case 44:;
    case 46:;
    case 45:;
    case 47:;
    case 48:;
    case 49:;
    case 50:;
        {
            aa_genss$do_shift((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
        }
        break;
    case 36:;
        {
            aa_genss$do_test((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
        }
        break;
    case 130:;
    case 129:;
    case 128:;
        {
            aa_genss$do_loop((struct aa_decls$opndrec *)a,(i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
        }
        break;
    case 131:;
        {
            aa_genss$do_jcxz((struct aa_decls$opndrec *)a,(i64)4);
        }
        break;
    case 132:;
        {
            aa_genss$do_jcxz((struct aa_decls$opndrec *)a,(i64)8);
        }
        break;
    case 127:;
        {
            aa_genss$genbyte((i64)215);
        }
        break;
    case 59:;
        {
            aa_genss$do_setcc((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
        }
        break;
    case 15:;
        {
            aa_genss$do_movxmm((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)4);
        }
        break;
    case 16:;
        {
            aa_genss$do_movxmm((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)8);
        }
        break;
    case 64:;
    case 65:;
    case 66:;
    case 67:;
    case 63:;
    case 110:;
    case 111:;
        {
            aa_genss$do_arithxmm((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)243,(i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
        }
        break;
    case 68:;
    case 69:;
    case 70:;
    case 71:;
    case 62:;
    case 112:;
    case 113:;
        {
            aa_genss$do_arithxmm((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)242,(i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
        }
        break;
    case 72:;
        {
            aa_genss$do_arithxmm((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)0,(i64)47);
        }
        break;
    case 73:;
        {
            aa_genss$do_arithxmm((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)102,(i64)47);
        }
        break;
    case 77:;
    case 75:;
        {
            aa_genss$do_logicxmm((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)aa_tables$mclcodes[((i64)(*m).opcode)-1],(i64)4);
        }
        break;
    case 76:;
    case 74:;
    case 79:;
    case 78:;
        {
            aa_genss$do_logicxmm((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)aa_tables$mclcodes[((i64)(*m).opcode)-1],(i64)8);
        }
        break;
    case 90:;
    case 91:;
        {
            aa_genss$do_pcmpistri((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)(*m).c,(i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
        }
        break;
    case 87:;
        {
            aa_genss$do_convertfloat((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)243);
        }
        break;
    case 86:;
        {
            aa_genss$do_convertfloat((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)242);
        }
        break;
    case 80:;
        {
            aa_genss$do_fix((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)243,(i64)45);
        }
        break;
    case 81:;
        {
            aa_genss$do_fix((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)242,(i64)45);
        }
        break;
    case 82:;
        {
            aa_genss$do_fix((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)243,(i64)44);
        }
        break;
    case 83:;
        {
            aa_genss$do_fix((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)242,(i64)44);
        }
        break;
    case 84:;
        {
            aa_genss$do_float((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)243);
        }
        break;
    case 85:;
        {
            aa_genss$do_float((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)242);
        }
        break;
    case 6:;
        {
            aa_genss$extraparam = (struct aa_decls$opndrec *)a;
        }
        break;
    case 14:;
        {
            aa_genss$do_cmovcc((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)aa_genss$extraparam,(struct aa_decls$opndrec *)b);
        }
        break;
    case 102:;
    case 103:;
    case 104:;
    case 105:;
    case 106:;
    case 107:;
    case 108:;
    case 109:;
        {
            aa_genss$genbyte((i64)217);
            aa_genss$genbyte((i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
        }
        break;
    case 92:;
    case 93:;
    case 94:;
        {
            aa_genss$do_fmem((struct aa_decls$opndrec *)a,(i64)1,(i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
        }
        break;
    case 95:;
    case 96:;
    case 97:;
        {
            aa_genss$do_fmem((struct aa_decls$opndrec *)a,(i64)0,(i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
        }
        break;
    case 98:;
    case 99:;
    case 100:;
    case 101:;
        {
            aa_genss$genbyte((i64)222);
            aa_genss$genbyte((i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
        }
        break;
    case 133:;
        {
            aa_genss$genbyte((i64)166);
        }
        break;
    case 134:;
        {
            aa_genss$genbyte((i64)102);
            aa_genss$genbyte((i64)167);
        }
        break;
    case 135:;
        {
            aa_genss$genbyte((i64)167);
        }
        break;
    case 136:;
        {
            aa_genss$genbyte((i64)72);
            aa_genss$genbyte((i64)167);
        }
        break;
    case 137:;
        {
            aa_genss$genbyte((i64)15);
            aa_genss$genbyte((i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
        }
        break;
    case 139:;
        {
            aa_genss$genbyte((i64)219);
            aa_genss$genbyte((i64)227);
        }
        break;
    case 140:;
    case 141:;
    case 142:;
    case 143:;
    case 144:;
    case 145:;
    case 146:;
        {
            aa_genss$genbyte((i64)217);
            aa_genss$genbyte((i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
        }
        break;
    case 138:;
        {
            aa_genss$do_popcnt((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
        }
        break;
    case 60:;
    case 61:;
        {
            aa_genss$do_bsf((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b,(i64)aa_tables$mclcodes[((i64)(*m).opcode)-1]);
        }
        break;
    default: {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"*** CAN'T DO OPCODE",NULL);
        msysc$m_print_str(aa_tables$mclnames[((i64)(*m).opcode)-1],NULL);
        msysc$m_print_str((byte*)"line",NULL);
        msysc$m_print_i64(aa_decls$alineno,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
    } //SW
;
}

static void aa_genss$genbyte(i64 x) {
    (*((*aa_genss$currdata).pcurr)++) = x;
}

static void aa_genss$genword(i64 x) {
    aa_lib$addword((struct aa_decls$dbuffer *)aa_genss$currdata,x);
}

static void aa_genss$gendword(i64 x) {
    aa_lib$adddword((struct aa_decls$dbuffer *)aa_genss$currdata,x);
}

static void aa_genss$genqword(i64 x) {
    aa_lib$addqword((struct aa_decls$dbuffer *)aa_genss$currdata,x);
}

static void aa_genss$genopnd(struct aa_decls$opndrec *a,i64 size) {
        u8 *  s;
        i64 x;
        i64 length;
    if ((size == (i64)0)) {
        size = (i64)(*a).size;
    }
;
    switch ((i64)(*a).mode) {
    case 2:;
    case 3:;
        {
        }
        break;
    case 6:;
        {
            s = (*a).svalue;
            length = strlen(s);
            if ((length > (i64)100)) {
                aa_lib$buffercheck((struct aa_decls$dbuffer *)aa_genss$currdata,msysc$m_imax((i64)1024,(length + (i64)1)));
            }
;
            L359 :;
            while (!!((u64)(*s))) {
                aa_genss$genbyte((i64)(u64)(*(s)++));
L360 :;
            }
L361 :;
            ;
            return;
        }
        break;
    default: {
        aa_lib$gerror((byte*)"GENOPND/bad opnd");
    }
    } //SW
;
    if ((!!((*a).labeldef) && (size <= (i64)2))) {
        aa_lib$gerror((byte*)"8/16-BIT RELOC");
    }
;
    if ((size==(i64)1)) {
        aa_genss$genbyte((*a).value);
    }
    else if ((size==(i64)2)) {
        aa_genss$genword((*a).value);
    }
    else if ((size==(i64)4)) {
        if (!!((*a).labeldef)) {
            aa_genss$genabs32((struct aa_decls$opndrec *)a);
        }
        else {
            if (!!((i64)(*a).valtype)) {
                aa_genss$gendword(aa_genss$getr32bits((*a).xvalue));
            }
            else {
                aa_genss$gendword((*a).value);
            }
;
        }
;
    }
    else if ((size==(i64)8)) {
        if (!!((*a).labeldef)) {
            aa_genss$genabs64((struct aa_decls$opndrec *)a);
        }
        else {
            x = (*a).value;
            if (!!((i64)(*a).valtype)) {
                aa_genss$genqword(x);
            }
            else {
                aa_genss$genqword(x);
            }
;
        }
;
    }
;
}

static void aa_genss$addrelocitem(i64 reloctype,struct aa_decls$strec *d) {
        struct aa_decls$relocrec *  r;
        i64 stindex;
        i64 adjust;
    stindex = aa_genss$getstindex((struct aa_decls$strec *)d);
    adjust = (i64)4;
    if ((reloctype == (i64)1)) {
        adjust = (i64)8;
    }
;
    r = (struct aa_decls$relocrec *)mlib$pcm_alloc((i64)32);
    (*r).nextreloc = (struct aa_decls$relocrec *)aa_genss$currrelocs;
    (*r).reloctype = reloctype;
    (*r).offset = (aa_genss$getcurrdatalen((i64)1) - adjust);
    (*r).stindex = stindex;
    ++(aa_genss$nrelocs);
    aa_genss$currrelocs = (struct aa_decls$relocrec *)r;
}

static i64 aa_genss$getstindex(struct aa_decls$strec *d) {
    if (((i64)(*d).stindex == (i64)0)) {
        if ((aa_decls$ss_nsymbols >= aa_decls$ss_symboltablesize)) {
            aa_genss$extendsymboltable();
        }
;
        (*d).stindex = ++(aa_decls$ss_nsymbols);
        (*aa_decls$ss_symboltable)[((i64)(*d).stindex)-1] = (struct aa_decls$strec *)d;
    }
;
    return (i64)(*d).stindex;
}

static void aa_genss$genrel32(struct aa_decls$opndrec *a) {
        struct aa_decls$strec *  d;
    d = (struct aa_decls$strec *)(*a).labeldef;
    if ((d == 0)) {
        aa_genss$gendword((*a).value);
        return;
    }
;
        {i64 $temp = (i64)(*d).reftype;
if (($temp==(i64)2)) {
        if (((i64)(*d).segment != aa_genss$currseg)) {
            aa_lib$gerror((byte*)"Rel label across segments");
        }
;
        aa_genss$gendword(((i64)(*d).offset - (aa_genss$getcurrdatalen((i64)2) + (i64)4)));
    }
    else if (($temp==(i64)1)) {
        (*d).fwdrefs = (struct aa_decls$fwdrec *)aa_genss$addfwdref((struct aa_decls$fwdrec *)(*d).fwdrefs,aa_genss$getcurrdatalen((i64)3),(i64)4,(i64)0);
        aa_genss$gendword((i64)0);
    }
    else {
        aa_genss$gendword((*a).value);
        aa_genss$addrelocitem((i64)4,(struct aa_decls$strec *)d);
    }
    };
}

static void aa_genss$genabs32(struct aa_decls$opndrec *a) {
        struct aa_decls$strec *  d;
    d = (struct aa_decls$strec *)(*a).labeldef;
        {i64 $temp = (i64)(*d).reftype;
if (($temp==(i64)2)) {
        aa_genss$gendword(((i64)(*d).offset + (*a).value));
        aa_genss$addrelocitem((i64)2,(struct aa_decls$strec *)d);
    }
    else if (($temp==(i64)1)) {
        (*d).fwdrefs = (struct aa_decls$fwdrec *)aa_genss$addfwdref((struct aa_decls$fwdrec *)(*d).fwdrefs,aa_genss$getcurrdatalen((i64)4),(i64)2,aa_genss$currseg);
        aa_genss$gendword((*a).value);
        aa_genss$addrelocitem((i64)2,(struct aa_decls$strec *)d);
    }
    else {
        aa_genss$gendword((*a).value);
        aa_genss$addrelocitem((i64)2,(struct aa_decls$strec *)d);
    }
    };
}

static void aa_genss$genabs64(struct aa_decls$opndrec *a) {
        struct aa_decls$strec *  d;
    d = (struct aa_decls$strec *)(*a).labeldef;
        {i64 $temp = (i64)(*d).reftype;
if (($temp==(i64)2)) {
        aa_genss$genqword(((i64)(*d).offset + (*a).value));
        aa_genss$addrelocitem((i64)1,(struct aa_decls$strec *)d);
    }
    else if (($temp==(i64)1)) {
        (*d).fwdrefs = (struct aa_decls$fwdrec *)aa_genss$addfwdref((struct aa_decls$fwdrec *)(*d).fwdrefs,aa_genss$getcurrdatalen((i64)5),(i64)2,aa_genss$currseg);
        aa_genss$genqword((*a).value);
        aa_genss$addrelocitem((i64)1,(struct aa_decls$strec *)d);
    }
    else {
        aa_genss$genqword((*a).value);
        aa_genss$addrelocitem((i64)1,(struct aa_decls$strec *)d);
    }
    };
}

static i64 aa_genss$getrel32(struct aa_decls$strec *d,i64 offset) {
    if (((i64)(*d).reftype == (i64)2)) {
        if (((i64)(*d).segment != aa_genss$currseg)) {
            aa_lib$gerror((byte*)"Rel label across segments2");
        }
;
        return ((i64)(*d).offset - (offset + (i64)1));
    }
    else {
        return (i64)2147483647;
    }
;
}

static void aa_genss$dofwdrefs(struct aa_decls$strec *d) {
        struct aa_decls$fwdrec *  f;
        i64 offset;
        byte *  p8;
        i32 *  p32;
        i64 *  p64;
        struct aa_decls$dbuffer *  data;
    if (((*d).fwdrefs == 0)) {
        return;
    }
;
    f = (struct aa_decls$fwdrec *)(*d).fwdrefs;
    L362 :;
    while (!!(f)) {
        offset = (i64)(*f).offset;
                {i64 $temp = (i64)(*f).reltype;
if (($temp==(i64)4)) {
            p32 = (i32 *)aa_lib$bufferelemptr((struct aa_decls$dbuffer *)aa_genss$currdata,offset);
            (*p32) = (((i64)(*d).offset - offset) - (i64)4);
        }
        else if (($temp==(i64)2) || ($temp==(i64)1)) {
                        {i64 $temp = (i64)(*f).seg;
if (($temp==(i64)1)) {
                data = (struct aa_decls$dbuffer *)aa_decls$ss_code;
            }
            else if (($temp==(i64)3)) {
                aa_lib$gerror((byte*)"Fwd ref in zdata");
            }
            else if (($temp==(i64)2)) {
                data = (struct aa_decls$dbuffer *)aa_decls$ss_idata;
            }
            };
            p32 = (i32 *)aa_lib$bufferelemptr((struct aa_decls$dbuffer *)data,offset);
            if (((i64)(*f).reltype == (i64)2)) {
                (*p32) = ((i64)(*p32) + (i64)(*d).offset);
            }
            else {
                p64 = (i64 *)p32;
                (*p64) = ((*p64) + (i64)(*d).offset);
            }
;
        }
        else if (($temp==(i64)6)) {
            p8 = (byte *)aa_lib$bufferelemptr((struct aa_decls$dbuffer *)aa_genss$currdata,offset);
            (*p8) = (((i64)(*d).offset - offset) - (i64)1);
        }
        else {
            msysc$m_print_startcon();
            msysc$m_print_str(aa_objdecls$relocnames[((i64)(*f).reltype)],NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            aa_lib$gerror((byte*)"DOFWDREFS/CAN'T DO RELTYPE");
        }
        };
        f = (struct aa_decls$fwdrec *)(*f).nextfwd;
L363 :;
    }
L364 :;
    ;
}

static void aa_genss$genrex(void) {
    if (!!((i64)aa_genss$f2override)) {
        aa_genss$genbyte((i64)242);
    }
;
    if (!!((i64)aa_genss$f3override)) {
        aa_genss$genbyte((i64)243);
    }
;
    if (!!((i64)aa_genss$sizeoverride)) {
        aa_genss$genbyte((i64)102);
    }
;
    if (!!((i64)aa_genss$addroverride)) {
        aa_genss$genbyte((i64)103);
    }
;
    if (!!((i64)aa_genss$nowmask)) {
        aa_genss$rex = msysc$m_setdotindex(aa_genss$rex,(i64)3,(u64)0u);
    }
;
    if (!!((i64)aa_genss$rex)) {
        aa_genss$genbyte((((i64)aa_genss$rex & (i64)15) + (i64)64));
    }
;
}

static i64 aa_genss$isbytesized(i64 x) {
    return (i64)((i64)-128<=x && x<=(i64)127);
}

static i64 aa_genss$isdwordsized(i64 x) {
    return (i64)((i64)-2147483648<=x && x<=(i64)2147483647);
}

static void aa_genss$do_push(struct aa_decls$opndrec *a) {
        i64 code;
    if (((i64)(*a).size == (i64)0)) {
        (*a).size = (i64)8;
    }
;
        {i64 $temp = (i64)(*a).mode;
if (($temp==(i64)1)) {
        if (((i64)(*a).size != (i64)8)) {
            aa_lib$gerror((byte*)"pushreg not 64-bit");
        }
;
        code = aa_genss$getregcode((i64)(*a).reg,(i64)1,(i64)0);
        aa_genss$rex = msysc$m_setdotindex(aa_genss$rex,(i64)3,(u64)0u);
        aa_genss$genrex();
        aa_genss$genbyte(((i64)80 + code));
    }
    else if (($temp==(i64)2)) {
        if (!!((*a).labeldef)) {
            aa_genss$genbyte((i64)104);
            aa_genss$genopnd((struct aa_decls$opndrec *)a,(i64)4);
        }
        else if (!!(aa_genss$isbytesized((*a).value))) {
            aa_genss$genbyte((i64)106);
            aa_genss$genbyte((*a).value);
        }
        else if (!!(aa_genss$isdwordsized((*a).value))) {
            aa_genss$genbyte((i64)104);
            aa_genss$gendword((*a).value);
        }
        else {
            aa_lib$gerror((byte*)"push imm value too large");
        }
;
    }
    else if (($temp==(i64)3)) {
        if (((i64)(*a).size != (i64)8)) {
            aa_lib$gerror((byte*)"push not 64-bit");
        }
;
        aa_genss$genxrm((i64)255,(i64)6,(struct aa_decls$opndrec *)a);
    }
    else {
        aa_lib$gerror((byte*)"push opnd?");
    }
    };
}

static void aa_genss$do_pop(struct aa_decls$opndrec *a) {
        i64 code;
    if (((i64)(*a).size == (i64)0)) {
        (*a).size = (i64)8;
    }
;
        {i64 $temp = (i64)(*a).mode;
if (($temp==(i64)1)) {
        if (((i64)(*a).size != (i64)8)) {
            aa_lib$gerror((byte*)"popreg not 64-bit");
        }
;
        code = aa_genss$getregcode((i64)(*a).reg,(i64)1,(i64)0);
        aa_genss$genrex();
        aa_genss$genbyte(((i64)88 + code));
    }
    else if (($temp==(i64)3)) {
        if (((i64)(*a).size != (i64)8)) {
            aa_lib$gerror((byte*)"pop not 64-bit");
        }
;
        aa_genss$genxrm((i64)143,(i64)0,(struct aa_decls$opndrec *)a);
    }
    else {
        aa_lib$gerror((byte*)"pop opnd?");
    }
    };
}

static void aa_genss$do_inc(struct aa_decls$opndrec *a,i64 code) {
        {i64 $temp = (i64)(*a).mode;
if (($temp==(i64)1) || ($temp==(i64)3)) {
        aa_genss$genxrm((((i64)(*a).size == (i64)1) ? (i64)254 : (i64)255),code,(struct aa_decls$opndrec *)a);
    }
    else {
        aa_lib$gerror((byte*)"inc/opnd?");
    }
    };
}

static void aa_genss$do_neg(struct aa_decls$opndrec *a,i64 code) {
        {i64 $temp = (i64)(*a).mode;
if (($temp==(i64)1) || ($temp==(i64)3)) {
        aa_genss$genxrm((((i64)(*a).size == (i64)1) ? (i64)246 : (i64)247),code,(struct aa_decls$opndrec *)a);
    }
    else {
        aa_lib$gerror((byte*)"neg/div/etc opnd?");
    }
    };
}

static void aa_genss$genamode(struct aa_decls$opndrec *a,i64 am) {
        i64 sib;
        i64 mode;
        i64 dispsize;
    sib = (am >> (i64)16);
    mode = ((am >> (i64)8) & (i64)255);
    dispsize = (am & (i64)255);
    aa_genss$genbyte(mode);
    if ((sib >= (i64)0)) {
        aa_genss$genbyte(sib);
    }
;
    if ((dispsize==(i64)0)) {
    }
    else if ((dispsize==(i64)1)) {
        aa_genss$genbyte((*a).value);
    }
    else if ((dispsize==(i64)4)) {
        if (!!((*a).labeldef)) {
            aa_genss$genabs32((struct aa_decls$opndrec *)a);
        }
        else {
            aa_genss$gendword((*a).value);
        }
;
    }
    else {
        aa_lib$gerror((byte*)"genamode size 2/8");
    }
;
}

static i64 aa_genss$makemodrm(i64 mode,i64 opc,i64 rm) {
    return (((mode << (i64)6) + (opc << (i64)3)) + rm);
}

static void aa_genss$setopsize(struct aa_decls$opndrec *a) {
        {i64 $temp = (i64)(*a).size;
if (($temp==(i64)1)) {
    }
    else if (($temp==(i64)2)) {
        aa_genss$sizeoverride = (i64)1;
    }
    else if (($temp==(i64)8)) {
        aa_genss$rex |= (u8)8u;
    }
    else if (($temp==(i64)4)) {
    }
    else if (($temp==(i64)16)) {
    }
    else {
        aa_lib$gerror((byte*)"Operand size not set");
    }
    };
}

static i64 aa_genss$getdispsize(struct aa_decls$opndrec *a,i64 mand) {
    if (!!((*a).labeldef)) {
        return (i64)4;
    }
;
    if ((!!((*a).value) || !!(mand))) {
        if (!!(aa_genss$isbytesized((*a).value))) {
            return (i64)1;
        }
        else {
            return (i64)4;
        }
;
    }
    else {
        return (i64)0;
    }
;
}

static void aa_genss$genrmbyte(i64 mode,i64 opc,i64 rm) {
    aa_genss$genbyte((((mode << (i64)6) + (opc << (i64)3)) + rm));
}

static i64 aa_genss$makeam(i64 m,i64 s,i64 d) {
    return (((s << (i64)16) + (m << (i64)8)) + d);
}

static void aa_genss$do_lea(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b) {
    if (!((((i64)(*a).mode == (i64)1) && ((i64)(*b).mode == (i64)3)))) {
        aa_lib$gerror((byte*)"LEA not reg/mem");
    }
;
    if (((i64)(*a).size < (i64)4)) {
        aa_lib$gerror((byte*)"LEA size error");
    }
;
    aa_genss$genrrm((i64)141,(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
}

static void aa_genss$do_movsx(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 opc) {
    if (((i64)(*a).mode != (i64)1)) {
        aa_lib$gerror((byte*)"movsx not reg");
    }
;
    if ((((i64)(*a).size == (i64)8) && ((i64)(*b).size == (i64)4))) {
        if ((opc == (i64)190)) {
            aa_genss$do_movsxd((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
        }
        else {
            a = (struct aa_decls$opndrec *)aa_lib$regtable[((i64)(*a).reg)-1][((i64)4)-1];
            aa_genss$do_mov((struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
        }
;
        return;
    }
;
    if ((((i64)(*a).size == (i64)1) || ((i64)(*a).size <= (i64)(*b).size))) {
        aa_lib$gerror((byte*)"movsx size error");
    }
;
    if (((opc == (i64)182) && ((i64)(*b).size == (i64)4))) {
        aa_lib$gerror((byte*)"movsx 4=>8 bytes?");
    }
;
        {i64 $temp = (i64)(*b).mode;
if (($temp==(i64)1)) {
    }
    else if (($temp==(i64)3)) {
        if (((i64)(*b).size == (i64)0)) {
            aa_lib$gerror((byte*)"movsx need size prefix");
        }
;
        if (((i64)(*b).size == (i64)8)) {
            aa_lib$gerror((byte*)"movsx size 8");
        }
;
    }
    else {
        aa_lib$gerror((byte*)"movsx not reg/mem");
    }
    };
    aa_genss$genrrm(((i64)3840 + (((i64)(*b).size == (i64)1) ? opc : (opc + (i64)1))),(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
}

static void aa_genss$checkhighreg(struct aa_decls$opndrec *a) {
    if (((i64)(*a).mode == (i64)1)) {
                {i64 $temp = (i64)(*a).reg;
if (($temp==(i64)6) || ($temp==(i64)4) || ($temp==(i64)15) || ($temp==(i64)16)) {
            aa_genss$rex |= (u8)64u;
        }
        };
    }
;
}

static void aa_genss$do_exch(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b) {
        i64 regcode;
    if ((((((i64)(*a).mode == (i64)1) && ((i64)(*b).mode == (i64)1)) && (((i64)(*a).reg == (i64)1) || ((i64)(*b).reg == (i64)1))) && ((i64)(*a).size != (i64)1))) {
        if (((i64)(*a).reg != (i64)1)) {
            {struct aa_decls$opndrec *  temp = a; a = b; b = temp; };
        }
;
        if (((i64)(*a).size != (i64)(*b).size)) {
            aa_lib$gerror((byte*)"exch size");
        }
;
        aa_genss$setopsize((struct aa_decls$opndrec *)a);
        regcode = aa_genss$getregcode((i64)(*b).reg,(i64)1,(i64)0);
        aa_genss$genrex();
        aa_genss$genbyte(((i64)144 + regcode));
        return;
    }
;
    if (((i64)(*a).mode == (i64)3)) {
        {struct aa_decls$opndrec *  temp = a; a = b; b = temp; };
    }
;
    if (!((((i64)(*a).mode == (i64)1) && (((i64)(*b).mode == (i64)1) || ((i64)(*b).mode == (i64)3))))) {
        aa_lib$gerror((byte*)"exch opnds");
    }
;
    if ((((i64)(*b).size == (i64)0) && ((i64)(*b).mode == (i64)3))) {
        (*b).size = (i64)(*a).size;
    }
;
    if (((i64)(*a).size != (i64)(*b).size)) {
        aa_lib$gerror((byte*)"exch size");
    }
;
    aa_genss$genrrm((((i64)(*a).size == (i64)1) ? (i64)134 : (i64)135),(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
}

static void aa_genss$do_movsxd(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b) {
    if ((((i64)(*b).mode == (i64)3) && ((i64)(*b).size == (i64)0))) {
        (*b).size = (i64)4;
    }
;
    if ((((i64)(*a).size != (i64)8) || ((i64)(*b).size > (i64)4))) {
        aa_lib$gerror((byte*)"movsxd size");
    }
;
    if ((((i64)(*a).mode != (i64)1) || (((i64)(*b).mode != (i64)1) && ((i64)(*b).mode != (i64)3)))) {
        aa_lib$gerror((byte*)"movsxd opnds");
    }
;
    aa_genss$genrrm((i64)99,(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
}

static void aa_genss$do_imul2(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b) {
        i64 opc;
        i64 value;
    if (((i64)(*a).mode != (i64)1)) {
        aa_lib$gerror((byte*)"imul2 opnds");
    }
;
    if (((i64)(*b).size == (i64)0)) {
        (*b).size = (i64)(*a).size;
    }
;
    if (((i64)(*a).size == (i64)1)) {
        aa_lib$gerror((byte*)"imul2 byte");
    }
;
        {i64 $temp = (i64)(*b).mode;
if (($temp==(i64)1) || ($temp==(i64)3)) {
        if (((i64)(*a).size != (i64)(*b).size)) {
            aa_lib$gerror((byte*)"imul2 size");
        }
;
        aa_genss$genrrm((i64)4015,(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
    }
    else if (($temp==(i64)2)) {
        if (!!((*b).labeldef)) {
            aa_lib$gerror((byte*)"mul/label");
        }
;
        value = (*b).value;
        if (((i64)-128<=value && value<=(i64)127)) {
            opc = (i64)107;
        }
        else {
            opc = (i64)105;
        }
;
        aa_genss$genrrm(opc,(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)a);
        if (((i64)-128<=value && value<=(i64)127)) {
            aa_genss$genbyte(value);
        }
        else if (((i64)(*a).size == (i64)2)) {
            aa_genss$genword(value);
        }
        else {
            aa_genss$gendword(value);
        }
;
    }
    else {
        aa_lib$gerror((byte*)"imul2 opnds");
    }
    };
}

static void aa_genss$do_shift(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 code) {
        i64 w;
        i64 opc;
        i64 needdisp;
    if ((((i64)(*a).mode != (i64)1) && ((i64)(*a).mode != (i64)3))) {
        aa_lib$gerror((byte*)"shift opnds1?");
    }
;
    if (!!((*b).labeldef)) {
        aa_lib$gerror((byte*)"shift/label");
    }
;
    w = (((i64)(*a).size == (i64)1) ? (i64)0 : (i64)1);
    needdisp = (i64)0;
        {i64 $temp = (i64)(*b).mode;
if (($temp==(i64)2)) {
        if (((*b).value == (i64)1)) {
            opc = ((i64)208 + w);
        }
        else {
            opc = ((i64)192 + w);
            needdisp = (i64)1;
        }
;
    }
    else if (($temp==(i64)1)) {
        if ((((i64)(*b).reg != (i64)11) || ((i64)(*b).size != (i64)1))) {
            aa_lib$gerror((byte*)"cl or b10 needed");
        }
;
        opc = ((i64)210 + w);
    }
    else {
        aa_lib$gerror((byte*)"shift opnds2?");
    }
    };
    aa_genss$genxrm(opc,code,(struct aa_decls$opndrec *)a);
    if (!!(needdisp)) {
        aa_genss$genbyte((*b).value);
    }
;
}

static void aa_genss$do_test(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b) {
        i64 value;
    if (((((i64)(*a).mode == (i64)1) && ((i64)(*a).reg == (i64)1)) && ((i64)(*b).mode == (i64)2))) {
        value = (*b).value;
                {i64 $temp = (i64)(*a).size;
if (($temp==(i64)1)) {
            aa_genss$genbyte((i64)168);
            aa_genss$genbyte(value);
        }
        else if (($temp==(i64)2)) {
            aa_genss$genbyte((i64)102);
            aa_genss$genbyte((i64)169);
            aa_genss$genword(value);
        }
        else if (($temp==(i64)4)) {
            aa_genss$genbyte((i64)169);
            aa_genss$gendword(value);
        }
        else {
            aa_genss$genbyte((i64)72);
            aa_genss$genbyte((i64)169);
            aa_genss$gendword(value);
        }
        };
    }
    else if (((((i64)(*a).mode == (i64)1) || ((i64)(*a).mode == (i64)3)) && ((i64)(*b).mode == (i64)2))) {
        aa_genss$genxrm((((i64)(*a).size == (i64)1) ? (i64)246 : (i64)247),(i64)0,(struct aa_decls$opndrec *)a);
                {i64 $temp = (i64)(*a).size;
if (($temp==(i64)1)) {
            aa_genss$genbyte(value);
        }
        else if (($temp==(i64)2)) {
            aa_genss$genword(value);
        }
        else {
            aa_genss$gendword(value);
        }
        };
    }
    else if ((((i64)(*a).mode == (i64)1 || (i64)(*a).mode == (i64)3) && ((i64)(*b).mode == (i64)1))) {
        //domemreg:
L365 :;
;
        aa_genss$genrrm((((i64)(*a).size == (i64)1) ? (i64)132 : (i64)133),(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
    }
    else if ((((i64)(*a).mode == (i64)1) && ((i64)(*b).mode == (i64)3))) {
        {struct aa_decls$opndrec *  temp = a; a = b; b = temp; };
        goto L365 ;
;
    }
    else {
        aa_lib$gerror((byte*)"test opnds");
    }
;
}

static void aa_genss$do_loop(struct aa_decls$opndrec *a,i64 opc) {
        i64 offset;
    offset = aa_genss$getrel32((struct aa_decls$strec *)(*a).labeldef,(aa_genss$getcurrdatalen((i64)9) + (i64)1));
    if ((offset < (i64)0)) {
        if ((offset < (i64)-126)) {
            aa_lib$gerror((byte*)"loop jmp out of range");
        }
;
        aa_genss$genbyte(opc);
        aa_genss$genbyte(offset);
    }
    else {
        aa_lib$gerror((byte*)"Can't do loopxx fwd jump");
    }
;
}

static void aa_genss$do_jcxz(struct aa_decls$opndrec *a,i64 opsize) {
        i64 offset;
    offset = aa_genss$getrel32((struct aa_decls$strec *)(*a).labeldef,(aa_genss$getcurrdatalen((i64)10) + (i64)1));
    if ((offset < (i64)0)) {
        if ((offset < (i64)-126)) {
            aa_lib$gerror((byte*)"jcxz jmp out of range");
        }
;
        if ((opsize == (i64)4)) {
            aa_genss$genbyte((i64)103);
        }
;
        aa_genss$genbyte((i64)227);
        aa_genss$genbyte(offset);
    }
    else {
        aa_lib$gerror((byte*)"Can't do jcxz fwd jump");
    }
;
}

static void aa_genss$do_setcc(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b) {
    if ((!(((i64)(*b).mode == (i64)1 || (i64)(*b).mode == (i64)3)) || ((i64)(*b).size > (i64)1))) {
        aa_lib$gerror((byte*)"setcc opnd/size");
    }
;
    aa_genss$genxrm(((i64)3984 + (*a).value),(i64)0,(struct aa_decls$opndrec *)b);
}

static void aa_genss$do_arithxmm(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 prefix,i64 opc) {
    if ((((i64)(*a).mode != (i64)5) || (((i64)(*b).mode != (i64)5) && ((i64)(*b).mode != (i64)3)))) {
        aa_lib$gerror((byte*)"arithxmm opnds");
    }
;
    if (!!(prefix)) {
        aa_genss$genbyte(prefix);
    }
;
    aa_genss$genrrm(((i64)3840 + opc),(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
}

static void aa_genss$do_logicxmm(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 opc,i64 size) {
    if ((((i64)(*a).mode != (i64)5) || (((i64)(*b).mode != (i64)5) && ((i64)(*b).mode != (i64)3)))) {
        aa_lib$gerror((byte*)"logicxmm opnds");
    }
;
    if ((size == (i64)8)) {
        aa_genss$genbyte((i64)102);
    }
;
    aa_genss$genrrm(((i64)3840 + opc),(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
}

static void aa_genss$do_convertfloat(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 prefix) {
    if ((((i64)(*a).mode != (i64)5) || (((i64)(*b).mode != (i64)5) && ((i64)(*b).mode != (i64)3)))) {
        aa_lib$gerror((byte*)"convertfloat opnds");
    }
;
    aa_genss$genbyte(prefix);
    aa_genss$nowmask = (i64)1;
    aa_genss$genrrm((i64)3930,(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
}

static void aa_genss$do_fix(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 prefix,i64 opc) {
    if ((((i64)(*a).mode != (i64)1) || (((i64)(*b).mode != (i64)5) && ((i64)(*b).mode != (i64)3)))) {
        aa_lib$gerror((byte*)"fix opnds");
    }
;
    aa_genss$checksize((struct aa_decls$opndrec *)a,(i64)4,(i64)8);
    (*b).size = ((prefix == (i64)243) ? (i64)4 : (i64)8);
    aa_genss$genbyte(prefix);
    aa_genss$genrrm(((i64)3840 + opc),(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
}

static void aa_genss$do_float(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 prefix) {
    if ((((i64)(*a).mode != (i64)5) || (((i64)(*b).mode != (i64)1) && ((i64)(*b).mode != (i64)3)))) {
        aa_lib$gerror((byte*)"float opnds");
    }
;
    aa_genss$checksize((struct aa_decls$opndrec *)b,(i64)4,(i64)8);
    (*a).size = ((prefix == (i64)243) ? (i64)4 : (i64)8);
    aa_genss$genbyte(prefix);
    aa_genss$usesizeb = (i64)1;
    aa_genss$genrrm((i64)3882,(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
}

static void aa_genss$do_call(struct aa_decls$opndrec *a) {
        {i64 $temp = (i64)(*a).mode;
if (($temp==(i64)2)) {
        aa_genss$genbyte((i64)232);
        aa_genss$genrel32((struct aa_decls$opndrec *)a);
    }
    else {
                {i64 $temp = (i64)(*a).size;
if (($temp==(i64)0)) {
            (*a).size = (i64)8;
        }
        else if (($temp==(i64)1) || ($temp==(i64)2) || ($temp==(i64)4)) {
            aa_lib$gerror((byte*)"call[]size");
        }
        };
        aa_genss$genxrm((i64)255,(i64)2,(struct aa_decls$opndrec *)a);
    }
    };
}

static void aa_genss$do_jmp(struct aa_decls$opndrec *a,struct aa_lib$mclrec *m) {
        i64 offset;
        i64 shortjmp;
        {i64 $temp = (i64)(*a).mode;
if (($temp==(i64)2)) {
        offset = aa_genss$getrel32((struct aa_decls$strec *)(*a).labeldef,(aa_genss$getcurrdatalen((i64)11) + (i64)1));
        if (((offset < (i64)0) && (offset > (i64)-126))) {
            aa_genss$genbyte((i64)235);
            aa_genss$genbyte(offset);
        }
        else {
            shortjmp = (i64)0;
            if ((offset > (i64)0)) {
                shortjmp = aa_genss$checkshortjump((struct aa_lib$mclrec *)m,(struct aa_decls$strec *)(*a).labeldef);
            }
;
            if (!(!!(shortjmp))) {
                aa_genss$genbyte((i64)233);
                aa_genss$genrel32((struct aa_decls$opndrec *)a);
            }
            else {
                aa_genss$genbyte((i64)235);
                aa_genss$genrel8((struct aa_decls$opndrec *)a);
            }
;
        }
;
    }
    else {
                {i64 $temp = (i64)(*a).size;
if (($temp==(i64)0)) {
            (*a).size = (i64)8;
        }
        else if (($temp==(i64)1) || ($temp==(i64)2) || ($temp==(i64)4)) {
            aa_lib$gerror((byte*)"jmp[]size");
        }
        };
        aa_genss$genxrm((i64)255,(i64)4,(struct aa_decls$opndrec *)a);
    }
    };
}

static i64 aa_genss$getcurrdatalen(i64 id) {
    if ((aa_genss$currseg == (i64)3)) {
        return aa_decls$ss_zdatalen;
    }
;
    return aa_lib$bufferlength((struct aa_decls$dbuffer *)aa_genss$currdata);
}

static void aa_genss$do_cmovcc(struct aa_decls$opndrec *c,struct aa_decls$opndrec *a,struct aa_decls$opndrec *b) {
    if ((((i64)(*a).size != (i64)(*b).size) && !!((i64)(*b).size))) {
        aa_lib$gerror((byte*)"Opnd size mismatch");
    }
;
    if (((i64)(*a).size == (i64)1)) {
        aa_lib$gerror((byte*)"cmov/byte");
    }
;
    aa_genss$genrrm(((i64)3904 + (*c).value),(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
}

static void aa_genss$do_fmem(struct aa_decls$opndrec *a,i64 freal,i64 code) {
        i64 mf;
    if (((i64)(*a).mode != (i64)3)) {
        aa_lib$gerror((byte*)"fmem/not mem");
    }
;
    if (!!(freal)) {
                {i64 $temp = (i64)(*a).size;
if (($temp==(i64)4)) {
            mf = (i64)0;
        }
        else if (($temp==(i64)8)) {
            mf = (i64)2;
        }
        else if (($temp==(i64)10) || ($temp==(i64)16)) {
            mf = (i64)1;
            if ((code==(i64)0)) {
                code = (i64)5;
            }
            else if ((code==(i64)3)) {
                code = (i64)7;
            }
            else {
                aa_lib$gerror((byte*)"r80 not allowed");
            }
;
        }
        else {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"SIZE=",NULL);
            msysc$m_print_i64((i64)(*a).size,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            aa_lib$gerror((byte*)"fmem size");
        }
        };
    }
    else {
                {i64 $temp = (i64)(*a).size;
if (($temp==(i64)2)) {
            mf = (i64)3;
        }
        else if (($temp==(i64)4)) {
            mf = (i64)1;
        }
        else if (($temp==(i64)8)) {
            mf = (i64)3;
            if ((code==(i64)0)) {
                code = (i64)5;
            }
            else if ((code==(i64)3)) {
                code = (i64)7;
            }
            else {
                aa_lib$gerror((byte*)"fst i64?");
            }
;
        }
        else {
            aa_lib$gerror((byte*)"fmem int size");
        }
        };
    }
;
    aa_genss$genxrm(((i64)217 + (mf << (i64)1)),code,(struct aa_decls$opndrec *)a);
}

static i64 aa_genss$getr32bits(r64 x) {
        r32 sx;
    sx = (r32)x;
    return *(i64*)&sx;
}

static void aa_genss$genrel8(struct aa_decls$opndrec *a) {
        struct aa_decls$strec *  d;
    d = (struct aa_decls$strec *)(*a).labeldef;
    if (((i64)(*d).reftype == (i64)1)) {
        (*d).fwdrefs = (struct aa_decls$fwdrec *)aa_genss$addfwdref((struct aa_decls$fwdrec *)(*d).fwdrefs,aa_genss$getcurrdatalen((i64)3),(i64)6,(i64)0);
        aa_genss$genbyte((i64)0);
    }
    else {
        aa_lib$gerror((byte*)"genrel8");
    }
;
}

static i64 aa_genss$checkshortjump(struct aa_lib$mclrec *m,struct aa_decls$strec *d) {
        i64 n;
    n = (i64)0;
    m = (struct aa_lib$mclrec *)(*m).nextmcl;
    L366 :;
    while ((!!(m) && (n <= (i64)8))) {
        ++(n);
        if ((((i64)(*m).opcode == (i64)4) && ((*(*m).a).labeldef == d))) {
            return (i64)1;
        }
;
        m = (struct aa_lib$mclrec *)(*m).nextmcl;
L367 :;
    }
L368 :;
    ;
    return (i64)0;
}

static struct aa_decls$fwdrec *aa_genss$addfwdref(struct aa_decls$fwdrec *p,i64 offset,i64 reltype,i64 seg) {
        struct aa_decls$fwdrec *  q;
    q = (struct aa_decls$fwdrec *)mlib$pcm_alloc((i64)16);
    (*q).nextfwd = (struct aa_decls$fwdrec *)p;
    (*q).offset = offset;
    (*q).reltype = reltype;
    (*q).seg = seg;
    return (struct aa_decls$fwdrec *)q;
}

static void aa_genss$switchseg(i64 newseg) {
    if ((newseg == aa_genss$currseg)) {
        return;
    }
;
    if ((aa_genss$currseg==(i64)1)) {
        aa_decls$ss_coderelocs = (struct aa_decls$relocrec *)aa_genss$currrelocs;
        aa_decls$ss_ncoderelocs = aa_genss$nrelocs;
    }
    else if ((aa_genss$currseg==(i64)2)) {
        aa_decls$ss_idatarelocs = (struct aa_decls$relocrec *)aa_genss$currrelocs;
        aa_decls$ss_nidatarelocs = aa_genss$nrelocs;
    }
;
    aa_genss$currseg = newseg;
    if ((aa_genss$currseg==(i64)1)) {
        aa_genss$currdata = (struct aa_decls$dbuffer *)aa_decls$ss_code;
        aa_genss$currrelocs = (struct aa_decls$relocrec *)aa_decls$ss_coderelocs;
        aa_genss$nrelocs = aa_decls$ss_ncoderelocs;
    }
    else if ((aa_genss$currseg==(i64)2)) {
        aa_genss$currdata = (struct aa_decls$dbuffer *)aa_decls$ss_idata;
        aa_genss$currrelocs = (struct aa_decls$relocrec *)aa_decls$ss_idatarelocs;
        aa_genss$nrelocs = aa_decls$ss_nidatarelocs;
    }
    else if ((aa_genss$currseg==(i64)3)) {
        aa_genss$currdata = (struct aa_decls$dbuffer *)aa_decls$ss_zdata;
    }
;
}

static void aa_genss$do_popcnt(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b) {
    if (((i64)(*b).mode == (i64)3)) {
        if (((i64)(*b).size == (i64)0)) {
            (*b).size = (i64)8;
        }
;
    }
;
    aa_genss$f3override = (i64)1;
    aa_genss$genrrm((i64)4024,(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
}

static void aa_genss$do_bsf(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 opc) {
    if (((i64)(*b).mode == (i64)3)) {
        if (((i64)(*b).size == (i64)0)) {
            (*b).size = (i64)8;
        }
;
    }
;
    if (((i64)(*a).size != (i64)(*b).size)) {
        aa_lib$gerror((byte*)"bsf size");
    }
;
    aa_genss$genrrm(((i64)3840 + opc),(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
}

static void aa_genss$extendsymboltable(void) {
        struct aa_decls$strec *(*oldsymboltable)[];
        i64 oldsymboltablesize;
        i64 i;
    oldsymboltablesize = aa_decls$ss_symboltablesize;
    oldsymboltable = (struct aa_decls$strec *(*)[])aa_decls$ss_symboltable;
    aa_decls$ss_symboltablesize *= (i64)2;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"EXTENDING SYMBOL TABLE TO",NULL);
    msysc$m_print_i64(aa_decls$ss_symboltablesize,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    aa_decls$ss_symboltable = (struct aa_decls$strec *(*)[])mlib$pcm_alloc(((i64)8 * aa_decls$ss_symboltablesize));
    for (i=(i64)1;i<=aa_decls$ss_nsymbols;++i) {
L369 :;
        (*aa_decls$ss_symboltable)[(i)-1] = (struct aa_decls$strec *)(*oldsymboltable)[(i)-1];
L370 :;
    }
L371 :;
    ;
    mlib$pcm_free(oldsymboltable,((i64)8 * oldsymboltablesize));
}

static void aa_genss$do_pcmpistri(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 c,i64 opc) {
    if ((((i64)(*a).mode != (i64)5) || (((i64)(*b).mode != (i64)5) && ((i64)(*b).mode != (i64)3)))) {
        aa_lib$gerror((byte*)"pcmpistrx opnds");
    }
;
    (*a).size = ((*b).size = (i64)8);
    aa_genss$sizeoverride = (i64)1;
    aa_genss$nowmask = (i64)1;
    aa_genss$genrrm((i64)997987,(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
    aa_genss$genbyte(c);
}

static void aa_genss$genxrm(i64 opcode,i64 code,struct aa_decls$opndrec *b) {
        i64 am;
    aa_genss$setopsize((struct aa_decls$opndrec *)b);
    am = aa_genss$newgenrm((i64)0,code,b,(i64)0);
        {i64 $temp = (i64)(*aa_genss$currmcl).opcode;
if (($temp==(i64)11) || ($temp==(i64)12)) {
        aa_genss$rex = msysc$m_setdotindex(aa_genss$rex,(i64)3,(u64)0u);
    }
    };
    aa_genss$genrex();
    if (!!((opcode & (i64)16711680))) {
        aa_genss$genbyte(((opcode >> (i64)16) & (i64)255));
    }
;
    if (!!((opcode & (i64)65280))) {
        aa_genss$genbyte(((opcode >> (i64)8) & (i64)255));
    }
;
    aa_genss$genbyte(opcode);
    aa_genss$genamode((struct aa_decls$opndrec *)b,am);
}

static void aa_genss$genrrm(i64 opcode,struct aa_decls$opndrec *a,struct aa_decls$opndrec *b) {
        i64 am;
    aa_genss$setopsize((struct aa_decls$opndrec *)a);
    if (!!((i64)aa_genss$usesizeb)) {
        aa_genss$rex = msysc$m_setdotindex(aa_genss$rex,(i64)3,(u64)0u);
        if (((i64)(*b).size == (i64)8)) {
            aa_genss$rex |= (u8)8u;
        }
;
    }
;
    aa_genss$checkhighreg((struct aa_decls$opndrec *)a);
    am = aa_genss$newgenrm((i64)(*a).reg,(i64)0,b,(i64)((i64)(*a).mode == (i64)5));
    aa_genss$genrex();
    if (!!((opcode & (i64)16711680))) {
        aa_genss$genbyte(((opcode >> (i64)16) & (i64)255));
    }
;
    if (!!((opcode & (i64)65280))) {
        aa_genss$genbyte(((opcode >> (i64)8) & (i64)255));
    }
;
    aa_genss$genbyte(opcode);
    aa_genss$genamode((struct aa_decls$opndrec *)b,am);
}

static i64 aa_genss$getregcode(i64 reg,i64 mask,i64 isxreg) {
        i64 regcode;
    if (!(!!(isxreg))) {
        regcode = (i64)aa_tables$regcodes[(reg)];
    }
    else {
        regcode = (reg - (i64)1);
    }
;
    if ((regcode >= (i64)8)) {
        regcode -= (i64)8;
        aa_genss$rex |= (byte)mask;
    }
;
    return regcode;
}

static void aa_genss$checkimmrange(i64 value,i64 size) {
    if ((size==(i64)1)) {
        if (!(((i64)-128<=value && value<=(i64)255))) {
            aa_lib$gerror((byte*)"exceeding byte value");
        }
;
    }
    else if ((size==(i64)2)) {
        if (!(((i64)-32768<=value && value<=(i64)65535))) {
            aa_lib$gerror((byte*)"exceeding word16 value");
        }
;
    }
    else {
        if (!(((i64)-2147483648<=value && value<=(i64)4294967295))) {
            aa_lib$gerror((byte*)"2:exceeding word32 value");
        }
;
    }
;
}

static i64 aa_genss$newgenrm(i64 reg,i64 opc,struct aa_decls$opndrec *b,i64 isxreg) {
        static i64 scaletable[8] = {(i64)0,(i64)1,(i64)0,(i64)2,(i64)0,(i64)0,(i64)0,(i64)3};
        i64 mode;
        i64 rm;
        i64 scale;
        i64 dispsize;
        i64 needsib;
        i64 sib;
        i64 index;
        i64 base;
        i64 regix;
    mode = (rm = (i64)0);
    scale = (i64)0;
    dispsize = (i64)0;
    needsib = (i64)0;
    sib = (i64)-1;
    if ((((i64)(*b).mode == (i64)3) && ((i64)(*b).addrsize == (i64)4))) {
        aa_genss$addroverride = (i64)1;
    }
;
    if (!!(reg)) {
        opc = aa_genss$getregcode(reg,(i64)4,isxreg);
    }
;
        {i64 $temp = (i64)(*b).mode;
if (($temp==(i64)1) || ($temp==(i64)5)) {
        rm = aa_genss$getregcode((i64)(*b).reg,(i64)1,(i64)((i64)(*b).mode == (i64)5));
        aa_genss$checkhighreg((struct aa_decls$opndrec *)b);
        return aa_genss$makeam(aa_genss$makemodrm((i64)3,opc,rm),sib,dispsize);
    }
    else if (($temp==(i64)3)) {
    }
    else {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"OPNDNAMES[B.MODE]=",NULL);
        msysc$m_print_str(aa_lib$opndnames[((i64)(*b).mode)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        aa_lib$gerror((byte*)"genrm not mem");
    }
    };
    reg = (i64)(*b).reg;
    regix = (i64)(*b).regix;
    if ((reg==regix && regix==(i64)0)) {
        mode = (i64)0;
        rm = (i64)4;
        scale = (i64)1;
        index = (i64)4;
        base = (i64)5;
        dispsize = (i64)4;
    }
    else if ((((i64)(*b).scale <= (i64)1) && (regix == (i64)0))) {
        dispsize = aa_genss$getdispsize((struct aa_decls$opndrec *)b,(i64)0);
        if (!!(dispsize)) {
            mode = ((dispsize == (i64)1) ? (i64)1 : (i64)2);
        }
;
        rm = (base = aa_genss$getregcode(reg,(i64)1,(i64)0));
        if ((rm != (i64)4)) {
            if (((rm == (i64)5) && (dispsize == (i64)0))) {
                mode = (i64)1;
                dispsize = (i64)1;
            }
;
            index = (i64)0;
        }
        else {
            index = (i64)4;
            scale = (i64)1;
        }
;
    }
    else if ((!!(regix) && (reg == (i64)0))) {
        dispsize = (i64)4;
        mode = (i64)0;
        rm = (i64)4;
        scale = (!!((i64)(*b).scale) ? (i64)(*b).scale : (i64)1);
        base = (i64)5;
        index = aa_genss$getregcode(regix,(i64)2,(i64)0);
        if ((regix == (i64)16)) {
            aa_lib$gerror((byte*)"Scaled rstack?");
        }
;
    }
    else {
        dispsize = aa_genss$getdispsize((struct aa_decls$opndrec *)b,(i64)0);
        if (!!(dispsize)) {
            mode = ((dispsize == (i64)1) ? (i64)1 : (i64)2);
        }
;
        rm = (i64)4;
        scale = (!!((i64)(*b).scale) ? (i64)(*b).scale : (i64)1);
        if ((reg == (i64)0)) {
            base = (i64)5;
        }
        else {
            if (((reg == (i64)15 || reg == (i64)8) && (dispsize == (i64)0))) {
                mode = (i64)1;
                dispsize = (i64)1;
            }
;
            base = aa_genss$getregcode(reg,(i64)1,(i64)0);
        }
;
        if ((regix == (i64)0)) {
            index = (i64)4;
        }
        else {
            index = aa_genss$getregcode(regix,(i64)2,(i64)0);
        }
;
        if ((!!(regix) && !(!!(reg)))) {
            dispsize = (i64)4;
        }
;
        if (((regix == (i64)16) && (scale > (i64)1))) {
            aa_lib$gerror((byte*)"Can't scale rstack");
        }
;
    }
;
    if (!!(scale)) {
        sib = (((scaletable[(scale)-1] << (i64)6) + (index << (i64)3)) + base);
    }
;
    return aa_genss$makeam(aa_genss$makemodrm(mode,opc,rm),sib,dispsize);
}

static void aa_genss$do_mov(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b) {
        i64 regcode;
        i64 opc;
        i64 value;
        {i64 $temp = (i64)(*a).mode;
if (($temp==(i64)1)) {
                {i64 $temp = (i64)(*b).mode;
if (($temp==(i64)1) || ($temp==(i64)3)) {
            if ((((i64)(*a).size != (i64)(*b).size) && !!((i64)(*b).size))) {
                aa_lib$gerror((byte*)"Opnd size mismatch");
            }
;
            aa_genss$genrrm((((i64)(*a).size == (i64)1) ? (i64)138 : (i64)139),(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
        }
        else if (($temp==(i64)2)) {
            value = (*b).value;
            regcode = aa_genss$getregcode((i64)(*a).reg,(i64)1,(i64)0);
            aa_genss$setopsize((struct aa_decls$opndrec *)a);
            if ((!!((*b).labeldef) && ((i64)(*a).size <= (i64)2))) {
                aa_lib$gerror((byte*)"mov imm?");
            }
;
                        {i64 $temp = (i64)(*a).size;
if (($temp==(i64)1)) {
                if (!(((i64)-128<=value && value<=(i64)255))) {
                    aa_lib$gerror((byte*)"exceeding byte value");
                }
;
                aa_genss$genrex();
                aa_genss$genbyte(((i64)176 + regcode));
                aa_genss$genbyte(value);
            }
            else if (($temp==(i64)2)) {
                if (!(((i64)-32768<=value && value<=(i64)65535))) {
                    aa_lib$gerror((byte*)"exceeding word16 value");
                }
;
                aa_genss$genbyte((i64)102);
                aa_genss$genrex();
                aa_genss$genbyte(((i64)184 + regcode));
                aa_genss$genword(value);
            }
            else if (($temp==(i64)4)) {
                if (!!((*b).labeldef)) {
                    aa_genss$genrex();
                    aa_genss$genbyte(((i64)184 + regcode));
                    aa_genss$genopnd((struct aa_decls$opndrec *)b,(i64)4);
                }
                else {
                    if (!(((i64)-2147483648<=value && value<=(i64)(u32)(i64)4294967295))) {
                        msysc$m_print_startcon();
                        msysc$m_print_i64(value,NULL);
                        msysc$m_print_ptr((void *)value,NULL);
                        msysc$m_print_newline();
                        msysc$m_print_end();
                        ;
                        aa_lib$gerror((byte*)"1:exceeding word32 value");
                    }
;
                    //doreg32:
L372 :;
;
                    aa_genss$genrex();
                    aa_genss$genbyte(((i64)184 + regcode));
                    aa_genss$gendword(value);
                }
;
            }
            else {
                if (!!((*b).labeldef)) {
                    aa_genss$rex |= (u8)8u;
                    aa_genss$genrex();
                    aa_genss$genbyte(((i64)184 + regcode));
                    aa_genss$genopnd((struct aa_decls$opndrec *)b,(i64)8);
                }
                else {
                    if (((value >= (i64)0) && (value <= (i64)4294967295))) {
                        aa_genss$rex = msysc$m_setdotindex(aa_genss$rex,(i64)3,(u64)0u);
                        goto L372 ;
;
                    }
;
                    aa_genss$rex |= (u8)8u;
                    aa_genss$genrex();
                    aa_genss$genbyte(((i64)184 + regcode));
                    aa_genss$genqword(value);
                }
;
            }
            };
        }
        else {
            aa_lib$gerror((byte*)"MOV REG/??");
        }
        };
    }
    else if (($temp==(i64)3)) {
                {i64 $temp = (i64)(*b).mode;
if (($temp==(i64)1)) {
            if (((i64)(*a).size == (i64)0)) {
                (*a).size = (i64)(*b).size;
            }
;
            if ((((i64)(*a).size != (i64)(*b).size) && !!((i64)(*a).size))) {
                aa_lib$gerror((byte*)"Opnd size mismatch");
            }
;
            aa_genss$genrrm((((i64)(*b).size == (i64)1) ? (i64)136 : (i64)137),(struct aa_decls$opndrec *)b,(struct aa_decls$opndrec *)a);
        }
        else if (($temp==(i64)2)) {
            value = (*b).value;
            if (((i64)(*a).size == (i64)0)) {
                (*a).size = (i64)1;
            }
;
            if ((!!((*b).labeldef) && ((i64)(*a).size <= (i64)2))) {
                aa_lib$gerror((byte*)"mov imm?");
            }
;
            aa_genss$setopsize((struct aa_decls$opndrec *)a);
            opc = (((i64)(*a).size == (i64)1) ? (i64)198 : (i64)199);
            if (!(!!((*b).labeldef))) {
                aa_genss$checkimmrange(value,(i64)(*a).size);
            }
;
            aa_genss$genxrm(opc,(i64)0,(struct aa_decls$opndrec *)a);
            value = (*b).value;
                        {i64 $temp = (i64)(*a).size;
if (($temp==(i64)1)) {
                aa_genss$genbyte(value);
            }
            else if (($temp==(i64)2)) {
                aa_genss$genword(value);
            }
            else if (($temp==(i64)4) || ($temp==(i64)8)) {
                aa_genss$genopnd((struct aa_decls$opndrec *)b,(i64)4);
            }
            };
        }
        else {
            aa_lib$gerror((byte*)"MOV MEM/?");
        }
        };
    }
    else {
        aa_lib$gerror((byte*)"MOV ?/..");
    }
    };
}

static void aa_genss$do_arith(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 code) {
        i64 opc;
        i64 dispsize;
        i64 x;
        {i64 $temp = (i64)(*a).mode;
if (($temp==(i64)1)) {
                {i64 $temp = (i64)(*b).mode;
if (($temp==(i64)1) || ($temp==(i64)3)) {
            opc = ((code << (i64)3) | (((i64)(*a).size == (i64)1) ? (i64)2 : (i64)3));
            aa_genss$genrrm(opc,(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
        }
        else if (($temp==(i64)2)) {
            //doregimm:
L373 :;
;
            if (!!((*b).labeldef)) {
                if (((i64)(*a).size < (i64)4)) {
                    aa_lib$gerror((byte*)"add imm/size");
                }
;
                aa_genss$genxrm((i64)129,code,(struct aa_decls$opndrec *)a);
                aa_genss$genopnd((struct aa_decls$opndrec *)b,(i64)4);
                return;
            }
;
            x = (*b).value;
            dispsize = (i64)1;
            if (((i64)(*a).size == (i64)1)) {
                opc = (i64)128;
                aa_genss$checkimmrange(x,(i64)1);
                if (!((x >= (i64)-128 && x <= (i64)255))) {
                    aa_lib$gerror((byte*)"Exceeding i8/u8 range");
                }
;
            }
            else if (((i64)-128<=x && x<=(i64)127)) {
                opc = (i64)131;
            }
            else {
                aa_genss$checkimmrange(x,(i64)4);
                opc = (i64)129;
                dispsize = (((i64)(*a).size == (i64)2) ? (i64)2 : (i64)4);
            }
;
            aa_genss$genxrm(opc,code,(struct aa_decls$opndrec *)a);
            if ((dispsize==(i64)1)) {
                aa_genss$genbyte(x);
            }
            else if ((dispsize==(i64)2)) {
                aa_genss$genword(x);
            }
            else if ((dispsize==(i64)4)) {
                aa_genss$gendword(x);
            }
;
        }
        else {
            aa_lib$gerror((byte*)"ADD reg,???");
        }
        };
    }
    else if (($temp==(i64)3)) {
                {i64 $temp = (i64)(*b).mode;
if (($temp==(i64)1)) {
            opc = ((code << (i64)3) | (((i64)(*b).size == (i64)1) ? (i64)0 : (i64)1));
            aa_genss$genrrm(opc,(struct aa_decls$opndrec *)b,(struct aa_decls$opndrec *)a);
        }
        else if (($temp==(i64)2)) {
            goto L373 ;
;
        }
        else {
            aa_lib$gerror((byte*)"ADD mem,???");
        }
        };
    }
    else {
        msysc$m_print_startcon();
        msysc$m_print_str(aa_genss$opnames[(code)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        aa_lib$gerror((byte*)"Can't add to this opnd");
    }
    };
}

static void aa_genss$do_movxmm(struct aa_decls$opndrec *a,struct aa_decls$opndrec *b,i64 size) {
    if (((i64)(*b).size == (i64)0)) {
        (*b).size = size;
    }
;
        {i64 $temp = (i64)(*a).mode;
if (($temp==(i64)1)) {
                {i64 $temp = (i64)(*b).mode;
if (($temp==(i64)5)) {
            if (((i64)(*a).size != size)) {
                aa_lib$gerror((byte*)"1:movdq size");
            }
;
            (*b).size = (i64)(*a).size;
            aa_genss$sizeoverride = (i64)1;
            aa_genss$genrrm((i64)3966,(struct aa_decls$opndrec *)b,(struct aa_decls$opndrec *)a);
        }
        else {
            aa_lib$gerror((byte*)"movdq reg,?");
        }
        };
    }
    else if (($temp==(i64)5)) {
                {i64 $temp = (i64)(*b).mode;
if (($temp==(i64)1)) {
            (*a).size = (i64)(*b).size;
            if (((i64)(*b).size != size)) {
                aa_lib$gerror((byte*)"3:movdq size");
            }
;
            aa_genss$sizeoverride = (i64)1;
            aa_genss$genrrm((i64)3950,(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
        }
        else if (($temp==(i64)5)) {
            (*a).size = (i64)(*b).size;
            aa_genss$f3override = (i64)1;
            aa_genss$genrrm((i64)3966,(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
        }
        else if (($temp==(i64)3)) {
            (*a).size = (i64)(*b).size;
            if (((i64)(*b).size != size)) {
                aa_lib$gerror((byte*)"3:movdq size");
            }
;
            if ((size == (i64)4)) {
                aa_genss$sizeoverride = (i64)1;
                aa_genss$nowmask = (i64)1;
                aa_genss$genrrm((i64)3950,(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
            }
            else {
                aa_genss$f3override = (i64)1;
                aa_genss$nowmask = (i64)1;
                aa_genss$genrrm((i64)3966,(struct aa_decls$opndrec *)a,(struct aa_decls$opndrec *)b);
            }
;
        }
        else {
            aa_lib$gerror((byte*)"movdq xreg,?");
        }
        };
    }
    else if (($temp==(i64)3)) {
                {i64 $temp = (i64)(*b).mode;
if (($temp==(i64)5)) {
            if ((!!((i64)(*a).size) && ((i64)(*a).size != size))) {
                aa_lib$gerror((byte*)"5:movdq size");
            }
;
            aa_genss$sizeoverride = (i64)1;
            aa_genss$genrrm(((size == (i64)4) ? (i64)3966 : (i64)4054),(struct aa_decls$opndrec *)b,(struct aa_decls$opndrec *)a);
        }
        else {
            aa_lib$gerror((byte*)"movdq mem,?");
        }
        };
    }
    else {
        aa_lib$gerror((byte*)"movdq opnds");
    }
    };
}

static void aa_genss$checksize(struct aa_decls$opndrec *a,i64 size1,i64 size2) {
    if (((i64)(*a).size == (i64)0)) {
        aa_lib$gerror((byte*)"Need size");
    }
;
    if ((!!(size1) && !(((i64)(*a).size == size1 || (i64)(*a).size == size2)))) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"A.SIZE=",NULL);
        msysc$m_print_i64((i64)(*a).size,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        aa_lib$gerror((byte*)"Wrong size");
    }
;
}

// START
void aa_genss$start(void) {

}

void aa_lib$initlib(void) {
        i64 reg;
        i64 size;
    aa_lib$zero_opnd = (struct aa_decls$opndrec *)aa_lib$genint((i64)0,(i64)4);
    for (reg=(i64)1;reg<=(i64)16;++reg) {
L374 :;
        for (size=(i64)1;size<=(i64)8;++size) {
L377 :;
            if ((size==(i64)1) || (size==(i64)2) || (size==(i64)4) || (size==(i64)8)) {
                aa_lib$regtable[(reg)-1][(size)-1] = (struct aa_decls$opndrec *)aa_lib$genreg0(reg,size);
            }
;
L378 :;
        }
L379 :;
        ;
L375 :;
    }
L376 :;
    ;
    for (reg=(i64)17;reg<=(i64)20;++reg) {
L380 :;
        aa_lib$regtable[(reg)-1][((i64)1)-1] = (struct aa_decls$opndrec *)aa_lib$genreg0(reg,(i64)1);
L381 :;
    }
L382 :;
    ;
    aa_decls$ss_symboltable = (struct aa_decls$strec *(*)[])mlib$pcm_alloc((i64)131072);
    aa_decls$ss_symboltablesize = (i64)16384;
    aa_decls$ss_nsymbols = (i64)0;
}

void aa_lib$genmc(i64 opcode,struct aa_decls$opndrec *a,struct aa_decls$opndrec *b) {
        struct aa_lib$mclrec *  m;
        i64 nopnds;
    m = (struct aa_lib$mclrec *)mlib$pcm_alloc((i64)32);
    (*m).nextmcl = 0;
    if ((aa_lex$lxsymbol == (i64)11)) {
        (*m).lineno = (aa_decls$lxlineno - (i64)1);
    }
    else {
        (*m).lineno = aa_decls$lxlineno;
    }
;
    (*m).opcode = opcode;
    nopnds = ((a == 0) ? (i64)0 : ((b == 0) ? (i64)1 : (i64)2));
    if (((nopnds == (i64)2) && (opcode == (i64)90 || opcode == (i64)91))) {
        nopnds = (i64)3;
    }
;
    if ((nopnds < (i64)aa_tables$mclnopnds[(opcode)-1])) {
        aa_lib$serror((byte*)"Too few operands");
    }
    else if ((nopnds > (i64)aa_tables$mclnopnds[(opcode)-1])) {
        aa_lib$serror((byte*)"Too many operands");
    }
;
    (*m).a = (struct aa_decls$opndrec *)a;
    (*m).b = (struct aa_decls$opndrec *)b;
    if (!!(aa_lib$mccode)) {
        (*aa_lib$mccodex).nextmcl = (struct aa_lib$mclrec *)m;
        aa_lib$mccodex = (struct aa_lib$mclrec *)m;
    }
    else {
        aa_lib$mccode = (aa_lib$mccodex = (struct aa_lib$mclrec *)m);
    }
;
}

void aa_lib$genmcstr(i64 opcode,u8 *s) {
    aa_lib$genmc(opcode,(struct aa_decls$opndrec *)aa_lib$genstrimm(s),0);
}

static struct aa_decls$opndrec *aa_lib$newopnd(i64 mode) {
        struct aa_decls$opndrec *  a;
    a = (struct aa_decls$opndrec *)mlib$pcm_allocz((i64)24);
    (*a).mode = mode;
    return (struct aa_decls$opndrec *)a;
}

struct aa_decls$opndrec *aa_lib$genxreg(i64 xreg) {
        struct aa_decls$opndrec *  a;
    a = (struct aa_decls$opndrec *)aa_lib$newopnd((i64)5);
    (*a).reg = xreg;
    (*a).size = (i64)16;
    return (struct aa_decls$opndrec *)a;
}

struct aa_decls$opndrec *aa_lib$genindex(i64 areg,i64 ireg,i64 scale,struct aa_decls$opndrec *x,i64 size,i64 addrsize) {
        struct aa_decls$opndrec *  a;
    if (!!(x)) {
        a = (struct aa_decls$opndrec *)x;
        (*x).mode = (i64)3;
    }
    else {
        a = (struct aa_decls$opndrec *)aa_lib$newopnd((i64)3);
    }
;
    (*a).reg = areg;
    (*a).regix = ireg;
    (*a).scale = scale;
    (*a).size = size;
    (*a).addrsize = addrsize;
    return (struct aa_decls$opndrec *)a;
}

struct mlib$strbuffer *aa_lib$writemclblock(void) {
        i64 i;
        struct aa_lib$mclrec *  m;
    mlib$gs_init((struct mlib$strbuffer *)aa_lib$dest);
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(byte*)"MC CODE");
    m = (struct aa_lib$mclrec *)aa_lib$mccode;
    i = (i64)1;
    L383 :;
    while (!!(m)) {
        aa_lib$writemcl(i,(struct aa_lib$mclrec *)m);
        m = (struct aa_lib$mclrec *)(*m).nextmcl;
        ++(i);
L384 :;
    }
L385 :;
    ;
    return (struct mlib$strbuffer *)aa_lib$dest;
}

void aa_lib$gencomment(u8 *s) {
    if ((s == 0)) {
        aa_lib$genmc((i64)2,0,0);
    }
    else {
        aa_lib$genmcstr((i64)1,s);
    }
;
}

struct aa_decls$opndrec *aa_lib$genstrimm(u8 *s) {
        struct aa_decls$opndrec *  a;
    a = (struct aa_decls$opndrec *)aa_lib$newopnd((i64)6);
    (*a).svalue = s;
    return (struct aa_decls$opndrec *)a;
}

static u8 *aa_lib$getsizetag(i64 size) {
    if ((size==(i64)1)) {
        return (byte*)"b";
    }
    else if ((size==(i64)2)) {
        return (byte*)"h";
    }
    else if ((size==(i64)4)) {
        return (byte*)"w";
    }
    else if ((size==(i64)8)) {
        return (byte*)"d";
    }
;
    aa_lib$gerror((byte*)"GETSIZETAG?");
    return (u8 *)0;
}

static void aa_lib$writemcl(i64 index,struct aa_lib$mclrec *mcl) {
        u8 mclstr[512];
        u8 str[512];
        u8 *  semi;
    strcpy((u8 *)mclstr,aa_lib$strmcl((struct aa_lib$mclrec *)mcl));
    if (((i64)(u64)mclstr[((i64)1)-1] == (i64)0)) {
        return;
    }
;
        {i64 $temp = (i64)(*mcl).opcode;
if (($temp==(i64)1)) {
        semi = (byte*)";";
    }
    else {
        semi = (byte*)" ";
    }
    };
    msysc$m_print_startstr(str);
    msysc$m_print_str(semi,(byte*)"z3");
    msysc$m_print_i64(index,(byte*)"z4");
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)" ",NULL);
    msysc$m_print_end();
    ;
    mlib$gs_str((struct mlib$strbuffer *)aa_lib$dest,(u8 *)str);
    mlib$gs_strln((struct mlib$strbuffer *)aa_lib$dest,(u8 *)mclstr);
}

u8 *aa_lib$strmcl(struct aa_lib$mclrec *mcl) {
        static u8 str[512];
        u8 str2[128];
        i64 opcode;
        i64 sizepref;
    opcode = (i64)(*mcl).opcode;
    if ((opcode==(i64)7)) {
        return (*(*mcl).a).svalue;
    }
    else if ((opcode==(i64)2)) {
        return (byte*)"";
    }
    else if ((opcode==(i64)1)) {
        strcpy((u8 *)str,(byte*)";");
        strcat((u8 *)str,(*(*mcl).a).svalue);
        return (u8 *)str;
    }
    else if ((opcode==(i64)4)) {
        strcpy((u8 *)str,(*(*(*mcl).a).labeldef).name);
        strcat((u8 *)str,(byte*)":");
        return (u8 *)str;
    }
;
    strcpy((u8 *)str,(byte*)"\t\t");
    if ((opcode==(i64)25)) {
        strcat((u8 *)str,(byte*)"j");
        strcat((u8 *)str,aa_tables$condnames[((*(*mcl).a).value)]);
    }
    else if ((opcode==(i64)59)) {
        strcat((u8 *)str,(byte*)"set");
        strcat((u8 *)str,aa_tables$condnames[((*(*mcl).a).value)]);
    }
    else if ((opcode==(i64)14)) {
        strcat((u8 *)str,(byte*)"cmov");
        strcat((u8 *)str,aa_tables$condnames[((*(*mcl).a).value)]);
    }
    else {
        strcat((u8 *)str,(aa_tables$mclnames[(opcode)-1] + (i64)2));
    }
;
    mlib$ipadstr((u8 *)str,(i64)12,(byte*)" ");
    if ((!!((*mcl).a) && !!((*mcl).b))) {
        sizepref = aa_lib$needsizeprefix((i64)(*mcl).opcode,(struct aa_decls$opndrec *)(*mcl).a,(struct aa_decls$opndrec *)(*mcl).b);
        strcat((u8 *)str,aa_lib$stropnd((struct aa_decls$opndrec *)(*mcl).a,sizepref));
        strcat((u8 *)str,(byte*)",\t");
        strcat((u8 *)str,aa_lib$stropnd((struct aa_decls$opndrec *)(*mcl).b,sizepref));
    }
    else if (!!((*mcl).a)) {
        if (((i64)(*mcl).opcode == (i64)20)) {
            strcat((u8 *)str,aa_lib$stropnd((struct aa_decls$opndrec *)(*mcl).a,(i64)0));
        }
        else {
            strcat((u8 *)str,aa_lib$stropnd((struct aa_decls$opndrec *)(*mcl).a,(i64)1));
        }
;
    }
;
    if ((opcode==(i64)90) || (opcode==(i64)91)) {
        msysc$m_print_startstr(str2);
        msysc$m_print_setfmt((byte*)", #");
        msysc$m_print_i64((i64)(*mcl).c,NULL);
        msysc$m_print_end();
        ;
        strcat((u8 *)str,(u8 *)str2);
    }
;
    return (u8 *)str;
}

u8 *aa_lib$stropnd(struct aa_decls$opndrec *a,i64 sizeprefix) {
        static u8 str[256];
        u8 *  plus;
        u8 *  s;
        i64 value;
        struct aa_decls$strec *  d;
        {i64 $temp = (i64)(*a).mode;
if (($temp==(i64)1)) {
        return aa_lib$getregname((i64)(*a).reg,(i64)(*a).size);
    }
    else if (($temp==(i64)2)) {
        d = (struct aa_decls$strec *)(*a).labeldef;
        value = (*a).value;
        if (!!(d)) {
            if (((i64)(*d).symbol == (i64)18)) {
                return aa_lib$inttostr((*(*d).expr).value);
            }
;
            s = aa_lib$getfullname((struct aa_decls$strec *)d);
            if (!!(value)) {
                if ((value > (i64)0)) {
                    strcpy((u8 *)str,s);
                    strcat((u8 *)str,(byte*)"+");
                    strcat((u8 *)str,aa_lib$inttostr(value));
                }
                else {
                    strcpy((u8 *)str,s);
                    strcat((u8 *)str,aa_lib$inttostr(value));
                }
;
                return (u8 *)str;
            }
            else {
                strcpy((u8 *)str,s);
                return (u8 *)str;
            }
;
        }
;
        if (((i64)(*a).valtype == (i64)0)) {
            return aa_lib$inttostr(value);
        }
        else {
            return aa_lib$realtostr(*(r64*)&value);
        }
;
    }
    else if (($temp==(i64)3)) {
        str[((i64)1)-1] = (u64)0u;
        strcat((u8 *)str,aa_lib$getsizeprefix((i64)(*a).size,sizeprefix));
        strcat((u8 *)str,(byte*)"[");
        plus = (byte*)"";
        if (!!((i64)(*a).reg)) {
            strcat((u8 *)str,aa_lib$getregname((i64)(*a).reg,(i64)(*a).addrsize));
            plus = (byte*)"+";
        }
;
        if (!!((i64)(*a).regix)) {
            strcat((u8 *)str,plus);
            strcat((u8 *)str,aa_lib$getregname((i64)(*a).regix,(i64)(*a).addrsize));
            plus = (byte*)"+";
            if (((i64)(*a).scale > (i64)1)) {
                strcat((u8 *)str,(byte*)"*");
                strcat((u8 *)str,aa_lib$inttostr((i64)(*a).scale));
            }
;
        }
;
        if (!!((*a).labeldef)) {
            strcat((u8 *)str,plus);
            strcat((u8 *)str,aa_lib$strdef((struct aa_decls$strec *)(*a).labeldef));
            plus = (byte*)"+";
        }
;
        if (((*a).value > (i64)0)) {
            strcat((u8 *)str,plus);
            strcat((u8 *)str,aa_lib$inttostr((*a).value));
        }
        else if (((*a).value < (i64)0)) {
            strcat((u8 *)str,aa_lib$inttostr((*a).value));
        }
;
        strcat((u8 *)str,(byte*)"]");
    }
    else if (($temp==(i64)6)) {
        if ((strlen((*a).svalue) >= (i64)256)) {
            msysc$m_print_startstr(str);
            msysc$m_print_str((byte*)"\"<Long string>\"",NULL);
            msysc$m_print_end();
            ;
        }
        else {
            msysc$m_print_startstr(str);
            msysc$m_print_str((byte*)"\"",NULL);
            msysc$m_print_nogap();
            msysc$m_print_str((*a).svalue,NULL);
            msysc$m_print_nogap();
            msysc$m_print_str((byte*)"\"",NULL);
            msysc$m_print_end();
            ;
        }
;
    }
    else if (($temp==(i64)4)) {
        return aa_lib$opndnames[((*a).value)];
    }
    else if (($temp==(i64)5)) {
        return aa_lib$xgetregname((i64)(*a).reg);
    }
    else {
        return (byte*)"<BAD OPND>";
    }
    };
    return (u8 *)str;
}

static u8 *aa_lib$strdef(struct aa_decls$strec *def) {
    if (((i64)(*def).symbol == (i64)18)) {
        return aa_lib$inttostr((*(*def).expr).value);
    }
;
    return aa_lib$getfullname((struct aa_decls$strec *)def);
}

void aa_lib$setsegment(i64 seg) {
    if ((seg == aa_lib$currsegment)) {
        return;
    }
;
    if ((seg==(i64)68)) {
        aa_lib$genmcstr((i64)118,(byte*)".data");
    }
    else if ((seg==(i64)90)) {
        aa_lib$genmcstr((i64)118,(byte*)".bss");
    }
    else if ((seg==(i64)67)) {
        aa_lib$genmcstr((i64)118,(byte*)".text");
    }
    else if ((seg==(i64)82)) {
        aa_lib$genmcstr((i64)118,(byte*)".rodata");
    }
;
    aa_lib$currsegment = seg;
}

static u8 *aa_lib$getsizeprefix(i64 size,i64 enable) {
    if (!(!!(enable))) {
        return (byte*)"";
    }
;
    if ((size==(i64)1)) {
        return (byte*)"byte ";
    }
    else if ((size==(i64)2)) {
        return (byte*)"word ";
    }
    else if ((size==(i64)4)) {
        return (byte*)"dword ";
    }
    else if ((size==(i64)8)) {
        return (byte*)"qword ";
    }
    else if ((size==(i64)0)) {
        return (byte*)"";
    }
;
    return (byte*)"N:";
}

static i64 aa_lib$needsizeprefix(i64 opcode,struct aa_decls$opndrec *a,struct aa_decls$opndrec *b) {
    if ((opcode==(i64)17) || (opcode==(i64)18)) {
        return (i64)1;
    }
    else if ((opcode==(i64)84) || (opcode==(i64)85)) {
        return (i64)1;
    }
;
    if ((((((i64)(*a).mode == (i64)1) || ((i64)(*a).mode == (i64)5)) || ((i64)(*b).mode == (i64)1)) || ((i64)(*b).mode == (i64)5))) {
        return (i64)0;
    }
;
    return (i64)1;
}

struct aa_decls$opndrec *aa_lib$genimm_expr(struct aa_decls$strec *d,i64 value,i64 t,i64 size) {
        struct aa_decls$opndrec *  a;
    a = (struct aa_decls$opndrec *)aa_lib$newopnd((i64)2);
    (*a).size = size;
    (*a).labeldef = (struct aa_decls$strec *)d;
    (*a).value = value;
    (*a).valtype = t;
    return (struct aa_decls$opndrec *)a;
}

struct aa_decls$opndrec *aa_lib$genint(i64 x,i64 size) {
        struct aa_decls$opndrec *  a;
    a = (struct aa_decls$opndrec *)aa_lib$newopnd((i64)2);
    (*a).size = size;
    (*a).value = x;
    return (struct aa_decls$opndrec *)a;
}

struct aa_decls$opndrec *aa_lib$genlab(struct aa_decls$strec *d,i64 size) {
        struct aa_decls$opndrec *  a;
    a = (struct aa_decls$opndrec *)aa_lib$newopnd((i64)2);
    (*a).size = size;
    (*a).labeldef = (struct aa_decls$strec *)d;
    return (struct aa_decls$opndrec *)a;
}

struct aa_decls$opndrec *aa_lib$genmem(struct aa_decls$strec *d,i64 size) {
        struct aa_decls$opndrec *  a;
    a = (struct aa_decls$opndrec *)aa_lib$genlab((struct aa_decls$strec *)d,size);
    (*a).mode = (i64)3;
    return (struct aa_decls$opndrec *)a;
}

struct aa_decls$opndrec *aa_lib$genreg0(i64 reg,i64 size) {
        struct aa_decls$opndrec *  a;
    a = (struct aa_decls$opndrec *)aa_lib$newopnd((i64)1);
    (*a).reg = reg;
    (*a).size = size;
    return (struct aa_decls$opndrec *)a;
}

u8 *aa_lib$getfullname(struct aa_decls$strec *d) {
        static u8 str[256];
        u8 *  ms;
    ms = (byte*)"";
    if (!!((*d).basedef)) {
        ms = (*(*d).basedef).name;
    }
;
    msysc$m_print_startstr(str);
    msysc$m_print_setfmt((byte*)"<# : ## &:# SYM:## M:#>");
    msysc$m_print_str((*d).name,NULL);
    msysc$m_print_str((byte*)"#",NULL);
    msysc$m_print_i64((i64)(*d).moduleno,NULL);
    msysc$m_print_ptr(d,(byte*)"8");
    msysc$m_print_i64((strlen(aa_tables$symbolnames[((i64)(*d).symbol)-1]) - (i64)3),(byte*)"v");
    msysc$m_print_str(aa_tables$symbolnames[((i64)(*d).symbol)-1],(byte*)".*");
    msysc$m_print_str(ms,NULL);
    msysc$m_print_end();
    ;
    return (u8 *)str;
    return (*d).name;
}

u8 *aa_lib$getregname(i64 reg,i64 size) {
        u8 *  prefix;
        u8 *  rs;
        static u8 str[32];
    if ((reg==(i64)0)) {
        return (byte*)"-";
    }
    else if ((reg==(i64)15)) {
        rs = (byte*)"frame";
    }
    else if ((reg==(i64)16)) {
        rs = (byte*)"stack";
    }
    else {
        rs = aa_lib$inttostr((reg - (i64)1));
    }
;
    if ((size==(i64)1)) {
        prefix = (byte*)"B";
    }
    else if ((size==(i64)2)) {
        prefix = (byte*)"W";
    }
    else if ((size==(i64)4)) {
        prefix = (byte*)"A";
    }
    else {
        prefix = (byte*)"D";
    }
;
    strcpy((u8 *)str,prefix);
    strcat((u8 *)str,rs);
    return (u8 *)str;
}

u8 *aa_lib$xgetregname(i64 reg) {
        static u8 str[16];
    msysc$m_print_startstr(str);
    msysc$m_print_str((byte*)"xmm",NULL);
    msysc$m_print_nogap();
    msysc$m_print_i64((reg - (i64)1),NULL);
    msysc$m_print_end();
    ;
    return (u8 *)str;
}

void aa_lib$printst(void *f) {
        struct aa_decls$strec *  r;
    r = (struct aa_decls$strec *)aa_decls$modulenamelist;
    L386 :;
    while (!!(r)) {
        aa_lib$printstrec(f,(struct aa_decls$strec *)r);
        r = (struct aa_decls$strec *)(*r).nextdef;
L387 :;
    }
L388 :;
    ;
}

void aa_lib$printstrec(void *f,struct aa_decls$strec *d) {
        {i64 $temp = (i64)(*d).symbol;
if (($temp==(i64)19) || ($temp==(i64)20) || ($temp==(i64)22)) {
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"Label:       ",NULL);
        msysc$m_print_str(mlib$padstr((*d).name,(i64)16,(byte*)" "),NULL);
        msysc$m_print_str((((i64)(*d).scope == (i64)1) ? (byte*)"U" : (byte*)"-"),NULL);
        msysc$m_print_str(aa_tables$symbolnames[((i64)(*d).symbol)-1],NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)"\t",NULL);
        msysc$m_print_nogap();
        msysc$m_print_str(mlib$padstr((!!((i64)(*d).segment) ? aa_mcxdecls$segmentnames[((i64)(*d).segment)-1] : (byte*)"no seg"),(i64)12,(byte*)" "),NULL);
        msysc$m_print_i64((i64)(*d).offset,NULL);
        msysc$m_print_ptr((*d).fwdrefs,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)21)) {
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"Label:       ",NULL);
        msysc$m_print_str(mlib$padstr((*d).name,(i64)16,(byte*)" "),NULL);
        msysc$m_print_str((byte*)"EXTERN",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)18)) {
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"Named const: ",NULL);
        msysc$m_print_str(mlib$padstr((*d).name,(i64)16,(byte*)" "),NULL);
        msysc$m_print_str((byte*)"=",NULL);
        msysc$m_print_str(aa_lib$stropnd((struct aa_decls$opndrec *)(*d).expr,(i64)0),NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
    else {
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"??",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
    };
}

void aa_lib$adddef(struct aa_decls$strec *d) {
    (*d).nextdef = (struct aa_decls$strec *)aa_decls$modulenamelist;
    aa_decls$modulenamelist = (struct aa_decls$strec *)d;
}

void aa_lib$addimport(struct aa_decls$strec *d) {
        struct aa_decls$stlistrec *  p;
    p = (struct aa_decls$stlistrec *)mlib$pcm_alloc((i64)16);
    (*p).def = (struct aa_decls$strec *)d;
    (*p).nextitem = (struct aa_decls$stlistrec *)aa_decls$globalimportlist;
    aa_decls$globalimportlist = (struct aa_decls$stlistrec *)p;
}

void aa_lib$createlabel(struct aa_decls$strec *symptr,i64 symbol) {
    (*symptr).symbol = symbol;
    (*symptr).stindex = (i64)0;
    (*symptr).moduleno = aa_decls$currmoduleno;
    aa_lib$adddef((struct aa_decls$strec *)symptr);
}

void aa_lib$createnamedconst(struct aa_decls$strec *symptr,struct aa_decls$opndrec *expr) {
    (*symptr).symbol = (i64)18;
    (*symptr).expr = (struct aa_decls$opndrec *)expr;
    aa_lib$adddef((struct aa_decls$strec *)symptr);
}

void aa_lib$createregalias(struct aa_decls$strec *symptr,i64 regindex,i64 regsize) {
    (*symptr).symbol = (i64)24;
    (*symptr).ksymbol = (i64)24;
    (*symptr).subcode = regindex;
    (*symptr).regsize = regsize;
    aa_lib$adddef((struct aa_decls$strec *)symptr);
}

void aa_lib$createxregalias(struct aa_decls$strec *symptr,i64 regindex) {
    (*symptr).symbol = (i64)25;
    (*symptr).ksymbol = (i64)25;
    (*symptr).subcode = regindex;
    aa_lib$adddef((struct aa_decls$strec *)symptr);
}

void aa_lib$gerror(u8 *mess) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"\nSS code gen error:",NULL);
    msysc$m_print_str(mess,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"On line:",NULL);
    msysc$m_print_i64(aa_decls$alineno,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    exit((i64)1);
}

void aa_lib$serror(u8 *mess) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"\nSyntax error: '",NULL);
    msysc$m_print_nogap();
    msysc$m_print_str(mess,NULL);
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)"' on line",NULL);
    msysc$m_print_i64(aa_decls$lxlineno,NULL);
    msysc$m_print_str(aa_decls$moduletable[(aa_decls$currmoduleno)-1].name,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    exit((i64)1);
}

void aa_lib$serror_s(u8 *mess,u8 *param) {
        u8 str[256];
    sprintf((u8 *)str,mess,param);
    aa_lib$serror((u8 *)str);
}

static u8 *aa_lib$inttostr(i64 a) {
        static u8 str[64];
    msysc$getstrint(a,(u8 *)str);
    return (u8 *)str;
}

static u8 *aa_lib$realtostr(r64 a) {
        static u8 str[64];
    strcpy((u8 *)str,msysc$strreal(a,0));
    return (u8 *)str;
}

struct aa_decls$dbuffer *aa_lib$buffercreate(i64 size) {
        struct aa_decls$dbuffer *  a;
    a = (struct aa_decls$dbuffer *)mlib$pcm_alloc((i64)32);
    (*a).alloc = size;
    (*a).pstart = ((*a).pcurr = (byte *)mlib$pcm_alloc((*a).alloc));
    (*a).pend = ((*a).pstart + (*a).alloc);
    return (struct aa_decls$dbuffer *)a;
}

static void aa_lib$bufferexpand(struct aa_decls$dbuffer *a) {
        i64 newalloc;
        i64 usedbytes;
        byte *  p;
    newalloc = ((*a).alloc * (i64)2);
    usedbytes = ((*a).pcurr - (*a).pstart);
    if ((usedbytes > (*a).alloc)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"dbuffer error",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startcon();
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startcon();
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startcon();
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        exit(0);
    }
;
    p = (byte *)mlib$pcm_alloc(newalloc);
    memcpy(p,(*a).pstart,(u64)usedbytes);
    (*a).pstart = p;
    (*a).pcurr = (p + usedbytes);
    (*a).alloc = newalloc;
    (*a).pend = (p + newalloc);
}

void aa_lib$buffercheck(struct aa_decls$dbuffer *a,i64 n) {
    L389 :;
    while ((((*a).pend - (*a).pcurr) < n)) {
        aa_lib$bufferexpand((struct aa_decls$dbuffer *)a);
L390 :;
    }
L391 :;
    ;
}

i64 aa_lib$bufferlength(struct aa_decls$dbuffer *a) {
    return ((*a).pcurr - (*a).pstart);
}

void *aa_lib$bufferelemptr(struct aa_decls$dbuffer *a,i64 offset) {
    return ((*a).pstart + offset);
}

void aa_lib$addbyte(struct aa_decls$dbuffer *a,i64 x) {
    (*(*a).pcurr) = x;
    ++((*a).pcurr);
}

void aa_lib$addword(struct aa_decls$dbuffer *a,i64 x) {
    (*(*a).pcurr16) = x;
    ++((*a).pcurr16);
}

void aa_lib$adddword(struct aa_decls$dbuffer *a,i64 x) {
    (*(*a).pcurr32) = x;
    ++((*a).pcurr32);
}

void aa_lib$addqword(struct aa_decls$dbuffer *a,i64 x) {
    (*(*a).pcurr64) = (u64)x;
    ++((*a).pcurr64);
}

void aa_lib$printmodulesymbols(void *f) {
        struct aa_decls$strec *  d;
        struct aa_decls$strec *  e;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"MODULE SYMBOLS IN",NULL);
    msysc$m_print_str(aa_decls$moduletable[(aa_decls$currmoduleno)-1].name,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    d = (struct aa_decls$strec *)aa_decls$modulenamelist;
    L392 :;
    while (!!(d)) {
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"   ",NULL);
        msysc$m_print_nogap();
        msysc$m_print_str(mlib$padstr((*d).name,(i64)14,(byte*)" "),NULL);
        msysc$m_print_str(mlib$padstr(aa_tables$symbolnames[((i64)(*d).symbol)-1],(i64)12,(byte*)" "),NULL);
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_setfmt((byte*)"|| # # #");
        msysc$m_print_i64((i64)(*d).htfirstindex,(byte*)"6");
        msysc$m_print_i64((i64)(*d).htindex,(byte*)"6");
        msysc$m_print_ptr(d,(byte*)"8H");
        msysc$m_print_end();
        ;
        e = (struct aa_decls$strec *)aa_decls$dupltable[((i64)(*d).htfirstindex)];
        if (!!(e)) {
            msysc$m_print_startfile(f);
            msysc$m_print_str((byte*)"||",NULL);
            msysc$m_print_end();
            ;
            L395 :;
            while (!!(e)) {
                msysc$m_print_startfile(f);
                msysc$m_print_str((byte*)"(",NULL);
                msysc$m_print_nogap();
                msysc$m_print_str((*e).name,NULL);
                msysc$m_print_nogap();
                msysc$m_print_str((byte*)")",NULL);
                msysc$m_print_end();
                ;
                e = (struct aa_decls$strec *)(*e).nextdupl;
L396 :;
            }
L397 :;
            ;
        }
;
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)" BASE:",NULL);
        msysc$m_print_str((!!((*d).basedef) ? (*(*d).basedef).name : (byte*)""),NULL);
        msysc$m_print_ptr((*d).basedef,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        d = (struct aa_decls$strec *)(*d).nextdef;
L393 :;
    }
L394 :;
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

void aa_lib$printimportsymbols(void *f) {
        struct aa_decls$strec *  d;
        struct aa_decls$stlistrec *  p;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"GLOBAL IMPORT TABLE",NULL);
    msysc$m_print_ptr(aa_decls$globalimportlist,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    p = (struct aa_decls$stlistrec *)aa_decls$globalimportlist;
    L398 :;
    while (!!(p)) {
        d = (struct aa_decls$strec *)(*p).def;
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"   ",NULL);
        msysc$m_print_nogap();
        msysc$m_print_str(mlib$padstr((*d).name,(i64)14,(byte*)" "),NULL);
        msysc$m_print_str(mlib$padstr(aa_tables$symbolnames[((i64)(*d).symbol)-1],(i64)12,(byte*)" "),NULL);
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"D.OFFSET=",NULL);
        msysc$m_print_i64((i64)(*d).offset,NULL);
        msysc$m_print_str(aa_tables$reftypenames[((i64)(*d).reftype)],NULL);
        msysc$m_print_ptr(d,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        p = (struct aa_decls$stlistrec *)(*p).nextitem;
L399 :;
    }
L400 :;
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

void aa_lib$printdupltable(void *f) {
        u8 str[256];
        struct aa_decls$strec *  d;
        i64 i;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"DUPL TABLE",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)0;i<=(i64)65535;++i) {
L401 :;
        if (!!(aa_decls$dupltable[(i)])) {
            d = (struct aa_decls$strec *)aa_decls$dupltable[(i)];
            msysc$m_print_startfile(f);
            msysc$m_print_str((byte*)"\t",NULL);
            msysc$m_print_i64((i64)(*d).htfirstindex,NULL);
            msysc$m_print_nogap();
            msysc$m_print_str((byte*)":",NULL);
            msysc$m_print_end();
            ;
            L404 :;
            while (!!(d)) {
                msysc$m_print_startstr(str);
                msysc$m_print_setfmt((byte*)"(# # (#) #) ");
                msysc$m_print_i64((i64)(*d).htindex,(byte*)"6");
                msysc$m_print_str((*d).name,NULL);
                msysc$m_print_str(aa_decls$moduletable[((i64)(*d).moduleno)-1].name,NULL);
                msysc$m_print_ptr(d,(byte*)"8H");
                msysc$m_print_end();
                ;
                d = (struct aa_decls$strec *)(*d).nextdupl;
L405 :;
            }
L406 :;
            ;
            msysc$m_print_startfile(f);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
;
L402 :;
    }
L403 :;
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

// START
void aa_lib$start(void) {

}

void msysc$m_init(i64 nargs,u8 *(*args)[],u8 *(*envstrings)[]) {
        i64 j;
        i64 i;
    msysc$nsysparams = nargs;
    if ((msysc$nsysparams > (i64)128)) {
        printf((byte*)"Too many params\n");
        exit((i64)1);
    }
;
    for (i=(i64)1;i<=nargs;++i) {
L407 :;
        msysc$sysparams[(i)-1] = (*args)[(i)-1];
L408 :;
    }
L409 :;
    ;
    msysc$ncmdparams = (msysc$nsysparams - (msysc$$cmdskip + (i64)1));
    msysc$cmdparams = (u8 *(*)[])&msysc$sysparams[((msysc$$cmdskip + (i64)1))-1];
    j = (i64)1;
    msysc$nenvstrings = (i64)0;
}

i64 msysc$m_getdotindex(u64 a,i64 i) {
    return (((i64)a & ((i64)1 << i)) >> i);
}

u64 msysc$m_setdotindex(u64 a,i64 i,i64 x) {
    return (u64)(((i64)a & ~(((i64)1 << i))) | (i64)((u64)x << i));
}

i64 msysc$m_getdotslice(u64 a,i64 i,i64 j) {
    if ((i >= j)) {
        return (i64)((a >> j) & ~(((u64)18446744073709551615u << ((i - j) + (i64)1))));
    }
    else {
        return (i64)((a >> i) & ~(((u64)18446744073709551615u << ((j - i) + (i64)1))));
    }
;
}

u64 msysc$m_setdotslice(u64 a,i64 i,i64 j,u64 x) {
        u64 mask64;
    if ((i > j)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"SETDOTSLICE?",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        exit((i64)52);
    }
;
    mask64 = (~(((u64)18446744073709551615u << ((j - i) + (i64)1))) << i);
    return ((a & ~(mask64)) | (x << i));
}

i64 msysc$m_get_nprocs(void) {
    return msysc$_fnnprocs;
}

i64 msysc$m_get_nexports(void) {
    return msysc$_fnnexports;
}

u8 *msysc$m_get_procname(i64 n) {
    return msysc$_fnnames[(n)-1];
}

void *msysc$m_get_procaddr(i64 n) {
    return msysc$_fnaddresses[(n)-1];
}

void *msysc$m_get_procexport(i64 n) {
    return 0;
}

static void msysc$pushio(void) {
    if ((msysc$niostack >= (i64)10)) {
        printf((byte*)"Too many io levels\n");
        exit((i64)53);
    }
;
    ++(msysc$niostack);
    msysc$outchan_stack[(msysc$niostack)-1] = msysc$outchan;
    msysc$outdev_stack[(msysc$niostack)-1] = msysc$outdev;
    msysc$fmtstr_stack[(msysc$niostack)-1] = msysc$fmtstr;
    msysc$needgap_stack[(msysc$niostack)-1] = msysc$needgap;
    msysc$needgap = (i64)0;
    msysc$fmtstr = 0;
    msysc$outchan = 0;
}

void msysc$m_print_startfile(void *dev) {
    msysc$pushio();
    msysc$outchan = dev;
    if (!!(dev)) {
        msysc$outdev = (i64)2;
    }
    else {
        msysc$outdev = (i64)1;
    }
;
}

void msysc$m_print_startstr(u8 *s) {
        u8 **  p;
    msysc$pushio();
    msysc$ptr_stack[(msysc$niostack)-1] = s;
    p = &msysc$ptr_stack[(msysc$niostack)-1];
    msysc$outchan = p;
    msysc$outdev = (i64)3;
}

void msysc$m_print_startptr(u8 **p) {
    msysc$pushio();
    msysc$outchan = p;
    msysc$outdev = (i64)3;
}

void msysc$m_print_startcon(void) {
    msysc$pushio();
    msysc$outdev = (i64)1;
}

void msysc$m_print_setfmt(u8 *format) {
    msysc$fmtstr = format;
}

void msysc$m_print_end(void) {
    msysc$needgap = (i64)0;
    msysc$nextfmtchars((i64)1);
    if ((msysc$niostack == (i64)0)) {
        return;
    }
;
    msysc$outchan = msysc$outchan_stack[(msysc$niostack)-1];
    msysc$outdev = msysc$outdev_stack[(msysc$niostack)-1];
    msysc$fmtstr = msysc$fmtstr_stack[(msysc$niostack)-1];
    msysc$needgap = (i64)msysc$needgap_stack[(msysc$niostack)-1];
    --(msysc$niostack);
}

void msysc$m_print_ptr(void *a,u8 *fmtstyle) {
    msysc$nextfmtchars((i64)0);
    msysc$printstr(msysc$strword((u64)a,(byte*)"z8h"));
    msysc$needgap = (i64)1;
}

void msysc$m_print_i64(i64 a,u8 *fmtstyle) {
        u8 s[40];
        struct msysc$fmtrec fmt;
        i64 n;
    msysc$nextfmtchars((i64)0);
    if ((fmtstyle == 0)) {
        if ((a >= (i64)0)) {
            n = msysc$u64tostr((u64)a,(u8 *)s,(u64)10u,(i64)0);
        }
        else {
            s[((i64)1)-1] = '-';
            n = (msysc$u64tostr((u64)-(a),(u8 *)&s[((i64)2)-1],(u64)10u,(i64)0) + (i64)1);
        }
;
        msysc$printstr_n((u8 *)s,n);
    }
    else {
        msysc$strtofmt(fmtstyle,(i64)-1,&fmt);
        if (((u64)fmt.param == 'V')) {
            msysc$fmtparam = a;
            msysc$needgap = (i64)0;
        }
        else {
            msysc$tostr_i64(a,(struct msysc$fmtrec *)&fmt);
        }
;
    }
;
    msysc$needgap = (i64)1;
}

void msysc$m_print_u64(u64 a,u8 *fmtstyle) {
        struct msysc$fmtrec fmt;
    msysc$nextfmtchars((i64)0);
    if ((fmtstyle == 0)) {
        msysc$printstr(msysc$strword(a,0));
    }
    else {
        msysc$strtofmt(fmtstyle,(i64)-1,&fmt);
        msysc$tostr_u64(a,(struct msysc$fmtrec *)&fmt);
    }
;
    msysc$needgap = (i64)1;
}

void msysc$m_print_r64(r64 x,u8 *fmtstyle) {
        u8 s[360];
        struct msysc$fmtrec fmt;
    msysc$nextfmtchars((i64)0);
    if ((fmtstyle == 0)) {
        sprintf((u8 *)s,(byte*)"%f",x);
        msysc$printstr((u8 *)s);
    }
    else {
        msysc$strtofmt(fmtstyle,(i64)-1,&fmt);
        msysc$tostr_r64(x,(struct msysc$fmtrec *)&fmt);
    }
;
    msysc$needgap = (i64)1;
}

void msysc$m_print_r32(r32 x,u8 *fmtstyle) {
    msysc$m_print_r64((r64)x,fmtstyle);
}

void msysc$m_print_c8(i64 a,u8 *fmtstyle) {
        u8 s[40];
    msysc$nextfmtchars((i64)0);
    s[((i64)1)-1] = (u64)a;
    s[((i64)2)-1] = (u64)0u;
    msysc$printstr((u8 *)s);
    msysc$needgap = (i64)1;
}

void msysc$m_print_str(u8 *s,u8 *fmtstyle) {
        struct msysc$fmtrec fmt;
    msysc$nextfmtchars((i64)0);
    if ((fmtstyle == 0)) {
        msysc$printstr(s);
    }
    else {
        msysc$strtofmt(fmtstyle,(i64)-1,&fmt);
        msysc$tostr_str(s,(struct msysc$fmtrec *)&fmt);
    }
;
    msysc$needgap = (i64)1;
}

void msysc$m_print_newline(void) {
    msysc$needgap = (i64)0;
    msysc$nextfmtchars((i64)1);
    msysc$printstr((byte*)"\r\n");
}

void msysc$m_print_nogap(void) {
    msysc$needgap = (i64)0;
}

void msysc$m_print_space(void) {
    msysc$needgap = (i64)0;
    msysc$printstr((byte*)" ");
}

void msysc$printstr(u8 *s) {
        u8 **  p;
    if ((msysc$outdev==(i64)1)) {
        printf((byte*)"%s",s);
    }
    else if ((msysc$outdev==(i64)2)) {
        fprintf(msysc$outchan,(byte*)"%s",s);
    }
    else if ((msysc$outdev==(i64)3)) {
        p = (u8 **)msysc$outchan;
        strcpy((*p),s);
        (*p) += strlen(s);
    }
;
}

void msysc$printstr_n(u8 *s,i64 n) {
        u8 str[256];
        u8 **  p;
    if ((n==(i64)-1)) {
        n = strlen(s);
    }
    else if ((n==(i64)0)) {
        return;
    }
;
    if ((msysc$outdev==(i64)3)) {
        p = (u8 **)msysc$outchan;
        memcpy((void *)(*p),(void *)s,(u64)n);
        (*p) += n;
        (*(*p)) = (u64)0u;
    }
    else if ((msysc$outdev==(i64)2)) {
        s = msysc$makezstring(s,n,(u8 *)str);
        fprintf(msysc$outchan,(byte*)"%s",s);
        msysc$freezstring(s,n);
    }
    else if ((msysc$outdev==(i64)1)) {
        s = msysc$makezstring(s,n,(u8 *)str);
        printf((byte*)"%s",s);
        msysc$freezstring(s,n);
    }
;
}

void msysc$printstrn_app(u8 *s,i64 length,void *f) {
    if (!!(length)) {
        if ((f == 0)) {
            printf((u8*)"%.*s",(i32)length,s);;
        }
        else {
            fprintf(f,(u8*)"%.*s",(i32)length,s);;
        }
;
    }
;
}

static u8 *msysc$makezstring(u8 *s,i64 n,u8 *local) {
        u8 *  t;
    if ((n < (i64)256)) {
        memcpy((void *)local,(void *)s,(u64)n);
        (*(local + n)) = (u64)0u;
        return local;
    }
    else {
        t = (u8 *)mlib$pcm_alloc((n + (i64)1));
        memcpy((void *)t,(void *)s,(u64)n);
        (*(t + n)) = (u64)0u;
        return t;
    }
;
}

static void msysc$freezstring(u8 *t,i64 n) {
    if ((n >= (i64)256)) {
        mlib$pcm_free((void *)t,(n + (i64)1));
    }
;
}

static void msysc$printchar(i64 ch) {
        u8 **  p;
    if ((msysc$outdev==(i64)1)) {
        printf((u8*)"%c",(int)ch);
    }
    else if ((msysc$outdev==(i64)2)) {
        fprintf(msysc$outchan,(u8*)"%c",(int)ch);
    }
    else if ((msysc$outdev==(i64)3)) {
        p = (u8 **)msysc$outchan;
        (*(*p)) = (u64)ch;
        (*p) += (i64)1;
        (*(*p)) = (u64)0u;
    }
;
}

void msysc$nextfmtchars(i64 lastx) {
        u8 c;
        u8 *  pstart;
        i64 n;
    if (!(!!(msysc$fmtstr))) {
        if (!!(msysc$needgap)) {
            msysc$printchar((i64)32);
        }
;
        msysc$needgap = (i64)0;
        return;
    }
;
    pstart = msysc$fmtstr;
    n = (i64)0;
    L410 :;
    while (1) {
        c = (u64)(*msysc$fmtstr);
        switch ((i64)(u64)c) {
        case 35:;
            {
                if (!!(lastx)) {
                    goto L412 ;
;
                }
;
                ++(msysc$fmtstr);
                if (!!(n)) {
                    msysc$printstr_n(pstart,n);
                }
;
                return;
            }
            break;
        case 0:;
            {
                if (!!(n)) {
                    msysc$printstr_n(pstart,n);
                }
                else if (!(!!(lastx))) {
                    msysc$printstr_n((byte*)"|",(i64)1);
                }
;
                return;
            }
            break;
        case 126:;
            {
                if (!!(n)) {
                    msysc$printstr_n(pstart,n);
                    n = (i64)0;
                }
;
                ++(msysc$fmtstr);
                c = (u64)(*msysc$fmtstr);
                if (!!((u64)c)) {
                    ++(msysc$fmtstr);
                    msysc$printchar((i64)(u64)c);
                }
;
                pstart = msysc$fmtstr;
            }
            break;
        default: {
            //skip:
L412 :;
;
            ++(n);
            ++(msysc$fmtstr);
        }
        } //SW
;
    }
L411 :;
    ;
}

void msysc$strtofmt(u8 *s,i64 slen,struct msysc$fmtrec *fmt) {
        u8 c;
        byte wset;
        i64 n;
        u8 str[100];
    (*fmt) = msysc$defaultfmt;
    if ((s == 0)) {
        return;
    }
;
    if ((slen == (i64)-1)) {
        slen = strlen(s);
    }
;
    memcpy(str,(void *)s,(u64)slen);
    str[(slen)] = (u64)0u;
    s = (u8 *)str;
    wset = (i64)0;
    L413 :;
    while (!!((u64)(*s))) {
        c = (u64)(*s);
        ++(s);
        switch ((i64)(u64)c) {
        case 66:;
        case 98:;
            {
                (*fmt).base = (i64)2;
            }
            break;
        case 72:;
        case 104:;
            {
                (*fmt).base = (i64)16;
            }
            break;
        case 79:;
        case 111:;
            {
                (*fmt).base = (i64)8;
            }
            break;
        case 88:;
        case 120:;
            {
                c = (u64)(*s);
                if (!!((u64)c)) {
                    switch ((i64)(u64)c) {
                    case 48:;
                    case 49:;
                    case 50:;
                    case 51:;
                    case 52:;
                    case 53:;
                    case 54:;
                    case 55:;
                    case 56:;
                    case 57:;
                        {
                            c = ((u64)c - '0');
                        }
                        break;
                    case 65:;
                    case 66:;
                    case 67:;
                    case 68:;
                    case 69:;
                    case 70:;
                        {
                            c = (u64)((i64)((u64)c - 'A') + (i64)10);
                        }
                        break;
                    case 97:;
                    case 98:;
                    case 99:;
                    case 100:;
                    case 101:;
                    case 102:;
                        {
                            c = (u64)((i64)((u64)c - 'a') + (i64)10);
                        }
                        break;
                    default: {
                        c = (u64)10u;
                    }
                    } //SW
;
                    (*fmt).base = (i64)(u64)c;
                    ++(s);
                }
;
            }
            break;
        case 81:;
        case 113:;
            {
                (*fmt).quotechar = '"';
            }
            break;
        case 126:;
            {
                (*fmt).quotechar = '~';
            }
            break;
        case 74:;
        case 106:;
            {
                (*fmt).justify = (u64)toupper((i32)(u64)(*s));
                if (!!((u64)(*s))) {
                    ++(s);
                }
;
            }
            break;
        case 65:;
            {
                (*fmt).lettercase = 'A';
            }
            break;
        case 97:;
            {
                (*fmt).lettercase = 'a';
            }
            break;
        case 90:;
        case 122:;
            {
                (*fmt).padchar = '0';
            }
            break;
        case 83:;
        case 115:;
            {
                (*fmt).sepchar = (u64)(*s);
                if (!!((u64)(*s))) {
                    ++(s);
                }
;
            }
            break;
        case 80:;
        case 112:;
            {
                (*fmt).padchar = (u64)(*s);
                if (!!((u64)(*s))) {
                    ++(s);
                }
;
            }
            break;
        case 84:;
        case 116:;
            {
                (*fmt).suffix = (u64)(*s);
                if (!!((u64)(*s))) {
                    ++(s);
                }
;
            }
            break;
        case 87:;
        case 119:;
            {
                (*fmt).usigned = 'W';
            }
            break;
        case 69:;
        case 101:;
            {
                (*fmt).realfmt = 'e';
            }
            break;
        case 70:;
        case 102:;
            {
                (*fmt).realfmt = 'f';
            }
            break;
        case 71:;
        case 103:;
            {
                (*fmt).realfmt = 'g';
            }
            break;
        case 46:;
            {
                wset = (i64)1;
            }
            break;
        case 44:;
        case 95:;
            {
                (*fmt).sepchar = (u64)c;
            }
            break;
        case 43:;
            {
                (*fmt).plus = '+';
            }
            break;
        case 68:;
        case 100:;
            {
                (*fmt).charmode = 'D';
            }
            break;
        case 67:;
        case 99:;
            {
                (*fmt).charmode = 'C';
            }
            break;
        case 77:;
        case 109:;
            {
                (*fmt).heapmode = 'M';
            }
            break;
        case 86:;
        case 118:;
            {
                (*fmt).param = 'V';
            }
            break;
        case 42:;
            {
                n = msysc$fmtparam;
                goto L416 ;
;
            }
            break;
        default: {
            if ((((u64)c >= '0') && ((u64)c <= '9'))) {
                n = (i64)((u64)c - '0');
                L417 :;
                while (1) {
                    c = (u64)(*s);
                    if (((i64)(u64)(*s) == (i64)0)) {
                        goto L418 ;
                    }
;
                    if ((((u64)c >= '0') && ((u64)c <= '9'))) {
                        ++(s);
                        n = (((n * (i64)10) + (i64)(u64)c) - (i64)48);
                    }
                    else {
                        goto L418 ;
                    }
;
                }
L418 :;
                ;
                //gotwidth:
L416 :;
;
                if (!(!!((i64)wset))) {
                    (*fmt).minwidth = n;
                    wset = (i64)1;
                }
                else {
                    (*fmt).precision = n;
                }
;
            }
;
        }
        } //SW
;
L414 :;
    }
L415 :;
    ;
}

static i64 msysc$domultichar(u8 *p,i64 n,u8 *dest,struct msysc$fmtrec *fmt) {
        u8 str[20];
        u8 *  q;
        i64 nchars;
        i64 $av_1;
    q = (u8 *)str;
    nchars = n;
    $av_1 = n;
    while ($av_1-- > 0) {
L419 :;
        if (((i64)(u64)(*p) == (i64)0)) {
            goto L421 ;
        }
;
        (*q) = (u64)(*p);
        ++(q);
        ++(p);
L420 :;
    }
L421 :;
    ;
    (*q) = (u64)0u;
    return msysc$expandstr((u8 *)str,dest,strlen((u8 *)str),(struct msysc$fmtrec *)fmt);
}

static i64 msysc$expandstr(u8 *s,u8 *t,i64 n,struct msysc$fmtrec *fmt) {
        i64 i;
        i64 w;
        i64 m;
        i64 $av_1;
        i64 $av_2;
        i64 $av_3;
        i64 $av_4;
        i64 $av_5;
    w = (i64)(*fmt).minwidth;
    if (((w == (i64)0) || (w <= n))) {
        strncpy(t,s,(u64)n);
        (*(t + n)) = (u64)0u;
        return n;
    }
;
    if (((u64)(*fmt).justify == 'L')) {
        strncpy(t,s,(u64)n);
        t += n;
                ($av_1 = (w - n));
        for (i=(i64)1;i<=$av_1;++i) {
L422 :;
            (*t) = (u64)(*fmt).padchar;
            ++(t);
L423 :;
        }
L424 :;
        ;
        (*t) = (u64)0u;
    }
    else if (((u64)(*fmt).justify == 'R')) {
        if (((((u64)(*fmt).padchar == '0') && !!((i64)(*fmt).base)) && (((u64)(*s) == '-') || ((u64)(*s) == '+')))) {
            (*t) = (u64)(*s);
            ++(t);
            $av_2 = (w - n);
            while ($av_2-- > 0) {
L425 :;
                (*t) = (u64)(*fmt).padchar;
                ++(t);
L426 :;
            }
L427 :;
            ;
            strncpy(t,(s + (i64)1),(u64)(n - (i64)1));
            (*((t + n) - (i64)1)) = (u64)0u;
        }
        else {
            $av_3 = (w - n);
            while ($av_3-- > 0) {
L428 :;
                (*t) = (u64)(*fmt).padchar;
                ++(t);
L429 :;
            }
L430 :;
            ;
            strncpy(t,s,(u64)n);
            (*(t + n)) = (u64)0u;
        }
;
    }
    else {
        m = (((w - n) + (i64)1) / (i64)2);
        $av_4 = m;
        while ($av_4-- > 0) {
L431 :;
            (*t) = (u64)(*fmt).padchar;
            ++(t);
L432 :;
        }
L433 :;
        ;
        strncpy(t,s,(u64)n);
        t += n;
        $av_5 = ((w - n) - m);
        while ($av_5-- > 0) {
L434 :;
            (*t) = (u64)(*fmt).padchar;
            ++(t);
L435 :;
        }
L436 :;
        ;
        (*t) = (u64)0u;
    }
;
    return w;
}

static i64 msysc$u64tostr(u64 aa,u8 *s,u64 base,i64 sep) {
        u8 t[360];
        i64 i;
        i64 j;
        i64 k;
        i64 g;
        u8 *  s0;
    i = (i64)0;
    k = (i64)0;
    g = (((i64)base == (i64)10) ? (i64)3 : (i64)4);
    L437 :;
    do {
        t[(++(i))] = (u64)msysc$digits[((i64)(aa % base))];
        aa = (aa / base);
        ++(k);
        if (((!!(sep) && ((i64)aa != (i64)0)) && (k == g))) {
            t[(++(i))] = (u64)sep;
            k = (i64)0;
        }
;
L438 :;
    }
    while (!((i64)aa == (i64)0));
L439 :;
    ;
    j = i;
    s0 = s;
    L440 :;
    while (!!(i)) {
        (*s) = (u64)t[((i)--)];
        ++(s);
L441 :;
    }
L442 :;
    ;
    (*s) = (u64)0u;
    return j;
}

static i64 msysc$i64tostrfmt(i64 aa,u8 *s,struct msysc$fmtrec *fmt) {
        u8 str[360];
        i64 n;
        i64 usigned;
        static u64 mindint = (u64)9223372036854775808u;
    usigned = (i64)0;
    if (!!((u64)(*fmt).usigned)) {
        usigned = (i64)1;
    }
;
    if (((aa == (i64)mindint) && !(!!(usigned)))) {
        str[((i64)0)] = '-';
        n = (msysc$i64mintostr((u8 *)&str[((i64)1)],(i64)(*fmt).base,(i64)(u64)(*fmt).sepchar) + (i64)1);
    }
    else {
        if (((!(!!(usigned)) && (aa < (i64)0)) || !!((u64)(*fmt).plus))) {
            if ((aa < (i64)0)) {
                aa = -(aa);
                str[((i64)0)] = '-';
            }
            else {
                str[((i64)0)] = '+';
            }
;
            n = (msysc$u64tostr((u64)aa,(u8 *)&str[((i64)1)],(u64)(i64)(*fmt).base,(i64)(u64)(*fmt).sepchar) + (i64)1);
        }
        else {
            n = msysc$u64tostr((u64)aa,(u8 *)str,(u64)(i64)(*fmt).base,(i64)(u64)(*fmt).sepchar);
        }
;
    }
;
    if (!!((u64)(*fmt).suffix)) {
        str[(n)] = (u64)(*fmt).suffix;
        str[(++(n))] = (u64)0u;
    }
;
    if (((((i64)(*fmt).base > (i64)10) || !!((u64)(*fmt).suffix)) && ((u64)(*fmt).lettercase == 'a'))) {
        msysc$convlcstring((u8 *)str);
    }
;
    return msysc$expandstr((u8 *)str,s,n,(struct msysc$fmtrec *)fmt);
}

static i64 msysc$u64tostrfmt(i64 aa,u8 *s,struct msysc$fmtrec *fmt) {
        u8 str[360];
        i64 n;
    n = msysc$u64tostr((u64)aa,(u8 *)str,(u64)(i64)(*fmt).base,(i64)(u64)(*fmt).sepchar);
    if (!!((u64)(*fmt).suffix)) {
        str[(n)] = (u64)(*fmt).suffix;
        str[(++(n))] = (u64)0u;
    }
;
    if ((((i64)(*fmt).base > (i64)10) || (!!((u64)(*fmt).suffix) && ((u64)(*fmt).lettercase == 'a')))) {
        msysc$convlcstring((u8 *)str);
    }
;
    return msysc$expandstr((u8 *)str,s,n,(struct msysc$fmtrec *)fmt);
}

static i64 msysc$i64mintostr(u8 *s,i64 base,i64 sep) {
        u8 t[360];
        i64 i;
        i64 j;
        i64 k;
        i64 g;
    switch (base) {
    case 10:;
        {
            strcpy((u8 *)&t[((i64)0)],(byte*)"9223372036854775808");
            j = (i64)3;
        }
        break;
    case 16:;
        {
            strcpy((u8 *)&t[((i64)0)],(byte*)"8000000000000000");
            j = (i64)1;
        }
        break;
    case 2:;
        {
            strcpy((u8 *)&t[((i64)0)],(byte*)"1000000000000000000000000000000000000000000000000000000000000000");
            j = (i64)7;
        }
        break;
    default: {
        strcpy((u8 *)&t[((i64)0)],(byte*)"<mindint>");
    }
    } //SW
;
    i = strlen((u8 *)&t[((i64)0)]);
    s += i;
    if (!!(sep)) {
        s += j;
    }
;
    (*s) = (u64)0u;
    k = (i64)0;
    g = ((base == (i64)10) ? (i64)3 : (i64)4);
    L443 :;
    while (!!(i)) {
        --(s);
        (*s) = (u64)t[(((i)-- - (i64)1))];
        if (((!!(sep) && !!(i)) && (++(k) == g))) {
            --(s);
            (*s) = (u64)sep;
            k = (i64)0;
        }
;
L444 :;
    }
L445 :;
    ;
    return strlen(s);
}

static i64 msysc$strtostrfmt(u8 *s,u8 *t,i64 n,struct msysc$fmtrec *fmt) {
        u8 *  u;
        u8 *  v;
        u8 str[256];
        i64 w;
        i64 nheap;
    nheap = (i64)0;
    if ((!!((u64)(*fmt).quotechar) || !!((u64)(*fmt).lettercase))) {
        if ((n < (i64)256)) {
            u = (u8 *)str;
        }
        else {
            nheap = (n + (i64)3);
            u = (u8 *)mlib$pcm_alloc(nheap);
        }
;
        if (!!((u64)(*fmt).quotechar)) {
            v = u;
            (*v) = (u64)(*fmt).quotechar;
            ++(v);
            if (!!(n)) {
                strcpy(v,s);
                v += n;
            }
;
            (*v) = (u64)(*fmt).quotechar;
            ++(v);
            (*v) = (u64)0u;
            n += (i64)2;
        }
        else {
            memcpy((void *)u,(void *)s,(u64)n);
        }
;
        switch ((i64)(u64)(*fmt).lettercase) {
        case 97:;
            {
                msysc$convlcstring(u);
            }
            break;
        case 65:;
            {
                msysc$convucstring(u);
            }
            break;
        } //SW
;
        s = u;
    }
;
    w = (i64)(*fmt).minwidth;
    if ((w > n)) {
        n = msysc$expandstr(s,t,n,(struct msysc$fmtrec *)fmt);
    }
    else {
        memcpy((void *)t,(void *)s,(u64)n);
    }
;
    if (!!(nheap)) {
        mlib$pcm_free((void *)u,nheap);
    }
;
    return n;
}

static void msysc$tostr_i64(i64 a,struct msysc$fmtrec *fmt) {
        u8 str[360];
        i64 n;
        {u64 $temp = (u64)(*fmt).charmode;
if (($temp==(u64)0u)) {
        n = msysc$i64tostrfmt(a,(u8 *)str,(struct msysc$fmtrec *)fmt);
    }
    else if (($temp=='D') || ($temp=='d')) {
        n = msysc$domultichar((u8 *)&a,(i64)8,(u8 *)str,(struct msysc$fmtrec *)fmt);
    }
    else {
        msysc$printchar(a);
        return;
    }
    };
    msysc$printstr_n((u8 *)str,n);
}

static void msysc$tostr_u64(u64 a,struct msysc$fmtrec *fmt) {
        u8 str[360];
        i64 n;
        {u64 $temp = (u64)(*fmt).charmode;
if (($temp=='D') || ($temp=='d')) {
        n = msysc$domultichar((u8 *)&a,(i64)8,(u8 *)str,(struct msysc$fmtrec *)fmt);
    }
    else if (($temp=='C') || ($temp=='c')) {
        msysc$printchar((i64)a);
        return;
    }
    else {
        n = msysc$u64tostrfmt((i64)a,(u8 *)str,(struct msysc$fmtrec *)fmt);
    }
    };
    msysc$printstr_n((u8 *)str,n);
}

static void msysc$tostr_r64(r64 x,struct msysc$fmtrec *fmt) {
        u8 str[360];
        u8 str2[360];
        u8 cfmt[10];
        i64 n;
    cfmt[((i64)0)] = '%';
    if (!!((i64)(*fmt).precision)) {
        cfmt[((i64)1)] = '.';
        cfmt[((i64)2)] = '*';
        cfmt[((i64)3)] = (u64)(*fmt).realfmt;
        cfmt[((i64)4)] = (u64)0u;
        sprintf((u8 *)str,(u8 *)cfmt,(i64)(*fmt).precision,x);
    }
    else {
        cfmt[((i64)1)] = (u64)(*fmt).realfmt;
        cfmt[((i64)2)] = (u64)0u;
        sprintf((u8 *)str,(u8 *)cfmt,x);
    }
;
    n = strlen((u8 *)str);
    if ((n < (i64)(*fmt).minwidth)) {
        n = msysc$expandstr((u8 *)str,(u8 *)str2,n,(struct msysc$fmtrec *)fmt);
        strcpy((u8 *)str,(u8 *)str2);
    }
;
    msysc$printstr_n((u8 *)str,n);
}

static void msysc$tostr_str(u8 *s,struct msysc$fmtrec *fmt) {
        i64 oldlen;
        i64 newlen;
        i64 n;
        u8 *  t;
    oldlen = strlen(s);
    newlen = oldlen;
    if (((!!((u64)(*fmt).quotechar) || ((i64)(*fmt).minwidth > newlen)) || !!((u64)(*fmt).lettercase))) {
        if (!!((u64)(*fmt).quotechar)) {
            newlen += (i64)2;
        }
;
        if (((i64)(*fmt).minwidth > newlen)) {
            newlen = (i64)(*fmt).minwidth;
        }
;
        t = (u8 *)mlib$pcm_alloc((newlen + (i64)1));
        n = msysc$strtostrfmt(s,t,oldlen,(struct msysc$fmtrec *)fmt);
        msysc$printstr_n(t,n);
        mlib$pcm_free((void *)t,(newlen + (i64)1));
    }
    else {
        msysc$printstr_n(s,oldlen);
    }
;
}

static struct msysc$fmtrec *msysc$getfmt(u8 *fmtstyle) {
        static struct msysc$fmtrec fmt;
    if (!!(fmtstyle)) {
        msysc$strtofmt(fmtstyle,(i64)-1,&fmt);
        return (struct msysc$fmtrec *)&fmt;
    }
    else {
        return (struct msysc$fmtrec *)&msysc$defaultfmt;
    }
;
}

u8 *msysc$strint(i64 a,u8 *fmtstyle) {
        static u8 str[100];
        struct msysc$fmtrec *  fmt;
    msysc$m_print_startstr((u8 *)str);
    msysc$tostr_i64(a,(struct msysc$fmtrec *)(fmt = (struct msysc$fmtrec *)msysc$getfmt(fmtstyle)));
    msysc$m_print_end();
    return msysc$getstr((u8 *)str,(struct msysc$fmtrec *)fmt);
}

void msysc$getstrint(i64 a,u8 *dest) {
    msysc$m_print_startstr(dest);
    msysc$tostr_i64(a,(struct msysc$fmtrec *)msysc$getfmt(0));
    msysc$m_print_end();
}

u8 *msysc$strword(u64 a,u8 *fmtstyle) {
        static u8 str[100];
        struct msysc$fmtrec *  fmt;
    msysc$m_print_startstr((u8 *)str);
    msysc$tostr_u64(a,(struct msysc$fmtrec *)(fmt = (struct msysc$fmtrec *)msysc$getfmt(fmtstyle)));
    msysc$m_print_end();
    return msysc$getstr((u8 *)str,(struct msysc$fmtrec *)fmt);
}

u8 *msysc$strreal(r64 a,u8 *fmtstyle) {
        static u8 str[320];
        struct msysc$fmtrec *  fmt;
    msysc$m_print_startstr((u8 *)str);
    msysc$tostr_r64(a,(struct msysc$fmtrec *)(fmt = (struct msysc$fmtrec *)msysc$getfmt(fmtstyle)));
    msysc$m_print_end();
    return msysc$getstr((u8 *)str,(struct msysc$fmtrec *)fmt);
}

static u8 *msysc$getstr(u8 *s,struct msysc$fmtrec *fmt) {
    if (!!((u64)(*fmt).heapmode)) {
        return mlib$pcm_copyheapstring(s);
    }
    else {
        return s;
    }
;
}

static void msysc$initreadbuffer(void) {
    if (!!(msysc$rd_buffer)) {
        return;
    }
;
    msysc$rd_buffer = (u8 *)mlib$pcm_alloc((i64)16384);
    (*msysc$rd_buffer) = (u64)0u;
    msysc$rd_pos = (msysc$rd_lastpos = msysc$rd_buffer);
}

void msysc$m_read_conline(void) {
    msysc$initreadbuffer();
    mlib$readlinen(0,msysc$rd_buffer,(i64)16384);
    msysc$rd_length = strlen(msysc$rd_buffer);
    msysc$rd_pos = msysc$rd_buffer;
    msysc$rd_lastpos = 0;
}

void msysc$m_read_fileline(void *f) {
    msysc$initreadbuffer();
    mlib$readlinen(f,msysc$rd_buffer,(i64)16384);
    msysc$rd_length = strlen(msysc$rd_buffer);
    msysc$rd_pos = msysc$rd_buffer;
    msysc$rd_lastpos = 0;
}

void msysc$m_read_strline(u8 *s) {
        i64 n;
    msysc$initreadbuffer();
    n = strlen(s);
    if ((n < (i64)16384)) {
        strcpy(msysc$rd_buffer,s);
    }
    else {
        memcpy((void *)msysc$rd_buffer,(void *)s,(u64)16383u);
        (*((msysc$rd_buffer + (i64)16384) - (i64)1)) = (u64)0u;
    }
;
    msysc$rd_length = n;
    msysc$rd_pos = msysc$rd_buffer;
    msysc$rd_lastpos = 0;
}

static u8 *msysc$readitem(i64 *itemlength) {
        u8 *  p;
        u8 *  s;
        u8 *  itemstr;
        u8 quotechar;
        u8 c;
    if (!(!!(msysc$rd_buffer))) {
        msysc$initreadbuffer();
    }
;
    s = msysc$rd_pos;
    L446 :;
    while ((((u64)(*s) == ' ') || ((i64)(u64)(*s) == (i64)9))) {
        ++(s);
L447 :;
    }
L448 :;
    ;
    itemstr = s;
    msysc$rd_lastpos = (msysc$rd_pos = s);
    if (((i64)(u64)(*s) == (i64)0)) {
        msysc$termchar = (i64)0;
        (*itemlength) = (i64)0;
        return s;
    }
;
    quotechar = (u64)0u;
    if (((u64)(*s) == '"')) {
        quotechar = '"';
        ++(s);
    }
    else if (((u64)(*s) == (u64)39u)) {
        quotechar = (u64)39u;
        ++(s);
    }
;
    p = (itemstr = s);
    L449 :;
    while (!!((u64)(*s))) {
        c = (u64)(*(s)++);
        switch ((i64)(u64)c) {
        case 32:;
        case 9:;
        case 44:;
        case 61:;
            {
                if ((!!((u64)quotechar) || (p == s))) {
                    goto L452 ;
;
                }
;
                msysc$termchar = (i64)(u64)c;
                goto L451 ;
            }
            break;
        default: {
            //normalchar:
L452 :;
;
            if (((u64)c == (u64)quotechar)) {
                if (((u64)(*s) == (u64)quotechar)) {
                    (*p) = (u64)c;
                    ++(s);
                    ++(p);
                }
                else {
                    msysc$termchar = (i64)(u64)(*s);
                    if (((msysc$termchar == (i64)44) || (msysc$termchar == (i64)61))) {
                        ++(s);
                        msysc$termchar = (i64)(u64)(*s);
                    }
;
                    goto L451 ;
                }
;
            }
            else {
                (*p) = (u64)c;
                ++(p);
            }
;
        }
        } //SW
;
L450 :;
    }
L451 :;
    ;
    if (((i64)(u64)(*s) == (i64)0)) {
        msysc$termchar = (i64)0;
    }
;
    (*itemlength) = (p - itemstr);
    msysc$rd_pos = s;
    return itemstr;
}

i64 msysc$strtoint(u8 *s,i64 length,i64 base) {
        byte signd;
        u64 aa;
        u8 c;
        u8 d;
    msysc$itemerror = (i64)0;
    if ((length == (i64)-1)) {
        length = strlen(s);
    }
;
    signd = (i64)0;
    if ((!!(length) && ((u64)(*s) == '-'))) {
        signd = (i64)1;
        ++(s);
        --(length);
    }
    else if ((!!(length) && ((u64)(*s) == '+'))) {
        ++(s);
        --(length);
    }
;
    aa = (u64)0u;
    L453 :;
    while (!!(length)) {
        c = (u64)(*(s)++);
        --(length);
        switch ((i64)(u64)c) {
        case 65:;
        case 66:;
        case 67:;
        case 68:;
        case 69:;
        case 70:;
            {
                d = (u64)((i64)((u64)c - 'A') + (i64)10);
            }
            break;
        case 97:;
        case 98:;
        case 99:;
        case 100:;
        case 101:;
        case 102:;
            {
                d = (u64)((i64)((u64)c - 'a') + (i64)10);
            }
            break;
        case 48:;
        case 49:;
        case 50:;
        case 51:;
        case 52:;
        case 53:;
        case 54:;
        case 55:;
        case 56:;
        case 57:;
            {
                d = ((u64)c - '0');
            }
            break;
        case 95:;
        case 39:;
            {
                goto L454 ;
            }
            break;
        default: {
            msysc$itemerror = (i64)1;
            goto L455 ;
        }
        } //SW
;
        if (((i64)(u64)d >= base)) {
            msysc$itemerror = (i64)1;
            goto L455 ;
        }
;
        aa = (u64)(((i64)aa * base) + (i64)(u64)d);
L454 :;
    }
L455 :;
    ;
    if (!!((i64)signd)) {
        return (i64)-(aa);
    }
    else {
        return (i64)aa;
    }
;
}

i64 msysc$m_read_i64(i64 fmt) {
        u8 *  s;
        i64 length;
    if ((fmt==(i64)67) || (fmt==(i64)99)) {
        msysc$rd_lastpos = msysc$rd_pos;
        if (!!((u64)(*msysc$rd_pos))) {
            return (i64)(u64)(*(msysc$rd_pos)++);
        }
        else {
            return (i64)0;
        }
;
    }
    else if ((fmt==(i64)84) || (fmt==(i64)116)) {
        return msysc$termchar;
    }
    else if ((fmt==(i64)69) || (fmt==(i64)101)) {
        return msysc$itemerror;
    }
;
    s = msysc$readitem(&length);
    if ((fmt==(i64)0) || (fmt==(i64)73) || (fmt==(i64)105)) {
        return msysc$strtoint(s,length,(i64)10);
    }
    else if ((fmt==(i64)66) || (fmt==(i64)98)) {
        return msysc$strtoint(s,length,(i64)2);
    }
    else if ((fmt==(i64)72) || (fmt==(i64)104)) {
        return msysc$strtoint(s,length,(i64)16);
    }
;
    return (i64)0;
}

r64 msysc$m_read_r64(i64 fmt) {
        u8 str[512];
        u8 *  s;
        i64 length;
        i32 numlength;
        r64 x;
    s = msysc$readitem(&length);
    if (((length == (i64)0) || (length >= (i64)512))) {
        return (double)0.;
    }
;
    memcpy(str,(void *)s,(u64)length);
    str[((length + (i64)1))-1] = (u64)0u;
    msysc$itemerror = (i64)0;
    if (((sscanf((u8 *)str,(byte*)"%lf%n",&x,&numlength) == (i64)0) || ((i64)numlength != length))) {
        x = (double)0.;
        msysc$itemerror = (i64)1;
    }
;
    return x;
}

void msysc$m_read_str(u8 *dest,i64 destlen,i64 fmt) {
        u8 *  s;
        i64 length;
    msysc$itemerror = (i64)0;
    if (((fmt == (i64)76) || (fmt == (i64)108))) {
        s = msysc$rd_pos;
        length = ((msysc$rd_buffer + msysc$rd_length) - msysc$rd_pos);
    }
    else {
        s = msysc$readitem(&length);
        if (((fmt == (i64)78) || (fmt == (i64)110))) {
            msysc$iconvlcn(s,length);
        }
;
    }
;
    if ((destlen > (i64)0)) {
        if ((length >= destlen)) {
            length = (destlen - (i64)1);
            msysc$itemerror = (i64)1;
        }
;
    }
;
    memcpy((void *)dest,(void *)s,(u64)length);
    (*(dest + length)) = (u64)0u;
}

void msysc$readstr(u8 *dest,i64 fmt,i64 destlen) {
    msysc$m_read_str(dest,destlen,fmt);
}

void msysc$rereadln(void) {
    msysc$rd_pos = msysc$rd_buffer;
    msysc$rd_lastpos = msysc$rd_pos;
}

void msysc$reread(void) {
    msysc$rd_pos = msysc$rd_lastpos;
}

i64 msysc$valint(u8 *s,i64 fmt) {
        u8 *  old_pos;
        u8 *  old_lastpos;
        i64 aa;
    msysc$initreadbuffer();
    old_pos = msysc$rd_pos;
    old_lastpos = msysc$rd_lastpos;
    msysc$rd_pos = s;
    aa = msysc$m_read_i64(fmt);
    msysc$rd_pos = old_pos;
    msysc$rd_lastpos = old_lastpos;
    return aa;
}

r64 msysc$valreal(u8 *s) {
        u8 *  old_pos;
        u8 *  old_lastpos;
        r64 x;
    msysc$initreadbuffer();
    old_pos = msysc$rd_pos;
    old_lastpos = msysc$rd_lastpos;
    msysc$rd_pos = s;
    x = msysc$m_read_r64((i64)0);
    msysc$rd_pos = old_pos;
    msysc$rd_lastpos = old_lastpos;
    return x;
}

static void msysc$iconvlcn(u8 *s,i64 n) {
        i64 $av_1;
    $av_1 = n;
    while ($av_1-- > 0) {
L456 :;
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L457 :;
    }
L458 :;
    ;
}

static void msysc$iconvucn(u8 *s,i64 n) {
        i64 $av_1;
    $av_1 = n;
    while ($av_1-- > 0) {
L459 :;
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L460 :;
    }
L461 :;
    ;
}

static void msysc$convlcstring(u8 *s) {
    L462 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L463 :;
    }
L464 :;
    ;
}

static void msysc$convucstring(u8 *s) {
    L465 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L466 :;
    }
L467 :;
    ;
}

i64 msysc$m_power_i64(i64 n,i64 a) {
    if ((n < (i64)0)) {
        return (i64)0;
    }
    else if ((n == (i64)0)) {
        return (i64)1;
    }
    else if ((n == (i64)1)) {
        return a;
    }
    else if (((n & (i64)1) == (i64)0)) {
        return msysc$m_power_i64((n / (i64)2),(a*a));
    }
    else {
        return (msysc$m_power_i64(((n - (i64)1) / (i64)2),(a*a)) * a);
    }
;
}

void msysc$m_intoverflow(void) {
    mlib$abortprogram((byte*)"Integer overflow detected");
}

void msysc$m_dotindex(u64 i,u64 a) {
    mlib$abortprogram((byte*)"DOT INDEX");
}

void msysc$m_dotslice(u64 j,u64 i,u64 a) {
    mlib$abortprogram((byte*)"DOT SLICE");
}

void msysc$m_popdotindex(u64 i,u64 *p,u64 x) {
    mlib$abortprogram((byte*)"POP DOT INDEX");
}

void msysc$m_popdotslice(u64 j,u64 i,u64 *p,u64 x) {
    mlib$abortprogram((byte*)"POP DOT SLICE");
}

i64 msysc$m_imin(i64 a,i64 b) {
    return (a<b?a:b);
}

i64 msysc$m_imax(i64 a,i64 b) {
    return (a>b?a:b);
}

r64 msysc$m_sign(r64 x) {
    if ((x > (double)0.)) {
        return (double)1.;
    }
    else if ((x < (double)0.)) {
        return (double)-1.;
    }
    else {
        return (double)0.;
    }
;
}

r64 msysc$m_tp_i64tor64(i64 a) {
        r64 x;
    memcpy(&x,&a,(u64)8u);
    return x;
}

i64 msysc$m_tp_r64toi64(r64 x) {
        i64 a;
    memcpy(&a,&x,(u64)8u);
    return a;
}

i64 msysc$m_tp_reftoi64(void *p) {
        i64 a;
    memcpy(&a,&p,(u64)8u);
    return a;
}

void *msysc$m_tp_i64toref(i64 a) {
        void *  p;
    memcpy(&p,&a,(u64)8u);
    return p;
}

// START
void msysc$start(void) {
    mlib$start();
    mclib$start();
    mwindows$start();
    mwindllc$start();

}

void *mlib$pcm_alloc(i64 n) {
        byte *  p;
    if (!(!!((i64)mlib$pcm_setup))) {
        mlib$pcm_init();
    }
;
    if ((n > (i64)2048)) {
        mlib$alloccode = mlib$pcm_getac(n);
        mlib$allocbytes = (i64)mlib$allocupper[(mlib$alloccode)];
        p = (byte *)mlib$allocmem(mlib$allocbytes);
        if (!(!!(p))) {
            mlib$abortprogram((byte*)"pcm_alloc failure");
        }
;
        return p;
    }
;
    mlib$alloccode = (i64)mlib$sizeindextable[(n)];
    mlib$allocbytes = (i64)mlib$allocupper[(mlib$alloccode)];
    mlib$smallmemtotal += mlib$allocbytes;
    if (!!((p = (byte *)mlib$freelist[(mlib$alloccode)]))) {
        mlib$freelist[(mlib$alloccode)] = (u64 *)(i64)(*mlib$freelist[(mlib$alloccode)]);
        return p;
    }
;
    p = mlib$pcheapptr;
    mlib$pcheapptr += mlib$allocbytes;
    if ((mlib$pcheapptr >= mlib$pcheapend)) {
        p = (byte *)mlib$pcm_newblock(mlib$allocbytes);
        return p;
    }
;
    return p;
}

void mlib$pcm_free(void *p,i64 n) {
        i64 acode;
    if ((n == (i64)0)) {
        return;
    }
;
    if ((n > (i64)2048)) {
        mlib$memtotal -= n;
        free(p);
        return;
    }
;
    if (!!(p)) {
        acode = (i64)mlib$sizeindextable[(n)];
        mlib$smallmemtotal -= (i64)mlib$allocupper[(acode)];
        (*(u64 *)p) = (u64)(i64)mlib$freelist[(acode)];
        mlib$freelist[(acode)] = (u64 *)p;
    }
;
}

void mlib$pcm_freeac(void *p,i64 alloc) {
    mlib$pcm_free(p,(i64)mlib$allocupper[(alloc)]);
}

void mlib$pcm_clearmem(void *p,i64 n) {
    memset(p,(i32)(i64)0,(u64)n);
}

void mlib$pcm_init(void) {
        i64 j;
        i64 k;
        i64 size;
        i64 i;
    mlib$alloccode = (i64)0;
    if (!!((i64)mlib$pcm_setup)) {
        return;
    }
;
    mlib$pcm_newblock((i64)0);
    for (i=(i64)1;i<=(i64)2048;++i) {
L468 :;
        j = (i64)1;
        k = (i64)16;
        L471 :;
        while ((i > k)) {
            k = (k << (i64)1);
            ++(j);
L472 :;
        }
L473 :;
        ;
        mlib$sizeindextable[(i)] = j;
L469 :;
    }
L470 :;
    ;
    mlib$allocupper[((i64)1)] = (u64)16u;
    size = (i64)16;
    for (i=(i64)2;i<=(i64)27;++i) {
L474 :;
        size *= (i64)2;
        mlib$allocupper[(i)] = (u64)size;
        if ((size >= (i64)33554432)) {
            k = i;
            goto L476 ;
        }
;
L475 :;
    }
L476 :;
    ;
    for (i=(k + (i64)1);i<=(i64)300;++i) {
L477 :;
        size += (i64)33554432;
        if ((size < (i64)8589934592)) {
            mlib$allocupper[(i)] = (u64)size;
            mlib$maxmemory = (u64)size;
        }
        else {
            mlib$maxalloccode = (i - (i64)1);
            goto L479 ;
        }
;
L478 :;
    }
L479 :;
    ;
    mlib$pcm_setup = (i64)1;
}

i64 mlib$pcm_getac(i64 size) {
    if ((size <= (i64)2048)) {
        return (i64)mlib$sizeindextable[(size)];
    }
;
    size = ((size + (i64)255) >> (i64)8);
    if ((size <= (i64)2048)) {
        return ((i64)mlib$sizeindextable[(size)] + (i64)8);
    }
;
    size = ((size + (i64)63) >> (i64)6);
    if ((size <= (i64)2048)) {
        return ((i64)mlib$sizeindextable[(size)] + (i64)14);
    }
;
    size = ((((size - (i64)2048) + (i64)2047) / (i64)2048) + (i64)22);
    return size;
}

void *mlib$pcm_newblock(i64 itemsize) {
        static i64 totalheapsize;
        byte *  p;
    totalheapsize += (i64)2097152;
    mlib$alloccode = (i64)0;
    p = (byte *)mlib$allocmem((i64)2097152);
    if ((p == 0)) {
        mlib$abortprogram((byte*)"Can't alloc pc heap");
    }
;
    mlib$pcheapptr = p;
    mlib$pcheapend = (p + (i64)2097152);
    if ((mlib$pcheapstart == 0)) {
        mlib$pcheapstart = p;
    }
;
    mlib$pcheapptr += itemsize;
    return (u32 *)p;
}

i64 mlib$pcm_round(i64 n) {
        static i32 allocbytes[9] = {(i32)(i64)0,(i32)(i64)16,(i32)(i64)32,(i32)(i64)64,(i32)(i64)128,(i32)(i64)256,(i32)(i64)512,(i32)(i64)1024,(i32)(i64)2048};
    if ((n > (i64)2048)) {
        return n;
    }
    else {
        return (i64)allocbytes[((i64)mlib$sizeindextable[(n)])];
    }
;
}

void *mlib$pcm_allocz(i64 n) {
        void *  p;
    p = mlib$pcm_alloc(n);
    memset(p,(i32)(i64)0,(u64)n);
    return p;
}

u8 *mlib$pcm_copyheapstring(u8 *s) {
        u8 *  q;
        i64 n;
    if ((s == 0)) {
        return 0;
    }
;
    n = (strlen(s) + (i64)1);
    q = (u8 *)mlib$pcm_alloc(n);
    memcpy((void *)q,(void *)s,(u64)n);
    return q;
}

u8 *mlib$pcm_copyheapstringn(u8 *s,i64 n) {
        u8 *  q;
    if ((s == 0)) {
        return 0;
    }
;
    q = (u8 *)mlib$pcm_alloc((n + (i64)1));
    memcpy((void *)q,(void *)s,(u64)n);
    (*(q + n)) = (u64)0u;
    return q;
}

u8 *mlib$pcm_copyheapblock(u8 *s,i64 length) {
        u8 *  q;
    if ((length == (i64)0)) {
        return 0;
    }
;
    q = (u8 *)mlib$pcm_alloc(length);
    memcpy((void *)q,(void *)s,(u64)length);
    return q;
}

void *mlib$allocmem(i64 n) {
        void *  p;
    p = malloc((u64)n);
    if (!!(p)) {
        mlib$memtotal += n;
        return p;
    }
;
    msysc$m_print_startcon();
    msysc$m_print_i64(n,NULL);
    msysc$m_print_i64(mlib$memtotal,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mlib$abortprogram((byte*)"Alloc mem failure");
    return 0;
}

void *mlib$reallocmem(void *p,i64 n) {
    p = realloc(p,(u64)n);
    if (!!(p)) {
        return p;
    }
;
    msysc$m_print_startcon();
    msysc$m_print_i64(n,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mlib$abortprogram((byte*)"Realloc mem failure");
    return 0;
}

void mlib$abortprogram(u8 *s) {
    msysc$m_print_startcon();
    msysc$m_print_str(s,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"ABORTING: Press key...",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    exit((i64)5);
}

i64 mlib$getfilesize(void *handlex) {
        u32 p;
        u32 size;
    p = ftell(handlex);
    fseek(handlex,(i32)(i64)0,(i32)(i64)2);
    size = ftell(handlex);
    fseek(handlex,(i32)(i64)p,(i32)(i64)0);
    return (i64)size;
}

void mlib$readrandom(void *handlex,byte *mem,i64 offset,i64 size) {
        i64 a;
    fseek(handlex,(i32)offset,(i32)(i64)0);
    a = (i64)fread(mem,(u64)1u,(u64)size,handlex);
}

i64 mlib$writerandom(void *handlex,byte *mem,i64 offset,i64 size) {
    fseek(handlex,(i32)offset,(i32)(i64)0);
    return (i64)fwrite(mem,(u64)1u,(u64)size,handlex);
}

i64 mlib$setfilepos(void *file,i64 offset) {
    return fseek(file,(i32)offset,(i32)(i64)0);
}

i64 mlib$getfilepos(void *file) {
    return ftell(file);
}

byte *mlib$readfile(u8 *filename) {
        void *  f;
        i64 size;
        byte *  m;
        byte *  p;
    f = fopen(filename,(byte*)"rb");
    if ((f == 0)) {
        return 0;
    }
;
    mlib$rfsize = (size = mlib$getfilesize(f));
    m = (byte *)mlib$pcm_alloc((size + (i64)2));
    if ((m == 0)) {
        return 0;
    }
;
    mlib$readrandom(f,m,(i64)0,size);
    p = (m + size);
    (*(u16 *)p) = (i64)0;
    fclose(f);
    return m;
}

i64 mlib$writefile(u8 *filename,byte *data,i64 size) {
        void *  f;
        i64 n;
    f = fopen(filename,(byte*)"wb");
    if ((f == 0)) {
        return (i64)0;
    }
;
    n = mlib$writerandom(f,data,(i64)0,size);
    fclose(f);
    return n;
}

i64 mlib$checkfile(u8 *file) {
        void *  f;
    if (!!((f = fopen(file,(byte*)"rb")))) {
        fclose(f);
        return (i64)1;
    }
;
    return (i64)0;
}

void mlib$readlinen(void *handlex,u8 *buffer,i64 size) {
        i64 ch;
        u8 *  p;
        i64 n;
        byte crseen;
    if ((handlex == 0)) {
        handlex = mwindows$os_getstdin();
    }
;
    if ((handlex == 0)) {
        n = (i64)0;
        p = buffer;
        L480 :;
        while (1) {
            ch = getchar();
            if ((((ch == (i64)13) || (ch == (i64)10)) || (ch == (i64)-1))) {
                (*p) = (u64)0u;
                return;
            }
;
            (*(p)++) = (u64)ch;
            ++(n);
            if ((n >= (size - (i64)2))) {
                (*p) = (u64)0u;
                return;
            }
;
        }
L481 :;
        ;
    }
;
    (*buffer) = (u64)0u;
    if ((fgets(buffer,(size - (i64)2),handlex) == 0)) {
        return;
    }
;
    n = strlen(buffer);
    if ((n == (i64)0)) {
        return;
    }
;
    p = ((buffer + n) - (i64)1);
    crseen = (i64)0;
    L482 :;
    while (((p >= buffer) && (((i64)(u64)(*p) == (i64)13) || ((i64)(u64)(*p) == (i64)10)))) {
        if ((((i64)(u64)(*p) == (i64)13) || ((i64)(u64)(*p) == (i64)10))) {
            crseen = (i64)1;
        }
;
        (*(p)--) = (u64)0u;
L483 :;
    }
L484 :;
    ;
    if ((!(!!((i64)crseen)) && ((n + (i64)4) > size))) {
        msysc$m_print_startcon();
        msysc$m_print_i64(size,NULL);
        msysc$m_print_i64(n,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        mlib$abortprogram((byte*)"line too long");
    }
;
}

void mlib$iconvlcn(u8 *s,i64 n) {
        i64 $av_1;
    $av_1 = n;
    while ($av_1-- > 0) {
L485 :;
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L486 :;
    }
L487 :;
    ;
}

void mlib$iconvucn(u8 *s,i64 n) {
        i64 $av_1;
    $av_1 = n;
    while ($av_1-- > 0) {
L488 :;
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L489 :;
    }
L490 :;
    ;
}

u8 *mlib$convlcstring(u8 *s) {
        u8 *  s0;
    s0 = s;
    L491 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L492 :;
    }
L493 :;
    ;
    return s0;
}

u8 *mlib$convucstring(u8 *s) {
        u8 *  s0;
    s0 = s;
    L494 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L495 :;
    }
L496 :;
    ;
    return s0;
}

u8 *mlib$changeext(u8 *s,u8 *newext) {
        static u8 newfile[260];
        u8 newext2[32];
        u8 *  sext;
        i64 n;
    strcpy((u8 *)&newfile[((i64)1)-1],s);
        {u64 $temp = (u64)(*newext);
if (($temp==(u64)0u)) {
        newext2[((i64)1)-1] = (u64)0u;
        newext2[((i64)2)-1] = (u64)0u;
    }
    else if (($temp=='.')) {
        strcpy((u8 *)&newext2[((i64)1)-1],newext);
    }
    else {
        strcpy((u8 *)&newext2[((i64)1)-1],(byte*)".");
        strcat((u8 *)&newext2[((i64)1)-1],newext);
    }
    };
    sext = mlib$extractext(s,(i64)1);
        {u64 $temp = (u64)(*sext);
if (($temp==(u64)0u)) {
        strcat((u8 *)&newfile[((i64)1)-1],(u8 *)&newext2[((i64)1)-1]);
    }
    else if (($temp=='.')) {
        strcat((u8 *)&newfile[((i64)1)-1],(u8 *)&newext2[((i64)2)-1]);
    }
    else {
        n = ((sext - s) - (i64)2);
        strcpy((u8 *)((&newfile[((i64)1)-1] + n) + (i64)1),(u8 *)&newext2[((i64)1)-1]);
    }
    };
    return (u8 *)&newfile[((i64)1)-1];
}

u8 *mlib$extractext(u8 *s,i64 period) {
        u8 *  t;
        u8 *  u;
    t = mlib$extractfile(s);
    if (((i64)(u64)(*t) == (i64)0)) {
        return (byte*)"";
    }
;
    u = ((t + strlen(t)) - (i64)1);
    L497 :;
    while ((u >= t)) {
        if (((u64)(*u) == '.')) {
            if (((i64)(u64)(*(u + (i64)1)) == (i64)0)) {
                return (!!(period) ? (byte*)"." : (byte*)"");
            }
;
            return (u + (i64)1);
        }
;
        --(u);
L498 :;
    }
L499 :;
    ;
    return (byte*)"";
}

u8 *mlib$extractpath(u8 *s) {
        static u8 str[260];
        u8 *  t;
        i64 n;
    t = ((s + strlen(s)) - (i64)1);
    L500 :;
    while ((t >= s)) {
        switch ((i64)(u64)(*t)) {
        case 92:;
        case 47:;
        case 58:;
            {
                n = ((t - s) + (i64)1);
                memcpy(str,(void *)s,(u64)n);
                str[(n)] = (u64)0u;
                return (u8 *)str;
            }
            break;
        } //SW
;
        --(t);
L501 :;
    }
L502 :;
    ;
    return (byte*)"";
}

u8 *mlib$extractfile(u8 *s) {
        u8 *  t;
    t = mlib$extractpath(s);
    if (((i64)(u64)(*t) == (i64)0)) {
        return s;
    }
;
    return (s + strlen(t));
}

u8 *mlib$extractbasefile(u8 *s) {
        static u8 str[100];
        u8 *  f;
        u8 *  e;
        i64 n;
        i64 flen;
    f = mlib$extractfile(s);
    flen = strlen(f);
    if ((flen == (i64)0)) {
        return (byte*)"";
    }
;
    e = mlib$extractext(f,(i64)0);
    if (!!((u64)(*e))) {
        n = ((flen - strlen(e)) - (i64)1);
        memcpy(&str,(void *)f,(u64)n);
        str[(n)] = (u64)0u;
        return (u8 *)str;
    }
;
    if (((u64)(*((f + flen) - (i64)1)) == '.')) {
        memcpy(&str,(void *)f,(u64)(flen - (i64)1));
        str[((flen - (i64)1))] = (u64)0u;
        return (u8 *)str;
    }
;
    return f;
}

u8 *mlib$addext(u8 *s,u8 *newext) {
        u8 *  sext;
    sext = mlib$extractext(s,(i64)1);
    if (((i64)(u64)(*sext) == (i64)0)) {
        return mlib$changeext(s,newext);
    }
;
    return s;
}

void *mlib$pcm_alloc32(void) {
        byte *  p;
    mlib$allocbytes = (i64)32;
    mlib$smallmemtotal += (i64)32;
    if (!!((p = (byte *)mlib$freelist[((i64)2)]))) {
        mlib$freelist[((i64)2)] = (u64 *)(i64)(*mlib$freelist[((i64)2)]);
        return p;
    }
;
    return mlib$pcm_alloc((i64)32);
}

void mlib$pcm_free32(void *p) {
    mlib$smallmemtotal -= (i64)32;
    (*(u64 *)p) = (u64)(i64)mlib$freelist[((i64)2)];
    mlib$freelist[((i64)2)] = (u64 *)p;
}

void mlib$outbyte(void *f,i64 x) {
    fwrite(&x,(u64)1u,(u64)1u,f);
}

void mlib$outword16(void *f,u64 x) {
    fwrite(&x,(u64)2u,(u64)1u,f);
}

void mlib$outword32(void *f,u64 x) {
    fwrite(&x,(u64)4u,(u64)1u,f);
}

void mlib$outword64(void *f,u64 x) {
    fwrite(&x,(u64)8u,(u64)1u,f);
}

void mlib$outstring(void *f,u8 *s) {
    fwrite((void *)s,(u64)(strlen(s) + (i64)1),(u64)1u,f);
}

void mlib$outblock(void *f,void *p,i64 n) {
    fwrite(p,(u64)n,(u64)1u,f);
}

i64 mlib$myeof(void *f) {
        i64 c;
    c = fgetc(f);
    if ((c == (i64)-1)) {
        return (i64)1;
    }
;
    ungetc((i32)c,f);
    return (i64)0;
}

void mlib$strbuffer_add(struct mlib$strbuffer *dest,u8 *s,i64 n) {
        i64 newlen;
        i64 oldlen;
        u8 *  newptr;
    if ((n == (i64)0)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"N=0",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    if ((n == (i64)-1)) {
        n = strlen(s);
    }
;
    oldlen = (i64)(*dest).length;
    if ((oldlen == (i64)0)) {
        (*dest).strptr = (u8 *)mlib$pcm_alloc((n + (i64)1));
        (*dest).allocated = mlib$allocbytes;
        (*dest).length = n;
        memcpy((void *)(*dest).strptr,(void *)s,(u64)n);
        (*((*dest).strptr + n)) = (u64)0u;
        return;
    }
;
    newlen = (oldlen + n);
    if (((newlen + (i64)1) > (i64)(*dest).allocated)) {
        newptr = (u8 *)mlib$pcm_alloc((newlen + (i64)1));
        memcpy((void *)newptr,(void *)(*dest).strptr,(u64)oldlen);
        (*dest).strptr = newptr;
        (*dest).allocated = mlib$allocbytes;
    }
;
    memcpy((void *)((*dest).strptr + oldlen),(void *)s,(u64)n);
    (*((*dest).strptr + newlen)) = (u64)0u;
    (*dest).length = newlen;
}

void mlib$gs_init(struct mlib$strbuffer *dest) {
    mlib$pcm_clearmem(dest,(i64)16);
}

void mlib$gs_free(struct mlib$strbuffer *dest) {
    if (!!((i64)(*dest).allocated)) {
        mlib$pcm_free((void *)(*dest).strptr,(i64)(*dest).allocated);
    }
;
}

void mlib$gs_str(struct mlib$strbuffer *dest,u8 *s) {
    mlib$strbuffer_add((struct mlib$strbuffer *)dest,s,(i64)-1);
}

void mlib$gs_char(struct mlib$strbuffer *dest,i64 c) {
        u8 s[16];
    s[((i64)1)-1] = (u64)c;
    s[((i64)2)-1] = (u64)0u;
    mlib$strbuffer_add((struct mlib$strbuffer *)dest,(u8 *)s,(i64)1);
}

void mlib$gs_strn(struct mlib$strbuffer *dest,u8 *s,i64 length) {
    mlib$strbuffer_add((struct mlib$strbuffer *)dest,s,length);
}

void mlib$gs_strvar(struct mlib$strbuffer *dest,struct mlib$strbuffer *s) {
    mlib$strbuffer_add((struct mlib$strbuffer *)dest,(*s).strptr,(i64)-1);
}

void mlib$gs_strint(struct mlib$strbuffer *dest,i64 a) {
    mlib$strbuffer_add((struct mlib$strbuffer *)dest,msysc$strint(a,0),(i64)-1);
}

void mlib$gs_strln(struct mlib$strbuffer *dest,u8 *s) {
    mlib$gs_str((struct mlib$strbuffer *)dest,s);
    mlib$gs_line((struct mlib$strbuffer *)dest);
}

void mlib$gs_strsp(struct mlib$strbuffer *dest,u8 *s) {
    mlib$gs_str((struct mlib$strbuffer *)dest,s);
    mlib$gs_str((struct mlib$strbuffer *)dest,(byte*)" ");
}

void mlib$gs_line(struct mlib$strbuffer *dest) {
    mlib$strbuffer_add((struct mlib$strbuffer *)dest,(byte*)"\r\n",(i64)-1);
}

i64 mlib$gs_getcol(struct mlib$strbuffer *dest) {
    return (i64)(*dest).length;
}

void mlib$gs_leftstr(struct mlib$strbuffer *dest,u8 *s,i64 w,i64 padch) {
        i64 col;
        i64 i;
        i64 n;
        i64 slen;
        u8 str[2560];
    col = (i64)(*dest).length;
    strcpy((u8 *)str,s);
    slen = strlen(s);
    n = (w - slen);
    if ((n > (i64)0)) {
        for (i=(i64)1;i<=n;++i) {
L503 :;
            str[((slen + i))-1] = (u64)padch;
L504 :;
        }
L505 :;
        ;
        str[(((slen + n) + (i64)1))-1] = (u64)0u;
    }
;
    mlib$gs_str((struct mlib$strbuffer *)dest,(u8 *)str);
}

void mlib$gs_leftint(struct mlib$strbuffer *dest,i64 a,i64 w,i64 padch) {
    mlib$gs_leftstr((struct mlib$strbuffer *)dest,msysc$strint(a,0),w,padch);
}

void mlib$gs_padto(struct mlib$strbuffer *dest,i64 col,i64 ch) {
        i64 n;
        u8 str[2560];
        i64 i;
    n = (col - (i64)(*dest).length);
    if ((n <= (i64)0)) {
        return;
    }
;
    for (i=(i64)1;i<=n;++i) {
L506 :;
        str[(i)-1] = (u64)ch;
L507 :;
    }
L508 :;
    ;
    str[((n + (i64)1))-1] = (u64)0u;
    mlib$gs_str((struct mlib$strbuffer *)dest,(u8 *)str);
}

void mlib$gs_println(struct mlib$strbuffer *dest,void *f) {
    if (((i64)(*dest).length == (i64)0)) {
        return;
    }
;
    (*((*dest).strptr + (i64)(*dest).length)) = (u64)0u;
    if ((f == 0)) {
        msysc$m_print_startcon();
        msysc$m_print_str((*dest).strptr,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
    else {
        msysc$m_print_startfile(f);
        msysc$m_print_str((*dest).strptr,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
}

i64 mlib$nextcmdparamnew(i64 *paramno,u8 **name,u8 **value,u8 *defext) {
        static i64 infile = (i64)0;
        static u8 *  filestart = 0;
        static u8 *  fileptr = 0;
        static byte colonseen = (byte)(i64)0;
        u8 *  q;
        u8 *  item;
        u8 *  fileext;
        static i64 atsize;
        static u8 str[300];
    //reenter:
L509 :;
;
    (*value) = 0;
    (*name) = 0;
    if (!!(infile)) {
        if ((mlib$readnextfileitem(&fileptr,&item) == (i64)0)) {
            mlib$pcm_free((void *)filestart,atsize);
            infile = (i64)0;
            goto L509 ;
;
        }
;
    }
    else {
        if (((*paramno) > msysc$ncmdparams)) {
            return (i64)0;
        }
;
        item = (*msysc$cmdparams)[((*paramno))];
        ++((*paramno));
        if (((u64)(*item) == '@')) {
            if (!!(infile)) {
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"Nested @",NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                exit((i64)1);
            }
;
            filestart = (fileptr = (u8 *)mlib$readfile((item + (i64)1)));
            if ((filestart == 0)) {
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"Can't open",NULL);
                msysc$m_print_str(item,NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                exit((i64)7);
            }
;
            infile = (i64)1;
            atsize = mlib$allocbytes;
            goto L509 ;
;
        }
;
        if (((u64)(*item) == ':')) {
            colonseen = (i64)1;
            return (i64)4;
        }
;
    }
;
    (*value) = 0;
    if (((u64)(*item) == '-')) {
        (*name) = (item + (!!((i64)colonseen) ? (i64)0 : (i64)1));
        q = strchr(item,(i32)':');
        if (!(!!(q))) {
            q = strchr(item,(i32)'=');
        }
;
        if (!!(q)) {
            (*value) = (q + (i64)1);
            (*q) = (u64)0u;
        }
;
        return (!!((i64)colonseen) ? (i64)5 : (i64)1);
    }
;
    fileext = mlib$extractext(item,(i64)0);
    (*name) = item;
    if (((i64)(u64)(*fileext) == (i64)0)) {
        strcpy((u8 *)str,(*name));
        if ((!!(defext) && !(!!((i64)colonseen)))) {
            (*name) = mlib$addext((u8 *)str,defext);
        }
;
    }
    else if ((!!(mlib$eqstring(fileext,(byte*)"dll")) || !!(mlib$eqstring(fileext,(byte*)"mcx")))) {
        return (!!((i64)colonseen) ? (i64)5 : (i64)3);
    }
;
    if (!!((i64)colonseen)) {
        return (i64)5;
    }
    else {
        return (i64)2;
    }
;
}

static i64 mlib$readnextfileitem(u8 **fileptr,u8 **item) {
        u8 *  p;
        u8 *  pstart;
        u8 *  pend;
        i64 n;
        static u8 str[256];
    p = (*fileptr);
    //reenter:
L510 :;
;
    L511 :;
    while (1) {
                {u64 $temp = (u64)(*p);
if (($temp==' ') || ($temp==(u64)9u) || ($temp==(u64)13u) || ($temp==(u64)10u)) {
            ++(p);
        }
        else if (($temp==(u64)26u) || ($temp==(u64)0u)) {
            return (i64)0;
        }
        else {
            goto L512 ;
        }
        };
    }
L512 :;
    ;
        {u64 $temp = (u64)(*p);
if (($temp=='!') || ($temp=='#')) {
        ++(p);
        L513 :;
                {u64 $temp = (u64)(*(p)++);
if (($temp==(u64)10u)) {
            goto L510 ;
;
        }
        else if (($temp==(u64)26u) || ($temp==(u64)0u)) {
            (*fileptr) = (p - (i64)1);
            return (i64)0;
        }
        else {
        }
        }goto L513 ;
L514 :;
        ;
    }
    };
        {u64 $temp = (u64)(*p);
if (($temp=='"')) {
        pstart = ++(p);
        L515 :;
        while (1) {
                        {u64 $temp = (u64)(*p);
if (($temp==(u64)0u) || ($temp==(u64)26u)) {
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"Unexpected EOF in @file",NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                exit((i64)8);
            }
            else if (($temp=='"')) {
                pend = (p)++;
                if (((u64)(*p) == ',')) {
                    ++(p);
                }
;
                goto L516 ;
            }
            };
            ++(p);
        }
L516 :;
        ;
    }
    else {
        pstart = p;
        L517 :;
        while (1) {
                        {u64 $temp = (u64)(*p);
if (($temp==(u64)0u) || ($temp==(u64)26u)) {
                pend = p;
                goto L518 ;
            }
            else if (($temp==' ') || ($temp==(u64)9u) || ($temp==',') || ($temp==(u64)13u) || ($temp==(u64)10u)) {
                pend = (p)++;
                goto L518 ;
            }
            };
            ++(p);
        }
L518 :;
        ;
    }
    };
    n = (pend - pstart);
    if ((n >= (i64)256)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"@file item too long",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        exit((i64)9);
    }
;
    memcpy(str,(void *)pstart,(u64)n);
    str[((n + (i64)1))-1] = (u64)0u;
    (*item) = (u8 *)str;
    (*fileptr) = p;
    return (i64)1;
}

void mlib$ipadstr(u8 *s,i64 width,u8 *padchar) {
        i64 n;
        i64 $av_1;
    n = strlen(s);
    $av_1 = (width - n);
    while ($av_1-- > 0) {
L519 :;
        strcat(s,padchar);
L520 :;
    }
L521 :;
    ;
}

u8 *mlib$padstr(u8 *s,i64 width,u8 *padchar) {
        static u8 str[256];
    strcpy((u8 *)str,s);
    mlib$ipadstr((u8 *)str,width,padchar);
    return (u8 *)str;
}

u8 *mlib$chr(i64 c) {
        static u8 str[8];
    str[((i64)1)-1] = (u64)c;
    str[((i64)2)-1] = (u64)0u;
    return (u8 *)str;
}

i64 mlib$cmpstring(u8 *s,u8 *t) {
        i64 res;
    if (((res = strcmp(s,t)) < (i64)0)) {
        return (i64)-1;
    }
    else if ((res > (i64)0)) {
        return (i64)1;
    }
    else {
        return (i64)0;
    }
;
}

i64 mlib$cmpstringn(u8 *s,u8 *t,i64 n) {
        i64 res;
    if (((res = strncmp(s,t,(u64)n)) < (i64)0)) {
        return (i64)-1;
    }
    else if ((res > (i64)0)) {
        return (i64)1;
    }
    else {
        return (i64)0;
    }
;
}

i64 mlib$eqstring(u8 *s,u8 *t) {
    return (i64)(strcmp(s,t) == (i64)0);
}

i64 mlib$cmpbytes(void *p,void *q,i64 n) {
        i64 res;
    if (((res = memcmp(p,q,(u64)n)) < (i64)0)) {
        return (i64)-1;
    }
    else if ((res > (i64)0)) {
        return (i64)1;
    }
    else {
        return (i64)0;
    }
;
}

i64 mlib$eqbytes(void *p,void *q,i64 n) {
    return (i64)(memcmp(p,q,(u64)n) == (i64)0);
}

void mlib$mseed(u64 a,u64 b) {
    mlib$seed[((i64)1)-1] = a;
    if (!!(b)) {
        mlib$seed[((i64)2)-1] = b;
    }
    else {
        mlib$seed[((i64)2)-1] ^= a;
    }
;
}

u64 mlib$mrandom(void) {
        i64 x;
        i64 y;
    x = (i64)mlib$seed[((i64)1)-1];
    y = (i64)mlib$seed[((i64)2)-1];
    mlib$seed[((i64)1)-1] = (u64)y;
    x ^= (x << (i64)23);
    mlib$seed[((i64)2)-1] = (u64)(((x ^ y) ^ (x >> (i64)17)) ^ (y >> (i64)26));
    return (u64)((i64)mlib$seed[((i64)2)-1] + y);
}

i64 mlib$mrandomp(void) {
    return ((i64)mlib$mrandom() & (i64)9223372036854775807);
}

i64 mlib$mrandomint(i64 n) {
    return (mlib$mrandomp() % n);
}

i64 mlib$mrandomrange(i64 a,i64 b) {
        i64 span;
    span = ((b - a) + (i64)1);
    if ((span <= (i64)0)) {
        return (i64)0;
    }
;
    return ((mlib$mrandomp() % span) + a);
}

r64 mlib$mrandomreal(void) {
        r64 x;
    L522 :;
    do {
        x = ((r64)mlib$mrandomp() / (double)9223372036854775800.);
L523 :;
    }
    while (!(x != (double)1.));
L524 :;
    ;
    return x;
}

r64 mlib$mrandomreal1(void) {
    return (r64)(mlib$mrandomp() / (i64)9223372036854775807);
}

u8 *mlib$readline(void) {
    msysc$m_read_conline();
    ;
    return msysc$rd_buffer;
}

void *mlib$findfunction(u8 *name) {
        i64 $av_1;
        i64 i;
        ($av_1 = msysc$m_get_nprocs());
    for (i=(i64)1;i<=$av_1;++i) {
L525 :;
        if (!!(mlib$eqstring(msysc$m_get_procname(i),name))) {
            return msysc$m_get_procaddr(i);
        }
;
L526 :;
    }
L527 :;
    ;
    return 0;
}

i64 mlib$roundtoblock(i64 n,i64 align) {
    if (((n & (align - (i64)1)) == (i64)0)) {
        return n;
    }
;
    return (n + (align - (n & (align - (i64)1))));
}

// START
void mlib$start(void) {

}

// START
void mclib$start(void) {

}

void mwindows$os_init(void) {
    mwindows$hconsole = GetStdHandle((u32)(i64)(u32)(i64)-11);
    mwindows$hconsolein = GetStdHandle((u32)(i64)(u32)(i64)-10);
    mwindows$lastkey.repeatcount = (i64)0;
    mwindows$keypending = (i64)0;
    SetConsoleCtrlHandler(0,(i64)1);
    SetConsoleMode(mwindows$hconsole,(u32)(i64)3);
    mwindows$init_flag = (i64)1;
}

i64 mwindows$os_execwait(u8 *cmdline,i64 newconsole,u8 *workdir) {
        u32 exitcode;
        i64 status;
        i64 cflags;
        struct mwindows$rstartupinfo si;
        struct mwindows$rprocess_information xpi;
    cflags = (i64)0;
    memset(&(si),0,104);
    memset(&(xpi),0,24);
    switch (newconsole) {
    case 0:;
        {
            cflags = (i64)32;
        }
        break;
    case 1:;
        {
            cflags = (i64)48;
        }
        break;
    case 2:;
        {
            cflags = (i64)48;
        }
        break;
    } //SW
;
    si.size = (i64)104;
    status = CreateProcessA(0,cmdline,0,0,(i64)1,(u32)cflags,0,0,&si,&xpi);
    if ((status == (i64)0)) {
        status = GetLastError();
        printf((byte*)"Winexec error: %lld\n",status);
        return (i64)-1;
    }
;
    WaitForSingleObject(xpi.process,(u32)(i64)4294967295);
    GetExitCodeProcess(xpi.process,&exitcode);
    CloseHandle(xpi.process);
    CloseHandle(xpi.thread);
    return (i64)exitcode;
}

i64 mwindows$os_execcmd(u8 *cmdline,i64 newconsole) {
        struct mwindows$rstartupinfo si;
        struct mwindows$rprocess_information xpi;
    memset(&(si),0,104);
    memset(&(xpi),0,24);
    si.size = (i64)104;
    CreateProcessA(0,cmdline,0,0,(i64)1,(u32)((i64)32 | (!!(newconsole) ? (i64)16 : (i64)0)),0,0,&si,&xpi);
    CloseHandle(xpi.process);
    CloseHandle(xpi.thread);
    return (i64)1;
}

i64 mwindows$os_getch(void) {
        i64 k;
    k = (mwindows$os_getchx() & (i64)255);
    return k;
}

i64 mwindows$os_kbhit(void) {
        u32 count;
    if (!(!!(mwindows$init_flag))) {
        mwindows$os_init();
    }
;
    GetNumberOfConsoleInputEvents(mwindows$hconsolein,&count);
    return (i64)((i64)count > (i64)1);
}

u64 mwindows$os_getdllinst(u8 *name) {
        void *  hinst;
    hinst = LoadLibraryA(name);
    return (u64)hinst;
}

void *mwindows$os_getdllprocaddr(i64 hinst,u8 *name) {
    return GetProcAddress((void *)hinst,name);
}

void mwindows$os_initwindows(void) {
    mwindows$os_init();
    mwindows$os_gxregisterclass((byte*)"pcc001");
}

void mwindows$os_gxregisterclass(u8 *classname) {
        struct mwindows$rwndclassex r;
        static byte registered;
    if (!!((i64)registered)) {
        return;
    }
;
    memset(&(r),0,80);
    r.size = (i64)80;
    r.style = (i64)40;
    r.wndproc = (void (*)(void))&mwindows$mainwndproc;
    r.instance = 0;
    r.icon = 0;
    r.cursor = LoadCursorA(0,(u8 *)(void *)(i64)32512);
    r.background = (void *)(i64)16;
    r.menuname = 0;
    r.classname = classname;
    r.iconsm = 0;
    if ((RegisterClassExA(&r) == (i64)0)) {
        printf((byte*)"Regclass error: %lld %lld\n",classname,GetLastError());
        exit((i64)1);
    }
;
    registered = (i64)1;
}

i64 mwindows$mainwndproc(void *hwnd,u32 message,u64 wparam,u64 lparam) {
        struct mwindows$rmsg m;
        i64 result;
        static i64 count = (i64)0;
    m.hwnd = hwnd;
    m.message = (i64)message;
    m.wparam = wparam;
    m.lparam = lparam;
    m.pt.x = (i64)0;
    m.pt.y = (i64)0;
    if (!!(mwindows$wndproc_callbackfn)) {
        result = ((*mwindows$wndproc_callbackfn))(&m);
    }
    else {
        result = (i64)0;
    }
;
    if (((i64)m.message == (i64)2)) {
        return (i64)0;
    }
;
    if (!(!!(result))) {
        return DefWindowProcA(hwnd,(u32)(i64)message,wparam,lparam);
    }
    else {
        return (i64)0;
    }
;
}

void mwindows$os_setmesshandler(void *addr) {
    mwindows$wndproc_callbackfn = (i64 (*)(void *))addr;
}

i64 mwindows$os_getchx(void) {
        i64 count;
        i64 charcode;
        i64 keyshift;
        i64 keycode;
        i64 altdown;
        i64 ctrldown;
        i64 shiftdown;
        i64 capslock;
    if (!(!!(mwindows$init_flag))) {
        mwindows$os_init();
    }
;
    if (!!(mwindows$keypending)) {
        mwindows$lastkey = mwindows$pendkey;
        mwindows$keypending = (i64)0;
    }
    else {
        if (((i64)mwindows$lastkey.repeatcount == (i64)0)) {
            L528 :;
            do {
                count = (i64)0;
                ReadConsoleInputA(mwindows$hconsolein,&mwindows$lastkey,(u32)(i64)1,&count);
L529 :;
            }
            while (!(((i64)mwindows$lastkey.eventtype == (i64)1) && ((i64)mwindows$lastkey.keydown == (i64)1)));
L530 :;
            ;
        }
;
    }
;
    altdown = (!!(((i64)mwindows$lastkey.controlkeystate & (i64)3)) ? (i64)1 : (i64)0);
    ctrldown = (!!(((i64)mwindows$lastkey.controlkeystate & (i64)12)) ? (i64)1 : (i64)0);
    shiftdown = (!!(((i64)mwindows$lastkey.controlkeystate & (i64)16)) ? (i64)1 : (i64)0);
    capslock = (!!(((i64)mwindows$lastkey.controlkeystate & (i64)128)) ? (i64)1 : (i64)0);
    --(mwindows$lastkey.repeatcount);
    charcode = (i64)mwindows$lastkey.asciichar;
    keycode = ((i64)mwindows$lastkey.virtualkeycode & (i64)255);
    if ((charcode < (i64)0)) {
        if ((charcode < (i64)-128)) {
            charcode = (i64)0;
        }
        else {
            charcode += (i64)256;
        }
;
    }
;
    if (((!!(altdown) && !!(ctrldown)) && (charcode == (i64)166))) {
        altdown = (ctrldown = (i64)0);
    }
    else {
        if ((!!(altdown) || !!(ctrldown))) {
            charcode = (i64)0;
            if (((keycode >= (i64)65) && (keycode <= (i64)90))) {
                charcode = (keycode - (i64)64);
            }
;
        }
;
    }
;
    keyshift = ((((capslock << (i64)3) | (altdown << (i64)2)) | (ctrldown << (i64)1)) | shiftdown);
    return (((keyshift << (i64)24) | (keycode << (i64)16)) | charcode);
}

u8 *mwindows$os_getos(void) {
    if (((i64)64 == (i64)32)) {
        return (byte*)"W32";
    }
    else {
        return (byte*)"W64";
    }
;
}

i64 mwindows$os_gethostsize(void) {
    return (i64)64;
}

i64 mwindows$os_shellexec(u8 *opc,u8 *file) {
    return system(file);
}

void mwindows$os_sleep(i64 a) {
    Sleep((u32)a);
}

void *mwindows$os_getstdin(void) {
    return fopen((byte*)"con",(byte*)"rb");
}

void *mwindows$os_getstdout(void) {
    return fopen((byte*)"con",(byte*)"wb");
}

u8 *mwindows$os_gethostname(void) {
        static u8 name[300];
    GetModuleFileNameA(0,(u8 *)name,(u32)(i64)300);
    return (u8 *)name;
}

u8 *mwindows$os_getmpath(void) {
    return (byte*)"C:\\m\\";
}

i64 mwindows$os_clock(void) {
    return clock();
}

i64 mwindows$os_ticks(void) {
    return (i64)GetTickCount64();
}

i64 mwindows$os_hptimer(void) {
        i64 t;
    QueryPerformanceCounter(&t);
    return t;
}

i64 mwindows$os_iswindows(void) {
    return (i64)1;
}

void mwindows$os_getsystime(struct mwindows$rsystemtime *tm) {
    GetLocalTime((struct mwindows$rsystemtime *)tm);
}

void mwindows$os_peek(void) {
        i64 ticks;
        static i64 lastticks;
        byte m[100];
    ticks = (i64)GetTickCount64();
    if (((ticks - lastticks) >= (i64)1000)) {
        lastticks = ticks;
        PeekMessageA(&m,0,(u32)(i64)0,(u32)(i64)0,(u32)(i64)0);
    }
;
}

byte *mwindows$os_allocexecmem(i64 n) {
        byte *  p;
        u32 oldprot;
        i64 status;
    p = (byte *)VirtualAlloc(0,(u32)n,(u32)(i64)12288,(u32)(i64)1);
    if ((p == 0)) {
        return 0;
    }
;
    status = VirtualProtect(p,(u32)n,(u32)(i64)64,(u32 *)&oldprot);
    if ((status == (i64)0)) {
        return 0;
    }
;
    return p;
}

// START
void mwindows$start(void) {

}

u64 mwindllc$os_calldllfunction(void (*fnaddr)(void),i64 retcode,i64 nargs,i64 (*args)[],byte (*argcodes)[]) {
    if ((retcode == (i64)73)) {
        return (u64)mwindllc$calldll_cint((void (*)(void))fnaddr,args,nargs);
    }
    else {
        return (u64)mwindllc$calldll_creal((void (*)(void))fnaddr,args,nargs);
    }
;
}

u64 mwindllc$os_pushargs(u64 (*args)[],i64 nargs,i64 nextra,void (*fnaddr)(void),i64 isfloat) {
    return mwindllc$os_calldllfunction((void (*)(void))fnaddr,(!!(isfloat) ? (i64)0 : (i64)73),nargs,(i64 (*)[])args,0);
}

static i64 mwindllc$calldll_cint(void (*fnaddr)(void),i64 (*params)[],i64 nparams) {
    switch (nparams) {
    case 0:;
        {
            return ((*(i64 (*)(void))fnaddr))();
        }
        break;
    case 1:;
        {
            return ((*(i64 (*)(i64,...))fnaddr))((*params)[((i64)1)-1]);
        }
        break;
    case 2:;
        {
            return ((*(i64 (*)(i64,i64,...))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1]);
        }
        break;
    case 3:;
        {
            return ((*(i64 (*)(i64,i64,i64,...))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1]);
        }
        break;
    case 4:;
        {
            return ((*(i64 (*)(i64,i64,i64,i64,...))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1]);
        }
        break;
    case 5:;
        {
            return ((*(i64 (*)(i64,i64,i64,i64,i64,...))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1]);
        }
        break;
    case 6:;
        {
            return ((*(i64 (*)(i64,i64,i64,i64,i64,i64,...))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1]);
        }
        break;
    case 9:;
        {
            return ((*(i64 (*)(i64,i64,i64,i64,i64,i64,i64,i64,i64,...))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1],(*params)[((i64)7)-1],(*params)[((i64)8)-1],(*params)[((i64)9)-1]);
        }
        break;
    case 10:;
        {
            return ((*(i64 (*)(i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,...))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1],(*params)[((i64)7)-1],(*params)[((i64)8)-1],(*params)[((i64)9)-1],(*params)[((i64)10)-1]);
        }
        break;
    case 11:;
        {
            return ((*(i64 (*)(i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,...))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1],(*params)[((i64)7)-1],(*params)[((i64)8)-1],(*params)[((i64)9)-1],(*params)[((i64)10)-1],(*params)[((i64)11)-1]);
        }
        break;
    case 12:;
        {
            return ((*(i64 (*)(i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,...))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1],(*params)[((i64)7)-1],(*params)[((i64)8)-1],(*params)[((i64)9)-1],(*params)[((i64)10)-1],(*params)[((i64)11)-1],(*params)[((i64)12)-1]);
        }
        break;
    case 14:;
        {
            return ((*(i64 (*)(i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,...))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1],(*params)[((i64)7)-1],(*params)[((i64)8)-1],(*params)[((i64)9)-1],(*params)[((i64)10)-1],(*params)[((i64)11)-1],(*params)[((i64)12)-1],(*params)[((i64)13)-1],(*params)[((i64)14)-1]);
        }
        break;
    default: {
        msysc$m_print_startcon();
        msysc$m_print_i64(nparams,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"calldll/c/int unsupported # of params",NULL);
        msysc$m_print_i64(nparams,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        exit((i64)1);
    }
    } //SW
;
    return (i64)0;
}

static i64 mwindllc$calldll_creal(void (*fnaddr)(void),i64 (*params)[],i64 nparams) {
        r64 x;
    switch (nparams) {
    case 0:;
        {
            return (i64)((*(r64 (*)(void))fnaddr))();
        }
        break;
    case 1:;
        {
            mwindllc$os_dummycall((r64)(*params)[((i64)1)-1],(r64)(*params)[((i64)2)-1],(r64)(*params)[((i64)3)-1],(r64)(*params)[((i64)4)-1]);
            x = ((*(r64 (*)(i64,...))fnaddr))((*params)[((i64)1)-1]);
        }
        break;
    case 2:;
        {
            x = ((*(r64 (*)(i64,i64,...))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1]);
        }
        break;
    default: {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"calldll/c/real too many params",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        exit((i64)1);
    }
    } //SW
;
    return *(i64*)&x;
}

void mwindllc$os_dummycall(r64 a,r64 b,r64 c,r64 d) {
}

// START
void mwindllc$start(void) {

}


/* ********** End of C Code ********** */

