
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
struct mm_decls$tokenrec;
struct mm_decls$overloadrec;
struct mm_decls$procrec;
struct mm_decls$typenamerec;
struct mm_decls$posrec;
struct mm_decls$uflagsrec;
struct mm_decls$strec;
struct mm_decls$unitrec;
struct mm_decls$fwdrec;
struct mm_decls$modulerec;
struct mm_decls$subprogrec;
struct mc_genc$stlinkrec;
struct mc_decls$opndrec;
struct mc_decls$mclrec;
struct mc_decls$constrec;
struct mc_decls$relocrec;
struct mc_decls$dbuffer;
struct msysc$procinforec;
struct msysc$fmtrec;
struct mlib$strbuffer;
struct mlinux$termios;
struct mlinux$rsystemtime;

/* Struct Definitions */
struct mm_decls$tokenrec {
    byte symbol;
    byte subcode;
    u16 spare;
    u32 pos;
    union {
        struct mm_decls$strec *  symptr;
        i64 value;
        r64 xvalue;
        u64 uvalue;
        u8 *  svalue;
    };
};

struct mm_decls$overloadrec {
    i32 amode;
    i32 bmode;
    i32 rmode;
    i16 moduleno;
    i16 flags;
    struct mm_decls$unitrec *  fncode;
    struct mm_decls$overloadrec* nextoverload;
};

struct mm_decls$procrec {
    struct mm_decls$strec *  def;
    struct mm_decls$procrec* nextproc;
};

struct mm_decls$typenamerec {
    struct mm_decls$strec *  owner;
    struct mm_decls$strec *  defa;
    union {
        struct mm_decls$strec *  defb;
        struct mm_decls$strec *  def;
    };
    i32 *  pmode;
};

struct mm_decls$posrec {
    u32 pos;
};

struct mm_decls$uflagsrec {
    byte codes[7];
    byte ulength;
};

struct mm_decls$strec {
    u8 *  name;
    struct mm_decls$strec* owner;
    struct mm_decls$strec* deflist;
    struct mm_decls$strec* deflistx;
    struct mm_decls$strec* nextdef;
    struct mm_decls$strec* nextdupl;
    struct mm_decls$strec* firstdupl;
    struct mm_decls$unitrec *  code;
    i32 mode;
    byte namelen;
    byte symbol;
    byte nameid;
    byte subcode;
    union {
        i32 index;
        i32 labelno;
    };
    i32 offset;
    u32 pos;
    u16 flags;
    byte moduleno;
    byte subprogno;
    struct mm_decls$unitrec *  equivvar;
    struct {
        u8 *  truename;
        struct mm_decls$strec* paramlist;
        byte asmused;
        byte dllindex;
        byte fflang;
        byte nretvalues;
        byte varparams;
        byte isthreaded;
        i16 dummy1;
    };
    struct {
        struct mm_decls$strec* equivfield;
        struct mm_decls$uflagsrec uflags;
        i32 baseclass;
        byte bitfieldwidth;
        byte align;
        byte bitoffset;
        byte equivoffset;
    };
    struct {
        struct mm_decls$strec* nextparam;
        byte parammode;
        byte optional;
        byte variadic;
        byte dummy3;
    };
    i16 nrefs;
    i16 regsize;
    i16 maxalign;
    struct mm_decls$fwdrec *  fwdrefs;
    byte reftype;
    byte segment;
    i32 stindex;
    i16 importindex;
    struct mm_decls$strec* nextsym;
    i16 impindex;
    i16 expindex;
    byte reg;
    byte scope;
    byte equals;
};

struct mm_decls$unitrec {
    byte tag;
    byte simple;
    byte ifretflag;
    byte spare;
    u32 pos;
    struct mm_decls$unitrec* nextunit;
    union {
        struct {
            union {
                struct mm_decls$unitrec* a;
                struct mm_decls$strec *  def;
                struct mm_decls$strec *  labeldef;
                i64 value;
                u64 uvalue;
                r64 xvalue;
                u8 *  svalue;
                i64 range_lower;
            };
            union {
                struct mm_decls$unitrec* b;
                i64 range_upper;
            };
            struct mm_decls$unitrec* c;
        };
        struct mm_decls$unitrec *  abc[3];
    };
    union {
        struct {
            u32 slength;
            byte isastring;
        };
        struct {
            byte dottedname;
            byte avcode;
        };
        union {
            struct {
                byte reg;
                byte regix;
                byte scale;
                byte prefixmode;
                byte regsize;
                byte cond;
                byte spare2;
                byte spare3;
            };
            u64 reginfo;
        };
        union {
            u32 length;
            byte makearray;
        };
        byte addroffirst;
        u32 offset;
        i32 whenlabel;
        i32 swapvar;
        struct {
            union {
                i16 bitopindex;
                i16 opcindex;
                i16 fnindex;
                i16 condcode;
                i16 asmopcode;
                i16 bfcode;
            };
        };
        i32 index;
        byte cmpgenop[4];
    };
    i32 mode;
    i32 convmode;
    byte moduleno;
    byte subprogno;
    byte initlet;
    byte isconst;
    byte resultflag;
    byte pclop;
    byte istrueconst;
    byte memmode;
};

struct mm_decls$fwdrec {
    struct mm_decls$fwdrec* nextfwd;
    i32 offset;
    i16 reltype;
    i16 seg;
};

struct mm_decls$modulerec {
    u8 *  name;
    struct mm_decls$strec *  stmodule;
    struct mm_decls$strec *  stsubprog;
    u8 *  path;
    struct mm_decls$strec *  ststart;
    struct mm_decls$strec *  stmain;
    struct mm_decls$strec *  stmacro;
    struct mm_decls$unitrec *  modulecode;
    i16 fileno;
    i16 issyslib;
    i16 subprogno;
};

struct mm_decls$subprogrec {
    u8 *  name;
    struct mm_decls$strec *  stsubprog;
    i64 issyslib;
    u8 *  path;
    i16 firstmodule;
    i64 fileno;
};

struct mc_genc$stlinkrec {
    struct mm_decls$strec *  def;
    struct mc_genc$stlinkrec* nextsymbol;
};

struct mc_decls$opndrec {
    union {
        struct mm_decls$strec *  def;
        i64 value;
        r64 xvalue;
        u8 *  svalue;
        i64 labelno;
        i64 sysfn;
    };
    u16 misc;
    byte reg;
    byte regix;
    i32 offset;
};

struct mc_decls$mclrec {
    struct mc_decls$mclrec* nextmcl;
    struct mc_decls$opndrec *  a;
    struct mc_decls$opndrec *  b;
    byte opcode;
    byte cond;
    byte c;
    byte spare;
    u32 seqno;
};

struct mc_decls$constrec {
    union {
        i64 value;
        r64 xvalue;
        u8 *  svalue;
    };
    struct mc_decls$constrec* nextconst;
    i64 labelno;
};

struct mc_decls$relocrec {
    struct mc_decls$relocrec* nextreloc;
    i64 reloctype;
    i64 offset;
    i64 stindex;
};

struct mc_decls$dbuffer {
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

struct mlinux$termios {
    i32 c_iflag;
    i32 c_oflag;
    i32 c_cflag;
    i32 c_lflag;
    u8 c_line;
    u8 c_cc[32];
    byte filler[3];
    i32 c_ispeed;
    i32 c_ospeed;
};

struct mlinux$rsystemtime {
    i32 year;
    i32 month;
    i32 dayofweek;
    i32 day;
    i32 hour;
    i32 minute;
    i32 second;
    i64 milliseconds;
};


/* PROCDECLS */
int main(int, char**, char**);
static void mm_cli$do_loadmodules(void);
static void mm_cli$do_parse(void);
static void mm_cli$do_name(void);
static void mm_cli$do_type(void);
static void mm_cli$initdata(void);
static void mm_cli$getinputoptions(void);
static void mm_cli$do_option(i64 sw,u8 *value);
static void mm_cli$showcaption(void);
void mm_cli$showhelp(void);
void mm_cli$initassemsymbols(void);
static void mm_cli$do_writeexports(void);
static u8 *mm_cli$getoutfilename(u8 *file,u8 *ext);
static void mm_cli$fixstartprocs(void);
static struct mm_decls$strec *mm_cli$addstartproc(struct mm_decls$strec *owner,u8 *name,i64 scope,i64 moduleno);
static void mm_cli$stepruncount(void);
void mm_cli$showmoduleinfo(void);
void mm_cli$start(void);
struct mm_decls$unitrec *mm_assem$readassemline(void);
struct mm_decls$unitrec *mm_assem$readassemblock(void);
static struct mm_decls$unitrec *mm_assem$assembleline(i64 oneline);
static struct mm_decls$unitrec *mm_assem$readassemopnd(void);
void mm_assem$start(void);
void mc_blockc$evalunit(struct mm_decls$unitrec *p);
void mc_blockc$evalstmt(struct mm_decls$unitrec *p);
void mc_blockc$evalunitc(struct mm_decls$unitrec *p);
static void mc_blockc$evalblock(struct mm_decls$unitrec *p,i64 braces);
void mc_blockc$do_block(struct mm_decls$unitrec *p,i64 braces);
void mc_blockc$evalblocklab(struct mm_decls$unitrec *p,i64 lab1,i64 lab2,struct mm_decls$unitrec *pincr,i64 labincr,i64 braces);
void mc_blockc$do_blocklab(struct mm_decls$unitrec *p,i64 lab1,i64 lab2,struct mm_decls$unitrec *pincr,i64 labincr,i64 braces);
static void mc_blockc$do_const(struct mm_decls$unitrec *p);
static void mc_blockc$do_return(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a);
static void mc_blockc$do_assign(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mc_blockc$do_bin(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mc_blockc$do_binto(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mc_blockc$do_unary(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a);
static void mc_blockc$do_convert(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a);
static void mc_blockc$do_if(struct mm_decls$unitrec *pcond,struct mm_decls$unitrec *plist,struct mm_decls$unitrec *pelse);
static void mc_blockc$do_ifx(struct mm_decls$unitrec *pcond,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,i64 addrof);
static void mc_blockc$do_call(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,i64 isfn);
static void mc_blockc$do_do(struct mm_decls$unitrec *a);
static i64 mc_blockc$definelabel(void);
static i64 mc_blockc$createfwdlabel(void);
static void mc_blockc$definefwdlabel(i64 lab);
static void mc_blockc$stacklooplabels(i64 a,i64 b,i64 c);
static void mc_blockc$unstacklooplabels(void);
static i64 mc_blockc$findlooplabel(i64 k,i64 n);
static void mc_blockc$do_exit(struct mm_decls$unitrec *p);
static void mc_blockc$do_to(struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,struct mm_decls$unitrec *c);
static void mc_blockc$do_while(struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,struct mm_decls$unitrec *pincr);
static void mc_blockc$do_repeat(struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mc_blockc$do_forup(struct mm_decls$unitrec *p,struct mm_decls$unitrec *pindex,struct mm_decls$unitrec *pfrom,struct mm_decls$unitrec *pbody);
static void mc_blockc$do_forall(struct mm_decls$unitrec *p,struct mm_decls$unitrec *pindex,struct mm_decls$unitrec *plist,struct mm_decls$unitrec *pbody);
static void mc_blockc$dxlabel(i64 lab);
static void mc_blockc$do_print(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mc_blockc$do_read(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a);
static void mc_blockc$do_readln(struct mm_decls$unitrec *a);
static void mc_blockc$do_index(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *pindex);
static void mc_blockc$do_case(struct mm_decls$unitrec *p,struct mm_decls$unitrec *pindex,struct mm_decls$unitrec *pwhenthen,struct mm_decls$unitrec *pelse);
static void mc_blockc$do_switch(struct mm_decls$unitrec *p,struct mm_decls$unitrec *pindex,struct mm_decls$unitrec *pwhenthen,struct mm_decls$unitrec *pelse);
static void mc_blockc$do_goto(struct mm_decls$unitrec *a);
static void mc_blockc$do_max(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mc_blockc$do_maxto(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static i64 mc_blockc$isexpr(struct mm_decls$unitrec *p);
static void mc_blockc$do_swap(struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mc_blockc$do_inrange(struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,struct mm_decls$unitrec *c);
static void mc_blockc$do_inset(struct mm_decls$unitrec *a,struct mm_decls$unitrec *p);
static void mc_blockc$do_cmpchain(struct mm_decls$unitrec *p);
static void mc_blockc$do_blockcopy(struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mc_blockc$do_select(struct mm_decls$unitrec *pindex,struct mm_decls$unitrec *plist,struct mm_decls$unitrec *pdefault);
static void mc_blockc$do_dotindex(struct mm_decls$unitrec *a,struct mm_decls$unitrec *i);
static void mc_blockc$do_supportcall(u8 *fnname,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,struct mm_decls$unitrec *c,struct mm_decls$unitrec *d);
static void mc_blockc$applymemmode(struct mm_decls$unitrec *p);
static void mc_blockc$domaths2(u8 *name,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mc_blockc$do_typepun(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a);
void mc_blockc$start(void);
void mm_decls$start(void);
void mm_diags$printoverloads(void *f);
void mm_diags$printst(void *f,struct mm_decls$strec *p,i64 level);
static void mm_diags$printstrec(void *f,struct mm_decls$strec *p,i64 level);
void mm_diags$printstflat(void *f);
void mm_diags$printcode(void *f,u8 *caption);
void mm_diags$printunit(struct mm_decls$unitrec *p,i64 level,u8 *prefix,void *dev);
static void mm_diags$printunitlist(void *dev,struct mm_decls$unitrec *p,i64 level,u8 *prefix);
static u8 *mm_diags$getprefix(i64 level,u8 *prefix,struct mm_decls$unitrec *p);
static u8 *mm_diags$getlineinfok(void);
void mm_diags$printmodelist(void *f);
void mm_diags$showprojectinfo(void *dev);
void mm_diags$showlogfile(void);
static void mm_diags$showstflat(u8 *caption,void *f);
static void mm_diags$showsttree(u8 *caption,void *f);
void mm_diags$showast(u8 *filename);
void mm_diags$printsymbol(struct mm_decls$tokenrec *lp);
void mm_diags$start(void);
void mm_export$writeexports(u8 *outfile,u8 *modulename);
static void mm_export$exportstatic(struct mm_decls$strec *d);
static void mm_export$exportconst(struct mm_decls$strec *d);
static void mm_export$exportproc(struct mm_decls$strec *d);
static void mm_export$wxstr(u8 *s);
static void mm_export$wxstrln(u8 *s);
static void mm_export$wxline(void);
static void mm_export$exportrecord(struct mm_decls$strec *d);
static void mm_export$wxmode(i64 mode);
void mm_export$start(void);
void mc_genc$codegen_clang(void);
static void mc_genc$do_infoheader(u8 *cfilename);
static void mc_genc$do_cheader(u8 *cfilename);
static void mc_genc$do_alltypes(void);
static void mc_genc$scansymbol(struct mm_decls$strec *d);
static void mc_genc$addsymbol(struct mm_decls$strec *d);
static void mc_genc$do_typedef_fwd(struct mm_decls$strec *d);
static void mc_genc$do_typedef(struct mm_decls$strec *d);
static void mc_genc$do_allprocdecls(void);
static void mc_genc$do_allvars(void);
static void mc_genc$do_allprocdefs(void);
static void mc_genc$do_procdecl(struct mm_decls$strec *d);
static void mc_genc$do_vardef(struct mm_decls$strec *d);
static void mc_genc$do_dllvar(struct mm_decls$strec *d);
static void mc_genc$genprocdef(struct mm_decls$strec *p);
static void mc_genc$do_maininit(void);
static void mc_genc$do_startinit(struct mm_decls$strec *p);
static void mc_genc$do_mainterm(void);
static void mc_genc$writefn_nprocs(void);
static void mc_genc$writefn_names(void);
static void mc_genc$writefn_addresses(void);
static void mc_genc$genlocalvar(struct mm_decls$strec *d);
static void mc_genc$docallproc(struct mm_decls$strec *d);
void mc_genc$start(void);
void mm_lex$lexreadtoken(void);
void mm_lex$lex(void);
void mm_lex$lexsetup(void);
void mm_lex$printstrn(u8 *s,i64 length);
static void mm_lex$readrawstring(void);
static void mm_lex$lookup(u8 *name,i64 length,i64 hashindex0);
static i64 mm_lex$lookupsys(u8 *name);
static i64 mm_lex$gethashvaluez(u8 *s);
static void mm_lex$inithashtable(void);
void mm_lex$printhashtable(void);
void mm_lex$addreservedword(u8 *name,i64 symbol,i64 subcode,i64 regsize);
static i64 mm_lex$dolexdirective(i64 index);
void mm_lex$startlex(i64 fileno);
void mm_lex$start(void);
struct mm_decls$strec *mm_lex$addnamestr(u8 *name);
void mm_lex$ps(u8 *caption);
void mm_lex$psnext(u8 *caption);
void mm_lex$psx(u8 *caption);
void mm_lex$stacksource(i64 fileno,i64 isimport);
void mm_lex$unstacksource(void);
static void mm_lex$readarraystring(i64 prefix);
static i64 mm_lex$setinttype(u64 a);
static void mm_lex$readrawxname(void);
static void mm_lex$lxerror_s(u8 *mess,u8 *s);
static void mm_lex$lxreadstring(i64 termchar);
static void mm_lex$readdec(void);
static void mm_lex$readhex(void);
static void mm_lex$readoct(void);
static void mm_lex$readbin(void);
static void mm_lex$readreal(void);
struct mm_decls$strec *mm_lib$newstrec(void);
struct mm_decls$strec *mm_lib$getduplnameptr(struct mm_decls$strec *owner,struct mm_decls$strec *symptr,i64 id);
void mm_lib$adddef(struct mm_decls$strec *owner,struct mm_decls$strec *p);
struct mm_decls$unitrec *mm_lib$createname(struct mm_decls$strec *p);
struct mm_decls$unitrec *mm_lib$createunit0(i64 tag);
struct mm_decls$unitrec *mm_lib$createunit1(i64 tag,struct mm_decls$unitrec *p);
struct mm_decls$unitrec *mm_lib$createunit2(i64 tag,struct mm_decls$unitrec *p,struct mm_decls$unitrec *q);
struct mm_decls$unitrec *mm_lib$createunit3(i64 tag,struct mm_decls$unitrec *p,struct mm_decls$unitrec *q,struct mm_decls$unitrec *r);
void mm_lib$insertunit(struct mm_decls$unitrec *p,i64 tag);
void mm_lib$deleteunit(struct mm_decls$unitrec *p,struct mm_decls$unitrec *q);
struct mm_decls$unitrec *mm_lib$createconstunit(u64 a,i64 t);
struct mm_decls$unitrec *mm_lib$createstringconstunit(u8 *s,i64 length);
i64 mm_lib$newtypename(struct mm_decls$strec *a,struct mm_decls$strec *b);
i64 mm_lib$createusertype(struct mm_decls$strec *stname);
i64 mm_lib$createusertypefromstr(u8 *name);
struct mm_decls$unitrec *mm_lib$getrangelwbunit(struct mm_decls$unitrec *p);
struct mm_decls$unitrec *mm_lib$getrangeupbunit(struct mm_decls$unitrec *p);
i64 mm_lib$createarraymode(struct mm_decls$strec *owner,i64 target,struct mm_decls$unitrec *dimexpr,i64 typedefx);
static i64 mm_lib$sameunit(struct mm_decls$unitrec *p,struct mm_decls$unitrec *q,struct mm_decls$strec *powner,struct mm_decls$strec *qowner);
i64 mm_lib$createarraymodek(struct mm_decls$strec *owner,i64 target,i64 lower,i64 length,i64 typedefx);
u8 *mm_lib$nextautotype(void);
i64 mm_lib$createslicemode(struct mm_decls$strec *owner,i64 slicetype,i64 target,struct mm_decls$unitrec *dimexpr,i64 typedefx);
i64 mm_lib$createslicemodek(struct mm_decls$strec *owner,i64 target,i64 lower,i64 typedefx);
i64 mm_lib$createrefmode(struct mm_decls$strec *owner,i64 target,i64 typedefx);
i64 mm_lib$createrefprocmode(struct mm_decls$strec *owner,struct mm_decls$strec *stproc,struct mm_decls$strec *paramlist,i64 kwd,i64 prettype,i64 typedefx);
void mm_lib$copyttvalues(i64 dest,i64 source);
u8 *mm_lib$getdottedname(struct mm_decls$strec *p);
struct mm_decls$strec *mm_lib$getavname(struct mm_decls$strec *owner,i64 id);
void mm_lib$unionstr_clear(struct mm_decls$uflagsrec *u);
void mm_lib$unionstr_append(struct mm_decls$uflagsrec *u,i64 c);
void mm_lib$unionstr_concat(struct mm_decls$uflagsrec *u,struct mm_decls$uflagsrec *v);
i64 mm_lib$unionstr_last(struct mm_decls$uflagsrec *u);
void mm_lib$unionstr_copy(struct mm_decls$uflagsrec *u,struct mm_decls$uflagsrec *v);
i64 mm_lib$createrecordmode(struct mm_decls$strec *owner,i64 typedefx);
i64 mm_lib$createtuplemode(struct mm_decls$strec *owner,i64 (*elements)[],i64 elementslen,i64 typedefx);
i64 mm_lib$convertstring(u8 *s,u8 *t);
struct mlib$strbuffer *mm_lib$strexpr(struct mm_decls$unitrec *p);
void mm_lib$jevalx(struct mlib$strbuffer *dest,struct mm_decls$unitrec *p);
u8 *mm_lib$strmode(i64 m,i64 expand);
u8 *mm_lib$strmode2(i64 m,i64 expand);
void mm_lib$istrmode(i64 m,i64 expand,u8 *dest);
void mm_lib$addtoproclist(struct mm_decls$strec *d);
void mm_lib$addstatic(struct mm_decls$strec *d);
void mm_lib$addexpconst(struct mm_decls$strec *d);
u8 *mm_lib$typename(i64 m);
struct mm_decls$unitrec *mm_lib$allocunitrec(void);
struct mm_decls$strec *mm_lib$createdupldef(struct mm_decls$strec *owner,struct mm_decls$strec *symptr,i64 id);
struct mm_decls$strec *mm_lib$createnewmoduledef(struct mm_decls$strec *owner,struct mm_decls$strec *symptr,i64 id);
struct mm_decls$unitrec *mm_lib$duplunit(struct mm_decls$unitrec *p,i64 lineno);
i64 mm_lib$checkblockreturn(struct mm_decls$unitrec *p);
i64 mm_lib$isconstunit(struct mm_decls$unitrec *a);
void mm_lib$getownername(struct mm_decls$strec *d,u8 *dest);
i64 mm_lib$getalignment(i64 m);
i64 mm_lib$ispoweroftwo(i64 x);
void mm_lib$addlistunit(struct mm_decls$unitrec **ulist,struct mm_decls$unitrec **ulistx,struct mm_decls$unitrec *p);
i64 mm_lib$storemode(struct mm_decls$strec *owner,i64 m,i32 *pmode);
i64 mm_lib$gettypebase(i64 m);
void mm_lib$writegsfile(u8 *filename,struct mlib$strbuffer *d);
void mm_lib$addtolog(u8 *filename,void *logdest);
struct mm_decls$strec *mm_lib$getprocretmodes(struct mm_decls$unitrec *p);
i64 mm_lib$getmemmode(struct mm_decls$unitrec *p);
i64 mm_lib$getpclmode(i64 t);
u8 *mm_lib$getfullname(struct mm_decls$strec *d);
u8 *mm_lib$getbasename(u8 *s);
void mm_lib$start(void);
void mc_libc$cccomment(u8 *s);
void mc_libc$ccblank(void);
void mc_libc$cclinecomment(u8 *s);
void mc_libc$ccchar(i64 c);
void mc_libc$cctab(i64 level);
void mc_libc$ccstr(u8 *s,i64 level);
void mc_libc$ccstrline(u8 *cstr);
void mc_libc$ccstrsemi(u8 *cstr);
void mc_libc$ccstrsemiu(struct mm_decls$unitrec *p);
void mc_libc$ccsendline(void);
void mc_libc$ccint(i64 a);
void mc_libc$ccinitline(void);
u8 *mc_libc$strmodec(i64 m,u8 *name,i64 addtab);
void mc_libc$strmodec2(i64 m,u8 *name,i64 addtab);
static u8 *mc_libc$strmodex(i64 m);
u8 *mc_libc$strprocsig(struct mm_decls$strec *p,u8 *name,i64 showparamnames);
u8 *mc_libc$getprocname(struct mm_decls$strec *d);
u8 *mc_libc$getfullnamec(struct mm_decls$strec *d);
u8 *mc_libc$getfullnamec2(struct mm_decls$strec *d);
u8 *mc_libc$genclabel(i64 n,i64 colon);
void mc_libc$genrecorddef(i64 m);
void mc_libc$genrecordfwd(i64 m);
void mc_libc$do_initdata(struct mm_decls$unitrec *p,i64 docomma,i64 level);
void mc_libc$cclongstr(u8 *svalue,i64 length);
static void mc_libc$do_makelist(struct mm_decls$unitrec *a,i64 length,i64 docomma,i64 level);
i64 mc_libc$issimplec(struct mm_decls$unitrec *p);
u8 *mc_libc$strstringc(u8 *s,i64 length);
void mc_libc$do_syscallproc(u8 *fnname,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mc_libc$evalsysparam(struct mm_decls$unitrec *a);
void mc_libc$dxstr(u8 *str);
void mc_libc$dxchar(i64 ch);
void mc_libc$dxint(i64 a);
void mc_libc$start(void);
i64 mm_libsourcesc$findsyslib(u8 *filename);
void mm_libsourcesc$start(void);
void mm_modules$readprojectfile(u8 *filename);
static void mm_modules$initheadervars(void);
static void mm_modules$readmoduledir(void);
static i64 mm_modules$checkwhen(void);
static void mm_modules$addmodule(u8 *modulename,struct mm_decls$strec *stalias);
static void mm_modules$addsubprogram(u8 *subprogname,i64 fileno);
static void mm_modules$addfirstsubprogram(u8 *progname,i64 fileno);
static void mm_modules$readsubprogram(void);
static void mm_modules$readimport(void);
static void mm_modules$readinclude(void);
static u8 *mm_modules$readvar(void);
static u8 *mm_modules$fixpath(u8 *path);
static void mm_modules$dosetvar(void);
static void mm_modules$doshowvar(void);
static void mm_modules$setmixedprogram(u8 *basefile);
static void mm_modules$setmixedimport(void);
void mm_modules$loadmodules(void);
static void mm_modules$loadmodule(struct mm_decls$modulerec *pm);
static void mm_modules$addsyslib(void);
void mm_modules$addlib(u8 *libname,i64 libtype);
static u8 *mm_modules$readfileline(u8 *s);
static u8 *mm_modules$findnextlineheader(u8 *s);
static u8 *mm_modules$loadmafile(u8 *filespec,u8 *builtinstr);
void mm_modules$start(void);
void mm_name$rx_typetable(void);
void mm_name$rx_unit(struct mm_decls$strec *owner,struct mm_decls$unitrec *p);
i64 mm_name$rx_module(i64 n);
void mm_name$rx_deflist(struct mm_decls$strec *owner,struct mm_decls$strec *p);
void mm_name$rx_passdef(struct mm_decls$strec *owner,struct mm_decls$strec *p);
static void mm_name$rx_unitlist(struct mm_decls$strec *owner,struct mm_decls$unitrec *p);
struct mm_decls$strec *mm_name$resolvetopname(struct mm_decls$strec *owner,struct mm_decls$strec *stnewname,i64 moduleno,i64 allowmod);
void mm_name$resolvename(struct mm_decls$strec *owner,struct mm_decls$unitrec *p);
struct mm_decls$strec *mm_name$finddupl(struct mm_decls$strec *d,struct mm_decls$strec *pdupl);
struct mm_decls$strec *mm_name$finddupl_sub(struct mm_decls$strec *d,struct mm_decls$strec *pdupl);
static void mm_name$resolvedot(struct mm_decls$strec *owner,struct mm_decls$unitrec *p);
static void mm_name$fixmode(struct mm_decls$typenamerec *p);
void mm_name$fixusertypes(void);
static struct mm_decls$strec *mm_name$addframevar(struct mm_decls$strec *owner,struct mm_decls$strec *d,i64 moduleno,i64 mode);
static struct mm_decls$unitrec *mm_name$copylistunit(struct mm_decls$unitrec *p);
static struct mm_decls$unitrec *mm_name$copyunit(struct mm_decls$unitrec *p);
static void mm_name$replaceunit(struct mm_decls$unitrec *p,struct mm_decls$unitrec *q);
static void mm_name$expandmacro(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mm_name$duplfield(struct mm_decls$strec *owner,struct mm_decls$strec *p,struct mm_decls$strec *q);
static void mm_name$do_baseclass(struct mm_decls$strec *p);
void mm_name$start(void);
i64 mm_parse$parsemodule(i64 n);
struct mm_decls$unitrec *mm_parse$readmoduledefs(struct mm_decls$strec *owner);
static void mm_parse$initparser(void);
void mm_parse$skipsemi(void);
struct mm_decls$unitrec *mm_parse$makeblock(struct mm_decls$unitrec *p);
static void mm_parse$checkequals(void);
static i64 mm_parse$getcurrline(void);
static i64 mm_parse$checkbegin(i64 fbrack);
static void mm_parse$checkbeginend(i64 closesym,i64 kwd,i64 startline);
void mm_parse$checkend(i64 endsym,i64 endkwd1,i64 endkwd2,i64 startline);
static struct mm_decls$unitrec *mm_parse$readvardef(struct mm_decls$strec *owner,i64 scope,i64 isstatic,i64 varid,i64 k);
static void mm_parse$readconstdef(struct mm_decls$strec *owner,i64 scope);
static struct mm_decls$unitrec *mm_parse$readlbrack(void);
static void mm_parse$addlistparam(struct mm_decls$strec **ulist,struct mm_decls$strec **ulistx,struct mm_decls$strec *p);
static struct mm_decls$unitrec *mm_parse$readcast(void);
static struct mm_decls$unitrec *mm_parse$readopc(void);
static struct mm_decls$unitrec *mm_parse$readsprint(void);
static struct mm_decls$unitrec *mm_parse$readsread(void);
static struct mm_decls$unitrec *mm_parse$readcompilervar(void);
static struct mm_decls$unitrec *mm_parse$readcastx(void);
void mm_parse$checksymbol(i64 symbol);
void mm_parse$lexchecksymbol(i64 symbol);
i64 mm_parse$readtypespec(struct mm_decls$strec *owner,i64 typedefx);
static i64 mm_parse$readslicetype(struct mm_decls$strec *owner,i64 slicetype,i64 typedefx);
static struct mm_decls$unitrec *mm_parse$readslist(i64 iscall,i64 donulls);
static struct mm_decls$unitrec *mm_parse$readindex(struct mm_decls$unitrec *p,i64 dot);
static struct mm_decls$unitrec *mm_parse$readdotsuffix(struct mm_decls$unitrec *p);
static struct mm_decls$unitrec *mm_parse$readconstexpr(i64 needconst);
static i64 mm_parse$readconstint(void);
static void mm_parse$readprocdef(struct mm_decls$strec *procowner,i64 scope,i64 fflang);
struct mm_decls$strec *mm_parse$readprocdecl(struct mm_decls$strec *procowner,i64 scope,i64 fflang);
static struct mm_decls$strec *mm_parse$readparams(struct mm_decls$strec *procowner,struct mm_decls$strec *owner,i64 fflang,i64 *varparams,i64 *nparams);
static struct mm_decls$unitrec *mm_parse$readcondsuffix(struct mm_decls$unitrec *p);
static struct mm_decls$unitrec *mm_parse$readif(void);
static struct mm_decls$unitrec *mm_parse$readgoto(i64 gototag);
static struct mm_decls$unitrec *mm_parse$readunless(void);
static struct mm_decls$unitrec *mm_parse$readswitchcase(void);
static struct mm_decls$unitrec *mm_parse$readstop(void);
static struct mm_decls$unitrec *mm_parse$readreturn(void);
static struct mm_decls$unitrec *mm_parse$readdo(void);
static struct mm_decls$unitrec *mm_parse$readto(void);
static struct mm_decls$unitrec *mm_parse$readwhile(void);
static struct mm_decls$unitrec *mm_parse$readrepeat(void);
static struct mm_decls$unitrec *mm_parse$readloopcontrol(void);
static struct mm_decls$unitrec *mm_parse$readprint(void);
static struct mm_decls$unitrec *mm_parse$readread(void);
static struct mm_decls$unitrec *mm_parse$readfor(void);
static struct mm_decls$unitrec *mm_parse$readname(void);
void mm_parse$readtypedef(struct mm_decls$strec *owner,i64 scope);
void mm_parse$readrecordfields(struct mm_decls$strec *owner,i64 m);
void mm_parse$readtabledef(struct mm_decls$strec *owner,i64 scope);
void mm_parse$readclassdef(struct mm_decls$strec *owner,i64 scope);
static void mm_parse$readclassbody(struct mm_decls$strec *owner,i64 classkwd);
static void mm_parse$readimportmodule(struct mm_decls$strec *owner);
static void mm_parse$readimportbody(struct mm_decls$strec *owner);
static struct mm_decls$strec *mm_parse$readequivfield(struct mm_decls$strec *owner);
static i64 mm_parse$readrefproc(struct mm_decls$strec *owner,i64 typedefx,i64 fflang);
static void mm_parse$pushproc(struct mm_decls$strec *p);
static void mm_parse$popproc(void);
static struct mm_decls$unitrec *mm_parse$makeastring(void);
static i64 mm_parse$readreturntype(struct mm_decls$strec *owner,i64 (*retmodes)[]);
static struct mm_decls$unitrec *mm_parse$readset(void);
static i64 mm_parse$istypestarter(void);
struct mm_decls$unitrec *mm_parse$readunit(void);
static struct mm_decls$unitrec *mm_parse$readassignment(struct mm_decls$unitrec *pt);
static struct mm_decls$unitrec *mm_parse$readorterms(struct mm_decls$unitrec *pt);
static struct mm_decls$unitrec *mm_parse$readandterms(struct mm_decls$unitrec *pt);
static struct mm_decls$unitrec *mm_parse$readcmpterms(struct mm_decls$unitrec *pt);
static struct mm_decls$unitrec *mm_parse$readinterms(struct mm_decls$unitrec *pt);
static struct mm_decls$unitrec *mm_parse$readrangeterm(struct mm_decls$unitrec *pt);
static struct mm_decls$unitrec *mm_parse$readaddterms(struct mm_decls$unitrec *pt);
static struct mm_decls$unitrec *mm_parse$readmulterms(struct mm_decls$unitrec *pt);
static struct mm_decls$unitrec *mm_parse$readpowerterms(struct mm_decls$unitrec *p);
static struct mm_decls$unitrec *mm_parse$readterm2(void);
static struct mm_decls$unitrec *mm_parse$readterm(void);
static void mm_parse$readmacrodef(struct mm_decls$strec *owner,i64 scope);
static struct mm_decls$unitrec *mm_parse$readrecase(void);
static void mm_parse$adddocstring(u8 *s);
static struct mm_decls$unitrec *mm_parse$fixcond(struct mm_decls$unitrec *p);
static struct mm_decls$unitrec *mm_parse$readsunit(i64 inwhile);
void mm_parse$start(void);
i64 mm_support$loadsourcefile(u8 *filespec);
static i64 mm_support$loadbundledfile(u8 *filespec,i64 issyslib,i64 support);
void mm_support$mcerror(u8 *mess);
void mm_support$serror_gen(u8 *mess);
static void mm_support$showdivider(u64 ch);
static void mm_support$showerrorsource(i64 pos,struct mm_decls$strec *stproc);
void mm_support$stopcompiler(u8 *filename,i64 lineno);
void mm_support$serror(u8 *mess);
void mm_support$serror_s(u8 *mess,u8 *a);
void mm_support$error_gen(i64 pass,u8 *mess,struct mm_decls$unitrec *p);
void mm_support$rxerror(u8 *mess,struct mm_decls$unitrec *p);
i64 mm_support$gerror(u8 *mess,struct mm_decls$unitrec *p);
void mm_support$axerror(u8 *mess);
void mm_support$txerror(u8 *mess,struct mm_decls$unitrec *p);
void mm_support$txerror_s(u8 *mess,u8 *a,struct mm_decls$unitrec *p);
void mm_support$txerror_ss(u8 *mess,u8 *a,u8 *b);
void mm_support$rxerror_s(u8 *mess,u8 *a,struct mm_decls$unitrec *p);
void mm_support$gerror_s(u8 *mess,u8 *s,struct mm_decls$unitrec *p);
void mm_support$gerror_t(u8 *mess,struct mm_decls$unitrec *p);
void mm_support$lxerror_gen(u8 *mess);
void mm_support$lxerror(u8 *mess);
void mm_support$loaderror(u8 *mess,u8 *mess2,u8 *mess3);
void mm_support$gs_additem(struct mlib$strbuffer *dest,u8 *s);
void mm_support$gs_copytostr(struct mlib$strbuffer *source,u8 *s);
i64 mm_support$isalphanum(i64 c);
void mm_support$init_tt_tables(void);
void mm_support$addspecialtypes(void);
i64 mm_support$getsupportfile(u8 *filename,u8 *ext,u8 *path,i64 issyslib,i64 issupport);
static i64 mm_support$isabspath(u8 *filespec);
void mm_support$initbblib(void);
i64 mm_support$getfileno(u64 pos);
i64 mm_support$getlineno(u64 pos);
static u8 *mm_support$getsourceline(u64 pos);
static u8 *mm_support$getsourcestart(u64 pos);
static u8 *mm_support$getsourcepos(u64 pos);
void mm_support$do_writema(void);
void mm_support$start(void);
void mm_tables$start(void);
static void mm_type$tpass(struct mm_decls$unitrec *p,i64 t,i64 lv);
void mm_type$tx_allprocs(void);
static void mm_type$tx_block(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 t,i64 lv);
void mm_type$tx_typetable(void);
static void mm_type$setmodesize(i64 m);
static void mm_type$setarraysize(i64 m);
static void mm_type$setslicesize(i64 m);
i64 mm_type$tx_module(i64 n);
void mm_type$tx_passdef(struct mm_decls$strec *p);
static void mm_type$tx_unitlist(struct mm_decls$unitrec *p,i64 t,i64 lv);
static void mm_type$tx_namedef(struct mm_decls$strec *d);
void mm_type$tx_namedconst(struct mm_decls$strec *d);
static void mm_type$checkconstexpr(struct mm_decls$unitrec *p);
static i64 mm_type$getconstint(struct mm_decls$unitrec *q);
static void mm_type$makenewconst(struct mm_decls$unitrec *p,i64 x,i64 t);
static void mm_type$tx_name(struct mm_decls$unitrec *p,i64 t,i64 lv);
static void mm_type$tx_bin(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mm_type$tx_binto(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static i64 mm_type$getdominantmode(i64 amode,i64 bmode);
static void mm_type$tx_cmpchain(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a);
static void mm_type$tx_callproc(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *pargs,i64 t);
static void mm_type$tx_unary(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a);
static void mm_type$tx_if(struct mm_decls$unitrec *p,struct mm_decls$unitrec *pcond,struct mm_decls$unitrec *plist,struct mm_decls$unitrec *pelse,i64 t,i64 lv);
static void mm_type$tx_incrto(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 t);
static void mm_type$tx_for(struct mm_decls$unitrec *pindex,struct mm_decls$unitrec *pfrom,struct mm_decls$unitrec *pbody);
static void mm_type$tx_forall(struct mm_decls$unitrec *pindex,struct mm_decls$unitrec *plist,struct mm_decls$unitrec *pbody);
static void mm_type$tx_index(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,i64 t,i64 lv);
static void mm_type$tx_makerange(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mm_type$tx_ptr(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 t,i64 lv);
static void mm_type$setrecordsize(i64 m);
static void mm_type$checkblocktype(i64 m);
static void mm_type$scanrecord(i64 state,struct mm_decls$strec *(*fields)[],i64 *index,i64 *isize,i64 offset,i64 calign,i64 *maxalign);
static i64 mm_type$roundoffset(i64 offset,i64 alignment);
static void mm_type$tx_convert(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 hard);
static void mm_type$tx_makelist(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 t,i64 lv);
static void mm_type$tx_makeslice(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 t);
static void mm_type$tx_makeset(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 t);
static void mm_type$tx_dot(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,i64 lv);
static struct mm_decls$strec *mm_type$resolvefield(struct mm_decls$strec *d,i64 m);
static void mm_type$tx_andl(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mm_type$convintconst(struct mm_decls$unitrec *p,i64 x);
static void mm_type$tx_sliceptr(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a);
static void mm_type$tx_swap(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mm_type$tx_select(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,struct mm_decls$unitrec *c,i64 t,i64 lv);
static void mm_type$tx_case(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,struct mm_decls$unitrec *c,i64 t,i64 lv);
static void mm_type$tx_notl(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a);
static void mm_type$tx_istruel(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a);
static void mm_type$tx_typepun(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a);
static void mm_type$tx_exit(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a);
static void mm_type$tx_goto(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a);
static void mm_type$tx_switch(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,struct mm_decls$unitrec *c,i64 t,i64 lv);
static void mm_type$tx_addroffirst(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 t);
static void mm_type$tx_return(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 t);
static void mm_type$tx_dotindex(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,i64 lv);
static void mm_type$tx_slice(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mm_type$twiden(struct mm_decls$unitrec *p,i64 lv);
static void mm_type$tstringslice(struct mm_decls$unitrec *p,i64 slicemode);
static void mm_type$tx_bitfield(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 lv);
static void mm_type$deref(struct mm_decls$unitrec *a,i64 needres);
static void mm_type$tmethodcall(struct mm_decls$unitrec *p,struct mm_decls$unitrec *pdot,struct mm_decls$unitrec *pargs);
static void mm_type$do_bounds(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a);
static void mm_type$addnotl(struct mm_decls$unitrec *p);
static void mm_type$tevaluate(struct mm_decls$unitrec *p);
static struct mm_decls$unitrec *mm_type$addrdotindex(struct mm_decls$unitrec *p,i64 *offset);
static void mm_type$tevalbinop(struct mm_decls$unitrec *p);
static void mm_type$tevalmonop(struct mm_decls$unitrec *p);
static i64 mm_type$iscondtrue(struct mm_decls$unitrec *p);
static i64 mm_type$iscondfalse(struct mm_decls$unitrec *p);
static void mm_type$fixchararray(struct mm_decls$unitrec *a);
static void mm_type$combinestrings(struct mm_decls$unitrec *p);
static void mm_type$tx_strinclude(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a);
static void mm_type$coerceunit(struct mm_decls$unitrec *p,i64 t,i64 hard);
static i64 mm_type$getconversionop(i64 s,i64 t,i64 hard);
static void mm_type$applyconversion(struct mm_decls$unitrec *p,i64 s,i64 t,i64 opc);
static void mm_type$checkmodes(i64 s,i64 t);
static i64 mm_type$comparemodes(i64 s,i64 t);
static i64 mm_type$tevalconvert(struct mm_decls$unitrec *p,i64 s,i64 t,i64 opc);
static void mm_type$tx_assign(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,i64 t);
static void mm_type$tx_assignmultmult(struct mm_decls$unitrec *pp,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mm_type$tx_assignmultscalar(struct mm_decls$unitrec *pp,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,i64 t);
static void mm_type$tpasslv(struct mm_decls$unitrec *p,i64 t);
static i64 mm_type$dobinnumx(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static i64 mm_type$dobinnumf(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static i64 mm_type$dobinnumi(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static i64 mm_type$doin(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b);
static void mm_type$setsimple(struct mm_decls$unitrec *p);
void mm_type$start(void);
void mm_winc$codegen(void);
i64 mm_winc$runlibfile(u8 *filename);
i64 mm_winc$writeexefile(u8 *exename,i64 gendll);
i64 mm_winc$writelibfile(u8 *filename);
i64 mm_winc$writeasmfile(u8 *filename);
void mm_winc$do_link_win(u8 *cfile,u8 *exefile,u8 *linkoption,i64 ccompiler,i64 optimise);
void mm_winc$do_link_lin(u8 *cfile,u8 *exefile,u8 *linkoption,i64 ccompiler,i64 optimise);
void mm_winc$start(void);
void mc_decls$start(void);
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
static void mlib$addtomemalloc(i32 *ptr,i64 size);
static void mlib$removefrommemalloc(i32 *ptr,i64 size);
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
void *mlib$pcm_alloc64(void);
void mlib$pcm_free64(void *p);
void *mlib$pcm_alloc16(void);
void mlib$pcm_free16(void *p);
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
byte *mlib$checkpackfile(void);
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
extern void sleep(u32 $1);
extern i32 __getmainargs(i32 *$1,void *$2,void *$3,i64 $4,void *$5);
void mclib$start(void);
extern void *dlopen(u8 *$1,i32 $2);
extern void *dlsym(void *$1,u8 *$2);
extern i32 tcgetattr(i32 $1,struct mlinux$termios *$2);
extern i32 tcsetattr(i32 $1,i32 $2,struct mlinux$termios *$3);
void mlinux$os_init(void);
i64 mlinux$os_execwait(u8 *cmdline,i64 newconsole,u8 *workdir);
i64 mlinux$os_execcmd(u8 *cmdline,i64 newconsole);
i64 mlinux$os_getch(void);
i64 mlinux$os_kbhit(void);
void mlinux$os_flushkeys(void);
void *mlinux$os_getconsolein(void);
void *mlinux$os_getconsoleout(void);
void *mlinux$os_proginstance(void);
u64 mlinux$os_getdllinst(u8 *name);
void *mlinux$os_getdllprocaddr(i64 hlib,u8 *name);
void mlinux$os_initwindows(void);
i64 mlinux$os_getchx(void);
u8 *mlinux$os_getos(void);
i64 mlinux$os_gethostsize(void);
i64 mlinux$os_iswindows(void);
i64 mlinux$os_shellexec(u8 *opc,u8 *file);
void mlinux$os_sleep(i64 a);
void *mlinux$os_getstdin(void);
void *mlinux$os_getstdout(void);
u8 *mlinux$os_gethostname(void);
u8 *mlinux$os_getmpath(void);
void mlinux$os_exitprocess(i64 x);
i64 mlinux$os_clock(void);
i64 mlinux$os_ticks(void);
i64 mlinux$os_getclockspersec(void);
void mlinux$os_setmesshandler(void *addr);
i64 mlinux$os_hpcounter(void);
i64 mlinux$os_hpfrequency(void);
i64 mlinux$os_filelastwritetime(u8 *filename);
void mlinux$os_getsystime(struct mlinux$rsystemtime *tm);
void mlinux$os_peek(void);
byte *mlinux$os_allocexecmem(i64 n);
void mlinux$start(void);
u64 mwindllc$os_calldllfunction(void (*fnaddr)(void),i64 retcode,i64 nargs,i64 (*args)[],byte (*argcodes)[]);
u64 mwindllc$os_pushargs(u64 (*args)[],i64 nargs,i64 nextra,void (*fnaddr)(void),i64 isfloat);
static i64 mwindllc$calldll_cint(void (*fnaddr)(void),i64 (*params)[],i64 nparams);
static i64 mwindllc$calldll_creal(void (*fnaddr)(void),i64 (*params)[],i64 nparams);
void mwindllc$os_dummycall(r64 a,r64 b,r64 c,r64 d);
void mwindllc$start(void);

/* VARS */
static byte mm_cli$fmodinfo;
static u8 *  mm_cli$projectmodule;
static u8 *  mm_cli$optionnames[59] = {
    (byte*)"header",
    (byte*)"load",
    (byte*)"fixup",
    (byte*)"parse",
    (byte*)"name",
    (byte*)"type",
    (byte*)"asm",
    (byte*)"c",
    (byte*)"mcl",
    (byte*)"obj",
    (byte*)"mx",
    (byte*)"ml",
    (byte*)"exe",
    (byte*)"mexe",
    (byte*)"run",
    (byte*)"sys",
    (byte*)"minsys",
    (byte*)"nosys",
    (byte*)"minos",
    (byte*)"nofile",
    (byte*)"debug",
    (byte*)"gcc",
    (byte*)"tcc",
    (byte*)"tc",
    (byte*)"bcc",
    (byte*)"ma",
    (byte*)"mas",
    (byte*)"docs",
    (byte*)"exp",
    (byte*)"lib",
    (byte*)"opt",
    (byte*)"opt1",
    (byte*)"opt2",
    (byte*)"ast1",
    (byte*)"ast2",
    (byte*)"ast3",
    (byte*)"showmx",
    (byte*)"showasm",
    (byte*)"st",
    (byte*)"pst",
    (byte*)"stflat",
    (byte*)"types",
    (byte*)"overloads",
    (byte*)"ss",
    (byte*)"modules",
    (byte*)"shortnames",
    (byte*)"modinfo",
    (byte*)"time",
    (byte*)"v",
    (byte*)"vv",
    (byte*)"q",
    (byte*)"h",
    (byte*)"help",
    (byte*)"ext",
    (byte*)"out",
    (byte*)"outpath",
    (byte*)"unused",
    (byte*)"set",
    (byte*)"linux"
};
static byte mm_cli$fasmexe;
static i64 mm_cli$abc;
static i64 mm_cli$def;
static u8 *  mm_cli$outext = (byte*)"";
static i64 mm_cli$startclock;
static i64 mm_cli$endclock;
static i64 mm_cli$rpclock;
static byte mm_cli$msfile;
static u8 *  mm_cli$inputfile;
static i64 mc_blockc$loopstack[50][4];
static i64 mc_blockc$loopindex;
static i64 mc_blockc$blocklevel;
static struct mm_decls$strec *  mm_decls$stprogram;
static struct mm_decls$strec *  mm_decls$stmodule;
static struct mm_decls$strec *  mm_decls$stsubprog;
static struct mm_decls$strec *  mm_decls$stsysmodule;
static struct mm_decls$strec *  mm_decls$alldeflist;
static i64 mm_decls$currmoduleno;
static struct mm_decls$tokenrec mm_decls$lx;
static struct mm_decls$tokenrec mm_decls$nextlx;
static struct mm_decls$modulerec mm_decls$moduletable[201];
static byte mm_decls$moduletosub[201];
static struct mm_decls$subprogrec mm_decls$subprogtable[31];
static u8 *  mm_decls$libfiles[51];
static byte mm_decls$libtypes[51];
static u8 *  mm_decls$sourcefilespecs[1001];
static u8 *  mm_decls$sourcefilepaths[1001];
static u8 *  mm_decls$sourcefilenames[1001];
static byte mm_decls$sourcefilesys[1001];
static byte mm_decls$sourcefilesupport[1001];
static u8 *  mm_decls$sourcefiletext[1001];
static u8 *  mm_decls$sourcefiledupl[1001];
static i64 mm_decls$sourcefilesizes[1001];
static i64 mm_decls$nmodules;
static i64 mm_decls$nsubprogs;
static i64 mm_decls$nsourcefiles;
static i64 mm_decls$nlibfiles;
static i64 mm_decls$mainmoduleno;
static i64 mm_decls$ntypes;
static struct mm_decls$strec *  mm_decls$ttnamedef[6001];
static struct mm_decls$strec *  mm_decls$ttowner[6001];
static i32 mm_decls$ttbasetype[6001];
static u8 *  mm_decls$ttname[6001];
static u32 mm_decls$ttsize[6001];
static byte mm_decls$ttsizeset[6001];
static i32 mm_decls$ttlower[6001];
static i32 mm_decls$ttlength[6001];
static i32 (*mm_decls$ttmult[6001])[];
static struct mm_decls$unitrec *  mm_decls$ttdimexpr[6001];
static i32 mm_decls$tttarget[6001];
static byte mm_decls$ttusercat[6001];
static i32 mm_decls$ttlineno[6001];
static byte mm_decls$ttsigned[6001];
static byte mm_decls$ttisreal[6001];
static byte mm_decls$ttisinteger[6001];
static byte mm_decls$ttisshort[6001];
static byte mm_decls$ttisref[6001];
static byte mm_decls$ttcat[6001];
static byte mm_decls$ttisblock[6001];
static struct mm_decls$typenamerec mm_decls$typenames[8001];
static struct mm_decls$posrec mm_decls$typenamepos[8001];
static i64 mm_decls$ntypenames;
static byte mm_decls$typestarterset[173];
static struct mm_decls$strec *  mm_decls$currproc;
static struct mm_decls$strec *  mm_decls$currsubprog;
static i64 mm_decls$debug = (i64)0;
static i64 mm_decls$assemmode = (i64)0;
static i64 mm_decls$headermode = (i64)0;
static struct mm_decls$procrec *  mm_decls$proclist;
static struct mm_decls$procrec *  mm_decls$proclistx;
static struct mm_decls$procrec *  mm_decls$staticlist;
static struct mm_decls$procrec *  mm_decls$staticlistx;
static struct mm_decls$procrec *  mm_decls$constlist;
static struct mm_decls$procrec *  mm_decls$constlistx;
static struct mm_decls$unitrec *  mm_decls$nullunit;
static i64 mm_decls$targetbits = (i64)64;
static i64 mm_decls$targetsize = (i64)8;
static u8 *  mm_decls$docstrings[20];
static i64 mm_decls$ndocstrings;
static i64 mm_decls$ndllproctable;
static struct mm_decls$strec *  mm_decls$dllproctable[1000];
static i64 mm_decls$fverbose = (i64)1;
static byte mm_decls$msyslevel = (byte)(i64)2;
static byte mm_decls$mvarlib = (byte)(i64)0;
static byte mm_decls$fvarnames = (byte)(i64)0;
static byte mm_decls$minos = (byte)(i64)0;
static byte mm_decls$freadma;
static byte mm_decls$fwritema;
static byte mm_decls$fwriteexports;
static byte mm_decls$fwritedocs;
static byte mm_decls$fexe;
static byte mm_decls$fobj;
static byte mm_decls$fwritelibs;
static byte mm_decls$fshowtiming;
static byte mm_decls$fshowss;
static byte mm_decls$fshowmx;
static byte mm_decls$fshowpcl;
static byte mm_decls$fshowasm;
static byte mm_decls$fshowast1;
static byte mm_decls$fshowast2;
static byte mm_decls$fshowast3;
static byte mm_decls$fshowst;
static byte mm_decls$fshowstflat;
static byte mm_decls$fshowtypes;
static byte mm_decls$fshowoverloads;
static byte mm_decls$fshowmodules;
static byte mm_decls$foptim;
static byte mm_decls$fcheckunusedlocals = (byte)(i64)0;
static byte mm_decls$fwindows = (byte)(i64)1;
static byte mm_decls$flinux;
static byte mm_decls$fnofile;
static byte mm_decls$dointlibs = (byte)(i64)1;
static u8 *  mm_decls$passnames[14] = {
    (byte*)"header_pass",
    (byte*)"load_pass",
    (byte*)"parse_pass",
    (byte*)"fixup_pass",
    (byte*)"name_pass",
    (byte*)"type_pass",
    (byte*)"pcl_pass",
    (byte*)"mcl_pass",
    (byte*)"asm_pass",
    (byte*)"objpass",
    (byte*)"exe_pass",
    (byte*)"lib_pass",
    (byte*)"run_pass",
    (byte*)"clang_pass"
};
static u8 *  mm_decls$ccnames[4] = {(byte*)"gcc_cc",(byte*)"tcc_cc",(byte*)"tc_cc",(byte*)"bcc_cc"};
static i64 mm_decls$passlevel = (i64)0;
static i64 mm_decls$prodmode = (i64)0;
static i64 mm_decls$debugmode = (i64)0;
static i64 mm_decls$libmode = (i64)0;
static i64 mm_decls$mxstub = (i64)0;
static i64 mm_decls$ccompiler = (i64)1;
static u8 *  mm_decls$outfile;
static u8 *  mm_decls$destfilename;
static u8 *  mm_decls$destfilepath;
static u8 *  mm_decls$asmfilename;
static u8 *  mm_decls$pclfilename;
static u8 *  mm_decls$exefilename;
static u8 *  mm_decls$libfilename;
static u8 *  mm_decls$objfilename;
static u8 *  mm_decls$mafilename;
static u8 *  mm_decls$expfilename;
static struct mm_decls$strec *  mm_decls$extendtypelist;
static struct mm_decls$overloadrec *  mm_decls$overloadtable[126];
static i64 mm_decls$nunits;
static i64 mm_decls$nstrecs;
static i64 mm_decls$nreadassign;
static i64 mm_decls$nsimple;
static i64 mm_decls$nlbrack;
static i64 mm_decls$nincr;
static i64 mm_diags$currlineno;
static i64 mm_diags$currfileno;
static struct mlib$strbuffer mm_export$sbuffer;
static struct mlib$strbuffer *  mm_export$dest = (struct mlib$strbuffer *)&mm_export$sbuffer;
static struct mc_genc$stlinkrec *  mc_genc$allsymbols;
static struct mc_genc$stlinkrec *  mc_genc$allsymbolsx;
static i64 mc_genc$nallprocs;
static i64 mc_genc$nexports;
static u8 *  mc_genc$cfilename;
static u8 *  mm_lex$lxstart_stack[20];
static u8 *  mm_lex$lxsource_stack[20];
static u8 *  mm_lex$lxsptr_stack[20];
static i64 mm_lex$lxfileno_stack[20];
static struct mm_decls$tokenrec mm_lex$lxnextlx_stack[20];
static byte mm_lex$lximport_stack[20];
static i64 mm_lex$sourcelevel = (i64)0;
static i64 mm_lex$lximport;
static u8 *  mm_lex$lxsource;
static u8 *  mm_lex$lxstart;
static u8 *  mm_lex$lxsptr;
static i64 mm_lex$lxifcond;
static i64 mm_lex$longsuffix;
static i64 mm_lex$lxalllines;
static i64 mm_lex$lxfileno;
static struct mm_decls$strec *  mm_lex$hashtable[65536];
static i64 mm_lex$astringlength;
static u8 *  mm_lex$u64maxstr = (byte*)"18446744073709551615";
static byte mm_lex$alphamap[256];
static struct mm_decls$strec *  mm_lex$shortnames[26];
static i64 mm_lib$autotypeno = (i64)0;
static i64 mm_lib$nextavindex = (i64)0;
static i64 mm_lib$nextsvindex = (i64)0;
static struct mlib$strbuffer mm_lib$exprstrvar;
static struct mlib$strbuffer *  mm_lib$exprstr = (struct mlib$strbuffer *)&mm_lib$exprstrvar;
static struct mm_decls$unitrec *  mm_lib$unitheapptr = 0;
static i64 mm_lib$remainingunits = (i64)0;
static struct mlib$strbuffer mm_lib$sbuffer;
static struct mlib$strbuffer *  mm_lib$dest = (struct mlib$strbuffer *)&mm_lib$sbuffer;
static u8 *  mm_lib$framevarname;
static struct mm_decls$unitrec *  mc_libc$nilunit;
static u8 mc_libc$clinebuffer[4096];
static u8 *  mc_libc$clineptr;
static u8 *  mc_libc$clineend;
static u8 *  mm_libsourcesc$syslibnames[5] = {(byte*)"msysc.m",(byte*)"mlib.m",(byte*)"mclib.m",(byte*)"mlinux.m",(byte*)"mwindllc.m"};
static u8 *  mm_libsourcesc$libtext[5] = {
(byte*)"!MSYS version for C target\n\n!import clib\n!import mlib\n\n[]ref void _fnaddresses\n[]ichar _fnnames\n![1]procinforec _fnexports\nint _fnnprocs\nint _fnnexports\n\nglobal record procinforec=\n\tword16\t\tfnindex\n\tbyte\t\trettype\n\tbyte\t\tnparams\n\t[12]byte\tparamlist\nend\n\n!for print/read routines\n!------------------------------------------\nrecord fmtrec=\t! (default)\n\tbyte\tminwidth\t! n (0)   min field width (0 if not used or don't care)\n\ti8\t\tprecision\t! .n (0)   number of decimals/significant figures/max width\n\tbyte\tbase\t\t! B,H or Xn (10)  2 to 16\n\n\tchar\tquotechar\t! Qc (0)   0 or '\"' or c\n\tchar\tpadchar\t\t! Pc, Z (' ')\n\tchar\trealfmt\t\t! E,F,G ('f') 'e' or 'f' or 'g'\n\n\tchar\tplus\t\t! (0)   0 or '+'\n\tchar\tsepchar\t\t! Sc (0)   0 or ',' or c placed every 3 (base=10) or 4 digits\n\tchar\tlettercase\t! A,a ('A') 'A' or 'a'\n\tchar\tjustify\t\t! JL, JR, JC ('R') 'L' or 'R' or 'C'?\n\tchar\tsuffix\t\t! Tc (0)   0 or 'B' or 'H' or c\n\tchar\tusigned\t\t! W (0)   0 or 'W' force unsigned o/p for ints (eg. for hex display)\n\tchar\tcharmode\t! C,D (0)  0 or 'C' or 'D'\to/p int as int or single char or double/multi-char\n\tchar\theapmode\t! M (0)  'M' for str-functions, return ptr tp heap string\n\tchar\tparam\t\t! Use int value for <fmtparam>\n\tbyte\tspare\nend\n\nint fmtparam\t\t\t!as set with :'V'\n\nenumdata =\n\tstd_io,\n\tfile_io,\n\tstr_io\nend\n\nconst comma = ','\n\nglobal int needgap\t\t\t= 0\nint outdev\t\t\t= std_io\nfilehandle outchan\t= nil\nref char fmtstr \t= nil\n\nconst maxiostack=10\n[maxiostack]filehandle\toutchan_stack\n[maxiostack]int\t\t\toutdev_stack\n[maxiostack]ref char\tfmtstr_stack\n[maxiostack]byte\t\tneedgap_stack\n\n[maxiostack]ref char\tptr_stack\t\t!this one doesn't need pushing, as each is pointed to from outchan\nint niostack=0\n\n[0:]char digits=A\"0123456789ABCDEF\"\nconst onesixty=360\nfmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,0,0)\n\nconst smallstrlen=256\n\n!Read buffer vars\nconst rd_buffersize = 16384\t!total capacity of line buffer\n\nexport ref char rd_buffer\t\t! point to start of read buffer\nint rd_length\t\t\t! length of this line (as read by readln)\nref char rd_pos\t\t\t! current position it's up to (next read starts here)\nref char rd_lastpos\t\t! set by sread() just before reading used for reread()\nint termchar\t\t\t! terminator char set by readxxx()\nint itemerror\t\t\t!\tset by some read functions, eg for reals\n\nexport int $cmdskip\t\t\t!0 unless set by READMCX/etc\n\nconst maxparam=128\nexport int nsysparams\nexport int ncmdparams\nexport int nenvstrings\nexport [maxparam]ichar sysparams\nexport ref[0:]ichar cmdparams\nexport ref[]ichar envstrings\n\n\n!------------------------------------------\n\nword64 mask63\t= 0x7FFF'FFFF'FFFF'FFFF\nreal offset64\t= 9223372036854775808.0\t\t! 2**63 as r64\nreal offset32\t= 9223372036854775808.0\t\t! 2**63 as r32\n\nexport proc m_init(int nargs, ref[]ichar args, envstrings)=\n!export proc m_init(int nargs, ref ref char args0, envstrings0)=\n!\tref[]ichar args, envstrings\n!\targs:=cast(args0)\n!\tenvstrings:=cast(envstrings0)\n\n\tnsysparams:=nargs\n\n\tif nsysparams>maxparam then\n\t\tprintf(\"Too many params\\n\")\n\t\tstop 1\n\tfi\n\n\tfor i:=1 to nargs do\n\t\tsysparams[i]:=args[i]\n\tod\n\n!assume nsysparams is >=1, since first is always the program name\n\tncmdparams:=nsysparams-($cmdskip+1)\n\tcmdparams:=cast(&sysparams[$cmdskip+1])\n\n\tint j:=1\n\tnenvstrings:=0\n\twhile envstrings[j] do\n\t\t++nenvstrings\n\t\t++j\n\tod\n\nend\n\n\nglobal function m_getdotindex(word64 a,int i)int=\n\treturn (a iand (1<<i))>>i\nend\n\nglobal func m_setdotindex(word64 a, int i,x)word64=\n\tref word32 a32\n\n!see comments on setdotslice\n\t(a iand inot (1<<i)) ior (word64(x)<<i)\nend\n\nglobal function m_getdotslice(word64 a,int i,j)int=\n\tif i>=j then\n\t\treturn (a>>j)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(i-j+1))\n\telse\n\t\treturn (a>>i)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))\n\tfi\nend\n\nglobal func m_setdotslice(word64 a, int i,j,word64 x)word64=\n!a^:=(a^ iand inot (1dw<<i)) ior (word64(x)<<i)\n\tint w\n\tword64 mask64\n\tword mask\n\tref word32 a32\n\n\tif i>j then println \"SETDOTSLICE?\"; stop 52 fi\n\n!when j>=32, assume 64 bit dest, otherwise assume 32 bits to avoid writing\n!to bytes beyond the 32-bit value\n!THIS WILL BE A PROBLEM IF writing to 8/16 bit values too\n\n\tmask64:=inot((0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i\t\t\t!shifted field of w 1s\n\t(a iand inot mask64) ior x<<i\nend\n\nglobal function m_get_nprocs:int=\n\treturn _fnnprocs\nend\n\nglobal function m_get_nexports:int=\n\treturn _fnnexports\nend\n\nglobal function m_get_procname(int n)ref char=\n\treturn _fnnames[n]\nend\n\nglobal function m_get_procaddr(int n)ref void=\n\treturn _fnaddresses[n]\nend\n\nglobal function m_get_procexport(int n)ref void=\n\tnil\n!\treturn &_fnexports[n]\nend\n\nproc pushio=\n\tif niostack>=maxiostack then\n\t\tprintf(\"Too many io levels\\n\")\n\t\tstop 53\n\tfi\n\t++niostack\n\toutchan_stack[niostack]\t:= outchan\n\toutdev_stack[niostack]\t:= outdev\n\tfmtstr_stack[niostack]\t:= fmtstr\n\tneedgap_stack[niostack]\t:= needgap\n\tneedgap:=0\n\tfmtstr:=nil\n\toutchan:=nil\nend\n\nglobal proc m_print_startfile(ref void dev)=\n\tpushio()\n\toutchan:=cast(dev)\n\tif dev then\n\t\toutdev:=file_io\n\telse\n\t\toutdev:=std_io\n\tfi\nend\n\nglobal proc m_print_startstr(ref char s)=\n\tref ref char p\n\tpushio()\n\n\tptr_stack[niostack]:=s\n\tp:=&ptr_stack[niostack]\n\n\toutchan:=cast(p)\n\toutdev:=str_io\nend\n\nglobal proc m_print_startptr(ref ref char p)=\n\tpushio()\n\n\toutchan:=cast(p)\n\toutdev:=str_io\nend\n\nglobal proc m_print_startcon=\n\tpushio()\n\toutdev:=std_io\nend\n\nglobal proc m_print_setfmt(ref char format)=\n\tfmtstr:=format\nend\n\nglobal proc m_print_end=\n\tneedgap:=0\n\tnextfmtchars(1)\n\tif niostack=0 then return fi\n\toutchan\t:= outchan_stack[niostack]\n\toutdev\t:= outdev_stack[niostack]\n\tfmtstr\t:= fmtstr_stack[niostack]\n\tneedgap\t:= needgap_stack[niostack]\n\t--niostack\nend\n\nglobal proc m_print_ptr(ref void a,ichar fmtstyle=nil)=\n\tnextfmtchars()\n\n\tprintstr(strword(u64(a),\"z8h\"))\n\tneedgap:=1\nend\n\nglobal proc m_print_i64(int64 a,ichar fmtstyle=nil)=\n\t[40]char s\n\tfmtrec fmt\n\tint n\n\n\tnextfmtchars()\n\n\tif fmtstyle=nil then\n\t\tif a>=0 then\n\t\t\tn:=u64tostr(a,&.s,10,0)\n\t\telse\n\t\t\ts[1]:='-'\n\t\t\tn:=u64tostr(-a,&s[2],10,0)+1\n\t\tfi\n\t\tprintstr_n(&.s,n)\n\n\telse\n\t\tstrtofmt(fmtstyle,-1,&fmt)\n\t\tif fmt.param='V' then\n\t\t\tfmtparam:=a\n\t\t\tneedgap:=0\n\t\telse\n\t\t\ttostr_i64(a,&fmt)\n\t\tfi\n\tfi\n\tneedgap:=1\nend\n\nglobal proc m_print_u64(word64 a,ichar fmtstyle=nil)=\n\tfmtrec fmt\n\n\tnextfmtchars()\n\tif fmtstyle=nil then\n\t\tprintstr(strword(a))\n\telse\n\t\tstrtofmt(fmtstyle,-1,&fmt)\n\t\ttostr_u64(a,&fmt)\n\tfi\n\tneedgap:=1\nend\n\nglobal proc m_print_r64(real x,ichar fmtstyle=nil)=\n\t[360]char s\n\tfmtrec fmt\n\n\tnextfmtchars()\n\tif fmtstyle=nil then\n\t\tsprintf(&.s,\"%f\",x)\n\t\tprintstr(&.s)\n\telse\n\t\tstrtofmt(fmtstyle,-1,&fmt)\n\t\ttostr_r64(x,&fmt)\n\tfi\n\n\tneedgap:=1\nend\n\nglobal proc m_print_r32(real32 x,ichar fmtstyle=nil)=\n\tm_print_r64(x,fmtstyle)\nend\n\nglobal proc m_print_c8(int64 a,ichar fmtstyle=nil)=\n\t[40]char s\n\tfmtrec fmt\n\tint n\n\n\tnextfmtchars()\n\n\ts[1]:=a\n\ts[2]:=0\n\tprintstr(&.s)\n\tneedgap:=1\nend\n\nglobal proc m_print_str(ichar s, fmtstyle=nil)=\n\tnextfmtchars()\n\tfmtrec fmt\n\tif fmtstyle=nil then\n\t\tprintstr(s)\n\telse\n\t\tstrtofmt(fmtstyle,-1,&fmt)\n\t\ttostr_str(s,&fmt)\n\tfi\n\tneedgap:=1\nend\n\nglobal proc m_print_newline=\n\tneedgap:=0\n\tnextfmtchars(1)\n\tprintstr(\"\\w\")\nend\n\nglobal proc m_print_nogap=\n\tneedgap:=0\nend\n\nglobal proc m_print_space=\n\tneedgap:=0\n!RETURN\n\tprintstr(\" \")\nend\n\nexport proc printstr(ichar s)=\n\tint n\n\tref ref char p\n\t\n\tcase outdev\n\twhen std_io then\n\t\tprintf(\"%s\",s)\n\twhen file_io then\n\t\tfprintf(outchan,\"%s\",s)\n\twhen str_io then\n\t\tp:=cast(outchan)\n\t\tstrcpy(p^,s)\n\t\tp^+:=strlen(s)\n\tesac\nend\n\nglobal proc printstr_n(ichar s,int n=-1)=\n\t[smallstrlen]char str\n\tref ref char p\n\n\tcase n\n\twhen -1 then n:=strlen(s)\t\t!assume zero-terminated\n\twhen 0 then return\n\tesac\n\n\tcase outdev\n\twhen str_io then\n\t\tp:=cast(outchan)\n\t\tmemcpy(p^,s,n)\n\t\tp^+:=n\n\t\tp^^:=0\n\twhen file_io then\n\t\ts:=makezstring(s,n,&.str)\n\t\tfprintf(outchan,\"%s\",s)\n\t\tfreezstring(s,n)\n\n\twhen std_io then\n\t\ts:=makezstring(s,n,&.str)\n\t\tprintf(\"%s\",s)\n\t\tfreezstring(s,n)\n!\t\tprintf(\"%.*s\",int32(n),s)\n\tesac\nend\n\nexport proc printstrn_app(ichar s, int length, filehandle f=nil)=\n\tif length then\n\t\tif f=nil then\n\t\t\temitc \"printf((u8*)\"\"%.*s\"\",(i32)length,s);\"\n\t\telse\n\t\t\temitc \"fprintf(f,(u8*)\"\"%.*s\"\",(i32)length,s);\"\n\t\tfi\n!\t\tprintf(\"%.*s\",length,s)\n\tfi\nend\n\nfunction makezstring(ichar s,int n,ichar local)ichar=\n\tichar t\n\tif n<smallstrlen then\n\t\tmemcpy(local,s,n)\n\t\t(local+n)^:=0\n\t\treturn local\n\telse\n\t\tt:=pcm_alloc(n+1)\n\t\tmemcpy(t,s,n)\n\t\t(t+n)^:=0\n\t\treturn t\n\tfi\nend\n\nproc freezstring(ichar t,int n)=\n\tif n>=smallstrlen then\n\t\tpcm_free(t,n+1)\n\tfi\nend\n\nproc printchar(int ch)=\n\tref ref char p\n\tcase outdev\n\twhen std_io then\n\t\temitc \"printf((u8*)\"\"%c\"\",(int)ch)\"\n\twhen file_io then\n\t\temitc \"fprintf(msysc$outchan,(u8*)\"\"%c\"\",(int)ch)\"\n\twhen str_io then\n\t\tp:=cast(outchan)\n\t\tp^^:=ch\n\t\tp^+:=1\n\t\tp^^:=0\n\tesac\nend\n\nglobal proc nextfmtchars(int lastx=0)=\n\tchar c\n\tref char pstart\n\tint n\n\n\tif not fmtstr then\t\t\t!format not in use\n\t\tif needgap then\n\t\t\tprintchar(' ')\n!\t\tprintstr_n(\" \",1)\n\t\tfi\n\t\tneedgap:=0\n\t\treturn\n\tfi\n\n\tpstart:=fmtstr\n\tn:=0\n\n\twhile (1) do\n\t\tc:=fmtstr^\n\t\tswitch c\n\t\twhen '#' then\n\t\t\tif lastx then\n\t\t\t\tgoto skip\n\t\t\tfi\n\t\t\t++fmtstr\n\t\t\tif n then\n\t\t\t\tprintstr_n(pstart,n)\n\t\t\tfi\n\t\t\treturn\n\t\twhen 0 then\n\t\t\tif n then\n\t\t\t\tprintstr_n(pstart,n)\n\t\t\telsif not lastx then\n\t\t\t\tprintstr_n(\"|\",1)\n\t\t\tfi\n\t\t\treturn\n\t\twhen '~' then\n\t\t\tif n then\n\t\t\t\tprintstr_n(pstart,n)\n\t\t\t\tn:=0\n\t\t\tfi\n\t\t\t++fmtstr\n\t\t\tc:=fmtstr^\n\t\t\tif c then\n\t\t\t\t++fmtstr\n\t\t\t\tprintchar(c)\n\t\t\tfi\n\t\t\tpstart:=fmtstr\n\t\telse\n\tskip::\n\t\t\t++n\n\t\t\t++fmtstr\n\t\tendswitch\n\tod\nend\n\nglobal proc strtofmt(ref char s,int slen,ref fmtrec fmt) =\t\t!PC_STRTOFMT\n!convert format code string in s, to fmtrec at fmt^\n!Format code is a string containing the following char codes (upper or lower when mostly)\n!n\tWidth\n!.n\tMax width/precision\n!A\tConvert to upper when\n!a\tConvert to lower when\n!B\tBinary\n!C\tShow int as single n-bit (unicode) character\n!D\tShow int as multi-bit (unicode) character\n!E,F,G\tSpecify format for double (corresponds to C format codes)\n!F\n!G\n!H\tHex\n!JC\tJustify centre\n!JL\tJustify left\n!JR\tJustify right\n!M\tHEAPMODE???\n!O\tOctal\n!Pc\tUse padding char c\n!Q\tAdd double quotes around string (and deal with embedded quotes)\n!'\tAdd single quotes around string (and deal with embedded quotes)\n!Sc\tUse separator char c between every 3 or 4 digits\n!Tc\tUse terminator char c (typically B or H)\n!U\tShow ints as unsigned\n!V\tFor ints, don't display: store value as parameter for subsequent '*'\n!W\tUnsigned\n!Xn\tUse base n (n is hex 0 to F)\n!Z\tUse \"0\" padding\n!+\tAlways have + or - in front of integers\n!~\tQuote char is ~\n!*\tSame as n but uses parameter set with :'V' on previous int\n\n\tchar c\n\tbyte wset\n\tint n\n\t[0:100]char str\n\n\tfmt^:=defaultfmt\n\n\tif s=nil then return fi\n\n\tif slen=-1 then slen:=strlen(s) fi\n\n\tmemcpy(&.str,s,slen)\t\t!convert s/slen to zero-terminated string\n\tstr[slen]:=0\n\ts:=&.str\n\n\twset:=0\n\twhile s^ do\n\t\tc:=s^\n\t\t++s\n\t\tswitch c\n\t\twhen 'B', 'b' then fmt^.base:=2\n\t\twhen 'H', 'h' then fmt^.base:=16\n\t\twhen 'O', 'o' then fmt^.base:=8\n\t\twhen 'X', 'x' then\n\t\t\tc:=s^\n\t\t\tif c then\n\t\t\t\tswitch c\n\t\t\t\twhen '0'..'9' then c:=c-'0'\n\t\t\t\twhen 'A'..'F' then c:=c-'A'+10\n\t\t\t\twhen 'a'..'f' then c:=c-'a'+10\n\t\t\t\telse\n\t\t\t\t\tc:=10\n\t\t\t\tend\n\t\t\t\tfmt^.base:=c\n\t\t\t\t++s\n\t\t\tfi\n\t\twhen 'Q', 'q' then fmt^.quotechar:='\"'\n\t\twhen '~' then fmt^.quotechar:='~'\n\t\twhen 'J', 'j' then\n\t\t\tfmt^.justify:=toupper(s^)\n\t\t\tif s^ then\n\t\t\t\t++s\n\t\t\tfi\n\t\twhen 'A' then fmt^.lettercase:='A'\n\t\twhen 'a' then fmt^.lettercase:='a'\n\t\twhen 'Z', 'z' then fmt^.padchar:='0'\n\t\twhen 'S', 's' then\n\t\t\tfmt^.sepchar:=s^\n\t\t\tif s^ then\n\t\t\t\t++s\n\t\t\tfi\n\t\twhen 'P', 'p' then\n\t\t\tfmt^.padchar:=s^\n\t\t\tif s^ then\n\t\t\t\t++s\n\t\t\tfi\n\t\twhen 'T', 't' then\n\t\t\tfmt^.suffix:=s^\n\t\t\tif s^ then\n\t\t\t\t++s\n\t\t\tfi\n\t\twhen 'W', 'w' then fmt^.usigned:='W'\n\t\twhen 'E', 'e' then fmt^.realfmt:='e'\n\t\twhen 'F', 'f' then fmt^.realfmt:='f'\n\t\twhen 'G', 'g' then fmt^.realfmt:='g'\n\t\twhen '.' then\n\t\t\twset:=1\n\t\twhen comma,'_' then fmt^.sepchar:=c\n\t\twhen '+' then fmt^.plus:='+'\n\t\twhen 'D', 'd' then fmt^.charmode:='D'\n\t\twhen 'C', 'c' then fmt^.charmode:='C'\n\t\twhen 'M', 'm' then fmt^.heapmode:='M'\n\t\twhen 'V','v' then fmt.param:='V'\n\t\twhen '*' then\n\t\t\tn:=fmtparam\n\t\t\tgoto gotwidth\n\t\telse\n\t\t\tif c>='0' and c<='9' then\n\t\t\t\tn:=c-'0'\n\t\t\t\tdo\n\t\t\t\t\tc:=s^\n\t\t\t\t\tif s^=0 then\n\t\t\t\t\t\texit\n\t\t\t\t\tfi\n\t\t\t\t\tif c>='0' and c<='9' then\n\t\t\t\t\t\t++s\n\t\t\t\t\t\tn:=n*10+c-'0'\n\t\t\t\t\telse\n\t\t\t\t\t\texit\n\t\t\t\t\tfi\n\t\t\t\tod\ngotwidth::\n\t\t\t\tif not wset then\n\t\t\t\t\tfmt^.minwidth:=n\n\t\t\t\t\twset:=1\n\t\t\t\telse\n\t\t\t\t\tfmt^.precision:=n\n\t\t\t\tfi\n\t\t\tfi\n\t\tendswitch\n\tod\nend\n\nfunction domultichar (ref char p,int n,ref char dest,ref fmtrec fmt)int =\n!there are n (4 or 8) chars at p.!\n!There could be 0 to 4 or 8 printable chars converted to string at dest\n\t[0:20]char str\n\tref char q\n\tint i,nchars\n\n\tq:=&.str\n\n\tnchars:=n\n\n\tto n do\n\t\tif p^=0 then exit fi\n\t\tq^:=p^\n\t\t++q\n\t\t++p\n\tod\n\tq^:=0\n\n\treturn expandstr(&.str,dest,strlen(&.str),fmt)\nend\n\nfunction expandstr(ref char s,ref char t,int n,ref fmtrec fmt)int =\t\t!EXPANDSTR\n!s contains a partly stringified value.\n!widen s if necessary, according to fmt, and copy result to t\n!n is current length of s\n!note) = for non-numeric strings, fmt^.base should be set to 0, to avoid moving\n!a leading +/- when right-justifying with '0' padding.\n!t MUST be big enough for the expanded string; caller must take care of this\n!result will be zero-terminated, for use in this module\n\n\tint i,w,m\n\n!check to see if result is acceptable as it is\n\tw:=fmt^.minwidth\n\tif w=0 or w<=n then\t\t! allow str to be longer than minwidth\n\t\tstrncpy(t,s,n)\n\t\t(t+n)^:=0\n\t\treturn n\n\tfi\n\n\tif fmt^.justify='L' then\t! left-justify\n\t\tstrncpy(t,s,n)\n\t\tt+:=n\n\t\tfor i:=1 to w-n do\n\t\t\tt^:=fmt^.padchar\n\t\t\t++t\n\t\tod\n\t\tt^:=0\n\telsif fmt^.justify='R' then\n\t\tif fmt^.padchar='0' and fmt^.base and (s^='-' or s^='+') then ! need to move sign outside \n\t\t\tt^:=s^\n\t\t\t++t\n\t\t\tto w-n do\n\t\t\t\tt^:=fmt^.padchar\n\t\t\t\t++t\n\t\t\tod\n\t\t\tstrncpy(t,s+1,n-1)\n\t\t\t(t+n-1)^:=0\n\t\telse\n\t\t\tto w-n do\n\t\t\t\tt^:=fmt^.padchar\n\t\t\t\t++t\n\t\t\tod\n\t\t\tstrncpy(t,s,n)\n\t\t\t(t+n)^:=0\n\t\tfi\n\n\telse\t\t\t\t! centre-justify?\n\n\t\tm:=(w-n+1)/2\n\t\tto m do\n\t\t\tt^:=fmt^.padchar\n\t\t\t++t\n\t\tod\n\t\tstrncpy(t,s,n)\n\t\tt+:=n\n\t\tto w-n-m do\n\t\t\tt^:=fmt^.padchar\n\t\t\t++t\n\t\tod\n\t\tt^:=0\n\n\tfi\n\treturn w\nend\n\n!function xdivrem(word64 a,b, &remainder)word64=\n!\tword64 q,r\n!ABORTPROGRAM(\"XDIVREM\")\n!!\tassem\n!!\t\txor rdx,rdx\n!!\t\tmov rax,[a]\n!!\t\tdiv qword [b]\n!!\t\tmov [q],rax\t\n!!\t\tmov [r],rdx\t\n!!\tend\n!!\tremainder:=r\n!\treturn q\n!end\n!\nfunction u64tostr(u64 aa,ref char s,word base,int sep)int =\t\t!U64TOSTR\n!convert 64-bit int a to string in s^\n!base is number base, usually 10 but can be 2 or 16. Other bases allowed\n!result when a=minint (will give \"<minint>\")\n\t[0:onesixty]char t\n\tu64 dd\n\tint i,j,k,g\n\tint dummy\n\tref char s0\n\n\ti:=0\n\tk:=0\n\tg:=(base=10|3|4)\n\n\trepeat\n!\t\taa:=xdivrem(aa,base,dd)\n!\t\tt[++i]:=digits[dd]\n\n\t\tt[++i]:=digits[aa rem base]\n\t\taa:=aa/base\n\n!BUG in separator logic, doesn't work when leading zeros used, eg. printing\n!out a full length binary\n!so perhaps move this out to expandstr\n\t\t++k\n\t\tif sep and aa<>0 and k=g then\n\t\t\tt[++i]:=sep\n\t\t\tk:=0\n\t\tfi\n\tuntil aa=0\n\n\tj:=i\n\ts0:=s\n\twhile i do\n\t\ts^:=t[i--]\n\t\t++s\n\tod\n\ts^:=0\n\n\treturn j\nend\n\nfunction i64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =\n!a is signed 64-bit int/long, fmt is a ref to a filled-in fmtrec\n!convert a to a string in s, according to fmt\n!a basic conversion is done first,: the field manipulation is done\n!signed=1 for int, 0 for u32 (fmt^.unsigned forces ints to be treated as longs)\n!returns length of s\n\t[0:onesixty]char str\t\t\t\t! allow for binary with separators!\n\tint i,j,k,n,w,usigned\n\tstatic u64 mindint=0x8000'0000'0000'0000\n\n\tusigned:=0\n\tif fmt^.usigned then\n\t\tusigned:=1\n\tfi\n\n\tif aa=mindint and not usigned then\t\t! minint\n\n\t\tstr[0]:='-'\n\t\tn:=i64mintostr(&str[1],fmt^.base,fmt^.sepchar)+1\n\telse\n\t\tif (not usigned and aa<-0) or fmt^.plus then\n\t\t\tif aa<0 then\n\t\t\t\taa:=-aa\n\t\t\t\tstr[0]:='-'\n\t\t\telse\n\t\t\t\tstr[0]:='+'\n\t\t\tfi\n\t\t\tn:=u64tostr(aa,&str[1],fmt^.base,fmt^.sepchar)+1\n\t\telse\n\t\t\tn:=u64tostr(aa,&.str,fmt^.base,fmt^.sepchar)\n\t\tfi\n\tfi\n\n\tif fmt^.suffix then\n\t\tstr[n]:=fmt^.suffix\n\t\tstr[++n]:=0\n\tfi\n\n!str uses upper cases for hex/etc see if lc needed\n\tif (fmt^.base>10 or fmt^.suffix) and fmt^.lettercase='a'\tthen\t! need lower when\n\t\tconvlcstring(&.str)\n\tfi\n\n!at this point, n is the str length including signs and suffix\n\treturn expandstr(&.str,s,n,fmt)\nend\n\nfunction u64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =\t\t!U64TOSTRFMT\n!see i64tostrfmt\n\t[0:onesixty]char str\t\t\t\t! allow for binary with separators!\n\tint i,j,k,n,w\n\n\tn:=u64tostr(aa,&.str,fmt^.base,fmt^.sepchar)\n\n\tif fmt^.suffix then\n\t\tstr[n]:=fmt^.suffix\n\t\tstr[++n]:=0\n\tfi\n\n!str uses upper cases for hex/etc see if lc needed\n\tif fmt^.base>10 or fmt^.suffix and fmt^.lettercase='a'\tthen\t! need lower when\n\t\tconvlcstring(&.str)\n\tfi\n\n!at this point, n is the str length including signs and suffix\n\treturn expandstr(&.str,s,n,fmt)\nend\n\nfunction i64mintostr(ref char s,int base,int sep)int =\t\t!I64MINTOSTR\n!convert minint to string in s do not include minus sign\n!return number of chars in string\n\t[0:onesixty]char t\n\tint i,j,k,g,neg\n\n\tswitch base\n\twhen 10 then\n\t\tstrcpy(&t[0],\"9223372036854775808\")\n\t\tj:=3\n\twhen 16 then\n\t\tstrcpy(&t[0],\"8000000000000000\")\n\t\tj:=1\n\twhen 2 then\n\t\tstrcpy(&t[0],\"1000000000000000000000000000000000000000000000000000000000000000\")\n\t\tj:=7\n\telse\n\t\tstrcpy(&t[0],\"<mindint>\")\n\tendswitch\n\n\ti:=strlen(&t[0])\n\ts+:=i\n\tif sep then\n\t\ts+:=j\n\tfi\n\ts^:=0\n\n\tk:=0\n\tg:=(base=10|3|4)\n\n\twhile i do\n\t\t--s\n\t\ts^:=t[i-- -1]\n\t\tif sep and i and ++k=g then\n\t\t\t--s\n\t\t\ts^:=sep\n\t\t\tk:=0\n\t\tfi\n\tod\n\treturn strlen(s)\nend\n\nfunction strtostrfmt(ref char s,ref char t,int n,ref fmtrec fmt)int =\n!s is a string process according to fmtrec fmt^, and return result in t\n!caller should check whether any changes are required to s (now it can just use s), but this\n!check is done here anyway (with a simple copy to t)\n!n is current length of s\n!return length of t\n!Three processing stages:\n!1 Basic input string s\n!2 Additions or mods: quotes, suffix, when conversion\n!3 Width adjustment\n!1 is detected here, 2 is done here, 3 is done by expandstr\n\tref char u,v\n\t[256]char str\n\tint w,nheap\t\t! whether any heap storage is used # bytes allocated\n\n\tnheap:=0\n\n\tif fmt^.quotechar or fmt^.lettercase then\t\t! need local copy\n\t\tif n<256 then\n\t\t\tu:=&.str\n\t\telse\n\t\t\tnheap:=n+3\t\t\t\t\t! allow for quotes+terminator\n\t\t\tu:=pcm_alloc(nheap)\n\t\tfi\n\t\tif fmt^.quotechar then\n\t\t\tv:=u\n\t\t\tv^:=fmt^.quotechar\n\t\t\t++v\n\t\t\tif n then\n\t\t\t\tstrcpy(v,s)\n\t\t\t\tv+:=n\n\t\t\tfi\n\t\t\tv^:=fmt^.quotechar\n\t\t\t++v\n\t\t\tv^:=0\n\t\t\tn+:=2\n\t\telse\n\t\t\tmemcpy(u,s,n)\n\t\tfi\n\t\tswitch fmt^.lettercase\n\t\twhen 'a' then\t! need lower when\n\t\t\tconvlcstring(u)\n\t\twhen 'A' then\n\t\t\tconvucstring(u)\n\t\tendswitch\n\t\ts:=u\n\tfi\n\n\tw:=fmt^.minwidth\n\tif w>n then\n\t\tn:=expandstr(s,t,n,fmt)\n\telse\n\t\tmemcpy(t,s,n)\n\tfi\n\tif nheap then\n\t\tpcm_free(u,nheap)\n\tfi\n\treturn n\nend\n\nproc tostr_i64(int64 a, ref fmtrec fmt)=\n\t[360]char str\n\tint n\n\n\tcase fmt^.charmode\n\twhen 0 then\n\t\tn:=i64tostrfmt(a,&.str,fmt)\n\twhen 'D','d' then\n\t\tn:=domultichar(ref char(&a),8,&.str,fmt)\n\n\telse\t\t\t\t\t\t!assume 'C'\n\t\tprintchar(a)\t\t\t!no other formatting allowed\n\t\treturn\n\tesac\n\n\tprintstr_n(&.str,n)\nend\n\nproc tostr_u64(word64 a, ref fmtrec fmt)=\n\t[360]char str\n\tint n\n\n\tcase fmt^.charmode\n\twhen 'D','d' then\n\t\tn:=domultichar(ref char(&a),8,&.str,fmt)\n\n\twhen 'C','c' then\n\t\tprintchar(a)\t\t\t!no other formatting allowed\n\t\treturn\n\n\telse\n\t\tn:=u64tostrfmt(a,&.str,fmt)\n\tesac\n\n\tprintstr_n(&.str,n)\nend\n\nproc tostr_r64(real x,ref fmtrec fmt) =\n\t[360]char str,str2\n\t[0:10]char cfmt\n\tint n\n\n\tcfmt[0]:='%'\n\n\tif fmt^.precision then\n\t\tcfmt[1]:='.'\n\t\tcfmt[2]:='*'\n\t\tcfmt[3]:=fmt^.realfmt\n\t\tcfmt[4]:=0\n\t\tsprintf(&.str,&.cfmt,fmt^.precision,x)\n\telse\n\t\tcfmt[1]:=fmt^.realfmt\n\t\tcfmt[2]:=0\n\t\tsprintf(&.str,&.cfmt,x)\n\tfi\n\n!at this point, n is the str length including signs and suffix\n\n\tn:=strlen(&.str)\t\t! current length\n\n\tif n<fmt^.minwidth then\n\t\tn:=expandstr(&.str,&.str2,n,fmt)\n\t\tstrcpy(&.str,&.str2)\n\tfi\n\n\tprintstr_n(&.str,n)\nend\n\nproc tostr_str(ref char s, ref fmtrec fmt) =\n\tint oldlen,newlen,n\n\tref char t\n\n!try and work out size of formatted string\n\toldlen:=strlen(s)\n\tnewlen:=oldlen\n\n\tif fmt^.quotechar or fmt^.minwidth>newlen or fmt^.lettercase then\n\t\tif fmt^.quotechar then\n\t\t\tnewlen+:=2\n\t\tfi\n\t\tif fmt^.minwidth>newlen then\n\t\t\tnewlen:=fmt^.minwidth\n\t\tfi\n\t\tt:=pcm_alloc(newlen+1)\n\t\tn:=strtostrfmt(s,t,oldlen,fmt)\n\t\tprintstr_n(t,n)\n\t\tpcm_free(t,newlen+1)\n\telse\n\t\tprintstr_n(s,oldlen)\n\tfi\nend\n\nfunction getfmt(ichar fmtstyle)ref fmtrec=\n\tstatic fmtrec fmt\n\tif fmtstyle then\n\t\tstrtofmt(fmtstyle,-1,&fmt)\n\t\treturn &fmt\n\telse\n\t\treturn &defaultfmt\n\tfi\nend\n\nexport function strint(int64 a, ichar fmtstyle=nil)ichar=\n\tstatic [100]char str\n\tref fmtrec fmt\n\n\tm_print_startstr(&.str)\n\ttostr_i64(a,fmt:=getfmt(fmtstyle))\n\tm_print_end()\n\treturn getstr(&.str,fmt)\nend\n\nexport proc getstrint(int64 a, ichar dest)=\n\tm_print_startstr(dest)\n\ttostr_i64(a,getfmt(nil))\n\tm_print_end()\nend\n\nexport function strword(word64 a, ichar fmtstyle=nil)ichar=\n\tstatic [100]char str\n\tref fmtrec fmt\n\n\tm_print_startstr(&.str)\n\ttostr_u64(a,fmt:=getfmt(fmtstyle))\n\tm_print_end()\n\treturn getstr(&.str,fmt)\nend\n\nexport function strreal(real a, ichar fmtstyle=nil)ichar=\n\tstatic [320]char str\n\tref fmtrec fmt\n\n\tm_print_startstr(&.str)\n\ttostr_r64(a,fmt:=getfmt(fmtstyle))\n\tm_print_end()\n\treturn getstr(&.str,fmt)\nend\n\nfunction getstr(ichar s, ref fmtrec fmt)ichar=\n\tif fmt^.heapmode then\n\t\treturn pcm_copyheapstring(s)\n\telse\n\t\treturn s\n\tfi\nend\n\nproc initreadbuffer=\n\tif rd_buffer then return fi\n!CPL \"INITREADBUFFER\"\n\trd_buffer:=pcm_alloc(rd_buffersize)\n\trd_buffer^:=0\n\trd_pos:=rd_lastpos:=rd_buffer\nend\n\nglobal proc m_read_conline=\n\tinitreadbuffer()\n\treadlinen(nil,rd_buffer,rd_buffersize)\n\n\trd_length:=strlen(rd_buffer)\n\trd_pos:=rd_buffer\n\trd_lastpos:=nil\nend\n\nglobal proc m_read_fileline(filehandle f)=\n\tinitreadbuffer()\n\treadlinen(f,rd_buffer,rd_buffersize)\n\n\trd_length:=strlen(rd_buffer)\n\trd_pos:=rd_buffer\n\trd_lastpos:=nil\nend\n\nexport proc m_read_strline(ichar s)=\n\tint n\n\n\tinitreadbuffer()\n\tn:=strlen(s)\n\n\tif n<rd_buffersize then\n\t\tstrcpy(rd_buffer,s)\n\telse\n\t\tmemcpy(rd_buffer,s,rd_buffersize-1)\n\t\t(rd_buffer+rd_buffersize-1)^:=0\n\tfi\n\trd_length:=n\n\trd_pos:=rd_buffer\n\trd_lastpos:=nil\nend\n\nfunction readitem(int &itemlength)ref char =\n!read next item from rd_buffer\n!identify a substring that can contain a name, int, real, string or filename\n!return updated position of s that points past the item and past the immediate\n!terminator \n!information about the read item is returned in itemstr, which points to\n!the start of the item, and in itemlength. Item excludes any surrounding whitespace\n!Item can be quoted, then the item points inside the quotes\n!Any embedded quotes are removed, and the characters moved up. The item will\n!be that reduced subsequence\n!NOTE THAT THIS IS DESTRUCTIVE. On reread, the input will be different.\n!I can mitigate this by adding spaces between the end of the item, and the next item,\n!overwriting also the terminator. But this won't restore the line if one of the next\n!reads is literal, using 'L' or 'C' codes.\n\tref char p,s,itemstr\n\tchar quotechar, c\n\n\tunless rd_buffer then \n\t\tinitreadbuffer()\n!abortprogram(\"No readln\")\n\tend unless\n\n\ts:=rd_pos\n\n!scan string, eliminating leading white space\n\twhile s^=' ' or s^=9 do\n\t\t++s\n\tod\n\n\titemstr:=s\t\t\t\t!assume starts here\n\trd_lastpos:=rd_pos:=s\n\n\tif s^=0 then\t\t\t! No more chars left to read return null string\n\t\ttermchar:=0\n\t\titemlength:=0\n\t\treturn s\n\tfi\n\n\tquotechar:=0\t\t\t! Allow possible enclosing single or double quotes\n\tif s^='\"' then\n\t\tquotechar:='\"'\n\t\t++s\n\telsif s^='\\'' then\n\t\tquotechar:='\\''\n\t\t++s\n\tfi\n\n!loop reading characters until separator or end reached\n\tp:=itemstr:=s\n\n\twhile s^ do\n\t\tc:=s++^\n\t\tswitch c\n\t\twhen ' ', 9, comma, '=' then\t\t! separator\n\t\t\tif quotechar or p=s then\t\t\t!can be considered part of name if inside quotes, or is only char\n\t\t\t\tgoto normalchar\n\t\t\tfi\n\t\t\ttermchar:=c\n\t\t\texit\n\t\telse\n\tnormalchar::\n\t\t\tif c=quotechar then\n\t\t\t\tif s^=quotechar then\t! embedded quote\n\t\t\t\t\tp^:=c\n\t\t\t\t\t++s\n\t\t\t\t\t++p\n\t\t\t\telse\t\t\t\t\t! end of name\n\t\t\t\t\ttermchar:=s^\n\t\t\t\t\tif termchar=',' or termchar='=' then\n\t\t\t\t\t\t++s\n\t\t\t\t\t\ttermchar:=s^\n\t\t\t\t\tfi\n\t\t\t\t\texit\n\t\t\t\tfi\n\t\t\telse\n\t\t\t\tp^:=c\n\t\t\t\t++p\n\t\t\tfi\n\t\tendswitch\n\tod\n\n\tif s^=0 then\n\t\ttermchar:=0\n\tfi\n\titemlength:=p-itemstr\t\t\t\t! actual length of token\n\trd_pos:=s\n\n\treturn itemstr\nend\n\nexport function strtoint(ichar s,int length=-1, base=10)int64=\n!return point to next char after terminator (which can be just off length of string)\n\tbyte signd\n\tword64 aa\n\tchar c,d\n\n\titemerror:=0\n\n\tif length=-1 then\n\t\tlength:=strlen(s)\n\tfi\n!check for sign\n\tsignd:=0\n\tif length and s^='-' then\n\t\tsignd:=1; ++s; --length\n\telsif length and s^='+' then\n\t\t++s; --length\n\tfi\n\n\taa:=0\n\twhile length do\n\t\tc:=s++^\n\t\t--length\n\t\tswitch c\n\t\twhen 'A'..'F' then d:=c-'A'+10\n\t\twhen 'a'..'f' then d:=c-'a'+10\n\t\twhen '0'..'9' then d:=c-'0'\n\t\twhen '_', '\\'' then\n\t\t\tnext\n\t\telse\n\t\t\titemerror:=1\n\t\t\texit\n\t\tendswitch\n\n\t\tif d>=base then\n\t\t\titemerror:=1\n\t\t\texit\n\t\tfi\n\t\taa:=aa*base+d\n\tod\n\n\tif signd then\n\t\treturn -aa\n\telse\n\t\treturn aa\n\tfi\nend\n\nglobal function m_read_i64(int fmt=0)int64=\n\tref char s\n\tint length,c\n\tint64 aa\n\n\tcase fmt\n\twhen 'C','c' then\n\t\trd_lastpos:=rd_pos\n\t\tif rd_pos^ then\n\t\t\treturn rd_pos++^\n\t\telse\n\t\t\treturn 0\n\t\tfi\n\twhen 'T','t' then\n\t\treturn termchar\n\twhen 'E','e' then\n\t\treturn itemerror\n\tesac\n\n\ts:=readitem(length)\n\n\n\tcase fmt\n\twhen 0,'I','i' then\n\t\treturn strtoint(s,length)\n\twhen 'B','b' then\n\t\treturn strtoint(s,length,2)\n\twhen 'H','h' then\n\t\treturn strtoint(s,length,16)\n\tesac\n\treturn 0\nend\n\nglobal function m_read_r64(int fmt=0)real=\n\t[512]char str\n\tref char s\n\tint length\n\tint32 numlength\n\treal x\n\n\ts:=readitem(length)\n\n\tif length=0 or length>=str.len then\t\t!assume not a real\n\t\treturn 0.0\n\tfi\n\tmemcpy(&.str,s,length)\n\tstr[length+1]:=0\n\n\titemerror:=0\n\n\tif sscanf(&.str,\"%lf%n\", &x, &numlength)=0 or numlength<>length then\n\t\tx:=0.0\n\t\titemerror:=1\n\tfi\n\n\treturn x\nend\n\nexport proc m_read_str(ref char dest, int destlen=0,fmt=0)=\n\tref char s\n\tint length,numlength\n\treal x\n\n\titemerror:=0\n\tif fmt='L' or fmt='l' then\n\t\ts:=rd_pos\n\t\tlength:=rd_buffer+rd_length-rd_pos\n\n\telse\n\t\ts:=readitem(length)\n\n\t\tif fmt='N' or fmt='n' then\n\t\t\ticonvlcn(s,length)\n\t\tfi\n\tfi\n\n\tif destlen>0 then\n\t\tif length>=destlen then\n\t\t\tlength:=destlen-1\n\t\t\titemerror:=1\n\t\tfi\n\tfi\n\tmemcpy(dest,s,length)\n\t(dest+length)^:=0\nend\n\nexport proc readstr(ref char dest, int fmt=0,destlen=0)=\n\tm_read_str(dest,destlen,fmt)\nend\n\nexport proc rereadln=\n\trd_pos:=rd_buffer\n\trd_lastpos:=rd_pos\nend\n\nexport proc reread=\n\trd_pos:=rd_lastpos\nend\n\nglobal function valint(ichar s, int fmt=0)int64=\n\tref char old_pos, old_lastpos\n\tint64 aa\n\n\tinitreadbuffer()\n\told_pos:=rd_pos\n\told_lastpos:=rd_lastpos\n\n\trd_pos:=s\n\taa:=m_read_i64(fmt)\n\trd_pos:=old_pos\n\trd_lastpos:=old_lastpos\n\treturn aa\nend\n\nglobal function valreal(ichar s)real=\n\tref char old_pos, old_lastpos\n\treal x\n\n\tinitreadbuffer()\n\told_pos:=rd_pos\n\told_lastpos:=rd_lastpos\n\n\trd_pos:=s\n\tx:=m_read_r64()\n\trd_pos:=old_pos\n\trd_lastpos:=old_lastpos\n\treturn x\nend\n\nproc iconvlcn(ref char s,int n) =\t\t!ICONVLCN\n\tto n do\n\t\ts^:=tolower(s^)\n\t\t++s\n\tod\nend\n\nproc iconvucn(ref char s,int n) =\t\t!ICONVUCN\n\tto n do\n\t\ts^:=toupper(s^)\n\t\t++s\n\tod\nend\n\nproc convlcstring(ref char s)=\t\t!CONVLCSTRING\n\twhile (s^) do\n\t\ts^:=tolower(s^)\n\t\t++s\n\tod\nend\n\nproc convucstring(ref char s)=\t\t!CONVUCSTRING\n\twhile (s^) do\n\t\ts^:=toupper(s^)\n\t\t++s\n\tod\nend\n\nglobal function m_power_i64(int64 n,a)int64=\n\tif n<0 then\n\t\treturn 0\n\telsif n=0 then\n\t\treturn 1\n\telsif n=1 then\n\t\treturn a\n\telsif (n iand 1)=0 then\n\t!\treturn ipower(a*a,n/2)\n\t\treturn m_power_i64(n/2,sqr a)\n\telse\t\t\t!assume odd\n\t\treturn m_power_i64((n-1)/2,sqr a)*a\n\tfi\nend\n\nglobal proc m_intoverflow=\n\tabortprogram(\"Integer overflow detected\")\nend\n\nglobal proc m_dotindex(word i,a)=\n!return a.[i] in d0\n\tABORTPROGRAM(\"DOT INDEX\")\n!\tassem\n!\t\tmov d0,[a]\n!\t\tmov cl,[i]\n!\t\tshr d0,cl\n!\t\tand d0,1\n!\tend\t\nend\n\nglobal proc m_dotslice(word j,i,a)=\n!return a.[i..j] in d0; assumes j>=i\n\tABORTPROGRAM(\"DOT SLICE\")\n!\tassem\n!\t\tmov d0,[a]\n!\t\tmov rcx,[i]\n!\t\tshr d0,cl\n!\t\tsub rcx,[j]\n!\t\tneg rcx\t\t\t\t!j-1\n!\t\tmov d2,0xFFFF'FFFF'FFFF'FFFE\n!\t\tshl d2,cl\n!\t\tnot d2\n!\t\tand d0,d2\n!\tend\t\nend\n\nglobal proc m_popdotindex(word i,ref word p,word x)=\n!p^.[i]:=x\n\tABORTPROGRAM(\"POP DOT INDEX\")\n!\tassem\n!\t\tmov d3,[p]\n!\t\tmov cl,[i]\n!\t\tmov d0,[d3]\n!\t\tmov d1,1\n!\t\tshl d1,cl\t\t\t!000001000\n!\t\tnot d1\t\t\t\t!111110111\n!\t\tand d0,d1\t\t\t!clear that bit in dest\n!\t\tmov d1,[x]\n!\t\tand d1,1\n!\t\tshl d1,cl\n!\t\tor d0,d1\n!\t\tmov [d3],d0\n!\tend\t\nend\n\nglobal proc m_popdotslice(word j,i, ref word p, word x)=\n!p^.[i..j]:=x\n\tABORTPROGRAM(\"POP DOT SLICE\")\n!\tassem\n!!d3 = p\n!!d4 = x, then shifted then masked x\n!!d5 = i\n!!d6 = clear mask\n!\n!\t\tmov d3,[p]\n!\t\tmov d4,[x]\n!\t\tmov d5,[i]\n!\t\tmov rcx,d5\t\t\t!i\n!\t\tshl d4,cl\t\t\t!x<<i\n!\t\tmov rcx,[j]\n!\t\tsub rcx,d5\t\t\t!j-i\n!\t\tinc rcx\t\t\t\t!j-i+1\n!\t\tmov d2,0xFFFF'FFFF'FFFF'FFFF\n!\t\tshl d2,cl\t\t\t!...111100000     (assume 5-bit slice)\n!\t\tnot d2\t\t\t\t!...000011111\n!\t\tmov rcx,d5\t\t\t!i\n!\t\tshl d2,cl\t\t\t!...000011111000  (assume i=3)\n!\t\tand d4,d2\t\t\t!mask x (truncate extra bits)\n!\t\tmov d0,[d3]\n!\t\tnot d2\t\t\t\t!...111100000111\n!\t\tand d0,d2\t\t\t!clear dest bits\n!\t\tor d0,d4\t\t\t!add in new bits\n!\t\tmov [d3],d0\n!\tend\t\nend\n\nglobal function m_imin(int64 a,b)int64=\n\treturn min(a,b)\nend\n\nglobal function m_imax(int64 a,b)int64=\n\treturn max(a,b)\nend\n\nglobal function m_sign(real x)real=\n\tif x>0.0 then return 1.0\n\telsif x<0.0 then return -1.0\n\telse return 0.0\n\tfi\nend\n\nexport function m_tp_i64tor64(i64 a)r64 x=\n\tmemcpy(&x, &a, 8)\n\tx\nend\n\nexport function m_tp_r64toi64(r64 x)i64 a=\n\tmemcpy(&a, &x, 8)\n\ta\nend\n\nexport function m_tp_reftoi64(ref void p)i64 a=\n\tmemcpy(&a, &p, 8)\n\ta\nend\n\nexport function m_tp_i64toref(i64 a)ref void p=\n\tmemcpy(&p, &a, 8)\n\tp\nend\n",
(byte*)"!const mem_check=1\nconst mem_check=0\n\nglobal [0..300]u64 allocupper\nglobal int alloccode\t\t\t\t!set by heapalloc\nexport int allocbytes\t\t\t\t!set by heapalloc\nexport int fdebug=0\nexport int rfsize\n\nconst threshold=1<<25\nconst alloc_step=1<<25\nword maxmemory\nint  maxalloccode\n\nGLOBAL REF VOID ALLOCBASE\n\nbyte pcm_setup=0\n\nint show=0\n\nglobal int memtotal=0\nexport int64 smallmemtotal=0\nglobal int smallmemobjs=0\nglobal int maxmemtotal=0\n\nEXPORT INT BIGMEMTOTAL\n!EXPORT INT BIGMEMMAX\n!EXPORT INT SMALLMEMMAX\n\n\n!store all allocated pointers\nconst int maxmemalloc=(mem_check|500000|2)\narray [maxmemalloc+1]ref int32 memalloctable\narray [maxmemalloc+1]int32 memallocsize\n\nconst pcheapsize=1048576*2\nref byte pcheapstart\nref byte pcheapend\t\t\t!points to first address past heap\nref byte pcheapptr\n\nconst int maxblockindex = 8 \t\t!2048\nexport const int maxblocksize = 2048\nexport const int $maxblocksizexx = 2048\n\narray [0:maxblocksize+1]byte sizeindextable\t!convert byte size to block index 1..maxblockindex\n\nconst int size16   = 1\t\t\t!the various index codes\nconst int size32   = 2\nconst int size64   = 3\nconst int size128  = 4\nconst int size256  = 5\nconst int size512  = 6\nconst int size1024 = 7\nconst int size2048 = 8\n\nexport [0:9]ref word freelist\n\nexport record strbuffer =\n\tichar strptr\n\tint32 length\n\tint32 allocated\nend\n\n!export tabledata() [0:]ichar pmnames=\nexport enumdata [0:]ichar pmnames=\n\t(pm_end=0,\t\t$),\n\t(pm_option,\t\t$),\n\t(pm_sourcefile,\t$),\n\t(pm_libfile,\t$),\n\t(pm_colon,\t\t$),\n\t(pm_extra,\t\t$),\nend\n\n[2]word seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)\n!array [2]int seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)\n\nexport function pcm_alloc(int n)ref void =\n\tref byte p\n!CPL \"ALLOC\"\n\n\tif not pcm_setup then\n\t\tpcm_init()\n\tfi\n\n!GOTO SKIP\n\tif n>maxblocksize then\t\t\t!large block allocation\n!SKIP::\n\t\talloccode:=pcm_getac(n)\n\t\tallocbytes:=allocupper[alloccode]\n\n\t\tp:=allocmem(allocbytes)\n\t\tif not p then\n\t\t\tabortprogram(\"pcm_alloc failure\")\n\t\tfi\n!CPL \"ALLOC\",N\nbigmemtotal+:=allocbytes\n!bigmemmax max:=bigmemtotal\n\n\t\tif mem_check then addtomemalloc(ref int32(p),allocbytes) fi\n\n\t\treturn p\n\tfi\n\n\talloccode:=sizeindextable[n]\t\t!Size code := 0,1,2 etc for 0, 16, 32 etc\n\tallocbytes:=allocupper[alloccode]\n\tsmallmemtotal+:=allocbytes\n!SMALLMEMMAX MAX:=SMALLMEMTOTAL\n\n\tif p:=ref byte(freelist[alloccode]) then\t\t!Items of this block size available\n\t\tif mem_check then addtomemalloc(ref int32(p),allocbytes) fi\n\t\tfreelist[alloccode]:=ref word(int((freelist[alloccode])^))\n\n\t\treturn p\n\tfi\n\n!No items in freelists: allocate new space in this heap block\n\tp:=pcheapptr\t\t\t\t!Create item at start of remaining pool in heap block\n\tpcheapptr+:=allocbytes\t\t\t!Shrink remaining pool\n\n\tif pcheapptr>=pcheapend then\t\t!Overflows?\n\t\tp:=pcm_newblock(allocbytes)\t\t!Create new heap block, and allocate from start of that\n\t\treturn p\n\tfi\n\tif mem_check then addtomemalloc(ref int32(p),allocbytes) fi\n\n\treturn p\nend\n\nexport proc pcm_free(ref void p,int n) =\n!n can be the actual size requested it does not need to be the allocated size\n\tint acode\n\n\tif n=0 then return fi\n\n!GOTO SKIP\n\tif n>maxblocksize then\t\t!large block\n!SKIP::\n\t\tif mem_check then removefrommemalloc(p,n) fi\n\nBIGMEMTOTAL-:=N\n\n!MAXMEMTOTAL-:=N\n\n\t\tfree(p)\n\t\treturn\n\tfi\n\n\tif p then\n\t\tacode:=sizeindextable[n]\t\t!Size code := 0,1,2 etc for 0, 16, 32 etc\n\n\t\tsmallmemtotal-:=allocupper[acode]\n\n\t\tif mem_check then removefrommemalloc(p,allocupper[acode]) fi\n\n\t\tcast(p,ref word)^:=word(int(freelist[acode]))\n\t\tfreelist[acode]:=p\n\tfi\nend\n\nexport proc pcm_freeac(ref void p,int alloc) =\n\tpcm_free(p,allocupper[alloc])\nend\n\n!export proc pcm_copymem4(ref void p,q,int n) =\t!PCM_COPYMEM4\n!!copy n bytes of memory from q to p.\n!!the memory spaces used are multiples of 16 bytes, but n itself could be anything\n!!n can be zero, and need not be a multiple of 4 bytes\n!\n!\tmemcpy(p,q,n)\n!end\n\nexport proc pcm_clearmem(ref void p,int n) =\n\tmemset(p,0,n)\nend\n\nexport proc pcm_init =\n!set up sizeindextable too\n\tint j,k,k1,k2\n\tint64 size\n\tconst limit=1<<33\n\n\talloccode:=0\n\tif pcm_setup then\n\t\treturn\n\tfi\n\n\tpcm_newblock(0)\n\n!\tALLOCBASE:=PCHEAPPTR\n\n\tfor i to maxblocksize do\t!table converts eg. 78 to 4 (4th of 16,32,64,128)\n\t\tj:=1\n\t\tk:=16\n\t\twhile i>k do\n\t\t\tk:=k<<1\n\t\t\t++j\n\t\tod\n\t\tsizeindextable[i]:=j\n\tod\n\n\tallocupper[1]:=16\n\tsize:=16\n\n\tfor i:=2 to 27 do\n\t\tsize*:=2\n\t\tallocupper[i]:=size\n\t\tif size>=threshold then\n\t\t\t\tk:=i\n\t\t\texit\n\t\tfi\n\tod\n\n\tfor i:=k+1 to allocupper.upb do\n\t\tsize+:=alloc_step\n\t\tif size<limit then\n\t\t\tallocupper[i]:=size\n\t\t\tmaxmemory:=size\n\t\telse\n\t\t\tmaxalloccode:=i-1\n\t\t\texit\n\t\tfi\n\t\t\n\tod\n\tpcm_setup:=1\n\n!FOR I,J IN SIZEINDEXTABLE DO\n!\tPRINTLN I,J, allocupper[i]\n!OD \n\nend\n\nexport function pcm_getac(int size)int =\n! convert linear blocksize from 0..approx 2GB to 8-bit allocation code\n\n!sizeindextable scales values from 0 to 2048 to allocation code 0 to 9\n\n\tif size<=maxblocksize then\n\t\treturn sizeindextable[size]\t\t!size 0 to 2KB\n\tfi\n\n\tsize:=(size+255)>>8\t\t\t\t\t!scale by 256\n\n!now same sizetable can be used for 2KB to 512KB (288 to 2KB)\n\n\tif size<=maxblocksize then\n\t\treturn sizeindextable[size]+8\n\tfi\n\n!sizetable now used for 512KB to 128MB (to 2KB)\n\tsize:=(size+63)>>6\t\t\t\t\t!scale by 256\n\n\tif size<=maxblocksize then\n\t\treturn sizeindextable[size]+14\n\tfi\n\n!size>2048, which means it had been over 128MB.\n\tsize:=(size-2048+2047)/2048+22\n\treturn size\nend\n\nexport function pcm_newblock(int itemsize)ref void=\n!create new heap block (can be first)\n!also optionally allocate small item at start\n!return pointer to this item (and to the heap block)\n\tstatic int totalheapsize\n\tref byte p\n\n\ttotalheapsize+:=pcheapsize\n\talloccode:=0\n\tp:=allocmem(pcheapsize)\t!can't free this block until appl terminates\n\tif p=nil then\n\t\tabortprogram(\"Can't alloc pc heap\")\n\tfi\n\n\tpcheapptr:=p\n\tpcheapend:=p+pcheapsize\n\n\tif pcheapstart=nil then\t\t!this is first block\n\t\tpcheapstart:=p\n\tfi\n\tpcheapptr+:=itemsize\n\treturn ref u32(p)\nend\n\nexport function pcm_round(int n)int =\n!for any size n, return actual number of bytes that would be allocated\n\tstatic [0:maxblockindex+1]int32 allocbytes=(0,16,32,64,128,256,512,1024,2048)\n\n\tif n>maxblocksize then\n\t\treturn n\n\telse\n\t\treturn allocbytes[sizeindextable[n]]\n\tfi\nend\n\n!export function pcm_array(int n)int =\t\t!PCM_ARRAY\n!!n bytes are needed for an array return the number of bytes to be actually allocated\n!\tint m\n!\n!\tif n<=maxblocksize then\t!automatic rounding up used for small heap\n!\t\treturn pcm_round(n)\n!\telse\t\t\t\t!devise some strategy probably doubling up.\n!\t\tm:=2048\n!\t\twhile n>m do\n!\t\t\tm<<:=1\n!\t\tod\n!\t\treturn m\n!\tfi\n!\n!end\n!\n!global proc pcm_printfreelist(int size,ref word p) =\t\t!PCM_PRINTFREELIST\n!\tprintln \"Size: \",size\n!\twhile p do\n!\t\tprint \" \",,p:\"h\"\n!\t\tp:=ref word(int(p^))\n!\tod\n!\tputs(\"\")\n!end\n!\n!global proc pcm_diags(ref char caption) =\t\t!PCM_DIAGS\n!\tint m\n!\n!\tprintln \"HEAP FREELISTS:\",caption\n!\n!\tm:=16\n!\tfor i:=1 to 8 do\n!\t\tpcm_printfreelist(m,freelist[i])\n!\t\tm<<:=1\n!\tod\n!end\n\nexport function pcm_allocz(int n)ref void =\n\tref void p\n\tp:=pcm_alloc(n)\n\n\tmemset(p,0,n)\n\treturn p\nend\n\nexport function pcm_copyheapstring(ref char s)ref char =\n!allocate enough bytes for string s: copy s to the heap\n!return pointer to new string\n\tref char q\n\tint n\n\tif s=nil then return nil fi\n\n\tn:=strlen(s)+1\n\tq:=pcm_alloc(n)\n\tmemcpy(q,s,n)\n\treturn q\nend\n\nexport function pcm_copyheapstringn(ref char s,int n)ref char =\n\tref char q\n\tif s=nil then return nil fi\n\n\tq:=pcm_alloc(n+1)\n\tmemcpy(q,s,n)\n\t(q+n)^:=0\n\treturn q\nend\n\nexport function pcm_copyheapblock(ref char s, int length)ref char =\n!allocate enough bytes for string s: copy s to the heap\n!return pointer to new string\n\tref char q\n\tif length=0 then return nil fi\n\n\tq:=pcm_alloc(length)\n\tmemcpy(q,s,length)\n\treturn q\nend\n\nproc addtomemalloc(ref int32 ptr,int size)=\n!add ptr to allocated table\n\tint allocated, code\n\n!CPL \"***************ADD TO ALLOC:\",ptr,size\n\tfor i to maxmemalloc do\n\t\tif memalloctable[i]=ptr then\n\t\t\tCPL \"ALLOC ERROR:\",ptr,\"ALREADY ALLOCATED\\n\\n\\n\"\n\t\t\tstop 2\n\t\tfi\n\n\t\tif memalloctable[i]=nil then\t\t!unused entry\n\t\t\tmemalloctable[i]:=ptr\n\n\t\t\tcode:=pcm_getac(size)\n\t\t\tallocated:=allocupper[code]\n\n!CPL \"STORING\",SIZE,ALLOCATED\n\t\t\tmemallocsize[i]:=allocated\n!\t\t\tmemallocsize[i]:=size\n\t\t\treturn\n\t\tfi\n\tod\n\tCPL \"MEMALLOCTABLE FULL\\n\\n\\n\\n\"; os_getch()\n\tCPL\n\tstop 3\nend\n\nproc removefrommemalloc(ref int32 ptr,int size)=\n!remove ptr to allocated table\n\tint allocated, code\n\n!CPL \"------------------************REMOVE FROM ALLOC:\",ptr,size\n\tcode:=pcm_getac(size)\n\tallocated:=allocupper[code]\n\n\tfor i to maxmemalloc do\n\t\tif memalloctable[i]=ptr then\n\t\t\tif memallocsize[i]<>ALLOCATED then\n!\t\t\t\tCPL \"REMOVE:FOUND\",ptr,\"IN MEMALLOCTABLE, FREESIZE=\",size,\", BUT STORED AS BLOCK SIZE:\",memallocsize[i]\n\t\t\t\tCPL \"REMOVE:FOUND\",ptr,\"IN MEMALLOCTABLE, ROUNDED FREESIZE=\",ALLOCATED,\", BUT STORED AS BLOCK SIZE:\",memallocsize[i]\n\t\t\t\tabortprogram(\"MEMSIZE\")\n\t\t\tfi\n\t\t\tmemalloctable[i]:=nil\n\t\t\treturn\n\t\tfi\n\tod\n\tCPL \"CAN'T FIND\",ptr,\"IN MEMALLOCTABLE\",size\n\tCPL \nos_GETCH()\n\tabortprogram(\"MEM\")\n\tstop 4\nend\n\nexport function allocmem(int n)ref void =\n\tref void p\n\n\tp:=malloc(n)\n\tif p then\n\t\treturn p\n\tfi\n\tprintln n,memtotal\n\tabortprogram(\"Alloc mem failure\")\n\treturn nil\nend\n\nglobal function reallocmem(ref void p,int n)ref void =\n\tp:=realloc(p,n)\n\treturn p when p\n\tprintln n\n\tabortprogram(\"Realloc mem failure\")\n\treturn nil\nend\n\nexport proc abortprogram(ref char s) =\n\tprintln s\n\tprint   \"ABORTING: Press key...\"\n!os_getch()\n\tstop 5\nend\n\nexport function getfilesize(filehandle handlex)int=\n\tword32 p,size\n\n\tp:=ftell(handlex)\t\t!current position\n\tfseek(handlex,0,2)\t\t!get to eof\n\tsize:=ftell(handlex)\t\t!size in bytes\n\tfseek(handlex,p,seek_set)\t!restore position\n\treturn size\nend\n\nexport proc readrandom(filehandle handlex, ref byte mem, int offset, size) =\n\tint a\n\tfseek(handlex,offset,seek_set)\n\ta:=fread(mem,1,size,handlex)\t\t\t!assign so as to remove gcc warning\nend\n\nexport function writerandom(filehandle handlex, ref byte mem, int offset,size)int =\n\tfseek(handlex,offset,seek_set)\n\treturn fwrite(mem,1,size,handlex)\nend\n\nexport function setfilepos(filehandle file,int offset)int=\n\treturn fseek(file,offset,0)\nend\n\nexport function getfilepos(filehandle file)int=\n\treturn ftell(file)\nend\n\nexport function readfile(ref char filename)ref byte =\n\tfilehandle f\n\tint size\n\tref byte m,p\n\n\tf:=fopen(filename,\"rb\")\n\tif f=nil then\n\t\treturn nil\n\tfi\n\trfsize:=size:=getfilesize(f)\n\n!\tm:=malloc(size+2)\t\t!allow space for etx/zeof etc\n\tm:=pcm_alloc(size+2)\t\t!allow space for etx/zeof etc\n\n\tif m=nil then\n\t\treturn nil\n\tfi\n\n\treadrandom(f,m,0,size)\n\tp:=m+size\t\t\t!point to following byte\n\t(ref u16(p)^:=0)\t!add two zero bytes\n\n\tfclose(f)\n\treturn m\nend\n\nexport function writefile(ref char filename,ref byte data,int size)int =\n\tfilehandle f\n\tint n\n\n\tf:=fopen(filename,\"wb\")\n\tif f=nil then\n\t\treturn 0\n\tfi\n\n\tn:=writerandom(f,data,0,size)\n\tfclose(f)\n\treturn n\nend\n\nexport function checkfile(ref char file)int=\n\tfilehandle f\n\tif f:=fopen(file,\"rb\") then\n\t\tfclose(f)\n\t\treturn 1\n\tfi\n\treturn 0\nend\n\nexport proc readlinen(filehandle handlex,ref char buffer,int size) =\n!size>2\n\tint ch\n\tref char p\n\tint n\n\tarray [0:100]char buff\n\tbyte crseen\n\n\tif handlex=nil then\n\t\thandlex:=filehandle(os_getstdin())\n\tfi\n\tif handlex=nil then\n\t\tn:=0\n\t\tp:=buffer\n\t\tdo\n\t\t\tch:=getchar()\n\t\t\tif ch=13 or ch=10 or ch=-1 then\n\t\t\t\tp^:=0\n\t\t\t\treturn\n\t\t\tfi\n\t\t\tp++^:=ch\n\t\t\t++n\n\t\t\tif n>=(size-2) then\n\t\t\t\tp^:=0\n\t\t\t\treturn\n\t\t\tfi\n\t\tod\n\tfi\n\n\tbuffer^:=0\n\tif fgets(buffer,size-2,handlex)=nil then\n\t\treturn\n\tfi\n\n\tn:=strlen(buffer)\n\tif n=0 then\n\t\treturn\n\tfi\n\n\tp:=buffer+n-1\t\t!point to last char\n\tcrseen:=0\n\twhile (p>=buffer and (p^=13 or p^=10)) do\n\t\tif p^=13 or p^=10 then crseen:=1 fi\n\t\tp--^ :=0\n\tod\n\n!NOTE: this check doesn't work when a line simply doesn't end with cr-lf\n\n\tif not crseen and (n+4>size) then\n\t\tcpl size,n\n\t\tabortprogram(\"line too long\")\n\tfi\nend\n\nexport proc iconvlcn(ref char s,int n) =\n\tto n do\n\t\ts^:=tolower(s^)\n\t\t++s\n\tod\nend\n\nexport proc iconvucn(ref char s,int n) =\n\tto n do\n\t\ts^:=toupper(s^)\n\t\t++s\n\tod\nend\n\nexport function convlcstring(ref char s)ichar s0=\n\ts0:=s\n\twhile (s^) do\n\t\ts^:=tolower(s^)\n\t\t++s\n\tod\n\ts0\nend\n\nexport function convucstring(ref char s)ichar s0=\n\ts0:=s\n\twhile (s^) do\n\t\ts^:=toupper(s^)\n\t\t++s\n\tod\n\ts0\nend\n\nexport function changeext(ref char s,newext)ichar=\n!whether filespec has an extension or not, change it to newext\n!newext should start with \".\"\n!return new string (locally stored static string, so must be used before calling again)\n\tstatic [260]char newfile\n\tarray [32]char newext2\n\tref char sext\n\tint n\n\n\tstrcpy(&newfile[1],s)\n\n\tcase newext^\n\twhen 0 then\n\t\tnewext2[1]:=0\n\t\tnewext2[2]:=0\n\twhen '.' then\n\t\tstrcpy(&newext2[1],newext)\n\telse\n\t\tstrcpy(&newext2[1],\".\")\n\t\tstrcat(&newext2[1],newext)\n\tesac\n\n\n\tsext:=extractext(s,1)\t\t\t!include \".\" when it is only extension\n\n\tcase sext^\n\twhen 0 then\t\t\t\t\t\t!no extension not even \".\"\n\t\tstrcat(&newfile[1],&newext2[1])\n\twhen '.' then\t\t\t\t\t\t!no extension not even \".\"\n\t\tstrcat(&newfile[1],&newext2[2])\n\telse\t\t\t\t\t\t\t!has extension\n\t\tn:=sext-s-2\t\t\t!n is number of chars before the \".\"\n\t\tstrcpy(&newfile[1]+n+1,&newext2[1])\n\tesac\n\n\treturn &newfile[1]\nend\n\nexport function extractext(ref char s,int period=0)ichar=\n!if filespec s has an extension, then return pointer to it otherwise return \"\"\n!if s ends with \".\", then returns \".\"\n\tref char t,u\n\n\tt:=extractfile(s)\n\n\tif t^=0 then\t\t\t!s contains no filename\n\t\treturn \"\"\n\tfi\n\n!t contains filename+ext\n\tu:=t+strlen(t)-1\t\t!u points to last char of t\n\n\twhile u>=t do\n\t\tif u^='.' then\t\t!start extension found\n\t\t\tif (u+1)^=0 then\t\t!null extension\n\t\t\t\treturn (period|\".\"|\"\")\n\t\t\tfi\n\t\t\treturn u+1\t\t\t!return last part of filename as extension exclude the dot\n\t\tfi\n\t\t--u\n\tod\n\treturn \"\"\t\t\t!no extension seen\nend\n\nexport function extractpath(ref char s)ichar=\n\tstatic [0:260]char str\n\tref char t\n\tint n\n\n\tt:=s+strlen(s)-1\t\t!t points to last char\n\n\twhile (t>=s) do\n\t\tswitch t^\n\t\twhen '\\\\','/',':' then\t\t!path separator or drive letter terminator assume no extension\n\t\t\tn:=t-s+1\t\t\t!n is number of chars in path, which includes rightmost / or \\ or :\n\t\t\tmemcpy(&.str,s,n)\n\t\t\tstr[n]:=0\n\t\t\treturn &.str\n\t\tendswitch\n\t\t--t\n\tod\n\treturn \"\"\t\t\t!no path found\nend\n\nexport function extractfile(ref char s)ichar=\n\tref char t\n\n\tt:=extractpath(s)\n\n\tif t^=0 then\t\t\t!s contains no path\n\t\treturn s\n\tfi\n\n\treturn s+strlen(t)\t\t!point to last part of s that contains the file\n\tend\n\nexport function extractbasefile(ref char s)ichar=\n\tstatic [0:100]char str\n\tref char f,e\n\tint n,flen\n\n\tf:=extractfile(s)\n\tflen:=strlen(f)\n\tif flen=0 then\t\t!s contains no path\n\t\treturn \"\"\n\tfi\n\te:=extractext(f,0)\n\n\tif e^ then\t\t\t!not null extension\n\t\tn:=flen-strlen(e)-1\n\t\tmemcpy(&str,f,n)\n\t\tstr[n]:=0\n\t\treturn &.str\n\tfi\n\tif (f+flen-1)^='.' then\n\t\tmemcpy(&str,f,flen-1)\n\t\tstr[flen-1]:=0\n\t\treturn &.str\n\tfi\n\treturn f\nend\n\nexport function addext(ref char s,ref char newext)ichar=\n!when filespec has no extension of its own, add newext\n\tref char sext\n\n\tsext:=extractext(s,1)\n\n\tif sext^=0 then\t\t\t\t\t\t!no extension not even \".\"\n\t\treturn changeext(s,newext)\n\tfi\n\n\treturn s\t\t\t\t\t\t\t!has own extension; use that\nend\n\n!export function alloctable(int n, size)ref void =\t\t!ALLOCTABLE\n!!Allocate table space for n elements, each of size <size>\n!!Allows for 1-based indexing, so allocates (n+1) elements\n!\tref void p\n!\n!\tp:=malloc((n+1)*size)\n!\n!\tif not p then\n!\t\tabortprogram(\"Alloctable failure\")\n!\tfi\n!\treturn p\n!end\n\n!export function zalloctable(int n, size)ref void =\t\t!ALLOCTABLE\n!!Allocate table space for n elements, each of size <size>\n!!Allows for 1-based indexing, so allocates (n+1) elements\n!\tref int p\n!\n!\tp:=alloctable(n,size)\n!\n!\tpcm_clearmem(p,(n+1)*size)\n!\treturn p\n!end\n!\n!global proc checkfreelists(ichar s)=\n!\tref word p,q\n!\tint64 aa\n!\n!\tfor i:=2 to 2 do\n!\t\tp:=freelist[i]\n!\n!\t\twhile p do\n!\t\t\taa:=int64(p)\n!\t\t\tif aa>0xffff'FFFF or aa<100 then\n!\t\t\t\tCPL s,\"FREE LIST ERROR\",i,p,q\n!!\t\t\tos_getch(); stop 1\n!\t\t\tfi\n!\t\t\tq:=p\n!\t\t\tp:=ref word(int(p^))\n!\t\tod\n!\n!\tod\n!end\n\nexport function pcm_alloc32:ref void =\n\tref byte p\n\n!RETURN PCM_ALLOC(32)\n\n\tallocbytes:=32\n\tsmallmemtotal+:=32\n!SMALLMEMMAX MAX:=SMALLMEMTOTAL\n\n\tif p:=ref byte(freelist[2]) then\t\t!Items of this block size available\n\t\tfreelist[2]:=ref word(int((freelist[2])^))\n\t\treturn p\n\tfi\n\n!No items in freelists: allocate new space in this heap block\n\treturn pcm_alloc(32)\nend\n\nexport proc pcm_free32(ref void p) =\n!n can be the actual size requested it does not need to be the allocated size\n\n!PCM_FREE(P,32)\n!RETURN\n\n\tsmallmemtotal-:=32\n\tif mem_check then removefrommemalloc(p,32) fi\n\n\tcast(p,ref word)^:=word(int(freelist[2]))\n\tfreelist[2]:=p\nend\n\nexport function pcm_alloc64:ref void =\n\tref byte p\n\tallocbytes:=64\n\tsmallmemtotal+:=64\n!SMALLMEMMAX MAX:=SMALLMEMTOTAL\n\n\tif p:=ref byte(freelist[3]) then\t\t!Items of this block size available\n\t\tfreelist[3]:=ref word(int((freelist[3])^))\n\t\treturn p\n\tfi\n\n!No items in freelists: allocate new space in this heap block\n\treturn pcm_alloc(64)\n\n!\tp:=pcheapptr\t\t\t\t!Create item at start of remaining pool in heap block\n!\tpcheapptr+:=64\t\t\t!Shrink remaining pool\n!\n!\tif pcheapptr>=pcheapend then\t\t!Overflows?\n!\t\tp:=pcm_newblock(64)\t\t!Create new heap block, and allocate from start of that\n!\t\treturn p\n!\tfi\n!\tif mem_check then addtomemalloc(ref int32(p),allocbytes) fi\n!\n!\treturn p\nend\n\nexport proc pcm_free64(ref void p) =\n!n can be the actual size requested it does not need to be the allocated size\n\n\tsmallmemtotal-:=64\n\tif mem_check then removefrommemalloc(p,64) fi\n\n\tcast(p,ref word)^:=word(int(freelist[3]))\n\tfreelist[3]:=p\nend\n\nexport function pcm_alloc16:ref void =\n\tref byte p\n\tallocbytes:=16\n\tsmallmemtotal+:=16\n!SMALLMEMMAX MAX:=SMALLMEMTOTAL\n\n\tif p:=ref byte(freelist[1]) then\t\t!Items of this block size available\n\t\tfreelist[1]:=ref word(int((freelist[1])^))\n\t\treturn p\n\tfi\n\n!No items in freelists: allocate new space in this heap block\n\treturn pcm_alloc(16)\nend\n\nexport proc pcm_free16(ref void p) =\n!n can be the actual size requested it does not need to be the allocated size\n\n\tsmallmemtotal-:=16\n\tif mem_check then removefrommemalloc(p,32) fi\n\n\tcast(p,ref word)^:=word(int(freelist[1]))\n\tfreelist[1]:=p\nend\n\nexport proc outbyte(filehandle f,int x)=\n\tfwrite(&x,1,1,f)\nend\n\nexport proc outword16(filehandle f,word x)=\n\tfwrite(&x,2,1,f)\nend\n\nexport proc outword32(filehandle f,word x)=\n\tfwrite(&x,4,1,f)\nend\n\nexport proc outword64(filehandle f,word64 x)=\n\tfwrite(&x,8,1,f)\nend\n\nexport proc outstring(filehandle f, ichar s)=\n\tfwrite(s,strlen(s)+1,1,f)\nend\n\nexport proc outblock(filehandle f, ref void p, int n)=\n\tfwrite(p,n,1,f)\nend\n\nexport function myeof(filehandle f)int=\n\tint c\n\n\tc:=fgetc(f)\n\tif c=c_eof then return 1 fi\n\tungetc(c,f)\n\treturn 0;\nend\n\nexport proc strbuffer_add(ref strbuffer dest, ichar s, int n=-1)=\n\tint newlen,oldlen\n\tichar newptr\n\n\tIF N=0 THEN CPL \"N=0\" FI\n\n\tif n=-1 then\n\t\tn:=strlen(s)\n\tfi\n\n\toldlen:=dest.length\n\n\tif oldlen=0 then\t\t\t\t!first string\n\t\tdest.strptr:=pcm_alloc(n+1)\n\t\tdest.allocated:=allocbytes\n\t\tdest.length:=n\t\t\t\t!length always excludes terminator\n\t\tmemcpy(dest.strptr,s,n)\n\t\t(dest.strptr+n)^:=0\n\t\treturn\n\tfi\n\n\tnewlen:=oldlen+n\n\tif newlen+1>dest.allocated then\n\t\tnewptr:=pcm_alloc(newlen+1)\n\t\tmemcpy(newptr,dest.strptr,oldlen)\n\t\tdest.strptr:=newptr\n\t\tdest.allocated:=allocbytes\n\tfi\n\n\tmemcpy(dest.strptr+oldlen,s,n)\n\t(dest.strptr+newlen)^:=0\n\n\tdest.length:=newlen\nend\n\nexport proc gs_init(ref strbuffer dest)=\n\tpcm_clearmem(dest,strbuffer.bytes)\nend\n\nexport proc gs_free(ref strbuffer dest)=\n\tif dest.allocated then\n\t\tpcm_free(dest.strptr,dest.allocated)\n\tfi\nend\n\nexport proc gs_str(ref strbuffer dest,ichar s)=\n\tstrbuffer_add(dest,s)\nend\n\nexport proc gs_char(ref strbuffer dest,int c)=\n\tarray [16]char s\n\n\ts[1]:=c\n\ts[2]:=0\n\n\tstrbuffer_add(dest,&.s,1)\nend\n\nexport proc gs_strn(ref strbuffer dest,ichar s,int length)=\n\tstrbuffer_add(dest,s,length)\nend\n\nexport proc gs_strvar(ref strbuffer dest,s)=\n\tstrbuffer_add(dest,s.strptr)\nend\n\nexport proc gs_strint(ref strbuffer dest,int64 a)=\n\tstrbuffer_add(dest,strint(a))\nend\n\nexport proc gs_strln(ref strbuffer dest,ichar s)=\n\tgs_str(dest,s)\n\tgs_line(dest)\nend\n\nexport proc gs_strsp(ref strbuffer dest,ichar s)=\n\tgs_str(dest,s)\n\tgs_str(dest,\" \")\nend\n\nexport proc gs_line(ref strbuffer dest)=\n\tstrbuffer_add(dest,\"\\w\")\nend\n\nexport function gs_getcol(ref strbuffer dest)int=\n\treturn dest.length\nend\n\nexport proc gs_leftstr(ref strbuffer dest, ichar s, int w, padch=' ')=\n\tint col,i,n,slen\n\tarray [2560]char str\n\tcol:=dest.length\n\tstrcpy(&.str,s)\n\tslen:=strlen(s)\n\tn:=w-slen\n\tif n>0 then\n\t\tfor i:=1 to n do\n\t\t\tstr[slen+i]:=padch\n\t\tod\n\t\tstr[slen+n+1]:=0\n\tfi\n\tgs_str(dest,&.str)\nend\n\nexport proc gs_leftint(ref strbuffer dest, int a, int w, padch=' ')=\n\tgs_leftstr(dest,strint(a),w,padch)\nend\n\nexport proc gs_padto(ref strbuffer dest,int col, ch=' ')=\n\tint n\n\tarray [2560]char str\n\n\tn:=col-dest.length\n\tif n<=0 then return fi\n\tfor i:=1 to n do\n\t\tstr[i]:=ch\n\tod\n\tstr[n+1]:=0\n\tgs_str(dest,&.str)\nend\n\nexport proc gs_println(ref strbuffer dest,filehandle f=nil)=\n\tif dest.length=0 then return fi\n\t(dest.strptr+dest.length)^:=0\n\n\tif f=nil then\n!\t\tprintln dest.strptr,,\"\\c\"\n\t\tprintln dest.strptr\n\telse\n!\t\tprintln @f,dest.strptr,,\"\\c\"\n\t\tprintln @f,dest.strptr\n\tfi\nend\n\nexport function nextcmdparamnew(int &paramno, ichar &name, &value, ichar defext=nil)int=\n\tstatic int infile=0\n\tstatic ichar filestart=nil\n\tstatic ichar fileptr=nil\n\tstatic byte colonseen=0\n\tref char q\n\tichar item,fileext\n\tichar rest\n\tint length\n\tstatic [300]char str\n\n\treenter::\n\tvalue:=nil\n\tname:=nil\n\n\tif infile then\n\t\tif readnextfileitem(fileptr,item)=0 then\t\t!eof\n\t\t\tfree(filestart)\t\t\t\t\t\t\t\t!file allocated via malloc\n\t\t\tinfile:=0\n\t\t\tgoto reenter\n\t\tfi\n\telse\n\t\tif paramno>ncmdparams then\n\t\t\treturn pm_end\n\t\tfi\n\t\titem:=cmdparams[paramno]\n\t\t++paramno\n\n\t\tlength:=strlen(item)\n\n\t\tif item^='@' then\t\t!@ file\n!\t\t\tfilestart:=fileptr:=cast(readfile(item+1))\n!\t\t\tif filestart=nil then\n!\t\t\t\tprintln \"Can't open\",item\n!\t\t\t\tstop 7\n!\t\t\tfi\n\t\t\tinfile:=1\n\t\t\tgoto reenter\n\t\tfi\n\n\t\tif item^=':' then\n\t\t\tcolonseen:=1\n\t\t\treturn pm_colon\n\t\tfi\n\tfi\n\n\tvalue:=nil\n\tif item^='-' then\n\t\tname:=item+(colonseen|0|1)\n\t\tq:=strchr(item,':')\n\t\tif not q then\n\t\t\tq:=strchr(item,'=')\n\t\tfi\n\t\tif q then\n\t\t\tvalue:=q+1\n\t\t\tq^:=0\n\t\tfi\n\t\treturn (colonseen|pm_extra|pm_option)\n\tfi\n\n\tfileext:=extractext(item,0)\n\tname:=item\n\n\tif fileext^=0 then\t\t\t\t\t\t\t!no extension\n\t\tstrcpy(&.str,name)\n\t\tif defext and not colonseen then\n\t\t\tname:=addext(&.str,defext)\t\t\t\t!try .c\n\t\tfi\n!\telsif eqstring(fileext,\"dll\") then\n\telsif eqstring(fileext,\"dll\") or eqstring(fileext,\"mcx\") then\n\t\treturn (colonseen|pm_extra|pm_libfile)\n\tfi\n\treturn (colonseen|pm_extra|pm_sourcefile)\nend\n\nfunction readnextfileitem(ichar &fileptr,&item)int=\n\tref char p,pstart,pend\n\tint n\n\tstatic [256]char str\n\n\tp:=fileptr\n\n\treenter::\n\tdo\n\t\tcase p^\n\t\twhen ' ','\\t',13,10 then\t!skip white space\n\t\t\t++p\n\t\twhen 26,0 then\t\t\t\t!eof\n\t\t\treturn 0\n\t\telse\n\t\t\texit\n\t\tesac\n\tod\n\n\tcase p^\n\twhen '!', '#' then\t\t\t!comment\n\t\t++p\n\t\tdocase p++^\n\t\twhen 10 then\n\t\t\tgoto reenter\n\t\twhen 26,0 then\n\t\t\tfileptr:=p-1\n\t\t\treturn 0\n\t\telse\n\n\t\tenddocase\n\tesac\n\n\n\tcase p^\n\twhen '\"' then\t\t\t\t!read until closing \"\n\t\tpstart:=++p\n\t\tdo\n\t\t\tcase p^\n\t\t\twhen 0,26 then\n\t\t\t\tprintln \"Unexpected EOF in @file\"\n\t\t\t\tstop 8\n\t\t\twhen '\"' then\n\t\t\t\tpend:=p++\n\t\t\t\tif p^=',' then ++p fi\n\t\t\t\texit\n\t\t\tesac\n\t\t\t++p\n\t\tod\n\telse\n\t\tpstart:=p\n\t\tdo\n\t\t\tcase p^\n\t\t\twhen 0,26 then\n\t\t\t\tpend:=p\n\t\t\t\texit\n\t\t\twhen ' ','\\t',',',13,10 then\n\t\t\t\tpend:=p++\n\t\t\t\texit\n\t\t\tesac\n\t\t\t++p\n\t\tod\n\tesac\n\n\tn:=pend-pstart\n\tif n>=str.len then\n\t\tprintln \"@file item too long\"\n\t\tstop 9\n\tfi\n\tmemcpy(&.str,pstart,n)\n\tstr[n+1]:=0\n\titem:=&.str\n\tfileptr:=p\n\n\treturn 1\nend\n\nexport proc ipadstr(ref char s,int width,ref char padchar=\" \")=\n\tint n\n\n\tn:=strlen(s)\n\tto width-n do\n\t\tstrcat(s,padchar)\n\tod\nend\n\nexport function padstr(ref char s,int width,ref char padchar=\" \")ichar=\n\tstatic [256]char str\n\n\tstrcpy(&.str,s)\n\tipadstr(&.str,width,padchar)\n\treturn &.str\nend\n\nexport function chr(int c)ichar=\n\tstatic [8]char str\n\n\tstr[1]:=c\n\tstr[2]:=0\n\treturn &.str\nend\n\nexport function cmpstring(ichar s,t)int=\n\tint res\n\tif (res:=strcmp(s,t))<0 then\n\t\treturn -1\n\telsif res>0 then\n\t\treturn 1\n\telse\n\t\treturn 0\n\tfi\nend\n\nexport function cmpstringn(ichar s,t,int n)int=\n\tint res\n\tif (res:=strncmp(s,t,n))<0 then\n\t\treturn -1\n\telsif res>0 then\n\t\treturn 1\n\telse\n\t\treturn 0\n\tfi\nend\n\nexport function eqstring(ichar s,t)int=\n\treturn strcmp(s,t)=0\nend\n\nexport function cmpbytes(ref void p,q,int n)int=\n\tint res\n\tif (res:=memcmp(p,q,n))<0 then\n\t\treturn -1\n\telsif res>0 then\n\t\treturn 1\n\telse\n\t\treturn 0\n\tfi\nend\n\nexport function eqbytes(ref void p,q,int n)int=\n\treturn memcmp(p,q,n)=0\nend\n\nexport proc mseed(word64 a,b=0)=\n\tseed[1]:=a\n\tif b then\n\t\tseed[2]:=b\n\telse\n\t\tseed[2] ixor:=a\n\tfi\nend\n\nexport function mrandom:word =\n!return pure 64-bit word value, 0 to 2**64-1\n!(cast result for signed value)\n!\tword64 x,y\n\tint x,y\n\tx:=seed[1]\n\ty:=seed[2]\n\tseed[1]:=y\n\tx ixor:=(x<<23)\n\tseed[2]:= x ixor y ixor (x>>17) ixor (y>>26)\n\treturn seed[2]+y\nend\n\nexport function mrandomp:int =\n!pure 64-bit int value, positive only, 0 to 2**63-1\n\treturn mrandom() iand 0x7FFF'FFFF'FFFF'FFFF\nend\n\nexport function mrandomint(int n)int=\n!positive random int value from 0 to n-1\n\treturn mrandomp() rem n\nend\n\nexport function mrandomrange(int a,b)int=\n!random int value from a to b inclusive\n!span extent must be 1 to 2**63-1\n\tint span\n\tspan:=b-a+1\n\tif span<=0 then\n\t\treturn 0\n\tfi\n\treturn (mrandomp() rem span)+a\nend\n\nexport function mrandomreal:real x=\n!positive random real value from 0 to just under (but not including) 1.0\n\trepeat x:=mrandomp()/9223372036854775808.0 until x<>1.0\n\treturn x\nend\n\nexport function mrandomreal1:real=\n!positive random real value from 0 to 1.0 inclusive\n\treturn mrandomp()/9223372036854775807\nend\n\nexport function checkpackfile:ref byte=\n!find out if this executable contains extra packed files\n!return 1 or 0\n\n\tint a,offset,i,size\n\t[100]char name\n\t[300]char exefile\n\tref byte packexeptr\t\t\t!for embedded pack files, contains pointer to in-memory version of this .exe file plus extras; else nil\n\tint packexesize\t\t\t\t!byte size\n\tref char packfilename\n\tint packfilesize\n\tref byte packfileptr\n\n!macro getfileint(data,offset)=(ref int32(data+offset))^\n\tmacro getfileint(data,offset)=cast(data+offset,ref int32)^\n\n\tstrcpy(&exefile[1],os_gethostname())\n\tprintln \"Attempting to open\",&.exefile\n\tpackexeptr:=readfile(&exefile[1])\n\n\tif not packexeptr then\n\t\tcpl \"Can't open\",&.exefile,packexeptr\n\t\tstop\n\tfi\n\n\tpackexesize:=rfsize\n\tcpl \"File read OK. Size\",packexesize\n\n\ta:=getfileint(packexeptr,packexesize-int32.bytes)\n\tif a<>'PCAK' then\n\t\tfree(packexeptr)\n\t\tpackfileptr:=nil\n\t\treturn nil\n\tfi\n\n\toffset:=getfileint(packexeptr,packexesize-int32.bytes*2)\n\n\tpackfilename:=cast(packexeptr+offset)\n\toffset+:=strlen(packfilename)+1\n\tpackfilesize:=getfileint(packexeptr,offset)\n\tpackfileptr:=packexeptr+offset+int32.bytes\n\n\treturn packfileptr\nend\n\n!export function pcm_allocx:ref void =\n!\tconst n=32\n!\tref word p\n!\n!\tallocbytes:=32\n!\n!\tif p:=ref word(freelist[2]) then\t\t!Items of this block size available\n!\t\tfreelist[2]:=ref word(int((freelist[2])^))\n!\n!\telse\n!\n!!No items in freelists: allocate new space in this heap block\n!\t\tp:=cast(pcheapptr)\t\t\t\t!Create item at start of remaining pool in heap block\n!\t\tpcheapptr+:=32\t\t\t!Shrink remaining pool\n!\n!\t\tif pcheapptr>=pcheapend then\t\t!Overflows?\n!\t\t\tp:=pcm_newblock(32)\t\t!Create new heap block, and allocate from start of that\n!\t\tfi\n!\n!\t\tp^:=0\n!\t\t(p+1)^:=0\n!\t\t(p+2)^:=0\n!\t\t(p+3)^:=0\n!\n!\t\treturn p\n!\tfi\n!end\n\nexport function readline:ichar=\n\treadln\n\treturn rd_buffer\nend\n\n!export function stralloc(ref void p)ichar=\n!\treturn strint(int(ref byte(p)-allocbase))\n!end\n\nexport function findfunction(ichar name)ref void=\n\tfor i to $getnprocs() do\n\t\tif eqstring($getprocname(i),name) then\n\t\t\treturn $getprocaddr(i)\n\t\tfi\n\tod\n\treturn nil\nend\n\nexport function roundtoblock(int n,align)int=\n!round up n until it is a multiple of filealign (which is a power of two)\n!return aligned value. Returns original if already aligned\n\tif n iand (align-1)=0 then return n fi\n\treturn n+(align-(n iand (align-1)))\nend\n\n",
(byte*)"export type filehandle=ref void\n\nimportdll $cstd=\n\tfunc  malloc\t\t(word64)ref void\n\tfunc  realloc\t(ref void, word)ref void\n\tproc free\t\t(ref void)\n\tproc memset\t\t(ref void, int32, word)\n\tproc memcpy\t\t(ref void, ref void, word)\n\tproc memmove\t\t(ref void, ref void, word)\n\tfunc  clock\t\t:int32\n\tfunc  ftell\t\t(filehandle)int32\n\tfunc  fseek\t\t(filehandle, int32, int32)int32\n\tfunc  fread\t\t(ref void, word, word, filehandle)word\n\tfunc  fwrite\t\t(ref void, word, word, filehandle)word\n\tfunc  getc\t\t(filehandle)int32\n\tfunc  ungetc\t\t(int32, filehandle)int32\n\tfunc  fopen\t\t(ichar a, b=\"rb\")filehandle\n\tfunc  fclose\t\t(filehandle)int32\n\tfunc  fgets\t\t(ichar, int, filehandle)ichar\n\tfunc  remove\t\t(ichar)int32\n\tfunc  rename\t\t(ichar, ichar)int32\n\tfunc  getchar\t:int32\n\tproc putchar\t(int32)\n\tproc setbuf\t\t(filehandle, ref byte)\n\n\tfunc  strlen\t\t(ichar)int\n\tfunc  strcpy\t\t(ichar, ichar)ichar\n\tfunc  strcmp\t\t(ichar, ichar)int32\n\tfunc  strncmp\t(ichar, ichar, word)int32\n\tfunc  strncpy\t(ichar, ichar, word)word\n\tfunc  memcmp\t\t(ref void, ref void, word)int32\n\tfunc  strcat\t\t(ichar, ichar)ichar\n\tfunc  tolower\t(int32)int32\n\tfunc  toupper\t(int32)int32\n\tfunc  isalpha\t(int32)int32\n\tfunc  isupper\t(int32)int32\n\tfunc  islower\t(int32)int32\n\tfunc  isalnum\t(int32)int32\n\tfunc  isspace\t(int32)int32\n\tfunc  strstr\t\t(ichar, ichar)ichar\n\tfunc  atol\t\t(ichar)int\n\tfunc  atoi\t\t(ichar)int32\n!\tfunc  strtod\t\t(ichar,ref ref char)real64\n\tfunc  strtod\t\t(ichar,ref ref char)real64\n\tfunc  _strdup\t(ichar)ichar\n\n\tfunc  puts\t\t(ichar)int32\n\tfunc  printf\t\t(ichar, ...)int32\n\n\tfunc  sprintf\t(ichar, ichar, ...)int32\n\n\tfunc  sscanf\t\t(ichar, ichar, ...)int32\n\tfunc  scanf\t\t(ichar, ...)int32\n\n\tfunc  rand\t\t:int32\n\tproc srand\t\t(word32)\n\tfunc  system\t\t(ichar)int32\n\n\tfunc  fgetc\t\t(filehandle)int32\n\tfunc  fputc\t\t(int32,  filehandle)int32\n\tfunc  fprintf\t(filehandle, ichar, ...)int32\n\tfunc  fputs\t\t(ichar,  filehandle)int32\n\tfunc  feof\t\t(filehandle)int32\n\tfunc  getch\t\t:int32\n\tfunc  _getch\t\t:int32\n\tfunc  kbhit\t\t:int32\n\tfunc  _mkdir\t\t(ichar)int32\n\tfunc  mkdir\t\t(ichar)int32\n\tfunc  strchr\t\t(ichar,int32)ichar\n\n\tfunc  _setmode\t(int32,int32)int32\n\n\tproc _exit\t\t(int32)\n\tproc \"exit\"\t\t(int32)\n!\tproc `exit\t\t(int32)\n\tfunc  pow\t\t(real,real)real\n\n\tfunc  `sin \t\t(real)real\n\tfunc  `cos\t\t(real)real\n\tfunc  `tan\t\t(real)real\n\tfunc  `asin\t\t(real)real\n\tfunc  `acos\t\t(real)real\n\tfunc  `atan \t\t(real)real\n\tfunc  `log\t\t(real)real\n\tfunc  `log10\t\t(real)real\n\tfunc  `exp\t\t(real)real\n\tfunc  `floor\t\t(real)real\n\tfunc  `ceil\t\t(real)real\n\n\tfunc  `llabs\t(i64)i64\n\n\tproc  qsort   \t(ref void, word64, word64, ref proc)\n\tproc  sleep\t\t(word32)\n\nend\n\nexport macro strdup=_strdup\n\nimportdll $cstdextra=\n\tfunc  __getmainargs(ref int32, ref void, ref void, int, ref void)int32\nend\n\nexport const c_eof\t\t=-1\nexport const seek_set\t= 0\nexport const seek_curr\t= 1\nexport const seek_end\t= 2\n",
(byte*)"!import clib\n!import mlib\n!\n!importlib cstd=\n!\tclang proc     sleep\t(word32)\n!end\n\nrecord termios =\n\tint32 c_iflag\n\tint32 c_oflag\n\tint32 c_cflag\n\tint32 c_lflag\n\tchar c_line\n\t[32]char c_cc\t\t\t\t!at offset 17\n\t[3]byte filler\n\tint32 c_ispeed\t\t\t\t!at offset 52\n\tint32 c_ospeed\nend\n\nimportlib dlstuff=\n\tclang function dlopen\t\t(ichar, int32)ref void\n\tclang function dlsym\t\t(ref void, ichar)ref void\n\tclang function tcgetattr\t(int32, ref termios) int32\n\tclang function tcsetattr\t(int32, int32, ref termios) int32\nend\n\n!this record is used by some apps, so these fields must be present\nexport record rsystemtime =\n\tint32 year\n\tint32 month\n\tint32 dayofweek\n\tint32 day\n\tint32 hour\n\tint32 minute\n\tint32 second\n\tint milliseconds\nend\n\nint init_flag=0\n\n\nexport proc os_init=\ninit_flag:=1\nend\n\nexport function os_execwait(ichar cmdline,int newconsole=0,ichar workdir=nil)int =\nreturn system(cmdline)\nend\n\nexport function os_execcmd(ichar cmdline, int newconsole)int =\nreturn system(cmdline)\nend\n\nexport function os_getch:int=\n\tconst ICANON  = 8x000002\n\tconst ECHO    = 8x000010\n\tconst TCSANOW = 0\n\n\ttermios old,new\n\tchar ch\n\n\ttcgetattr(0,&old)\n\tnew:=old\n\tnew.c_lflag iand:=inot ICANON\n\tnew.c_lflag iand:=inot ECHO\n\ttcsetattr(0,TCSANOW,&new)\n\n\tch:=getchar()\n\n\ttcsetattr(0,TCSANOW,&old)\n\n\treturn ch\nend\n\nexport function os_kbhit:int=\nabortprogram(\"kbhit\")\nreturn 0\nend\n\nexport proc os_flushkeys=\nabortprogram(\"flushkeys\")\nend\n\nexport function os_getconsolein:ref void=\nreturn nil\nend\n\nexport function os_getconsoleout:ref void=\nreturn nil\nend\n\nexport function os_proginstance:ref void=\nabortprogram(\"PROGINST\")\nreturn nil\nend\n\nexport function os_getdllinst(ichar name)u64=\nconst RTLD_LAZY=1\nref void h\n\nh:=dlopen(name,RTLD_LAZY)\n\nif h=nil then\n\tif strcmp(name,\"msvcrt\")=0 then\t\t\t!might be linux\n\t\th:=dlopen(\"libc.so.6\",RTLD_LAZY);\n\tfi\nfi\n\nreturn cast(h)\nend\n\nexport function os_getdllprocaddr(int hlib,ichar name)ref void=\nref void fnaddr\n\nif hlib=0 then\n\treturn nil\nfi\n\nfnaddr:=dlsym(cast(int(hlib)), name)\nreturn fnaddr\nend\n\nexport proc os_initwindows=\nend\n\nexport function os_getchx:int=\nabortprogram(\"getchx\")\nreturn 0\nend\n\nexport function os_getos=>ichar=\nif $targetbits=32 then\n\treturn \"L32\"\nelse\n\treturn \"L64\"\nfi\nend\n\nexport function os_gethostsize=>int=\nreturn $targetbits\nend\n\nexport function os_iswindows:int=\nreturn 0\nend\n\nexport function os_shellexec(ichar opc, file)int=\nabortprogram(\"SHELL EXEC\")\nreturn 0\nend\n\nexport proc  os_sleep(int a)=\nsleep(a)\nend\n\nexport function os_getstdin:filehandle =\nreturn nil\n!return fopen(\"con\",\"rb\")\nend\n\nexport function os_getstdout:filehandle =\nreturn nil\n!return fopen(\"con\",\"wb\")\nend\n\nexport function os_gethostname:ichar=\n!abortprogram(\"gethostname\")\nreturn \"\"\nend\n\nexport function os_getmpath:ichar=\n!abortprogram(\"getmpath\")\nreturn \"\"\nend\n\nexport proc os_exitprocess(int x)=\nstop\n!_exit(0)\n!ExitProcess(x)\nend\n\n!export function os_gettimestamp:dint=\n!return clock()\n!end\n!\n!export function os_gettickcount:dint=\n!return clock()\n!end\n\nexport function os_clock:int64=\nif os_iswindows() then\n\treturn clock()\nelse\n\treturn clock()/1000\nfi\nend\n\nexport function os_ticks:int64=\n\treturn clock()\nend\n\nexport function os_getclockspersec:int64=\nreturn (os_iswindows()|1000|1000'000)\nend\n\nexport proc os_setmesshandler(ref void addr)=\nabortprogram(\"SETMESSHANDLER\")\n!wndproc_callbackfn:=addr\nend\n\nexport function os_hpcounter:int64=\nreturn 1\nend\n\nexport function os_hpfrequency:int64=\nreturn 1\nend\n\nexport function os_filelastwritetime(ichar filename)int64=\nreturn 0\nend\n\nexport proc os_getsystime(ref rsystemtime tm)=\nmemset(tm,0,rsystemtime.bytes)\ntm.month:=1\t\t\t!avoid crashing the M compiler\nend\n\nexport proc os_peek=\nend\n\nexport function  os_allocexecmem(int n)ref byte=\n\tabortprogram(\"No allocexec\")\n\tnil\nend\n\n",
(byte*)"type dll0_int=ref clang function:int\ntype dll1_int=ref clang function(int)int\ntype dll2_int=ref clang function(int,int)int\ntype dll3_int=ref clang function(int,int,int)int\ntype dll4_int=ref clang function(int,int,int,int)int\ntype dll5_int=ref clang function(int,int,int,int,int)int\ntype dll6_int=ref clang function(int,int,int,int,int,int)int\ntype dll9_int=ref clang function(int,int,int,int, int,int,int,int, int)int\ntype dll10_int=ref clang function(int,int,int,int, int,int,int,int, int,int)int\ntype dll11_int=ref clang function(int,int,int,int, int,int,int,int, int,int,int)int\ntype dll12_int=ref clang function(int,int,int,int, int,int,int,int, int,int,int,int)int\ntype dll14_int=ref clang function(int,int,int,int, int,int,int,int, int,int,int,int, int,int)int\n\ntype dll0_r64=ref clang function:r64\ntype dll1_r64=ref clang function(int)r64\ntype dll2_r64=ref clang function(int,int)r64\n\ntype dll0_r64x=ref clang function:r64\ntype dll1_r64x=ref clang function(real)r64\ntype dll2_r64x=ref clang function(real,real)r64\n\ntype m_dll0_int=ref function:int\ntype m_dll1_int=ref function(int)int\ntype m_dll2_int=ref function(int,int)int\ntype m_dll3_int=ref function(int,int,int)int\ntype m_dll4_int=ref function(int,int,int,int)int\ntype m_dll5_int=ref function(int,int,int,int,int)int\ntype m_dll12_int=ref function(int,int,int,int, int,int,int,int, int,int,int,int)int\n\ntype m_dll0_r64=ref function:r64\ntype m_dll1_r64=ref function(int)r64\ntype m_dll2_r64=ref function(int,int)r64\n\n\nexport function os_calldllfunction(ref proc fnaddr,\n\t\tint retcode, nargs, ref[]i64 args, ref[]byte argcodes)word64 =\n!retcode is 'R' or 'I'\n!each argcodes element is 'R' or 'I' too\n!The x64 version can work with any combination.\n!Here, for C, only some combinations are dealt with:\n! I result, params all I (not all param counts)\n! R result, params all I (not all param counts)\n!Mixed params, for arbitrary return type, not handled (not really detected either)\n\n\tword64 a\n\treal64 x\n\tint oddstack, nextra, pushedbytes\n\n\tif retcode='I' then\n\t\treturn calldll_cint(fnaddr,args,nargs)\n\telse\n\t\treturn calldll_creal(fnaddr,args,nargs)\n\tfi\nend\t\n\nglobal function os_pushargs(ref[]word64 args, int nargs, nextra,\n\t\t\t\t\tref proc fnaddr, int isfloat)word64=\n\tword64 a\n\treal64 x\n!ABORTPROGRAM(\"PUSHARGS/C NOT READY\")\n\n\treturn os_calldllfunction(fnaddr, (isfloat|0|'I'), nargs, cast(args), nil)\n\n\n!\treturn a\nend\n\nfunction calldll_cint (ref proc fnaddr,ref[]i64 params,int nparams)i64=\nswitch nparams\nwhen 0 then\n\treturn dll0_int(fnaddr)^()\nwhen 1 then\n\treturn dll1_int(fnaddr)^(params^[1])\nwhen 2 then\n\treturn dll2_int(fnaddr)^(params^[1],params^[2])\nwhen 3 then\n\treturn dll3_int(fnaddr)^(params^[1],params^[2],params^[3])\nwhen 4 then\n\treturn dll4_int(fnaddr)^(params^[1],params^[2],params^[3],\n\t\t\tparams^[4])\nwhen 5 then\n\treturn dll5_int(fnaddr)^(params^[1],params^[2],params^[3],\n\t\t\tparams^[4], params^[5])\nwhen 6 then\n\treturn dll6_int(fnaddr)^(params^[1],params^[2],params^[3],\n\t\t\tparams^[4], params^[5],params^[6])\nwhen 9 then \n\treturn (dll9_int(fnaddr))^(params^[1],params^[2],params^[3],params^[4],\tparams^[5],params^[6],\n\t\t\t\tparams^[7],params^[8],params^[9])\nwhen 10 then \n\treturn (dll10_int(fnaddr))^(params^[1],params^[2],params^[3],params^[4],\tparams^[5],params^[6],\n\t\t\t\tparams^[7],params^[8],params^[9],params^[10])\nwhen 11 then \n\treturn (dll11_int(fnaddr))^(params^[1],params^[2],params^[3],params^[4],\tparams^[5],params^[6],\n\t\t\t\tparams^[7],params^[8],params^[9],params^[10],\tparams^[11])\n\nwhen 12 then \n\treturn (dll12_int(fnaddr))^(params^[1],params^[2],params^[3],params^[4],\tparams^[5],params^[6],\n\t\t\t\tparams^[7],params^[8],params^[9],params^[10],\tparams^[11],params^[12])\n\nwhen 14 then \n\treturn (dll14_int(fnaddr))^(params^[1],params^[2],params^[3],params^[4],\tparams^[5],params^[6],\n\t\t\t\tparams^[7],params^[8],params^[9],params^[10],\tparams^[11],params^[12],\n\t\t\t\tparams^[13],params^[14])\n\nelse\n\tcpl nparams\n\tprintln \"calldll/c/int unsupported # of params\", nparams\n\tstop 1\nendswitch\nreturn 0\nend\n\nfunction calldll_creal (ref proc fnaddr,ref[]i64 params,int nparams)i64=\nreal64 x\n\nswitch nparams\nwhen 0 then\n\treturn dll0_r64(fnaddr)^()\nwhen 1 then\n\tos_dummycall(params^[1],params^[2],params^[3],params^[4])\n\tx:=dll1_r64(fnaddr)^(params^[1])\nwhen 2 then\n\tx:=dll2_r64(fnaddr)^(params^[1],params^[2])\nelse\n\tprintln \"calldll/c/real too many params\"\n\tstop 1\nendswitch\nreturn int64@(x)\nend\n\n\nglobal proc os_dummycall(r64 a,b,c,d)=\nend\n"};
static byte mm_libsourcesc$syslibfileno[5];
static u8 *  mm_modules$headerpathx = (byte*)"";
static u8 *  mm_modules$altpathx = (byte*)"";
static u8 *  mm_modules$importpathx = (byte*)"";
static u8 *  mm_modules$subprogpath = (byte*)"";
static i64 mm_modules$dirpos;
static i64 mm_modules$issyslib;
static u8 *  mm_modules$headervars[11];
static struct mm_decls$strec *  mm_name$currstproc;
static i64 mm_name$allowmodname = (i64)0;
static i64 mm_name$noexpand;
static i64 mm_name$noassem;
static i64 mm_name$macrolevels;
static struct mm_decls$strec *  mm_name$macroparams[50];
static struct mm_decls$strec *  mm_name$macroparamsgen[50];
static struct mm_decls$unitrec *  mm_name$macroargs[50];
static i64 mm_name$nmacroparams;
static i64 mm_name$nmacroargs;
static i64 mm_parse$intabledata = (i64)0;
static i64 mm_parse$inreadprint = (i64)0;
static i64 mm_parse$inparamlist = (i64)0;
static i64 mm_parse$inrecordbody = (i64)0;
static i64 mm_parse$inimportmodule = (i64)0;
static i64 mm_parse$labelseen = (i64)0;
static u8 *  mm_parse$tabledataname = 0;
static struct mm_decls$strec *  mm_parse$procstack[10];
static i64 mm_parse$nprocstack = (i64)0;
static struct mm_decls$uflagsrec mm_parse$unionstring;
static struct mm_decls$uflagsrec mm_parse$unionpend;
static struct mm_decls$strec *  mm_parse$unionlastvar = 0;
static struct mm_decls$strec *  mm_parse$dretvar;
static i64 mm_parse$try_level = (i64)0;
static i64 mm_parse$varattribs = (i64)0;
static struct mm_decls$unitrec *  mm_parse$dollarstack[10];
static i64 mm_parse$ndollar = (i64)0;
static i64 mm_parse$insiderecord = (i64)0;
static i64 mm_parse$insidedllimport = (i64)0;
static struct mm_decls$strec *  mm_parse$forindexvars[10];
static i64 mm_parse$nforloops;
static void *  mm_parse$docfile;
static byte mm_support$bytemasks[8] = {(u8)1u,(u8)2u,(u8)4u,(u8)8u,(u8)16u,(u8)32u,(u8)64u,(u8)128u};
static u8 *  mm_tables$stdnames[30] = {
    (byte*)"void",
    (byte*)"c64",
    (byte*)"u64",
    (byte*)"i64",
    (byte*)"r32",
    (byte*)"r64",
    (byte*)"bool64",
    (byte*)"ref",
    (byte*)"rec",
    (byte*)"range",
    (byte*)"array",
    (byte*)"slice",
    (byte*)"c8",
    (byte*)"b8",
    (byte*)"i8",
    (byte*)"i16",
    (byte*)"i32",
    (byte*)"u8",
    (byte*)"u16",
    (byte*)"u32",
    (byte*)"ichar",
    (byte*)"auto",
    (byte*)"any",
    (byte*)"proc",
    (byte*)"label",
    (byte*)"type",
    (byte*)"bitfl",
    (byte*)"tuple",
    (byte*)"pend",
    (byte*)"last "
};
static byte mm_tables$stdbits[30] = {
    (u8)0u,
    (u8)64u,
    (u8)64u,
    (u8)64u,
    (u8)32u,
    (u8)64u,
    (u8)64u,
    (u8)64u,
    (u8)0u,
    (u8)128u,
    (u8)0u,
    (u8)128u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)16u,
    (u8)32u,
    (u8)8u,
    (u8)16u,
    (u8)32u,
    (u8)64u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)64u,
    (u8)8u,
    (u8)0u,
    (u8)0u,
    (u8)0u
};
static byte mm_tables$stdcat[30] = {
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)2u,
    (u8)3u,
    (u8)1u,
    (u8)1u,
    (u8)5u,
    (u8)5u,
    (u8)5u,
    (u8)5u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)1u,
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
static u8 *  mm_tables$catnames[6] = {(byte*)"voidcat",(byte*)"d64cat",(byte*)"x32cat",(byte*)"x64cat",(byte*)"shortcat",(byte*)"blockcat"};
static i64 mm_tables$trefproc;
static i64 mm_tables$treflabel;
static u8 *  mm_tables$sysfnnames[36] = {
    (byte*)"sf_init",
    (byte*)"sf_print_startfile",
    (byte*)"sf_print_startstr",
    (byte*)"sf_print_startptr",
    (byte*)"sf_print_startcon",
    (byte*)"sf_print_setfmt",
    (byte*)"sf_print_nogap",
    (byte*)"sf_print_space",
    (byte*)"sf_print_i64",
    (byte*)"sf_print_i64_nf",
    (byte*)"sf_print_u64",
    (byte*)"sf_print_r64",
    (byte*)"sf_print_r32",
    (byte*)"sf_print_str",
    (byte*)"sf_print_str_nf",
    (byte*)"sf_print_strsl",
    (byte*)"sf_print_ptr",
    (byte*)"sf_print_ptr_nf",
    (byte*)"sf_print_c8",
    (byte*)"sf_print_bool",
    (byte*)"sf_print_newline",
    (byte*)"sf_print_end",
    (byte*)"sf_read_i64",
    (byte*)"sf_read_r64",
    (byte*)"sf_read_str",
    (byte*)"sf_read_fileline",
    (byte*)"sf_read_strline",
    (byte*)"sf_read_conline",
    (byte*)"sf_get_nprocs",
    (byte*)"sf_get_procname",
    (byte*)"sf_get_procaddr",
    (byte*)"sf_gettttable",
    (byte*)"sf_getsttable",
    (byte*)"sf_getfftable",
    (byte*)"sf_power_i64",
    (byte*)"sf_unimpl"
};
static byte mm_tables$sysfnparams[36] = {
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
    (u8)0u
};
static byte mm_tables$sysfnres[36] = {
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
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u
};
static struct mm_decls$strec *  mm_tables$sysfnhandlers[36];
static i64 mm_tables$mlineno;
static u8 *  mm_tables$jtagnames[126] = {
    (byte*)"jnone",
    (byte*)"jconst",
    (byte*)"jnull",
    (byte*)"jname",
    (byte*)"jblock",
    (byte*)"jassem",
    (byte*)"jassemmacro",
    (byte*)"jassemreg",
    (byte*)"jassemxreg",
    (byte*)"jassemmem",
    (byte*)"jstrinclude",
    (byte*)"jandl",
    (byte*)"jorl",
    (byte*)"jnotl",
    (byte*)"jistruel",
    (byte*)"jmakelist",
    (byte*)"jmakerange",
    (byte*)"jmakeset",
    (byte*)"jmakedict",
    (byte*)"jmakeslice",
    (byte*)"jreturnmult",
    (byte*)"jkeyword",
    (byte*)"jkeyvalue",
    (byte*)"jassign",
    (byte*)"jassignmm",
    (byte*)"jassignms",
    (byte*)"jassignmdrem",
    (byte*)"jcopy",
    (byte*)"jcallfn",
    (byte*)"jcmp",
    (byte*)"jcmpchain",
    (byte*)"jbin",
    (byte*)"junary",
    (byte*)"jbinto",
    (byte*)"junaryto",
    (byte*)"jincr",
    (byte*)"jinrev",
    (byte*)"jinrange",
    (byte*)"jinset",
    (byte*)"jclamp",
    (byte*)"jindex",
    (byte*)"jslice",
    (byte*)"jdot",
    (byte*)"jdotindex",
    (byte*)"jdotslice",
    (byte*)"jptr",
    (byte*)"jaddrof",
    (byte*)"jaddroffirst",
    (byte*)"jconvert",
    (byte*)"jshorten",
    (byte*)"jautocast",
    (byte*)"jtypepun",
    (byte*)"jtypeconst",
    (byte*)"joperator",
    (byte*)"jupper",
    (byte*)"jbitwidth",
    (byte*)"jbytesize",
    (byte*)"jtypeof",
    (byte*)"jtypestr",
    (byte*)"jbitfield",
    (byte*)"jminvalue",
    (byte*)"jmaxvalue",
    (byte*)"jcvlineno",
    (byte*)"jcvstrlineno",
    (byte*)"jcvmodulename",
    (byte*)"jcvfilename",
    (byte*)"jcvfunction",
    (byte*)"jcvdate",
    (byte*)"jcvtime",
    (byte*)"jcvversion",
    (byte*)"jcvtypename",
    (byte*)"jcvtargetbits",
    (byte*)"jcvtargetsize",
    (byte*)"jcvtargetcode",
    (byte*)"jcvctarget",
    (byte*)"jcvwindows",
    (byte*)"jcvlinux",
    (byte*)"jcvnil",
    (byte*)"jcvpi",
    (byte*)"jcvinfinity",
    (byte*)"jcvtrue",
    (byte*)"jcvfalse",
    (byte*)"jwhenthen",
    (byte*)"jfmtitem",
    (byte*)"jnogap",
    (byte*)"jspace",
    (byte*)"jcallproc",
    (byte*)"jreturn",
    (byte*)"jsyscall",
    (byte*)"jto",
    (byte*)"jif",
    (byte*)"jforup",
    (byte*)"jfordown",
    (byte*)"jforall",
    (byte*)"jforallrev",
    (byte*)"jwhile",
    (byte*)"jrepeat",
    (byte*)"jgoto",
    (byte*)"jlabeldef",
    (byte*)"jredo",
    (byte*)"jnext",
    (byte*)"jexit",
    (byte*)"jdo",
    (byte*)"jcase",
    (byte*)"jdocase",
    (byte*)"jswitch",
    (byte*)"jdoswitch",
    (byte*)"jswap",
    (byte*)"jselect",
    (byte*)"jrecase",
    (byte*)"jprint",
    (byte*)"jprintln",
    (byte*)"jfprint",
    (byte*)"jfprintln",
    (byte*)"jsprint",
    (byte*)"jsfprint",
    (byte*)"jread",
    (byte*)"jreadln",
    (byte*)"jsread",
    (byte*)"jsreadln",
    (byte*)"jstop",
    (byte*)"jeval",
    (byte*)"jempty",
    (byte*)"jemitc",
    (byte*)"jinfinity",
    (byte*)"jdummy"
};
static byte mm_tables$jsubs[126] = {
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)3u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
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
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)3u,
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
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
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
    (u8)0u,
    (u8)0u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)2u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)2u,
    (u8)3u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)3u,
    (u8)3u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u
};
static byte mm_tables$jisexpr[126] = {
    (u8)0u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)2u,
    (u8)1u,
    (u8)2u,
    (u8)1u,
    (u8)2u,
    (u8)1u,
    (u8)3u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)1u,
    (u8)1u,
    (u8)3u,
    (u8)1u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)0u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)0u,
    (u8)0u,
    (u8)3u,
    (u8)0u,
    (u8)3u,
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
    (u8)3u,
    (u8)0u,
    (u8)3u,
    (u8)0u,
    (u8)0u,
    (u8)3u,
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
    (u8)3u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)3u
};
static u8 *  mm_tables$bitfieldnames[8] = {(byte*)"bf_msb",(byte*)"bf_lsb",(byte*)"bf_msbit",(byte*)"bf_lsbit",(byte*)"bf_msw",(byte*)"bf_lsw",(byte*)"bf_odd",(byte*)"bf_even"};
static u8 *  mm_tables$optypenames[4] = {(byte*)"no_op",(byte*)"bin_op",(byte*)"mon_op",(byte*)"prop_op"};
static u8 *  mm_tables$symbolnames[172] = {
    (byte*)"errorsym",
    (byte*)".",
    (byte*)"lexdotsym",
    (byte*)"&.",
    (byte*)",",
    (byte*)";",
    (byte*)":",
    (byte*)"::",
    (byte*)":=",
    (byte*)"::=",
    (byte*)"=>",
    (byte*)"->",
    (byte*)"(",
    (byte*)")",
    (byte*)"[",
    (byte*)"]",
    (byte*)"{",
    (byte*)"}",
    (byte*)"^",
    (byte*)"|",
    (byte*)"||",
    (byte*)"@",
    (byte*)"@@",
    (byte*)"?",
    (byte*)"&",
    (byte*)"&&",
    (byte*)"~",
    (byte*)"..",
    (byte*)"...",
    (byte*)"#",
    (byte*)"+",
    (byte*)"-",
    (byte*)"*",
    (byte*)"/",
    (byte*)"%",
    (byte*)"rem",
    (byte*)"rem",
    (byte*)"iand",
    (byte*)"ior",
    (byte*)"ixor",
    (byte*)"<<",
    (byte*)">>",
    (byte*)"min",
    (byte*)"max",
    (byte*)"and",
    (byte*)"or",
    (byte*)"xor",
    (byte*)"=",
    (byte*)"cmp",
    (byte*)"**",
    (byte*)"==",
    (byte*)"in",
    (byte*)"notin",
    (byte*)"inrev",
    (byte*)"$neg",
    (byte*)"not",
    (byte*)"istrue",
    (byte*)"inot",
    (byte*)"abs",
    (byte*)"sign",
    (byte*)"sqrt",
    (byte*)"sqr",
    (byte*)"propsym",
    (byte*)"mathsopsym",
    (byte*)"maths2opsym",
    (byte*)"bitfieldsym",
    (byte*)"eolsym",
    (byte*)"eofsym",
    (byte*)"rawxnamesym",
    (byte*)"docstringsym",
    (byte*)"incrsym",
    (byte*)"intconstsym",
    (byte*)"decimalconstsym",
    (byte*)"realconstsym",
    (byte*)"charconstsym",
    (byte*)"wcharconstsym",
    (byte*)"stringconstsym",
    (byte*)"astringconstsym",
    (byte*)"wstringconstsym",
    (byte*)"unitnamesym",
    (byte*)"namesym",
    (byte*)"ksourcedirsym",
    (byte*)"kstrincludesym",
    (byte*)"regsym",
    (byte*)"xregsym",
    (byte*)"fregsym",
    (byte*)"mregsym",
    (byte*)"jmpccsym",
    (byte*)"setccsym",
    (byte*)"movccsym",
    (byte*)"segnamesym",
    (byte*)"asmopcodesym",
    (byte*)"stdtypesym",
    (byte*)"ktypeofsym",
    (byte*)"ksubrangesym",
    (byte*)"koutsym",
    (byte*)"kicharsym",
    (byte*)"kifsym",
    (byte*)"kthensym",
    (byte*)"kelsifsym",
    (byte*)"kelsesym",
    (byte*)"kelsecasesym",
    (byte*)"kelseswitchsym",
    (byte*)"kelseselectsym",
    (byte*)"kendsym",
    (byte*)"kunlesssym",
    (byte*)"kcasesym",
    (byte*)"kdocasesym",
    (byte*)"krecasesym",
    (byte*)"kwhensym",
    (byte*)"kforsym",
    (byte*)"ktosym",
    (byte*)"kbysym",
    (byte*)"kdosym",
    (byte*)"kwhilesym",
    (byte*)"krepeatsym",
    (byte*)"kuntilsym",
    (byte*)"kreturnsym",
    (byte*)"kstopsym",
    (byte*)"kloopsym",
    (byte*)"kstepsym",
    (byte*)"kgotosym",
    (byte*)"kswitchsym",
    (byte*)"kdoswitchsym",
    (byte*)"kprintsym",
    (byte*)"ksprintsym",
    (byte*)"kreadsym",
    (byte*)"ksreadsym",
    (byte*)"ksreadlnsym",
    (byte*)"kprocsym",
    (byte*)"kfunctionsym",
    (byte*)"klabelsym",
    (byte*)"krecordsym",
    (byte*)"kstructsym",
    (byte*)"kunionsym",
    (byte*)"kimportmodulesym",
    (byte*)"ktypesym",
    (byte*)"ktypealiassym",
    (byte*)"kextendtypesym",
    (byte*)"krefsym",
    (byte*)"kmutsym",
    (byte*)"kletsym",
    (byte*)"kslicesym",
    (byte*)"karraysym",
    (byte*)"kdictsym",
    (byte*)"kmacrosym",
    (byte*)"kexpandsym",
    (byte*)"koperatorsym",
    (byte*)"kconstsym",
    (byte*)"knewsym",
    (byte*)"kclearsym",
    (byte*)"kclasssym",
    (byte*)"kheadersym",
    (byte*)"kheadervarsym",
    (byte*)"kfflangsym",
    (byte*)"kglobalsym",
    (byte*)"kstaticsym",
    (byte*)"kcastsym",
    (byte*)"compilervarsym",
    (byte*)"dollarsym",
    (byte*)"kevalsym",
    (byte*)"ktabledatasym",
    (byte*)"kstacksym",
    (byte*)"kclampsym",
    (byte*)"kswapsym",
    (byte*)"kerrorsym",
    (byte*)"kassemsym",
    (byte*)"ksyscallsym",
    (byte*)"kemitcsym",
    (byte*)"kemptysym",
    (byte*)"kcopysym",
    (byte*)"kdummysym"
};
static byte mm_tables$symboloptypes[172] = {
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
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
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
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
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)3u,
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
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u
};
static byte mm_tables$symbolgenops[172] = {
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
    (u8)15u,
    (u8)16u,
    (u8)24u,
    (u8)25u,
    (u8)0u,
    (u8)17u,
    (u8)0u,
    (u8)51u,
    (u8)23u,
    (u8)13u,
    (u8)14u,
    (u8)0u,
    (u8)29u,
    (u8)32u,
    (u8)33u,
    (u8)31u,
    (u8)30u,
    (u8)49u,
    (u8)35u,
    (u8)34u,
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
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u
};
static byte mm_tables$symbolgentoops[172] = {
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
    (u8)59u,
    (u8)60u,
    (u8)61u,
    (u8)62u,
    (u8)63u,
    (u8)64u,
    (u8)0u,
    (u8)65u,
    (u8)66u,
    (u8)67u,
    (u8)68u,
    (u8)69u,
    (u8)70u,
    (u8)71u,
    (u8)72u,
    (u8)73u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)79u,
    (u8)80u,
    (u8)78u,
    (u8)77u,
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
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u
};
static byte mm_tables$symbolopprios[172] = {
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
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
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)5u,
    (u8)0u,
    (u8)0u,
    (u8)4u,
    (u8)4u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)3u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)3u,
    (u8)3u,
    (u8)4u,
    (u8)4u,
    (u8)7u,
    (u8)8u,
    (u8)8u,
    (u8)6u,
    (u8)6u,
    (u8)2u,
    (u8)6u,
    (u8)6u,
    (u8)6u,
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
    (u8)0u
};
static byte mm_tables$exprstarter[172] = {
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
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
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
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
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
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
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)0u
};
static u8 *  mm_tables$pclnames[105] = {
    (byte*)"kzero",
    (byte*)"kadd",
    (byte*)"ksub",
    (byte*)"kmul",
    (byte*)"kdiv",
    (byte*)"kidiv",
    (byte*)"kirem",
    (byte*)"kidivrem",
    (byte*)"kiand",
    (byte*)"kior",
    (byte*)"kixor",
    (byte*)"kshl",
    (byte*)"kshr",
    (byte*)"kin",
    (byte*)"knotin",
    (byte*)"kmin",
    (byte*)"kmax",
    (byte*)"keq",
    (byte*)"kne",
    (byte*)"klt",
    (byte*)"kle",
    (byte*)"kge",
    (byte*)"kgt",
    (byte*)"ksame",
    (byte*)"kandl",
    (byte*)"korl",
    (byte*)"kaddrefoff",
    (byte*)"ksubrefoff",
    (byte*)"ksubref",
    (byte*)"kneg",
    (byte*)"kabs",
    (byte*)"kinot",
    (byte*)"knotl",
    (byte*)"kistruel",
    (byte*)"ksqr",
    (byte*)"ksqrt",
    (byte*)"ksin",
    (byte*)"kcos",
    (byte*)"ktan",
    (byte*)"kasin",
    (byte*)"kacos",
    (byte*)"katan",
    (byte*)"kln",
    (byte*)"klog",
    (byte*)"kexp",
    (byte*)"kround",
    (byte*)"kfloor",
    (byte*)"kceil",
    (byte*)"kfract",
    (byte*)"ksign",
    (byte*)"katan2",
    (byte*)"kpower",
    (byte*)"kfmod",
    (byte*)"kincr",
    (byte*)"kdecr",
    (byte*)"kincrload",
    (byte*)"kdecrload",
    (byte*)"kloadincr",
    (byte*)"kloaddecr",
    (byte*)"kaddto",
    (byte*)"ksubto",
    (byte*)"kmulto",
    (byte*)"kdivto",
    (byte*)"kidivto",
    (byte*)"kiremto",
    (byte*)"kiandto",
    (byte*)"kiorto",
    (byte*)"kixorto",
    (byte*)"kshlto",
    (byte*)"kshrto",
    (byte*)"kminto",
    (byte*)"kmaxto",
    (byte*)"kandlto",
    (byte*)"korlto",
    (byte*)"kaddrefoffto",
    (byte*)"ksubrefoffto",
    (byte*)"knegto",
    (byte*)"kabsto",
    (byte*)"kinotto",
    (byte*)"knotlto",
    (byte*)"kistruelto",
    (byte*)"ktypepun",
    (byte*)"ksoftconv",
    (byte*)"kfloat",
    (byte*)"kfix",
    (byte*)"ktruncate",
    (byte*)"kfwiden",
    (byte*)"kfnarrow",
    (byte*)"klen",
    (byte*)"klwb",
    (byte*)"kupb",
    (byte*)"kbounds",
    (byte*)"klenstr",
    (byte*)"kbitwidth",
    (byte*)"kbytesize",
    (byte*)"kminvalue",
    (byte*)"kmaxvalue",
    (byte*)"ktypestr",
    (byte*)"kerror",
    (byte*)"kharderror",
    (byte*)"karraytoslice",
    (byte*)"kichartoslice",
    (byte*)"ksofttruncshort",
    (byte*)"kcharaxtoichar",
    (byte*)"ksliceptr"
};
static u8 *  mm_tables$ccopnames[105] = {
    0,
    (byte*)"+",
    (byte*)"-",
    (byte*)"*",
    (byte*)"/",
    (byte*)"/",
    (byte*)"%",
    0,
    (byte*)"&",
    (byte*)"|",
    (byte*)"^",
    (byte*)"<<",
    (byte*)">>",
    0,
    0,
    0,
    0,
    (byte*)"==",
    (byte*)"!=",
    (byte*)"<",
    (byte*)"<=",
    (byte*)">=",
    (byte*)">",
    0,
    (byte*)"&&",
    (byte*)"||",
    (byte*)"+",
    (byte*)"-",
    (byte*)"-",
    (byte*)"-",
    0,
    (byte*)"~",
    (byte*)"!",
    (byte*)"!!",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    (byte*)"+=",
    (byte*)"-=",
    (byte*)"*=",
    (byte*)"/=",
    (byte*)"/=",
    (byte*)"%=",
    (byte*)"&=",
    (byte*)"|=",
    (byte*)"^=",
    (byte*)"<<=",
    (byte*)">>=",
    0,
    0,
    (byte*)"&&=",
    (byte*)"||=",
    (byte*)"+=",
    (byte*)"-=",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0
};
static byte mm_tables$complexops[15] = {
    (u8)51u,
    (u8)50u,
    (u8)36u,
    (u8)37u,
    (u8)38u,
    (u8)39u,
    (u8)40u,
    (u8)42u,
    (u8)43u,
    (u8)44u,
    (u8)45u,
    (u8)46u,
    (u8)47u,
    (u8)48u,
    (u8)52u
};
static byte mm_tables$complexopset[105];
static u8 *  mm_tables$sourcedirnames[2] = {(byte*)"includedir",(byte*)"binincludedir"};
static u8 *  mm_tables$headerdirnames[16] = {
    (byte*)"hdr_module",
    (byte*)"hdr_import",
    (byte*)"hdr_subprog",
    (byte*)"hdr_sysmodule",
    (byte*)"hdr_sysimport",
    (byte*)"hdr_syssubprog",
    (byte*)"hdr_minclude",
    (byte*)"hdr_altpath",
    (byte*)"hdr_importpath",
    (byte*)"hdr_linkdll",
    (byte*)"hdr_linklib",
    (byte*)"hdr_exportmodule",
    (byte*)"hdr_file",
    (byte*)"hdr_runexe",
    (byte*)"hdr_setvar",
    (byte*)"hdr_showvar"
};
static u8 *  mm_tables$headervarnames[11] = {
    (byte*)"hv_devpath",
    (byte*)"hv_mmpath",
    (byte*)"hv_hdrpath",
    (byte*)"hv_ctarget",
    (byte*)"hv_windows",
    (byte*)"hv_linux",
    (byte*)"hv_optim",
    (byte*)"hv_mainmodule",
    (byte*)"hv_a",
    (byte*)"hv_b",
    (byte*)"hv_c"
};
static u8 *  mm_tables$fflangnames[5] = {(byte*)"noff",(byte*)"windowsff",(byte*)"clangff",(byte*)"mlangff",(byte*)"callbackff"};
static u8 *  mm_tables$scopenames[4] = {(byte*)"Local",(byte*)"Global",(byte*)"Program",(byte*)"Export"};
static u8 *  mm_tables$parammodenames[4] = {(byte*)"Var ",(byte*)"In ",(byte*)"Out ",(byte*)"Opt "};
static u8 *  mm_tables$namenames[21] = {
    (byte*)"nullid",
    (byte*)"programid",
    (byte*)"subprogid",
    (byte*)"moduleid",
    (byte*)"dllmoduleid",
    (byte*)"typeid",
    (byte*)"procid",
    (byte*)"dllprocid",
    (byte*)"dllvarid",
    (byte*)"genprocid",
    (byte*)"constid",
    (byte*)"staticid",
    (byte*)"frameid",
    (byte*)"paramid",
    (byte*)"fieldid",
    (byte*)"genfieldid",
    (byte*)"enumid",
    (byte*)"labelid",
    (byte*)"macroid",
    (byte*)"macroparamid",
    (byte*)"linkid"
};
static u8 *  mm_tables$stnames[262] = {
    (byte*)"if",
    (byte*)"then",
    (byte*)"elsif",
    (byte*)"else",
    (byte*)"elsecase",
    (byte*)"elseswitch",
    (byte*)"case",
    (byte*)"docase",
    (byte*)"recase",
    (byte*)"when",
    (byte*)"for",
    (byte*)"forall",
    (byte*)"to",
    (byte*)"downto",
    (byte*)"by",
    (byte*)"do",
    (byte*)"end",
    (byte*)"while",
    (byte*)"repeat",
    (byte*)"until",
    (byte*)"always",
    (byte*)"return",
    (byte*)"stop",
    (byte*)"redo",
    (byte*)"loop",
    (byte*)"next",
    (byte*)"exit",
    (byte*)"$step",
    (byte*)"goto",
    (byte*)"go",
    (byte*)"switch",
    (byte*)"doswitch",
    (byte*)"tabledata",
    (byte*)"enumdata",
    (byte*)"clamp",
    (byte*)"eval",
    (byte*)"evalloadref",
    (byte*)"evalgetref",
    (byte*)"evalget",
    (byte*)"evalload",
    (byte*)"print",
    (byte*)"println",
    (byte*)"fprint",
    (byte*)"fprintln",
    (byte*)"sprint",
    (byte*)"sfprint",
    (byte*)"cp",
    (byte*)"cpl",
    (byte*)"read",
    (byte*)"readln",
    (byte*)"cast",
    (byte*)"function",
    (byte*)"func",
    (byte*)"procedure",
    (byte*)"proc",
    (byte*)"fun",
    (byte*)"sub",
    (byte*)"threadedproc",
    (byte*)"type",
    (byte*)"class",
    (byte*)"record",
    (byte*)"struct",
    (byte*)"union",
    (byte*)"ref",
    (byte*)"pointer",
    (byte*)"returning",
    (byte*)"mut",
    (byte*)"var",
    (byte*)"let",
    (byte*)"include",
    (byte*)"strinclude",
    (byte*)"bininclude",
    (byte*)"emitc",
    (byte*)"macro",
    (byte*)"assem",
    (byte*)"asm",
    (byte*)"static",
    (byte*)"const",
    (byte*)"$get_nprocs",
    (byte*)"$getnprocs",
    (byte*)"$get_procname",
    (byte*)"$getprocname",
    (byte*)"$get_procaddr",
    (byte*)"$getprocaddr",
    (byte*)"$gettttable",
    (byte*)"$getsttable",
    (byte*)"$getfftable",
    (byte*)"importdll",
    (byte*)"importlib",
    (byte*)"unless",
    (byte*)"out",
    (byte*)"global",
    (byte*)"export",
    (byte*)"clang",
    (byte*)"mlang",
    (byte*)"windows",
    (byte*)"callback",
    (byte*)"swap",
    (byte*)"void",
    (byte*)"int",
    (byte*)"word",
    (byte*)"real",
    (byte*)"ichar",
    (byte*)"int8",
    (byte*)"int16",
    (byte*)"int32",
    (byte*)"int64",
    (byte*)"i8",
    (byte*)"i16",
    (byte*)"i32",
    (byte*)"i64",
    (byte*)"real32",
    (byte*)"real64",
    (byte*)"r32",
    (byte*)"r64",
    (byte*)"float32",
    (byte*)"float64",
    (byte*)"byte",
    (byte*)"u8",
    (byte*)"u16",
    (byte*)"u32",
    (byte*)"u64",
    (byte*)"word8",
    (byte*)"word16",
    (byte*)"word32",
    (byte*)"word64",
    (byte*)"char",
    (byte*)"char64",
    (byte*)"bool64",
    (byte*)"bool",
    (byte*)"bool8",
    (byte*)"range",
    (byte*)"auto",
    (byte*)"label",
    (byte*)"slice",
    (byte*)"array",
    (byte*)"typeof",
    (byte*)"million",
    (byte*)"billion",
    (byte*)"thousand",
    (byte*)"$lineno",
    (byte*)"$strlineno",
    (byte*)"$filename",
    (byte*)"$modulename",
    (byte*)"$function",
    (byte*)"$date",
    (byte*)"$time",
    (byte*)"$version",
    (byte*)"$typename",
    (byte*)"$targetbits",
    (byte*)"$targetsize",
    (byte*)"$targetcode",
    (byte*)"$ctarget",
    (byte*)"$windows",
    (byte*)"$linux",
    (byte*)"nil",
    (byte*)"pi",
    (byte*)"true",
    (byte*)"false",
    (byte*)"infinity",
    (byte*)"$",
    (byte*)"and",
    (byte*)"or",
    (byte*)"xor",
    (byte*)"iand",
    (byte*)"ior",
    (byte*)"ixor",
    (byte*)"in",
    (byte*)"notin",
    (byte*)"inrev",
    (byte*)"rem",
    (byte*)"divrem",
    (byte*)"min",
    (byte*)"max",
    (byte*)"not",
    (byte*)"inot",
    (byte*)"istrue",
    (byte*)"abs",
    (byte*)"$neg",
    (byte*)"sqr",
    (byte*)"sqrt",
    (byte*)"sign",
    (byte*)"sin",
    (byte*)"cos",
    (byte*)"tan",
    (byte*)"asin",
    (byte*)"acos",
    (byte*)"atan",
    (byte*)"ln",
    (byte*)"log",
    (byte*)"exp",
    (byte*)"round",
    (byte*)"floor",
    (byte*)"ceil",
    (byte*)"fract",
    (byte*)"atan2",
    (byte*)"fmod",
    (byte*)"sliceptr",
    (byte*)"len",
    (byte*)"lwb",
    (byte*)"upb",
    (byte*)"bounds",
    (byte*)"bitwidth",
    (byte*)"bytes",
    (byte*)"minvalue",
    (byte*)"maxvalue",
    (byte*)"typestr",
    (byte*)"msb",
    (byte*)"lsb",
    (byte*)"msbit",
    (byte*)"lsbit",
    (byte*)"msw",
    (byte*)"lsw",
    (byte*)"odd",
    (byte*)"even",
    (byte*)"endif",
    (byte*)"fi",
    (byte*)"endcase",
    (byte*)"esac",
    (byte*)"enddocase",
    (byte*)"endswitch",
    (byte*)"enddoswitch",
    (byte*)"endfor",
    (byte*)"od",
    (byte*)"endproc",
    (byte*)"endfunction",
    (byte*)"endwhile",
    (byte*)"endto",
    (byte*)"enddo",
    (byte*)"endrecord",
    (byte*)"endassem",
    (byte*)"$caligned",
    (byte*)"empty",
    (byte*)"clear",
    (byte*)"copy",
    (byte*)"module",
    (byte*)"sysmodule",
    (byte*)"import",
    (byte*)"sysimport",
    (byte*)"minclude",
    (byte*)"subprog",
    (byte*)"syssubprog",
    (byte*)"altpath",
    (byte*)"importpath",
    (byte*)"linkdll",
    (byte*)"linklib",
    (byte*)"exportmodule",
    (byte*)"runexe",
    (byte*)"setvar",
    (byte*)"showvar",
    (byte*)"$devpath",
    (byte*)"$mmpath",
    (byte*)"$hdrpath",
    (byte*)"$$ctarget",
    (byte*)"$$windows",
    (byte*)"$$linux",
    (byte*)"$optim",
    (byte*)"$mainmodule",
    (byte*)"$a",
    (byte*)"$b",
    (byte*)"$c",
    (byte*)"$$dummy"
};
static i64 mm_tables$stsymbols[262] = {
    (i64)98,
    (i64)99,
    (i64)100,
    (i64)101,
    (i64)102,
    (i64)103,
    (i64)107,
    (i64)108,
    (i64)109,
    (i64)110,
    (i64)111,
    (i64)111,
    (i64)112,
    (i64)112,
    (i64)113,
    (i64)114,
    (i64)105,
    (i64)115,
    (i64)116,
    (i64)117,
    (i64)117,
    (i64)118,
    (i64)119,
    (i64)120,
    (i64)120,
    (i64)120,
    (i64)120,
    (i64)121,
    (i64)122,
    (i64)122,
    (i64)123,
    (i64)124,
    (i64)162,
    (i64)162,
    (i64)164,
    (i64)161,
    (i64)161,
    (i64)161,
    (i64)161,
    (i64)161,
    (i64)125,
    (i64)125,
    (i64)125,
    (i64)125,
    (i64)126,
    (i64)126,
    (i64)125,
    (i64)125,
    (i64)127,
    (i64)127,
    (i64)158,
    (i64)131,
    (i64)131,
    (i64)130,
    (i64)130,
    (i64)131,
    (i64)130,
    (i64)130,
    (i64)137,
    (i64)152,
    (i64)133,
    (i64)134,
    (i64)135,
    (i64)140,
    (i64)140,
    (i64)11,
    (i64)141,
    (i64)141,
    (i64)142,
    (i64)82,
    (i64)83,
    (i64)82,
    (i64)169,
    (i64)146,
    (i64)167,
    (i64)167,
    (i64)157,
    (i64)149,
    (i64)168,
    (i64)168,
    (i64)168,
    (i64)168,
    (i64)168,
    (i64)168,
    (i64)168,
    (i64)168,
    (i64)168,
    (i64)136,
    (i64)136,
    (i64)106,
    (i64)96,
    (i64)156,
    (i64)156,
    (i64)155,
    (i64)155,
    (i64)155,
    (i64)155,
    (i64)165,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)97,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)93,
    (i64)143,
    (i64)144,
    (i64)94,
    (i64)80,
    (i64)80,
    (i64)80,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)159,
    (i64)160,
    (i64)45,
    (i64)46,
    (i64)47,
    (i64)38,
    (i64)39,
    (i64)40,
    (i64)52,
    (i64)53,
    (i64)54,
    (i64)36,
    (i64)37,
    (i64)43,
    (i64)44,
    (i64)56,
    (i64)58,
    (i64)57,
    (i64)59,
    (i64)55,
    (i64)62,
    (i64)61,
    (i64)60,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)65,
    (i64)65,
    (i64)63,
    (i64)63,
    (i64)63,
    (i64)63,
    (i64)63,
    (i64)63,
    (i64)63,
    (i64)63,
    (i64)63,
    (i64)63,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)105,
    (i64)105,
    (i64)105,
    (i64)105,
    (i64)105,
    (i64)105,
    (i64)105,
    (i64)105,
    (i64)105,
    (i64)105,
    (i64)105,
    (i64)105,
    (i64)105,
    (i64)105,
    (i64)105,
    (i64)105,
    (i64)22,
    (i64)170,
    (i64)170,
    (i64)171,
    (i64)153,
    (i64)153,
    (i64)153,
    (i64)153,
    (i64)153,
    (i64)153,
    (i64)153,
    (i64)153,
    (i64)153,
    (i64)153,
    (i64)153,
    (i64)153,
    (i64)153,
    (i64)153,
    (i64)153,
    (i64)154,
    (i64)154,
    (i64)154,
    (i64)154,
    (i64)154,
    (i64)154,
    (i64)154,
    (i64)154,
    (i64)154,
    (i64)154,
    (i64)154,
    (i64)0
};
static i64 mm_tables$stsubcodes[262] = {
    (i64)90,
    (i64)0,
    (i64)90,
    (i64)0,
    (i64)103,
    (i64)105,
    (i64)103,
    (i64)104,
    (i64)109,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)0,
    (i64)0,
    (i64)99,
    (i64)99,
    (i64)100,
    (i64)101,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)105,
    (i64)106,
    (i64)0,
    (i64)1,
    (i64)0,
    (i64)0,
    (i64)2,
    (i64)3,
    (i64)1,
    (i64)0,
    (i64)110,
    (i64)111,
    (i64)112,
    (i64)113,
    (i64)114,
    (i64)115,
    (i64)110,
    (i64)111,
    (i64)116,
    (i64)117,
    (i64)48,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)1,
    (i64)2,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)0,
    (i64)2,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)29,
    (i64)29,
    (i64)30,
    (i64)30,
    (i64)31,
    (i64)31,
    (i64)32,
    (i64)33,
    (i64)34,
    (i64)68,
    (i64)76,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)3,
    (i64)2,
    (i64)3,
    (i64)1,
    (i64)4,
    (i64)0,
    (i64)0,
    (i64)3,
    (i64)2,
    (i64)5,
    (i64)0,
    (i64)14,
    (i64)15,
    (i64)16,
    (i64)3,
    (i64)14,
    (i64)15,
    (i64)16,
    (i64)3,
    (i64)4,
    (i64)5,
    (i64)4,
    (i64)5,
    (i64)4,
    (i64)5,
    (i64)17,
    (i64)17,
    (i64)18,
    (i64)19,
    (i64)2,
    (i64)17,
    (i64)18,
    (i64)19,
    (i64)2,
    (i64)12,
    (i64)1,
    (i64)6,
    (i64)6,
    (i64)13,
    (i64)9,
    (i64)21,
    (i64)24,
    (i64)11,
    (i64)0,
    (i64)0,
    (i64)2,
    (i64)3,
    (i64)1,
    (i64)62,
    (i64)63,
    (i64)65,
    (i64)64,
    (i64)66,
    (i64)67,
    (i64)68,
    (i64)69,
    (i64)70,
    (i64)71,
    (i64)72,
    (i64)73,
    (i64)74,
    (i64)75,
    (i64)76,
    (i64)77,
    (i64)78,
    (i64)80,
    (i64)81,
    (i64)79,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)13,
    (i64)14,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)30,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)36,
    (i64)37,
    (i64)38,
    (i64)39,
    (i64)40,
    (i64)41,
    (i64)42,
    (i64)43,
    (i64)44,
    (i64)45,
    (i64)46,
    (i64)47,
    (i64)48,
    (i64)50,
    (i64)52,
    (i64)104,
    (i64)88,
    (i64)89,
    (i64)90,
    (i64)91,
    (i64)93,
    (i64)94,
    (i64)95,
    (i64)96,
    (i64)97,
    (i64)1,
    (i64)2,
    (i64)3,
    (i64)4,
    (i64)5,
    (i64)6,
    (i64)7,
    (i64)8,
    (i64)98,
    (i64)98,
    (i64)107,
    (i64)107,
    (i64)108,
    (i64)123,
    (i64)124,
    (i64)111,
    (i64)114,
    (i64)130,
    (i64)131,
    (i64)115,
    (i64)112,
    (i64)114,
    (i64)133,
    (i64)167,
    (i64)1,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)4,
    (i64)2,
    (i64)5,
    (i64)7,
    (i64)3,
    (i64)6,
    (i64)8,
    (i64)9,
    (i64)10,
    (i64)11,
    (i64)12,
    (i64)14,
    (i64)15,
    (i64)16,
    (i64)1,
    (i64)2,
    (i64)3,
    (i64)4,
    (i64)5,
    (i64)6,
    (i64)7,
    (i64)8,
    (i64)9,
    (i64)10,
    (i64)11,
    (i64)0
};
static i64 mm_tables$d_typestarterset[9] = {(i64)93,(i64)15,(i64)140,(i64)133,(i64)97,(i64)94,(i64)143,(i64)145,(i64)144};
static byte mm_tables$intresultlist[8] = {(u8)13u,(u8)14u,(u8)89u,(u8)90u,(u8)88u,(u8)92u,(u8)93u,(u8)94u};
static i16 mm_tables$softconvtable[5][5] = {
    {(i16)82,(i16)82,(i16)82,(i16)83,(i16)83},
    {(i16)82,(i16)82,(i16)82,(i16)83,(i16)83},
    {(i16)82,(i16)82,(i16)82,(i16)83,(i16)83},
    {(i16)84,(i16)84,(i16)84,(i16)82,(i16)86},
    {(i16)84,(i16)84,(i16)84,(i16)87,(i16)82}
};
static byte mm_tables$intresult[105];
static byte mm_tables$endsexpr[172];
static byte mm_tables$exprendsymbols[12] = {
    (u8)14u,
    (u8)16u,
    (u8)99u,
    (u8)100u,
    (u8)101u,
    (u8)117u,
    (u8)114u,
    (u8)105u,
    (u8)5u,
    (u8)20u,
    (u8)6u,
    (u8)112u
};
static byte mm_tables$isbooltag[126];
static byte mm_tables$ismemtag[126];
static i64 mm_type$countedfields;
static i64 mm_type$inassem;
static i64 mm_type$inidata;
static i64 mm_type$deb;
static i64 mm_winc$cmdskip;
static byte mm_winc$fshortnames;
static u8 *  mc_decls$valtypenames[9] = {(byte*)"no_val",(byte*)"intimm_val",(byte*)"realimm_val",(byte*)"realmem_val",(byte*)"stringimm_val",(byte*)"comment_val",(byte*)"def_val",(byte*)"label_val",(byte*)"name_val"};
static u8 *  mc_decls$opndnames[5] = {(byte*)"a_none",(byte*)"a_reg",(byte*)"a_xreg",(byte*)"a_imm",(byte*)"a_mem"};
static u8 *  mc_decls$mclnames[150] = {
    (byte*)"m_procstart",
    (byte*)"m_procend",
    (byte*)"m_programend",
    (byte*)"m_comment",
    (byte*)"m_blank",
    (byte*)"m_deleted",
    (byte*)"m_labelname",
    (byte*)"m_define",
    (byte*)"m_definereg",
    (byte*)"m_evalx",
    (byte*)"m_labelx",
    (byte*)"m_nop",
    (byte*)"m_mov",
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
    (byte*)"m_leave",
    (byte*)"m_retn",
    (byte*)"m_jmp",
    (byte*)"m_jmpcc",
    (byte*)"m_xchg",
    (byte*)"m_add",
    (byte*)"m_sub",
    (byte*)"m_adc",
    (byte*)"m_sbb",
    (byte*)"m_imul",
    (byte*)"m_mul",
    (byte*)"m_imul2",
    (byte*)"m_imul3",
    (byte*)"m_idiv",
    (byte*)"m_div",
    (byte*)"m_andx",
    (byte*)"m_orx",
    (byte*)"m_xorx",
    (byte*)"m_test",
    (byte*)"m_cmp",
    (byte*)"m_shl",
    (byte*)"m_sar",
    (byte*)"m_shr",
    (byte*)"m_rol",
    (byte*)"m_ror",
    (byte*)"m_rcl",
    (byte*)"m_rcr",
    (byte*)"m_neg",
    (byte*)"m_notx",
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
    (byte*)"m_ddoffset",
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
    (byte*)"m_cpuid",
    (byte*)"m_halt"
};
static byte mc_decls$mclnopnds[150] = {
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
    (u8)1u,
    (u8)0u,
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
    (u8)0u,
    (u8)1u,
    (u8)1u,
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
    (u8)0u,
    (u8)0u
};
static byte mc_decls$mclcodes[150] = {
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
    (u8)232u,
    (u8)195u,
    (u8)201u,
    (u8)0u,
    (u8)233u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)5u,
    (u8)2u,
    (u8)3u,
    (u8)5u,
    (u8)4u,
    (u8)0u,
    (u8)0u,
    (u8)7u,
    (u8)6u,
    (u8)4u,
    (u8)1u,
    (u8)6u,
    (u8)0u,
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
    (u8)0u,
    (u8)244u
};
static u8 *  mc_decls$regnames[37] = {
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
    (byte*)"r19",
    (byte*)"xr0",
    (byte*)"xr1",
    (byte*)"xr2",
    (byte*)"xr3",
    (byte*)"xr4",
    (byte*)"xr5",
    (byte*)"xr6",
    (byte*)"xr7",
    (byte*)"xr8",
    (byte*)"xr9",
    (byte*)"xr10",
    (byte*)"xr11",
    (byte*)"xr12",
    (byte*)"xr13",
    (byte*)"xr14",
    (byte*)"xr15"
};
static byte mc_decls$regcodes[37] = {
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
    (u8)6u,
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
    (u8)15u
};
static u8 *  mc_decls$condnames[20] = {
    (byte*)"ov",
    (byte*)"nov",
    (byte*)"ltu",
    (byte*)"geu",
    (byte*)"eq",
    (byte*)"ne",
    (byte*)"leu",
    (byte*)"gtu",
    (byte*)"s",
    (byte*)"ns",
    (byte*)"p",
    (byte*)"np",
    (byte*)"lt",
    (byte*)"ge",
    (byte*)"le",
    (byte*)"gt",
    (byte*)"flt",
    (byte*)"fge",
    (byte*)"fle",
    (byte*)"fgt"
};
static u8 *  mc_decls$asmcondnames[20] = {
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
    (byte*)"g",
    (byte*)"b",
    (byte*)"ae",
    (byte*)"be",
    (byte*)"a"
};
static i64 mc_decls$asmrevcond[20] = {
    (i64)1,
    (i64)0,
    (i64)3,
    (i64)2,
    (i64)5,
    (i64)4,
    (i64)7,
    (i64)6,
    (i64)9,
    (i64)8,
    (i64)11,
    (i64)10,
    (i64)13,
    (i64)12,
    (i64)15,
    (i64)14,
    (i64)17,
    (i64)16,
    (i64)19,
    (i64)18
};
static u8 *  mc_decls$dregnames[136] = {
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
static byte mc_decls$regsizes[136] = {
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
static byte mc_decls$regindices[136] = {
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
static u8 *  mc_decls$xmmregnames[16] = {
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
static u8 *  mc_decls$fregnames[8] = {(byte*)"st0",(byte*)"st1",(byte*)"st2",(byte*)"st3",(byte*)"st4",(byte*)"st5",(byte*)"st6",(byte*)"st7"};
static u8 *  mc_decls$mregnames[8] = {(byte*)"mmx0",(byte*)"mmx1",(byte*)"mmx2",(byte*)"mmx3",(byte*)"mmx4",(byte*)"mmx5",(byte*)"mmx6",(byte*)"mmx7"};
static u8 *  mc_decls$jmpccnames[18] = {
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
static byte mc_decls$jmpcccodes[18] = {
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
static u8 *  mc_decls$setccnames[16] = {
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
    (byte*)"setg"
};
static byte mc_decls$setcccodes[16] = {
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
    (u8)15u
};
static u8 *  mc_decls$cmovccnames[16] = {
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
    (byte*)"cmovg"
};
static byte mc_decls$cmovcccodes[16] = {
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
    (u8)15u
};
static u8 *  mc_decls$reftypenames[3] = {(byte*)"extern_ref",(byte*)"fwd_ref",(byte*)"back_ref"};
static i64 mc_decls$mlabelno;
static i64 mc_decls$mstackdepth;
static i64 mc_decls$retindex;
static u64 mc_decls$regset;
static i64 mc_decls$nregs;
static i64 mc_decls$nxregs;
static u64 mc_decls$isregvar;
static i64 mc_decls$inf_proccalls;
static i64 mc_decls$inf_proclocals;
static i64 mc_decls$inf_procxlocals;
static i64 mc_decls$inf_leafproc;
static i64 mc_decls$inf_highreg;
static i64 mc_decls$inf_highxreg;
static i64 mc_decls$inf_maxargs;
static i64 mc_decls$inf_assem;
static i64 mc_decls$inf_r10used;
static i64 mc_decls$inf_r11used;
static i64 mc_decls$inf_r13used;
static i64 mc_decls$dsaveregs[16];
static i64 mc_decls$xsaveregs[16];
static i64 mc_decls$ndsaveregs;
static i64 mc_decls$nxsaveregs;
static i64 mc_decls$dsaveoffset;
static i64 mc_decls$xsaveoffset;
static i64 mc_decls$needstackframe;
static i64 mc_decls$framebytes;
static i64 mc_decls$parambytes;
static i64 mc_decls$needshadow32;
static i64 mc_decls$dspillbytes;
static i64 mc_decls$xspillbytes;
static i64 mc_decls$alignbytes;
static i64 mc_decls$localbytes;
static i64 mc_decls$shadowbytes;
static byte mc_decls$noxorclear;
static struct mm_decls$strec *  mc_decls$procdef;
static i64 mc_decls$ncalldepth;
static struct mm_decls$strec *  mc_decls$paramdefs[32];
static struct mm_decls$strec *  mc_decls$localdefs[256];
static i64 mc_decls$nparams;
static i64 mc_decls$nlocals;
static i64 mc_decls$retmode;
static i64 mc_decls$passno;
static i64 mc_decls$sa_nargs;
static i64 mc_decls$multregs[6] = {(i64)1,(i64)2,(i64)3,(i64)11,(i64)12,(i64)13};
static i64 mc_decls$multxregs[6] = {(i64)21,(i64)22,(i64)23,(i64)24,(i64)25,(i64)26};
static i64 mc_decls$paramoffset;
static i64 mc_decls$lababs32;
static i64 mc_decls$lababs64;
static i64 mc_decls$labneg32;
static i64 mc_decls$labneg64;
static i64 mc_decls$labmask63;
static i64 mc_decls$laboffset64;
static i64 mc_decls$labzero;
static i64 mc_decls$kk0used = (i64)0;
static i64 mc_decls$stackaligned;
static struct mc_decls$mclrec *  mc_decls$mccode;
static struct mc_decls$mclrec *  mc_decls$mccodex;
static i64 mc_decls$currsegment = (i64)0;
static i64 mc_decls$currzdataalign = (i64)0;
static i64 mc_decls$curridataalign = (i64)0;
static i64 mc_decls$frameoffset;
static i64 mc_decls$isthreadedproc;
static i64 mc_decls$iscallbackproc;
static i64 mc_decls$structretoffset;
static struct mc_decls$mclrec *  mc_decls$stacksetinstr;
static i64 mc_decls$currblocksize;
static u8 *  mc_decls$allasmstr;
static i64 mc_decls$allasmstrlen;
static struct mc_decls$opndrec *  mc_decls$dstackopnd;
static struct mc_decls$opndrec *  mc_decls$distackopnd;
static struct mc_decls$opndrec *  mc_decls$dframeopnd;
static struct mc_decls$opndrec *  mc_decls$zero_opnd = 0;
static struct mc_decls$opndrec *  mc_decls$regtable[16][8];
static struct mc_decls$opndrec *  mc_decls$frameregtable[193];
static struct mc_decls$constrec *  mc_decls$cstringlist;
static struct mc_decls$constrec *  mc_decls$vstringlist;
static struct mc_decls$constrec *  mc_decls$creallist;
static struct mc_decls$constrec *  mc_decls$creal32list;
static i64 mc_decls$destlinestart;
static struct mm_decls$strec *  mc_decls$currasmproc;
static i64 mc_decls$noregvar;
static i64 mc_decls$lab_funcnametable;
static i64 mc_decls$lab_funcaddrtable;
static i64 mc_decls$lab_funcnprocs;
static i64 mc_decls$ss_zdatalen;
static struct mc_decls$dbuffer *  mc_decls$ss_zdata;
static struct mc_decls$dbuffer *  mc_decls$ss_idata;
static struct mc_decls$dbuffer *  mc_decls$ss_code;
static struct mc_decls$relocrec *  mc_decls$ss_idatarelocs;
static struct mc_decls$relocrec *  mc_decls$ss_coderelocs;
static i64 mc_decls$ss_nidatarelocs;
static i64 mc_decls$ss_ncoderelocs;
static struct mm_decls$strec *(*mc_decls$ss_symboltable)[];
static i64 mc_decls$ss_nsymbols;
static i64 mc_decls$ss_symboltablesize;
static struct mm_decls$strec *(*mc_decls$labeldeftable)[];
static i64 mc_decls$alineno;
static u8 *  mc_decls$segmentnames[5] = {(byte*)"code",(byte*)"idata",(byte*)"zdata",(byte*)"rodata",(byte*)"impdata_seg"};
static u8 *  mc_decls$loadnames[4] = {(byte*)"load_op",(byte*)"get_op",(byte*)"loadref_op",(byte*)"getref_op"};
static i64 mc_decls$nallmcl;
static struct mm_decls$strec *  mc_decls$blockdefs[50];
static i64 mc_decls$nblocktemps;
static struct mm_decls$strec *  mc_decls$blockretname;
static void *  msysc$_fnaddresses[]= {
    &main,
    &mm_cli$do_loadmodules,
    &mm_cli$do_parse,
    &mm_cli$do_name,
    &mm_cli$do_type,
    &mm_cli$initdata,
    &mm_cli$getinputoptions,
    &mm_cli$do_option,
    &mm_cli$showcaption,
    &mm_cli$showhelp,
    &mm_cli$initassemsymbols,
    &mm_cli$do_writeexports,
    &mm_cli$getoutfilename,
    &mm_cli$fixstartprocs,
    &mm_cli$addstartproc,
    &mm_cli$stepruncount,
    &mm_cli$showmoduleinfo,
    &mm_cli$start,
    &mm_assem$readassemline,
    &mm_assem$readassemblock,
    &mm_assem$assembleline,
    &mm_assem$readassemopnd,
    &mm_assem$start,
    &mc_blockc$evalunit,
    &mc_blockc$evalstmt,
    &mc_blockc$evalunitc,
    &mc_blockc$evalblock,
    &mc_blockc$do_block,
    &mc_blockc$evalblocklab,
    &mc_blockc$do_blocklab,
    &mc_blockc$do_const,
    &mc_blockc$do_return,
    &mc_blockc$do_assign,
    &mc_blockc$do_bin,
    &mc_blockc$do_binto,
    &mc_blockc$do_unary,
    &mc_blockc$do_convert,
    &mc_blockc$do_if,
    &mc_blockc$do_ifx,
    &mc_blockc$do_call,
    &mc_blockc$do_do,
    &mc_blockc$definelabel,
    &mc_blockc$createfwdlabel,
    &mc_blockc$definefwdlabel,
    &mc_blockc$stacklooplabels,
    &mc_blockc$unstacklooplabels,
    &mc_blockc$findlooplabel,
    &mc_blockc$do_exit,
    &mc_blockc$do_to,
    &mc_blockc$do_while,
    &mc_blockc$do_repeat,
    &mc_blockc$do_forup,
    &mc_blockc$do_forall,
    &mc_blockc$dxlabel,
    &mc_blockc$do_print,
    &mc_blockc$do_read,
    &mc_blockc$do_readln,
    &mc_blockc$do_index,
    &mc_blockc$do_case,
    &mc_blockc$do_switch,
    &mc_blockc$do_goto,
    &mc_blockc$do_max,
    &mc_blockc$do_maxto,
    &mc_blockc$isexpr,
    &mc_blockc$do_swap,
    &mc_blockc$do_inrange,
    &mc_blockc$do_inset,
    &mc_blockc$do_cmpchain,
    &mc_blockc$do_blockcopy,
    &mc_blockc$do_select,
    &mc_blockc$do_dotindex,
    &mc_blockc$do_supportcall,
    &mc_blockc$applymemmode,
    &mc_blockc$domaths2,
    &mc_blockc$do_typepun,
    &mc_blockc$start,
    &mm_decls$start,
    &mm_diags$printoverloads,
    &mm_diags$printst,
    &mm_diags$printstrec,
    &mm_diags$printstflat,
    &mm_diags$printcode,
    &mm_diags$printunit,
    &mm_diags$printunitlist,
    &mm_diags$getprefix,
    &mm_diags$getlineinfok,
    &mm_diags$printmodelist,
    &mm_diags$showprojectinfo,
    &mm_diags$showlogfile,
    &mm_diags$showstflat,
    &mm_diags$showsttree,
    &mm_diags$showast,
    &mm_diags$printsymbol,
    &mm_diags$start,
    &mm_export$writeexports,
    &mm_export$exportstatic,
    &mm_export$exportconst,
    &mm_export$exportproc,
    &mm_export$wxstr,
    &mm_export$wxstrln,
    &mm_export$wxline,
    &mm_export$exportrecord,
    &mm_export$wxmode,
    &mm_export$start,
    &mc_genc$codegen_clang,
    &mc_genc$do_infoheader,
    &mc_genc$do_cheader,
    &mc_genc$do_alltypes,
    &mc_genc$scansymbol,
    &mc_genc$addsymbol,
    &mc_genc$do_typedef_fwd,
    &mc_genc$do_typedef,
    &mc_genc$do_allprocdecls,
    &mc_genc$do_allvars,
    &mc_genc$do_allprocdefs,
    &mc_genc$do_procdecl,
    &mc_genc$do_vardef,
    &mc_genc$do_dllvar,
    &mc_genc$genprocdef,
    &mc_genc$do_maininit,
    &mc_genc$do_startinit,
    &mc_genc$do_mainterm,
    &mc_genc$writefn_nprocs,
    &mc_genc$writefn_names,
    &mc_genc$writefn_addresses,
    &mc_genc$genlocalvar,
    &mc_genc$docallproc,
    &mc_genc$start,
    &mm_lex$lexreadtoken,
    &mm_lex$lex,
    &mm_lex$lexsetup,
    &mm_lex$printstrn,
    &mm_lex$readrawstring,
    &mm_lex$lookup,
    &mm_lex$lookupsys,
    &mm_lex$gethashvaluez,
    &mm_lex$inithashtable,
    &mm_lex$printhashtable,
    &mm_lex$addreservedword,
    &mm_lex$dolexdirective,
    &mm_lex$startlex,
    &mm_lex$start,
    &mm_lex$addnamestr,
    &mm_lex$ps,
    &mm_lex$psnext,
    &mm_lex$psx,
    &mm_lex$stacksource,
    &mm_lex$unstacksource,
    &mm_lex$readarraystring,
    &mm_lex$setinttype,
    &mm_lex$readrawxname,
    &mm_lex$lxerror_s,
    &mm_lex$lxreadstring,
    &mm_lex$readdec,
    &mm_lex$readhex,
    &mm_lex$readoct,
    &mm_lex$readbin,
    &mm_lex$readreal,
    &mm_lib$newstrec,
    &mm_lib$getduplnameptr,
    &mm_lib$adddef,
    &mm_lib$createname,
    &mm_lib$createunit0,
    &mm_lib$createunit1,
    &mm_lib$createunit2,
    &mm_lib$createunit3,
    &mm_lib$insertunit,
    &mm_lib$deleteunit,
    &mm_lib$createconstunit,
    &mm_lib$createstringconstunit,
    &mm_lib$newtypename,
    &mm_lib$createusertype,
    &mm_lib$createusertypefromstr,
    &mm_lib$getrangelwbunit,
    &mm_lib$getrangeupbunit,
    &mm_lib$createarraymode,
    &mm_lib$sameunit,
    &mm_lib$createarraymodek,
    &mm_lib$nextautotype,
    &mm_lib$createslicemode,
    &mm_lib$createslicemodek,
    &mm_lib$createrefmode,
    &mm_lib$createrefprocmode,
    &mm_lib$copyttvalues,
    &mm_lib$getdottedname,
    &mm_lib$getavname,
    &mm_lib$unionstr_clear,
    &mm_lib$unionstr_append,
    &mm_lib$unionstr_concat,
    &mm_lib$unionstr_last,
    &mm_lib$unionstr_copy,
    &mm_lib$createrecordmode,
    &mm_lib$createtuplemode,
    &mm_lib$convertstring,
    &mm_lib$strexpr,
    &mm_lib$jevalx,
    &mm_lib$strmode,
    &mm_lib$strmode2,
    &mm_lib$istrmode,
    &mm_lib$addtoproclist,
    &mm_lib$addstatic,
    &mm_lib$addexpconst,
    &mm_lib$typename,
    &mm_lib$allocunitrec,
    &mm_lib$createdupldef,
    &mm_lib$createnewmoduledef,
    &mm_lib$duplunit,
    &mm_lib$checkblockreturn,
    &mm_lib$isconstunit,
    &mm_lib$getownername,
    &mm_lib$getalignment,
    &mm_lib$ispoweroftwo,
    &mm_lib$addlistunit,
    &mm_lib$storemode,
    &mm_lib$gettypebase,
    &mm_lib$writegsfile,
    &mm_lib$addtolog,
    &mm_lib$getprocretmodes,
    &mm_lib$getmemmode,
    &mm_lib$getpclmode,
    &mm_lib$getfullname,
    &mm_lib$getbasename,
    &mm_lib$start,
    &mc_libc$cccomment,
    &mc_libc$ccblank,
    &mc_libc$cclinecomment,
    &mc_libc$ccchar,
    &mc_libc$cctab,
    &mc_libc$ccstr,
    &mc_libc$ccstrline,
    &mc_libc$ccstrsemi,
    &mc_libc$ccstrsemiu,
    &mc_libc$ccsendline,
    &mc_libc$ccint,
    &mc_libc$ccinitline,
    &mc_libc$strmodec,
    &mc_libc$strmodec2,
    &mc_libc$strmodex,
    &mc_libc$strprocsig,
    &mc_libc$getprocname,
    &mc_libc$getfullnamec,
    &mc_libc$getfullnamec2,
    &mc_libc$genclabel,
    &mc_libc$genrecorddef,
    &mc_libc$genrecordfwd,
    &mc_libc$do_initdata,
    &mc_libc$cclongstr,
    &mc_libc$do_makelist,
    &mc_libc$issimplec,
    &mc_libc$strstringc,
    &mc_libc$do_syscallproc,
    &mc_libc$evalsysparam,
    &mc_libc$dxstr,
    &mc_libc$dxchar,
    &mc_libc$dxint,
    &mc_libc$start,
    &mm_libsourcesc$findsyslib,
    &mm_libsourcesc$start,
    &mm_modules$readprojectfile,
    &mm_modules$initheadervars,
    &mm_modules$readmoduledir,
    &mm_modules$checkwhen,
    &mm_modules$addmodule,
    &mm_modules$addsubprogram,
    &mm_modules$addfirstsubprogram,
    &mm_modules$readsubprogram,
    &mm_modules$readimport,
    &mm_modules$readinclude,
    &mm_modules$readvar,
    &mm_modules$fixpath,
    &mm_modules$dosetvar,
    &mm_modules$doshowvar,
    &mm_modules$setmixedprogram,
    &mm_modules$setmixedimport,
    &mm_modules$loadmodules,
    &mm_modules$loadmodule,
    &mm_modules$addsyslib,
    &mm_modules$addlib,
    &mm_modules$readfileline,
    &mm_modules$findnextlineheader,
    &mm_modules$loadmafile,
    &mm_modules$start,
    &mm_name$rx_typetable,
    &mm_name$rx_unit,
    &mm_name$rx_module,
    &mm_name$rx_deflist,
    &mm_name$rx_passdef,
    &mm_name$rx_unitlist,
    &mm_name$resolvetopname,
    &mm_name$resolvename,
    &mm_name$finddupl,
    &mm_name$finddupl_sub,
    &mm_name$resolvedot,
    &mm_name$fixmode,
    &mm_name$fixusertypes,
    &mm_name$addframevar,
    &mm_name$copylistunit,
    &mm_name$copyunit,
    &mm_name$replaceunit,
    &mm_name$expandmacro,
    &mm_name$duplfield,
    &mm_name$do_baseclass,
    &mm_name$start,
    &mm_parse$parsemodule,
    &mm_parse$readmoduledefs,
    &mm_parse$initparser,
    &mm_parse$skipsemi,
    &mm_parse$makeblock,
    &mm_parse$checkequals,
    &mm_parse$getcurrline,
    &mm_parse$checkbegin,
    &mm_parse$checkbeginend,
    &mm_parse$checkend,
    &mm_parse$readvardef,
    &mm_parse$readconstdef,
    &mm_parse$readlbrack,
    &mm_parse$addlistparam,
    &mm_parse$readcast,
    &mm_parse$readopc,
    &mm_parse$readsprint,
    &mm_parse$readsread,
    &mm_parse$readcompilervar,
    &mm_parse$readcastx,
    &mm_parse$checksymbol,
    &mm_parse$lexchecksymbol,
    &mm_parse$readtypespec,
    &mm_parse$readslicetype,
    &mm_parse$readslist,
    &mm_parse$readindex,
    &mm_parse$readdotsuffix,
    &mm_parse$readconstexpr,
    &mm_parse$readconstint,
    &mm_parse$readprocdef,
    &mm_parse$readprocdecl,
    &mm_parse$readparams,
    &mm_parse$readcondsuffix,
    &mm_parse$readif,
    &mm_parse$readgoto,
    &mm_parse$readunless,
    &mm_parse$readswitchcase,
    &mm_parse$readstop,
    &mm_parse$readreturn,
    &mm_parse$readdo,
    &mm_parse$readto,
    &mm_parse$readwhile,
    &mm_parse$readrepeat,
    &mm_parse$readloopcontrol,
    &mm_parse$readprint,
    &mm_parse$readread,
    &mm_parse$readfor,
    &mm_parse$readname,
    &mm_parse$readtypedef,
    &mm_parse$readrecordfields,
    &mm_parse$readtabledef,
    &mm_parse$readclassdef,
    &mm_parse$readclassbody,
    &mm_parse$readimportmodule,
    &mm_parse$readimportbody,
    &mm_parse$readequivfield,
    &mm_parse$readrefproc,
    &mm_parse$pushproc,
    &mm_parse$popproc,
    &mm_parse$makeastring,
    &mm_parse$readreturntype,
    &mm_parse$readset,
    &mm_parse$istypestarter,
    &mm_parse$readunit,
    &mm_parse$readassignment,
    &mm_parse$readorterms,
    &mm_parse$readandterms,
    &mm_parse$readcmpterms,
    &mm_parse$readinterms,
    &mm_parse$readrangeterm,
    &mm_parse$readaddterms,
    &mm_parse$readmulterms,
    &mm_parse$readpowerterms,
    &mm_parse$readterm2,
    &mm_parse$readterm,
    &mm_parse$readmacrodef,
    &mm_parse$readrecase,
    &mm_parse$adddocstring,
    &mm_parse$fixcond,
    &mm_parse$readsunit,
    &mm_parse$start,
    &mm_support$loadsourcefile,
    &mm_support$loadbundledfile,
    &mm_support$mcerror,
    &mm_support$serror_gen,
    &mm_support$showdivider,
    &mm_support$showerrorsource,
    &mm_support$stopcompiler,
    &mm_support$serror,
    &mm_support$serror_s,
    &mm_support$error_gen,
    &mm_support$rxerror,
    &mm_support$gerror,
    &mm_support$axerror,
    &mm_support$txerror,
    &mm_support$txerror_s,
    &mm_support$txerror_ss,
    &mm_support$rxerror_s,
    &mm_support$gerror_s,
    &mm_support$gerror_t,
    &mm_support$lxerror_gen,
    &mm_support$lxerror,
    &mm_support$loaderror,
    &mm_support$gs_additem,
    &mm_support$gs_copytostr,
    &mm_support$isalphanum,
    &mm_support$init_tt_tables,
    &mm_support$addspecialtypes,
    &mm_support$getsupportfile,
    &mm_support$isabspath,
    &mm_support$initbblib,
    &mm_support$getfileno,
    &mm_support$getlineno,
    &mm_support$getsourceline,
    &mm_support$getsourcestart,
    &mm_support$getsourcepos,
    &mm_support$do_writema,
    &mm_support$start,
    &mm_tables$start,
    &mm_type$tpass,
    &mm_type$tx_allprocs,
    &mm_type$tx_block,
    &mm_type$tx_typetable,
    &mm_type$setmodesize,
    &mm_type$setarraysize,
    &mm_type$setslicesize,
    &mm_type$tx_module,
    &mm_type$tx_passdef,
    &mm_type$tx_unitlist,
    &mm_type$tx_namedef,
    &mm_type$tx_namedconst,
    &mm_type$checkconstexpr,
    &mm_type$getconstint,
    &mm_type$makenewconst,
    &mm_type$tx_name,
    &mm_type$tx_bin,
    &mm_type$tx_binto,
    &mm_type$getdominantmode,
    &mm_type$tx_cmpchain,
    &mm_type$tx_callproc,
    &mm_type$tx_unary,
    &mm_type$tx_if,
    &mm_type$tx_incrto,
    &mm_type$tx_for,
    &mm_type$tx_forall,
    &mm_type$tx_index,
    &mm_type$tx_makerange,
    &mm_type$tx_ptr,
    &mm_type$setrecordsize,
    &mm_type$checkblocktype,
    &mm_type$scanrecord,
    &mm_type$roundoffset,
    &mm_type$tx_convert,
    &mm_type$tx_makelist,
    &mm_type$tx_makeslice,
    &mm_type$tx_makeset,
    &mm_type$tx_dot,
    &mm_type$resolvefield,
    &mm_type$tx_andl,
    &mm_type$convintconst,
    &mm_type$tx_sliceptr,
    &mm_type$tx_swap,
    &mm_type$tx_select,
    &mm_type$tx_case,
    &mm_type$tx_notl,
    &mm_type$tx_istruel,
    &mm_type$tx_typepun,
    &mm_type$tx_exit,
    &mm_type$tx_goto,
    &mm_type$tx_switch,
    &mm_type$tx_addroffirst,
    &mm_type$tx_return,
    &mm_type$tx_dotindex,
    &mm_type$tx_slice,
    &mm_type$twiden,
    &mm_type$tstringslice,
    &mm_type$tx_bitfield,
    &mm_type$deref,
    &mm_type$tmethodcall,
    &mm_type$do_bounds,
    &mm_type$addnotl,
    &mm_type$tevaluate,
    &mm_type$addrdotindex,
    &mm_type$tevalbinop,
    &mm_type$tevalmonop,
    &mm_type$iscondtrue,
    &mm_type$iscondfalse,
    &mm_type$fixchararray,
    &mm_type$combinestrings,
    &mm_type$tx_strinclude,
    &mm_type$coerceunit,
    &mm_type$getconversionop,
    &mm_type$applyconversion,
    &mm_type$checkmodes,
    &mm_type$comparemodes,
    &mm_type$tevalconvert,
    &mm_type$tx_assign,
    &mm_type$tx_assignmultmult,
    &mm_type$tx_assignmultscalar,
    &mm_type$tpasslv,
    &mm_type$dobinnumx,
    &mm_type$dobinnumf,
    &mm_type$dobinnumi,
    &mm_type$doin,
    &mm_type$setsimple,
    &mm_type$start,
    &mm_winc$codegen,
    &mm_winc$runlibfile,
    &mm_winc$writeexefile,
    &mm_winc$writelibfile,
    &mm_winc$writeasmfile,
    &mm_winc$do_link_win,
    &mm_winc$do_link_lin,
    &mm_winc$start,
    &mc_decls$start,
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
    &mlib$addtomemalloc,
    &mlib$removefrommemalloc,
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
    &mlib$pcm_alloc64,
    &mlib$pcm_free64,
    &mlib$pcm_alloc16,
    &mlib$pcm_free16,
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
    &mlib$checkpackfile,
    &mlib$readline,
    &mlib$findfunction,
    &mlib$roundtoblock,
    &mlib$start,
    &mclib$start,
    &mlinux$os_init,
    &mlinux$os_execwait,
    &mlinux$os_execcmd,
    &mlinux$os_getch,
    &mlinux$os_kbhit,
    &mlinux$os_flushkeys,
    &mlinux$os_getconsolein,
    &mlinux$os_getconsoleout,
    &mlinux$os_proginstance,
    &mlinux$os_getdllinst,
    &mlinux$os_getdllprocaddr,
    &mlinux$os_initwindows,
    &mlinux$os_getchx,
    &mlinux$os_getos,
    &mlinux$os_gethostsize,
    &mlinux$os_iswindows,
    &mlinux$os_shellexec,
    &mlinux$os_sleep,
    &mlinux$os_getstdin,
    &mlinux$os_getstdout,
    &mlinux$os_gethostname,
    &mlinux$os_getmpath,
    &mlinux$os_exitprocess,
    &mlinux$os_clock,
    &mlinux$os_ticks,
    &mlinux$os_getclockspersec,
    &mlinux$os_setmesshandler,
    &mlinux$os_hpcounter,
    &mlinux$os_hpfrequency,
    &mlinux$os_filelastwritetime,
    &mlinux$os_getsystime,
    &mlinux$os_peek,
    &mlinux$os_allocexecmem,
    &mlinux$start,
    &mwindllc$os_calldllfunction,
    &mwindllc$os_pushargs,
    &mwindllc$calldll_cint,
    &mwindllc$calldll_creal,
    &mwindllc$os_dummycall,
    &mwindllc$start,
0};
static u8 *  msysc$_fnnames[]= {
    (byte*)"main",
    (byte*)"do_loadmodules",
    (byte*)"do_parse",
    (byte*)"do_name",
    (byte*)"do_type",
    (byte*)"initdata",
    (byte*)"getinputoptions",
    (byte*)"do_option",
    (byte*)"showcaption",
    (byte*)"showhelp",
    (byte*)"initassemsymbols",
    (byte*)"do_writeexports",
    (byte*)"getoutfilename",
    (byte*)"fixstartprocs",
    (byte*)"addstartproc",
    (byte*)"stepruncount",
    (byte*)"showmoduleinfo",
    (byte*)"start",
    (byte*)"readassemline",
    (byte*)"readassemblock",
    (byte*)"assembleline",
    (byte*)"readassemopnd",
    (byte*)"start",
    (byte*)"evalunit",
    (byte*)"evalstmt",
    (byte*)"evalunitc",
    (byte*)"evalblock",
    (byte*)"do_block",
    (byte*)"evalblocklab",
    (byte*)"do_blocklab",
    (byte*)"do_const",
    (byte*)"do_return",
    (byte*)"do_assign",
    (byte*)"do_bin",
    (byte*)"do_binto",
    (byte*)"do_unary",
    (byte*)"do_convert",
    (byte*)"do_if",
    (byte*)"do_ifx",
    (byte*)"do_call",
    (byte*)"do_do",
    (byte*)"definelabel",
    (byte*)"createfwdlabel",
    (byte*)"definefwdlabel",
    (byte*)"stacklooplabels",
    (byte*)"unstacklooplabels",
    (byte*)"findlooplabel",
    (byte*)"do_exit",
    (byte*)"do_to",
    (byte*)"do_while",
    (byte*)"do_repeat",
    (byte*)"do_forup",
    (byte*)"do_forall",
    (byte*)"dxlabel",
    (byte*)"do_print",
    (byte*)"do_read",
    (byte*)"do_readln",
    (byte*)"do_index",
    (byte*)"do_case",
    (byte*)"do_switch",
    (byte*)"do_goto",
    (byte*)"do_max",
    (byte*)"do_maxto",
    (byte*)"isexpr",
    (byte*)"do_swap",
    (byte*)"do_inrange",
    (byte*)"do_inset",
    (byte*)"do_cmpchain",
    (byte*)"do_blockcopy",
    (byte*)"do_select",
    (byte*)"do_dotindex",
    (byte*)"do_supportcall",
    (byte*)"applymemmode",
    (byte*)"domaths2",
    (byte*)"do_typepun",
    (byte*)"start",
    (byte*)"start",
    (byte*)"printoverloads",
    (byte*)"printst",
    (byte*)"printstrec",
    (byte*)"printstflat",
    (byte*)"printcode",
    (byte*)"printunit",
    (byte*)"printunitlist",
    (byte*)"getprefix",
    (byte*)"getlineinfok",
    (byte*)"printmodelist",
    (byte*)"showprojectinfo",
    (byte*)"showlogfile",
    (byte*)"showstflat",
    (byte*)"showsttree",
    (byte*)"showast",
    (byte*)"printsymbol",
    (byte*)"start",
    (byte*)"writeexports",
    (byte*)"exportstatic",
    (byte*)"exportconst",
    (byte*)"exportproc",
    (byte*)"wxstr",
    (byte*)"wxstrln",
    (byte*)"wxline",
    (byte*)"exportrecord",
    (byte*)"wxmode",
    (byte*)"start",
    (byte*)"codegen_clang",
    (byte*)"do_infoheader",
    (byte*)"do_cheader",
    (byte*)"do_alltypes",
    (byte*)"scansymbol",
    (byte*)"addsymbol",
    (byte*)"do_typedef_fwd",
    (byte*)"do_typedef",
    (byte*)"do_allprocdecls",
    (byte*)"do_allvars",
    (byte*)"do_allprocdefs",
    (byte*)"do_procdecl",
    (byte*)"do_vardef",
    (byte*)"do_dllvar",
    (byte*)"genprocdef",
    (byte*)"do_maininit",
    (byte*)"do_startinit",
    (byte*)"do_mainterm",
    (byte*)"writefn_nprocs",
    (byte*)"writefn_names",
    (byte*)"writefn_addresses",
    (byte*)"genlocalvar",
    (byte*)"docallproc",
    (byte*)"start",
    (byte*)"lexreadtoken",
    (byte*)"lex",
    (byte*)"lexsetup",
    (byte*)"printstrn",
    (byte*)"readrawstring",
    (byte*)"lookup",
    (byte*)"lookupsys",
    (byte*)"gethashvaluez",
    (byte*)"inithashtable",
    (byte*)"printhashtable",
    (byte*)"addreservedword",
    (byte*)"dolexdirective",
    (byte*)"startlex",
    (byte*)"start",
    (byte*)"addnamestr",
    (byte*)"ps",
    (byte*)"psnext",
    (byte*)"psx",
    (byte*)"stacksource",
    (byte*)"unstacksource",
    (byte*)"readarraystring",
    (byte*)"setinttype",
    (byte*)"readrawxname",
    (byte*)"lxerror_s",
    (byte*)"lxreadstring",
    (byte*)"readdec",
    (byte*)"readhex",
    (byte*)"readoct",
    (byte*)"readbin",
    (byte*)"readreal",
    (byte*)"newstrec",
    (byte*)"getduplnameptr",
    (byte*)"adddef",
    (byte*)"createname",
    (byte*)"createunit0",
    (byte*)"createunit1",
    (byte*)"createunit2",
    (byte*)"createunit3",
    (byte*)"insertunit",
    (byte*)"deleteunit",
    (byte*)"createconstunit",
    (byte*)"createstringconstunit",
    (byte*)"newtypename",
    (byte*)"createusertype",
    (byte*)"createusertypefromstr",
    (byte*)"getrangelwbunit",
    (byte*)"getrangeupbunit",
    (byte*)"createarraymode",
    (byte*)"sameunit",
    (byte*)"createarraymodek",
    (byte*)"nextautotype",
    (byte*)"createslicemode",
    (byte*)"createslicemodek",
    (byte*)"createrefmode",
    (byte*)"createrefprocmode",
    (byte*)"copyttvalues",
    (byte*)"getdottedname",
    (byte*)"getavname",
    (byte*)"unionstr_clear",
    (byte*)"unionstr_append",
    (byte*)"unionstr_concat",
    (byte*)"unionstr_last",
    (byte*)"unionstr_copy",
    (byte*)"createrecordmode",
    (byte*)"createtuplemode",
    (byte*)"convertstring",
    (byte*)"strexpr",
    (byte*)"jevalx",
    (byte*)"strmode",
    (byte*)"strmode2",
    (byte*)"istrmode",
    (byte*)"addtoproclist",
    (byte*)"addstatic",
    (byte*)"addexpconst",
    (byte*)"typename",
    (byte*)"allocunitrec",
    (byte*)"createdupldef",
    (byte*)"createnewmoduledef",
    (byte*)"duplunit",
    (byte*)"checkblockreturn",
    (byte*)"isconstunit",
    (byte*)"getownername",
    (byte*)"getalignment",
    (byte*)"ispoweroftwo",
    (byte*)"addlistunit",
    (byte*)"storemode",
    (byte*)"gettypebase",
    (byte*)"writegsfile",
    (byte*)"addtolog",
    (byte*)"getprocretmodes",
    (byte*)"getmemmode",
    (byte*)"getpclmode",
    (byte*)"getfullname",
    (byte*)"getbasename",
    (byte*)"start",
    (byte*)"cccomment",
    (byte*)"ccblank",
    (byte*)"cclinecomment",
    (byte*)"ccchar",
    (byte*)"cctab",
    (byte*)"ccstr",
    (byte*)"ccstrline",
    (byte*)"ccstrsemi",
    (byte*)"ccstrsemiu",
    (byte*)"ccsendline",
    (byte*)"ccint",
    (byte*)"ccinitline",
    (byte*)"strmodec",
    (byte*)"strmodec2",
    (byte*)"strmodex",
    (byte*)"strprocsig",
    (byte*)"getprocname",
    (byte*)"getfullnamec",
    (byte*)"getfullnamec2",
    (byte*)"genclabel",
    (byte*)"genrecorddef",
    (byte*)"genrecordfwd",
    (byte*)"do_initdata",
    (byte*)"cclongstr",
    (byte*)"do_makelist",
    (byte*)"issimplec",
    (byte*)"strstringc",
    (byte*)"do_syscallproc",
    (byte*)"evalsysparam",
    (byte*)"dxstr",
    (byte*)"dxchar",
    (byte*)"dxint",
    (byte*)"start",
    (byte*)"findsyslib",
    (byte*)"start",
    (byte*)"readprojectfile",
    (byte*)"initheadervars",
    (byte*)"readmoduledir",
    (byte*)"checkwhen",
    (byte*)"addmodule",
    (byte*)"addsubprogram",
    (byte*)"addfirstsubprogram",
    (byte*)"readsubprogram",
    (byte*)"readimport",
    (byte*)"readinclude",
    (byte*)"readvar",
    (byte*)"fixpath",
    (byte*)"dosetvar",
    (byte*)"doshowvar",
    (byte*)"setmixedprogram",
    (byte*)"setmixedimport",
    (byte*)"loadmodules",
    (byte*)"loadmodule",
    (byte*)"addsyslib",
    (byte*)"addlib",
    (byte*)"readfileline",
    (byte*)"findnextlineheader",
    (byte*)"loadmafile",
    (byte*)"start",
    (byte*)"rx_typetable",
    (byte*)"rx_unit",
    (byte*)"rx_module",
    (byte*)"rx_deflist",
    (byte*)"rx_passdef",
    (byte*)"rx_unitlist",
    (byte*)"resolvetopname",
    (byte*)"resolvename",
    (byte*)"finddupl",
    (byte*)"finddupl_sub",
    (byte*)"resolvedot",
    (byte*)"fixmode",
    (byte*)"fixusertypes",
    (byte*)"addframevar",
    (byte*)"copylistunit",
    (byte*)"copyunit",
    (byte*)"replaceunit",
    (byte*)"expandmacro",
    (byte*)"duplfield",
    (byte*)"do_baseclass",
    (byte*)"start",
    (byte*)"parsemodule",
    (byte*)"readmoduledefs",
    (byte*)"initparser",
    (byte*)"skipsemi",
    (byte*)"makeblock",
    (byte*)"checkequals",
    (byte*)"getcurrline",
    (byte*)"checkbegin",
    (byte*)"checkbeginend",
    (byte*)"checkend",
    (byte*)"readvardef",
    (byte*)"readconstdef",
    (byte*)"readlbrack",
    (byte*)"addlistparam",
    (byte*)"readcast",
    (byte*)"readopc",
    (byte*)"readsprint",
    (byte*)"readsread",
    (byte*)"readcompilervar",
    (byte*)"readcastx",
    (byte*)"checksymbol",
    (byte*)"lexchecksymbol",
    (byte*)"readtypespec",
    (byte*)"readslicetype",
    (byte*)"readslist",
    (byte*)"readindex",
    (byte*)"readdotsuffix",
    (byte*)"readconstexpr",
    (byte*)"readconstint",
    (byte*)"readprocdef",
    (byte*)"readprocdecl",
    (byte*)"readparams",
    (byte*)"readcondsuffix",
    (byte*)"readif",
    (byte*)"readgoto",
    (byte*)"readunless",
    (byte*)"readswitchcase",
    (byte*)"readstop",
    (byte*)"readreturn",
    (byte*)"readdo",
    (byte*)"readto",
    (byte*)"readwhile",
    (byte*)"readrepeat",
    (byte*)"readloopcontrol",
    (byte*)"readprint",
    (byte*)"readread",
    (byte*)"readfor",
    (byte*)"readname",
    (byte*)"readtypedef",
    (byte*)"readrecordfields",
    (byte*)"readtabledef",
    (byte*)"readclassdef",
    (byte*)"readclassbody",
    (byte*)"readimportmodule",
    (byte*)"readimportbody",
    (byte*)"readequivfield",
    (byte*)"readrefproc",
    (byte*)"pushproc",
    (byte*)"popproc",
    (byte*)"makeastring",
    (byte*)"readreturntype",
    (byte*)"readset",
    (byte*)"istypestarter",
    (byte*)"readunit",
    (byte*)"readassignment",
    (byte*)"readorterms",
    (byte*)"readandterms",
    (byte*)"readcmpterms",
    (byte*)"readinterms",
    (byte*)"readrangeterm",
    (byte*)"readaddterms",
    (byte*)"readmulterms",
    (byte*)"readpowerterms",
    (byte*)"readterm2",
    (byte*)"readterm",
    (byte*)"readmacrodef",
    (byte*)"readrecase",
    (byte*)"adddocstring",
    (byte*)"fixcond",
    (byte*)"readsunit",
    (byte*)"start",
    (byte*)"loadsourcefile",
    (byte*)"loadbundledfile",
    (byte*)"mcerror",
    (byte*)"serror_gen",
    (byte*)"showdivider",
    (byte*)"showerrorsource",
    (byte*)"stopcompiler",
    (byte*)"serror",
    (byte*)"serror_s",
    (byte*)"error_gen",
    (byte*)"rxerror",
    (byte*)"gerror",
    (byte*)"axerror",
    (byte*)"txerror",
    (byte*)"txerror_s",
    (byte*)"txerror_ss",
    (byte*)"rxerror_s",
    (byte*)"gerror_s",
    (byte*)"gerror_t",
    (byte*)"lxerror_gen",
    (byte*)"lxerror",
    (byte*)"loaderror",
    (byte*)"gs_additem",
    (byte*)"gs_copytostr",
    (byte*)"isalphanum",
    (byte*)"init_tt_tables",
    (byte*)"addspecialtypes",
    (byte*)"getsupportfile",
    (byte*)"isabspath",
    (byte*)"initbblib",
    (byte*)"getfileno",
    (byte*)"getlineno",
    (byte*)"getsourceline",
    (byte*)"getsourcestart",
    (byte*)"getsourcepos",
    (byte*)"do_writema",
    (byte*)"start",
    (byte*)"start",
    (byte*)"tpass",
    (byte*)"tx_allprocs",
    (byte*)"tx_block",
    (byte*)"tx_typetable",
    (byte*)"setmodesize",
    (byte*)"setarraysize",
    (byte*)"setslicesize",
    (byte*)"tx_module",
    (byte*)"tx_passdef",
    (byte*)"tx_unitlist",
    (byte*)"tx_namedef",
    (byte*)"tx_namedconst",
    (byte*)"checkconstexpr",
    (byte*)"getconstint",
    (byte*)"makenewconst",
    (byte*)"tx_name",
    (byte*)"tx_bin",
    (byte*)"tx_binto",
    (byte*)"getdominantmode",
    (byte*)"tx_cmpchain",
    (byte*)"tx_callproc",
    (byte*)"tx_unary",
    (byte*)"tx_if",
    (byte*)"tx_incrto",
    (byte*)"tx_for",
    (byte*)"tx_forall",
    (byte*)"tx_index",
    (byte*)"tx_makerange",
    (byte*)"tx_ptr",
    (byte*)"setrecordsize",
    (byte*)"checkblocktype",
    (byte*)"scanrecord",
    (byte*)"roundoffset",
    (byte*)"tx_convert",
    (byte*)"tx_makelist",
    (byte*)"tx_makeslice",
    (byte*)"tx_makeset",
    (byte*)"tx_dot",
    (byte*)"resolvefield",
    (byte*)"tx_andl",
    (byte*)"convintconst",
    (byte*)"tx_sliceptr",
    (byte*)"tx_swap",
    (byte*)"tx_select",
    (byte*)"tx_case",
    (byte*)"tx_notl",
    (byte*)"tx_istruel",
    (byte*)"tx_typepun",
    (byte*)"tx_exit",
    (byte*)"tx_goto",
    (byte*)"tx_switch",
    (byte*)"tx_addroffirst",
    (byte*)"tx_return",
    (byte*)"tx_dotindex",
    (byte*)"tx_slice",
    (byte*)"twiden",
    (byte*)"tstringslice",
    (byte*)"tx_bitfield",
    (byte*)"deref",
    (byte*)"tmethodcall",
    (byte*)"do_bounds",
    (byte*)"addnotl",
    (byte*)"tevaluate",
    (byte*)"addrdotindex",
    (byte*)"tevalbinop",
    (byte*)"tevalmonop",
    (byte*)"iscondtrue",
    (byte*)"iscondfalse",
    (byte*)"fixchararray",
    (byte*)"combinestrings",
    (byte*)"tx_strinclude",
    (byte*)"coerceunit",
    (byte*)"getconversionop",
    (byte*)"applyconversion",
    (byte*)"checkmodes",
    (byte*)"comparemodes",
    (byte*)"tevalconvert",
    (byte*)"tx_assign",
    (byte*)"tx_assignmultmult",
    (byte*)"tx_assignmultscalar",
    (byte*)"tpasslv",
    (byte*)"dobinnumx",
    (byte*)"dobinnumf",
    (byte*)"dobinnumi",
    (byte*)"doin",
    (byte*)"setsimple",
    (byte*)"start",
    (byte*)"codegen",
    (byte*)"runlibfile",
    (byte*)"writeexefile",
    (byte*)"writelibfile",
    (byte*)"writeasmfile",
    (byte*)"do_link_win",
    (byte*)"do_link_lin",
    (byte*)"start",
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
    (byte*)"addtomemalloc",
    (byte*)"removefrommemalloc",
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
    (byte*)"pcm_alloc64",
    (byte*)"pcm_free64",
    (byte*)"pcm_alloc16",
    (byte*)"pcm_free16",
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
    (byte*)"checkpackfile",
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
    (byte*)"os_flushkeys",
    (byte*)"os_getconsolein",
    (byte*)"os_getconsoleout",
    (byte*)"os_proginstance",
    (byte*)"os_getdllinst",
    (byte*)"os_getdllprocaddr",
    (byte*)"os_initwindows",
    (byte*)"os_getchx",
    (byte*)"os_getos",
    (byte*)"os_gethostsize",
    (byte*)"os_iswindows",
    (byte*)"os_shellexec",
    (byte*)"os_sleep",
    (byte*)"os_getstdin",
    (byte*)"os_getstdout",
    (byte*)"os_gethostname",
    (byte*)"os_getmpath",
    (byte*)"os_exitprocess",
    (byte*)"os_clock",
    (byte*)"os_ticks",
    (byte*)"os_getclockspersec",
    (byte*)"os_setmesshandler",
    (byte*)"os_hpcounter",
    (byte*)"os_hpfrequency",
    (byte*)"os_filelastwritetime",
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
static i64 msysc$_fnnprocs=730;
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
static i64 mlib$bigmemtotal;
static i32 *  mlib$memalloctable[3];
static i32 mlib$memallocsize[3];
static byte *  mlib$pcheapstart;
static byte *  mlib$pcheapend;
static byte *  mlib$pcheapptr;
static byte mlib$sizeindextable[2049];
static u64 *  mlib$freelist[9];
static u8 *  mlib$pmnames[6] = {(byte*)"pm_end",(byte*)"pm_option",(byte*)"pm_sourcefile",(byte*)"pm_libfile",(byte*)"pm_colon",(byte*)"pm_extra"};
static u64 mlib$seed[2] = {(u64)2993073034246558322u,(u64)1617678968452121188u};
static i64 mlinux$init_flag = (i64)0;

/* PROCDEFS */
int main(int _nargs, char** _args, char** _envstrings) {
    msysc$m_init(_nargs, (void*)_args, (void*)_envstrings);

// call main-start() routines...
    msysc$start();
    mm_cli$start();

        i64 t;
    mm_cli$startclock = mlinux$os_clock();
    mm_cli$stepruncount();
    mm_cli$initdata();
    mm_cli$getinputoptions();
    mm_cli$rpclock = clock();
    mm_modules$readprojectfile(mm_cli$inputfile);
    if ((mm_decls$fverbose >= (i64)1)) {
        if ((mm_decls$passlevel == (i64)13)) {
            if ((!(!!((i64)mm_cli$msfile)) || (mm_decls$fverbose > (i64)1))) {
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"Compiling",NULL);
                msysc$m_print_str(mm_cli$inputfile,NULL);
                msysc$m_print_str((byte*)"to memory",NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
            }
;
        }
        else {
            msysc$m_print_startcon();
            msysc$m_print_setfmt((byte*)"M6 Compiling # to #");
            msysc$m_print_str(mm_cli$inputfile,(byte*)"14jlp-");
            msysc$m_print_str(mlib$changeext(mm_decls$outfile,mm_cli$outext),NULL);
            msysc$m_print_space();
            msysc$m_print_end();
            ;
            msysc$m_print_startcon();
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
;
    }
;
    remove((byte*)"mx.log");
    mm_cli$do_loadmodules();
    mm_cli$do_parse();
    mm_cli$do_name();
    mm_cli$do_type();
    mm_support$do_writema();
    if (!!((i64)mm_decls$fwritema)) {
        goto L1 ;
;
    }
;
    mm_cli$do_writeexports();
    if ((mm_decls$passlevel==(i64)8)) {
        mm_winc$codegen();
    }
    else if ((mm_decls$passlevel==(i64)9)) {
        mm_winc$writeasmfile(mm_decls$asmfilename);
    }
    else if ((mm_decls$passlevel==(i64)11)) {
        mm_winc$writeexefile(mm_decls$exefilename,(i64)0);
    }
    else if ((mm_decls$passlevel==(i64)12)) {
        mm_winc$writelibfile(mm_decls$libfilename);
    }
    else if ((mm_decls$passlevel==(i64)13)) {
        mm_winc$runlibfile(mm_decls$libfilename);
    }
;
    //finish:
L1 :;
;
    if ((mm_decls$fverbose >= (i64)2)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Finished.",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    if (!!(mm_decls$debugmode)) {
        mm_diags$showlogfile();
    }
;
    if (!!((i64)mm_decls$fshowtiming)) {
        mm_cli$endclock = mlinux$os_clock();
        t = (mm_cli$endclock - mm_cli$startclock);
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Time",NULL);
        msysc$m_print_i64(t,NULL);
        msysc$m_print_str((byte*)"ms",NULL);
        msysc$m_print_end();
        ;
        if (!!(t)) {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)",",NULL);
            msysc$m_print_i64((i64)((r64)mm_lex$lxalllines / (r64)t),NULL);
            msysc$m_print_nogap();
            msysc$m_print_str((byte*)"K lines per second (",NULL);
            msysc$m_print_nogap();
            msysc$m_print_i64(mm_lex$lxalllines,NULL);
            msysc$m_print_nogap();
            msysc$m_print_str((byte*)")",NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
        else {
            msysc$m_print_startcon();
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
;
    }
;
    return 0;
}

static void mm_cli$do_loadmodules(void) {
        i64 tt;
    if ((mm_decls$passlevel < (i64)2)) {
        return;
    }
;
    if (!!((i64)mm_cli$fmodinfo)) {
        mm_cli$showmoduleinfo();
        exit(0);
    }
;
    mm_modules$loadmodules();
    tt = (clock() - mm_cli$rpclock);
    if (!!((i64)mm_decls$fshowtiming)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"LOAD",NULL);
        msysc$m_print_str((byte*)"TT=",NULL);
        msysc$m_print_i64(tt,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    mm_support$addspecialtypes();
}

static void mm_cli$do_parse(void) {
        i64 tt;
        i64 i;
    if ((mm_decls$passlevel < (i64)3)) {
        return;
    }
;
    if (!!((i64)mm_decls$fwritedocs)) {
        mm_parse$docfile = fopen(mlib$changeext(mm_decls$outfile,(byte*)"txt"),(byte*)"w");
    }
;
    tt = clock();
    for (i=(i64)2;i<=mm_decls$nmodules;++i) {
L2 :;
        mm_parse$parsemodule(i);
L3 :;
    }
L4 :;
    ;
    mm_parse$parsemodule((i64)1);
    tt = (clock() - tt);
    if (!!((i64)mm_decls$fshowtiming)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"PARSE",NULL);
        msysc$m_print_str((byte*)"TT=",NULL);
        msysc$m_print_i64(tt,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    if (!!(mm_parse$docfile)) {
        fclose(mm_parse$docfile);
    }
;
    if ((!(!!(mm_decls$debugmode)) || (mm_decls$passlevel >= (i64)4))) {
        mm_name$fixusertypes();
    }
;
    mm_cli$fixstartprocs();
    if ((!!(mm_decls$debugmode) && !!((i64)mm_decls$fshowast1))) {
        mm_diags$showast((byte*)"AST1");
    }
;
}

static void mm_cli$do_name(void) {
        i64 tt;
        i64 i;
    if ((mm_decls$passlevel < (i64)5)) {
        return;
    }
;
    mm_name$rx_typetable();
    tt = clock();
    for (i=(i64)2;i<=mm_decls$nmodules;++i) {
L5 :;
        mm_name$rx_module(i);
L6 :;
    }
L7 :;
    ;
    mm_name$rx_module((i64)1);
    tt = (clock() - tt);
    if (!!((i64)mm_decls$fshowtiming)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"NAME",NULL);
        msysc$m_print_str((byte*)"TT=",NULL);
        msysc$m_print_i64(tt,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    if ((!!(mm_decls$debugmode) && !!((i64)mm_decls$fshowast2))) {
        mm_diags$showast((byte*)"AST2");
    }
;
}

static void mm_cli$do_type(void) {
        i64 tt;
        i64 i;
    if ((mm_decls$passlevel < (i64)6)) {
        return;
    }
;
    tt = clock();
    mm_type$tx_typetable();
    for (i=(i64)1;i<=mm_decls$nmodules;++i) {
L8 :;
        mm_type$tx_module(i);
L9 :;
    }
L10 :;
    ;
    mm_type$tx_allprocs();
    if (!!((i64)mm_decls$fshowtiming)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"TYPE",NULL);
        msysc$m_print_i64((clock() - tt),NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    if ((!!(mm_decls$debugmode) && !!((i64)mm_decls$fshowast3))) {
        mm_diags$showast((byte*)"AST3");
    }
;
}

static void mm_cli$initdata(void) {
    mlib$pcm_init();
    mm_lex$lexsetup();
    mm_cli$initassemsymbols();
    mm_support$init_tt_tables();
    mm_support$initbblib();
    mc_libc$nilunit = (struct mm_decls$unitrec *)malloc((u64)16u);
    if (!!(mlinux$os_iswindows())) {
        mm_decls$fwindows = (i64)1;
    }
    else {
        mm_decls$flinux = (i64)1;
    }
;
}

static void mm_cli$getinputoptions(void) {
        i64 paramno;
        i64 pmtype;
        i64 sw;
        i64 ncolons;
        u8 *  name;
        u8 *  value;
        u8 *  filename;
        u8 filespec[300];
    mm_decls$prodmode = (i64)1;
    paramno = (i64)1;
    ncolons = (i64)0;
    if (!!(mlib$eqstring(mlib$extractfile(mlinux$os_gethostname()),(byte*)"ms.exe"))) {
        mm_cli$msfile = (i64)1;
        mm_cli$do_option((i64)15,(byte*)"");
    }
;
    L11 :;
    while (!!((pmtype = mlib$nextcmdparamnew(&paramno,&name,&value,(byte*)"m")))) {
        if ((pmtype==(i64)1)) {
            mlib$convlcstring(name);
            for (sw=(i64)1;sw<=(i64)59;++sw) {
L14 :;
                if (!!(mlib$eqstring(name,mm_cli$optionnames[(sw)-1]))) {
                    mm_cli$do_option(sw,value);
                    goto L16 ;
                }
;
L15 :;
            }
            {
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"Unknown option:",NULL);
                msysc$m_print_str(name,NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                exit((i64)99);
            }
L16 :;
            ;
        }
        else if ((pmtype==(i64)2)) {
            if (!!(mm_cli$inputfile)) {
                mm_support$loaderror((byte*)"Specify one lead module only",(byte*)"",(byte*)"");
            }
;
            mlib$convlcstring(name);
            mm_cli$inputfile = mlib$pcm_copyheapstring(name);
            if ((mm_decls$passlevel == (i64)13)) {
                mm_winc$cmdskip = ((paramno - (i64)1) + msysc$$cmdskip);
                goto L13 ;
            }
;
        }
        else if ((pmtype==(i64)3)) {
            mm_support$loaderror((byte*)"Lib files go in module headers",(byte*)"",(byte*)"");
        }
        else {
            mm_support$loaderror((byte*)"Invalid params",(byte*)"",(byte*)"");
        }
;
L12 :;
    }
L13 :;
    ;
    if ((mm_decls$prodmode==mm_decls$debugmode && mm_decls$debugmode==(i64)0)) {
        mm_decls$passlevel = (i64)11;
        mm_cli$outext = (byte*)"exe";
        if (!!((i64)mm_decls$flinux)) {
            mm_cli$outext = (byte*)"";
        }
;
        mm_decls$prodmode = (i64)1;
    }
    else if ((!!(mm_decls$prodmode) && (mm_decls$passlevel == (i64)0))) {
        mm_decls$passlevel = (i64)11;
        mm_cli$outext = (byte*)"exe";
        if (!!((i64)mm_decls$flinux)) {
            mm_cli$outext = (byte*)"";
        }
;
    }
    else if ((!!(mm_decls$debugmode) && (mm_decls$passlevel == (i64)0))) {
        mm_decls$passlevel = (i64)8;
        mm_cli$outext = (byte*)"asm";
    }
;
    if (((i64)mm_decls$msyslevel == (i64)-1)) {
        mm_decls$msyslevel = (!!(mm_decls$prodmode) ? (i64)2 : (i64)0);
        mm_decls$msyslevel = (!!(mm_decls$prodmode) ? (i64)2 : (i64)0);
    }
;
    if ((mm_cli$inputfile == 0)) {
        mm_cli$showcaption();
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Usage:",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"\t",NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((*msysc$cmdparams)[((i64)0)],NULL);
        msysc$m_print_str((byte*)"filename[.m]     # Compile project to executable",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"\t",NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((*msysc$cmdparams)[((i64)0)],NULL);
        msysc$m_print_str((byte*)"-help            # Other options",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        exit(0);
    }
    else {
        filename = mm_cli$inputfile;
        mm_decls$outfile = mlib$pcm_copyheapstring(filename);
        if (!!((i64)mm_decls$fwritema)) {
            mm_cli$outext = (byte*)"ma";
        }
;
        if (!!(mm_decls$destfilename)) {
            mm_decls$outfile = mm_decls$destfilename;
        }
        else if (!!(mm_decls$destfilepath)) {
            strcpy((u8 *)filespec,mm_decls$destfilepath);
            strcat(mlib$extractfile((u8 *)filespec),mm_decls$outfile);
            mm_decls$outfile = mlib$pcm_copyheapstring((u8 *)filespec);
        }
;
    }
;
    mm_decls$asmfilename = mm_cli$getoutfilename(mm_decls$outfile,(byte*)"c");
    mm_decls$exefilename = mm_cli$getoutfilename(mm_decls$outfile,(byte*)"exe");
    mm_decls$libfilename = mm_cli$getoutfilename(mm_decls$outfile,(!!(mm_decls$libmode) ? (byte*)"ml" : (byte*)"mx"));
    mm_decls$objfilename = mm_cli$getoutfilename(mm_decls$outfile,(byte*)"obj");
    mm_decls$mafilename = mm_cli$getoutfilename(mm_decls$outfile,(byte*)"ma");
    strcpy(filespec,mlib$changeext(mm_decls$outfile,(byte*)""));
    strcat(filespec,(byte*)"_exp");
    mm_decls$expfilename = mm_cli$getoutfilename(filespec,(byte*)"m");
}

static void mm_cli$do_option(i64 sw,u8 *value) {
        static byte outused;
        static byte outpathused;
    switch (sw) {
    case 1:;
        {
            mm_decls$passlevel = (i64)1;
        }
        break;
    case 2:;
        {
            mm_decls$passlevel = (i64)2;
        }
        break;
    case 4:;
        {
            mm_decls$passlevel = (i64)3;
        }
        break;
    case 3:;
        {
            mm_decls$passlevel = (i64)4;
        }
        break;
    case 5:;
        {
            mm_decls$passlevel = (i64)5;
        }
        break;
    case 6:;
        {
            mm_decls$passlevel = (i64)6;
        }
        break;
    case 7:;
        {
            mm_decls$passlevel = (i64)9;
            mm_cli$outext = (byte*)"asm";
        }
        break;
    case 8:;
        {
            if ((u64)0u) {
                mm_support$loaderror((byte*)"-c not allowed",(byte*)"",(byte*)"");
            }
;
            mm_decls$passlevel = (i64)9;
            mm_cli$outext = (byte*)"c";
        }
        break;
    case 9:;
        {
            mm_decls$passlevel = (i64)8;
            mm_cli$outext = (byte*)"asm";
        }
        break;
    case 10:;
        {
            mm_decls$passlevel = (i64)10;
            mm_cli$outext = (byte*)"obj";
        }
        break;
    case 13:;
        {
            mm_decls$passlevel = (i64)11;
            mm_cli$outext = (byte*)"exe";
        }
        break;
    case 14:;
        {
            mm_decls$passlevel = (i64)12;
            mm_cli$outext = (byte*)"exe";
            mm_decls$mxstub = (i64)1;
        }
        break;
    case 11:;
        {
            mm_decls$passlevel = (i64)12;
            mm_cli$outext = (byte*)"mx";
        }
        break;
    case 12:;
        {
            mm_decls$passlevel = (i64)12;
            mm_cli$outext = (byte*)"ml";
            mm_decls$libmode = (i64)1;
        }
        break;
    case 15:;
        {
            mm_decls$passlevel = (i64)13;
            mm_cli$outext = (byte*)"mem";
        }
        break;
    case 26:;
        {
            mm_decls$fwritema = (i64)1;
            mm_cli$outext = (byte*)"ma";
        }
        break;
    case 27:;
        {
            mm_decls$fwritema = (i64)2;
            mm_cli$outext = (byte*)"ma";
        }
        break;
    case 29:;
        {
            mm_decls$fwriteexports = (i64)1;
        }
        break;
    case 28:;
        {
            mm_decls$fwritedocs = (i64)1;
        }
        break;
    case 30:;
        {
            mm_decls$libmode = (i64)1;
        }
        break;
    case 16:;
        {
            mm_decls$msyslevel = (i64)2;
        }
        break;
    case 17:;
        {
            mm_decls$msyslevel = (i64)1;
        }
        break;
    case 18:;
        {
            mm_decls$msyslevel = (i64)0;
        }
        break;
    case 19:;
        {
            mm_decls$minos = (i64)1;
        }
        break;
    case 20:;
        {
            mm_decls$fnofile = (i64)1;
        }
        break;
    case 31:;
        {
            mm_decls$foptim = (i64)2;
        }
        break;
    case 32:;
        {
            mm_decls$foptim = (i64)1;
        }
        break;
    case 33:;
        {
            mm_decls$foptim = (i64)2;
        }
        break;
    case 21:;
        {
            mm_decls$debugmode = (i64)1;
            mm_decls$prodmode = (i64)0;
        }
        break;
    case 48:;
        {
            mm_decls$fshowtiming = (i64)1;
        }
        break;
    case 49:;
        {
            mm_decls$fverbose = (i64)2;
        }
        break;
    case 50:;
        {
            mm_decls$fverbose = (i64)3;
        }
        break;
    case 51:;
        {
            mm_decls$fverbose = (i64)0;
        }
        break;
    case 52:;
    case 53:;
        {
            mm_cli$showhelp();
            exit(0);
        }
        break;
    case 54:;
        {
            mm_decls$dointlibs = (i64)0;
        }
        break;
    case 55:;
        {
            if (!!((i64)outpathused)) {
                mm_support$loaderror((byte*)"mixed out/path",(byte*)"",(byte*)"");
            }
;
            mm_decls$destfilename = mlib$pcm_copyheapstring(value);
            outused = (i64)1;
        }
        break;
    case 56:;
        {
            if (!!((i64)outused)) {
                mm_support$loaderror((byte*)"mixed out/path",(byte*)"",(byte*)"");
            }
;
            if (!(((u64)(*((value + strlen(value)) - (i64)1)) == (u64)92u || (u64)(*((value + strlen(value)) - (i64)1)) == '/'))) {
                mm_support$loaderror((byte*)"Path needs to end with \\ or /",(byte*)"",(byte*)"");
            }
;
            mm_decls$destfilepath = mlib$pcm_copyheapstring(value);
            outpathused = (i64)1;
        }
        break;
    case 57:;
        {
            mm_decls$fcheckunusedlocals = (i64)1;
        }
        break;
    case 34:;
        {
            mm_decls$fshowast1 = (i64)1;
        }
        break;
    case 35:;
        {
            mm_decls$fshowast2 = (i64)1;
        }
        break;
    case 36:;
        {
            mm_decls$fshowast3 = (i64)1;
        }
        break;
    case 37:;
        {
            mm_decls$fshowmx = (i64)1;
        }
        break;
    case 38:;
        {
            mm_decls$fshowasm = (i64)1;
        }
        break;
    case 39:;
        {
            mm_decls$fshowst = (i64)1;
        }
        break;
    case 41:;
        {
            mm_decls$fshowstflat = (i64)1;
        }
        break;
    case 42:;
        {
            mm_decls$fshowtypes = (i64)1;
        }
        break;
    case 43:;
        {
            mm_decls$fshowoverloads = (i64)1;
        }
        break;
    case 44:;
        {
            mm_decls$fshowss = (i64)1;
        }
        break;
    case 45:;
        {
            mm_decls$fshowmodules = (i64)1;
        }
        break;
    case 46:;
        {
            mm_winc$fshortnames = (i64)1;
        }
        break;
    case 22:;
        {
            mm_decls$ccompiler = (i64)1;
        }
        break;
    case 23:;
        {
            mm_decls$ccompiler = (i64)2;
        }
        break;
    case 24:;
        {
            mm_decls$ccompiler = (i64)3;
        }
        break;
    case 25:;
        {
            mm_decls$ccompiler = (i64)4;
        }
        break;
    case 59:;
        {
            mm_decls$flinux = (i64)1;
            mm_decls$fwindows = (i64)0;
        }
        break;
    case 47:;
        {
            mm_cli$fmodinfo = (i64)1;
        }
        break;
    } //SW
;
}

static void mm_cli$showcaption(void) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"M",NULL);
    msysc$m_print_str((byte*)"Compiler [M6]",NULL);
    msysc$m_print_str((byte*)"17-Dec-2022",NULL);
    msysc$m_print_str((byte*)"21:51:51",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

void mm_cli$showhelp(void) {
        static u8 *  helptext = (byte*)"M Compiler Generating x64 native code - Windows Version\n\nWhole-program compiler builds entire program from the lead module\ninto a executable file.\n\n    mm main              # Create main.exe from lead module main.m\n    mm main.m            # Same (.m extension is default)\n    mm -c main           # Create single-file main.asm intermediate ASM\n\nOptions:\n\n    -exe                 # Generate .exe executable file (default)\n    -dll                 # Generate .dll library and .exp file\n    -pcl                 # Generate intermediate PCL file only\n    -asm                 # Generate intermediate ASM file only\n\n    -opt                 # Apply simple optimiser\n\n    -out:file            # Name of output file \n\n    -ma                  # Create .ma file combining source/support files\n    -docs                # Create .txt with docstrings of exported files (not finished)\n    -run                 # For -exe mode only: run resulting executable\n\n    @file                # Read options from file\n\nExample:\n\n     mm -run prog : abc def\n\nAny parameters for the new program must follow \" : \" (spaces needed).\n";
    msysc$m_print_startcon();
    msysc$m_print_str(helptext,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

void mm_cli$initassemsymbols(void) {
        u8 str[32];
        i64 i;
        static u8 *  regnames[6] = {(byte*)"aframe",(byte*)"dframe",(byte*)"astack",(byte*)"dstack",(byte*)"dprog",(byte*)"dsptr"};
        static byte regnos[6] = {(u8)15u,(u8)15u,(u8)16u,(u8)16u,(u8)9u,(u8)10u};
        static byte sizes[6] = {(u8)4u,(u8)8u,(u8)4u,(u8)8u,(u8)8u,(u8)8u};
    for (i=(i64)1;i<=(i64)150;++i) {
L17 :;
        if ((i != (i64)31)) {
            mm_lex$addreservedword((mc_decls$mclnames[(i)-1] + (i64)2),(i64)92,i,(i64)0);
        }
;
L18 :;
    }
L19 :;
    ;
    for (i=(i64)1;i<=(i64)136;++i) {
L20 :;
        mm_lex$addreservedword(mc_decls$dregnames[(i)-1],(i64)84,(i64)mc_decls$regindices[(i)-1],(i64)mc_decls$regsizes[(i)-1]);
L21 :;
    }
L22 :;
    ;
    for (i=(i64)1;i<=(i64)16;++i) {
L23 :;
        mm_lex$addreservedword(mc_decls$xmmregnames[(i)-1],(i64)85,((i + (i64)21) - (i64)1),(i64)0);
L24 :;
    }
L25 :;
    ;
    for (i=(i64)1;i<=(i64)8;++i) {
L26 :;
        mm_lex$addreservedword(mc_decls$fregnames[(i)-1],(i64)86,i,(i64)0);
L27 :;
    }
L28 :;
    ;
    for (i=(i64)1;i<=(i64)8;++i) {
L29 :;
        mm_lex$addreservedword(mc_decls$mregnames[(i)-1],(i64)87,i,(i64)0);
L30 :;
    }
L31 :;
    ;
    for (i=(i64)1;i<=(i64)18;++i) {
L32 :;
        mm_lex$addreservedword(mc_decls$jmpccnames[(i)-1],(i64)88,(i64)mc_decls$jmpcccodes[(i)-1],(i64)0);
L33 :;
    }
L34 :;
    ;
    for (i=(i64)1;i<=(i64)16;++i) {
L35 :;
        mm_lex$addreservedword(mc_decls$setccnames[(i)-1],(i64)89,(i64)mc_decls$setcccodes[(i)-1],(i64)0);
L36 :;
    }
L37 :;
    ;
    for (i=(i64)1;i<=(i64)16;++i) {
L38 :;
        mm_lex$addreservedword(mc_decls$cmovccnames[(i)-1],(i64)90,(i64)mc_decls$cmovcccodes[(i)-1],(i64)0);
L39 :;
    }
L40 :;
    ;
    for (i=(i64)1;i<=(i64)5;++i) {
L41 :;
        strcpy((u8 *)str,mc_decls$segmentnames[(i)-1]);
        str[((strlen((u8 *)str) - (i64)3))-1] = (u64)0u;
        mm_lex$addreservedword(mlib$pcm_copyheapstring((u8 *)str),(i64)91,i,(i64)0);
L42 :;
    }
L43 :;
    ;
    for (i=(i64)1;i<=(i64)6;++i) {
L44 :;
        mm_lex$addreservedword(regnames[(i)-1],(i64)84,(i64)regnos[(i)-1],(i64)sizes[(i)-1]);
L45 :;
    }
L46 :;
    ;
}

static void mm_cli$do_writeexports(void) {
    if ((!(!!((i64)mm_decls$fwriteexports)) && (mm_decls$passlevel != (i64)12))) {
        return;
    }
;
    if (!(!!(mm_decls$libmode))) {
        return;
    }
;
    mm_export$writeexports(mm_decls$expfilename,mlib$extractbasefile(mm_decls$libfilename));
    if (!!((i64)mm_decls$fwriteexports)) {
        exit(0);
    }
;
}

static u8 *mm_cli$getoutfilename(u8 *file,u8 *ext) {
    return mlib$pcm_copyheapstring(mlib$changeext(file,ext));
}

static void mm_cli$fixstartprocs(void) {
        struct mm_decls$modulerec *  ms;
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  q;
        i64 i;
    for (i=(i64)1;i<=mm_decls$nmodules;++i) {
L47 :;
        ms = (struct mm_decls$modulerec *)&mm_decls$moduletable[(i)];
        if (((*ms).ststart == 0)) {
            (*ms).ststart = mm_cli$addstartproc((*ms).stmodule,(byte*)"start",(i64)2,i);
        }
;
        if (!!((*ms).modulecode)) {
            p = mm_parse$makeblock((*ms).modulecode);
            q = (*(*ms).ststart).code;
            (*p).nextunit = (*q).a;
            (*(*(*ms).ststart).code).a = p;
        }
;
        if ((((i == mm_decls$mainmoduleno) && ((*ms).stmain == 0)) && !!((*ms).modulecode))) {
            (*ms).stmain = mm_cli$addstartproc((*ms).stmodule,(byte*)"main",(i64)3,i);
        }
;
L48 :;
    }
L49 :;
    ;
}

static struct mm_decls$strec *mm_cli$addstartproc(struct mm_decls$strec *owner,u8 *name,i64 scope,i64 moduleno) {
        struct mm_decls$strec *  stproc;
    stproc = mm_lib$getduplnameptr(owner,(struct mm_decls$strec *)mm_lex$addnamestr(name),(i64)6);
    (*stproc).scope = scope;
    (*stproc).moduleno = moduleno;
    (*stproc).subprogno = (i64)mm_decls$moduletosub[(moduleno)];
    (*stproc).code = mm_parse$makeblock(0);
    mm_lib$adddef(owner,stproc);
    mm_lib$addtoproclist(stproc);
    return stproc;
}

static void mm_cli$stepruncount(void) {
        i64 count;
        void *  f;
    f = fopen((byte*)"C:/mx//bcrun.txt",(byte*)"r+");
    if (!(!!(f))) {
        return;
    }
;
    {
        msysc$m_read_fileline(f);
        ;
        count = msysc$m_read_i64((i64)0);
        ;
    }
    fseek(f,(i32)(i64)0,(i32)(i64)0);
    msysc$m_print_startfile(f);
    msysc$m_print_i64((count + (i64)1),NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    fclose(f);
}

void mm_cli$showmoduleinfo(void) {
        void *  f;
        struct mm_decls$modulerec *  pm;
        i64 i;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Writing to",NULL);
    msysc$m_print_str((byte*)"$temp",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    f = fopen((byte*)"$temp",(byte*)"wb");
    if (!!(mm_cli$projectmodule)) {
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"module",NULL);
        msysc$m_print_str(mm_cli$projectmodule,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    for (i=(i64)1;i<=mm_decls$nmodules;++i) {
L50 :;
        pm = (struct mm_decls$modulerec *)&mm_decls$moduletable[(i)];
        msysc$m_print_startfile(f);
        msysc$m_print_setfmt((byte*)"module ##.m");
        msysc$m_print_str((!!((*pm).path) ? (*pm).path : (byte*)"--"),NULL);
        msysc$m_print_str((*pm).name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L51 :;
    }
L52 :;
    ;
    fclose(f);
}

// START
void mm_cli$start(void) {
    mm_assem$start();
    mc_blockc$start();
    mm_decls$start();
    mm_diags$start();
    mm_export$start();
    mc_genc$start();
    mm_lex$start();
    mm_lib$start();
    mc_libc$start();
    mm_libsourcesc$start();
    mm_modules$start();
    mm_name$start();
    mm_parse$start();
    mm_support$start();
    mm_tables$start();
    mm_type$start();
    mm_winc$start();
    mc_decls$start();

}

struct mm_decls$unitrec *mm_assem$readassemline(void) {
    mm_lex$lex();
    return mm_assem$assembleline((i64)1);
}

struct mm_decls$unitrec *mm_assem$readassemblock(void) {
        struct mm_decls$unitrec *  ulist;
        struct mm_decls$unitrec *  ulistx;
        struct mm_decls$unitrec *  u;
    ulist = (ulistx = 0);
    L53 :;
    while (1) {
        mm_lex$lex();
                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)68)) {
            mm_support$serror((byte*)"EOF: 'End' missing in Assembler code");
        }
        else if (($temp==(i64)105)) {
            mm_parse$checkend((i64)mm_decls$lx.symbol,(i64)167,(i64)0,(i64)0);
            mm_lex$lex();
            goto L54 ;
        }
        else if (($temp==(i64)6)) {
        }
        else {
            u = mm_assem$assembleline((i64)0);
            mm_lib$addlistunit(&ulist,&ulistx,u);
        }
        };
    }
L54 :;
    ;
    return mm_parse$makeblock(ulist);
}

static struct mm_decls$unitrec *mm_assem$assembleline(i64 oneline) {
        struct mm_decls$unitrec *  dlist;
        struct mm_decls$unitrec *  dlistx;
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  pname;
        struct mm_decls$unitrec *  q;
        i64 opc;
        i64 noperands;
        struct mm_decls$strec *  stname;
    dlist = (dlistx = 0);
    if ((((i64)mm_decls$lx.symbol == (i64)81) && ((i64)mm_decls$nextlx.symbol == (i64)7 || (i64)mm_decls$nextlx.symbol == (i64)8))) {
        p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)98);
        stname = (struct mm_decls$strec *)mm_lib$getduplnameptr(mm_decls$currproc,(struct mm_decls$strec *)mm_decls$lx.symptr,(i64)17);
        (*p).def = (struct mm_decls$strec *)stname;
        mm_lib$adddef(mm_decls$currproc,(struct mm_decls$strec *)stname);
        mm_lex$lex();
        if (!!(oneline)) {
            mm_lex$lex();
        }
;
        return p;
    }
    else if (((i64)mm_decls$lx.symbol == (i64)33)) {
        mm_parse$lexchecksymbol((i64)81);
        pname = (struct mm_decls$unitrec *)mm_lib$createname((struct mm_decls$strec *)mm_decls$lx.symptr);
        (*pname).pos = (i64)mm_decls$lx.pos;
        mm_lex$lex();
        if (((i64)mm_decls$lx.symbol != (i64)6)) {
            L55 :;
            do {
                mm_lib$addlistunit(&dlist,&dlistx,mm_parse$readunit());
                if (((i64)mm_decls$lx.symbol == (i64)5)) {
                    mm_lex$lex();
                }
;
L56 :;
            }
            while (!((i64)mm_decls$lx.symbol == (i64)6 || (i64)mm_decls$lx.symbol == (i64)68));
L57 :;
            ;
        }
;
        return (struct mm_decls$unitrec *)mm_lib$createunit2((i64)6,(struct mm_decls$unitrec *)pname,(struct mm_decls$unitrec *)dlist);
    }
;
        {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)45)) {
        opc = (i64)40;
        //doop:
L58 :;
;
        p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)5);
        (*p).asmopcode = opc;
        mm_lex$lex();
    }
    else if (($temp==(i64)46)) {
        opc = (i64)41;
        goto L58 ;
;
    }
    else if (($temp==(i64)47)) {
        opc = (i64)42;
        goto L58 ;
;
    }
    else if (($temp==(i64)56)) {
        opc = (i64)53;
        goto L58 ;
;
    }
    else if (($temp==(i64)130)) {
        if (((i64)mm_decls$lx.subcode == (i64)1)) {
            opc = (i64)31;
            goto L58 ;
;
        }
;
        goto L59 ;
;
    }
    else {
        if (((i64)mm_decls$lx.symbol == (i64)81)) {
            p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)5);
                        {i64 $temp = (i64)mm_decls$lx.subcode;
if (($temp==(i64)92)) {
                (*p).asmopcode = (i64)(*mm_decls$lx.symptr).index;
            }
            else if (($temp==(i64)88)) {
                (*p).asmopcode = (i64)28;
                (*p).cond = (i64)(*mm_decls$lx.symptr).index;
            }
            else if (($temp==(i64)89)) {
                (*p).asmopcode = (i64)60;
                (*p).cond = (i64)(*mm_decls$lx.symptr).index;
            }
            else if (($temp==(i64)90)) {
                (*p).asmopcode = (i64)17;
                (*p).cond = (i64)(*mm_decls$lx.symptr).index;
            }
            else {
                mm_lex$ps((byte*)"ASM");
                mm_support$serror((byte*)"x64 op expected");
            }
            };
            mm_lex$lex();
        }
        else {
            //$else:
L59 :;
;
            mm_lex$ps((byte*)"ASM");
            mm_support$serror((byte*)"ASM???");
        }
;
    }
    };
    if (!(((i64)mm_decls$lx.symbol == (i64)6 || (i64)mm_decls$lx.symbol == (i64)68))) {
        noperands = (i64)0;
        L60 :;
        while (1) {
            q = mm_assem$readassemopnd();
            if ((++(noperands) <= (i64)3)) {
                (*p).abc[(noperands)-1] = q;
            }
            else {
                mm_support$serror((byte*)"Too many asm opnds");
            }
;
            if (((i64)mm_decls$lx.symbol != (i64)5)) {
                goto L61 ;
            }
            else {
                mm_lex$lex();
            }
;
        }
L61 :;
        ;
    }
;
    mm_parse$checksymbol((i64)6);
    return p;
}

static struct mm_decls$unitrec *mm_assem$readassemopnd(void) {
        struct mm_decls$unitrec *  p;
        i64 reg;
        i64 regix;
        i64 scale;
        i64 prefixmode;
        struct mm_decls$unitrec *  pcode;
        {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)72) || ($temp==(i64)74)) {
        return mm_parse$readunit();
    }
    else if (($temp==(i64)81)) {
                {i64 $temp = (i64)(*mm_decls$lx.symptr).subcode;
if (($temp==(i64)84)) {
            p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)7);
            (*p).index = (i64)(*mm_decls$lx.symptr).index;
            (*p).regsize = (i64)(*mm_decls$lx.symptr).regsize;
            mm_lex$lex();
            return p;
        }
        else if (($temp==(i64)85)) {
            p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)8);
            (*p).index = (i64)(*mm_decls$lx.symptr).index;
            mm_lex$lex();
            return p;
        }
        };
        return mm_parse$readunit();
    }
    else if (($temp==(i64)31) || ($temp==(i64)32)) {
        return mm_parse$readunit();
    }
    else if (($temp==(i64)93)) {
                {i64 $temp = (i64)mm_decls$lx.subcode;
if (($temp==(i64)17) || ($temp==(i64)18) || ($temp==(i64)19) || ($temp==(i64)2)) {
        }
        else {
            mm_support$serror((byte*)"Bad prefix");
        }
        };
        prefixmode = (i64)mm_decls$lx.subcode;
        mm_parse$lexchecksymbol((i64)15);
        goto L62 ;
;
    }
    else if (($temp==(i64)15)) {
        prefixmode = (i64)0;
        //gotprefix:
L62 :;
;
        reg = (regix = (i64)0);
        pcode = 0;
        scale = (i64)1;
        mm_lex$lex();
        if ((((i64)mm_decls$lx.symbol == (i64)81) && ((i64)(*mm_decls$lx.symptr).subcode == (i64)84))) {
            reg = (i64)(*mm_decls$lx.symptr).index;
            mm_lex$lex();
        }
;
        if (((((i64)mm_decls$lx.symbol == (i64)31) && ((i64)mm_decls$nextlx.symbol == (i64)81)) && ((i64)(*mm_decls$nextlx.symptr).subcode == (i64)84))) {
            mm_lex$lex();
        }
;
        if ((((i64)mm_decls$lx.symbol == (i64)81) && ((i64)(*mm_decls$lx.symptr).subcode == (i64)84))) {
            regix = (i64)(*mm_decls$lx.symptr).index;
            mm_lex$lex();
        }
;
        if (((i64)mm_decls$lx.symbol == (i64)33)) {
            mm_parse$lexchecksymbol((i64)72);
                        {i64 $temp = (scale = mm_decls$lx.value);
if (($temp==(i64)1) || ($temp==(i64)2) || ($temp==(i64)4) || ($temp==(i64)8)) {
            }
            else {
                mm_support$serror((byte*)"Bad scale");
            }
            };
            mm_lex$lex();
        }
;
                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)31) || ($temp==(i64)32) || ($temp==(i64)72) || ($temp==(i64)81) || ($temp==(i64)13) || ($temp==(i64)168)) {
            pcode = mm_parse$readunit();
        }
        };
        mm_parse$checksymbol((i64)16);
        mm_lex$lex();
        p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)9,(struct mm_decls$unitrec *)pcode);
        if (((regix == (i64)0) && (scale > (i64)1))) {
            regix = reg;
            reg = (i64)0;
        }
;
        if (((pcode == 0) && ((reg + regix) == (i64)0))) {
            mm_support$serror((byte*)"Empty []");
        }
;
        (*p).reg = reg;
        (*p).regix = regix;
        (*p).scale = scale;
        (*p).prefixmode = prefixmode;
        return p;
    }
    else {
        mm_lex$ps((byte*)"BAD OPND");
        mm_support$serror((byte*)"ASM: Bad operand?");
    }
    };
    return (struct mm_decls$unitrec *)0;
}

// START
void mm_assem$start(void) {

}

void mc_blockc$evalunit(struct mm_decls$unitrec *p) {
        struct mm_decls$unitrec *  a;
        struct mm_decls$unitrec *  b;
        struct mm_decls$unitrec *  c;
        struct mm_decls$strec *  d;
    if ((p == 0)) {
        return;
    }
;
    mm_tables$mlineno = (i64)(*p).pos;
    a = (*p).a;
    b = (*p).b;
    c = (*p).c;
    switch ((i64)(*p).tag) {
    case 1:;
        {
            mc_blockc$do_const(p);
        }
        break;
    case 3:;
        {
            d = (*p).def;
            if (((i64)(*d).nameid == (i64)17)) {
                if (((i64)(*d).index == (i64)0)) {
                    (*d).index = ++(mc_decls$mlabelno);
                }
;
                mc_libc$ccstr((byte*)"goto ",(i64)0);
                mc_libc$ccstrsemi(mc_libc$genclabel((i64)(*d).index,(i64)0));
            }
            else {
                mc_blockc$applymemmode(p);
                mc_libc$dxstr(mc_libc$getfullnamec(d));
            }
;
        }
        break;
    case 4:;
        {
            mc_blockc$do_block(p,(i64)1);
        }
        break;
    case 87:;
        {
            mc_blockc$do_return(p,a);
        }
        break;
    case 23:;
        {
            mc_blockc$do_assign(p,a,b);
        }
        break;
    case 121:;
        {
            mc_blockc$evalunit(a);
            mc_libc$ccstrline((byte*)";");
        }
        break;
    case 89:;
        {
            mc_blockc$do_to(a,b,c);
        }
        break;
    case 90:;
        {
            if ((!!(mc_blockc$isexpr(p)) && !(!!((i64)(*p).ifretflag)))) {
                mc_blockc$do_ifx(a,b,c,(i64)0);
            }
            else {
                mc_blockc$do_if(a,b,c);
            }
;
        }
        break;
    case 91:;
    case 92:;
        {
            mc_blockc$do_forup(p,a,b,c);
        }
        break;
    case 93:;
        {
            mc_blockc$do_forall(p,a,b,c);
        }
        break;
    case 95:;
        {
            mc_blockc$do_while(a,b,c);
        }
        break;
    case 96:;
        {
            mc_blockc$do_repeat(a,b);
        }
        break;
    case 97:;
        {
            mc_blockc$do_goto(a);
        }
        break;
    case 98:;
        {
            d = (*p).def;
            if (((i64)(*d).index == (i64)0)) {
                (*d).index = ++(mc_decls$mlabelno);
            }
;
            mc_libc$ccstr((byte*)"//",(i64)0);
            mc_libc$ccstr((*d).name,(i64)0);
            mc_libc$ccstrline((byte*)":");
            mc_libc$ccstrline(mc_libc$genclabel((i64)(*d).index,(i64)1));
        }
        break;
    case 99:;
    case 100:;
    case 101:;
        {
            mc_blockc$do_exit(p);
        }
        break;
    case 102:;
        {
            mc_blockc$do_do(a);
        }
        break;
    case 103:;
    case 104:;
        {
            mc_blockc$do_case(p,a,b,c);
        }
        break;
    case 105:;
    case 106:;
        {
            mc_blockc$do_switch(p,a,b,c);
        }
        break;
    case 107:;
        {
            mc_blockc$do_swap(a,b);
        }
        break;
    case 108:;
        {
            mc_blockc$do_select(a,b,c);
        }
        break;
    case 110:;
    case 111:;
    case 112:;
    case 113:;
        {
            mc_blockc$do_print(p,a,b);
        }
        break;
    case 116:;
        {
            mc_blockc$do_read(p,a);
        }
        break;
    case 117:;
        {
            mc_blockc$do_readln(a);
        }
        break;
    case 120:;
        {
            mc_libc$ccstr((byte*)"exit(",(i64)0);
            if (!!(a)) {
                mc_blockc$evalunit(a);
            }
            else {
                mc_libc$dxchar((i64)48);
            }
;
            mc_libc$dxchar((i64)41);
        }
        break;
    case 28:;
    case 86:;
        {
            mc_blockc$do_call(p,a,b,(i64)((i64)(*p).tag == (i64)28));
        }
        break;
    case 31:;
    case 11:;
    case 12:;
    case 29:;
        {
            mc_blockc$do_bin(p,a,b);
        }
        break;
    case 40:;
        {
            mc_blockc$do_index(p,a,b);
        }
        break;
    case 43:;
        {
            mc_blockc$do_supportcall((byte*)"getdotindex",a,b,0,0);
        }
        break;
    case 44:;
        {
            mc_blockc$do_supportcall((byte*)"getdotslice",a,(*b).a,(*b).b,0);
        }
        break;
    case 42:;
        {
            mc_blockc$applymemmode(p);
            mc_blockc$evalunit(a);
            mc_libc$dxchar((i64)46);
            mc_blockc$evalunit(b);
        }
        break;
    case 45:;
        {
            mc_blockc$applymemmode(p);
            if (((i64)(*a).tag == (i64)46)) {
                mc_blockc$evalunit((*a).a);
            }
            else {
                mc_libc$dxstr((byte*)"(*");
                mc_blockc$evalunit(a);
                mc_libc$dxchar((i64)41);
            }
;
        }
        break;
    case 46:;
        {
                        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)45)) {
                mc_blockc$evalunit((*a).a);
            }
            else if (($temp==(i64)90)) {
                mc_blockc$do_ifx((*a).a,(*a).b,(*a).c,(i64)1);
            }
            else if (($temp==(i64)28) || ($temp==(i64)86)) {
                mc_blockc$evalunit(a);
            }
            else {
                mc_libc$dxstr((byte*)"&");
                mc_blockc$evalunit(a);
            }
            };
        }
        break;
    case 47:;
        {
            mc_blockc$evalunit(a);
        }
        break;
    case 48:;
        {
            mc_blockc$do_convert(p,a);
        }
        break;
    case 51:;
        {
            mc_blockc$do_typepun(p,a);
        }
        break;
    case 49:;
        {
            mc_libc$dxstr((byte*)"(");
            mc_libc$dxstr(mc_libc$strmodec((i64)(*p).mode,(byte*)"",(i64)1));
            mc_libc$dxstr((byte*)")");
            mc_blockc$evalunit(a);
        }
        break;
    case 32:;
    case 13:;
        {
            mc_blockc$do_unary(p,a);
        }
        break;
    case 14:;
        {
            mc_libc$dxstr((byte*)"!!(");
            mc_blockc$evalunit(a);
            mc_libc$dxstr((byte*)")");
        }
        break;
    case 35:;
        {
                        {i64 $temp = (i64)(*p).pclop;
if (($temp==(i64)53) || ($temp==(i64)55)) {
                mc_libc$ccstr((byte*)"++(",(i64)0);
                mc_blockc$evalunit(a);
                mc_libc$ccstr((byte*)")",(i64)0);
            }
            else if (($temp==(i64)54) || ($temp==(i64)56)) {
                mc_libc$ccstr((byte*)"--(",(i64)0);
                mc_blockc$evalunit(a);
                mc_libc$ccstr((byte*)")",(i64)0);
            }
            else if (($temp==(i64)57)) {
                mc_libc$ccstr((byte*)"(",(i64)0);
                mc_blockc$evalunit(a);
                mc_libc$ccstr((byte*)")++",(i64)0);
            }
            else {
                mc_libc$ccstr((byte*)"(",(i64)0);
                mc_blockc$evalunit(a);
                mc_libc$ccstr((byte*)")--",(i64)0);
            }
            };
        }
        break;
    case 33:;
        {
            mc_blockc$do_binto(p,a,b);
        }
        break;
    case 88:;
        {
            mc_libc$ccstr((byte*)"msysc$m_",(i64)0);
            mc_libc$ccstr((mm_tables$sysfnnames[((i64)(*p).fnindex)-1] + (i64)3),(i64)0);
            mc_libc$ccchar((i64)40);
            if (!!(a)) {
                mc_blockc$evalunit(a);
            }
;
            mc_libc$ccchar((i64)41);
        }
        break;
    case 62:;
        {
            mc_libc$ccint(mm_support$getlineno((u64)mm_tables$mlineno));
        }
        break;
    case 122:;
        {
            mc_libc$dxstr((byte*)"memset(&(");
            mc_blockc$evalunit(a);
            mc_libc$dxstr((byte*)"),0,");
            mc_libc$dxint((i64)mm_decls$ttsize[((i64)(*a).mode)]);
            mc_libc$dxchar((i64)41);
        }
        break;
    case 123:;
        {
            mc_libc$ccstr((*p).svalue,(i64)0);
        }
        break;
    case 37:;
        {
            mc_blockc$do_inrange(a,(*b).a,(*b).b);
        }
        break;
    case 38:;
        {
            mc_blockc$do_inset(a,(*b).a);
        }
        break;
    case 30:;
        {
            mc_blockc$do_cmpchain(p);
        }
        break;
    case 124:;
        {
            mc_libc$ccstr((byte*)"m$infinity",(i64)0);
        }
        break;
    default: {
        mm_support$gerror_s((byte*)"Evalunit: ",mm_tables$jtagnames[((i64)(*p).tag)],0);
    }
    } //SW
;
}

void mc_blockc$evalstmt(struct mm_decls$unitrec *p) {
    mc_libc$cctab(mc_blockc$blocklevel);
    mc_blockc$evalunit(p);
    if (((i64)(*p).tag != (i64)4)) {
        mc_libc$ccstrline((byte*)";");
    }
;
}

void mc_blockc$evalunitc(struct mm_decls$unitrec *p) {
        struct mm_decls$unitrec *  q;
    if (((i64)(*p).tag != (i64)4)) {
        mc_blockc$evalunit(p);
        return;
    }
;
    mc_libc$dxchar((i64)40);
    q = (*p).a;
    L63 :;
    while (!!(q)) {
        if ((q != (*p).a)) {
            mc_libc$dxstr((byte*)", ");
        }
;
        mc_blockc$evalunit(q);
L64 :;
        q = (*q).nextunit;
L66 :;
            }
L65 :;
    ;
    mc_libc$dxchar((i64)41);
}

static void mc_blockc$evalblock(struct mm_decls$unitrec *p,i64 braces) {
        struct mm_decls$unitrec r;
        struct mm_decls$unitrec *  pnext;
    if (((i64)(*p).tag == (i64)4)) {
        mc_blockc$do_block(p,braces);
    }
    else {
        r.tag = (i64)4;
        r.a = p;
        pnext = (*p).nextunit;
        (*p).nextunit = 0;
        mc_blockc$do_block(&r,braces);
        (*p).nextunit = pnext;
    }
;
}

void mc_blockc$do_block(struct mm_decls$unitrec *p,i64 braces) {
        struct mm_decls$unitrec *  a;
    a = (*p).a;
    ++(mc_blockc$blocklevel);
    if (!!(braces)) {
        mc_libc$ccstrline((byte*)"{");
    }
;
    L67 :;
    while (!!(a)) {
        mc_blockc$evalstmt(a);
L68 :;
        a = (*a).nextunit;
L70 :;
            }
L69 :;
    ;
    --(mc_blockc$blocklevel);
    if (!!(braces)) {
        mc_libc$cctab(mc_blockc$blocklevel);
        mc_libc$ccstrline((byte*)"}");
    }
;
}

void mc_blockc$evalblocklab(struct mm_decls$unitrec *p,i64 lab1,i64 lab2,struct mm_decls$unitrec *pincr,i64 labincr,i64 braces) {
        struct mm_decls$unitrec r;
        struct mm_decls$unitrec *  pnext;
    if (((i64)(*p).tag == (i64)4)) {
        mc_blockc$do_blocklab(p,lab1,lab2,pincr,labincr,braces);
    }
    else {
        r.tag = (i64)4;
        r.a = p;
        pnext = (*p).nextunit;
        (*p).nextunit = 0;
        mc_blockc$do_blocklab(&r,lab1,lab2,pincr,labincr,braces);
        (*p).nextunit = pnext;
    }
;
}

void mc_blockc$do_blocklab(struct mm_decls$unitrec *p,i64 lab1,i64 lab2,struct mm_decls$unitrec *pincr,i64 labincr,i64 braces) {
        struct mm_decls$unitrec *  a;
    a = (*p).a;
    if (!!(braces)) {
        mc_libc$ccstrline((byte*)"{");
    }
;
    if (!!(lab1)) {
        mc_blockc$dxlabel(lab1);
    }
;
    ++(mc_blockc$blocklevel);
    L71 :;
    while (!!(a)) {
        mc_blockc$evalstmt(a);
        a = (*a).nextunit;
L72 :;
    }
L73 :;
    ;
    if (!!(lab2)) {
        mc_blockc$dxlabel(lab2);
    }
;
    if (!!(pincr)) {
        mc_blockc$evalstmt(pincr);
        mc_blockc$definefwdlabel(labincr);
    }
;
    --(mc_blockc$blocklevel);
    if (!!(braces)) {
        mc_libc$cctab(mc_blockc$blocklevel);
        mc_libc$ccstrline((byte*)"}");
    }
;
}

static void mc_blockc$do_const(struct mm_decls$unitrec *p) {
        u8 str[256];
        i64 a;
        u64 u;
        i64 m;
    str[((i64)1)-1] = (u64)0u;
    m = (i64)(*p).mode;
    if (!!((i64)mm_decls$ttsigned[(m)])) {
        a = (*p).value;
                {i64 $temp = (i64)mm_decls$ttbasetype[((i64)(*p).mode)];
if (($temp==(i64)14)) {
            msysc$m_print_startstr(str);
            msysc$m_print_str((byte*)"(i8)",NULL);
            msysc$m_print_nogap();
            msysc$m_print_i64(a,NULL);
            msysc$m_print_end();
            ;
        }
        else if (($temp==(i64)15)) {
            msysc$m_print_startstr(str);
            msysc$m_print_str((byte*)"(i16)",NULL);
            msysc$m_print_nogap();
            msysc$m_print_i64(a,NULL);
            msysc$m_print_end();
            ;
        }
        else if (($temp==(i64)16)) {
            if ((a == (i64)-2147483648)) {
                mc_libc$dxstr((byte*)"(int32_t)(-2147483647-1)");
            }
            else {
                msysc$m_print_startstr(str);
                msysc$m_print_str((byte*)"(int32_t)",NULL);
                msysc$m_print_nogap();
                msysc$m_print_i64(a,NULL);
                msysc$m_print_end();
                ;
            }
;
        }
        else if (($temp==(i64)3)) {
            if ((a == (i64)(-9223372036854775807-1))) {
                mc_libc$dxstr((byte*)"(i64)(-9223372036854775807-1)");
            }
            else {
                msysc$m_print_startstr(str);
                msysc$m_print_setfmt((byte*)"(i64)");
                msysc$m_print_i64(a,NULL);
                msysc$m_print_end();
                ;
            }
;
        }
        else {
            mm_support$gerror((byte*)"CONST/INT",0);
        }
        };
    }
    else if ((!!((i64)mm_decls$ttisinteger[(m)]) || (m == (i64)6))) {
        u = (*p).uvalue;
                {i64 $temp = (i64)mm_decls$ttbasetype[((i64)(*p).mode)];
if (($temp==(i64)17)) {
            msysc$m_print_startstr(str);
            msysc$m_print_setfmt((byte*)"(u8)#u");
            msysc$m_print_u64(u,NULL);
            msysc$m_print_end();
            ;
        }
        else if (($temp==(i64)18)) {
            msysc$m_print_startstr(str);
            msysc$m_print_setfmt((byte*)"(u16)#u");
            msysc$m_print_u64(u,NULL);
            msysc$m_print_end();
            ;
        }
        else if (($temp==(i64)19)) {
            msysc$m_print_startstr(str);
            msysc$m_print_setfmt((byte*)"(u32)#u");
            msysc$m_print_u64(u,NULL);
            msysc$m_print_end();
            ;
        }
        else if (($temp==(i64)2) || ($temp==(i64)6)) {
            msysc$m_print_startstr(str);
            msysc$m_print_setfmt((byte*)"(u64)#u");
            msysc$m_print_u64(u,NULL);
            msysc$m_print_end();
            ;
        }
        else if (($temp==(i64)12) || ($temp==(i64)1)) {
            if ((((((i64)u < (i64)32) || ((i64)u > (i64)126)) || (u == (u64)92u)) || (u == (u64)39u))) {
                msysc$m_print_startstr(str);
                msysc$m_print_setfmt((byte*)"(u64)#u");
                msysc$m_print_u64(u,NULL);
                msysc$m_print_end();
                ;
            }
            else {
                msysc$m_print_startstr(str);
                msysc$m_print_setfmt((byte*)"'#'");
                msysc$m_print_u64(u,(byte*)"c");
                msysc$m_print_end();
                ;
            }
;
        }
        else {
            mm_support$gerror((byte*)"const/word",0);
        }
        };
    }
    else if (!!((i64)mm_decls$ttisref[(m)])) {
        if (((((i64)(*p).tag == (i64)1) && ((i64)(*p).mode == (i64)20)) && !!((i64)(*p).isastring))) {
            if (!!((*p).svalue)) {
                mc_libc$cclongstr((*p).svalue,(i64)(*p).slength);
                return;
            }
            else {
                strcpy((u8 *)str,(byte*)"0");
            }
;
        }
        else {
            msysc$m_print_startstr(str);
            msysc$m_print_i64((*p).value,(byte*)"H");
            msysc$m_print_end();
            ;
        }
;
    }
    else if (!!((i64)mm_decls$ttisreal[(m)])) {
        if (((i64)mm_decls$ttbasetype[((i64)(*p).mode)] == (i64)5)) {
            msysc$m_print_startstr(str);
            msysc$m_print_setfmt((byte*)"(double)");
            msysc$m_print_r64((*p).xvalue,(byte*)".20g");
            msysc$m_print_end();
            ;
            if ((strchr((u8 *)str,(i32)'.') == 0)) {
                strcat((u8 *)str,(byte*)".");
            }
;
        }
        else {
            msysc$m_print_startstr(str);
            msysc$m_print_str((byte*)"(float)",NULL);
            msysc$m_print_nogap();
            msysc$m_print_r64((*p).xvalue,NULL);
            msysc$m_print_end();
            ;
        }
;
    }
    else {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"STRMODE(M)=",NULL);
        msysc$m_print_str(mm_lib$strmode(m,(i64)1),NULL);
        msysc$m_print_i64((i64)mm_decls$ttisref[(m)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        mm_support$gerror((byte*)"doconst",0);
    }
;
    mc_libc$dxstr((u8 *)str);
}

static void mc_blockc$do_return(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a) {
    if (!(!!(a))) {
        mc_libc$ccstr((byte*)"return",(i64)0);
    }
    else {
        if ((((i64)(*a).tag == (i64)48) && ((i64)(*(*a).a).tag == (i64)4))) {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"RETURN CONV BLOCK",NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
;
        mc_libc$ccstr((byte*)"return ",(i64)0);
        mc_blockc$evalunit(a);
    }
;
}

static void mc_blockc$do_assign(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)43)) {
        mc_blockc$evalunit((*a).a);
        mc_libc$dxstr((byte*)" = ");
        mc_blockc$do_supportcall((byte*)"setdotindex",(*a).a,(*a).b,b,0);
    }
    else if (($temp==(i64)44)) {
        mc_blockc$evalunit((*a).a);
        mc_libc$dxstr((byte*)" = ");
        mc_blockc$do_supportcall((byte*)"setdotslice",(*a).a,(*(*a).b).a,(*(*a).b).b,b);
    }
    else {
        if (((i64)mm_decls$ttbasetype[((i64)(*a).mode)] == (i64)10)) {
            if (!!((i64)(*p).resultflag)) {
                mm_support$gerror((byte*)"Block assign returning value?",0);
            }
;
            mc_blockc$do_blockcopy(a,b);
        }
        else {
            if (!!((i64)(*p).resultflag)) {
                mc_libc$dxchar((i64)40);
            }
;
            //normalassign:
L74 :;
;
            mc_blockc$evalunit(a);
            mc_libc$dxstr((byte*)" = ");
            mc_blockc$evalunit(b);
            if (!!((i64)(*p).resultflag)) {
                mc_libc$dxchar((i64)41);
            }
;
        }
;
    }
    };
}

static void mc_blockc$do_bin(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
        u8 *  opstr;
        {i64 $temp = (i64)(*p).pclop;
if (($temp==(i64)15) || ($temp==(i64)16)) {
        mc_blockc$do_max(p,a,b);
        return;
    }
    else if (($temp==(i64)51)) {
        if (!!((i64)mm_decls$ttisinteger[((i64)(*a).mode)])) {
            mc_blockc$do_supportcall((byte*)"power_i64",b,a,0,0);
        }
        else {
            mc_blockc$domaths2((byte*)"pow",a,b);
        }
;
        return;
    }
    };
    mc_libc$dxchar((i64)40);
    mc_blockc$evalunit(a);
    mc_libc$dxchar((i64)32);
    opstr = mm_tables$ccopnames[((i64)(*p).pclop)];
    if ((opstr == 0)) {
        mm_support$gerror_s((byte*)"No C binop:",mm_tables$pclnames[((i64)(*p).pclop)],0);
    }
;
    mc_libc$dxstr(opstr);
    mc_libc$dxchar((i64)32);
    mc_blockc$evalunit(b);
    mc_libc$dxchar((i64)41);
}

static void mc_blockc$do_binto(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
        u8 *  opstr;
        {i64 $temp = (i64)(*p).pclop;
if (($temp==(i64)70) || ($temp==(i64)71)) {
        mc_blockc$do_maxto(p,a,b);
        return;
    }
    };
    mc_blockc$evalunit(a);
    opstr = mm_tables$ccopnames[((i64)(*p).pclop)];
    if ((opstr == 0)) {
        mm_support$gerror_s((byte*)"No C bintoop:",mm_tables$pclnames[((i64)(*p).pclop)],0);
    }
;
    mc_libc$dxchar((i64)32);
    mc_libc$dxstr(opstr);
    mc_libc$dxchar((i64)32);
    mc_blockc$evalunit(b);
}

static void mc_blockc$do_unary(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a) {
        u8 *  opstr;
        {i64 $temp = (i64)(*p).pclop;
if (($temp==(i64)30)) {
        if (!!((i64)mm_decls$ttisreal[((i64)(*a).mode)])) {
            opstr = (byte*)"fabs";
        }
        else {
            opstr = (byte*)"m$llabs";
        }
;
    }
    else if (($temp==(i64)34)) {
        mc_libc$dxchar((i64)40);
        mc_blockc$evalunit(a);
        mc_libc$dxchar((i64)42);
        mc_blockc$evalunit(a);
        mc_libc$dxchar((i64)41);
        return;
    }
    else if (($temp==(i64)35) || ($temp==(i64)36) || ($temp==(i64)37) || ($temp==(i64)38) || ($temp==(i64)39) || ($temp==(i64)40) || ($temp==(i64)41) || ($temp==(i64)44) || ($temp==(i64)46) || ($temp==(i64)47)) {
        opstr = (mm_tables$pclnames[((i64)(*p).pclop)] + (i64)1);
    }
    else if (($temp==(i64)42)) {
        opstr = (byte*)"log";
    }
    else if (($temp==(i64)43)) {
        opstr = (byte*)"log10";
    }
    else if (($temp==(i64)88)) {
        mc_blockc$evalunit(a);
        mc_libc$dxstr((byte*)".len");
        return;
    }
    else if (($temp==(i64)104)) {
        mc_libc$dxstr((byte*)"((");
        mc_libc$dxstr(mc_libc$strmodec((i64)mm_decls$tttarget[((i64)(*a).mode)],(byte*)"",(i64)1));
        mc_libc$dxstr((byte*)"*)");
        mc_blockc$evalunit(a);
        mc_libc$ccstr((byte*)".ptr)",(i64)0);
        return;
    }
    else {
        opstr = mm_tables$ccopnames[((i64)(*p).pclop)];
        if ((opstr == 0)) {
            mm_support$gerror_s((byte*)"No C unaryop:",mm_tables$pclnames[((i64)(*p).pclop)],0);
        }
;
    }
    };
    mc_libc$dxstr(opstr);
    mc_libc$dxchar((i64)40);
    mc_blockc$evalunit(a);
    mc_libc$dxchar((i64)41);
}

static void mc_blockc$do_convert(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a) {
        u8 str[100];
        i64 oldmode;
        i64 newmode;
    oldmode = (i64)(*p).convmode;
    newmode = (i64)(*p).mode;
        {i64 $temp = (i64)(*p).pclop;
if (($temp==(i64)83) || ($temp==(i64)84) || ($temp==(i64)86) || ($temp==(i64)87) || ($temp==(i64)82)) {
        //docast:
L75 :;
;
        mc_libc$dxstr((byte*)"(");
        mc_libc$dxstr(mc_libc$strmodec(newmode,(byte*)"",(i64)1));
        mc_libc$dxstr((byte*)")");
        mc_blockc$evalunit(a);
    }
    else if (($temp==(i64)85)) {
        mc_libc$dxchar((i64)40);
        mc_libc$dxstr(mc_libc$strmodec(newmode,(byte*)"",(i64)1));
        mc_libc$dxchar((i64)41);
        mc_libc$dxchar((i64)40);
        mc_libc$dxstr(mc_libc$strmodec(oldmode,(byte*)"",(i64)1));
        mc_libc$dxchar((i64)41);
        mc_blockc$evalunit(a);
    }
    else {
        //error:
L76 :;
;
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"#-># (#)");
        msysc$m_print_str(mm_lib$strmode(oldmode,(i64)1),NULL);
        msysc$m_print_str(mm_lib$strmode(newmode,(i64)1),NULL);
        msysc$m_print_str(mm_tables$pclnames[((i64)(*p).pclop)],NULL);
        msysc$m_print_end();
        ;
        mm_support$gerror_s((byte*)"Convert ",str,0);
    }
    };
}

static void mc_blockc$do_if(struct mm_decls$unitrec *pcond,struct mm_decls$unitrec *plist,struct mm_decls$unitrec *pelse) {
        struct mm_decls$unitrec *  pcond0;
    pcond0 = pcond;
    L77 :;
    while (!!(pcond)) {
        if ((pcond == pcond0)) {
            mc_libc$ccstr((byte*)"if (",(i64)0);
        }
        else {
            mc_libc$ccstr((byte*)"else if (",mc_blockc$blocklevel);
        }
;
        mc_blockc$evalunit(pcond);
        mc_libc$ccstr((byte*)") ",(i64)0);
        mc_blockc$evalblock(plist,(i64)1);
L78 :;
        {
            pcond = (*pcond).nextunit;
            plist = (*plist).nextunit;
        }
L80 :;
            }
L79 :;
    ;
    if (!!(pelse)) {
        mc_libc$ccstr((byte*)"else ",mc_blockc$blocklevel);
        mc_blockc$evalblock(pelse,(i64)1);
    }
;
}

static void mc_blockc$do_ifx(struct mm_decls$unitrec *pcond,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,i64 addrof) {
        struct mm_decls$unitrec *  pcond0;
    pcond0 = pcond;
    mc_libc$dxchar((i64)40);
    mc_blockc$evalunit(pcond);
    mc_libc$dxstr((byte*)" ? ");
    if (!!(addrof)) {
        mc_libc$dxchar((i64)38);
    }
;
    if (((i64)(*a).tag == (i64)48)) {
        a = (*a).a;
    }
;
    mc_blockc$evalunit(a);
    mc_libc$dxstr((byte*)" : ");
    if (!!(addrof)) {
        mc_libc$dxchar((i64)38);
    }
;
    if (((i64)(*b).tag == (i64)48)) {
        b = (*b).a;
    }
;
    mc_blockc$evalunit(b);
    mc_libc$dxchar((i64)41);
}

static void mc_blockc$do_call(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,i64 isfn) {
        struct mm_decls$unitrec *  q;
    if (((i64)(*a).tag == (i64)3)) {
        mc_libc$dxstr(mc_libc$getprocname((*a).def));
    }
    else {
        mc_libc$dxchar((i64)40);
        mc_blockc$evalunit(a);
        mc_libc$dxchar((i64)41);
    }
;
    mc_libc$dxchar((i64)40);
    q = b;
    L81 :;
    while (!!(q)) {
        mc_blockc$evalunit(q);
        q = (*q).nextunit;
        if (!!(q)) {
            mc_libc$dxchar((i64)44);
        }
;
L82 :;
    }
L83 :;
    ;
    mc_libc$dxchar((i64)41);
}

static void mc_blockc$do_do(struct mm_decls$unitrec *a) {
        i64 lab_abc;
        i64 lab_d;
    lab_abc = mc_blockc$definelabel();
    lab_d = mc_blockc$createfwdlabel();
    mc_blockc$stacklooplabels(lab_abc,lab_abc,lab_d);
    mc_libc$ccstr((byte*)"while (1) ",(i64)0);
    mc_blockc$evalblock(a,(i64)1);
    mc_blockc$definefwdlabel(lab_d);
    mc_blockc$unstacklooplabels();
}

static i64 mc_blockc$definelabel(void) {
    ++(mc_decls$mlabelno);
    mc_libc$ccstr(mc_libc$genclabel(mc_decls$mlabelno,(i64)1),(i64)0);
    mc_libc$ccsendline();
    mc_libc$cctab(mc_blockc$blocklevel);
    return mc_decls$mlabelno;
}

static i64 mc_blockc$createfwdlabel(void) {
    return ++(mc_decls$mlabelno);
}

static void mc_blockc$definefwdlabel(i64 lab) {
    mc_libc$ccstr(mc_libc$genclabel(lab,(i64)1),(i64)0);
    mc_libc$ccsendline();
    mc_libc$cctab(mc_blockc$blocklevel);
}

static void mc_blockc$stacklooplabels(i64 a,i64 b,i64 c) {
    ++(mc_blockc$loopindex);
    if ((mc_blockc$loopindex > (i64)50)) {
        mm_support$gerror((byte*)"Too many nested loops",0);
    }
;
    mc_blockc$loopstack[(mc_blockc$loopindex)-1][((i64)1)-1] = a;
    mc_blockc$loopstack[(mc_blockc$loopindex)-1][((i64)2)-1] = b;
    mc_blockc$loopstack[(mc_blockc$loopindex)-1][((i64)3)-1] = c;
}

static void mc_blockc$unstacklooplabels(void) {
    --(mc_blockc$loopindex);
}

static i64 mc_blockc$findlooplabel(i64 k,i64 n) {
        i64 i;
    i = (mc_blockc$loopindex - (n - (i64)1));
    if (((i < (i64)1) || (i > mc_blockc$loopindex))) {
        mm_support$gerror((byte*)"Bad loop index",0);
    }
;
    return mc_blockc$loopstack[(i)-1][(k)-1];
}

static void mc_blockc$do_exit(struct mm_decls$unitrec *p) {
        i64 k;
        i64 n;
        i64 index;
    switch ((i64)(*p).tag) {
    case 99:;
        {
            k = (i64)1;
        }
        break;
    case 100:;
        {
            k = (i64)2;
        }
        break;
    case 101:;
        {
            k = (i64)3;
        }
        break;
    } //SW
;
    index = (i64)(*p).index;
    if ((index == (i64)0)) {
        index = mc_blockc$loopindex;
    }
;
    n = mc_blockc$findlooplabel(k,index);
    if ((n == (i64)0)) {
        mm_support$gerror((byte*)"Bad exit/loop index",p);
    }
    else {
        mc_libc$ccstr((byte*)"goto ",(i64)0);
        mc_libc$ccstr(mc_libc$genclabel(n,(i64)0),(i64)0);
    }
;
}

static void mc_blockc$do_to(struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,struct mm_decls$unitrec *c) {
        i64 lab_b;
        i64 lab_c;
        i64 lab_d;
    mc_libc$ccstr(mc_libc$getfullnamec((*c).def),(i64)0);
    mc_libc$ccstr((byte*)" = ",(i64)0);
    mc_libc$ccstrsemiu(a);
    lab_b = mc_blockc$createfwdlabel();
    lab_c = mc_blockc$createfwdlabel();
    lab_d = mc_blockc$createfwdlabel();
    mc_blockc$stacklooplabels(lab_b,lab_c,lab_d);
    mc_libc$cctab(mc_blockc$blocklevel);
    mc_libc$ccstr((byte*)"while (",(i64)0);
    mc_libc$ccstr(mc_libc$getfullnamec((*c).def),(i64)0);
    mc_libc$ccstr((byte*)"-- > 0) ",(i64)0);
    mc_blockc$evalblocklab(b,lab_b,lab_c,0,(i64)0,(i64)1);
    mc_blockc$definefwdlabel(lab_d);
    mc_blockc$unstacklooplabels();
}

static void mc_blockc$do_while(struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,struct mm_decls$unitrec *pincr) {
        i64 lab_b;
        i64 lab_c;
        i64 lab_d;
        i64 lab_incr;
    lab_b = mc_blockc$definelabel();
    lab_c = mc_blockc$createfwdlabel();
    lab_d = mc_blockc$createfwdlabel();
    if (!!(pincr)) {
        lab_incr = mc_blockc$createfwdlabel();
    }
    else {
        lab_incr = lab_c;
    }
;
    mc_blockc$stacklooplabels(lab_b,lab_c,lab_d);
    mc_libc$ccstr((byte*)"while (",(i64)0);
    mc_blockc$evalunit(a);
    mc_libc$ccstr((byte*)") ",(i64)0);
    mc_blockc$evalblocklab(b,(i64)0,lab_c,pincr,lab_incr,(i64)1);
    mc_blockc$definefwdlabel(lab_d);
    mc_blockc$unstacklooplabels();
}

static void mc_blockc$do_repeat(struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
        i64 lab_ab;
        i64 lab_c;
        i64 lab_d;
    lab_ab = mc_blockc$definelabel();
    lab_c = mc_blockc$createfwdlabel();
    lab_d = mc_blockc$createfwdlabel();
    mc_blockc$stacklooplabels(lab_ab,lab_c,lab_d);
    mc_libc$ccstr((byte*)"do ",(i64)0);
    mc_blockc$evalblocklab(a,(i64)0,lab_c,0,(i64)0,(i64)1);
    mc_libc$ccstr((byte*)"while (!",mc_blockc$blocklevel);
    mc_blockc$evalunit(b);
    mc_libc$ccstrline((byte*)");");
    mc_blockc$definefwdlabel(lab_d);
    mc_blockc$unstacklooplabels();
}

static void mc_blockc$do_forup(struct mm_decls$unitrec *p,struct mm_decls$unitrec *pindex,struct mm_decls$unitrec *pfrom,struct mm_decls$unitrec *pbody) {
        i64 lab_b;
        i64 lab_c;
        i64 lab_d;
        i64 down;
        struct mm_decls$unitrec *  pto;
        struct mm_decls$unitrec *  pstep;
        struct mm_decls$unitrec *  pelse;
        struct mm_decls$unitrec *  px;
        struct mm_decls$unitrec *  ptoinit;
        u8 varstr[512];
        struct mm_decls$strec *  d;
    down = (i64)((i64)(*p).tag == (i64)92);
    pto = (*pfrom).nextunit;
    pstep = (*pto).nextunit;
    pelse = (*pbody).nextunit;
    ptoinit = (*pindex).nextunit;
    if (((i64)(*pto).tag == (i64)45)) {
        px = (*pto).a;
        if (((((i64)(*px).tag == (i64)3) && ((i64)(*(d = (*px).def)).nameid == (i64)13)) && ((i64)(*d).parammode == (i64)2))) {
            mm_support$gerror((byte*)"Possibly using &param as for-loop limit",0);
        }
;
    }
;
    lab_b = mc_blockc$createfwdlabel();
    lab_c = mc_blockc$createfwdlabel();
    lab_d = mc_blockc$createfwdlabel();
    mc_blockc$stacklooplabels(lab_b,lab_c,lab_d);
    if (((i64)(*pindex).tag != (i64)3)) {
        mm_support$gerror((byte*)"for/name",0);
    }
;
    if (!!(ptoinit)) {
        mc_blockc$evalstmt(ptoinit);
        mc_libc$cctab(mc_blockc$blocklevel);
    }
;
    strcpy((u8 *)varstr,mc_libc$getfullnamec((*pindex).def));
    mc_libc$ccstr((byte*)"for (",(i64)0);
    mc_libc$ccstr((u8 *)varstr,(i64)0);
    mc_libc$ccchar((i64)61);
    mc_blockc$evalunit(pfrom);
    mc_libc$ccchar((i64)59);
    mc_libc$ccstr((u8 *)varstr,(i64)0);
    mc_libc$ccstr((!!(down) ? (byte*)">=" : (byte*)"<="),(i64)0);
    mc_blockc$evalunit(pto);
    mc_libc$ccchar((i64)59);
    if (!!(pstep)) {
        mc_libc$ccstr((u8 *)varstr,(i64)0);
        mc_libc$ccstr((!!(down) ? (byte*)"-=" : (byte*)"+="),(i64)0);
        mc_blockc$evalunit(pstep);
    }
    else {
        mc_libc$ccstr((!!(down) ? (byte*)"--" : (byte*)"++"),(i64)0);
        mc_libc$ccstr((u8 *)varstr,(i64)0);
    }
;
    mc_libc$ccstr((byte*)") ",(i64)0);
    mc_blockc$evalblocklab(pbody,lab_b,lab_c,0,(i64)0,(i64)1);
    if (!!(pelse)) {
        mc_libc$cctab(mc_blockc$blocklevel);
        mc_blockc$evalblock(pelse,(i64)1);
    }
;
    mc_blockc$definefwdlabel(lab_d);
    mc_blockc$unstacklooplabels();
}

static void mc_blockc$do_forall(struct mm_decls$unitrec *p,struct mm_decls$unitrec *pindex,struct mm_decls$unitrec *plist,struct mm_decls$unitrec *pbody) {
        struct mm_decls$unitrec *  plocal;
        struct mm_decls$unitrec *  pfrom;
        struct mm_decls$unitrec *  pto;
        struct mm_decls$unitrec *  pelse;
        struct mm_decls$unitrec *  passign;
        i64 lab_b;
        i64 lab_c;
        i64 lab_d;
        i64 lab_e;
        i64 lab_cmp;
        i64 down;
        u8 varstr[512];
    down = (i64)0;
    plocal = (*pindex).nextunit;
    pfrom = (*plocal).nextunit;
    pto = (*pfrom).nextunit;
    passign = (*plist).nextunit;
    pelse = (*pbody).nextunit;
    lab_b = mc_blockc$createfwdlabel();
    lab_c = mc_blockc$createfwdlabel();
    lab_d = mc_blockc$createfwdlabel();
    lab_cmp = mc_blockc$createfwdlabel();
    if (!!(pelse)) {
        lab_e = mc_blockc$createfwdlabel();
    }
    else {
        lab_e = lab_d;
    }
;
    mc_blockc$stacklooplabels(lab_b,lab_c,lab_d);
    strcpy((u8 *)varstr,mc_libc$getfullnamec((*pindex).def));
    mc_libc$ccstr((byte*)"for (",(i64)0);
    mc_libc$ccstr((u8 *)varstr,(i64)0);
    mc_libc$ccchar((i64)61);
    mc_blockc$evalunit(pfrom);
    mc_libc$ccchar((i64)59);
    mc_libc$ccstr((u8 *)varstr,(i64)0);
    mc_libc$ccstr((!!(down) ? (byte*)">=" : (byte*)"<="),(i64)0);
    mc_blockc$evalunit(pto);
    mc_libc$ccchar((i64)59);
    mc_libc$ccstr((!!(down) ? (byte*)"--" : (byte*)"++"),(i64)0);
    mc_libc$ccstr((u8 *)varstr,(i64)0);
    mc_libc$ccstrline((byte*)") {");
    mc_blockc$evalstmt(passign);
    mc_blockc$evalblocklab(pbody,lab_b,lab_c,0,(i64)0,(i64)0);
    mc_libc$ccstr((byte*)"}",mc_blockc$blocklevel);
    if (!!(pelse)) {
        mc_libc$ccsendline();
        mc_blockc$evalblock(pelse,(i64)1);
    }
;
    mc_blockc$definefwdlabel(lab_d);
    mc_blockc$unstacklooplabels();
}

static void mc_blockc$dxlabel(i64 lab) {
    mc_libc$ccstr(mc_libc$genclabel(lab,(i64)1),(i64)0);
    mc_libc$ccsendline();
}

static void mc_blockc$do_print(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
        struct mm_decls$unitrec *  q;
        struct mm_decls$unitrec *  r;
        struct mm_decls$unitrec *  s;
        i64 m;
        i64 widenop;
        u8 *  fn;
    if (!!(a)) {
        if (((i64)mm_decls$ttbasetype[((i64)(*a).mode)] != (i64)7)) {
            mm_support$gerror((byte*)"@dev no ref",0);
        }
;
                {i64 $temp = (i64)mm_decls$ttbasetype[((i64)mm_decls$tttarget[((i64)(*a).mode)])];
if (($temp==(i64)0)) {
            mc_libc$do_syscallproc((byte*)"msysc$m_print_startfile",a,0);
        }
        else if (($temp==(i64)12)) {
            mc_libc$do_syscallproc((byte*)"msysc$m_print_startstr",a,0);
        }
        else if (($temp==(i64)7)) {
            mc_libc$do_syscallproc((byte*)"msysc$m_print_startptr",a,0);
        }
        else {
            mm_support$gerror((byte*)"@dev?",0);
        }
        };
    }
    else {
        mc_libc$do_syscallproc((byte*)"msysc$m_print_startcon",0,0);
    }
;
    q = b;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)112) || ($temp==(i64)113)) {
        if ((((i64)mm_decls$ttbasetype[((i64)(*q).mode)] != (i64)7) || ((i64)mm_decls$ttbasetype[((i64)mm_decls$tttarget[((i64)(*q).mode)])] != (i64)12))) {
            mm_support$gerror((byte*)"string expected",0);
        }
;
        mc_libc$do_syscallproc((byte*)"msysc$m_print_setfmt",q,0);
        q = (*p).c;
    }
    };
    L84 :;
    while (!!(q)) {
        s = 0;
                {i64 $temp = (i64)(*q).tag;
if (($temp==(i64)83)) {
            s = (*q).b;
            r = (*q).a;
            m = (i64)(*r).mode;
        }
        else if (($temp==(i64)84)) {
            mc_libc$do_syscallproc((byte*)"msysc$m_print_nogap",0,0);
            q = (*q).nextunit;
            goto L85 ;
        }
        else if (($temp==(i64)85)) {
            mc_libc$do_syscallproc((byte*)"msysc$m_print_space",0,0);
            q = (*q).nextunit;
            goto L85 ;
        }
        else {
            s = mc_libc$nilunit;
            r = q;
            m = (i64)(*q).mode;
        }
        };
        widenop = (i64)0;
                {i64 $temp = (i64)mm_decls$ttbasetype[(m)];
if (($temp==(i64)3)) {
            fn = (byte*)"msysc$m_print_i64";
        }
        else if (($temp==(i64)14) || ($temp==(i64)15) || ($temp==(i64)16)) {
            fn = (byte*)"msysc$m_print_i64";
        }
        else if (($temp==(i64)2)) {
            fn = (byte*)"msysc$m_print_u64";
        }
        else if (($temp==(i64)17) || ($temp==(i64)18) || ($temp==(i64)19)) {
            fn = (byte*)"msysc$m_print_u64";
        }
        else if (($temp==(i64)4)) {
            fn = (byte*)"msysc$m_print_r64";
        }
        else if (($temp==(i64)5)) {
            fn = (byte*)"msysc$m_print_r64";
        }
        else if (($temp==(i64)7)) {
            if ((((i64)mm_decls$tttarget[(m)] == (i64)12) || (((i64)mm_decls$ttbasetype[((i64)mm_decls$tttarget[(m)])] == (i64)10) && ((i64)mm_decls$tttarget[((i64)mm_decls$tttarget[(m)])] == (i64)12)))) {
                fn = (byte*)"msysc$m_print_str";
            }
            else {
                fn = (byte*)"msysc$m_print_ptr";
            }
;
        }
        else if (($temp==(i64)10)) {
            mm_support$gerror((byte*)"PRINTARRAY",0);
            q = (*q).nextunit;
        }
        else if (($temp==(i64)8)) {
            mm_support$gerror((byte*)"PRINTRECORD",0);
        }
        else if (($temp==(i64)12) || ($temp==(i64)1)) {
            fn = (byte*)"msysc$m_print_c8";
        }
        else {
            mm_support$gerror_s((byte*)"PRINT/T=",mm_lib$strmode(m,(i64)1),0);
        }
        };
        if (!!(widenop)) {
            mm_support$gerror((byte*)"WIDEN/PRINT",0);
        }
;
        mc_libc$do_syscallproc(fn,r,s);
        q = (*q).nextunit;
L85 :;
    }
L86 :;
    ;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)111) || ($temp==(i64)113)) {
        mc_libc$do_syscallproc((byte*)"msysc$m_print_newline",0,0);
    }
    };
    mc_libc$do_syscallproc((byte*)"msysc$m_print_end",0,0);
}

static void mc_blockc$do_read(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a) {
        i64 m;
    m = (i64)(*p).mode;
    if ((a == 0)) {
        a = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)0u,(i64)3);
    }
;
    if (!!((i64)mm_decls$ttisinteger[(m)])) {
        mc_libc$do_syscallproc((byte*)"msysc$m_read_i64",a,0);
    }
    else if (!!((i64)mm_decls$ttisreal[(m)])) {
        mc_libc$do_syscallproc((byte*)"msysc$m_read_r64",a,0);
    }
    else {
        mm_support$gerror((byte*)"CAN'T READ THIS ITEM",0);
    }
;
}

static void mc_blockc$do_readln(struct mm_decls$unitrec *a) {
    if (!!(a)) {
        if (((i64)mm_decls$ttbasetype[((i64)(*a).mode)] != (i64)7)) {
            mm_support$gerror((byte*)"@dev no ref",0);
        }
;
                {i64 $temp = (i64)mm_decls$ttbasetype[((i64)mm_decls$tttarget[((i64)(*a).mode)])];
if (($temp==(i64)0)) {
            mc_libc$do_syscallproc((byte*)"msysc$m_read_fileline",a,0);
        }
        else if (($temp==(i64)17) || ($temp==(i64)12)) {
            mc_libc$do_syscallproc((byte*)"msysc$m_read_strline",a,0);
        }
        else {
            mm_support$gerror((byte*)"rd@dev?",0);
        }
        };
    }
    else {
        mc_libc$do_syscallproc((byte*)"msysc$m_read_conline",0,0);
    }
;
}

static void mc_blockc$do_index(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *pindex) {
        i64 lower;
    mc_blockc$applymemmode(p);
    if (((i64)mm_decls$ttbasetype[((i64)(*a).mode)] == (i64)11)) {
        mc_libc$dxstr((byte*)"((");
        mc_libc$dxstr(mc_libc$strmodec((i64)mm_decls$tttarget[((i64)(*a).mode)],(byte*)"",(i64)1));
        mc_libc$dxstr((byte*)"*)");
        mc_blockc$evalunit(a);
        mc_libc$ccstr((byte*)".ptr)",(i64)0);
    }
    else {
        mc_blockc$evalunit(a);
    }
;
    mc_libc$dxstr((byte*)"[(");
    mc_blockc$evalunit(pindex);
    mc_libc$dxchar((i64)41);
    lower = (i64)mm_decls$ttlower[((i64)(*a).mode)];
    if ((lower > (i64)0)) {
        mc_libc$dxchar((i64)45);
        mc_libc$dxint(lower);
    }
    else if ((lower < (i64)0)) {
        mc_libc$dxchar((i64)43);
        mc_libc$dxint(m$llabs(lower));
    }
;
    mc_libc$dxchar((i64)93);
}

static void mc_blockc$do_case(struct mm_decls$unitrec *p,struct mm_decls$unitrec *pindex,struct mm_decls$unitrec *pwhenthen,struct mm_decls$unitrec *pelse) {
        i64 loopsw;
        i64 lab_abc;
        i64 lab_d;
        u8 *  indexstr;
        struct mm_decls$unitrec *  w;
        struct mm_decls$unitrec *  wt;
    loopsw = (i64)((i64)(*p).tag == (i64)104);
    if (!!(loopsw)) {
        lab_abc = mc_blockc$definelabel();
        lab_d = mc_blockc$createfwdlabel();
        mc_blockc$stacklooplabels(lab_abc,lab_abc,lab_d);
    }
;
    wt = pwhenthen;
    indexstr = 0;
    if ((wt == 0)) {
        goto L87 ;
;
    }
;
    if (((i64)(*pindex).tag != (i64)3)) {
        mc_libc$cctab(mc_blockc$blocklevel);
        mc_libc$ccstr((byte*)"{",(i64)0);
        mc_libc$ccstr(mc_libc$strmodec((i64)(*pindex).mode,(byte*)"",(i64)1),(i64)0);
        mc_libc$ccstr((byte*)" $temp = ",(i64)0);
        mc_blockc$evalunitc(pindex);
        mc_libc$ccstrline((byte*)";");
        indexstr = (byte*)"$temp";
    }
;
    L88 :;
    while (!!(wt)) {
        if ((wt == pwhenthen)) {
            mc_libc$ccstr((byte*)"if (",(i64)0);
        }
        else {
            mc_libc$ccstr((byte*)"else if (",mc_blockc$blocklevel);
        }
;
        w = (*wt).a;
        L91 :;
        while (!!(w)) {
            if ((w != (*wt).a)) {
                mc_libc$ccstr((byte*)" || ",(i64)0);
            }
;
            if (!!(pindex)) {
                mc_libc$ccchar((i64)40);
                if (!!(indexstr)) {
                    mc_libc$ccstr(indexstr,(i64)0);
                }
                else {
                    mc_blockc$evalunit(pindex);
                }
;
                mc_libc$ccstr((byte*)"==",(i64)0);
                mc_blockc$evalunit(w);
                mc_libc$ccchar((i64)41);
            }
            else {
                mc_blockc$evalunit(w);
            }
;
            w = (*w).nextunit;
L92 :;
        }
L93 :;
        ;
        mc_libc$ccstr((byte*)") ",(i64)0);
        mc_blockc$evalblock((*wt).b,(i64)1);
        wt = (*wt).nextunit;
L89 :;
    }
L90 :;
    ;
    if (!!(pelse)) {
        mc_libc$ccstr((byte*)"else ",mc_blockc$blocklevel);
        //doelse:
L87 :;
;
        mc_blockc$evalblock(pelse,(i64)1);
    }
;
    if (!!(indexstr)) {
        mc_libc$ccstr((byte*)"}",mc_blockc$blocklevel);
    }
;
    if (!!(loopsw)) {
        mc_libc$ccstr((byte*)"goto ",(i64)0);
        mc_libc$ccstrsemi(mc_libc$genclabel(lab_abc,(i64)0));
        mc_blockc$definefwdlabel(lab_d);
        mc_blockc$unstacklooplabels();
    }
;
}

static void mc_blockc$do_switch(struct mm_decls$unitrec *p,struct mm_decls$unitrec *pindex,struct mm_decls$unitrec *pwhenthen,struct mm_decls$unitrec *pelse) {
        i64 loopsw;
        i64 lab_a;
        i64 lab_d;
        i64 x;
        i64 y;
        i64 i;
        struct mm_decls$unitrec *  w;
        struct mm_decls$unitrec *  wt;
    loopsw = (i64)((i64)(*p).tag == (i64)106);
    if (!!(loopsw)) {
        lab_a = mc_blockc$definelabel();
        lab_d = mc_blockc$createfwdlabel();
        mc_blockc$stacklooplabels(lab_a,lab_a,lab_d);
    }
;
    mc_libc$ccstr((byte*)"switch (",(i64)0);
    mc_blockc$evalunitc(pindex);
    mc_libc$ccstrline((byte*)") {");
    wt = pwhenthen;
    L94 :;
    while (!!(wt)) {
        w = (*wt).a;
        L97 :;
        while (!!(w)) {
                        {i64 $temp = (i64)(*w).tag;
if (($temp==(i64)16)) {
                x = (*(*w).a).value;
                y = (*(*w).b).value;
            }
            else if (($temp==(i64)1)) {
                x = (y = (*w).value);
            }
            };
            for (i=x;i<=y;++i) {
L100 :;
                mc_libc$cctab(mc_blockc$blocklevel);
                mc_libc$ccstr((byte*)"case ",(i64)0);
                mc_libc$ccint(i);
                if ((((*wt).b == 0) && (i == y))) {
                    mc_libc$ccstrline((byte*)":;// ");
                }
                else {
                    mc_libc$ccstrline((byte*)":;");
                }
;
L101 :;
            }
L102 :;
            ;
            w = (*w).nextunit;
L98 :;
        }
L99 :;
        ;
        ++(mc_blockc$blocklevel);
        mc_libc$cctab(mc_blockc$blocklevel);
        mc_blockc$evalblock((*wt).b,(i64)1);
        mc_libc$cctab(mc_blockc$blocklevel);
        mc_libc$ccstrsemi((byte*)"break");
        --(mc_blockc$blocklevel);
        wt = (*wt).nextunit;
L95 :;
    }
L96 :;
    ;
    if (!!(pelse)) {
        mc_libc$cctab(mc_blockc$blocklevel);
        mc_libc$ccstr((byte*)"default: ",(i64)0);
        mc_blockc$evalblock(pelse,(i64)1);
    }
;
    mc_libc$cctab(mc_blockc$blocklevel);
    mc_libc$ccstrline((byte*)"} //SW");
    if (!!(loopsw)) {
        mc_libc$ccstr((byte*)"goto ",(i64)0);
        mc_libc$ccstrsemi(mc_libc$genclabel(lab_a,(i64)0));
        mc_blockc$definefwdlabel(lab_d);
        mc_blockc$unstacklooplabels();
    }
;
}

static void mc_blockc$do_goto(struct mm_decls$unitrec *a) {
        struct mm_decls$strec *  d;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)3)) {
        d = (*a).def;
        if (((i64)(*d).index == (i64)0)) {
            (*d).index = ++(mc_decls$mlabelno);
        }
;
                {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)17)) {
            mc_libc$ccstr((byte*)"goto ",(i64)0);
            mc_libc$ccstrsemi(mc_libc$genclabel((i64)(*d).index,(i64)0));
        }
        else {
            mm_support$gerror_s((byte*)"Not label/block name: %s",(*d).name,0);
        }
        };
    }
    else {
        mm_support$gerror((byte*)"Can't do goto with label pointer",0);
    }
    };
}

static void mc_blockc$do_max(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
    if ((!!(mc_libc$issimplec(a)) && !!(mc_libc$issimplec(b)))) {
        mc_libc$dxchar((i64)40);
        mc_blockc$evalunit(a);
        mc_libc$dxchar((((i64)(*p).pclop == (i64)15) ? (i64)60 : (i64)62));
        mc_blockc$evalunit(b);
        mc_libc$dxchar((i64)63);
        mc_blockc$evalunit(a);
        mc_libc$dxchar((i64)58);
        mc_blockc$evalunit(b);
        mc_libc$dxchar((i64)41);
    }
    else {
        mc_libc$dxstr((byte*)"msysc$m_");
        if (!!((i64)mm_decls$ttisreal[((i64)(*a).mode)])) {
            mc_libc$dxchar((i64)102);
        }
        else {
            mc_libc$dxchar((i64)105);
        }
;
        mc_libc$dxstr((((i64)(*p).pclop == (i64)15) ? (byte*)"min(" : (byte*)"max("));
        mc_blockc$evalunit(a);
        mc_libc$dxchar((i64)44);
        mc_blockc$evalunit(b);
        mc_libc$dxchar((i64)41);
    }
;
}

static void mc_blockc$do_maxto(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
    mc_blockc$evalunit(a);
    mc_libc$ccstr((byte*)"=(",(i64)0);
    mc_blockc$evalunit(a);
    mc_libc$ccchar((((i64)(*p).pclop == (i64)70) ? (i64)60 : (i64)62));
    mc_blockc$evalunit(b);
    mc_libc$ccchar((i64)63);
    mc_blockc$evalunit(a);
    mc_libc$ccchar((i64)58);
    mc_blockc$evalunit(b);
    mc_libc$ccstrsemi((byte*)")");
}

static i64 mc_blockc$isexpr(struct mm_decls$unitrec *p) {
        i64 expr;
    expr = (i64)0;
    if (!!((i64)mm_tables$jisexpr[((i64)(*p).tag)])) {
        expr = (i64)1;
    }
;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)90) || ($temp==(i64)105) || ($temp==(i64)103) || ($temp==(i64)108)) {
        if (((i64)(*p).mode == (i64)0)) {
            expr = (i64)0;
        }
;
    }
    };
    return expr;
}

static void mc_blockc$do_swap(struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
    mc_libc$ccstr((byte*)"{",(i64)0);
    mc_libc$ccstr(mc_libc$strmodec((i64)(*a).mode,(byte*)"temp",(i64)1),(i64)0);
    mc_libc$ccstr((byte*)" = ",(i64)0);
    mc_blockc$evalunit(a);
    mc_libc$ccstr((byte*)"; ",(i64)0);
    mc_blockc$evalunit(a);
    mc_libc$ccstr((byte*)" = ",(i64)0);
    mc_blockc$evalunit(b);
    mc_libc$ccstr((byte*)"; ",(i64)0);
    mc_blockc$evalunit(b);
    mc_libc$ccstr((byte*)" = temp; }",(i64)0);
}

static void mc_blockc$do_inrange(struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,struct mm_decls$unitrec *c) {
    mc_libc$dxchar((i64)40);
    mc_blockc$evalunit(a);
    mc_libc$dxstr((byte*)" >= ");
    mc_blockc$evalunit(b);
    mc_libc$dxstr((byte*)" && ");
    mc_blockc$evalunit(a);
    mc_libc$dxstr((byte*)" <= ");
    mc_blockc$evalunit(c);
    mc_libc$dxchar((i64)41);
}

static void mc_blockc$do_inset(struct mm_decls$unitrec *a,struct mm_decls$unitrec *p) {
    mc_libc$dxchar((i64)40);
    L103 :;
    while (!!(p)) {
        mc_blockc$evalunit(a);
        mc_libc$dxstr((byte*)" == ");
        mc_blockc$evalunit(p);
        if (!!((*p).nextunit)) {
            mc_libc$dxstr((byte*)" || ");
        }
;
L104 :;
        p = (*p).nextunit;
L106 :;
            }
L105 :;
    ;
    mc_libc$dxchar((i64)41);
}

static void mc_blockc$do_cmpchain(struct mm_decls$unitrec *p) {
        struct mm_decls$unitrec *  q;
        struct mm_decls$unitrec *  r;
        i64 i;
    q = (*p).a;
    i = (i64)1;
    mc_libc$dxchar((i64)40);
    L107 :;
    do {
        if ((i > (i64)1)) {
            mc_libc$dxstr((byte*)" && ");
        }
;
        mc_blockc$evalunit(q);
        r = (*q).nextunit;
        mc_libc$dxstr(mm_tables$ccopnames[((i64)(*p).cmpgenop[(i)-1])]);
        ++(i);
        mc_blockc$evalunit((r = (*q).nextunit));
        q = r;
L108 :;
    }
    while (!((*q).nextunit == 0));
L109 :;
    ;
    mc_libc$dxchar((i64)41);
}

static void mc_blockc$do_blockcopy(struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
    mc_libc$dxstr((byte*)"memcpy(&");
    mc_blockc$evalunit(a);
    if (((i64)(*b).tag == (i64)45)) {
        mc_libc$dxstr((byte*)",");
        mc_blockc$evalunit((*b).a);
    }
    else {
        mc_libc$dxstr((byte*)",&");
        mc_blockc$evalunit(b);
    }
;
    mc_libc$dxchar((i64)44);
    mc_libc$dxint((i64)mm_decls$ttsize[((i64)(*a).mode)]);
    mc_libc$dxchar((i64)41);
}

static void mc_blockc$do_select(struct mm_decls$unitrec *pindex,struct mm_decls$unitrec *plist,struct mm_decls$unitrec *pdefault) {
        i64 i;
        struct mm_decls$unitrec *  q;
        i64 $av_1;
    q = plist;
    i = (i64)1;
    L110 :;
    while (!!(q)) {
        mc_libc$dxchar((i64)40);
        mc_blockc$evalunit(pindex);
        mc_libc$dxstr((byte*)"==");
        mc_libc$dxint(i);
        ++(i);
        mc_libc$dxchar((i64)63);
        mc_blockc$evalunit(q);
        mc_libc$dxchar((i64)58);
        q = (*q).nextunit;
L111 :;
    }
L112 :;
    ;
    mc_blockc$evalunit(pdefault);
    $av_1 = (i - (i64)1);
    while ($av_1-- > 0) {
L113 :;
        mc_libc$dxchar((i64)41);
L114 :;
    }
L115 :;
    ;
}

static void mc_blockc$do_dotindex(struct mm_decls$unitrec *a,struct mm_decls$unitrec *i) {
    mc_libc$dxstr((byte*)"call m$getdotindex(");
    mc_blockc$evalunit(a);
    mc_libc$dxchar((i64)44);
    mc_blockc$evalunit(i);
    mc_libc$dxchar((i64)41);
}

static void mc_blockc$do_supportcall(u8 *fnname,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,struct mm_decls$unitrec *c,struct mm_decls$unitrec *d) {
    mc_libc$dxstr((byte*)"msysc$m_");
    mc_libc$dxstr(fnname);
    mc_libc$dxchar((i64)40);
    mc_blockc$evalunit(a);
    if (!!(b)) {
        mc_libc$dxchar((i64)44);
        mc_blockc$evalunit(b);
        if (!!(c)) {
            mc_libc$dxchar((i64)44);
            mc_blockc$evalunit(c);
            if (!!(d)) {
                mc_libc$dxchar((i64)44);
                mc_blockc$evalunit(d);
            }
;
        }
;
    }
;
    mc_libc$dxchar((i64)41);
}

static void mc_blockc$applymemmode(struct mm_decls$unitrec *p) {
    if (!!((i64)(*p).memmode)) {
        mc_libc$dxchar((i64)40);
        mc_libc$dxstr(mc_libc$strmodec((i64)(*p).mode,(byte*)"",(i64)1));
        mc_libc$dxchar((i64)41);
    }
;
}

static void mc_blockc$domaths2(u8 *name,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
    mc_libc$dxstr(name);
    mc_libc$dxchar((i64)40);
    mc_blockc$evalunit(a);
    mc_libc$dxchar((i64)44);
    mc_blockc$evalunit(b);
    mc_libc$dxchar((i64)41);
}

static void mc_blockc$do_typepun(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a) {
        i64 amode;
        i64 pmode;
    amode = (i64)(*a).mode;
    pmode = (i64)(*p).mode;
    if (((i64)(*a).tag == (i64)1)) {
        if ((!!(((i64)mm_decls$ttisinteger[(amode)] + (i64)mm_decls$ttisreal[(amode)])) && !!(((i64)mm_decls$ttisinteger[(pmode)] + (i64)mm_decls$ttisreal[(pmode)])))) {
            (*a).mode = pmode;
            mc_blockc$do_const(a);
        }
        else if ((!!(((i64)mm_decls$ttisinteger[(amode)] + (i64)mm_decls$ttisreal[(amode)])) && !!((i64)mm_decls$ttisref[(pmode)]))) {
            mc_libc$dxstr((byte*)"(");
            mc_libc$dxstr(mc_libc$strmodec(pmode,(byte*)"",(i64)1));
            mc_libc$dxstr((byte*)")");
            mc_blockc$do_const(a);
        }
        else {
            mm_support$gerror((byte*)"@const",0);
        }
;
    }
    else if (((i64)(*a).tag == (i64)3)) {
        mc_libc$dxstr((byte*)"*(");
        mc_libc$dxstr(mc_libc$strmodec(pmode,(byte*)"",(i64)1));
        mc_libc$dxstr((byte*)"*)&");
        mc_blockc$evalunit(a);
    }
    else {
        mc_libc$dxchar((i64)40);
        mc_libc$dxstr(mc_libc$strmodec(pmode,(byte*)"",(i64)1));
        mc_libc$dxchar((i64)41);
        if ((!!((i64)mm_decls$ttisreal[(amode)]) && !!((i64)mm_decls$ttisinteger[(pmode)]))) {
            mc_blockc$do_supportcall((byte*)"tp_r64toi64",a,0,0,0);
        }
        else if ((!!((i64)mm_decls$ttisinteger[(amode)]) && !!((i64)mm_decls$ttisreal[(pmode)]))) {
            mc_blockc$do_supportcall((byte*)"tp_i64tor64",a,0,0,0);
        }
        else if ((!!((i64)mm_decls$ttisinteger[(amode)]) && !!((i64)mm_decls$ttisref[(pmode)]))) {
            mc_blockc$do_supportcall((byte*)"tp_i64toref",a,0,0,0);
        }
        else if ((!!((i64)mm_decls$ttisref[(amode)]) && !!((i64)mm_decls$ttisinteger[(pmode)]))) {
            mc_blockc$do_supportcall((byte*)"tp_reftoi64",a,0,0,0);
        }
        else {
            msysc$m_print_startcon();
            msysc$m_print_str(mm_lib$strmode(pmode,(i64)1),NULL);
            msysc$m_print_str((byte*)"@",NULL);
            msysc$m_print_str(mm_lib$strmode(amode,(i64)1),NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            mm_support$gerror((byte*)"Typepun?",0);
        }
;
    }
;
}

// START
void mc_blockc$start(void) {

}

// START
void mm_decls$start(void) {

}

void mm_diags$printoverloads(void *f) {
        struct mm_decls$overloadrec *  p;
        i64 i;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"OVERLOADS",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=(i64)125;++i) {
L116 :;
        p = (struct mm_decls$overloadrec *)mm_decls$overloadtable[(i)];
        if (!!(p)) {
            L119 :;
            while (!!(p)) {
                if (!!((i64)(*p).bmode)) {
                    msysc$m_print_startfile(f);
                    msysc$m_print_setfmt((byte*)"operator (#)(#,#)#");
                    msysc$m_print_str((mm_tables$jtagnames[(i)] + (i64)2),NULL);
                    msysc$m_print_str(mm_lib$strmode((i64)(*p).amode,(i64)1),NULL);
                    msysc$m_print_str(mm_lib$strmode((i64)(*p).bmode,(i64)1),NULL);
                    msysc$m_print_str(mm_lib$strmode((i64)(*p).rmode,(i64)1),NULL);
                    msysc$m_print_end();
                    ;
                }
                else {
                    msysc$m_print_startfile(f);
                    msysc$m_print_setfmt((byte*)"operator (#)(#)#");
                    msysc$m_print_str((mm_tables$jtagnames[(i)] + (i64)2),NULL);
                    msysc$m_print_str(mm_lib$strmode((i64)(*p).amode,(i64)1),NULL);
                    msysc$m_print_str(mm_lib$strmode((i64)(*p).rmode,(i64)1),NULL);
                    msysc$m_print_end();
                    ;
                }
;
                if (!!((*p).fncode)) {
                    msysc$m_print_startfile(f);
                    msysc$m_print_str((byte*)"=",NULL);
                    msysc$m_print_ptr((*p).fncode,NULL);
                    msysc$m_print_str((*mm_lib$strexpr((struct mm_decls$unitrec *)(*p).fncode)).strptr,NULL);
                    msysc$m_print_end();
                    ;
                }
;
                msysc$m_print_startfile(f);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                p = (struct mm_decls$overloadrec *)(*p).nextoverload;
L120 :;
            }
L121 :;
            ;
            msysc$m_print_startfile(f);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
;
L117 :;
    }
L118 :;
    ;
}

void mm_diags$printst(void *f,struct mm_decls$strec *p,i64 level) {
        struct mm_decls$strec *  q;
    if (((i64)(*p).symbol != (i64)81)) {
        mm_support$mcerror((byte*)"PRINTST not name");
    }
;
    mm_diags$printstrec(f,(struct mm_decls$strec *)p,level);
    q = (struct mm_decls$strec *)(*p).deflist;
    L122 :;
    while ((q != 0)) {
        mm_diags$printst(f,(struct mm_decls$strec *)q,(level + (i64)1));
        q = (struct mm_decls$strec *)(*q).nextdef;
L123 :;
    }
L124 :;
    ;
}

static void mm_diags$printstrec(void *f,struct mm_decls$strec *p,i64 level) {
        struct mm_decls$strec dd;
        struct mlib$strbuffer v;
        struct mlib$strbuffer *  d;
        i64 col;
        i64 offset;
        u8 str[256];
        i64 $av_1;
    d = &v;
    mlib$gs_init((struct mlib$strbuffer *)d);
    msysc$m_print_startstr(str);
    msysc$m_print_ptr(p,NULL);
    msysc$m_print_end();
    ;
    mlib$gs_str((struct mlib$strbuffer *)d,str);
    mlib$gs_str((struct mlib$strbuffer *)d,(byte*)" ");
    offset = (i64)0;
    $av_1 = level;
    while ($av_1-- > 0) {
L125 :;
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"    ");
        offset += (i64)4;
L126 :;
    }
L127 :;
    ;
    mlib$gs_str((struct mlib$strbuffer *)d,(byte*)":");
    mlib$gs_leftstr((struct mlib$strbuffer *)d,(*p).name,((i64)28 - offset),(i64)45);
    mlib$gs_leftstr((struct mlib$strbuffer *)d,mm_tables$namenames[((i64)(*p).nameid)],(i64)12,(i64)46);
    col = mlib$gs_getcol((struct mlib$strbuffer *)d);
    dd = (*p);
    mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"[");
    if (!!(msysc$m_getdotindex((i64)(*p).flags,(i64)12))) {
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"Imp ");
    }
    else {
        mlib$gs_str((struct mlib$strbuffer *)d,((i64)(*p).scope==1?(byte*)"Sub ":((i64)(*p).scope==2?(byte*)"Prog ":((i64)(*p).scope==3?(byte*)"Exp ":(byte*)"Mod "))));
    }
;
    if (!!(msysc$m_getdotindex((i64)dd.flags,(i64)0))) {
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"Stat");
    }
;
    if (!!((i64)dd.fflang)) {
        mlib$gs_strsp((struct mlib$strbuffer *)d,mm_tables$fflangnames[((i64)dd.fflang)]);
    }
;
    if ((((i64)dd.nameid == (i64)13) && !!((i64)dd.parammode))) {
        mlib$gs_str((struct mlib$strbuffer *)d,mm_tables$parammodenames[((i64)dd.parammode)]);
    }
;
    if (!!((i64)dd.align)) {
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"@@");
        mlib$gs_strint((struct mlib$strbuffer *)d,(i64)dd.align);
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)" maxalign:");
        mlib$gs_strint((struct mlib$strbuffer *)d,(i64)dd.maxalign);
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)" ");
    }
;
    if (!!((i64)dd.optional)) {
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"Opt ");
    }
;
    if (!!((i64)dd.varparams)) {
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"Var:");
        mlib$gs_strint((struct mlib$strbuffer *)d,(i64)dd.varparams);
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)" ");
    }
;
    if (!!((i64)dd.moduleno)) {
        if (((i64)dd.nameid != (i64)2)) {
            msysc$m_print_startstr(str);
            msysc$m_print_str((byte*)"Modno#",NULL);
            msysc$m_print_nogap();
            msysc$m_print_i64((i64)dd.moduleno,NULL);
            msysc$m_print_end();
            ;
        }
        else {
            msysc$m_print_startstr(str);
            msysc$m_print_str((byte*)"Subno#",NULL);
            msysc$m_print_nogap();
            msysc$m_print_i64((i64)dd.subprogno,NULL);
            msysc$m_print_end();
            ;
        }
;
        mlib$gs_str((struct mlib$strbuffer *)d,(u8 *)str);
    }
;
    if (!!(msysc$m_getdotindex((i64)dd.flags,(i64)1))) {
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"U ");
    }
;
    if (!!((i64)dd.isthreaded)) {
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"Threaded ");
    }
;
    mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"]");
    mlib$gs_padto((struct mlib$strbuffer *)d,(col + (i64)10),(i64)61);
    if (!!((*p).owner)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"(#)");
        msysc$m_print_str((*(*p).owner).name,NULL);
        msysc$m_print_end();
        ;
        mlib$gs_leftstr((struct mlib$strbuffer *)d,(u8 *)str,(i64)18,(i64)45);
    }
    else {
        mlib$gs_leftstr((struct mlib$strbuffer *)d,(byte*)"()",(i64)18,(i64)45);
    }
;
        {i64 $temp = (i64)(*p).mode;
if (($temp==(i64)0)) {
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"Void ");
    }
    else {
        mlib$gs_strint((struct mlib$strbuffer *)d,(i64)(*p).mode);
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)":");
        mlib$gs_str((struct mlib$strbuffer *)d,mm_lib$strmode((i64)(*p).mode,(i64)1));
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)" ");
    }
    };
        {i64 $temp = (i64)(*p).nameid;
if (($temp==(i64)14) || ($temp==(i64)13)) {
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)" Offset:");
        mlib$gs_strint((struct mlib$strbuffer *)d,(i64)(*p).offset);
        if (((i64)(*p).mode == (i64)26)) {
            mlib$gs_str((struct mlib$strbuffer *)d,(byte*)" Bitoffset:");
            mlib$gs_strint((struct mlib$strbuffer *)d,(i64)(*p).bitoffset);
            mlib$gs_str((struct mlib$strbuffer *)d,(byte*)":");
            mlib$gs_strint((struct mlib$strbuffer *)d,(i64)(*p).bitfieldwidth);
        }
;
        sprintf((u8 *)str,(byte*)"%.*s",(i64)(*p).uflags.ulength,&(*p).uflags.codes);
        msysc$m_print_startstr(str);
        msysc$m_print_i64((i64)(*p).uflags.ulength,(byte*)"v");
        msysc$m_print_str((u8 *)&(*p).uflags.codes,(byte*)".*");
        msysc$m_print_end();
        ;
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)" UFLAGS:");
        mlib$gs_str((struct mlib$strbuffer *)d,(u8 *)str);
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"-");
        mlib$gs_strint((struct mlib$strbuffer *)d,(i64)(*p).uflags.ulength);
        if (!!((*p).code)) {
            mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"/:=");
            mlib$gs_strvar((struct mlib$strbuffer *)d,(struct mlib$strbuffer *)mm_lib$strexpr((struct mm_decls$unitrec *)(*p).code));
        }
;
        if ((((i64)(*p).nameid == (i64)13) && !!((i64)(*p).variadic))) {
            mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"...");
        }
;
    }
    else if (($temp==(i64)15)) {
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"Index:");
        mlib$gs_strint((struct mlib$strbuffer *)d,(i64)(*p).offset);
    }
    else if (($temp==(i64)6) || ($temp==(i64)9)) {
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"Index:");
        mlib$gs_strint((struct mlib$strbuffer *)d,(i64)(*p).index);
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)" Nret:");
        mlib$gs_strint((struct mlib$strbuffer *)d,(i64)(*p).nretvalues);
    }
    else if (($temp==(i64)7)) {
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"Index/PCaddr:");
        mlib$gs_strint((struct mlib$strbuffer *)d,(i64)(*p).index);
        if (!!((*p).truename)) {
            mlib$gs_str((struct mlib$strbuffer *)d,(byte*)" Truename:");
            mlib$gs_str((struct mlib$strbuffer *)d,(*p).truename);
        }
;
    }
    else if (($temp==(i64)11)) {
        if (!!((*p).code)) {
            mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"=");
            mlib$gs_strvar((struct mlib$strbuffer *)d,(struct mlib$strbuffer *)mm_lib$strexpr((struct mm_decls$unitrec *)(*p).code));
        }
;
    }
    else if (($temp==(i64)12)) {
        if (!!((*p).code)) {
            mlib$gs_str((struct mlib$strbuffer *)d,(byte*)":=");
            mlib$gs_strvar((struct mlib$strbuffer *)d,(struct mlib$strbuffer *)mm_lib$strexpr((struct mm_decls$unitrec *)(*p).code));
        }
;
    }
    else if (($temp==(i64)10)) {
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"Const:");
        mlib$gs_strvar((struct mlib$strbuffer *)d,(struct mlib$strbuffer *)mm_lib$strexpr((struct mm_decls$unitrec *)(*p).code));
    }
    else if (($temp==(i64)5)) {
        if (!!((i64)(*p).baseclass)) {
            mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"Baseclass:");
            mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"<HAS BASECLASS>");
        }
;
    }
    else if (($temp==(i64)16)) {
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"Enum:");
        mlib$gs_strint((struct mlib$strbuffer *)d,(i64)(*p).index);
    }
    else if (($temp==(i64)4)) {
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)"DLL#:");
        mlib$gs_strint((struct mlib$strbuffer *)d,(i64)(*p).dllindex);
    }
    };
    if (!!(msysc$m_getdotindex((i64)(*p).flags,(i64)9))) {
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)" @");
        mlib$gs_str((struct mlib$strbuffer *)d,(*(*p).equivfield).name);
        mlib$gs_str((struct mlib$strbuffer *)d,(byte*)" +");
        mlib$gs_strint((struct mlib$strbuffer *)d,(i64)(*p).equivoffset);
    }
;
    if (!!(msysc$m_getdotindex((i64)(*p).flags,(i64)10))) {
        mlib$gs_strvar((struct mlib$strbuffer *)d,(struct mlib$strbuffer *)mm_lib$strexpr((struct mm_decls$unitrec *)(*p).equivvar));
    }
;
    mlib$gs_str((struct mlib$strbuffer *)d,(byte*)" Lineno: ???");
    mlib$gs_println((struct mlib$strbuffer *)d,f);
        {i64 $temp = (i64)(*p).nameid;
if (($temp==(i64)10) || ($temp==(i64)12) || ($temp==(i64)11) || ($temp==(i64)18)) {
        if (!!((*p).code)) {
            mm_diags$printunit((struct mm_decls$unitrec *)(*p).code,(i64)0,(byte*)"*",f);
        }
;
    }
    };
}

void mm_diags$printstflat(void *f) {
        i64 i;
        struct mm_decls$strec *  p;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"GLOBAL SYMBOL TABLE:",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)0;i<=(i64)65534;++i) {
L128 :;
        p = (struct mm_decls$strec *)&mm_lex$hashtable[(i)];
        if (!!((*p).name)) {
                        {i64 $temp = (i64)(*p).symbol;
if (($temp==(i64)81)) {
                msysc$m_print_startfile(f);
                msysc$m_print_i64(i,NULL);
                msysc$m_print_ptr(p,NULL);
                msysc$m_print_str((byte*)":",NULL);
                msysc$m_print_str((*p).name,NULL);
                msysc$m_print_str(mm_tables$symbolnames[((i64)(*p).symbol)-1],NULL);
                msysc$m_print_str(mm_tables$namenames[((i64)(*p).nameid)],NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                p = (struct mm_decls$strec *)(*p).nextdupl;
                L131 :;
                while (!!(p)) {
                    msysc$m_print_startfile(f);
                    msysc$m_print_str((byte*)"\t",NULL);
                    msysc$m_print_ptr(p,NULL);
                    msysc$m_print_str((*p).name,NULL);
                    msysc$m_print_str(mm_tables$symbolnames[((i64)(*p).symbol)-1],NULL);
                    msysc$m_print_str(mm_tables$namenames[((i64)(*p).nameid)],NULL);
                    msysc$m_print_str((byte*)"(From",NULL);
                    msysc$m_print_str((!!((*p).owner) ? (*(*p).owner).name : (byte*)"-"),NULL);
                    msysc$m_print_nogap();
                    msysc$m_print_str((byte*)")",NULL);
                    msysc$m_print_newline();
                    msysc$m_print_end();
                    ;
                    p = (struct mm_decls$strec *)(*p).nextdupl;
L132 :;
                }
L133 :;
                ;
            }
            };
        }
;
L129 :;
    }
L130 :;
    ;
}

void mm_diags$printcode(void *f,u8 *caption) {
        struct mm_decls$strec *  p;
        struct mm_decls$procrec *  pp;
    pp = (struct mm_decls$procrec *)mm_decls$proclist;
    L134 :;
    while (!!(pp)) {
        p = (struct mm_decls$strec *)(*pp).def;
        msysc$m_print_startfile(f);
        msysc$m_print_str((*p).name,NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)"=",NULL);
        msysc$m_print_str(((i64)(*p).scope==1?(byte*)"Sub":((i64)(*p).scope==2?(byte*)"Prog":((i64)(*p).scope==3?(byte*)"Exp":(byte*)"Mod"))),NULL);
        msysc$m_print_end();
        ;
        if (((i64)(*(*p).owner).nameid == (i64)5)) {
            msysc$m_print_startfile(f);
            msysc$m_print_str((byte*)" in record",NULL);
            msysc$m_print_str((*(*p).owner).name,NULL);
            msysc$m_print_end();
            ;
        }
;
        msysc$m_print_startfile(f);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        mm_diags$printunit((struct mm_decls$unitrec *)(*p).code,(i64)0,(byte*)"1",f);
        msysc$m_print_startfile(f);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        pp = (struct mm_decls$procrec *)(*pp).nextproc;
L135 :;
    }
L136 :;
    ;
}

void mm_diags$printunit(struct mm_decls$unitrec *p,i64 level,u8 *prefix,void *dev) {
        struct mm_decls$strec *  d;
        i64 t;
        u8 *  idname;
        i64 a;
        r32 x32;
        static i64 cmpchain = (i64)0;
        u8 opndno[16];
        i64 $av_2;
        i64 i;
    if ((p == 0)) {
        return;
    }
;
    if (!!((i64)(*p).pos)) {
        mm_diags$currlineno = mm_support$getlineno((u64)(i64)(*p).pos);
        mm_diags$currfileno = (i64)msysc$m_getdotslice((i64)(*p).pos,(i64)24,(i64)31);
    }
;
    msysc$m_print_startfile(dev);
    msysc$m_print_ptr(p,NULL);
    msysc$m_print_str((byte*)":",NULL);
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(dev);
    msysc$m_print_str(mm_diags$getprefix(level,prefix,(struct mm_decls$unitrec *)p),NULL);
    msysc$m_print_end();
    ;
    idname = (mm_tables$jtagnames[((i64)(*p).tag)] + (i64)1);
    msysc$m_print_startfile(dev);
    msysc$m_print_str(idname,NULL);
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)":",NULL);
    msysc$m_print_end();
    ;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)3)) {
        d = (struct mm_decls$strec *)(*p).def;
        msysc$m_print_startfile(dev);
        msysc$m_print_str((*d).name,NULL);
        msysc$m_print_str(mm_tables$namenames[((i64)(*d).nameid)],NULL);
        msysc$m_print_end();
        ;
        if (!!((*d).code)) {
            msysc$m_print_startfile(dev);
            msysc$m_print_str((byte*)" {",NULL);
            msysc$m_print_nogap();
            msysc$m_print_str(mm_tables$jtagnames[((i64)(*(*d).code).tag)],NULL);
            msysc$m_print_nogap();
            msysc$m_print_str((byte*)"}",NULL);
            msysc$m_print_end();
            ;
        }
;
        msysc$m_print_startfile(dev);
        msysc$m_print_str((byte*)" ",NULL);
        msysc$m_print_nogap();
        msysc$m_print_str(mm_lib$getdottedname((struct mm_decls$strec *)d),NULL);
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(dev);
        msysc$m_print_str((!!((i64)(*p).dottedname) ? (byte*)" {Dotted}" : (byte*)""),NULL);
        msysc$m_print_end();
        ;
        if (!!((*p).c)) {
            msysc$m_print_startfile(dev);
            msysc$m_print_str((byte*)" Lastcall:",NULL);
            msysc$m_print_ptr((*p).c,NULL);
            msysc$m_print_end();
            ;
        }
;
        if (!!((i64)(*p).addroffirst)) {
            msysc$m_print_startfile(dev);
            msysc$m_print_str((byte*)" Addroffirst.",NULL);
            msysc$m_print_end();
            ;
        }
;
        msysc$m_print_startfile(dev);
        msysc$m_print_str((byte*)" Moduleno:",NULL);
        msysc$m_print_i64((i64)(*p).moduleno,NULL);
        msysc$m_print_end();
        ;
        if (!!((i64)(*p).avcode)) {
            msysc$m_print_startfile(dev);
            msysc$m_print_str((byte*)" AV:",NULL);
            msysc$m_print_c8((u64)(u8)(i64)(*p).avcode,NULL);
            msysc$m_print_end();
            ;
        }
;
        msysc$m_print_startfile(dev);
        msysc$m_print_str((byte*)"P.INDEX=",NULL);
        msysc$m_print_i64((i64)(*p).index,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)98)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_str((*(*p).def).name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)1) || ($temp==(i64)123)) {
        t = (i64)(*p).mode;
        a = (*p).value;
        if ((t == (i64)20)) {
            if (((i64)(*p).slength > (i64)256)) {
                msysc$m_print_startfile(dev);
                msysc$m_print_str((byte*)"\"",NULL);
                msysc$m_print_nogap();
                msysc$m_print_str((byte*)"(LONGSTR)",NULL);
                msysc$m_print_str((byte*)"\" *",NULL);
                msysc$m_print_nogap();
                msysc$m_print_i64((i64)(*p).slength,NULL);
                msysc$m_print_end();
                ;
            }
            else if (!!((i64)(*p).slength)) {
                msysc$m_print_startfile(dev);
                msysc$m_print_str((byte*)"\"",NULL);
                msysc$m_print_nogap();
                msysc$m_print_str((*p).svalue,NULL);
                msysc$m_print_nogap();
                msysc$m_print_str((byte*)"\" *",NULL);
                msysc$m_print_nogap();
                msysc$m_print_i64((i64)(*p).slength,NULL);
                msysc$m_print_end();
                ;
            }
            else {
                msysc$m_print_startfile(dev);
                msysc$m_print_str((byte*)"\"\"",NULL);
                msysc$m_print_end();
                ;
            }
;
        }
        else {
                        {i64 $temp = (i64)mm_decls$ttbasetype[(t)];
if (($temp==(i64)3) || ($temp==(i64)16) || ($temp==(i64)15) || ($temp==(i64)14)) {
                msysc$m_print_startfile(dev);
                msysc$m_print_i64(a,NULL);
                msysc$m_print_end();
                ;
            }
            else if (($temp==(i64)2) || ($temp==(i64)19) || ($temp==(i64)18) || ($temp==(i64)17)) {
                msysc$m_print_startfile(dev);
                msysc$m_print_u64((u64)a,NULL);
                msysc$m_print_end();
                ;
            }
            else if (($temp==(i64)1) || ($temp==(i64)12)) {
                msysc$m_print_startfile(dev);
                msysc$m_print_str(mlib$chr(a),NULL);
                msysc$m_print_end();
                ;
            }
            else if (($temp==(i64)4)) {
                x32 = (r32)(*p).xvalue;
                msysc$m_print_startfile(dev);
                msysc$m_print_r64((r64)x32,NULL);
                msysc$m_print_end();
                ;
            }
            else if (($temp==(i64)5)) {
                msysc$m_print_startfile(dev);
                msysc$m_print_r64((*p).xvalue,NULL);
                msysc$m_print_end();
                ;
            }
            else if (($temp==(i64)7)) {
                if (!!((*p).value)) {
                    msysc$m_print_startfile(dev);
                    msysc$m_print_str((byte*)"#",NULL);
                    msysc$m_print_nogap();
                    msysc$m_print_i64((*p).value,NULL);
                    msysc$m_print_i64((i64)(*p).slength,NULL);
                    msysc$m_print_end();
                    ;
                }
                else {
                    msysc$m_print_startfile(dev);
                    msysc$m_print_str((byte*)"NIL",NULL);
                    msysc$m_print_end();
                    ;
                }
;
            }
            else if (($temp==(i64)6)) {
                msysc$m_print_startfile(dev);
                msysc$m_print_str((!!((*p).value) ? (byte*)"True" : (byte*)"False"),NULL);
                msysc$m_print_end();
                ;
            }
            else {
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"TYPENAME(T)=",NULL);
                msysc$m_print_str(mm_lib$typename(t),NULL);
                msysc$m_print_str(mm_lib$typename((i64)mm_decls$ttbasetype[(t)]),NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                msysc$m_print_startfile(dev);
                msysc$m_print_str((byte*)"<PRINTUNIT BAD CONST PROBABLY VOID",NULL);
                msysc$m_print_end();
                ;
            }
            };
        }
;
        msysc$m_print_startfile(dev);
        msysc$m_print_str((byte*)" ",NULL);
        msysc$m_print_nogap();
        msysc$m_print_str(mm_lib$typename(t),NULL);
        msysc$m_print_end();
        ;
        if (!!((i64)(*p).isastring)) {
            msysc$m_print_startfile(dev);
            msysc$m_print_str((byte*)" <isstr>",NULL);
            msysc$m_print_end();
            ;
        }
;
        if (!!((i64)(*p).whenlabel)) {
            msysc$m_print_startfile(dev);
            msysc$m_print_str((byte*)" *L",NULL);
            msysc$m_print_nogap();
            msysc$m_print_i64((i64)(*p).whenlabel,NULL);
            msysc$m_print_end();
            ;
        }
;
    }
    else if (($temp==(i64)52)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_str(mm_lib$typename((i64)(*p).mode),NULL);
        msysc$m_print_str(mm_lib$typename((*p).value),NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)59)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_str((mm_tables$bitfieldnames[((i64)(*p).bfcode)-1] + (i64)3),NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)48) || ($temp==(i64)51)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_str((byte*)" Convmode:",NULL);
        msysc$m_print_str(mm_lib$strmode((i64)(*p).convmode,(i64)1),NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)15)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_str((byte*)"Len:",NULL);
        msysc$m_print_i64((i64)(*p).length,NULL);
        msysc$m_print_str((byte*)" Makeax:",NULL);
        msysc$m_print_i64((i64)(*p).makearray,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)42)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_str((byte*)"Offset:",NULL);
        msysc$m_print_i64((i64)(*p).offset,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)40) || ($temp==(i64)45)) {
    }
    else if (($temp==(i64)101) || ($temp==(i64)99) || ($temp==(i64)100)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_str((byte*)"#",NULL);
        msysc$m_print_nogap();
        msysc$m_print_i64((i64)(*p).index,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)88)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_str((mm_tables$sysfnnames[((i64)(*p).fnindex)-1] + (i64)3),NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)5)) {
    }
    else if (($temp==(i64)7)) {
    }
    else if (($temp==(i64)8)) {
    }
    else if (($temp==(i64)9)) {
    }
    else if (($temp==(i64)30)) {
        for (i=(i64)1;i<=(i64)4;++i) {
L137 :;
            if (((i64)(*p).cmpgenop[(i)-1] == (i64)0)) {
                goto L139 ;
            }
;
            msysc$m_print_startfile(dev);
            msysc$m_print_str(mm_tables$pclnames[((i64)(*p).cmpgenop[(i)-1])],NULL);
            msysc$m_print_nogap();
            msysc$m_print_str((byte*)" ",NULL);
            msysc$m_print_end();
            ;
L138 :;
        }
L139 :;
        ;
    }
    };
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)3) || ($temp==(i64)45) || ($temp==(i64)40) || ($temp==(i64)42) || ($temp==(i64)86) || ($temp==(i64)28) || ($temp==(i64)23)) {
        if (((i64)(*p).memmode == (i64)0)) {
        }
        else {
            msysc$m_print_startfile(dev);
            msysc$m_print_str((byte*)" WIDEN FROM:",NULL);
            msysc$m_print_str(mm_lib$strmode((i64)(*p).memmode,(i64)1),NULL);
            msysc$m_print_end();
            ;
        }
;
    }
    };
    if (!!((i64)(*p).isconst)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_str((byte*)" Is const",NULL);
        msysc$m_print_end();
        ;
    }
    else {
        msysc$m_print_startfile(dev);
        msysc$m_print_str((byte*)" Not const",NULL);
        msysc$m_print_end();
        ;
    }
;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)31) || ($temp==(i64)33) || ($temp==(i64)32) || ($temp==(i64)34) || ($temp==(i64)29) || ($temp==(i64)35) || ($temp==(i64)48) || ($temp==(i64)11) || ($temp==(i64)12) || ($temp==(i64)13) || ($temp==(i64)14)) {
        if (!!((i64)(*p).pclop)) {
            msysc$m_print_startfile(dev);
            msysc$m_print_setfmt((byte*)" Pcl<#>");
            msysc$m_print_str(mm_tables$pclnames[((i64)(*p).pclop)],NULL);
            msysc$m_print_end();
            ;
        }
        else {
            msysc$m_print_startfile(dev);
            msysc$m_print_setfmt((byte*)" no-op");
            msysc$m_print_end();
            ;
        }
;
    }
    };
    msysc$m_print_startfile(dev);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
        ($av_2 = (i64)mm_tables$jsubs[((i64)(*p).tag)]);
    for (i=(i64)1;i<=$av_2;++i) {
L140 :;
        strcpy(opndno,msysc$strint(i,0));
        mm_diags$printunitlist(dev,(struct mm_decls$unitrec *)(*p).abc[(i)-1],(level + (i64)1),opndno);
L141 :;
    }
L142 :;
    ;
}

static void mm_diags$printunitlist(void *dev,struct mm_decls$unitrec *p,i64 level,u8 *prefix) {
    if ((p == 0)) {
        return;
    }
;
    L143 :;
    while (!!(p)) {
        mm_diags$printunit((struct mm_decls$unitrec *)p,level,prefix,dev);
        p = (struct mm_decls$unitrec *)(*p).nextunit;
L144 :;
    }
L145 :;
    ;
}

static u8 *mm_diags$getprefix(i64 level,u8 *prefix,struct mm_decls$unitrec *p) {
        static u8 str[1024];
        u8 indentstr[1024];
        u8 modestr[16384];
        u8 *  isexpr;
        i64 $av_1;
    indentstr[((i64)1)-1] = (u64)0u;
    if ((level > (i64)10)) {
        level = (i64)10;
    }
;
    $av_1 = level;
    while ($av_1-- > 0) {
L146 :;
        strcat((u8 *)indentstr,(byte*)"- ");
L147 :;
    }
L148 :;
    ;
    isexpr = (byte*)"-";
    if (!!((i64)mm_tables$jisexpr[((i64)(*p).tag)])) {
        isexpr = (byte*)"x";
    }
;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)90) || ($temp==(i64)105) || ($temp==(i64)103) || ($temp==(i64)108)) {
        if (((i64)(*p).mode == (i64)0)) {
            isexpr = (byte*)"x";
        }
;
    }
    };
    msysc$m_print_startstr(modestr);
    msysc$m_print_setfmt((byte*)"#<#> #:#");
    msysc$m_print_str(isexpr,NULL);
    msysc$m_print_str((!!((i64)(*p).simple) ? (byte*)"S" : (byte*)"-"),NULL);
    msysc$m_print_str((!!((i64)(*p).resultflag) ? (byte*)"RES" : (byte*)"---"),NULL);
    msysc$m_print_str(mm_lib$strmode((i64)(*p).mode,(i64)1),NULL);
    msysc$m_print_end();
    ;
    modestr[((i64)256)-1] = (u64)0u;
    strcat((u8 *)modestr,(byte*)"-----------------------------");
    modestr[((i64)17)-1] = ' ';
    modestr[((i64)18)-1] = (u64)0u;
    strcpy((u8 *)str,mm_diags$getlineinfok());
    strcat((u8 *)str,(u8 *)modestr);
    strcat((u8 *)str,(u8 *)indentstr);
    strcat((u8 *)str,prefix);
    if (!!((u64)(*prefix))) {
        strcat((u8 *)str,(byte*)" ");
    }
;
    return (u8 *)str;
}

static u8 *mm_diags$getlineinfok(void) {
        static u8 str[40];
    msysc$m_print_startstr(str);
    msysc$m_print_setfmt((byte*)"# # ");
    msysc$m_print_i64(mm_diags$currfileno,(byte*)"Z2");
    msysc$m_print_i64(mm_diags$currlineno,(byte*)"z4");
    msysc$m_print_end();
    ;
    return (u8 *)str;
}

void mm_diags$printmodelist(void *f) {
        i64 mbase;
        static u8 *  tab = (byte*)"\t";
        i64 $av_1;
        i64 i;
        i64 m;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"NTYPENAMES=",NULL);
    msysc$m_print_i64(mm_decls$ntypenames,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=mm_decls$ntypenames;++i) {
L149 :;
        msysc$m_print_startfile(f);
        msysc$m_print_i64(i,NULL);
        msysc$m_print_str((*mm_decls$typenames[(i)].def).name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L150 :;
    }
L151 :;
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"MODELIST",NULL);
    msysc$m_print_i64(mm_decls$ntypes,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (m=(i64)0;m<=mm_decls$ntypes;++m) {
L152 :;
        msysc$m_print_startfile(f);
        msysc$m_print_i64(m,(byte*)"4");
        msysc$m_print_str(mm_lib$strmode(m,(i64)1),NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        mbase = (i64)mm_decls$ttbasetype[(m)];
        msysc$m_print_startfile(f);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_str((byte*)"Basetype:",NULL);
        msysc$m_print_i64(mbase,NULL);
        msysc$m_print_str(mm_lib$strmode(mbase,(i64)1),NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_str((byte*)"ttname:",NULL);
        msysc$m_print_str(mm_decls$ttname[(m)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_str((byte*)"ttnamedef:",NULL);
        msysc$m_print_ptr(mm_decls$ttnamedef[(m)],NULL);
        msysc$m_print_str((!!(mm_decls$ttnamedef[(m)]) ? (*mm_decls$ttnamedef[(m)]).name : (byte*)"-"),NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_str((byte*)"Target:",NULL);
        msysc$m_print_str(mm_lib$strmode((i64)mm_decls$tttarget[(m)],(i64)1),NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_str((byte*)"Size:",NULL);
        msysc$m_print_i64((i64)mm_decls$ttsize[(m)],NULL);
        msysc$m_print_str((byte*)"Sizeset",NULL);
        msysc$m_print_i64((i64)mm_decls$ttsizeset[(m)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_setfmt((byte*)"# Bounds: #..#  Length:#");
        msysc$m_print_str(tab,NULL);
        msysc$m_print_i64((i64)mm_decls$ttlower[(m)],NULL);
        msysc$m_print_i64((((i64)mm_decls$ttlower[(m)] + (i64)mm_decls$ttlength[(m)]) - (i64)1),NULL);
        msysc$m_print_i64((i64)mm_decls$ttlength[(m)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        if ((mbase == (i64)27)) {
            msysc$m_print_startfile(f);
            msysc$m_print_str(tab,NULL);
            msysc$m_print_str((byte*)"Mult:",NULL);
            msysc$m_print_end();
            ;
                        ($av_1 = (i64)mm_decls$ttlength[(m)]);
            for (i=(i64)1;i<=$av_1;++i) {
L155 :;
                msysc$m_print_startfile(f);
                msysc$m_print_str(mm_lib$strmode((i64)(*mm_decls$ttmult[(m)])[(i)-1],(i64)1),NULL);
                msysc$m_print_nogap();
                msysc$m_print_str((byte*)" ",NULL);
                msysc$m_print_end();
                ;
L156 :;
            }
L157 :;
            ;
            msysc$m_print_startfile(f);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
;
        msysc$m_print_startfile(f);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_str((byte*)"Signed:",NULL);
        msysc$m_print_i64((i64)mm_decls$ttsigned[(m)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_str((byte*)"Isreal:",NULL);
        msysc$m_print_i64((i64)mm_decls$ttisreal[(m)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_str((byte*)"Isinteger:",NULL);
        msysc$m_print_i64((i64)mm_decls$ttisinteger[(m)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_str((byte*)"Isshort:",NULL);
        msysc$m_print_i64((i64)mm_decls$ttisshort[(m)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_str((byte*)"Isref:",NULL);
        msysc$m_print_i64((i64)mm_decls$ttisref[(m)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_str((byte*)"Isblock:",NULL);
        msysc$m_print_i64((i64)mm_decls$ttisblock[(m)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_str((byte*)"Cat:",NULL);
        msysc$m_print_str(mm_tables$catnames[((i64)mm_decls$ttcat[(m)])],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L153 :;
    }
L154 :;
    ;
}

void mm_diags$showprojectinfo(void *dev) {
        struct mm_decls$modulerec *  pm;
        struct mm_decls$subprogrec *  ps;
        static u8 *  tab = (byte*)"    ";
        i64 i;
        i64 j;
    msysc$m_print_startfile(dev);
    msysc$m_print_str((byte*)"Project Structure:",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(dev);
    msysc$m_print_str((byte*)"---------------------------------------",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(dev);
    msysc$m_print_str((byte*)"Modules",NULL);
    msysc$m_print_i64(mm_decls$nmodules,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=mm_decls$nmodules;++i) {
L158 :;
        pm = &mm_decls$moduletable[(i)];
        if (((i > (i64)1) && ((i64)(*pm).subprogno != (i64)mm_decls$moduletable[((i - (i64)1))].subprogno))) {
            msysc$m_print_startfile(dev);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
;
        msysc$m_print_startfile(dev);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_i64(i,(byte*)"2");
        msysc$m_print_nogap();
        msysc$m_print_str((((i64)mm_decls$subprogtable[((i64)mm_decls$moduletosub[(i)])].firstmodule == i) ? (byte*)"*" : (byte*)" "),NULL);
        msysc$m_print_str((*pm).name,(byte*)"16jl");
        msysc$m_print_str((byte*)"Sys:",NULL);
        msysc$m_print_i64((i64)(*pm).issyslib,NULL);
        msysc$m_print_str((byte*)"Path:",NULL);
        msysc$m_print_str((*pm).path,NULL);
        msysc$m_print_str((byte*)"Sub:",NULL);
        msysc$m_print_str(mm_decls$subprogtable[((i64)(*pm).subprogno)].name,NULL);
        msysc$m_print_str((byte*)"Fileno:",NULL);
        msysc$m_print_i64((i64)(*pm).fileno,NULL);
        msysc$m_print_end();
        ;
        if (!!((*pm).stmacro)) {
            msysc$m_print_startfile(dev);
            msysc$m_print_str((byte*)" Alias:",NULL);
            msysc$m_print_str((*(*pm).stmacro).name,NULL);
            msysc$m_print_end();
            ;
        }
;
        if (!!((*pm).stmain)) {
            msysc$m_print_startfile(dev);
            msysc$m_print_space();
            msysc$m_print_str((*(*pm).stmain).name,NULL);
            msysc$m_print_str((byte*)":",NULL);
            msysc$m_print_str(mm_tables$scopenames[((i64)(*(*pm).stmain).scope)],NULL);
            msysc$m_print_ptr((*pm).stmain,NULL);
            msysc$m_print_end();
            ;
        }
;
        if (!!((*pm).ststart)) {
            msysc$m_print_startfile(dev);
            msysc$m_print_space();
            msysc$m_print_str((*(*pm).ststart).name,NULL);
            msysc$m_print_str((byte*)":",NULL);
            msysc$m_print_str(mm_tables$scopenames[((i64)(*(*pm).ststart).scope)],NULL);
            msysc$m_print_ptr((*pm).ststart,NULL);
            msysc$m_print_end();
            ;
        }
;
        msysc$m_print_startfile(dev);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L159 :;
    }
L160 :;
    ;
    msysc$m_print_startfile(dev);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(dev);
    msysc$m_print_str((byte*)"Subprograms",NULL);
    msysc$m_print_i64(mm_decls$nsubprogs,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=mm_decls$nsubprogs;++i) {
L161 :;
        ps = &mm_decls$subprogtable[(i)];
        msysc$m_print_startfile(dev);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_i64(i,NULL);
        msysc$m_print_str((*ps).name,NULL);
        msysc$m_print_str((byte*)"Sys:",NULL);
        msysc$m_print_i64((*ps).issyslib,NULL);
        msysc$m_print_str((byte*)"Path:",NULL);
        msysc$m_print_str((*ps).path,NULL);
        msysc$m_print_str((byte*)"Fileno:",NULL);
        msysc$m_print_i64((*ps).fileno,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        if (!!((i64)(*ps).firstmodule)) {
            msysc$m_print_startfile(dev);
            msysc$m_print_str(tab,NULL);
            msysc$m_print_str(tab,NULL);
            msysc$m_print_end();
            ;
            for (j=(i64)1;j<=mm_decls$nmodules;++j) {
L164 :;
                if (((i64)mm_decls$moduletable[(j)].subprogno == i)) {
                    msysc$m_print_startfile(dev);
                    msysc$m_print_space();
                    msysc$m_print_str(mm_decls$moduletable[(j)].name,NULL);
                    msysc$m_print_end();
                    ;
                    if (((i64)(*ps).firstmodule == j)) {
                        msysc$m_print_startfile(dev);
                        msysc$m_print_str((byte*)"*",NULL);
                        msysc$m_print_end();
                        ;
                    }
;
                }
;
L165 :;
            }
L166 :;
            ;
            msysc$m_print_startfile(dev);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
;
L162 :;
    }
L163 :;
    ;
    msysc$m_print_startfile(dev);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(dev);
    msysc$m_print_str((byte*)"Link files",NULL);
    msysc$m_print_i64(mm_decls$nlibfiles,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=mm_decls$nlibfiles;++i) {
L167 :;
        msysc$m_print_startfile(dev);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_str(mm_decls$libfiles[(i)],(byte*)"16jl");
        msysc$m_print_str((((i64)mm_decls$libtypes[(i)] == (i64)68) ? (byte*)"DLL" : (byte*)"LIB"),NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L168 :;
    }
L169 :;
    ;
    msysc$m_print_startfile(dev);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

void mm_diags$showlogfile(void) {
        u8 str[256];
        void *  logdev;
        i64 size;
    if (!(!!(mm_decls$debugmode))) {
        return;
    }
;
    logdev = fopen((byte*)"mx.log",(byte*)"w");
    if (!!((i64)mm_decls$fshowmodules)) {
        mm_diags$showprojectinfo(logdev);
    }
;
    if ((!!((i64)mm_decls$fshowasm) && (mm_decls$passlevel >= (i64)8))) {
        msysc$m_print_startfile(logdev);
        msysc$m_print_str((byte*)"PROC",NULL);
        msysc$m_print_str((byte*)"C-CODE",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        mm_lib$addtolog(mm_decls$asmfilename,logdev);
    }
;
    if ((!!((i64)mm_decls$fshowpcl) && (mm_decls$passlevel >= (i64)7))) {
        msysc$m_print_startfile(logdev);
        msysc$m_print_str((byte*)"PROC PCL",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        mm_lib$addtolog(mm_decls$pclfilename,logdev);
    }
;
    if ((!!((i64)mm_decls$fshowast3) && (mm_decls$passlevel >= (i64)6))) {
        mm_lib$addtolog((byte*)"AST3",logdev);
    }
;
    if ((!!((i64)mm_decls$fshowast2) && (mm_decls$passlevel >= (i64)5))) {
        mm_lib$addtolog((byte*)"AST2",logdev);
    }
;
    if ((!!((i64)mm_decls$fshowast1) && (mm_decls$passlevel >= (i64)3))) {
        mm_lib$addtolog((byte*)"AST1",logdev);
    }
;
    if (!!((i64)mm_decls$fshowst)) {
        mm_diags$showsttree((byte*)"SYMBOL TABLE",logdev);
    }
;
    if (!!((i64)mm_decls$fshowstflat)) {
        mm_diags$showstflat((byte*)"FLAT SYMBOL TABLE",logdev);
    }
;
    if (!!((i64)mm_decls$fshowtypes)) {
        mm_diags$printmodelist(logdev);
    }
;
    if (!!((i64)mm_decls$fshowoverloads)) {
        mm_diags$printoverloads(logdev);
    }
;
    size = mlib$getfilesize(logdev);
    fclose(logdev);
    if (!!(size)) {
        msysc$m_print_startstr(str);
        msysc$m_print_str((byte*)"c:\\m\\ed.bat -w ",NULL);
        msysc$m_print_str((byte*)"mx.log",NULL);
        msysc$m_print_end();
        ;
        if (!!(mlib$checkfile((byte*)"mm.m"))) {
            mlinux$os_execwait((u8 *)str,(i64)1,0);
        }
        else {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"Diagnostic outputs written to",NULL);
            msysc$m_print_str((byte*)"mx.log",NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
;
    }
;
}

static void mm_diags$showstflat(u8 *caption,void *f) {
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"PROC",NULL);
    msysc$m_print_str(caption,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mm_diags$printstflat(f);
    msysc$m_print_startfile(f);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

static void mm_diags$showsttree(u8 *caption,void *f) {
        struct mm_decls$procrec *  pp;
        struct mm_decls$strec *  d;
        i64 i;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"PROC",NULL);
    msysc$m_print_str(caption,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mm_diags$printst(f,(struct mm_decls$strec *)mm_decls$stprogram,(i64)0);
    msysc$m_print_startfile(f);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"Proc List:",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    pp = (struct mm_decls$procrec *)mm_decls$proclist;
    L170 :;
    while (!!(pp)) {
        d = (*pp).def;
        msysc$m_print_startfile(f);
        msysc$m_print_setfmt((byte*)"#\t#.# (#) Mod:");
        msysc$m_print_ptr(d,NULL);
        msysc$m_print_str((*(*d).owner).name,NULL);
        msysc$m_print_str((*d).name,(byte*)"20jl");
        msysc$m_print_str(mm_tables$namenames[((i64)(*d).nameid)],NULL);
        msysc$m_print_i64((i64)(*d).moduleno,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        pp = (struct mm_decls$procrec *)(*pp).nextproc;
L171 :;
    }
L172 :;
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"End\n",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"DLL Proc List:",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=mm_decls$ndllproctable;++i) {
L173 :;
        d = mm_decls$dllproctable[(i)-1];
        msysc$m_print_startfile(f);
        msysc$m_print_setfmt((byte*)"#\t#.# (#) Mod: # # #");
        msysc$m_print_ptr(d,NULL);
        msysc$m_print_str((*(*d).owner).name,NULL);
        msysc$m_print_str((*d).name,(byte*)"20jl");
        msysc$m_print_str(mm_tables$namenames[((i64)(*d).nameid)],NULL);
        msysc$m_print_i64((i64)(*d).moduleno,NULL);
        msysc$m_print_str(mm_decls$libfiles[((i64)(*d).dllindex)],NULL);
        msysc$m_print_i64((i64)mm_decls$libtypes[((i64)(*d).dllindex)],(byte*)"c");
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L174 :;
    }
L175 :;
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"End\n",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

void mm_diags$showast(u8 *filename) {
        void *  f;
    f = fopen(filename,(byte*)"w");
    if (!(!!(f))) {
        return;
    }
;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"PROC",NULL);
    msysc$m_print_str(filename,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mm_diags$printcode(f,(byte*)"");
    msysc$m_print_startfile(f);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    fclose(f);
}

void mm_diags$printsymbol(struct mm_decls$tokenrec *lp) {
        struct mm_decls$tokenrec l;
    l = (*lp);
    printf((byte*)"%-18s",mm_tables$symbolnames[((i64)l.symbol)-1]);
    switch ((i64)l.symbol) {
    case 81:;
        {
            mm_lex$printstrn((*l.symptr).name,(i64)(*l.symptr).namelen);
            if (!!((i64)l.subcode)) {
                msysc$m_print_startcon();
                msysc$m_print_setfmt((byte*)" [#]");
                msysc$m_print_str(mm_tables$symbolnames[((i64)l.subcode)-1],NULL);
                msysc$m_print_end();
                ;
            }
;
        }
        break;
    case 72:;
        {
                        {i64 $temp = (i64)l.subcode;
if (($temp==(i64)3)) {
                msysc$m_print_startcon();
                msysc$m_print_i64(l.value,NULL);
                msysc$m_print_str((byte*)"int",NULL);
                msysc$m_print_end();
                ;
            }
            else if (($temp==(i64)2)) {
                msysc$m_print_startcon();
                msysc$m_print_u64(l.uvalue,NULL);
                msysc$m_print_str((byte*)"word",NULL);
                msysc$m_print_end();
                ;
            }
            else {
                msysc$m_print_startcon();
                msysc$m_print_i64(l.value,NULL);
                msysc$m_print_end();
                ;
            }
            };
        }
        break;
    case 74:;
        {
            msysc$m_print_startcon();
            msysc$m_print_r64(l.xvalue,NULL);
            msysc$m_print_end();
            ;
        }
        break;
    case 77:;
        {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"\"",NULL);
            msysc$m_print_end();
            ;
            msysc$printstr(l.svalue);
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"\"",NULL);
            msysc$m_print_i64(strlen(l.svalue),NULL);
            msysc$m_print_end();
            ;
        }
        break;
    case 75:;
        {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"'",NULL);
            msysc$m_print_end();
            ;
            msysc$printstr(l.svalue);
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"'",NULL);
            msysc$m_print_end();
            ;
        }
        break;
    case 73:;
        {
            msysc$printstr(l.svalue);
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"L",NULL);
            msysc$m_print_end();
            ;
        }
        break;
    case 9:;
    case 25:;
    case 19:;
    case 10:;
    case 28:;
    case 45:;
    case 46:;
    case 48:;
    case 49:;
    case 31:;
    case 32:;
    case 33:;
    case 34:;
    case 35:;
    case 36:;
    case 38:;
    case 39:;
    case 40:;
    case 41:;
    case 42:;
    case 43:;
    case 44:;
    case 50:;
    case 51:;
        {
            msysc$m_print_startcon();
            msysc$m_print_str(mm_tables$symbolnames[((i64)l.symbol)-1],NULL);
            msysc$m_print_end();
            ;
        }
        break;
    default: {
        if (!!((i64)l.subcode)) {
            msysc$m_print_startcon();
            msysc$m_print_setfmt((byte*)"SUBCODE:");
            msysc$m_print_i64((i64)l.subcode,NULL);
            msysc$m_print_end();
            ;
        }
;
    }
    } //SW
;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

// START
void mm_diags$start(void) {

}

void mm_export$writeexports(u8 *outfile,u8 *modulename) {
        struct mm_decls$strec *  d;
        struct mm_decls$procrec *  pp;
        void *  f;
        i64 i;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Writing exports file to",NULL);
    msysc$m_print_str(outfile,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mlib$gs_init((struct mlib$strbuffer *)mm_export$dest);
    mm_export$wxstr((byte*)"importlib ");
    mm_export$wxstr(modulename);
    mm_export$wxstrln((byte*)" =");
    for (i=(i64)29;i<=mm_decls$ntypes;++i) {
L176 :;
        d = (struct mm_decls$strec *)mm_decls$ttnamedef[(i)];
        if ((((i64)(*d).scope == (i64)3) && ((u64)(*(*d).name) != '$'))) {
                        {i64 $temp = (i64)mm_decls$ttbasetype[(i)];
if (($temp==(i64)8)) {
                mm_export$exportrecord((struct mm_decls$strec *)d);
            }
            else {
                mm_export$wxstr((byte*)"    type ");
                mm_export$wxstr((*d).name);
                mm_export$wxstr((byte*)" = ");
                mm_export$wxstr(mm_lib$strmode((i64)(*d).mode,(i64)0));
                mm_export$wxline();
            }
            };
        }
;
L177 :;
    }
L178 :;
    ;
    pp = (struct mm_decls$procrec *)mm_decls$staticlist;
    L179 :;
    while (!!(pp)) {
        d = (struct mm_decls$strec *)(*pp).def;
        if (((i64)(*d).scope == (i64)3)) {
            mm_export$exportstatic((struct mm_decls$strec *)d);
        }
;
L180 :;
        pp = (struct mm_decls$procrec *)(*pp).nextproc;
L182 :;
            }
L181 :;
    ;
    if (!!(mm_decls$staticlist)) {
        mm_export$wxline();
    }
;
    pp = (struct mm_decls$procrec *)mm_decls$constlist;
    L183 :;
    while (!!(pp)) {
        d = (struct mm_decls$strec *)(*pp).def;
        if (((i64)(*d).scope == (i64)3)) {
            mm_export$exportconst((struct mm_decls$strec *)d);
        }
;
L184 :;
        pp = (struct mm_decls$procrec *)(*pp).nextproc;
L186 :;
            }
L185 :;
    ;
    if (!!(mm_decls$constlist)) {
        mm_export$wxline();
    }
;
    pp = (struct mm_decls$procrec *)mm_decls$proclist;
    L187 :;
    while (!!(pp)) {
        d = (struct mm_decls$strec *)(*pp).def;
        if (((i64)(*d).scope == (i64)3)) {
            mm_export$exportproc((struct mm_decls$strec *)d);
        }
;
L188 :;
        pp = (struct mm_decls$procrec *)(*pp).nextproc;
L190 :;
            }
L189 :;
    ;
    mm_export$wxstrln((byte*)"end importlib");
    f = fopen(outfile,(byte*)"wb");
    mlib$gs_println((struct mlib$strbuffer *)mm_export$dest,f);
    fclose(f);
}

static void mm_export$exportstatic(struct mm_decls$strec *d) {
    mm_export$wxstr((byte*)"    var ");
    mm_export$wxmode((i64)(*d).mode);
    mm_export$wxstr((byte*)" ");
    mm_export$wxstr((*d).name);
    mm_export$wxline();
}

static void mm_export$exportconst(struct mm_decls$strec *d) {
    mm_export$wxstr((byte*)"    const ");
    mm_export$wxmode((i64)(*d).mode);
    mm_export$wxstr((byte*)" ");
    mm_export$wxstr((*d).name);
    mm_export$wxstr((byte*)" = ");
    mm_lib$jevalx((struct mlib$strbuffer *)mm_export$dest,(struct mm_decls$unitrec *)(*d).code);
    mm_export$wxline();
}

static void mm_export$exportproc(struct mm_decls$strec *d) {
        struct mm_decls$strec *  e;
        i64 currmode;
        i64 needcomma;
    mm_export$wxstr((byte*)"    ");
    mm_export$wxstr((((i64)(*d).mode == (i64)0) ? (byte*)"proc " : (byte*)"func "));
    mm_export$wxstr((*d).name);
    mm_export$wxstr((byte*)"(");
    e = (struct mm_decls$strec *)(*d).deflist;
    needcomma = (i64)0;
    currmode = (i64)0;
    L191 :;
    while (!!(e)) {
        if (((i64)(*e).nameid == (i64)13)) {
            if (!!(needcomma)) {
                mm_export$wxstr((byte*)",");
            }
;
            if (((i64)(*e).parammode != (i64)2)) {
                if (((i64)(*e).mode != currmode)) {
                    mm_export$wxmode((i64)(*e).mode);
                    mm_export$wxstr((byte*)" ");
                    currmode = (i64)(*e).mode;
                }
;
            }
            else {
                mm_export$wxmode((i64)mm_decls$tttarget[((i64)(*e).mode)]);
                mm_export$wxstr((byte*)" &");
                currmode = (i64)0;
            }
;
            mm_export$wxstr((*e).name);
            if (!!((*e).code)) {
                mm_export$wxstr((byte*)"=");
                if (((!!((i64)mm_decls$ttisref[((i64)(*e).mode)]) && ((i64)(*(*e).code).tag == (i64)1)) && ((*(*e).code).value == (i64)0))) {
                    mm_export$wxstr((byte*)"nil");
                }
                else {
                    mm_lib$jevalx((struct mlib$strbuffer *)mm_export$dest,(struct mm_decls$unitrec *)(*e).code);
                }
;
            }
;
            needcomma = (i64)1;
        }
;
        e = (struct mm_decls$strec *)(*e).nextdef;
L192 :;
    }
L193 :;
    ;
    mm_export$wxstr((byte*)")");
    if (!!((i64)(*d).mode)) {
        mm_export$wxstr((byte*)" => ");
        mm_export$wxmode((i64)(*d).mode);
    }
;
    mm_export$wxline();
}

static void mm_export$wxstr(u8 *s) {
    mlib$gs_str((struct mlib$strbuffer *)mm_export$dest,s);
}

static void mm_export$wxstrln(u8 *s) {
    mlib$gs_strln((struct mlib$strbuffer *)mm_export$dest,s);
}

static void mm_export$wxline(void) {
    mlib$gs_line((struct mlib$strbuffer *)mm_export$dest);
}

static void mm_export$exportrecord(struct mm_decls$strec *d) {
        struct mm_decls$strec *  e;
        u8 *  flags;
        i64 flag;
        i64 indent;
        i64 $av_1;
        i64 $av_2;
        i64 $av_3;
        i64 $av_4;
    e = (struct mm_decls$strec *)(*d).deflist;
    mm_export$wxstr((byte*)"    record ");
    mm_export$wxstr((*d).name);
    mm_export$wxstr((byte*)" = ");
    mm_export$wxline();
    indent = (i64)2;
    L194 :;
    while (!!(e)) {
        if (((i64)(*e).nameid == (i64)14)) {
            flags = (u8 *)&(*e).uflags;
            L197 :;
                        {u64 $temp = (u64)(*flags);
if (($temp=='S')) {
                $av_1 = indent;
                while ($av_1-- > 0) {
L199 :;
                    mm_export$wxstr((byte*)"    ");
L200 :;
                }
L201 :;
                ;
                mm_export$wxstrln((byte*)"struct");
                ++(indent);
                ++(flags);
            }
            else if (($temp=='U')) {
                $av_2 = indent;
                while ($av_2-- > 0) {
L202 :;
                    mm_export$wxstr((byte*)"    ");
L203 :;
                }
L204 :;
                ;
                mm_export$wxstrln((byte*)"union");
                ++(indent);
                ++(flags);
            }
            else {
                goto L198 ;
            }
            }goto L197 ;
L198 :;
            ;
            $av_3 = indent;
            while ($av_3-- > 0) {
L205 :;
                mm_export$wxstr((byte*)"    ");
L206 :;
            }
L207 :;
            ;
            mm_export$wxmode((i64)(*e).mode);
            mm_export$wxstr((byte*)" ");
            mm_export$wxstrln((*e).name);
            L208 :;
            while (1) {
                flag = (i64)(u64)(*(flags)++);
                if ((flag==(i64)42)) {
                }
                else if ((flag==(i64)69)) {
                    --(indent);
                    $av_4 = indent;
                    while ($av_4-- > 0) {
L210 :;
                        mm_export$wxstr((byte*)"    ");
L211 :;
                    }
L212 :;
                    ;
                    mm_export$wxstrln((byte*)"end");
                }
                else {
                    goto L209 ;
                }
;
            }
L209 :;
            ;
        }
;
        e = (struct mm_decls$strec *)(*e).nextdef;
L195 :;
    }
L196 :;
    ;
    mm_export$wxstrln((byte*)"    end");
    mm_export$wxline();
}

static void mm_export$wxmode(i64 mode) {
        u8 *  name;
    if ((mode >= (i64)29)) {
        name = (*mm_decls$ttnamedef[(mode)]).name;
        if (((u64)(*name) != '$')) {
            mm_export$wxstr(name);
            return;
        }
;
    }
;
    mm_export$wxstr(mm_lib$strmode(mode,(i64)0));
}

// START
void mm_export$start(void) {

}

void mc_genc$codegen_clang(void) {
    if ((mm_decls$fverbose > (i64)1)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Generating C code:",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    mc_genc$cfilename = mm_decls$asmfilename;
    mc_genc$allsymbols = (mc_genc$allsymbolsx = 0);
    mc_genc$scansymbol(mm_decls$stprogram);
    mlib$gs_init((struct mlib$strbuffer *)mm_lib$dest);
    mc_libc$ccinitline();
    if (!(((i64)mm_decls$msyslevel == (i64)0))) {
        mc_genc$do_cheader(mc_genc$cfilename);
    }
;
    mc_genc$do_alltypes();
    mc_genc$do_allprocdecls();
    mc_genc$do_allvars();
    mc_genc$do_allprocdefs();
    mc_libc$cclinecomment((byte*)"********** End of C Code **********");
}

static void mc_genc$do_infoheader(u8 *cfilename) {
        i64 i;
    mc_libc$ccstrline((byte*)"/*");
    mc_libc$ccstrline((byte*)"  M to C  Whole Program Translator");
    mc_libc$ccstr((byte*)"  Input:  ",(i64)0);
    mc_libc$ccstr(mm_cli$inputfile,(i64)0);
    mc_libc$ccstrline((byte*)" plus imported modules");
    mc_libc$ccstr((byte*)"  Output: ",(i64)0);
    mc_libc$ccstr(cfilename,(i64)0);
    mc_libc$ccstrline((byte*)" (this file, or renamed from that)");
    mc_libc$ccstrline((byte*)"          File represents entire program");
    mc_libc$ccstrline((byte*)"  Target: C 64-bit");
    if (!!((i64)mm_decls$fwindows)) {
        mc_libc$ccstrline((byte*)"  OS:     Windows");
    }
    else {
        mc_libc$ccstrline((byte*)"  OS:     Other");
    }
;
    mc_libc$ccstrline((byte*)"");
    mc_libc$ccstrline((byte*)"  Modules:");
    for (i=(i64)1;i<=mm_decls$nmodules;++i) {
L213 :;
        mc_libc$ccstr((byte*)"  Module ",(i64)0);
        mc_libc$ccint(i);
        mc_libc$ccstr((byte*)": ",(i64)0);
        mc_libc$ccstrline(mm_decls$sourcefilespecs[((i64)mm_decls$moduletable[(i)].fileno)]);
L214 :;
    }
L215 :;
    ;
    mc_libc$ccstrline((byte*)"");
    mc_libc$ccstrline((byte*)"*********** Start of C Code **********/");
    mc_libc$ccblank();
}

static void mc_genc$do_cheader(u8 *cfilename) {
    mc_libc$ccstrline((byte*)"\n#pragma GCC diagnostic ignored \"-Wbuiltin-declaration-mismatch\"\n\n//#include <stdio.h>\n//#include <stdlib.h>\n//#include <ctype.h>\n//#include <string.h>\n//#include <math.h>\n//#include <time.h>\n//#include <sys/types.h>\n//#include <sys/stat.h>\n\n//#pragma pack(1)\n\ntypedef signed char         i8;\ntypedef short              i16;\ntypedef int                i32;\ntypedef long long          i64;\n\ntypedef unsigned char       u8;\ntypedef unsigned short     u16;\ntypedef unsigned int       u32;\ntypedef unsigned long long u64;\n\ntypedef unsigned char     byte;\n\ntypedef float              r32;\ntypedef double             r64;\n\nextern r64 sqrt(r64);\nextern r64 fabs(r64);\n\ntypedef struct {void* ptr; u64 length;} Slice;\n\n#define NULL ((void*)0)\n\n#ifndef CALLBACK\n#define CALLBACK\n#endif\n\ni64 m$llabs(i64 a){ return (a>=0?a:-a);}\n#define m$infinity (1.0/0.0)\n");
    mc_libc$ccsendline();
}

static void mc_genc$do_alltypes(void) {
        i64 i;
    mc_libc$cclinecomment((byte*)"Forward Struct Declarations");
    for (i=(i64)29;i<=mm_decls$ntypes;++i) {
L216 :;
        if ((((i64)mm_decls$ttbasetype[(i)] == (i64)8) && ((i64)mm_decls$tttarget[(i)] == (i64)0))) {
            mc_genc$do_typedef_fwd(mm_decls$ttnamedef[(i)]);
        }
;
L217 :;
    }
L218 :;
    ;
    mc_libc$ccblank();
    if ((mm_decls$ntypes >= (i64)29)) {
        mc_libc$cclinecomment((byte*)"Struct Definitions");
    }
;
    for (i=(i64)29;i<=mm_decls$ntypes;++i) {
L219 :;
        if ((((i64)mm_decls$ttbasetype[(i)] == (i64)8) && ((i64)mm_decls$tttarget[(i)] == (i64)0))) {
            mc_genc$do_typedef(mm_decls$ttnamedef[(i)]);
        }
;
L220 :;
    }
L221 :;
    ;
    mc_libc$ccblank();
}

static void mc_genc$scansymbol(struct mm_decls$strec *d) {
        struct mm_decls$strec *  e;
    mc_genc$addsymbol(d);
        {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)6) || ($temp==(i64)5) || ($temp==(i64)7)) {
        return;
    }
    };
    e = (struct mm_decls$strec *)(*d).deflist;
    L222 :;
    while (!!(e)) {
        mc_genc$scansymbol(e);
        e = (struct mm_decls$strec *)(*e).nextdef;
L223 :;
    }
L224 :;
    ;
}

static void mc_genc$addsymbol(struct mm_decls$strec *d) {
        struct mc_genc$stlinkrec *  p;
    p = (struct mc_genc$stlinkrec *)mlib$pcm_alloc((i64)16);
    (*p).def = d;
    (*p).nextsymbol = 0;
    if ((mc_genc$allsymbols == 0)) {
        mc_genc$allsymbols = (struct mc_genc$stlinkrec *)p;
    }
    else {
        (*mc_genc$allsymbolsx).nextsymbol = (struct mc_genc$stlinkrec *)p;
    }
;
    mc_genc$allsymbolsx = (struct mc_genc$stlinkrec *)p;
}

static void mc_genc$do_typedef_fwd(struct mm_decls$strec *d) {
    if (((i64)mm_decls$ttbasetype[((i64)(*d).mode)] != (i64)8)) {
        return;
    }
;
    mc_libc$genrecordfwd((i64)(*d).mode);
}

static void mc_genc$do_typedef(struct mm_decls$strec *d) {
    if (((i64)mm_decls$ttbasetype[((i64)(*d).mode)] != (i64)8)) {
        return;
    }
;
    mc_libc$genrecorddef((i64)(*d).mode);
}

static void mc_genc$do_allprocdecls(void) {
        struct mc_genc$stlinkrec *  p;
        struct mm_decls$strec *  d;
    mc_libc$cclinecomment((byte*)"PROCDECLS");
    p = (struct mc_genc$stlinkrec *)mc_genc$allsymbols;
    L225 :;
    while (!!(p)) {
        d = (*p).def;
                {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)6)) {
            mc_genc$do_procdecl(d);
        }
        else if (($temp==(i64)7)) {
            mc_genc$do_procdecl(d);
        }
        };
        p = (struct mc_genc$stlinkrec *)(*p).nextsymbol;
L226 :;
    }
L227 :;
    ;
    mc_libc$ccblank();
}

static void mc_genc$do_allvars(void) {
        struct mc_genc$stlinkrec *  p;
        struct mm_decls$strec *  d;
    mc_libc$cclinecomment((byte*)"VARS");
    p = (struct mc_genc$stlinkrec *)mc_genc$allsymbols;
    L228 :;
    while (!!(p)) {
        d = (*p).def;
                {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)11)) {
            mc_genc$do_vardef(d);
        }
        else if (($temp==(i64)8)) {
            mc_genc$do_dllvar(d);
        }
        };
        p = (struct mc_genc$stlinkrec *)(*p).nextsymbol;
L229 :;
    }
L230 :;
    ;
    mc_libc$ccblank();
}

static void mc_genc$do_allprocdefs(void) {
        struct mc_genc$stlinkrec *  p;
        struct mm_decls$strec *  d;
    mc_libc$cclinecomment((byte*)"PROCDEFS");
    p = (struct mc_genc$stlinkrec *)mc_genc$allsymbols;
    L231 :;
    while (!!(p)) {
        d = (*p).def;
        if ((((i64)(*d).nameid == (i64)6) && !!((*d).code))) {
            mc_genc$genprocdef(d);
        }
;
        p = (struct mc_genc$stlinkrec *)(*p).nextsymbol;
L232 :;
    }
L233 :;
    ;
    mc_libc$ccblank();
}

static void mc_genc$do_procdecl(struct mm_decls$strec *d) {
    if ((mm_decls$moduletable[((i64)(*d).moduleno)].stmain == d)) {
        mc_libc$ccstrline((byte*)"int main(int, char**, char**);");
        return;
    }
;
    if (((*d).code == 0)) {
        mc_libc$ccstr((byte*)"extern ",(i64)0);
    }
    else if (((i64)(*d).scope == (i64)0)) {
        mc_libc$ccstr((byte*)"static ",(i64)0);
    }
;
    mc_libc$ccstr(mc_libc$strprocsig(d,0,(i64)1),(i64)0);
    mc_libc$ccstrline((byte*)";");
}

static void mc_genc$do_vardef(struct mm_decls$strec *d) {
    if (((i64)(*d).nameid == (i64)11)) {
        mc_libc$ccstr((byte*)"static ",(i64)0);
    }
;
    mc_libc$ccstr(mc_libc$strmodec((i64)(*d).mode,mc_libc$getfullnamec(d),(i64)1),(i64)0);
    if (!!((*d).code)) {
        mc_libc$ccstr((byte*)" = ",(i64)0);
        mc_libc$do_initdata((*d).code,(i64)0,(i64)0);
    }
    else if (((u64)(*(*d).name) == '_')) {
        if (!!(mlib$eqstring((*d).name,(byte*)"_fnnprocs"))) {
            mc_genc$writefn_nprocs();
        }
        else if (!!(mlib$eqstring((*d).name,(byte*)"_fnaddresses"))) {
            mc_genc$writefn_addresses();
        }
        else if (!!(mlib$eqstring((*d).name,(byte*)"_fnnames"))) {
            mc_genc$writefn_names();
        }
;
    }
    else if (!!(msysc$m_getdotindex((i64)(*d).flags,(i64)10))) {
        mm_support$gerror((byte*)"MODULE@",0);
    }
;
    mc_libc$ccstrline((byte*)";");
}

static void mc_genc$do_dllvar(struct mm_decls$strec *d) {
    mc_libc$ccstr((byte*)"extern ",(i64)0);
    mc_libc$ccstr(mc_libc$strmodec((i64)(*d).mode,(*d).name,(i64)1),(i64)0);
    mc_libc$ccstrline((byte*)";");
}

static void mc_genc$genprocdef(struct mm_decls$strec *p) {
        u8 str[256];
        struct mm_decls$strec *  d;
        i64 ismain;
        i64 isstart;
    if (((i64)(*p).mode != (i64)0)) {
        if (!(!!(mm_lib$checkblockreturn((*p).code)))) {
            mm_support$gerror_s((byte*)"C:Function needs explicit return: ",(*p).name,0);
        }
;
    }
;
    if (!!(mlib$eqstring((*p).name,(byte*)"start"))) {
        mc_libc$ccstrline((byte*)"// START");
    }
;
    mm_decls$currproc = p;
    ismain = (isstart = (i64)0);
    if ((p==mm_decls$moduletable[((i64)(*p).moduleno)].stmain)) {
        ismain = (i64)1;
    }
    else if ((p==mm_decls$moduletable[((i64)(*p).moduleno)].ststart)) {
        isstart = (i64)1;
    }
;
    if (((i64)(*p).scope == (i64)0)) {
        mc_libc$ccstr((byte*)"static ",(i64)0);
    }
;
    if (!!(msysc$m_getdotindex((i64)(*p).flags,(i64)5))) {
        mc_libc$ccstr((byte*)"CALLBACK ",(i64)0);
    }
;
    if (!!(ismain)) {
        mc_genc$do_maininit();
    }
    else {
        mc_libc$ccstr(mc_libc$strprocsig(p,0,(i64)1),(i64)0);
        mc_libc$ccstrline((byte*)" {");
        if (!!(isstart)) {
            mc_genc$do_startinit(p);
        }
;
    }
;
    d = (struct mm_decls$strec *)(*p).deflist;
    L234 :;
    while (!!(d)) {
        switch ((i64)(*d).nameid) {
        case 11:;
        case 12:;
            {
                if ((!!(msysc$m_getdotindex((i64)(*d).flags,(i64)1)) || !!((*d).code))) {
                    mc_genc$genlocalvar(d);
                }
;
            }
            break;
        case 10:;
            {
            }
            break;
        case 13:;
            {
            }
            break;
        case 17:;
            {
            }
            break;
        case 5:;
            {
            }
            break;
        case 6:;
            {
            }
            break;
        case 18:;
            {
            }
            break;
        default: {
            msysc$m_print_startstr(str);
            msysc$m_print_setfmt((byte*)"Can't output: # in # : #");
            msysc$m_print_str((*d).name,NULL);
            msysc$m_print_str((*p).name,NULL);
            msysc$m_print_str(mm_tables$namenames[((i64)(*d).nameid)],NULL);
            msysc$m_print_end();
            ;
            mm_support$gerror((u8 *)str,0);
        }
        } //SW
;
        d = (struct mm_decls$strec *)(*d).nextdef;
L235 :;
    }
L236 :;
    ;
    mc_blockc$blocklevel = (i64)0;
    mc_blockc$do_block((*p).code,(i64)0);
    if (!!(ismain)) {
        mc_genc$do_mainterm();
    }
;
    mc_libc$cctab((i64)0);
    mc_libc$ccstr((byte*)"}",(i64)0);
    mc_libc$ccblank();
    mc_libc$ccblank();
}

static void mc_genc$do_maininit(void) {
        struct mm_decls$strec *  d;
        i64 i;
    mc_libc$ccstrline((byte*)"int main(int _nargs, char** _args, char** _envstrings) {");
    if (((i64)mm_decls$msyslevel == (i64)1)) {
        mc_libc$ccstrline((byte*)"    mminc$m_init(_nargs, (void*)_args, (void*)_envstrings);");
    }
    else {
        mc_libc$ccstrline((byte*)"    msysc$m_init(_nargs, (void*)_args, (void*)_envstrings);");
    }
;
    mc_libc$ccsendline();
    mc_libc$ccstrline((byte*)"// call main-start() routines...");
    for (i=(i64)2;i<=mm_decls$nsubprogs;++i) {
L237 :;
        d = mm_decls$moduletable[((i64)mm_decls$subprogtable[(i)].firstmodule)].ststart;
        mc_genc$docallproc(d);
L238 :;
    }
L239 :;
    ;
    d = mm_decls$moduletable[((i64)mm_decls$subprogtable[((i64)1)].firstmodule)].ststart;
    mc_genc$docallproc(d);
    mc_libc$ccsendline();
}

static void mc_genc$do_startinit(struct mm_decls$strec *p) {
        struct mm_decls$strec *  d;
        i64 m;
        i64 s;
        i64 i;
    m = (i64)(*p).moduleno;
    s = (i64)(*p).subprogno;
    if (((i64)mm_decls$subprogtable[(s)].firstmodule == m)) {
        for (i=(i64)1;i<=mm_decls$nmodules;++i) {
L240 :;
            if ((((i64)mm_decls$moduletosub[(i)] == s) && (i != m))) {
                d = mm_decls$moduletable[(i)].ststart;
                mc_genc$docallproc(d);
            }
;
L241 :;
        }
L242 :;
        ;
    }
;
    mc_libc$ccsendline();
}

static void mc_genc$do_mainterm(void) {
    mc_libc$ccstrline((byte*)"    return 0;");
}

static void mc_genc$writefn_nprocs(void) {
    mc_libc$ccstr((byte*)"=",(i64)0);
    mc_libc$ccint(mc_genc$nallprocs);
}

static void mc_genc$writefn_names(void) {
        i64 i;
        struct mm_decls$strec *  d;
    mc_libc$ccstrline((byte*)"= {");
    mc_genc$nallprocs = (i64)0;
    for (i=(i64)1;i<=mm_decls$nmodules;++i) {
L243 :;
        d = (struct mm_decls$strec *)(*mm_decls$moduletable[(i)].stmodule).deflist;
        L246 :;
        while (!!(d)) {
            if (((i64)(*d).nameid == (i64)6)) {
                ++(mc_genc$nallprocs);
                mc_libc$ccstr((byte*)"(byte*)\"",(i64)1);
                mc_libc$ccstr((*d).name,(i64)0);
                mc_libc$ccstrline((byte*)"\",");
            }
;
            d = (struct mm_decls$strec *)(*d).nextdef;
L247 :;
        }
L248 :;
        ;
L244 :;
    }
L245 :;
    ;
    mc_libc$ccstr((byte*)"(byte*)\"\"}",(i64)0);
}

static void mc_genc$writefn_addresses(void) {
        i64 i;
        struct mm_decls$strec *  d;
    mc_libc$ccstrline((byte*)"= {");
    mc_genc$nallprocs = (i64)0;
    for (i=(i64)1;i<=mm_decls$nmodules;++i) {
L249 :;
        d = (struct mm_decls$strec *)(*mm_decls$moduletable[(i)].stmodule).deflist;
        L252 :;
        while (!!(d)) {
            if (((i64)(*d).nameid == (i64)6)) {
                ++(mc_genc$nallprocs);
                mc_libc$ccstr((byte*)"&",(i64)1);
                if ((!!(mlib$eqstring((*d).name,(byte*)"main")) && ((i64)(*d).moduleno == mm_decls$mainmoduleno))) {
                    mc_libc$ccstr((byte*)"main",(i64)0);
                }
                else {
                    mc_libc$ccstr(mc_libc$getprocname(d),(i64)0);
                }
;
                mc_libc$ccstrline((byte*)",");
            }
;
            d = (struct mm_decls$strec *)(*d).nextdef;
L253 :;
        }
L254 :;
        ;
L250 :;
    }
L251 :;
    ;
    mc_libc$ccstr((byte*)"0}",(i64)0);
}

static void mc_genc$genlocalvar(struct mm_decls$strec *d) {
    mc_libc$cctab((i64)1);
    if (((i64)(*d).nameid == (i64)11)) {
        mc_libc$ccstr((byte*)"static ",(i64)1);
        mc_libc$ccstr(mc_libc$strmodec((i64)(*d).mode,(*d).name,(i64)1),(i64)0);
        if (!!((*d).code)) {
            mc_libc$ccstr((byte*)" = ",(i64)0);
            mc_libc$do_initdata((*d).code,(i64)0,(i64)0);
        }
;
    }
    else {
        mc_libc$ccstr(mc_libc$strmodec((i64)(*d).mode,(*d).name,(i64)1),(i64)1);
    }
;
    if (!!((*d).code)) {
    }
    else if (!!(msysc$m_getdotindex((i64)(*d).flags,(i64)10))) {
        mm_support$gerror_s((byte*)"LOCAL@",(*d).name,0);
    }
;
    mc_libc$ccstrline((byte*)";");
}

static void mc_genc$docallproc(struct mm_decls$strec *d) {
    mc_libc$ccstr(mc_libc$getfullnamec(d),(i64)1);
    mc_libc$ccstrline((byte*)"();");
}

// START
void mc_genc$start(void) {

}

void mm_lex$lexreadtoken(void) {
        i64 c;
        i64 hsum;
        i64 commentseen;
        i64 length;
        u8 *  lxsvalue;
        struct mm_decls$strec *  d;
    mm_decls$nextlx.subcode = (i64)0;
    L255 :;
    switch ((mm_lex$lxstart = mm_lex$lxsptr, (i64)(u64)(*(mm_lex$lxsptr)++))) {
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
    case 95:;
    case 36:;
        {
            lxsvalue = (mm_lex$lxsptr - (i64)1);
            //doname:
L257 :;
;
            hsum = (i64)(u64)(*lxsvalue);
            if (((c = (i64)(u64)(*mm_lex$lxsptr)) == (i64)34)) {
                if ((hsum==(i64)70) || (hsum==(i64)102) || (hsum==(i64)82) || (hsum==(i64)114)) {
                    mm_lex$readrawstring();
                    return;
                }
                else if ((hsum==(i64)65) || (hsum==(i64)97) || (hsum==(i64)90) || (hsum==(i64)122)) {
                    mm_lex$readarraystring((i64)(u64)(*lxsvalue));
                    return;
                }
;
            }
            else if (!!((i64)mm_lex$alphamap[(c)])) {
                L258 :;
                                {i64 $temp = (i64)mm_lex$alphamap[((c = (i64)(u64)(*mm_lex$lxsptr)))];
if (($temp==(i64)1)) {
                    hsum = (((hsum << (i64)4) - hsum) + c);
                    ++(mm_lex$lxsptr);
                }
                else if (($temp==(i64)2)) {
                    (*mm_lex$lxsptr) = (u64)(c + (i64)32);
                    hsum = (((hsum << (i64)4) - hsum) + (c + (i64)32));
                    ++(mm_lex$lxsptr);
                }
                else {
                    goto L259 ;
                }
                }goto L258 ;
L259 :;
                ;
            }
            else {
                d = mm_lex$shortnames[((i64)(u64)(*lxsvalue))-97];
                if (!!(d)) {
                    mm_decls$nextlx.symptr = (struct mm_decls$strec *)d;
                    mm_decls$nextlx.symbol = (i64)(*d).symbol;
                    mm_decls$nextlx.subcode = (i64)(*d).subcode;
                    return;
                }
;
            }
;
            mm_lex$lookup(lxsvalue,(mm_lex$lxsptr - lxsvalue),((hsum << (i64)5) - hsum));
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
            lxsvalue = (mm_lex$lxsptr - (i64)1);
            (*lxsvalue) += (u8)(i64)32;
            goto L257 ;
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
            mm_lex$lxstart = (mm_lex$lxsptr - (i64)1);
                        {u64 $temp = (u64)(*mm_lex$lxsptr);
if (($temp==')') || ($temp==(u64)13u) || ($temp==',') || ($temp==' ')) {
                mm_decls$nextlx.symbol = (i64)72;
                mm_decls$nextlx.subcode = (i64)3;
                mm_decls$nextlx.value = (i64)((u64)(*mm_lex$lxstart) - '0');
            }
            else if (($temp=='x') || ($temp=='X')) {
                                {u64 $temp = (u64)(*mm_lex$lxstart);
if (($temp=='0')) {
                    ++(mm_lex$lxsptr);
                    mm_lex$readhex();
                }
                else if (($temp=='2')) {
                    ++(mm_lex$lxsptr);
                    mm_lex$readbin();
                }
                else if (($temp=='8')) {
                    ++(mm_lex$lxsptr);
                    mm_lex$readoct();
                }
                else {
                    mm_support$lxerror((byte*)"Bad base");
                }
                };
            }
            else {
                --(mm_lex$lxsptr);
                mm_lex$readdec();
            }
            };
            return;
        }
        break;
    case 33:;
        {
            //docomment:
L260 :;
;
            L261 :;
            switch ((c = (i64)(u64)(*(mm_lex$lxsptr)++))) {
            case 13:;
                {
                    ++(mm_lex$lxalllines);
                    ++(mm_lex$lxsptr);
                    goto L262 ;
                }
                break;
            case 10:;
                {
                    ++(mm_lex$lxalllines);
                    goto L262 ;
                }
                break;
            case 0:;
                {
                    --(mm_lex$lxsptr);
                    goto L262 ;
                }
                break;
            } //SW
goto L261 ;
L262 :;
            ;
            mm_decls$nextlx.symbol = (i64)67;
            return;
        }
        break;
    case 35:;
        {
            goto L260 ;
;
            ++(mm_lex$lxsptr);
            lxsvalue = mm_lex$lxsptr;
            L263 :;
            switch ((c = (i64)(u64)(*(mm_lex$lxsptr)++))) {
            case 13:;
                {
                    ++(mm_lex$lxalllines);
                    ++(mm_lex$lxsptr);
                    goto L264 ;
                }
                break;
            case 10:;
                {
                    ++(mm_lex$lxalllines);
                    goto L264 ;
                }
                break;
            case 0:;
                {
                    --(mm_lex$lxsptr);
                    goto L264 ;
                }
                break;
            } //SW
goto L263 ;
L264 :;
            ;
            length = (mm_lex$lxsptr - lxsvalue);
            mm_decls$nextlx.symbol = (i64)70;
            mm_decls$nextlx.svalue = mlib$pcm_copyheapstringn(lxsvalue,length);
            return;
        }
        break;
    case 92:;
        {
            commentseen = (i64)0;
            L265 :;
            switch ((i64)(u64)(*(mm_lex$lxsptr)++)) {
            case 13:;
                {
                    ++(mm_lex$lxalllines);
                    ++(mm_lex$lxsptr);
                    goto L266 ;
                }
                break;
            case 10:;
                {
                    ++(mm_lex$lxalllines);
                    goto L266 ;
                }
                break;
            case 0:;
                {
                    mm_decls$nextlx.symbol = (i64)68;
                    --(mm_lex$lxsptr);
                    return;
                }
                break;
            case 32:;
            case 9:;
                {
                }
                break;
            case 33:;
                {
                    commentseen = (i64)1;
                }
                break;
            default: {
                if (!(!!(commentseen))) {
                    mm_support$lxerror((byte*)"\\ not followed by eol");
                }
;
            }
            } //SW
goto L265 ;
L266 :;
            ;
            L267 :;
            switch ((i64)(u64)(*(mm_lex$lxsptr)++)) {
            case 13:;
                {
                    ++(mm_lex$lxalllines);
                    ++(mm_lex$lxsptr);
                }
                break;
            case 10:;
                {
                    ++(mm_lex$lxalllines);
                }
                break;
            case 32:;
            case 9:;
                {
                }
                break;
            default: {
                --(mm_lex$lxsptr);
                goto L268 ;
            }
            } //SW
goto L267 ;
L268 :;
            ;
        }
        break;
    case 123:;
        {
            mm_decls$nextlx.symbol = (i64)17;
            return;
        }
        break;
    case 125:;
        {
            mm_decls$nextlx.symbol = (i64)18;
            return;
        }
        break;
    case 46:;
        {
            switch ((i64)(u64)(*mm_lex$lxsptr)) {
            case 46:;
                {
                    ++(mm_lex$lxsptr);
                    if (((u64)(*mm_lex$lxsptr) == '.')) {
                        ++(mm_lex$lxsptr);
                        mm_decls$nextlx.symbol = (i64)29;
                    }
                    else {
                        mm_decls$nextlx.symbol = (i64)28;
                        mm_decls$nextlx.subcode = (i64)16;
                    }
;
                    return;
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
                    --(mm_lex$lxsptr);
                    mm_support$lxerror((byte*)".123 not done");
                    return;
                }
                break;
            default: {
                mm_decls$nextlx.symbol = (i64)2;
                return;
            }
            } //SW
;
        }
        break;
    case 44:;
        {
            mm_decls$nextlx.symbol = (i64)5;
            return;
        }
        break;
    case 59:;
        {
            mm_decls$nextlx.symbol = (i64)6;
            return;
        }
        break;
    case 58:;
        {
            switch ((i64)(u64)(*mm_lex$lxsptr)) {
            case 61:;
                {
                    ++(mm_lex$lxsptr);
                    mm_decls$nextlx.symbol = (i64)9;
                    mm_decls$nextlx.subcode = (i64)23;
                }
                break;
            case 58:;
                {
                    ++(mm_lex$lxsptr);
                                        {u64 $temp = (u64)(*mm_lex$lxsptr);
if (($temp=='=')) {
                        ++(mm_lex$lxsptr);
                        mm_decls$nextlx.symbol = (i64)10;
                    }
                    else {
                        mm_decls$nextlx.symbol = (i64)8;
                    }
                    };
                }
                break;
            default: {
                mm_decls$nextlx.symbol = (i64)7;
            }
            } //SW
;
            return;
        }
        break;
    case 40:;
        {
            ++(mm_decls$nlbrack);
            mm_decls$nextlx.symbol = (i64)13;
            return;
        }
        break;
    case 41:;
        {
            mm_decls$nextlx.symbol = (i64)14;
            return;
        }
        break;
    case 91:;
        {
            mm_decls$nextlx.symbol = (i64)15;
            return;
        }
        break;
    case 93:;
        {
            mm_decls$nextlx.symbol = (i64)16;
            return;
        }
        break;
    case 124:;
        {
            if (((u64)(*mm_lex$lxsptr) == '|')) {
                ++(mm_lex$lxsptr);
                mm_decls$nextlx.symbol = (i64)21;
            }
            else {
                mm_decls$nextlx.symbol = (i64)20;
            }
;
            return;
        }
        break;
    case 94:;
        {
            mm_decls$nextlx.symbol = (i64)19;
            return;
        }
        break;
    case 64:;
        {
            if (((u64)(*mm_lex$lxsptr) == '@')) {
                ++(mm_lex$lxsptr);
                mm_decls$nextlx.symbol = (i64)23;
            }
            else {
                mm_decls$nextlx.symbol = (i64)22;
            }
;
            return;
        }
        break;
    case 63:;
        {
            mm_decls$nextlx.symbol = (i64)24;
            return;
        }
        break;
    case 126:;
        {
            mm_decls$nextlx.symbol = (i64)27;
            return;
        }
        break;
    case 43:;
        {
            mm_decls$nextlx.symbol = (i64)31;
            if (((u64)(*mm_lex$lxsptr) == '+')) {
                ++(mm_lex$lxsptr);
                mm_decls$nextlx.symbol = (i64)71;
                mm_decls$nextlx.subcode = (i64)53;
                ++(mm_decls$nincr);
                return;
            }
;
            return;
        }
        break;
    case 45:;
        {
            mm_decls$nextlx.symbol = (i64)32;
                        {u64 $temp = (u64)(*mm_lex$lxsptr);
if (($temp=='-')) {
                ++(mm_lex$lxsptr);
                mm_decls$nextlx.symbol = (i64)71;
                ++(mm_decls$nincr);
                mm_decls$nextlx.subcode = (i64)54;
                return;
            }
            else if (($temp=='>')) {
                ++(mm_lex$lxsptr);
                mm_decls$nextlx.symbol = (i64)12;
                return;
            }
            };
            return;
        }
        break;
    case 42:;
        {
            if (((u64)(*mm_lex$lxsptr) == '*')) {
                ++(mm_lex$lxsptr);
                mm_decls$nextlx.symbol = (i64)50;
            }
            else {
                mm_decls$nextlx.symbol = (i64)33;
            }
;
            return;
        }
        break;
    case 47:;
        {
            mm_decls$nextlx.symbol = (i64)34;
            return;
        }
        break;
    case 37:;
        {
            mm_decls$nextlx.symbol = (i64)35;
            return;
        }
        break;
    case 61:;
        {
                        {u64 $temp = (u64)(*mm_lex$lxsptr);
if (($temp=='>')) {
                mm_decls$nextlx.symbol = (i64)11;
                ++(mm_lex$lxsptr);
            }
            else if (($temp=='=')) {
                ++(mm_lex$lxsptr);
                mm_decls$nextlx.symbol = (i64)51;
            }
            else {
                mm_decls$nextlx.symbol = (i64)48;
                mm_decls$nextlx.subcode = (i64)17;
            }
            };
            return;
        }
        break;
    case 60:;
        {
            mm_decls$nextlx.symbol = (i64)49;
            switch ((i64)(u64)(*mm_lex$lxsptr)) {
            case 61:;
                {
                    ++(mm_lex$lxsptr);
                    mm_decls$nextlx.subcode = (i64)20;
                }
                break;
            case 62:;
                {
                    ++(mm_lex$lxsptr);
                    mm_decls$nextlx.subcode = (i64)18;
                }
                break;
            case 60:;
                {
                    ++(mm_lex$lxsptr);
                    mm_decls$nextlx.symbol = (i64)41;
                }
                break;
            default: {
                mm_decls$nextlx.subcode = (i64)19;
            }
            } //SW
;
            return;
        }
        break;
    case 62:;
        {
            mm_decls$nextlx.symbol = (i64)49;
            switch ((i64)(u64)(*mm_lex$lxsptr)) {
            case 61:;
                {
                    ++(mm_lex$lxsptr);
                    mm_decls$nextlx.symbol = (i64)49;
                    mm_decls$nextlx.subcode = (i64)21;
                }
                break;
            case 62:;
                {
                    ++(mm_lex$lxsptr);
                    mm_decls$nextlx.symbol = (i64)42;
                }
                break;
            default: {
                mm_decls$nextlx.symbol = (i64)49;
                mm_decls$nextlx.subcode = (i64)22;
            }
            } //SW
;
            return;
        }
        break;
    case 38:;
        {
                        {u64 $temp = (u64)(*mm_lex$lxsptr);
if (($temp=='.')) {
                ++(mm_lex$lxsptr);
                mm_decls$nextlx.symbol = (i64)4;
                mm_decls$nextlx.subcode = (i64)0;
            }
            else {
                mm_decls$nextlx.symbol = (i64)25;
                mm_decls$nextlx.subcode = (i64)46;
            }
            };
            return;
        }
        break;
    case 39:;
        {
            mm_lex$lxreadstring((i64)39);
            return;
        }
        break;
    case 34:;
        {
            mm_lex$lxreadstring((i64)34);
            return;
        }
        break;
    case 96:;
        {
            mm_lex$readrawxname();
            return;
        }
        break;
    case 32:;
    case 9:;
        {
        }
        break;
    case 13:;
        {
            ++(mm_lex$lxsptr);
            ++(mm_lex$lxalllines);
            mm_decls$nextlx.symbol = (i64)67;
            return;
        }
        break;
    case 10:;
        {
            ++(mm_lex$lxalllines);
            mm_decls$nextlx.symbol = (i64)67;
            return;
        }
        break;
    case 0:;
        {
            if (!!(mm_lex$sourcelevel)) {
                mm_lex$unstacksource();
                return;
            }
            else {
                mm_decls$nextlx.symbol = (i64)68;
                --(mm_lex$lxsptr);
                return;
            }
;
        }
        break;
    case 239:;
        {
            mm_lex$lxsptr += (i64)2;
        }
        break;
    default: {
        mm_decls$nextlx.symbol = (i64)1;
        return;
    }
    } //SW
goto L255 ;
L256 :;
    ;
}

void mm_lex$lex(void) {
        i64 lena;
        i64 lenb;
        u8 *  p;
    mm_decls$lx = mm_decls$nextlx;
    mm_decls$lx.pos = msysc$m_setdotslice(mm_decls$lx.pos,(i64)0,(i64)23,(u64)(mm_lex$lxstart - mm_lex$lxsource));
    //reenter:
L269 :;
;
    mm_lex$lexreadtoken();
    //reenter2:
L270 :;
;
    switch ((i64)mm_decls$nextlx.symbol) {
    case 107:;
    case 123:;
    case 108:;
    case 124:;
    case 111:;
    case 114:;
    case 112:;
    case 130:;
    case 131:;
    case 136:;
    case 106:;
    case 133:;
    case 134:;
    case 135:;
    case 137:;
    case 115:;
    case 152:;
    case 162:;
    case 167:;
    case 98:;
        {
            if (((i64)mm_decls$lx.symbol == (i64)105)) {
                if (!!((i64)mm_decls$lx.subcode)) {
                    mm_support$lxerror((byte*)"end if if?");
                }
;
                mm_decls$lx.subcode = (i64)mm_decls$nextlx.symbol;
                goto L269 ;
;
            }
;
        }
        break;
    case 67:;
        {
            if (((i64)mm_decls$lx.symbol == (i64)5 || (i64)mm_decls$lx.symbol == (i64)15 || (i64)mm_decls$lx.symbol == (i64)13)) {
                goto L269 ;
;
            }
            else if (((((i64)mm_tables$symboloptypes[((i64)mm_decls$lx.symbol)-1] == (i64)1) && !(!!(mm_decls$assemmode))) && !(((i64)mm_decls$lx.symbol == (i64)44 || (i64)mm_decls$lx.symbol == (i64)43)))) {
                goto L269 ;
;
            }
;
            mm_decls$nextlx.symbol = (i64)6;
            mm_decls$nextlx.subcode = (i64)1;
        }
        break;
    case 77:;
        {
            if (((i64)mm_decls$lx.symbol == (i64)77)) {
                lena = strlen(mm_decls$lx.svalue);
                lenb = strlen(mm_decls$nextlx.svalue);
                p = (u8 *)mlib$pcm_alloc(((lena + lenb) + (i64)1));
                memcpy((void *)p,(void *)mm_decls$lx.svalue,(u64)lena);
                memcpy((void *)(p + lena),(void *)mm_decls$nextlx.svalue,(u64)lenb);
                (*((p + lena) + lenb)) = (u64)0u;
                mm_decls$lx.svalue = p;
            }
;
        }
        break;
    case 82:;
        {
            if (!(!!(mm_lex$dolexdirective((i64)mm_decls$nextlx.subcode)))) {
                goto L269 ;
;
            }
;
        }
        break;
    case 81:;
        {
                        {i64 $temp = (i64)mm_decls$nextlx.subcode;
if (($temp==(i64)80)) {
                                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)72)) {
                                        {i64 $temp = (i64)(*mm_decls$nextlx.symptr).index;
if (($temp==(i64)2)) {
                        mm_decls$lx.value *= (i64)1000000;
                    }
                    else if (($temp==(i64)3)) {
                        mm_decls$lx.value *= (i64)1000000000;
                    }
                    else if (($temp==(i64)1)) {
                        mm_decls$lx.value *= (i64)1000;
                    }
                    else if (($temp==(i64)4)) {
                        mm_decls$lx.value *= (i64)1024;
                    }
                    else if (($temp==(i64)5)) {
                        mm_decls$lx.value *= (i64)1048576;
                    }
                    else if (($temp==(i64)6)) {
                        mm_decls$lx.value *= (i64)1073741824;
                    }
                    else {
                        mm_support$lxerror((byte*)"Can't do this unit index");
                    }
                    };
                    mm_decls$lx.subcode = mm_lex$setinttype((u64)mm_decls$lx.value);
                    goto L269 ;
;
                }
                else if (($temp==(i64)74)) {
                    mm_support$lxerror((byte*)"Unit suffix after float not implem");
                }
                else {
                    mm_decls$nextlx.symbol = (i64)81;
                }
                };
            }
            else if (($temp==(i64)153)) {
                if (!(!!(mm_decls$headermode))) {
                    mm_decls$nextlx.symbol = (i64)81;
                }
                else {
                    mm_decls$nextlx.symbol = (i64)153;
                    mm_decls$nextlx.subcode = (i64)(*mm_decls$nextlx.symptr).index;
                }
;
            }
            else {
                mm_decls$nextlx.symbol = (i64)81;
            }
            };
        }
        break;
    case 69:;
        {
            mm_decls$nextlx.symbol = (i64)81;
        }
        break;
    case 52:;
        {
            if (((i64)mm_decls$lx.symbol == (i64)56)) {
                mm_decls$lx.symbol = (i64)53;
                mm_decls$lx.subcode = (i64)14;
                goto L269 ;
;
            }
;
        }
        break;
    case 48:;
        {
            if (((i64)mm_decls$lx.symbol == (i64)56)) {
                mm_decls$lx.symbol = (i64)49;
                mm_decls$lx.subcode = (i64)18;
                goto L269 ;
;
            }
;
        }
        break;
    } //SW
;
    mm_decls$nextlx.pos = ((i64)mm_decls$nextlx.pos | (mm_lex$lxfileno << (i64)24));
}

void mm_lex$lexsetup(void) {
    mm_lex$inithashtable();
}

void mm_lex$printstrn(u8 *s,i64 length) {
    if (!!(length)) {
        msysc$m_print_startcon();
        msysc$m_print_i64(length,(byte*)"v");
        msysc$m_print_str(s,(byte*)".*");
        msysc$m_print_end();
        ;
    }
;
}

static void mm_lex$readrawstring(void) {
        u8 *  dest;
        i64 c;
    mm_decls$nextlx.symbol = (i64)77;
    mm_decls$nextlx.svalue = ++(mm_lex$lxsptr);
    dest = mm_lex$lxsptr;
    L271 :;
    switch ((c = (i64)(u64)(*(mm_lex$lxsptr)++))) {
    case 34:;
        {
            if (((u64)(*mm_lex$lxsptr) == '"')) {
                (*(dest)++) = '"';
                ++(mm_lex$lxsptr);
            }
            else {
                (*(mm_lex$lxsptr - (i64)1)) = (u64)0u;
                goto L272 ;
            }
;
        }
        break;
    case 13:;
    case 10:;
    case 0:;
        {
            mm_support$lxerror((byte*)"Raw string not terminated");
            --(mm_lex$lxsptr);
            goto L272 ;
        }
        break;
    default: {
        (*(dest)++) = (u64)c;
    }
    } //SW
goto L271 ;
L272 :;
    ;
}

static void mm_lex$lookup(u8 *name,i64 length,i64 hashindex0) {
        i64 wrapped;
        i64 n;
        struct mm_decls$strec *  d;
        i64 j;
    j = (hashindex0 & (i64)65535);
    d = mm_lex$hashtable[(j)];
    wrapped = (i64)0;
    L273 :;
    while (1) {
        if ((d == 0)) {
            goto L274 ;
        }
;
        if ((((n = (i64)(*d).namelen) == length) && (memcmp((void *)(*d).name,(void *)name,(u64)n) == (i64)0))) {
            mm_decls$nextlx.symptr = (struct mm_decls$strec *)d;
            mm_decls$nextlx.symbol = (i64)(*d).symbol;
            mm_decls$nextlx.subcode = (i64)(*d).subcode;
            return;
        }
;
        if ((++(j) >= (i64)65536)) {
            if (!!(wrapped)) {
                mlib$abortprogram((byte*)"HASHTABLE FULL");
            }
;
            wrapped = (i64)1;
            j = (i64)0;
        }
;
        d = mm_lex$hashtable[(j)];
    }
L274 :;
    ;
    d = (struct mm_decls$strec *)mlib$pcm_allocz((i64)193);
    mm_lex$hashtable[(j)] = d;
    (*d).name = mlib$pcm_copyheapstringn(name,length);
    (*d).namelen = length;
    (*d).symbol = (i64)81;
    mm_decls$nextlx.symptr = (struct mm_decls$strec *)d;
    mm_decls$nextlx.symbol = (i64)(*d).symbol;
    if (((length == (i64)1) && ((u64)(*name) >= (i64)97 && (u64)(*name) <= (i64)122))) {
        mm_lex$shortnames[((i64)(u64)(*name))-97] = d;
    }
;
}

static i64 mm_lex$lookupsys(u8 *name) {
        i64 j;
        i64 wrapped;
    j = (mm_lex$gethashvaluez(name) & (i64)65535);
    mm_decls$lx.symptr = (struct mm_decls$strec *)mm_lex$hashtable[(j)];
    wrapped = (i64)0;
    L275 :;
    while (1) {
        if ((mm_decls$lx.symptr == 0)) {
            goto L276 ;
        }
        else if (!!(mlib$eqstring((*mm_decls$lx.symptr).name,name))) {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"Lex dupl name:",NULL);
            msysc$m_print_str(name,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            exit((i64)1);
        }
;
        if ((++(j) >= (i64)65536)) {
            if (!!(wrapped)) {
                mlib$abortprogram((byte*)"SYS:HASHTABLE FULL");
            }
;
            wrapped = (i64)1;
            j = (i64)0;
        }
;
        mm_decls$lx.symptr = (struct mm_decls$strec *)mm_lex$hashtable[(j)];
    }
L276 :;
    ;
    mm_decls$lx.symptr = (struct mm_decls$strec *)mlib$pcm_allocz((i64)193);
    mm_lex$hashtable[(j)] = (struct mm_decls$strec *)mm_decls$lx.symptr;
    (*mm_decls$lx.symptr).name = name;
    (*mm_decls$lx.symptr).namelen = strlen(name);
    (*mm_decls$lx.symptr).symbol = (i64)81;
    return (i64)0;
}

static i64 mm_lex$gethashvaluez(u8 *s) {
        i64 c;
        i64 hsum;
    if (((i64)(u64)(*s) == (i64)0)) {
        return (i64)0;
    }
;
    hsum = (i64)(u64)(*(s)++);
    L277 :;
    while (1) {
        c = (i64)(u64)(*(s)++);
        if ((c == (i64)0)) {
            goto L278 ;
        }
;
        hsum = (((hsum << (i64)4) - hsum) + c);
    }
L278 :;
    ;
    return ((hsum << (i64)5) - hsum);
}

static void mm_lex$inithashtable(void) {
        i64 i;
    memset(&mm_lex$hashtable,(i32)(i64)0,(u64)524288u);
    for (i=(i64)1;i<=(i64)262;++i) {
L279 :;
        mm_lex$lookupsys(mm_tables$stnames[(i)-1]);
        (*mm_decls$lx.symptr).symbol = mm_tables$stsymbols[(i)-1];
                {i64 $temp = mm_tables$stsymbols[(i)-1];
if (($temp==(i64)80) || ($temp==(i64)153)) {
            (*mm_decls$lx.symptr).index = mm_tables$stsubcodes[(i)-1];
            (*mm_decls$lx.symptr).subcode = mm_tables$stsymbols[(i)-1];
            (*mm_decls$lx.symptr).symbol = (i64)81;
        }
        else {
            (*mm_decls$lx.symptr).subcode = mm_tables$stsubcodes[(i)-1];
        }
        };
L280 :;
    }
L281 :;
    ;
}

void mm_lex$printhashtable(void) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Hashtable:",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

void mm_lex$addreservedword(u8 *name,i64 symbol,i64 subcode,i64 regsize) {
    mm_lex$lookupsys(name);
    (*mm_decls$lx.symptr).symbol = (i64)81;
    (*mm_decls$lx.symptr).subcode = symbol;
    (*mm_decls$lx.symptr).index = subcode;
    (*mm_decls$lx.symptr).regsize = regsize;
}

static i64 mm_lex$dolexdirective(i64 index) {
        u8 *  file;
        i64 fileno;
        i64 length;
    if ((index==(i64)2)) {
        mm_lex$lexreadtoken();
        if (((i64)mm_decls$nextlx.symbol != (i64)77)) {
            mm_support$lxerror((byte*)"strincl: string expected");
        }
        else {
            file = mm_decls$nextlx.svalue;
        }
;
        fileno = mm_support$getsupportfile(file,(byte*)"",(byte*)"",(i64)0,(i64)0);
        mm_decls$nextlx.svalue = mm_decls$sourcefiletext[(fileno)];
        mm_lex$astringlength = (length = mm_decls$sourcefilesizes[(fileno)]);
        mm_decls$nextlx.symbol = (i64)78;
        mm_decls$nextlx.subcode = (i64)65;
        (*(mm_decls$nextlx.svalue + length)) = (u64)0u;
        return (i64)1;
    }
    else if ((index==(i64)1)) {
        mm_lex$lexreadtoken();
        if (((i64)mm_decls$nextlx.symbol != (i64)77)) {
            mm_support$lxerror((byte*)"include: string expected");
        }
;
        file = mm_decls$nextlx.svalue;
        mlib$convlcstring(file);
        file = mlib$addext(file,(byte*)"m");
        fileno = mm_support$getsupportfile(file,(byte*)"",mm_decls$sourcefilepaths[(mm_lex$lxfileno)],(i64)0,(i64)1);
        mm_lex$lexreadtoken();
        mm_lex$stacksource(fileno,(i64)0);
        return (i64)0;
    }
    else {
        msysc$m_print_startcon();
        msysc$m_print_str(mm_tables$sourcedirnames[(index)-1],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        mm_support$lxerror((byte*)"Directive not implemented");
    }
;
    return (i64)0;
}

void mm_lex$startlex(i64 fileno) {
    mm_lex$lxsource = (mm_lex$lxsptr = mm_decls$sourcefiletext[(fileno)]);
    mm_decls$nextlx.pos = (i64)0;
    mm_lex$lxfileno = fileno;
    mm_decls$nextlx.symbol = (i64)6;
    mm_decls$nextlx.subcode = (i64)0;
}

// START
void mm_lex$start(void) {

        i64 i;
    for (i=(i64)0;i<=(i64)255;++i) {
L282 :;
        switch (i) {
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
            {
                mm_lex$alphamap[(i)] = (i64)1;
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
                mm_lex$alphamap[(i)] = (i64)2;
            }
            break;
        } //SW
;
L283 :;
    }
L284 :;
    ;
}

struct mm_decls$strec *mm_lex$addnamestr(u8 *name) {
        struct mm_decls$tokenrec oldlx;
        struct mm_decls$strec *  symptr;
    oldlx = mm_decls$nextlx;
    mm_lex$lookup(name,strlen(name),mm_lex$gethashvaluez(name));
    symptr = (struct mm_decls$strec *)mm_decls$nextlx.symptr;
    mm_decls$nextlx = oldlx;
    return (struct mm_decls$strec *)symptr;
}

void mm_lex$ps(u8 *caption) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"PS:",NULL);
    msysc$m_print_str(caption,NULL);
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)": ",NULL);
    msysc$m_print_end();
    ;
    mm_diags$printsymbol(&mm_decls$lx);
}

void mm_lex$psnext(u8 *caption) {
    msysc$m_print_startcon();
    msysc$m_print_str(caption,NULL);
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)": ",NULL);
    msysc$m_print_end();
    ;
    mm_diags$printsymbol(&mm_decls$nextlx);
}

void mm_lex$psx(u8 *caption) {
    msysc$m_print_startcon();
    msysc$m_print_str(caption,NULL);
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)": ",NULL);
    msysc$m_print_end();
    ;
    mm_diags$printsymbol(&mm_decls$lx);
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"\t",NULL);
    msysc$m_print_end();
    ;
    mm_diags$printsymbol(&mm_decls$nextlx);
}

void mm_lex$stacksource(i64 fileno,i64 isimport) {
    if ((mm_lex$sourcelevel >= (i64)20)) {
        mm_support$lxerror((byte*)"Include file/macro overflow");
    }
;
    ++(mm_lex$sourcelevel);
    mm_lex$lxstart_stack[(mm_lex$sourcelevel)-1] = mm_lex$lxstart;
    mm_lex$lxsource_stack[(mm_lex$sourcelevel)-1] = mm_lex$lxsource;
    mm_lex$lxsptr_stack[(mm_lex$sourcelevel)-1] = mm_lex$lxsptr;
    mm_lex$lxfileno_stack[(mm_lex$sourcelevel)-1] = mm_lex$lxfileno;
    mm_lex$lxnextlx_stack[(mm_lex$sourcelevel)-1] = mm_decls$nextlx;
    mm_lex$lximport_stack[(mm_lex$sourcelevel)-1] = mm_lex$lximport;
    mm_lex$lximport = isimport;
    mm_lex$lxsource = (mm_lex$lxsptr = mm_decls$sourcefiletext[(fileno)]);
    mm_decls$nextlx.pos = (i64)0;
    mm_lex$lxfileno = fileno;
    mm_decls$nextlx.symbol = (i64)6;
    mm_decls$nextlx.subcode = (i64)0;
}

void mm_lex$unstacksource(void) {
    if ((mm_lex$sourcelevel > (i64)0)) {
        mm_lex$lxstart = mm_lex$lxstart_stack[(mm_lex$sourcelevel)-1];
        mm_lex$lxsource = mm_lex$lxsource_stack[(mm_lex$sourcelevel)-1];
        mm_lex$lxsptr = mm_lex$lxsptr_stack[(mm_lex$sourcelevel)-1];
        mm_decls$nextlx = mm_lex$lxnextlx_stack[(mm_lex$sourcelevel)-1];
        mm_lex$lxfileno = mm_lex$lxfileno_stack[(mm_lex$sourcelevel)-1];
        mm_lex$lximport = (i64)mm_lex$lximport_stack[(mm_lex$sourcelevel)-1];
        --(mm_lex$sourcelevel);
    }
;
}

static void mm_lex$readarraystring(i64 prefix) {
    ++(mm_lex$lxsptr);
    mm_lex$lxreadstring((i64)34);
    mm_decls$nextlx.symbol = (i64)78;
    mm_decls$nextlx.subcode = toupper((i32)prefix);
    mm_lex$astringlength = strlen(mm_decls$nextlx.svalue);
}

static i64 mm_lex$setinttype(u64 a) {
    if ((a <= (u64)9223372036854775807u)) {
        return (i64)3;
    }
    else {
        return (i64)2;
    }
;
}

static void mm_lex$readrawxname(void) {
        i64 c;
        i64 hsum;
        i64 length;
    mm_decls$nextlx.svalue = mm_lex$lxsptr;
    hsum = (i64)0;
    L285 :;
    switch ((c = (i64)(u64)(*(mm_lex$lxsptr)++))) {
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
        {
            hsum = (((hsum << (i64)4) - hsum) + c);
        }
        break;
    default: {
        --(mm_lex$lxsptr);
        goto L286 ;
    }
    } //SW
goto L285 ;
L286 :;
    ;
    length = (mm_lex$lxsptr - mm_decls$nextlx.svalue);
    if ((length == (i64)0)) {
        mm_support$lxerror((byte*)"Bad ` name");
    }
;
    mm_lex$lookup(mm_decls$nextlx.svalue,length,((hsum << (i64)5) - hsum));
    mm_decls$nextlx.symbol = (i64)69;
    return;
}

static void mm_lex$lxerror_s(u8 *mess,u8 *s) {
    mm_support$lxerror(mess);
}

static void mm_lex$lxreadstring(i64 termchar) {
        u8 *  s;
        u8 *  t;
        i64 c;
        i64 d;
        i64 length;
        i64 hasescape;
        u8 str[8];
        i64 $av_1;
    if ((termchar == (i64)34)) {
        mm_decls$nextlx.symbol = (i64)77;
    }
    else {
        mm_decls$nextlx.symbol = (i64)75;
        mm_decls$nextlx.subcode = (i64)3;
    }
;
    s = mm_lex$lxsptr;
    length = (i64)0;
    hasescape = (i64)0;
    L287 :;
    switch ((c = (i64)(u64)(*(mm_lex$lxsptr)++))) {
    case 92:;
        {
            c = (i64)(u64)(*mm_lex$lxsptr);
            if ((c >= (i64)65 && c <= (i64)90)) {
                c += (i64)32;
            }
;
            ++(mm_lex$lxsptr);
            hasescape = (i64)1;
            switch (c) {
            case 97:;
            case 98:;
            case 99:;
            case 101:;
            case 114:;
            case 102:;
            case 108:;
            case 110:;
            case 115:;
            case 116:;
            case 118:;
            case 121:;
            case 122:;
            case 48:;
            case 34:;
            case 113:;
            case 92:;
            case 39:;
                {
                    ++(length);
                }
                break;
            case 119:;
                {
                    ++(length);
                }
                break;
            case 120:;
                {
                    mm_lex$lxsptr += (i64)2;
                    ++(length);
                }
                break;
            default: {
                mm_support$lxerror((byte*)"Bad str escape");
            }
            } //SW
;
        }
        break;
    case 34:;
    case 39:;
        {
            if ((c == termchar)) {
                if (((i64)(u64)(*mm_lex$lxsptr) == c)) {
                    hasescape = (i64)1;
                    ++(mm_lex$lxsptr);
                    ++(length);
                }
                else {
                    goto L288 ;
                }
;
            }
            else {
                ++(length);
            }
;
        }
        break;
    case 13:;
    case 10:;
    case 0:;
        {
            mm_support$lxerror((byte*)"String not terminated");
        }
        break;
    default: {
        ++(length);
    }
    } //SW
goto L287 ;
L288 :;
    ;
    if ((length == (i64)0)) {
        mm_decls$nextlx.svalue = (byte*)"";
        return;
    }
    else if (!(!!(hasescape))) {
        mm_decls$nextlx.svalue = mlib$pcm_copyheapstringn(s,length);
        return;
    }
;
    mm_decls$nextlx.svalue = (t = (u8 *)mlib$pcm_alloc((length + (i64)1)));
    L289 :;
    while (1) {
        switch ((c = (i64)(u64)(*(s)++))) {
        case 92:;
            {
                c = (i64)(u64)(*s);
                if (((c >= (i64)65) && (c <= (i64)90))) {
                    c += (i64)32;
                }
;
                ++(s);
                switch (c) {
                case 97:;
                    {
                        c = (i64)7;
                    }
                    break;
                case 98:;
                    {
                        c = (i64)8;
                    }
                    break;
                case 99:;
                case 114:;
                    {
                        c = (i64)13;
                    }
                    break;
                case 101:;
                    {
                        c = (i64)26;
                    }
                    break;
                case 102:;
                    {
                        c = (i64)12;
                    }
                    break;
                case 108:;
                case 110:;
                    {
                        c = (i64)10;
                    }
                    break;
                case 115:;
                    {
                        c = (i64)27;
                    }
                    break;
                case 116:;
                    {
                        c = (i64)9;
                    }
                    break;
                case 118:;
                    {
                        c = (i64)11;
                    }
                    break;
                case 119:;
                    {
                        (*(t)++) = (u64)13u;
                        c = (i64)10;
                    }
                    break;
                case 120:;
                    {
                        c = (i64)0;
                        $av_1 = (i64)2;
                        while ($av_1-- > 0) {
L291 :;
                                                        {i64 $temp = (d = (i64)(u64)(*(s)++));
if (($temp==(i64)65) || ($temp==(i64)66) || ($temp==(i64)67) || ($temp==(i64)68) || ($temp==(i64)69) || ($temp==(i64)70)) {
                                c = ((((c * (i64)16) + d) - (i64)65) + (i64)10);
                            }
                            else if (($temp==(i64)97) || ($temp==(i64)98) || ($temp==(i64)99) || ($temp==(i64)100) || ($temp==(i64)101) || ($temp==(i64)102)) {
                                c = ((((c * (i64)16) + d) - (i64)97) + (i64)10);
                            }
                            else if (($temp==(i64)48) || ($temp==(i64)49) || ($temp==(i64)50) || ($temp==(i64)51) || ($temp==(i64)52) || ($temp==(i64)53) || ($temp==(i64)54) || ($temp==(i64)55) || ($temp==(i64)56) || ($temp==(i64)57)) {
                                c = (((c * (i64)16) + d) - (i64)48);
                            }
                            else {
                                mm_support$lxerror((byte*)"Bad \\x code");
                            }
                            };
L292 :;
                        }
L293 :;
                        ;
                    }
                    break;
                case 121:;
                    {
                        c = (i64)16;
                    }
                    break;
                case 122:;
                case 48:;
                    {
                        c = (i64)0;
                    }
                    break;
                case 34:;
                case 81:;
                    {
                        c = (i64)34;
                    }
                    break;
                case 92:;
                    {
                        c = (i64)92;
                    }
                    break;
                case 39:;
                    {
                        c = (i64)39;
                    }
                    break;
                default: {
                    str[((i64)1)-1] = (u64)c;
                    str[((i64)2)-1] = (u64)0u;
                    mm_lex$lxerror_s((byte*)"Unknown string escape: \\%s",(u8 *)str);
                }
                } //SW
;
            }
            break;
        case 34:;
        case 39:;
            {
                if ((c == termchar)) {
                    if (((i64)(u64)(*s) == c)) {
                        ++(s);
                    }
                    else {
                        goto L290 ;
                    }
;
                }
;
            }
            break;
        case 13:;
        case 10:;
        case 0:;
            {
                mm_support$lxerror((byte*)"String not terminated");
            }
            break;
        } //SW
;
        (*(t)++) = (u64)c;
    }
L290 :;
    ;
    (*t) = (u64)0u;
}

static void mm_lex$readdec(void) {
        i64 c;
        u8 *  dest;
        u8 *  destend;
        u8 *  pstart;
        i64 islong;
        i64 length;
        byte str[1024];
        u64 a;
        i64 $av_1;
    islong = (i64)0;
    pstart = mm_lex$lxsptr;
    dest = (u8 *)str;
    destend = ((dest + (i64)1024) - (i64)10);
    a = (u64)0u;
    L294 :;
    while (1) {
        switch ((c = (i64)(u64)(*(mm_lex$lxsptr)++))) {
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
                a = (u64)((((i64)a * (i64)10) + c) - (i64)48);
                (*(dest)++) = (u64)c;
            }
            break;
        case 101:;
        case 69:;
            {
                mm_lex$lxsptr = pstart;
                mm_lex$readreal();
                return;
            }
            break;
        case 46:;
            {
                if (((u64)(*mm_lex$lxsptr) != '.')) {
                    mm_lex$lxsptr = pstart;
                    mm_lex$readreal();
                    return;
                }
;
                --(mm_lex$lxsptr);
                goto L295 ;
            }
            break;
        case 95:;
        case 39:;
            {
            }
            break;
        case 108:;
        case 76:;
            {
                (*dest) = (u64)0u;
                mm_support$lxerror((byte*)"MAKEDECIMAL NOT READY");
                return;
            }
            break;
        case 98:;
        case 66:;
            {
                length = (dest - str);
                if ((length > (i64)64)) {
                    mm_support$lxerror((byte*)"bin overflow");
                }
;
                dest = (u8 *)str;
                a = (u64)0u;
                $av_1 = length;
                while ($av_1-- > 0) {
L296 :;
                    if (((u64)(*dest) >= '2')) {
                        mm_support$lxerror((byte*)"bad bin digit");
                    }
;
                    a = (u64)((((i64)a * (i64)2) + (i64)(u64)(*(dest)++)) - (i64)48);
L297 :;
                }
L298 :;
                ;
                goto L299 ;
;
            }
            break;
        default: {
            --(mm_lex$lxsptr);
            goto L295 ;
        }
        } //SW
;
        if ((dest >= destend)) {
            mm_support$lxerror((byte*)"Numlit too long");
        }
;
    }
L295 :;
    ;
    length = (dest - str);
    if (((length > (i64)20) || ((length == (i64)20) && !!(strncmp((u8 *)str,mm_lex$u64maxstr,(u64)20u))))) {
        mm_support$lxerror((byte*)"2:MAKEDECIMAL NOT READY");
        return;
    }
;
    //finish:
L299 :;
;
    mm_decls$nextlx.symbol = (i64)72;
    mm_decls$nextlx.subcode = mm_lex$setinttype(a);
    mm_decls$nextlx.value = (i64)a;
}

static void mm_lex$readhex(void) {
        i64 c;
        u8 *  dest;
        u8 *  destend;
        u8 *  pstart;
        i64 length;
        byte str[1024];
        u64 a;
    pstart = mm_lex$lxsptr;
    dest = (u8 *)str;
    destend = ((dest + (i64)1024) - (i64)10);
    a = (u64)0u;
    L300 :;
    while (1) {
        switch ((c = (i64)(u64)(*(mm_lex$lxsptr)++))) {
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
                a = (u64)((((i64)a * (i64)16) + c) - (i64)48);
                (*(dest)++) = (u64)c;
            }
            break;
        case 65:;
        case 66:;
        case 67:;
        case 68:;
        case 69:;
        case 70:;
            {
                (*(dest)++) = (u64)c;
                a = (u64)(((((i64)a * (i64)16) + c) - (i64)65) + (i64)10);
            }
            break;
        case 97:;
        case 98:;
        case 99:;
        case 100:;
        case 101:;
        case 102:;
            {
                (*(dest)++) = (u64)(c - (i64)32);
                a = (u64)(((((i64)a * (i64)16) + c) - (i64)97) + (i64)10);
            }
            break;
        case 95:;
        case 39:;
            {
            }
            break;
        case 108:;
        case 76:;
            {
                (*dest) = (u64)0u;
                mm_support$lxerror((byte*)"3:MAKEDECIMAL NOT READY");
                return;
            }
            break;
        case 46:;
            {
                --(mm_lex$lxsptr);
                goto L301 ;
            }
            break;
        default: {
            --(mm_lex$lxsptr);
            goto L301 ;
        }
        } //SW
;
        if ((dest >= destend)) {
            mm_support$lxerror((byte*)"Numlit too long");
        }
;
    }
L301 :;
    ;
    length = (dest - str);
    if ((length > (i64)16)) {
        mm_support$lxerror((byte*)"4:MAKEDECIMAL NOT READY");
        return;
    }
;
    mm_decls$nextlx.symbol = (i64)72;
    mm_decls$nextlx.subcode = mm_lex$setinttype(a);
    mm_decls$nextlx.value = (i64)a;
}

static void mm_lex$readoct(void) {
        i64 c;
        u8 *  dest;
        u8 *  destend;
        u8 *  pstart;
        i64 length;
        byte str[1024];
        u64 a;
    pstart = mm_lex$lxsptr;
    dest = (u8 *)str;
    destend = ((dest + (i64)1024) - (i64)10);
    a = (u64)0u;
    L302 :;
    while (1) {
        switch ((c = (i64)(u64)(*(mm_lex$lxsptr)++))) {
        case 48:;
        case 49:;
        case 50:;
        case 51:;
        case 52:;
        case 53:;
        case 54:;
        case 55:;
            {
                a = (u64)((((i64)a * (i64)8) + c) - (i64)48);
                (*(dest)++) = (u64)c;
            }
            break;
        case 95:;
        case 39:;
            {
            }
            break;
        case 46:;
            {
                --(mm_lex$lxsptr);
                goto L303 ;
            }
            break;
        default: {
            --(mm_lex$lxsptr);
            goto L303 ;
        }
        } //SW
;
        if ((dest >= destend)) {
            mm_support$lxerror((byte*)"Numlit too long");
        }
;
    }
L303 :;
    ;
    length = (dest - str);
    if ((length > (i64)22)) {
        mm_support$lxerror((byte*)"oct overflow");
    }
;
    mm_decls$nextlx.symbol = (i64)72;
    mm_decls$nextlx.subcode = mm_lex$setinttype(a);
    mm_decls$nextlx.value = (i64)a;
}

static void mm_lex$readbin(void) {
        i64 c;
        u8 *  dest;
        u8 *  destend;
        u8 *  pstart;
        i64 length;
        byte str[1024];
        u64 a;
    pstart = mm_lex$lxsptr;
    dest = (u8 *)str;
    destend = ((dest + (i64)1024) - (i64)10);
    a = (u64)0u;
    L304 :;
    while (1) {
        switch ((c = (i64)(u64)(*(mm_lex$lxsptr)++))) {
        case 48:;
        case 49:;
            {
                a = (u64)((((i64)a * (i64)2) + c) - (i64)48);
                (*(dest)++) = (u64)c;
            }
            break;
        case 95:;
        case 39:;
            {
            }
            break;
        case 108:;
        case 76:;
            {
                (*dest) = (u64)0u;
                mm_support$lxerror((byte*)"5:MAKEDECIMAL NOT READY");
                return;
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
                mm_support$lxerror((byte*)"bin bad digit");
            }
            break;
        case 46:;
            {
                --(mm_lex$lxsptr);
                goto L305 ;
            }
            break;
        default: {
            --(mm_lex$lxsptr);
            goto L305 ;
        }
        } //SW
;
        if ((dest >= destend)) {
            mm_support$lxerror((byte*)"bin overflow");
        }
;
    }
L305 :;
    ;
    length = (dest - str);
    if ((length > (i64)64)) {
        mm_support$lxerror((byte*)"6:MAKEDECIMAL NOT READY");
        return;
    }
;
    mm_decls$nextlx.symbol = (i64)72;
    mm_decls$nextlx.subcode = mm_lex$setinttype(a);
    mm_decls$nextlx.value = (i64)a;
}

static void mm_lex$readreal(void) {
        i64 c;
        i64 negexpon;
        i64 dotseen;
        i64 length;
        i64 fractlen;
        i64 expon;
        i64 expseen;
        r64 x;
        u8 str[1024];
        u8 *  dest;
        u8 *  destend;
        i64 $av_1;
        i64 $av_2;
        i64 $av_3;
        i64 i;
    dest = (u8 *)str;
    destend = ((dest + (i64)1024) - (i64)100);
    length = (negexpon = (dotseen = (expseen = (expon = (fractlen = (i64)0)))));
    L306 :;
    while (1) {
        switch ((c = (i64)(u64)(*(mm_lex$lxsptr)++))) {
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
                (*(dest)++) = (u64)c;
                ++(length);
                if (!!(dotseen)) {
                    ++(fractlen);
                }
;
            }
            break;
        case 46:;
            {
                if (!!(dotseen)) {
                    --(mm_lex$lxsptr);
                    goto L307 ;
                }
;
                dotseen = (i64)1;
                (*(dest)++) = (u64)c;
            }
            break;
        case 101:;
        case 69:;
            {
                if (!!(expseen)) {
                    mm_support$lxerror((byte*)"double expon");
                }
;
                expseen = (i64)1;
                (*(dest)++) = (u64)c;
                L308 :;
                while (((u64)(*mm_lex$lxsptr) == ' ')) {
                    ++(mm_lex$lxsptr);
L309 :;
                }
L310 :;
                ;
                if (((u64)(*mm_lex$lxsptr) == '+' || (u64)(*mm_lex$lxsptr) == '-')) {
                    if (((u64)(*mm_lex$lxsptr) == '-')) {
                        negexpon = (i64)1;
                    }
;
                    (*(dest)++) = (u64)(*(mm_lex$lxsptr)++);
                }
;
                expon = (i64)0;
                L311 :;
                switch ((c = (i64)(u64)(*(mm_lex$lxsptr)++))) {
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
                        (*(dest)++) = (u64)c;
                        if ((dest >= destend)) {
                            mm_support$lxerror((byte*)"expon?");
                        }
;
                    }
                    break;
                case 95:;
                case 39:;
                    {
                    }
                    break;
                case 108:;
                case 76:;
                    {
                        (*dest) = (u64)0u;
                        mm_support$lxerror((byte*)"7:MAKEDECIMAL NOT READY");
                        return;
                    }
                    break;
                default: {
                    --(mm_lex$lxsptr);
                    goto L307 ;
                }
                } //SW
goto L311 ;
L312 :;
                ;
            }
            break;
        case 95:;
        case 39:;
            {
            }
            break;
        case 108:;
        case 76:;
            {
                mm_support$lxerror((byte*)"8:MAKEDECIMAL NOT READY");
                return;
            }
            break;
        default: {
            --(mm_lex$lxsptr);
            goto L307 ;
        }
        } //SW
;
        if ((dest >= destend)) {
            mm_support$lxerror((byte*)"r64lit too long");
        }
;
    }
L307 :;
    ;
    (*dest) = (u64)0u;
    if (!!(negexpon)) {
        expon = -(expon);
    }
;
    expon -= fractlen;
    x = (double)0.;
        ($av_1 = (length + dotseen));
    for (i=(i64)1;i<=$av_1;++i) {
L313 :;
        c = (i64)(u64)str[(i)-1];
        if ((c != (i64)46)) {
            x = (((x * (double)10.) + (r64)c) - (r64)'0');
        }
;
L314 :;
    }
L315 :;
    ;
    if ((expon >= (i64)0)) {
        $av_2 = expon;
        while ($av_2-- > 0) {
L316 :;
            x *= (double)10.;
L317 :;
        }
L318 :;
        ;
    }
    else {
        $av_3 = -(expon);
        while ($av_3-- > 0) {
L319 :;
            x /= (double)10.;
L320 :;
        }
L321 :;
        ;
    }
;
    mm_decls$nextlx.xvalue = x;
    mm_decls$nextlx.symbol = (i64)74;
    mm_decls$nextlx.subcode = (i64)5;
}

struct mm_decls$strec *mm_lib$newstrec(void) {
        struct mm_decls$strec *  p;
    p = (struct mm_decls$strec *)mlib$pcm_alloc((i64)193);
    memset(&((*p)),0,193);
    ++(mm_decls$nstrecs);
    (*p).pos = (i64)mm_decls$lx.pos;
    (*p).moduleno = mm_decls$currmoduleno;
    (*p).subprogno = (i64)mm_decls$moduletosub[(mm_decls$currmoduleno)];
    return p;
}

struct mm_decls$strec *mm_lib$getduplnameptr(struct mm_decls$strec *owner,struct mm_decls$strec *symptr,i64 id) {
        struct mm_decls$strec *  p;
    p = mm_lib$newstrec();
    (*p).name = (*symptr).name;
    (*p).namelen = (i64)(*symptr).namelen;
    (*p).symbol = (i64)81;
    (*p).owner = (struct mm_decls$strec *)owner;
    (*p).nameid = id;
    (*p).nextdupl = (*symptr).nextdupl;
    (*p).firstdupl = (struct mm_decls$strec *)symptr;
    (*symptr).nextdupl = (struct mm_decls$strec *)p;
    return p;
}

void mm_lib$adddef(struct mm_decls$strec *owner,struct mm_decls$strec *p) {
        struct mm_decls$strec *  q;
    if (!!((q = (struct mm_decls$strec *)(*p).nextdupl))) {
        if (((*q).owner == owner)) {
            msysc$m_print_startcon();
            msysc$m_print_str((*q).name,NULL);
            msysc$m_print_str((byte*)"in",NULL);
            msysc$m_print_str((*owner).name,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            mm_support$serror((byte*)"Duplicate name");
        }
;
    }
;
    if (((*owner).deflist == 0)) {
        (*owner).deflist = (struct mm_decls$strec *)p;
    }
    else {
        (*(*owner).deflistx).nextdef = (struct mm_decls$strec *)p;
    }
;
    (*owner).deflistx = (struct mm_decls$strec *)p;
}

struct mm_decls$unitrec *mm_lib$createname(struct mm_decls$strec *p) {
        struct mm_decls$unitrec *  u;
    u = (struct mm_decls$unitrec *)mm_lib$allocunitrec();
    (*u).tag = (i64)3;
    (*u).def = p;
    (*u).simple = (i64)1;
    return (struct mm_decls$unitrec *)u;
}

struct mm_decls$unitrec *mm_lib$createunit0(i64 tag) {
        struct mm_decls$unitrec *  u;
    u = (struct mm_decls$unitrec *)mm_lib$allocunitrec();
    (*u).tag = tag;
    return (struct mm_decls$unitrec *)u;
}

struct mm_decls$unitrec *mm_lib$createunit1(i64 tag,struct mm_decls$unitrec *p) {
        struct mm_decls$unitrec *  u;
    u = (struct mm_decls$unitrec *)mm_lib$allocunitrec();
    (*u).tag = tag;
    (*u).a = (struct mm_decls$unitrec *)p;
    return (struct mm_decls$unitrec *)u;
}

struct mm_decls$unitrec *mm_lib$createunit2(i64 tag,struct mm_decls$unitrec *p,struct mm_decls$unitrec *q) {
        struct mm_decls$unitrec *  u;
    u = (struct mm_decls$unitrec *)mm_lib$allocunitrec();
    (*u).tag = tag;
    (*u).a = (struct mm_decls$unitrec *)p;
    (*u).b = (struct mm_decls$unitrec *)q;
    return (struct mm_decls$unitrec *)u;
}

struct mm_decls$unitrec *mm_lib$createunit3(i64 tag,struct mm_decls$unitrec *p,struct mm_decls$unitrec *q,struct mm_decls$unitrec *r) {
        struct mm_decls$unitrec *  u;
    u = (struct mm_decls$unitrec *)mm_lib$allocunitrec();
    (*u).tag = tag;
    (*u).a = (struct mm_decls$unitrec *)p;
    (*u).b = (struct mm_decls$unitrec *)q;
    (*u).c = (struct mm_decls$unitrec *)r;
    return (struct mm_decls$unitrec *)u;
}

void mm_lib$insertunit(struct mm_decls$unitrec *p,i64 tag) {
        struct mm_decls$unitrec *  q;
        struct mm_decls$unitrec *  nextunit;
        i64 mode;
    q = (struct mm_decls$unitrec *)mm_lib$allocunitrec();
    (*q) = (*p);
    mode = (i64)(*q).mode;
    nextunit = (*q).nextunit;
    (*q).nextunit = 0;
    memset(&((*p)),0,64);
    (*p).tag = tag;
    (*p).pos = (i64)(*q).pos;
    (*p).a = q;
    (*p).mode = mode;
    (*p).nextunit = nextunit;
    (*p).resultflag = (i64)(*q).resultflag;
}

void mm_lib$deleteunit(struct mm_decls$unitrec *p,struct mm_decls$unitrec *q) {
        struct mm_decls$unitrec *  r;
    r = (*p).nextunit;
    (*p) = (*q);
    (*p).nextunit = r;
}

struct mm_decls$unitrec *mm_lib$createconstunit(u64 a,i64 t) {
        struct mm_decls$unitrec *  u;
    u = (struct mm_decls$unitrec *)mm_lib$allocunitrec();
    (*u).tag = (i64)1;
    (*u).value = (i64)a;
    (*u).mode = t;
    (*u).isconst = (i64)1;
    (*u).simple = (i64)1;
    return (struct mm_decls$unitrec *)u;
}

struct mm_decls$unitrec *mm_lib$createstringconstunit(u8 *s,i64 length) {
        struct mm_decls$unitrec *  u;
    u = (struct mm_decls$unitrec *)mm_lib$allocunitrec();
    (*u).tag = (i64)1;
    (*u).svalue = s;
    (*u).mode = (i64)20;
    (*u).isastring = (i64)1;
    if ((length == (i64)-1)) {
        (*u).slength = strlen(s);
    }
    else {
        (*u).slength = length;
    }
;
    return (struct mm_decls$unitrec *)u;
}

i64 mm_lib$newtypename(struct mm_decls$strec *a,struct mm_decls$strec *b) {
    if ((mm_decls$ntypenames >= (i64)8000)) {
        mm_support$serror((byte*)"Too many type names");
    }
;
    ++(mm_decls$ntypenames);
    mm_decls$typenames[(mm_decls$ntypenames)].defa = a;
    mm_decls$typenames[(mm_decls$ntypenames)].defb = b;
    mm_decls$typenamepos[(mm_decls$ntypenames)].pos = (i64)mm_decls$lx.pos;
    return -(mm_decls$ntypenames);
}

i64 mm_lib$createusertype(struct mm_decls$strec *stname) {
    if ((mm_decls$ntypes >= (i64)6000)) {
        msysc$m_print_startcon();
        msysc$m_print_i64(mm_decls$ntypes,NULL);
        msysc$m_print_str((*stname).name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        mm_support$serror((byte*)"Too many types");
    }
;
    ++(mm_decls$ntypes);
    mm_decls$ttname[(mm_decls$ntypes)] = (*stname).name;
    mm_decls$ttnamedef[(mm_decls$ntypes)] = stname;
    mm_decls$ttbasetype[(mm_decls$ntypes)] = (i64)0;
    mm_decls$ttlineno[(mm_decls$ntypes)] = (i64)mm_decls$lx.pos;
    (*stname).mode = mm_decls$ntypes;
    return mm_decls$ntypes;
}

i64 mm_lib$createusertypefromstr(u8 *name) {
        struct mm_decls$strec *  stname;
    stname = mm_lib$getduplnameptr(mm_decls$stmodule,(struct mm_decls$strec *)mm_lex$addnamestr(name),(i64)5);
    return mm_lib$createusertype(stname);
}

struct mm_decls$unitrec *mm_lib$getrangelwbunit(struct mm_decls$unitrec *p) {
    if (((i64)(*p).tag == (i64)16)) {
        return (struct mm_decls$unitrec *)(*p).a;
    }
    else {
        p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)32,(struct mm_decls$unitrec *)p);
        (*p).pclop = (i64)89;
        return (struct mm_decls$unitrec *)p;
    }
;
}

struct mm_decls$unitrec *mm_lib$getrangeupbunit(struct mm_decls$unitrec *p) {
    if (((i64)(*p).tag == (i64)16)) {
        return (struct mm_decls$unitrec *)(*p).b;
    }
    else {
        p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)32,(struct mm_decls$unitrec *)p);
        (*p).pclop = (i64)90;
        return (struct mm_decls$unitrec *)p;
    }
;
}

i64 mm_lib$createarraymode(struct mm_decls$strec *owner,i64 target,struct mm_decls$unitrec *dimexpr,i64 typedefx) {
        i64 k;
        i64 m;
    if ((typedefx == (i64)0)) {
        for (k=(i64)29;k<=mm_decls$ntypes;++k) {
L322 :;
            if ((((((i64)mm_decls$ttusercat[(k)] == (i64)0) && ((i64)mm_decls$ttbasetype[(k)] == (i64)10)) && ((i64)mm_decls$tttarget[(k)] == target)) && !!(mm_lib$sameunit(dimexpr,mm_decls$ttdimexpr[(k)],owner,mm_decls$ttowner[(k)])))) {
                return k;
            }
;
L323 :;
        }
L324 :;
        ;
        m = mm_lib$createusertypefromstr(mm_lib$nextautotype());
    }
    else {
        m = typedefx;
    }
;
    mm_decls$ttbasetype[(m)] = (i64)10;
    mm_decls$ttlower[(m)] = (i64)1;
    mm_decls$ttdimexpr[(m)] = dimexpr;
    mm_lib$storemode(owner,target,&mm_decls$tttarget[(m)]);
    mm_decls$ttowner[(m)] = owner;
    mm_decls$ttcat[(m)] = (i64)5;
    mm_decls$ttisblock[(m)] = (i64)1;
    return m;
}

static i64 mm_lib$sameunit(struct mm_decls$unitrec *p,struct mm_decls$unitrec *q,struct mm_decls$strec *powner,struct mm_decls$strec *qowner) {
    if ((p == q)) {
        return (i64)1;
    }
;
    if (((p == 0) || (q == 0))) {
        return (i64)0;
    }
;
    if (((i64)(*p).tag != (i64)(*q).tag)) {
        return (i64)0;
    }
;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)1)) {
        return (i64)((*p).value == (*q).value);
    }
    else if (($temp==(i64)16) || ($temp==(i64)22)) {
        return (i64)(!!(mm_lib$sameunit((*p).a,(*q).a,0,0)) && !!(mm_lib$sameunit((*p).b,(*q).b,0,0)));
    }
    else if (($temp==(i64)3)) {
        if ((((*p).def == (*q).def) && (powner == qowner))) {
            return (i64)1;
        }
;
    }
    };
    return (i64)0;
}

i64 mm_lib$createarraymodek(struct mm_decls$strec *owner,i64 target,i64 lower,i64 length,i64 typedefx) {
        i64 atype;
        i64 m;
    atype = (i64)10;
    if ((typedefx == (i64)0)) {
        m = mm_lib$createusertypefromstr(mm_lib$nextautotype());
    }
    else {
        m = typedefx;
    }
;
    mm_decls$ttbasetype[(m)] = atype;
    mm_decls$ttlower[(m)] = lower;
    mm_decls$ttlength[(m)] = length;
    if ((target < (i64)0)) {
        mm_support$serror((byte*)"CREATEARRAYMODEK/TARGET NOT RESOLVED");
    }
;
    mm_decls$ttsize[(m)] = (length * (i64)mm_decls$ttsize[(target)]);
    mm_lib$storemode(owner,target,&mm_decls$tttarget[(m)]);
    mm_decls$ttowner[(m)] = owner;
    mm_decls$ttcat[(m)] = (i64)5;
    mm_decls$ttisblock[(m)] = (i64)1;
    return m;
}

u8 *mm_lib$nextautotype(void) {
        static u8 str[32];
    msysc$m_print_startstr(str);
    msysc$m_print_str((byte*)"$T",NULL);
    msysc$m_print_nogap();
    msysc$m_print_i64(++(mm_lib$autotypeno),NULL);
    msysc$m_print_end();
    ;
    return (u8 *)str;
}

i64 mm_lib$createslicemode(struct mm_decls$strec *owner,i64 slicetype,i64 target,struct mm_decls$unitrec *dimexpr,i64 typedefx) {
        i64 m;
    if ((typedefx == (i64)0)) {
        m = mm_lib$createusertypefromstr(mm_lib$nextautotype());
    }
    else {
        m = typedefx;
    }
;
    mm_decls$ttbasetype[(m)] = slicetype;
    if (!!(dimexpr)) {
        mm_decls$ttdimexpr[(m)] = dimexpr;
    }
    else {
        mm_decls$ttlower[(m)] = (i64)1;
    }
;
    mm_lib$storemode(owner,target,&mm_decls$tttarget[(m)]);
    mm_decls$ttowner[(m)] = owner;
    mm_decls$ttcat[(m)] = (i64)5;
    mm_decls$ttisblock[(m)] = (i64)1;
    return m;
}

i64 mm_lib$createslicemodek(struct mm_decls$strec *owner,i64 target,i64 lower,i64 typedefx) {
        i64 m;
    if ((typedefx == (i64)0)) {
        m = mm_lib$createusertypefromstr(mm_lib$nextautotype());
    }
    else {
        m = typedefx;
    }
;
    mm_decls$ttbasetype[(m)] = (i64)11;
    mm_decls$ttlower[(m)] = lower;
    mm_lib$storemode(owner,target,&mm_decls$tttarget[(m)]);
    mm_decls$ttowner[(m)] = owner;
    mm_decls$ttcat[(m)] = (i64)5;
    mm_decls$ttisblock[(m)] = (i64)1;
    return m;
}

i64 mm_lib$createrefmode(struct mm_decls$strec *owner,i64 target,i64 typedefx) {
        i64 k;
        i64 m;
    if ((typedefx == (i64)0)) {
        for (k=(i64)29;k<=mm_decls$ntypes;++k) {
L325 :;
            if (!!((i64)mm_decls$ttisref[(k)])) {
                if (((i64)mm_decls$tttarget[(k)] == target)) {
                    return k;
                }
;
            }
;
L326 :;
        }
L327 :;
        ;
        m = mm_lib$createusertypefromstr(mm_lib$nextautotype());
    }
    else {
        m = typedefx;
    }
;
    mm_lib$storemode(owner,target,&mm_decls$tttarget[(m)]);
    mm_decls$ttbasetype[(m)] = (i64)7;
    mm_decls$ttsize[(m)] = (i64)mm_decls$ttsize[((i64)7)];
    mm_decls$ttisref[(m)] = (i64)1;
    mm_decls$ttcat[(m)] = (i64)1;
    return m;
}

i64 mm_lib$createrefprocmode(struct mm_decls$strec *owner,struct mm_decls$strec *stproc,struct mm_decls$strec *paramlist,i64 kwd,i64 prettype,i64 typedefx) {
        i64 m;
        i64 mproc;
    mproc = mm_lib$createusertype(stproc);
    (*stproc).paramlist = (struct mm_decls$strec *)paramlist;
    (*stproc).mode = prettype;
    mm_decls$ttbasetype[(mproc)] = (i64)23;
    if ((typedefx == (i64)0)) {
        m = mm_lib$createusertypefromstr(mm_lib$nextautotype());
    }
    else {
        m = typedefx;
    }
;
    mm_decls$tttarget[(m)] = mproc;
    mm_decls$ttbasetype[(m)] = (i64)7;
    mm_decls$ttsize[(m)] = (i64)mm_decls$ttsize[((i64)7)];
    mm_decls$ttisref[(m)] = (i64)1;
    mm_decls$ttcat[(m)] = (i64)1;
    return m;
}

void mm_lib$copyttvalues(i64 dest,i64 source) {
    mm_decls$ttsigned[(dest)] = (i64)mm_decls$ttsigned[(source)];
    mm_decls$ttisreal[(dest)] = (i64)mm_decls$ttisreal[(source)];
    mm_decls$ttisinteger[(dest)] = (i64)mm_decls$ttisinteger[(source)];
    mm_decls$ttisshort[(dest)] = (i64)mm_decls$ttisshort[(source)];
    mm_decls$ttisref[(dest)] = (i64)mm_decls$ttisref[(source)];
    mm_decls$ttcat[(dest)] = (i64)mm_decls$ttcat[(source)];
    mm_decls$ttisblock[(dest)] = (i64)mm_decls$ttisblock[(source)];
}

u8 *mm_lib$getdottedname(struct mm_decls$strec *p) {
        static u8 str[256];
        u8 str2[256];
        struct mm_decls$strec *  owner;
    strcpy((u8 *)str,(*p).name);
    owner = (struct mm_decls$strec *)(*p).owner;
    L328 :;
    while ((!!(owner) && ((i64)(*owner).nameid != (i64)1))) {
        strcpy((u8 *)str2,(u8 *)str);
        strcpy((u8 *)str,(*owner).name);
        strcat((u8 *)str,(byte*)".");
        strcat((u8 *)str,(u8 *)str2);
        owner = (struct mm_decls$strec *)(*owner).owner;
L329 :;
    }
L330 :;
    ;
    return (u8 *)str;
}

struct mm_decls$strec *mm_lib$getavname(struct mm_decls$strec *owner,i64 id) {
        struct mm_decls$strec *  p;
        u8 str[32];
        u8 *  name;
    if (((id == (i64)12) && ((i64)(*owner).nameid != (i64)6))) {
        mm_support$serror((byte*)"Auto frame not in proc");
    }
;
    if ((id == (i64)12)) {
        msysc$m_print_startstr(str);
        msysc$m_print_str((byte*)"$av_",NULL);
        msysc$m_print_nogap();
        msysc$m_print_i64(++(mm_lib$nextavindex),NULL);
        msysc$m_print_end();
        ;
    }
    else {
        msysc$m_print_startstr(str);
        msysc$m_print_str((byte*)"$sv_",NULL);
        msysc$m_print_nogap();
        msysc$m_print_i64(++(mm_lib$nextsvindex),NULL);
        msysc$m_print_end();
        ;
    }
;
    name = mlib$pcm_copyheapstring((u8 *)str);
    mm_lex$addnamestr(name);
    p = mm_lib$getduplnameptr(owner,(struct mm_decls$strec *)mm_lex$addnamestr(name),id);
    (*p).flags = msysc$m_setdotindex((*p).flags,(i64)1,(u64)1u);
    (*p).mode = (i64)3;
    mm_lib$adddef(owner,p);
    return p;
}

void mm_lib$unionstr_clear(struct mm_decls$uflagsrec *u) {
    (*(u64 *)u) = (u64)0u;
}

void mm_lib$unionstr_append(struct mm_decls$uflagsrec *u,i64 c) {
    if (((i64)(*u).ulength == (i64)6)) {
        mm_support$serror((byte*)"Uflags overflow/a");
    }
;
    ++((*u).ulength);
    (*u).codes[((i64)(*u).ulength)-1] = c;
}

void mm_lib$unionstr_concat(struct mm_decls$uflagsrec *u,struct mm_decls$uflagsrec *v) {
        i64 ulen;
        i64 vlen;
        i64 i;
    ulen = (i64)(*u).ulength;
    vlen = (i64)(*v).ulength;
    if (((ulen + vlen) > (i64)7)) {
        mm_support$serror((byte*)"Uflags overflow/c");
    }
;
    for (i=(i64)1;i<=vlen;++i) {
L331 :;
        (*u).codes[((i + ulen))-1] = (i64)(*v).codes[(i)-1];
L332 :;
    }
L333 :;
    ;
    (*u).ulength = (ulen + vlen);
}

i64 mm_lib$unionstr_last(struct mm_decls$uflagsrec *u) {
    if (!!((i64)(*u).ulength)) {
        return (i64)(*u).codes[((i64)(*u).ulength)-1];
    }
;
    return (i64)0;
}

void mm_lib$unionstr_copy(struct mm_decls$uflagsrec *u,struct mm_decls$uflagsrec *v) {
    memcpy(u,v,(u64)8u);
}

i64 mm_lib$createrecordmode(struct mm_decls$strec *owner,i64 typedefx) {
        i64 m;
    if ((typedefx == (i64)0)) {
        m = mm_lib$createusertype(owner);
    }
    else {
        m = typedefx;
    }
;
    mm_decls$ttbasetype[(m)] = (i64)8;
    mm_decls$ttusercat[(m)] = (i64)1;
    mm_decls$ttcat[(m)] = (i64)5;
    mm_decls$ttisblock[(m)] = (i64)1;
    return m;
}

i64 mm_lib$createtuplemode(struct mm_decls$strec *owner,i64 (*elements)[],i64 elementslen,i64 typedefx) {
        i64 m;
        i64 i;
    if ((typedefx == (i64)0)) {
        m = mm_lib$createusertype(owner);
    }
    else {
        m = typedefx;
    }
;
    mm_decls$ttbasetype[(m)] = (i64)27;
    mm_decls$ttusercat[(m)] = (i64)1;
    mm_decls$ttlength[(m)] = elementslen;
    mm_decls$ttmult[(m)] = (i32 (*)[])mlib$pcm_alloc((elementslen * (i64)4));
    for (i=(i64)1;i<=elementslen;++i) {
L334 :;
        mm_lib$storemode(owner,(*elements)[(i)-1],&(*mm_decls$ttmult[(m)])[(i)-1]);
L335 :;
    }
L336 :;
    ;
    return m;
}

i64 mm_lib$convertstring(u8 *s,u8 *t) {
        i64 c;
        u8 *  t0;
    t0 = t;
    L337 :;
    while (!!((c = (i64)(u64)(*(s)++)))) {
        switch (c) {
        case 34:;
            {
                (*(t)++) = (u64)92u;
                (*(t)++) = '"';
            }
            break;
        case 10:;
            {
                (*(t)++) = (u64)92u;
                (*(t)++) = 'n';
            }
            break;
        case 13:;
            {
                (*(t)++) = (u64)92u;
                (*(t)++) = 'c';
            }
            break;
        case 9:;
            {
                (*(t)++) = (u64)92u;
                (*(t)++) = 't';
            }
            break;
        case 92:;
            {
                (*(t)++) = (u64)92u;
                (*(t)++) = (u64)92u;
            }
            break;
        case 7:;
        case 8:;
        case 26:;
        case 27:;
            {
                (*(t)++) = '<';
                (*(t)++) = (u64)((c / (i64)10) + (i64)48);
                (*(t)++) = (u64)((c % (i64)10) + (i64)48);
                (*(t)++) = '>';
            }
            break;
        default: {
            (*(t)++) = (u64)c;
        }
        } //SW
;
L338 :;
    }
L339 :;
    ;
    (*t) = (u64)0u;
    return (t - t0);
}

struct mlib$strbuffer *mm_lib$strexpr(struct mm_decls$unitrec *p) {
    mlib$gs_init((struct mlib$strbuffer *)mm_lib$exprstr);
    mm_lib$jevalx((struct mlib$strbuffer *)mm_lib$exprstr,(struct mm_decls$unitrec *)p);
    return (struct mlib$strbuffer *)mm_lib$exprstr;
}

void mm_lib$jevalx(struct mlib$strbuffer *dest,struct mm_decls$unitrec *p) {
        struct mm_decls$unitrec *  q;
        struct mm_decls$unitrec *  a;
        struct mm_decls$unitrec *  b;
        u8 str[500];
    if ((p == 0)) {
        return;
    }
;
    a = (*p).a;
    b = (*p).b;
    switch ((i64)(*p).tag) {
    case 1:;
        {
                        {i64 $temp = (i64)mm_decls$ttbasetype[((i64)(*p).mode)];
if (($temp==(i64)16) || ($temp==(i64)3) || ($temp==(i64)14) || ($temp==(i64)15)) {
                msysc$getstrint((*p).value,(u8 *)str);
            }
            else if (($temp==(i64)19) || ($temp==(i64)2) || ($temp==(i64)17) || ($temp==(i64)18)) {
                strcpy((u8 *)str,msysc$strword((*p).uvalue,0));
            }
            else if (($temp==(i64)12) || ($temp==(i64)1)) {
                str[((i64)1)-1] = (u64)(*p).uvalue;
                str[((i64)0)-1] = (u64)0u;
            }
            else if (($temp==(i64)5) || ($temp==(i64)4)) {
                msysc$m_print_startstr(str);
                msysc$m_print_r64((*p).xvalue,NULL);
                msysc$m_print_end();
                ;
            }
            else if (($temp==(i64)7)) {
                if ((((i64)(*p).mode == (i64)20) && !!((i64)(*p).isastring))) {
                    if (((i64)(*p).slength > (i64)250)) {
                        strcpy((u8 *)str,(byte*)"LONGSTR)");
                    }
                    else {
                        mm_lib$convertstring((*p).svalue,(u8 *)str);
                    }
;
                    mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"\"");
                    mm_support$gs_additem((struct mlib$strbuffer *)dest,(u8 *)str);
                    mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"\"");
                    return;
                }
                else {
                    msysc$m_print_startstr(str);
                    msysc$m_print_ptr((void *)(*p).value,NULL);
                    msysc$m_print_end();
                    ;
                }
;
            }
            else {
                strcpy((u8 *)str,(byte*)"<EVAL/CONST PROBABLY VOID>");
            }
            };
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(u8 *)str);
        }
        break;
    case 3:;
        {
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(*(*p).def).name);
        }
        break;
    case 31:;
    case 29:;
        {
            strcpy((u8 *)str,mm_tables$pclnames[((i64)(*p).pclop)]);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"(");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(u8 *)str);
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)b);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)")");
        }
        break;
    case 32:;
    case 14:;
    case 13:;
        {
            strcpy((u8 *)str,mm_tables$pclnames[((i64)(*p).pclop)]);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(u8 *)str);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"(");
            if (((i64)(*a).tag == (i64)52)) {
                mm_support$gs_additem((struct mlib$strbuffer *)dest,mm_lib$strmode((*a).value,(i64)1));
            }
            else {
                mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            }
;
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)")");
        }
        break;
    case 28:;
    case 86:;
        {
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"(");
            q = b;
            L340 :;
            while (!!(q)) {
                mm_lib$jevalx(dest,(struct mm_decls$unitrec *)q);
                q = (*q).nextunit;
                if (!!(q)) {
                    mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)",");
                }
;
L341 :;
            }
L342 :;
            ;
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)")");
        }
        break;
    case 40:;
    case 43:;
    case 41:;
    case 44:;
        {
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            if ((((i64)(*p).tag == (i64)43) || ((i64)(*p).tag == (i64)44))) {
                mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)".");
            }
;
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"[");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)b);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"]");
        }
        break;
    case 42:;
        {
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)".");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)b);
        }
        break;
    case 15:;
        {
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"(");
            q = a;
            L343 :;
            while (!!(q)) {
                mm_lib$jevalx(dest,(struct mm_decls$unitrec *)q);
                q = (*q).nextunit;
                if (!!(q)) {
                    mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)",");
                }
;
L344 :;
            }
L345 :;
            ;
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)")");
        }
        break;
    case 16:;
        {
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"(");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"..");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)b);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)")");
        }
        break;
    case 23:;
        {
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)":=");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)b);
        }
        break;
    case 90:;
        {
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"(");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"|");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)b);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"|");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)(*p).c);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)")");
        }
        break;
    case 52:;
        {
            mm_support$gs_additem((struct mlib$strbuffer *)dest,mm_lib$strmode((i64)(*p).mode,(i64)1));
        }
        break;
    case 48:;
    case 51:;
        {
            mm_support$gs_additem((struct mlib$strbuffer *)dest,mm_lib$strmode((i64)(*p).convmode,(i64)1));
            if (((i64)(*p).tag == (i64)51)) {
                mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"@");
            }
;
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"(");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)")");
        }
        break;
    case 49:;
        {
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"shorten(");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)")");
        }
        break;
    case 50:;
        {
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"cast(");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)")");
        }
        break;
    case 22:;
        {
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)":");
            if (!!(b)) {
                mm_lib$jevalx(dest,(struct mm_decls$unitrec *)(*p).b);
            }
            else {
                mlib$gs_str((struct mlib$strbuffer *)dest,(byte*)"-");
            }
;
        }
        break;
    case 45:;
        {
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"^");
        }
        break;
    case 39:;
        {
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"(");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)",");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)b);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)",");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)(*p).c);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)")");
        }
        break;
    case 4:;
        {
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"<JBLOCK>");
        }
        break;
    case 2:;
        {
            mlib$gs_str((struct mlib$strbuffer *)dest,(byte*)"<nullunit>");
        }
        break;
    case 46:;
        {
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"&");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            if (!!(b)) {
                mlib$gs_str((struct mlib$strbuffer *)dest,(byte*)"+");
                mlib$gs_strint((struct mlib$strbuffer *)dest,(*b).value);
            }
;
        }
        break;
    case 47:;
        {
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"&.");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
        }
        break;
    case 58:;
        {
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)"TYPESTR(");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            mm_support$gs_additem((struct mlib$strbuffer *)dest,(byte*)")");
        }
        break;
    case 62:;
    case 65:;
    case 64:;
        {
            mlib$gs_str((struct mlib$strbuffer *)dest,(byte*)"$");
            mlib$gs_str((struct mlib$strbuffer *)dest,(mm_tables$jtagnames[((i64)(*p).tag)] + (i64)2));
        }
        break;
    case 59:;
        {
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            mlib$gs_str((struct mlib$strbuffer *)dest,(byte*)".");
            mlib$gs_str((struct mlib$strbuffer *)dest,mm_tables$bitfieldnames[((i64)(*p).bitopindex)-1]);
        }
        break;
    case 83:;
        {
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            mlib$gs_str((struct mlib$strbuffer *)dest,(byte*)":");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)b);
        }
        break;
    case 57:;
        {
            mlib$gs_str((struct mlib$strbuffer *)dest,(byte*)"typeof(");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            mlib$gs_str((struct mlib$strbuffer *)dest,(byte*)")");
        }
        break;
    case 88:;
        {
            mlib$gs_str((struct mlib$strbuffer *)dest,(mm_tables$sysfnnames[((i64)(*p).fnindex)-1] + (i64)3));
            mlib$gs_str((struct mlib$strbuffer *)dest,(byte*)"(");
            if (!!(a)) {
                mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
            }
;
            mlib$gs_str((struct mlib$strbuffer *)dest,(byte*)")");
        }
        break;
    case 35:;
        {
            mlib$gs_str((struct mlib$strbuffer *)dest,(byte*)"incr ");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
        }
        break;
    case 10:;
        {
            mlib$gs_str((struct mlib$strbuffer *)dest,(byte*)"newstrinclude ");
            mm_lib$jevalx(dest,(struct mm_decls$unitrec *)a);
        }
        break;
    default: {
        msysc$m_print_startcon();
        msysc$m_print_str(mm_tables$jtagnames[((i64)(*p).tag)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        mm_support$gerror((byte*)"CAN'T DO JEVAL",(struct mm_decls$unitrec *)p);
    }
    } //SW
;
}

u8 *mm_lib$strmode(i64 m,i64 expand) {
        static u8 str[4096];
    mm_lib$istrmode(m,expand,(u8 *)str);
    return (u8 *)str;
}

u8 *mm_lib$strmode2(i64 m,i64 expand) {
        static u8 str[4096];
    mm_lib$istrmode(m,expand,(u8 *)str);
    return (u8 *)str;
}

void mm_lib$istrmode(i64 m,i64 expand,u8 *dest) {
        struct mm_decls$strec *  d;
        struct mm_decls$strec *  q;
        i64 needcomma;
        i64 i;
        i64 target;
        i64 mbase;
        i64 n;
        struct mlib$strbuffer sxx;
        struct mlib$strbuffer *  xx;
        u8 strdim[100];
        u8 *  prefix;
        struct mm_decls$typenamerec tn;
    xx = (struct mlib$strbuffer *)&sxx;
    if ((m < (i64)0)) {
        strcpy(dest,(byte*)"*");
        tn = mm_decls$typenames[(-(m))];
        if ((tn.defb == 0)) {
            strcat(dest,(byte*)"typeof(");
            strcat(dest,(*tn.defa).name);
            strcat(dest,(byte*)")");
        }
        else {
            if (!!(tn.defa)) {
                strcat(dest,(*tn.defa).name);
                strcat(dest,(byte*)".");
            }
;
            strcat(dest,(*tn.def).name);
        }
;
        return;
    }
;
    if (((m < (i64)29) && (m != (i64)7))) {
        strcpy(dest,mm_lib$typename(m));
        return;
    }
;
        {i64 $temp = (mbase = (i64)mm_decls$ttbasetype[(m)]);
if (($temp==(i64)7)) {
        strcpy(dest,(byte*)"ref ");
        target = (i64)mm_decls$tttarget[(m)];
        if (((target >= (i64)0) && ((i64)mm_decls$ttbasetype[(target)] == (i64)8))) {
            strcat(dest,mm_lib$typename(target));
        }
        else {
            mm_lib$istrmode((i64)mm_decls$tttarget[(m)],(i64)0,(dest + strlen(dest)));
        }
;
    }
    else if (($temp==(i64)10)) {
        if (!!(mm_decls$ttdimexpr[(m)])) {
            mm_support$gs_copytostr((struct mlib$strbuffer *)mm_lib$strexpr((struct mm_decls$unitrec *)mm_decls$ttdimexpr[(m)]),(u8 *)strdim);
            msysc$m_print_startstr(dest);
            msysc$m_print_setfmt((byte*)"@[#]");
            msysc$m_print_str(strdim,NULL);
            msysc$m_print_end();
            ;
        }
        else {
            if (!!((i64)mm_decls$ttlength[(m)])) {
                if (((i64)mm_decls$ttlower[(m)] == (i64)1)) {
                    msysc$m_print_startstr(dest);
                    msysc$m_print_setfmt((byte*)"[#]");
                    msysc$m_print_i64((((i64)mm_decls$ttlength[(m)] + (i64)mm_decls$ttlower[(m)]) - (i64)1),NULL);
                    msysc$m_print_end();
                    ;
                }
                else {
                    msysc$m_print_startstr(dest);
                    msysc$m_print_setfmt((byte*)"[#..#]");
                    msysc$m_print_i64((i64)mm_decls$ttlower[(m)],NULL);
                    msysc$m_print_i64((((i64)mm_decls$ttlength[(m)] + (i64)mm_decls$ttlower[(m)]) - (i64)1),NULL);
                    msysc$m_print_end();
                    ;
                }
;
            }
            else {
                if (((i64)mm_decls$ttlower[(m)] == (i64)1)) {
                    msysc$m_print_startstr(dest);
                    msysc$m_print_setfmt((byte*)"[]");
                    msysc$m_print_end();
                    ;
                }
                else {
                    msysc$m_print_startstr(dest);
                    msysc$m_print_setfmt((byte*)"[#:]");
                    msysc$m_print_i64((i64)mm_decls$ttlower[(m)],NULL);
                    msysc$m_print_end();
                    ;
                }
;
            }
;
        }
;
        mm_lib$istrmode((i64)mm_decls$tttarget[(m)],(i64)0,(dest + strlen(dest)));
    }
    else if (($temp==(i64)11)) {
        prefix = mm_tables$stdnames[(mbase)];
        if (!!(mm_decls$ttdimexpr[(m)])) {
            mm_support$gs_copytostr((struct mlib$strbuffer *)mm_lib$strexpr((struct mm_decls$unitrec *)mm_decls$ttdimexpr[(m)]),(u8 *)strdim);
            msysc$m_print_startstr(dest);
            msysc$m_print_setfmt((byte*)"@#[#:]");
            msysc$m_print_str(prefix,NULL);
            msysc$m_print_str(strdim,NULL);
            msysc$m_print_end();
            ;
        }
        else {
            if (((i64)mm_decls$ttlower[(m)] == (i64)1)) {
                strcpy(dest,prefix);
                strcat(dest,(byte*)"[]");
            }
            else {
                msysc$m_print_startstr(dest);
                msysc$m_print_setfmt((byte*)"#[#:]");
                msysc$m_print_str(prefix,NULL);
                msysc$m_print_i64((i64)mm_decls$ttlower[(m)],NULL);
                msysc$m_print_end();
                ;
            }
;
        }
;
        mm_lib$istrmode((i64)mm_decls$tttarget[(m)],(i64)0,(dest + strlen(dest)));
    }
    else if (($temp==(i64)8)) {
        if (!(!!(expand))) {
            strcpy(dest,mm_lib$typename(m));
            return;
        }
;
        strcpy(dest,(byte*)"");
        if ((expand != (i64)2)) {
            strcat(dest,mm_lib$typename((i64)mm_decls$ttbasetype[(m)]));
        }
;
        strcat(dest,(byte*)"(");
        d = mm_decls$ttnamedef[(m)];
        needcomma = (i64)0;
        q = (struct mm_decls$strec *)(*d).deflist;
        L346 :;
        while (!!(q)) {
            if (!!(needcomma)) {
                strcat(dest,(byte*)",");
            }
;
            needcomma = (i64)1;
            mm_lib$istrmode((i64)(*q).mode,(i64)0,(dest + strlen(dest)));
            strcat(dest,(byte*)" ");
            strcat(dest,(*q).name);
L347 :;
            q = (struct mm_decls$strec *)(*q).nextdef;
L349 :;
                    }
L348 :;
        ;
        strcat(dest,(byte*)")");
    }
    else if (($temp==(i64)0)) {
        strcpy(dest,(byte*)"void");
    }
    else if (($temp==(i64)29)) {
        strcpy(dest,mm_lib$typename(m));
    }
    else if (($temp==(i64)23)) {
        d = mm_decls$ttnamedef[(m)];
        strcpy(dest,(byte*)"proc(");
        q = (struct mm_decls$strec *)(*d).paramlist;
        needcomma = (i64)0;
        L350 :;
        while ((q != 0)) {
            if (!!(needcomma)) {
                strcat(dest,(byte*)",");
            }
;
            needcomma = (i64)1;
            mm_lib$istrmode((i64)(*q).mode,(i64)0,(dest + strlen(dest)));
            strcat(dest,(byte*)" ");
            strcat(dest,(*q).name);
            q = (struct mm_decls$strec *)(*q).nextdef;
L351 :;
        }
L352 :;
        ;
        strcat(dest,(byte*)")");
        if (((i64)(*d).mode != (i64)0)) {
            mm_lib$istrmode((i64)(*d).mode,(i64)0,(dest + strlen(dest)));
        }
;
    }
    else if (($temp==(i64)27)) {
        strcpy(dest,(byte*)"Tuple(");
        n = (i64)mm_decls$ttlength[(m)];
        for (i=(i64)1;i<=n;++i) {
L353 :;
            mm_lib$istrmode((i64)(*mm_decls$ttmult[(m)])[(i)-1],(i64)0,(dest + strlen(dest)));
            if ((i < n)) {
                strcat(dest,(byte*)",");
            }
;
L354 :;
        }
L355 :;
        ;
        strcat(dest,(byte*)")");
    }
    else if (($temp==(i64)26)) {
        strcpy(dest,(byte*)"bitfield");
    }
    else {
        if (((i64)mm_decls$ttbasetype[(m)] < (i64)29)) {
            strcpy(dest,(byte*)"Alias for:");
            mm_lib$istrmode((i64)mm_decls$tttarget[(m)],(i64)0,(dest + strlen(dest)));
        }
        else {
            msysc$m_print_startcon();
            msysc$m_print_str(mm_lib$typename(m),NULL);
            msysc$m_print_str(mm_lib$strmode((i64)mm_decls$ttbasetype[(m)],(i64)1),NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            mm_support$mcerror((byte*)"NEWSTRMODE");
        }
;
    }
    };
}

void mm_lib$addtoproclist(struct mm_decls$strec *d) {
        struct mm_decls$procrec *  pp;
    pp = (struct mm_decls$procrec *)mlib$pcm_alloc((i64)16);
    if ((mm_decls$proclist == 0)) {
        mm_decls$proclist = (mm_decls$proclistx = (struct mm_decls$procrec *)pp);
    }
    else {
        (*mm_decls$proclistx).nextproc = (struct mm_decls$procrec *)pp;
        mm_decls$proclistx = (struct mm_decls$procrec *)pp;
    }
;
    (*pp).def = d;
}

void mm_lib$addstatic(struct mm_decls$strec *d) {
        struct mm_decls$procrec *  pp;
    pp = (struct mm_decls$procrec *)mlib$pcm_alloc((i64)16);
    if ((mm_decls$staticlist == 0)) {
        mm_decls$staticlist = (mm_decls$staticlistx = (struct mm_decls$procrec *)pp);
    }
    else {
        (*mm_decls$staticlistx).nextproc = (struct mm_decls$procrec *)pp;
        mm_decls$staticlistx = (struct mm_decls$procrec *)pp;
    }
;
    (*pp).def = d;
}

void mm_lib$addexpconst(struct mm_decls$strec *d) {
        struct mm_decls$procrec *  pp;
    pp = (struct mm_decls$procrec *)mlib$pcm_alloc((i64)16);
    if ((mm_decls$constlist == 0)) {
        mm_decls$constlist = (mm_decls$constlistx = (struct mm_decls$procrec *)pp);
    }
    else {
        (*mm_decls$constlistx).nextproc = (struct mm_decls$procrec *)pp;
        mm_decls$constlistx = (struct mm_decls$procrec *)pp;
    }
;
    (*pp).def = d;
}

u8 *mm_lib$typename(i64 m) {
    if ((m >= (i64)0)) {
        return mm_decls$ttname[(m)];
    }
;
    return (*mm_decls$typenames[(-(m))].def).name;
}

struct mm_decls$unitrec *mm_lib$allocunitrec(void) {
        struct mm_decls$unitrec *  p;
    ++(mm_decls$nunits);
    if (!!((mm_lib$remainingunits)--)) {
        p = (struct mm_decls$unitrec *)mm_lib$unitheapptr;
        ++(mm_lib$unitheapptr);
        (*p).pos = (i64)mm_decls$lx.pos;
        (*p).moduleno = mm_decls$currmoduleno;
        (*p).subprogno = (i64)mm_decls$moduletosub[(mm_decls$currmoduleno)];
        return (struct mm_decls$unitrec *)p;
    }
;
    p = (struct mm_decls$unitrec *)(mm_lib$unitheapptr = (struct mm_decls$unitrec *)mlib$pcm_alloc((i64)2097152));
    memset(p,(i32)(i64)0,(u64)2097152u);
    mm_lib$remainingunits = (i64)32767;
    ++(mm_lib$unitheapptr);
    (*p).pos = (i64)mm_decls$lx.pos;
    (*p).moduleno = mm_decls$currmoduleno;
    (*p).subprogno = (i64)mm_decls$moduletosub[(mm_decls$currmoduleno)];
    return (struct mm_decls$unitrec *)p;
}

struct mm_decls$strec *mm_lib$createdupldef(struct mm_decls$strec *owner,struct mm_decls$strec *symptr,i64 id) {
        struct mm_decls$strec *  p;
    p = mm_lib$newstrec();
    (*p).name = (*symptr).name;
    (*p).namelen = (i64)(*symptr).namelen;
    (*p).symbol = (i64)81;
    (*p).owner = (struct mm_decls$strec *)owner;
    (*p).nameid = id;
    (*p).nextdupl = (*symptr).nextdupl;
    (*symptr).nextdupl = (struct mm_decls$strec *)p;
    if (!!(owner)) {
        if (((*owner).deflist == 0)) {
            (*owner).deflist = (struct mm_decls$strec *)((*owner).deflistx = (struct mm_decls$strec *)p);
        }
        else {
            (*(*owner).deflistx).nextdef = (struct mm_decls$strec *)p;
            (*owner).deflistx = (struct mm_decls$strec *)p;
        }
;
    }
;
    return p;
}

struct mm_decls$strec *mm_lib$createnewmoduledef(struct mm_decls$strec *owner,struct mm_decls$strec *symptr,i64 id) {
    return mm_lib$createdupldef(owner,symptr,id);
}

struct mm_decls$unitrec *mm_lib$duplunit(struct mm_decls$unitrec *p,i64 lineno) {
        struct mm_decls$unitrec *  q;
        i64 $av_1;
        i64 i;
    if ((p == 0)) {
        return 0;
    }
;
    q = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)(*p).tag);
    (*q) = (*p);
    (*q).nextunit = 0;
        ($av_1 = (i64)mm_tables$jsubs[((i64)(*q).tag)]);
    for (i=(i64)1;i<=$av_1;++i) {
L356 :;
        (*q).abc[(i)-1] = mm_lib$duplunit((*q).abc[(i)-1],(i64)0);
L357 :;
    }
L358 :;
    ;
    return q;
}

i64 mm_lib$checkblockreturn(struct mm_decls$unitrec *p) {
        struct mm_decls$unitrec *  e;
        struct mm_decls$unitrec *  wt;
        i64 m;
    if ((p == 0)) {
        return (i64)0;
    }
;
    m = (i64)(*p).mode;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)87)) {
        return (i64)1;
    }
    else if (($temp==(i64)120)) {
        return (i64)1;
    }
    else if (($temp==(i64)90)) {
        (*p).ifretflag = (i64)1;
        e = (*p).b;
        L359 :;
        while (!!(e)) {
            if (!(!!(mm_lib$checkblockreturn(e)))) {
                return (i64)0;
            }
;
L360 :;
            e = (*e).nextunit;
L362 :;
                    }
L361 :;
        ;
        return mm_lib$checkblockreturn((*p).c);
    }
    else if (($temp==(i64)4)) {
        e = (*p).a;
        if (!!(e)) {
            L363 :;
            while ((!!(e) && !!((*e).nextunit))) {
                e = (*e).nextunit;
L364 :;
            }
L365 :;
            ;
            return mm_lib$checkblockreturn(e);
        }
;
    }
    else if (($temp==(i64)103) || ($temp==(i64)105) || ($temp==(i64)104) || ($temp==(i64)106)) {
        (*p).ifretflag = (i64)1;
        wt = (*p).b;
        L366 :;
        while (!!(wt)) {
            if (!(!!(mm_lib$checkblockreturn((*wt).b)))) {
                return (i64)0;
            }
;
            wt = (*wt).nextunit;
L367 :;
        }
L368 :;
        ;
        return mm_lib$checkblockreturn((*p).c);
    }
    else if (($temp==(i64)5)) {
        return (i64)1;
    }
    };
    if ((!!((i64)mm_tables$jisexpr[((i64)(*p).tag)]) && (m != (i64)0))) {
        if ((u64)1u) {
            mm_lib$insertunit(p,(i64)87);
            (*p).mode = m;
        }
;
        return (i64)1;
    }
    else {
        return (i64)0;
    }
;
}

i64 mm_lib$isconstunit(struct mm_decls$unitrec *a) {
    return (i64)(*a).isconst;
}

void mm_lib$getownername(struct mm_decls$strec *d,u8 *dest) {
        struct mm_decls$strec *  owner;
    owner = (struct mm_decls$strec *)(*d).owner;
    if (((owner == 0) || ((i64)(*owner).nameid == (i64)1))) {
        return;
    }
;
    mm_lib$getownername(owner,dest);
    strcat(dest,(*owner).name);
    strcat(dest,(byte*)".");
}

i64 mm_lib$getalignment(i64 m) {
        i64 a;
        {i64 $temp = (i64)mm_decls$ttbasetype[(m)];
if (($temp==(i64)10)) {
        return mm_lib$getalignment((i64)mm_decls$tttarget[(m)]);
    }
    else if (($temp==(i64)8)) {
        return (i64)8;
    }
    else {
        if (!!((i64)mm_decls$ttisblock[(m)])) {
            return (i64)8;
        }
;
    }
    };
    a = (i64)mm_decls$ttsize[(m)];
    if ((a==(i64)1) || (a==(i64)2) || (a==(i64)4) || (a==(i64)8)) {
        return a;
    }
    else if ((a==(i64)0)) {
        return (i64)8;
    }
;
    msysc$m_print_startcon();
    msysc$m_print_str(mm_lib$strmode(m,(i64)1),NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mm_support$gerror((byte*)"GETALIGN SIZE NOT 1248",0);
    return (i64)0;
}

i64 mm_lib$ispoweroftwo(i64 x) {
        i64 a;
        i64 n;
        i64 $av_1;
    a = (i64)1;
    n = (i64)0;
    $av_1 = (i64)60;
    while ($av_1-- > 0) {
L369 :;
        ++(n);
        a = (a << (i64)1);
        if ((a == x)) {
            return n;
        }
;
L370 :;
    }
L371 :;
    ;
    return (i64)0;
}

void mm_lib$addlistunit(struct mm_decls$unitrec **ulist,struct mm_decls$unitrec **ulistx,struct mm_decls$unitrec *p) {
    if (((*ulist) == 0)) {
        (*ulist) = ((*ulistx) = p);
    }
    else {
        (*(*ulistx)).nextunit = p;
    }
;
    (*ulistx) = p;
}

i64 mm_lib$storemode(struct mm_decls$strec *owner,i64 m,i32 *pmode) {
        struct mm_decls$typenamerec *  r;
    if ((m >= (i64)0)) {
        (*pmode) = m;
        return m;
    }
;
    r = &mm_decls$typenames[(-(m))];
    if (((*r).pmode == 0)) {
        (*r).owner = owner;
        (*pmode) = m;
        (*r).pmode = pmode;
        if (((*r).pmode == 0)) {
            mm_support$serror((byte*)"PMODE=NIL");
        }
;
        return m;
    }
;
    m = mm_lib$newtypename((*r).defa,(*r).defb);
    r = &mm_decls$typenames[(-(m))];
    (*r).owner = owner;
    (*pmode) = m;
    (*r).pmode = pmode;
    return m;
}

i64 mm_lib$gettypebase(i64 m) {
    switch ((i64)mm_decls$ttbasetype[(m)]) {
    case 14:;
    case 15:;
    case 16:;
        {
            return (i64)3;
        }
        break;
    case 17:;
    case 18:;
    case 19:;
        {
            return (i64)3;
        }
        break;
    case 4:;
        {
            return (i64)5;
        }
        break;
    case 12:;
        {
            return (i64)1;
        }
        break;
    default: {
        return m;
    }
    } //SW
;
}

void mm_lib$writegsfile(u8 *filename,struct mlib$strbuffer *d) {
        void *  f;
    f = fopen(filename,(byte*)"wb");
    mlib$gs_println((struct mlib$strbuffer *)d,f);
    fclose(f);
}

void mm_lib$addtolog(u8 *filename,void *logdest) {
        void *  f;
        i64 c;
    f = fopen(filename,(byte*)"rb");
    if ((f == 0)) {
        return;
    }
;
    L372 :;
    while (1) {
        c = fgetc(f);
        if ((c == (i64)-1)) {
            goto L373 ;
        }
;
        fputc((i32)c,logdest);
    }
L373 :;
    ;
    fclose(f);
}

struct mm_decls$strec *mm_lib$getprocretmodes(struct mm_decls$unitrec *p) {
        struct mm_decls$unitrec *  a;
    if (((i64)(*p).tag != (i64)28)) {
        mm_support$txerror((byte*)"multass/need multfn",0);
    }
;
    a = (*p).a;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)3)) {
        return (*a).def;
    }
    else {
        return mm_decls$ttnamedef[((i64)mm_decls$tttarget[((i64)(*a).mode)])];
    }
    };
}

i64 mm_lib$getmemmode(struct mm_decls$unitrec *p) {
    if (!!((i64)(*p).memmode)) {
        return (i64)(*p).memmode;
    }
;
    return (i64)(*p).mode;
}

i64 mm_lib$getpclmode(i64 t) {
        i64 u;
    u = (i64)mm_decls$ttbasetype[(t)];
    if ((u==(i64)1)) {
        u = (i64)2;
    }
    else if ((u==(i64)12)) {
        u = (i64)17;
    }
    else if ((u==(i64)8) || (u==(i64)10)) {
        if (!(!!((i64)mm_decls$ttisblock[(t)]))) {
                        {i64 $temp = (i64)mm_decls$ttsize[(t)];
if (($temp==(i64)8)) {
                u = (i64)2;
            }
            else if (($temp==(i64)4)) {
                u = (i64)19;
            }
            else if (($temp==(i64)2)) {
                u = (i64)18;
            }
            else {
                u = (i64)17;
            }
            };
        }
;
    }
;
    return u;
}

u8 *mm_lib$getfullname(struct mm_decls$strec *d) {
        static u8 str[128];
        struct mm_decls$strec *  chain[16];
        i64 n;
        struct mm_decls$strec *  e;
        i64 i;
    n = (i64)0;
    e = d;
    if (!!(msysc$m_getdotindex((i64)(*d).flags,(i64)12))) {
        return (*d).name;
    }
;
    L374 :;
    do {
        chain[(++(n))-1] = e;
        e = (struct mm_decls$strec *)(*e).owner;
L375 :;
    }
    while (!((e == 0) || ((i64)(*e).nameid == (i64)1)));
L376 :;
    ;
    strcpy(str,(*chain[(n)-1]).name);
    for (i=(n - (i64)1);i>=(i64)1;--i) {
L377 :;
        strcat(str,(byte*)".");
        strcat(str,(*chain[(i)-1]).name);
L378 :;
    }
L379 :;
    ;
    return str;
}

u8 *mm_lib$getbasename(u8 *s) {
        u8 *  t;
    t = ((s + strlen(s)) - (i64)1);
    L380 :;
    while (((t > s) && ((u64)(*(t - (i64)1)) != '.'))) {
        --(t);
L381 :;
    }
L382 :;
    ;
    return t;
}

// START
void mm_lib$start(void) {

}

void mc_libc$cccomment(u8 *s) {
    mc_libc$ccstr((byte*)"/* ",(i64)0);
    mc_libc$ccstr(s,(i64)0);
    mc_libc$ccstr((byte*)" */",(i64)0);
    mc_libc$ccsendline();
}

void mc_libc$ccblank(void) {
    mc_libc$ccsendline();
}

void mc_libc$cclinecomment(u8 *s) {
    mc_libc$cccomment(s);
}

void mc_libc$ccchar(i64 c) {
    (*mc_libc$clineptr) = (u64)c;
    ++(mc_libc$clineptr);
}

void mc_libc$cctab(i64 level) {
        i64 $av_1;
    $av_1 = (level * (i64)4);
    while ($av_1-- > 0) {
L383 :;
        (*(mc_libc$clineptr)++) = ' ';
L384 :;
    }
L385 :;
    ;
}

void mc_libc$ccstr(u8 *s,i64 level) {
    if (!!(level)) {
        mc_libc$cctab(level);
    }
;
    L386 :;
    while (!!((u64)(*s))) {
        (*(mc_libc$clineptr)++) = (u64)(*(s)++);
L387 :;
    }
L388 :;
    ;
}

void mc_libc$ccstrline(u8 *cstr) {
    mc_libc$ccstr(cstr,(i64)0);
    mc_libc$ccsendline();
}

void mc_libc$ccstrsemi(u8 *cstr) {
    mc_libc$ccstr(cstr,(i64)0);
    mc_libc$ccchar((i64)59);
    mc_libc$ccsendline();
}

void mc_libc$ccstrsemiu(struct mm_decls$unitrec *p) {
    mc_blockc$evalunit(p);
    mc_libc$ccchar((i64)59);
    mc_libc$ccsendline();
}

void mc_libc$ccsendline(void) {
    (*mc_libc$clineptr) = (u64)0u;
    mlib$gs_strln((struct mlib$strbuffer *)mm_lib$dest,(u8 *)mc_libc$clinebuffer);
    mc_libc$ccinitline();
}

void mc_libc$ccint(i64 a) {
    mc_libc$ccstr(msysc$strint(a,0),(i64)0);
}

void mc_libc$ccinitline(void) {
    mc_libc$clineptr = (u8 *)mc_libc$clinebuffer;
    mc_libc$clineend = ((mc_libc$clineptr + (i64)4096) - (i64)1);
}

u8 *mc_libc$strmodec(i64 m,u8 *name,i64 addtab) {
        static u8 str[1024];
        u8 *  oldlineptr;
    oldlineptr = mc_libc$clineptr;
    mc_libc$clineptr = (u8 *)str;
    mc_libc$strmodec2(m,name,addtab);
    (*mc_libc$clineptr) = (u64)0u;
    mc_libc$clineptr = oldlineptr;
    return (u8 *)str;
}

void mc_libc$strmodec2(i64 m,u8 *name,i64 addtab) {
        u8 *  suffix;
        u8 *  sp;
        u8 *  spsuffix;
        u8 str[1024];
        u8 buffer[1024];
        i64 target;
    if ((!!(name) && !!((u64)(*name)))) {
        suffix = name;
        strcpy((u8 *)str,(byte*)" ");
        strcat((u8 *)str,name);
        spsuffix = (u8 *)str;
        sp = (byte*)"  ";
                {u64 $temp = (u64)(*name);
if (($temp=='[') || ($temp=='(') || ($temp=='*')) {
            //lab1:
L389 :;
;
            sp = (byte*)"";
        }
        else {
            if (!(!!(addtab))) {
                goto L389 ;
;
            }
;
        }
        };
    }
    else {
        sp = (suffix = (spsuffix = (byte*)""));
    }
;
        {i64 $temp = (i64)mm_decls$ttbasetype[(m)];
if (($temp==(i64)16) || ($temp==(i64)19) || ($temp==(i64)3) || ($temp==(i64)2) || ($temp==(i64)4) || ($temp==(i64)5) || ($temp==(i64)0)) {
        mc_libc$ccstr(mc_libc$strmodex(m),(i64)0);
        mc_libc$ccstr(spsuffix,(i64)0);
    }
    else if (($temp==(i64)7)) {
                {i64 $temp = (i64)mm_decls$ttbasetype[((i64)mm_decls$tttarget[(m)])];
if (($temp==(i64)23)) {
            if (((i64)mm_decls$tttarget[(m)] == (i64)23)) {
                mc_libc$ccstr((byte*)"REF PROC",(i64)0);
                mc_libc$ccstr(suffix,(i64)0);
            }
            else {
                mc_libc$ccstr(mc_libc$strprocsig(mm_decls$ttnamedef[(m)],suffix,(i64)0),(i64)0);
            }
;
        }
        else if (($temp==(i64)24)) {
            mc_libc$ccstr((byte*)"int *",(i64)0);
            mc_libc$ccstr(spsuffix,(i64)0);
        }
        else {
            target = (i64)mm_decls$tttarget[(m)];
            if (((i64)mm_decls$ttbasetype[(target)] == (i64)10)) {
                msysc$m_print_startstr(buffer);
                msysc$m_print_setfmt((byte*)"(*#)");
                msysc$m_print_str(suffix,NULL);
                msysc$m_print_end();
                ;
            }
            else {
                msysc$m_print_startstr(buffer);
                msysc$m_print_setfmt((byte*)"*##");
                msysc$m_print_str(sp,NULL);
                msysc$m_print_str(suffix,NULL);
                msysc$m_print_end();
                ;
            }
;
            mc_libc$strmodec2(target,(u8 *)buffer,(i64)1);
        }
        };
    }
    else if (($temp==(i64)10)) {
        if (!!((i64)mm_decls$ttlength[(m)])) {
            msysc$m_print_startstr(buffer);
            msysc$m_print_setfmt((byte*)"#[#]");
            msysc$m_print_str(suffix,NULL);
            msysc$m_print_i64((i64)mm_decls$ttlength[(m)],NULL);
            msysc$m_print_end();
            ;
        }
        else {
            msysc$m_print_startstr(buffer);
            msysc$m_print_str(suffix,NULL);
            msysc$m_print_nogap();
            msysc$m_print_str((byte*)"[]",NULL);
            msysc$m_print_end();
            ;
        }
;
        mc_libc$strmodec2((i64)mm_decls$tttarget[(m)],(u8 *)buffer,(i64)1);
    }
    else if (($temp==(i64)8)) {
        mc_libc$ccstr((byte*)"struct ",(i64)0);
        mc_libc$ccstr(mc_libc$getfullnamec(mm_decls$ttnamedef[(m)]),(i64)0);
        mc_libc$ccstr(spsuffix,(i64)0);
    }
    else if (($temp==(i64)11)) {
        mc_libc$ccstr((byte*)"Slice ",(i64)0);
        mc_libc$ccstr(spsuffix,(i64)0);
    }
    else {
        mc_libc$ccstr(mc_libc$strmodex(m),(i64)0);
        mc_libc$ccstr(spsuffix,(i64)0);
    }
    };
}

static u8 *mc_libc$strmodex(i64 m) {
        {i64 $temp = (i64)mm_decls$ttbasetype[(m)];
if (($temp==(i64)16)) {
        return (byte*)"i32";
    }
    else if (($temp==(i64)3)) {
        return (byte*)"i64";
    }
    else if (($temp==(i64)14)) {
        return (byte*)"i8";
    }
    else if (($temp==(i64)15)) {
        return (byte*)"i16";
    }
    else if (($temp==(i64)19)) {
        return (byte*)"u32";
    }
    else if (($temp==(i64)2)) {
        return (byte*)"u64";
    }
    else if (($temp==(i64)17)) {
        return (byte*)"byte";
    }
    else if (($temp==(i64)18)) {
        return (byte*)"u16";
    }
    else if (($temp==(i64)4)) {
        return (byte*)"r32";
    }
    else if (($temp==(i64)5)) {
        return (byte*)"r64";
    }
    else if (($temp==(i64)12)) {
        return (byte*)"u8";
    }
    else if (($temp==(i64)1)) {
        return (byte*)"u64";
    }
    else {
        return mm_lib$strmode(m,(i64)1);
    }
    };
    return (byte*)"";
}

u8 *mc_libc$strprocsig(struct mm_decls$strec *p,u8 *name,i64 showparamnames) {
        u8 paramstr[512];
        u8 buffer[512];
        struct mm_decls$strec *  pm;
        i64 rettype;
        u8 *  stdcall;
        u8 *  scallback;
    pm = (struct mm_decls$strec *)(*p).paramlist;
    rettype = (i64)(*p).mode;
    strcpy((u8 *)paramstr,(byte*)"(");
    if (!!(pm)) {
        L390 :;
        while (!!(pm)) {
            strcat((u8 *)paramstr,mc_libc$strmodec((i64)(*pm).mode,(!!(showparamnames) ? (*pm).name : (byte*)""),(i64)0));
            pm = (struct mm_decls$strec *)(*pm).nextparam;
            if (!!(pm)) {
                strcat((u8 *)paramstr,(byte*)",");
            }
;
L391 :;
        }
L392 :;
        ;
        if (!!((i64)(*p).varparams)) {
            if (!!((*p).paramlist)) {
                strcat((u8 *)paramstr,(byte*)",");
            }
;
            strcat((u8 *)paramstr,(byte*)"...");
        }
;
        strcat((u8 *)paramstr,(byte*)")");
    }
    else {
        if (!!((i64)(*p).varparams)) {
            strcat((u8 *)paramstr,(byte*)"...");
        }
        else {
            strcat((u8 *)paramstr,(byte*)"void)");
        }
;
    }
;
    stdcall = (byte*)"";
    if (((i64)(*p).fflang == (i64)1)) {
        if ((mm_decls$targetbits == (i64)64)) {
        }
        else {
            stdcall = (byte*)"__stdcall ";
        }
;
    }
;
    if (!!(msysc$m_getdotindex((i64)(*p).flags,(i64)5))) {
        scallback = (byte*)"gcc_callback";
    }
    else {
        scallback = (byte*)"";
    }
;
    if ((name == 0)) {
        msysc$m_print_startstr(buffer);
        msysc$m_print_str(stdcall,NULL);
        msysc$m_print_nogap();
        msysc$m_print_str(mc_libc$getprocname(p),NULL);
        msysc$m_print_nogap();
        msysc$m_print_str(paramstr,NULL);
        msysc$m_print_end();
        ;
    }
    else {
        msysc$m_print_startstr(buffer);
        msysc$m_print_setfmt((byte*)"#(#*#)#");
        msysc$m_print_str(scallback,NULL);
        msysc$m_print_str(stdcall,NULL);
        msysc$m_print_str(name,NULL);
        msysc$m_print_str(paramstr,NULL);
        msysc$m_print_end();
        ;
    }
;
    return mlib$pcm_copyheapstring(mc_libc$strmodec(rettype,(u8 *)buffer,(i64)!(!!(showparamnames))));
}

u8 *mc_libc$getprocname(struct mm_decls$strec *d) {
        u8 *  name;
    name = (*d).name;
        {i64 $temp = (i64)(*d).fflang;
if (($temp==(i64)2) || ($temp==(i64)1)) {
        if (!!((*d).truename)) {
            return (*d).truename;
        }
;
        return name;
    }
    else {
        return mc_libc$getfullnamec(d);
    }
    };
    return (byte*)"";
}

u8 *mc_libc$getfullnamec(struct mm_decls$strec *d) {
    return mc_libc$getfullnamec2(d);
}

u8 *mc_libc$getfullnamec2(struct mm_decls$strec *d) {
        static u8 str[256];
        {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)6) || ($temp==(i64)11) || ($temp==(i64)5) || ($temp==(i64)10)) {
                {i64 $temp = (i64)(*(*d).owner).nameid;
if (($temp==(i64)3)) {
            strcpy((u8 *)str,(*(*d).owner).name);
            strcat((u8 *)str,(byte*)"$");
            strcat((u8 *)str,(*d).name);
            return (u8 *)str;
        }
        else if (($temp==(i64)6)) {
            if (((i64)(*d).nameid == (i64)5)) {
                strcpy((u8 *)str,(*(*(*d).owner).owner).name);
                strcat((u8 *)str,(byte*)"$");
                strcat((u8 *)str,(*(*d).owner).name);
                strcat((u8 *)str,(byte*)"$");
                strcat((u8 *)str,(*d).name);
                return (u8 *)str;
            }
;
        }
        };
    }
    };
    if (!!((*d).truename)) {
        return (*d).truename;
    }
;
    return (*d).name;
}

u8 *mc_libc$genclabel(i64 n,i64 colon) {
        static u8 str[16];
    msysc$m_print_startstr(str);
    msysc$m_print_str((byte*)"L",NULL);
    msysc$m_print_nogap();
    msysc$m_print_i64(n,NULL);
    msysc$m_print_str((!!(colon) ? (byte*)":;" : (byte*)""),NULL);
    msysc$m_print_end();
    ;
    return (u8 *)str;
}

void mc_libc$genrecorddef(i64 m) {
        static i64 seqno = (i64)0;
        struct mm_decls$strec *  q;
        i64 indent;
        i64 index;
        i64 nref;
        i64 target;
        byte flags[8];
        byte f;
        u8 *  name;
        struct mm_decls$strec *  d;
        i64 $av_1;
    d = mm_decls$ttnamedef[(m)];
    name = (*d).name;
    if (!!((i64)(*d).align)) {
        mc_libc$ccstrline((byte*)"#pragma pack(8)");
    }
;
    mc_libc$ccstr((byte*)"struct ",(i64)0);
    mc_libc$ccstr(mc_libc$getfullnamec(d),(i64)0);
    mc_libc$ccstrline((byte*)" {");
    indent = (i64)1;
    q = (struct mm_decls$strec *)(*d).deflist;
    L393 :;
    while (!!(q)) {
        memcpy(&flags,&(*q).uflags,(u64)8u);
        index = (i64)1;
        flags[((i64)8)-1] = (i64)0;
                {i64 $temp = (i64)(*q).nameid;
if (($temp==(i64)14)) {
            L396 :;
            while ((((f = (i64)flags[(index)-1]) == (i64)83) || ((i64)f == (i64)85))) {
                ++(index);
                mc_libc$cctab(indent);
                mc_libc$ccstrline((((i64)f == (i64)83) ? (byte*)"struct {" : (byte*)"union {"));
                ++(indent);
L397 :;
            }
L398 :;
            ;
                        {i64 $temp = (i64)mm_decls$ttbasetype[((i64)(*q).mode)];
if (($temp==(i64)7)) {
                target = (i64)mm_decls$tttarget[((i64)(*q).mode)];
                nref = (i64)1;
                L399 :;
                while (((i64)mm_decls$ttbasetype[(target)] == (i64)7)) {
                    target = (i64)mm_decls$tttarget[(target)];
                    ++(nref);
L400 :;
                }
L401 :;
                ;
                if ((target == m)) {
                    mc_libc$cctab(indent);
                    mc_libc$ccstr((byte*)"struct ",(i64)0);
                    mc_libc$ccstr(mc_libc$getfullnamec(d),(i64)0);
                    $av_1 = nref;
                    while ($av_1-- > 0) {
L402 :;
                        mc_libc$ccchar((i64)42);
L403 :;
                    }
L404 :;
                    ;
                    mc_libc$ccchar((i64)32);
                    mc_libc$ccstrsemi((*q).name);
                }
                else {
                    goto L405 ;
;
                }
;
            }
            else if (($temp==(i64)26)) {
            }
            else {
                //normal:
L405 :;
;
                mc_libc$cctab(indent);
                mc_libc$ccstrsemi(mc_libc$strmodec((i64)(*q).mode,(*q).name,(i64)1));
            }
            };
            if (((i64)flags[(index)-1] == (i64)42)) {
                ++(index);
            }
;
            L406 :;
            while (((i64)flags[(index)-1] == (i64)69)) {
                --(indent);
                ++(index);
                mc_libc$cctab(indent);
                mc_libc$ccstrsemi((byte*)"}");
L407 :;
            }
L408 :;
            ;
        }
        else {
            mm_support$gerror((byte*)"Non-field in struct",0);
        }
        };
        q = (struct mm_decls$strec *)(*q).nextdef;
L394 :;
    }
L395 :;
    ;
    mc_libc$ccstrsemi((byte*)"}");
    if (!!((i64)(*d).align)) {
        mc_libc$ccstrline((byte*)"#pragma pack(1)");
    }
;
    mc_libc$ccblank();
}

void mc_libc$genrecordfwd(i64 m) {
    mc_libc$ccstr((byte*)"struct ",(i64)0);
    mc_libc$ccstrsemi(mc_libc$getfullnamec(mm_decls$ttnamedef[(m)]));
}

void mc_libc$do_initdata(struct mm_decls$unitrec *p,i64 docomma,i64 level) {
    if ((p == 0)) {
        return;
    }
;
    if (((i64)(*p).tag != (i64)15)) {
        if (((((i64)(*p).tag == (i64)1) && ((i64)(*p).mode == (i64)20)) && !!((i64)(*p).isastring))) {
            if (((i64)(*p).slength == (i64)0)) {
                mc_libc$ccstr((byte*)"(byte*)\"\"",(i64)0);
            }
            else {
                mc_libc$cclongstr((*p).svalue,(i64)(*p).slength);
            }
;
        }
        else {
            mc_blockc$evalunit(p);
        }
;
        if (!!(docomma)) {
            mc_libc$ccchar((i64)44);
        }
;
        return;
    }
;
    mc_libc$do_makelist((*p).a,(i64)(*p).length,docomma,(level + (i64)1));
}

void mc_libc$cclongstr(u8 *svalue,i64 length) {
    if (((mc_libc$clineptr + (length * (i64)2)) >= mc_libc$clineend)) {
        mc_libc$ccsendline();
        mlib$gs_str((struct mlib$strbuffer *)mm_lib$dest,mc_libc$strstringc(svalue,length));
    }
    else {
        mc_libc$dxstr(mc_libc$strstringc(svalue,length));
    }
;
}

static void mc_libc$do_makelist(struct mm_decls$unitrec *a,i64 length,i64 docomma,i64 level) {
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  q;
        i64 $av_1;
    p = a;
    if (((length <= (i64)10) && ((i64)(*a).tag != (i64)15))) {
        mc_libc$ccstr((byte*)"{",(i64)0);
        L409 :;
        while (!!(p)) {
            q = (*p).nextunit;
            mc_libc$do_initdata(p,(i64)(q != 0),level);
            p = q;
L410 :;
        }
L411 :;
        ;
    }
    else {
        mc_libc$ccstrline((byte*)"{");
        L412 :;
        while (!!(p)) {
            q = (*p).nextunit;
            $av_1 = level;
            while ($av_1-- > 0) {
L415 :;
                mc_libc$ccstr((byte*)"    ",(i64)0);
L416 :;
            }
L417 :;
            ;
            mc_libc$do_initdata(p,(i64)(q != 0),level);
            p = q;
            mc_libc$ccstrline((byte*)"");
L413 :;
        }
L414 :;
        ;
    }
;
    mc_libc$ccchar((i64)125);
    if (!!(docomma)) {
        mc_libc$ccchar((i64)44);
    }
;
}

i64 mc_libc$issimplec(struct mm_decls$unitrec *p) {
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)1) || ($temp==(i64)3)) {
        return (i64)1;
    }
    };
    return (i64)0;
}

u8 *mc_libc$strstringc(u8 *s,i64 length) {
        i64 c;
        static u8 str[1024];
        u8 *  dest;
        u8 *  t;
    if ((length > (i64)512)) {
        dest = (u8 *)mlib$pcm_alloc((length * (i64)2));
    }
    else {
        dest = (u8 *)str;
    }
;
    t = dest;
    strcpy(t,(byte*)"(byte*)");
    t += strlen(t);
    (*(t)++) = '"';
    L418 :;
    while (!!((c = (i64)(u64)(*(s)++)))) {
        if ((c==(i64)34)) {
            (*(t)++) = (u64)92u;
            (*(t)++) = '"';
        }
        else if ((c==(i64)10)) {
            (*(t)++) = (u64)92u;
            (*(t)++) = 'n';
        }
        else if ((c==(i64)13)) {
            (*(t)++) = (u64)92u;
            (*(t)++) = 'r';
        }
        else if ((c==(i64)9)) {
            (*(t)++) = (u64)92u;
            (*(t)++) = 't';
        }
        else if ((c==(i64)92)) {
            (*(t)++) = (u64)92u;
            (*(t)++) = (u64)92u;
        }
        else {
            if ((c < (i64)32)) {
                (*(t)++) = (u64)92u;
                (*(t)++) = (u64)((c >> (i64)6) + (i64)48);
                (*(t)++) = (u64)(((c >> (i64)3) & (i64)7) + (i64)48);
                (*(t)++) = (u64)((c & (i64)7) + (i64)48);
            }
            else {
                (*(t)++) = (u64)c;
            }
;
        }
;
L419 :;
    }
L420 :;
    ;
    (*(t)++) = '"';
    (*t) = (u64)0u;
    return dest;
}

void mc_libc$do_syscallproc(u8 *fnname,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
        u8 str[32];
    if (((((i64)mm_decls$msyslevel == (i64)1) && (strlen(fnname) >= (i64)6)) && !!(mlib$eqbytes((void *)fnname,(byte*)"msysc$",(i64)6)))) {
        strcpy(str,(byte*)"mminc");
        strcat(str,(fnname + (i64)5));
        fnname = str;
    }
;
    mc_libc$dxstr(fnname);
    mc_libc$dxstr((byte*)"(");
    if (!!(a)) {
        mc_libc$evalsysparam(a);
        if (!!(b)) {
            mc_libc$dxstr((byte*)",");
            mc_libc$evalsysparam(b);
        }
;
    }
;
    mc_libc$dxstr((byte*)");");
    mc_libc$ccsendline();
    mc_libc$cctab(mc_blockc$blocklevel);
}

static void mc_libc$evalsysparam(struct mm_decls$unitrec *a) {
    if ((a==0)) {
    }
    else if ((a==mc_libc$nilunit)) {
        mc_libc$dxstr((byte*)"NULL");
    }
    else {
        mc_blockc$evalunit(a);
    }
;
}

void mc_libc$dxstr(u8 *str) {
    L421 :;
    while (!!((u64)(*str))) {
        (*mc_libc$clineptr) = (u64)(*str);
        ++(mc_libc$clineptr);
        ++(str);
L422 :;
    }
L423 :;
    ;
}

void mc_libc$dxchar(i64 ch) {
    (*(mc_libc$clineptr)++) = (u64)ch;
}

void mc_libc$dxint(i64 a) {
    mc_libc$dxstr(msysc$strint(a,0));
}

// START
void mc_libc$start(void) {

}

i64 mm_libsourcesc$findsyslib(u8 *filename) {
        i64 i;
    if ((u64)0u) {
        return (i64)0;
    }
;
    filename = mlib$extractfile(filename);
    for (i=(i64)1;i<=(i64)5;++i) {
L424 :;
        if (!!(mlib$eqstring(mm_libsourcesc$syslibnames[(i)-1],filename))) {
            if (!!((i64)mm_libsourcesc$syslibfileno[(i)-1])) {
                return (i64)mm_libsourcesc$syslibfileno[(i)-1];
            }
;
            if ((mm_decls$nsourcefiles >= (i64)1000)) {
                mm_support$loaderror((byte*)"fsl: too many files",(byte*)"",(byte*)"");
            }
;
            ++(mm_decls$nsourcefiles);
            mm_decls$sourcefilenames[(mm_decls$nsourcefiles)] = mlib$pcm_copyheapstring(filename);
            mm_decls$sourcefiletext[(mm_decls$nsourcefiles)] = mlib$pcm_copyheapstring(mm_libsourcesc$libtext[(i)-1]);
            if (!!((i64)mm_decls$fwritema)) {
                mm_decls$sourcefiledupl[(mm_decls$nsourcefiles)] = mlib$pcm_copyheapstring(mm_libsourcesc$libtext[(i)-1]);
            }
;
            mm_decls$sourcefilesizes[(mm_decls$nsourcefiles)] = strlen(mm_libsourcesc$libtext[(i)-1]);
            mm_decls$sourcefilepaths[(mm_decls$nsourcefiles)] = (byte*)"";
            mm_decls$sourcefilespecs[(mm_decls$nsourcefiles)] = (byte*)"";
            mm_decls$sourcefilesys[(mm_decls$nsourcefiles)] = (i64)1;
            mm_decls$sourcefilesupport[(mm_decls$nsourcefiles)] = (i64)0;
            mm_libsourcesc$syslibfileno[(i)-1] = mm_decls$nsourcefiles;
            return mm_decls$nsourcefiles;
        }
;
L425 :;
    }
L426 :;
    ;
    return (i64)0;
}

// START
void mm_libsourcesc$start(void) {

}

void mm_modules$readprojectfile(u8 *filename) {
        i64 fileno;
        i64 headerdir;
        i64 dir;
        i64 oldsyslib;
        i64 found;
        u8 *  basefile;
        u8 *  extension;
    extension = mlib$convlcstring(mlib$extractext(filename,(i64)0));
    found = mlib$checkfile(filename);
    if ((!(!!(found)) && !(!!(mlib$eqstring(extension,(byte*)"ma"))))) {
        filename = mlib$pcm_copyheapstring(mlib$changeext(filename,(byte*)"ma"));
        found = mlib$checkfile(filename);
        if (!!(found)) {
            msysc$m_print_startcon();
            msysc$m_print_setfmt((byte*)"(Building #)");
            msysc$m_print_str(filename,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            extension = (byte*)"ma";
        }
;
    }
;
    if (!(!!(found))) {
        mm_support$loaderror((byte*)"Can't find main module or project: ##",filename,(byte*)"");
    }
;
    if (!!(mlib$eqstring(extension,(byte*)"ma"))) {
        filename = mm_modules$loadmafile(filename,0);
    }
;
    fileno = mm_support$getsupportfile(filename,(byte*)"",(byte*)"",(i64)0,(i64)0);
    basefile = mlib$extractbasefile(mm_decls$sourcefilenames[(fileno)]);
    mm_cli$projectmodule = mm_decls$sourcefilespecs[(fileno)];
    mm_modules$initheadervars();
    mm_decls$headermode = (i64)1;
    headerdir = (i64)0;
    mm_decls$moduletable[((i64)0)].name = (byte*)"PROGRAM";
    mm_decls$moduletable[((i64)0)].fileno = (i64)0;
    mm_decls$stprogram = mm_lib$createdupldef(0,(struct mm_decls$strec *)mm_lex$addnamestr((byte*)"$prog"),(i64)1);
    mm_decls$moduletable[((i64)0)].stmodule = mm_decls$stprogram;
    mm_modules$addfirstsubprogram(basefile,fileno);
    mm_lex$startlex(fileno);
    L427 :;
    while (1) {
        mm_lex$lex();
        mm_parse$skipsemi();
                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)153)) {
            headerdir = (i64)1;
            dir = (i64)mm_decls$lx.subcode;
            mm_modules$dirpos = (i64)mm_decls$lx.pos;
            mm_lex$lex();
            if ((dir==(i64)1)) {
                mm_modules$readmoduledir();
                mm_modules$headervars[((i64)8)-1] = (byte*)"";
            }
            else if ((dir==(i64)4)) {
                oldsyslib = mm_modules$issyslib;
                mm_modules$issyslib = (i64)1;
                mm_modules$readmoduledir();
                mm_modules$issyslib = oldsyslib;
                mm_modules$headervars[((i64)8)-1] = (byte*)"";
            }
            else if ((dir==(i64)3)) {
                mm_modules$altpathx = (byte*)"";
                mm_modules$issyslib = (i64)0;
                mm_modules$readsubprogram();
            }
            else if ((dir==(i64)6)) {
                if (((i64)(u64)(*mm_modules$importpathx) == (i64)0)) {
                    mm_modules$importpathx = mm_modules$headervars[((i64)1)-1];
                }
;
                mm_modules$issyslib = (i64)1;
                mm_modules$readsubprogram();
            }
            else if ((dir==(i64)2)) {
                if ((((i64)mm_decls$lx.symbol == (i64)81) && !!(mlib$eqstring((*mm_decls$lx.symptr).name,(byte*)"mlib")))) {
                    goto L429 ;
;
                }
;
                mm_modules$issyslib = (i64)0;
                mm_modules$altpathx = (byte*)"";
                mm_modules$readimport();
            }
            else if ((dir==(i64)7)) {
                mm_modules$readinclude();
            }
            else if ((dir==(i64)5)) {
                //$hdr_sysimport:
L429 :;
;
                if (((i64)(u64)(*mm_modules$importpathx) == (i64)0)) {
                    mm_modules$importpathx = mm_modules$headervars[((i64)1)-1];
                }
;
                mm_modules$issyslib = (i64)1;
                mm_modules$altpathx = (byte*)"";
                mm_modules$readimport();
            }
            else if ((dir==(i64)8)) {
                mm_modules$altpathx = mm_modules$fixpath(mm_modules$readvar());
            }
            else if ((dir==(i64)9)) {
                mm_modules$importpathx = mm_modules$fixpath(mm_modules$readvar());
                mm_modules$subprogpath = (!!((u64)(*mm_modules$importpathx)) ? mm_modules$importpathx : mm_modules$headerpathx);
            }
            else if ((dir==(i64)15)) {
                mm_modules$dosetvar();
            }
            else if ((dir==(i64)16)) {
                mm_modules$doshowvar();
            }
            else if ((dir==(i64)10)) {
                mm_modules$addlib(mm_modules$readvar(),(i64)68);
            }
            else if ((dir==(i64)11)) {
                mm_modules$addlib(mm_modules$readvar(),(i64)76);
            }
            else {
                mm_support$loaderror((byte*)"Hdr directive not ready:##",mm_tables$headerdirnames[(dir)-1],(byte*)"");
            }
;
            mm_parse$checksymbol((i64)6);
        }
        else if (($temp==(i64)6)) {
        }
        else if (($temp==(i64)68)) {
            goto L428 ;
        }
        else {
            if ((!!(mm_lex$sourcelevel) && !!(mm_lex$lximport))) {
                mm_modules$setmixedimport();
                mm_lex$unstacksource();
            }
            else {
                mm_cli$projectmodule = 0;
                mm_modules$setmixedprogram(basefile);
                goto L428 ;
            }
;
        }
        };
    }
L428 :;
    ;
    if ((mm_decls$nmodules == (i64)0)) {
        mm_support$loaderror((byte*)"No modules specified",(byte*)"",(byte*)"");
    }
;
    mm_modules$addsyslib();
    mm_modules$addlib((byte*)"msvcrt",(i64)68);
    mm_modules$addlib((byte*)"user32",(i64)68);
    mm_modules$addlib((byte*)"gdi32",(i64)68);
    mm_modules$addlib((byte*)"kernel32",(i64)68);
}

static void mm_modules$initheadervars(void) {
        i64 i;
    for (i=(i64)1;i<=(i64)11;++i) {
L430 :;
        mm_modules$headervars[(i)-1] = (byte*)"";
L431 :;
    }
L432 :;
    ;
    mm_modules$headervars[((i64)1)-1] = (byte*)"C:/mx/";
    mm_modules$headervars[((i64)2)-1] = mlib$pcm_copyheapstring(mlib$extractpath(msysc$sysparams[((i64)1)-1]));
    mm_modules$subprogpath = (mm_modules$headerpathx = (mm_modules$headervars[((i64)3)-1] = mlib$pcm_copyheapstring(mm_decls$sourcefilepaths[((i64)1)])));
    if (!!((i64)mm_decls$fwindows)) {
        mm_modules$headervars[((i64)5)-1] = (byte*)"1";
    }
;
    mm_modules$headervars[((i64)8)-1] = (byte*)"1";
    if ((u64)1u) {
        mm_modules$headervars[((i64)4)-1] = (byte*)"1";
    }
;
}

static void mm_modules$readmoduledir(void) {
        u8 *  modulename;
        u8 *  modulefilespec;
        struct mm_decls$strec *  stalias;
    mm_parse$checksymbol((i64)81);
    modulename = (modulefilespec = mlib$pcm_copyheapstring((*mm_decls$lx.symptr).name));
    mlib$convlcstring(modulename);
    stalias = 0;
    mm_lex$lex();
    if ((((i64)mm_decls$lx.symbol == (i64)81) && !!(mlib$eqstring((*mm_decls$lx.symptr).name,(byte*)"as")))) {
        mm_lex$lex();
        if (((i64)mm_decls$lx.symbol == (i64)81)) {
            stalias = (struct mm_decls$strec *)mm_decls$lx.symptr;
            mm_lex$lex();
        }
        else {
            stalias = (struct mm_decls$strec *)mm_lex$addnamestr(mm_modules$readvar());
        }
;
    }
;
    if (!!(mm_modules$checkwhen())) {
        mm_modules$addmodule(modulename,stalias);
    }
;
}

static i64 mm_modules$checkwhen(void) {
        i64 index;
    if (((i64)mm_decls$lx.symbol != (i64)110)) {
        return (i64)1;
    }
;
    mm_lex$lex();
    mm_parse$checksymbol((i64)154);
    index = (i64)mm_decls$lx.subcode;
    mm_lex$lex();
    return mlib$eqstring(mm_modules$headervars[(index)-1],(byte*)"1");
}

static void mm_modules$addmodule(u8 *modulename,struct mm_decls$strec *stalias) {
        struct mm_decls$modulerec *  pm;
        struct mm_decls$subprogrec *  ps;
        i64 i;
    for (i=(i64)1;i<=mm_decls$nmodules;++i) {
L433 :;
        if (!!(mlib$eqstring(mm_decls$moduletable[(i)].name,modulename))) {
            mm_support$loaderror((byte*)"Duplicate module name: # (Line:#)",modulename,msysc$strint(mm_support$getlineno((u64)mm_modules$dirpos),0));
        }
;
L434 :;
    }
L435 :;
    ;
    for (i=(i64)1;i<=mm_decls$nsubprogs;++i) {
L436 :;
        if (!!(mlib$eqstring(mm_decls$subprogtable[(i)].name,modulename))) {
            mm_support$loaderror((byte*)"Clashing subprog/module name: # (Line:#)",modulename,msysc$strint(mm_support$getlineno((u64)mm_modules$dirpos),0));
        }
;
L437 :;
    }
L438 :;
    ;
    if ((mm_decls$nmodules >= (i64)200)) {
        mm_support$loaderror((byte*)"Too many modules",modulename,(byte*)"");
    }
;
    pm = (struct mm_decls$modulerec *)&mm_decls$moduletable[(++(mm_decls$nmodules))];
    (*pm).name = mlib$pcm_copyheapstring(modulename);
    (*pm).subprogno = mm_decls$nsubprogs;
    (*pm).stmodule = (mm_decls$stmodule = mm_lib$createnewmoduledef(mm_decls$stprogram,(struct mm_decls$strec *)mm_lex$addnamestr(modulename),(i64)3));
    (*pm).path = (!!((u64)(*mm_modules$altpathx)) ? mm_modules$altpathx : mm_modules$subprogpath);
    (*pm).issyslib = mm_modules$issyslib;
    (*mm_decls$stmodule).moduleno = mm_decls$nmodules;
    (*mm_decls$stmodule).subprogno = mm_decls$nsubprogs;
    mm_decls$moduletosub[(mm_decls$nmodules)] = mm_decls$nsubprogs;
    ps = (struct mm_decls$subprogrec *)&mm_decls$subprogtable[(mm_decls$nsubprogs)];
    if (((i64)(*ps).firstmodule == (i64)0)) {
        (*ps).firstmodule = mm_decls$nmodules;
    }
;
    if (!!(stalias)) {
        (*pm).stmacro = mm_lib$getduplnameptr(mm_decls$stprogram,stalias,(i64)18);
        mm_lib$adddef(mm_decls$stprogram,(*pm).stmacro);
        (*(*pm).stmacro).paramlist = 0;
        (*(*pm).stmacro).code = (struct mm_decls$unitrec *)mm_lib$createname(mm_decls$stmodule);
    }
;
}

static void mm_modules$addsubprogram(u8 *subprogname,i64 fileno) {
        struct mm_decls$subprogrec *  ps;
        i64 i;
    if ((mm_decls$nsubprogs >= (i64)30)) {
        mm_support$loaderror((byte*)"Too many subprograms",subprogname,(byte*)"");
    }
;
    for (i=(i64)1;i<=mm_decls$nsubprogs;++i) {
L439 :;
        if (!!(mlib$eqstring(mm_decls$subprogtable[(i)].name,subprogname))) {
            mm_support$loaderror((byte*)"Duplicate subprog name: # (Line:#)",subprogname,msysc$strint(mm_support$getlineno((u64)mm_modules$dirpos),0));
        }
;
L440 :;
    }
L441 :;
    ;
    ps = (struct mm_decls$subprogrec *)&mm_decls$subprogtable[(++(mm_decls$nsubprogs))];
    (*ps).name = mlib$pcm_copyheapstring(subprogname);
    mm_modules$subprogpath = ((*ps).path = (!!((u64)(*mm_modules$importpathx)) ? mm_modules$importpathx : mm_modules$subprogpath));
    mm_decls$stsubprog = mm_lib$createnewmoduledef(mm_decls$stprogram,(struct mm_decls$strec *)mm_lex$addnamestr(subprogname),(i64)2);
    (*mm_decls$stsubprog).subprogno = mm_decls$nsubprogs;
    (*ps).stsubprog = mm_decls$stsubprog;
    (*ps).fileno = fileno;
    (*ps).issyslib = mm_modules$issyslib;
}

static void mm_modules$addfirstsubprogram(u8 *progname,i64 fileno) {
        struct mm_decls$subprogrec *  ps;
    mm_decls$nsubprogs = (i64)1;
    ps = (struct mm_decls$subprogrec *)&mm_decls$subprogtable[((i64)1)];
    (*ps).name = mlib$pcm_copyheapstring(progname);
    (*ps).path = mm_modules$headerpathx;
    mm_decls$stsubprog = mm_lib$createnewmoduledef(mm_decls$stprogram,(struct mm_decls$strec *)mm_lex$addnamestr(progname),(i64)2);
    (*mm_decls$stsubprog).subprogno = (i64)1;
    (*ps).stsubprog = mm_decls$stsubprog;
    (*ps).fileno = fileno;
    mm_decls$mainmoduleno = (i64)1;
}

static void mm_modules$readsubprogram(void) {
        u8 *  subprogname;
        u8 *  subprogfilespec;
    mm_parse$checksymbol((i64)81);
    subprogname = (subprogfilespec = mlib$pcm_copyheapstring((*mm_decls$lx.symptr).name));
    mlib$convlcstring(subprogname);
    mm_lex$lex();
    if (((i64)mm_decls$lx.symbol == (i64)110)) {
        mm_lex$lex();
        mm_lex$lex();
    }
;
    mm_modules$addsubprogram(subprogname,(i64)0);
}

static void mm_modules$readimport(void) {
        u8 *  subprogname;
        u8 *  path;
        i64 fileno;
    mm_parse$checksymbol((i64)81);
    subprogname = mlib$pcm_copyheapstring((*mm_decls$lx.symptr).name);
    mlib$convlcstring(subprogname);
    mm_lex$lex();
    path = (!!((u64)(*mm_modules$importpathx)) ? mm_modules$importpathx : mm_modules$subprogpath);
    fileno = mm_support$getsupportfile(subprogname,(byte*)"m",path,(i64)0,(i64)0);
    mm_modules$addsubprogram(subprogname,fileno);
    mm_lex$stacksource(fileno,(i64)0);
}

static void mm_modules$readinclude(void) {
        u8 *  name;
        i64 fileno;
    mm_parse$checksymbol((i64)77);
    name = mlib$pcm_copyheapstring(mm_decls$lx.svalue);
    mm_lex$lex();
    fileno = mm_support$getsupportfile(name,(byte*)"m",(byte*)"",(i64)0,(i64)0);
    mm_lex$stacksource(fileno,(i64)0);
}

static u8 *mm_modules$readvar(void) {
        u8 *  s;
        {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)77)) {
        s = mlib$pcm_copyheapstring(mm_decls$lx.svalue);
    }
    else if (($temp==(i64)154)) {
        s = mm_modules$headervars[((i64)mm_decls$lx.subcode)-1];
    }
    else if (($temp==(i64)81)) {
        s = (*mm_decls$lx.symptr).name;
    }
    else {
        mm_support$loaderror((byte*)"readvar/bad expr",(byte*)"",(byte*)"");
        s = (byte*)"?";
    }
    };
    mm_lex$lex();
    return s;
}

static u8 *mm_modules$fixpath(u8 *path) {
        u8 newpath[300];
        i64 n;
    n = strlen(path);
    if ((n == (i64)0)) {
        return path;
    }
;
    if (((u64)(*((path + n) - (i64)1)) == (u64)92u || (u64)(*((path + n) - (i64)1)) == '/')) {
        return path;
    }
;
    strcpy(newpath,path);
    strcat(newpath,(byte*)"\\");
    return mlib$pcm_copyheapstring(newpath);
}

static void mm_modules$dosetvar(void) {
        i64 index;
    mm_parse$checksymbol((i64)154);
    index = (i64)mm_decls$lx.subcode;
    mm_lex$lex();
    mm_parse$checksymbol((i64)48);
    mm_lex$lex();
    mm_modules$headervars[(index)-1] = mm_modules$readvar();
}

static void mm_modules$doshowvar(void) {
    if (((i64)mm_decls$lx.symbol == (i64)77)) {
        msysc$m_print_startcon();
        msysc$m_print_str(mm_decls$lx.svalue,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
    else {
        mm_parse$checksymbol((i64)154);
        msysc$m_print_startcon();
        msysc$m_print_str((mm_tables$headervarnames[((i64)mm_decls$lx.subcode)-1] + (i64)3),NULL);
        msysc$m_print_str((byte*)"=",NULL);
        msysc$m_print_str(mm_modules$headervars[((i64)mm_decls$lx.subcode)-1],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    mm_lex$lex();
}

static void mm_modules$setmixedprogram(u8 *basefile) {
        u8 name[100];
        i64 oldns;
    msysc$m_print_startstr(name);
    msysc$m_print_str((byte*)"$",NULL);
    msysc$m_print_nogap();
    msysc$m_print_str(basefile,NULL);
    msysc$m_print_end();
    ;
    oldns = mm_decls$nsubprogs;
    mm_decls$nsubprogs = (i64)1;
    mm_modules$addmodule(name,0);
    mm_decls$nsubprogs = oldns;
    mm_decls$moduletable[(mm_decls$nmodules)].fileno = (i64)1;
    mm_decls$mainmoduleno = (mm_decls$subprogtable[((i64)1)].firstmodule = mm_decls$nmodules);
}

static void mm_modules$setmixedimport(void) {
        u8 name[100];
    msysc$m_print_startstr(name);
    msysc$m_print_str((byte*)"$",NULL);
    msysc$m_print_nogap();
    msysc$m_print_str(mm_decls$subprogtable[(mm_decls$nsubprogs)].name,NULL);
    msysc$m_print_end();
    ;
    mm_modules$addmodule(name,0);
    mm_decls$moduletable[(mm_decls$nmodules)].fileno = mm_decls$subprogtable[(mm_decls$nsubprogs)].fileno;
    mm_decls$subprogtable[(mm_decls$nsubprogs)].firstmodule = mm_decls$nmodules;
}

void mm_modules$loadmodules(void) {
        struct mm_decls$modulerec *  pm;
        i64 i;
    for (i=(i64)1;i<=mm_decls$nmodules;++i) {
L442 :;
        pm = (struct mm_decls$modulerec *)&mm_decls$moduletable[(i)];
        mm_modules$loadmodule((struct mm_decls$modulerec *)pm);
L443 :;
    }
L444 :;
    ;
}

static void mm_modules$loadmodule(struct mm_decls$modulerec *pm) {
        u8 *  path;
    if (!!((i64)(*pm).fileno)) {
        return;
    }
;
    path = (*pm).path;
    if ((((i64)(u64)(*path) == (i64)0) && !!((i64)(*pm).issyslib))) {
        path = (byte*)"c:\\mx\\";
    }
;
    (*pm).fileno = mm_support$getsupportfile((*pm).name,(byte*)"m",path,(i64)(*pm).issyslib,(i64)0);
}

static void mm_modules$addsyslib(void) {
        i64 i;
    if (((i64)mm_decls$msyslevel == (i64)0)) {
        return;
    }
;
    for (i=(i64)1;i<=mm_decls$nsubprogs;++i) {
L445 :;
        if (!!(mlib$eqstring(mm_decls$subprogtable[(i)].name,(byte*)"mlibx"))) {
            return;
        }
;
L446 :;
    }
L447 :;
    ;
    mm_modules$issyslib = (i64)1;
    mm_modules$importpathx = mm_modules$headervars[((i64)1)-1];
    mm_modules$altpathx = (byte*)"";
    if (((i64)mm_decls$msyslevel == (i64)1)) {
        mm_modules$addsubprogram((byte*)"mlibmin",(i64)0);
        mm_modules$addmodule((byte*)"mminc",0);
        return;
    }
;
    mm_modules$addsubprogram((byte*)"mlibx",(i64)0);
    if ((u64)1u) {
        mm_modules$addmodule((byte*)"msysc",0);
        mm_modules$addmodule((byte*)"mlib",0);
        mm_modules$addmodule((byte*)"mclib",0);
        if (!!((i64)mm_decls$flinux)) {
            mm_modules$addmodule((byte*)"mlinux",0);
            mm_modules$addmodule((byte*)"mwindllc",0);
        }
        else {
            mm_modules$addmodule((byte*)"mwindows",0);
            mm_modules$addmodule((byte*)"mwindllc",0);
        }
;
    }
    else {
        mm_modules$addmodule((byte*)"msys",0);
        mm_modules$addmodule((byte*)"mlib",0);
        mm_modules$addmodule((byte*)"mclib",0);
        mm_modules$addmodule((byte*)"mwindows",0);
        mm_modules$addmodule((byte*)"mwindll",0);
    }
;
}

void mm_modules$addlib(u8 *libname,i64 libtype) {
        i64 i;
    for (i=(i64)1;i<=mm_decls$nlibfiles;++i) {
L448 :;
        if (!!(mlib$eqstring(mm_decls$libfiles[(i)],libname))) {
            return;
        }
;
L449 :;
    }
L450 :;
    ;
    if ((mm_decls$nlibfiles >= (i64)50)) {
        mm_support$loaderror((byte*)"Too many libs",(byte*)"",(byte*)"");
    }
;
    mm_decls$libfiles[(++(mm_decls$nlibfiles))] = libname;
    mm_decls$libtypes[(mm_decls$nlibfiles)] = libtype;
}

static u8 *mm_modules$readfileline(u8 *s) {
        u8 str[2048];
        u8 *  t;
        i64 n;
        i64 c;
    t = str;
    n = (i64)0;
    L451 :;
        {i64 $temp = (c = (i64)(u64)(*(s)++));
if (($temp==(i64)0)) {
        --(s);
        goto L452 ;
    }
    else if (($temp==(i64)10)) {
        goto L452 ;
    }
    else {
        if ((n < (i64)2048)) {
            (*(t)++) = (u64)c;
        }
;
    }
    }goto L451 ;
L452 :;
    ;
    (*t) = (u64)0u;
    msysc$m_read_strline(str);
    ;
    return s;
}

static u8 *mm_modules$findnextlineheader(u8 *s) {
        i64 c;
    L453 :;
        {i64 $temp = (c = (i64)(u64)(*(s)++));
if (($temp==(i64)0)) {
        return 0;
    }
    else if (($temp==(i64)10)) {
        if (((((u64)(*s) == '=') && ((u64)(*(s + (i64)1)) == '=')) && ((u64)(*(s + (i64)2)) == '='))) {
            return (s + (i64)3);
        }
;
    }
    }goto L453 ;
L454 :;
    ;
    return (u8 *)0;
}

static u8 *mm_modules$loadmafile(u8 *filespec,u8 *builtinstr) {
        u8 *  s;
        u8 *  t;
        u8 name[100];
        u8 newfilespec[300];
        i64 sys;
        i64 support;
        i64 i;
    mm_decls$freadma = (i64)1;
    if (!!(filespec)) {
        s = (u8 *)mlib$readfile(filespec);
        if ((s == 0)) {
            mm_support$loaderror((byte*)"Can't find MA file ##",filespec,(byte*)"");
        }
;
        strcpy(newfilespec,mlib$extractpath(filespec));
    }
    else {
        s = builtinstr;
        newfilespec[((i64)1)-1] = (u64)0u;
    }
;
    s = mm_modules$readfileline((s + (i64)3));
    msysc$readstr(name,(i64)110,(i64)0);
    if (!(!!(mlib$eqstring(name,(byte*)"ma")))) {
        mm_support$loaderror((byte*)"MA: bad header",(byte*)"",(byte*)"");
    }
;
    --(s);
    if (!!(mm_decls$nsourcefiles)) {
        mm_support$loaderror((byte*)"MA/table not empty",(byte*)"",(byte*)"");
    }
;
    s = mm_modules$findnextlineheader(s);
    L455 :;
    while (1) {
        if ((s == 0)) {
            mm_support$loaderror((byte*)"Unexpected EOF in MA file",(byte*)"",(byte*)"");
            goto L456 ;
        }
;
        s = mm_modules$readfileline(s);
        msysc$readstr(name,(i64)110,(i64)0);
        {
            sys = msysc$m_read_i64((i64)0);
            ;
            support = msysc$m_read_i64((i64)0);
            ;
        }
        if (!!(mlib$eqstring(name,(byte*)"end"))) {
            goto L456 ;
        }
;
        if ((mm_decls$nsourcefiles >= (i64)1000)) {
            mm_support$loaderror((byte*)"Too many files in MA",(byte*)"",(byte*)"");
        }
;
        t = mm_modules$findnextlineheader(s);
        if ((t == 0)) {
            mm_support$loaderror((byte*)"MA error",(byte*)"",(byte*)"");
        }
;
        ++(mm_decls$nsourcefiles);
        mm_decls$sourcefilenames[(mm_decls$nsourcefiles)] = (mm_decls$sourcefilespecs[(mm_decls$nsourcefiles)] = mlib$pcm_copyheapstring(name));
        mm_decls$sourcefilesizes[(mm_decls$nsourcefiles)] = ((t - s) - (i64)3);
        mm_decls$sourcefiletext[(mm_decls$nsourcefiles)] = s;
        mm_decls$sourcefilepaths[(mm_decls$nsourcefiles)] = (byte*)"";
        mm_decls$sourcefilespecs[(mm_decls$nsourcefiles)] = (byte*)"";
        mm_decls$sourcefilesys[(mm_decls$nsourcefiles)] = sys;
        mm_decls$sourcefilesupport[(mm_decls$nsourcefiles)] = support;
        s = t;
    }
L456 :;
    ;
    for (i=(i64)1;i<=mm_decls$nsourcefiles;++i) {
L457 :;
        (*(mm_decls$sourcefiletext[(i)] + mm_decls$sourcefilesizes[(i)])) = (u64)0u;
L458 :;
    }
L459 :;
    ;
    strcat(newfilespec,mm_decls$sourcefilenames[((i64)1)]);
    return mlib$pcm_copyheapstring(newfilespec);
}

// START
void mm_modules$start(void) {

}

void mm_name$rx_typetable(void) {
        struct mm_decls$strec *  d;
        i64 i;
    for (i=(i64)29;i<=mm_decls$ntypes;++i) {
L460 :;
        if (((i64)mm_decls$ttbasetype[(i)] == (i64)8)) {
            d = mm_decls$ttnamedef[(i)];
            if (!!((i64)(*d).baseclass)) {
                mm_name$do_baseclass(d);
            }
;
        }
;
L461 :;
    }
L462 :;
    ;
}

void mm_name$rx_unit(struct mm_decls$strec *owner,struct mm_decls$unitrec *p) {
        struct mm_decls$strec *  d;
        struct mm_decls$unitrec *  a;
        struct mm_decls$unitrec *  b;
        i64 n;
        i64 oldnoexpand;
        i64 oldnoassem;
        i64 oldtag;
        i64 useparams;
        i64 $av_1;
        i64 i;
    a = (*p).a;
    b = (*p).b;
    mm_tables$mlineno = (i64)(*p).pos;
    switch ((i64)(*p).tag) {
    case 3:;
        {
            mm_name$resolvename(owner,p);
            if (((((i64)(*p).tag == (i64)3) && ((i64)(*(*p).def).nameid == (i64)18)) && !(!!(mm_name$noexpand)))) {
                ++(mm_name$macrolevels);
                mm_name$expandmacro(p,p,0);
                mm_name$rx_unit(owner,p);
                --(mm_name$macrolevels);
            }
;
        }
        break;
    case 21:;
        {
            mm_name$rx_unit(owner,b);
        }
        break;
    case 42:;
        {
            mm_name$resolvedot(owner,p);
        }
        break;
    case 86:;
    case 28:;
        {
            oldtag = (i64)(*p).tag;
            if (((i64)(*a).tag == (i64)3)) {
                oldnoexpand = mm_name$noexpand;
                mm_name$noexpand = (i64)1;
                mm_name$rx_unit(owner,a);
                mm_name$noexpand = oldnoexpand;
            }
            else {
                mm_name$rx_unit(owner,a);
            }
;
            mm_name$rx_unitlist(owner,b);
            if (((i64)(*a).tag == (i64)3)) {
                d = (*a).def;
                                {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)5)) {
                    (*p).tag = (i64)48;
                    mm_lib$storemode(owner,(i64)(*d).mode,&(*p).convmode);
                    (*p).a = b;
                    if (!!((*b).nextunit)) {
                        (*p).a = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)15,(struct mm_decls$unitrec *)b);
                        n = (i64)0;
                        L463 :;
                        while (!!(b)) {
                            ++(n);
                            b = (*b).nextunit;
L464 :;
                        }
L465 :;
                        ;
                        (*(*p).a).length = n;
                    }
;
                }
                else if (($temp==(i64)18)) {
                    ++(mm_name$macrolevels);
                    if (!!((*d).deflist)) {
                        mm_name$expandmacro(p,a,b);
                        b = 0;
                        useparams = (i64)0;
                    }
                    else {
                        mm_name$expandmacro(p,a,0);
                        useparams = (i64)1;
                    }
;
                    mm_name$rx_unit(owner,p);
                    --(mm_name$macrolevels);
                    if ((!!(useparams) && !(((i64)(*p).tag == (i64)86 || (i64)(*p).tag == (i64)28)))) {
                        mm_lib$insertunit(p,oldtag);
                        (*p).b = b;
                    }
;
                }
                else {
                    if (((i64)(*d).mode == (i64)0)) {
                        (*p).tag = (i64)86;
                    }
;
                }
                };
            }
;
        }
        break;
    case 11:;
    case 12:;
        {
            mm_name$rx_unit(owner,a);
            mm_name$rx_unit(owner,b);
            if (!(!!((i64)mm_tables$isbooltag[((i64)(*a).tag)]))) {
                mm_lib$insertunit(a,(i64)14);
                (*a).pclop = (i64)33;
            }
;
            if (!(!!((i64)mm_tables$isbooltag[((i64)(*b).tag)]))) {
                mm_lib$insertunit(b,(i64)14);
                (*b).pclop = (i64)33;
            }
;
        }
        break;
    case 14:;
        {
            //doistruel:
L466 :;
;
            mm_name$rx_unit(owner,a);
            if (!!((i64)mm_tables$isbooltag[((i64)(*a).tag)])) {
                mm_lib$deleteunit(p,a);
            }
;
            goto L467 ;
;
        }
        break;
    case 13:;
        {
            mm_name$rx_unit(owner,a);
            if (((i64)(*a).tag == (i64)13)) {
                mm_lib$deleteunit(p,a);
                (*p).tag = (i64)14;
                (*p).pclop = (i64)33;
                a = (*p).a;
                goto L466 ;
;
            }
;
            if (!(!!((i64)mm_tables$isbooltag[((i64)(*a).tag)]))) {
                mm_lib$insertunit(a,(i64)14);
                (*a).pclop = (i64)33;
                a = (*p).a;
            }
;
            goto L467 ;
;
        }
        break;
    case 6:;
        {
            mm_name$resolvename(owner,a);
            if (!(!!(mm_name$noexpand))) {
                ++(mm_name$macrolevels);
                oldnoassem = mm_name$noassem;
                mm_name$noassem = (i64)1;
                mm_name$expandmacro(p,a,b);
                mm_name$noassem = oldnoassem;
                mm_name$rx_unit(owner,p);
                --(mm_name$macrolevels);
            }
;
        }
        break;
    default: {
        //doabc:
L467 :;
;
                ($av_1 = (i64)mm_tables$jsubs[((i64)(*p).tag)]);
        for (i=(i64)1;i<=$av_1;++i) {
L468 :;
            mm_name$rx_unitlist(owner,(*p).abc[(i)-1]);
L469 :;
        }
L470 :;
        ;
    }
    } //SW
;
}

i64 mm_name$rx_module(i64 n) {
    mm_decls$currmoduleno = n;
    mm_name$rx_passdef(mm_decls$stprogram,mm_decls$moduletable[(n)].stmodule);
    return (i64)1;
}

void mm_name$rx_deflist(struct mm_decls$strec *owner,struct mm_decls$strec *p) {
        struct mm_decls$strec *  pstart;
    pstart = p;
    L471 :;
    while (!!(p)) {
        mm_name$rx_passdef(owner,p);
        p = (struct mm_decls$strec *)(*p).nextdef;
L472 :;
    }
L473 :;
    ;
}

void mm_name$rx_passdef(struct mm_decls$strec *owner,struct mm_decls$strec *p) {
        {i64 $temp = (i64)(*p).nameid;
if (($temp==(i64)3) || ($temp==(i64)4)) {
        mm_name$rx_deflist(p,(struct mm_decls$strec *)(*p).deflist);
    }
    else if (($temp==(i64)6)) {
        mm_name$rx_deflist(p,(struct mm_decls$strec *)(*p).deflist);
        mm_name$currstproc = p;
        mm_name$rx_unit(p,(*p).code);
        mm_name$currstproc = 0;
    }
    else if (($temp==(i64)7)) {
        mm_name$rx_deflist(p,(struct mm_decls$strec *)(*p).deflist);
    }
    else if (($temp==(i64)10) || ($temp==(i64)11) || ($temp==(i64)12) || ($temp==(i64)13)) {
        if (!!(msysc$m_getdotindex((i64)(*p).flags,(i64)10))) {
            mm_name$rx_unit(owner,(*p).equivvar);
        }
;
        if (!!((*p).code)) {
            mm_name$rx_unit(owner,(*p).code);
        }
;
    }
    else if (($temp==(i64)5)) {
        mm_name$rx_deflist(p,(struct mm_decls$strec *)(*p).deflist);
    }
    else {
    }
    };
}

static void mm_name$rx_unitlist(struct mm_decls$strec *owner,struct mm_decls$unitrec *p) {
    L474 :;
    while (!!(p)) {
        mm_name$rx_unit(owner,p);
        p = (*p).nextunit;
L475 :;
    }
L476 :;
    ;
}

struct mm_decls$strec *mm_name$resolvetopname(struct mm_decls$strec *owner,struct mm_decls$strec *stnewname,i64 moduleno,i64 allowmod) {
        i64 extcount;
        i64 subprogno;
        struct mm_decls$strec *  p;
        struct mm_decls$strec *  q;
        struct mm_decls$strec *  powner;
        struct mm_decls$strec *  extdef;
        struct mm_decls$strec *  moddef;
        struct mm_decls$strec *  ambiglist[10];
        i64 i;
    if (((i64)(*owner).nameid == (i64)6)) {
        q = (struct mm_decls$strec *)(*owner).deflist;
        L477 :;
        while (!!(q)) {
            if (((*q).firstdupl == stnewname)) {
                return q;
            }
;
L478 :;
            q = (struct mm_decls$strec *)(*q).nextdef;
L480 :;
                    }
L479 :;
        ;
    }
;
    p = (struct mm_decls$strec *)(*stnewname).nextdupl;
    subprogno = (i64)mm_decls$moduletosub[(moduleno)];
    extcount = (i64)0;
    extdef = (moddef = 0);
    L481 :;
    while (!!(p)) {
        powner = (struct mm_decls$strec *)(*p).owner;
        switch ((i64)(*powner).nameid) {
        case 3:;
            {
                if (((i64)(*powner).moduleno == moduleno)) {
                    return p;
                }
                else if (!!((i64)(*p).scope)) {
                    if (((((i64)(*powner).subprogno == subprogno) || ((i64)(*p).scope == (i64)2)) || !!(msysc$m_getdotindex((i64)(*p).flags,(i64)12)))) {
                        ++(extcount);
                        extdef = p;
                        if ((extcount < (i64)10)) {
                            ambiglist[(extcount)-1] = extdef;
                        }
;
                    }
;
                }
;
            }
            break;
        case 5:;
            {
                if (((powner == owner) || (powner == (*owner).owner))) {
                    return p;
                }
;
            }
            break;
        case 1:;
            {
                                {i64 $temp = (i64)(*p).nameid;
if (($temp==(i64)3) || ($temp==(i64)2)) {
                    moddef = p;
                }
                else if (($temp==(i64)18)) {
                    return p;
                }
                };
            }
            break;
        } //SW
;
L482 :;
        p = (struct mm_decls$strec *)(*p).nextdupl;
L484 :;
            }
L483 :;
    ;
    if ((!!(allowmod) && !!(moddef))) {
        return moddef;
    }
;
    if (!!(extdef)) {
        if ((extcount > (i64)1)) {
            for (i=(i64)1;i<=extcount;++i) {
L485 :;
                extdef = ambiglist[(i)-1];
                msysc$m_print_startcon();
                msysc$m_print_i64(i,NULL);
                msysc$m_print_str((*(*extdef).owner).name,NULL);
                msysc$m_print_str(mm_tables$namenames[((i64)(*(*extdef).owner).nameid)],NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
L486 :;
            }
L487 :;
            ;
            mm_support$rxerror_s((byte*)"Ambiguous ext name: #",(*extdef).name,0);
        }
;
        return extdef;
    }
;
    return (struct mm_decls$strec *)0;
}

void mm_name$resolvename(struct mm_decls$strec *owner,struct mm_decls$unitrec *p) {
        struct mm_decls$strec *  d;
        struct mm_decls$strec *  e;
        i64 moduleno;
        i64 mode;
        i64 islet;
        u8 str[300];
    d = (*p).def;
    moduleno = (i64)(*p).moduleno;
    if (((i64)(*d).nameid != (i64)0)) {
        return;
    }
;
    e = mm_name$resolvetopname(owner,d,moduleno,mm_name$allowmodname);
    if (!(!!(e))) {
        islet = (i64)0;
        mode = (i64)0;
                {i64 $temp = (i64)(*p).avcode;
if (($temp==(i64)73) || ($temp==(i64)84) || ($temp==(i64)83)) {
            mode = (i64)3;
            islet = (i64)1;
        }
        else if (($temp==(i64)76) || ($temp==(i64)65)) {
            mode = (i64)22;
        }
        };
        if ((mode == (i64)0)) {
            strcpy(str,(*d).name);
            mlib$convucstring(str);
            mm_support$rxerror_s((byte*)"pcl:Undefined: #",str,p);
        }
        else {
            e = mm_name$addframevar(owner,d,moduleno,mode);
            (*e).pos = (i64)(*p).pos;
            (*e).flags = msysc$m_setdotindex((*e).flags,(i64)4,(u64)islet);
        }
;
    }
;
    (*e).flags = msysc$m_setdotindex((*e).flags,(i64)1,(u64)1u);
    (*p).def = e;
}

struct mm_decls$strec *mm_name$finddupl(struct mm_decls$strec *d,struct mm_decls$strec *pdupl) {
    if (((i64)(*pdupl).nameid != (i64)0)) {
        return pdupl;
    }
;
    pdupl = (struct mm_decls$strec *)(*pdupl).nextdupl;
    L488 :;
    while (!!(pdupl)) {
        if (((*pdupl).owner == d)) {
            return pdupl;
        }
;
        pdupl = (struct mm_decls$strec *)(*pdupl).nextdupl;
L489 :;
    }
L490 :;
    ;
    return (struct mm_decls$strec *)0;
}

struct mm_decls$strec *mm_name$finddupl_sub(struct mm_decls$strec *d,struct mm_decls$strec *pdupl) {
        i64 subprogno;
    if (((i64)(*pdupl).nameid != (i64)0)) {
        return pdupl;
    }
;
    pdupl = (struct mm_decls$strec *)(*pdupl).nextdupl;
    subprogno = (i64)(*d).subprogno;
    L491 :;
    while (!!(pdupl)) {
        if (((i64)(*(*pdupl).owner).subprogno == subprogno)) {
            return pdupl;
        }
;
        pdupl = (struct mm_decls$strec *)(*pdupl).nextdupl;
L492 :;
    }
L493 :;
    ;
    return (struct mm_decls$strec *)0;
}

static void mm_name$resolvedot(struct mm_decls$strec *owner,struct mm_decls$unitrec *p) {
        struct mm_decls$unitrec *  lhs;
        struct mm_decls$unitrec *  rhs;
        struct mm_decls$strec *  d;
        struct mm_decls$strec *  e;
        struct mm_decls$strec *  t;
        i64 m;
        i64 moduleno;
        i64 subprogno;
        i64 oldallowmod;
    moduleno = (i64)(*p).moduleno;
    subprogno = (i64)(*p).subprogno;
    lhs = (*p).a;
    rhs = (*p).b;
    e = (*rhs).def;
    oldallowmod = mm_name$allowmodname;
    mm_name$allowmodname = (i64)((i64)(*lhs).tag == (i64)3);
    mm_name$rx_unit(owner,lhs);
    mm_name$allowmodname = oldallowmod;
        {i64 $temp = (i64)(*lhs).tag;
if (($temp==(i64)3)) {
        d = (*lhs).def;
                {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)3) || ($temp==(i64)5) || ($temp==(i64)6) || ($temp==(i64)5)) {
            e = mm_name$finddupl(d,e);
            if (!!(e)) {
                if (((i64)(*d).nameid == (i64)3)) {
                    if (((i64)(*e).subprogno != subprogno)) {
                        if (((i64)(*e).scope < (i64)2)) {
                            mm_support$rxerror_s((byte*)"Need export to import '#'",(*e).name,0);
                        }
;
                    }
                    else if (((i64)(*e).moduleno != moduleno)) {
                        if (!(!!((i64)(*e).scope))) {
                            mm_support$rxerror_s((byte*)"Need global to import '#'",(*e).name,0);
                        }
;
                    }
;
                }
;
                //domodule:
L494 :;
;
                (*p).tag = (i64)3;
                (*p).a = ((*p).b = 0);
                (*p).def = e;
                                {i64 $temp = (i64)(*e).nameid;
if (($temp==(i64)16)) {
                }
                else if (($temp==(i64)10)) {
                }
                };
            }
            else {
                mm_support$rxerror_s((byte*)"Can't resolve .#",(*(*(*p).b).def).name,p);
            }
;
        }
        else if (($temp==(i64)12) || ($temp==(i64)11) || ($temp==(i64)13)) {
            m = (i64)(*d).mode;
                        {i64 $temp = (i64)mm_decls$ttbasetype[(m)];
if (($temp==(i64)8)) {
            }
            else if (($temp==(i64)7)) {
                L495 :;
                while (1) {
                    m = (i64)mm_decls$tttarget[(m)];
                                        {i64 $temp = (i64)mm_decls$ttbasetype[(m)];
if (($temp==(i64)8)) {
                        goto L496 ;
                    }
                    else if (($temp==(i64)7)) {
                    }
                    else {
                        mm_support$rxerror((byte*)"2:Record expected",0);
                    }
                    };
                }
L496 :;
                ;
            }
            else {
                mm_support$rxerror((byte*)"Record expected",0);
            }
            };
            t = mm_decls$ttnamedef[(m)];
            e = mm_name$finddupl(t,e);
            if (!!(e)) {
                (*(*p).b).def = e;
            }
            else {
                mm_support$rxerror_s((byte*)"Not a field: #",(*(*rhs).def).name,0);
            }
;
        }
        else if (($temp==(i64)2)) {
            e = mm_name$finddupl_sub(d,e);
            if (!!(e)) {
                if (((i64)(*e).subprogno != subprogno)) {
                    if (((i64)(*e).scope < (i64)2)) {
                        mm_support$rxerror_s((byte*)"Need export to import '#'",(*e).name,0);
                    }
;
                }
;
                goto L494 ;
;
            }
            else {
                mm_support$rxerror_s((byte*)"Can't resolve sub.#",(*(*(*p).b).def).name,p);
            }
;
        }
        };
    }
    else {
        if (!(!!((*e).nextdupl))) {
            mm_support$rxerror_s((byte*)"Not a field: #",(*e).name,0);
        }
;
    }
    };
}

static void mm_name$fixmode(struct mm_decls$typenamerec *p) {
        i32 *  pmode;
        struct mm_decls$strec *  a;
        struct mm_decls$strec *  d;
        struct mm_decls$strec *  e;
        struct mm_decls$strec *  f;
        struct mm_decls$strec *  owner;
        i64 m;
        i64 moduleno;
    pmode = (*p).pmode;
    m = -((i64)(*pmode));
    d = (owner = (*p).owner);
    L497 :;
    while (((i64)(*d).nameid != (i64)3)) {
        d = (struct mm_decls$strec *)(*d).owner;
L498 :;
    }
L499 :;
    ;
    moduleno = (i64)(*d).moduleno;
    a = (*p).defa;
    d = (*p).defb;
    if (((a == 0) && !!(d))) {
        e = mm_name$resolvetopname(owner,d,moduleno,(i64)0);
    }
    else if (((d == 0) && !!(a))) {
        mm_support$rxerror((byte*)"Fixmode can't do typeof yet",0);
    }
    else {
        e = mm_name$resolvetopname(owner,a,moduleno,(i64)0);
        if (!!(e)) {
            f = (struct mm_decls$strec *)(*e).deflist;
            e = 0;
            L500 :;
            while (!!(f)) {
                if ((((i64)(*f).nameid == (i64)5) && ((*f).firstdupl == d))) {
                    e = f;
                    goto L502 ;
                }
;
                f = (struct mm_decls$strec *)(*f).nextdef;
L501 :;
            }
L502 :;
            ;
        }
;
    }
;
    if ((!!(e) && ((i64)(*e).nameid == (i64)5))) {
        (*pmode) = (i64)(*e).mode;
    }
    else {
        mm_support$rxerror_s((byte*)"2:Can't resolve tentative type: #",(*d).name,0);
    }
;
}

void mm_name$fixusertypes(void) {
        struct mm_decls$typenamerec *  p;
        i64 npasses;
        i64 notresolved;
        struct mm_decls$strec *  d;
        i64 i;
    npasses = (i64)0;
    L503 :;
    do {
        ++(npasses);
        notresolved = (i64)0;
        for (i=(i64)1;i<=mm_decls$ntypenames;++i) {
L506 :;
            p = (struct mm_decls$typenamerec *)&mm_decls$typenames[(i)];
            if (((i64)(*(*p).pmode) < (i64)0)) {
                mm_tables$mlineno = (i64)mm_decls$typenamepos[(i)].pos;
                mm_name$fixmode((struct mm_decls$typenamerec *)p);
                if (((i64)(*(*p).pmode) < (i64)0)) {
                    ++(notresolved);
                }
;
            }
;
L507 :;
        }
L508 :;
        ;
        if ((npasses > (i64)5)) {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"Type phase errors - check these user types:",NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            for (i=(i64)1;i<=mm_decls$ntypenames;++i) {
L509 :;
                p = (struct mm_decls$typenamerec *)&mm_decls$typenames[(i)];
                if (((i64)(*(*p).pmode) < (i64)0)) {
                    d = (*p).defb;
                    if ((d == 0)) {
                        d = (*p).defa;
                    }
;
                    msysc$m_print_startcon();
                    msysc$m_print_str((byte*)"\t",NULL);
                    msysc$m_print_str((*d).name,NULL);
                    msysc$m_print_newline();
                    msysc$m_print_end();
                    ;
                }
;
L510 :;
            }
L511 :;
            ;
            mm_support$rxerror((byte*)"Fixtypes: too many passes (cyclic ref?)",0);
        }
;
L504 :;
    }
    while (!(notresolved == (i64)0));
L505 :;
    ;
}

static struct mm_decls$strec *mm_name$addframevar(struct mm_decls$strec *owner,struct mm_decls$strec *d,i64 moduleno,i64 mode) {
        struct mm_decls$strec *  e;
    e = mm_lib$getduplnameptr(owner,d,(i64)12);
    mm_lib$storemode(owner,mode,&(*e).mode);
    mm_lib$adddef(owner,e);
    return e;
}

static struct mm_decls$unitrec *mm_name$copylistunit(struct mm_decls$unitrec *p) {
        struct mm_decls$unitrec *  q;
        struct mm_decls$unitrec *  plist;
        struct mm_decls$unitrec *  plistx;
    plist = (plistx = 0);
    L512 :;
    while (!!(p)) {
        q = mm_name$copyunit(p);
        mm_lib$addlistunit(&plist,&plistx,q);
        p = (*p).nextunit;
L513 :;
    }
L514 :;
    ;
    return plist;
}

static struct mm_decls$unitrec *mm_name$copyunit(struct mm_decls$unitrec *p) {
        struct mm_decls$unitrec *  q;
        struct mm_decls$strec *  d;
        i64 $av_1;
        i64 i;
    if ((p == 0)) {
        return 0;
    }
;
    if (((i64)(*p).tag == (i64)3)) {
        d = (*p).def;
        for (i=(i64)1;i<=mm_name$nmacroparams;++i) {
L515 :;
            if ((mm_name$macroparamsgen[(i)-1] == d)) {
                return mm_name$copyunit(mm_name$macroargs[(i)-1]);
                goto L517 ;
            }
;
L516 :;
        }
L517 :;
        ;
    }
;
    q = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)(*p).tag);
    (*q) = (*p);
    (*q).nextunit = 0;
        ($av_1 = (i64)mm_tables$jsubs[((i64)(*q).tag)]);
    for (i=(i64)1;i<=$av_1;++i) {
L518 :;
        (*q).abc[(i)-1] = mm_name$copylistunit((*q).abc[(i)-1]);
L519 :;
    }
L520 :;
    ;
    return q;
}

static void mm_name$replaceunit(struct mm_decls$unitrec *p,struct mm_decls$unitrec *q) {
        struct mm_decls$unitrec *  pnext;
    pnext = (*p).nextunit;
    (*p) = (*q);
    (*p).nextunit = pnext;
}

static void mm_name$expandmacro(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
        struct mm_decls$strec *  d;
        struct mm_decls$strec *  pm;
        struct mm_decls$unitrec *  pnew;
        i64 ignoreargs;
    if ((mm_name$macrolevels > (i64)10)) {
        mm_support$rxerror((byte*)"Too many macro levels (recursive macro?)",0);
    }
;
    d = (*a).def;
    pm = (struct mm_decls$strec *)(*d).paramlist;
    mm_name$nmacroparams = (i64)0;
    L521 :;
    while (!!(pm)) {
        if ((mm_name$nmacroparams >= (i64)50)) {
            mm_support$rxerror((byte*)"macro param overflow",0);
        }
;
        mm_name$macroparams[(++(mm_name$nmacroparams))-1] = pm;
        mm_name$macroparamsgen[(mm_name$nmacroparams)-1] = (struct mm_decls$strec *)(*pm).firstdupl;
        pm = (struct mm_decls$strec *)(*pm).nextparam;
L522 :;
    }
L523 :;
    ;
    mm_name$nmacroargs = (i64)0;
    L524 :;
    while (!!(b)) {
        if ((mm_name$nmacroargs >= (i64)50)) {
            mm_support$rxerror((byte*)"macro arg overflow",0);
        }
;
        mm_name$macroargs[(++(mm_name$nmacroargs))-1] = b;
        b = (*b).nextunit;
L525 :;
    }
L526 :;
    ;
    if ((mm_name$nmacroargs < mm_name$nmacroparams)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"NMACROARGS=",NULL);
        msysc$m_print_i64(mm_name$nmacroargs,NULL);
        msysc$m_print_i64(mm_name$nmacroparams,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        mm_support$rxerror((byte*)"Too few macro args",0);
    }
;
    ignoreargs = (i64)0;
    if (((mm_name$nmacroargs > (i64)0) && (mm_name$nmacroparams == (i64)0))) {
        ignoreargs = (i64)1;
        mm_name$nmacroargs = (mm_name$nmacroparams = (i64)0);
    }
    else if ((mm_name$nmacroargs > mm_name$nmacroparams)) {
        mm_support$rxerror((byte*)"Too many macro args",0);
    }
;
    pnew = mm_name$copyunit((*d).code);
    if (!(!!(ignoreargs))) {
        mm_name$replaceunit(p,pnew);
    }
    else {
        (*p).a = pnew;
    }
;
}

static void mm_name$duplfield(struct mm_decls$strec *owner,struct mm_decls$strec *p,struct mm_decls$strec *q) {
    if (!!((*p).code)) {
        mm_support$serror((byte*)"DUPLFIELD");
    }
;
    (*q).flags = msysc$m_setdotindex((*q).flags,(i64)9,msysc$m_getdotindex((i64)(*p).flags,(i64)9));
    (*q).flags = (i64)(*p).flags;
    (*q).uflags = (*p).uflags;
    mm_lib$storemode(owner,(i64)(*p).mode,&(*q).mode);
}

static void mm_name$do_baseclass(struct mm_decls$strec *p) {
        struct mm_decls$strec *  d;
        struct mm_decls$strec *  e;
        struct mm_decls$strec *  newd;
        struct mm_decls$strec *  dbase;
        i64 normalexit;
    dbase = mm_decls$ttnamedef[((i64)(*p).baseclass)];
    d = (struct mm_decls$strec *)(*dbase).deflist;
    L527 :;
    while (!!(d)) {
        e = (struct mm_decls$strec *)(*p).deflist;
        normalexit = (i64)1;
        L530 :;
        while (!!(e)) {
            if (!!(mlib$eqstring((*d).name,(*e).name))) {
                normalexit = (i64)0;
                goto L532 ;
            }
;
            e = (struct mm_decls$strec *)(*e).nextdef;
L531 :;
        }
L532 :;
        ;
        if (!!(normalexit)) {
                        {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)6) || ($temp==(i64)20)) {
                newd = mm_lib$getduplnameptr(p,d,(i64)20);
                (*newd).equivfield = (struct mm_decls$strec *)d;
            }
            else {
                newd = mm_lib$getduplnameptr(p,d,(i64)(*d).nameid);
                mm_name$duplfield((struct mm_decls$strec *)(*p).owner,d,newd);
            }
            };
            mm_lib$adddef(p,newd);
        }
;
        d = (struct mm_decls$strec *)(*d).nextdef;
L528 :;
    }
L529 :;
    ;
}

// START
void mm_name$start(void) {

}

i64 mm_parse$parsemodule(i64 n) {
        struct mm_decls$modulerec *  pm;
        struct mm_decls$strec *  owner;
    mm_parse$initparser();
    pm = (struct mm_decls$modulerec *)&mm_decls$moduletable[(n)];
    mm_decls$currmoduleno = n;
    mm_decls$stmodule = (*pm).stmodule;
    mm_decls$currproc = mm_decls$stmodule;
    mm_decls$stsubprog = mm_decls$subprogtable[((i64)(*mm_decls$stmodule).moduleno)].stsubprog;
    mm_decls$currsubprog = mm_decls$stsubprog;
    mm_lex$startlex((i64)(*pm).fileno);
    owner = (struct mm_decls$strec *)mm_decls$stmodule;
    mm_lex$lex();
    (*pm).modulecode = mm_parse$readmoduledefs((struct mm_decls$strec *)owner);
    return (i64)1;
}

struct mm_decls$unitrec *mm_parse$readmoduledefs(struct mm_decls$strec *owner) {
        i64 globalflag;
        i64 callbackflag;
        struct mm_decls$unitrec *  ulist;
        struct mm_decls$unitrec *  ulistx;
        struct mm_decls$unitrec *  p;
    globalflag = (i64)0;
    callbackflag = (i64)0;
    ulist = (ulistx = 0);
    L533 :;
    while (1) {
        switch ((i64)mm_decls$lx.symbol) {
        case 156:;
            {
                if (!!(globalflag)) {
                    mm_support$serror((byte*)"global global?");
                }
;
                globalflag = (i64)mm_decls$lx.subcode;
                if (((globalflag == (i64)3) && ((i64)(*mm_decls$stmodule).subprogno != (i64)1))) {
                    globalflag = (i64)2;
                }
;
                mm_lex$lex();
            }
            break;
        case 130:;
        case 131:;
            {
                mm_parse$readprocdef((struct mm_decls$strec *)owner,globalflag,callbackflag);
                callbackflag = (i64)0;
                globalflag = (i64)0;
            }
            break;
        case 93:;
        case 140:;
        case 97:;
        case 94:;
        case 15:;
        case 145:;
        case 143:;
            {
                //dovar:
L535 :;
;
                mm_parse$readvardef((struct mm_decls$strec *)owner,globalflag,(i64)0,(i64)11,(i64)0);
                globalflag = (i64)0;
            }
            break;
        case 141:;
            {
                mm_lex$lex();
                mm_parse$readvardef((struct mm_decls$strec *)owner,globalflag,(i64)0,(i64)11,(i64)141);
                globalflag = (i64)0;
            }
            break;
        case 142:;
            {
                mm_lex$lex();
                mm_parse$readvardef((struct mm_decls$strec *)owner,globalflag,(i64)0,(i64)11,(i64)142);
                globalflag = (i64)0;
            }
            break;
        case 144:;
            {
                mm_parse$lexchecksymbol((i64)15);
                goto L535 ;
;
            }
            break;
        case 136:;
            {
                mm_parse$readimportmodule((struct mm_decls$strec *)owner);
            }
            break;
        case 137:;
            {
                mm_parse$readtypedef((struct mm_decls$strec *)owner,globalflag);
                globalflag = (i64)0;
            }
            break;
        case 149:;
            {
                mm_parse$readconstdef((struct mm_decls$strec *)owner,globalflag);
                globalflag = (i64)0;
            }
            break;
        case 152:;
        case 133:;
            {
                mm_parse$readclassdef((struct mm_decls$strec *)owner,globalflag);
                globalflag = (i64)0;
            }
            break;
        case 162:;
            {
                mm_parse$readtabledef((struct mm_decls$strec *)owner,globalflag);
                globalflag = (i64)0;
            }
            break;
        case 70:;
            {
                mm_parse$adddocstring(mm_decls$lx.svalue);
                mm_lex$lex();
            }
            break;
        case 6:;
            {
                mm_lex$lex();
            }
            break;
        case 68:;
            {
                goto L534 ;
            }
            break;
        case 155:;
            {
                if (((i64)mm_decls$lx.subcode == (i64)4)) {
                    callbackflag = (i64)4;
                    mm_lex$lex();
                }
                else {
                    mm_support$serror((byte*)"fflang?");
                }
;
            }
            break;
        case 146:;
            {
                mm_parse$readmacrodef((struct mm_decls$strec *)owner,globalflag);
                globalflag = (i64)0;
            }
            break;
        case 153:;
            {
                L536 :;
                do {
                    mm_lex$lex();
L537 :;
                }
                while (!((i64)mm_decls$lx.symbol == (i64)6));
L538 :;
                ;
            }
            break;
        case 2:;
            {
                mm_support$serror((byte*)"MODULE/DOT");
            }
            break;
        case 81:;
            {
                if (!!(mm_parse$istypestarter())) {
                    goto L535 ;
;
                }
;
                goto L539 ;
;
            }
            break;
        default: {
            //doexec:
L539 :;
;
            p = mm_parse$readunit();
            mm_lib$addlistunit(&ulist,&ulistx,p);
        }
        } //SW
;
    }
L534 :;
    ;
    return ulist;
}

static void mm_parse$initparser(void) {
        u8 *  tabledataname;
    if (!(!!(mm_decls$nullunit))) {
        mm_decls$nullunit = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)2);
    }
;
    mm_parse$try_level = (i64)0;
    mm_decls$currproc = 0;
    mm_parse$varattribs = (i64)0;
    mm_parse$intabledata = (i64)0;
    mm_parse$inreadprint = (i64)0;
    mm_parse$inparamlist = (i64)0;
    mm_parse$inrecordbody = (i64)0;
    mm_parse$inimportmodule = (i64)0;
    tabledataname = (byte*)"";
    mm_parse$labelseen = (i64)0;
    mm_parse$ndollar = (i64)0;
}

void mm_parse$skipsemi(void) {
    L540 :;
    while (((i64)mm_decls$lx.symbol == (i64)6)) {
        mm_lex$lex();
L541 :;
    }
L542 :;
    ;
}

struct mm_decls$unitrec *mm_parse$makeblock(struct mm_decls$unitrec *p) {
    if ((!!(p) && ((i64)(*p).tag == (i64)4))) {
        return p;
    }
;
    return (struct mm_decls$unitrec *)mm_lib$createunit1((i64)4,(struct mm_decls$unitrec *)p);
}

static void mm_parse$checkequals(void) {
    if (((i64)mm_decls$lx.symbol != (i64)48)) {
        mm_support$serror((byte*)"\"=\" expected");
    }
;
}

static i64 mm_parse$getcurrline(void) {
    return (i64)mm_decls$lx.pos;
}

static i64 mm_parse$checkbegin(i64 fbrack) {
        i64 closesym;
    mm_parse$skipsemi();
    if ((((i64)mm_decls$lx.symbol == (i64)13) && !!(fbrack))) {
        closesym = (i64)14;
        mm_lex$lex();
    }
    else {
        closesym = (i64)105;
    }
;
    return closesym;
}

static void mm_parse$checkbeginend(i64 closesym,i64 kwd,i64 startline) {
    mm_parse$skipsemi();
    if ((closesym == (i64)14)) {
        mm_parse$checksymbol(closesym);
    }
    else {
        mm_parse$checkend(closesym,kwd,(i64)0,startline);
    }
;
    mm_lex$lex();
}

void mm_parse$checkend(i64 endsym,i64 endkwd1,i64 endkwd2,i64 startline) {
        u8 str[100];
    if ((endsym==(i64)mm_decls$lx.symbol && (i64)mm_decls$lx.symbol==(i64)14)) {
        return;
    }
;
    if (((i64)mm_decls$lx.symbol != (i64)105)) {
        strcpy((u8 *)str,(byte*)"Bad 'end' ");
        //error:
L543 :;
;
        if (!!(startline)) {
            msysc$m_print_startstr((str + strlen((u8 *)str)));
            msysc$m_print_setfmt((byte*)" (from line #)");
            msysc$m_print_i64((startline & (i64)16777215),NULL);
            msysc$m_print_end();
            ;
        }
;
        mm_support$serror((u8 *)str);
    }
;
    if (((i64)mm_decls$lx.subcode == (i64)0)) {
        return;
    }
;
    if (!(((!!(endkwd1) && (endkwd1 == (i64)mm_decls$lx.subcode)) || (!!(endkwd2) && (endkwd2 == (i64)mm_decls$lx.subcode))))) {
        strcpy((u8 *)str,(byte*)"Mismatched 'end'");
        goto L543 ;
;
    }
;
}

static struct mm_decls$unitrec *mm_parse$readvardef(struct mm_decls$strec *owner,i64 scope,i64 isstatic,i64 varid,i64 k) {
        struct mm_decls$unitrec *  ulist;
        struct mm_decls$unitrec *  ulistx;
        struct mm_decls$unitrec *  p;
        i64 nvars;
        i64 m;
        i64 initcode;
        struct mm_decls$strec *  stname;
    ulist = (ulistx = 0);
    if (!!(mm_parse$istypestarter())) {
        m = mm_parse$readtypespec((struct mm_decls$strec *)owner,(i64)0);
    }
    else if (!!(k)) {
        m = (i64)21;
    }
    else {
        mm_support$serror((byte*)"Readvar?");
    }
;
    nvars = (i64)0;
    L544 :;
    while (((i64)mm_decls$lx.symbol == (i64)81)) {
        ++(nvars);
        stname = (struct mm_decls$strec *)mm_lib$getduplnameptr((struct mm_decls$strec *)owner,(struct mm_decls$strec *)mm_decls$lx.symptr,varid);
        (*stname).scope = scope;
        (*stname).flags = msysc$m_setdotindex((*stname).flags,(i64)0,(u64)isstatic);
        (*stname).flags = msysc$m_setdotindex((*stname).flags,(i64)4,(u64)(k == (i64)142));
        if ((varid == (i64)8)) {
            (*stname).flags = msysc$m_setdotindex((*stname).flags,(i64)12,(u64)1u);
        }
;
        mm_lib$adddef((struct mm_decls$strec *)owner,(struct mm_decls$strec *)stname);
        if ((varid == (i64)11)) {
            mm_lib$addstatic((struct mm_decls$strec *)stname);
        }
;
        mm_lex$lex();
        if (((i64)mm_decls$lx.symbol == (i64)7)) {
            if ((m != (i64)21)) {
                mm_support$serror((byte*)"Mixed var T x:T");
            }
;
            mm_lex$lex();
            m = mm_parse$readtypespec((struct mm_decls$strec *)owner,(i64)0);
        }
;
        mm_lib$storemode((struct mm_decls$strec *)owner,m,&(*stname).mode);
        if (((i64)mm_decls$lx.symbol == (i64)9 || (i64)mm_decls$lx.symbol == (i64)48 || (i64)mm_decls$lx.symbol == (i64)10)) {
                        {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)48)) {
                initcode = (i64)1;
            }
            else if (($temp==(i64)9)) {
                initcode = (i64)2;
            }
            else {
                initcode = (i64)3;
            }
            };
            if (((i64)mm_decls$lx.symbol != (i64)48)) {
                if ((varid == (i64)11)) {
                    mm_support$serror((byte*)"Non-variants can't use :=");
                    if (((i64)(*owner).nameid == (i64)6)) {
                        mm_support$serror((byte*)"Can't use := for statics inside procs");
                    }
;
                }
;
            }
            else {
                if ((varid == (i64)12)) {
                    mm_support$serror((byte*)"Need 'static' for '='");
                    mm_lib$addstatic((struct mm_decls$strec *)stname);
                }
;
            }
;
            mm_lex$lex();
            if (((i64)mm_decls$lx.symbol == (i64)170)) {
                mm_lex$lex();
                if ((varid != (i64)12)) {
                    mm_support$serror((byte*)"empty: not frame");
                }
;
                p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)122,(struct mm_decls$unitrec *)mm_lib$createname((struct mm_decls$strec *)stname));
                mm_lib$addlistunit(&ulist,&ulistx,p);
            }
            else {
                (*stname).code = mm_parse$readunit();
                (*stname).equals = initcode;
                if ((varid == (i64)12)) {
                    p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)23,(struct mm_decls$unitrec *)mm_lib$createname((struct mm_decls$strec *)stname),(struct mm_decls$unitrec *)(*stname).code);
                    (*p).initlet = (i64)1;
                    mm_lib$addlistunit(&ulist,&ulistx,p);
                }
;
            }
;
        }
        else if (((i64)mm_decls$lx.symbol == (i64)22)) {
            if ((k == (i64)142)) {
                mm_support$serror((byte*)"let@");
            }
;
            mm_lex$lex();
            (*stname).flags = msysc$m_setdotindex((*stname).flags,(i64)10,(u64)1u);
            (*stname).equivvar = mm_parse$readunit();
        }
        else if ((k == (i64)142)) {
            mm_support$serror((byte*)"let needs :=/=");
        }
;
        if (((i64)mm_decls$lx.symbol != (i64)5)) {
            goto L546 ;
        }
;
        mm_lex$lex();
L545 :;
    }
L546 :;
    ;
    if ((nvars == (i64)0)) {
        mm_support$serror((byte*)"No vars declared");
    }
;
    return ulist;
}

static void mm_parse$readconstdef(struct mm_decls$strec *owner,i64 scope) {
        i64 nconsts;
        i64 deft;
        i64 m;
        struct mm_decls$strec *  stname;
    mm_lex$lex();
    nconsts = (i64)0;
    if (!!(mm_parse$istypestarter())) {
        deft = mm_parse$readtypespec((struct mm_decls$strec *)owner,(i64)0);
    }
    else {
        deft = (i64)21;
    }
;
    L547 :;
    while (((i64)mm_decls$lx.symbol == (i64)81)) {
        stname = (struct mm_decls$strec *)mm_lib$getduplnameptr((struct mm_decls$strec *)owner,(struct mm_decls$strec *)mm_decls$lx.symptr,(i64)10);
        mm_lex$lex();
        mm_parse$checkequals();
        mm_lex$lex();
        (*stname).code = mm_parse$readconstexpr((i64)1);
        m = deft;
        mm_lib$storemode((struct mm_decls$strec *)owner,m,&(*stname).mode);
        ++(nconsts);
        (*stname).scope = scope;
        mm_lib$adddef((struct mm_decls$strec *)owner,(struct mm_decls$strec *)stname);
        if (((scope == (i64)3) && ((u64)(*(*stname).name) != '$'))) {
            mm_lib$addexpconst((struct mm_decls$strec *)stname);
        }
;
        if (((i64)mm_decls$lx.symbol != (i64)5)) {
            goto L549 ;
        }
;
        mm_lex$lex();
L548 :;
    }
L549 :;
    ;
    if ((nconsts == (i64)0)) {
        mm_support$serror((byte*)"No consts declared");
    }
;
}

static struct mm_decls$unitrec *mm_parse$readlbrack(void) {
        struct mm_decls$unitrec *  ulist;
        struct mm_decls$unitrec *  ulistx;
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  q;
        struct mm_decls$unitrec *  r;
        struct mm_decls$unitrec *  plower;
        i64 oldirp;
        i64 length;
        i64 usecomma;
    mm_lex$lex();
    ulist = (ulistx = 0);
    plower = 0;
    length = (i64)0;
    if (((i64)mm_decls$lx.symbol == (i64)22)) {
        mm_lex$lex();
        oldirp = mm_parse$inreadprint;
        mm_parse$inreadprint = (i64)1;
        plower = mm_parse$readunit();
        mm_parse$inreadprint = oldirp;
        mm_parse$checksymbol((i64)7);
        mm_lex$lex();
    }
    else if ((((i64)mm_decls$lx.symbol == (i64)72) && ((i64)mm_decls$nextlx.symbol == (i64)7))) {
        plower = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)mm_decls$lx.value,(i64)mm_decls$lx.subcode);
        (*plower).istrueconst = (i64)1;
        mm_lex$lex();
        mm_lex$lex();
    }
    else if ((((i64)mm_tables$symboloptypes[((i64)mm_decls$lx.symbol)-1] == (i64)1) && ((i64)mm_decls$nextlx.symbol == (i64)14))) {
        p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)53);
        (*p).pclop = (i64)mm_tables$symbolgentoops[((i64)mm_decls$lx.symbol)-1];
        mm_lex$lex();
        mm_lex$lex();
        return p;
    }
    else if ((((i64)mm_tables$symboloptypes[((i64)mm_decls$lx.symbol)-1] == (i64)1) && ((i64)mm_decls$nextlx.symbol == (i64)9))) {
        p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)53);
        (*p).pclop = (i64)mm_tables$symbolgentoops[((i64)mm_decls$lx.symbol)-1];
        mm_lex$lex();
        mm_parse$lexchecksymbol((i64)14);
        mm_lex$lex();
        return p;
    }
;
        {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)14)) {
        mm_lex$lex();
        p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)15);
        (*p).b = plower;
        (*p).length = (i64)0;
        return p;
    }
    else {
        p = mm_parse$readunit();
    }
    };
        {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)14)) {
        mm_lex$lex();
        return p;
    }
    else if (($temp==(i64)5)) {
        usecomma = (i64)1;
        if (((i64)mm_decls$nextlx.symbol == (i64)14)) {
            mm_lex$lex();
            mm_lex$lex();
            p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)15,(struct mm_decls$unitrec *)p);
            (*p).length = (i64)1;
            (*p).b = plower;
            return p;
        }
;
        //docomma:
L550 :;
;
        length = (i64)1;
        ulist = (ulistx = p);
        if (!!(usecomma)) {
            L551 :;
            do {
                mm_lex$lex();
                if (((i64)mm_decls$lx.symbol == (i64)14)) {
                    goto L553 ;
                }
;
                if (((i64)mm_decls$lx.symbol == (i64)5)) {
                    mm_support$serror((byte*)",, null expr not allowed");
                }
;
                mm_lib$addlistunit(&ulist,&ulistx,mm_parse$readunit());
                ++(length);
                mm_parse$skipsemi();
L552 :;
            }
            while (!((i64)mm_decls$lx.symbol != (i64)5));
L553 :;
            ;
        }
        else {
            L554 :;
            do {
                mm_parse$skipsemi();
                if (((i64)mm_decls$lx.symbol == (i64)14)) {
                    goto L556 ;
                }
;
                if (((i64)mm_decls$lx.symbol == (i64)5)) {
                    mm_support$serror((byte*)",, null expr not allowed");
                }
;
                mm_lib$addlistunit(&ulist,&ulistx,mm_parse$readunit());
                ++(length);
L555 :;
            }
            while (!((i64)mm_decls$lx.symbol != (i64)6));
L556 :;
            ;
        }
;
        mm_parse$checksymbol((i64)14);
        mm_lex$lex();
        p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)15,(struct mm_decls$unitrec *)ulist);
        (*p).length = length;
        (*p).b = plower;
        return p;
    }
    else if (($temp==(i64)20)) {
        mm_lex$lex();
        q = mm_parse$readunit();
                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)20)) {
            mm_lex$lex();
            r = mm_parse$readsunit((i64)0);
            mm_parse$checksymbol((i64)14);
            mm_lex$lex();
            return (struct mm_decls$unitrec *)mm_lib$createunit3((i64)90,(struct mm_decls$unitrec *)mm_parse$fixcond(p),(struct mm_decls$unitrec *)q,(struct mm_decls$unitrec *)r);
        }
        else if (($temp==(i64)14)) {
            mm_lex$lex();
            return (struct mm_decls$unitrec *)mm_lib$createunit3((i64)90,(struct mm_decls$unitrec *)mm_parse$fixcond(p),(struct mm_decls$unitrec *)q,0);
        }
        };
        mm_lib$addlistunit(&ulist,&ulistx,q);
        mm_parse$checksymbol((i64)5);
        if (((i64)mm_decls$nextlx.symbol != (i64)20)) {
            L557 :;
            do {
                mm_lex$lex();
                mm_lib$addlistunit(&ulist,&ulistx,mm_parse$readunit());
L558 :;
            }
            while (!((i64)mm_decls$lx.symbol != (i64)5));
L559 :;
            ;
            mm_parse$checksymbol((i64)20);
        }
        else {
            mm_lex$lex();
        }
;
        mm_lex$lex();
        r = mm_parse$readunit();
        mm_parse$checksymbol((i64)14);
        mm_lex$lex();
        return (struct mm_decls$unitrec *)mm_lib$createunit3((i64)108,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)ulist,(struct mm_decls$unitrec *)r);
    }
    else if (($temp==(i64)6)) {
        if (((i64)mm_decls$lx.subcode == (i64)1)) {
            usecomma = (i64)0;
            goto L550 ;
;
        }
;
        ulist = (ulistx = p);
        L560 :;
        do {
            mm_parse$skipsemi();
            if (((i64)mm_decls$lx.symbol == (i64)14)) {
                goto L562 ;
            }
;
            mm_lib$addlistunit(&ulist,&ulistx,mm_parse$readunit());
L561 :;
        }
        while (!((i64)mm_decls$lx.symbol != (i64)6));
L562 :;
        ;
        mm_parse$checksymbol((i64)14);
        mm_lex$lex();
        return mm_parse$makeblock(ulist);
    }
    else {
        mm_support$serror((byte*)"(x ...");
    }
    };
    return (struct mm_decls$unitrec *)0;
}

static void mm_parse$addlistparam(struct mm_decls$strec **ulist,struct mm_decls$strec **ulistx,struct mm_decls$strec *p) {
    if (((*ulist) == 0)) {
        (*ulist) = ((*ulistx) = (struct mm_decls$strec *)p);
    }
    else {
        (*(*ulistx)).nextparam = (struct mm_decls$strec *)p;
    }
;
    (*ulistx) = (struct mm_decls$strec *)p;
}

static struct mm_decls$unitrec *mm_parse$readcast(void) {
        struct mm_decls$unitrec *  p;
        i64 opc;
        i64 t;
    t = mm_parse$readtypespec((struct mm_decls$strec *)mm_decls$currproc,(i64)0);
        {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)14)) {
        p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)52);
        (*p).mode = (i64)25;
        (*p).value = t;
        return p;
    }
    else if (($temp==(i64)22)) {
        opc = (i64)51;
        mm_lex$lex();
    }
    else if (($temp==(i64)2)) {
        if (((i64)mm_decls$nextlx.symbol == (i64)137)) {
            mm_lex$lex();
            p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)52);
            (*p).value = t;
            (*p).mode = (i64)25;
            mm_lex$lex();
        }
        else {
            p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)52);
            (*p).value = t;
        }
;
        return p;
    }
    else {
        opc = (i64)48;
    }
    };
    mm_parse$checksymbol((i64)13);
    mm_lex$lex();
    p = mm_parse$readunit();
    mm_parse$checksymbol((i64)14);
    mm_lex$lex();
    p = (struct mm_decls$unitrec *)mm_lib$createunit1(opc,(struct mm_decls$unitrec *)p);
    mm_lib$storemode(mm_decls$currproc,t,&(*p).convmode);
    return p;
}

static struct mm_decls$unitrec *mm_parse$readopc(void) {
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  q;
        struct mm_decls$unitrec *  r;
        i64 tag;
        i64 opc;
        i64 firstsym;
    firstsym = (i64)mm_decls$lx.symbol;
        {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)64)) {
        tag = (i64)32;
        opc = (i64)mm_decls$lx.subcode;
    }
    else if (($temp==(i64)65)) {
        tag = (i64)31;
        opc = (i64)mm_decls$lx.subcode;
    }
    else {
        tag = (i64)32;
        opc = (i64)mm_tables$symbolgenops[(firstsym)-1];
    }
    };
    mm_lex$lex();
    if ((firstsym==(i64)31)) {
        return mm_parse$readterm2();
    }
    else if ((firstsym==(i64)32)) {
        opc = (i64)29;
    }
    else if ((firstsym==(i64)43) || (firstsym==(i64)44) || (firstsym==(i64)65)) {
        p = mm_parse$readterm2();
        if (((i64)(*p).tag == (i64)15)) {
            if (((i64)(*p).length != (i64)2)) {
                mm_support$serror((byte*)"Needs (x,y)");
            }
;
            q = (*p).a;
            r = (*q).nextunit;
            (*q).nextunit = 0;
            p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)31,(struct mm_decls$unitrec *)q,(struct mm_decls$unitrec *)r);
            (*p).pclop = opc;
            return p;
        }
        else {
            mm_support$serror((byte*)"READOPC/SINGLE OPND?");
            return (struct mm_decls$unitrec *)mm_lib$createunit1(opc,(struct mm_decls$unitrec *)p);
        }
;
    }
    else {
        if (((i64)mm_tables$symboloptypes[(firstsym)-1] == (i64)1)) {
            mm_support$serror((byte*)"Can't be used as unary op");
        }
;
    }
;
    if (((i64)mm_decls$lx.symbol == (i64)9)) {
        mm_lex$lex();
        tag = (i64)34;
        if ((firstsym==(i64)32)) {
            opc = (i64)76;
        }
        else {
            opc = (i64)mm_tables$symbolgentoops[(firstsym)-1];
            if ((opc == (i64)0)) {
                mm_support$serror((byte*)"op:= not available");
            }
;
        }
;
    }
;
    p = (struct mm_decls$unitrec *)mm_lib$createunit1(tag,(struct mm_decls$unitrec *)(q = mm_parse$readterm2()));
    (*p).pclop = opc;
    if (((i64)(*q).tag == (i64)15)) {
        mm_support$serror((byte*)"Too many opnds");
    }
;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readsprint(void) {
        i64 oldinreadprint;
        i64 opc;
        i64 isfprint;
        struct mm_decls$unitrec *  pformat;
        struct mm_decls$unitrec *  pdev;
        struct mm_decls$unitrec *  printlist;
        struct mm_decls$unitrec *  printlistx;
        struct mm_decls$unitrec *  p;
    oldinreadprint = mm_parse$inreadprint;
    mm_parse$inreadprint = (i64)1;
    opc = (i64)mm_decls$lx.subcode;
    mm_parse$lexchecksymbol((i64)13);
    mm_lex$lex();
    if ((opc==(i64)115)) {
        isfprint = (i64)1;
    }
    else {
        isfprint = (i64)0;
    }
;
    printlist = (printlistx = 0);
    pformat = (pdev = 0);
    if (((i64)mm_decls$lx.symbol == (i64)22)) {
        mm_lex$lex();
        pdev = mm_parse$readunit();
        if (((i64)mm_decls$lx.symbol == (i64)5)) {
            mm_lex$lex();
        }
        else {
            goto L563 ;
;
        }
;
    }
;
    if (!!(isfprint)) {
        pformat = mm_parse$readunit();
        if (((i64)mm_decls$lx.symbol == (i64)5)) {
            mm_lex$lex();
        }
        else {
            goto L563 ;
;
        }
;
    }
;
    if (((i64)mm_decls$lx.symbol == (i64)14)) {
        goto L563 ;
;
    }
;
    L564 :;
    while (1) {
        if (((i64)mm_decls$lx.symbol == (i64)5)) {
            mm_lib$addlistunit(&printlist,&printlistx,(struct mm_decls$unitrec *)mm_lib$createunit0((i64)84));
        }
        else {
            p = mm_parse$readunit();
            if (((i64)mm_decls$lx.symbol == (i64)7)) {
                mm_lex$lex();
                p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)83,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)mm_parse$readunit());
            }
;
            mm_lib$addlistunit(&printlist,&printlistx,p);
        }
;
        if (((i64)mm_decls$lx.symbol != (i64)5)) {
            goto L565 ;
        }
;
        mm_lex$lex();
    }
L565 :;
    ;
    mm_parse$checksymbol((i64)14);
    //finish:
L563 :;
;
    mm_lex$lex();
    mm_parse$inreadprint = oldinreadprint;
    if ((((opc == (i64)110) || (opc == (i64)112)) && (printlist == 0))) {
        mm_support$serror((byte*)"No print items");
    }
;
    if (!!(isfprint)) {
        if (((i64)(*pformat).tag == (i64)2)) {
            mm_support$serror((byte*)"No fmt str");
        }
;
        return (struct mm_decls$unitrec *)mm_lib$createunit3(opc,(struct mm_decls$unitrec *)pdev,(struct mm_decls$unitrec *)pformat,(struct mm_decls$unitrec *)printlist);
    }
    else {
        return (struct mm_decls$unitrec *)mm_lib$createunit2(opc,(struct mm_decls$unitrec *)pdev,(struct mm_decls$unitrec *)printlist);
    }
;
}

static struct mm_decls$unitrec *mm_parse$readsread(void) {
        i64 oldinreadprint;
        i64 opc;
        struct mm_decls$unitrec *  pformat;
        struct mm_decls$unitrec *  pdev;
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  readlist;
        struct mm_decls$unitrec *  readlistx;
    oldinreadprint = mm_parse$inreadprint;
    mm_parse$inreadprint = (i64)1;
    opc = (i64)mm_decls$lx.subcode;
    mm_parse$lexchecksymbol((i64)13);
    mm_lex$lex();
    readlist = (readlistx = 0);
    pformat = (pdev = 0);
    if (((i64)mm_decls$lx.symbol == (i64)22)) {
        if ((opc == (i64)116)) {
            mm_support$serror((byte*)"@ on read");
        }
;
        mm_lex$lex();
        pdev = mm_parse$readunit();
        if (((i64)mm_decls$lx.symbol == (i64)5)) {
            mm_lex$lex();
        }
        else {
            goto L566 ;
;
        }
;
    }
;
    if (((i64)mm_decls$lx.symbol == (i64)14)) {
        goto L566 ;
;
    }
;
    L567 :;
    while (1) {
        p = mm_parse$readunit();
        if (((i64)mm_decls$lx.symbol == (i64)7)) {
            mm_lex$lex();
            p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)83,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)mm_parse$readunit());
        }
;
        mm_lib$addlistunit(&readlist,&readlistx,p);
        if (((i64)mm_decls$lx.symbol != (i64)5)) {
            goto L568 ;
        }
;
        mm_lex$lex();
    }
L568 :;
    ;
    mm_parse$checksymbol((i64)14);
    //finish:
L566 :;
;
    mm_lex$lex();
    mm_parse$inreadprint = oldinreadprint;
    if (((opc == (i64)116) && (readlist == 0))) {
        mm_support$serror((byte*)"No read items");
    }
;
    return (struct mm_decls$unitrec *)mm_lib$createunit2(opc,(struct mm_decls$unitrec *)pdev,(struct mm_decls$unitrec *)readlist);
}

static struct mm_decls$unitrec *mm_parse$readcompilervar(void) {
        u8 str[100];
        struct mlinux$rsystemtime tm;
        static u8 *  monthnames[12] = {
    (byte*)"Jan",
    (byte*)"Feb",
    (byte*)"Mar",
    (byte*)"Apr",
    (byte*)"May",
    (byte*)"Jun",
    (byte*)"Jul",
    (byte*)"Aug",
    (byte*)"Sep",
    (byte*)"Oct",
    (byte*)"Nov",
    (byte*)"Dec"
};
        struct mm_decls$unitrec *  p;
        struct mm_decls$modulerec *  currmodule;
    currmodule = (struct mm_decls$modulerec *)&mm_decls$moduletable[(mm_decls$currmoduleno)];
    switch ((i64)mm_decls$lx.subcode) {
    case 77:;
        {
            p = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)0u,(i64)7);
            mm_lex$lex();
            return p;
        }
        break;
    case 78:;
        {
            p = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)(i64)4614256656552045848,(i64)5);
            mm_lex$lex();
            return p;
        }
        break;
    case 79:;
        {
            if ((u64)1u) {
                p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)124);
                (*p).mode = (i64)5;
            }
            else {
                p = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)(i64)msysc$m_tp_r64toi64(m$infinity),(i64)5);
            }
;
            mm_lex$lex();
            return p;
        }
        break;
    case 62:;
        {
            p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)62);
            mm_lex$lex();
            return p;
        }
        break;
    case 63:;
        {
            msysc$getstrint(mm_support$getlineno((u64)(i64)mm_decls$lx.pos),(u8 *)str);
        }
        break;
    case 64:;
        {
            strcpy(str,(*mm_decls$stmodule).name);
        }
        break;
    case 65:;
        {
            strcpy(str,mm_decls$sourcefilepaths[((i64)(*currmodule).fileno)]);
        }
        break;
    case 66:;
        {
            strcpy((u8 *)str,(*mm_decls$currproc).name);
        }
        break;
    case 67:;
        {
            mlinux$os_getsystime(&tm);
            msysc$m_print_startstr(str);
            msysc$m_print_setfmt((byte*)"#-#-#");
            msysc$m_print_i64((i64)tm.day,NULL);
            msysc$m_print_str(monthnames[((i64)tm.month)-1],NULL);
            msysc$m_print_i64((i64)tm.year,(byte*)"4");
            msysc$m_print_end();
            ;
        }
        break;
    case 68:;
        {
            mlinux$os_getsystime(&tm);
            msysc$m_print_startstr(str);
            msysc$m_print_setfmt((byte*)"#:#:#");
            msysc$m_print_i64((i64)tm.hour,(byte*)"z2");
            msysc$m_print_i64((i64)tm.minute,(byte*)"z2");
            msysc$m_print_i64((i64)tm.second,(byte*)"z2");
            msysc$m_print_end();
            ;
        }
        break;
    case 71:;
        {
            mm_lex$lex();
            return (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)mm_decls$targetbits,(i64)3);
        }
        break;
    case 72:;
        {
            mm_lex$lex();
            return (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)mm_decls$targetsize,(i64)3);
        }
        break;
    case 73:;
        {
            strcpy((u8 *)str,(byte*)"wx64");
        }
        break;
    case 74:;
        {
            p = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)1u,(i64)3);
            mm_lex$lex();
            return p;
        }
        break;
    case 75:;
        {
            p = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)(i64)mm_decls$fwindows,(i64)3);
            mm_lex$lex();
            return p;
        }
        break;
    case 76:;
        {
            p = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)(i64)mm_decls$flinux,(i64)3);
            mm_lex$lex();
            return p;
        }
        break;
    case 69:;
        {
            strcpy((u8 *)str,(byte*)"Compiler:BX Experimental");
        }
        break;
    case 80:;
    case 81:;
        {
            p = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)((i64)mm_decls$lx.subcode == (i64)80),(i64)6);
            mm_lex$lex();
            return p;
        }
        break;
    default: {
        mm_support$serror_s((byte*)"compiler var not impl: #",mm_tables$jtagnames[((i64)mm_decls$lx.subcode)]);
    }
    } //SW
;
    mm_lex$lex();
    return (struct mm_decls$unitrec *)mm_lib$createstringconstunit(mlib$pcm_copyheapstring((u8 *)str),(i64)-1);
}

static struct mm_decls$unitrec *mm_parse$readcastx(void) {
        i64 opc;
        i64 m;
        struct mm_decls$unitrec *  p;
    mm_lex$lex();
    opc = (i64)48;
    if (((i64)mm_decls$lx.symbol == (i64)22)) {
        opc = (i64)51;
        mm_lex$lex();
    }
;
    mm_parse$checksymbol((i64)13);
    mm_lex$lex();
    m = (i64)0;
    p = mm_parse$readunit();
    if (((i64)mm_decls$lx.symbol != (i64)5)) {
        if ((opc == (i64)51)) {
            mm_support$serror((byte*)"@ type missing");
        }
;
        opc = (i64)50;
    }
    else {
        mm_lex$lex();
        m = mm_parse$readtypespec((struct mm_decls$strec *)mm_decls$currproc,(i64)0);
    }
;
    mm_parse$checksymbol((i64)14);
    mm_lex$lex();
    p = (struct mm_decls$unitrec *)mm_lib$createunit1(opc,(struct mm_decls$unitrec *)p);
    mm_lib$storemode(mm_decls$currproc,m,&(*p).convmode);
    return p;
}

void mm_parse$checksymbol(i64 symbol) {
        u8 str[100];
    if (((i64)mm_decls$lx.symbol != symbol)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"# expected, not #");
        msysc$m_print_str(mm_tables$symbolnames[(symbol)-1],NULL);
        msysc$m_print_str(mm_tables$symbolnames[((i64)mm_decls$lx.symbol)-1],NULL);
        msysc$m_print_end();
        ;
        mm_support$serror((u8 *)str);
    }
;
}

void mm_parse$lexchecksymbol(i64 symbol) {
    mm_lex$lex();
    mm_parse$checksymbol(symbol);
}

i64 mm_parse$readtypespec(struct mm_decls$strec *owner,i64 typedefx) {
        struct mm_decls$strec *  d;
        i64 t;
        i64 fflang;
        struct mm_decls$unitrec *  dim;
        struct mm_decls$unitrec *  length;
        struct mm_decls$unitrec *  dims[30];
        i64 ndims;
        i64 i;
        {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)15)) {
        //arraybounds:
L569 :;
;
        mm_lex$lex();
        ndims = (i64)0;
        mm_parse$inreadprint = (i64)1;
        L570 :;
        while (1) {
            length = 0;
            if ((((i64)mm_decls$lx.symbol == (i64)16) || ((i64)mm_decls$lx.symbol == (i64)5))) {
                dim = 0;
            }
            else {
                dim = mm_parse$readunit();
                                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)16) || ($temp==(i64)5)) {
                }
                else if (($temp==(i64)7)) {
                    mm_lex$lex();
                    if (!((((i64)mm_decls$lx.symbol == (i64)5) || ((i64)mm_decls$lx.symbol == (i64)16)))) {
                        length = mm_parse$readunit();
                        dim = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)22,(struct mm_decls$unitrec *)dim,(struct mm_decls$unitrec *)length);
                    }
                    else {
                        dim = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)22,(struct mm_decls$unitrec *)dim);
                    }
;
                }
                };
            }
;
            if ((ndims >= (i64)30)) {
                mm_support$serror((byte*)"Too many array dims");
            }
;
            dims[(++(ndims))-1] = dim;
            if (((i64)mm_decls$lx.symbol != (i64)5)) {
                goto L571 ;
            }
;
            mm_lex$lex();
        }
L571 :;
        ;
        mm_parse$inreadprint = (i64)0;
        mm_parse$checksymbol((i64)16);
        mm_lex$lex();
        t = mm_parse$readtypespec(owner,(i64)0);
        for (i=ndims;i>=(i64)1;--i) {
L572 :;
            t = mm_lib$createarraymode((struct mm_decls$strec *)owner,t,dims[(i)-1],((i == (i64)1) ? typedefx : (i64)0));
L573 :;
        }
L574 :;
        ;
        return t;
    }
    else if (($temp==(i64)93)) {
        t = (i64)mm_decls$lx.subcode;
        mm_lex$lex();
    }
    else if (($temp==(i64)81)) {
        d = (struct mm_decls$strec *)mm_decls$lx.symptr;
        mm_lex$lex();
        if (((i64)mm_decls$lx.symbol == (i64)2)) {
            mm_parse$lexchecksymbol((i64)81);
            t = mm_lib$newtypename((struct mm_decls$strec *)d,(struct mm_decls$strec *)mm_decls$lx.symptr);
            mm_lex$lex();
        }
        else {
            t = mm_lib$newtypename(0,(struct mm_decls$strec *)d);
        }
;
    }
    else if (($temp==(i64)133) || ($temp==(i64)134)) {
        mm_support$serror((byte*)"Use 'record name =' syntax");
    }
    else if (($temp==(i64)135)) {
        mm_support$serror((byte*)"Top-level union not allowed");
    }
    else if (($temp==(i64)140)) {
        fflang = (i64)0;
        //retry:
L575 :;
;
        mm_lex$lex();
        if (((i64)mm_decls$lx.symbol == (i64)112)) {
            mm_lex$lex();
        }
;
                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)130) || ($temp==(i64)131)) {
            t = mm_parse$readrefproc((struct mm_decls$strec *)owner,typedefx,fflang);
        }
        else if (($temp==(i64)155)) {
            fflang = (i64)mm_decls$lx.subcode;
            goto L575 ;
;
        }
        else {
            if (((i64)mm_decls$lx.symbol == (i64)93)) {
                                {i64 $temp = (i64)mm_decls$lx.subcode;
if (($temp==(i64)12)) {
                    t = (i64)20;
                    if (!!(typedefx)) {
                        mm_decls$tttarget[(typedefx)] = (i64)12;
                    }
;
                }
                else {
                    goto L576 ;
;
                }
                };
                mm_lex$lex();
            }
            else {
                //readtarget:
L576 :;
;
                t = mm_parse$readtypespec(owner,(i64)0);
                t = mm_lib$createrefmode((struct mm_decls$strec *)owner,t,typedefx);
            }
;
        }
        };
    }
    else if (($temp==(i64)97)) {
        mm_lex$lex();
        t = (i64)20;
        if (!!(typedefx)) {
            mm_decls$tttarget[(typedefx)] = (i64)12;
        }
;
    }
    else if (($temp==(i64)94)) {
        mm_parse$lexchecksymbol((i64)13);
        mm_parse$lexchecksymbol((i64)81);
        t = mm_lib$newtypename((struct mm_decls$strec *)mm_decls$lx.symptr,0);
        mm_parse$lexchecksymbol((i64)14);
        mm_lex$lex();
    }
    else if (($temp==(i64)143)) {
        t = mm_parse$readslicetype((struct mm_decls$strec *)owner,(i64)mm_decls$lx.subcode,typedefx);
    }
    else if (($temp==(i64)144)) {
        mm_parse$lexchecksymbol((i64)15);
        goto L569 ;
;
    }
    else {
        mm_support$serror((byte*)"Bad type starter");
    }
    };
    if (!!(typedefx)) {
        mm_decls$ttbasetype[(typedefx)] = (i64)mm_decls$ttbasetype[(t)];
    }
;
    return t;
}

static i64 mm_parse$readslicetype(struct mm_decls$strec *owner,i64 slicetype,i64 typedefx) {
        struct mm_decls$unitrec *  plower;
        i64 t;
    mm_parse$lexchecksymbol((i64)15);
    mm_lex$lex();
    if (((i64)mm_decls$lx.symbol != (i64)16)) {
        mm_parse$inreadprint = (i64)1;
        plower = mm_parse$readunit();
        mm_parse$inreadprint = (i64)0;
        mm_parse$checksymbol((i64)7);
        mm_parse$lexchecksymbol((i64)16);
    }
    else {
        plower = 0;
    }
;
    mm_lex$lex();
    t = mm_parse$readtypespec((struct mm_decls$strec *)owner,typedefx);
    return mm_lib$createslicemode((struct mm_decls$strec *)owner,slicetype,t,plower,typedefx);
}

static struct mm_decls$unitrec *mm_parse$readslist(i64 iscall,i64 donulls) {
        struct mm_decls$unitrec *  ulist;
        struct mm_decls$unitrec *  ulistx;
        i64 oldinparamlist;
    ulist = (ulistx = 0);
    mm_parse$skipsemi();
    if (((i64)mm_decls$lx.symbol == (i64)14)) {
        return ulist;
    }
;
    oldinparamlist = mm_parse$inparamlist;
    mm_parse$inparamlist = iscall;
    L577 :;
    while (1) {
        mm_parse$skipsemi();
                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)5)) {
            if (!!(donulls)) {
                mm_lib$addlistunit(&ulist,&ulistx,(struct mm_decls$unitrec *)mm_lib$createunit0((i64)2));
            }
            else {
                mm_support$serror((byte*)"null comma expr not allowed");
            }
;
            mm_lex$lex();
        }
        else if (($temp==(i64)14)) {
            if (!!(donulls)) {
                mm_lib$addlistunit(&ulist,&ulistx,mm_decls$nullunit);
            }
;
            goto L578 ;
        }
        else {
            mm_lib$addlistunit(&ulist,&ulistx,mm_parse$readunit());
            if (((i64)mm_decls$lx.symbol == (i64)5 || (i64)mm_decls$lx.symbol == (i64)6)) {
                mm_lex$lex();
                if (((i64)mm_decls$lx.symbol == (i64)14)) {
                    goto L578 ;
                }
;
            }
            else {
                mm_parse$skipsemi();
                if (((i64)mm_decls$lx.symbol == (i64)14)) {
                    goto L578 ;
                }
;
                mm_support$serror((byte*)"SLIST?");
            }
;
        }
        };
    }
L578 :;
    ;
    mm_parse$inparamlist = oldinparamlist;
    return ulist;
}

static struct mm_decls$unitrec *mm_parse$readindex(struct mm_decls$unitrec *p,i64 dot) {
        struct mm_decls$unitrec *  q;
        struct mm_decls$unitrec *  plower;
        struct mm_decls$unitrec *  pupper;
    mm_lex$lex();
    if (!(!!(dot))) {
                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)16)) {
            //fullslice:
L579 :;
;
            mm_lex$lex();
            plower = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)32,(struct mm_decls$unitrec *)mm_lib$duplunit(p,(i64)0));
            (*plower).pclop = (i64)89;
            pupper = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)32,(struct mm_decls$unitrec *)mm_lib$duplunit(p,(i64)0));
            (*pupper).pclop = (i64)90;
            p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)41,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)mm_lib$createunit2((i64)16,(struct mm_decls$unitrec *)plower,(struct mm_decls$unitrec *)pupper));
            return p;
        }
        else if (($temp==(i64)28) || ($temp==(i64)7)) {
            mm_parse$lexchecksymbol((i64)16);
            goto L579 ;
;
        }
        };
    }
;
    L580 :;
    while (1) {
        if ((mm_parse$ndollar >= (i64)10)) {
            mm_support$serror((byte*)"Too many nested a[$]");
        }
;
        mm_parse$dollarstack[(++(mm_parse$ndollar))-1] = p;
        q = mm_parse$readunit();
        --(mm_parse$ndollar);
        if (((i64)(*q).tag == (i64)16)) {
            p = (struct mm_decls$unitrec *)mm_lib$createunit2((!!(dot) ? (i64)44 : (i64)41),(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)q);
        }
        else {
            p = (struct mm_decls$unitrec *)mm_lib$createunit2((!!(dot) ? (i64)43 : (i64)40),(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)q);
        }
;
        if (((i64)mm_decls$lx.symbol != (i64)5)) {
            goto L581 ;
        }
;
        mm_lex$lex();
    }
L581 :;
    ;
    mm_parse$checksymbol((i64)16);
    mm_lex$lex();
    return p;
}

static struct mm_decls$unitrec *mm_parse$readdotsuffix(struct mm_decls$unitrec *p) {
        struct mm_decls$unitrec *  q;
    L582 :;
    while (((i64)mm_decls$lx.symbol == (i64)2)) {
        mm_lex$lex();
        switch ((i64)mm_decls$lx.symbol) {
        case 15:;
            {
                p = mm_parse$readindex(p,(i64)1);
            }
            break;
        case 81:;
            {
                p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)42,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)mm_lib$createname((struct mm_decls$strec *)mm_decls$lx.symptr));
                mm_lex$lex();
            }
            break;
        case 63:;
            {
                //doprop:
L585 :;
;
                p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)32,(struct mm_decls$unitrec *)p);
                (*p).pclop = (i64)mm_decls$lx.subcode;
                mm_lex$lex();
            }
            break;
        case 66:;
            {
                p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)59,(struct mm_decls$unitrec *)p);
                (*p).bfcode = (i64)mm_decls$lx.subcode;
                mm_lex$lex();
            }
            break;
        case 137:;
            {
                                {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)52)) {
                }
                else {
                    p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)57,(struct mm_decls$unitrec *)p);
                }
                };
                mm_lex$lex();
            }
            break;
        case 44:;
            {
                mm_decls$lx.subcode = (i64)96;
                goto L585 ;
;
            }
            break;
        case 43:;
            {
                mm_decls$lx.subcode = (i64)95;
                goto L585 ;
;
            }
            break;
        case 93:;
            {
                if ((((i64)(*p).tag == (i64)52) && ((i64)mm_decls$lx.subcode == (i64)9))) {
                    q = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)16,(struct mm_decls$unitrec *)mm_lib$createunit1((i64)32,(struct mm_decls$unitrec *)p),(struct mm_decls$unitrec *)mm_lib$createunit1((i64)32,(struct mm_decls$unitrec *)p));
                    (*(*q).a).pclop = (i64)95;
                    (*(*q).b).pclop = (i64)96;
                }
                else {
                    goto L586 ;
;
                }
;
                mm_lex$lex();
                p = q;
            }
            break;
        default: {
            //error:
L586 :;
;
            mm_support$serror((byte*)"Unknown dot suffix");
        }
        } //SW
;
L583 :;
    }
L584 :;
    ;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readconstexpr(i64 needconst) {
    return mm_parse$readunit();
}

static i64 mm_parse$readconstint(void) {
        i64 x;
    if (((i64)mm_decls$lx.symbol == (i64)72)) {
        x = mm_decls$lx.value;
        mm_lex$lex();
        return x;
    }
    else if (((i64)mm_decls$lx.symbol == (i64)32)) {
        mm_lex$lex();
        if (((i64)mm_decls$lx.symbol == (i64)72)) {
            x = mm_decls$lx.value;
            mm_lex$lex();
            return -(x);
        }
;
    }
;
    mm_support$serror((byte*)"Can't do complex expr");
    return (i64)0;
}

static void mm_parse$readprocdef(struct mm_decls$strec *procowner,i64 scope,i64 fflang) {
        i64 kwd;
        i64 startline;
        i64 closesym;
        i64 shortfun;
        struct mm_decls$strec *  stproc;
        struct mm_decls$strec *  stname;
        i64 i;
    kwd = (i64)mm_decls$lx.symbol;
    shortfun = (i64)((i64)mm_decls$lx.subcode == (i64)1);
    mm_parse$nforloops = (i64)0;
    mm_decls$assemmode = (i64)1;
    stproc = (struct mm_decls$strec *)mm_parse$readprocdecl((struct mm_decls$strec *)procowner,scope,fflang);
    mm_decls$assemmode = (i64)0;
    mm_parse$checkequals();
    mm_lex$lex();
    startline = mm_parse$getcurrline();
    if (!(!!(shortfun))) {
        closesym = mm_parse$checkbegin((i64)0);
    }
;
    mm_parse$pushproc((struct mm_decls$strec *)stproc);
    mm_lib$nextavindex = (i64)0;
    if (!!(mm_parse$dretvar)) {
        stname = (struct mm_decls$strec *)mm_lib$getduplnameptr((struct mm_decls$strec *)stproc,(struct mm_decls$strec *)mm_parse$dretvar,(i64)12);
        mm_lib$storemode((struct mm_decls$strec *)procowner,(i64)(*stproc).mode,&(*stname).mode);
        mm_lib$adddef((struct mm_decls$strec *)stproc,(struct mm_decls$strec *)stname);
    }
;
    mm_lib$addtoproclist((struct mm_decls$strec *)stproc);
    if (!!(shortfun)) {
        (*stproc).code = mm_parse$readunit();
        mm_parse$checksymbol((i64)6);
        mm_lex$lex();
    }
    else {
        (*stproc).code = mm_parse$readsunit((i64)0);
        mm_parse$checkbeginend(closesym,kwd,startline);
    }
;
    (*stproc).code = mm_parse$makeblock((*stproc).code);
    if (((!!(mm_decls$ndocstrings) && !!(mm_parse$docfile)) && ((i64)(*stproc).scope >= (i64)2))) {
        msysc$m_print_startfile(mm_parse$docfile);
        msysc$m_print_str((byte*)"proc",NULL);
        msysc$m_print_str((*stproc).name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        for (i=(i64)1;i<=mm_decls$ndocstrings;++i) {
L587 :;
            msysc$m_print_startfile(mm_parse$docfile);
            msysc$m_print_str(mm_decls$docstrings[(i)-1],NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            mlib$pcm_free((void *)mm_decls$docstrings[(i)-1],strlen((mm_decls$docstrings[(i)-1] + (i64)1)));
L588 :;
        }
L589 :;
        ;
        msysc$m_print_startfile(mm_parse$docfile);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        mm_decls$ndocstrings = (i64)0;
    }
;
    mm_parse$popproc();
}

struct mm_decls$strec *mm_parse$readprocdecl(struct mm_decls$strec *procowner,i64 scope,i64 fflang) {
        i64 kwd;
        i64 varparams;
        i64 try_level;
        i64 nparams;
        i64 nretvalues;
        i64 isthreaded;
        i64 retmodes[4];
        u8 *  metadata;
        u8 *  truename;
        struct mm_decls$strec *  pequiv;
        struct mm_decls$strec *  stproc;
        struct mm_decls$strec *  owner;
        struct mm_decls$strec *  paramlist;
        struct mm_decls$strec *  nameptr;
    kwd = (i64)mm_decls$lx.symbol;
    isthreaded = (i64)((i64)mm_decls$lx.subcode == (i64)2);
    pequiv = 0;
    metadata = (byte*)"";
    truename = 0;
    varparams = (i64)0;
    try_level = (i64)0;
    mm_lex$lex();
    if (((i64)mm_decls$lx.symbol == (i64)77)) {
        truename = mlib$pcm_copyheapstring(mm_decls$lx.svalue);
        mlib$convlcstring(mm_decls$lx.svalue);
        mm_decls$lx.symptr = (struct mm_decls$strec *)mm_lex$addnamestr(mm_decls$lx.svalue);
    }
    else {
        mm_parse$checksymbol((i64)81);
    }
;
    nameptr = (struct mm_decls$strec *)mm_decls$lx.symptr;
    stproc = (struct mm_decls$strec *)mm_lib$getduplnameptr((struct mm_decls$strec *)procowner,(struct mm_decls$strec *)nameptr,(!!(mm_parse$insidedllimport) ? (i64)7 : (i64)6));
    if (!!(mm_parse$insidedllimport)) {
        scope = (i64)1;
        (*stproc).dllindex = mm_parse$insidedllimport;
    }
;
    (*stproc).isthreaded = isthreaded;
    if (!!(truename)) {
        (*stproc).truename = truename;
    }
;
    mm_lib$adddef((struct mm_decls$strec *)procowner,(struct mm_decls$strec *)stproc);
    if (((i64)(*stproc).nameid == (i64)7)) {
        (*stproc).flags = msysc$m_setdotindex((*stproc).flags,(i64)12,(u64)1u);
    }
;
    owner = stproc;
    mm_parse$pushproc((struct mm_decls$strec *)stproc);
    mm_lex$lex();
    paramlist = 0;
    retmodes[((i64)1)-1] = (i64)0;
    nparams = (i64)0;
    nretvalues = (i64)0;
    nretvalues = (i64)0;
    if (((i64)mm_decls$lx.symbol == (i64)13)) {
        mm_lex$lex();
        if (((i64)mm_decls$lx.symbol != (i64)14)) {
            paramlist = (struct mm_decls$strec *)mm_parse$readparams((struct mm_decls$strec *)procowner,(struct mm_decls$strec *)stproc,fflang,&varparams,&nparams);
            mm_parse$checksymbol((i64)14);
        }
;
        mm_lex$lex();
        if ((((i64)mm_decls$lx.symbol == (i64)7) || ((i64)mm_decls$lx.symbol == (i64)11))) {
            mm_lex$lex();
            nretvalues = mm_parse$readreturntype((struct mm_decls$strec *)owner,&retmodes);
        }
        else if ((!!((i64)mm_decls$typestarterset[((i64)mm_decls$lx.symbol)]) || ((i64)mm_decls$lx.symbol == (i64)81))) {
            nretvalues = mm_parse$readreturntype((struct mm_decls$strec *)owner,&retmodes);
        }
;
    }
    else if ((((i64)mm_decls$lx.symbol == (i64)7) || ((i64)mm_decls$lx.symbol == (i64)11))) {
        mm_lex$lex();
        nretvalues = mm_parse$readreturntype((struct mm_decls$strec *)owner,&retmodes);
    }
;
    mm_parse$dretvar = 0;
    if ((nretvalues == (i64)1)) {
        if (((i64)mm_decls$lx.symbol == (i64)81)) {
            mm_parse$dretvar = (struct mm_decls$strec *)mm_decls$lx.symptr;
            mm_lex$lex();
        }
;
    }
;
    if (!((!!(nretvalues) || (kwd != (i64)131)))) {
        mm_support$serror((byte*)"Function needs ret type");
    }
;
    if ((!!(nretvalues) && (kwd != (i64)131))) {
        mm_support$serror((byte*)"Proc can't return value");
    }
;
    (*stproc).paramlist = (struct mm_decls$strec *)paramlist;
    (*stproc).nretvalues = nretvalues;
    if ((nretvalues==(i64)0)) {
        (*stproc).mode = (i64)0;
    }
    else if ((nretvalues==(i64)1)) {
        mm_lib$storemode((struct mm_decls$strec *)procowner,retmodes[((i64)1)-1],&(*stproc).mode);
    }
    else {
        (*stproc).mode = mm_lib$createtuplemode((struct mm_decls$strec *)procowner,&retmodes,nretvalues,(i64)0);
    }
;
    if (((i64)mm_decls$lx.symbol == (i64)22)) {
        mm_parse$lexchecksymbol((i64)81);
        mm_support$serror((byte*)"READPROCDEF @");
        mm_lex$lex();
        (*stproc).flags = msysc$m_setdotindex((*stproc).flags,(i64)10,(u64)1u);
    }
;
    (*stproc).code = 0;
    if ((fflang==(i64)2) || (fflang==(i64)1)) {
    }
    else {
                {i64 $temp = (i64)(*procowner).nameid;
if (($temp==(i64)3)) {
        }
        else if (($temp==(i64)4)) {
            mm_support$serror((byte*)"Need FF specifier");
        }
        };
    }
;
    (*stproc).scope = scope;
    (*stproc).varparams = varparams;
    (*stproc).fflang = fflang;
    if ((procowner == mm_decls$stmodule)) {
        if ((((i64)(*stproc).namelen == (i64)5) && !!(mlib$eqstring((*stproc).name,(byte*)"start")))) {
            mm_decls$moduletable[((i64)(*mm_decls$stmodule).moduleno)].ststart = (struct mm_decls$strec *)stproc;
            (*stproc).scope = (i64)1;
        }
        else if (((((i64)(*stproc).namelen == (i64)4) && !!(mlib$eqstring((*stproc).name,(byte*)"main"))) && ((i64)(*mm_decls$stmodule).moduleno == mm_decls$mainmoduleno))) {
            mm_decls$moduletable[((i64)(*mm_decls$stmodule).moduleno)].stmain = (struct mm_decls$strec *)stproc;
            (*stproc).scope = (i64)3;
        }
;
    }
;
    mm_parse$popproc();
    return (struct mm_decls$strec *)stproc;
}

static struct mm_decls$strec *mm_parse$readparams(struct mm_decls$strec *procowner,struct mm_decls$strec *owner,i64 fflang,i64 *varparams,i64 *nparams) {
        struct mm_decls$strec *  stlist;
        struct mm_decls$strec *  stlistx;
        struct mm_decls$strec *  stname;
        i64 parammode;
        i64 pmode;
        i64 m;
        i64 isoptional;
        i64 types;
        u8 str[32];
    stlist = (stlistx = 0);
    pmode = (i64)0;
    (*nparams) = (i64)0;
    parammode = (i64)0;
    types = (i64)0;
    if ((fflang == (i64)0)) {
        fflang = (i64)3;
    }
;
    if ((((i64)mm_decls$lx.symbol == (i64)81) && ((i64)mm_decls$nextlx.symbol == (i64)5 || (i64)mm_decls$nextlx.symbol == (i64)14))) {
        types = (i64)1;
    }
;
    L590 :;
    while (1) {
        parammode = (i64)0;
        isoptional = (i64)0;
        if ((!!(types) || !!(mm_parse$istypestarter()))) {
            pmode = mm_parse$readtypespec((struct mm_decls$strec *)procowner,(i64)0);
            //gotmode:
L592 :;
;
            if ((((*nparams) == (i64)0) && ((i64)mm_decls$lx.symbol == (i64)5 || (i64)mm_decls$lx.symbol == (i64)14))) {
                L593 :;
                while (1) {
                    ++((*nparams));
                    str[((i64)1)-1] = '$';
                    str[((i64)2)-1] = (u64)0u;
                    strcat(str,msysc$strint((*nparams),0));
                    stname = (struct mm_decls$strec *)mm_lib$getduplnameptr((struct mm_decls$strec *)owner,(struct mm_decls$strec *)mm_lex$addnamestr((u8 *)str),(i64)13);
                    mm_lib$adddef((struct mm_decls$strec *)owner,(struct mm_decls$strec *)stname);
                    mm_lib$storemode((struct mm_decls$strec *)owner,pmode,&(*stname).mode);
                    (*stname).parammode = parammode;
                    mm_parse$addlistparam((struct mm_decls$strec **)&stlist,(struct mm_decls$strec **)&stlistx,(struct mm_decls$strec *)stname);
                                        {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)14)) {
                        goto L594 ;
                    }
                    };
                    mm_parse$checksymbol((i64)5);
                    mm_lex$lex();
                    if (((i64)mm_decls$lx.symbol == (i64)29)) {
                        (*varparams) = ((*nparams) + (i64)1);
                        mm_lex$lex();
                        goto L594 ;
                    }
;
                    pmode = mm_parse$readtypespec((struct mm_decls$strec *)procowner,(i64)0);
                }
L594 :;
                ;
                return (struct mm_decls$strec *)stlist;
            }
;
        }
        else if ((pmode == (i64)0)) {
            mm_support$serror((byte*)"Type expected");
        }
;
                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)52)) {
            parammode = (i64)1;
            mm_lex$lex();
            if (((i64)mm_decls$lx.symbol == (i64)7)) {
                mm_lex$lex();
            }
;
        }
        else if (($temp==(i64)96) || ($temp==(i64)25)) {
            parammode = (i64)2;
            mm_lex$lex();
            if (((i64)mm_decls$lx.symbol == (i64)7)) {
                mm_lex$lex();
            }
;
        }
        else if (($temp==(i64)24)) {
            isoptional = (i64)1;
            mm_lex$lex();
        }
        else if (($temp==(i64)29)) {
            (*varparams) = (i64)1;
            mm_lex$lex();
            return (struct mm_decls$strec *)stlist;
        }
        };
        mm_parse$checksymbol((i64)81);
        ++((*nparams));
        stname = (struct mm_decls$strec *)mm_lib$getduplnameptr((struct mm_decls$strec *)owner,(struct mm_decls$strec *)mm_decls$lx.symptr,(i64)13);
        mm_lib$adddef((struct mm_decls$strec *)owner,(struct mm_decls$strec *)stname);
        mm_lex$lex();
        if ((parammode == (i64)2)) {
            m = mm_lib$createrefmode((struct mm_decls$strec *)procowner,pmode,(i64)0);
        }
        else {
            m = pmode;
        }
;
        mm_lib$storemode((struct mm_decls$strec *)owner,m,&(*stname).mode);
        (*stname).parammode = parammode;
        (*stname).optional = isoptional;
        mm_parse$addlistparam((struct mm_decls$strec **)&stlist,(struct mm_decls$strec **)&stlistx,(struct mm_decls$strec *)stname);
                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)9) || ($temp==(i64)48)) {
            mm_lex$lex();
            (*stname).code = mm_parse$readunit();
            (*stname).equals = (i64)1;
            (*stname).optional = (i64)1;
        }
        };
                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)5)) {
            mm_lex$lex();
        }
        else if (($temp==(i64)14)) {
            goto L591 ;
        }
        else {
            mm_support$serror((byte*)"nameparams1");
        }
        };
    }
L591 :;
    ;
    return (struct mm_decls$strec *)stlist;
}

static struct mm_decls$unitrec *mm_parse$readcondsuffix(struct mm_decls$unitrec *p) {
        struct mm_decls$unitrec *  q;
    switch ((i64)mm_decls$lx.symbol) {
    case 110:;
        {
            mm_lex$lex();
            return (struct mm_decls$unitrec *)mm_lib$createunit2((i64)90,(struct mm_decls$unitrec *)mm_parse$fixcond(mm_parse$readunit()),(struct mm_decls$unitrec *)mm_lib$createunit1((i64)4,(struct mm_decls$unitrec *)p));
        }
        break;
    case 106:;
        {
            mm_lex$lex();
            q = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)13,(struct mm_decls$unitrec *)mm_parse$fixcond(mm_parse$readunit()));
            (*q).pclop = (i64)32;
            return (struct mm_decls$unitrec *)mm_lib$createunit2((i64)90,(struct mm_decls$unitrec *)q,(struct mm_decls$unitrec *)mm_lib$createunit1((i64)4,(struct mm_decls$unitrec *)p));
        }
        break;
    default: {
        return p;
    }
    } //SW
;
}

static struct mm_decls$unitrec *mm_parse$readif(void) {
        i64 pos1;
        i64 kwd;
        struct mm_decls$unitrec *  clist;
        struct mm_decls$unitrec *  clistx;
        struct mm_decls$unitrec *  plist;
        struct mm_decls$unitrec *  plistx;
        struct mm_decls$unitrec *  pelse;
        struct mm_decls$unitrec *  p;
    pos1 = (i64)mm_decls$lx.pos;
    kwd = (i64)mm_decls$lx.symbol;
    clist = (clistx = (plist = (plistx = (pelse = 0))));
    L595 :;
    do {
        mm_lex$lex();
        mm_lib$addlistunit(&clist,&clistx,mm_parse$fixcond(mm_parse$readsunit((i64)0)));
        mm_parse$skipsemi();
        mm_parse$checksymbol((i64)99);
        mm_lex$lex();
        mm_lib$addlistunit(&plist,&plistx,mm_parse$readsunit((i64)0));
        mm_parse$skipsemi();
L596 :;
    }
    while (!((i64)mm_decls$lx.symbol != (i64)100));
L597 :;
    ;
        {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)101)) {
        mm_lex$lex();
        pelse = mm_parse$readsunit((i64)0);
        mm_parse$checkend((i64)105,kwd,(i64)0,(i64)0);
        mm_lex$lex();
    }
    else if (($temp==(i64)102) || ($temp==(i64)103)) {
        mm_decls$lx.symbol = kwd;
        pelse = mm_parse$makeblock(mm_parse$readswitchcase());
    }
    else {
        mm_parse$checkend((i64)105,kwd,(i64)0,(i64)0);
        mm_lex$lex();
    }
    };
    p = (struct mm_decls$unitrec *)mm_lib$createunit3((i64)90,(struct mm_decls$unitrec *)clist,(struct mm_decls$unitrec *)plist,(struct mm_decls$unitrec *)pelse);
    (*p).pos = pos1;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readgoto(i64 gototag) {
    if (((i64)mm_decls$lx.subcode == (i64)1)) {
        mm_parse$lexchecksymbol((i64)112);
    }
;
    mm_lex$lex();
    return mm_parse$readcondsuffix((struct mm_decls$unitrec *)mm_lib$createunit1(gototag,(struct mm_decls$unitrec *)mm_parse$readunit()));
}

static struct mm_decls$unitrec *mm_parse$readunless(void) {
        i64 pos;
        struct mm_decls$unitrec *  pcond;
        struct mm_decls$unitrec *  pthen;
        struct mm_decls$unitrec *  pelse;
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  q;
    pos = (i64)mm_decls$lx.pos;
    mm_lex$lex();
    pcond = mm_parse$fixcond(mm_parse$readsunit((i64)0));
    mm_parse$checksymbol((i64)99);
    mm_lex$lex();
    pthen = mm_parse$readsunit((i64)0);
    if (((i64)mm_decls$lx.symbol == (i64)101)) {
        mm_lex$lex();
        pelse = mm_parse$readsunit((i64)0);
    }
    else {
        pelse = 0;
    }
;
    mm_parse$checkend((i64)105,(i64)106,(i64)0,(i64)0);
    mm_lex$lex();
    p = (struct mm_decls$unitrec *)mm_lib$createunit3((i64)90,(struct mm_decls$unitrec *)(q = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)13,(struct mm_decls$unitrec *)pcond)),(struct mm_decls$unitrec *)pthen,(struct mm_decls$unitrec *)pelse);
    (*q).pclop = (i64)32;
    (*p).pos = pos;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readswitchcase(void) {
        i64 pos1;
        i64 kwd;
        i64 opc;
        i64 pos2;
        i64 rangeused;
        i64 nwhen;
        struct mm_decls$unitrec *  pexpr;
        struct mm_decls$unitrec *  pwhenlist;
        struct mm_decls$unitrec *  pwhenlistx;
        struct mm_decls$unitrec *  pwhen;
        struct mm_decls$unitrec *  pwhenx;
        struct mm_decls$unitrec *  pelse;
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  pthen;
        struct mm_decls$unitrec *  pwhenthen;
    pos1 = (i64)mm_decls$lx.pos;
    kwd = (i64)mm_decls$lx.symbol;
    opc = (i64)mm_decls$lx.subcode;
    mm_lex$lex();
    mm_parse$skipsemi();
    if (((i64)mm_decls$lx.symbol == (i64)110)) {
        if ((kwd == (i64)123)) {
            mm_support$serror((byte*)"switch expr missing");
        }
;
        pexpr = 0;
    }
    else {
        pexpr = mm_parse$readsunit((i64)0);
    }
;
    pwhenlist = (pwhenlistx = 0);
    rangeused = (i64)0;
    nwhen = (i64)0;
    mm_parse$skipsemi();
    L598 :;
    while (((i64)mm_decls$lx.symbol == (i64)110)) {
        pos2 = (i64)mm_decls$lx.pos;
        mm_lex$lex();
        pwhen = (pwhenx = 0);
        L601 :;
        while (1) {
            p = mm_parse$readunit();
            ++(nwhen);
            (*p).pos = pos2;
            if (((i64)(*p).tag == (i64)16)) {
                rangeused = (i64)1;
            }
;
            mm_lib$addlistunit(&pwhen,&pwhenx,p);
            if (((i64)mm_decls$lx.symbol != (i64)5)) {
                goto L602 ;
            }
;
            mm_lex$lex();
        }
L602 :;
        ;
        if (((i64)mm_decls$lx.symbol != (i64)11)) {
            mm_parse$checksymbol((i64)99);
        }
;
        mm_lex$lex();
        pthen = mm_parse$readsunit((i64)0);
        pwhenthen = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)82,(struct mm_decls$unitrec *)pwhen,(struct mm_decls$unitrec *)pthen);
        (*pwhenthen).pos = pos2;
        mm_lib$addlistunit(&pwhenlist,&pwhenlistx,pwhenthen);
L599 :;
    }
L600 :;
    ;
    if (((opc == (i64)105) && !(!!(rangeused)))) {
        if ((nwhen <= (i64)8)) {
        }
;
    }
;
        {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)101)) {
        mm_lex$lex();
        pelse = mm_parse$readsunit((i64)0);
        mm_parse$checkend((i64)105,kwd,(i64)0,(i64)0);
        mm_lex$lex();
    }
    else if (($temp==(i64)100)) {
        mm_decls$lx.symbol = kwd;
        pelse = mm_parse$makeblock(mm_parse$readif());
    }
    else if (($temp==(i64)102) || ($temp==(i64)103)) {
        mm_decls$lx.symbol = kwd;
        pelse = mm_parse$makeblock(mm_parse$readswitchcase());
    }
    else {
        pelse = 0;
        mm_parse$checkend((i64)105,kwd,(i64)0,(i64)0);
        mm_lex$lex();
    }
    };
    p = (struct mm_decls$unitrec *)mm_lib$createunit3(opc,(struct mm_decls$unitrec *)pexpr,(struct mm_decls$unitrec *)pwhenlist,(struct mm_decls$unitrec *)pelse);
    (*p).pos = pos1;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readstop(void) {
        struct mm_decls$unitrec *  p;
    mm_lex$lex();
    if (!!((i64)mm_tables$exprstarter[((i64)mm_decls$lx.symbol)-1])) {
        p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)120,(struct mm_decls$unitrec *)mm_parse$readunit());
    }
    else {
        p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)120);
    }
;
    return mm_parse$readcondsuffix(p);
}

static struct mm_decls$unitrec *mm_parse$readreturn(void) {
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  q;
    mm_lex$lex();
    if (!!((i64)mm_tables$exprstarter[((i64)mm_decls$lx.symbol)-1])) {
        q = mm_parse$readunit();
        p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)87,(struct mm_decls$unitrec *)q);
        (*p).length = (i64)1;
    }
    else {
        p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)87);
        (*p).length = (i64)0;
    }
;
    return mm_parse$readcondsuffix(p);
}

static struct mm_decls$unitrec *mm_parse$readdo(void) {
        struct mm_decls$unitrec *  p;
        i64 pos;
    pos = (i64)mm_decls$lx.pos;
    mm_lex$lex();
    p = mm_parse$readsunit((i64)0);
    mm_parse$checkend((i64)105,(i64)114,(i64)0,(i64)0);
    mm_lex$lex();
    p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)102,(struct mm_decls$unitrec *)p);
    (*p).pos = pos;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readto(void) {
        i64 pos;
        i64 id;
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  pcount;
        struct mm_decls$unitrec *  pbody;
    pos = (i64)mm_decls$lx.pos;
    mm_lex$lex();
    pcount = mm_parse$readunit();
    mm_parse$checksymbol((i64)114);
    mm_lex$lex();
    pbody = mm_parse$readsunit((i64)0);
    mm_parse$checkend((i64)105,(i64)112,(i64)114,(i64)0);
    mm_lex$lex();
    id = (i64)12;
    if (((i64)(*mm_decls$currproc).nameid != (i64)6)) {
        id = (i64)11;
    }
;
    p = (struct mm_decls$unitrec *)mm_lib$createunit3((i64)89,(struct mm_decls$unitrec *)pcount,(struct mm_decls$unitrec *)pbody,(struct mm_decls$unitrec *)mm_lib$createname(mm_lib$getavname(mm_decls$currproc,id)));
    (*p).pos = pos;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readwhile(void) {
        i64 pos;
        struct mm_decls$unitrec *  pcond;
        struct mm_decls$unitrec *  pbody;
        struct mm_decls$unitrec *  pincr;
        struct mm_decls$unitrec *  p;
    pos = (i64)mm_decls$lx.pos;
    mm_lex$lex();
    pcond = mm_parse$fixcond(mm_parse$readsunit((i64)1));
    pincr = 0;
    if (((i64)mm_decls$lx.symbol == (i64)5)) {
        mm_lex$lex();
        pincr = mm_parse$readsunit((i64)1);
    }
;
    mm_parse$checksymbol((i64)114);
    mm_lex$lex();
    pbody = mm_parse$readsunit((i64)0);
    if (((i64)mm_decls$lx.symbol == (i64)121)) {
        if (!!(pincr)) {
            mm_support$serror((byte*)"Double incr");
        }
;
        mm_lex$lex();
        pincr = mm_parse$readsunit((i64)0);
    }
;
    mm_parse$checkend((i64)105,(i64)115,(i64)114,(i64)0);
    mm_lex$lex();
    p = (struct mm_decls$unitrec *)mm_lib$createunit3((i64)95,(struct mm_decls$unitrec *)pcond,(struct mm_decls$unitrec *)pbody,(struct mm_decls$unitrec *)pincr);
    (*p).pos = pos;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readrepeat(void) {
        i64 pos;
        struct mm_decls$unitrec *  pbody;
        struct mm_decls$unitrec *  pcond;
        struct mm_decls$unitrec *  p;
    pos = (i64)mm_decls$lx.pos;
    mm_lex$lex();
    pbody = mm_parse$readsunit((i64)0);
    mm_parse$checksymbol((i64)117);
    mm_lex$lex();
    pcond = mm_parse$fixcond(mm_parse$readunit());
    p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)96,(struct mm_decls$unitrec *)pbody,(struct mm_decls$unitrec *)pcond);
    (*p).pos = pos;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readloopcontrol(void) {
        i64 opc;
        struct mm_decls$unitrec *  p;
    opc = (i64)mm_decls$lx.subcode;
    mm_lex$lex();
    if ((((i64)mm_decls$lx.symbol == (i64)81) && !!(mlib$eqstring((*mm_decls$lx.symptr).name,(byte*)"all")))) {
        mm_lex$lex();
        p = (struct mm_decls$unitrec *)mm_lib$createunit1(opc,(struct mm_decls$unitrec *)mm_lib$createconstunit((u64)0u,(i64)3));
    }
    else if (!!((i64)mm_tables$exprstarter[((i64)mm_decls$lx.symbol)-1])) {
        p = (struct mm_decls$unitrec *)mm_lib$createunit1(opc,(struct mm_decls$unitrec *)mm_parse$readconstexpr((i64)1));
    }
    else {
        p = (struct mm_decls$unitrec *)mm_lib$createunit1(opc,(struct mm_decls$unitrec *)mm_lib$createconstunit((u64)1u,(i64)3));
    }
;
    return mm_parse$readcondsuffix(p);
}

static struct mm_decls$unitrec *mm_parse$readprint(void) {
        i64 oldinreadprint;
        i64 opc;
        i64 isfprint;
        i64 fshowname;
        struct mm_decls$unitrec *  pformat;
        struct mm_decls$unitrec *  pdev;
        struct mm_decls$unitrec *  printlist;
        struct mm_decls$unitrec *  printlistx;
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  q;
        struct mlib$strbuffer *  expr;
        u8 *  s;
    oldinreadprint = mm_parse$inreadprint;
    mm_parse$inreadprint = (i64)1;
    opc = (i64)mm_decls$lx.subcode;
    if ((opc==(i64)112) || (opc==(i64)113)) {
        isfprint = (i64)1;
    }
    else {
        isfprint = (i64)0;
    }
;
    mm_lex$lex();
    printlist = (printlistx = 0);
    pformat = (pdev = 0);
    if (((i64)mm_decls$lx.symbol == (i64)22)) {
        mm_lex$lex();
        pdev = mm_parse$readunit();
        if (((i64)mm_decls$lx.symbol == (i64)5)) {
            mm_lex$lex();
        }
        else {
            goto L603 ;
;
        }
;
    }
;
    if (!!(isfprint)) {
        pformat = mm_parse$readunit();
        if (((i64)mm_decls$lx.symbol == (i64)5)) {
            mm_lex$lex();
        }
        else {
            goto L603 ;
;
        }
;
    }
;
    if (!(!!((i64)mm_tables$exprstarter[((i64)mm_decls$lx.symbol)-1]))) {
        goto L603 ;
;
    }
;
    L604 :;
    while (1) {
                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)5)) {
            mm_lib$addlistunit(&printlist,&printlistx,(struct mm_decls$unitrec *)mm_lib$createunit0((i64)84));
        }
        else if (($temp==(i64)160)) {
            mm_lib$addlistunit(&printlist,&printlistx,(struct mm_decls$unitrec *)mm_lib$createunit0((i64)85));
            mm_lex$lex();
        }
        else {
            fshowname = (i64)0;
            if (((i64)mm_decls$lx.symbol == (i64)48)) {
                fshowname = (i64)1;
                mm_lex$lex();
            }
;
            p = mm_parse$readunit();
            if (((i64)mm_decls$lx.symbol == (i64)7)) {
                mm_lex$lex();
                p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)83,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)mm_parse$readunit());
            }
;
            if (!!(fshowname)) {
                expr = (struct mlib$strbuffer *)mm_lib$strexpr((struct mm_decls$unitrec *)p);
                mlib$strbuffer_add((struct mlib$strbuffer *)expr,(byte*)"=",(i64)-1);
                s = (*expr).strptr;
                mlib$iconvucn((*expr).strptr,(i64)(*expr).length);
                mm_lib$addlistunit(&printlist,&printlistx,(q = (struct mm_decls$unitrec *)mm_lib$createstringconstunit(s,(i64)(*expr).length)));
            }
;
            mm_lib$addlistunit(&printlist,&printlistx,p);
        }
        };
        if (((i64)mm_decls$lx.symbol != (i64)5)) {
            goto L605 ;
        }
;
        mm_lex$lex();
    }
L605 :;
    ;
    //finish:
L603 :;
;
    mm_parse$inreadprint = oldinreadprint;
    if (((opc == (i64)110) && (printlist == 0))) {
        mm_support$serror((byte*)"No print items");
    }
;
    if ((((opc == (i64)112) && (printlist == 0)) && (pformat == 0))) {
        mm_support$serror((byte*)"No print items");
    }
;
    if (!!(isfprint)) {
        if ((pformat == 0)) {
            mm_support$serror((byte*)"No fmt str");
        }
;
        return (struct mm_decls$unitrec *)mm_lib$createunit3(opc,(struct mm_decls$unitrec *)pdev,(struct mm_decls$unitrec *)pformat,(struct mm_decls$unitrec *)printlist);
    }
    else {
        return (struct mm_decls$unitrec *)mm_lib$createunit2(opc,(struct mm_decls$unitrec *)pdev,(struct mm_decls$unitrec *)printlist);
    }
;
}

static struct mm_decls$unitrec *mm_parse$readread(void) {
        i64 oldinreadprint;
        i64 opc;
        struct mm_decls$unitrec *  pformat;
        struct mm_decls$unitrec *  pdev;
        struct mm_decls$unitrec *  readlist;
        struct mm_decls$unitrec *  readlistx;
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  pread;
    oldinreadprint = mm_parse$inreadprint;
    mm_parse$inreadprint = (i64)1;
    opc = (i64)mm_decls$lx.subcode;
    mm_lex$lex();
    readlist = (readlistx = 0);
    pformat = (pdev = 0);
    if (((i64)mm_decls$lx.symbol == (i64)22)) {
        if ((opc == (i64)116)) {
            mm_support$serror((byte*)"@ on read");
        }
;
        mm_lex$lex();
        pdev = mm_parse$readunit();
        if (((i64)mm_decls$lx.symbol == (i64)5)) {
            mm_lex$lex();
        }
;
    }
;
    if ((opc == (i64)117)) {
        mm_lib$addlistunit(&readlist,&readlistx,(struct mm_decls$unitrec *)mm_lib$createunit1((i64)117,(struct mm_decls$unitrec *)pdev));
    }
;
    if (!(!!((i64)mm_tables$exprstarter[((i64)mm_decls$lx.symbol)-1]))) {
        goto L606 ;
;
    }
;
    L607 :;
    while (1) {
        p = mm_parse$readunit();
        if (((i64)mm_decls$lx.symbol == (i64)7)) {
            mm_lex$lex();
            pformat = mm_parse$readunit();
        }
        else {
            pformat = 0;
        }
;
        pread = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)116,(struct mm_decls$unitrec *)pformat);
        p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)23,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)pread);
        mm_lib$addlistunit(&readlist,&readlistx,p);
        if (((i64)mm_decls$lx.symbol != (i64)5)) {
            goto L608 ;
        }
;
        mm_lex$lex();
    }
L608 :;
    ;
    //finish:
L606 :;
;
    mm_parse$inreadprint = oldinreadprint;
    if (((opc == (i64)116) && (readlist == 0))) {
        mm_support$serror((byte*)"No read items");
    }
;
    if (!!((*readlist).nextunit)) {
        return (struct mm_decls$unitrec *)mm_lib$createunit1((i64)4,(struct mm_decls$unitrec *)readlist);
    }
    else {
        return readlist;
    }
;
}

static struct mm_decls$unitrec *mm_parse$readfor(void) {
        i64 pos;
        i64 opc;
        struct mm_decls$unitrec *  pindex;
        struct mm_decls$unitrec *  plocal;
        struct mm_decls$unitrec *  pfrom;
        struct mm_decls$unitrec *  pto;
        struct mm_decls$unitrec *  pstep;
        struct mm_decls$unitrec *  ptoinit;
        struct mm_decls$unitrec *  plist;
        struct mm_decls$unitrec *  passign;
        struct mm_decls$unitrec *  pcond;
        struct mm_decls$unitrec *  pbody;
        struct mm_decls$unitrec *  pelse;
        struct mm_decls$unitrec *  p;
        i64 i;
    pos = (i64)mm_decls$lx.pos;
    mm_lex$lex();
    plocal = 0;
    ptoinit = 0;
    pindex = mm_parse$readname();
    if ((mm_parse$nforloops >= (i64)10)) {
        mm_support$serror((byte*)"Too many for-loops");
    }
;
    for (i=(i64)1;i<=mm_parse$nforloops;++i) {
L609 :;
        if ((mm_parse$forindexvars[(i)-1] == (*pindex).def)) {
            mm_support$serror((byte*)"Re-using nested loop index");
        }
;
L610 :;
    }
L611 :;
    ;
    mm_parse$forindexvars[(++(mm_parse$nforloops))-1] = (struct mm_decls$strec *)(*pindex).def;
    if (((i64)mm_decls$lx.symbol == (i64)5)) {
        mm_lex$lex();
        plocal = mm_parse$readname();
    }
;
    opc = (i64)91;
    pstep = 0;
    pcond = 0;
    if (((i64)mm_decls$lx.symbol == (i64)52 || (i64)mm_decls$lx.symbol == (i64)54)) {
        if (((i64)mm_decls$lx.symbol == (i64)36)) {
            opc = (i64)92;
        }
;
        mm_lex$lex();
        plist = mm_parse$readunit();
        if ((((i64)(*plist).tag == (i64)32) && ((i64)(*plist).pclop == (i64)91))) {
            pfrom = (struct mm_decls$unitrec *)mm_lib$getrangelwbunit((struct mm_decls$unitrec *)(*plist).a);
            pto = (struct mm_decls$unitrec *)mm_lib$getrangeupbunit((struct mm_decls$unitrec *)(*plist).a);
        }
        else if (((i64)(*plist).tag == (i64)16)) {
            pfrom = (*plist).a;
            pto = (*plist).b;
        }
        else {
            opc = ((opc == (i64)91) ? (i64)93 : (i64)94);
            pfrom = (struct mm_decls$unitrec *)mm_lib$getrangelwbunit((struct mm_decls$unitrec *)mm_lib$duplunit(plist,(i64)0));
            pto = (struct mm_decls$unitrec *)mm_lib$getrangeupbunit((struct mm_decls$unitrec *)mm_lib$duplunit(plist,(i64)0));
        }
;
    }
    else {
        if (((i64)mm_decls$lx.symbol == (i64)9)) {
            mm_lex$lex();
            pfrom = mm_parse$readunit();
        }
        else {
            pfrom = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)1u,(i64)3);
        }
;
        mm_parse$checksymbol((i64)112);
        opc = (((i64)mm_decls$lx.subcode == (i64)1) ? (i64)92 : (i64)91);
        mm_lex$lex();
        pto = mm_parse$readunit();
        if (((i64)mm_decls$lx.symbol == (i64)113)) {
            mm_lex$lex();
            pstep = mm_parse$readconstexpr((i64)0);
            if (((i64)(*pstep).tag == (i64)1)) {
                if (((*pstep).value == (i64)1)) {
                    pstep = 0;
                }
;
            }
;
        }
;
    }
;
    if (((i64)mm_decls$lx.symbol == (i64)110)) {
        mm_lex$lex();
        pcond = mm_parse$fixcond(mm_parse$readunit());
    }
;
    mm_parse$checksymbol((i64)114);
    mm_lex$lex();
    pbody = mm_parse$readsunit((i64)0);
    pelse = 0;
    if (((i64)mm_decls$lx.symbol == (i64)101)) {
        mm_lex$lex();
        pelse = mm_parse$readsunit((i64)0);
    }
;
    mm_parse$checkend((i64)105,(i64)111,(i64)114,(i64)0);
    mm_lex$lex();
    if ((pcond != 0)) {
        pbody = mm_parse$makeblock((struct mm_decls$unitrec *)mm_lib$createunit2((i64)90,(struct mm_decls$unitrec *)pcond,(struct mm_decls$unitrec *)pbody));
    }
;
    (*pbody).nextunit = pelse;
    if ((opc==(i64)91) || (opc==(i64)92)) {
        if (!!(plocal)) {
            mm_support$serror((byte*)"for i,x?");
        }
;
        (*pindex).avcode = (i64)73;
        if (!(((i64)(*pto).tag == (i64)1 || (i64)(*pto).tag == (i64)3))) {
            plocal = (struct mm_decls$unitrec *)mm_lib$createname(mm_lib$getavname(mm_decls$currproc,(i64)12));
            (*plocal).avcode = (i64)73;
            ptoinit = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)23,(struct mm_decls$unitrec *)plocal,(struct mm_decls$unitrec *)pto);
            (*pindex).nextunit = ptoinit;
            pto = plocal;
        }
;
        (*pfrom).nextunit = pto;
        (*pto).nextunit = pstep;
        p = (struct mm_decls$unitrec *)mm_lib$createunit3(opc,(struct mm_decls$unitrec *)pindex,(struct mm_decls$unitrec *)pfrom,(struct mm_decls$unitrec *)pbody);
    }
    else {
        if ((plocal == 0)) {
            plocal = pindex;
            pindex = (struct mm_decls$unitrec *)mm_lib$createname(mm_lib$getavname(mm_decls$currproc,(i64)12));
        }
;
        (*pindex).avcode = (i64)73;
        (*plocal).avcode = (i64)76;
        (*pindex).nextunit = plocal;
        (*plocal).nextunit = pfrom;
        (*pfrom).nextunit = pto;
        passign = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)23,(struct mm_decls$unitrec *)mm_lib$duplunit(plocal,(i64)0),(struct mm_decls$unitrec *)mm_lib$createunit2((i64)40,(struct mm_decls$unitrec *)mm_lib$duplunit(plist,(i64)0),(struct mm_decls$unitrec *)mm_lib$duplunit(pindex,(i64)0)));
        (*plist).nextunit = passign;
        p = (struct mm_decls$unitrec *)mm_lib$createunit3(opc,(struct mm_decls$unitrec *)pindex,(struct mm_decls$unitrec *)plist,(struct mm_decls$unitrec *)pbody);
    }
;
    (*p).pos = pos;
    --(mm_parse$nforloops);
    return p;
}

static struct mm_decls$unitrec *mm_parse$readname(void) {
        struct mm_decls$unitrec *  p;
    p = mm_parse$readterm2();
    if (((i64)(*p).tag != (i64)3)) {
        mm_support$serror((byte*)"Name expected");
    }
;
    return p;
}

void mm_parse$readtypedef(struct mm_decls$strec *owner,i64 scope) {
        struct mm_decls$strec *  sttype;
        struct mm_decls$strec *  stname;
        i64 t;
        i64 m;
    mm_parse$lexchecksymbol((i64)81);
    stname = (struct mm_decls$strec *)mm_decls$lx.symptr;
    mm_lex$lex();
    mm_parse$checkequals();
    mm_lex$lex();
    sttype = (struct mm_decls$strec *)mm_lib$getduplnameptr((struct mm_decls$strec *)owner,(struct mm_decls$strec *)stname,(i64)5);
    mm_lib$adddef((struct mm_decls$strec *)owner,(struct mm_decls$strec *)sttype);
    m = mm_lib$createusertype((struct mm_decls$strec *)sttype);
    mm_decls$ttusercat[(m)] = (i64)1;
    t = mm_parse$readtypespec((struct mm_decls$strec *)sttype,m);
    (*sttype).scope = scope;
    mm_lib$storemode((struct mm_decls$strec *)owner,t,&(*sttype).mode);
    if ((t >= (i64)0)) {
        if (!!(((i64)mm_decls$ttisinteger[(t)] + (i64)mm_decls$ttisreal[(t)]))) {
            mm_decls$tttarget[(m)] = t;
        }
        else if (!!((i64)mm_decls$ttisref[(t)])) {
        }
        else {
                        {i64 $temp = (i64)mm_decls$ttbasetype[(t)];
if (($temp==(i64)10)) {
            }
            else if (($temp==(i64)11)) {
            }
            else if (($temp==(i64)8)) {
            }
            else {
                mm_decls$tttarget[(m)] = t;
            }
            };
        }
;
    }
    else {
        mm_lib$storemode((struct mm_decls$strec *)owner,t,&mm_decls$tttarget[(m)]);
    }
;
    if ((t >= (i64)0)) {
        mm_lib$copyttvalues(m,t);
    }
    else {
        mm_decls$ttbasetype[(m)] = (i64)28;
    }
;
}

void mm_parse$readrecordfields(struct mm_decls$strec *owner,i64 m) {
        i64 nvars;
        i64 offset;
        struct mm_decls$strec *  stname;
        struct mm_decls$strec *  stbitfield;
    nvars = (i64)0;
    L612 :;
    while (((i64)mm_decls$lx.symbol == (i64)81)) {
        stname = (struct mm_decls$strec *)mm_lib$getduplnameptr((struct mm_decls$strec *)owner,(struct mm_decls$strec *)mm_decls$lx.symptr,(i64)14);
        mm_lib$storemode((struct mm_decls$strec *)owner,m,&(*stname).mode);
        ++(nvars);
        if (!!((i64)mm_parse$unionpend.ulength)) {
            mm_lib$unionstr_copy((struct mm_decls$uflagsrec *)&(*stname).uflags,(struct mm_decls$uflagsrec *)&mm_parse$unionpend);
            mm_lib$unionstr_concat((struct mm_decls$uflagsrec *)&mm_parse$unionstring,(struct mm_decls$uflagsrec *)&mm_parse$unionpend);
            mm_lib$unionstr_clear(&mm_parse$unionpend);
        }
        else {
            mm_lib$unionstr_clear(&(*stname).uflags);
        }
;
        mm_parse$unionlastvar = (struct mm_decls$strec *)stname;
        mm_lib$adddef((struct mm_decls$strec *)owner,(struct mm_decls$strec *)stname);
        mm_lex$lex();
                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)22)) {
            mm_lex$lex();
            (*stname).flags = msysc$m_setdotindex((*stname).flags,(i64)9,(u64)1u);
            (*stname).equivfield = (struct mm_decls$strec *)mm_parse$readequivfield((struct mm_decls$strec *)owner);
            if (((i64)mm_decls$lx.symbol == (i64)31)) {
                mm_lex$lex();
                offset = mm_parse$readconstint();
                if ((offset > (i64)255)) {
                    mm_support$serror((byte*)"Offset>255");
                }
;
                (*stname).equivoffset = offset;
            }
;
        }
        else if (($temp==(i64)23)) {
            mm_parse$lexchecksymbol((i64)72);
                        {i64 $temp = mm_decls$lx.value;
if (($temp==(i64)1) || ($temp==(i64)2) || ($temp==(i64)4) || ($temp==(i64)8)) {
                (*stname).align = mm_decls$lx.value;
            }
            else if (($temp==(i64)0)) {
                (*stname).align = (i64)255;
            }
            else {
                mm_support$serror((byte*)"@@ bad align");
            }
            };
            mm_lex$lex();
        }
        else if (($temp==(i64)7)) {
            mm_parse$lexchecksymbol((i64)13);
            L615 :;
            do {
                mm_parse$lexchecksymbol((i64)81);
                stbitfield = (struct mm_decls$strec *)mm_lib$getduplnameptr((struct mm_decls$strec *)owner,(struct mm_decls$strec *)mm_decls$lx.symptr,(i64)14);
                (*stbitfield).mode = (i64)26;
                mm_lib$adddef((struct mm_decls$strec *)owner,(struct mm_decls$strec *)stbitfield);
                (*stbitfield).flags = msysc$m_setdotindex((*stbitfield).flags,(i64)9,(u64)1u);
                (*stbitfield).equivfield = (struct mm_decls$strec *)stname;
                mm_parse$lexchecksymbol((i64)7);
                mm_parse$lexchecksymbol((i64)72);
                (*stbitfield).bitfieldwidth = mm_decls$lx.value;
                mm_lex$lex();
L616 :;
            }
            while (!((i64)mm_decls$lx.symbol != (i64)5));
L617 :;
            ;
            mm_parse$checksymbol((i64)14);
            mm_lex$lex();
        }
        };
        if (((i64)mm_decls$lx.symbol != (i64)5)) {
            goto L614 ;
        }
;
        mm_lex$lex();
L613 :;
    }
L614 :;
    ;
    if ((nvars == (i64)0)) {
        mm_support$serror((byte*)"No fields declared");
    }
;
}

void mm_parse$readtabledef(struct mm_decls$strec *owner,i64 scope) {
        i64 i;
        i64 ncols;
        i64 nrows;
        i64 enums;
        i64 nextenumvalue;
        i64 firstval;
        i64 lastval;
        i64 startline;
        i64 closesym;
        i64 ltype;
        struct mm_decls$strec *  stvar;
        struct mm_decls$strec *  stenum;
        struct mm_decls$strec *  stgen;
        struct mm_decls$strec *  varnameptrs[20];
        i64 varlisttypes[20];
        struct mm_decls$unitrec *  plist[20];
        struct mm_decls$unitrec *  plistx[20];
        i64 enumvalues[500];
    enums = (i64)mm_decls$lx.subcode;
    mm_lex$lex();
    mm_parse$tabledataname = 0;
    if (((i64)mm_decls$lx.symbol == (i64)13)) {
        if (!(!!(enums))) {
            mm_support$serror((byte*)"use 'enumdata'");
        }
;
        enums = (i64)1;
        mm_lex$lex();
        mm_parse$checksymbol((i64)14);
        mm_lex$lex();
    }
;
    nextenumvalue = (i64)1;
    nrows = (i64)0;
    ncols = (i64)0;
    L618 :;
    while (((i64)mm_decls$lx.symbol != (i64)48)) {
        ltype = mm_parse$readtypespec((struct mm_decls$strec *)owner,(i64)0);
        mm_parse$checksymbol((i64)81);
        if ((++(ncols) > (i64)20)) {
            mm_support$serror((byte*)"tabledata/too many columns");
        }
;
        varnameptrs[(ncols)-1] = (struct mm_decls$strec *)mm_decls$lx.symptr;
        varlisttypes[(ncols)-1] = ltype;
        mm_lex$lex();
        if (((i64)mm_decls$lx.symbol == (i64)5)) {
            mm_lex$lex();
        }
        else {
            goto L620 ;
        }
;
L619 :;
    }
L620 :;
    ;
    mm_lex$lex();
    mm_parse$skipsemi();
    startline = mm_parse$getcurrline();
    closesym = mm_parse$checkbegin((i64)0);
    mm_parse$skipsemi();
    firstval = (lastval = (i64)0);
    for (i=(i64)1;i<=ncols;++i) {
L621 :;
        plist[(i)-1] = (plistx[(i)-1] = 0);
L622 :;
    }
L623 :;
    ;
    mm_parse$intabledata = (i64)1;
    L624 :;
    while (1) {
        mm_parse$skipsemi();
        if ((ncols > (i64)0)) {
            mm_parse$checksymbol((i64)13);
            mm_lex$lex();
        }
;
        if ((++(nrows) > (i64)500)) {
            mm_support$serror((byte*)"tabledata:too many rows");
        }
;
        if (!!(enums)) {
            mm_parse$checksymbol((i64)81);
            stgen = (struct mm_decls$strec *)mm_decls$lx.symptr;
            mm_parse$tabledataname = (*stgen).name;
            mm_lex$lex();
            if (((i64)mm_decls$lx.symbol == (i64)48)) {
                if ((nrows > (i64)1)) {
                    mm_support$serror((byte*)"tabledata '=' not 1st");
                }
;
                mm_lex$lex();
                nextenumvalue = mm_parse$readconstint();
            }
;
            enumvalues[(nrows)-1] = nextenumvalue;
            stenum = (struct mm_decls$strec *)mm_lib$getduplnameptr((struct mm_decls$strec *)owner,(struct mm_decls$strec *)stgen,(i64)10);
            (*stenum).mode = (i64)3;
            (*stenum).code = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)nextenumvalue,(i64)3);
            (*stenum).scope = scope;
            mm_lib$adddef((struct mm_decls$strec *)owner,(struct mm_decls$strec *)stenum);
            if ((scope == (i64)3)) {
                mm_lib$addexpconst((struct mm_decls$strec *)stenum);
            }
;
            if ((nrows == (i64)1)) {
                firstval = nextenumvalue;
            }
;
            lastval = nextenumvalue;
            ++(nextenumvalue);
            if (!!(ncols)) {
                mm_parse$checksymbol((i64)5);
                mm_lex$lex();
            }
;
        }
;
        for (i=(i64)1;i<=ncols;++i) {
L626 :;
            mm_lib$addlistunit(&plist[(i)-1],&plistx[(i)-1],mm_parse$readunit());
            if ((i == ncols)) {
                mm_parse$checksymbol((i64)14);
            }
            else {
                mm_parse$checksymbol((i64)5);
            }
;
            mm_lex$lex();
L627 :;
        }
L628 :;
        ;
        if (((i64)mm_decls$lx.symbol != (i64)5)) {
            goto L625 ;
        }
;
        mm_lex$lex();
        if (((i64)mm_decls$lx.symbol == closesym)) {
            goto L625 ;
        }
;
    }
L625 :;
    ;
    mm_parse$intabledata = (i64)0;
    mm_parse$skipsemi();
    mm_parse$checkbeginend(closesym,(i64)162,startline);
    if ((nrows == (i64)0)) {
        mm_support$serror((byte*)"No table data");
    }
;
    for (i=(i64)1;i<=ncols;++i) {
L629 :;
        stvar = (struct mm_decls$strec *)mm_lib$getduplnameptr((struct mm_decls$strec *)owner,(struct mm_decls$strec *)varnameptrs[(i)-1],(i64)11);
        (*stvar).code = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)15,(struct mm_decls$unitrec *)plist[(i)-1]);
        (*(*stvar).code).length = nrows;
        mm_lib$storemode((struct mm_decls$strec *)owner,varlisttypes[(i)-1],&(*stvar).mode);
        (*stvar).scope = scope;
        mm_lib$adddef((struct mm_decls$strec *)owner,(struct mm_decls$strec *)stvar);
        mm_lib$addstatic((struct mm_decls$strec *)stvar);
L630 :;
    }
L631 :;
    ;
}

void mm_parse$readclassdef(struct mm_decls$strec *owner,i64 scope) {
        i64 kwd;
        i64 baseclass;
        i64 m;
        i64 startline;
        i64 closesym;
        i64 mrec;
        i64 isrecord;
        i64 align;
        struct mm_decls$strec *  nameptr;
        struct mm_decls$strec *  sttype;
    kwd = (i64)mm_decls$lx.symbol;
    isrecord = (i64)(kwd == (i64)133);
    mm_parse$lexchecksymbol((i64)81);
    nameptr = (struct mm_decls$strec *)mm_decls$lx.symptr;
    mm_lex$lex();
    baseclass = (i64)0;
    if (((i64)mm_decls$lx.symbol == (i64)13)) {
        mm_lex$lex();
        baseclass = mm_parse$readtypespec((struct mm_decls$strec *)owner,(i64)0);
        mm_parse$checksymbol((i64)14);
        mm_lex$lex();
    }
;
    mm_parse$checkequals();
    mm_lex$lex();
    align = (i64)0;
    if (((i64)mm_decls$lx.symbol == (i64)22)) {
        if (((i64)mm_decls$lx.subcode == (i64)0)) {
            mm_lex$lex();
            align = mm_parse$readconstint();
        }
        else {
            mm_lex$lex();
        }
;
        align = (i64)1;
    }
;
    sttype = (struct mm_decls$strec *)mm_lib$getduplnameptr((struct mm_decls$strec *)owner,(struct mm_decls$strec *)nameptr,(i64)5);
    mm_lib$adddef((struct mm_decls$strec *)owner,(struct mm_decls$strec *)sttype);
    m = mm_lib$createusertype((struct mm_decls$strec *)sttype);
    mrec = mm_lib$createrecordmode((struct mm_decls$strec *)owner,m);
    mm_lib$storemode((struct mm_decls$strec *)owner,mrec,&(*sttype).mode);
    mm_lib$storemode((struct mm_decls$strec *)owner,baseclass,&(*sttype).baseclass);
    (*sttype).align = align;
    closesym = mm_parse$checkbegin((i64)1);
    startline = mm_parse$getcurrline();
    mm_parse$readclassbody((struct mm_decls$strec *)sttype,kwd);
    mm_parse$checkbeginend(closesym,kwd,startline);
    (*sttype).scope = scope;
}

static void mm_parse$readclassbody(struct mm_decls$strec *owner,i64 classkwd) {
        i64 kwd;
        i64 t;
    mm_lib$unionstr_clear(&mm_parse$unionstring);
    mm_lib$unionstr_clear(&mm_parse$unionpend);
    L632 :;
    switch ((i64)mm_decls$lx.symbol) {
    case 149:;
        {
            mm_parse$readconstdef((struct mm_decls$strec *)owner,(i64)0);
        }
        break;
    case 131:;
    case 130:;
        {
            kwd = (i64)mm_decls$lx.symbol;
            if (!!(msysc$m_getdotindex((i64)(*owner).flags,(i64)12))) {
                mm_parse$readprocdecl((struct mm_decls$strec *)owner,(i64)0,(i64)0);
            }
            else {
                mm_parse$readprocdef((struct mm_decls$strec *)owner,(i64)0,(i64)0);
            }
;
        }
        break;
    case 152:;
    case 133:;
        {
            mm_parse$readclassdef((struct mm_decls$strec *)owner,(i64)0);
        }
        break;
    case 137:;
        {
            mm_parse$readtypedef((struct mm_decls$strec *)owner,(i64)0);
        }
        break;
    case 68:;
        {
            mm_support$serror((byte*)"Class eof?");
            goto L633 ;
        }
        break;
    case 6:;
        {
            mm_lex$lex();
        }
        break;
    case 162:;
        {
            mm_parse$readtabledef((struct mm_decls$strec *)owner,(i64)0);
        }
        break;
    case 146:;
        {
            mm_parse$readmacrodef((struct mm_decls$strec *)owner,(i64)0);
        }
        break;
    case 134:;
    case 135:;
        {
            mm_lib$unionstr_append((struct mm_decls$uflagsrec *)&mm_parse$unionpend,(((i64)mm_decls$lx.symbol == (i64)134) ? (i64)83 : (i64)85));
            mm_parse$unionlastvar = 0;
            mm_lex$lex();
        }
        break;
    case 105:;
    case 14:;
        {
            if (!!((i64)mm_parse$unionstring.ulength)) {
                mm_parse$checkend((i64)105,((mm_lib$unionstr_last((struct mm_decls$uflagsrec *)&mm_parse$unionstring) == (i64)83) ? (i64)134 : (i64)135),(i64)0,(i64)0);
                mm_lex$lex();
                if (((mm_parse$unionlastvar == 0) || !!((i64)mm_parse$unionpend.ulength))) {
                    mm_support$serror((byte*)"Empty union group");
                }
;
                                {i64 $temp = mm_lib$unionstr_last((struct mm_decls$uflagsrec *)&(*mm_parse$unionlastvar).uflags);
if (($temp==(i64)69) || ($temp==(i64)42)) {
                }
                else {
                    mm_lib$unionstr_append((struct mm_decls$uflagsrec *)&(*mm_parse$unionlastvar).uflags,(i64)42);
                }
                };
                mm_lib$unionstr_append((struct mm_decls$uflagsrec *)&(*mm_parse$unionlastvar).uflags,(i64)69);
                --(mm_parse$unionstring.ulength);
            }
            else {
                goto L633 ;
            }
;
        }
        break;
    case 141:;
        {
            mm_lex$lex();
            if (!!(mm_parse$istypestarter())) {
                //readmut:
L634 :;
;
                ++(mm_parse$insiderecord);
                t = mm_parse$readtypespec((struct mm_decls$strec *)owner,(i64)0);
                --(mm_parse$insiderecord);
            }
            else {
                t = (i64)21;
            }
;
            mm_parse$readrecordfields((struct mm_decls$strec *)owner,t);
        }
        break;
    case 142:;
        {
            mm_support$serror((byte*)"Let not allowed");
        }
        break;
    default: {
        if (!!(mm_parse$istypestarter())) {
            goto L634 ;
;
        }
        else {
            goto L633 ;
        }
;
    }
    } //SW
goto L632 ;
L633 :;
    ;
}

static void mm_parse$readimportmodule(struct mm_decls$strec *owner) {
        i64 isnew;
        i64 startline;
        i64 closesym;
        i64 libtype;
        struct mm_decls$strec *  d;
        struct mm_decls$strec *  stname;
    if (!!(mm_parse$insidedllimport)) {
        mm_support$serror((byte*)"nested importdll");
    }
;
    libtype = (i64)mm_decls$lx.subcode;
    mm_lex$lex();
    if (((i64)mm_decls$lx.symbol == (i64)77)) {
        stname = (struct mm_decls$strec *)mm_lex$addnamestr(mm_decls$lx.svalue);
    }
    else {
        mm_parse$checksymbol((i64)81);
        stname = (struct mm_decls$strec *)mm_decls$lx.symptr;
    }
;
    mm_lex$lex();
    if (((i64)mm_decls$lx.symbol == (i64)13)) {
        L635 :;
        do {
            mm_lex$lex();
            mm_parse$checksymbol((i64)81);
            mm_modules$addlib((*mm_decls$lx.symptr).name,libtype);
            mm_lex$lex();
L636 :;
        }
        while (!((i64)mm_decls$lx.symbol != (i64)5));
L637 :;
        ;
        mm_parse$checksymbol((i64)14);
        mm_lex$lex();
    }
;
    mm_parse$checkequals();
    mm_lex$lex();
    isnew = (i64)1;
    d = (struct mm_decls$strec *)(*stname).nextdupl;
    L638 :;
    while (!!(d)) {
        if (((i64)(*d).nameid == (i64)4)) {
            stname = d;
            isnew = (i64)0;
            goto L640 ;
        }
;
        d = (struct mm_decls$strec *)(*d).nextdupl;
L639 :;
    }
L640 :;
    ;
    if (!!(isnew)) {
        stname = (struct mm_decls$strec *)mm_lib$getduplnameptr(mm_decls$stmodule,(struct mm_decls$strec *)stname,(i64)4);
        mm_lib$adddef(mm_decls$stmodule,(struct mm_decls$strec *)stname);
        mm_modules$addlib((*stname).name,libtype);
        (*stname).dllindex = mm_decls$nlibfiles;
    }
;
    startline = mm_parse$getcurrline();
    closesym = mm_parse$checkbegin((i64)0);
    mm_parse$insidedllimport = (i64)(*stname).dllindex;
    mm_parse$readimportbody((struct mm_decls$strec *)owner);
    mm_parse$insidedllimport = (i64)0;
    mm_parse$checkbeginend(closesym,(i64)136,startline);
}

static void mm_parse$readimportbody(struct mm_decls$strec *owner) {
        i64 pos;
        i64 fflang;
        struct mm_decls$strec *  d;
    pos = (i64)mm_decls$lx.pos;
    L641 :;
    while (1) {
        mm_parse$skipsemi();
        switch ((i64)mm_decls$lx.symbol) {
        case 155:;
            {
                fflang = (i64)mm_decls$lx.subcode;
                mm_lex$lex();
                                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)130) || ($temp==(i64)131)) {
                    goto L643 ;
;
                }
                };
            }
            break;
        case 130:;
        case 131:;
            {
                fflang = (i64)0;
                //doproc:
L643 :;
;
                d = (struct mm_decls$strec *)mm_parse$readprocdecl((struct mm_decls$strec *)owner,(i64)0,fflang);
                if ((mm_decls$ndllproctable >= (i64)1000)) {
                    mm_support$serror((byte*)"Too many dll procs");
                }
;
                mm_decls$dllproctable[(++(mm_decls$ndllproctable))-1] = d;
            }
            break;
        case 137:;
            {
                mm_parse$readtypedef((struct mm_decls$strec *)owner,(i64)1);
            }
            break;
        case 149:;
            {
                mm_parse$readconstdef((struct mm_decls$strec *)owner,(i64)1);
            }
            break;
        case 152:;
        case 133:;
            {
                mm_parse$readclassdef((struct mm_decls$strec *)owner,(i64)1);
            }
            break;
        case 141:;
            {
                mm_lex$lex();
                mm_parse$readvardef((struct mm_decls$strec *)owner,(i64)1,(i64)0,(i64)8,(i64)141);
            }
            break;
        case 93:;
        case 81:;
        case 140:;
        case 97:;
        case 94:;
        case 15:;
        case 145:;
        case 143:;
            {
                mm_parse$readvardef((struct mm_decls$strec *)owner,(i64)1,(i64)0,(i64)8,(i64)0);
            }
            break;
        case 68:;
            {
                goto L642 ;
            }
            break;
        case 105:;
            {
                goto L642 ;
            }
            break;
        default: {
            mm_lex$ps((byte*)"symbol");
            mm_support$serror((byte*)"Not allowed in importmodule");
        }
        } //SW
;
    }
L642 :;
    ;
}

static struct mm_decls$strec *mm_parse$readequivfield(struct mm_decls$strec *owner) {
        struct mm_decls$strec *  p;
        struct mm_decls$strec *  d;
    mm_parse$checksymbol((i64)81);
    d = (struct mm_decls$strec *)mm_decls$lx.symptr;
    mm_lex$lex();
    p = (struct mm_decls$strec *)(*owner).deflist;
    L644 :;
    while (!!(p)) {
        if (!!(mlib$eqstring((*p).name,(*d).name))) {
            return (struct mm_decls$strec *)p;
        }
;
        p = (struct mm_decls$strec *)(*p).nextdef;
L645 :;
    }
L646 :;
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((*d).name,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mm_support$serror((byte*)"Can't find @ field");
    return (struct mm_decls$strec *)0;
}

static i64 mm_parse$readrefproc(struct mm_decls$strec *owner,i64 typedefx,i64 fflang) {
        i64 kwd;
        i64 prettype;
        i64 m;
        i64 varparams;
        i64 nparams;
        i64 retmodes[4];
        struct mm_decls$strec *  paramlist;
        struct mm_decls$strec *  stproc;
        i64 nretvalues;
        u8 *  name;
    kwd = (i64)mm_decls$lx.symbol;
    mm_lex$lex();
    paramlist = 0;
    prettype = (i64)0;
    nretvalues = (i64)0;
    name = mm_lib$nextautotype();
    stproc = (struct mm_decls$strec *)mm_lib$getduplnameptr(mm_decls$stmodule,(struct mm_decls$strec *)mm_lex$addnamestr(name),(i64)5);
    mm_lib$adddef(mm_decls$stmodule,(struct mm_decls$strec *)stproc);
    retmodes[((i64)1)-1] = (i64)0;
    if ((kwd == (i64)131)) {
        if (((i64)mm_decls$lx.symbol == (i64)13)) {
            mm_lex$lex();
            if (((i64)mm_decls$lx.symbol != (i64)14)) {
                paramlist = (struct mm_decls$strec *)mm_parse$readparams((struct mm_decls$strec *)owner,(struct mm_decls$strec *)stproc,(i64)0,&varparams,&nparams);
                mm_parse$checksymbol((i64)14);
            }
;
            mm_lex$lex();
            if ((((i64)mm_decls$lx.symbol == (i64)7) || ((i64)mm_decls$lx.symbol == (i64)11))) {
                mm_lex$lex();
                nretvalues = mm_parse$readreturntype((struct mm_decls$strec *)stproc,&retmodes);
            }
            else if ((!!((i64)mm_decls$typestarterset[((i64)mm_decls$lx.symbol)]) || ((i64)mm_decls$lx.symbol == (i64)81))) {
                nretvalues = mm_parse$readreturntype((struct mm_decls$strec *)stproc,&retmodes);
            }
;
        }
        else if ((((i64)mm_decls$lx.symbol == (i64)7) || ((i64)mm_decls$lx.symbol == (i64)11))) {
            mm_lex$lex();
            nretvalues = mm_parse$readreturntype((struct mm_decls$strec *)stproc,&retmodes);
        }
;
        if ((nretvalues == (i64)0)) {
            mm_support$serror((byte*)"Function needs return type");
        }
;
        if ((!!(nretvalues) && (kwd == (i64)130))) {
            mm_support$serror((byte*)"Proc can't return value");
        }
;
    }
    else {
        if (((i64)mm_decls$lx.symbol == (i64)13)) {
            mm_lex$lex();
            if (((i64)mm_decls$lx.symbol != (i64)14)) {
                paramlist = (struct mm_decls$strec *)mm_parse$readparams((struct mm_decls$strec *)owner,(struct mm_decls$strec *)stproc,(i64)0,&varparams,&nparams);
                mm_parse$checksymbol((i64)14);
            }
;
            mm_lex$lex();
        }
;
        if (((!!((i64)mm_decls$typestarterset[((i64)mm_decls$lx.symbol)]) || ((i64)mm_decls$lx.symbol == (i64)7)) || ((i64)mm_decls$lx.symbol == (i64)11))) {
            mm_support$serror((byte*)"proc can't have ret value");
        }
;
    }
;
    m = mm_lib$createrefprocmode((struct mm_decls$strec *)owner,(struct mm_decls$strec *)stproc,(struct mm_decls$strec *)paramlist,kwd,prettype,typedefx);
    mm_lib$storemode((struct mm_decls$strec *)owner,retmodes[((i64)1)-1],&(*stproc).mode);
    (*stproc).nretvalues = nretvalues;
    mm_decls$ttnamedef[(m)] = (struct mm_decls$strec *)stproc;
    (*stproc).fflang = fflang;
    return m;
}

static void mm_parse$pushproc(struct mm_decls$strec *p) {
    if ((mm_parse$nprocstack >= (i64)10)) {
        mm_support$serror((byte*)"Too many nested proc");
    }
;
    mm_parse$procstack[(++(mm_parse$nprocstack))-1] = (struct mm_decls$strec *)mm_decls$currproc;
    mm_decls$currproc = (struct mm_decls$strec *)p;
}

static void mm_parse$popproc(void) {
    if (!!(mm_parse$nprocstack)) {
        mm_decls$currproc = (struct mm_decls$strec *)mm_parse$procstack[((mm_parse$nprocstack)--)-1];
    }
    else {
        mm_decls$currproc = mm_decls$stmodule;
    }
;
}

static struct mm_decls$unitrec *mm_parse$makeastring(void) {
        struct mm_decls$unitrec *  ulist;
        struct mm_decls$unitrec *  ulistx;
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  pconst;
        u8 *  s;
        i64 length;
        i64 $av_1;
    ulist = (ulistx = 0);
    s = mm_decls$lx.svalue;
    length = mm_lex$astringlength;
    $av_1 = mm_lex$astringlength;
    while ($av_1-- > 0) {
L647 :;
        pconst = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)(u64)(*s),(i64)3);
        mm_lib$addlistunit(&ulist,&ulistx,pconst);
        ++(s);
L648 :;
    }
L649 :;
    ;
    if (((i64)mm_decls$lx.subcode == (i64)90)) {
        pconst = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)0u,(i64)3);
        mm_lib$addlistunit(&ulist,&ulistx,pconst);
        ++(length);
    }
;
    p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)15,(struct mm_decls$unitrec *)ulist);
    (*p).length = length;
    return p;
}

static i64 mm_parse$readreturntype(struct mm_decls$strec *owner,i64 (*retmodes)[]) {
        i64 nretvalues;
    (*retmodes)[((i64)1)-1] = mm_parse$readtypespec((struct mm_decls$strec *)owner,(i64)0);
    nretvalues = (i64)1;
    L650 :;
    while (((i64)mm_decls$lx.symbol == (i64)5)) {
        if ((nretvalues >= (i64)4)) {
            mm_support$serror((byte*)"Too many return values");
        }
;
        mm_lex$lex();
        (*retmodes)[(++(nretvalues))-1] = mm_parse$readtypespec((struct mm_decls$strec *)owner,(i64)0);
L651 :;
    }
L652 :;
    ;
    return nretvalues;
}

static struct mm_decls$unitrec *mm_parse$readset(void) {
        i64 length;
        i64 nkeyvalues;
        i64 oldirp;
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  ulist;
        struct mm_decls$unitrec *  ulistx;
    mm_lex$lex();
        {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)16)) {
        mm_lex$lex();
        return (struct mm_decls$unitrec *)mm_lib$createunit1((i64)17,0);
    }
    else if (($temp==(i64)7)) {
        mm_parse$lexchecksymbol((i64)16);
        mm_lex$lex();
        return (struct mm_decls$unitrec *)mm_lib$createunit1((i64)18,0);
    }
    };
    length = (i64)0;
    nkeyvalues = (i64)0;
    ulist = (ulistx = 0);
    L653 :;
    while (1) {
        oldirp = mm_parse$inreadprint;
        mm_parse$inreadprint = (i64)0;
        p = mm_parse$readunit();
        mm_parse$inreadprint = oldirp;
        if (((i64)(*p).tag == (i64)22)) {
            ++(nkeyvalues);
        }
;
        ++(length);
        mm_lib$addlistunit(&ulist,&ulistx,p);
                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)5)) {
            mm_lex$lex();
            if (((i64)mm_decls$lx.symbol == (i64)16)) {
                goto L654 ;
            }
;
        }
        else if (($temp==(i64)6)) {
            mm_parse$lexchecksymbol((i64)16);
            goto L654 ;
        }
        else if (($temp==(i64)16)) {
            goto L654 ;
        }
        else {
            mm_support$serror((byte*)"readset?");
        }
        };
        mm_parse$skipsemi();
    }
L654 :;
    ;
    mm_lex$lex();
    if (!!(nkeyvalues)) {
        if ((length > nkeyvalues)) {
            mm_support$serror((byte*)"dict: mixed elements");
        }
;
        p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)18,(struct mm_decls$unitrec *)ulist);
    }
    else {
        p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)17,(struct mm_decls$unitrec *)ulist);
    }
;
    (*p).length = length;
    return p;
}

static i64 mm_parse$istypestarter(void) {
    if (!!((i64)mm_decls$typestarterset[((i64)mm_decls$lx.symbol)])) {
        return (i64)1;
    }
;
    if (((i64)mm_decls$lx.symbol == (i64)81)) {
                {i64 $temp = (i64)mm_decls$nextlx.symbol;
if (($temp==(i64)81)) {
            return (i64)1;
        }
        else if (($temp==(i64)25)) {
            return (i64)1;
        }
        };
    }
;
    return (i64)0;
}

struct mm_decls$unitrec *mm_parse$readunit(void) {
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  pt;
        i64 pos;
    pt = 0;
    pos = (i64)mm_decls$lx.pos;
    pt = mm_parse$readterm2();
    if (((i64)mm_tables$jisexpr[((i64)(*pt).tag)] == (i64)0)) {
        return pt;
    }
;
    if (((i64)mm_decls$lx.symbol == (i64)9)) {
        mm_lex$lex();
        p = mm_parse$readterm2();
        p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)23,(struct mm_decls$unitrec *)pt,(struct mm_decls$unitrec *)mm_parse$readassignment(p));
    }
    else {
        p = mm_parse$readassignment(pt);
        (*p).pos = pos;
    }
;
    L655 :;
    while (((i64)mm_decls$lx.symbol == (i64)12)) {
        mm_lex$lex();
        p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)28,(struct mm_decls$unitrec *)mm_parse$readassignment(0),(struct mm_decls$unitrec *)p);
L656 :;
    }
L657 :;
    ;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readassignment(struct mm_decls$unitrec *pt) {
        struct mm_decls$unitrec *  p;
        i64 pos;
        i64 opc;
        struct mm_decls$unitrec *  q;
    p = mm_parse$readorterms(pt);
    if (((opc = (i64)mm_decls$lx.symbol) == (i64)9 || (opc = (i64)mm_decls$lx.symbol) == (i64)10)) {
        pos = (i64)mm_decls$lx.pos;
        mm_lex$lex();
        if (((i64)mm_decls$lx.symbol == (i64)170)) {
            p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)122,(struct mm_decls$unitrec *)p);
            mm_lex$lex();
        }
        else {
            q = mm_parse$readassignment(0);
            if ((opc == (i64)10)) {
                q = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)27,(struct mm_decls$unitrec *)q);
            }
;
            p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)23,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)q);
        }
;
        (*p).pos = pos;
    }
;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readorterms(struct mm_decls$unitrec *pt) {
        struct mm_decls$unitrec *  p;
        i64 pos;
    p = mm_parse$readandterms(pt);
    L658 :;
    while (((i64)mm_decls$lx.symbol == (i64)46)) {
        pos = (i64)mm_decls$lx.pos;
        mm_lex$lex();
        if (((i64)mm_decls$lx.symbol == (i64)9)) {
            mm_lex$lex();
            p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)33,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)mm_parse$readassignment(0));
            (*p).pclop = (i64)73;
            (*p).pos = pos;
            goto L660 ;
        }
;
        p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)12,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)mm_parse$readandterms(0));
        (*p).pclop = (i64)25;
        (*p).pos = pos;
L659 :;
    }
L660 :;
    ;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readandterms(struct mm_decls$unitrec *pt) {
        struct mm_decls$unitrec *  p;
        i64 pos;
    p = mm_parse$readcmpterms(pt);
    L661 :;
    while (((i64)mm_decls$lx.symbol == (i64)45)) {
        pos = (i64)mm_decls$lx.pos;
        mm_lex$lex();
        if (((i64)mm_decls$lx.symbol == (i64)9)) {
            mm_lex$lex();
            p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)33,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)mm_parse$readassignment(0));
            (*p).pclop = (i64)72;
            (*p).pos = pos;
            goto L663 ;
        }
;
        p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)11,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)mm_parse$readcmpterms(0));
        (*p).pclop = (i64)24;
        (*p).pos = pos;
L662 :;
    }
L663 :;
    ;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readcmpterms(struct mm_decls$unitrec *pt) {
        struct mm_decls$unitrec *  p;
        i64 pos;
        i64 n;
        struct mm_decls$unitrec *  ulist;
        struct mm_decls$unitrec *  ulistx;
        struct mm_decls$unitrec *  q;
        byte genops[4];
    p = mm_parse$readinterms(pt);
    if (!(((i64)mm_decls$lx.symbol == (i64)48 || (i64)mm_decls$lx.symbol == (i64)49))) {
        return p;
    }
;
    ulist = (ulistx = p);
    p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)30,(struct mm_decls$unitrec *)p);
    n = (i64)0;
    memset(&(genops),0,4);
    L664 :;
    switch ((i64)mm_decls$lx.symbol) {
    case 48:;
    case 49:;
        {
            ++(n);
            if ((n > (i64)4)) {
                mm_support$serror((byte*)"cmpchain: Too many items");
            }
;
            genops[(n)-1] = (i64)mm_decls$lx.subcode;
            pos = (i64)mm_decls$lx.pos;
            mm_lex$lex();
            q = mm_parse$readinterms(0);
            mm_lib$addlistunit(&ulist,&ulistx,q);
            (*q).pos = pos;
        }
        break;
    default: {
        goto L665 ;
    }
    } //SW
goto L664 ;
L665 :;
    ;
    if ((n == (i64)1)) {
        (*p).tag = (i64)29;
        q = (*p).a;
        (*p).pclop = (i64)genops[((i64)1)-1];
        (*p).b = (*q).nextunit;
        (*q).nextunit = 0;
    }
    else {
        memcpy(&(*p).cmpgenop,&genops,4);
    }
;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readinterms(struct mm_decls$unitrec *pt) {
        struct mm_decls$unitrec *  p;
        i64 pos;
        i64 opc;
    p = mm_parse$readrangeterm(pt);
    L666 :;
    switch ((i64)mm_decls$lx.symbol) {
    case 52:;
    case 53:;
        {
            opc = (i64)mm_decls$lx.subcode;
            pos = (i64)mm_decls$lx.pos;
            mm_lex$lex();
            p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)31,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)mm_parse$readrangeterm(0));
            (*p).pclop = opc;
            (*p).pos = pos;
        }
        break;
    default: {
        goto L667 ;
    }
    } //SW
goto L666 ;
L667 :;
    ;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readrangeterm(struct mm_decls$unitrec *pt) {
        struct mm_decls$unitrec *  p;
        i64 pos;
    p = mm_parse$readaddterms(pt);
    if (((i64)mm_decls$lx.symbol == (i64)28)) {
        pos = (i64)mm_decls$lx.pos;
        mm_lex$lex();
        p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)16,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)mm_parse$readaddterms(0));
        (*p).pos = pos;
    }
;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readaddterms(struct mm_decls$unitrec *pt) {
        struct mm_decls$unitrec *  p;
        i64 pos;
        i64 sym;
        i64 genop;
    p = mm_parse$readmulterms(pt);
    L668 :;
    switch ((sym = (i64)mm_decls$lx.symbol)) {
    case 31:;
    case 32:;
    case 38:;
    case 39:;
    case 40:;
    case 43:;
    case 44:;
        {
            pos = (i64)mm_decls$lx.pos;
            genop = (i64)mm_decls$lx.subcode;
            mm_lex$lex();
            if (((i64)mm_decls$lx.symbol == (i64)9)) {
                mm_lex$lex();
                p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)33,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)mm_parse$readassignment(0));
                (*p).pclop = (i64)mm_tables$symbolgentoops[(sym)-1];
                (*p).pos = pos;
                goto L669 ;
            }
;
            p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)31,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)mm_parse$readmulterms(0));
            (*p).pclop = (i64)mm_tables$symbolgenops[(sym)-1];
            (*p).pos = pos;
        }
        break;
    default: {
        goto L669 ;
    }
    } //SW
goto L668 ;
L669 :;
    ;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readmulterms(struct mm_decls$unitrec *pt) {
        struct mm_decls$unitrec *  p;
        i64 pos;
        i64 sym;
    p = mm_parse$readpowerterms(pt);
    L670 :;
    switch ((sym = (i64)mm_decls$lx.symbol)) {
    case 33:;
    case 34:;
    case 35:;
    case 36:;
    case 41:;
    case 42:;
    case 37:;
        {
            pos = (i64)mm_decls$lx.pos;
            mm_lex$lex();
            if (((i64)mm_decls$lx.symbol == (i64)9)) {
                mm_lex$lex();
                p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)33,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)mm_parse$readassignment(0));
                (*p).pclop = (i64)mm_tables$symbolgentoops[(sym)-1];
                (*p).pos = pos;
                goto L671 ;
            }
;
            p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)31,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)mm_parse$readpowerterms(0));
            (*p).pclop = (i64)mm_tables$symbolgenops[(sym)-1];
            (*p).pos = pos;
        }
        break;
    default: {
        goto L671 ;
    }
    } //SW
goto L670 ;
L671 :;
    ;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readpowerterms(struct mm_decls$unitrec *p) {
        i64 pos;
    if ((p == 0)) {
        p = mm_parse$readterm2();
    }
;
    L672 :;
    while (((i64)mm_decls$lx.symbol == (i64)50)) {
        pos = (i64)mm_decls$lx.pos;
        mm_lex$lex();
        p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)31,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)mm_parse$readpowerterms(0));
        (*p).pclop = (i64)51;
        (*p).pos = pos;
L673 :;
    }
L674 :;
    ;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readterm2(void) {
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  q;
        i64 opc;
        i64 oldinrp;
        i64 pos;
    pos = (i64)mm_decls$lx.pos;
    p = mm_parse$readterm();
    L675 :;
    switch ((i64)mm_decls$lx.symbol) {
    case 13:;
        {
            mm_lex$lex();
            oldinrp = mm_parse$inreadprint;
            mm_parse$inreadprint = (i64)0;
            q = mm_parse$readslist((i64)1,(i64)1);
            mm_parse$checksymbol((i64)14);
            mm_lex$lex();
            if (((i64)(*p).tag == (i64)88)) {
                (*p).a = q;
            }
            else {
                p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)28,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)q);
            }
;
            mm_parse$inreadprint = oldinrp;
            p = mm_parse$readcondsuffix(p);
        }
        break;
    case 19:;
        {
            p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)45,(struct mm_decls$unitrec *)p);
            mm_lex$lex();
        }
        break;
    case 15:;
        {
            p = mm_parse$readindex(p,(i64)0);
        }
        break;
    case 2:;
        {
            p = mm_parse$readdotsuffix(p);
        }
        break;
    case 7:;
        {
            if (!!(mm_parse$inreadprint)) {
                goto L676 ;
            }
;
            mm_lex$lex();
            q = mm_parse$readunit();
            p = (struct mm_decls$unitrec *)mm_lib$createunit2((!!(mm_parse$inparamlist) ? (i64)21 : (i64)22),(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)q);
        }
        break;
    case 71:;
        {
                        {i64 $temp = (i64)mm_decls$lx.subcode;
if (($temp==(i64)53)) {
                opc = (i64)57;
            }
            else if (($temp==(i64)54)) {
                opc = (i64)58;
            }
            };
            mm_lex$lex();
            p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)35,(struct mm_decls$unitrec *)p);
            (*p).pclop = opc;
        }
        break;
    case 17:;
        {
            mm_support$serror((byte*)"X{...} not ready");
        }
        break;
    default: {
        goto L676 ;
    }
    } //SW
goto L675 ;
L676 :;
    ;
    (*p).pos = pos;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readterm(void) {
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  q;
        struct mm_decls$unitrec *  r;
        u64 a;
        i64 opc;
        i64 pos;
        i64 length;
    pos = (i64)mm_decls$lx.pos;
    switch ((i64)mm_decls$lx.symbol) {
    case 81:;
        {
            if (((i64)mm_decls$nextlx.symbol == (i64)22)) {
                p = mm_parse$readcast();
            }
            else {
                p = (struct mm_decls$unitrec *)mm_lib$createname((struct mm_decls$strec *)mm_decls$lx.symptr);
                (*p).pos = (i64)mm_decls$lx.pos;
                mm_lex$lex();
            }
;
        }
        break;
    case 72:;
    case 74:;
        {
            p = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)mm_decls$lx.value,(i64)mm_decls$lx.subcode);
            (*p).istrueconst = (i64)1;
            mm_lex$lex();
        }
        break;
    case 77:;
        {
            p = (struct mm_decls$unitrec *)mm_lib$createstringconstunit(mm_decls$lx.svalue,(i64)-1);
            mm_lex$lex();
        }
        break;
    case 78:;
        {
            p = mm_parse$makeastring();
            mm_lex$lex();
        }
        break;
    case 73:;
        {
            mm_support$serror((byte*)"DEC CONST");
        }
        break;
    case 75:;
        {
            length = strlen(mm_decls$lx.svalue);
            if ((length > (i64)8)) {
                mm_support$serror((byte*)"Char const too long");
            }
;
            a = (u64)0u;
            if (!!(length)) {
                memcpy(&a,(void *)mm_decls$lx.svalue,(u64)length);
            }
;
            p = (struct mm_decls$unitrec *)mm_lib$createconstunit(a,(i64)1);
            (*p).istrueconst = (i64)1;
            mm_lex$lex();
        }
        break;
    case 13:;
        {
            p = mm_parse$readlbrack();
        }
        break;
    case 93:;
    case 140:;
    case 97:;
    case 94:;
        {
            p = mm_parse$readcast();
        }
        break;
    case 31:;
    case 32:;
    case 43:;
    case 44:;
    case 59:;
    case 58:;
    case 64:;
    case 61:;
    case 62:;
    case 65:;
    case 60:;
        {
            p = mm_parse$readopc();
        }
        break;
    case 56:;
        {
            if (((i64)mm_decls$nextlx.symbol == (i64)9)) {
                p = mm_parse$readopc();
            }
            else {
                mm_lex$lex();
                p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)13,(struct mm_decls$unitrec *)mm_parse$readterm2());
                (*p).pclop = (i64)32;
            }
;
        }
        break;
    case 57:;
        {
            if (((i64)mm_decls$nextlx.symbol == (i64)9)) {
                p = mm_parse$readopc();
            }
            else {
                mm_lex$lex();
                p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)14,(struct mm_decls$unitrec *)mm_parse$readterm2());
                (*p).pclop = (i64)33;
            }
;
        }
        break;
    case 15:;
        {
            p = mm_parse$readset();
        }
        break;
    case 71:;
        {
            opc = (i64)mm_decls$lx.subcode;
            mm_lex$lex();
            p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)35,(struct mm_decls$unitrec *)mm_parse$readterm2());
            (*p).pclop = opc;
        }
        break;
    case 126:;
        {
            p = mm_parse$readsprint();
        }
        break;
    case 128:;
    case 129:;
        {
            p = mm_parse$readsread();
        }
        break;
    case 25:;
    case 26:;
        {
            opc = (i64)mm_decls$lx.subcode;
            mm_lex$lex();
            p = (struct mm_decls$unitrec *)mm_lib$createunit1(opc,(struct mm_decls$unitrec *)mm_parse$readterm2());
            if (((i64)(*(*p).a).tag == (i64)28)) {
                if (!!((*(*p).a).b)) {
                    mm_support$serror((byte*)"Params not allowed");
                }
;
                (*p).a = (*(*p).a).a;
            }
;
        }
        break;
    case 4:;
        {
            mm_lex$lex();
            p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)47,(struct mm_decls$unitrec *)mm_parse$readterm2());
        }
        break;
    case 159:;
        {
            p = mm_parse$readcompilervar();
        }
        break;
    case 166:;
        {
            p = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)(i64)mm_decls$lx.subcode,(i64)3);
            mm_lex$lex();
        }
        break;
    case 160:;
        {
            if (!!(mm_parse$intabledata)) {
                if (!(!!(mm_parse$tabledataname))) {
                    mm_support$serror((byte*)"$: no enum");
                }
;
                p = (struct mm_decls$unitrec *)mm_lib$createstringconstunit(mm_parse$tabledataname,(i64)-1);
            }
            else {
                if ((mm_parse$ndollar <= (i64)0)) {
                    mm_support$serror((byte*)"[$] No array");
                }
;
                p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)32,(struct mm_decls$unitrec *)mm_parse$dollarstack[(mm_parse$ndollar)-1]);
                (*p).pclop = (i64)90;
            }
;
            mm_lex$lex();
        }
        break;
    case 158:;
        {
            p = mm_parse$readcastx();
        }
        break;
    case 164:;
        {
            mm_parse$lexchecksymbol((i64)13);
            mm_lex$lex();
            p = mm_parse$readunit();
            mm_parse$checksymbol((i64)5);
            mm_lex$lex();
            q = mm_parse$readunit();
            if ((((i64)mm_decls$lx.symbol == (i64)14) && ((i64)(*q).tag == (i64)16))) {
                r = (*q).b;
                q = (*q).a;
            }
            else {
                mm_parse$checksymbol((i64)5);
                mm_lex$lex();
                r = mm_parse$readunit();
                mm_parse$checksymbol((i64)14);
            }
;
            mm_lex$lex();
            q = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)31,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)q);
            (*q).pclop = (i64)16;
            p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)31,(struct mm_decls$unitrec *)q,(struct mm_decls$unitrec *)r);
            (*p).pclop = (i64)15;
        }
        break;
    case 122:;
        {
            p = mm_parse$readgoto((i64)97);
        }
        break;
    case 98:;
        {
            p = mm_parse$readif();
        }
        break;
    case 106:;
        {
            p = mm_parse$readunless();
        }
        break;
    case 107:;
    case 108:;
    case 123:;
    case 124:;
        {
            p = mm_parse$readswitchcase();
        }
        break;
    case 109:;
        {
            p = mm_parse$readrecase();
        }
        break;
    case 111:;
        {
            p = mm_parse$readfor();
        }
        break;
    case 112:;
        {
            p = mm_parse$readto();
        }
        break;
    case 114:;
        {
            p = mm_parse$readdo();
        }
        break;
    case 115:;
        {
            p = mm_parse$readwhile();
        }
        break;
    case 116:;
        {
            p = mm_parse$readrepeat();
        }
        break;
    case 120:;
        {
            p = mm_parse$readloopcontrol();
        }
        break;
    case 118:;
        {
            p = mm_parse$readreturn();
        }
        break;
    case 119:;
        {
            p = mm_parse$readstop();
        }
        break;
    case 125:;
        {
            p = mm_parse$readprint();
        }
        break;
    case 127:;
        {
            p = mm_parse$readread();
        }
        break;
    case 165:;
        {
            mm_parse$lexchecksymbol((i64)13);
            mm_lex$lex();
            p = mm_parse$readunit();
            mm_parse$checksymbol((i64)5);
            mm_lex$lex();
            q = mm_parse$readunit();
            mm_parse$checksymbol((i64)14);
            mm_lex$lex();
            p = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)107,(struct mm_decls$unitrec *)p,(struct mm_decls$unitrec *)q);
        }
        break;
    case 161:;
        {
            opc = (i64)mm_decls$lx.subcode;
            mm_lex$lex();
            p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)121,(struct mm_decls$unitrec *)mm_parse$readunit());
            (*p).index = opc;
        }
        break;
    case 167:;
        {
            (*mm_decls$currproc).asmused = (i64)1;
            mm_decls$assemmode = (i64)1;
            if (((i64)mm_decls$lx.subcode == (i64)0)) {
                p = mm_assem$readassemline();
            }
            else {
                p = mm_assem$readassemblock();
            }
;
            mm_decls$assemmode = (i64)0;
        }
        break;
    case 168:;
        {
            p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)88);
            (*p).fnindex = (i64)mm_decls$lx.subcode;
            mm_lex$lex();
        }
        break;
    case 83:;
        {
            mm_lex$lex();
            p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)10,(struct mm_decls$unitrec *)mm_parse$readterm2());
        }
        break;
    case 169:;
        {
            if ((u64)0u) {
                mm_support$serror((byte*)"emitc?");
            }
;
            mm_lex$lex();
            mm_parse$checksymbol((i64)77);
            p = (struct mm_decls$unitrec *)mm_lib$createstringconstunit(mm_decls$lx.svalue,(i64)-1);
            (*p).tag = (i64)123;
            mm_lex$lex();
        }
        break;
    case 170:;
        {
            mm_lex$lex();
            p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)122,(struct mm_decls$unitrec *)mm_parse$readterm2());
        }
        break;
    case 171:;
        {
            mm_lex$lex();
            p = (struct mm_decls$unitrec *)mm_lib$createunit1((i64)27,(struct mm_decls$unitrec *)mm_parse$readterm2());
        }
        break;
    case 17:;
        {
            mm_support$serror((byte*)"{...} not ready");
        }
        break;
    default: {
        msysc$m_print_startcon();
        msysc$m_print_str(mm_tables$symbolnames[((i64)mm_decls$lx.symbol)-1],NULL);
        msysc$m_print_str((byte*)"LX.SYMBOL=",NULL);
        msysc$m_print_i64((i64)mm_decls$lx.symbol,NULL);
        msysc$m_print_i64(mm_parse$istypestarter(),NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        mm_support$serror((byte*)"readterm?");
    }
    } //SW
;
    (*p).pos = pos;
    return p;
}

static void mm_parse$readmacrodef(struct mm_decls$strec *owner,i64 scope) {
        struct mm_decls$strec *  nameptr;
        struct mm_decls$strec *  stmacro;
        struct mm_decls$strec *  paramlist;
        struct mm_decls$strec *  paramlistx;
        struct mm_decls$strec *  stname;
    mm_parse$lexchecksymbol((i64)81);
    nameptr = (struct mm_decls$strec *)mm_decls$lx.symptr;
    stmacro = (struct mm_decls$strec *)mm_lib$getduplnameptr((struct mm_decls$strec *)owner,(struct mm_decls$strec *)nameptr,(i64)18);
    mm_lib$adddef((struct mm_decls$strec *)owner,(struct mm_decls$strec *)stmacro);
    owner = (struct mm_decls$strec *)stmacro;
    mm_lex$lex();
    paramlist = (paramlistx = 0);
    if (((i64)mm_decls$lx.symbol == (i64)13)) {
        mm_lex$lex();
        if (((i64)mm_decls$lx.symbol != (i64)14)) {
            L677 :;
            while (1) {
                                {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)81)) {
                    stname = (struct mm_decls$strec *)mm_lib$getduplnameptr((struct mm_decls$strec *)owner,(struct mm_decls$strec *)mm_decls$lx.symptr,(i64)19);
                    mm_lib$adddef((struct mm_decls$strec *)owner,(struct mm_decls$strec *)stname);
                    mm_parse$addlistparam((struct mm_decls$strec **)&paramlist,(struct mm_decls$strec **)&paramlistx,(struct mm_decls$strec *)stname);
                    mm_lex$lex();
                    if (((i64)mm_decls$lx.symbol == (i64)14)) {
                        goto L678 ;
                    }
;
                    mm_parse$checksymbol((i64)5);
                    mm_lex$lex();
                }
                else {
                    mm_support$serror((byte*)"macro def params");
                }
                };
            }
L678 :;
            ;
        }
;
        mm_lex$lex();
    }
;
    (*stmacro).paramlist = (struct mm_decls$strec *)paramlist;
    (*stmacro).scope = scope;
    mm_parse$checkequals();
    mm_lex$lex();
    (*stmacro).code = mm_parse$readunit();
}

static struct mm_decls$unitrec *mm_parse$readrecase(void) {
    mm_lex$lex();
    if (((i64)mm_decls$lx.symbol == (i64)101)) {
        mm_lex$lex();
        return (struct mm_decls$unitrec *)mm_lib$createunit0((i64)109);
    }
    else {
        return (struct mm_decls$unitrec *)mm_lib$createunit1((i64)109,(struct mm_decls$unitrec *)mm_parse$readunit());
    }
;
}

static void mm_parse$adddocstring(u8 *s) {
    if ((mm_decls$ndocstrings > (i64)20)) {
        mm_support$serror((byte*)"Too many docstrings");
    }
;
    mm_decls$docstrings[(++(mm_decls$ndocstrings))-1] = mlib$pcm_copyheapstringn(s,strlen(s));
}

static struct mm_decls$unitrec *mm_parse$fixcond(struct mm_decls$unitrec *p) {
    if ((((i64)(*p).tag == (i64)4) && ((*p).a == 0))) {
        mm_support$serror((byte*)"Empty cond");
    }
;
    if (!(!!((i64)mm_tables$isbooltag[((i64)(*p).tag)]))) {
        mm_lib$insertunit(p,(i64)14);
        (*p).pclop = (i64)33;
    }
;
    return p;
}

static struct mm_decls$unitrec *mm_parse$readsunit(i64 inwhile) {
        i64 pos;
        i64 sym;
        i64 opc;
        struct mm_decls$unitrec *  ulist;
        struct mm_decls$unitrec *  ulistx;
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  q;
        struct mm_decls$unitrec *  r;
        struct mm_decls$strec *  stname;
    pos = (i64)mm_decls$lx.pos;
    ulist = (ulistx = 0);
    L679 :;
    do {
        L682 :;
        while (((i64)mm_decls$lx.symbol == (i64)6)) {
            mm_lex$lex();
L683 :;
        }
L684 :;
        ;
        switch ((i64)mm_decls$lx.symbol) {
        case 157:;
            {
                mm_lex$lex();
                if (((i64)mm_decls$lx.symbol == (i64)142 || (i64)mm_decls$lx.symbol == (i64)141)) {
                    opc = (i64)mm_decls$lx.symbol;
                    mm_lex$lex();
                }
                else {
                    opc = (i64)0;
                }
;
                mm_parse$readvardef((struct mm_decls$strec *)mm_decls$currproc,(i64)0,(i64)1,(i64)11,opc);
            }
            break;
        case 130:;
        case 131:;
            {
                mm_parse$readprocdef((struct mm_decls$strec *)mm_decls$currproc,(i64)0,(i64)0);
            }
            break;
        case 93:;
        case 140:;
        case 97:;
        case 94:;
        case 145:;
        case 143:;
        case 15:;
            {
                if (((i64)mm_decls$nextlx.symbol == (i64)13 || (i64)mm_decls$nextlx.symbol == (i64)22 || (i64)mm_decls$nextlx.symbol == (i64)2)) {
                    goto L685 ;
;
                }
                else {
                    sym = (i64)0;
                    goto L686 ;
;
                }
;
            }
            break;
        case 144:;
            {
                mm_parse$lexchecksymbol((i64)15);
                sym = (i64)0;
                goto L686 ;
;
            }
            break;
        case 141:;
        case 142:;
            {
                sym = (i64)mm_decls$lx.symbol;
                mm_lex$lex();
                //dovar:
L686 :;
;
                q = mm_parse$readvardef((struct mm_decls$strec *)mm_decls$currproc,(i64)0,(i64)0,(i64)12,sym);
                L687 :;
                while (!!(q)) {
                    r = (*q).nextunit;
                    (*q).nextunit = 0;
                    mm_lib$addlistunit(&ulist,&ulistx,q);
                    q = r;
L688 :;
                }
L689 :;
                ;
            }
            break;
        case 137:;
            {
                mm_parse$readtypedef((struct mm_decls$strec *)mm_decls$currproc,(i64)0);
            }
            break;
        case 149:;
            {
                mm_parse$readconstdef((struct mm_decls$strec *)mm_decls$currproc,(i64)0);
            }
            break;
        case 152:;
        case 133:;
            {
                mm_parse$readclassdef((struct mm_decls$strec *)mm_decls$currproc,(i64)0);
            }
            break;
        case 70:;
            {
                mm_parse$adddocstring(mm_decls$lx.svalue);
                mm_lex$lex();
            }
            break;
        case 146:;
            {
                mm_parse$readmacrodef((struct mm_decls$strec *)mm_decls$currproc,(i64)0);
            }
            break;
        case 162:;
            {
                mm_parse$readtabledef((struct mm_decls$strec *)mm_decls$currproc,(i64)0);
            }
            break;
        case 68:;
            {
                msysc$m_print_startcon();
                msysc$m_print_str((*mm_decls$currproc).name,NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                mm_support$serror((byte*)"Unexpected EOF in proc");
            }
            break;
        case 14:;
        case 99:;
        case 100:;
        case 101:;
        case 117:;
        case 110:;
        case 102:;
        case 103:;
        case 105:;
            {
                goto L681 ;
            }
            break;
        case 81:;
            {
                                {i64 $temp = (i64)mm_decls$nextlx.symbol;
if (($temp==(i64)8)) {
                    p = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)98);
                    stname = (struct mm_decls$strec *)mm_lib$getduplnameptr(mm_decls$currproc,(struct mm_decls$strec *)mm_decls$lx.symptr,(i64)17);
                    mm_lib$adddef(mm_decls$currproc,(struct mm_decls$strec *)stname);
                    (*p).def = (struct mm_decls$strec *)stname;
                    mm_lex$lex();
                    mm_decls$lx.symbol = (i64)6;
                    mm_lib$addlistunit(&ulist,&ulistx,p);
                }
                else if (($temp==(i64)81)) {
                    sym = (i64)141;
                    goto L686 ;
;
                    goto L685 ;
;
                }
                else {
                    goto L685 ;
;
                }
                };
            }
            break;
        case 114:;
            {
                if (!!(inwhile)) {
                    goto L681 ;
                }
;
                goto L685 ;
;
            }
            break;
        case 6:;
            {
            }
            break;
        case 121:;
            {
                goto L681 ;
            }
            break;
        default: {
            //doexec:
L685 :;
;
            p = mm_parse$readunit();
            //doexec2:
L690 :;
;
            if ((((i64)(*p).tag == (i64)3) && ((i64)mm_decls$lx.symbol == (i64)81))) {
                mm_support$serror((byte*)"Possibly var/let needed");
            }
;
            mm_lib$addlistunit(&ulist,&ulistx,p);
            if (((i64)mm_decls$lx.symbol == (i64)114)) {
                goto L681 ;
            }
;
        }
        } //SW
;
L680 :;
    }
    while (!((i64)mm_decls$lx.symbol != (i64)6));
L681 :;
    ;
        {i64 $temp = (i64)mm_decls$lx.symbol;
if (($temp==(i64)14) || ($temp==(i64)99) || ($temp==(i64)100) || ($temp==(i64)101) || ($temp==(i64)117) || ($temp==(i64)110) || ($temp==(i64)114) || ($temp==(i64)102) || ($temp==(i64)103) || ($temp==(i64)105) || ($temp==(i64)5) || ($temp==(i64)20) || ($temp==(i64)121)) {
    }
    else {
        mm_support$serror((byte*)"Readsunit: \";\" expected, or bad unit starter");
    }
    };
    if (((ulist == 0) || !!((*ulist).nextunit))) {
        return (struct mm_decls$unitrec *)mm_lib$createunit1((i64)4,(struct mm_decls$unitrec *)ulist);
    }
    else {
        return ulist;
    }
;
}

// START
void mm_parse$start(void) {

}

i64 mm_support$loadsourcefile(u8 *filespec) {
        u8 *  s;
        u8 *  basefilename;
    if ((mm_decls$nsourcefiles > (i64)1000)) {
        mm_support$loaderror((byte*)"Too many source files",(byte*)"",(byte*)"");
    }
;
    basefilename = mlib$extractfile(filespec);
    ++(mm_decls$nsourcefiles);
    mm_decls$sourcefilespecs[(mm_decls$nsourcefiles)] = mlib$pcm_copyheapstring(filespec);
    mm_decls$sourcefilepaths[(mm_decls$nsourcefiles)] = mlib$pcm_copyheapstring(mlib$extractpath(filespec));
    mm_decls$sourcefilenames[(mm_decls$nsourcefiles)] = mlib$pcm_copyheapstring(basefilename);
    s = (u8 *)mlib$readfile(filespec);
    if (!(!!(s))) {
        mm_support$loaderror((byte*)"LSF can't load ",filespec,(byte*)"");
    }
;
    mm_decls$sourcefiletext[(mm_decls$nsourcefiles)] = s;
    if (!!((i64)mm_decls$fwritema)) {
        mm_decls$sourcefiledupl[(mm_decls$nsourcefiles)] = mlib$pcm_copyheapstring(s);
    }
;
    mm_decls$sourcefilesizes[(mm_decls$nsourcefiles)] = mlib$rfsize;
    (*(s + mlib$rfsize)) = (u64)0u;
    return mm_decls$nsourcefiles;
}

static i64 mm_support$loadbundledfile(u8 *filespec,i64 issyslib,i64 support) {
        i64 fileno;
        u8 *  file;
        i64 i;
    file = mlib$extractfile(filespec);
    for (i=(i64)1;i<=mm_decls$nsourcefiles;++i) {
L691 :;
        if ((!!(mlib$eqstring(file,mm_decls$sourcefilenames[(i)])) && (support == (i64)mm_decls$sourcefilesupport[(i)]))) {
            return i;
        }
;
L692 :;
    }
L693 :;
    ;
    fileno = mm_libsourcesc$findsyslib(file);
    if (!!(fileno)) {
        return fileno;
    }
;
    if (!(!!(issyslib))) {
        mm_support$loaderror((byte*)"Can't find bundled file: ##",filespec,(byte*)"");
    }
;
    return (i64)0;
}

void mm_support$mcerror(u8 *mess) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"MC Error:",NULL);
    msysc$m_print_str(mess,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    exit((i64)1);
}

void mm_support$serror_gen(u8 *mess) {
    mm_support$showdivider('*');
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Syntax Error:",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mm_support$showerrorsource((i64)mm_decls$lx.pos,mm_decls$currproc);
    msysc$m_print_startcon();
    msysc$m_print_str(mess,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mm_support$stopcompiler(mm_decls$sourcefilespecs[((i64)msysc$m_getdotslice((i64)mm_decls$lx.pos,(i64)24,(i64)31))],mm_support$getlineno((u64)(i64)mm_decls$lx.pos));
}

static void mm_support$showdivider(u64 ch) {
        i64 $av_1;
    $av_1 = (i64)87;
    while ($av_1-- > 0) {
L694 :;
        msysc$m_print_startcon();
        msysc$m_print_c8(ch,NULL);
        msysc$m_print_end();
        ;
L695 :;
    }
L696 :;
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

static void mm_support$showerrorsource(i64 pos,struct mm_decls$strec *stproc) {
        i64 fileno;
        i64 lineoffset;
        u8 *  errorline;
        u8 *  s;
        i64 $av_1;
        i64 $av_2;
        i64 $av_3;
    fileno = mm_support$getfileno((u64)pos);
    msysc$m_print_startcon();
    msysc$m_print_setfmt((byte*)"    Line:     #");
    msysc$m_print_i64(mm_support$getlineno((u64)pos),NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    if ((!!(stproc) && ((i64)(*stproc).nameid == (i64)6))) {
        msysc$m_print_startcon();
        msysc$m_print_setfmt((byte*)"    Function: #()");
        msysc$m_print_str((*stproc).name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    msysc$m_print_startcon();
    msysc$m_print_setfmt((byte*)"    Module:   # (#)");
    msysc$m_print_str(mm_decls$sourcefilenames[(fileno)],NULL);
    msysc$m_print_str(mm_decls$sourcefilespecs[(fileno)],NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mm_support$showdivider('-');
    s = (errorline = mm_support$getsourceline((u64)pos));
    lineoffset = (mm_support$getsourcepos((u64)pos) - errorline);
    $av_1 = (i64)6;
    while ($av_1-- > 0) {
L697 :;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)" ",NULL);
        msysc$m_print_end();
        ;
L698 :;
    }
L699 :;
    ;
    L700 :;
    while (!(((u64)(*s) == (i64)10 || (u64)(*s) == (i64)0))) {
        msysc$m_print_startcon();
        msysc$m_print_c8((u64)(*(s)++),NULL);
        msysc$m_print_end();
        ;
L701 :;
    }
L702 :;
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    s = errorline;
    $av_2 = (i64)6;
    while ($av_2-- > 0) {
L703 :;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)" ",NULL);
        msysc$m_print_end();
        ;
L704 :;
    }
L705 :;
    ;
    $av_3 = lineoffset;
    while ($av_3-- > 0) {
L706 :;
        if (((i64)(u64)(*s) == (i64)9)) {
            msysc$m_print_startcon();
            msysc$m_print_c8((u64)9u,NULL);
            msysc$m_print_end();
            ;
        }
        else {
            msysc$m_print_startcon();
            msysc$m_print_c8(' ',NULL);
            msysc$m_print_end();
            ;
        }
;
        ++(s);
L707 :;
    }
L708 :;
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"^",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mm_support$showdivider('-');
}

void mm_support$stopcompiler(u8 *filename,i64 lineno) {
        void *  f;
    f = fopen((byte*)"$error.tmp",(byte*)"w");
    msysc$m_print_startfile(f);
    msysc$m_print_str(filename,NULL);
    msysc$m_print_i64(lineno,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    fclose(f);
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    exit((i64)1);
}

void mm_support$serror(u8 *mess) {
    mm_support$serror_gen(mess);
}

void mm_support$serror_s(u8 *mess,u8 *a) {
        u8 str[256];
    msysc$m_print_startstr(str);
    msysc$m_print_setfmt(mess);
    msysc$m_print_str(a,NULL);
    msysc$m_print_end();
    ;
    mm_support$serror_gen((u8 *)str);
}

void mm_support$error_gen(i64 pass,u8 *mess,struct mm_decls$unitrec *p) {
        i64 pos;
    if (!!(p)) {
        pos = (i64)(*p).pos;
    }
    else {
        pos = mm_tables$mlineno;
    }
;
    mm_support$showdivider('*');
    if ((pass==(i64)78)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"RX Name Error: ",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
    else if ((pass==(i64)84)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"TX Type Error: ",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
    else if ((pass==(i64)71)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"GX Code Gen Error: ",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
    else if ((pass==(i64)65)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"AX Code Gen Error: ",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    mm_support$showerrorsource(pos,mm_decls$currproc);
    msysc$m_print_startcon();
    msysc$m_print_str(mess,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mm_support$stopcompiler(mm_decls$sourcefilespecs[(mm_support$getfileno((u64)pos))],mm_support$getlineno((u64)pos));
}

void mm_support$rxerror(u8 *mess,struct mm_decls$unitrec *p) {
    mm_support$error_gen((i64)78,mess,p);
}

i64 mm_support$gerror(u8 *mess,struct mm_decls$unitrec *p) {
    mm_support$error_gen((i64)71,mess,p);
    return (i64)0;
}

void mm_support$axerror(u8 *mess) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"ALINENO=",NULL);
    msysc$m_print_i64(mc_decls$alineno,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mm_support$error_gen((i64)65,mess,0);
}

void mm_support$txerror(u8 *mess,struct mm_decls$unitrec *p) {
    mm_support$error_gen((i64)84,mess,p);
}

void mm_support$txerror_s(u8 *mess,u8 *a,struct mm_decls$unitrec *p) {
        u8 str[256];
    msysc$m_print_startstr(str);
    msysc$m_print_setfmt(mess);
    msysc$m_print_str(a,NULL);
    msysc$m_print_end();
    ;
    mm_support$error_gen((i64)84,(u8 *)str,p);
}

void mm_support$txerror_ss(u8 *mess,u8 *a,u8 *b) {
        u8 str[256];
    msysc$m_print_startstr(str);
    msysc$m_print_setfmt(mess);
    msysc$m_print_str(a,NULL);
    msysc$m_print_str(b,NULL);
    msysc$m_print_end();
    ;
    mm_support$error_gen((i64)84,(u8 *)str,0);
}

void mm_support$rxerror_s(u8 *mess,u8 *a,struct mm_decls$unitrec *p) {
        u8 str[256];
    msysc$m_print_startstr(str);
    msysc$m_print_setfmt(mess);
    msysc$m_print_str(a,NULL);
    msysc$m_print_end();
    ;
    mm_support$error_gen((i64)78,(u8 *)str,p);
}

void mm_support$gerror_s(u8 *mess,u8 *s,struct mm_decls$unitrec *p) {
        u8 str[256];
    msysc$m_print_startstr(str);
    msysc$m_print_setfmt(mess);
    msysc$m_print_str(s,NULL);
    msysc$m_print_end();
    ;
    mm_support$error_gen((i64)71,(u8 *)str,p);
}

void mm_support$gerror_t(u8 *mess,struct mm_decls$unitrec *p) {
        u8 str[256];
    msysc$m_print_startstr(str);
    msysc$m_print_setfmt(mess);
    msysc$m_print_str(mm_lib$strmode((i64)(*p).mode,(i64)1),NULL);
    msysc$m_print_end();
    ;
    mm_support$error_gen((i64)71,(u8 *)str,p);
}

void mm_support$lxerror_gen(u8 *mess) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"On line",NULL);
    msysc$m_print_i64(mm_support$getlineno((u64)(i64)mm_decls$lx.pos),NULL);
    msysc$m_print_str((byte*)"in file",NULL);
    msysc$m_print_str(mm_decls$sourcefilespecs[((i64)msysc$m_getdotslice((i64)mm_decls$lx.pos,(i64)24,(i64)31))],NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"**** Lex Error:",NULL);
    msysc$m_print_str(mess,NULL);
    msysc$m_print_str((byte*)"****",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mm_support$stopcompiler(mm_decls$sourcefilespecs[((i64)msysc$m_getdotslice((i64)mm_decls$lx.pos,(i64)24,(i64)31))],mm_support$getlineno((u64)(i64)mm_decls$lx.pos));
}

void mm_support$lxerror(u8 *mess) {
    mm_support$lxerror_gen(mess);
}

void mm_support$loaderror(u8 *mess,u8 *mess2,u8 *mess3) {
        u8 str[512];
    if (!!(strchr(mess,(i32)'#'))) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt(mess);
        msysc$m_print_str(mess2,NULL);
        msysc$m_print_str(mess3,NULL);
        msysc$m_print_end();
        ;
    }
    else {
        msysc$m_print_startstr(str);
        msysc$m_print_str(mess,NULL);
        msysc$m_print_end();
        ;
    }
;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Load Error:",NULL);
    msysc$m_print_str(str,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Stopping",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    exit((i64)1);
}

void mm_support$gs_additem(struct mlib$strbuffer *dest,u8 *s) {
        u8 *  d;
        i64 lastchar;
        i64 nextchar;
    d = (*dest).strptr;
    if (!!((i64)(*dest).length)) {
        lastchar = (i64)(u64)(*((d + (i64)(*dest).length) - (i64)1));
        nextchar = (i64)(u64)(*s);
        if ((!!(mm_support$isalphanum(lastchar)) && !!(mm_support$isalphanum(nextchar)))) {
            mlib$strbuffer_add((struct mlib$strbuffer *)dest,(byte*)" ",(i64)-1);
        }
;
    }
;
    mlib$strbuffer_add((struct mlib$strbuffer *)dest,s,(i64)-1);
}

void mm_support$gs_copytostr(struct mlib$strbuffer *source,u8 *s) {
    if (!!((i64)(*source).length)) {
        memcpy((void *)s,(void *)(*source).strptr,(u64)(i64)(*source).length);
        (*(s + (i64)(*source).length)) = (u64)0u;
    }
    else {
        (*s) = (u64)0u;
    }
;
}

i64 mm_support$isalphanum(i64 c) {
    if (((((c >= (i64)65) && (c <= (i64)90)) || ((c >= (i64)97) && (c <= (i64)122))) || ((c >= (i64)48) && (c <= (i64)57)))) {
        return (i64)1;
    }
;
    return (i64)0;
}

void mm_support$init_tt_tables(void) {
        i64 i;
        i64 size;
        i64 bitsize;
    for (i=(i64)0;i<=(i64)28;++i) {
L709 :;
        mm_decls$ttname[(i)] = mm_tables$stdnames[(i)];
        mm_decls$ttbasetype[(i)] = i;
        bitsize = (i64)mm_tables$stdbits[(i)];
        switch (bitsize) {
        case 0:;
            {
                size = (i64)0;
            }
            break;
        case 1:;
        case 2:;
        case 4:;
            {
                size = (i64)1;
            }
            break;
        default: {
            size = (bitsize / (i64)8);
        }
        } //SW
;
        mm_decls$ttsize[(i)] = size;
        if ((i==(i64)14) || (i==(i64)15) || (i==(i64)16) || (i==(i64)3)) {
            mm_decls$ttsigned[(i)] = (i64)1;
            mm_decls$ttisinteger[(i)] = (i64)1;
        }
        else if ((i==(i64)17) || (i==(i64)18) || (i==(i64)19) || (i==(i64)2) || (i==(i64)12) || (i==(i64)1)) {
            mm_decls$ttisinteger[(i)] = (i64)1;
        }
        else if ((i==(i64)4) || (i==(i64)5)) {
            mm_decls$ttisreal[(i)] = (i64)1;
        }
        else if ((i==(i64)7) || (i==(i64)20)) {
            mm_decls$ttisref[(i)] = (i64)1;
        }
;
        mm_decls$ttisshort[(i)] = (i64)((i64)mm_tables$stdcat[(i)] == (i64)4);
        mm_decls$ttlower[(i)] = (i64)1;
        mm_decls$ttcat[(i)] = (i64)mm_tables$stdcat[(i)];
        mm_decls$ttisblock[(i)] = (i64)((i64)mm_tables$stdcat[(i)] == (i64)5);
L710 :;
    }
L711 :;
    ;
    mm_decls$ttbasetype[((i64)20)] = (i64)7;
    mm_decls$tttarget[((i64)20)] = (i64)12;
    mm_decls$ntypes = (i64)28;
}

void mm_support$addspecialtypes(void) {
    mm_tables$trefproc = mm_lib$createrefmode(0,(i64)23,(i64)0);
    mm_tables$treflabel = mm_lib$createrefmode(0,(i64)24,(i64)0);
}

i64 mm_support$getsupportfile(u8 *filename,u8 *ext,u8 *path,i64 issyslib,i64 issupport) {
        u8 filespec[300];
        u8 filespec2[300];
        u8 *  file;
        i64 fileno;
    file = filename;
    if ((mm_decls$fverbose == (i64)3)) {
        msysc$m_print_startcon();
        msysc$m_print_setfmt((byte*)"Get file:# (ext:#) (path:#)");
        msysc$m_print_str(filename,NULL);
        msysc$m_print_str(ext,NULL);
        msysc$m_print_str(path,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    if (!!((u64)(*ext))) {
        strcpy(filespec,mlib$addext(filename,ext));
        file = (u8 *)filespec;
    }
;
    if (!!((i64)mm_decls$freadma)) {
        fileno = mm_support$loadbundledfile(file,issyslib,issupport);
        if (!!(fileno)) {
            return fileno;
        }
;
    }
;
    if ((!!(issyslib) && !!((i64)mm_decls$dointlibs))) {
        fileno = mm_libsourcesc$findsyslib(file);
        if (((mm_decls$fverbose == (i64)3) && !!(fileno))) {
            msysc$m_print_startcon();
            msysc$m_print_setfmt((byte*)"Found in syslib: #");
            msysc$m_print_str(mm_decls$sourcefilenames[(fileno)],NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
;
        if (!!(fileno)) {
            return fileno;
        }
;
    }
;
    if (!(!!(mm_support$isabspath(file)))) {
        strcpy(filespec2,path);
        strcat(filespec2,file);
        file = (u8 *)filespec2;
    }
;
    if (((mm_decls$fverbose == (i64)3) && !!(fileno))) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Checkfile:",NULL);
        msysc$m_print_str(file,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    if (((file == 0) || !(!!(mlib$checkfile(file))))) {
        mm_support$loaderror((byte*)"Can't find file: # #",filename,(byte*)"");
    }
;
    fileno = mm_support$loadsourcefile(file);
    if (((mm_decls$fverbose == (i64)3) && !!(fileno))) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Found:",NULL);
        msysc$m_print_str(file,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    mm_decls$sourcefilesupport[(fileno)] = issupport;
    mm_decls$sourcefilesys[(fileno)] = issyslib;
    return fileno;
}

static i64 mm_support$isabspath(u8 *filespec) {
        u8 *  path;
    path = mlib$extractpath(filespec);
    if ((((u64)(*path) == (u64)92u || (u64)(*path) == '/') || (((i64)(u64)(*path) != (i64)0) && ((u64)(*(path + (i64)1)) == ':')))) {
        return (i64)1;
    }
;
    return (i64)0;
}

void mm_support$initbblib(void) {
        i64 i;
    for (i=(i64)1;i<=(i64)9;++i) {
L712 :;
        mm_decls$typestarterset[(mm_tables$d_typestarterset[(i)-1])] = (i64)1;
L713 :;
    }
L714 :;
    ;
}

i64 mm_support$getfileno(u64 pos) {
        i64 fileno;
    fileno = (i64)msysc$m_getdotslice(pos,(i64)24,(i64)31);
    if (((fileno < (i64)1) || (fileno > mm_decls$nsourcefiles))) {
        return (i64)1;
    }
;
    return fileno;
}

i64 mm_support$getlineno(u64 pos) {
        u8 *  source;
        u8 *  sline;
        u8 *  s;
        i64 lineno;
    source = mm_support$getsourcestart(pos);
    sline = mm_support$getsourceline(pos);
    s = sline;
    lineno = (i64)1;
    L715 :;
    while ((s > source)) {
        if (((i64)(u64)(*s) == (i64)10)) {
            ++(lineno);
        }
;
        --(s);
L716 :;
    }
L717 :;
    ;
    return lineno;
}

static u8 *mm_support$getsourceline(u64 pos) {
        u8 *  source;
        u8 *  s;
    source = mm_support$getsourcestart(pos);
    s = mm_support$getsourcepos(pos);
    L718 :;
    while (((s > source) && ((i64)(u64)(*s) != (i64)10))) {
        --(s);
L719 :;
    }
L720 :;
    ;
    if (((i64)(u64)(*s) == (i64)10)) {
        ++(s);
    }
;
    return s;
}

static u8 *mm_support$getsourcestart(u64 pos) {
    return mm_decls$sourcefiletext[(mm_support$getfileno(pos))];
}

static u8 *mm_support$getsourcepos(u64 pos) {
    return (mm_decls$sourcefiletext[(mm_support$getfileno(pos))] + (i64)msysc$m_getdotslice(pos,(i64)0,(i64)23));
}

void mm_support$do_writema(void) {
        u8 filename[300];
        i64 sflist[1000];
        void *  f;
        i64 offset;
        i64 nfiles;
        i64 fileno;
        i64 i;
    if (!(!!((i64)mm_decls$fwritema))) {
        return;
    }
;
    strcpy(filename,mlib$changeext(mm_decls$sourcefilespecs[((i64)1)],(byte*)"ma"));
    nfiles = (i64)0;
    for (i=(i64)1;i<=mm_decls$nsourcefiles;++i) {
L721 :;
        if ((!!((i64)mm_decls$sourcefilesys[(i)]) && ((i64)mm_decls$fwritema == (i64)1))) {
            goto L722 ;
        }
;
        sflist[(++(nfiles))-1] = i;
L722 :;
    }
L723 :;
    ;
    if ((nfiles == (i64)0)) {
        mm_support$loaderror((byte*)"MA: no files",(byte*)"",(byte*)"");
    }
;
    f = fopen(filename,(byte*)"wb");
    if (!(!!(f))) {
        mm_support$loaderror((byte*)"Can't create MA file #",filename,(byte*)"");
    }
;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Writing ",NULL);
    msysc$m_print_str(filename,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_setfmt((byte*)"=== MA # ===");
    msysc$m_print_i64(nfiles,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=nfiles;++i) {
L724 :;
        fileno = sflist[(i)-1];
        msysc$m_print_startfile(f);
        msysc$m_print_setfmt((byte*)"=== # # # #/# ===");
        msysc$m_print_str(mm_decls$sourcefilenames[(fileno)],NULL);
        msysc$m_print_i64((i64)mm_decls$sourcefilesys[(fileno)],NULL);
        msysc$m_print_i64((i64)mm_decls$sourcefilesupport[(fileno)],NULL);
        msysc$m_print_i64(i,NULL);
        msysc$m_print_i64(nfiles,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        offset = mlib$getfilepos(f);
        mlib$writerandom(f,(byte *)mm_decls$sourcefiledupl[(fileno)],offset,mm_decls$sourcefilesizes[(fileno)]);
L725 :;
    }
L726 :;
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"=== END ===",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=nfiles;++i) {
L727 :;
        msysc$m_print_startfile(f);
        msysc$m_print_setfmt((byte*)"# # #");
        msysc$m_print_i64(i,NULL);
        msysc$m_print_str(mm_decls$sourcefilenames[(sflist[(i)-1])],NULL);
        msysc$m_print_i64((i64)mm_decls$sourcefilesys[(sflist[(i)-1])],NULL);
        msysc$m_print_i64((i64)mm_decls$sourcefilesupport[(sflist[(i)-1])],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L728 :;
    }
L729 :;
    ;
    fclose(f);
}

// START
void mm_support$start(void) {

}

// START
void mm_tables$start(void) {

        i64 i;
    for (i=(i64)1;i<=(i64)8;++i) {
L730 :;
        mm_tables$intresult[((i64)mm_tables$intresultlist[(i)-1])] = (i64)1;
L731 :;
    }
L732 :;
    ;
    for (i=(i64)1;i<=(i64)12;++i) {
L733 :;
        mm_tables$endsexpr[((i64)mm_tables$exprendsymbols[(i)-1])-1] = (i64)1;
L734 :;
    }
L735 :;
    ;
    mm_tables$isbooltag[((i64)29)] = (i64)1;
    mm_tables$isbooltag[((i64)30)] = (i64)1;
    mm_tables$isbooltag[((i64)11)] = (i64)1;
    mm_tables$isbooltag[((i64)12)] = (i64)1;
    mm_tables$isbooltag[((i64)13)] = (i64)1;
    mm_tables$isbooltag[((i64)14)] = (i64)1;
    mm_tables$isbooltag[((i64)37)] = (i64)1;
    mm_tables$isbooltag[((i64)38)] = (i64)1;
    mm_tables$ismemtag[((i64)3)] = (i64)1;
    mm_tables$ismemtag[((i64)40)] = (i64)1;
    mm_tables$ismemtag[((i64)42)] = (i64)1;
    mm_tables$ismemtag[((i64)45)] = (i64)1;
    mm_tables$ismemtag[((i64)46)] = (i64)2;
    mm_tables$ismemtag[((i64)47)] = (i64)2;
    mm_tables$ismemtag[((i64)1)] = (i64)2;
    mm_tables$ismemtag[((i64)43)] = (i64)1;
    mm_tables$ismemtag[((i64)44)] = (i64)1;
    for (i=(i64)1;i<=(i64)15;++i) {
L736 :;
        mm_tables$complexopset[((i64)mm_tables$complexops[(i)-1])] = (i64)1;
L737 :;
    }
L738 :;
    ;
}

static void mm_type$tpass(struct mm_decls$unitrec *p,i64 t,i64 lv) {
        struct mm_decls$unitrec *  a;
        struct mm_decls$unitrec *  b;
        struct mm_decls$unitrec *  c;
        i64 oldmlineno;
        i64 m;
        i64 paramtype;
        i64 restype;
        i64 $av_1;
        i64 i;
    if ((p == 0)) {
        return;
    }
;
    oldmlineno = mm_tables$mlineno;
    mm_tables$mlineno = (i64)(*p).pos;
    a = (*p).a;
    b = (*p).b;
    c = (*p).c;
    (*p).resultflag = (i64)(t != (i64)0);
    switch ((i64)(*p).tag) {
    case 3:;
        {
            mm_type$tx_name(p,t,lv);
        }
        break;
    case 1:;
        {
        }
        break;
    case 52:;
        {
            (*p).mode = (i64)3;
        }
        break;
    case 56:;
    case 55:;
        {
            mm_type$tpass(a,(i64)22,(i64)0);
            (*p).mode = (i64)3;
        }
        break;
    case 31:;
    case 29:;
        {
            mm_type$tx_bin(p,a,b);
        }
        break;
    case 32:;
        {
            mm_type$tx_unary(p,a);
        }
        break;
    case 33:;
        {
            mm_type$tx_binto(p,a,b);
        }
        break;
    case 34:;
        {
            mm_type$tpasslv(a,(i64)22);
            (*p).mode = (i64)0;
        }
        break;
    case 23:;
        {
            mm_type$tx_assign(p,a,b,t);
        }
        break;
    case 46:;
        {
            if (((i64)(*a).tag == (i64)45)) {
                mm_lib$deleteunit(p,a);
                mm_lib$deleteunit(p,(*p).a);
                mm_type$tpass(p,t,(i64)0);
            }
            else {
                mm_type$tpasslv(a,(i64)22);
                (*p).mode = mm_lib$createrefmode(0,(i64)(*a).mode,(i64)0);
                mm_type$setsimple(p);
            }
;
        }
        break;
    case 47:;
        {
            mm_type$tx_addroffirst(p,a,t);
        }
        break;
    case 90:;
        {
            mm_type$tx_if(p,a,b,c,t,lv);
        }
        break;
    case 40:;
        {
            mm_type$tx_index(p,a,b,t,lv);
        }
        break;
    case 45:;
        {
            mm_type$tx_ptr(p,a,t,lv);
        }
        break;
    case 86:;
    case 28:;
        {
            mm_type$tx_callproc(p,a,b,t);
        }
        break;
    case 42:;
        {
            mm_type$tx_dot(p,a,b,lv);
        }
        break;
    case 11:;
    case 12:;
        {
            mm_type$tx_andl(p,a,b);
        }
        break;
    case 13:;
        {
            mm_type$tx_notl(p,a);
        }
        break;
    case 14:;
        {
            mm_type$tx_istruel(p,a);
        }
        break;
    case 48:;
        {
            mm_type$tx_convert(p,a,(i64)1);
        }
        break;
    case 51:;
        {
            mm_type$tx_typepun(p,a);
        }
        break;
    case 35:;
        {
            mm_type$tx_incrto(p,a,t);
        }
        break;
    case 16:;
        {
            mm_type$tx_makerange(p,a,b);
        }
        break;
    case 107:;
        {
            mm_type$tx_swap(p,a,b);
        }
        break;
    case 108:;
        {
            mm_type$tx_select(p,a,b,c,t,lv);
        }
        break;
    case 105:;
    case 106:;
        {
            mm_type$tx_switch(p,a,b,c,t,lv);
        }
        break;
    case 103:;
    case 104:;
        {
            mm_type$tx_case(p,a,b,c,t,lv);
        }
        break;
    case 43:;
    case 44:;
        {
            mm_type$tx_dotindex(p,a,b,lv);
        }
        break;
    case 41:;
        {
            mm_type$tx_slice(p,a,b);
        }
        break;
    case 4:;
        {
            mm_type$tx_block(p,a,t,lv);
        }
        break;
    case 121:;
        {
            mm_type$tpass(a,(i64)22,(i64)0);
        }
        break;
    case 102:;
        {
            mm_type$tpass(a,(i64)0,(i64)0);
        }
        break;
    case 87:;
        {
            mm_type$tx_return(p,a,t);
        }
        break;
    case 110:;
    case 111:;
    case 112:;
    case 113:;
        {
            mm_type$tx_unitlist(a,(i64)22,(i64)0);
            mm_type$fixchararray(a);
            L739 :;
            while (!!(b)) {
                if (((i64)(*b).tag == (i64)83)) {
                    mm_type$tpass((c = (*b).a),(i64)22,(i64)0);
                    mm_type$tpass((*b).b,(i64)20,(i64)0);
                }
                else {
                    mm_type$tpass((c = b),(i64)22,(i64)0);
                }
;
                mm_type$fixchararray(c);
                b = (*b).nextunit;
L740 :;
            }
L741 :;
            ;
            mm_type$tx_unitlist((*p).c,(i64)22,(i64)0);
        }
        break;
    case 91:;
    case 92:;
        {
            mm_type$tx_for(a,b,c);
        }
        break;
    case 93:;
    case 94:;
        {
            mm_type$tx_forall(a,b,c);
        }
        break;
    case 89:;
        {
            mm_type$tpass(a,(i64)3,(i64)0);
            mm_type$tpass(b,(i64)0,(i64)0);
            mm_type$tpass(c,(i64)3,(i64)0);
        }
        break;
    case 50:;
        {
            mm_type$tpass(a,(i64)22,(i64)0);
            if ((t == (i64)22)) {
                mm_support$txerror((byte*)"cast() needs type",0);
            }
;
            mm_type$coerceunit(a,t,(i64)1);
            mm_lib$deleteunit(p,a);
        }
        break;
    case 15:;
        {
            mm_type$tx_makelist(p,a,t,lv);
        }
        break;
    case 120:;
        {
            mm_type$tpass(a,(i64)3,(i64)0);
        }
        break;
    case 101:;
    case 99:;
    case 100:;
        {
            mm_type$tx_exit(p,a);
        }
        break;
    case 97:;
        {
            mm_type$tx_goto(p,a);
        }
        break;
    case 98:;
        {
        }
        break;
    case 95:;
        {
            mm_type$tpass(a,(i64)22,(i64)0);
            if (!!(mm_type$iscondtrue(a))) {
                (*p).tag = (i64)102;
                (*p).a = b;
            }
            else if (!!(mm_type$iscondfalse(a))) {
                (*p).tag = (i64)2;
            }
;
            mm_type$tpass(b,(i64)0,(i64)0);
            mm_type$tpass(c,(i64)0,(i64)0);
        }
        break;
    case 96:;
        {
            mm_type$tpass(a,(i64)0,(i64)0);
            mm_type$tpass(b,(i64)22,(i64)0);
            if ((!!(mm_type$iscondtrue(b)) || !!(mm_type$iscondfalse(b)))) {
                mm_support$txerror((byte*)"repeat/const cond",0);
            }
;
        }
        break;
    case 84:;
    case 85:;
        {
        }
        break;
    case 5:;
        {
            if ((t != (i64)0)) {
                (*p).mode = t;
            }
;
            mm_type$inassem = (i64)1;
            mm_type$tx_unitlist(a,(i64)22,(i64)0);
            mm_type$tx_unitlist(b,(i64)22,(i64)0);
            mm_type$tx_unitlist(c,(i64)22,(i64)0);
            mm_type$inassem = (i64)0;
        }
        break;
    case 7:;
    case 8:;
        {
        }
        break;
    case 9:;
        {
            mm_type$tpass(a,(i64)22,(i64)0);
        }
        break;
    case 57:;
        {
            mm_type$tpass(a,(i64)22,(i64)0);
            if (((i64)(*a).tag == (i64)52)) {
                (*p).value = (*a).value;
            }
            else {
                (*p).value = (i64)(*a).mode;
            }
;
            (*p).tag = (i64)52;
            (*p).mode = (i64)3;
        }
        break;
    case 58:;
        {
            mm_type$tpass(a,(i64)22,(i64)0);
            if (((i64)(*a).tag == (i64)52)) {
                m = (*a).value;
            }
            else {
                mm_type$tpass(a,(i64)22,(i64)0);
                m = (i64)(*a).mode;
            }
;
            (*p).tag = (i64)1;
            (*p).mode = (i64)20;
            (*p).a = 0;
            (*p).svalue = mlib$pcm_copyheapstring(mm_lib$strmode(m,(i64)0));
            (*p).slength = strlen((*p).svalue);
            (*p).isastring = (i64)1;
        }
        break;
    case 83:;
        {
            mm_type$tpass(a,(i64)22,(i64)0);
            mm_type$tpass(b,(i64)22,(i64)0);
        }
        break;
    case 117:;
        {
            mm_type$tpass(a,(i64)22,(i64)0);
        }
        break;
    case 116:;
        {
            if (!!(a)) {
                mm_type$tpass(a,(i64)1,(i64)0);
            }
;
            if ((!!((i64)mm_decls$ttisinteger[(t)]) || !!((i64)mm_decls$ttisreal[(t)]))) {
                t = mm_lib$gettypebase(t);
            }
;
            (*p).mode = t;
        }
        break;
    case 109:;
        {
            if (!!(a)) {
                mm_type$tpass(a,(i64)3,(i64)0);
                if (((i64)(*a).tag != (i64)1)) {
                    mm_support$txerror((byte*)"recase must be const",0);
                }
;
            }
;
        }
        break;
    case 62:;
        {
            (*p).mode = (i64)3;
        }
        break;
    case 65:;
    case 64:;
        {
            (*p).mode = (i64)20;
        }
        break;
    case 59:;
        {
            mm_type$tx_bitfield(p,a,lv);
        }
        break;
    case 88:;
        {
            restype = (i64)0;
            paramtype = (i64)0;
                        {i64 $temp = (i64)(*p).fnindex;
if (($temp==(i64)29)) {
                restype = (i64)3;
            }
            else if (($temp==(i64)30)) {
                paramtype = (i64)3;
                restype = (i64)20;
            }
            else if (($temp==(i64)31)) {
                paramtype = (i64)3;
                restype = (i64)7;
            }
            else if (($temp==(i64)32) || ($temp==(i64)33) || ($temp==(i64)34)) {
                restype = (i64)7;
            }
            };
            if ((paramtype != (i64)0)) {
                if ((a == 0)) {
                    mm_support$txerror((byte*)"sys: arg missing",0);
                }
;
                mm_type$tpass(a,paramtype,(i64)0);
                if (!!((*a).nextunit)) {
                    mm_support$txerror((byte*)"sys: too many args",0);
                }
;
            }
            else if (!!(a)) {
                mm_support$txerror((byte*)"sys: too many args",0);
            }
;
            (*p).mode = restype;
        }
        break;
    case 30:;
        {
            mm_type$tx_cmpchain(p,a);
        }
        break;
    case 122:;
        {
            mm_type$tpasslv(a,(i64)22);
        }
        break;
    case 49:;
        {
        }
        break;
    case 10:;
        {
            mm_type$tx_strinclude(p,a);
        }
        break;
    case 19:;
        {
            mm_type$tx_makeslice(p,a,t);
        }
        break;
    case 17:;
        {
            mm_type$tx_makeset(p,a,t);
        }
        break;
    case 123:;
        {
        }
        break;
    case 124:;
        {
        }
        break;
    case 53:;
        {
            (*p).mode = (i64)3;
            (*p).tag = (i64)1;
            (*p).value = (i64)(*p).pclop;
        }
        break;
    default: {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"TXUNIT: CAN'T DO:",NULL);
        msysc$m_print_str(mm_tables$jtagnames[((i64)(*p).tag)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        //doelse:
L742 :;
;
                ($av_1 = (i64)mm_tables$jsubs[((i64)(*p).tag)]);
        for (i=(i64)1;i<=$av_1;++i) {
L743 :;
            mm_type$tx_unitlist((*p).abc[(i)-1],t,(i64)0);
L744 :;
        }
L745 :;
        ;
    }
    } //SW
;
    mm_type$tevaluate(p);
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)15) || ($temp==(i64)87)) {
    }
    else {
        if ((((t != (i64)22) && (t != (i64)0)) && ((i64)(*p).mode != t))) {
            mm_type$coerceunit(p,t,(i64)0);
        }
;
    }
    };
    if ((t == (i64)0)) {
                {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)1) || ($temp==(i64)31) || ($temp==(i64)32) || ($temp==(i64)29)) {
        }
        else if (($temp==(i64)3)) {
            if (!((!!((i64)mm_decls$ttisref[((i64)(*p).mode)]) && ((i64)mm_decls$tttarget[((i64)(*p).mode)] == (i64)24)))) {
            }
;
        }
        };
    }
;
    mm_tables$mlineno = oldmlineno;
}

void mm_type$tx_allprocs(void) {
        struct mm_decls$procrec *  pp;
        struct mm_decls$unitrec *  pcode;
        struct mm_decls$strec *  d;
    pp = (struct mm_decls$procrec *)mm_decls$proclist;
    L746 :;
    while (!!(pp)) {
        mm_decls$currproc = (*pp).def;
        pcode = (*mm_decls$currproc).code;
        if (!!((i64)mm_decls$ttisshort[((i64)(*mm_decls$currproc).mode)])) {
            mm_tables$mlineno = (i64)(*mm_decls$currproc).pos;
            mm_support$txerror((byte*)"proc short ret type",0);
        }
;
        if ((u64)0u) {
            d = (struct mm_decls$strec *)(*mm_decls$currproc).deflist;
            L750 :;
            while (!!(d)) {
                if (((i64)(*d).nameid == (i64)13)) {
                    if ((!!((i64)mm_decls$ttisblock[((i64)(*d).mode)]) && ((i64)(*d).parammode != (i64)2))) {
                        (*d).parammode = (i64)2;
                        (*d).mode = mm_lib$createrefmode(0,(i64)(*d).mode,(i64)0);
                    }
;
                }
;
L751 :;
                d = (struct mm_decls$strec *)(*d).nextdef;
L753 :;
                            }
L752 :;
            ;
        }
;
L747 :;
        pp = (struct mm_decls$procrec *)(*pp).nextproc;
L749 :;
            }
L748 :;
    ;
    pp = (struct mm_decls$procrec *)mm_decls$proclist;
    L754 :;
    while (!!(pp)) {
        mm_decls$currproc = (*pp).def;
        pcode = (*mm_decls$currproc).code;
        mm_type$tpass(pcode,(((i64)(*mm_decls$currproc).nretvalues > (i64)1) ? (i64)27 : (i64)(*mm_decls$currproc).mode),(i64)0);
                {i64 $temp = (i64)mm_decls$ttbasetype[((i64)(*mm_decls$currproc).mode)];
if (($temp==(i64)0)) {
        }
        else if (($temp==(i64)27)) {
        }
        else {
            if (((u64)0u && ((i64)(*pcode).tag != (i64)87))) {
                mm_lib$insertunit(pcode,(i64)87);
                (*pcode).mode = (i64)(*mm_decls$currproc).mode;
                (*pcode).resultflag = (i64)1;
            }
;
        }
        };
        pp = (struct mm_decls$procrec *)(*pp).nextproc;
L755 :;
    }
L756 :;
    ;
}

static void mm_type$tx_block(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 t,i64 lv) {
    L757 :;
    while ((!!(a) && !!((*a).nextunit))) {
        mm_type$tpass(a,(i64)0,(i64)0);
        a = (*a).nextunit;
L758 :;
    }
L759 :;
    ;
    if (!!(a)) {
        mm_type$tpass(a,t,lv);
        (*p).mode = ((t != (i64)0) ? (i64)(*a).mode : (i64)0);
    }
;
}

void mm_type$tx_typetable(void) {
        struct mm_decls$strec *  d;
        i64 i;
    for (i=(i64)29;i<=mm_decls$ntypes;++i) {
L760 :;
        if (((i64)mm_decls$ttbasetype[(i)] == (i64)8)) {
            mm_type$tx_passdef((d = mm_decls$ttnamedef[(i)]));
        }
;
        mm_type$setmodesize(i);
L761 :;
    }
L762 :;
    ;
}

static void mm_type$setmodesize(i64 m) {
        i64 size;
        i64 target;
    if (!!((i64)mm_decls$ttsize[(m)])) {
        return;
    }
;
    mm_tables$mlineno = (i64)mm_decls$ttlineno[(m)];
        {i64 $temp = (i64)mm_decls$ttbasetype[(m)];
if (($temp==(i64)10)) {
        mm_type$setarraysize(m);
    }
    else if (($temp==(i64)8)) {
        mm_type$setrecordsize(m);
    }
    else if (($temp==(i64)0) || ($temp==(i64)23)) {
    }
    else if (($temp==(i64)11)) {
        mm_type$setslicesize(m);
    }
    else if (($temp==(i64)21)) {
        mm_support$txerror((byte*)"SETMODESIZE/AUTO?",0);
    }
    else if (($temp==(i64)22)) {
    }
    else if (($temp==(i64)28)) {
        target = (i64)mm_decls$tttarget[(m)];
        mm_type$setmodesize(target);
        mm_decls$ttbasetype[(m)] = (i64)mm_decls$ttbasetype[(target)];
        mm_decls$ttsize[(m)] = (i64)mm_decls$ttsize[(target)];
        mm_decls$ttlower[(m)] = (i64)mm_decls$ttlower[(target)];
        mm_decls$ttlength[(m)] = (i64)mm_decls$ttlength[(target)];
        mm_decls$ttnamedef[(m)] = mm_decls$ttnamedef[(target)];
    }
    else if (($temp==(i64)27)) {
    }
    else {
        if (!!((size = (i64)mm_decls$ttsize[((i64)mm_decls$ttbasetype[(m)])]))) {
            mm_decls$ttsize[(m)] = size;
            return;
        }
;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"SIZE 0:",NULL);
        msysc$m_print_str(mm_lib$strmode(m,(i64)1),NULL);
        msysc$m_print_str((byte*)"M=",NULL);
        msysc$m_print_i64(m,NULL);
        msysc$m_print_str((byte*)"STDNAMES[TTBASETYPE[M]]=",NULL);
        msysc$m_print_str(mm_tables$stdnames[((i64)mm_decls$ttbasetype[(m)])],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Can't set mode size",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
    };
}

static void mm_type$setarraysize(i64 m) {
        i64 lower;
        i64 length;
        i64 elemsize;
        i64 target;
        i64 size;
        struct mm_decls$unitrec *  pdim;
        struct mm_decls$unitrec *  a;
        struct mm_decls$unitrec *  b;
    if (!!((i64)mm_decls$ttsizeset[(m)])) {
        return;
    }
;
    pdim = mm_decls$ttdimexpr[(m)];
    if (!!(pdim)) {
        a = (*pdim).a;
        b = (*pdim).b;
        mm_name$rx_unit(mm_decls$ttowner[(m)],pdim);
                {i64 $temp = (i64)(*pdim).tag;
if (($temp==(i64)16)) {
            mm_type$tpass(a,(i64)22,(i64)0);
            mm_type$tpass(b,(i64)22,(i64)0);
            lower = mm_type$getconstint(a);
            length = ((mm_type$getconstint(b) - lower) + (i64)1);
        }
        else if (($temp==(i64)22)) {
            mm_type$tpass(a,(i64)22,(i64)0);
            lower = mm_type$getconstint(a);
            if (!!(b)) {
                mm_type$tpass(b,(i64)22,(i64)0);
                length = mm_type$getconstint(b);
            }
            else {
                length = (i64)0;
            }
;
        }
        else {
            mm_type$tpass(pdim,(i64)22,(i64)0);
            length = mm_type$getconstint(pdim);
            lower = (i64)1;
        }
        };
    }
    else {
        lower = (i64)1;
        length = (i64)0;
    }
;
    mm_decls$ttdimexpr[(m)] = 0;
    mm_decls$ttlower[(m)] = lower;
    mm_decls$ttlength[(m)] = length;
    target = (i64)mm_decls$tttarget[(m)];
    mm_type$setmodesize(target);
    elemsize = (i64)mm_decls$ttsize[((i64)mm_decls$tttarget[(m)])];
    mm_decls$ttsize[(m)] = (size = (length * elemsize));
    mm_decls$ttsizeset[(m)] = (i64)1;
    mm_type$checkblocktype(m);
}

static void mm_type$setslicesize(i64 m) {
        struct mm_decls$unitrec *  pdim;
    if (!!((i64)mm_decls$ttsize[(m)])) {
        return;
    }
;
    pdim = mm_decls$ttdimexpr[(m)];
    if (!!(pdim)) {
        mm_name$rx_unit(mm_decls$ttowner[(m)],pdim);
        mm_type$tpass(pdim,(i64)22,(i64)0);
        mm_decls$ttlower[(m)] = mm_type$getconstint(pdim);
        mm_decls$ttdimexpr[(m)] = 0;
    }
    else {
        mm_decls$ttlower[(m)] = (i64)1;
    }
;
    mm_type$setmodesize((i64)mm_decls$tttarget[(m)]);
    mm_decls$ttsize[(m)] = (i64)mm_decls$ttsize[((i64)11)];
}

i64 mm_type$tx_module(i64 n) {
    mm_decls$currmoduleno = n;
    mm_type$tx_passdef(mm_decls$moduletable[(n)].stmodule);
    return (i64)1;
}

void mm_type$tx_passdef(struct mm_decls$strec *p) {
        struct mm_decls$strec *  d;
        i64 oldmlineno;
        struct mm_decls$unitrec *  q;
    if (!!(msysc$m_getdotindex((i64)(*p).flags,(i64)2))) {
        return;
    }
;
    oldmlineno = mm_tables$mlineno;
    mm_tables$mlineno = (i64)(*p).pos;
    d = (struct mm_decls$strec *)(*p).deflist;
    L763 :;
    while (!!(d)) {
        mm_type$tx_passdef(d);
        d = (struct mm_decls$strec *)(*d).nextdef;
L764 :;
    }
L765 :;
    ;
    q = (*p).code;
        {i64 $temp = (i64)(*p).nameid;
if (($temp==(i64)6)) {
        mm_decls$currproc = 0;
    }
    else if (($temp==(i64)10) || ($temp==(i64)16)) {
        mm_type$tx_namedconst(p);
    }
    else if (($temp==(i64)11) || ($temp==(i64)12) || ($temp==(i64)13)) {
        mm_type$tx_namedef(p);
    }
    };
    (*p).flags = msysc$m_setdotindex((*p).flags,(i64)2,(u64)1u);
    mm_tables$mlineno = oldmlineno;
}

static void mm_type$tx_unitlist(struct mm_decls$unitrec *p,i64 t,i64 lv) {
    L766 :;
    while (!!(p)) {
        mm_type$tpass(p,t,(i64)0);
        p = (*p).nextunit;
L767 :;
    }
L768 :;
    ;
}

static void mm_type$tx_namedef(struct mm_decls$strec *d) {
        i64 m;
        i64 mold;
        i64 inidataold;
        struct mm_decls$unitrec *  dcode;
        struct mm_decls$unitrec *  pequiv;
    if (!!(mm_type$deb)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"NAMEDEF1",NULL);
        msysc$m_print_str((*d).name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    m = (i64)(*d).mode;
    mm_type$setmodesize(m);
    if (!!(msysc$m_getdotindex((i64)(*d).flags,(i64)3))) {
        mm_support$txerror((byte*)"Circular reference detected",0);
    }
;
    if (!!(msysc$m_getdotindex((i64)(*d).flags,(i64)2))) {
        return;
    }
;
    dcode = (*d).code;
    (*d).flags = msysc$m_setdotindex((*d).flags,(i64)3,(u64)1u);
    if (!!(msysc$m_getdotindex((i64)(*d).flags,(i64)10))) {
        pequiv = (*d).equivvar;
        if (((i64)(*pequiv).tag == (i64)46)) {
            mm_lib$deleteunit(pequiv,(*pequiv).a);
        }
;
        if (((i64)(*pequiv).tag != (i64)3)) {
            mm_support$txerror((byte*)"@name needed",0);
        }
;
        mm_type$tpass(pequiv,(i64)22,(i64)0);
    }
;
    if ((!!(dcode) && ((i64)(*d).nameid != (i64)12))) {
        mold = m;
        m = mm_lib$gettypebase(m);
        if (((((i64)mm_decls$ttbasetype[(m)] == (i64)11) && ((i64)(*dcode).tag == (i64)1)) && ((i64)(*dcode).mode == (i64)20))) {
            mm_type$tpass(dcode,(i64)20,(i64)0);
        }
        else {
            inidataold = mm_type$inidata;
            mm_type$inidata = (i64)1;
            mm_type$tpass(dcode,m,(i64)0);
            mm_type$inidata = inidataold;
        }
;
        (*d).flags = msysc$m_setdotindex((*d).flags,(i64)3,(u64)0u);
        (*d).flags = msysc$m_setdotindex((*d).flags,(i64)2,(u64)1u);
        if ((((i64)mm_decls$ttbasetype[(m)] == (i64)10) && ((i64)mm_decls$ttlength[(m)] == (i64)0))) {
            (*d).mode = (i64)(*dcode).mode;
        }
;
        if ((mold != m)) {
            if ((!!((i64)mm_decls$ttisinteger[(m)]) && !!((i64)mm_decls$ttisshort[(mold)]))) {
                mm_lib$insertunit((*d).code,(i64)49);
                (*(*d).code).mode = mold;
            }
            else if ((mold == (i64)4)) {
                (*(*d).code).mode = mold;
            }
;
        }
;
        if (((i64)(*d).nameid == (i64)11)) {
            mm_type$checkconstexpr((*d).code);
        }
;
    }
    else if ((((!!(dcode) && ((i64)(*d).nameid == (i64)12)) && ((i64)mm_decls$ttbasetype[(m)] == (i64)10)) && ((i64)mm_decls$ttlength[(m)] == (i64)0))) {
        mm_type$tpass(dcode,m,(i64)0);
        (*d).mode = (i64)(*dcode).mode;
        (*d).flags = msysc$m_setdotindex((*d).flags,(i64)3,(u64)0u);
        (*d).flags = msysc$m_setdotindex((*d).flags,(i64)2,(u64)1u);
    }
    else {
        (*d).flags = msysc$m_setdotindex((*d).flags,(i64)3,(u64)0u);
        (*d).flags = msysc$m_setdotindex((*d).flags,(i64)2,(u64)1u);
    }
;
}

void mm_type$tx_namedconst(struct mm_decls$strec *d) {
        i64 m;
        struct mm_decls$unitrec *  q;
    if (!!(msysc$m_getdotindex((i64)(*d).flags,(i64)3))) {
        mm_support$txerror((byte*)"Circular const reference detected",0);
    }
;
    if (!!(msysc$m_getdotindex((i64)(*d).flags,(i64)2))) {
        return;
    }
;
    q = (*d).code;
    m = (i64)(*d).mode;
    (*d).flags = msysc$m_setdotindex((*d).flags,(i64)3,(u64)1u);
    mm_type$tpass(q,((m == (i64)21) ? (i64)22 : m),(i64)0);
    (*d).flags = msysc$m_setdotindex((*d).flags,(i64)3,(u64)0u);
    mm_type$checkconstexpr(q);
    if ((m == (i64)21)) {
        (*d).mode = (i64)(*q).mode;
    }
;
    (*d).flags = msysc$m_setdotindex((*d).flags,(i64)2,(u64)1u);
}

static void mm_type$checkconstexpr(struct mm_decls$unitrec *p) {
        struct mm_decls$unitrec *  q;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)1) || ($temp==(i64)52)) {
        return;
    }
    else if (($temp==(i64)15)) {
        q = (*p).a;
        L769 :;
        while (!!(q)) {
            mm_type$checkconstexpr(q);
            q = (*q).nextunit;
L770 :;
        }
L771 :;
        ;
    }
    else if (($temp==(i64)48)) {
        if (((i64)mm_decls$ttbasetype[((i64)(*(*p).a).mode)] == (i64)7)) {
            if (((i64)mm_decls$tttarget[((i64)(*(*p).a).mode)] == (i64)0)) {
                (*(*p).a).mode = (i64)(*p).mode;
                mm_lib$deleteunit(p,(*p).a);
            }
            else {
                goto L772 ;
;
            }
;
        }
;
        //cerror:
L772 :;
;
        if ((u64)0u) {
            goto L773 ;
;
        }
;
    }
    else if (($temp==(i64)49)) {
        mm_type$checkconstexpr((*p).a);
    }
    else if (($temp==(i64)46) || ($temp==(i64)47)) {
                {i64 $temp = (i64)(*(*p).a).tag;
if (($temp==(i64)3)) {
        }
        else {
            goto L773 ;
;
        }
        };
    }
    else if (($temp==(i64)3)) {
        if (((i64)(*(*p).def).nameid == (i64)14)) {
            return;
        }
;
        if (((i64)(*(*p).def).nameid == (i64)17)) {
            return;
        }
;
        goto L773 ;
;
    }
    else {
        //error:
L773 :;
;
        msysc$m_print_startcon();
        msysc$m_print_str(mm_tables$jtagnames[((i64)(*p).tag)],NULL);
        msysc$m_print_str(mm_lib$strmode((i64)(*p).mode,(i64)1),NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        mm_support$txerror((byte*)"Getconstexpr: not const",0);
    }
    };
}

static i64 mm_type$getconstint(struct mm_decls$unitrec *q) {
    mm_type$checkconstexpr(q);
    if ((!!((i64)mm_decls$ttisinteger[((i64)(*q).mode)]) || ((i64)(*q).tag == (i64)52))) {
        return (*q).value;
    }
    else if (!!((i64)mm_decls$ttisreal[((i64)(*q).mode)])) {
        return (i64)(*q).xvalue;
    }
    else {
        msysc$m_print_startcon();
        msysc$m_print_str(mm_lib$strmode((i64)(*q).mode,(i64)1),NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        mm_support$txerror((byte*)"Getconstint: not int32/64",0);
    }
;
    return (i64)0;
}

static void mm_type$makenewconst(struct mm_decls$unitrec *p,i64 x,i64 t) {
    (*p).tag = (i64)1;
    (*p).a = ((*p).b = 0);
    (*p).value = x;
    (*p).isconst = (i64)1;
    (*p).simple = (i64)1;
    if ((t != (i64)0)) {
        (*p).mode = t;
    }
;
}

static void mm_type$tx_name(struct mm_decls$unitrec *p,i64 t,i64 lv) {
        struct mm_decls$strec *  d;
        i64 oldmlineno;
        struct mm_decls$unitrec *  pcode;
    oldmlineno = mm_tables$mlineno;
    d = (*p).def;
    mm_tables$mlineno = (i64)(*d).pos;
    switch ((i64)(*d).nameid) {
    case 10:;
    case 16:;
        {
            if (!!(lv)) {
                mm_support$txerror((byte*)"&const",0);
            }
;
            mm_type$tx_namedconst(d);
            pcode = (*d).code;
            (*p).tag = (i64)1;
            (*p).def = 0;
            (*p).a = 0;
            (*p).c = 0;
            if (((i64)(*pcode).tag == (i64)48)) {
                (*p).value = (*(*pcode).a).value;
            }
            else {
                (*p).value = (*pcode).value;
            }
;
            (*p).slength = (i64)(*pcode).slength;
            (*p).mode = (i64)(*d).mode;
            (*p).isconst = (i64)1;
            (*p).isastring = (i64)(*pcode).isastring;
        }
        break;
    case 11:;
    case 12:;
    case 13:;
        {
            if ((!!(msysc$m_getdotindex((i64)(*d).flags,(i64)4)) && !!(lv))) {
                mm_support$txerror_s((byte*)"Can't use 'let' as lvalue: ",(*d).name,0);
            }
;
            mm_type$tx_namedef(d);
            if (!(!!(mm_type$inassem))) {
                (*p).mode = (i64)(*d).mode;
                if (((i64)(*d).parammode == (i64)2)) {
                    mm_lib$insertunit(p,(i64)45);
                    (*p).mode = (i64)mm_decls$tttarget[((i64)(*d).mode)];
                }
;
                mm_type$twiden(p,lv);
            }
            else {
                (*p).mode = (i64)20;
            }
;
        }
        break;
    case 6:;
    case 7:;
        {
            (*p).mode = mm_tables$trefproc;
        }
        break;
    case 17:;
        {
            (*p).mode = mm_tables$treflabel;
        }
        break;
    case 3:;
        {
            mm_support$txerror_s((byte*)"Module name can't be used on it's own: #",(*d).name,0);
        }
        break;
    case 14:;
        {
            (*p).tag = (i64)1;
            (*p).def = 0;
            (*p).a = 0;
            (*p).c = 0;
            (*p).value = (i64)(*d).offset;
            (*p).mode = (i64)3;
            (*p).isconst = (i64)1;
        }
        break;
    case 5:;
        {
            (*p).tag = (i64)52;
            (*p).value = (i64)(*d).mode;
            (*p).mode = (i64)3;
        }
        break;
    case 8:;
        {
            if (!!((*d).code)) {
                mm_support$txerror((byte*)"Can't init dllvar",0);
            }
;
            (*p).mode = (i64)(*d).mode;
        }
        break;
    default: {
        mm_tables$mlineno = (i64)(*p).pos;
        mm_support$txerror_ss((byte*)"TNAME? # #",mm_tables$namenames[((i64)(*d).nameid)],(*d).name);
    }
    } //SW
;
    mm_tables$mlineno = oldmlineno;
}

static void mm_type$tx_bin(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
        i64 amode;
        i64 bmode;
    mm_type$tpass(a,(i64)22,(i64)0);
    mm_type$tpass(b,(i64)22,(i64)0);
    amode = (i64)(*a).mode;
    bmode = (i64)(*b).mode;
    switch ((i64)(*p).pclop) {
    case 1:;
        {
            if (!!(mm_type$dobinnumx(p,a,b))) {
                return;
            }
;
            if (!!((i64)mm_decls$ttisref[(amode)])) {
                if (((!!((i64)mm_decls$ttisref[(bmode)]) && !!((i64)(*a).isastring)) && !!((i64)(*b).isastring))) {
                    mm_type$combinestrings(p);
                    return;
                }
;
                if ((bmode >= (i64)1 && bmode <= (i64)5)) {
                    mm_type$coerceunit(b,(i64)3,(i64)0);
                    (*p).pclop = (i64)26;
                    (*p).mode = amode;
                    return;
                }
;
            }
;
        }
        break;
    case 2:;
        {
            if (!!(mm_type$dobinnumx(p,a,b))) {
                return;
            }
;
            if (!!((i64)mm_decls$ttisref[(amode)])) {
                if (!!((i64)mm_decls$ttisref[(bmode)])) {
                    if (!!(mm_type$comparemodes(amode,bmode))) {
                        (*p).pclop = (i64)28;
                        (*p).mode = (i64)3;
                        return;
                    }
                    else {
                        mm_support$txerror((byte*)"ref-ref: not compat",0);
                    }
;
                }
;
                if ((bmode >= (i64)1 && bmode <= (i64)5)) {
                    mm_type$coerceunit(b,(i64)3,(i64)0);
                    (*p).pclop = (i64)27;
                    (*p).mode = amode;
                    return;
                }
;
            }
;
        }
        break;
    case 17:;
    case 18:;
    case 19:;
    case 20:;
    case 21:;
    case 22:;
        {
            if (!!(mm_type$dobinnumx(p,a,b))) {
                (*p).mode = (i64)6;
                return;
            }
;
            (*p).mode = (i64)6;
            if ((!!((i64)mm_decls$ttisref[(amode)]) && !!((i64)mm_decls$ttisref[(bmode)]))) {
                if (!(!!(mm_type$comparemodes(amode,bmode)))) {
                    mm_support$txerror((byte*)"Cmp ref/ref not compat",0);
                }
;
                return;
            }
;
            if (((i64)(*p).pclop == (i64)17 || (i64)(*p).pclop == (i64)18)) {
                if (!!(mm_type$comparemodes(amode,bmode))) {
                    return;
                }
;
            }
;
        }
        break;
    case 3:;
        {
            if (!!(mm_type$dobinnumx(p,a,b))) {
                return;
            }
;
        }
        break;
    case 4:;
        {
            if (((amode == (i64)3 || amode == (i64)2 || amode == (i64)1) && (bmode == (i64)3 || bmode == (i64)2 || bmode == (i64)1))) {
                (*p).pclop = (i64)5;
                goto L774 ;
;
            }
;
            if (!!(mm_type$dobinnumf(p,a,b))) {
                return;
            }
;
            if (((amode >= (i64)1 && amode <= (i64)5) && (bmode >= (i64)1 && bmode <= (i64)5))) {
                (*p).mode = (i64)5;
                mm_type$coerceunit(a,(i64)5,(i64)0);
                mm_type$coerceunit(b,(i64)5,(i64)0);
                return;
            }
;
        }
        break;
    case 5:;
    case 6:;
    case 7:;
    case 8:;
    case 9:;
    case 10:;
        {
            //doidiv:
L774 :;
;
            if (!!(mm_type$dobinnumi(p,a,b))) {
                return;
            }
;
        }
        break;
    case 15:;
    case 16:;
        {
            if (!!(mm_type$dobinnumx(p,a,b))) {
                return;
            }
;
        }
        break;
    case 51:;
        {
            if (!!(mm_type$dobinnumx(p,a,b))) {
                return;
            }
;
        }
        break;
    case 52:;
    case 50:;
        {
            if (!!(mm_type$dobinnumf(p,a,b))) {
                return;
            }
;
        }
        break;
    case 11:;
    case 12:;
        {
            if ((amode == (i64)3 || amode == (i64)2 || amode == (i64)1)) {
                mm_type$coerceunit(b,(i64)3,(i64)0);
                (*p).mode = amode;
                return;
            }
;
        }
        break;
    case 13:;
    case 14:;
        {
            mm_type$doin(p,a,b);
            return;
        }
        break;
    case 24:;
    case 25:;
        {
            (*p).mode = (i64)6;
            if ((amode==bmode && bmode==(i64)6)) {
                return;
            }
;
        }
        break;
    default: {
        mm_support$txerror((byte*)"txbin?",0);
    }
    } //SW
;
    msysc$m_print_startcon();
    msysc$m_print_str(mm_tables$pclnames[((i64)(*p).pclop)],NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mm_support$txerror_ss((byte*)"BIN/CAN'T RESOLVE MODES",mm_lib$strmode(amode,(i64)1),mm_lib$strmode2(bmode,(i64)1));
}

static void mm_type$tx_binto(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
        i64 abase;
        i64 bbase;
        i64 amode;
        i64 bmode;
    mm_type$tpasslv(a,(i64)22);
    mm_type$tpass(b,(i64)22,(i64)0);
    amode = (i64)(*a).mode;
    bmode = (i64)(*b).mode;
    abase = (i64)mm_decls$ttbasetype[(amode)];
    bbase = (i64)mm_decls$ttbasetype[(bmode)];
    if ((((i64)(*p).pclop == (i64)62) && !!((i64)mm_decls$ttisinteger[(abase)]))) {
        (*p).pclop = (i64)63;
    }
;
    (*p).mode = (i64)0;
        {i64 $temp = (i64)(*p).pclop;
if (($temp==(i64)59)) {
        if (((abase == (i64)7) && (bbase == (i64)7))) {
            mm_support$txerror((byte*)"to:ref+ref",0);
        }
;
        if (((abase == (i64)7) && (bbase <= (i64)5))) {
            mm_type$coerceunit(b,(i64)3,(i64)0);
            (*p).pclop = (i64)74;
            return;
        }
;
    }
    else if (($temp==(i64)60)) {
        if (((abase == (i64)7) && (bbase <= (i64)5))) {
            mm_type$coerceunit(b,(i64)3,(i64)0);
            (*p).pclop = (i64)75;
            return;
        }
;
    }
    else if (($temp==(i64)68) || ($temp==(i64)69)) {
        mm_type$coerceunit(b,(i64)3,(i64)0);
        return;
    }
    };
    if (((abase >= (i64)1 && abase <= (i64)5) && (bbase >= (i64)1 && bbase <= (i64)5))) {
        mm_type$coerceunit(b,abase,(i64)0);
    }
    else if ((!!((i64)mm_decls$ttisshort[(abase)]) && (bbase >= (i64)1 && bbase <= (i64)5))) {
        mm_type$coerceunit(b,abase,(i64)0);
    }
    else {
        if (!(!!(mm_type$comparemodes(amode,bmode)))) {
            mm_support$txerror_ss((byte*)"BIN: modes not compatible: # #",mm_lib$strmode(amode,(i64)1),mm_lib$strmode(bmode,(i64)1));
        }
;
    }
;
}

static i64 mm_type$getdominantmode(i64 amode,i64 bmode) {
        i64 abase;
        i64 bbase;
    abase = (i64)mm_decls$ttbasetype[(amode)];
    bbase = (i64)mm_decls$ttbasetype[(bmode)];
    if (((abase >= (i64)1 && abase <= (i64)5) && (bbase >= (i64)1 && bbase <= (i64)5))) {
        return (abase>bbase?abase:bbase);
    }
;
    if (!(!!(mm_type$comparemodes(amode,bmode)))) {
        mm_support$txerror((byte*)"Getdom: no dominant mode",0);
    }
;
    return amode;
}

static void mm_type$tx_cmpchain(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a) {
        i64 u;
        struct mm_decls$unitrec *  q;
        struct mm_decls$unitrec *  r;
    q = a;
    L775 :;
    while (!!(q)) {
        mm_type$tpass(q,(i64)22,(i64)0);
        if ((q == a)) {
            u = (i64)(*q).mode;
        }
        else {
            u = mm_type$getdominantmode(u,(i64)(*q).mode);
        }
;
        q = (*q).nextunit;
L776 :;
    }
L777 :;
    ;
    q = a;
    r = (*a).nextunit;
    L778 :;
    while (!!(q)) {
        mm_type$coerceunit(q,u,(i64)0);
        q = (*q).nextunit;
L779 :;
    }
L780 :;
    ;
    (*p).mode = (i64)6;
}

static void mm_type$tx_callproc(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *pargs,i64 t) {
        struct mm_decls$unitrec *  q;
        struct mm_decls$strec *  d;
        struct mm_decls$strec *  e;
        struct mm_decls$strec *  pm;
        struct mm_decls$strec *  paramlist[100];
        struct mm_decls$unitrec *  arglist[100];
        struct mm_decls$unitrec *  newarglist[100];
        i64 nparams;
        i64 i;
        i64 j;
        i64 k;
        i64 nargs;
        i64 m;
        i64 kwdused;
        i64 qm;
        i64 ismproc;
        u8 *  name;
        struct mm_decls$unitrec *  ulist;
        struct mm_decls$unitrec *  ulistx;
    mm_type$tpass(a,(i64)22,(i64)0);
    nargs = (nparams = (i64)0);
    ismproc = (i64)0;
    //retry:
L781 :;
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)3)) {
        d = (*a).def;
        if (((i64)(*d).nameid == (i64)6 || (i64)(*d).nameid == (i64)7)) {
            ismproc = (i64)((i64)(*d).nameid == (i64)6);
            //getparams:
L782 :;
;
            e = (struct mm_decls$strec *)(*d).deflist;
            L783 :;
            while (!!(e)) {
                if (((i64)(*e).nameid == (i64)13)) {
                    if ((nparams >= (i64)100)) {
                        mm_support$txerror((byte*)"Param overflow",0);
                    }
;
                    paramlist[(++(nparams))-1] = e;
                }
;
                e = (struct mm_decls$strec *)(*e).nextdef;
L784 :;
            }
L785 :;
            ;
        }
        else {
            L786 :;
            while (((i64)mm_decls$ttbasetype[((i64)(*a).mode)] == (i64)7)) {
                mm_lib$insertunit(a,(i64)45);
                (*a).mode = (i64)mm_decls$tttarget[((i64)(*a).mode)];
L787 :;
            }
L788 :;
            ;
            goto L789 ;
;
        }
;
    }
    else if (($temp==(i64)90) || ($temp==(i64)108)) {
        mm_support$txerror((byte*)"Can't do ifx/function",0);
    }
    else {
        //dorefproc:
L789 :;
;
        if (((i64)(*a).tag == (i64)42)) {
            mm_type$tmethodcall(p,a,pargs);
            a = (*p).a;
            pargs = (*p).b;
            goto L781 ;
;
        }
;
        if (((i64)mm_decls$ttbasetype[((i64)(*a).mode)] != (i64)23)) {
            mm_support$txerror((byte*)"Function pointer expected",0);
        }
;
        d = mm_decls$ttnamedef[((i64)(*a).mode)];
        if ((d == 0)) {
            mm_support$txerror((byte*)"Function expected",0);
        }
;
        goto L782 ;
;
    }
    };
    q = pargs;
    L790 :;
    while (!!(q)) {
        if ((nargs >= (i64)100)) {
            mm_support$txerror((byte*)"Param overflow",0);
        }
;
        arglist[(++(nargs))-1] = q;
        q = (*q).nextunit;
L791 :;
    }
L792 :;
    ;
    (*p).mode = (i64)(*d).mode;
    if ((((i64)(*p).mode == (i64)0) && ((i64)(*p).tag == (i64)28))) {
        (*p).tag = (i64)86;
    }
;
    if ((!!((i64)(*p).mode) && (t != (i64)0))) {
        mm_type$twiden(p,(i64)0);
    }
;
    if (!!((i64)(*d).varparams)) {
        for (i=(i64)1;i<=nargs;++i) {
L793 :;
            if ((i <= nparams)) {
                mm_type$tpass(arglist[(i)-1],(i64)(*paramlist[(i)-1]).mode,(i64)0);
            }
            else {
                mm_type$tpass(arglist[(i)-1],(i64)22,(i64)0);
            }
;
L794 :;
        }
L795 :;
        ;
        if ((t == (i64)0)) {
            (*p).tag = (i64)86;
        }
;
        return;
    }
;
    k = (i64)0;
    kwdused = (i64)0;
    for (i=(i64)1;i<=nparams;++i) {
L796 :;
        newarglist[(i)-1] = 0;
L797 :;
    }
L798 :;
    ;
    for (i=(i64)1;i<=nargs;++i) {
L799 :;
        q = arglist[(i)-1];
        switch ((i64)(*q).tag) {
        case 21:;
            {
                name = (*(*(*q).a).def).name;
                for (j=(i64)1;j<=nparams;++j) {
L802 :;
                    if (!!(mlib$eqstring((*paramlist[(j)-1]).name,name))) {
                        goto L804 ;
                    }
;
L803 :;
                }
                {
                    mm_support$txerror_s((byte*)"Can't find kwd param: #",name,0);
                }
L804 :;
                ;
                if (!!(newarglist[(j)-1])) {
                    mm_support$txerror_s((byte*)"Kwd: # already used or was implicit",name,0);
                }
;
                newarglist[(j)-1] = (*q).b;
                kwdused = (i64)1;
            }
            break;
        case 2:;
            {
                if (!!(kwdused)) {
                    mm_support$txerror((byte*)"Normal param follows kwd",0);
                }
;
                q = 0;
                goto L805 ;
;
            }
            break;
        default: {
            //doregparam:
L805 :;
;
            if (!!(kwdused)) {
                mm_support$txerror((byte*)"Normal param follows kwd",0);
            }
;
            if ((k >= nparams)) {
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"K=",NULL);
                msysc$m_print_i64(k,NULL);
                msysc$m_print_str((byte*)"NPARAMS=",NULL);
                msysc$m_print_i64(nparams,NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                mm_support$txerror((byte*)"Too many params supplied",0);
            }
;
            newarglist[(++(k))-1] = q;
        }
        } //SW
;
L800 :;
    }
L801 :;
    ;
    for (i=(i64)1;i<=nparams;++i) {
L806 :;
        q = newarglist[(i)-1];
        pm = paramlist[(i)-1];
        if ((q == 0)) {
            if (!(!!((i64)(*pm).optional))) {
                mm_support$txerror_s((byte*)"Param not optional: #",msysc$strint(i,0),0);
            }
;
            if (!!((*pm).code)) {
                newarglist[(i)-1] = mm_lib$duplunit((*pm).code,(i64)(*p).pos);
            }
            else {
                newarglist[(i)-1] = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)0u,(i64)3);
            }
;
        }
;
L807 :;
    }
L808 :;
    ;
    ulist = 0;
    ulistx = 0;
    for (i=(i64)1;i<=nparams;++i) {
L809 :;
        pm = paramlist[(i)-1];
        q = newarglist[(i)-1];
        if (((i64)(*pm).parammode == (i64)2)) {
            mm_type$tpass(q,(m = (i64)mm_decls$tttarget[((i64)(*pm).mode)]),(i64)1);
            qm = (i64)(*q).mode;
            if (!(!!(mm_type$comparemodes(qm,m)))) {
                mm_support$txerror_ss((byte*)"&param: type mismatch",mm_lib$strmode(qm,(i64)1),mm_lib$strmode(m,(i64)1));
            }
;
            if (!(((u64)1u && ((i64)(*q).tag == (i64)48)))) {
                mm_lib$insertunit(q,(i64)46);
                (*q).mode = (i64)(*pm).mode;
            }
            else {
                (*q).tag = (i64)46;
                (*(*q).a).mode = (i64)(*pm).mode;
            }
;
            mm_type$setsimple(q);
        }
        else {
            mm_type$tpass(q,(i64)(*pm).mode,(i64)0);
        }
;
        mm_lib$addlistunit(&ulist,&ulistx,q);
        (*q).nextunit = 0;
L810 :;
    }
L811 :;
    ;
    (*p).b = ulist;
    if ((t == (i64)0)) {
        (*p).tag = (i64)86;
    }
;
}

static void mm_type$tx_unary(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a) {
        i64 size;
        i64 amode;
        i64 mbase;
        i64 x;
        i64 resmode;
    mm_type$tpass(a,(i64)22,(i64)0);
    amode = (i64)(*a).mode;
    resmode = amode;
    switch ((i64)(*p).pclop) {
    case 89:;
    case 90:;
    case 88:;
    case 91:;
        {
            mm_type$do_bounds(p,a);
            return;
        }
        break;
    case 94:;
    case 93:;
        {
            size = ((i64)mm_decls$ttsize[((((i64)(*a).tag == (i64)52) ? (*a).value : amode))] * (((i64)(*p).pclop == (i64)94) ? (i64)1 : (i64)8));
            mm_type$makenewconst(p,size,(i64)0);
            resmode = (i64)3;
        }
        break;
    case 95:;
    case 96:;
        {
            resmode = (i64)3;
            if (((i64)(*a).tag == (i64)52)) {
                mbase = (i64)mm_decls$ttbasetype[((*a).value)];
            }
            else {
                mbase = (i64)mm_decls$ttbasetype[(mm_lib$getmemmode(a))];
            }
;
            if (((i64)(*p).pclop == (i64)95)) {
                if ((mbase==(i64)14)) {
                    x = (i64)-128;
                }
                else if ((mbase==(i64)15)) {
                    x = (i64)-32768;
                }
                else if ((mbase==(i64)16)) {
                    x = (i64)-2147483648;
                }
                else if ((mbase==(i64)3)) {
                    x = (i64)(-9223372036854775807-1);
                }
                else if ((mbase==(i64)17) || (mbase==(i64)18) || (mbase==(i64)19) || (mbase==(i64)2) || (mbase==(i64)12) || (mbase==(i64)1)) {
                    x = (i64)0;
                }
                else {
                    mm_support$txerror_s((byte*)"Can't do minvalue on #",mm_lib$strmode(mbase,(i64)1),0);
                }
;
            }
            else {
                if ((mbase==(i64)14)) {
                    x = (i64)127;
                }
                else if ((mbase==(i64)15)) {
                    x = (i64)32767;
                }
                else if ((mbase==(i64)16)) {
                    x = (i64)2147483647;
                }
                else if ((mbase==(i64)3)) {
                    x = (i64)9223372036854775807;
                }
                else if ((mbase==(i64)17) || (mbase==(i64)12)) {
                    x = (i64)255;
                }
                else if ((mbase==(i64)18)) {
                    x = (i64)65535;
                }
                else if ((mbase==(i64)19)) {
                    x = (i64)4294967295;
                }
                else if ((mbase==(i64)2)) {
                    x = (i64)0;
                    --(x);
                    resmode = (i64)2;
                }
                else {
                    mm_support$txerror_s((byte*)"Can't do maxvalue on #",mm_lib$strmode(mbase,(i64)1),0);
                }
;
            }
;
            (*p).tag = (i64)1;
            (*p).a = 0;
            (*p).value = x;
            (*p).isconst = (i64)1;
            (*p).simple = (i64)1;
        }
        break;
    case 41:;
    case 42:;
    case 44:;
    case 36:;
    case 37:;
    case 38:;
    case 39:;
    case 40:;
        {
            mm_type$coerceunit(a,(i64)5,(i64)0);
            resmode = (i64)5;
        }
        break;
    case 35:;
        {
            if (!((amode == (i64)4 || amode == (i64)5))) {
                mm_type$coerceunit(a,(i64)5,(i64)0);
                resmode = (i64)5;
            }
;
        }
        break;
    case 97:;
        {
            (*p).tag = (i64)1;
            if (((i64)(*a).tag == (i64)52)) {
                amode = (*a).value;
            }
            else {
                amode = mm_lib$getmemmode(a);
            }
;
            (*p).mode = (i64)20;
            (*p).svalue = mlib$pcm_copyheapstring(mm_lib$strmode(amode,(i64)1));
            (*p).isastring = (i64)1;
            (*p).length = strlen((*p).svalue);
            return;
        }
        break;
    case 104:;
        {
            mm_type$tx_sliceptr(p,a);
            return;
        }
        break;
    case 31:;
        {
            if (!(!!((i64)mm_decls$ttisinteger[(amode)]))) {
                mm_support$txerror((byte*)"Inot",0);
            }
;
        }
        break;
    case 29:;
    case 30:;
    case 34:;
    case 43:;
    case 46:;
    case 47:;
        {
            if ((!(!!((i64)mm_decls$ttisinteger[(amode)])) && !(!!((i64)mm_decls$ttisreal[(amode)])))) {
                mm_support$txerror((byte*)"Neg/Abs?",0);
            }
;
        }
        break;
    default: {
        mm_support$txerror((byte*)"TX:UNARY NOT CHECKED",0);
    }
    } //SW
;
    (*p).mode = resmode;
}

static void mm_type$tx_if(struct mm_decls$unitrec *p,struct mm_decls$unitrec *pcond,struct mm_decls$unitrec *plist,struct mm_decls$unitrec *pelse,i64 t,i64 lv) {
        struct mm_decls$unitrec *  pc;
        struct mm_decls$unitrec *  pl;
        i64 u;
    pc = pcond;
    pl = plist;
    u = (i64)0;
    if ((t != (i64)22)) {
        u = t;
    }
;
    L812 :;
    while (!!(pc)) {
        mm_type$tpass(pc,(i64)22,(i64)0);
        mm_type$tpass(pl,t,lv);
        if ((t == (i64)22)) {
            if ((u == (i64)0)) {
                u = (i64)(*pl).mode;
            }
            else {
                u = mm_type$getdominantmode(u,(i64)(*pl).mode);
            }
;
        }
;
L813 :;
        {
            pc = (*pc).nextunit;
            pl = (*pl).nextunit;
        }
L815 :;
            }
L814 :;
    ;
    if (((t != (i64)0) && (pelse == 0))) {
        mm_support$txerror((byte*)"else needed",0);
    }
;
    mm_type$tpass(pelse,t,lv);
    if ((t == (i64)22)) {
        u = mm_type$getdominantmode(u,(i64)(*pelse).mode);
    }
;
    if ((t != (i64)0)) {
        pl = plist;
        L816 :;
        while (!!(pl)) {
            if ((t == (i64)22)) {
                mm_type$coerceunit(pl,u,(i64)0);
            }
;
L817 :;
            pl = (*pl).nextunit;
L819 :;
                    }
L818 :;
        ;
        if ((t == (i64)22)) {
            mm_type$coerceunit(pelse,u,(i64)0);
        }
;
        (*p).mode = u;
    }
;
    if (((*pcond).nextunit==(*plist).nextunit && (*plist).nextunit==0)) {
        if (!!(mm_type$iscondtrue(pcond))) {
            if (((u64)0u || ((i64)(*plist).tag == (i64)1))) {
                mm_lib$deleteunit(p,plist);
            }
;
        }
        else if (!!(mm_type$iscondfalse(pcond))) {
            if ((pelse == 0)) {
                pelse = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)4);
            }
;
            if (((u64)0u || ((i64)(*pelse).tag == (i64)1))) {
                mm_lib$deleteunit(p,pelse);
            }
;
        }
;
    }
;
    mm_type$setsimple(p);
}

static void mm_type$tx_incrto(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 t) {
    mm_type$tpasslv(a,(i64)22);
    if (!((!!((i64)mm_decls$ttisref[((i64)(*a).mode)]) || !!((i64)mm_decls$ttisinteger[((i64)(*a).mode)])))) {
        mm_support$txerror((byte*)"incr not int/ref",0);
    }
;
    if ((t != (i64)0)) {
                {i64 $temp = (i64)(*p).pclop;
if (($temp==(i64)53)) {
            (*p).pclop = (i64)55;
        }
        else if (($temp==(i64)54)) {
            (*p).pclop = (i64)56;
        }
        };
        (*p).mode = mm_lib$gettypebase((i64)(*a).mode);
    }
    else {
                {i64 $temp = (i64)(*p).pclop;
if (($temp==(i64)57)) {
            (*p).pclop = (i64)53;
        }
        else if (($temp==(i64)58)) {
            (*p).pclop = (i64)54;
        }
        };
        (*p).mode = (i64)0;
    }
;
    mm_type$twiden(p,(i64)0);
}

static void mm_type$tx_for(struct mm_decls$unitrec *pindex,struct mm_decls$unitrec *pfrom,struct mm_decls$unitrec *pbody) {
        struct mm_decls$unitrec *  pto;
        struct mm_decls$unitrec *  pstep;
        struct mm_decls$unitrec *  passign;
        i64 u;
    pto = (*pfrom).nextunit;
    pstep = (*pto).nextunit;
    passign = (*pindex).nextunit;
    mm_type$tpass(pindex,(i64)22,(i64)0);
    if (((i64)(*pindex).tag != (i64)3)) {
        mm_support$txerror((byte*)"Loop index not a variable",0);
    }
;
    u = (i64)(*pindex).mode;
    if (!!(passign)) {
        mm_type$tpass(passign,(i64)22,(i64)0);
        if (((i64)(*(*passign).b).tag == (i64)1 || (i64)(*(*passign).b).tag == (i64)3)) {
            (*pindex).nextunit = 0;
            pto = ((*pfrom).nextunit = (*passign).b);
            (*pto).nextunit = pstep;
            (*(*(*passign).a).def).flags = msysc$m_setdotindex((*(*(*passign).a).def).flags,(i64)1,(u64)0u);
        }
;
    }
;
    mm_type$tpass(pfrom,u,(i64)0);
    mm_type$tpass(pto,u,(i64)0);
    mm_type$tpass(pstep,u,(i64)0);
    mm_type$tpass(pbody,(i64)0,(i64)0);
    mm_type$tpass((*pbody).nextunit,(i64)0,(i64)0);
}

static void mm_type$tx_forall(struct mm_decls$unitrec *pindex,struct mm_decls$unitrec *plist,struct mm_decls$unitrec *pbody) {
        struct mm_decls$unitrec *  plocal;
        struct mm_decls$unitrec *  pfrom;
        struct mm_decls$unitrec *  pto;
        struct mm_decls$unitrec *  passign;
        i64 mlist;
        i64 elemtype;
    plocal = (*pindex).nextunit;
    pfrom = (*plocal).nextunit;
    pto = (*pfrom).nextunit;
    passign = (*plist).nextunit;
    mm_type$tpass(pindex,(i64)3,(i64)0);
    mm_type$tpass(pfrom,(i64)3,(i64)0);
    mm_type$tpass(pto,(i64)3,(i64)0);
    mm_type$tpass(plist,(i64)22,(i64)0);
    mlist = (i64)(*plist).mode;
        {i64 $temp = (i64)mm_decls$ttbasetype[(mlist)];
if (($temp==(i64)10)) {
        elemtype = (i64)mm_decls$tttarget[(mlist)];
    }
    else if (($temp==(i64)11)) {
        elemtype = (i64)mm_decls$tttarget[(mlist)];
    }
    else {
        mm_support$txerror((byte*)"forall/can't iterate",0);
    }
    };
    mm_type$tpass(plocal,(i64)22,(i64)0);
    if (((i64)(*plocal).mode == (i64)22)) {
        (*plocal).mode = elemtype;
        (*(*plocal).def).mode = elemtype;
    }
;
    mm_type$tpass(passign,(i64)22,(i64)0);
    mm_type$tpass(pbody,(i64)0,(i64)0);
    mm_type$tpass((*pbody).nextunit,(i64)0,(i64)0);
}

static void mm_type$tx_index(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,i64 t,i64 lv) {
        i64 amode;
    mm_type$tpass(a,(i64)22,lv);
    mm_type$deref(a,(i64)(t != (i64)0));
    amode = (i64)(*a).mode;
    mm_type$tpass(b,(i64)3,(i64)0);
    if (!(((i64)mm_decls$ttbasetype[(amode)] == (i64)10 || (i64)mm_decls$ttbasetype[(amode)] == (i64)11))) {
        mm_support$txerror_s((byte*)"Can't index: #",mm_lib$strmode(amode,(i64)1),0);
    }
;
    (*p).mode = (i64)mm_decls$tttarget[(amode)];
    mm_type$twiden(p,lv);
    mm_type$setsimple(p);
}

static void mm_type$tx_makerange(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
        i64 amode;
        i64 bmode;
    mm_type$tpass(a,(i64)3,(i64)0);
    mm_type$tpass(b,(i64)3,(i64)0);
    amode = (i64)(*a).mode;
    bmode = (i64)(*b).mode;
    mm_type$coerceunit(a,(i64)3,(i64)0);
    mm_type$coerceunit(b,(i64)3,(i64)0);
    mm_type$setsimple(p);
    (*p).mode = (i64)9;
}

static void mm_type$tx_ptr(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 t,i64 lv) {
    mm_type$tpass(a,(i64)22,(i64)0);
        {i64 $temp = (i64)mm_decls$ttbasetype[((i64)(*a).mode)];
if (($temp==(i64)0)) {
        mm_support$txerror((byte*)"Deref Void",0);
    }
    else if (($temp==(i64)7)) {
        (*p).mode = (i64)mm_decls$tttarget[((i64)(*a).mode)];
    }
    else if (($temp==(i64)11)) {
        mm_support$txerror((byte*)"Can't deref slice",0);
    }
    else {
        mm_support$txerror((byte*)"PTR: need ref T",0);
    }
    };
    mm_type$setsimple(p);
    mm_type$twiden(p,lv);
}

static void mm_type$setrecordsize(i64 m) {
        struct mm_decls$strec *  fieldlist[208];
        i64 nfields;
        i64 size;
        i64 index;
        i64 maxalign;
        struct mm_decls$strec *  d;
        struct mm_decls$strec *  e;
        u8 *  flags;
        i64 flag;
    if (!!((i64)mm_decls$ttsize[(m)])) {
        return;
    }
;
    d = mm_decls$ttnamedef[(m)];
    e = (struct mm_decls$strec *)(*d).deflist;
    nfields = (i64)0;
    fieldlist[(++(nfields))-1] = (struct mm_decls$strec *)'S';
    L820 :;
    while (!!(e)) {
        if (((i64)(*e).nameid == (i64)14)) {
            if ((nfields >= (i64)200)) {
                mm_support$gerror((byte*)"srs:too many fields",0);
            }
;
            mm_type$setmodesize((i64)(*e).mode);
            flags = (u8 *)&(*e).uflags;
            L823 :;
                        {u64 $temp = (u64)(*flags);
if (($temp=='S') || ($temp=='U')) {
                flag = (i64)(u64)(*flags);
                fieldlist[(++(nfields))-1] = *(struct mm_decls$strec **)&flag;
                ++(flags);
            }
            else {
                goto L824 ;
            }
            }goto L823 ;
L824 :;
            ;
            fieldlist[(++(nfields))-1] = e;
            L825 :;
            while (1) {
                flag = (i64)(u64)(*(flags)++);
                if ((flag==(i64)42)) {
                }
                else if ((flag==(i64)69)) {
                    fieldlist[(++(nfields))-1] = (struct mm_decls$strec *)'E';
                }
                else {
                    goto L826 ;
                }
;
            }
L826 :;
            ;
        }
;
        e = (struct mm_decls$strec *)(*e).nextdef;
L821 :;
    }
L822 :;
    ;
    fieldlist[(++(nfields))-1] = (struct mm_decls$strec *)'E';
    fieldlist[((nfields + (i64)1))-1] = 0;
    mm_type$countedfields = (i64)0;
    index = (i64)2;
    maxalign = (i64)1;
    mm_type$scanrecord((i64)83,(struct mm_decls$strec *(*)[])&fieldlist,&index,&size,(i64)0,(i64)(*d).align,&maxalign);
    if (!!((i64)(*d).align)) {
        size = mm_type$roundoffset(size,maxalign);
        (*d).maxalign = maxalign;
    }
    else {
        (*d).maxalign = (i64)1;
    }
;
    mm_decls$ttsize[(m)] = size;
    mm_decls$ttlength[(m)] = mm_type$countedfields;
    mm_decls$ttlower[(m)] = (i64)1;
    mm_type$checkblocktype(m);
}

static void mm_type$checkblocktype(i64 m) {
        {i64 $temp = (i64)mm_decls$ttsize[(m)];
if (($temp==(i64)1) || ($temp==(i64)2) || ($temp==(i64)4)) {
        mm_decls$ttisblock[(m)] = (i64)0;
        mm_decls$ttcat[(m)] = (i64)4;
    }
    else if (($temp==(i64)8)) {
        mm_decls$ttisblock[(m)] = (i64)0;
        mm_decls$ttcat[(m)] = (i64)1;
    }
    };
}

static void mm_type$scanrecord(i64 state,struct mm_decls$strec *(*fields)[],i64 *index,i64 *isize,i64 offset,i64 calign,i64 *maxalign) {
        struct mm_decls$strec *  e;
        struct mm_decls$strec *  f;
        struct mm_decls$strec *  ea;
        i64 size;
        i64 fieldsize;
        i64 bitoffset;
        i64 alignment;
        i64 newoffset;
    size = (i64)0;
    bitoffset = (i64)0;
    L827 :;
    while (!!((f = (*fields)[(((*index))++)-1]))) {
                {i64 $temp = (i64)f;
if (($temp==(i64)83) || ($temp==(i64)85)) {
            mm_type$scanrecord((i64)f,fields,index,&fieldsize,offset,calign,maxalign);
        }
        else if (($temp==(i64)69)) {
            if ((state == (i64)85)) {
                ++(mm_type$countedfields);
            }
;
            (*isize) = size;
            return;
        }
        else {
            if (((i64)(*f).mode == (i64)26)) {
                fieldsize = (i64)0;
                ea = (struct mm_decls$strec *)(*f).equivfield;
                (*f).offset = (i64)(*ea).offset;
                (*f).bitoffset = bitoffset;
                bitoffset += (i64)(*f).bitfieldwidth;
                if ((bitoffset > ((i64)mm_decls$ttsize[((i64)(*(*f).equivfield).mode)] * (i64)8))) {
                    mm_support$txerror((byte*)"Bit fields overflow type",0);
                }
;
            }
            else if (!!(msysc$m_getdotindex((i64)(*f).flags,(i64)9))) {
                if ((u64)1u) {
                    mm_support$gerror((byte*)"Field@",0);
                }
;
                bitoffset = (i64)0;
                e = (struct mm_decls$strec *)(*f).equivfield;
                fieldsize = (i64)0;
                (*f).offset = ((i64)(*e).offset + (i64)(*f).equivoffset);
            }
            else {
                bitoffset = (i64)0;
                if ((state == (i64)83)) {
                    ++(mm_type$countedfields);
                }
;
                fieldsize = (i64)mm_decls$ttsize[((i64)(*f).mode)];
                if (!!(calign)) {
                    alignment = mm_lib$getalignment((i64)(*f).mode);
                    if ((alignment > (*maxalign))) {
                        (*maxalign) = alignment;
                    }
;
                    newoffset = mm_type$roundoffset(offset,alignment);
                    size += (newoffset - offset);
                }
                else {
                    newoffset = offset;
                }
;
                (*f).offset = newoffset;
                offset = newoffset;
            }
;
        }
        };
        if ((state == (i64)83)) {
            offset += fieldsize;
            size += fieldsize;
        }
        else {
            size = (size>fieldsize?size:fieldsize);
        }
;
L828 :;
    }
L829 :;
    ;
}

static i64 mm_type$roundoffset(i64 offset,i64 alignment) {
        i64 mask;
    if ((alignment == (i64)1)) {
        return offset;
    }
;
    mask = (alignment - (i64)1);
    L830 :;
    while (!!((offset & mask))) {
        ++(offset);
L831 :;
    }
L832 :;
    ;
    return offset;
}

static void mm_type$tx_convert(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 hard) {
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)15)) {
        mm_type$tx_makelist(a,(*a).a,(i64)(*p).convmode,(i64)0);
    }
    else {
        mm_type$tpass(a,(i64)22,(i64)0);
        mm_type$coerceunit(a,(i64)(*p).convmode,hard);
    }
    };
    mm_lib$deleteunit(p,a);
}

static void mm_type$tx_makelist(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 t,i64 lv) {
        i64 alength;
        i64 tlength;
        i64 elemtype;
        i64 newt;
        i64 isconst;
        struct mm_decls$unitrec *  q;
        struct mm_decls$strec *  e;
    alength = (i64)(*p).length;
    newt = (i64)0;
    isconst = (i64)1;
    tlength = (i64)mm_decls$ttlength[(t)];
    if (!!(tlength)) {
        if ((alength < tlength)) {
            mm_support$txerror_ss((byte*)"Too few elements",msysc$strint(alength,0),msysc$strint(tlength,0));
        }
        else if ((alength > tlength)) {
            mm_support$txerror_ss((byte*)"Too many elements",msysc$strint(alength,0),msysc$strint(tlength,0));
        }
;
    }
;
        {i64 $temp = (i64)mm_decls$ttbasetype[(t)];
if (($temp==(i64)10)) {
        elemtype = (i64)mm_decls$tttarget[(t)];
        if ((tlength == (i64)0)) {
            newt = mm_lib$createarraymodek(0,elemtype,(i64)mm_decls$ttlower[(t)],alength,(i64)0);
        }
        else {
            newt = t;
        }
;
        q = a;
        L833 :;
        while (!!(q)) {
            mm_type$tpass(q,elemtype,lv);
            if (!(((i64)(*q).tag == (i64)1))) {
                isconst = (i64)0;
            }
;
            q = (*q).nextunit;
L834 :;
        }
L835 :;
        ;
        (*p).mode = newt;
    }
    else if (($temp==(i64)8)) {
        e = (struct mm_decls$strec *)(*mm_decls$ttnamedef[(t)]).deflist;
        q = a;
        L836 :;
        while ((!!(q) && !!(e))) {
            if (((i64)(*e).nameid == (i64)14)) {
                L839 :;
                while (((i64)(*e).mode == (i64)26)) {
                    e = (struct mm_decls$strec *)(*e).nextdef;
                    if (!(!!(e))) {
                        goto L841 ;
                    }
;
L840 :;
                }
L841 :;
                ;
                mm_type$tpass(q,(i64)(*e).mode,lv);
                if (!(((i64)(*q).tag == (i64)1))) {
                    isconst = (i64)0;
                }
;
                q = (*q).nextunit;
            }
;
            e = (struct mm_decls$strec *)(*e).nextdef;
L837 :;
        }
L838 :;
        ;
        L842 :;
        while ((!!(e) && (((i64)(*e).nameid != (i64)14) || ((i64)(*e).mode == (i64)26)))) {
            e = (struct mm_decls$strec *)(*e).nextdef;
L843 :;
        }
L844 :;
        ;
        if ((!!(q) || !!(e))) {
            mm_support$txerror((byte*)"Can't initialise unions",0);
        }
;
        (*p).mode = t;
    }
    else {
        mm_support$txerror_s((byte*)"Unknown makelist type: #",mm_lib$strmode(t,(i64)1),0);
    }
    };
    (*p).isconst = isconst;
    mm_type$tpass((*p).b,(i64)3,(i64)0);
    if ((!(!!(mm_type$inidata)) && !!(isconst))) {
        e = mm_lib$getavname(mm_decls$currproc,(i64)11);
        (*e).mode = t;
        mm_lib$addstatic(e);
        q = (struct mm_decls$unitrec *)mm_lib$createunit0((i64)0);
        (*q) = (*p);
        (*e).code = q;
        (*p).tag = (i64)3;
        (*p).def = e;
    }
;
}

static void mm_type$tx_makeslice(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 t) {
    if (((i64)(*p).length != (i64)2)) {
        mm_support$txerror((byte*)"slice:=[a,b]",0);
    }
;
    (*p).b = (*a).nextunit;
    (*a).nextunit = 0;
    mm_type$tpass(a,(i64)22,(i64)0);
    if (((i64)mm_decls$ttbasetype[((i64)(*a).mode)] != (i64)7)) {
        mm_support$txerror((byte*)"slice init not ref",0);
    }
;
    if (((i64)mm_decls$tttarget[((i64)(*a).mode)] != (i64)0)) {
        if (!(!!(mm_type$comparemodes((i64)(*a).mode,mm_lib$createrefmode(0,(i64)mm_decls$tttarget[(t)],(i64)0))))) {
            mm_support$txerror((byte*)"slice/ptr mismatch",0);
        }
;
    }
;
    mm_type$tpass((*p).b,(i64)3,(i64)0);
    (*p).mode = t;
    (*p).tag = (i64)19;
    (*p).resultflag = (i64)1;
    mm_type$tpass((*p).b,(i64)3,(i64)0);
}

static void mm_type$tx_makeset(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 t) {
    (*p).isconst = (i64)1;
    if (((i64)mm_decls$ttbasetype[(t)] == (i64)11)) {
        mm_type$tx_makeslice(p,a,t);
        return;
    }
;
    L845 :;
    while (!!(a)) {
        mm_type$tpass(a,(i64)22,(i64)0);
        if (!(!!((i64)(*a).isconst))) {
            (*p).isconst = (i64)0;
        }
;
L846 :;
        a = (*a).nextunit;
L848 :;
            }
L847 :;
    ;
    (*p).mode = (i64)0;
}

static void mm_type$tx_dot(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,i64 lv) {
        i64 recmode;
        i64 recbasemode;
        i64 i;
        i64 j;
        i64 newtag;
        i64 tmode;
        struct mm_decls$unitrec *  pindex;
        struct mm_decls$strec *  d;
        struct mm_decls$strec *  dequiv;
    mm_type$tpass(a,(i64)22,(i64)0);
    mm_type$setsimple(a);
    recmode = (i64)(*a).mode;
    recbasemode = (i64)mm_decls$ttbasetype[(recmode)];
    L849 :;
    while ((recbasemode == (i64)7)) {
        tmode = (i64)mm_decls$tttarget[(recmode)];
        mm_lib$insertunit(a,(i64)45);
        mm_type$setsimple(a);
        recmode = ((*a).mode = tmode);
        recbasemode = (i64)mm_decls$ttbasetype[(recmode)];
L850 :;
    }
L851 :;
    ;
    if (((i64)mm_decls$ttbasetype[(recmode)] != (i64)8)) {
        mm_support$txerror((byte*)"Bad record type",0);
    }
;
    d = (*b).def;
    if (((i64)(*d).nameid == (i64)0)) {
        d = ((*b).def = mm_type$resolvefield(d,recmode));
    }
;
    if (((i64)(*d).mode == (i64)26)) {
        i = (i64)(*d).bitoffset;
        j = ((i + (i64)(*d).bitfieldwidth) - (i64)1);
        dequiv = (struct mm_decls$strec *)(*d).equivfield;
        (*b).def = dequiv;
        (*b).mode = (i64)(*dequiv).mode;
        (*p).offset = (i64)(*d).offset;
        if ((i == j)) {
            pindex = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)i,(i64)3);
            newtag = (i64)43;
        }
        else {
            pindex = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)16,(struct mm_decls$unitrec *)mm_lib$createconstunit((u64)i,(i64)3),(struct mm_decls$unitrec *)mm_lib$createconstunit((u64)j,(i64)3));
            (*pindex).mode = (i64)9;
            (*(*pindex).a).resultflag = (i64)1;
            (*(*pindex).b).resultflag = (i64)1;
            (*pindex).simple = (i64)1;
            newtag = (i64)44;
        }
;
        (*p).mode = (i64)(*b).mode;
        mm_type$twiden(p,lv);
        mm_type$setsimple(p);
        mm_lib$insertunit(p,newtag);
        (*p).mode = (i64)2;
        (*p).b = pindex;
        (*(*p).a).resultflag = (i64)1;
        (*(*p).b).resultflag = (i64)1;
        (*p).resultflag = (i64)1;
        mm_type$setsimple(p);
        return;
    }
;
    (*b).mode = (i64)(*d).mode;
    (*p).mode = (i64)(*d).mode;
    mm_type$setsimple(p);
    (*p).offset = (i64)(*d).offset;
    mm_type$twiden(p,lv);
}

static struct mm_decls$strec *mm_type$resolvefield(struct mm_decls$strec *d,i64 m) {
        struct mm_decls$strec *  e;
        struct mm_decls$strec *  t;
        {i64 $temp = (i64)mm_decls$ttbasetype[(m)];
if (($temp==(i64)8)) {
    }
    else if (($temp==(i64)7)) {
        m = (i64)mm_decls$tttarget[(m)];
        if (((i64)mm_decls$ttbasetype[(m)] != (i64)8)) {
            mm_support$txerror((byte*)"3:record expected",0);
        }
;
    }
    else {
        mm_support$txerror((byte*)"4:record expected",0);
    }
    };
    t = mm_decls$ttnamedef[(m)];
    e = mm_name$finddupl(t,d);
    if (!(!!(e))) {
        mm_support$txerror_s((byte*)"Not a field: #",(*d).name,0);
    }
;
    return e;
}

static void mm_type$tx_andl(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
    mm_type$tpass(a,(i64)6,(i64)0);
    mm_type$tpass(b,(i64)6,(i64)0);
    (*p).mode = (i64)6;
    mm_type$setsimple(p);
}

static void mm_type$convintconst(struct mm_decls$unitrec *p,i64 x) {
    (*p).tag = (i64)1;
    (*p).mode = (i64)3;
    (*p).a = ((*p).b = ((*p).c = 0));
    (*p).value = x;
    (*p).isconst = (i64)1;
}

static void mm_type$tx_sliceptr(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a) {
        i64 m;
        i64 tmode;
    m = (i64)(*a).mode;
        {i64 $temp = (i64)mm_decls$ttbasetype[(m)];
if (($temp==(i64)11)) {
    }
    else {
        mm_support$txerror_s((byte*)"SLICEPTR #",mm_lib$strmode(m,(i64)1),0);
    }
    };
    tmode = mm_lib$createarraymodek(0,(i64)mm_decls$tttarget[(m)],(i64)mm_decls$ttlower[(m)],(i64)0,(i64)0);
    (*p).mode = mm_lib$createrefmode(0,tmode,(i64)0);
}

static void mm_type$tx_swap(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
    mm_type$tpasslv(a,(i64)22);
    mm_type$tpasslv(b,(i64)22);
    if (!(!!(mm_type$comparemodes((i64)(*a).mode,(i64)(*b).mode)))) {
        mm_support$txerror((byte*)"SWAP: type mismatch",0);
    }
;
    (*p).mode = (i64)0;
}

static void mm_type$tx_select(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,struct mm_decls$unitrec *c,i64 t,i64 lv) {
        i64 u;
        struct mm_decls$unitrec *  q;
    mm_type$tpass(a,(i64)3,(i64)0);
    q = b;
    L852 :;
    while (!!(q)) {
        mm_type$tpass(q,t,lv);
        if ((q == b)) {
            u = (i64)(*q).mode;
        }
        else {
            u = mm_type$getdominantmode(u,(i64)(*q).mode);
        }
;
        q = (*q).nextunit;
L853 :;
    }
L854 :;
    ;
    mm_type$tpass(c,t,lv);
    u = mm_type$getdominantmode(u,(i64)(*c).mode);
    q = b;
    L855 :;
    while (!!(q)) {
        mm_type$coerceunit(q,u,(i64)0);
        q = (*q).nextunit;
L856 :;
    }
L857 :;
    ;
    if ((t != (i64)0)) {
        (*p).mode = u;
    }
    else {
        (*p).mode = (i64)0;
    }
;
}

static void mm_type$tx_case(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,struct mm_decls$unitrec *c,i64 t,i64 lv) {
        i64 amode;
        i64 u;
        struct mm_decls$unitrec *  wt;
        struct mm_decls$unitrec *  w;
    if ((((i64)(*p).tag == (i64)104) && !!(lv))) {
        mm_support$gerror((byte*)"&docase",0);
    }
;
    mm_type$tpass(a,(i64)22,(i64)0);
    if ((a == 0)) {
        amode = (i64)22;
    }
    else {
        amode = (i64)(*a).mode;
    }
;
    if ((!!((i64)mm_decls$ttisinteger[(amode)]) && ((i64)mm_decls$ttsize[(amode)] < (i64)8))) {
        mm_type$coerceunit(a,(i64)3,(i64)0);
        amode = (i64)3;
    }
;
    u = (i64)0;
    wt = b;
    L858 :;
    while (!!(wt)) {
        w = (*wt).a;
        L861 :;
        while (!!(w)) {
            mm_type$tpass(w,(i64)22,(i64)0);
            if (((i64)(*w).tag == (i64)16)) {
                if (!(!!((i64)mm_decls$ttisinteger[(amode)]))) {
                    mm_support$txerror((byte*)"case: need int index",0);
                }
;
            }
            else {
                if ((amode == (i64)22)) {
                    if (!(!!((i64)mm_tables$isbooltag[((i64)(*w).tag)]))) {
                        mm_support$txerror((byte*)"CASE/BOOL?",0);
                        mm_lib$insertunit(w,(i64)14);
                    }
;
                }
                else {
                    mm_type$coerceunit(w,amode,(i64)0);
                }
;
            }
;
            w = (*w).nextunit;
L862 :;
        }
L863 :;
        ;
        mm_type$tpass((*wt).b,t,lv);
        if ((t != (i64)0)) {
            if (!!(u)) {
                u = mm_type$getdominantmode(u,(i64)(*(*wt).b).mode);
            }
            else {
                u = (i64)(*(*wt).b).mode;
            }
;
        }
;
        wt = (*wt).nextunit;
L859 :;
    }
L860 :;
    ;
    if (!!(c)) {
        mm_type$tpass(c,t,lv);
        if ((t == (i64)22)) {
            u = mm_type$getdominantmode(u,(i64)(*c).mode);
        }
;
    }
    else if ((t != (i64)0)) {
        mm_support$txerror((byte*)"case needs else",0);
    }
;
    if ((t != (i64)0)) {
        (*p).mode = u;
    }
    else {
        (*p).mode = (i64)0;
    }
;
}

static void mm_type$tx_notl(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a) {
    mm_type$tpass(a,(i64)22,(i64)0);
    (*p).mode = (i64)6;
    mm_type$setsimple(p);
}

static void mm_type$tx_istruel(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a) {
        i64 abase;
    mm_type$tpass(a,(i64)22,(i64)0);
    if (!!((i64)mm_tables$isbooltag[((i64)(*a).tag)])) {
        mm_lib$deleteunit(p,a);
        return;
    }
;
    abase = (i64)mm_decls$ttbasetype[((i64)(*a).mode)];
    if ((abase == (i64)7)) {
        abase = (i64)3;
    }
;
    (*p).mode = (i64)6;
    mm_type$setsimple(p);
}

static void mm_type$tx_typepun(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a) {
        i64 smode;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)15)) {
        mm_support$txerror((byte*)"TYPEPUN/LIST",0);
    }
    else {
        mm_type$tpass(a,(i64)22,(i64)0);
        smode = mm_lib$getmemmode(a);
        if (((i64)mm_decls$ttsize[(smode)] < (i64)mm_decls$ttsize[((i64)(*p).convmode)])) {
            mm_support$txerror((byte*)"Typepun: sizes must match",0);
        }
;
        (*p).mode = mm_lib$gettypebase((i64)(*p).convmode);
    }
    };
}

static void mm_type$tx_exit(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a) {
    if ((a == 0)) {
        return;
    }
;
    mm_type$tpass(a,(i64)3,(i64)0);
    if (((i64)(*a).tag != (i64)1)) {
        mm_support$txerror((byte*)"exit/etc not const",0);
    }
;
    (*p).index = (*a).value;
    (*p).a = 0;
}

static void mm_type$tx_goto(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a) {
        i64 m;
    mm_type$tpass(a,(i64)22,(i64)0);
    m = (i64)(*a).mode;
    if ((((i64)mm_decls$ttbasetype[(m)] != (i64)7) || ((i64)mm_decls$ttbasetype[((i64)mm_decls$tttarget[(m)])] != (i64)24))) {
        mm_support$txerror((byte*)"goto: not label",0);
    }
;
}

static void mm_type$tx_switch(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,struct mm_decls$unitrec *c,i64 t,i64 lv) {
        byte valueset[2048];
        struct mm_decls$unitrec *  wt;
        struct mm_decls$unitrec *  w;
        i64 ax;
        i64 bx;
        i64 i;
        i64 u;
    if ((((i64)(*p).tag == (i64)106) && !!(lv))) {
        mm_support$gerror((byte*)"&doswitch",0);
    }
;
    mm_type$tpass(a,(i64)3,(i64)0);
    memset(&valueset,(i32)(i64)0,(u64)2048u);
    u = (i64)0;
    wt = b;
    L864 :;
    while (!!(wt)) {
        w = (*wt).a;
        L867 :;
        while (!!(w)) {
            mm_type$tpass(w,(i64)22,(i64)0);
            if (!(!!(mm_lib$isconstunit(w)))) {
                mm_support$txerror((byte*)"Switch not constant",0);
            }
;
                        {i64 $temp = (i64)mm_decls$ttbasetype[((i64)(*w).mode)];
if (($temp==(i64)9)) {
                ax = (*(*w).a).value;
                bx = (*(*w).b).value;
                //dorange:
L870 :;
;
                for (i=ax;i<=bx;++i) {
L871 :;
                    if (((i < (i64)0) || (i > (i64)2047))) {
                        mm_support$txerror((byte*)"switch: value out of range",0);
                    }
;
                    if (!!((i64)valueset[(i)])) {
                        msysc$m_print_startcon();
                        msysc$m_print_i64(i,NULL);
                        msysc$m_print_newline();
                        msysc$m_print_end();
                        ;
                        mm_support$txerror((byte*)"Duplicate switch value",0);
                    }
;
                    valueset[(i)] = (i64)1;
L872 :;
                }
L873 :;
                ;
            }
            else {
                mm_type$coerceunit(w,(i64)3,(i64)0);
                mm_type$tevaluate(w);
                if (((i64)(*w).tag != (i64)1)) {
                    mm_support$txerror((byte*)"Switch value: not const int",0);
                }
;
                ax = (bx = (*w).value);
                goto L870 ;
;
            }
            };
            w = (*w).nextunit;
L868 :;
        }
L869 :;
        ;
        mm_type$tpass((*wt).b,t,lv);
        if ((t == (i64)22)) {
            if (!!(u)) {
                u = mm_type$getdominantmode(u,(i64)(*(*wt).b).mode);
            }
            else {
                u = (i64)(*(*wt).b).mode;
            }
;
        }
;
        wt = (*wt).nextunit;
L865 :;
    }
L866 :;
    ;
    if (!!(c)) {
        mm_type$tpass(c,t,lv);
        if ((t == (i64)22)) {
            u = mm_type$getdominantmode(u,(i64)(*c).mode);
        }
;
    }
    else if ((t != (i64)0)) {
        mm_support$txerror((byte*)"switch needs else",0);
    }
;
    if ((t != (i64)0)) {
        w = (*b).a;
        L874 :;
        while (!!(w)) {
            if ((t == (i64)22)) {
                mm_type$coerceunit((*b).b,u,(i64)0);
            }
;
            (*w).mode = (i64)(*(*b).b).mode;
            w = (*w).nextunit;
L875 :;
        }
L876 :;
        ;
        if ((t == (i64)22)) {
            mm_type$coerceunit(c,u,(i64)0);
            (*p).mode = u;
        }
        else {
            (*p).mode = t;
        }
;
    }
    else {
        (*p).mode = (i64)0;
    }
;
}

static void mm_type$tx_addroffirst(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 t) {
        i64 m;
    mm_type$tpass(a,(i64)22,(i64)0);
    m = (i64)(*a).mode;
    if (((i64)mm_decls$ttbasetype[(m)] != (i64)10)) {
        mm_support$txerror((byte*)"&. ref[] expected",0);
    }
;
    m = mm_lib$createrefmode(0,(i64)mm_decls$tttarget[(m)],(i64)0);
    if (((i64)(*a).tag == (i64)3)) {
        (*a).addroffirst = (i64)1;
    }
;
    (*p).mode = m;
}

static void mm_type$tx_return(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 t) {
        i64 m;
        i64 nret;
        i64 i;
        i32 (*pmult)[];
        struct mm_decls$unitrec *  q;
    m = (i64)(*mm_decls$currproc).mode;
    nret = (i64)(*mm_decls$currproc).nretvalues;
    pmult = mm_decls$ttmult[((i64)(*mm_decls$currproc).mode)];
    if ((a == 0)) {
        if (!!(nret)) {
            mm_support$txerror((byte*)"return value(s) missing",0);
        }
;
        return;
    }
    else if ((nret == (i64)0)) {
        mm_support$txerror((byte*)"Superfluous return value",0);
    }
;
    if (((i64)(*a).tag == (i64)15)) {
        (*a).tag = (i64)20;
        if (((i64)(*a).length != nret)) {
                        {i64 $temp = (i64)mm_decls$ttbasetype[(m)];
if (($temp==(i64)8) || ($temp==(i64)10)) {
                mm_support$txerror((byte*)"return constructor not supported",0);
            }
            else {
                mm_support$txerror((byte*)"Wrong number of return values",0);
            }
            };
        }
;
        q = (*a).a;
        for (i=(i64)1;i<=nret;++i) {
L877 :;
            mm_type$tpass(q,(i64)(*pmult)[(i)-1],(i64)0);
            q = (*q).nextunit;
L878 :;
        }
L879 :;
        ;
        mm_lib$deleteunit(p,a);
        (*p).resultflag = (i64)1;
        if ((t == (i64)0)) {
            (*p).mode = (i64)0;
        }
        else {
            (*p).mode = (i64)27;
        }
;
    }
    else {
        if ((nret > (i64)1)) {
            mm_support$txerror((byte*)"RETERROR?",0);
        }
;
        mm_type$tpass(a,m,(i64)0);
        if ((t == (i64)0)) {
            (*p).mode = (i64)0;
        }
        else {
            mm_lib$deleteunit(p,a);
        }
;
    }
;
    if (!!((i64)mm_decls$ttisshort[((i64)(*p).mode)])) {
        mm_support$txerror((byte*)"SHORT RET TYPE",0);
    }
;
}

static void mm_type$tx_dotindex(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,i64 lv) {
        i64 pmode;
        struct mm_decls$unitrec *  i;
        struct mm_decls$unitrec *  j;
    mm_type$tpass(a,(i64)22,lv);
    pmode = (i64)2;
    if (!(!!((i64)mm_decls$ttisinteger[((i64)(*a).mode)]))) {
        mm_support$txerror((byte*)"a.[i]: not int/str value",0);
    }
;
    mm_type$tpass(b,(i64)22,(i64)0);
        {i64 $temp = (i64)mm_decls$ttbasetype[((i64)(*b).mode)];
if (($temp==(i64)9)) {
        i = (*b).a;
        j = (*b).b;
        if (((i64)(*i).tag==(i64)(*j).tag && (i64)(*j).tag==(i64)1)) {
            if (((*i).value > (*j).value)) {
                {struct mm_decls$unitrec *  temp = (*b).a; (*b).a = (*b).b; (*b).b = temp; };
            }
;
        }
;
    }
    else {
        mm_type$coerceunit(b,(i64)3,(i64)0);
    }
    };
    (*p).mode = pmode;
    mm_type$setsimple(p);
}

static void mm_type$tx_slice(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
    mm_type$tpass(a,(i64)22,(i64)0);
    mm_type$tpass(b,(i64)22,(i64)0);
    mm_type$setsimple(a);
    if (((i64)(*a).mode == (i64)20)) {
        (*p).mode = mm_lib$createslicemodek(mm_decls$currproc,(i64)12,(i64)1,(i64)0);
    }
    else {
        mm_type$deref(a,(i64)1);
                {i64 $temp = (i64)mm_decls$ttbasetype[((i64)(*a).mode)];
if (($temp==(i64)10)) {
            (*p).mode = mm_lib$createslicemodek(mm_decls$currproc,(i64)mm_decls$tttarget[((i64)(*a).mode)],(i64)1,(i64)0);
        }
        else if (($temp==(i64)11)) {
            (*p).mode = (i64)(*a).mode;
        }
        else {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"STRMODE(A.MODE)=",NULL);
            msysc$m_print_str(mm_lib$strmode((i64)(*a).mode,(i64)1),NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            mm_support$txerror((byte*)"a[i..j]: not array",0);
        }
        };
    }
;
}

static void mm_type$twiden(struct mm_decls$unitrec *p,i64 lv) {
        i64 m;
        i64 mbase;
    mbase = (i64)mm_decls$ttbasetype[((m = (i64)(*p).mode))];
    if ((mbase == (i64)0)) {
        return;
    }
;
    if (!!(lv)) {
        return;
    }
;
    if (!(!!((i64)mm_decls$ttisshort[(mbase)]))) {
        return;
    }
;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)3) || ($temp==(i64)45) || ($temp==(i64)40) || ($temp==(i64)42)) {
        (*p).memmode = m;
        (*p).mode = mm_lib$gettypebase(m);
    }
    else if (($temp==(i64)86) || ($temp==(i64)28)) {
        (*p).memmode = m;
        (*p).mode = mm_lib$gettypebase(m);
    }
    else {
        mm_diags$printunit((struct mm_decls$unitrec *)p,(i64)0,(byte*)"*",0);
        mm_support$txerror_s((byte*)"widen? #",mm_tables$jtagnames[((i64)(*p).tag)],0);
    }
    };
}

static void mm_type$tstringslice(struct mm_decls$unitrec *p,i64 slicemode) {
        struct mm_decls$unitrec *  a;
        struct mm_decls$unitrec *  b;
        struct mm_decls$unitrec *  prange;
    if (((i64)mm_decls$tttarget[(slicemode)] != (i64)12)) {
        mm_support$txerror((byte*)"Not char slice",0);
    }
;
    a = p;
    mm_lib$insertunit(p,(i64)41);
    if (((i64)(*(*p).a).tag == (i64)1)) {
    }
    else {
        b = mm_lib$duplunit((*p).a,(i64)0);
        mm_lib$insertunit(b,(i64)32);
        prange = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)16,(struct mm_decls$unitrec *)mm_lib$createconstunit((u64)1u,(i64)3),(struct mm_decls$unitrec *)b);
        (*prange).mode = (i64)9;
        (*p).b = prange;
    }
;
    (*p).mode = slicemode;
}

static void mm_type$tx_bitfield(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,i64 lv) {
        i64 i;
        i64 j;
        i64 bitsize;
        i64 topbit;
        struct mm_decls$unitrec *  r;
    mm_type$tpass(a,(i64)22,lv);
    if ((!(!!((i64)mm_decls$ttisinteger[((i64)(*a).mode)])) && !(!!((i64)mm_decls$ttisref[((i64)(*a).mode)])))) {
        mm_support$txerror((byte*)"Int/ref needed",0);
    }
;
    bitsize = ((i64)mm_decls$ttsize[((i64)mm_decls$ttbasetype[((i64)(*a).mode)])] * (i64)8);
    topbit = (bitsize - (i64)1);
        {i64 $temp = (i64)(*p).bfcode;
if (($temp==(i64)2)) {
        i = (i64)0;
        j = (i64)7;
    }
    else if (($temp==(i64)1)) {
        j = topbit;
        i = (topbit - (i64)7);
    }
    else if (($temp==(i64)4)) {
        i = (j = (i64)0);
    }
    else if (($temp==(i64)7) || ($temp==(i64)8)) {
        if (!!(lv)) {
            mm_support$txerror((byte*)"Can't assign",0);
        }
;
        i = (j = (i64)0);
    }
    else if (($temp==(i64)3)) {
        i = (j = topbit);
    }
    else if (($temp==(i64)6)) {
        i = (i64)0;
        j = ((bitsize / (i64)2) - (i64)1);
    }
    else if (($temp==(i64)5)) {
        i = (bitsize / (i64)2);
        j = topbit;
    }
    else {
        msysc$m_print_startcon();
        msysc$m_print_i64((i64)(*p).bfcode,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        mm_support$txerror((byte*)"BITFIELD",0);
    }
    };
    if ((i == j)) {
        (*p).tag = (i64)43;
        (*p).b = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)i,(i64)3);
        (*p).resultflag = (i64)1;
        (*(*p).b).resultflag = (i64)1;
        if (((i64)(*p).bitopindex == (i64)8)) {
            (*p).mode = (i64)2;
            mm_type$addnotl(p);
        }
;
    }
    else {
        r = (struct mm_decls$unitrec *)mm_lib$createunit2((i64)16,(struct mm_decls$unitrec *)mm_lib$createconstunit((u64)i,(i64)3),(struct mm_decls$unitrec *)mm_lib$createconstunit((u64)j,(i64)3));
        (*(*r).a).resultflag = (i64)1;
        (*(*r).b).resultflag = (i64)1;
        (*r).mode = (i64)9;
        (*p).tag = (i64)44;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"HERE",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        (*p).b = r;
    }
;
    (*p).mode = (i64)2;
}

static void mm_type$deref(struct mm_decls$unitrec *a,i64 needres) {
        i64 abasemode;
        i64 tmode;
    abasemode = (i64)mm_decls$ttbasetype[((i64)(*a).mode)];
    L880 :;
    while ((abasemode == (i64)7)) {
        tmode = (i64)mm_decls$tttarget[((i64)(*a).mode)];
        mm_lib$insertunit(a,(i64)45);
        mm_type$setsimple(a);
        (*a).mode = tmode;
        abasemode = (i64)mm_decls$ttbasetype[((i64)(*a).mode)];
L881 :;
    }
L882 :;
    ;
}

static void mm_type$tmethodcall(struct mm_decls$unitrec *p,struct mm_decls$unitrec *pdot,struct mm_decls$unitrec *pargs) {
        i64 mrec;
        struct mm_decls$unitrec *  prec;
        struct mm_decls$unitrec *  pfield;
        struct mm_decls$unitrec *  pfunc;
        struct mm_decls$strec *  d;
        struct mm_decls$strec *  e;
    prec = (*pdot).a;
    pfield = (*pdot).b;
    mrec = (i64)(*prec).mode;
    d = (*pfield).def;
    e = mm_type$resolvefield(d,mrec);
    if ((e == 0)) {
        mm_support$txerror_s((byte*)"Can't resolve method:",(*d).name,0);
    }
;
    pfunc = (struct mm_decls$unitrec *)mm_lib$createname(e);
    (*pfunc).mode = (i64)(*e).mode;
    (*prec).nextunit = pargs;
    (*p).a = pfunc;
    (*p).b = prec;
}

static void mm_type$do_bounds(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a) {
        i64 m;
        i64 mbase;
    mm_type$deref(a,(i64)1);
    m = (i64)(*a).mode;
    if (((i64)(*a).tag == (i64)52)) {
        m = (*a).value;
    }
;
    mbase = (i64)mm_decls$ttbasetype[(m)];
    (*p).mode = (i64)3;
        {i64 $temp = (i64)(*p).pclop;
if (($temp==(i64)89)) {
        if ((mbase==(i64)10) || (mbase==(i64)11)) {
            mm_type$convintconst(p,(i64)mm_decls$ttlower[(m)]);
            return;
        }
        else {
            //error:
L883 :;
;
            mm_support$txerror_s((byte*)"lwb/upb/len?",mm_lib$strmode(m,(i64)1),0);
        }
;
    }
    else if (($temp==(i64)90)) {
        if ((mbase==(i64)10)) {
            mm_type$convintconst(p,(((i64)mm_decls$ttlower[(m)] + (i64)mm_decls$ttlength[(m)]) - (i64)1));
        }
        else if ((mbase==(i64)11)) {
            (*p).pclop = (i64)90;
        }
        else {
            goto L883 ;
;
        }
;
    }
    else if (($temp==(i64)88)) {
        if ((mbase==(i64)10)) {
            mm_type$convintconst(p,(i64)mm_decls$ttlength[(m)]);
        }
        else if ((mbase==(i64)11)) {
            (*p).pclop = (i64)88;
        }
        else {
            goto L883 ;
;
        }
;
    }
    else if (($temp==(i64)91)) {
        (*p).mode = (i64)9;
        if ((mbase==(i64)10)) {
            (*p).range_lower = (i64)mm_decls$ttlower[(m)];
            (*p).range_upper = (((*p).range_lower + (i64)mm_decls$ttlength[(m)]) - (i64)1);
            (*p).tag = (i64)1;
            (*p).a = ((*p).b = ((*p).c = 0));
            (*p).isconst = (i64)1;
            return;
        }
        else if ((mbase==(i64)11)) {
        }
        else {
            goto L883 ;
;
        }
;
    }
    };
}

static void mm_type$addnotl(struct mm_decls$unitrec *p) {
    mm_lib$insertunit(p,(i64)13);
    (*p).mode = (i64)6;
    (*p).pclop = (i64)32;
}

static void mm_type$tevaluate(struct mm_decls$unitrec *p) {
        struct mm_decls$unitrec *  a;
        struct mm_decls$unitrec *  b;
        i64 tag;
    tag = (i64)(*p).tag;
    if (((i64)mm_tables$jisexpr[(tag)] == (i64)2)) {
        mm_type$tevalbinop(p);
        mm_type$setsimple(p);
    }
    else if (((i64)mm_tables$jisexpr[(tag)] == (i64)1)) {
        mm_type$tevalmonop(p);
        if ((((i64)(*p).tag == (i64)32) && !(!!((i64)mm_tables$complexopset[((i64)(*p).pclop)])))) {
            mm_type$setsimple(p);
        }
;
    }
    else {
        if ((tag==(i64)16)) {
            a = (*p).a;
            b = (*p).b;
            if (((i64)mm_decls$ttsize[((i64)(*a).mode)] <= (i64)8)) {
                mm_type$tevaluate(a);
                mm_type$tevaluate(b);
                if ((((i64)(*a).tag == (i64)1) && ((i64)(*b).tag == (i64)1))) {
                    (*p).isconst = ((i64)(*a).isconst & (i64)(*b).isconst);
                }
;
            }
;
        }
;
    }
;
}

static struct mm_decls$unitrec *mm_type$addrdotindex(struct mm_decls$unitrec *p,i64 *offset) {
        struct mm_decls$unitrec *  q;
        i64 axmode;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"ADDRDOTIX",NULL);
    msysc$m_print_str(mm_lib$strmode((i64)(*p).mode,(i64)1),NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)42)) {
        if (((i64)(*(*p).a).tag == (i64)3)) {
            (*offset) = (i64)(*p).offset;
            return (*p).a;
        }
        else {
            q = mm_type$addrdotindex((*p).a,offset);
            (*offset) += (i64)(*p).offset;
            return q;
        }
;
    }
    else if (($temp==(i64)40)) {
        axmode = (i64)(*(*p).a).mode;
        if (((i64)(*(*p).b).tag == (i64)1)) {
            if (((i64)(*(*p).a).tag == (i64)3)) {
                (*offset) = (((*(*p).b).value - (i64)mm_decls$ttlower[(axmode)]) * (i64)mm_decls$ttsize[((i64)mm_decls$tttarget[(axmode)])]);
                return (*p).a;
            }
            else {
                q = mm_type$addrdotindex((*p).a,offset);
                if (!!(q)) {
                    (*offset) += (((*(*p).b).value - (i64)mm_decls$ttlower[(axmode)]) * (i64)mm_decls$ttsize[((i64)mm_decls$tttarget[(axmode)])]);
                }
;
                return q;
            }
;
        }
        else {
            return 0;
        }
;
    }
    else {
        return 0;
    }
    };
}

static void mm_type$tevalbinop(struct mm_decls$unitrec *p) {
        i64 a;
        i64 b;
        i64 c;
        i64 offset;
        r64 x;
        r64 y;
        r64 z;
        struct mm_decls$unitrec *  lhs;
        struct mm_decls$unitrec *  rhs;
    lhs = (*p).a;
    rhs = (*p).b;
    if (!(((i64)(*lhs).tag==(i64)(*rhs).tag && (i64)(*rhs).tag==(i64)1))) {
        if ((((i64)(*lhs).tag == (i64)46) && ((i64)(*rhs).tag == (i64)1))) {
            if (((i64)(*(*lhs).a).tag == (i64)3)) {
                offset = ((*rhs).value * (i64)mm_decls$ttsize[((i64)mm_decls$tttarget[((i64)(*lhs).mode)])]);
                if (((*lhs).b == 0)) {
                    (*lhs).b = (struct mm_decls$unitrec *)mm_lib$createconstunit((u64)offset,(i64)3);
                }
                else {
                    (*(*lhs).b).value += offset;
                }
;
                mm_lib$deleteunit(p,lhs);
            }
;
        }
;
        return;
    }
;
    if (!!((i64)mm_decls$ttisreal[((i64)(*p).mode)])) {
        x = (*(*p).a).xvalue;
        y = (*(*p).b).xvalue;
    }
    else {
        a = (*(*p).a).value;
        b = (*(*p).b).value;
    }
;
        {i64 $temp = (i64)(*p).mode;
if (($temp==(i64)3) || ($temp==(i64)2)) {
        switch ((i64)(*p).pclop) {
        case 1:;
            {
                c = (a + b);
            }
            break;
        case 2:;
            {
                c = (a - b);
            }
            break;
        case 3:;
            {
                c = (a * b);
            }
            break;
        case 5:;
            {
                c = (a / b);
            }
            break;
        case 6:;
            {
                c = (a % b);
            }
            break;
        case 11:;
            {
                c = (a << b);
            }
            break;
        case 17:;
            {
                c = (i64)(a == b);
            }
            break;
        case 18:;
            {
                c = (i64)(a != b);
            }
            break;
        case 19:;
            {
                c = (i64)(a < b);
            }
            break;
        case 20:;
            {
                c = (i64)(a <= b);
            }
            break;
        case 21:;
            {
                c = (i64)(a >= b);
            }
            break;
        case 22:;
            {
                c = (i64)(a > b);
            }
            break;
        case 24:;
            {
                c = (i64)(!!(a) && !!(b));
            }
            break;
        case 25:;
            {
                c = (i64)(!!(a) || !!(b));
            }
            break;
        case 8:;
            {
                c = (a & b);
            }
            break;
        case 9:;
            {
                c = (a | b);
            }
            break;
        default: {
            return;
        }
        } //SW
;
    }
    else if (($temp==(i64)5) || ($temp==(i64)4)) {
        switch ((i64)(*p).pclop) {
        case 1:;
            {
                z = (x + y);
            }
            break;
        case 2:;
            {
                z = (x - y);
            }
            break;
        case 3:;
            {
                z = (x * y);
            }
            break;
        case 4:;
            {
                z = (x / y);
            }
            break;
        default: {
            return;
        }
        } //SW
;
    }
    else {
        return;
    }
    };
    if (!!((i64)mm_decls$ttisreal[((i64)(*p).mode)])) {
        mm_type$makenewconst(p,*(i64*)&z,(i64)0);
    }
    else {
        mm_type$makenewconst(p,c,(i64)0);
    }
;
}

static void mm_type$tevalmonop(struct mm_decls$unitrec *p) {
        i64 a;
        i64 c;
        r64 x;
        r64 z;
    if (((i64)(*p).tag == (i64)30)) {
        return;
    }
;
    if (!(((i64)(*(*p).a).tag == (i64)1))) {
        return;
    }
;
    a = (*(*p).a).value;
    x = (*(*p).a).xvalue;
        {i64 $temp = (i64)(*p).mode;
if (($temp==(i64)3) || ($temp==(i64)2)) {
        switch ((i64)(*p).pclop) {
        case 29:;
            {
                c = -(a);
            }
            break;
        case 33:;
            {
                c = (i64)!!(a);
                (*p).mode = (i64)6;
            }
            break;
        case 32:;
            {
                c = (i64)!(!!(a));
                (*p).mode = (i64)6;
            }
            break;
        case 31:;
            {
                c = ~(a);
            }
            break;
        case 30:;
            {
                c = m$llabs(a);
            }
            break;
        default: {
            return;
        }
        } //SW
;
    }
    else if (($temp==(i64)5) || ($temp==(i64)4)) {
        switch ((i64)(*p).pclop) {
        case 29:;
            {
                z = -(x);
            }
            break;
        case 41:;
            {
                z = atan(x);
            }
            break;
        case 35:;
            {
                z = sqrt(x);
            }
            break;
        default: {
            return;
        }
        } //SW
;
    }
    else if (($temp==(i64)6)) {
                {i64 $temp = (i64)(*p).pclop;
if (($temp==(i64)33)) {
            c = (i64)!!(a);
            (*p).mode = (i64)6;
        }
        else if (($temp==(i64)32)) {
            c = (i64)!(!!(a));
            (*p).mode = (i64)6;
        }
        };
    }
    else {
        return;
    }
    };
    if (!!((i64)mm_decls$ttisreal[((i64)(*p).mode)])) {
        mm_type$makenewconst(p,*(i64*)&z,(i64)0);
    }
    else {
        mm_type$makenewconst(p,c,(i64)0);
    }
;
}

static i64 mm_type$iscondtrue(struct mm_decls$unitrec *p) {
    return (i64)(((i64)(*p).tag == (i64)1) && ((*p).value != (i64)0));
}

static i64 mm_type$iscondfalse(struct mm_decls$unitrec *p) {
    return (i64)(((i64)(*p).tag == (i64)1) && ((*p).value == (i64)0));
}

static void mm_type$fixchararray(struct mm_decls$unitrec *a) {
    if (((!!(a) && ((i64)mm_decls$ttbasetype[((i64)(*a).mode)] == (i64)10)) && ((i64)mm_decls$tttarget[((i64)(*a).mode)] == (i64)12))) {
        mm_type$coerceunit(a,(i64)20,(i64)0);
    }
;
}

static void mm_type$combinestrings(struct mm_decls$unitrec *p) {
        struct mm_decls$unitrec *  a;
        struct mm_decls$unitrec *  b;
        i64 alen;
        i64 blen;
        i64 clen;
        u8 *  s;
    a = (*p).a;
    b = (*p).b;
    alen = (i64)(*a).length;
    blen = (i64)(*b).length;
    clen = (alen + blen);
    if ((blen == (i64)0)) {
        mm_lib$deleteunit(p,a);
        return;
    }
    else if ((alen == (i64)0)) {
        mm_lib$deleteunit(p,b);
        return;
    }
;
    s = (u8 *)mlib$pcm_alloc((clen + (i64)1));
    memcpy((void *)s,(void *)(*a).svalue,(u64)alen);
    memcpy((void *)(s + alen),(void *)(*b).svalue,(u64)blen);
    (*(s + clen)) = (u64)0u;
    mm_lib$deleteunit(p,a);
    (*p).length = clen;
    (*p).svalue = s;
}

static void mm_type$tx_strinclude(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a) {
        i64 fileno;
    mm_type$tpass(a,(i64)22,(i64)0);
    if ((((i64)(*a).tag != (i64)1) || !(!!((i64)(*a).isastring)))) {
        mm_support$txerror((byte*)"strincl/not string",0);
    }
;
    fileno = (i64)mm_decls$moduletable[((i64)(*p).moduleno)].fileno;
    fileno = mm_support$getsupportfile((*a).svalue,(byte*)"",mm_decls$sourcefilepaths[(fileno)],(i64)0,(i64)1);
    (*a).svalue = mm_decls$sourcefiletext[(fileno)];
    (*a).slength = mm_decls$sourcefilesizes[(fileno)];
    mm_lib$deleteunit(p,a);
}

static void mm_type$coerceunit(struct mm_decls$unitrec *p,i64 t,i64 hard) {
        i64 opc;
        i64 s;
    s = (i64)(*p).mode;
    if (((t == (i64)0) || (s == t))) {
        return;
    }
;
    if ((s == (i64)0)) {
        mm_support$txerror((byte*)"Void expression/return value missing",0);
    }
;
    opc = mm_type$getconversionop(s,t,hard);
    mm_type$applyconversion(p,s,t,opc);
    mm_type$setsimple(p);
}

static i64 mm_type$getconversionop(i64 s,i64 t,i64 hard) {
        i64 opc;
        i64 sbase;
        i64 tbase;
        i64 starg;
        i64 ttarg;
    sbase = (i64)mm_decls$ttbasetype[(s)];
    tbase = (i64)mm_decls$ttbasetype[(t)];
    if ((s == t)) {
        return (i64)0;
    }
;
    opc = (i64)98;
    starg = (i64)mm_decls$tttarget[(s)];
    ttarg = (i64)mm_decls$tttarget[(t)];
    if ((s == (i64)20)) {
        sbase = (i64)20;
    }
;
    if ((t == (i64)20)) {
        tbase = (i64)20;
    }
;
    switch (sbase) {
    case 1:;
    case 2:;
    case 3:;
    case 4:;
    case 5:;
        {
            switch (tbase) {
            case 1:;
            case 2:;
            case 3:;
            case 4:;
            case 5:;
                {
                    opc = (i64)mm_tables$softconvtable[(sbase)-1][(tbase)-1];
                }
                break;
            case 7:;
            case 20:;
                {
                    opc = (i64)82;
                    //checkhard:
L884 :;
;
                    if (!(!!(hard))) {
                        opc = (i64)99;
                    }
;
                }
                break;
            case 12:;
            case 13:;
            case 14:;
            case 15:;
            case 16:;
            case 17:;
            case 18:;
            case 19:;
                {
                    if (!!((i64)mm_decls$ttisinteger[(sbase)])) {
                        if (!(!!(hard))) {
                            opc = (i64)102;
                        }
                        else {
                            opc = (i64)85;
                        }
;
                    }
;
                }
                break;
            case 6:;
                {
                    opc = (i64)33;
                }
                break;
            case 25:;
                {
                    opc = (i64)82;
                }
                break;
            } //SW
;
        }
        break;
    case 6:;
        {
            if ((tbase == (i64)3 || tbase == (i64)2)) {
                opc = (i64)82;
            }
;
        }
        break;
    case 7:;
        {
            if ((tbase==(i64)3) || (tbase==(i64)2)) {
                opc = (i64)82;
                goto L884 ;
;
            }
            else if ((tbase==(i64)7)) {
                if (((starg == (i64)0) || (ttarg == (i64)0))) {
                    if (((u64)1u && (ttarg == (i64)0))) {
                        return (i64)0;
                    }
;
                    opc = (i64)82;
                }
                else {
                    //checkref:
L885 :;
;
                    opc = (i64)82;
                    if (!(!!(mm_type$comparemodes(s,t)))) {
                        goto L884 ;
;
                    }
;
                }
;
            }
            else if ((tbase==(i64)20)) {
                goto L885 ;
;
            }
            else if ((tbase==(i64)6)) {
                opc = (i64)33;
            }
;
        }
        break;
    case 20:;
        {
            if ((tbase==(i64)3) || (tbase==(i64)2)) {
                opc = (i64)82;
                goto L884 ;
;
            }
            else if ((tbase==(i64)7)) {
                if ((!!(mm_type$comparemodes(s,t)) || !!(hard))) {
                    opc = (i64)82;
                }
                else {
                    opc = (i64)99;
                }
;
            }
            else if ((tbase==(i64)6)) {
                opc = (i64)33;
            }
            else if ((tbase==(i64)11)) {
                opc = (i64)101;
            }
;
        }
        break;
    case 10:;
        {
            if ((tbase==(i64)10)) {
                if (!!(mm_type$comparemodes(s,t))) {
                    opc = (i64)82;
                }
;
            }
            else if ((tbase==(i64)11)) {
                if (!!(mm_type$comparemodes(starg,ttarg))) {
                    opc = (i64)100;
                }
;
            }
            else if ((tbase==(i64)20)) {
                if ((starg == (i64)12 || starg == (i64)17)) {
                    opc = (i64)103;
                }
;
            }
;
        }
        break;
    case 11:;
        {
            if ((tbase==(i64)11)) {
                if (!!(mm_type$comparemodes(s,t))) {
                    opc = (i64)82;
                }
;
            }
            else if ((tbase==(i64)7)) {
                if (((ttarg == (i64)0) || !!(mm_type$comparemodes(starg,ttarg)))) {
                    opc = (i64)104;
                }
;
            }
;
        }
        break;
    case 25:;
        {
            if ((tbase <= (i64)5)) {
                opc = (i64)82;
            }
;
        }
        break;
    default: {
        return (i64)98;
    }
    } //SW
;
    return opc;
}

static void mm_type$applyconversion(struct mm_decls$unitrec *p,i64 s,i64 t,i64 opc) {
        struct mm_decls$unitrec *  q;
    if ((opc==(i64)0)) {
        return;
    }
    else if ((opc==(i64)98)) {
        mm_support$txerror_ss((byte*)"Can't do conversion: # => #",mm_lib$strmode(s,(i64)1),mm_lib$strmode2(t,(i64)1));
    }
    else if ((opc==(i64)99)) {
        mm_support$txerror_ss((byte*)"Need explicit cast: # => #",mm_lib$strmode(s,(i64)1),mm_lib$strmode2(t,(i64)1));
    }
    else if ((opc==(i64)82)) {
        if ((u64)0u) {
            (*p).mode = t;
            return;
        }
;
    }
    else if ((opc==(i64)102)) {
        if (!!(mm_type$tevalconvert(p,s,t,opc))) {
            return;
        }
;
        mm_lib$insertunit(p,(i64)49);
        (*p).mode = t;
        return;
    }
    else if ((opc==(i64)100)) {
        mm_lib$insertunit(p,(i64)41);
        (*p).mode = t;
        return;
    }
    else if ((opc==(i64)101)) {
        mm_type$tstringslice(p,t);
        return;
    }
    else if ((opc==(i64)103)) {
        mm_lib$insertunit(p,(i64)47);
        (*p).mode = (i64)20;
        return;
    }
;
    if (!!(mm_type$tevalconvert(p,s,t,opc))) {
        return;
    }
;
    if (((u64)1u && ((i64)(*p).tag == (i64)4))) {
        (*p).mode = t;
        q = (*p).a;
        L886 :;
        while (!!((*q).nextunit)) {
            q = (*q).nextunit;
L887 :;
        }
L888 :;
        ;
        p = q;
        goto L889 ;
;
    }
    else {
        //dorest:
L889 :;
;
        mm_lib$insertunit(p,(i64)48);
        (*p).pclop = opc;
        (*p).convmode = s;
        (*p).resultflag = (i64)1;
        if (!!((i64)mm_decls$ttisshort[(t)])) {
            (*p).convmode = t;
            t = mm_lib$gettypebase(t);
        }
;
        (*p).mode = t;
    }
;
}

static void mm_type$checkmodes(i64 s,i64 t) {
    if (!(!!(mm_type$comparemodes(s,t)))) {
        mm_support$txerror_ss((byte*)"Type-compare error: # <-> #",mm_lib$strmode(s,(i64)1),mm_lib$strmode2(t,(i64)1));
    }
;
}

static i64 mm_type$comparemodes(i64 s,i64 t) {
        i64 sbase;
        i64 tbase;
        i64 starg;
        i64 ttarg;
        struct mm_decls$strec *  d;
        struct mm_decls$strec *  e;
    if ((s == t)) {
        return (i64)1;
    }
;
    sbase = (i64)mm_decls$ttbasetype[(s)];
    tbase = (i64)mm_decls$ttbasetype[(t)];
    starg = (i64)mm_decls$tttarget[(s)];
    ttarg = (i64)mm_decls$tttarget[(t)];
    if ((sbase == tbase)) {
        if ((sbase==(i64)7)) {
            if (((starg == (i64)0) || (ttarg == (i64)0))) {
                return (i64)1;
            }
;
            return mm_type$comparemodes(starg,ttarg);
        }
        else if ((sbase==(i64)10)) {
            if (!(!!(mm_type$comparemodes(starg,ttarg)))) {
                return (i64)0;
            }
;
            if (((((i64)mm_decls$ttlength[(s)] == (i64)mm_decls$ttlength[(t)]) || ((i64)mm_decls$ttlength[(s)] == (i64)0)) || ((i64)mm_decls$ttlength[(t)] == (i64)0))) {
                return (i64)1;
            }
;
        }
        else if ((sbase==(i64)11)) {
            return mm_type$comparemodes(starg,ttarg);
        }
        else if ((sbase==(i64)23)) {
            d = mm_decls$ttnamedef[(s)];
            e = mm_decls$ttnamedef[(t)];
            if ((!!(d) && !!(e))) {
                if (!(!!(mm_type$comparemodes((i64)(*d).mode,(i64)(*e).mode)))) {
                    return (i64)0;
                }
;
                if ((((*d).paramlist == 0) && ((*e).paramlist == 0))) {
                    return (i64)1;
                }
;
            }
;
        }
;
    }
    else if ((((sbase == (i64)12) && (tbase == (i64)17)) || ((sbase == (i64)17) && (tbase == (i64)12)))) {
        return (i64)1;
    }
    else {
    }
;
    return (i64)0;
}

static i64 mm_type$tevalconvert(struct mm_decls$unitrec *p,i64 s,i64 t,i64 opc) {
        r64 x;
        r64 z;
        i64 a;
        i64 c;
        i64 sbase;
        i64 tbase;
    if (((i64)(*p).tag != (i64)1)) {
        mm_type$setsimple(p);
        return (i64)0;
    }
;
    a = (*p).value;
    x = (*p).xvalue;
        {i64 $temp = ((s << (i64)16) | t);
if (($temp==(i64)196613) || ($temp==(i64)196612)) {
        z = (r64)a;
    }
    else if (($temp==(i64)327683)) {
        c = (i64)x;
    }
    else if (($temp==(i64)327684)) {
        z = (r64)(r32)x;
    }
    else if (($temp==(i64)196625)) {
        c = (i64)(byte)a;
    }
    else if (($temp==(i64)196623)) {
        c = (i64)(i16)a;
    }
    else {
        if (((!!((i64)mm_decls$ttisinteger[(s)]) && !!((i64)mm_decls$ttisinteger[(t)])) && ((i64)mm_decls$ttsize[(s)] == (i64)mm_decls$ttsize[(t)]))) {
            c = a;
        }
        else {
            sbase = (i64)mm_decls$ttbasetype[(s)];
            tbase = (i64)mm_decls$ttbasetype[(t)];
            if ((sbase == tbase)) {
                return (i64)1;
            }
;
            return (i64)0;
        }
;
    }
    };
    if (!!((i64)mm_decls$ttisreal[(t)])) {
        mm_type$makenewconst(p,*(i64*)&z,t);
    }
    else {
        mm_type$makenewconst(p,c,t);
    }
;
    return (i64)1;
}

static void mm_type$tx_assign(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,i64 t) {
        i64 m;
        i64 mm;
        i64 needres;
        struct mm_decls$strec *  d;
    needres = (i64)(t != (i64)0);
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)15)) {
        if (((i64)(*b).tag == (i64)15)) {
            if (!!(needres)) {
                mm_support$txerror((byte*)"Mult assign has no result",0);
            }
;
            mm_type$tx_assignmultmult(p,a,b);
        }
        else {
            mm_type$tx_assignmultscalar(p,a,b,t);
        }
;
        return;
    }
    else if (($temp==(i64)43) || ($temp==(i64)44)) {
        mm_type$tx_dotindex(a,(*a).a,(*a).b,(i64)1);
        mm_type$tpass(b,(i64)(*a).mode,(i64)0);
        (*p).mode = (i64)3;
        return;
    }
    };
    if (((((i64)(*a).tag == (i64)3) && !!(msysc$m_getdotindex((i64)(*(*a).def).flags,(i64)4))) && !!((i64)(*p).initlet))) {
        mm_type$tpass(a,(i64)22,(i64)0);
    }
    else {
        mm_type$tpasslv(a,(i64)22);
    }
;
    m = (i64)(*a).mode;
    (*a).resultflag = needres;
    if ((((i64)mm_decls$ttbasetype[(m)] == (i64)11) && ((i64)(*b).tag == (i64)17 || (i64)(*b).tag == (i64)15))) {
        mm_type$tx_makeslice(b,(*b).a,m);
        (*p).mode = m;
    }
    else if ((!!((i64)mm_decls$ttisshort[(m)]) && !!(needres))) {
        (*p).memmode = m;
        (*p).mode = mm_lib$gettypebase(m);
        mm_type$tpass(b,(i64)(*p).mode,(i64)0);
    }
    else {
        if (((i64)(*b).pclop == (i64)5 || (i64)(*b).pclop == (i64)6)) {
            mm_type$tpass(b,(i64)22,(i64)0);
        }
        else if (((i64)(*b).tag == (i64)116)) {
            mm_type$tpass(b,m,(i64)0);
        }
        else {
            mm = m;
            if (!!((i64)mm_decls$ttisshort[(m)])) {
                mm = mm_lib$gettypebase(m);
            }
;
                        {i64 $temp = (i64)(*b).tag;
if (($temp==(i64)50)) {
                mm_type$tpass(b,mm,(i64)0);
            }
            else if (($temp==(i64)15)) {
                mm_type$tpass(b,m,(i64)0);
            }
            else {
                mm_type$tpass(b,(i64)22,(i64)0);
            }
            };
            if (((i64)mm_decls$ttbasetype[((i64)(*b).mode)] == (i64)27)) {
                d = mm_lib$getprocretmodes(b);
                mm_type$coerceunit(a,(i64)(*mm_decls$ttmult[((i64)(*d).mode)])[((i64)1)-1],(i64)0);
                (*p).mode = (i64)(*a).mode;
            }
            else {
                mm_type$coerceunit(b,mm,(i64)0);
                (*p).mode = mm;
            }
;
        }
;
    }
;
    mm_type$setsimple(p);
}

static void mm_type$tx_assignmultmult(struct mm_decls$unitrec *pp,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  q;
        struct mm_decls$unitrec *  lhs;
        struct mm_decls$unitrec *  rhs;
    (*pp).tag = (i64)24;
    if (((i64)(*a).length != (i64)(*b).length)) {
        mm_support$txerror((byte*)"Mult assign: count mismatch",0);
    }
;
    if (((i64)(*a).length == (i64)0)) {
        mm_support$txerror((byte*)"Invalid assignment",0);
    }
;
    rhs = (*b).a;
    lhs = (*a).a;
    p = lhs;
    L890 :;
    while (!!(p)) {
        mm_type$tpasslv(p,(i64)22);
L891 :;
        p = (*p).nextunit;
L893 :;
            }
L892 :;
    ;
    p = lhs;
    q = rhs;
    L894 :;
    while (!!(q)) {
        mm_type$tpass(q,(i64)(*p).mode,(i64)0);
L895 :;
        {
            p = (*p).nextunit;
            q = (*q).nextunit;
        }
L897 :;
            }
L896 :;
    ;
}

static void mm_type$tx_assignmultscalar(struct mm_decls$unitrec *pp,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b,i64 t) {
        struct mm_decls$unitrec *  p;
        struct mm_decls$unitrec *  alist;
        i64 nretmodes;
        i64 i;
        i64 alength;
        i32 (*pmult)[];
        struct mm_decls$strec *  d;
    alist = (*a).a;
    alength = (i64)(*a).length;
    nretmodes = (i64)0;
    (*pp).tag = (i64)25;
    mm_type$tpass(b,(i64)22,(i64)0);
        {i64 $temp = (i64)mm_decls$ttbasetype[((i64)(*b).mode)];
if (($temp==(i64)27)) {
        d = mm_lib$getprocretmodes(b);
        nretmodes = (i64)(*d).nretvalues;
        if (((i64)mm_decls$ttbasetype[((i64)(*d).mode)] != (i64)27)) {
            mm_support$txerror((byte*)"Not a tuple",0);
        }
;
        if ((alength > nretmodes)) {
            mm_support$txerror((byte*)"mult ass/mult returns don't agree in number",0);
        }
;
        if ((nretmodes <= (i64)1)) {
            mm_support$txerror((byte*)"mult ass rhs needs fn yielding 2+ values",0);
        }
;
        p = alist;
        pmult = mm_decls$ttmult[((i64)(*d).mode)];
        i = (i64)1;
        L898 :;
        while (!!(p)) {
            mm_type$tpasslv(p,(i64)(*pmult)[((i)++)-1]);
L899 :;
            p = (*p).nextunit;
L901 :;
                    }
L900 :;
        ;
    }
    else if (($temp==(i64)11)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"MULT:=SLICE",NULL);
        msysc$m_print_i64((i64)(*a).length,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        if ((alength != (i64)2)) {
            mm_support$txerror((byte*)"(a,b):=slice",0);
        }
;
        mm_type$tpasslv(alist,mm_lib$createrefmode(0,(i64)mm_decls$tttarget[((i64)(*b).mode)],(i64)0));
        mm_type$tpasslv((*alist).nextunit,(i64)3);
    }
    else if (($temp==(i64)9)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"MULT:=RANGE",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)8)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"MULT:=RECORD",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
    else {
        if ((((i64)(*b).tag == (i64)31) && ((i64)(*b).pclop == (i64)7))) {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"MULT:=DIVREM",NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            if ((alength != (i64)2)) {
                mm_support$txerror((byte*)"(a,b):=divrem",0);
            }
;
            mm_type$tpasslv(alist,(i64)3);
            mm_type$tpasslv((*alist).nextunit,(i64)3);
            (*pp).tag = (i64)26;
        }
        else {
            mm_support$txerror_s((byte*)"Can't expand to mult values:",mm_lib$strmode((i64)(*b).mode,(i64)1),0);
        }
;
    }
    };
    (*pp).mode = t;
}

static void mm_type$tpasslv(struct mm_decls$unitrec *p,i64 t) {
    mm_type$tpass(p,(i64)22,(i64)1);
    if (!((t == (i64)22 || t == (i64)0))) {
        if (!(!!(mm_type$comparemodes((i64)(*p).mode,t)))) {
            mm_support$txerror_ss((byte*)"PassLV type mismatch: #:=#",mm_lib$strmode((i64)(*p).mode,(i64)1),mm_lib$strmode2(t,(i64)1));
        }
;
    }
;
}

static i64 mm_type$dobinnumx(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
        i64 amode;
        i64 bmode;
        i64 cmode;
    amode = (i64)(*a).mode;
    bmode = (i64)(*b).mode;
    if (((amode >= (i64)1 && amode <= (i64)5) && (bmode >= (i64)1 && bmode <= (i64)5))) {
        (*p).mode = (cmode = (amode>bmode?amode:bmode));
        mm_type$coerceunit(a,cmode,(i64)0);
        mm_type$coerceunit(b,cmode,(i64)0);
        return (i64)1;
    }
;
    return (i64)0;
}

static i64 mm_type$dobinnumf(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
        i64 amode;
        i64 bmode;
        i64 cmode;
    amode = (i64)(*a).mode;
    bmode = (i64)(*b).mode;
    if (((amode == (i64)5 || amode == (i64)4) && (bmode == (i64)5 || bmode == (i64)4))) {
        (*p).mode = (cmode = (amode>bmode?amode:bmode));
        mm_type$coerceunit(a,cmode,(i64)0);
        mm_type$coerceunit(b,cmode,(i64)0);
        return (i64)1;
    }
;
    return (i64)0;
}

static i64 mm_type$dobinnumi(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
        i64 amode;
        i64 bmode;
        i64 cmode;
    amode = (i64)(*a).mode;
    bmode = (i64)(*b).mode;
    if (((amode == (i64)3 || amode == (i64)2 || amode == (i64)1) && (bmode == (i64)3 || bmode == (i64)2 || bmode == (i64)1))) {
        (*p).mode = (cmode = (amode>bmode?amode:bmode));
        mm_type$coerceunit(a,cmode,(i64)0);
        mm_type$coerceunit(b,cmode,(i64)0);
        return (i64)1;
    }
;
    return (i64)0;
}

static i64 mm_type$doin(struct mm_decls$unitrec *p,struct mm_decls$unitrec *a,struct mm_decls$unitrec *b) {
        i64 simpleset;
        struct mm_decls$unitrec *  q;
    simpleset = (i64)1;
    if (((i64)(*b).tag == (i64)17)) {
        q = (*b).a;
        L902 :;
        while (!!(q)) {
            if (!(!!((i64)mm_decls$ttisinteger[((i64)(*q).mode)]))) {
                simpleset = (i64)0;
                goto L904 ;
            }
;
L903 :;
            q = (*q).nextunit;
L905 :;
                    }
L904 :;
        ;
    }
;
    if (((((i64)(*a).mode >= (i64)1 && (i64)(*a).mode <= (i64)5) && ((i64)(*b).tag == (i64)16 || (i64)(*b).tag == (i64)17)) && !!(simpleset))) {
        (*p).tag = (((i64)(*b).tag == (i64)16) ? (i64)37 : (i64)38);
    }
    else {
        mm_support$txerror((byte*)"doin",0);
    }
;
    (*p).mode = (i64)6;
    if (((i64)(*p).pclop == (i64)14)) {
        mm_type$addnotl(p);
    }
;
    return (i64)1;
}

static void mm_type$setsimple(struct mm_decls$unitrec *p) {
        struct mm_decls$unitrec *  a;
        i64 $av_1;
        i64 i;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)28) || ($temp==(i64)86)) {
        return;
    }
    else if (($temp==(i64)31) || ($temp==(i64)32)) {
        if (!!((i64)mm_tables$complexopset[((i64)(*p).pclop)])) {
            return;
        }
;
    }
    };
        ($av_1 = (i64)mm_tables$jsubs[((i64)(*p).tag)]);
    for (i=(i64)1;i<=$av_1;++i) {
L906 :;
        a = (*p).abc[(i)-1];
        if ((!!(a) && !(!!((i64)(*a).simple)))) {
            return;
        }
;
L907 :;
    }
L908 :;
    ;
    (*p).simple = (i64)1;
}

// START
void mm_type$start(void) {

}

void mm_winc$codegen(void) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"CODEGEN/CLANG",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mm_support$loaderror((byte*)"CODEGEN_CLANG NOT READY",(byte*)"",(byte*)"");
}

i64 mm_winc$runlibfile(u8 *filename) {
    mm_support$loaderror((byte*)"RUNLIBFILE(C)",(byte*)"",(byte*)"");
    return (i64)1;
}

i64 mm_winc$writeexefile(u8 *exename,i64 gendll) {
    mm_winc$writeasmfile(mm_decls$asmfilename);
    if (!!(mlinux$os_iswindows())) {
        mm_winc$do_link_win(mm_decls$asmfilename,exename,(byte*)"exe",mm_decls$ccompiler,(i64)mm_decls$foptim);
    }
    else {
        mm_winc$do_link_lin(mm_decls$asmfilename,exename,(byte*)"exe",mm_decls$ccompiler,(i64)mm_decls$foptim);
    }
;
    return (i64)1;
}

i64 mm_winc$writelibfile(u8 *filename) {
    mm_support$loaderror((byte*)"WRITELIBFILE(C)",(byte*)"",(byte*)"");
    return (i64)1;
}

i64 mm_winc$writeasmfile(u8 *filename) {
    mc_genc$codegen_clang();
    mm_lib$writegsfile(filename,(struct mlib$strbuffer *)mm_lib$dest);
    mlib$gs_free((struct mlib$strbuffer *)mm_lib$dest);
    return (i64)1;
}

void mm_winc$do_link_win(u8 *cfile,u8 *exefile,u8 *linkoption,i64 ccompiler,i64 optimise) {
        u8 str[256];
        u8 str2[256];
        i64 status;
        i64 doobj;
    doobj = mlib$eqstring(linkoption,(byte*)"obj");
    if ((ccompiler==(i64)1)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"gcc -m64 # # -o# # -s ");
        msysc$m_print_str((!!(doobj) ? (byte*)"-c" : (byte*)""),NULL);
        msysc$m_print_str((!!(optimise) ? (byte*)"-O3" : (byte*)""),NULL);
        msysc$m_print_str(exefile,NULL);
        msysc$m_print_str(cfile,NULL);
        msysc$m_print_end();
        ;
    }
    else if ((ccompiler==(i64)2)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"tcc # -o# # # -luser32 c:\\windows\\system32\\kernel32.dll -fdollars-in-identifiers");
        msysc$m_print_str((!!(doobj) ? (byte*)"-c" : (byte*)""),NULL);
        msysc$m_print_str(exefile,NULL);
        msysc$m_print_str(cfile,NULL);
        msysc$m_print_str((!!(doobj) ? (byte*)"" : (byte*)"c:\\windows\\system32\\user32.dll"),NULL);
        msysc$m_print_end();
        ;
    }
    else if ((ccompiler==(i64)3)) {
        strcpy(str2,cfile);
        str2[((strlen(cfile) - (i64)1))-1] = (u64)0u;
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"call tc #");
        msysc$m_print_str(str2,NULL);
        msysc$m_print_end();
        ;
    }
    else if ((ccompiler==(i64)4)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"bcc # -out:# #");
        msysc$m_print_str((!!(doobj) ? (byte*)"-c" : (byte*)""),NULL);
        msysc$m_print_str(exefile,NULL);
        msysc$m_print_str(cfile,NULL);
        msysc$m_print_end();
        ;
    }
;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"W:Invoking C compiler:",NULL);
    msysc$m_print_str(str,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    status = mlinux$os_execwait((u8 *)str,(i64)0,0);
    if ((status == (i64)0)) {
        return;
    }
;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"M compiler: couldn't compile intermediate C",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    exit((i64)0);
}

void mm_winc$do_link_lin(u8 *cfile,u8 *exefile,u8 *linkoption,i64 ccompiler,i64 optimise) {
        u8 str[256];
        u8 newexefile[300];
        i64 status;
        i64 doobj;
    doobj = mlib$eqstring(linkoption,(byte*)"obj");
    strcpy(newexefile,mlib$changeext(exefile,(byte*)""));
    if ((ccompiler==(i64)1)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"gcc -m64 # # -o# # -lm -ldl -s -fno-builtin");
        msysc$m_print_str((!!(doobj) ? (byte*)"-c" : (byte*)""),NULL);
        msysc$m_print_str((!!(optimise) ? (byte*)"-O3" : (byte*)""),NULL);
        msysc$m_print_str(newexefile,NULL);
        msysc$m_print_str(cfile,NULL);
        msysc$m_print_end();
        ;
    }
    else if ((ccompiler==(i64)2)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"tcc # -o# # -lm -ldl -fdollars-in-identifiers");
        msysc$m_print_str((!!(doobj) ? (byte*)"-c" : (byte*)""),NULL);
        msysc$m_print_str(newexefile,NULL);
        msysc$m_print_str(cfile,NULL);
        msysc$m_print_end();
        ;
    }
    else {
        mlib$abortprogram((byte*)"Not available on Linux");
    }
;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"L:Invoking C compiler:",NULL);
    msysc$m_print_str(str,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    status = mlinux$os_execwait((u8 *)str,(i64)0,0);
    if ((status == (i64)0)) {
        return;
    }
;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"M compiler: couldn't compile intermediate C",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    exit((i64)0);
}

// START
void mm_winc$start(void) {

}

// START
void mc_decls$start(void) {

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
L909 :;
        msysc$sysparams[(i)-1] = (*args)[(i)-1];
L910 :;
    }
L911 :;
    ;
    msysc$ncmdparams = (msysc$nsysparams - (msysc$$cmdskip + (i64)1));
    msysc$cmdparams = (u8 *(*)[])&msysc$sysparams[((msysc$$cmdskip + (i64)1))-1];
    j = (i64)1;
    msysc$nenvstrings = (i64)0;
    L912 :;
    while (!!((*envstrings)[(j)-1])) {
        ++(msysc$nenvstrings);
        ++(j);
L913 :;
    }
L914 :;
    ;
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
    L915 :;
    while (1) {
        c = (u64)(*msysc$fmtstr);
        switch ((i64)(u64)c) {
        case 35:;
            {
                if (!!(lastx)) {
                    goto L917 ;
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
L917 :;
;
            ++(n);
            ++(msysc$fmtstr);
        }
        } //SW
;
    }
L916 :;
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
    L918 :;
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
                goto L921 ;
;
            }
            break;
        default: {
            if ((((u64)c >= '0') && ((u64)c <= '9'))) {
                n = (i64)((u64)c - '0');
                L922 :;
                while (1) {
                    c = (u64)(*s);
                    if (((i64)(u64)(*s) == (i64)0)) {
                        goto L923 ;
                    }
;
                    if ((((u64)c >= '0') && ((u64)c <= '9'))) {
                        ++(s);
                        n = (((n * (i64)10) + (i64)(u64)c) - (i64)48);
                    }
                    else {
                        goto L923 ;
                    }
;
                }
L923 :;
                ;
                //gotwidth:
L921 :;
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
L919 :;
    }
L920 :;
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
L924 :;
        if (((i64)(u64)(*p) == (i64)0)) {
            goto L926 ;
        }
;
        (*q) = (u64)(*p);
        ++(q);
        ++(p);
L925 :;
    }
L926 :;
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
L927 :;
            (*t) = (u64)(*fmt).padchar;
            ++(t);
L928 :;
        }
L929 :;
        ;
        (*t) = (u64)0u;
    }
    else if (((u64)(*fmt).justify == 'R')) {
        if (((((u64)(*fmt).padchar == '0') && !!((i64)(*fmt).base)) && (((u64)(*s) == '-') || ((u64)(*s) == '+')))) {
            (*t) = (u64)(*s);
            ++(t);
            $av_2 = (w - n);
            while ($av_2-- > 0) {
L930 :;
                (*t) = (u64)(*fmt).padchar;
                ++(t);
L931 :;
            }
L932 :;
            ;
            strncpy(t,(s + (i64)1),(u64)(n - (i64)1));
            (*((t + n) - (i64)1)) = (u64)0u;
        }
        else {
            $av_3 = (w - n);
            while ($av_3-- > 0) {
L933 :;
                (*t) = (u64)(*fmt).padchar;
                ++(t);
L934 :;
            }
L935 :;
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
L936 :;
            (*t) = (u64)(*fmt).padchar;
            ++(t);
L937 :;
        }
L938 :;
        ;
        strncpy(t,s,(u64)n);
        t += n;
        $av_5 = ((w - n) - m);
        while ($av_5-- > 0) {
L939 :;
            (*t) = (u64)(*fmt).padchar;
            ++(t);
L940 :;
        }
L941 :;
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
    L942 :;
    do {
        t[(++(i))] = (u64)msysc$digits[((i64)(aa % base))];
        aa = (aa / base);
        ++(k);
        if (((!!(sep) && ((i64)aa != (i64)0)) && (k == g))) {
            t[(++(i))] = (u64)sep;
            k = (i64)0;
        }
;
L943 :;
    }
    while (!((i64)aa == (i64)0));
L944 :;
    ;
    j = i;
    s0 = s;
    L945 :;
    while (!!(i)) {
        (*s) = (u64)t[((i)--)];
        ++(s);
L946 :;
    }
L947 :;
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
    L948 :;
    while (!!(i)) {
        --(s);
        (*s) = (u64)t[(((i)-- - (i64)1))];
        if (((!!(sep) && !!(i)) && (++(k) == g))) {
            --(s);
            (*s) = (u64)sep;
            k = (i64)0;
        }
;
L949 :;
    }
L950 :;
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
    L951 :;
    while ((((u64)(*s) == ' ') || ((i64)(u64)(*s) == (i64)9))) {
        ++(s);
L952 :;
    }
L953 :;
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
    L954 :;
    while (!!((u64)(*s))) {
        c = (u64)(*(s)++);
        switch ((i64)(u64)c) {
        case 32:;
        case 9:;
        case 44:;
        case 61:;
            {
                if ((!!((u64)quotechar) || (p == s))) {
                    goto L957 ;
;
                }
;
                msysc$termchar = (i64)(u64)c;
                goto L956 ;
            }
            break;
        default: {
            //normalchar:
L957 :;
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
                    goto L956 ;
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
L955 :;
    }
L956 :;
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
    L958 :;
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
                goto L959 ;
            }
            break;
        default: {
            msysc$itemerror = (i64)1;
            goto L960 ;
        }
        } //SW
;
        if (((i64)(u64)d >= base)) {
            msysc$itemerror = (i64)1;
            goto L960 ;
        }
;
        aa = (u64)(((i64)aa * base) + (i64)(u64)d);
L959 :;
    }
L960 :;
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
L961 :;
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L962 :;
    }
L963 :;
    ;
}

static void msysc$iconvucn(u8 *s,i64 n) {
        i64 $av_1;
    $av_1 = n;
    while ($av_1-- > 0) {
L964 :;
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L965 :;
    }
L966 :;
    ;
}

static void msysc$convlcstring(u8 *s) {
    L967 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L968 :;
    }
L969 :;
    ;
}

static void msysc$convucstring(u8 *s) {
    L970 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L971 :;
    }
L972 :;
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
    mlinux$start();
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
        mlib$bigmemtotal += mlib$allocbytes;
        if ((u64)0u) {
            mlib$addtomemalloc((i32 *)p,mlib$allocbytes);
        }
;
        return p;
    }
;
    mlib$alloccode = (i64)mlib$sizeindextable[(n)];
    mlib$allocbytes = (i64)mlib$allocupper[(mlib$alloccode)];
    mlib$smallmemtotal += mlib$allocbytes;
    if (!!((p = (byte *)mlib$freelist[(mlib$alloccode)]))) {
        if ((u64)0u) {
            mlib$addtomemalloc((i32 *)p,mlib$allocbytes);
        }
;
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
    if ((u64)0u) {
        mlib$addtomemalloc((i32 *)p,mlib$allocbytes);
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
        if ((u64)0u) {
            mlib$removefrommemalloc((i32 *)p,n);
        }
;
        mlib$bigmemtotal -= n;
        free(p);
        return;
    }
;
    if (!!(p)) {
        acode = (i64)mlib$sizeindextable[(n)];
        mlib$smallmemtotal -= (i64)mlib$allocupper[(acode)];
        if ((u64)0u) {
            mlib$removefrommemalloc((i32 *)p,(i64)mlib$allocupper[(acode)]);
        }
;
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
L973 :;
        j = (i64)1;
        k = (i64)16;
        L976 :;
        while ((i > k)) {
            k = (k << (i64)1);
            ++(j);
L977 :;
        }
L978 :;
        ;
        mlib$sizeindextable[(i)] = j;
L974 :;
    }
L975 :;
    ;
    mlib$allocupper[((i64)1)] = (u64)16u;
    size = (i64)16;
    for (i=(i64)2;i<=(i64)27;++i) {
L979 :;
        size *= (i64)2;
        mlib$allocupper[(i)] = (u64)size;
        if ((size >= (i64)33554432)) {
            k = i;
            goto L981 ;
        }
;
L980 :;
    }
L981 :;
    ;
    for (i=(k + (i64)1);i<=(i64)300;++i) {
L982 :;
        size += (i64)33554432;
        if ((size < (i64)8589934592)) {
            mlib$allocupper[(i)] = (u64)size;
            mlib$maxmemory = (u64)size;
        }
        else {
            mlib$maxalloccode = (i - (i64)1);
            goto L984 ;
        }
;
L983 :;
    }
L984 :;
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

static void mlib$addtomemalloc(i32 *ptr,i64 size) {
        i64 allocated;
        i64 code;
        i64 i;
    for (i=(i64)1;i<=(i64)2;++i) {
L985 :;
        if ((mlib$memalloctable[(i)-1] == ptr)) {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"ALLOC ERROR:",NULL);
            msysc$m_print_ptr(ptr,NULL);
            msysc$m_print_str((byte*)"ALREADY ALLOCATED\n\n\n",NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            exit((i64)2);
        }
;
        if ((mlib$memalloctable[(i)-1] == 0)) {
            mlib$memalloctable[(i)-1] = ptr;
            code = mlib$pcm_getac(size);
            allocated = (i64)mlib$allocupper[(code)];
            mlib$memallocsize[(i)-1] = allocated;
            return;
        }
;
L986 :;
    }
L987 :;
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"MEMALLOCTABLE FULL\n\n\n\n",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mlinux$os_getch();
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    exit((i64)3);
}

static void mlib$removefrommemalloc(i32 *ptr,i64 size) {
        i64 allocated;
        i64 code;
        i64 i;
    code = mlib$pcm_getac(size);
    allocated = (i64)mlib$allocupper[(code)];
    for (i=(i64)1;i<=(i64)2;++i) {
L988 :;
        if ((mlib$memalloctable[(i)-1] == ptr)) {
            if (((i64)mlib$memallocsize[(i)-1] != allocated)) {
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"REMOVE:FOUND",NULL);
                msysc$m_print_ptr(ptr,NULL);
                msysc$m_print_str((byte*)"IN MEMALLOCTABLE, ROUNDED FREESIZE=",NULL);
                msysc$m_print_i64(allocated,NULL);
                msysc$m_print_str((byte*)", BUT STORED AS BLOCK SIZE:",NULL);
                msysc$m_print_i64((i64)mlib$memallocsize[(i)-1],NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                mlib$abortprogram((byte*)"MEMSIZE");
            }
;
            mlib$memalloctable[(i)-1] = 0;
            return;
        }
;
L989 :;
    }
L990 :;
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"CAN'T FIND",NULL);
    msysc$m_print_ptr(ptr,NULL);
    msysc$m_print_str((byte*)"IN MEMALLOCTABLE",NULL);
    msysc$m_print_i64(size,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mlinux$os_getch();
    mlib$abortprogram((byte*)"MEM");
    exit((i64)4);
}

void *mlib$allocmem(i64 n) {
        void *  p;
    p = malloc((u64)n);
    if (!!(p)) {
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
        handlex = mlinux$os_getstdin();
    }
;
    if ((handlex == 0)) {
        n = (i64)0;
        p = buffer;
        L991 :;
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
L992 :;
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
    L993 :;
    while (((p >= buffer) && (((i64)(u64)(*p) == (i64)13) || ((i64)(u64)(*p) == (i64)10)))) {
        if ((((i64)(u64)(*p) == (i64)13) || ((i64)(u64)(*p) == (i64)10))) {
            crseen = (i64)1;
        }
;
        (*(p)--) = (u64)0u;
L994 :;
    }
L995 :;
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
L996 :;
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L997 :;
    }
L998 :;
    ;
}

void mlib$iconvucn(u8 *s,i64 n) {
        i64 $av_1;
    $av_1 = n;
    while ($av_1-- > 0) {
L999 :;
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L1000 :;
    }
L1001 :;
    ;
}

u8 *mlib$convlcstring(u8 *s) {
        u8 *  s0;
    s0 = s;
    L1002 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L1003 :;
    }
L1004 :;
    ;
    return s0;
}

u8 *mlib$convucstring(u8 *s) {
        u8 *  s0;
    s0 = s;
    L1005 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L1006 :;
    }
L1007 :;
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
    L1008 :;
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
L1009 :;
    }
L1010 :;
    ;
    return (byte*)"";
}

u8 *mlib$extractpath(u8 *s) {
        static u8 str[260];
        u8 *  t;
        i64 n;
    t = ((s + strlen(s)) - (i64)1);
    L1011 :;
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
L1012 :;
    }
L1013 :;
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
    if ((u64)0u) {
        mlib$removefrommemalloc((i32 *)p,(i64)32);
    }
;
    (*(u64 *)p) = (u64)(i64)mlib$freelist[((i64)2)];
    mlib$freelist[((i64)2)] = (u64 *)p;
}

void *mlib$pcm_alloc64(void) {
        byte *  p;
    mlib$allocbytes = (i64)64;
    mlib$smallmemtotal += (i64)64;
    if (!!((p = (byte *)mlib$freelist[((i64)3)]))) {
        mlib$freelist[((i64)3)] = (u64 *)(i64)(*mlib$freelist[((i64)3)]);
        return p;
    }
;
    return mlib$pcm_alloc((i64)64);
}

void mlib$pcm_free64(void *p) {
    mlib$smallmemtotal -= (i64)64;
    if ((u64)0u) {
        mlib$removefrommemalloc((i32 *)p,(i64)64);
    }
;
    (*(u64 *)p) = (u64)(i64)mlib$freelist[((i64)3)];
    mlib$freelist[((i64)3)] = (u64 *)p;
}

void *mlib$pcm_alloc16(void) {
        byte *  p;
    mlib$allocbytes = (i64)16;
    mlib$smallmemtotal += (i64)16;
    if (!!((p = (byte *)mlib$freelist[((i64)1)]))) {
        mlib$freelist[((i64)1)] = (u64 *)(i64)(*mlib$freelist[((i64)1)]);
        return p;
    }
;
    return mlib$pcm_alloc((i64)16);
}

void mlib$pcm_free16(void *p) {
    mlib$smallmemtotal -= (i64)16;
    if ((u64)0u) {
        mlib$removefrommemalloc((i32 *)p,(i64)32);
    }
;
    (*(u64 *)p) = (u64)(i64)mlib$freelist[((i64)1)];
    mlib$freelist[((i64)1)] = (u64 *)p;
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
L1014 :;
            str[((slen + i))-1] = (u64)padch;
L1015 :;
        }
L1016 :;
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
L1017 :;
        str[(i)-1] = (u64)ch;
L1018 :;
    }
L1019 :;
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
        i64 length;
        static u8 str[300];
    //reenter:
L1020 :;
;
    (*value) = 0;
    (*name) = 0;
    if (!!(infile)) {
        if ((mlib$readnextfileitem(&fileptr,&item) == (i64)0)) {
            free((void *)filestart);
            infile = (i64)0;
            goto L1020 ;
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
        length = strlen(item);
        if (((u64)(*item) == '@')) {
            infile = (i64)1;
            goto L1020 ;
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
L1021 :;
;
    L1022 :;
    while (1) {
                {u64 $temp = (u64)(*p);
if (($temp==' ') || ($temp==(u64)9u) || ($temp==(u64)13u) || ($temp==(u64)10u)) {
            ++(p);
        }
        else if (($temp==(u64)26u) || ($temp==(u64)0u)) {
            return (i64)0;
        }
        else {
            goto L1023 ;
        }
        };
    }
L1023 :;
    ;
        {u64 $temp = (u64)(*p);
if (($temp=='!') || ($temp=='#')) {
        ++(p);
        L1024 :;
                {u64 $temp = (u64)(*(p)++);
if (($temp==(u64)10u)) {
            goto L1021 ;
;
        }
        else if (($temp==(u64)26u) || ($temp==(u64)0u)) {
            (*fileptr) = (p - (i64)1);
            return (i64)0;
        }
        else {
        }
        }goto L1024 ;
L1025 :;
        ;
    }
    };
        {u64 $temp = (u64)(*p);
if (($temp=='"')) {
        pstart = ++(p);
        L1026 :;
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
                goto L1027 ;
            }
            };
            ++(p);
        }
L1027 :;
        ;
    }
    else {
        pstart = p;
        L1028 :;
        while (1) {
                        {u64 $temp = (u64)(*p);
if (($temp==(u64)0u) || ($temp==(u64)26u)) {
                pend = p;
                goto L1029 ;
            }
            else if (($temp==' ') || ($temp==(u64)9u) || ($temp==',') || ($temp==(u64)13u) || ($temp==(u64)10u)) {
                pend = (p)++;
                goto L1029 ;
            }
            };
            ++(p);
        }
L1029 :;
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
L1030 :;
        strcat(s,padchar);
L1031 :;
    }
L1032 :;
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
    L1033 :;
    do {
        x = ((r64)mlib$mrandomp() / (double)9223372036854775800.);
L1034 :;
    }
    while (!(x != (double)1.));
L1035 :;
    ;
    return x;
}

r64 mlib$mrandomreal1(void) {
    return (r64)(mlib$mrandomp() / (i64)9223372036854775807);
}

byte *mlib$checkpackfile(void) {
        i64 a;
        i64 offset;
        u8 exefile[300];
        byte *  packexeptr;
        i64 packexesize;
        u8 *  packfilename;
        i64 packfilesize;
        byte *  packfileptr;
    strcpy((u8 *)&exefile[((i64)1)-1],mlinux$os_gethostname());
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Attempting to open",NULL);
    msysc$m_print_str(exefile,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    packexeptr = mlib$readfile((u8 *)&exefile[((i64)1)-1]);
    if (!(!!(packexeptr))) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Can't open",NULL);
        msysc$m_print_str(exefile,NULL);
        msysc$m_print_ptr(packexeptr,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        exit(0);
    }
;
    packexesize = mlib$rfsize;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"File read OK. Size",NULL);
    msysc$m_print_i64(packexesize,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    a = (i64)(*(i32 *)(packexeptr + (packexesize - (i64)4)));
    if ((a != (i64)1262568272)) {
        free(packexeptr);
        packfileptr = 0;
        return 0;
    }
;
    offset = (i64)(*(i32 *)(packexeptr + (packexesize - (i64)8)));
    packfilename = (u8 *)(packexeptr + offset);
    offset += (strlen(packfilename) + (i64)1);
    packfilesize = (i64)(*(i32 *)(packexeptr + offset));
    packfileptr = ((packexeptr + offset) + (i64)4);
    return packfileptr;
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
L1036 :;
        if (!!(mlib$eqstring(msysc$m_get_procname(i),name))) {
            return msysc$m_get_procaddr(i);
        }
;
L1037 :;
    }
L1038 :;
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

void mlinux$os_init(void) {
    mlinux$init_flag = (i64)1;
}

i64 mlinux$os_execwait(u8 *cmdline,i64 newconsole,u8 *workdir) {
    return system(cmdline);
}

i64 mlinux$os_execcmd(u8 *cmdline,i64 newconsole) {
    return system(cmdline);
}

i64 mlinux$os_getch(void) {
        struct mlinux$termios old;
        struct mlinux$termios new;
        u8 ch;
    tcgetattr((i32)(i64)0,&old);
    new = old;
    new.c_lflag &= (i32)(i64)-3;
    new.c_lflag &= (i32)(i64)-9;
    tcsetattr((i32)(i64)0,(i32)(i64)0,(struct mlinux$termios *)&new);
    ch = (u64)getchar();
    tcsetattr((i32)(i64)0,(i32)(i64)0,(struct mlinux$termios *)&old);
    return (i64)(u64)ch;
}

i64 mlinux$os_kbhit(void) {
    mlib$abortprogram((byte*)"kbhit");
    return (i64)0;
}

void mlinux$os_flushkeys(void) {
    mlib$abortprogram((byte*)"flushkeys");
}

void *mlinux$os_getconsolein(void) {
    return 0;
}

void *mlinux$os_getconsoleout(void) {
    return 0;
}

void *mlinux$os_proginstance(void) {
    mlib$abortprogram((byte*)"PROGINST");
    return 0;
}

u64 mlinux$os_getdllinst(u8 *name) {
        void *  h;
    h = dlopen(name,(i32)(i64)1);
    if ((h == 0)) {
        if ((strcmp(name,(byte*)"msvcrt") == (i64)0)) {
            h = dlopen((byte*)"libc.so.6",(i32)(i64)1);
        }
;
    }
;
    return (u64)h;
}

void *mlinux$os_getdllprocaddr(i64 hlib,u8 *name) {
        void *  fnaddr;
    if ((hlib == (i64)0)) {
        return 0;
    }
;
    fnaddr = dlsym((void *)hlib,name);
    return fnaddr;
}

void mlinux$os_initwindows(void) {
}

i64 mlinux$os_getchx(void) {
    mlib$abortprogram((byte*)"getchx");
    return (i64)0;
}

u8 *mlinux$os_getos(void) {
    if (((i64)64 == (i64)32)) {
        return (byte*)"L32";
    }
    else {
        return (byte*)"L64";
    }
;
}

i64 mlinux$os_gethostsize(void) {
    return (i64)64;
}

i64 mlinux$os_iswindows(void) {
    return (i64)0;
}

i64 mlinux$os_shellexec(u8 *opc,u8 *file) {
    mlib$abortprogram((byte*)"SHELL EXEC");
    return (i64)0;
}

void mlinux$os_sleep(i64 a) {
    sleep((u32)a);
}

void *mlinux$os_getstdin(void) {
    return 0;
}

void *mlinux$os_getstdout(void) {
    return 0;
}

u8 *mlinux$os_gethostname(void) {
    return (byte*)"";
}

u8 *mlinux$os_getmpath(void) {
    return (byte*)"";
}

void mlinux$os_exitprocess(i64 x) {
    exit(0);
}

i64 mlinux$os_clock(void) {
    if (!!(mlinux$os_iswindows())) {
        return clock();
    }
    else {
        return (clock() / (i64)1000);
    }
;
}

i64 mlinux$os_ticks(void) {
    return clock();
}

i64 mlinux$os_getclockspersec(void) {
    if (!!(mlinux$os_iswindows())) {
        return (i64)1000;
    }
    else {
        return (i64)1000000;
    }
;
}

void mlinux$os_setmesshandler(void *addr) {
    mlib$abortprogram((byte*)"SETMESSHANDLER");
}

i64 mlinux$os_hpcounter(void) {
    return (i64)1;
}

i64 mlinux$os_hpfrequency(void) {
    return (i64)1;
}

i64 mlinux$os_filelastwritetime(u8 *filename) {
    return (i64)0;
}

void mlinux$os_getsystime(struct mlinux$rsystemtime *tm) {
    memset(tm,(i32)(i64)0,(u64)36u);
    (*tm).month = (i64)1;
}

void mlinux$os_peek(void) {
}

byte *mlinux$os_allocexecmem(i64 n) {
    mlib$abortprogram((byte*)"No allocexec");
    return (byte *)0;
}

// START
void mlinux$start(void) {

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
            return ((*(i64 (*)(i64))fnaddr))((*params)[((i64)1)-1]);
        }
        break;
    case 2:;
        {
            return ((*(i64 (*)(i64,i64))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1]);
        }
        break;
    case 3:;
        {
            return ((*(i64 (*)(i64,i64,i64))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1]);
        }
        break;
    case 4:;
        {
            return ((*(i64 (*)(i64,i64,i64,i64))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1]);
        }
        break;
    case 5:;
        {
            return ((*(i64 (*)(i64,i64,i64,i64,i64))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1]);
        }
        break;
    case 6:;
        {
            return ((*(i64 (*)(i64,i64,i64,i64,i64,i64))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1]);
        }
        break;
    case 9:;
        {
            return ((*(i64 (*)(i64,i64,i64,i64,i64,i64,i64,i64,i64))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1],(*params)[((i64)7)-1],(*params)[((i64)8)-1],(*params)[((i64)9)-1]);
        }
        break;
    case 10:;
        {
            return ((*(i64 (*)(i64,i64,i64,i64,i64,i64,i64,i64,i64,i64))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1],(*params)[((i64)7)-1],(*params)[((i64)8)-1],(*params)[((i64)9)-1],(*params)[((i64)10)-1]);
        }
        break;
    case 11:;
        {
            return ((*(i64 (*)(i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1],(*params)[((i64)7)-1],(*params)[((i64)8)-1],(*params)[((i64)9)-1],(*params)[((i64)10)-1],(*params)[((i64)11)-1]);
        }
        break;
    case 12:;
        {
            return ((*(i64 (*)(i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1],(*params)[((i64)7)-1],(*params)[((i64)8)-1],(*params)[((i64)9)-1],(*params)[((i64)10)-1],(*params)[((i64)11)-1],(*params)[((i64)12)-1]);
        }
        break;
    case 14:;
        {
            return ((*(i64 (*)(i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1],(*params)[((i64)7)-1],(*params)[((i64)8)-1],(*params)[((i64)9)-1],(*params)[((i64)10)-1],(*params)[((i64)11)-1],(*params)[((i64)12)-1],(*params)[((i64)13)-1],(*params)[((i64)14)-1]);
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
            x = ((*(r64 (*)(i64))fnaddr))((*params)[((i64)1)-1]);
        }
        break;
    case 2:;
        {
            x = ((*(r64 (*)(i64,i64))fnaddr))((*params)[((i64)1)-1],(*params)[((i64)2)-1]);
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

