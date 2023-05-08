// Build on Linux using:
//
// gcc -O3 pcilin.c -opci -fno-builtin -ldl -lm
//


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
struct msysc$procinforec;
struct msysc$fmtrec;
struct mlib$strbuffer;
struct mlinux$termios;
struct mlinux$timeval;
struct mlinux$tm_rec;
struct mlinux$rsystemtime;
struct pci_decls$lexrec;
struct pci_decls$attrrec;
struct pci_decls$opndrec;
struct pci_decls$strec;
struct pci_decls$pclrec;
struct pci_decls$funcdescr;
struct pci_decls$datarec;
struct pci_decls$labelrec;

/* Struct Definitions */
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

struct mlinux$timeval {
    i64 tv_sec;
    i64 tv_usec;
};

struct mlinux$tm_rec {
    i32 tm_sec;
    i32 tm_min;
    i32 tm_hour;
    i32 tm_mday;
    i32 tm_mon;
    i32 tm_year;
    i32 tm_wday;
    i32 tm_yday;
    i32 tm_isdst;
    byte padding[20];
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

struct pci_decls$lexrec {
    i64 symbol;
    i64 value;
    r64 xvalue;
    u8 *  svalue;
    struct pci_decls$strec *  symptr;
    i64 lineno;
    i64 opcode;
    i64 mode;
};

struct pci_decls$attrrec {
    union {
        struct {
            i32 x;
            i32 y;
        };
        struct {
            u16 nlocals;
            u16 nparams;
            u16 nretvalues;
        };
        struct {
            i32 swmin;
            i32 swmax;
        };
        struct {
            i32 nargs;
            i32 nvars;
        };
        struct {
            u32 scale;
            i32 poffset;
        };
        i64 step;
        byte popone;
    };
};

struct pci_decls$opndrec {
    union {
        struct pci_decls$strec *  def;
        i64 value;
        r64 xvalue;
        r32 xvalue32;
        u8 *  svalue;
        u32 memsize;
        i64 importno;
        i64 labelno;
        i64 frameoffset;
        byte *  staticaddr;
        i64 pcindex;
    };
};

struct pci_decls$strec {
    u8 *  name;
    byte namelen;
    byte opcode;
    byte typecode;
    byte flags;
    byte idtype;
    byte nparams;
    u16 nlocals;
    union {
        i64 pcindex;
        byte *  staticaddr;
        i64 frameoffset;
        i64 importno;
        i64 libindex;
    };
    byte nretvalues;
    union {
        byte mode;
        byte retmode[3];
    };
    u32 memsize;
};

struct pci_decls$pclrec {
    byte opcode;
    byte mode;
    byte mode2;
    byte opndtype;
    u32 memsize;
    u16 jcode;
    byte spare[6];
    struct pci_decls$opndrec a;
    struct pci_decls$attrrec x;
};

struct pci_decls$funcdescr {
    struct pci_decls$strec *  def;
    union {
        void (*addr)(void);
        i64 pcindex;
    };
    i32 functype;
    i32 cbindex;
};

struct pci_decls$datarec {
    struct pci_decls$strec **  pdef;
    struct pci_decls$datarec* nextdata;
};

struct pci_decls$labelrec {
    i64 *  plabel;
    struct pci_decls$labelrec* nextlabel;
};


/* PROCDECLS */
void pci_decls$start(void);
i64 pci_execc$exec(i64 pcstart,i64 spstart);
void pci_execc$runpcl(void);
static r32 pci_execc$getr32(i64 a);
static i64 pci_execc$putr32(r32 x);
void pci_execc$start(void);
void pci_fixup$fixuppcl(void);
static void pci_fixup$addpclop(i64 k,i64 j);
static void pci_fixup$convertpcl(i64 index);
static i64 pci_fixup$findjcode(struct pci_decls$pclrec *p);
void pci_fixup$start(void);
void pci_lex$startlex(i64 lineno);
void pci_lex$startlextest(u8 *s);
void pci_lex$lex(void);
void pci_lex$start(void);
static void pci_lex$lxerror(u8 *mess);
void pci_lex$printlx(void);
static void pci_lex$readdec(i64 neg);
static void pci_lex$readhex(void);
static void pci_lex$readreal(i64 neg);
static void pci_lex$readstring(i64 termchar);
static void pci_lex$inithashtable(void);
static void pci_lex$adddlllib(u8 *name);
void pci_lex$printhashtable(void);
void pci_lex$addreservedword(u8 *name,i64 opcode,i64 mode);
static i64 pci_lex$lookup(u8 *name,i64 length,i64 hashindex0);
static i64 pci_lex$gethashvaluez(u8 *s);
struct pci_decls$strec *pci_lex$findsymbol(u8 *name);
void pci_parse$parseline(i64 lineno);
static void pci_parse$readmode(byte *m,i64 *size);
static i64 pci_parse$readint(void);
static void pci_parse$readattrs(struct pci_decls$pclrec *p,i64 nattrs);
static void pci_parse$readopnd(struct pci_decls$pclrec *p,i64 flags);
static void pci_parse$checkdefined(struct pci_decls$pclrec *p,i64 idtype);
static void pci_parse$checksymbol(i64 symbol);
static void pci_parse$dolocal(struct pci_decls$pclrec *p);
static void pci_parse$doparam(struct pci_decls$pclrec *p);
static void pci_parse$dostatic(struct pci_decls$pclrec *p);
static void pci_parse$doidata(struct pci_decls$pclrec *p);
static i64 pci_parse$getpclsize(struct pci_decls$pclrec *p);
static void pci_parse$doproc(struct pci_decls$pclrec *p);
static void pci_parse$doextproc(struct pci_decls$pclrec *p);
static void pci_parse$doextparam(struct pci_decls$pclrec *p);
static void pci_parse$updateproc(void);
static void pci_parse$undefinedcheck(void);
void pci_parse$parseprogram(void);
static void pci_parse$processop(struct pci_decls$pclrec *p,i64 size);
void pci_parse$start(void);
i64 pci_support$docalldll(u64 fnindex,void *fnaddr,i64 (*revargs)[],i64 nargs,i64 nvars,i64 isfunc);
void pci_support$loadlibs(void);
void (*pci_support$getdllfnptr(i64 fnindex))(void);
void pci_support$initpcl1(void);
void pci_support$gerror(u8 *mess,u8 *mess2);
void pci_support$pcerror3(u8 *mess,u8 *mess2,i64 pc);
void pci_support$loaderror(u8 *mess);
void pci_support$addlib(struct pci_decls$strec *d);
i64 pci_support$serror(u8 *mess);
void pci_support$docmdskip(void);
void pci_support$adddataref(struct pci_decls$strec **pd);
void pci_support$addlabelref(i64 *plab);
void pci_support$updatedatarefs(void);
void pci_support$storebit(i64 *p,i64 i,i64 j,i64 x);
void pci_support$start(void);
void pci_tables$start(void);
int main(int, char**, char**);
static void $pci$getinputoptions(void);
static void $pci$do_option(i64 sw,u8 *value);
u8 *$pci$addstr(u8 *s,u8 *t);
static void $pci$loadprogram(void);
static void $pci$writeswitch(void);
void $pci$start(void);
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
extern void *dlopen(u8 *$1,i32 $2);
extern void *dlsym(void *$1,u8 *$2);
extern i32 tcgetattr(i32 $1,struct mlinux$termios *$2);
extern i32 tcsetattr(i32 $1,i32 $2,struct mlinux$termios *$3);
extern i32 gettimeofday(struct mlinux$timeval *$1,void *$2);
extern void *gmtime_r(i64 *$1,struct mlinux$tm_rec *$2);
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
static struct pci_decls$lexrec pci_decls$lx;
static u8 *  pci_decls$inputfile;
static u8 *  pci_decls$psource;
static u8 *(*pci_decls$sourcelines)[];
static i64 pci_decls$nsourcelines;
static struct pci_decls$strec pci_decls$hashtable[65536];
static struct pci_decls$strec *  pci_decls$stvoid;
static i32 pci_decls$labeltable[15000];
static i64 pci_decls$highlabel;
static struct pci_decls$pclrec (*pci_decls$pcltable)[];
static i32 (*pci_decls$pcllines)[];
static u16 (*pci_decls$pcllabels)[][8];
static i64 pci_decls$npcl;
static u64 pci_decls$libinst[20];
static u8 *  pci_decls$libnames[20];
static struct pci_decls$strec *  pci_decls$libdef[20];
static byte pci_decls$libtype[20];
static i64 pci_decls$nlibs;
static struct pci_decls$strec *  pci_decls$extdef[500];
static void *  pci_decls$extaddr[500];
static byte pci_decls$extnparams[500];
static byte pci_decls$exttypes[500][16];
static byte pci_decls$extrettype[500];
static byte pci_decls$extvariadic[500];
static i64 pci_decls$nextprocs;
static i64 pci_decls$cmdskip;
static struct pci_decls$datarec *  pci_decls$datalist;
static struct pci_decls$labelrec *  pci_decls$labellist;
static byte pci_execc$debug;
static struct pci_decls$strec *  pci_fixup$stproc;
static u8 *  pci_lex$lxstart;
static u8 *  pci_lex$lxsptr;
static u8 pci_lex$alphamap[256];
static struct pci_decls$strec *  pci_parse$stproc;
static struct pci_decls$strec *  pci_parse$stextproc;
static struct pci_decls$strec *  pci_parse$ststatic;
static i64 pci_parse$nstaticbytes;
static struct pci_decls$strec *  pci_parse$stmain;
static i64 pci_parse$pcmain;
static i64 pci_parse$undefflag;
static i64 pci_parse$pcllineno;
static i64 pci_parse$lastopcode;
static i64 pci_parse$framebytes;
static i64 pci_parse$parambytes;
static i64 pci_parse$entrypending;
static u8 *  pci_tables$symbolnames[13] = {
    (byte*)"errorsym",
    (byte*)"namesym",
    (byte*)"labelsym",
    (byte*)"intsym",
    (byte*)"realsym",
    (byte*)"stringsym",
    (byte*)"charstringsym",
    (byte*)"starsym",
    (byte*)"colonsym",
    (byte*)"opcodesym",
    (byte*)"typesym",
    (byte*)"eolsym",
    (byte*)"sizesym"
};
static u8 *  pci_tables$typenames[13] = {
    (byte*)"",
    (byte*)"i64",
    (byte*)"u64",
    (byte*)"r64",
    (byte*)"r32",
    (byte*)"mem",
    (byte*)"i8",
    (byte*)"i16",
    (byte*)"i32",
    (byte*)"u8",
    (byte*)"u16",
    (byte*)"u32",
    (byte*)"void"
};
static byte pci_tables$typesizes[13] = {
    (u8)0u,
    (u8)8u,
    (u8)8u,
    (u8)8u,
    (u8)4u,
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)4u,
    (u8)1u,
    (u8)2u,
    (u8)4u,
    (u8)0u
};
static u8 *  pci_tables$idtypenames[7] = {(byte*)"noid",(byte*)"procid",(byte*)"staticid",(byte*)"frameid",(byte*)"paramid",(byte*)"importid",(byte*)"libid"};
static u8 *  pci_tables$pclnames[150] = {
    (byte*)"kproc",
    (byte*)"kparam",
    (byte*)"klocal",
    (byte*)"krettype",
    (byte*)"kend",
    (byte*)"kextproc",
    (byte*)"kextparam",
    (byte*)"kextvariadic",
    (byte*)"kextend",
    (byte*)"kistatic",
    (byte*)"kzstatic",
    (byte*)"kdata",
    (byte*)"klinkdll",
    (byte*)"kstartmx",
    (byte*)"kresetmx",
    (byte*)"kendmx",
    (byte*)"kload",
    (byte*)"kloadref",
    (byte*)"kloadimm",
    (byte*)"kstore",
    (byte*)"kunload",
    (byte*)"kdouble",
    (byte*)"kdupl",
    (byte*)"kswapopnds",
    (byte*)"kswapmem",
    (byte*)"kclear",
    (byte*)"kiload",
    (byte*)"kistore",
    (byte*)"kiloadx",
    (byte*)"kistorex",
    (byte*)"kaddptrx",
    (byte*)"ksubptrx",
    (byte*)"ksubptr",
    (byte*)"kcallp",
    (byte*)"kcallf",
    (byte*)"kicallp",
    (byte*)"kicallf",
    (byte*)"ksetcall",
    (byte*)"ksetarg",
    (byte*)"ksetret",
    (byte*)"kreturn",
    (byte*)"kstop",
    (byte*)"kjump",
    (byte*)"kijump",
    (byte*)"kjumpeq",
    (byte*)"kjumpne",
    (byte*)"kjumplt",
    (byte*)"kjumple",
    (byte*)"kjumpge",
    (byte*)"kjumpgt",
    (byte*)"kjumpt",
    (byte*)"kjumpf",
    (byte*)"kforup",
    (byte*)"kfordown",
    (byte*)"kto",
    (byte*)"kswitch",
    (byte*)"kswlabel",
    (byte*)"kendsw",
    (byte*)"kloadbit",
    (byte*)"kstorebit",
    (byte*)"kloadbf",
    (byte*)"kstorebf",
    (byte*)"kadd",
    (byte*)"ksub",
    (byte*)"kmul",
    (byte*)"kdivf",
    (byte*)"kdiv",
    (byte*)"krem",
    (byte*)"kdivrem",
    (byte*)"kbitand",
    (byte*)"kbitor",
    (byte*)"kbitxor",
    (byte*)"kshl",
    (byte*)"kshr",
    (byte*)"kmin",
    (byte*)"kmax",
    (byte*)"keq",
    (byte*)"kne",
    (byte*)"klt",
    (byte*)"kle",
    (byte*)"kge",
    (byte*)"kgt",
    (byte*)"kpower",
    (byte*)"katan2",
    (byte*)"kaddto",
    (byte*)"ksubto",
    (byte*)"kmulto",
    (byte*)"kdivfto",
    (byte*)"kdivto",
    (byte*)"kremto",
    (byte*)"kbitandto",
    (byte*)"kbitorto",
    (byte*)"kbitxorto",
    (byte*)"kshlto",
    (byte*)"kshrto",
    (byte*)"kminto",
    (byte*)"kmaxto",
    (byte*)"kaddpxto",
    (byte*)"ksubpxto",
    (byte*)"kneg",
    (byte*)"kabs",
    (byte*)"kbitnot",
    (byte*)"knot",
    (byte*)"knotnot",
    (byte*)"ksqr",
    (byte*)"ksign",
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
    (byte*)"knegto",
    (byte*)"kabsto",
    (byte*)"kbitnotto",
    (byte*)"knotto",
    (byte*)"knotnotto",
    (byte*)"kincrto",
    (byte*)"kincrload",
    (byte*)"kloadincr",
    (byte*)"kdecrto",
    (byte*)"kdecrload",
    (byte*)"kloaddecr",
    (byte*)"kfloat",
    (byte*)"kfix",
    (byte*)"ktruncate",
    (byte*)"kfwiden",
    (byte*)"kfnarrow",
    (byte*)"ktypepun",
    (byte*)"kwiden",
    (byte*)"kopnd",
    (byte*)"kassem",
    (byte*)"kprinti64",
    (byte*)"kprintu64",
    (byte*)"kprintr64",
    (byte*)"kprintr32",
    (byte*)"kprintstr",
    (byte*)"kprinthex",
    (byte*)"kprintsp",
    (byte*)"ktest",
    (byte*)"kdebug",
    (byte*)"klast"
};
static byte pci_tables$pclmain[150] = {
    (u8)16u,
    (u8)16u,
    (u8)16u,
    (u8)0u,
    (u8)0u,
    (u8)16u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)16u,
    (u8)16u,
    (u8)31u,
    (u8)24u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)16u,
    (u8)17u,
    (u8)14u,
    (u8)16u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)16u,
    (u8)16u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)48u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
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
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)19u,
    (u8)8u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)8u,
    (u8)0u,
    (u8)0u
};
static byte pci_tables$pclattr[150] = {
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
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
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
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
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
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
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
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
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
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
    (u8)0u
};
static u16 pci_tables$pcljcode[150] = {
    (u16)(i64)359,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)39,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)375,
    (u16)(i64)376,
    (u16)(i64)377,
    (u16)(i64)378,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)363,
    (u16)(i64)364,
    (u16)(i64)365,
    (u16)(i64)379,
    (u16)(i64)380,
    (u16)(i64)381,
    (u16)(i64)382,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)385,
    (u16)(i64)386,
    (u16)(i64)395,
    (u16)(i64)389,
    (u16)(i64)390,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)304,
    (u16)(i64)310,
    (u16)(i64)316,
    (u16)(i64)369,
    (u16)(i64)370,
    (u16)(i64)0,
    (u16)(i64)371,
    (u16)(i64)372,
    (u16)(i64)373,
    (u16)(i64)374,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)355,
    (u16)(i64)356,
    (u16)(i64)357,
    (u16)(i64)358,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)354,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)361,
    (u16)(i64)362,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)366,
    (u16)(i64)367,
    (u16)(i64)368,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)340,
    (u16)(i64)341,
    (u16)(i64)342,
    (u16)(i64)343,
    (u16)(i64)344,
    (u16)(i64)345,
    (u16)(i64)346,
    (u16)(i64)347,
    (u16)(i64)348,
    (u16)(i64)349,
    (u16)(i64)350,
    (u16)(i64)351,
    (u16)(i64)352,
    (u16)(i64)353,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)326,
    (u16)(i64)327,
    (u16)(i64)0,
    (u16)(i64)0,
    (u16)(i64)405,
    (u16)(i64)396,
    (u16)(i64)397,
    (u16)(i64)398,
    (u16)(i64)399,
    (u16)(i64)400,
    (u16)(i64)401,
    (u16)(i64)402,
    (u16)(i64)403,
    (u16)(i64)404,
    (u16)(i64)408,
    (u16)(i64)0
};
static u8 *  pci_tables$jcodenames[410] = {
    (byte*)"jnotready",
    (byte*)"jload_mi8",
    (byte*)"jload_mi16",
    (byte*)"jload_mi32",
    (byte*)"jload_mi64",
    (byte*)"jload_mu8",
    (byte*)"jload_mu16",
    (byte*)"jload_mu32",
    (byte*)"jload_mr64",
    (byte*)"jload_mr32",
    (byte*)"jload_mmem",
    (byte*)"jload_fi8",
    (byte*)"jload_fi16",
    (byte*)"jload_fi32",
    (byte*)"jload_fi64",
    (byte*)"jload_fu8",
    (byte*)"jload_fu16",
    (byte*)"jload_fu32",
    (byte*)"jload_fr64",
    (byte*)"jload_fr32",
    (byte*)"jload_fmem",
    (byte*)"jstore_mi8",
    (byte*)"jstore_mi16",
    (byte*)"jstore_mi32",
    (byte*)"jstore_mi64",
    (byte*)"jstore_mr64",
    (byte*)"jstore_mr32",
    (byte*)"jstore_mmem",
    (byte*)"jstore_fi8",
    (byte*)"jstore_fi16",
    (byte*)"jstore_fi32",
    (byte*)"jstore_fi64",
    (byte*)"jstore_fr64",
    (byte*)"jstore_fr32",
    (byte*)"jstore_fmem",
    (byte*)"jloadimm_i64",
    (byte*)"jloadimm_r64",
    (byte*)"jloadimm_r32",
    (byte*)"jloadimm_str",
    (byte*)"jloadref_m",
    (byte*)"jloadref_f",
    (byte*)"jloadref_lab",
    (byte*)"jloadref_p",
    (byte*)"jloadref_ext",
    (byte*)"jiload_i8",
    (byte*)"jiload_i16",
    (byte*)"jiload_i32",
    (byte*)"jiload_i64",
    (byte*)"jiload_u8",
    (byte*)"jiload_u16",
    (byte*)"jiload_u32",
    (byte*)"jiload_r64",
    (byte*)"jiload_r32",
    (byte*)"jiload_mem",
    (byte*)"jiloadx_i8",
    (byte*)"jiloadx_i16",
    (byte*)"jiloadx_i32",
    (byte*)"jiloadx_i64",
    (byte*)"jiloadx_u8",
    (byte*)"jiloadx_u16",
    (byte*)"jiloadx_u32",
    (byte*)"jiloadx_r64",
    (byte*)"jiloadx_r32",
    (byte*)"jiloadx_mem",
    (byte*)"jistore_i8",
    (byte*)"jistore_i16",
    (byte*)"jistore_i32",
    (byte*)"jistore_i64",
    (byte*)"jistore_r64",
    (byte*)"jistore_r32",
    (byte*)"jistore_mem",
    (byte*)"jistorex_i8",
    (byte*)"jistorex_i16",
    (byte*)"jistorex_i32",
    (byte*)"jistorex_i64",
    (byte*)"jistorex_r64",
    (byte*)"jistorex_r32",
    (byte*)"jistorex_mem",
    (byte*)"jswapmem_i8",
    (byte*)"jswapmem_i16",
    (byte*)"jswapmem_i32",
    (byte*)"jswapmem_i64",
    (byte*)"jswapmem_mem",
    (byte*)"jclear_i8",
    (byte*)"jclear_i16",
    (byte*)"jclear_i32",
    (byte*)"jclear_i64",
    (byte*)"jclear_mem",
    (byte*)"jjumpeq_i64",
    (byte*)"jjumpeq_r64",
    (byte*)"jjumpeq_r32",
    (byte*)"jjumpeq_mem",
    (byte*)"jjumpne_i64",
    (byte*)"jjumpne_r64",
    (byte*)"jjumpne_r32",
    (byte*)"jjumpne_mem",
    (byte*)"jjumplt_i64",
    (byte*)"jjumplt_u64",
    (byte*)"jjumplt_r64",
    (byte*)"jjumplt_r32",
    (byte*)"jjumple_i64",
    (byte*)"jjumple_u64",
    (byte*)"jjumple_r64",
    (byte*)"jjumple_r32",
    (byte*)"jjumpge_i64",
    (byte*)"jjumpge_u64",
    (byte*)"jjumpge_r64",
    (byte*)"jjumpge_r32",
    (byte*)"jjumpgt_i64",
    (byte*)"jjumpgt_u64",
    (byte*)"jjumpgt_r64",
    (byte*)"jjumpgt_r32",
    (byte*)"jjumpt_i64",
    (byte*)"jjumpt_r64",
    (byte*)"jjumpt_r32",
    (byte*)"jjumpf_i64",
    (byte*)"jjumpf_r64",
    (byte*)"jjumpf_r32",
    (byte*)"jadd_i64",
    (byte*)"jadd_r64",
    (byte*)"jadd_r32",
    (byte*)"jsub_i64",
    (byte*)"jsub_r64",
    (byte*)"jsub_r32",
    (byte*)"jmul_i64",
    (byte*)"jmul_r64",
    (byte*)"jmul_r32",
    (byte*)"jneg_i64",
    (byte*)"jneg_r64",
    (byte*)"jneg_r32",
    (byte*)"jabs_i64",
    (byte*)"jabs_r64",
    (byte*)"jabs_r32",
    (byte*)"jsqr_i64",
    (byte*)"jsqr_r64",
    (byte*)"jsqr_r32",
    (byte*)"jsign_i64",
    (byte*)"jsign_r64",
    (byte*)"jsign_r32",
    (byte*)"jnegto_i8",
    (byte*)"jnegto_i16",
    (byte*)"jnegto_i32",
    (byte*)"jnegto_i64",
    (byte*)"jnegto_r64",
    (byte*)"jnegto_r32",
    (byte*)"jabsto_i8",
    (byte*)"jabsto_i16",
    (byte*)"jabsto_i32",
    (byte*)"jabsto_i64",
    (byte*)"jabsto_r64",
    (byte*)"jabsto_r32",
    (byte*)"jdivf_r64",
    (byte*)"jdivf_r32",
    (byte*)"jdiv_i64",
    (byte*)"jdiv_u64",
    (byte*)"jrem_i64",
    (byte*)"jrem_u64",
    (byte*)"jdivrem_i64",
    (byte*)"jdivrem_u64",
    (byte*)"jshr_i64",
    (byte*)"jshr_u64",
    (byte*)"jeq_i64",
    (byte*)"jeq_u64",
    (byte*)"jeq_r64",
    (byte*)"jeq_r32",
    (byte*)"jne_i64",
    (byte*)"jne_u64",
    (byte*)"jne_r64",
    (byte*)"jne_r32",
    (byte*)"jmin_i64",
    (byte*)"jmin_u64",
    (byte*)"jmin_r64",
    (byte*)"jmin_r32",
    (byte*)"jmax_i64",
    (byte*)"jmax_u64",
    (byte*)"jmax_r64",
    (byte*)"jmax_r32",
    (byte*)"jlt_i64",
    (byte*)"jlt_u64",
    (byte*)"jlt_r64",
    (byte*)"jlt_r32",
    (byte*)"jle_i64",
    (byte*)"jle_u64",
    (byte*)"jle_r64",
    (byte*)"jle_r32",
    (byte*)"jge_i64",
    (byte*)"jge_u64",
    (byte*)"jge_r64",
    (byte*)"jge_r32",
    (byte*)"jgt_i64",
    (byte*)"jgt_u64",
    (byte*)"jgt_r64",
    (byte*)"jgt_r32",
    (byte*)"jpower_i64",
    (byte*)"jpower_u64",
    (byte*)"jpower_r64",
    (byte*)"jaddto_i8",
    (byte*)"jaddto_i16",
    (byte*)"jaddto_i32",
    (byte*)"jaddto_i64",
    (byte*)"jaddto_r64",
    (byte*)"jaddto_r32",
    (byte*)"jsubto_i8",
    (byte*)"jsubto_i16",
    (byte*)"jsubto_i32",
    (byte*)"jsubto_i64",
    (byte*)"jsubto_r64",
    (byte*)"jsubto_r32",
    (byte*)"jmulto_i8",
    (byte*)"jmulto_i16",
    (byte*)"jmulto_i32",
    (byte*)"jmulto_i64",
    (byte*)"jmulto_r64",
    (byte*)"jmulto_r32",
    (byte*)"jdivto_i8",
    (byte*)"jdivto_i16",
    (byte*)"jdivto_i32",
    (byte*)"jdivto_i64",
    (byte*)"jdivto_u8",
    (byte*)"jdivto_u16",
    (byte*)"jdivto_u32",
    (byte*)"jdivto_u64",
    (byte*)"jdivfto_r64",
    (byte*)"jdivfto_r32",
    (byte*)"jminto_i8",
    (byte*)"jminto_i16",
    (byte*)"jminto_i32",
    (byte*)"jminto_i64",
    (byte*)"jminto_u8",
    (byte*)"jminto_u16",
    (byte*)"jminto_u32",
    (byte*)"jminto_u64",
    (byte*)"jminto_r64",
    (byte*)"jminto_r32",
    (byte*)"jmaxto_i8",
    (byte*)"jmaxto_i16",
    (byte*)"jmaxto_i32",
    (byte*)"jmaxto_i64",
    (byte*)"jmaxto_u8",
    (byte*)"jmaxto_u16",
    (byte*)"jmaxto_u32",
    (byte*)"jmaxto_u64",
    (byte*)"jmaxto_r64",
    (byte*)"jmaxto_r32",
    (byte*)"jbitnotto_i8",
    (byte*)"jbitnotto_i16",
    (byte*)"jbitnotto_i32",
    (byte*)"jbitnotto_i64",
    (byte*)"jnotto_i8",
    (byte*)"jnotto_i16",
    (byte*)"jnotto_i32",
    (byte*)"jnotto_i64",
    (byte*)"jnotnotto_i8",
    (byte*)"jnotnotto_i16",
    (byte*)"jnotnotto_i32",
    (byte*)"jnotnotto_i64",
    (byte*)"jincrto_i8",
    (byte*)"jincrto_i16",
    (byte*)"jincrto_i32",
    (byte*)"jincrto_i64",
    (byte*)"jincrload_i8",
    (byte*)"jincrload_i16",
    (byte*)"jincrload_i32",
    (byte*)"jincrload_i64",
    (byte*)"jloadincr_i8",
    (byte*)"jloadincr_i16",
    (byte*)"jloadincr_i32",
    (byte*)"jloadincr_i64",
    (byte*)"jdecrto_i8",
    (byte*)"jdecrto_i16",
    (byte*)"jdecrto_i32",
    (byte*)"jdecrto_i64",
    (byte*)"jdecrload_i8",
    (byte*)"jdecrload_i16",
    (byte*)"jdecrload_i32",
    (byte*)"jdecrload_i64",
    (byte*)"jloaddecr_i8",
    (byte*)"jloaddecr_i16",
    (byte*)"jloaddecr_i32",
    (byte*)"jloaddecr_i64",
    (byte*)"jbitandto_i8",
    (byte*)"jbitandto_i16",
    (byte*)"jbitandto_i32",
    (byte*)"jbitandto_i64",
    (byte*)"jbitorto_i8",
    (byte*)"jbitorto_i16",
    (byte*)"jbitorto_i32",
    (byte*)"jbitorto_i64",
    (byte*)"jbitxorto_i8",
    (byte*)"jbitxorto_i16",
    (byte*)"jbitxorto_i32",
    (byte*)"jbitxorto_i64",
    (byte*)"jshlto_i8",
    (byte*)"jshlto_i16",
    (byte*)"jshlto_i32",
    (byte*)"jshlto_i64",
    (byte*)"jshrto_i8",
    (byte*)"jshrto_i16",
    (byte*)"jshrto_i32",
    (byte*)"jshrto_i64",
    (byte*)"jshrto_u8",
    (byte*)"jshrto_u16",
    (byte*)"jshrto_u32",
    (byte*)"jshrto_u64",
    (byte*)"jforup_mm",
    (byte*)"jforup_mf",
    (byte*)"jforup_fm",
    (byte*)"jforup_ff",
    (byte*)"jforup_mi64",
    (byte*)"jforup_fi64",
    (byte*)"jfordown_mm",
    (byte*)"jfordown_mf",
    (byte*)"jfordown_fm",
    (byte*)"jfordown_ff",
    (byte*)"jfordown_mi64",
    (byte*)"jfordown_fi64",
    (byte*)"jto_m",
    (byte*)"jto_f",
    (byte*)"jfloat_r64_i64",
    (byte*)"jfloat_r64_u64",
    (byte*)"jfloat_r32_i64",
    (byte*)"jfloat_r32_u64",
    (byte*)"jfix_i64_r64",
    (byte*)"jfix_u64_r64",
    (byte*)"jfix_i64_r32",
    (byte*)"jfix_u64_r32",
    (byte*)"jfwiden",
    (byte*)"jfnarrow",
    (byte*)"jtruncate_i8",
    (byte*)"jtruncate_i16",
    (byte*)"jtruncate_i32",
    (byte*)"jtruncate_u8",
    (byte*)"jtruncate_u16",
    (byte*)"jtruncate_u32",
    (byte*)"jwiden_i8",
    (byte*)"jwiden_i16",
    (byte*)"jwiden_i32",
    (byte*)"jwiden_u8",
    (byte*)"jwiden_u16",
    (byte*)"jwiden_u32",
    (byte*)"jsqrt",
    (byte*)"jsin",
    (byte*)"jcos",
    (byte*)"jtan",
    (byte*)"jasin",
    (byte*)"jacos",
    (byte*)"jatan",
    (byte*)"jlog",
    (byte*)"jlog10",
    (byte*)"jexp",
    (byte*)"jround",
    (byte*)"jfloor",
    (byte*)"jceil",
    (byte*)"jfract",
    (byte*)"jatan2",
    (byte*)"jbitand",
    (byte*)"jbitor",
    (byte*)"jbitxor",
    (byte*)"jshl",
    (byte*)"jprocent",
    (byte*)"jsubent",
    (byte*)"jaddpxto",
    (byte*)"jsubpxto",
    (byte*)"jaddptrx",
    (byte*)"jsubptrx",
    (byte*)"jsubptr",
    (byte*)"jbitnot",
    (byte*)"jnot",
    (byte*)"jnotnot",
    (byte*)"jswitch",
    (byte*)"jswlabel",
    (byte*)"jloadbit",
    (byte*)"jstorebit",
    (byte*)"jloadbf",
    (byte*)"jstorebf",
    (byte*)"junload",
    (byte*)"jdouble",
    (byte*)"jdupl",
    (byte*)"jswapopnds",
    (byte*)"jcallp",
    (byte*)"jcallf",
    (byte*)"jicallp",
    (byte*)"jicallf",
    (byte*)"jcalldllp",
    (byte*)"jcalldllf",
    (byte*)"jsetret_m",
    (byte*)"jreturn_p",
    (byte*)"jreturn_f",
    (byte*)"jreturn_m",
    (byte*)"jjump",
    (byte*)"jijump",
    (byte*)"jstartmx",
    (byte*)"jresetmx",
    (byte*)"jendmx",
    (byte*)"jcallargs",
    (byte*)"jstop",
    (byte*)"jassem",
    (byte*)"jprinti64",
    (byte*)"jprintu64",
    (byte*)"jprintr64",
    (byte*)"jprintr32",
    (byte*)"jprintstr",
    (byte*)"jprinthex",
    (byte*)"jprintsp",
    (byte*)"jtest",
    (byte*)"jopnd",
    (byte*)"jpushany",
    (byte*)"jstackadj",
    (byte*)"jdebug",
    (byte*)"jlast"
};
static byte pci_tables$pcllist[56] = {
    (u8)17u,
    (u8)20u,
    (u8)27u,
    (u8)29u,
    (u8)28u,
    (u8)30u,
    (u8)126u,
    (u8)129u,
    (u8)128u,
    (u8)131u,
    (u8)127u,
    (u8)130u,
    (u8)19u,
    (u8)25u,
    (u8)26u,
    (u8)63u,
    (u8)64u,
    (u8)65u,
    (u8)67u,
    (u8)68u,
    (u8)69u,
    (u8)75u,
    (u8)76u,
    (u8)83u,
    (u8)74u,
    (u8)66u,
    (u8)85u,
    (u8)86u,
    (u8)87u,
    (u8)89u,
    (u8)88u,
    (u8)91u,
    (u8)92u,
    (u8)93u,
    (u8)94u,
    (u8)95u,
    (u8)96u,
    (u8)97u,
    (u8)77u,
    (u8)78u,
    (u8)79u,
    (u8)80u,
    (u8)81u,
    (u8)82u,
    (u8)47u,
    (u8)48u,
    (u8)49u,
    (u8)50u,
    (u8)45u,
    (u8)46u,
    (u8)51u,
    (u8)52u,
    (u8)105u,
    (u8)100u,
    (u8)101u,
    (u8)138u
};
static u16 pci_tables$jcodemodelist[56][12] = {
    {
        (u16)(i64)0,
        (u16)(i64)4,
        (u16)(i64)4,
        (u16)(i64)4,
        (u16)(i64)7,
        (u16)(i64)10,
        (u16)(i64)1,
        (u16)(i64)2,
        (u16)(i64)3,
        (u16)(i64)5,
        (u16)(i64)6,
        (u16)(i64)7
},
    {
        (u16)(i64)0,
        (u16)(i64)24,
        (u16)(i64)24,
        (u16)(i64)24,
        (u16)(i64)23,
        (u16)(i64)27,
        (u16)(i64)21,
        (u16)(i64)22,
        (u16)(i64)23,
        (u16)(i64)21,
        (u16)(i64)22,
        (u16)(i64)23
},
    {
        (u16)(i64)0,
        (u16)(i64)47,
        (u16)(i64)47,
        (u16)(i64)47,
        (u16)(i64)46,
        (u16)(i64)53,
        (u16)(i64)44,
        (u16)(i64)45,
        (u16)(i64)46,
        (u16)(i64)48,
        (u16)(i64)49,
        (u16)(i64)50
},
    {
        (u16)(i64)0,
        (u16)(i64)57,
        (u16)(i64)57,
        (u16)(i64)57,
        (u16)(i64)56,
        (u16)(i64)63,
        (u16)(i64)54,
        (u16)(i64)55,
        (u16)(i64)56,
        (u16)(i64)58,
        (u16)(i64)59,
        (u16)(i64)60
},
    {
        (u16)(i64)0,
        (u16)(i64)67,
        (u16)(i64)67,
        (u16)(i64)67,
        (u16)(i64)66,
        (u16)(i64)70,
        (u16)(i64)64,
        (u16)(i64)65,
        (u16)(i64)66,
        (u16)(i64)64,
        (u16)(i64)65,
        (u16)(i64)66
},
    {
        (u16)(i64)0,
        (u16)(i64)74,
        (u16)(i64)74,
        (u16)(i64)74,
        (u16)(i64)73,
        (u16)(i64)77,
        (u16)(i64)71,
        (u16)(i64)72,
        (u16)(i64)73,
        (u16)(i64)71,
        (u16)(i64)72,
        (u16)(i64)73
},
    {
        (u16)(i64)0,
        (u16)(i64)259,
        (u16)(i64)259,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)256,
        (u16)(i64)257,
        (u16)(i64)258,
        (u16)(i64)256,
        (u16)(i64)257,
        (u16)(i64)258
},
    {
        (u16)(i64)0,
        (u16)(i64)271,
        (u16)(i64)271,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)268,
        (u16)(i64)269,
        (u16)(i64)270,
        (u16)(i64)268,
        (u16)(i64)269,
        (u16)(i64)270
},
    {
        (u16)(i64)0,
        (u16)(i64)267,
        (u16)(i64)267,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)264,
        (u16)(i64)265,
        (u16)(i64)266,
        (u16)(i64)264,
        (u16)(i64)265,
        (u16)(i64)266
},
    {
        (u16)(i64)0,
        (u16)(i64)279,
        (u16)(i64)279,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)276,
        (u16)(i64)277,
        (u16)(i64)278,
        (u16)(i64)276,
        (u16)(i64)277,
        (u16)(i64)278
},
    {
        (u16)(i64)0,
        (u16)(i64)263,
        (u16)(i64)263,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)260,
        (u16)(i64)261,
        (u16)(i64)262,
        (u16)(i64)260,
        (u16)(i64)261,
        (u16)(i64)262
},
    {
        (u16)(i64)0,
        (u16)(i64)275,
        (u16)(i64)275,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)272,
        (u16)(i64)273,
        (u16)(i64)274,
        (u16)(i64)272,
        (u16)(i64)273,
        (u16)(i64)274
},
    {
        (u16)(i64)0,
        (u16)(i64)35,
        (u16)(i64)35,
        (u16)(i64)36,
        (u16)(i64)37,
        (u16)(i64)0,
        (u16)(i64)35,
        (u16)(i64)35,
        (u16)(i64)35,
        (u16)(i64)35,
        (u16)(i64)35,
        (u16)(i64)35
},
    {
        (u16)(i64)0,
        (u16)(i64)81,
        (u16)(i64)81,
        (u16)(i64)81,
        (u16)(i64)80,
        (u16)(i64)82,
        (u16)(i64)78,
        (u16)(i64)79,
        (u16)(i64)80,
        (u16)(i64)78,
        (u16)(i64)79,
        (u16)(i64)80
},
    {
        (u16)(i64)0,
        (u16)(i64)86,
        (u16)(i64)86,
        (u16)(i64)86,
        (u16)(i64)85,
        (u16)(i64)87,
        (u16)(i64)83,
        (u16)(i64)84,
        (u16)(i64)85,
        (u16)(i64)83,
        (u16)(i64)84,
        (u16)(i64)85
},
    {
        (u16)(i64)0,
        (u16)(i64)118,
        (u16)(i64)118,
        (u16)(i64)119,
        (u16)(i64)120,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)121,
        (u16)(i64)121,
        (u16)(i64)122,
        (u16)(i64)123,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)124,
        (u16)(i64)124,
        (u16)(i64)125,
        (u16)(i64)126,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)153,
        (u16)(i64)154,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)155,
        (u16)(i64)156,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)157,
        (u16)(i64)158,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)169,
        (u16)(i64)170,
        (u16)(i64)171,
        (u16)(i64)172,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)173,
        (u16)(i64)174,
        (u16)(i64)175,
        (u16)(i64)176,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)193,
        (u16)(i64)194,
        (u16)(i64)195,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)159,
        (u16)(i64)160,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)151,
        (u16)(i64)152,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)199,
        (u16)(i64)199,
        (u16)(i64)200,
        (u16)(i64)201,
        (u16)(i64)0,
        (u16)(i64)196,
        (u16)(i64)197,
        (u16)(i64)198,
        (u16)(i64)196,
        (u16)(i64)197,
        (u16)(i64)198
},
    {
        (u16)(i64)0,
        (u16)(i64)205,
        (u16)(i64)205,
        (u16)(i64)206,
        (u16)(i64)207,
        (u16)(i64)0,
        (u16)(i64)202,
        (u16)(i64)203,
        (u16)(i64)204,
        (u16)(i64)202,
        (u16)(i64)203,
        (u16)(i64)204
},
    {
        (u16)(i64)0,
        (u16)(i64)211,
        (u16)(i64)211,
        (u16)(i64)212,
        (u16)(i64)213,
        (u16)(i64)0,
        (u16)(i64)208,
        (u16)(i64)209,
        (u16)(i64)210,
        (u16)(i64)208,
        (u16)(i64)209,
        (u16)(i64)210
},
    {
        (u16)(i64)0,
        (u16)(i64)217,
        (u16)(i64)217,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)214,
        (u16)(i64)215,
        (u16)(i64)216,
        (u16)(i64)214,
        (u16)(i64)215,
        (u16)(i64)216
},
    {
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)222,
        (u16)(i64)223,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)283,
        (u16)(i64)283,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)280,
        (u16)(i64)281,
        (u16)(i64)282,
        (u16)(i64)280,
        (u16)(i64)281,
        (u16)(i64)282
},
    {
        (u16)(i64)0,
        (u16)(i64)287,
        (u16)(i64)287,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)284,
        (u16)(i64)285,
        (u16)(i64)286,
        (u16)(i64)284,
        (u16)(i64)285,
        (u16)(i64)286
},
    {
        (u16)(i64)0,
        (u16)(i64)291,
        (u16)(i64)291,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)288,
        (u16)(i64)289,
        (u16)(i64)290,
        (u16)(i64)288,
        (u16)(i64)289,
        (u16)(i64)290
},
    {
        (u16)(i64)0,
        (u16)(i64)295,
        (u16)(i64)295,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)292,
        (u16)(i64)293,
        (u16)(i64)294,
        (u16)(i64)292,
        (u16)(i64)293,
        (u16)(i64)294
},
    {
        (u16)(i64)0,
        (u16)(i64)299,
        (u16)(i64)299,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)296,
        (u16)(i64)297,
        (u16)(i64)298,
        (u16)(i64)300,
        (u16)(i64)301,
        (u16)(i64)302
},
    {
        (u16)(i64)0,
        (u16)(i64)227,
        (u16)(i64)227,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)224,
        (u16)(i64)225,
        (u16)(i64)226,
        (u16)(i64)228,
        (u16)(i64)229,
        (u16)(i64)230
},
    {
        (u16)(i64)0,
        (u16)(i64)237,
        (u16)(i64)237,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)234,
        (u16)(i64)235,
        (u16)(i64)236,
        (u16)(i64)238,
        (u16)(i64)239,
        (u16)(i64)240
},
    {
        (u16)(i64)0,
        (u16)(i64)161,
        (u16)(i64)161,
        (u16)(i64)163,
        (u16)(i64)164,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)165,
        (u16)(i64)165,
        (u16)(i64)167,
        (u16)(i64)168,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)177,
        (u16)(i64)178,
        (u16)(i64)179,
        (u16)(i64)180,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)181,
        (u16)(i64)182,
        (u16)(i64)183,
        (u16)(i64)184,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)185,
        (u16)(i64)186,
        (u16)(i64)187,
        (u16)(i64)188,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)189,
        (u16)(i64)190,
        (u16)(i64)191,
        (u16)(i64)192,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)96,
        (u16)(i64)97,
        (u16)(i64)98,
        (u16)(i64)99,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)100,
        (u16)(i64)101,
        (u16)(i64)102,
        (u16)(i64)103,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)104,
        (u16)(i64)105,
        (u16)(i64)106,
        (u16)(i64)107,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)108,
        (u16)(i64)109,
        (u16)(i64)110,
        (u16)(i64)111,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)88,
        (u16)(i64)88,
        (u16)(i64)89,
        (u16)(i64)90,
        (u16)(i64)91,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)92,
        (u16)(i64)92,
        (u16)(i64)93,
        (u16)(i64)94,
        (u16)(i64)95,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)112,
        (u16)(i64)112,
        (u16)(i64)113,
        (u16)(i64)114,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)115,
        (u16)(i64)115,
        (u16)(i64)116,
        (u16)(i64)117,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)133,
        (u16)(i64)133,
        (u16)(i64)134,
        (u16)(i64)135,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)127,
        (u16)(i64)127,
        (u16)(i64)128,
        (u16)(i64)129,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)130,
        (u16)(i64)130,
        (u16)(i64)131,
        (u16)(i64)132,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0
},
    {
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)0,
        (u16)(i64)334,
        (u16)(i64)335,
        (u16)(i64)336,
        (u16)(i64)337,
        (u16)(i64)338,
        (u16)(i64)339
}
};
static u8 *  $pci$passnames[3] = {(byte*)"parse_pass",(byte*)"fixup_pass",(byte*)"run_pass"};
static byte $pci$passlevel;
static byte $pci$fshowpcl;
static byte $pci$fshowst;
static byte $pci$fshowlibs;
static u8 *  $pci$optionnames[8] = {(byte*)"parse",(byte*)"fixup",(byte*)"dparse",(byte*)"dfixup",(byte*)"run",(byte*)"showpcl",(byte*)"showst",(byte*)"showlibs"};
static byte $pci$ftest;
static void *  msysc$_fnaddresses[]= {
    &pci_decls$start,
    &pci_execc$exec,
    &pci_execc$runpcl,
    &pci_execc$getr32,
    &pci_execc$putr32,
    &pci_execc$start,
    &pci_fixup$fixuppcl,
    &pci_fixup$addpclop,
    &pci_fixup$convertpcl,
    &pci_fixup$findjcode,
    &pci_fixup$start,
    &pci_lex$startlex,
    &pci_lex$startlextest,
    &pci_lex$lex,
    &pci_lex$start,
    &pci_lex$lxerror,
    &pci_lex$printlx,
    &pci_lex$readdec,
    &pci_lex$readhex,
    &pci_lex$readreal,
    &pci_lex$readstring,
    &pci_lex$inithashtable,
    &pci_lex$adddlllib,
    &pci_lex$printhashtable,
    &pci_lex$addreservedword,
    &pci_lex$lookup,
    &pci_lex$gethashvaluez,
    &pci_lex$findsymbol,
    &pci_parse$parseline,
    &pci_parse$readmode,
    &pci_parse$readint,
    &pci_parse$readattrs,
    &pci_parse$readopnd,
    &pci_parse$checkdefined,
    &pci_parse$checksymbol,
    &pci_parse$dolocal,
    &pci_parse$doparam,
    &pci_parse$dostatic,
    &pci_parse$doidata,
    &pci_parse$getpclsize,
    &pci_parse$doproc,
    &pci_parse$doextproc,
    &pci_parse$doextparam,
    &pci_parse$updateproc,
    &pci_parse$undefinedcheck,
    &pci_parse$parseprogram,
    &pci_parse$processop,
    &pci_parse$start,
    &pci_support$docalldll,
    &pci_support$loadlibs,
    &pci_support$getdllfnptr,
    &pci_support$initpcl1,
    &pci_support$gerror,
    &pci_support$pcerror3,
    &pci_support$loaderror,
    &pci_support$addlib,
    &pci_support$serror,
    &pci_support$docmdskip,
    &pci_support$adddataref,
    &pci_support$addlabelref,
    &pci_support$updatedatarefs,
    &pci_support$storebit,
    &pci_support$start,
    &pci_tables$start,
    &main,
    &$pci$getinputoptions,
    &$pci$do_option,
    &$pci$addstr,
    &$pci$loadprogram,
    &$pci$writeswitch,
    &$pci$start,
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
    (byte*)"start",
    (byte*)"exec",
    (byte*)"runpcl",
    (byte*)"getr32",
    (byte*)"putr32",
    (byte*)"start",
    (byte*)"fixuppcl",
    (byte*)"addpclop",
    (byte*)"convertpcl",
    (byte*)"findjcode",
    (byte*)"start",
    (byte*)"startlex",
    (byte*)"startlextest",
    (byte*)"lex",
    (byte*)"start",
    (byte*)"lxerror",
    (byte*)"printlx",
    (byte*)"readdec",
    (byte*)"readhex",
    (byte*)"readreal",
    (byte*)"readstring",
    (byte*)"inithashtable",
    (byte*)"adddlllib",
    (byte*)"printhashtable",
    (byte*)"addreservedword",
    (byte*)"lookup",
    (byte*)"gethashvaluez",
    (byte*)"findsymbol",
    (byte*)"parseline",
    (byte*)"readmode",
    (byte*)"readint",
    (byte*)"readattrs",
    (byte*)"readopnd",
    (byte*)"checkdefined",
    (byte*)"checksymbol",
    (byte*)"dolocal",
    (byte*)"doparam",
    (byte*)"dostatic",
    (byte*)"doidata",
    (byte*)"getpclsize",
    (byte*)"doproc",
    (byte*)"doextproc",
    (byte*)"doextparam",
    (byte*)"updateproc",
    (byte*)"undefinedcheck",
    (byte*)"parseprogram",
    (byte*)"processop",
    (byte*)"start",
    (byte*)"docalldll",
    (byte*)"loadlibs",
    (byte*)"getdllfnptr",
    (byte*)"initpcl1",
    (byte*)"gerror",
    (byte*)"pcerror3",
    (byte*)"loaderror",
    (byte*)"addlib",
    (byte*)"serror",
    (byte*)"docmdskip",
    (byte*)"adddataref",
    (byte*)"addlabelref",
    (byte*)"updatedatarefs",
    (byte*)"storebit",
    (byte*)"start",
    (byte*)"start",
    (byte*)"main",
    (byte*)"getinputoptions",
    (byte*)"do_option",
    (byte*)"addstr",
    (byte*)"loadprogram",
    (byte*)"writeswitch",
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
static i64 msysc$_fnnprocs=276;
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
static i64 mlinux$init_flag = (i64)0;

/* PROCDEFS */
// START
void pci_decls$start(void) {

}

i64 pci_execc$exec(i64 pcstart,i64 spstart) {
        i64 stack[70000];
        r64 (*xstack)[70000];
        u64 (*ustack)[70000];
        void *(*pstack)[70000];
        i64 pc;
        i64 sp;
        i64 fp;
        i64 *  pi64;
        i64 n;
        i64 x;
        i64 a;
        i64 b;
        byte *  pu8;
        u16 *  pu16;
        u32 *  pu32;
        u64 *  pu64;
        i8 *  pi8;
        i16 *  pi16;
        i32 *  pi32;
        r64 *  pr64;
        i64 *  pi64b;
        i64 i;
        i64 j;
        i64 spr;
        r64 xx;
        struct pci_decls$strec *  d;
    pc = pcstart;
    sp = spstart;
    fp = (i64)0;
    xstack = (r64 (*)[70000])&stack;
    ustack = (u64 (*)[70000])&stack;
    pstack = (void *(*)[70000])&stack;
    L1 :;
    switch ((i64)(*pci_decls$pcltable)[(pc)-1].jcode) {
    case 0:;
        {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"NO HANDLER",NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 1:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 2:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 3:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 4:;
        {
            pi64 = (i64 *)(*(*pci_decls$pcltable)[(pc)-1].a.def).staticaddr;
            stack[(++(sp))-1] = (*pi64);
            ++(pc);
        }
        break;
    case 5:;
        {
            pu8 = (*(*pci_decls$pcltable)[(pc)-1].a.def).staticaddr;
            stack[(++(sp))-1] = (i64)(*pu8);
            ++(pc);
        }
        break;
    case 6:;
        {
            pu16 = (u16 *)(*(*pci_decls$pcltable)[(pc)-1].a.def).staticaddr;
            stack[(++(sp))-1] = (i64)(*pu16);
            ++(pc);
        }
        break;
    case 7:;
        {
            pu32 = (u32 *)(*(*pci_decls$pcltable)[(pc)-1].a.def).staticaddr;
            stack[(++(sp))-1] = (i64)(*pu32);
            ++(pc);
        }
        break;
    case 8:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 9:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 10:;
        {
            (*pstack)[(++(sp))-1] = (*(*pci_decls$pcltable)[(pc)-1].a.def).staticaddr;
            ++(pc);
        }
        break;
    case 11:;
        {
            stack[(++(sp))-1] = (i64)(i8)stack[((fp + (*(*pci_decls$pcltable)[(pc)-1].a.def).frameoffset))-1];
            ++(pc);
        }
        break;
    case 12:;
        {
            stack[(++(sp))-1] = (i64)(i16)stack[((fp + (*(*pci_decls$pcltable)[(pc)-1].a.def).frameoffset))-1];
            ++(pc);
        }
        break;
    case 13:;
        {
            stack[(++(sp))-1] = (i64)(i32)stack[((fp + (*(*pci_decls$pcltable)[(pc)-1].a.def).frameoffset))-1];
            ++(pc);
        }
        break;
    case 14:;
        {
            stack[(++(sp))-1] = stack[((fp + (*(*pci_decls$pcltable)[(pc)-1].a.def).frameoffset))-1];
            ++(pc);
        }
        break;
    case 15:;
        {
            stack[(++(sp))-1] = (i64)(byte)stack[((fp + (*(*pci_decls$pcltable)[(pc)-1].a.def).frameoffset))-1];
            ++(pc);
        }
        break;
    case 16:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 17:;
        {
            stack[(++(sp))-1] = (i64)(u32)stack[((fp + (*(*pci_decls$pcltable)[(pc)-1].a.def).frameoffset))-1];
            ++(pc);
        }
        break;
    case 18:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 19:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 20:;
        {
            (*pstack)[(++(sp))-1] = &stack[((fp + (*(*pci_decls$pcltable)[(pc)-1].a.def).frameoffset))-1];
            ++(pc);
        }
        break;
    case 21:;
        {
            pi8 = (i8 *)(*(*pci_decls$pcltable)[(pc)-1].a.def).staticaddr;
            (*pi8) = stack[((sp)--)-1];
            ++(pc);
        }
        break;
    case 22:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 23:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 24:;
        {
            pi64 = (i64 *)(*(*pci_decls$pcltable)[(pc)-1].a.def).staticaddr;
            (*pi64) = stack[((sp)--)-1];
            ++(pc);
        }
        break;
    case 25:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 26:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 27:;
        {
            pi8 = (i8 *)(*(*pci_decls$pcltable)[(pc)-1].a.def).staticaddr;
            memcpy(pi8,(*pstack)[((sp)--)-1],(u64)(i64)(*pci_decls$pcltable)[(pc)-1].memsize);
            ++(pc);
        }
        break;
    case 31:;
    case 30:;
    case 29:;
    case 28:;
        {
            stack[((fp + (*(*pci_decls$pcltable)[(pc)-1].a.def).frameoffset))-1] = stack[((sp)--)-1];
            ++(pc);
        }
        break;
    case 32:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 33:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 34:;
        {
            pi8 = (i8 *)&stack[((fp + (*(*pci_decls$pcltable)[(pc)-1].a.def).frameoffset))-1];
            memcpy(pi8,(*pstack)[((sp)--)-1],(u64)(i64)(*pci_decls$pcltable)[(pc)-1].memsize);
            ++(pc);
        }
        break;
    case 35:;
    case 36:;
        {
            stack[(++(sp))-1] = (*pci_decls$pcltable)[(pc)-1].a.value;
            ++(pc);
        }
        break;
    case 37:;
        {
            stack[(++(sp))-1] = pci_execc$putr32((*pci_decls$pcltable)[(pc)-1].a.xvalue32);
            ++(pc);
        }
        break;
    case 38:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 39:;
        {
            stack[(++(sp))-1] = (i64)(*(*pci_decls$pcltable)[(pc)-1].a.def).staticaddr;
            ++(pc);
        }
        break;
    case 40:;
        {
            stack[(++(sp))-1] = (i64)&stack[((fp + (*(*pci_decls$pcltable)[(pc)-1].a.def).frameoffset))-1];
            ++(pc);
        }
        break;
    case 41:;
        {
            stack[(++(sp))-1] = (i64)pci_decls$labeltable[((*pci_decls$pcltable)[(pc)-1].a.labelno)-1];
            ++(pc);
        }
        break;
    case 42:;
        {
            stack[(++(sp))-1] = (*(*pci_decls$pcltable)[(pc)-1].a.def).pcindex;
            ++(pc);
        }
        break;
    case 43:;
        {
            d = (*pci_decls$pcltable)[(pc)-1].a.def;
            (*pstack)[(++(sp))-1] = pci_support$getdllfnptr((*d).libindex);
            ++(pc);
        }
        break;
    case 44:;
        {
            pi8 = (i8 *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (i64)(*pi8);
            ++(pc);
        }
        break;
    case 45:;
        {
            pi16 = (i16 *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (i64)(*pi16);
            ++(pc);
        }
        break;
    case 46:;
        {
            pi32 = (i32 *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (i64)(*pi32);
            ++(pc);
        }
        break;
    case 47:;
        {
            pi64 = (i64 *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (*pi64);
            ++(pc);
        }
        break;
    case 48:;
        {
            pu8 = (byte *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (i64)(*pu8);
            ++(pc);
        }
        break;
    case 49:;
        {
            pu16 = (u16 *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (i64)(*pu16);
            ++(pc);
        }
        break;
    case 50:;
        {
            pu32 = (u32 *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (i64)(*pu32);
            ++(pc);
        }
        break;
    case 51:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 52:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 53:;
        {
            ++(pc);
        }
        break;
    case 54:;
        {
            x = ((stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale) + (i64)(*pci_decls$pcltable)[(pc)-1].x.poffset);
            pi8 = (i8 *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (i64)(*(pi8 + x));
            ++(pc);
        }
        break;
    case 55:;
        {
            x = ((stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale) + (i64)(*pci_decls$pcltable)[(pc)-1].x.poffset);
            pi16 = (i16 *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (i64)(*(i16 *)((byte *)pi16 + x));
            ++(pc);
        }
        break;
    case 56:;
        {
            x = ((stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale) + (i64)(*pci_decls$pcltable)[(pc)-1].x.poffset);
            pi32 = (i32 *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (i64)(*(i32 *)((byte *)pi32 + x));
            ++(pc);
        }
        break;
    case 57:;
        {
            x = ((stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale) + (i64)(*pci_decls$pcltable)[(pc)-1].x.poffset);
            pi64 = (i64 *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (*(i64 *)((byte *)pi64 + x));
            ++(pc);
        }
        break;
    case 58:;
        {
            x = ((stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale) + (i64)(*pci_decls$pcltable)[(pc)-1].x.poffset);
            pu8 = (byte *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (i64)(*(pu8 + x));
            ++(pc);
        }
        break;
    case 59:;
        {
            x = ((stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale) + (i64)(*pci_decls$pcltable)[(pc)-1].x.poffset);
            pu16 = (u16 *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (i64)(*(u16 *)((byte *)pu16 + x));
            ++(pc);
        }
        break;
    case 60:;
        {
            x = ((stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale) + (i64)(*pci_decls$pcltable)[(pc)-1].x.poffset);
            pu32 = (u32 *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (i64)(*(u32 *)((byte *)pu32 + x));
            ++(pc);
        }
        break;
    case 61:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 62:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 63:;
        {
            x = ((stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale) + (i64)(*pci_decls$pcltable)[(pc)-1].x.poffset);
            pu8 = (byte *)(*pstack)[(sp)-1];
            (*pstack)[(sp)-1] = (pu8 + x);
            ++(pc);
        }
        break;
    case 64:;
        {
            pi8 = (i8 *)(*pstack)[((sp)--)-1];
            (*pi8) = stack[((sp)--)-1];
            ++(pc);
        }
        break;
    case 65:;
        {
            pi16 = (i16 *)(*pstack)[((sp)--)-1];
            (*pi16) = stack[((sp)--)-1];
            ++(pc);
        }
        break;
    case 66:;
        {
            pi32 = (i32 *)(*pstack)[((sp)--)-1];
            (*pi32) = stack[((sp)--)-1];
            ++(pc);
        }
        break;
    case 67:;
        {
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            (*pi64) = stack[((sp)--)-1];
            ++(pc);
        }
        break;
    case 68:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 69:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 70:;
        {
            pi8 = (i8 *)(*pstack)[((sp)--)-1];
            memcpy(pi8,(*pstack)[((sp)--)-1],(u64)(i64)(*pci_decls$pcltable)[(pc)-1].memsize);
            ++(pc);
        }
        break;
    case 71:;
        {
            x = ((stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale) + (i64)(*pci_decls$pcltable)[(pc)-1].x.poffset);
            pi8 = (i8 *)(*pstack)[((sp)--)-1];
            (*(pi8 + x)) = stack[((sp)--)-1];
            ++(pc);
        }
        break;
    case 72:;
        {
            x = ((stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale) + (i64)(*pci_decls$pcltable)[(pc)-1].x.poffset);
            pi16 = (i16 *)(*pstack)[((sp)--)-1];
            (*(i16 *)((byte *)pi16 + x)) = stack[((sp)--)-1];
            ++(pc);
        }
        break;
    case 73:;
        {
            x = ((stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale) + (i64)(*pci_decls$pcltable)[(pc)-1].x.poffset);
            pi32 = (i32 *)(*pstack)[((sp)--)-1];
            (*(i32 *)((byte *)pi32 + x)) = stack[((sp)--)-1];
            ++(pc);
        }
        break;
    case 74:;
        {
            x = ((stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale) + (i64)(*pci_decls$pcltable)[(pc)-1].x.poffset);
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            (*(i64 *)((byte *)pi64 + x)) = stack[((sp)--)-1];
            ++(pc);
        }
        break;
    case 75:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 76:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 77:;
        {
            x = ((stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale) + (i64)(*pci_decls$pcltable)[(pc)-1].x.poffset);
            pi8 = (i8 *)(*pstack)[((sp)--)-1];
            memcpy((pi8 + x),(*pstack)[((sp)--)-1],(u64)(i64)(*pci_decls$pcltable)[(pc)-1].memsize);
            ++(pc);
        }
        break;
    case 78:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 79:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 80:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 81:;
        {
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            pi64b = (i64 *)(*pstack)[((sp)--)-1];
            {i64 temp = (*pi64); (*pi64) = (*pi64b); (*pi64b) = temp; };
            ++(pc);
        }
        break;
    case 82:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 83:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 84:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 85:;
        {
            pi32 = (i32 *)(*pstack)[((sp)--)-1];
            (*pi32) = (i64)0;
            ++(pc);
        }
        break;
    case 86:;
        {
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            (*pi64) = (i64)0;
            ++(pc);
        }
        break;
    case 87:;
        {
            pi8 = (i8 *)(*pstack)[((sp)--)-1];
            memset(pi8,(i32)(i64)0,(u64)(i64)(*pci_decls$pcltable)[(pc)-1].memsize);
            ++(pc);
        }
        break;
    case 88:;
        {
            sp -= (i64)2;
            x = (i64)(stack[((sp + (i64)1))-1] == stack[((sp + (i64)2))-1]);
            goto L4 ;
;
        }
        break;
    case 89:;
        {
            goto L3 ;
;
        }
        break;
    case 90:;
        {
            goto L3 ;
;
        }
        break;
    case 91:;
        {
            goto L3 ;
;
        }
        break;
    case 92:;
    case 93:;
        {
            sp -= (i64)2;
            x = (i64)(stack[((sp + (i64)1))-1] != stack[((sp + (i64)2))-1]);
            //dojumpcond:
L4 :;
;
            if (!!(x)) {
                pc = (i64)pci_decls$labeltable[((*pci_decls$pcltable)[(pc)-1].a.labelno)-1];
            }
            else {
                sp += (i64)(*pci_decls$pcltable)[(pc)-1].x.popone;
                ++(pc);
            }
;
        }
        break;
    case 94:;
        {
            goto L3 ;
;
        }
        break;
    case 95:;
        {
            goto L3 ;
;
        }
        break;
    case 96:;
        {
            sp -= (i64)2;
            x = (i64)(stack[((sp + (i64)1))-1] < stack[((sp + (i64)2))-1]);
            goto L4 ;
;
        }
        break;
    case 97:;
        {
            sp -= (i64)2;
            x = (i64)((*ustack)[((sp + (i64)1))-1] < (*ustack)[((sp + (i64)2))-1]);
            goto L4 ;
;
        }
        break;
    case 98:;
        {
            goto L3 ;
;
        }
        break;
    case 99:;
        {
            goto L3 ;
;
        }
        break;
    case 100:;
        {
            sp -= (i64)2;
            x = (i64)(stack[((sp + (i64)1))-1] <= stack[((sp + (i64)2))-1]);
            goto L4 ;
;
        }
        break;
    case 101:;
        {
            sp -= (i64)2;
            x = (i64)((*ustack)[((sp + (i64)1))-1] <= (*ustack)[((sp + (i64)2))-1]);
            goto L4 ;
;
        }
        break;
    case 102:;
        {
            sp -= (i64)2;
            x = (i64)((*xstack)[((sp + (i64)1))-1] <= (*xstack)[((sp + (i64)2))-1]);
            goto L4 ;
;
        }
        break;
    case 103:;
        {
            goto L3 ;
;
        }
        break;
    case 104:;
        {
            sp -= (i64)2;
            x = (i64)(stack[((sp + (i64)1))-1] >= stack[((sp + (i64)2))-1]);
            goto L4 ;
;
        }
        break;
    case 105:;
        {
            sp -= (i64)2;
            x = (i64)((*ustack)[((sp + (i64)1))-1] >= (*ustack)[((sp + (i64)2))-1]);
            goto L4 ;
;
        }
        break;
    case 106:;
        {
            goto L3 ;
;
        }
        break;
    case 107:;
        {
            goto L3 ;
;
        }
        break;
    case 108:;
        {
            sp -= (i64)2;
            x = (i64)(stack[((sp + (i64)1))-1] > stack[((sp + (i64)2))-1]);
            goto L4 ;
;
        }
        break;
    case 109:;
        {
            sp -= (i64)2;
            x = (i64)((*ustack)[((sp + (i64)1))-1] > (*ustack)[((sp + (i64)2))-1]);
            goto L4 ;
;
        }
        break;
    case 110:;
        {
            goto L3 ;
;
        }
        break;
    case 111:;
        {
            goto L3 ;
;
        }
        break;
    case 112:;
    case 113:;
    case 114:;
        {
            if (!!(stack[((sp)--)-1])) {
                pc = (i64)pci_decls$labeltable[((*pci_decls$pcltable)[(pc)-1].a.labelno)-1];
            }
            else {
                ++(pc);
            }
;
        }
        break;
    case 115:;
    case 116:;
    case 117:;
        {
            if ((stack[((sp)--)-1] == (i64)0)) {
                pc = (i64)pci_decls$labeltable[((*pci_decls$pcltable)[(pc)-1].a.labelno)-1];
            }
            else {
                ++(pc);
            }
;
        }
        break;
    case 118:;
        {
            --(sp);
            stack[(sp)-1] += stack[((sp + (i64)1))-1];
            ++(pc);
        }
        break;
    case 119:;
        {
            --(sp);
            (*xstack)[(sp)-1] += (*xstack)[((sp + (i64)1))-1];
            ++(pc);
        }
        break;
    case 120:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 121:;
        {
            --(sp);
            stack[(sp)-1] -= stack[((sp + (i64)1))-1];
            ++(pc);
        }
        break;
    case 122:;
        {
            --(sp);
            (*xstack)[(sp)-1] -= (*xstack)[((sp + (i64)1))-1];
            ++(pc);
        }
        break;
    case 123:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 124:;
        {
            --(sp);
            stack[(sp)-1] *= stack[((sp + (i64)1))-1];
            ++(pc);
        }
        break;
    case 125:;
        {
            --(sp);
            (*xstack)[(sp)-1] *= (*xstack)[((sp + (i64)1))-1];
            ++(pc);
        }
        break;
    case 126:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 127:;
        {
            stack[(sp)-1] = -(stack[(sp)-1]);
            ++(pc);
        }
        break;
    case 128:;
        {
            (*xstack)[(sp)-1] = -((*xstack)[(sp)-1]);
            ++(pc);
        }
        break;
    case 129:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 130:;
        {
            stack[(sp)-1] = m$llabs(stack[(sp)-1]);
            ++(pc);
        }
        break;
    case 131:;
        {
            (*xstack)[(sp)-1] = fabs((*xstack)[(sp)-1]);
            ++(pc);
        }
        break;
    case 132:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 133:;
        {
            stack[(sp)-1] *= stack[(sp)-1];
            ++(pc);
        }
        break;
    case 134:;
        {
            (*xstack)[(sp)-1] *= (*xstack)[(sp)-1];
            ++(pc);
        }
        break;
    case 135:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 136:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 137:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 138:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 139:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 140:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 141:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 142:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 143:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 144:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 145:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 146:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 147:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 148:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 149:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 150:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 151:;
        {
            --(sp);
            (*xstack)[(sp)-1] /= (*xstack)[((sp + (i64)1))-1];
            ++(pc);
        }
        break;
    case 152:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 153:;
        {
            --(sp);
            stack[(sp)-1] = (stack[(sp)-1] / stack[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 154:;
        {
            --(sp);
            (*ustack)[(sp)-1] = ((*ustack)[(sp)-1] / (*ustack)[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 155:;
        {
            --(sp);
            stack[(sp)-1] = (stack[(sp)-1] % stack[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 156:;
        {
            --(sp);
            (*ustack)[(sp)-1] = ((*ustack)[(sp)-1] % (*ustack)[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 157:;
        {
            a = (stack[((sp - (i64)1))-1] / stack[(sp)-1]);
            b = (stack[((sp - (i64)1))-1] % stack[(sp)-1]);
            stack[((sp - (i64)1))-1] = a;
            stack[(sp)-1] = b;
            ++(pc);
        }
        break;
    case 158:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 159:;
        {
            --(sp);
            stack[(sp)-1] = (stack[(sp)-1] >> stack[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 160:;
        {
            --(sp);
            (*ustack)[(sp)-1] = ((*ustack)[(sp)-1] >> (i64)(*ustack)[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 161:;
        {
            --(sp);
            stack[(sp)-1] = (i64)(stack[(sp)-1] == stack[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 163:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 164:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 165:;
        {
            --(sp);
            stack[(sp)-1] = (i64)(stack[(sp)-1] != stack[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 167:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 168:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 169:;
        {
            --(sp);
            stack[(sp)-1]=(stack[(sp)-1]<stack[((sp + (i64)1))-1]?stack[(sp)-1]:stack[((sp + (i64)1))-1]);
;
            ++(pc);
        }
        break;
    case 170:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 171:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 172:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 173:;
        {
            --(sp);
            stack[(sp)-1]=(stack[(sp)-1]>stack[((sp + (i64)1))-1]?stack[(sp)-1]:stack[((sp + (i64)1))-1]);
;
            ++(pc);
        }
        break;
    case 174:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 175:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 176:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 177:;
        {
            --(sp);
            stack[(sp)-1] = (i64)(stack[(sp)-1] < stack[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 178:;
        {
            --(sp);
            stack[(sp)-1] = (i64)((*ustack)[(sp)-1] <= (*ustack)[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 179:;
        {
            --(sp);
            stack[(sp)-1] = (i64)(stack[(sp)-1] < stack[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 180:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 181:;
        {
            --(sp);
            stack[(sp)-1] = (i64)(stack[(sp)-1] <= stack[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 182:;
        {
            --(sp);
            (*ustack)[(sp)-1] = (u64)((*ustack)[(sp)-1] <= (*ustack)[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 183:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 184:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 185:;
        {
            --(sp);
            stack[(sp)-1] = (i64)(stack[(sp)-1] >= stack[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 186:;
        {
            --(sp);
            (*ustack)[(sp)-1] = (u64)((*ustack)[(sp)-1] >= (*ustack)[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 187:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 188:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 189:;
        {
            --(sp);
            stack[(sp)-1] = (i64)(stack[(sp)-1] > stack[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 190:;
        {
            --(sp);
            stack[(sp)-1] = (i64)((*ustack)[(sp)-1] > (*ustack)[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 191:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 192:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 193:;
        {
            --(sp);
            stack[(sp)-1] = msysc$m_power_i64(stack[((sp + (i64)1))-1],stack[(sp)-1]);
            ++(pc);
        }
        break;
    case 194:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 195:;
        {
            --(sp);
            (*xstack)[(sp)-1] = pow((*xstack)[(sp)-1],(*xstack)[((sp + (i64)1))-1]);
            ++(pc);
        }
        break;
    case 196:;
        {
            x = stack[((sp)--)-1];
            pi8 = (i8 *)(*pstack)[((sp)--)-1];
            (*pi8) += (i8)x;
            ++(pc);
        }
        break;
    case 197:;
        {
            x = stack[((sp)--)-1];
            pi16 = (i16 *)(*pstack)[((sp)--)-1];
            (*pi16) += (i16)x;
            ++(pc);
        }
        break;
    case 198:;
        {
            x = stack[((sp)--)-1];
            pi32 = (i32 *)(*pstack)[((sp)--)-1];
            (*pi32) += (i32)x;
            ++(pc);
        }
        break;
    case 199:;
        {
            x = stack[((sp)--)-1];
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            (*pi64) += x;
            ++(pc);
        }
        break;
    case 200:;
        {
            xx = (r64)stack[((sp)--)-1];
            pr64 = (r64 *)(*pstack)[((sp)--)-1];
            (*pr64) += xx;
            ++(pc);
        }
        break;
    case 201:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 202:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 203:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 204:;
        {
            x = stack[((sp)--)-1];
            pi32 = (i32 *)(*pstack)[((sp)--)-1];
            (*pi32) -= (i32)x;
            ++(pc);
        }
        break;
    case 205:;
        {
            x = stack[((sp)--)-1];
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            (*pi64) -= x;
            ++(pc);
        }
        break;
    case 206:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 207:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 208:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 209:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 210:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 211:;
        {
            x = stack[((sp)--)-1];
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            (*pi64) *= x;
            ++(pc);
        }
        break;
    case 212:;
        {
            xx = (*xstack)[((sp)--)-1];
            pr64 = (r64 *)(*pstack)[((sp)--)-1];
            (*pr64) *= xx;
            ++(pc);
        }
        break;
    case 213:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 214:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 215:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 216:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 217:;
        {
            x = stack[((sp)--)-1];
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            (*pi64) = ((*pi64) / x);
            ++(pc);
        }
        break;
    case 218:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 219:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 220:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 221:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 222:;
        {
            xx = (*xstack)[((sp)--)-1];
            pr64 = (r64 *)(*pstack)[((sp)--)-1];
            (*pr64) = ((*pr64) / xx);
            ++(pc);
        }
        break;
    case 223:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 224:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 225:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 226:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 227:;
        {
            x = stack[((sp)--)-1];
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            (*pi64)=((*pi64)<x?(*pi64):x);
;
            ++(pc);
        }
        break;
    case 228:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 229:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 230:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 231:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 232:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 233:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 234:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 235:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 236:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 237:;
        {
            x = stack[((sp)--)-1];
            pu64 = (u64 *)(*pstack)[((sp)--)-1];
            (*pu64)=((*pu64)>(u64)x?(*pu64):(u64)x);
;
            ++(pc);
        }
        break;
    case 238:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 239:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 240:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 241:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 242:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 243:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 244:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 245:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 246:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 247:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 248:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 249:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 250:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 251:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 252:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 253:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 254:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 255:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 256:;
        {
            pi8 = (i8 *)(*pstack)[((sp)--)-1];
            (*pi8) += (i8)(*pci_decls$pcltable)[(pc)-1].x.step;
            ++(pc);
        }
        break;
    case 257:;
        {
            pi16 = (i16 *)(*pstack)[((sp)--)-1];
            (*pi16) += (i16)(*pci_decls$pcltable)[(pc)-1].x.step;
            ++(pc);
        }
        break;
    case 258:;
        {
            pi32 = (i32 *)(*pstack)[((sp)--)-1];
            (*pi32) += (i32)(*pci_decls$pcltable)[(pc)-1].x.step;
            ++(pc);
        }
        break;
    case 259:;
        {
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            (*pi64) += (*pci_decls$pcltable)[(pc)-1].x.step;
            ++(pc);
        }
        break;
    case 260:;
        {
            pi8 = (i8 *)(*pstack)[(sp)-1];
            (*pi8) += (i8)(*pci_decls$pcltable)[(pc)-1].x.step;
            stack[(sp)-1] = (i64)(*pi8);
            ++(pc);
        }
        break;
    case 261:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 262:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 263:;
        {
            pi64 = (i64 *)(*pstack)[(sp)-1];
            (*pi64) += (*pci_decls$pcltable)[(pc)-1].x.step;
            stack[(sp)-1] = (*pi64);
            ++(pc);
        }
        break;
    case 264:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 265:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 266:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 267:;
        {
            pi64 = (i64 *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (*pi64);
            (*pi64) += (*pci_decls$pcltable)[(pc)-1].x.step;
            ++(pc);
        }
        break;
    case 268:;
        {
            pi8 = (i8 *)(*pstack)[((sp)--)-1];
            (*pi8) -= (i8)(*pci_decls$pcltable)[(pc)-1].x.step;
            ++(pc);
        }
        break;
    case 269:;
        {
            pi16 = (i16 *)(*pstack)[((sp)--)-1];
            (*pi16) -= (i16)(*pci_decls$pcltable)[(pc)-1].x.step;
            ++(pc);
        }
        break;
    case 270:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 271:;
        {
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            (*pi64) -= (*pci_decls$pcltable)[(pc)-1].x.step;
            ++(pc);
        }
        break;
    case 272:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 273:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 274:;
        {
            pi32 = (i32 *)(*pstack)[(sp)-1];
            (*pi32) -= (i32)(*pci_decls$pcltable)[(pc)-1].x.step;
            stack[(sp)-1] = (i64)(*pi32);
            ++(pc);
        }
        break;
    case 275:;
        {
            pi64 = (i64 *)(*pstack)[(sp)-1];
            (*pi64) -= (*pci_decls$pcltable)[(pc)-1].x.step;
            stack[(sp)-1] = (*pi64);
            ++(pc);
        }
        break;
    case 276:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 277:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 278:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 279:;
        {
            pi64 = (i64 *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (*pi64);
            (*pi64) -= (*pci_decls$pcltable)[(pc)-1].x.step;
            ++(pc);
        }
        break;
    case 280:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 281:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 282:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 283:;
        {
            x = stack[((sp)--)-1];
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            (*pi64) &= x;
            ++(pc);
        }
        break;
    case 284:;
        {
            x = stack[((sp)--)-1];
            pi8 = (i8 *)(*pstack)[((sp)--)-1];
            (*pi8) |= (i8)x;
            ++(pc);
        }
        break;
    case 285:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 286:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 287:;
        {
            x = stack[((sp)--)-1];
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            (*pi64) |= x;
            ++(pc);
        }
        break;
    case 288:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 289:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 290:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 291:;
        {
            x = stack[((sp)--)-1];
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            (*pi64) ^= x;
            ++(pc);
        }
        break;
    case 292:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 293:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 294:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 295:;
        {
            x = stack[((sp)--)-1];
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            (*pi64) <<= x;
            ++(pc);
        }
        break;
    case 296:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 297:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 298:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 299:;
        {
            x = stack[((sp)--)-1];
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            (*pi64) >>= x;
            ++(pc);
        }
        break;
    case 300:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 301:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 302:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 303:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 304:;
        {
            pi64 = (i64 *)(*(*pci_decls$pcltable)[((pc + (i64)1))-1].a.def).staticaddr;
            (*pi64) += (*pci_decls$pcltable)[(pc)-1].x.step;
            if (((*pi64) <= (*(i64 *)(*(*pci_decls$pcltable)[((pc + (i64)2))-1].a.def).staticaddr))) {
                pc = (i64)pci_decls$labeltable[((*pci_decls$pcltable)[(pc)-1].a.labelno)-1];
            }
            else {
                pc += (i64)3;
            }
;
        }
        break;
    case 305:;
        {
            goto L3 ;
;
        }
        break;
    case 306:;
        {
            pi64 = &stack[((fp + (*(*pci_decls$pcltable)[((pc + (i64)1))-1].a.def).frameoffset))-1];
            (*pi64) += (*pci_decls$pcltable)[(pc)-1].x.step;
            if (((*pi64) <= (*(i64 *)(*(*pci_decls$pcltable)[((pc + (i64)2))-1].a.def).staticaddr))) {
                pc = (i64)pci_decls$labeltable[((*pci_decls$pcltable)[(pc)-1].a.labelno)-1];
            }
            else {
                pc += (i64)3;
            }
;
        }
        break;
    case 307:;
        {
            pi64 = &stack[((fp + (*(*pci_decls$pcltable)[((pc + (i64)1))-1].a.def).frameoffset))-1];
            (*pi64) += (*pci_decls$pcltable)[(pc)-1].x.step;
            if (((*pi64) <= stack[((fp + (*(*pci_decls$pcltable)[((pc + (i64)2))-1].a.def).frameoffset))-1])) {
                pc = (i64)pci_decls$labeltable[((*pci_decls$pcltable)[(pc)-1].a.labelno)-1];
            }
            else {
                pc += (i64)3;
            }
;
        }
        break;
    case 308:;
        {
            goto L3 ;
;
        }
        break;
    case 309:;
        {
            pi64 = &stack[((fp + (*(*pci_decls$pcltable)[((pc + (i64)1))-1].a.def).frameoffset))-1];
            (*pi64) += (*pci_decls$pcltable)[(pc)-1].x.step;
            if (((*pi64) <= (*pci_decls$pcltable)[((pc + (i64)2))-1].a.value)) {
                pc = (i64)pci_decls$labeltable[((*pci_decls$pcltable)[(pc)-1].a.labelno)-1];
            }
            else {
                pc += (i64)3;
            }
;
        }
        break;
    case 310:;
        {
            goto L3 ;
;
        }
        break;
    case 311:;
        {
            goto L3 ;
;
        }
        break;
    case 312:;
        {
            goto L3 ;
;
        }
        break;
    case 313:;
        {
            goto L3 ;
;
        }
        break;
    case 314:;
        {
            goto L3 ;
;
        }
        break;
    case 315:;
        {
            pi64 = &stack[((fp + (*(*pci_decls$pcltable)[((pc + (i64)1))-1].a.def).frameoffset))-1];
            (*pi64) -= (*pci_decls$pcltable)[(pc)-1].x.step;
            if (((*pi64) >= (*pci_decls$pcltable)[((pc + (i64)2))-1].a.value)) {
                pc = (i64)pci_decls$labeltable[((*pci_decls$pcltable)[(pc)-1].a.labelno)-1];
            }
            else {
                pc += (i64)3;
            }
;
        }
        break;
    case 316:;
        {
            goto L3 ;
;
        }
        break;
    case 317:;
        {
            pi64 = &stack[((fp + (*(*pci_decls$pcltable)[((pc + (i64)1))-1].a.def).frameoffset))-1];
            --((*pi64));
            if (!!((*pi64))) {
                pc = (i64)pci_decls$labeltable[((*pci_decls$pcltable)[(pc)-1].a.labelno)-1];
            }
            else {
                pc += (i64)2;
            }
;
        }
        break;
    case 318:;
        {
            (*xstack)[(sp)-1] = (r64)stack[(sp)-1];
            ++(pc);
        }
        break;
    case 319:;
        {
            (*xstack)[(sp)-1] = (r64)(*ustack)[(sp)-1];
            ++(pc);
        }
        break;
    case 320:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 321:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 322:;
        {
            stack[(sp)-1] = (i64)(*xstack)[(sp)-1];
            ++(pc);
        }
        break;
    case 323:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 324:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 325:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 326:;
        {
            (*xstack)[(sp)-1] = (r64)pci_execc$getr32(stack[(sp)-1]);
            ++(pc);
        }
        break;
    case 327:;
        {
            stack[(sp)-1] = (i64)msysc$m_tp_r64toi64((r32)(*xstack)[(sp)-1]);
            ++(pc);
        }
        break;
    case 328:;
        {
            stack[(sp)-1] = (i64)(i8)stack[(sp)-1];
            ++(pc);
        }
        break;
    case 329:;
        {
            stack[(sp)-1] = (i64)(i16)stack[(sp)-1];
            ++(pc);
        }
        break;
    case 330:;
        {
            stack[(sp)-1] = (i64)(i32)stack[(sp)-1];
            ++(pc);
        }
        break;
    case 331:;
        {
            stack[(sp)-1] = (i64)(byte)stack[(sp)-1];
            ++(pc);
        }
        break;
    case 332:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 333:;
        {
            stack[(sp)-1] = (i64)(u32)stack[(sp)-1];
            ++(pc);
        }
        break;
    case 334:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 335:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 336:;
        {
            stack[(sp)-1] = (i64)(i32)stack[(sp)-1];
            ++(pc);
        }
        break;
    case 337:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 338:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 339:;
        {
            stack[(sp)-1] = (i64)(u32)stack[(sp)-1];
            ++(pc);
        }
        break;
    case 340:;
        {
            (*xstack)[(sp)-1] = sqrt((*xstack)[(sp)-1]);
            ++(pc);
        }
        break;
    case 341:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 342:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 343:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 344:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 345:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 346:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 347:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 348:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 349:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 350:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 351:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 352:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 353:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 354:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 355:;
        {
            --(sp);
            stack[(sp)-1] &= stack[((sp + (i64)1))-1];
            ++(pc);
        }
        break;
    case 356:;
        {
            --(sp);
            stack[(sp)-1] |= stack[((sp + (i64)1))-1];
            ++(pc);
        }
        break;
    case 357:;
        {
            --(sp);
            stack[(sp)-1] ^= stack[((sp + (i64)1))-1];
            ++(pc);
        }
        break;
    case 358:;
        {
            --(sp);
            stack[(sp)-1] <<= stack[((sp + (i64)1))-1];
            ++(pc);
        }
        break;
    case 359:;
        {
            stack[(++(sp))-1] = fp;
            fp = sp;
            sp += (i64)(*pci_decls$pcltable)[(pc)-1].x.nlocals;
            if ((u64)0u) {
                stack[(++(sp))-1] = (i64)1457115631958533000;
            }
;
            ++(pc);
        }
        break;
    case 360:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 361:;
        {
            n = (stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale);
            pu64 = (u64 *)(*pstack)[((sp)--)-1];
            (*pu64) += (u64)n;
            ++(pc);
        }
        break;
    case 362:;
        {
            n = (stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale);
            pu64 = (u64 *)(*pstack)[((sp)--)-1];
            (*pu64) -= (u64)n;
            ++(pc);
        }
        break;
    case 363:;
        {
            x = ((stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale) + (i64)(*pci_decls$pcltable)[(pc)-1].x.poffset);
            pi64 = (i64 *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (i64)((byte *)pi64 + x);
            ++(pc);
        }
        break;
    case 364:;
        {
            x = ((stack[((sp)--)-1] * (i64)(*pci_decls$pcltable)[(pc)-1].x.scale) + (i64)(*pci_decls$pcltable)[(pc)-1].x.poffset);
            pi64 = (i64 *)(*pstack)[(sp)-1];
            stack[(sp)-1] = (i64)((byte *)pi64 - x);
            ++(pc);
        }
        break;
    case 365:;
        {
            --(sp);
            stack[(sp)-1] = (((byte *)(*pstack)[(sp)-1] - (byte *)(*pstack)[((sp + (i64)1))-1]) / (i64)(*pci_decls$pcltable)[(pc)-1].x.scale);
            ++(pc);
        }
        break;
    case 366:;
        {
            stack[(sp)-1] = ~(stack[(sp)-1]);
            ++(pc);
        }
        break;
    case 367:;
        {
            stack[(sp)-1] = (i64)!(!!(stack[(sp)-1]));
            ++(pc);
        }
        break;
    case 368:;
        {
            stack[(sp)-1] = (i64)!!(stack[(sp)-1]);
            ++(pc);
        }
        break;
    case 369:;
        {
            n = stack[((sp)--)-1];
            if ((n >= (i64)(*pci_decls$pcltable)[(pc)-1].x.swmin && n <= (i64)(*pci_decls$pcltable)[(pc)-1].x.swmax)) {
                pc = (((pc + n) - (i64)(*pci_decls$pcltable)[(pc)-1].x.swmin) + (i64)2);
                pc = (i64)pci_decls$labeltable[((*pci_decls$pcltable)[(pc)-1].a.labelno)-1];
            }
            else {
                pc = (i64)pci_decls$labeltable[((*pci_decls$pcltable)[((pc + (i64)1))-1].a.labelno)-1];
            }
;
        }
        break;
    case 370:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 371:;
        {
            n = stack[((sp)--)-1];
            stack[(sp)-1] = ((stack[(sp)-1] >> n) & (i64)1);
            ++(pc);
        }
        break;
    case 372:;
        {
            n = stack[((sp)--)-1];
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            x = stack[((sp)--)-1];
            (*pi64) = (((*pi64) & ~(((i64)1 << n))) | ((x & (i64)1) << n));
            ++(pc);
        }
        break;
    case 373:;
        {
            j = stack[((sp)--)-1];
            i = stack[((sp)--)-1];
            if ((i > j)) {
                {i64 temp = i; i = j; j = temp; };
            }
;
            stack[(sp)-1] = ((stack[(sp)-1] >> i) & (i64)~(((u64)18446744073709551615u << ((j - i) + (i64)1))));
            ++(pc);
        }
        break;
    case 374:;
        {
            j = stack[((sp)--)-1];
            i = stack[((sp)--)-1];
            if ((i > j)) {
                {i64 temp = i; i = j; j = temp; };
            }
;
            pi64 = (i64 *)(*pstack)[((sp)--)-1];
            x = stack[((sp)--)-1];
            pci_support$storebit(pi64,i,j,x);
            ++(pc);
        }
        break;
    case 375:;
        {
            --(sp);
            ++(pc);
        }
        break;
    case 376:;
        {
            stack[((sp + (i64)1))-1] = stack[(sp)-1];
            ++(sp);
            ++(pc);
        }
        break;
    case 377:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 378:;
        {
            {i64 temp = stack[(sp)-1]; stack[(sp)-1] = stack[((sp - (i64)1))-1]; stack[((sp - (i64)1))-1] = temp; };
            ++(pc);
        }
        break;
    case 379:;
    case 380:;
        {
            if ((u64)0u) {
                if ((sp > (i64)69990)) {
                    pci_support$pcerror3((byte*)"Stack overflow",(byte*)"",pc);
                }
;
            }
;
            stack[(++(sp))-1] = (pc + (i64)1);
            pc = (*(*pci_decls$pcltable)[(pc)-1].a.def).pcindex;
        }
        break;
    case 381:;
        {
            if ((stack[(sp)-1] > (i64)1000000)) {
                pi64 = (i64 *)(*pstack)[((sp)--)-1];
                n = (i64)(*pci_decls$pcltable)[(pc)-1].x.nargs;
                sp -= (n - (i64)1);
                pci_support$docalldll((u64)0u,pi64,(i64 (*)[])&stack[(sp)-1],n,(i64)(*pci_decls$pcltable)[(pc)-1].x.nvars,(i64)0);
                --(sp);
                ++(pc);
            }
            else {
                x = (pc + (i64)1);
                pc = stack[(sp)-1];
                stack[(sp)-1] = x;
            }
;
        }
        break;
    case 382:;
        {
            if ((stack[(sp)-1] > (i64)1000000)) {
                pi64 = (i64 *)(*pstack)[((sp)--)-1];
                n = (i64)(*pci_decls$pcltable)[(pc)-1].x.nargs;
                sp -= (n - (i64)1);
                x = pci_support$docalldll((u64)0u,pi64,(i64 (*)[])&stack[(sp)-1],n,(i64)(*pci_decls$pcltable)[(pc)-1].x.nvars,(i64)1);
                stack[(sp)-1] = x;
                ++(pc);
            }
            else {
                x = (pc + (i64)1);
                pc = stack[(sp)-1];
                stack[(sp)-1] = x;
            }
;
        }
        break;
    case 383:;
        {
            n = (i64)(*pci_decls$pcltable)[(pc)-1].x.nargs;
            sp -= (n - (i64)1);
            pci_support$docalldll((u64)(*(*pci_decls$pcltable)[(pc)-1].a.def).importno,0,(i64 (*)[])&stack[(sp)-1],n,(i64)(*pci_decls$pcltable)[(pc)-1].x.nvars,(i64)0);
            --(sp);
            ++(pc);
        }
        break;
    case 384:;
        {
            n = (i64)(*pci_decls$pcltable)[(pc)-1].x.nargs;
            sp -= (n - (i64)1);
            x = pci_support$docalldll((u64)(*(*pci_decls$pcltable)[(pc)-1].a.def).importno,0,(i64 (*)[])&stack[(sp)-1],n,(i64)(*pci_decls$pcltable)[(pc)-1].x.nvars,(i64)1);
            stack[(sp)-1] = x;
            ++(pc);
        }
        break;
    case 385:;
        {
            pi8 = (i8 *)(*pstack)[((fp + (i64)-2))-1];
            memcpy(pi8,(*pstack)[(sp)-1],(u64)(i64)(*pci_decls$pcltable)[(pc)-1].memsize);
            (*pstack)[(sp)-1] = pi8;
            ++(pc);
        }
        break;
    case 386:;
        {
            if ((u64)0u) {
                if ((stack[((sp)--)-1] != (i64)1457115631958533000)) {
                    pci_support$pcerror3((byte*)"retp: stack error:",(byte*)"",pc);
                }
;
            }
;
            n = (i64)(*pci_decls$pcltable)[(pc)-1].x.nparams;
            sp -= (i64)(*pci_decls$pcltable)[(pc)-1].x.nlocals;
            fp = stack[((sp)--)-1];
            if (((fp < (i64)0) || (fp > (i64)69980))) {
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"GETNLOCALS=",NULL);
                msysc$m_print_i64((i64)(*pci_decls$pcltable)[(pc)-1].x.nlocals,NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                pci_support$pcerror3((byte*)"RETP:INVALID FRAME PTR",msysc$strint(fp,0),pc);
            }
;
            pc = stack[((sp)--)-1];
            sp -= n;
        }
        break;
    case 387:;
        {
            if ((u64)0u) {
                if ((stack[((sp - (i64)1))-1] != (i64)1457115631958533000)) {
                    pci_support$pcerror3((byte*)"retf: stack error:",(byte*)"",pc);
                }
;
                stack[((sp - (i64)1))-1] = stack[(sp)-1];
                --(sp);
            }
;
            x = stack[(sp)-1];
            n = (i64)(*pci_decls$pcltable)[(pc)-1].x.nparams;
            sp -= (i64)(*pci_decls$pcltable)[(pc)-1].x.nlocals;
            fp = stack[(--(sp))-1];
            pc = stack[(--(sp))-1];
            sp -= n;
            stack[(sp)-1] = x;
        }
        break;
    case 388:;
        {
            a = (i64)(*pci_decls$pcltable)[(pc)-1].x.nretvalues;
            spr = ((sp - a) + (i64)1);
            sp -= ((i64)(*pci_decls$pcltable)[(pc)-1].x.nlocals + a);
            n = (i64)(*pci_decls$pcltable)[(pc)-1].x.nparams;
            fp = stack[((sp)--)-1];
            pc = stack[((sp)--)-1];
            sp -= n;
            for (i=(i64)1;i<=a;++i) {
L5 :;
                stack[(++(sp))-1] = stack[(((spr + i) - (i64)1))-1];
L6 :;
            }
L7 :;
            ;
        }
        break;
    case 389:;
        {
            pc = (i64)pci_decls$labeltable[((*pci_decls$pcltable)[(pc)-1].a.labelno)-1];
        }
        break;
    case 390:;
        {
            pc = stack[((sp)--)-1];
        }
        break;
    case 391:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 392:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 393:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 394:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 395:;
        {
            return stack[(sp)-1];
            goto L2 ;
        }
        break;
    case 396:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 397:;
        {
            msysc$m_print_startcon();
            msysc$m_print_i64(stack[((sp)--)-1],NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            ++(pc);
        }
        break;
    case 398:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 399:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 400:;
        {
            msysc$m_print_startcon();
            msysc$m_print_r64(pci_execc$getr32(stack[((sp)--)-1]),NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            ++(pc);
        }
        break;
    case 401:;
        {
            msysc$m_print_startcon();
            msysc$m_print_str((u8 *)stack[((sp)--)-1],NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            ++(pc);
        }
        break;
    case 402:;
        {
            msysc$m_print_startcon();
            msysc$m_print_i64(stack[(sp)-1],(byte*)"h");
            msysc$m_print_i64(stack[(sp)-1],NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            --(sp);
            ++(pc);
        }
        break;
    case 403:;
        {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"SP=",NULL);
            msysc$m_print_i64(sp,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            ++(pc);
        }
        break;
    case 404:;
        {
            ++(pc);
        }
        break;
    case 405:;
        {
            ++(pc);
        }
        break;
    case 406:;
        {
            ++(sp);
            ++(pc);
        }
        break;
    case 407:;
        {
            if (!(((i64)(*pci_decls$pcltable)[((pc - (i64)1))-1].jcode == (i64)383 || (i64)(*pci_decls$pcltable)[((pc - (i64)1))-1].jcode == (i64)384))) {
                                {i64 $temp = (i64)(*pci_decls$pcltable)[(pc)-1].x.x;
if (($temp==(i64)0)) {
                    --(sp);
                }
                else if (($temp==(i64)1)) {
                    --(sp);
                    stack[(sp)-1] = stack[((sp + (i64)1))-1];
                }
                else {
                    pci_support$serror((byte*)"stackadj/mult");
                }
                };
            }
;
            ++(pc);
        }
        break;
    case 409:;
        {
            goto L3 ;
;
            ++(pc);
        }
        break;
    case 408:;
        {
                        {i64 $temp = (i64)(*pci_decls$pcltable)[(pc)-1].x.x;
if (($temp==(i64)0) || ($temp==(i64)1)) {
                pci_execc$debug = (i64)(*pci_decls$pcltable)[(pc)-1].x.x;
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"Debug set to",NULL);
                msysc$m_print_i64((i64)pci_execc$debug,NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
            }
            else if (($temp==(i64)2)) {
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"SP=",NULL);
                msysc$m_print_i64(sp,NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
            }
            };
            ++(pc);
        }
        break;
    default: {
        //unimpl:
L3 :;
;
        msysc$m_print_startcon();
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startcon();
        msysc$m_print_setfmt((byte*)"Unimpl: # (#) on line: #");
        msysc$m_print_str(pci_tables$jcodenames[((i64)(*pci_decls$pcltable)[(pc)-1].jcode)],NULL);
        msysc$m_print_str(pci_tables$pclnames[((i64)(*pci_decls$pcltable)[(pc)-1].opcode)-1],NULL);
        msysc$m_print_i64((i64)(*pci_decls$pcllines)[(pc)-1],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startcon();
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        exit((i64)1);
    }
    } //SW
goto L1 ;
L2 :;
    ;
    return (i64)0;
}

void pci_execc$runpcl(void) {
        i64 tt;
        i64 stopcode;
    tt = clock();
    if ((pci_parse$pcmain == (i64)0)) {
        pci_support$pcerror3((byte*)"'main' not present",(byte*)"",(i64)0);
    }
;
    if (!!(pci_parse$undefflag)) {
        pci_support$pcerror3((byte*)"Some names/labels undefined",(byte*)"",(i64)0);
    }
;
    pci_support$updatedatarefs();
    pci_support$loadlibs();
    pci_support$docmdskip();
    stopcode = pci_execc$exec(pci_parse$pcmain,(i64)0);
    tt = (clock() - tt);
}

static r32 pci_execc$getr32(i64 a) {
        u32 b;
        r32 x;
    b = a;
    x = (*(r32 *)&b);
    return x;
}

static i64 pci_execc$putr32(r32 x) {
        u32 a;
    a = *(i64*)&x;
    return (i64)a;
}

// START
void pci_execc$start(void) {

}

void pci_fixup$fixuppcl(void) {
        i64 pcindex;
    pci_fixup$stproc = 0;
    for (pcindex=(i64)1;pcindex<=pci_decls$npcl;++pcindex) {
L8 :;
        pci_fixup$convertpcl(pcindex);
L9 :;
    }
L10 :;
    ;
    if (!!(pci_parse$pcmain)) {
        pci_fixup$addpclop((i64)34,(i64)379);
        pci_parse$pcmain = pci_decls$npcl;
        (*pci_decls$pcltable)[(pci_decls$npcl)-1].a.def = pci_parse$stmain;
        (*pci_decls$pcltable)[(pci_decls$npcl)-1].opndtype = (i64)2;
        pci_fixup$addpclop((i64)19,(i64)35);
        pci_fixup$addpclop((i64)42,(i64)395);
    }
;
}

static void pci_fixup$addpclop(i64 k,i64 j) {
    ++(pci_decls$npcl);
    (*pci_decls$pcltable)[(pci_decls$npcl)-1].opcode = k;
    (*pci_decls$pcltable)[(pci_decls$npcl)-1].jcode = j;
}

static void pci_fixup$convertpcl(i64 index) {
        struct pci_decls$pclrec *  p;
        struct pci_decls$pclrec *  q;
        struct pci_decls$pclrec *  r;
        i64 opcode;
        i64 jcode;
        i64 offset;
    p = &(*pci_decls$pcltable)[(index)-1];
    opcode = (i64)(*p).opcode;
    jcode = (i64)pci_tables$pcljcode[(opcode)-1];
    if ((jcode == (i64)0)) {
        jcode = pci_fixup$findjcode((struct pci_decls$pclrec *)p);
        if ((jcode == (i64)0)) {
            jcode = (i64)0;
        }
;
    }
;
    if ((((i64)(*p).opndtype == (i64)2) && ((i64)(*(*p).a.def).idtype == (i64)3 || (i64)(*(*p).a.def).idtype == (i64)4))) {
        if ((opcode==(i64)17)) {
            jcode = ((i64)11 + (jcode - (i64)1));
        }
        else if ((opcode==(i64)20)) {
            jcode = ((i64)28 + (jcode - (i64)21));
        }
        else if ((opcode==(i64)18)) {
            jcode = (i64)40;
        }
;
    }
    else {
        if ((opcode==(i64)1)) {
            pci_fixup$stproc = (*p).a.def;
        }
        else if ((opcode==(i64)139)) {
        }
        else if ((opcode==(i64)53) || (opcode==(i64)54)) {
            q = &(*pci_decls$pcltable)[((index + (i64)1))-1];
            r = &(*pci_decls$pcltable)[((index + (i64)2))-1];
            offset = (i64)0;
            if (((i64)(*(*q).a.def).idtype == (i64)2)) {
                if (((i64)(*r).opndtype == (i64)4)) {
                    offset = (i64)4;
                }
                else if (((i64)(*(*r).a.def).idtype == (i64)2)) {
                }
                else {
                    offset = (i64)1;
                }
;
            }
            else {
                if (((i64)(*r).opndtype == (i64)4)) {
                    offset = (i64)5;
                }
                else if (((i64)(*(*r).a.def).idtype == (i64)2)) {
                    offset = (i64)2;
                }
                else {
                    offset = (i64)3;
                }
;
            }
;
            jcode += offset;
        }
        else if ((opcode==(i64)55)) {
            q = &(*pci_decls$pcltable)[((index + (i64)1))-1];
            if (((i64)(*(*q).a.def).idtype != (i64)2)) {
                ++(jcode);
            }
;
        }
        else if ((opcode==(i64)18)) {
                        {i64 $temp = (i64)(*p).opndtype;
if (($temp==(i64)3)) {
                jcode = (i64)41;
            }
            else if (($temp==(i64)2)) {
                                {i64 $temp = (i64)(*(*p).a.def).idtype;
if (($temp==(i64)1)) {
                    jcode = (i64)42;
                }
                else if (($temp==(i64)5)) {
                    jcode = (i64)43;
                }
                };
            }
            };
        }
        else if ((opcode==(i64)41)) {
                        {i64 $temp = (i64)(*p).x.nretvalues;
if (($temp==(i64)0)) {
            }
            else if (($temp==(i64)1)) {
                jcode = (i64)387;
            }
            else {
                jcode = (i64)388;
            }
            };
        }
        else if ((opcode==(i64)34)) {
            if (!!(msysc$m_getdotindex((i64)(*(*p).a.def).flags,(i64)0))) {
                jcode = (i64)383;
            }
;
        }
        else if ((opcode==(i64)35)) {
            if (!!(msysc$m_getdotindex((i64)(*(*p).a.def).flags,(i64)0))) {
                jcode = (i64)384;
            }
;
        }
        else if ((opcode==(i64)133)) {
            if (((i64)(*p).mode == (i64)1)) {
                jcode = (((i64)(*p).mode2 == (i64)3) ? (i64)322 : (i64)324);
            }
            else {
                jcode = (((i64)(*p).mode2 == (i64)3) ? (i64)323 : (i64)325);
            }
;
        }
        else if ((opcode==(i64)132)) {
            if (((i64)(*p).mode == (i64)3)) {
                jcode = (((i64)(*p).mode2 == (i64)1) ? (i64)318 : (i64)319);
            }
            else {
                jcode = (((i64)(*p).mode2 == (i64)1) ? (i64)320 : (i64)321);
            }
;
        }
        else if ((opcode==(i64)40)) {
            if (((i64)(*p).mode != (i64)5)) {
                return;
            }
;
        }
        else if ((opcode==(i64)134)) {
                        {i64 $temp = (i64)(*p).mode2;
if (($temp==(i64)6)) {
                jcode = (i64)328;
            }
            else if (($temp==(i64)7)) {
                jcode = (i64)329;
            }
            else if (($temp==(i64)8)) {
                jcode = (i64)330;
            }
            else if (($temp==(i64)9)) {
                jcode = (i64)331;
            }
            else if (($temp==(i64)10)) {
                jcode = (i64)332;
            }
            else {
                jcode = (i64)333;
            }
            };
        }
;
    }
;
    (*pci_decls$pcltable)[(index)-1].jcode = jcode;
}

static i64 pci_fixup$findjcode(struct pci_decls$pclrec *p) {
        i64 i;
    for (i=(i64)1;i<=(i64)56;++i) {
L11 :;
        if (((i64)pci_tables$pcllist[(i)-1] == (i64)(*p).opcode)) {
            return (i64)pci_tables$jcodemodelist[(i)-1][((i64)(*p).mode)];
        }
;
L12 :;
    }
    {
    }
L13 :;
    ;
    return (i64)0;
}

// START
void pci_fixup$start(void) {

}

void pci_lex$startlex(i64 lineno) {
    pci_lex$lxsptr = (*pci_decls$sourcelines)[(lineno)-1];
    pci_decls$lx.lineno = lineno;
}

void pci_lex$startlextest(u8 *s) {
    pci_lex$lxsptr = s;
    pci_decls$lx.lineno = (i64)1;
}

void pci_lex$lex(void) {
        u8 str[256];
        i64 c;
        i64 hsum;
        i64 length;
        u8 *  ss;
    pci_decls$lx.value = (i64)0;
    L14 :;
    switch ((pci_lex$lxstart = pci_lex$lxsptr, (i64)(u64)(*(pci_lex$lxsptr)++))) {
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
    case 95:;
    case 36:;
    case 46:;
        {
            ss = (pci_lex$lxsptr - (i64)1);
            hsum = (i64)(u64)(*ss);
            L16 :;
            while (!!((u64)pci_lex$alphamap[((c = (i64)(u64)(*(pci_lex$lxsptr)++)))])) {
                hsum = (((hsum << (i64)4) - hsum) + c);
L17 :;
            }
L18 :;
            ;
            --(pci_lex$lxsptr);
            pci_decls$lx.symbol = (i64)2;
            pci_decls$lx.opcode = (pci_decls$lx.mode = (i64)0);
            length = (pci_lex$lxsptr - ss);
            if ((((u64)(*ss) == '.') && !!(pci_parse$stproc))) {
                strcpy(str,(*pci_parse$stproc).name);
                memcpy((str + (i64)(*pci_parse$stproc).namelen),(void *)ss,(u64)length);
                length += (i64)(*pci_parse$stproc).namelen;
                str[((length + (i64)1))-1] = (u64)0u;
                pci_lex$lookup(str,length,pci_lex$gethashvaluez(str));
            }
            else {
                pci_lex$lookup(ss,length,((hsum << (i64)5) - hsum));
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
            if ((((u64)(*pci_lex$lxstart) == '0') && ((u64)(*pci_lex$lxsptr) == 'x' || (u64)(*pci_lex$lxsptr) == 'X'))) {
                ++(pci_lex$lxsptr);
                pci_lex$readhex();
            }
            else {
                --(pci_lex$lxsptr);
                pci_lex$readdec((i64)0);
            }
;
            return;
        }
        break;
    case 45:;
        {
            if (((u64)(*pci_lex$lxsptr) == '-')) {
                ++(pci_lex$lxsptr);
                pci_decls$lx.symbol = (i64)2;
                pci_decls$lx.symptr = (struct pci_decls$strec *)pci_decls$stvoid;
                pci_decls$lx.mode = (i64)12;
            }
            else {
                pci_lex$readdec((i64)1);
            }
;
            return;
        }
        break;
    case 33:;
    case 59:;
    case 0:;
        {
            pci_decls$lx.symbol = (i64)12;
            --(pci_lex$lxsptr);
            return;
        }
        break;
    case 35:;
        {
            if (!(((u64)(*pci_lex$lxsptr) >= (i64)48 && (u64)(*pci_lex$lxsptr) <= (i64)57))) {
                pci_lex$lxerror((byte*)"Label?");
            }
;
            pci_lex$readdec((i64)0);
            pci_decls$lx.symbol = (i64)3;
            return;
        }
        break;
    case 42:;
        {
            pci_decls$lx.symbol = (i64)8;
            return;
        }
        break;
    case 32:;
    case 9:;
        {
        }
        break;
    case 58:;
        {
            pci_decls$lx.symbol = (i64)9;
            return;
        }
        break;
    case 34:;
        {
            pci_lex$readstring((i64)34);
            return;
        }
        break;
    case 39:;
        {
            pci_lex$readstring((i64)39);
            return;
        }
        break;
    default: {
        pci_decls$lx.symbol = (i64)1;
        return;
    }
    } //SW
goto L14 ;
L15 :;
    ;
}

// START
void pci_lex$start(void) {

        i64 i;
    for (i=(i64)0;i<=(i64)255;++i) {
L19 :;
        if ((((((i >= (i64)65 && i <= (i64)90) || (i >= (i64)97 && i <= (i64)122)) || (i >= (i64)48 && i <= (i64)57)) || (i == '$' || i == '_' || i == '.')) || (i >= (i64)128))) {
            pci_lex$alphamap[(i)] = (u64)1u;
        }
;
L20 :;
    }
L21 :;
    ;
    pci_lex$inithashtable();
}

static void pci_lex$lxerror(u8 *mess) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"LEX error:",NULL);
    msysc$m_print_str(mess,NULL);
    msysc$m_print_str((byte*)"on line:",NULL);
    msysc$m_print_i64(pci_decls$lx.lineno,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    exit((i64)1);
}

void pci_lex$printlx(void) {
    msysc$m_print_startcon();
    msysc$m_print_i64(pci_decls$lx.lineno,NULL);
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)":",NULL);
    msysc$m_print_str(pci_tables$symbolnames[(pci_decls$lx.symbol)-1],NULL);
    msysc$m_print_space();
    msysc$m_print_end();
    ;
        {i64 $temp = pci_decls$lx.symbol;
if (($temp==(i64)2)) {
        if (!!(pci_decls$lx.opcode)) {
            msysc$m_print_startcon();
            msysc$m_print_setfmt((byte*)"<OP:#>");
            msysc$m_print_str(pci_tables$pclnames[(pci_decls$lx.opcode)-1],NULL);
            msysc$m_print_end();
            ;
        }
        else if (!!(pci_decls$lx.mode)) {
            msysc$m_print_startcon();
            msysc$m_print_setfmt((byte*)"<TYPE:#>");
            msysc$m_print_str(pci_tables$typenames[(pci_decls$lx.mode)],NULL);
            msysc$m_print_end();
            ;
        }
        else {
            msysc$m_print_startcon();
            msysc$m_print_str(pci_decls$lx.svalue,NULL);
            msysc$m_print_end();
            ;
        }
;
    }
    else if (($temp==(i64)4)) {
        msysc$m_print_startcon();
        msysc$m_print_i64(pci_decls$lx.value,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)5)) {
        msysc$m_print_startcon();
        msysc$m_print_r64(pci_decls$lx.xvalue,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)6)) {
        msysc$m_print_startcon();
        msysc$m_print_setfmt((byte*)"\"#\"");
        msysc$m_print_str(pci_decls$lx.svalue,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)7)) {
        msysc$m_print_startcon();
        msysc$m_print_setfmt((byte*)"'#'");
        msysc$m_print_str(pci_decls$lx.svalue,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)3)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"#",NULL);
        msysc$m_print_nogap();
        msysc$m_print_i64(pci_decls$lx.value,NULL);
        msysc$m_print_end();
        ;
    }
    };
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

static void pci_lex$readdec(i64 neg) {
        i64 c;
        u8 *  dest;
        u8 *  destend;
        u8 *  pstart;
        i64 length;
        byte str[1024];
        u64 a;
    pstart = pci_lex$lxsptr;
    dest = (u8 *)str;
    destend = ((dest + (i64)1024) - (i64)10);
    a = (u64)0u;
    L22 :;
    while (1) {
        switch ((c = (i64)(u64)(*(pci_lex$lxsptr)++))) {
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
                pci_lex$lxsptr = pstart;
                pci_lex$readreal(neg);
                return;
            }
            break;
        case 46:;
            {
                pci_lex$lxsptr = pstart;
                pci_lex$readreal(neg);
                return;
            }
            break;
        default: {
            --(pci_lex$lxsptr);
            goto L23 ;
        }
        } //SW
;
        if ((dest >= destend)) {
            pci_lex$lxerror((byte*)"Numlit too long");
        }
;
    }
L23 :;
    ;
    length = (dest - str);
    if (((length > (i64)20) || ((length == (i64)20) && !!(strncmp((u8 *)str,(byte*)"18446744073709551615",(u64)20u))))) {
        pci_lex$lxerror((byte*)"u64 overflow");
    }
;
    //finish:
L24 :;
;
    pci_decls$lx.symbol = (i64)4;
    pci_decls$lx.value = (i64)(!!(neg) ? -(a) : a);
}

static void pci_lex$readhex(void) {
        i64 c;
        u8 *  dest;
        u8 *  destend;
        u8 *  pstart;
        i64 length;
        byte str[1024];
        u64 a;
    pstart = pci_lex$lxsptr;
    dest = (u8 *)str;
    destend = ((dest + (i64)1024) - (i64)10);
    a = (u64)0u;
    L25 :;
    while (1) {
        switch ((c = (i64)(u64)(*(pci_lex$lxsptr)++))) {
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
        default: {
            --(pci_lex$lxsptr);
            goto L26 ;
        }
        } //SW
;
        if ((dest >= destend)) {
            pci_lex$lxerror((byte*)"Numlit too long");
        }
;
    }
L26 :;
    ;
    length = (dest - str);
    if ((length > (i64)16)) {
        pci_lex$lxerror((byte*)"u64 overflow");
    }
;
    pci_decls$lx.symbol = (i64)4;
    pci_decls$lx.value = (i64)a;
}

static void pci_lex$readreal(i64 neg) {
        i64 c;
        i64 negexpon;
        i64 dotseen;
        i64 length;
        i64 fractlen;
        i64 expon;
        i64 expseen;
        u8 str[1024];
        u8 *  dest;
        u8 *  destend;
    dest = (u8 *)str;
    destend = ((dest + (i64)1024) - (i64)100);
    length = (negexpon = (dotseen = (expseen = (expon = (fractlen = (i64)0)))));
    L27 :;
    while (1) {
        switch ((c = (i64)(u64)(*(pci_lex$lxsptr)++))) {
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
                    --(pci_lex$lxsptr);
                    goto L28 ;
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
                    pci_lex$lxerror((byte*)"double expon");
                }
;
                expseen = (i64)1;
                (*(dest)++) = (u64)c;
                L29 :;
                while (((u64)(*pci_lex$lxsptr) == ' ')) {
                    ++(pci_lex$lxsptr);
L30 :;
                }
L31 :;
                ;
                if (((u64)(*pci_lex$lxsptr) == '+' || (u64)(*pci_lex$lxsptr) == '-')) {
                    if (((u64)(*pci_lex$lxsptr) == '-')) {
                        negexpon = (i64)1;
                    }
;
                    (*(dest)++) = (u64)(*(pci_lex$lxsptr)++);
                }
;
                expon = (i64)0;
                L32 :;
                switch ((c = (i64)(u64)(*(pci_lex$lxsptr)++))) {
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
                            pci_lex$lxerror((byte*)"expon?");
                        }
;
                    }
                    break;
                default: {
                    --(pci_lex$lxsptr);
                    goto L28 ;
                }
                } //SW
goto L32 ;
L33 :;
                ;
            }
            break;
        default: {
            --(pci_lex$lxsptr);
            goto L28 ;
        }
        } //SW
;
        if ((dest >= destend)) {
            pci_lex$lxerror((byte*)"r64lit too long");
        }
;
    }
L28 :;
    ;
    (*dest) = (u64)0u;
    pci_decls$lx.xvalue = strtod(str,0);
    if (!!(neg)) {
        pci_decls$lx.xvalue = -(pci_decls$lx.xvalue);
    }
;
    pci_decls$lx.symbol = (i64)5;
}

static void pci_lex$readstring(i64 termchar) {
        u8 *  s;
        u8 *  t;
        i64 c;
        i64 d;
        i64 length;
        i64 hasescape;
        u8 str[8];
        i64 $av_1;
    pci_decls$lx.symbol = ((termchar == (i64)34) ? (i64)6 : (i64)7);
    s = pci_lex$lxsptr;
    length = (i64)0;
    hasescape = (i64)0;
    L34 :;
    switch ((c = (i64)(u64)(*(pci_lex$lxsptr)++))) {
    case 92:;
        {
            c = (i64)(u64)(*pci_lex$lxsptr);
            ++(pci_lex$lxsptr);
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
                    pci_lex$lxsptr += (i64)2;
                    ++(length);
                }
                break;
            default: {
                pci_lex$lxerror((byte*)"Bad str escape");
            }
            } //SW
;
        }
        break;
    case 34:;
    case 39:;
        {
            if ((c == termchar)) {
                goto L35 ;
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
            pci_lex$lxerror((byte*)"String not terminated");
        }
        break;
    default: {
        ++(length);
    }
    } //SW
goto L34 ;
L35 :;
    ;
    if ((length == (i64)0)) {
        pci_decls$lx.svalue = (byte*)"";
        return;
    }
    else if (!(!!(hasescape))) {
        pci_decls$lx.svalue = mlib$pcm_copyheapstringn(s,length);
        return;
    }
;
    pci_decls$lx.svalue = (t = (u8 *)mlib$pcm_alloc((length + (i64)1)));
    L36 :;
    while (1) {
        switch ((c = (i64)(u64)(*(s)++))) {
        case 92:;
            {
                switch ((c = (i64)(u64)(*(s)++))) {
                case 99:;
                case 114:;
                    {
                        c = (i64)13;
                    }
                    break;
                case 108:;
                case 110:;
                    {
                        c = (i64)10;
                    }
                    break;
                case 116:;
                    {
                        c = (i64)9;
                    }
                    break;
                case 120:;
                    {
                        c = (i64)0;
                        $av_1 = (i64)2;
                        while ($av_1-- > 0) {
L38 :;
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
                                pci_lex$lxerror((byte*)"Bad \\x code");
                            }
                            };
L39 :;
                        }
L40 :;
                        ;
                    }
                    break;
                case 34:;
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
                    pci_lex$lxerror($pci$addstr((byte*)"Unknown string escape: ",str));
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
                        goto L37 ;
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
                pci_lex$lxerror((byte*)"String not terminated");
            }
            break;
        } //SW
;
        (*(t)++) = (u64)c;
    }
L37 :;
    ;
    (*t) = (u64)0u;
}

static void pci_lex$inithashtable(void) {
        i64 i;
    for (i=(i64)1;i<=(i64)150;++i) {
L41 :;
        pci_lex$addreservedword((pci_tables$pclnames[(i)-1] + (i64)1),i,(i64)0);
L42 :;
    }
L43 :;
    ;
    for (i=(i64)1;i<=(i64)12;++i) {
L44 :;
        pci_lex$addreservedword(pci_tables$typenames[(i)],(i64)0,i);
        if ((i == (i64)12)) {
            pci_decls$stvoid = (struct pci_decls$strec *)pci_decls$lx.symptr;
        }
;
L45 :;
    }
L46 :;
    ;
    pci_lex$addreservedword((byte*)"int",(i64)0,(i64)1);
    pci_lex$adddlllib((byte*)"libc.so.6");
}

static void pci_lex$adddlllib(u8 *name) {
    ++(pci_decls$nlibs);
    pci_lex$addreservedword(name,(i64)0,(i64)0);
    (*pci_decls$lx.symptr).idtype = (i64)6;
    pci_decls$libdef[(pci_decls$nlibs)-1] = (struct pci_decls$strec *)pci_decls$lx.symptr;
    pci_decls$libnames[(pci_decls$nlibs)-1] = (*pci_decls$lx.symptr).name;
    pci_decls$libtype[(pci_decls$nlibs)-1] = (i64)68;
}

void pci_lex$printhashtable(void) {
        struct pci_decls$strec *  d;
        i64 i;
    for (i=(i64)0;i<=(i64)65535;++i) {
L47 :;
        d = (struct pci_decls$strec *)&pci_decls$hashtable[(i)];
        if (!!((i64)(*d).namelen)) {
            msysc$m_print_startcon();
            msysc$m_print_i64(i,NULL);
            msysc$m_print_str((*d).name,NULL);
            msysc$m_print_i64((i64)(*d).opcode,NULL);
            msysc$m_print_i64((i64)(*d).typecode,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
;
L48 :;
    }
L49 :;
    ;
}

void pci_lex$addreservedword(u8 *name,i64 opcode,i64 mode) {
    if (!!(pci_lex$lookup(name,strlen(name),pci_lex$gethashvaluez(name)))) {
        pci_lex$lxerror($pci$addstr((byte*)"Dupl name:",name));
    }
;
    (*pci_decls$lx.symptr).opcode = opcode;
    (*pci_decls$lx.symptr).typecode = mode;
}

static i64 pci_lex$lookup(u8 *name,i64 length,i64 hashindex0) {
        i64 wrapped;
        i64 n;
        struct pci_decls$strec *  d;
        i64 j;
    j = (hashindex0 & (i64)65535);
    d = (struct pci_decls$strec *)&pci_decls$hashtable[(j)];
    wrapped = (i64)0;
    L50 :;
    while (1) {
        if (((i64)(*d).namelen == (i64)0)) {
            goto L51 ;
        }
;
        if ((((n = (i64)(*d).namelen) == length) && (memcmp((void *)(*d).name,(void *)name,(u64)n) == (i64)0))) {
            pci_decls$lx.symptr = (struct pci_decls$strec *)d;
            pci_decls$lx.opcode = (i64)(*d).opcode;
            pci_decls$lx.mode = (i64)(*d).typecode;
            return (i64)1;
        }
;
        if ((++(j) >= (i64)65536)) {
            if (!!(wrapped)) {
                pci_lex$lxerror((byte*)"Hashtab full");
            }
;
            wrapped = (i64)1;
            j = (i64)0;
        }
;
        d = (struct pci_decls$strec *)&pci_decls$hashtable[(j)];
    }
L51 :;
    ;
    (*d).name = mlib$pcm_copyheapstringn(name,length);
    (*d).namelen = length;
    pci_decls$lx.svalue = (*d).name;
    pci_decls$lx.symptr = (struct pci_decls$strec *)d;
    return (i64)0;
}

static i64 pci_lex$gethashvaluez(u8 *s) {
        i64 c;
        i64 hsum;
    if (((i64)(u64)(*s) == (i64)0)) {
        return (i64)0;
    }
;
    hsum = (i64)(u64)(*(s)++);
    L52 :;
    while (1) {
        c = (i64)(u64)(*(s)++);
        if ((c == (i64)0)) {
            goto L53 ;
        }
;
        hsum = (((hsum << (i64)4) - hsum) + c);
    }
L53 :;
    ;
    return ((hsum << (i64)5) - hsum);
}

struct pci_decls$strec *pci_lex$findsymbol(u8 *name) {
    if (!!(pci_lex$lookup(name,strlen(name),pci_lex$gethashvaluez(name)))) {
        return (struct pci_decls$strec *)pci_decls$lx.symptr;
    }
;
    return (struct pci_decls$strec *)0;
}

void pci_parse$parseline(i64 lineno) {
        struct pci_decls$pclrec p;
        i64 mode;
        i64 size;
        i64 size2;
        i64 opcode;
        i64 labelno;
        i64 i;
    memset(&(p),0,32);
    mode = (i64)0;
    size = (i64)0;
    pci_parse$pcllineno = lineno;
    pci_lex$startlex(lineno);
    pci_lex$lex();
        {i64 $temp = pci_decls$lx.symbol;
if (($temp==(i64)2)) {
        opcode = (p.opcode = pci_decls$lx.opcode);
        if ((opcode==(i64)0)) {
            pci_support$serror($pci$addstr((byte*)"Unknown opcode:",pci_decls$lx.svalue));
        }
        else if ((opcode==(i64)5) || (opcode==(i64)13) || (opcode==(i64)9) || (opcode==(i64)8)) {
            pci_parse$processop((struct pci_decls$pclrec *)&p,(i64)0);
            return;
        }
;
    }
    else if (($temp==(i64)3)) {
        if (!(!!(pci_parse$stproc))) {
            pci_support$serror((byte*)"Label outside proc");
        }
;
        labelno = pci_decls$lx.value;
        if ((labelno > (i64)15000)) {
            pci_support$serror((byte*)"Label value too high");
        }
;
        pci_decls$highlabel=(pci_decls$highlabel>labelno?pci_decls$highlabel:labelno);
;
        if (((i64)pci_decls$labeltable[(labelno)-1] > (i64)0)) {
            pci_support$serror($pci$addstr((byte*)"Dupl label:",msysc$strint(labelno,0)));
        }
;
        pci_decls$labeltable[(labelno)-1] = (pci_decls$npcl + (i64)1);
        for (i=(i64)1;i<=(i64)8;++i) {
L54 :;
            if (((i64)(*pci_decls$pcllabels)[((pci_decls$npcl + (i64)1))-1][(i)-1] == (i64)0)) {
                (*pci_decls$pcllabels)[((pci_decls$npcl + (i64)1))-1][(i)-1] = labelno;
                goto L56 ;
            }
;
L55 :;
        }
        {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"(More than 4 labels for this instr, line:",NULL);
            msysc$m_print_i64(pci_decls$lx.lineno,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
L56 :;
        ;
        pci_lex$lex();
        pci_parse$checksymbol((i64)9);
        pci_lex$lex();
        return;
    }
    else if (($temp==(i64)12)) {
        return;
    }
    else {
        pci_support$serror((byte*)"Bad line");
    }
    };
    pci_lex$lex();
    pci_parse$readmode(&p.mode,&size);
    if ((opcode==(i64)132) || (opcode==(i64)133) || (opcode==(i64)134) || (opcode==(i64)135) || (opcode==(i64)136) || (opcode==(i64)137)) {
        pci_parse$readmode(&p.mode2,&size2);
        if (((i64)p.mode2 == (i64)5)) {
            pci_support$serror((byte*)"Mem?");
        }
;
        pci_parse$processop((struct pci_decls$pclrec *)&p,(i64)0);
        pci_parse$lastopcode = opcode;
        return;
    }
;
    if (!!((i64)pci_tables$pclmain[(opcode)-1])) {
        pci_parse$readopnd((struct pci_decls$pclrec *)&p,(i64)pci_tables$pclmain[(opcode)-1]);
        if (((opcode == (i64)1) && (pci_decls$lx.symbol == (i64)8))) {
            (*p.a.def).flags = msysc$m_setdotindex((*p.a.def).flags,(i64)1,(u64)1u);
            pci_lex$lex();
        }
;
    }
;
    if (!!((i64)pci_tables$pclattr[(opcode)-1])) {
        pci_parse$readattrs((struct pci_decls$pclrec *)&p,(i64)pci_tables$pclattr[(opcode)-1]);
    }
;
    if (!!(size)) {
        if ((size > (i64)4294967295)) {
            pci_support$serror((byte*)"BLOCK SIZE TOO BIG");
        }
;
        p.memsize = size;
    }
;
    pci_parse$checksymbol((i64)12);
    pci_parse$processop((struct pci_decls$pclrec *)&p,(i64)0);
    pci_parse$lastopcode = opcode;
}

static void pci_parse$readmode(byte *m,i64 *size) {
    (*m) = (i64)0;
    (*size) = (i64)0;
    if (((pci_decls$lx.symbol == (i64)2) && !!(pci_decls$lx.mode))) {
        (*m) = pci_decls$lx.mode;
        if (((i64)(*m) == (i64)12)) {
            (*m) = (i64)0;
        }
;
        pci_lex$lex();
        if (((i64)(*m) == (i64)5)) {
            if ((pci_decls$lx.symbol != (i64)4)) {
                pci_support$serror((byte*)"Mem size missing");
            }
;
            (*size) = pci_decls$lx.value;
            pci_lex$lex();
        }
;
    }
;
}

static i64 pci_parse$readint(void) {
        i64 x;
    if ((pci_decls$lx.symbol == (i64)4)) {
        x = pci_decls$lx.value;
        pci_lex$lex();
    }
    else {
        pci_support$serror((byte*)"Intconst expected");
    }
;
    return x;
}

static void pci_parse$readattrs(struct pci_decls$pclrec *p,i64 nattrs) {
        i64 i;
    for (i=(i64)1;i<=nattrs;++i) {
L57 :;
        if ((pci_decls$lx.symbol == (i64)12)) {
            goto L59 ;
        }
;
        if (!((pci_decls$lx.symbol == (i64)4))) {
            pci_support$serror((byte*)"Intconst expected");
        }
;
        if ((i == (i64)1)) {
            (*p).x.x = pci_decls$lx.value;
        }
        else {
            (*p).x.y = pci_decls$lx.value;
        }
;
        pci_lex$lex();
L58 :;
    }
L59 :;
    ;
}

static void pci_parse$readopnd(struct pci_decls$pclrec *p,i64 flags) {
        i64 labelno;
        struct pci_decls$opndrec *  q;
        struct pci_decls$strec *  d;
    q = &(*p).a;
        {i64 $temp = pci_decls$lx.symbol;
if (($temp==(i64)3)) {
        (*q).value = (labelno = pci_decls$lx.value);
        pci_decls$highlabel=(pci_decls$highlabel>labelno?pci_decls$highlabel:labelno);
;
        if (((i64)pci_decls$labeltable[(labelno)-1] == (i64)0)) {
            pci_decls$labeltable[(labelno)-1] = (i64)-1;
        }
;
        if (!(!!((flags & (i64)1)))) {
            goto L60 ;
;
        }
;
    }
    else if (($temp==(i64)2)) {
        d = ((*q).def = (struct pci_decls$strec *)pci_decls$lx.symptr);
        if (!(!!((flags & (i64)16)))) {
            goto L60 ;
;
        }
;
        if (((i64)(*p).mode == (i64)0)) {
            (*p).mode = (i64)(*d).mode;
        }
;
    }
    else if (($temp==(i64)4)) {
        (*q).value = pci_decls$lx.value;
        if (((i64)(*p).mode == (i64)0)) {
            (*p).mode = (i64)1;
        }
;
        if (!(!!((flags & (i64)2)))) {
            goto L60 ;
;
        }
;
    }
    else if (($temp==(i64)5)) {
        (*q).xvalue = pci_decls$lx.xvalue;
        if (((i64)(*p).mode == (i64)0)) {
            (*p).mode = (i64)3;
        }
        else if (((i64)(*p).mode == (i64)4)) {
            (*q).xvalue32 = (r32)(*q).xvalue;
        }
;
        if (!(!!((flags & (i64)4)))) {
            goto L60 ;
;
        }
;
    }
    else if (($temp==(i64)6) || ($temp==(i64)7)) {
        (*q).svalue = pci_decls$lx.svalue;
        if (((i64)(*p).mode == (i64)0)) {
            (*p).mode = (i64)2;
        }
;
        if (!(!!((flags & (i64)8)))) {
            goto L60 ;
;
        }
;
    }
    else if (($temp==(i64)12)) {
        if (!(!!((flags & (i64)32)))) {
            pci_support$serror((byte*)"Missing opnd");
        }
;
    }
    else {
        //error:
L60 :;
;
        pci_support$serror((byte*)"Bad opnd");
    }
    };
    (*p).opndtype = pci_decls$lx.symbol;
    pci_lex$lex();
}

static void pci_parse$checkdefined(struct pci_decls$pclrec *p,i64 idtype) {
        struct pci_decls$strec *  d;
    d = (*p).a.def;
    if (!!((i64)(*d).idtype)) {
        pci_support$serror($pci$addstr((byte*)"Dupl name: ",(*d).name));
    }
;
    (*d).idtype = idtype;
    (*d).mode = (i64)(*p).mode;
    if (((i64)(*p).mode == (i64)5)) {
        (*d).memsize = pci_parse$getpclsize((struct pci_decls$pclrec *)p);
    }
    else {
        (*d).memsize = (i64)8;
    }
;
}

static void pci_parse$checksymbol(i64 symbol) {
    if ((pci_decls$lx.symbol != symbol)) {
        msysc$m_print_startcon();
        msysc$m_print_setfmt((byte*)"'#' expected not '#'");
        msysc$m_print_str(pci_tables$symbolnames[(symbol)-1],NULL);
        msysc$m_print_str(pci_tables$symbolnames[(pci_decls$lx.symbol)-1],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        pci_support$serror((byte*)"");
    }
;
}

static void pci_parse$dolocal(struct pci_decls$pclrec *p) {
        struct pci_decls$strec *  d;
    d = (*p).a.def;
    pci_parse$checkdefined((struct pci_decls$pclrec *)p,(i64)3);
    ++((*pci_parse$stproc).nlocals);
    if (((i64)(*d).mode == (i64)0)) {
        (*d).mode = (i64)1;
    }
;
    (*d).frameoffset = ((pci_parse$framebytes / (i64)8) + (i64)1);
    pci_parse$framebytes += mlib$roundtoblock((i64)(*d).memsize,(i64)8);
}

static void pci_parse$doparam(struct pci_decls$pclrec *p) {
        struct pci_decls$strec *  d;
    d = (*p).a.def;
    pci_parse$checkdefined((struct pci_decls$pclrec *)p,(i64)4);
    ++((*pci_parse$stproc).nparams);
    if (((i64)(*d).mode == (i64)0)) {
        (*d).mode = (i64)1;
    }
;
    if (((i64)(*d).mode == (i64)5)) {
        pci_parse$parambytes += (i64)8;
    }
    else {
        pci_parse$parambytes += (i64)(*d).memsize;
    }
;
    (*d).frameoffset = -(((pci_parse$parambytes / (i64)8) + (i64)1));
}

static void pci_parse$dostatic(struct pci_decls$pclrec *p) {
        struct pci_decls$strec *  d;
    d = (*p).a.def;
    pci_parse$checkdefined((struct pci_decls$pclrec *)p,(i64)2);
    if (((i64)$pci$passlevel == (i64)3)) {
        (*d).staticaddr = (byte *)mlib$allocmem((i64)(*d).memsize);
        if (((i64)(*d).mode == (i64)0)) {
            (*d).mode = (i64)1;
        }
;
        mlib$pcm_clearmem((*d).staticaddr,(i64)(*d).memsize);
    }
;
    if (((i64)(*p).opcode == (i64)10)) {
        pci_parse$ststatic = (*p).a.def;
        pci_parse$nstaticbytes = (i64)0;
    }
    else {
        pci_parse$ststatic = 0;
    }
;
}

static void pci_parse$doidata(struct pci_decls$pclrec *p) {
        i64 n;
        i64 length;
        struct pci_decls$opndrec *  q;
        byte *  pdest;
        r32 sx;
    if (!((pci_parse$lastopcode == (i64)10 || pci_parse$lastopcode == (i64)12))) {
        pci_support$serror((byte*)"data out of sequence");
    }
;
    if ((pci_parse$ststatic == 0)) {
        pci_support$serror((byte*)"No current istatic");
    }
;
    q = (struct pci_decls$opndrec *)&(*p).a;
    n = (i64)pci_tables$typesizes[((i64)(*p).mode)];
    if (((i64)(*p).mode == (i64)5)) {
        n = (i64)(*p).memsize;
    }
;
    if (((i64)(*p).opndtype == (i64)7)) {
        length = strlen((*q).svalue);
        if ((!!(n) && (n != length))) {
            pci_support$serror((byte*)"data/char size mismatch");
        }
;
        if ((n == (i64)0)) {
            n = length;
        }
;
    }
    else if ((n == (i64)0)) {
        n = (i64)8;
    }
    else if (!((n == (i64)1 || n == (i64)2 || n == (i64)4 || n == (i64)8))) {
        pci_support$serror((byte*)"data size not 1/2/4/8");
    }
;
    if (((pci_parse$nstaticbytes + n) > (i64)(*pci_parse$ststatic).memsize)) {
        pci_support$serror((byte*)"Too much data for static");
    }
;
    pdest = ((*pci_parse$ststatic).staticaddr + pci_parse$nstaticbytes);
    if (((i64)$pci$passlevel == (i64)3)) {
                {i64 $temp = (i64)(*p).opndtype;
if (($temp==(i64)4)) {
            memcpy(pdest,q,(u64)n);
        }
        else if (($temp==(i64)5)) {
            if ((n==(i64)8)) {
                memcpy(pdest,q,(u64)8u);
            }
            else if ((n==(i64)4)) {
                sx = (r32)(*q).xvalue;
                memcpy(pdest,&sx,(u64)4u);
            }
            else {
                pci_support$serror((byte*)"real can't fit in 1-2 bytes");
            }
;
        }
        else if (($temp==(i64)7)) {
            memcpy(pdest,(void *)(*q).svalue,(u64)n);
        }
        else if (($temp==(i64)6)) {
            if ((n != (i64)8)) {
                pci_support$serror((byte*)"String data?");
            }
;
            memcpy(pdest,q,(u64)8u);
        }
        else if (($temp==(i64)3)) {
            if ((n != (i64)8)) {
                pci_support$serror((byte*)"Label data?");
            }
;
            (*pdest) = (*q).labelno;
            pci_support$addlabelref((i64 *)pdest);
        }
        else if (($temp==(i64)2)) {
            if ((n != (i64)8)) {
                pci_support$serror((byte*)"Name data?");
            }
;
            (*(u64 *)pdest) = (u64)(*q).def;
            pci_support$adddataref((struct pci_decls$strec **)pdest);
        }
        };
    }
;
    pci_parse$nstaticbytes += n;
}

static i64 pci_parse$getpclsize(struct pci_decls$pclrec *p) {
    if (((i64)(*p).mode == (i64)5)) {
        return (i64)(*p).memsize;
    }
;
    return (i64)0;
}

static void pci_parse$doproc(struct pci_decls$pclrec *p) {
    if ((!!(pci_parse$stproc) || !!(pci_parse$stextproc))) {
        pci_support$serror((byte*)"\"end\" missing, or nested");
    }
;
    pci_parse$framebytes = (pci_parse$parambytes = (i64)0);
    pci_parse$stproc = (*p).a.def;
    if (!!(mlib$eqstring((*pci_parse$stproc).name,(byte*)"main"))) {
        (*pci_parse$stproc).flags = msysc$m_setdotindex((*pci_parse$stproc).flags,(i64)1,(u64)1u);
        pci_parse$stmain = pci_parse$stproc;
        pci_parse$pcmain = (pci_decls$npcl + (i64)1);
    }
;
    pci_parse$checkdefined((struct pci_decls$pclrec *)p,(i64)1);
    (*pci_parse$stproc).pcindex = (pci_decls$npcl + (i64)1);
    pci_parse$entrypending = (i64)1;
}

static void pci_parse$doextproc(struct pci_decls$pclrec *p) {
    if ((!!(pci_parse$stproc) || !!(pci_parse$stextproc))) {
        pci_support$serror((byte*)"\"end\" missing, or nested");
    }
;
    pci_parse$stextproc = (*p).a.def;
    pci_parse$checkdefined((struct pci_decls$pclrec *)p,(i64)5);
    if ((pci_decls$nextprocs > (i64)500)) {
        pci_support$serror((byte*)"Too many imports");
    }
;
    ++(pci_decls$nextprocs);
    pci_decls$extdef[(pci_decls$nextprocs)-1] = pci_parse$stextproc;
    (*pci_parse$stextproc).importno = pci_decls$nextprocs;
    (*pci_parse$stextproc).flags = msysc$m_setdotindex((*pci_parse$stextproc).flags,(i64)0,(u64)1u);
    (*pci_parse$stextproc).mode = (i64)(*p).mode;
    pci_decls$extrettype[(pci_decls$nextprocs)-1] = (i64)(*p).mode;
}

static void pci_parse$doextparam(struct pci_decls$pclrec *p) {
        i64 index;
    if ((pci_parse$stextproc == 0)) {
        pci_support$serror((byte*)"\"extproc missing");
    }
;
    index = (*pci_parse$stextproc).importno;
    if (((i64)pci_decls$extnparams[(index)-1] >= (i64)16)) {
        pci_support$serror((byte*)"Too many extparams");
    }
;
    ++(pci_decls$extnparams[(index)-1]);
    pci_decls$exttypes[(index)-1][((i64)pci_decls$extnparams[(index)-1])-1] = (i64)(*p).mode;
}

static void pci_parse$updateproc(void) {
        struct pci_decls$pclrec *  p;
    if (!!(msysc$m_getdotindex((i64)(*pci_parse$stproc).flags,(i64)2))) {
        return;
    }
;
    p = (struct pci_decls$pclrec *)&(*pci_decls$pcltable)[((*pci_parse$stproc).pcindex)-1];
    (*p).x.nlocals = ((*pci_parse$stproc).nlocals = (pci_parse$framebytes / (i64)8));
    (*p).x.nparams = ((*pci_parse$stproc).nparams = (pci_parse$parambytes / (i64)8));
    (*pci_parse$stproc).flags = msysc$m_setdotindex((*pci_parse$stproc).flags,(i64)2,(u64)1u);
}

static void pci_parse$undefinedcheck(void) {
        struct pci_decls$strec *  d;
        i64 i;
    for (i=(i64)1;i<=pci_decls$highlabel;++i) {
L61 :;
        if (((i64)pci_decls$labeltable[(i)-1] == (i64)-1)) {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"Undefined label: #",NULL);
            msysc$m_print_nogap();
            msysc$m_print_i64(i,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            pci_parse$undefflag = (i64)1;
        }
;
L62 :;
    }
L63 :;
    ;
    for (i=(i64)0;i<=(i64)65535;++i) {
L64 :;
        d = (struct pci_decls$strec *)&pci_decls$hashtable[(i)];
        if ((!!((i64)(*d).namelen) && ((i64)(*d).opcode==(i64)(*d).typecode && (i64)(*d).typecode==(i64)(*d).idtype && (i64)(*d).idtype==(i64)0))) {
            msysc$m_print_startcon();
            msysc$m_print_ptr(d,NULL);
            msysc$m_print_str((byte*)"Undefined:",NULL);
            msysc$m_print_str((*d).name,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            pci_parse$undefflag = (i64)1;
        }
;
L65 :;
    }
L66 :;
    ;
}

void pci_parse$parseprogram(void) {
        i64 i;
    pci_support$initpcl1();
    pci_parse$lastopcode = (i64)0;
    for (i=(i64)1;i<=pci_decls$nsourcelines;++i) {
L67 :;
        pci_parse$parseline(i);
L68 :;
    }
L69 :;
    ;
    if ((!!(pci_parse$stproc) || !!(pci_parse$stextproc))) {
        pci_support$serror((byte*)"2:\"end\" missing");
    }
;
    pci_parse$undefinedcheck();
}

static void pci_parse$processop(struct pci_decls$pclrec *p,i64 size) {
        i64 index;
    switch ((i64)(*p).opcode) {
    case 1:;
        {
            pci_parse$doproc((struct pci_decls$pclrec *)p);
            (*pci_decls$pcltable)[(++(pci_decls$npcl))-1] = (*p);
            (*pci_decls$pcllines)[(pci_decls$npcl)-1] = pci_decls$lx.lineno;
            return;
        }
        break;
    case 3:;
        {
            pci_parse$dolocal((struct pci_decls$pclrec *)p);
            return;
        }
        break;
    case 2:;
        {
            pci_parse$doparam((struct pci_decls$pclrec *)p);
            return;
        }
        break;
    case 4:;
        {
            if (((i64)(*pci_parse$stproc).nretvalues >= (i64)3)) {
                pci_support$serror((byte*)"Too many return types");
            }
;
            if (((i64)(*p).mode == (i64)0)) {
                (*p).mode = (i64)1;
            }
;
            (*pci_parse$stproc).retmode[(++((*pci_parse$stproc).nretvalues))-1] = (i64)(*p).mode;
            (*pci_parse$stproc).memsize = pci_parse$getpclsize((struct pci_decls$pclrec *)p);
            return;
        }
        break;
    case 10:;
    case 11:;
        {
            pci_parse$dostatic((struct pci_decls$pclrec *)p);
            return;
        }
        break;
    case 12:;
        {
            pci_parse$doidata((struct pci_decls$pclrec *)p);
            return;
        }
        break;
    case 41:;
        {
            if (!(!!(pci_parse$stproc))) {
                pci_support$serror((byte*)"\"proc\" missing");
            }
;
            pci_parse$updateproc();
            (*p).x.nlocals = (i64)(*pci_parse$stproc).nlocals;
            (*p).x.nparams = (i64)(*pci_parse$stproc).nparams;
            (*p).x.nretvalues = (i64)(*pci_parse$stproc).nretvalues;
        }
        break;
    case 5:;
        {
            if (!(!!(pci_parse$stproc))) {
                pci_support$serror((byte*)"\"proc\" missing");
            }
;
            pci_parse$updateproc();
            pci_parse$stproc = 0;
            pci_lex$lex();
            return;
        }
        break;
    case 58:;
        {
            return;
        }
        break;
    case 38:;
    case 39:;
        {
            return;
        }
        break;
    case 14:;
    case 15:;
    case 16:;
        {
            return;
        }
        break;
    case 137:;
        {
            return;
        }
        break;
    case 13:;
        {
            pci_lex$lex();
            pci_parse$checksymbol((i64)2);
            pci_support$addlib((struct pci_decls$strec *)pci_decls$lx.symptr);
            pci_lex$lex();
            return;
        }
        break;
    case 6:;
        {
            pci_parse$doextproc((struct pci_decls$pclrec *)p);
            return;
        }
        break;
    case 7:;
        {
            pci_parse$doextparam((struct pci_decls$pclrec *)p);
            return;
        }
        break;
    case 8:;
        {
            if (!(!!(pci_parse$stextproc))) {
                pci_support$serror((byte*)"extvar?");
            }
;
            index = (*pci_parse$stextproc).importno;
            pci_decls$extvariadic[(index)-1] = ((i64)pci_decls$extnparams[(index)-1] + (i64)1);
            return;
        }
        break;
    case 9:;
        {
            if (!(!!(pci_parse$stextproc))) {
                pci_support$serror((byte*)"\"extproc\" missing");
            }
;
            pci_parse$stextproc = 0;
            pci_lex$lex();
            return;
        }
        break;
    case 40:;
        {
            if (((i64)(*p).mode != (i64)5)) {
                return;
            }
;
        }
        break;
    } //SW
;
    (*pci_decls$pcltable)[(++(pci_decls$npcl))-1] = (*p);
    (*pci_decls$pcllines)[(pci_decls$npcl)-1] = pci_decls$lx.lineno;
}

// START
void pci_parse$start(void) {

}

i64 pci_support$docalldll(u64 fnindex,void *fnaddr,i64 (*revargs)[],i64 nargs,i64 nvars,i64 isfunc) {
        i64 args[100];
        i64 retval;
        i64 i;
    for (i=nargs;i>=(i64)1;--i) {
L70 :;
        args[(((nargs - i) + (i64)1))-1] = (*revargs)[(i)-1];
L71 :;
    }
L72 :;
    ;
    if ((fnaddr == 0)) {
        fnaddr = pci_support$getdllfnptr((i64)fnindex);
    }
;
    retval = (i64)mwindllc$os_calldllfunction((void (*)(void))fnaddr,(i64)73,nargs,(i64 (*)[])&args,0);
    return retval;
}

void pci_support$loadlibs(void) {
        i64 i;
    for (i=(i64)1;i<=pci_decls$nlibs;++i) {
L73 :;
        pci_decls$libinst[(i)-1] = mlinux$os_getdllinst(pci_decls$libnames[(i)-1]);
        if (!(!!(pci_decls$libinst[(i)-1]))) {
            pci_support$pcerror3((byte*)"Can't load lib:",pci_decls$libnames[(i)-1],(i64)0);
        }
;
L74 :;
    }
L75 :;
    ;
}

void (*pci_support$getdllfnptr(i64 fnindex))(void) {
        void (*fnaddr)(void);
        u8 *  procname;
        i64 i;
    fnaddr = (void (*)(void))pci_decls$extaddr[(fnindex)-1];
    if (!!(fnaddr)) {
        return fnaddr;
    }
;
    procname = (*pci_decls$extdef[(fnindex)-1]).name;
    for (i=(i64)1;i<=pci_decls$nlibs;++i) {
L76 :;
        fnaddr = (void (*)(void))mlinux$os_getdllprocaddr((i64)pci_decls$libinst[(i)-1],procname);
        if (!!(fnaddr)) {
            goto L78 ;
        }
;
L77 :;
    }
    {
        pci_support$pcerror3((byte*)"Can't find DLL func:",procname,(i64)0);
    }
L78 :;
    ;
    pci_decls$extaddr[(fnindex)-1] = fnaddr;
    return fnaddr;
}

void pci_support$initpcl1(void) {
        i64 n;
    n = (pci_decls$nsourcelines + (i64)10);
    pci_decls$pcltable = (struct pci_decls$pclrec (*)[])mlib$pcm_allocz((n * (i64)32));
    pci_decls$pcllabels = (u16 (*)[][8])mlib$pcm_allocz(((n * (i64)2) * (i64)8));
    pci_decls$pcllines = (i32 (*)[])mlib$pcm_allocz((n * (i64)4));
    pci_decls$npcl = (i64)0;
}

void pci_support$gerror(u8 *mess,u8 *mess2) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"PCLGEN:",NULL);
    msysc$m_print_str(mess,NULL);
    msysc$m_print_str(mess2,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    exit((i64)1);
}

void pci_support$pcerror3(u8 *mess,u8 *mess2,i64 pc) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"PCERROR:",NULL);
    msysc$m_print_str(mess,NULL);
    msysc$m_print_str(mess2,NULL);
    msysc$m_print_end();
    ;
    if (!!(pc)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)" on line",NULL);
        msysc$m_print_i64((i64)(*pci_decls$pcllines)[(pc)-1],NULL);
        msysc$m_print_end();
        ;
    }
;
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

void pci_support$loaderror(u8 *mess) {
    msysc$m_print_startcon();
    msysc$m_print_str(mess,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mlib$abortprogram((byte*)"");
}

void pci_support$addlib(struct pci_decls$strec *d) {
        i64 i;
    for (i=(i64)1;i<=pci_decls$nlibs;++i) {
L79 :;
        if (!!(mlib$eqstring((*d).name,pci_decls$libnames[(i)-1]))) {
            return;
        }
;
L80 :;
    }
L81 :;
    ;
    if ((pci_decls$nlibs >= (i64)20)) {
        pci_support$serror((byte*)"Too many libs");
    }
;
    ++(pci_decls$nlibs);
    pci_decls$libnames[(pci_decls$nlibs)-1] = mlib$pcm_copyheapstring((*d).name);
    (*d).libindex = pci_decls$nlibs;
    (*d).idtype = (i64)6;
}

i64 pci_support$serror(u8 *mess) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Syntax error:",NULL);
    msysc$m_print_str(mess,NULL);
    msysc$m_print_str((byte*)"on line",NULL);
    msysc$m_print_i64(pci_decls$lx.lineno,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    exit((i64)1);
    return (i64)0;
}

void pci_support$docmdskip(void) {
        struct pci_decls$strec *  d;
    d = pci_lex$findsymbol((byte*)"$cmdskip");
    if (!!(d)) {
        (*(i64 *)(*d).staticaddr) = pci_decls$cmdskip;
    }
;
}

void pci_support$adddataref(struct pci_decls$strec **pd) {
        struct pci_decls$datarec *  p;
    p = (struct pci_decls$datarec *)mlib$pcm_allocz((i64)16);
    (*p).pdef = (struct pci_decls$strec **)pd;
    (*p).nextdata = (struct pci_decls$datarec *)pci_decls$datalist;
    pci_decls$datalist = (struct pci_decls$datarec *)p;
}

void pci_support$addlabelref(i64 *plab) {
        struct pci_decls$labelrec *  p;
    p = (struct pci_decls$labelrec *)mlib$pcm_allocz((i64)16);
    (*p).plabel = plab;
    (*p).nextlabel = (struct pci_decls$labelrec *)pci_decls$labellist;
    pci_decls$labellist = (struct pci_decls$labelrec *)p;
}

void pci_support$updatedatarefs(void) {
        struct pci_decls$datarec *  p;
        struct pci_decls$labelrec *  q;
        u64 *  pd;
        struct pci_decls$strec *  d;
        i64 lab;
    p = (struct pci_decls$datarec *)pci_decls$datalist;
    L82 :;
    while (!!(p)) {
        d = (*(*p).pdef);
        pd = (u64 *)(*p).pdef;
                {i64 $temp = (i64)(*d).idtype;
if (($temp==(i64)2)) {
            (*pd) = (u64)(*d).staticaddr;
        }
        else if (($temp==(i64)1)) {
            (*pd) = (u64)(*d).pcindex;
        }
        else {
            pci_support$pcerror3((byte*)"DATALIST: Can't fixup",(*d).name,(i64)0);
        }
        };
L83 :;
        p = (struct pci_decls$datarec *)(*p).nextdata;
L85 :;
            }
L84 :;
    ;
    q = (struct pci_decls$labelrec *)pci_decls$labellist;
    L86 :;
    while (!!(q)) {
        lab = (*(*q).plabel);
        (*(*q).plabel) = (i64)pci_decls$labeltable[(lab)-1];
L87 :;
        q = (struct pci_decls$labelrec *)(*q).nextlabel;
L89 :;
            }
L88 :;
    ;
}

void pci_support$storebit(i64 *p,i64 i,i64 j,i64 x) {
        i64 bitlength;
        u64 mask1;
        u64 mask2;
    bitlength = ((j - i) + (i64)1);
    if ((bitlength == (i64)64)) {
        (*p) = x;
        return;
    }
;
    mask1 = (u64)18446744073709551614u;
    if ((bitlength > (i64)1)) {
        mask1 = (mask1 << (bitlength - (i64)1));
    }
;
    mask1 = ~(mask1);
    if (!!(i)) {
        mask1 = (mask1 << i);
    }
;
    mask2 = ~(mask1);
    if (!!(i)) {
        x = (x << i);
    }
;
    (*p) = (((*p) & (i64)mask2) | (x & (i64)mask1));
}

// START
void pci_support$start(void) {

}

// START
void pci_tables$start(void) {

}

int main(int _nargs, char** _args, char** _envstrings) {
    msysc$m_init(_nargs, (void*)_args, (void*)_envstrings);

// call main-start() routines...
    msysc$start();
    $pci$start();

    $pci$getinputoptions();
    $pci$loadprogram();
    pci_parse$parseprogram();
    if (((i64)$pci$passlevel >= (i64)2)) {
        pci_fixup$fixuppcl();
        if (((i64)$pci$passlevel == (i64)3)) {
            pci_execc$runpcl();
        }
;
    }
;
    return 0;
}

static void $pci$getinputoptions(void) {
        i64 paramno;
        i64 pmtype;
        u8 *  name;
        u8 *  value;
        i64 sw;
    paramno = (i64)1;
    L90 :;
    while (!!((pmtype = mlib$nextcmdparamnew(&paramno,&name,&value,(byte*)"pcl")))) {
        if ((pmtype==(i64)1)) {
            mlib$convlcstring(name);
            for (sw=(i64)1;sw<=(i64)8;++sw) {
L93 :;
                if (!!(mlib$eqstring(name,$pci$optionnames[(sw)-1]))) {
                    $pci$do_option(sw,value);
                    goto L95 ;
                }
;
L94 :;
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
L95 :;
            ;
        }
        else if ((pmtype==(i64)2)) {
            if (!!(pci_decls$inputfile)) {
                pci_support$loaderror((byte*)"Too many input files");
            }
;
            mlib$convlcstring(name);
            if (!!(mlib$eqstring(mlib$extractext(name,(i64)0),(byte*)"m"))) {
                name = mlib$changeext(name,(byte*)"pcl");
            }
;
            pci_decls$inputfile = mlib$pcm_copyheapstring(name);
            pci_decls$cmdskip = ((paramno - (i64)1) + msysc$$cmdskip);
            goto L92 ;
        }
        else {
            pci_support$loaderror((byte*)"Invalid params");
        }
;
L91 :;
    }
L92 :;
    ;
    if ((pci_decls$inputfile == 0)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Usage:",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"\t",NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((*msysc$cmdparams)[((i64)0)],NULL);
        msysc$m_print_str((byte*)"filename[.pcl]",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        exit(0);
    }
;
    if (((i64)$pci$passlevel == (i64)0)) {
        $pci$passlevel = (i64)3;
    }
;
}

static void $pci$do_option(i64 sw,u8 *value) {
    switch (sw) {
    case 1:;
        {
            $pci$passlevel = (i64)1;
        }
        break;
    case 2:;
        {
            $pci$passlevel = (i64)2;
        }
        break;
    case 3:;
        {
            $pci$passlevel = (i64)1;
            $pci$fshowpcl = (i64)1;
        }
        break;
    case 4:;
        {
            $pci$passlevel = (i64)2;
            $pci$fshowpcl = (i64)1;
        }
        break;
    case 5:;
        {
            $pci$passlevel = (i64)3;
        }
        break;
    case 6:;
        {
            $pci$fshowpcl = (i64)1;
        }
        break;
    case 7:;
        {
            $pci$fshowst = (i64)1;
        }
        break;
    case 8:;
        {
            $pci$fshowlibs = (i64)1;
        }
        break;
    } //SW
;
}

u8 *$pci$addstr(u8 *s,u8 *t) {
        static u8 str[256];
    strcpy(str,s);
    strcat(str,t);
    return str;
}

static void $pci$loadprogram(void) {
        u8 *  p;
        i64 i;
    pci_decls$psource = (u8 *)mlib$readfile(pci_decls$inputfile);
    if (((pci_decls$psource == 0) || ((i64)(u64)(*pci_decls$psource) == (i64)0))) {
        pci_support$loaderror($pci$addstr((byte*)"Can't load ",pci_decls$inputfile));
    }
;
    p = pci_decls$psource;
    pci_decls$nsourcelines = (i64)0;
    L96 :;
        {u64 $temp = (u64)(*p);
if (($temp==(u64)10u)) {
        ++(pci_decls$nsourcelines);
        ++(p);
    }
    else if (($temp==(u64)0u)) {
        if (((i64)(u64)(*(p - (i64)1)) != (i64)10)) {
            ++(pci_decls$nsourcelines);
        }
;
        goto L97 ;
    }
    else {
        ++(p);
    }
    }goto L96 ;
L97 :;
    ;
    pci_decls$sourcelines = (u8 *(*)[])mlib$pcm_alloc((pci_decls$nsourcelines * (i64)8));
    p = pci_decls$psource;
    i = (i64)1;
    (*pci_decls$sourcelines)[((i64)1)-1] = pci_decls$psource;
    L98 :;
        {u64 $temp = (u64)(*p);
if (($temp==(u64)13u)) {
        (*(p)++) = (u64)0u;
    }
    else if (($temp==(u64)10u)) {
        (*(p)++) = (u64)0u;
        if (!!((u64)(*p))) {
            (*pci_decls$sourcelines)[(++(i))-1] = p;
        }
;
    }
    else if (($temp==(u64)0u)) {
        goto L99 ;
    }
    else {
        ++(p);
    }
    }goto L98 ;
L99 :;
    ;
}

static void $pci$writeswitch(void) {
        i64 $av_1;
        u8 *  x;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"\tdoswitch getopcode",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for ($av_1=(i64)0;$av_1<=(i64)409;++$av_1) {
    (x = pci_tables$jcodenames[($av_1)]);
L100 :;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"\twhen",NULL);
        msysc$m_print_str(x,NULL);
        msysc$m_print_str((byte*)"then",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"\t\tunimpl",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"\t\tsteppc",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startcon();
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L101 :;
    }L102 :;
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"\telse",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"unimpl::",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"\tend doswitch",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

// START
void $pci$start(void) {
    pci_decls$start();
    pci_execc$start();
    pci_fixup$start();
    pci_lex$start();
    pci_parse$start();
    pci_support$start();
    pci_tables$start();

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
L104 :;
        msysc$sysparams[(i)-1] = (*args)[(i)-1];
L105 :;
    }
L106 :;
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
    L107 :;
    while (1) {
        c = (u64)(*msysc$fmtstr);
        switch ((i64)(u64)c) {
        case 35:;
            {
                if (!!(lastx)) {
                    goto L109 ;
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
L109 :;
;
            ++(n);
            ++(msysc$fmtstr);
        }
        } //SW
;
    }
L108 :;
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
    L110 :;
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
                goto L113 ;
;
            }
            break;
        default: {
            if ((((u64)c >= '0') && ((u64)c <= '9'))) {
                n = (i64)((u64)c - '0');
                L114 :;
                while (1) {
                    c = (u64)(*s);
                    if (((i64)(u64)(*s) == (i64)0)) {
                        goto L115 ;
                    }
;
                    if ((((u64)c >= '0') && ((u64)c <= '9'))) {
                        ++(s);
                        n = (((n * (i64)10) + (i64)(u64)c) - (i64)48);
                    }
                    else {
                        goto L115 ;
                    }
;
                }
L115 :;
                ;
                //gotwidth:
L113 :;
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
L111 :;
    }
L112 :;
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
L116 :;
        if (((i64)(u64)(*p) == (i64)0)) {
            goto L118 ;
        }
;
        (*q) = (u64)(*p);
        ++(q);
        ++(p);
L117 :;
    }
L118 :;
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
L119 :;
            (*t) = (u64)(*fmt).padchar;
            ++(t);
L120 :;
        }
L121 :;
        ;
        (*t) = (u64)0u;
    }
    else if (((u64)(*fmt).justify == 'R')) {
        if (((((u64)(*fmt).padchar == '0') && !!((i64)(*fmt).base)) && (((u64)(*s) == '-') || ((u64)(*s) == '+')))) {
            (*t) = (u64)(*s);
            ++(t);
            $av_2 = (w - n);
            while ($av_2-- > 0) {
L122 :;
                (*t) = (u64)(*fmt).padchar;
                ++(t);
L123 :;
            }
L124 :;
            ;
            strncpy(t,(s + (i64)1),(u64)(n - (i64)1));
            (*((t + n) - (i64)1)) = (u64)0u;
        }
        else {
            $av_3 = (w - n);
            while ($av_3-- > 0) {
L125 :;
                (*t) = (u64)(*fmt).padchar;
                ++(t);
L126 :;
            }
L127 :;
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
L128 :;
            (*t) = (u64)(*fmt).padchar;
            ++(t);
L129 :;
        }
L130 :;
        ;
        strncpy(t,s,(u64)n);
        t += n;
        $av_5 = ((w - n) - m);
        while ($av_5-- > 0) {
L131 :;
            (*t) = (u64)(*fmt).padchar;
            ++(t);
L132 :;
        }
L133 :;
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
    L134 :;
    do {
        t[(++(i))] = (u64)msysc$digits[((i64)(aa % base))];
        aa = (aa / base);
        ++(k);
        if (((!!(sep) && ((i64)aa != (i64)0)) && (k == g))) {
            t[(++(i))] = (u64)sep;
            k = (i64)0;
        }
;
L135 :;
    }
    while (!((i64)aa == (i64)0));
L136 :;
    ;
    j = i;
    s0 = s;
    L137 :;
    while (!!(i)) {
        (*s) = (u64)t[((i)--)];
        ++(s);
L138 :;
    }
L139 :;
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
    L140 :;
    while (!!(i)) {
        --(s);
        (*s) = (u64)t[(((i)-- - (i64)1))];
        if (((!!(sep) && !!(i)) && (++(k) == g))) {
            --(s);
            (*s) = (u64)sep;
            k = (i64)0;
        }
;
L141 :;
    }
L142 :;
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
    L143 :;
    while ((((u64)(*s) == ' ') || ((i64)(u64)(*s) == (i64)9))) {
        ++(s);
L144 :;
    }
L145 :;
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
    L146 :;
    while (!!((u64)(*s))) {
        c = (u64)(*(s)++);
        switch ((i64)(u64)c) {
        case 32:;
        case 9:;
        case 44:;
        case 61:;
            {
                if ((!!((u64)quotechar) || (p == s))) {
                    goto L149 ;
;
                }
;
                msysc$termchar = (i64)(u64)c;
                goto L148 ;
            }
            break;
        default: {
            //normalchar:
L149 :;
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
                    goto L148 ;
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
L147 :;
    }
L148 :;
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
    L150 :;
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
                goto L151 ;
            }
            break;
        default: {
            msysc$itemerror = (i64)1;
            goto L152 ;
        }
        } //SW
;
        if (((i64)(u64)d >= base)) {
            msysc$itemerror = (i64)1;
            goto L152 ;
        }
;
        aa = (u64)(((i64)aa * base) + (i64)(u64)d);
L151 :;
    }
L152 :;
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
L153 :;
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L154 :;
    }
L155 :;
    ;
}

static void msysc$iconvucn(u8 *s,i64 n) {
        i64 $av_1;
    $av_1 = n;
    while ($av_1-- > 0) {
L156 :;
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L157 :;
    }
L158 :;
    ;
}

static void msysc$convlcstring(u8 *s) {
    L159 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L160 :;
    }
L161 :;
    ;
}

static void msysc$convucstring(u8 *s) {
    L162 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L163 :;
    }
L164 :;
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
L165 :;
        j = (i64)1;
        k = (i64)16;
        L168 :;
        while ((i > k)) {
            k = (k << (i64)1);
            ++(j);
L169 :;
        }
L170 :;
        ;
        mlib$sizeindextable[(i)] = j;
L166 :;
    }
L167 :;
    ;
    mlib$allocupper[((i64)1)] = (u64)16u;
    size = (i64)16;
    for (i=(i64)2;i<=(i64)27;++i) {
L171 :;
        size *= (i64)2;
        mlib$allocupper[(i)] = (u64)size;
        if ((size >= (i64)33554432)) {
            k = i;
            goto L173 ;
        }
;
L172 :;
    }
L173 :;
    ;
    for (i=(k + (i64)1);i<=(i64)300;++i) {
L174 :;
        size += (i64)33554432;
        if ((size < (i64)8589934592)) {
            mlib$allocupper[(i)] = (u64)size;
            mlib$maxmemory = (u64)size;
        }
        else {
            mlib$maxalloccode = (i - (i64)1);
            goto L176 ;
        }
;
L175 :;
    }
L176 :;
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
        handlex = mlinux$os_getstdin();
    }
;
    if ((handlex == 0)) {
        n = (i64)0;
        p = buffer;
        L177 :;
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
L178 :;
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
    L179 :;
    while (((p >= buffer) && (((i64)(u64)(*p) == (i64)13) || ((i64)(u64)(*p) == (i64)10)))) {
        if ((((i64)(u64)(*p) == (i64)13) || ((i64)(u64)(*p) == (i64)10))) {
            crseen = (i64)1;
        }
;
        (*(p)--) = (u64)0u;
L180 :;
    }
L181 :;
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
L182 :;
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L183 :;
    }
L184 :;
    ;
}

void mlib$iconvucn(u8 *s,i64 n) {
        i64 $av_1;
    $av_1 = n;
    while ($av_1-- > 0) {
L185 :;
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L186 :;
    }
L187 :;
    ;
}

u8 *mlib$convlcstring(u8 *s) {
        u8 *  s0;
    s0 = s;
    L188 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L189 :;
    }
L190 :;
    ;
    return s0;
}

u8 *mlib$convucstring(u8 *s) {
        u8 *  s0;
    s0 = s;
    L191 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L192 :;
    }
L193 :;
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
    L194 :;
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
L195 :;
    }
L196 :;
    ;
    return (byte*)"";
}

u8 *mlib$extractpath(u8 *s) {
        static u8 str[260];
        u8 *  t;
        i64 n;
    t = ((s + strlen(s)) - (i64)1);
    L197 :;
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
L198 :;
    }
L199 :;
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
L200 :;
            str[((slen + i))-1] = (u64)padch;
L201 :;
        }
L202 :;
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
L203 :;
        str[(i)-1] = (u64)ch;
L204 :;
    }
L205 :;
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
L206 :;
;
    (*value) = 0;
    (*name) = 0;
    if (!!(infile)) {
        if ((mlib$readnextfileitem(&fileptr,&item) == (i64)0)) {
            mlib$pcm_free((void *)filestart,atsize);
            infile = (i64)0;
            goto L206 ;
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
            goto L206 ;
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
L207 :;
;
    L208 :;
    while (1) {
                {u64 $temp = (u64)(*p);
if (($temp==' ') || ($temp==(u64)9u) || ($temp==(u64)13u) || ($temp==(u64)10u)) {
            ++(p);
        }
        else if (($temp==(u64)26u) || ($temp==(u64)0u)) {
            return (i64)0;
        }
        else {
            goto L209 ;
        }
        };
    }
L209 :;
    ;
        {u64 $temp = (u64)(*p);
if (($temp=='!') || ($temp=='#')) {
        ++(p);
        L210 :;
                {u64 $temp = (u64)(*(p)++);
if (($temp==(u64)10u)) {
            goto L207 ;
;
        }
        else if (($temp==(u64)26u) || ($temp==(u64)0u)) {
            (*fileptr) = (p - (i64)1);
            return (i64)0;
        }
        else {
        }
        }goto L210 ;
L211 :;
        ;
    }
    };
        {u64 $temp = (u64)(*p);
if (($temp=='"')) {
        pstart = ++(p);
        L212 :;
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
                goto L213 ;
            }
            };
            ++(p);
        }
L213 :;
        ;
    }
    else {
        pstart = p;
        L214 :;
        while (1) {
                        {u64 $temp = (u64)(*p);
if (($temp==(u64)0u) || ($temp==(u64)26u)) {
                pend = p;
                goto L215 ;
            }
            else if (($temp==' ') || ($temp==(u64)9u) || ($temp==',') || ($temp==(u64)13u) || ($temp==(u64)10u)) {
                pend = (p)++;
                goto L215 ;
            }
            };
            ++(p);
        }
L215 :;
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
L216 :;
        strcat(s,padchar);
L217 :;
    }
L218 :;
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
    L219 :;
    do {
        x = ((r64)mlib$mrandomp() / (double)9223372036854775800.);
L220 :;
    }
    while (!(x != (double)1.));
L221 :;
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
L222 :;
        if (!!(mlib$eqstring(msysc$m_get_procname(i),name))) {
            return msysc$m_get_procaddr(i);
        }
;
L223 :;
    }
L224 :;
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
        struct mlinux$timeval tv;
        struct mlinux$tm_rec tmr;
    gettimeofday(&tv,0);
    gmtime_r(&tv.tv_sec,&tmr);
    (*tm).year = ((i64)tmr.tm_year + (i64)1900);
    (*tm).month = ((i64)tmr.tm_mon + (i64)1);
    (*tm).dayofweek = ((i64)tmr.tm_wday + (i64)1);
    (*tm).day = (i64)tmr.tm_mday;
    (*tm).hour = (i64)tmr.tm_hour;
    (*tm).minute = (i64)tmr.tm_min;
    (*tm).second = (i64)tmr.tm_sec;
    (*tm).milliseconds = (tv.tv_usec / (i64)1000);
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

