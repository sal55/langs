
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
struct mlinux$rsystemtime;

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
static void $clex$readtoken(void);
static void $clex$printsymbol(void);
static void $clex$readname(i64 c);
static void $clex$readdecdigits(i64 c);
static void $clex$readhexdigits(void);
static void $clex$readbindigits(void);
static void $clex$readstring(i64 termchar);
static void $clex$readlinecomment(void);
static void $clex$readblockcomment(void);
static void $clex$readdecimal(u8 *p,i64 length);
static void $clex$readhex(u8 *p,i64 length);
static void $clex$readbin(u8 *p,i64 length);
static r64 $clex$readreal(u8 *istr,i64 ilength,u8 *fstr,i64 flength,i64 expon);
static void $clex$printstr(u8 *s,i64 length);
static void $clex$initdata(void);
void $clex$start(void);
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
static u8 *  $clex$symbolnames[30] = {
    (byte*)"error_sym",
    (byte*)"dot_sym",
    (byte*)"comma_sym",
    (byte*)"semi_sym",
    (byte*)"question_sym",
    (byte*)"colon_sym",
    (byte*)"assign_sym",
    (byte*)"lbrack_sym",
    (byte*)"rbrack_sym",
    (byte*)"lsq_sym",
    (byte*)"rsq_sym",
    (byte*)"lcurly_sym",
    (byte*)"rcurly_sym",
    (byte*)"addr_sym",
    (byte*)"deref_sym",
    (byte*)"ellipsis_sym",
    (byte*)"op_sym",
    (byte*)"opto_sym",
    (byte*)"eol_sym",
    (byte*)"eof_sym",
    (byte*)"hash_sym",
    (byte*)"incr_sym",
    (byte*)"name_sym",
    (byte*)"intconst_sym",
    (byte*)"realconst_sym",
    (byte*)"charconst_sym",
    (byte*)"wcharconst_sym",
    (byte*)"stringconst_sym",
    (byte*)"wstringconst_sym",
    (byte*)"kdummy_sym"
};
static u8 *  $clex$jtagnames[35] = {
    (byte*)"j_eq",
    (byte*)"j_ne",
    (byte*)"j_lt",
    (byte*)"j_le",
    (byte*)"j_gt",
    (byte*)"j_ge",
    (byte*)"j_add",
    (byte*)"j_sub",
    (byte*)"j_mul",
    (byte*)"j_div",
    (byte*)"j_rem",
    (byte*)"j_iand",
    (byte*)"j_ior",
    (byte*)"j_ixor",
    (byte*)"j_shl",
    (byte*)"j_shr",
    (byte*)"j_andand",
    (byte*)"j_oror",
    (byte*)"j_neg",
    (byte*)"j_abs",
    (byte*)"j_not",
    (byte*)"j_inot",
    (byte*)"j_preincr",
    (byte*)"j_predecr",
    (byte*)"j_addto",
    (byte*)"j_subto",
    (byte*)"j_multo",
    (byte*)"j_divto",
    (byte*)"j_remto",
    (byte*)"j_iandto",
    (byte*)"j_iorto",
    (byte*)"j_ixorto",
    (byte*)"j_shlto",
    (byte*)"j_shrto",
    (byte*)"j_dummy"
};
static byte *  $clex$lxsptr;
static i64 $clex$lxlineno;
static i64 $clex$lxfileno;
static i64 $clex$lxsymbol;
static i64 $clex$lxsubcode;
static i64 $clex$lxvalue;
static r64 $clex$lxvaluex;
static u8 *  $clex$lxvaluestr;
static i64 $clex$lxlength;
static u8 $clex$alphamap[256];
static void *  msysc$_fnaddresses[]= {
    &main,
    &$clex$readtoken,
    &$clex$printsymbol,
    &$clex$readname,
    &$clex$readdecdigits,
    &$clex$readhexdigits,
    &$clex$readbindigits,
    &$clex$readstring,
    &$clex$readlinecomment,
    &$clex$readblockcomment,
    &$clex$readdecimal,
    &$clex$readhex,
    &$clex$readbin,
    &$clex$readreal,
    &$clex$printstr,
    &$clex$initdata,
    &$clex$start,
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
    (byte*)"readtoken",
    (byte*)"printsymbol",
    (byte*)"readname",
    (byte*)"readdecdigits",
    (byte*)"readhexdigits",
    (byte*)"readbindigits",
    (byte*)"readstring",
    (byte*)"readlinecomment",
    (byte*)"readblockcomment",
    (byte*)"readdecimal",
    (byte*)"readhex",
    (byte*)"readbin",
    (byte*)"readreal",
    (byte*)"printstr",
    (byte*)"initdata",
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
static i64 msysc$_fnnprocs=229;
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
int main(int _nargs, char** _args, char** _envstrings) {
    msysc$m_init(_nargs, (void*)_args, (void*)_envstrings);

// call main-start() routines...
    msysc$start();
    $clex$start();

        u8 *  psource;
        i64 ntokens;
        i64 nlines;
        i64 nchars;
        i64 nn;
        i64 t;
        r64 tsecs;
        u8 *  infile;
        i64 $av_1;
    infile = (byte*)"sqlite3.c";
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Scanning:",NULL);
    msysc$m_print_str(infile,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    $clex$initdata();
    psource = (u8 *)mlib$readfile(infile);
    t = mlinux$os_clock();
    if ((psource == 0)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Can't open",NULL);
        msysc$m_print_str(infile,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        exit(0);
    }
;
    nchars = (nlines = (ntokens = (i64)0));
    nn = (i64)10;
    $av_1 = nn;
    while ($av_1-- > 0) {
L1 :;
        $clex$lxsptr = (byte *)psource;
        $clex$lxfileno = ($clex$lxlineno = (i64)1);
        L4 :;
        do {
            $clex$readtoken();
            ++(ntokens);
L5 :;
        }
        while (!($clex$lxsymbol == (i64)20));
L6 :;
        ;
        nlines += $clex$lxlineno;
        nchars += mlib$rfsize;
L2 :;
    }
L3 :;
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Finished",NULL);
    msysc$m_print_str((byte*)"NLINES=",NULL);
    msysc$m_print_i64(nlines,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    t = (mlinux$os_clock() - t);
    tsecs = ((r64)t / (double)1000.);
    msysc$m_print_startcon();
    msysc$m_print_r64(tsecs,NULL);
    msysc$m_print_str((byte*)"Seconds",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Source file=",NULL);
    msysc$m_print_str(infile,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_i64(ntokens,NULL);
    msysc$m_print_str((byte*)"Tokens,",NULL);
    msysc$m_print_i64(nlines,NULL);
    msysc$m_print_str((byte*)"Lines,",NULL);
    msysc$m_print_i64(nchars,NULL);
    msysc$m_print_str((byte*)"Chars",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_i64((i64)((r64)nlines / tsecs),(byte*)"s,");
    msysc$m_print_str((byte*)"Lines per second",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_i64((i64)((r64)ntokens / tsecs),(byte*)"s,");
    msysc$m_print_str((byte*)"Tokens per second",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_i64((i64)((r64)nchars / tsecs),(byte*)"s,");
    msysc$m_print_str((byte*)"Chars per second",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_r64(tsecs,NULL);
    msysc$m_print_str((byte*)" Seconds",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    return 0;
}

static void $clex$readtoken(void) {
        i64 c;
    $clex$lxsubcode = (i64)0;
    L7 :;
    switch ((c = (i64)(*($clex$lxsptr)++))) {
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
        {
            //freddy:
L9 :;
;
            $clex$lxvaluestr = (u8 *)($clex$lxsptr - (i64)1);
            L10 :;
            while (!!((u64)$clex$alphamap[((i64)(*($clex$lxsptr)++))])) {
L11 :;
            }
L12 :;
            ;
            $clex$lxsymbol = (i64)23;
            $clex$lxlength = ($clex$lxsptr - $clex$lxvaluestr);
            --($clex$lxsptr);
            goto L8 ;
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
                        {i64 $temp = (i64)(*$clex$lxsptr);
if (($temp==(i64)120) || ($temp==(i64)88)) {
                $clex$readhexdigits();
            }
            else if (($temp==(i64)98) || ($temp==(i64)66)) {
                $clex$readbindigits();
            }
            else {
                $clex$readdecdigits(c);
            }
            };
            --($clex$lxsptr);
            goto L8 ;
        }
        break;
    case 32:;
    case 9:;
        {
        }
        break;
    case 13:;
        {
            if (((i64)(*$clex$lxsptr) == (i64)10)) {
                ++($clex$lxsptr);
            }
;
            ++($clex$lxlineno);
            $clex$lxsymbol = (i64)19;
            goto L8 ;
        }
        break;
    case 10:;
        {
            ++($clex$lxlineno);
            $clex$lxsymbol = (i64)19;
            goto L8 ;
        }
        break;
    case 26:;
    case 0:;
        {
            $clex$lxsymbol = (i64)20;
            goto L8 ;
        }
        break;
    case 39:;
        {
            $clex$readstring(c);
            goto L8 ;
        }
        break;
    case 34:;
        {
            $clex$readstring(c);
            goto L8 ;
        }
        break;
    case 35:;
        {
            $clex$lxsymbol = (i64)21;
            goto L8 ;
        }
        break;
    case 92:;
        {
        }
        break;
    case 43:;
        {
                        {i64 $temp = (i64)(*$clex$lxsptr);
if (($temp==(i64)43)) {
                ++($clex$lxsptr);
                $clex$lxsymbol = (i64)22;
                $clex$lxsubcode = (i64)23;
            }
            else if (($temp==(i64)61)) {
                ++($clex$lxsptr);
                $clex$lxsymbol = (i64)18;
                $clex$lxsubcode = (i64)25;
            }
            else {
                $clex$lxsymbol = (i64)17;
                $clex$lxsubcode = (i64)7;
            }
            };
            goto L8 ;
        }
        break;
    case 45:;
        {
                        {i64 $temp = (i64)(*$clex$lxsptr);
if (($temp==(i64)45)) {
                ++($clex$lxsptr);
                $clex$lxsymbol = (i64)22;
                $clex$lxsubcode = (i64)24;
            }
            else if (($temp==(i64)61)) {
                ++($clex$lxsptr);
                $clex$lxsymbol = (i64)18;
                $clex$lxsubcode = (i64)26;
            }
            else if (($temp==(i64)62)) {
                $clex$lxsymbol = (i64)15;
            }
            else {
                $clex$lxsymbol = (i64)17;
                $clex$lxsubcode = (i64)8;
            }
            };
            goto L8 ;
        }
        break;
    case 42:;
        {
            if (((i64)(*$clex$lxsptr) == (i64)61)) {
                ++($clex$lxsptr);
                $clex$lxsymbol = (i64)18;
                $clex$lxsubcode = (i64)27;
            }
            else {
                $clex$lxsymbol = (i64)17;
                $clex$lxsubcode = (i64)9;
            }
;
            goto L8 ;
        }
        break;
    case 47:;
        {
                        {i64 $temp = (i64)(*$clex$lxsptr);
if (($temp==(i64)47)) {
                $clex$readlinecomment();
            }
            else if (($temp==(i64)42)) {
                $clex$readblockcomment();
            }
            else if (($temp==(i64)61)) {
                $clex$lxsymbol = (i64)18;
                $clex$lxsubcode = (i64)28;
                goto L8 ;
            }
            else {
                $clex$lxsymbol = (i64)17;
                $clex$lxsubcode = (i64)10;
                goto L8 ;
            }
            };
        }
        break;
    case 37:;
        {
            if (((i64)(*$clex$lxsptr) == (i64)61)) {
                ++($clex$lxsptr);
                $clex$lxsymbol = (i64)18;
                $clex$lxsubcode = (i64)29;
            }
            else {
                $clex$lxsymbol = (i64)17;
                $clex$lxsubcode = (i64)11;
            }
;
            goto L8 ;
        }
        break;
    case 40:;
        {
            $clex$lxsymbol = (i64)8;
            goto L8 ;
        }
        break;
    case 41:;
        {
            $clex$lxsymbol = (i64)9;
            goto L8 ;
        }
        break;
    case 123:;
        {
            $clex$lxsymbol = (i64)12;
            goto L8 ;
        }
        break;
    case 125:;
        {
            $clex$lxsymbol = (i64)13;
            goto L8 ;
        }
        break;
    case 60:;
        {
            $clex$lxsymbol = (i64)17;
                        {i64 $temp = (i64)(*$clex$lxsptr);
if (($temp==(i64)60)) {
                ++($clex$lxsptr);
                if (((i64)(*$clex$lxsptr) == (i64)61)) {
                    ++($clex$lxsptr);
                    $clex$lxsymbol = (i64)18;
                    $clex$lxsubcode = (i64)33;
                }
                else {
                    $clex$lxsubcode = (i64)15;
                }
;
            }
            else if (($temp==(i64)61)) {
                ++($clex$lxsptr);
                $clex$lxsubcode = (i64)4;
            }
            else {
                $clex$lxsubcode = (i64)3;
            }
            };
            goto L8 ;
        }
        break;
    case 62:;
        {
            $clex$lxsymbol = (i64)17;
                        {i64 $temp = (i64)(*$clex$lxsptr);
if (($temp==(i64)62)) {
                ++($clex$lxsptr);
                if (((i64)(*$clex$lxsptr) == (i64)61)) {
                    ++($clex$lxsptr);
                    $clex$lxsymbol = (i64)18;
                    $clex$lxsubcode = (i64)34;
                }
                else {
                    $clex$lxsubcode = (i64)15;
                }
;
            }
            else if (($temp==(i64)61)) {
                ++($clex$lxsptr);
                $clex$lxsubcode = (i64)6;
            }
            else {
                $clex$lxsubcode = (i64)5;
            }
            };
            goto L8 ;
        }
        break;
    case 91:;
        {
            $clex$lxsymbol = (i64)10;
            goto L8 ;
        }
        break;
    case 93:;
        {
            $clex$lxsymbol = (i64)11;
            goto L8 ;
        }
        break;
    case 46:;
        {
            switch ((i64)(*$clex$lxsptr)) {
            case 101:;
            case 69:;
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
                    $clex$readdecdigits((i64)(*$clex$lxsptr));
                }
                break;
            default: {
                $clex$lxsymbol = (i64)2;
            }
            } //SW
;
            goto L8 ;
        }
        break;
    case 44:;
        {
            $clex$lxsymbol = (i64)3;
            goto L8 ;
        }
        break;
    case 58:;
        {
            $clex$lxsymbol = (i64)6;
            goto L8 ;
        }
        break;
    case 59:;
        {
            $clex$lxsymbol = (i64)4;
            goto L8 ;
        }
        break;
    case 94:;
        {
            if (((i64)(*$clex$lxsptr) == (i64)61)) {
                ++($clex$lxsptr);
                $clex$lxsymbol = (i64)18;
                $clex$lxsubcode = (i64)32;
            }
            else {
                $clex$lxsymbol = (i64)17;
                $clex$lxsubcode = (i64)14;
            }
;
        }
        break;
    case 124:;
        {
                        {i64 $temp = (i64)(*$clex$lxsptr);
if (($temp==(i64)124)) {
                ++($clex$lxsptr);
                $clex$lxsymbol = (i64)17;
                $clex$lxsubcode = (i64)18;
            }
            else if (($temp==(i64)61)) {
                $clex$lxsymbol = (i64)18;
                $clex$lxsubcode = (i64)31;
            }
            else {
                $clex$lxsymbol = (i64)17;
                $clex$lxsubcode = (i64)13;
            }
            };
            goto L8 ;
        }
        break;
    case 38:;
        {
                        {i64 $temp = (i64)(*$clex$lxsptr);
if (($temp==(i64)38)) {
                ++($clex$lxsptr);
                $clex$lxsymbol = (i64)17;
                $clex$lxsubcode = (i64)17;
            }
            else if (($temp==(i64)61)) {
                $clex$lxsymbol = (i64)18;
                $clex$lxsubcode = (i64)30;
            }
            else {
                $clex$lxsymbol = (i64)17;
                $clex$lxsubcode = (i64)12;
            }
            };
            goto L8 ;
        }
        break;
    case 63:;
        {
            $clex$lxsymbol = (i64)5;
            goto L8 ;
        }
        break;
    case 126:;
        {
            $clex$lxsymbol = (i64)17;
            $clex$lxsubcode = (i64)22;
            goto L8 ;
        }
        break;
    case 61:;
        {
            if (((i64)(*$clex$lxsptr) == (i64)61)) {
                ++($clex$lxsptr);
                $clex$lxsymbol = (i64)17;
                $clex$lxsubcode = (i64)1;
            }
            else {
                $clex$lxsymbol = (i64)7;
            }
;
            goto L8 ;
        }
        break;
    case 33:;
        {
            if (((i64)(*$clex$lxsptr) == (i64)61)) {
                ++($clex$lxsptr);
                $clex$lxsymbol = (i64)17;
                $clex$lxsubcode = (i64)2;
            }
            else {
                $clex$lxsymbol = (i64)17;
                $clex$lxsubcode = (i64)21;
            }
;
            goto L8 ;
        }
        break;
    default: {
        $clex$lxsymbol = (i64)1;
        $clex$lxvalue = c;
        goto L8 ;
    }
    } //SW
goto L7 ;
L8 :;
    ;
}

static void $clex$printsymbol(void) {
    if (($clex$lxsymbol == (i64)0)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"ZERO?",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        return;
    }
;
    msysc$m_print_startcon();
    msysc$m_print_str($clex$symbolnames[($clex$lxsymbol)-1],NULL);
    msysc$m_print_str((byte*)"\t",NULL);
    msysc$m_print_end();
    ;
    if (($clex$lxsymbol==(i64)17) || ($clex$lxsymbol==(i64)18)) {
        msysc$m_print_startcon();
        msysc$m_print_str($clex$jtagnames[($clex$lxsubcode)-1],NULL);
        msysc$m_print_end();
        ;
    }
    else if (($clex$lxsymbol==(i64)24)) {
        msysc$m_print_startcon();
        msysc$m_print_i64($clex$lxvalue,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($clex$lxsymbol==(i64)25)) {
        msysc$m_print_startcon();
        msysc$m_print_r64($clex$lxvaluex,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($clex$lxsymbol==(i64)28)) {
        $clex$printstr($clex$lxvaluestr,$clex$lxlength);
    }
    else if (($clex$lxsymbol==(i64)23)) {
        $clex$printstr($clex$lxvaluestr,$clex$lxlength);
    }
;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

static void $clex$readname(i64 c) {
    $clex$lxvaluestr = (u8 *)($clex$lxsptr - (i64)1);
    L13 :;
    while (!!((u64)$clex$alphamap[((i64)(*$clex$lxsptr))])) {
        ++($clex$lxsptr);
L14 :;
    }
L15 :;
    ;
    $clex$lxsymbol = (i64)23;
    $clex$lxlength = ($clex$lxsptr - $clex$lxvaluestr);
}

static void $clex$readdecdigits(i64 c) {
        u8 *  intstr;
        u8 *  fractstr;
        i64 intlength;
        i64 fractlength;
        i64 expsign;
        i64 expon;
        i64 firstdigitseen;
    if ((c == (i64)46)) {
        intstr = 0;
        intlength = (i64)0;
        goto L16 ;
;
    }
;
    intstr = (u8 *)($clex$lxsptr - (i64)1);
    L17 :;
    while ((((c = (i64)(*($clex$lxsptr)++)) >= (i64)48) && (c <= (i64)57))) {
L18 :;
    }
L19 :;
    ;
    intlength = (($clex$lxsptr - intstr) - (i64)1);
    switch (c) {
    case 46:;
        {
            ++($clex$lxsptr);
            if (((i64)(*$clex$lxsptr) != (i64)46)) {
                goto L16 ;
;
            }
            else {
                --($clex$lxsptr);
            }
;
        }
        break;
    case 101:;
    case 69:;
        {
            intlength = ($clex$lxsptr - intstr);
            ++($clex$lxsptr);
            fractstr = 0;
            fractlength = (i64)0;
            goto L20 ;
;
        }
        break;
    case 108:;
    case 76:;
        {
            ++($clex$lxsptr);
                        {i64 $temp = (i64)(*$clex$lxsptr);
if (($temp==(i64)108) || ($temp==(i64)76)) {
                ++($clex$lxsptr);
            }
            else if (($temp==(i64)117) || ($temp==(i64)85)) {
                ++($clex$lxsptr);
            }
            };
        }
        break;
    case 117:;
    case 85:;
        {
            ++($clex$lxsptr);
                        {i64 $temp = (i64)(*$clex$lxsptr);
if (($temp==(i64)108) || ($temp==(i64)76)) {
                ++($clex$lxsptr);
            }
            else if (($temp==(i64)117) || ($temp==(i64)85)) {
                ++($clex$lxsptr);
            }
            };
        }
        break;
    } //SW
;
    $clex$lxsymbol = (i64)24;
    $clex$readdecimal(intstr,intlength);
    return;
    //readfraction:
L16 :;
;
    c = (i64)(*$clex$lxsptr);
    if (((c >= (i64)48) && (c <= (i64)57))) {
        fractstr = (u8 *)$clex$lxsptr;
        ++($clex$lxsptr);
        L21 :;
        switch ((c = (i64)(*$clex$lxsptr))) {
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
                ++($clex$lxsptr);
            }
            break;
        default: {
            goto L22 ;
        }
        } //SW
goto L21 ;
L22 :;
        ;
        fractlength = ($clex$lxsptr - fractstr);
        if ((c==(i64)101) || (c==(i64)69)) {
            ++($clex$lxsptr);
            //readexpon:
L20 :;
;
            firstdigitseen = (i64)9;
                        {i64 $temp = (i64)(*$clex$lxsptr);
if (($temp==(i64)45)) {
                expsign = (i64)-1;
                ++($clex$lxsptr);
            }
            else if (($temp==(i64)43)) {
                expsign = (i64)1;
                ++($clex$lxsptr);
            }
            };
            L23 :;
            while (1) {
                c = (i64)(*$clex$lxsptr);
                if (((c >= (i64)48) && (c <= (i64)57))) {
                    ++($clex$lxsptr);
                    expon = (((expon * (i64)10) + c) - (i64)57);
                    firstdigitseen = (i64)1;
                }
                else {
                    if (!(!!(firstdigitseen))) {
                        msysc$m_print_startcon();
                        msysc$m_print_str((byte*)"FDS",NULL);
                        msysc$m_print_newline();
                        msysc$m_print_end();
                        ;
                        $clex$lxsymbol = (i64)1;
                        $clex$lxsubcode = (i64)3;
                        return;
                    }
;
                    goto L24 ;
                }
;
            }
L24 :;
            ;
        }
;
        $clex$lxsymbol = (i64)25;
        $clex$lxvaluex = $clex$readreal(intstr,intlength,fractstr,fractlength,(expon * expon));
    }
;
}

static void $clex$readhexdigits(void) {
        u8 *  intstr;
        i64 intlength;
        i64 c;
    intstr = (u8 *)++($clex$lxsptr);
    L25 :;
    switch ((c = (i64)(*($clex$lxsptr)++))) {
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
    case 65:;
    case 66:;
    case 67:;
    case 68:;
    case 69:;
    case 70:;
    case 97:;
    case 98:;
    case 99:;
    case 100:;
    case 101:;
    case 102:;
        {
        }
        break;
    default: {
        goto L26 ;
    }
    } //SW
goto L25 ;
L26 :;
    ;
    intlength = (($clex$lxsptr - intstr) - (i64)1);
    $clex$lxsymbol = (i64)24;
    $clex$readhex(intstr,intlength);
}

static void $clex$readbindigits(void) {
        u8 *  intstr;
        i64 intlength;
        i64 c;
    intstr = (u8 *)++($clex$lxsptr);
    L27 :;
    switch ((c = (i64)(*($clex$lxsptr)++))) {
    case 48:;
    case 49:;
        {
        }
        break;
    default: {
        goto L28 ;
    }
    } //SW
goto L27 ;
L28 :;
    ;
    intlength = (($clex$lxsptr - intstr) - (i64)1);
    $clex$lxsymbol = (i64)24;
    $clex$readbin(intstr,intlength);
}

static void $clex$readstring(i64 termchar) {
        u8 *  strstart;
        u8 *  s;
        u8 c;
    strstart = (u8 *)$clex$lxsptr;
    s = strstart;
    L29 :;
    while (1) {
        switch ((i64)(c = (u64)(i64)(*($clex$lxsptr)++))) {
        case 92:;
            {
                c = (u64)(i64)(*$clex$lxsptr);
                if ((((u64)c >= 'A') && ((u64)c <= 'Z'))) {
                    c += (u8)(i64)32;
                }
;
                ++($clex$lxsptr);
                switch ((i64)(u64)c) {
                case 119:;
                    {
                        (*s) = (u64)13u;
                        ++(s);
                        c = (u64)10u;
                    }
                    break;
                case 99:;
                case 114:;
                    {
                        c = (u64)13u;
                    }
                    break;
                case 108:;
                case 110:;
                    {
                        c = (u64)10u;
                    }
                    break;
                case 116:;
                    {
                        c = (u64)9u;
                    }
                    break;
                case 102:;
                    {
                        c = (u64)12u;
                    }
                    break;
                case 118:;
                    {
                        c = (u64)11u;
                    }
                    break;
                case 97:;
                    {
                        c = (u64)7u;
                    }
                    break;
                case 98:;
                    {
                        c = (u64)8u;
                    }
                    break;
                case 34:;
                case 81:;
                    {
                        c = '"';
                    }
                    break;
                case 101:;
                    {
                        c = '?';
                    }
                    break;
                case 122:;
                case 48:;
                    {
                        c = (u64)0u;
                    }
                    break;
                case 92:;
                    {
                        c = (u64)92u;
                    }
                    break;
                case 39:;
                    {
                        c = (u64)39u;
                    }
                    break;
                default: {
                    c = '?';
                    return;
                }
                } //SW
;
            }
            break;
        case 34:;
        case 39:;
            {
                if (((i64)(u64)c == termchar)) {
                    if (((i64)(*$clex$lxsptr) == (i64)(u64)c)) {
                        ++($clex$lxsptr);
                    }
                    else {
                        goto L30 ;
                    }
;
                }
;
            }
            break;
        case 13:;
        case 10:;
        case 26:;
            {
                --($clex$lxsptr);
                goto L30 ;
            }
            break;
        } //SW
;
        (*s) = (u64)c;
        ++(s);
    }
L30 :;
    ;
    $clex$lxvaluestr = strstart;
    $clex$lxlength = (s - strstart);
    $clex$lxsymbol = ((termchar == (i64)34) ? (i64)28 : (i64)26);
}

static void $clex$readlinecomment(void) {
    ++($clex$lxsptr);
    L31 :;
    switch ((i64)(*($clex$lxsptr)++)) {
    case 13:;
        {
            if (((i64)(*$clex$lxsptr) == (i64)10)) {
                ++($clex$lxsptr);
            }
;
            goto L32 ;
        }
        break;
    case 10:;
        {
            goto L32 ;
        }
        break;
    case 26:;
    case 0:;
        {
            --($clex$lxsptr);
            goto L32 ;
        }
        break;
    } //SW
goto L31 ;
L32 :;
    ;
    ++($clex$lxlineno);
}

static void $clex$readblockcomment(void) {
    ++($clex$lxsptr);
    L33 :;
    switch ((i64)(*($clex$lxsptr)++)) {
    case 13:;
        {
            if (((i64)(*$clex$lxsptr) == (i64)10)) {
                ++($clex$lxsptr);
            }
;
            ++($clex$lxlineno);
        }
        break;
    case 10:;
        {
            ++($clex$lxlineno);
        }
        break;
    case 26:;
    case 0:;
        {
            $clex$lxsymbol = (i64)1;
            return;
        }
        break;
    case 42:;
        {
            if (((i64)(*$clex$lxsptr) == (i64)47)) {
                ++($clex$lxsptr);
                goto L34 ;
            }
;
        }
        break;
    } //SW
goto L33 ;
L34 :;
    ;
}

static void $clex$readdecimal(u8 *p,i64 length) {
        i64 c;
    if ((length <= (i64)20)) {
        $clex$lxvalue = (i64)((u64)(*p) - '0');
        L35 :;
        while ((--(length) > (i64)0)) {
            c = (i64)(u64)(*++(p));
            $clex$lxvalue = ((($clex$lxvalue * (i64)10) + c) - (i64)48);
L36 :;
        }
L37 :;
        ;
        $clex$lxsymbol = (i64)24;
        return;
    }
;
}

static void $clex$readhex(u8 *p,i64 length) {
        i64 c;
        i64 $av_1;
    if ((length <= (i64)16)) {
        $clex$lxvalue = (i64)0;
        $av_1 = length;
        while ($av_1-- > 0) {
L38 :;
            c = (i64)(u64)(*(p)++);
            if ((c <= (i64)57)) {
                $clex$lxvalue = ((($clex$lxvalue * (i64)16) + c) - (i64)48);
            }
            else if ((c <= (i64)70)) {
                $clex$lxvalue = (((($clex$lxvalue * (i64)16) + c) - (i64)65) + (i64)10);
            }
            else {
                $clex$lxvalue = (((($clex$lxvalue * (i64)16) + c) - (i64)97) + (i64)10);
            }
;
L39 :;
        }
L40 :;
        ;
        $clex$lxsymbol = (i64)24;
        return;
    }
;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"LONG INT HEX",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

static void $clex$readbin(u8 *p,i64 length) {
        i64 $av_1;
    if ((length <= (i64)64)) {
        $clex$lxvalue = (i64)0;
        $av_1 = length;
        while ($av_1-- > 0) {
L41 :;
                        {u64 $temp = (u64)(*(p)++);
if (($temp=='0')) {
                $clex$lxvalue = ($clex$lxvalue * (i64)2);
            }
            else if (($temp=='1')) {
                $clex$lxvalue = (($clex$lxvalue * (i64)2) + (i64)1);
            }
            };
L42 :;
        }
L43 :;
        ;
        $clex$lxsymbol = (i64)24;
        return;
    }
;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"LONG INT BIN",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    exit(0);
}

static r64 $clex$readreal(u8 *istr,i64 ilength,u8 *fstr,i64 flength,i64 expon) {
    return (double)0.;
}

static void $clex$printstr(u8 *s,i64 length) {
    printf((byte*)"%.*s",length,s);
}

static void $clex$initdata(void) {
        i64 i;
    for (i=(i64)0;i<=(i64)255;++i) {
L44 :;
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
                $clex$alphamap[(i)] = (u64)1u;
            }
            break;
        } //SW
;
L45 :;
    }
L46 :;
    ;
}

// START
void $clex$start(void) {

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
L47 :;
        msysc$sysparams[(i)-1] = (*args)[(i)-1];
L48 :;
    }
L49 :;
    ;
    msysc$ncmdparams = (msysc$nsysparams - (msysc$$cmdskip + (i64)1));
    msysc$cmdparams = (u8 *(*)[])&msysc$sysparams[((msysc$$cmdskip + (i64)1))-1];
    j = (i64)1;
    msysc$nenvstrings = (i64)0;
    L50 :;
    while (!!((*envstrings)[(j)-1])) {
        ++(msysc$nenvstrings);
        ++(j);
L51 :;
    }
L52 :;
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
    L53 :;
    while (1) {
        c = (u64)(*msysc$fmtstr);
        switch ((i64)(u64)c) {
        case 35:;
            {
                if (!!(lastx)) {
                    goto L55 ;
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
L55 :;
;
            ++(n);
            ++(msysc$fmtstr);
        }
        } //SW
;
    }
L54 :;
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
    L56 :;
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
                goto L59 ;
;
            }
            break;
        default: {
            if ((((u64)c >= '0') && ((u64)c <= '9'))) {
                n = (i64)((u64)c - '0');
                L60 :;
                while (1) {
                    c = (u64)(*s);
                    if (((i64)(u64)(*s) == (i64)0)) {
                        goto L61 ;
                    }
;
                    if ((((u64)c >= '0') && ((u64)c <= '9'))) {
                        ++(s);
                        n = (((n * (i64)10) + (i64)(u64)c) - (i64)48);
                    }
                    else {
                        goto L61 ;
                    }
;
                }
L61 :;
                ;
                //gotwidth:
L59 :;
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
L57 :;
    }
L58 :;
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
L62 :;
        if (((i64)(u64)(*p) == (i64)0)) {
            goto L64 ;
        }
;
        (*q) = (u64)(*p);
        ++(q);
        ++(p);
L63 :;
    }
L64 :;
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
L65 :;
            (*t) = (u64)(*fmt).padchar;
            ++(t);
L66 :;
        }
L67 :;
        ;
        (*t) = (u64)0u;
    }
    else if (((u64)(*fmt).justify == 'R')) {
        if (((((u64)(*fmt).padchar == '0') && !!((i64)(*fmt).base)) && (((u64)(*s) == '-') || ((u64)(*s) == '+')))) {
            (*t) = (u64)(*s);
            ++(t);
            $av_2 = (w - n);
            while ($av_2-- > 0) {
L68 :;
                (*t) = (u64)(*fmt).padchar;
                ++(t);
L69 :;
            }
L70 :;
            ;
            strncpy(t,(s + (i64)1),(u64)(n - (i64)1));
            (*((t + n) - (i64)1)) = (u64)0u;
        }
        else {
            $av_3 = (w - n);
            while ($av_3-- > 0) {
L71 :;
                (*t) = (u64)(*fmt).padchar;
                ++(t);
L72 :;
            }
L73 :;
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
L74 :;
            (*t) = (u64)(*fmt).padchar;
            ++(t);
L75 :;
        }
L76 :;
        ;
        strncpy(t,s,(u64)n);
        t += n;
        $av_5 = ((w - n) - m);
        while ($av_5-- > 0) {
L77 :;
            (*t) = (u64)(*fmt).padchar;
            ++(t);
L78 :;
        }
L79 :;
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
    L80 :;
    do {
        t[(++(i))] = (u64)msysc$digits[((i64)(aa % base))];
        aa = (aa / base);
        ++(k);
        if (((!!(sep) && ((i64)aa != (i64)0)) && (k == g))) {
            t[(++(i))] = (u64)sep;
            k = (i64)0;
        }
;
L81 :;
    }
    while (!((i64)aa == (i64)0));
L82 :;
    ;
    j = i;
    s0 = s;
    L83 :;
    while (!!(i)) {
        (*s) = (u64)t[((i)--)];
        ++(s);
L84 :;
    }
L85 :;
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
    L86 :;
    while (!!(i)) {
        --(s);
        (*s) = (u64)t[(((i)-- - (i64)1))];
        if (((!!(sep) && !!(i)) && (++(k) == g))) {
            --(s);
            (*s) = (u64)sep;
            k = (i64)0;
        }
;
L87 :;
    }
L88 :;
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
    msysc$rd_length = strlen(msysc$rd_buffer);
    msysc$rd_pos = msysc$rd_buffer;
    msysc$rd_lastpos = 0;
}

void msysc$m_read_fileline(void *f) {
    msysc$initreadbuffer();
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
    L89 :;
    while ((((u64)(*s) == ' ') || ((i64)(u64)(*s) == (i64)9))) {
        ++(s);
L90 :;
    }
L91 :;
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
    L92 :;
    while (!!((u64)(*s))) {
        c = (u64)(*(s)++);
        switch ((i64)(u64)c) {
        case 32:;
        case 9:;
        case 44:;
        case 61:;
            {
                if ((!!((u64)quotechar) || (p == s))) {
                    goto L95 ;
;
                }
;
                msysc$termchar = (i64)(u64)c;
                goto L94 ;
            }
            break;
        default: {
            //normalchar:
L95 :;
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
                    goto L94 ;
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
L93 :;
    }
L94 :;
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
    L96 :;
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
                goto L97 ;
            }
            break;
        default: {
            msysc$itemerror = (i64)1;
            goto L98 ;
        }
        } //SW
;
        if (((i64)(u64)d >= base)) {
            msysc$itemerror = (i64)1;
            goto L98 ;
        }
;
        aa = (u64)(((i64)aa * base) + (i64)(u64)d);
L97 :;
    }
L98 :;
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
L99 :;
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L100 :;
    }
L101 :;
    ;
}

static void msysc$iconvucn(u8 *s,i64 n) {
        i64 $av_1;
    $av_1 = n;
    while ($av_1-- > 0) {
L102 :;
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L103 :;
    }
L104 :;
    ;
}

static void msysc$convlcstring(u8 *s) {
    L105 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L106 :;
    }
L107 :;
    ;
}

static void msysc$convucstring(u8 *s) {
    L108 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L109 :;
    }
L110 :;
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
        mlib$maxmemtotal -= n;
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
L111 :;
        j = (i64)1;
        k = (i64)16;
        L114 :;
        while ((i > k)) {
            k = (k << (i64)1);
            ++(j);
L115 :;
        }
L116 :;
        ;
        mlib$sizeindextable[(i)] = j;
L112 :;
    }
L113 :;
    ;
    mlib$allocupper[((i64)1)] = (u64)16u;
    size = (i64)16;
    for (i=(i64)2;i<=(i64)27;++i) {
L117 :;
        size *= (i64)2;
        mlib$allocupper[(i)] = (u64)size;
        if ((size >= (i64)33554432)) {
            k = i;
            goto L119 ;
        }
;
L118 :;
    }
L119 :;
    ;
    for (i=(k + (i64)1);i<=(i64)300;++i) {
L120 :;
        size += (i64)33554432;
        if ((size < (i64)8589934592)) {
            mlib$allocupper[(i)] = (u64)size;
            mlib$maxmemory = (u64)size;
        }
        else {
            mlib$maxalloccode = (i - (i64)1);
            goto L122 ;
        }
;
L121 :;
    }
L122 :;
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
L123 :;
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
L124 :;
    }
L125 :;
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"MEMALLOCTABLE FULL\n\n\n\n",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mlinux$os_getch();
    exit((i64)3);
}

static void mlib$removefrommemalloc(i32 *ptr,i64 size) {
        i64 allocated;
        i64 code;
        i64 i;
    code = mlib$pcm_getac(size);
    allocated = (i64)mlib$allocupper[(code)];
    for (i=(i64)1;i<=(i64)2;++i) {
L126 :;
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
L127 :;
    }
L128 :;
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"CAN'T FIND",NULL);
    msysc$m_print_ptr(ptr,NULL);
    msysc$m_print_str((byte*)"IN MEMALLOCTABLE",NULL);
    msysc$m_print_i64(size,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
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
    m = (byte *)malloc((u64)(size + (i64)2));
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
        L129 :;
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
L130 :;
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
    L131 :;
    while (((p >= buffer) && (((i64)(u64)(*p) == (i64)13) || ((i64)(u64)(*p) == (i64)10)))) {
        if ((((i64)(u64)(*p) == (i64)13) || ((i64)(u64)(*p) == (i64)10))) {
            crseen = (i64)1;
        }
;
        (*(p)--) = (u64)0u;
L132 :;
    }
L133 :;
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
L134 :;
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L135 :;
    }
L136 :;
    ;
}

void mlib$iconvucn(u8 *s,i64 n) {
        i64 $av_1;
    $av_1 = n;
    while ($av_1-- > 0) {
L137 :;
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L138 :;
    }
L139 :;
    ;
}

u8 *mlib$convlcstring(u8 *s) {
        u8 *  s0;
    s0 = s;
    L140 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L141 :;
    }
L142 :;
    ;
    return s0;
}

u8 *mlib$convucstring(u8 *s) {
        u8 *  s0;
    s0 = s;
    L143 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L144 :;
    }
L145 :;
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
    L146 :;
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
L147 :;
    }
L148 :;
    ;
    return (byte*)"";
}

u8 *mlib$extractpath(u8 *s) {
        static u8 str[260];
        u8 *  t;
        i64 n;
    t = ((s + strlen(s)) - (i64)1);
    L149 :;
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
L150 :;
    }
L151 :;
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
L152 :;
            str[((slen + i))-1] = (u64)padch;
L153 :;
        }
L154 :;
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
L155 :;
        str[(i)-1] = (u64)ch;
L156 :;
    }
L157 :;
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
L158 :;
;
    (*value) = 0;
    (*name) = 0;
    if (!!(infile)) {
        if ((mlib$readnextfileitem(&fileptr,&item) == (i64)0)) {
            free((void *)filestart);
            infile = (i64)0;
            goto L158 ;
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
            goto L158 ;
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
L159 :;
;
    L160 :;
    while (1) {
                {u64 $temp = (u64)(*p);
if (($temp==' ') || ($temp==(u64)9u) || ($temp==(u64)13u) || ($temp==(u64)10u)) {
            ++(p);
        }
        else if (($temp==(u64)26u) || ($temp==(u64)0u)) {
            return (i64)0;
        }
        else {
            goto L161 ;
        }
        };
    }
L161 :;
    ;
        {u64 $temp = (u64)(*p);
if (($temp=='!') || ($temp=='#')) {
        ++(p);
        L162 :;
                {u64 $temp = (u64)(*(p)++);
if (($temp==(u64)10u)) {
            goto L159 ;
;
        }
        else if (($temp==(u64)26u) || ($temp==(u64)0u)) {
            (*fileptr) = (p - (i64)1);
            return (i64)0;
        }
        else {
        }
        }goto L162 ;
L163 :;
        ;
    }
    };
        {u64 $temp = (u64)(*p);
if (($temp=='"')) {
        pstart = ++(p);
        L164 :;
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
                goto L165 ;
            }
            };
            ++(p);
        }
L165 :;
        ;
    }
    else {
        pstart = p;
        L166 :;
        while (1) {
                        {u64 $temp = (u64)(*p);
if (($temp==(u64)0u) || ($temp==(u64)26u)) {
                pend = p;
                goto L167 ;
            }
            else if (($temp==' ') || ($temp==(u64)9u) || ($temp==',') || ($temp==(u64)13u) || ($temp==(u64)10u)) {
                pend = (p)++;
                goto L167 ;
            }
            };
            ++(p);
        }
L167 :;
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
L168 :;
        strcat(s,padchar);
L169 :;
    }
L170 :;
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
    L171 :;
    do {
        x = ((r64)mlib$mrandomp() / (double)9223372036854775800.);
L172 :;
    }
    while (!(x != (double)1.));
L173 :;
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
    msysc$m_print_ptr(&exefile,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    packexeptr = mlib$readfile((u8 *)&exefile[((i64)1)-1]);
    if (!(!!(packexeptr))) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Can't open",NULL);
        msysc$m_print_ptr(&exefile,NULL);
        msysc$m_print_ptr(&packexeptr,NULL);
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
L174 :;
        if (!!(mlib$eqstring(msysc$m_get_procname(i),name))) {
            return msysc$m_get_procaddr(i);
        }
;
L175 :;
    }
L176 :;
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
