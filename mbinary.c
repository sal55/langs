/*
  M to C  Whole Program Translator
  Input:  b.m plus imported modules
  Output: b.c (this file, or renamed from that)
          File represents entire program
  Target: C 64-bit
  OS:     Linux

  Modules:
  Module 1: b.m
  Module 2: <Built-in: clibnewc.m>
  Module 3: <Built-in: mlib.m>
  Module 4: <Built-in: msysnewc.m>
  Module 5: <Built-in: oslinux.m>

*********** Start of C Code **********/


#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#pragma pack(1)

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef unsigned char byte;

typedef void* var;

#ifndef CALLBACK
#define CALLBACK
#endif

#if (UINTPTR_MAX<0xFFFFFFFFFFFFFFFF)
	#error "Need 64-bit target. Try -m64"
#endif

/* Forward Struct Declarations */
struct mlib_strbuffer;
struct msysnewc_procinforec;
struct msysnewc_fmtrec;
struct oslinux_termios;
struct oslinux_rsystemtime;
struct b_treenode;

/* Struct Definitions */
struct mlib_strbuffer {
    byte *  strptr;
    i32 length;
    i32 allocated;
};

struct msysnewc_procinforec {
    u16 fnindex;
    byte rettype;
    byte nparams;
    byte paramlist[12];
};

struct msysnewc_fmtrec {
    byte minwidth;
    i8 precision;
    byte base;
    byte quotechar;
    byte padchar;
    byte realfmt;
    byte plus;
    byte sepchar;
    byte lettercase;
    byte justify;
    byte suffix;
    byte usigned;
    byte charmode;
    byte heapmode;
    byte param;
    byte spare;
};

struct oslinux_termios {
    i32 c_iflag;
    i32 c_oflag;
    i32 c_cflag;
    i32 c_lflag;
    byte c_line;
    byte c_cc[32];
    byte filler[3];
    i32 c_ispeed;
    i32 c_ospeed;
};

struct oslinux_rsystemtime {
    i32 year;
    i32 month;
    i32 dayofweek;
    i32 day;
    i32 hour;
    i32 minute;
    i32 second;
    i64 milliseconds;
};

struct b_treenode {
    struct b_treenode* left;
    struct b_treenode* right;
    i64 item;
};


/* PROCDECLS */
static struct b_treenode * b_newtreenode(struct b_treenode * left,struct b_treenode * right,i64 item);
static i64 b_itemcheck(struct b_treenode * tree);
static struct b_treenode * b_bottomuptree(i64 item,i64 depth);
static void b_deletetree(struct b_treenode * tree);
void start(void);
void * mlib_pcm_alloc(i64 n);
void mlib_pcm_freestr(byte * s);
void mlib_pcm_free(void * p,i64 n);
void mlib_pcm_freeac(void * p,i64 alloc);
void mlib_pcm_copymem4(void * p,void * q,i64 n);
void mlib_pcm_clearmem(void * p,i64 n);
void mlib_pcm_init(void);
i64 mlib_pcm_getac(i64 size);
void * mlib_pcm_newblock(i64 itemsize);
i64 mlib_pcm_round(i64 n);
i64 mlib_pcm_array(i64 n);
void mlib_pcm_printfreelist(i64 size,u64 * p);
void mlib_pcm_diags(byte * caption);
void * mlib_pcm_allocz(i64 n);
byte * mlib_pcm_copyheapstring(byte * s);
byte * mlib_pcm_copyheapstringn(byte * s,i64 n);
byte * mlib_pcm_copyheapblock(byte * s,i64 length);
static void mlib_addtomemalloc(i32 * ptr,i64 size);
static void mlib_removefrommemalloc(i32 * ptr,i64 size);
void * mlib_allocmem(i64 n);
void * mlib_reallocmem(void * p,i64 n);
void mlib_abortprogram(byte * s);
i64 mlib_getfilesize(void * handlex);
void mlib_readrandom(void * handlex,byte * mem,i64 offset,i64 size);
i64 mlib_writerandom(void * handlex,byte * mem,i64 offset,i64 size);
i64 mlib_setfilepos(void * file,i64 offset);
i64 mlib_getfilepos(void * file);
byte * mlib_readfile(byte * filename);
i64 mlib_writefile(byte * filename,byte * data,i64 size);
i64 mlib_checkfile(byte * file);
void mlib_readlinen(void * handlex,byte * buffer,i64 size);
void mlib_iconvlcn(byte * s,i64 n);
void mlib_iconvucn(byte * s,i64 n);
void mlib_convlcstring(byte * s);
void mlib_convucstring(byte * s);
byte * mlib_changeext(byte * s,byte * newext);
byte * mlib_extractext(byte * s,i64 period);
byte * mlib_extractpath(byte * s);
byte * mlib_extractfile(byte * s);
byte * mlib_extractbasefile(byte * s);
byte * mlib_addext(byte * s,byte * newext);
void * mlib_alloctable(i64 n,i64 size);
void * mlib_zalloctable(i64 n,i64 size);
void mlib_checkfreelists(byte * s);
void * mlib_pcm_alloc32(void);
void mlib_pcm_free32(void * p);
void mlib_outbyte(void * f,i64 x);
void mlib_outword16(void * f,u64 x);
void mlib_outword(void * f,u64 x);
void mlib_outword64(void * f,u64 x);
i64 mlib_myeof(void * f);
void * mlib_pcm_smallallocz(i64 n);
void * mlib_pcm_smallalloc(i64 n);
void mlib_strbuffer_add(struct mlib_strbuffer * dest,byte * s,i64 n);
void mlib_gs_init(struct mlib_strbuffer * dest);
void mlib_gs_free(struct mlib_strbuffer * dest);
void mlib_gs_str(struct mlib_strbuffer * dest,byte * s);
void mlib_gs_char(struct mlib_strbuffer * dest,i64 c);
void mlib_gs_strn(struct mlib_strbuffer * dest,byte * s,i64 length);
void mlib_gs_strvar(struct mlib_strbuffer * dest,struct mlib_strbuffer * s);
void mlib_gs_strint(struct mlib_strbuffer * dest,i64 a);
void mlib_gs_strln(struct mlib_strbuffer * dest,byte * s);
void mlib_gs_strsp(struct mlib_strbuffer * dest,byte * s);
void mlib_gs_line(struct mlib_strbuffer * dest);
i64 mlib_gs_getcol(struct mlib_strbuffer * dest);
void mlib_gs_leftstr(struct mlib_strbuffer * dest,byte * s,i64 w,i64 padch);
void mlib_gs_leftint(struct mlib_strbuffer * dest,i64 a,i64 w,i64 padch);
void mlib_gs_padto(struct mlib_strbuffer * dest,i64 col,i64 ch);
void mlib_gs_println(struct mlib_strbuffer * dest,void * f);
i64 mlib_nextcmdparam(i64 * paramno,byte * * name,byte * * value,byte * defext);
static i64 mlib_readnextfileitem(byte * * fileptr,byte * * item);
void mlib_ipadstr(byte * s,i64 width,byte * padchar);
byte * mlib_padstr(byte * s,i64 width,byte * padchar);
byte * mlib_chr(i64 c);
i64 mlib_cmpstring(byte * s,byte * t);
i64 mlib_cmpstringn(byte * s,byte * t,i64 n);
i64 mlib_eqstring(byte * s,byte * t);
i64 mlib_cmpbytes(void * p,void * q,i64 n);
i64 mlib_eqbytes(void * p,void * q,i64 n);
void mlib_mseed(u64 a,u64 b);
u64 mlib_mrandom(void);
i64 mlib_mrandomp(void);
i64 mlib_mrandomint(i64 n);
i64 mlib_mrandomrange(i64 a,i64 b);
double mlib_mrandomreal(void);
double mlib_mrandomreal1(void);
byte * mlib_checkpackfile(void);
i64 msysnewc_m_getdotindex(u64 a,i64 i);
void msysnewc_m_setdotindex(u64 * a,i64 i,i64 x);
i64 msysnewc_m_getdotslice(u64 a,i64 i,i64 j);
void msysnewc_m_setdotslice(u64 * a,i64 i,i64 j,u64 x);
i64 msysnewc_m_get_nprocs(void);
i64 msysnewc_m_get_nexports(void);
void * msysnewc_m_get_procname(i64 n);
byte * msysnewc_m_get_procaddr(i64 n);
void * msysnewc_m_get_procexport(i64 n);
static void msysnewc_pushio(void);
void msysnewc_m_print_startfile(void * dev);
void msysnewc_m_print_startstr(byte * s);
void msysnewc_m_print_startptr(byte * * p);
void msysnewc_m_print_startcon(void);
void msysnewc_m_print_setfmt(byte * format);
void msysnewc_m_print_end(void);
void msysnewc_m_print_ptr(void * a,byte * fmtstyle);
void msysnewc_m_print_i64(i64 a,byte * fmtstyle);
void msysnewc_m_print_u64(u64 a,byte * fmtstyle);
void msysnewc_m_print_r64(double x,byte * fmtstyle);
void msysnewc_m_print_r32(float x,byte * fmtstyle);
void msysnewc_m_print_c8(i64 a,byte * fmtstyle);
void msysnewc_m_print_str(byte * s,byte * fmtstyle);
void msysnewc_m_print_newline(void);
void msysnewc_m_print_nogap(void);
void msysnewc_printstr(byte * s);
void msysnewc_printstr_n(byte * s,i64 n);
void msysnewc_printstrn_app(byte * s,i64 length,void * f);
static byte * msysnewc_makezstring(byte * s,i64 n,byte * local);
static void msysnewc_freezstring(byte * t,i64 n);
static void msysnewc_printchar(i64 ch);
void msysnewc_nextfmtchars(i64 lastx);
void msysnewc_strtofmt(byte * s,i64 slen,struct msysnewc_fmtrec * fmt);
static i64 msysnewc_domultichar(byte * p,i64 n,byte * dest,struct msysnewc_fmtrec * fmt);
static i64 msysnewc_expandstr(byte * s,byte * t,i64 n,struct msysnewc_fmtrec * fmt);
static u64 msysnewc_xdivrem(u64 a,u64 b,u64 * remainder);
static i64 msysnewc_u64tostr(u64 aa,byte * s,u64 base,i64 sep);
static i64 msysnewc_i64tostrfmt(i64 aa,byte * s,struct msysnewc_fmtrec * fmt);
static i64 msysnewc_u64tostrfmt(i64 aa,byte * s,struct msysnewc_fmtrec * fmt);
static i64 msysnewc_i64mintostr(byte * s,i64 base,i64 sep);
static i64 msysnewc_strtostrfmt(byte * s,byte * t,i64 n,struct msysnewc_fmtrec * fmt);
static void msysnewc_tostr_i64(i64 a,struct msysnewc_fmtrec * fmt);
static void msysnewc_tostr_u64(u64 a,struct msysnewc_fmtrec * fmt);
static void msysnewc_tostr_r64(double x,struct msysnewc_fmtrec * fmt);
static void msysnewc_tostr_str(byte * s,struct msysnewc_fmtrec * fmt);
static struct msysnewc_fmtrec * msysnewc_getfmt(byte * fmtstyle);
byte * msysnewc_strint(i64 a,byte * fmtstyle);
void msysnewc_getstrint(i64 a,byte * dest);
byte * msysnewc_strword(u64 a,byte * fmtstyle);
byte * msysnewc_strreal(double a,byte * fmtstyle);
static byte * msysnewc_getstr(byte * s,struct msysnewc_fmtrec * fmt);
static void msysnewc_initreadbuffer(void);
void msysnewc_m_read_conline(void);
void msysnewc_m_read_fileline(void * f);
void msysnewc_m_read_strline(byte * s);
static byte * msysnewc_readitem(i64 * itemlength);
i64 msysnewc_strtoint(byte * s,i64 length,i64 base);
i64 msysnewc_m_read_i64(i64 fmt);
double msysnewc_m_read_r64(i64 fmt);
void msysnewc_m_read_str(byte * dest,i64 destlen,i64 fmt);
void msysnewc_readstr(byte * dest,i64 fmt,i64 destlen);
void msysnewc_rereadln(void);
void msysnewc_reread(void);
i64 msysnewc_valint(byte * s,i64 fmt);
double msysnewc_valreal(byte * s);
static void msysnewc_iconvlcn(byte * s,i64 n);
static void msysnewc_iconvucn(byte * s,i64 n);
static void msysnewc_convlcstring(byte * s);
static void msysnewc_convucstring(byte * s);
i64 msysnewc_m_power_i64(i64 n,i64 a);
void msysnewc_m_intoverflow(void);
void msysnewc_m_dotindex(u64 i,u64 a);
void msysnewc_m_dotslice(u64 j,u64 i,u64 a);
void msysnewc_m_popdotindex(u64 i,u64 * p,u64 x);
void msysnewc_m_popdotslice(u64 j,u64 i,u64 * p,u64 x);
i64 msysnewc_m_imin(i64 a,i64 b);
i64 msysnewc_m_imax(i64 a,i64 b);
double msysnewc_m_sign(double x);
extern void sleep(u32 _1);
extern void * dlopen(byte * _1,i32 _2);
extern void * dlsym(void * _1,byte * _2);
extern i32 tcgetattr(i32 _1,struct oslinux_termios * _2);
extern i32 tcsetattr(i32 _1,i32 _2,struct oslinux_termios * _3);
void oslinux_os_init(void);
i64 oslinux_os_execwait(byte * cmdline,i64 newconsole,byte * workdir);
i64 oslinux_os_execcmd(byte * cmdline,i64 newconsole);
i64 oslinux_os_getch(void);
i64 oslinux_os_kbhit(void);
void oslinux_os_flushkeys(void);
void * oslinux_os_getconsolein(void);
void * oslinux_os_getconsoleout(void);
void * oslinux_os_proginstance(void);
u64 oslinux_os_getdllinst(byte * name);
void * oslinux_os_getdllprocaddr(i64 hlib,byte * name);
void oslinux_os_initwindows(void);
i64 oslinux_os_getchx(void);
byte * oslinux_os_getos(void);
i64 oslinux_os_gethostsize(void);
i64 oslinux_os_iswindows(void);
i64 oslinux_os_shellexec(byte * opc,byte * file);
void oslinux_os_sleep(i64 a);
void * oslinux_os_getstdin(void);
void * oslinux_os_getstdout(void);
byte * oslinux_os_gethostname(void);
byte * oslinux_os_getmpath(void);
void oslinux_os_exitprocess(i64 x);
i64 oslinux_os_clock(void);
i64 oslinux_os_getclockspersec(void);
void oslinux_os_setmesshandler(void * addr);
i64 oslinux_os_hpcounter(void);
i64 oslinux_os_hpfrequency(void);
i64 oslinux_os_filelastwritetime(byte * filename);
void oslinux_os_getsystime(struct oslinux_rsystemtime * tm);
void oslinux_os_peek(void);

/* VARS */
static i64 b_nallocs = (i64)0;
i64 mlib_mdebug;
u64 mlib_allocupper[301];
i64 mlib_alloccode;
i64 mlib_allocbytes;
i64 mlib_fdebug = (i64)0;
i64 mlib_rfsize;
static u64 mlib_maxmemory;
static i64 mlib_maxalloccode;
static byte mlib_pcm_setup = (u8)0u;
static i64 mlib_show = (i64)0;
i64 mlib_memtotal = (i64)0;
i64 mlib_smallmemtotal = (i64)0;
i64 mlib_smallmemobjs = (i64)0;
i64 mlib_maxmemtotal = (i64)0;
static i32 *  mlib_memalloctable[500001];
static i32 mlib_memallocsize[500001];
static byte *  mlib_pcheapstart;
static byte *  mlib_pcheapend;
static byte *  mlib_pcheapptr;
static byte mlib_sizeindextable[2049];
u64 *  mlib_freelist[9];
byte *  mlib_pmnames[6] = {(byte*)"pm_end",(byte*)"pm_option",(byte*)"pm_sourcefile",(byte*)"pm_libfile",(byte*)"pm_colon",(byte*)"pm_extra"};
static i64 mlib_seed[2] = {(i64)2993073034246558322,(i64)1617678968452121188};
static void *  msysnewc__fnaddresses[]= {
    &b_newtreenode,
    &b_itemcheck,
    &b_bottomuptree,
    &b_deletetree,
    &start,
    &mlib_pcm_alloc,
    &mlib_pcm_freestr,
    &mlib_pcm_free,
    &mlib_pcm_freeac,
    &mlib_pcm_copymem4,
    &mlib_pcm_clearmem,
    &mlib_pcm_init,
    &mlib_pcm_getac,
    &mlib_pcm_newblock,
    &mlib_pcm_round,
    &mlib_pcm_array,
    &mlib_pcm_printfreelist,
    &mlib_pcm_diags,
    &mlib_pcm_allocz,
    &mlib_pcm_copyheapstring,
    &mlib_pcm_copyheapstringn,
    &mlib_pcm_copyheapblock,
    &mlib_addtomemalloc,
    &mlib_removefrommemalloc,
    &mlib_allocmem,
    &mlib_reallocmem,
    &mlib_abortprogram,
    &mlib_getfilesize,
    &mlib_readrandom,
    &mlib_writerandom,
    &mlib_setfilepos,
    &mlib_getfilepos,
    &mlib_readfile,
    &mlib_writefile,
    &mlib_checkfile,
    &mlib_readlinen,
    &mlib_iconvlcn,
    &mlib_iconvucn,
    &mlib_convlcstring,
    &mlib_convucstring,
    &mlib_changeext,
    &mlib_extractext,
    &mlib_extractpath,
    &mlib_extractfile,
    &mlib_extractbasefile,
    &mlib_addext,
    &mlib_alloctable,
    &mlib_zalloctable,
    &mlib_checkfreelists,
    &mlib_pcm_alloc32,
    &mlib_pcm_free32,
    &mlib_outbyte,
    &mlib_outword16,
    &mlib_outword,
    &mlib_outword64,
    &mlib_myeof,
    &mlib_pcm_smallallocz,
    &mlib_pcm_smallalloc,
    &mlib_strbuffer_add,
    &mlib_gs_init,
    &mlib_gs_free,
    &mlib_gs_str,
    &mlib_gs_char,
    &mlib_gs_strn,
    &mlib_gs_strvar,
    &mlib_gs_strint,
    &mlib_gs_strln,
    &mlib_gs_strsp,
    &mlib_gs_line,
    &mlib_gs_getcol,
    &mlib_gs_leftstr,
    &mlib_gs_leftint,
    &mlib_gs_padto,
    &mlib_gs_println,
    &mlib_nextcmdparam,
    &mlib_readnextfileitem,
    &mlib_ipadstr,
    &mlib_padstr,
    &mlib_chr,
    &mlib_cmpstring,
    &mlib_cmpstringn,
    &mlib_eqstring,
    &mlib_cmpbytes,
    &mlib_eqbytes,
    &mlib_mseed,
    &mlib_mrandom,
    &mlib_mrandomp,
    &mlib_mrandomint,
    &mlib_mrandomrange,
    &mlib_mrandomreal,
    &mlib_mrandomreal1,
    &mlib_checkpackfile,
    &msysnewc_m_getdotindex,
    &msysnewc_m_setdotindex,
    &msysnewc_m_getdotslice,
    &msysnewc_m_setdotslice,
    &msysnewc_m_get_nprocs,
    &msysnewc_m_get_nexports,
    &msysnewc_m_get_procname,
    &msysnewc_m_get_procaddr,
    &msysnewc_m_get_procexport,
    &msysnewc_pushio,
    &msysnewc_m_print_startfile,
    &msysnewc_m_print_startstr,
    &msysnewc_m_print_startptr,
    &msysnewc_m_print_startcon,
    &msysnewc_m_print_setfmt,
    &msysnewc_m_print_end,
    &msysnewc_m_print_ptr,
    &msysnewc_m_print_i64,
    &msysnewc_m_print_u64,
    &msysnewc_m_print_r64,
    &msysnewc_m_print_r32,
    &msysnewc_m_print_c8,
    &msysnewc_m_print_str,
    &msysnewc_m_print_newline,
    &msysnewc_m_print_nogap,
    &msysnewc_printstr,
    &msysnewc_printstr_n,
    &msysnewc_printstrn_app,
    &msysnewc_makezstring,
    &msysnewc_freezstring,
    &msysnewc_printchar,
    &msysnewc_nextfmtchars,
    &msysnewc_strtofmt,
    &msysnewc_domultichar,
    &msysnewc_expandstr,
    &msysnewc_xdivrem,
    &msysnewc_u64tostr,
    &msysnewc_i64tostrfmt,
    &msysnewc_u64tostrfmt,
    &msysnewc_i64mintostr,
    &msysnewc_strtostrfmt,
    &msysnewc_tostr_i64,
    &msysnewc_tostr_u64,
    &msysnewc_tostr_r64,
    &msysnewc_tostr_str,
    &msysnewc_getfmt,
    &msysnewc_strint,
    &msysnewc_getstrint,
    &msysnewc_strword,
    &msysnewc_strreal,
    &msysnewc_getstr,
    &msysnewc_initreadbuffer,
    &msysnewc_m_read_conline,
    &msysnewc_m_read_fileline,
    &msysnewc_m_read_strline,
    &msysnewc_readitem,
    &msysnewc_strtoint,
    &msysnewc_m_read_i64,
    &msysnewc_m_read_r64,
    &msysnewc_m_read_str,
    &msysnewc_readstr,
    &msysnewc_rereadln,
    &msysnewc_reread,
    &msysnewc_valint,
    &msysnewc_valreal,
    &msysnewc_iconvlcn,
    &msysnewc_iconvucn,
    &msysnewc_convlcstring,
    &msysnewc_convucstring,
    &msysnewc_m_power_i64,
    &msysnewc_m_intoverflow,
    &msysnewc_m_dotindex,
    &msysnewc_m_dotslice,
    &msysnewc_m_popdotindex,
    &msysnewc_m_popdotslice,
    &msysnewc_m_imin,
    &msysnewc_m_imax,
    &msysnewc_m_sign,
    &oslinux_os_init,
    &oslinux_os_execwait,
    &oslinux_os_execcmd,
    &oslinux_os_getch,
    &oslinux_os_kbhit,
    &oslinux_os_flushkeys,
    &oslinux_os_getconsolein,
    &oslinux_os_getconsoleout,
    &oslinux_os_proginstance,
    &oslinux_os_getdllinst,
    &oslinux_os_getdllprocaddr,
    &oslinux_os_initwindows,
    &oslinux_os_getchx,
    &oslinux_os_getos,
    &oslinux_os_gethostsize,
    &oslinux_os_iswindows,
    &oslinux_os_shellexec,
    &oslinux_os_sleep,
    &oslinux_os_getstdin,
    &oslinux_os_getstdout,
    &oslinux_os_gethostname,
    &oslinux_os_getmpath,
    &oslinux_os_exitprocess,
    &oslinux_os_clock,
    &oslinux_os_getclockspersec,
    &oslinux_os_setmesshandler,
    &oslinux_os_hpcounter,
    &oslinux_os_hpfrequency,
    &oslinux_os_filelastwritetime,
    &oslinux_os_getsystime,
    &oslinux_os_peek,
0};
static byte *  msysnewc__fnnames[]= {
    (byte*)"newtreenode",
    (byte*)"itemcheck",
    (byte*)"bottomuptree",
    (byte*)"deletetree",
    (byte*)"start",
    (byte*)"pcm_alloc",
    (byte*)"pcm_freestr",
    (byte*)"pcm_free",
    (byte*)"pcm_freeac",
    (byte*)"pcm_copymem4",
    (byte*)"pcm_clearmem",
    (byte*)"pcm_init",
    (byte*)"pcm_getac",
    (byte*)"pcm_newblock",
    (byte*)"pcm_round",
    (byte*)"pcm_array",
    (byte*)"pcm_printfreelist",
    (byte*)"pcm_diags",
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
    (byte*)"alloctable",
    (byte*)"zalloctable",
    (byte*)"checkfreelists",
    (byte*)"pcm_alloc32",
    (byte*)"pcm_free32",
    (byte*)"outbyte",
    (byte*)"outword16",
    (byte*)"outword",
    (byte*)"outword64",
    (byte*)"myeof",
    (byte*)"pcm_smallallocz",
    (byte*)"pcm_smallalloc",
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
    (byte*)"nextcmdparam",
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
    (byte*)"xdivrem",
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
    (byte*)"os_getclockspersec",
    (byte*)"os_setmesshandler",
    (byte*)"os_hpcounter",
    (byte*)"os_hpfrequency",
    (byte*)"os_filelastwritetime",
    (byte*)"os_getsystime",
    (byte*)"os_peek",
(byte*)""};
static struct msysnewc_procinforec msysnewc__fnexports[]= {
	{0, 0,0, {0,0,0, 0,0,0, 0,0,0, 0,0,0}}}
;
static i64 msysnewc__fnnprocs=201;
static i64 msysnewc__fnnexports=0;
static i64 msysnewc_fmtparam;
i64 msysnewc_needgap = (i64)0;
static i64 msysnewc_outdev = (i64)1;
static void *  msysnewc_outchan = 0;
static byte *  msysnewc_fmtstr = 0;
static void *  msysnewc_outchan_stack[10];
static i64 msysnewc_outdev_stack[10];
static byte *  msysnewc_fmtstr_stack[10];
static byte msysnewc_needgap_stack[10];
static byte *  msysnewc_ptr_stack[10];
static i64 msysnewc_niostack = (i64)0;
static byte msysnewc_digits[16] = {
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    'A',
    'B',
    'C',
    'D',
    'E',
    'F'
};
static struct msysnewc_fmtrec msysnewc_defaultfmt = {
    (u8)0u,
    (i8)0,
    (u8)10u,
    (u64)0u,
    ' ',
    'f',
    (u64)0u,
    (u64)0u,
    (u64)0u,
    'R',
    (u64)0u,
    (u64)0u,
    (u64)0u,
    (u64)0u,
    (u64)0u,
    (u8)0u
};
static byte *  msysnewc_rd_buffer;
static i64 msysnewc_rd_length;
static byte *  msysnewc_rd_pos;
static byte *  msysnewc_rd_lastpos;
static i64 msysnewc_termchar;
static i64 msysnewc_itemerror;
i64 msysnewc_nsysparams;
byte *  msysnewc_sysparams[128];
static u64 msysnewc_callbackstack[9][8];
static i64 msysnewc_ncallbacks = (i64)0;
static u64 msysnewc_mask63 = (u64)9223372036854775807u;
static double msysnewc_offset64 = (double)9223372036854775800.;
static double msysnewc_offset32 = (double)9223372036854775800.;
static i64 oslinux_init_flag = (i64)0;

/* PROCDEFS */
static struct b_treenode * b_newtreenode(struct b_treenode * left,struct b_treenode * right,i64 item) {
    struct b_treenode *  t;
    t = (struct b_treenode *)(mlib_pcm_alloc((i64)24));
    ++b_nallocs;
    (*t).left = left;
    (*t).right = right;
    (*t).item = item;
    return t;
}

static i64 b_itemcheck(struct b_treenode * tree) {
    if (((*tree).left == 0)) {
        return (*tree).item;
    } else {
        return (((*tree).item + b_itemcheck((*tree).left)) - b_itemcheck((*tree).right));
    };
}

static struct b_treenode * b_bottomuptree(i64 item,i64 depth) {
    if ((depth > (i64)0)) {
        return b_newtreenode(b_bottomuptree((((i64)2 * item) - (i64)1),(depth - (i64)1)),b_bottomuptree(((i64)2 * item),(depth - (i64)1)),item);
    } else {
        return b_newtreenode((struct b_treenode *)(0),(struct b_treenode *)(0),item);
    };
}

static void b_deletetree(struct b_treenode * tree) {
    if (!!((*tree).left)) {
        b_deletetree((*tree).left);
        b_deletetree((*tree).right);
    };
    mlib_pcm_free((void *)(tree),(i64)24);
}

// START
void start(void) {
    struct b_treenode *  stretchtree;
    struct b_treenode *  longlivedtree;
    struct b_treenode *  temptree;
    i64 n;
    i64 depth;
    i64 mindepth;
    i64 maxdepth;
    i64 stretchdepth;
    i64 check;
    i64 iterations;
    i64 i;
    n = (i64)15;
    n = (i64)16;
    mindepth = (i64)4;
    if (((mindepth + (i64)2) > n)) {
        maxdepth = (mindepth + (i64)1);
    } else {
        maxdepth = n;
    };
    stretchdepth = (maxdepth + (i64)1);
    stretchtree = b_bottomuptree((i64)0,stretchdepth);
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"Stretch tree of depth",NULL);
    msysnewc_m_print_i64(stretchdepth,NULL);
    msysnewc_m_print_str((byte*)"\tcheck:",NULL);
    msysnewc_m_print_i64(b_itemcheck(stretchtree),NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    b_deletetree(stretchtree);
    longlivedtree = b_bottomuptree((i64)0,maxdepth);
    depth = mindepth;
    L1 :;
    while ((depth <= maxdepth)) {
        iterations = ((i64)1 << ((maxdepth - depth) + mindepth));
        check = (i64)0;
        L4 :;
        for (i=(i64)1;i<=iterations;i+=(i64)1) {
L5 :;
            temptree = b_bottomuptree(i,depth);
            check += b_itemcheck(temptree);
            b_deletetree(temptree);
            temptree = b_bottomuptree(-(i),depth);
            check += b_itemcheck(temptree);
            b_deletetree(temptree);
L6 :;
        }L7 :;
        ;
        msysnewc_m_print_startcon();
        msysnewc_m_print_i64((iterations * (i64)2),NULL);
        msysnewc_m_print_str((byte*)"\tTrees of depth",NULL);
        msysnewc_m_print_i64(depth,NULL);
        msysnewc_m_print_str((byte*)" check:",NULL);
        msysnewc_m_print_i64(check,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        depth += (i64)2;
L2 :;
    }L3 :;
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"long lived tree of depth",NULL);
    msysnewc_m_print_i64(maxdepth,NULL);
    msysnewc_m_print_str((byte*)"\tcheck:",NULL);
    msysnewc_m_print_i64(b_itemcheck(longlivedtree),NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"NALLOCS=",NULL);
    msysnewc_m_print_i64(b_nallocs,NULL);
    msysnewc_m_print_i64((b_nallocs * (i64)24),NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

int main(int nargs, char** args) {
int i;
	msysnewc_nsysparams=nargs;
	if (msysnewc_nsysparams>nargs) {puts("Too many params"); exit(1);}
	for (i=1; i<=nargs; ++i) msysnewc_sysparams[i-1]=(byte*)args[i-1];


	start();
	return 0;
}

void * mlib_pcm_alloc(i64 n) {
    byte *  p;
    if (!(!!((u64)(mlib_pcm_setup)))) {
        mlib_pcm_init();
    };
    if ((n > (i64)2048)) {
        mlib_alloccode = mlib_pcm_getac(n);
        mlib_allocbytes = (i64)(mlib_allocupper[(mlib_alloccode)]);
        p = (byte *)(mlib_allocmem(mlib_allocbytes));
        if (!(!!(p))) {
            mlib_abortprogram((byte*)"pcm_alloc failure");
        };
        if (!!((i64)0)) {
            mlib_addtomemalloc((i32 *)(p),mlib_allocbytes);
        };
        return (void *)(p);
    };
    mlib_alloccode = (i64)(mlib_sizeindextable[(n)]);
    if ((mlib_alloccode == (i64)0)) {
        mlib_alloccode = (i64)1;
    };
    mlib_allocbytes = (i64)(mlib_allocupper[(mlib_alloccode)]);
    mlib_smallmemtotal += mlib_allocbytes;
    if (!!((p = (byte *)(mlib_freelist[(mlib_alloccode)])))) {
        if (!!((i64)0)) {
            mlib_addtomemalloc((i32 *)(p),mlib_allocbytes);
        };
        mlib_freelist[(mlib_alloccode)] = (u64 *)((i64)((*mlib_freelist[(mlib_alloccode)])));
        return (void *)(p);
    };
    p = mlib_pcheapptr;
    mlib_pcheapptr += mlib_allocbytes;
    if ((mlib_pcheapptr >= mlib_pcheapend)) {
        p = (byte *)(mlib_pcm_newblock(mlib_allocbytes));
        return (void *)(p);
    };
    if (!!((i64)0)) {
        mlib_addtomemalloc((i32 *)(p),mlib_allocbytes);
    };
    return (void *)(p);
}

void mlib_pcm_freestr(byte * s) {
    mlib_pcm_free((void *)(s),((i64)(strlen((i8 *)(s))) + (i64)1));
}

void mlib_pcm_free(void * p,i64 n) {
    i64 acode;
    if ((n == (i64)0)) {
        return;
    };
    if ((n > (i64)2048)) {
        if (!!((i64)0)) {
            mlib_removefrommemalloc((i32 *)(p),n);
        };
        free(p);
        return;
    };
    if (!!(p)) {
        acode = (i64)(mlib_sizeindextable[(n)]);
        mlib_smallmemtotal -= (i64)(mlib_allocupper[(acode)]);
        if (!!((i64)0)) {
            mlib_removefrommemalloc((i32 *)(p),(i64)(mlib_allocupper[(acode)]));
        };
        (*(u64 *)(p)) = (u64)((i64)(mlib_freelist[(acode)]));
        mlib_freelist[(acode)] = (u64 *)(p);
    };
}

void mlib_pcm_freeac(void * p,i64 alloc) {
    mlib_pcm_free(p,(i64)(mlib_allocupper[(alloc)]));
}

void mlib_pcm_copymem4(void * p,void * q,i64 n) {
    memcpy(p,q,(u64)(n));
}

void mlib_pcm_clearmem(void * p,i64 n) {
    memset(p,(i64)0,(u64)(n));
}

void mlib_pcm_init(void) {
    i64 j;
    i64 k;
    i64 size;
    i64 av_1;
    i64 i;
    if (!!((u64)(mlib_pcm_setup))) {
        return;
    };
    mlib_pcm_newblock((i64)0);
    L8 :;
    for (i=(i64)1;i<=(i64)2048;i+=(i64)1) {
L9 :;
        j = (i64)1;
        k = (i64)16;
        L12 :;
        while ((i > k)) {
            k = (k << (i64)1);
            ++j;
L13 :;
        }L14 :;
        ;
        mlib_sizeindextable[(i)] = (u64)(j);
L10 :;
    }L11 :;
    ;
    mlib_allocupper[((i64)1)] = (u64)((i64)16);
    size = (i64)16;
    L15 :;
    for (i=(i64)2;i<=(i64)27;i+=(i64)1) {
L16 :;
        size *= (i64)2;
        mlib_allocupper[(i)] = (u64)(size);
        if ((size >= (i64)33554432)) {
            k = i;
            goto L18 ;
        };
L17 :;
    }L18 :;
    ;
    L19 :;
    for (i=(k + (i64)1);i<=(i64)300;i+=(i64)1) {
L20 :;
        size += (i64)33554432;
        if ((size < (i64)8589934592)) {
            mlib_allocupper[(i)] = (u64)(size);
            mlib_maxmemory = (u64)(size);
        } else {
            mlib_maxalloccode = (i - (i64)1);
            goto L22 ;
        };
L21 :;
    }L22 :;
    ;
    mlib_pcm_setup = (u64)((i64)1);
}

i64 mlib_pcm_getac(i64 size) {
    if ((size <= (i64)2048)) {
        return (i64)(mlib_sizeindextable[(size)]);
    };
    size = ((size + (i64)255) >> (i64)8);
    if ((size <= (i64)2048)) {
        return ((i64)((u64)(mlib_sizeindextable[(size)])) + (i64)8);
    };
    size = ((size + (i64)63) >> (i64)6);
    if ((size <= (i64)2048)) {
        return ((i64)((u64)(mlib_sizeindextable[(size)])) + (i64)14);
    };
    size = ((((size - (i64)2048) + (i64)2047) / (i64)2048) + (i64)22);
    return size;
}

void * mlib_pcm_newblock(i64 itemsize) {
    static i64 totalheapsize;
    byte *  p;
    totalheapsize += (i64)2097152;
    mlib_alloccode = (i64)0;
    p = (byte *)(mlib_allocmem((i64)2097152));
    if ((p == 0)) {
        mlib_abortprogram((byte*)"Can't alloc pc heap");
    };
    mlib_pcheapptr = p;
    mlib_pcheapend = (p + (i64)2097152);
    if ((mlib_pcheapstart == 0)) {
        mlib_pcheapstart = p;
    };
    mlib_pcheapptr += itemsize;
    return (void *)((u32 *)(p));
}

i64 mlib_pcm_round(i64 n) {
    static i32 allocbytes[9] = {(i32)0,(i32)16,(i32)32,(i32)64,(i32)128,(i32)256,(i32)512,(i32)1024,(i32)2048};
    if ((n > (i64)2048)) {
        return n;
    } else {
        return (i64)(allocbytes[((i64)(mlib_sizeindextable[(n)]))]);
    };
}

i64 mlib_pcm_array(i64 n) {
    i64 m;
    if ((n <= (i64)2048)) {
        return mlib_pcm_round(n);
    } else {
        m = (i64)2048;
        L23 :;
        while ((n > m)) {
            m <<= (i64)1;
L24 :;
        }L25 :;
        ;
        return m;
    };
}

void mlib_pcm_printfreelist(i64 size,u64 * p) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"Size: ",NULL);
    msysnewc_m_print_i64(size,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    L26 :;
    while (!!(p)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)" ",NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_ptr(p,(byte*)"h");
        msysnewc_m_print_end();
        ;
        p = (u64 *)((i64)((*p)));
L27 :;
    }L28 :;
    ;
    puts((i8 *)((byte*)""));
}

void mlib_pcm_diags(byte * caption) {
    i64 m;
    i64 i;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"HEAP FREELISTS:",NULL);
    msysnewc_m_print_str(caption,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    m = (i64)16;
    L29 :;
    for (i=(i64)1;i<=(i64)8;i+=(i64)1) {
L30 :;
        mlib_pcm_printfreelist(m,mlib_freelist[(i)]);
        m <<= (i64)1;
L31 :;
    }L32 :;
    ;
}

void * mlib_pcm_allocz(i64 n) {
    void *  p;
    p = mlib_pcm_alloc(n);
    memset(p,(i64)0,(u64)(n));
    return p;
}

byte * mlib_pcm_copyheapstring(byte * s) {
    byte *  q;
    i64 n;
    if ((s == 0)) {
        return (byte *)(0);
    };
    n = ((i64)(strlen((i8 *)(s))) + (i64)1);
    q = (byte *)(mlib_pcm_alloc(n));
    memcpy((void *)(q),(void *)(s),(u64)(n));
    return q;
}

byte * mlib_pcm_copyheapstringn(byte * s,i64 n) {
    byte *  q;
    if ((s == 0)) {
        return (byte *)(0);
    };
    q = (byte *)(mlib_pcm_alloc((n + (i64)1)));
    memcpy((void *)(q),(void *)(s),(u64)(n));
    (*(q + n)) = (u64)0u;
    return q;
}

byte * mlib_pcm_copyheapblock(byte * s,i64 length) {
    byte *  q;
    if ((length == (i64)0)) {
        return (byte *)(0);
    };
    q = (byte *)(mlib_pcm_alloc(length));
    memcpy((void *)(q),(void *)(s),(u64)(length));
    return q;
}

static void mlib_addtomemalloc(i32 * ptr,i64 size) {
    i64 i;
    L33 :;
    for (i=(i64)1;i<=(i64)500000;i+=(i64)1) {
L34 :;
        if ((mlib_memalloctable[(i)-1] == ptr)) {
            msysnewc_m_print_startcon();
            msysnewc_m_print_str((byte*)"ALLOC ERROR:",NULL);
            msysnewc_m_print_ptr(ptr,NULL);
            msysnewc_m_print_str((byte*)"ALREADY ALLOCATED\n\n\n",NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            msysnewc_m_print_startcon();
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            msysnewc_m_print_startcon();
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            exit((i64)2);
        };
        if ((mlib_memalloctable[(i)-1] == 0)) {
            mlib_memalloctable[(i)-1] = ptr;
            mlib_memallocsize[(i)-1] = size;
            return;
        };
L35 :;
    }L36 :;
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"MEMALLOCTABLE FULL\n\n\n\n",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    oslinux_os_getch();
    exit((i64)3);
}

static void mlib_removefrommemalloc(i32 * ptr,i64 size) {
    i64 i;
    L37 :;
    for (i=(i64)1;i<=(i64)500000;i+=(i64)1) {
L38 :;
        if ((mlib_memalloctable[(i)-1] == ptr)) {
            if (((i64)(mlib_memallocsize[(i)-1]) != size)) {
                msysnewc_m_print_startcon();
                msysnewc_m_print_str((byte*)"REMOVE:FOUND",NULL);
                msysnewc_m_print_ptr(ptr,NULL);
                msysnewc_m_print_str((byte*)"IN MEMALLOCTABLE, FREESIZE=",NULL);
                msysnewc_m_print_i64(size,NULL);
                msysnewc_m_print_str((byte*)", BUT STORED AS BLOCK SIZE:",NULL);
                msysnewc_m_print_i64(mlib_memallocsize[(i)-1],NULL);
                msysnewc_m_print_newline();
                msysnewc_m_print_end();
                ;
                msysnewc_m_print_startcon();
                msysnewc_m_print_newline();
                msysnewc_m_print_end();
                ;
                msysnewc_m_print_startcon();
                msysnewc_m_print_newline();
                msysnewc_m_print_end();
                ;
                mlib_abortprogram((byte*)"MEMSIZE");
            };
            mlib_memalloctable[(i)-1] = (i32 *)(0);
            return;
        };
L39 :;
    }L40 :;
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"CAN'T FIND",NULL);
    msysnewc_m_print_ptr(ptr,NULL);
    msysnewc_m_print_str((byte*)"IN MEMALLOCTABLE",NULL);
    msysnewc_m_print_i64(size,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    mlib_abortprogram((byte*)"MEM");
    exit((i64)4);
}

void * mlib_allocmem(i64 n) {
    void *  p;
    p = malloc((u64)(n));
    if (!!(p)) {
        return p;
    };
    msysnewc_m_print_startcon();
    msysnewc_m_print_i64(n,NULL);
    msysnewc_m_print_i64(mlib_memtotal,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    mlib_abortprogram((byte*)"Alloc mem failure");
    return 0;
}

void * mlib_reallocmem(void * p,i64 n) {
    p = realloc(p,(u64)(n));
    if (!!(p)) {
        return p;
    };
    msysnewc_m_print_startcon();
    msysnewc_m_print_i64(n,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    mlib_abortprogram((byte*)"Realloc mem failure");
    return 0;
}

void mlib_abortprogram(byte * s) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str(s,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"ABORTING: Press key...",NULL);
    msysnewc_m_print_end();
    ;
    oslinux_os_getch();
    exit((i64)5);
}

i64 mlib_getfilesize(void * handlex) {
    u32 p;
    u32 size;
    p = (u64)(ftell(handlex));
    fseek(handlex,(i64)0,(i64)2);
    size = (u64)(ftell(handlex));
    fseek(handlex,(i64)((i32)(p)),(i64)0);
    return (i64)(size);
}

void mlib_readrandom(void * handlex,byte * mem,i64 offset,i64 size) {
    i64 a;
    fseek(handlex,(i64)((i32)(offset)),(i64)0);
    a = (i64)(fread((void *)(mem),(u64)((i64)1),(u64)(size),handlex));
}

i64 mlib_writerandom(void * handlex,byte * mem,i64 offset,i64 size) {
    fseek(handlex,(i64)((i32)(offset)),(i64)0);
    return (i64)(fwrite((void *)(mem),(u64)((i64)1),(u64)(size),handlex));
}

i64 mlib_setfilepos(void * file,i64 offset) {
    return (i64)(fseek(file,(i64)((i32)(offset)),(i64)0));
}

i64 mlib_getfilepos(void * file) {
    return (i64)(ftell(file));
}

byte * mlib_readfile(byte * filename) {
    void *  f;
    i64 size;
    byte *  m;
    byte *  p;
    f = fopen((i8 *)(filename),(i8 *)((byte*)"rb"));
    if ((f == 0)) {
        return (byte *)(0);
    };
    mlib_rfsize = (size = mlib_getfilesize(f));
    m = (byte *)(malloc((u64)((size + (i64)4))));
    if ((m == 0)) {
        return (byte *)(0);
    };
    mlib_readrandom(f,m,(i64)0,size);
    p = (m + size);
    (*p) = (u64)((i64)0);
    (*(p + (i64)1)) = (u64)((i64)26);
    (*(p + (i64)2)) = (u64)((i64)0);
    fclose(f);
    return m;
}

i64 mlib_writefile(byte * filename,byte * data,i64 size) {
    void *  f;
    i64 n;
    f = fopen((i8 *)(filename),(i8 *)((byte*)"wb"));
    if ((f == 0)) {
        return (i64)0;
    };
    n = mlib_writerandom(f,data,(i64)0,size);
    fclose(f);
    return n;
}

i64 mlib_checkfile(byte * file) {
    void *  f;
    if (!!((f = fopen((i8 *)(file),(i8 *)((byte*)"rb"))))) {
        fclose(f);
        return (i64)1;
    };
    return (i64)0;
}

void mlib_readlinen(void * handlex,byte * buffer,i64 size) {
    i64 ch;
    byte *  p;
    i64 n;
    byte crseen;
    if ((handlex == 0)) {
        handlex = oslinux_os_getstdin();
    };
    if ((handlex == 0)) {
        n = (i64)0;
        p = buffer;
        L41 :;
        while (1) {
            ch = (i64)(getchar());
            if ((((ch == (i64)13) || (ch == (i64)10)) || (ch == (i64)-1))) {
                (*p) = (u64)0u;
                return;
            };
            (*p++) = (u64)(ch);
            ++n;
            if ((n >= (size - (i64)2))) {
                (*p) = (u64)0u;
                return;
            };
        }L42 :;
        ;
    };
    (*buffer) = (u64)0u;
    if ((fgets((i8 *)(buffer),(size - (i64)2),handlex) == 0)) {
        return;
    };
    n = (i64)(strlen((i8 *)(buffer)));
    if ((n == (i64)0)) {
        return;
    };
    p = ((buffer + n) - (i64)1);
    crseen = (u64)((i64)0);
    L43 :;
    while (((p >= buffer) && (((i64)((*p)) == (i64)13) || ((i64)((*p)) == (i64)10)))) {
        if ((((i64)((*p)) == (i64)13) || ((i64)((*p)) == (i64)10))) {
            crseen = (u64)((i64)1);
        };
        (*p--) = (u64)0u;
L44 :;
    }L45 :;
    ;
    if ((!(!!((u64)(crseen))) && ((n + (i64)4) > size))) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_i64(size,NULL);
        msysnewc_m_print_i64(n,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        mlib_abortprogram((byte*)"line too long");
    };
}

void mlib_iconvlcn(byte * s,i64 n) {
    i64 av_1;
    av_1 = n;
    while (av_1-- > 0) {
L46 :;
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L47 :;
    }L48 :;
    ;
}

void mlib_iconvucn(byte * s,i64 n) {
    i64 av_1;
    av_1 = n;
    while (av_1-- > 0) {
L49 :;
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L50 :;
    }L51 :;
    ;
}

void mlib_convlcstring(byte * s) {
    L52 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L53 :;
    }L54 :;
    ;
}

void mlib_convucstring(byte * s) {
    L55 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L56 :;
    }L57 :;
    ;
}

byte * mlib_changeext(byte * s,byte * newext) {
    static byte newfile[260];
    byte newext2[32];
    byte *  sext;
    i64 n;
    strcpy((i8 *)(&newfile[((i64)1)-1]),(i8 *)(s));
    if (((i64)((*newext))==(i64)0)) {
        newext2[((i64)1)-1] = (u64)0u;
        newext2[((i64)2)-1] = (u64)0u;
    }else if (((i64)((*newext))==(i64)46)) {
        strcpy((i8 *)(&newext2[((i64)1)-1]),(i8 *)(newext));
    } else {
        strcpy((i8 *)(&newext2[((i64)1)-1]),(i8 *)((byte*)"."));
        strcat((i8 *)(&newext2[((i64)1)-1]),(i8 *)(newext));
    };
    sext = mlib_extractext(s,(i64)1);
    if (((i64)((*sext))==(i64)0)) {
        strcat((i8 *)(&newfile[((i64)1)-1]),(i8 *)(&newext2[((i64)1)-1]));
    }else if (((i64)((*sext))==(i64)46)) {
        strcat((i8 *)(&newfile[((i64)1)-1]),(i8 *)(&newext2[((i64)2)-1]));
    } else {
        n = ((sext - s) - (i64)2);
        strcpy((i8 *)(((&newfile[((i64)1)-1] + n) + (i64)1)),(i8 *)(&newext2[((i64)1)-1]));
    };
    return &newfile[((i64)1)-1];
}

byte * mlib_extractext(byte * s,i64 period) {
    byte *  t;
    byte *  u;
    t = mlib_extractfile(s);
    if (((i64)((*t)) == (i64)0)) {
        return (byte*)"";
    };
    u = ((t + (i64)(strlen((i8 *)(t)))) - (i64)1);
    L58 :;
    while ((u >= t)) {
        if (((u64)((*u)) == '.')) {
            if (((i64)((*(u + (i64)1))) == (i64)0)) {
                return (!!(period)?(byte*)".":(byte*)"");
            };
            return (u + (i64)1);
        };
        --u;
L59 :;
    }L60 :;
    ;
    return (byte*)"";
}

byte * mlib_extractpath(byte * s) {
    static byte str[260];
    byte *  t;
    i64 n;
    t = ((s + (i64)(strlen((i8 *)(s)))) - (i64)1);
    L61 :;
    while ((t >= s)) {
        switch ((i64)((*t))) {
        case 92:;
        case 47:;
        case 58:;
        {
            n = ((t - s) + (i64)1);
            memcpy((void *)(str),(void *)(s),(u64)(n));
            str[(n)] = (u64)0u;
            return str;
        }break;
        default: {
        }
        } //SW
;
        --t;
L62 :;
    }L63 :;
    ;
    return (byte*)"";
}

byte * mlib_extractfile(byte * s) {
    byte *  t;
    t = mlib_extractpath(s);
    if (((i64)((*t)) == (i64)0)) {
        return s;
    };
    return (s + (i64)(strlen((i8 *)(t))));
}

byte * mlib_extractbasefile(byte * s) {
    static byte str[100];
    byte *  f;
    byte *  e;
    i64 n;
    i64 flen;
    f = mlib_extractfile(s);
    flen = (i64)(strlen((i8 *)(f)));
    if ((flen == (i64)0)) {
        return (byte*)"";
    };
    e = mlib_extractext(f,(i64)0);
    if (!!((u64)((*e)))) {
        n = ((flen - (i64)(strlen((i8 *)(e)))) - (i64)1);
        memcpy((void *)(&str),(void *)(f),(u64)(n));
        str[(n)] = (u64)0u;
        return str;
    };
    if (((u64)((*((f + flen) - (i64)1))) == '.')) {
        memcpy((void *)(&str),(void *)(f),(u64)((flen - (i64)1)));
        str[((flen - (i64)1))] = (u64)0u;
        return str;
    };
    return f;
}

byte * mlib_addext(byte * s,byte * newext) {
    byte *  sext;
    sext = mlib_extractext(s,(i64)1);
    if (((i64)((*sext)) == (i64)0)) {
        return mlib_changeext(s,newext);
    };
    return s;
}

void * mlib_alloctable(i64 n,i64 size) {
    void *  p;
    p = malloc((u64)(((n + (i64)1) * size)));
    if (!(!!(p))) {
        mlib_abortprogram((byte*)"Alloctable failure");
    };
    return p;
}

void * mlib_zalloctable(i64 n,i64 size) {
    i64 *  p;
    p = (i64 *)(mlib_alloctable(n,size));
    mlib_pcm_clearmem((void *)(p),((n + (i64)1) * size));
    return (void *)(p);
}

void mlib_checkfreelists(byte * s) {
    u64 *  p;
    u64 *  q;
    i64 aa;
    i64 i;
    L64 :;
    for (i=(i64)2;i<=(i64)2;i+=(i64)1) {
L65 :;
        p = mlib_freelist[(i)];
        L68 :;
        while (!!(p)) {
            aa = (i64)(p);
            if (((aa > (i64)4294967295) || (aa < (i64)100))) {
                msysnewc_m_print_startcon();
                msysnewc_m_print_str(s,NULL);
                msysnewc_m_print_str((byte*)"FREE LIST ERROR",NULL);
                msysnewc_m_print_i64(i,NULL);
                msysnewc_m_print_ptr(p,NULL);
                msysnewc_m_print_ptr(q,NULL);
                msysnewc_m_print_newline();
                msysnewc_m_print_end();
                ;
            };
            q = p;
            p = (u64 *)((i64)((*p)));
L69 :;
        }L70 :;
        ;
L66 :;
    }L67 :;
    ;
}

void * mlib_pcm_alloc32(void) {
    mlib_allocbytes = (i64)32;
    return mlib_pcm_alloc((i64)32);
}

void mlib_pcm_free32(void * p) {
    mlib_smallmemtotal -= (i64)32;
    if (!!((i64)0)) {
        mlib_removefrommemalloc((i32 *)(p),(i64)32);
    };
    (*(u64 *)(p)) = (u64)((i64)(mlib_freelist[((i64)2)]));
    mlib_freelist[((i64)2)] = (u64 *)(p);
}

void mlib_outbyte(void * f,i64 x) {
    fwrite((void *)(&x),(u64)((i64)1),(u64)((i64)1),f);
}

void mlib_outword16(void * f,u64 x) {
    fwrite((void *)(&x),(u64)((i64)2),(u64)((i64)1),f);
}

void mlib_outword(void * f,u64 x) {
    fwrite((void *)(&x),(u64)((i64)4),(u64)((i64)1),f);
}

void mlib_outword64(void * f,u64 x) {
    fwrite((void *)(&x),(u64)((i64)8),(u64)((i64)1),f);
}

i64 mlib_myeof(void * f) {
    i64 c;
    c = (i64)(fgetc(f));
    if ((c == (i64)-1)) {
        return (i64)1;
    };
    ungetc((i64)((i32)(c)),f);
    return (i64)0;
}

void * mlib_pcm_smallallocz(i64 n) {
    byte *  p;
    if (((mlib_alloccode = (i64)(mlib_sizeindextable[(n)])) == (i64)0)) {
        mlib_alloccode = (i64)1;
    };
    mlib_allocbytes = (i64)(mlib_allocupper[(mlib_alloccode)]);
    p = mlib_pcheapptr;
    mlib_pcheapptr += mlib_allocbytes;
    if ((mlib_pcheapptr >= mlib_pcheapend)) {
        p = (byte *)(mlib_pcm_newblock(mlib_allocbytes));
        memset((void *)(p),(i64)0,(u64)(n));
        return (void *)(p);
    };
    memset((void *)(p),(i64)0,(u64)(n));
    return (void *)(p);
}

void * mlib_pcm_smallalloc(i64 n) {
    byte *  p;
    if (((mlib_alloccode = (i64)(mlib_sizeindextable[(n)])) == (i64)0)) {
        mlib_alloccode = (i64)1;
    };
    mlib_allocbytes = (i64)(mlib_allocupper[(mlib_alloccode)]);
    p = mlib_pcheapptr;
    mlib_pcheapptr += mlib_allocbytes;
    if ((mlib_pcheapptr >= mlib_pcheapend)) {
        p = (byte *)(mlib_pcm_newblock(mlib_allocbytes));
        return (void *)(p);
    };
    return (void *)(p);
}

void mlib_strbuffer_add(struct mlib_strbuffer * dest,byte * s,i64 n) {
    i64 newlen;
    i64 oldlen;
    byte *  newptr;
    if ((n == (i64)0)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"N=0",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    };
    if ((n == (i64)-1)) {
        n = (i64)(strlen((i8 *)(s)));
    };
    oldlen = (i64)((*dest).length);
    if ((oldlen == (i64)0)) {
        (*dest).strptr = (byte *)(mlib_pcm_alloc((n + (i64)1)));
        (*dest).allocated = mlib_allocbytes;
        (*dest).length = n;
        memcpy((void *)((*dest).strptr),(void *)(s),(u64)(n));
        (*((*dest).strptr + n)) = (u64)0u;
        return;
    };
    newlen = (oldlen + n);
    if (((newlen + (i64)1) > (i64)((*dest).allocated))) {
        newptr = (byte *)(mlib_pcm_alloc((newlen + (i64)1)));
        memcpy((void *)(newptr),(void *)((*dest).strptr),(u64)(oldlen));
        (*dest).strptr = newptr;
        (*dest).allocated = mlib_allocbytes;
    };
    memcpy((void *)(((*dest).strptr + oldlen)),(void *)(s),(u64)(n));
    (*((*dest).strptr + newlen)) = (u64)0u;
    (*dest).length = newlen;
}

void mlib_gs_init(struct mlib_strbuffer * dest) {
    mlib_pcm_clearmem((void *)(dest),(i64)16);
}

void mlib_gs_free(struct mlib_strbuffer * dest) {
    if (!!((i64)((*dest).allocated))) {
        mlib_pcm_free((void *)((*dest).strptr),(i64)((*dest).allocated));
    };
}

void mlib_gs_str(struct mlib_strbuffer * dest,byte * s) {
    mlib_strbuffer_add(dest,s,(i64)-1);
}

void mlib_gs_char(struct mlib_strbuffer * dest,i64 c) {
    byte s[16];
    s[((i64)1)-1] = (u64)(c);
    s[((i64)2)-1] = (u64)0u;
    mlib_strbuffer_add(dest,s,(i64)1);
}

void mlib_gs_strn(struct mlib_strbuffer * dest,byte * s,i64 length) {
    mlib_strbuffer_add(dest,s,length);
}

void mlib_gs_strvar(struct mlib_strbuffer * dest,struct mlib_strbuffer * s) {
    mlib_strbuffer_add(dest,(*s).strptr,(i64)-1);
}

void mlib_gs_strint(struct mlib_strbuffer * dest,i64 a) {
    mlib_strbuffer_add(dest,msysnewc_strint(a,(byte *)(0)),(i64)-1);
}

void mlib_gs_strln(struct mlib_strbuffer * dest,byte * s) {
    mlib_gs_str(dest,s);
    mlib_gs_line(dest);
}

void mlib_gs_strsp(struct mlib_strbuffer * dest,byte * s) {
    mlib_gs_str(dest,s);
    mlib_gs_str(dest,(byte*)" ");
}

void mlib_gs_line(struct mlib_strbuffer * dest) {
    mlib_strbuffer_add(dest,(byte*)"\r\n",(i64)-1);
}

i64 mlib_gs_getcol(struct mlib_strbuffer * dest) {
    return (i64)((*dest).length);
}

void mlib_gs_leftstr(struct mlib_strbuffer * dest,byte * s,i64 w,i64 padch) {
    i64 col;
    i64 i;
    i64 n;
    i64 slen;
    byte str[2560];
    col = (i64)((*dest).length);
    strcpy((i8 *)(str),(i8 *)(s));
    slen = (i64)(strlen((i8 *)(s)));
    n = (w - slen);
    if ((n > (i64)0)) {
        L71 :;
        for (i=(i64)1;i<=n;i+=(i64)1) {
L72 :;
            str[((slen + i))-1] = (u64)(padch);
L73 :;
        }L74 :;
        ;
        str[(((slen + n) + (i64)1))-1] = (u64)0u;
    };
    mlib_gs_str(dest,str);
}

void mlib_gs_leftint(struct mlib_strbuffer * dest,i64 a,i64 w,i64 padch) {
    mlib_gs_leftstr(dest,msysnewc_strint(a,(byte *)(0)),w,padch);
}

void mlib_gs_padto(struct mlib_strbuffer * dest,i64 col,i64 ch) {
    i64 n;
    byte str[2560];
    i64 i;
    n = (col - (i64)((*dest).length));
    if ((n <= (i64)0)) {
        return;
    };
    L75 :;
    for (i=(i64)1;i<=n;i+=(i64)1) {
L76 :;
        str[(i)-1] = (u64)(ch);
L77 :;
    }L78 :;
    ;
    str[((n + (i64)1))-1] = (u64)0u;
    mlib_gs_str(dest,str);
}

void mlib_gs_println(struct mlib_strbuffer * dest,void * f) {
    (*((*dest).strptr + (i64)((*dest).length))) = (u64)0u;
    if ((f == 0)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((*dest).strptr,NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_str((byte*)"\r",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    } else {
        msysnewc_m_print_startfile(f);
        msysnewc_m_print_str((*dest).strptr,NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_str((byte*)"\r",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    };
}

i64 mlib_nextcmdparam(i64 * paramno,byte * * name,byte * * value,byte * defext) {
    static i64 infile = (i64)0;
    static byte *  filestart = 0;
    static byte *  fileptr = 0;
    static byte colonseen = (u8)0u;
    byte *  q;
    byte *  item;
    byte *  fileext;
    i64 length;
    static byte str[300];
    //reenter:
L79 :;
;
    (*value) = (byte *)(0);
    (*name) = (byte *)(0);
    if (!!(infile)) {
        if ((mlib_readnextfileitem(&fileptr,&item) == (i64)0)) {
            free((void *)(filestart));
            infile = (i64)0;
            goto L79 ;
;
        };
    } else {
        if (((*paramno) > msysnewc_nsysparams)) {
            return (i64)0;
        };
        item = msysnewc_sysparams[((*paramno))-1];
        ++(*paramno);
        length = (i64)(strlen((i8 *)(item)));
        if (((u64)((*item)) == '@')) {
            filestart = (fileptr = (byte *)(mlib_readfile((item + (i64)1))));
            if ((filestart == 0)) {
                msysnewc_m_print_startcon();
                msysnewc_m_print_str((byte*)"Can't open",NULL);
                msysnewc_m_print_str(item,NULL);
                msysnewc_m_print_newline();
                msysnewc_m_print_end();
                ;
                exit((i64)7);
            };
            infile = (i64)1;
            goto L79 ;
;
        };
        if (((u64)((*item)) == ':')) {
            colonseen = (u64)((i64)1);
            return (i64)4;
        };
    };
    (*value) = (byte *)(0);
    if (((u64)((*item)) == '-')) {
        (*name) = (item + (!!((u64)(colonseen))?(i64)0:(i64)1));
        q = (byte *)(strchr((i8 *)(item),(i64)58));
        if (!(!!(q))) {
            q = (byte *)(strchr((i8 *)(item),(i64)61));
        };
        if (!!(q)) {
            (*value) = (q + (i64)1);
            (*q) = (u64)0u;
        };
        return (!!((u64)(colonseen))?(i64)5:(i64)1);
    };
    fileext = mlib_extractext(item,(i64)0);
    (*name) = item;
    if (((i64)((*fileext)) == (i64)0)) {
        strcpy((i8 *)(str),(i8 *)((*name)));
        if ((!!(defext) && !(!!((u64)(colonseen))))) {
            (*name) = mlib_addext(str,defext);
        };
    } else if (!!(mlib_eqstring(fileext,(byte*)"dll"))) {
        return (!!((u64)(colonseen))?(i64)5:(i64)3);
    };
    return (!!((u64)(colonseen))?(i64)5:(i64)2);
}

static i64 mlib_readnextfileitem(byte * * fileptr,byte * * item) {
    byte *  p;
    byte *  pstart;
    byte *  pend;
    i64 n;
    static byte str[256];
    p = (*fileptr);
    //reenter:
L80 :;
;
    L81 :;
    while (1) {
        if (((i64)((*p))==(i64)32) || ((i64)((*p))==(i64)9) || ((i64)((*p))==(i64)13) || ((i64)((*p))==(i64)10)) {
            ++p;
        }else if (((i64)((*p))==(i64)26) || ((i64)((*p))==(i64)0)) {
            return (i64)0;
        } else {
            goto L82 ;
        };
    }L82 :;
    ;
    if (((i64)((*p))==(i64)33) || ((i64)((*p))==(i64)35)) {
        ++p;
        L83 :;
        if (((i64)((*p++))==(i64)10)) {
            goto L80 ;
;
        }else if (((i64)((*p++))==(i64)26) || ((i64)((*p++))==(i64)0)) {
            (*fileptr) = (p - (i64)1);
            return (i64)0;
        } else {
        }goto L83 ;
L84 :;
        ;
    };
    if (((i64)((*p))==(i64)34)) {
        pstart = ++p;
        L85 :;
        while (1) {
            if (((i64)((*p))==(i64)0) || ((i64)((*p))==(i64)26)) {
                msysnewc_m_print_startcon();
                msysnewc_m_print_str((byte*)"Unexpected EOF in @file",NULL);
                msysnewc_m_print_newline();
                msysnewc_m_print_end();
                ;
                exit((i64)8);
            }else if (((i64)((*p))==(i64)34)) {
                pend = p++;
                if (((u64)((*p)) == ',')) {
                    ++p;
                };
                goto L86 ;
            };
            ++p;
        }L86 :;
        ;
    } else {
        pstart = p;
        L87 :;
        while (1) {
            if (((i64)((*p))==(i64)0) || ((i64)((*p))==(i64)26)) {
                pend = p;
                goto L88 ;
            }else if (((i64)((*p))==(i64)32) || ((i64)((*p))==(i64)9) || ((i64)((*p))==(i64)44) || ((i64)((*p))==(i64)13) || ((i64)((*p))==(i64)10)) {
                pend = p++;
                goto L88 ;
            };
            ++p;
        }L88 :;
        ;
    };
    n = (pend - pstart);
    if ((n >= (i64)256)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"@file item too long",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        exit((i64)9);
    };
    memcpy((void *)(str),(void *)(pstart),(u64)(n));
    str[((n + (i64)1))-1] = (u64)0u;
    (*item) = str;
    (*fileptr) = p;
    return (i64)1;
}

void mlib_ipadstr(byte * s,i64 width,byte * padchar) {
    i64 n;
    i64 av_1;
    n = (i64)(strlen((i8 *)(s)));
    av_1 = (width - n);
    while (av_1-- > 0) {
L89 :;
        strcat((i8 *)(s),(i8 *)(padchar));
L90 :;
    }L91 :;
    ;
}

byte * mlib_padstr(byte * s,i64 width,byte * padchar) {
    static byte str[256];
    strcpy((i8 *)(str),(i8 *)(s));
    mlib_ipadstr(str,width,padchar);
    return str;
}

byte * mlib_chr(i64 c) {
    static byte str[8];
    str[((i64)1)-1] = (u64)(c);
    str[((i64)2)-1] = (u64)0u;
    return str;
}

i64 mlib_cmpstring(byte * s,byte * t) {
    i64 res;
    if (((res = (i64)(strcmp((i8 *)(s),(i8 *)(t)))) < (i64)0)) {
        return (i64)-1;
    } else if ((res > (i64)0)) {
        return (i64)1;
    } else {
        return (i64)0;
    };
}

i64 mlib_cmpstringn(byte * s,byte * t,i64 n) {
    i64 res;
    if (((res = (i64)(strncmp((i8 *)(s),(i8 *)(t),(u64)(n)))) < (i64)0)) {
        return (i64)-1;
    } else if ((res > (i64)0)) {
        return (i64)1;
    } else {
        return (i64)0;
    };
}

i64 mlib_eqstring(byte * s,byte * t) {
    return ((i64)(strcmp((i8 *)(s),(i8 *)(t))) == (i64)0);
}

i64 mlib_cmpbytes(void * p,void * q,i64 n) {
    i64 res;
    if (((res = (i64)(memcmp(p,q,(u64)(n)))) < (i64)0)) {
        return (i64)-1;
    } else if ((res > (i64)0)) {
        return (i64)1;
    } else {
        return (i64)0;
    };
}

i64 mlib_eqbytes(void * p,void * q,i64 n) {
    return ((i64)(memcmp(p,q,(u64)(n))) == (i64)0);
}

void mlib_mseed(u64 a,u64 b) {
    mlib_seed[((i64)1)-1] = (i64)(a);
    if (!!(b)) {
        mlib_seed[((i64)2)-1] = (i64)(b);
    } else {
        mlib_seed[((i64)2)-1] ^= (i64)(a);
    };
}

u64 mlib_mrandom(void) {
    u64 x;
    u64 y;
    x = (u64)(mlib_seed[((i64)1)-1]);
    y = (u64)(mlib_seed[((i64)2)-1]);
    mlib_seed[((i64)1)-1] = (i64)(y);
    x ^= (x << (i64)23);
    mlib_seed[((i64)2)-1] = (i64)((((x ^ y) ^ (x >> (i64)17)) ^ (y >> (i64)26)));
    return (u64)((mlib_seed[((i64)2)-1] + (i64)(y)));
}

i64 mlib_mrandomp(void) {
    return (i64)((mlib_mrandom() & (u64)9223372036854775807u));
}

i64 mlib_mrandomint(i64 n) {
    return (mlib_mrandomp() % n);
}

i64 mlib_mrandomrange(i64 a,i64 b) {
    i64 span;
    span = ((b - a) + (i64)1);
    if ((span <= (i64)0)) {
        return (i64)0;
    };
    return ((mlib_mrandomp() % span) + a);
}

double mlib_mrandomreal(void) {
    double x;
    L92 :;
    do {
        x = ((double)(mlib_mrandomp()) / (double)9223372036854775800.);
L93 :;
    } while (!(x != (double)1.));L94 :;
    ;
    return x;
}

double mlib_mrandomreal1(void) {
    return (double)((mlib_mrandomp() / (i64)((u64)9223372036854775807u)));
}

byte * mlib_checkpackfile(void) {
    i64 a;
    i64 offset;
    byte exefile[300];
    byte *  packexeptr;
    i64 packexesize;
    byte *  packfilename;
    i64 packfilesize;
    byte *  packfileptr;
    strcpy((i8 *)(&exefile[((i64)1)-1]),(i8 *)(oslinux_os_gethostname()));
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"Attempting to open",NULL);
    msysnewc_m_print_ptr(&exefile,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    packexeptr = mlib_readfile(&exefile[((i64)1)-1]);
    if (!(!!(packexeptr))) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"Can't open",NULL);
        msysnewc_m_print_ptr(&exefile,NULL);
        msysnewc_m_print_ptr(&packexeptr,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        exit(0);
    };
    packexesize = mlib_rfsize;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"File read OK. Size",NULL);
    msysnewc_m_print_i64(packexesize,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    a = (i64)((*(i32 *)((packexeptr + (packexesize - (i64)4)))));
    if ((a != (i64)1262568272)) {
        free((void *)(packexeptr));
        packfileptr = (byte *)(0);
        return (byte *)(0);
    };
    offset = (i64)((*(i32 *)((packexeptr + (packexesize - (i64)8)))));
    packfilename = (byte *)((packexeptr + offset));
    offset += ((i64)(strlen((i8 *)(packfilename))) + (i64)1);
    packfilesize = (i64)((*(i32 *)((packexeptr + offset))));
    packfileptr = ((packexeptr + offset) + (i64)4);
    return packfileptr;
}

i64 msysnewc_m_getdotindex(u64 a,i64 i) {
    return (((i64)(a) & ((i64)1 << i)) >> i);
}

void msysnewc_m_setdotindex(u64 * a,i64 i,i64 x) {
    u32 *  a32;
    if ((i >= (i64)32)) {
        (*a) = (u64)((((i64)((*a)) & ~(((i64)1 << i))) | (i64)(((u64)(x) << i))));
    } else {
        a32 = (u32 *)(a);
        (*a32) = (u64)((((i64)((u64)((*a32))) & ~(((i64)1 << i))) | (i64)(((u64)(x) << i))));
    };
}

i64 msysnewc_m_getdotslice(u64 a,i64 i,i64 j) {
    if ((i >= j)) {
        return (i64)(((a >> j) & ~(((u64)18446744073709551615u << ((i - j) + (i64)1)))));
    } else {
        return (i64)(((a >> i) & ~(((u64)18446744073709551615u << ((j - i) + (i64)1)))));
    };
}

void msysnewc_m_setdotslice(u64 * a,i64 i,i64 j,u64 x) {
    u64 mask64;
    u64 mask;
    u32 *  a32;
    if ((i > j)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"SETDOTSLICE?",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        exit((i64)52);
    };
    if ((j >= (i64)32)) {
        mask64 = (~(((u64)18446744073709551615u << ((j - i) + (i64)1))) << i);
        (*a) = (((*a) & ~(mask64)) | (x << i));
    } else {
        a32 = (u32 *)(a);
        mask = (~(((u64)18446744073709551615u << ((j - i) + (i64)1))) << i);
        (*a32) = (((u64)((*a32)) & ~(mask)) | (x << i));
    };
}

i64 msysnewc_m_get_nprocs(void) {
    return msysnewc__fnnprocs;
}

i64 msysnewc_m_get_nexports(void) {
    return msysnewc__fnnexports;
}

void * msysnewc_m_get_procname(i64 n) {
    return (void *)(msysnewc__fnnames[(n)-1]);
}

byte * msysnewc_m_get_procaddr(i64 n) {
    return (byte *)(msysnewc__fnaddresses[(n)-1]);
}

void * msysnewc_m_get_procexport(i64 n) {
    return (void *)(&msysnewc__fnexports[(n)-1]);
}

static void msysnewc_pushio(void) {
    if ((msysnewc_niostack >= (i64)10)) {
        printf((i8 *)((byte*)"Too many io levels\n"));
        exit((i64)53);
    };
    ++msysnewc_niostack;
    msysnewc_outchan_stack[(msysnewc_niostack)-1] = msysnewc_outchan;
    msysnewc_outdev_stack[(msysnewc_niostack)-1] = msysnewc_outdev;
    msysnewc_fmtstr_stack[(msysnewc_niostack)-1] = msysnewc_fmtstr;
    msysnewc_needgap_stack[(msysnewc_niostack)-1] = (u64)(msysnewc_needgap);
    msysnewc_needgap = (i64)0;
    msysnewc_fmtstr = (byte *)(0);
    msysnewc_outchan = 0;
}

void msysnewc_m_print_startfile(void * dev) {
    msysnewc_pushio();
    msysnewc_outchan = dev;
    if (!!(dev)) {
        msysnewc_outdev = (i64)2;
    } else {
        msysnewc_outdev = (i64)1;
    };
}

void msysnewc_m_print_startstr(byte * s) {
    byte * *  p;
    msysnewc_pushio();
    msysnewc_ptr_stack[(msysnewc_niostack)-1] = s;
    p = &msysnewc_ptr_stack[(msysnewc_niostack)-1];
    msysnewc_outchan = (void *)(p);
    msysnewc_outdev = (i64)3;
}

void msysnewc_m_print_startptr(byte * * p) {
    msysnewc_pushio();
    msysnewc_outchan = (void *)(p);
    msysnewc_outdev = (i64)3;
}

void msysnewc_m_print_startcon(void) {
    msysnewc_pushio();
    msysnewc_outdev = (i64)1;
}

void msysnewc_m_print_setfmt(byte * format) {
    msysnewc_fmtstr = format;
}

void msysnewc_m_print_end(void) {
    msysnewc_needgap = (i64)0;
    msysnewc_nextfmtchars((i64)1);
    if ((msysnewc_niostack == (i64)0)) {
        return;
    };
    msysnewc_outchan = msysnewc_outchan_stack[(msysnewc_niostack)-1];
    msysnewc_outdev = msysnewc_outdev_stack[(msysnewc_niostack)-1];
    msysnewc_fmtstr = msysnewc_fmtstr_stack[(msysnewc_niostack)-1];
    msysnewc_needgap = (i64)(msysnewc_needgap_stack[(msysnewc_niostack)-1]);
    --msysnewc_niostack;
}

void msysnewc_m_print_ptr(void * a,byte * fmtstyle) {
    msysnewc_nextfmtchars((i64)0);
    msysnewc_printstr(msysnewc_strword((u64)(a),(byte*)"z8h"));
    msysnewc_needgap = (i64)1;
}

void msysnewc_m_print_i64(i64 a,byte * fmtstyle) {
    byte s[40];
    struct msysnewc_fmtrec fmt;
    i64 n;
    msysnewc_nextfmtchars((i64)0);
    if ((fmtstyle == 0)) {
        if ((a >= (i64)0)) {
            n = msysnewc_u64tostr((u64)(a),s,(u64)((i64)10),(i64)0);
        } else {
            s[((i64)1)-1] = '-';
            n = (msysnewc_u64tostr((u64)(-(a)),&s[((i64)2)-1],(u64)((i64)10),(i64)0) + (i64)1);
        };
        msysnewc_printstr_n(s,n);
    } else {
        msysnewc_strtofmt(fmtstyle,(i64)-1,&fmt);
        if (((u64)(fmt.param) == 'V')) {
            msysnewc_fmtparam = a;
            msysnewc_needgap = (i64)0;
        } else {
            msysnewc_tostr_i64(a,&fmt);
        };
    };
    msysnewc_needgap = (i64)1;
}

void msysnewc_m_print_u64(u64 a,byte * fmtstyle) {
    struct msysnewc_fmtrec fmt;
    msysnewc_nextfmtchars((i64)0);
    if ((fmtstyle == 0)) {
        msysnewc_printstr(msysnewc_strword(a,(byte *)(0)));
    } else {
        msysnewc_strtofmt(fmtstyle,(i64)-1,&fmt);
        msysnewc_tostr_u64(a,&fmt);
    };
    msysnewc_needgap = (i64)1;
}

void msysnewc_m_print_r64(double x,byte * fmtstyle) {
    byte s[360];
    struct msysnewc_fmtrec fmt;
    msysnewc_nextfmtchars((i64)0);
    if ((fmtstyle == 0)) {
        sprintf((i8 *)(s),(i8 *)((byte*)"%f"),x);
        msysnewc_printstr(s);
    } else {
        msysnewc_strtofmt(fmtstyle,(i64)-1,&fmt);
        msysnewc_tostr_r64(x,&fmt);
    };
    msysnewc_needgap = (i64)1;
}

void msysnewc_m_print_r32(float x,byte * fmtstyle) {
    msysnewc_m_print_r64((double)(x),fmtstyle);
}

void msysnewc_m_print_c8(i64 a,byte * fmtstyle) {
    byte s[40];
    msysnewc_nextfmtchars((i64)0);
    s[((i64)1)-1] = (u64)(a);
    s[((i64)2)-1] = (u64)0u;
    msysnewc_printstr(s);
    msysnewc_needgap = (i64)1;
}

void msysnewc_m_print_str(byte * s,byte * fmtstyle) {
    struct msysnewc_fmtrec fmt;
    msysnewc_nextfmtchars((i64)0);
    if ((fmtstyle == 0)) {
        msysnewc_printstr(s);
    } else {
        msysnewc_strtofmt(fmtstyle,(i64)-1,&fmt);
        msysnewc_tostr_str(s,&fmt);
    };
    msysnewc_needgap = (i64)1;
}

void msysnewc_m_print_newline(void) {
    msysnewc_needgap = (i64)0;
    msysnewc_nextfmtchars((i64)1);
    msysnewc_printstr((byte*)"\r\n");
}

void msysnewc_m_print_nogap(void) {
    msysnewc_needgap = (i64)0;
}

void msysnewc_printstr(byte * s) {
    byte * *  p;
    if ((msysnewc_outdev==(i64)1)) {
        printf((i8 *)((byte*)"%s"),s);
    }else if ((msysnewc_outdev==(i64)2)) {
        fprintf(msysnewc_outchan,(i8 *)((byte*)"%s"),s);
    }else if ((msysnewc_outdev==(i64)3)) {
        p = (byte * *)(msysnewc_outchan);
        strcpy((i8 *)((*p)),(i8 *)(s));
        (*p) += (i64)(strlen((i8 *)(s)));
    };
}

void msysnewc_printstr_n(byte * s,i64 n) {
    byte str[256];
    byte * *  p;
    if ((n==(i64)-1)) {
        n = (i64)(strlen((i8 *)(s)));
    }else if ((n==(i64)0)) {
        return;
    };
    if ((msysnewc_outdev==(i64)3)) {
        p = (byte * *)(msysnewc_outchan);
        memcpy((void *)((*p)),(void *)(s),(u64)(n));
        (*p) += n;
        (*(*p)) = (u64)0u;
    }else if ((msysnewc_outdev==(i64)2)) {
        s = msysnewc_makezstring(s,n,str);
        fprintf(msysnewc_outchan,(i8 *)((byte*)"%s"),s);
        msysnewc_freezstring(s,n);
    }else if ((msysnewc_outdev==(i64)1)) {
        s = msysnewc_makezstring(s,n,str);
        printf((i8 *)((byte*)"%s"),s);
        msysnewc_freezstring(s,n);
    };
}

void msysnewc_printstrn_app(byte * s,i64 length,void * f) {
    if (!!(length)) {
        if ((f == 0)) {
            printf("%.*s",(i32)length,s);;
        } else {
            fprintf(f,"%.*s",(i32)length,s);;
        };
    };
}

static byte * msysnewc_makezstring(byte * s,i64 n,byte * local) {
    byte *  t;
    if ((n < (i64)256)) {
        memcpy((void *)(local),(void *)(s),(u64)(n));
        (*(local + n)) = (u64)0u;
        return local;
    } else {
        t = (byte *)(mlib_pcm_alloc((n + (i64)1)));
        memcpy((void *)(t),(void *)(s),(u64)(n));
        (*(t + n)) = (u64)0u;
        return t;
    };
}

static void msysnewc_freezstring(byte * t,i64 n) {
    if ((n >= (i64)256)) {
        mlib_pcm_free((void *)(t),(n + (i64)1));
    };
}

static void msysnewc_printchar(i64 ch) {
    byte * *  p;
    if ((msysnewc_outdev==(i64)1)) {
        printf("%c",(int)ch);
    }else if ((msysnewc_outdev==(i64)2)) {
        fprintf(msysnewc_outchan,"%c",(int)ch);
    }else if ((msysnewc_outdev==(i64)3)) {
        p = (byte * *)(msysnewc_outchan);
        (*(*p)) = (u64)(ch);
        (*p) += (i64)1;
        (*(*p)) = (u64)0u;
    };
}

void msysnewc_nextfmtchars(i64 lastx) {
    byte c;
    byte *  pstart;
    i64 n;
    if (!(!!(msysnewc_fmtstr))) {
        if (!!(msysnewc_needgap)) {
            msysnewc_printchar((i64)32);
        };
        msysnewc_needgap = (i64)0;
        return;
    };
    pstart = msysnewc_fmtstr;
    n = (i64)0;
    L95 :;
    while (!!((i64)1)) {
        c = (u64)((*msysnewc_fmtstr));
        switch ((i64)(c)) {
        case 35:;
        {
            if (!!(lastx)) {
                goto L98 ;
;
            };
            ++msysnewc_fmtstr;
            if (!!(n)) {
                msysnewc_printstr_n(pstart,n);
            };
            return;
        }break;
        case 0:;
        {
            if (!!(n)) {
                msysnewc_printstr_n(pstart,n);
            } else if (!(!!(lastx))) {
                msysnewc_printstr_n((byte*)"|",(i64)1);
            };
            return;
        }break;
        case 126:;
        {
            if (!!(n)) {
                msysnewc_printstr_n(pstart,n);
                n = (i64)0;
            };
            ++msysnewc_fmtstr;
            c = (u64)((*msysnewc_fmtstr));
            if (!!((u64)(c))) {
                ++msysnewc_fmtstr;
                msysnewc_printchar((i64)(c));
            };
            pstart = msysnewc_fmtstr;
        }break;
        default: {
            //skip:
L98 :;
;
            ++n;
            ++msysnewc_fmtstr;
        }
        } //SW
;
L96 :;
    }L97 :;
    ;
}

void msysnewc_strtofmt(byte * s,i64 slen,struct msysnewc_fmtrec * fmt) {
    byte c;
    byte wset;
    i64 n;
    byte str[100];
    (*fmt) = msysnewc_defaultfmt;
    if ((s == 0)) {
        return;
    };
    if ((slen == (i64)-1)) {
        slen = (i64)(strlen((i8 *)(s)));
    };
    memcpy((void *)(str),(void *)(s),(u64)(slen));
    str[(slen)] = (u64)0u;
    s = str;
    wset = (u64)((i64)0);
    L99 :;
    while (!!((u64)((*s)))) {
        c = (u64)((*s));
        ++s;
        switch ((i64)(c)) {
        case 66:;
        case 98:;
        {
            (*fmt).base = (u64)((i64)2);
        }break;
        case 72:;
        case 104:;
        {
            (*fmt).base = (u64)((i64)16);
        }break;
        case 79:;
        case 111:;
        {
            (*fmt).base = (u64)((i64)8);
        }break;
        case 88:;
        case 120:;
        {
            c = (u64)((*s));
            if (!!((u64)(c))) {
                switch ((i64)(c)) {
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
                    c = ((u64)(c) - '0');
                }break;
                case 65:;
                case 66:;
                case 67:;
                case 68:;
                case 69:;
                case 70:;
                {
                    c = (u64)((((u64)(c) - 'A') + (i64)10));
                }break;
                case 97:;
                case 98:;
                case 99:;
                case 100:;
                case 101:;
                case 102:;
                {
                    c = (u64)((((u64)(c) - 'a') + (i64)10));
                }break;
                default: {
                    c = (u64)10u;
                }
                } //SW
;
                (*fmt).base = (u64)(c);
                ++s;
            };
        }break;
        case 81:;
        case 113:;
        {
            (*fmt).quotechar = '"';
        }break;
        case 126:;
        {
            (*fmt).quotechar = '~';
        }break;
        case 74:;
        case 106:;
        {
            (*fmt).justify = (u64)(toupper((i64)((i32)((*s)))));
            if (!!((u64)((*s)))) {
                ++s;
            };
        }break;
        case 65:;
        {
            (*fmt).lettercase = 'A';
        }break;
        case 97:;
        {
            (*fmt).lettercase = 'a';
        }break;
        case 90:;
        case 122:;
        {
            (*fmt).padchar = '0';
        }break;
        case 83:;
        case 115:;
        {
            (*fmt).sepchar = (u64)((*s));
            if (!!((u64)((*s)))) {
                ++s;
            };
        }break;
        case 80:;
        case 112:;
        {
            (*fmt).padchar = (u64)((*s));
            if (!!((u64)((*s)))) {
                ++s;
            };
        }break;
        case 84:;
        case 116:;
        {
            (*fmt).suffix = (u64)((*s));
            if (!!((u64)((*s)))) {
                ++s;
            };
        }break;
        case 87:;
        case 119:;
        {
            (*fmt).usigned = 'W';
        }break;
        case 69:;
        case 101:;
        {
            (*fmt).realfmt = 'e';
        }break;
        case 70:;
        case 102:;
        {
            (*fmt).realfmt = 'f';
        }break;
        case 71:;
        case 103:;
        {
            (*fmt).realfmt = 'g';
        }break;
        case 46:;
        {
            wset = (u64)((i64)1);
        }break;
        case 44:;
        case 95:;
        {
            (*fmt).sepchar = (u64)(c);
        }break;
        case 43:;
        {
            (*fmt).plus = '+';
        }break;
        case 68:;
        case 100:;
        {
            (*fmt).charmode = 'D';
        }break;
        case 67:;
        case 99:;
        {
            (*fmt).charmode = 'C';
        }break;
        case 77:;
        case 109:;
        {
            (*fmt).heapmode = 'M';
        }break;
        case 86:;
        case 118:;
        {
            (*fmt).param = 'V';
        }break;
        case 42:;
        {
            n = msysnewc_fmtparam;
            goto L102 ;
;
        }break;
        default: {
            if ((((u64)(c) >= '0') && ((u64)(c) <= '9'))) {
                n = ((u64)(c) - '0');
                L103 :;
                while (1) {
                    c = (u64)((*s));
                    if (((i64)((*s)) == (i64)0)) {
                        goto L104 ;
                    };
                    if ((((u64)(c) >= '0') && ((u64)(c) <= '9'))) {
                        ++s;
                        n = (((n * (i64)10) + (i64)(c)) - (i64)48);
                    } else {
                        goto L104 ;
                    };
                }L104 :;
                ;
                //gotwidth:
L102 :;
;
                if (!(!!((u64)(wset)))) {
                    (*fmt).minwidth = (u64)(n);
                    wset = (u64)((i64)1);
                } else {
                    (*fmt).precision = n;
                };
            };
        }
        } //SW
;
L100 :;
    }L101 :;
    ;
}

static i64 msysnewc_domultichar(byte * p,i64 n,byte * dest,struct msysnewc_fmtrec * fmt) {
    byte str[20];
    byte *  q;
    i64 nchars;
    i64 av_1;
    q = str;
    nchars = n;
    av_1 = n;
    while (av_1-- > 0) {
L105 :;
        if (((i64)((*p)) == (i64)0)) {
            goto L107 ;
        };
        (*q) = (u64)((*p));
        ++q;
        ++p;
L106 :;
    }L107 :;
    ;
    (*q) = (u64)0u;
    return msysnewc_expandstr(str,dest,(i64)(strlen((i8 *)(str))),fmt);
}

static i64 msysnewc_expandstr(byte * s,byte * t,i64 n,struct msysnewc_fmtrec * fmt) {
    i64 i;
    i64 w;
    i64 m;
    i64 av_1;
    i64 av_2;
    i64 av_3;
    i64 av_4;
    i64 av_5;
    w = (i64)((*fmt).minwidth);
    if (((w == (i64)0) || (w <= n))) {
        strncpy((i8 *)(t),(i8 *)(s),(u64)(n));
        (*(t + n)) = (u64)0u;
        return n;
    };
    if (((u64)((*fmt).justify) == 'L')) {
        strncpy((i8 *)(t),(i8 *)(s),(u64)(n));
        t += n;
        L108 :;
        for (i=(i64)1;i<=(w - n);i+=(i64)1) {
L109 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L110 :;
        }L111 :;
        ;
        (*t) = (u64)0u;
    } else if (((u64)((*fmt).justify) == 'R')) {
        if (((((u64)((*fmt).padchar) == '0') && !!((u64)((*fmt).base))) && (((u64)((*s)) == '-') || ((u64)((*s)) == '+')))) {
            (*t) = (u64)((*s));
            ++t;
            av_2 = (w - n);
            while (av_2-- > 0) {
L112 :;
                (*t) = (u64)((*fmt).padchar);
                ++t;
L113 :;
            }L114 :;
            ;
            strncpy((i8 *)(t),(i8 *)((s + (i64)1)),(u64)((n - (i64)1)));
            (*((t + n) - (i64)1)) = (u64)0u;
        } else {
            av_3 = (w - n);
            while (av_3-- > 0) {
L115 :;
                (*t) = (u64)((*fmt).padchar);
                ++t;
L116 :;
            }L117 :;
            ;
            strncpy((i8 *)(t),(i8 *)(s),(u64)(n));
            (*(t + n)) = (u64)0u;
        };
    } else {
        m = (((w - n) + (i64)1) / (i64)2);
        av_4 = m;
        while (av_4-- > 0) {
L118 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L119 :;
        }L120 :;
        ;
        strncpy((i8 *)(t),(i8 *)(s),(u64)(n));
        t += n;
        av_5 = ((w - n) - m);
        while (av_5-- > 0) {
L121 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L122 :;
        }L123 :;
        ;
        (*t) = (u64)0u;
    };
    return w;
}

static u64 msysnewc_xdivrem(u64 a,u64 b,u64 * remainder) {
    u64 q;
    mlib_abortprogram((byte*)"XDIVREM");
    return q;
}

static i64 msysnewc_u64tostr(u64 aa,byte * s,u64 base,i64 sep) {
    byte t[360];
    i64 i;
    i64 j;
    i64 k;
    i64 g;
    byte *  s0;
    i = (i64)0;
    k = (i64)0;
    g = (((i64)(base) == (i64)10)?(i64)3:(i64)4);
    L124 :;
    do {
        t[(++i)] = (u64)(msysnewc_digits[((i64)((aa % base)))]);
        aa = (aa / base);
        ++k;
        if (((!!(sep) && ((i64)(aa) != (i64)0)) && (k == g))) {
            t[(++i)] = (u64)(sep);
            k = (i64)0;
        };
L125 :;
    } while (!((i64)(aa) == (i64)0));L126 :;
    ;
    j = i;
    s0 = s;
    L127 :;
    while (!!(i)) {
        (*s) = (u64)(t[(i--)]);
        ++s;
L128 :;
    }L129 :;
    ;
    (*s) = (u64)0u;
    return j;
}

static i64 msysnewc_i64tostrfmt(i64 aa,byte * s,struct msysnewc_fmtrec * fmt) {
    byte str[360];
    i64 n;
    i64 usigned;
    static u64 mindint = (u64)9223372036854775808u;
    usigned = (i64)0;
    if (!!((u64)((*fmt).usigned))) {
        usigned = (i64)1;
    };
    if (((aa == (i64)(mindint)) && !(!!(usigned)))) {
        str[((i64)0)] = '-';
        n = (msysnewc_i64mintostr(&str[((i64)1)],(i64)((*fmt).base),(i64)((*fmt).sepchar)) + (i64)1);
    } else {
        if (((!(!!(usigned)) && (aa < (i64)0)) || !!((u64)((*fmt).plus)))) {
            if ((aa < (i64)0)) {
                aa = -(aa);
                str[((i64)0)] = '-';
            } else {
                str[((i64)0)] = '+';
            };
            n = (msysnewc_u64tostr((u64)(aa),&str[((i64)1)],(u64)((*fmt).base),(i64)((*fmt).sepchar)) + (i64)1);
        } else {
            n = msysnewc_u64tostr((u64)(aa),str,(u64)((*fmt).base),(i64)((*fmt).sepchar));
        };
    };
    if (!!((u64)((*fmt).suffix))) {
        str[(n)] = (u64)((*fmt).suffix);
        str[(++n)] = (u64)0u;
    };
    if (((((i64)((u64)((*fmt).base)) > (i64)10) || !!((u64)((*fmt).suffix))) && ((u64)((*fmt).lettercase) == 'a'))) {
        msysnewc_convlcstring(str);
    };
    return msysnewc_expandstr(str,s,n,fmt);
}

static i64 msysnewc_u64tostrfmt(i64 aa,byte * s,struct msysnewc_fmtrec * fmt) {
    byte str[360];
    i64 n;
    n = msysnewc_u64tostr((u64)(aa),str,(u64)((*fmt).base),(i64)((*fmt).sepchar));
    if (!!((u64)((*fmt).suffix))) {
        str[(n)] = (u64)((*fmt).suffix);
        str[(++n)] = (u64)0u;
    };
    if ((((i64)((u64)((*fmt).base)) > (i64)10) || (!!((u64)((*fmt).suffix)) && ((u64)((*fmt).lettercase) == 'a')))) {
        msysnewc_convlcstring(str);
    };
    return msysnewc_expandstr(str,s,n,fmt);
}

static i64 msysnewc_i64mintostr(byte * s,i64 base,i64 sep) {
    byte t[360];
    i64 i;
    i64 j;
    i64 k;
    i64 g;
    switch (base) {
    case 10:;
    {
        strcpy((i8 *)(&t[((i64)0)]),(i8 *)((byte*)"9223372036854775808"));
        j = (i64)3;
    }break;
    case 16:;
    {
        strcpy((i8 *)(&t[((i64)0)]),(i8 *)((byte*)"8000000000000000"));
        j = (i64)1;
    }break;
    case 2:;
    {
        strcpy((i8 *)(&t[((i64)0)]),(i8 *)((byte*)"1000000000000000000000000000000000000000000000000000000000000000"));
        j = (i64)7;
    }break;
    default: {
        strcpy((i8 *)(&t[((i64)0)]),(i8 *)((byte*)"<mindint>"));
    }
    } //SW
;
    i = (i64)(strlen((i8 *)(&t[((i64)0)])));
    s += i;
    if (!!(sep)) {
        s += j;
    };
    (*s) = (u64)0u;
    k = (i64)0;
    g = ((base == (i64)10)?(i64)3:(i64)4);
    L130 :;
    while (!!(i)) {
        --s;
        (*s) = (u64)(t[((i-- - (i64)1))]);
        if (((!!(sep) && !!(i)) && (++k == g))) {
            --s;
            (*s) = (u64)(sep);
            k = (i64)0;
        };
L131 :;
    }L132 :;
    ;
    return (i64)(strlen((i8 *)(s)));
}

static i64 msysnewc_strtostrfmt(byte * s,byte * t,i64 n,struct msysnewc_fmtrec * fmt) {
    byte *  u;
    byte *  v;
    byte str[256];
    i64 w;
    i64 nheap;
    nheap = (i64)0;
    if ((!!((u64)((*fmt).quotechar)) || !!((u64)((*fmt).lettercase)))) {
        if ((n < (i64)256)) {
            u = str;
        } else {
            nheap = (n + (i64)3);
            u = (byte *)(mlib_pcm_alloc(nheap));
        };
        if (!!((u64)((*fmt).quotechar))) {
            v = u;
            (*v) = (u64)((*fmt).quotechar);
            ++v;
            if (!!(n)) {
                strcpy((i8 *)(v),(i8 *)(s));
                v += n;
            };
            (*v) = (u64)((*fmt).quotechar);
            ++v;
            (*v) = (u64)0u;
            n += (i64)2;
        } else {
            memcpy((void *)(u),(void *)(s),(u64)(n));
        };
        switch ((i64)((*fmt).lettercase)) {
        case 97:;
        {
            msysnewc_convlcstring(u);
        }break;
        case 65:;
        {
            msysnewc_convucstring(u);
        }break;
        default: {
        }
        } //SW
;
        s = u;
    };
    w = (i64)((*fmt).minwidth);
    if ((w > n)) {
        n = msysnewc_expandstr(s,t,n,fmt);
    } else {
        memcpy((void *)(t),(void *)(s),(u64)(n));
    };
    if (!!(nheap)) {
        mlib_pcm_free((void *)(u),nheap);
    };
    return n;
}

static void msysnewc_tostr_i64(i64 a,struct msysnewc_fmtrec * fmt) {
    byte str[360];
    i64 n;
    if (((i64)((*fmt).charmode)==(i64)0)) {
        n = msysnewc_i64tostrfmt(a,str,fmt);
    }else if (((i64)((*fmt).charmode)==(i64)68) || ((i64)((*fmt).charmode)==(i64)100)) {
        n = msysnewc_domultichar((byte *)(&a),(i64)8,str,fmt);
    } else {
        msysnewc_printchar(a);
        return;
    };
    msysnewc_printstr_n(str,n);
}

static void msysnewc_tostr_u64(u64 a,struct msysnewc_fmtrec * fmt) {
    byte str[360];
    i64 n;
    if (((i64)((*fmt).charmode)==(i64)68) || ((i64)((*fmt).charmode)==(i64)100)) {
        n = msysnewc_domultichar((byte *)(&a),(i64)8,str,fmt);
    }else if (((i64)((*fmt).charmode)==(i64)67) || ((i64)((*fmt).charmode)==(i64)99)) {
        msysnewc_printchar((i64)(a));
        return;
    } else {
        n = msysnewc_u64tostrfmt((i64)(a),str,fmt);
    };
    msysnewc_printstr_n(str,n);
}

static void msysnewc_tostr_r64(double x,struct msysnewc_fmtrec * fmt) {
    byte str[360];
    byte str2[360];
    byte cfmt[10];
    i64 n;
    cfmt[((i64)0)] = '%';
    if (!!((i64)((*fmt).precision))) {
        cfmt[((i64)1)] = '.';
        cfmt[((i64)2)] = '*';
        cfmt[((i64)3)] = (u64)((*fmt).realfmt);
        cfmt[((i64)4)] = (u64)0u;
        sprintf((i8 *)(str),(i8 *)(cfmt),(i64)((*fmt).precision),x);
    } else {
        cfmt[((i64)1)] = (u64)((*fmt).realfmt);
        cfmt[((i64)2)] = (u64)0u;
        sprintf((i8 *)(str),(i8 *)(cfmt),x);
    };
    n = (i64)(strlen((i8 *)(str)));
    if ((n < (i64)((u64)((*fmt).minwidth)))) {
        n = msysnewc_expandstr(str,str2,n,fmt);
        strcpy((i8 *)(str),(i8 *)(str2));
    };
    msysnewc_printstr_n(str,n);
}

static void msysnewc_tostr_str(byte * s,struct msysnewc_fmtrec * fmt) {
    i64 oldlen;
    i64 newlen;
    i64 n;
    byte *  t;
    oldlen = (i64)(strlen((i8 *)(s)));
    newlen = oldlen;
    if (((!!((u64)((*fmt).quotechar)) || ((i64)((u64)((*fmt).minwidth)) > newlen)) || !!((u64)((*fmt).lettercase)))) {
        if (!!((u64)((*fmt).quotechar))) {
            newlen += (i64)2;
        };
        if (((i64)((u64)((*fmt).minwidth)) > newlen)) {
            newlen = (i64)((*fmt).minwidth);
        };
        t = (byte *)(mlib_pcm_alloc((newlen + (i64)1)));
        n = msysnewc_strtostrfmt(s,t,oldlen,fmt);
        msysnewc_printstr_n(t,n);
        mlib_pcm_free((void *)(t),(newlen + (i64)1));
    } else {
        msysnewc_printstr_n(s,oldlen);
    };
}

static struct msysnewc_fmtrec * msysnewc_getfmt(byte * fmtstyle) {
    static struct msysnewc_fmtrec fmt;
    if (!!(fmtstyle)) {
        msysnewc_strtofmt(fmtstyle,(i64)-1,&fmt);
        return &fmt;
    } else {
        return &msysnewc_defaultfmt;
    };
}

byte * msysnewc_strint(i64 a,byte * fmtstyle) {
    static byte str[100];
    struct msysnewc_fmtrec *  fmt;
    msysnewc_m_print_startstr(str);
    msysnewc_tostr_i64(a,(fmt = msysnewc_getfmt(fmtstyle)));
    msysnewc_m_print_end();
    return msysnewc_getstr(str,fmt);
}

void msysnewc_getstrint(i64 a,byte * dest) {
    msysnewc_m_print_startstr(dest);
    msysnewc_tostr_i64(a,msysnewc_getfmt((byte *)(0)));
    msysnewc_m_print_end();
}

byte * msysnewc_strword(u64 a,byte * fmtstyle) {
    static byte str[100];
    struct msysnewc_fmtrec *  fmt;
    msysnewc_m_print_startstr(str);
    msysnewc_tostr_u64(a,(fmt = msysnewc_getfmt(fmtstyle)));
    msysnewc_m_print_end();
    return msysnewc_getstr(str,fmt);
}

byte * msysnewc_strreal(double a,byte * fmtstyle) {
    static byte str[320];
    struct msysnewc_fmtrec *  fmt;
    msysnewc_m_print_startstr(str);
    msysnewc_tostr_r64(a,(fmt = msysnewc_getfmt(fmtstyle)));
    msysnewc_m_print_end();
    return msysnewc_getstr(str,fmt);
}

static byte * msysnewc_getstr(byte * s,struct msysnewc_fmtrec * fmt) {
    if (!!((u64)((*fmt).heapmode))) {
        return mlib_pcm_copyheapstring(s);
    } else {
        return s;
    };
}

static void msysnewc_initreadbuffer(void) {
    if (!!(msysnewc_rd_buffer)) {
        return;
    };
    msysnewc_rd_buffer = (byte *)(mlib_pcm_alloc((i64)16384));
    (*msysnewc_rd_buffer) = (u64)0u;
    msysnewc_rd_pos = (msysnewc_rd_lastpos = msysnewc_rd_buffer);
}

void msysnewc_m_read_conline(void) {
    msysnewc_initreadbuffer();
    mlib_readlinen(0,msysnewc_rd_buffer,(i64)16384);
    msysnewc_rd_length = (i64)(strlen((i8 *)(msysnewc_rd_buffer)));
    msysnewc_rd_pos = msysnewc_rd_buffer;
    msysnewc_rd_lastpos = (byte *)(0);
}

void msysnewc_m_read_fileline(void * f) {
    msysnewc_initreadbuffer();
    mlib_readlinen(f,msysnewc_rd_buffer,(i64)16384);
    msysnewc_rd_length = (i64)(strlen((i8 *)(msysnewc_rd_buffer)));
    msysnewc_rd_pos = msysnewc_rd_buffer;
    msysnewc_rd_lastpos = (byte *)(0);
}

void msysnewc_m_read_strline(byte * s) {
    i64 n;
    msysnewc_initreadbuffer();
    n = (i64)(strlen((i8 *)(s)));
    if ((n < (i64)16384)) {
        strcpy((i8 *)(msysnewc_rd_buffer),(i8 *)(s));
    } else {
        memcpy((void *)(msysnewc_rd_buffer),(void *)(s),(u64)((i64)16383));
        (*((msysnewc_rd_buffer + (i64)16384) - (i64)1)) = (u64)0u;
    };
    msysnewc_rd_length = n;
    msysnewc_rd_pos = msysnewc_rd_buffer;
    msysnewc_rd_lastpos = (byte *)(0);
}

static byte * msysnewc_readitem(i64 * itemlength) {
    byte *  p;
    byte *  s;
    byte *  itemstr;
    byte quotechar;
    byte c;
    if (!(!!(msysnewc_rd_buffer))) {
        msysnewc_initreadbuffer();
    };
    s = msysnewc_rd_pos;
    L133 :;
    while ((((u64)((*s)) == ' ') || ((i64)((*s)) == (i64)9))) {
        ++s;
L134 :;
    }L135 :;
    ;
    itemstr = s;
    msysnewc_rd_lastpos = (msysnewc_rd_pos = s);
    if (((i64)((*s)) == (i64)0)) {
        msysnewc_termchar = (i64)0;
        (*itemlength) = (i64)0;
        return s;
    };
    quotechar = (u64)0u;
    if (((u64)((*s)) == '"')) {
        quotechar = '"';
        ++s;
    } else if (((u64)((*s)) == (u64)39u)) {
        quotechar = (u64)39u;
        ++s;
    };
    p = (itemstr = s);
    L136 :;
    while (!!((u64)((*s)))) {
        c = (u64)((*s++));
        switch ((i64)(c)) {
        case 32:;
        case 9:;
        case 44:;
        case 61:;
        {
            if ((!!((u64)(quotechar)) || (p == s))) {
                goto L139 ;
;
            };
            msysnewc_termchar = (i64)(c);
            goto L138 ;
        }break;
        default: {
            //normalchar:
L139 :;
;
            if (((u64)(c) == (u64)(quotechar))) {
                if (((u64)((*s)) == (u64)(quotechar))) {
                    (*p) = (u64)(c);
                    ++s;
                    ++p;
                } else {
                    msysnewc_termchar = (i64)((*s));
                    if (((msysnewc_termchar == (i64)44) || (msysnewc_termchar == (i64)61))) {
                        ++s;
                        msysnewc_termchar = (i64)((*s));
                    };
                    goto L138 ;
                };
            } else {
                (*p) = (u64)(c);
                ++p;
            };
        }
        } //SW
;
L137 :;
    }L138 :;
    ;
    if (((i64)((*s)) == (i64)0)) {
        msysnewc_termchar = (i64)0;
    };
    (*itemlength) = (p - itemstr);
    msysnewc_rd_pos = s;
    return itemstr;
}

i64 msysnewc_strtoint(byte * s,i64 length,i64 base) {
    byte signd;
    u64 aa;
    byte c;
    byte d;
    msysnewc_itemerror = (i64)0;
    if ((length == (i64)-1)) {
        length = (i64)(strlen((i8 *)(s)));
    };
    signd = (u64)((i64)0);
    if ((!!(length) && ((u64)((*s)) == '-'))) {
        signd = (u64)((i64)1);
        ++s;
        --length;
    } else if ((!!(length) && ((u64)((*s)) == '+'))) {
        ++s;
        --length;
    };
    aa = (u64)((i64)0);
    L140 :;
    while (!!(length)) {
        c = (u64)((*s++));
        --length;
        switch ((i64)(c)) {
        case 65:;
        case 66:;
        case 67:;
        case 68:;
        case 69:;
        case 70:;
        {
            d = (u64)((((u64)(c) - 'A') + (i64)10));
        }break;
        case 97:;
        case 98:;
        case 99:;
        case 100:;
        case 101:;
        case 102:;
        {
            d = (u64)((((u64)(c) - 'a') + (i64)10));
        }break;
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
            d = ((u64)(c) - '0');
        }break;
        case 95:;
        case 39:;
        {
            goto L141 ;
        }break;
        default: {
            msysnewc_itemerror = (i64)1;
            goto L142 ;
        }
        } //SW
;
        if (((i64)(d) >= base)) {
            msysnewc_itemerror = (i64)1;
            goto L142 ;
        };
        aa = (u64)((((i64)(aa) * base) + (i64)(d)));
L141 :;
    }L142 :;
    ;
    if (!!((u64)(signd))) {
        return (i64)(-(aa));
    } else {
        return (i64)(aa);
    };
}

i64 msysnewc_m_read_i64(i64 fmt) {
    byte *  s;
    i64 length;
    if ((fmt==(i64)67) || (fmt==(i64)99)) {
        msysnewc_rd_lastpos = msysnewc_rd_pos;
        if (!!((u64)((*msysnewc_rd_pos)))) {
            return (i64)((*msysnewc_rd_pos++));
        } else {
            return (i64)0;
        };
    }else if ((fmt==(i64)84) || (fmt==(i64)116)) {
        return msysnewc_termchar;
    }else if ((fmt==(i64)69) || (fmt==(i64)101)) {
        return msysnewc_itemerror;
    };
    s = msysnewc_readitem(&length);
    if ((fmt==(i64)0) || (fmt==(i64)73) || (fmt==(i64)105)) {
        return msysnewc_strtoint(s,length,(i64)10);
    }else if ((fmt==(i64)66) || (fmt==(i64)98)) {
        return msysnewc_strtoint(s,length,(i64)2);
    }else if ((fmt==(i64)72) || (fmt==(i64)104)) {
        return msysnewc_strtoint(s,length,(i64)16);
    };
    return (i64)0;
}

double msysnewc_m_read_r64(i64 fmt) {
    byte str[512];
    byte *  s;
    i64 length;
    i32 numlength;
    double x;
    s = msysnewc_readitem(&length);
    if (((length == (i64)0) || (length >= (i64)512))) {
        return (double)0.;
    };
    memcpy((void *)(str),(void *)(s),(u64)(length));
    str[((length + (i64)1))-1] = (u64)0u;
    msysnewc_itemerror = (i64)0;
    if ((((i64)(sscanf((i8 *)(str),(i8 *)((byte*)"%lf%n"),&x,&numlength)) == (i64)0) || ((i64)(numlength) != length))) {
        x = (double)0.;
        msysnewc_itemerror = (i64)1;
    };
    return x;
}

void msysnewc_m_read_str(byte * dest,i64 destlen,i64 fmt) {
    byte *  s;
    i64 length;
    msysnewc_itemerror = (i64)0;
    if (((fmt == (i64)76) || (fmt == (i64)108))) {
        s = msysnewc_rd_pos;
        length = ((msysnewc_rd_buffer + msysnewc_rd_length) - msysnewc_rd_pos);
    } else {
        s = msysnewc_readitem(&length);
        if (((fmt == (i64)78) || (fmt == (i64)110))) {
            msysnewc_iconvlcn(s,length);
        };
    };
    if ((destlen > (i64)0)) {
        if ((length >= destlen)) {
            length = (destlen - (i64)1);
            msysnewc_itemerror = (i64)1;
        };
    };
    memcpy((void *)(dest),(void *)(s),(u64)(length));
    (*(dest + length)) = (u64)0u;
}

void msysnewc_readstr(byte * dest,i64 fmt,i64 destlen) {
    msysnewc_m_read_str(dest,destlen,fmt);
}

void msysnewc_rereadln(void) {
    msysnewc_rd_pos = msysnewc_rd_buffer;
    msysnewc_rd_lastpos = msysnewc_rd_pos;
}

void msysnewc_reread(void) {
    msysnewc_rd_pos = msysnewc_rd_lastpos;
}

i64 msysnewc_valint(byte * s,i64 fmt) {
    byte *  old_pos;
    byte *  old_lastpos;
    i64 aa;
    msysnewc_initreadbuffer();
    old_pos = msysnewc_rd_pos;
    old_lastpos = msysnewc_rd_lastpos;
    msysnewc_rd_pos = s;
    aa = msysnewc_m_read_i64(fmt);
    msysnewc_rd_pos = old_pos;
    msysnewc_rd_lastpos = old_lastpos;
    return aa;
}

double msysnewc_valreal(byte * s) {
    byte *  old_pos;
    byte *  old_lastpos;
    double x;
    msysnewc_initreadbuffer();
    old_pos = msysnewc_rd_pos;
    old_lastpos = msysnewc_rd_lastpos;
    msysnewc_rd_pos = s;
    x = msysnewc_m_read_r64((i64)0);
    msysnewc_rd_pos = old_pos;
    msysnewc_rd_lastpos = old_lastpos;
    return x;
}

static void msysnewc_iconvlcn(byte * s,i64 n) {
    i64 av_1;
    av_1 = n;
    while (av_1-- > 0) {
L143 :;
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L144 :;
    }L145 :;
    ;
}

static void msysnewc_iconvucn(byte * s,i64 n) {
    i64 av_1;
    av_1 = n;
    while (av_1-- > 0) {
L146 :;
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L147 :;
    }L148 :;
    ;
}

static void msysnewc_convlcstring(byte * s) {
    L149 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L150 :;
    }L151 :;
    ;
}

static void msysnewc_convucstring(byte * s) {
    L152 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L153 :;
    }L154 :;
    ;
}

i64 msysnewc_m_power_i64(i64 n,i64 a) {
    if ((n < (i64)0)) {
        return (i64)0;
    } else if ((n == (i64)0)) {
        return (i64)1;
    } else if ((n == (i64)1)) {
        return a;
    } else if (((n & (i64)1) == (i64)0)) {
        return msysnewc_m_power_i64((n / (i64)2),(a * a));
    } else {
        return (msysnewc_m_power_i64(((n - (i64)1) / (i64)2),(a * a)) * a);
    };
}

void msysnewc_m_intoverflow(void) {
    mlib_abortprogram((byte*)"Integer overflow detected");
}

void msysnewc_m_dotindex(u64 i,u64 a) {
    mlib_abortprogram((byte*)"DOT INDEX");
}

void msysnewc_m_dotslice(u64 j,u64 i,u64 a) {
    mlib_abortprogram((byte*)"DOT SLICE");
}

void msysnewc_m_popdotindex(u64 i,u64 * p,u64 x) {
    mlib_abortprogram((byte*)"POP DOT INDEX");
}

void msysnewc_m_popdotslice(u64 j,u64 i,u64 * p,u64 x) {
    mlib_abortprogram((byte*)"POP DOT SLICE");
}

i64 msysnewc_m_imin(i64 a,i64 b) {
    return (a<b?a:b);
}

i64 msysnewc_m_imax(i64 a,i64 b) {
    return (a>b?a:b);
}

double msysnewc_m_sign(double x) {
    if ((x > (double)0.)) {
        return (double)1.;
    } else if ((x < (double)0.)) {
        return (double)-1.;
    } else {
        return (double)0.;
    };
}

void oslinux_os_init(void) {
    oslinux_init_flag = (i64)1;
}

i64 oslinux_os_execwait(byte * cmdline,i64 newconsole,byte * workdir) {
    return (i64)(system((i8 *)(cmdline)));
}

i64 oslinux_os_execcmd(byte * cmdline,i64 newconsole) {
    return (i64)(system((i8 *)(cmdline)));
}

i64 oslinux_os_getch(void) {
    struct oslinux_termios old;
    struct oslinux_termios new;
    byte ch;
    tcgetattr((i64)0,&old);
    new = old;
    new.c_lflag &= (i32)-3;
    new.c_lflag &= (i32)-9;
    tcsetattr((i64)0,(i64)0,&new);
    ch = (u64)(getchar());
    tcsetattr((i64)0,(i64)0,&old);
    return (i64)(ch);
}

i64 oslinux_os_kbhit(void) {
    mlib_abortprogram((byte*)"kbhit");
    return (i64)0;
}

void oslinux_os_flushkeys(void) {
    mlib_abortprogram((byte*)"flushkeys");
}

void * oslinux_os_getconsolein(void) {
    return 0;
}

void * oslinux_os_getconsoleout(void) {
    return 0;
}

void * oslinux_os_proginstance(void) {
    mlib_abortprogram((byte*)"PROGINST");
    return 0;
}

u64 oslinux_os_getdllinst(byte * name) {
    void *  h;
    h = dlopen(name,(i64)1);
    if ((h == 0)) {
        if (((i64)(strcmp((i8 *)(name),(i8 *)((byte*)"msvcrt"))) == (i64)0)) {
            h = dlopen((byte*)"libc.so.6",(i64)1);
        };
    };
    return (u64)(h);
}

void * oslinux_os_getdllprocaddr(i64 hlib,byte * name) {
    void *  fnaddr;
    if ((hlib == (i64)0)) {
        return 0;
    };
    fnaddr = dlsym((void *)(hlib),name);
    return fnaddr;
}

void oslinux_os_initwindows(void) {
}

i64 oslinux_os_getchx(void) {
    mlib_abortprogram((byte*)"getchx");
    return (i64)0;
}

byte * oslinux_os_getos(void) {
    if (((i64)64 == (i64)32)) {
        return (byte*)"L32";
    } else {
        return (byte*)"L64";
    };
}

i64 oslinux_os_gethostsize(void) {
    return (i64)64;
}

i64 oslinux_os_iswindows(void) {
    return (i64)0;
}

i64 oslinux_os_shellexec(byte * opc,byte * file) {
    mlib_abortprogram((byte*)"SHELL EXEC");
    return (i64)0;
}

void oslinux_os_sleep(i64 a) {
    sleep((u64)((u32)(a)));
}

void * oslinux_os_getstdin(void) {
    return 0;
}

void * oslinux_os_getstdout(void) {
    return 0;
}

byte * oslinux_os_gethostname(void) {
    return (byte*)"";
}

byte * oslinux_os_getmpath(void) {
    return (byte*)"";
}

void oslinux_os_exitprocess(i64 x) {
    exit(0);
}

i64 oslinux_os_clock(void) {
    if (!!(oslinux_os_iswindows())) {
        return (i64)(clock());
    } else {
        return ((i64)(clock()) / (i64)1000);
    };
}

i64 oslinux_os_getclockspersec(void) {
    return (!!(oslinux_os_iswindows())?(i64)1000:(i64)1000000);
}

void oslinux_os_setmesshandler(void * addr) {
    mlib_abortprogram((byte*)"SETMESSHANDLER");
}

i64 oslinux_os_hpcounter(void) {
    return (i64)1;
}

i64 oslinux_os_hpfrequency(void) {
    return (i64)1;
}

i64 oslinux_os_filelastwritetime(byte * filename) {
    return (i64)0;
}

void oslinux_os_getsystime(struct oslinux_rsystemtime * tm) {
    memset((void *)(tm),(i64)0,(u64)((i64)36));
    (*tm).month = (i64)1;
}

void oslinux_os_peek(void) {
}


/* ********** End of C Code ********** */
