/*
  M to C  Whole Program Translator
  Input:  clex.m plus imported modules
  Output: \bench\c\clex.c (this file, or renamed from that)
          File represents entire program
  Target: C 64-bit
  OS:     Windows

  Modules:
  Module 1: clex.m
  Module 2: <Built-in: clibnewc.m>
  Module 3: <Built-in: mlib.m>
  Module 4: <Built-in: msysnewc.m>
  Module 5: <Built-in: oswindows.m>

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
struct oswindows_rsystemtime;
struct oswindows_input_record;
struct oswindows_rspoint;
struct oswindows_rsrect;
struct oswindows_rpoint;
struct oswindows_rconsole;
struct oswindows_rstartupinfo;
struct oswindows_rprocess_information;
struct oswindows_rwndclassex;
struct oswindows_rmsg;

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

struct oswindows_rsystemtime {
    u16 year;
    u16 month;
    u16 dayofweek;
    u16 day;
    u16 hour;
    u16 minute;
    u16 second;
    u16 milliseconds;
};

struct oswindows_input_record {
    u16 eventtype;
    u16 padding;
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

struct oswindows_rspoint {
    i16 x;
    i16 y;
};

struct oswindows_rsrect {
    i16 leftx;
    i16 top;
    i16 rightx;
    i16 bottom;
};

struct oswindows_rpoint {
    i32 x;
    i32 y;
};

struct oswindows_rconsole {
    struct oswindows_rspoint size;
    struct oswindows_rspoint pos;
    u16 attributes;
    struct oswindows_rsrect window;
    struct oswindows_rspoint maxwindowsize;
};

struct oswindows_rstartupinfo {
    u32 size;
    u32 dummy1;
    byte *  reserved;
    byte *  desktop;
    byte *  title;
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

struct oswindows_rprocess_information {
    void *  process;
    void *  thread;
    u32 processid;
    u32 threadid;
};

struct oswindows_rwndclassex {
    u32 size;
    u32 style;
    void (*wndproc)(void);
    i32 clsextra;
    i32 wndextra;
    void *  instance;
    void *  icon;
    void *  cursor;
    void *  background;
    byte *  menuname;
    byte *  classname;
    void *  iconsm;
};

struct oswindows_rmsg {
    void *  hwnd;
    u32 message;
    u32 dummy1;
    u64 wparam;
    u64 lparam;
    u32 time;
    u32 dummy2;
    struct oswindows_rpoint pt;
};


/* PROCDECLS */
void start(void);
static void clex_readtoken(void);
static void clex_printsymbol(void);
static void clex_readname(i64 c);
static void clex_readdecdigits(i64 c);
static void clex_readhexdigits(void);
static void clex_readbindigits(void);
static void clex_readstring(i64 termchar);
static void clex_readlinecomment(void);
static void clex_readblockcomment(void);
static void clex_readdecimal(byte * p,i64 length);
static void clex_readhex(byte * p,i64 length);
static void clex_readbin(byte * p,i64 length);
static double clex_readreal(byte * istr,i64 ilength,byte * fstr,i64 flength,i64 expon);
static void clex_printstr(byte * s,i64 length);
static void clex_initdata(void);
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
extern void * GetStdHandle(u32 _1);
extern u32 GetConsoleScreenBufferInfo(void * _1,void * _2);
extern u32 SetConsoleCtrlHandler(void (*_1)(void),u32 _2);
extern u32 SetConsoleMode(void * _1,u32 _2);
extern u32 CreateProcessA(byte * _1,byte * _2,void * _3,void * _4,u32 _5,u32 _6,void * _7,byte * _8,void * _9,void * _10);
extern u32 GetLastError(void);
extern u32 WaitForSingleObject(void * _1,u32 _2);
extern u32 GetExitCodeProcess(void * _1,void * _2);
extern u32 CloseHandle(void * _1);
extern u32 GetNumberOfConsoleInputEvents(void * _1,void * _2);
extern u32 FlushConsoleInputBuffer(void * _1);
extern void * LoadLibraryA(byte * _1);
extern void * GetProcAddress(void * _1,byte * _2);
extern void * LoadCursorA(void * _1,byte * _2);
extern u32 RegisterClassExA(void * _1);
extern u32 DefWindowProcA(void * _1,u32 _2,u64 _3,u64 _4);
extern u32 ReadConsoleInputA(void * _1,void * _2,u32 _3,void * _4);
extern void Sleep(u32 _1);
extern u32 GetModuleFileNameA(void * _1,byte * _2,u32 _3);
extern void ExitProcess(u32 _1);
extern void PostQuitMessage(i32 _1);
extern void MessageBoxA(i32 x,byte * message,byte * caption,i32 y);
extern u32 QueryPerformanceCounter(i64 * _1);
extern u32 QueryPerformanceFrequency(i64 * _1);
extern void * CreateFileA(byte * _1,u32 _2,u32 _3,void * _4,u32 _5,u32 _6,void * _7);
extern u32 GetFileTime(void * _1,void * _2,void * _3,void * _4);
extern void GetSystemTime(struct oswindows_rsystemtime * _1);
extern void GetLocalTime(struct oswindows_rsystemtime * _1);
extern u32 GetTickCount(void);
extern u32 PeekMessageA(void * _1,void * * _2,u32 _3,u32 _4,u32 _5);
void oswindows_os_init(void);
i64 oswindows_os_execwait(byte * cmdline,i64 newconsole,byte * workdir);
i64 oswindows_os_execcmd(byte * cmdline,i64 newconsole);
i64 oswindows_os_getch(void);
i64 oswindows_os_kbhit(void);
void oswindows_os_flushkeys(void);
void * oswindows_os_getconsolein(void);
void * oswindows_os_getconsoleout(void);
void * oswindows_os_proginstance(void);
u64 oswindows_os_getdllinst(byte * name);
void * oswindows_os_getdllprocaddr(i64 hinst,byte * name);
void oswindows_os_initwindows(void);
void oswindows_os_gxregisterclass(byte * classname);
i64 oswindows_mainwndproc(void * hwnd,u32 message,u64 wparam,u64 lparam);
static void oswindows_timerproc(void * hwnd,i64 msg,i64 id,i64 time);
void oswindows_os_setmesshandler(void * addr);
i64 oswindows_os_getchx(void);
byte * oswindows_os_getos(void);
i64 oswindows_os_gethostsize(void);
i64 oswindows_os_shellexec(byte * opc,byte * file);
void oswindows_os_sleep(i64 a);
void * oswindows_os_getstdin(void);
void * oswindows_os_getstdout(void);
byte * oswindows_os_gethostname(void);
byte * oswindows_os_getmpath(void);
void oswindows_os_exitprocess(i64 x);
i64 oswindows_os_clock(void);
i64 oswindows_os_getclockspersec(void);
i64 oswindows_os_iswindows(void);
i64 oswindows_os_filelastwritetime(byte * filename);
void oswindows_os_getsystime(struct oswindows_rsystemtime * tm);
void oswindows_os_messagebox(byte * s,byte * t);
i64 oswindows_os_hpcounter(void);
i64 oswindows_os_hpfrequency(void);
void oswindows_os_peek(void);

/* VARS */
byte *  clex_symbolnames[30] = {
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
byte *  clex_jtagnames[35] = {
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
static byte *  clex_lxsptr;
static i64 clex_lxlineno;
static i64 clex_lxfileno;
static i64 clex_lxsymbol;
static i64 clex_lxsubcode;
static i64 clex_lxvalue;
static double clex_lxvaluex;
static byte *  clex_lxvaluestr;
static i64 clex_lxlength;
static byte clex_alphamap[256];
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
    &start,
    &clex_readtoken,
    &clex_printsymbol,
    &clex_readname,
    &clex_readdecdigits,
    &clex_readhexdigits,
    &clex_readbindigits,
    &clex_readstring,
    &clex_readlinecomment,
    &clex_readblockcomment,
    &clex_readdecimal,
    &clex_readhex,
    &clex_readbin,
    &clex_readreal,
    &clex_printstr,
    &clex_initdata,
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
    &oswindows_os_init,
    &oswindows_os_execwait,
    &oswindows_os_execcmd,
    &oswindows_os_getch,
    &oswindows_os_kbhit,
    &oswindows_os_flushkeys,
    &oswindows_os_getconsolein,
    &oswindows_os_getconsoleout,
    &oswindows_os_proginstance,
    &oswindows_os_getdllinst,
    &oswindows_os_getdllprocaddr,
    &oswindows_os_initwindows,
    &oswindows_os_gxregisterclass,
    &oswindows_mainwndproc,
    &oswindows_timerproc,
    &oswindows_os_setmesshandler,
    &oswindows_os_getchx,
    &oswindows_os_getos,
    &oswindows_os_gethostsize,
    &oswindows_os_shellexec,
    &oswindows_os_sleep,
    &oswindows_os_getstdin,
    &oswindows_os_getstdout,
    &oswindows_os_gethostname,
    &oswindows_os_getmpath,
    &oswindows_os_exitprocess,
    &oswindows_os_clock,
    &oswindows_os_getclockspersec,
    &oswindows_os_iswindows,
    &oswindows_os_filelastwritetime,
    &oswindows_os_getsystime,
    &oswindows_os_messagebox,
    &oswindows_os_hpcounter,
    &oswindows_os_hpfrequency,
    &oswindows_os_peek,
0};
static byte *  msysnewc__fnnames[]= {
    (byte*)"start",
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
    (byte*)"os_gxregisterclass",
    (byte*)"mainwndproc",
    (byte*)"timerproc",
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
    (byte*)"os_exitprocess",
    (byte*)"os_clock",
    (byte*)"os_getclockspersec",
    (byte*)"os_iswindows",
    (byte*)"os_filelastwritetime",
    (byte*)"os_getsystime",
    (byte*)"os_messagebox",
    (byte*)"os_hpcounter",
    (byte*)"os_hpfrequency",
    (byte*)"os_peek",
(byte*)""};
static struct msysnewc_procinforec msysnewc__fnexports[]= {
	{0, 0,0, {0,0,0, 0,0,0, 0,0,0, 0,0,0}}}
;
static i64 msysnewc__fnnprocs=216;
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
static void *  oswindows_hconsole;
static void *  oswindows_hconsolein;
static struct oswindows_input_record oswindows_lastkey;
static struct oswindows_input_record oswindows_pendkey;
static i64 oswindows_keypending;
static i64 (*oswindows_wndproc_callbackfn)(void *) = 0;
static i64 oswindows_init_flag = (i64)0;

/* PROCDEFS */
// START
void start(void) {
    byte *  psource;
    i64 ntokens;
    i64 nlines;
    i64 nchars;
    i64 nn;
    i64 t;
    double tsecs;
    byte *  infile;
    i64 av_1;
    if ((msysnewc_nsysparams >= (i64)2)) {
        infile = msysnewc_sysparams[((i64)2)-1];
    } else {
        exit(0);
    };
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"Scanning:",NULL);
    msysnewc_m_print_str(infile,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    clex_initdata();
    psource = (byte *)(mlib_readfile(infile));
    t = oswindows_os_clock();
    if ((psource == 0)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"Can't open",NULL);
        msysnewc_m_print_str(infile,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        exit(0);
    };
    nchars = (nlines = (ntokens = (i64)0));
    nn = (!!((i64)0)?(i64)1:(i64)100);
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"CL4",NULL);
    msysnewc_m_print_i64(nn,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    av_1 = nn;
    while (av_1-- > 0) {
L1 :;
        clex_lxsptr = psource;
        clex_lxfileno = (clex_lxlineno = (i64)1);
        L4 :;
        do {
            clex_readtoken();
            ++ntokens;
            if (!!((i64)0)) {
                msysnewc_m_print_startcon();
                msysnewc_m_print_i64(ntokens,NULL);
                msysnewc_m_print_nogap();
                msysnewc_m_print_str((byte*)":",NULL);
                msysnewc_m_print_end();
                ;
                clex_printsymbol();
            };
L5 :;
        } while (!(clex_lxsymbol == (i64)20));L6 :;
        ;
        nlines += clex_lxlineno;
        nchars += mlib_rfsize;
L2 :;
    }L3 :;
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"Finished",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    t = (oswindows_os_clock() - t);
    tsecs = ((double)(t) / (double)1000.);
    msysnewc_m_print_startcon();
    msysnewc_m_print_r64(tsecs,NULL);
    msysnewc_m_print_str((byte*)"Seconds",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"Source file=",NULL);
    msysnewc_m_print_str(infile,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_i64(ntokens,NULL);
    msysnewc_m_print_str((byte*)"Tokens,",NULL);
    msysnewc_m_print_i64(nlines,NULL);
    msysnewc_m_print_str((byte*)"Lines,",NULL);
    msysnewc_m_print_i64(nchars,NULL);
    msysnewc_m_print_str((byte*)"Chars",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_r64(((double)(ntokens) / tsecs),NULL);
    msysnewc_m_print_str((byte*)"Tokens per second",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_r64(((double)(nlines) / tsecs),NULL);
    msysnewc_m_print_str((byte*)"Lines per second",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_r64(((double)(nchars) / tsecs),NULL);
    msysnewc_m_print_str((byte*)"Chars per second",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_r64(tsecs,NULL);
    msysnewc_m_print_str((byte*)" Seconds",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
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

static void clex_readtoken(void) {
    byte c;
    clex_lxsubcode = (i64)0;
    L7 :;
    while (1) {
        c = (u64)((*clex_lxsptr++));
        switch ((i64)(c)) {
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
            clex_lxvaluestr = (clex_lxsptr - (i64)1);
            L9 :;
            while (!!((u64)(clex_alphamap[((i64)((*clex_lxsptr++)))]))) {
L10 :;
            }L11 :;
            ;
            clex_lxsymbol = (i64)23;
            clex_lxlength = (clex_lxsptr - clex_lxvaluestr);
            return;
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
            if (((i64)((*clex_lxsptr))==(i64)120) || ((i64)((*clex_lxsptr))==(i64)88)) {
                clex_readhexdigits();
            }else if (((i64)((*clex_lxsptr))==(i64)98) || ((i64)((*clex_lxsptr))==(i64)66)) {
                clex_readbindigits();
            } else {
                clex_readdecdigits((i64)(c));
            };
            return;
        }break;
        case 32:;
        case 9:;
        {
        }break;
        case 13:;
        {
            if (((i64)((*clex_lxsptr)) == (i64)10)) {
                ++clex_lxsptr;
            };
            ++clex_lxlineno;
            clex_lxsymbol = (i64)19;
            return;
        }break;
        case 10:;
        {
            ++clex_lxlineno;
            clex_lxsymbol = (i64)19;
            return;
        }break;
        case 26:;
        case 0:;
        {
            clex_lxsymbol = (i64)20;
            return;
        }break;
        case 39:;
        {
            clex_readstring((i64)(c));
            return;
        }break;
        case 34:;
        {
            clex_readstring((i64)(c));
            return;
        }break;
        case 35:;
        {
            clex_lxsymbol = (i64)21;
            return;
        }break;
        case 92:;
        {
        }break;
        case 43:;
        {
            if (((i64)((*clex_lxsptr))==(i64)43)) {
                ++clex_lxsptr;
                clex_lxsymbol = (i64)22;
                clex_lxsubcode = (i64)23;
            }else if (((i64)((*clex_lxsptr))==(i64)61)) {
                ++clex_lxsptr;
                clex_lxsymbol = (i64)18;
                clex_lxsubcode = (i64)25;
            } else {
                clex_lxsymbol = (i64)17;
                clex_lxsubcode = (i64)7;
            };
            return;
        }break;
        case 45:;
        {
            if (((i64)((*clex_lxsptr))==(i64)45)) {
                ++clex_lxsptr;
                clex_lxsymbol = (i64)22;
                clex_lxsubcode = (i64)24;
            }else if (((i64)((*clex_lxsptr))==(i64)61)) {
                ++clex_lxsptr;
                clex_lxsymbol = (i64)18;
                clex_lxsubcode = (i64)26;
            }else if (((i64)((*clex_lxsptr))==(i64)62)) {
                clex_lxsymbol = (i64)15;
            } else {
                clex_lxsymbol = (i64)17;
                clex_lxsubcode = (i64)8;
            };
            return;
        }break;
        case 42:;
        {
            if (((u64)((*clex_lxsptr)) == '=')) {
                ++clex_lxsptr;
                clex_lxsymbol = (i64)18;
                clex_lxsubcode = (i64)27;
            } else {
                clex_lxsymbol = (i64)17;
                clex_lxsubcode = (i64)9;
            };
            return;
        }break;
        case 47:;
        {
            if (((i64)((*clex_lxsptr))==(i64)47)) {
                clex_readlinecomment();
            }else if (((i64)((*clex_lxsptr))==(i64)42)) {
                clex_readblockcomment();
            }else if (((i64)((*clex_lxsptr))==(i64)61)) {
                clex_lxsymbol = (i64)18;
                clex_lxsubcode = (i64)28;
                return;
            } else {
                clex_lxsymbol = (i64)17;
                clex_lxsubcode = (i64)10;
                return;
            };
        }break;
        case 37:;
        {
            if (((u64)((*clex_lxsptr)) == '=')) {
                ++clex_lxsptr;
                clex_lxsymbol = (i64)18;
                clex_lxsubcode = (i64)29;
            } else {
                clex_lxsymbol = (i64)17;
                clex_lxsubcode = (i64)11;
            };
            return;
        }break;
        case 40:;
        {
            clex_lxsymbol = (i64)8;
            return;
        }break;
        case 41:;
        {
            clex_lxsymbol = (i64)9;
            return;
        }break;
        case 123:;
        {
            clex_lxsymbol = (i64)12;
            return;
        }break;
        case 125:;
        {
            clex_lxsymbol = (i64)13;
            return;
        }break;
        case 60:;
        {
            clex_lxsymbol = (i64)17;
            if (((i64)((*clex_lxsptr))==(i64)60)) {
                ++clex_lxsptr;
                if (((u64)((*clex_lxsptr)) == '=')) {
                    ++clex_lxsptr;
                    clex_lxsymbol = (i64)18;
                    clex_lxsubcode = (i64)33;
                } else {
                    clex_lxsubcode = (i64)15;
                };
            }else if (((i64)((*clex_lxsptr))==(i64)61)) {
                ++clex_lxsptr;
                clex_lxsubcode = (i64)4;
            } else {
                clex_lxsubcode = (i64)3;
            };
            return;
        }break;
        case 62:;
        {
            clex_lxsymbol = (i64)17;
            if (((i64)((*clex_lxsptr))==(i64)62)) {
                ++clex_lxsptr;
                if (((u64)((*clex_lxsptr)) == '=')) {
                    ++clex_lxsptr;
                    clex_lxsymbol = (i64)18;
                    clex_lxsubcode = (i64)34;
                } else {
                    clex_lxsubcode = (i64)15;
                };
            }else if (((i64)((*clex_lxsptr))==(i64)61)) {
                ++clex_lxsptr;
                clex_lxsubcode = (i64)6;
            } else {
                clex_lxsubcode = (i64)5;
            };
            return;
        }break;
        case 91:;
        {
            clex_lxsymbol = (i64)10;
            return;
        }break;
        case 93:;
        {
            clex_lxsymbol = (i64)11;
            return;
        }break;
        case 46:;
        {
            switch ((i64)((*clex_lxsptr))) {
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
                clex_readdecdigits((i64)((*clex_lxsptr)));
            }break;
            default: {
                clex_lxsymbol = (i64)2;
            }
            } //SW
;
            return;
        }break;
        case 44:;
        {
            clex_lxsymbol = (i64)3;
            return;
        }break;
        case 58:;
        {
            clex_lxsymbol = (i64)6;
            return;
        }break;
        case 59:;
        {
            clex_lxsymbol = (i64)4;
            return;
        }break;
        case 94:;
        {
            if (((u64)((*clex_lxsptr)) == '=')) {
                ++clex_lxsptr;
                clex_lxsymbol = (i64)18;
                clex_lxsubcode = (i64)32;
            } else {
                clex_lxsymbol = (i64)17;
                clex_lxsubcode = (i64)14;
            };
        }break;
        case 124:;
        {
            if (((i64)((*clex_lxsptr))==(i64)124)) {
                ++clex_lxsptr;
                clex_lxsymbol = (i64)17;
                clex_lxsubcode = (i64)18;
            }else if (((i64)((*clex_lxsptr))==(i64)61)) {
                clex_lxsymbol = (i64)18;
                clex_lxsubcode = (i64)31;
            } else {
                clex_lxsymbol = (i64)17;
                clex_lxsubcode = (i64)13;
            };
            return;
        }break;
        case 38:;
        {
            if (((i64)((*clex_lxsptr))==(i64)38)) {
                ++clex_lxsptr;
                clex_lxsymbol = (i64)17;
                clex_lxsubcode = (i64)17;
            }else if (((i64)((*clex_lxsptr))==(i64)61)) {
                clex_lxsymbol = (i64)18;
                clex_lxsubcode = (i64)30;
            } else {
                clex_lxsymbol = (i64)17;
                clex_lxsubcode = (i64)12;
            };
            return;
        }break;
        case 63:;
        {
            clex_lxsymbol = (i64)5;
            return;
        }break;
        case 126:;
        {
            clex_lxsymbol = (i64)17;
            clex_lxsubcode = (i64)22;
            return;
        }break;
        case 61:;
        {
            if (((u64)((*clex_lxsptr)) == '=')) {
                ++clex_lxsptr;
                clex_lxsymbol = (i64)17;
                clex_lxsubcode = (i64)1;
            } else {
                clex_lxsymbol = (i64)7;
            };
            return;
        }break;
        case 33:;
        {
            if (((u64)((*clex_lxsptr)) == '=')) {
                ++clex_lxsptr;
                clex_lxsymbol = (i64)17;
                clex_lxsubcode = (i64)2;
            } else {
                clex_lxsymbol = (i64)17;
                clex_lxsubcode = (i64)21;
            };
            return;
        }break;
        default: {
            clex_lxsymbol = (i64)1;
            clex_lxvalue = (i64)(c);
            return;
        }
        } //SW
;
    }L8 :;
    ;
}

static void clex_printsymbol(void) {
    if ((clex_lxsymbol == (i64)0)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"ZERO?",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        return;
    };
    msysnewc_m_print_startcon();
    msysnewc_m_print_str(clex_symbolnames[(clex_lxsymbol)-1],NULL);
    msysnewc_m_print_str((byte*)"\t",NULL);
    msysnewc_m_print_end();
    ;
    if ((clex_lxsymbol==(i64)17) || (clex_lxsymbol==(i64)18)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str(clex_jtagnames[(clex_lxsubcode)-1],NULL);
        msysnewc_m_print_end();
        ;
    }else if ((clex_lxsymbol==(i64)24)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_i64(clex_lxvalue,NULL);
        msysnewc_m_print_end();
        ;
    }else if ((clex_lxsymbol==(i64)25)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_r64(clex_lxvaluex,NULL);
        msysnewc_m_print_end();
        ;
    }else if ((clex_lxsymbol==(i64)28)) {
        clex_printstr(clex_lxvaluestr,clex_lxlength);
    }else if ((clex_lxsymbol==(i64)23)) {
        clex_printstr(clex_lxvaluestr,clex_lxlength);
    };
    msysnewc_m_print_startcon();
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

static void clex_readname(i64 c) {
    clex_lxvaluestr = (clex_lxsptr - (i64)1);
    L12 :;
    while (!!((u64)(clex_alphamap[((i64)((*clex_lxsptr)))]))) {
        ++clex_lxsptr;
L13 :;
    }L14 :;
    ;
    clex_lxsymbol = (i64)23;
    clex_lxlength = (clex_lxsptr - clex_lxvaluestr);
}

static void clex_readdecdigits(i64 c) {
    byte *  intstr;
    byte *  fractstr;
    i64 intlength;
    i64 fractlength;
    i64 expsign;
    i64 expon;
    i64 firstdigitseen;
    if ((c == (i64)46)) {
        intstr = (byte *)(0);
        intlength = (i64)0;
        goto L15 ;
;
    };
    intstr = (clex_lxsptr - (i64)1);
    L16 :;
    while ((((c = (i64)((*clex_lxsptr++))) >= (i64)48) && (c <= (i64)57))) {
L17 :;
    }L18 :;
    ;
    intlength = ((clex_lxsptr - intstr) - (i64)1);
    switch (c) {
    case 46:;
    {
        ++clex_lxsptr;
        if (((u64)((*clex_lxsptr)) != '.')) {
            goto L15 ;
;
        } else {
            --clex_lxsptr;
        };
    }break;
    case 101:;
    case 69:;
    {
        intlength = (clex_lxsptr - intstr);
        ++clex_lxsptr;
        fractstr = (byte *)(0);
        fractlength = (i64)0;
        goto L19 ;
;
    }break;
    case 108:;
    case 76:;
    {
        ++clex_lxsptr;
        if (((i64)((*clex_lxsptr))==(i64)108) || ((i64)((*clex_lxsptr))==(i64)76)) {
            ++clex_lxsptr;
        }else if (((i64)((*clex_lxsptr))==(i64)117) || ((i64)((*clex_lxsptr))==(i64)85)) {
            ++clex_lxsptr;
        };
    }break;
    case 117:;
    case 85:;
    {
        ++clex_lxsptr;
        if (((i64)((*clex_lxsptr))==(i64)108) || ((i64)((*clex_lxsptr))==(i64)76)) {
            ++clex_lxsptr;
        }else if (((i64)((*clex_lxsptr))==(i64)117) || ((i64)((*clex_lxsptr))==(i64)85)) {
            ++clex_lxsptr;
        };
    }break;
    default: {
    }
    } //SW
;
    clex_lxsymbol = (i64)24;
    clex_readdecimal(intstr,intlength);
    return;
    //readfraction:
L15 :;
;
    c = (i64)((*clex_lxsptr));
    if (((c >= (i64)48) && (c <= (i64)57))) {
        fractstr = clex_lxsptr;
        ++clex_lxsptr;
        L20 :;
        switch ((c = (i64)((*clex_lxsptr)))) {
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
            ++clex_lxsptr;
        }break;
        default: {
            goto L21 ;
        }
        } //SW
goto L20 ;
L21 :;
        ;
        fractlength = (clex_lxsptr - fractstr);
        if ((c==(i64)101) || (c==(i64)69)) {
            ++clex_lxsptr;
            //readexpon:
L19 :;
;
            firstdigitseen = (i64)9;
            if (((i64)((*clex_lxsptr))==(i64)45)) {
                expsign = (i64)-1;
                ++clex_lxsptr;
            }else if (((i64)((*clex_lxsptr))==(i64)43)) {
                expsign = (i64)1;
                ++clex_lxsptr;
            };
            L22 :;
            while (1) {
                c = (i64)((*clex_lxsptr));
                if (((c >= (i64)48) && (c <= (i64)57))) {
                    ++clex_lxsptr;
                    expon = (((expon * (i64)10) + c) - (i64)57);
                    firstdigitseen = (i64)1;
                } else {
                    if (!(!!(firstdigitseen))) {
                        msysnewc_m_print_startcon();
                        msysnewc_m_print_str((byte*)"FDS",NULL);
                        msysnewc_m_print_newline();
                        msysnewc_m_print_end();
                        ;
                        clex_lxsymbol = (i64)1;
                        clex_lxsubcode = (i64)3;
                        return;
                    };
                    goto L23 ;
                };
            }L23 :;
            ;
        };
        clex_lxsymbol = (i64)25;
        clex_lxvaluex = clex_readreal(intstr,intlength,fractstr,fractlength,(expon * expon));
    };
}

static void clex_readhexdigits(void) {
    byte *  intstr;
    i64 intlength;
    i64 c;
    intstr = ++clex_lxsptr;
    L24 :;
    switch ((c = (i64)((*clex_lxsptr++)))) {
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
    }break;
    default: {
        goto L25 ;
    }
    } //SW
goto L24 ;
L25 :;
    ;
    intlength = ((clex_lxsptr - intstr) - (i64)1);
    clex_lxsymbol = (i64)24;
    clex_readhex(intstr,intlength);
}

static void clex_readbindigits(void) {
    byte *  intstr;
    i64 intlength;
    i64 c;
    intstr = ++clex_lxsptr;
    L26 :;
    switch ((c = (i64)((*clex_lxsptr++)))) {
    case 48:;
    case 49:;
    {
    }break;
    default: {
        goto L27 ;
    }
    } //SW
goto L26 ;
L27 :;
    ;
    intlength = ((clex_lxsptr - intstr) - (i64)1);
    clex_lxsymbol = (i64)24;
    clex_readbin(intstr,intlength);
}

static void clex_readstring(i64 termchar) {
    byte *  strstart;
    byte *  s;
    byte c;
    strstart = clex_lxsptr;
    s = strstart;
    L28 :;
    while (1) {
        switch ((i64)((c = (u64)((*clex_lxsptr++))))) {
        case 92:;
        {
            c = (u64)((*clex_lxsptr));
            if ((((u64)(c) >= 'A') && ((u64)(c) <= 'Z'))) {
                c += ' ';
            };
            ++clex_lxsptr;
            switch ((i64)(c)) {
            case 119:;
            {
                (*s) = (u64)13u;
                ++s;
                c = (u64)10u;
            }break;
            case 99:;
            case 114:;
            {
                c = (u64)13u;
            }break;
            case 108:;
            case 110:;
            {
                c = (u64)10u;
            }break;
            case 116:;
            {
                c = (u64)9u;
            }break;
            case 102:;
            {
                c = (u64)12u;
            }break;
            case 118:;
            {
                c = (u64)11u;
            }break;
            case 97:;
            {
                c = (u64)7u;
            }break;
            case 98:;
            {
                c = (u64)8u;
            }break;
            case 34:;
            case 81:;
            {
                c = '"';
            }break;
            case 101:;
            {
                c = '?';
            }break;
            case 122:;
            case 48:;
            {
                c = (u64)0u;
            }break;
            case 92:;
            {
                c = (u64)92u;
            }break;
            case 39:;
            {
                c = (u64)39u;
            }break;
            default: {
                c = '?';
                return;
            }
            } //SW
;
        }break;
        case 34:;
        case 39:;
        {
            if (((i64)(c) == termchar)) {
                if (((u64)((*clex_lxsptr)) == (u64)(c))) {
                    ++clex_lxsptr;
                } else {
                    goto L29 ;
                };
            };
        }break;
        case 13:;
        case 10:;
        case 26:;
        {
            --clex_lxsptr;
            goto L29 ;
        }break;
        default: {
        }
        } //SW
;
        (*s) = (u64)(c);
        ++s;
    }L29 :;
    ;
    clex_lxvaluestr = strstart;
    clex_lxlength = (s - strstart);
    clex_lxsymbol = ((termchar == (i64)34)?(i64)28:(i64)26);
}

static void clex_readlinecomment(void) {
    ++clex_lxsptr;
    L30 :;
    switch ((i64)((*clex_lxsptr++))) {
    case 13:;
    {
        if (((i64)((*clex_lxsptr)) == (i64)10)) {
            ++clex_lxsptr;
        };
        goto L31 ;
    }break;
    case 10:;
    {
        goto L31 ;
    }break;
    case 26:;
    case 0:;
    {
        --clex_lxsptr;
        goto L31 ;
    }break;
    default: {
    }
    } //SW
goto L30 ;
L31 :;
    ;
    ++clex_lxlineno;
}

static void clex_readblockcomment(void) {
    ++clex_lxsptr;
    L32 :;
    switch ((i64)((*clex_lxsptr++))) {
    case 13:;
    {
        if (((i64)((*clex_lxsptr)) == (i64)10)) {
            ++clex_lxsptr;
        };
        ++clex_lxlineno;
    }break;
    case 10:;
    {
        ++clex_lxlineno;
    }break;
    case 26:;
    case 0:;
    {
        clex_lxsymbol = (i64)1;
        return;
    }break;
    case 42:;
    {
        if (((u64)((*clex_lxsptr)) == '/')) {
            ++clex_lxsptr;
            goto L33 ;
        };
    }break;
    default: {
    }
    } //SW
goto L32 ;
L33 :;
    ;
}

static void clex_readdecimal(byte * p,i64 length) {
    i64 c;
    if ((length <= (i64)20)) {
        clex_lxvalue = ((u64)((*p)) - '0');
        L34 :;
        while ((--length > (i64)0)) {
            c = (i64)((*++p));
            clex_lxvalue = (((clex_lxvalue * (i64)10) + c) - (i64)48);
L35 :;
        }L36 :;
        ;
        clex_lxsymbol = (i64)24;
        return;
    };
}

static void clex_readhex(byte * p,i64 length) {
    i64 c;
    i64 av_1;
    if ((length <= (i64)16)) {
        clex_lxvalue = (i64)0;
        av_1 = length;
        while (av_1-- > 0) {
L37 :;
            c = (i64)((*p++));
            if ((c <= (i64)57)) {
                clex_lxvalue = (((clex_lxvalue * (i64)16) + c) - (i64)48);
            } else if ((c <= (i64)70)) {
                clex_lxvalue = ((((clex_lxvalue * (i64)16) + c) - (i64)65) + (i64)10);
            } else {
                clex_lxvalue = ((((clex_lxvalue * (i64)16) + c) - (i64)97) + (i64)10);
            };
L38 :;
        }L39 :;
        ;
        clex_lxsymbol = (i64)24;
        return;
    };
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"LONG INT HEX",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

static void clex_readbin(byte * p,i64 length) {
    i64 av_1;
    if ((length <= (i64)64)) {
        clex_lxvalue = (i64)0;
        av_1 = length;
        while (av_1-- > 0) {
L40 :;
            if (((i64)((*p++))==(i64)48)) {
                clex_lxvalue = (clex_lxvalue * (i64)2);
            }else if (((i64)((*p++))==(i64)49)) {
                clex_lxvalue = ((clex_lxvalue * (i64)2) + (i64)1);
            };
L41 :;
        }L42 :;
        ;
        clex_lxsymbol = (i64)24;
        return;
    };
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"LONG INT BIN",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"LONG INT BIN",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"LONG INT BIN",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"LONG INT BIN",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    exit(0);
}

static double clex_readreal(byte * istr,i64 ilength,byte * fstr,i64 flength,i64 expon) {
    return (double)0.;
}

static void clex_printstr(byte * s,i64 length) {
    printf((i8 *)((byte*)"%.*s"),length,s);
}

static void clex_initdata(void) {
    i64 i;
    L43 :;
    for (i=(i64)0;i<=(i64)255;i+=(i64)1) {
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
            clex_alphamap[(i)] = (u64)1u;
        }break;
        default: {
        }
        } //SW
;
L45 :;
    }L46 :;
    ;
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
    L47 :;
    for (i=(i64)1;i<=(i64)2048;i+=(i64)1) {
L48 :;
        j = (i64)1;
        k = (i64)16;
        L51 :;
        while ((i > k)) {
            k = (k << (i64)1);
            ++j;
L52 :;
        }L53 :;
        ;
        mlib_sizeindextable[(i)] = (u64)(j);
L49 :;
    }L50 :;
    ;
    mlib_allocupper[((i64)1)] = (u64)((i64)16);
    size = (i64)16;
    L54 :;
    for (i=(i64)2;i<=(i64)27;i+=(i64)1) {
L55 :;
        size *= (i64)2;
        mlib_allocupper[(i)] = (u64)(size);
        if ((size >= (i64)33554432)) {
            k = i;
            goto L57 ;
        };
L56 :;
    }L57 :;
    ;
    L58 :;
    for (i=(k + (i64)1);i<=(i64)300;i+=(i64)1) {
L59 :;
        size += (i64)33554432;
        if ((size < (i64)8589934592)) {
            mlib_allocupper[(i)] = (u64)(size);
            mlib_maxmemory = (u64)(size);
        } else {
            mlib_maxalloccode = (i - (i64)1);
            goto L61 ;
        };
L60 :;
    }L61 :;
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
        L62 :;
        while ((n > m)) {
            m <<= (i64)1;
L63 :;
        }L64 :;
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
    L65 :;
    while (!!(p)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)" ",NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_ptr(p,(byte*)"h");
        msysnewc_m_print_end();
        ;
        p = (u64 *)((i64)((*p)));
L66 :;
    }L67 :;
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
    L68 :;
    for (i=(i64)1;i<=(i64)8;i+=(i64)1) {
L69 :;
        mlib_pcm_printfreelist(m,mlib_freelist[(i)]);
        m <<= (i64)1;
L70 :;
    }L71 :;
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
    L72 :;
    for (i=(i64)1;i<=(i64)500000;i+=(i64)1) {
L73 :;
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
L74 :;
    }L75 :;
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"MEMALLOCTABLE FULL\n\n\n\n",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    oswindows_os_getch();
    exit((i64)3);
}

static void mlib_removefrommemalloc(i32 * ptr,i64 size) {
    i64 i;
    L76 :;
    for (i=(i64)1;i<=(i64)500000;i+=(i64)1) {
L77 :;
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
L78 :;
    }L79 :;
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
    oswindows_os_getch();
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
        handlex = oswindows_os_getstdin();
    };
    if ((handlex == 0)) {
        n = (i64)0;
        p = buffer;
        L80 :;
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
        }L81 :;
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
    L82 :;
    while (((p >= buffer) && (((i64)((*p)) == (i64)13) || ((i64)((*p)) == (i64)10)))) {
        if ((((i64)((*p)) == (i64)13) || ((i64)((*p)) == (i64)10))) {
            crseen = (u64)((i64)1);
        };
        (*p--) = (u64)0u;
L83 :;
    }L84 :;
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
L85 :;
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L86 :;
    }L87 :;
    ;
}

void mlib_iconvucn(byte * s,i64 n) {
    i64 av_1;
    av_1 = n;
    while (av_1-- > 0) {
L88 :;
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L89 :;
    }L90 :;
    ;
}

void mlib_convlcstring(byte * s) {
    L91 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L92 :;
    }L93 :;
    ;
}

void mlib_convucstring(byte * s) {
    L94 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L95 :;
    }L96 :;
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
    L97 :;
    while ((u >= t)) {
        if (((u64)((*u)) == '.')) {
            if (((i64)((*(u + (i64)1))) == (i64)0)) {
                return (!!(period)?(byte*)".":(byte*)"");
            };
            return (u + (i64)1);
        };
        --u;
L98 :;
    }L99 :;
    ;
    return (byte*)"";
}

byte * mlib_extractpath(byte * s) {
    static byte str[260];
    byte *  t;
    i64 n;
    t = ((s + (i64)(strlen((i8 *)(s)))) - (i64)1);
    L100 :;
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
L101 :;
    }L102 :;
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
    L103 :;
    for (i=(i64)2;i<=(i64)2;i+=(i64)1) {
L104 :;
        p = mlib_freelist[(i)];
        L107 :;
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
L108 :;
        }L109 :;
        ;
L105 :;
    }L106 :;
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
        L110 :;
        for (i=(i64)1;i<=n;i+=(i64)1) {
L111 :;
            str[((slen + i))-1] = (u64)(padch);
L112 :;
        }L113 :;
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
    L114 :;
    for (i=(i64)1;i<=n;i+=(i64)1) {
L115 :;
        str[(i)-1] = (u64)(ch);
L116 :;
    }L117 :;
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
L118 :;
;
    (*value) = (byte *)(0);
    (*name) = (byte *)(0);
    if (!!(infile)) {
        if ((mlib_readnextfileitem(&fileptr,&item) == (i64)0)) {
            free((void *)(filestart));
            infile = (i64)0;
            goto L118 ;
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
            goto L118 ;
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
L119 :;
;
    L120 :;
    while (1) {
        if (((i64)((*p))==(i64)32) || ((i64)((*p))==(i64)9) || ((i64)((*p))==(i64)13) || ((i64)((*p))==(i64)10)) {
            ++p;
        }else if (((i64)((*p))==(i64)26) || ((i64)((*p))==(i64)0)) {
            return (i64)0;
        } else {
            goto L121 ;
        };
    }L121 :;
    ;
    if (((i64)((*p))==(i64)33) || ((i64)((*p))==(i64)35)) {
        ++p;
        L122 :;
        if (((i64)((*p++))==(i64)10)) {
            goto L119 ;
;
        }else if (((i64)((*p++))==(i64)26) || ((i64)((*p++))==(i64)0)) {
            (*fileptr) = (p - (i64)1);
            return (i64)0;
        } else {
        }goto L122 ;
L123 :;
        ;
    };
    if (((i64)((*p))==(i64)34)) {
        pstart = ++p;
        L124 :;
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
                goto L125 ;
            };
            ++p;
        }L125 :;
        ;
    } else {
        pstart = p;
        L126 :;
        while (1) {
            if (((i64)((*p))==(i64)0) || ((i64)((*p))==(i64)26)) {
                pend = p;
                goto L127 ;
            }else if (((i64)((*p))==(i64)32) || ((i64)((*p))==(i64)9) || ((i64)((*p))==(i64)44) || ((i64)((*p))==(i64)13) || ((i64)((*p))==(i64)10)) {
                pend = p++;
                goto L127 ;
            };
            ++p;
        }L127 :;
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
L128 :;
        strcat((i8 *)(s),(i8 *)(padchar));
L129 :;
    }L130 :;
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
    L131 :;
    do {
        x = ((double)(mlib_mrandomp()) / (double)9223372036854775800.);
L132 :;
    } while (!(x != (double)1.));L133 :;
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
    strcpy((i8 *)(&exefile[((i64)1)-1]),(i8 *)(oswindows_os_gethostname()));
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
    L134 :;
    while (!!((i64)1)) {
        c = (u64)((*msysnewc_fmtstr));
        switch ((i64)(c)) {
        case 35:;
        {
            if (!!(lastx)) {
                goto L137 ;
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
L137 :;
;
            ++n;
            ++msysnewc_fmtstr;
        }
        } //SW
;
L135 :;
    }L136 :;
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
    L138 :;
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
            goto L141 ;
;
        }break;
        default: {
            if ((((u64)(c) >= '0') && ((u64)(c) <= '9'))) {
                n = ((u64)(c) - '0');
                L142 :;
                while (1) {
                    c = (u64)((*s));
                    if (((i64)((*s)) == (i64)0)) {
                        goto L143 ;
                    };
                    if ((((u64)(c) >= '0') && ((u64)(c) <= '9'))) {
                        ++s;
                        n = (((n * (i64)10) + (i64)(c)) - (i64)48);
                    } else {
                        goto L143 ;
                    };
                }L143 :;
                ;
                //gotwidth:
L141 :;
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
L139 :;
    }L140 :;
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
L144 :;
        if (((i64)((*p)) == (i64)0)) {
            goto L146 ;
        };
        (*q) = (u64)((*p));
        ++q;
        ++p;
L145 :;
    }L146 :;
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
        L147 :;
        for (i=(i64)1;i<=(w - n);i+=(i64)1) {
L148 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L149 :;
        }L150 :;
        ;
        (*t) = (u64)0u;
    } else if (((u64)((*fmt).justify) == 'R')) {
        if (((((u64)((*fmt).padchar) == '0') && !!((u64)((*fmt).base))) && (((u64)((*s)) == '-') || ((u64)((*s)) == '+')))) {
            (*t) = (u64)((*s));
            ++t;
            av_2 = (w - n);
            while (av_2-- > 0) {
L151 :;
                (*t) = (u64)((*fmt).padchar);
                ++t;
L152 :;
            }L153 :;
            ;
            strncpy((i8 *)(t),(i8 *)((s + (i64)1)),(u64)((n - (i64)1)));
            (*((t + n) - (i64)1)) = (u64)0u;
        } else {
            av_3 = (w - n);
            while (av_3-- > 0) {
L154 :;
                (*t) = (u64)((*fmt).padchar);
                ++t;
L155 :;
            }L156 :;
            ;
            strncpy((i8 *)(t),(i8 *)(s),(u64)(n));
            (*(t + n)) = (u64)0u;
        };
    } else {
        m = (((w - n) + (i64)1) / (i64)2);
        av_4 = m;
        while (av_4-- > 0) {
L157 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L158 :;
        }L159 :;
        ;
        strncpy((i8 *)(t),(i8 *)(s),(u64)(n));
        t += n;
        av_5 = ((w - n) - m);
        while (av_5-- > 0) {
L160 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L161 :;
        }L162 :;
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
    L163 :;
    do {
        t[(++i)] = (u64)(msysnewc_digits[((i64)((aa % base)))]);
        aa = (aa / base);
        ++k;
        if (((!!(sep) && ((i64)(aa) != (i64)0)) && (k == g))) {
            t[(++i)] = (u64)(sep);
            k = (i64)0;
        };
L164 :;
    } while (!((i64)(aa) == (i64)0));L165 :;
    ;
    j = i;
    s0 = s;
    L166 :;
    while (!!(i)) {
        (*s) = (u64)(t[(i--)]);
        ++s;
L167 :;
    }L168 :;
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
    L169 :;
    while (!!(i)) {
        --s;
        (*s) = (u64)(t[((i-- - (i64)1))]);
        if (((!!(sep) && !!(i)) && (++k == g))) {
            --s;
            (*s) = (u64)(sep);
            k = (i64)0;
        };
L170 :;
    }L171 :;
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
    L172 :;
    while ((((u64)((*s)) == ' ') || ((i64)((*s)) == (i64)9))) {
        ++s;
L173 :;
    }L174 :;
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
    L175 :;
    while (!!((u64)((*s)))) {
        c = (u64)((*s++));
        switch ((i64)(c)) {
        case 32:;
        case 9:;
        case 44:;
        case 61:;
        {
            if ((!!((u64)(quotechar)) || (p == s))) {
                goto L178 ;
;
            };
            msysnewc_termchar = (i64)(c);
            goto L177 ;
        }break;
        default: {
            //normalchar:
L178 :;
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
                    goto L177 ;
                };
            } else {
                (*p) = (u64)(c);
                ++p;
            };
        }
        } //SW
;
L176 :;
    }L177 :;
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
    L179 :;
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
            goto L180 ;
        }break;
        default: {
            msysnewc_itemerror = (i64)1;
            goto L181 ;
        }
        } //SW
;
        if (((i64)(d) >= base)) {
            msysnewc_itemerror = (i64)1;
            goto L181 ;
        };
        aa = (u64)((((i64)(aa) * base) + (i64)(d)));
L180 :;
    }L181 :;
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
L182 :;
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L183 :;
    }L184 :;
    ;
}

static void msysnewc_iconvucn(byte * s,i64 n) {
    i64 av_1;
    av_1 = n;
    while (av_1-- > 0) {
L185 :;
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L186 :;
    }L187 :;
    ;
}

static void msysnewc_convlcstring(byte * s) {
    L188 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L189 :;
    }L190 :;
    ;
}

static void msysnewc_convucstring(byte * s) {
    L191 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L192 :;
    }L193 :;
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

void oswindows_os_init(void) {
    oswindows_hconsole = GetStdHandle((u64)4294967285u);
    oswindows_hconsolein = GetStdHandle((u64)4294967286u);
    oswindows_lastkey.repeatcount = (u64)((i64)0);
    oswindows_keypending = (i64)0;
    SetConsoleCtrlHandler((void (*)(void))(0),(u64)1u);
    SetConsoleMode(oswindows_hconsole,(u64)3u);
    oswindows_init_flag = (i64)1;
}

i64 oswindows_os_execwait(byte * cmdline,i64 newconsole,byte * workdir) {
    u32 exitcode;
    i64 status;
    i64 cflags;
    struct oswindows_rstartupinfo si;
    struct oswindows_rprocess_information xpi;
    cflags = (i64)0;
    memset((void *)(&si),(i64)0,(u64)((i64)104));
    memset((void *)(&xpi),(i64)0,(u64)((i64)24));
    switch (newconsole) {
    case 0:;
    {
        cflags = (i64)32;
    }break;
    case 1:;
    {
        cflags = (i64)48;
    }break;
    case 2:;
    {
        cflags = (i64)48;
    }break;
    default: {
    }
    } //SW
;
    si.size = (u64)((i64)104);
    status = (i64)(CreateProcessA((byte *)(0),cmdline,0,0,(u64)1u,(u64)((u32)(cflags)),0,(byte *)(0),(void *)(&si),(void *)(&xpi)));
    if ((status == (i64)0)) {
        status = (i64)(GetLastError());
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"Winexec error:",NULL);
        msysnewc_m_print_i64(status,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        return (i64)-1;
    };
    WaitForSingleObject(xpi.process,(u64)4294967295u);
    GetExitCodeProcess(xpi.process,(void *)(&exitcode));
    CloseHandle(xpi.process);
    CloseHandle(xpi.thread);
    return (i64)(exitcode);
}

i64 oswindows_os_execcmd(byte * cmdline,i64 newconsole) {
    struct oswindows_rstartupinfo si;
    struct oswindows_rprocess_information xpi;
    memset((void *)(&si),(i64)0,(u64)((i64)104));
    memset((void *)(&xpi),(i64)0,(u64)((i64)24));
    si.size = (u64)((i64)104);
    CreateProcessA((byte *)(0),cmdline,0,0,(u64)1u,(u64)((u32)(((i64)32 | (!!(newconsole)?(i64)16:(i64)0)))),0,(byte *)(0),(void *)(&si),(void *)(&xpi));
    CloseHandle(xpi.process);
    CloseHandle(xpi.thread);
    return (i64)1;
}

i64 oswindows_os_getch(void) {
    i64 k;
    k = (oswindows_os_getchx() & (i64)255);
    return k;
}

i64 oswindows_os_kbhit(void) {
    u32 count;
    if (!(!!(oswindows_init_flag))) {
        oswindows_os_init();
    };
    GetNumberOfConsoleInputEvents(oswindows_hconsolein,(void *)(&count));
    return ((i64)((u64)(count)) > (i64)1);
}

void oswindows_os_flushkeys(void) {
    FlushConsoleInputBuffer(oswindows_hconsolein);
}

void * oswindows_os_getconsolein(void) {
    return oswindows_hconsolein;
}

void * oswindows_os_getconsoleout(void) {
    return oswindows_hconsole;
}

void * oswindows_os_proginstance(void) {
    mlib_abortprogram((byte*)"PROGINST");
    return 0;
}

u64 oswindows_os_getdllinst(byte * name) {
    void *  hinst;
    hinst = LoadLibraryA(name);
    return (u64)(hinst);
}

void * oswindows_os_getdllprocaddr(i64 hinst,byte * name) {
    return GetProcAddress((void *)(hinst),name);
}

void oswindows_os_initwindows(void) {
    oswindows_os_init();
    oswindows_os_gxregisterclass((byte*)"pcc001");
}

void oswindows_os_gxregisterclass(byte * classname) {
    struct oswindows_rwndclassex r;
    static byte registered;
    if (!!((u64)(registered))) {
        return;
    };
    memset((void *)(&r),(i64)0,(u64)((i64)80));
    r.size = (u64)((i64)80);
    r.style = (u64)((i64)40);
    r.wndproc = (void (*)(void))(&oswindows_mainwndproc);
    r.instance = 0;
    r.icon = 0;
    r.cursor = LoadCursorA(0,(byte *)((void *)((i64)32512)));
    r.background = (void *)((i64)16);
    r.menuname = (byte *)(0);
    r.classname = classname;
    r.iconsm = 0;
    if (((i64)((u64)(RegisterClassExA((void *)(&r)))) == (i64)0)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str(classname,NULL);
        msysnewc_m_print_ptr(GetLastError,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        mlib_abortprogram((byte*)"Registerclass error");
    };
    registered = (u64)((i64)1);
}

CALLBACK i64 oswindows_mainwndproc(void * hwnd,u32 message,u64 wparam,u64 lparam) {
    struct oswindows_rmsg m;
    i64 result;
    static i64 count = (i64)0;
    m.hwnd = hwnd;
    m.message = (u64)(message);
    m.wparam = wparam;
    m.lparam = lparam;
    m.pt.x = (i64)0;
    m.pt.y = (i64)0;
    if (!!(oswindows_wndproc_callbackfn)) {
        result = ((*oswindows_wndproc_callbackfn))((void *)(&m));
    } else {
        result = (i64)0;
    };
    if (((i64)((u64)(m.message)) == (i64)2)) {
        return (i64)0;
    };
    if (!(!!(result))) {
        return (i64)(DefWindowProcA(hwnd,(u64)(message),wparam,lparam));
    } else {
        return (i64)0;
    };
}

static void oswindows_timerproc(void * hwnd,i64 msg,i64 id,i64 time) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"TIMERPROC",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

void oswindows_os_setmesshandler(void * addr) {
    oswindows_wndproc_callbackfn = (i64 (*)(void *))(addr);
}

i64 oswindows_os_getchx(void) {
    i64 count;
    i64 charcode;
    i64 keyshift;
    i64 keycode;
    i64 altdown;
    i64 ctrldown;
    i64 shiftdown;
    i64 capslock;
    if (!(!!(oswindows_init_flag))) {
        oswindows_os_init();
    };
    if (!!(oswindows_keypending)) {
        oswindows_lastkey = oswindows_pendkey;
        oswindows_keypending = (i64)0;
    } else {
        if (((i64)((u64)(oswindows_lastkey.repeatcount)) == (i64)0)) {
            L194 :;
            do {
                count = (i64)0;
                ReadConsoleInputA(oswindows_hconsolein,(void *)(&oswindows_lastkey),(u64)1u,(void *)(&count));
L195 :;
            } while (!(((i64)((u64)(oswindows_lastkey.eventtype)) == (i64)1) && ((i64)((u64)(oswindows_lastkey.keydown)) == (i64)1)));L196 :;
            ;
        };
    };
    altdown = (!!(((i64)((u64)(oswindows_lastkey.controlkeystate)) & (i64)3))?(i64)1:(i64)0);
    ctrldown = (!!(((i64)((u64)(oswindows_lastkey.controlkeystate)) & (i64)12))?(i64)1:(i64)0);
    shiftdown = (!!(((i64)((u64)(oswindows_lastkey.controlkeystate)) & (i64)16))?(i64)1:(i64)0);
    capslock = (!!(((i64)((u64)(oswindows_lastkey.controlkeystate)) & (i64)128))?(i64)1:(i64)0);
    --oswindows_lastkey.repeatcount;
    charcode = (i64)(oswindows_lastkey.asciichar);
    keycode = ((i64)((u64)(oswindows_lastkey.virtualkeycode)) & (i64)255);
    if ((charcode < (i64)0)) {
        if ((charcode < (i64)-128)) {
            charcode = (i64)0;
        } else {
            charcode += (i64)256;
        };
    };
    if (((!!(altdown) && !!(ctrldown)) && (charcode == (i64)166))) {
        altdown = (ctrldown = (i64)0);
    } else {
        if ((!!(altdown) || !!(ctrldown))) {
            charcode = (i64)0;
            if (((keycode >= (i64)65) && (keycode <= (i64)90))) {
                charcode = (keycode - (i64)64);
            };
        };
    };
    keyshift = ((((capslock << (i64)3) | (altdown << (i64)2)) | (ctrldown << (i64)1)) | shiftdown);
    return (((keyshift << (i64)24) | (keycode << (i64)16)) | charcode);
}

byte * oswindows_os_getos(void) {
    if (((i64)64 == (i64)32)) {
        return (byte*)"W32";
    } else {
        return (byte*)"W64";
    };
}

i64 oswindows_os_gethostsize(void) {
    return (i64)64;
}

i64 oswindows_os_shellexec(byte * opc,byte * file) {
    return (i64)(system((i8 *)(file)));
}

void oswindows_os_sleep(i64 a) {
    Sleep((u64)((u32)(a)));
}

void * oswindows_os_getstdin(void) {
    return fopen((i8 *)((byte*)"con"),(i8 *)((byte*)"rb"));
}

void * oswindows_os_getstdout(void) {
    return fopen((i8 *)((byte*)"con"),(i8 *)((byte*)"wb"));
}

byte * oswindows_os_gethostname(void) {
    static byte name[300];
    GetModuleFileNameA(0,name,(u64)300u);
    strcat((i8 *)(name),(i8 *)((byte*)"/"));
    return name;
}

byte * oswindows_os_getmpath(void) {
    return (byte*)"C:\\m\\";
}

void oswindows_os_exitprocess(i64 x) {
    exit(x);
}

i64 oswindows_os_clock(void) {
    return (i64)(clock());
}

i64 oswindows_os_getclockspersec(void) {
    return (i64)1000;
}

i64 oswindows_os_iswindows(void) {
    return (i64)1;
}

i64 oswindows_os_filelastwritetime(byte * filename) {
    void *  f;
    i64 ctime;
    i64 atime;
    i64 wtime;
    if ((filename == 0)) {
        return (i64)1;
    };
    f = CreateFileA(filename,(u64)2147483648u,(u64)1u,0,(u64)3u,(u64)3u,0);
    if (((i64)(f) == (i64)-1)) {
        return (i64)0;
    };
    GetFileTime(f,(void *)(&ctime),(void *)(&atime),(void *)(&wtime));
    CloseHandle(f);
    return wtime;
}

void oswindows_os_getsystime(struct oswindows_rsystemtime * tm) {
    GetLocalTime(tm);
}

void oswindows_os_messagebox(byte * s,byte * t) {
    MessageBoxA((i64)0,s,t,(i64)0);
}

i64 oswindows_os_hpcounter(void) {
    i64 a;
    QueryPerformanceCounter(&a);
    return a;
}

i64 oswindows_os_hpfrequency(void) {
    i64 a;
    QueryPerformanceFrequency(&a);
    return a;
}

void oswindows_os_peek(void) {
    i64 ticks;
    static i64 lastticks;
    byte m[100];
    ticks = (i64)(GetTickCount());
    if (((ticks - lastticks) >= (i64)1000)) {
        lastticks = ticks;
        PeekMessageA((void *)(&m),(void * *)(0),(u64)0u,(u64)0u,(u64)0u);
    };
}


/* ********** End of C Code ********** */
