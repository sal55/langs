/*
Info header for C distribution.
Project: 'MU': M Compiler with C Target for Linux

Typical Build Instructions (needs 64-bit compiler):

  gcc -O3 mu64.c -omu -lm -ldl
  tcc mu64.c -omu -lm -ldl

This program compiles an entire M project from its lead module.

Output is a single executable .obj or .c file depending on options. Default
output is executable file

Dependencies:

Requires a 64-bit C compiler to bootstrap the compiler, and also
to compile the output of MU. Tested on gcc (and tcc but not on RPi)

Run as follows (assumes gcc present to build intermediate C)

    ./mu hello               # compile hello.m project to hello.exe

    ./mu hello -c            # compile hello.m project to single hello.c file

    ./mu hello -tcc          # build using tcc


Other options:

    -c                    # Generate only intermediate C output file
    -exe                  # Generate executable via C compiler (default)
    -obj                  # Generate single object file
 
    -gcc                  # Use gcc (default)
    -tcc                  # Use tcc

    -opt                  # use -O3 for gcc only

    -run                  # For -exe mode only: run resulting executable

    -help                 # Show summary

Example:

     ./mu -run prog : abc def

Any parameters for the new program must follow " : " (spaces needed).

Test hello.m file:

    proc start =
      println "Hello, World!"
    end

For bigger M examples, see github site.

*/

/*
  M to C  Whole Program Translator
  Input:  mu.m plus imported modules
  Output: mu64.c (this file, or renamed from that)
          File represents entire program
  Target: C 64-bit
  OS:     Linux

  Modules:
  Module 1: mu.m
  Module 2: ./mm_decls.m
  Module 3: <Built-in: clibc.m>
  Module 4: ./mm_tables.m
  Module 5: ./mm_start.m
  Module 6: <Built-in: msysc.m>
  Module 7: <Built-in: mlib.m>
  Module 8: <Built-in: oslinux.m>
  Module 9: ./mm_support.m
  Module 10: ./mm_lib.m
  Module 11: ./mm_lex.m
  Module 12: ./mm_diags.m
  Module 13: ./mm_genc64.m
  Module 14: ./mm_genc.m
  Module 15: ./mm_libc.m
  Module 16: ./mm_blockc.m
  Module 17: ./mm_parse.m
  Module 18: ./mm_name.m
  Module 19: ./mm_type.m

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

#ifndef CALLBACK
#define CALLBACK
#endif

#if (UINTPTR_MAX<0xFFFFFFFFFFFFFFFF)
	#error "Need 64-bit target. Try -m64"
#endif

/* Forward Struct Declarations */
struct mm_decls_lexrec;
struct mm_decls_uflagsrec;
struct mm_decls_fieldrec;
struct mm_decls_qint;
struct mm_decls_strec;
struct mm_decls_unitrec;
struct mm_decls_modulerec;
struct mm_decls_dllprocrec;
struct mm_decls_procrec;
struct mm_decls_userxrec;
struct msysc_procinforec;
struct msysc_fmtrec;
struct mlib_strbuffer;
struct oslinux_termios;
struct oslinux_rsystemtime;
struct mm_genc_stlinkrec;

/* Struct Definitions */
struct mm_decls_lexrec {
    union {
        i64 value;
        double xvalue;
        u64 uvalue;
        byte *  svalue;
        struct mm_decls_qint *  qvalue;
    };
    struct mm_decls_strec *  symptr;
    i32 hashvalue;
    i32 length;
    i32 lineno;
    byte symbol;
    byte subcode;
    byte fileno;
    byte spare;
};

struct mm_decls_uflagsrec {
    byte codes[7];
    byte ulength;
};

struct mm_decls_fieldrec {
    byte *  name;
    i16 recordtype;
    i16 fieldtype;
    i32 fieldoffset;
};

struct mm_decls_qint {
    u64 lower;
    i64 upper;
};

struct mm_decls_strec {
    byte *  name;
    struct mm_decls_strec* owner;
    struct mm_decls_strec* deflist;
    struct mm_decls_strec* deflistx;
    struct mm_decls_strec* nextdef;
    struct mm_decls_strec* nextdupl;
    struct mm_decls_strec* firstdupl;
    union {
        struct mm_decls_strec* nextparam;
    };
    struct mm_decls_unitrec *  code;
    union {
        struct mm_decls_strec* paramlist;
        struct mm_decls_uflagsrec uflags;
    };
    union {
        struct mm_decls_strec* equivfield;
        struct mm_decls_unitrec *  equivvar;
    };
    union {
        byte *  truename;
        byte *  metadata;
        byte *  macrovalue;
        struct mm_decls_strec* nulldef;
    };
    union {
        i32 modelist[4];
        i32 mode;
    };
    byte namelen;
    byte symbol;
    byte nameid;
    byte txdone;
    i32 subcode;
    i32 index;
    union {
        i32 offset;
        i32 base_class;
    };
    i32 lineno;
    byte isglobal;
    byte isstatic;
    byte equals;
    byte at;
    byte parammode;
    byte optional;
    union {
        byte varparams;
        byte bitoffset;
    };
    byte used;
    union {
        byte asmused;
        byte bitfieldwidth;
    };
    byte circflag;
    byte fflang;
    byte moduleno;
    byte imported;
    byte nretvalues;
    byte namecat;
    union {
        byte align;
        byte dllindex;
        byte extmodno;
    };
    byte islet;
    byte simplefunc;
};

struct mm_decls_unitrec {
    i32 tag;
    i32 lineno;
    struct mm_decls_unitrec* nextunit;
    union {
        struct mm_decls_strec *  def;
        i64 value;
        u64 uvalue;
        double xvalue;
        byte *  svalue;
        struct mm_decls_qint *  qvalue;
        struct mm_decls_strec *  labeldef;
        struct {
            i64 range_lower;
            i64 range_upper;
        };
        struct {
            u64 value_lower;
            i64 value_upper;
        };
        struct {
            u64 uvalue_lower;
            u64 uvalue_upper;
        };
    };
    union {
        i32 opcode;
        i32 index;
        i32 whenlabel;
        i32 trylevel;
        i32 slength;
        i32 length;
        byte dottedname;
        i32 offset;
    };
    i32 mode;
    i32 moduleno;
    i32 addroffirst;
    i32 isconst;
    i32 popflag;
    i32 ifretflag;
    i32 newmode;
    i32 isastring;
    struct mm_decls_unitrec* a;
    struct mm_decls_unitrec* b;
    struct mm_decls_unitrec* c;
};

struct mm_decls_modulerec {
    byte *  name;
    struct mm_decls_strec *  stmodule;
    i64 fileno;
    union {
        byte *  asmstr;
        byte *  clangstr;
    };
    i64 strlength;
    byte importmap[50];
    struct mm_decls_strec *  stinitproc;
};

struct mm_decls_dllprocrec {
    byte *  name;
    void (*address)(void);
    i64 dllindex;
};

struct mm_decls_procrec {
    struct mm_decls_strec *  def;
    struct mm_decls_procrec* nextproc;
};

struct mm_decls_userxrec {
    struct mm_decls_strec *  owner;
    i32 *  pmode;
    struct mm_decls_userxrec* nextmode;
};

struct msysc_procinforec {
    u16 fnindex;
    byte rettype;
    byte nparams;
    byte paramlist[12];
};

struct msysc_fmtrec {
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

struct mlib_strbuffer {
    byte *  strptr;
    i32 length;
    i32 allocated;
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

struct mm_genc_stlinkrec {
    struct mm_decls_strec *  def;
    struct mm_genc_stlinkrec* nextsymbol;
};


/* PROCDECLS */
void start(void);
void mm_start_start_common(i64 itarget);
static void mm_start_debugcompiler(void);
static void mm_start_do_loadmodules(void);
static void mm_start_do_parse(void);
static void mm_start_do_name(void);
static void mm_start_do_type(void);
static void mm_start_do_runprog(void);
static i64 mm_start_loadmainmodule(byte * filespec);
static i64 mm_start_addmodule(byte * modulename,i64 fileno,i64 * exportflag);
static i64 mm_start_loadimport(byte * modulename,i64 * exportflag,byte * ownername);
static i64 mm_start_readimportlist(struct mm_decls_modulerec * m,byte * (*importnames)[],byte (*importflags)[],i64 maximports);
static void mm_start_pslex(void);
static void mm_start_initdata(void);
static void mm_start_initsearchdirs(void);
static void mm_start_addsearchdir(byte * path);
static void mm_start_showsearchdirs(void);
static void mm_start_showast(byte * filename,i64 * flag);
static void mm_start_showstflat(byte * caption,void * f);
static void mm_start_showsttree(byte * caption,void * f);
void mm_start_showtiming(void);
static void mm_start_getinputoptions(void);
static void mm_start_do_option(i64 sw,byte * value);
static void mm_start_showcaption(void);
static void mm_start_addtolog(byte * filename,void * logdest);
static void mm_start_addoptionvar(byte * name,byte * value);
void mm_start_addmodulemapping(byte * old,byte * new,byte * optionname,byte * valuename);
static void mm_start_dosetoptionvar(byte * s);
static i64 mm_start_findoptionvar(byte * name);
static void mm_start_getpsname(byte * dest);
static void mm_start_domapmodule(void);
byte * mm_start_mapimport(byte * name);
static void mm_start_do_writema(void);
i64 msysc_m_getdotindex(u64 a,i64 i);
void msysc_m_setdotindex(u64 * a,i64 i,i64 x);
i64 msysc_m_getdotslice(u64 a,i64 i,i64 j);
void msysc_m_setdotslice(u64 * a,i64 i,i64 j,u64 x);
i64 msysc_m_getnprocs(void);
i64 msysc_m_getnexports(void);
void * msysc_m_getprocaddr(i64 n);
byte * msysc_m_getprocname(i64 n);
void * msysc_m_getprocexport(i64 n);
static void msysc_pushio(void);
void msysc_m_print_startfile(void * dev);
void msysc_m_print_startstr(byte * s);
void msysc_m_print_startptr(byte * * p);
void msysc_m_print_startcon(void);
void msysc_m_print_setfmt(byte * format);
void msysc_m_print_end(void);
void msysc_m_print_ptr(void * a,byte * fmtstyle);
void msysc_m_print_i64(i64 a,byte * fmtstyle);
void msysc_m_print_u64(u64 a,byte * fmtstyle);
void msysc_m_print_r64(double x,byte * fmtstyle);
void msysc_m_print_r32(float x,byte * fmtstyle);
void msysc_m_print_c8(i64 a,byte * fmtstyle);
void msysc_m_print_str(byte * s,byte * fmtstyle);
void msysc_m_print_newline(void);
void msysc_m_print_nogap(void);
static void msysc_printstr(byte * s);
void msysc_printstr_n(byte * s,i64 n);
static byte * msysc_makezstring(byte * s,i64 n,byte * local);
static void msysc_freezstring(byte * t,i64 n);
static void msysc_printchar(i64 ch);
static void msysc_nextfmtchars(i64 lastx);
void msysc_strtofmt(byte * s,i64 slen,struct msysc_fmtrec * fmt);
static i64 msysc_domultichar(byte * p,i64 n,byte * dest,struct msysc_fmtrec * fmt);
static i64 msysc_expandstr(byte * s,byte * t,i64 n,struct msysc_fmtrec * fmt);
static u64 msysc_xdivrem(u64 a,u64 b,u64 * remainder);
static i64 msysc_u64tostr(u64 aa,byte * s,u64 base,i64 sep);
static i64 msysc_i64tostrfmt(i64 aa,byte * s,struct msysc_fmtrec * fmt);
static i64 msysc_u64tostrfmt(i64 aa,byte * s,struct msysc_fmtrec * fmt);
static i64 msysc_i64mintostr(byte * s,i64 base,i64 sep);
static i64 msysc_strtostrfmt(byte * s,byte * t,i64 n,struct msysc_fmtrec * fmt);
static void msysc_tostr_i64(i64 a,struct msysc_fmtrec * fmt);
static void msysc_tostr_u64(u64 a,struct msysc_fmtrec * fmt);
static void msysc_tostr_r64(double x,struct msysc_fmtrec * fmt);
static void msysc_tostr_str(byte * s,struct msysc_fmtrec * fmt);
static struct msysc_fmtrec * msysc_getfmt(byte * fmtstyle);
byte * msysc_strint(i64 a,byte * fmtstyle);
void msysc_getstrint(i64 a,byte * dest);
byte * msysc_strword(u64 a,byte * fmtstyle);
byte * msysc_strreal(double a,byte * fmtstyle);
static byte * msysc_getstr(byte * s,struct msysc_fmtrec * fmt);
static void msysc_initreadbuffer(void);
void msysc_m_read_conline(void);
void msysc_m_read_fileline(void * f);
void msysc_m_read_strline(byte * s);
static byte * msysc_readitem(i64 * itemlength);
static i64 msysc_strtoint(byte * s,i64 length,i64 base);
i64 msysc_m_read_i64(i64 fmt);
double msysc_m_read_r64(i64 fmt);
void msysc_m_read_str(byte * dest,i64 destlen,i64 fmt);
void msysc_readstr(byte * dest,i64 fmt,i64 destlen);
void msysc_rereadln(void);
void msysc_reread(void);
i64 msysc_valint(byte * s,i64 fmt);
double msysc_valreal(byte * s);
static void msysc_iconvlcn(byte * s,i64 n);
static void msysc_iconvucn(byte * s,i64 n);
static void msysc_convlcstring(byte * s);
static void msysc_convucstring(byte * s);
i64 msysc_m_power_i64(i64 n,i64 a);
void msysc_m_intoverflow(void);
void msysc_m_dotindex(u64 i,u64 a);
void msysc_m_dotslice(u64 j,u64 i,u64 a);
void msysc_m_popdotindex(u64 i,u64 * p,u64 x);
void msysc_m_popdotslice(u64 j,u64 i,u64 * p,u64 x);
i64 msysc_m_imin(i64 a,i64 b);
i64 msysc_m_imax(i64 a,i64 b);
double msysc_m_sign(double x);
void * mlib_pcm_alloc(i64 n);
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
i64 mlib_mrandom(void);
i64 mlib_mrandomp(void);
i64 mlib_mrandomint(i64 n);
i64 mlib_mrandomrange(i64 a,i64 b);
double mlib_mrandomreal(void);
byte * mlib_checkpackfile(void);
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
i64 mm_support_loadsourcefile(byte * filespec);
i64 mm_support_loadbuiltin(byte * shortfile,byte * text);
i64 mm_support_loadbundledfile(byte * filespec);
void mm_support_mcerror(byte * mess);
void mm_support_serror_gen(byte * mess);
static void mm_support_stopcompiler(byte * filename,i64 lineno);
void mm_support_serror(byte * mess);
void mm_support_serror_s(byte * mess,byte * a);
void mm_support_error_gen(i64 pass,byte * mess,struct mm_decls_unitrec * p);
void mm_support_rxerror(byte * mess,struct mm_decls_unitrec * p);
void mm_support_gerror(byte * mess,struct mm_decls_unitrec * p);
void mm_support_txerror(byte * mess,struct mm_decls_unitrec * p);
void mm_support_txerror_s(byte * mess,byte * a,struct mm_decls_unitrec * p);
void mm_support_txerror_ss(byte * mess,byte * a,byte * b);
void mm_support_rxerror_s(byte * mess,byte * a,struct mm_decls_unitrec * p);
void mm_support_gerror_s(byte * mess,byte * s,struct mm_decls_unitrec * p);
void mm_support_lxerror_gen(byte * mess);
void mm_support_lxerror_s(byte * mess,byte * a);
void mm_support_lxerror(byte * mess);
i64 mm_support_testelem(byte (*p)[],i64 n);
void mm_support_setelem(byte (*p)[],i64 n);
i64 mm_support_nextpoweroftwo(i64 x);
void mm_support_loaderror(byte * mess,byte * mess2,byte * mess3);
void mm_support_gs_additem(struct mlib_strbuffer * dest,byte * s);
void mm_support_gs_copytostr(struct mlib_strbuffer * source,byte * s);
i64 mm_support_isalphanum(i64 c);
void mm_support_inittypetables(void);
void mm_support_addspecialtypes(void);
static byte * mm_support_findfile(byte * filename);
byte * mm_support_findstdlib(byte * name);
i64 mm_support_getmainfile(byte * filename);
i64 mm_support_getmodulefile(byte * modulename,byte * ownername);
i64 mm_support_getsupportfile(byte * filename);
void mm_support_writemafile(byte * leadmodule,byte * destfile);
void mm_support_loadmafile(void);
static struct mm_decls_strec * mm_lib_newstrec(void);
void mm_lib_initqclib(void);
struct mm_decls_strec * mm_lib_getduplnameptr(struct mm_decls_strec * owner,struct mm_decls_strec * symptr,i64 id);
void mm_lib_adddef(struct mm_decls_strec * owner,struct mm_decls_strec * p);
void mm_lib_adddef_nodupl(struct mm_decls_strec * owner,struct mm_decls_strec * p);
struct mm_decls_unitrec * mm_lib_createname(struct mm_decls_strec * p);
struct mm_decls_unitrec * mm_lib_createunit0(i64 tag);
struct mm_decls_unitrec * mm_lib_createunit1(i64 tag,struct mm_decls_unitrec * p);
struct mm_decls_unitrec * mm_lib_createunit2(i64 tag,struct mm_decls_unitrec * p,struct mm_decls_unitrec * q);
struct mm_decls_unitrec * mm_lib_createunit3(i64 tag,struct mm_decls_unitrec * p,struct mm_decls_unitrec * q,struct mm_decls_unitrec * r);
void mm_lib_insertunit(struct mm_decls_unitrec * p,i64 tag);
void mm_lib_deleteunit(struct mm_decls_unitrec * p,struct mm_decls_unitrec * q);
struct mm_decls_unitrec * mm_lib_createconstunit(u64 a,i64 t);
struct mm_decls_unitrec * mm_lib_createstringconstunit(byte * s,i64 length);
i64 mm_lib_getoptocode(i64 opc);
i64 mm_lib_createtype(struct mm_decls_strec * d);
i64 mm_lib_createusertype(struct mm_decls_strec * stname);
i64 mm_lib_createusertypefromstr(byte * name);
i64 mm_lib_getconstvalue(struct mm_decls_unitrec * p,i64 id);
struct mm_decls_unitrec * mm_lib_getrangelwbunit(struct mm_decls_unitrec * p);
struct mm_decls_unitrec * mm_lib_getrangeupbunit(struct mm_decls_unitrec * p);
i64 mm_lib_createarraymode(struct mm_decls_strec * owner,i64 target,struct mm_decls_unitrec * dimexpr,i64 typedefx);
i64 mm_lib_createflexarraymode(struct mm_decls_strec * owner,i64 target,i64 typedefx);
i64 mm_lib_createarraymodek(struct mm_decls_strec * owner,i64 target,i64 lower,i64 length,i64 typedefx);
i64 mm_lib_createsetmode(struct mm_decls_strec * owner,struct mm_decls_unitrec * dimexpr,i64 typedefx);
i64 mm_lib_createsetmodek(struct mm_decls_strec * owner,i64 length,i64 typedefx);
i64 mm_lib_createdictmode(struct mm_decls_strec * owner,i64 target,i64 keymode,i64 typedefx);
byte * mm_lib_nextautotype(void);
void mm_lib_converttoslice(i64 t,i64 sltype);
i64 mm_lib_createslicemode(struct mm_decls_strec * owner,i64 target,struct mm_decls_unitrec * dimexpr,i64 typedefx);
i64 mm_lib_createslicemodek(struct mm_decls_strec * owner,i64 target,i64 lower,i64 typedefx);
i64 mm_lib_createstringmode(i64 t,i64 length,i64 typedefx);
i64 mm_lib_createrefmode(struct mm_decls_strec * owner,i64 target,i64 typedefx);
i64 mm_lib_createrefbitmode(struct mm_decls_strec * owner,i64 target,i64 typedefx);
i64 mm_lib_createsubrangemode(struct mm_decls_strec * owner,struct mm_decls_unitrec * prange,i64 typedefx);
i64 mm_lib_createrefprocmode(struct mm_decls_strec * owner,struct mm_decls_strec * stproc,struct mm_decls_strec * paramlist,i64 kwd,i64 prettype,i64 typedefx);
void mm_lib_setnameptr(struct mm_decls_unitrec * p);
byte * mm_lib_getdottedname(struct mm_decls_strec * p);
struct mm_decls_strec * mm_lib_getavname(struct mm_decls_strec * owner,i64 id);
void mm_lib_unionstr_clear(struct mm_decls_uflagsrec * u);
void mm_lib_unionstr_append(struct mm_decls_uflagsrec * u,i64 c);
void mm_lib_unionstr_concat(struct mm_decls_uflagsrec * u,struct mm_decls_uflagsrec * v);
i64 mm_lib_unionstr_last(struct mm_decls_uflagsrec * u);
void mm_lib_unionstr_copy(struct mm_decls_uflagsrec * u,struct mm_decls_uflagsrec * v);
void mm_lib_unionstr_print(struct mm_decls_uflagsrec * u);
i64 mm_lib_createrecordmode(struct mm_decls_strec * owner,i64 t,i64 typedefx);
i64 mm_lib_createenummode(struct mm_decls_strec * owner,i64 typedefx);
void mm_lib_convertstring(byte * s,byte * t);
struct mlib_strbuffer * mm_lib_strexpr(struct mm_decls_unitrec * p);
static void mm_lib_jeval(struct mlib_strbuffer * dest,struct mm_decls_unitrec * p);
byte * mm_lib_getopcjname(i64 opc);
byte * mm_lib_strmode(i64 m,i64 expand);
byte * mm_lib_strmode2(i64 m,i64 expand);
void mm_lib_istrmode(i64 m,i64 expand,byte * dest);
i64 mm_lib_countunits(struct mm_decls_unitrec * p);
struct mm_decls_strec * mm_lib_finddefstr(struct mm_decls_strec * owner,byte * name);
void mm_lib_addtoproclist(struct mm_decls_strec * d);
void mm_lib_addstatic(struct mm_decls_strec * d);
i64 mm_lib_newusertypex(struct mm_decls_strec * d,struct mm_decls_strec * e);
byte * mm_lib_typename(i64 m);
struct mm_decls_unitrec * mm_lib_allocunitrec(void);
struct mm_decls_strec * mm_lib_createdupldef(struct mm_decls_strec * owner,struct mm_decls_strec * symptr,i64 id);
struct mm_decls_strec * mm_lib_createnewmoduledef(struct mm_decls_strec * owner,struct mm_decls_strec * symptr);
void mm_lib_storemode(i64 id,struct mm_decls_strec * owner,i64 m,i32 * p);
struct mm_decls_unitrec * mm_lib_duplunit(struct mm_decls_unitrec * p,i64 lineno);
void mm_lib_addstr(byte * * p,byte * s);
void mm_lib_addchar(byte * * p,byte c);
void mm_lib_addint(byte * * p,i64 x);
i64 mm_lib_iscallbackfn(struct mm_decls_strec * p);
i64 mm_lib_isstringconst(struct mm_decls_unitrec * p);
i64 mm_lib_checkblockreturn(struct mm_decls_unitrec * p);
byte * mm_lib_strqvalue(struct mm_decls_qint * aa);
struct mm_decls_qint * mm_lib_makeqvalue(i64 a,i64 scat);
struct mm_decls_qint * mm_lib_makeqvalue_ab(i64 a,i64 b);
i64 mm_lib_isconstint(struct mm_decls_unitrec * a);
i64 mm_lib_isconstunit(struct mm_decls_unitrec * a);
byte * mm_lib_getfullname(struct mm_decls_strec * d,i64 fromassem);
void mm_lib_getownername(struct mm_decls_strec * d,byte * dest);
i64 mm_lib_isintmode(i64 m);
i64 mm_lib_isnumericmode(i64 m);
i64 mm_lib_isrefmode(i64 m);
byte * mm_lib_strconstopnd(struct mm_decls_unitrec * p);
i64 mm_lib_gettypecat_t(i64 m);
i64 mm_lib_getalignment(i64 m);
i64 mm_lib_ispoweroftwo(i64 x);
void mm_lib_addlistunit(struct mm_decls_unitrec * * ulist,struct mm_decls_unitrec * * ulistx,struct mm_decls_unitrec * p);
i64 mm_lib_issimpletype(i64 m);
i64 mm_lib_getpacktype(i64 m);
void mm_lex_lexreadtoken(void);
static void mm_lex_lxreadstring(i64 termchar);
static void mm_lex_readnumber(i64 base);
static void mm_lex_readdecimalnumber(void);
static void mm_lex_readrealnumber(byte * intstart,i64 intlen,i64 base);
static double mm_lex_readrealbest(i64 intlen,i64 fractlen,i64 expon,byte * realstr);
static i64 mm_lex_readexponent(i64 base);
void mm_lex_printsymbol(struct mm_decls_lexrec * lp);
static void mm_lex_stringtonumber(byte * s,i64 length,i64 base);
static void mm_lex_stringtodecimalnumber(byte * s,i64 length,i64 suffix);
void mm_lex_lexsetup(void);
void mm_lex_printstrn(byte * s,i64 length);
static byte * mm_lex_scannumber(i64 base);
static void mm_lex_readrawstring(void);
static i64 mm_lex_lookup(void);
i64 mm_lex_gethashvaluez(byte * s);
static void mm_lex_inithashtable(void);
static i64 mm_lex_dolexdirective(i64 index);
static void mm_lex_lexreadline(void);
void mm_lex_startlex(byte * caption,i64 fileno);
byte * mm_lex_convertzstring(byte * s,i64 length);
struct mm_decls_strec * mm_lex_addnamestr(byte * name);
void mm_lex_ps1(byte * caption);
void mm_lex_ps2(byte * caption);
void mm_lex_ps(byte * caption);
void mm_lex_lex(void);
void mm_lex_showhashtablesize(void);
i64 mm_lex_checkname(byte * name,i64 length);
static byte * mm_lex_getstrfile(byte * filename,i32 * length);
static void mm_lex_stacksourcefile(byte * file,i64 ismainmodule);
static void mm_lex_stacksource(byte * sptr,i64 fileno,i64 isfile);
static void mm_lex_unstacksource(void);
static void mm_lex_readarraystring(i64 prefix);
static void mm_lex_qadd(struct mm_decls_qint * aa,struct mm_decls_qint * bb);
static void mm_lex_qadddigit(struct mm_decls_qint * aa,i64 x);
static void mm_lex_qshift(struct mm_decls_qint * aa,i64 n);
static void mm_lex_qmul10(struct mm_decls_qint * aa);
static void mm_lex_qmulbase(struct mm_decls_qint * aa,i64 base);
static struct mm_decls_qint * mm_lex_stringtonumber128(byte * s,i64 length,i64 base);
static i64 mm_lex_setinttype(u64 a);
void mm_diags_printmodelist(void * f);
void mm_diags_printst(void * f,struct mm_decls_strec * p,i64 level);
static void mm_diags_printstrec(void * f,struct mm_decls_strec * p,i64 level);
void mm_diags_printstflat(void * f);
void mm_diags_printcode(void * f,byte * caption);
void mm_diags_printmodulecode(void * f,struct mm_decls_strec * m);
void mm_diags_printunit(struct mm_decls_unitrec * p,i64 level,byte * prefix,void * dev);
static void mm_diags_printunitlist(void * dev,struct mm_decls_unitrec * p,i64 level,byte * prefix);
static byte * mm_diags_getprefix(i64 level,byte * prefix,struct mm_decls_unitrec * p);
static byte * mm_diags_getlineinfok(void);
void mm_genc64_do_codegen1(byte * outfilesource,i64 fshowpcl1);
void mm_genc64_do_codegen2(i64 fshowmcl1);
void mm_genc64_do_writesource(byte * outfilesource,i64 dontlink);
void mm_genc64_do_link(byte * cfile,byte * exefile,byte * linkoption,i64 ccompiler,i64 optimise);
void mm_genc64_do_link_win(byte * cfile,byte * exefile,byte * linkoption,i64 ccompiler,i64 optimise);
void mm_genc64_do_link_lin(byte * cfile,byte * exefile,byte * linkoption,i64 ccompiler,i64 optimise);
void mm_genc64_showhelp(void);
i64 mm_genc_codegen_clang(byte * cfilename);
static void mm_genc_do_infoheader(byte * cfilename);
static void mm_genc_do_cheader(byte * cfilename);
static void mm_genc_do_alltypes(void);
static void mm_genc_do_allprocdecls(void);
static void mm_genc_do_allvars(void);
static void mm_genc_do_allprocdefs(void);
static void mm_genc_do_typedef_fwd(struct mm_decls_strec * d);
static void mm_genc_do_typedef(struct mm_decls_strec * d);
static void mm_genc_do_procdecl(struct mm_decls_strec * d);
static void mm_genc_do_vardef(struct mm_decls_strec * d);
static void mm_genc_do_dllvar(struct mm_decls_strec * d);
static void mm_genc_addsymbol(struct mm_decls_strec * d);
static void mm_genc_scansymbol(struct mm_decls_strec * d);
static void mm_genc_genlocalvar(struct mm_decls_strec * d);
static void mm_genc_genprocdef(struct mm_decls_strec * p);
static void mm_genc_writefn_nprocs(void);
static void mm_genc_writefn_nexports(void);
static void mm_genc_writefn_names(void);
static void mm_genc_writefn_addresses(void);
static void mm_genc_writefn_exports(void);
static void mm_libc_Dinit(void);
void mm_libc_cccomment(byte * s);
void mm_libc_ccblank(void);
void mm_libc_cclinecomment(byte * s);
void mm_libc_ccchar(i64 c);
void mm_libc_cctab(i64 level);
void mm_libc_ccstr(byte * s,i64 level);
void mm_libc_ccstrline(byte * cstr);
void mm_libc_ccstrsemi(byte * cstr);
void mm_libc_ccstrsemiu(struct mm_decls_unitrec * p);
void mm_libc_ccsendline(void);
void mm_libc_ccint(i64 a);
void mm_libc_ccinitline(void);
byte * mm_libc_strmodec(i64 m,byte * name,i64 addtab);
void mm_libc_strmodec2(i64 m,byte * name,i64 addtab);
static byte * mm_libc_strmodex(i64 m);
byte * mm_libc_strprocsig(struct mm_decls_strec * p,byte * name,i64 showparamnames);
byte * mm_libc_getprocname(struct mm_decls_strec * d);
byte * mm_libc_getfullnamec(struct mm_decls_strec * d);
byte * mm_libc_getfullnamec2(struct mm_decls_strec * d);
byte * mm_libc_genclabel(i64 n,i64 colon);
void mm_libc_genrecorddef(i64 m);
void mm_libc_genrecordfwd(i64 m);
void mm_libc_do_initdata(struct mm_decls_unitrec * p,i64 docomma,i64 level);
void mm_libc_cclongstr(byte * svalue,i64 length);
static void mm_libc_do_makelist(struct mm_decls_unitrec * a,i64 length,i64 docomma,i64 level);
i64 mm_libc_issimplec(struct mm_decls_unitrec * p);
byte * mm_libc_strstringc(byte * s,i64 length);
void mm_libc_do_syscallproc(byte * fnname,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_libc_evalsysparam(struct mm_decls_unitrec * a);
void mm_libc_dxstr(byte * str);
void mm_libc_dxchar(i64 ch);
void mm_libc_dxint(i64 a);
void mm_blockc_evalunit(struct mm_decls_unitrec * p);
void mm_blockc_do_block(struct mm_decls_unitrec * p);
void mm_blockc_do_blocklab(struct mm_decls_unitrec * p,i64 lab1,i64 lab2);
void mm_blockc_evalblock(struct mm_decls_unitrec * p);
void mm_blockc_evalblocklab(struct mm_decls_unitrec * p,i64 lab1,i64 lab2);
void mm_blockc_evalstmt(struct mm_decls_unitrec * p);
static void mm_blockc_loneexpr(struct mm_decls_unitrec * p);
static void mm_blockc_do_const(struct mm_decls_unitrec * p);
static void mm_blockc_do_bin(i64 opc,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_blockc_do_binto(i64 opc,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_blockc_do_unary(byte * opc,struct mm_decls_unitrec * a);
static void mm_blockc_do_if(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,struct mm_decls_unitrec * c);
static void mm_blockc_do_longif(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_blockc_do_call(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_blockc_do_goto(struct mm_decls_unitrec * a);
static void mm_blockc_do_do(struct mm_decls_unitrec * a);
static i64 mm_blockc_definelabel(void);
static i64 mm_blockc_createfwdlabel(void);
static void mm_blockc_definefwdlabel(i64 lab);
static void mm_blockc_stacklooplabels(i64 a,i64 b,i64 c,i64 d);
static void mm_blockc_unstacklooplabels(void);
static i64 mm_blockc_findlooplabel(i64 k,i64 n);
static void mm_blockc_do_exit(struct mm_decls_unitrec * p);
static void mm_blockc_do_to(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,struct mm_decls_unitrec * c);
static void mm_blockc_do_while(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_blockc_do_repeat(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_blockc_do_forup(struct mm_decls_unitrec * p,struct mm_decls_unitrec * ivar,struct mm_decls_unitrec * pbody,struct mm_decls_unitrec * pautovar);
static void mm_blockc_do_return(struct mm_decls_unitrec * a);
static void mm_blockc_do_print(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_blockc_do_read(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_blockc_do_readln(struct mm_decls_unitrec * a);
static void mm_blockc_do_convert(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_blockc_do_index(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * pindex);
static void mm_blockc_do_dot(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_blockc_do_swap(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_blockc_do_case(struct mm_decls_unitrec * p,struct mm_decls_unitrec * pindex,struct mm_decls_unitrec * pwhenthen,struct mm_decls_unitrec * pelse);
static void mm_blockc_do_switch(struct mm_decls_unitrec * p,struct mm_decls_unitrec * pindex,struct mm_decls_unitrec * pwhenthen,struct mm_decls_unitrec * pelse);
static void mm_blockc_do_max(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_blockc_do_maxto(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_blockc_do_blockcopy(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_blockc_do_select(struct mm_decls_unitrec * pindex,struct mm_decls_unitrec * plist,struct mm_decls_unitrec * pdefault);
static void mm_blockc_do_maths(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_blockc_do_maths2(byte * name,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_blockc_do_ifx(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,struct mm_decls_unitrec * c,i64 addrof);
static void mm_blockc_do_exprlist(struct mm_decls_unitrec * a);
static void mm_blockc_do_inrange(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_blockc_do_inset(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static i64 mm_blockc_isexpr(struct mm_decls_unitrec * p);
static void mm_blockc_dxlabel(i64 lab);
i64 mm_parse_parsemodule(i64 n);
i64 mm_parse_readmoduledefs(struct mm_decls_strec * owner);
static void mm_parse_initparser(void);
static void mm_parse_skipsemi(void);
static struct mm_decls_unitrec * mm_parse_makeblock(struct mm_decls_unitrec * p);
static struct mm_decls_unitrec * mm_parse_makestmtblock(struct mm_decls_unitrec * p);
static void mm_parse_checkequals(void);
static i64 mm_parse_getcurrline(void);
static i64 mm_parse_checkbegin(i64 fbrack);
static void mm_parse_checkbeginend(i64 closesym,i64 kwd,i64 startline);
static void mm_parse_checkend(i64 endsym,i64 endkwd1,i64 endkwd2,i64 startline);
static struct mm_decls_unitrec * mm_parse_readvardef(struct mm_decls_strec * owner,i64 isglobal,i64 isstatic,i64 varid);
static void mm_parse_readconstdef(struct mm_decls_strec * owner,i64 isglobal);
static struct mm_decls_unitrec * mm_parse_readlbrack(void);
static void mm_parse_addlistparam(struct mm_decls_strec * * ulist,struct mm_decls_strec * * ulistx,struct mm_decls_strec * p);
static struct mm_decls_unitrec * mm_parse_readcast(i64 t);
static struct mm_decls_unitrec * mm_parse_readopc(void);
static struct mm_decls_unitrec * mm_parse_readsprint(void);
static struct mm_decls_unitrec * mm_parse_readsread(void);
static struct mm_decls_unitrec * mm_parse_readcompilervar(void);
static struct mm_decls_unitrec * mm_parse_readcastx(void);
void mm_parse_checksymbol(i64 symbol);
static i64 mm_parse_readtypespec(struct mm_decls_strec * owner,i64 typedefx);
static i64 mm_parse_readslicetype(struct mm_decls_strec * owner,i64 typedefx);
static i64 mm_parse_readflexarray(struct mm_decls_strec * owner,i64 typedefx);
static struct mm_decls_unitrec * mm_parse_readslist(i64 iscall,i64 donulls);
static struct mm_decls_unitrec * mm_parse_readindex(struct mm_decls_unitrec * p,i64 dot);
static struct mm_decls_unitrec * mm_parse_readdotsuffix(struct mm_decls_unitrec * p);
i64 mm_parse_isconstexpr(struct mm_decls_unitrec * p);
static struct mm_decls_unitrec * mm_parse_readkeyindex(struct mm_decls_unitrec * p);
static struct mm_decls_unitrec * mm_parse_readconstexpr(i64 needconst);
static i64 mm_parse_readconstint(void);
static void mm_parse_readprocdef(struct mm_decls_strec * procowner,i64 isglobal,i64 fflang);
struct mm_decls_strec * mm_parse_readprocdecl(struct mm_decls_strec * procowner,i64 isglobal,i64 fflang);
static struct mm_decls_strec * mm_parse_readparams(struct mm_decls_strec * procowner,struct mm_decls_strec * owner,i64 fflang,i64 * varparams,i64 * nparams);
static struct mm_decls_strec * mm_parse_readparams_types(struct mm_decls_strec * procowner,struct mm_decls_strec * owner,i64 fflang,i64 * varparams,i64 * nparams,i64 pmode);
static struct mm_decls_unitrec * mm_parse_readcondsuffix(struct mm_decls_unitrec * p);
static struct mm_decls_unitrec * mm_parse_readif(void);
static struct mm_decls_unitrec * mm_parse_readgoto(i64 gototag);
static struct mm_decls_unitrec * mm_parse_readunless(void);
static struct mm_decls_unitrec * mm_parse_readswitchcase(void);
static struct mm_decls_unitrec * mm_parse_readstop(void);
static struct mm_decls_unitrec * mm_parse_readreturn(void);
static struct mm_decls_unitrec * mm_parse_readdo(void);
static struct mm_decls_unitrec * mm_parse_readto(void);
static struct mm_decls_unitrec * mm_parse_readwhile(void);
static struct mm_decls_unitrec * mm_parse_readrepeat(void);
static struct mm_decls_unitrec * mm_parse_readloopcontrol(void);
static struct mm_decls_unitrec * mm_parse_readprint(void);
static struct mm_decls_unitrec * mm_parse_readread(void);
static struct mm_decls_unitrec * mm_parse_readtry(void);
static struct mm_decls_unitrec * mm_parse_readraise(void);
static struct mm_decls_unitrec * mm_parse_readfor(void);
static struct mm_decls_unitrec * mm_parse_readforall(void);
void mm_parse_readtypedef(struct mm_decls_strec * owner,i64 isglobal);
void mm_parse_readrecordfields(struct mm_decls_strec * owner,i64 m);
void mm_parse_readtabledef(i64 isglobal);
void mm_parse_readclassdef(struct mm_decls_strec * owner,i64 isglobal);
static void mm_parse_readclassbody(struct mm_decls_strec * owner,i64 classkwd);
static i64 mm_parse_readenumtype(struct mm_decls_strec * owner,i64 typedefx,i64 isglobal);
static void mm_parse_duplfield(struct mm_decls_strec * owner,struct mm_decls_strec * p,struct mm_decls_strec * q);
static void mm_parse_readimportmodule(struct mm_decls_strec * owner);
static void mm_parse_readimportbody(struct mm_decls_strec * owner);
static struct mm_decls_strec * mm_parse_readequivfield(struct mm_decls_strec * owner);
static struct mm_decls_unitrec * mm_parse_readapplyop(i64 inexpr);
static i64 mm_parse_readrefproc(struct mm_decls_strec * owner,i64 typedefx,i64 fflang);
static void mm_parse_pushproc(struct mm_decls_strec * p);
static void mm_parse_popproc(void);
static struct mm_decls_unitrec * mm_parse_readassemline(void);
static struct mm_decls_unitrec * mm_parse_readassemblock(void);
static struct mm_decls_unitrec * mm_parse_assembleline(void);
static struct mm_decls_unitrec * mm_parse_makeastring(void);
static i64 mm_parse_readreturntype(struct mm_decls_strec * owner,i64 (*retmodes)[]);
static struct mm_decls_unitrec * mm_parse_readset(void);
static i64 mm_parse_istypestarter(void);
static struct mm_decls_unitrec * mm_parse_readunit(void);
static struct mm_decls_unitrec * mm_parse_readfactor(i64 level);
static struct mm_decls_unitrec * mm_parse_readterm2(void);
static struct mm_decls_unitrec * mm_parse_readterm(void);
static struct mm_decls_unitrec * mm_parse_readxunit(void);
static struct mm_decls_unitrec * mm_parse_readsunit(i64 inwhile);
static void mm_parse_readmacrodef(struct mm_decls_strec * owner,i64 isglobal);
static void mm_parse_readimportalias(struct mm_decls_strec * dimport);
static void mm_parse_domappedalias(struct mm_decls_strec * dimport,struct mm_decls_strec * stimport);
static struct mm_decls_unitrec * mm_parse_readrecase(void);
void mm_name_rx_unit(struct mm_decls_strec * owner,struct mm_decls_unitrec * p);
i64 mm_name_rx_module(i64 n);
void mm_name_rx_deflist(struct mm_decls_strec * owner,struct mm_decls_strec * p);
void mm_name_rx_passdef(struct mm_decls_strec * owner,struct mm_decls_strec * p);
static void mm_name_rx_unitlist(struct mm_decls_strec * owner,struct mm_decls_unitrec * p);
struct mm_decls_strec * mm_name_resolvetopname(struct mm_decls_strec * owner,struct mm_decls_strec * stnewname,i64 moduleno,i64 fmodule);
void mm_name_resolvename(struct mm_decls_strec * owner,struct mm_decls_unitrec * p,i64 mode);
struct mm_decls_strec * mm_name_finddupl(struct mm_decls_strec * d,struct mm_decls_strec * pdupl);
static void mm_name_resolvedot(struct mm_decls_strec * owner,struct mm_decls_unitrec * p);
static void mm_name_fixmode(struct mm_decls_strec * owner,struct mm_decls_strec * p);
static i64 mm_name_fixmode2(struct mm_decls_strec * owner,i64 m);
void mm_name_fixusertypes(void);
static void mm_name_rx_assem(struct mm_decls_strec * owner,struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
struct mm_decls_strec * mm_name_resolve_equiv_name(struct mm_decls_strec * owner,struct mm_decls_strec * p);
static struct mm_decls_strec * mm_name_addframevar(struct mm_decls_strec * owner,struct mm_decls_strec * d,i64 moduleno,i64 mode);
static void mm_name_converteqeq(struct mm_decls_strec * owner,struct mm_decls_unitrec * p);
static struct mm_decls_unitrec * mm_name_copylistunit(struct mm_decls_unitrec * p);
static struct mm_decls_unitrec * mm_name_copyunit(struct mm_decls_unitrec * p);
static void mm_name_replaceunit(struct mm_decls_unitrec * p,struct mm_decls_unitrec * q);
static void mm_name_expandmacro(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tpass(struct mm_decls_unitrec * p,i64 t,i64 lv);
static void mm_type_tx_block(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t,i64 lv);
void mm_type_tx_typetable(void);
static void mm_type_setmodesize(i64 m);
static void mm_type_setarraysize(i64 m);
static void mm_type_setslicesize(i64 m);
static void mm_type_tcond(struct mm_decls_unitrec * p);
i64 mm_type_tx_module(i64 n);
void mm_type_tx_passdef(struct mm_decls_strec * p);
static void mm_type_tx_unitlist(struct mm_decls_unitrec * p,i64 t,i64 lv);
static void mm_type_tx_namedef(struct mm_decls_strec * d);
void mm_type_tx_namedconst(struct mm_decls_strec * d);
static void mm_type_tx_expr(struct mm_decls_unitrec * p,i64 t);
static void mm_type_checkconstexpr(struct mm_decls_unitrec * p);
static i64 mm_type_getconstint(struct mm_decls_unitrec * q,i64 t);
static void mm_type_tevaluate(struct mm_decls_unitrec * p);
static void mm_type_tevalbinop(struct mm_decls_unitrec * p);
static void mm_type_tevalmonop(struct mm_decls_unitrec * p);
static void mm_type_tevalconvert(struct mm_decls_unitrec * p);
static void mm_type_makenewconst(struct mm_decls_unitrec * p,i64 x,i64 t);
static void mm_type_tx_name(struct mm_decls_unitrec * p,i64 t,i64 lv);
static void mm_type_getdominantmode(i64 tag,i64 s,i64 t,i64 * u,i64 * v);
static void mm_type_getdominantmodepp(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,i64 * u,i64 * v);
static void mm_type_coerceunit(struct mm_decls_unitrec * p,i64 t,i64 hard);
static void mm_type_tx_add(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_mul(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_shl(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_iand(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_eq(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_lt(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_callproc(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * pargs,i64 t);
static void mm_type_tx_neg(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_if(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,struct mm_decls_unitrec * c,i64 t,i64 lv);
static void mm_type_tx_longif(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,i64 t,i64 lv);
static void mm_type_tx_preincr(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t);
static void mm_type_tx_for(struct mm_decls_unitrec * a,struct mm_decls_unitrec * pbody,struct mm_decls_unitrec * c);
static void mm_type_tx_index(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,i64 t,i64 lv);
static void mm_type_tx_makerange(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_makeset(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t);
static void mm_type_tx_makedict(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t);
static void mm_type_tx_ptr(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t,i64 lv);
static void mm_type_setrecordsize(i64 m);
static void mm_type_scanrecord(i64 state,struct mm_decls_strec * (*fields)[],i64 * index,i64 * isize,i64 offset);
static void mm_type_tx_convert(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 hard);
static void mm_type_tx_makelist(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t,i64 lv);
static void mm_type_tx_dot(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,i64 lv);
static struct mm_decls_strec * mm_type_resolvefield(struct mm_decls_strec * d,i64 m);
static i64 mm_type_comparemodes(i64 s,i64 t);
static i64 mm_type_isboolunit(struct mm_decls_unitrec * p);
static void mm_type_checkbool(i64 m);
static void mm_type_tx_andl(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_convintconst(struct mm_decls_unitrec * p,i64 x);
static void mm_type_tx_upb(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_type_tx_len(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_type_tx_lwb(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_type_tx_bounds(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_type_tx_sliceptr(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_type_tx_inot(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_type_tx_atan2(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_swap(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_sqrt(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_type_tx_select(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,struct mm_decls_unitrec * c,i64 t,i64 lv);
static void mm_type_tx_case(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,struct mm_decls_unitrec * c,i64 t,i64 lv);
static void mm_type_tx_notl(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_type_tx_istruel(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_type_tx_addto(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_iandto(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_negto(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_type_tx_typepun(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_type_tx_bytesize(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_type_tx_exit(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_type_tx_goto(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_type_tx_switch(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,struct mm_decls_unitrec * c,i64 t,i64 lv);
static void mm_type_tx_power(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_addroffirst(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t);
static void mm_type_tx_minvalue(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_type_tx_return(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t);
static void mm_type_tx_dotindex(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,i64 lv);
static void mm_type_tx_slice(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_assign(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,i64 t);
static void mm_type_tx_multassign(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_in(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static struct mm_decls_strec * mm_type_getprocretmodes(struct mm_decls_unitrec * p);
static void mm_type_tx_exprlist(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t);
static void mm_type_tx_sign(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a);
static void mm_type_fixvoidunit(struct mm_decls_unitrec * a);
static void mm_type_twiden(struct mm_decls_unitrec * p,i64 lv);
static i64 mm_type_twidenshort(struct mm_decls_unitrec * p);
static void mm_type_tx_head(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_concat(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_twidenopnd(struct mm_decls_unitrec * p);
static void mm_type_joinstrings(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_removeaddrof(struct mm_decls_unitrec * p);
static void mm_type_twholearrayslice(struct mm_decls_unitrec * p,i64 slicemode);
static void mm_type_tarrayslice(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b);
static void mm_type_tx_bitfield(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 lv);

/* VARS */
i64 mm_decls_ntypes;
i64 mm_decls_nuserxtypes;
i64 mm_decls_userxtypebase;
struct mm_decls_userxrec *  mm_decls_userxmodelist;
i32 mm_decls_ttmodule[4000];
struct mm_decls_strec *  mm_decls_ttnamedef[4000];
struct mm_decls_strec *  mm_decls_ttowner[4000];
i32 mm_decls_ttbasetype[4000];
byte *  mm_decls_ttname[4000];
byte mm_decls_tttypecat[4000];
byte mm_decls_ttbitwidth[4000];
i32 mm_decls_ttsize[4000];
byte mm_decls_ttsizeset[4000];
i32 mm_decls_ttlower[4000];
i32 mm_decls_ttlength[4000];
struct mm_decls_unitrec *  mm_decls_ttdimexpr[4000];
i32 mm_decls_tttarget[4000];
i32 mm_decls_ttkeymode[4000];
byte mm_decls_ttusercat[4000];
i32 mm_decls_ttlineno[4000];
byte mm_decls_ttisint[4000];
byte mm_decls_ttisword[4000];
byte mm_decls_ttischar[4000];
byte mm_decls_ttiswordchar[4000];
byte mm_decls_ttisreal[4000];
byte mm_decls_ttisinteger[4000];
byte mm_decls_ttisnumeric[4000];
byte mm_decls_ttisshortint[4000];
byte mm_decls_ttisshortreal[4000];
byte mm_decls_ttisbit[4000];
byte mm_decls_ttisref[4000];
byte mm_decls_tttypecode[4000];
byte mm_decls_ttisflex[4000];
byte mm_decls_ttisvariant[4000];
byte mm_decls_ttisflexvar[4000];
byte mm_decls_typestarterset[4000];
struct mm_decls_strec *  mm_decls_ttnamedefx[4000];
struct mm_decls_strec *  mm_decls_ttnamedefx2[4000];
i64 mm_decls_ttlinenox[4000];
i64 mm_decls_ttxmap[4000];
byte mm_decls_ttxmoduleno[4000];
byte mm_decls_exprstarterset[145];
struct mm_decls_modulerec mm_decls_moduletable[51];
byte *  mm_decls_inputfiles[51];
byte *  mm_decls_libfiles[51];
byte *  mm_decls_sourcefilenames[251];
byte *  mm_decls_sourcefilepaths[251];
byte *  mm_decls_sourcefiletext[251];
i64 mm_decls_sourcefilesizes[251];
i64 mm_decls_nmodules;
i64 mm_decls_nsourcefiles;
i64 mm_decls_ninputfiles;
i64 mm_decls_nlibfiles;
byte *  mm_decls_mafilenames[251];
i64 mm_decls_mafilesizes[251];
i64 mm_decls_mafileoffsets[251];
byte *  mm_decls_mafiletext[251];
byte mm_decls_mafilefileno[251];
byte mm_decls_mafilemult[251];
i64 mm_decls_nmafiles;
byte *  mm_decls_mafilesource;
struct mm_decls_strec *  mm_decls_currmodule;
i64 mm_decls_currmoduleno;
byte *  mm_decls_searchdirs[10];
i64 mm_decls_nsearchdirs = (i64)0;
struct mm_decls_strec *  mm_decls_stprogram;
struct mm_decls_strec *  mm_decls_stmodule;
struct mm_decls_strec *  mm_decls_stsysmodule;
struct mm_decls_strec *  mm_decls_alldeflist;
i64 mm_decls_optflag = (i64)0;
struct mm_decls_unitrec *  mm_decls_nullunit;
byte *  mm_decls_libpaths[10];
i64 mm_decls_nlibpaths;
i64 mm_decls_mlineno = (i64)0;
i64 mm_decls_debug = (i64)0;
i64 mm_decls_assemmode = (i64)0;
i64 mm_decls_totalstrings = (i64)0;
i64 mm_decls_ndlltable;
i64 mm_decls_ndllproctable;
byte *  mm_decls_dlltable[50];
u64 mm_decls_dllinsttable[50];
struct mm_decls_dllprocrec mm_decls_dllproctable[500];
i64 mm_decls_ncmdparams;
byte *  mm_decls_cmdparamtable[33];
struct mm_decls_procrec *  mm_decls_proclist;
i64 mm_decls_nproclist;
struct mm_decls_procrec *  mm_decls_staticlist;
i64 mm_decls_nstaticlist;
struct mm_decls_strec *  mm_decls_currproc;
byte *  mm_decls_modenames[3] = {(byte*)"compile_mode",(byte*)"link_mode",(byte*)"run_mode"};
byte *  mm_decls_targetnames[6] = {(byte*)"wx64",(byte*)"wc64",(byte*)"lx64",(byte*)"lc64",(byte*)"lc32",(byte*)"wc32"};
byte *  mm_decls_targetosnames[6] = {(byte*)"windows",(byte*)"windows",(byte*)"linux",(byte*)"linux",(byte*)"linux",(byte*)"windows"};
byte *  mm_decls_targetlangnames[6] = {(byte*)"x64",(byte*)"clang",(byte*)"x64",(byte*)"clang",(byte*)"clang",(byte*)"clang"};
byte *  mm_decls_targetexts[6] = {(byte*)"asm",(byte*)"c",(byte*)"asm",(byte*)"c",(byte*)"c",(byte*)"c"};
byte mm_decls_tg_ctarget[6] = {(u8)0u,(u8)1u,(u8)0u,(u8)1u,(u8)1u,(u8)1u};
byte mm_decls_tg_targetbits[6] = {(u8)64u,(u8)64u,(u8)64u,(u8)64u,(u8)32u,(u8)32u};
byte mm_decls_tg_islinux[6] = {(u8)0u,(u8)0u,(u8)1u,(u8)1u,(u8)1u,(u8)0u};
byte *  mm_decls_ccnames[3] = {(byte*)"gcc_cc",(byte*)"tcc_cc",(byte*)"bcc_cc"};
i64 mm_decls_passlevel;
i64 mm_decls_target;
i64 mm_decls_ctarget = (i64)0;
i64 mm_decls_islinux = (i64)0;
i64 mm_decls_targetbits;
i64 mm_decls_targetsize;
i64 mm_decls_fverbose = (i64)0;
i64 mm_decls_fquiet = (i64)0;
byte mm_decls_fdebugcompiler;
i64 mm_decls_foptimise = (i64)0;
i64 mm_decls_fhpcounter = (i64)0;
byte mm_decls_fnomsys;
i64 mm_decls_fvarnames = (i64)0;
i64 mm_decls_fbundled = (i64)0;
byte *  mm_decls_mafilename;
i64 mm_decls_fwritema;
i64 mm_decls_fcheckunusedlocals = (i64)0;
i64 mm_decls_dointlibs = (i64)1;
struct mm_decls_lexrec mm_decls_lx;
struct mm_decls_lexrec mm_decls_nextlx;
i64 mm_decls_labelno = (i64)0;
byte *  mm_decls_infotext;
i64 mm_decls_nalllines;
i64 mm_decls_prescanmode;
byte *  mm_decls_genericmodules[25];
byte *  mm_decls_actualmodules[25];
i64 mm_decls_nmodulemap;
byte *  mm_decls_cclibtable[10];
i64 mm_decls_ncclibs;
byte *  mm_tables_stdtypenames[57] = {
    (byte*)"void",
    (byte*)"i8",
    (byte*)"i16",
    (byte*)"i32",
    (byte*)"i64",
    (byte*)"i128",
    (byte*)"u8",
    (byte*)"u16",
    (byte*)"u32",
    (byte*)"u64",
    (byte*)"u128",
    (byte*)"r32",
    (byte*)"r64",
    (byte*)"c8",
    (byte*)"c16",
    (byte*)"c64",
    (byte*)"dec",
    (byte*)"u1",
    (byte*)"u2",
    (byte*)"u4",
    (byte*)"ref",
    (byte*)"tany",
    (byte*)"refb",
    (byte*)"subrng",
    (byte*)"enum",
    (byte*)"bf",
    (byte*)"cx",
    (byte*)"range",
    (byte*)"sx",
    (byte*)"ax",
    (byte*)"bx",
    (byte*)"set",
    (byte*)"rec",
    (byte*)"class",
    (byte*)"str",
    (byte*)"wstr",
    (byte*)"fax",
    (byte*)"fbits",
    (byte*)"ftab",
    (byte*)"fset",
    (byte*)"dict",
    (byte*)"proc",
    (byte*)"d124",
    (byte*)"x4",
    (byte*)"d8",
    (byte*)"x8",
    (byte*)"d16",
    (byte*)"x16",
    (byte*)"blk",
    (byte*)"flex",
    (byte*)"var",
    (byte*)"tgeneric",
    (byte*)"tauto",
    (byte*)"ttype",
    (byte*)"lab",
    (byte*)"mult",
    (byte*)"tlast"
};
byte mm_tables_stdtypebits[57] = {
    (u8)0u,
    (u8)8u,
    (u8)16u,
    (u8)32u,
    (u8)64u,
    (u8)128u,
    (u8)8u,
    (u8)16u,
    (u8)32u,
    (u8)64u,
    (u8)128u,
    (u8)32u,
    (u8)64u,
    (u8)8u,
    (u8)16u,
    (u8)64u,
    (u8)64u,
    (u8)1u,
    (u8)2u,
    (u8)4u,
    (u8)64u,
    (u8)0u,
    (u8)128u,
    (u8)64u,
    (u8)64u,
    (u8)8u,
    (u8)128u,
    (u8)128u,
    (u8)128u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)64u,
    (u8)64u,
    (u8)64u,
    (u8)64u,
    (u8)64u,
    (u8)64u,
    (u8)64u,
    (u8)64u,
    (u8)0u,
    (u8)64u,
    (u8)64u,
    (u8)64u,
    (u8)64u,
    (u8)128u,
    (u8)128u,
    (u8)0u,
    (u8)64u,
    (u8)128u,
    (u8)0u,
    (u8)0u,
    (u8)32u,
    (u8)0u,
    (u8)0u,
    (u8)0u
};
byte mm_tables_stdtypecode[57] = {
    (u8)0u,
    (u8)73u,
    (u8)73u,
    (u8)73u,
    (u8)73u,
    (u8)73u,
    (u8)85u,
    (u8)85u,
    (u8)85u,
    (u8)85u,
    (u8)85u,
    (u8)82u,
    (u8)82u,
    (u8)85u,
    (u8)85u,
    (u8)85u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)80u,
    (u8)0u,
    (u8)80u,
    (u8)73u,
    (u8)73u,
    (u8)85u,
    (u8)0u,
    (u8)0u,
    (u8)65u,
    (u8)65u,
    (u8)65u,
    (u8)65u,
    (u8)0u,
    (u8)0u,
    (u8)65u,
    (u8)65u,
    (u8)65u,
    (u8)65u,
    (u8)65u,
    (u8)65u,
    (u8)65u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)65u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u
};
byte mm_tables_stdtypecat[57] = {
    (u8)0u,
    (u8)42u,
    (u8)42u,
    (u8)42u,
    (u8)44u,
    (u8)46u,
    (u8)42u,
    (u8)42u,
    (u8)42u,
    (u8)44u,
    (u8)46u,
    (u8)42u,
    (u8)44u,
    (u8)42u,
    (u8)42u,
    (u8)44u,
    (u8)49u,
    (u8)42u,
    (u8)42u,
    (u8)42u,
    (u8)44u,
    (u8)0u,
    (u8)46u,
    (u8)44u,
    (u8)44u,
    (u8)42u,
    (u8)46u,
    (u8)46u,
    (u8)46u,
    (u8)48u,
    (u8)48u,
    (u8)48u,
    (u8)48u,
    (u8)49u,
    (u8)49u,
    (u8)49u,
    (u8)49u,
    (u8)49u,
    (u8)49u,
    (u8)49u,
    (u8)49u,
    (u8)41u,
    (u8)42u,
    (u8)43u,
    (u8)44u,
    (u8)45u,
    (u8)46u,
    (u8)47u,
    (u8)48u,
    (u8)49u,
    (u8)50u,
    (u8)0u,
    (u8)0u,
    (u8)44u,
    (u8)0u,
    (u8)0u,
    (u8)0u
};
byte mm_tables_stdtypebase[57] = {
    (u8)0u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)5u,
    (u8)9u,
    (u8)9u,
    (u8)9u,
    (u8)9u,
    (u8)10u,
    (u8)11u,
    (u8)12u,
    (u8)15u,
    (u8)15u,
    (u8)15u,
    (u8)16u,
    (u8)4u,
    (u8)4u,
    (u8)4u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
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
i64 mm_tables_trefproc;
i64 mm_tables_treflabel;
i64 mm_tables_trefchar;
byte *  mm_tables_packtypenames[20] = {
    (byte*)"tp_void",
    (byte*)"tp_i64",
    (byte*)"tp_u64",
    (byte*)"tp_r64",
    (byte*)"tp_pvoid",
    (byte*)"tp_pi8",
    (byte*)"tp_pi16",
    (byte*)"tp_pi32",
    (byte*)"tp_pi64",
    (byte*)"tp_pi128",
    (byte*)"tp_pu8",
    (byte*)"tp_pu16",
    (byte*)"tp_pu32",
    (byte*)"tp_pu64",
    (byte*)"tp_pu128",
    (byte*)"tp_pr32",
    (byte*)"tp_pr64",
    (byte*)"tp_pstruct",
    (byte*)"tp_stringz",
    (byte*)"tp_variant"
};
i64 mm_tables_packtypewidths[20] = {
    (i64)0,
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
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)64
};
byte *  mm_tables_jtagnames[244] = {
    (byte*)"j_none",
    (byte*)"j_const",
    (byte*)"j_null",
    (byte*)"j_name",
    (byte*)"j_block",
    (byte*)"j_stmtblock",
    (byte*)"j_decimal",
    (byte*)"j_assem",
    (byte*)"j_assemmacro",
    (byte*)"j_andl",
    (byte*)"j_orl",
    (byte*)"j_xorl",
    (byte*)"j_notl",
    (byte*)"j_istruel",
    (byte*)"j_makelist",
    (byte*)"j_makerange",
    (byte*)"j_makeset",
    (byte*)"j_makedict",
    (byte*)"j_exprlist",
    (byte*)"j_multexpr",
    (byte*)"j_returnmult",
    (byte*)"j_keyword",
    (byte*)"j_keyvalue",
    (byte*)"j_assignx",
    (byte*)"j_deepcopyx",
    (byte*)"j_callfn",
    (byte*)"j_callmfn",
    (byte*)"j_applyop",
    (byte*)"j_applyopx",
    (byte*)"j_andand",
    (byte*)"j_eq",
    (byte*)"j_ne",
    (byte*)"j_lt",
    (byte*)"j_le",
    (byte*)"j_gt",
    (byte*)"j_ge",
    (byte*)"j_isequal",
    (byte*)"j_add",
    (byte*)"j_sub",
    (byte*)"j_mul",
    (byte*)"j_div",
    (byte*)"j_idiv",
    (byte*)"j_irem",
    (byte*)"j_idivrem",
    (byte*)"j_iand",
    (byte*)"j_ior",
    (byte*)"j_ixor",
    (byte*)"j_shl",
    (byte*)"j_shr",
    (byte*)"j_in",
    (byte*)"j_notin",
    (byte*)"j_inrev",
    (byte*)"j_inrange",
    (byte*)"j_inset",
    (byte*)"j_min",
    (byte*)"j_max",
    (byte*)"j_subref",
    (byte*)"j_addoffset",
    (byte*)"j_suboffset",
    (byte*)"j_concat",
    (byte*)"j_append",
    (byte*)"j_clamp",
    (byte*)"j_left",
    (byte*)"j_right",
    (byte*)"j_head",
    (byte*)"j_tail",
    (byte*)"j_init",
    (byte*)"j_last",
    (byte*)"j_take",
    (byte*)"j_drop",
    (byte*)"j_dupl",
    (byte*)"j_ireverse",
    (byte*)"j_reverse",
    (byte*)"j_insert",
    (byte*)"j_delete",
    (byte*)"j_prepend",
    (byte*)"j_zip",
    (byte*)"j_convlc",
    (byte*)"j_convuc",
    (byte*)"j_flexptr",
    (byte*)"j_stringz",
    (byte*)"j_sliceptr",
    (byte*)"j_index",
    (byte*)"j_slice",
    (byte*)"j_dotindex",
    (byte*)"j_dotslice",
    (byte*)"j_anddotindex",
    (byte*)"j_anddotslice",
    (byte*)"j_keyindex",
    (byte*)"j_dot",
    (byte*)"j_dotattr",
    (byte*)"j_atan2",
    (byte*)"j_power",
    (byte*)"j_ptr",
    (byte*)"j_addrof",
    (byte*)"j_addroffirst",
    (byte*)"j_convert",
    (byte*)"j_convertref",
    (byte*)"j_autocast",
    (byte*)"j_typepun",
    (byte*)"j_typeconst",
    (byte*)"j_operator",
    (byte*)"j_upper",
    (byte*)"j_neg",
    (byte*)"j_abs",
    (byte*)"j_inot",
    (byte*)"j_chr",
    (byte*)"j_asc",
    (byte*)"j_sqrt",
    (byte*)"j_sqr",
    (byte*)"j_cube",
    (byte*)"j_sign",
    (byte*)"j_sin",
    (byte*)"j_cos",
    (byte*)"j_tan",
    (byte*)"j_asin",
    (byte*)"j_acos",
    (byte*)"j_atan",
    (byte*)"j_ln",
    (byte*)"j_lg",
    (byte*)"j_log",
    (byte*)"j_exp",
    (byte*)"j_round",
    (byte*)"j_floor",
    (byte*)"j_ceil",
    (byte*)"j_fract",
    (byte*)"j_fmod",
    (byte*)"j_lwb",
    (byte*)"j_upb",
    (byte*)"j_len",
    (byte*)"j_bounds",
    (byte*)"j_bitwidth",
    (byte*)"j_bytesize",
    (byte*)"j_typeof",
    (byte*)"j_typestr",
    (byte*)"j_bitfield",
    (byte*)"j_minvalue",
    (byte*)"j_maxvalue",
    (byte*)"j_preincrx",
    (byte*)"j_predecrx",
    (byte*)"j_postincrx",
    (byte*)"j_postdecrx",
    (byte*)"j_incr",
    (byte*)"j_decr",
    (byte*)"j_addto",
    (byte*)"j_subto",
    (byte*)"j_multo",
    (byte*)"j_divto",
    (byte*)"j_idivto",
    (byte*)"j_iremto",
    (byte*)"j_iandto",
    (byte*)"j_iorto",
    (byte*)"j_ixorto",
    (byte*)"j_shlto",
    (byte*)"j_shrto",
    (byte*)"j_minto",
    (byte*)"j_maxto",
    (byte*)"j_addoffsetto",
    (byte*)"j_suboffsetto",
    (byte*)"j_negto",
    (byte*)"j_absto",
    (byte*)"j_inotto",
    (byte*)"j_isvoid",
    (byte*)"j_isdef",
    (byte*)"j_isint",
    (byte*)"j_isreal",
    (byte*)"j_isstring",
    (byte*)"j_islist",
    (byte*)"j_isrecord",
    (byte*)"j_isarray",
    (byte*)"j_isset",
    (byte*)"j_ispointer",
    (byte*)"j_ismutable",
    (byte*)"j_cvlineno",
    (byte*)"j_cvstrlineno",
    (byte*)"j_cvmodulename",
    (byte*)"j_cvfilename",
    (byte*)"j_cvfunction",
    (byte*)"j_cvdate",
    (byte*)"j_cvtime",
    (byte*)"j_cvversion",
    (byte*)"j_cvtypename",
    (byte*)"j_cvtargetbits",
    (byte*)"j_cvtargetsize",
    (byte*)"j_cvtargetcode",
    (byte*)"j_whenthen",
    (byte*)"j_elsif",
    (byte*)"j_fmtitem",
    (byte*)"j_nogap",
    (byte*)"j_callproc",
    (byte*)"j_callmproc",
    (byte*)"j_return",
    (byte*)"j_assign",
    (byte*)"j_shallowcopy",
    (byte*)"j_deepcopy",
    (byte*)"j_to",
    (byte*)"j_if",
    (byte*)"j_longif",
    (byte*)"j_forup",
    (byte*)"j_fordown",
    (byte*)"j_forstep",
    (byte*)"j_forall",
    (byte*)"j_forallrev",
    (byte*)"j_foreach",
    (byte*)"j_foreachrev",
    (byte*)"j_cfor",
    (byte*)"j_while",
    (byte*)"j_repeat",
    (byte*)"j_goto",
    (byte*)"j_gotoblock",
    (byte*)"j_labeldef",
    (byte*)"j_restart",
    (byte*)"j_redo",
    (byte*)"j_next",
    (byte*)"j_exit",
    (byte*)"j_do",
    (byte*)"j_case",
    (byte*)"j_docase",
    (byte*)"j_switch",
    (byte*)"j_doswitch",
    (byte*)"j_swap",
    (byte*)"j_select",
    (byte*)"j_recase",
    (byte*)"j_print",
    (byte*)"j_println",
    (byte*)"j_fprint",
    (byte*)"j_fprintln",
    (byte*)"j_cprint",
    (byte*)"j_cprintln",
    (byte*)"j_sprint",
    (byte*)"j_sfprint",
    (byte*)"j_read",
    (byte*)"j_readln",
    (byte*)"j_sread",
    (byte*)"j_sreadln",
    (byte*)"j_stop",
    (byte*)"j_try",
    (byte*)"j_except",
    (byte*)"j_yield",
    (byte*)"j_raise",
    (byte*)"j_eval",
    (byte*)"j_lambda",
    (byte*)"j_emitc",
    (byte*)"j_dummy"
};
byte mm_tables_jisexpr[244] = {
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
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
    (u8)1u,
    (u8)0u,
    (u8)1u,
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
    (u8)1u,
    (u8)1u,
    (u8)1u
};
byte *  mm_tables_fflangnames[7] = {(byte*)"noff",(byte*)"windowsff",(byte*)"clangff",(byte*)"qlangff",(byte*)"blangff",(byte*)"callbackff",(byte*)"dummyff"};
byte *  mm_tables_bitfieldnames[8] = {(byte*)"bf_msb",(byte*)"bf_lsb",(byte*)"bf_msbit",(byte*)"bf_lsbit",(byte*)"bf_msw",(byte*)"bf_lsw",(byte*)"bf_odd",(byte*)"bf_even"};
byte *  mm_tables_symbolnames[145] = {
    (byte*)"errorsym",
    (byte*)"dotsym",
    (byte*)"lexdotsym",
    (byte*)"anddotsym",
    (byte*)"commasym",
    (byte*)"semisym",
    (byte*)"colonsym",
    (byte*)"dcolonsym",
    (byte*)"assignsym",
    (byte*)"deepcopysym",
    (byte*)"sendtosym",
    (byte*)"lbracksym",
    (byte*)"rbracksym",
    (byte*)"lsqsym",
    (byte*)"rsqsym",
    (byte*)"lcurlysym",
    (byte*)"rcurlysym",
    (byte*)"ptrsym",
    (byte*)"barsym",
    (byte*)"dbarsym",
    (byte*)"atsym",
    (byte*)"datsym",
    (byte*)"questionsym",
    (byte*)"addrsym",
    (byte*)"daddrsym",
    (byte*)"poundsym",
    (byte*)"curlsym",
    (byte*)"gatesym",
    (byte*)"rangesym",
    (byte*)"ellipsissym",
    (byte*)"hashsym",
    (byte*)"opsym",
    (byte*)"opsym2",
    (byte*)"bitfieldsym",
    (byte*)"eolsym",
    (byte*)"eofsym",
    (byte*)"rawnamesym",
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
    (byte*)"emitcsym",
    (byte*)"unitnamesym",
    (byte*)"namesym",
    (byte*)"ksourcedirsym",
    (byte*)"registersym",
    (byte*)"stdtypesym",
    (byte*)"machinetypesym",
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
    (byte*)"kforallsym",
    (byte*)"ktosym",
    (byte*)"kbysym",
    (byte*)"kdosym",
    (byte*)"kwhilesym",
    (byte*)"krepeatsym",
    (byte*)"kuntilsym",
    (byte*)"kreturnsym",
    (byte*)"kstopsym",
    (byte*)"kloopsym",
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
    (byte*)"kimportsym",
    (byte*)"kimportmodulesym",
    (byte*)"kimportpathsym",
    (byte*)"kmapmodulesym",
    (byte*)"kmodulesym",
    (byte*)"ktypesym",
    (byte*)"ktypeattrsym",
    (byte*)"krefsym",
    (byte*)"kvarsym",
    (byte*)"kletsym",
    (byte*)"kvariantsym",
    (byte*)"kslicesym",
    (byte*)"kflexsym",
    (byte*)"kmacrosym",
    (byte*)"kexpandsym",
    (byte*)"koperatorsym",
    (byte*)"kconstsym",
    (byte*)"klocalssym",
    (byte*)"kenumsym",
    (byte*)"knewsym",
    (byte*)"kclasssym",
    (byte*)"kdoblocksym",
    (byte*)"kblockdefsym",
    (byte*)"kdirectivesym",
    (byte*)"kfflangsym",
    (byte*)"kglobalsym",
    (byte*)"kstaticsym",
    (byte*)"ktrysym",
    (byte*)"kexceptsym",
    (byte*)"kfinallysym",
    (byte*)"kraisesym",
    (byte*)"kyieldsym",
    (byte*)"kextendsym",
    (byte*)"kblocksym",
    (byte*)"kcastsym",
    (byte*)"ktypeconstsym",
    (byte*)"compilervarsym",
    (byte*)"dollarsym",
    (byte*)"kevalsym",
    (byte*)"ktabledatasym",
    (byte*)"kmapsym",
    (byte*)"kapplyopsym",
    (byte*)"kstacksym",
    (byte*)"kclampsym",
    (byte*)"kswapsym",
    (byte*)"kerrorsym",
    (byte*)"sysconstsym",
    (byte*)"kassemsym",
    (byte*)"kdummysym"
};
byte *  mm_tables_sourcedirnames[15] = {
    (byte*)"emitcdir",
    (byte*)"ifdir",
    (byte*)"elsifdir",
    (byte*)"elsedir",
    (byte*)"endifdir",
    (byte*)"debuglinedir",
    (byte*)"includedir",
    (byte*)"endincludedir",
    (byte*)"commentdir",
    (byte*)"endcommentdir",
    (byte*)"strincludedir",
    (byte*)"binincludedir",
    (byte*)"cclibdir",
    (byte*)"targetlangdir",
    (byte*)"enddir"
};
byte *  mm_tables_parammodenames[4] = {(byte*)"Var ",(byte*)"In ",(byte*)"Out ",(byte*)"Opt "};
byte *  mm_tables_namecatnames[8] = {(byte*)"-",(byte*)"proc",(byte*)"gproc",(byte*)"dllproc",(byte*)"dllmodule",(byte*)"dllvar",(byte*)"static",(byte*)"frame"};
byte mm_tables_qualifiedname[8] = {(u8)0u,(u8)1u,(u8)1u,(u8)0u,(u8)0u,(u8)0u,(u8)1u,(u8)1u};
byte *  mm_tables_namenames[21] = {
    (byte*)"nullid",
    (byte*)"programid",
    (byte*)"moduleid",
    (byte*)"dllmoduleid",
    (byte*)"typeid",
    (byte*)"procid",
    (byte*)"dllprocid",
    (byte*)"dllvarid",
    (byte*)"constid",
    (byte*)"staticid",
    (byte*)"frameid",
    (byte*)"paramid",
    (byte*)"fieldid",
    (byte*)"genfieldid",
    (byte*)"enumid",
    (byte*)"labelid",
    (byte*)"blockid",
    (byte*)"aliasid",
    (byte*)"macroid",
    (byte*)"macroparamid",
    (byte*)"linkid"
};
byte mm_tables_defaultnamecat[21] = {
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)4u,
    (u8)0u,
    (u8)1u,
    (u8)3u,
    (u8)5u,
    (u8)0u,
    (u8)6u,
    (u8)7u,
    (u8)7u,
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
byte *  mm_tables_stnames[403] = {
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
    (byte*)"foreach",
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
    (byte*)"yield",
    (byte*)"stop",
    (byte*)"restart",
    (byte*)"redo",
    (byte*)"loop",
    (byte*)"next",
    (byte*)"exit",
    (byte*)"goto",
    (byte*)"go",
    (byte*)"switch",
    (byte*)"doswitch",
    (byte*)"tabledata",
    (byte*)"clamp",
    (byte*)"eval",
    (byte*)"print",
    (byte*)"println",
    (byte*)"fprint",
    (byte*)"fprintln",
    (byte*)"cprint",
    (byte*)"cprintln",
    (byte*)"sprint",
    (byte*)"sfprint",
    (byte*)"cp",
    (byte*)"cpl",
    (byte*)"read",
    (byte*)"readln",
    (byte*)"cast",
    (byte*)"proc",
    (byte*)"function",
    (byte*)"threadedproc",
    (byte*)"threadedfunction",
    (byte*)"type",
    (byte*)"class",
    (byte*)"record",
    (byte*)"struct",
    (byte*)"union",
    (byte*)"ref",
    (byte*)"var",
    (byte*)"let",
    (byte*)"any",
    (byte*)"include",
    (byte*)"strinclude",
    (byte*)"bininclude",
    (byte*)"emitc",
    (byte*)"cclib",
    (byte*)"macro",
    (byte*)"operator",
    (byte*)"assem",
    (byte*)"asm",
    (byte*)"static",
    (byte*)"const",
    (byte*)"enum",
    (byte*)"importdll",
    (byte*)"importlib",
    (byte*)"import",
    (byte*)"importpath",
    (byte*)"mapmodule",
    (byte*)"unless",
    (byte*)"try",
    (byte*)"except",
    (byte*)"finally",
    (byte*)"raise",
    (byte*)"out",
    (byte*)"global",
    (byte*)"export",
    (byte*)"clang",
    (byte*)"qlang",
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
    (byte*)"int128",
    (byte*)"i8",
    (byte*)"i16",
    (byte*)"i32",
    (byte*)"i64",
    (byte*)"i128",
    (byte*)"real32",
    (byte*)"real64",
    (byte*)"r32",
    (byte*)"r64",
    (byte*)"float32",
    (byte*)"float64",
    (byte*)"byte",
    (byte*)"u1",
    (byte*)"u2",
    (byte*)"u4",
    (byte*)"u8",
    (byte*)"u16",
    (byte*)"u32",
    (byte*)"u64",
    (byte*)"u128",
    (byte*)"word8",
    (byte*)"word16",
    (byte*)"word32",
    (byte*)"word64",
    (byte*)"word128",
    (byte*)"bit",
    (byte*)"bit2",
    (byte*)"bit4",
    (byte*)"char",
    (byte*)"wchar",
    (byte*)"char64",
    (byte*)"array",
    (byte*)"bitarray",
    (byte*)"complex",
    (byte*)"string",
    (byte*)"wstring",
    (byte*)"set",
    (byte*)"decimal",
    (byte*)"dict",
    (byte*)"range",
    (byte*)"label",
    (byte*)"auto",
    (byte*)"intm",
    (byte*)"intp",
    (byte*)"wordm",
    (byte*)"wordp",
    (byte*)"slice",
    (byte*)"flex",
    (byte*)"typeof",
    (byte*)"subrange",
    (byte*)"million",
    (byte*)"billion",
    (byte*)"thousand",
    (byte*)"kb",
    (byte*)"mb",
    (byte*)"gb",
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
    (byte*)"sqrt",
    (byte*)"sqr",
    (byte*)"cube",
    (byte*)"cos",
    (byte*)"sin",
    (byte*)"tan",
    (byte*)"asin",
    (byte*)"acos",
    (byte*)"atan",
    (byte*)"atan2",
    (byte*)"sign",
    (byte*)"ln",
    (byte*)"log",
    (byte*)"lg",
    (byte*)"exp",
    (byte*)"round",
    (byte*)"floor",
    (byte*)"ceil",
    (byte*)"fract",
    (byte*)"fmod",
    (byte*)"head",
    (byte*)"tail",
    (byte*)"init",
    (byte*)"take",
    (byte*)"drop",
    (byte*)"insert",
    (byte*)"delete",
    (byte*)"ireverse",
    (byte*)"reverse",
    (byte*)"dupl",
    (byte*)"zip",
    (byte*)"prepend",
    (byte*)"append",
    (byte*)"concat",
    (byte*)"convlc",
    (byte*)"convuc",
    (byte*)"flexptr",
    (byte*)"sliceptr",
    (byte*)"stringz",
    (byte*)"len",
    (byte*)"lwb",
    (byte*)"upb",
    (byte*)"bounds",
    (byte*)"bitwidth",
    (byte*)"bytes",
    (byte*)"minvalue",
    (byte*)"maxvalue",
    (byte*)"typestr",
    (byte*)"isvoid",
    (byte*)"isdef",
    (byte*)"isint",
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
    (byte*)"endforall",
    (byte*)"od",
    (byte*)"endproc",
    (byte*)"endfunction",
    (byte*)"endwhile",
    (byte*)"endto",
    (byte*)"enddo",
    (byte*)"endunless",
    (byte*)"endmodule",
    (byte*)"endimportmodule",
    (byte*)"endtry",
    (byte*)"endrecord",
    (byte*)"endclass",
    (byte*)"endblock",
    (byte*)"endassem",
    (byte*)"nil",
    (byte*)"con",
    (byte*)"pi",
    (byte*)"eax",
    (byte*)"ebx",
    (byte*)"ecx",
    (byte*)"edx",
    (byte*)"esi",
    (byte*)"edi",
    (byte*)"esp",
    (byte*)"ebp",
    (byte*)"ax",
    (byte*)"bx",
    (byte*)"cx",
    (byte*)"dx",
    (byte*)"si",
    (byte*)"di",
    (byte*)"sp",
    (byte*)"bp",
    (byte*)"rax",
    (byte*)"rbx",
    (byte*)"rcx",
    (byte*)"rdx",
    (byte*)"rsi",
    (byte*)"rdi",
    (byte*)"rsp",
    (byte*)"rbp",
    (byte*)"r8",
    (byte*)"r9",
    (byte*)"r10",
    (byte*)"r11",
    (byte*)"r12",
    (byte*)"r13",
    (byte*)"r14",
    (byte*)"r15",
    (byte*)"al",
    (byte*)"bl",
    (byte*)"cl",
    (byte*)"dl",
    (byte*)"ah",
    (byte*)"bh",
    (byte*)"ch",
    (byte*)"dh",
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
    (byte*)"aframe",
    (byte*)"astack",
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
    (byte*)"dstack",
    (byte*)"dframe",
    (byte*)"dprog",
    (byte*)"dsptr",
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
    (byte*)"xmm15",
    (byte*)"qword",
    (byte*)"$$dummy"
};
i64 mm_tables_stsymbols[403] = {
    (i64)59,
    (i64)60,
    (i64)61,
    (i64)62,
    (i64)63,
    (i64)64,
    (i64)68,
    (i64)69,
    (i64)70,
    (i64)71,
    (i64)72,
    (i64)73,
    (i64)73,
    (i64)74,
    (i64)74,
    (i64)75,
    (i64)76,
    (i64)66,
    (i64)77,
    (i64)78,
    (i64)79,
    (i64)79,
    (i64)80,
    (i64)128,
    (i64)81,
    (i64)82,
    (i64)82,
    (i64)82,
    (i64)82,
    (i64)82,
    (i64)83,
    (i64)83,
    (i64)84,
    (i64)85,
    (i64)136,
    (i64)140,
    (i64)135,
    (i64)86,
    (i64)86,
    (i64)86,
    (i64)86,
    (i64)86,
    (i64)86,
    (i64)87,
    (i64)87,
    (i64)86,
    (i64)86,
    (i64)88,
    (i64)88,
    (i64)131,
    (i64)91,
    (i64)92,
    (i64)91,
    (i64)92,
    (i64)102,
    (i64)117,
    (i64)94,
    (i64)95,
    (i64)96,
    (i64)104,
    (i64)105,
    (i64)106,
    (i64)107,
    (i64)51,
    (i64)51,
    (i64)51,
    (i64)51,
    (i64)51,
    (i64)110,
    (i64)112,
    (i64)144,
    (i64)144,
    (i64)123,
    (i64)113,
    (i64)115,
    (i64)98,
    (i64)98,
    (i64)97,
    (i64)99,
    (i64)100,
    (i64)67,
    (i64)124,
    (i64)125,
    (i64)126,
    (i64)127,
    (i64)57,
    (i64)122,
    (i64)122,
    (i64)121,
    (i64)121,
    (i64)121,
    (i64)121,
    (i64)141,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)58,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)53,
    (i64)54,
    (i64)54,
    (i64)54,
    (i64)54,
    (i64)108,
    (i64)109,
    (i64)55,
    (i64)56,
    (i64)49,
    (i64)49,
    (i64)49,
    (i64)49,
    (i64)49,
    (i64)49,
    (i64)133,
    (i64)133,
    (i64)133,
    (i64)133,
    (i64)133,
    (i64)133,
    (i64)133,
    (i64)133,
    (i64)133,
    (i64)133,
    (i64)133,
    (i64)133,
    (i64)134,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)32,
    (i64)33,
    (i64)33,
    (i64)33,
    (i64)33,
    (i64)33,
    (i64)33,
    (i64)33,
    (i64)33,
    (i64)33,
    (i64)33,
    (i64)33,
    (i64)33,
    (i64)34,
    (i64)34,
    (i64)34,
    (i64)34,
    (i64)34,
    (i64)34,
    (i64)34,
    (i64)34,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)66,
    (i64)143,
    (i64)143,
    (i64)143,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)52,
    (i64)0
};
i64 mm_tables_stsubcodes[403] = {
    (i64)196,
    (i64)0,
    (i64)196,
    (i64)0,
    (i64)216,
    (i64)218,
    (i64)216,
    (i64)217,
    (i64)222,
    (i64)0,
    (i64)0,
    (i64)201,
    (i64)203,
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
    (i64)0,
    (i64)211,
    (i64)212,
    (i64)212,
    (i64)213,
    (i64)214,
    (i64)0,
    (i64)1,
    (i64)218,
    (i64)219,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)223,
    (i64)224,
    (i64)225,
    (i64)226,
    (i64)227,
    (i64)228,
    (i64)229,
    (i64)230,
    (i64)223,
    (i64)224,
    (i64)231,
    (i64)232,
    (i64)96,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)1,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)7,
    (i64)11,
    (i64)12,
    (i64)1,
    (i64)13,
    (i64)0,
    (i64)0,
    (i64)1,
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
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)2,
    (i64)2,
    (i64)3,
    (i64)1,
    (i64)5,
    (i64)0,
    (i64)0,
    (i64)4,
    (i64)9,
    (i64)12,
    (i64)0,
    (i64)1,
    (i64)2,
    (i64)3,
    (i64)4,
    (i64)5,
    (i64)1,
    (i64)2,
    (i64)3,
    (i64)4,
    (i64)5,
    (i64)11,
    (i64)12,
    (i64)11,
    (i64)12,
    (i64)11,
    (i64)12,
    (i64)6,
    (i64)17,
    (i64)18,
    (i64)19,
    (i64)6,
    (i64)7,
    (i64)8,
    (i64)9,
    (i64)10,
    (i64)6,
    (i64)7,
    (i64)8,
    (i64)9,
    (i64)10,
    (i64)17,
    (i64)18,
    (i64)19,
    (i64)13,
    (i64)14,
    (i64)15,
    (i64)29,
    (i64)30,
    (i64)26,
    (i64)34,
    (i64)35,
    (i64)31,
    (i64)16,
    (i64)40,
    (i64)27,
    (i64)54,
    (i64)52,
    (i64)73,
    (i64)105,
    (i64)87,
    (i64)119,
    (i64)28,
    (i64)36,
    (i64)0,
    (i64)0,
    (i64)2,
    (i64)3,
    (i64)1,
    (i64)4,
    (i64)5,
    (i64)6,
    (i64)173,
    (i64)174,
    (i64)176,
    (i64)175,
    (i64)177,
    (i64)178,
    (i64)179,
    (i64)180,
    (i64)181,
    (i64)182,
    (i64)183,
    (i64)184,
    (i64)0,
    (i64)9,
    (i64)10,
    (i64)11,
    (i64)44,
    (i64)45,
    (i64)46,
    (i64)49,
    (i64)50,
    (i64)51,
    (i64)42,
    (i64)43,
    (i64)54,
    (i64)55,
    (i64)12,
    (i64)105,
    (i64)13,
    (i64)104,
    (i64)103,
    (i64)108,
    (i64)109,
    (i64)110,
    (i64)113,
    (i64)112,
    (i64)114,
    (i64)115,
    (i64)116,
    (i64)117,
    (i64)91,
    (i64)111,
    (i64)118,
    (i64)120,
    (i64)119,
    (i64)121,
    (i64)122,
    (i64)123,
    (i64)124,
    (i64)125,
    (i64)126,
    (i64)64,
    (i64)65,
    (i64)66,
    (i64)68,
    (i64)69,
    (i64)73,
    (i64)74,
    (i64)71,
    (i64)72,
    (i64)70,
    (i64)76,
    (i64)75,
    (i64)60,
    (i64)59,
    (i64)77,
    (i64)78,
    (i64)79,
    (i64)81,
    (i64)80,
    (i64)129,
    (i64)127,
    (i64)128,
    (i64)130,
    (i64)131,
    (i64)132,
    (i64)136,
    (i64)137,
    (i64)134,
    (i64)162,
    (i64)163,
    (i64)164,
    (i64)1,
    (i64)2,
    (i64)3,
    (i64)4,
    (i64)5,
    (i64)6,
    (i64)7,
    (i64)8,
    (i64)59,
    (i64)59,
    (i64)68,
    (i64)68,
    (i64)69,
    (i64)84,
    (i64)85,
    (i64)72,
    (i64)73,
    (i64)76,
    (i64)91,
    (i64)92,
    (i64)77,
    (i64)74,
    (i64)76,
    (i64)67,
    (i64)101,
    (i64)98,
    (i64)124,
    (i64)94,
    (i64)117,
    (i64)130,
    (i64)144,
    (i64)1,
    (i64)4,
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
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0
};
i64 mm_tables_oplist[37] = {
    (i64)37,
    (i64)38,
    (i64)39,
    (i64)40,
    (i64)41,
    (i64)42,
    (i64)43,
    (i64)9,
    (i64)10,
    (i64)11,
    (i64)44,
    (i64)45,
    (i64)46,
    (i64)47,
    (i64)48,
    (i64)49,
    (i64)50,
    (i64)51,
    (i64)30,
    (i64)31,
    (i64)32,
    (i64)35,
    (i64)33,
    (i64)34,
    (i64)36,
    (i64)54,
    (i64)55,
    (i64)92,
    (i64)91,
    (i64)57,
    (i64)58,
    (i64)56,
    (i64)59,
    (i64)60,
    (i64)23,
    (i64)24,
    (i64)15
};
i64 mm_tables_oppriolist[37] = {
    (i64)4,
    (i64)4,
    (i64)3,
    (i64)3,
    (i64)3,
    (i64)3,
    (i64)3,
    (i64)7,
    (i64)8,
    (i64)6,
    (i64)4,
    (i64)4,
    (i64)4,
    (i64)3,
    (i64)3,
    (i64)6,
    (i64)6,
    (i64)6,
    (i64)6,
    (i64)6,
    (i64)6,
    (i64)6,
    (i64)6,
    (i64)6,
    (i64)6,
    (i64)4,
    (i64)4,
    (i64)2,
    (i64)3,
    (i64)4,
    (i64)4,
    (i64)4,
    (i64)4,
    (i64)4,
    (i64)1,
    (i64)1,
    (i64)5
};
byte mm_tables_jtagpriotable[244];
byte *  mm_tables_convnames[33] = {
    (byte*)"c_none",
    (byte*)"c_soft",
    (byte*)"c_hard",
    (byte*)"c_bool",
    (byte*)"c_iwiden",
    (byte*)"c_uwiden",
    (byte*)"c_ifloat",
    (byte*)"c_ufloat",
    (byte*)"c_ifix",
    (byte*)"c_ufix",
    (byte*)"c_diwiden",
    (byte*)"c_duwiden",
    (byte*)"c_difloat",
    (byte*)"c_dufloat",
    (byte*)"c_difix",
    (byte*)"c_dufix",
    (byte*)"c_dfnarrow",
    (byte*)"c_dfwiden",
    (byte*)"c_narrow",
    (byte*)"c_softtruncate",
    (byte*)"c_truncate",
    (byte*)"c_fnarrow",
    (byte*)"c_fwiden",
    (byte*)"c_inttoref",
    (byte*)"c_reftoint",
    (byte*)"c_reftoref",
    (byte*)"c_anytovariant",
    (byte*)"c_anytodecimal",
    (byte*)"c_ichartostring",
    (byte*)"c_varianttoany",
    (byte*)"c_decimaltoany",
    (byte*)"c_stringtoichar",
    (byte*)"c_error"
};
byte mm_tables_dominantmode[32][32];
byte mm_tables_conversionops[32][32];
byte mm_tables_typesetuptable[80][4] = {
    {(u8)4u,(u8)4u,(u8)4u,(u8)0u},
    {(u8)4u,(u8)5u,(u8)5u,(u8)4u},
    {(u8)4u,(u8)9u,(u8)4u,(u8)1u},
    {(u8)4u,(u8)10u,(u8)5u,(u8)5u},
    {(u8)4u,(u8)15u,(u8)4u,(u8)19u},
    {(u8)4u,(u8)11u,(u8)12u,(u8)6u},
    {(u8)4u,(u8)12u,(u8)12u,(u8)6u},
    {(u8)4u,(u8)16u,(u8)16u,(u8)10u},
    {(u8)4u,(u8)20u,(u8)0u,(u8)23u},
    {(u8)5u,(u8)4u,(u8)5u,(u8)19u},
    {(u8)5u,(u8)5u,(u8)5u,(u8)0u},
    {(u8)5u,(u8)9u,(u8)5u,(u8)1u},
    {(u8)5u,(u8)10u,(u8)5u,(u8)1u},
    {(u8)5u,(u8)15u,(u8)5u,(u8)19u},
    {(u8)5u,(u8)11u,(u8)16u,(u8)6u},
    {(u8)5u,(u8)12u,(u8)16u,(u8)6u},
    {(u8)5u,(u8)16u,(u8)16u,(u8)12u},
    {(u8)5u,(u8)20u,(u8)0u,(u8)23u},
    {(u8)9u,(u8)4u,(u8)4u,(u8)1u},
    {(u8)9u,(u8)5u,(u8)5u,(u8)5u},
    {(u8)9u,(u8)9u,(u8)9u,(u8)0u},
    {(u8)9u,(u8)10u,(u8)10u,(u8)5u},
    {(u8)9u,(u8)15u,(u8)9u,(u8)19u},
    {(u8)9u,(u8)11u,(u8)12u,(u8)7u},
    {(u8)9u,(u8)12u,(u8)12u,(u8)7u},
    {(u8)9u,(u8)16u,(u8)16u,(u8)13u},
    {(u8)9u,(u8)20u,(u8)0u,(u8)23u},
    {(u8)10u,(u8)4u,(u8)5u,(u8)1u},
    {(u8)10u,(u8)5u,(u8)5u,(u8)1u},
    {(u8)10u,(u8)9u,(u8)10u,(u8)0u},
    {(u8)10u,(u8)10u,(u8)10u,(u8)0u},
    {(u8)10u,(u8)15u,(u8)10u,(u8)19u},
    {(u8)10u,(u8)11u,(u8)16u,(u8)7u},
    {(u8)10u,(u8)12u,(u8)16u,(u8)7u},
    {(u8)10u,(u8)16u,(u8)16u,(u8)13u},
    {(u8)10u,(u8)20u,(u8)0u,(u8)23u},
    {(u8)15u,(u8)4u,(u8)4u,(u8)1u},
    {(u8)15u,(u8)5u,(u8)5u,(u8)5u},
    {(u8)15u,(u8)9u,(u8)9u,(u8)1u},
    {(u8)15u,(u8)10u,(u8)10u,(u8)5u},
    {(u8)15u,(u8)15u,(u8)15u,(u8)0u},
    {(u8)15u,(u8)11u,(u8)12u,(u8)7u},
    {(u8)15u,(u8)12u,(u8)12u,(u8)7u},
    {(u8)15u,(u8)16u,(u8)16u,(u8)13u},
    {(u8)15u,(u8)20u,(u8)0u,(u8)23u},
    {(u8)11u,(u8)4u,(u8)11u,(u8)8u},
    {(u8)11u,(u8)5u,(u8)11u,(u8)8u},
    {(u8)11u,(u8)9u,(u8)11u,(u8)9u},
    {(u8)11u,(u8)10u,(u8)16u,(u8)9u},
    {(u8)11u,(u8)15u,(u8)11u,(u8)9u},
    {(u8)11u,(u8)11u,(u8)11u,(u8)0u},
    {(u8)11u,(u8)12u,(u8)12u,(u8)22u},
    {(u8)11u,(u8)16u,(u8)16u,(u8)17u},
    {(u8)11u,(u8)20u,(u8)0u,(u8)32u},
    {(u8)12u,(u8)4u,(u8)12u,(u8)8u},
    {(u8)12u,(u8)5u,(u8)16u,(u8)8u},
    {(u8)12u,(u8)9u,(u8)12u,(u8)9u},
    {(u8)12u,(u8)10u,(u8)16u,(u8)9u},
    {(u8)12u,(u8)15u,(u8)12u,(u8)9u},
    {(u8)12u,(u8)11u,(u8)12u,(u8)21u},
    {(u8)12u,(u8)12u,(u8)12u,(u8)0u},
    {(u8)12u,(u8)16u,(u8)16u,(u8)17u},
    {(u8)12u,(u8)20u,(u8)0u,(u8)32u},
    {(u8)16u,(u8)4u,(u8)16u,(u8)14u},
    {(u8)16u,(u8)5u,(u8)16u,(u8)14u},
    {(u8)16u,(u8)9u,(u8)12u,(u8)15u},
    {(u8)16u,(u8)10u,(u8)16u,(u8)15u},
    {(u8)16u,(u8)15u,(u8)12u,(u8)15u},
    {(u8)16u,(u8)11u,(u8)12u,(u8)16u},
    {(u8)16u,(u8)12u,(u8)12u,(u8)16u},
    {(u8)16u,(u8)16u,(u8)16u,(u8)0u},
    {(u8)16u,(u8)20u,(u8)0u,(u8)32u},
    {(u8)20u,(u8)4u,(u8)0u,(u8)24u},
    {(u8)20u,(u8)5u,(u8)0u,(u8)32u},
    {(u8)20u,(u8)9u,(u8)0u,(u8)24u},
    {(u8)20u,(u8)10u,(u8)0u,(u8)32u},
    {(u8)20u,(u8)11u,(u8)0u,(u8)32u},
    {(u8)20u,(u8)12u,(u8)0u,(u8)32u},
    {(u8)20u,(u8)16u,(u8)0u,(u8)32u},
    {(u8)20u,(u8)20u,(u8)20u,(u8)25u}
};
i64 mm_tables_d_exprstarterset[28] = {
    (i64)12,
    (i64)14,
    (i64)18,
    (i64)24,
    (i64)32,
    (i64)50,
    (i64)39,
    (i64)40,
    (i64)41,
    (i64)42,
    (i64)43,
    (i64)45,
    (i64)53,
    (i64)87,
    (i64)89,
    (i64)90,
    (i64)116,
    (i64)134,
    (i64)133,
    (i64)140,
    (i64)138,
    (i64)142,
    (i64)104,
    (i64)131,
    (i64)4,
    (i64)46,
    (i64)55,
    (i64)59
};
i64 mm_tables_d_typestarterset[9] = {(i64)53,(i64)14,(i64)104,(i64)115,(i64)94,(i64)58,(i64)55,(i64)108,(i64)109};
byte mm_tables_exprtermset[146];
byte mm_tables_condopset[244];
byte mm_tables_d_boolunitset[13] = {
    (u8)30u,
    (u8)31u,
    (u8)32u,
    (u8)33u,
    (u8)35u,
    (u8)34u,
    (u8)9u,
    (u8)10u,
    (u8)12u,
    (u8)13u,
    (u8)11u,
    (u8)52u,
    (u8)53u
};
byte mm_tables_boolunitset[244];
byte mm_tables_d_refunitset[13] = {
    (u8)3u,
    (u8)89u,
    (u8)82u,
    (u8)196u,
    (u8)221u,
    (u8)197u,
    (u8)18u,
    (u8)94u,
    (u8)93u,
    (u8)19u,
    (u8)84u,
    (u8)85u,
    (u8)135u
};
byte mm_tables_refunitset[244];
byte mm_tables_d_binopset[30] = {
    (u8)9u,
    (u8)10u,
    (u8)30u,
    (u8)31u,
    (u8)32u,
    (u8)33u,
    (u8)34u,
    (u8)35u,
    (u8)37u,
    (u8)38u,
    (u8)39u,
    (u8)40u,
    (u8)41u,
    (u8)42u,
    (u8)44u,
    (u8)45u,
    (u8)46u,
    (u8)47u,
    (u8)48u,
    (u8)54u,
    (u8)55u,
    (u8)56u,
    (u8)57u,
    (u8)58u,
    (u8)59u,
    (u8)92u,
    (u8)11u,
    (u8)43u,
    (u8)91u,
    (u8)126u
};
byte mm_tables_binopset[244];
byte mm_tables_d_monopset[31] = {
    (u8)103u,
    (u8)104u,
    (u8)105u,
    (u8)109u,
    (u8)110u,
    (u8)111u,
    (u8)112u,
    (u8)113u,
    (u8)114u,
    (u8)115u,
    (u8)116u,
    (u8)117u,
    (u8)118u,
    (u8)119u,
    (u8)120u,
    (u8)121u,
    (u8)122u,
    (u8)123u,
    (u8)124u,
    (u8)125u,
    (u8)127u,
    (u8)128u,
    (u8)129u,
    (u8)130u,
    (u8)81u,
    (u8)131u,
    (u8)132u,
    (u8)136u,
    (u8)137u,
    (u8)12u,
    (u8)13u
};
byte mm_tables_monopset[244];
static byte *  mm_start_optionnames[30] = {
    (byte*)"exe",
    (byte*)"obj",
    (byte*)"gcc",
    (byte*)"tcc",
    (byte*)"bcc",
    (byte*)"opt",
    (byte*)"c",
    (byte*)"link",
    (byte*)"run",
    (byte*)"load",
    (byte*)"parse",
    (byte*)"name",
    (byte*)"type",
    (byte*)"gen1",
    (byte*)"gen2",
    (byte*)"gen3",
    (byte*)"ma",
    (byte*)"time",
    (byte*)"v",
    (byte*)"v2",
    (byte*)"q",
    (byte*)"h",
    (byte*)"help",
    (byte*)"ext",
    (byte*)"out",
    (byte*)"nosys",
    (byte*)"unused",
    (byte*)"debug",
    (byte*)"set",
    (byte*)"writelibs"
};
static i64 mm_start_totallines = (i64)0;
static i64 mm_start_nstringobjects = (i64)0;
static i64 mm_start_fexe;
static i64 mm_start_fobj;
static i64 mm_start_fwritelibs;
static i64 mm_start_fshowtiming;
static i64 mm_start_fshowpcl1;
static i64 mm_start_fshowpcl2;
static i64 mm_start_fshowmcl1;
static i64 mm_start_fshowasm;
static i64 mm_start_fshowast1;
static i64 mm_start_fshowast2;
static i64 mm_start_fshowast3;
static i64 mm_start_fshowst;
static i64 mm_start_fshowstflat;
static i64 mm_start_fshowtypes;
static i64 mm_start_ccompiler = (i64)1;
static i64 mm_start_foptimise;
static i64 mm_start_cc_mode;
static i64 mm_start_passlevel = (i64)6;
static byte *  mm_start_outfile;
static byte *  mm_start_outfilesource;
static byte *  mm_start_outfilebin;
static byte *  mm_start_destfilename;
static byte *  mm_start_linkoption;
static byte *  mm_start_extraparams[128];
static byte *  mm_start_extravalues[128];
static i64 mm_start_nextraparams = (i64)0;
static byte *  mm_start_optionvars[25];
static byte *  mm_start_optionvalues[25];
static i64 mm_start_noptionvars;
i64 mm_start_startclock;
i64 mm_start_endclock;
static void *  msysc__fnaddresses[]= {
    &start,
    &mm_start_start_common,
    &mm_start_debugcompiler,
    &mm_start_do_loadmodules,
    &mm_start_do_parse,
    &mm_start_do_name,
    &mm_start_do_type,
    &mm_start_do_runprog,
    &mm_start_loadmainmodule,
    &mm_start_addmodule,
    &mm_start_loadimport,
    &mm_start_readimportlist,
    &mm_start_pslex,
    &mm_start_initdata,
    &mm_start_initsearchdirs,
    &mm_start_addsearchdir,
    &mm_start_showsearchdirs,
    &mm_start_showast,
    &mm_start_showstflat,
    &mm_start_showsttree,
    &mm_start_showtiming,
    &mm_start_getinputoptions,
    &mm_start_do_option,
    &mm_start_showcaption,
    &mm_start_addtolog,
    &mm_start_addoptionvar,
    &mm_start_addmodulemapping,
    &mm_start_dosetoptionvar,
    &mm_start_findoptionvar,
    &mm_start_getpsname,
    &mm_start_domapmodule,
    &mm_start_mapimport,
    &mm_start_do_writema,
    &msysc_m_getdotindex,
    &msysc_m_setdotindex,
    &msysc_m_getdotslice,
    &msysc_m_setdotslice,
    &msysc_m_getnprocs,
    &msysc_m_getnexports,
    &msysc_m_getprocaddr,
    &msysc_m_getprocname,
    &msysc_m_getprocexport,
    &msysc_pushio,
    &msysc_m_print_startfile,
    &msysc_m_print_startstr,
    &msysc_m_print_startptr,
    &msysc_m_print_startcon,
    &msysc_m_print_setfmt,
    &msysc_m_print_end,
    &msysc_m_print_ptr,
    &msysc_m_print_i64,
    &msysc_m_print_u64,
    &msysc_m_print_r64,
    &msysc_m_print_r32,
    &msysc_m_print_c8,
    &msysc_m_print_str,
    &msysc_m_print_newline,
    &msysc_m_print_nogap,
    &msysc_printstr,
    &msysc_printstr_n,
    &msysc_makezstring,
    &msysc_freezstring,
    &msysc_printchar,
    &msysc_nextfmtchars,
    &msysc_strtofmt,
    &msysc_domultichar,
    &msysc_expandstr,
    &msysc_xdivrem,
    &msysc_u64tostr,
    &msysc_i64tostrfmt,
    &msysc_u64tostrfmt,
    &msysc_i64mintostr,
    &msysc_strtostrfmt,
    &msysc_tostr_i64,
    &msysc_tostr_u64,
    &msysc_tostr_r64,
    &msysc_tostr_str,
    &msysc_getfmt,
    &msysc_strint,
    &msysc_getstrint,
    &msysc_strword,
    &msysc_strreal,
    &msysc_getstr,
    &msysc_initreadbuffer,
    &msysc_m_read_conline,
    &msysc_m_read_fileline,
    &msysc_m_read_strline,
    &msysc_readitem,
    &msysc_strtoint,
    &msysc_m_read_i64,
    &msysc_m_read_r64,
    &msysc_m_read_str,
    &msysc_readstr,
    &msysc_rereadln,
    &msysc_reread,
    &msysc_valint,
    &msysc_valreal,
    &msysc_iconvlcn,
    &msysc_iconvucn,
    &msysc_convlcstring,
    &msysc_convucstring,
    &msysc_m_power_i64,
    &msysc_m_intoverflow,
    &msysc_m_dotindex,
    &msysc_m_dotslice,
    &msysc_m_popdotindex,
    &msysc_m_popdotslice,
    &msysc_m_imin,
    &msysc_m_imax,
    &msysc_m_sign,
    &mlib_pcm_alloc,
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
    &mlib_checkpackfile,
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
    &mm_support_loadsourcefile,
    &mm_support_loadbuiltin,
    &mm_support_loadbundledfile,
    &mm_support_mcerror,
    &mm_support_serror_gen,
    &mm_support_stopcompiler,
    &mm_support_serror,
    &mm_support_serror_s,
    &mm_support_error_gen,
    &mm_support_rxerror,
    &mm_support_gerror,
    &mm_support_txerror,
    &mm_support_txerror_s,
    &mm_support_txerror_ss,
    &mm_support_rxerror_s,
    &mm_support_gerror_s,
    &mm_support_lxerror_gen,
    &mm_support_lxerror_s,
    &mm_support_lxerror,
    &mm_support_testelem,
    &mm_support_setelem,
    &mm_support_nextpoweroftwo,
    &mm_support_loaderror,
    &mm_support_gs_additem,
    &mm_support_gs_copytostr,
    &mm_support_isalphanum,
    &mm_support_inittypetables,
    &mm_support_addspecialtypes,
    &mm_support_findfile,
    &mm_support_findstdlib,
    &mm_support_getmainfile,
    &mm_support_getmodulefile,
    &mm_support_getsupportfile,
    &mm_support_writemafile,
    &mm_support_loadmafile,
    &mm_lib_newstrec,
    &mm_lib_initqclib,
    &mm_lib_getduplnameptr,
    &mm_lib_adddef,
    &mm_lib_adddef_nodupl,
    &mm_lib_createname,
    &mm_lib_createunit0,
    &mm_lib_createunit1,
    &mm_lib_createunit2,
    &mm_lib_createunit3,
    &mm_lib_insertunit,
    &mm_lib_deleteunit,
    &mm_lib_createconstunit,
    &mm_lib_createstringconstunit,
    &mm_lib_getoptocode,
    &mm_lib_createtype,
    &mm_lib_createusertype,
    &mm_lib_createusertypefromstr,
    &mm_lib_getconstvalue,
    &mm_lib_getrangelwbunit,
    &mm_lib_getrangeupbunit,
    &mm_lib_createarraymode,
    &mm_lib_createflexarraymode,
    &mm_lib_createarraymodek,
    &mm_lib_createsetmode,
    &mm_lib_createsetmodek,
    &mm_lib_createdictmode,
    &mm_lib_nextautotype,
    &mm_lib_converttoslice,
    &mm_lib_createslicemode,
    &mm_lib_createslicemodek,
    &mm_lib_createstringmode,
    &mm_lib_createrefmode,
    &mm_lib_createrefbitmode,
    &mm_lib_createsubrangemode,
    &mm_lib_createrefprocmode,
    &mm_lib_setnameptr,
    &mm_lib_getdottedname,
    &mm_lib_getavname,
    &mm_lib_unionstr_clear,
    &mm_lib_unionstr_append,
    &mm_lib_unionstr_concat,
    &mm_lib_unionstr_last,
    &mm_lib_unionstr_copy,
    &mm_lib_unionstr_print,
    &mm_lib_createrecordmode,
    &mm_lib_createenummode,
    &mm_lib_convertstring,
    &mm_lib_strexpr,
    &mm_lib_jeval,
    &mm_lib_getopcjname,
    &mm_lib_strmode,
    &mm_lib_strmode2,
    &mm_lib_istrmode,
    &mm_lib_countunits,
    &mm_lib_finddefstr,
    &mm_lib_addtoproclist,
    &mm_lib_addstatic,
    &mm_lib_newusertypex,
    &mm_lib_typename,
    &mm_lib_allocunitrec,
    &mm_lib_createdupldef,
    &mm_lib_createnewmoduledef,
    &mm_lib_storemode,
    &mm_lib_duplunit,
    &mm_lib_addstr,
    &mm_lib_addchar,
    &mm_lib_addint,
    &mm_lib_iscallbackfn,
    &mm_lib_isstringconst,
    &mm_lib_checkblockreturn,
    &mm_lib_strqvalue,
    &mm_lib_makeqvalue,
    &mm_lib_makeqvalue_ab,
    &mm_lib_isconstint,
    &mm_lib_isconstunit,
    &mm_lib_getfullname,
    &mm_lib_getownername,
    &mm_lib_isintmode,
    &mm_lib_isnumericmode,
    &mm_lib_isrefmode,
    &mm_lib_strconstopnd,
    &mm_lib_gettypecat_t,
    &mm_lib_getalignment,
    &mm_lib_ispoweroftwo,
    &mm_lib_addlistunit,
    &mm_lib_issimpletype,
    &mm_lib_getpacktype,
    &mm_lex_lexreadtoken,
    &mm_lex_lxreadstring,
    &mm_lex_readnumber,
    &mm_lex_readdecimalnumber,
    &mm_lex_readrealnumber,
    &mm_lex_readrealbest,
    &mm_lex_readexponent,
    &mm_lex_printsymbol,
    &mm_lex_stringtonumber,
    &mm_lex_stringtodecimalnumber,
    &mm_lex_lexsetup,
    &mm_lex_printstrn,
    &mm_lex_scannumber,
    &mm_lex_readrawstring,
    &mm_lex_lookup,
    &mm_lex_gethashvaluez,
    &mm_lex_inithashtable,
    &mm_lex_dolexdirective,
    &mm_lex_lexreadline,
    &mm_lex_startlex,
    &mm_lex_convertzstring,
    &mm_lex_addnamestr,
    &mm_lex_ps1,
    &mm_lex_ps2,
    &mm_lex_ps,
    &mm_lex_lex,
    &mm_lex_showhashtablesize,
    &mm_lex_checkname,
    &mm_lex_getstrfile,
    &mm_lex_stacksourcefile,
    &mm_lex_stacksource,
    &mm_lex_unstacksource,
    &mm_lex_readarraystring,
    &mm_lex_qadd,
    &mm_lex_qadddigit,
    &mm_lex_qshift,
    &mm_lex_qmul10,
    &mm_lex_qmulbase,
    &mm_lex_stringtonumber128,
    &mm_lex_setinttype,
    &mm_diags_printmodelist,
    &mm_diags_printst,
    &mm_diags_printstrec,
    &mm_diags_printstflat,
    &mm_diags_printcode,
    &mm_diags_printmodulecode,
    &mm_diags_printunit,
    &mm_diags_printunitlist,
    &mm_diags_getprefix,
    &mm_diags_getlineinfok,
    &mm_genc64_do_codegen1,
    &mm_genc64_do_codegen2,
    &mm_genc64_do_writesource,
    &mm_genc64_do_link,
    &mm_genc64_do_link_win,
    &mm_genc64_do_link_lin,
    &mm_genc64_showhelp,
    &mm_genc_codegen_clang,
    &mm_genc_do_infoheader,
    &mm_genc_do_cheader,
    &mm_genc_do_alltypes,
    &mm_genc_do_allprocdecls,
    &mm_genc_do_allvars,
    &mm_genc_do_allprocdefs,
    &mm_genc_do_typedef_fwd,
    &mm_genc_do_typedef,
    &mm_genc_do_procdecl,
    &mm_genc_do_vardef,
    &mm_genc_do_dllvar,
    &mm_genc_addsymbol,
    &mm_genc_scansymbol,
    &mm_genc_genlocalvar,
    &mm_genc_genprocdef,
    &mm_genc_writefn_nprocs,
    &mm_genc_writefn_nexports,
    &mm_genc_writefn_names,
    &mm_genc_writefn_addresses,
    &mm_genc_writefn_exports,
    &mm_libc_Dinit,
    &mm_libc_cccomment,
    &mm_libc_ccblank,
    &mm_libc_cclinecomment,
    &mm_libc_ccchar,
    &mm_libc_cctab,
    &mm_libc_ccstr,
    &mm_libc_ccstrline,
    &mm_libc_ccstrsemi,
    &mm_libc_ccstrsemiu,
    &mm_libc_ccsendline,
    &mm_libc_ccint,
    &mm_libc_ccinitline,
    &mm_libc_strmodec,
    &mm_libc_strmodec2,
    &mm_libc_strmodex,
    &mm_libc_strprocsig,
    &mm_libc_getprocname,
    &mm_libc_getfullnamec,
    &mm_libc_getfullnamec2,
    &mm_libc_genclabel,
    &mm_libc_genrecorddef,
    &mm_libc_genrecordfwd,
    &mm_libc_do_initdata,
    &mm_libc_cclongstr,
    &mm_libc_do_makelist,
    &mm_libc_issimplec,
    &mm_libc_strstringc,
    &mm_libc_do_syscallproc,
    &mm_libc_evalsysparam,
    &mm_libc_dxstr,
    &mm_libc_dxchar,
    &mm_libc_dxint,
    &mm_blockc_evalunit,
    &mm_blockc_do_block,
    &mm_blockc_do_blocklab,
    &mm_blockc_evalblock,
    &mm_blockc_evalblocklab,
    &mm_blockc_evalstmt,
    &mm_blockc_loneexpr,
    &mm_blockc_do_const,
    &mm_blockc_do_bin,
    &mm_blockc_do_binto,
    &mm_blockc_do_unary,
    &mm_blockc_do_if,
    &mm_blockc_do_longif,
    &mm_blockc_do_call,
    &mm_blockc_do_goto,
    &mm_blockc_do_do,
    &mm_blockc_definelabel,
    &mm_blockc_createfwdlabel,
    &mm_blockc_definefwdlabel,
    &mm_blockc_stacklooplabels,
    &mm_blockc_unstacklooplabels,
    &mm_blockc_findlooplabel,
    &mm_blockc_do_exit,
    &mm_blockc_do_to,
    &mm_blockc_do_while,
    &mm_blockc_do_repeat,
    &mm_blockc_do_forup,
    &mm_blockc_do_return,
    &mm_blockc_do_print,
    &mm_blockc_do_read,
    &mm_blockc_do_readln,
    &mm_blockc_do_convert,
    &mm_blockc_do_index,
    &mm_blockc_do_dot,
    &mm_blockc_do_swap,
    &mm_blockc_do_case,
    &mm_blockc_do_switch,
    &mm_blockc_do_max,
    &mm_blockc_do_maxto,
    &mm_blockc_do_blockcopy,
    &mm_blockc_do_select,
    &mm_blockc_do_maths,
    &mm_blockc_do_maths2,
    &mm_blockc_do_ifx,
    &mm_blockc_do_exprlist,
    &mm_blockc_do_inrange,
    &mm_blockc_do_inset,
    &mm_blockc_isexpr,
    &mm_blockc_dxlabel,
    &mm_parse_parsemodule,
    &mm_parse_readmoduledefs,
    &mm_parse_initparser,
    &mm_parse_skipsemi,
    &mm_parse_makeblock,
    &mm_parse_makestmtblock,
    &mm_parse_checkequals,
    &mm_parse_getcurrline,
    &mm_parse_checkbegin,
    &mm_parse_checkbeginend,
    &mm_parse_checkend,
    &mm_parse_readvardef,
    &mm_parse_readconstdef,
    &mm_parse_readlbrack,
    &mm_parse_addlistparam,
    &mm_parse_readcast,
    &mm_parse_readopc,
    &mm_parse_readsprint,
    &mm_parse_readsread,
    &mm_parse_readcompilervar,
    &mm_parse_readcastx,
    &mm_parse_checksymbol,
    &mm_parse_readtypespec,
    &mm_parse_readslicetype,
    &mm_parse_readflexarray,
    &mm_parse_readslist,
    &mm_parse_readindex,
    &mm_parse_readdotsuffix,
    &mm_parse_isconstexpr,
    &mm_parse_readkeyindex,
    &mm_parse_readconstexpr,
    &mm_parse_readconstint,
    &mm_parse_readprocdef,
    &mm_parse_readprocdecl,
    &mm_parse_readparams,
    &mm_parse_readparams_types,
    &mm_parse_readcondsuffix,
    &mm_parse_readif,
    &mm_parse_readgoto,
    &mm_parse_readunless,
    &mm_parse_readswitchcase,
    &mm_parse_readstop,
    &mm_parse_readreturn,
    &mm_parse_readdo,
    &mm_parse_readto,
    &mm_parse_readwhile,
    &mm_parse_readrepeat,
    &mm_parse_readloopcontrol,
    &mm_parse_readprint,
    &mm_parse_readread,
    &mm_parse_readtry,
    &mm_parse_readraise,
    &mm_parse_readfor,
    &mm_parse_readforall,
    &mm_parse_readtypedef,
    &mm_parse_readrecordfields,
    &mm_parse_readtabledef,
    &mm_parse_readclassdef,
    &mm_parse_readclassbody,
    &mm_parse_readenumtype,
    &mm_parse_duplfield,
    &mm_parse_readimportmodule,
    &mm_parse_readimportbody,
    &mm_parse_readequivfield,
    &mm_parse_readapplyop,
    &mm_parse_readrefproc,
    &mm_parse_pushproc,
    &mm_parse_popproc,
    &mm_parse_readassemline,
    &mm_parse_readassemblock,
    &mm_parse_assembleline,
    &mm_parse_makeastring,
    &mm_parse_readreturntype,
    &mm_parse_readset,
    &mm_parse_istypestarter,
    &mm_parse_readunit,
    &mm_parse_readfactor,
    &mm_parse_readterm2,
    &mm_parse_readterm,
    &mm_parse_readxunit,
    &mm_parse_readsunit,
    &mm_parse_readmacrodef,
    &mm_parse_readimportalias,
    &mm_parse_domappedalias,
    &mm_parse_readrecase,
    &mm_name_rx_unit,
    &mm_name_rx_module,
    &mm_name_rx_deflist,
    &mm_name_rx_passdef,
    &mm_name_rx_unitlist,
    &mm_name_resolvetopname,
    &mm_name_resolvename,
    &mm_name_finddupl,
    &mm_name_resolvedot,
    &mm_name_fixmode,
    &mm_name_fixmode2,
    &mm_name_fixusertypes,
    &mm_name_rx_assem,
    &mm_name_resolve_equiv_name,
    &mm_name_addframevar,
    &mm_name_converteqeq,
    &mm_name_copylistunit,
    &mm_name_copyunit,
    &mm_name_replaceunit,
    &mm_name_expandmacro,
    &mm_type_tpass,
    &mm_type_tx_block,
    &mm_type_tx_typetable,
    &mm_type_setmodesize,
    &mm_type_setarraysize,
    &mm_type_setslicesize,
    &mm_type_tcond,
    &mm_type_tx_module,
    &mm_type_tx_passdef,
    &mm_type_tx_unitlist,
    &mm_type_tx_namedef,
    &mm_type_tx_namedconst,
    &mm_type_tx_expr,
    &mm_type_checkconstexpr,
    &mm_type_getconstint,
    &mm_type_tevaluate,
    &mm_type_tevalbinop,
    &mm_type_tevalmonop,
    &mm_type_tevalconvert,
    &mm_type_makenewconst,
    &mm_type_tx_name,
    &mm_type_getdominantmode,
    &mm_type_getdominantmodepp,
    &mm_type_coerceunit,
    &mm_type_tx_add,
    &mm_type_tx_mul,
    &mm_type_tx_shl,
    &mm_type_tx_iand,
    &mm_type_tx_eq,
    &mm_type_tx_lt,
    &mm_type_tx_callproc,
    &mm_type_tx_neg,
    &mm_type_tx_if,
    &mm_type_tx_longif,
    &mm_type_tx_preincr,
    &mm_type_tx_for,
    &mm_type_tx_index,
    &mm_type_tx_makerange,
    &mm_type_tx_makeset,
    &mm_type_tx_makedict,
    &mm_type_tx_ptr,
    &mm_type_setrecordsize,
    &mm_type_scanrecord,
    &mm_type_tx_convert,
    &mm_type_tx_makelist,
    &mm_type_tx_dot,
    &mm_type_resolvefield,
    &mm_type_comparemodes,
    &mm_type_isboolunit,
    &mm_type_checkbool,
    &mm_type_tx_andl,
    &mm_type_convintconst,
    &mm_type_tx_upb,
    &mm_type_tx_len,
    &mm_type_tx_lwb,
    &mm_type_tx_bounds,
    &mm_type_tx_sliceptr,
    &mm_type_tx_inot,
    &mm_type_tx_atan2,
    &mm_type_tx_swap,
    &mm_type_tx_sqrt,
    &mm_type_tx_select,
    &mm_type_tx_case,
    &mm_type_tx_notl,
    &mm_type_tx_istruel,
    &mm_type_tx_addto,
    &mm_type_tx_iandto,
    &mm_type_tx_negto,
    &mm_type_tx_typepun,
    &mm_type_tx_bytesize,
    &mm_type_tx_exit,
    &mm_type_tx_goto,
    &mm_type_tx_switch,
    &mm_type_tx_power,
    &mm_type_tx_addroffirst,
    &mm_type_tx_minvalue,
    &mm_type_tx_return,
    &mm_type_tx_dotindex,
    &mm_type_tx_slice,
    &mm_type_tx_assign,
    &mm_type_tx_multassign,
    &mm_type_tx_in,
    &mm_type_getprocretmodes,
    &mm_type_tx_exprlist,
    &mm_type_tx_sign,
    &mm_type_fixvoidunit,
    &mm_type_twiden,
    &mm_type_twidenshort,
    &mm_type_tx_head,
    &mm_type_tx_concat,
    &mm_type_twidenopnd,
    &mm_type_joinstrings,
    &mm_type_removeaddrof,
    &mm_type_twholearrayslice,
    &mm_type_tarrayslice,
    &mm_type_tx_bitfield,
0};
static byte *  msysc__fnnames[]= {
    (byte*)"start",
    (byte*)"start_common",
    (byte*)"debugcompiler",
    (byte*)"do_loadmodules",
    (byte*)"do_parse",
    (byte*)"do_name",
    (byte*)"do_type",
    (byte*)"do_runprog",
    (byte*)"loadmainmodule",
    (byte*)"addmodule",
    (byte*)"loadimport",
    (byte*)"readimportlist",
    (byte*)"pslex",
    (byte*)"initdata",
    (byte*)"initsearchdirs",
    (byte*)"addsearchdir",
    (byte*)"showsearchdirs",
    (byte*)"showast",
    (byte*)"showstflat",
    (byte*)"showsttree",
    (byte*)"showtiming",
    (byte*)"getinputoptions",
    (byte*)"do_option",
    (byte*)"showcaption",
    (byte*)"addtolog",
    (byte*)"addoptionvar",
    (byte*)"addmodulemapping",
    (byte*)"dosetoptionvar",
    (byte*)"findoptionvar",
    (byte*)"getpsname",
    (byte*)"domapmodule",
    (byte*)"mapimport",
    (byte*)"do_writema",
    (byte*)"m_getdotindex",
    (byte*)"m_setdotindex",
    (byte*)"m_getdotslice",
    (byte*)"m_setdotslice",
    (byte*)"m_getnprocs",
    (byte*)"m_getnexports",
    (byte*)"m_getprocaddr",
    (byte*)"m_getprocname",
    (byte*)"m_getprocexport",
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
    (byte*)"pcm_alloc",
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
    (byte*)"checkpackfile",
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
    (byte*)"loadsourcefile",
    (byte*)"loadbuiltin",
    (byte*)"loadbundledfile",
    (byte*)"mcerror",
    (byte*)"serror_gen",
    (byte*)"stopcompiler",
    (byte*)"serror",
    (byte*)"serror_s",
    (byte*)"error_gen",
    (byte*)"rxerror",
    (byte*)"gerror",
    (byte*)"txerror",
    (byte*)"txerror_s",
    (byte*)"txerror_ss",
    (byte*)"rxerror_s",
    (byte*)"gerror_s",
    (byte*)"lxerror_gen",
    (byte*)"lxerror_s",
    (byte*)"lxerror",
    (byte*)"testelem",
    (byte*)"setelem",
    (byte*)"nextpoweroftwo",
    (byte*)"loaderror",
    (byte*)"gs_additem",
    (byte*)"gs_copytostr",
    (byte*)"isalphanum",
    (byte*)"inittypetables",
    (byte*)"addspecialtypes",
    (byte*)"findfile",
    (byte*)"findstdlib",
    (byte*)"getmainfile",
    (byte*)"getmodulefile",
    (byte*)"getsupportfile",
    (byte*)"writemafile",
    (byte*)"loadmafile",
    (byte*)"newstrec",
    (byte*)"initqclib",
    (byte*)"getduplnameptr",
    (byte*)"adddef",
    (byte*)"adddef_nodupl",
    (byte*)"createname",
    (byte*)"createunit0",
    (byte*)"createunit1",
    (byte*)"createunit2",
    (byte*)"createunit3",
    (byte*)"insertunit",
    (byte*)"deleteunit",
    (byte*)"createconstunit",
    (byte*)"createstringconstunit",
    (byte*)"getoptocode",
    (byte*)"createtype",
    (byte*)"createusertype",
    (byte*)"createusertypefromstr",
    (byte*)"getconstvalue",
    (byte*)"getrangelwbunit",
    (byte*)"getrangeupbunit",
    (byte*)"createarraymode",
    (byte*)"createflexarraymode",
    (byte*)"createarraymodek",
    (byte*)"createsetmode",
    (byte*)"createsetmodek",
    (byte*)"createdictmode",
    (byte*)"nextautotype",
    (byte*)"converttoslice",
    (byte*)"createslicemode",
    (byte*)"createslicemodek",
    (byte*)"createstringmode",
    (byte*)"createrefmode",
    (byte*)"createrefbitmode",
    (byte*)"createsubrangemode",
    (byte*)"createrefprocmode",
    (byte*)"setnameptr",
    (byte*)"getdottedname",
    (byte*)"getavname",
    (byte*)"unionstr_clear",
    (byte*)"unionstr_append",
    (byte*)"unionstr_concat",
    (byte*)"unionstr_last",
    (byte*)"unionstr_copy",
    (byte*)"unionstr_print",
    (byte*)"createrecordmode",
    (byte*)"createenummode",
    (byte*)"convertstring",
    (byte*)"strexpr",
    (byte*)"jeval",
    (byte*)"getopcjname",
    (byte*)"strmode",
    (byte*)"strmode2",
    (byte*)"istrmode",
    (byte*)"countunits",
    (byte*)"finddefstr",
    (byte*)"addtoproclist",
    (byte*)"addstatic",
    (byte*)"newusertypex",
    (byte*)"typename",
    (byte*)"allocunitrec",
    (byte*)"createdupldef",
    (byte*)"createnewmoduledef",
    (byte*)"storemode",
    (byte*)"duplunit",
    (byte*)"addstr",
    (byte*)"addchar",
    (byte*)"addint",
    (byte*)"iscallbackfn",
    (byte*)"isstringconst",
    (byte*)"checkblockreturn",
    (byte*)"strqvalue",
    (byte*)"makeqvalue",
    (byte*)"makeqvalue_ab",
    (byte*)"isconstint",
    (byte*)"isconstunit",
    (byte*)"getfullname",
    (byte*)"getownername",
    (byte*)"isintmode",
    (byte*)"isnumericmode",
    (byte*)"isrefmode",
    (byte*)"strconstopnd",
    (byte*)"gettypecat_t",
    (byte*)"getalignment",
    (byte*)"ispoweroftwo",
    (byte*)"addlistunit",
    (byte*)"issimpletype",
    (byte*)"getpacktype",
    (byte*)"lexreadtoken",
    (byte*)"lxreadstring",
    (byte*)"readnumber",
    (byte*)"readdecimalnumber",
    (byte*)"readrealnumber",
    (byte*)"readrealbest",
    (byte*)"readexponent",
    (byte*)"printsymbol",
    (byte*)"stringtonumber",
    (byte*)"stringtodecimalnumber",
    (byte*)"lexsetup",
    (byte*)"printstrn",
    (byte*)"scannumber",
    (byte*)"readrawstring",
    (byte*)"lookup",
    (byte*)"gethashvaluez",
    (byte*)"inithashtable",
    (byte*)"dolexdirective",
    (byte*)"lexreadline",
    (byte*)"startlex",
    (byte*)"convertzstring",
    (byte*)"addnamestr",
    (byte*)"ps1",
    (byte*)"ps2",
    (byte*)"ps",
    (byte*)"lex",
    (byte*)"showhashtablesize",
    (byte*)"checkname",
    (byte*)"getstrfile",
    (byte*)"stacksourcefile",
    (byte*)"stacksource",
    (byte*)"unstacksource",
    (byte*)"readarraystring",
    (byte*)"qadd",
    (byte*)"qadddigit",
    (byte*)"qshift",
    (byte*)"qmul10",
    (byte*)"qmulbase",
    (byte*)"stringtonumber128",
    (byte*)"setinttype",
    (byte*)"printmodelist",
    (byte*)"printst",
    (byte*)"printstrec",
    (byte*)"printstflat",
    (byte*)"printcode",
    (byte*)"printmodulecode",
    (byte*)"printunit",
    (byte*)"printunitlist",
    (byte*)"getprefix",
    (byte*)"getlineinfok",
    (byte*)"do_codegen1",
    (byte*)"do_codegen2",
    (byte*)"do_writesource",
    (byte*)"do_link",
    (byte*)"do_link_win",
    (byte*)"do_link_lin",
    (byte*)"showhelp",
    (byte*)"codegen_clang",
    (byte*)"do_infoheader",
    (byte*)"do_cheader",
    (byte*)"do_alltypes",
    (byte*)"do_allprocdecls",
    (byte*)"do_allvars",
    (byte*)"do_allprocdefs",
    (byte*)"do_typedef_fwd",
    (byte*)"do_typedef",
    (byte*)"do_procdecl",
    (byte*)"do_vardef",
    (byte*)"do_dllvar",
    (byte*)"addsymbol",
    (byte*)"scansymbol",
    (byte*)"genlocalvar",
    (byte*)"genprocdef",
    (byte*)"writefn_nprocs",
    (byte*)"writefn_nexports",
    (byte*)"writefn_names",
    (byte*)"writefn_addresses",
    (byte*)"writefn_exports",
    (byte*)"$init",
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
    (byte*)"evalunit",
    (byte*)"do_block",
    (byte*)"do_blocklab",
    (byte*)"evalblock",
    (byte*)"evalblocklab",
    (byte*)"evalstmt",
    (byte*)"loneexpr",
    (byte*)"do_const",
    (byte*)"do_bin",
    (byte*)"do_binto",
    (byte*)"do_unary",
    (byte*)"do_if",
    (byte*)"do_longif",
    (byte*)"do_call",
    (byte*)"do_goto",
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
    (byte*)"do_return",
    (byte*)"do_print",
    (byte*)"do_read",
    (byte*)"do_readln",
    (byte*)"do_convert",
    (byte*)"do_index",
    (byte*)"do_dot",
    (byte*)"do_swap",
    (byte*)"do_case",
    (byte*)"do_switch",
    (byte*)"do_max",
    (byte*)"do_maxto",
    (byte*)"do_blockcopy",
    (byte*)"do_select",
    (byte*)"do_maths",
    (byte*)"do_maths2",
    (byte*)"do_ifx",
    (byte*)"do_exprlist",
    (byte*)"do_inrange",
    (byte*)"do_inset",
    (byte*)"isexpr",
    (byte*)"dxlabel",
    (byte*)"parsemodule",
    (byte*)"readmoduledefs",
    (byte*)"initparser",
    (byte*)"skipsemi",
    (byte*)"makeblock",
    (byte*)"makestmtblock",
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
    (byte*)"readtypespec",
    (byte*)"readslicetype",
    (byte*)"readflexarray",
    (byte*)"readslist",
    (byte*)"readindex",
    (byte*)"readdotsuffix",
    (byte*)"isconstexpr",
    (byte*)"readkeyindex",
    (byte*)"readconstexpr",
    (byte*)"readconstint",
    (byte*)"readprocdef",
    (byte*)"readprocdecl",
    (byte*)"readparams",
    (byte*)"readparams_types",
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
    (byte*)"readtry",
    (byte*)"readraise",
    (byte*)"readfor",
    (byte*)"readforall",
    (byte*)"readtypedef",
    (byte*)"readrecordfields",
    (byte*)"readtabledef",
    (byte*)"readclassdef",
    (byte*)"readclassbody",
    (byte*)"readenumtype",
    (byte*)"duplfield",
    (byte*)"readimportmodule",
    (byte*)"readimportbody",
    (byte*)"readequivfield",
    (byte*)"readapplyop",
    (byte*)"readrefproc",
    (byte*)"pushproc",
    (byte*)"popproc",
    (byte*)"readassemline",
    (byte*)"readassemblock",
    (byte*)"assembleline",
    (byte*)"makeastring",
    (byte*)"readreturntype",
    (byte*)"readset",
    (byte*)"istypestarter",
    (byte*)"readunit",
    (byte*)"readfactor",
    (byte*)"readterm2",
    (byte*)"readterm",
    (byte*)"readxunit",
    (byte*)"readsunit",
    (byte*)"readmacrodef",
    (byte*)"readimportalias",
    (byte*)"domappedalias",
    (byte*)"readrecase",
    (byte*)"rx_unit",
    (byte*)"rx_module",
    (byte*)"rx_deflist",
    (byte*)"rx_passdef",
    (byte*)"rx_unitlist",
    (byte*)"resolvetopname",
    (byte*)"resolvename",
    (byte*)"finddupl",
    (byte*)"resolvedot",
    (byte*)"fixmode",
    (byte*)"fixmode2",
    (byte*)"fixusertypes",
    (byte*)"rx_assem",
    (byte*)"resolve_equiv_name",
    (byte*)"addframevar",
    (byte*)"converteqeq",
    (byte*)"copylistunit",
    (byte*)"copyunit",
    (byte*)"replaceunit",
    (byte*)"expandmacro",
    (byte*)"tpass",
    (byte*)"tx_block",
    (byte*)"tx_typetable",
    (byte*)"setmodesize",
    (byte*)"setarraysize",
    (byte*)"setslicesize",
    (byte*)"tcond",
    (byte*)"tx_module",
    (byte*)"tx_passdef",
    (byte*)"tx_unitlist",
    (byte*)"tx_namedef",
    (byte*)"tx_namedconst",
    (byte*)"tx_expr",
    (byte*)"checkconstexpr",
    (byte*)"getconstint",
    (byte*)"tevaluate",
    (byte*)"tevalbinop",
    (byte*)"tevalmonop",
    (byte*)"tevalconvert",
    (byte*)"makenewconst",
    (byte*)"tx_name",
    (byte*)"getdominantmode",
    (byte*)"getdominantmodepp",
    (byte*)"coerceunit",
    (byte*)"tx_add",
    (byte*)"tx_mul",
    (byte*)"tx_shl",
    (byte*)"tx_iand",
    (byte*)"tx_eq",
    (byte*)"tx_lt",
    (byte*)"tx_callproc",
    (byte*)"tx_neg",
    (byte*)"tx_if",
    (byte*)"tx_longif",
    (byte*)"tx_preincr",
    (byte*)"tx_for",
    (byte*)"tx_index",
    (byte*)"tx_makerange",
    (byte*)"tx_makeset",
    (byte*)"tx_makedict",
    (byte*)"tx_ptr",
    (byte*)"setrecordsize",
    (byte*)"scanrecord",
    (byte*)"tx_convert",
    (byte*)"tx_makelist",
    (byte*)"tx_dot",
    (byte*)"resolvefield",
    (byte*)"comparemodes",
    (byte*)"isboolunit",
    (byte*)"checkbool",
    (byte*)"tx_andl",
    (byte*)"convintconst",
    (byte*)"tx_upb",
    (byte*)"tx_len",
    (byte*)"tx_lwb",
    (byte*)"tx_bounds",
    (byte*)"tx_sliceptr",
    (byte*)"tx_inot",
    (byte*)"tx_atan2",
    (byte*)"tx_swap",
    (byte*)"tx_sqrt",
    (byte*)"tx_select",
    (byte*)"tx_case",
    (byte*)"tx_notl",
    (byte*)"tx_istruel",
    (byte*)"tx_addto",
    (byte*)"tx_iandto",
    (byte*)"tx_negto",
    (byte*)"tx_typepun",
    (byte*)"tx_bytesize",
    (byte*)"tx_exit",
    (byte*)"tx_goto",
    (byte*)"tx_switch",
    (byte*)"tx_power",
    (byte*)"tx_addroffirst",
    (byte*)"tx_minvalue",
    (byte*)"tx_return",
    (byte*)"tx_dotindex",
    (byte*)"tx_slice",
    (byte*)"tx_assign",
    (byte*)"tx_multassign",
    (byte*)"tx_in",
    (byte*)"getprocretmodes",
    (byte*)"tx_exprlist",
    (byte*)"tx_sign",
    (byte*)"fixvoidunit",
    (byte*)"twiden",
    (byte*)"twidenshort",
    (byte*)"tx_head",
    (byte*)"tx_concat",
    (byte*)"twidenopnd",
    (byte*)"joinstrings",
    (byte*)"removeaddrof",
    (byte*)"twholearrayslice",
    (byte*)"tarrayslice",
    (byte*)"tx_bitfield",
(byte*)""};
static struct msysc_procinforec msysc__fnexports[]= {
	{0, 0,0, {0,0,0, 0,0,0, 0,0,0, 0,0,0}}}
;
static i64 msysc__fnnprocs=708;
static i64 msysc__fnnexports=0;
static i64 msysc_fmtparam;
static i64 msysc_needgap = (i64)0;
static i64 msysc_outdev = (i64)1;
static void *  msysc_outchan = 0;
static byte *  msysc_fmtstr = 0;
static void *  msysc_outchan_stack[10];
static i64 msysc_outdev_stack[10];
static byte *  msysc_fmtstr_stack[10];
static byte msysc_needgap_stack[10];
static byte *  msysc_ptr_stack[10];
static i64 msysc_niostack = (i64)0;
static byte msysc_digits[16] = {
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
static struct msysc_fmtrec msysc_defaultfmt = {
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
static byte *  msysc_rd_buffer;
static i64 msysc_rd_length;
static byte *  msysc_rd_pos;
static byte *  msysc_rd_lastpos;
static i64 msysc_termchar;
static i64 msysc_itemerror;
i64 msysc_nsysparams;
byte *  msysc_sysparams[128];
static u64 msysc_callbackstack[9][8];
static i64 msysc_ncallbacks = (i64)0;
static u64 msysc_mask63 = (u64)9223372036854775807u;
static double msysc_offset64 = (double)9223372036854775800.;
static double msysc_offset32 = (double)9223372036854775800.;
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
static i64 oslinux_init_flag = (i64)0;
byte mm_support_bytemasks[8] = {(u8)1u,(u8)2u,(u8)4u,(u8)8u,(u8)16u,(u8)32u,(u8)64u,(u8)128u};
static i64 mm_lib_autotypeno = (i64)0;
i64 mm_lib_nextavindex = (i64)0;
static i64 mm_lib_nextsvindex = (i64)0;
static struct mlib_strbuffer mm_lib_exprstrvar;
static struct mlib_strbuffer *  mm_lib_exprstr = &mm_lib_exprstrvar;
static i64 mm_lib_opc_codes[32] = {
    (i64)37,
    (i64)38,
    (i64)39,
    (i64)40,
    (i64)103,
    (i64)30,
    (i64)31,
    (i64)32,
    (i64)33,
    (i64)34,
    (i64)35,
    (i64)44,
    (i64)45,
    (i64)46,
    (i64)105,
    (i64)47,
    (i64)48,
    (i64)9,
    (i64)10,
    (i64)12,
    (i64)144,
    (i64)145,
    (i64)146,
    (i64)147,
    (i64)159,
    (i64)153,
    (i64)154,
    (i64)138,
    (i64)140,
    (i64)139,
    (i64)141,
    (i64)0
};
static byte *  mm_lib_opc_names[32] = {
    (byte*)"+",
    (byte*)"-",
    (byte*)"*",
    (byte*)"/",
    (byte*)"-",
    (byte*)"=",
    (byte*)"<>",
    (byte*)"<",
    (byte*)"<=",
    (byte*)">",
    (byte*)">=",
    (byte*)"iand",
    (byte*)"ior",
    (byte*)"ixor",
    (byte*)"inot",
    (byte*)"<<",
    (byte*)">>",
    (byte*)"and",
    (byte*)"or",
    (byte*)"not",
    (byte*)"+:=",
    (byte*)"-:=",
    (byte*)"*:=",
    (byte*)"/:=",
    (byte*)"-:=",
    (byte*)"<<:=",
    (byte*)">>:=",
    (byte*)"++",
    (byte*)"++",
    (byte*)"--",
    (byte*)"--",
    (byte*)""
};
static struct mm_decls_unitrec *  mm_lib_unitheapptr = 0;
static i64 mm_lib_remainingunits = (i64)0;
static struct mlib_strbuffer mm_lib_sbuffer;
struct mlib_strbuffer *  mm_lib_dest = &mm_lib_sbuffer;
byte *  mm_lib_framevarname;
i64 mm_lex_nlookups;
i64 mm_lex_nclashes;
static byte *  mm_lex_lxstart_stack[20];
static byte *  mm_lex_lxsptr_stack[20];
static i64 mm_lex_lxfileno_stack[20];
static i64 mm_lex_lxlineno_stack[20];
static byte mm_lex_isfile_stack[20];
static i64 mm_lex_sourcelevel = (i64)0;
static byte *  mm_lex_lxstart;
static byte *  mm_lex_lxsptr;
static i64 mm_lex_lxifcond;
static i64 mm_lex_longsuffix;
struct mm_decls_strec mm_lex_hashtable[32768];
static byte *  mm_lex_maxnumlist[16] = {
    (byte*)"",
    (byte*)"1111111111111111111111111111111111111111111111111111111111111111",
    (byte*)"11112220022122120101211020120210210211220",
    (byte*)"33333333333333333333333333333333",
    (byte*)"2214220303114400424121122430",
    (byte*)"3520522010102100444244423",
    (byte*)"45012021522523134134601",
    (byte*)"1777777777777777777777",
    (byte*)"145808576354216723756",
    (byte*)"18446744073709551615",
    (byte*)"335500516A429071284",
    (byte*)"839365134A2A240713",
    (byte*)"219505A9511A867B72",
    (byte*)"8681049ADB03DB171",
    (byte*)"2C1D56B648C6CD110",
    (byte*)"FFFFFFFFFFFFFFFF"
};
static i64 mm_lex_maxnumlen[16];
static i64 mm_diags_currlineno;
byte *  mm_genc64_stdlibnames[5] = {(byte*)"msysc.m",(byte*)"mlib.m",(byte*)"clibc.m",(byte*)"oslinux.m",(byte*)"oswindllc.m"};
byte *  mm_genc64_stdlibtext[5] = {\
(byte*)"!MSYS version for C target\r\n\r\nimport clib\r\nimport mlib\r\n\r\nvar []ref void _fnaddresses\r\nvar []ichar _fnnames\r\nvar []procinforec _fnexports\r\nvar int _fnnprocs\r\nvar int _fnnexports\r\n\r\nglobal record procinforec=\r\n\tvar word16\t\tfnindex\r\n\tvar byte\t\trettype\r\n\tvar byte\t\tnparams\r\n\tvar [12]byte\tparamlist\r\nend\r\n\r\n!for print/read routines\r\n!------------------------------------------\r\nrecord fmtrec=\t! (default)\r\n\tvar byte\tminwidth\t! n (0)   min field width (0 if not used or don't care)\r\n\tvar i8\t\tprecision\t! .n (0)   number of decimals/significant figures/max width\r\n\tvar byte\tbase\t\t! B,H or Xn (10)  2 to 16\r\n\r\n\tvar char\tquotechar\t! Qc (0)   0 or '\"' or c\r\n\tvar char\tpadchar\t\t! Pc, Z (' ')\r\n\tvar char\trealfmt\t\t! E,F,G ('f') 'e' or 'f' or 'g'\r\n\r\n\tvar char\tplus\t\t! (0)   0 or '+'\r\n\tvar char\tsepchar\t\t! Sc (0)   0 or ',' or c placed every 3 (base=10) or 4 digits\r\n\tvar char\tlettercase\t! A,a ('A') 'A' or 'a'\r\n\tvar char\tjustify\t\t! JL, JR, JC ('R') 'L' or 'R' or 'C'?\r\n\tvar char\tsuffix\t\t! Tc (0)   0 or 'B' or 'H' or c\r\n\tvar char\tusigned\t\t! W (0)   0 or 'W' force unsigned o/p for ints (eg. for hex display)\r\n\tvar char\tcharmode\t! C,D (0)  0 or 'C' or 'D'\to/p int as int or single char or double/multi-char\r\n\tvar char\theapmode\t! M (0)  'M' for str-functions, return ptr tp heap string\r\n\tvar char\tparam\t\t! Use int value for <fmtparam>\r\n\tvar byte\tspare\r\nend\r\n\r\nvar int fmtparam\t\t\t!as set with :'V'\r\n\r\nenum (std_io,file_io,str_io)\r\n\r\nconst comma = ','\r\n\r\nvar int needgap\t\t\t= 0\r\nvar int outdev\t\t\t= std_io\r\nvar filehandle outchan\t= nil\r\nvar ref char fmtstr \t= nil\r\n\r\nconst maxiostack=10\r\nvar [maxiostack]filehandle\toutchan_stack\r\nvar [maxiostack]int\t\t\toutdev_stack\r\nvar [maxiostack]ref char\tfmtstr_stack\r\nvar [maxiostack]byte\t\tneedgap_stack\r\n\r\nvar [maxiostack]ref char\tptr_stack\t\t!this one doesn't need pushing, as each is pointed to from outchan\r\nvar int niostack=0\r\n\r\nvar [0:]char digits=A\"0123456789ABCDEF\"\r\nconst onesixty=360\r\nvar fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,0,0)\r\n\r\nconst smallstrlen=256\r\n\r\n!Read buffer vars\r\nconst rd_buffersize = 16384\t!total capacity of line buffer\r\n\r\nvar ref char rd_buffer\t\t! point to start of read buffer\r\nvar int rd_length\t\t\t! length of this line (as read by readln)\r\nvar ref char rd_pos\t\t\t! current position it's up to (next read starts here)\r\nvar ref char rd_lastpos\t\t! set by sread() just before reading used for reread()\r\nvar int termchar\t\t\t! terminator char set by readxxx()\r\nvar int itemerror\t\t\t!\tset by some read functions, eg for reals\r\n\r\n!------------------------------------------\r\n\r\nconst maxparam=128\r\nglobal var int nsysparams\r\nglobal var [maxparam]ichar sysparams\r\n\r\nconst maxcallback=8\r\nvar [0..maxcallback,8]word64 callbackstack\r\nvar int ncallbacks=0\r\n\r\nvar word64 mask63\t= 0x7FFF'FFFF'FFFF'FFFF\r\nvar real offset64\t= 9223372036854775808.0\t\t! 2**63 as r64\r\nvar real offset32\t= 9223372036854775808.0\t\t! 2**63 as r32\r\n\r\nglobal function m_getdotindex(word64 a,int i)int=\r\n!return (a iand (1dw<<i))>>i\r\nreturn (a iand (1<<i))>>i\r\nend\r\n\r\nglobal proc m_setdotindex(ref word64 a, int i,x)=\r\nvar ref word32 a32\r\n\r\n!see comments on setdotslice\r\nif i>=32 then\r\n!\ta^:=(a^ iand inot (1dw<<i)) ior (word64(x)<<i)\r\n\ta^:=(a^ iand inot (1<<i)) ior (word64(x)<<i)\r\nelse\r\n\ta32:=cast(a)\r\n\ta32^:=(a32^ iand inot (1<<i)) ior (word(x)<<i)\r\nfi\r\nend\r\n\r\nglobal function m_getdotslice(word64 a,int i,j)int=\r\nif i>=j then\r\n\treturn (a>>j)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(i-j+1))\r\nelse\r\n\treturn (a>>i)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))\r\nfi\r\nend\r\n\r\nglobal proc m_setdotslice(ref word64 a, int i,j,word64 x)=\r\n!a^:=(a^ iand inot (1dw<<i)) ior (word64(x)<<i)\r\nvar int w\r\nvar word64 mask64\r\nvar word mask\r\nvar ref word32 a32\r\n\r\nif i>j then println \"SETDOTSLICE?\"; stop 52 fi\r\n\r\n!when j>=32, assume 64 bit dest, otherwise assume 32 bits to avoid writing\r\n!to bytes beyond the 32-bit value\r\n!THIS WILL BE A PROBLEM IF writing to 8/16 bit values too\r\n\r\nif j>=32 then\r\n\tmask64:=inot((0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i\t\t\t!shifted field of w 1s\r\n\ta^:=(a^ iand inot mask64) ior x<<i\r\nelse\r\n\ta32:=cast(a)\r\n\tmask:=inot((0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i\t\t\t!shifted field of w 1s\r\n\ta32^:=(a32^ iand inot mask) ior x<<i\r\nfi\r\n\r\nend\r\n\r\nglobal function m_getnprocs:int=\r\n\treturn _fnnprocs\r\nend\r\n\r\nglobal function m_getnexports:int=\r\n\treturn _fnnexports\r\nend\r\n\r\nglobal function m_getprocaddr(int n)ref void=\r\n\treturn _fnaddresses[n]\r\nend\r\n\r\nglobal function m_getprocname(int n)ichar=\r\n\treturn _fnnames[n]\r\nend\r\n\r\nglobal function m_getprocexport(int n)ref void=\r\n\treturn &_fnexports[n]\r\nend\r\n\r\nproc pushio=\r\n\tif niostack>=maxiostack then\r\n\t\tprintf(\"Too many io levels\\n\")\r\n\t\tstop 53\r\n\tfi\r\n\t++niostack\r\n\toutchan_stack[niostack]\t:= outchan\r\n\toutdev_stack[niostack]\t:= outdev\r\n\tfmtstr_stack[niostack]\t:= fmtstr\r\n\tneedgap_stack[niostack]\t:= needgap\r\n\tneedgap:=0\r\n\tfmtstr:=nil\r\n\toutchan:=nil\r\nend\r\n\r\nglobal proc m_print_startfile(ref void dev)=\r\n\tpushio()\r\n\toutchan:=cast(dev)\r\n\tif dev then\r\n\t\toutdev:=file_io\r\n\telse\r\n\t\toutdev:=std_io\r\n\tfi\r\nend\r\n\r\nglobal proc m_print_startstr(ref char s)=\r\n\tvar ref ref char p\r\n\tpushio()\r\n\r\n\tptr_stack[niostack]:=s\r\n\tp:=&ptr_stack[niostack]\r\n\r\n\toutchan:=cast(p)\r\n\toutdev:=str_io\r\nend\r\n\r\nglobal proc m_print_startptr(ref ref char p)=\r\n\tpushio()\r\n\r\n\toutchan:=cast(p)\r\n\toutdev:=str_io\r\nend\r\n\r\nglobal proc m_print_startcon=\r\n\tpushio()\r\n\toutdev:=std_io\r\nend\r\n\r\nglobal proc m_print_setfmt(ref char format)=\r\n\tfmtstr:=format\r\nend\r\n\r\nglobal proc m_print_end=\r\n\tneedgap:=0\r\n\tnextfmtchars(1)\r\n\tif niostack=0 then return fi\r\n\toutchan\t:= outchan_stack[niostack]\r\n\toutdev\t:= outdev_stack[niostack]\r\n\tfmtstr\t:= fmtstr_stack[niostack]\r\n\tneedgap\t:= needgap_stack[niostack]\r\n\t--niostack\r\nend\r\n\r\nglobal proc m_print_ptr(ref void a,ichar fmtstyle=nil)=\r\n\tnextfmtchars()\r\n\r\n\tprintstr(strword(u64(a),\"z8h\"))\r\n\tneedgap:=1\r\nend\r\n\r\nglobal proc m_print_i64(int64 a,ichar fmtstyle=nil)=\r\n\tvar [40]char s\r\n\tvar fmtrec fmt\r\n\tvar int n\r\n\r\n\tnextfmtchars()\r\n\r\n\tif fmtstyle=nil then\r\n\t\tif a>=0 then\r\n\t\t\tn:=u64tostr(a,&.s,10,0)\r\n\t\telse\r\n\t\t\ts[1]:='-'\r\n\t\t\tn:=u64tostr(-a,&s[2],10,0)+1\r\n\t\tfi\r\n\t\tprintstr_n(&.s,n)\r\n\r\n\telse\r\n\t\tstrtofmt(fmtstyle,-1,&fmt)\r\n\t\tif fmt.param='V' then\r\n\t\t\tfmtparam:=a\r\n\t\t\tneedgap:=0\r\n\t\telse\r\n\t\t\ttostr_i64(a,&fmt)\r\n\t\tfi\r\n\tfi\r\n\tneedgap:=1\r\nend\r\n\r\nglobal proc m_print_u64(word64 a,ichar fmtstyle=nil)=\r\n\tvar fmtrec fmt\r\n\r\n\tnextfmtchars()\r\n\tif fmtstyle=nil then\r\n\t\tprintstr(strword(a))\r\n\telse\r\n\t\tstrtofmt(fmtstyle,-1,&fmt)\r\n\t\ttostr_u64(a,&fmt)\r\n\tfi\r\n\tneedgap:=1\r\nend\r\n\r\n!global proc m_print_i128(int128 a,ichar fmtstyle=nil)=\r\n!\tvar [40]char s\r\n!\tvar fmtrec fmt\r\n!\r\n!\tnextfmtchars()\r\n!\tstrtofmt(fmtstyle,-1,&fmt)\r\n!\tif a>=0 then\r\n!\t\ttostr_u128(a,&fmt,0)\r\n!\telse\r\n!\t\ttostr_u128(-a,&fmt,1)\r\n!\tfi\r\n!\r\n!\tneedgap:=1\r\n!end\r\n!\r\n!global proc m_print_u128(word128 a,ichar fmtstyle=nil)=\r\n!\tvar [40]char s\r\n!\tvar fmtrec fmt\r\n!\r\n!\tnextfmtchars()\r\n!\tstrtofmt(fmtstyle,-1,&fmt)\r\n!\ttostr_u128(a,&fmt,0)\r\n!\tneedgap:=1\r\n!end\r\n\r\nglobal proc m_print_r64(real x,ichar fmtstyle=nil)=\r\n\tvar [360]char s\r\n\tvar fmtrec fmt\r\n\r\n\tnextfmtchars()\r\n\tif fmtstyle=nil then\r\n\t\tsprintf(&.s,\"%f\",x)\r\n\t\tprintstr(&.s)\r\n\telse\r\n\t\tstrtofmt(fmtstyle,-1,&fmt)\r\n\t\ttostr_r64(x,&fmt)\r\n\tfi\r\n\r\n\tneedgap:=1\r\nend\r\n\r\nglobal proc m_print_r32(real32 x,ichar fmtstyle=nil)=\r\n\tm_print_r64(x,fmtstyle)\r\nend\r\n\r\nglobal proc m_print_c8(int64 a,ichar fmtstyle=nil)=\r\n\tvar [40]char s\r\n\tvar fmtrec fmt\r\n\tvar int n\r\n\r\n\tnextfmtchars()\r\n\r\n\ts[1]:=a\r\n\ts[2]:=0\r\n\tprintstr(&.s)\r\n\tneedgap:=1\r\nend\r\n\r\nglobal proc m_print_str(ichar s, fmtstyle=nil)=\r\n\tnextfmtchars()\r\n\tvar fmtrec fmt\r\n\tif fmtstyle=nil then\r\n\t\tprintstr(s)\r\n\telse\r\n\t\tstrtofmt(fmtstyle,-1,&fmt)\r\n\t\ttostr_str(s,&fmt)\r\n\tfi\r\n\tneedgap:=1\r\nend\r\n\r\nglobal proc m_print_newline=\r\n\tneedgap:=0\r\n\tnextfmtchars(1)\r\n\tprintstr(\"\\w\")\r\nend\r\n\r\nglobal proc m_print_nogap=\r\n\tneedgap:=0\r\nend\r\n\r\nproc printstr(ichar s)=\r\n\tvar int n\r\n\tvar ref ref char p\r\n\t\r\n\tcase outdev\r\n\twhen std_io then\r\n\t\tprintf(\"%s\",s)\r\n\twhen file_io then\r\n\t\tfprintf(outchan,\"%s\",s)\r\n\twhen str_io then\r\n\t\tp:=cast(outchan)\r\n\t\tstrcpy(p^,s)\r\n\t\tp^+:=strlen(s)\r\n\tesac\r\nend\r\n\r\nglobal proc printstr_n(ichar s,int n)=\r\n\tvar [smallstrlen]char str\r\n\tvar ref ref char p\r\n\r\n\tcase n\r\n\twhen -1 then n:=strlen(s)\t\t!assume zero-terminated\r\n\twhen 0 then return\r\n\tesac\r\n\r\n\tcase outdev\r\n\twhen str_io then\r\n\t\tp:=cast(outchan)\r\n\t\tmemcpy(p^,s,n)\r\n\t\tp^+:=n\r\n\t\tp^^:=0\r\n\twhen file_io then\r\n\t\ts:=makezstring(s,n,&.str)\r\n\t\tfprintf(outchan,\"%s\",s)\r\n\t\tfreezstring(s,n)\r\n\r\n\twhen std_io then\r\n\t\ts:=makezstring(s,n,&.str)\r\n\t\tprintf(\"%s\",s)\r\n\t\tfreezstring(s,n)\r\n!\t\tprintf(\"%.*s\",int32(n),s)\r\n\tesac\r\nend\r\n\r\nfunction makezstring(ichar s,int n,ichar local)ichar=\r\n\tvar ichar t\r\n\tif n<smallstrlen then\r\n\t\tmemcpy(local,s,n)\r\n\t\t(local+n)^:=0\r\n\t\treturn local\r\n\telse\r\n\t\tt:=pcm_alloc(n+1)\r\n\t\tmemcpy(t,s,n)\r\n\t\t(t+n)^:=0\r\n\t\treturn t\r\n\tfi\r\nend\r\n\r\nproc freezstring(ichar t,int n)=\r\n\tif n>=smallstrlen then\r\n\t\tpcm_free(t,n+1)\r\n\tfi\r\nend\r\n\r\nproc printchar(int ch)=\r\n\tvar ref ref char p\r\n\tcase outdev\r\n\twhen std_io then\r\n\t\temitc \"printf(\"\"%c\"\",(int)ch)\"\r\n\twhen file_io then\r\n\t\temitc \"fprintf(msysc_outchan,\"\"%c\"\",(int)ch)\"\r\n\twhen str_io then\r\n\t\tp:=cast(outchan)\r\n\t\tp^^:=ch\r\n\t\tp^+:=1\r\n\t\tp^^:=0\r\n\tesac\r\nend\r\n\r\nproc nextfmtchars(int lastx=0)=\r\n\tvar char c\r\n\tvar ref char pstart\r\n\tvar int n\r\n\r\n\tif not fmtstr then\t\t\t!format not in use\r\n\t\tif needgap then\r\n\t\t\tprintchar(' ')\r\n!\t\tprintstr_n(\" \",1)\r\n\t\tfi\r\n\t\tneedgap:=0\r\n\t\treturn\r\n\tfi\r\n\r\n\tpstart:=fmtstr\r\n\tn:=0\r\n\r\n\twhile (1) do\r\n\t\tc:=fmtstr^\r\n\t\tswitch c\r\n\t\twhen '#' then\r\n\t\t\tif lastx then\r\n\t\t\t\tgoto skip\r\n\t\t\tfi\r\n\t\t\t++fmtstr\r\n\t\t\tif n then\r\n\t\t\t\tprintstr_n(pstart,n)\r\n\t\t\tfi\r\n\t\t\treturn\r\n\t\twhen 0 then\r\n\t\t\tif n then\r\n\t\t\t\tprintstr_n(pstart,n)\r\n\t\t\telsif not lastx then\r\n\t\t\t\tprintstr_n(\"|\",1)\r\n\t\t\tfi\r\n\t\t\treturn\r\n\t\twhen '~' then\r\n\t\t\tif n then\r\n\t\t\t\tprintstr_n(pstart,n)\r\n\t\t\t\tn:=0\r\n\t\t\tfi\r\n\t\t\t++fmtstr\r\n\t\t\tc:=fmtstr^\r\n\t\t\tif c then\r\n\t\t\t\t++fmtstr\r\n\t\t\t\tprintchar(c)\r\n\t\t\tfi\r\n\t\t\tpstart:=fmtstr\r\n\t\telse\r\n\tskip::\r\n\t\t\t++n\r\n\t\t\t++fmtstr\r\n\t\tendswitch\r\n\tod\r\nend\r\n\r\nglobal proc strtofmt(ref char s,int slen,ref fmtrec fmt) =\t\t!PC_STRTOFMT\r\n!convert format code string in s, to fmtrec at fmt^\r\n!Format code is a string containing the following char codes (upper or lower when mostly)\r\n!n\tWidth\r\n!.n\tMax width/precision\r\n!A\tConvert to upper when\r\n!a\tConvert to lower when\r\n!B\tBinary\r\n!C\tShow int as single n-bit (unicode) character\r\n!D\tShow int as multi-bit (unicode) character\r\n!E,F,G\tSpecify format for double (corresponds to C format codes)\r\n!F\r\n!G\r\n!H\tHex\r\n!JC\tJustify centre\r\n!JL\tJustify left\r\n!JR\tJustify right\r\n!M\tHEAPMODE???\r\n!O\tOctal\r\n!Pc\tUse padding char c\r\n!Q\tAdd double quotes around string (and deal with embedded quotes)\r\n!'\tAdd single quotes around string (and deal with embedded quotes)\r\n!Sc\tUse separator char c between every 3 or 4 digits\r\n!Tc\tUse terminator char c (typically B or H)\r\n!U\tShow ints as unsigned\r\n!V\tFor ints, don't display: store value as parameter for subsequent '*'\r\n!W\tUnsigned\r\n!Xn\tUse base n (n is hex 0 to F)\r\n!Z\tUse \"0\" padding\r\n!+\tAlways have + or - in front of integers\r\n!~\tQuote char is ~\r\n!*\tSame as n but uses parameter set with :'V' on previous int\r\n\r\n\tvar char c\r\n\tvar byte wset\r\n\tvar int n\r\n\tvar [0:100]char str\r\n\r\n\tfmt^:=defaultfmt\r\n\r\n\tif s=nil then return fi\r\n\r\n\tif slen=-1 then slen:=strlen(s) fi\r\n\r\n\tmemcpy(&.str,s,slen)\t\t!convert s/slen to zero-terminated string\r\n\tstr[slen]:=0\r\n\ts:=&.str\r\n\r\n\twset:=0\r\n\twhile s^ do\r\n\t\tc:=s^\r\n\t\t++s\r\n\t\tswitch c\r\n\t\twhen 'B', 'b' then fmt^.base:=2\r\n\t\twhen 'H', 'h' then fmt^.base:=16\r\n\t\twhen 'O', 'o' then fmt^.base:=8\r\n\t\twhen 'X', 'x' then\r\n\t\t\tc:=s^\r\n\t\t\tif c then\r\n\t\t\t\tswitch c\r\n\t\t\t\twhen '0'..'9' then c:=c-'0'\r\n\t\t\t\twhen 'A'..'F' then c:=c-'A'+10\r\n\t\t\t\twhen 'a'..'f' then c:=c-'a'+10\r\n\t\t\t\telse\r\n\t\t\t\t\tc:=10\r\n\t\t\t\tend\r\n\t\t\t\tfmt^.base:=c\r\n\t\t\t\t++s\r\n\t\t\tfi\r\n\t\twhen 'Q', 'q' then fmt^.quotechar:='\"'\r\n\t\twhen '~' then fmt^.quotechar:='~'\r\n\t\twhen 'J', 'j' then\r\n\t\t\tfmt^.justify:=toupper(s^)\r\n\t\t\tif s^ then\r\n\t\t\t\t++s\r\n\t\t\tfi\r\n\t\twhen 'A' then fmt^.lettercase:='A'\r\n\t\twhen 'a' then fmt^.lettercase:='a'\r\n\t\twhen 'Z', 'z' then fmt^.padchar:='0'\r\n\t\twhen 'S', 's' then\r\n\t\t\tfmt^.sepchar:=s^\r\n\t\t\tif s^ then\r\n\t\t\t\t++s\r\n\t\t\tfi\r\n\t\twhen 'P', 'p' then\r\n\t\t\tfmt^.padchar:=s^\r\n\t\t\tif s^ then\r\n\t\t\t\t++s\r\n\t\t\tfi\r\n\t\twhen 'T', 't' then\r\n\t\t\tfmt^.suffix:=s^\r\n\t\t\tif s^ then\r\n\t\t\t\t++s\r\n\t\t\tfi\r\n\t\twhen 'W', 'w' then fmt^.usigned:='W'\r\n\t\twhen 'E', 'e' then fmt^.realfmt:='e'\r\n\t\twhen 'F', 'f' then fmt^.realfmt:='f'\r\n\t\twhen 'G', 'g' then fmt^.realfmt:='g'\r\n\t\twhen '.' then\r\n\t\t\twset:=1\r\n\t\twhen comma,'_' then fmt^.sepchar:=c\r\n\t\twhen '+' then fmt^.plus:='+'\r\n\t\twhen 'D', 'd' then fmt^.charmode:='D'\r\n\t\twhen 'C', 'c' then fmt^.charmode:='C'\r\n\t\twhen 'M', 'm' then fmt^.heapmode:='M'\r\n\t\twhen 'V','v' then fmt.param:='V'\r\n\t\twhen '*' then\r\n\t\t\tn:=fmtparam\r\n\t\t\tgoto gotwidth\r\n\t\telse\r\n\t\t\tif c>='0' and c<='9' then\r\n\t\t\t\tn:=c-'0'\r\n\t\t\t\tdo\r\n\t\t\t\t\tc:=s^\r\n\t\t\t\t\tif s^=0 then\r\n\t\t\t\t\t\texit\r\n\t\t\t\t\tfi\r\n\t\t\t\t\tif c>='0' and c<='9' then\r\n\t\t\t\t\t\t++s\r\n\t\t\t\t\t\tn:=n*10+c-'0'\r\n\t\t\t\t\telse\r\n\t\t\t\t\t\texit\r\n\t\t\t\t\tfi\r\n\t\t\t\tod\r\ngotwidth::\r\n\t\t\t\tif not wset then\r\n\t\t\t\t\tfmt^.minwidth:=n\r\n\t\t\t\t\twset:=1\r\n\t\t\t\telse\r\n\t\t\t\t\tfmt^.precision:=n\r\n\t\t\t\tfi\r\n\t\t\tfi\r\n\t\tendswitch\r\n\tod\r\nend\r\n\r\nfunction domultichar (ref char p,int n,ref char dest,ref fmtrec fmt)int =\r\n!there are n (4 or 8) chars at p.!\r\n!There could be 0 to 4 or 8 printable chars converted to string at dest\r\n\tvar [0:20]char str\r\n\tvar ref char q\r\n\tvar int i,nchars\r\n\r\n\tq:=&.str\r\n\r\n\tnchars:=n\r\n\r\n\tto n do\r\n\t\tif p^=0 then exit fi\r\n\t\tq^:=p^\r\n\t\t++q\r\n\t\t++p\r\n\tod\r\n\tq^:=0\r\n\r\n\treturn expandstr(&.str,dest,strlen(&.str),fmt)\r\nend\r\n\r\nfunction expandstr(ref char s,ref char t,int n,ref fmtrec fmt)int =\t\t!EXPANDSTR\r\n!s contains a partly stringified value.\r\n!widen s if necessary, according to fmt, and copy result to t\r\n!n is current length of s\r\n!note) = for non-numeric strings, fmt^.base should be set to 0, to avoid moving\r\n!a leading +/- when right-justifying with '0' padding.\r\n!t MUST be big enough for the expanded string; caller must take care of this\r\n!result will be zero-terminated, for use in this module\r\n\r\n\tvar int i,w,m\r\n\r\n!check to see if result is acceptable as it is\r\n\tw:=fmt^.minwidth\r\n\tif w=0 or w<=n then\t\t! allow str to be longer than minwidth\r\n\t\tstrncpy(t,s,n)\r\n\t\t(t+n)^:=0\r\n\t\treturn n\r\n\tfi\r\n\r\n\tif fmt^.justify='L' then\t! left-justify\r\n\t\tstrncpy(t,s,n)\r\n\t\tt+:=n\r\n\t\tfor i:=1 to w-n do\r\n\t\t\tt^:=fmt^.padchar\r\n\t\t\t++t\r\n\t\tod\r\n\t\tt^:=0\r\n\telsif fmt^.justify='R' then\r\n\t\tif fmt^.padchar='0' and fmt^.base and (s^='-' or s^='+') then ! need to move sign outside \r\n\t\t\tt^:=s^\r\n\t\t\t++t\r\n\t\t\tto w-n do\r\n\t\t\t\tt^:=fmt^.padchar\r\n\t\t\t\t++t\r\n\t\t\tod\r\n\t\t\tstrncpy(t,s+1,n-1)\r\n\t\t\t(t+n-1)^:=0\r\n\t\telse\r\n\t\t\tto w-n do\r\n\t\t\t\tt^:=fmt^.padchar\r\n\t\t\t\t++t\r\n\t\t\tod\r\n\t\t\tstrncpy(t,s,n)\r\n\t\t\t(t+n)^:=0\r\n\t\tfi\r\n\r\n\telse\t\t\t\t! centre-justify?\r\n\r\n\t\tm:=(w-n+1)/2\r\n\t\tto m do\r\n\t\t\tt^:=fmt^.padchar\r\n\t\t\t++t\r\n\t\tod\r\n\t\tstrncpy(t,s,n)\r\n\t\tt+:=n\r\n\t\tto w-n-m do\r\n\t\t\tt^:=fmt^.padchar\r\n\t\t\t++t\r\n\t\tod\r\n\t\tt^:=0\r\n\r\n\tfi\r\n\treturn w\r\nend\r\n\r\n!function xdivrem(word64 a,b)word64,word64=\r\n!\tassem\r\n!\t\txor rdx,rdx\r\n!\t\tmov rax,[a]\r\n!\t\tdiv qword [b]\r\n!\t\tmov D1,rdx\r\n!\tend\r\n!end\r\n\r\nfunction xdivrem(word64 a,b, &remainder)word64=\r\n\tvar word64 q,r\r\nABORTPROGRAM(\"XDIVREM\")\r\n!\tassem\r\n!\t\txor rdx,rdx\r\n!\t\tmov rax,[a]\r\n!\t\tdiv qword [b]\r\n!\t\tmov [q],rax\t\r\n!\t\tmov [r],rdx\t\r\n!\tend\r\n!\tremainder:=r\r\n\treturn q\r\nend\r\n\r\nfunction u64tostr(u64 aa,ref char s,word base,int sep)int =\t\t!U64TOSTR\r\n!convert 64-bit int a to string in s^\r\n!base is number base, usually 10 but can be 2 or 16. Other bases allowed\r\n!result when a=minint (will give \"<minint>\")\r\n\tvar [0:onesixty]char t\r\n\tvar u64 dd\r\n\tvar int i,j,k,g\r\n\tvar int dummy\r\n\tvar ref char s0\r\n\r\n\ti:=0\r\n\tk:=0\r\n\tg:=(base=10|3|4)\r\n\r\n\trepeat\r\n!\t\taa:=xdivrem(aa,base,dd)\r\n!\t\tt[++i]:=digits[dd]\r\n\r\n\t\tt[++i]:=digits[aa rem base]\r\n\t\taa:=aa/base\r\n\r\n!BUG in separator logic, doesn't work when leading zeros used, eg. printing\r\n!out a full length binary\r\n!so perhaps move this out to expandstr\r\n\t\t++k\r\n\t\tif sep and aa<>0 and k=g then\r\n\t\t\tt[++i]:=sep\r\n\t\t\tk:=0\r\n\t\tfi\r\n\tuntil aa=0\r\n\r\n\tj:=i\r\n\ts0:=s\r\n\twhile i do\r\n\t\ts^:=t[i--]\r\n\t\t++s\r\n\tod\r\n\ts^:=0\r\n\r\n\treturn j\r\nend\r\n\r\n!function u128tostr(u128 aa,ref char s,word base,int sep)int =\r\n!!convert 128-bit int a to string in s^\r\n!!base is number base, usually 10 but can be 2 to 16\r\n!\tvar [0:160]char t\r\n!\tvar u64 dd\r\n!\tvar int i,j,k,g\r\n!\tvar int dummy\r\n!\tvar ref char s0\r\n!\r\n!\ti:=0\r\n!\tk:=0\r\n!\tg:=(base=10|3|4)\r\n!\r\n!\trepeat\r\n!\t\taa:=xdivrem128(aa,base,dd)\r\n!\t\tt[++i]:=digits[dd]\r\n!\r\n!!\t\tt[++i]:=digits[aa rem base]\r\n!!\t\taa:=aa/base\r\n!\r\n!!BUG in separator logic, doesn't work when leading zeros used, eg. printing\r\n!!out a full length binary\r\n!!so perhaps move this out to expandstr\r\n!\t\t++k\r\n!\t\tif sep and aa<>0 and k=g then\r\n!\t\t\tt[++i]:=sep\r\n!\t\t\tk:=0\r\n!\t\tfi\r\n!\tuntil aa=0\r\n!\r\n!\tj:=i\r\n!\ts0:=s\r\n!\twhile i do\r\n!\t\ts^:=t[i--]\r\n!\t\t++s\r\n!\tod\r\n!\ts^:=0\r\n!\r\n!\treturn j\r\n!end\r\n!\r\n!function xdivrem128(word128 a, word64 b, &remainder)word128=\r\n!\tvar word128 d,e,r\r\n!\tvar word rlow\r\n!\r\n!\td:=a/b\r\n!\tr:=a-d*b\r\n!\r\n!\tassem\r\n!\t\tmov d0,[r]\r\n!\t\tmov [rlow],d0\r\n!\tend\r\n!\tremainder:=rlow\r\n!\treturn d\r\n!end\r\n\r\nfunction i64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =\r\n!a is signed 64-bit int/long, fmt is a ref to a filled-in fmtrec\r\n!convert a to a string in s, according to fmt\r\n!a basic conversion is done first,: the field manipulation is done\r\n!signed=1 for int, 0 for u32 (fmt^.unsigned forces ints to be treated as longs)\r\n!returns length of s\r\n\tvar [0:onesixty]char str\t\t\t\t! allow for binary with separators!\r\n\tvar int i,j,k,n,w,usigned\r\n\tstatic var u64 mindint=0x8000'0000'0000'0000\r\n\r\n\tusigned:=0\r\n\tif fmt^.usigned then\r\n\t\tusigned:=1\r\n\tfi\r\n\r\n\tif aa=mindint and not usigned then\t\t! minint\r\n\r\n\t\tstr[0]:='-'\r\n\t\tn:=i64mintostr(&str[1],fmt^.base,fmt^.sepchar)+1\r\n\telse\r\n\t\tif (not usigned and aa<-0) or fmt^.plus then\r\n\t\t\tif aa<0 then\r\n\t\t\t\taa:=-aa\r\n\t\t\t\tstr[0]:='-'\r\n\t\t\telse\r\n\t\t\t\tstr[0]:='+'\r\n\t\t\tfi\r\n\t\t\tn:=u64tostr(aa,&str[1],fmt^.base,fmt^.sepchar)+1\r\n\t\telse\r\n\t\t\tn:=u64tostr(aa,&.str,fmt^.base,fmt^.sepchar)\r\n\t\tfi\r\n\tfi\r\n\r\n\tif fmt^.suffix then\r\n\t\tstr[n]:=fmt^.suffix\r\n\t\tstr[++n]:=0\r\n\tfi\r\n\r\n!str uses upper cases for hex/etc see if lc needed\r\n\tif (fmt^.base>10 or fmt^.suffix) and fmt^.lettercase='a'\tthen\t! need lower when\r\n\t\tconvlcstring(&.str)\r\n\tfi\r\n\r\n!at this point, n is the str length including signs and suffix\r\n\treturn expandstr(&.str,s,n,fmt)\r\nend\r\n\r\nfunction u64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =\t\t!U64TOSTRFMT\r\n!see i64tostrfmt\r\n\tvar [0:onesixty]char str\t\t\t\t! allow for binary with separators!\r\n\tvar int i,j,k,n,w\r\n\r\n\tn:=u64tostr(aa,&.str,fmt^.base,fmt^.sepchar)\r\n\r\n\tif fmt^.suffix then\r\n\t\tstr[n]:=fmt^.suffix\r\n\t\tstr[++n]:=0\r\n\tfi\r\n\r\n!str uses upper cases for hex/etc see if lc needed\r\n\tif fmt^.base>10 or fmt^.suffix and fmt^.lettercase='a'\tthen\t! need lower when\r\n\t\tconvlcstring(&.str)\r\n\tfi\r\n\r\n!at this point, n is the str length including signs and suffix\r\n\treturn expandstr(&.str,s,n,fmt)\r\nend\r\n\r\n!function u128tostrfmt(i128 aa,ref char s,ref fmtrec fmt)int =\t\t!U64TOSTRFMT\r\n!!see i64tostrfmt\r\n!\tvar [0:onesixty]char str\t\t\t\t! allow for binary with separators!\r\n!\tvar int i,j,k,n,w\r\n!\r\n!\tn:=u128tostr(aa,&.str,fmt^.base,fmt^.sepchar)\r\n!\r\n!\tif fmt^.suffix then\r\n!\t\tstr[n]:=fmt^.suffix\r\n!\t\tstr[++n]:=0\r\n!\tfi\r\n!\r\n!!str uses upper cases for hex/etc see if lc needed\r\n!\tif fmt^.base>10 or fmt^.suffix and fmt^.lettercase='a'\tthen\t! need lower when\r\n!\t\tconvlcstring(&.str)\r\n!\tfi\r\n!\r\n!!at this point, n is the str length including signs and suffix\r\n!\treturn expandstr(&.str,s,n,fmt)\r\n!end\r\n\r\nfunction i64mintostr(ref char s,int base,int sep)int =\t\t!I64MINTOSTR\r\n!convert minint to string in s do not include minus sign\r\n!return number of chars in string\r\n\tvar [0:onesixty]char t\r\n\tvar int i,j,k,g,neg\r\n\r\n\tswitch base\r\n\twhen 10 then\r\n\t\tstrcpy(&t[0],\"9223372036854775808\")\r\n\t\tj:=3\r\n\twhen 16 then\r\n\t\tstrcpy(&t[0],\"8000000000000000\")\r\n\t\tj:=1\r\n\twhen 2 then\r\n\t\tstrcpy(&t[0],\"1000000000000000000000000000000000000000000000000000000000000000\")\r\n\t\tj:=7\r\n\telse\r\n\t\tstrcpy(&t[0],\"<mindint>\")\r\n\tendswitch\r\n\r\n\ti:=strlen(&t[0])\r\n\ts+:=i\r\n\tif sep then\r\n\t\ts+:=j\r\n\tfi\r\n\ts^:=0\r\n\r\n\tk:=0\r\n\tg:=(base=10|3|4)\r\n\r\n\twhile i do\r\n\t\t--s\r\n\t\ts^:=t[i-- -1]\r\n\t\tif sep and i and ++k=g then\r\n\t\t\t--s\r\n\t\t\ts^:=sep\r\n\t\t\tk:=0\r\n\t\tfi\r\n\tod\r\n\treturn strlen(s)\r\nend\r\n\r\nfunction strtostrfmt(ref char s,ref char t,int n,ref fmtrec fmt)int =\r\n!s is a string process according to fmtrec fmt^, and return result in t\r\n!caller should check whether any changes are required to s (now it can just use s), but this\r\n!check is done here anyway (with a simple copy to t)\r\n!n is current length of s\r\n!return length of t\r\n!Three processing stages:\r\n!1 Basic input string s\r\n!2 Additions or mods: quotes, suffix, when conversion\r\n!3 Width adjustment\r\n!1 is detected here, 2 is done here, 3 is done by expandstr\r\n\tvar ref char u,v\r\n\tvar [256]char str\r\n\tvar int w,nheap\t\t! whether any heap storage is used # bytes allocated\r\n\r\n\tnheap:=0\r\n\r\n\tif fmt^.quotechar or fmt^.lettercase then\t\t! need local copy\r\n\t\tif n<256 then\r\n\t\t\tu:=&.str\r\n\t\telse\r\n\t\t\tnheap:=n+3\t\t\t\t\t! allow for quotes+terminator\r\n\t\t\tu:=pcm_alloc(nheap)\r\n\t\tfi\r\n\t\tif fmt^.quotechar then\r\n\t\t\tv:=u\r\n\t\t\tv^:=fmt^.quotechar\r\n\t\t\t++v\r\n\t\t\tif n then\r\n\t\t\t\tstrcpy(v,s)\r\n\t\t\t\tv+:=n\r\n\t\t\tfi\r\n\t\t\tv^:=fmt^.quotechar\r\n\t\t\t++v\r\n\t\t\tv^:=0\r\n\t\t\tn+:=2\r\n\t\telse\r\n\t\t\tmemcpy(u,s,n)\r\n\t\tfi\r\n\t\tswitch fmt^.lettercase\r\n\t\twhen 'a' then\t! need lower when\r\n\t\t\tconvlcstring(u)\r\n\t\twhen 'A' then\r\n\t\t\tconvucstring(u)\r\n\t\tendswitch\r\n\t\ts:=u\r\n\tfi\r\n\r\n\tw:=fmt^.minwidth\r\n\tif w>n then\r\n\t\tn:=expandstr(s,t,n,fmt)\r\n\telse\r\n\t\tmemcpy(t,s,n)\r\n\tfi\r\n\tif nheap then\r\n\t\tpcm_free(u,nheap)\r\n\tfi\r\n\treturn n\r\nend\r\n\r\nproc tostr_i64(int64 a, ref fmtrec fmt)=\r\n\tvar [360]char str\r\n\tvar int n\r\n\r\n\tcase fmt^.charmode\r\n\twhen 0 then\r\n\t\tn:=i64tostrfmt(a,&.str,fmt)\r\n\twhen 'D','d' then\r\n\t\tn:=domultichar(ref char(&a),8,&.str,fmt)\r\n\r\n\telse\t\t\t\t\t\t!assume 'C'\r\n\t\tprintchar(a)\t\t\t!no other formatting allowed\r\n\t\treturn\r\n\tesac\r\n\r\n\tprintstr_n(&.str,n)\r\nend\r\n\r\nproc tostr_u64(word64 a, ref fmtrec fmt)=\r\n\tvar [360]char str\r\n\tvar int n\r\n\r\n\tcase fmt^.charmode\r\n\twhen 'D','d' then\r\n\t\tn:=domultichar(ref char(&a),8,&.str,fmt)\r\n\r\n\twhen 'C','c' then\r\n\t\tprintchar(a)\t\t\t!no other formatting allowed\r\n\t\treturn\r\n\r\n\telse\r\n\t\tn:=u64tostrfmt(a,&.str,fmt)\r\n\tesac\r\n\r\n\tprintstr_n(&.str,n)\r\nend\r\n\r\n!proc tostr_u128(word128 a, ref fmtrec fmt,int neg)=\r\n!\tvar [360]char str\r\n!\tvar int n\r\n!\r\n!\tcase fmt^.charmode\r\n!\twhen 'D','d' then\r\n!\t\tn:=domultichar(ref char(&a),8,&.str,fmt)\r\n!\r\n!\twhen 'C','c' then\r\n!\t\tprintchar(a)\t\t\t!no other formatting allowed\r\n!\t\treturn\r\n!\r\n!\telse\r\n!\t\tif neg then\r\n!\t\t\tstr[1]:='-'\r\n!\t\t\tn:=u128tostrfmt(a,&str[2],fmt)+1\r\n!\t\telse\r\n!\t\t\tn:=u128tostrfmt(a,&.str,fmt)\r\n!\t\tfi\r\n!\tesac\r\n!\r\n!\tprintstr_n(&.str,n)\r\n!end\r\n\r\nproc tostr_r64(real x,ref fmtrec fmt) =\r\n\tvar [360]char str,str2\r\n\tvar [0:10]char cfmt\r\n\tvar int n\r\n\r\n\tcfmt[0]:='%'\r\n\r\n\tif fmt^.precision then\r\n\t\tcfmt[1]:='.'\r\n\t\tcfmt[2]:='*'\r\n\t\tcfmt[3]:=fmt^.realfmt\r\n\t\tcfmt[4]:=0\r\n\t\tsprintf(&.str,&.cfmt,fmt^.precision,x)\r\n\telse\r\n\t\tcfmt[1]:=fmt^.realfmt\r\n\t\tcfmt[2]:=0\r\n\t\tsprintf(&.str,&.cfmt,x)\r\n\tfi\r\n\r\n!at this point, n is the str length including signs and suffix\r\n\r\n\tn:=strlen(&.str)\t\t! current length\r\n\r\n\tif n<fmt^.minwidth then\r\n\t\tn:=expandstr(&.str,&.str2,n,fmt)\r\n\t\tstrcpy(&.str,&.str2)\r\n\tfi\r\n\r\n\tprintstr_n(&.str,n)\r\nend\r\n\r\nproc tostr_str(ref char s, ref fmtrec fmt) =\r\n\tvar int oldlen,newlen,n\r\n\tvar ref char t\r\n\r\n!try and work out size of formatted string\r\n\toldlen:=strlen(s)\r\n\tnewlen:=oldlen\r\n\r\n\tif fmt^.quotechar or fmt^.minwidth>newlen or fmt^.lettercase then\r\n\t\tif fmt^.quotechar then\r\n\t\t\tnewlen+:=2\r\n\t\tfi\r\n\t\tif fmt^.minwidth>newlen then\r\n\t\t\tnewlen:=fmt^.minwidth\r\n\t\tfi\r\n\t\tt:=pcm_alloc(newlen+1)\r\n\t\tn:=strtostrfmt(s,t,oldlen,fmt)\r\n\t\tprintstr_n(t,n)\r\n\t\tpcm_free(t,newlen+1)\r\n\telse\r\n\t\tprintstr_n(s,oldlen)\r\n\tfi\r\nend\r\n\r\nfunction getfmt(ichar fmtstyle)ref fmtrec=\r\n\tstatic var fmtrec fmt\r\n\tif fmtstyle then\r\n\t\tstrtofmt(fmtstyle,-1,&fmt)\r\n\t\treturn &fmt\r\n\telse\r\n\t\treturn &defaultfmt\r\n\tfi\r\nend\r\n\r\nglobal function strint(int64 a, ichar fmtstyle=nil)ichar=\r\n\tstatic var[100]char str\r\n\tvar ref fmtrec fmt\r\n\r\n\tm_print_startstr(&.str)\r\n\ttostr_i64(a,fmt:=getfmt(fmtstyle))\r\n\tm_print_end()\r\n\treturn getstr(&.str,fmt)\r\nend\r\n\r\nglobal proc getstrint(int64 a, ichar dest)=\r\n\tm_print_startstr(dest)\r\n\ttostr_i64(a,getfmt(nil))\r\n\tm_print_end()\r\nend\r\n\r\nglobal function strword(word64 a, ichar fmtstyle=nil)ichar=\r\n\tstatic var[100]char str\r\n\tvar ref fmtrec fmt\r\n\r\n\tm_print_startstr(&.str)\r\n\ttostr_u64(a,fmt:=getfmt(fmtstyle))\r\n\tm_print_end()\r\n\treturn getstr(&.str,fmt)\r\nend\r\n\r\nglobal function strreal(real a, ichar fmtstyle=nil)ichar=\r\n\tstatic var [320]char str\r\n\tvar ref fmtrec fmt\r\n\r\n\tm_print_startstr(&.str)\r\n\ttostr_r64(a,fmt:=getfmt(fmtstyle))\r\n\tm_print_end()\r\n\treturn getstr(&.str,fmt)\r\nend\r\n\r\nfunction getstr(ichar s, ref fmtrec fmt)ichar=\r\n\tif fmt^.heapmode then\r\n\t\treturn pcm_copyheapstring(s)\r\n\telse\r\n\t\treturn s\r\n\tfi\r\nend\r\n\r\nproc initreadbuffer=\r\n\tif rd_buffer then return fi\r\n!CPL \"INITREADBUFFER\"\r\n\trd_buffer:=pcm_alloc(rd_buffersize)\r\n\trd_buffer^:=0\r\n\trd_pos:=rd_lastpos:=rd_buffer\r\nend\r\n\r\nglobal proc m_read_conline=\r\n\tinitreadbuffer()\r\n\treadlinen(nil,rd_buffer,rd_buffersize)\r\n\r\n\trd_length:=strlen(rd_buffer)\r\n\trd_pos:=rd_buffer\r\n\trd_lastpos:=nil\r\nend\r\n\r\nglobal proc m_read_fileline(filehandle f)=\r\n\tinitreadbuffer()\r\n\treadlinen(f,rd_buffer,rd_buffersize)\r\n\r\n\trd_length:=strlen(rd_buffer)\r\n\trd_pos:=rd_buffer\r\n\trd_lastpos:=nil\r\nend\r\n\r\nglobal proc m_read_strline(ichar s)=\r\n\tvar int n\r\n\r\n\tinitreadbuffer()\r\n\tn:=strlen(s)\r\n\r\n\tif n<rd_buffersize then\r\n\t\tstrcpy(rd_buffer,s)\r\n\telse\r\n\t\tmemcpy(rd_buffer,s,rd_buffersize-1)\r\n\t\t(rd_buffer+rd_buffersize-1)^:=0\r\n\tfi\r\n\trd_length:=n\r\n\trd_pos:=rd_buffer\r\n\trd_lastpos:=nil\r\nend\r\n\r\nfunction readitem(int &itemlength)ref char =\r\n!read next item from rd_buffer\r\n!identify a substring that can contain a name, int, real, string or filename\r\n!return updated position of s that points past the item and past the immediate\r\n!terminator \r\n!information about the read item is returned in itemstr, which points to\r\n!the start of the item, and in itemlength. Item excludes any surrounding whitespace\r\n!Item can be quoted, then the item points inside the quotes\r\n!Any embedded quotes are removed, and the characters moved up. The item will\r\n!be that reduced subsequence\r\n!NOTE THAT THIS IS DESTRUCTIVE. On reread, the input will be different.\r\n!I can mitigate this by adding spaces between the end of the item, and the next item,\r\n!overwriting also the terminator. But this won't restore the line if one of the next\r\n!reads is literal, using 'L' or 'C' codes.\r\n\tvar ref char p,s,itemstr\r\n\tvar char quotechar, c\r\n\r\n\tunless rd_buffer then \r\n\t\tinitreadbuffer()\r\n!abortprogram(\"No readln\")\r\n\tend unless\r\n\r\n\ts:=rd_pos\r\n\r\n!scan string, eliminating leading white space\r\n\twhile s^=' ' or s^=9 do\r\n\t\t++s\r\n\tod\r\n\r\n\titemstr:=s\t\t\t\t!assume starts here\r\n\trd_lastpos:=rd_pos:=s\r\n\r\n\tif s^=0 then\t\t\t! No more chars left to read return null string\r\n\t\ttermchar:=0\r\n\t\titemlength:=0\r\n\t\treturn s\r\n\tfi\r\n\r\n\tquotechar:=0\t\t\t! Allow possible enclosing single or double quotes\r\n\tif s^='\"' then\r\n\t\tquotechar:='\"'\r\n\t\t++s\r\n\telsif s^='\\'' then\r\n\t\tquotechar:='\\''\r\n\t\t++s\r\n\tfi\r\n\r\n!loop reading characters until separator or end reached\r\n\tp:=itemstr:=s\r\n\r\n\twhile s^ do\r\n\t\tc:=s++^\r\n\t\tswitch c\r\n\t\twhen ' ', 9, comma, '=' then\t\t! separator\r\n\t\t\tif quotechar or p=s then\t\t\t!can be considered part of name if inside quotes, or is only char\r\n\t\t\t\tgoto normalchar\r\n\t\t\tfi\r\n\t\t\ttermchar:=c\r\n\t\t\texit\r\n\t\telse\r\n\tnormalchar::\r\n\t\t\tif c=quotechar then\r\n\t\t\t\tif s^=quotechar then\t! embedded quote\r\n\t\t\t\t\tp^:=c\r\n\t\t\t\t\t++s\r\n\t\t\t\t\t++p\r\n\t\t\t\telse\t\t\t\t\t! end of name\r\n\t\t\t\t\ttermchar:=s^\r\n\t\t\t\t\tif termchar=',' or termchar='=' then\r\n\t\t\t\t\t\t++s\r\n\t\t\t\t\t\ttermchar:=s^\r\n\t\t\t\t\tfi\r\n\t\t\t\t\texit\r\n\t\t\t\tfi\r\n\t\t\telse\r\n\t\t\t\tp^:=c\r\n\t\t\t\t++p\r\n\t\t\tfi\r\n\t\tendswitch\r\n\tod\r\n\r\n\tif s^=0 then\r\n\t\ttermchar:=0\r\n\tfi\r\n\titemlength:=p-itemstr\t\t\t\t! actual length of token\r\n\trd_pos:=s\r\n\r\n\treturn itemstr\r\nend\r\n\r\nfunction strtoint(ichar s,int length, base=10)int64=\r\n!return point to next char after terminator (which can be just off length of string)\r\n\tvar byte signd\r\n\tvar word64 aa\r\n\tvar char c,d\r\n\r\n\titemerror:=0\r\n\r\n!check for sign\r\n\tsignd:=0\r\n\tif length and s^='-' then\r\n\t\tsignd:=1; ++s; --length\r\n\telsif length and s^='+' then\r\n\t\t++s; --length\r\n\tfi\r\n\r\n\taa:=0\r\n\twhile length do\r\n\t\tc:=s++^\r\n\t\t--length\r\n\t\tswitch c\r\n\t\twhen 'A'..'F' then d:=c-'A'+10\r\n\t\twhen 'a'..'f' then d:=c-'a'+10\r\n\t\twhen '0'..'9' then d:=c-'0'\r\n\t\twhen '_', '\\'' then\r\n\t\t\tnext\r\n\t\telse\r\n\t\t\titemerror:=1\r\n\t\t\texit\r\n\t\tendswitch\r\n\r\n\t\tif d>=base then\r\n\t\t\titemerror:=1\r\n\t\t\texit\r\n\t\tfi\r\n\t\taa:=aa*base+d\r\n\tod\r\n\r\n\tif signd then\r\n\t\treturn -aa\r\n\telse\r\n\t\treturn aa\r\n\tfi\r\nend\r\n\r\nglobal function m_read_i64(int fmt=0)int64=\r\n\tvar ref char s\r\n\tvar int length,c\r\n\tvar int64 aa\r\n\r\n\tcase fmt\r\n\twhen 'C','c' then\r\n\t\trd_lastpos:=rd_pos\r\n\t\tif rd_pos^ then\r\n\t\t\treturn rd_pos++^\r\n\t\telse\r\n\t\t\treturn 0\r\n\t\tfi\r\n\twhen 'T','t' then\r\n\t\treturn termchar\r\n\twhen 'E','e' then\r\n\t\treturn itemerror\r\n\tesac\r\n\r\n\ts:=readitem(length)\r\n\r\n\r\n\tcase fmt\r\n\twhen 0,'I','i' then\r\n\t\treturn strtoint(s,length)\r\n\twhen 'B','b' then\r\n\t\treturn strtoint(s,length,2)\r\n\twhen 'H','h' then\r\n\t\treturn strtoint(s,length,16)\r\n\tesac\r\n\treturn 0\r\nend\r\n\r\nglobal function m_read_r64(int fmt=0)real=\r\n\tvar [512]char str\r\n\tvar ref char s\r\n\tvar int length\r\n\tvar int32 numlength\r\n\tvar real x\r\n\r\n\ts:=readitem(length)\r\n\r\n\tif length=0 or length>=str.len then\t\t!assume not a real\r\n\t\treturn 0.0\r\n\tfi\r\n\tmemcpy(&.str,s,length)\r\n\tstr[length+1]:=0\r\n\r\n\titemerror:=0\r\n\r\n\tif sscanf(&.str,\"%lf%n\", &x, &numlength)=0 or numlength<>length then\r\n\t\tx:=0.0\r\n\t\titemerror:=1\r\n\tfi\r\n\r\n\treturn x\r\nend\r\n\r\nglobal proc m_read_str(ref char dest, int destlen=0,fmt=0)=\r\n\tvar ref char s\r\n\tvar int length,numlength\r\n\tvar real x\r\n\r\n\titemerror:=0\r\n\tif fmt='L' or fmt='l' then\r\n\t\ts:=rd_pos\r\n\t\tlength:=rd_buffer+rd_length-rd_pos\r\n\r\n\telse\r\n\t\ts:=readitem(length)\r\n\r\n\t\tif fmt='N' or fmt='n' then\r\n\t\t\ticonvlcn(s,length)\r\n\t\tfi\r\n\tfi\r\n\r\n\tif destlen>0 then\r\n\t\tif length>=destlen then\r\n\t\t\tlength:=destlen-1\r\n\t\t\titemerror:=1\r\n\t\tfi\r\n\tfi\r\n\tmemcpy(dest,s,length)\r\n\t(dest+length)^:=0\r\nend\r\n\r\nglobal proc readstr(ref char dest, int fmt=0,destlen=0)=\r\n\tm_read_str(dest,destlen,fmt)\r\nend\r\n\r\nglobal proc rereadln=\r\n\trd_pos:=rd_buffer\r\n\trd_lastpos:=rd_pos\r\nend\r\n\r\nglobal proc reread=\r\n\trd_pos:=rd_lastpos\r\nend\r\n\r\nglobal function valint(ichar s, int fmt=0)int64=\r\nvar ref char old_pos, old_lastpos\r\nvar int64 aa\r\n\r\ninitreadbuffer()\r\nold_pos:=rd_pos\r\nold_lastpos:=rd_lastpos\r\n\r\nrd_pos:=s\r\naa:=m_read_i64(fmt)\r\nrd_pos:=old_pos\r\nrd_lastpos:=old_lastpos\r\nreturn aa\r\nend\r\n\r\nglobal function valreal(ichar s)real=\r\nvar ref char old_pos, old_lastpos\r\nvar real x\r\n\r\ninitreadbuffer()\r\nold_pos:=rd_pos\r\nold_lastpos:=rd_lastpos\r\n\r\nrd_pos:=s\r\nx:=m_read_r64()\r\nrd_pos:=old_pos\r\nrd_lastpos:=old_lastpos\r\nreturn x\r\nend\r\n\r\nproc iconvlcn(ref char s,int n) =\t\t!ICONVLCN\r\nto n do\r\n\ts^:=tolower(s^)\r\n\t++s\r\nod\r\nend\r\n\r\nproc iconvucn(ref char s,int n) =\t\t!ICONVUCN\r\nto n do\r\n\ts^:=toupper(s^)\r\n\t++s\r\nod\r\nend\r\n\r\nproc convlcstring(ref char s)=\t\t!CONVLCSTRING\r\nwhile (s^) do\r\n\ts^:=tolower(s^)\r\n\t++s\r\nod\r\nend\r\n\r\nproc convucstring(ref char s)=\t\t!CONVUCSTRING\r\nwhile (s^) do\r\n\ts^:=toupper(s^)\r\n\t++s\r\nod\r\nend\r\n\r\nglobal function m_power_i64(int64 n,a)int64=\r\nif n<0 then\r\n\treturn 0\r\nelsif n=0 then\r\n\treturn 1\r\nelsif n=1 then\r\n\treturn a\r\nelsif (n iand 1)=0 then\r\n!\treturn ipower(a*a,n/2)\r\n\treturn m_power_i64(n/2,sqr a)\r\nelse\t\t\t!assume odd\r\n\treturn m_power_i64((n-1)/2,sqr a)*a\r\nfi\r\nend\r\n\r\nglobal proc m_intoverflow=\r\nabortprogram(\"Integer overflow detected\")\r\nend\r\n\r\nglobal proc m_dotindex(word i,a)=\r\n!return a.[i] in d0\r\nABORTPROGRAM(\"DOT INDEX\")\r\n!\tassem\r\n!\t\tmov d0,[a]\r\n!\t\tmov cl,[i]\r\n!\t\tshr d0,cl\r\n!\t\tand d0,1\r\n!\tend\t\r\nend\r\n\r\nglobal proc m_dotslice(word j,i,a)=\r\n!return a.[i..j] in d0; assumes j>=i\r\nABORTPROGRAM(\"DOT SLICE\")\r\n!\tassem\r\n!\t\tmov d0,[a]\r\n!\t\tmov rcx,[i]\r\n!\t\tshr d0,cl\r\n!\t\tsub rcx,[j]\r\n!\t\tneg rcx\t\t\t\t!j-1\r\n!\t\tmov d2,0xFFFF'FFFF'FFFF'FFFE\r\n!\t\tshl d2,cl\r\n!\t\tnot d2\r\n!\t\tand d0,d2\r\n!\tend\t\r\nend\r\n\r\nglobal proc m_popdotindex(word i,ref word p,word x)=\r\n!p^.[i]:=x\r\nABORTPROGRAM(\"POP DOT INDEX\")\r\n!\tassem\r\n!\t\tmov d3,[p]\r\n!\t\tmov cl,[i]\r\n!\t\tmov d0,[d3]\r\n!\t\tmov d1,1\r\n!\t\tshl d1,cl\t\t\t!000001000\r\n!\t\tnot d1\t\t\t\t!111110111\r\n!\t\tand d0,d1\t\t\t!clear that bit in dest\r\n!\t\tmov d1,[x]\r\n!\t\tand d1,1\r\n!\t\tshl d1,cl\r\n!\t\tor d0,d1\r\n!\t\tmov [d3],d0\r\n!\tend\t\r\nend\r\n\r\nglobal proc m_popdotslice(word j,i, ref word p, word x)=\r\n!p^.[i..j]:=x\r\nABORTPROGRAM(\"POP DOT SLICE\")\r\n!\tassem\r\n!!d3 = p\r\n!!d4 = x, then shifted then masked x\r\n!!d5 = i\r\n!!d6 = clear mask\r\n!\r\n!\t\tmov d3,[p]\r\n!\t\tmov d4,[x]\r\n!\t\tmov d5,[i]\r\n!\t\tmov rcx,d5\t\t\t!i\r\n!\t\tshl d4,cl\t\t\t!x<<i\r\n!\t\tmov rcx,[j]\r\n!\t\tsub rcx,d5\t\t\t!j-i\r\n!\t\tinc rcx\t\t\t\t!j-i+1\r\n!\t\tmov d2,0xFFFF'FFFF'FFFF'FFFF\r\n!\t\tshl d2,cl\t\t\t!...111100000     (assume 5-bit slice)\r\n!\t\tnot d2\t\t\t\t!...000011111\r\n!\t\tmov rcx,d5\t\t\t!i\r\n!\t\tshl d2,cl\t\t\t!...000011111000  (assume i=3)\r\n!\t\tand d4,d2\t\t\t!mask x (truncate extra bits)\r\n!\t\tmov d0,[d3]\r\n!\t\tnot d2\t\t\t\t!...111100000111\r\n!\t\tand d0,d2\t\t\t!clear dest bits\r\n!\t\tor d0,d4\t\t\t!add in new bits\r\n!\t\tmov [d3],d0\r\n!\tend\t\r\nend\r\n\r\nglobal function m_imin(int64 a,b)int64=\r\nreturn min(a,b)\r\nend\r\n\r\nglobal function m_imax(int64 a,b)int64=\r\nreturn max(a,b)\r\nend\r\n\r\nglobal function m_sign(real x)real=\r\n\tif x>0.0 then return 1.0\r\n\telsif x<0.0 then return -1.0\r\n\telse return 0.0\r\n\tfi\r\nend\r\n",\
(byte*)"import msys\r\nimport clib\r\nimport oslib\r\n\r\n!const mem_check=1\r\nconst mem_check=0\r\n\r\nglobal var [0..300]u64 allocupper\r\nglobal var int alloccode\t\t\t\t!set by heapalloc\r\nglobal var int allocbytes\t\t\t\t!set by heapalloc\r\nglobal var int fdebug=0\r\nglobal var int rfsize\r\n\r\nconst threshold=1<<25\r\nconst alloc_step=1<<25\r\nvar word maxmemory\r\nvar int  maxalloccode\r\n\r\nvar byte pcm_setup=0\r\n\r\nvar int show=0\r\n\r\nglobal var int memtotal=0\r\nglobal var int64 smallmemtotal=0\r\nglobal var int smallmemobjs=0\r\nglobal var int maxmemtotal=0\r\n\r\n!store all allocated pointers\r\nconst int maxmemalloc=500000\r\nvar [maxmemalloc+1]ref int32 memalloctable\r\nvar [maxmemalloc+1]int32 memallocsize\r\n\r\nconst pcheapsize=1048576*2\r\nvar ref byte pcheapstart\r\nvar ref byte pcheapend\t\t\t!points to first address past heap\r\nvar ref byte pcheapptr\r\n\r\nconst int maxblockindex = 8 \t\t!2048\r\nglobal const int maxblocksize = 2048\r\n\r\nvar [0:maxblocksize+1]byte sizeindextable\t!convert byte size to block index 1..maxblockindex\r\n\r\nconst int size16   = 1\t\t\t!the various index codes\r\nconst int size32   = 2\r\nconst int size64   = 3\r\nconst int size128  = 4\r\nconst int size256  = 5\r\nconst int size512  = 6\r\nconst int size1024 = 7\r\nconst int size2048 = 8\r\n\r\nGLOBAL var [0:9]ref wordp freelist\r\n\r\nglobal record strbuffer =\r\n\tvar ichar strptr\r\n\tvar int32 length\r\n\tvar int32 allocated\r\nend\r\n\r\nglobal tabledata() [0:]ichar pmnames=\r\n\t(pm_end=0,\t\t$),\r\n\t(pm_option,\t\t$),\r\n\t(pm_sourcefile,\t$),\r\n\t(pm_libfile,\t$),\r\n\t(pm_colon,\t\t$),\r\n\t(pm_extra,\t\t$),\r\nend\r\n\r\nvar [2]int seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)\r\n\r\nglobal function pcm_alloc(int n)ref void =\t\t!PCM_ALLOC\r\nvar ref byte p\r\n!int i\r\n\r\nif not pcm_setup then\r\n\tpcm_init()\r\n!\tabortprogram(\"need pcm_init\")\r\nfi\r\n\r\nif n>maxblocksize then\t\t\t!large block allocation\r\n\talloccode:=pcm_getac(n)\r\n\tallocbytes:=allocupper[alloccode]\r\n\r\n\tp:=allocmem(allocbytes)\r\n\tif not p then\r\n\t\tabortprogram(\"pcm_alloc failure\")\r\n\tfi\r\n\r\nif mem_check then addtomemalloc(ref int32(p),allocbytes) fi\r\n\r\n\treturn p\r\nfi\r\n\r\nalloccode:=sizeindextable[n]\t\t!Size code := 0,1,2 etc for 0, 16, 32 etc\r\n\r\nif alloccode=0 then\t\t\t\t\t!sizes below 16 bytes (can I adjust sizeindextable to?)\r\n\talloccode:=1\r\nfi\r\nallocbytes:=allocupper[alloccode]\r\n\r\nSMALLMEMTOTAL+:=ALLOCBYTES\r\n\r\nif p:=ref byte(freelist[alloccode]) then\t\t!Items of this block size available\r\nif mem_check then addtomemalloc(ref int32(p),allocbytes) fi\r\n\tfreelist[alloccode]:=ref wordp(int((freelist[alloccode])^))\r\n\r\n\treturn p\r\nfi\r\n\r\n!No items in freelists: allocate new space in this heap block\r\np:=pcheapptr\t\t\t\t!Create item at start of remaining pool in heap block\r\npcheapptr+:=allocbytes\t\t\t!Shrink remaining pool\r\n\r\nif pcheapptr>=pcheapend then\t\t!Overflows?\r\n\tp:=pcm_newblock(allocbytes)\t\t!Create new heap block, and allocate from start of that\r\n\treturn p\r\nfi\r\nif mem_check then addtomemalloc(ref int32(p),allocbytes) fi\r\n\r\nreturn p\r\nend\r\n\r\nglobal proc pcm_free(ref void p,int n) =\t\t!PCM_FREE\r\n!n can be the actual size requested it does not need to be the allocated size\r\nvar int acode\r\n\r\nif n=0 then return fi\r\n\r\nif n>maxblocksize then\t\t!large block\r\n\tif mem_check then removefrommemalloc(p,n) fi\r\n\r\n\tfree(p)\r\n\treturn\r\nfi\r\n\r\nif p then\r\n\tacode:=sizeindextable[n]\t\t!Size code := 0,1,2 etc for 0, 16, 32 etc\r\n\r\n\tsmallmemtotal-:=allocupper[acode]\r\n\r\n\tif mem_check then removefrommemalloc(p,allocupper[acode]) fi\r\n\r\n\t(ref wordp(p))^:=wordp(int(freelist[acode]))\r\n\tfreelist[acode]:=p\r\nfi\r\nend\r\n\r\nglobal proc pcm_freeac(ref void p,int alloc) =\t\t!PCM_FREEAC\r\npcm_free(p,allocupper[alloc])\r\nend\r\n\r\nglobal proc pcm_copymem4(ref void p,q,int n) =\t!PCM_COPYMEM4\r\n!copy n bytes of memory from q to p.\r\n!the memory spaces used are multiples of 16 bytes, but n itself could be anything\r\n!n can be zero, and need not be a multiple of 4 bytes\r\n\r\nmemcpy(p,q,n)\r\nend\r\n\r\nglobal proc pcm_clearmem(ref void p,int n) =\t\t!PCM_CLEARMEM\r\nmemset(p,0,n)\r\nend\r\n\r\nglobal proc pcm_init =\t\t!PCM_INIT\r\n!set up sizeindextable too\r\n!sizeindextable[0] = 0\r\nvar int j,k,k1,k2\r\nvar int64 size\r\nconst limit=1<<33\r\n\r\nif pcm_setup then\r\n\treturn\r\nfi\r\n\r\npcm_newblock(0)\r\n\r\nfor i to maxblocksize do\t!table converts eg. 78 to 4 (4th of 16,32,64,128)\r\n\tj:=1\r\n\tk:=16\r\n\twhile i>k do\r\n\t\tk:=k<<1\r\n\t\t++j\r\n\tod\r\n\tsizeindextable[i]:=j\r\nod\r\n\r\nallocupper[1]:=16\r\nsize:=16\r\n\r\nfor i:=2 to 27 do\r\n\tsize*:=2\r\n\tallocupper[i]:=size\r\n\tif size>=threshold then\r\n\t\t\tk:=i\r\n\t\texit\r\n\tfi\r\nod\r\n\r\nfor i:=k+1 to allocupper.upb do\r\n\tsize+:=alloc_step\r\n!\tif size>4 billion then\r\n!\t\tsize+:=alloc_step\r\n!\tfi\r\n\tif size<limit then\r\n\t\tallocupper[i]:=size\r\n\t\tmaxmemory:=size\r\n\telse\r\n\t\tmaxalloccode:=i-1\r\n\t\texit\r\n\tfi\r\n\t\t\r\nod\r\npcm_setup:=1\r\nend\r\n\r\nglobal function pcm_getac(int size)int =\t\t!PCM_GETAC\r\n! convert linear blocksize from 0..approx 2GB to 8-bit allocation code\r\n\r\n!sizeindextable scales values from 0 to 2048 to allocation code 0 to 9\r\n\r\nif size<=maxblocksize then\r\n\treturn sizeindextable[size]\t\t!size 0 to 2KB\r\nfi\r\n\r\nsize:=(size+255)>>8\t\t\t\t\t!scale by 256\r\n\r\n!now same sizetable can be used for 2KB to 512KB (288 to 2KB)\r\n\r\nif size<=maxblocksize then\r\n\treturn sizeindextable[size]+8\r\nfi\r\n\r\n!sizetable now used for 512KB to 128MB (to 2KB)\r\nsize:=(size+63)>>6\t\t\t\t\t!scale by 256\r\n\r\nif size<=maxblocksize then\r\n\treturn sizeindextable[size]+14\r\nfi\r\n\r\n\r\n!size>2048, which means it had been over 128MB.\r\n\r\nsize:=(size-2048+2047)/2048+22\r\nreturn size\r\nend\r\n\r\nglobal function pcm_newblock(int itemsize)ref void=\r\n!create new heap block (can be first)\r\n!also optionally allocate small item at start\r\n!return pointer to this item (and to the heap block)\r\nstatic var int totalheapsize\r\nvar ref byte p\r\n\r\ntotalheapsize+:=pcheapsize\r\nalloccode:=0\r\np:=allocmem(pcheapsize)\t!can't free this block until appl terminates\r\nif p=nil then\r\n\tabortprogram(\"Can't alloc pc heap\")\r\nfi\r\n\r\npcheapptr:=p\r\npcheapend:=p+pcheapsize\r\n\r\nif pcheapstart=nil then\t\t!this is first block\r\n\tpcheapstart:=p\r\nfi\r\npcheapptr+:=itemsize\r\nreturn ref u32(p)\r\nend\r\n\r\nglobal function pcm_round(int n)int =\t\t!PCM_ROUND\r\n!for any size n, return actual number of bytes that would be allocated\r\nstatic var [0:maxblockindex+1]int32 allocbytes=(0,16,32,64,128,256,512,1024,2048)\r\n\r\nif n>maxblocksize then\r\n\treturn n\r\nelse\r\n\treturn allocbytes[sizeindextable[n]]\r\nfi\r\nend\r\n\r\nglobal function pcm_array(int n)int =\t\t!PCM_ARRAY\r\n!n bytes are needed for an array return the number of bytes to be actually allocated\r\nvar int m\r\n\r\nif n<=maxblocksize then\t!automatic rounding up used for small heap\r\n\treturn pcm_round(n)\r\n! allocbytes[sizeindextable[n]]\r\nelse\t\t\t\t!devise some strategy probably doubling up.\r\n\tm:=2048\r\n\twhile n>m do\r\n\t\tm<<:=1\r\n\tod\r\n\treturn m\r\nfi\r\n\r\nend\r\n\r\nglobal proc pcm_printfreelist(int size,ref wordp p) =\t\t!PCM_PRINTFREELIST\r\nprintln \"Size: \",size\r\nwhile p do\r\n!\tprintf(\" %llX\",u64(p))\r\n\tprint \" \",,p:\"h\"\r\n\tp:=ref wordp(int(p^))\r\nod\r\nputs(\"\")\r\nend\r\n\r\nglobal proc pcm_diags(ref char caption) =\t\t!PCM_DIAGS\r\nvar int m\r\n\r\nprintln \"HEAP FREELISTS:\",caption\r\n\r\nm:=16\r\nfor i:=1 to 8 do\r\n\tpcm_printfreelist(m,freelist[i])\r\n\tm<<:=1\r\nod\r\nend\r\n\r\nglobal function pcm_allocz(int n)ref void =\t\t!PCM_ALLOCZ\r\nvar ref void p\r\np:=pcm_alloc(n)\r\n\r\nmemset(p,0,n)\r\nreturn p\r\nend\r\n\r\nglobal function pcm_copyheapstring(ref char s)ref char =\t\t!PCM_COPYHEAPSTRING\r\n!allocate enough bytes for string s: copy s to the heap\r\n!return pointer to new string\r\nvar ref char q\r\nvar int n\r\nif s=nil then return nil fi\r\n\r\nn:=strlen(s)+1\r\nq:=pcm_alloc(n)\r\nmemcpy(q,s,n)\r\nreturn q\r\nend\r\n\r\nproc addtomemalloc(ref int32 ptr,int size)=\r\n!add ptr to allocated table\r\n\r\n!CPL \"***************ADD TO ALLOC:\",ptr,size\r\n\r\nfor i to maxmemalloc do\r\n\tif memalloctable[i]=ptr then\r\n\t\tCPL \"ALLOC ERROR:\",ptr,\"ALREADY ALLOCATED\\n\\n\\n\"\r\nCPL\r\nCPL\r\n\t\tstop 2\r\n\tfi\r\n\r\n\tif memalloctable[i]=nil then\t\t!unused entry\r\n\t\tmemalloctable[i]:=ptr\r\n\t\tmemallocsize[i]:=size\r\n\t\treturn\r\n\tfi\r\nod\r\nCPL \"MEMALLOCTABLE FULL\\n\\n\\n\\n\"; os_getch()\r\nstop 3\r\nend\r\n\r\nproc removefrommemalloc(ref int32 ptr,int size)=\r\n!remove ptr to allocated table\r\n\r\n!CPL \"------------------************REMOVE FROM ALLOC:\",ptr,size\r\n\r\nfor i to maxmemalloc do\r\n\tif memalloctable[i]=ptr then\r\n\r\nif memallocsize[i]<>size then\r\n\tCPL \"REMOVE:FOUND\",ptr,\"IN MEMALLOCTABLE, FREESIZE=\",size,\", BUT STORED AS BLOCK SIZE:\",memallocsize[i]\r\n!PCERROR(\"MEMERROR\")\r\nCPL\r\nCPL\r\n\tabortprogram(\"MEMSIZE\")\r\nfi\r\n\r\n\t\tmemalloctable[i]:=nil\r\n\t\treturn\r\n\tfi\r\nod\r\nCPL \"CAN'T FIND\",ptr,\"IN MEMALLOCTABLE\",size\r\nCPL\r\nCPL\r\nabortprogram(\"MEM\")\r\nstop 4\r\nend\r\n\r\nglobal function allocmem(int n)ref void =\t\t!ALLOCMEM\r\nvar ref void p\r\n\r\np:=malloc(n)\r\nif (p) then\r\n\treturn p\r\nfi\r\nprintln n,memtotal\r\nabortprogram(\"Alloc mem failure\")\r\nreturn nil\r\nend\r\n\r\nglobal function reallocmem(ref void p,int n)ref void =\t\t!REALLOCMEM\r\np:=realloc(p,n)\r\nreturn p when p\r\nprintln n\r\nabortprogram(\"Realloc mem failure\")\r\nreturn nil\r\nend\r\n\r\nglobal proc abortprogram(ref char s) =\t\t!ABORTPROGRAM\r\nprintln s\r\nprint   \"ABORTING: Press key...\"\r\nos_getch()\r\nstop 5\r\nend\r\n\r\nglobal function getfilesize(filehandle handlex)int=\t\t!GETFILESIZE\r\n\tvar word32 p,size\r\n\r\n\tp:=ftell(handlex)\t\t!current position\r\n\tfseek(handlex,0,2)\t\t!get to eof\r\n\tsize:=ftell(handlex)\t\t!size in bytes\r\n\tfseek(handlex,p,seek_set)\t!restore position\r\n\treturn size\r\nend\r\n\r\nglobal proc readrandom(filehandle handlex, ref byte mem, int offset, size) =\t\t!READRANDOM\r\n\tvar int a\r\n\tfseek(handlex,offset,seek_set)\r\n\ta:=fread(mem,1,size,handlex)\t\t\t!assign so as to remove gcc warning\r\nend\r\n\r\nglobal function writerandom(filehandle handlex, ref byte mem, int offset,size)int =\t\t!WRITERANDOM\r\n\tfseek(handlex,offset,seek_set)\r\n\treturn fwrite(mem,1,size,handlex)\r\nend\r\n\r\nglobal function setfilepos(filehandle file,int offset)int=\r\n\treturn fseek(file,offset,0)\r\nend\r\n\r\nglobal function getfilepos(filehandle file)int=\r\n\treturn ftell(file)\r\nend\r\n\r\nglobal function readfile(ref char filename)ref byte =\t\t!READFILE\r\nvar filehandle f\r\nvar int size\r\nvar ref byte m,p\r\n\r\nf:=fopen(filename,\"rb\")\r\nif f=nil then\r\n\treturn nil\r\nfi\r\nrfsize:=size:=getfilesize(f)\r\n\r\nm:=malloc(size+4)\t\t!allow space for etx/zeof etc\r\n\r\nif m=nil then\r\n\treturn nil\r\nfi\r\n\r\nreadrandom(f,m,0,size)\r\np:=m+size\t\t\t!point to following byte\r\np^:=0\r\n(p+1)^:=26\r\n(p+2)^:=0\t\t\t!allow use as string\r\n\r\nfclose(f)\r\nreturn m\r\nend\r\n\r\nglobal function writefile(ref char filename,ref byte data,int size)int =\r\nvar filehandle f\r\nvar int n\r\n\r\nf:=fopen(filename,\"wb\")\r\nif f=nil then\r\n\treturn 0\r\nfi\r\n\r\nn:=writerandom(f,data,0,size)\r\nfclose(f)\r\nreturn n\r\nend\r\n\r\nglobal function checkfile(ref char file)int=\t\t!CHECKFILE\r\nvar filehandle f\r\nif f:=fopen(file,\"rb\") then\r\n\tfclose(f)\r\n\treturn 1\r\nfi\r\nreturn 0\r\nend\r\n\r\nglobal proc readlinen(filehandle handlex,ref char buffer,int size) =\t\t!READLINEN\r\n!size>2\r\nvar int ch\r\nvar ref char p\r\nvar int n\r\nvar [0:100]char buff\r\nvar byte crseen\r\n\r\nif handlex=nil then\r\n\thandlex:=filehandle(os_getstdin())\r\nfi\r\nif handlex=nil then\r\n\tn:=0\r\n\tp:=buffer\r\n\tdo\r\n\t\tch:=getchar()\r\n\t\tif ch=13 or ch=10 or ch=-1 then\r\n\t\t\tp^:=0\r\n\t\t\treturn\r\n\t\tfi\r\n\t\tp++^:=ch\r\n\t\t++n\r\n\t\tif n>=(size-2) then\r\n\t\t\tp^:=0\r\n\t\t\treturn\r\n\t\tfi\r\n\tod\r\nfi\r\n\r\nbuffer^:=0\r\nif fgets(buffer,size-2,handlex)=nil then\r\n\treturn\r\nfi\r\n\r\nn:=strlen(buffer)\r\nif n=0 then\r\n\treturn\r\nfi\r\n\r\np:=buffer+n-1\t\t!point to last char\r\ncrseen:=0\r\nwhile (p>=buffer and (p^=13 or p^=10)) do\r\n\tif p^=13 or p^=10 then crseen:=1 fi\r\n\tp--^ :=0\r\nod\r\n\r\n!NOTE: this check doesn't work when a line simply doesn't end with cr-lf\r\n\r\nif not crseen and (n+4>size) then\r\n\tcpl size,n\r\n\tabortprogram(\"line too long\")\r\nfi\r\nend\r\n\r\nglobal proc iconvlcn(ref char s,int n) =\t\t!ICONVLCN\r\nto n do\r\n\ts^:=tolower(s^)\r\n\t++s\r\nod\r\nend\r\n\r\nglobal proc iconvucn(ref char s,int n) =\t\t!ICONVUCN\r\nto n do\r\n\ts^:=toupper(s^)\r\n\t++s\r\nod\r\nend\r\n\r\nglobal proc convlcstring(ref char s)=\t\t!CONVLCSTRING\r\nwhile (s^) do\r\n\ts^:=tolower(s^)\r\n\t++s\r\nod\r\nend\r\n\r\nglobal proc convucstring(ref char s)=\t\t!CONVUCSTRING\r\nwhile (s^) do\r\n\ts^:=toupper(s^)\r\n\t++s\r\nod\r\nend\r\n\r\nglobal function changeext(ref char s,newext)ichar=\t\t!CHANGEEXT\r\n!whether filespec has an extension or not, change it to newext\r\n!newext should start with \".\"\r\n!return new string (locally stored static string, so must be used before calling again)\r\nstatic var [260]char newfile\r\nvar [32]char newext2\r\nvar ref char sext\r\nvar int n\r\n\r\nstrcpy(&newfile[1],s)\r\n\r\ncase newext^\r\nwhen 0 then\r\n\tnewext2[1]:=0\r\n\tnewext2[2]:=0\r\nwhen '.' then\r\n\tstrcpy(&newext2[1],newext)\r\nelse\r\n\tstrcpy(&newext2[1],\".\")\r\n\tstrcat(&newext2[1],newext)\r\nesac\r\n\r\n\r\nsext:=extractext(s,1)\t\t\t!include \".\" when it is only extension\r\n\r\ncase sext^\r\nwhen 0 then\t\t\t\t\t\t!no extension not even \".\"\r\n\tstrcat(&newfile[1],&newext2[1])\r\nwhen '.' then\t\t\t\t\t\t!no extension not even \".\"\r\n\tstrcat(&newfile[1],&newext2[2])\r\nelse\t\t\t\t\t\t\t!has extension\r\n\tn:=sext-s-2\t\t\t!n is number of chars before the \".\"\r\n\tstrcpy(&newfile[1]+n+1,&newext2[1])\r\nesac\r\n\r\nreturn &newfile[1]\r\nend\r\n\r\nglobal function extractext(ref char s,int period=0)ichar=\t\t!EXTRACTEXT\r\n!if filespec s has an extension, then return pointer to it otherwise return \"\"\r\n!if s ends with \".\", then returns \".\"\r\nvar ref char t,u\r\n\r\nt:=extractfile(s)\r\n\r\nif t^=0 then\t\t\t!s contains no filename\r\n\treturn \"\"\r\nfi\r\n\r\n!t contains filename+ext\r\nu:=t+strlen(t)-1\t\t!u points to last char of t\r\n\r\nwhile u>=t do\r\n\tif u^='.' then\t\t!start extension found\r\n\t\tif (u+1)^=0 then\t\t!null extension\r\n\t\t\treturn (period|\".\"|\"\")\r\n\t\tfi\r\n\t\treturn u+1\t\t\t!return last part of filename as extension exclude the dot\r\n\tfi\r\n\t--u\r\nod\r\nreturn \"\"\t\t\t!no extension seen\r\nend\r\n\r\nglobal function extractpath(ref char s)ichar=\t\t!EXTRACTPATH\r\nstatic var [0:260]char str\r\nvar ref char t\r\nvar int n\r\n\r\nt:=s+strlen(s)-1\t\t!t points to last char\r\n\r\nwhile (t>=s) do\r\n\tswitch t^\r\n\twhen '\\\\','/',':' then\t\t!path separator or drive letter terminator assume no extension\r\n\t\tn:=t-s+1\t\t\t!n is number of chars in path, which includes rightmost / or \\ or :\r\n\t\tmemcpy(&.str,s,n)\r\n\t\tstr[n]:=0\r\n\t\treturn &.str\r\n\tendswitch\r\n\t--t\r\nod\r\nreturn \"\"\t\t\t!no path found\r\nend\r\n\r\nglobal function extractfile(ref char s)ichar=\t\t!EXTRACTFILE\r\nvar ref char t\r\n\r\nt:=extractpath(s)\r\n\r\nif t^=0 then\t\t\t!s contains no path\r\n\treturn s\r\nfi\r\n\r\nreturn s+strlen(t)\t\t!point to last part of s that contains the file\r\nend\r\n\r\nglobal function extractbasefile(ref char s)ichar=\t\t!EXTRACTBASEFILE\r\nstatic var [0:100]char str\r\nvar ref char f,e\r\nvar int n,flen\r\n\r\nf:=extractfile(s)\r\nflen:=strlen(f)\r\nif flen=0 then\t\t!s contains no path\r\n\treturn \"\"\r\nfi\r\ne:=extractext(f,0)\r\n\r\nif e^ then\t\t\t!not null extension\r\n\tn:=flen-strlen(e)-1\r\n\tmemcpy(&str,f,n)\r\n\tstr[n]:=0\r\n\treturn &.str\r\nfi\r\nif (f+flen-1)^='.' then\r\n\tmemcpy(&str,f,flen-1)\r\n\tstr[flen-1]:=0\r\n\treturn &.str\r\nfi\r\nreturn f\r\nend\r\n\r\nglobal function addext(ref char s,ref char newext)ichar=\t\t!ADDEXT\r\n!when filespec has no extension of its own, add newext\r\nvar ref char sext\r\n\r\nsext:=extractext(s,1)\r\n\r\nif sext^=0 then\t\t\t\t\t\t!no extension not even \".\"\r\n\treturn changeext(s,newext)\r\nfi\r\n\r\nreturn s\t\t\t\t\t\t\t!has own extension; use that\r\nend\r\n\r\nglobal function alloctable(int n, size)ref void =\t\t!ALLOCTABLE\r\n!Allocate table space for n elements, each of size <size>\r\n!Allows for 1-based indexing, so allocates (n+1) elements\r\nvar ref void p\r\n\r\np:=malloc((n+1)*size)\r\n\r\nif not p then\r\n\tabortprogram(\"Alloctable failure\")\r\nfi\r\nreturn p\r\nend\r\n\r\nglobal function zalloctable(int n, size)ref void =\t\t!ALLOCTABLE\r\n!Allocate table space for n elements, each of size <size>\r\n!Allows for 1-based indexing, so allocates (n+1) elements\r\nvar ref int p\r\n\r\np:=alloctable(n,size)\r\n\r\npcm_clearmem(p,(n+1)*size)\r\nreturn p\r\nend\r\n\r\nglobal proc checkfreelists(ichar s)=\r\nvar ref wordp p,q\r\nvar int64 aa\r\n\r\nfor i:=2 to 2 do\r\n\tp:=freelist[i]\r\n\r\n\twhile p do\r\n\t\taa:=int64(p)\r\n\t\tif aa>0xffff'FFFF or aa<100 then\r\n\t\t\tCPL s,\"FREE LIST ERROR\",i,p,q\r\n!\t\t\tos_getch(); stop 1\r\n\t\tfi\r\n\t\tq:=p\r\n\t\tp:=ref wordp(int(p^))\r\n\tod\r\n\r\nod\r\nend\r\n\r\nglobal function pcm_alloc32:ref void =\t\t!PCM_ALLOC\r\nvar ref byte p\r\n\r\nallocbytes:=32\r\n!smallmemtotal+:=32\r\n\r\n!if p:=ref byte(freelist[2]) then\t\t!Items of this block size available\r\n!\tfreelist[2]:=ref wordp((freelist[2])^)\r\n!\tif mem_check then addtomemalloc(ref int32(p),32) fi\r\n!\treturn p\r\n!fi\r\n\r\n!No items in freelists: allocate new space in this heap block\r\n\r\nreturn pcm_alloc(32)\r\nend\r\n\r\nglobal proc pcm_free32(ref void p) =\r\n!n can be the actual size requested it does not need to be the allocated size\r\n\r\n!CPL \"PCMFREE32\"\r\nsmallmemtotal-:=32\r\nif mem_check then removefrommemalloc(p,32) fi\r\n!(ref wordp(p))^:=wordp(freelist[2])\r\n(ref wordp(p))^:=wordp(int(freelist[2]))\r\nfreelist[2]:=p\r\nend\r\n\r\nglobal proc outbyte(filehandle f,int x)=\r\nfwrite(&x,1,1,f)\r\nend\r\n\r\nglobal proc outword16(filehandle f,word x)=\r\nfwrite(&x,2,1,f)\r\nend\r\n\r\nglobal proc outword(filehandle f,word x)=\r\nfwrite(&x,4,1,f)\r\nend\r\n\r\nglobal proc outword64(filehandle f,word64 x)=\r\nfwrite(&x,8,1,f)\r\nend\r\n\r\nglobal function myeof(filehandle f)int=\r\nvar int c\r\n\r\nc:=fgetc(f)\r\nif c=c_eof then return 1 fi\r\nungetc(c,f)\r\nreturn 0;\r\nend\r\n\r\nglobal function pcm_smallallocz(int n)ref void =\r\nvar ref byte p\r\n\r\nif (alloccode:=sizeindextable[n])=0 then\r\n\talloccode:=1\r\nfi\r\nallocbytes:=allocupper[alloccode]\r\n\r\n!No items in freelists: allocate new space in this heap block\r\np:=pcheapptr\t\t\t\t!Create item at start of remaining pool in heap block\r\npcheapptr+:=allocbytes\t\t\t!Shrink remaining pool\r\n\r\nif pcheapptr>=pcheapend then\t\t!Overflows?\r\n\tp:=pcm_newblock(allocbytes)\t\t!Create new heap block, and allocate from start of that\r\n\tmemset(p,0,n)\r\n\treturn p\r\nfi\r\n\r\nmemset(p,0,n)\r\n\r\nreturn p\r\nend\r\n\r\n!global function pcm_fastalloc(int n)ref void =\r\nglobal function pcm_smallalloc(int n)ref void =\r\nvar ref byte p\r\n\r\nif (alloccode:=sizeindextable[n])=0 then\r\n\talloccode:=1\r\nfi\r\nallocbytes:=allocupper[alloccode]\r\n\r\n!No items in freelists: allocate new space in this heap block\r\np:=pcheapptr\t\t\t\t!Create item at start of remaining pool in heap block\r\npcheapptr+:=allocbytes\t\t\t!Shrink remaining pool\r\n\r\nif pcheapptr>=pcheapend then\t\t!Overflows?\r\n\tp:=pcm_newblock(allocbytes)\t\t!Create new heap block, and allocate from start of that\r\n\treturn p\r\nfi\r\n\r\nreturn p\r\nend\r\n\r\nglobal proc strbuffer_add(ref strbuffer dest, ichar s, int n=-1)=\r\nvar int newlen,oldlen\r\nvar ichar newptr\r\n\r\nif n=-1 then\r\n\tn:=strlen(s)\r\nfi\r\n\r\noldlen:=dest^.length\r\n\r\nif oldlen=0 then\t\t\t\t!first string\r\n\tdest^.strptr:=pcm_alloc(n+1)\r\n\tdest^.allocated:=allocbytes\r\n\tdest^.length:=n\t\t\t\t!length always excludes terminator\r\n\tmemcpy(dest^.strptr,s,n)\r\n\t(dest^.strptr+n)^:=0\r\n\treturn\r\nfi\r\n\r\nnewlen:=oldlen+n\r\nif newlen+1>dest^.allocated then\r\n\tnewptr:=pcm_alloc(newlen+1)\r\n\tmemcpy(newptr,dest^.strptr,oldlen)\r\n\tdest^.strptr:=newptr\r\n\tdest^.allocated:=allocbytes\r\nfi\r\n\r\nmemcpy(dest^.strptr+oldlen,s,n)\r\n(dest^.strptr+newlen)^:=0\r\ndest^.length:=newlen\r\nend\r\n\r\nglobal proc gs_init(ref strbuffer dest)=\t\t\t!INITGENSTR\r\npcm_clearmem(dest,strbuffer.bytes)\r\nend\r\n\r\nglobal proc gs_free(ref strbuffer dest)=\r\nif dest^.allocated then\r\n\tpcm_free(dest^.strptr,dest^.allocated)\r\nfi\r\nend\r\n\r\nglobal proc gs_str(ref strbuffer dest,ichar s)=\t\t\t!GENSTR\r\nstrbuffer_add(dest,s)\r\nend\r\n\r\nglobal proc gs_char(ref strbuffer dest,int c)=\r\nvar [16]char s\r\n\r\ns[1]:=c\r\ns[2]:=0\r\n\r\nstrbuffer_add(dest,&.s,1)\r\nend\r\n\r\nglobal proc gs_strn(ref strbuffer dest,ichar s,int length)=\r\nstrbuffer_add(dest,s,length)\r\nend\r\n\r\nglobal proc gs_strvar(ref strbuffer dest,s)=\t\t\t!GENSTR\r\nstrbuffer_add(dest,s^.strptr)\r\nend\r\n\r\nglobal proc gs_strint(ref strbuffer dest,int64 a)=\r\nstrbuffer_add(dest,strint(a))\r\nend\r\n\r\nglobal proc gs_strln(ref strbuffer dest,ichar s)=\t\t!GENSTRLN\r\ngs_str(dest,s)\r\ngs_line(dest)\r\nend\r\n\r\nglobal proc gs_strsp(ref strbuffer dest,ichar s)=\r\ngs_str(dest,s)\r\ngs_str(dest,\" \")\r\nend\r\n\r\nglobal proc gs_line(ref strbuffer dest)=\r\nstrbuffer_add(dest,\"\\w\")\r\nend\r\n\r\nglobal function gs_getcol(ref strbuffer dest)int=\r\nreturn dest^.length\r\nend\r\n\r\nglobal proc gs_leftstr(ref strbuffer dest, ichar s, int w, padch=' ')=\r\nvar int col,i,n,slen\r\nvar [2560]char str\r\ncol:=dest^.length\r\nstrcpy(&.str,s)\r\nslen:=strlen(s)\r\nn:=w-slen\r\n!CPL =slen,=w,=n\r\nif n>0 then\r\n\tfor i:=1 to n do\r\n\t\tstr[slen+i]:=padch\r\n\tod\r\n\tstr[slen+n+1]:=0\r\nfi\r\ngs_str(dest,&.str)\r\nend\r\n\r\nglobal proc gs_leftint(ref strbuffer dest, int a, int w, padch=' ')=\r\ngs_leftstr(dest,strint(a),w,padch)\r\nend\r\n\r\nglobal proc gs_padto(ref strbuffer dest,int col, ch=' ')=\r\nvar int n\r\nvar [2560]char str\r\n\r\nn:=col-dest^.length\r\nif n<=0 then return fi\r\nfor i:=1 to n do\r\n\tstr[i]:=ch\r\nod\r\nstr[n+1]:=0\r\ngs_str(dest,&.str)\r\nend\r\n\r\nglobal proc gs_println(ref strbuffer dest,filehandle f)=\r\n(dest.strptr+dest.length)^:=0\r\n\r\nif f=nil then\r\n\tprintln dest.strptr,,\"\\c\"\r\nelse\r\n\tprintln @f,dest.strptr,,\"\\c\"\r\nfi\r\nend\r\n\r\nglobal function nextcmdparam(int &paramno, ichar &name, &value, ichar defext=nil)int=\r\nstatic var int infile=0\r\nstatic var ichar filestart=nil\r\nstatic var ichar fileptr=nil\r\nstatic var byte colonseen=0\r\nvar ref char q\r\nvar ichar item,fileext\r\nvar ichar rest\r\nvar int length\r\nstatic var [300]char str\r\n\r\nreenter::\r\nvalue:=nil\r\nname:=nil\r\n\r\nif infile then\r\n\tif readnextfileitem(fileptr,item)=0 then\t\t!eof\r\n\t\tfree(filestart)\t\t\t\t\t\t\t\t!file allocated via malloc\r\n\t\tinfile:=0\r\n\t\tgoto reenter\r\n\tfi\r\nelse\r\n\tif paramno>nsysparams then\r\n\t\treturn pm_end\r\n\tfi\r\n\titem:=sysparams[paramno]\r\n\t++paramno\r\n\r\n\tlength:=strlen(item)\r\n\r\n\tif item^='@' then\t\t!@ file\r\n\t\tfilestart:=fileptr:=cast(readfile(item+1))\r\n\t\tif filestart=nil then\r\n\t\t\tprintln \"Can't open\",item\r\n\t\t\tstop 7\r\n\t\tfi\r\n\t\tinfile:=1\r\n\t\tgoto reenter\r\n\tfi\r\n\r\n\tif item^=':' then\r\n\t\tcolonseen:=1\r\n\t\treturn pm_colon\r\n\tfi\r\nfi\r\n\r\nvalue:=nil\r\nif item^='-' then\r\n\tname:=item+(colonseen|0|1)\r\n\tq:=strchr(item,':')\r\n\tif not q then\r\n\t\tq:=strchr(item,'=')\r\n\tfi\r\n\tif q then\r\n\t\tvalue:=q+1\r\n\t\tq^:=0\r\n\tfi\r\n\treturn (colonseen|pm_extra|pm_option)\r\nfi\r\n\r\nfileext:=extractext(item,0)\r\nname:=item\r\n\r\nif fileext^=0 then\t\t\t\t\t\t\t!no extension\r\n\tstrcpy(&.str,name)\r\n\tif defext and not colonseen then\r\n\t\tname:=addext(&.str,defext)\t\t\t\t!try .c\r\n\tfi\r\nelsif eqstring(fileext,\"dll\") then\r\n\treturn (colonseen|pm_extra|pm_libfile)\r\nfi\r\nreturn (colonseen|pm_extra|pm_sourcefile)\r\nend\r\n\r\nfunction readnextfileitem(ichar &fileptr,&item)int=\r\nvar ref char p,pstart,pend\r\nvar int n\r\nstatic var [256]char str\r\n\r\np:=fileptr\r\n\r\nreenter::\r\ndo\r\n\tcase p^\r\n\twhen ' ','\\t',13,10 then\t!skip white space\r\n\t\t++p\r\n\twhen 26,0 then\t\t\t\t!eof\r\n\t\treturn 0\r\n\telse\r\n\t\texit\r\n\tesac\r\nod\r\n\r\ncase p^\r\nwhen '!', '#' then\t\t\t!comment\r\n\t++p\r\n\tdocase p++^\r\n\twhen 10 then\r\n\t\tgoto reenter\r\n\twhen 26,0 then\r\n\t\tfileptr:=p-1\r\n\t\treturn 0\r\n\telse\r\n\r\n\tenddocase\r\nesac\r\n\r\n\r\ncase p^\r\nwhen '\"' then\t\t\t\t!read until closing \"\r\n\tpstart:=++p\r\n\tdo\r\n\t\tcase p^\r\n\t\twhen 0,26 then\r\n\t\t\tprintln \"Unexpected EOF in @file\"\r\n\t\t\tstop 8\r\n\t\twhen '\"' then\r\n\t\t\tpend:=p++\r\n\t\t\tif p^=',' then ++p fi\r\n\t\t\texit\r\n\t\tesac\r\n\t\t++p\r\n\tod\r\nelse\r\n\tpstart:=p\r\n\tdo\r\n\t\tcase p^\r\n\t\twhen 0,26 then\r\n\t\t\tpend:=p\r\n\t\t\texit\r\n\t\twhen ' ','\\t',',',13,10 then\r\n\t\t\tpend:=p++\r\n\t\t\texit\r\n\t\tesac\r\n\t\t++p\r\n\tod\r\nesac\r\n\r\nn:=pend-pstart\r\nif n>=str.len then\r\n\tprintln \"@file item too long\"\r\n\tstop 9\r\nfi\r\nmemcpy(&.str,pstart,n)\r\nstr[n+1]:=0\r\nitem:=&.str\r\nfileptr:=p\r\n\r\nreturn 1\r\nend\r\n\r\nglobal proc ipadstr(ref char s,int width,ref char padchar=\" \")=\r\nvar int n\r\n\r\nn:=strlen(s)\r\nto width-n do\r\n\tstrcat(s,padchar)\r\nod\r\nend\r\n\r\nglobal function padstr(ref char s,int width,ref char padchar=\" \")ichar=\r\nstatic var [256]char str\r\n\r\nstrcpy(&.str,s)\r\nipadstr(&.str,width,padchar)\r\nreturn &.str\r\nend\r\n\r\nglobal function chr(int c)ichar=\r\nstatic var[8]char str\r\n\r\nstr[1]:=c\r\nstr[2]:=0\r\nreturn &.str\r\nend\r\n\r\nglobal function cmpstring(ichar s,t)int=\r\n\tvar int res\r\n\tif (res:=strcmp(s,t))<0 then\r\n\t\treturn -1\r\n\telsif res>0 then\r\n\t\treturn 1\r\n\telse\r\n\t\treturn 0\r\n\tfi\r\nend\r\n\r\nglobal function cmpstringn(ichar s,t,int n)int=\r\n\tvar int res\r\n\tif (res:=strncmp(s,t,n))<0 then\r\n\t\treturn -1\r\n\telsif res>0 then\r\n\t\treturn 1\r\n\telse\r\n\t\treturn 0\r\n\tfi\r\nend\r\n\r\nglobal function eqstring(ichar s,t)int=\r\n\treturn strcmp(s,t)=0\r\nend\r\n\r\nglobal function cmpbytes(ref void p,q,int n)int=\r\n\tvar int res\r\n\tif (res:=memcmp(p,q,n))<0 then\r\n\t\treturn -1\r\n\telsif res>0 then\r\n\t\treturn 1\r\n\telse\r\n\t\treturn 0\r\n\tfi\r\nend\r\n\r\nglobal function eqbytes(ref void p,q,int n)int=\r\n\treturn memcmp(p,q,n)=0\r\nend\r\n\r\nglobal proc mseed(word64 a,b=0)=\r\nseed[1]:=a\r\nif b then\r\n\tseed[2]:=b\r\nelse\r\n\tseed[2] ixor:=a\r\nfi\r\nend\r\n\r\nglobal function mrandom:int =\r\n!return pure 64-bit int value, positive or negative -2**63 to +2**63-1\r\n!(cast result for unsigned value)\r\n\tvar word64 x,y\r\n\tx:=seed[1]\r\n\ty:=seed[2]\r\n\tseed[1]:=y\r\n\tx ixor:=(x<<23)\r\n\tseed[2]:= x ixor y ixor (x>>17) ixor (y>>26)\r\n\treturn seed[2]+y\r\nend\r\n\r\nglobal function mrandomp:int =\r\n!pure 64-bit int value, positive only, 0 to 2**63-1\r\n\treturn mrandom() iand 0x7FFF'FFFF'FFFF'FFFF\r\nend\r\n\r\nglobal function mrandomint(int n)int=\r\n!positive random int value from 0 to n-1\r\n\treturn mrandomp() rem n\r\nend\r\n\r\nglobal function mrandomrange(int a,b)int=\r\n!random int value from a to b inclusive\r\n!span extent must be 1 to 2**63-1\r\n\tvar int span\r\n\tspan:=b-a+1\r\n\tif span<=0 then\r\n\t\treturn 0\r\n\tfi\r\n\treturn (mrandomp() rem span)+a\r\nend\r\n\r\nglobal function mrandomreal:real =\r\n!positive random real value from 0 to 0.999999999999999999891579782751449556599254719913005828857421875\r\n!upper limit is (2**63-1)/(2**63)\r\n\treturn real(mrandomp())/9223372036854775808.0\r\nend\r\n\r\nglobal function checkpackfile:ref byte=\r\n!find out if this executable contains extra packed files\r\n!return 1 or 0\r\n\r\nvar int a,offset,i,size\r\nvar [100]char name\r\nvar [300]char exefile\r\nvar ref byte packexeptr\t\t\t!for embedded pack files, contains pointer to in-memory version of this .exe file plus extras; else nil\r\nvar int packexesize\t\t\t\t!byte size\r\nvar ref char packfilename\r\nvar int packfilesize\r\nvar ref byte packfileptr\r\n\r\nmacro getfileint(data,offset)=(ref int32(data+offset))^\r\n\r\nstrcpy(&exefile[1],os_gethostname())\r\nprintln \"Attempting to open\",&exefile\r\npackexeptr:=readfile(&exefile[1])\r\n\r\nif not packexeptr then\r\n\tcpl \"Can't open\",&exefile,&packexeptr\r\n\tstop\r\nfi\r\n\r\npackexesize:=rfsize\r\ncpl \"File read OK. Size\",packexesize\r\n!STOP\r\n\r\na:=getfileint(packexeptr,packexesize-int32.bytes)\r\nif a<>'PCAK' then\r\n\tfree(packexeptr)\r\n\tpackfileptr:=nil\r\n\treturn nil\r\nfi\r\n\r\noffset:=getfileint(packexeptr,packexesize-int32.bytes*2)\r\n\r\npackfilename:=cast(packexeptr+offset)\r\noffset+:=strlen(packfilename)+1\r\npackfilesize:=getfileint(packexeptr,offset)\r\npackfileptr:=packexeptr+offset+int32.bytes\r\n\r\nreturn packfileptr\r\nend\r\n",\
(byte*)"global type filehandle=ref void\r\n!global type cstring=ref i8\r\ntype cstring=ref i8\r\n\r\nimportlib cstd=\r\n!\tclang function malloc\t(wordm)ref void\r\n\tclang function malloc\t(word64)ref void\r\n\tclang function realloc\t(ref void, wordm)ref void\r\n\tclang proc     free\t\t(ref void)\r\n\tclang proc     memset\t(ref void, int32, wordm)\r\n\tclang proc     memcpy\t(ref void, ref void, wordm)\r\n\tclang function clock\t:int32\r\n\tclang function ftell\t(filehandle)int32\r\n\tclang function fseek\t(filehandle, int32, int32)int32\r\n\tclang function fread\t(ref void, wordm, wordm, filehandle)wordm\r\n\tclang function fwrite\t(ref void, wordm, wordm, filehandle)wordm\r\n\tclang function getc\t\t(filehandle)int32\r\n\tclang function ungetc\t(int32, filehandle)int32\r\n\tclang function fopen\t(cstring,cstring=\"rb\")filehandle\r\n\tclang function fclose\t(filehandle)int32\r\n\tclang function fgets\t(cstring, int, filehandle)cstring\r\n\tclang function remove\t(cstring)int32\r\n\tclang function rename\t(cstring, cstring)int32\r\n\tclang function getchar\t:int32\r\n\tclang proc     putchar\t(int32)\r\n\tclang proc     setbuf\t(filehandle, ref byte)\r\n\r\n\tclang function strlen\t(cstring)wordm\r\n\tclang function strcpy\t(cstring, cstring)cstring\r\n\tclang function strcmp\t(cstring, cstring)int32\r\n\tclang function strncmp\t(cstring, cstring, wordm)int32\r\n\tclang function strncpy\t(cstring, cstring, wordm)wordm\r\n\tclang function memcmp\t(ref void, ref void, wordm)int32\r\n\tclang function strcat\t(cstring, cstring)cstring\r\n\tclang function tolower\t(int32)int32\r\n\tclang function toupper\t(int32)int32\r\n\tclang function isalpha\t(int32)int32\r\n\tclang function isupper\t(int32)int32\r\n\tclang function islower\t(int32)int32\r\n\tclang function isalnum\t(int32)int32\r\n\tclang function isspace\t(int32)int32\r\n\tclang function strstr\t(cstring, cstring)cstring\r\n\tclang function atol\t\t(cstring)intm\r\n\tclang function atoi\t\t(cstring)int32\r\n!\tclang function strtod\t(cstring,ref cstring)real64\r\n\tclang function strtod\t(cstring,ref void)real64\r\n\r\n\tclang function puts\t\t(cstring)int32\r\n\tclang function printf\t(cstring, ...)int32\r\n\r\n\tclang function sprintf\t(cstring, cstring, ...)int32\r\n!\tclang function __mingw_sprintf\t(cstring, ...)int32\r\n\r\n\tclang function sscanf\t(cstring, cstring, ...)int32\r\n\tclang function scanf\t(cstring, ...)int32\r\n\r\n\tclang function rand\t\t:int32\r\n\tclang proc     srand\t(word32)\r\n\tclang function system\t(cstring)int32\r\n\r\n\tclang function fgetc\t(filehandle)int32\r\n\tclang function fputc\t(int32,  filehandle)int32\r\n\tclang function fprintf\t(filehandle, cstring, ...)int32\r\n\tclang function fputs\t(cstring,  filehandle)int32\r\n\tclang function feof\t\t(filehandle)int32\r\n\tclang function getch\t:int32\r\n\tclang function kbhit\t:int32\r\n\tclang function _mkdir\t(cstring)int32\r\n\tclang function mkdir\t(cstring)int32\r\n\tclang function dummy\t(real)real\r\n\tclang function strchr\t(cstring,int32)cstring\r\n\r\n\tclang proc     _exit\t(int32)\r\n\tclang proc     \"exit\"\t(int32)\r\n\tclang function\tpow\t\t(real,real)real\r\n\tclang function\tfreddy\t(int,real,real)real\r\n\r\nend\r\n\r\nglobal const c_eof\t\t=-1\r\nglobal const seek_set\t= 0\r\nglobal const seek_curr\t= 1\r\nglobal const seek_end\t= 2\r\n\r\n",\
(byte*)"import clib\r\nimport mlib\r\n\r\nimportlib cstd=\r\n\tclang proc     sleep\t(word32)\r\nend\r\n\r\nrecord termios =\r\n\tvar int32 c_iflag\r\n\tvar int32 c_oflag\r\n\tvar int32 c_cflag\r\n\tvar int32 c_lflag\r\n\tvar char c_line\r\n\tvar [32]char c_cc\t\t\t\t!at offset 17\r\n\tvar [3]byte filler\r\n\tvar int32 c_ispeed\t\t\t\t!at offset 52\r\n\tvar int32 c_ospeed\r\nend\r\n\r\nimportlib dlstuff=\r\n\tclang function dlopen\t\t(ichar, int32)ref void\r\n\tclang function dlsym\t\t(ref void, ichar)ref void\r\n\tclang function tcgetattr\t(int32, ref termios) int32\r\n\tclang function tcsetattr\t(int32, int32, ref termios) int32\r\nend\r\n\r\n!this record is used by some apps, so these fields must be present\r\nglobal record rsystemtime =\r\n\tvar int32 year\r\n\tvar int32 month\r\n\tvar int32 dayofweek\r\n\tvar int32 day\r\n\tvar int32 hour\r\n\tvar int32 minute\r\n\tvar int32 second\r\n\tvar int milliseconds\r\nend\r\n\r\nvar int init_flag=0\r\n\r\n\r\nglobal proc os_init=\r\ninit_flag:=1\r\nend\r\n\r\nglobal function os_execwait(ichar cmdline,int newconsole=0,ichar workdir=nil)int =\r\nreturn system(cmdline)\r\nend\r\n\r\nglobal function os_execcmd(ichar cmdline, int newconsole)int =\r\nreturn system(cmdline)\r\nend\r\n\r\nglobal function os_getch:int=\r\nconst ICANON  = 8x000002\r\nconst ECHO    = 8x000010\r\nconst TCSANOW = 0\r\n\r\nvar termios old,new\r\nvar char ch\r\n\r\ntcgetattr(0,&old)\r\nnew:=old\r\nnew.c_lflag iand:=inot ICANON\r\nnew.c_lflag iand:=inot ECHO\r\ntcsetattr(0,TCSANOW,&new)\r\n\r\nch:=getchar()\r\n\r\ntcsetattr(0,TCSANOW,&old)\r\n\r\nreturn ch\r\nend\r\n\r\nglobal function os_kbhit:int=\r\nabortprogram(\"kbhit\")\r\nreturn 0\r\nend\r\n\r\nglobal proc os_flushkeys=\r\nabortprogram(\"flushkeys\")\r\nend\r\n\r\nglobal function os_getconsolein:ref void=\r\nreturn nil\r\nend\r\n\r\nglobal function os_getconsoleout:ref void=\r\nreturn nil\r\nend\r\n\r\nglobal function os_proginstance:ref void=\r\nabortprogram(\"PROGINST\")\r\nreturn nil\r\nend\r\n\r\nglobal function os_getdllinst(ichar name)u64=\r\nconst RTLD_LAZY=1\r\nvar ref void h\r\n\r\nh:=dlopen(name,RTLD_LAZY)\r\n\r\nif h=nil then\r\n\tif strcmp(name,\"msvcrt\")=0 then\t\t\t!might be linux\r\n\t\th:=dlopen(\"libc.so.6\",RTLD_LAZY);\r\n\tfi\r\nfi\r\n\r\nreturn cast(h)\r\nend\r\n\r\nglobal function os_getdllprocaddr(intm hlib,ichar name)ref void=\r\nvar ref void fnaddr\r\n\r\nif hlib=0 then\r\n\treturn nil\r\nfi\r\n\r\nfnaddr:=dlsym(cast(int(hlib)), name)\r\nreturn fnaddr\r\nend\r\n\r\nglobal proc os_initwindows=\r\nend\r\n\r\nglobal function os_getchx:int=\r\nabortprogram(\"getchx\")\r\nreturn 0\r\nend\r\n\r\nglobal function os_getos=>ichar=\r\nif $targetbits=32 then\r\n\treturn \"L32\"\r\nelse\r\n\treturn \"L64\"\r\nfi\r\nend\r\n\r\nglobal function os_gethostsize=>int=\r\nreturn $targetbits\r\nend\r\n\r\nglobal function os_iswindows:int=\r\nreturn 0\r\nend\r\n\r\nglobal function os_shellexec(ichar opc, file)int=\r\nabortprogram(\"SHELL EXEC\")\r\nreturn 0\r\nend\r\n\r\nglobal proc  os_sleep(int a)=\r\nsleep(a)\r\nend\r\n\r\nglobal function os_getstdin:filehandle =\r\nreturn nil\r\n!return fopen(\"con\",\"rb\")\r\nend\r\n\r\nglobal function os_getstdout:filehandle =\r\nreturn nil\r\n!return fopen(\"con\",\"wb\")\r\nend\r\n\r\nglobal function os_gethostname:ichar=\r\n!abortprogram(\"gethostname\")\r\nreturn \"\"\r\nend\r\n\r\nglobal function os_getmpath:ichar=\r\n!abortprogram(\"getmpath\")\r\nreturn \"\"\r\nend\r\n\r\nglobal proc os_exitprocess(int x)=\r\nstop\r\n!_exit(0)\r\n!ExitProcess(x)\r\nend\r\n\r\n!global function os_gettimestamp:dint=\r\n!return clock()\r\n!end\r\n!\r\n!global function os_gettickcount:dint=\r\n!return clock()\r\n!end\r\n\r\nglobal function os_clock:int64=\r\nif os_iswindows() then\r\n\treturn clock()\r\nelse\r\n\treturn clock()/1000\r\nfi\r\nend\r\n\r\nglobal function os_getclockspersec:int64=\r\nreturn (os_iswindows()|1000|1000'000)\r\nend\r\n\r\nglobal proc os_setmesshandler(ref void addr)=\r\nabortprogram(\"SETMESSHANDLER\")\r\n!wndproc_callbackfn:=addr\r\nend\r\n\r\nglobal function os_hpcounter:int64=\r\nreturn 1\r\nend\r\n\r\nglobal function os_hpfrequency:int64=\r\nreturn 1\r\nend\r\n\r\nglobal function os_filelastwritetime(ichar filename)int64=\r\nreturn 0\r\nend\r\n\r\nglobal proc os_getsystime(ref rsystemtime tm)=\r\n!global proc os_getsystime(ref void tm)=\r\nmemset(tm,0,rsystemtime.bytes)\r\ntm.month:=1\t\t\t!avoid crashing the M compiler\r\nend\r\n\r\nglobal proc os_peek=\r\nend\r\n",\
(byte*)"import clib\r\nimport mlib\r\n\r\ntype dll0_intm=ref clang function:intm\r\ntype dll1_intm=ref clang function(intm)intm\r\ntype dll2_intm=ref clang function(intm,intm)intm\r\ntype dll3_intm=ref clang function(intm,intm,intm)intm\r\ntype dll4_intm=ref clang function(intm,intm,intm,intm)intm\r\ntype dll5_intm=ref clang function(intm,intm,intm,intm,intm)intm\r\ntype dll6_intm=ref clang function(intm,intm,intm,intm,intm,intm)intm\r\ntype dll9_intm=ref clang function(intm,intm,intm,intm, intm,intm,intm,intm, intm)intm\r\ntype dll10_intm=ref clang function(intm,intm,intm,intm, intm,intm,intm,intm, intm,intm)intm\r\ntype dll11_intm=ref clang function(intm,intm,intm,intm, intm,intm,intm,intm, intm,intm,intm)intm\r\ntype dll12_intm=ref clang function(intm,intm,intm,intm, intm,intm,intm,intm, intm,intm,intm,intm)intm\r\ntype dll14_intm=ref clang function(intm,intm,intm,intm, intm,intm,intm,intm, intm,intm,intm,intm, intm,intm)intm\r\n\r\ntype dll0_r64=ref clang function:r64\r\ntype dll1_r64=ref clang function(intm)r64\r\ntype dll2_r64=ref clang function(intm,intm)r64\r\n\r\ntype dll0_r64x=ref clang function:r64\r\ntype dll1_r64x=ref clang function(real)r64\r\ntype dll2_r64x=ref clang function(real,real)r64\r\n\r\ntype m_dll0_intm=ref function:intm\r\ntype m_dll1_intm=ref function(intm)intm\r\ntype m_dll2_intm=ref function(intm,intm)intm\r\ntype m_dll3_intm=ref function(intm,intm,intm)intm\r\ntype m_dll4_intm=ref function(intm,intm,intm,intm)intm\r\ntype m_dll5_intm=ref function(intm,intm,intm,intm,intm)intm\r\ntype m_dll12_intm=ref function(intm,intm,intm,intm, intm,intm,intm,intm, intm,intm,intm,intm)intm\r\n\r\ntype m_dll0_r64=ref function:r64\r\ntype m_dll1_r64=ref function(intm)r64\r\ntype m_dll2_r64=ref function(intm,intm)r64\r\n\r\n\r\nglobal function os_calldllfunction(ref proc fnaddr,\r\n\t\tint retcode, nargs, ref[]i64 args, ref[]byte argcodes)word64 =\r\n!retcode is 'R' or 'I'\r\n!each argcodes element is 'R' or 'I' too\r\n!The x64 version can work with any combination.\r\n!Here, for C, only some combinations are dealt with:\r\n! I result, params all I (not all param counts)\r\n! R result, params all I (not all param counts)\r\n!Mixed params, for arbitrary return type, not handled (not really detected either)\r\n\r\n\tvar word64 a\r\n\tvar real64 x\r\n\tvar int oddstack, nextra, pushedbytes\r\n\r\n\tif retcode='I' then\r\n\t\treturn calldll_cint(fnaddr,args,nargs)\r\n\telse\r\n\t\treturn calldll_creal(fnaddr,args,nargs)\r\n\tfi\r\nend\t\r\n\r\nglobal proc os_pushargs(ref[]word64 args, int nargs, nextra)=\r\n!implements central part of 'callapplproc' which needs to be in asm\r\n\tvar word64 a\r\n\r\nABORTPROGRAM(\"PUSHARGS/C NOT READY\")\r\nend\r\n\r\nfunction calldll_cint (ref proc fnaddr,ref[]i64 params,int nparams)i64=\r\nswitch nparams\r\nwhen 0 then\r\n\treturn dll0_intm(fnaddr)^()\r\nwhen 1 then\r\n\treturn dll1_intm(fnaddr)^(params^[1])\r\nwhen 2 then\r\n\treturn dll2_intm(fnaddr)^(params^[1],params^[2])\r\nwhen 3 then\r\n\treturn dll3_intm(fnaddr)^(params^[1],params^[2],params^[3])\r\nwhen 4 then\r\n\treturn dll4_intm(fnaddr)^(params^[1],params^[2],params^[3],\r\n\t\t\tparams^[4])\r\nwhen 5 then\r\n\treturn dll5_intm(fnaddr)^(params^[1],params^[2],params^[3],\r\n\t\t\tparams^[4], params^[5])\r\nwhen 6 then\r\n\treturn dll6_intm(fnaddr)^(params^[1],params^[2],params^[3],\r\n\t\t\tparams^[4], params^[5],params^[6])\r\nwhen 9 then \r\n\treturn (dll9_intm(fnaddr))^(params^[1],params^[2],params^[3],params^[4],\tparams^[5],params^[6],\r\n\t\t\t\tparams^[7],params^[8],params^[9])\r\nwhen 10 then \r\n\treturn (dll10_intm(fnaddr))^(params^[1],params^[2],params^[3],params^[4],\tparams^[5],params^[6],\r\n\t\t\t\tparams^[7],params^[8],params^[9],params^[10])\r\nwhen 11 then \r\n\treturn (dll11_intm(fnaddr))^(params^[1],params^[2],params^[3],params^[4],\tparams^[5],params^[6],\r\n\t\t\t\tparams^[7],params^[8],params^[9],params^[10],\tparams^[11])\r\n\r\nwhen 12 then \r\n\treturn (dll12_intm(fnaddr))^(params^[1],params^[2],params^[3],params^[4],\tparams^[5],params^[6],\r\n\t\t\t\tparams^[7],params^[8],params^[9],params^[10],\tparams^[11],params^[12])\r\n\r\nwhen 14 then \r\n\treturn (dll14_intm(fnaddr))^(params^[1],params^[2],params^[3],params^[4],\tparams^[5],params^[6],\r\n\t\t\t\tparams^[7],params^[8],params^[9],params^[10],\tparams^[11],params^[12],\r\n\t\t\t\tparams^[13],params^[14])\r\n\r\nelse\r\n\tcpl nparams\r\n\tprintln \"calldll/c/int unsupported # of params\", nparams\r\n\tstop 1\r\nendswitch\r\nreturn 0\r\nend\r\n\r\nfunction calldll_creal (ref proc fnaddr,ref[]i64 params,int nparams)i64=\r\nvar real64 x\r\n\r\nswitch nparams\r\nwhen 0 then\r\n\treturn dll0_r64(fnaddr)^()\r\nwhen 1 then\r\n\tos_dummycall(params^[1],params^[2],params^[3],params^[4])\r\n\tx:=dll1_r64(fnaddr)^(params^[1])\r\nwhen 2 then\r\n\tx:=dll2_r64(fnaddr)^(params^[1],params^[2])\r\nelse\r\n\tprintln \"calldll/c/real too many params\"\r\n\tstop 1\r\nendswitch\r\nreturn int64@(x)\r\nend\r\n\r\n\r\nglobal proc os_dummycall(r64 a,b,c,d)=\r\nend\r\n"};
static struct mm_genc_stlinkrec *  mm_genc_allsymbols;
static struct mm_genc_stlinkrec *  mm_genc_allsymbolsx;
static i64 mm_genc_nallprocs;
static i64 mm_genc_nexports;
struct mm_decls_unitrec *  mm_libc_zerounit;
struct mm_decls_unitrec *  mm_libc_nilunit;
static byte mm_libc_clinebuffer[4096];
byte *  mm_libc_clineptr;
byte *  mm_libc_clineend;
static i64 mm_blockc_loopstack[50][4];
static i64 mm_blockc_loopindex;
i64 mm_blockc_blocklevel;
static i64 mm_parse_intabledata = (i64)0;
static i64 mm_parse_inreadprint = (i64)0;
static i64 mm_parse_inparamlist = (i64)0;
static i64 mm_parse_inrecordbody = (i64)0;
static i64 mm_parse_inimportmodule = (i64)0;
static i64 mm_parse_labelseen = (i64)0;
static byte *  mm_parse_tabledataname = 0;
static struct mm_decls_strec *  mm_parse_procstack[10];
static i64 mm_parse_nprocstack = (i64)0;
static struct mm_decls_uflagsrec mm_parse_unionstring;
static struct mm_decls_uflagsrec mm_parse_unionpend;
static struct mm_decls_strec *  mm_parse_unionlastvar = 0;
static struct mm_decls_strec *  mm_parse_dretvar;
static i64 mm_parse_try_level = (i64)0;
static i64 mm_parse_varattribs = (i64)0;
static struct mm_decls_unitrec *  mm_parse_dollarstack[10];
static i64 mm_parse_ndollar = (i64)0;
static i64 mm_parse_inmultexpr = (i64)0;
static i64 mm_parse_insiderecord = (i64)0;
static i64 mm_parse_insidedllimport = (i64)0;
static struct mm_decls_strec *  mm_name_currstproc;
static i64 mm_name_allowmodname = (i64)0;
static i64 mm_name_noexpand;
static i64 mm_name_noassem;
static i64 mm_name_macrolevels;
static struct mm_decls_strec *  mm_name_macroparams[50];
static struct mm_decls_strec *  mm_name_macroparamsgen[50];
static struct mm_decls_unitrec *  mm_name_macroargs[50];
static i64 mm_name_nmacroparams;
static i64 mm_name_nmacroargs;
static byte *  mm_type_lvnames[5] = {(byte*)"no_lv",(byte*)"need_lv",(byte*)"addrof_lv",(byte*)"index_lv",(byte*)"indexlv_lv"};
static i64 mm_type_countedfields;

/* PROCDEFS */
// START
void start(void) {
    mm_start_addmodulemapping((byte*)"oslib",(byte*)"oslinux",(byte *)(0),(byte *)(0));
    mm_start_addmodulemapping((byte*)"osdll",(byte*)"oswindllc",(byte *)(0),(byte *)(0));
    mm_start_addmodulemapping((byte*)"msys",(byte*)"msysc",(byte *)(0),(byte *)(0));
    mm_start_addmodulemapping((byte*)"clib",(byte*)"clibc",(byte *)(0),(byte *)(0));
    mm_start_start_common((i64)4);
}

int main(int nargs, char** args) {
int i;
	msysc_nsysparams=nargs;
	if (msysc_nsysparams>nargs) {puts("Too many params"); exit(1);}
	for (i=1; i<=nargs; ++i) msysc_sysparams[i-1]=(byte*)args[i-1];

	mm_libc_Dinit();

	start();
	return 0;
}

void mm_start_start_common(i64 itarget) {
    mm_start_startclock = oslinux_os_clock();
    mm_decls_target = itarget;
    mm_decls_ctarget = (i64)(mm_decls_tg_ctarget[(mm_decls_target)-1]);
    mm_decls_islinux = (i64)(mm_decls_tg_islinux[(mm_decls_target)-1]);
    mm_decls_targetbits = (i64)(mm_decls_tg_targetbits[(mm_decls_target)-1]);
    mm_decls_targetsize = (mm_decls_targetbits / (i64)8);
    mm_start_addoptionvar((byte*)"target",mm_decls_targetnames[(mm_decls_target)-1]);
    mm_start_addoptionvar((byte*)"os",mm_decls_targetosnames[(mm_decls_target)-1]);
    mm_start_addoptionvar((byte*)"targetlang",mm_decls_targetlangnames[(mm_decls_target)-1]);
    mm_start_addoptionvar((byte*)"ctarget",(!!(mm_decls_ctarget)?(byte*)"1":(byte*)"0"));
    mm_start_initdata();
    mm_start_getinputoptions();
    if (!!((u64)(mm_decls_fdebugcompiler))) {
        mm_start_debugcompiler();
        exit(0);
    };
    if ((!(!!(mm_decls_fverbose)) && !(!!(mm_decls_fquiet)))) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"Compiling",NULL);
        msysc_m_print_str(mm_decls_inputfiles[((i64)1)],NULL);
        msysc_m_print_str((byte*)"to",NULL);
        msysc_m_print_str(mm_start_outfile,NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
    };
    mm_start_initsearchdirs();
    mm_start_do_loadmodules();
    mm_start_do_parse();
    mm_start_do_writema();
    mm_start_do_name();
    mm_start_do_type();
    mm_genc64_do_codegen1(mm_start_outfilesource,mm_start_fshowpcl1);
    mm_genc64_do_codegen2(mm_start_fshowmcl1);
    mm_genc64_do_writesource(mm_start_outfilesource,(mm_start_cc_mode < (i64)2));
    if ((mm_start_cc_mode >= (i64)2)) {
        mm_genc64_do_link(mm_start_outfilesource,mm_start_outfilebin,mm_start_linkoption,mm_start_ccompiler,mm_start_foptimise);
    };
    mm_start_endclock = oslinux_os_clock();
    if ((mm_start_cc_mode == (i64)3)) {
        mm_start_do_runprog();
    };
    if (!!(mm_decls_fverbose)) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"Finished.",NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
    };
    if (!!(mm_start_fshowtiming)) {
        mm_start_showtiming();
    };
    exit((i64)0);
}

static void mm_start_debugcompiler(void) {
    byte str[200];
    i64 t;
    void *  logdev;
    mm_start_fshowasm = (!!((i64)1) && (mm_start_passlevel >= (i64)6));
    mm_start_fshowmcl1 = (!!((i64)1) && (mm_start_passlevel >= (i64)5));
    mm_start_fshowpcl1 = (!!((i64)1) && (mm_start_passlevel >= (i64)4));
    mm_start_fshowast1 = (i64)1;
    mm_start_fshowast2 = (!!((i64)1) && (mm_start_passlevel >= (i64)2));
    mm_start_fshowast3 = (!!((i64)1) && (mm_start_passlevel >= (i64)3));
    mm_start_fshowst = (i64)1;
    mm_start_cc_mode = (i64)0;
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"*************DEBUG:Compiling",NULL);
    msysc_m_print_str(mm_decls_inputfiles[((i64)1)],NULL);
    msysc_m_print_str((byte*)"to",NULL);
    msysc_m_print_str(mm_start_outfile,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    mm_start_initsearchdirs();
    remove((i8 *)((byte*)"qx.log"));
    mm_start_do_loadmodules();
    t = (i64)(clock());
    if ((mm_start_passlevel >= (i64)1)) {
        mm_start_do_parse();
        mm_start_showast((byte*)"AST1",&mm_start_fshowast1);
        mm_start_do_writema();
    };
    if ((mm_start_passlevel >= (i64)2)) {
        mm_start_do_name();
        mm_start_showast((byte*)"AST2",&mm_start_fshowast2);
    };
    if ((mm_start_passlevel >= (i64)3)) {
        mm_start_do_type();
        mm_start_showast((byte*)"AST3",&mm_start_fshowast3);
    };
    if ((mm_start_passlevel >= (i64)4)) {
        mm_genc64_do_codegen1(mm_start_outfilesource,mm_start_fshowpcl1);
    };
    if ((mm_start_passlevel >= (i64)5)) {
        mm_genc64_do_codegen2(mm_start_fshowmcl1);
    };
    if ((mm_start_passlevel >= (i64)6)) {
        mm_genc64_do_writesource(mm_start_outfilesource,(mm_start_cc_mode < (i64)2));
    };
    if ((mm_start_passlevel >= (i64)7)) {
        mm_genc64_do_link(mm_start_outfilesource,mm_start_outfilebin,mm_start_linkoption,mm_start_ccompiler,mm_start_foptimise);
    };
    mm_start_endclock = oslinux_os_clock();
    if (!!(mm_start_fshowtiming)) {
        mm_start_showtiming();
    };
    if (!!(mm_decls_fverbose)) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"Finished.",NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
    };
    if (((((((((((mm_start_fshowpcl1 == (i64)2) || (mm_start_fshowpcl2 == (i64)2)) || (mm_start_fshowast1 == (i64)2)) || (mm_start_fshowast2 == (i64)2)) || (mm_start_fshowast3 == (i64)2)) || !!(mm_start_fshowst)) || !!(mm_start_fshowstflat)) || !!(mm_start_fshowtypes)) || (mm_start_fshowmcl1 == (i64)2)) || !!(mm_start_fshowasm))) {
        logdev = fopen((i8 *)((byte*)"qx.log"),(i8 *)((byte*)"w"));
        if ((mm_start_fshowasm == (i64)1)) {
            mm_start_addtolog(mm_start_outfilesource,logdev);
        };
        if ((mm_start_fshowmcl1 == (i64)2)) {
            mm_start_addtolog((byte*)"MCL",logdev);
        };
        if ((mm_start_fshowpcl1 == (i64)2)) {
            mm_start_addtolog((byte*)"PCL",logdev);
        };
        if ((mm_start_fshowast3 == (i64)2)) {
            mm_start_addtolog((byte*)"AST3",logdev);
        };
        if ((mm_start_fshowast2 == (i64)2)) {
            mm_start_addtolog((byte*)"AST2",logdev);
        };
        if ((mm_start_fshowast1 == (i64)2)) {
            mm_start_addtolog((byte*)"AST1",logdev);
        };
        if (!!(mm_start_fshowst)) {
            mm_start_showsttree((byte*)"SYMBOL TABLE",logdev);
        };
        if (!!(mm_start_fshowstflat)) {
            mm_start_showstflat((byte*)"FLAT SYMBOL TABLE",logdev);
        };
        if (!!(mm_start_fshowtypes)) {
            mm_diags_printmodelist(logdev);
        };
        fclose(logdev);
        msysc_m_print_startstr(str);
        msysc_m_print_str((byte*)"\\m\\med.bat",NULL);
        msysc_m_print_str((byte*)"qx.log",NULL);
        msysc_m_print_end();
        ;
        if (!!(mlib_checkfile((byte*)"mc.m"))) {
            oslinux_os_execwait(str,(i64)1,(byte *)(0));
        } else {
            msysc_m_print_startcon();
            msysc_m_print_str((byte*)"Diagnostic outputs written to",NULL);
            msysc_m_print_str((byte*)"qx.log",NULL);
            msysc_m_print_newline();
            msysc_m_print_end();
            ;
        };
    };
    exit((i64)0);
}

static void mm_start_do_loadmodules(void) {
    if (!!(mm_decls_fbundled)) {
        mm_support_loadmafile();
    };
    mm_start_loadmainmodule(mm_decls_inputfiles[((i64)1)]);
}

static void mm_start_do_parse(void) {
    i64 i;
    L1 :;
    for (i=(i64)2;i<=mm_decls_nmodules;++i) {
L2 :;
        mm_parse_parsemodule(i);
L3 :;
    }L4 :;
    ;
    mm_parse_parsemodule((i64)1);
    mm_name_fixusertypes();
}

static void mm_start_do_name(void) {
    i64 i;
    L5 :;
    for (i=(i64)2;i<=mm_decls_nmodules;++i) {
L6 :;
        mm_name_rx_module(i);
L7 :;
    }L8 :;
    ;
    mm_name_rx_module((i64)1);
}

static void mm_start_do_type(void) {
    i64 i;
    mm_type_tx_typetable();
    L9 :;
    for (i=(i64)1;i<=mm_decls_nmodules;++i) {
L10 :;
        mm_type_tx_module(i);
L11 :;
    }L12 :;
    ;
}

static void mm_start_do_runprog(void) {
    byte str[300];
    i64 i;
    if (!!(mm_decls_islinux)) {
        msysc_m_print_startstr(str);
        msysc_m_print_str((byte*)"./",NULL);
        msysc_m_print_nogap();
        msysc_m_print_str(mm_start_outfilebin,NULL);
        msysc_m_print_end();
        ;
    } else {
        strcpy((i8 *)(str),(i8 *)(mm_start_outfilebin));
    };
    L13 :;
    for (i=(i64)1;i<=mm_start_nextraparams;++i) {
L14 :;
        strcat((i8 *)(str),(i8 *)((byte*)" "));
        strcat((i8 *)(str),(i8 *)(mm_start_extraparams[(i)-1]));
        if (!!(mm_start_extravalues[(i)-1])) {
            strcat((i8 *)(str),(i8 *)((byte*)":"));
            strcat((i8 *)(str),(i8 *)(mm_start_extravalues[(i)-1]));
        };
L15 :;
    }L16 :;
    ;
    oslinux_os_execwait(str,(i64)0,(byte *)(0));
}

static i64 mm_start_loadmainmodule(byte * filespec) {
    byte modulename[100];
    byte path[300];
    i64 flag;
    i64 fileno;
    mlib_pcm_clearmem((void *)(&mm_decls_moduletable[((i64)0)]),(i64)98);
    mm_decls_sourcefilenames[((i64)0)] = (byte*)"<dummy file>";
    mm_decls_sourcefilepaths[((i64)0)] = (byte*)"<dummy path>";
    mm_decls_sourcefiletext[((i64)0)] = (byte*)"<sourcefile0>";
    mm_decls_sourcefilesizes[((i64)0)] = (i64)(strlen((i8 *)(mm_decls_sourcefiletext[((i64)0)])));
    mm_decls_moduletable[((i64)0)].name = (byte*)"PROGRAM";
    mm_decls_moduletable[((i64)0)].fileno = (i64)0;
    mm_decls_stprogram = mm_lib_createdupldef((struct mm_decls_strec *)(0),mm_lex_addnamestr((byte*)"$prog"),(i64)1);
    mm_decls_moduletable[((i64)0)].stmodule = mm_decls_stprogram;
    fileno = mm_support_getmainfile(filespec);
    mm_decls_infotext = (byte *)(0);
    if (!!(mm_decls_ctarget)) {
        mm_decls_infotext = (byte *)(mlib_readfile(mlib_changeext(filespec,(byte*)"txt")));
    };
    strcpy((i8 *)(modulename),(i8 *)(mlib_extractbasefile(filespec)));
    strcpy((i8 *)(path),(i8 *)(mlib_extractpath(filespec)));
    if (!!((u64)(path[((i64)1)-1]))) {
        mm_start_addsearchdir(path);
    };
    mm_start_addmodule(modulename,fileno,&flag);
    mm_support_addspecialtypes();
    return (i64)1;
}

static i64 mm_start_addmodule(byte * modulename,i64 fileno,i64 * exportflag) {
    struct mm_decls_modulerec m;
    byte *  importnames[50];
    byte importflags[51];
    i64 importmoduleno[50];
    i64 nimports;
    i64 i;
    i64 k;
    i64 flag;
    i64 j;
    i64 newmodno;
    struct mm_decls_modulerec *  pmodule;
    mlib_convlcstring(modulename);
    mlib_pcm_clearmem((void *)(&m),(i64)98);
    m.name = mlib_pcm_copyheapstring(modulename);
    m.fileno = fileno;
    mm_decls_stmodule = mm_lib_createnewmoduledef(mm_decls_stprogram,mm_lex_addnamestr(m.name));
    m.stmodule = mm_decls_stmodule;
    if ((mm_decls_nmodules >= (i64)50)) {
        mm_support_loaderror((byte*)"Too many modules",modulename,(byte*)"");
    };
    pmodule = &mm_decls_moduletable[((newmodno = ++mm_decls_nmodules))];
    memcpy(&(*pmodule),&m,98);
    (*pmodule).importmap[(newmodno)-1] = (u64)((i64)1);
    (*m.stmodule).moduleno = (u64)(newmodno);
    memset((void *)(&importflags),(i64)0,(u64)((i64)51));
    nimports = mm_start_readimportlist(&m,&importnames,&importflags,(i64)50);
    L17 :;
    for (i=(i64)1;i<=nimports;++i) {
L18 :;
        flag = (i64)0;
        if ((mm_decls_fverbose == (i64)2)) {
            msysc_m_print_startcon();
            msysc_m_print_str((byte*)"Load import for",NULL);
            msysc_m_print_str(modulename,NULL);
            msysc_m_print_str((byte*)"IMPORTNAMES[I]=",NULL);
            msysc_m_print_str(importnames[(i)-1],NULL);
            msysc_m_print_newline();
            msysc_m_print_end();
            ;
        };
        k = mm_start_loadimport(importnames[(i)-1],&flag,modulename);
        if (!!(flag)) {
            importflags[(i)] = (u64)((i64)1);
        };
        (*pmodule).importmap[(k)-1] = (u64)((i64)1);
        importmoduleno[(i)-1] = k;
L19 :;
    }L20 :;
    ;
    L21 :;
    for (i=(i64)1;i<=nimports;++i) {
L22 :;
        if (!!((u64)(importflags[(i)]))) {
            k = importmoduleno[(i)-1];
            L25 :;
            for (j=(i64)1;j<=mm_decls_nmodules;++j) {
L26 :;
                if (!!((u64)(mm_decls_moduletable[(k)].importmap[(j)-1]))) {
                    (*pmodule).importmap[(j)-1] = (u64)((i64)1);
                };
L27 :;
            }L28 :;
            ;
        };
L23 :;
    }L24 :;
    ;
    (*exportflag) = (i64)(importflags[((i64)0)]);
    return newmodno;
}

static i64 mm_start_loadimport(byte * modulename,i64 * exportflag,byte * ownername) {
    i64 i;
    i64 fileno;
    byte *  newname;
    newname = modulename;
    L29 :;
    for (i=(i64)1;i<=mm_decls_nmodules;++i) {
L30 :;
        if (!!(mlib_eqstring(mm_decls_moduletable[(i)].name,newname))) {
            return i;
        };
L31 :;
    }L32 :;
    ;
    fileno = mm_support_getmodulefile(modulename,ownername);
    return mm_start_addmodule(newname,fileno,exportflag);
}

static i64 mm_start_readimportlist(struct mm_decls_modulerec * m,byte * (*importnames)[],byte (*importflags)[],i64 maximports) {
    i64 n;
    i64 flag;
    i64 exportflag;
    i64 i;
    byte *  iname;
    i64 needmsys;
    byte *  msysname;
    mm_lex_startlex((byte*)"IMPORTS",(*m).fileno);
    exportflag = (i64)0;
    n = (i64)0;
    L33 :;
    while (1) {
        mm_lex_lexreadtoken();
        if (((i64)(mm_decls_nextlx.symbol)==(i64)36)) {
            goto L34 ;
        }else if (((i64)(mm_decls_nextlx.symbol)==(i64)6) || ((i64)(mm_decls_nextlx.symbol)==(i64)35)) {
        }else if (((i64)(mm_decls_nextlx.symbol)==(i64)37)) {
            flag = (i64)0;
            if (!!(mm_lex_checkname((byte*)"import",(i64)0))) {
                mm_start_pslex();
                if ((((i64)((u64)(mm_decls_nextlx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_nextlx.subcode)) == (i64)39))) {
                    flag = (i64)1;
                    mm_start_pslex();
                };
                if (((i64)((u64)(mm_decls_nextlx.symbol)) != (i64)37)) {
                    mlib_abortprogram((byte*)"import: modulename expected");
                };
                if ((++n >= maximports)) {
                    mlib_abortprogram((byte*)"too many imports");
                };
                iname = mm_start_mapimport(mm_decls_nextlx.svalue);
                (*importnames)[(n)-1] = mlib_pcm_copyheapstring(iname);
                (*importflags)[(n)] = (u64)(flag);
            } else if (!!(mm_lex_checkname((byte*)"importpath",(i64)0))) {
                mm_start_pslex();
                if (((i64)((u64)(mm_decls_nextlx.symbol)) == (i64)45)) {
                    mm_start_addsearchdir(mm_decls_nextlx.svalue);
                    mm_start_pslex();
                } else {
                    mlib_abortprogram((byte*)"string path expected");
                };
            } else if (!!(mm_lex_checkname((byte*)"mapmodule",(i64)0))) {
                mm_start_domapmodule();
            } else if (!!(mm_lex_checkname((byte*)"as",(i64)0))) {
                mm_start_pslex();
                mm_start_pslex();
            } else {
                goto L34 ;
            };
        } else {
            goto L34 ;
        };
    }L34 :;
    ;
    msysname = (!!(mm_decls_ctarget)?(byte*)"msysc":(byte*)"msys");
    if ((mm_decls_nmodules == (i64)1)) {
        needmsys = (i64)1;
        L35 :;
        for (i=(i64)1;i<=n;++i) {
L36 :;
            if (!!(mlib_eqstring((*importnames)[(i)-1],msysname))) {
                needmsys = (i64)0;
                goto L38 ;
            };
L37 :;
        }L38 :;
        ;
        if (!!((u64)(mm_decls_fnomsys))) {
            needmsys = (i64)0;
        };
        if (!!(needmsys)) {
            ++n;
            (*importnames)[(n)-1] = mlib_pcm_copyheapstring(msysname);
            (*importflags)[(n)] = (u64)((i64)0);
        };
    };
    (*importflags)[((i64)0)] = (u64)(exportflag);
    return n;
}

static void mm_start_pslex(void) {
    static byte psname[256];
    mm_decls_prescanmode = (i64)1;
    mm_lex_lexreadtoken();
    mm_decls_prescanmode = (i64)0;
    if (((i64)((u64)(mm_decls_nextlx.symbol)) == (i64)37)) {
        strcpy((i8 *)(psname),(i8 *)(mm_lex_convertzstring(mm_decls_nextlx.svalue,(i64)(mm_decls_nextlx.length))));
        mm_decls_nextlx.svalue = psname;
    };
}

static void mm_start_initdata(void) {
    mlib_pcm_init();
    mm_lex_lexsetup();
    mm_support_inittypetables();
    mm_lib_initqclib();
}

static void mm_start_initsearchdirs(void) {
    mm_decls_nsearchdirs = (i64)0;
    mm_start_addsearchdir((byte*)"c:/mcc/");
    mm_start_addsearchdir(oslinux_os_getmpath());
    mm_start_addsearchdir(oslinux_os_gethostname());
    mm_start_addsearchdir((byte*)"./");
}

static void mm_start_addsearchdir(byte * path) {
    i64 i;
    L39 :;
    for (i=(i64)1;i<=mm_decls_nsearchdirs;++i) {
L40 :;
        if (!!(mlib_eqstring(mm_decls_searchdirs[(i)-1],path))) {
            return;
        };
L41 :;
    }L42 :;
    ;
    if ((mm_decls_nsearchdirs > (i64)10)) {
        mm_support_loaderror((byte*)"Too many search paths",(byte*)"",(byte*)"");
    };
    mm_decls_searchdirs[(++mm_decls_nsearchdirs)-1] = mlib_pcm_copyheapstring(path);
}

static void mm_start_showsearchdirs(void) {
    i64 i;
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"Import search paths:",NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    L43 :;
    for (i=(i64)1;i<=mm_decls_nsearchdirs;++i) {
L44 :;
        if (!!((u64)((*mm_decls_searchdirs[(i)-1])))) {
            msysc_m_print_startcon();
            msysc_m_print_i64(i,NULL);
            msysc_m_print_nogap();
            msysc_m_print_str((byte*)":",NULL);
            msysc_m_print_str(mm_decls_searchdirs[(i)-1],NULL);
            msysc_m_print_newline();
            msysc_m_print_end();
            ;
        } else {
            msysc_m_print_startcon();
            msysc_m_print_i64(i,NULL);
            msysc_m_print_nogap();
            msysc_m_print_str((byte*)": .",NULL);
            msysc_m_print_newline();
            msysc_m_print_end();
            ;
        };
L45 :;
    }L46 :;
    ;
    msysc_m_print_startcon();
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
}

static void mm_start_showast(byte * filename,i64 * flag) {
    void *  f;
    if (!(!!((*flag)))) {
        return;
    };
    (*flag) = (i64)2;
    f = fopen((i8 *)(filename),(i8 *)((byte*)"w"));
    if (!(!!(f))) {
        return;
    };
    msysc_m_print_startfile(f);
    msysc_m_print_str((byte*)"PROC",NULL);
    msysc_m_print_str(filename,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    mm_diags_printcode(f,(byte*)"");
    msysc_m_print_startfile(f);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    fclose(f);
}

static void mm_start_showstflat(byte * caption,void * f) {
    msysc_m_print_startfile(f);
    msysc_m_print_str((byte*)"PROC",NULL);
    msysc_m_print_str(caption,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    mm_diags_printstflat(f);
    msysc_m_print_startfile(f);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
}

static void mm_start_showsttree(byte * caption,void * f) {
    msysc_m_print_startfile(f);
    msysc_m_print_str((byte*)"PROC",NULL);
    msysc_m_print_str(caption,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    mm_diags_printst(f,mm_decls_stprogram,(i64)0);
    msysc_m_print_startfile(f);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
}

void mm_start_showtiming(void) {
    double t;
    t = (double)((mm_start_endclock - mm_start_startclock));
    if (!!(mm_decls_nalllines)) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"Time:",NULL);
        msysc_m_print_r64(t,NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
    } else {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"Time:",NULL);
        msysc_m_print_r64(t,NULL);
        msysc_m_print_i64(mm_decls_nalllines,NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
    };
}

static void mm_start_getinputoptions(void) {
    i64 paramno;
    i64 pmtype;
    i64 sw;
    i64 ncolons;
    byte *  name;
    byte *  value;
    byte *  filename;
    byte *  ext;
    i64 av_1;
    paramno = (i64)2;
    ncolons = (i64)0;
    L47 :;
    while (!!((pmtype = mlib_nextcmdparam(&paramno,&name,&value,(byte*)"m")))) {
        if ((pmtype==(i64)1)) {
            mlib_convlcstring(name);
            L50 :;
            for (sw=(i64)1;sw<=(i64)30;++sw) {
L51 :;
                if (!!(mlib_eqstring(name,mm_start_optionnames[(sw)-1]))) {
                    mm_start_do_option(sw,value);
                    goto L53 ;
                };
L52 :;
            }
            {
                msysc_m_print_startcon();
                msysc_m_print_str((byte*)"Unknown option:",NULL);
                msysc_m_print_str(name,NULL);
                msysc_m_print_newline();
                msysc_m_print_end();
                ;
                exit((i64)99);
            }L53 :;
            ;
        }else if ((pmtype==(i64)2)) {
            if ((mm_decls_ninputfiles >= (i64)50)) {
                mm_support_loaderror((byte*)"Too many input files",(byte*)"",(byte*)"");
            };
            mlib_convlcstring(name);
            mm_decls_inputfiles[(++mm_decls_ninputfiles)] = mlib_pcm_copyheapstring(name);
        }else if ((pmtype==(i64)3)) {
            if ((mm_decls_nlibfiles >= (i64)50)) {
                mm_support_loaderror((byte*)"Too many lib files",(byte*)"",(byte*)"");
            };
            mm_decls_libfiles[(++mm_decls_nlibfiles)] = mlib_pcm_copyheapstring(name);
        }else if ((pmtype==(i64)4)) {
            if ((++ncolons > (i64)1)) {
                name = (byte*)":";
                value = (byte *)(0);
                goto L54 ;
;
            };
        }else if ((pmtype==(i64)5)) {
            //doextra:
L54 :;
;
            mm_start_extraparams[(++mm_start_nextraparams)-1] = mlib_pcm_copyheapstring(name);
            mm_start_extravalues[(mm_start_nextraparams)-1] = mlib_pcm_copyheapstring(value);
        };
L48 :;
    }L49 :;
    ;
    if ((mm_start_cc_mode == (i64)0)) {
        mm_start_cc_mode = (i64)2;
    };
    if ((mm_start_linkoption == 0)) {
        mm_start_linkoption = (byte*)"exe";
    };
    if (((mm_decls_ninputfiles == (i64)0) && !(!!(mm_start_fwritelibs)))) {
        mm_start_showcaption();
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"Usage:",NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"\t",NULL);
        msysc_m_print_nogap();
        msysc_m_print_str(msysc_sysparams[((i64)1)-1],NULL);
        msysc_m_print_str((byte*)"filename[.m]     # Compile project to executable",NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"\t",NULL);
        msysc_m_print_nogap();
        msysc_m_print_str(msysc_sysparams[((i64)1)-1],NULL);
        msysc_m_print_str((byte*)"-help            # Other options",NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        exit(0);
    } else if ((mm_decls_ninputfiles == (i64)1)) {
        filename = mm_decls_inputfiles[((i64)1)];
        ext = mlib_extractext(filename,(i64)0);
        if (!!(mlib_eqstring(ext,(byte*)"ma"))) {
            mm_decls_fbundled = (i64)1;
            mm_decls_mafilename = mlib_pcm_copyheapstring(filename);
            mm_decls_inputfiles[((i64)1)] = mlib_pcm_copyheapstring(mlib_changeext(filename,(byte*)"m"));
        };
        mm_start_outfilesource = mlib_pcm_copyheapstring(mlib_changeext(filename,mm_decls_targetexts[(mm_decls_target)-1]));
        if ((!!(mm_decls_islinux) && !!(mlib_eqstring(mm_start_linkoption,(byte*)"exe")))) {
            mm_start_linkoption = (byte*)"";
        };
        mm_start_outfilebin = mlib_pcm_copyheapstring(mlib_changeext(filename,mm_start_linkoption));
        if ((mm_start_cc_mode == (i64)1)) {
            if (!!(mm_start_destfilename)) {
                mm_start_outfilesource = mm_start_destfilename;
            };
            mm_start_outfile = mm_start_outfilesource;
        } else {
            if (!!(mm_start_destfilename)) {
                mm_start_outfilebin = mm_start_destfilename;
            };
            mm_start_outfile = mm_start_outfilebin;
        };
    } else {
        mm_support_loaderror((byte*)"Specify one lead module only",(byte*)"",(byte*)"");
    };
}

static void mm_start_do_option(i64 sw,byte * value) {
    if ((sw==(i64)7)) {
        mm_start_cc_mode = (i64)1;
    }else if ((sw==(i64)8)) {
        mm_start_cc_mode = (i64)2;
    }else if ((sw==(i64)9)) {
        mm_start_cc_mode = (i64)3;
    }else if ((sw==(i64)3)) {
        mm_start_ccompiler = (i64)1;
    }else if ((sw==(i64)4)) {
        mm_start_ccompiler = (i64)2;
    }else if ((sw==(i64)5)) {
        mm_start_ccompiler = (i64)3;
    }else if ((sw==(i64)6)) {
        mm_start_foptimise = (i64)1;
    }else if ((sw==(i64)1)) {
        mm_start_linkoption = (byte*)"exe";
    }else if ((sw==(i64)2)) {
        mm_start_linkoption = (byte*)"obj";
    }else if ((sw==(i64)18)) {
        mm_start_fshowtiming = (i64)1;
    }else if ((sw==(i64)19)) {
        mm_decls_fverbose = (i64)1;
    }else if ((sw==(i64)20)) {
        mm_decls_fverbose = (i64)2;
    }else if ((sw==(i64)21)) {
        mm_decls_fquiet = (i64)1;
    }else if ((sw==(i64)22) || (sw==(i64)23)) {
        mm_genc64_showhelp();
        exit(0);
    }else if ((sw==(i64)24)) {
        mm_decls_dointlibs = (i64)0;
    }else if ((sw==(i64)30)) {
        mm_start_fwritelibs = (i64)1;
    }else if ((sw==(i64)25)) {
        mm_start_destfilename = mlib_pcm_copyheapstring(value);
    }else if ((sw==(i64)27)) {
        mm_decls_fcheckunusedlocals = (i64)1;
    }else if ((sw==(i64)26)) {
        mm_decls_fnomsys = (u64)((i64)1);
    }else if ((sw==(i64)28)) {
        mm_decls_fdebugcompiler = (u64)((i64)1);
    }else if ((sw==(i64)10)) {
        mm_start_passlevel = (i64)0;
    }else if ((sw==(i64)11)) {
        mm_start_passlevel = (i64)1;
    }else if ((sw==(i64)12)) {
        mm_start_passlevel = (i64)2;
    }else if ((sw==(i64)13)) {
        mm_start_passlevel = (i64)3;
    }else if ((sw==(i64)14)) {
        mm_start_passlevel = (i64)4;
    }else if ((sw==(i64)15)) {
        mm_start_passlevel = (i64)5;
    }else if ((sw==(i64)16)) {
        mm_start_passlevel = (i64)6;
    }else if ((sw==(i64)29)) {
        mm_start_dosetoptionvar(value);
    }else if ((sw==(i64)17)) {
        mm_decls_fwritema = (i64)1;
    };
}

static void mm_start_showcaption(void) {
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"M Compiler",NULL);
    msysc_m_print_str((byte*)"30-Sep-2019",NULL);
    msysc_m_print_str((byte*)"12:03:27",NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
}

static void mm_start_addtolog(byte * filename,void * logdest) {
    void *  f;
    i64 c;
    f = fopen((i8 *)(filename),(i8 *)((byte*)"rb"));
    if ((f == 0)) {
        return;
    };
    L55 :;
    while (1) {
        c = (i64)(fgetc(f));
        if ((c == (i64)-1)) {
            goto L56 ;
        };
        fputc((i64)((i32)(c)),logdest);
    }L56 :;
    ;
    fclose(f);
}

static void mm_start_addoptionvar(byte * name,byte * value) {
    i64 i;
    if ((mm_start_noptionvars >= (i64)25)) {
        mlib_abortprogram((byte*)"Too many option vars");
    };
    L57 :;
    for (i=(i64)1;i<=mm_start_noptionvars;++i) {
L58 :;
        if (!!(mlib_eqstring(name,mm_start_optionvars[(i)-1]))) {
            msysc_m_print_startcon();
            msysc_m_print_str(name,NULL);
            msysc_m_print_newline();
            msysc_m_print_end();
            ;
            mlib_abortprogram((byte*)"Dupl optionvar");
        };
L59 :;
    }L60 :;
    ;
    mm_start_optionvars[(++mm_start_noptionvars)-1] = mlib_pcm_copyheapstring(name);
    if ((value == 0)) {
        mm_start_optionvalues[(mm_start_noptionvars)-1] = (byte*)"1";
    } else {
        mm_start_optionvalues[(mm_start_noptionvars)-1] = mlib_pcm_copyheapstring(value);
    };
}

void mm_start_addmodulemapping(byte * old,byte * new,byte * optionname,byte * valuename) {
    i64 option;
    i64 i;
    if (!!(optionname)) {
        option = mm_start_findoptionvar(optionname);
        if (!!(option)) {
            if (!!(mlib_eqstring(optionname,mm_start_optionvars[(option)-1]))) {
                if (!(!!(mlib_eqstring(mm_start_optionvalues[(option)-1],valuename)))) {
                    return;
                };
            };
        } else {
            return;
        };
    };
    if ((mm_decls_nmodulemap >= (i64)25)) {
        mlib_abortprogram((byte*)"Too many module mappings");
    };
    L61 :;
    for (i=(i64)1;i<=mm_decls_nmodulemap;++i) {
L62 :;
        if (!!(mlib_eqstring(old,mm_decls_genericmodules[(i)-1]))) {
            msysc_m_print_startcon();
            msysc_m_print_str(old,NULL);
            msysc_m_print_newline();
            msysc_m_print_end();
            ;
            mlib_abortprogram((byte*)"Dupl module mapping");
        };
L63 :;
    }L64 :;
    ;
    mm_decls_genericmodules[(++mm_decls_nmodulemap)-1] = mlib_pcm_copyheapstring(old);
    mm_decls_actualmodules[(mm_decls_nmodulemap)-1] = mlib_pcm_copyheapstring(new);
}

static void mm_start_dosetoptionvar(byte * s) {
    byte *  t;
    byte name[256];
    byte value[256];
    if (((s == 0) || ((i64)((*s)) == (i64)0))) {
        mlib_abortprogram((byte*)"set:no option");
    };
    t = name;
    strcpy((i8 *)(t),(i8 *)(s));
    value[((i64)1)-1] = (u64)0u;
    L65 :;
    while (!!((u64)((*t)))) {
        if (((u64)((*t)) == ':')) {
            (*t) = (u64)0u;
            strcpy((i8 *)(value),(i8 *)((t + (i64)1)));
            goto L67 ;
        };
        ++t;
L66 :;
    }L67 :;
    ;
    if (((i64)(value[((i64)1)-1]) == (i64)0)) {
        strcpy((i8 *)(value),(i8 *)((byte*)"1"));
    };
    mm_start_addoptionvar(name,value);
}

static i64 mm_start_findoptionvar(byte * name) {
    i64 i;
    L68 :;
    for (i=(i64)1;i<=mm_start_noptionvars;++i) {
L69 :;
        if (!!(mlib_eqstring(name,mm_start_optionvars[(i)-1]))) {
            return i;
        };
L70 :;
    }L71 :;
    ;
    return (i64)0;
}

static void mm_start_getpsname(byte * dest) {
    mm_start_pslex();
    if (((i64)(mm_decls_nextlx.symbol)==(i64)37)) {
    }else if (((i64)(mm_decls_nextlx.symbol)==(i64)45)) {
    }else if (((i64)(mm_decls_nextlx.symbol)==(i64)40)) {
        mm_decls_nextlx.svalue = msysc_strint((i64)(mm_decls_nextlx.svalue),(byte *)(0));
    } else {
        mlib_abortprogram((byte*)"map1");
    };
    strcpy((i8 *)(dest),(i8 *)(mm_decls_nextlx.svalue));
    mm_start_pslex();
}

static void mm_start_domapmodule(void) {
    byte genname[256];
    byte actualname[256];
    byte optionname[256];
    byte valuename[256];
    i64 cond;
    mm_start_getpsname(genname);
    if (((i64)((u64)(mm_decls_nextlx.symbol)) != (i64)11)) {
        mlib_abortprogram((byte*)"=> expected");
    };
    mm_start_getpsname(actualname);
    cond = (i64)0;
    if ((((i64)((u64)(mm_decls_nextlx.symbol)) == (i64)37) && !!(mm_lex_checkname((byte*)"when",(i64)0)))) {
        mm_start_getpsname(optionname);
        if ((((i64)((u64)(mm_decls_nextlx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_nextlx.subcode)) == (i64)30))) {
            mm_start_getpsname(valuename);
        } else {
            strcpy((i8 *)(valuename),(i8 *)((byte*)"1"));
        };
        cond = (i64)1;
    };
    L72 :;
    while (!(((mm_decls_nextlx.symbol == (i64)35) || (mm_decls_nextlx.symbol == (i64)36)))) {
        mm_start_pslex();
L73 :;
    }L74 :;
    ;
    if (!!(cond)) {
        mm_start_addmodulemapping(genname,actualname,optionname,valuename);
    } else {
        mm_start_addmodulemapping(genname,actualname,(byte *)(0),(byte *)(0));
    };
}

byte * mm_start_mapimport(byte * name) {
    i64 i;
    L75 :;
    for (i=(i64)1;i<=mm_decls_nmodulemap;++i) {
L76 :;
        if (!!(mlib_eqstring(name,mm_decls_genericmodules[(i)-1]))) {
            return mm_decls_actualmodules[(i)-1];
        };
L77 :;
    }L78 :;
    ;
    return name;
}

static void mm_start_do_writema(void) {
    if (!!(mm_decls_fwritema)) {
        if (!!(mm_decls_fbundled)) {
            mm_support_loaderror((byte*)"-ma used with .ma input",(byte*)"",(byte*)"");
        };
        mm_support_writemafile(mm_decls_inputfiles[((i64)1)],mm_start_destfilename);
        exit(0);
    };
}

i64 msysc_m_getdotindex(u64 a,i64 i) {
    return (((i64)(a) & ((i64)1 << i)) >> i);
}

void msysc_m_setdotindex(u64 * a,i64 i,i64 x) {
    u32 *  a32;
    if ((i >= (i64)32)) {
        (*a) = (u64)((((i64)((*a)) & ~(((i64)1 << i))) | (i64)(((u64)(x) << i))));
    } else {
        a32 = (u32 *)(a);
        (*a32) = (u64)((((i64)((u64)((*a32))) & ~(((i64)1 << i))) | (i64)(((u64)(x) << i))));
    };
}

i64 msysc_m_getdotslice(u64 a,i64 i,i64 j) {
    if ((i >= j)) {
        return (i64)(((a >> j) & ~(((u64)18446744073709551615u << ((i - j) + (i64)1)))));
    } else {
        return (i64)(((a >> i) & ~(((u64)18446744073709551615u << ((j - i) + (i64)1)))));
    };
}

void msysc_m_setdotslice(u64 * a,i64 i,i64 j,u64 x) {
    u64 mask64;
    u64 mask;
    u32 *  a32;
    if ((i > j)) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"SETDOTSLICE?",NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
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

i64 msysc_m_getnprocs(void) {
    return msysc__fnnprocs;
}

i64 msysc_m_getnexports(void) {
    return msysc__fnnexports;
}

void * msysc_m_getprocaddr(i64 n) {
    return msysc__fnaddresses[(n)-1];
}

byte * msysc_m_getprocname(i64 n) {
    return msysc__fnnames[(n)-1];
}

void * msysc_m_getprocexport(i64 n) {
    return (void *)(&msysc__fnexports[(n)-1]);
}

static void msysc_pushio(void) {
    if ((msysc_niostack >= (i64)10)) {
        printf((i8 *)((byte*)"Too many io levels\n"));
        exit((i64)53);
    };
    ++msysc_niostack;
    msysc_outchan_stack[(msysc_niostack)-1] = msysc_outchan;
    msysc_outdev_stack[(msysc_niostack)-1] = msysc_outdev;
    msysc_fmtstr_stack[(msysc_niostack)-1] = msysc_fmtstr;
    msysc_needgap_stack[(msysc_niostack)-1] = (u64)(msysc_needgap);
    msysc_needgap = (i64)0;
    msysc_fmtstr = (byte *)(0);
    msysc_outchan = 0;
}

void msysc_m_print_startfile(void * dev) {
    msysc_pushio();
    msysc_outchan = dev;
    if (!!(dev)) {
        msysc_outdev = (i64)2;
    } else {
        msysc_outdev = (i64)1;
    };
}

void msysc_m_print_startstr(byte * s) {
    byte * *  p;
    msysc_pushio();
    msysc_ptr_stack[(msysc_niostack)-1] = s;
    p = &msysc_ptr_stack[(msysc_niostack)-1];
    msysc_outchan = (void *)(p);
    msysc_outdev = (i64)3;
}

void msysc_m_print_startptr(byte * * p) {
    msysc_pushio();
    msysc_outchan = (void *)(p);
    msysc_outdev = (i64)3;
}

void msysc_m_print_startcon(void) {
    msysc_pushio();
    msysc_outdev = (i64)1;
}

void msysc_m_print_setfmt(byte * format) {
    msysc_fmtstr = format;
}

void msysc_m_print_end(void) {
    msysc_needgap = (i64)0;
    msysc_nextfmtchars((i64)1);
    if ((msysc_niostack == (i64)0)) {
        return;
    };
    msysc_outchan = msysc_outchan_stack[(msysc_niostack)-1];
    msysc_outdev = msysc_outdev_stack[(msysc_niostack)-1];
    msysc_fmtstr = msysc_fmtstr_stack[(msysc_niostack)-1];
    msysc_needgap = (i64)(msysc_needgap_stack[(msysc_niostack)-1]);
    --msysc_niostack;
}

void msysc_m_print_ptr(void * a,byte * fmtstyle) {
    msysc_nextfmtchars((i64)0);
    msysc_printstr(msysc_strword((u64)(a),(byte*)"z8h"));
    msysc_needgap = (i64)1;
}

void msysc_m_print_i64(i64 a,byte * fmtstyle) {
    byte s[40];
    struct msysc_fmtrec fmt;
    i64 n;
    msysc_nextfmtchars((i64)0);
    if ((fmtstyle == 0)) {
        if ((a >= (i64)0)) {
            n = msysc_u64tostr((u64)(a),s,(u64)((i64)10),(i64)0);
        } else {
            s[((i64)1)-1] = '-';
            n = (msysc_u64tostr((u64)(-(a)),&s[((i64)2)-1],(u64)((i64)10),(i64)0) + (i64)1);
        };
        msysc_printstr_n(s,n);
    } else {
        msysc_strtofmt(fmtstyle,(i64)-1,&fmt);
        if (((u64)(fmt.param) == 'V')) {
            msysc_fmtparam = a;
            msysc_needgap = (i64)0;
        } else {
            msysc_tostr_i64(a,&fmt);
        };
    };
    msysc_needgap = (i64)1;
}

void msysc_m_print_u64(u64 a,byte * fmtstyle) {
    struct msysc_fmtrec fmt;
    msysc_nextfmtchars((i64)0);
    if ((fmtstyle == 0)) {
        msysc_printstr(msysc_strword(a,(byte *)(0)));
    } else {
        msysc_strtofmt(fmtstyle,(i64)-1,&fmt);
        msysc_tostr_u64(a,&fmt);
    };
    msysc_needgap = (i64)1;
}

void msysc_m_print_r64(double x,byte * fmtstyle) {
    byte s[360];
    struct msysc_fmtrec fmt;
    msysc_nextfmtchars((i64)0);
    if ((fmtstyle == 0)) {
        sprintf((i8 *)(s),(i8 *)((byte*)"%f"),x);
        msysc_printstr(s);
    } else {
        msysc_strtofmt(fmtstyle,(i64)-1,&fmt);
        msysc_tostr_r64(x,&fmt);
    };
    msysc_needgap = (i64)1;
}

void msysc_m_print_r32(float x,byte * fmtstyle) {
    msysc_m_print_r64((double)(x),fmtstyle);
}

void msysc_m_print_c8(i64 a,byte * fmtstyle) {
    byte s[40];
    msysc_nextfmtchars((i64)0);
    s[((i64)1)-1] = (u64)(a);
    s[((i64)2)-1] = (u64)0u;
    msysc_printstr(s);
    msysc_needgap = (i64)1;
}

void msysc_m_print_str(byte * s,byte * fmtstyle) {
    struct msysc_fmtrec fmt;
    msysc_nextfmtchars((i64)0);
    if ((fmtstyle == 0)) {
        msysc_printstr(s);
    } else {
        msysc_strtofmt(fmtstyle,(i64)-1,&fmt);
        msysc_tostr_str(s,&fmt);
    };
    msysc_needgap = (i64)1;
}

void msysc_m_print_newline(void) {
    msysc_needgap = (i64)0;
    msysc_nextfmtchars((i64)1);
    msysc_printstr((byte*)"\r\n");
}

void msysc_m_print_nogap(void) {
    msysc_needgap = (i64)0;
}

static void msysc_printstr(byte * s) {
    byte * *  p;
    if ((msysc_outdev==(i64)1)) {
        printf((i8 *)((byte*)"%s"),s);
    }else if ((msysc_outdev==(i64)2)) {
        fprintf(msysc_outchan,(i8 *)((byte*)"%s"),s);
    }else if ((msysc_outdev==(i64)3)) {
        p = (byte * *)(msysc_outchan);
        strcpy((i8 *)((*p)),(i8 *)(s));
        (*p) += (i64)(strlen((i8 *)(s)));
    };
}

void msysc_printstr_n(byte * s,i64 n) {
    byte str[256];
    byte * *  p;
    if ((n==(i64)-1)) {
        n = (i64)(strlen((i8 *)(s)));
    }else if ((n==(i64)0)) {
        return;
    };
    if ((msysc_outdev==(i64)3)) {
        p = (byte * *)(msysc_outchan);
        memcpy((void *)((*p)),(void *)(s),(u64)(n));
        (*p) += n;
        (*(*p)) = (u64)0u;
    }else if ((msysc_outdev==(i64)2)) {
        s = msysc_makezstring(s,n,str);
        fprintf(msysc_outchan,(i8 *)((byte*)"%s"),s);
        msysc_freezstring(s,n);
    }else if ((msysc_outdev==(i64)1)) {
        s = msysc_makezstring(s,n,str);
        printf((i8 *)((byte*)"%s"),s);
        msysc_freezstring(s,n);
    };
}

static byte * msysc_makezstring(byte * s,i64 n,byte * local) {
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

static void msysc_freezstring(byte * t,i64 n) {
    if ((n >= (i64)256)) {
        mlib_pcm_free((void *)(t),(n + (i64)1));
    };
}

static void msysc_printchar(i64 ch) {
    byte * *  p;
    if ((msysc_outdev==(i64)1)) {
        printf("%c",(int)ch);
    }else if ((msysc_outdev==(i64)2)) {
        fprintf(msysc_outchan,"%c",(int)ch);
    }else if ((msysc_outdev==(i64)3)) {
        p = (byte * *)(msysc_outchan);
        (*(*p)) = (u64)(ch);
        (*p) += (i64)1;
        (*(*p)) = (u64)0u;
    };
}

static void msysc_nextfmtchars(i64 lastx) {
    byte c;
    byte *  pstart;
    i64 n;
    if (!(!!(msysc_fmtstr))) {
        if (!!(msysc_needgap)) {
            msysc_printchar((i64)32);
        };
        msysc_needgap = (i64)0;
        return;
    };
    pstart = msysc_fmtstr;
    n = (i64)0;
    L79 :;
    while (!!((i64)1)) {
        c = (u64)((*msysc_fmtstr));
        switch ((i64)(c)) {
        case 35:;
        {
            if (!!(lastx)) {
                goto L82 ;
;
            };
            ++msysc_fmtstr;
            if (!!(n)) {
                msysc_printstr_n(pstart,n);
            };
            return;
        }break;
        case 0:;
        {
            if (!!(n)) {
                msysc_printstr_n(pstart,n);
            } else if (!(!!(lastx))) {
                msysc_printstr_n((byte*)"|",(i64)1);
            };
            return;
        }break;
        case 126:;
        {
            if (!!(n)) {
                msysc_printstr_n(pstart,n);
                n = (i64)0;
            };
            ++msysc_fmtstr;
            c = (u64)((*msysc_fmtstr));
            if (!!((u64)(c))) {
                ++msysc_fmtstr;
                msysc_printchar((i64)(c));
            };
            pstart = msysc_fmtstr;
        }break;
        default: {
            //skip:
L82 :;
;
            ++n;
            ++msysc_fmtstr;
        }
        } //SW
;
L80 :;
    }L81 :;
    ;
}

void msysc_strtofmt(byte * s,i64 slen,struct msysc_fmtrec * fmt) {
    byte c;
    byte wset;
    i64 n;
    byte str[100];
    (*fmt) = msysc_defaultfmt;
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
    L83 :;
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
            n = msysc_fmtparam;
            goto L86 ;
;
        }break;
        default: {
            if ((((u64)(c) >= '0') && ((u64)(c) <= '9'))) {
                n = ((u64)(c) - '0');
                L87 :;
                while (1) {
                    c = (u64)((*s));
                    if (((i64)((*s)) == (i64)0)) {
                        goto L88 ;
                    };
                    if ((((u64)(c) >= '0') && ((u64)(c) <= '9'))) {
                        ++s;
                        n = (((n * (i64)10) + (i64)(c)) - (i64)48);
                    } else {
                        goto L88 ;
                    };
                }L88 :;
                ;
                //gotwidth:
L86 :;
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
L84 :;
    }L85 :;
    ;
}

static i64 msysc_domultichar(byte * p,i64 n,byte * dest,struct msysc_fmtrec * fmt) {
    byte str[20];
    byte *  q;
    i64 nchars;
    i64 av_1;
    q = str;
    nchars = n;
    av_1 = n;
    while (av_1-- > 0) {
L89 :;
        if (((i64)((*p)) == (i64)0)) {
            goto L91 ;
        };
        (*q) = (u64)((*p));
        ++q;
        ++p;
L90 :;
    }L91 :;
    ;
    (*q) = (u64)0u;
    return msysc_expandstr(str,dest,(i64)(strlen((i8 *)(str))),fmt);
}

static i64 msysc_expandstr(byte * s,byte * t,i64 n,struct msysc_fmtrec * fmt) {
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
        L92 :;
        for (i=(i64)1;i<=(w - n);++i) {
L93 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L94 :;
        }L95 :;
        ;
        (*t) = (u64)0u;
    } else if (((u64)((*fmt).justify) == 'R')) {
        if (((((u64)((*fmt).padchar) == '0') && !!((u64)((*fmt).base))) && (((u64)((*s)) == '-') || ((u64)((*s)) == '+')))) {
            (*t) = (u64)((*s));
            ++t;
            av_2 = (w - n);
            while (av_2-- > 0) {
L96 :;
                (*t) = (u64)((*fmt).padchar);
                ++t;
L97 :;
            }L98 :;
            ;
            strncpy((i8 *)(t),(i8 *)((s + (i64)1)),(u64)((n - (i64)1)));
            (*((t + n) - (i64)1)) = (u64)0u;
        } else {
            av_3 = (w - n);
            while (av_3-- > 0) {
L99 :;
                (*t) = (u64)((*fmt).padchar);
                ++t;
L100 :;
            }L101 :;
            ;
            strncpy((i8 *)(t),(i8 *)(s),(u64)(n));
            (*(t + n)) = (u64)0u;
        };
    } else {
        m = (((w - n) + (i64)1) / (i64)2);
        av_4 = m;
        while (av_4-- > 0) {
L102 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L103 :;
        }L104 :;
        ;
        strncpy((i8 *)(t),(i8 *)(s),(u64)(n));
        t += n;
        av_5 = ((w - n) - m);
        while (av_5-- > 0) {
L105 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L106 :;
        }L107 :;
        ;
        (*t) = (u64)0u;
    };
    return w;
}

static u64 msysc_xdivrem(u64 a,u64 b,u64 * remainder) {
    u64 q;
    mlib_abortprogram((byte*)"XDIVREM");
    return q;
}

static i64 msysc_u64tostr(u64 aa,byte * s,u64 base,i64 sep) {
    byte t[360];
    i64 i;
    i64 j;
    i64 k;
    i64 g;
    byte *  s0;
    i = (i64)0;
    k = (i64)0;
    g = (((i64)(base) == (i64)10)?(i64)3:(i64)4);
    L108 :;
    do {
        t[(++i)] = (u64)(msysc_digits[((i64)((aa % base)))]);
        aa = (aa / base);
        ++k;
        if (((!!(sep) && ((i64)(aa) != (i64)0)) && (k == g))) {
            t[(++i)] = (u64)(sep);
            k = (i64)0;
        };
L109 :;
    } while (!((i64)(aa) == (i64)0));L110 :;
    ;
    j = i;
    s0 = s;
    L111 :;
    while (!!(i)) {
        (*s) = (u64)(t[(i--)]);
        ++s;
L112 :;
    }L113 :;
    ;
    (*s) = (u64)0u;
    return j;
}

static i64 msysc_i64tostrfmt(i64 aa,byte * s,struct msysc_fmtrec * fmt) {
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
        n = (msysc_i64mintostr(&str[((i64)1)],(i64)((*fmt).base),(i64)((*fmt).sepchar)) + (i64)1);
    } else {
        if (((!(!!(usigned)) && (aa < (i64)0)) || !!((u64)((*fmt).plus)))) {
            if ((aa < (i64)0)) {
                aa = -(aa);
                str[((i64)0)] = '-';
            } else {
                str[((i64)0)] = '+';
            };
            n = (msysc_u64tostr((u64)(aa),&str[((i64)1)],(u64)((*fmt).base),(i64)((*fmt).sepchar)) + (i64)1);
        } else {
            n = msysc_u64tostr((u64)(aa),str,(u64)((*fmt).base),(i64)((*fmt).sepchar));
        };
    };
    if (!!((u64)((*fmt).suffix))) {
        str[(n)] = (u64)((*fmt).suffix);
        str[(++n)] = (u64)0u;
    };
    if (((((i64)((u64)((*fmt).base)) > (i64)10) || !!((u64)((*fmt).suffix))) && ((u64)((*fmt).lettercase) == 'a'))) {
        msysc_convlcstring(str);
    };
    return msysc_expandstr(str,s,n,fmt);
}

static i64 msysc_u64tostrfmt(i64 aa,byte * s,struct msysc_fmtrec * fmt) {
    byte str[360];
    i64 n;
    n = msysc_u64tostr((u64)(aa),str,(u64)((*fmt).base),(i64)((*fmt).sepchar));
    if (!!((u64)((*fmt).suffix))) {
        str[(n)] = (u64)((*fmt).suffix);
        str[(++n)] = (u64)0u;
    };
    if ((((i64)((u64)((*fmt).base)) > (i64)10) || (!!((u64)((*fmt).suffix)) && ((u64)((*fmt).lettercase) == 'a')))) {
        msysc_convlcstring(str);
    };
    return msysc_expandstr(str,s,n,fmt);
}

static i64 msysc_i64mintostr(byte * s,i64 base,i64 sep) {
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
    L114 :;
    while (!!(i)) {
        --s;
        (*s) = (u64)(t[((i-- - (i64)1))]);
        if (((!!(sep) && !!(i)) && (++k == g))) {
            --s;
            (*s) = (u64)(sep);
            k = (i64)0;
        };
L115 :;
    }L116 :;
    ;
    return (i64)(strlen((i8 *)(s)));
}

static i64 msysc_strtostrfmt(byte * s,byte * t,i64 n,struct msysc_fmtrec * fmt) {
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
            msysc_convlcstring(u);
        }break;
        case 65:;
        {
            msysc_convucstring(u);
        }break;
        default: {
        }
        } //SW
;
        s = u;
    };
    w = (i64)((*fmt).minwidth);
    if ((w > n)) {
        n = msysc_expandstr(s,t,n,fmt);
    } else {
        memcpy((void *)(t),(void *)(s),(u64)(n));
    };
    if (!!(nheap)) {
        mlib_pcm_free((void *)(u),nheap);
    };
    return n;
}

static void msysc_tostr_i64(i64 a,struct msysc_fmtrec * fmt) {
    byte str[360];
    i64 n;
    if (((i64)((*fmt).charmode)==(i64)0)) {
        n = msysc_i64tostrfmt(a,str,fmt);
    }else if (((i64)((*fmt).charmode)==(i64)68) || ((i64)((*fmt).charmode)==(i64)100)) {
        n = msysc_domultichar((byte *)(&a),(i64)8,str,fmt);
    } else {
        msysc_printchar(a);
        return;
    };
    msysc_printstr_n(str,n);
}

static void msysc_tostr_u64(u64 a,struct msysc_fmtrec * fmt) {
    byte str[360];
    i64 n;
    if (((i64)((*fmt).charmode)==(i64)68) || ((i64)((*fmt).charmode)==(i64)100)) {
        n = msysc_domultichar((byte *)(&a),(i64)8,str,fmt);
    }else if (((i64)((*fmt).charmode)==(i64)67) || ((i64)((*fmt).charmode)==(i64)99)) {
        msysc_printchar((i64)(a));
        return;
    } else {
        n = msysc_u64tostrfmt((i64)(a),str,fmt);
    };
    msysc_printstr_n(str,n);
}

static void msysc_tostr_r64(double x,struct msysc_fmtrec * fmt) {
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
        n = msysc_expandstr(str,str2,n,fmt);
        strcpy((i8 *)(str),(i8 *)(str2));
    };
    msysc_printstr_n(str,n);
}

static void msysc_tostr_str(byte * s,struct msysc_fmtrec * fmt) {
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
        n = msysc_strtostrfmt(s,t,oldlen,fmt);
        msysc_printstr_n(t,n);
        mlib_pcm_free((void *)(t),(newlen + (i64)1));
    } else {
        msysc_printstr_n(s,oldlen);
    };
}

static struct msysc_fmtrec * msysc_getfmt(byte * fmtstyle) {
    static struct msysc_fmtrec fmt;
    if (!!(fmtstyle)) {
        msysc_strtofmt(fmtstyle,(i64)-1,&fmt);
        return &fmt;
    } else {
        return &msysc_defaultfmt;
    };
}

byte * msysc_strint(i64 a,byte * fmtstyle) {
    static byte str[100];
    struct msysc_fmtrec *  fmt;
    msysc_m_print_startstr(str);
    msysc_tostr_i64(a,(fmt = msysc_getfmt(fmtstyle)));
    msysc_m_print_end();
    return msysc_getstr(str,fmt);
}

void msysc_getstrint(i64 a,byte * dest) {
    msysc_m_print_startstr(dest);
    msysc_tostr_i64(a,msysc_getfmt((byte *)(0)));
    msysc_m_print_end();
}

byte * msysc_strword(u64 a,byte * fmtstyle) {
    static byte str[100];
    struct msysc_fmtrec *  fmt;
    msysc_m_print_startstr(str);
    msysc_tostr_u64(a,(fmt = msysc_getfmt(fmtstyle)));
    msysc_m_print_end();
    return msysc_getstr(str,fmt);
}

byte * msysc_strreal(double a,byte * fmtstyle) {
    static byte str[320];
    struct msysc_fmtrec *  fmt;
    msysc_m_print_startstr(str);
    msysc_tostr_r64(a,(fmt = msysc_getfmt(fmtstyle)));
    msysc_m_print_end();
    return msysc_getstr(str,fmt);
}

static byte * msysc_getstr(byte * s,struct msysc_fmtrec * fmt) {
    if (!!((u64)((*fmt).heapmode))) {
        return mlib_pcm_copyheapstring(s);
    } else {
        return s;
    };
}

static void msysc_initreadbuffer(void) {
    if (!!(msysc_rd_buffer)) {
        return;
    };
    msysc_rd_buffer = (byte *)(mlib_pcm_alloc((i64)16384));
    (*msysc_rd_buffer) = (u64)0u;
    msysc_rd_pos = (msysc_rd_lastpos = msysc_rd_buffer);
}

void msysc_m_read_conline(void) {
    msysc_initreadbuffer();
    mlib_readlinen(0,msysc_rd_buffer,(i64)16384);
    msysc_rd_length = (i64)(strlen((i8 *)(msysc_rd_buffer)));
    msysc_rd_pos = msysc_rd_buffer;
    msysc_rd_lastpos = (byte *)(0);
}

void msysc_m_read_fileline(void * f) {
    msysc_initreadbuffer();
    mlib_readlinen(f,msysc_rd_buffer,(i64)16384);
    msysc_rd_length = (i64)(strlen((i8 *)(msysc_rd_buffer)));
    msysc_rd_pos = msysc_rd_buffer;
    msysc_rd_lastpos = (byte *)(0);
}

void msysc_m_read_strline(byte * s) {
    i64 n;
    msysc_initreadbuffer();
    n = (i64)(strlen((i8 *)(s)));
    if ((n < (i64)16384)) {
        strcpy((i8 *)(msysc_rd_buffer),(i8 *)(s));
    } else {
        memcpy((void *)(msysc_rd_buffer),(void *)(s),(u64)((i64)16383));
        (*((msysc_rd_buffer + (i64)16384) - (i64)1)) = (u64)0u;
    };
    msysc_rd_length = n;
    msysc_rd_pos = msysc_rd_buffer;
    msysc_rd_lastpos = (byte *)(0);
}

static byte * msysc_readitem(i64 * itemlength) {
    byte *  p;
    byte *  s;
    byte *  itemstr;
    byte quotechar;
    byte c;
    if (!(!!(msysc_rd_buffer))) {
        msysc_initreadbuffer();
    };
    s = msysc_rd_pos;
    L117 :;
    while ((((u64)((*s)) == ' ') || ((i64)((*s)) == (i64)9))) {
        ++s;
L118 :;
    }L119 :;
    ;
    itemstr = s;
    msysc_rd_lastpos = (msysc_rd_pos = s);
    if (((i64)((*s)) == (i64)0)) {
        msysc_termchar = (i64)0;
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
    L120 :;
    while (!!((u64)((*s)))) {
        c = (u64)((*s++));
        switch ((i64)(c)) {
        case 32:;
        case 9:;
        case 44:;
        case 61:;
        {
            if ((!!((u64)(quotechar)) || (p == s))) {
                goto L123 ;
;
            };
            msysc_termchar = (i64)(c);
            goto L122 ;
        }break;
        default: {
            //normalchar:
L123 :;
;
            if (((u64)(c) == (u64)(quotechar))) {
                if (((u64)((*s)) == (u64)(quotechar))) {
                    (*p) = (u64)(c);
                    ++s;
                    ++p;
                } else {
                    msysc_termchar = (i64)((*s));
                    if (((msysc_termchar == (i64)44) || (msysc_termchar == (i64)61))) {
                        ++s;
                        msysc_termchar = (i64)((*s));
                    };
                    goto L122 ;
                };
            } else {
                (*p) = (u64)(c);
                ++p;
            };
        }
        } //SW
;
L121 :;
    }L122 :;
    ;
    if (((i64)((*s)) == (i64)0)) {
        msysc_termchar = (i64)0;
    };
    (*itemlength) = (p - itemstr);
    msysc_rd_pos = s;
    return itemstr;
}

static i64 msysc_strtoint(byte * s,i64 length,i64 base) {
    byte signd;
    u64 aa;
    byte c;
    byte d;
    msysc_itemerror = (i64)0;
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
    L124 :;
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
            goto L125 ;
        }break;
        default: {
            msysc_itemerror = (i64)1;
            goto L126 ;
        }
        } //SW
;
        if (((i64)(d) >= base)) {
            msysc_itemerror = (i64)1;
            goto L126 ;
        };
        aa = (u64)((((i64)(aa) * base) + (i64)(d)));
L125 :;
    }L126 :;
    ;
    if (!!((u64)(signd))) {
        return (i64)(-(aa));
    } else {
        return (i64)(aa);
    };
}

i64 msysc_m_read_i64(i64 fmt) {
    byte *  s;
    i64 length;
    if ((fmt==(i64)67) || (fmt==(i64)99)) {
        msysc_rd_lastpos = msysc_rd_pos;
        if (!!((u64)((*msysc_rd_pos)))) {
            return (i64)((*msysc_rd_pos++));
        } else {
            return (i64)0;
        };
    }else if ((fmt==(i64)84) || (fmt==(i64)116)) {
        return msysc_termchar;
    }else if ((fmt==(i64)69) || (fmt==(i64)101)) {
        return msysc_itemerror;
    };
    s = msysc_readitem(&length);
    if ((fmt==(i64)0) || (fmt==(i64)73) || (fmt==(i64)105)) {
        return msysc_strtoint(s,length,(i64)10);
    }else if ((fmt==(i64)66) || (fmt==(i64)98)) {
        return msysc_strtoint(s,length,(i64)2);
    }else if ((fmt==(i64)72) || (fmt==(i64)104)) {
        return msysc_strtoint(s,length,(i64)16);
    };
    return (i64)0;
}

double msysc_m_read_r64(i64 fmt) {
    byte str[512];
    byte *  s;
    i64 length;
    i32 numlength;
    double x;
    s = msysc_readitem(&length);
    if (((length == (i64)0) || (length >= (i64)512))) {
        return (double)0.;
    };
    memcpy((void *)(str),(void *)(s),(u64)(length));
    str[((length + (i64)1))-1] = (u64)0u;
    msysc_itemerror = (i64)0;
    if ((((i64)(sscanf((i8 *)(str),(i8 *)((byte*)"%lf%n"),&x,&numlength)) == (i64)0) || ((i64)(numlength) != length))) {
        x = (double)0.;
        msysc_itemerror = (i64)1;
    };
    return x;
}

void msysc_m_read_str(byte * dest,i64 destlen,i64 fmt) {
    byte *  s;
    i64 length;
    msysc_itemerror = (i64)0;
    if (((fmt == (i64)76) || (fmt == (i64)108))) {
        s = msysc_rd_pos;
        length = ((msysc_rd_buffer + msysc_rd_length) - msysc_rd_pos);
    } else {
        s = msysc_readitem(&length);
        if (((fmt == (i64)78) || (fmt == (i64)110))) {
            msysc_iconvlcn(s,length);
        };
    };
    if ((destlen > (i64)0)) {
        if ((length >= destlen)) {
            length = (destlen - (i64)1);
            msysc_itemerror = (i64)1;
        };
    };
    memcpy((void *)(dest),(void *)(s),(u64)(length));
    (*(dest + length)) = (u64)0u;
}

void msysc_readstr(byte * dest,i64 fmt,i64 destlen) {
    msysc_m_read_str(dest,destlen,fmt);
}

void msysc_rereadln(void) {
    msysc_rd_pos = msysc_rd_buffer;
    msysc_rd_lastpos = msysc_rd_pos;
}

void msysc_reread(void) {
    msysc_rd_pos = msysc_rd_lastpos;
}

i64 msysc_valint(byte * s,i64 fmt) {
    byte *  old_pos;
    byte *  old_lastpos;
    i64 aa;
    msysc_initreadbuffer();
    old_pos = msysc_rd_pos;
    old_lastpos = msysc_rd_lastpos;
    msysc_rd_pos = s;
    aa = msysc_m_read_i64(fmt);
    msysc_rd_pos = old_pos;
    msysc_rd_lastpos = old_lastpos;
    return aa;
}

double msysc_valreal(byte * s) {
    byte *  old_pos;
    byte *  old_lastpos;
    double x;
    msysc_initreadbuffer();
    old_pos = msysc_rd_pos;
    old_lastpos = msysc_rd_lastpos;
    msysc_rd_pos = s;
    x = msysc_m_read_r64((i64)0);
    msysc_rd_pos = old_pos;
    msysc_rd_lastpos = old_lastpos;
    return x;
}

static void msysc_iconvlcn(byte * s,i64 n) {
    i64 av_1;
    av_1 = n;
    while (av_1-- > 0) {
L127 :;
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L128 :;
    }L129 :;
    ;
}

static void msysc_iconvucn(byte * s,i64 n) {
    i64 av_1;
    av_1 = n;
    while (av_1-- > 0) {
L130 :;
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L131 :;
    }L132 :;
    ;
}

static void msysc_convlcstring(byte * s) {
    L133 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L134 :;
    }L135 :;
    ;
}

static void msysc_convucstring(byte * s) {
    L136 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L137 :;
    }L138 :;
    ;
}

i64 msysc_m_power_i64(i64 n,i64 a) {
    if ((n < (i64)0)) {
        return (i64)0;
    } else if ((n == (i64)0)) {
        return (i64)1;
    } else if ((n == (i64)1)) {
        return a;
    } else if (((n & (i64)1) == (i64)0)) {
        return msysc_m_power_i64((n / (i64)2),(a * a));
    } else {
        return (msysc_m_power_i64(((n - (i64)1) / (i64)2),(a * a)) * a);
    };
}

void msysc_m_intoverflow(void) {
    mlib_abortprogram((byte*)"Integer overflow detected");
}

void msysc_m_dotindex(u64 i,u64 a) {
    mlib_abortprogram((byte*)"DOT INDEX");
}

void msysc_m_dotslice(u64 j,u64 i,u64 a) {
    mlib_abortprogram((byte*)"DOT SLICE");
}

void msysc_m_popdotindex(u64 i,u64 * p,u64 x) {
    mlib_abortprogram((byte*)"POP DOT INDEX");
}

void msysc_m_popdotslice(u64 j,u64 i,u64 * p,u64 x) {
    mlib_abortprogram((byte*)"POP DOT SLICE");
}

i64 msysc_m_imin(i64 a,i64 b) {
    return (a<b?a:b);
}

i64 msysc_m_imax(i64 a,i64 b) {
    return (a>b?a:b);
}

double msysc_m_sign(double x) {
    if ((x > (double)0.)) {
        return (double)1.;
    } else if ((x < (double)0.)) {
        return (double)-1.;
    } else {
        return (double)0.;
    };
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
    L139 :;
    for (i=(i64)1;i<=(i64)2048;++i) {
L140 :;
        j = (i64)1;
        k = (i64)16;
        L143 :;
        while ((i > k)) {
            k = (k << (i64)1);
            ++j;
L144 :;
        }L145 :;
        ;
        mlib_sizeindextable[(i)] = (u64)(j);
L141 :;
    }L142 :;
    ;
    mlib_allocupper[((i64)1)] = (u64)((i64)16);
    size = (i64)16;
    L146 :;
    for (i=(i64)2;i<=(i64)27;++i) {
L147 :;
        size *= (i64)2;
        mlib_allocupper[(i)] = (u64)(size);
        if ((size >= (i64)33554432)) {
            k = i;
            goto L149 ;
        };
L148 :;
    }L149 :;
    ;
    L150 :;
    for (i=(k + (i64)1);i<=(i64)300;++i) {
L151 :;
        size += (i64)33554432;
        if ((size < (i64)8589934592)) {
            mlib_allocupper[(i)] = (u64)(size);
            mlib_maxmemory = (u64)(size);
        } else {
            mlib_maxalloccode = (i - (i64)1);
            goto L153 ;
        };
L152 :;
    }L153 :;
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
        L154 :;
        while ((n > m)) {
            m <<= (i64)1;
L155 :;
        }L156 :;
        ;
        return m;
    };
}

void mlib_pcm_printfreelist(i64 size,u64 * p) {
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"Size: ",NULL);
    msysc_m_print_i64(size,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    L157 :;
    while (!!(p)) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)" ",NULL);
        msysc_m_print_nogap();
        msysc_m_print_ptr(p,(byte*)"h");
        msysc_m_print_end();
        ;
        p = (u64 *)((i64)((*p)));
L158 :;
    }L159 :;
    ;
    puts((i8 *)((byte*)""));
}

void mlib_pcm_diags(byte * caption) {
    i64 m;
    i64 i;
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"HEAP FREELISTS:",NULL);
    msysc_m_print_str(caption,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    m = (i64)16;
    L160 :;
    for (i=(i64)1;i<=(i64)8;++i) {
L161 :;
        mlib_pcm_printfreelist(m,mlib_freelist[(i)]);
        m <<= (i64)1;
L162 :;
    }L163 :;
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

static void mlib_addtomemalloc(i32 * ptr,i64 size) {
    i64 i;
    L164 :;
    for (i=(i64)1;i<=(i64)500000;++i) {
L165 :;
        if ((mlib_memalloctable[(i)-1] == ptr)) {
            msysc_m_print_startcon();
            msysc_m_print_str((byte*)"ALLOC ERROR:",NULL);
            msysc_m_print_ptr(ptr,NULL);
            msysc_m_print_str((byte*)"ALREADY ALLOCATED\n\n\n",NULL);
            msysc_m_print_newline();
            msysc_m_print_end();
            ;
            msysc_m_print_startcon();
            msysc_m_print_newline();
            msysc_m_print_end();
            ;
            msysc_m_print_startcon();
            msysc_m_print_newline();
            msysc_m_print_end();
            ;
            exit((i64)2);
        };
        if ((mlib_memalloctable[(i)-1] == 0)) {
            mlib_memalloctable[(i)-1] = ptr;
            mlib_memallocsize[(i)-1] = size;
            return;
        };
L166 :;
    }L167 :;
    ;
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"MEMALLOCTABLE FULL\n\n\n\n",NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    oslinux_os_getch();
    exit((i64)3);
}

static void mlib_removefrommemalloc(i32 * ptr,i64 size) {
    i64 i;
    L168 :;
    for (i=(i64)1;i<=(i64)500000;++i) {
L169 :;
        if ((mlib_memalloctable[(i)-1] == ptr)) {
            if (((i64)(mlib_memallocsize[(i)-1]) != size)) {
                msysc_m_print_startcon();
                msysc_m_print_str((byte*)"REMOVE:FOUND",NULL);
                msysc_m_print_ptr(ptr,NULL);
                msysc_m_print_str((byte*)"IN MEMALLOCTABLE, FREESIZE=",NULL);
                msysc_m_print_i64(size,NULL);
                msysc_m_print_str((byte*)", BUT STORED AS BLOCK SIZE:",NULL);
                msysc_m_print_i64(mlib_memallocsize[(i)-1],NULL);
                msysc_m_print_newline();
                msysc_m_print_end();
                ;
                msysc_m_print_startcon();
                msysc_m_print_newline();
                msysc_m_print_end();
                ;
                msysc_m_print_startcon();
                msysc_m_print_newline();
                msysc_m_print_end();
                ;
                mlib_abortprogram((byte*)"MEMSIZE");
            };
            mlib_memalloctable[(i)-1] = (i32 *)(0);
            return;
        };
L170 :;
    }L171 :;
    ;
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"CAN'T FIND",NULL);
    msysc_m_print_ptr(ptr,NULL);
    msysc_m_print_str((byte*)"IN MEMALLOCTABLE",NULL);
    msysc_m_print_i64(size,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    msysc_m_print_startcon();
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    msysc_m_print_startcon();
    msysc_m_print_newline();
    msysc_m_print_end();
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
    msysc_m_print_startcon();
    msysc_m_print_i64(n,NULL);
    msysc_m_print_i64(mlib_memtotal,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    mlib_abortprogram((byte*)"Alloc mem failure");
    return 0;
}

void * mlib_reallocmem(void * p,i64 n) {
    p = realloc(p,(u64)(n));
    if (!!(p)) {
        return p;
    };
    msysc_m_print_startcon();
    msysc_m_print_i64(n,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    mlib_abortprogram((byte*)"Realloc mem failure");
    return 0;
}

void mlib_abortprogram(byte * s) {
    msysc_m_print_startcon();
    msysc_m_print_str(s,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"ABORTING: Press key...",NULL);
    msysc_m_print_end();
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
        L172 :;
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
        }L173 :;
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
    L174 :;
    while (((p >= buffer) && (((i64)((*p)) == (i64)13) || ((i64)((*p)) == (i64)10)))) {
        if ((((i64)((*p)) == (i64)13) || ((i64)((*p)) == (i64)10))) {
            crseen = (u64)((i64)1);
        };
        (*p--) = (u64)0u;
L175 :;
    }L176 :;
    ;
    if ((!(!!((u64)(crseen))) && ((n + (i64)4) > size))) {
        msysc_m_print_startcon();
        msysc_m_print_i64(size,NULL);
        msysc_m_print_i64(n,NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        mlib_abortprogram((byte*)"line too long");
    };
}

void mlib_iconvlcn(byte * s,i64 n) {
    i64 av_1;
    av_1 = n;
    while (av_1-- > 0) {
L177 :;
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L178 :;
    }L179 :;
    ;
}

void mlib_iconvucn(byte * s,i64 n) {
    i64 av_1;
    av_1 = n;
    while (av_1-- > 0) {
L180 :;
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L181 :;
    }L182 :;
    ;
}

void mlib_convlcstring(byte * s) {
    L183 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L184 :;
    }L185 :;
    ;
}

void mlib_convucstring(byte * s) {
    L186 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L187 :;
    }L188 :;
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
    L189 :;
    while ((u >= t)) {
        if (((u64)((*u)) == '.')) {
            if (((i64)((*(u + (i64)1))) == (i64)0)) {
                return (!!(period)?(byte*)".":(byte*)"");
            };
            return (u + (i64)1);
        };
        --u;
L190 :;
    }L191 :;
    ;
    return (byte*)"";
}

byte * mlib_extractpath(byte * s) {
    static byte str[260];
    byte *  t;
    i64 n;
    t = ((s + (i64)(strlen((i8 *)(s)))) - (i64)1);
    L192 :;
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
L193 :;
    }L194 :;
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
    L195 :;
    for (i=(i64)2;i<=(i64)2;++i) {
L196 :;
        p = mlib_freelist[(i)];
        L199 :;
        while (!!(p)) {
            aa = (i64)(p);
            if (((aa > (i64)4294967295) || (aa < (i64)100))) {
                msysc_m_print_startcon();
                msysc_m_print_str(s,NULL);
                msysc_m_print_str((byte*)"FREE LIST ERROR",NULL);
                msysc_m_print_i64(i,NULL);
                msysc_m_print_ptr(p,NULL);
                msysc_m_print_ptr(q,NULL);
                msysc_m_print_newline();
                msysc_m_print_end();
                ;
            };
            q = p;
            p = (u64 *)((i64)((*p)));
L200 :;
        }L201 :;
        ;
L197 :;
    }L198 :;
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
    mlib_strbuffer_add(dest,msysc_strint(a,(byte *)(0)),(i64)-1);
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
        L202 :;
        for (i=(i64)1;i<=n;++i) {
L203 :;
            str[((slen + i))-1] = (u64)(padch);
L204 :;
        }L205 :;
        ;
        str[(((slen + n) + (i64)1))-1] = (u64)0u;
    };
    mlib_gs_str(dest,str);
}

void mlib_gs_leftint(struct mlib_strbuffer * dest,i64 a,i64 w,i64 padch) {
    mlib_gs_leftstr(dest,msysc_strint(a,(byte *)(0)),w,padch);
}

void mlib_gs_padto(struct mlib_strbuffer * dest,i64 col,i64 ch) {
    i64 n;
    byte str[2560];
    i64 i;
    n = (col - (i64)((*dest).length));
    if ((n <= (i64)0)) {
        return;
    };
    L206 :;
    for (i=(i64)1;i<=n;++i) {
L207 :;
        str[(i)-1] = (u64)(ch);
L208 :;
    }L209 :;
    ;
    str[((n + (i64)1))-1] = (u64)0u;
    mlib_gs_str(dest,str);
}

void mlib_gs_println(struct mlib_strbuffer * dest,void * f) {
    (*((*dest).strptr + (i64)((*dest).length))) = (u64)0u;
    if ((f == 0)) {
        msysc_m_print_startcon();
        msysc_m_print_str((*dest).strptr,NULL);
        msysc_m_print_nogap();
        msysc_m_print_str((byte*)"\r",NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
    } else {
        msysc_m_print_startfile(f);
        msysc_m_print_str((*dest).strptr,NULL);
        msysc_m_print_nogap();
        msysc_m_print_str((byte*)"\r",NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
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
L210 :;
;
    (*value) = (byte *)(0);
    (*name) = (byte *)(0);
    if (!!(infile)) {
        if ((mlib_readnextfileitem(&fileptr,&item) == (i64)0)) {
            free((void *)(filestart));
            infile = (i64)0;
            goto L210 ;
;
        };
    } else {
        if (((*paramno) > msysc_nsysparams)) {
            return (i64)0;
        };
        item = msysc_sysparams[((*paramno))-1];
        ++(*paramno);
        length = (i64)(strlen((i8 *)(item)));
        if (((u64)((*item)) == '@')) {
            filestart = (fileptr = (byte *)(mlib_readfile((item + (i64)1))));
            if ((filestart == 0)) {
                msysc_m_print_startcon();
                msysc_m_print_str((byte*)"Can't open",NULL);
                msysc_m_print_str(item,NULL);
                msysc_m_print_newline();
                msysc_m_print_end();
                ;
                exit((i64)7);
            };
            infile = (i64)1;
            goto L210 ;
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
L211 :;
;
    L212 :;
    while (1) {
        if (((i64)((*p))==(i64)32) || ((i64)((*p))==(i64)9) || ((i64)((*p))==(i64)13) || ((i64)((*p))==(i64)10)) {
            ++p;
        }else if (((i64)((*p))==(i64)26) || ((i64)((*p))==(i64)0)) {
            return (i64)0;
        } else {
            goto L213 ;
        };
    }L213 :;
    ;
    if (((i64)((*p))==(i64)33) || ((i64)((*p))==(i64)35)) {
        ++p;
        L214 :;
        if (((i64)((*p++))==(i64)10)) {
            goto L211 ;
;
        }else if (((i64)((*p++))==(i64)26) || ((i64)((*p++))==(i64)0)) {
            (*fileptr) = (p - (i64)1);
            return (i64)0;
        } else {
        }goto L214 ;
L215 :;
        ;
    };
    if (((i64)((*p))==(i64)34)) {
        pstart = ++p;
        L216 :;
        while (1) {
            if (((i64)((*p))==(i64)0) || ((i64)((*p))==(i64)26)) {
                msysc_m_print_startcon();
                msysc_m_print_str((byte*)"Unexpected EOF in @file",NULL);
                msysc_m_print_newline();
                msysc_m_print_end();
                ;
                exit((i64)8);
            }else if (((i64)((*p))==(i64)34)) {
                pend = p++;
                if (((u64)((*p)) == ',')) {
                    ++p;
                };
                goto L217 ;
            };
            ++p;
        }L217 :;
        ;
    } else {
        pstart = p;
        L218 :;
        while (1) {
            if (((i64)((*p))==(i64)0) || ((i64)((*p))==(i64)26)) {
                pend = p;
                goto L219 ;
            }else if (((i64)((*p))==(i64)32) || ((i64)((*p))==(i64)9) || ((i64)((*p))==(i64)44) || ((i64)((*p))==(i64)13) || ((i64)((*p))==(i64)10)) {
                pend = p++;
                goto L219 ;
            };
            ++p;
        }L219 :;
        ;
    };
    n = (pend - pstart);
    if ((n >= (i64)256)) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"@file item too long",NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
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
L220 :;
        strcat((i8 *)(s),(i8 *)(padchar));
L221 :;
    }L222 :;
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

i64 mlib_mrandom(void) {
    u64 x;
    u64 y;
    x = (u64)(mlib_seed[((i64)1)-1]);
    y = (u64)(mlib_seed[((i64)2)-1]);
    mlib_seed[((i64)1)-1] = (i64)(y);
    x ^= (x << (i64)23);
    mlib_seed[((i64)2)-1] = (i64)((((x ^ y) ^ (x >> (i64)17)) ^ (y >> (i64)26)));
    return (mlib_seed[((i64)2)-1] + (i64)(y));
}

i64 mlib_mrandomp(void) {
    return (mlib_mrandom() & (i64)((u64)9223372036854775807u));
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
    return ((double)(mlib_mrandomp()) / (double)9223372036854775800.);
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
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"Attempting to open",NULL);
    msysc_m_print_ptr(&exefile,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    packexeptr = mlib_readfile(&exefile[((i64)1)-1]);
    if (!(!!(packexeptr))) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"Can't open",NULL);
        msysc_m_print_ptr(&exefile,NULL);
        msysc_m_print_ptr(&packexeptr,NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        exit(0);
    };
    packexesize = mlib_rfsize;
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"File read OK. Size",NULL);
    msysc_m_print_i64(packexesize,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
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
    memcpy(&new,&old,60);
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

i64 mm_support_loadsourcefile(byte * filespec) {
    byte *  s;
    byte *  shortfile;
    if ((mm_decls_nsourcefiles > (i64)250)) {
        mm_support_loaderror((byte*)"Too many source files",(byte*)"",(byte*)"");
    };
    shortfile = mlib_extractfile(filespec);
    ++mm_decls_nsourcefiles;
    mm_decls_sourcefilepaths[(mm_decls_nsourcefiles)] = mlib_pcm_copyheapstring(filespec);
    mm_decls_sourcefilenames[(mm_decls_nsourcefiles)] = mlib_pcm_copyheapstring(shortfile);
    s = (byte *)(mlib_readfile(filespec));
    if (!(!!(s))) {
        mm_support_loaderror((byte*)"LSF can't load ",filespec,(byte*)"");
    };
    mm_decls_sourcefiletext[(mm_decls_nsourcefiles)] = s;
    if (!!(mm_decls_fwritema)) {
        mm_decls_mafiletext[(mm_decls_nsourcefiles)] = mlib_pcm_copyheapstring(s);
    };
    mm_decls_sourcefilesizes[(mm_decls_nsourcefiles)] = mlib_rfsize;
    (*(s + mlib_rfsize)) = (u64)0u;
    return mm_decls_nsourcefiles;
}

i64 mm_support_loadbuiltin(byte * shortfile,byte * text) {
    byte str[128];
    if ((mm_decls_nsourcefiles > (i64)250)) {
        mm_support_loaderror((byte*)"Too many source files",(byte*)"",(byte*)"");
    };
    ++mm_decls_nsourcefiles;
    msysc_m_print_startstr(str);
    msysc_m_print_setfmt((byte*)"<Built-in: #>");
    msysc_m_print_str(shortfile,NULL);
    msysc_m_print_end();
    ;
    mm_decls_sourcefilepaths[(mm_decls_nsourcefiles)] = mlib_pcm_copyheapstring(str);
    mm_decls_sourcefilenames[(mm_decls_nsourcefiles)] = mlib_pcm_copyheapstring(shortfile);
    mm_decls_sourcefiletext[(mm_decls_nsourcefiles)] = mlib_pcm_copyheapstring(text);
    if (!!(mm_decls_fwritema)) {
        mm_decls_mafiletext[(mm_decls_nsourcefiles)] = mlib_pcm_copyheapstring(text);
    };
    mm_decls_sourcefilesizes[(mm_decls_nsourcefiles)] = (i64)(strlen((i8 *)(text)));
    return mm_decls_nsourcefiles;
}

i64 mm_support_loadbundledfile(byte * filespec) {
    i64 fileno;
    byte *  file;
    i64 i;
    file = mlib_extractfile(filespec);
    L223 :;
    for (i=(i64)1;i<=mm_decls_nmafiles;++i) {
L224 :;
        if (!!(mlib_eqstring(file,mm_decls_mafilenames[(i)]))) {
            fileno = (i64)(mm_decls_mafilefileno[(i)]);
            if (!(!!(fileno))) {
                fileno = ++mm_decls_nsourcefiles;
                mm_decls_mafilefileno[(i)] = (u64)(fileno);
                mm_decls_sourcefilepaths[(mm_decls_nsourcefiles)] = mm_decls_mafilenames[(i)];
                mm_decls_sourcefilenames[(mm_decls_nsourcefiles)] = mm_decls_mafilenames[(i)];
                mm_decls_sourcefiletext[(mm_decls_nsourcefiles)] = mm_decls_mafiletext[(i)];
                mm_decls_sourcefilesizes[(mm_decls_nsourcefiles)] = mm_decls_mafilesizes[(i)];
                if (!!((u64)(mm_decls_mafilemult[(i)]))) {
                    mm_decls_sourcefiletext[(mm_decls_nsourcefiles)] = mlib_pcm_copyheapstring(mm_decls_mafiletext[(i)]);
                };
            };
            return fileno;
        };
L225 :;
    }L226 :;
    ;
    mm_support_loaderror((byte*)"Can't find bundled file: # #",filespec,(byte*)"");
    return (i64)0;
}

void mm_support_mcerror(byte * mess) {
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"MC Error:",NULL);
    msysc_m_print_str(mess,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    exit((i64)1);
}

void mm_support_serror_gen(byte * mess) {
    if ((!!(mm_decls_currproc) && ((i64)((u64)((*mm_decls_currproc).nameid)) == (i64)5))) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"In function",NULL);
        msysc_m_print_str((*mm_decls_currproc).name,NULL);
        msysc_m_print_nogap();
        msysc_m_print_str((byte*)" ",NULL);
        msysc_m_print_end();
        ;
    };
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"On line",NULL);
    msysc_m_print_i64(((i64)(mm_decls_lx.lineno) & (i64)16777215),NULL);
    msysc_m_print_str((byte*)"in file",NULL);
    msysc_m_print_str(mm_decls_sourcefilepaths[((i64)(mm_decls_lx.fileno))],NULL);
    msysc_m_print_str(mm_decls_sourcefilenames[((i64)(mm_decls_lx.fileno))],NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    msysc_m_print_startcon();
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"**** Syntax Error:",NULL);
    msysc_m_print_str(mess,NULL);
    msysc_m_print_str((byte*)"****",NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    mm_support_stopcompiler(mm_decls_sourcefilepaths[((i64)(mm_decls_lx.fileno))],((i64)(mm_decls_lx.lineno) & (i64)16777215));
}

static void mm_support_stopcompiler(byte * filename,i64 lineno) {
    void *  f;
    f = fopen((i8 *)((byte*)"$error.tmp"),(i8 *)((byte*)"w"));
    msysc_m_print_startfile(f);
    msysc_m_print_str(filename,NULL);
    msysc_m_print_i64(lineno,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    fclose(f);
    msysc_m_print_startcon();
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    msysc_m_print_startcon();
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    exit((i64)1);
}

void mm_support_serror(byte * mess) {
    mm_support_serror_gen(mess);
}

void mm_support_serror_s(byte * mess,byte * a) {
    byte str[256];
    msysc_m_print_startstr(str);
    msysc_m_print_setfmt(mess);
    msysc_m_print_str(a,NULL);
    msysc_m_print_end();
    ;
    mm_support_serror_gen(str);
}

void mm_support_error_gen(i64 pass,byte * mess,struct mm_decls_unitrec * p) {
    i64 lineno;
    i64 fileno;
    if (!!(p)) {
        fileno = ((i64)((*p).lineno) >> (i64)24);
        lineno = ((i64)((*p).lineno) & (i64)16777215);
    } else {
        fileno = (mm_decls_mlineno >> (i64)24);
        lineno = (mm_decls_mlineno & (i64)16777215);
    };
    if ((!!(mm_decls_currproc) && ((i64)((u64)((*mm_decls_currproc).nameid)) == (i64)5))) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"In function",NULL);
        msysc_m_print_str((*mm_decls_currproc).name,NULL);
        msysc_m_print_nogap();
        msysc_m_print_str((byte*)" ",NULL);
        msysc_m_print_end();
        ;
    };
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"On line",NULL);
    msysc_m_print_i64((lineno & (i64)16777215),NULL);
    msysc_m_print_str((byte*)"in file",NULL);
    msysc_m_print_str(mm_decls_sourcefilepaths[(fileno)],NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    msysc_m_print_startcon();
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    if ((pass==(i64)78)) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"**** RX Name Error: ",NULL);
        msysc_m_print_end();
        ;
    }else if ((pass==(i64)84)) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"**** TX Type Error: ",NULL);
        msysc_m_print_end();
        ;
    }else if ((pass==(i64)71)) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"**** GX Code Gen Error: ",NULL);
        msysc_m_print_end();
        ;
    };
    msysc_m_print_startcon();
    msysc_m_print_str(mess,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    oslinux_os_getch();
    mm_support_stopcompiler(mm_decls_sourcefilepaths[(fileno)],(lineno & (i64)16777215));
}

void mm_support_rxerror(byte * mess,struct mm_decls_unitrec * p) {
    mm_support_error_gen((i64)78,mess,p);
}

void mm_support_gerror(byte * mess,struct mm_decls_unitrec * p) {
    mm_support_error_gen((i64)71,mess,p);
}

void mm_support_txerror(byte * mess,struct mm_decls_unitrec * p) {
    mm_support_error_gen((i64)84,mess,p);
}

void mm_support_txerror_s(byte * mess,byte * a,struct mm_decls_unitrec * p) {
    byte str[256];
    msysc_m_print_startstr(str);
    msysc_m_print_setfmt(mess);
    msysc_m_print_str(a,NULL);
    msysc_m_print_end();
    ;
    mm_support_error_gen((i64)84,str,p);
}

void mm_support_txerror_ss(byte * mess,byte * a,byte * b) {
    byte str[256];
    msysc_m_print_startstr(str);
    msysc_m_print_setfmt(mess);
    msysc_m_print_str(a,NULL);
    msysc_m_print_str(b,NULL);
    msysc_m_print_end();
    ;
    mm_support_error_gen((i64)84,str,(struct mm_decls_unitrec *)(0));
}

void mm_support_rxerror_s(byte * mess,byte * a,struct mm_decls_unitrec * p) {
    byte str[256];
    msysc_m_print_startstr(str);
    msysc_m_print_setfmt(mess);
    msysc_m_print_str(a,NULL);
    msysc_m_print_end();
    ;
    mm_support_error_gen((i64)78,str,p);
}

void mm_support_gerror_s(byte * mess,byte * s,struct mm_decls_unitrec * p) {
    byte str[256];
    msysc_m_print_startstr(str);
    msysc_m_print_setfmt(mess);
    msysc_m_print_str(s,NULL);
    msysc_m_print_end();
    ;
    mm_support_error_gen((i64)71,str,p);
}

void mm_support_lxerror_gen(byte * mess) {
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"On line",NULL);
    msysc_m_print_i64(mm_decls_nextlx.lineno,NULL);
    msysc_m_print_str((byte*)"in file",NULL);
    msysc_m_print_str(mm_decls_sourcefilepaths[((i64)(mm_decls_nextlx.fileno))],NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    msysc_m_print_startcon();
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"**** Lex Error:",NULL);
    msysc_m_print_str(mess,NULL);
    msysc_m_print_str((byte*)"****",NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    msysc_m_print_startcon();
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    mm_support_stopcompiler(mm_decls_sourcefilepaths[((i64)(mm_decls_nextlx.fileno))],(i64)(mm_decls_nextlx.lineno));
}

void mm_support_lxerror_s(byte * mess,byte * a) {
    byte str[256];
    msysc_m_print_startstr(str);
    msysc_m_print_setfmt(mess);
    msysc_m_print_str(a,NULL);
    msysc_m_print_end();
    ;
    mm_support_lxerror_gen(str);
}

void mm_support_lxerror(byte * mess) {
    mm_support_lxerror_gen(mess);
}

i64 mm_support_testelem(byte (*p)[],i64 n) {
    return (!!(((u64)((*p)[((n >> (i64)3))]) & (u64)(mm_support_bytemasks[((n & (i64)7))])))?(i64)1:(i64)0);
}

void mm_support_setelem(byte (*p)[],i64 n) {
    (*p)[((n >> (i64)3))] |= mm_support_bytemasks[((n & (i64)7))];
}

i64 mm_support_nextpoweroftwo(i64 x) {
    i64 a = (i64)1;
    if ((x == (i64)0)) {
        return (i64)0;
    };
    a = (i64)1;
    L227 :;
    while ((a < x)) {
        a <<= (i64)1;
L228 :;
    }L229 :;
    ;
    return a;
}

void mm_support_loaderror(byte * mess,byte * mess2,byte * mess3) {
    byte str[512];
    msysc_m_print_startstr(str);
    msysc_m_print_setfmt(mess);
    msysc_m_print_str(mess2,NULL);
    msysc_m_print_str(mess3,NULL);
    msysc_m_print_end();
    ;
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"Load Error:",NULL);
    msysc_m_print_str(str,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"Stopping",NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    exit((i64)1);
}

void mm_support_gs_additem(struct mlib_strbuffer * dest,byte * s) {
    byte *  d;
    i64 lastchar;
    i64 nextchar;
    d = (*dest).strptr;
    if (!!((i64)((*dest).length))) {
        lastchar = (i64)((*((d + (i64)((*dest).length)) - (i64)1)));
        nextchar = (i64)((*s));
        if ((!!(mm_support_isalphanum(lastchar)) && !!(mm_support_isalphanum(nextchar)))) {
            mlib_strbuffer_add(dest,(byte*)" ",(i64)-1);
        };
    };
    mlib_strbuffer_add(dest,s,(i64)-1);
}

void mm_support_gs_copytostr(struct mlib_strbuffer * source,byte * s) {
    if (!!((i64)((*source).length))) {
        memcpy((void *)(s),(void *)((*source).strptr),(u64)((*source).length));
        (*(s + (i64)((*source).length))) = (u64)0u;
    } else {
        (*s) = (u64)0u;
    };
}

i64 mm_support_isalphanum(i64 c) {
    if (((((c >= (i64)65) && (c <= (i64)90)) || ((c >= (i64)97) && (c <= (i64)122))) || ((c >= (i64)48) && (c <= (i64)57)))) {
        return (i64)1;
    };
    return (i64)0;
}

void mm_support_inittypetables(void) {
    i64 i;
    i64 size;
    i64 bitsize;
    i64 s;
    i64 t;
    i64 u;
    i64 v;
    i64 av_1;
    i64 av_2;
    L230 :;
    for (i=(i64)0;i<=(i64)55;++i) {
L231 :;
        mm_decls_ttname[(i)] = mm_tables_stdtypenames[(i)];
        mm_decls_ttbasetype[(i)] = i;
        bitsize = (i64)(mm_tables_stdtypebits[(i)]);
        switch (bitsize) {
        case 0:;
        {
            size = (i64)0;
        }break;
        case 1:;
        case 2:;
        case 4:;
        {
            size = (i64)1;
        }break;
        default: {
            size = (bitsize / (i64)8);
        }
        } //SW
;
        mm_decls_ttsize[(i)] = size;
        mm_decls_ttbitwidth[(i)] = (u64)(bitsize);
        mm_decls_ttisint[(i)] = (u64)(((u64)(mm_tables_stdtypecode[(i)]) == (u64)73u));
        mm_decls_ttisword[(i)] = (u64)(((u64)(mm_tables_stdtypecode[(i)]) == (u64)85u));
        if ((i==(i64)13) || (i==(i64)14) || (i==(i64)14)) {
            mm_decls_ttischar[(i)] = (u64)((i64)1);
        };
        mm_decls_ttiswordchar[(i)] = ((u64)(mm_decls_ttisword[(i)]) | (u64)(mm_decls_ttischar[(i)]));
        if (((u64)(mm_tables_stdtypecode[(i)]) == (u64)82u)) {
            mm_decls_ttisreal[(i)] = (u64)((i64)1);
        };
        mm_decls_ttisinteger[(i)] = ((u64)(mm_decls_ttisint[(i)]) | (u64)(mm_decls_ttiswordchar[(i)]));
        mm_decls_ttisnumeric[(i)] = ((u64)(mm_decls_ttisinteger[(i)]) | (u64)(mm_decls_ttisreal[(i)]));
        mm_decls_ttisshortint[(i)] = (u64)((!!(mm_decls_ttisinteger[(i)]) && ((i64)(mm_decls_ttsize[(i)]) < (i64)8)));
        mm_decls_ttisshortreal[(i)] = (u64)((i == (i64)11));
        if ((i==(i64)17) || (i==(i64)18) || (i==(i64)19)) {
            mm_decls_ttisbit[(i)] = (u64)((i64)1);
        };
        if (((i == (i64)20) || (t == mm_tables_trefproc))) {
            mm_decls_ttisref[(i)] = (u64)((i64)1);
        };
        mm_decls_tttypecode[(i)] = (u64)(mm_tables_stdtypecode[(i)]);
        mm_decls_ttisflex[(i)] = (u64)(((i64)((u64)(mm_tables_stdtypecat[(i)])) == (i64)49));
        mm_decls_ttisvariant[(i)] = (u64)(((i64)((u64)(mm_tables_stdtypecat[(i)])) == (i64)50));
        mm_decls_ttisflexvar[(i)] = ((u64)(mm_decls_ttisflex[(i)]) | (u64)(mm_decls_ttisvariant[(i)]));
        mm_decls_ttlower[(i)] = (i64)1;
L232 :;
    }L233 :;
    ;
    mm_decls_ttsize[((i64)20)] = mm_decls_targetsize;
    mm_decls_ttbitwidth[((i64)20)] = (u64)(mm_decls_targetbits);
    mm_decls_ntypes = (i64)55;
    mm_decls_ttbasetype[((i64)24)] = (i64)3;
    L234 :;
    for (i=(i64)1;i<=(i64)80;++i) {
L235 :;
        s = (i64)(mm_tables_typesetuptable[(i)-1][((i64)1)-1]);
        t = (i64)(mm_tables_typesetuptable[(i)-1][((i64)2)-1]);
        u = (i64)(mm_tables_typesetuptable[(i)-1][((i64)3)-1]);
        v = (i64)(mm_tables_typesetuptable[(i)-1][((i64)4)-1]);
        mm_tables_dominantmode[(s)][(t)] = (u64)(u);
        mm_tables_conversionops[(s)][(t)] = (u64)(v);
        if ((((v == (i64)4) || (v == (i64)5)) && ((i64)(mm_decls_ttsize[(s)]) == (i64)(mm_decls_ttsize[(t)])))) {
            msysc_m_print_startcon();
            msysc_m_print_str((byte*)"******* WIDEN SAME SIZE??",NULL);
            msysc_m_print_newline();
            msysc_m_print_end();
            ;
        };
L236 :;
    }L237 :;
    ;
}

void mm_support_addspecialtypes(void) {
    mm_tables_trefproc = mm_lib_createrefmode((struct mm_decls_strec *)(0),(i64)41,(i64)0);
    mm_tables_treflabel = mm_lib_createrefmode((struct mm_decls_strec *)(0),(i64)54,(i64)0);
    mm_tables_trefchar = mm_lib_createrefmode((struct mm_decls_strec *)(0),(i64)13,(i64)0);
}

static byte * mm_support_findfile(byte * filename) {
    static byte filespec[300];
    i64 i;
    L238 :;
    for (i=mm_decls_nsearchdirs;i>=(i64)1;--i) {
L239 :;
        strcpy((i8 *)(filespec),(i8 *)(mm_decls_searchdirs[(i)-1]));
        strcat((i8 *)(filespec),(i8 *)(filename));
        if (!!(mlib_checkfile(filespec))) {
            return filespec;
        };
L240 :;
    }L241 :;
    ;
    return (byte *)(0);
}

byte * mm_support_findstdlib(byte * name) {
    i64 av_1;
    i64 i;
    L242 :;
    for (i=(i64)1;i<=(i64)5;++i) {
L243 :;
        if (!!(mlib_eqstring(name,mm_genc64_stdlibnames[(i)-1]))) {
            return mm_genc64_stdlibtext[(i)-1];
        };
L244 :;
    }L245 :;
    ;
    return (byte *)(0);
}

i64 mm_support_getmainfile(byte * filename) {
    if (!!(mm_decls_fbundled)) {
        return mm_support_loadbundledfile(filename);
    };
    if (!(!!(mlib_checkfile(filename)))) {
        mm_support_loaderror((byte*)"Can't find main module: ##",filename,(byte*)"");
    };
    return mm_support_loadsourcefile(filename);
}

i64 mm_support_getmodulefile(byte * modulename,byte * ownername) {
    byte filename[300];
    byte *  file;
    byte *  libtext;
    strcpy((i8 *)(filename),(i8 *)(mlib_addext(modulename,(byte*)"m")));
    if (!!(mm_decls_fbundled)) {
        return mm_support_loadbundledfile(filename);
    };
    if (!!(mm_decls_dointlibs)) {
        libtext = mm_support_findstdlib(filename);
        if (!!(libtext)) {
            return mm_support_loadbuiltin(filename,libtext);
        };
    };
    file = mm_support_findfile(filename);
    if ((file == 0)) {
        mm_support_loaderror((byte*)"Can't find import module: # imported in: #",modulename,ownername);
    };
    return mm_support_loadsourcefile(file);
}

i64 mm_support_getsupportfile(byte * filename) {
    byte *  path;
    byte *  file;
    if (!!(mm_decls_fbundled)) {
        return mm_support_loadbundledfile(filename);
    };
    path = mlib_extractpath(filename);
    if (((((*path) == (i64)92) || ((*path) == (i64)47)) || (((i64)((*path)) != (i64)0) && ((u64)((*(path + (i64)1))) == ':')))) {
        file = filename;
    } else {
        file = mm_support_findfile(filename);
    };
    if (((file == 0) || !(!!(mlib_checkfile(file))))) {
        mm_support_loaderror((byte*)"Can't find include file: # #",filename,(byte*)"");
    };
    return mm_support_loadsourcefile(file);
}

void mm_support_writemafile(byte * leadmodule,byte * destfile) {
    byte filename[256];
    void *  f;
    i64 fileoffsets[250];
    i64 headeroffsets[250];
    i64 offset;
    i64 nn;
    i64 i;
    strcpy((i8 *)(filename),(i8 *)(mlib_changeext(leadmodule,(byte*)"ma")));
    if (!!(destfile)) {
        strcpy((i8 *)(filename),(i8 *)(destfile));
    };
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"Writing MA File",NULL);
    msysc_m_print_str(filename,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    f = fopen((i8 *)(filename),(i8 *)((byte*)"wb"));
    if (!(!!(f))) {
        mm_support_loaderror((byte*)"Can't create ma file #",filename,(byte*)"");
    };
    msysc_m_print_startfile(f);
    msysc_m_print_str((byte*)"mafile",NULL);
    msysc_m_print_i64(mm_decls_nsourcefiles,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    L246 :;
    for (i=(i64)1;i<=mm_decls_nsourcefiles;++i) {
L247 :;
        msysc_m_print_startfile(f);
        msysc_m_print_i64(i,(byte*)"3");
        msysc_m_print_str(mm_decls_sourcefilenames[(i)],(byte*)"16jl");
        msysc_m_print_i64(mm_decls_sourcefilesizes[(i)],(byte*)"7");
        msysc_m_print_end();
        ;
        headeroffsets[(i)-1] = (mlib_getfilepos(f) + (i64)1);
        msysc_m_print_startfile(f);
        msysc_m_print_str((byte*)"         ",NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
L248 :;
    }L249 :;
    ;
    L250 :;
    for (i=(i64)1;i<=mm_decls_nsourcefiles;++i) {
L251 :;
        msysc_m_print_startfile(f);
        msysc_m_print_setfmt((byte*)"=== # #/# ===");
        msysc_m_print_str(mm_decls_sourcefilenames[(i)],NULL);
        msysc_m_print_i64(i,NULL);
        msysc_m_print_i64(mm_decls_nsourcefiles,NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        offset = mlib_getfilepos(f);
        fileoffsets[(i)-1] = offset;
        nn = mlib_writerandom(f,(byte *)(mm_decls_mafiletext[(i)]),offset,mm_decls_sourcefilesizes[(i)]);
L252 :;
    }L253 :;
    ;
    msysc_m_print_startfile(f);
    msysc_m_print_str((byte*)"=== end ===",NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    L254 :;
    for (i=(i64)1;i<=mm_decls_nsourcefiles;++i) {
L255 :;
        mlib_setfilepos(f,headeroffsets[(i)-1]);
        msysc_m_print_startfile(f);
        msysc_m_print_i64(fileoffsets[(i)-1],(byte*)"8");
        msysc_m_print_end();
        ;
L256 :;
    }L257 :;
    ;
    fclose(f);
}

void mm_support_loadmafile(void) {
    void *  f;
    byte kwd[16];
    byte filename[256];
    i64 index;
    i64 size;
    i64 offset;
    i64 i;
    f = fopen((i8 *)(mm_decls_mafilename),(i8 *)((byte*)"rb"));
    if (!(!!(f))) {
        mm_support_loaderror((byte*)"Can't open ##",mm_decls_mafilename,(byte*)"");
    };
        msysc_m_read_fileline(f);
    ;
;
    msysc_readstr(kwd,(i64)110,(i64)16);
    if (!(!!(mlib_eqstring(kwd,(byte*)"mafile")))) {
        mm_support_loaderror((byte*)"Bad sig in ma file: # '#'",mm_decls_mafilename,kwd);
    };
        mm_decls_nmafiles = msysc_m_read_i64(0);
    ;
;
    L258 :;
    for (i=(i64)1;i<=mm_decls_nmafiles;++i) {
L259 :;
                msysc_m_read_fileline(f);
        ;
        index = msysc_m_read_i64(0);
        ;
;
        msysc_readstr(filename,(i64)110,(i64)256);
                size = msysc_m_read_i64(0);
        ;
        offset = msysc_m_read_i64(0);
        ;
;
        mm_decls_mafilenames[(i)] = mlib_pcm_copyheapstring(filename);
        mm_decls_mafilesizes[(i)] = size;
        mm_decls_mafileoffsets[(i)] = offset;
        mm_decls_mafilefileno[(i)] = (u64)((i64)0);
        mm_decls_mafilemult[(i)] = (u64)((i64)0);
L260 :;
    }L261 :;
    ;
    fclose(f);
    mm_decls_mafilesource = (byte *)(mlib_readfile(mm_decls_mafilename));
    if (!(!!(mm_decls_mafilesource))) {
        mm_support_loaderror((byte*)"MA load?",(byte*)"",(byte*)"");
    };
    L262 :;
    for (i=(i64)1;i<=mm_decls_nmafiles;++i) {
L263 :;
        size = mm_decls_mafilesizes[(i)];
        offset = mm_decls_mafileoffsets[(i)];
        mm_decls_mafiletext[(i)] = (mm_decls_mafilesource + offset);
        (*((mm_decls_mafilesource + offset) + size)) = (u64)0u;
L264 :;
    }L265 :;
    ;
}

static struct mm_decls_strec * mm_lib_newstrec(void) {
    struct mm_decls_strec *  p;
    p = (struct mm_decls_strec *)(mlib_pcm_alloc((i64)150));
    memset((void *)(p),(i64)0,(u64)((i64)150));
    (*p).lineno = (i64)(mm_decls_lx.lineno);
    (*p).moduleno = (u64)(mm_decls_currmoduleno);
    return p;
}

void mm_lib_initqclib(void) {
    i64 i;
    i64 av_1;
    i64 av_2;
    i64 av_3;
    i64 av_4;
    i64 av_5;
    i64 av_6;
    i64 av_7;
    L266 :;
    for (i=(i64)1;i<=(i64)37;++i) {
L267 :;
        mm_tables_jtagpriotable[(mm_tables_oplist[(i)-1])] = (u64)(mm_tables_oppriolist[(i)-1]);
L268 :;
    }L269 :;
    ;
    L270 :;
    for (i=(i64)1;i<=(i64)28;++i) {
L271 :;
        mm_decls_exprstarterset[(mm_tables_d_exprstarterset[(i)-1])] = (u64)((i64)1);
L272 :;
    }L273 :;
    ;
    L274 :;
    for (i=(i64)1;i<=(i64)9;++i) {
L275 :;
        mm_decls_typestarterset[(mm_tables_d_typestarterset[(i)-1])] = (u64)((i64)1);
L276 :;
    }L277 :;
    ;
    L278 :;
    for (i=(i64)1;i<=(i64)13;++i) {
L279 :;
        mm_tables_boolunitset[((i64)(mm_tables_d_boolunitset[(i)-1]))] = (u64)((i64)1);
L280 :;
    }L281 :;
    ;
    L282 :;
    for (i=(i64)1;i<=(i64)13;++i) {
L283 :;
        mm_tables_refunitset[((i64)(mm_tables_d_refunitset[(i)-1]))] = (u64)((i64)1);
L284 :;
    }L285 :;
    ;
    L286 :;
    for (i=(i64)1;i<=(i64)30;++i) {
L287 :;
        mm_tables_binopset[((i64)(mm_tables_d_binopset[(i)-1]))] = (u64)((i64)1);
L288 :;
    }L289 :;
    ;
    L290 :;
    for (i=(i64)1;i<=(i64)31;++i) {
L291 :;
        mm_tables_monopset[((i64)(mm_tables_d_monopset[(i)-1]))] = (u64)((i64)1);
L292 :;
    }L293 :;
    ;
    mm_tables_condopset[((i64)30)] = (u64)((i64)1);
    mm_tables_condopset[((i64)31)] = (u64)((i64)1);
    mm_tables_condopset[((i64)32)] = (u64)((i64)1);
    mm_tables_condopset[((i64)33)] = (u64)((i64)1);
    mm_tables_condopset[((i64)35)] = (u64)((i64)1);
    mm_tables_condopset[((i64)34)] = (u64)((i64)1);
    mm_tables_exprtermset[((i64)32)] = (u64)((i64)1);
    mm_tables_exprtermset[((i64)21)] = (u64)((i64)1);
    mm_tables_exprtermset[((i64)12)] = (u64)((i64)1);
    mm_tables_exprtermset[((i64)18)] = (u64)((i64)1);
    mm_tables_exprtermset[((i64)14)] = (u64)((i64)1);
    mm_tables_exprtermset[((i64)16)] = (u64)((i64)1);
    mm_tables_exprtermset[((i64)2)] = (u64)((i64)1);
    mm_tables_exprtermset[((i64)7)] = (u64)((i64)1);
    mm_tables_exprtermset[((i64)39)] = (u64)((i64)1);
    mm_tables_exprtermset[((i64)4)] = (u64)((i64)1);
    mm_tables_exprtermset[((i64)9)] = (u64)((i64)1);
    mm_tables_exprtermset[((i64)24)] = (u64)((i64)1);
    mm_tables_exprtermset[((i64)29)] = (u64)((i64)1);
}

struct mm_decls_strec * mm_lib_getduplnameptr(struct mm_decls_strec * owner,struct mm_decls_strec * symptr,i64 id) {
    struct mm_decls_strec *  p;
    p = mm_lib_newstrec();
    (*p).name = (*symptr).name;
    (*p).namelen = (u64)((*symptr).namelen);
    (*p).symbol = (u64)((i64)50);
    (*p).owner = owner;
    (*p).nameid = (u64)(id);
    (*p).namecat = (u64)(mm_tables_defaultnamecat[(id)]);
    if (((id == (i64)10) || (id == (i64)11))) {
    };
    (*p).nextdupl = (*symptr).nextdupl;
    (*p).firstdupl = symptr;
    (*symptr).nextdupl = p;
    return p;
}

void mm_lib_adddef(struct mm_decls_strec * owner,struct mm_decls_strec * p) {
    struct mm_decls_strec *  q;
    if (!!((q = (*p).nextdupl))) {
        if (((*q).owner == owner)) {
            msysc_m_print_startcon();
            msysc_m_print_str((*q).name,NULL);
            msysc_m_print_str((byte*)"in",NULL);
            msysc_m_print_str((*owner).name,NULL);
            msysc_m_print_newline();
            msysc_m_print_end();
            ;
            mm_support_serror((byte*)"Duplicate name");
        };
    };
    if (((*owner).deflist == 0)) {
        (*owner).deflist = p;
    } else {
        (*(*owner).deflistx).nextdef = p;
    };
    (*owner).deflistx = p;
}

void mm_lib_adddef_nodupl(struct mm_decls_strec * owner,struct mm_decls_strec * p) {
    if (((*owner).deflist == 0)) {
        (*owner).deflist = p;
    } else {
        (*(*owner).deflistx).nextdef = p;
    };
    (*owner).deflistx = p;
}

struct mm_decls_unitrec * mm_lib_createname(struct mm_decls_strec * p) {
    struct mm_decls_unitrec *  u;
    u = mm_lib_allocunitrec();
    (*u).tag = (i64)3;
    (*u).def = p;
    return u;
}

struct mm_decls_unitrec * mm_lib_createunit0(i64 tag) {
    struct mm_decls_unitrec *  u;
    u = mm_lib_allocunitrec();
    (*u).tag = tag;
    return u;
}

struct mm_decls_unitrec * mm_lib_createunit1(i64 tag,struct mm_decls_unitrec * p) {
    struct mm_decls_unitrec *  u;
    u = mm_lib_allocunitrec();
    (*u).tag = tag;
    (*u).a = p;
    return u;
}

struct mm_decls_unitrec * mm_lib_createunit2(i64 tag,struct mm_decls_unitrec * p,struct mm_decls_unitrec * q) {
    struct mm_decls_unitrec *  u;
    u = mm_lib_allocunitrec();
    (*u).tag = tag;
    (*u).a = p;
    (*u).b = q;
    return u;
}

struct mm_decls_unitrec * mm_lib_createunit3(i64 tag,struct mm_decls_unitrec * p,struct mm_decls_unitrec * q,struct mm_decls_unitrec * r) {
    struct mm_decls_unitrec *  u;
    u = mm_lib_allocunitrec();
    (*u).tag = tag;
    (*u).a = p;
    (*u).b = q;
    (*u).c = r;
    return u;
}

void mm_lib_insertunit(struct mm_decls_unitrec * p,i64 tag) {
    struct mm_decls_unitrec *  q;
    struct mm_decls_unitrec *  nextunit;
    i64 mode;
    q = mm_lib_allocunitrec();
    memcpy(&(*q),p,92);
    mode = (i64)((*q).mode);
    nextunit = (*q).nextunit;
    (*q).nextunit = (struct mm_decls_unitrec *)(0);
    memset((void *)(p),(i64)0,(u64)((i64)92));
    (*p).tag = tag;
    (*p).lineno = (i64)((*q).lineno);
    (*p).a = q;
    (*p).mode = mode;
    (*p).nextunit = nextunit;
}

void mm_lib_deleteunit(struct mm_decls_unitrec * p,struct mm_decls_unitrec * q) {
    struct mm_decls_unitrec *  r = (*p).nextunit;
    r = (*p).nextunit;
    memcpy(&(*p),q,92);
    (*p).nextunit = r;
}

struct mm_decls_unitrec * mm_lib_createconstunit(u64 a,i64 t) {
    struct mm_decls_unitrec *  u;
    u = mm_lib_allocunitrec();
    (*u).tag = (i64)1;
    (*u).value = (i64)(a);
    (*u).mode = t;
    (*u).isconst = (i64)1;
    return u;
}

struct mm_decls_unitrec * mm_lib_createstringconstunit(byte * s,i64 length) {
    struct mm_decls_unitrec *  u;
    u = mm_lib_allocunitrec();
    (*u).tag = (i64)1;
    (*u).svalue = s;
    (*u).mode = mm_tables_trefchar;
    (*u).isastring = (i64)1;
    if ((length == (i64)-1)) {
        (*u).slength = (i64)(strlen((i8 *)(s)));
    } else {
        (*u).slength = length;
    };
    return u;
}

i64 mm_lib_getoptocode(i64 opc) {
    static i16 opctotable[244];
    i64 opcto;
    i64 i;
    byte str[20];
    i64 av_1;
    opcto = (i64)(opctotable[(opc)]);
    if (!!(opcto)) {
        return opcto;
    };
    strcpy((i8 *)(str),(i8 *)(mm_tables_jtagnames[(opc)]));
    strcat((i8 *)(str),(i8 *)((byte*)"to"));
    L294 :;
    for (i=(i64)0;i<=(i64)243;++i) {
L295 :;
        if (!!(mlib_eqstring(mm_tables_jtagnames[(i)],str))) {
            opctotable[(opc)] = i;
            return i;
        };
L296 :;
    }L297 :;
    ;
    msysc_m_print_startcon();
    msysc_m_print_str(mm_tables_jtagnames[(opc)],NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    mm_support_serror((byte*)"Can't find -to version");
    return (i64)0;
}

i64 mm_lib_createtype(struct mm_decls_strec * d) {
    if (((i64)((u64)((*d).nameid)) == (i64)4)) {
        return (i64)((*d).mode);
    };
    return mm_lib_createusertype(d);
}

i64 mm_lib_createusertype(struct mm_decls_strec * stname) {
    if ((mm_decls_ntypes >= (i64)4000)) {
        msysc_m_print_startcon();
        msysc_m_print_i64(mm_decls_ntypes,NULL);
        msysc_m_print_str((*stname).name,NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        mm_support_serror((byte*)"Too many types");
    };
    ++mm_decls_ntypes;
    mm_decls_ttname[(mm_decls_ntypes)] = (*stname).name;
    mm_decls_ttnamedef[(mm_decls_ntypes)] = stname;
    mm_decls_ttbasetype[(mm_decls_ntypes)] = (i64)0;
    mm_decls_ttlineno[(mm_decls_ntypes)] = (i64)(mm_decls_lx.lineno);
    (*stname).mode = mm_decls_ntypes;
    return mm_decls_ntypes;
}

i64 mm_lib_createusertypefromstr(byte * name) {
    struct mm_decls_strec *  stname;
    stname = mm_lib_getduplnameptr(mm_decls_stmodule,mm_lex_addnamestr(name),(i64)4);
    mm_lib_adddef((!!(mm_decls_stmodule)?mm_decls_stmodule:mm_decls_stprogram),stname);
    return mm_lib_createusertype(stname);
}

i64 mm_lib_getconstvalue(struct mm_decls_unitrec * p,i64 id) {
    if ((!!(p) && ((i64)((*p).tag) == (i64)1))) {
        return (*p).value;
    };
    mm_support_serror((byte*)"GCV Not constant");
    return (i64)0;
}

struct mm_decls_unitrec * mm_lib_getrangelwbunit(struct mm_decls_unitrec * p) {
    if (((i64)((*p).tag) == (i64)15)) {
        return (*p).a;
    } else {
        return mm_lib_createunit1((i64)127,p);
    };
}

struct mm_decls_unitrec * mm_lib_getrangeupbunit(struct mm_decls_unitrec * p) {
    if (((i64)((*p).tag) == (i64)15)) {
        return (*p).b;
    } else {
        return mm_lib_createunit1((i64)128,p);
    };
}

i64 mm_lib_createarraymode(struct mm_decls_strec * owner,i64 target,struct mm_decls_unitrec * dimexpr,i64 typedefx) {
    i64 atype;
    i64 m;
    atype = (((target == (i64)17) || (target == (i64)18) || (target == (i64)19))?(i64)30:(i64)29);
    if ((typedefx == (i64)0)) {
        m = mm_lib_createusertypefromstr(mm_lib_nextautotype());
    } else {
        m = typedefx;
    };
    mm_decls_ttbasetype[(m)] = atype;
    mm_decls_ttlower[(m)] = (i64)1;
    mm_decls_ttdimexpr[(m)] = dimexpr;
    mm_lib_storemode((i64)101,owner,target,&mm_decls_tttarget[(m)]);
    mm_decls_ttowner[(m)] = owner;
    return m;
}

i64 mm_lib_createflexarraymode(struct mm_decls_strec * owner,i64 target,i64 typedefx) {
    i64 atype;
    i64 m;
    atype = (((target == (i64)17) || (target == (i64)18) || (target == (i64)19))?(i64)37:(i64)36);
    if ((typedefx == (i64)0)) {
        m = mm_lib_createusertypefromstr(mm_lib_nextautotype());
    } else {
        m = typedefx;
    };
    mm_decls_ttbasetype[(m)] = atype;
    mm_decls_ttlower[(m)] = (i64)1;
    mm_lib_storemode((i64)101,owner,target,&mm_decls_tttarget[(m)]);
    mm_decls_ttowner[(m)] = owner;
    mm_decls_ttisflex[(m)] = (u64)((i64)1);
    mm_decls_ttsize[(m)] = mm_decls_targetsize;
    return m;
}

i64 mm_lib_createarraymodek(struct mm_decls_strec * owner,i64 target,i64 lower,i64 length,i64 typedefx) {
    i64 atype;
    i64 m;
    atype = (i64)29;
    if ((typedefx == (i64)0)) {
        m = mm_lib_createusertypefromstr(mm_lib_nextautotype());
    } else {
        m = typedefx;
    };
    mm_decls_ttbasetype[(m)] = atype;
    mm_decls_ttlower[(m)] = lower;
    mm_decls_ttlength[(m)] = length;
    if ((target < (i64)0)) {
        mm_support_serror((byte*)"CREATEARRAYMODEK/TARGET NOT RESOLVED");
    };
    mm_decls_ttsize[(m)] = (length * (i64)(mm_decls_ttsize[(target)]));
    mm_lib_storemode((i64)101,owner,target,&mm_decls_tttarget[(m)]);
    mm_decls_ttowner[(m)] = owner;
    return m;
}

i64 mm_lib_createsetmode(struct mm_decls_strec * owner,struct mm_decls_unitrec * dimexpr,i64 typedefx) {
    i64 m;
    if ((typedefx == (i64)0)) {
        m = mm_lib_createusertypefromstr(mm_lib_nextautotype());
    } else {
        m = typedefx;
    };
    mm_decls_ttbasetype[(m)] = (i64)31;
    mm_decls_ttlower[(m)] = (i64)0;
    mm_decls_ttdimexpr[(m)] = dimexpr;
    mm_decls_ttowner[(m)] = owner;
    return m;
}

i64 mm_lib_createsetmodek(struct mm_decls_strec * owner,i64 length,i64 typedefx) {
    i64 m;
    if ((typedefx == (i64)0)) {
        m = mm_lib_createusertypefromstr(mm_lib_nextautotype());
    } else {
        m = typedefx;
    };
    mm_decls_ttbasetype[(m)] = (i64)31;
    mm_decls_ttlower[(m)] = (i64)0;
    mm_decls_ttlength[(m)] = length;
    mm_decls_ttowner[(m)] = owner;
    return m;
}

i64 mm_lib_createdictmode(struct mm_decls_strec * owner,i64 target,i64 keymode,i64 typedefx) {
    i64 m;
    if ((typedefx == (i64)0)) {
        m = mm_lib_createusertypefromstr(mm_lib_nextautotype());
    } else {
        m = typedefx;
    };
    mm_decls_ttbasetype[(m)] = (i64)40;
    mm_decls_ttlower[(m)] = (i64)1;
    mm_lib_storemode((i64)101,owner,target,&mm_decls_tttarget[(m)]);
    mm_lib_storemode((i64)201,owner,keymode,&mm_decls_ttkeymode[(m)]);
    mm_decls_ttowner[(m)] = owner;
    mm_decls_ttisflex[(m)] = (u64)((i64)1);
    return m;
}

byte * mm_lib_nextautotype(void) {
    static byte str[32];
    msysc_m_print_startstr(str);
    msysc_m_print_str((!!(mm_decls_ctarget)?(byte*)"_T$":(byte*)"$T"),NULL);
    msysc_m_print_nogap();
    msysc_m_print_i64(++mm_lib_autotypeno,NULL);
    msysc_m_print_end();
    ;
    return str;
}

void mm_lib_converttoslice(i64 t,i64 sltype) {
    mm_decls_ttbasetype[(t)] = sltype;
    mm_decls_ttsize[(t)] = (i64)(mm_decls_ttsize[((i64)28)]);
}

i64 mm_lib_createslicemode(struct mm_decls_strec * owner,i64 target,struct mm_decls_unitrec * dimexpr,i64 typedefx) {
    i64 m;
    if ((typedefx == (i64)0)) {
        m = mm_lib_createusertypefromstr(mm_lib_nextautotype());
    } else {
        m = typedefx;
    };
    mm_decls_ttbasetype[(m)] = (i64)28;
    if (!!(dimexpr)) {
        mm_decls_ttdimexpr[(m)] = dimexpr;
    } else {
        mm_decls_ttlower[(m)] = (i64)1;
    };
    mm_lib_storemode((i64)178,owner,target,&mm_decls_tttarget[(m)]);
    mm_decls_ttowner[(m)] = owner;
    return m;
}

i64 mm_lib_createslicemodek(struct mm_decls_strec * owner,i64 target,i64 lower,i64 typedefx) {
    i64 m;
    if ((typedefx == (i64)0)) {
        m = mm_lib_createusertypefromstr(mm_lib_nextautotype());
    } else {
        m = typedefx;
    };
    mm_decls_ttbasetype[(m)] = (i64)28;
    mm_decls_ttlower[(m)] = lower;
    mm_decls_tttarget[(m)] = target;
    mm_decls_ttowner[(m)] = owner;
    return m;
}

i64 mm_lib_createstringmode(i64 t,i64 length,i64 typedefx) {
    i64 k;
    i64 m;
    if ((typedefx == (i64)0)) {
        L298 :;
        for (k=(i64)56;k<=mm_decls_ntypes;++k) {
L299 :;
            if (((((i64)((u64)(mm_decls_ttusercat[(k)])) == (i64)0) && ((i64)(mm_decls_ttbasetype[(k)]) == t)) && ((i64)(mm_decls_ttlength[(k)]) == length))) {
                return k;
            };
L300 :;
        }L301 :;
        ;
        m = mm_lib_createusertypefromstr(mm_lib_nextautotype());
    } else {
        m = typedefx;
    };
    mm_decls_ttbasetype[(m)] = t;
    mm_decls_ttlower[(m)] = ((t == mm_tables_trefchar)?(i64)1:(i64)0);
    mm_decls_ttsize[(m)] = length;
    mm_decls_ttlength[(m)] = length;
    return m;
}

i64 mm_lib_createrefmode(struct mm_decls_strec * owner,i64 target,i64 typedefx) {
    i64 k;
    i64 m;
    if ((typedefx == (i64)0)) {
        L302 :;
        for (k=(i64)56;k<=mm_decls_ntypes;++k) {
L303 :;
            if (((((i64)((u64)(mm_decls_ttusercat[(k)])) == (i64)0) && ((i64)(mm_decls_ttbasetype[(k)]) == (i64)20)) && ((i64)(mm_decls_tttarget[(k)]) == target))) {
                return k;
            };
L304 :;
        }L305 :;
        ;
        m = mm_lib_createusertypefromstr(mm_lib_nextautotype());
    } else {
        m = typedefx;
    };
    mm_lib_storemode((i64)102,owner,target,&mm_decls_tttarget[(m)]);
    mm_decls_ttbasetype[(m)] = (i64)20;
    mm_decls_ttsize[(m)] = (i64)(mm_decls_ttsize[((i64)20)]);
    mm_decls_ttisref[(m)] = (u64)((i64)1);
    mm_decls_tttypecode[(m)] = (u64)80u;
    return m;
}

i64 mm_lib_createrefbitmode(struct mm_decls_strec * owner,i64 target,i64 typedefx) {
    i64 k;
    i64 m;
    if ((typedefx == (i64)0)) {
        L306 :;
        for (k=(i64)56;k<=mm_decls_ntypes;++k) {
L307 :;
            if (((((i64)((u64)(mm_decls_ttusercat[(k)])) == (i64)0) && ((i64)(mm_decls_ttbasetype[(k)]) == (i64)22)) && ((i64)(mm_decls_tttarget[(k)]) == target))) {
                return k;
            };
L308 :;
        }L309 :;
        ;
        m = mm_lib_createusertypefromstr(mm_lib_nextautotype());
    } else {
        m = typedefx;
    };
    mm_lib_storemode((i64)102,owner,target,&mm_decls_tttarget[(m)]);
    mm_decls_ttbasetype[(m)] = (i64)22;
    mm_decls_ttsize[(m)] = (i64)(mm_decls_ttsize[((i64)22)]);
    return m;
}

i64 mm_lib_createsubrangemode(struct mm_decls_strec * owner,struct mm_decls_unitrec * prange,i64 typedefx) {
    i64 m;
    if ((typedefx == (i64)0)) {
        m = mm_lib_createusertypefromstr(mm_lib_nextautotype());
    } else {
        m = typedefx;
    };
    mm_decls_ttbasetype[(m)] = (i64)23;
    mm_decls_ttsize[(m)] = (i64)(mm_decls_ttsize[((i64)23)]);
    mm_decls_ttdimexpr[(m)] = prange;
    return m;
}

i64 mm_lib_createrefprocmode(struct mm_decls_strec * owner,struct mm_decls_strec * stproc,struct mm_decls_strec * paramlist,i64 kwd,i64 prettype,i64 typedefx) {
    i64 m;
    i64 mproc;
    mproc = mm_lib_createusertype(stproc);
    (*stproc).paramlist = paramlist;
    (*stproc).mode = prettype;
    mm_decls_ttbasetype[(mproc)] = (i64)41;
    if ((typedefx == (i64)0)) {
        m = mm_lib_createusertypefromstr(mm_lib_nextautotype());
    } else {
        m = typedefx;
    };
    mm_lib_storemode((i64)103,owner,mproc,&mm_decls_tttarget[(m)]);
    mm_decls_ttbasetype[(m)] = (i64)20;
    mm_decls_ttsize[(m)] = (i64)(mm_decls_ttsize[((i64)20)]);
    mm_decls_ttisref[(m)] = (u64)((i64)1);
    return m;
}

void mm_lib_setnameptr(struct mm_decls_unitrec * p) {
    (*(*p).def).code = p;
}

byte * mm_lib_getdottedname(struct mm_decls_strec * p) {
    static byte str[256];
    byte str2[256];
    struct mm_decls_strec *  owner;
    strcpy((i8 *)(str),(i8 *)((*p).name));
    owner = (*p).owner;
    L310 :;
    while ((!!(owner) && ((i64)((u64)((*owner).nameid)) != (i64)1))) {
        strcpy((i8 *)(str2),(i8 *)(str));
        strcpy((i8 *)(str),(i8 *)((*owner).name));
        strcat((i8 *)(str),(i8 *)((byte*)"."));
        strcat((i8 *)(str),(i8 *)(str2));
        owner = (*owner).owner;
L311 :;
    }L312 :;
    ;
    return str;
}

struct mm_decls_strec * mm_lib_getavname(struct mm_decls_strec * owner,i64 id) {
    struct mm_decls_strec *  p;
    byte str[32];
    byte *  name;
    if (((id == (i64)10) && ((i64)((u64)((*owner).nameid)) != (i64)5))) {
        mm_support_serror((byte*)"Auto frame var not in proc");
    };
    if ((id == (i64)10)) {
        msysc_m_print_startstr(str);
        msysc_m_print_str((!!(mm_decls_ctarget)?(byte*)"av_":(byte*)"av$"),NULL);
        msysc_m_print_nogap();
        msysc_m_print_i64(++mm_lib_nextavindex,NULL);
        msysc_m_print_end();
        ;
    } else {
        msysc_m_print_startstr(str);
        msysc_m_print_str((!!(mm_decls_ctarget)?(byte*)"sv_":(byte*)"sv$"),NULL);
        msysc_m_print_i64(++mm_lib_nextsvindex,NULL);
        msysc_m_print_end();
        ;
    };
    name = mlib_pcm_copyheapstring(str);
    mm_lex_addnamestr(name);
    p = mm_lib_getduplnameptr(owner,mm_lex_addnamestr(name),id);
    (*p).namecat = (u64)((i64)7);
    (*p).used = (u64)((i64)1);
    (*p).mode = (i64)4;
    mm_lib_adddef(owner,p);
    return p;
}

void mm_lib_unionstr_clear(struct mm_decls_uflagsrec * u) {
    (*(u64 *)(u)) = (u64)((i64)0);
}

void mm_lib_unionstr_append(struct mm_decls_uflagsrec * u,i64 c) {
    if (((i64)((u64)((*u).ulength)) == (i64)6)) {
        mm_support_serror((byte*)"Uflags overflow/a");
    };
    ++(*u).ulength;
    (*u).codes[((i64)((*u).ulength))-1] = (u64)(c);
}

void mm_lib_unionstr_concat(struct mm_decls_uflagsrec * u,struct mm_decls_uflagsrec * v) {
    i64 ulen;
    i64 vlen;
    i64 i;
    ulen = (i64)((*u).ulength);
    vlen = (i64)((*v).ulength);
    if (((ulen + vlen) > (i64)7)) {
        mm_support_serror((byte*)"Uflags overflow/c");
    };
    L313 :;
    for (i=(i64)1;i<=vlen;++i) {
L314 :;
        (*u).codes[((i + ulen))-1] = (u64)((*v).codes[(i)-1]);
L315 :;
    }L316 :;
    ;
    (*u).ulength = (u64)((ulen + vlen));
}

i64 mm_lib_unionstr_last(struct mm_decls_uflagsrec * u) {
    if (!!((u64)((*u).ulength))) {
        return (i64)((*u).codes[((i64)((*u).ulength))-1]);
    };
    return (i64)0;
}

void mm_lib_unionstr_copy(struct mm_decls_uflagsrec * u,struct mm_decls_uflagsrec * v) {
    memcpy((void *)(u),(void *)(v),(u64)((i64)8));
}

void mm_lib_unionstr_print(struct mm_decls_uflagsrec * u) {
    mm_lex_printstrn((byte *)(&(*u).codes),(i64)((*u).ulength));
}

i64 mm_lib_createrecordmode(struct mm_decls_strec * owner,i64 t,i64 typedefx) {
    i64 m;
    if ((typedefx == (i64)0)) {
        m = mm_lib_createusertype(owner);
    } else {
        m = typedefx;
    };
    mm_decls_ttbasetype[(m)] = t;
    mm_decls_ttusercat[(m)] = (u64)((i64)1);
    return m;
}

i64 mm_lib_createenummode(struct mm_decls_strec * owner,i64 typedefx) {
    i64 m;
    if ((typedefx == (i64)0)) {
        m = mm_lib_createusertype(owner);
    } else {
        m = typedefx;
    };
    mm_decls_ttbasetype[(m)] = (i64)24;
    mm_decls_ttusercat[(m)] = (u64)((i64)1);
    return m;
}

void mm_lib_convertstring(byte * s,byte * t) {
    i64 c;
    L317 :;
    while (!!((c = (i64)((*s++))))) {
        switch (c) {
        case 34:;
        {
            (*t++) = (u64)92u;
            (*t++) = '"';
        }break;
        case 10:;
        {
            (*t++) = (u64)92u;
            (*t++) = 'n';
        }break;
        case 13:;
        {
            (*t++) = (u64)92u;
            (*t++) = 'c';
        }break;
        case 9:;
        {
            (*t++) = (u64)92u;
            (*t++) = 't';
        }break;
        case 92:;
        {
            (*t++) = (u64)92u;
            (*t++) = (u64)92u;
        }break;
        case 7:;
        case 8:;
        case 26:;
        case 27:;
        {
            (*t++) = '<';
            (*t++) = (u64)(((c / (i64)10) + (i64)48));
            (*t++) = (u64)(((c % (i64)10) + (i64)48));
            (*t++) = '>';
        }break;
        default: {
            (*t++) = (u64)(c);
        }
        } //SW
;
L318 :;
    }L319 :;
    ;
    (*t) = (u64)0u;
}

struct mlib_strbuffer * mm_lib_strexpr(struct mm_decls_unitrec * p) {
    mlib_gs_init(mm_lib_exprstr);
    mm_lib_jeval(mm_lib_exprstr,p);
    return mm_lib_exprstr;
}

static void mm_lib_jeval(struct mlib_strbuffer * dest,struct mm_decls_unitrec * p) {
    struct mm_decls_unitrec *  q;
    struct mm_decls_unitrec *  a;
    struct mm_decls_unitrec *  b;
    byte str[500];
    if ((p == 0)) {
        return;
    };
    a = (*p).a;
    b = (*p).b;
    switch ((i64)((*p).tag)) {
    case 1:;
    {
        if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)3) || ((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)4) || ((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)1) || ((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)2)) {
            msysc_getstrint((*p).value,str);
        }else if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)8) || ((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)9) || ((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)6) || ((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)7)) {
            strcpy((i8 *)(str),(i8 *)(msysc_strword((*p).uvalue,(byte *)(0))));
        }else if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)13) || ((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)14) || ((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)15)) {
            str[((i64)1)-1] = (u64)((*p).uvalue);
            str[((i64)0)-1] = (u64)0u;
        }else if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)5) || ((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)10)) {
            msysc_m_print_startstr(str);
            msysc_m_print_i64((*(*p).qvalue).upper,NULL);
            msysc_m_print_nogap();
            msysc_m_print_str((byte*)":",NULL);
            msysc_m_print_nogap();
            msysc_m_print_u64((*(*p).qvalue).lower,NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)12)) {
            msysc_m_print_startstr(str);
            msysc_m_print_r64((*p).xvalue,NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)20)) {
            if ((((i64)((*p).mode) == mm_tables_trefchar) && !!((i64)((*p).isastring)))) {
                if (((i64)((*p).slength) > (i64)250)) {
                    strcpy((i8 *)(str),(i8 *)((byte*)"LONGSTR)"));
                } else {
                    mm_lib_convertstring((*p).svalue,str);
                };
                mm_support_gs_additem(dest,(byte*)"\"");
                mm_support_gs_additem(dest,str);
                mm_support_gs_additem(dest,(byte*)"\"");
                return;
            } else {
                msysc_m_print_startstr(str);
                msysc_m_print_ptr((void *)((*p).value),NULL);
                msysc_m_print_end();
                ;
            };
        } else {
            if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)27)) {
                msysc_m_print_startstr(str);
                msysc_m_print_setfmt((byte*)"#..#");
                msysc_m_print_u64((*(*p).qvalue).lower,NULL);
                msysc_m_print_i64((*(*p).qvalue).upper,NULL);
                msysc_m_print_end();
                ;
            } else {
                sprintf((i8 *)(str),(i8 *)((byte*)"<EVAL/CONST PROBABLY VOID>"));
            };
        };
        mm_support_gs_additem(dest,str);
    }break;
    case 3:;
    {
        mm_support_gs_additem(dest,(*(*p).def).name);
    }break;
    case 9:;
    case 10:;
    case 29:;
    case 30:;
    case 31:;
    case 32:;
    case 33:;
    case 34:;
    case 35:;
    case 37:;
    case 38:;
    case 39:;
    case 40:;
    case 41:;
    case 42:;
    case 44:;
    case 45:;
    case 46:;
    case 47:;
    case 48:;
    case 49:;
    case 50:;
    case 51:;
    case 54:;
    case 55:;
    case 56:;
    case 57:;
    case 58:;
    case 59:;
    case 91:;
    case 92:;
    case 11:;
    case 36:;
    case 43:;
    case 60:;
    {
        strcpy((i8 *)(str),(i8 *)(mm_lib_getopcjname((i64)((*p).tag))));
        mm_support_gs_additem(dest,(byte*)"(");
        mm_lib_jeval(dest,a);
        mm_support_gs_additem(dest,str);
        mm_lib_jeval(dest,b);
        mm_support_gs_additem(dest,(byte*)")");
    }break;
    case 103:;
    case 104:;
    case 105:;
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
    case 123:;
    case 124:;
    case 125:;
    case 126:;
    case 127:;
    case 128:;
    case 129:;
    case 131:;
    case 132:;
    case 136:;
    case 137:;
    case 12:;
    case 13:;
    {
        strcpy((i8 *)(str),(i8 *)(mm_lib_getopcjname((i64)((*p).tag))));
        mm_support_gs_additem(dest,str);
        mm_support_gs_additem(dest,(byte*)"(");
        mm_lib_jeval(dest,a);
        mm_support_gs_additem(dest,(byte*)")");
    }break;
    case 25:;
    case 189:;
    {
        mm_lib_jeval(dest,a);
        mm_support_gs_additem(dest,(byte*)"(");
        q = b;
        L320 :;
        while (!!(q)) {
            mm_lib_jeval(dest,q);
            q = (*q).nextunit;
            if (!!(q)) {
                mm_support_gs_additem(dest,(byte*)",");
            };
L321 :;
        }L322 :;
        ;
        mm_support_gs_additem(dest,(byte*)")");
    }break;
    case 82:;
    case 84:;
    case 83:;
    case 85:;
    {
        mm_lib_jeval(dest,a);
        if ((((i64)((*p).tag) == (i64)84) || ((i64)((*p).tag) == (i64)85))) {
            mm_support_gs_additem(dest,(byte*)".");
        };
        mm_support_gs_additem(dest,(byte*)"[");
        mm_lib_jeval(dest,b);
        mm_support_gs_additem(dest,(byte*)"]");
    }break;
    case 89:;
    {
        mm_lib_jeval(dest,a);
        mm_support_gs_additem(dest,(byte*)".");
        mm_lib_jeval(dest,b);
    }break;
    case 14:;
    {
        mm_support_gs_additem(dest,(byte*)"(");
        q = a;
        L323 :;
        while (!!(q)) {
            mm_lib_jeval(dest,q);
            q = (*q).nextunit;
            if (!!(q)) {
                mm_support_gs_additem(dest,(byte*)",");
            };
L324 :;
        }L325 :;
        ;
        mm_support_gs_additem(dest,(byte*)")");
    }break;
    case 15:;
    {
        mm_support_gs_additem(dest,(byte*)"(");
        mm_lib_jeval(dest,a);
        mm_support_gs_additem(dest,(byte*)"..");
        mm_lib_jeval(dest,b);
        mm_support_gs_additem(dest,(byte*)")");
    }break;
    case 23:;
    {
        mm_lib_jeval(dest,a);
        mm_support_gs_additem(dest,(byte*)":=");
        mm_lib_jeval(dest,b);
    }break;
    case 196:;
    {
        mm_support_gs_additem(dest,(byte*)"(");
        mm_lib_jeval(dest,a);
        mm_support_gs_additem(dest,(byte*)"|");
        mm_lib_jeval(dest,b);
        mm_support_gs_additem(dest,(byte*)"|");
        mm_lib_jeval(dest,(*p).c);
        mm_support_gs_additem(dest,(byte*)")");
    }break;
    case 100:;
    {
        mm_support_gs_additem(dest,mm_lib_strmode((i64)((*p).mode),(i64)1));
    }break;
    case 96:;
    {
        mm_support_gs_additem(dest,mm_lib_strmode((i64)((*p).newmode),(i64)1));
        mm_support_gs_additem(dest,(byte*)"(");
        mm_lib_jeval(dest,a);
        mm_support_gs_additem(dest,(byte*)")");
    }break;
    case 98:;
    {
        mm_support_gs_additem(dest,(byte*)"cast(");
        mm_lib_jeval(dest,a);
        mm_support_gs_additem(dest,(byte*)")");
    }break;
    case 22:;
    {
        mm_lib_jeval(dest,a);
        mm_support_gs_additem(dest,(byte*)":");
        if (!!(b)) {
            mm_lib_jeval(dest,(*p).b);
        } else {
            mlib_gs_str(dest,(byte*)"-");
        };
    }break;
    case 93:;
    {
        mm_lib_jeval(dest,a);
        mm_support_gs_additem(dest,(byte*)"^");
    }break;
    case 61:;
    {
        mm_support_gs_additem(dest,(byte*)"(");
        mm_lib_jeval(dest,a);
        mm_support_gs_additem(dest,(byte*)",");
        mm_lib_jeval(dest,b);
        mm_support_gs_additem(dest,(byte*)",");
        mm_lib_jeval(dest,(*p).c);
        mm_support_gs_additem(dest,(byte*)")");
    }break;
    case 4:;
    {
        mm_support_gs_additem(dest,(byte*)"<JBLOCK>");
    }break;
    case 2:;
    {
        mlib_gs_str(dest,(byte*)"<nullunit>");
    }break;
    case 94:;
    {
        mm_support_gs_additem(dest,(byte*)"&");
        mm_lib_jeval(dest,a);
    }break;
    case 95:;
    {
        mm_support_gs_additem(dest,(byte*)"&.");
        mm_lib_jeval(dest,a);
    }break;
    case 97:;
    {
        mlib_gs_str(dest,(byte*)"CONVERTREF<>");
    }break;
    case 134:;
    {
        mm_support_gs_additem(dest,(byte*)"TYPESTR(");
        mm_lib_jeval(dest,a);
        mm_support_gs_additem(dest,(byte*)")");
    }break;
    case 64:;
    case 65:;
    case 66:;
    case 67:;
    case 68:;
    case 69:;
    case 72:;
    case 62:;
    case 63:;
    case 77:;
    case 78:;
    case 79:;
    case 80:;
    {
        mlib_gs_str(dest,(mm_tables_jtagnames[((i64)((*p).tag))] + (i64)2));
        mlib_gs_str(dest,(byte*)"(");
        mm_lib_jeval(dest,a);
        if (((i64)((*p).tag)==(i64)68) || ((i64)((*p).tag)==(i64)69) || ((i64)((*p).tag)==(i64)78) || ((i64)((*p).tag)==(i64)77) || ((i64)((*p).tag)==(i64)62) || ((i64)((*p).tag)==(i64)63)) {
            mlib_gs_str(dest,(byte*)",");
            mm_lib_jeval(dest,b);
        };
        mlib_gs_str(dest,(byte*)")");
    }break;
    case 173:;
    case 176:;
    case 175:;
    {
        mlib_gs_str(dest,(byte*)"$");
        mlib_gs_str(dest,(mm_tables_jtagnames[((i64)((*p).tag))] + (i64)2));
    }break;
    case 135:;
    {
        mm_lib_jeval(dest,a);
        mlib_gs_str(dest,(byte*)".");
        mlib_gs_str(dest,mm_tables_bitfieldnames[((i64)((*p).opcode))-1]);
    }break;
    case 187:;
    {
        mm_lib_jeval(dest,a);
        mlib_gs_str(dest,(byte*)":");
        mm_lib_jeval(dest,b);
    }break;
    case 133:;
    {
        mlib_gs_str(dest,(byte*)"typeof(");
        mm_lib_jeval(dest,a);
        mlib_gs_str(dest,(byte*)")");
    }break;
    default: {
        msysc_m_print_startcon();
        msysc_m_print_str(mm_tables_jtagnames[((i64)((*p).tag))],NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        mm_support_gerror((byte*)"CAN'T DO JEVAL",p);
    }
    } //SW
;
}

byte * mm_lib_getopcjname(i64 opc) {
    i64 i;
    i64 av_1;
    L326 :;
    for (i=(i64)1;i<=(i64)32;++i) {
L327 :;
        if ((opc == mm_lib_opc_codes[(i)-1])) {
            return mm_lib_opc_names[(i)-1];
        };
L328 :;
    }L329 :;
    ;
    return (mm_tables_jtagnames[(opc)] + (i64)2);
}

byte * mm_lib_strmode(i64 m,i64 expand) {
    static byte str[4096];
    mm_lib_istrmode(m,expand,str);
    return str;
}

byte * mm_lib_strmode2(i64 m,i64 expand) {
    static byte str[4096];
    mm_lib_istrmode(m,expand,str);
    return str;
}

void mm_lib_istrmode(i64 m,i64 expand,byte * dest) {
    struct mm_decls_strec *  d;
    struct mm_decls_strec *  q;
    i64 value;
    i64 needcomma;
    i64 x;
    i64 target;
    i64 mbase;
    struct mlib_strbuffer sxx;
    struct mlib_strbuffer *  xx = &sxx;
    byte strdim[100];
    byte *  prefix;
    xx = &sxx;
    if ((m < (i64)0)) {
        strcpy((i8 *)(dest),(i8 *)((byte*)"*"));
        strcat((i8 *)(dest),(i8 *)((*mm_decls_ttnamedefx[(-(m))]).name));
        if (!!(mm_decls_ttnamedefx2[(-(m))])) {
            strcat((i8 *)(dest),(i8 *)((byte*)"."));
            strcat((i8 *)(dest),(i8 *)((*mm_decls_ttnamedefx2[(-(m))]).name));
        };
        return;
    };
    if (((m < (i64)56) && (m != (i64)20))) {
        strcpy((i8 *)(dest),(i8 *)(mm_lib_typename(m)));
        return;
    };
    if (((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)20) || ((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)22)) {
        strcpy((i8 *)(dest),((mbase == (i64)20)?(i8 *)((byte*)"ref "):(i8 *)((byte*)"refbit ")));
        target = (i64)(mm_decls_tttarget[(m)]);
        if (((target >= (i64)0) && ((i64)(mm_decls_ttbasetype[(target)]) == (i64)32))) {
            strcat((i8 *)(dest),(i8 *)(mm_lib_typename(target)));
        } else {
            mm_lib_istrmode((i64)(mm_decls_tttarget[(m)]),(i64)0,(dest + (i64)(strlen((i8 *)(dest)))));
        };
    }else if (((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)31)) {
        if (!!(mm_decls_ttdimexpr[(m)])) {
            mm_support_gs_copytostr(mm_lib_strexpr(mm_decls_ttdimexpr[(m)]),strdim);
            msysc_m_print_startstr(dest);
            msysc_m_print_setfmt((byte*)"set[#]");
            msysc_m_print_str(strdim,NULL);
            msysc_m_print_end();
            ;
        } else {
            msysc_m_print_startstr(dest);
            msysc_m_print_setfmt((byte*)"set[#]");
            msysc_m_print_i64(mm_decls_ttlength[(m)],NULL);
            msysc_m_print_end();
            ;
        };
    }else if (((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)29) || ((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)30)) {
        if (!!(mm_decls_ttdimexpr[(m)])) {
            mm_support_gs_copytostr(mm_lib_strexpr(mm_decls_ttdimexpr[(m)]),strdim);
            msysc_m_print_startstr(dest);
            msysc_m_print_setfmt((byte*)"@[#]");
            msysc_m_print_str(strdim,NULL);
            msysc_m_print_end();
            ;
        } else {
            if (((i64)(mm_decls_ttlower[(m)]) == (i64)1)) {
                msysc_m_print_startstr(dest);
                msysc_m_print_setfmt((byte*)"[#]");
                msysc_m_print_i64((((i64)(mm_decls_ttlength[(m)]) + (i64)(mm_decls_ttlower[(m)])) - (i64)1),NULL);
                msysc_m_print_end();
                ;
            } else {
                msysc_m_print_startstr(dest);
                msysc_m_print_setfmt((byte*)"[#..#]");
                msysc_m_print_i64(mm_decls_ttlower[(m)],NULL);
                msysc_m_print_i64((((i64)(mm_decls_ttlength[(m)]) + (i64)(mm_decls_ttlower[(m)])) - (i64)1),NULL);
                msysc_m_print_end();
                ;
            };
        };
        mm_lib_istrmode((i64)(mm_decls_tttarget[(m)]),(i64)0,(dest + (i64)(strlen((i8 *)(dest)))));
    }else if (((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)28)) {
        prefix = ((mbase != (i64)28)?(byte*)"":mm_tables_stdtypenames[(mbase)]);
        if (!!(mm_decls_ttdimexpr[(m)])) {
            mm_support_gs_copytostr(mm_lib_strexpr(mm_decls_ttdimexpr[(m)]),strdim);
            msysc_m_print_startstr(dest);
            msysc_m_print_setfmt((byte*)"@slice[#:]");
            msysc_m_print_str(strdim,NULL);
            msysc_m_print_end();
            ;
        } else {
            if (((i64)(mm_decls_ttlower[(m)]) == (i64)1)) {
                strcpy((i8 *)(dest),(i8 *)((byte*)"slice[]"));
            } else {
                msysc_m_print_startstr(dest);
                msysc_m_print_setfmt((byte*)"slice[#:]");
                msysc_m_print_i64(mm_decls_ttlower[(m)],NULL);
                msysc_m_print_end();
                ;
            };
        };
        mm_lib_istrmode((i64)(mm_decls_tttarget[(m)]),(i64)0,(dest + (i64)(strlen((i8 *)(dest)))));
    }else if (((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)36) || ((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)37)) {
        strcpy((i8 *)(dest),(i8 *)((byte*)"flex[]"));
        mm_lib_istrmode((i64)(mm_decls_tttarget[(m)]),(i64)0,(dest + (i64)(strlen((i8 *)(dest)))));
    }else if (((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)38)) {
        if (!!(mm_decls_ttdimexpr[(m)])) {
            mm_support_gs_copytostr(mm_lib_strexpr(mm_decls_ttdimexpr[(m)]),strdim);
            msysc_m_print_startstr(dest);
            msysc_m_print_setfmt((byte*)"[#:*,*]");
            msysc_m_print_str(strdim,NULL);
            msysc_m_print_end();
            ;
        } else {
            strcpy((i8 *)(dest),(i8 *)((byte*)"[*,*]"));
        };
        mm_lib_istrmode((i64)(mm_decls_tttarget[(m)]),(i64)0,(dest + (i64)(strlen((i8 *)(dest)))));
    }else if (((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)24)) {
        strcpy((i8 *)(dest),(i8 *)((byte*)"enum("));
        d = mm_decls_ttnamedef[(m)];
        value = (i64)1;
        needcomma = (i64)0;
        q = (*d).deflist;
        L330 :;
        while (!!(q)) {
            if (!!(needcomma)) {
                strcat((i8 *)(dest),(i8 *)((byte*)","));
            };
            needcomma = (i64)1;
            strcat((i8 *)(dest),(i8 *)((*q).name));
            x = (i64)((*q).index);
            if ((x != value)) {
                value = x;
                msysc_getstrint(value,(dest + (i64)(strlen((i8 *)(dest)))));
            };
            ++value;
            q = (*q).nextdef;
L331 :;
        }L332 :;
        ;
        strcat((i8 *)(dest),(i8 *)((byte*)")"));
    }else if (((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)32) || ((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)33)) {
        strcpy((i8 *)(dest),(i8 *)((byte*)""));
        if (!(!!(expand))) {
            strcpy((i8 *)(dest),(i8 *)(mm_lib_typename(m)));
            return;
        };
        strcat((i8 *)(dest),(i8 *)(mm_lib_typename((i64)(mm_decls_ttbasetype[(m)]))));
        strcat((i8 *)(dest),(i8 *)((byte*)"("));
        d = mm_decls_ttnamedef[(m)];
        needcomma = (i64)0;
        q = (*d).deflist;
        L333 :;
        while (!!(q)) {
            if (!!(needcomma)) {
                strcat((i8 *)(dest),(i8 *)((byte*)","));
            };
            needcomma = (i64)1;
            mm_lib_istrmode((i64)((*q).mode),(i64)0,(dest + (i64)(strlen((i8 *)(dest)))));
            strcat((i8 *)(dest),(i8 *)((byte*)" "));
            strcat((i8 *)(dest),(i8 *)((*q).name));
            q = (*q).nextdef;
L334 :;
        }L335 :;
        ;
        strcat((i8 *)(dest),(i8 *)((byte*)")"));
    }else if (((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)0)) {
        strcpy((i8 *)(dest),(i8 *)((byte*)"void"));
    }else if (((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)56)) {
        strcpy((i8 *)(dest),(i8 *)(mm_lib_typename(m)));
    }else if (((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)41)) {
        d = mm_decls_ttnamedef[(m)];
        strcpy((i8 *)(dest),(i8 *)((byte*)"proc("));
        q = (*d).paramlist;
        needcomma = (i64)0;
        L336 :;
        while ((q != 0)) {
            if (!!(needcomma)) {
                strcat((i8 *)(dest),(i8 *)((byte*)","));
            };
            needcomma = (i64)1;
            mm_lib_istrmode((i64)((*q).mode),(i64)0,(dest + (i64)(strlen((i8 *)(dest)))));
            strcat((i8 *)(dest),(i8 *)((byte*)" "));
            strcat((i8 *)(dest),(i8 *)((*q).name));
            q = (*q).nextdef;
L337 :;
        }L338 :;
        ;
        strcat((i8 *)(dest),(i8 *)((byte*)")"));
        if (((i64)((*d).mode) != (i64)0)) {
            mm_lib_istrmode((i64)((*d).mode),(i64)0,(dest + (i64)(strlen((i8 *)(dest)))));
        };
    }else if (((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)27)) {
        strcpy((i8 *)(dest),(i8 *)((byte*)"range"));
    }else if (((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)40)) {
        strcpy((i8 *)(dest),(i8 *)((byte*)"dict["));
        mm_lib_istrmode((i64)(mm_decls_ttkeymode[(m)]),(i64)0,(dest + (i64)(strlen((i8 *)(dest)))));
        strcat((i8 *)(dest),(i8 *)((byte*)"]"));
        mm_lib_istrmode((i64)(mm_decls_tttarget[(m)]),(i64)0,(dest + (i64)(strlen((i8 *)(dest)))));
    }else if (((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)23)) {
        strcpy((i8 *)(dest),(i8 *)((byte*)"subrange("));
        strcat((i8 *)(dest),(i8 *)((*mm_lib_strexpr(mm_decls_ttdimexpr[(m)])).strptr));
        strcat((i8 *)(dest),(i8 *)((byte*)")"));
    }else if (((mbase = (i64)(mm_decls_ttbasetype[(m)]))==(i64)25)) {
        strcpy((i8 *)(dest),(i8 *)((byte*)"bitfield"));
    } else {
        msysc_m_print_startcon();
        msysc_m_print_str(mm_lib_typename(m),NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        mm_support_mcerror((byte*)"NEWSTRMODE");
    };
}

i64 mm_lib_countunits(struct mm_decls_unitrec * p) {
    i64 n;
    n = (i64)0;
    L339 :;
    while (!!(p)) {
        ++n;
        p = (*p).nextunit;
L340 :;
    }L341 :;
    ;
    return n;
}

struct mm_decls_strec * mm_lib_finddefstr(struct mm_decls_strec * owner,byte * name) {
    struct mm_decls_strec *  d;
    d = (*owner).deflist;
    L342 :;
    while (!!(d)) {
        if (!!(mlib_eqstring((*d).name,name))) {
            return d;
        };
        d = (*d).nextdef;
L343 :;
    }L344 :;
    ;
    return (struct mm_decls_strec *)(0);
}

void mm_lib_addtoproclist(struct mm_decls_strec * d) {
    struct mm_decls_procrec *  pp;
    ++mm_decls_nproclist;
    pp = (struct mm_decls_procrec *)(mlib_pcm_alloc((i64)16));
    (*pp).nextproc = mm_decls_proclist;
    mm_decls_proclist = pp;
    (*pp).def = d;
}

void mm_lib_addstatic(struct mm_decls_strec * d) {
    struct mm_decls_procrec *  pp;
    ++mm_decls_nstaticlist;
    pp = (struct mm_decls_procrec *)(mlib_pcm_alloc((i64)16));
    (*pp).nextproc = mm_decls_staticlist;
    mm_decls_staticlist = pp;
    (*pp).def = d;
}

i64 mm_lib_newusertypex(struct mm_decls_strec * d,struct mm_decls_strec * e) {
    if ((mm_decls_nuserxtypes >= (i64)4000)) {
        mm_support_serror((byte*)"Too many external user types");
    };
    ++mm_decls_nuserxtypes;
    mm_decls_ttnamedefx[(mm_decls_nuserxtypes)] = d;
    mm_decls_ttnamedefx2[(mm_decls_nuserxtypes)] = e;
    mm_decls_ttxmoduleno[(mm_decls_nuserxtypes)] = (u64)(mm_decls_currmoduleno);
    mm_decls_ttlinenox[(mm_decls_nuserxtypes)] = ((i64)(mm_decls_lx.lineno) & (i64)16777215);
    return -(mm_decls_nuserxtypes);
}

byte * mm_lib_typename(i64 m) {
    if ((m >= (i64)0)) {
        return mm_decls_ttname[(m)];
    };
    return (*mm_decls_ttnamedefx[(-(m))]).name;
}

struct mm_decls_unitrec * mm_lib_allocunitrec(void) {
    struct mm_decls_unitrec *  p;
    if (!!(mm_lib_remainingunits--)) {
        p = mm_lib_unitheapptr;
        ++mm_lib_unitheapptr;
        (*p).lineno = (i64)(mm_decls_lx.lineno);
        (*p).moduleno = mm_decls_currmoduleno;
        return p;
    };
    p = (mm_lib_unitheapptr = (struct mm_decls_unitrec *)(mlib_pcm_alloc((i64)4600000)));
    memset((void *)(p),(i64)0,(u64)((i64)4600000));
    mm_lib_remainingunits = (i64)49999;
    ++mm_lib_unitheapptr;
    (*p).lineno = (i64)(mm_decls_lx.lineno);
    (*p).moduleno = mm_decls_currmoduleno;
    return p;
}

struct mm_decls_strec * mm_lib_createdupldef(struct mm_decls_strec * owner,struct mm_decls_strec * symptr,i64 id) {
    struct mm_decls_strec *  p;
    p = mm_lib_newstrec();
    (*p).name = (*symptr).name;
    (*p).namelen = (u64)((*symptr).namelen);
    (*p).symbol = (u64)((i64)50);
    (*p).owner = owner;
    (*p).nameid = (u64)(id);
    (*p).nextdupl = (*symptr).nextdupl;
    (*symptr).nextdupl = p;
    if (!!(owner)) {
        if (((*owner).deflist == 0)) {
            (*owner).deflist = ((*owner).deflistx = p);
        } else {
            (*(*owner).deflistx).nextdef = p;
            (*owner).deflistx = p;
        };
    };
    return p;
}

struct mm_decls_strec * mm_lib_createnewmoduledef(struct mm_decls_strec * owner,struct mm_decls_strec * symptr) {
    struct mm_decls_strec *  p;
    p = mm_lib_createdupldef(owner,symptr,(i64)2);
    return p;
}

void mm_lib_storemode(i64 id,struct mm_decls_strec * owner,i64 m,i32 * p) {
    struct mm_decls_userxrec *  q;
    (*p) = m;
    if ((m >= (i64)0)) {
        return;
    };
    q = (struct mm_decls_userxrec *)(mlib_pcm_alloc((i64)24));
    (*q).owner = owner;
    if ((owner == 0)) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"ID=",NULL);
        msysc_m_print_i64(id,NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        mm_support_serror((byte*)"STOREMODE/OWNER=0");
    };
    (*q).pmode = p;
    (*q).nextmode = mm_decls_userxmodelist;
    mm_decls_userxmodelist = q;
}

struct mm_decls_unitrec * mm_lib_duplunit(struct mm_decls_unitrec * p,i64 lineno) {
    struct mm_decls_unitrec *  q;
    if ((p == 0)) {
        return (struct mm_decls_unitrec *)(0);
    };
    q = mm_lib_createunit0((i64)((*p).tag));
    (*q).a = mm_lib_duplunit((*p).a,lineno);
    (*q).b = mm_lib_duplunit((*p).b,lineno);
    (*q).c = mm_lib_duplunit((*p).c,lineno);
    (*q).lineno = (!!(lineno)?lineno:(i64)((*p).lineno));
    (*q).value = (*p).value;
    (*q).opcode = (i64)((*p).opcode);
    (*q).mode = (i64)((*p).mode);
    (*q).moduleno = (i64)((*p).moduleno);
    (*q).isastring = (i64)((*p).isastring);
    return q;
}

void mm_lib_addstr(byte * * p,byte * s) {
    L345 :;
    while (!!((u64)((*s)))) {
        (*(*p)) = (u64)((*s));
        ++(*p);
        ++s;
L346 :;
    }L347 :;
    ;
}

void mm_lib_addchar(byte * * p,byte c) {
    (*(*p)) = (u64)(c);
    ++(*p);
}

void mm_lib_addint(byte * * p,i64 x) {
    mm_lib_addstr(p,msysc_strint(x,(byte *)(0)));
}

i64 mm_lib_iscallbackfn(struct mm_decls_strec * p) {
    return ((i64)((u64)((*p).fflang)) == (i64)5);
}

i64 mm_lib_isstringconst(struct mm_decls_unitrec * p) {
    i64 m;
    i64 target;
    m = (i64)((*p).mode);
    if ((((i64)((*p).tag) == (i64)1) && ((i64)(mm_decls_ttbasetype[(m)]) == (i64)20))) {
        target = (i64)(mm_decls_ttbasetype[((i64)(mm_decls_tttarget[(m)]))]);
        if (((target == (i64)13) || !!((i64)((*p).slength)))) {
            return (i64)1;
        };
    };
    return (i64)0;
}

i64 mm_lib_checkblockreturn(struct mm_decls_unitrec * p) {
    struct mm_decls_unitrec *  e;
    struct mm_decls_unitrec *  wt;
    i64 m;
    if ((p == 0)) {
        return (i64)0;
    };
    m = (i64)((*p).mode);
    if (((i64)((*p).tag)==(i64)191)) {
        return (i64)1;
    }else if (((i64)((*p).tag)==(i64)235)) {
        return (i64)1;
    }else if (((i64)((*p).tag)==(i64)196)) {
        (*p).ifretflag = (i64)1;
        return (!!(mm_lib_checkblockreturn((*p).b)) && !!(mm_lib_checkblockreturn((*p).c)));
    }else if (((i64)((*p).tag)==(i64)197)) {
        e = (*p).a;
        (*p).ifretflag = (i64)1;
        L348 :;
        while (!!(e)) {
            if (!(!!(mm_lib_checkblockreturn((*e).b)))) {
                return (i64)0;
            };
            e = (*e).nextunit;
L349 :;
        }L350 :;
        ;
        return mm_lib_checkblockreturn((*p).b);
    }else if (((i64)((*p).tag)==(i64)4)) {
        e = (*p).a;
        if (!!(e)) {
            L351 :;
            while ((!!(e) && !!((*e).nextunit))) {
                e = (*e).nextunit;
L352 :;
            }L353 :;
            ;
            return mm_lib_checkblockreturn(e);
        };
    }else if (((i64)((*p).tag)==(i64)216) || ((i64)((*p).tag)==(i64)218) || ((i64)((*p).tag)==(i64)217) || ((i64)((*p).tag)==(i64)219)) {
        (*p).ifretflag = (i64)1;
        wt = (*p).b;
        L354 :;
        while (!!(wt)) {
            if (!(!!(mm_lib_checkblockreturn((*wt).b)))) {
                return (i64)0;
            };
            wt = (*wt).nextunit;
L355 :;
        }L356 :;
        ;
        return mm_lib_checkblockreturn((*p).c);
    }else if (((i64)((*p).tag)==(i64)7)) {
        return (i64)1;
    };
    if ((!!((u64)(mm_tables_jisexpr[((i64)((*p).tag))])) && (m != (i64)0))) {
        if (!!(mm_decls_ctarget)) {
            mm_lib_insertunit(p,(i64)191);
            (*p).mode = m;
        };
        return (i64)1;
    } else {
        return (i64)0;
    };
}

byte * mm_lib_strqvalue(struct mm_decls_qint * aa) {
    static byte str[64];
    msysc_m_print_startstr(str);
    msysc_m_print_setfmt((byte*)"#:#\n");
    msysc_m_print_i64((*aa).upper,(byte*)"H");
    msysc_m_print_u64((*aa).lower,(byte*)"Z16H");
    msysc_m_print_end();
    ;
    return str;
}

struct mm_decls_qint * mm_lib_makeqvalue(i64 a,i64 scat) {
    struct mm_decls_qint *  aa;
    aa = (struct mm_decls_qint *)(mlib_pcm_alloc((i64)16));
    (*aa).lower = (u64)(a);
    (*aa).upper = (i64)0;
    if (((scat == (i64)73) && (a < (i64)0))) {
        (*aa).upper = (i64)4294967295;
    };
    return aa;
}

struct mm_decls_qint * mm_lib_makeqvalue_ab(i64 a,i64 b) {
    struct mm_decls_qint *  aa;
    aa = (struct mm_decls_qint *)(mlib_pcm_alloc((i64)16));
    (*aa).lower = (u64)(a);
    (*aa).upper = b;
    return aa;
}

i64 mm_lib_isconstint(struct mm_decls_unitrec * a) {
    if ((!!((i64)((*a).isconst)) && !!(mm_lib_isintmode((i64)((*a).mode))))) {
        return (i64)1;
    };
    return (i64)0;
}

i64 mm_lib_isconstunit(struct mm_decls_unitrec * a) {
    return (i64)((*a).isconst);
}

byte * mm_lib_getfullname(struct mm_decls_strec * d,i64 fromassem) {
    static byte str[256];
    if ((!(!!(fromassem)) && (mm_decls_fvarnames != (i64)2))) {
        if (((i64)((*d).nameid)==(i64)10) || ((i64)((*d).nameid)==(i64)11)) {
            if (!!(mm_decls_fvarnames)) {
                mm_lib_framevarname = (*d).name;
            };
            msysc_getstrint((i64)((*d).offset),str);
            return str;
        };
    };
    if (!!((u64)(mm_tables_qualifiedname[((i64)((*d).namecat))]))) {
        str[((i64)1)-1] = (u64)0u;
        mm_lib_getownername(d,str);
        strcat((i8 *)(str),(i8 *)((*d).name));
        return str;
    } else if (((i64)((u64)((*d).namecat)) == (i64)3)) {
        msysc_m_print_startstr(str);
        msysc_m_print_str((byte*)"`",NULL);
        msysc_m_print_nogap();
        msysc_m_print_str((!!((*d).truename)?(*d).truename:(*d).name),NULL);
        msysc_m_print_end();
        ;
        return str;
    } else {
        return (*d).name;
    };
}

void mm_lib_getownername(struct mm_decls_strec * d,byte * dest) {
    struct mm_decls_strec *  owner;
    owner = (*d).owner;
    if (((owner == 0) || ((i64)((u64)((*owner).nameid)) == (i64)1))) {
        return;
    };
    mm_lib_getownername(owner,dest);
    strcat((i8 *)(dest),(i8 *)((*owner).name));
    strcat((i8 *)(dest),(i8 *)((byte*)"."));
}

i64 mm_lib_isintmode(i64 m) {
    return (i64)(mm_decls_ttisinteger[(m)]);
}

i64 mm_lib_isnumericmode(i64 m) {
    return (i64)(mm_decls_ttisnumeric[(m)]);
}

i64 mm_lib_isrefmode(i64 m) {
    return (i64)(mm_decls_ttisref[(m)]);
}

byte * mm_lib_strconstopnd(struct mm_decls_unitrec * p) {
    static byte str[256];
    i64 a;
    i64 t;
    float x32;
    t = (i64)((*p).mode);
    a = (*p).value;
    if ((t == mm_tables_trefchar)) {
        if (((i64)((*p).slength) >= (i64)256)) {
            msysc_m_print_startstr(str);
            msysc_m_print_str((byte*)"\"",NULL);
            msysc_m_print_nogap();
            msysc_m_print_str((byte*)"(LONGSTR)",NULL);
            msysc_m_print_str((byte*)"\" *",NULL);
            msysc_m_print_nogap();
            msysc_m_print_i64((*p).slength,NULL);
            msysc_m_print_end();
            ;
        } else if (!!((i64)((*p).slength))) {
            msysc_m_print_startstr(str);
            msysc_m_print_str((byte*)"\"",NULL);
            msysc_m_print_nogap();
            msysc_m_print_str((*p).svalue,NULL);
            msysc_m_print_nogap();
            msysc_m_print_str((byte*)"\" *",NULL);
            msysc_m_print_nogap();
            msysc_m_print_i64((*p).slength,NULL);
            msysc_m_print_end();
            ;
        } else {
            msysc_m_print_startstr(str);
            msysc_m_print_str((byte*)"\"\"",NULL);
            msysc_m_print_end();
            ;
        };
    } else {
        if (((i64)(mm_decls_ttbasetype[(t)])==(i64)1)) {
            msysc_m_print_startstr(str);
            msysc_m_print_i64((i8)(a),NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)2)) {
            msysc_m_print_startstr(str);
            msysc_m_print_i64((i16)(a),NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)3)) {
            msysc_m_print_startstr(str);
            msysc_m_print_i64((i32)(a),NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)4)) {
            msysc_m_print_startstr(str);
            msysc_m_print_i64(a,NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)6)) {
            msysc_m_print_startstr(str);
            msysc_m_print_u64((byte)(a),NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)7)) {
            msysc_m_print_startstr(str);
            msysc_m_print_u64((u16)(a),NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)8)) {
            msysc_m_print_startstr(str);
            msysc_m_print_u64((u32)(a),NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)9)) {
            msysc_m_print_startstr(str);
            msysc_m_print_u64((u64)(a),NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)13) || ((i64)(mm_decls_ttbasetype[(t)])==(i64)14) || ((i64)(mm_decls_ttbasetype[(t)])==(i64)15)) {
            msysc_m_print_startstr(str);
            msysc_m_print_str((byte*)"C64",NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)11)) {
            x32 = (float)((*p).xvalue);
            msysc_m_print_startstr(str);
            msysc_m_print_r64((double)(x32),NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)12)) {
            msysc_m_print_startstr(str);
            msysc_m_print_r64((*p).xvalue,NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)5) || ((i64)(mm_decls_ttbasetype[(t)])==(i64)10)) {
            msysc_m_print_startstr(str);
            msysc_m_print_str(mm_lib_strqvalue((*p).qvalue),NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)27)) {
            msysc_m_print_startstr(str);
            msysc_m_print_u64((*(*p).qvalue).lower,NULL);
            msysc_m_print_nogap();
            msysc_m_print_str((byte*)"..",NULL);
            msysc_m_print_nogap();
            msysc_m_print_i64((*(*p).qvalue).upper,NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)20)) {
            if (!!((*p).value)) {
                msysc_m_print_startstr(str);
                msysc_m_print_str((byte*)"#",NULL);
                msysc_m_print_nogap();
                msysc_m_print_i64((*p).value,NULL);
                msysc_m_print_i64((*p).slength,NULL);
                msysc_m_print_end();
                ;
            } else {
                msysc_m_print_startstr(str);
                msysc_m_print_str((byte*)"NIL",NULL);
                msysc_m_print_end();
                ;
            };
        } else {
            msysc_m_print_startcon();
            msysc_m_print_str(mm_lib_typename(t),NULL);
            msysc_m_print_str(mm_lib_typename((i64)(mm_decls_ttbasetype[(t)])),NULL);
            msysc_m_print_newline();
            msysc_m_print_end();
            ;
            mm_support_gerror((byte*)"STROPND CONST?",(struct mm_decls_unitrec *)(0));
        };
    };
    return str;
}

i64 mm_lib_gettypecat_t(i64 m) {
    return (i64)(mm_tables_stdtypecat[((i64)(mm_decls_ttbasetype[(m)]))]);
}

i64 mm_lib_getalignment(i64 m) {
    i64 a;
    if (((i64)(mm_decls_ttbasetype[(m)])==(i64)29)) {
        return mm_lib_getalignment((i64)(mm_decls_tttarget[(m)]));
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)32)) {
        return (i64)16;
    };
    a = (i64)(mm_decls_ttsize[(m)]);
    if ((a==(i64)1) || (a==(i64)2) || (a==(i64)4) || (a==(i64)8) || (a==(i64)16)) {
        return a;
    }else if ((a==(i64)0)) {
        return (i64)8;
    };
    msysc_m_print_startcon();
    msysc_m_print_str(mm_lib_strmode(m,(i64)1),NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    mm_support_gerror((byte*)"GETALIGN SIZE NOT 1248",(struct mm_decls_unitrec *)(0));
    return (i64)0;
}

i64 mm_lib_ispoweroftwo(i64 x) {
    i64 a;
    i64 n;
    i64 av_1;
    a = (i64)1;
    n = (i64)0;
    av_1 = (i64)30;
    while (av_1-- > 0) {
L357 :;
        ++n;
        a = (a << (i64)1);
        if ((a == x)) {
            return n;
        };
L358 :;
    }L359 :;
    ;
    return (i64)0;
}

void mm_lib_addlistunit(struct mm_decls_unitrec * * ulist,struct mm_decls_unitrec * * ulistx,struct mm_decls_unitrec * p) {
    if (((*ulist) == 0)) {
        (*ulist) = ((*ulistx) = p);
    } else {
        (*(*ulistx)).nextunit = p;
    };
    (*ulistx) = p;
}

i64 mm_lib_issimpletype(i64 m) {
    if (!!((u64)(mm_decls_ttisflexvar[(m)]))) {
        return (i64)0;
    };
    if (((i64)((u64)(mm_tables_stdtypecat[((i64)(mm_decls_ttbasetype[(m)]))])) == (i64)48)) {
        return (i64)0;
    };
    return (i64)1;
}

i64 mm_lib_getpacktype(i64 m) {
    i64 target;
    i64 mbase;
    struct mm_decls_strec *  d;
    if ((m == mm_tables_trefchar)) {
        return (i64)18;
    };
    target = (i64)(mm_decls_tttarget[(m)]);
    mbase = (i64)(mm_decls_ttbasetype[(m)]);
    if ((mbase <= (i64)12)) {
        if ((mbase==(i64)4)) {
            return (i64)1;
        }else if ((mbase==(i64)9)) {
            return (i64)2;
        }else if ((mbase==(i64)12)) {
            return (i64)3;
        }else if ((mbase==(i64)0)) {
            return (i64)0;
        } else {
            mm_support_gerror((byte*)"getpacktype1",(struct mm_decls_unitrec *)(0));
        };
    } else if (!!((u64)(mm_decls_ttisref[(m)]))) {
        if ((target>=(i64)0 && target<=(i64)12)) {
            return ((i64)4 + (target - (i64)0));
        };
        d = mm_decls_ttnamedef[(target)];
        if ((!!(d) && !!(mlib_eqstring((*d).name,(byte*)"varrec")))) {
            return (i64)19;
        };
    };
    return (i64)0;
}

void mm_lex_lexreadtoken(void) {
    i64 c;
    i64 hsum;
    i64 commentseen;
    mm_decls_nextlx.subcode = (u64)((i64)0);
    L360 :;
    switch ((i64)((*mm_lex_lxsptr++))) {
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
        mm_decls_nextlx.svalue = (mm_lex_lxsptr - (i64)1);
        //doname:
L362 :;
;
        hsum = (i64)((*mm_decls_nextlx.svalue));
        mm_decls_nextlx.hashvalue = (i64)0;
        L363 :;
        switch ((c = (i64)((*mm_lex_lxsptr++)))) {
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
            (*(mm_lex_lxsptr - (i64)1)) = (u64)((c + (i64)32));
            hsum = ((((hsum << (i64)4) - hsum) + c) + (i64)32);
        }break;
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
        }break;
        case 34:;
        {
            --mm_lex_lxsptr;
            if (((mm_decls_nextlx.svalue + (i64)1) == mm_lex_lxsptr)) {
                if (((i64)((*mm_decls_nextlx.svalue))==(i64)70) || ((i64)((*mm_decls_nextlx.svalue))==(i64)102) || ((i64)((*mm_decls_nextlx.svalue))==(i64)82) || ((i64)((*mm_decls_nextlx.svalue))==(i64)114)) {
                    mm_lex_readrawstring();
                    return;
                }else if (((i64)((*mm_decls_nextlx.svalue))==(i64)65) || ((i64)((*mm_decls_nextlx.svalue))==(i64)97) || ((i64)((*mm_decls_nextlx.svalue))==(i64)90) || ((i64)((*mm_decls_nextlx.svalue))==(i64)122)) {
                    mm_lex_readarraystring((i64)((*mm_decls_nextlx.svalue)));
                    return;
                };
            };
            goto L364 ;
        }break;
        default: {
            --mm_lex_lxsptr;
            goto L364 ;
        }
        } //SW
goto L363 ;
L364 :;
        ;
        mm_decls_nextlx.symbol = (u64)((i64)37);
        mm_decls_nextlx.length = (mm_lex_lxsptr - mm_decls_nextlx.svalue);
        mm_decls_nextlx.hashvalue = ((hsum << (i64)5) - hsum);
        return;
    }break;
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
        mm_decls_nextlx.svalue = (mm_lex_lxsptr - (i64)1);
        (*mm_decls_nextlx.svalue) += ' ';
        goto L362 ;
;
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
        c = (i64)((*(mm_lex_lxsptr - (i64)1)));
        if (((i64)((*mm_lex_lxsptr))==(i64)32) || ((i64)((*mm_lex_lxsptr))==(i64)41) || ((i64)((*mm_lex_lxsptr))==(i64)13) || ((i64)((*mm_lex_lxsptr))==(i64)44) || ((i64)((*mm_lex_lxsptr))==(i64)124)) {
            mm_decls_nextlx.symbol = (u64)((i64)40);
            mm_decls_nextlx.subcode = (u64)((i64)4);
            mm_decls_nextlx.value = (c - (i64)48);
        }else if (((i64)((*mm_lex_lxsptr))==(i64)120) || ((i64)((*mm_lex_lxsptr))==(i64)88)) {
            if ((c==(i64)48)) {
                ++mm_lex_lxsptr;
                mm_lex_readnumber((i64)16);
            }else if ((c==(i64)49)) {
                mm_support_lxerror((byte*)"Bad base");
            } else {
                ++mm_lex_lxsptr;
                mm_lex_readnumber((c - (i64)48));
            };
        } else {
            --mm_lex_lxsptr;
            mm_lex_readdecimalnumber();
        };
        return;
    }break;
    case 33:;
    {
        //docomment:
L365 :;
;
        L366 :;
        switch ((c = (i64)((*mm_lex_lxsptr++)))) {
        case 13:;
        {
            ++mm_lex_lxsptr;
            goto L367 ;
        }break;
        case 10:;
        {
            goto L367 ;
        }break;
        case 26:;
        case 0:;
        {
            --mm_lex_lxsptr;
            goto L367 ;
        }break;
        default: {
        }
        } //SW
goto L366 ;
L367 :;
        ;
        ++mm_decls_nextlx.lineno;
        ++mm_decls_nalllines;
        mm_decls_nextlx.symbol = (u64)((i64)35);
        return;
    }break;
    case 35:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)31);
        return;
    }break;
    case 92:;
    {
        commentseen = (i64)0;
        L368 :;
        switch ((i64)((*mm_lex_lxsptr++))) {
        case 13:;
        {
            ++mm_decls_nalllines;
            ++mm_decls_nextlx.lineno;
            ++mm_lex_lxsptr;
            goto L369 ;
        }break;
        case 10:;
        {
            ++mm_decls_nalllines;
            ++mm_decls_nextlx.lineno;
            goto L369 ;
        }break;
        case 26:;
        case 0:;
        {
            mm_decls_nextlx.symbol = (u64)((i64)36);
            --mm_lex_lxsptr;
            return;
        }break;
        case 32:;
        case 9:;
        {
        }break;
        case 33:;
        {
            commentseen = (i64)1;
        }break;
        default: {
            if (!(!!(commentseen))) {
                mm_support_lxerror((byte*)"\\ not followed by eol");
            };
        }
        } //SW
goto L368 ;
L369 :;
        ;
        L370 :;
        switch ((i64)((*mm_lex_lxsptr++))) {
        case 13:;
        {
            ++mm_decls_nextlx.lineno;
            ++mm_decls_nalllines;
            ++mm_lex_lxsptr;
        }break;
        case 10:;
        {
            ++mm_decls_nalllines;
            ++mm_decls_nextlx.lineno;
        }break;
        case 32:;
        case 9:;
        {
        }break;
        default: {
            --mm_lex_lxsptr;
            goto L371 ;
        }
        } //SW
goto L370 ;
L371 :;
        ;
    }break;
    case 123:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)16);
        return;
    }break;
    case 125:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)17);
        return;
    }break;
    case 46:;
    {
        switch ((i64)((*mm_lex_lxsptr))) {
        case 46:;
        {
            ++mm_lex_lxsptr;
            if (((u64)((*mm_lex_lxsptr)) == '.')) {
                ++mm_lex_lxsptr;
                mm_decls_nextlx.symbol = (u64)((i64)30);
            } else {
                mm_decls_nextlx.symbol = (u64)((i64)29);
                mm_decls_nextlx.subcode = (u64)((i64)15);
            };
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
            --mm_lex_lxsptr;
            mm_lex_readrealnumber((byte *)(0),(i64)0,(i64)10);
            return;
        }break;
        default: {
            mm_decls_nextlx.symbol = (u64)((i64)2);
            return;
        }
        } //SW
;
    }break;
    case 44:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)5);
        return;
    }break;
    case 59:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)6);
        return;
    }break;
    case 58:;
    {
        switch ((i64)((*mm_lex_lxsptr))) {
        case 61:;
        {
            ++mm_lex_lxsptr;
            mm_decls_nextlx.symbol = (u64)((i64)9);
            mm_decls_nextlx.subcode = (u64)((i64)23);
        }break;
        case 58:;
        {
            ++mm_lex_lxsptr;
            if (((i64)((*mm_lex_lxsptr))==(i64)61)) {
                ++mm_lex_lxsptr;
                mm_decls_nextlx.symbol = (u64)((i64)10);
                mm_decls_nextlx.subcode = (u64)((i64)24);
            } else {
                mm_decls_nextlx.symbol = (u64)((i64)8);
            };
        }break;
        default: {
            mm_decls_nextlx.symbol = (u64)((i64)7);
        }
        } //SW
;
        return;
    }break;
    case 40:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)12);
        return;
    }break;
    case 41:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)13);
        return;
    }break;
    case 91:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)14);
        return;
    }break;
    case 93:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)15);
        return;
    }break;
    case 124:;
    {
        if (((u64)((*mm_lex_lxsptr)) == '|')) {
            ++mm_lex_lxsptr;
            mm_decls_nextlx.symbol = (u64)((i64)20);
        } else {
            mm_decls_nextlx.symbol = (u64)((i64)19);
        };
        return;
    }break;
    case 94:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)18);
        return;
    }break;
    case 64:;
    {
        if (((u64)((*mm_lex_lxsptr)) == '@')) {
            ++mm_lex_lxsptr;
            mm_decls_nextlx.symbol = (u64)((i64)22);
        } else {
            mm_decls_nextlx.symbol = (u64)((i64)21);
        };
        return;
    }break;
    case 63:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)23);
        return;
    }break;
    case 126:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)27);
        return;
    }break;
    case 43:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)32);
        if (((u64)((*mm_lex_lxsptr)) == '+')) {
            ++mm_lex_lxsptr;
            mm_decls_nextlx.symbol = (u64)((i64)39);
            mm_decls_nextlx.subcode = (u64)((i64)138);
            return;
        } else {
            mm_decls_nextlx.subcode = (u64)((i64)37);
        };
        return;
    }break;
    case 45:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)32);
        if (((u64)((*mm_lex_lxsptr)) == '-')) {
            ++mm_lex_lxsptr;
            mm_decls_nextlx.symbol = (u64)((i64)39);
            mm_decls_nextlx.subcode = (u64)((i64)139);
            return;
        } else {
            mm_decls_nextlx.subcode = (u64)((i64)38);
        };
        return;
    }break;
    case 42:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)32);
        if (((u64)((*mm_lex_lxsptr)) == '*')) {
            ++mm_lex_lxsptr;
            mm_decls_nextlx.subcode = (u64)((i64)92);
        } else {
            mm_decls_nextlx.subcode = (u64)((i64)39);
        };
        return;
    }break;
    case 47:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)32);
        mm_decls_nextlx.subcode = (u64)((i64)40);
        return;
    }break;
    case 37:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)32);
        mm_decls_nextlx.subcode = (u64)((i64)41);
        return;
    }break;
    case 61:;
    {
        if (((i64)((*mm_lex_lxsptr))==(i64)62)) {
            mm_decls_nextlx.symbol = (u64)((i64)11);
            ++mm_lex_lxsptr;
        }else if (((i64)((*mm_lex_lxsptr))==(i64)61)) {
            mm_decls_nextlx.symbol = (u64)((i64)32);
            mm_decls_nextlx.subcode = (u64)((i64)36);
            ++mm_lex_lxsptr;
        } else {
            mm_decls_nextlx.symbol = (u64)((i64)32);
            mm_decls_nextlx.subcode = (u64)((i64)30);
        };
        return;
    }break;
    case 60:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)32);
        switch ((i64)((*mm_lex_lxsptr))) {
        case 61:;
        {
            ++mm_lex_lxsptr;
            mm_decls_nextlx.subcode = (u64)((i64)33);
        }break;
        case 62:;
        {
            ++mm_lex_lxsptr;
            mm_decls_nextlx.subcode = (u64)((i64)31);
        }break;
        case 60:;
        {
            ++mm_lex_lxsptr;
            mm_decls_nextlx.subcode = (u64)((i64)47);
        }break;
        default: {
            mm_decls_nextlx.subcode = (u64)((i64)32);
        }
        } //SW
;
        return;
    }break;
    case 62:;
    {
        mm_decls_nextlx.symbol = (u64)((i64)32);
        switch ((i64)((*mm_lex_lxsptr))) {
        case 61:;
        {
            ++mm_lex_lxsptr;
            mm_decls_nextlx.subcode = (u64)((i64)35);
        }break;
        case 62:;
        {
            ++mm_lex_lxsptr;
            mm_decls_nextlx.subcode = (u64)((i64)48);
        }break;
        default: {
            mm_decls_nextlx.subcode = (u64)((i64)34);
        }
        } //SW
;
        return;
    }break;
    case 38:;
    {
        if (((i64)((*mm_lex_lxsptr))==(i64)38)) {
            ++mm_lex_lxsptr;
            mm_decls_nextlx.symbol = (u64)((i64)32);
            mm_decls_nextlx.subcode = (u64)((i64)29);
        }else if (((i64)((*mm_lex_lxsptr))==(i64)46)) {
            ++mm_lex_lxsptr;
            mm_decls_nextlx.symbol = (u64)((i64)4);
            mm_decls_nextlx.subcode = (u64)((i64)0);
        } else {
            mm_decls_nextlx.symbol = (u64)((i64)24);
            mm_decls_nextlx.subcode = (u64)((i64)94);
        };
        return;
    }break;
    case 39:;
    case 96:;
    {
        mm_lex_lxreadstring((i64)39);
        return;
    }break;
    case 34:;
    {
        mm_lex_lxreadstring((i64)34);
        return;
    }break;
    case 32:;
    case 9:;
    {
    }break;
    case 13:;
    {
        ++mm_lex_lxsptr;
        ++mm_decls_nextlx.lineno;
        ++mm_decls_nalllines;
        mm_decls_nextlx.symbol = (u64)((i64)35);
        return;
    }break;
    case 10:;
    {
        ++mm_decls_nextlx.lineno;
        ++mm_decls_nalllines;
        mm_decls_nextlx.symbol = (u64)((i64)35);
        return;
    }break;
    case 26:;
    case 0:;
    {
        if (!!(mm_lex_sourcelevel)) {
            mm_lex_unstacksource();
        } else {
            mm_decls_nextlx.symbol = (u64)((i64)36);
            --mm_lex_lxsptr;
            return;
        };
    }break;
    default: {
        mm_decls_nextlx.symbol = (u64)((i64)1);
        return;
    }
    } //SW
goto L360 ;
L361 :;
    ;
}

static void mm_lex_lxreadstring(i64 termchar) {
    static byte psname[256];
    byte *  dest;
    i64 c;
    i64 d;
    byte str[8];
    i64 av_1;
    if ((termchar == (i64)34)) {
        mm_decls_nextlx.symbol = (u64)((i64)45);
    } else {
        mm_decls_nextlx.symbol = (u64)((i64)43);
        mm_decls_nextlx.subcode = (u64)((i64)4);
    };
    if (!(!!(mm_decls_prescanmode))) {
        dest = mm_lex_lxsptr;
    } else {
        dest = psname;
    };
    mm_decls_nextlx.svalue = dest;
    L372 :;
    while (1) {
        switch ((c = (i64)((*mm_lex_lxsptr++)))) {
        case 92:;
        {
            c = (i64)((*mm_lex_lxsptr));
            if (((c >= (i64)65) && (c <= (i64)90))) {
                c += (i64)32;
            };
            ++mm_lex_lxsptr;
            switch (c) {
            case 97:;
            {
                c = (i64)7;
            }break;
            case 98:;
            {
                c = (i64)8;
            }break;
            case 99:;
            case 114:;
            {
                c = (i64)13;
            }break;
            case 101:;
            {
                c = (i64)26;
            }break;
            case 102:;
            {
                c = (i64)12;
            }break;
            case 108:;
            case 110:;
            {
                c = (i64)10;
            }break;
            case 115:;
            {
                c = (i64)27;
            }break;
            case 116:;
            {
                c = (i64)9;
            }break;
            case 118:;
            {
                c = (i64)11;
            }break;
            case 119:;
            {
                (*dest++) = (u64)13u;
                c = (i64)10;
            }break;
            case 120:;
            {
                c = (i64)0;
                av_1 = (i64)2;
                while (av_1-- > 0) {
L374 :;
                    if (((d = (i64)((*mm_lex_lxsptr++)))==(i64)65) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)66) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)67) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)68) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)69) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)70)) {
                        c = ((((c * (i64)16) + d) - (i64)65) + (i64)10);
                    }else if (((d = (i64)((*mm_lex_lxsptr++)))==(i64)97) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)98) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)99) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)100) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)101) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)102)) {
                        c = ((((c * (i64)16) + d) - (i64)97) + (i64)10);
                    }else if (((d = (i64)((*mm_lex_lxsptr++)))==(i64)48) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)49) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)50) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)51) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)52) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)53) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)54) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)55) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)56) || ((d = (i64)((*mm_lex_lxsptr++)))==(i64)57)) {
                        c = (((c * (i64)16) + d) - (i64)48);
                    } else {
                        mm_support_lxerror((byte*)"Bad \\x code");
                    };
L375 :;
                }L376 :;
                ;
            }break;
            case 121:;
            {
                c = (i64)16;
            }break;
            case 122:;
            case 48:;
            {
                c = (i64)0;
            }break;
            case 34:;
            case 81:;
            {
                c = (i64)34;
            }break;
            case 92:;
            {
                c = (i64)92;
            }break;
            case 39:;
            {
                c = (i64)39;
            }break;
            default: {
                str[((i64)1)-1] = (u64)(c);
                str[((i64)2)-1] = (u64)0u;
                mm_support_lxerror_s((byte*)"Unknown string escape: \\%s",str);
            }
            } //SW
;
        }break;
        case 34:;
        case 39:;
        {
            if ((c == termchar)) {
                if (((i64)((*mm_lex_lxsptr)) == c)) {
                    ++mm_lex_lxsptr;
                } else {
                    goto L373 ;
                };
            };
        }break;
        case 13:;
        case 10:;
        case 26:;
        case 0:;
        {
            mm_support_lxerror((byte*)"String not terminated");
        }break;
        default: {
        }
        } //SW
;
        if (!(!!(mm_decls_prescanmode))) {
            (*dest++) = (u64)(c);
        } else {
            if (((dest - mm_decls_nextlx.svalue) < (i64)251)) {
                (*dest++) = (u64)(c);
            };
        };
    }L373 :;
    ;
    mm_decls_nextlx.length = (dest - mm_decls_nextlx.svalue);
    (*(mm_decls_nextlx.svalue + (i64)(mm_decls_nextlx.length))) = (u64)0u;
}

static void mm_lex_readnumber(i64 base) {
    byte *  pstart;
    byte *  dest;
    i64 c;
    dest = (pstart = mm_lex_lxsptr);
    if ((base == (i64)10)) {
        L377 :;
        switch ((c = (i64)((*mm_lex_lxsptr++)))) {
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
            (*dest++) = (u64)(c);
        }break;
        case 95:;
        case 39:;
        case 96:;
        {
        }break;
        default: {
            --mm_lex_lxsptr;
            goto L378 ;
        }
        } //SW
goto L377 ;
L378 :;
        ;
    } else {
        dest = mm_lex_scannumber(base);
        c = (i64)((*mm_lex_lxsptr));
    };
    switch (c) {
    case 46:;
    {
        if (((u64)((*(mm_lex_lxsptr + (i64)1))) != '.')) {
            mm_lex_readrealnumber(pstart,(dest - pstart),base);
            return;
        };
    }break;
    case 101:;
    case 69:;
    {
        if ((base < (i64)15)) {
            mm_lex_readrealnumber(pstart,(dest - pstart),base);
            return;
        };
    }break;
    case 112:;
    case 80:;
    {
        if ((base >= (i64)15)) {
            mm_lex_readrealnumber(pstart,(dest - pstart),base);
            return;
        };
    }break;
    default: {
    }
    } //SW
;
    mm_lex_stringtonumber(pstart,(dest - pstart),base);
}

static void mm_lex_readdecimalnumber(void) {
    byte *  pstart;
    byte *  dest;
    i64 c;
    i64 n;
    i64 base;
    i64 suffix;
    byte *  p;
    i64 av_1;
    dest = (pstart = mm_lex_lxsptr);
    suffix = (i64)0;
    L379 :;
    switch ((c = (i64)((*mm_lex_lxsptr++)))) {
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
        (*dest++) = (u64)(c);
    }break;
    case 95:;
    case 39:;
    case 96:;
    {
    }break;
    default: {
        --mm_lex_lxsptr;
        goto L380 ;
    }
    } //SW
goto L379 ;
L380 :;
    ;
    switch (c) {
    case 46:;
    {
        if (((u64)((*(mm_lex_lxsptr + (i64)1))) != '.')) {
            mm_lex_readrealnumber(pstart,(dest - pstart),(i64)10);
            return;
        };
    }break;
    case 101:;
    case 69:;
    {
        mm_lex_readrealnumber(pstart,(dest - pstart),(i64)10);
        return;
    }break;
    case 98:;
    case 66:;
    {
        ++mm_lex_lxsptr;
        n = (dest - pstart);
        p = pstart;
        av_1 = n;
        while (av_1-- > 0) {
L381 :;
            if ((((u64)((*p)) < '0') || ((u64)((*p)) > '1'))) {
                mm_support_lxerror((byte*)"1101B: bad digit");
            };
            ++p;
L382 :;
        }L383 :;
        ;
        mm_lex_stringtonumber(pstart,n,(i64)2);
        return;
    }break;
    case 120:;
    case 88:;
    {
        ++mm_lex_lxsptr;
        mm_lex_stringtodecimalnumber(pstart,(dest - pstart),(i64)0);
        base = mm_decls_nextlx.value;
        if ((base > (i64)16)) {
            mm_support_lxerror((byte*)"Number base over 16");
        };
        mm_lex_readnumber(base);
        return;
    }break;
    case 108:;
    case 76:;
    {
        suffix = c;
        ++mm_lex_lxsptr;
    }break;
    case 119:;
    case 87:;
    {
        suffix = c;
        ++mm_lex_lxsptr;
    }break;
    default: {
    }
    } //SW
;
    mm_lex_stringtodecimalnumber(pstart,(dest - pstart),suffix);
}

static void mm_lex_readrealnumber(byte * intstart,i64 intlen,i64 base) {
    byte *  fractstart;
    byte *  ss;
    i64 fractlen;
    i64 expon;
    i64 i;
    i64 c;
    double basex;
    double x;
    byte realstr[500];
    byte str[32];
    i64 av_1;
    i64 av_2;
    i64 av_3;
    fractstart = (byte *)(0);
    fractlen = (i64)0;
    expon = (i64)0;
    mm_lex_longsuffix = (i64)0;
    if (((u64)((*mm_lex_lxsptr)) == '.')) {
        fractstart = ++mm_lex_lxsptr;
        fractlen = (mm_lex_scannumber(base) - fractstart);
    };
    if (((i64)((*mm_lex_lxsptr))==(i64)101) || ((i64)((*mm_lex_lxsptr))==(i64)69)) {
        if ((base < (i64)15)) {
            ++mm_lex_lxsptr;
            expon = mm_lex_readexponent(base);
        };
    }else if (((i64)((*mm_lex_lxsptr))==(i64)112) || ((i64)((*mm_lex_lxsptr))==(i64)80)) {
        if ((base >= (i64)15)) {
            ++mm_lex_lxsptr;
            expon = mm_lex_readexponent(base);
        };
    }else if (((i64)((*mm_lex_lxsptr))==(i64)108) || ((i64)((*mm_lex_lxsptr))==(i64)76)) {
        if (!!(mm_lex_longsuffix)) {
            mm_support_lxerror((byte*)"LL?");
        };
        mm_lex_longsuffix = (i64)76;
        ++mm_lex_lxsptr;
    };
    if ((mm_lex_longsuffix == (i64)76)) {
        ss = (byte *)(mlib_pcm_alloc(((intlen + fractlen) + (i64)16)));
        memcpy((void *)(ss),(void *)(intstart),(u64)(intlen));
        memcpy((void *)((ss + intlen)),(void *)((byte*)"."),(u64)((i64)1));
        memcpy((void *)(((ss + intlen) + (i64)1)),(void *)(fractstart),(u64)(fractlen));
        memcpy((void *)((((ss + intlen) + fractlen) + (i64)1)),(void *)((byte*)"e"),(u64)((i64)1));
        msysc_getstrint(expon,str);
        memcpy((void *)((((ss + intlen) + fractlen) + (i64)2)),(void *)(str),(u64)(((i64)(strlen((i8 *)(str))) + (i64)1)));
        mm_decls_nextlx.symbol = (u64)((i64)41);
        mm_decls_nextlx.subcode = (u64)((i64)16);
        mm_decls_nextlx.svalue = ss;
        mm_decls_nextlx.length = (i64)(strlen((i8 *)(ss)));
        return;
    };
    if (((intlen + fractlen) > (i64)500)) {
        mm_support_lxerror((byte*)"Real too long");
    };
    if (!!(intlen)) {
        memcpy((void *)(&realstr),(void *)(intstart),(u64)(intlen));
    };
    if (!!(fractlen)) {
        memcpy((void *)((&realstr[((i64)1)-1] + intlen)),(void *)(fractstart),(u64)(fractlen));
    };
    if ((base == (i64)10)) {
        x = mm_lex_readrealbest(intlen,fractlen,expon,realstr);
    } else {
        basex = (double)(base);
        expon -= fractlen;
        x = (double)0.;
        L384 :;
        for (i=(i64)1;i<=(intlen + fractlen);++i) {
L385 :;
            c = (i64)(realstr[(i)-1]);
            if (((c >= (i64)48) && (c <= (i64)57))) {
                x = (((x * basex) + (double)(c)) - (double)48.);
            } else if ((c > (i64)97)) {
                x = ((((x * basex) + (double)(c)) - (double)97.) + (double)10.);
            } else {
                x = ((((x * basex) + (double)(c)) - (double)65.) + (double)10.);
            };
L386 :;
        }L387 :;
        ;
        if ((expon >= (i64)0)) {
            av_2 = expon;
            while (av_2-- > 0) {
L388 :;
                x *= basex;
L389 :;
            }L390 :;
            ;
        } else {
            av_3 = -(expon);
            while (av_3-- > 0) {
L391 :;
                x /= basex;
L392 :;
            }L393 :;
            ;
        };
    };
    mm_decls_nextlx.symbol = (u64)((i64)42);
    mm_decls_nextlx.subcode = (u64)((i64)12);
    mm_decls_nextlx.xvalue = x;
}

static double mm_lex_readrealbest(i64 intlen,i64 fractlen,i64 expon,byte * realstr) {
    byte expstr[32];
    (*((realstr + intlen) + fractlen)) = (u64)0u;
    expon -= fractlen;
    msysc_m_print_startstr(expstr);
    msysc_m_print_str((byte*)"e",NULL);
    msysc_m_print_nogap();
    msysc_m_print_i64(expon,NULL);
    msysc_m_print_end();
    ;
    strcat((i8 *)(realstr),(i8 *)(expstr));
    return strtod((i8 *)(realstr),0);
}

static i64 mm_lex_readexponent(i64 base) {
    byte *  numstart;
    i64 length;
    i64 neg;
    neg = (i64)0;
    if (((i64)((*mm_lex_lxsptr))==(i64)43)) {
        ++mm_lex_lxsptr;
    }else if (((i64)((*mm_lex_lxsptr))==(i64)45)) {
        ++mm_lex_lxsptr;
        neg = (i64)1;
    };
    numstart = mm_lex_lxsptr;
    length = (mm_lex_scannumber(base) - numstart);
    if ((length == (i64)0)) {
        mm_support_lxerror((byte*)"Bad expon");
    };
    mm_lex_stringtonumber(numstart,length,base);
    return (!!(neg)?-(mm_decls_nextlx.value):mm_decls_nextlx.value);
}

void mm_lex_printsymbol(struct mm_decls_lexrec * lp) {
    struct mm_decls_lexrec l;
    memcpy(&l,lp,32);
    printf((i8 *)((byte*)"%-18s"),mm_tables_symbolnames[((i64)(l.symbol))-1]);
    if (((i64)(l.symbol)==(i64)37)) {
        mm_lex_printstrn(l.svalue,(i64)(l.length));
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)" (",NULL);
        msysc_m_print_nogap();
        msysc_m_print_i64(l.hashvalue,NULL);
        msysc_m_print_nogap();
        msysc_m_print_str((byte*)")",NULL);
        msysc_m_print_end();
        ;
    }else if (((i64)(l.symbol)==(i64)50)) {
        mm_lex_printstrn((*l.symptr).name,(i64)((*l.symptr).namelen));
    }else if (((i64)(l.symbol)==(i64)40)) {
        if (((i64)(l.subcode)==(i64)4)) {
            msysc_m_print_startcon();
            msysc_m_print_i64(l.value,NULL);
            msysc_m_print_str((byte*)"int",NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(l.subcode)==(i64)9)) {
            msysc_m_print_startcon();
            msysc_m_print_u64(l.uvalue,NULL);
            msysc_m_print_str((byte*)"word",NULL);
            msysc_m_print_end();
            ;
        } else {
            msysc_m_print_startcon();
            msysc_m_print_i64(l.value,NULL);
            msysc_m_print_end();
            ;
        };
    }else if (((i64)(l.symbol)==(i64)42)) {
        msysc_m_print_startcon();
        msysc_m_print_r64(l.xvalue,NULL);
        msysc_m_print_end();
        ;
    }else if (((i64)(l.symbol)==(i64)45)) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"\"",NULL);
        msysc_m_print_end();
        ;
        mm_lex_printstrn(l.svalue,(i64)(l.length));
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"\"",NULL);
        msysc_m_print_end();
        ;
    }else if (((i64)(l.symbol)==(i64)43)) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"'",NULL);
        msysc_m_print_end();
        ;
        mm_lex_printstrn(l.svalue,(i64)(l.length));
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"'",NULL);
        msysc_m_print_end();
        ;
    }else if (((i64)(l.symbol)==(i64)41)) {
        mm_lex_printstrn(l.svalue,(i64)(l.length));
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"L",NULL);
        msysc_m_print_end();
        ;
    }else if (((i64)(l.symbol)==(i64)32) || ((i64)(l.symbol)==(i64)9) || ((i64)(l.symbol)==(i64)24) || ((i64)(l.symbol)==(i64)18) || ((i64)(l.symbol)==(i64)10) || ((i64)(l.symbol)==(i64)29)) {
        msysc_m_print_startcon();
        msysc_m_print_str(mm_tables_jtagnames[((i64)(l.subcode))],NULL);
        msysc_m_print_end();
        ;
    } else {
        if (!!((u64)(l.subcode))) {
            msysc_m_print_startcon();
            msysc_m_print_str((byte*)"#",NULL);
            msysc_m_print_u64(l.subcode,NULL);
            msysc_m_print_end();
            ;
        };
    };
    msysc_m_print_startcon();
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
}

static void mm_lex_stringtonumber(byte * s,i64 length,i64 base) {
    i64 a;
    i64 c;
    i64 av_1;
    i64 av_2;
    L394 :;
    while (((length >= (i64)2) && ((u64)((*s)) == '0'))) {
        ++s;
        --length;
L395 :;
    }L396 :;
    ;
    mm_decls_nextlx.symbol = (u64)((i64)40);
    if (((length > mm_lex_maxnumlen[(base)-1]) || ((length == mm_lex_maxnumlen[(base)-1]) && ((i64)(strncmp((i8 *)(s),(i8 *)(mm_lex_maxnumlist[(base)-1]),(u64)(length))) > (i64)0)))) {
        if ((base != (i64)16)) {
            mm_support_lxerror((byte*)"longint const");
        } else {
            if (((length > (i64)32) || ((length == (i64)32) && ((i64)(strncmp((i8 *)(s),(i8 *)((byte*)"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"),(u64)((i64)32))) > (i64)0)))) {
                mm_support_lxerror((byte*)"longint const");
            } else {
                if (((length == (i64)32) && ((i64)(strncmp((i8 *)(s),(i8 *)((byte*)"7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"),(u64)((i64)32))) > (i64)0))) {
                    mm_decls_nextlx.subcode = (u64)((i64)10);
                } else {
                    mm_decls_nextlx.subcode = (u64)((i64)5);
                };
                mm_decls_nextlx.qvalue = mm_lex_stringtonumber128(s,length,(i64)16);
            };
        };
        return;
    };
    a = (i64)0;
    if ((base <= (i64)10)) {
        av_1 = length;
        while (av_1-- > 0) {
L397 :;
            a = (((a * base) + (i64)((*s++))) - (i64)48);
L398 :;
        }L399 :;
        ;
    } else {
        av_2 = length;
        while (av_2-- > 0) {
L400 :;
            c = (i64)((*s++));
            if ((c >= (i64)97)) {
                a = ((((a * base) + c) - (i64)97) + (i64)10);
            } else if ((c >= (i64)65)) {
                a = ((((a * base) + c) - (i64)65) + (i64)10);
            } else {
                a = (((a * base) + c) - (i64)48);
            };
L401 :;
        }L402 :;
        ;
    };
    mm_decls_nextlx.value = a;
    mm_decls_nextlx.subcode = (u64)(mm_lex_setinttype((u64)(a)));
}

static void mm_lex_stringtodecimalnumber(byte * s,i64 length,i64 suffix) {
    i64 a;
    i64 av_1;
    L403 :;
    while (((length >= (i64)2) && ((u64)((*s)) == '0'))) {
        ++s;
        --length;
L404 :;
    }L405 :;
    ;
    mm_decls_nextlx.symbol = (u64)((i64)40);
    if ((((length > (i64)20) || ((length == (i64)20) && ((i64)(strncmp((i8 *)(s),(i8 *)((byte*)"18446744073709551615"),(u64)((i64)20))) > (i64)0))) || !!(suffix))) {
        if (((length > (i64)39) || ((length == (i64)39) && ((i64)(strncmp((i8 *)(s),(i8 *)((byte*)"340282366920938463463374607431768211455"),(u64)((i64)39))) > (i64)0)))) {
            if ((suffix == (i64)87)) {
                mm_support_lxerror((byte*)"-W overflows 128 bits");
            };
            //dolongint:
L406 :;
;
            mm_decls_nextlx.symbol = (u64)((i64)41);
            mm_decls_nextlx.subcode = (u64)((i64)16);
            mm_decls_nextlx.svalue = mlib_pcm_copyheapstring(s);
            mm_decls_nextlx.length = length;
        } else {
            if ((suffix == (i64)76)) {
                goto L406 ;
;
            };
            if (((length == (i64)39) && ((i64)(strncmp((i8 *)(s),(i8 *)((byte*)"170141183460469231731687303715884105727"),(u64)((i64)39))) > (i64)0))) {
                mm_decls_nextlx.subcode = (u64)((i64)10);
            } else {
                mm_decls_nextlx.subcode = (u64)((i64)5);
            };
            mm_decls_nextlx.qvalue = mm_lex_stringtonumber128(s,length,(i64)10);
        };
        return;
    };
    a = (i64)0;
    av_1 = length;
    while (av_1-- > 0) {
L407 :;
        a = (((a * (i64)10) + (i64)((*s++))) - (i64)48);
L408 :;
    }L409 :;
    ;
    mm_decls_nextlx.value = a;
    mm_decls_nextlx.subcode = (u64)(mm_lex_setinttype((u64)(a)));
}

void mm_lex_lexsetup(void) {
    i64 i;
    i64 av_1;
    L410 :;
    for (i=(i64)1;i<=(i64)16;++i) {
L411 :;
        mm_lex_maxnumlen[(i)-1] = (i64)(strlen((i8 *)(mm_lex_maxnumlist[(i)-1])));
L412 :;
    }L413 :;
    ;
    mm_lex_inithashtable();
}

void mm_lex_printstrn(byte * s,i64 length) {
    if (!!(length)) {
        msysc_m_print_startcon();
        msysc_m_print_i64(length,(byte*)"v");
        msysc_m_print_str(s,(byte*)".*");
        msysc_m_print_end();
        ;
    };
}

static byte * mm_lex_scannumber(i64 base) {
    byte *  dest;
    i64 c;
    dest = mm_lex_lxsptr;
    L414 :;
    switch ((c = (i64)((*mm_lex_lxsptr++)))) {
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
        (*dest++) = (u64)(c);
        if ((c >= ((i64)48 + base))) {
            mm_support_lxerror((byte*)"Digit out of range");
        };
    }break;
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
        if ((base == (i64)16)) {
            (*dest++) = (u64)(c);
        } else {
            --mm_lex_lxsptr;
            goto L415 ;
        };
    }break;
    case 95:;
    case 39:;
    case 96:;
    {
    }break;
    case 108:;
    case 76:;
    {
        mm_lex_longsuffix = (i64)76;
        goto L415 ;
    }break;
    default: {
        --mm_lex_lxsptr;
        goto L415 ;
    }
    } //SW
goto L414 ;
L415 :;
    ;
    return dest;
}

static void mm_lex_readrawstring(void) {
    byte *  dest;
    i64 c;
    mm_decls_nextlx.symbol = (u64)((i64)45);
    mm_decls_nextlx.svalue = ++mm_lex_lxsptr;
    dest = mm_lex_lxsptr;
    L416 :;
    switch ((c = (i64)((*mm_lex_lxsptr++)))) {
    case 34:;
    {
        if (((u64)((*mm_lex_lxsptr)) == '"')) {
            (*dest++) = '"';
            ++mm_lex_lxsptr;
        } else {
            (*(mm_lex_lxsptr - (i64)1)) = (u64)0u;
            goto L417 ;
        };
    }break;
    case 13:;
    case 10:;
    case 26:;
    case 0:;
    {
        mm_support_lxerror((byte*)"Raw string not terminated");
        --mm_lex_lxsptr;
        goto L417 ;
    }break;
    default: {
        (*dest++) = (u64)(c);
    }
    } //SW
goto L416 ;
L417 :;
    ;
    mm_decls_nextlx.length = (dest - mm_decls_nextlx.svalue);
}

static i64 mm_lex_lookup(void) {
    i64 j;
    i64 wrapped;
    j = ((i64)(mm_decls_nextlx.hashvalue) & (i64)32767);
    mm_decls_nextlx.symptr = &mm_lex_hashtable[(j)];
    wrapped = (i64)0;
    ++mm_lex_nlookups;
    L418 :;
    while (1) {
        if (((i64)((*mm_decls_nextlx.symptr).namelen)==(i64)0)) {
            goto L419 ;
        }else if (((i64)((*mm_decls_nextlx.symptr).namelen)==(i64)(mm_decls_nextlx.length))) {
            if (((i64)(memcmp((void *)((*mm_decls_nextlx.symptr).name),(void *)(mm_decls_nextlx.svalue),(u64)(mm_decls_nextlx.length))) == (i64)0)) {
                return (i64)1;
            };
        };
        ++mm_lex_nclashes;
        ++mm_decls_nextlx.symptr;
        if ((++j >= (i64)32768)) {
            if (!!(wrapped)) {
                mlib_abortprogram((byte*)"HASHTABLE FULL");
            };
            wrapped = (i64)1;
            mm_decls_nextlx.symptr = &mm_lex_hashtable[((i64)0)];
            j = (i64)0;
        };
    }L419 :;
    ;
    (*mm_decls_nextlx.symptr).name = mm_decls_nextlx.svalue;
    (*mm_decls_nextlx.symptr).namelen = (u64)(mm_decls_nextlx.length);
    (*mm_decls_nextlx.symptr).symbol = (u64)((i64)37);
    return (i64)0;
}

i64 mm_lex_gethashvaluez(byte * s) {
    i64 c;
    i64 hsum;
    if (((i64)((*s)) == (i64)0)) {
        return (i64)0;
    };
    hsum = (i64)((*s++));
    L420 :;
    while (1) {
        c = (i64)((*s++));
        if ((c == (i64)0)) {
            goto L421 ;
        };
        hsum = (((hsum << (i64)4) - hsum) + c);
    }L421 :;
    ;
    return ((hsum << (i64)5) - hsum);
}

static void mm_lex_inithashtable(void) {
    i64 i;
    i64 av_1;
    memset((void *)(&mm_lex_hashtable),(i64)0,(u64)((i64)4915200));
    L422 :;
    for (i=(i64)1;i<=(i64)403;++i) {
L423 :;
        mm_decls_nextlx.svalue = mlib_pcm_copyheapstring(mm_tables_stnames[(i)-1]);
        mm_decls_nextlx.length = (i64)(strlen((i8 *)(mm_decls_nextlx.svalue)));
        mm_decls_nextlx.hashvalue = mm_lex_gethashvaluez(mm_decls_nextlx.svalue);
        if (!!(mm_lex_lookup())) {
            msysc_m_print_startcon();
            msysc_m_print_str(mm_tables_stnames[(i)-1],NULL);
            msysc_m_print_newline();
            msysc_m_print_end();
            ;
            mlib_abortprogram((byte*)"Duplicate symbol table entry");
        };
        (*mm_decls_nextlx.symptr).symbol = (u64)(mm_tables_stsymbols[(i)-1]);
        if ((mm_tables_stsymbols[(i)-1]==(i64)49)) {
            (*mm_decls_nextlx.symptr).index = mm_tables_stsubcodes[(i)-1];
            (*mm_decls_nextlx.symptr).subcode = (i64)49;
            (*mm_decls_nextlx.symptr).symbol = (u64)((i64)37);
        }else if ((mm_tables_stsymbols[(i)-1]==(i64)52)) {
            (*mm_decls_nextlx.symptr).index = (i64)0;
            (*mm_decls_nextlx.symptr).subcode = (i64)52;
            (*mm_decls_nextlx.symptr).symbol = (u64)((i64)37);
        } else {
            (*mm_decls_nextlx.symptr).subcode = mm_tables_stsubcodes[(i)-1];
        };
L424 :;
    }L425 :;
    ;
}

static i64 mm_lex_dolexdirective(i64 index) {
    byte *  file;
    i64 fileno;
    if ((index==(i64)11) || (index==(i64)12)) {
        mm_lex_lexreadtoken();
        if (((i64)((u64)(mm_decls_nextlx.symbol)) != (i64)45)) {
            mm_support_lxerror((byte*)"strincl: string expected");
        };
        file = mm_decls_nextlx.svalue;
        fileno = mm_support_getsupportfile(file);
        mm_decls_nextlx.svalue = mm_decls_sourcefiletext[(fileno)];
        mm_decls_nextlx.length = mm_decls_sourcefilesizes[(fileno)];
        mm_decls_nextlx.symbol = ((index == (i64)11)?(u64)((i64)45):(u64)((i64)46));
        mm_decls_nextlx.subcode = (u64)65u;
        (*(mm_decls_nextlx.svalue + (i64)(mm_decls_nextlx.length))) = (u64)0u;
        return (i64)1;
    }else if ((index==(i64)7)) {
        mm_lex_lexreadtoken();
        if (((i64)((u64)(mm_decls_nextlx.symbol)) != (i64)45)) {
            mm_support_lxerror((byte*)"include: string expected");
        };
        file = mm_decls_nextlx.svalue;
        mlib_convlcstring(file);
        file = mlib_addext(file,(byte*)".m");
        if (!!(mm_decls_fverbose)) {
            msysc_m_print_startcon();
            msysc_m_print_str((byte*)"  Include:",NULL);
            msysc_m_print_str(file,NULL);
            msysc_m_print_newline();
            msysc_m_print_end();
            ;
        };
        mm_lex_stacksourcefile(file,(i64)0);
        return (i64)0;
    }else if ((index==(i64)1)) {
        mm_lex_lexreadtoken();
        if (((i64)((u64)(mm_decls_nextlx.symbol)) != (i64)45)) {
            mm_support_lxerror((byte*)"emitc/not str");
        };
        mm_decls_nextlx.symbol = (u64)((i64)48);
        return (i64)1;
    }else if ((index==(i64)13)) {
        mm_lex_lexreadtoken();
        if (((i64)((u64)(mm_decls_nextlx.symbol)) != (i64)45)) {
            mm_support_lxerror((byte*)"cclib/not str");
        };
        if ((mm_decls_ncclibs >= (i64)10)) {
            mm_support_lxerror((byte*)"Too many cc libs");
        };
        mm_decls_cclibtable[(++mm_decls_ncclibs)-1] = mlib_pcm_copyheapstring(mm_decls_nextlx.svalue);
        return (i64)0;
    } else {
        msysc_m_print_startcon();
        msysc_m_print_str(mm_tables_sourcedirnames[(index)-1],NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        mm_support_lxerror((byte*)"Directive not implemented");
    };
    return (i64)0;
}

static void mm_lex_lexreadline(void) {
    L426 :;
    switch ((i64)((*mm_lex_lxsptr))) {
    case 13:;
    case 10:;
    {
        return;
    }break;
    case 26:;
    case 0:;
    {
        --mm_lex_lxsptr;
        return;
    }break;
    default: {
        ++mm_lex_lxsptr;
    }
    } //SW
goto L426 ;
L427 :;
    ;
}

void mm_lex_startlex(byte * caption,i64 fileno) {
    mm_lex_lxsptr = mm_decls_sourcefiletext[(fileno)];
    mm_decls_nextlx.fileno = (u64)(fileno);
    mm_decls_nextlx.lineno = (i64)1;
    mm_decls_nextlx.symbol = (u64)((i64)6);
    mm_decls_nextlx.subcode = (u64)((i64)0);
}

byte * mm_lex_convertzstring(byte * s,i64 length) {
    static byte str[300];
    if ((length > (i64)300)) {
        mlib_abortprogram((byte*)"convertzstr");
    };
    memcpy((void *)(str),(void *)(s),(u64)(length));
    str[((length + (i64)1))-1] = (u64)0u;
    return str;
}

struct mm_decls_strec * mm_lex_addnamestr(byte * name) {
    struct mm_decls_lexrec oldlx;
    struct mm_decls_strec *  symptr;
    memcpy(&oldlx,&mm_decls_nextlx,32);
    mm_decls_nextlx.hashvalue = mm_lex_gethashvaluez(name);
    mm_decls_nextlx.length = (i64)(strlen((i8 *)(name)));
    mm_decls_nextlx.svalue = (byte *)(mlib_pcm_alloc(((i64)(mm_decls_nextlx.length) + (i64)1)));
    memcpy((void *)(mm_decls_nextlx.svalue),(void *)(name),(u64)(((i64)(mm_decls_nextlx.length) + (i64)1)));
    mm_lex_lookup();
    symptr = mm_decls_nextlx.symptr;
    memcpy(&mm_decls_nextlx,&oldlx,32);
    return symptr;
}

void mm_lex_ps1(byte * caption) {
    msysc_m_print_startcon();
    msysc_m_print_str(caption,NULL);
    msysc_m_print_nogap();
    msysc_m_print_str((byte*)":::",NULL);
    msysc_m_print_end();
    ;
    mm_lex_printsymbol(&mm_decls_lx);
}

void mm_lex_ps2(byte * caption) {
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"\t",NULL);
    msysc_m_print_nogap();
    msysc_m_print_str(caption,NULL);
    msysc_m_print_nogap();
    msysc_m_print_str((byte*)":##",NULL);
    msysc_m_print_end();
    ;
    mm_lex_printsymbol(&mm_decls_nextlx);
}

void mm_lex_ps(byte * caption) {
    mm_lex_ps1(caption);
}

void mm_lex_lex(void) {
    i64 n;
    byte *  p;
    memcpy(&mm_decls_lx,&mm_decls_nextlx,32);
    mm_decls_lx.lineno = (((i64)(mm_decls_lx.fileno) << (i64)24) + (i64)(mm_decls_lx.lineno));
    //reenter:
L428 :;
;
    mm_lex_lexreadtoken();
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)50)) {
        (*((*mm_decls_lx.symptr).name + (i64)(mm_decls_lx.length))) = (u64)0u;
    };
    switch ((i64)(mm_decls_nextlx.symbol)) {
    case 37:;
    {
        if (!(!!(mm_lex_lookup()))) {
            mm_decls_nextlx.symbol = (u64)((i64)50);
            return;
        };
        //found:
L429 :;
;
        mm_decls_nextlx.symbol = (u64)((*mm_decls_nextlx.symptr).symbol);
        mm_decls_nextlx.subcode = (u64)((*mm_decls_nextlx.symptr).subcode);
        switch ((i64)(mm_decls_nextlx.symbol)) {
        case 51:;
        {
            if (!(!!(mm_lex_dolexdirective((i64)(mm_decls_nextlx.subcode))))) {
                goto L428 ;
;
            };
        }break;
        case 37:;
        {
            if ((((i64)((u64)(mm_decls_nextlx.subcode)) == (i64)49) && (((i64)((u64)(mm_decls_lx.symbol)) == (i64)40) || ((i64)((u64)(mm_decls_lx.symbol)) == (i64)42)))) {
                if (((i64)(mm_decls_lx.symbol)==(i64)40)) {
                    if ((((i64)((u64)(mm_decls_lx.subcode)) == (i64)5) || ((i64)((u64)(mm_decls_lx.subcode)) == (i64)10))) {
                        mm_support_lxerror((byte*)"No suffix on i128/u128");
                    };
                    if (((i64)((*mm_decls_nextlx.symptr).index)==(i64)2)) {
                        mm_decls_lx.value *= (i64)1000000;
                    }else if (((i64)((*mm_decls_nextlx.symptr).index)==(i64)3)) {
                        mm_decls_lx.value *= (i64)1000000000;
                    }else if (((i64)((*mm_decls_nextlx.symptr).index)==(i64)1)) {
                        mm_decls_lx.value *= (i64)1000;
                    }else if (((i64)((*mm_decls_nextlx.symptr).index)==(i64)4)) {
                        mm_decls_lx.value *= (i64)1024;
                    }else if (((i64)((*mm_decls_nextlx.symptr).index)==(i64)5)) {
                        mm_decls_lx.value *= (i64)1048576;
                    }else if (((i64)((*mm_decls_nextlx.symptr).index)==(i64)6)) {
                        mm_decls_lx.value *= (i64)1073741824;
                    } else {
                        mm_support_lxerror((byte*)"Can't do this unit index");
                    };
                    mm_decls_lx.subcode = (u64)(mm_lex_setinttype((u64)(mm_decls_lx.value)));
                } else {
                    mm_support_lxerror((byte*)"Unit suffix after float not implem");
                };
                goto L428 ;
;
            } else {
                mm_decls_nextlx.symbol = (u64)((i64)50);
                mm_decls_nextlx.svalue = (*mm_decls_nextlx.symptr).name;
            };
        }break;
        case 50:;
        {
            mm_support_lxerror((byte*)"NEXT NAME!!!");
        }break;
        case 68:;
        case 84:;
        case 69:;
        case 85:;
        case 72:;
        case 73:;
        case 76:;
        case 74:;
        case 91:;
        case 92:;
        case 98:;
        case 67:;
        case 94:;
        case 95:;
        case 96:;
        case 102:;
        case 77:;
        case 117:;
        case 124:;
        case 136:;
        case 144:;
        case 59:;
        {
            if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)66)) {
                mm_decls_lx.subcode = (u64)(mm_decls_nextlx.symbol);
                goto L428 ;
;
            };
        }break;
        case 32:;
        {
            goto L430 ;
;
        }break;
        case 143:;
        {
            if (((i64)(mm_decls_nextlx.subcode)==(i64)4)) {
                mm_decls_nextlx.symbol = (u64)((i64)40);
                mm_decls_nextlx.value = (i64)0;
                mm_decls_nextlx.subcode = (u64)((i64)4);
            }else if (((i64)(mm_decls_nextlx.subcode)==(i64)1)) {
                mm_decls_nextlx.symbol = (u64)((i64)40);
                mm_decls_nextlx.value = (i64)0;
                mm_decls_nextlx.subcode = (u64)((i64)20);
            }else if (((i64)(mm_decls_nextlx.subcode)==(i64)2)) {
                mm_decls_nextlx.symbol = (u64)((i64)42);
                mm_decls_nextlx.xvalue = (double)3.1415926535897931;
                mm_decls_nextlx.subcode = (u64)((i64)12);
            }else if (((i64)(mm_decls_nextlx.subcode)==(i64)3)) {
                mm_decls_nextlx.symbol = (u64)((i64)45);
                mm_decls_nextlx.svalue = (byte*)"\t";
                mm_decls_nextlx.length = (i64)1;
            } else {
                mm_support_lxerror((byte*)"sysconst?");
            };
        }break;
        case 54:;
        {
            if (((i64)(mm_decls_nextlx.subcode)==(i64)73) || ((i64)(mm_decls_nextlx.subcode)==(i64)105)) {
                mm_decls_nextlx.subcode = ((mm_decls_targetbits == (i64)32)?(u64)((i64)3):(u64)((i64)4));
            }else if (((i64)(mm_decls_nextlx.subcode)==(i64)87) || ((i64)(mm_decls_nextlx.subcode)==(i64)119)) {
                mm_decls_nextlx.subcode = ((mm_decls_targetbits == (i64)32)?(u64)((i64)8):(u64)((i64)9));
            };
            mm_decls_nextlx.symbol = (u64)((i64)53);
        }break;
        default: {
        }
        } //SW
;
    }break;
    case 35:;
    {
        switch ((i64)(mm_decls_lx.symbol)) {
        case 5:;
        case 14:;
        case 12:;
        case 9:;
        case 6:;
        {
            goto L428 ;
;
        }break;
        case 32:;
        {
            if (!(!!(mm_decls_assemmode))) {
                goto L428 ;
;
            };
            mm_decls_nextlx.symbol = (u64)((i64)6);
        }break;
        default: {
            mm_decls_nextlx.symbol = (u64)((i64)6);
        }
        } //SW
;
    }break;
    case 45:;
    {
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)45)) {
            n = ((i64)(mm_decls_nextlx.length) + (i64)(mm_decls_lx.length));
            p = (byte *)(mlib_pcm_alloc((n + (i64)1)));
            memcpy((void *)(p),(void *)(mm_decls_lx.svalue),(u64)(mm_decls_lx.length));
            memcpy((void *)((p + (i64)(mm_decls_lx.length))),(void *)(mm_decls_nextlx.svalue),(u64)(mm_decls_nextlx.length));
            (*(p + n)) = (u64)0u;
            mm_decls_lx.svalue = p;
            mm_decls_lx.length = n;
            goto L428 ;
;
        };
    }break;
    case 32:;
    {
        //doopsym:
L430 :;
;
        if (((((i64)((u64)(mm_decls_nextlx.subcode)) == (i64)49) && ((i64)((u64)(mm_decls_lx.symbol)) == (i64)32)) && ((i64)((u64)(mm_decls_lx.subcode)) == (i64)12))) {
            mm_decls_lx.subcode = (u64)((i64)50);
            goto L428 ;
;
        };
    }break;
    case 36:;
    {
    }break;
    default: {
    }
    } //SW
;
}

void mm_lex_showhashtablesize(void) {
    i64 i;
    i64 n;
    n = (i64)0;
    L431 :;
    for (i=(i64)0;i<=(i64)32767;++i) {
L432 :;
        if (!!(mm_lex_hashtable[(i)].name)) {
            ++n;
        };
L433 :;
    }L434 :;
    ;
}

i64 mm_lex_checkname(byte * name,i64 length) {
    if ((length == (i64)0)) {
        length = (i64)(strlen((i8 *)(name)));
    };
    if ((((i64)(mm_decls_nextlx.length) == length) && ((i64)(memcmp((void *)(mm_decls_nextlx.svalue),(void *)(name),(u64)(length))) == (i64)0))) {
        return (i64)1;
    };
    return (i64)0;
}

static byte * mm_lex_getstrfile(byte * filename,i32 * length) {
    byte *  file;
    static byte filespec[300];
    i64 i;
    L435 :;
    for (i=mm_decls_nsearchdirs;i>=(i64)1;--i) {
L436 :;
        strcpy((i8 *)(filespec),(i8 *)(mm_decls_searchdirs[(i)-1]));
        strcat((i8 *)(filespec),(i8 *)(filename));
        if (!!(mlib_checkfile(filespec))) {
            file = (byte *)(mlib_readfile(filespec));
            (*length) = mlib_rfsize;
            return file;
        };
L437 :;
    }L438 :;
    ;
    return (byte *)(0);
}

static void mm_lex_stacksourcefile(byte * file,i64 ismainmodule) {
    i64 fileno;
    fileno = mm_support_getsupportfile(file);
    mm_lex_stacksource(mm_decls_sourcefiletext[(fileno)],fileno,(i64)1);
}

static void mm_lex_stacksource(byte * sptr,i64 fileno,i64 isfile) {
    if ((mm_lex_sourcelevel >= (i64)20)) {
        mm_support_lxerror((byte*)"Include file/macro overflow");
    };
    ++mm_lex_sourcelevel;
    mm_lex_lxstart_stack[(mm_lex_sourcelevel)-1] = mm_lex_lxstart;
    mm_lex_lxsptr_stack[(mm_lex_sourcelevel)-1] = mm_lex_lxsptr;
    mm_lex_lxfileno_stack[(mm_lex_sourcelevel)-1] = (i64)(mm_decls_nextlx.fileno);
    mm_lex_lxlineno_stack[(mm_lex_sourcelevel)-1] = (i64)(mm_decls_nextlx.lineno);
    mm_lex_isfile_stack[(mm_lex_sourcelevel)-1] = (u64)(isfile);
    mm_lex_lxstart = (mm_lex_lxsptr = sptr);
    mm_decls_nextlx.lineno = (i64)1;
    mm_decls_nextlx.fileno = (u64)(fileno);
}

static void mm_lex_unstacksource(void) {
    if ((mm_lex_sourcelevel > (i64)0)) {
        mm_lex_lxstart = mm_lex_lxstart_stack[(mm_lex_sourcelevel)-1];
        mm_lex_lxsptr = mm_lex_lxsptr_stack[(mm_lex_sourcelevel)-1];
        mm_decls_nextlx.lineno = mm_lex_lxlineno_stack[(mm_lex_sourcelevel)-1];
        mm_decls_nextlx.fileno = (u64)(mm_lex_lxfileno_stack[(mm_lex_sourcelevel)-1]);
        --mm_lex_sourcelevel;
    };
}

static void mm_lex_readarraystring(i64 prefix) {
    ++mm_lex_lxsptr;
    mm_lex_lxreadstring((i64)34);
    mm_decls_nextlx.symbol = (u64)((i64)46);
    mm_decls_nextlx.subcode = (u64)(toupper((i64)((i32)(prefix))));
}

static void mm_lex_qadd(struct mm_decls_qint * aa,struct mm_decls_qint * bb) {
    u64 low;
    low = ((*aa).lower + (*bb).lower);
    if (((*aa).lower > low)) {
        ++(*aa).upper;
    };
    (*aa).lower = low;
    (*aa).upper += (*bb).upper;
}

static void mm_lex_qadddigit(struct mm_decls_qint * aa,i64 x) {
    u64 low;
    low = (u64)(((i64)((*aa).lower) + x));
    if (((*aa).lower > low)) {
        ++(*aa).upper;
    };
    (*aa).lower = low;
}

static void mm_lex_qshift(struct mm_decls_qint * aa,i64 n) {
    u64 overflow;
    i64 av_1;
    av_1 = n;
    while (av_1-- > 0) {
L439 :;
        overflow = (u64)((i64)0);
        if (!!(((*aa).lower & (u64)9223372036854775808u))) {
            overflow = (u64)((i64)1);
        };
        (*aa).lower <<= (u64)((i64)1);
        (*aa).upper = (((*aa).upper << (i64)1) + (i64)(overflow));
L440 :;
    }L441 :;
    ;
}

static void mm_lex_qmul10(struct mm_decls_qint * aa) {
    struct mm_decls_qint bb;
    bb = (*aa);
    mm_lex_qshift(aa,(i64)2);
    mm_lex_qadd(aa,&bb);
    mm_lex_qshift(aa,(i64)1);
}

static void mm_lex_qmulbase(struct mm_decls_qint * aa,i64 base) {
    struct mm_decls_qint bb;
    i64 av_1;
    if ((base==(i64)16)) {
        mm_lex_qshift(aa,(i64)4);
    }else if ((base==(i64)10)) {
        mm_lex_qmul10(aa);
    } else {
        bb = (*aa);
        av_1 = (base - (i64)1);
        while (av_1-- > 0) {
L442 :;
            mm_lex_qadd(aa,&bb);
L443 :;
        }L444 :;
        ;
    };
}

static struct mm_decls_qint * mm_lex_stringtonumber128(byte * s,i64 length,i64 base) {
    struct mm_decls_qint *  aa;
    i64 c;
    i64 d;
    i64 av_1;
    aa = (struct mm_decls_qint *)(mlib_pcm_allocz((i64)16));
    av_1 = length;
    while (av_1-- > 0) {
L445 :;
        mm_lex_qmulbase(aa,base);
        c = (i64)((*s++));
        if ((c >= (i64)97)) {
            d = ((c - (i64)97) + (i64)10);
        } else if ((c >= (i64)65)) {
            d = ((c - (i64)65) + (i64)10);
        } else {
            d = (c - (i64)48);
        };
        mm_lex_qadddigit(aa,d);
L446 :;
    }L447 :;
    ;
    return aa;
}

static i64 mm_lex_setinttype(u64 a) {
    if ((a < (u64)9223372036854775807u)) {
        return (i64)4;
    } else {
        return (i64)9;
    };
}

void mm_diags_printmodelist(void * f) {
    byte *  mstr;
    struct mlib_strbuffer destv;
    struct mlib_strbuffer *  dest = &destv;
    i64 m;
    dest = &destv;
    msysc_m_print_startfile(f);
    msysc_m_print_str((byte*)"MODELIST",NULL);
    msysc_m_print_i64(mm_decls_ntypes,NULL);
    msysc_m_print_i64(mm_decls_nuserxtypes,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    mlib_gs_init(dest);
    mlib_gs_leftstr(dest,(byte*)"#",(i64)4,(i64)32);
    mlib_gs_leftstr(dest,(byte*)"Name",(i64)13,(i64)32);
    mlib_gs_leftstr(dest,(byte*)"Base",(i64)13,(i64)32);
    mlib_gs_leftstr(dest,(byte*)"Bit",(i64)3,(i64)32);
    mlib_gs_leftstr(dest,(byte*)"Target",(i64)12,(i64)32);
    mlib_gs_leftstr(dest,(byte*)"Def",(i64)4,(i64)32);
    mlib_gs_leftstr(dest,(byte*)"Lwb",(i64)5,(i64)32);
    mlib_gs_leftstr(dest,(byte*)"Upb",(i64)5,(i64)32);
    mlib_gs_leftstr(dest,(byte*)"Len",(i64)6,(i64)32);
    mlib_gs_leftstr(dest,(byte*)"Size",(i64)6,(i64)32);
    mlib_gs_leftstr(dest,(byte*)"Cat",(i64)4,(i64)32);
    mlib_gs_leftstr(dest,(byte*)"Used",(i64)4,(i64)32);
    mlib_gs_leftstr(dest,(byte*)"Mode",(i64)32,(i64)32);
    mlib_gs_println(dest,f);
    L448 :;
    for (m=(i64)0;m<=mm_decls_ntypes;++m) {
L449 :;
        mlib_gs_init(dest);
        mlib_gs_leftint(dest,m,(i64)4,(i64)32);
        mlib_gs_leftstr(dest,mm_lib_typename(m),(i64)13,(i64)32);
        mlib_gs_leftstr(dest,mm_lib_typename((i64)(mm_decls_ttbasetype[(m)])),(i64)13,(i64)32);
        mlib_gs_leftint(dest,(i64)(mm_decls_ttbitwidth[(m)]),(i64)3,(i64)32);
        if (!!((i64)(mm_decls_tttarget[(m)]))) {
            mlib_gs_leftstr(dest,mm_lib_typename((i64)(mm_decls_tttarget[(m)])),(i64)12,(i64)32);
        } else {
            mlib_gs_leftstr(dest,(byte*)"-",(i64)12,(i64)32);
        };
        if (!!(mm_decls_ttnamedef[(m)])) {
            mlib_gs_leftstr(dest,(byte*)"+",(i64)4,(i64)32);
        } else {
            mlib_gs_leftstr(dest,(byte*)"-",(i64)4,(i64)32);
        };
        if (((i64)(mm_decls_ttbasetype[(m)])==(i64)29) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)32) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)24) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)30) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)31)) {
            mlib_gs_leftint(dest,(i64)(mm_decls_ttlower[(m)]),(i64)5,(i64)32);
            mlib_gs_leftint(dest,(((i64)(mm_decls_ttlower[(m)]) + (i64)(mm_decls_ttlength[(m)])) - (i64)1),(i64)5,(i64)32);
            mlib_gs_leftint(dest,(i64)(mm_decls_ttlength[(m)]),(i64)6,(i64)32);
        } else {
            mlib_gs_leftstr(dest,(byte*)"",(i64)5,(i64)32);
            mlib_gs_leftstr(dest,(byte*)"",(i64)5,(i64)32);
            mlib_gs_leftstr(dest,(byte*)"",(i64)6,(i64)32);
        };
        mlib_gs_leftint(dest,(i64)(mm_decls_ttsize[(m)]),(i64)6,(i64)32);
        mlib_gs_leftint(dest,(i64)(mm_decls_ttusercat[(m)]),(i64)4,(i64)32);
        mstr = mm_lib_strmode(m,(i64)1);
        if (((i64)(strlen((i8 *)(mstr))) < (i64)16)) {
            mlib_gs_str(dest,mstr);
        } else {
            mlib_gs_println(dest,f);
            mlib_gs_init(dest);
            mlib_gs_str(dest,mstr);
        };
        mlib_gs_println(dest,f);
L450 :;
    }L451 :;
    ;
    msysc_m_print_startfile(f);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    msysc_m_print_startfile(f);
    msysc_m_print_str((byte*)"USERXTYPES:",NULL);
    msysc_m_print_i64(mm_decls_nuserxtypes,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    L452 :;
    for (m=(i64)1;m<=mm_decls_nuserxtypes;++m) {
L453 :;
        mlib_gs_init(dest);
        mlib_gs_strint(dest,m);
        mlib_gs_str(dest,(byte*)": ");
        mlib_gs_str(dest,(*mm_decls_ttnamedefx[(m)]).name);
        if (!!(mm_decls_ttnamedefx2[(m)])) {
            mlib_gs_str(dest,(byte*)".");
            mlib_gs_str(dest,(*mm_decls_ttnamedefx2[(m)]).name);
        };
        mlib_gs_str(dest,(byte*)" ");
        mlib_gs_strint(dest,mm_decls_ttxmap[(m)]);
        mlib_gs_println(dest,f);
L454 :;
    }L455 :;
    ;
}

void mm_diags_printst(void * f,struct mm_decls_strec * p,i64 level) {
    struct mm_decls_strec *  q;
    if (((i64)((u64)((*p).symbol)) != (i64)50)) {
        mm_support_mcerror((byte*)"PRINTST not name");
    };
    mm_diags_printstrec(f,p,level);
    q = (*p).deflist;
    L456 :;
    while ((q != 0)) {
        mm_diags_printst(f,q,(level + (i64)1));
        q = (*q).nextdef;
L457 :;
    }L458 :;
    ;
}

static void mm_diags_printstrec(void * f,struct mm_decls_strec * p,i64 level) {
    struct mm_decls_strec dd;
    struct mlib_strbuffer v;
    struct mlib_strbuffer *  d = &v;
    i64 col;
    i64 offset;
    i64 n;
    byte str[256];
    i64 av_1;
    i64 i;
    d = &v;
    mlib_gs_init(d);
    offset = (i64)0;
    av_1 = level;
    while (av_1-- > 0) {
L459 :;
        mlib_gs_str(d,(byte*)"    ");
        offset += (i64)4;
L460 :;
    }L461 :;
    ;
    mlib_gs_str(d,(byte*)":");
    mlib_gs_leftstr(d,(*p).name,((i64)28 - offset),(i64)45);
    mlib_gs_leftstr(d,mm_tables_namenames[((i64)((*p).nameid))],(i64)12,(i64)46);
    col = mlib_gs_getcol(d);
    memcpy(&dd,p,150);
    mlib_gs_str(d,(byte*)"[");
    if (!!((u64)((*p).imported))) {
        mlib_gs_str(d,(((i64)((u64)((*p).imported)) == (i64)2)?(byte*)"Imp/CLIB ":(byte*)"Imp "));
    } else {
        mlib_gs_str(d,((i64)((*p).isglobal)==1?(byte*)"Glob ":((i64)((*p).isglobal)==2?(byte*)"Exp ":(byte*)"Loc ")));
    };
    if (!!((u64)(dd.isstatic))) {
        mlib_gs_str(d,(byte*)"Stat");
    };
    if (!!((u64)(dd.fflang))) {
        mlib_gs_strsp(d,mm_tables_fflangnames[((i64)(dd.fflang))]);
    };
    if (!!((u64)(dd.parammode))) {
        mlib_gs_str(d,mm_tables_parammodenames[((i64)(dd.parammode))]);
    };
    if (!!((u64)(dd.align))) {
        mlib_gs_str(d,(byte*)"@@");
        mlib_gs_strint(d,(i64)(dd.align));
        mlib_gs_str(d,(byte*)" ");
    };
    if (!!((u64)(dd.optional))) {
        mlib_gs_str(d,(byte*)"Opt ");
    };
    if (!!((u64)(dd.varparams))) {
        mlib_gs_str(d,(byte*)"Var ");
    };
    if (!!((u64)(dd.moduleno))) {
        msysc_m_print_startstr(str);
        msysc_m_print_str((byte*)"Modno#",NULL);
        msysc_m_print_nogap();
        msysc_m_print_u64(dd.moduleno,NULL);
        msysc_m_print_end();
        ;
        mlib_gs_str(d,str);
    };
    if (!!((u64)(dd.equals))) {
        mlib_gs_str(d,(byte*)":= ");
    };
    if (!!((u64)(dd.used))) {
        mlib_gs_str(d,(byte*)"U ");
    };
    mlib_gs_str(d,(byte*)"]");
    mlib_gs_padto(d,(col + (i64)10),(i64)61);
    if (!!((*p).owner)) {
        msysc_m_print_startstr(str);
        msysc_m_print_setfmt((byte*)"(#)");
        msysc_m_print_str((*(*p).owner).name,NULL);
        msysc_m_print_end();
        ;
        mlib_gs_leftstr(d,str,(i64)18,(i64)45);
    } else {
        mlib_gs_leftstr(d,(byte*)"()",(i64)18,(i64)45);
    };
    if (((i64)((*p).mode)==(i64)0)) {
        mlib_gs_str(d,(byte*)"Void ");
    } else {
        mlib_gs_strint(d,(i64)((*p).mode));
        mlib_gs_str(d,(byte*)":");
        n = (i64)((*p).nretvalues);
        if ((n == (i64)0)) {
            n = (i64)1;
        };
        if ((n > (i64)1)) {
            mlib_gs_str(d,(byte*)"(");
        };
        L462 :;
        for (i=(i64)1;i<=n;++i) {
L463 :;
            mlib_gs_str(d,mm_lib_strmode((i64)((*p).modelist[(i)-1]),(i64)1));
            if ((i < n)) {
                mlib_gs_str(d,(byte*)",");
            };
L464 :;
        }L465 :;
        ;
        if ((n > (i64)1)) {
            mlib_gs_str(d,(byte*)")");
        };
        mlib_gs_str(d,(byte*)" ");
    };
    if (((i64)((*p).nameid)==(i64)12) || ((i64)((*p).nameid)==(i64)11)) {
        mlib_gs_str(d,(byte*)" Offset:");
        mlib_gs_strint(d,(i64)((*p).offset));
        if (((i64)((*p).mode) == (i64)25)) {
            mlib_gs_str(d,(byte*)" Bitoffset:");
            mlib_gs_strint(d,(i64)((*p).bitoffset));
            mlib_gs_str(d,(byte*)":");
            mlib_gs_strint(d,(i64)((*p).bitfieldwidth));
        };
        msysc_m_print_startstr(str);
        msysc_m_print_u64((*p).uflags.ulength,(byte*)"v");
        msysc_m_print_str((byte *)(&(*p).uflags.codes),(byte*)".*");
        msysc_m_print_end();
        ;
        mlib_gs_str(d,(byte*)" UFLAGS:");
        mlib_gs_str(d,str);
        mlib_gs_str(d,(byte*)"-");
        mlib_gs_strint(d,(i64)((*p).uflags.ulength));
        if (!!((*p).code)) {
            mlib_gs_str(d,(byte*)":=");
            mlib_gs_strvar(d,mm_lib_strexpr((*p).code));
        };
    }else if (((i64)((*p).nameid)==(i64)13)) {
        mlib_gs_str(d,(byte*)"Index:");
        mlib_gs_strint(d,(i64)((*p).offset));
    }else if (((i64)((*p).nameid)==(i64)5)) {
        mlib_gs_str(d,(byte*)"Index:");
        mlib_gs_strint(d,(i64)((*p).index));
        mlib_gs_str(d,(byte*)" Nret:");
        mlib_gs_strint(d,(i64)((*p).nretvalues));
        mlib_gs_str(d,(byte*)" Simple:");
        mlib_gs_strint(d,(i64)((*p).simplefunc));
    }else if (((i64)((*p).nameid)==(i64)6)) {
        mlib_gs_str(d,(byte*)"Index/PCaddr:");
        mlib_gs_strint(d,(i64)((*p).index));
        if (!!((*p).truename)) {
            mlib_gs_str(d,(byte*)" Truename:");
            mlib_gs_str(d,(*p).truename);
        };
    }else if (((i64)((*p).nameid)==(i64)9)) {
        if (!!((*p).code)) {
            mlib_gs_str(d,(byte*)"=");
            mlib_gs_strvar(d,mm_lib_strexpr((*p).code));
        };
    }else if (((i64)((*p).nameid)==(i64)10)) {
        if (!!((*p).code)) {
            mlib_gs_str(d,(byte*)":=");
            mlib_gs_strvar(d,mm_lib_strexpr((*p).code));
        };
    }else if (((i64)((*p).nameid)==(i64)8)) {
        mlib_gs_str(d,(byte*)"Const:");
        mlib_gs_strvar(d,mm_lib_strexpr((*p).code));
    }else if (((i64)((*p).nameid)==(i64)4)) {
        if (!!((i64)((*p).base_class))) {
            mlib_gs_str(d,(byte*)"Baseclass:");
            mlib_gs_str(d,mm_lib_typename((i64)((*p).base_class)));
        };
    }else if (((i64)((*p).nameid)==(i64)14)) {
        mlib_gs_str(d,(byte*)"Enum:");
        mlib_gs_strint(d,(i64)((*p).index));
    }else if (((i64)((*p).nameid)==(i64)3)) {
        mlib_gs_str(d,(byte*)"DLL#:");
        mlib_gs_strint(d,(i64)((*p).dllindex));
    };
    if (((i64)((*p).at)==(i64)2)) {
        mlib_gs_str(d,(byte*)" @");
        mlib_gs_str(d,(*(*p).equivfield).name);
    }else if (((i64)((*p).at)==(i64)1)) {
        mlib_gs_str(d,(byte*)" @");
        mlib_gs_strvar(d,mm_lib_strexpr((*p).equivvar));
    };
    mlib_gs_str(d,(byte*)" Module# ");
    mlib_gs_strint(d,(i64)((*p).moduleno));
    mlib_gs_str(d,(byte*)" Lineno:");
    mlib_gs_strint(d,((i64)((*p).lineno) & (i64)16777215));
    mlib_gs_println(d,f);
    if (((i64)((*p).nameid)==(i64)8) || ((i64)((*p).nameid)==(i64)10) || ((i64)((*p).nameid)==(i64)9) || ((i64)((*p).nameid)==(i64)18)) {
        if (!!((*p).code)) {
            mm_diags_printunit((*p).code,(i64)0,(byte*)"*",f);
        };
    };
}

void mm_diags_printstflat(void * f) {
    i64 i;
    struct mm_decls_strec *  p;
    i64 av_1;
    msysc_m_print_startfile(f);
    msysc_m_print_str((byte*)"GLOBAL SYMBOL TABLE:",NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    L466 :;
    for (i=(i64)0;i<=(i64)32766;++i) {
L467 :;
        p = &mm_lex_hashtable[(i)];
        if (!!((*p).name)) {
            if (((i64)((*p).symbol)==(i64)37)) {
                msysc_m_print_startfile(f);
                msysc_m_print_i64(i,NULL);
                msysc_m_print_ptr(p,NULL);
                msysc_m_print_str((byte*)":",NULL);
                msysc_m_print_str((*p).name,NULL);
                msysc_m_print_str(mm_tables_symbolnames[((i64)((*p).symbol))-1],NULL);
                msysc_m_print_str(mm_tables_namenames[((i64)((*p).nameid))],NULL);
                msysc_m_print_newline();
                msysc_m_print_end();
                ;
                p = (*p).nextdupl;
                L470 :;
                while (!!(p)) {
                    msysc_m_print_startfile(f);
                    msysc_m_print_str((byte*)"\t",NULL);
                    msysc_m_print_ptr(p,NULL);
                    msysc_m_print_str((*p).name,NULL);
                    msysc_m_print_str(mm_tables_symbolnames[((i64)((*p).symbol))-1],NULL);
                    msysc_m_print_str(mm_tables_namenames[((i64)((*p).nameid))],NULL);
                    msysc_m_print_str((byte*)"(From",NULL);
                    msysc_m_print_str((!!((*p).owner)?(*(*p).owner).name:(byte*)"-"),NULL);
                    msysc_m_print_nogap();
                    msysc_m_print_str((byte*)")",NULL);
                    msysc_m_print_newline();
                    msysc_m_print_end();
                    ;
                    p = (*p).nextdupl;
L471 :;
                }L472 :;
                ;
            };
        };
L468 :;
    }L469 :;
    ;
}

void mm_diags_printcode(void * f,byte * caption) {
    struct mm_decls_strec *  p;
    p = (*mm_decls_stprogram).deflist;
    msysc_m_print_startfile(f);
    msysc_m_print_str(caption,NULL);
    msysc_m_print_str((byte*)"PROGRAM",NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    L473 :;
    while (!!(p)) {
        mm_diags_printmodulecode(f,p);
        p = (*p).nextdef;
L474 :;
    }L475 :;
    ;
}

void mm_diags_printmodulecode(void * f,struct mm_decls_strec * m) {
    struct mm_decls_strec *  p;
    p = (*m).deflist;
    mm_decls_currmodule = m;
    msysc_m_print_startfile(f);
    msysc_m_print_str((byte*)"MODULE:",NULL);
    msysc_m_print_str((*m).name,NULL);
    msysc_m_print_str(mm_tables_namenames[((i64)((*m).nameid))],NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    L476 :;
    while (!!(p)) {
        if (((i64)((*p).nameid)==(i64)5)) {
            if (!(!!((u64)((*p).imported)))) {
                msysc_m_print_startfile(f);
                msysc_m_print_str((*p).name,NULL);
                msysc_m_print_nogap();
                msysc_m_print_str((byte*)"=",NULL);
                msysc_m_print_str(((i64)((*p).isglobal)==1?(byte*)"Global":((i64)((*p).isglobal)==2?(byte*)"Export":(byte*)"Local")),NULL);
                msysc_m_print_newline();
                msysc_m_print_end();
                ;
                mm_diags_printunit((*p).code,(i64)0,(byte*)"1",f);
                msysc_m_print_startfile(f);
                msysc_m_print_newline();
                msysc_m_print_end();
                ;
            };
        };
        p = (*p).nextdef;
L477 :;
    }L478 :;
    ;
}

void mm_diags_printunit(struct mm_decls_unitrec * p,i64 level,byte * prefix,void * dev) {
    struct mm_decls_strec *  d;
    i64 t;
    byte *  idname;
    i64 a;
    float x32;
    if ((p == 0)) {
        return;
    };
    if (!!((i64)((*p).lineno))) {
        mm_diags_currlineno = ((i64)((*p).lineno) & (i64)16777215);
    };
    msysc_m_print_startfile(dev);
    msysc_m_print_ptr(p,NULL);
    msysc_m_print_str((byte*)":",NULL);
    msysc_m_print_end();
    ;
    msysc_m_print_startfile(dev);
    msysc_m_print_str(mm_diags_getprefix(level,prefix,p),NULL);
    msysc_m_print_end();
    ;
    idname = (mm_tables_jtagnames[((i64)((*p).tag))] + (i64)2);
    msysc_m_print_startfile(dev);
    msysc_m_print_str(idname,NULL);
    msysc_m_print_nogap();
    msysc_m_print_str((byte*)": ",NULL);
    msysc_m_print_end();
    ;
    if (((i64)((*p).tag)==(i64)3)) {
        d = (*p).def;
        msysc_m_print_startfile(dev);
        msysc_m_print_str((*d).name,NULL);
        msysc_m_print_str(mm_tables_namenames[((i64)((*d).nameid))],NULL);
        msysc_m_print_end();
        ;
        if (!!((*d).code)) {
            msysc_m_print_startfile(dev);
            msysc_m_print_str((byte*)" {",NULL);
            msysc_m_print_nogap();
            msysc_m_print_str(mm_tables_jtagnames[((i64)((*(*d).code).tag))],NULL);
            msysc_m_print_nogap();
            msysc_m_print_str((byte*)"}",NULL);
            msysc_m_print_end();
            ;
        };
        msysc_m_print_startfile(dev);
        msysc_m_print_str((byte*)" ",NULL);
        msysc_m_print_nogap();
        msysc_m_print_str(mm_lib_getdottedname(d),NULL);
        msysc_m_print_end();
        ;
        msysc_m_print_startfile(dev);
        msysc_m_print_str((!!((u64)((*p).dottedname))?(byte*)" {Dotted}":(byte*)""),NULL);
        msysc_m_print_end();
        ;
        if (!!((*p).c)) {
            msysc_m_print_startfile(dev);
            msysc_m_print_str((byte*)" Lastcall:",NULL);
            msysc_m_print_ptr((*p).c,NULL);
            msysc_m_print_end();
            ;
        };
        if (!!((i64)((*p).addroffirst))) {
            msysc_m_print_startfile(dev);
            msysc_m_print_str((byte*)" Addroffirst.",NULL);
            msysc_m_print_end();
            ;
        };
        msysc_m_print_startfile(dev);
        msysc_m_print_str((byte*)" Moduleno:",NULL);
        msysc_m_print_i64((*p).moduleno,NULL);
        msysc_m_print_end();
        ;
    }else if (((i64)((*p).tag)==(i64)210)) {
        msysc_m_print_startfile(dev);
        msysc_m_print_str((*(*p).def).name,NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
    }else if (((i64)((*p).tag)==(i64)1) || ((i64)((*p).tag)==(i64)242)) {
        t = (i64)((*p).mode);
        a = (*p).value;
        if ((t == mm_tables_trefchar)) {
            if (((i64)((*p).slength) > (i64)256)) {
                msysc_m_print_startfile(dev);
                msysc_m_print_str((byte*)"\"",NULL);
                msysc_m_print_nogap();
                msysc_m_print_str((byte*)"(LONGSTR)",NULL);
                msysc_m_print_str((byte*)"\" *",NULL);
                msysc_m_print_nogap();
                msysc_m_print_i64((*p).slength,NULL);
                msysc_m_print_end();
                ;
            } else if (!!((i64)((*p).slength))) {
                msysc_m_print_startfile(dev);
                msysc_m_print_str((byte*)"\"",NULL);
                msysc_m_print_nogap();
                msysc_m_print_str((*p).svalue,NULL);
                msysc_m_print_nogap();
                msysc_m_print_str((byte*)"\" *",NULL);
                msysc_m_print_nogap();
                msysc_m_print_i64((*p).slength,NULL);
                msysc_m_print_end();
                ;
            } else {
                msysc_m_print_startfile(dev);
                msysc_m_print_str((byte*)"\"\"",NULL);
                msysc_m_print_end();
                ;
            };
        } else {
            if (((i64)(mm_decls_ttbasetype[(t)])==(i64)4) || ((i64)(mm_decls_ttbasetype[(t)])==(i64)3) || ((i64)(mm_decls_ttbasetype[(t)])==(i64)2) || ((i64)(mm_decls_ttbasetype[(t)])==(i64)1)) {
                msysc_m_print_startfile(dev);
                msysc_m_print_i64(a,NULL);
                msysc_m_print_end();
                ;
            }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)9) || ((i64)(mm_decls_ttbasetype[(t)])==(i64)8) || ((i64)(mm_decls_ttbasetype[(t)])==(i64)7) || ((i64)(mm_decls_ttbasetype[(t)])==(i64)6)) {
                msysc_m_print_startfile(dev);
                msysc_m_print_u64((u64)(a),NULL);
                msysc_m_print_end();
                ;
            }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)15) || ((i64)(mm_decls_ttbasetype[(t)])==(i64)13) || ((i64)(mm_decls_ttbasetype[(t)])==(i64)14)) {
                msysc_m_print_startfile(dev);
                msysc_m_print_str(mlib_chr(a),NULL);
                msysc_m_print_end();
                ;
            }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)11)) {
                x32 = (float)((*p).xvalue);
                msysc_m_print_startfile(dev);
                msysc_m_print_r64((double)(x32),NULL);
                msysc_m_print_end();
                ;
            }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)12)) {
                msysc_m_print_startfile(dev);
                msysc_m_print_r64((*p).xvalue,NULL);
                msysc_m_print_end();
                ;
            }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)20)) {
                if (!!((*p).value)) {
                    msysc_m_print_startfile(dev);
                    msysc_m_print_str((byte*)"#",NULL);
                    msysc_m_print_nogap();
                    msysc_m_print_i64((*p).value,NULL);
                    msysc_m_print_i64((*p).slength,NULL);
                    msysc_m_print_end();
                    ;
                } else {
                    msysc_m_print_startfile(dev);
                    msysc_m_print_str((byte*)"NIL",NULL);
                    msysc_m_print_end();
                    ;
                };
            }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)5) || ((i64)(mm_decls_ttbasetype[(t)])==(i64)10)) {
                msysc_m_print_startfile(dev);
                msysc_m_print_str(mm_lib_strqvalue((*p).qvalue),NULL);
                msysc_m_print_end();
                ;
            } else {
                if (((i64)(mm_decls_ttbasetype[(t)])==(i64)27)) {
                    msysc_m_print_startfile(dev);
                    msysc_m_print_u64((*(*p).qvalue).lower,NULL);
                    msysc_m_print_nogap();
                    msysc_m_print_str((byte*)"..",NULL);
                    msysc_m_print_nogap();
                    msysc_m_print_i64((*(*p).qvalue).upper,NULL);
                    msysc_m_print_end();
                    ;
                } else {
                    msysc_m_print_startcon();
                    msysc_m_print_str((byte*)"TYPENAME(T)=",NULL);
                    msysc_m_print_str(mm_lib_typename(t),NULL);
                    msysc_m_print_str(mm_lib_typename((i64)(mm_decls_ttbasetype[(t)])),NULL);
                    msysc_m_print_newline();
                    msysc_m_print_end();
                    ;
                    msysc_m_print_startfile(dev);
                    msysc_m_print_str((byte*)"<PRINTUNIT BAD CONST PROBABLY VOID",NULL);
                    msysc_m_print_end();
                    ;
                };
            };
        };
        msysc_m_print_startfile(dev);
        msysc_m_print_str((byte*)" ",NULL);
        msysc_m_print_nogap();
        msysc_m_print_str(mm_lib_typename(t),NULL);
        msysc_m_print_end();
        ;
        if (!!((i64)((*p).isastring))) {
            msysc_m_print_startfile(dev);
            msysc_m_print_str((byte*)" <isstr>",NULL);
            msysc_m_print_end();
            ;
        };
        if (!!((i64)((*p).whenlabel))) {
            msysc_m_print_startfile(dev);
            msysc_m_print_str((byte*)" *L",NULL);
            msysc_m_print_nogap();
            msysc_m_print_i64((*p).whenlabel,NULL);
            msysc_m_print_end();
            ;
        };
    }else if (((i64)((*p).tag)==(i64)6)) {
        msysc_m_print_startfile(dev);
        msysc_m_print_str((*p).svalue,NULL);
        msysc_m_print_str((byte*)"Len:",NULL);
        msysc_m_print_i64((*p).slength,NULL);
        msysc_m_print_end();
        ;
    }else if (((i64)((*p).tag)==(i64)100)) {
        msysc_m_print_startfile(dev);
        msysc_m_print_str(mm_lib_typename((i64)((*p).mode)),NULL);
        msysc_m_print_str(mm_lib_typename((*p).value),NULL);
        msysc_m_print_end();
        ;
    }else if (((i64)((*p).tag)==(i64)101)) {
        msysc_m_print_startfile(dev);
        msysc_m_print_str((mm_tables_jtagnames[((i64)((*p).opcode))] + (i64)2),NULL);
        msysc_m_print_end();
        ;
    }else if (((i64)((*p).tag)==(i64)135)) {
        msysc_m_print_startfile(dev);
        msysc_m_print_str((mm_tables_bitfieldnames[((i64)((*p).opcode))-1] + (i64)3),NULL);
        msysc_m_print_end();
        ;
    }else if (((i64)((*p).tag)==(i64)96) || ((i64)((*p).tag)==(i64)99)) {
        msysc_m_print_startfile(dev);
        msysc_m_print_str(mm_tables_convnames[((i64)((*p).opcode))],NULL);
        msysc_m_print_str((byte*)" to:",NULL);
        msysc_m_print_str(mm_lib_strmode((i64)((*p).newmode),(i64)1),NULL);
        msysc_m_print_end();
        ;
    }else if (((i64)((*p).tag)==(i64)14) || ((i64)((*p).tag)==(i64)19)) {
        msysc_m_print_startfile(dev);
        msysc_m_print_str((byte*)"Len:",NULL);
        msysc_m_print_i64((*p).length,NULL);
        msysc_m_print_end();
        ;
    }else if (((i64)((*p).tag)==(i64)89)) {
        msysc_m_print_startfile(dev);
        msysc_m_print_str((byte*)"Offset:",NULL);
        msysc_m_print_i64((*p).offset,NULL);
        msysc_m_print_end();
        ;
    }else if (((i64)((*p).tag)==(i64)82) || ((i64)((*p).tag)==(i64)93)) {
    }else if (((i64)((*p).tag)==(i64)214) || ((i64)((*p).tag)==(i64)212) || ((i64)((*p).tag)==(i64)211) || ((i64)((*p).tag)==(i64)213)) {
        msysc_m_print_startfile(dev);
        msysc_m_print_str((byte*)"#",NULL);
        msysc_m_print_nogap();
        msysc_m_print_i64((*p).index,NULL);
        msysc_m_print_end();
        ;
    };
    if (!!((i64)((*p).isconst))) {
        msysc_m_print_startfile(dev);
        msysc_m_print_str((byte*)" Is const",NULL);
        msysc_m_print_end();
        ;
    };
    msysc_m_print_startfile(dev);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    mm_diags_printunitlist(dev,(*p).a,(level + (i64)1),(byte*)"1");
    mm_diags_printunitlist(dev,(*p).b,(level + (i64)1),(byte*)"2");
    if (((i64)((*p).tag) != (i64)1)) {
        mm_diags_printunitlist(dev,(*p).c,(level + (i64)1),(byte*)"3");
    };
}

static void mm_diags_printunitlist(void * dev,struct mm_decls_unitrec * p,i64 level,byte * prefix) {
    if ((p == 0)) {
        return;
    };
    L479 :;
    while (!!(p)) {
        mm_diags_printunit(p,level,prefix,dev);
        p = (*p).nextunit;
L480 :;
    }L481 :;
    ;
}

static byte * mm_diags_getprefix(i64 level,byte * prefix,struct mm_decls_unitrec * p) {
    static byte str[1024];
    byte indentstr[1024];
    byte modestr[16384];
    byte *  isexpr;
    i64 av_1;
    indentstr[((i64)1)-1] = (u64)0u;
    if ((level > (i64)10)) {
        level = (i64)10;
    };
    av_1 = level;
    while (av_1-- > 0) {
L482 :;
        strcat((i8 *)(indentstr),(i8 *)((byte*)"- "));
L483 :;
    }L484 :;
    ;
    isexpr = (byte*)"S";
    if (!!((u64)(mm_tables_jisexpr[((i64)((*p).tag))]))) {
        isexpr = (byte*)"E";
    };
    if (((i64)((*p).tag)==(i64)196) || ((i64)((*p).tag)==(i64)218) || ((i64)((*p).tag)==(i64)216) || ((i64)((*p).tag)==(i64)221)) {
        if (((i64)((*p).mode) == (i64)0)) {
            isexpr = (byte*)"S";
        };
    };
    msysc_m_print_startstr(modestr);
    msysc_m_print_setfmt((byte*)"# #:#");
    msysc_m_print_str(isexpr,NULL);
    msysc_m_print_str((!!((i64)((*p).popflag))?(byte*)"POP":(byte*)"---"),NULL);
    msysc_m_print_str(mm_lib_strmode((i64)((*p).mode),(i64)1),NULL);
    msysc_m_print_end();
    ;
    modestr[((i64)256)-1] = (u64)0u;
    strcat((i8 *)(modestr),(i8 *)((byte*)"-----------------------------"));
    modestr[((i64)17)-1] = ' ';
    modestr[((i64)18)-1] = (u64)0u;
    strcpy((i8 *)(str),(i8 *)(mm_diags_getlineinfok()));
    strcat((i8 *)(str),(i8 *)(modestr));
    strcat((i8 *)(str),(i8 *)(indentstr));
    strcat((i8 *)(str),(i8 *)(prefix));
    if (!!((u64)((*prefix)))) {
        strcat((i8 *)(str),(i8 *)((byte*)" "));
    };
    return str;
}

static byte * mm_diags_getlineinfok(void) {
    static byte str[40];
    msysc_m_print_startstr(str);
    msysc_m_print_i64(mm_diags_currlineno,(byte*)"z4");
    msysc_m_print_nogap();
    msysc_m_print_str((byte*)" ",NULL);
    msysc_m_print_end();
    ;
    return str;
}

void mm_genc64_do_codegen1(byte * outfilesource,i64 fshowpcl1) {
    mm_genc_codegen_clang(outfilesource);
}

void mm_genc64_do_codegen2(i64 fshowmcl1) {
}

void mm_genc64_do_writesource(byte * outfilesource,i64 dontlink) {
    mlib_writefile(outfilesource,(byte *)(mm_decls_moduletable[((i64)1)].clangstr),mm_decls_moduletable[((i64)1)].strlength);
}

void mm_genc64_do_link(byte * cfile,byte * exefile,byte * linkoption,i64 ccompiler,i64 optimise) {
    if (!!(mm_decls_islinux)) {
        mm_genc64_do_link_lin(cfile,exefile,linkoption,ccompiler,optimise);
    } else {
        mm_genc64_do_link_win(cfile,exefile,linkoption,ccompiler,optimise);
    };
}

void mm_genc64_do_link_win(byte * cfile,byte * exefile,byte * linkoption,i64 ccompiler,i64 optimise) {
    byte str[256];
    i64 status;
    i64 doobj;
    doobj = mlib_eqstring(linkoption,(byte*)"obj");
    if ((ccompiler==(i64)1)) {
        msysc_m_print_startstr(str);
        msysc_m_print_setfmt((byte*)"gcc -m64 # # -o# #");
        msysc_m_print_str((!!(doobj)?(byte*)"-c":(byte*)""),NULL);
        msysc_m_print_str((!!(optimise)?(byte*)"-O3":(byte*)""),NULL);
        msysc_m_print_str(exefile,NULL);
        msysc_m_print_str(cfile,NULL);
        msysc_m_print_end();
        ;
    }else if ((ccompiler==(i64)2)) {
        msysc_m_print_startstr(str);
        msysc_m_print_setfmt((byte*)"tcc # -o# # #");
        msysc_m_print_str((!!(doobj)?(byte*)"-c":(byte*)""),NULL);
        msysc_m_print_str(exefile,NULL);
        msysc_m_print_str(cfile,NULL);
        msysc_m_print_str((!!(doobj)?(byte*)"":(byte*)"c:\\windows\\system32\\user32.dll"),NULL);
        msysc_m_print_end();
        ;
    }else if ((ccompiler==(i64)3)) {
        msysc_m_print_startstr(str);
        msysc_m_print_setfmt((byte*)"bcc # -out:# #");
        msysc_m_print_str((!!(doobj)?(byte*)"-c":(byte*)""),NULL);
        msysc_m_print_str(exefile,NULL);
        msysc_m_print_str(cfile,NULL);
        msysc_m_print_end();
        ;
    };
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"W:Invoking C compiler:",NULL);
    msysc_m_print_str(str,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    status = oslinux_os_execwait(str,(i64)0,(byte *)(0));
    if ((status == (i64)0)) {
        return;
    };
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"M compiler: couldn't compile intermediate C",NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    exit((i64)0);
}

void mm_genc64_do_link_lin(byte * cfile,byte * exefile,byte * linkoption,i64 ccompiler,i64 optimise) {
    byte str[256];
    i64 status;
    i64 doobj;
    i64 i;
    doobj = mlib_eqstring(linkoption,(byte*)"obj");
    if ((ccompiler==(i64)1)) {
        msysc_m_print_startstr(str);
        msysc_m_print_setfmt((byte*)"gcc # # -o# # -lm -ldl");
        msysc_m_print_str((!!(doobj)?(byte*)"-c":(byte*)""),NULL);
        msysc_m_print_str((!!(optimise)?(byte*)"-O3":(byte*)""),NULL);
        msysc_m_print_str(exefile,NULL);
        msysc_m_print_str(cfile,NULL);
        msysc_m_print_end();
        ;
    }else if ((ccompiler==(i64)2)) {
        msysc_m_print_startstr(str);
        msysc_m_print_setfmt((byte*)"tcc # -o# # -lm -ldl");
        msysc_m_print_str((!!(doobj)?(byte*)"-c":(byte*)""),NULL);
        msysc_m_print_str(exefile,NULL);
        msysc_m_print_str(cfile,NULL);
        msysc_m_print_end();
        ;
    }else if ((ccompiler==(i64)3)) {
        mlib_abortprogram((byte*)"bcc not available on Linux");
    };
    L485 :;
    for (i=(i64)1;i<=mm_decls_ncclibs;++i) {
L486 :;
        strcat((i8 *)(str),(i8 *)((byte*)" -l"));
        strcat((i8 *)(str),(i8 *)(mm_decls_cclibtable[(i)-1]));
L487 :;
    }L488 :;
    ;
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"L:Invoking C compiler:",NULL);
    msysc_m_print_str(str,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    status = oslinux_os_execwait(str,(i64)0,(byte *)(0));
    if ((status == (i64)0)) {
        return;
    };
    msysc_m_print_startcon();
    msysc_m_print_str((byte*)"M compiler: couldn't compile intermediate C",NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    exit((i64)0);
}

void mm_genc64_showhelp(void) {
    static byte *  mchelptext = (byte*)"'MC' M Compiler Using C Intermediate - Windows Version\r\n\r\nWhole-program compiler builds entire program from the lead module\r\ninto a single intermediate C file, then invokes a C compiler to\r\ncreate the executable:\r\n\r\n    mc main              # Create main.exe from lead module main.m\r\n\r\n    mc main.m            # Same (.m extension is default)\r\n\r\n    mc -c main           # Create single-file main.c representation\r\n\r\nOptions:\r\n\r\n    -c                    # Generate only intermediate C output file\r\n    -exe                  # Generate executable via C compiler (default)\r\n    -obj                  # Generate single object file (bcc generates .asm)\r\n\r\n    -out:file             # Name of output file \r\n\r\n    -gcc                  # Use gcc to compile the C file (default)\r\n    -tcc                  # Use tcc\r\n    -bcc                  # Use bcc\r\n\r\n    -opt                  # use -O3 when using gcc\r\n\r\n    -run                  # For -exe mode only: run resulting executable\r\n\r\n    @file                 # Read options from file\r\n\r\nExample:\r\n\r\n     mc -run prog : abc def\r\n\r\nAny parameters for the new program must follow \" : \" (spaces needed).\r\n";
    static byte *  muhelptext = (byte*)"'MU' M Compiler Using C Intermediate - Linux Version\r\n\r\nWhole-program compiler builds entire program from the lead module\r\ninto a single intermediate C file, then invokes a C compiler to\r\ncreate the executable:\r\n\r\n    ./mu main              # Compile main.m lead module to executable via gcc/tcc\r\n    ./mu -c main           # Create single-file main.c representation\r\n\r\nOptions (for extras see mu32.c or mu64.c file):\r\n\r\n    -exe                  # Generate executable via C compiler (default)\r\n    -c                    # Generate only intermediate C output file\r\n    -obj                  # Generate single object file\r\n\r\n    -out:file             # Name of output file \r\n\r\n    -gcc                  # Use gcc to compile the C file (default)\r\n    -tcc                  # Use tcc (needs to be installed)\r\n\r\n    -opt                  # use -O3 when using gcc\r\n    -run                  # For -exe mode only: run resulting executable\r\n    @file                 # Read options from file\r\n";
    if (!!(mm_decls_islinux)) {
        msysc_m_print_startcon();
        msysc_m_print_str(muhelptext,NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
    } else {
        msysc_m_print_startcon();
        msysc_m_print_str(mchelptext,NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
    };
}

i64 mm_genc_codegen_clang(byte * cfilename) {
    if (!!(mm_decls_fverbose)) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"Generating C code:",NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
    };
    mm_genc_allsymbols = (mm_genc_allsymbolsx = (struct mm_genc_stlinkrec *)(0));
    mm_genc_scansymbol(mm_decls_stprogram);
    mlib_gs_init(mm_lib_dest);
    mm_libc_ccinitline();
    mm_genc_do_infoheader(cfilename);
    mm_genc_do_cheader(cfilename);
    mm_genc_do_alltypes();
    mm_genc_do_allprocdecls();
    mm_genc_do_allvars();
    mm_genc_do_allprocdefs();
    mm_libc_cclinecomment((byte*)"********** End of C Code **********");
    mm_decls_moduletable[((i64)1)].clangstr = (*mm_lib_dest).strptr;
    mm_decls_moduletable[((i64)1)].strlength = (i64)((*mm_lib_dest).length);
    return (i64)1;
}

static void mm_genc_do_infoheader(byte * cfilename) {
    i64 i;
    if (!!(mm_decls_infotext)) {
        if (((i64)(strlen((i8 *)(mm_decls_infotext))) >= (i64)4096)) {
            mm_support_gerror((byte*)"infotext too big",(struct mm_decls_unitrec *)(0));
        };
        mm_libc_ccstrline(mm_decls_infotext);
    };
    mm_libc_ccstrline((byte*)"/*");
    mm_libc_ccstrline((byte*)"  M to C  Whole Program Translator");
    mm_libc_ccstr((byte*)"  Input:  ",(i64)0);
    mm_libc_ccstr(mm_decls_inputfiles[((i64)1)],(i64)0);
    mm_libc_ccstrline((byte*)" plus imported modules");
    mm_libc_ccstr((byte*)"  Output: ",(i64)0);
    mm_libc_ccstr(cfilename,(i64)0);
    mm_libc_ccstrline((byte*)" (this file, or renamed from that)");
    mm_libc_ccstrline((byte*)"          File represents entire program");
    if ((mm_decls_targetbits == (i64)32)) {
        mm_libc_ccstrline((byte*)"  Target: C 32-bit");
    } else {
        mm_libc_ccstrline((byte*)"  Target: C 64-bit");
    };
    if (!!(mm_decls_islinux)) {
        mm_libc_ccstrline((byte*)"  OS:     Linux");
    } else {
        mm_libc_ccstrline((byte*)"  OS:     Windows");
    };
    mm_libc_ccstrline((byte*)"");
    mm_libc_ccstrline((byte*)"  Modules:");
    L489 :;
    for (i=(i64)1;i<=mm_decls_nmodules;++i) {
L490 :;
        mm_libc_ccstr((byte*)"  Module ",(i64)0);
        mm_libc_ccint(i);
        mm_libc_ccstr((byte*)": ",(i64)0);
        mm_libc_ccstrline(mm_decls_sourcefilepaths[(mm_decls_moduletable[(i)].fileno)]);
L491 :;
    }L492 :;
    ;
    mm_libc_ccstrline((byte*)"");
    mm_libc_ccstrline((byte*)"*********** Start of C Code **********/");
    mm_libc_ccblank();
}

static void mm_genc_do_cheader(byte * cfilename) {
    mm_libc_ccstrline((byte*)"\r\n#include <stdio.h>\r\n#include <stdlib.h>\r\n#include <stdint.h>\r\n#include <ctype.h>\r\n#include <string.h>\r\n#include <math.h>\r\n#include <time.h>\r\n#include <sys/types.h>\r\n#include <sys/stat.h>\r\n\r\n#pragma pack(1)\r\n\r\ntypedef int8_t i8;\r\ntypedef int16_t i16;\r\ntypedef int32_t i32;\r\ntypedef int64_t i64;\r\n\r\ntypedef uint8_t u8;\r\ntypedef uint16_t u16;\r\ntypedef uint32_t u32;\r\ntypedef uint64_t u64;\r\n\r\ntypedef unsigned char byte;\r\n\r\n#ifndef CALLBACK\r\n#define CALLBACK\r\n#endif\r\n");
    if ((mm_decls_targetbits == (i64)32)) {
    } else {
        mm_libc_ccstrline((byte*)"#if (UINTPTR_MAX<0xFFFFFFFFFFFFFFFF)");
        mm_libc_ccstrline((byte*)"\t#error \"Need 64-bit target. Try -m64\"");
        mm_libc_ccstrline((byte*)"#endif");
    };
    mm_libc_ccsendline();
}

static void mm_genc_do_alltypes(void) {
    i64 i;
    mm_libc_cclinecomment((byte*)"Forward Struct Declarations");
    L493 :;
    for (i=(i64)56;i<=mm_decls_ntypes;++i) {
L494 :;
        if (((i64)(mm_decls_ttbasetype[(i)]) == (i64)32)) {
            mm_genc_do_typedef_fwd(mm_decls_ttnamedef[(i)]);
        };
L495 :;
    }L496 :;
    ;
    mm_libc_ccblank();
    if ((mm_decls_ntypes >= (i64)56)) {
        mm_libc_cclinecomment((byte*)"Struct Definitions");
    };
    L497 :;
    for (i=(i64)56;i<=mm_decls_ntypes;++i) {
L498 :;
        if (((i64)(mm_decls_ttbasetype[(i)]) == (i64)32)) {
            mm_genc_do_typedef(mm_decls_ttnamedef[(i)]);
        };
L499 :;
    }L500 :;
    ;
    mm_libc_ccblank();
}

static void mm_genc_do_allprocdecls(void) {
    struct mm_genc_stlinkrec *  p;
    struct mm_decls_strec *  d;
    mm_libc_cclinecomment((byte*)"PROCDECLS");
    p = mm_genc_allsymbols;
    L501 :;
    while (!!(p)) {
        d = (*p).def;
        if (((i64)((*d).nameid)==(i64)5)) {
            mm_genc_do_procdecl(d);
        }else if (((i64)((*d).nameid)==(i64)6)) {
            if (!(!!(mlib_eqstring((*(*d).owner).name,(byte*)"clibc")))) {
                mm_genc_do_procdecl(d);
            };
        };
        p = (*p).nextsymbol;
L502 :;
    }L503 :;
    ;
    mm_libc_ccblank();
}

static void mm_genc_do_allvars(void) {
    struct mm_genc_stlinkrec *  p;
    struct mm_decls_strec *  d;
    mm_libc_cclinecomment((byte*)"VARS");
    p = mm_genc_allsymbols;
    L504 :;
    while (!!(p)) {
        d = (*p).def;
        if (((i64)((*d).nameid)==(i64)9)) {
            mm_genc_do_vardef(d);
        }else if (((i64)((*d).nameid)==(i64)7)) {
            mm_genc_do_dllvar(d);
        };
        p = (*p).nextsymbol;
L505 :;
    }L506 :;
    ;
    mm_libc_ccblank();
}

static void mm_genc_do_allprocdefs(void) {
    struct mm_genc_stlinkrec *  p;
    struct mm_decls_strec *  d;
    mm_libc_cclinecomment((byte*)"PROCDEFS");
    p = mm_genc_allsymbols;
    L507 :;
    while (!!(p)) {
        d = (*p).def;
        if ((((i64)((u64)((*d).nameid)) == (i64)5) && !!((*d).code))) {
            mm_genc_genprocdef(d);
        };
        p = (*p).nextsymbol;
L508 :;
    }L509 :;
    ;
    mm_libc_ccblank();
}

static void mm_genc_do_typedef_fwd(struct mm_decls_strec * d) {
    if (((i64)(mm_decls_ttbasetype[((i64)((*d).mode))]) != (i64)32)) {
        return;
    };
    mm_libc_genrecordfwd((i64)((*d).mode));
}

static void mm_genc_do_typedef(struct mm_decls_strec * d) {
    if (((i64)(mm_decls_ttbasetype[((i64)((*d).mode))]) != (i64)32)) {
        return;
    };
    mm_libc_genrecorddef((i64)((*d).mode));
}

static void mm_genc_do_procdecl(struct mm_decls_strec * d) {
    if (((i64)((u64)((*d).imported)) == (i64)2)) {
        return;
    };
    if (((*d).code == 0)) {
        mm_libc_ccstr((byte*)"extern ",(i64)0);
    } else if (!!((u64)((*d).isglobal))) {
    } else {
        mm_libc_ccstr((byte*)"static ",(i64)0);
    };
    mm_libc_ccstr(mm_libc_strprocsig(d,(byte *)(0),(i64)1),(i64)0);
    mm_libc_ccstrline((byte*)";");
}

static void mm_genc_do_vardef(struct mm_decls_strec * d) {
    if (!(!!((u64)((*d).isglobal)))) {
        mm_libc_ccstr((byte*)"static ",(i64)0);
    };
    mm_libc_ccstr(mm_libc_strmodec((i64)((*d).mode),mm_libc_getfullnamec(d),(i64)1),(i64)0);
    if (!!((*d).code)) {
        mm_libc_ccstr((byte*)" = ",(i64)0);
        mm_libc_do_initdata((*d).code,(i64)0,(i64)0);
    } else if (((u64)((*(*d).name)) == '_')) {
        if (!!(mlib_eqstring((*d).name,(byte*)"_fnnprocs"))) {
            mm_genc_writefn_nprocs();
        } else if (!!(mlib_eqstring((*d).name,(byte*)"_fnaddresses"))) {
            mm_genc_writefn_addresses();
        } else if (!!(mlib_eqstring((*d).name,(byte*)"_fnnames"))) {
            mm_genc_writefn_names();
        } else if (!!(mlib_eqstring((*d).name,(byte*)"_fnnexports"))) {
            mm_genc_writefn_nexports();
        } else if (!!(mlib_eqstring((*d).name,(byte*)"_fnexports"))) {
            mm_genc_writefn_exports();
        };
    } else if (!!((u64)((*d).at))) {
        mm_support_gerror((byte*)"MODULE@",(struct mm_decls_unitrec *)(0));
    };
    mm_libc_ccstrline((byte*)";");
}

static void mm_genc_do_dllvar(struct mm_decls_strec * d) {
    mm_libc_ccstr((byte*)"extern ",(i64)0);
    mm_libc_ccstr(mm_libc_strmodec((i64)((*d).mode),(*d).name,(i64)1),(i64)0);
    mm_libc_ccstrline((byte*)";");
}

static void mm_genc_addsymbol(struct mm_decls_strec * d) {
    struct mm_genc_stlinkrec *  p;
    p = (struct mm_genc_stlinkrec *)(mlib_pcm_alloc((i64)16));
    (*p).def = d;
    (*p).nextsymbol = (struct mm_genc_stlinkrec *)(0);
    if ((mm_genc_allsymbols == 0)) {
        mm_genc_allsymbols = p;
    } else {
        (*mm_genc_allsymbolsx).nextsymbol = p;
    };
    mm_genc_allsymbolsx = p;
}

static void mm_genc_scansymbol(struct mm_decls_strec * d) {
    struct mm_decls_strec *  e;
    mm_genc_addsymbol(d);
    if (((i64)((*d).nameid)==(i64)5) || ((i64)((*d).nameid)==(i64)4) || ((i64)((*d).nameid)==(i64)6)) {
        return;
    };
    e = (*d).deflist;
    L510 :;
    while (!!(e)) {
        mm_genc_scansymbol(e);
        e = (*e).nextdef;
L511 :;
    }L512 :;
    ;
}

static void mm_genc_genlocalvar(struct mm_decls_strec * d) {
    if (((i64)((u64)((*d).nameid)) == (i64)9)) {
        mm_libc_ccstr((byte*)"static ",(i64)1);
        mm_libc_ccstr(mm_libc_strmodec((i64)((*d).mode),(*d).name,(i64)1),(i64)0);
    } else {
        mm_libc_ccstr(mm_libc_strmodec((i64)((*d).mode),(*d).name,(i64)1),(i64)1);
    };
    if (!!((*d).code)) {
        mm_libc_ccstr((byte*)" = ",(i64)0);
        mm_libc_do_initdata((*d).code,(i64)0,(i64)0);
    } else if (!!((u64)((*d).at))) {
        mm_support_gerror((byte*)"LOCAL@",(struct mm_decls_unitrec *)(0));
    };
    mm_libc_ccstrline((byte*)";");
}

static void mm_genc_genprocdef(struct mm_decls_strec * p) {
    byte str[256];
    struct mm_decls_strec *  d;
    i64 i;
    if (((i64)((*p).mode) != (i64)0)) {
        if (!(!!(mm_lib_checkblockreturn((*p).code)))) {
            mm_support_gerror_s((byte*)"C:Function needs explicit return: ",(*p).name,(struct mm_decls_unitrec *)(0));
        };
    };
    if (!!(mlib_eqstring((*p).name,(byte*)"start"))) {
        mm_libc_ccstrline((byte*)"// START");
    };
    mm_decls_currproc = p;
    if (!(!!((u64)((*p).isglobal)))) {
        mm_libc_ccstr((byte*)"static ",(i64)0);
    };
    if (!!(mm_lib_iscallbackfn(p))) {
        mm_libc_ccstr((byte*)"CALLBACK ",(i64)0);
    };
    mm_libc_ccstr(mm_libc_strprocsig(p,(byte *)(0),(i64)1),(i64)0);
    mm_libc_ccstrline((byte*)" {");
    d = (*p).deflist;
    L513 :;
    while (!!(d)) {
        switch ((i64)((*d).nameid)) {
        case 9:;
        case 10:;
        {
            if ((!!((u64)((*d).used)) || !!((*d).code))) {
                mm_genc_genlocalvar(d);
            };
        }break;
        case 8:;
        {
        }break;
        case 11:;
        {
        }break;
        case 15:;
        {
        }break;
        case 4:;
        {
        }break;
        case 5:;
        {
        }break;
        case 18:;
        {
        }break;
        default: {
            msysc_m_print_startstr(str);
            msysc_m_print_setfmt((byte*)"Can't output: # in # : #");
            msysc_m_print_str((*d).name,NULL);
            msysc_m_print_str((*p).name,NULL);
            msysc_m_print_str(mm_tables_namenames[((i64)((*d).nameid))],NULL);
            msysc_m_print_end();
            ;
            mm_support_gerror(str,(struct mm_decls_unitrec *)(0));
        }
        } //SW
;
        d = (*d).nextdef;
L514 :;
    }L515 :;
    ;
    mm_blockc_blocklevel = (i64)0;
    mm_blockc_do_block((*p).code);
    mm_libc_ccblank();
    mm_libc_ccblank();
    if (!!(mlib_eqstring((*p).name,(byte*)"start"))) {
        mm_libc_ccstrline((byte*)"int main(int nargs, char** args) {");
        mm_libc_ccstrline((byte*)"int i;");
        mm_libc_ccstrline((byte*)"\tmsysc_nsysparams=nargs;");
        mm_libc_ccstrline((byte*)"\tif (msysc_nsysparams>nargs) {puts(\"Too many params\"); exit(1);}");
        mm_libc_ccstrline((byte*)"\tfor (i=1; i<=nargs; ++i) msysc_sysparams[i-1]=(byte*)args[i-1];");
        mm_libc_ccsendline();
        L516 :;
        for (i=mm_decls_nmodules;i>=(i64)1;--i) {
L517 :;
            d = mm_decls_moduletable[(i)].stinitproc;
            if (!!(d)) {
                mm_libc_ccchar((i64)9);
                mm_libc_ccstr(mm_libc_getfullnamec(d),(i64)0);
                mm_libc_ccstrline((byte*)"();");
            };
L518 :;
        }L519 :;
        ;
        mm_libc_ccsendline();
        mm_libc_ccstrline((byte*)"\tstart();");
        mm_libc_ccstrline((byte*)"\treturn 0;");
        mm_libc_ccstrline((byte*)"}");
        mm_libc_ccstrline((byte*)"");
    };
}

static void mm_genc_writefn_nprocs(void) {
    mm_libc_ccstr((byte*)"=",(i64)0);
    mm_libc_ccint(mm_genc_nallprocs);
}

static void mm_genc_writefn_nexports(void) {
    mm_libc_ccstr((byte*)"=",(i64)0);
    mm_libc_ccint(mm_genc_nexports);
}

static void mm_genc_writefn_names(void) {
    i64 i;
    struct mm_decls_strec *  d;
    mm_libc_ccstrline((byte*)"= {");
    mm_genc_nallprocs = (i64)0;
    L520 :;
    for (i=(i64)1;i<=mm_decls_nmodules;++i) {
L521 :;
        d = (*mm_decls_moduletable[(i)].stmodule).deflist;
        L524 :;
        while (!!(d)) {
            if (((i64)((u64)((*d).nameid)) == (i64)5)) {
                ++mm_genc_nallprocs;
                mm_libc_ccstr((byte*)"(byte*)\"",(i64)1);
                mm_libc_ccstr((*d).name,(i64)0);
                mm_libc_ccstrline((byte*)"\",");
            };
            d = (*d).nextdef;
L525 :;
        }L526 :;
        ;
L522 :;
    }L523 :;
    ;
    mm_libc_ccstr((byte*)"(byte*)\"\"}",(i64)0);
}

static void mm_genc_writefn_addresses(void) {
    i64 i;
    struct mm_decls_strec *  d;
    mm_libc_ccstrline((byte*)"= {");
    mm_genc_nallprocs = (i64)0;
    L527 :;
    for (i=(i64)1;i<=mm_decls_nmodules;++i) {
L528 :;
        d = (*mm_decls_moduletable[(i)].stmodule).deflist;
        L531 :;
        while (!!(d)) {
            if (((i64)((u64)((*d).nameid)) == (i64)5)) {
                ++mm_genc_nallprocs;
                mm_libc_ccstr((byte*)"&",(i64)1);
                mm_libc_ccstr(mm_libc_getprocname(d),(i64)0);
                mm_libc_ccstrline((byte*)",");
            };
            d = (*d).nextdef;
L532 :;
        }L533 :;
        ;
L529 :;
    }L530 :;
    ;
    mm_libc_ccstr((byte*)"0}",(i64)0);
}

static void mm_genc_writefn_exports(void) {
    i64 i;
    i64 nprocs;
    i64 nparams;
    struct mm_decls_strec *  d;
    struct mm_decls_strec *  e;
    struct mm_decls_strec *  params[100];
    i64 j;
    mm_libc_ccstrline((byte*)"= {");
    nprocs = (mm_genc_nexports = (i64)0);
    L534 :;
    for (i=(i64)1;i<=mm_decls_nmodules;++i) {
L535 :;
        d = (*mm_decls_moduletable[(i)].stmodule).deflist;
        L538 :;
        while (!!(d)) {
            if (((i64)((u64)((*d).nameid)) == (i64)5)) {
                ++nprocs;
                if (((i64)((u64)((*d).isglobal)) == (i64)2)) {
                    ++mm_genc_nexports;
                    mlib_gs_str(mm_lib_dest,(byte*)"{");
                    mlib_gs_strint(mm_lib_dest,nprocs);
                    mlib_gs_str(mm_lib_dest,(byte*)", ");
                    nparams = (i64)0;
                    e = (*d).paramlist;
                    L541 :;
                    while (!!(e)) {
                        if (((i64)((u64)((*e).nameid)) == (i64)11)) {
                            ++nparams;
                            if ((nparams > (i64)100)) {
                                mm_support_gerror((byte*)"Export: too many params",(struct mm_decls_unitrec *)(0));
                            };
                            params[(nparams)-1] = e;
                        };
                        e = (*e).nextdef;
L542 :;
                    }L543 :;
                    ;
                    nparams=(nparams<(i64)12?nparams:(i64)12);
;
                    mlib_gs_strint(mm_lib_dest,mm_lib_getpacktype((i64)((*d).mode)));
                    mlib_gs_char(mm_lib_dest,(i64)44);
                    mlib_gs_strint(mm_lib_dest,nparams);
                    mlib_gs_str(mm_lib_dest,(byte*)", {");
                    L544 :;
                    for (j=(i64)1;j<=(i64)12;++j) {
L545 :;
                        if ((j != (i64)1)) {
                            mlib_gs_char(mm_lib_dest,(i64)44);
                        };
                        if ((j <= nparams)) {
                            e = params[(j)-1];
                            mm_decls_optflag = (!!((u64)((*e).optional))?(i64)64:(i64)0);
                            mlib_gs_strint(mm_lib_dest,(mm_lib_getpacktype((i64)((*e).mode)) + mm_decls_optflag));
                        } else {
                            mlib_gs_char(mm_lib_dest,(i64)48);
                        };
L546 :;
                    }L547 :;
                    ;
                    mlib_gs_strln(mm_lib_dest,(byte*)"}},");
                };
            };
            d = (*d).nextdef;
L539 :;
        }L540 :;
        ;
L536 :;
    }L537 :;
    ;
    mlib_gs_strln(mm_lib_dest,(byte*)"\t{0, 0,0, {0,0,0, 0,0,0, 0,0,0, 0,0,0}}}");
}

static void mm_libc_Dinit(void) {
    mm_libc_zerounit = (struct mm_decls_unitrec *)(malloc((u64)((i64)16)));
    mm_libc_nilunit = (struct mm_decls_unitrec *)(malloc((u64)((i64)16)));
}

void mm_libc_cccomment(byte * s) {
    mm_libc_ccstr((byte*)"/* ",(i64)0);
    mm_libc_ccstr(s,(i64)0);
    mm_libc_ccstr((byte*)" */",(i64)0);
    mm_libc_ccsendline();
}

void mm_libc_ccblank(void) {
    mm_libc_ccsendline();
}

void mm_libc_cclinecomment(byte * s) {
    mm_libc_cccomment(s);
}

void mm_libc_ccchar(i64 c) {
    (*mm_libc_clineptr) = (u64)(c);
    ++mm_libc_clineptr;
}

void mm_libc_cctab(i64 level) {
    i64 av_1;
    av_1 = (level * (i64)4);
    while (av_1-- > 0) {
L548 :;
        (*mm_libc_clineptr++) = ' ';
L549 :;
    }L550 :;
    ;
}

void mm_libc_ccstr(byte * s,i64 level) {
    if (!!(level)) {
        mm_libc_cctab(level);
    };
    L551 :;
    while (!!((u64)((*s)))) {
        (*mm_libc_clineptr++) = (u64)((*s++));
L552 :;
    }L553 :;
    ;
}

void mm_libc_ccstrline(byte * cstr) {
    mm_libc_ccstr(cstr,(i64)0);
    mm_libc_ccsendline();
}

void mm_libc_ccstrsemi(byte * cstr) {
    mm_libc_ccstr(cstr,(i64)0);
    mm_libc_ccchar((i64)59);
    mm_libc_ccsendline();
}

void mm_libc_ccstrsemiu(struct mm_decls_unitrec * p) {
    mm_blockc_evalunit(p);
    mm_libc_ccchar((i64)59);
    mm_libc_ccsendline();
}

void mm_libc_ccsendline(void) {
    (*mm_libc_clineptr) = (u64)0u;
    mlib_gs_strln(mm_lib_dest,mm_libc_clinebuffer);
    mm_libc_ccinitline();
}

void mm_libc_ccint(i64 a) {
    mm_libc_ccstr(msysc_strint(a,(byte *)(0)),(i64)0);
}

void mm_libc_ccinitline(void) {
    mm_libc_clineptr = mm_libc_clinebuffer;
    mm_libc_clineend = ((mm_libc_clineptr + (i64)4096) - (i64)1);
}

byte * mm_libc_strmodec(i64 m,byte * name,i64 addtab) {
    static byte str[1024];
    byte *  oldlineptr;
    oldlineptr = mm_libc_clineptr;
    mm_libc_clineptr = str;
    mm_libc_strmodec2(m,name,addtab);
    (*mm_libc_clineptr) = (u64)0u;
    mm_libc_clineptr = oldlineptr;
    return str;
}

void mm_libc_strmodec2(i64 m,byte * name,i64 addtab) {
    byte *  suffix;
    byte *  sp;
    byte *  spsuffix;
    byte str[1024];
    byte buffer[1024];
    i64 target;
    if ((!!(name) && !!((u64)((*name))))) {
        suffix = name;
        strcpy((i8 *)(str),(i8 *)((byte*)" "));
        strcat((i8 *)(str),(i8 *)(name));
        spsuffix = str;
        sp = (byte*)"  ";
        if (((i64)((*name))==(i64)91) || ((i64)((*name))==(i64)40) || ((i64)((*name))==(i64)42)) {
            //lab1:
L554 :;
;
            sp = (byte*)" ";
        } else {
            if (!(!!(addtab))) {
                goto L554 ;
;
            };
        };
    } else {
        sp = (suffix = (spsuffix = (byte*)""));
    };
    if (((i64)(mm_decls_ttbasetype[(m)])==(i64)3) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)8) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)4) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)9) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)11) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)12) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)0)) {
        mm_libc_ccstr(mm_libc_strmodex(m),(i64)0);
        mm_libc_ccstr(spsuffix,(i64)0);
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)20)) {
        if (((i64)(mm_decls_ttbasetype[((i64)(mm_decls_tttarget[(m)]))])==(i64)41)) {
            if (((i64)(mm_decls_tttarget[(m)]) == (i64)41)) {
                mm_libc_ccstr((byte*)"REF PROC",(i64)0);
                mm_libc_ccstr(suffix,(i64)0);
            } else {
                mm_libc_ccstr(mm_libc_strprocsig(mm_decls_ttnamedef[(m)],suffix,(i64)0),(i64)0);
            };
        }else if (((i64)(mm_decls_ttbasetype[((i64)(mm_decls_tttarget[(m)]))])==(i64)54)) {
            mm_libc_ccstr((byte*)"int *",(i64)0);
            mm_libc_ccstr(spsuffix,(i64)0);
        } else {
            target = (i64)(mm_decls_tttarget[(m)]);
            if (((i64)(mm_decls_ttbasetype[(target)]) == (i64)29)) {
                msysc_m_print_startstr(buffer);
                msysc_m_print_setfmt((byte*)"(*#)");
                msysc_m_print_str(suffix,NULL);
                msysc_m_print_end();
                ;
            } else {
                msysc_m_print_startstr(buffer);
                msysc_m_print_setfmt((byte*)"*##");
                msysc_m_print_str(sp,NULL);
                msysc_m_print_str(suffix,NULL);
                msysc_m_print_end();
                ;
            };
            mm_libc_strmodec2(target,buffer,(i64)1);
        };
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)29)) {
        if (!!((i64)(mm_decls_ttlength[(m)]))) {
            msysc_m_print_startstr(buffer);
            msysc_m_print_setfmt((byte*)"#[#]");
            msysc_m_print_str(suffix,NULL);
            msysc_m_print_i64(mm_decls_ttlength[(m)],NULL);
            msysc_m_print_end();
            ;
        } else {
            msysc_m_print_startstr(buffer);
            msysc_m_print_str(suffix,NULL);
            msysc_m_print_nogap();
            msysc_m_print_str((byte*)"[]",NULL);
            msysc_m_print_end();
            ;
        };
        mm_libc_strmodec2((i64)(mm_decls_tttarget[(m)]),buffer,(i64)1);
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)32)) {
        mm_libc_ccstr((byte*)"struct ",(i64)0);
        mm_libc_ccstr(mm_libc_getfullnamec(mm_decls_ttnamedef[(m)]),(i64)0);
        mm_libc_ccstr(spsuffix,(i64)0);
    } else {
        mm_libc_ccstr(mm_libc_strmodex(m),(i64)0);
        mm_libc_ccstr(spsuffix,(i64)0);
    };
}

static byte * mm_libc_strmodex(i64 m) {
    if (((i64)(mm_decls_ttbasetype[(m)])==(i64)3)) {
        return (byte*)"i32";
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)4)) {
        return (byte*)"i64";
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)1)) {
        return (byte*)"i8";
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)2)) {
        return (byte*)"i16";
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)8)) {
        return (byte*)"u32";
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)9)) {
        return (byte*)"u64";
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)6)) {
        return (byte*)"byte";
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)7)) {
        return (byte*)"u16";
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)11)) {
        return (byte*)"float";
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)12)) {
        return (byte*)"double";
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)13)) {
        return (byte*)"byte";
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)15)) {
        return (byte*)"u64";
    } else {
        return mm_lib_strmode(m,(i64)1);
    };
    return (byte*)"";
}

byte * mm_libc_strprocsig(struct mm_decls_strec * p,byte * name,i64 showparamnames) {
    byte paramstr[512];
    byte buffer[512];
    struct mm_decls_strec *  pm;
    i64 rettype;
    byte *  stdcall;
    byte *  scallback;
    pm = (*p).paramlist;
    rettype = (i64)((*p).mode);
    strcpy((i8 *)(paramstr),(i8 *)((byte*)"("));
    if (!!(pm)) {
        L555 :;
        while (!!(pm)) {
            strcat((i8 *)(paramstr),(i8 *)(mm_libc_strmodec((i64)((*pm).mode),(!!(showparamnames)?(*pm).name:(byte*)""),(i64)0)));
            pm = (*pm).nextparam;
            if (!!(pm)) {
                strcat((i8 *)(paramstr),(i8 *)((byte*)","));
            };
L556 :;
        }L557 :;
        ;
        if (!!((u64)((*p).varparams))) {
            if (!!((*p).paramlist)) {
                strcat((i8 *)(paramstr),(i8 *)((byte*)","));
            };
            strcat((i8 *)(paramstr),(i8 *)((byte*)"..."));
        };
        strcat((i8 *)(paramstr),(i8 *)((byte*)")"));
    } else {
        if (!!((u64)((*p).varparams))) {
            strcat((i8 *)(paramstr),(i8 *)((byte*)"..."));
        } else {
            strcat((i8 *)(paramstr),(i8 *)((byte*)"void)"));
        };
    };
    stdcall = (byte*)"";
    if (((i64)((u64)((*p).fflang)) == (i64)1)) {
        if ((mm_decls_targetbits == (i64)64)) {
        } else {
            stdcall = (byte*)"__stdcall ";
        };
    };
    if (!!(mm_lib_iscallbackfn(p))) {
        scallback = (byte*)"gcc_callback";
    } else {
        scallback = (byte*)"";
    };
    if ((name == 0)) {
        msysc_m_print_startstr(buffer);
        msysc_m_print_str(stdcall,NULL);
        msysc_m_print_nogap();
        msysc_m_print_str(mm_libc_getprocname(p),NULL);
        msysc_m_print_nogap();
        msysc_m_print_str(paramstr,NULL);
        msysc_m_print_end();
        ;
    } else {
        msysc_m_print_startstr(buffer);
        msysc_m_print_setfmt((byte*)"#(#*#)#");
        msysc_m_print_str(scallback,NULL);
        msysc_m_print_str(stdcall,NULL);
        msysc_m_print_str(name,NULL);
        msysc_m_print_str(paramstr,NULL);
        msysc_m_print_end();
        ;
    };
    return mlib_pcm_copyheapstring(mm_libc_strmodec(rettype,buffer,!(showparamnames)));
}

byte * mm_libc_getprocname(struct mm_decls_strec * d) {
    byte *  name;
    name = (*d).name;
    if (((i64)((*d).fflang)==(i64)2) || ((i64)((*d).fflang)==(i64)1)) {
        if (!!((*d).truename)) {
            return (*d).truename;
        };
        return name;
    } else {
        if (!!(mlib_eqstring(name,(byte*)"main"))) {
            return (byte*)"main";
        } else if (!!(mlib_eqstring(name,(byte*)"start"))) {
            return (byte*)"start";
        } else {
            return mm_libc_getfullnamec(d);
        };
    };
    return (byte*)"";
}

byte * mm_libc_getfullnamec(struct mm_decls_strec * d) {
    byte *  name;
    byte str[256];
    byte *  pdollar;
    i64 n;
    name = mm_libc_getfullnamec2(d);
    L558 :;
    while (1) {
        pdollar = (byte *)(strchr((i8 *)(name),(i64)36));
        if ((pdollar == 0)) {
            return name;
        };
        n = (pdollar - name);
        if (!!(n)) {
            memcpy((void *)(str),(void *)(name),(u64)(n));
        };
        memcpy((void *)((str + n)),(void *)((byte*)"D"),(u64)((i64)1));
        strcpy((i8 *)(((str + n) + (i64)1)),(i8 *)((pdollar + (i64)1)));
        strcpy((i8 *)(name),(i8 *)(str));
    }L559 :;
    ;
    return name;
}

byte * mm_libc_getfullnamec2(struct mm_decls_strec * d) {
    static byte str[256];
    if (((i64)((*d).nameid)==(i64)5) || ((i64)((*d).nameid)==(i64)9) || ((i64)((*d).nameid)==(i64)4) || ((i64)((*d).nameid)==(i64)8)) {
        if (((i64)((*(*d).owner).nameid)==(i64)2)) {
            strcpy((i8 *)(str),(i8 *)((*(*d).owner).name));
            strcat((i8 *)(str),(i8 *)((byte*)"_"));
            strcat((i8 *)(str),(i8 *)((*d).name));
            return str;
        }else if (((i64)((*(*d).owner).nameid)==(i64)5)) {
            if (((i64)((u64)((*d).nameid)) == (i64)4)) {
                strcpy((i8 *)(str),(i8 *)((*(*(*d).owner).owner).name));
                strcat((i8 *)(str),(i8 *)((byte*)"_"));
                strcat((i8 *)(str),(i8 *)((*(*d).owner).name));
                strcat((i8 *)(str),(i8 *)((byte*)"_"));
                strcat((i8 *)(str),(i8 *)((*d).name));
                return str;
            };
        };
    };
    if (!!((*d).truename)) {
        return (*d).truename;
    };
    return (*d).name;
}

byte * mm_libc_genclabel(i64 n,i64 colon) {
    static byte str[16];
    msysc_m_print_startstr(str);
    msysc_m_print_str((byte*)"L",NULL);
    msysc_m_print_nogap();
    msysc_m_print_i64(n,NULL);
    msysc_m_print_str((!!(colon)?(byte*)":;":(byte*)""),NULL);
    msysc_m_print_end();
    ;
    return str;
}

void mm_libc_genrecorddef(i64 m) {
    struct mm_decls_strec *  q;
    i64 indent;
    i64 index;
    i64 nref;
    i64 target;
    byte flags[8];
    byte f;
    byte *  name;
    struct mm_decls_strec *  d;
    i64 av_1;
    d = mm_decls_ttnamedef[(m)];
    name = (*d).name;
    mm_libc_ccstr((byte*)"struct ",(i64)0);
    mm_libc_ccstr(mm_libc_getfullnamec(d),(i64)0);
    mm_libc_ccstrline((byte*)" {");
    indent = (i64)1;
    q = (*d).deflist;
    L560 :;
    while (!!(q)) {
        memcpy((void *)(&flags),(void *)(&(*q).uflags),(u64)((i64)8));
        index = (i64)1;
        flags[((i64)8)-1] = (u64)((i64)0);
        if (((i64)((*q).nameid)==(i64)12)) {
            L563 :;
            while ((((u64)((f = (u64)(flags[(index)-1]))) == (u64)83u) || ((u64)(f) == (u64)85u))) {
                ++index;
                mm_libc_cctab(indent);
                mm_libc_ccstrline((((u64)(f) == (u64)83u)?(byte*)"struct {":(byte*)"union {"));
                ++indent;
L564 :;
            }L565 :;
            ;
            if (((i64)(mm_decls_ttbasetype[((i64)((*q).mode))]) == (i64)20)) {
                target = (i64)(mm_decls_tttarget[((i64)((*q).mode))]);
                nref = (i64)1;
                L566 :;
                while (((i64)(mm_decls_ttbasetype[(target)]) == (i64)20)) {
                    target = (i64)(mm_decls_tttarget[(target)]);
                    ++nref;
L567 :;
                }L568 :;
                ;
                if ((target == m)) {
                    mm_libc_cctab(indent);
                    mm_libc_ccstr((byte*)"struct ",(i64)0);
                    mm_libc_ccstr(mm_libc_getfullnamec(d),(i64)0);
                    av_1 = nref;
                    while (av_1-- > 0) {
L569 :;
                        mm_libc_ccchar((i64)42);
L570 :;
                    }L571 :;
                    ;
                    mm_libc_ccchar((i64)32);
                    mm_libc_ccstrsemi((*q).name);
                } else {
                    goto L572 ;
;
                };
            } else {
                //normal:
L572 :;
;
                mm_libc_cctab(indent);
                mm_libc_ccstrsemi(mm_libc_strmodec((i64)((*q).mode),(*q).name,(i64)1));
            };
            if (!!((u64)((*q).at))) {
                mm_support_gerror((byte*)"@ in Struct",(struct mm_decls_unitrec *)(0));
            };
            if (((u64)(flags[(index)-1]) == (u64)42u)) {
                ++index;
            };
            L573 :;
            while (((u64)(flags[(index)-1]) == (u64)69u)) {
                --indent;
                ++index;
                mm_libc_cctab(indent);
                mm_libc_ccstrsemi((byte*)"}");
L574 :;
            }L575 :;
            ;
        } else {
            mm_support_gerror((byte*)"Non-field in struct",(struct mm_decls_unitrec *)(0));
        };
        q = (*q).nextdef;
L561 :;
    }L562 :;
    ;
    mm_libc_ccstrsemi((byte*)"}");
    mm_libc_ccblank();
}

void mm_libc_genrecordfwd(i64 m) {
    mm_libc_ccstr((byte*)"struct ",(i64)0);
    mm_libc_ccstrsemi(mm_libc_getfullnamec(mm_decls_ttnamedef[(m)]));
}

void mm_libc_do_initdata(struct mm_decls_unitrec * p,i64 docomma,i64 level) {
    if ((p == 0)) {
        return;
    };
    if (((i64)((*p).tag) != (i64)14)) {
        if ((!!(mm_lib_isstringconst(p)) && !!((i64)((*p).isastring)))) {
            if (((i64)((*p).slength) == (i64)0)) {
                mm_libc_ccstr((byte*)"(byte*)\"\"",(i64)0);
            } else {
                mm_libc_cclongstr((*p).svalue,(i64)((*p).slength));
            };
        } else {
            mm_blockc_evalunit(p);
        };
        if (!!(docomma)) {
            mm_libc_ccchar((i64)44);
        };
        return;
    };
    mm_libc_do_makelist((*p).a,(i64)((*p).length),docomma,(level + (i64)1));
}

void mm_libc_cclongstr(byte * svalue,i64 length) {
    if (((mm_libc_clineptr + (length * (i64)2)) >= mm_libc_clineend)) {
        mm_libc_dxchar((i64)92);
        mm_libc_ccsendline();
        mlib_gs_str(mm_lib_dest,mm_libc_strstringc(svalue,length));
    } else {
        mm_libc_dxstr(mm_libc_strstringc(svalue,length));
    };
}

static void mm_libc_do_makelist(struct mm_decls_unitrec * a,i64 length,i64 docomma,i64 level) {
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  q;
    i64 av_1;
    p = a;
    if (((length <= (i64)10) && ((i64)((*a).tag) != (i64)14))) {
        mm_libc_ccstr((byte*)"{",(i64)0);
        L576 :;
        while (!!(p)) {
            q = (*p).nextunit;
            mm_libc_do_initdata(p,(q != 0),level);
            p = q;
L577 :;
        }L578 :;
        ;
    } else {
        mm_libc_ccstrline((byte*)"{");
        L579 :;
        while (!!(p)) {
            q = (*p).nextunit;
            av_1 = level;
            while (av_1-- > 0) {
L582 :;
                mm_libc_ccstr((byte*)"    ",(i64)0);
L583 :;
            }L584 :;
            ;
            mm_libc_do_initdata(p,(q != 0),level);
            p = q;
            mm_libc_ccstrline((byte*)"");
L580 :;
        }L581 :;
        ;
    };
    mm_libc_ccchar((i64)125);
    if (!!(docomma)) {
        mm_libc_ccchar((i64)44);
    };
}

i64 mm_libc_issimplec(struct mm_decls_unitrec * p) {
    if (((i64)((*p).tag)==(i64)1) || ((i64)((*p).tag)==(i64)3)) {
        return (i64)1;
    };
    return (i64)0;
}

byte * mm_libc_strstringc(byte * s,i64 length) {
    i64 c;
    static byte str[1024];
    byte *  dest;
    byte *  t;
    if ((length > (i64)512)) {
        dest = (byte *)(mlib_pcm_alloc((length * (i64)2)));
    } else {
        dest = str;
    };
    t = dest;
    strcpy((i8 *)(t),(i8 *)((byte*)"(byte*)"));
    t += (i64)(strlen((i8 *)(t)));
    (*t++) = '"';
    L585 :;
    while (!!((c = (i64)((*s++))))) {
        if ((c==(i64)34)) {
            (*t++) = (u64)92u;
            (*t++) = '"';
        }else if ((c==(i64)10)) {
            (*t++) = (u64)92u;
            (*t++) = 'n';
        }else if ((c==(i64)13)) {
            (*t++) = (u64)92u;
            (*t++) = 'r';
        }else if ((c==(i64)9)) {
            (*t++) = (u64)92u;
            (*t++) = 't';
        }else if ((c==(i64)92)) {
            (*t++) = (u64)92u;
            (*t++) = (u64)92u;
        } else {
            if ((c < (i64)32)) {
                (*t++) = (u64)92u;
                (*t++) = (u64)(((c >> (i64)6) + (i64)48));
                (*t++) = (u64)((((c >> (i64)3) & (i64)7) + (i64)48));
                (*t++) = (u64)(((c & (i64)7) + (i64)48));
            } else {
                (*t++) = (u64)(c);
            };
        };
L586 :;
    }L587 :;
    ;
    (*t++) = '"';
    (*t) = (u64)0u;
    return dest;
}

void mm_libc_do_syscallproc(byte * fnname,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    mm_libc_dxstr(fnname);
    mm_libc_dxstr((byte*)"(");
    if (!!(a)) {
        mm_libc_evalsysparam(a);
        if (!!(b)) {
            mm_libc_dxstr((byte*)",");
            mm_libc_evalsysparam(b);
        };
    };
    mm_libc_dxstr((byte*)");");
    mm_libc_ccsendline();
    mm_libc_cctab(mm_blockc_blocklevel);
}

static void mm_libc_evalsysparam(struct mm_decls_unitrec * a) {
    if ((a==(struct mm_decls_unitrec *)(0))) {
    }else if ((a==mm_libc_nilunit)) {
        mm_libc_dxstr((byte*)"NULL");
    }else if ((a==mm_libc_zerounit)) {
        mm_libc_dxstr((byte*)"0");
    } else {
        mm_blockc_evalunit(a);
    };
}

void mm_libc_dxstr(byte * str) {
    L588 :;
    while (!!((u64)((*str)))) {
        (*mm_libc_clineptr) = (u64)((*str));
        ++mm_libc_clineptr;
        ++str;
L589 :;
    }L590 :;
    ;
}

void mm_libc_dxchar(i64 ch) {
    (*mm_libc_clineptr++) = (u64)(ch);
}

void mm_libc_dxint(i64 a) {
    mm_libc_dxstr(msysc_strint(a,(byte *)(0)));
}

void mm_blockc_evalunit(struct mm_decls_unitrec * p) {
    struct mm_decls_unitrec *  a;
    struct mm_decls_unitrec *  b;
    struct mm_decls_strec *  d;
    mm_decls_mlineno = (i64)((*p).lineno);
    a = (*p).a;
    b = (*p).b;
    switch ((i64)((*p).tag)) {
    case 1:;
    {
        mm_blockc_do_const(p);
    }break;
    case 3:;
    {
        mm_libc_dxstr(mm_libc_getfullnamec((*p).def));
    }break;
    case 4:;
    {
        mm_libc_dxchar((i64)40);
        L591 :;
        while ((!!(a) && !!((*a).nextunit))) {
            mm_blockc_evalunit(a);
            mm_libc_dxchar((i64)44);
            a = (*a).nextunit;
L592 :;
        }L593 :;
        ;
        mm_blockc_evalunit(a);
        mm_libc_dxchar((i64)41);
    }break;
    case 5:;
    {
        L594 :;
        while (!!(a)) {
            mm_blockc_evalstmt(a);
            a = (*a).nextunit;
L595 :;
        }L596 :;
        ;
    }break;
    case 189:;
    case 190:;
    case 25:;
    case 26:;
    {
        mm_blockc_do_call(p,a,b);
    }break;
    case 191:;
    {
        mm_blockc_do_return(a);
    }break;
    case 192:;
    case 23:;
    {
        if (((i64)(mm_decls_ttsize[((i64)((*a).mode))])==(i64)1) || ((i64)(mm_decls_ttsize[((i64)((*a).mode))])==(i64)2) || ((i64)(mm_decls_ttsize[((i64)((*a).mode))])==(i64)4) || ((i64)(mm_decls_ttsize[((i64)((*a).mode))])==(i64)8) || ((i64)(mm_decls_ttsize[((i64)((*a).mode))])==(i64)16)) {
            if (((i64)((*p).tag) == (i64)23)) {
                mm_libc_dxchar((i64)40);
            };
            mm_blockc_evalunit(a);
            mm_libc_dxstr((byte*)" = ");
            mm_blockc_evalunit(b);
            if (((i64)((*p).tag) == (i64)23)) {
                mm_libc_dxchar((i64)41);
            };
        } else {
            if (((i64)((*p).tag) == (i64)23)) {
                mm_support_gerror((byte*)"Block assign returning value?",(struct mm_decls_unitrec *)(0));
            };
            mm_blockc_do_blockcopy(a,b);
        };
    }break;
    case 195:;
    {
        mm_blockc_do_to(a,b,(*p).c);
    }break;
    case 196:;
    {
        if ((!!(mm_blockc_isexpr(p)) && !(!!((i64)((*p).ifretflag))))) {
            mm_blockc_do_ifx(a,b,(*p).c,(i64)0);
        } else {
            mm_blockc_do_if(a,b,(*p).c);
        };
    }break;
    case 197:;
    {
        mm_blockc_do_longif(a,b);
    }break;
    case 198:;
    case 199:;
    {
        mm_blockc_do_forup(p,a,b,(*p).c);
    }break;
    case 206:;
    {
        mm_blockc_do_while(a,b);
    }break;
    case 207:;
    {
        mm_blockc_do_repeat(a,b);
    }break;
    case 208:;
    {
        mm_blockc_do_goto(a);
    }break;
    case 210:;
    {
        d = (*p).def;
        if (((i64)((*d).index) == (i64)0)) {
            (*d).index = ++mm_decls_labelno;
        };
        mm_libc_ccstr((byte*)"//",(i64)0);
        mm_libc_ccstr((*d).name,(i64)0);
        mm_libc_ccstrline((byte*)":");
        mm_libc_ccstrline(mm_libc_genclabel((i64)((*d).index),(i64)1));
    }break;
    case 211:;
    case 212:;
    case 213:;
    case 214:;
    {
        mm_blockc_do_exit(p);
    }break;
    case 215:;
    {
        mm_blockc_do_do(a);
    }break;
    case 216:;
    case 217:;
    {
        mm_blockc_do_case(p,a,b,(*p).c);
    }break;
    case 218:;
    case 219:;
    {
        mm_blockc_do_switch(p,a,b,(*p).c);
    }break;
    case 220:;
    {
        mm_blockc_do_swap(a,b);
    }break;
    case 221:;
    {
        mm_blockc_do_select(a,b,(*p).c);
    }break;
    case 223:;
    case 224:;
    {
        mm_blockc_do_print(p,a,b);
    }break;
    case 225:;
    case 226:;
    {
        mm_blockc_do_print(p,a,b);
    }break;
    case 231:;
    {
        mm_blockc_do_read(p,a);
    }break;
    case 232:;
    {
        mm_blockc_do_readln(a);
    }break;
    case 235:;
    {
        mm_libc_ccstr((byte*)"exit(",(i64)0);
        if (!!(a)) {
            mm_blockc_evalunit(a);
        } else {
            mm_libc_dxchar((i64)48);
        };
        mm_libc_dxchar((i64)41);
    }break;
    case 9:;
    {
        mm_blockc_do_bin((i64)9766,a,b);
    }break;
    case 10:;
    {
        mm_blockc_do_bin((i64)31868,a,b);
    }break;
    case 12:;
    {
        mm_blockc_do_unary((byte*)"!",a);
    }break;
    case 13:;
    {
        mm_blockc_do_unary((byte*)"!!",a);
    }break;
    case 30:;
    {
        mm_blockc_do_bin((i64)15677,a,b);
    }break;
    case 31:;
    {
        mm_blockc_do_bin((i64)15649,a,b);
    }break;
    case 32:;
    {
        mm_blockc_do_bin((i64)60,a,b);
    }break;
    case 33:;
    {
        mm_blockc_do_bin((i64)15676,a,b);
    }break;
    case 35:;
    {
        mm_blockc_do_bin((i64)15678,a,b);
    }break;
    case 34:;
    {
        mm_blockc_do_bin((i64)62,a,b);
    }break;
    case 37:;
    {
        mm_blockc_do_bin((i64)43,a,b);
    }break;
    case 38:;
    {
        mm_blockc_do_bin((i64)45,a,b);
    }break;
    case 39:;
    {
        mm_blockc_do_bin((i64)42,a,b);
    }break;
    case 40:;
    case 41:;
    {
        mm_blockc_do_bin((i64)47,a,b);
    }break;
    case 42:;
    {
        mm_blockc_do_bin((i64)37,a,b);
    }break;
    case 44:;
    {
        mm_blockc_do_bin((i64)38,a,b);
    }break;
    case 45:;
    {
        mm_blockc_do_bin((i64)124,a,b);
    }break;
    case 46:;
    {
        mm_blockc_do_bin((i64)94,a,b);
    }break;
    case 47:;
    {
        mm_blockc_do_bin((i64)15420,a,b);
    }break;
    case 48:;
    {
        mm_blockc_do_bin((i64)15934,a,b);
    }break;
    case 54:;
    case 55:;
    {
        mm_blockc_do_max(p,a,b);
    }break;
    case 52:;
    {
        mm_blockc_do_inrange(a,b);
    }break;
    case 53:;
    {
        mm_blockc_do_inset(p,a,b);
    }break;
    case 57:;
    {
        mm_blockc_do_bin((i64)43,a,b);
    }break;
    case 157:;
    {
        mm_blockc_do_binto((i64)43,a,b);
    }break;
    case 58:;
    {
        mm_blockc_do_bin((i64)45,a,b);
    }break;
    case 158:;
    {
        mm_blockc_do_binto((i64)45,a,b);
    }break;
    case 56:;
    {
        mm_blockc_do_bin((i64)45,a,b);
    }break;
    case 82:;
    {
        mm_blockc_do_index(p,a,b);
    }break;
    case 89:;
    {
        mm_blockc_do_dot(a,b);
    }break;
    case 91:;
    {
        mm_blockc_do_maths2((byte*)"atan2",a,b);
    }break;
    case 92:;
    {
        if (!!((u64)(mm_decls_ttisreal[((i64)((*p).mode))]))) {
            mm_blockc_do_maths2((byte*)"pow",a,b);
        } else {
            mm_libc_do_syscallproc((byte*)"msysc_m_power_i64",b,a);
        };
    }break;
    case 93:;
    {
        if (((i64)((*a).tag) == (i64)94)) {
            mm_blockc_evalunit((*a).a);
        } else {
            mm_libc_dxstr((byte*)"(*");
            mm_blockc_evalunit(a);
            mm_libc_dxchar((i64)41);
        };
    }break;
    case 94:;
    {
        if (((i64)((*a).tag)==(i64)93)) {
            mm_blockc_evalunit((*a).a);
        }else if (((i64)((*a).tag)==(i64)196)) {
            mm_blockc_do_ifx((*a).a,(*a).b,(*a).c,(i64)1);
        } else {
            mm_libc_dxstr((byte*)"&");
            mm_blockc_evalunit(a);
        };
    }break;
    case 95:;
    {
        mm_blockc_evalunit(a);
    }break;
    case 96:;
    case 97:;
    {
        mm_blockc_do_convert(p,a);
    }break;
    case 99:;
    {
        if (((i64)((*a).tag) == (i64)1)) {
            if ((!!((u64)(mm_decls_ttisnumeric[((i64)((*a).mode))])) && !!((u64)(mm_decls_ttisnumeric[((i64)((*p).mode))])))) {
                (*a).mode = (i64)((*p).mode);
                mm_blockc_do_const(a);
            } else if ((!!((u64)(mm_decls_ttisnumeric[((i64)((*a).mode))])) && !!((u64)(mm_decls_ttisref[((i64)((*p).mode))])))) {
                mm_libc_dxstr((byte*)"(");
                mm_libc_dxstr(mm_libc_strmodec((i64)((*p).mode),(byte*)"",(i64)1));
                mm_libc_dxstr((byte*)")");
                mm_blockc_do_const(a);
            } else {
                mm_support_gerror((byte*)"@const",(struct mm_decls_unitrec *)(0));
            };
        } else {
            mm_libc_dxstr((byte*)"*(");
            mm_libc_dxstr(mm_libc_strmodec((i64)((*p).mode),(byte*)"",(i64)1));
            mm_libc_dxstr((byte*)"*)&");
            mm_blockc_evalunit(a);
        };
    }break;
    case 103:;
    {
        mm_blockc_do_unary((byte*)"-",a);
    }break;
    case 104:;
    {
        if (!!((u64)(mm_decls_ttisreal[((i64)((*a).mode))]))) {
            mm_blockc_do_unary((byte*)"fabs",a);
        } else if (((i64)(mm_decls_ttsize[((i64)((*a).mode))]) == (i64)4)) {
            mm_blockc_do_unary((byte*)"abs",a);
        } else {
            mm_blockc_do_unary((byte*)"labs",a);
        };
    }break;
    case 105:;
    {
        mm_blockc_do_unary((byte*)"~",a);
    }break;
    case 109:;
    {
        mm_blockc_do_bin((i64)42,a,a);
    }break;
    case 108:;
    {
        mm_blockc_do_unary((byte*)"sqrt(",a);
        mm_libc_dxchar((i64)41);
    }break;
    case 111:;
    {
        mm_libc_do_syscallproc((byte*)"msysc_m_sign",a,(struct mm_decls_unitrec *)(0));
    }break;
    case 112:;
    {
        mm_blockc_do_maths(p,a);
    }break;
    case 113:;
    {
        mm_blockc_do_maths(p,a);
    }break;
    case 114:;
    {
        mm_blockc_do_maths(p,a);
    }break;
    case 115:;
    {
        mm_blockc_do_maths(p,a);
    }break;
    case 116:;
    {
        mm_blockc_do_maths(p,a);
    }break;
    case 117:;
    {
        mm_blockc_do_maths(p,a);
    }break;
    case 118:;
    {
        mm_blockc_do_maths(p,a);
    }break;
    case 119:;
    {
        mm_blockc_do_maths(p,a);
    }break;
    case 120:;
    {
        mm_blockc_do_maths(p,a);
    }break;
    case 121:;
    {
        mm_blockc_do_maths(p,a);
    }break;
    case 122:;
    {
        mm_blockc_do_maths(p,a);
    }break;
    case 123:;
    {
        mm_blockc_do_maths(p,a);
    }break;
    case 124:;
    {
        mm_blockc_do_maths(p,a);
    }break;
    case 125:;
    {
        mm_blockc_do_maths(p,a);
    }break;
    case 126:;
    {
        mm_blockc_do_maths2((byte*)"fmod",a,b);
    }break;
    case 142:;
    {
        mm_libc_ccstr((byte*)"++",(i64)0);
        mm_blockc_evalunit(a);
    }break;
    case 143:;
    {
        mm_libc_ccstr((byte*)"--",(i64)0);
        mm_blockc_evalunit(a);
    }break;
    case 138:;
    {
        mm_libc_ccstr((byte*)"++",(i64)0);
        mm_blockc_evalunit(a);
    }break;
    case 139:;
    {
        mm_libc_ccstr((byte*)"--",(i64)0);
        mm_blockc_evalunit(a);
    }break;
    case 140:;
    {
        mm_blockc_evalunit(a);
        mm_libc_ccstr((byte*)"++",(i64)0);
    }break;
    case 141:;
    {
        mm_blockc_evalunit(a);
        mm_libc_ccstr((byte*)"--",(i64)0);
    }break;
    case 144:;
    {
        mm_blockc_do_binto((i64)43,a,b);
    }break;
    case 145:;
    {
        mm_blockc_do_binto((i64)45,a,b);
    }break;
    case 146:;
    {
        mm_blockc_do_binto((i64)42,a,b);
    }break;
    case 147:;
    {
        mm_blockc_do_binto((i64)47,a,b);
    }break;
    case 148:;
    {
        mm_blockc_do_binto((i64)47,a,b);
    }break;
    case 149:;
    {
        mm_blockc_do_binto((i64)37,a,b);
    }break;
    case 150:;
    {
        mm_blockc_do_binto((i64)38,a,b);
    }break;
    case 151:;
    {
        mm_blockc_do_binto((i64)124,a,b);
    }break;
    case 152:;
    {
        mm_blockc_do_binto((i64)94,a,b);
    }break;
    case 153:;
    {
        mm_blockc_do_binto((i64)15420,a,b);
    }break;
    case 154:;
    {
        mm_blockc_do_binto((i64)15934,a,b);
    }break;
    case 155:;
    case 156:;
    {
        mm_blockc_do_maxto(p,a,b);
    }break;
    case 173:;
    {
        mm_libc_dxint(((i64)((*p).lineno) & (i64)16777215));
    }break;
    case 242:;
    {
        mm_libc_ccstr((*p).svalue,(i64)0);
    }break;
    default: {
        mm_support_gerror_s((byte*)"UNSUPPORTED TAG: %s",mm_tables_jtagnames[((i64)((*p).tag))],(struct mm_decls_unitrec *)(0));
    }
    } //SW
;
}

void mm_blockc_do_block(struct mm_decls_unitrec * p) {
    L597 :;
    while ((!!(p) && ((i64)((*p).tag) == (i64)4))) {
        p = (*p).a;
L598 :;
    }L599 :;
    ;
    ++mm_blockc_blocklevel;
    L600 :;
    while (!!(p)) {
        mm_blockc_evalstmt(p);
        p = (*p).nextunit;
L601 :;
    }L602 :;
    ;
    --mm_blockc_blocklevel;
    mm_libc_cctab(mm_blockc_blocklevel);
    mm_libc_ccstr((byte*)"}",(i64)0);
}

void mm_blockc_do_blocklab(struct mm_decls_unitrec * p,i64 lab1,i64 lab2) {
    L603 :;
    while ((!!(p) && ((i64)((*p).tag) == (i64)4))) {
        p = (*p).a;
L604 :;
    }L605 :;
    ;
    if (!!(lab1)) {
        mm_blockc_dxlabel(lab1);
    };
    ++mm_blockc_blocklevel;
    L606 :;
    while (!!(p)) {
        mm_blockc_evalstmt(p);
        p = (*p).nextunit;
L607 :;
    }L608 :;
    ;
    --mm_blockc_blocklevel;
    if (!!(lab2)) {
        mm_blockc_dxlabel(lab2);
    };
    mm_libc_cctab(mm_blockc_blocklevel);
    mm_libc_ccstr((byte*)"}",(i64)0);
}

void mm_blockc_evalblock(struct mm_decls_unitrec * p) {
    struct mm_decls_unitrec r;
    struct mm_decls_unitrec *  pnext;
    if (((i64)((*p).tag) == (i64)4)) {
        mm_blockc_do_block(p);
    } else {
        r.tag = (i64)4;
        r.a = p;
        pnext = (*p).nextunit;
        (*p).nextunit = (struct mm_decls_unitrec *)(0);
        mm_blockc_do_block(&r);
        (*p).nextunit = pnext;
    };
}

void mm_blockc_evalblocklab(struct mm_decls_unitrec * p,i64 lab1,i64 lab2) {
    struct mm_decls_unitrec r;
    struct mm_decls_unitrec *  pnext;
    if (((i64)((*p).tag) == (i64)4)) {
        mm_blockc_do_blocklab(p,lab1,lab2);
    } else {
        r.tag = (i64)4;
        r.a = p;
        pnext = (*p).nextunit;
        (*p).nextunit = (struct mm_decls_unitrec *)(0);
        mm_blockc_do_blocklab(&r,lab1,lab2);
        (*p).nextunit = pnext;
    };
}

void mm_blockc_evalstmt(struct mm_decls_unitrec * p) {
    mm_libc_cctab(mm_blockc_blocklevel);
    mm_blockc_evalunit(p);
    mm_libc_ccstrline((byte*)";");
}

static void mm_blockc_loneexpr(struct mm_decls_unitrec * p) {
    if (!!(p)) {
        mm_blockc_evalunit(p);
        mm_libc_ccstrsemi((byte*)"");
    };
}

static void mm_blockc_do_const(struct mm_decls_unitrec * p) {
    byte str[256];
    i64 a;
    u64 u;
    i64 m;
    str[((i64)1)-1] = (u64)0u;
    m = (i64)((*p).mode);
    if (!!((u64)(mm_decls_ttisint[(m)]))) {
        a = (*p).value;
        if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)1)) {
            msysc_m_print_startstr(str);
            msysc_m_print_str((byte*)"(i8)",NULL);
            msysc_m_print_nogap();
            msysc_m_print_i64(a,NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)2)) {
            msysc_m_print_startstr(str);
            msysc_m_print_str((byte*)"(i16)",NULL);
            msysc_m_print_nogap();
            msysc_m_print_i64(a,NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)3)) {
            if ((a == (i64)-2147483648)) {
                mm_libc_dxstr((byte*)"(i32)(-2147483647-1)");
            } else {
                msysc_m_print_startstr(str);
                msysc_m_print_str((byte*)"(i32)",NULL);
                msysc_m_print_nogap();
                msysc_m_print_i64(a,NULL);
                msysc_m_print_end();
                ;
            };
        }else if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)4)) {
            if ((a == (i64)(-9223372036854775807-1))) {
                mm_libc_dxstr((byte*)"(i64)(-9223372036854775807-1)");
            } else {
                msysc_m_print_startstr(str);
                msysc_m_print_setfmt((byte*)"(i64)");
                msysc_m_print_i64(a,NULL);
                msysc_m_print_end();
                ;
            };
        }else if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)5)) {
            mm_support_gerror((byte*)"i128/const",(struct mm_decls_unitrec *)(0));
        } else {
            mm_support_gerror((byte*)"CONST/INT",(struct mm_decls_unitrec *)(0));
        };
    } else if (!!((u64)(mm_decls_ttisword[(m)]))) {
        u = (*p).uvalue;
        if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)6)) {
            msysc_m_print_startstr(str);
            msysc_m_print_setfmt((byte*)"(u8)#u");
            msysc_m_print_u64(u,NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)7)) {
            msysc_m_print_startstr(str);
            msysc_m_print_setfmt((byte*)"(u16)#u");
            msysc_m_print_u64(u,NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)8)) {
            msysc_m_print_startstr(str);
            msysc_m_print_setfmt((byte*)"(u32)#u");
            msysc_m_print_u64(u,NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)9)) {
            msysc_m_print_startstr(str);
            msysc_m_print_setfmt((byte*)"(u64)#u");
            msysc_m_print_u64(u,NULL);
            msysc_m_print_end();
            ;
        }else if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)10)) {
            mm_support_gerror((byte*)"u128/const",(struct mm_decls_unitrec *)(0));
        }else if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)13) || ((i64)(mm_decls_ttbasetype[((i64)((*p).mode))])==(i64)15)) {
            if ((((((i64)(u) < (i64)32) || ((i64)(u) > (i64)126)) || (u == (u64)92u)) || (u == (u64)39u))) {
                msysc_m_print_startstr(str);
                msysc_m_print_setfmt((byte*)"(u64)#u");
                msysc_m_print_u64(u,NULL);
                msysc_m_print_end();
                ;
            } else {
                msysc_m_print_startstr(str);
                msysc_m_print_setfmt((byte*)"'#'");
                msysc_m_print_u64(u,(byte*)"c");
                msysc_m_print_end();
                ;
            };
        } else {
            mm_support_gerror((byte*)"CONST/WORD",(struct mm_decls_unitrec *)(0));
        };
    } else if (!!((u64)(mm_decls_ttisref[(m)]))) {
        if (!!(mm_lib_isstringconst(p))) {
            if (!!((*p).svalue)) {
                mm_libc_cclongstr((*p).svalue,(i64)((*p).slength));
                return;
            } else {
                strcpy((i8 *)(str),(i8 *)((byte*)"0"));
            };
        } else {
            msysc_m_print_startstr(str);
            msysc_m_print_i64((*p).value,(byte*)"H");
            msysc_m_print_end();
            ;
        };
    } else if (!!((u64)(mm_decls_ttisreal[(m)]))) {
        if (((i64)(mm_decls_ttbasetype[((i64)((*p).mode))]) == (i64)12)) {
            msysc_m_print_startstr(str);
            msysc_m_print_setfmt((byte*)"(double)");
            msysc_m_print_r64((*p).xvalue,(byte*)".20g");
            msysc_m_print_end();
            ;
            if ((strchr((i8 *)(str),(i64)46) == 0)) {
                strcat((i8 *)(str),(i8 *)((byte*)"."));
            };
        } else {
            msysc_m_print_startstr(str);
            msysc_m_print_str((byte*)"(float)",NULL);
            msysc_m_print_nogap();
            msysc_m_print_r64((*p).xvalue,NULL);
            msysc_m_print_end();
            ;
        };
    } else {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"STRMODE(M)=",NULL);
        msysc_m_print_str(mm_lib_strmode(m,(i64)1),NULL);
        msysc_m_print_u64(mm_decls_ttisref[(m)],NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        mm_support_gerror((byte*)"doconst",(struct mm_decls_unitrec *)(0));
    };
    mm_libc_dxstr(str);
}

static void mm_blockc_do_bin(i64 opc,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    mm_libc_dxchar((i64)40);
    mm_blockc_evalunit(a);
    mm_libc_dxchar((i64)32);
    if ((opc < (i64)256)) {
        mm_libc_dxchar(opc);
    } else {
        mm_libc_dxchar((opc & (i64)255));
        mm_libc_dxchar((opc >> (i64)8));
    };
    mm_libc_dxchar((i64)32);
    mm_blockc_evalunit(b);
    mm_libc_dxchar((i64)41);
}

static void mm_blockc_do_binto(i64 opc,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    mm_blockc_evalunit(a);
    mm_libc_ccchar((i64)32);
    if ((opc < (i64)256)) {
        mm_libc_ccchar(opc);
    } else {
        mm_libc_ccchar((opc & (i64)255));
        mm_libc_ccchar((opc >> (i64)8));
    };
    mm_libc_ccstr((byte*)"= ",(i64)0);
    mm_blockc_evalunit(b);
}

static void mm_blockc_do_unary(byte * opc,struct mm_decls_unitrec * a) {
    mm_libc_dxstr(opc);
    mm_libc_dxchar((i64)40);
    mm_blockc_evalunit(a);
    mm_libc_dxchar((i64)41);
}

static void mm_blockc_do_if(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,struct mm_decls_unitrec * c) {
    mm_libc_ccstr((byte*)"if (",(i64)0);
    mm_blockc_evalunit(a);
    mm_libc_ccstrline((byte*)") {");
    mm_blockc_evalblock(b);
    if (!!(c)) {
        mm_libc_ccstrline((byte*)" else {");
        mm_blockc_evalblock(c);
    };
}

static void mm_blockc_do_longif(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    struct mm_decls_unitrec *  q;
    q = a;
    L609 :;
    while (!!(q)) {
        if ((q == a)) {
            mm_libc_ccstr((byte*)"if (",(i64)0);
            mm_blockc_evalunit((*q).a);
            mm_libc_ccstrline((byte*)") {");
        } else {
            mm_libc_ccstr((byte*)" else if (",(i64)0);
            mm_blockc_evalunit((*q).a);
            mm_libc_ccstrline((byte*)") {");
        };
        mm_blockc_evalblock((*q).b);
        q = (*q).nextunit;
L610 :;
    }L611 :;
    ;
    if (!!(b)) {
        mm_libc_ccstrline((byte*)" else {");
        mm_blockc_evalblock(b);
    };
}

static void mm_blockc_do_call(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    struct mm_decls_unitrec *  q;
    if (((i64)((*a).tag) == (i64)3)) {
        mm_libc_dxstr(mm_libc_getprocname((*a).def));
    } else {
        mm_libc_dxchar((i64)40);
        mm_blockc_evalunit(a);
        mm_libc_dxchar((i64)41);
    };
    mm_libc_dxchar((i64)40);
    q = b;
    L612 :;
    while (!!(q)) {
        mm_blockc_evalunit(q);
        q = (*q).nextunit;
        if (!!(q)) {
            mm_libc_dxchar((i64)44);
        };
L613 :;
    }L614 :;
    ;
    mm_libc_dxchar((i64)41);
}

static void mm_blockc_do_goto(struct mm_decls_unitrec * a) {
    struct mm_decls_strec *  d;
    if (((i64)((*a).tag)==(i64)3)) {
        d = (*a).def;
        if (((i64)((*d).index) == (i64)0)) {
            (*d).index = ++mm_decls_labelno;
        };
        if (((i64)((*d).nameid)==(i64)15)) {
            mm_libc_ccstr((byte*)"goto ",(i64)0);
            mm_libc_ccstrsemi(mm_libc_genclabel((i64)((*d).index),(i64)0));
        } else {
            mm_support_gerror_s((byte*)"Not label/block name: %s",(*d).name,(struct mm_decls_unitrec *)(0));
        };
    } else {
        mm_support_gerror((byte*)"Can't do goto with label pointer",(struct mm_decls_unitrec *)(0));
    };
}

static void mm_blockc_do_do(struct mm_decls_unitrec * a) {
    i64 lab_abc;
    i64 lab_d;
    lab_abc = mm_blockc_definelabel();
    lab_d = mm_blockc_createfwdlabel();
    mm_blockc_stacklooplabels(lab_abc,lab_abc,lab_abc,lab_d);
    mm_libc_ccstrline((byte*)"while (1) {");
    mm_blockc_evalblock(a);
    mm_blockc_definefwdlabel(lab_d);
    mm_blockc_unstacklooplabels();
}

static i64 mm_blockc_definelabel(void) {
    ++mm_decls_labelno;
    mm_libc_ccstr(mm_libc_genclabel(mm_decls_labelno,(i64)1),(i64)0);
    mm_libc_ccsendline();
    mm_libc_cctab(mm_blockc_blocklevel);
    return mm_decls_labelno;
}

static i64 mm_blockc_createfwdlabel(void) {
    return ++mm_decls_labelno;
}

static void mm_blockc_definefwdlabel(i64 lab) {
    mm_libc_ccstr(mm_libc_genclabel(lab,(i64)1),(i64)0);
    mm_libc_ccsendline();
    mm_libc_cctab(mm_blockc_blocklevel);
}

static void mm_blockc_stacklooplabels(i64 a,i64 b,i64 c,i64 d) {
    ++mm_blockc_loopindex;
    if ((mm_blockc_loopindex > (i64)50)) {
        mm_support_gerror((byte*)"Too many nested loops",(struct mm_decls_unitrec *)(0));
    };
    mm_blockc_loopstack[(mm_blockc_loopindex)-1][((i64)1)-1] = a;
    mm_blockc_loopstack[(mm_blockc_loopindex)-1][((i64)2)-1] = b;
    mm_blockc_loopstack[(mm_blockc_loopindex)-1][((i64)3)-1] = c;
    mm_blockc_loopstack[(mm_blockc_loopindex)-1][((i64)4)-1] = d;
}

static void mm_blockc_unstacklooplabels(void) {
    --mm_blockc_loopindex;
}

static i64 mm_blockc_findlooplabel(i64 k,i64 n) {
    i64 i;
    i = (mm_blockc_loopindex - (n - (i64)1));
    if (((i < (i64)1) || (i > mm_blockc_loopindex))) {
        mm_support_gerror((byte*)"Bad loop index",(struct mm_decls_unitrec *)(0));
    };
    return mm_blockc_loopstack[(i)-1][(k)-1];
}

static void mm_blockc_do_exit(struct mm_decls_unitrec * p) {
    i64 k;
    i64 n;
    i64 index;
    switch ((i64)((*p).tag)) {
    case 211:;
    {
        k = (i64)1;
    }break;
    case 212:;
    {
        k = (i64)2;
    }break;
    case 213:;
    {
        k = (i64)3;
    }break;
    case 214:;
    {
        k = (i64)4;
    }break;
    default: {
    }
    } //SW
;
    index = (i64)((*p).index);
    if ((index == (i64)0)) {
        index = mm_blockc_loopindex;
    };
    n = mm_blockc_findlooplabel(k,index);
    if ((n == (i64)0)) {
        mm_support_gerror((byte*)"Bad exit/loop index",p);
    } else {
        mm_libc_ccstr((byte*)"goto ",(i64)0);
        mm_libc_ccstr(mm_libc_genclabel(n,(i64)0),(i64)0);
    };
}

static void mm_blockc_do_to(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,struct mm_decls_unitrec * c) {
    i64 lab_ab;
    i64 lab_c;
    i64 lab_d;
    mm_libc_ccstr(mm_libc_getfullnamec((*c).def),(i64)0);
    mm_libc_ccstr((byte*)" = ",(i64)0);
    mm_libc_ccstrsemiu(a);
    lab_ab = mm_blockc_createfwdlabel();
    lab_c = mm_blockc_createfwdlabel();
    lab_d = mm_blockc_createfwdlabel();
    mm_blockc_stacklooplabels(lab_ab,lab_ab,lab_c,lab_d);
    mm_libc_cctab(mm_blockc_blocklevel);
    mm_libc_ccstr((byte*)"while (",(i64)0);
    mm_libc_ccstr(mm_libc_getfullnamec((*c).def),(i64)0);
    mm_libc_ccstrline((byte*)"-- > 0) {");
    mm_blockc_evalblocklab(b,lab_ab,lab_c);
    mm_blockc_definefwdlabel(lab_d);
    mm_blockc_unstacklooplabels();
}

static void mm_blockc_do_while(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    i64 lab_ab;
    i64 lab_c;
    i64 lab_d;
    lab_ab = mm_blockc_definelabel();
    lab_c = mm_blockc_createfwdlabel();
    lab_d = mm_blockc_createfwdlabel();
    mm_blockc_stacklooplabels(lab_ab,lab_ab,lab_c,lab_d);
    mm_libc_ccstr((byte*)"while (",(i64)0);
    mm_blockc_evalunit(a);
    mm_libc_ccstrline((byte*)") {");
    mm_blockc_evalblocklab(b,(i64)0,lab_c);
    mm_blockc_definefwdlabel(lab_d);
    mm_blockc_unstacklooplabels();
}

static void mm_blockc_do_repeat(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    i64 lab_ab;
    i64 lab_c;
    i64 lab_d;
    lab_ab = mm_blockc_definelabel();
    lab_c = mm_blockc_createfwdlabel();
    lab_d = mm_blockc_createfwdlabel();
    mm_blockc_stacklooplabels(lab_ab,lab_ab,lab_c,lab_d);
    mm_libc_ccstrline((byte*)"do {");
    mm_blockc_evalblocklab(a,(i64)0,lab_c);
    mm_libc_ccstr((byte*)" while (!",(i64)0);
    mm_blockc_evalunit(b);
    mm_libc_ccstr((byte*)");",(i64)0);
    mm_blockc_definefwdlabel(lab_d);
    mm_blockc_unstacklooplabels();
}

static void mm_blockc_do_forup(struct mm_decls_unitrec * p,struct mm_decls_unitrec * ivar,struct mm_decls_unitrec * pbody,struct mm_decls_unitrec * pautovar) {
    i64 lab_a;
    i64 lab_b;
    i64 lab_c;
    i64 lab_d;
    i64 down;
    struct mm_decls_unitrec *  pfrom;
    struct mm_decls_unitrec *  pto;
    struct mm_decls_unitrec *  pstep;
    struct mm_decls_unitrec *  pelse;
    struct mm_decls_unitrec *  px;
    byte varstr[512];
    struct mm_decls_strec *  d;
    switch ((i64)((*p).tag)) {
    case 198:;
    {
        down = (i64)0;
    }break;
    case 199:;
    {
        down = (i64)1;
    }break;
    default: {
        down = (i64)0;
    }
    } //SW
;
    pfrom = (*ivar).nextunit;
    pto = (*pfrom).nextunit;
    pstep = (*pto).nextunit;
    pelse = (*pbody).nextunit;
    if (((i64)((*pto).tag) == (i64)93)) {
        px = (*pto).a;
        if (((((i64)((*px).tag) == (i64)3) && ((i64)((u64)((*(d = (*px).def)).nameid)) == (i64)11)) && ((i64)((u64)((*d).parammode)) == (i64)2))) {
            mm_support_gerror((byte*)"Possibly using &param as for-loop limit",(struct mm_decls_unitrec *)(0));
        };
    };
    lab_a = mm_blockc_definelabel();
    lab_b = mm_blockc_createfwdlabel();
    lab_c = mm_blockc_createfwdlabel();
    lab_d = mm_blockc_createfwdlabel();
    mm_blockc_stacklooplabels(lab_a,lab_b,lab_c,lab_d);
    if (((i64)((*ivar).tag) != (i64)3)) {
        mm_support_gerror((byte*)"for/name",(struct mm_decls_unitrec *)(0));
    };
    strcpy((i8 *)(varstr),(i8 *)(mm_libc_getfullnamec((*ivar).def)));
    mm_libc_ccstr((byte*)"for (",(i64)0);
    mm_libc_ccstr(varstr,(i64)0);
    mm_libc_ccchar((i64)61);
    mm_blockc_evalunit(pfrom);
    mm_libc_ccchar((i64)59);
    mm_libc_ccstr(varstr,(i64)0);
    mm_libc_ccstr((!!(down)?(byte*)">=":(byte*)"<="),(i64)0);
    mm_blockc_evalunit(pto);
    mm_libc_ccchar((i64)59);
    if (!!(pstep)) {
        mm_libc_ccstr(varstr,(i64)0);
        mm_libc_ccstr((!!(down)?(byte*)"-=":(byte*)"+="),(i64)0);
        mm_blockc_evalunit(pstep);
    } else {
        mm_libc_ccstr((!!(down)?(byte*)"--":(byte*)"++"),(i64)0);
        mm_libc_ccstr(varstr,(i64)0);
    };
    mm_libc_ccstrline((byte*)") {");
    mm_blockc_evalblocklab(pbody,lab_b,lab_c);
    if (!!(pelse)) {
        mm_libc_ccsendline();
        mm_libc_cctab(mm_blockc_blocklevel);
        mm_libc_ccstrline((byte*)"{");
        mm_blockc_evalblock(pelse);
    };
    mm_blockc_definefwdlabel(lab_d);
    mm_blockc_unstacklooplabels();
}

static void mm_blockc_do_return(struct mm_decls_unitrec * a) {
    if (!(!!(a))) {
        mm_libc_ccstr((byte*)"return",(i64)0);
    } else {
        mm_libc_ccstr((byte*)"return ",(i64)0);
        mm_blockc_evalunit(a);
    };
}

static void mm_blockc_do_print(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    struct mm_decls_unitrec *  q;
    struct mm_decls_unitrec *  r;
    struct mm_decls_unitrec *  s;
    i64 m;
    i64 widenop;
    byte *  fn;
    if (!!(a)) {
        if (((i64)(mm_decls_ttbasetype[((i64)((*a).mode))]) != (i64)20)) {
            mm_support_gerror((byte*)"@dev no ref",(struct mm_decls_unitrec *)(0));
        };
        if (((i64)(mm_decls_ttbasetype[((i64)(mm_decls_tttarget[((i64)((*a).mode))]))])==(i64)0)) {
            mm_libc_do_syscallproc((byte*)"msysc_m_print_startfile",a,(struct mm_decls_unitrec *)(0));
        }else if (((i64)(mm_decls_ttbasetype[((i64)(mm_decls_tttarget[((i64)((*a).mode))]))])==(i64)13)) {
            mm_libc_do_syscallproc((byte*)"msysc_m_print_startstr",a,(struct mm_decls_unitrec *)(0));
        }else if (((i64)(mm_decls_ttbasetype[((i64)(mm_decls_tttarget[((i64)((*a).mode))]))])==(i64)20)) {
            mm_libc_do_syscallproc((byte*)"msysc_m_print_startptr",a,(struct mm_decls_unitrec *)(0));
        } else {
            mm_support_gerror((byte*)"@dev?",(struct mm_decls_unitrec *)(0));
        };
    } else {
        mm_libc_do_syscallproc((byte*)"msysc_m_print_startcon",(struct mm_decls_unitrec *)(0),(struct mm_decls_unitrec *)(0));
    };
    q = b;
    if (((i64)((*p).tag)==(i64)225) || ((i64)((*p).tag)==(i64)226)) {
        if ((((i64)(mm_decls_ttbasetype[((i64)((*q).mode))]) != (i64)20) || ((i64)(mm_decls_ttbasetype[((i64)(mm_decls_tttarget[((i64)((*q).mode))]))]) != (i64)13))) {
            mm_support_gerror((byte*)"string expected",(struct mm_decls_unitrec *)(0));
        };
        mm_libc_do_syscallproc((byte*)"msysc_m_print_setfmt",q,(struct mm_decls_unitrec *)(0));
        q = (*p).c;
    };
    L615 :;
    while (!!(q)) {
        s = (struct mm_decls_unitrec *)(0);
        if (((i64)((*q).tag)==(i64)187)) {
            s = (*q).b;
            r = (*q).a;
            m = (i64)((*r).mode);
        }else if (((i64)((*q).tag)==(i64)188)) {
            mm_libc_do_syscallproc((byte*)"msysc_m_print_nogap",(struct mm_decls_unitrec *)(0),(struct mm_decls_unitrec *)(0));
            q = (*q).nextunit;
            goto L616 ;
        } else {
            s = mm_libc_nilunit;
            r = q;
            m = (i64)((*q).mode);
        };
        widenop = (i64)0;
        if (((i64)(mm_decls_ttbasetype[(m)])==(i64)4)) {
            fn = (byte*)"msysc_m_print_i64";
        }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)1) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)2) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)3)) {
            fn = (byte*)"msysc_m_print_i64";
        }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)9)) {
            fn = (byte*)"msysc_m_print_u64";
        }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)6) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)7) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)8)) {
            fn = (byte*)"msysc_m_print_u64";
        }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)11)) {
            fn = (byte*)"msysc_m_print_r64";
        }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)12)) {
            fn = (byte*)"msysc_m_print_r64";
        }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)5)) {
            fn = (byte*)"msysc_m_print_i128";
        }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)10)) {
            fn = (byte*)"msysc_m_print_u128";
        }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)34)) {
            fn = (byte*)"msysc_m_print_flexstr";
        }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)20)) {
            if ((((i64)(mm_decls_tttarget[(m)]) == (i64)13) || (((i64)(mm_decls_tttarget[(m)]) == (i64)29) && ((i64)(mm_decls_tttarget[((i64)(mm_decls_tttarget[(m)]))]) == (i64)13)))) {
                fn = (byte*)"msysc_m_print_str";
            } else {
                fn = (byte*)"msysc_m_print_ptr";
            };
        }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)29)) {
            mm_support_gerror((byte*)"PRINTARRAY",(struct mm_decls_unitrec *)(0));
            q = (*q).nextunit;
        }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)32)) {
            mm_support_gerror((byte*)"PRINTRECORD",(struct mm_decls_unitrec *)(0));
        }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)13) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)14) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)15)) {
            fn = (byte*)"msysc_m_print_c8";
        } else {
            mm_support_gerror_s((byte*)"PRINT/T=%s",mm_lib_strmode(m,(i64)1),(struct mm_decls_unitrec *)(0));
        };
        if (!!(widenop)) {
            mm_support_gerror((byte*)"WIDEN/PRINT",(struct mm_decls_unitrec *)(0));
        };
        mm_libc_do_syscallproc(fn,r,s);
        q = (*q).nextunit;
L616 :;
    }L617 :;
    ;
    if (((i64)((*p).tag)==(i64)224) || ((i64)((*p).tag)==(i64)226)) {
        mm_libc_do_syscallproc((byte*)"msysc_m_print_newline",(struct mm_decls_unitrec *)(0),(struct mm_decls_unitrec *)(0));
    };
    mm_libc_do_syscallproc((byte*)"msysc_m_print_end",(struct mm_decls_unitrec *)(0),(struct mm_decls_unitrec *)(0));
}

static void mm_blockc_do_read(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    i64 m;
    if (!(!!(a))) {
        a = mm_libc_zerounit;
    };
    m = (i64)((*p).mode);
    if (!!((u64)(mm_decls_ttisinteger[(m)]))) {
        mm_libc_do_syscallproc((byte*)"msysc_m_read_i64",a,(struct mm_decls_unitrec *)(0));
    } else if (!!((u64)(mm_decls_ttisreal[(m)]))) {
    } else {
        mm_support_gerror((byte*)"CAN'T READ THIS ITEM",(struct mm_decls_unitrec *)(0));
    };
}

static void mm_blockc_do_readln(struct mm_decls_unitrec * a) {
    if (!!(a)) {
        if (((i64)(mm_decls_ttbasetype[((i64)((*a).mode))]) != (i64)20)) {
            mm_support_gerror((byte*)"@dev no ref",(struct mm_decls_unitrec *)(0));
        };
        if (((i64)(mm_decls_ttbasetype[((i64)(mm_decls_tttarget[((i64)((*a).mode))]))])==(i64)0)) {
            mm_libc_do_syscallproc((byte*)"msysc_m_read_fileline",a,(struct mm_decls_unitrec *)(0));
        }else if (((i64)(mm_decls_ttbasetype[((i64)(mm_decls_tttarget[((i64)((*a).mode))]))])==(i64)6)) {
            mm_libc_do_syscallproc((byte*)"msysc_m_read_strline",a,(struct mm_decls_unitrec *)(0));
        } else {
            mm_support_gerror((byte*)"rd@dev?",(struct mm_decls_unitrec *)(0));
        };
    } else {
        mm_libc_do_syscallproc((byte*)"msysc_m_read_conline",(struct mm_decls_unitrec *)(0),(struct mm_decls_unitrec *)(0));
    };
}

static void mm_blockc_do_convert(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    i64 ssize;
    i64 tsize;
    mm_libc_dxchar((i64)40);
    mm_libc_dxstr(mm_libc_strmodec((i64)((*p).mode),(byte*)"",(i64)1));
    mm_libc_dxchar((i64)41);
    ssize = (i64)(mm_decls_ttsize[((i64)((*a).mode))]);
    tsize = (i64)(mm_decls_ttsize[((i64)((*p).mode))]);
    if (((i64)((*p).opcode)==(i64)23)) {
        if ((ssize != tsize)) {
            mm_libc_dxstr(((tsize == (i64)8)?(byte*)"(u64)":(byte*)"(u32)"));
        };
    }else if (((i64)((*p).opcode)==(i64)24)) {
        if ((ssize != tsize)) {
            mm_libc_dxstr(((ssize == (i64)8)?(byte*)"(u64)":(byte*)"(u32)"));
        };
    };
    mm_libc_dxchar((i64)40);
    mm_blockc_evalunit(a);
    mm_libc_dxchar((i64)41);
}

static void mm_blockc_do_index(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * pindex) {
    i64 lower;
    mm_blockc_evalunit(a);
    mm_libc_dxstr((byte*)"[(");
    mm_blockc_evalunit(pindex);
    mm_libc_dxchar((i64)41);
    lower = (i64)(mm_decls_ttlower[((i64)((*a).mode))]);
    if ((lower > (i64)0)) {
        mm_libc_dxchar((i64)45);
        mm_libc_dxint(lower);
    } else if ((lower < (i64)0)) {
        mm_libc_dxchar((i64)43);
        mm_libc_dxint(labs(lower));
    };
    mm_libc_dxchar((i64)93);
}

static void mm_blockc_do_dot(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    mm_blockc_evalunit(a);
    mm_libc_dxchar((i64)46);
    mm_blockc_evalunit(b);
}

static void mm_blockc_do_swap(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    mm_libc_ccstr((byte*)"{",(i64)0);
    mm_libc_ccstr(mm_libc_strmodec((i64)((*a).mode),(byte*)"temp",(i64)1),(i64)0);
    mm_libc_ccstr((byte*)" = ",(i64)0);
    mm_blockc_evalunit(a);
    mm_libc_ccstr((byte*)"; ",(i64)0);
    mm_blockc_evalunit(a);
    mm_libc_ccstr((byte*)" = ",(i64)0);
    mm_blockc_evalunit(b);
    mm_libc_ccstr((byte*)"; ",(i64)0);
    mm_blockc_evalunit(b);
    mm_libc_ccstr((byte*)" = temp; }",(i64)0);
}

static void mm_blockc_do_case(struct mm_decls_unitrec * p,struct mm_decls_unitrec * pindex,struct mm_decls_unitrec * pwhenthen,struct mm_decls_unitrec * pelse) {
    i64 loopsw;
    i64 lab_abc;
    i64 lab_d;
    struct mm_decls_unitrec *  w;
    struct mm_decls_unitrec *  wt;
    loopsw = ((i64)((*p).tag) == (i64)217);
    if (!!(loopsw)) {
        lab_abc = mm_blockc_definelabel();
        lab_d = mm_blockc_createfwdlabel();
        mm_blockc_stacklooplabels(lab_abc,lab_abc,lab_abc,lab_d);
    };
    wt = pwhenthen;
    L618 :;
    while (!!(wt)) {
        if ((wt == pwhenthen)) {
            mm_libc_ccstr((byte*)"if (",(i64)0);
        } else {
            mm_libc_ccstr((byte*)"else if (",(i64)0);
        };
        w = (*wt).a;
        L621 :;
        while (!!(w)) {
            if ((w != (*wt).a)) {
                mm_libc_ccstr((byte*)" || ",(i64)0);
            };
            if (!!(pindex)) {
                mm_libc_ccchar((i64)40);
                mm_blockc_evalunit(pindex);
                mm_libc_ccstr((byte*)"==",(i64)0);
                mm_blockc_evalunit(w);
                mm_libc_ccchar((i64)41);
            } else {
                mm_blockc_evalunit(w);
            };
            w = (*w).nextunit;
L622 :;
        }L623 :;
        ;
        mm_libc_ccstrline((byte*)") {");
        mm_blockc_evalblock((*wt).b);
        wt = (*wt).nextunit;
L619 :;
    }L620 :;
    ;
    if (!!(pelse)) {
        mm_libc_ccstrline((byte*)" else {");
        mm_blockc_evalblock(pelse);
    };
    if (!!(loopsw)) {
        mm_libc_ccstr((byte*)"goto ",(i64)0);
        mm_libc_ccstrsemi(mm_libc_genclabel(lab_abc,(i64)0));
        mm_blockc_definefwdlabel(lab_d);
        mm_blockc_unstacklooplabels();
    };
}

static void mm_blockc_do_switch(struct mm_decls_unitrec * p,struct mm_decls_unitrec * pindex,struct mm_decls_unitrec * pwhenthen,struct mm_decls_unitrec * pelse) {
    i64 loopsw;
    i64 lab_a;
    i64 lab_d;
    i64 x;
    i64 y;
    i64 i;
    struct mm_decls_unitrec *  w;
    struct mm_decls_unitrec *  wt;
    loopsw = ((i64)((*p).tag) == (i64)219);
    if (!!(loopsw)) {
        lab_a = mm_blockc_definelabel();
        lab_d = mm_blockc_createfwdlabel();
        mm_blockc_stacklooplabels(lab_a,lab_a,lab_a,lab_d);
    };
    mm_libc_ccstr((byte*)"switch (",(i64)0);
    mm_blockc_evalunit(pindex);
    mm_libc_ccstrline((byte*)") {");
    wt = pwhenthen;
    L624 :;
    while (!!(wt)) {
        w = (*wt).a;
        L627 :;
        while (!!(w)) {
            if (((i64)((*w).tag)==(i64)15)) {
                x = (*(*w).a).value;
                y = (*(*w).b).value;
            }else if (((i64)((*w).tag)==(i64)1)) {
                x = (y = (*w).value);
            };
            L630 :;
            for (i=x;i<=y;++i) {
L631 :;
                mm_libc_cctab(mm_blockc_blocklevel);
                mm_libc_ccstr((byte*)"case ",(i64)0);
                mm_libc_ccint(i);
                if ((((*wt).b == 0) && (i == y))) {
                    mm_libc_ccstrline((byte*)":;// ");
                } else {
                    mm_libc_ccstrline((byte*)":;");
                };
L632 :;
            }L633 :;
            ;
            w = (*w).nextunit;
L628 :;
        }L629 :;
        ;
        mm_libc_cctab(mm_blockc_blocklevel);
        mm_libc_ccstrline((byte*)"{");
        mm_blockc_evalblock((*wt).b);
        mm_libc_ccstrsemi((byte*)"break");
        wt = (*wt).nextunit;
L625 :;
    }L626 :;
    ;
    mm_libc_cctab(mm_blockc_blocklevel);
    mm_libc_ccstrline((byte*)"default: {");
    if (!!(pelse)) {
        mm_blockc_evalblock(pelse);
        mm_libc_ccsendline();
    } else {
        mm_libc_cctab(mm_blockc_blocklevel);
        mm_libc_ccstrline((byte*)"}");
    };
    mm_libc_cctab(mm_blockc_blocklevel);
    mm_libc_ccstrline((byte*)"} //SW");
    if (!!(loopsw)) {
        mm_libc_ccstr((byte*)"goto ",(i64)0);
        mm_libc_ccstrsemi(mm_libc_genclabel(lab_a,(i64)0));
        mm_blockc_definefwdlabel(lab_d);
        mm_blockc_unstacklooplabels();
    };
}

static void mm_blockc_do_max(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    if ((!!(mm_libc_issimplec(a)) && !!(mm_libc_issimplec(b)))) {
        mm_libc_dxchar((i64)40);
        mm_blockc_evalunit(a);
        mm_libc_dxchar((((i64)((*p).tag) == (i64)54)?(i64)60:(i64)62));
        mm_blockc_evalunit(b);
        mm_libc_dxchar((i64)63);
        mm_blockc_evalunit(a);
        mm_libc_dxchar((i64)58);
        mm_blockc_evalunit(b);
        mm_libc_dxchar((i64)41);
    } else {
        mm_libc_dxstr((byte*)"msysc_m_");
        if (!!((u64)(mm_decls_ttisreal[((i64)((*a).mode))]))) {
            mm_libc_dxchar((i64)102);
        } else {
            mm_libc_dxchar((i64)105);
        };
        mm_libc_dxstr((((i64)((*p).tag) == (i64)54)?(byte*)"min(":(byte*)"max("));
        mm_blockc_evalunit(a);
        mm_libc_dxchar((i64)44);
        mm_blockc_evalunit(b);
        mm_libc_dxchar((i64)41);
    };
}

static void mm_blockc_do_maxto(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    mm_blockc_evalunit(a);
    mm_libc_ccstr((byte*)"=(",(i64)0);
    mm_blockc_evalunit(a);
    mm_libc_ccchar((((i64)((*p).tag) == (i64)155)?(i64)60:(i64)62));
    mm_blockc_evalunit(b);
    mm_libc_ccchar((i64)63);
    mm_blockc_evalunit(a);
    mm_libc_ccchar((i64)58);
    mm_blockc_evalunit(b);
    mm_libc_ccstrsemi((byte*)")");
}

static void mm_blockc_do_blockcopy(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    mm_libc_dxstr((byte*)"memcpy(&");
    mm_blockc_evalunit(a);
    if (((i64)((*b).tag) == (i64)93)) {
        mm_libc_dxstr((byte*)",");
        mm_blockc_evalunit((*b).a);
    } else {
        mm_libc_dxstr((byte*)",&");
        mm_blockc_evalunit(b);
    };
    mm_libc_dxchar((i64)44);
    mm_libc_dxint((i64)(mm_decls_ttsize[((i64)((*a).mode))]));
    mm_libc_dxchar((i64)41);
}

static void mm_blockc_do_select(struct mm_decls_unitrec * pindex,struct mm_decls_unitrec * plist,struct mm_decls_unitrec * pdefault) {
    i64 i;
    struct mm_decls_unitrec *  q;
    i64 av_1;
    q = plist;
    i = (i64)1;
    L634 :;
    while (!!(q)) {
        mm_libc_dxchar((i64)40);
        mm_blockc_evalunit(pindex);
        mm_libc_dxstr((byte*)"==");
        mm_libc_dxint(i);
        ++i;
        mm_libc_dxchar((i64)63);
        mm_blockc_evalunit(q);
        mm_libc_dxchar((i64)58);
        q = (*q).nextunit;
L635 :;
    }L636 :;
    ;
    mm_blockc_evalunit(pdefault);
    av_1 = (i - (i64)1);
    while (av_1-- > 0) {
L637 :;
        mm_libc_dxchar((i64)41);
L638 :;
    }L639 :;
    ;
}

static void mm_blockc_do_maths(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    if (((i64)((*p).tag)==(i64)112) || ((i64)((*p).tag)==(i64)113) || ((i64)((*p).tag)==(i64)114) || ((i64)((*p).tag)==(i64)115) || ((i64)((*p).tag)==(i64)116) || ((i64)((*p).tag)==(i64)117) || ((i64)((*p).tag)==(i64)121) || ((i64)((*p).tag)==(i64)123) || ((i64)((*p).tag)==(i64)124) || ((i64)((*p).tag)==(i64)120)) {
        mm_libc_dxstr((mm_tables_jtagnames[((i64)((*p).tag))] + (i64)2));
        mm_libc_dxchar((i64)40);
        mm_blockc_evalunit(a);
        mm_libc_dxchar((i64)41);
    } else {
        mm_support_gerror_s((byte*)"MATHS:",mm_tables_jtagnames[((i64)((*p).tag))],(struct mm_decls_unitrec *)(0));
    };
}

static void mm_blockc_do_maths2(byte * name,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    mm_libc_dxstr(name);
    mm_libc_dxchar((i64)40);
    mm_blockc_evalunit(a);
    mm_libc_dxchar((i64)44);
    mm_blockc_evalunit(b);
    mm_libc_dxchar((i64)41);
}

static void mm_blockc_do_ifx(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,struct mm_decls_unitrec * c,i64 addrof) {
    mm_libc_dxchar((i64)40);
    mm_blockc_evalunit(a);
    mm_libc_dxchar((i64)63);
    if (!!(addrof)) {
        mm_libc_dxchar((i64)38);
    };
    mm_blockc_evalunit(b);
    mm_libc_dxchar((i64)58);
    if (!!(addrof)) {
        mm_libc_dxchar((i64)38);
    };
    mm_blockc_evalunit(c);
    mm_libc_dxchar((i64)41);
}

static void mm_blockc_do_exprlist(struct mm_decls_unitrec * a) {
    mm_libc_dxchar((i64)40);
    L640 :;
    while (!!(a)) {
        mm_blockc_evalunit(a);
        a = (*a).nextunit;
        if (!!(a)) {
            mm_libc_dxchar((i64)44);
        };
L641 :;
    }L642 :;
    ;
    mm_libc_dxchar((i64)41);
}

static void mm_blockc_do_inrange(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    struct mm_decls_unitrec *  l;
    struct mm_decls_unitrec *  r;
    l = (*b).a;
    r = (*b).b;
    mm_libc_dxstr((byte*)"(");
    mm_blockc_evalunit(a);
    mm_libc_dxstr((byte*)">=");
    mm_blockc_evalunit(l);
    mm_libc_dxstr((byte*)" && ");
    mm_blockc_evalunit(a);
    mm_libc_dxstr((byte*)"<=");
    mm_blockc_evalunit(r);
    mm_libc_dxstr((byte*)")");
}

static void mm_blockc_do_inset(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    struct mm_decls_unitrec *  s;
    mm_libc_dxchar((i64)40);
    s = (*b).a;
    L643 :;
    while (!!(s)) {
        mm_blockc_do_bin((i64)15677,a,s);
        s = (*s).nextunit;
        if (!!(s)) {
            mm_libc_dxstr((byte*)" || ");
        };
L644 :;
    }L645 :;
    ;
    mm_libc_dxchar((i64)41);
}

static i64 mm_blockc_isexpr(struct mm_decls_unitrec * p) {
    i64 expr;
    expr = (i64)0;
    if (!!((u64)(mm_tables_jisexpr[((i64)((*p).tag))]))) {
        expr = (i64)1;
    };
    if (((i64)((*p).tag)==(i64)196) || ((i64)((*p).tag)==(i64)218) || ((i64)((*p).tag)==(i64)216) || ((i64)((*p).tag)==(i64)221)) {
        if (((i64)((*p).mode) == (i64)0)) {
            expr = (i64)0;
        };
    }else if (((i64)((*p).tag)==(i64)25)) {
        if (!!((i64)((*p).popflag))) {
            expr = (i64)0;
        };
    };
    return expr;
}

static void mm_blockc_dxlabel(i64 lab) {
    mm_libc_ccstr(mm_libc_genclabel(lab,(i64)1),(i64)0);
    mm_libc_ccsendline();
}

i64 mm_parse_parsemodule(i64 n) {
    struct mm_decls_modulerec m;
    struct mm_decls_strec *  owner;
    i64 status;
    mm_parse_initparser();
    memcpy(&m,&mm_decls_moduletable[(n)],98);
    mm_decls_currmoduleno = n;
    mm_decls_stmodule = mm_decls_moduletable[(n)].stmodule;
    mm_decls_currproc = mm_decls_stmodule;
    mm_lex_startlex((byte*)"PARSEMODULE",m.fileno);
    owner = mm_decls_stmodule;
    mm_lex_lex();
    status = mm_parse_readmoduledefs(owner);
    if (!(!!(status))) {
        return (i64)0;
    };
    return status;
}

i64 mm_parse_readmoduledefs(struct mm_decls_strec * owner) {
    struct mm_decls_strec *  dimport;
    struct mm_decls_strec *  stimport;
    i64 globalflag;
    i64 i;
    i64 callbackflag;
    byte *  name;
    globalflag = (i64)0;
    callbackflag = (i64)0;
    L646 :;
    while (1) {
        switch ((i64)(mm_decls_lx.symbol)) {
        case 122:;
        {
            if (!!(globalflag)) {
                mm_support_serror((byte*)"global global?");
            };
            globalflag = (i64)(mm_decls_lx.subcode);
            mm_lex_lex();
        }break;
        case 91:;
        case 92:;
        {
            mm_parse_readprocdef(owner,globalflag,callbackflag);
            callbackflag = (i64)0;
            globalflag = (i64)0;
        }break;
        case 53:;
        case 50:;
        case 14:;
        case 104:;
        case 58:;
        case 55:;
        {
            mm_support_serror((byte*)"Need Var or Let");
        }break;
        case 105:;
        case 107:;
        {
            mm_parse_readvardef(owner,globalflag,(i64)0,(i64)9);
            globalflag = (i64)0;
        }break;
        case 106:;
        {
            mm_parse_readvardef(owner,globalflag,(i64)0,(i64)9);
            globalflag = (i64)0;
        }break;
        case 98:;
        {
            mm_parse_readimportmodule(owner);
        }break;
        case 99:;
        {
            mm_lex_lex();
            mm_parse_checksymbol((i64)45);
            mm_lex_lex();
        }break;
        case 100:;
        {
            L648 :;
            do {
                mm_lex_lex();
L649 :;
            } while (!((mm_decls_lx.symbol == (i64)6) || (mm_decls_lx.symbol == (i64)36)));L650 :;
            ;
        }break;
        case 102:;
        {
            mm_parse_readtypedef(owner,globalflag);
            globalflag = (i64)0;
        }break;
        case 113:;
        {
            mm_parse_readconstdef(owner,globalflag);
            globalflag = (i64)0;
        }break;
        case 117:;
        case 94:;
        {
            mm_parse_readclassdef(owner,globalflag);
            globalflag = (i64)0;
        }break;
        case 115:;
        {
            mm_lex_lex();
            mm_parse_readenumtype(owner,(i64)0,(i64)0);
        }break;
        case 136:;
        {
            mm_parse_readtabledef(globalflag);
            globalflag = (i64)0;
        }break;
        case 38:;
        {
            mm_support_serror((byte*)"DOCSTRING");
        }break;
        case 97:;
        {
            if (!!(globalflag)) {
                mm_support_serror((byte*)"glob/import?");
            };
            mm_lex_lex();
            if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_lx.subcode)) == (i64)39))) {
                mm_lex_lex();
            };
            mm_parse_checksymbol((i64)50);
            dimport = mm_decls_lx.symptr;
            name = mm_start_mapimport((*dimport).name);
            L651 :;
            for (i=(i64)1;i<=mm_decls_nmodules;++i) {
L652 :;
                if (!!(mlib_eqstring(name,mm_decls_moduletable[(i)].name))) {
                    stimport = mm_decls_moduletable[(i)].stmodule;
                    goto L654 ;
                };
L653 :;
            }
            {
                msysc_m_print_startcon();
                msysc_m_print_str((*mm_decls_lx.symptr).name,NULL);
                msysc_m_print_newline();
                msysc_m_print_end();
                ;
                mm_support_serror((byte*)"Import stmt out of position?");
            }L654 :;
            ;
            mm_lex_lex();
            mm_parse_domappedalias(dimport,stimport);
            if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)50) && !!(mlib_eqstring((*mm_decls_lx.symptr).name,(byte*)"as")))) {
                mm_parse_readimportalias(dimport);
            };
        }break;
        case 6:;
        {
            mm_lex_lex();
        }break;
        case 36:;
        {
            goto L647 ;
        }break;
        case 121:;
        {
            if (((i64)((u64)(mm_decls_lx.subcode)) == (i64)5)) {
                callbackflag = (i64)5;
                mm_lex_lex();
            } else {
                goto L655 ;
;
            };
        }break;
        case 110:;
        {
            mm_parse_readmacrodef(owner,globalflag);
            globalflag = (i64)0;
        }break;
        case 111:;
        {
            mm_support_serror((byte*)"MODULE/EXPAND");
        }break;
        case 112:;
        {
            mm_support_serror((byte*)"MODULE/OPERATOR");
        }break;
        case 2:;
        {
            mm_support_serror((byte*)"MODULE/DOT");
        }break;
        default: {
            //error:
L655 :;
;
            mm_lex_ps1((byte*)"symbol");
            mm_support_serror((byte*)"Not allowed at module level");
        }
        } //SW
;
    }L647 :;
    ;
    return (i64)1;
}

static void mm_parse_initparser(void) {
    byte *  tabledataname = (byte*)"";
    if (!(!!(mm_decls_nullunit))) {
        mm_decls_nullunit = mm_lib_createunit0((i64)2);
    };
    mm_parse_try_level = (i64)0;
    mm_decls_currproc = (struct mm_decls_strec *)(0);
    mm_parse_varattribs = (i64)0;
    mm_parse_intabledata = (i64)0;
    mm_parse_inreadprint = (i64)0;
    mm_parse_inparamlist = (i64)0;
    mm_parse_inrecordbody = (i64)0;
    mm_parse_inimportmodule = (i64)0;
    tabledataname = (byte*)"";
    mm_parse_labelseen = (i64)0;
    mm_parse_ndollar = (i64)0;
}

static void mm_parse_skipsemi(void) {
    L656 :;
    while (((i64)((u64)(mm_decls_lx.symbol)) == (i64)6)) {
        mm_lex_lex();
L657 :;
    }L658 :;
    ;
}

static struct mm_decls_unitrec * mm_parse_makeblock(struct mm_decls_unitrec * p) {
    return mm_lib_createunit1((i64)4,p);
}

static struct mm_decls_unitrec * mm_parse_makestmtblock(struct mm_decls_unitrec * p) {
    return mm_lib_createunit1((i64)5,p);
}

static void mm_parse_checkequals(void) {
    if (!((((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_lx.subcode)) == (i64)30)))) {
        mm_support_serror((byte*)"\"=\" expected");
    };
}

static i64 mm_parse_getcurrline(void) {
    return (i64)(mm_decls_lx.lineno);
}

static i64 mm_parse_checkbegin(i64 fbrack) {
    i64 closesym;
    mm_parse_skipsemi();
    if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)12) && !!(fbrack))) {
        closesym = (i64)13;
        mm_lex_lex();
    } else if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)16)) {
        closesym = (i64)17;
        mm_lex_lex();
    } else {
        closesym = (i64)66;
    };
    return closesym;
}

static void mm_parse_checkbeginend(i64 closesym,i64 kwd,i64 startline) {
    mm_parse_skipsemi();
    if (((closesym == (i64)13) || (closesym == (i64)17))) {
        mm_parse_checksymbol(closesym);
    } else {
        mm_parse_checkend(closesym,kwd,(i64)0,startline);
    };
    mm_lex_lex();
}

static void mm_parse_checkend(i64 endsym,i64 endkwd1,i64 endkwd2,i64 startline) {
    byte str[100];
    if (((endsym == (i64)((u64)(mm_decls_lx.symbol))) && ((i64)((u64)(mm_decls_lx.symbol)) == (i64)13))) {
        return;
    };
    if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)66)) {
        strcpy((i8 *)(str),(i8 *)((byte*)"Bad 'end' "));
        //error:
L659 :;
;
        if (!!(startline)) {
            msysc_m_print_startstr((str + (i64)(strlen((i8 *)(str)))));
            msysc_m_print_setfmt((byte*)" (from line #)");
            msysc_m_print_i64(startline,NULL);
            msysc_m_print_end();
            ;
        };
        mm_support_serror(str);
    };
    if (((i64)((u64)(mm_decls_lx.subcode)) == (i64)0)) {
        return;
    };
    if (!(((!!(endkwd1) && (endkwd1 == (i64)((u64)(mm_decls_lx.subcode)))) || (!!(endkwd2) && (endkwd2 == (i64)((u64)(mm_decls_lx.subcode))))))) {
        strcpy((i8 *)(str),(i8 *)((byte*)"Mismatched 'end'"));
        goto L659 ;
;
    };
}

static struct mm_decls_unitrec * mm_parse_readvardef(struct mm_decls_strec * owner,i64 isglobal,i64 isstatic,i64 varid) {
    struct mm_decls_unitrec *  ulist;
    struct mm_decls_unitrec *  ulistx;
    struct mm_decls_unitrec *  p;
    i64 nvars;
    i64 m;
    i64 k;
    struct mm_decls_strec *  stname;
    k = (i64)(mm_decls_lx.symbol);
    mm_lex_lex();
    ulist = (ulistx = (struct mm_decls_unitrec *)(0));
    if ((k != (i64)107)) {
        if (!!(mm_parse_istypestarter())) {
            m = mm_parse_readtypespec(owner,(i64)0);
        } else {
            m = (i64)52;
        };
    } else {
        m = (i64)50;
    };
    nvars = (i64)0;
    L660 :;
    while (((i64)((u64)(mm_decls_lx.symbol)) == (i64)50)) {
        ++nvars;
        stname = mm_lib_getduplnameptr(owner,mm_decls_lx.symptr,varid);
        mm_lib_storemode((i64)1,owner,m,&(*stname).mode);
        (*stname).isglobal = (u64)(isglobal);
        (*stname).isstatic = (u64)(isstatic);
        (*stname).islet = (u64)((k == (i64)106));
        mm_lib_adddef(owner,stname);
        if ((varid == (i64)9)) {
            mm_lib_addstatic(stname);
        };
        mm_lex_lex();
        if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)9) || (((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_lx.subcode)) == (i64)30)))) {
            if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)9)) {
                if ((varid == (i64)9)) {
                    mm_support_serror((byte*)"Need = on static not :=");
                };
            } else {
                if ((varid == (i64)10)) {
                    mm_support_serror((byte*)"Need 'static' for '='");
                    mm_lib_addstatic(stname);
                };
            };
            mm_lex_lex();
            (*stname).code = mm_parse_readunit();
            (*stname).equals = (u64)((i64)1);
            if ((varid == (i64)10)) {
                p = mm_lib_createunit2((i64)23,mm_lib_createname(stname),(*stname).code);
                mm_lib_addlistunit(&ulist,&ulistx,p);
            };
        } else if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)21)) {
            if ((k == (i64)106)) {
                mm_support_serror((byte*)"let@");
            };
            mm_lex_lex();
            (*stname).at = (u64)((i64)1);
            (*stname).equivvar = mm_parse_readunit();
        } else if ((k == (i64)106)) {
            mm_support_serror((byte*)"let needs :=/=");
        };
        if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)5)) {
            goto L662 ;
        };
        mm_lex_lex();
L661 :;
    }L662 :;
    ;
    if ((nvars == (i64)0)) {
        mm_support_serror((byte*)"No vars declared");
    };
    return ulist;
}

static void mm_parse_readconstdef(struct mm_decls_strec * owner,i64 isglobal) {
    i64 nconsts;
    i64 deft;
    i64 m;
    struct mm_decls_strec *  stname;
    mm_lex_lex();
    nconsts = (i64)0;
    if (!!(mm_parse_istypestarter())) {
        deft = mm_parse_readtypespec(owner,(i64)0);
    } else {
        deft = (i64)52;
    };
    L663 :;
    while (((i64)((u64)(mm_decls_lx.symbol)) == (i64)50)) {
        stname = mm_lib_getduplnameptr(owner,mm_decls_lx.symptr,(i64)8);
        mm_lex_lex();
        mm_parse_checkequals();
        mm_lex_lex();
        (*stname).code = mm_parse_readconstexpr((i64)1);
        m = deft;
        mm_lib_storemode((i64)2,owner,m,&(*stname).mode);
        ++nconsts;
        (*stname).isglobal = (u64)(isglobal);
        mm_lib_adddef(owner,stname);
        if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)5)) {
            goto L665 ;
        };
        mm_lex_lex();
L664 :;
    }L665 :;
    ;
    if ((nconsts == (i64)0)) {
        mm_support_serror((byte*)"No consts declared");
    };
}

static struct mm_decls_unitrec * mm_parse_readlbrack(void) {
    struct mm_decls_unitrec *  ulist;
    struct mm_decls_unitrec *  ulistx;
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  q;
    struct mm_decls_unitrec *  r;
    i64 length;
    mm_lex_lex();
    ulist = (ulistx = (struct mm_decls_unitrec *)(0));
    length = (i64)0;
    if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_nextlx.symbol)) == (i64)13))) {
        p = mm_lib_createunit0((i64)101);
        (*p).opcode = (i64)(mm_decls_lx.subcode);
        mm_lex_lex();
        mm_lex_lex();
        return p;
    } else if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_nextlx.symbol)) == (i64)9))) {
        p = mm_lib_createunit0((i64)101);
        (*p).opcode = mm_lib_getoptocode((i64)(mm_decls_lx.subcode));
        mm_lex_lex();
        mm_lex_lex();
        mm_parse_checksymbol((i64)13);
        mm_lex_lex();
        return p;
    };
    if (((i64)(mm_decls_lx.symbol)==(i64)13)) {
        mm_lex_lex();
        p = mm_lib_createunit0((i64)14);
        (*p).length = (i64)0;
        return p;
    } else {
        p = mm_parse_readxunit();
    };
    if (((i64)(mm_decls_lx.symbol)==(i64)13)) {
        mm_lex_lex();
        return p;
    }else if (((i64)(mm_decls_lx.symbol)==(i64)5)) {
        length = (i64)1;
        if (((i64)((u64)(mm_decls_nextlx.symbol)) == (i64)13)) {
            mm_lex_lex();
            mm_lex_lex();
            p = mm_lib_createunit1((i64)14,p);
            (*p).length = length;
            return p;
        };
        ulist = (ulistx = p);
        L666 :;
        do {
            mm_lex_lex();
            if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)13)) {
                goto L668 ;
            };
            if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
                mm_support_serror((byte*)",, null expr not allowed");
            };
            mm_lib_addlistunit(&ulist,&ulistx,mm_parse_readxunit());
            ++length;
            mm_parse_skipsemi();
L667 :;
        } while (!((i64)((u64)(mm_decls_lx.symbol)) != (i64)5));L668 :;
        ;
        mm_parse_checksymbol((i64)13);
        mm_lex_lex();
        p = mm_lib_createunit1((i64)14,ulist);
        (*p).length = length;
        return p;
    }else if (((i64)(mm_decls_lx.symbol)==(i64)19)) {
        mm_lex_lex();
        q = mm_parse_readxunit();
        if (((i64)(mm_decls_lx.symbol)==(i64)19)) {
            mm_lex_lex();
            r = mm_parse_readsunit((i64)0);
            mm_parse_checksymbol((i64)13);
            mm_lex_lex();
            return mm_lib_createunit3((i64)196,p,q,r);
        }else if (((i64)(mm_decls_lx.symbol)==(i64)13)) {
            mm_lex_lex();
            return mm_lib_createunit3((i64)196,p,q,(struct mm_decls_unitrec *)(0));
        };
        mm_lib_addlistunit(&ulist,&ulistx,q);
        mm_parse_checksymbol((i64)5);
        if (((i64)((u64)(mm_decls_nextlx.symbol)) != (i64)19)) {
            L669 :;
            do {
                mm_lex_lex();
                mm_lib_addlistunit(&ulist,&ulistx,mm_parse_readxunit());
L670 :;
            } while (!((i64)((u64)(mm_decls_lx.symbol)) != (i64)5));L671 :;
            ;
            mm_parse_checksymbol((i64)19);
        } else {
            mm_lex_lex();
        };
        mm_lex_lex();
        r = mm_parse_readxunit();
        mm_parse_checksymbol((i64)13);
        mm_lex_lex();
        return mm_lib_createunit3((i64)221,p,ulist,r);
    } else {
        mm_support_serror((byte*)"(x ...");
    };
    return (struct mm_decls_unitrec *)(0);
}

static void mm_parse_addlistparam(struct mm_decls_strec * * ulist,struct mm_decls_strec * * ulistx,struct mm_decls_strec * p) {
    if (((*ulist) == 0)) {
        (*ulist) = ((*ulistx) = p);
    } else {
        (*(*ulistx)).nextparam = p;
    };
    (*ulistx) = p;
}

static struct mm_decls_unitrec * mm_parse_readcast(i64 t) {
    struct mm_decls_unitrec *  p;
    i64 opc;
    if ((t == (i64)0)) {
        t = mm_parse_readtypespec(mm_decls_currproc,(i64)0);
    };
    if (((i64)(mm_decls_lx.symbol)==(i64)21)) {
        opc = (i64)99;
        mm_lex_lex();
    }else if (((i64)(mm_decls_lx.symbol)==(i64)2)) {
        if (((i64)((u64)(mm_decls_nextlx.symbol)) == (i64)102)) {
            mm_lex_lex();
            p = mm_lib_createunit0((i64)100);
            mm_lib_storemode((i64)4,mm_decls_currproc,t,(i32 *)(&(*p).value));
            mm_lex_lex();
        } else {
            p = mm_lib_createunit0((i64)100);
            mm_lib_storemode((i64)4,mm_decls_currproc,t,(i32 *)(&(*p).value));
        };
        return p;
    } else {
        opc = (i64)96;
    };
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)12)) {
        mm_lex_lex();
        p = mm_parse_readunit();
        mm_parse_checksymbol((i64)13);
        mm_lex_lex();
    } else {
        mm_support_serror((byte*)"INT A,B,C: MAY NEED VAR OR LET");
        p = mm_parse_readterm2();
    };
    p = mm_lib_createunit1(opc,p);
    mm_lib_storemode((i64)5,mm_decls_currproc,t,&(*p).newmode);
    return p;
}

static struct mm_decls_unitrec * mm_parse_readopc(void) {
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  q;
    i64 opc;
    opc = (i64)(mm_decls_lx.subcode);
    mm_lex_lex();
    if ((opc==(i64)37)) {
        return mm_parse_readterm2();
    }else if ((opc==(i64)38)) {
        opc = (i64)103;
    }else if ((opc==(i64)54) || (opc==(i64)55) || (opc==(i64)91) || (opc==(i64)126) || (opc==(i64)59) || (opc==(i64)60) || (opc==(i64)78) || (opc==(i64)77) || (opc==(i64)62) || (opc==(i64)63) || (opc==(i64)68) || (opc==(i64)69)) {
        mm_parse_checksymbol((i64)12);
        mm_lex_lex();
        p = mm_parse_readunit();
        if (((i64)(mm_decls_lx.symbol)==(i64)5)) {
            mm_lex_lex();
            q = mm_parse_readunit();
            mm_parse_checksymbol((i64)13);
            mm_lex_lex();
            return mm_lib_createunit2(opc,p,q);
        }else if (((i64)(mm_decls_lx.symbol)==(i64)13)) {
            if (((opc == (i64)62) || (opc == (i64)63) || (opc == (i64)78) || (opc == (i64)77))) {
                mm_lex_lex();
                return mm_lib_createunit1(opc,p);
            };
        };
        mm_support_serror((byte*)"readopc: missing 2nd opnd");
    } else {
        if (!!((u64)(mm_tables_binopset[(opc)]))) {
            mm_support_serror((byte*)"Can't be used as unary op");
        };
    };
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)9)) {
        mm_lex_lex();
        opc = mm_lib_getoptocode(opc);
    };
    p = mm_lib_createunit1(opc,mm_parse_readterm2());
    return p;
}

static struct mm_decls_unitrec * mm_parse_readsprint(void) {
    i64 oldinreadprint;
    i64 opc;
    i64 isfprint;
    struct mm_decls_unitrec *  pformat;
    struct mm_decls_unitrec *  pdev;
    struct mm_decls_unitrec *  printlist;
    struct mm_decls_unitrec *  printlistx;
    struct mm_decls_unitrec *  p;
    oldinreadprint = mm_parse_inreadprint;
    mm_parse_inreadprint = (i64)1;
    opc = (i64)(mm_decls_lx.subcode);
    mm_lex_lex();
    mm_parse_checksymbol((i64)12);
    mm_lex_lex();
    if ((opc==(i64)230) || (opc==(i64)227)) {
        isfprint = (i64)1;
    } else {
        isfprint = (i64)0;
    };
    printlist = (printlistx = (struct mm_decls_unitrec *)(0));
    pformat = (pdev = (struct mm_decls_unitrec *)(0));
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)21)) {
        mm_lex_lex();
        pdev = mm_parse_readunit();
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
            mm_lex_lex();
        } else {
            goto L672 ;
;
        };
    };
    if (!!(isfprint)) {
        pformat = mm_parse_readunit();
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
            mm_lex_lex();
        } else {
            goto L672 ;
;
        };
    };
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)13)) {
        goto L672 ;
;
    };
    L673 :;
    while (1) {
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
            mm_lib_addlistunit(&printlist,&printlistx,mm_lib_createunit0((i64)188));
        } else {
            p = mm_parse_readunit();
            if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)7)) {
                mm_lex_lex();
                p = mm_lib_createunit2((i64)187,p,mm_parse_readunit());
            };
            mm_lib_addlistunit(&printlist,&printlistx,p);
        };
        if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)5)) {
            goto L674 ;
        };
        mm_lex_lex();
    }L674 :;
    ;
    mm_parse_checksymbol((i64)13);
    //finish:
L672 :;
;
    mm_lex_lex();
    mm_parse_inreadprint = oldinreadprint;
    if ((((opc == (i64)223) || (opc == (i64)225)) && (printlist == 0))) {
        mm_support_serror((byte*)"No print items");
    };
    if (!!(isfprint)) {
        if (((i64)((*pformat).tag) == (i64)2)) {
            mm_support_serror((byte*)"No fmt str");
        };
        return mm_lib_createunit3(opc,pdev,pformat,printlist);
    } else {
        return mm_lib_createunit2(opc,pdev,printlist);
    };
}

static struct mm_decls_unitrec * mm_parse_readsread(void) {
    i64 oldinreadprint;
    i64 opc;
    struct mm_decls_unitrec *  pformat;
    struct mm_decls_unitrec *  pdev;
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  readlist;
    struct mm_decls_unitrec *  readlistx;
    oldinreadprint = mm_parse_inreadprint;
    mm_parse_inreadprint = (i64)1;
    opc = (i64)(mm_decls_lx.subcode);
    mm_lex_lex();
    mm_parse_checksymbol((i64)12);
    mm_lex_lex();
    readlist = (readlistx = (struct mm_decls_unitrec *)(0));
    pformat = (pdev = (struct mm_decls_unitrec *)(0));
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)21)) {
        if ((opc == (i64)231)) {
            mm_support_serror((byte*)"@ on read");
        };
        mm_lex_lex();
        pdev = mm_parse_readunit();
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
            mm_lex_lex();
        } else {
            goto L675 ;
;
        };
    };
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)13)) {
        goto L675 ;
;
    };
    L676 :;
    while (1) {
        p = mm_parse_readunit();
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)7)) {
            mm_lex_lex();
            p = mm_lib_createunit2((i64)187,p,mm_parse_readunit());
        };
        mm_lib_addlistunit(&readlist,&readlistx,p);
        if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)5)) {
            goto L677 ;
        };
        mm_lex_lex();
    }L677 :;
    ;
    mm_parse_checksymbol((i64)13);
    //finish:
L675 :;
;
    mm_lex_lex();
    mm_parse_inreadprint = oldinreadprint;
    if (((opc == (i64)231) && (readlist == 0))) {
        mm_support_serror((byte*)"No read items");
    };
    return mm_lib_createunit2(opc,pdev,readlist);
}

static struct mm_decls_unitrec * mm_parse_readcompilervar(void) {
    byte str[100];
    struct oslinux_rsystemtime tm;
    static byte *  monthnames[12] = {
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
    struct mm_decls_unitrec *  p;
    if (((i64)(mm_decls_lx.subcode)==(i64)173)) {
        p = mm_lib_createunit0((i64)173);
        mm_lex_lex();
        return p;
    }else if (((i64)(mm_decls_lx.subcode)==(i64)174)) {
        msysc_getstrint(((i64)(mm_decls_lx.lineno) & (i64)16777215),str);
    }else if (((i64)(mm_decls_lx.subcode)==(i64)175)) {
        p = mm_lib_createunit0((i64)175);
        mm_lex_lex();
        return p;
    }else if (((i64)(mm_decls_lx.subcode)==(i64)176)) {
        p = mm_lib_createunit0((i64)176);
        mm_lex_lex();
        return p;
    }else if (((i64)(mm_decls_lx.subcode)==(i64)177)) {
        strcpy((i8 *)(str),(i8 *)((*mm_decls_currproc).name));
    }else if (((i64)(mm_decls_lx.subcode)==(i64)178)) {
        oslinux_os_getsystime(&tm);
        msysc_m_print_startstr(str);
        msysc_m_print_setfmt((byte*)"#-#-#");
        msysc_m_print_i64(tm.day,NULL);
        msysc_m_print_str(monthnames[((i64)(tm.month))-1],NULL);
        msysc_m_print_i64(tm.year,(byte*)"4");
        msysc_m_print_end();
        ;
    }else if (((i64)(mm_decls_lx.subcode)==(i64)179)) {
        oslinux_os_getsystime(&tm);
        msysc_m_print_startstr(str);
        msysc_m_print_setfmt((byte*)"#:#:#");
        msysc_m_print_i64(tm.hour,(byte*)"z2");
        msysc_m_print_i64(tm.minute,(byte*)"z2");
        msysc_m_print_i64(tm.second,(byte*)"z2");
        msysc_m_print_end();
        ;
    }else if (((i64)(mm_decls_lx.subcode)==(i64)182)) {
        mm_lex_lex();
        return mm_lib_createconstunit((u64)(mm_decls_targetbits),(i64)4);
    }else if (((i64)(mm_decls_lx.subcode)==(i64)183)) {
        mm_lex_lex();
        return mm_lib_createconstunit((u64)(mm_decls_targetsize),(i64)4);
    }else if (((i64)(mm_decls_lx.subcode)==(i64)184)) {
        strcpy((i8 *)(str),(i8 *)(mm_decls_targetnames[(mm_decls_target)-1]));
    } else {
        mm_support_serror_s((byte*)"compiler var not impl: #",mm_tables_jtagnames[((i64)(mm_decls_lx.subcode))]);
    };
    mm_lex_lex();
    return mm_lib_createstringconstunit(mlib_pcm_copyheapstring(str),(i64)-1);
}

static struct mm_decls_unitrec * mm_parse_readcastx(void) {
    mm_lex_lex();
    return mm_lib_createunit1((i64)98,mm_parse_readterm2());
}

void mm_parse_checksymbol(i64 symbol) {
    byte str[100];
    if (((i64)((u64)(mm_decls_lx.symbol)) != symbol)) {
        msysc_m_print_startstr(str);
        msysc_m_print_setfmt((byte*)"# expected, not #");
        msysc_m_print_str(mm_tables_symbolnames[(symbol)-1],NULL);
        msysc_m_print_str(mm_tables_symbolnames[((i64)(mm_decls_lx.symbol))-1],NULL);
        msysc_m_print_end();
        ;
        mm_support_serror(str);
    };
}

static i64 mm_parse_readtypespec(struct mm_decls_strec * owner,i64 typedefx) {
    struct mm_decls_strec *  d;
    i64 t;
    i64 fflang;
    struct mm_decls_unitrec *  x;
    struct mm_decls_unitrec *  dim;
    struct mm_decls_unitrec *  length;
    struct mm_decls_unitrec *  dims[30];
    i64 ndims;
    i64 i;
    i64 k;
    if (((i64)(mm_decls_lx.symbol)==(i64)14)) {
        //arraybounds:
L678 :;
;
        mm_lex_lex();
        ndims = (i64)0;
        mm_parse_inreadprint = (i64)1;
        L679 :;
        while (1) {
            length = (struct mm_decls_unitrec *)(0);
            if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)15) || ((i64)((u64)(mm_decls_lx.symbol)) == (i64)5))) {
                dim = (struct mm_decls_unitrec *)(0);
            } else {
                dim = mm_parse_readunit();
                if (((i64)(mm_decls_lx.symbol)==(i64)15) || ((i64)(mm_decls_lx.symbol)==(i64)5)) {
                }else if (((i64)(mm_decls_lx.symbol)==(i64)7)) {
                    mm_lex_lex();
                    if (!((((i64)((u64)(mm_decls_lx.symbol)) == (i64)5) || ((i64)((u64)(mm_decls_lx.symbol)) == (i64)15)))) {
                        length = mm_parse_readunit();
                        dim = mm_lib_createunit2((i64)22,dim,length);
                    } else {
                        dim = mm_lib_createunit1((i64)22,dim);
                    };
                };
            };
            if ((ndims >= (i64)30)) {
                mm_support_serror((byte*)"Too many array dims");
            };
            dims[(++ndims)-1] = dim;
            if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)5)) {
                goto L680 ;
            };
            mm_lex_lex();
        }L680 :;
        ;
        mm_parse_inreadprint = (i64)0;
        mm_parse_checksymbol((i64)15);
        mm_lex_lex();
        t = mm_parse_readtypespec(owner,(i64)0);
        L681 :;
        for (i=ndims;i>=(i64)1;--i) {
L682 :;
            t = mm_lib_createarraymode(owner,t,dims[(i)-1],((i == (i64)1)?typedefx:(i64)0));
L683 :;
        }L684 :;
        ;
        return t;
    }else if (((i64)(mm_decls_lx.symbol)==(i64)53)) {
        t = (i64)(mm_decls_lx.subcode);
        mm_lex_lex();
        if ((t==(i64)34)) {
            if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_lx.subcode)) == (i64)39))) {
                mm_lex_lex();
                length = mm_parse_readterm2();
                t = mm_lib_createarraymode(owner,(i64)13,length,typedefx);
            };
        }else if ((t==(i64)31)) {
            if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)14)) {
                mm_lex_lex();
                if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_lx.subcode)) == (i64)39))) {
                    t = (i64)39;
                    mm_lex_lex();
                } else {
                    length = mm_parse_readunit();
                    t = mm_lib_createsetmode(owner,length,typedefx);
                };
                mm_parse_checksymbol((i64)15);
                mm_lex_lex();
            } else {
                t = (i64)39;
            };
        }else if ((t==(i64)40)) {
            mm_parse_checksymbol((i64)14);
            mm_lex_lex();
            k = mm_parse_readtypespec(owner,(i64)0);
            mm_parse_checksymbol((i64)15);
            mm_lex_lex();
            t = mm_parse_readtypespec(owner,(i64)0);
            t = mm_lib_createdictmode(owner,t,k,typedefx);
        };
    }else if (((i64)(mm_decls_lx.symbol)==(i64)50)) {
        d = mm_decls_lx.symptr;
        mm_lex_lex();
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)2)) {
            mm_lex_lex();
            mm_parse_checksymbol((i64)50);
            t = mm_lib_newusertypex(d,mm_decls_lx.symptr);
            mm_lex_lex();
        } else {
            t = mm_lib_newusertypex(d,(struct mm_decls_strec *)(0));
        };
    }else if (((i64)(mm_decls_lx.symbol)==(i64)115)) {
        mm_lex_lex();
        t = mm_parse_readenumtype(owner,typedefx,(i64)0);
    }else if (((i64)(mm_decls_lx.symbol)==(i64)12)) {
        t = mm_parse_readenumtype(owner,typedefx,(i64)0);
    }else if (((i64)(mm_decls_lx.symbol)==(i64)94) || ((i64)(mm_decls_lx.symbol)==(i64)95)) {
        mm_support_serror((byte*)"Use 'record name =' syntax");
    }else if (((i64)(mm_decls_lx.symbol)==(i64)96)) {
        mm_support_serror((byte*)"Top-level union not allowed");
    }else if (((i64)(mm_decls_lx.symbol)==(i64)104)) {
        fflang = (i64)0;
        //retry:
L685 :;
;
        mm_lex_lex();
        if (((i64)(mm_decls_lx.symbol)==(i64)91) || ((i64)(mm_decls_lx.symbol)==(i64)92)) {
            t = mm_parse_readrefproc(owner,typedefx,fflang);
        }else if (((i64)(mm_decls_lx.symbol)==(i64)121)) {
            fflang = (i64)(mm_decls_lx.subcode);
            goto L685 ;
;
        } else {
            if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)53)) {
                if (((i64)(mm_decls_lx.subcode)==(i64)54)) {
                    t = mm_tables_treflabel;
                }else if (((i64)(mm_decls_lx.subcode)==(i64)17) || ((i64)(mm_decls_lx.subcode)==(i64)18) || ((i64)(mm_decls_lx.subcode)==(i64)19)) {
                    t = mm_lib_createrefbitmode(owner,(i64)(mm_decls_lx.subcode),typedefx);
                }else if (((i64)(mm_decls_lx.subcode)==(i64)13)) {
                    t = mm_tables_trefchar;
                } else {
                    goto L686 ;
;
                };
                mm_lex_lex();
            } else {
                //readtarget:
L686 :;
;
                t = mm_parse_readtypespec(owner,(i64)0);
                t = mm_lib_createrefmode(owner,t,typedefx);
            };
        };
    }else if (((i64)(mm_decls_lx.symbol)==(i64)58)) {
        mm_lex_lex();
        t = mm_tables_trefchar;
    }else if (((i64)(mm_decls_lx.symbol)==(i64)55)) {
        mm_lex_lex();
        mm_parse_checksymbol((i64)12);
        mm_lex_lex();
        mm_parse_checksymbol((i64)50);
        mm_support_serror((byte*)"TYPEOF/NO 'NAME' MODULE");
        if (!!(d)) {
            t = (i64)((*d).mode);
        } else {
            mm_support_serror((byte*)"Typeof?");
        };
        mm_lex_lex();
        mm_parse_checksymbol((i64)13);
        mm_lex_lex();
    }else if (((i64)(mm_decls_lx.symbol)==(i64)56)) {
        mm_lex_lex();
        x = mm_parse_readunit();
        if (((i64)((*x).tag) != (i64)15)) {
            mm_support_serror((byte*)"range expected");
        };
        t = mm_lib_createsubrangemode(owner,x,typedefx);
    }else if (((i64)(mm_decls_lx.symbol)==(i64)108)) {
        t = mm_parse_readslicetype(owner,typedefx);
    }else if (((i64)(mm_decls_lx.symbol)==(i64)109)) {
        t = mm_parse_readflexarray(owner,typedefx);
    }else if (((i64)(mm_decls_lx.symbol)==(i64)107)) {
        mm_lex_lex();
        t = (i64)50;
    } else {
        mm_support_serror((byte*)"Bad type starter");
    };
    return t;
}

static i64 mm_parse_readslicetype(struct mm_decls_strec * owner,i64 typedefx) {
    struct mm_decls_unitrec *  plower;
    i64 t;
    mm_lex_lex();
    mm_parse_checksymbol((i64)14);
    mm_lex_lex();
    if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)15)) {
        mm_parse_inreadprint = (i64)1;
        plower = mm_parse_readunit();
        mm_parse_inreadprint = (i64)0;
        mm_parse_checksymbol((i64)7);
        mm_lex_lex();
        mm_parse_checksymbol((i64)15);
    } else {
        plower = (struct mm_decls_unitrec *)(0);
    };
    mm_lex_lex();
    t = mm_parse_readtypespec(owner,typedefx);
    return mm_lib_createslicemode(owner,t,plower,typedefx);
}

static i64 mm_parse_readflexarray(struct mm_decls_strec * owner,i64 typedefx) {
    i64 t;
    mm_lex_lex();
    mm_parse_checksymbol((i64)14);
    mm_lex_lex();
    mm_parse_checksymbol((i64)15);
    mm_lex_lex();
    t = mm_parse_readtypespec(owner,typedefx);
    return mm_lib_createflexarraymode(owner,t,typedefx);
}

static struct mm_decls_unitrec * mm_parse_readslist(i64 iscall,i64 donulls) {
    struct mm_decls_unitrec *  ulist;
    struct mm_decls_unitrec *  ulistx;
    i64 oldinparamlist;
    ulist = (ulistx = (struct mm_decls_unitrec *)(0));
    mm_parse_skipsemi();
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)13)) {
        return ulist;
    };
    oldinparamlist = mm_parse_inparamlist;
    mm_parse_inparamlist = iscall;
    L687 :;
    while (1) {
        mm_parse_skipsemi();
        if (((i64)(mm_decls_lx.symbol)==(i64)5)) {
            if (!!(donulls)) {
                mm_lib_addlistunit(&ulist,&ulistx,mm_lib_createunit0((i64)2));
            } else {
                mm_support_serror((byte*)"null comma expr not allowed");
            };
            mm_lex_lex();
        }else if (((i64)(mm_decls_lx.symbol)==(i64)13)) {
            if (!!(donulls)) {
                mm_lib_addlistunit(&ulist,&ulistx,mm_decls_nullunit);
            };
            goto L688 ;
        } else {
            mm_lib_addlistunit(&ulist,&ulistx,mm_parse_readunit());
            if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
                mm_lex_lex();
                if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)13)) {
                    goto L688 ;
                };
            } else {
                mm_parse_skipsemi();
                if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)13)) {
                    goto L688 ;
                };
                mm_support_serror((byte*)"SLIST?");
            };
        };
    }L688 :;
    ;
    mm_parse_inparamlist = oldinparamlist;
    return ulist;
}

static struct mm_decls_unitrec * mm_parse_readindex(struct mm_decls_unitrec * p,i64 dot) {
    struct mm_decls_unitrec *  q;
    struct mm_decls_unitrec *  plower;
    struct mm_decls_unitrec *  pupper;
    mm_lex_lex();
    if (!(!!(dot))) {
        if (((i64)(mm_decls_lx.symbol)==(i64)15)) {
            //fullslice:
L689 :;
;
            mm_lex_lex();
            plower = mm_lib_createunit1((i64)127,mm_lib_duplunit(p,(i64)0));
            pupper = mm_lib_createunit1((i64)128,mm_lib_duplunit(p,(i64)0));
            p = mm_lib_createunit2((i64)83,p,mm_lib_createunit2((i64)15,plower,pupper));
            return p;
        }else if (((i64)(mm_decls_lx.symbol)==(i64)29) || ((i64)(mm_decls_lx.symbol)==(i64)7)) {
            mm_lex_lex();
            mm_parse_checksymbol((i64)15);
            goto L689 ;
;
        };
    };
    L690 :;
    while (1) {
        if ((mm_parse_ndollar >= (i64)10)) {
            mm_support_serror((byte*)"Too many nested a[$]");
        };
        mm_parse_dollarstack[(++mm_parse_ndollar)-1] = p;
        q = mm_parse_readunit();
        --mm_parse_ndollar;
        if (((i64)((*q).tag) == (i64)15)) {
            p = mm_lib_createunit2((!!(dot)?(i64)85:(i64)83),p,q);
        } else {
            p = mm_lib_createunit2((!!(dot)?(i64)84:(i64)82),p,q);
        };
        if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)5)) {
            goto L691 ;
        };
        mm_lex_lex();
    }L691 :;
    ;
    mm_parse_checksymbol((i64)15);
    mm_lex_lex();
    return p;
}

static struct mm_decls_unitrec * mm_parse_readdotsuffix(struct mm_decls_unitrec * p) {
    L692 :;
    while (((i64)((u64)(mm_decls_lx.symbol)) == (i64)2)) {
        mm_lex_lex();
        switch ((i64)(mm_decls_lx.symbol)) {
        case 14:;
        {
            p = mm_parse_readindex(p,(i64)1);
        }break;
        case 50:;
        {
            p = mm_lib_createunit2((i64)89,p,mm_lib_createname(mm_decls_lx.symptr));
            mm_lex_lex();
        }break;
        case 32:;
        case 33:;
        {
            p = mm_lib_createunit1((i64)(mm_decls_lx.subcode),p);
            mm_lex_lex();
        }break;
        case 34:;
        {
            p = mm_lib_createunit1((i64)135,p);
            (*p).opcode = (i64)(mm_decls_lx.subcode);
            mm_lex_lex();
        }break;
        case 12:;
        {
            mm_lex_lex();
            p = mm_lib_createunit2((i64)90,p,mm_parse_readunit());
            mm_parse_checksymbol((i64)13);
            mm_lex_lex();
        }break;
        case 102:;
        {
            if (((i64)((*p).tag)==(i64)100)) {
            } else {
                p = mm_lib_createunit1((i64)133,p);
            };
            mm_lex_lex();
        }break;
        default: {
            mm_support_serror((byte*)"Unknown dot suffix");
        }
        } //SW
;
L693 :;
    }L694 :;
    ;
    return p;
}

i64 mm_parse_isconstexpr(struct mm_decls_unitrec * p) {
    return ((i64)((*p).tag) == (i64)1);
}

static struct mm_decls_unitrec * mm_parse_readkeyindex(struct mm_decls_unitrec * p) {
    struct mm_decls_unitrec *  q;
    mm_lex_lex();
    p = mm_parse_readunit();
    q = (struct mm_decls_unitrec *)(0);
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
        mm_lex_lex();
        q = mm_parse_readunit();
    };
    p = mm_lib_createunit2((i64)88,p,q);
    mm_parse_checksymbol((i64)17);
    mm_lex_lex();
    return p;
}

static struct mm_decls_unitrec * mm_parse_readconstexpr(i64 needconst) {
    return mm_parse_readunit();
}

static i64 mm_parse_readconstint(void) {
    i64 x;
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)40)) {
        x = mm_decls_lx.value;
        mm_lex_lex();
        return x;
    } else if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_lx.subcode)) == (i64)38))) {
        mm_lex_lex();
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)40)) {
            x = mm_decls_lx.value;
            mm_lex_lex();
            return -(x);
        };
    };
    mm_support_serror((byte*)"Can't do complex expr");
    return (i64)0;
}

static void mm_parse_readprocdef(struct mm_decls_strec * procowner,i64 isglobal,i64 fflang) {
    i64 kwd;
    i64 startline;
    i64 closesym;
    struct mm_decls_strec *  stproc;
    struct mm_decls_strec *  stname;
    kwd = (i64)(mm_decls_lx.symbol);
    stproc = mm_parse_readprocdecl(procowner,isglobal,fflang);
    mm_parse_checkequals();
    mm_lex_lex();
    startline = mm_parse_getcurrline();
    closesym = mm_parse_checkbegin((i64)0);
    mm_parse_pushproc(stproc);
    mm_lib_nextavindex = (i64)0;
    if (!!(mm_parse_dretvar)) {
        stname = mm_lib_getduplnameptr(stproc,mm_parse_dretvar,(i64)10);
        mm_lib_storemode((i64)1,stproc,(i64)((*stproc).mode),&(*stname).mode);
        mm_lib_adddef(stproc,stname);
    };
    mm_lib_addtoproclist(stproc);
    (*stproc).code = mm_parse_readsunit((i64)0);
    mm_parse_checkbeginend(closesym,kwd,startline);
    (*stproc).equals = (u64)((i64)1);
    mm_parse_popproc();
}

struct mm_decls_strec * mm_parse_readprocdecl(struct mm_decls_strec * procowner,i64 isglobal,i64 fflang) {
    i64 kwd;
    i64 varparams;
    i64 try_level;
    i64 prettype;
    i64 nparams;
    i64 nretvalues;
    i64 retmodes[4];
    byte *  metadata;
    byte *  truename;
    struct mm_decls_strec *  pequiv;
    struct mm_decls_strec *  stproc;
    struct mm_decls_strec *  owner;
    struct mm_decls_strec *  paramlist;
    struct mm_decls_strec *  nameptr;
    i64 i;
    kwd = (i64)(mm_decls_lx.symbol);
    pequiv = (struct mm_decls_strec *)(0);
    metadata = (byte*)"";
    truename = (byte *)(0);
    varparams = (i64)0;
    try_level = (i64)0;
    mm_lex_lex();
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)45)) {
        truename = mlib_pcm_copyheapstring(mm_decls_lx.svalue);
        mlib_convlcstring(mm_decls_lx.svalue);
        mm_decls_lx.symptr = mm_lex_addnamestr(mm_decls_lx.svalue);
    } else {
        mm_parse_checksymbol((i64)50);
    };
    nameptr = mm_decls_lx.symptr;
    stproc = mm_lib_getduplnameptr(procowner,nameptr,(!!(mm_parse_insidedllimport)?(i64)6:(i64)5));
    if (!!(mm_parse_insidedllimport)) {
        isglobal = (i64)1;
    };
    if (!!(truename)) {
        (*stproc).truename = truename;
    };
    if ((((u64)((*(*stproc).name)) == '$') && !!(mlib_eqstring((*stproc).name,(byte*)"$init")))) {
        mm_decls_moduletable[((i64)((*mm_decls_stmodule).moduleno))].stinitproc = stproc;
    };
    mm_lib_adddef(procowner,stproc);
    if (((i64)((u64)((*stproc).nameid)) == (i64)6)) {
        (*stproc).imported = (u64)((i64)1);
        if (!!(mlib_eqstring((*procowner).name,(byte*)"cstd"))) {
            (*stproc).imported = (u64)((i64)2);
        };
    };
    owner = stproc;
    mm_parse_pushproc(stproc);
    mm_lex_lex();
    paramlist = (struct mm_decls_strec *)(0);
    prettype = (i64)0;
    nparams = (i64)0;
    nretvalues = (i64)0;
    if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_lx.subcode)) == (i64)32))) {
        if (((i64)((u64)((*stproc).nameid)) == (i64)6)) {
            mm_support_serror((byte*)"Metadata on dllproc");
        };
        mm_lex_lex();
        mm_parse_checksymbol((i64)45);
        (*stproc).metadata = mm_decls_lx.svalue;
        mm_lex_lex();
        if (!((((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && (((i64)((u64)(mm_decls_lx.subcode)) == (i64)34) || ((i64)((u64)(mm_decls_lx.subcode)) == (i64)35))))) {
            mm_support_serror((byte*)"\">\" expected");
        };
        if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_lx.subcode)) == (i64)35))) {
            mm_decls_lx.subcode = (u64)((i64)30);
        } else {
            mm_lex_lex();
        };
    };
    nretvalues = (i64)0;
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)12)) {
        mm_lex_lex();
        if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)13)) {
            paramlist = mm_parse_readparams(procowner,stproc,fflang,&varparams,&nparams);
            mm_parse_checksymbol((i64)13);
        };
        mm_lex_lex();
        if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)7) || ((i64)((u64)(mm_decls_lx.symbol)) == (i64)11))) {
            mm_lex_lex();
            nretvalues = mm_parse_readreturntype(owner,&retmodes);
        } else if ((!!((u64)(mm_decls_typestarterset[((i64)(mm_decls_lx.symbol))])) || ((i64)((u64)(mm_decls_lx.symbol)) == (i64)50))) {
            nretvalues = mm_parse_readreturntype(owner,&retmodes);
        };
    } else if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)7) || ((i64)((u64)(mm_decls_lx.symbol)) == (i64)11))) {
        mm_lex_lex();
        nretvalues = mm_parse_readreturntype(owner,&retmodes);
    };
    if (!!(nretvalues)) {
        prettype = retmodes[((i64)1)-1];
    };
    mm_parse_dretvar = (struct mm_decls_strec *)(0);
    if ((nretvalues == (i64)1)) {
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)50)) {
            mm_parse_dretvar = mm_decls_lx.symptr;
            mm_lex_lex();
        };
    };
    if (!((!!(nretvalues) || (kwd != (i64)92)))) {
        mm_support_serror((byte*)"Function needs ret type");
    };
    if ((!!(nretvalues) && (kwd != (i64)92))) {
        mm_support_serror((byte*)"Proc can't return value");
    };
    (*stproc).paramlist = paramlist;
    (*stproc).nretvalues = (u64)(nretvalues);
    L695 :;
    for (i=(i64)1;i<=nretvalues;++i) {
L696 :;
        mm_lib_storemode((i64)6,procowner,retmodes[(i)-1],&(*stproc).modelist[(i)-1]);
L697 :;
    }L698 :;
    ;
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)21)) {
        mm_lex_lex();
        mm_parse_checksymbol((i64)50);
        mm_lex_lex();
        (*stproc).at = (u64)((i64)1);
    };
    (*stproc).code = (struct mm_decls_unitrec *)(0);
    if ((fflang==(i64)2) || (fflang==(i64)1) || (fflang==(i64)3)) {
    } else {
        if (((i64)((*procowner).nameid)==(i64)2)) {
        }else if (((i64)((*procowner).nameid)==(i64)3)) {
            mm_support_serror((byte*)"Need FF specifier");
        };
    };
    (*stproc).isglobal = (u64)(isglobal);
    (*stproc).varparams = (u64)(varparams);
    (*stproc).fflang = (u64)(fflang);
    if ((((procowner == mm_decls_stmodule) && (((i64)((u64)((*stproc).namelen)) == (i64)5) && !!(mlib_eqstring((*stproc).name,(byte*)"start")))) || (((i64)((u64)((*stproc).namelen)) == (i64)4) && !!(mlib_eqstring((*stproc).name,(byte*)"main"))))) {
        (*stproc).isglobal = (u64)((i64)1);
    };
    mm_parse_popproc();
    return stproc;
}

static struct mm_decls_strec * mm_parse_readparams(struct mm_decls_strec * procowner,struct mm_decls_strec * owner,i64 fflang,i64 * varparams,i64 * nparams) {
    struct mm_decls_strec *  stlist;
    struct mm_decls_strec *  stlistx;
    struct mm_decls_strec *  stname;
    i64 parammode;
    i64 pmode;
    i64 m;
    stlist = (stlistx = (struct mm_decls_strec *)(0));
    pmode = (i64)50;
    (*nparams) = (i64)0;
    if ((fflang == (i64)0)) {
        fflang = (i64)3;
    };
    if ((((mm_decls_lx.symbol == (i64)57) || (mm_decls_lx.symbol == (i64)24)) || (((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_lx.subcode)) == (i64)49)))) {
        pmode = (i64)50;
    } else if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)50) && ((mm_decls_nextlx.symbol == (i64)5) || (mm_decls_nextlx.symbol == (i64)13)))) {
        if ((fflang != (i64)3)) {
            pmode = mm_parse_readtypespec(procowner,(i64)0);
            return mm_parse_readparams_types(procowner,owner,fflang,varparams,nparams,pmode);
        } else {
            pmode = (i64)50;
        };
    } else {
        pmode = mm_parse_readtypespec(procowner,(i64)0);
        if (((mm_decls_lx.symbol == (i64)5) || (mm_decls_lx.symbol == (i64)13))) {
            return mm_parse_readparams_types(procowner,owner,fflang,varparams,nparams,pmode);
        };
    };
    goto L699 ;
;
    L700 :;
    while (1) {
        if (!!(mm_parse_istypestarter())) {
            pmode = mm_parse_readtypespec(procowner,(i64)0);
        };
        //gotmode:
L699 :;
;
        parammode = (i64)0;
        if (((i64)(mm_decls_lx.symbol)==(i64)32)) {
            if (((i64)((u64)(mm_decls_lx.subcode)) != (i64)49)) {
                mm_support_serror((byte*)"params/op?");
            };
            parammode = (i64)1;
            mm_lex_lex();
            if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)7)) {
                mm_lex_lex();
            };
        }else if (((i64)(mm_decls_lx.symbol)==(i64)57) || ((i64)(mm_decls_lx.symbol)==(i64)24)) {
            parammode = (i64)2;
            mm_lex_lex();
            if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)7)) {
                mm_lex_lex();
            };
        }else if (((i64)(mm_decls_lx.symbol)==(i64)23)) {
            if ((pmode != (i64)50)) {
                mm_support_serror((byte*)"? not on variant");
            };
            parammode = (i64)3;
        };
        mm_parse_checksymbol((i64)50);
        ++(*nparams);
        stname = mm_lib_getduplnameptr(owner,mm_decls_lx.symptr,(i64)11);
        mm_lib_adddef(owner,stname);
        mm_lex_lex();
        if ((parammode == (i64)2)) {
            m = mm_lib_createrefmode(procowner,pmode,(i64)0);
        } else {
            m = pmode;
        };
        mm_lib_storemode((i64)7,owner,m,&(*stname).mode);
        (*stname).parammode = (u64)(parammode);
        mm_parse_addlistparam(&stlist,&stlistx,stname);
        if (((i64)(mm_decls_lx.symbol)==(i64)9)) {
            mm_lex_lex();
            //dodefvalue:
L702 :;
;
            (*stname).code = mm_parse_readunit();
            (*stname).equals = (u64)((i64)1);
            (*stname).optional = (u64)((i64)1);
        }else if (((i64)(mm_decls_lx.symbol)==(i64)32)) {
            if (((i64)((u64)(mm_decls_lx.subcode)) == (i64)30)) {
                mm_lex_lex();
                goto L702 ;
;
            };
        };
        if (((i64)(mm_decls_lx.symbol)==(i64)5)) {
            mm_lex_lex();
        }else if (((i64)(mm_decls_lx.symbol)==(i64)13)) {
            goto L701 ;
        } else {
            mm_support_serror((byte*)"nameparams1");
        };
    }L701 :;
    ;
    return stlist;
}

static struct mm_decls_strec * mm_parse_readparams_types(struct mm_decls_strec * procowner,struct mm_decls_strec * owner,i64 fflang,i64 * varparams,i64 * nparams,i64 pmode) {
    struct mm_decls_strec *  stlist;
    struct mm_decls_strec *  stlistx;
    struct mm_decls_strec *  stname;
    i64 m;
    byte str[30];
    stlist = (stlistx = (struct mm_decls_strec *)(0));
    stname = (struct mm_decls_strec *)(0);
    (*nparams) = (i64)0;
    goto L703 ;
;
    L704 :;
    while (1) {
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)30)) {
            (*varparams) = (i64)1;
            mm_lex_lex();
            mm_parse_checksymbol((i64)13);
            goto L705 ;
        };
        pmode = mm_parse_readtypespec(procowner,(i64)0);
        //gotmode:
L703 :;
;
        ++(*nparams);
        msysc_m_print_startstr(str);
        msysc_m_print_str((!!(mm_decls_ctarget)?(byte*)"_":(byte*)"$"),NULL);
        msysc_m_print_nogap();
        msysc_m_print_i64((*nparams),NULL);
        msysc_m_print_end();
        ;
        stname = mm_lib_getduplnameptr(owner,mm_lex_addnamestr(str),(i64)11);
        mm_lib_adddef(owner,stname);
        m = pmode;
        mm_lib_storemode((i64)8,owner,pmode,&(*stname).mode);
        mm_parse_addlistparam(&stlist,&stlistx,stname);
        if (((i64)(mm_decls_lx.symbol)==(i64)9) || ((i64)(mm_decls_lx.symbol)==(i64)32)) {
            if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_lx.subcode)) != (i64)30))) {
                mm_support_serror((byte*)"rdparams");
            };
            mm_lex_lex();
            (*stname).code = mm_parse_readunit();
            (*stname).equals = (u64)((i64)1);
        }else if (((i64)(mm_decls_lx.symbol)==(i64)50)) {
            mm_support_serror((byte*)"Can't mixed unnamed/named params");
        };
        if (((i64)(mm_decls_lx.symbol)==(i64)5)) {
            mm_lex_lex();
        }else if (((i64)(mm_decls_lx.symbol)==(i64)13)) {
            goto L705 ;
        } else {
            mm_support_serror((byte*)"typeparams3");
        };
    }L705 :;
    ;
    return stlist;
}

static struct mm_decls_unitrec * mm_parse_readcondsuffix(struct mm_decls_unitrec * p) {
    switch ((i64)(mm_decls_lx.symbol)) {
    case 71:;
    {
        mm_lex_lex();
        return mm_lib_createunit2((i64)196,mm_parse_readunit(),mm_lib_createunit1((i64)4,p));
    }break;
    case 67:;
    {
        mm_lex_lex();
        return mm_lib_createunit2((i64)196,mm_lib_createunit1((i64)12,mm_parse_readunit()),mm_lib_createunit1((i64)4,p));
    }break;
    default: {
        return p;
    }
    } //SW
;
}

static struct mm_decls_unitrec * mm_parse_readif(void) {
    i64 line;
    i64 kwd;
    i64 lineno;
    struct mm_decls_unitrec *  pthen;
    struct mm_decls_unitrec *  pcond;
    struct mm_decls_unitrec *  plist;
    struct mm_decls_unitrec *  plistx;
    struct mm_decls_unitrec *  pelse;
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  pelsif;
    line = (i64)(mm_decls_lx.lineno);
    kwd = (i64)(mm_decls_lx.symbol);
    mm_lex_lex();
    pcond = mm_parse_readsunit((i64)0);
    mm_parse_skipsemi();
    mm_parse_checksymbol((i64)60);
    mm_lex_lex();
    pthen = mm_parse_readsunit((i64)0);
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)61)) {
        lineno = (i64)(mm_decls_lx.lineno);
        plist = (plistx = mm_lib_createunit2((i64)186,pcond,pthen));
        L706 :;
        while (((i64)((u64)(mm_decls_lx.symbol)) == (i64)61)) {
            lineno = (i64)(mm_decls_lx.lineno);
            mm_lex_lex();
            pcond = mm_parse_readunit();
            mm_parse_checksymbol((i64)60);
            mm_lex_lex();
            pthen = mm_parse_readsunit((i64)0);
            pelsif = mm_lib_createunit2((i64)186,pcond,pthen);
            (*pelsif).lineno = lineno;
            mm_lib_addlistunit(&plist,&plistx,pelsif);
L707 :;
        }L708 :;
        ;
        if (((i64)(mm_decls_lx.symbol)==(i64)62)) {
            mm_lex_lex();
            pelse = mm_parse_readsunit((i64)0);
            mm_parse_checkend((i64)66,kwd,(i64)0,(i64)0);
            mm_lex_lex();
        }else if (((i64)(mm_decls_lx.symbol)==(i64)63) || ((i64)(mm_decls_lx.symbol)==(i64)64)) {
            mm_decls_lx.symbol = (u64)(kwd);
            pelse = mm_parse_makeblock(mm_parse_readswitchcase());
        } else {
            pelse = (struct mm_decls_unitrec *)(0);
            mm_parse_checkend((i64)66,kwd,(i64)0,(i64)0);
            mm_lex_lex();
        };
        p = mm_lib_createunit2((i64)197,plist,pelse);
        (*p).lineno = line;
        return p;
    };
    if (((i64)(mm_decls_lx.symbol)==(i64)62)) {
        mm_lex_lex();
        pelse = mm_parse_readsunit((i64)0);
        mm_parse_checkend((i64)66,kwd,(i64)0,(i64)0);
        mm_lex_lex();
    }else if (((i64)(mm_decls_lx.symbol)==(i64)63) || ((i64)(mm_decls_lx.symbol)==(i64)64)) {
        mm_decls_lx.symbol = (u64)(kwd);
        pelse = mm_parse_makeblock(mm_parse_readswitchcase());
    } else {
        pelse = (struct mm_decls_unitrec *)(0);
        mm_parse_checkend((i64)66,kwd,(i64)0,(i64)0);
        mm_lex_lex();
    };
    p = mm_lib_createunit3((i64)196,pcond,pthen,pelse);
    (*p).lineno = line;
    return p;
}

static struct mm_decls_unitrec * mm_parse_readgoto(i64 gototag) {
    struct mm_decls_unitrec *  p;
    if (((i64)((u64)(mm_decls_lx.subcode)) == (i64)1)) {
        mm_lex_lex();
        mm_parse_checksymbol((i64)74);
    };
    mm_lex_lex();
    if ((((((i64)((u64)(mm_decls_lx.symbol)) == (i64)50) && ((i64)((u64)(mm_decls_nextlx.symbol)) != (i64)18)) && ((i64)((u64)(mm_decls_nextlx.symbol)) != (i64)14)) && ((i64)((u64)(mm_decls_nextlx.symbol)) != (i64)2))) {
        p = mm_lib_createname(mm_decls_lx.symptr);
        mm_lex_lex();
    } else {
        mm_support_serror((byte*)"GOTO LABEL EXPR");
    };
    return mm_parse_readcondsuffix(mm_lib_createunit1(gototag,p));
}

static struct mm_decls_unitrec * mm_parse_readunless(void) {
    i64 line;
    struct mm_decls_unitrec *  pcond;
    struct mm_decls_unitrec *  pthen;
    struct mm_decls_unitrec *  pelse;
    struct mm_decls_unitrec *  p;
    line = (i64)(mm_decls_lx.lineno);
    mm_lex_lex();
    pcond = mm_parse_readsunit((i64)0);
    mm_parse_checksymbol((i64)60);
    mm_lex_lex();
    pthen = mm_parse_readsunit((i64)0);
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)62)) {
        mm_lex_lex();
        pelse = mm_parse_readsunit((i64)0);
    } else {
        pelse = (struct mm_decls_unitrec *)(0);
    };
    mm_parse_checkend((i64)66,(i64)67,(i64)0,(i64)0);
    mm_lex_lex();
    p = mm_lib_createunit3((i64)196,mm_lib_createunit1((i64)12,pcond),pthen,pelse);
    (*p).lineno = line;
    return p;
}

static struct mm_decls_unitrec * mm_parse_readswitchcase(void) {
    i64 line;
    i64 kwd;
    i64 opc;
    i64 lineno;
    i64 rangeused;
    i64 nwhen;
    struct mm_decls_unitrec *  pexpr;
    struct mm_decls_unitrec *  pwhenlist;
    struct mm_decls_unitrec *  pwhenlistx;
    struct mm_decls_unitrec *  pwhen;
    struct mm_decls_unitrec *  pwhenx;
    struct mm_decls_unitrec *  pelse;
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  pthen;
    struct mm_decls_unitrec *  pwhenthen;
    line = (i64)(mm_decls_lx.lineno);
    kwd = (i64)(mm_decls_lx.symbol);
    opc = (i64)(mm_decls_lx.subcode);
    mm_lex_lex();
    mm_parse_skipsemi();
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)71)) {
        if ((kwd == (i64)84)) {
            mm_support_serror((byte*)"switch expr missing");
        };
        pexpr = (struct mm_decls_unitrec *)(0);
    } else {
        pexpr = mm_parse_readsunit((i64)0);
    };
    pwhenlist = (pwhenlistx = (struct mm_decls_unitrec *)(0));
    rangeused = (i64)0;
    nwhen = (i64)0;
    mm_parse_skipsemi();
    L709 :;
    while (((i64)((u64)(mm_decls_lx.symbol)) == (i64)71)) {
        lineno = (i64)(mm_decls_lx.lineno);
        mm_lex_lex();
        pwhen = (pwhenx = (struct mm_decls_unitrec *)(0));
        L712 :;
        while (1) {
            p = mm_parse_readunit();
            ++nwhen;
            (*p).lineno = lineno;
            if (((i64)((*p).tag) == (i64)15)) {
                rangeused = (i64)1;
            };
            mm_lib_addlistunit(&pwhen,&pwhenx,p);
            if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)5)) {
                goto L713 ;
            };
            mm_lex_lex();
        }L713 :;
        ;
        mm_parse_checksymbol((i64)60);
        mm_lex_lex();
        pthen = mm_parse_readsunit((i64)0);
        pwhenthen = mm_lib_createunit2((i64)185,pwhen,pthen);
        (*pwhenthen).lineno = lineno;
        mm_lib_addlistunit(&pwhenlist,&pwhenlistx,pwhenthen);
L710 :;
    }L711 :;
    ;
    if (((i64)(mm_decls_lx.symbol)==(i64)62)) {
        mm_lex_lex();
        pelse = mm_parse_readsunit((i64)0);
        mm_parse_checkend((i64)66,kwd,(i64)0,(i64)0);
        mm_lex_lex();
    }else if (((i64)(mm_decls_lx.symbol)==(i64)61)) {
        mm_decls_lx.symbol = (u64)(kwd);
        pelse = mm_parse_makeblock(mm_parse_readif());
    }else if (((i64)(mm_decls_lx.symbol)==(i64)63) || ((i64)(mm_decls_lx.symbol)==(i64)64)) {
        mm_decls_lx.symbol = (u64)(kwd);
        pelse = mm_parse_makeblock(mm_parse_readswitchcase());
    } else {
        pelse = (struct mm_decls_unitrec *)(0);
        mm_parse_checkend((i64)66,kwd,(i64)0,(i64)0);
        mm_lex_lex();
    };
    p = mm_lib_createunit3(opc,pexpr,pwhenlist,pelse);
    (*p).lineno = line;
    return p;
}

static struct mm_decls_unitrec * mm_parse_readstop(void) {
    struct mm_decls_unitrec *  p;
    mm_lex_lex();
    if (!!((u64)(mm_decls_exprstarterset[((i64)(mm_decls_lx.symbol))]))) {
        p = mm_lib_createunit1((i64)235,mm_parse_readunit());
    } else {
        p = mm_lib_createunit0((i64)235);
    };
    return mm_parse_readcondsuffix(p);
}

static struct mm_decls_unitrec * mm_parse_readreturn(void) {
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  q;
    mm_lex_lex();
    if (!!((u64)(mm_decls_exprstarterset[((i64)(mm_decls_lx.symbol))]))) {
        q = mm_parse_readunit();
        p = mm_lib_createunit1((i64)191,q);
        (*p).length = (i64)1;
    } else {
        p = mm_lib_createunit0((i64)191);
        (*p).length = (i64)0;
    };
    return mm_parse_readcondsuffix(p);
}

static struct mm_decls_unitrec * mm_parse_readdo(void) {
    struct mm_decls_unitrec *  p;
    i64 line;
    line = (i64)(mm_decls_lx.lineno);
    mm_lex_lex();
    p = mm_parse_readsunit((i64)0);
    mm_parse_checkend((i64)66,(i64)76,(i64)0,(i64)0);
    mm_lex_lex();
    p = mm_lib_createunit1((i64)215,p);
    (*p).lineno = line;
    return p;
}

static struct mm_decls_unitrec * mm_parse_readto(void) {
    i64 line;
    i64 id;
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  pcount;
    struct mm_decls_unitrec *  pbody;
    line = (i64)(mm_decls_lx.lineno);
    mm_lex_lex();
    pcount = mm_parse_readunit();
    mm_parse_checksymbol((i64)76);
    mm_lex_lex();
    pbody = mm_parse_readsunit((i64)0);
    mm_parse_checkend((i64)66,(i64)74,(i64)76,(i64)0);
    mm_lex_lex();
    id = (i64)10;
    if (((i64)((u64)((*mm_decls_currproc).nameid)) != (i64)5)) {
        id = (i64)9;
    };
    p = mm_lib_createunit3((i64)195,pcount,pbody,mm_lib_createname(mm_lib_getavname(mm_decls_currproc,id)));
    (*p).lineno = line;
    return p;
}

static struct mm_decls_unitrec * mm_parse_readwhile(void) {
    i64 line;
    struct mm_decls_unitrec *  pcond;
    struct mm_decls_unitrec *  pbody;
    struct mm_decls_unitrec *  p;
    line = (i64)(mm_decls_lx.lineno);
    mm_lex_lex();
    pcond = mm_parse_readsunit((i64)1);
    mm_parse_checksymbol((i64)76);
    mm_lex_lex();
    pbody = mm_parse_readsunit((i64)0);
    mm_parse_checkend((i64)66,(i64)77,(i64)76,(i64)0);
    mm_lex_lex();
    p = mm_lib_createunit2((i64)206,pcond,pbody);
    (*p).lineno = line;
    return p;
}

static struct mm_decls_unitrec * mm_parse_readrepeat(void) {
    i64 line;
    struct mm_decls_unitrec *  pbody;
    struct mm_decls_unitrec *  pcond;
    struct mm_decls_unitrec *  p;
    line = (i64)(mm_decls_lx.lineno);
    mm_lex_lex();
    pbody = mm_parse_readsunit((i64)0);
    mm_parse_checksymbol((i64)79);
    mm_lex_lex();
    pcond = mm_parse_readunit();
    p = mm_lib_createunit2((i64)207,pbody,pcond);
    (*p).lineno = line;
    return p;
}

static struct mm_decls_unitrec * mm_parse_readloopcontrol(void) {
    i64 opc;
    struct mm_decls_unitrec *  p;
    opc = (i64)(mm_decls_lx.subcode);
    mm_lex_lex();
    if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)50) && !!(mlib_eqstring((*mm_decls_lx.symptr).name,(byte*)"all")))) {
        mm_lex_lex();
        p = mm_lib_createunit1(opc,mm_lib_createconstunit((u64)((i64)0),(i64)4));
    } else if (!!((u64)(mm_decls_exprstarterset[((i64)(mm_decls_lx.symbol))]))) {
        p = mm_lib_createunit1(opc,mm_parse_readconstexpr((i64)1));
    } else {
        p = mm_lib_createunit1(opc,mm_lib_createconstunit((u64)((i64)1),(i64)4));
    };
    return mm_parse_readcondsuffix(p);
}

static struct mm_decls_unitrec * mm_parse_readprint(void) {
    i64 oldinreadprint;
    i64 opc;
    i64 isfprint;
    i64 fshowname;
    struct mm_decls_unitrec *  pformat;
    struct mm_decls_unitrec *  pdev;
    struct mm_decls_unitrec *  printlist;
    struct mm_decls_unitrec *  printlistx;
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  q;
    struct mlib_strbuffer *  expr;
    byte *  s;
    oldinreadprint = mm_parse_inreadprint;
    mm_parse_inreadprint = (i64)1;
    opc = (i64)(mm_decls_lx.subcode);
    if ((opc==(i64)225) || (opc==(i64)226) || (opc==(i64)227) || (opc==(i64)228)) {
        isfprint = (i64)1;
    } else {
        isfprint = (i64)0;
    };
    mm_lex_lex();
    printlist = (printlistx = (struct mm_decls_unitrec *)(0));
    pformat = (pdev = (struct mm_decls_unitrec *)(0));
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)21)) {
        mm_lex_lex();
        pdev = mm_parse_readunit();
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
            mm_lex_lex();
        } else {
            goto L714 ;
;
        };
    };
    if (!!(isfprint)) {
        if ((!(!!((u64)(mm_decls_exprstarterset[((i64)(mm_decls_lx.symbol))]))) && (opc == (i64)228))) {
            goto L714 ;
;
        };
        pformat = mm_parse_readunit();
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
            mm_lex_lex();
        } else {
            goto L714 ;
;
        };
    };
    if (!(!!((u64)(mm_decls_exprstarterset[((i64)(mm_decls_lx.symbol))])))) {
        goto L714 ;
;
    };
    L715 :;
    while (1) {
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
            mm_lib_addlistunit(&printlist,&printlistx,mm_lib_createunit0((i64)188));
        } else {
            fshowname = (i64)0;
            if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_lx.subcode)) == (i64)30))) {
                fshowname = (i64)1;
                mm_lex_lex();
            };
            p = mm_parse_readunit();
            if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)7)) {
                mm_lex_lex();
                p = mm_lib_createunit2((i64)187,p,mm_parse_readunit());
            };
            if (!!(fshowname)) {
                expr = mm_lib_strexpr(p);
                mlib_strbuffer_add(expr,(byte*)"=",(i64)-1);
                s = (*expr).strptr;
                mlib_iconvucn((*expr).strptr,(i64)((*expr).length));
                mm_lib_addlistunit(&printlist,&printlistx,(q = mm_lib_createstringconstunit(s,(i64)((*expr).length))));
            };
            mm_lib_addlistunit(&printlist,&printlistx,p);
        };
        if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)5)) {
            goto L716 ;
        };
        mm_lex_lex();
    }L716 :;
    ;
    //finish:
L714 :;
;
    mm_parse_inreadprint = oldinreadprint;
    if (((opc == (i64)223) && (printlist == 0))) {
        mm_support_serror((byte*)"No print items");
    };
    if ((((opc == (i64)225) && (printlist == 0)) && (pformat == 0))) {
        mm_support_serror((byte*)"No print items");
    };
    if ((((opc == (i64)227) && (printlist == 0)) && (pformat == 0))) {
        mm_support_serror((byte*)"No cprint items");
    };
    if (!!(isfprint)) {
        if (((pformat == 0) && (opc != (i64)228))) {
            mm_support_serror((byte*)"No fmt str");
        };
        return mm_lib_createunit3(opc,pdev,pformat,printlist);
    } else {
        return mm_lib_createunit2(opc,pdev,printlist);
    };
}

static struct mm_decls_unitrec * mm_parse_readread(void) {
    i64 oldinreadprint;
    i64 opc;
    struct mm_decls_unitrec *  pformat;
    struct mm_decls_unitrec *  pdev;
    struct mm_decls_unitrec *  readlist;
    struct mm_decls_unitrec *  readlistx;
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  pread;
    oldinreadprint = mm_parse_inreadprint;
    mm_parse_inreadprint = (i64)1;
    opc = (i64)(mm_decls_lx.subcode);
    mm_lex_lex();
    readlist = (readlistx = (struct mm_decls_unitrec *)(0));
    pformat = (pdev = (struct mm_decls_unitrec *)(0));
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)21)) {
        if ((opc == (i64)231)) {
            mm_support_serror((byte*)"@ on read");
        };
        mm_lex_lex();
        pdev = mm_parse_readunit();
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
            mm_lex_lex();
        };
    };
    if ((opc == (i64)232)) {
        mm_lib_addlistunit(&readlist,&readlistx,mm_lib_createunit1((i64)232,pdev));
    };
    if (!(!!((u64)(mm_decls_exprstarterset[((i64)(mm_decls_lx.symbol))])))) {
        goto L717 ;
;
    };
    L718 :;
    while (1) {
        p = mm_parse_readunit();
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)7)) {
            mm_lex_lex();
            pformat = mm_parse_readunit();
        } else {
            pformat = (struct mm_decls_unitrec *)(0);
        };
        pread = mm_lib_createunit1((i64)231,pformat);
        p = mm_lib_createunit2((i64)23,p,pread);
        mm_lib_addlistunit(&readlist,&readlistx,p);
        if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)5)) {
            goto L719 ;
        };
        mm_lex_lex();
    }L719 :;
    ;
    //finish:
L717 :;
;
    mm_parse_inreadprint = oldinreadprint;
    if (((opc == (i64)231) && (readlist == 0))) {
        mm_support_serror((byte*)"No read items");
    };
    return mm_parse_makestmtblock(readlist);
}

static struct mm_decls_unitrec * mm_parse_readtry(void) {
    struct mm_decls_unitrec *  ptry;
    struct mm_decls_unitrec *  pexceptlist;
    struct mm_decls_unitrec *  pexceptlistx;
    struct mm_decls_unitrec *  px;
    struct mm_decls_unitrec *  exlist;
    struct mm_decls_unitrec *  exlistx;
    ++mm_parse_try_level;
    mm_lex_lex();
    ptry = mm_parse_readsunit((i64)0);
    pexceptlist = (pexceptlistx = (struct mm_decls_unitrec *)(0));
    L720 :;
    while (((i64)((u64)(mm_decls_lx.symbol)) == (i64)125)) {
        mm_lex_lex();
        exlist = (exlistx = (struct mm_decls_unitrec *)(0));
        L723 :;
        while (1) {
            mm_lib_addlistunit(&exlist,&exlistx,mm_parse_readconstexpr((i64)1));
            if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)5)) {
                goto L724 ;
            };
            mm_lex_lex();
        }L724 :;
        ;
        mm_parse_checksymbol((i64)60);
        mm_lex_lex();
        px = mm_parse_readsunit((i64)0);
        mm_lib_addlistunit(&pexceptlist,&pexceptlistx,mm_lib_createunit2((i64)237,exlist,px));
L721 :;
    }L722 :;
    ;
    mm_parse_checkend((i64)66,(i64)124,(i64)0,(i64)0);
    mm_lex_lex();
    --mm_parse_try_level;
    return mm_lib_createunit2((i64)236,ptry,pexceptlist);
}

static struct mm_decls_unitrec * mm_parse_readraise(void) {
    struct mm_decls_unitrec *  p;
    mm_lex_lex();
    p = mm_parse_readunit();
    return mm_lib_createunit1((i64)239,p);
}

static struct mm_decls_unitrec * mm_parse_readfor(void) {
    i64 line;
    i64 opc;
    i64 down;
    struct mm_decls_unitrec *  pstep;
    struct mm_decls_unitrec *  pvar;
    struct mm_decls_unitrec *  pcond;
    struct mm_decls_unitrec *  pfrom;
    struct mm_decls_unitrec *  pto;
    struct mm_decls_unitrec *  pelse;
    struct mm_decls_unitrec *  prange;
    struct mm_decls_unitrec *  pautovars;
    struct mm_decls_unitrec *  pbody;
    struct mm_decls_unitrec *  p;
    line = (i64)(mm_decls_lx.lineno);
    mm_lex_lex();
    pvar = mm_parse_readterm2();
    if (((i64)((*pvar).tag) != (i64)3)) {
        mm_support_serror((byte*)"For: name expected");
    };
    opc = (i64)198;
    pstep = (struct mm_decls_unitrec *)(0);
    pcond = (struct mm_decls_unitrec *)(0);
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)32)) {
        if (((i64)((u64)(mm_decls_lx.subcode)) == (i64)51)) {
            down = (i64)199;
        } else if (((i64)((u64)(mm_decls_lx.subcode)) != (i64)49)) {
            mm_support_serror((byte*)"in/inrev expected");
        };
        mm_lex_lex();
        prange = mm_parse_readunit();
        pfrom = mm_lib_getrangelwbunit(prange);
        pto = mm_lib_getrangeupbunit(prange);
    } else {
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)9)) {
            mm_lex_lex();
            pfrom = mm_parse_readunit();
        } else {
            pfrom = mm_lib_createconstunit((u64)((i64)1),(i64)4);
        };
        mm_parse_checksymbol((i64)74);
        opc = (((i64)((u64)(mm_decls_lx.subcode)) == (i64)1)?(i64)199:(i64)198);
        mm_lex_lex();
        pto = mm_parse_readunit();
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)75)) {
            if ((opc == (i64)199)) {
                mm_support_serror((byte*)"downto/by");
            };
            mm_lex_lex();
            pstep = mm_parse_readconstexpr((i64)0);
            if ((((i64)((*pstep).tag) == (i64)1) && ((*pstep).value == (i64)1))) {
                opc = (i64)198;
                pstep = (struct mm_decls_unitrec *)(0);
            } else if ((((i64)((*pstep).tag) == (i64)1) && ((*pstep).value == (i64)-1))) {
                opc = (i64)199;
                pstep = (struct mm_decls_unitrec *)(0);
            };
        } else {
            pstep = (struct mm_decls_unitrec *)(0);
        };
    };
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)71)) {
        mm_lex_lex();
        pcond = mm_parse_readunit();
    };
    mm_parse_checksymbol((i64)76);
    mm_lex_lex();
    pbody = mm_parse_readsunit((i64)0);
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)62)) {
        mm_lex_lex();
        pelse = mm_parse_readsunit((i64)0);
    } else {
        pelse = (struct mm_decls_unitrec *)(0);
    };
    mm_parse_checkend((i64)66,(i64)72,(i64)76,(i64)0);
    mm_lex_lex();
    if ((pcond != 0)) {
        pbody = mm_parse_makeblock(mm_lib_createunit2((i64)196,pcond,pbody));
    };
    pautovars = (struct mm_decls_unitrec *)(0);
    if (!((((i64)((*pto).tag) == (i64)1) || ((i64)((*pto).tag) == (i64)3)))) {
        pautovars = mm_lib_createname(mm_lib_getavname(mm_decls_currproc,(i64)10));
        if (!!(pstep)) {
            if (!((((i64)((*pstep).tag) == (i64)1) || ((i64)((*pstep).tag) == (i64)3)))) {
                (*pautovars).nextunit = mm_lib_createname(mm_lib_getavname(mm_decls_currproc,(i64)10));
            };
        };
    };
    (*pvar).nextunit = pfrom;
    (*pfrom).nextunit = pto;
    (*pto).nextunit = pstep;
    (*pbody).nextunit = pelse;
    p = mm_lib_createunit3(opc,pvar,pbody,pautovars);
    (*p).lineno = line;
    return p;
}

static struct mm_decls_unitrec * mm_parse_readforall(void) {
    i64 opc;
    i64 line;
    i64 isforall;
    struct mm_decls_unitrec *  pindex;
    struct mm_decls_unitrec *  pvar;
    struct mm_decls_unitrec *  pcond;
    struct mm_decls_unitrec *  plist;
    struct mm_decls_unitrec *  pbody;
    struct mm_decls_unitrec *  pelse;
    struct mm_decls_unitrec *  pfor;
    struct mm_decls_unitrec *  pautovar;
    line = (i64)(mm_decls_lx.lineno);
    opc = (i64)(mm_decls_lx.subcode);
    isforall = (opc == (i64)201);
    mm_lex_lex();
    pvar = mm_parse_readterm2();
    pindex = (struct mm_decls_unitrec *)(0);
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
        mm_lex_lex();
        pindex = pvar;
        pvar = mm_parse_readterm2();
    };
    if (((i64)((*pvar).tag) != (i64)3)) {
        mm_support_serror((byte*)"forall var not name");
    };
    pcond = (struct mm_decls_unitrec *)(0);
    if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && (((i64)((u64)(mm_decls_lx.subcode)) == (i64)49) || ((i64)((u64)(mm_decls_lx.subcode)) == (i64)51)))) {
        if (((i64)((u64)(mm_decls_lx.subcode)) == (i64)51)) {
            opc = ((opc == (i64)201)?(i64)202:(i64)204);
        };
        mm_lex_lex();
        plist = mm_parse_readunit();
    } else {
        mm_support_serror((byte*)"in/inrev expected");
    };
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)71)) {
        mm_lex_lex();
        pcond = mm_parse_readunit();
    };
    mm_parse_checksymbol((i64)76);
    mm_lex_lex();
    pbody = mm_parse_readsunit((i64)0);
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)62)) {
        mm_lex_lex();
        pelse = mm_parse_readsunit((i64)0);
    } else {
        pelse = (struct mm_decls_unitrec *)(0);
    };
    mm_parse_checkend((i64)66,(i64)73,(i64)76,(i64)0);
    mm_lex_lex();
    if ((pindex == 0)) {
        pindex = mm_lib_createname(mm_lib_getavname(mm_decls_currproc,(i64)10));
    };
    if ((pcond != 0)) {
        pbody = mm_lib_createunit2((i64)196,pcond,pbody);
        (*pbody).lineno = line;
    };
    pautovar = mm_lib_createname(mm_lib_getavname(mm_decls_currproc,(i64)10));
    (*pindex).nextunit = pvar;
    (*pvar).nextunit = plist;
    (*pbody).nextunit = pelse;
    pfor = mm_lib_createunit3(opc,pindex,pbody,pautovar);
    (*pfor).lineno = line;
    return pfor;
}

void mm_parse_readtypedef(struct mm_decls_strec * owner,i64 isglobal) {
    struct mm_decls_strec *  sttype;
    struct mm_decls_strec *  stname;
    i64 t;
    i64 m;
    mm_lex_lex();
    mm_parse_checksymbol((i64)50);
    stname = mm_decls_lx.symptr;
    mm_lex_lex();
    mm_parse_checkequals();
    mm_lex_lex();
    sttype = (struct mm_decls_strec *)(0);
    if ((sttype == 0)) {
        sttype = mm_lib_getduplnameptr(owner,stname,(i64)4);
        mm_lib_adddef(owner,sttype);
        m = mm_lib_createusertype(sttype);
        mm_decls_ttusercat[(m)] = (u64)((i64)1);
    } else {
        m = (i64)((*sttype).mode);
    };
    t = mm_parse_readtypespec(sttype,m);
    (*sttype).isglobal = (u64)(isglobal);
    mm_lib_storemode((i64)9,owner,t,&(*sttype).mode);
    if ((t >= (i64)0)) {
        mm_decls_ttisint[(m)] = (u64)(mm_decls_ttisint[(t)]);
        mm_decls_ttisword[(m)] = (u64)(mm_decls_ttisword[(t)]);
        mm_decls_ttiswordchar[(m)] = (u64)(mm_decls_ttiswordchar[(t)]);
        mm_decls_ttisreal[(m)] = (u64)(mm_decls_ttisreal[(t)]);
        mm_decls_ttisinteger[(m)] = (u64)(mm_decls_ttisinteger[(t)]);
        mm_decls_ttisnumeric[(m)] = ((u64)(mm_decls_ttisinteger[(t)]) | (u64)(mm_decls_ttisreal[(t)]));
        mm_decls_ttisshortint[(m)] = (u64)(mm_decls_ttisshortint[(t)]);
        mm_decls_ttisshortreal[(m)] = (u64)(mm_decls_ttisshortreal[(t)]);
        mm_decls_ttisbit[(m)] = (u64)(mm_decls_ttisbit[(t)]);
        mm_decls_ttisbit[(m)] = (u64)(mm_decls_ttisbit[(t)]);
        mm_decls_ttisref[(m)] = (u64)(mm_decls_ttisref[(t)]);
        mm_decls_tttypecode[(m)] = (u64)(mm_decls_tttypecode[(t)]);
    };
}

void mm_parse_readrecordfields(struct mm_decls_strec * owner,i64 m) {
    i64 nvars;
    struct mm_decls_strec *  stname;
    struct mm_decls_strec *  stbitfield;
    nvars = (i64)0;
    L725 :;
    while (((i64)((u64)(mm_decls_lx.symbol)) == (i64)50)) {
        stname = mm_lib_getduplnameptr(owner,mm_decls_lx.symptr,(i64)12);
        mm_lib_storemode((i64)10,owner,m,&(*stname).mode);
        ++nvars;
        if (!!((u64)(mm_parse_unionpend.ulength))) {
            mm_lib_unionstr_copy(&(*stname).uflags,&mm_parse_unionpend);
            mm_lib_unionstr_concat(&mm_parse_unionstring,&mm_parse_unionpend);
            mm_lib_unionstr_clear(&mm_parse_unionpend);
        } else {
            mm_lib_unionstr_clear(&(*stname).uflags);
        };
        mm_parse_unionlastvar = stname;
        mm_lib_adddef(owner,stname);
        mm_lex_lex();
        if (((i64)(mm_decls_lx.symbol)==(i64)21)) {
            mm_lex_lex();
            (*stname).at = (u64)((i64)2);
            (*stname).equivfield = mm_parse_readequivfield(owner);
        }else if (((i64)(mm_decls_lx.symbol)==(i64)22)) {
            mm_lex_lex();
            mm_parse_checksymbol((i64)40);
            if ((mm_decls_lx.value==(i64)1) || (mm_decls_lx.value==(i64)2) || (mm_decls_lx.value==(i64)4) || (mm_decls_lx.value==(i64)8) || (mm_decls_lx.value==(i64)16)) {
                (*stname).align = (u64)(mm_decls_lx.value);
            }else if ((mm_decls_lx.value==(i64)0)) {
                (*stname).align = (u64)((i64)255);
            } else {
                mm_support_serror((byte*)"@@ bad align");
            };
            mm_lex_lex();
        }else if (((i64)(mm_decls_lx.symbol)==(i64)7)) {
            mm_lex_lex();
            mm_parse_checksymbol((i64)12);
            L728 :;
            do {
                mm_lex_lex();
                mm_parse_checksymbol((i64)50);
                stbitfield = mm_lib_getduplnameptr(owner,mm_decls_lx.symptr,(i64)12);
                (*stbitfield).mode = (i64)25;
                mm_lib_adddef(owner,stbitfield);
                (*stbitfield).at = (u64)((i64)2);
                (*stbitfield).equivfield = stname;
                mm_lex_lex();
                mm_parse_checksymbol((i64)7);
                mm_lex_lex();
                mm_parse_checksymbol((i64)40);
                (*stbitfield).bitfieldwidth = (u64)(mm_decls_lx.value);
                mm_lex_lex();
L729 :;
            } while (!((i64)((u64)(mm_decls_lx.symbol)) != (i64)5));L730 :;
            ;
            mm_parse_checksymbol((i64)13);
            mm_lex_lex();
        };
        if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)5)) {
            goto L727 ;
        };
        mm_lex_lex();
L726 :;
    }L727 :;
    ;
    if ((nvars == (i64)0)) {
        mm_support_serror((byte*)"No fields declared");
    };
}

void mm_parse_readtabledef(i64 isglobal) {
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
    byte *  enumtypename;
    struct mm_decls_strec *  stvar;
    struct mm_decls_strec *  stenum;
    struct mm_decls_strec *  stgen;
    struct mm_decls_strec *  varnameptrs[20];
    i64 varlisttypes[20];
    struct mm_decls_unitrec *  plist[20];
    struct mm_decls_unitrec *  plistx[20];
    i64 enumvalues[500];
    mm_lex_lex();
    enums = (i64)0;
    enumtypename = (byte *)(0);
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)12)) {
        enums = (i64)1;
        mm_lex_lex();
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)50)) {
            enumtypename = (*mm_decls_lx.symptr).name;
            mm_lex_lex();
        };
        mm_parse_checksymbol((i64)13);
        mm_lex_lex();
    };
    nextenumvalue = (i64)1;
    nrows = (i64)0;
    ncols = (i64)0;
    L731 :;
    while (((i64)((u64)(mm_decls_lx.symbol)) != (i64)32)) {
        ltype = mm_parse_readtypespec(mm_decls_currproc,(i64)0);
        mm_parse_checksymbol((i64)50);
        if ((++ncols > (i64)20)) {
            mm_support_serror((byte*)"tabledata/too many columns");
        };
        varnameptrs[(ncols)-1] = mm_decls_lx.symptr;
        varlisttypes[(ncols)-1] = ltype;
        mm_lex_lex();
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
            mm_lex_lex();
        } else {
            goto L733 ;
        };
L732 :;
    }L733 :;
    ;
    mm_parse_checkequals();
    mm_lex_lex();
    mm_parse_skipsemi();
    startline = mm_parse_getcurrline();
    closesym = mm_parse_checkbegin((i64)0);
    mm_parse_skipsemi();
    firstval = (lastval = (i64)0);
    L734 :;
    for (i=(i64)1;i<=ncols;++i) {
L735 :;
        plist[(i)-1] = (plistx[(i)-1] = (struct mm_decls_unitrec *)(0));
L736 :;
    }L737 :;
    ;
    mm_parse_intabledata = (i64)1;
    L738 :;
    while (1) {
        mm_parse_skipsemi();
        mm_parse_checksymbol((i64)12);
        mm_lex_lex();
        if ((++nrows > (i64)500)) {
            mm_support_serror((byte*)"tabledata:too many rows");
        };
        if (!!(enums)) {
            mm_parse_checksymbol((i64)50);
            stgen = mm_decls_lx.symptr;
            mm_parse_tabledataname = (*stgen).name;
            mm_lex_lex();
            if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_lx.subcode)) == (i64)30))) {
                mm_lex_lex();
                nextenumvalue = mm_parse_readconstint();
            };
            enumvalues[(nrows)-1] = nextenumvalue;
            stenum = mm_lib_getduplnameptr(mm_decls_currproc,stgen,(i64)8);
            mm_lib_storemode((i64)11,mm_decls_currproc,(i64)4,&(*stenum).mode);
            (*stenum).code = mm_lib_createconstunit((u64)(nextenumvalue),(i64)4);
            (*stenum).isglobal = (u64)(isglobal);
            mm_lib_adddef(mm_decls_currproc,stenum);
            if ((nrows == (i64)1)) {
                firstval = nextenumvalue;
            };
            lastval = nextenumvalue;
            ++nextenumvalue;
            if (!!(ncols)) {
                mm_parse_checksymbol((i64)5);
            };
            mm_lex_lex();
        };
        L740 :;
        for (i=(i64)1;i<=ncols;++i) {
L741 :;
            mm_lib_addlistunit(&plist[(i)-1],&plistx[(i)-1],mm_parse_readunit());
            if ((i == ncols)) {
                mm_parse_checksymbol((i64)13);
            } else {
                mm_parse_checksymbol((i64)5);
            };
            mm_lex_lex();
L742 :;
        }L743 :;
        ;
        if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)5)) {
            goto L739 ;
        };
        mm_lex_lex();
        if (((i64)((u64)(mm_decls_lx.symbol)) == closesym)) {
            goto L739 ;
        };
    }L739 :;
    ;
    mm_parse_intabledata = (i64)0;
    mm_parse_skipsemi();
    mm_parse_checkbeginend(closesym,(i64)136,startline);
    if ((nrows == (i64)0)) {
        mm_support_serror((byte*)"No table data");
    };
    L744 :;
    for (i=(i64)1;i<=ncols;++i) {
L745 :;
        stvar = mm_lib_getduplnameptr(mm_decls_currproc,varnameptrs[(i)-1],(i64)9);
        (*stvar).code = mm_lib_createunit1((i64)14,plist[(i)-1]);
        (*(*stvar).code).length = nrows;
        mm_lib_storemode((i64)12,mm_decls_currproc,varlisttypes[(i)-1],&(*stvar).mode);
        (*stvar).isglobal = (u64)(isglobal);
        mm_lib_adddef(mm_decls_currproc,stvar);
        mm_lib_addstatic(stvar);
L746 :;
    }L747 :;
    ;
}

void mm_parse_readclassdef(struct mm_decls_strec * owner,i64 isglobal) {
    i64 kwd;
    i64 baseclass;
    i64 m;
    i64 startline;
    i64 closesym;
    i64 mrec;
    i64 normalexit;
    i64 isrecord;
    struct mm_decls_strec *  nameptr;
    struct mm_decls_strec *  sttype;
    struct mm_decls_strec *  newd;
    struct mm_decls_strec *  d;
    struct mm_decls_strec *  e;
    kwd = (i64)(mm_decls_lx.symbol);
    isrecord = (kwd == (i64)94);
    mm_lex_lex();
    mm_parse_checksymbol((i64)50);
    nameptr = mm_decls_lx.symptr;
    mm_lex_lex();
    baseclass = (i64)0;
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)12)) {
        mm_lex_lex();
        baseclass = mm_parse_readtypespec(owner,(i64)0);
        mm_parse_checksymbol((i64)13);
        mm_lex_lex();
    };
    mm_parse_checkequals();
    mm_lex_lex();
    sttype = mm_lib_getduplnameptr(owner,nameptr,(i64)4);
    mm_lib_adddef(owner,sttype);
    m = mm_lib_createusertype(sttype);
    mrec = mm_lib_createrecordmode(owner,(!!(isrecord)?(i64)32:(i64)33),m);
    mm_lib_storemode((i64)13,owner,mrec,&(*sttype).mode);
    (*sttype).base_class = baseclass;
    closesym = mm_parse_checkbegin((i64)1);
    startline = mm_parse_getcurrline();
    mm_parse_readclassbody(sttype,kwd);
    mm_parse_checkbeginend(closesym,kwd,startline);
    if (!!(baseclass)) {
        d = (*mm_decls_ttnamedef[(baseclass)]).deflist;
        L748 :;
        while (!!(d)) {
            e = (*sttype).deflist;
            normalexit = (i64)1;
            L751 :;
            while (!!(e)) {
                if (!!(mlib_eqstring((*d).name,(*e).name))) {
                    normalexit = (i64)0;
                    goto L753 ;
                };
                e = (*e).nextdef;
L752 :;
            }L753 :;
            ;
            if (!!(normalexit)) {
                if (((i64)((*d).nameid)==(i64)5) || ((i64)((*d).nameid)==(i64)20)) {
                    newd = mm_lib_getduplnameptr(sttype,d,(i64)20);
                    (*newd).equivfield = d;
                } else {
                    newd = mm_lib_getduplnameptr(sttype,d,(i64)((*d).nameid));
                    mm_parse_duplfield(owner,d,newd);
                };
                mm_lib_adddef(sttype,newd);
            };
            d = (*d).nextdef;
L749 :;
        }L750 :;
        ;
    };
    (*sttype).isglobal = (u64)(isglobal);
}

static void mm_parse_readclassbody(struct mm_decls_strec * owner,i64 classkwd) {
    i64 kwd;
    i64 t;
    mm_lib_unionstr_clear(&mm_parse_unionstring);
    mm_lib_unionstr_clear(&mm_parse_unionpend);
    L754 :;
    switch ((i64)(mm_decls_lx.symbol)) {
    case 113:;
    {
        mm_parse_readconstdef(owner,(i64)0);
    }break;
    case 92:;
    case 91:;
    {
        kwd = (i64)(mm_decls_lx.symbol);
        if (!!((u64)((*owner).imported))) {
            mm_parse_readprocdecl(owner,(i64)0,(i64)0);
        } else {
            mm_parse_readprocdef(owner,(i64)0,(i64)0);
        };
    }break;
    case 117:;
    {
        mm_lex_lex();
        mm_support_serror((byte*)"CLASS CLASS");
    }break;
    case 94:;
    {
        mm_lex_lex();
        mm_support_serror((byte*)"CLASS RECORD");
    }break;
    case 102:;
    {
        mm_lex_lex();
        mm_support_serror((byte*)"CLASS TYPE");
    }break;
    case 36:;
    {
        mm_support_serror((byte*)"Class eof?");
        goto L755 ;
    }break;
    case 6:;
    {
        mm_lex_lex();
    }break;
    case 50:;
    {
        ++mm_parse_insiderecord;
        t = mm_lib_newusertypex(mm_decls_lx.symptr,(struct mm_decls_strec *)(0));
        --mm_parse_insiderecord;
        mm_lex_lex();
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)2)) {
            mm_support_serror((byte*)"Can't do a.b type inside class");
        };
        mm_parse_readrecordfields(owner,t);
    }break;
    case 95:;
    case 96:;
    {
        mm_lib_unionstr_append(&mm_parse_unionpend,(((i64)((u64)(mm_decls_lx.symbol)) == (i64)95)?(i64)83:(i64)85));
        mm_parse_unionlastvar = (struct mm_decls_strec *)(0);
        mm_lex_lex();
    }break;
    case 66:;
    case 13:;
    case 17:;
    {
        if (!!((u64)(mm_parse_unionstring.ulength))) {
            mm_parse_checkend((i64)66,((mm_lib_unionstr_last(&mm_parse_unionstring) == (i64)83)?(i64)95:(i64)96),(i64)0,(i64)0);
            mm_lex_lex();
            if (((mm_parse_unionlastvar == 0) || !!((u64)(mm_parse_unionpend.ulength)))) {
                mm_support_serror((byte*)"Empty union group");
            };
            if ((mm_lib_unionstr_last(&(*mm_parse_unionlastvar).uflags)==(i64)69) || (mm_lib_unionstr_last(&(*mm_parse_unionlastvar).uflags)==(i64)42)) {
            } else {
                mm_lib_unionstr_append(&(*mm_parse_unionlastvar).uflags,(i64)42);
            };
            mm_lib_unionstr_append(&(*mm_parse_unionlastvar).uflags,(i64)69);
            --mm_parse_unionstring.ulength;
        } else {
            goto L755 ;
        };
    }break;
    case 105:;
    {
        mm_lex_lex();
        if (!(!!(mm_parse_istypestarter()))) {
            mm_support_serror((byte*)"need type");
        };
        ++mm_parse_insiderecord;
        t = mm_parse_readtypespec(owner,(i64)0);
        --mm_parse_insiderecord;
        mm_parse_readrecordfields(owner,t);
    }break;
    case 107:;
    {
        mm_lex_lex();
        mm_parse_readrecordfields(owner,(i64)50);
    }break;
    case 106:;
    {
        mm_support_serror((byte*)"Let not allowed");
    }break;
    default: {
        if (!!(mm_parse_istypestarter())) {
            mm_support_serror((byte*)"record:need var");
        } else {
            goto L755 ;
        };
    }
    } //SW
goto L754 ;
L755 :;
    ;
}

static i64 mm_parse_readenumtype(struct mm_decls_strec * owner,i64 typedefx,i64 isglobal) {
    struct mm_decls_strec *  enumowner;
    struct mm_decls_strec *  stname;
    struct mm_decls_strec *  nameptr;
    i64 isanon;
    i64 index;
    i64 startline;
    i64 closesym;
    struct mm_decls_unitrec *  pone;
    struct mm_decls_unitrec *  pindex;
    enumowner = owner;
    isanon = (i64)0;
    if (!(!!(typedefx))) {
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)50)) {
            stname = mm_lib_getduplnameptr(owner,mm_decls_lx.symptr,(i64)4);
            owner = stname;
            mm_lex_lex();
            mm_parse_checkequals();
            mm_lex_lex();
            mm_lib_adddef(enumowner,owner);
        } else {
            isanon = (i64)1;
        };
        mm_parse_checksymbol((i64)12);
        mm_lex_lex();
    } else {
        owner = mm_decls_ttnamedef[(typedefx)];
        startline = mm_parse_getcurrline();
        closesym = mm_parse_checkbegin((i64)1);
    };
    pone = mm_lib_createconstunit((u64)((i64)1),(i64)4);
    pindex = pone;
    index = (i64)1;
    L756 :;
    while (((i64)((u64)(mm_decls_lx.symbol)) == (i64)50)) {
        nameptr = mm_decls_lx.symptr;
        mm_lex_lex();
        if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_lx.subcode)) == (i64)30))) {
            mm_lex_lex();
            pindex = mm_parse_readunit();
        };
        if (!(!!(isanon))) {
            stname = mm_lib_getduplnameptr(owner,nameptr,(i64)14);
            (*stname).code = pindex;
            mm_lib_storemode((i64)14,owner,(i64)4,&(*stname).mode);
            mm_lib_adddef(owner,stname);
        } else {
            stname = mm_lib_getduplnameptr(enumowner,nameptr,(i64)8);
            (*stname).code = pindex;
            mm_lib_storemode((i64)15,owner,(i64)4,&(*stname).mode);
            mm_lib_adddef(enumowner,stname);
        };
        pindex = mm_lib_createunit2((i64)37,pindex,pone);
        (*stname).isglobal = (u64)(isglobal);
        if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)5)) {
            goto L758 ;
        };
        mm_lex_lex();
L757 :;
    }L758 :;
    ;
    if (!(!!(typedefx))) {
        mm_parse_checksymbol((i64)13);
        mm_lex_lex();
    } else {
        mm_parse_checkbeginend(closesym,(i64)115,startline);
    };
    if (!(!!(isanon))) {
        return mm_lib_createenummode(owner,typedefx);
    } else {
        return (i64)0;
    };
}

static void mm_parse_duplfield(struct mm_decls_strec * owner,struct mm_decls_strec * p,struct mm_decls_strec * q) {
    if (!!((*p).code)) {
        mm_support_serror((byte*)"DUPLFIELD");
    };
    (*q).at = (u64)((*p).at);
    (*q).uflags = (*p).uflags;
    mm_lib_storemode((i64)16,owner,(i64)((*p).mode),&(*q).mode);
}

static void mm_parse_readimportmodule(struct mm_decls_strec * owner) {
    i64 isnew;
    i64 startline;
    i64 closesym;
    struct mm_decls_strec *  d;
    struct mm_decls_strec *  stname;
    if (!!(mm_parse_insidedllimport)) {
        mm_support_serror((byte*)"nested importdll");
    };
    mm_lex_lex();
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)45)) {
        stname = mm_lex_addnamestr(mm_decls_lx.svalue);
    } else {
        mm_parse_checksymbol((i64)50);
        stname = mm_decls_lx.symptr;
    };
    mm_lex_lex();
    mm_parse_checkequals();
    mm_lex_lex();
    isnew = (i64)1;
    d = (*stname).nextdupl;
    L759 :;
    while (!!(d)) {
        if (((i64)((u64)((*d).nameid)) == (i64)3)) {
            stname = d;
            isnew = (i64)0;
            goto L761 ;
        };
        d = (*d).nextdupl;
L760 :;
    }L761 :;
    ;
    if (!!(isnew)) {
        stname = mm_lib_getduplnameptr(mm_decls_stmodule,stname,(i64)3);
        if (!!(mlib_eqstring((*stname).name,(byte*)"sys"))) {
            mm_decls_stsysmodule = stname;
        };
        mm_lib_adddef(mm_decls_stmodule,stname);
        if ((mm_decls_ndlltable >= (i64)50)) {
            mm_support_serror((byte*)"Too many DLL libs");
        };
        mm_decls_dlltable[(++mm_decls_ndlltable)-1] = (*stname).name;
        (*stname).dllindex = (u64)(mm_decls_ndlltable);
    };
    startline = mm_parse_getcurrline();
    closesym = mm_parse_checkbegin((i64)0);
    mm_parse_insidedllimport = (i64)1;
    mm_parse_readimportbody(owner);
    mm_parse_insidedllimport = (i64)0;
    mm_parse_checkbeginend(closesym,(i64)98,startline);
}

static void mm_parse_readimportbody(struct mm_decls_strec * owner) {
    i64 lineno;
    i64 fflang;
    lineno = (i64)(mm_decls_lx.lineno);
    L762 :;
    while (1) {
        mm_parse_skipsemi();
        switch ((i64)(mm_decls_lx.symbol)) {
        case 121:;
        {
            fflang = (i64)(mm_decls_lx.subcode);
            mm_lex_lex();
            if (((i64)(mm_decls_lx.symbol)==(i64)91) || ((i64)(mm_decls_lx.symbol)==(i64)92)) {
                mm_parse_readprocdecl(owner,(i64)0,fflang);
            };
        }break;
        case 91:;
        case 92:;
        {
            mm_parse_readprocdecl(owner,(i64)0,(i64)0);
        }break;
        case 102:;
        {
            mm_parse_readtypedef(owner,(i64)0);
        }break;
        case 113:;
        {
            mm_parse_readconstdef(owner,(i64)1);
        }break;
        case 117:;
        case 94:;
        {
            mm_parse_readclassdef(owner,(i64)0);
        }break;
        case 105:;
        {
            mm_parse_readvardef(owner,(i64)1,(i64)0,(i64)7);
        }break;
        case 36:;
        {
            goto L763 ;
        }break;
        case 66:;
        {
            goto L763 ;
        }break;
        default: {
            mm_lex_ps1((byte*)"symbol");
            mm_support_serror((byte*)"Not allowed in importmodule");
        }
        } //SW
;
    }L763 :;
    ;
}

static struct mm_decls_strec * mm_parse_readequivfield(struct mm_decls_strec * owner) {
    struct mm_decls_strec *  p;
    struct mm_decls_strec *  d;
    mm_parse_checksymbol((i64)50);
    d = mm_decls_lx.symptr;
    mm_lex_lex();
    p = (*owner).deflist;
    L764 :;
    while (!!(p)) {
        if (!!(mlib_eqstring((*p).name,(*d).name))) {
            return p;
        };
        p = (*p).nextdef;
L765 :;
    }L766 :;
    ;
    msysc_m_print_startcon();
    msysc_m_print_str((*d).name,NULL);
    msysc_m_print_newline();
    msysc_m_print_end();
    ;
    mm_support_serror((byte*)"Can't find @ field");
    return (struct mm_decls_strec *)(0);
}

static struct mm_decls_unitrec * mm_parse_readapplyop(i64 inexpr) {
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  a;
    struct mm_decls_unitrec *  b;
    mm_lex_lex();
    mm_parse_checksymbol((i64)12);
    mm_lex_lex();
    p = mm_parse_readunit();
    mm_parse_checksymbol((i64)5);
    mm_lex_lex();
    a = mm_parse_readunit();
    b = (struct mm_decls_unitrec *)(0);
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
        mm_lex_lex();
        b = mm_parse_readunit();
    };
    mm_parse_checksymbol((i64)13);
    mm_lex_lex();
    return mm_lib_createunit3((!!(inexpr)?(i64)28:(i64)27),p,a,b);
}

static i64 mm_parse_readrefproc(struct mm_decls_strec * owner,i64 typedefx,i64 fflang) {
    i64 kwd;
    i64 prettype;
    i64 m;
    i64 varparams;
    i64 nparams;
    i64 retmodes[4];
    struct mm_decls_strec *  paramlist;
    struct mm_decls_strec *  stproc;
    i64 nretvalues;
    byte *  name;
    i64 i;
    kwd = (i64)(mm_decls_lx.symbol);
    mm_lex_lex();
    paramlist = (struct mm_decls_strec *)(0);
    prettype = (i64)0;
    nretvalues = (i64)0;
    name = mm_lib_nextautotype();
    stproc = mm_lib_getduplnameptr(mm_decls_stmodule,mm_lex_addnamestr(name),(i64)4);
    mm_lib_adddef(mm_decls_stmodule,stproc);
    if ((kwd == (i64)92)) {
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)12)) {
            mm_lex_lex();
            if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)13)) {
                paramlist = mm_parse_readparams(owner,stproc,(i64)0,&varparams,&nparams);
                mm_parse_checksymbol((i64)13);
            };
            mm_lex_lex();
            if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)7) || ((i64)((u64)(mm_decls_lx.symbol)) == (i64)11))) {
                mm_lex_lex();
                nretvalues = mm_parse_readreturntype(stproc,&retmodes);
            } else if ((!!((u64)(mm_decls_typestarterset[((i64)(mm_decls_lx.symbol))])) || ((i64)((u64)(mm_decls_lx.symbol)) == (i64)50))) {
                nretvalues = mm_parse_readreturntype(stproc,&retmodes);
            };
        } else if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)7) || ((i64)((u64)(mm_decls_lx.symbol)) == (i64)11))) {
            mm_lex_lex();
            nretvalues = mm_parse_readreturntype(stproc,&retmodes);
        };
        if ((nretvalues == (i64)0)) {
            mm_support_serror((byte*)"Function needs return type");
        };
        if ((!!(nretvalues) && (kwd == (i64)91))) {
            mm_support_serror((byte*)"Proc can't return value");
        };
    } else {
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)12)) {
            mm_lex_lex();
            if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)13)) {
                paramlist = mm_parse_readparams(owner,stproc,(i64)0,&varparams,&nparams);
                mm_parse_checksymbol((i64)13);
            };
            mm_lex_lex();
        };
        if (((!!((u64)(mm_decls_typestarterset[((i64)(mm_decls_lx.symbol))])) || ((i64)((u64)(mm_decls_lx.symbol)) == (i64)7)) || ((i64)((u64)(mm_decls_lx.symbol)) == (i64)11))) {
            mm_support_serror((byte*)"proc can't have ret value");
        };
    };
    m = mm_lib_createrefprocmode(owner,stproc,paramlist,kwd,prettype,typedefx);
    L767 :;
    for (i=(i64)1;i<=nretvalues;++i) {
L768 :;
        mm_lib_storemode((i64)17,owner,retmodes[(i)-1],&(*stproc).modelist[(i)-1]);
L769 :;
    }L770 :;
    ;
    (*stproc).nretvalues = (u64)(nretvalues);
    mm_decls_ttnamedef[(m)] = stproc;
    (*stproc).fflang = (u64)(fflang);
    return m;
}

static void mm_parse_pushproc(struct mm_decls_strec * p) {
    if ((mm_parse_nprocstack >= (i64)10)) {
        mm_support_serror((byte*)"Too many nested proc");
    };
    mm_parse_procstack[(++mm_parse_nprocstack)-1] = mm_decls_currproc;
    mm_decls_currproc = p;
}

static void mm_parse_popproc(void) {
    if (!!(mm_parse_nprocstack)) {
        mm_decls_currproc = mm_parse_procstack[(mm_parse_nprocstack--)-1];
    } else {
        mm_decls_currproc = mm_decls_stmodule;
    };
}

static struct mm_decls_unitrec * mm_parse_readassemline(void) {
    mm_lex_lex();
    return mm_parse_assembleline();
}

static struct mm_decls_unitrec * mm_parse_readassemblock(void) {
    struct mm_decls_unitrec *  ulist;
    struct mm_decls_unitrec *  ulistx;
    struct mm_decls_unitrec *  u;
    ulist = (ulistx = (struct mm_decls_unitrec *)(0));
    L771 :;
    while (1) {
        mm_lex_lex();
        if (((i64)(mm_decls_lx.symbol)==(i64)36)) {
            mm_support_serror((byte*)"EOF: 'End' missing in Assembler code");
        }else if (((i64)(mm_decls_lx.symbol)==(i64)66)) {
            mm_parse_checkend((i64)(mm_decls_lx.symbol),(i64)144,(i64)0,(i64)0);
            mm_lex_lex();
            goto L772 ;
        }else if (((i64)(mm_decls_lx.symbol)==(i64)6)) {
        } else {
            u = mm_parse_assembleline();
            mm_lib_addlistunit(&ulist,&ulistx,u);
        };
    }L772 :;
    ;
    return mm_parse_makeblock(ulist);
}

static struct mm_decls_unitrec * mm_parse_assembleline(void) {
    struct mm_decls_unitrec *  dlist;
    struct mm_decls_unitrec *  dlistx;
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  pname;
    byte deststr[512];
    byte *  pdest;
    dlist = (dlistx = (struct mm_decls_unitrec *)(0));
    pdest = &deststr[((i64)1)-1];
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)31)) {
        mm_lex_lex();
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)40)) {
            mm_lib_addstr(&pdest,(*mm_decls_currproc).name);
            mm_lib_addchar(&pdest,'.');
            mm_lib_addint(&pdest,mm_decls_lx.value);
            mm_lib_addstr(&pdest,(byte*)":\t");
        } else if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)50)) {
            mm_lib_addstr(&pdest,(*mm_decls_currproc).name);
            mm_lib_addchar(&pdest,'.');
            mm_lib_addstr(&pdest,(*mm_decls_lx.symptr).name);
            mm_lib_addstr(&pdest,(byte*)":\t");
        } else {
            mm_support_serror((byte*)"asm #");
        };
        mm_lex_lex();
        mm_parse_checksymbol((i64)7);
        mm_lex_lex();
    } else if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)50) && ((i64)((u64)(mm_decls_nextlx.symbol)) == (i64)7))) {
        mm_lib_addstr(&pdest,(*mm_decls_currproc).name);
        mm_lib_addchar(&pdest,'.');
        mm_lib_addstr(&pdest,(*mm_decls_lx.symptr).name);
        mm_lib_addstr(&pdest,(byte*)":\t");
        mm_lex_lex();
        mm_lex_lex();
    } else if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)21)) {
        mm_lex_lex();
        mm_parse_checksymbol((i64)50);
        mm_lib_addstr(&pdest,(*mm_decls_lx.symptr).name);
        mm_lib_addchar(&pdest,':');
        mm_lex_lex();
        mm_parse_checksymbol((i64)7);
        mm_lex_lex();
    } else if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)32) && ((i64)((u64)(mm_decls_lx.subcode)) == (i64)39))) {
        mm_lex_lex();
        mm_parse_checksymbol((i64)50);
        pname = mm_lib_createname(mm_decls_lx.symptr);
        (*pname).lineno = (i64)(mm_decls_lx.lineno);
        mm_lex_lex();
        if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)6)) {
            L773 :;
            do {
                mm_lib_addlistunit(&dlist,&dlistx,mm_parse_readunit());
                if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
                    mm_lex_lex();
                };
L774 :;
            } while (!((mm_decls_lx.symbol == (i64)6) || (mm_decls_lx.symbol == (i64)36)));L775 :;
            ;
        };
        return mm_lib_createunit2((i64)8,pname,dlist);
    };
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)32)) {
        if (((i64)(mm_decls_lx.subcode)==(i64)47)) {
            mm_lib_addstr(&pdest,(byte*)"\tshl ");
        }else if (((i64)(mm_decls_lx.subcode)==(i64)48)) {
            mm_lib_addstr(&pdest,(byte*)"\tshr ");
        }else if (((i64)(mm_decls_lx.subcode)==(i64)9)) {
            mm_lib_addstr(&pdest,(byte*)"\tand\t");
        }else if (((i64)(mm_decls_lx.subcode)==(i64)10)) {
            mm_lib_addstr(&pdest,(byte*)"\tor\t");
        }else if (((i64)(mm_decls_lx.subcode)==(i64)12)) {
            mm_lib_addstr(&pdest,(byte*)"\tnot\t");
        }else if (((i64)(mm_decls_lx.subcode)==(i64)11)) {
            mm_lib_addstr(&pdest,(byte*)"\txor\t");
        } else {
            mm_support_serror_s((byte*)"Asm op?? #",mm_tables_jtagnames[((i64)(mm_decls_lx.subcode))]);
        };
        mm_lex_lex();
    } else if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)50)) {
        mm_lib_addchar(&pdest,(u64)9u);
        mm_lib_addstr(&pdest,(*mm_decls_lx.symptr).name);
        mm_lib_addchar(&pdest,(u64)9u);
        mm_lex_lex();
    };
    L776 :;
    while ((((i64)((u64)(mm_decls_lx.symbol)) != (i64)6) && ((i64)((u64)(mm_decls_lx.symbol)) != (i64)36))) {
        switch ((i64)(mm_decls_lx.symbol)) {
        case 50:;
        {
            if (((i64)((*mm_decls_lx.symptr).subcode) == (i64)52)) {
                mm_lib_addstr(&pdest,(*mm_decls_lx.symptr).name);
            } else {
                mm_lib_addchar(&pdest,'#');
                pname = mm_lib_createunit0((i64)3);
                (*pname).def = mm_decls_lx.symptr;
                mm_lib_addlistunit(&dlist,&dlistx,pname);
            };
        }break;
        case 31:;
        {
            mm_lex_lex();
            if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)40)) {
                mm_lib_addstr(&pdest,(*mm_decls_currproc).name);
                mm_lib_addchar(&pdest,'.');
                mm_lib_addint(&pdest,mm_decls_lx.value);
            } else if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)50)) {
                mm_lib_addstr(&pdest,(*mm_decls_currproc).name);
                mm_lib_addchar(&pdest,'.');
                mm_lib_addstr(&pdest,(*mm_decls_lx.symptr).name);
            } else {
                mm_support_serror((byte*)"#label?");
            };
        }break;
        case 21:;
        {
            mm_lex_lex();
            mm_parse_checksymbol((i64)50);
            mm_lib_addstr(&pdest,(*mm_decls_lx.symptr).name);
        }break;
        case 5:;
        {
            mm_lib_addchar(&pdest,',');
        }break;
        case 14:;
        {
            mm_lib_addchar(&pdest,'[');
        }break;
        case 15:;
        {
            mm_lib_addchar(&pdest,']');
        }break;
        case 12:;
        {
            mm_lib_addchar(&pdest,'(');
        }break;
        case 13:;
        {
            mm_lib_addchar(&pdest,')');
        }break;
        case 32:;
        {
            if (((i64)(mm_decls_lx.subcode)==(i64)37)) {
                mm_lib_addchar(&pdest,'+');
            }else if (((i64)(mm_decls_lx.subcode)==(i64)38)) {
                mm_lib_addchar(&pdest,'-');
            }else if (((i64)(mm_decls_lx.subcode)==(i64)39)) {
                mm_lib_addchar(&pdest,'*');
            }else if (((i64)(mm_decls_lx.subcode)==(i64)40)) {
                mm_lib_addchar(&pdest,'/');
            }else if (((i64)(mm_decls_lx.subcode)==(i64)9)) {
                mm_lib_addstr(&pdest,(byte*)"and");
            }else if (((i64)(mm_decls_lx.subcode)==(i64)10)) {
                mm_lib_addstr(&pdest,(byte*)"or");
            }else if (((i64)(mm_decls_lx.subcode)==(i64)12)) {
                mm_lib_addstr(&pdest,(byte*)"not");
            }else if (((i64)(mm_decls_lx.subcode)==(i64)11)) {
                mm_lib_addstr(&pdest,(byte*)"xor");
            } else {
                mm_support_serror_s((byte*)"1:Unknown asm op: #",mm_tables_jtagnames[((i64)(mm_decls_lx.subcode))]);
            };
        }break;
        case 7:;
        {
            mm_lib_addchar(&pdest,':');
        }break;
        case 40:;
        {
            mm_lib_addint(&pdest,mm_decls_lx.value);
        }break;
        case 43:;
        {
            mm_lib_addchar(&pdest,(u64)39u);
            mm_lib_addchar(&pdest,(u64)((*mm_decls_lx.svalue)));
            mm_lib_addchar(&pdest,(u64)39u);
        }break;
        case 42:;
        {
            mm_lib_addstr(&pdest,msysc_strreal(mm_decls_lx.xvalue,(byte *)(0)));
        }break;
        case 45:;
        {
            mm_lib_addchar(&pdest,'"');
            mm_lib_addstr(&pdest,mm_decls_lx.svalue);
            mm_lib_addchar(&pdest,'"');
        }break;
        case 53:;
        {
            if (((i64)(mm_decls_lx.subcode)==(i64)6)) {
                mm_lib_addstr(&pdest,(byte*)"byte ");
            }else if (((i64)(mm_decls_lx.subcode)==(i64)7)) {
                mm_lib_addstr(&pdest,(byte*)"word16 ");
            }else if (((i64)(mm_decls_lx.subcode)==(i64)8)) {
                mm_lib_addstr(&pdest,(byte*)"word32 ");
            }else if (((i64)(mm_decls_lx.subcode)==(i64)9)) {
                mm_lib_addstr(&pdest,(byte*)"word64 ");
            }else if (((i64)(mm_decls_lx.subcode)==(i64)10)) {
                mm_lib_addstr(&pdest,(byte*)"word128 ");
            } else {
                mm_support_serror((byte*)"<asm type>");
            };
        }break;
        case 82:;
        {
            mm_lib_addstr(&pdest,(byte*)"exit");
        }break;
        default: {
            mm_lex_ps((byte*)"ASM");
            mm_support_serror((byte*)"2:Unknown asm symbol");
        }
        } //SW
;
        mm_lex_lex();
L777 :;
    }L778 :;
    ;
    (*pdest) = (u64)0u;
    pname = mm_lib_createstringconstunit(mlib_pcm_copyheapstring(deststr),((pdest + (i64)1) - deststr));
    p = mm_lib_createunit2((i64)7,pname,dlist);
    return p;
}

static struct mm_decls_unitrec * mm_parse_makeastring(void) {
    struct mm_decls_unitrec *  ulist;
    struct mm_decls_unitrec *  ulistx;
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  pconst;
    byte *  s;
    i64 length;
    i64 av_1;
    ulist = (ulistx = (struct mm_decls_unitrec *)(0));
    s = mm_decls_lx.svalue;
    length = (i64)(mm_decls_lx.length);
    av_1 = length;
    while (av_1-- > 0) {
L779 :;
        pconst = mm_lib_createconstunit((u64)((*s)),(i64)4);
        mm_lib_addlistunit(&ulist,&ulistx,pconst);
        ++s;
L780 :;
    }L781 :;
    ;
    if (((u64)(mm_decls_lx.subcode) == (u64)90u)) {
        pconst = mm_lib_createconstunit((u64)((i64)0),(i64)4);
        mm_lib_addlistunit(&ulist,&ulistx,pconst);
        ++length;
    };
    p = mm_lib_createunit1((i64)14,ulist);
    (*p).length = length;
    return p;
}

static i64 mm_parse_readreturntype(struct mm_decls_strec * owner,i64 (*retmodes)[]) {
    i64 nretvalues;
    (*retmodes)[((i64)1)-1] = mm_parse_readtypespec(owner,(i64)0);
    nretvalues = (i64)1;
    L782 :;
    while (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
        if ((nretvalues >= (i64)4)) {
            mm_support_serror((byte*)"Too many return values");
        };
        mm_lex_lex();
        (*retmodes)[(++nretvalues)-1] = mm_parse_readtypespec(owner,(i64)0);
L783 :;
    }L784 :;
    ;
    return nretvalues;
}

static struct mm_decls_unitrec * mm_parse_readset(void) {
    i64 length;
    i64 nkeyvalues;
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  ulist;
    struct mm_decls_unitrec *  ulistx;
    mm_lex_lex();
    if (((i64)(mm_decls_lx.symbol)==(i64)15)) {
        mm_lex_lex();
        return mm_lib_createunit1((i64)16,(struct mm_decls_unitrec *)(0));
    }else if (((i64)(mm_decls_lx.symbol)==(i64)7)) {
        mm_lex_lex();
        mm_parse_checksymbol((i64)15);
        mm_lex_lex();
        return mm_lib_createunit1((i64)17,(struct mm_decls_unitrec *)(0));
    };
    p = mm_parse_readunit();
    length = (i64)1;
    nkeyvalues = (i64)0;
    if (((i64)((*p).tag) == (i64)22)) {
        ++nkeyvalues;
    };
    ulist = (ulistx = p);
    L785 :;
    while (((i64)((u64)(mm_decls_lx.symbol)) == (i64)5)) {
        mm_lex_lex();
        mm_lib_addlistunit(&ulist,&ulistx,(p = mm_parse_readunit()));
        if (((i64)((*p).tag) == (i64)22)) {
            ++nkeyvalues;
        };
        ++length;
        mm_parse_skipsemi();
L786 :;
    }L787 :;
    ;
    mm_parse_checksymbol((i64)15);
    mm_lex_lex();
    if (!!(nkeyvalues)) {
        if ((length > nkeyvalues)) {
            mm_support_serror((byte*)"dict: mixed elements");
        };
        p = mm_lib_createunit1((i64)17,ulist);
    } else {
        p = mm_lib_createunit1((i64)16,ulist);
    };
    (*p).length = length;
    return p;
}

static i64 mm_parse_istypestarter(void) {
    if ((!!((u64)(mm_decls_typestarterset[((i64)(mm_decls_lx.symbol))])) || (((i64)((u64)(mm_decls_lx.symbol)) == (i64)50) && ((i64)((u64)(mm_decls_nextlx.symbol)) == (i64)50)))) {
        return (i64)1;
    };
    return (i64)0;
}

static struct mm_decls_unitrec * mm_parse_readunit(void) {
    return mm_parse_readfactor((i64)8);
}

static struct mm_decls_unitrec * mm_parse_readfactor(i64 level) {
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  q;
    i64 opc;
    i64 opprio;
    i64 lineno;
    i64 isassign;
    if ((level <= (i64)1)) {
        if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)50)) {
            if (((i64)(mm_decls_nextlx.symbol)==(i64)6) || ((i64)(mm_decls_nextlx.symbol)==(i64)5)) {
                p = mm_lib_createname(mm_decls_lx.symptr);
                (*p).lineno = (i64)(mm_decls_lx.lineno);
                mm_lex_lex();
                return p;
            }else if (((i64)(mm_decls_nextlx.symbol)==(i64)32) || ((i64)(mm_decls_nextlx.symbol)==(i64)9)) {
                p = mm_lib_createname(mm_decls_lx.symptr);
                (*p).lineno = (i64)(mm_decls_lx.lineno);
                mm_lex_lex();
                goto L788 ;
;
            };
        };
        p = mm_parse_readterm2();
    } else {
        p = mm_parse_readfactor((level - (i64)1));
    };
    L789 :;
    switch ((i64)(mm_decls_lx.symbol)) {
    case 32:;
    case 9:;
    case 24:;
    case 29:;
    case 10:;
    {
        //gotterm:
L788 :;
;
        opc = (i64)(mm_decls_lx.subcode);
        lineno = (i64)(mm_decls_lx.lineno);
        if (((mm_decls_nextlx.symbol == (i64)9) || (mm_decls_nextlx.symbol == (i64)10))) {
            mm_lex_lex();
            isassign = (i64)1;
            opprio = (i64)(mm_tables_jtagpriotable[((i64)23)]);
            opc = mm_lib_getoptocode(opc);
        } else {
            isassign = ((opc == (i64)23) || (opc == (i64)24));
            opprio = (i64)(mm_tables_jtagpriotable[(opc)]);
        };
        if ((opprio != level)) {
            goto L790 ;
        };
        mm_lex_lex();
        if (!!(isassign)) {
            q = mm_parse_readunit();
        } else if ((opc == (i64)92)) {
            q = mm_parse_readfactor(level);
        } else {
            q = mm_parse_readfactor((level - (i64)1));
        };
        p = mm_lib_createunit2(opc,p,q);
        (*p).lineno = lineno;
    }break;
    default: {
        goto L790 ;
    }
    } //SW
goto L789 ;
L790 :;
    ;
    return p;
}

static struct mm_decls_unitrec * mm_parse_readterm2(void) {
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  q;
    i64 opc;
    i64 oldinrp;
    i64 lineno;
    lineno = (i64)(mm_decls_lx.lineno);
    p = mm_parse_readterm();
    L791 :;
    switch ((i64)(mm_decls_lx.symbol)) {
    case 12:;
    {
        mm_lex_lex();
        oldinrp = mm_parse_inreadprint;
        mm_parse_inreadprint = (i64)0;
        q = mm_parse_readslist((i64)1,(i64)1);
        mm_parse_checksymbol((i64)13);
        mm_lex_lex();
        p = mm_lib_createunit2((((i64)((*p).tag) == (i64)89)?(i64)26:(i64)25),p,q);
        mm_parse_inreadprint = oldinrp;
        p = mm_parse_readcondsuffix(p);
    }break;
    case 18:;
    {
        p = mm_lib_createunit1((i64)93,p);
        mm_lex_lex();
    }break;
    case 14:;
    {
        p = mm_parse_readindex(p,(i64)0);
    }break;
    case 2:;
    {
        p = mm_parse_readdotsuffix(p);
    }break;
    case 16:;
    {
        p = mm_parse_readkeyindex(p);
    }break;
    case 7:;
    {
        if (!!(mm_parse_inreadprint)) {
            goto L792 ;
        };
        mm_lex_lex();
        q = mm_parse_readunit();
        p = mm_lib_createunit2((!!(mm_parse_inparamlist)?(i64)21:(i64)22),p,q);
    }break;
    case 39:;
    {
        if (((i64)(mm_decls_lx.subcode)==(i64)138)) {
            opc = (i64)140;
        }else if (((i64)(mm_decls_lx.subcode)==(i64)139)) {
            opc = (i64)141;
        };
        mm_lex_lex();
        p = mm_lib_createunit1(opc,p);
    }break;
    case 4:;
    {
        mm_lex_lex();
        mm_parse_checksymbol((i64)14);
        mm_lex_lex();
        q = mm_parse_readunit();
        if (((i64)((*q).tag) == (i64)15)) {
            p = mm_lib_createunit2((i64)87,p,q);
        } else {
            p = mm_lib_createunit2((i64)86,p,q);
        };
        mm_parse_checksymbol((i64)15);
        mm_lex_lex();
    }break;
    default: {
        goto L792 ;
    }
    } //SW
goto L791 ;
L792 :;
    ;
    (*p).lineno = lineno;
    return p;
}

static struct mm_decls_unitrec * mm_parse_readterm(void) {
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  q;
    struct mm_decls_unitrec *  r;
    byte *  pbyte;
    u64 a;
    i64 opc;
    i64 lineno;
    i64 shift;
    i64 t;
    i64 av_1;
    lineno = (i64)(mm_decls_lx.lineno);
    switch ((i64)(mm_decls_lx.symbol)) {
    case 50:;
    {
        if (((i64)((u64)(mm_decls_nextlx.symbol)) == (i64)21)) {
            p = mm_parse_readcast((i64)0);
        } else {
            p = mm_lib_createname(mm_decls_lx.symptr);
            (*p).lineno = (i64)(mm_decls_lx.lineno);
            mm_lex_lex();
        };
    }break;
    case 40:;
    case 42:;
    {
        p = mm_lib_createconstunit((u64)(mm_decls_lx.value),(i64)(mm_decls_lx.subcode));
        mm_lex_lex();
    }break;
    case 45:;
    {
        p = mm_lib_createstringconstunit(mm_decls_lx.svalue,(i64)(mm_decls_lx.length));
        mm_lex_lex();
    }break;
    case 46:;
    {
        p = mm_parse_makeastring();
        mm_lex_lex();
    }break;
    case 41:;
    {
        (*(mm_decls_lx.svalue + (i64)(mm_decls_lx.length))) = (u64)0u;
        p = mm_lib_createunit0((i64)6);
        (*p).svalue = mm_decls_lx.svalue;
        (*p).slength = (i64)(mm_decls_lx.length);
        (*p).mode = (i64)16;
        mm_lex_lex();
    }break;
    case 43:;
    {
        a = (u64)((i64)0);
        shift = (i64)0;
        pbyte = mm_decls_lx.svalue;
        av_1 = (i64)(mm_decls_lx.length);
        while (av_1-- > 0) {
L793 :;
            a = (a | ((u64)((*pbyte)) << shift));
            shift += (i64)8;
            ++pbyte;
L794 :;
        }L795 :;
        ;
        if ((a <= (u64)9223372036854775807u)) {
            t = (i64)15;
        } else {
            t = (i64)15;
        };
        p = mm_lib_createconstunit(a,t);
        mm_lex_lex();
    }break;
    case 12:;
    {
        p = mm_parse_readlbrack();
    }break;
    case 53:;
    case 104:;
    case 58:;
    case 55:;
    {
        p = mm_parse_readcast((i64)0);
    }break;
    case 32:;
    {
        p = mm_parse_readopc();
    }break;
    case 14:;
    {
        p = mm_parse_readset();
    }break;
    case 39:;
    {
        opc = (i64)(mm_decls_lx.subcode);
        mm_lex_lex();
        p = mm_lib_createunit1(opc,mm_parse_readterm2());
    }break;
    case 87:;
    {
        p = mm_parse_readsprint();
    }break;
    case 89:;
    case 90:;
    {
        p = mm_parse_readsread();
    }break;
    case 24:;
    {
        mm_lex_lex();
        p = mm_lib_createunit1((i64)94,mm_parse_readterm2());
        if (((i64)((*(*p).a).tag) == (i64)25)) {
            if (!!((*(*p).a).b)) {
                mm_support_serror((byte*)"Params not allowed");
            };
            (*p).a = (*(*p).a).a;
        };
    }break;
    case 4:;
    {
        mm_lex_lex();
        p = mm_lib_createunit1((i64)95,mm_parse_readterm2());
    }break;
    case 133:;
    {
        p = mm_parse_readcompilervar();
    }break;
    case 142:;
    {
        p = mm_lib_createconstunit((u64)(mm_decls_lx.subcode),(i64)4);
        mm_lex_lex();
    }break;
    case 134:;
    {
        if (!!(mm_parse_intabledata)) {
            p = mm_lib_createstringconstunit(mm_parse_tabledataname,(i64)-1);
        } else {
            if ((mm_parse_ndollar <= (i64)0)) {
                mm_support_serror((byte*)"[$] No array");
            };
            p = mm_lib_createunit1((i64)128,mm_parse_dollarstack[(mm_parse_ndollar)-1]);
        };
        mm_lex_lex();
    }break;
    case 138:;
    {
        p = mm_parse_readapplyop((i64)1);
    }break;
    case 131:;
    {
        p = mm_parse_readcastx();
    }break;
    case 132:;
    {
        mm_lex_lex();
        mm_parse_checksymbol((i64)12);
        mm_lex_lex();
        p = mm_lib_createunit0((i64)100);
        mm_lib_storemode((i64)3,mm_decls_currproc,mm_parse_readtypespec(mm_decls_currproc,(i64)0),(i32 *)(&(*p).value));
        mm_parse_checksymbol((i64)13);
        mm_lex_lex();
    }break;
    case 140:;
    {
        mm_lex_lex();
        mm_parse_checksymbol((i64)12);
        mm_lex_lex();
        p = mm_parse_readunit();
        mm_parse_checksymbol((i64)5);
        mm_lex_lex();
        q = mm_parse_readunit();
        if ((((i64)((u64)(mm_decls_lx.symbol)) == (i64)13) && ((i64)((*q).tag) == (i64)15))) {
            r = (*q).b;
            q = (*q).a;
        } else {
            mm_parse_checksymbol((i64)5);
            mm_lex_lex();
            r = mm_parse_readunit();
            mm_parse_checksymbol((i64)13);
        };
        mm_lex_lex();
        q = mm_lib_createunit2((i64)55,p,q);
        p = mm_lib_createunit2((i64)54,q,r);
    }break;
    case 83:;
    {
        p = mm_parse_readgoto((i64)208);
    }break;
    case 59:;
    {
        p = mm_parse_readif();
    }break;
    case 67:;
    {
        p = mm_parse_readunless();
    }break;
    case 68:;
    case 69:;
    case 84:;
    case 85:;
    {
        p = mm_parse_readswitchcase();
    }break;
    case 70:;
    {
        p = mm_parse_readrecase();
    }break;
    case 72:;
    {
        p = mm_parse_readfor();
    }break;
    case 73:;
    {
        p = mm_parse_readforall();
    }break;
    case 74:;
    {
        p = mm_parse_readto();
    }break;
    case 76:;
    {
        p = mm_parse_readdo();
    }break;
    case 77:;
    {
        p = mm_parse_readwhile();
    }break;
    case 78:;
    {
        p = mm_parse_readrepeat();
    }break;
    case 82:;
    {
        p = mm_parse_readloopcontrol();
    }break;
    case 80:;
    {
        p = mm_parse_readreturn();
    }break;
    case 81:;
    {
        p = mm_parse_readstop();
    }break;
    case 86:;
    {
        p = mm_parse_readprint();
    }break;
    case 88:;
    {
        p = mm_parse_readread();
    }break;
    case 124:;
    {
        p = mm_parse_readtry();
    }break;
    case 127:;
    {
        p = mm_parse_readraise();
    }break;
    case 141:;
    {
        mm_lex_lex();
        mm_parse_checksymbol((i64)12);
        mm_lex_lex();
        p = mm_parse_readunit();
        mm_parse_checksymbol((i64)5);
        mm_lex_lex();
        q = mm_parse_readunit();
        mm_parse_checksymbol((i64)13);
        mm_lex_lex();
        p = mm_lib_createunit2((i64)220,p,q);
    }break;
    case 135:;
    {
        mm_lex_lex();
        p = mm_lib_createunit1((i64)240,mm_parse_readunit());
    }break;
    case 144:;
    {
        (*mm_decls_currproc).asmused = (u64)((i64)1);
        mm_decls_assemmode = (i64)1;
        if (((i64)((u64)(mm_decls_lx.subcode)) == (i64)0)) {
            p = mm_parse_readassemline();
        } else {
            p = mm_parse_readassemblock();
        };
        mm_decls_assemmode = (i64)0;
    }break;
    case 16:;
    {
        mm_lex_lex();
        p = mm_parse_readsunit((i64)0);
        mm_parse_checksymbol((i64)17);
        mm_lex_lex();
        p = mm_lib_createunit1((i64)241,p);
    }break;
    case 48:;
    {
        p = mm_lib_createstringconstunit(mm_decls_lx.svalue,(i64)(mm_decls_lx.length));
        (*p).tag = (i64)242;
        mm_lex_lex();
    }break;
    default: {
        msysc_m_print_startcon();
        msysc_m_print_str(mm_tables_symbolnames[((i64)(mm_decls_lx.symbol))-1],NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        mm_support_serror((byte*)"readterm?");
    }
    } //SW
;
    (*p).lineno = lineno;
    return p;
}

static struct mm_decls_unitrec * mm_parse_readxunit(void) {
    return mm_parse_readsunit((i64)0);
}

static struct mm_decls_unitrec * mm_parse_readsunit(i64 inwhile) {
    i64 lineno;
    struct mm_decls_unitrec *  ulist;
    struct mm_decls_unitrec *  ulistx;
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  q;
    struct mm_decls_unitrec *  r;
    struct mm_decls_strec *  stname;
    lineno = (i64)(mm_decls_lx.lineno);
    ulist = (ulistx = (struct mm_decls_unitrec *)(0));
    L796 :;
    do {
        L799 :;
        while (((i64)((u64)(mm_decls_lx.symbol)) == (i64)6)) {
            mm_lex_lex();
L800 :;
        }L801 :;
        ;
        switch ((i64)(mm_decls_lx.symbol)) {
        case 123:;
        {
            mm_lex_lex();
            mm_parse_checksymbol((i64)105);
            mm_parse_readvardef(mm_decls_currproc,(i64)0,(i64)1,(i64)9);
        }break;
        case 91:;
        case 92:;
        {
            mm_parse_readprocdef(mm_decls_currproc,(i64)0,(i64)0);
        }break;
        case 53:;
        case 14:;
        case 104:;
        case 58:;
        case 55:;
        {
            goto L802 ;
;
        }break;
        case 105:;
        case 107:;
        case 106:;
        {
            q = mm_parse_readvardef(mm_decls_currproc,(i64)0,(i64)0,(i64)10);
            L803 :;
            while (!!(q)) {
                r = (*q).nextunit;
                (*q).nextunit = (struct mm_decls_unitrec *)(0);
                mm_lib_addlistunit(&ulist,&ulistx,q);
                q = r;
L804 :;
            }L805 :;
            ;
        }break;
        case 102:;
        {
            mm_parse_readtypedef(mm_decls_currproc,(i64)0);
        }break;
        case 113:;
        {
            mm_parse_readconstdef(mm_decls_currproc,(i64)0);
        }break;
        case 117:;
        case 94:;
        {
            mm_parse_readclassdef(mm_decls_currproc,(i64)0);
        }break;
        case 38:;
        {
            mm_support_serror((byte*)"DOCSTRING");
        }break;
        case 115:;
        {
            mm_lex_lex();
            mm_parse_readenumtype(mm_decls_currproc,(i64)0,(i64)0);
        }break;
        case 110:;
        {
            mm_parse_readmacrodef(mm_decls_currproc,(i64)0);
        }break;
        case 36:;
        {
            msysc_m_print_startcon();
            msysc_m_print_str((*mm_decls_currproc).name,NULL);
            msysc_m_print_newline();
            msysc_m_print_end();
            ;
            mm_support_serror((byte*)"Unexpected EOF in proc");
        }break;
        case 13:;
        case 60:;
        case 61:;
        case 62:;
        case 79:;
        case 71:;
        case 63:;
        case 64:;
        case 125:;
        case 66:;
        case 17:;
        {
            goto L798 ;
        }break;
        case 50:;
        {
            if (((i64)((u64)(mm_decls_nextlx.symbol)) == (i64)8)) {
                p = mm_lib_createunit0((i64)210);
                stname = mm_lib_getduplnameptr(mm_decls_currproc,mm_decls_lx.symptr,(i64)15);
                (*p).def = stname;
                (*p).trylevel = mm_parse_try_level;
                mm_lex_lex();
                mm_decls_lx.symbol = (u64)((i64)6);
                mm_lib_addlistunit(&ulist,&ulistx,p);
            } else {
                goto L802 ;
;
            };
        }break;
        case 76:;
        {
            if (!!(inwhile)) {
                goto L798 ;
            };
            goto L802 ;
;
        }break;
        case 6:;
        {
        }break;
        default: {
            //doexec:
L802 :;
;
            p = mm_parse_readunit();
            if ((((i64)((*p).tag) == (i64)3) && ((i64)((u64)(mm_decls_lx.symbol)) == (i64)50))) {
                mm_support_serror((byte*)"Possibly var/let needed");
            };
            mm_lib_addlistunit(&ulist,&ulistx,p);
            if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)76)) {
                goto L798 ;
            };
        }
        } //SW
;
L797 :;
    } while (!((i64)((u64)(mm_decls_lx.symbol)) != (i64)6));L798 :;
    ;
    if (((i64)(mm_decls_lx.symbol)==(i64)13) || ((i64)(mm_decls_lx.symbol)==(i64)60) || ((i64)(mm_decls_lx.symbol)==(i64)61) || ((i64)(mm_decls_lx.symbol)==(i64)62) || ((i64)(mm_decls_lx.symbol)==(i64)79) || ((i64)(mm_decls_lx.symbol)==(i64)71) || ((i64)(mm_decls_lx.symbol)==(i64)76) || ((i64)(mm_decls_lx.symbol)==(i64)63) || ((i64)(mm_decls_lx.symbol)==(i64)64) || ((i64)(mm_decls_lx.symbol)==(i64)125) || ((i64)(mm_decls_lx.symbol)==(i64)66) || ((i64)(mm_decls_lx.symbol)==(i64)17) || ((i64)(mm_decls_lx.symbol)==(i64)5) || ((i64)(mm_decls_lx.symbol)==(i64)19)) {
    } else {
        mm_support_serror((byte*)"Readsunit: \";\" expected, or bad unit starter");
    };
    if (((ulist == 0) || !!((*ulist).nextunit))) {
        return mm_lib_createunit1((i64)4,ulist);
    } else {
        return ulist;
    };
}

static void mm_parse_readmacrodef(struct mm_decls_strec * owner,i64 isglobal) {
    struct mm_decls_strec *  nameptr;
    struct mm_decls_strec *  stmacro;
    struct mm_decls_strec *  paramlist;
    struct mm_decls_strec *  paramlistx;
    struct mm_decls_strec *  stname;
    mm_lex_lex();
    mm_parse_checksymbol((i64)50);
    nameptr = mm_decls_lx.symptr;
    stmacro = mm_lib_getduplnameptr(owner,nameptr,(i64)18);
    mm_lib_adddef(owner,stmacro);
    owner = stmacro;
    mm_lex_lex();
    paramlist = (paramlistx = (struct mm_decls_strec *)(0));
    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)12)) {
        mm_lex_lex();
        if (((i64)((u64)(mm_decls_lx.symbol)) != (i64)13)) {
            L806 :;
            while (1) {
                if (((i64)(mm_decls_lx.symbol)==(i64)50)) {
                    stname = mm_lib_getduplnameptr(owner,mm_decls_lx.symptr,(i64)19);
                    mm_lib_adddef(owner,stname);
                    mm_parse_addlistparam(&paramlist,&paramlistx,stname);
                    (*stname).nulldef = mm_decls_lx.symptr;
                    mm_lex_lex();
                    if (((i64)((u64)(mm_decls_lx.symbol)) == (i64)13)) {
                        goto L807 ;
                    };
                    mm_parse_checksymbol((i64)5);
                    mm_lex_lex();
                } else {
                    mm_support_serror((byte*)"macro def params");
                };
            }L807 :;
            ;
        };
        mm_lex_lex();
    };
    (*stmacro).paramlist = paramlist;
    (*stmacro).isglobal = (u64)(isglobal);
    mm_parse_checkequals();
    mm_lex_lex();
    (*stmacro).code = mm_parse_readunit();
}

static void mm_parse_readimportalias(struct mm_decls_strec * dimport) {
    struct mm_decls_strec *  stmacro;
    mm_lex_lex();
    mm_parse_checksymbol((i64)50);
    stmacro = mm_lib_getduplnameptr(mm_decls_stmodule,mm_decls_lx.symptr,(i64)18);
    mm_lib_adddef(mm_decls_stmodule,stmacro);
    mm_lex_lex();
    (*stmacro).paramlist = (struct mm_decls_strec *)(0);
    (*stmacro).code = mm_lib_createname(dimport);
}

static void mm_parse_domappedalias(struct mm_decls_strec * dimport,struct mm_decls_strec * stimport) {
    struct mm_decls_strec *  stmacro;
    if (!!(mlib_eqstring((*dimport).name,(*stimport).name))) {
        return;
    };
    stmacro = mm_lib_getduplnameptr(mm_decls_stmodule,dimport,(i64)18);
    mm_lib_adddef(mm_decls_stmodule,stmacro);
    (*stmacro).paramlist = (struct mm_decls_strec *)(0);
    (*stmacro).code = mm_lib_createname(stimport);
}

static struct mm_decls_unitrec * mm_parse_readrecase(void) {
    mm_lex_lex();
    return mm_lib_createunit1((i64)222,mm_parse_readunit());
}

void mm_name_rx_unit(struct mm_decls_strec * owner,struct mm_decls_unitrec * p) {
    struct mm_decls_strec *  d;
    struct mm_decls_unitrec *  a;
    struct mm_decls_unitrec *  b;
    i64 n;
    i64 oldnoexpand;
    i64 oldnoassem;
    a = (*p).a;
    b = (*p).b;
    mm_decls_mlineno = (i64)((*p).lineno);
    switch ((i64)((*p).tag)) {
    case 3:;
    {
        mm_name_resolvename(owner,p,(i64)0);
        if ((((i64)((u64)((*(*p).def).nameid)) == (i64)18) && !(!!(mm_name_noexpand)))) {
            ++mm_name_macrolevels;
            mm_name_expandmacro(p,p,(struct mm_decls_unitrec *)(0));
            mm_name_rx_unit(owner,p);
            --mm_name_macrolevels;
        };
    }break;
    case 21:;
    {
        mm_name_rx_unit(owner,b);
    }break;
    case 89:;
    {
        mm_name_resolvedot(owner,p);
    }break;
    case 189:;
    case 25:;
    case 26:;
    {
        if (((i64)((*a).tag) == (i64)3)) {
            oldnoexpand = mm_name_noexpand;
            mm_name_noexpand = (i64)1;
            mm_name_rx_unit(owner,a);
            mm_name_noexpand = oldnoexpand;
        } else {
            mm_name_rx_unit(owner,a);
        };
        mm_name_rx_unitlist(owner,b);
        if (((i64)((*a).tag) == (i64)3)) {
            d = (*a).def;
            if (((i64)((*d).nameid)==(i64)4)) {
                (*p).tag = (i64)96;
                mm_lib_storemode((i64)17,owner,(i64)((*d).mode),&(*p).newmode);
                (*p).a = b;
                (*p).b = (struct mm_decls_unitrec *)(0);
                if (!!((*b).nextunit)) {
                    (*p).a = mm_lib_createunit1((i64)14,b);
                    n = (i64)0;
                    L808 :;
                    while (!!(b)) {
                        ++n;
                        b = (*b).nextunit;
L809 :;
                    }L810 :;
                    ;
                    (*(*p).a).length = n;
                };
            }else if (((i64)((*d).nameid)==(i64)18)) {
                ++mm_name_macrolevels;
                mm_name_expandmacro(p,a,b);
                mm_name_rx_unit(owner,p);
                --mm_name_macrolevels;
            } else {
                if (((i64)((*d).mode) == (i64)0)) {
                    (*p).tag = (i64)189;
                };
            };
        };
    }break;
    case 30:;
    case 31:;
    case 32:;
    case 33:;
    case 35:;
    case 34:;
    {
        if (((i64)((*(*p).a).tag)==(i64)30) || ((i64)((*(*p).a).tag)==(i64)31) || ((i64)((*(*p).a).tag)==(i64)32) || ((i64)((*(*p).a).tag)==(i64)33) || ((i64)((*(*p).a).tag)==(i64)35) || ((i64)((*(*p).a).tag)==(i64)34)) {
            mm_name_converteqeq(owner,p);
        } else {
            goto L811 ;
;
        };
        goto L811 ;
;
    }break;
    case 7:;
    {
        if (!(!!(mm_name_noassem))) {
            mm_name_rx_assem(owner,p,a,b);
        };
    }break;
    case 8:;
    {
        mm_name_resolvename(owner,a,(i64)0);
        if (!(!!(mm_name_noexpand))) {
            ++mm_name_macrolevels;
            oldnoassem = mm_name_noassem;
            mm_name_noassem = (i64)1;
            mm_name_expandmacro(p,a,b);
            mm_name_noassem = oldnoassem;
            mm_name_rx_unit(owner,p);
            --mm_name_macrolevels;
        };
    }break;
    case 198:;
    case 199:;
    {
        mm_name_resolvename(owner,a,(i64)4);
        a = (*a).nextunit;
        goto L811 ;
;
    }break;
    default: {
        //doabc:
L811 :;
;
        mm_name_rx_unitlist(owner,a);
        if (!!(b)) {
            mm_name_rx_unitlist(owner,b);
            if (!!((*p).c)) {
                mm_name_rx_unitlist(owner,(*p).c);
            };
        };
    }
    } //SW
;
}

i64 mm_name_rx_module(i64 n) {
    mm_decls_currmoduleno = n;
    mm_name_rx_passdef(mm_decls_stprogram,mm_decls_moduletable[(n)].stmodule);
    return (i64)1;
}

void mm_name_rx_deflist(struct mm_decls_strec * owner,struct mm_decls_strec * p) {
    struct mm_decls_strec *  pstart = p;
    pstart = p;
    L812 :;
    while (!!(p)) {
        mm_name_rx_passdef(owner,p);
        p = (*p).nextdef;
L813 :;
    }L814 :;
    ;
}

void mm_name_rx_passdef(struct mm_decls_strec * owner,struct mm_decls_strec * p) {
    if (((i64)((*p).nameid)==(i64)2) || ((i64)((*p).nameid)==(i64)3)) {
        mm_name_rx_deflist(p,(*p).deflist);
    }else if (((i64)((*p).nameid)==(i64)5)) {
        mm_name_fixmode(owner,p);
        mm_name_rx_deflist(p,(*p).deflist);
        mm_name_currstproc = p;
        mm_name_rx_unit(p,(*p).code);
        mm_name_currstproc = (struct mm_decls_strec *)(0);
    }else if (((i64)((*p).nameid)==(i64)6)) {
        mm_name_fixmode(owner,p);
        mm_name_rx_deflist(p,(*p).deflist);
    }else if (((i64)((*p).nameid)==(i64)8) || ((i64)((*p).nameid)==(i64)9) || ((i64)((*p).nameid)==(i64)10) || ((i64)((*p).nameid)==(i64)11)) {
        mm_name_fixmode(owner,p);
        if (((i64)((u64)((*p).at)) == (i64)1)) {
            mm_name_rx_unit(owner,(*p).equivvar);
        };
        if (!!((*p).code)) {
            mm_name_rx_unit(owner,(*p).code);
        };
    }else if (((i64)((*p).nameid)==(i64)4)) {
        mm_name_fixmode(owner,p);
    } else {
    };
}

static void mm_name_rx_unitlist(struct mm_decls_strec * owner,struct mm_decls_unitrec * p) {
    L815 :;
    while (!!(p)) {
        mm_name_rx_unit(owner,p);
        p = (*p).nextunit;
L816 :;
    }L817 :;
    ;
}

struct mm_decls_strec * mm_name_resolvetopname(struct mm_decls_strec * owner,struct mm_decls_strec * stnewname,i64 moduleno,i64 fmodule) {
    i64 i;
    i64 extcount;
    i64 modno;
    struct mm_decls_strec *  p;
    struct mm_decls_strec *  powner;
    struct mm_decls_strec *  dlldef;
    struct mm_decls_strec *  extdef;
    struct mm_decls_strec *  moddef;
    struct mm_decls_strec *  extmod;
    struct mm_decls_strec *  q;
    struct mm_decls_strec *  ambiglist[10];
    if (((i64)((u64)((*owner).nameid)) == (i64)5)) {
        q = (*owner).deflist;
        L818 :;
        while (!!(q)) {
            if (((*q).firstdupl == stnewname)) {
                return q;
            };
            q = (*q).nextdef;
L819 :;
        }L820 :;
        ;
    };
    p = (*stnewname).nextdupl;
    extcount = (i64)0;
    extmod = (dlldef = (extdef = (moddef = (struct mm_decls_strec *)(0))));
    L821 :;
    while (!!(p)) {
        powner = (*p).owner;
        switch ((i64)((*powner).nameid)) {
        case 5:;
        {
            if ((powner == owner)) {
                return p;
            };
        }break;
        case 2:;
        {
            if (((i64)((u64)((*powner).moduleno)) == moduleno)) {
                if (((i64)((u64)((*owner).nameid)) == (i64)2)) {
                    return p;
                };
                moddef = p;
            } else if (!!((u64)(mm_decls_moduletable[(moduleno)].importmap[((i64)((*powner).moduleno))-1]))) {
                if (!!((u64)((*p).isglobal))) {
                    ++extcount;
                    extdef = p;
                    if ((extcount < (i64)10)) {
                        ambiglist[(extcount)-1] = extdef;
                    };
                };
            };
        }break;
        case 3:;
        {
            modno = (i64)((*(*powner).owner).moduleno);
            if (((modno == moduleno) || !!((u64)(mm_decls_moduletable[(moduleno)].importmap[(modno)-1])))) {
                dlldef = p;
            };
        }break;
        case 4:;
        {
            if ((powner == owner)) {
                return p;
            };
        }break;
        case 1:;
        {
            if (((i64)((u64)((*p).nameid)) == (i64)2)) {
                if (((i64)((u64)((*p).moduleno)) == moduleno)) {
                    if (!!(fmodule)) {
                        return p;
                    };
                } else {
                    extmod = p;
                };
            };
        }break;
        default: {
        }
        } //SW
;
        p = (*p).nextdupl;
L822 :;
    }L823 :;
    ;
    if (!!(moddef)) {
        return moddef;
    };
    if (!!(extdef)) {
        if ((extcount > (i64)1)) {
            L824 :;
            for (i=(i64)1;i<=extcount;++i) {
L825 :;
                extdef = ambiglist[(i)-1];
                msysc_m_print_startcon();
                msysc_m_print_i64(i,NULL);
                msysc_m_print_str((*(*extdef).owner).name,NULL);
                msysc_m_print_str(mm_tables_namenames[((i64)((*(*extdef).owner).nameid))],NULL);
                msysc_m_print_newline();
                msysc_m_print_end();
                ;
L826 :;
            }L827 :;
            ;
            mm_support_rxerror_s((byte*)"Ambiguous ext name: #",(*extdef).name,(struct mm_decls_unitrec *)(0));
        };
        return extdef;
    };
    if (!!(extmod)) {
        return extmod;
    };
    return dlldef;
}

void mm_name_resolvename(struct mm_decls_strec * owner,struct mm_decls_unitrec * p,i64 mode) {
    struct mm_decls_strec *  d;
    struct mm_decls_strec *  e;
    i64 moduleno;
    d = (*p).def;
    moduleno = (i64)((*p).moduleno);
    if (((i64)((u64)((*d).nameid)) != (i64)0)) {
        return;
    };
    e = mm_name_resolvetopname(owner,d,moduleno,mm_name_allowmodname);
    if (!(!!(e))) {
        if ((mode == (i64)0)) {
            mm_support_rxerror_s((byte*)"Undefined: #",(*d).name,p);
        } else {
            e = mm_name_addframevar(owner,d,moduleno,mode);
            (*e).lineno = (i64)((*p).lineno);
            (*e).islet = (u64)((i64)1);
        };
    };
    (*e).used = (u64)((i64)1);
    mm_name_fixmode(owner,e);
    if ((((i64)((u64)((*e).nameid)) == (i64)11) && ((i64)((u64)((*e).parammode)) == (i64)2))) {
        (*p).tag = (i64)93;
        (*p).a = mm_lib_createname(e);
        (*p).def = (struct mm_decls_strec *)(0);
    };
    (*p).def = e;
    if (((i64)((*e).nameid)==(i64)5)) {
        if (!!((u64)((*e).isglobal))) {
            (*e).namecat = (u64)((i64)2);
        };
    };
}

struct mm_decls_strec * mm_name_finddupl(struct mm_decls_strec * d,struct mm_decls_strec * pdupl) {
    if (((i64)((u64)((*pdupl).nameid)) != (i64)0)) {
        return pdupl;
    };
    pdupl = (*pdupl).nextdupl;
    L828 :;
    while (!!(pdupl)) {
        if (((*pdupl).owner == d)) {
            return pdupl;
        };
        pdupl = (*pdupl).nextdupl;
L829 :;
    }L830 :;
    ;
    return (struct mm_decls_strec *)(0);
}

static void mm_name_resolvedot(struct mm_decls_strec * owner,struct mm_decls_unitrec * p) {
    struct mm_decls_unitrec *  lhs;
    struct mm_decls_unitrec *  rhs;
    struct mm_decls_strec *  d;
    struct mm_decls_strec *  e;
    struct mm_decls_strec *  t;
    i64 m;
    lhs = (*p).a;
    rhs = (*p).b;
    e = (*rhs).def;
    mm_name_rx_unit(owner,lhs);
    if (((i64)((*lhs).tag)==(i64)3)) {
        d = (*lhs).def;
        if (((i64)((*d).nameid)==(i64)2) || ((i64)((*d).nameid)==(i64)4) || ((i64)((*d).nameid)==(i64)5) || ((i64)((*d).nameid)==(i64)4) || ((i64)((*d).nameid)==(i64)3)) {
            e = mm_name_finddupl(d,e);
            if (!!(e)) {
                (*p).tag = (i64)3;
                (*p).a = ((*p).b = (struct mm_decls_unitrec *)(0));
                (*p).def = e;
                if (((i64)((*e).nameid)==(i64)14)) {
                }else if (((i64)((*e).nameid)==(i64)8)) {
                }else if (((i64)((*e).nameid)==(i64)18)) {
                    if ((((i64)((u64)((*e).nameid)) == (i64)18) && !(!!(mm_name_noexpand)))) {
                        ++mm_name_macrolevels;
                        mm_name_expandmacro(p,p,(struct mm_decls_unitrec *)(0));
                        mm_name_rx_unit(owner,p);
                        --mm_name_macrolevels;
                    };
                };
            } else {
                mm_support_rxerror_s((byte*)"Can't resolve .#",(*(*(*p).b).def).name,p);
            };
        }else if (((i64)((*d).nameid)==(i64)10) || ((i64)((*d).nameid)==(i64)9) || ((i64)((*d).nameid)==(i64)11)) {
            m = (i64)((*d).mode);
            if (((i64)(mm_decls_ttbasetype[(m)])==(i64)32) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)33)) {
            }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)20)) {
                L831 :;
                while (1) {
                    m = (i64)(mm_decls_tttarget[(m)]);
                    if (((i64)(mm_decls_ttbasetype[(m)])==(i64)32) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)33)) {
                        goto L832 ;
                    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)20)) {
                    } else {
                        mm_support_rxerror((byte*)"2:record expected",(struct mm_decls_unitrec *)(0));
                    };
                }L832 :;
                ;
            } else {
                mm_support_rxerror((byte*)"record expected",(struct mm_decls_unitrec *)(0));
            };
            t = mm_decls_ttnamedef[(m)];
            e = mm_name_finddupl(t,e);
            if (!!(e)) {
                (*(*p).b).def = e;
            } else {
                mm_support_rxerror_s((byte*)"Not a field: #",(*(*rhs).def).name,(struct mm_decls_unitrec *)(0));
            };
        };
    } else {
        if (!(!!((*e).nextdupl))) {
            mm_support_rxerror_s((byte*)"Not a field: #",(*e).name,(struct mm_decls_unitrec *)(0));
        };
    };
}

static void mm_name_fixmode(struct mm_decls_strec * owner,struct mm_decls_strec * p) {
    struct mm_decls_strec *  d;
    struct mm_decls_strec *  e;
    i64 m;
    m = (i64)((*p).mode);
    if ((m >= (i64)0)) {
        return;
    };
    m = -(m);
    if (!!(mm_decls_ttxmap[(m)])) {
        (*p).mode = mm_decls_ttxmap[(m)];
        return;
    };
    if (!!(mm_decls_ttnamedefx2[(m)])) {
        mm_support_rxerror((byte*)"Can't resolve a:b tentative types yet",(struct mm_decls_unitrec *)(0));
    };
    d = mm_decls_ttnamedefx[(m)];
    e = mm_name_resolvetopname(owner,d,(i64)(mm_decls_ttxmoduleno[(m)]),(i64)0);
    if (!!(e)) {
        mm_decls_ttxmap[(m)] = (i64)((*e).mode);
        (*p).mode = (i64)((*e).mode);
    } else {
        mm_decls_mlineno = (mm_decls_ttlinenox[(m)] + (i64)(((u64)(mm_decls_ttxmoduleno[(m)]) << (i64)24)));
        mm_support_rxerror_s((byte*)"Can't resolve tentative type: #",(*d).name,(struct mm_decls_unitrec *)(0));
    };
}

static i64 mm_name_fixmode2(struct mm_decls_strec * owner,i64 m) {
    struct mm_decls_strec *  d;
    struct mm_decls_strec *  e;
    byte str[256];
    if ((m >= (i64)0)) {
        return m;
    };
    m = -(m);
    if (!!(mm_decls_ttxmap[(m)])) {
        return mm_decls_ttxmap[(m)];
    };
    if (!!(mm_decls_ttnamedefx2[(m)])) {
        mm_support_rxerror((byte*)"2:Can't resolve a:b tentative types yet",(struct mm_decls_unitrec *)(0));
    };
    d = mm_decls_ttnamedefx[(m)];
    if ((owner == 0)) {
        msysc_m_print_startcon();
        msysc_m_print_str((*d).name,NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        mm_support_rxerror((byte*)"FIXMODE2 OWNER=0",(struct mm_decls_unitrec *)(0));
    };
    e = mm_name_resolvetopname(owner,d,(i64)(mm_decls_ttxmoduleno[(m)]),(i64)0);
    if (!!(e)) {
        mm_decls_ttxmap[(m)] = (i64)((*e).mode);
        return (i64)((*e).mode);
    } else {
        msysc_m_print_startstr(str);
        msysc_m_print_setfmt((byte*)"# in module #, line:#");
        msysc_m_print_str((*d).name,NULL);
        msysc_m_print_str(mm_decls_moduletable[((i64)(mm_decls_ttxmoduleno[(m)]))].name,NULL);
        msysc_m_print_i64(mm_decls_ttlinenox[(m)],NULL);
        msysc_m_print_end();
        ;
        mm_decls_mlineno = (mm_decls_ttlinenox[(m)] + (i64)(((u64)(mm_decls_ttxmoduleno[(m)]) << (i64)24)));
        mm_support_rxerror_s((byte*)"2:Can't resolve tentative type: #",str,(struct mm_decls_unitrec *)(0));
    };
    return (i64)0;
}

void mm_name_fixusertypes(void) {
    struct mm_decls_userxrec *  p;
    i64 m;
    i64 rescan;
    i64 i;
    L833 :;
    for (i=(i64)1;i<=(i64)2;++i) {
L834 :;
        p = mm_decls_userxmodelist;
        rescan = (i64)0;
        L837 :;
        while (!!(p)) {
            m = (i64)((*(*p).pmode));
            if ((m < (i64)0)) {
                m = mm_name_fixmode2((*p).owner,m);
                if ((((m < (i64)0) && (i == (i64)2)) && !!(mm_decls_ttxmap[(labs(m))]))) {
                    m = mm_decls_ttxmap[(labs(m))];
                };
                if ((m < (i64)0)) {
                    rescan = (i64)1;
                } else {
                    (*(*p).pmode) = m;
                    if (((i64)(mm_decls_tttarget[(m)]) == m)) {
                        msysc_m_print_startcon();
                        msysc_m_print_str((byte*)"TTNAME[M]=",NULL);
                        msysc_m_print_str(mm_decls_ttname[(m)],NULL);
                        msysc_m_print_newline();
                        msysc_m_print_end();
                        ;
                        mm_support_rxerror((byte*)"RECURSIVE TYPE?",(struct mm_decls_unitrec *)(0));
                    };
                };
            };
            p = (*p).nextmode;
L838 :;
        }L839 :;
        ;
        if (!(!!(rescan))) {
            goto L836 ;
        };
L835 :;
    }L836 :;
    ;
    if (!!(rescan)) {
        mm_support_rxerror((byte*)"FIXUSERTYPES PHASE ERROR",(struct mm_decls_unitrec *)(0));
    };
}

static void mm_name_rx_assem(struct mm_decls_strec * owner,struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    struct mm_decls_unitrec *  q;
    struct mm_decls_strec *  d;
    byte *  s;
    byte *  pdest;
    byte str[512];
    i64 c;
    q = b;
    L840 :;
    while (!!(q)) {
        if (((i64)((*q).tag) == (i64)3)) {
            mm_name_resolvename(owner,q,(i64)0);
        };
        q = (*q).nextunit;
L841 :;
    }L842 :;
    ;
    pdest = str;
    s = (*a).svalue;
    q = b;
    L843 :;
    while (!!((c = (i64)((*s++))))) {
        if ((c == (i64)35)) {
            if (((i64)((*q).tag)==(i64)3)) {
                d = (*q).def;
                if (((i64)((*d).nameid)==(i64)8)) {
                    mm_type_tx_namedconst(d);
                    mm_lib_addint(&pdest,(*(*d).code).value);
                }else if (((i64)((*d).nameid)==(i64)10) || ((i64)((*d).nameid)==(i64)11)) {
                    mm_lib_addstr(&pdest,(byte*)"Aframe+");
                    mm_lib_addstr(&pdest,mm_lib_getfullname(d,(i64)1));
                } else {
                    mm_lib_addstr(&pdest,mm_lib_getfullname(d,(i64)0));
                };
            }else if (((i64)((*q).tag)==(i64)1)) {
                if ((!(mm_decls_ttisint[((i64)((*q).mode))]) != (i64)73)) {
                    mm_support_rxerror((byte*)"assem/macro/not int",(struct mm_decls_unitrec *)(0));
                };
                mm_lib_addint(&pdest,(*q).value);
            } else {
                mm_support_rxerror((byte*)"assem/macro/arg?",(struct mm_decls_unitrec *)(0));
            };
            q = (*q).nextunit;
        } else {
            mm_lib_addchar(&pdest,(u64)((byte)(c)));
        };
L844 :;
    }L845 :;
    ;
    (*pdest) = (u64)0u;
    (*a).svalue = mlib_pcm_copyheapstring(str);
    (*a).slength = (i64)(strlen((i8 *)(str)));
}

struct mm_decls_strec * mm_name_resolve_equiv_name(struct mm_decls_strec * owner,struct mm_decls_strec * p) {
    if (((i64)((u64)((*p).nameid)) == (i64)12)) {
        return p;
    };
    mm_support_rxerror((byte*)"RESOLVE EQUIV FIELD/COMPLEX",(struct mm_decls_unitrec *)(0));
    return (struct mm_decls_strec *)(0);
}

static struct mm_decls_strec * mm_name_addframevar(struct mm_decls_strec * owner,struct mm_decls_strec * d,i64 moduleno,i64 mode) {
    struct mm_decls_strec *  e;
    e = mm_lib_getduplnameptr(owner,d,(i64)10);
    mm_lib_storemode((i64)1,owner,mode,&(*e).mode);
    mm_lib_adddef(owner,e);
    return e;
}

static void mm_name_converteqeq(struct mm_decls_strec * owner,struct mm_decls_unitrec * p) {
    i64 leftop;
    i64 rightop;
    struct mm_decls_unitrec *  w;
    struct mm_decls_unitrec *  y1;
    struct mm_decls_unitrec *  y2;
    struct mm_decls_unitrec *  z;
    w = (*p).a;
    y1 = (*w).b;
    y2 = mm_lib_duplunit(y1,(i64)0);
    z = (*p).b;
    leftop = (i64)((*w).tag);
    rightop = (i64)((*p).tag);
    (*p).tag = (i64)9;
    (*p).b = mm_lib_createunit2(rightop,y2,z);
    (*(*p).b).lineno = (i64)((*p).lineno);
    mm_name_rx_unitlist(owner,w);
    mm_name_rx_unitlist(owner,y2);
    mm_name_rx_unitlist(owner,z);
}

static struct mm_decls_unitrec * mm_name_copylistunit(struct mm_decls_unitrec * p) {
    struct mm_decls_unitrec *  q;
    struct mm_decls_unitrec *  plist;
    struct mm_decls_unitrec *  plistx;
    plist = (plistx = (struct mm_decls_unitrec *)(0));
    L846 :;
    while (!!(p)) {
        q = mm_name_copyunit(p);
        mm_lib_addlistunit(&plist,&plistx,q);
        p = (*p).nextunit;
L847 :;
    }L848 :;
    ;
    return plist;
}

static struct mm_decls_unitrec * mm_name_copyunit(struct mm_decls_unitrec * p) {
    struct mm_decls_unitrec *  q;
    struct mm_decls_strec *  d;
    i64 i;
    if ((p == 0)) {
        return (struct mm_decls_unitrec *)(0);
    };
    if (((i64)((*p).tag) == (i64)3)) {
        d = (*p).def;
        L849 :;
        for (i=(i64)1;i<=mm_name_nmacroparams;++i) {
L850 :;
            if ((mm_name_macroparamsgen[(i)-1] == d)) {
                return mm_name_copyunit(mm_name_macroargs[(i)-1]);
                goto L852 ;
            };
L851 :;
        }L852 :;
        ;
    };
    q = mm_lib_createunit0((i64)((*p).tag));
    (*q).a = mm_name_copylistunit((*p).a);
    (*q).b = mm_name_copylistunit((*p).b);
    (*q).c = mm_name_copylistunit((*p).c);
    (*q).lineno = (i64)((*p).lineno);
    (*q).value = (*p).value;
    (*q).opcode = (i64)((*p).opcode);
    (*q).mode = (i64)((*p).mode);
    (*q).newmode = (i64)((*p).newmode);
    (*q).moduleno = (i64)((*p).moduleno);
    (*q).isastring = (i64)((*p).isastring);
    (*q).nextunit = (struct mm_decls_unitrec *)(0);
    return q;
}

static void mm_name_replaceunit(struct mm_decls_unitrec * p,struct mm_decls_unitrec * q) {
    struct mm_decls_unitrec *  pnext;
    pnext = (*p).nextunit;
    memcpy(&(*p),q,92);
    (*p).nextunit = pnext;
}

static void mm_name_expandmacro(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    struct mm_decls_strec *  d;
    struct mm_decls_strec *  pm;
    struct mm_decls_unitrec *  pnew;
    i64 ignoreargs;
    if ((mm_name_macrolevels > (i64)10)) {
        mm_support_rxerror((byte*)"Too many macro levels (recursive macro?)",(struct mm_decls_unitrec *)(0));
    };
    d = (*a).def;
    pm = (*d).paramlist;
    mm_name_nmacroparams = (i64)0;
    L853 :;
    while (!!(pm)) {
        if ((mm_name_nmacroparams >= (i64)50)) {
            mm_support_rxerror((byte*)"macro param overflow",(struct mm_decls_unitrec *)(0));
        };
        mm_name_macroparams[(++mm_name_nmacroparams)-1] = pm;
        mm_name_macroparamsgen[(mm_name_nmacroparams)-1] = (*pm).nulldef;
        pm = (*pm).nextparam;
L854 :;
    }L855 :;
    ;
    mm_name_nmacroargs = (i64)0;
    L856 :;
    while (!!(b)) {
        if ((mm_name_nmacroargs >= (i64)50)) {
            mm_support_rxerror((byte*)"macro arg overflow",(struct mm_decls_unitrec *)(0));
        };
        mm_name_macroargs[(++mm_name_nmacroargs)-1] = b;
        b = (*b).nextunit;
L857 :;
    }L858 :;
    ;
    if ((mm_name_nmacroargs < mm_name_nmacroparams)) {
        mm_support_rxerror((byte*)"Too few macro args",(struct mm_decls_unitrec *)(0));
    };
    ignoreargs = (i64)0;
    if (((mm_name_nmacroargs > (i64)0) && (mm_name_nmacroparams == (i64)0))) {
        ignoreargs = (i64)1;
        mm_name_nmacroargs = (mm_name_nmacroparams = (i64)0);
    } else if ((mm_name_nmacroargs > mm_name_nmacroparams)) {
        mm_support_rxerror((byte*)"Too many macro args",(struct mm_decls_unitrec *)(0));
    };
    pnew = mm_name_copyunit((*d).code);
    if (!(!!(ignoreargs))) {
        mm_name_replaceunit(p,pnew);
    } else {
        (*p).a = pnew;
    };
}

static void mm_type_tpass(struct mm_decls_unitrec * p,i64 t,i64 lv) {
    struct mm_decls_unitrec *  a;
    struct mm_decls_unitrec *  b;
    i64 oldmlineno;
    i64 m;
    if ((p == 0)) {
        return;
    };
    if ((((lv == (i64)1) || (lv == (i64)2) || (lv == (i64)4)) && !(!!((u64)(mm_tables_refunitset[((i64)((*p).tag))]))))) {
        mm_support_txerror((byte*)"not allowed as lvalue",(struct mm_decls_unitrec *)(0));
    };
    oldmlineno = mm_decls_mlineno;
    mm_decls_mlineno = (i64)((*p).lineno);
    a = (*p).a;
    b = (*p).b;
    switch ((i64)((*p).tag)) {
    case 3:;
    {
        mm_type_tx_name(p,t,lv);
    }break;
    case 1:;
    case 6:;
    {
    }break;
    case 100:;
    {
        (*p).mode = (i64)4;
    }break;
    case 132:;
    case 131:;
    {
        mm_type_tx_bytesize(p,a);
    }break;
    case 37:;
    case 38:;
    {
        mm_type_tx_add(p,a,b);
    }break;
    case 39:;
    case 40:;
    case 41:;
    case 42:;
    case 54:;
    case 55:;
    {
        mm_type_tx_mul(p,a,b);
    }break;
    case 192:;
    case 194:;
    {
        mm_type_tx_assign(p,a,b,(i64)0);
    }break;
    case 23:;
    case 24:;
    {
        mm_type_tx_assign(p,a,b,t);
    }break;
    case 19:;
    {
        L859 :;
        while (!!(a)) {
            mm_type_tpass(a,(i64)21,(i64)0);
            a = (*a).nextunit;
L860 :;
        }L861 :;
        ;
    }break;
    case 91:;
    case 126:;
    {
        mm_type_tx_atan2(p,a,b);
    }break;
    case 47:;
    case 48:;
    {
        mm_type_tx_shl(p,a,b);
    }break;
    case 44:;
    case 45:;
    case 46:;
    {
        mm_type_tx_iand(p,a,b);
    }break;
    case 30:;
    case 31:;
    {
        mm_type_tx_eq(p,a,b);
    }break;
    case 32:;
    case 33:;
    case 35:;
    case 34:;
    {
        mm_type_tx_lt(p,a,b);
    }break;
    case 94:;
    {
        if (((i64)((*a).tag) == (i64)93)) {
            mm_lib_deleteunit(p,a);
            mm_lib_deleteunit(p,(*p).a);
            mm_type_tpass(p,t,(i64)0);
        } else {
            mm_type_tpass(a,(i64)21,(i64)1);
            (*p).mode = mm_lib_createrefmode((struct mm_decls_strec *)(0),(i64)((*a).mode),(i64)0);
        };
    }break;
    case 95:;
    {
        mm_type_tx_addroffirst(p,a,t);
    }break;
    case 144:;
    case 145:;
    case 146:;
    case 147:;
    case 148:;
    case 153:;
    case 154:;
    case 155:;
    case 156:;
    case 149:;
    {
        mm_type_tx_addto(p,a,b);
    }break;
    case 150:;
    case 151:;
    case 152:;
    {
        mm_type_tx_iandto(p,a,b);
    }break;
    case 196:;
    {
        mm_type_tx_if(p,a,b,(*p).c,t,lv);
    }break;
    case 197:;
    {
        mm_type_tx_longif(p,a,b,t,lv);
    }break;
    case 82:;
    {
        mm_type_tx_index(p,a,b,t,lv);
    }break;
    case 93:;
    {
        mm_type_tx_ptr(p,a,t,lv);
    }break;
    case 189:;
    case 25:;
    case 190:;
    case 26:;
    {
        mm_type_tx_callproc(p,a,b,t);
    }break;
    case 89:;
    {
        mm_type_tx_dot(p,a,b,lv);
    }break;
    case 108:;
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
    {
        mm_type_tx_sqrt(p,a);
    }break;
    case 111:;
    {
        mm_type_tx_sign(p,a);
    }break;
    case 92:;
    {
        mm_type_tx_power(p,a,b);
    }break;
    case 9:;
    case 10:;
    {
        mm_type_tx_andl(p,a,b);
    }break;
    case 103:;
    case 104:;
    case 109:;
    case 110:;
    {
        mm_type_tx_neg(p,a,b);
    }break;
    case 105:;
    {
        mm_type_tx_inot(p,a);
    }break;
    case 12:;
    {
        mm_type_tx_notl(p,a);
    }break;
    case 13:;
    {
        mm_type_tx_istruel(p,a);
    }break;
    case 96:;
    {
        mm_type_tx_convert(p,a,(i64)1);
    }break;
    case 99:;
    {
        mm_type_tx_typepun(p,a);
    }break;
    case 129:;
    {
        mm_type_tx_len(p,a);
    }break;
    case 127:;
    {
        mm_type_tx_lwb(p,a);
    }break;
    case 128:;
    {
        mm_type_tx_upb(p,a);
    }break;
    case 130:;
    {
        mm_type_tx_bounds(p,a);
    }break;
    case 81:;
    {
        mm_type_tx_sliceptr(p,a);
    }break;
    case 138:;
    case 139:;
    case 140:;
    case 141:;
    {
        mm_type_tx_preincr(p,a,t);
    }break;
    case 15:;
    {
        mm_type_tx_makerange(p,a,b);
    }break;
    case 16:;
    {
        mm_type_tx_makeset(p,a,t);
    }break;
    case 17:;
    {
        mm_type_tx_makedict(p,a,t);
    }break;
    case 220:;
    {
        mm_type_tx_swap(p,a,b);
    }break;
    case 221:;
    {
        mm_type_tx_select(p,a,b,(*p).c,t,lv);
    }break;
    case 218:;
    case 219:;
    {
        mm_type_tx_switch(p,a,b,(*p).c,t,lv);
    }break;
    case 216:;
    case 217:;
    {
        mm_type_tx_case(p,a,b,(*p).c,t,lv);
    }break;
    case 18:;
    {
        mm_type_tx_exprlist(p,a,t);
    }break;
    case 84:;
    case 85:;
    case 86:;
    {
        mm_type_tx_dotindex(p,a,b,lv);
    }break;
    case 83:;
    {
        mm_type_tx_slice(p,a,b);
    }break;
    case 136:;
    case 137:;
    {
        mm_type_tx_minvalue(p,a);
    }break;
    case 159:;
    case 160:;
    case 161:;
    {
        mm_type_tx_negto(p,a);
    }break;
    case 4:;
    case 5:;
    {
        mm_type_tx_block(p,a,t,lv);
    }break;
    case 240:;
    {
        mm_type_tpass(a,(i64)21,(i64)0);
        (*p).mode = (i64)((*a).mode);
    }break;
    case 215:;
    {
        mm_type_tpass(a,(i64)0,(i64)0);
    }break;
    case 191:;
    {
        mm_type_tx_return(p,a,t);
    }break;
    case 223:;
    case 224:;
    case 225:;
    case 226:;
    {
        mm_type_tx_unitlist(a,(i64)21,(i64)0);
        mm_type_tx_unitlist(b,(i64)21,(i64)0);
        mm_type_tx_unitlist((*p).c,(i64)21,(i64)0);
    }break;
    case 198:;
    case 199:;
    case 200:;
    {
        mm_type_tx_for(a,b,(*p).c);
    }break;
    case 195:;
    {
        mm_type_tpass(a,(i64)4,(i64)0);
        mm_type_tpass(b,(i64)0,(i64)0);
    }break;
    case 98:;
    {
        mm_type_tpass(a,(i64)21,(i64)0);
        mm_type_coerceunit(a,t,(i64)1);
        mm_lib_deleteunit(p,a);
    }break;
    case 14:;
    {
        mm_type_tx_makelist(p,a,t,lv);
    }break;
    case 235:;
    {
        mm_type_tpass(a,(i64)4,(i64)0);
    }break;
    case 214:;
    case 212:;
    case 211:;
    case 213:;
    {
        mm_type_tx_exit(p,a);
    }break;
    case 208:;
    {
        mm_type_tx_goto(p,a);
    }break;
    case 210:;
    {
    }break;
    case 206:;
    {
        mm_type_tcond(a);
        mm_type_tpass(b,(i64)0,(i64)0);
    }break;
    case 207:;
    {
        mm_type_tpass(a,(i64)0,(i64)0);
        mm_type_tcond(b);
    }break;
    case 188:;
    {
    }break;
    case 7:;
    {
        if ((t != (i64)0)) {
            (*p).mode = t;
        };
    }break;
    case 133:;
    {
        mm_type_tpass(a,(i64)21,(i64)0);
        if (((i64)((*a).tag) == (i64)100)) {
            (*p).value = (*a).value;
        } else {
            (*p).value = (i64)((*a).mode);
        };
        (*p).tag = (i64)100;
        (*p).mode = (i64)4;
        (*p).a = (struct mm_decls_unitrec *)(0);
    }break;
    case 134:;
    {
        mm_type_tpass(a,(i64)21,(i64)0);
        if (((i64)((*a).tag) == (i64)100)) {
            m = (*a).value;
        } else {
            mm_type_tpass(a,(i64)21,(i64)0);
            m = (i64)((*a).mode);
        };
        (*p).tag = (i64)1;
        (*p).mode = mm_tables_trefchar;
        (*p).svalue = mlib_pcm_copyheapstring(mm_lib_strmode(m,(i64)0));
        (*p).slength = (i64)(strlen((i8 *)((*p).svalue)));
        (*p).isastring = (i64)1;
        (*p).a = (struct mm_decls_unitrec *)(0);
    }break;
    case 97:;
    {
        mm_type_tpass(a,(i64)21,(i64)0);
    }break;
    case 187:;
    {
        mm_type_tpass(a,(i64)21,(i64)0);
        mm_type_tpass(b,(i64)21,(i64)0);
    }break;
    case 232:;
    {
        mm_type_tpass(a,(i64)21,(i64)0);
    }break;
    case 231:;
    {
        if (!!(a)) {
            mm_type_tpass(a,(i64)15,(i64)0);
        };
        if (!!((u64)(mm_decls_ttisnumeric[(t)]))) {
            t = (i64)(mm_tables_stdtypebase[((i64)(mm_decls_ttbasetype[(t)]))]);
        };
        (*p).mode = t;
    }break;
    case 49:;
    case 50:;
    {
        mm_type_tx_in(p,a,b);
    }break;
    case 222:;
    {
        mm_type_tpass(a,(i64)4,(i64)0);
        if (((i64)((*a).tag) != (i64)1)) {
            mm_support_txerror((byte*)"recase must be const",(struct mm_decls_unitrec *)(0));
        };
    }break;
    case 64:;
    case 65:;
    case 66:;
    case 67:;
    case 68:;
    case 69:;
    case 72:;
    case 62:;
    case 63:;
    case 77:;
    case 78:;
    case 79:;
    case 80:;
    case 70:;
    {
        mm_type_tx_head(p,a,b);
    }break;
    case 75:;
    case 60:;
    case 59:;
    {
        mm_type_tx_concat(p,a,b);
    }break;
    case 173:;
    {
        (*p).mode = (i64)4;
    }break;
    case 176:;
    case 175:;
    {
        (*p).mode = mm_tables_trefchar;
    }break;
    case 135:;
    {
        mm_type_tx_bitfield(p,a,lv);
    }break;
    case 242:;
    {
    }break;
    default: {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"TXUNIT: CAN'T DO:",NULL);
        msysc_m_print_str(mm_tables_jtagnames[((i64)((*p).tag))],NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        mm_type_tx_unitlist(a,t,(i64)0);
        mm_type_tx_unitlist(b,t,(i64)0);
        mm_type_tx_unitlist((*p).c,t,(i64)0);
    }
    } //SW
;
    mm_type_tevaluate(p);
    if (((i64)((*p).tag)==(i64)14) || ((i64)((*p).tag)==(i64)191)) {
    } else {
        if ((((t != (i64)21) && (t != (i64)0)) && ((i64)((*p).mode) != t))) {
            mm_type_coerceunit(p,t,(i64)0);
        };
    };
    if ((t == (i64)0)) {
        mm_type_fixvoidunit(p);
    };
    mm_decls_mlineno = oldmlineno;
}

static void mm_type_tx_block(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t,i64 lv) {
    L862 :;
    while ((!!(a) && !!((*a).nextunit))) {
        mm_type_tpass(a,(i64)0,(i64)0);
        a = (*a).nextunit;
L863 :;
    }L864 :;
    ;
    if (!!(a)) {
        mm_type_tx_unitlist(a,t,lv);
        (*p).mode = ((t != (i64)0)?(i64)((*a).mode):(i64)0);
    };
}

void mm_type_tx_typetable(void) {
    i64 i;
    L865 :;
    for (i=(i64)56;i<=mm_decls_ntypes;++i) {
L866 :;
        mm_type_setmodesize(i);
L867 :;
    }L868 :;
    ;
}

static void mm_type_setmodesize(i64 m) {
    if (!!((i64)(mm_decls_ttsize[(m)]))) {
        return;
    };
    mm_decls_mlineno = (i64)(mm_decls_ttlineno[(m)]);
    if (((i64)(mm_decls_ttbasetype[(m)])==(i64)29)) {
        mm_type_setarraysize(m);
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)32)) {
        mm_type_setrecordsize(m);
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)0) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)41)) {
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)28)) {
        mm_type_setslicesize(m);
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)40)) {
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)36)) {
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)37)) {
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)52)) {
    } else {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"SIZE 0:",NULL);
        msysc_m_print_str(mm_lib_strmode(m,(i64)1),NULL);
        msysc_m_print_i64(m,NULL);
        msysc_m_print_str(mm_tables_stdtypenames[((i64)(mm_decls_ttbasetype[(m)]))],NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"********",NULL);
        msysc_m_print_ptr(mm_decls_ttnamedef[(m)],NULL);
        msysc_m_print_ptr(mm_decls_ttowner[(m)],NULL);
        msysc_m_print_str((byte*)"M=",NULL);
        msysc_m_print_i64(m,NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"Can't set mode size",NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
    };
}

static void mm_type_setarraysize(i64 m) {
    i64 lower;
    i64 length;
    i64 elemsize;
    i64 target;
    struct mm_decls_unitrec *  pdim;
    struct mm_decls_unitrec *  a;
    struct mm_decls_unitrec *  b;
    if (!!((u64)(mm_decls_ttsizeset[(m)]))) {
        return;
    };
    pdim = mm_decls_ttdimexpr[(m)];
    if (!!(pdim)) {
        a = (*pdim).a;
        b = (*pdim).b;
        mm_name_rx_unit(mm_decls_ttowner[(m)],pdim);
        if (((i64)((*pdim).tag)==(i64)15)) {
            mm_type_tpass(a,(i64)21,(i64)0);
            mm_type_tpass(b,(i64)21,(i64)0);
            lower = mm_type_getconstint(a,(i64)21);
            length = ((mm_type_getconstint(b,(i64)21) - lower) + (i64)1);
        }else if (((i64)((*pdim).tag)==(i64)22)) {
            mm_type_tpass(a,(i64)21,(i64)0);
            lower = mm_type_getconstint(a,(i64)21);
            if (!!(b)) {
                mm_type_tpass(b,(i64)21,(i64)0);
                length = mm_type_getconstint(b,(i64)21);
            } else {
                length = (i64)0;
            };
        } else {
            mm_type_tpass(pdim,(i64)21,(i64)0);
            length = mm_type_getconstint(pdim,(i64)21);
            lower = (i64)1;
        };
    } else {
        lower = (i64)1;
        length = (i64)0;
    };
    mm_decls_ttdimexpr[(m)] = (struct mm_decls_unitrec *)(0);
    mm_decls_ttlower[(m)] = lower;
    mm_decls_ttlength[(m)] = length;
    target = (i64)(mm_decls_tttarget[(m)]);
    mm_type_setmodesize(target);
    elemsize = (i64)(mm_decls_ttsize[((i64)(mm_decls_tttarget[(m)]))]);
    mm_decls_ttsize[(m)] = (length * elemsize);
    mm_decls_ttsizeset[(m)] = (u64)((i64)1);
}

static void mm_type_setslicesize(i64 m) {
    struct mm_decls_unitrec *  pdim;
    if (!!((i64)(mm_decls_ttsize[(m)]))) {
        return;
    };
    pdim = mm_decls_ttdimexpr[(m)];
    if (!!(pdim)) {
        mm_name_rx_unit(mm_decls_ttowner[(m)],pdim);
        mm_type_tpass(pdim,(i64)21,(i64)0);
        mm_decls_ttlower[(m)] = mm_type_getconstint(pdim,(i64)21);
        mm_decls_ttdimexpr[(m)] = (struct mm_decls_unitrec *)(0);
    } else {
        mm_decls_ttlower[(m)] = (i64)1;
    };
    mm_type_setmodesize((i64)(mm_decls_tttarget[(m)]));
    mm_decls_ttsize[(m)] = (i64)(mm_decls_ttsize[((i64)28)]);
}

static void mm_type_tcond(struct mm_decls_unitrec * p) {
    struct mm_decls_unitrec *  a;
    struct mm_decls_unitrec *  b;
    a = (*p).a;
    b = (*p).b;
    if (((i64)((*p).tag)==(i64)9) || ((i64)((*p).tag)==(i64)10) || ((i64)((*p).tag)==(i64)11)) {
        mm_type_tcond(a);
        mm_type_tcond(b);
    }else if (((i64)((*p).tag)==(i64)12)) {
        mm_type_tcond(a);
        if (((i64)((*a).tag) == (i64)12)) {
            mm_lib_deleteunit(p,a);
            (*p).tag = (i64)13;
            if (!!((u64)(mm_tables_boolunitset[((i64)((*(*p).a).tag))]))) {
                mm_lib_deleteunit(p,(*p).a);
            };
        };
    }else if (((i64)((*p).tag)==(i64)13)) {
        mm_type_tpass(a,(i64)21,(i64)0);
        if (!!((u64)(mm_tables_boolunitset[((i64)((*a).tag))]))) {
            mm_lib_deleteunit(p,a);
        };
    } else {
        mm_type_tpass(p,(i64)21,(i64)0);
        mm_type_twidenopnd(p);
        if (!(!!((u64)(mm_tables_boolunitset[((i64)((*p).tag))])))) {
            mm_lib_insertunit(p,(i64)13);
        };
    };
    (*p).mode = (i64)4;
}

i64 mm_type_tx_module(i64 n) {
    mm_decls_currmoduleno = n;
    mm_type_tx_passdef(mm_decls_moduletable[(n)].stmodule);
    return (i64)1;
}

void mm_type_tx_passdef(struct mm_decls_strec * p) {
    struct mm_decls_strec *  d;
    i64 oldmlineno;
    i64 simplefunc;
    struct mm_decls_unitrec *  q;
    if (!!((u64)((*p).txdone))) {
        return;
    };
    oldmlineno = mm_decls_mlineno;
    mm_decls_mlineno = (i64)((*p).lineno);
    simplefunc = (i64)1;
    d = (*p).deflist;
    L869 :;
    while (!!(d)) {
        mm_type_tx_passdef(d);
        if (((((*p).nameid == (i64)5) || ((*p).nameid == (i64)10)) && ((i64)((u64)((*d).nameid)) == (i64)11))) {
            if (!(!!(mm_lib_issimpletype((i64)((*d).mode))))) {
                simplefunc = (i64)0;
            };
        };
        d = (*d).nextdef;
L870 :;
    }L871 :;
    ;
    q = (*p).code;
    if (((i64)((*p).nameid)==(i64)5)) {
        mm_decls_currproc = p;
        mm_type_tpass(q,(((i64)((u64)((*mm_decls_currproc).nretvalues)) > (i64)1)?(i64)55:(i64)((*p).mode)),(i64)0);
        if ((((i64)((u64)((*p).nretvalues)) > (i64)1) || !(!!(mm_lib_issimpletype((i64)((*p).mode)))))) {
            simplefunc = (i64)0;
        };
        (*p).simplefunc = (u64)(simplefunc);
        mm_decls_currproc = (struct mm_decls_strec *)(0);
    }else if (((i64)((*p).nameid)==(i64)8) || ((i64)((*p).nameid)==(i64)14)) {
        mm_type_tx_namedconst(p);
    }else if (((i64)((*p).nameid)==(i64)9) || ((i64)((*p).nameid)==(i64)10) || ((i64)((*p).nameid)==(i64)11)) {
        mm_type_tx_namedef(p);
    };
    (*p).txdone = (u64)((i64)1);
    mm_decls_mlineno = oldmlineno;
}

static void mm_type_tx_unitlist(struct mm_decls_unitrec * p,i64 t,i64 lv) {
    L872 :;
    while (!!(p)) {
        mm_type_tpass(p,t,(i64)0);
        p = (*p).nextunit;
L873 :;
    }L874 :;
    ;
}

static void mm_type_tx_namedef(struct mm_decls_strec * d) {
    i64 m;
    struct mm_decls_unitrec *  dcode;
    m = (i64)((*d).mode);
    mm_type_setmodesize(m);
    if (!!((u64)((*d).circflag))) {
        mm_support_txerror((byte*)"Circular reference detected",(struct mm_decls_unitrec *)(0));
    };
    if (!!((u64)((*d).txdone))) {
        return;
    };
    dcode = (*d).code;
    (*d).circflag = (u64)((i64)1);
    if (((i64)((u64)((*d).at)) == (i64)1)) {
        mm_type_tpass((*d).equivvar,(i64)20,(i64)0);
    };
    if ((!!(dcode) && ((i64)((u64)((*d).nameid)) != (i64)10))) {
        if (((((i64)(mm_decls_ttbasetype[(m)]) == (i64)28) && ((i64)((*dcode).tag) == (i64)1)) && ((i64)((*dcode).mode) == mm_tables_trefchar))) {
            mm_type_tpass(dcode,mm_tables_trefchar,(i64)0);
        } else {
            mm_type_tpass(dcode,m,(i64)0);
        };
        (*d).circflag = (u64)((i64)0);
        (*d).txdone = (u64)((i64)1);
        if (((i64)((u64)((*d).nameid)) == (i64)9)) {
            mm_type_checkconstexpr((*d).code);
        };
        if ((((i64)(mm_decls_ttbasetype[(m)]) == (i64)29) && ((i64)(mm_decls_ttlength[(m)]) == (i64)0))) {
            (*d).mode = (i64)((*dcode).mode);
        };
    } else {
        (*d).circflag = (u64)((i64)0);
        (*d).txdone = (u64)((i64)1);
    };
}

void mm_type_tx_namedconst(struct mm_decls_strec * d) {
    i64 m;
    struct mm_decls_unitrec *  q;
    if (!!((u64)((*d).circflag))) {
        mm_support_txerror((byte*)"Circular const reference detected",(struct mm_decls_unitrec *)(0));
    };
    if (!!((u64)((*d).txdone))) {
        return;
    };
    q = (*d).code;
    m = (i64)((*d).mode);
    (*d).circflag = (u64)((i64)1);
    mm_type_tx_expr(q,((m == (i64)52)?(i64)21:m));
    (*d).circflag = (u64)((i64)0);
    mm_type_checkconstexpr(q);
    if ((m == (i64)52)) {
        (*d).mode = (i64)((*q).mode);
    };
    (*d).txdone = (u64)((i64)1);
}

static void mm_type_tx_expr(struct mm_decls_unitrec * p,i64 t) {
    mm_type_tpass(p,t,(i64)0);
}

static void mm_type_checkconstexpr(struct mm_decls_unitrec * p) {
    struct mm_decls_unitrec *  q;
    if (((i64)((*p).tag)==(i64)1)) {
        return;
    }else if (((i64)((*p).tag)==(i64)14)) {
        q = (*p).a;
        L875 :;
        while (!!(q)) {
            mm_type_checkconstexpr(q);
            q = (*q).nextunit;
L876 :;
        }L877 :;
        ;
    }else if (((i64)((*p).tag)==(i64)97)) {
        if (((i64)(mm_decls_tttarget[((i64)((*(*p).a).mode))]) == (i64)0)) {
            (*(*p).a).mode = (i64)((*p).mode);
            mm_lib_deleteunit(p,(*p).a);
        } else {
            goto L878 ;
;
        };
    }else if (((i64)((*p).tag)==(i64)96)) {
        if (((i64)((*p).opcode)==(i64)1) || ((i64)((*p).opcode)==(i64)0) || ((i64)((*p).opcode)==(i64)28)) {
        } else {
            goto L878 ;
;
        };
    }else if (((i64)((*p).tag)==(i64)94)) {
        if (((i64)((*(*p).a).tag)==(i64)3)) {
        } else {
            goto L878 ;
;
        };
    } else {
        //error:
L878 :;
;
        msysc_m_print_startcon();
        msysc_m_print_str(mm_tables_jtagnames[((i64)((*p).tag))],NULL);
        msysc_m_print_str(mm_lib_strmode((i64)((*p).mode),(i64)1),NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        mm_diags_printunit(p,(i64)0,(byte*)"*",0);
        mm_support_txerror((byte*)"Getconstexpr: not const",(struct mm_decls_unitrec *)(0));
    };
}

static i64 mm_type_getconstint(struct mm_decls_unitrec * q,i64 t) {
    mm_type_checkconstexpr(q);
    if (((i64)(mm_decls_tttypecode[((i64)((*q).mode))])==(i64)73) || ((i64)(mm_decls_tttypecode[((i64)((*q).mode))])==(i64)85)) {
        if (((i64)(mm_decls_ttsize[((i64)((*q).mode))]) == (i64)16)) {
            mm_support_gerror((byte*)"GETCONSTINT/128",(struct mm_decls_unitrec *)(0));
        };
        return (*q).value;
    }else if (((i64)(mm_decls_tttypecode[((i64)((*q).mode))])==(i64)82)) {
        return (i64)((*q).xvalue);
    } else {
        msysc_m_print_startcon();
        msysc_m_print_str(mm_lib_strmode((i64)((*q).mode),(i64)1),NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        mm_support_txerror((byte*)"Getconstint: not int32/64",(struct mm_decls_unitrec *)(0));
    };
    return (i64)0;
}

static void mm_type_tevaluate(struct mm_decls_unitrec * p) {
    struct mm_decls_unitrec *  a;
    struct mm_decls_unitrec *  b;
    i64 tag = (i64)((*p).tag);
    tag = (i64)((*p).tag);
    if (!!((u64)(mm_tables_binopset[(tag)]))) {
        mm_type_tevalbinop(p);
    } else if (!!((u64)(mm_tables_monopset[(tag)]))) {
        mm_type_tevalmonop(p);
    } else {
        if ((tag==(i64)15)) {
            a = (*p).a;
            b = (*p).b;
            if (((i64)(mm_decls_ttsize[((i64)((*a).mode))]) <= (i64)8)) {
                mm_type_tevaluate(a);
                mm_type_tevaluate(b);
                if ((((i64)((*a).tag) == (i64)1) && ((i64)((*b).tag) == (i64)1))) {
                    (*p).isconst = ((i64)((*a).isconst) & (i64)((*b).isconst));
                };
            };
        }else if ((tag==(i64)96)) {
            mm_type_tevalconvert(p);
        };
    };
}

static void mm_type_tevalbinop(struct mm_decls_unitrec * p) {
    i64 a;
    i64 b;
    i64 c;
    double x;
    double y;
    double z;
    if ((((i64)((*(*p).a).tag) != (i64)1) || ((i64)((*(*p).b).tag) != (i64)1))) {
        return;
    };
    if (((i64)(mm_decls_ttsize[((i64)((*p).mode))]) > (i64)8)) {
        return;
    };
    if (((i64)(mm_decls_tttypecode[((i64)((*p).mode))])==(i64)73)) {
        a = (*(*p).a).value;
        b = (*(*p).b).value;
        switch ((i64)((*p).tag)) {
        case 37:;
        {
            c = (a + b);
        }break;
        case 38:;
        {
            c = (a - b);
        }break;
        case 39:;
        {
            c = (a * b);
        }break;
        case 40:;
        case 41:;
        {
            if ((b == (i64)0)) {
                mm_support_txerror((byte*)"div by 0",(struct mm_decls_unitrec *)(0));
            };
            c = (a / b);
        }break;
        case 42:;
        {
            if ((b == (i64)0)) {
                mm_support_txerror((byte*)"div by 0",(struct mm_decls_unitrec *)(0));
            };
            c = (a % b);
        }break;
        case 47:;
        {
            c = (a << b);
        }break;
        case 48:;
        {
            c = (a >> b);
        }break;
        case 44:;
        {
            c = (a & b);
        }break;
        case 45:;
        {
            c = (a | b);
        }break;
        case 46:;
        {
            c = (a ^ b);
        }break;
        default: {
            return;
        }
        } //SW
;
        mm_type_makenewconst(p,c,(i64)0);
    }else if (((i64)(mm_decls_tttypecode[((i64)((*p).mode))])==(i64)82)) {
        x = (*(*p).a).xvalue;
        y = (*(*p).b).xvalue;
        switch ((i64)((*p).tag)) {
        case 37:;
        {
            z = (x + y);
        }break;
        case 38:;
        {
            z = (x - y);
        }break;
        case 39:;
        {
            z = (x * y);
        }break;
        case 40:;
        {
            if ((y == (double)0.)) {
                mm_support_txerror((byte*)"div by 0",(struct mm_decls_unitrec *)(0));
            };
            z = (x / y);
        }break;
        default: {
            return;
        }
        } //SW
;
        mm_type_makenewconst(p,*(i64*)&z,(i64)0);
    };
}

static void mm_type_tevalmonop(struct mm_decls_unitrec * p) {
    struct mm_decls_unitrec *  a;
    i64 ix;
    i64 iz;
    double x;
    double z;
    a = (*p).a;
    if (((i64)(mm_decls_ttsize[((i64)((*p).mode))]) > (i64)8)) {
        return;
    };
    if (((i64)((*a).tag) != (i64)1)) {
        if (((i64)((*p).tag)==(i64)132)) {
            if (((i64)((*a).tag) == (i64)100)) {
                mm_type_makenewconst(p,(i64)(mm_decls_ttsize[((*a).value)]),(i64)0);
            } else {
                mm_type_makenewconst(p,(i64)(mm_decls_ttsize[((i64)((*a).mode))]),(i64)0);
            };
        }else if (((i64)((*p).tag)==(i64)131)) {
            if (((i64)((*a).tag) == (i64)100)) {
                mm_type_makenewconst(p,(i64)(mm_decls_ttbitwidth[((*a).value)]),(i64)0);
            } else {
                mm_type_makenewconst(p,(i64)(mm_decls_ttbitwidth[((i64)((*a).mode))]),(i64)0);
            };
        };
        return;
    };
    if (((i64)(mm_decls_tttypecode[((i64)((*p).mode))])==(i64)73) || ((i64)(mm_decls_tttypecode[((i64)((*p).mode))])==(i64)85)) {
        ix = (*a).value;
        switch ((i64)((*p).tag)) {
        case 103:;
        {
            iz = -(ix);
        }break;
        case 105:;
        {
            iz = ~(ix);
        }break;
        case 12:;
        {
            iz = !(ix);
        }break;
        case 104:;
        {
            iz = labs(ix);
        }break;
        case 132:;
        {
            iz = (i64)(mm_decls_ttsize[((i64)((*p).mode))]);
        }break;
        default: {
            return;
        }
        } //SW
;
        mm_type_makenewconst(p,iz,(i64)0);
    }else if (((i64)(mm_decls_tttypecode[((i64)((*p).mode))])==(i64)82)) {
        x = (*a).xvalue;
        switch ((i64)((*p).tag)) {
        case 103:;
        {
            z = -(x);
        }break;
        default: {
            return;
        }
        } //SW
;
        mm_type_makenewconst(p,*(i64*)&z,(i64)0);
    };
}

static void mm_type_tevalconvert(struct mm_decls_unitrec * p) {
    struct mm_decls_unitrec *  q;
    i64 a;
    i64 b;
    u64 u;
    double x;
    i64 s;
    i64 t;
    void *  pvoid;
    q = (*p).a;
    mm_type_tevaluate(q);
    s = (i64)((*q).mode);
    if (((i64)((*p).opcode)==(i64)1)) {
        //dosoft:
L879 :;
;
        if (!(!!(mm_decls_ctarget))) {
            //delmode:
L880 :;
;
            (*q).mode = (i64)((*p).mode);
            mm_lib_deleteunit(p,q);
            return;
        } else if (((((*p).mode == (i64)4) || ((*p).mode == (i64)9)) && ((i64)((*q).mode) == (i64)15))) {
            goto L880 ;
;
        };
    }else if (((i64)((*p).opcode)==(i64)25)) {
        if (!(!!(mm_decls_ctarget))) {
            goto L879 ;
;
        };
        (*p).tag = (i64)97;
        return;
    }else if (((i64)((*p).opcode)==(i64)23) || ((i64)((*p).opcode)==(i64)24)) {
        goto L879 ;
;
    };
    if (((i64)((*q).tag) != (i64)1)) {
        return;
    };
    if ((s == mm_tables_trefchar)) {
        return;
    };
    t = (i64)(mm_decls_ttbasetype[((i64)((*p).newmode))]);
    if ((s == t)) {
        mm_lib_deleteunit(p,q);
    };
    x = (*q).xvalue;
    a = (*q).value;
    u = (*q).uvalue;
    if (((i64)((*p).opcode)==(i64)19) || ((i64)((*p).opcode)==(i64)20)) {
        if (((s == (i64)5) || (s == (i64)10))) {
            a = (i64)((*(*q).qvalue).lower);
        };
        if ((t==(i64)6) || (t==(i64)13)) {
            b = (a & (i64)255);
        }else if ((t==(i64)7) || (t==(i64)14)) {
            b = (a & (i64)65535);
        }else if ((t==(i64)8)) {
            b = (a & (i64)4294967295);
        }else if ((t==(i64)9) || (t==(i64)4) || (t==(i64)15)) {
            b = a;
        }else if ((t==(i64)1)) {
            b = (i64)((i8)((a & (i64)255)));
        }else if ((t==(i64)2)) {
            b = (i64)((i16)((a & (i64)65535)));
        }else if ((t==(i64)3)) {
            b = (i64)((i32)((a & (i64)4294967295)));
        } else {
            msysc_m_print_startcon();
            msysc_m_print_str((byte*)"STRMODE(S)=",NULL);
            msysc_m_print_str(mm_lib_strmode(s,(i64)1),NULL);
            msysc_m_print_str((byte*)"=>",NULL);
            msysc_m_print_str(mm_lib_strmode(t,(i64)1),NULL);
            msysc_m_print_newline();
            msysc_m_print_end();
            ;
            mm_support_txerror((byte*)"EVALC/TRUNC",(struct mm_decls_unitrec *)(0));
        };
        mm_type_makenewconst(p,b,(i64)0);
    }else if (((i64)((*p).opcode)==(i64)4) || ((i64)((*p).opcode)==(i64)5)) {
        if ((t==(i64)5) || (t==(i64)10)) {
            pvoid = (void *)(mm_lib_makeqvalue(a,(i64)(mm_decls_tttypecode[(s)])));
            mm_type_makenewconst(p,*(i64*)&pvoid,(i64)0);
        } else {
            mm_type_makenewconst(p,(i64)(u),(i64)0);
        };
    }else if (((i64)((*p).opcode)==(i64)6)) {
        x = (double)(a);
        mm_type_makenewconst(p,*(i64*)&x,(i64)0);
    }else if (((i64)((*p).opcode)==(i64)7)) {
        x = (double)(a);
        mm_type_makenewconst(p,*(i64*)&x,(i64)0);
    }else if (((i64)((*p).opcode)==(i64)8)) {
        a = ((*p).value = (i64)(x));
        mm_type_makenewconst(p,(i64)(x),(i64)0);
    }else if (((i64)((*p).opcode)==(i64)9)) {
        mm_support_txerror((byte*)"UFIX",(struct mm_decls_unitrec *)(0));
    }else if (((i64)((*p).opcode)==(i64)22)) {
        mm_support_txerror((byte*)"EVALC/FWIDEN",(struct mm_decls_unitrec *)(0));
    }else if (((i64)((*p).opcode)==(i64)21)) {
        mm_type_makenewconst(p,*(i64*)&x,(i64)11);
    }else if (((i64)((*p).opcode)==(i64)18)) {
        mm_support_txerror((byte*)"EVALC/NARROW",(struct mm_decls_unitrec *)(0));
    } else {
    };
}

static void mm_type_makenewconst(struct mm_decls_unitrec * p,i64 x,i64 t) {
    (*p).tag = (i64)1;
    (*p).value = x;
    (*p).a = (struct mm_decls_unitrec *)(0);
    (*p).b = (struct mm_decls_unitrec *)(0);
    (*p).isconst = (i64)1;
    if ((t != (i64)0)) {
        (*p).mode = t;
    };
}

static void mm_type_tx_name(struct mm_decls_unitrec * p,i64 t,i64 lv) {
    struct mm_decls_strec *  d;
    i64 oldmlineno;
    struct mm_decls_unitrec *  pcode;
    oldmlineno = mm_decls_mlineno;
    d = (*p).def;
    mm_decls_mlineno = (i64)((*d).lineno);
    switch ((i64)((*d).nameid)) {
    case 8:;
    case 14:;
    {
        if (!!(lv)) {
            mm_support_txerror((byte*)"&const",(struct mm_decls_unitrec *)(0));
        };
        mm_type_tx_namedconst(d);
        pcode = (*d).code;
        (*p).tag = (i64)1;
        (*p).def = (struct mm_decls_strec *)(0);
        (*p).a = (struct mm_decls_unitrec *)(0);
        (*p).c = (struct mm_decls_unitrec *)(0);
        if (((i64)((*pcode).tag) == (i64)96)) {
            (*p).value = (*(*pcode).a).value;
        } else {
            (*p).value = (*pcode).value;
        };
        (*p).slength = (i64)((*pcode).slength);
        (*p).mode = (i64)((*d).mode);
        (*p).isconst = (i64)1;
        (*p).isastring = (i64)((*pcode).isastring);
    }break;
    case 9:;
    case 10:;
    case 11:;
    {
        if ((!!((u64)((*d).islet)) && !!(lv))) {
            mm_support_txerror((byte*)"Can't use 'let' var as lvalue",(struct mm_decls_unitrec *)(0));
        };
        mm_type_tx_namedef(d);
        (*p).mode = (i64)((*d).mode);
        mm_type_twiden(p,lv);
    }break;
    case 5:;
    case 6:;
    {
        (*p).mode = mm_tables_trefproc;
    }break;
    case 15:;
    case 16:;
    {
        (*p).mode = mm_tables_treflabel;
    }break;
    case 2:;
    {
        mm_support_txerror_s((byte*)"Module name can't be used on it's own: #",(*d).name,(struct mm_decls_unitrec *)(0));
    }break;
    case 12:;
    {
        (*p).mode = (i64)((*d).mode);
    }break;
    case 4:;
    {
        (*p).tag = (i64)100;
        (*p).value = (i64)((*d).mode);
        (*p).mode = (i64)4;
    }break;
    case 7:;
    {
        if (!!((*d).code)) {
            mm_support_txerror((byte*)"Can't init dllvar",(struct mm_decls_unitrec *)(0));
        };
        (*p).mode = (i64)((*d).mode);
    }break;
    default: {
        mm_support_txerror_ss((byte*)"TNAME? # #",mm_tables_namenames[((i64)((*d).nameid))],(*d).name);
    }
    } //SW
;
    mm_decls_mlineno = oldmlineno;
}

static void mm_type_getdominantmode(i64 tag,i64 s,i64 t,i64 * u,i64 * v) {
    i64 sbase = (i64)(mm_decls_ttbasetype[(s)]);
    i64 tbase = (i64)(mm_decls_ttbasetype[(t)]);
    sbase = (i64)(mm_decls_ttbasetype[(s)]);
    tbase = (i64)(mm_decls_ttbasetype[(t)]);
    if (((sbase <= (i64)21) && (tbase <= (i64)21))) {
        (*u) = (i64)(mm_tables_dominantmode[(sbase)][(tbase)]);
        if (((*u) == (i64)20)) {
            (*u) = s;
        };
        (*v) = (*u);
        return;
    };
    (*u) = ((*v) = s);
    if ((s==(i64)50)) {
    }else if ((s==(i64)34) || (s==(i64)36) || (s==(i64)37)) {
        if ((!!(mm_lib_isintmode(tbase)) && (tag == (i64)39))) {
            (*v) = (i64)4;
        };
    } else {
        (*u) = ((*v) = t);
        if ((tbase==(i64)50) || (tbase==(i64)34) || (tbase==(i64)36) || (tbase==(i64)37)) {
        } else {
            (*u) = ((*v) = (i64)0);
        };
    };
}

static void mm_type_getdominantmodepp(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,i64 * u,i64 * v) {
    i64 abase;
    i64 bbase;
    i64 amode;
    i64 bmode;
    i64 tag;
    abase = (i64)(mm_decls_ttbasetype[((amode = (i64)((*a).mode)))]);
    bbase = (i64)(mm_decls_ttbasetype[((bmode = (i64)((*b).mode)))]);
    (*u) = ((*v) = amode);
    tag = (i64)((*p).tag);
    if ((abase == (i64)20)) {
        if ((bbase == (i64)20)) {
            switch (tag) {
            case 30:;
            case 31:;
            {
                if ((((i64)(mm_decls_tttarget[(amode)]) == (i64)0) || ((i64)(mm_decls_tttarget[(bmode)]) == (i64)0))) {
                    return;
                };
            }break;
            case 32:;
            case 33:;
            case 35:;
            case 34:;
            case 196:;
            {
            }break;
            case 38:;
            {
                (*p).tag = (i64)56;
            }break;
            default: {
                if (((((((((i64)((*p).tag) == (i64)37) && (amode == mm_tables_trefchar)) && (bmode == mm_tables_trefchar)) && ((i64)((*a).tag) == (i64)1)) && ((i64)((*b).tag) == (i64)1)) && !!((i64)((*a).isastring))) && !!((i64)((*b).isastring)))) {
                    mm_type_joinstrings(p,a,b);
                    return;
                };
                mm_support_txerror((byte*)"ref+ref",(struct mm_decls_unitrec *)(0));
            }
            } //SW
;
            if (!(!!(mm_type_comparemodes(amode,bmode)))) {
                (*u) = ((*v) = (i64)0);
            };
        } else if (!!((u64)(mm_decls_ttisinteger[(bbase)]))) {
            if (!(((tag == (i64)37) || (tag == (i64)38)))) {
                mm_support_txerror((byte*)"ref+T",(struct mm_decls_unitrec *)(0));
            };
            (*p).tag = ((tag == (i64)37)?(i64)57:(i64)58);
            (*v) = (i64)4;
        } else if (((bbase == (i64)34) && (amode == mm_tables_trefchar))) {
            (*u) = ((*v) = bmode);
        } else {
            (*u) = ((*v) = (i64)0);
        };
        return;
    } else if (((abase <= (i64)21) && (abase <= (i64)21))) {
        if (!!((u64)(mm_decls_ttisshortint[(abase)]))) {
            abase = mm_type_twidenshort(a);
        };
        if (!!((u64)(mm_decls_ttisshortint[(bbase)]))) {
            bbase = mm_type_twidenshort(b);
        };
        (*u) = (i64)(mm_tables_dominantmode[(abase)][(bbase)]);
        if (((*u) == (i64)20)) {
            (*u) = amode;
        };
        (*v) = (*u);
        return;
    };
    if ((amode==(i64)50)) {
    }else if ((amode==(i64)34) || (amode==(i64)36) || (amode==(i64)37)) {
        if (!!(mm_lib_isintmode(bbase))) {
            if ((tag == (i64)39)) {
                (*v) = (i64)4;
            };
        };
    } else {
        (*u) = ((*v) = bmode);
        if ((bbase==(i64)50) || (bbase==(i64)34) || (bbase==(i64)36) || (bbase==(i64)37)) {
        } else {
            (*u) = ((*v) = (i64)0);
        };
    };
}

static void mm_type_coerceunit(struct mm_decls_unitrec * p,i64 t,i64 hard) {
    i64 s;
    i64 sbase;
    i64 tbase;
    i64 cc;
    i64 starget;
    i64 ttarget;
    i64 result;
    if ((t == (i64)0)) {
        return;
    };
    s = (i64)((*p).mode);
    //retry:
L881 :;
;
    if ((s == t)) {
        return;
    };
    if (!!(mm_type_comparemodes(s,t))) {
        return;
    };
    if (((s == (i64)0) && (t != (i64)0))) {
        mm_support_txerror((byte*)"Void type not allowed in expr",(struct mm_decls_unitrec *)(0));
    };
    sbase = (i64)(mm_decls_ttbasetype[(s)]);
    tbase = (i64)(mm_decls_ttbasetype[(t)]);
    result = t;
    if (((sbase >= (i64)21) || (tbase >= (i64)21))) {
        cc = (i64)0;
        if ((tbase==(i64)34)) {
            if (((i64)((*p).mode) == mm_tables_trefchar)) {
                cc = (i64)28;
            };
        }else if ((tbase==(i64)16)) {
            cc = (i64)27;
        }else if ((tbase==(i64)50)) {
            cc = (i64)26;
        } else {
            if ((sbase==(i64)29)) {
                if ((tbase==(i64)28)) {
                    mm_type_twholearrayslice(p,t);
                    return;
                };
            }else if ((sbase==(i64)16)) {
                cc = (i64)30;
            }else if ((sbase==(i64)50)) {
                cc = (i64)29;
            }else if ((sbase==(i64)55)) {
                if ((((i64)((*p).tag) == (i64)25) && ((i64)((*(*p).a).tag) == (i64)3))) {
                    s = (i64)((*(*(*p).a).def).mode);
                    goto L881 ;
;
                } else {
                    mm_support_txerror((byte*)"coerce/mult",(struct mm_decls_unitrec *)(0));
                };
            };
        };
        if ((cc == (i64)0)) {
            if (!(!!(hard))) {
                mm_support_txerror_ss((byte*)"Explicit cast needed: # => #",mm_lib_strmode(s,(i64)1),mm_lib_strmode2(t,(i64)1));
            } else {
                cc = (i64)2;
            };
        };
    } else if ((!!((u64)(mm_decls_ttisinteger[(sbase)])) && !!((u64)(mm_decls_ttisshortint[(tbase)])))) {
        cc = (!!(hard)?(i64)20:(i64)19);
    } else if ((!!((u64)(mm_decls_ttisshortint[(sbase)])) && !!((u64)(mm_decls_ttisinteger[(tbase)])))) {
        mm_type_twidenopnd(p);
        (*p).mode = (i64)(((*p).newmode = t));
        mm_type_tevalconvert(p);
        return;
    } else if ((!!((u64)(mm_decls_ttisshortint[(sbase)])) && !!((u64)(mm_decls_ttisreal[(tbase)])))) {
        cc = (!!((u64)(mm_decls_ttisint[(sbase)]))?(i64)6:(i64)7);
        goto L882 ;
;
    } else if ((!!((u64)(mm_decls_ttisreal[(sbase)])) && !!((u64)(mm_decls_ttisshortint[(tbase)])))) {
        cc = (!!((u64)(mm_decls_ttisint[(tbase)]))?(i64)8:(i64)9);
    } else {
        cc = (i64)(mm_tables_conversionops[(sbase)][(tbase)]);
        //gotcc:
L882 :;
;
        if ((cc==(i64)32)) {
            mm_support_txerror_ss((byte*)"Conversion not allowed: # => #",mm_lib_strmode(s,(i64)1),mm_lib_strmode2(t,(i64)1));
        }else if ((cc==(i64)0)) {
        }else if ((cc==(i64)25)) {
            starget = (i64)(mm_decls_tttarget[(s)]);
            ttarget = (i64)(mm_decls_tttarget[(t)]);
            if ((starget == ttarget)) {
                return;
            } else if (((starget == (i64)0) || (ttarget == (i64)0))) {
                if ((!!(mm_decls_ctarget) && (starget != (i64)0))) {
                    cc = (i64)1;
                };
            } else if (!(!!(hard))) {
                if (!(!!(mm_type_comparemodes(s,t)))) {
                    if (((!!(mm_decls_ctarget) && (starget == (i64)13)) && (ttarget == (i64)1))) {
                        cc = (i64)1;
                    } else if (((!!(mm_decls_ctarget) && (starget == (i64)1)) && (ttarget == (i64)13))) {
                        cc = (i64)1;
                    } else {
                        msysc_m_print_startcon();
                        msysc_m_print_str(mm_lib_strmode(s,(i64)1),NULL);
                        msysc_m_print_str((byte*)"||",NULL);
                        msysc_m_print_str(mm_lib_strmode2(t,(i64)1),NULL);
                        msysc_m_print_newline();
                        msysc_m_print_end();
                        ;
                        mm_support_txerror((byte*)"ref->ref needs explicit cast",(struct mm_decls_unitrec *)(0));
                    };
                };
            };
        }else if ((cc==(i64)23) || (cc==(i64)24)) {
            if (!(!!(hard))) {
                mm_support_txerror_ss((byte*)"ref<=>int need explicit cast: # => #",mm_lib_strmode(s,(i64)1),mm_lib_strmode2(t,(i64)1));
            };
        };
    };
    if ((cc == (i64)0)) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"COERCEUNIT",NULL);
        msysc_m_print_str(mm_lib_strmode(s,(i64)1),NULL);
        msysc_m_print_str(mm_lib_strmode(t,(i64)1),NULL);
        msysc_m_print_i64(hard,NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        mm_diags_printunit(p,(i64)0,(byte*)"*",0);
        mm_support_txerror((byte*)"CONV CODE=0",(struct mm_decls_unitrec *)(0));
    };
    mm_lib_insertunit(p,(i64)96);
    if (((cc == (i64)19) && !!(hard))) {
        cc = (i64)20;
    };
    (*p).opcode = cc;
    (*p).newmode = t;
    (*p).mode = result;
    mm_type_tevalconvert(p);
}

static void mm_type_tx_add(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    i64 u;
    i64 v;
    mm_type_tpass(a,(i64)21,(i64)0);
    mm_type_tpass(b,(i64)21,(i64)0);
    mm_type_getdominantmodepp(p,a,b,&u,&v);
    if (((i64)((*p).tag) == (i64)1)) {
        return;
    };
    mm_type_coerceunit(a,u,(i64)0);
    mm_type_coerceunit(b,v,(i64)0);
    (*p).mode = u;
    if ((u == (i64)0)) {
        mm_support_txerror((byte*)"add/no dom",p);
    };
    mm_type_coerceunit(a,u,(i64)0);
    mm_type_coerceunit(b,v,(i64)0);
    (*p).mode = u;
    if ((((i64)((*p).tag) == (i64)56) && !!((u64)(mm_decls_ttisref[(v)])))) {
        (*p).mode = (i64)4;
    };
    if (((i64)(mm_decls_ttbasetype[(u)]) == (i64)20)) {
        if (((!(!!(mm_decls_ctarget)) && ((i64)((*b).tag) == (i64)1)) && ((i64)(mm_decls_ttbasetype[((i64)((*b).mode))]) != (i64)20))) {
            (*b).value *= (i64)(mm_decls_ttsize[((i64)(mm_decls_tttarget[((i64)((*a).mode))]))]);
            if (((i64)((*p).tag)==(i64)57)) {
                (*p).tag = (i64)37;
            }else if (((i64)((*p).tag)==(i64)58)) {
                (*p).tag = (i64)38;
            };
        };
    };
}

static void mm_type_tx_mul(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    i64 u;
    i64 v;
    mm_type_tpass(a,(i64)21,(i64)0);
    mm_type_tpass(b,(i64)21,(i64)0);
    mm_type_getdominantmodepp(p,a,b,&u,&v);
    if ((u == (i64)0)) {
        mm_support_txerror((byte*)"Bad mul/div/rem types",p);
    };
    mm_type_coerceunit(a,u,(i64)0);
    mm_type_coerceunit(b,v,(i64)0);
    (*p).mode = u;
    if ((((u64)(mm_tables_stdtypecode[(u)]) != (u64)82u) && ((i64)((*p).tag) == (i64)40))) {
        (*p).tag = (i64)41;
    };
}

static void mm_type_tx_shl(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    mm_type_tpass(a,(i64)21,(i64)0);
    mm_type_tpass(b,(i64)21,(i64)0);
    mm_type_twidenopnd(a);
    if (!(!!(mm_lib_isintmode((i64)((*a).mode))))) {
        mm_support_txerror((byte*)"SHL/not int",(struct mm_decls_unitrec *)(0));
    };
    mm_type_coerceunit(b,(i64)4,(i64)0);
    (*p).mode = (i64)((*a).mode);
}

static void mm_type_tx_iand(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    i64 u;
    i64 v;
    mm_type_tpass(a,(i64)21,(i64)0);
    mm_type_tpass(b,(i64)21,(i64)0);
    mm_type_getdominantmodepp(p,a,b,&u,&v);
    if (!(!!((u64)(mm_decls_ttisinteger[(u)])))) {
        mm_support_txerror((byte*)"IAND/not int",(struct mm_decls_unitrec *)(0));
    };
    mm_type_coerceunit(a,u,(i64)0);
    mm_type_coerceunit(b,u,(i64)0);
    (*p).mode = u;
}

static void mm_type_tx_eq(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    i64 u;
    i64 v;
    mm_type_tpass(a,(i64)21,(i64)0);
    mm_type_tpass(b,(i64)21,(i64)0);
    if (((!!((u64)(mm_decls_ttisref[((i64)((*a).mode))])) && !!((u64)(mm_decls_ttisref[((i64)((*b).mode))]))) && (((i64)(mm_decls_tttarget[((i64)((*a).mode))]) == (i64)0) || ((i64)(mm_decls_tttarget[((i64)((*b).mode))]) == (i64)0)))) {
    } else {
        mm_type_getdominantmodepp(p,a,b,&u,&v);
        if ((u == (i64)0)) {
            mm_support_txerror((byte*)"EQ/NE",(struct mm_decls_unitrec *)(0));
        };
        mm_type_coerceunit(a,u,(i64)0);
        mm_type_coerceunit(b,u,(i64)0);
    };
    (*p).mode = (i64)4;
}

static void mm_type_tx_lt(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    i64 u;
    i64 v;
    mm_type_tpass(a,(i64)21,(i64)0);
    mm_type_tpass(b,(i64)21,(i64)0);
    mm_type_getdominantmodepp(p,a,b,&u,&v);
    if ((u == (i64)0)) {
        mm_support_txerror((byte*)"lt/le/ge/gt",(struct mm_decls_unitrec *)(0));
    };
    mm_type_coerceunit(a,u,(i64)0);
    mm_type_coerceunit(b,u,(i64)0);
    (*p).mode = (i64)4;
}

static void mm_type_tx_callproc(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * pargs,i64 t) {
    struct mm_decls_unitrec *  q;
    struct mm_decls_strec *  d;
    struct mm_decls_strec *  e;
    struct mm_decls_strec *  pm;
    struct mm_decls_strec *  paramlist[100];
    struct mm_decls_unitrec *  arglist[100];
    struct mm_decls_unitrec *  newarglist[100];
    i64 nparams;
    i64 i;
    i64 j;
    i64 k;
    i64 nargs;
    i64 m;
    i64 kwdused;
    i64 qm;
    byte *  name;
    struct mm_decls_unitrec *  ulist = (struct mm_decls_unitrec *)(0);
    struct mm_decls_unitrec *  ulistx;
    mm_type_tpass(a,(i64)21,(i64)0);
    nargs = (nparams = (i64)0);
    if (((i64)((*a).tag)==(i64)3)) {
        d = (*a).def;
        //getparams:
L883 :;
;
        e = (*d).deflist;
        L884 :;
        while (!!(e)) {
            if (((i64)((u64)((*e).nameid)) == (i64)11)) {
                if ((nparams >= (i64)100)) {
                    mm_support_txerror((byte*)"Param overflow",(struct mm_decls_unitrec *)(0));
                };
                paramlist[(++nparams)-1] = e;
            };
            e = (*e).nextdef;
L885 :;
        }L886 :;
        ;
    } else {
        d = mm_decls_ttnamedef[((i64)((*a).mode))];
        goto L883 ;
;
    };
    q = pargs;
    L887 :;
    while (!!(q)) {
        if ((nargs >= (i64)100)) {
            mm_support_txerror((byte*)"Param overflow",(struct mm_decls_unitrec *)(0));
        };
        arglist[(++nargs)-1] = q;
        q = (*q).nextunit;
L888 :;
    }L889 :;
    ;
    (*p).mode = (i64)((*d).mode);
    if (((i64)((u64)((*d).nretvalues)) > (i64)1)) {
        (*p).mode = (i64)55;
    };
    if ((((i64)((*p).mode) == (i64)0) && ((i64)((*p).tag) == (i64)25))) {
        (*p).tag = (i64)189;
    };
    if (!!((i64)((*p).mode))) {
        mm_type_twiden(p,(i64)0);
    };
    if (!!((u64)((*d).varparams))) {
        L890 :;
        for (i=(i64)1;i<=nargs;++i) {
L891 :;
            if ((i <= nparams)) {
                mm_type_tpass(arglist[(i)-1],(i64)((*paramlist[(i)-1]).mode),(i64)0);
            } else {
                mm_type_tpass(arglist[(i)-1],(i64)21,(i64)0);
            };
            if ((mm_decls_targetbits == (i64)64)) {
                mm_type_twidenopnd(arglist[(i)-1]);
            };
L892 :;
        }L893 :;
        ;
        return;
    };
    k = (i64)0;
    kwdused = (i64)0;
    L894 :;
    for (i=(i64)1;i<=nparams;++i) {
L895 :;
        newarglist[(i)-1] = (struct mm_decls_unitrec *)(0);
L896 :;
    }L897 :;
    ;
    L898 :;
    for (i=(i64)1;i<=nargs;++i) {
L899 :;
        q = arglist[(i)-1];
        switch ((i64)((*q).tag)) {
        case 21:;
        {
            name = (*(*(*q).a).def).name;
            L902 :;
            for (j=(i64)1;j<=nparams;++j) {
L903 :;
                if (!!(mlib_eqstring((*paramlist[(j)-1]).name,name))) {
                    goto L905 ;
                };
L904 :;
            }
            {
                mm_support_txerror_s((byte*)"Can't find kwd param: #",name,(struct mm_decls_unitrec *)(0));
            }L905 :;
            ;
            if (!!(newarglist[(j)-1])) {
                mm_support_txerror_s((byte*)"Kwd: # already used or was implicit",name,(struct mm_decls_unitrec *)(0));
            };
            newarglist[(j)-1] = (*q).b;
            kwdused = (i64)1;
        }break;
        case 2:;
        {
            if (!!(kwdused)) {
                mm_support_txerror((byte*)"Normal param follows kwd",(struct mm_decls_unitrec *)(0));
            };
            q = (struct mm_decls_unitrec *)(0);
            goto L906 ;
;
        }break;
        default: {
            //doregparam:
L906 :;
;
            if (!!(kwdused)) {
                mm_support_txerror((byte*)"Normal param follows kwd",(struct mm_decls_unitrec *)(0));
            };
            if ((k >= nparams)) {
                msysc_m_print_startcon();
                msysc_m_print_str((byte*)"K=",NULL);
                msysc_m_print_i64(k,NULL);
                msysc_m_print_str((byte*)"NPARAMS=",NULL);
                msysc_m_print_i64(nparams,NULL);
                msysc_m_print_newline();
                msysc_m_print_end();
                ;
                mm_support_txerror((byte*)"Too many params supplied",(struct mm_decls_unitrec *)(0));
            };
            newarglist[(++k)-1] = q;
        }
        } //SW
;
L900 :;
    }L901 :;
    ;
    L907 :;
    for (i=(i64)1;i<=nparams;++i) {
L908 :;
        q = newarglist[(i)-1];
        pm = paramlist[(i)-1];
        if ((q == 0)) {
            if (!(!!((u64)((*pm).optional)))) {
                mm_support_txerror_s((byte*)"Param not optional: %lld",(byte *)((void *)(i)),(struct mm_decls_unitrec *)(0));
            };
            if (!!((*pm).code)) {
                newarglist[(i)-1] = mm_lib_duplunit((*pm).code,(i64)((*p).lineno));
            } else {
                newarglist[(i)-1] = mm_lib_createconstunit((u64)((i64)0),(i64)4);
            };
        };
L909 :;
    }L910 :;
    ;
    ulist = (struct mm_decls_unitrec *)(0);
    L911 :;
    for (i=(i64)1;i<=nparams;++i) {
L912 :;
        pm = paramlist[(i)-1];
        q = newarglist[(i)-1];
        if (((i64)((u64)((*pm).parammode)) == (i64)2)) {
            mm_type_tpass(q,(i64)21,(i64)1);
            m = (i64)(mm_decls_tttarget[((i64)((*pm).mode))]);
            qm = (i64)((*q).mode);
            if (!(!!(mm_type_comparemodes(qm,m)))) {
                mm_support_txerror((byte*)"&param: type mismatch",(struct mm_decls_unitrec *)(0));
            };
            mm_lib_insertunit(q,(i64)94);
            (*q).mode = mm_lib_createrefmode((struct mm_decls_strec *)(0),qm,(i64)0);
        } else {
            mm_type_tpass(q,(i64)((*pm).mode),(i64)0);
            if ((mm_decls_targetbits == (i64)64)) {
                mm_type_twidenopnd(q);
            };
        };
        if ((ulist == 0)) {
            ulist = q;
        } else {
            (*ulistx).nextunit = q;
        };
        ulistx = q;
        (*q).nextunit = (struct mm_decls_unitrec *)(0);
L913 :;
    }L914 :;
    ;
    (*p).b = ulist;
}

static void mm_type_tx_neg(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    i64 u;
    mm_type_tpass(a,(i64)21,(i64)0);
    if (!(!!(mm_lib_isnumericmode((i64)((*a).mode))))) {
        mm_support_txerror((byte*)"Neg: not numeric",a);
    };
    mm_type_twidenopnd(a);
    u = (i64)((*a).mode);
    mm_type_coerceunit(a,u,(i64)0);
    (*p).mode = u;
}

static void mm_type_tx_if(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,struct mm_decls_unitrec * c,i64 t,i64 lv) {
    i64 u;
    i64 v;
    mm_type_tcond(a);
    mm_type_tpass(b,t,lv);
    if (((t != (i64)0) && !(!!(c)))) {
        mm_support_txerror((byte*)"if needs else",(struct mm_decls_unitrec *)(0));
    };
    mm_type_tpass(c,t,lv);
    if ((t == (i64)21)) {
        mm_type_getdominantmodepp(p,b,c,&u,&v);
        mm_type_coerceunit(b,u,(i64)0);
        mm_type_coerceunit(c,u,(i64)0);
        (*p).mode = u;
    } else {
        (*p).mode = t;
    };
}

static void mm_type_tx_longif(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,i64 t,i64 lv) {
    struct mm_decls_unitrec *  q;
    struct mm_decls_unitrec *  r;
    i64 u;
    i64 v;
    u = (i64)0;
    q = a;
    L915 :;
    while (!!(q)) {
        mm_type_tcond((*q).a);
        r = (*q).b;
        mm_type_tpass(r,t,lv);
        if ((t == (i64)21)) {
            if ((u == (i64)0)) {
                u = (i64)((*r).mode);
            } else {
                mm_type_getdominantmode((i64)0,u,(i64)((*r).mode),&u,&v);
            };
        };
        q = (*q).nextunit;
L916 :;
    }L917 :;
    ;
    if (((t != (i64)0) && (b == 0))) {
        mm_support_txerror((byte*)"longif needs else",(struct mm_decls_unitrec *)(0));
    };
    mm_type_tpass(b,t,lv);
    if ((t == (i64)21)) {
        mm_type_getdominantmode((i64)0,u,(i64)((*b).mode),&u,&v);
    };
    if ((t != (i64)0)) {
        q = a;
        L918 :;
        while (!!(q)) {
            if ((t == (i64)21)) {
                mm_type_coerceunit((*q).b,u,(i64)0);
            };
            (*q).mode = (i64)((*(*q).b).mode);
            q = (*q).nextunit;
L919 :;
        }L920 :;
        ;
        if ((t == (i64)21)) {
            mm_type_coerceunit(b,u,(i64)0);
        };
        (*p).mode = (i64)((*b).mode);
    };
}

static void mm_type_tx_preincr(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t) {
    mm_type_tpass(a,(i64)21,(i64)1);
    if (!((!!((u64)(mm_decls_ttisinteger[((i64)((*a).mode))])) || !!((u64)(mm_decls_ttisref[((i64)((*a).mode))]))))) {
        mm_support_txerror((byte*)"incr: not int/ref",(struct mm_decls_unitrec *)(0));
    };
    if ((t == (i64)0)) {
        if (((i64)((*p).tag)==(i64)138) || ((i64)((*p).tag)==(i64)140)) {
            (*p).tag = (i64)142;
        }else if (((i64)((*p).tag)==(i64)139) || ((i64)((*p).tag)==(i64)141)) {
            (*p).tag = (i64)143;
        };
    } else {
        (*p).mode = (i64)((*a).mode);
    };
    if ((t != (i64)0)) {
        mm_type_twiden(p,(i64)0);
    };
}

static void mm_type_tx_for(struct mm_decls_unitrec * a,struct mm_decls_unitrec * pbody,struct mm_decls_unitrec * c) {
    struct mm_decls_unitrec *  pfrom;
    struct mm_decls_unitrec *  pto;
    struct mm_decls_unitrec *  pstep;
    i64 u;
    pfrom = (*a).nextunit;
    pto = (*pfrom).nextunit;
    pstep = (*pto).nextunit;
    mm_type_tpass(a,(i64)21,(i64)0);
    u = (i64)((*a).mode);
    mm_type_tpass(pfrom,u,(i64)0);
    mm_type_tpass(pto,u,(i64)0);
    mm_type_tpass(pstep,u,(i64)0);
    mm_type_tpass(pbody,(i64)0,(i64)0);
    mm_type_tpass((*pbody).nextunit,(i64)0,(i64)0);
}

static void mm_type_tx_index(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,i64 t,i64 lv) {
    i64 amode;
    i64 emode;
    i64 abasemode;
    i64 tmode;
    mm_type_tpass(a,(i64)21,lv);
    mm_type_tpass(b,(i64)21,(i64)0);
    amode = (i64)((*a).mode);
    if (((i64)(mm_decls_ttbasetype[((i64)((*b).mode))])==(i64)27)) {
        mm_support_txerror((byte*)"CAN'T DO SLICING YET",(struct mm_decls_unitrec *)(0));
    } else {
        mm_type_coerceunit(b,(i64)4,(i64)0);
    };
    abasemode = (i64)(mm_decls_ttbasetype[(amode)]);
    L921 :;
    while ((abasemode == (i64)20)) {
        tmode = (i64)(mm_decls_tttarget[(amode)]);
        mm_lib_insertunit(a,(i64)93);
        amode = (i64)(((*a).mode = tmode));
        abasemode = (i64)(mm_decls_ttbasetype[(amode)]);
L922 :;
    }L923 :;
    ;
    if (((u64)(mm_tables_stdtypecode[(abasemode)]) != (u64)65u)) {
        mm_support_txerror_s((byte*)"Can't index: #",mm_lib_strmode(amode,(i64)1),(struct mm_decls_unitrec *)(0));
    };
    if ((abasemode==(i64)50)) {
        emode = (i64)50;
    }else if ((abasemode==(i64)34)) {
        emode = (i64)34;
    } else {
        emode = (i64)(mm_decls_tttarget[(amode)]);
    };
    (*p).mode = emode;
    mm_type_twiden(p,lv);
}

static void mm_type_tx_makerange(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    i64 u;
    i64 v;
    i64 amode;
    i64 bmode;
    mm_type_tpass(a,(i64)21,(i64)0);
    mm_type_tpass(b,(i64)21,(i64)0);
    amode = (i64)((*a).mode);
    bmode = (i64)((*b).mode);
    if ((!(!!((u64)(mm_decls_ttisinteger[(amode)]))) || !(!!((u64)(mm_decls_ttisinteger[(bmode)]))))) {
        mm_support_txerror((byte*)"range not int",(struct mm_decls_unitrec *)(0));
    };
    if (((u64)(mm_decls_tttypecode[(amode)]) != (u64)(mm_decls_tttypecode[(bmode)]))) {
        mm_support_txerror((byte*)"range: mixed i64/u64",(struct mm_decls_unitrec *)(0));
    };
    mm_type_getdominantmodepp(p,a,b,&u,&v);
    if (!!((u64)(mm_decls_ttisint[(amode)]))) {
        mm_type_coerceunit(a,(i64)4,(i64)0);
        mm_type_coerceunit(b,(i64)4,(i64)0);
    } else {
        mm_type_coerceunit(a,(i64)9,(i64)0);
        mm_type_coerceunit(b,(i64)9,(i64)0);
    };
    (*p).mode = (i64)27;
}

static void mm_type_tx_makeset(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t) {
    i64 y;
    i64 isconst;
    i64 lower;
    i64 upper;
    if ((t == (i64)0)) {
        mm_support_txerror((byte*)"open(var) set type",(struct mm_decls_unitrec *)(0));
    };
    lower = (i64)2000000000;
    upper = (i64)-2000000000;
    isconst = (i64)1;
    L924 :;
    while (!!(a)) {
        mm_type_tpass(a,(i64)21,(i64)0);
        if (!(!!((i64)((*a).isconst)))) {
            isconst = (i64)0;
        } else {
            if (((i64)((*a).tag)==(i64)15)) {
                lower=(lower<(*(*a).a).value?lower:(*(*a).a).value);
;
                upper=(upper>(*(*a).b).value?upper:(*(*a).b).value);
;
            }else if (((i64)((*a).tag)==(i64)1)) {
                mm_type_coerceunit(a,(i64)4,(i64)0);
                lower=(lower<(y = (*a).value)?lower:(y = (*a).value));
;
                upper=(upper>(y = (*a).value)?upper:(y = (*a).value));
;
            };
        };
        a = (*a).nextunit;
L925 :;
    }L926 :;
    ;
    (*p).isconst = isconst;
    (*p).range_lower = lower;
    (*p).range_upper = upper;
    (*p).mode = (i64)39;
}

static void mm_type_tx_makedict(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t) {
    i64 isconst;
    i64 km;
    i64 vm;
    if ((t == (i64)0)) {
        mm_support_txerror((byte*)"open(var) dict type",(struct mm_decls_unitrec *)(0));
    };
    if (((i64)(mm_decls_ttbasetype[(t)]) != (i64)40)) {
        mm_support_txerror((byte*)"not dict type",(struct mm_decls_unitrec *)(0));
    };
    km = (i64)(mm_decls_ttkeymode[(t)]);
    vm = (i64)(mm_decls_tttarget[(t)]);
    isconst = (i64)1;
    L927 :;
    while (!!(a)) {
        mm_type_tpass((*a).a,km,(i64)0);
        mm_type_tpass((*a).b,vm,(i64)0);
        a = (*a).nextunit;
L928 :;
    }L929 :;
    ;
    (*p).isconst = isconst;
    (*p).mode = t;
}

static void mm_type_tx_ptr(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t,i64 lv) {
    struct mm_decls_strec *  d;
    if (((i64)((*p).tag)==(i64)3)) {
        d = (*p).def;
        if (((i64)((*d).nameid)==(i64)9) || ((i64)((*d).nameid)==(i64)10) || ((i64)((*d).nameid)==(i64)11)) {
        } else {
            mm_support_txerror_s((byte*)"Can't use as ptr: ",(*d).name,(struct mm_decls_unitrec *)(0));
        };
    };
    mm_type_tpass(a,(i64)21,(i64)0);
    if (((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)0)) {
        mm_support_txerror((byte*)"Deref Void",(struct mm_decls_unitrec *)(0));
    }else if (((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)20)) {
        (*p).mode = (i64)(mm_decls_tttarget[((i64)((*a).mode))]);
    }else if (((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)28)) {
        mm_support_txerror((byte*)"Can't deref slice",(struct mm_decls_unitrec *)(0));
    }else if (((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)50)) {
        (*p).mode = (i64)50;
    } else {
        mm_support_txerror((byte*)"PTR: need ref T",(struct mm_decls_unitrec *)(0));
    };
    mm_type_twiden(p,lv);
}

static void mm_type_setrecordsize(i64 m) {
    struct mm_decls_strec *  fieldlist[208];
    i64 nfields;
    i64 size;
    i64 index;
    struct mm_decls_strec *  d;
    struct mm_decls_strec *  e;
    byte *  flags;
    i64 flag;
    if (!!((i64)(mm_decls_ttsize[(m)]))) {
        return;
    };
    d = mm_decls_ttnamedef[(m)];
    e = (*d).deflist;
    nfields = (i64)0;
    fieldlist[(++nfields)-1] = (struct mm_decls_strec *)'S';
    L930 :;
    while (!!(e)) {
        if ((nfields >= (i64)200)) {
            mm_support_gerror((byte*)"srs:too many fields",(struct mm_decls_unitrec *)(0));
        };
        mm_type_setmodesize((i64)((*e).mode));
        flags = (byte *)(&(*e).uflags);
        L933 :;
        if (((i64)((*flags))==(i64)83) || ((i64)((*flags))==(i64)85)) {
            flag = (i64)((*flags));
            fieldlist[(++nfields)-1] = *(struct mm_decls_strec **)&flag;
            ++flags;
        } else {
            goto L934 ;
        }goto L933 ;
L934 :;
        ;
        fieldlist[(++nfields)-1] = e;
        L935 :;
        while (1) {
            flag = (i64)((*flags++));
            if ((flag==(i64)42)) {
            }else if ((flag==(i64)69)) {
                fieldlist[(++nfields)-1] = (struct mm_decls_strec *)'E';
            } else {
                goto L936 ;
            };
        }L936 :;
        ;
        e = (*e).nextdef;
L931 :;
    }L932 :;
    ;
    fieldlist[(++nfields)-1] = (struct mm_decls_strec *)'E';
    fieldlist[((nfields + (i64)1))-1] = (struct mm_decls_strec *)(0);
    mm_type_countedfields = (i64)0;
    index = (i64)2;
    mm_type_scanrecord((i64)83,&fieldlist,&index,&size,(i64)0);
    mm_decls_ttsize[(m)] = size;
    mm_decls_ttlength[(m)] = mm_type_countedfields;
    mm_decls_ttlower[(m)] = (i64)1;
}

static void mm_type_scanrecord(i64 state,struct mm_decls_strec * (*fields)[],i64 * index,i64 * isize,i64 offset) {
    struct mm_decls_strec *  e;
    struct mm_decls_strec *  f;
    struct mm_decls_strec *  ea;
    i64 size = (i64)0;
    i64 fieldsize;
    i64 bitoffset;
    size = (i64)0;
    L937 :;
    while (!!((f = (*fields)[((*index)++)-1]))) {
        if (((i64)(f)==(i64)83) || ((i64)(f)==(i64)85)) {
            mm_type_scanrecord((i64)(f),fields,index,&fieldsize,offset);
        }else if (((i64)(f)==(i64)69)) {
            if ((state == (i64)85)) {
                ++mm_type_countedfields;
            };
            (*isize) = size;
            return;
        } else {
            if (((i64)((*f).mode) == (i64)25)) {
                fieldsize = (i64)0;
                ea = (*f).equivfield;
                (*f).offset = (i64)((*ea).offset);
                (*f).bitoffset = (u64)(bitoffset);
                bitoffset += (i64)((*f).bitfieldwidth);
                if ((bitoffset > (i64)((u64)(mm_decls_ttbitwidth[((i64)((*(*f).equivfield).mode))])))) {
                    mm_support_txerror((byte*)"Bit fields overflow type",(struct mm_decls_unitrec *)(0));
                };
            } else if (!!((u64)((*f).at))) {
                bitoffset = (i64)0;
                e = (*f).equivfield;
                fieldsize = (i64)0;
                ea = mm_name_resolve_equiv_name((*f).owner,e);
                (*f).offset = (i64)((*ea).offset);
            } else {
                bitoffset = (i64)0;
                if ((state == (i64)83)) {
                    ++mm_type_countedfields;
                };
                fieldsize = (i64)(mm_decls_ttsize[((i64)((*f).mode))]);
                (*f).offset = offset;
            };
        };
        if ((state == (i64)83)) {
            offset += fieldsize;
            size += fieldsize;
        } else {
            size = (size>fieldsize?size:fieldsize);
        };
L938 :;
    }L939 :;
    ;
}

static void mm_type_tx_convert(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 hard) {
    if (((i64)((*a).tag) == (i64)14)) {
        mm_type_tx_makelist(a,(*a).a,(i64)((*p).newmode),(i64)0);
    } else {
        mm_type_tpass(a,(i64)21,(i64)0);
        mm_type_coerceunit(a,(i64)((*p).newmode),hard);
    };
    mm_lib_deleteunit(p,a);
}

static void mm_type_tx_makelist(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t,i64 lv) {
    i64 alength;
    i64 tlength;
    i64 elemtype;
    i64 newt;
    i64 isconst;
    struct mm_decls_unitrec *  q;
    struct mm_decls_unitrec *  b;
    struct mm_decls_strec *  e;
    alength = (i64)((*p).length);
    newt = (i64)0;
    isconst = (i64)1;
    tlength = (i64)(mm_decls_ttlength[(t)]);
    if (!!(tlength)) {
        if ((alength < tlength)) {
            mm_support_txerror((byte*)"Too few elements",(struct mm_decls_unitrec *)(0));
        } else if ((alength > tlength)) {
            mm_support_txerror((byte*)"Too many elements",(struct mm_decls_unitrec *)(0));
        };
    };
    if (((i64)(mm_decls_ttbasetype[(t)])==(i64)29)) {
        elemtype = (i64)(mm_decls_tttarget[(t)]);
        if ((tlength == (i64)0)) {
            newt = mm_lib_createarraymodek((struct mm_decls_strec *)(0),elemtype,(i64)(mm_decls_ttlower[(t)]),alength,(i64)0);
        } else {
            newt = t;
        };
        q = a;
        L940 :;
        while (!!(q)) {
            mm_type_tpass(q,elemtype,lv);
            if (!(((i64)((*q).tag) == (i64)1))) {
                isconst = (i64)0;
            };
            q = (*q).nextunit;
L941 :;
        }L942 :;
        ;
        (*p).mode = newt;
    }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)32)) {
        e = (*mm_decls_ttnamedef[(t)]).deflist;
        q = a;
        L943 :;
        while ((!!(q) && !!(e))) {
            L946 :;
            while (((i64)((*e).mode) == (i64)25)) {
                e = (*e).nextdef;
                if (!(!!(e))) {
                    goto L948 ;
                };
L947 :;
            }L948 :;
            ;
            mm_type_tpass(q,(i64)((*e).mode),lv);
            if (!(((i64)((*q).tag) == (i64)1))) {
                isconst = (i64)0;
            };
            q = (*q).nextunit;
            e = (*e).nextdef;
L944 :;
        }L945 :;
        ;
        L949 :;
        while ((!!(e) && ((i64)((*e).mode) == (i64)25))) {
            e = (*e).nextdef;
L950 :;
        }L951 :;
        ;
        if ((!!(q) || !!(e))) {
            mm_support_txerror((byte*)"Can't initialise unions",(struct mm_decls_unitrec *)(0));
        };
        (*p).mode = t;
    }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)28)) {
        if ((((a == 0) || !!((b = (*a).nextunit,(b == 0)))) || !!((*b).nextunit))) {
            mm_support_txerror((byte*)"bad slice init",(struct mm_decls_unitrec *)(0));
        };
        mm_type_tpass(a,(i64)21,lv);
        if (((i64)(mm_decls_ttbasetype[((i64)((*a).mode))]) != (i64)20)) {
            mm_support_txerror((byte*)"slice init not ref",(struct mm_decls_unitrec *)(0));
        };
        mm_type_tpass(b,(i64)4,(i64)0);
        (*p).mode = t;
    }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)36)) {
        elemtype = (i64)(mm_decls_tttarget[(t)]);
        q = a;
        L952 :;
        while (!!(q)) {
            mm_type_tpass(q,elemtype,lv);
            if (!(((i64)((*q).tag) == (i64)1))) {
                isconst = (i64)0;
            };
            q = (*q).nextunit;
L953 :;
        }L954 :;
        ;
        (*p).mode = t;
    }else if (((i64)(mm_decls_ttbasetype[(t)])==(i64)0) || ((i64)(mm_decls_ttbasetype[(t)])==(i64)50)) {
        q = a;
        L955 :;
        while (!!(q)) {
            mm_type_tpass(q,(i64)21,lv);
            if (!(((i64)((*q).tag) == (i64)1))) {
                isconst = (i64)0;
            };
            q = (*q).nextunit;
L956 :;
        }L957 :;
        ;
        (*p).mode = (i64)50;
    } else {
        mm_support_txerror_s((byte*)"Unknown makelist type: #",mm_lib_strmode(t,(i64)1),(struct mm_decls_unitrec *)(0));
    };
    (*p).isconst = isconst;
}

static void mm_type_tx_dot(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,i64 lv) {
    i64 recmode;
    i64 recbasemode;
    i64 i;
    i64 j;
    i64 newtag;
    i64 tmode;
    struct mm_decls_unitrec *  pindex;
    struct mm_decls_strec *  d;
    struct mm_decls_strec *  dequiv;
    mm_type_tpass(a,(i64)21,lv);
    recmode = (i64)((*a).mode);
    recbasemode = (i64)(mm_decls_ttbasetype[(recmode)]);
    L958 :;
    while ((recbasemode == (i64)20)) {
        tmode = (i64)(mm_decls_tttarget[(recmode)]);
        mm_lib_insertunit(a,(i64)93);
        recmode = (i64)(((*a).mode = tmode));
        recbasemode = (i64)(mm_decls_ttbasetype[(recmode)]);
L959 :;
    }L960 :;
    ;
    if (((i64)(mm_decls_ttbasetype[(recmode)]) != (i64)32)) {
        mm_support_txerror((byte*)"Bad record type",(struct mm_decls_unitrec *)(0));
    };
    d = (*b).def;
    if (((i64)((u64)((*d).nameid)) == (i64)0)) {
        d = ((*b).def = mm_type_resolvefield(d,recmode));
    };
    if (((i64)((*d).mode) == (i64)25)) {
        i = (i64)((*d).bitoffset);
        j = ((i + (i64)((u64)((*d).bitfieldwidth))) - (i64)1);
        dequiv = (*d).equivfield;
        (*b).def = dequiv;
        (*b).mode = (i64)((*dequiv).mode);
        (*p).offset = (i64)((*d).offset);
        if ((i == j)) {
            pindex = mm_lib_createconstunit((u64)(i),(i64)4);
            newtag = (i64)84;
        } else {
            pindex = mm_lib_createunit2((i64)15,mm_lib_createconstunit((u64)(i),(i64)4),mm_lib_createconstunit((u64)(j),(i64)4));
            (*pindex).mode = (i64)27;
            newtag = (i64)85;
        };
        (*p).mode = (i64)((*b).mode);
        mm_type_twiden(p,lv);
        mm_lib_insertunit(p,newtag);
        (*p).mode = (i64)9;
        (*p).b = pindex;
        return;
    };
    (*b).mode = (i64)((*d).mode);
    (*p).mode = (i64)((*d).mode);
    if (!!((u64)(mm_decls_ttisflexvar[((i64)((*d).mode))]))) {
        msysc_m_print_startcon();
        msysc_m_print_str((byte*)"DOT:NEED TO REMOVE ADDR OF",NULL);
        msysc_m_print_newline();
        msysc_m_print_end();
        ;
        mm_type_removeaddrof(a);
    };
    (*p).offset = (i64)((*d).offset);
    mm_type_twiden(p,lv);
}

static struct mm_decls_strec * mm_type_resolvefield(struct mm_decls_strec * d,i64 m) {
    struct mm_decls_strec *  e;
    struct mm_decls_strec *  t;
    if (((i64)(mm_decls_ttbasetype[(m)])==(i64)32) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)33)) {
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)20)) {
        m = (i64)(mm_decls_tttarget[(m)]);
        if (((i64)(mm_decls_ttbasetype[(m)]) != (i64)32)) {
            mm_support_txerror((byte*)"3:record expected",(struct mm_decls_unitrec *)(0));
        };
    } else {
        mm_support_txerror((byte*)"4:record expected",(struct mm_decls_unitrec *)(0));
    };
    t = mm_decls_ttnamedef[(m)];
    e = mm_name_finddupl(t,d);
    if (!(!!(e))) {
        mm_support_txerror_s((byte*)"Not a field: #",(*d).name,(struct mm_decls_unitrec *)(0));
    };
    return e;
}

static i64 mm_type_comparemodes(i64 s,i64 t) {
    i64 sbase;
    i64 tbase;
    struct mm_decls_strec *  d;
    struct mm_decls_strec *  e;
    if ((s == t)) {
        return (i64)1;
    };
    sbase = (i64)(mm_decls_ttbasetype[(s)]);
    tbase = (i64)(mm_decls_ttbasetype[(t)]);
    if ((sbase != tbase)) {
        return (i64)0;
    };
    if ((sbase==(i64)20)) {
        return mm_type_comparemodes((i64)(mm_decls_tttarget[(s)]),(i64)(mm_decls_tttarget[(t)]));
    }else if ((sbase==(i64)29)) {
        if ((!!(mm_type_comparemodes((i64)(mm_decls_tttarget[(s)]),(i64)(mm_decls_tttarget[(t)]))) && ((((i64)(mm_decls_ttlength[(s)]) == (i64)(mm_decls_ttlength[(t)])) || ((i64)(mm_decls_ttlength[(s)]) == (i64)0)) || ((i64)(mm_decls_ttlength[(t)]) == (i64)0)))) {
            return (i64)1;
        };
    }else if ((sbase==(i64)28)) {
        return mm_type_comparemodes((i64)(mm_decls_tttarget[(s)]),(i64)(mm_decls_tttarget[(t)]));
    }else if ((sbase==(i64)41)) {
        d = mm_decls_ttnamedef[(s)];
        e = mm_decls_ttnamedef[(t)];
        if ((!!(d) && !!(e))) {
            if (!(!!(mm_type_comparemodes((i64)((*d).mode),(i64)((*e).mode))))) {
                return (i64)0;
            };
            if ((((*d).paramlist == 0) && ((*e).paramlist == 0))) {
                return (i64)1;
            };
        };
    };
    return (i64)0;
}

static i64 mm_type_isboolunit(struct mm_decls_unitrec * p) {
    return (i64)(mm_tables_boolunitset[((i64)((*p).tag))]);
}

static void mm_type_checkbool(i64 m) {
    if (!(!!((u64)(mm_decls_tttypecode[(m)])))) {
        mm_support_txerror_s((byte*)"Can't convert to bool: #",mm_lib_strmode(m,(i64)1),(struct mm_decls_unitrec *)(0));
    };
}

static void mm_type_tx_andl(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    mm_type_tpass(a,(i64)21,(i64)0);
    mm_type_tpass(b,(i64)21,(i64)0);
    if (!(!!(mm_type_isboolunit(a)))) {
        mm_lib_insertunit(a,(i64)13);
    };
    if (!(!!(mm_type_isboolunit(b)))) {
        mm_lib_insertunit(b,(i64)13);
    };
    mm_type_checkbool((i64)((*a).mode));
    mm_type_checkbool((i64)((*b).mode));
    (*p).mode = (i64)4;
}

static void mm_type_convintconst(struct mm_decls_unitrec * p,i64 x) {
    (*p).tag = (i64)1;
    (*p).mode = (i64)4;
    (*p).value = x;
    (*p).a = ((*p).b = ((*p).c = (struct mm_decls_unitrec *)(0)));
    (*p).isconst = (i64)1;
}

static void mm_type_tx_upb(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    i64 m;
    mm_type_tpass(a,(i64)21,(i64)0);
    m = (i64)((*a).mode);
    if (((i64)(mm_decls_ttbasetype[(m)])==(i64)29)) {
        mm_type_convintconst(p,(((i64)(mm_decls_ttlower[(m)]) + (i64)(mm_decls_ttlength[(m)])) - (i64)1));
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)28) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)36) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)34)) {
    } else {
        mm_support_txerror_s((byte*)"UPB #",mm_lib_strmode(m,(i64)1),(struct mm_decls_unitrec *)(0));
    };
    (*p).mode = (i64)4;
}

static void mm_type_tx_len(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    i64 m;
    mm_type_tpass(a,(i64)21,(i64)0);
    m = (i64)((*a).mode);
    if (((i64)(mm_decls_ttbasetype[(m)])==(i64)29)) {
        mm_type_convintconst(p,(i64)(mm_decls_ttlength[(m)]));
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)28) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)36) || ((i64)(mm_decls_ttbasetype[(m)])==(i64)34)) {
    } else {
        mm_support_txerror_s((byte*)"LEN #",mm_lib_strmode(m,(i64)1),(struct mm_decls_unitrec *)(0));
    };
    (*p).mode = (i64)4;
}

static void mm_type_tx_lwb(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    i64 m;
    mm_type_tpass(a,(i64)21,(i64)0);
    m = (i64)((*a).mode);
    if (((i64)(mm_decls_ttbasetype[(m)])==(i64)29)) {
        mm_type_convintconst(p,(i64)(mm_decls_ttlower[(m)]));
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)28)) {
        mm_type_convintconst(p,(i64)(mm_decls_ttlower[(m)]));
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)34)) {
        mm_type_convintconst(p,(i64)1);
    } else {
        mm_support_txerror_s((byte*)"LWB #",mm_lib_strmode(m,(i64)1),(struct mm_decls_unitrec *)(0));
    };
    (*p).mode = (i64)4;
}

static void mm_type_tx_bounds(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    i64 m;
    i64 lower;
    i64 upper;
    mm_type_tpass(a,(i64)21,(i64)0);
    m = (i64)((*a).mode);
    if (((i64)(mm_decls_ttbasetype[(m)])==(i64)29)) {
        lower = (i64)(mm_decls_ttlower[(m)]);
        upper = ((lower + (i64)(mm_decls_ttlength[(m)])) - (i64)1);
    }else if (((i64)(mm_decls_ttbasetype[(m)])==(i64)28)) {
        return;
    } else {
        mm_support_txerror_s((byte*)"BOUNDS #",mm_lib_strmode(m,(i64)1),(struct mm_decls_unitrec *)(0));
    };
    (*p).tag = (i64)1;
    (*p).mode = (i64)27;
    (*p).qvalue = mm_lib_makeqvalue_ab(lower,upper);
    (*p).a = ((*p).b = ((*p).c = (struct mm_decls_unitrec *)(0)));
    (*p).isconst = (i64)1;
}

static void mm_type_tx_sliceptr(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    i64 m;
    i64 tmode;
    mm_type_tpass(a,(i64)21,(i64)0);
    m = (i64)((*a).mode);
    if (((i64)(mm_decls_ttbasetype[(m)])==(i64)28)) {
    } else {
        mm_support_txerror_s((byte*)"SLICEPTR #",mm_lib_strmode(m,(i64)1),(struct mm_decls_unitrec *)(0));
    };
    tmode = mm_lib_createarraymodek((struct mm_decls_strec *)(0),(i64)(mm_decls_tttarget[(m)]),(i64)(mm_decls_ttlower[(m)]),(i64)0,(i64)0);
    (*p).mode = mm_lib_createrefmode((struct mm_decls_strec *)(0),tmode,(i64)0);
}

static void mm_type_tx_inot(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    i64 u;
    mm_type_tpass(a,(i64)21,(i64)0);
    if (!(!!(mm_lib_isintmode((i64)((*a).mode))))) {
        mm_support_txerror((byte*)"INOT/not int",(struct mm_decls_unitrec *)(0));
    };
    mm_type_twidenopnd(a);
    u = (i64)(mm_decls_ttbasetype[((i64)((*a).mode))]);
    mm_type_coerceunit(a,u,(i64)0);
    (*p).mode = u;
}

static void mm_type_tx_atan2(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    mm_type_tpass(a,(i64)12,(i64)0);
    mm_type_tpass(b,(i64)12,(i64)0);
    (*p).mode = (i64)12;
}

static void mm_type_tx_swap(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    mm_type_tpass(a,(i64)21,(i64)1);
    mm_type_tpass(b,(i64)21,(i64)1);
    if (!(!!(mm_type_comparemodes((i64)((*a).mode),(i64)((*b).mode))))) {
        mm_support_txerror((byte*)"SWAP: type mismatch",(struct mm_decls_unitrec *)(0));
    };
    (*p).mode = (i64)0;
}

static void mm_type_tx_sqrt(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    mm_type_tpass(a,(i64)21,(i64)0);
    if (!(!!(mm_lib_isnumericmode((i64)((*a).mode))))) {
        mm_support_txerror((byte*)"maths: not numeric",(struct mm_decls_unitrec *)(0));
    };
    mm_type_coerceunit(a,(i64)12,(i64)0);
    (*p).mode = (i64)12;
}

static void mm_type_tx_select(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,struct mm_decls_unitrec * c,i64 t,i64 lv) {
    i64 u;
    i64 v;
    struct mm_decls_unitrec *  q;
    mm_type_tpass(a,(i64)4,(i64)0);
    q = b;
    L961 :;
    while (!!(q)) {
        mm_type_tpass(q,t,lv);
        if ((q == b)) {
            u = (i64)((*q).mode);
        } else {
            mm_type_getdominantmode((i64)0,u,(i64)((*q).mode),&u,&v);
        };
        q = (*q).nextunit;
L962 :;
    }L963 :;
    ;
    mm_type_tpass(c,t,lv);
    mm_type_getdominantmode((i64)0,u,(i64)((*c).mode),&u,&v);
    q = b;
    L964 :;
    while (!!(q)) {
        mm_type_coerceunit(q,u,(i64)0);
        q = (*q).nextunit;
L965 :;
    }L966 :;
    ;
    (*p).mode = u;
}

static void mm_type_tx_case(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,struct mm_decls_unitrec * c,i64 t,i64 lv) {
    i64 amode;
    i64 u;
    i64 v;
    struct mm_decls_unitrec *  wt;
    struct mm_decls_unitrec *  w;
    if ((((i64)((*p).tag) == (i64)217) && !!(lv))) {
        mm_support_gerror((byte*)"&docase",(struct mm_decls_unitrec *)(0));
    };
    mm_type_tpass(a,(i64)21,(i64)0);
    if ((a == 0)) {
        amode = (i64)21;
    } else {
        amode = (i64)((*a).mode);
    };
    if ((!!(mm_lib_isintmode(amode)) && ((i64)(mm_decls_ttsize[(amode)]) < (i64)8))) {
        mm_type_coerceunit(a,(i64)4,(i64)0);
        amode = (i64)4;
    };
    u = (i64)0;
    wt = b;
    L967 :;
    while (!!(wt)) {
        w = (*wt).a;
        L970 :;
        while (!!(w)) {
            mm_type_tpass(w,(i64)21,(i64)0);
            if (((i64)((*w).tag) == (i64)15)) {
                if (!(!!(mm_lib_isintmode(amode)))) {
                    mm_support_txerror((byte*)"case: need int index",(struct mm_decls_unitrec *)(0));
                };
            } else {
                mm_type_coerceunit(w,amode,(i64)0);
                if (((amode == (i64)21) && !(!!(mm_type_isboolunit(w))))) {
                    mm_lib_insertunit(w,(i64)13);
                };
            };
            w = (*w).nextunit;
L971 :;
        }L972 :;
        ;
        mm_type_tpass((*wt).b,t,lv);
        if ((t != (i64)0)) {
            if (!!(u)) {
                mm_type_getdominantmode((i64)0,u,(i64)((*(*wt).b).mode),&u,&v);
            } else {
                u = (i64)((*(*wt).b).mode);
            };
        };
        wt = (*wt).nextunit;
L968 :;
    }L969 :;
    ;
    if (!!(c)) {
        mm_type_tpass(c,t,lv);
        if ((t == (i64)21)) {
            mm_type_getdominantmode((i64)0,u,(i64)((*c).mode),&u,&v);
        };
    } else if ((t != (i64)0)) {
        mm_support_txerror((byte*)"case needs else",(struct mm_decls_unitrec *)(0));
    };
    if ((t != (i64)0)) {
        (*p).mode = u;
    } else {
        (*p).mode = (i64)0;
    };
}

static void mm_type_tx_notl(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    mm_type_tpass(a,(i64)21,(i64)0);
    if (((i64)((*a).tag)==(i64)12)) {
        mm_lib_deleteunit(p,a);
        (*p).tag = (i64)13;
    };
    if ((((i64)((*p).tag) == (i64)13) && !!(mm_type_isboolunit((*p).a)))) {
        mm_lib_deleteunit(p,(*p).a);
    };
    mm_type_checkbool((i64)((*a).mode));
    (*p).mode = (i64)4;
}

static void mm_type_tx_istruel(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    mm_type_tpass(a,(i64)21,(i64)0);
    if (!!(mm_type_isboolunit(a))) {
        mm_lib_deleteunit(p,a);
    };
    mm_type_checkbool((i64)((*a).mode));
    (*p).mode = (i64)4;
}

static void mm_type_tx_addto(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    i64 issub;
    i64 atype;
    i64 btype;
    i64 u;
    mm_type_tpass(a,(i64)21,(i64)1);
    mm_type_tpass(b,(i64)21,(i64)0);
    issub = ((i64)((*p).tag) == (i64)38);
    atype = (i64)(mm_decls_ttbasetype[((i64)((*a).mode))]);
    btype = (i64)(mm_decls_ttbasetype[((i64)((*b).mode))]);
    if ((((i64)((*p).tag) == (i64)147) && ((u64)(mm_tables_stdtypecode[(atype)]) != (u64)82u))) {
        (*p).tag = (i64)148;
    };
    u = atype;
    if ((atype==(i64)20)) {
        if ((btype == (i64)20)) {
            if (!(!!(issub))) {
                mm_support_txerror((byte*)"ref+ref",(struct mm_decls_unitrec *)(0));
            };
            if (!(!!(mm_type_comparemodes((i64)((*a).mode),(i64)((*b).mode))))) {
                mm_support_txerror((byte*)"ref-ref bad types",(struct mm_decls_unitrec *)(0));
            };
            u = btype;
        } else {
            u = (i64)4;
        };
    }else if ((atype==(i64)34)) {
        if (!!((u64)(mm_decls_ttisinteger[(btype)]))) {
            u = (i64)4;
        };
    };
    mm_type_coerceunit(b,u,(i64)0);
    if (!!(mm_lib_isrefmode(atype))) {
        if (((!(!!(mm_decls_ctarget)) && ((i64)((*b).tag) == (i64)1)) && (btype != (i64)20))) {
            (*b).value *= (i64)(mm_decls_ttsize[((i64)(mm_decls_tttarget[((i64)((*a).mode))]))]);
        } else {
            (*p).tag = (((i64)((*p).tag) == (i64)144)?(i64)157:(i64)158);
        };
    };
    (*p).mode = (i64)0;
}

static void mm_type_tx_iandto(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    mm_type_tpass(a,(i64)21,(i64)1);
    mm_type_tpass(b,(i64)(mm_decls_ttbasetype[((i64)((*a).mode))]),(i64)0);
    if (!(!!(mm_lib_isintmode((i64)((*a).mode))))) {
        mm_support_txerror((byte*)"iand: not int",(struct mm_decls_unitrec *)(0));
    };
    (*p).mode = (i64)0;
}

static void mm_type_tx_negto(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    mm_type_tpass(a,(i64)21,(i64)1);
    (*p).mode = (i64)0;
}

static void mm_type_tx_typepun(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    if (((i64)((*a).tag)==(i64)14)) {
        mm_support_txerror((byte*)"TYPEPUN/LIST",(struct mm_decls_unitrec *)(0));
    } else {
        mm_type_tpass(a,(i64)21,(i64)0);
        (*p).mode = (i64)((*p).newmode);
    };
}

static void mm_type_tx_bytesize(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    mm_type_tpass(a,(i64)21,(i64)0);
    (*p).mode = (i64)4;
}

static void mm_type_tx_exit(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    if ((a == 0)) {
        return;
    };
    mm_type_tpass(a,(i64)4,(i64)0);
    if (((i64)((*a).tag) != (i64)1)) {
        mm_support_txerror((byte*)"exit/etc not const",(struct mm_decls_unitrec *)(0));
    };
    (*p).index = (*a).value;
    (*p).a = (struct mm_decls_unitrec *)(0);
}

static void mm_type_tx_goto(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    i64 m;
    mm_type_tpass(a,(i64)21,(i64)0);
    m = (i64)((*a).mode);
    if ((((i64)(mm_decls_ttbasetype[(m)]) != (i64)20) || ((i64)(mm_decls_ttbasetype[((i64)(mm_decls_tttarget[(m)]))]) != (i64)54))) {
        mm_support_txerror((byte*)"goto: not label",(struct mm_decls_unitrec *)(0));
    };
}

static void mm_type_tx_switch(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,struct mm_decls_unitrec * c,i64 t,i64 lv) {
    byte valueset[2001];
    struct mm_decls_unitrec *  wt;
    struct mm_decls_unitrec *  w;
    i64 ax;
    i64 bx;
    i64 i;
    i64 u;
    i64 v;
    if ((((i64)((*p).tag) == (i64)219) && !!(lv))) {
        mm_support_gerror((byte*)"&doswitch",(struct mm_decls_unitrec *)(0));
    };
    mm_type_tpass(a,(i64)4,(i64)0);
    memset((void *)(&valueset),(i64)0,(u64)((i64)2001));
    u = (i64)0;
    wt = b;
    L973 :;
    while (!!(wt)) {
        w = (*wt).a;
        L976 :;
        while (!!(w)) {
            mm_type_tpass(w,(i64)21,(i64)0);
            if (!(!!(mm_lib_isconstunit(w)))) {
                mm_support_txerror((byte*)"Switch not constant",(struct mm_decls_unitrec *)(0));
            };
            if (((i64)(mm_decls_ttbasetype[((i64)((*w).mode))])==(i64)27)) {
                ax = (*(*w).a).value;
                bx = (*(*w).b).value;
                //dorange:
L979 :;
;
                L980 :;
                for (i=ax;i<=bx;++i) {
L981 :;
                    if (((i < (i64)0) || (i > (i64)2000))) {
                        mm_support_txerror((byte*)"switch: value out of range",(struct mm_decls_unitrec *)(0));
                    };
                    if (!!((u64)(valueset[(i)]))) {
                        msysc_m_print_startcon();
                        msysc_m_print_i64(i,NULL);
                        msysc_m_print_newline();
                        msysc_m_print_end();
                        ;
                        mm_support_txerror((byte*)"Duplicate switch value",(struct mm_decls_unitrec *)(0));
                    };
                    valueset[(i)] = (u64)((i64)1);
L982 :;
                }L983 :;
                ;
            } else {
                mm_type_coerceunit(w,(i64)4,(i64)0);
                mm_type_tevaluate(w);
                if (((i64)((*w).tag) != (i64)1)) {
                    mm_support_txerror((byte*)"Switch value: not const int",(struct mm_decls_unitrec *)(0));
                };
                ax = (bx = (*w).value);
                goto L979 ;
;
            };
            w = (*w).nextunit;
L977 :;
        }L978 :;
        ;
        mm_type_tpass((*wt).b,t,lv);
        if ((t == (i64)21)) {
            if (!!(u)) {
                mm_type_getdominantmode((i64)0,u,(i64)((*(*wt).b).mode),&u,&v);
            } else {
                u = (i64)((*(*wt).b).mode);
            };
        };
        wt = (*wt).nextunit;
L974 :;
    }L975 :;
    ;
    if (!!(c)) {
        mm_type_tpass(c,t,lv);
        if ((t == (i64)21)) {
            mm_type_getdominantmode((i64)0,u,(i64)((*c).mode),&u,&v);
        };
    } else if ((t != (i64)0)) {
        mm_support_txerror((byte*)"switch needs else",(struct mm_decls_unitrec *)(0));
    };
    if ((t != (i64)0)) {
        w = (*b).a;
        L984 :;
        while (!!(w)) {
            if ((t == (i64)21)) {
                mm_type_coerceunit((*b).b,u,(i64)0);
            };
            (*w).mode = (i64)((*(*b).b).mode);
            w = (*w).nextunit;
L985 :;
        }L986 :;
        ;
        if ((t == (i64)21)) {
            mm_type_coerceunit(c,u,(i64)0);
            (*p).mode = u;
        } else {
            (*p).mode = t;
        };
    } else {
        (*p).mode = (i64)0;
    };
}

static void mm_type_tx_power(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    i64 u;
    i64 v;
    mm_type_tpass(a,(i64)21,(i64)0);
    mm_type_tpass(b,(i64)21,(i64)0);
    u = (v = (i64)(mm_tables_dominantmode[((i64)((*a).mode))][((i64)((*b).mode))]));
    mm_type_coerceunit(a,u,(i64)0);
    mm_type_coerceunit(b,v,(i64)0);
    (*p).mode = u;
}

static void mm_type_tx_addroffirst(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t) {
    i64 m;
    mm_type_tpass(a,(i64)21,(i64)0);
    m = (i64)((*a).mode);
    if (((i64)(mm_decls_ttbasetype[(m)]) != (i64)29)) {
        mm_support_txerror((byte*)"&. ref[] expected",(struct mm_decls_unitrec *)(0));
    };
    m = mm_lib_createrefmode((struct mm_decls_strec *)(0),(i64)(mm_decls_tttarget[(m)]),(i64)0);
    if (((i64)((*a).tag) == (i64)3)) {
        (*a).addroffirst = (i64)1;
    };
    (*p).mode = m;
}

static void mm_type_tx_minvalue(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    i64 u;
    i64 tmax;
    i64 x;
    struct mm_decls_qint *  aa;
    if (((i64)((*a).tag) == (i64)100)) {
        u = (i64)(mm_decls_ttbasetype[((*a).value)]);
        //dotypeconst:
L987 :;
;
        tmax = (i64)4;
        if (((i64)((*p).tag) == (i64)136)) {
            if ((u==(i64)1)) {
                x = (i64)-128;
            }else if ((u==(i64)2)) {
                x = (i64)-32768;
            }else if ((u==(i64)3)) {
                x = (i64)-2147483648;
            }else if ((u==(i64)4)) {
                x = (i64)(-9223372036854775807-1);
            }else if ((u==(i64)5)) {
                aa = (struct mm_decls_qint *)(mlib_pcm_allocz((i64)16));
                (*aa).lower = (u64)((i64)0);
                (*aa).upper = (i64)((u64)9223372036854775808u);
                x = (i64)(aa);
                tmax = (i64)5;
            }else if ((u==(i64)6) || (u==(i64)7) || (u==(i64)8) || (u==(i64)9) || (u==(i64)10) || (u==(i64)13) || (u==(i64)14)) {
                x = (i64)0;
            } else {
                mm_support_txerror_s((byte*)"Can't do minvalue on #",mm_lib_strmode(u,(i64)1),(struct mm_decls_unitrec *)(0));
            };
        } else {
            if ((u==(i64)1)) {
                x = (i64)127;
            }else if ((u==(i64)2)) {
                x = (i64)32767;
            }else if ((u==(i64)3)) {
                x = (i64)2147483647;
            }else if ((u==(i64)4)) {
                x = (i64)((u64)9223372036854775807u);
            }else if ((u==(i64)5)) {
                aa = (struct mm_decls_qint *)(mlib_pcm_allocz((i64)16));
                (*aa).lower = (u64)18446744073709551615u;
                (*aa).upper = (i64)((u64)9223372036854775807u);
                x = (i64)(aa);
                tmax = (i64)5;
            }else if ((u==(i64)6) || (u==(i64)13)) {
                x = (i64)255;
            }else if ((u==(i64)7) || (u==(i64)14)) {
                x = (i64)65535;
            }else if ((u==(i64)8)) {
                x = (i64)4294967295;
            }else if ((u==(i64)9)) {
                x = (i64)0;
                --x;
                tmax = (i64)9;
            }else if ((u==(i64)10)) {
                aa = (struct mm_decls_qint *)(mlib_pcm_allocz((i64)16));
                (*aa).lower = (u64)(((*aa).upper = (i64)((u64)18446744073709551615u)));
                x = (i64)(aa);
                tmax = (i64)10;
            } else {
                mm_support_txerror_s((byte*)"Can't do maxvalue on #",mm_lib_strmode(u,(i64)1),(struct mm_decls_unitrec *)(0));
            };
        };
        (*p).tag = (i64)1;
        (*p).value = x;
        (*p).a = (struct mm_decls_unitrec *)(0);
        (*p).mode = tmax;
        (*p).isconst = (i64)1;
    } else {
        mm_type_tpass(a,(i64)21,(i64)0);
        u = (i64)(mm_decls_ttbasetype[((i64)((*a).mode))]);
        goto L987 ;
;
    };
}

static void mm_type_tx_return(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t) {
    i64 m;
    i64 nret;
    i64 i;
    struct mm_decls_unitrec *  q;
    m = (i64)((*mm_decls_currproc).mode);
    nret = (i64)((*mm_decls_currproc).nretvalues);
    if ((a == 0)) {
        if (!!(nret)) {
            mm_support_txerror((byte*)"return value(s) missing",(struct mm_decls_unitrec *)(0));
        };
        return;
    } else if ((nret == (i64)0)) {
        mm_support_txerror((byte*)"Superfluous return value",(struct mm_decls_unitrec *)(0));
    };
    if (((i64)((*a).tag) == (i64)14)) {
        (*a).tag = (i64)20;
        if (((i64)((*a).length) != nret)) {
            mm_support_txerror((byte*)"Wrong number of return values",(struct mm_decls_unitrec *)(0));
        };
        q = (*a).a;
        L988 :;
        for (i=(i64)1;i<=nret;++i) {
L989 :;
            mm_type_tpass(q,(i64)((*mm_decls_currproc).modelist[(i)-1]),(i64)0);
            q = (*q).nextunit;
L990 :;
        }L991 :;
        ;
        mm_lib_deleteunit(p,a);
        if ((t == (i64)0)) {
            (*p).mode = (i64)0;
        } else {
            (*p).mode = (i64)55;
        };
    } else {
        if ((nret > (i64)1)) {
            mm_support_txerror((byte*)"RETERROR?",(struct mm_decls_unitrec *)(0));
        };
        mm_type_tpass(a,m,(i64)0);
        if ((t == (i64)0)) {
            (*p).mode = (i64)0;
        } else {
            if (!(!!(mm_decls_ctarget))) {
                mm_lib_deleteunit(p,a);
            } else {
                (*p).mode = (i64)((*a).mode);
            };
        };
    };
}

static void mm_type_tx_dotindex(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,i64 lv) {
    i64 pmode;
    struct mm_decls_unitrec *  i;
    struct mm_decls_unitrec *  j;
    mm_type_tpass(a,(i64)21,lv);
    pmode = (i64)9;
    if (!(!!(mm_lib_isintmode((i64)((*a).mode))))) {
        if (((i64)((*a).mode)==(i64)34)) {
            if (((i64)((*p).tag) == (i64)85)) {
                pmode = (i64)34;
            } else {
                pmode = (i64)15;
            };
        } else {
            mm_support_txerror((byte*)"a.[i]: not int/str value",(struct mm_decls_unitrec *)(0));
        };
    };
    mm_type_tpass(b,(i64)21,(i64)0);
    if (((i64)(mm_decls_ttbasetype[((i64)((*b).mode))])==(i64)27)) {
        i = (*b).a;
        j = (*b).b;
        if ((((i64)((*i).tag) == (i64)((*j).tag)) && ((i64)((*j).tag) == (i64)1))) {
            if (((*i).value > (*j).value)) {
                {struct mm_decls_unitrec *  temp = (*b).a; (*b).a = (*b).b; (*b).b = temp; };
            };
        };
    } else {
        mm_type_coerceunit(b,(i64)4,(i64)0);
    };
    (*p).mode = pmode;
}

static void mm_type_tx_slice(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    mm_type_tpass(a,(i64)21,(i64)0);
    mm_type_tpass(b,(i64)21,(i64)0);
    if (((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)29)) {
        mm_type_tarrayslice(p,a,b);
    }else if (((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)36) || ((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)37) || ((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)34)) {
        (*p).mode = (i64)((*a).mode);
    }else if (((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)28)) {
        mm_support_txerror((byte*)"slice of slice not implemented",(struct mm_decls_unitrec *)(0));
    } else {
        mm_support_txerror((byte*)"a[i..j]: not array",(struct mm_decls_unitrec *)(0));
    };
}

static void mm_type_tx_assign(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b,i64 t) {
    i64 m;
    if (((i64)((*a).tag)==(i64)14)) {
        mm_type_tx_multassign(a,b);
    }else if (((i64)((*a).tag)==(i64)84) || ((i64)((*a).tag)==(i64)85)) {
        mm_type_tx_dotindex(a,(*a).a,(*a).b,(i64)1);
        mm_type_tpass(b,(i64)4,(i64)0);
    } else {
        mm_type_tpass(a,(i64)21,(i64)1);
        m = (i64)((*a).mode);
        if ((mm_lib_gettypecat_t(m) == (i64)42)) {
            mm_type_tpass(b,(i64)(mm_tables_stdtypebase[((i64)(mm_decls_ttbasetype[(m)]))]),(i64)0);
            (*p).mode = m;
            if ((t != (i64)0)) {
                mm_type_twidenopnd(p);
            };
        } else {
            mm_type_tpass(b,m,(i64)0);
            (*p).mode = m;
        };
    };
}

static void mm_type_tx_multassign(struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    struct mm_decls_unitrec *  p;
    struct mm_decls_unitrec *  q;
    struct mm_decls_unitrec *  lhs;
    struct mm_decls_unitrec *  rhs;
    i64 nretmodes;
    i64 i;
    struct mm_decls_strec *  d;
    nretmodes = (i64)0;
    if (((i64)((*b).tag) != (i64)14)) {
        mm_type_tpass(b,(i64)21,(i64)0);
        d = mm_type_getprocretmodes(b);
        nretmodes = (i64)((*d).nretvalues);
        if (((i64)((*a).length) > nretmodes)) {
            mm_support_txerror((byte*)"mult ass/mult returns don't agree in number",(struct mm_decls_unitrec *)(0));
        };
        if ((nretmodes <= (i64)1)) {
            mm_support_txerror((byte*)"mult ass rhs needs fn yielding 2+ values",(struct mm_decls_unitrec *)(0));
        };
        p = (*a).a;
        i = (i64)1;
        L992 :;
        while (!!(p)) {
            mm_type_tpass(p,(i64)21,(i64)1);
            if (((i64)((*p).mode) != (i64)((*d).modelist[(i++)-1]))) {
                mm_support_txerror((byte*)"mult ass/mult fn needs exact type match",(struct mm_decls_unitrec *)(0));
            };
            p = (*p).nextunit;
L993 :;
        }L994 :;
        ;
        return;
    };
    if (((i64)((*a).length) != (i64)((*b).length))) {
        mm_support_txerror((byte*)"Mult assign: count mismatch",(struct mm_decls_unitrec *)(0));
    };
    if (((i64)((*a).length) == (i64)0)) {
        mm_support_txerror((byte*)"Invalid assignment",(struct mm_decls_unitrec *)(0));
    };
    rhs = (*b).a;
    lhs = (*a).a;
    p = lhs;
    L995 :;
    while (!!(p)) {
        mm_type_tpass(p,(i64)21,(i64)1);
        p = (*p).nextunit;
L996 :;
    }L997 :;
    ;
    p = lhs;
    q = rhs;
    L998 :;
    while (!!(q)) {
        mm_type_tpass(q,(i64)((*p).mode),(i64)0);
        p = (*p).nextunit;
        q = (*q).nextunit;
L999 :;
    }L1000 :;
    ;
}

static void mm_type_tx_in(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    mm_type_tpass(a,(i64)21,(i64)0);
    if (!(!!(mm_lib_isintmode((i64)((*a).mode))))) {
        mm_support_txerror((byte*)"in opnd must be int",(struct mm_decls_unitrec *)(0));
    };
    mm_type_tpass(b,(i64)21,(i64)0);
    (*p).mode = (i64)4;
    if (((i64)((*b).tag)==(i64)15)) {
        if (((i64)((*p).tag) == (i64)50)) {
            (*p).tag = (i64)52;
            mm_lib_insertunit(p,(i64)12);
        } else {
            (*p).tag = (i64)52;
        };
    }else if (((i64)((*b).tag)==(i64)16)) {
        if (((i64)((*p).tag) == (i64)50)) {
            (*p).tag = (i64)53;
            mm_lib_insertunit(p,(i64)12);
        } else {
            (*p).tag = (i64)53;
        };
    } else {
        if (((i64)(mm_decls_tttypecode[((i64)((*b).mode))])==(i64)73) || ((i64)(mm_decls_tttypecode[((i64)((*b).mode))])==(i64)85)) {
        } else {
            mm_support_txerror((byte*)"in rhs must be range/set",(struct mm_decls_unitrec *)(0));
        };
    };
    (*p).mode = (i64)4;
}

static struct mm_decls_strec * mm_type_getprocretmodes(struct mm_decls_unitrec * p) {
    struct mm_decls_unitrec *  a;
    if (((i64)((*p).tag) != (i64)25)) {
        mm_support_txerror((byte*)"multass/need multfn",(struct mm_decls_unitrec *)(0));
    };
    a = (*p).a;
    if (((i64)((*a).tag)==(i64)3)) {
        return (*a).def;
    } else {
        return mm_decls_ttnamedef[((i64)(mm_decls_tttarget[((i64)((*a).mode))]))];
    };
}

static void mm_type_tx_exprlist(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 t) {
    struct mm_decls_unitrec *  q;
    q = a;
    L1001 :;
    while ((!!(q) && !!((*q).nextunit))) {
        mm_type_tpass(q,(i64)21,(i64)0);
        q = (*q).nextunit;
L1002 :;
    }L1003 :;
    ;
    mm_type_tpass(q,t,(i64)0);
    (*p).mode = (i64)((*q).mode);
}

static void mm_type_tx_sign(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a) {
    mm_type_tpass(a,(i64)21,(i64)0);
    if (!!((u64)(mm_decls_ttisreal[((i64)((*a).mode))]))) {
        mm_type_coerceunit(a,(i64)12,(i64)0);
        (*p).mode = (i64)12;
    } else {
        mm_type_coerceunit(a,(i64)4,(i64)0);
        (*p).mode = (i64)4;
    };
}

static void mm_type_fixvoidunit(struct mm_decls_unitrec * a) {
    if (((i64)((*a).tag)==(i64)23)) {
        (*a).tag = (i64)192;
    }else if (((i64)((*a).tag)==(i64)196) || ((i64)((*a).tag)==(i64)197) || ((i64)((*a).tag)==(i64)216) || ((i64)((*a).tag)==(i64)218) || ((i64)((*a).tag)==(i64)191) || ((i64)((*a).tag)==(i64)221) || ((i64)((*a).tag)==(i64)4)) {
        if (((i64)((*a).mode) != (i64)0)) {
            (*a).popflag = (i64)1;
        };
    }else if (((i64)((*a).tag)==(i64)24)) {
        (*a).tag = (i64)194;
    }else if (((i64)((*a).tag)==(i64)191)) {
        if (!!((*a).a)) {
        };
    } else {
        if (((i64)((*a).mode) == (i64)0)) {
        } else {
            (*a).popflag = (i64)1;
        };
    };
}

static void mm_type_twiden(struct mm_decls_unitrec * p,i64 lv) {
    i64 m;
    i64 mbase;
    mbase = (i64)(mm_decls_ttbasetype[((m = (i64)((*p).mode)))]);
    if ((lv==(i64)1)) {
    }else if ((lv==(i64)3) || (lv==(i64)4)) {
        if (!(!!((u64)(mm_decls_ttisflexvar[(m)])))) {
            if ((((lv == (i64)4) || (((u64)(mm_tables_stdtypecode[(mbase)]) == (u64)65u) && ((i64)((u64)(mm_tables_stdtypecat[(mbase)])) == (i64)48))) || (mbase == (i64)32))) {
                if (((i64)((*p).tag) == (i64)93)) {
                    mm_lib_deleteunit(p,(*p).a);
                } else {
                    mm_lib_insertunit(p,(i64)94);
                    (*p).mode = mm_lib_createrefmode((struct mm_decls_strec *)(0),m,(i64)0);
                };
            };
        };
    }else if ((lv==(i64)2)) {
        mm_support_txerror((byte*)"widen/addrof",(struct mm_decls_unitrec *)(0));
    };
}

static i64 mm_type_twidenshort(struct mm_decls_unitrec * p) {
    if (((i64)((*p).tag) == (i64)1)) {
        (*p).mode = (i64)(mm_tables_stdtypebase[((i64)(mm_decls_ttbasetype[((i64)((*p).mode))]))]);
        return (i64)((*p).mode);
    };
    mm_lib_insertunit(p,(i64)96);
    if (((i64)(((*p).newmode = (i64)(mm_tables_stdtypebase[((i64)(mm_decls_ttbasetype[((i64)((*p).mode))]))])))==(i64)4)) {
        (*p).opcode = (i64)4;
    }else if (((i64)(((*p).newmode = (i64)(mm_tables_stdtypebase[((i64)(mm_decls_ttbasetype[((i64)((*p).mode))]))])))==(i64)9) || ((i64)(((*p).newmode = (i64)(mm_tables_stdtypebase[((i64)(mm_decls_ttbasetype[((i64)((*p).mode))]))])))==(i64)15)) {
        (*p).opcode = (i64)5;
    };
    (*p).mode = (i64)((*p).newmode);
    return (i64)((*p).mode);
}

static void mm_type_tx_head(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    mm_type_tpass(a,(i64)21,(i64)0);
    (*p).mode = (i64)((*a).mode);
    if (((i64)((*p).tag)==(i64)79)) {
        (*p).mode = mm_lib_createrefmode((struct mm_decls_strec *)(0),(i64)6,(i64)0);
    }else if (((i64)((*p).tag)==(i64)80)) {
        (*p).mode = mm_tables_trefchar;
    };
    if (((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)34) || ((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)36) || ((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)37) || ((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)50)) {
    } else {
        mm_support_txerror((byte*)"head/etc can't be used with this type",(struct mm_decls_unitrec *)(0));
    };
    if (((i64)((*p).tag)==(i64)68) || ((i64)((*p).tag)==(i64)69) || ((i64)((*p).tag)==(i64)62) || ((i64)((*p).tag)==(i64)63) || ((i64)((*p).tag)==(i64)77) || ((i64)((*p).tag)==(i64)78)) {
        if ((b == 0)) {
            if (((i64)((*p).tag)==(i64)62) || ((i64)((*p).tag)==(i64)63)) {
                (*p).b = mm_lib_createconstunit((u64)((i64)1),(i64)4);
            }else if (((i64)((*p).tag)==(i64)77) || ((i64)((*p).tag)==(i64)78)) {
                (*p).b = mm_lib_createconstunit((u64)((i64)-1),(i64)4);
            } else {
                mm_support_txerror((byte*)"count missing",(struct mm_decls_unitrec *)(0));
            };
        } else {
            mm_type_tpass(b,(i64)4,(i64)0);
        };
    } else {
        if (!!(b)) {
            mm_support_txerror((byte*)"Extra opnd",(struct mm_decls_unitrec *)(0));
        };
    };
}

static void mm_type_tx_concat(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    i64 u;
    i64 v;
    mm_type_tpass(a,(i64)21,(i64)0);
    mm_type_tpass(b,(i64)21,(i64)0);
    (*p).mode = (i64)((*a).mode);
    if (((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)34) || ((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)36) || ((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)37) || ((i64)(mm_decls_ttbasetype[((i64)((*a).mode))])==(i64)50)) {
    } else {
        mm_support_txerror((byte*)"head/etc can't be used with this type",(struct mm_decls_unitrec *)(0));
    };
    mm_type_getdominantmodepp(p,a,b,&u,&v);
    mm_type_coerceunit(a,u,(i64)0);
    mm_type_coerceunit(b,v,(i64)0);
}

static void mm_type_twidenopnd(struct mm_decls_unitrec * p) {
    if (!!((u64)(mm_decls_ttisshortint[((i64)((*p).mode))]))) {
        mm_type_twidenshort(p);
    };
}

static void mm_type_joinstrings(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    i64 newlen;
    i64 alen = (i64)((*a).slength);
    i64 blen = (i64)((*b).slength);
    byte *  newstr;
    alen = (i64)((*a).slength);
    blen = (i64)((*b).slength);
    newlen = (alen + blen);
    newstr = (byte *)(mlib_pcm_alloc((newlen + (i64)1)));
    if (!!(alen)) {
        memcpy((void *)(newstr),(void *)((*a).svalue),(u64)(alen));
    };
    if (!!(blen)) {
        memcpy((void *)((newstr + alen)),(void *)((*b).svalue),(u64)(blen));
    };
    (*((newstr + alen) + blen)) = (u64)0u;
    (*a).svalue = newstr;
    (*a).slength = newlen;
    mm_lib_deleteunit(p,a);
}

static void mm_type_removeaddrof(struct mm_decls_unitrec * p) {
    if ((p == 0)) {
        return;
    };
    if (((i64)((*p).tag)==(i64)94)) {
        mm_lib_deleteunit(p,(*p).a);
    }else if (((i64)((*p).tag)==(i64)196)) {
        mm_type_removeaddrof((*p).b);
        mm_type_removeaddrof((*p).c);
    } else {
        mm_support_txerror((byte*)"dot/flex: complex record expr, can't remove &",(struct mm_decls_unitrec *)(0));
    };
}

static void mm_type_twholearrayslice(struct mm_decls_unitrec * p,i64 slicemode) {
    i64 m;
    i64 marray;
    i64 length;
    m = mm_lib_createrefmode((struct mm_decls_strec *)(0),(marray = (i64)((*p).mode)),(i64)0);
    length = (i64)(mm_decls_ttlength[(marray)]);
    mm_lib_insertunit(p,(i64)94);
    (*p).mode = m;
    mm_lib_insertunit(p,(i64)14);
    (*p).length = (i64)2;
    (*(*p).a).nextunit = mm_lib_createconstunit((u64)(length),(i64)4);
    (*p).mode = slicemode;
}

static void mm_type_tarrayslice(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,struct mm_decls_unitrec * b) {
    struct mm_decls_unitrec *  bi;
    struct mm_decls_unitrec *  bj;
    struct mm_decls_unitrec *  plength;
    i64 m;
    i64 marray;
    bi = (*b).a;
    bj = (*b).b;
    marray = (i64)((*a).mode);
    m = (i64)(mm_decls_tttarget[(marray)]);
    mm_type_tpass(a,(i64)21,(i64)3);
    mm_lib_insertunit(a,(i64)82);
    (*a).mode = m;
    (*a).b = mm_lib_duplunit(bi,(i64)0);
    mm_lib_insertunit(a,(i64)94);
    (*a).mode = mm_lib_createrefmode((struct mm_decls_strec *)(0),m,(i64)0);
    (*p).tag = (i64)14;
    (*p).mode = mm_lib_createslicemodek(mm_decls_currproc,m,(i64)1,(i64)0);
    (*p).length = (i64)2;
    (*p).b = (struct mm_decls_unitrec *)(0);
    plength = mm_lib_createunit2((i64)38,bj,bi);
    (*plength).mode = (i64)4;
    mm_type_tevalbinop(plength);
    plength = mm_lib_createunit2((i64)37,plength,mm_lib_createconstunit((u64)((i64)1),(i64)4));
    (*plength).mode = (i64)4;
    mm_type_tevalbinop(plength);
    (*(*p).a).nextunit = plength;
    mm_diags_printunit(p,(i64)0,(byte*)"*",0);
}

static void mm_type_tx_bitfield(struct mm_decls_unitrec * p,struct mm_decls_unitrec * a,i64 lv) {
    i64 i;
    i64 j;
    i64 bitsize;
    i64 topbit;
    struct mm_decls_unitrec *  r;
    mm_type_tpass(a,(i64)21,lv);
    if (!(!!((u64)(mm_decls_ttisinteger[((i64)((*a).mode))])))) {
        mm_support_txerror((byte*)"Int needed",(struct mm_decls_unitrec *)(0));
    };
    bitsize = (i64)(mm_decls_ttbitwidth[((i64)(mm_decls_ttbasetype[((i64)((*a).mode))]))]);
    topbit = (bitsize - (i64)1);
    if (((i64)((*p).opcode)==(i64)2)) {
        i = (i64)0;
        j = (i64)7;
    }else if (((i64)((*p).opcode)==(i64)1)) {
        j = topbit;
        i = (topbit - (i64)7);
    }else if (((i64)((*p).opcode)==(i64)4)) {
        i = (j = (i64)0);
    }else if (((i64)((*p).opcode)==(i64)7) || ((i64)((*p).opcode)==(i64)8)) {
        if (!!(lv)) {
            mm_support_txerror((byte*)"Can't assign",(struct mm_decls_unitrec *)(0));
        };
        i = (j = (i64)0);
    }else if (((i64)((*p).opcode)==(i64)3)) {
        i = (j = topbit);
    }else if (((i64)((*p).opcode)==(i64)6)) {
        i = (i64)0;
        j = ((bitsize / (i64)2) - (i64)1);
    }else if (((i64)((*p).opcode)==(i64)5)) {
        i = (bitsize / (i64)2);
        j = topbit;
    };
    if ((i == j)) {
        (*p).tag = (i64)84;
        (*p).b = mm_lib_createconstunit((u64)(i),(i64)4);
        if (((i64)((*p).opcode) == (i64)8)) {
            (*p).mode = (i64)9;
            mm_lib_insertunit(p,(i64)12);
        };
    } else {
        r = mm_lib_createunit2((i64)15,mm_lib_createconstunit((u64)(i),(i64)4),mm_lib_createconstunit((u64)(j),(i64)4));
        (*r).mode = (i64)27;
        (*p).tag = (i64)85;
        (*p).b = r;
    };
    (*p).mode = (i64)9;
}


/* ********** End of C Code ********** */
