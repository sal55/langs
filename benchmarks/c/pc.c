/*
Info header for C distribution.
Project: Q Byte-Code Interpreter (Q Compiler is separate)

Typical Build Instructions:

  gcc -O3 pc_win64.c -opc.exe         # Windows
  gcc -m64 -O3 pc_lin64.c -opc -lm -ldl    # Linux

Actual program name varies according to target OS and target word size.
The -m64 or -m32 flag should match that in the file name, or it can be
omitted when gcc defaults to the right value.

This program is an interpreter for the Q language.

It runs precompiled bytecode programs, or it can also compile Q source
programs into single-file .pc bytecode files.

Run as follows:


  pc hello.q      # Compile hello.q to hello.pc then run hello.pc

  pc hello.pc     # Run hello.pc (created from prior compilations)

  pc hello        # Run hello.pc if present else hello.q
                  # Unless hello.q is newer then always compile/run that
                  # (Requires Windows target to compare file times)

Test hello.q file:

  proc start =
    println "Hello, World!"
  end

*/

/*
  M to C  Whole Program Translator
  Input:  pc.m plus imported modules
  Output: \bench\c\pc.c (this file, or renamed from that)
          File represents entire program
  Target: C 64-bit
  OS:     Windows

  Modules:
  Module 1: pc.m
  Module 2: <Built-in: msysnewc.m>
  Module 3: <Built-in: clibnewc.m>
  Module 4: <Built-in: mlib.m>
  Module 5: <Built-in: oswindows.m>
  Module 6: ./pci.m
  Module 7: ./pc_types.m
  Module 8: ./pc_decls.m
  Module 9: ./pq_common.m
  Module 10: ./pc_support.m
  Module 11: ./pc_misc.m
  Module 12: ./pc_pcfns.m
  Module 13: ./pc_objlib.m
  Module 14: ./pc_bignum.m
  Module 15: c:/mx/mbignum.m
  Module 16: ./pc_print.m
  Module 17: ./pc_jhandlers.m
  Module 18: ./pc_oslayer.m
  Module 19: <Built-in: oswindllc.m>
  Module 20: ./pc_host.m
  Module 21: ./pc_dxfns.m
  Module 22: ./pc_khandlers.m
  Module 23: ./pc_assemc.m

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
struct msysnewc_procinforec;
struct msysnewc_fmtrec;
struct mlib_strbuffer;
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
struct pc_decls_uflagsrec;
struct pc_decls_fieldrec;
struct pc_decls_strec;
struct pc_decls_listrec;
struct pc_decls_stringrec;
struct pc_decls_recordrec;
struct pc_decls_decimalrec;
struct pc_decls_setrec;
struct pc_decls_dictrec;
struct pc_decls_arrayrec;
struct pc_decls_bitsrec;
struct pc_decls_structrec;
struct pc_decls_objrec;
struct pc_decls_exceptionrec;
struct pc_decls_returnrec;
struct pc_decls_refrec;
struct pc_decls_operatorrec;
struct pc_decls_iterrec;
struct pc_decls_varrec;
struct pc_decls_genfieldnamerec;
struct pc_decls_genfielddatarec;
struct pc_decls_modulerec;
struct pc_decls_dllprocrec;
struct pc_decls_applprocrec;
struct pc_decls_procrec;
struct pc_decls_fmtrec;
struct mbignum_bignumrec;
struct mbignum_constrec;
struct pc_host_dimrec;
struct pc_host_overloadrec;
struct pc_host_pch_setoverload_rec;

/* Struct Definitions */
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

struct mlib_strbuffer {
    byte *  strptr;
    i32 length;
    i32 allocated;
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

struct pc_decls_uflagsrec {
    byte codes[7];
    byte ulength;
};

struct pc_decls_fieldrec {
    byte *  name;
    i16 recordtype;
    i16 fieldtype;
    i32 fieldoffset;
};

struct pc_decls_strec {
    byte *  name;
    struct pc_decls_strec* owner;
    struct pc_decls_strec* deflist;
    struct pc_decls_strec* nextdef;
    byte *  metadata;
    byte symbol;
    byte nameid;
    i16 subcode;
    i16 mode;
    byte ax_at;
    byte ax_moduleno;
    i32 index;
    union {
        void *  address;
        i32 offset;
        u64 *  pcaddress;
    };
};

struct pc_decls_listrec {
    u32 refcount;
    u16 tag;
    struct {
        byte objtype;
        byte mutable;
    };
    union {
        struct pc_decls_varrec *  vptr;
        u64 padding1;
    };
    u32 length;
    i32 lower;
    union {
        struct pc_decls_objrec *  objptr2;
        u32 allocated;
        u64 padding2;
    };
};

struct pc_decls_stringrec {
    u32 refcount;
    u16 tag;
    struct {
        byte objtype;
        byte mutable;
    };
    union {
        byte *  strptr;
        u64 padding1;
    };
    i32 length;
    i32 spare3;
    union {
        struct pc_decls_objrec *  objptr2;
        u32 allocated;
    };
};

struct pc_decls_recordrec {
    u32 refcount;
    u16 tag;
    struct {
        byte spare;
        byte mutable;
    };
    union {
        struct pc_decls_varrec *  vptr;
        byte *  ptr;
        u64 padding1;
    };
    u32 length;
    i32 lower;
    i64 spare2;
};

struct pc_decls_decimalrec {
    u32 refcount;
    u16 tag;
    byte spare1;
    byte spare2;
    union {
        void *  bnptr;
        u64 padding1;
    };
    i64 spare3;
    i64 spare4;
};

struct pc_decls_setrec {
    u32 refcount;
    u16 tag;
    struct {
        byte spare;
        byte mutable;
    };
    union {
        byte *  ptr;
        u64 padding1;
    };
    u32 length;
    i16 lower;
    i16 elemtag;
    u64 allocated64;
};

struct pc_decls_dictrec {
    u32 refcount;
    u16 tag;
    struct {
        byte spare;
        byte mutable;
    };
    union {
        struct pc_decls_varrec *  vptr;
        u64 padding1;
    };
    u32 length;
    i32 lower;
    union {
        struct {
            u32 allocated;
            u32 dictitems;
        };
        struct pc_decls_objrec *  objptr2;
    };
};

struct pc_decls_arrayrec {
    u32 refcount;
    u16 tag;
    struct {
        byte objtype;
        byte mutable;
    };
    union {
        byte *  ptr;
        u64 padding1;
    };
    u32 length;
    i16 lower;
    i16 elemtag;
    union {
        struct pc_decls_objrec *  objptr2;
        u32 allocated;
    };
};

struct pc_decls_bitsrec {
    u32 refcount;
    u16 tag;
    struct {
        byte objtype;
        byte mutable;
    };
    union {
        byte *  ptr;
        u64 padding1;
    };
    u32 length;
    i16 lower;
    byte elemtag;
    byte bitoffset;
    union {
        struct pc_decls_objrec *  objptr2;
        u64 allocated64;
    };
};

struct pc_decls_structrec {
    u32 refcount;
    u16 tag;
    struct {
        byte spare;
        byte mutable;
    };
    union {
        byte *  ptr;
        u64 padding1;
    };
    u32 length;
    i16 lower;
    i16 elemtag;
    union {
        struct pc_decls_objrec *  objptr2;
        u32 allocated;
    };
};

struct pc_decls_objrec {
    union {
        struct {
            u32 refcount;
            u16 tag;
            byte objtype;
            byte spare1;
            u64 spare2;
            u64 spare3;
            union {
                struct pc_decls_objrec* objptr2;
                u64 dummy4;
            };
        };
        struct pc_decls_listrec ulist;
        struct pc_decls_stringrec ustr;
        struct pc_decls_recordrec urec;
        struct pc_decls_decimalrec udec;
        struct pc_decls_setrec uset;
        struct pc_decls_dictrec udict;
        struct pc_decls_arrayrec uarray;
        struct pc_decls_bitsrec ubits;
        struct pc_decls_structrec ustruct;
    };
};

struct pc_decls_exceptionrec {
    union {
        struct {
            u16 tag;
            byte hasref;
            byte exceptiontype;
        };
    };
    struct {
        i16 frameoffset;
        i16 nexceptions;
    };
    byte *  ptr;
};

struct pc_decls_returnrec {
    union {
        struct {
            u16 tag;
            byte hasref;
            byte stackadj;
        };
    };
    i32 frameptr_low;
    u64 *  retaddr;
};

struct pc_decls_refrec {
    union {
        struct {
            u16 tag;
            byte hasref;
            byte spare1;
        };
    };
    struct {
        u16 elemtag;
        byte bitoffset;
        byte bitlength;
    };
    union {
        byte *  ptr;
        i64 *  ptr64;
    };
};

struct pc_decls_operatorrec {
    union {
        struct {
            u16 tag;
            byte hasref;
            byte opdims;
        };
    };
    u32 spare2;
    i64 opcode;
};

struct pc_decls_iterrec {
    union {
        struct {
            u16 tag;
            byte hasref;
            byte opdims;
        };
    };
    u32 itcount;
    byte ittype;
    byte spare3[3];
};

struct pc_decls_varrec {
    union {
        struct {
            union {
                struct {
                    u16 tag;
                    byte hasref;
                    byte spare1;
                };
                u32 tagx;
            };
            u32 spare2;
            union {
                i64 value;
                double xvalue;
                u64 uvalue;
                struct {
                    i32 range_lower;
                    i32 range_upper;
                };
                struct pc_decls_objrec *  objptr;
                struct pc_decls_varrec* varptr;
                byte *  refptr;
            };
        };
        struct pc_decls_exceptionrec uexcept;
        struct pc_decls_returnrec uret;
        struct pc_decls_refrec uref;
        struct pc_decls_operatorrec uop;
        struct pc_decls_iterrec uiter;
    };
};

struct pc_decls_genfieldnamerec {
    byte *  name;
    i32 dataindex;
    union {
        i32 datalength;
        i32 datalast;
    };
};

struct pc_decls_genfielddatarec {
    i32 fieldindex;
    i32 recordtype;
    i32 fieldtype;
    union {
        i32 offset;
        u32 index;
        u32 procoffset;
    };
};

struct pc_decls_modulerec {
    byte *  name;
    byte *  filename;
    byte *  sourcecode;
    u64 (*pccode)[];
    u16 (*linetable)[];
    i32 sourcelen;
    i32 npccode;
    i32 pcindex;
    i32 level;
    i32 exported;
    byte importmap[50];
};

struct pc_decls_dllprocrec {
    byte *  name;
    void (*address)(void);
    i32 dllindex;
};

struct pc_decls_applprocrec {
    byte *  name;
    void (*address)(void);
    struct msysnewc_procinforec *  info;
};

struct pc_decls_procrec {
    struct pc_decls_strec *  def;
    struct pc_decls_procrec* nextproc;
};

struct pc_decls_fmtrec {
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
    byte showtype;
    byte spare[2];
};

struct mbignum_bignumrec {
    i32 (*num)[];
    i64 length;
    i64 expon;
    i32 neg;
    i32 numtype;
};

struct mbignum_constrec {
    i64 value;
    struct mbignum_bignumrec *  bnvalue;
    struct mbignum_constrec* nextconst;
};

struct pc_host_dimrec {
    i64 lbound;
    i64 upper;
    i64 length;
};

struct pc_host_overloadrec {
    i64 optype;
    i64 optype2;
    u64 *  pchandler;
    struct pc_host_overloadrec* nextrec;
};

struct pc_host_pch_setoverload_rec {
    i64 cmd;
    void *  tableptr;
    void *  handleptr;
    struct pc_host_overloadrec * *  ovlist;
};


/* PROCDECLS */
void start(void);
static void pc_getinputoptions(i64 * filetype);
static void pc_do_option(i64 sw,byte * value);
static void pc_getsyscmdline(i64 n);
static void pc_showcaption(void);
static void pc_showhelp(void);
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
i64 pci_runpcl(byte * filename,i64 filetype);
void pci_run_initdata(void);
i64 pci_runpcprogram(void);
static void pci_initbytecode(void);
static void pci_disploop(void);
static void pci_pclinit(void);
static void pci_fixup_all_pc(void);
static void pci_fixup_module_pc(i64 mx);
static i64 * pci_disploop_fn(i64 n);
static void pci_disploop_deb(void);
void pci_runproc(void * fnptr,struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * dest);
static void pci_allocatestrings(void);
byte * pci_compileq(byte * qfilename);
static i64 pci_loadpcfile(byte * s);
void pci_initpcldata(void);
static struct pc_decls_strec * pci_createstentry(i64 index,byte * name,i64 owner,i64 id);
static u64 * pci_getprocaddr(i64 n);
void pci_pcl_initusertypes(void);
void pci_setcmdparam(i64 index,byte * s);
void pc_support_prterror(byte * mess);
i64 pc_support_testelem(byte (*p)[],i64 n);
void pc_support_setelem(byte (*p)[],i64 n);
void pc_support_pcustype_def(byte * mess,struct pc_decls_varrec * x);
u64 * pc_support_pcustype(byte * mess,struct pc_decls_varrec * x);
u64 * pc_support_pcustypet(byte * mess,i64 t);
void pc_support_pcmxtypes_def(byte * mess,struct pc_decls_varrec * x,struct pc_decls_varrec * y);
u64 * pc_support_pcmxtypes(byte * mess,struct pc_decls_varrec * x,struct pc_decls_varrec * y);
u64 * pc_support_pcmxtypestt(byte * mess,i64 s,i64 t);
byte * pc_support_gettypename(i64 t);
void pc_support_inittypetables(void);
u64 * pc_support_pcerror(byte * mess);
void pc_support_vxunimpl(byte * mess);
void pc_support_pclunimpl(i64 cmd);
byte * pc_support_convcstring(byte * svalue,i64 length);
i64 pc_support_getintvalue(struct pc_decls_varrec * p);
i64 pc_support_nextpoweroftwo(i64 x);
void pc_support_showlinenumber(void);
static void pc_support_printlinenumber(i64 lineno,i64 moduleno,byte * calledfrom);
void pc_support_findlinenumber(u64 * ptr,i64 * lineno,i64 * moduleno);
i64 pc_support_findpcindex(u64 * ptr,i64 * moduleno);
void pc_support_showlinetable(byte * caption,i64 i);
void pc_support_writezstring(void * f,byte * s);
void pc_support_writezint(void * f,i64 x);
void pc_support_writezint4(void * f,i64 x);
void pc_support_writezrange(void * f,byte * p);
void pc_support_writezreal(void * f,double x);
void pc_support_writezeof(void * f);
static void pc_support_zerror(byte * mess);
i64 pc_support_readzvalue(byte * * pp,i32 * dest,i32 * dest2);
i64 pc_support_readzint(byte * * p);
i64 pc_support_readzdint(byte * * p);
double pc_support_readzreal(byte * * p);
byte * pc_support_readzstring(byte * * p,i64 * ilength);
byte * pc_support_readzblock(byte * * pp,i64 length);
void pc_support_checkmt(i64 id);
i64 pc_support_ipower(i64 a,i64 n);
void pc_support_loaderror(byte * mess,byte * mess2);
i64 pc_support_gettypesig(i64 s,i64 t);
byte * pc_support_getfnname(void * fnaddr);
void pc_support_junimpl(byte * s);
u64 * pc_misc_raiseexception(i64 exceptno);
void pc_misc_raise_error(i64 error_no);
static void pc_misc_default_exception(i64 exceptno);
void pc_pcfns_pc_unshare(struct pc_decls_varrec * p);
void pc_pcfns_pc_free(struct pc_decls_varrec * p);
struct pc_decls_varrec * pc_pcfns_pc_share(struct pc_decls_varrec * p);
void pc_pcfns_pc_dupl(struct pc_decls_varrec * p);
void pc_pcfns_j_free_s(struct pc_decls_varrec * p);
void pc_pcfns_j_free_m(struct pc_decls_varrec * p);
void pc_pcfns_j_free_l_d(struct pc_decls_varrec * p);
void pc_pcfns_j_free_k(struct pc_decls_varrec * p);
void pc_pcfns_j_free_a(struct pc_decls_varrec * p);
void pc_pcfns_j_free_j(struct pc_decls_varrec * p);
void pc_pcfns_j_free_b_e(struct pc_decls_varrec * p);
void pc_pcfns_j_dupl_s(struct pc_decls_varrec * p);
void pc_pcfns_j_dupl_l_m_d(struct pc_decls_varrec * p);
void pc_pcfns_j_dupl_a(struct pc_decls_varrec * p);
void pc_pcfns_j_dupl_j(struct pc_decls_varrec * p);
void pc_pcfns_j_dupl_b(struct pc_decls_varrec * p);
void pc_pcfns_j_dupl_e(struct pc_decls_varrec * p);
void pc_pcfns_j_dupl_k(struct pc_decls_varrec * p);
void pc_pcfns_pc_makelist(i64 n,struct pc_decls_varrec * a,struct pc_decls_varrec * b,i64 lower);
void pc_pcfns_pc_makerecord(i64 n,i64 t,struct pc_decls_varrec * a,struct pc_decls_varrec * b);
void pc_pcfns_pc_makearray(i64 n,i64 arraytype,i64 elemtype,i64 lower,struct pc_decls_varrec * a,struct pc_decls_varrec * b);
void pc_pcfns_pc_makerange(struct pc_decls_varrec * x,struct pc_decls_varrec * y,struct pc_decls_varrec * z);
void pc_pcfns_pc_makeset(i64 n,struct pc_decls_varrec * data,struct pc_decls_varrec * dest);
void pc_pcfns_pc_makestruct(i64 n,i64 t,struct pc_decls_varrec * a,struct pc_decls_varrec * b);
void pc_pcfns_pc_makedict(i64 n,struct pc_decls_varrec * a,struct pc_decls_varrec * b);
void pc_pcfns_pc_storepacked(byte * p,struct pc_decls_varrec * q,i64 t);
static void pc_pcfns_adddictitem(struct pc_decls_varrec * d,struct pc_decls_varrec * p,struct pc_decls_varrec * q);
i64 pc_pcfns_gethashvalue(struct pc_decls_varrec * p);
struct pc_decls_varrec * pc_pcfns_finddictitem(struct pc_decls_varrec * vd,struct pc_decls_varrec * p,i64 doins);
static void pc_pcfns_expanddict(struct pc_decls_varrec * vd);
static void pc_pcfns_setfslength(byte * s,i64 m,i64 n);
i64 pc_pcfns_getfslength(byte * s,i64 m);
void pc_pcfns_pc_storeptr(struct pc_decls_varrec * p,struct pc_decls_varrec * q);
void pc_pcfns_pc_storebit(byte * p,i64 shift,struct pc_decls_varrec * q,i64 t,i64 bitlength);
void pc_pcfns_pc_popptrlist(struct pc_decls_varrec * p,struct pc_decls_varrec * q);
void pc_pcfns_pc_loadpacked(void * p,i64 t,struct pc_decls_varrec * dest,struct pc_decls_objrec * ownerobj);
void pc_pcfns_pc_loadbit(byte * p,i64 shift,i64 t,i64 bitlength,struct pc_decls_varrec * dest);
void pc_pcfns_pc_loadptr(struct pc_decls_varrec * x,struct pc_decls_varrec * y);
void pc_pcfns_pc_storestring(struct pc_decls_varrec * p,struct pc_decls_varrec * q);
void pc_pcfns_pc_iconvert(i64 t,struct pc_decls_varrec * x);
void pc_pcfns_pc_iconvcase(struct pc_decls_varrec * a,struct pc_decls_varrec * b,i64 upper);
i64 pc_pcfns_pc_eqstring_nf(struct pc_decls_varrec * x,struct pc_decls_varrec * y);
i64 pc_pcfns_pc_equal_nf(struct pc_decls_varrec * x,struct pc_decls_varrec * y,i64 shallow);
i64 pc_pcfns_comparebytes(byte * p,byte * q,i64 n);
i64 pc_pcfns_pc_compare_nf(struct pc_decls_varrec * x,struct pc_decls_varrec * y);
i64 pc_pcfns_cmpstring_len(byte * s,byte * t,i64 slen,i64 tlen);
i64 pc_pcfns_pc_eqstring(struct pc_decls_varrec * x,struct pc_decls_varrec * y);
i64 pc_pcfns_pc_equal(struct pc_decls_varrec * x,struct pc_decls_varrec * y,i64 shallow);
i64 pc_pcfns_pc_compare(struct pc_decls_varrec * x,struct pc_decls_varrec * y);
i64 pc_pcfns_u8inarray(byte a,struct pc_decls_objrec * p);
i64 pc_pcfns_u16inarray(u16 a,struct pc_decls_objrec * p);
i64 pc_pcfns_u32inarray(u32 a,struct pc_decls_objrec * p);
i64 pc_pcfns_u64inarray(u64 a,struct pc_decls_objrec * p);
i64 pc_pcfns_bitinbits(byte a,struct pc_decls_objrec * p);
i64 pc_pcfns_pc_strinstr(struct pc_decls_varrec * x,struct pc_decls_varrec * y);
byte * pc_pcfns_getbitoffset(byte * p,i64 offset,i64 index,i64 t,byte * newoffset);
void pc_pcfns_pc_iappendlist(struct pc_decls_varrec * a,struct pc_decls_varrec * b);
void pc_pcfns_pc_iappendarray(struct pc_decls_varrec * a,struct pc_decls_varrec * b);
void pc_pcfns_pc_mul_listi(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c);
void pc_pcfns_pc_mul_stri(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c);
void pc_pcfns_pc_duplvar(struct pc_decls_varrec * p);
void pc_pcfns_pc_iconcatlist(struct pc_decls_varrec * a,struct pc_decls_varrec * b);
void pc_pcfns_pc_iappendbits(struct pc_decls_varrec * a,struct pc_decls_varrec * b);
void pc_pcfns_pc_makestring(byte * s,i64 length,struct pc_decls_varrec * dest);
void pc_pcfns_pc_makestringx(byte * s,i64 length,i64 allocated,struct pc_decls_varrec * dest);
void pc_pcfns_pc_makestringn(i64 length,struct pc_decls_varrec * dest);
void pc_pcfns_pc_emptystring(struct pc_decls_varrec * dest);
void pc_pcfns_pc_makechar(i64 ch,struct pc_decls_varrec * dest);
static void pc_objlib_Dinit(void);
struct pc_decls_objrec * pc_objlib_obj_new(i64 tag);
void pc_objlib_freeobject(struct pc_decls_objrec * p);
struct pc_decls_objrec * pc_objlib_array_new(i64 ta,i64 elemtype,i64 length,i64 lower);
struct pc_decls_objrec * pc_objlib_list_new(i64 length,i64 lower,struct pc_decls_varrec * defval);
void pc_objlib_objtovar(struct pc_decls_objrec * p,struct pc_decls_varrec * q);
struct pc_decls_objrec * pc_objlib_set_new(i64 length,i64 lower);
struct pc_decls_objrec * pc_objlib_bits_new(i64 elemtype,i64 length,i64 lower);
struct pc_decls_objrec * pc_objlib_struct_new(i64 t);
struct pc_decls_objrec * pc_objlib_dict_new(i64 n);
struct pc_decls_objrec * pc_objlib_record_new(i64 rectype);
void pc_objlib_list_free(struct pc_decls_objrec * p);
void pc_objlib_record_free(struct pc_decls_objrec * p);
void pc_objlib_array_free(struct pc_decls_objrec * p);
void pc_objlib_bits_free(struct pc_decls_objrec * p);
void pc_objlib_dict_free(struct pc_decls_objrec * p);
i64 pc_objlib_bits_bytesize(struct pc_decls_objrec * p);
void pc_objlib_list_resize(struct pc_decls_objrec * p,i64 n);
void pc_objlib_array_resize(struct pc_decls_objrec * p,i64 n);
void pc_objlib_bits_resize(struct pc_decls_objrec * p,i64 n);
void pc_objlib_string_resize(struct pc_decls_objrec * p,i64 n);
struct pc_decls_objrec * pc_objlib_copyonwrite(struct pc_decls_objrec * p,i64 tag);
struct pc_decls_objrec * pc_objlib_make_strslicexobj(byte * s,i64 length);
struct pc_decls_objrec * pc_objlib_bignum_make(void * bn);
void pc_bignum_bx_makestr(byte * s,i64 length,struct pc_decls_varrec * p);
byte * pc_bignum_bx_tostring(struct pc_decls_varrec * a,i64 fmt);
void pc_bignum_bx_dupl(struct pc_decls_varrec * p);
void pc_bignum_bx_negto(struct pc_decls_varrec * p);
void pc_bignum_bx_absto(struct pc_decls_varrec * p);
static struct mbignum_bignumrec * pc_bignum_makebnvar(struct pc_decls_varrec * dest,struct mbignum_bignumrec * bn);
void pc_bignum_bx_free(struct pc_decls_varrec * a);
void pc_bignum_bx_makeint(i64 aa,struct pc_decls_varrec * dest);
void pc_bignum_bx_add(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c);
void pc_bignum_bx_sub(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c);
void pc_bignum_bx_mul(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c);
void pc_bignum_bx_div(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c);
void pc_bignum_bx_idiv(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c);
void pc_bignum_bx_irem(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c);
i64 pc_bignum_bx_equal(struct pc_decls_varrec * a,struct pc_decls_varrec * b);
i64 pc_bignum_bx_cmp(struct pc_decls_varrec * a,struct pc_decls_varrec * b);
i64 pc_bignum_bx_int(struct pc_decls_varrec * p);
void pc_bignum_bx_power(struct pc_decls_varrec * a,i64 n,struct pc_decls_varrec * dest);
void pc_bignum_bx_reduce(struct pc_decls_varrec * bn);
i64 pc_bignum_bx_length(struct pc_decls_varrec * bn);
struct mbignum_bignumrec * mbignum_bn_init(void);
static i64 mbignum_readexpon(byte * s);
void mbignum_bn_print(struct mbignum_bignumrec * a,i64 format);
void mbignum_bn_println(struct mbignum_bignumrec * a,i64 format);
static i64 mbignum_getbintype(struct mbignum_bignumrec * a,struct mbignum_bignumrec * b);
static struct mbignum_bignumrec * mbignum_makebignum(i64 length);
static i32 * mbignum_makesmallnum(i64 length);
static struct mbignum_bignumrec * mbignum_smalltobig(struct mbignum_bignumrec * c,i32 * a,i64 length,i64 alloc,i64 offset);
static void mbignum_freesmall(i32 * p,i64 length);
void * mbignum_bn_alloc(i64 size);
void * mbignum_checkedmalloc(i64 size);
void mbignum_bn_free(struct mbignum_bignumrec * a);
static void mbignum_freemem(void * p,i64 size);
void mbignum_bn_setzero(struct mbignum_bignumrec * a);
void mbignum_bn_move(struct mbignum_bignumrec * a,struct mbignum_bignumrec * b);
void mbignum_bn_dupl(struct mbignum_bignumrec * a,struct mbignum_bignumrec * b);
void mbignum_bn_setinf(struct mbignum_bignumrec * dest);
void mbignum_bn_setnan(struct mbignum_bignumrec * dest);
static void mbignum_bn_error(byte * mess);
i64 mbignum_bn_iszero(struct mbignum_bignumrec * a);
void mbignum_bn_negto(struct mbignum_bignumrec * a);
void mbignum_bn_absto(struct mbignum_bignumrec * a);
i64 mbignum_bn_isint(struct mbignum_bignumrec * a);
i64 mbignum_bn_getprec(struct mbignum_bignumrec * a);
void mbignum_bn_setprec(struct mbignum_bignumrec * a,i64 prec);
i64 mbignum_bn_getglobalprec(void);
void mbignum_bn_setglobalprec(i64 prec);
struct mbignum_bignumrec * mbignum_bn_makeint(i64 x);
struct mbignum_bignumrec * mbignum_bn_makefloat(double x);
void mbignum_bn_ipower(struct mbignum_bignumrec * d,struct mbignum_bignumrec * a,i64 n);
static i64 mbignum_smallsubto(i32 * p,i32 * q,i64 plen,i64 qlen);
static i64 mbignum_smallmulto(i32 * p,i32 * q,i64 plen,i64 m);
i64 mbignum_bn_equal(struct mbignum_bignumrec * a,struct mbignum_bignumrec * b);
void mbignum_bn_addu(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b);
static void mbignum_bn_subu(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b);
static void mbignum_bn_mulu(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b);
static i32 mbignum_smalldiv(i32 * x,i32 * b,i64 * xlen,i64 nb);
void mbignum_bn_idivu(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b,struct mbignum_bignumrec * rm);
static i64 mbignum_strvaln(byte * s,i64 n);
struct mbignum_bignumrec * mbignum_bn_makestr(byte * s,i64 length);
static void mbignum_bn_fdivu(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b,i64 precision);
static byte * mbignum_tostring_float(struct mbignum_bignumrec * a,i64 fmt);
byte * mbignum_bn_tostring(struct mbignum_bignumrec * a,i64 fmt);
static byte * mbignum_tostring_scient(struct mbignum_bignumrec * a);
i64 mbignum_bn_add(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b);
i64 mbignum_bn_sub(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b);
i64 mbignum_bn_mul(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b);
i64 mbignum_bn_mulp(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b,i64 prec);
i64 mbignum_bn_div(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b,i64 prec);
i64 mbignum_bn_idiv(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b);
i64 mbignum_bn_idivrem(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * rm,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b);
i64 mbignum_bn_irem(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b);
i64 mbignum_bn_cmp(struct mbignum_bignumrec * a,struct mbignum_bignumrec * b);
struct mbignum_bignumrec * mbignum_bn_const(i64 value);
i64 mbignum_bn_sign(struct mbignum_bignumrec * a);
static struct mbignum_bignumrec * mbignum_badnumber(void);
i64 mbignum_bn_digits(struct mbignum_bignumrec * a);
i64 mbignum_bn_toint(struct mbignum_bignumrec * a);
double mbignum_bn_tofloat(struct mbignum_bignumrec * a);
void mbignum_bn_fix(struct mbignum_bignumrec * c,struct mbignum_bignumrec * a);
void mbignum_bntest(struct mbignum_bignumrec * a);
void pc_print_pch_print(struct pc_decls_varrec * p,struct pc_decls_varrec * fmt);
void pc_print_pch_println(void);
void pc_print_pch_startprintcon(void);
void pc_print_pch_startprint(struct pc_decls_varrec * p);
void pc_print_pch_endprint(void);
void pc_print_pch_strstartprint(void);
void pc_print_pch_strendprint(struct pc_decls_varrec * dest);
void pc_print_pch_setformat(struct pc_decls_varrec * p);
void pc_print_pch_setformat2(struct pc_decls_varrec * p);
void pc_print_pch_dprint(struct pc_decls_varrec * p,struct pc_decls_varrec * fmt);
void pc_print_pch_printnogap(void);
static void pc_print_initfmtcode(struct pc_decls_fmtrec * f);
static i64 pc_print_i64mintostr(byte * s,i64 base,i64 sep);
static i64 pc_print_u64tostr(u64 aa,byte * s,u64 base,i64 sep);
static i64 pc_print_i64tostrfmt(i64 aa,byte * s,struct pc_decls_fmtrec * fmt,i64 usigned);
static i64 pc_print_u64tostrfmt(i64 aa,byte * s,struct pc_decls_fmtrec * fmt);
static i64 pc_print_strtostrfmt(byte * s,byte * t,i64 n,struct pc_decls_fmtrec * fmt);
static i64 pc_print_expandstr(byte * s,byte * t,i64 n,struct pc_decls_fmtrec * fmt);
void pc_print_pc_strtofmt(byte * s,i64 slen,struct pc_decls_fmtrec * fmt);
static void pc_print_printstrz(byte * s);
static void pc_print_printstr_n(byte * s,i64 n);
void pc_print_printerror(byte * s);
void pc_print_addstring(struct pc_decls_objrec * p,byte * t,i64 n);
void pc_print_j_tostr_i(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest);
void pc_print_j_tostr_r(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest);
void pc_print_j_tostr_w(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest);
void pc_print_j_tostr_n(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest);
void pc_print_j_tostr_s(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest);
void pc_print_j_tostr_l_m(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest);
void pc_print_j_tostr_a(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest);
void pc_print_j_tostr_b(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest);
void pc_print_j_tostr_e(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest);
void pc_print_j_tostr_k(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest);
void pc_print_j_tostr_j(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest);
void pc_print_j_tostr_d(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest);
void pc_print_j_tostr_z(struct pc_decls_varrec * a,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest);
static void pc_print_printnextfmtchars(i64 lastx);
static i64 pc_print_getreadfmtcode(struct pc_decls_varrec * p);
void pc_print_pch_sreadln(struct pc_decls_varrec * dev,struct pc_decls_varrec * dest);
void pc_print_pch_strtoval(struct pc_decls_varrec * p,struct pc_decls_varrec * fmt,struct pc_decls_varrec * dest);
void pc_print_pch_reread(void);
void pc_print_pch_rereadln(void);
static byte * pc_print_readname(byte * s,i64 length,struct pc_decls_varrec * dest);
static byte * pc_print_readstring(byte * s,i64 length,struct pc_decls_varrec * dest);
static byte * pc_print_readint(byte * sold,i64 length,struct pc_decls_varrec * dest);
static byte * pc_print_readhex(byte * sold,i64 length,struct pc_decls_varrec * dest);
static byte * pc_print_readbin(byte * sold,i64 length,struct pc_decls_varrec * dest);
static byte * pc_print_readreal(byte * sold,i64 length,struct pc_decls_varrec * dest);
void pc_print_pch_readln(struct pc_decls_varrec * dev);
static void pc_print_stepkbpos(byte * s);
void pc_print_pch_sread(struct pc_decls_varrec * fmt,struct pc_decls_varrec * dest);
static void pc_print_domultichar(byte * p,i64 n,byte * dest,struct pc_decls_fmtrec * fmt);
void pc_print_pch_tostr(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result);
struct pc_decls_fmtrec * pc_print_pc_getfmt(struct pc_decls_varrec * p,struct pc_decls_fmtrec * fmt);
static void pc_print_pc_readlinen(void * handlex,byte * buffer,i64 size);
static byte * pc_print_readitem(byte * s,i64 length,byte * * itemstr,i64 * itemlength);
static byte * pc_print_readany(byte * sold,i64 length,struct pc_decls_varrec * dest);
static void pc_print_strtoreal(byte * s,i64 length,struct pc_decls_varrec * dest);
static void pc_print_strtoint(byte * s,i64 length,struct pc_decls_varrec * dest);
static void pc_print_calltostrtable(struct pc_decls_varrec * q,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest);
static byte * pc_print_printbn(struct pc_decls_varrec * a0,struct pc_decls_fmtrec * fmt,i64 * length);
void pc_jhandlers_initcalltables(void);
static void pc_jhandlers_initjhandler(byte * p,void * fnaddr);
static void pc_jhandlers_add_stable_entry(void * (*table)[],i64 t,void * fnaddr);
static void pc_jhandlers_add_dtable_entry(u64 * (*(*table)[])(void),i64 s,i64 t,void * fnaddr);
static void pc_jhandlers_def_handler(void);
static void pc_jhandlers_ddef_handler(void);
u64 * pc_jhandlers_j_add_i_w(void);
u64 * pc_jhandlers_j_add_r(void);
u64 * pc_jhandlers_j_add_s(void);
u64 * pc_jhandlers_j_add_j(void);
u64 * pc_jhandlers_j_add_e(void);
u64 * pc_jhandlers_j_add_z(void);
u64 * pc_jhandlers_j_add_iw_wi(void);
u64 * pc_jhandlers_j_add_zz(void);
u64 * pc_jhandlers_j_sub_i_w(void);
u64 * pc_jhandlers_j_sub_r(void);
u64 * pc_jhandlers_j_sub_p(void);
u64 * pc_jhandlers_j_sub_j(void);
u64 * pc_jhandlers_j_sub_e(void);
u64 * pc_jhandlers_j_sub_z(void);
u64 * pc_jhandlers_j_sub_zz(void);
static u64 * pc_jhandlers_j_mixed_iw_wi(void);
static u64 * pc_jhandlers_j_mixed_ir(void);
static u64 * pc_jhandlers_j_mixed_ri(void);
static u64 * pc_jhandlers_j_mixed_ij(void);
static u64 * pc_jhandlers_j_mixed_ji(void);
u64 * pc_jhandlers_j_mul_i_w(void);
u64 * pc_jhandlers_j_mul_r(void);
u64 * pc_jhandlers_j_mul_j(void);
u64 * pc_jhandlers_j_mul_z(void);
u64 * pc_jhandlers_j_mul_li(void);
u64 * pc_jhandlers_j_mul_si(void);
u64 * pc_jhandlers_j_mul_e(void);
u64 * pc_jhandlers_j_mul_zz(void);
u64 * pc_jhandlers_j_div_i(void);
u64 * pc_jhandlers_j_div_r(void);
u64 * pc_jhandlers_j_div_j(void);
u64 * pc_jhandlers_j_div_z(void);
u64 * pc_jhandlers_j_jumple_i(void);
u64 * pc_jhandlers_j_jumple_r(void);
u64 * pc_jhandlers_j_jumple_s(void);
u64 * pc_jhandlers_j_jumple_z(void);
u64 * pc_jhandlers_j_jumpeq_i_r_t_o(void);
u64 * pc_jhandlers_j_jumpeq_v_p_f_g(void);
u64 * pc_jhandlers_j_jumpeq_s(void);
u64 * pc_jhandlers_j_jumpeq_z(void);
u64 * pc_jhandlers_j_jumpeq_zz(void);
u64 * pc_jhandlers_j_jumpne_i_r_t_o(void);
u64 * pc_jhandlers_j_jumpne_v_p_f_g(void);
u64 * pc_jhandlers_j_jumpne_s(void);
u64 * pc_jhandlers_j_jumpne_z(void);
u64 * pc_jhandlers_j_jumpne_zz(void);
u64 * pc_jhandlers_j_jumpge_i(void);
u64 * pc_jhandlers_j_jumpge_w(void);
u64 * pc_jhandlers_j_jumpge_r(void);
u64 * pc_jhandlers_j_jumpge_z(void);
u64 * pc_jhandlers_j_jumpgt_i(void);
u64 * pc_jhandlers_j_jumpgt_w(void);
u64 * pc_jhandlers_j_jumpgt_r(void);
u64 * pc_jhandlers_j_jumpgt_z(void);
u64 * pc_jhandlers_j_jumplt_i(void);
u64 * pc_jhandlers_j_jumplt_v_p(void);
u64 * pc_jhandlers_j_jumplt_r(void);
u64 * pc_jhandlers_j_jumplt_z(void);
u64 * pc_jhandlers_j_pushix_li(void);
u64 * pc_jhandlers_j_pushix_mi(void);
u64 * pc_jhandlers_j_pushix_vi(void);
u64 * pc_jhandlers_j_pushix_ln(void);
u64 * pc_jhandlers_j_pushix_ll(void);
u64 * pc_jhandlers_j_pushix_le(void);
u64 * pc_jhandlers_j_pushix_ai(void);
u64 * pc_jhandlers_j_pushix_bi_ei(void);
u64 * pc_jhandlers_j_pushix_an(void);
u64 * pc_jhandlers_j_pushix_si(void);
u64 * pc_jhandlers_j_pushix_sn(void);
u64 * pc_jhandlers_j_pushix_bn(void);
u64 * pc_jhandlers_j_pushix_di(void);
u64 * pc_jhandlers_j_pushix_zz(void);
u64 * pc_jhandlers_j_pushixref_li(void);
u64 * pc_jhandlers_j_pushixref_ln(void);
u64 * pc_jhandlers_j_pushixref_ai(void);
u64 * pc_jhandlers_j_pushixref_si(void);
u64 * pc_jhandlers_j_pushixref_bi(void);
u64 * pc_jhandlers_j_pushixref_zz(void);
u64 * pc_jhandlers_j_pushdotix_si(void);
u64 * pc_jhandlers_j_pushdotix_sn(void);
u64 * pc_jhandlers_j_pushdotix_ii(void);
u64 * pc_jhandlers_j_pushdotix_in(void);
u64 * pc_jhandlers_j_pushdotix_ei(void);
u64 * pc_jhandlers_j_pushdotix_mi(void);
u64 * pc_jhandlers_j_pushdotix_zz(void);
u64 * pc_jhandlers_j_pushdotixref_si(void);
u64 * pc_jhandlers_j_pushdotixref_sn(void);
u64 * pc_jhandlers_j_pushdotixref_ii(void);
u64 * pc_jhandlers_j_pushdotixref_in_wn(void);
u64 * pc_jhandlers_j_pushdotixref_ei(void);
u64 * pc_jhandlers_j_pushdotixref_zz(void);
u64 * pc_jhandlers_j_addto_i(void);
u64 * pc_jhandlers_j_addto_r(void);
u64 * pc_jhandlers_j_addto_s(void);
u64 * pc_jhandlers_j_addto_p(void);
u64 * pc_jhandlers_j_addto_z(void);
u64 * pc_jhandlers_j_addto_si(void);
u64 * pc_jhandlers_j_addto_ir(void);
u64 * pc_jhandlers_j_addto_ri(void);
u64 * pc_jhandlers_j_addto_zz(void);
u64 * pc_jhandlers_j_subto_i(void);
u64 * pc_jhandlers_j_subto_r(void);
u64 * pc_jhandlers_j_subto_z(void);
u64 * pc_jhandlers_j_subto_ir(void);
u64 * pc_jhandlers_j_subto_ri(void);
u64 * pc_jhandlers_j_subto_zz(void);
u64 * pc_jhandlers_j_multo_i_w(void);
u64 * pc_jhandlers_j_multo_r(void);
u64 * pc_jhandlers_j_multo_z(void);
u64 * pc_jhandlers_j_divto_i_w(void);
u64 * pc_jhandlers_j_divto_r(void);
u64 * pc_jhandlers_j_divto_z(void);
u64 * pc_jhandlers_j_idivto_i(void);
u64 * pc_jhandlers_j_idivto_z(void);
u64 * pc_jhandlers_j_iand_i_w(void);
u64 * pc_jhandlers_j_iand_e(void);
u64 * pc_jhandlers_j_iand_z(void);
u64 * pc_jhandlers_j_ior_i_w(void);
u64 * pc_jhandlers_j_ior_e(void);
u64 * pc_jhandlers_j_ior_z(void);
u64 * pc_jhandlers_j_ixor_i_w(void);
u64 * pc_jhandlers_j_ixor_e(void);
u64 * pc_jhandlers_j_ixor_z(void);
u64 * pc_jhandlers_j_iandto_i_w(void);
u64 * pc_jhandlers_j_iandto_z(void);
u64 * pc_jhandlers_j_iorto_i_w(void);
u64 * pc_jhandlers_j_iorto_z(void);
u64 * pc_jhandlers_j_ixorto_i_w(void);
u64 * pc_jhandlers_j_ixorto_z(void);
u64 * pc_jhandlers_j_shlto_i(void);
u64 * pc_jhandlers_j_shlto_z(void);
u64 * pc_jhandlers_j_shrto_i(void);
u64 * pc_jhandlers_j_shrto_z(void);
u64 * pc_jhandlers_j_concat_s(void);
u64 * pc_jhandlers_j_concat_l(void);
u64 * pc_jhandlers_j_concat_z(void);
u64 * pc_jhandlers_j_concatto_s(void);
u64 * pc_jhandlers_j_concatto_l(void);
u64 * pc_jhandlers_j_concatto_z(void);
u64 * pc_jhandlers_j_append_s(void);
u64 * pc_jhandlers_j_append_l(void);
u64 * pc_jhandlers_j_append_a(void);
u64 * pc_jhandlers_j_append_b(void);
u64 * pc_jhandlers_j_append_z(void);
u64 * pc_jhandlers_j_appendto_s(void);
u64 * pc_jhandlers_j_appendto_l(void);
u64 * pc_jhandlers_j_appendto_a(void);
u64 * pc_jhandlers_j_appendto_b_e(void);
u64 * pc_jhandlers_j_appendto_z(void);
u64 * pc_jhandlers_j_max_i(void);
u64 * pc_jhandlers_j_max_r(void);
u64 * pc_jhandlers_j_max_z(void);
u64 * pc_jhandlers_j_min_z(void);
u64 * pc_jhandlers_j_len_l_a_e_s_b_d(void);
u64 * pc_jhandlers_j_len_m_k(void);
u64 * pc_jhandlers_j_len_n(void);
u64 * pc_jhandlers_j_len_z(void);
u64 * pc_jhandlers_j_lwb_l(void);
u64 * pc_jhandlers_j_lwb_a_b(void);
u64 * pc_jhandlers_j_lwb_s_m_k_d(void);
u64 * pc_jhandlers_j_lwb_e(void);
u64 * pc_jhandlers_j_lwb_n(void);
u64 * pc_jhandlers_j_lwb_z(void);
u64 * pc_jhandlers_j_upb_l(void);
u64 * pc_jhandlers_j_upb_a_b(void);
u64 * pc_jhandlers_j_upb_s_d(void);
u64 * pc_jhandlers_j_upb_m_k(void);
u64 * pc_jhandlers_j_upb_e(void);
u64 * pc_jhandlers_j_upb_n(void);
u64 * pc_jhandlers_j_upb_z(void);
u64 * pc_jhandlers_j_bounds_l_a_b_s_e(void);
u64 * pc_jhandlers_j_bounds_m_k(void);
u64 * pc_jhandlers_j_bounds_n(void);
u64 * pc_jhandlers_j_bounds_z(void);
u64 * pc_jhandlers_j_minto_i(void);
u64 * pc_jhandlers_j_minto_r(void);
u64 * pc_jhandlers_j_minto_z(void);
u64 * pc_jhandlers_j_maxto_i(void);
u64 * pc_jhandlers_j_maxto_r(void);
u64 * pc_jhandlers_j_maxto_z(void);
u64 * pc_jhandlers_j_neg_i_w(void);
u64 * pc_jhandlers_j_neg_r(void);
u64 * pc_jhandlers_j_neg_j(void);
u64 * pc_jhandlers_j_neg_e(void);
u64 * pc_jhandlers_j_neg_z(void);
u64 * pc_jhandlers_j_abs_i_w(void);
u64 * pc_jhandlers_j_abs_r(void);
u64 * pc_jhandlers_j_abs_j(void);
u64 * pc_jhandlers_j_abs_z(void);
u64 * pc_jhandlers_j_inot_i_w(void);
u64 * pc_jhandlers_j_inot_e(void);
u64 * pc_jhandlers_j_inot_z(void);
u64 * pc_jhandlers_j_istrue_i_w_r(void);
u64 * pc_jhandlers_j_istrue_l_a_e_s_b(void);
u64 * pc_jhandlers_j_istrue_k_m_h(void);
u64 * pc_jhandlers_j_istrue_j(void);
u64 * pc_jhandlers_j_istrue_z(void);
u64 * pc_jhandlers_j_jumpfalse_i_w_r_v_p_f(void);
u64 * pc_jhandlers_j_jumpfalse_s_l_e_a_b(void);
u64 * pc_jhandlers_j_jumpfalse_z(void);
u64 * pc_jhandlers_j_jumptrue_i_r_w_v_p_f(void);
u64 * pc_jhandlers_j_jumptrue_s_l_e_a_b(void);
u64 * pc_jhandlers_j_jumptrue_z(void);
u64 * pc_jhandlers_j_shl_i_w(void);
u64 * pc_jhandlers_j_shl_z(void);
u64 * pc_jhandlers_j_shr_i_w(void);
u64 * pc_jhandlers_j_shr_j_i(void);
u64 * pc_jhandlers_j_shr_z(void);
u64 * pc_jhandlers_j_shr_wi(void);
u64 * pc_jhandlers_j_shr_zz(void);
u64 * pc_jhandlers_j_idiv_w(void);
u64 * pc_jhandlers_j_idiv_j(void);
u64 * pc_jhandlers_j_idiv_z(void);
u64 * pc_jhandlers_j_rem_w(void);
u64 * pc_jhandlers_j_rem_j(void);
u64 * pc_jhandlers_j_remz(void);
i64 pc_oslayer_runproc_m(void * amsg);
void pc_oslayer_os_getconsize(struct pc_decls_varrec * result);
void pc_oslayer_pch_setmesshandler(struct pc_decls_varrec * fn);
void pc_oslayer_pch_gethostname(struct pc_decls_varrec * result);
void pc_oslayer_os_initdllmodules(void);
i64 pc_oslayer_os_loaddllmodule(byte * dllname);
void pc_oslayer_os_initdllfunctions(void);
void * pc_oslayer_os_loaddllfunction(i64 fnindex);
void pc_oslayer_pch_getos(struct pc_decls_varrec * result);
void pc_oslayer_pch_gethostsize(struct pc_decls_varrec * result);
void pc_oslayer_pch_iswindows(struct pc_decls_varrec * result);
void pc_oslayer_os_calldll(i64 calltype,i64 fnindex,i64 offset,i64 nparams,i64 restype,struct pc_decls_varrec * dest);
u64 oswindllc_os_calldllfunction(void (*fnaddr)(void),i64 retcode,i64 nargs,i64 (*args)[],byte (*argcodes)[]);
u64 oswindllc_os_pushargs(u64 (*args)[],i64 nargs,i64 nextra,void (*fnaddr)(void),i64 isfloat);
static i64 oswindllc_calldll_cint(void (*fnaddr)(void),i64 (*params)[],i64 nparams);
static i64 oswindllc_calldll_creal(void (*fnaddr)(void),i64 (*params)[],i64 nparams);
void oswindllc_os_dummycall(double a,double b,double c,double d);
extern byte * imgload_bgr(byte * filename,i64 * x,i64 * y,i64 * channels,i64 needchannels);
void pc_host_callhostfunction(i64 hostfn,i64 calledasfn);
static void pc_host_pch_leftstr(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c,struct pc_decls_varrec * result);
static void pc_host_pch_rightstr(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c,struct pc_decls_varrec * result);
static void pc_host_pch_convlc(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result);
static void pc_host_pch_convuc(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result);
static void pc_host_pch_iconvlc(struct pc_decls_varrec * a,struct pc_decls_varrec * b);
static void pc_host_pch_iconvuc(struct pc_decls_varrec * a,struct pc_decls_varrec * b);
static void pc_host_pch_stop(void);
static void pc_host_pch_stopx(struct pc_decls_varrec * a);
static void pc_host_pch_ismain(struct pc_decls_varrec * a,struct pc_decls_varrec * result);
static void pc_host_pch_waitkey(struct pc_decls_varrec * result);
static void pc_host_pch_testkey(struct pc_decls_varrec * result);
static void pc_host_pch_execwait(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c,struct pc_decls_varrec * result);
static void pc_host_pch_execcmd(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c,struct pc_decls_varrec * result);
static void pc_host_pch_makestr(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result);
static void pc_host_pch_makestrslice(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result);
static void pc_host_pch_makeref(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result);
static void pc_host_pch_new(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c,struct pc_decls_varrec * d,struct pc_decls_varrec * result);
static void pc_host_pch_newheap(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c,struct pc_decls_varrec * d,struct pc_decls_varrec * result);
static void pc_host_pch_heapvar(struct pc_decls_varrec * a,struct pc_decls_varrec * result);
static void pc_host_pch_freeheap(struct pc_decls_varrec * a);
static void pc_host_pch_getcmdparam(struct pc_decls_varrec * a,struct pc_decls_varrec * result);
static void pc_host_pch_setpcerror(struct pc_decls_varrec * a);
static void pc_host_pch_setdebug(struct pc_decls_varrec * a);
static void pc_host_pch_setfprintf(struct pc_decls_varrec * a,struct pc_decls_varrec * b);
static void pc_host_pch_ticks(struct pc_decls_varrec * result);
static void pc_host_pch_sleep(struct pc_decls_varrec * a);
static void pc_host_pch_random(struct pc_decls_varrec * a,struct pc_decls_varrec * result);
static void pc_host_pch_findmetafunction(struct pc_decls_varrec * a,struct pc_decls_varrec * result);
static void pc_host_pch_loadpcl(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result);
static void pc_host_pch_runpcl(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result);
static void pc_host_pch_runtask(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result);
static void pc_host_pch_callext(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c);
static void pc_host_pch_system(struct pc_decls_varrec * a,struct pc_decls_varrec * result);
static void pc_host_pch_shellexec(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result);
static void pc_host_pch_gethash(struct pc_decls_varrec * a,struct pc_decls_varrec * result);
static void pc_host_pch_test(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result);
static void pc_host_pch_pcldata(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result);
static void pc_host_pch_getcstring(struct pc_decls_varrec * a,struct pc_decls_varrec * result);
static void pc_host_pch_getparam(struct pc_decls_varrec * a,struct pc_decls_varrec * result);
static void pc_host_pch_clearlist(struct pc_decls_varrec * a);
static void pc_host_pch_makelink(struct pc_decls_varrec * a,struct pc_decls_varrec * result);
static void pc_host_pch_allparams(struct pc_decls_varrec * a,struct pc_decls_varrec * result);
static void pc_host_pch_stackvars(struct pc_decls_varrec * result);
static void pc_host_pch_makeempty(struct pc_decls_varrec * a,struct pc_decls_varrec * result);
static void pc_host_pch_readlines(struct pc_decls_varrec * a,struct pc_decls_varrec * result);
static void pc_host_pch_dictitems(struct pc_decls_varrec * a,struct pc_decls_varrec * result);
static void pc_host_pch_setoverload(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c);
static void pc_host_pch_errorinfo(struct pc_decls_varrec * a,struct pc_decls_varrec * result);
static void pc_host_getbounds(struct pc_decls_varrec * p,struct pc_host_dimrec * dims,i64 lower);
static i64 pc_host_checkparam(struct pc_decls_varrec * p,i64 tag,i64 defaultx);
static void pc_host_leftstring(struct pc_decls_varrec * a,i64 n,struct pc_decls_varrec * result);
static void pc_host_rightstring(struct pc_decls_varrec * a,i64 n,struct pc_decls_varrec * result);
static void pc_host_padstring_right(struct pc_decls_varrec * a,i64 n,i64 fillchar,struct pc_decls_varrec * result);
static void pc_host_padstring_left(struct pc_decls_varrec * a,i64 n,i64 fillchar,struct pc_decls_varrec * result);
static void pc_host_pcld_makevint(struct pc_decls_varrec * p,i64 a);
static void pc_host_pcld_makelist(struct pc_decls_varrec * p,struct pc_decls_varrec * result,i64 n);
static void pc_host_getproctabledata(struct pc_decls_procrec * p,struct pc_decls_varrec * result);
static u64 * pc_host_convert_handler(i64 _1);
static void pc_host_addtoproclist(struct pc_decls_strec * d);
static u64 * pc_host_tostr_handler(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest);
static u64 * pc_host_add_handler(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest);
static void pc_host_addovrecord(struct pc_host_overloadrec * * p,i64 t,u64 * fnptr);
static void (*pc_host_findapplproc(i64 fnindex))(void);
void pc_host_do_callapplproc(i64 fnindex,i64 nargs,struct pc_decls_varrec * result);
static i64 pc_host_vartopack(struct pc_decls_varrec * p,i64 tp);
i64 pc_host_dummyfn(i64 a,i64 b,i64 c);
struct pc_decls_varrec * pc_host_new_random(struct pc_decls_varrec * a);
struct pc_decls_varrec * pc_host_new_heapvar(struct pc_decls_varrec * a);
i64 pc_host_mfib(i64 n);
void pc_dxfns_dx_iorset(struct pc_decls_varrec * x,struct pc_decls_varrec * y);
void pc_dxfns_dx_iandset(struct pc_decls_varrec * x,struct pc_decls_varrec * y);
void pc_dxfns_dx_ixorset(struct pc_decls_varrec * x,struct pc_decls_varrec * y);
void pc_dxfns_dx_inotset(struct pc_decls_varrec * x);
void pc_dxfns_dx_subset(struct pc_decls_varrec * x,struct pc_decls_varrec * y);
void pc_dxfns_inotsetbits(i64 * p,i64 n);
void pc_dxfns_iorsetbits(i64 * p,i64 * q,i64 n);
void pc_dxfns_iandsetbits(i64 * p,i64 * q,i64 n);
void pc_dxfns_ixorsetbits(i64 * p,i64 * q,i64 n);
void pc_dxfns_subsetbits(i64 * p,i64 * q,i64 n);
void pc_dxfns_iresizeset(struct pc_decls_varrec * p,i64 n);
i64 pc_dxfns_dx_varinvar(struct pc_decls_varrec * x,struct pc_decls_varrec * y);
i64 pc_dxfns_dx_mixed(struct pc_decls_varrec * x,struct pc_decls_varrec * y);
void * pc_khandlers_k_zero(void);
void * pc_khandlers_k_nop(void);
void * pc_khandlers_k_procstart(void);
void * pc_khandlers_k_procend(void);
void * pc_khandlers_k_endmodule(void);
void * pc_khandlers_k_push_m(void);
void * pc_khandlers_k_push_f(void);
void * pc_khandlers_k_push_am(void);
void * pc_khandlers_k_push_af(void);
void * pc_khandlers_k_push_ap(void);
void * pc_khandlers_k_push_al(void);
void * pc_khandlers_k_push_ci(void);
void * pc_khandlers_k_push_cw(void);
void * pc_khandlers_k_push_cr(void);
void * pc_khandlers_k_push_cn(void);
void * pc_khandlers_k_push_cs(void);
void * pc_khandlers_k_push_t(void);
void * pc_khandlers_k_push_op(void);
void * pc_khandlers_k_pushz(void);
void * pc_khandlers_k_pushz_void(void);
void * pc_khandlers_k_pushz_str(void);
void * pc_khandlers_k_pushz_list(void);
void * pc_khandlers_k_pushz_listl(void);
void * pc_khandlers_k_pushz_set(void);
void * pc_khandlers_k_pushz_arrayl(void);
void * pc_khandlers_k_pop_m(void);
void * pc_khandlers_k_pop_f(void);
void * pc_khandlers_k_store_m(void);
void * pc_khandlers_k_store_f(void);
void * pc_khandlers_k_pushptr(void);
void * pc_khandlers_k_popptr(void);
void * pc_khandlers_k_storeptr(void);
void * pc_khandlers_k_zpop_m(void);
void * pc_khandlers_k_zpop_f(void);
void * pc_khandlers_k_zstore_m(void);
void * pc_khandlers_k_zstore_f(void);
void * pc_khandlers_k_copy(void);
void * pc_khandlers_k_swap(void);
void * pc_khandlers_k_convptr(void);
void * pc_khandlers_k_jump(void);
void * pc_khandlers_k_jumpptr(void);
void * pc_khandlers_k_jumptrue(void);
void * pc_khandlers_k_jumpfalse(void);
void * pc_khandlers_k_jumpdef(void);
void * pc_khandlers_k_jumpvoid(void);
void * pc_khandlers_k_jumpeq(void);
void * pc_khandlers_k_jumpne(void);
void * pc_khandlers_k_jumplt(void);
void * pc_khandlers_k_jumple(void);
void * pc_khandlers_k_jumpge(void);
void * pc_khandlers_k_jumpgt(void);
void * pc_khandlers_k_jumptesteq(void);
void * pc_khandlers_k_jumptestne(void);
void * pc_khandlers_k_jumplabel(void);
void * pc_khandlers_k_jumpclabel(void);
void * pc_khandlers_k_switch(void);
void * pc_khandlers_k_cswitch(void);
void * pc_khandlers_k_new(void);
void * pc_khandlers_k_to_f(void);
void * pc_khandlers_k_for_fci(void);
void * pc_khandlers_k_for_ff(void);
void * pc_khandlers_k_ford_fci(void);
void * pc_khandlers_k_ford_ff(void);
void * pc_khandlers_k_call(void);
void * pc_khandlers_k_callptr(void);
void * pc_khandlers_k_return(void);
void * pc_khandlers_k_startdll(void);
void * pc_khandlers_k_pushdll(void);
void * pc_khandlers_k_calldll(void);
void * pc_khandlers_k_callhost(void);
void * pc_khandlers_k_stackframe(void);
void * pc_khandlers_k_free(void);
void * pc_khandlers_k_addsp(void);
void * pc_khandlers_k_stop(void);
void * pc_khandlers_k_test(void);
void * pc_khandlers_k_makelist(void);
void * pc_khandlers_k_makerecord(void);
void * pc_khandlers_k_makearray(void);
void * pc_khandlers_k_makestruct(void);
void * pc_khandlers_k_makeset(void);
void * pc_khandlers_k_makerange(void);
void * pc_khandlers_k_makedict(void);
void * pc_khandlers_k_pushdot(void);
void * pc_khandlers_k_pushdotref(void);
void * pc_khandlers_k_softconv(void);
void * pc_khandlers_k_hardconv(void);
void * pc_khandlers_k_mixed(void);
void * pc_khandlers_k_incrptr(void);
void * pc_khandlers_k_incrto_m(void);
void * pc_khandlers_k_incrto_f(void);
void * pc_khandlers_k_loadincr(void);
void * pc_khandlers_k_incrload(void);
void * pc_khandlers_k_decrptr(void);
void * pc_khandlers_k_decrto_m(void);
void * pc_khandlers_k_decrto_f(void);
void * pc_khandlers_k_loaddecr(void);
void * pc_khandlers_k_decrload(void);
void * pc_khandlers_k_incr(void);
void * pc_khandlers_k_decr(void);
void * pc_khandlers_k_neg(void);
void * pc_khandlers_k_abs(void);
void * pc_khandlers_k_not(void);
void * pc_khandlers_k_inot(void);
void * pc_khandlers_k_istrue(void);
void * pc_khandlers_k_asc(void);
void * pc_khandlers_k_chr(void);
void * pc_khandlers_k_sqrt(void);
void * pc_khandlers_k_sqr(void);
void * pc_khandlers_k_cube(void);
void * pc_khandlers_k_sin(void);
void * pc_khandlers_k_cos(void);
void * pc_khandlers_k_tan(void);
void * pc_khandlers_k_asin(void);
void * pc_khandlers_k_acos(void);
void * pc_khandlers_k_atan(void);
void * pc_khandlers_k_sign(void);
void * pc_khandlers_k_ln(void);
void * pc_khandlers_k_log(void);
void * pc_khandlers_k_lg(void);
void * pc_khandlers_k_exp(void);
void * pc_khandlers_k_round(void);
void * pc_khandlers_k_floor(void);
void * pc_khandlers_k_ceil(void);
void * pc_khandlers_k_fract(void);
void * pc_khandlers_k_negto(void);
void * pc_khandlers_k_absto(void);
void * pc_khandlers_k_notto(void);
void * pc_khandlers_k_inotto(void);
void * pc_khandlers_k_len(void);
void * pc_khandlers_k_lwb(void);
void * pc_khandlers_k_upb(void);
void * pc_khandlers_k_bounds(void);
void * pc_khandlers_k_bits(void);
void * pc_khandlers_k_bytes(void);
void * pc_khandlers_k_type(void);
void * pc_khandlers_k_elemtype(void);
void * pc_khandlers_k_basetype(void);
void * pc_khandlers_k_minval(void);
void * pc_khandlers_k_maxval(void);
void * pc_khandlers_k_isint(void);
void * pc_khandlers_k_isreal(void);
void * pc_khandlers_k_isstring(void);
void * pc_khandlers_k_isrange(void);
void * pc_khandlers_k_isnumber(void);
void * pc_khandlers_k_isarray(void);
void * pc_khandlers_k_isrecord(void);
void * pc_khandlers_k_ispointer(void);
void * pc_khandlers_k_ismutable(void);
void * pc_khandlers_k_isset(void);
void * pc_khandlers_k_isvoid(void);
void * pc_khandlers_k_isdef(void);
void * pc_khandlers_k_tostr(void);
void * pc_khandlers_k_isequal(void);
void * pc_khandlers_k_add(void);
void * pc_khandlers_k_sub(void);
void * pc_khandlers_k_mul(void);
void * pc_khandlers_k_div(void);
void * pc_khandlers_k_idiv(void);
void * pc_khandlers_k_rem(void);
void * pc_khandlers_k_divrem(void);
void * pc_khandlers_k_iand(void);
void * pc_khandlers_k_ior(void);
void * pc_khandlers_k_ixor(void);
void * pc_khandlers_k_shl(void);
void * pc_khandlers_k_shr(void);
void * pc_khandlers_k_in(void);
void * pc_khandlers_k_notin(void);
void * pc_khandlers_k_inrev(void);
void * pc_khandlers_k_eq(void);
void * pc_khandlers_k_ne(void);
void * pc_khandlers_k_lt(void);
void * pc_khandlers_k_le(void);
void * pc_khandlers_k_ge(void);
void * pc_khandlers_k_gt(void);
void * pc_khandlers_k_min(void);
void * pc_khandlers_k_max(void);
void * pc_khandlers_k_concat(void);
void * pc_khandlers_k_append(void);
void * pc_khandlers_k_power(void);
void * pc_khandlers_k_atan2(void);
void * pc_khandlers_k_addto(void);
void * pc_khandlers_k_subto(void);
void * pc_khandlers_k_multo(void);
void * pc_khandlers_k_divto(void);
void * pc_khandlers_k_idivto(void);
void * pc_khandlers_k_iandto(void);
void * pc_khandlers_k_iorto(void);
void * pc_khandlers_k_ixorto(void);
void * pc_khandlers_k_shlto(void);
void * pc_khandlers_k_shrto(void);
void * pc_khandlers_k_minto(void);
void * pc_khandlers_k_maxto(void);
void * pc_khandlers_k_concatto(void);
void * pc_khandlers_k_appendto(void);
void * pc_khandlers_k_pushix(void);
void * pc_khandlers_k_pushdotix(void);
void * pc_khandlers_k_pushkeyix(void);
void * pc_khandlers_k_pushkeyixd(void);
void * pc_khandlers_k_pushixref(void);
void * pc_khandlers_k_pushdotixref(void);
void * pc_khandlers_k_pushkeyixref(void);
void * pc_khandlers_k_pushbyteix(void);
void * pc_khandlers_k_pushbyteixref(void);
void * pc_khandlers_k_appendset(void);
void * pc_khandlers_k_pushdotm(void);
void * pc_khandlers_k_pushdott(void);
void * pc_khandlers_k_push_ad(void);
void * pc_khandlers_k_push_try(void);
void * pc_khandlers_k_raise(void);
void * pc_khandlers_k_applyop(void);
void * pc_khandlers_k_makeiter(void);
void * pc_khandlers_k_forall(void);
void * pc_khandlers_k_forallx(void);
void * pc_khandlers_k_foreach(void);
void * pc_khandlers_k_foreachx(void);
void * pc_khandlers_k_expandrange(void);
void * pc_khandlers_k_callappl(void);
void pc_assemc_fixup_asm(i64 mx);
i64 pc_assemc_asmavailable(void);
void pc_assemc_addcountint(void * cmd);
void pc_assemc_addcountext(void);
void pc_assemc_showasmcmd(void * cmd);
i64 * pc_assemc_disploop_asm(void);
u64 pc_assemc_getasmjump(i64 cmd);

/* VARS */
static byte *  pc_optionnames[7] = {(byte*)"fn",(byte*)"asm",(byte*)"debug",(byte*)"fdebug",(byte*)"v",(byte*)"help",(byte*)"ext"};
static byte *  pc_inputfile;
static void *  msysnewc__fnaddresses[]= {
    &start,
    &pc_getinputoptions,
    &pc_do_option,
    &pc_getsyscmdline,
    &pc_showcaption,
    &pc_showhelp,
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
    &pci_runpcl,
    &pci_run_initdata,
    &pci_runpcprogram,
    &pci_initbytecode,
    &pci_disploop,
    &pci_pclinit,
    &pci_fixup_all_pc,
    &pci_fixup_module_pc,
    &pci_disploop_fn,
    &pci_disploop_deb,
    &pci_runproc,
    &pci_allocatestrings,
    &pci_compileq,
    &pci_loadpcfile,
    &pci_initpcldata,
    &pci_createstentry,
    &pci_getprocaddr,
    &pci_pcl_initusertypes,
    &pci_setcmdparam,
    &pc_support_prterror,
    &pc_support_testelem,
    &pc_support_setelem,
    &pc_support_pcustype_def,
    &pc_support_pcustype,
    &pc_support_pcustypet,
    &pc_support_pcmxtypes_def,
    &pc_support_pcmxtypes,
    &pc_support_pcmxtypestt,
    &pc_support_gettypename,
    &pc_support_inittypetables,
    &pc_support_pcerror,
    &pc_support_vxunimpl,
    &pc_support_pclunimpl,
    &pc_support_convcstring,
    &pc_support_getintvalue,
    &pc_support_nextpoweroftwo,
    &pc_support_showlinenumber,
    &pc_support_printlinenumber,
    &pc_support_findlinenumber,
    &pc_support_findpcindex,
    &pc_support_showlinetable,
    &pc_support_writezstring,
    &pc_support_writezint,
    &pc_support_writezint4,
    &pc_support_writezrange,
    &pc_support_writezreal,
    &pc_support_writezeof,
    &pc_support_zerror,
    &pc_support_readzvalue,
    &pc_support_readzint,
    &pc_support_readzdint,
    &pc_support_readzreal,
    &pc_support_readzstring,
    &pc_support_readzblock,
    &pc_support_checkmt,
    &pc_support_ipower,
    &pc_support_loaderror,
    &pc_support_gettypesig,
    &pc_support_getfnname,
    &pc_support_junimpl,
    &pc_misc_raiseexception,
    &pc_misc_raise_error,
    &pc_misc_default_exception,
    &pc_pcfns_pc_unshare,
    &pc_pcfns_pc_free,
    &pc_pcfns_pc_share,
    &pc_pcfns_pc_dupl,
    &pc_pcfns_j_free_s,
    &pc_pcfns_j_free_m,
    &pc_pcfns_j_free_l_d,
    &pc_pcfns_j_free_k,
    &pc_pcfns_j_free_a,
    &pc_pcfns_j_free_j,
    &pc_pcfns_j_free_b_e,
    &pc_pcfns_j_dupl_s,
    &pc_pcfns_j_dupl_l_m_d,
    &pc_pcfns_j_dupl_a,
    &pc_pcfns_j_dupl_j,
    &pc_pcfns_j_dupl_b,
    &pc_pcfns_j_dupl_e,
    &pc_pcfns_j_dupl_k,
    &pc_pcfns_pc_makelist,
    &pc_pcfns_pc_makerecord,
    &pc_pcfns_pc_makearray,
    &pc_pcfns_pc_makerange,
    &pc_pcfns_pc_makeset,
    &pc_pcfns_pc_makestruct,
    &pc_pcfns_pc_makedict,
    &pc_pcfns_pc_storepacked,
    &pc_pcfns_adddictitem,
    &pc_pcfns_gethashvalue,
    &pc_pcfns_finddictitem,
    &pc_pcfns_expanddict,
    &pc_pcfns_setfslength,
    &pc_pcfns_getfslength,
    &pc_pcfns_pc_storeptr,
    &pc_pcfns_pc_storebit,
    &pc_pcfns_pc_popptrlist,
    &pc_pcfns_pc_loadpacked,
    &pc_pcfns_pc_loadbit,
    &pc_pcfns_pc_loadptr,
    &pc_pcfns_pc_storestring,
    &pc_pcfns_pc_iconvert,
    &pc_pcfns_pc_iconvcase,
    &pc_pcfns_pc_eqstring_nf,
    &pc_pcfns_pc_equal_nf,
    &pc_pcfns_comparebytes,
    &pc_pcfns_pc_compare_nf,
    &pc_pcfns_cmpstring_len,
    &pc_pcfns_pc_eqstring,
    &pc_pcfns_pc_equal,
    &pc_pcfns_pc_compare,
    &pc_pcfns_u8inarray,
    &pc_pcfns_u16inarray,
    &pc_pcfns_u32inarray,
    &pc_pcfns_u64inarray,
    &pc_pcfns_bitinbits,
    &pc_pcfns_pc_strinstr,
    &pc_pcfns_getbitoffset,
    &pc_pcfns_pc_iappendlist,
    &pc_pcfns_pc_iappendarray,
    &pc_pcfns_pc_mul_listi,
    &pc_pcfns_pc_mul_stri,
    &pc_pcfns_pc_duplvar,
    &pc_pcfns_pc_iconcatlist,
    &pc_pcfns_pc_iappendbits,
    &pc_pcfns_pc_makestring,
    &pc_pcfns_pc_makestringx,
    &pc_pcfns_pc_makestringn,
    &pc_pcfns_pc_emptystring,
    &pc_pcfns_pc_makechar,
    &pc_objlib_Dinit,
    &pc_objlib_obj_new,
    &pc_objlib_freeobject,
    &pc_objlib_array_new,
    &pc_objlib_list_new,
    &pc_objlib_objtovar,
    &pc_objlib_set_new,
    &pc_objlib_bits_new,
    &pc_objlib_struct_new,
    &pc_objlib_dict_new,
    &pc_objlib_record_new,
    &pc_objlib_list_free,
    &pc_objlib_record_free,
    &pc_objlib_array_free,
    &pc_objlib_bits_free,
    &pc_objlib_dict_free,
    &pc_objlib_bits_bytesize,
    &pc_objlib_list_resize,
    &pc_objlib_array_resize,
    &pc_objlib_bits_resize,
    &pc_objlib_string_resize,
    &pc_objlib_copyonwrite,
    &pc_objlib_make_strslicexobj,
    &pc_objlib_bignum_make,
    &pc_bignum_bx_makestr,
    &pc_bignum_bx_tostring,
    &pc_bignum_bx_dupl,
    &pc_bignum_bx_negto,
    &pc_bignum_bx_absto,
    &pc_bignum_makebnvar,
    &pc_bignum_bx_free,
    &pc_bignum_bx_makeint,
    &pc_bignum_bx_add,
    &pc_bignum_bx_sub,
    &pc_bignum_bx_mul,
    &pc_bignum_bx_div,
    &pc_bignum_bx_idiv,
    &pc_bignum_bx_irem,
    &pc_bignum_bx_equal,
    &pc_bignum_bx_cmp,
    &pc_bignum_bx_int,
    &pc_bignum_bx_power,
    &pc_bignum_bx_reduce,
    &pc_bignum_bx_length,
    &mbignum_bn_init,
    &mbignum_readexpon,
    &mbignum_bn_print,
    &mbignum_bn_println,
    &mbignum_getbintype,
    &mbignum_makebignum,
    &mbignum_makesmallnum,
    &mbignum_smalltobig,
    &mbignum_freesmall,
    &mbignum_bn_alloc,
    &mbignum_checkedmalloc,
    &mbignum_bn_free,
    &mbignum_freemem,
    &mbignum_bn_setzero,
    &mbignum_bn_move,
    &mbignum_bn_dupl,
    &mbignum_bn_setinf,
    &mbignum_bn_setnan,
    &mbignum_bn_error,
    &mbignum_bn_iszero,
    &mbignum_bn_negto,
    &mbignum_bn_absto,
    &mbignum_bn_isint,
    &mbignum_bn_getprec,
    &mbignum_bn_setprec,
    &mbignum_bn_getglobalprec,
    &mbignum_bn_setglobalprec,
    &mbignum_bn_makeint,
    &mbignum_bn_makefloat,
    &mbignum_bn_ipower,
    &mbignum_smallsubto,
    &mbignum_smallmulto,
    &mbignum_bn_equal,
    &mbignum_bn_addu,
    &mbignum_bn_subu,
    &mbignum_bn_mulu,
    &mbignum_smalldiv,
    &mbignum_bn_idivu,
    &mbignum_strvaln,
    &mbignum_bn_makestr,
    &mbignum_bn_fdivu,
    &mbignum_tostring_float,
    &mbignum_bn_tostring,
    &mbignum_tostring_scient,
    &mbignum_bn_add,
    &mbignum_bn_sub,
    &mbignum_bn_mul,
    &mbignum_bn_mulp,
    &mbignum_bn_div,
    &mbignum_bn_idiv,
    &mbignum_bn_idivrem,
    &mbignum_bn_irem,
    &mbignum_bn_cmp,
    &mbignum_bn_const,
    &mbignum_bn_sign,
    &mbignum_badnumber,
    &mbignum_bn_digits,
    &mbignum_bn_toint,
    &mbignum_bn_tofloat,
    &mbignum_bn_fix,
    &mbignum_bntest,
    &pc_print_pch_print,
    &pc_print_pch_println,
    &pc_print_pch_startprintcon,
    &pc_print_pch_startprint,
    &pc_print_pch_endprint,
    &pc_print_pch_strstartprint,
    &pc_print_pch_strendprint,
    &pc_print_pch_setformat,
    &pc_print_pch_setformat2,
    &pc_print_pch_dprint,
    &pc_print_pch_printnogap,
    &pc_print_initfmtcode,
    &pc_print_i64mintostr,
    &pc_print_u64tostr,
    &pc_print_i64tostrfmt,
    &pc_print_u64tostrfmt,
    &pc_print_strtostrfmt,
    &pc_print_expandstr,
    &pc_print_pc_strtofmt,
    &pc_print_printstrz,
    &pc_print_printstr_n,
    &pc_print_printerror,
    &pc_print_addstring,
    &pc_print_j_tostr_i,
    &pc_print_j_tostr_r,
    &pc_print_j_tostr_w,
    &pc_print_j_tostr_n,
    &pc_print_j_tostr_s,
    &pc_print_j_tostr_l_m,
    &pc_print_j_tostr_a,
    &pc_print_j_tostr_b,
    &pc_print_j_tostr_e,
    &pc_print_j_tostr_k,
    &pc_print_j_tostr_j,
    &pc_print_j_tostr_d,
    &pc_print_j_tostr_z,
    &pc_print_printnextfmtchars,
    &pc_print_getreadfmtcode,
    &pc_print_pch_sreadln,
    &pc_print_pch_strtoval,
    &pc_print_pch_reread,
    &pc_print_pch_rereadln,
    &pc_print_readname,
    &pc_print_readstring,
    &pc_print_readint,
    &pc_print_readhex,
    &pc_print_readbin,
    &pc_print_readreal,
    &pc_print_pch_readln,
    &pc_print_stepkbpos,
    &pc_print_pch_sread,
    &pc_print_domultichar,
    &pc_print_pch_tostr,
    &pc_print_pc_getfmt,
    &pc_print_pc_readlinen,
    &pc_print_readitem,
    &pc_print_readany,
    &pc_print_strtoreal,
    &pc_print_strtoint,
    &pc_print_calltostrtable,
    &pc_print_printbn,
    &pc_jhandlers_initcalltables,
    &pc_jhandlers_initjhandler,
    &pc_jhandlers_add_stable_entry,
    &pc_jhandlers_add_dtable_entry,
    &pc_jhandlers_def_handler,
    &pc_jhandlers_ddef_handler,
    &pc_jhandlers_j_add_i_w,
    &pc_jhandlers_j_add_r,
    &pc_jhandlers_j_add_s,
    &pc_jhandlers_j_add_j,
    &pc_jhandlers_j_add_e,
    &pc_jhandlers_j_add_z,
    &pc_jhandlers_j_add_iw_wi,
    &pc_jhandlers_j_add_zz,
    &pc_jhandlers_j_sub_i_w,
    &pc_jhandlers_j_sub_r,
    &pc_jhandlers_j_sub_p,
    &pc_jhandlers_j_sub_j,
    &pc_jhandlers_j_sub_e,
    &pc_jhandlers_j_sub_z,
    &pc_jhandlers_j_sub_zz,
    &pc_jhandlers_j_mixed_iw_wi,
    &pc_jhandlers_j_mixed_ir,
    &pc_jhandlers_j_mixed_ri,
    &pc_jhandlers_j_mixed_ij,
    &pc_jhandlers_j_mixed_ji,
    &pc_jhandlers_j_mul_i_w,
    &pc_jhandlers_j_mul_r,
    &pc_jhandlers_j_mul_j,
    &pc_jhandlers_j_mul_z,
    &pc_jhandlers_j_mul_li,
    &pc_jhandlers_j_mul_si,
    &pc_jhandlers_j_mul_e,
    &pc_jhandlers_j_mul_zz,
    &pc_jhandlers_j_div_i,
    &pc_jhandlers_j_div_r,
    &pc_jhandlers_j_div_j,
    &pc_jhandlers_j_div_z,
    &pc_jhandlers_j_jumple_i,
    &pc_jhandlers_j_jumple_r,
    &pc_jhandlers_j_jumple_s,
    &pc_jhandlers_j_jumple_z,
    &pc_jhandlers_j_jumpeq_i_r_t_o,
    &pc_jhandlers_j_jumpeq_v_p_f_g,
    &pc_jhandlers_j_jumpeq_s,
    &pc_jhandlers_j_jumpeq_z,
    &pc_jhandlers_j_jumpeq_zz,
    &pc_jhandlers_j_jumpne_i_r_t_o,
    &pc_jhandlers_j_jumpne_v_p_f_g,
    &pc_jhandlers_j_jumpne_s,
    &pc_jhandlers_j_jumpne_z,
    &pc_jhandlers_j_jumpne_zz,
    &pc_jhandlers_j_jumpge_i,
    &pc_jhandlers_j_jumpge_w,
    &pc_jhandlers_j_jumpge_r,
    &pc_jhandlers_j_jumpge_z,
    &pc_jhandlers_j_jumpgt_i,
    &pc_jhandlers_j_jumpgt_w,
    &pc_jhandlers_j_jumpgt_r,
    &pc_jhandlers_j_jumpgt_z,
    &pc_jhandlers_j_jumplt_i,
    &pc_jhandlers_j_jumplt_v_p,
    &pc_jhandlers_j_jumplt_r,
    &pc_jhandlers_j_jumplt_z,
    &pc_jhandlers_j_pushix_li,
    &pc_jhandlers_j_pushix_mi,
    &pc_jhandlers_j_pushix_vi,
    &pc_jhandlers_j_pushix_ln,
    &pc_jhandlers_j_pushix_ll,
    &pc_jhandlers_j_pushix_le,
    &pc_jhandlers_j_pushix_ai,
    &pc_jhandlers_j_pushix_bi_ei,
    &pc_jhandlers_j_pushix_an,
    &pc_jhandlers_j_pushix_si,
    &pc_jhandlers_j_pushix_sn,
    &pc_jhandlers_j_pushix_bn,
    &pc_jhandlers_j_pushix_di,
    &pc_jhandlers_j_pushix_zz,
    &pc_jhandlers_j_pushixref_li,
    &pc_jhandlers_j_pushixref_ln,
    &pc_jhandlers_j_pushixref_ai,
    &pc_jhandlers_j_pushixref_si,
    &pc_jhandlers_j_pushixref_bi,
    &pc_jhandlers_j_pushixref_zz,
    &pc_jhandlers_j_pushdotix_si,
    &pc_jhandlers_j_pushdotix_sn,
    &pc_jhandlers_j_pushdotix_ii,
    &pc_jhandlers_j_pushdotix_in,
    &pc_jhandlers_j_pushdotix_ei,
    &pc_jhandlers_j_pushdotix_mi,
    &pc_jhandlers_j_pushdotix_zz,
    &pc_jhandlers_j_pushdotixref_si,
    &pc_jhandlers_j_pushdotixref_sn,
    &pc_jhandlers_j_pushdotixref_ii,
    &pc_jhandlers_j_pushdotixref_in_wn,
    &pc_jhandlers_j_pushdotixref_ei,
    &pc_jhandlers_j_pushdotixref_zz,
    &pc_jhandlers_j_addto_i,
    &pc_jhandlers_j_addto_r,
    &pc_jhandlers_j_addto_s,
    &pc_jhandlers_j_addto_p,
    &pc_jhandlers_j_addto_z,
    &pc_jhandlers_j_addto_si,
    &pc_jhandlers_j_addto_ir,
    &pc_jhandlers_j_addto_ri,
    &pc_jhandlers_j_addto_zz,
    &pc_jhandlers_j_subto_i,
    &pc_jhandlers_j_subto_r,
    &pc_jhandlers_j_subto_z,
    &pc_jhandlers_j_subto_ir,
    &pc_jhandlers_j_subto_ri,
    &pc_jhandlers_j_subto_zz,
    &pc_jhandlers_j_multo_i_w,
    &pc_jhandlers_j_multo_r,
    &pc_jhandlers_j_multo_z,
    &pc_jhandlers_j_divto_i_w,
    &pc_jhandlers_j_divto_r,
    &pc_jhandlers_j_divto_z,
    &pc_jhandlers_j_idivto_i,
    &pc_jhandlers_j_idivto_z,
    &pc_jhandlers_j_iand_i_w,
    &pc_jhandlers_j_iand_e,
    &pc_jhandlers_j_iand_z,
    &pc_jhandlers_j_ior_i_w,
    &pc_jhandlers_j_ior_e,
    &pc_jhandlers_j_ior_z,
    &pc_jhandlers_j_ixor_i_w,
    &pc_jhandlers_j_ixor_e,
    &pc_jhandlers_j_ixor_z,
    &pc_jhandlers_j_iandto_i_w,
    &pc_jhandlers_j_iandto_z,
    &pc_jhandlers_j_iorto_i_w,
    &pc_jhandlers_j_iorto_z,
    &pc_jhandlers_j_ixorto_i_w,
    &pc_jhandlers_j_ixorto_z,
    &pc_jhandlers_j_shlto_i,
    &pc_jhandlers_j_shlto_z,
    &pc_jhandlers_j_shrto_i,
    &pc_jhandlers_j_shrto_z,
    &pc_jhandlers_j_concat_s,
    &pc_jhandlers_j_concat_l,
    &pc_jhandlers_j_concat_z,
    &pc_jhandlers_j_concatto_s,
    &pc_jhandlers_j_concatto_l,
    &pc_jhandlers_j_concatto_z,
    &pc_jhandlers_j_append_s,
    &pc_jhandlers_j_append_l,
    &pc_jhandlers_j_append_a,
    &pc_jhandlers_j_append_b,
    &pc_jhandlers_j_append_z,
    &pc_jhandlers_j_appendto_s,
    &pc_jhandlers_j_appendto_l,
    &pc_jhandlers_j_appendto_a,
    &pc_jhandlers_j_appendto_b_e,
    &pc_jhandlers_j_appendto_z,
    &pc_jhandlers_j_max_i,
    &pc_jhandlers_j_max_r,
    &pc_jhandlers_j_max_z,
    &pc_jhandlers_j_min_z,
    &pc_jhandlers_j_len_l_a_e_s_b_d,
    &pc_jhandlers_j_len_m_k,
    &pc_jhandlers_j_len_n,
    &pc_jhandlers_j_len_z,
    &pc_jhandlers_j_lwb_l,
    &pc_jhandlers_j_lwb_a_b,
    &pc_jhandlers_j_lwb_s_m_k_d,
    &pc_jhandlers_j_lwb_e,
    &pc_jhandlers_j_lwb_n,
    &pc_jhandlers_j_lwb_z,
    &pc_jhandlers_j_upb_l,
    &pc_jhandlers_j_upb_a_b,
    &pc_jhandlers_j_upb_s_d,
    &pc_jhandlers_j_upb_m_k,
    &pc_jhandlers_j_upb_e,
    &pc_jhandlers_j_upb_n,
    &pc_jhandlers_j_upb_z,
    &pc_jhandlers_j_bounds_l_a_b_s_e,
    &pc_jhandlers_j_bounds_m_k,
    &pc_jhandlers_j_bounds_n,
    &pc_jhandlers_j_bounds_z,
    &pc_jhandlers_j_minto_i,
    &pc_jhandlers_j_minto_r,
    &pc_jhandlers_j_minto_z,
    &pc_jhandlers_j_maxto_i,
    &pc_jhandlers_j_maxto_r,
    &pc_jhandlers_j_maxto_z,
    &pc_jhandlers_j_neg_i_w,
    &pc_jhandlers_j_neg_r,
    &pc_jhandlers_j_neg_j,
    &pc_jhandlers_j_neg_e,
    &pc_jhandlers_j_neg_z,
    &pc_jhandlers_j_abs_i_w,
    &pc_jhandlers_j_abs_r,
    &pc_jhandlers_j_abs_j,
    &pc_jhandlers_j_abs_z,
    &pc_jhandlers_j_inot_i_w,
    &pc_jhandlers_j_inot_e,
    &pc_jhandlers_j_inot_z,
    &pc_jhandlers_j_istrue_i_w_r,
    &pc_jhandlers_j_istrue_l_a_e_s_b,
    &pc_jhandlers_j_istrue_k_m_h,
    &pc_jhandlers_j_istrue_j,
    &pc_jhandlers_j_istrue_z,
    &pc_jhandlers_j_jumpfalse_i_w_r_v_p_f,
    &pc_jhandlers_j_jumpfalse_s_l_e_a_b,
    &pc_jhandlers_j_jumpfalse_z,
    &pc_jhandlers_j_jumptrue_i_r_w_v_p_f,
    &pc_jhandlers_j_jumptrue_s_l_e_a_b,
    &pc_jhandlers_j_jumptrue_z,
    &pc_jhandlers_j_shl_i_w,
    &pc_jhandlers_j_shl_z,
    &pc_jhandlers_j_shr_i_w,
    &pc_jhandlers_j_shr_j_i,
    &pc_jhandlers_j_shr_z,
    &pc_jhandlers_j_shr_wi,
    &pc_jhandlers_j_shr_zz,
    &pc_jhandlers_j_idiv_w,
    &pc_jhandlers_j_idiv_j,
    &pc_jhandlers_j_idiv_z,
    &pc_jhandlers_j_rem_w,
    &pc_jhandlers_j_rem_j,
    &pc_jhandlers_j_remz,
    &pc_oslayer_runproc_m,
    &pc_oslayer_os_getconsize,
    &pc_oslayer_pch_setmesshandler,
    &pc_oslayer_pch_gethostname,
    &pc_oslayer_os_initdllmodules,
    &pc_oslayer_os_loaddllmodule,
    &pc_oslayer_os_initdllfunctions,
    &pc_oslayer_os_loaddllfunction,
    &pc_oslayer_pch_getos,
    &pc_oslayer_pch_gethostsize,
    &pc_oslayer_pch_iswindows,
    &pc_oslayer_os_calldll,
    &oswindllc_os_calldllfunction,
    &oswindllc_os_pushargs,
    &oswindllc_calldll_cint,
    &oswindllc_calldll_creal,
    &oswindllc_os_dummycall,
    &pc_host_callhostfunction,
    &pc_host_pch_leftstr,
    &pc_host_pch_rightstr,
    &pc_host_pch_convlc,
    &pc_host_pch_convuc,
    &pc_host_pch_iconvlc,
    &pc_host_pch_iconvuc,
    &pc_host_pch_stop,
    &pc_host_pch_stopx,
    &pc_host_pch_ismain,
    &pc_host_pch_waitkey,
    &pc_host_pch_testkey,
    &pc_host_pch_execwait,
    &pc_host_pch_execcmd,
    &pc_host_pch_makestr,
    &pc_host_pch_makestrslice,
    &pc_host_pch_makeref,
    &pc_host_pch_new,
    &pc_host_pch_newheap,
    &pc_host_pch_heapvar,
    &pc_host_pch_freeheap,
    &pc_host_pch_getcmdparam,
    &pc_host_pch_setpcerror,
    &pc_host_pch_setdebug,
    &pc_host_pch_setfprintf,
    &pc_host_pch_ticks,
    &pc_host_pch_sleep,
    &pc_host_pch_random,
    &pc_host_pch_findmetafunction,
    &pc_host_pch_loadpcl,
    &pc_host_pch_runpcl,
    &pc_host_pch_runtask,
    &pc_host_pch_callext,
    &pc_host_pch_system,
    &pc_host_pch_shellexec,
    &pc_host_pch_gethash,
    &pc_host_pch_test,
    &pc_host_pch_pcldata,
    &pc_host_pch_getcstring,
    &pc_host_pch_getparam,
    &pc_host_pch_clearlist,
    &pc_host_pch_makelink,
    &pc_host_pch_allparams,
    &pc_host_pch_stackvars,
    &pc_host_pch_makeempty,
    &pc_host_pch_readlines,
    &pc_host_pch_dictitems,
    &pc_host_pch_setoverload,
    &pc_host_pch_errorinfo,
    &pc_host_getbounds,
    &pc_host_checkparam,
    &pc_host_leftstring,
    &pc_host_rightstring,
    &pc_host_padstring_right,
    &pc_host_padstring_left,
    &pc_host_pcld_makevint,
    &pc_host_pcld_makelist,
    &pc_host_getproctabledata,
    &pc_host_convert_handler,
    &pc_host_addtoproclist,
    &pc_host_tostr_handler,
    &pc_host_add_handler,
    &pc_host_addovrecord,
    &pc_host_findapplproc,
    &pc_host_do_callapplproc,
    &pc_host_vartopack,
    &pc_host_dummyfn,
    &pc_host_new_random,
    &pc_host_new_heapvar,
    &pc_host_mfib,
    &pc_dxfns_dx_iorset,
    &pc_dxfns_dx_iandset,
    &pc_dxfns_dx_ixorset,
    &pc_dxfns_dx_inotset,
    &pc_dxfns_dx_subset,
    &pc_dxfns_inotsetbits,
    &pc_dxfns_iorsetbits,
    &pc_dxfns_iandsetbits,
    &pc_dxfns_ixorsetbits,
    &pc_dxfns_subsetbits,
    &pc_dxfns_iresizeset,
    &pc_dxfns_dx_varinvar,
    &pc_dxfns_dx_mixed,
    &pc_khandlers_k_zero,
    &pc_khandlers_k_nop,
    &pc_khandlers_k_procstart,
    &pc_khandlers_k_procend,
    &pc_khandlers_k_endmodule,
    &pc_khandlers_k_push_m,
    &pc_khandlers_k_push_f,
    &pc_khandlers_k_push_am,
    &pc_khandlers_k_push_af,
    &pc_khandlers_k_push_ap,
    &pc_khandlers_k_push_al,
    &pc_khandlers_k_push_ci,
    &pc_khandlers_k_push_cw,
    &pc_khandlers_k_push_cr,
    &pc_khandlers_k_push_cn,
    &pc_khandlers_k_push_cs,
    &pc_khandlers_k_push_t,
    &pc_khandlers_k_push_op,
    &pc_khandlers_k_pushz,
    &pc_khandlers_k_pushz_void,
    &pc_khandlers_k_pushz_str,
    &pc_khandlers_k_pushz_list,
    &pc_khandlers_k_pushz_listl,
    &pc_khandlers_k_pushz_set,
    &pc_khandlers_k_pushz_arrayl,
    &pc_khandlers_k_pop_m,
    &pc_khandlers_k_pop_f,
    &pc_khandlers_k_store_m,
    &pc_khandlers_k_store_f,
    &pc_khandlers_k_pushptr,
    &pc_khandlers_k_popptr,
    &pc_khandlers_k_storeptr,
    &pc_khandlers_k_zpop_m,
    &pc_khandlers_k_zpop_f,
    &pc_khandlers_k_zstore_m,
    &pc_khandlers_k_zstore_f,
    &pc_khandlers_k_copy,
    &pc_khandlers_k_swap,
    &pc_khandlers_k_convptr,
    &pc_khandlers_k_jump,
    &pc_khandlers_k_jumpptr,
    &pc_khandlers_k_jumptrue,
    &pc_khandlers_k_jumpfalse,
    &pc_khandlers_k_jumpdef,
    &pc_khandlers_k_jumpvoid,
    &pc_khandlers_k_jumpeq,
    &pc_khandlers_k_jumpne,
    &pc_khandlers_k_jumplt,
    &pc_khandlers_k_jumple,
    &pc_khandlers_k_jumpge,
    &pc_khandlers_k_jumpgt,
    &pc_khandlers_k_jumptesteq,
    &pc_khandlers_k_jumptestne,
    &pc_khandlers_k_jumplabel,
    &pc_khandlers_k_jumpclabel,
    &pc_khandlers_k_switch,
    &pc_khandlers_k_cswitch,
    &pc_khandlers_k_new,
    &pc_khandlers_k_to_f,
    &pc_khandlers_k_for_fci,
    &pc_khandlers_k_for_ff,
    &pc_khandlers_k_ford_fci,
    &pc_khandlers_k_ford_ff,
    &pc_khandlers_k_call,
    &pc_khandlers_k_callptr,
    &pc_khandlers_k_return,
    &pc_khandlers_k_startdll,
    &pc_khandlers_k_pushdll,
    &pc_khandlers_k_calldll,
    &pc_khandlers_k_callhost,
    &pc_khandlers_k_stackframe,
    &pc_khandlers_k_free,
    &pc_khandlers_k_addsp,
    &pc_khandlers_k_stop,
    &pc_khandlers_k_test,
    &pc_khandlers_k_makelist,
    &pc_khandlers_k_makerecord,
    &pc_khandlers_k_makearray,
    &pc_khandlers_k_makestruct,
    &pc_khandlers_k_makeset,
    &pc_khandlers_k_makerange,
    &pc_khandlers_k_makedict,
    &pc_khandlers_k_pushdot,
    &pc_khandlers_k_pushdotref,
    &pc_khandlers_k_softconv,
    &pc_khandlers_k_hardconv,
    &pc_khandlers_k_mixed,
    &pc_khandlers_k_incrptr,
    &pc_khandlers_k_incrto_m,
    &pc_khandlers_k_incrto_f,
    &pc_khandlers_k_loadincr,
    &pc_khandlers_k_incrload,
    &pc_khandlers_k_decrptr,
    &pc_khandlers_k_decrto_m,
    &pc_khandlers_k_decrto_f,
    &pc_khandlers_k_loaddecr,
    &pc_khandlers_k_decrload,
    &pc_khandlers_k_incr,
    &pc_khandlers_k_decr,
    &pc_khandlers_k_neg,
    &pc_khandlers_k_abs,
    &pc_khandlers_k_not,
    &pc_khandlers_k_inot,
    &pc_khandlers_k_istrue,
    &pc_khandlers_k_asc,
    &pc_khandlers_k_chr,
    &pc_khandlers_k_sqrt,
    &pc_khandlers_k_sqr,
    &pc_khandlers_k_cube,
    &pc_khandlers_k_sin,
    &pc_khandlers_k_cos,
    &pc_khandlers_k_tan,
    &pc_khandlers_k_asin,
    &pc_khandlers_k_acos,
    &pc_khandlers_k_atan,
    &pc_khandlers_k_sign,
    &pc_khandlers_k_ln,
    &pc_khandlers_k_log,
    &pc_khandlers_k_lg,
    &pc_khandlers_k_exp,
    &pc_khandlers_k_round,
    &pc_khandlers_k_floor,
    &pc_khandlers_k_ceil,
    &pc_khandlers_k_fract,
    &pc_khandlers_k_negto,
    &pc_khandlers_k_absto,
    &pc_khandlers_k_notto,
    &pc_khandlers_k_inotto,
    &pc_khandlers_k_len,
    &pc_khandlers_k_lwb,
    &pc_khandlers_k_upb,
    &pc_khandlers_k_bounds,
    &pc_khandlers_k_bits,
    &pc_khandlers_k_bytes,
    &pc_khandlers_k_type,
    &pc_khandlers_k_elemtype,
    &pc_khandlers_k_basetype,
    &pc_khandlers_k_minval,
    &pc_khandlers_k_maxval,
    &pc_khandlers_k_isint,
    &pc_khandlers_k_isreal,
    &pc_khandlers_k_isstring,
    &pc_khandlers_k_isrange,
    &pc_khandlers_k_isnumber,
    &pc_khandlers_k_isarray,
    &pc_khandlers_k_isrecord,
    &pc_khandlers_k_ispointer,
    &pc_khandlers_k_ismutable,
    &pc_khandlers_k_isset,
    &pc_khandlers_k_isvoid,
    &pc_khandlers_k_isdef,
    &pc_khandlers_k_tostr,
    &pc_khandlers_k_isequal,
    &pc_khandlers_k_add,
    &pc_khandlers_k_sub,
    &pc_khandlers_k_mul,
    &pc_khandlers_k_div,
    &pc_khandlers_k_idiv,
    &pc_khandlers_k_rem,
    &pc_khandlers_k_divrem,
    &pc_khandlers_k_iand,
    &pc_khandlers_k_ior,
    &pc_khandlers_k_ixor,
    &pc_khandlers_k_shl,
    &pc_khandlers_k_shr,
    &pc_khandlers_k_in,
    &pc_khandlers_k_notin,
    &pc_khandlers_k_inrev,
    &pc_khandlers_k_eq,
    &pc_khandlers_k_ne,
    &pc_khandlers_k_lt,
    &pc_khandlers_k_le,
    &pc_khandlers_k_ge,
    &pc_khandlers_k_gt,
    &pc_khandlers_k_min,
    &pc_khandlers_k_max,
    &pc_khandlers_k_concat,
    &pc_khandlers_k_append,
    &pc_khandlers_k_power,
    &pc_khandlers_k_atan2,
    &pc_khandlers_k_addto,
    &pc_khandlers_k_subto,
    &pc_khandlers_k_multo,
    &pc_khandlers_k_divto,
    &pc_khandlers_k_idivto,
    &pc_khandlers_k_iandto,
    &pc_khandlers_k_iorto,
    &pc_khandlers_k_ixorto,
    &pc_khandlers_k_shlto,
    &pc_khandlers_k_shrto,
    &pc_khandlers_k_minto,
    &pc_khandlers_k_maxto,
    &pc_khandlers_k_concatto,
    &pc_khandlers_k_appendto,
    &pc_khandlers_k_pushix,
    &pc_khandlers_k_pushdotix,
    &pc_khandlers_k_pushkeyix,
    &pc_khandlers_k_pushkeyixd,
    &pc_khandlers_k_pushixref,
    &pc_khandlers_k_pushdotixref,
    &pc_khandlers_k_pushkeyixref,
    &pc_khandlers_k_pushbyteix,
    &pc_khandlers_k_pushbyteixref,
    &pc_khandlers_k_appendset,
    &pc_khandlers_k_pushdotm,
    &pc_khandlers_k_pushdott,
    &pc_khandlers_k_push_ad,
    &pc_khandlers_k_push_try,
    &pc_khandlers_k_raise,
    &pc_khandlers_k_applyop,
    &pc_khandlers_k_makeiter,
    &pc_khandlers_k_forall,
    &pc_khandlers_k_forallx,
    &pc_khandlers_k_foreach,
    &pc_khandlers_k_foreachx,
    &pc_khandlers_k_expandrange,
    &pc_khandlers_k_callappl,
    &pc_assemc_fixup_asm,
    &pc_assemc_asmavailable,
    &pc_assemc_addcountint,
    &pc_assemc_addcountext,
    &pc_assemc_showasmcmd,
    &pc_assemc_disploop_asm,
    &pc_assemc_getasmjump,
0};
static byte *  msysnewc__fnnames[]= {
    (byte*)"start",
    (byte*)"getinputoptions",
    (byte*)"do_option",
    (byte*)"getsyscmdline",
    (byte*)"showcaption",
    (byte*)"showhelp",
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
    (byte*)"runpcl",
    (byte*)"run_initdata",
    (byte*)"runpcprogram",
    (byte*)"initbytecode",
    (byte*)"disploop",
    (byte*)"pclinit",
    (byte*)"fixup_all_pc",
    (byte*)"fixup_module_pc",
    (byte*)"disploop_fn",
    (byte*)"disploop_deb",
    (byte*)"runproc",
    (byte*)"allocatestrings",
    (byte*)"compileq",
    (byte*)"loadpcfile",
    (byte*)"initpcldata",
    (byte*)"createstentry",
    (byte*)"getprocaddr",
    (byte*)"pcl_initusertypes",
    (byte*)"setcmdparam",
    (byte*)"prterror",
    (byte*)"testelem",
    (byte*)"setelem",
    (byte*)"pcustype_def",
    (byte*)"pcustype",
    (byte*)"pcustypet",
    (byte*)"pcmxtypes_def",
    (byte*)"pcmxtypes",
    (byte*)"pcmxtypestt",
    (byte*)"gettypename",
    (byte*)"inittypetables",
    (byte*)"pcerror",
    (byte*)"vxunimpl",
    (byte*)"pclunimpl",
    (byte*)"convcstring",
    (byte*)"getintvalue",
    (byte*)"nextpoweroftwo",
    (byte*)"showlinenumber",
    (byte*)"printlinenumber",
    (byte*)"findlinenumber",
    (byte*)"findpcindex",
    (byte*)"showlinetable",
    (byte*)"writezstring",
    (byte*)"writezint",
    (byte*)"writezint4",
    (byte*)"writezrange",
    (byte*)"writezreal",
    (byte*)"writezeof",
    (byte*)"zerror",
    (byte*)"readzvalue",
    (byte*)"readzint",
    (byte*)"readzdint",
    (byte*)"readzreal",
    (byte*)"readzstring",
    (byte*)"readzblock",
    (byte*)"checkmt",
    (byte*)"ipower",
    (byte*)"loaderror",
    (byte*)"gettypesig",
    (byte*)"getfnname",
    (byte*)"junimpl",
    (byte*)"raiseexception",
    (byte*)"raise_error",
    (byte*)"default_exception",
    (byte*)"pc_unshare",
    (byte*)"pc_free",
    (byte*)"pc_share",
    (byte*)"pc_dupl",
    (byte*)"j_free_s",
    (byte*)"j_free_m",
    (byte*)"j_free_l_d",
    (byte*)"j_free_k",
    (byte*)"j_free_a",
    (byte*)"j_free_j",
    (byte*)"j_free_b_e",
    (byte*)"j_dupl_s",
    (byte*)"j_dupl_l_m_d",
    (byte*)"j_dupl_a",
    (byte*)"j_dupl_j",
    (byte*)"j_dupl_b",
    (byte*)"j_dupl_e",
    (byte*)"j_dupl_k",
    (byte*)"pc_makelist",
    (byte*)"pc_makerecord",
    (byte*)"pc_makearray",
    (byte*)"pc_makerange",
    (byte*)"pc_makeset",
    (byte*)"pc_makestruct",
    (byte*)"pc_makedict",
    (byte*)"pc_storepacked",
    (byte*)"adddictitem",
    (byte*)"gethashvalue",
    (byte*)"finddictitem",
    (byte*)"expanddict",
    (byte*)"setfslength",
    (byte*)"getfslength",
    (byte*)"pc_storeptr",
    (byte*)"pc_storebit",
    (byte*)"pc_popptrlist",
    (byte*)"pc_loadpacked",
    (byte*)"pc_loadbit",
    (byte*)"pc_loadptr",
    (byte*)"pc_storestring",
    (byte*)"pc_iconvert",
    (byte*)"pc_iconvcase",
    (byte*)"pc_eqstring_nf",
    (byte*)"pc_equal_nf",
    (byte*)"comparebytes",
    (byte*)"pc_compare_nf",
    (byte*)"cmpstring_len",
    (byte*)"pc_eqstring",
    (byte*)"pc_equal",
    (byte*)"pc_compare",
    (byte*)"u8inarray",
    (byte*)"u16inarray",
    (byte*)"u32inarray",
    (byte*)"u64inarray",
    (byte*)"bitinbits",
    (byte*)"pc_strinstr",
    (byte*)"getbitoffset",
    (byte*)"pc_iappendlist",
    (byte*)"pc_iappendarray",
    (byte*)"pc_mul_listi",
    (byte*)"pc_mul_stri",
    (byte*)"pc_duplvar",
    (byte*)"pc_iconcatlist",
    (byte*)"pc_iappendbits",
    (byte*)"pc_makestring",
    (byte*)"pc_makestringx",
    (byte*)"pc_makestringn",
    (byte*)"pc_emptystring",
    (byte*)"pc_makechar",
    (byte*)"$init",
    (byte*)"obj_new",
    (byte*)"freeobject",
    (byte*)"array_new",
    (byte*)"list_new",
    (byte*)"objtovar",
    (byte*)"set_new",
    (byte*)"bits_new",
    (byte*)"struct_new",
    (byte*)"dict_new",
    (byte*)"record_new",
    (byte*)"list_free",
    (byte*)"record_free",
    (byte*)"array_free",
    (byte*)"bits_free",
    (byte*)"dict_free",
    (byte*)"bits_bytesize",
    (byte*)"list_resize",
    (byte*)"array_resize",
    (byte*)"bits_resize",
    (byte*)"string_resize",
    (byte*)"copyonwrite",
    (byte*)"make_strslicexobj",
    (byte*)"bignum_make",
    (byte*)"bx_makestr",
    (byte*)"bx_tostring",
    (byte*)"bx_dupl",
    (byte*)"bx_negto",
    (byte*)"bx_absto",
    (byte*)"makebnvar",
    (byte*)"bx_free",
    (byte*)"bx_makeint",
    (byte*)"bx_add",
    (byte*)"bx_sub",
    (byte*)"bx_mul",
    (byte*)"bx_div",
    (byte*)"bx_idiv",
    (byte*)"bx_irem",
    (byte*)"bx_equal",
    (byte*)"bx_cmp",
    (byte*)"bx_int",
    (byte*)"bx_power",
    (byte*)"bx_reduce",
    (byte*)"bx_length",
    (byte*)"bn_init",
    (byte*)"readexpon",
    (byte*)"bn_print",
    (byte*)"bn_println",
    (byte*)"getbintype",
    (byte*)"makebignum",
    (byte*)"makesmallnum",
    (byte*)"smalltobig",
    (byte*)"freesmall",
    (byte*)"bn_alloc",
    (byte*)"checkedmalloc",
    (byte*)"bn_free",
    (byte*)"freemem",
    (byte*)"bn_setzero",
    (byte*)"bn_move",
    (byte*)"bn_dupl",
    (byte*)"bn_setinf",
    (byte*)"bn_setnan",
    (byte*)"bn_error",
    (byte*)"bn_iszero",
    (byte*)"bn_negto",
    (byte*)"bn_absto",
    (byte*)"bn_isint",
    (byte*)"bn_getprec",
    (byte*)"bn_setprec",
    (byte*)"bn_getglobalprec",
    (byte*)"bn_setglobalprec",
    (byte*)"bn_makeint",
    (byte*)"bn_makefloat",
    (byte*)"bn_ipower",
    (byte*)"smallsubto",
    (byte*)"smallmulto",
    (byte*)"bn_equal",
    (byte*)"bn_addu",
    (byte*)"bn_subu",
    (byte*)"bn_mulu",
    (byte*)"smalldiv",
    (byte*)"bn_idivu",
    (byte*)"strvaln",
    (byte*)"bn_makestr",
    (byte*)"bn_fdivu",
    (byte*)"tostring_float",
    (byte*)"bn_tostring",
    (byte*)"tostring_scient",
    (byte*)"bn_add",
    (byte*)"bn_sub",
    (byte*)"bn_mul",
    (byte*)"bn_mulp",
    (byte*)"bn_div",
    (byte*)"bn_idiv",
    (byte*)"bn_idivrem",
    (byte*)"bn_irem",
    (byte*)"bn_cmp",
    (byte*)"bn_const",
    (byte*)"bn_sign",
    (byte*)"badnumber",
    (byte*)"bn_digits",
    (byte*)"bn_toint",
    (byte*)"bn_tofloat",
    (byte*)"bn_fix",
    (byte*)"bntest",
    (byte*)"pch_print",
    (byte*)"pch_println",
    (byte*)"pch_startprintcon",
    (byte*)"pch_startprint",
    (byte*)"pch_endprint",
    (byte*)"pch_strstartprint",
    (byte*)"pch_strendprint",
    (byte*)"pch_setformat",
    (byte*)"pch_setformat2",
    (byte*)"pch_dprint",
    (byte*)"pch_printnogap",
    (byte*)"initfmtcode",
    (byte*)"i64mintostr",
    (byte*)"u64tostr",
    (byte*)"i64tostrfmt",
    (byte*)"u64tostrfmt",
    (byte*)"strtostrfmt",
    (byte*)"expandstr",
    (byte*)"pc_strtofmt",
    (byte*)"printstrz",
    (byte*)"printstr_n",
    (byte*)"printerror",
    (byte*)"addstring",
    (byte*)"j_tostr_i",
    (byte*)"j_tostr_r",
    (byte*)"j_tostr_w",
    (byte*)"j_tostr_n",
    (byte*)"j_tostr_s",
    (byte*)"j_tostr_l_m",
    (byte*)"j_tostr_a",
    (byte*)"j_tostr_b",
    (byte*)"j_tostr_e",
    (byte*)"j_tostr_k",
    (byte*)"j_tostr_j",
    (byte*)"j_tostr_d",
    (byte*)"j_tostr_z",
    (byte*)"printnextfmtchars",
    (byte*)"getreadfmtcode",
    (byte*)"pch_sreadln",
    (byte*)"pch_strtoval",
    (byte*)"pch_reread",
    (byte*)"pch_rereadln",
    (byte*)"readname",
    (byte*)"readstring",
    (byte*)"readint",
    (byte*)"readhex",
    (byte*)"readbin",
    (byte*)"readreal",
    (byte*)"pch_readln",
    (byte*)"stepkbpos",
    (byte*)"pch_sread",
    (byte*)"domultichar",
    (byte*)"pch_tostr",
    (byte*)"pc_getfmt",
    (byte*)"pc_readlinen",
    (byte*)"readitem",
    (byte*)"readany",
    (byte*)"strtoreal",
    (byte*)"strtoint",
    (byte*)"calltostrtable",
    (byte*)"printbn",
    (byte*)"initcalltables",
    (byte*)"initjhandler",
    (byte*)"add_stable_entry",
    (byte*)"add_dtable_entry",
    (byte*)"def_handler",
    (byte*)"ddef_handler",
    (byte*)"j_add_i_w",
    (byte*)"j_add_r",
    (byte*)"j_add_s",
    (byte*)"j_add_j",
    (byte*)"j_add_e",
    (byte*)"j_add_z",
    (byte*)"j_add_iw_wi",
    (byte*)"j_add_zz",
    (byte*)"j_sub_i_w",
    (byte*)"j_sub_r",
    (byte*)"j_sub_p",
    (byte*)"j_sub_j",
    (byte*)"j_sub_e",
    (byte*)"j_sub_z",
    (byte*)"j_sub_zz",
    (byte*)"j_mixed_iw_wi",
    (byte*)"j_mixed_ir",
    (byte*)"j_mixed_ri",
    (byte*)"j_mixed_ij",
    (byte*)"j_mixed_ji",
    (byte*)"j_mul_i_w",
    (byte*)"j_mul_r",
    (byte*)"j_mul_j",
    (byte*)"j_mul_z",
    (byte*)"j_mul_li",
    (byte*)"j_mul_si",
    (byte*)"j_mul_e",
    (byte*)"j_mul_zz",
    (byte*)"j_div_i",
    (byte*)"j_div_r",
    (byte*)"j_div_j",
    (byte*)"j_div_z",
    (byte*)"j_jumple_i",
    (byte*)"j_jumple_r",
    (byte*)"j_jumple_s",
    (byte*)"j_jumple_z",
    (byte*)"j_jumpeq_i_r_t_o",
    (byte*)"j_jumpeq_v_p_f_g",
    (byte*)"j_jumpeq_s",
    (byte*)"j_jumpeq_z",
    (byte*)"j_jumpeq_zz",
    (byte*)"j_jumpne_i_r_t_o",
    (byte*)"j_jumpne_v_p_f_g",
    (byte*)"j_jumpne_s",
    (byte*)"j_jumpne_z",
    (byte*)"j_jumpne_zz",
    (byte*)"j_jumpge_i",
    (byte*)"j_jumpge_w",
    (byte*)"j_jumpge_r",
    (byte*)"j_jumpge_z",
    (byte*)"j_jumpgt_i",
    (byte*)"j_jumpgt_w",
    (byte*)"j_jumpgt_r",
    (byte*)"j_jumpgt_z",
    (byte*)"j_jumplt_i",
    (byte*)"j_jumplt_v_p",
    (byte*)"j_jumplt_r",
    (byte*)"j_jumplt_z",
    (byte*)"j_pushix_li",
    (byte*)"j_pushix_mi",
    (byte*)"j_pushix_vi",
    (byte*)"j_pushix_ln",
    (byte*)"j_pushix_ll",
    (byte*)"j_pushix_le",
    (byte*)"j_pushix_ai",
    (byte*)"j_pushix_bi_ei",
    (byte*)"j_pushix_an",
    (byte*)"j_pushix_si",
    (byte*)"j_pushix_sn",
    (byte*)"j_pushix_bn",
    (byte*)"j_pushix_di",
    (byte*)"j_pushix_zz",
    (byte*)"j_pushixref_li",
    (byte*)"j_pushixref_ln",
    (byte*)"j_pushixref_ai",
    (byte*)"j_pushixref_si",
    (byte*)"j_pushixref_bi",
    (byte*)"j_pushixref_zz",
    (byte*)"j_pushdotix_si",
    (byte*)"j_pushdotix_sn",
    (byte*)"j_pushdotix_ii",
    (byte*)"j_pushdotix_in",
    (byte*)"j_pushdotix_ei",
    (byte*)"j_pushdotix_mi",
    (byte*)"j_pushdotix_zz",
    (byte*)"j_pushdotixref_si",
    (byte*)"j_pushdotixref_sn",
    (byte*)"j_pushdotixref_ii",
    (byte*)"j_pushdotixref_in_wn",
    (byte*)"j_pushdotixref_ei",
    (byte*)"j_pushdotixref_zz",
    (byte*)"j_addto_i",
    (byte*)"j_addto_r",
    (byte*)"j_addto_s",
    (byte*)"j_addto_p",
    (byte*)"j_addto_z",
    (byte*)"j_addto_si",
    (byte*)"j_addto_ir",
    (byte*)"j_addto_ri",
    (byte*)"j_addto_zz",
    (byte*)"j_subto_i",
    (byte*)"j_subto_r",
    (byte*)"j_subto_z",
    (byte*)"j_subto_ir",
    (byte*)"j_subto_ri",
    (byte*)"j_subto_zz",
    (byte*)"j_multo_i_w",
    (byte*)"j_multo_r",
    (byte*)"j_multo_z",
    (byte*)"j_divto_i_w",
    (byte*)"j_divto_r",
    (byte*)"j_divto_z",
    (byte*)"j_idivto_i",
    (byte*)"j_idivto_z",
    (byte*)"j_iand_i_w",
    (byte*)"j_iand_e",
    (byte*)"j_iand_z",
    (byte*)"j_ior_i_w",
    (byte*)"j_ior_e",
    (byte*)"j_ior_z",
    (byte*)"j_ixor_i_w",
    (byte*)"j_ixor_e",
    (byte*)"j_ixor_z",
    (byte*)"j_iandto_i_w",
    (byte*)"j_iandto_z",
    (byte*)"j_iorto_i_w",
    (byte*)"j_iorto_z",
    (byte*)"j_ixorto_i_w",
    (byte*)"j_ixorto_z",
    (byte*)"j_shlto_i",
    (byte*)"j_shlto_z",
    (byte*)"j_shrto_i",
    (byte*)"j_shrto_z",
    (byte*)"j_concat_s",
    (byte*)"j_concat_l",
    (byte*)"j_concat_z",
    (byte*)"j_concatto_s",
    (byte*)"j_concatto_l",
    (byte*)"j_concatto_z",
    (byte*)"j_append_s",
    (byte*)"j_append_l",
    (byte*)"j_append_a",
    (byte*)"j_append_b",
    (byte*)"j_append_z",
    (byte*)"j_appendto_s",
    (byte*)"j_appendto_l",
    (byte*)"j_appendto_a",
    (byte*)"j_appendto_b_e",
    (byte*)"j_appendto_z",
    (byte*)"j_max_i",
    (byte*)"j_max_r",
    (byte*)"j_max_z",
    (byte*)"j_min_z",
    (byte*)"j_len_l_a_e_s_b_d",
    (byte*)"j_len_m_k",
    (byte*)"j_len_n",
    (byte*)"j_len_z",
    (byte*)"j_lwb_l",
    (byte*)"j_lwb_a_b",
    (byte*)"j_lwb_s_m_k_d",
    (byte*)"j_lwb_e",
    (byte*)"j_lwb_n",
    (byte*)"j_lwb_z",
    (byte*)"j_upb_l",
    (byte*)"j_upb_a_b",
    (byte*)"j_upb_s_d",
    (byte*)"j_upb_m_k",
    (byte*)"j_upb_e",
    (byte*)"j_upb_n",
    (byte*)"j_upb_z",
    (byte*)"j_bounds_l_a_b_s_e",
    (byte*)"j_bounds_m_k",
    (byte*)"j_bounds_n",
    (byte*)"j_bounds_z",
    (byte*)"j_minto_i",
    (byte*)"j_minto_r",
    (byte*)"j_minto_z",
    (byte*)"j_maxto_i",
    (byte*)"j_maxto_r",
    (byte*)"j_maxto_z",
    (byte*)"j_neg_i_w",
    (byte*)"j_neg_r",
    (byte*)"j_neg_j",
    (byte*)"j_neg_e",
    (byte*)"j_neg_z",
    (byte*)"j_abs_i_w",
    (byte*)"j_abs_r",
    (byte*)"j_abs_j",
    (byte*)"j_abs_z",
    (byte*)"j_inot_i_w",
    (byte*)"j_inot_e",
    (byte*)"j_inot_z",
    (byte*)"j_istrue_i_w_r",
    (byte*)"j_istrue_l_a_e_s_b",
    (byte*)"j_istrue_k_m_h",
    (byte*)"j_istrue_j",
    (byte*)"j_istrue_z",
    (byte*)"j_jumpfalse_i_w_r_v_p_f",
    (byte*)"j_jumpfalse_s_l_e_a_b",
    (byte*)"j_jumpfalse_z",
    (byte*)"j_jumptrue_i_r_w_v_p_f",
    (byte*)"j_jumptrue_s_l_e_a_b",
    (byte*)"j_jumptrue_z",
    (byte*)"j_shl_i_w",
    (byte*)"j_shl_z",
    (byte*)"j_shr_i_w",
    (byte*)"j_shr_j_i",
    (byte*)"j_shr_z",
    (byte*)"j_shr_wi",
    (byte*)"j_shr_zz",
    (byte*)"j_idiv_w",
    (byte*)"j_idiv_j",
    (byte*)"j_idiv_z",
    (byte*)"j_rem_w",
    (byte*)"j_rem_j",
    (byte*)"j_remz",
    (byte*)"runproc_m",
    (byte*)"os_getconsize",
    (byte*)"pch_setmesshandler",
    (byte*)"pch_gethostname",
    (byte*)"os_initdllmodules",
    (byte*)"os_loaddllmodule",
    (byte*)"os_initdllfunctions",
    (byte*)"os_loaddllfunction",
    (byte*)"pch_getos",
    (byte*)"pch_gethostsize",
    (byte*)"pch_iswindows",
    (byte*)"os_calldll",
    (byte*)"os_calldllfunction",
    (byte*)"os_pushargs",
    (byte*)"calldll_cint",
    (byte*)"calldll_creal",
    (byte*)"os_dummycall",
    (byte*)"callhostfunction",
    (byte*)"pch_leftstr",
    (byte*)"pch_rightstr",
    (byte*)"pch_convlc",
    (byte*)"pch_convuc",
    (byte*)"pch_iconvlc",
    (byte*)"pch_iconvuc",
    (byte*)"pch_stop",
    (byte*)"pch_stopx",
    (byte*)"pch_ismain",
    (byte*)"pch_waitkey",
    (byte*)"pch_testkey",
    (byte*)"pch_execwait",
    (byte*)"pch_execcmd",
    (byte*)"pch_makestr",
    (byte*)"pch_makestrslice",
    (byte*)"pch_makeref",
    (byte*)"pch_new",
    (byte*)"pch_newheap",
    (byte*)"pch_heapvar",
    (byte*)"pch_freeheap",
    (byte*)"pch_getcmdparam",
    (byte*)"pch_setpcerror",
    (byte*)"pch_setdebug",
    (byte*)"pch_setfprintf",
    (byte*)"pch_ticks",
    (byte*)"pch_sleep",
    (byte*)"pch_random",
    (byte*)"pch_findmetafunction",
    (byte*)"pch_loadpcl",
    (byte*)"pch_runpcl",
    (byte*)"pch_runtask",
    (byte*)"pch_callext",
    (byte*)"pch_system",
    (byte*)"pch_shellexec",
    (byte*)"pch_gethash",
    (byte*)"pch_test",
    (byte*)"pch_pcldata",
    (byte*)"pch_getcstring",
    (byte*)"pch_getparam",
    (byte*)"pch_clearlist",
    (byte*)"pch_makelink",
    (byte*)"pch_allparams",
    (byte*)"pch_stackvars",
    (byte*)"pch_makeempty",
    (byte*)"pch_readlines",
    (byte*)"pch_dictitems",
    (byte*)"pch_setoverload",
    (byte*)"pch_errorinfo",
    (byte*)"getbounds",
    (byte*)"checkparam",
    (byte*)"leftstring",
    (byte*)"rightstring",
    (byte*)"padstring_right",
    (byte*)"padstring_left",
    (byte*)"pcld_makevint",
    (byte*)"pcld_makelist",
    (byte*)"getproctabledata",
    (byte*)"convert_handler",
    (byte*)"addtoproclist",
    (byte*)"tostr_handler",
    (byte*)"add_handler",
    (byte*)"addovrecord",
    (byte*)"findapplproc",
    (byte*)"do_callapplproc",
    (byte*)"vartopack",
    (byte*)"dummyfn",
    (byte*)"new_random",
    (byte*)"new_heapvar",
    (byte*)"mfib",
    (byte*)"dx_iorset",
    (byte*)"dx_iandset",
    (byte*)"dx_ixorset",
    (byte*)"dx_inotset",
    (byte*)"dx_subset",
    (byte*)"inotsetbits",
    (byte*)"iorsetbits",
    (byte*)"iandsetbits",
    (byte*)"ixorsetbits",
    (byte*)"subsetbits",
    (byte*)"iresizeset",
    (byte*)"dx_varinvar",
    (byte*)"dx_mixed",
    (byte*)"k_zero",
    (byte*)"k_nop",
    (byte*)"k_procstart",
    (byte*)"k_procend",
    (byte*)"k_endmodule",
    (byte*)"k_push_m",
    (byte*)"k_push_f",
    (byte*)"k_push_am",
    (byte*)"k_push_af",
    (byte*)"k_push_ap",
    (byte*)"k_push_al",
    (byte*)"k_push_ci",
    (byte*)"k_push_cw",
    (byte*)"k_push_cr",
    (byte*)"k_push_cn",
    (byte*)"k_push_cs",
    (byte*)"k_push_t",
    (byte*)"k_push_op",
    (byte*)"k_pushz",
    (byte*)"k_pushz_void",
    (byte*)"k_pushz_str",
    (byte*)"k_pushz_list",
    (byte*)"k_pushz_listl",
    (byte*)"k_pushz_set",
    (byte*)"k_pushz_arrayl",
    (byte*)"k_pop_m",
    (byte*)"k_pop_f",
    (byte*)"k_store_m",
    (byte*)"k_store_f",
    (byte*)"k_pushptr",
    (byte*)"k_popptr",
    (byte*)"k_storeptr",
    (byte*)"k_zpop_m",
    (byte*)"k_zpop_f",
    (byte*)"k_zstore_m",
    (byte*)"k_zstore_f",
    (byte*)"k_copy",
    (byte*)"k_swap",
    (byte*)"k_convptr",
    (byte*)"k_jump",
    (byte*)"k_jumpptr",
    (byte*)"k_jumptrue",
    (byte*)"k_jumpfalse",
    (byte*)"k_jumpdef",
    (byte*)"k_jumpvoid",
    (byte*)"k_jumpeq",
    (byte*)"k_jumpne",
    (byte*)"k_jumplt",
    (byte*)"k_jumple",
    (byte*)"k_jumpge",
    (byte*)"k_jumpgt",
    (byte*)"k_jumptesteq",
    (byte*)"k_jumptestne",
    (byte*)"k_jumplabel",
    (byte*)"k_jumpclabel",
    (byte*)"k_switch",
    (byte*)"k_cswitch",
    (byte*)"k_new",
    (byte*)"k_to_f",
    (byte*)"k_for_fci",
    (byte*)"k_for_ff",
    (byte*)"k_ford_fci",
    (byte*)"k_ford_ff",
    (byte*)"k_call",
    (byte*)"k_callptr",
    (byte*)"k_return",
    (byte*)"k_startdll",
    (byte*)"k_pushdll",
    (byte*)"k_calldll",
    (byte*)"k_callhost",
    (byte*)"k_stackframe",
    (byte*)"k_free",
    (byte*)"k_addsp",
    (byte*)"k_stop",
    (byte*)"k_test",
    (byte*)"k_makelist",
    (byte*)"k_makerecord",
    (byte*)"k_makearray",
    (byte*)"k_makestruct",
    (byte*)"k_makeset",
    (byte*)"k_makerange",
    (byte*)"k_makedict",
    (byte*)"k_pushdot",
    (byte*)"k_pushdotref",
    (byte*)"k_softconv",
    (byte*)"k_hardconv",
    (byte*)"k_mixed",
    (byte*)"k_incrptr",
    (byte*)"k_incrto_m",
    (byte*)"k_incrto_f",
    (byte*)"k_loadincr",
    (byte*)"k_incrload",
    (byte*)"k_decrptr",
    (byte*)"k_decrto_m",
    (byte*)"k_decrto_f",
    (byte*)"k_loaddecr",
    (byte*)"k_decrload",
    (byte*)"k_incr",
    (byte*)"k_decr",
    (byte*)"k_neg",
    (byte*)"k_abs",
    (byte*)"k_not",
    (byte*)"k_inot",
    (byte*)"k_istrue",
    (byte*)"k_asc",
    (byte*)"k_chr",
    (byte*)"k_sqrt",
    (byte*)"k_sqr",
    (byte*)"k_cube",
    (byte*)"k_sin",
    (byte*)"k_cos",
    (byte*)"k_tan",
    (byte*)"k_asin",
    (byte*)"k_acos",
    (byte*)"k_atan",
    (byte*)"k_sign",
    (byte*)"k_ln",
    (byte*)"k_log",
    (byte*)"k_lg",
    (byte*)"k_exp",
    (byte*)"k_round",
    (byte*)"k_floor",
    (byte*)"k_ceil",
    (byte*)"k_fract",
    (byte*)"k_negto",
    (byte*)"k_absto",
    (byte*)"k_notto",
    (byte*)"k_inotto",
    (byte*)"k_len",
    (byte*)"k_lwb",
    (byte*)"k_upb",
    (byte*)"k_bounds",
    (byte*)"k_bits",
    (byte*)"k_bytes",
    (byte*)"k_type",
    (byte*)"k_elemtype",
    (byte*)"k_basetype",
    (byte*)"k_minval",
    (byte*)"k_maxval",
    (byte*)"k_isint",
    (byte*)"k_isreal",
    (byte*)"k_isstring",
    (byte*)"k_isrange",
    (byte*)"k_isnumber",
    (byte*)"k_isarray",
    (byte*)"k_isrecord",
    (byte*)"k_ispointer",
    (byte*)"k_ismutable",
    (byte*)"k_isset",
    (byte*)"k_isvoid",
    (byte*)"k_isdef",
    (byte*)"k_tostr",
    (byte*)"k_isequal",
    (byte*)"k_add",
    (byte*)"k_sub",
    (byte*)"k_mul",
    (byte*)"k_div",
    (byte*)"k_idiv",
    (byte*)"k_rem",
    (byte*)"k_divrem",
    (byte*)"k_iand",
    (byte*)"k_ior",
    (byte*)"k_ixor",
    (byte*)"k_shl",
    (byte*)"k_shr",
    (byte*)"k_in",
    (byte*)"k_notin",
    (byte*)"k_inrev",
    (byte*)"k_eq",
    (byte*)"k_ne",
    (byte*)"k_lt",
    (byte*)"k_le",
    (byte*)"k_ge",
    (byte*)"k_gt",
    (byte*)"k_min",
    (byte*)"k_max",
    (byte*)"k_concat",
    (byte*)"k_append",
    (byte*)"k_power",
    (byte*)"k_atan2",
    (byte*)"k_addto",
    (byte*)"k_subto",
    (byte*)"k_multo",
    (byte*)"k_divto",
    (byte*)"k_idivto",
    (byte*)"k_iandto",
    (byte*)"k_iorto",
    (byte*)"k_ixorto",
    (byte*)"k_shlto",
    (byte*)"k_shrto",
    (byte*)"k_minto",
    (byte*)"k_maxto",
    (byte*)"k_concatto",
    (byte*)"k_appendto",
    (byte*)"k_pushix",
    (byte*)"k_pushdotix",
    (byte*)"k_pushkeyix",
    (byte*)"k_pushkeyixd",
    (byte*)"k_pushixref",
    (byte*)"k_pushdotixref",
    (byte*)"k_pushkeyixref",
    (byte*)"k_pushbyteix",
    (byte*)"k_pushbyteixref",
    (byte*)"k_appendset",
    (byte*)"k_pushdotm",
    (byte*)"k_pushdott",
    (byte*)"k_push_ad",
    (byte*)"k_push_try",
    (byte*)"k_raise",
    (byte*)"k_applyop",
    (byte*)"k_makeiter",
    (byte*)"k_forall",
    (byte*)"k_forallx",
    (byte*)"k_foreach",
    (byte*)"k_foreachx",
    (byte*)"k_expandrange",
    (byte*)"k_callappl",
    (byte*)"fixup_asm",
    (byte*)"asmavailable",
    (byte*)"addcountint",
    (byte*)"addcountext",
    (byte*)"showasmcmd",
    (byte*)"disploop_asm",
    (byte*)"getasmjump",
(byte*)""};
static struct msysnewc_procinforec msysnewc__fnexports[]= {
{809, 1,3, {1,1,1,0,0,0,0,0,0,0,0,0}},
{810, 19,1, {19,0,0,0,0,0,0,0,0,0,0,0}},
{811, 19,1, {19,0,0,0,0,0,0,0,0,0,0,0}},
{812, 1,1, {1,0,0,0,0,0,0,0,0,0,0,0}},
	{0, 0,0, {0,0,0, 0,0,0, 0,0,0, 0,0,0}}}
;
static i64 msysnewc__fnnprocs=1049;
static i64 msysnewc__fnnexports=4;
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
static void *  oswindows_hconsole;
static void *  oswindows_hconsolein;
static struct oswindows_input_record oswindows_lastkey;
static struct oswindows_input_record oswindows_pendkey;
static i64 oswindows_keypending;
static i64 (*oswindows_wndproc_callbackfn)(void *) = 0;
static i64 oswindows_init_flag = (i64)0;
i64 pci_dispatch_type = (i64)5;
byte *  pci_filetypenames[4] = {(byte*)"pc_file",(byte*)"q_file",(byte*)"qq_file",(byte*)"mem_file"};
byte *  pci_errorcodenames[5] = {(byte*)"Runtime error",(byte*)"General load error:",(byte*)"Couldn't compile:",(byte*)"File not found:",(byte*)"normal_exit"};
static void *  pci_handlertable[218] = {
    0,
    (void *)(&pc_khandlers_k_nop),
    (void *)(&pc_khandlers_k_procstart),
    (void *)(&pc_khandlers_k_procend),
    (void *)(&pc_khandlers_k_endmodule),
    (void *)(&pc_khandlers_k_push_m),
    (void *)(&pc_khandlers_k_push_f),
    (void *)(&pc_khandlers_k_push_am),
    (void *)(&pc_khandlers_k_push_af),
    (void *)(&pc_khandlers_k_push_ap),
    (void *)(&pc_khandlers_k_push_al),
    (void *)(&pc_khandlers_k_push_ci),
    (void *)(&pc_khandlers_k_push_cw),
    (void *)(&pc_khandlers_k_push_cr),
    (void *)(&pc_khandlers_k_push_cn),
    (void *)(&pc_khandlers_k_push_cs),
    (void *)(&pc_khandlers_k_push_t),
    (void *)(&pc_khandlers_k_push_op),
    (void *)(&pc_khandlers_k_pushz),
    (void *)(&pc_khandlers_k_pushz_void),
    (void *)(&pc_khandlers_k_pushz_str),
    (void *)(&pc_khandlers_k_pushz_list),
    (void *)(&pc_khandlers_k_pushz_listl),
    (void *)(&pc_khandlers_k_pushz_set),
    (void *)(&pc_khandlers_k_pushz_arrayl),
    (void *)(&pc_khandlers_k_pop_m),
    (void *)(&pc_khandlers_k_pop_f),
    (void *)(&pc_khandlers_k_store_m),
    (void *)(&pc_khandlers_k_store_f),
    (void *)(&pc_khandlers_k_pushptr),
    (void *)(&pc_khandlers_k_popptr),
    (void *)(&pc_khandlers_k_storeptr),
    (void *)(&pc_khandlers_k_zpop_m),
    (void *)(&pc_khandlers_k_zpop_f),
    (void *)(&pc_khandlers_k_zstore_m),
    (void *)(&pc_khandlers_k_zstore_f),
    (void *)(&pc_khandlers_k_copy),
    (void *)(&pc_khandlers_k_swap),
    (void *)(&pc_khandlers_k_convptr),
    (void *)(&pc_khandlers_k_jump),
    (void *)(&pc_khandlers_k_jumpptr),
    (void *)(&pc_khandlers_k_jumptrue),
    (void *)(&pc_khandlers_k_jumpfalse),
    (void *)(&pc_khandlers_k_jumpdef),
    (void *)(&pc_khandlers_k_jumpvoid),
    (void *)(&pc_khandlers_k_jumpeq),
    (void *)(&pc_khandlers_k_jumpne),
    (void *)(&pc_khandlers_k_jumplt),
    (void *)(&pc_khandlers_k_jumple),
    (void *)(&pc_khandlers_k_jumpge),
    (void *)(&pc_khandlers_k_jumpgt),
    (void *)(&pc_khandlers_k_jumptesteq),
    (void *)(&pc_khandlers_k_jumptestne),
    (void *)(&pc_khandlers_k_jumplabel),
    (void *)(&pc_khandlers_k_jumpclabel),
    (void *)(&pc_khandlers_k_switch),
    (void *)(&pc_khandlers_k_cswitch),
    (void *)(&pc_khandlers_k_new),
    (void *)(&pc_khandlers_k_to_f),
    (void *)(&pc_khandlers_k_for_fci),
    (void *)(&pc_khandlers_k_for_ff),
    (void *)(&pc_khandlers_k_ford_fci),
    (void *)(&pc_khandlers_k_ford_ff),
    (void *)(&pc_khandlers_k_call),
    (void *)(&pc_khandlers_k_callptr),
    (void *)(&pc_khandlers_k_return),
    (void *)(&pc_khandlers_k_startdll),
    (void *)(&pc_khandlers_k_pushdll),
    (void *)(&pc_khandlers_k_calldll),
    (void *)(&pc_khandlers_k_callhost),
    (void *)(&pc_khandlers_k_stackframe),
    (void *)(&pc_khandlers_k_free),
    (void *)(&pc_khandlers_k_addsp),
    (void *)(&pc_khandlers_k_stop),
    (void *)(&pc_khandlers_k_test),
    (void *)(&pc_khandlers_k_makelist),
    (void *)(&pc_khandlers_k_makerecord),
    (void *)(&pc_khandlers_k_makearray),
    (void *)(&pc_khandlers_k_makestruct),
    (void *)(&pc_khandlers_k_makeset),
    (void *)(&pc_khandlers_k_makerange),
    (void *)(&pc_khandlers_k_makedict),
    (void *)(&pc_khandlers_k_pushdot),
    (void *)(&pc_khandlers_k_pushdotref),
    (void *)(&pc_khandlers_k_softconv),
    (void *)(&pc_khandlers_k_hardconv),
    (void *)(&pc_khandlers_k_mixed),
    (void *)(&pc_khandlers_k_incrptr),
    (void *)(&pc_khandlers_k_incrto_m),
    (void *)(&pc_khandlers_k_incrto_f),
    (void *)(&pc_khandlers_k_loadincr),
    (void *)(&pc_khandlers_k_incrload),
    (void *)(&pc_khandlers_k_decrptr),
    (void *)(&pc_khandlers_k_decrto_m),
    (void *)(&pc_khandlers_k_decrto_f),
    (void *)(&pc_khandlers_k_loaddecr),
    (void *)(&pc_khandlers_k_decrload),
    (void *)(&pc_khandlers_k_incr),
    (void *)(&pc_khandlers_k_decr),
    (void *)(&pc_khandlers_k_neg),
    (void *)(&pc_khandlers_k_abs),
    (void *)(&pc_khandlers_k_not),
    (void *)(&pc_khandlers_k_inot),
    (void *)(&pc_khandlers_k_istrue),
    (void *)(&pc_khandlers_k_asc),
    (void *)(&pc_khandlers_k_chr),
    (void *)(&pc_khandlers_k_sqrt),
    (void *)(&pc_khandlers_k_sqr),
    (void *)(&pc_khandlers_k_cube),
    (void *)(&pc_khandlers_k_sin),
    (void *)(&pc_khandlers_k_cos),
    (void *)(&pc_khandlers_k_tan),
    (void *)(&pc_khandlers_k_asin),
    (void *)(&pc_khandlers_k_acos),
    (void *)(&pc_khandlers_k_atan),
    (void *)(&pc_khandlers_k_sign),
    (void *)(&pc_khandlers_k_ln),
    (void *)(&pc_khandlers_k_log),
    (void *)(&pc_khandlers_k_lg),
    (void *)(&pc_khandlers_k_exp),
    (void *)(&pc_khandlers_k_round),
    (void *)(&pc_khandlers_k_floor),
    (void *)(&pc_khandlers_k_ceil),
    (void *)(&pc_khandlers_k_fract),
    (void *)(&pc_khandlers_k_negto),
    (void *)(&pc_khandlers_k_absto),
    (void *)(&pc_khandlers_k_notto),
    (void *)(&pc_khandlers_k_inotto),
    (void *)(&pc_khandlers_k_len),
    (void *)(&pc_khandlers_k_lwb),
    (void *)(&pc_khandlers_k_upb),
    (void *)(&pc_khandlers_k_bounds),
    (void *)(&pc_khandlers_k_bits),
    (void *)(&pc_khandlers_k_bytes),
    (void *)(&pc_khandlers_k_type),
    (void *)(&pc_khandlers_k_elemtype),
    (void *)(&pc_khandlers_k_basetype),
    (void *)(&pc_khandlers_k_minval),
    (void *)(&pc_khandlers_k_maxval),
    (void *)(&pc_khandlers_k_isint),
    (void *)(&pc_khandlers_k_isreal),
    (void *)(&pc_khandlers_k_isstring),
    (void *)(&pc_khandlers_k_isrange),
    (void *)(&pc_khandlers_k_isnumber),
    (void *)(&pc_khandlers_k_isarray),
    (void *)(&pc_khandlers_k_isrecord),
    (void *)(&pc_khandlers_k_ispointer),
    (void *)(&pc_khandlers_k_ismutable),
    (void *)(&pc_khandlers_k_isset),
    (void *)(&pc_khandlers_k_isvoid),
    (void *)(&pc_khandlers_k_isdef),
    (void *)(&pc_khandlers_k_tostr),
    (void *)(&pc_khandlers_k_isequal),
    (void *)(&pc_khandlers_k_add),
    (void *)(&pc_khandlers_k_sub),
    (void *)(&pc_khandlers_k_mul),
    (void *)(&pc_khandlers_k_div),
    (void *)(&pc_khandlers_k_idiv),
    (void *)(&pc_khandlers_k_rem),
    (void *)(&pc_khandlers_k_divrem),
    (void *)(&pc_khandlers_k_iand),
    (void *)(&pc_khandlers_k_ior),
    (void *)(&pc_khandlers_k_ixor),
    (void *)(&pc_khandlers_k_shl),
    (void *)(&pc_khandlers_k_shr),
    (void *)(&pc_khandlers_k_in),
    (void *)(&pc_khandlers_k_notin),
    (void *)(&pc_khandlers_k_inrev),
    (void *)(&pc_khandlers_k_eq),
    (void *)(&pc_khandlers_k_ne),
    (void *)(&pc_khandlers_k_lt),
    (void *)(&pc_khandlers_k_le),
    (void *)(&pc_khandlers_k_ge),
    (void *)(&pc_khandlers_k_gt),
    (void *)(&pc_khandlers_k_min),
    (void *)(&pc_khandlers_k_max),
    (void *)(&pc_khandlers_k_concat),
    (void *)(&pc_khandlers_k_append),
    (void *)(&pc_khandlers_k_power),
    (void *)(&pc_khandlers_k_atan2),
    (void *)(&pc_khandlers_k_addto),
    (void *)(&pc_khandlers_k_subto),
    (void *)(&pc_khandlers_k_multo),
    (void *)(&pc_khandlers_k_divto),
    (void *)(&pc_khandlers_k_idivto),
    (void *)(&pc_khandlers_k_iandto),
    (void *)(&pc_khandlers_k_iorto),
    (void *)(&pc_khandlers_k_ixorto),
    (void *)(&pc_khandlers_k_shlto),
    (void *)(&pc_khandlers_k_shrto),
    (void *)(&pc_khandlers_k_minto),
    (void *)(&pc_khandlers_k_maxto),
    (void *)(&pc_khandlers_k_concatto),
    (void *)(&pc_khandlers_k_appendto),
    (void *)(&pc_khandlers_k_pushix),
    (void *)(&pc_khandlers_k_pushdotix),
    (void *)(&pc_khandlers_k_pushkeyix),
    (void *)(&pc_khandlers_k_pushkeyixd),
    (void *)(&pc_khandlers_k_pushixref),
    (void *)(&pc_khandlers_k_pushdotixref),
    (void *)(&pc_khandlers_k_pushkeyixref),
    (void *)(&pc_khandlers_k_pushbyteix),
    (void *)(&pc_khandlers_k_pushbyteixref),
    (void *)(&pc_khandlers_k_appendset),
    (void *)(&pc_khandlers_k_pushdotm),
    (void *)(&pc_khandlers_k_pushdott),
    (void *)(&pc_khandlers_k_push_ad),
    (void *)(&pc_khandlers_k_push_try),
    (void *)(&pc_khandlers_k_raise),
    (void *)(&pc_khandlers_k_applyop),
    (void *)(&pc_khandlers_k_makeiter),
    (void *)(&pc_khandlers_k_forall),
    (void *)(&pc_khandlers_k_forallx),
    (void *)(&pc_khandlers_k_foreach),
    (void *)(&pc_khandlers_k_foreachx),
    (void *)(&pc_khandlers_k_expandrange),
    (void *)(&pc_khandlers_k_callappl),
    0
};
byte *  pc_types_stdtypenames[54] = {
    (byte*)"tvoid",
    (byte*)"tint",
    (byte*)"tword",
    (byte*)"treal",
    (byte*)"trange",
    (byte*)"tstring",
    (byte*)"twstring",
    (byte*)"tbignum",
    (byte*)"trational",
    (byte*)"tset",
    (byte*)"tdict",
    (byte*)"tword128",
    (byte*)"tenum",
    (byte*)"ttype",
    (byte*)"toperator",
    (byte*)"tsymbol",
    (byte*)"tretaddr",
    (byte*)"texception",
    (byte*)"trefproc",
    (byte*)"trefdllproc",
    (byte*)"treflabel",
    (byte*)"tstringz",
    (byte*)"trefvar",
    (byte*)"trefpacked",
    (byte*)"trefbit",
    (byte*)"trecordlink",
    (byte*)"treflist",
    (byte*)"trefarray",
    (byte*)"trefbits",
    (byte*)"tlist",
    (byte*)"tarray",
    (byte*)"tbits",
    (byte*)"trecord",
    (byte*)"tstruct",
    (byte*)"tuser",
    (byte*)"tvariant",
    (byte*)"tc8",
    (byte*)"ti8",
    (byte*)"ti16",
    (byte*)"ti32",
    (byte*)"ti64",
    (byte*)"tbit",
    (byte*)"tbit2",
    (byte*)"tbit4",
    (byte*)"tu8",
    (byte*)"tu16",
    (byte*)"tu32",
    (byte*)"tu64",
    (byte*)"tr32",
    (byte*)"tr64",
    (byte*)"tintm",
    (byte*)"twordm",
    (byte*)"trefm",
    (byte*)"tlast"
};
i64 pc_types_stdtypewidths[54] = {
    (i64)128,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)0,
    (i64)0,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)64,
    (i64)64,
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
    (i64)128,
    (i64)8,
    (i64)8,
    (i64)16,
    (i64)32,
    (i64)64,
    (i64)1,
    (i64)2,
    (i64)4,
    (i64)8,
    (i64)16,
    (i64)32,
    (i64)64,
    (i64)32,
    (i64)64,
    (i64)32,
    (i64)32,
    (i64)64,
    (i64)0
};
byte *  pc_decls_usercatnames[3] = {(byte*)"std_cat",(byte*)"anon_cat",(byte*)"user_cat"};
i64 pc_decls_ntypes;
i32 pc_decls_ttmodule[300];
struct pc_decls_strec *  pc_decls_ttnamedef[300];
i32 pc_decls_ttbasetype[300];
byte *  pc_decls_ttname[300];
i32 pc_decls_ttbitwidth[300];
i64 pc_decls_ttsize[300];
i32 pc_decls_ttlower[300];
u32 pc_decls_ttlength[300];
i32 pc_decls_ttstartfield[300];
i32 pc_decls_ttstructfields[300];
i32 pc_decls_tttarget[300];
byte pc_decls_ttusercat[300];
byte pc_decls_typestarterset[300];
byte *  pc_decls_objtypenames[3] = {(byte*)"normal_obj",(byte*)"slice_obj",(byte*)"extslice_obj"};
struct pc_decls_objrec *  pc_decls_chrtable[256];
struct pc_decls_modulerec pc_decls_moduletable[51];
i64 pc_decls_nmodules;
byte *  pc_decls_pendingmodules[50];
i64 pc_decls_npendingmodules;
i64 pc_decls_currmoduleno;
struct pc_decls_modulerec *  pc_decls_currmodule;
byte *  pc_decls_searchdirs[6];
i64 pc_decls_nsearchdirs = (i64)0;
struct pc_decls_strec *  pc_decls_stprogram;
i64 pc_decls_optflag = (i64)0;
struct pc_decls_genfieldnamerec pc_decls_genfieldnames[1000];
struct pc_decls_genfielddatarec pc_decls_genfielddata[1000];
u64 *  pc_decls_genfieldpcaddress[1000];
i64 pc_decls_ngenfieldnames;
i64 pc_decls_ngenfielddata;
byte *  pc_decls_libpaths[10];
i64 pc_decls_nlibpaths;
i64 pc_decls_ftrace = (i64)0;
i64 pc_decls_fdtrace = (i64)0;
i64 pc_decls_foptimise = (i64)0;
i64 pc_decls_mlineno = (i64)0;
i64 pc_decls_exportsprepass = (i64)0;
i64 pc_decls_debug = (i64)0;
i64 pc_decls_nnames;
i64 pc_decls_nchecks;
i64 pc_decls_forcheck;
i64 pc_decls_nclashes;
i64 pc_decls_nlookups;
i64 pc_decls_allnames;
i64 pc_decls_allfoundnames;
i64 pc_decls_allnotfoundnames;
struct pc_decls_varrec pc_decls_ttdeststrv;
struct pc_decls_varrec *  pc_decls_ttdeststr = &pc_decls_ttdeststrv;
i64 pc_decls_totalstrings = (i64)0;
byte *  pc_decls_dispatchnames[5] = {(byte*)"-lab",(byte*)"-fn",(byte*)"-deb1",(byte*)"-deb2",(byte*)"-asm"};
struct pc_decls_varrec *  pc_decls_sptr;
struct pc_decls_varrec *  pc_decls_stacklimit;
byte *  pc_decls_frameptr;
u64 *  pc_decls_pcptr;
struct pc_decls_varrec (*pc_decls_varstack)[];
struct pc_decls_objrec (*pc_decls_stringobjtable)[];
i64 pc_decls_dllindex;
i64 pc_decls_dllcallindex;
i64 pc_decls_dllparams[30];
i64 pc_decls_dllcallstack[30];
i16 pc_decls_dlltypes[30];
i64 pc_decls_ndlltable;
i64 pc_decls_ndllproctable;
byte *  pc_decls_dlltable[50];
u64 pc_decls_dllinsttable[50];
struct pc_decls_dllprocrec pc_decls_dllproctable[2000];
i64 pc_decls_napplproctable;
struct pc_decls_applprocrec pc_decls_applproctable[500];
void (*pc_decls_fprintf_ptr)(void);
void (*pc_decls_fgets_ptr)(void);
void (*pc_decls_pcl_callbackfn)(void) = 0;
byte *  pc_decls_pcerror_mess = 0;
struct pc_decls_varrec pc_decls_emptystringvar;
i64 pc_decls_ncmdparams;
byte *  pc_decls_cmdparamtable[33];
struct pc_decls_procrec *  pc_decls_proclist;
i64 pc_decls_nproclist;
i64 pc_decls_nstrings = (i64)0;
i64 pc_decls_nsymbols = (i64)0;
i64 pc_decls_nstructfields = (i64)0;
byte * (*pc_decls_stringtable)[];
i64 (*pc_decls_stringlentable)[];
struct pc_decls_strec (*pc_decls_pcsymboltable)[];
struct pc_decls_fieldrec (*pc_decls_pcfieldtable)[];
byte *  pc_decls_extrafiles[10];
byte *  pc_decls_extratext[10];
i64 pc_decls_extrasizes[10];
i64 pc_decls_nextra;
byte *  pc_decls_err_message;
struct pc_decls_varrec pc_decls_err_var1;
struct pc_decls_varrec pc_decls_err_var2;
u64 *  pc_decls_err_pcptr;
u64 *  pc_decls_stopseq;
u64 *  pc_decls_raiseseq;
u64 (*pc_decls_pccode)[];
i64 pc_decls_npccode = (i64)0;
i64 pc_decls_pcindex;
u16 (*pc_decls_linetable)[];
i64 pc_decls_cmdnopnds[218];
i64 pc_decls_lastticks = (i64)0;
u64 * (*pc_decls_neg_table[301])(void);
u64 * (*pc_decls_abs_table[301])(void);
u64 * (*pc_decls_inot_table[301])(void);
u64 * (*pc_decls_istrue_table[301])(void);
u64 * (*pc_decls_jumpf_table[301])(void);
u64 * (*pc_decls_jumpt_table[301])(void);
u64 * (*pc_decls_len_table[301])(void);
u64 * (*pc_decls_lwb_table[301])(void);
u64 * (*pc_decls_upb_table[301])(void);
u64 * (*pc_decls_bounds_table[301])(void);
u64 * (*pc_decls_incr_table[301])(void);
u64 * (*pc_decls_decr_table[301])(void);
u64 * (*pc_decls_add_table[301])(void);
u64 * (*pc_decls_sub_table[301])(void);
u64 * (*pc_decls_mul_table[301])(void);
u64 * (*pc_decls_div_table[301])(void);
u64 * (*pc_decls_idiv_table[301])(void);
u64 * (*pc_decls_rem_table[301])(void);
u64 * (*pc_decls_iand_table[301])(void);
u64 * (*pc_decls_ior_table[301])(void);
u64 * (*pc_decls_ixor_table[301])(void);
u64 * (*pc_decls_shl_table[301])(void);
u64 * (*pc_decls_shr_table[301])(void);
u64 * (*pc_decls_min_table[301])(void);
u64 * (*pc_decls_max_table[301])(void);
u64 * (*pc_decls_jumpeq_table[301])(void);
u64 * (*pc_decls_jumpne_table[301])(void);
u64 * (*pc_decls_jumplt_table[301])(void);
u64 * (*pc_decls_jumpgt_table[301])(void);
u64 * (*pc_decls_jumple_table[301])(void);
u64 * (*pc_decls_jumpge_table[301])(void);
u64 * (*pc_decls_jumptesteq_table[301])(void);
u64 * (*pc_decls_jumptestne_table[301])(void);
u64 * (*pc_decls_jumpfalse_table[301])(void);
u64 * (*pc_decls_jumptrue_table[301])(void);
u64 * (*pc_decls_eq_table[301])(void);
u64 * (*pc_decls_lt_table[301])(void);
u64 * (*pc_decls_le_table[301])(void);
u64 * (*pc_decls_concat_table[301])(void);
u64 * (*pc_decls_append_table[301])(void);
u64 * (*pc_decls_addto_table[301])(void);
u64 * (*pc_decls_subto_table[301])(void);
u64 * (*pc_decls_multo_table[301])(void);
u64 * (*pc_decls_divto_table[301])(void);
u64 * (*pc_decls_idivto_table[301])(void);
u64 * (*pc_decls_iandto_table[301])(void);
u64 * (*pc_decls_iorto_table[301])(void);
u64 * (*pc_decls_ixorto_table[301])(void);
u64 * (*pc_decls_shlto_table[301])(void);
u64 * (*pc_decls_shrto_table[301])(void);
u64 * (*pc_decls_minto_table[301])(void);
u64 * (*pc_decls_maxto_table[301])(void);
u64 * (*pc_decls_concatto_table[301])(void);
u64 * (*pc_decls_appendto_table[301])(void);
u64 * (*(*pc_decls_opc_tableptr)[301])(void);
void (*pc_decls_new_table[301])(struct pc_decls_varrec *,struct pc_decls_varrec *,struct pc_decls_varrec *,struct pc_decls_varrec *);
void (*pc_decls_free_table[301])(struct pc_decls_varrec *);
void (*pc_decls_dupl_table[301])(struct pc_decls_varrec *);
void (*pc_decls_tostr_table[301])(struct pc_decls_varrec *,struct pc_decls_varrec *,struct pc_decls_fmtrec *,struct pc_decls_objrec *);
u64 * (*pc_decls_in_dtable[301])(i64);
u64 * (*pc_decls_inrev_dtable[301])(i64);
u64 * (*pc_decls_pushix_dtable[301])(void);
u64 * (*pc_decls_pushixref_dtable[301])(void);
u64 * (*pc_decls_pushdotix_dtable[301])(void);
u64 * (*pc_decls_pushdotixref_dtable[301])(void);
u64 * (*pc_decls_mixed_dtable[301])(void);
u64 * (*pc_decls_convert_dtable[301])(i64);
u64 * (*pc_decls_mulx_dtable[301])(void);
u64 * (*pc_decls_add_dtable[301])(void);
u64 * (*pc_decls_sub_dtable[301])(void);
u64 * (*pc_decls_mul_dtable[301])(void);
u64 * (*pc_decls_div_dtable[301])(void);
u64 * (*pc_decls_idiv_dtable[301])(void);
u64 * (*pc_decls_rem_dtable[301])(void);
u64 * (*pc_decls_iand_dtable[301])(void);
u64 * (*pc_decls_ior_dtable[301])(void);
u64 * (*pc_decls_ixor_dtable[301])(void);
u64 * (*pc_decls_shl_dtable[301])(void);
u64 * (*pc_decls_shr_dtable[301])(void);
u64 * (*pc_decls_min_dtable[301])(void);
u64 * (*pc_decls_max_dtable[301])(void);
u64 * (*pc_decls_jumpeq_dtable[301])(void);
u64 * (*pc_decls_jumpne_dtable[301])(void);
u64 * (*pc_decls_jumplt_dtable[301])(void);
u64 * (*pc_decls_jumple_dtable[301])(void);
u64 * (*pc_decls_jumpge_dtable[301])(void);
u64 * (*pc_decls_jumpgt_dtable[301])(void);
u64 * (*pc_decls_addto_dtable[301])(void);
u64 * (*pc_decls_subto_dtable[301])(void);
u64 * (*pc_decls_multo_dtable[301])(void);
u64 * (*pc_decls_divto_dtable[301])(void);
u64 * (*pc_decls_idivto_dtable[301])(void);
u64 * (*pc_decls_iandto_dtable[301])(void);
u64 * (*pc_decls_iorto_dtable[301])(void);
u64 * (*pc_decls_ixorto_dtable[301])(void);
u64 * (*pc_decls_shlto_dtable[301])(void);
u64 * (*pc_decls_shrto_dtable[301])(void);
u64 * (*pc_decls_minto_dtable[301])(void);
u64 * (*pc_decls_maxto_dtable[301])(void);
i64 pc_decls_nexttypesig = (i64)0;
byte pc_decls_sigmap[256][256];
i64 pc_decls_overloadtype = (i64)0;
byte *  pc_decls_strpclversion;
i64 pc_decls_intcounts[2001];
i64 pc_decls_nallints;
i64 pc_decls_nsmallints;
i64 pc_decls_nallpops;
i64 pc_decls_nchanges;
byte *  pq_common_opndnames[18] = {
    (byte*)"cnone",
    (byte*)"cmemory",
    (byte*)"cframe",
    (byte*)"cproc",
    (byte*)"cdllproc",
    (byte*)"cdllvar",
    (byte*)"cfield",
    (byte*)"cgenfield",
    (byte*)"clabel",
    (byte*)"cint",
    (byte*)"cword",
    (byte*)"creal",
    (byte*)"crange",
    (byte*)"cstring",
    (byte*)"ctype",
    (byte*)"coperator",
    (byte*)"capplproc",
    (byte*)"?"
};
byte *  pq_common_cmdnames[218] = {
    (byte*)"kzero",
    (byte*)"knop",
    (byte*)"kprocstart",
    (byte*)"kprocend",
    (byte*)"kendmodule",
    (byte*)"kpush_m",
    (byte*)"kpush_f",
    (byte*)"kpush_am",
    (byte*)"kpush_af",
    (byte*)"kpush_ap",
    (byte*)"kpush_al",
    (byte*)"kpush_ci",
    (byte*)"kpush_cw",
    (byte*)"kpush_cr",
    (byte*)"kpush_cn",
    (byte*)"kpush_cs",
    (byte*)"kpush_t",
    (byte*)"kpush_op",
    (byte*)"kpushz",
    (byte*)"kpushz_void",
    (byte*)"kpushz_str",
    (byte*)"kpushz_list",
    (byte*)"kpushz_listl",
    (byte*)"kpushz_set",
    (byte*)"kpushz_arrayl",
    (byte*)"kpop_m",
    (byte*)"kpop_f",
    (byte*)"kstore_m",
    (byte*)"kstore_f",
    (byte*)"kpushptr",
    (byte*)"kpopptr",
    (byte*)"kstoreptr",
    (byte*)"kzpop_m",
    (byte*)"kzpop_f",
    (byte*)"kzstore_m",
    (byte*)"kzstore_f",
    (byte*)"kcopy",
    (byte*)"kswap",
    (byte*)"kconvptr",
    (byte*)"kjump",
    (byte*)"kjumpptr",
    (byte*)"kjumptrue",
    (byte*)"kjumpfalse",
    (byte*)"kjumpdef",
    (byte*)"kjumpvoid",
    (byte*)"kjumpeq",
    (byte*)"kjumpne",
    (byte*)"kjumplt",
    (byte*)"kjumple",
    (byte*)"kjumpge",
    (byte*)"kjumpgt",
    (byte*)"kjumptesteq",
    (byte*)"kjumptestne",
    (byte*)"kjumplabel",
    (byte*)"kjumpclabel",
    (byte*)"kswitch",
    (byte*)"kcswitch",
    (byte*)"knew",
    (byte*)"kto_f",
    (byte*)"kfor_fci",
    (byte*)"kfor_ff",
    (byte*)"kford_fci",
    (byte*)"kford_ff",
    (byte*)"kcall",
    (byte*)"kcallptr",
    (byte*)"kreturn",
    (byte*)"kstartdll",
    (byte*)"kpushdll",
    (byte*)"kcalldll",
    (byte*)"kcallhost",
    (byte*)"kstackframe",
    (byte*)"kfree",
    (byte*)"kaddsp",
    (byte*)"kstop",
    (byte*)"ktest",
    (byte*)"kmakelist",
    (byte*)"kmakerecord",
    (byte*)"kmakearray",
    (byte*)"kmakestruct",
    (byte*)"kmakeset",
    (byte*)"kmakerange",
    (byte*)"kmakedict",
    (byte*)"kpushdot",
    (byte*)"kpushdotref",
    (byte*)"ksoftconv",
    (byte*)"khardconv",
    (byte*)"kmixed",
    (byte*)"kincrptr",
    (byte*)"kincrto_m",
    (byte*)"kincrto_f",
    (byte*)"kloadincr",
    (byte*)"kincrload",
    (byte*)"kdecrptr",
    (byte*)"kdecrto_m",
    (byte*)"kdecrto_f",
    (byte*)"kloaddecr",
    (byte*)"kdecrload",
    (byte*)"kincr",
    (byte*)"kdecr",
    (byte*)"kneg",
    (byte*)"kabs",
    (byte*)"knot",
    (byte*)"kinot",
    (byte*)"kistrue",
    (byte*)"kasc",
    (byte*)"kchr",
    (byte*)"ksqrt",
    (byte*)"ksqr",
    (byte*)"kcube",
    (byte*)"ksin",
    (byte*)"kcos",
    (byte*)"ktan",
    (byte*)"kasin",
    (byte*)"kacos",
    (byte*)"katan",
    (byte*)"ksign",
    (byte*)"kln",
    (byte*)"klog",
    (byte*)"klg",
    (byte*)"kexp",
    (byte*)"kround",
    (byte*)"kfloor",
    (byte*)"kceil",
    (byte*)"kfract",
    (byte*)"knegto",
    (byte*)"kabsto",
    (byte*)"knotto",
    (byte*)"kinotto",
    (byte*)"klen",
    (byte*)"klwb",
    (byte*)"kupb",
    (byte*)"kbounds",
    (byte*)"kbits",
    (byte*)"kbytes",
    (byte*)"ktype",
    (byte*)"kelemtype",
    (byte*)"kbasetype",
    (byte*)"kminval",
    (byte*)"kmaxval",
    (byte*)"kisint",
    (byte*)"kisreal",
    (byte*)"kisstring",
    (byte*)"kisrange",
    (byte*)"kisnumber",
    (byte*)"kisarray",
    (byte*)"kisrecord",
    (byte*)"kispointer",
    (byte*)"kismutable",
    (byte*)"kisset",
    (byte*)"kisvoid",
    (byte*)"kisdef",
    (byte*)"ktostr",
    (byte*)"kisequal",
    (byte*)"kadd",
    (byte*)"ksub",
    (byte*)"kmul",
    (byte*)"kdiv",
    (byte*)"kidiv",
    (byte*)"krem",
    (byte*)"kdivrem",
    (byte*)"kiand",
    (byte*)"kior",
    (byte*)"kixor",
    (byte*)"kshl",
    (byte*)"kshr",
    (byte*)"kin",
    (byte*)"knotin",
    (byte*)"kinrev",
    (byte*)"keq",
    (byte*)"kne",
    (byte*)"klt",
    (byte*)"kle",
    (byte*)"kge",
    (byte*)"kgt",
    (byte*)"kmin",
    (byte*)"kmax",
    (byte*)"kconcat",
    (byte*)"kappend",
    (byte*)"kpower",
    (byte*)"katan2",
    (byte*)"kaddto",
    (byte*)"ksubto",
    (byte*)"kmulto",
    (byte*)"kdivto",
    (byte*)"kidivto",
    (byte*)"kiandto",
    (byte*)"kiorto",
    (byte*)"kixorto",
    (byte*)"kshlto",
    (byte*)"kshrto",
    (byte*)"kminto",
    (byte*)"kmaxto",
    (byte*)"kconcatto",
    (byte*)"kappendto",
    (byte*)"kpushix",
    (byte*)"kpushdotix",
    (byte*)"kpushkeyix",
    (byte*)"kpushkeyixd",
    (byte*)"kpushixref",
    (byte*)"kpushdotixref",
    (byte*)"kpushkeyixref",
    (byte*)"kpushbyteix",
    (byte*)"kpushbyteixref",
    (byte*)"kappendset",
    (byte*)"kpushdotm",
    (byte*)"kpushdott",
    (byte*)"kpush_ad",
    (byte*)"kpush_try",
    (byte*)"kraise",
    (byte*)"kapplyop",
    (byte*)"kmakeiter",
    (byte*)"kforall",
    (byte*)"kforallx",
    (byte*)"kforeach",
    (byte*)"kforeachx",
    (byte*)"kexpandrange",
    (byte*)"kcallappl",
    (byte*)"klastcmd"
};
byte pq_common_cmdfmt[218][4] = {
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)3u,(u8)9u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)1u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)2u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)1u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)2u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)3u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)9u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)10u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)11u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)12u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)13u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)14u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)15u,(u8)9u,(u8)0u,(u8)0u},
    {(u8)14u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)9u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)14u,(u8)9u,(u8)0u,(u8)0u},
    {(u8)1u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)2u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)1u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)2u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)1u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)2u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)1u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)2u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)9u,(u8)0u,(u8)0u},
    {(u8)9u,(u8)9u,(u8)0u,(u8)0u},
    {(u8)9u,(u8)9u,(u8)9u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)2u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)2u,(u8)9u,(u8)0u},
    {(u8)8u,(u8)2u,(u8)2u,(u8)0u},
    {(u8)8u,(u8)2u,(u8)9u,(u8)0u},
    {(u8)8u,(u8)2u,(u8)2u,(u8)0u},
    {(u8)3u,(u8)9u,(u8)0u,(u8)0u},
    {(u8)9u,(u8)9u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)14u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)4u,(u8)9u,(u8)14u,(u8)0u},
    {(u8)9u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)9u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)9u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)9u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)9u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)9u,(u8)9u,(u8)0u,(u8)0u},
    {(u8)9u,(u8)14u,(u8)0u,(u8)0u},
    {(u8)9u,(u8)9u,(u8)14u,(u8)14u},
    {(u8)9u,(u8)14u,(u8)0u,(u8)0u},
    {(u8)9u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)9u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)7u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)7u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)14u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)14u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)1u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)2u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)1u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)2u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)14u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)14u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)14u,(u8)3u,(u8)0u,(u8)0u},
    {(u8)14u,(u8)14u,(u8)0u,(u8)0u},
    {(u8)4u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)9u,(u8)9u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)9u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)9u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)8u,(u8)2u,(u8)2u,(u8)0u},
    {(u8)8u,(u8)2u,(u8)2u,(u8)2u},
    {(u8)8u,(u8)2u,(u8)2u,(u8)0u},
    {(u8)8u,(u8)2u,(u8)2u,(u8)2u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u},
    {(u8)16u,(u8)9u,(u8)0u,(u8)0u},
    {(u8)0u,(u8)0u,(u8)0u,(u8)0u}
};
void *  pq_common_cmdmap[218];
byte *  pq_common_bcdirnames[14] = {
    (byte*)"kkpclversion",
    (byte*)"kkmoduletable",
    (byte*)"kkdlltable",
    (byte*)"kkdllproctable",
    (byte*)"kksymboltable",
    (byte*)"kktypetable",
    (byte*)"kkgenfieldnames",
    (byte*)"kkgenfielddata",
    (byte*)"kkstringtable",
    (byte*)"kkstructtable",
    (byte*)"kkpccode",
    (byte*)"kkend",
    (byte*)"kknewstringtable",
    (byte*)"kkapplproctable"
};
byte *  pq_common_hostfnnames[73] = {
    (byte*)"host_dummy",
    (byte*)"host_startprint",
    (byte*)"host_startprintcon",
    (byte*)"host_strstartprint",
    (byte*)"host_setformat",
    (byte*)"host_endprint",
    (byte*)"host_strendprint",
    (byte*)"host_print",
    (byte*)"host_dprint",
    (byte*)"host_println",
    (byte*)"host_printnogap",
    (byte*)"host_readln",
    (byte*)"host_sreadln",
    (byte*)"host_sread",
    (byte*)"host_rereadln",
    (byte*)"host_reread",
    (byte*)"host_strtoval",
    (byte*)"host_tostr",
    (byte*)"host_leftstr",
    (byte*)"host_rightstr",
    (byte*)"host_convlc",
    (byte*)"host_convuc",
    (byte*)"host_iconvlc",
    (byte*)"host_iconvuc",
    (byte*)"host_stop",
    (byte*)"host_stopx",
    (byte*)"host_ismain",
    (byte*)"host_waitkey",
    (byte*)"host_testkey",
    (byte*)"host_execwait",
    (byte*)"host_execcmd",
    (byte*)"host_shellexec",
    (byte*)"host_system",
    (byte*)"host_makestr",
    (byte*)"host_makestrslice",
    (byte*)"host_makeref",
    (byte*)"host_new",
    (byte*)"host_newheap",
    (byte*)"host_readlines",
    (byte*)"host_heapvar",
    (byte*)"host_dictitems",
    (byte*)"host_freeheap",
    (byte*)"host_setoverload",
    (byte*)"host_getcmdparam",
    (byte*)"host_gethostname",
    (byte*)"host_setpcerror",
    (byte*)"host_setdebug",
    (byte*)"host_test",
    (byte*)"host_ticks",
    (byte*)"host_sleep",
    (byte*)"host_random",
    (byte*)"host_findmetafunction",
    (byte*)"host_gethash",
    (byte*)"host_getos",
    (byte*)"host_gethostsize",
    (byte*)"host_iswindows",
    (byte*)"host_setmesshandler",
    (byte*)"host_setfprintf",
    (byte*)"host_loadpcl",
    (byte*)"host_runpcl",
    (byte*)"host_runtask",
    (byte*)"host_callext",
    (byte*)"host_pcldata",
    (byte*)"host_getcstring",
    (byte*)"host_getparam",
    (byte*)"host_clearlist",
    (byte*)"host_makelink",
    (byte*)"host_allparams",
    (byte*)"host_stackvars",
    (byte*)"host_makeempty",
    (byte*)"host_errorinfo",
    (byte*)"host_strrepl",
    (byte*)"host_last"
};
i64 pq_common_hostnparams[73] = {
    (i64)0,
    (i64)1,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)0,
    (i64)0,
    (i64)2,
    (i64)2,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)0,
    (i64)0,
    (i64)2,
    (i64)2,
    (i64)3,
    (i64)3,
    (i64)2,
    (i64)2,
    (i64)2,
    (i64)2,
    (i64)0,
    (i64)1,
    (i64)1,
    (i64)0,
    (i64)0,
    (i64)3,
    (i64)3,
    (i64)2,
    (i64)1,
    (i64)2,
    (i64)2,
    (i64)2,
    (i64)4,
    (i64)4,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)3,
    (i64)1,
    (i64)0,
    (i64)1,
    (i64)1,
    (i64)2,
    (i64)0,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)2,
    (i64)2,
    (i64)2,
    (i64)2,
    (i64)3,
    (i64)2,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)0,
    (i64)1,
    (i64)1,
    (i64)3,
    (i64)0
};
i64 pq_common_hostisfn[73] = {
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
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)1,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)1,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)1,
    (i64)0,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)0,
    (i64)0,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)0,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)0,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)1,
    (i64)0
};
byte *  pq_common_namenames[20] = {
    (byte*)"nullid",
    (byte*)"programid",
    (byte*)"moduleid",
    (byte*)"dllmoduleid",
    (byte*)"typeid",
    (byte*)"procid",
    (byte*)"dllprocid",
    (byte*)"dllvarid",
    (byte*)"applprocid",
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
    (byte*)"linkid"
};
byte *  pq_common_errornames[7] = {(byte*)"pc_error",(byte*)"user_error",(byte*)"type_error",(byte*)"mixedtype_error",(byte*)"divide_error",(byte*)"stopmodule_error",(byte*)"bounds_error"};
byte pc_support_bytemasks[8] = {(u8)1u,(u8)2u,(u8)4u,(u8)8u,(u8)16u,(u8)32u,(u8)64u,(u8)128u};
i64 pc_pcfns_lookups;
i64 pc_pcfns_clashes;
i64 pc_pcfns_hist[65536];
struct pc_decls_objrec *  pc_objlib_zeroobj;
struct pc_decls_objrec *  pc_objlib_emptylist;
struct pc_decls_objrec *  pc_objlib_emptystring;
struct pc_decls_objrec *  pc_objlib_emptyset;
static byte *  mbignum_fpnames[4] = {(byte*)"zero_type",(byte*)"normal_type",(byte*)"inf_type",(byte*)"nan_type"};
static i64 mbignum_currprec = (i64)33;
static i64 mbignum_stblz;
static struct mbignum_constrec *  mbignum_constlist = 0;
i64 pc_print_mindev;
i64 pc_print_moutdev;
i64 *  pc_print_minchan;
void *  pc_print_moutchan;
struct pc_decls_varrec pc_print_minvar;
struct pc_decls_varrec pc_print_moutvar;
byte *  pc_print_mfmtstr;
byte *  pc_print_mfmtcurr;
struct pc_decls_fmtrec pc_print_defaultfmt = {
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
    {(u8)0u,(u8)0u}
};
void *  pc_print_testfilech;
static i32 pc_print_moutdevstack[6];
static void *  pc_print_moutchanstack[6];
static struct pc_decls_varrec pc_print_moutvarstack[6];
static byte pc_print_mgapstack[6];
static byte *  pc_print_mfmtstrstack[6];
static byte *  pc_print_mfmtcurrstack[6];
static i64 pc_print_noclevels;
static byte pc_print_mgapneeded;
static i64 pc_print_listdepth = (i64)0;
static byte pc_print_digits[16] = {
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
static byte *  pc_print_kb_start;
static byte *  pc_print_kb_pos;
static byte *  pc_print_kb_lastpos;
static i64 pc_print_kb_size;
static i64 pc_print_kb_linelength;
static i64 pc_print_kb_length;
static i64 pc_print_kb_lastlength;
static byte pc_print_termchar;
static i64 pc_print_itemerror;
static i32 pc_jhandlers_typemap[26] = {
    (i32)30,
    (i32)31,
    (i32)0,
    (i32)10,
    (i32)9,
    (i32)18,
    (i32)19,
    (i32)25,
    (i32)1,
    (i32)7,
    (i32)33,
    (i32)29,
    (i32)32,
    (i32)4,
    (i32)14,
    (i32)23,
    (i32)0,
    (i32)3,
    (i32)5,
    (i32)13,
    (i32)0,
    (i32)22,
    (i32)2,
    (i32)0,
    (i32)0,
    (i32)35
};
static byte *  pc_jhandlers_tabnames[66] = {
    (byte*)"add",
    (byte*)"sub",
    (byte*)"mul",
    (byte*)"div",
    (byte*)"idiv",
    (byte*)"rem",
    (byte*)"iand",
    (byte*)"ior",
    (byte*)"ixor",
    (byte*)"shl",
    (byte*)"shr",
    (byte*)"min",
    (byte*)"max",
    (byte*)"jumpeq",
    (byte*)"jumpne",
    (byte*)"jumplt",
    (byte*)"jumple",
    (byte*)"jumpge",
    (byte*)"jumpgt",
    (byte*)"jumptesteq",
    (byte*)"jumptestne",
    (byte*)"jumpfalse",
    (byte*)"jumptrue",
    (byte*)"eq",
    (byte*)"lt",
    (byte*)"le",
    (byte*)"concat",
    (byte*)"append",
    (byte*)"addto",
    (byte*)"subto",
    (byte*)"multo",
    (byte*)"divto",
    (byte*)"idivto",
    (byte*)"iandto",
    (byte*)"iorto",
    (byte*)"ixorto",
    (byte*)"shlto",
    (byte*)"shrto",
    (byte*)"minto",
    (byte*)"maxto",
    (byte*)"concatto",
    (byte*)"appendto",
    (byte*)"neg",
    (byte*)"abs",
    (byte*)"inot",
    (byte*)"istrue",
    (byte*)"jumpf",
    (byte*)"jumpt",
    (byte*)"len",
    (byte*)"lwb",
    (byte*)"upb",
    (byte*)"bounds",
    (byte*)"incr",
    (byte*)"decr",
    (byte*)"decr",
    (byte*)"free",
    (byte*)"dupl",
    (byte*)"tostr",
    (byte*)"in",
    (byte*)"inrev",
    (byte*)"pushix",
    (byte*)"pushixref",
    (byte*)"pushdotix",
    (byte*)"pushdotixref",
    (byte*)"convert",
    (byte*)"mixed"
};
static void *  pc_jhandlers_singletable[66] = {
    (void *)(&pc_decls_add_table),
    (void *)(&pc_decls_sub_table),
    (void *)(&pc_decls_mul_table),
    (void *)(&pc_decls_div_table),
    (void *)(&pc_decls_idiv_table),
    (void *)(&pc_decls_rem_table),
    (void *)(&pc_decls_iand_table),
    (void *)(&pc_decls_ior_table),
    (void *)(&pc_decls_ixor_table),
    (void *)(&pc_decls_shl_table),
    (void *)(&pc_decls_shr_table),
    (void *)(&pc_decls_min_table),
    (void *)(&pc_decls_max_table),
    (void *)(&pc_decls_jumpeq_table),
    (void *)(&pc_decls_jumpne_table),
    (void *)(&pc_decls_jumplt_table),
    (void *)(&pc_decls_jumple_table),
    (void *)(&pc_decls_jumpge_table),
    (void *)(&pc_decls_jumpgt_table),
    (void *)(&pc_decls_jumptesteq_table),
    (void *)(&pc_decls_jumptestne_table),
    (void *)(&pc_decls_jumpfalse_table),
    (void *)(&pc_decls_jumptrue_table),
    (void *)(&pc_decls_eq_table),
    (void *)(&pc_decls_lt_table),
    (void *)(&pc_decls_le_table),
    (void *)(&pc_decls_concat_table),
    (void *)(&pc_decls_append_table),
    (void *)(&pc_decls_addto_table),
    (void *)(&pc_decls_subto_table),
    (void *)(&pc_decls_multo_table),
    (void *)(&pc_decls_divto_table),
    (void *)(&pc_decls_idivto_table),
    (void *)(&pc_decls_iandto_table),
    (void *)(&pc_decls_iorto_table),
    (void *)(&pc_decls_ixorto_table),
    (void *)(&pc_decls_shlto_table),
    (void *)(&pc_decls_shrto_table),
    (void *)(&pc_decls_minto_table),
    (void *)(&pc_decls_maxto_table),
    (void *)(&pc_decls_concatto_table),
    (void *)(&pc_decls_appendto_table),
    (void *)(&pc_decls_neg_table),
    (void *)(&pc_decls_abs_table),
    (void *)(&pc_decls_inot_table),
    (void *)(&pc_decls_istrue_table),
    (void *)(&pc_decls_jumpf_table),
    (void *)(&pc_decls_jumpt_table),
    (void *)(&pc_decls_len_table),
    (void *)(&pc_decls_lwb_table),
    (void *)(&pc_decls_upb_table),
    (void *)(&pc_decls_bounds_table),
    (void *)(&pc_decls_incr_table),
    (void *)(&pc_decls_decr_table),
    (void *)(&pc_decls_decr_table),
    (void *)(&pc_decls_free_table),
    (void *)(&pc_decls_dupl_table),
    (void *)(&pc_decls_tostr_table),
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0
};
static void *  pc_jhandlers_doubletable[66] = {
    (void *)(&pc_decls_add_dtable),
    (void *)(&pc_decls_sub_dtable),
    (void *)(&pc_decls_mul_dtable),
    (void *)(&pc_decls_div_dtable),
    (void *)(&pc_decls_idiv_dtable),
    (void *)(&pc_decls_rem_dtable),
    (void *)(&pc_decls_iand_dtable),
    (void *)(&pc_decls_ior_dtable),
    (void *)(&pc_decls_ixor_dtable),
    (void *)(&pc_decls_shl_dtable),
    (void *)(&pc_decls_shr_dtable),
    (void *)(&pc_decls_min_dtable),
    (void *)(&pc_decls_max_dtable),
    (void *)(&pc_decls_jumpeq_dtable),
    (void *)(&pc_decls_jumpne_dtable),
    (void *)(&pc_decls_jumplt_dtable),
    (void *)(&pc_decls_jumple_dtable),
    (void *)(&pc_decls_jumpge_dtable),
    (void *)(&pc_decls_jumpgt_dtable),
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    (void *)(&pc_decls_addto_dtable),
    (void *)(&pc_decls_subto_dtable),
    (void *)(&pc_decls_multo_dtable),
    (void *)(&pc_decls_divto_dtable),
    (void *)(&pc_decls_idivto_dtable),
    (void *)(&pc_decls_iandto_dtable),
    (void *)(&pc_decls_iorto_dtable),
    (void *)(&pc_decls_ixorto_dtable),
    (void *)(&pc_decls_shlto_dtable),
    (void *)(&pc_decls_shrto_dtable),
    (void *)(&pc_decls_minto_dtable),
    (void *)(&pc_decls_maxto_dtable),
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
    (void *)(&pc_decls_in_dtable),
    (void *)(&pc_decls_inrev_dtable),
    (void *)(&pc_decls_pushix_dtable),
    (void *)(&pc_decls_pushixref_dtable),
    (void *)(&pc_decls_pushdotix_dtable),
    (void *)(&pc_decls_pushdotixref_dtable),
    (void *)(&pc_decls_convert_dtable),
    (void *)(&pc_decls_mixed_dtable)
};
static byte (*pc_jhandlers_mixedmap)[];
byte *  pc_host_packtypenames[20] = {
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
i64 pc_host_packtypewidths[20] = {
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
i64 pc_host_packconvtypes[20] = {
    (i64)0,
    (i64)1,
    (i64)2,
    (i64)3,
    (i64)23,
    (i64)23,
    (i64)23,
    (i64)23,
    (i64)23,
    (i64)23,
    (i64)23,
    (i64)23,
    (i64)23,
    (i64)23,
    (i64)23,
    (i64)23,
    (i64)23,
    (i64)23,
    (i64)21,
    (i64)35
};
static struct pc_host_overloadrec *  pc_host_tostr_list;
static struct pc_host_overloadrec *  pc_host_convert_list;
static struct pc_decls_procrec *  pc_host_proclistptr;
static struct pc_decls_varrec pc_host_applresult;
static void *  pc_host_hosttable[70] = {
    (void *)(&pc_print_pch_startprint),
    (void *)(&pc_print_pch_startprintcon),
    (void *)(&pc_print_pch_strstartprint),
    (void *)(&pc_print_pch_setformat),
    (void *)(&pc_print_pch_endprint),
    (void *)(&pc_print_pch_strendprint),
    (void *)(&pc_print_pch_print),
    (void *)(&pc_print_pch_dprint),
    (void *)(&pc_print_pch_println),
    (void *)(&pc_print_pch_printnogap),
    (void *)(&pc_print_pch_readln),
    (void *)(&pc_print_pch_sreadln),
    (void *)(&pc_print_pch_sread),
    (void *)(&pc_print_pch_rereadln),
    (void *)(&pc_print_pch_reread),
    (void *)(&pc_print_pch_strtoval),
    (void *)(&pc_print_pch_tostr),
    (void *)(&pc_host_pch_leftstr),
    (void *)(&pc_host_pch_rightstr),
    (void *)(&pc_host_pch_convlc),
    (void *)(&pc_host_pch_convuc),
    (void *)(&pc_host_pch_iconvlc),
    (void *)(&pc_host_pch_iconvuc),
    (void *)(&pc_host_pch_stop),
    (void *)(&pc_host_pch_stopx),
    (void *)(&pc_host_pch_ismain),
    (void *)(&pc_host_pch_waitkey),
    (void *)(&pc_host_pch_testkey),
    (void *)(&pc_host_pch_execwait),
    (void *)(&pc_host_pch_execcmd),
    (void *)(&pc_host_pch_shellexec),
    (void *)(&pc_host_pch_system),
    (void *)(&pc_host_pch_makestr),
    (void *)(&pc_host_pch_makestrslice),
    (void *)(&pc_host_pch_makeref),
    (void *)(&pc_host_pch_new),
    (void *)(&pc_host_pch_newheap),
    (void *)(&pc_host_pch_readlines),
    (void *)(&pc_host_pch_heapvar),
    (void *)(&pc_host_pch_dictitems),
    (void *)(&pc_host_pch_freeheap),
    (void *)(&pc_host_pch_setoverload),
    (void *)(&pc_host_pch_getcmdparam),
    (void *)(&pc_oslayer_pch_gethostname),
    (void *)(&pc_host_pch_setpcerror),
    (void *)(&pc_host_pch_setdebug),
    (void *)(&pc_host_pch_test),
    (void *)(&pc_host_pch_ticks),
    (void *)(&pc_host_pch_sleep),
    (void *)(&pc_host_pch_random),
    (void *)(&pc_host_pch_findmetafunction),
    (void *)(&pc_host_pch_gethash),
    (void *)(&pc_oslayer_pch_getos),
    (void *)(&pc_oslayer_pch_gethostsize),
    (void *)(&pc_oslayer_pch_iswindows),
    (void *)(&pc_oslayer_pch_setmesshandler),
    (void *)(&pc_host_pch_setfprintf),
    (void *)(&pc_host_pch_loadpcl),
    (void *)(&pc_host_pch_runpcl),
    (void *)(&pc_host_pch_runtask),
    (void *)(&pc_host_pch_callext),
    (void *)(&pc_host_pch_pcldata),
    (void *)(&pc_host_pch_getcstring),
    (void *)(&pc_host_pch_getparam),
    (void *)(&pc_host_pch_clearlist),
    (void *)(&pc_host_pch_makelink),
    (void *)(&pc_host_pch_allparams),
    (void *)(&pc_host_pch_stackvars),
    (void *)(&pc_host_pch_makeempty),
    (void *)(&pc_host_pch_errorinfo)
};
struct pc_decls_objrec *  pc_khandlers_zerostringobj;
byte pc_khandlers_stopped;

/* PROCDEFS */
// START
void start(void) {
    i64 stopcode;
    i64 filetype;
    pc_getinputoptions(&filetype);
    stopcode = pci_runpcl(pc_inputfile,filetype);
    if ((stopcode < (i64)0)) {
        pc_support_loaderror(pci_errorcodenames[(stopcode)+4],pc_inputfile);
    };
    exit(stopcode);
}

int main(int nargs, char** args) {
int i;
	msysnewc_nsysparams=nargs;
	if (msysnewc_nsysparams>nargs) {puts("Too many params"); exit(1);}
	for (i=1; i<=nargs; ++i) msysnewc_sysparams[i-1]=(byte*)args[i-1];

	pc_objlib_Dinit();

	start();
	return 0;
}

static void pc_getinputoptions(i64 * filetype) {
    i64 paramno;
    i64 pmtype;
    byte *  ext;
    byte *  name;
    byte *  value;
    i64 av_1;
    i64 sw;
    paramno = (i64)2;
    L1 :;
    while (!!((pmtype = mlib_nextcmdparam(&paramno,&name,&value,(byte*)"pc")))) {
        if ((pmtype==(i64)1)) {
            mlib_convlcstring(name);
            L4 :;
            for (sw=(i64)1;sw<=(i64)7;sw+=(i64)1) {
L5 :;
                if (!!(mlib_eqstring(name,pc_optionnames[(sw)-1]))) {
                    pc_do_option(sw,value);
                    goto L7 ;
                };
L6 :;
            }
            {
                msysnewc_m_print_startcon();
                msysnewc_m_print_str((byte*)"Unknown option:",NULL);
                msysnewc_m_print_str(name,NULL);
                msysnewc_m_print_newline();
                msysnewc_m_print_end();
                ;
                exit((i64)99);
            }L7 :;
            ;
        }else if ((pmtype==(i64)2)) {
            if (!!(pc_inputfile)) {
                pc_support_loaderror((byte*)"Only one input file allowed",(byte*)"");
            };
            pc_inputfile = mlib_pcm_copyheapstring(name);
            goto L3 ;
        } else {
            msysnewc_m_print_startcon();
            msysnewc_m_print_str((byte*)"Bad command param",NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            exit((i64)9);
        };
L2 :;
    }L3 :;
    ;
    if ((pc_inputfile == 0)) {
        pc_showcaption();
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"Usage:",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"\t",NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_str(msysnewc_sysparams[((i64)1)-1],NULL);
        msysnewc_m_print_str((byte*)"filename[.pc]",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"\t",NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_str(msysnewc_sysparams[((i64)1)-1],NULL);
        msysnewc_m_print_str((byte*)"filename.q",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"\t",NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_str(msysnewc_sysparams[((i64)1)-1],NULL);
        msysnewc_m_print_str((byte*)"-help",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        exit((i64)1);
    };
    ext = mlib_extractext(pc_inputfile,(i64)0);
    if (!!(mlib_eqstring(ext,(byte*)"pc"))) {
        (*filetype) = (i64)1;
    } else if (!!(mlib_eqstring(ext,(byte*)"q"))) {
        (*filetype) = (i64)2;
    } else {
        (*filetype) = (i64)2;
    };
    pc_getsyscmdline((paramno - (i64)1));
}

static void pc_do_option(i64 sw,byte * value) {
    if ((sw==(i64)1)) {
        pci_dispatch_type = (i64)2;
    }else if ((sw==(i64)2)) {
        pci_dispatch_type = (i64)5;
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"ASM DISPATCH",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    }else if ((sw==(i64)3)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"SET DEBUG DISPATCH",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pci_dispatch_type = (i64)3;
    }else if ((sw==(i64)4)) {
        pci_dispatch_type = (i64)4;
    }else if ((sw==(i64)6)) {
        pc_showhelp();
    }else if ((sw==(i64)5)) {
    };
}

static void pc_getsyscmdline(i64 n) {
    i64 i;
    pci_setcmdparam((i64)0,msysnewc_sysparams[((i64)1)-1]);
    L8 :;
    for (i=n;i<=msysnewc_nsysparams;i+=(i64)1) {
L9 :;
        pci_setcmdparam(((i - n) + (i64)1),msysnewc_sysparams[(i)-1]);
L10 :;
    }L11 :;
    ;
}

static void pc_showcaption(void) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"PCL Interpreter",NULL);
    msysnewc_m_print_str((byte*)"9-Sep-2020",NULL);
    msysnewc_m_print_str((byte*)"(",NULL);
    msysnewc_m_print_nogap();
    msysnewc_m_print_str(pc_decls_dispatchnames[(pci_dispatch_type)-1],NULL);
    msysnewc_m_print_str((byte*)"wc64",NULL);
    msysnewc_m_print_nogap();
    msysnewc_m_print_str((byte*)")",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

static void pc_showhelp(void) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"General usage:",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"\t",NULL);
    msysnewc_m_print_nogap();
    msysnewc_m_print_str(msysnewc_sysparams[((i64)1)-1],NULL);
    msysnewc_m_print_str((byte*)"[options] filename[.pc]",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"\t",NULL);
    msysnewc_m_print_nogap();
    msysnewc_m_print_str(msysnewc_sysparams[((i64)1)-1],NULL);
    msysnewc_m_print_str((byte*)"[options] filename.q",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"\tOptions:",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"\t\t-fn           Function table dispatcher (default)",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"\t\t-asm          Fast ASM dispatcher",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"\t\t-debug        Tracing dispatcher",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"\t\t-fdebug       Tracing dispatcher starts on $setdebug(1)",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    exit((i64)0);
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
    L12 :;
    while (!!((i64)1)) {
        c = (u64)((*msysnewc_fmtstr));
        switch ((i64)(c)) {
        case 35:;
        {
            if (!!(lastx)) {
                goto L15 ;
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
L15 :;
;
            ++n;
            ++msysnewc_fmtstr;
        }
        } //SW
;
L13 :;
    }L14 :;
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
    L16 :;
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
            goto L19 ;
;
        }break;
        default: {
            if ((((u64)(c) >= '0') && ((u64)(c) <= '9'))) {
                n = ((u64)(c) - '0');
                L20 :;
                while (1) {
                    c = (u64)((*s));
                    if (((i64)((*s)) == (i64)0)) {
                        goto L21 ;
                    };
                    if ((((u64)(c) >= '0') && ((u64)(c) <= '9'))) {
                        ++s;
                        n = (((n * (i64)10) + (i64)(c)) - (i64)48);
                    } else {
                        goto L21 ;
                    };
                }L21 :;
                ;
                //gotwidth:
L19 :;
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
L17 :;
    }L18 :;
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
L22 :;
        if (((i64)((*p)) == (i64)0)) {
            goto L24 ;
        };
        (*q) = (u64)((*p));
        ++q;
        ++p;
L23 :;
    }L24 :;
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
        L25 :;
        for (i=(i64)1;i<=(w - n);i+=(i64)1) {
L26 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L27 :;
        }L28 :;
        ;
        (*t) = (u64)0u;
    } else if (((u64)((*fmt).justify) == 'R')) {
        if (((((u64)((*fmt).padchar) == '0') && !!((u64)((*fmt).base))) && (((u64)((*s)) == '-') || ((u64)((*s)) == '+')))) {
            (*t) = (u64)((*s));
            ++t;
            av_2 = (w - n);
            while (av_2-- > 0) {
L29 :;
                (*t) = (u64)((*fmt).padchar);
                ++t;
L30 :;
            }L31 :;
            ;
            strncpy((i8 *)(t),(i8 *)((s + (i64)1)),(u64)((n - (i64)1)));
            (*((t + n) - (i64)1)) = (u64)0u;
        } else {
            av_3 = (w - n);
            while (av_3-- > 0) {
L32 :;
                (*t) = (u64)((*fmt).padchar);
                ++t;
L33 :;
            }L34 :;
            ;
            strncpy((i8 *)(t),(i8 *)(s),(u64)(n));
            (*(t + n)) = (u64)0u;
        };
    } else {
        m = (((w - n) + (i64)1) / (i64)2);
        av_4 = m;
        while (av_4-- > 0) {
L35 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L36 :;
        }L37 :;
        ;
        strncpy((i8 *)(t),(i8 *)(s),(u64)(n));
        t += n;
        av_5 = ((w - n) - m);
        while (av_5-- > 0) {
L38 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L39 :;
        }L40 :;
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
    L41 :;
    do {
        t[(++i)] = (u64)(msysnewc_digits[((i64)((aa % base)))]);
        aa = (aa / base);
        ++k;
        if (((!!(sep) && ((i64)(aa) != (i64)0)) && (k == g))) {
            t[(++i)] = (u64)(sep);
            k = (i64)0;
        };
L42 :;
    } while (!((i64)(aa) == (i64)0));L43 :;
    ;
    j = i;
    s0 = s;
    L44 :;
    while (!!(i)) {
        (*s) = (u64)(t[(i--)]);
        ++s;
L45 :;
    }L46 :;
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
    L47 :;
    while (!!(i)) {
        --s;
        (*s) = (u64)(t[((i-- - (i64)1))]);
        if (((!!(sep) && !!(i)) && (++k == g))) {
            --s;
            (*s) = (u64)(sep);
            k = (i64)0;
        };
L48 :;
    }L49 :;
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
    L50 :;
    while ((((u64)((*s)) == ' ') || ((i64)((*s)) == (i64)9))) {
        ++s;
L51 :;
    }L52 :;
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
    L53 :;
    while (!!((u64)((*s)))) {
        c = (u64)((*s++));
        switch ((i64)(c)) {
        case 32:;
        case 9:;
        case 44:;
        case 61:;
        {
            if ((!!((u64)(quotechar)) || (p == s))) {
                goto L56 ;
;
            };
            msysnewc_termchar = (i64)(c);
            goto L55 ;
        }break;
        default: {
            //normalchar:
L56 :;
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
                    goto L55 ;
                };
            } else {
                (*p) = (u64)(c);
                ++p;
            };
        }
        } //SW
;
L54 :;
    }L55 :;
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
    L57 :;
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
            goto L58 ;
        }break;
        default: {
            msysnewc_itemerror = (i64)1;
            goto L59 ;
        }
        } //SW
;
        if (((i64)(d) >= base)) {
            msysnewc_itemerror = (i64)1;
            goto L59 ;
        };
        aa = (u64)((((i64)(aa) * base) + (i64)(d)));
L58 :;
    }L59 :;
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
L60 :;
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L61 :;
    }L62 :;
    ;
}

static void msysnewc_iconvucn(byte * s,i64 n) {
    i64 av_1;
    av_1 = n;
    while (av_1-- > 0) {
L63 :;
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L64 :;
    }L65 :;
    ;
}

static void msysnewc_convlcstring(byte * s) {
    L66 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L67 :;
    }L68 :;
    ;
}

static void msysnewc_convucstring(byte * s) {
    L69 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L70 :;
    }L71 :;
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
    L72 :;
    for (i=(i64)1;i<=(i64)2048;i+=(i64)1) {
L73 :;
        j = (i64)1;
        k = (i64)16;
        L76 :;
        while ((i > k)) {
            k = (k << (i64)1);
            ++j;
L77 :;
        }L78 :;
        ;
        mlib_sizeindextable[(i)] = (u64)(j);
L74 :;
    }L75 :;
    ;
    mlib_allocupper[((i64)1)] = (u64)((i64)16);
    size = (i64)16;
    L79 :;
    for (i=(i64)2;i<=(i64)27;i+=(i64)1) {
L80 :;
        size *= (i64)2;
        mlib_allocupper[(i)] = (u64)(size);
        if ((size >= (i64)33554432)) {
            k = i;
            goto L82 ;
        };
L81 :;
    }L82 :;
    ;
    L83 :;
    for (i=(k + (i64)1);i<=(i64)300;i+=(i64)1) {
L84 :;
        size += (i64)33554432;
        if ((size < (i64)8589934592)) {
            mlib_allocupper[(i)] = (u64)(size);
            mlib_maxmemory = (u64)(size);
        } else {
            mlib_maxalloccode = (i - (i64)1);
            goto L86 ;
        };
L85 :;
    }L86 :;
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
        L87 :;
        while ((n > m)) {
            m <<= (i64)1;
L88 :;
        }L89 :;
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
    L90 :;
    while (!!(p)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)" ",NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_ptr(p,(byte*)"h");
        msysnewc_m_print_end();
        ;
        p = (u64 *)((i64)((*p)));
L91 :;
    }L92 :;
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
    L93 :;
    for (i=(i64)1;i<=(i64)8;i+=(i64)1) {
L94 :;
        mlib_pcm_printfreelist(m,mlib_freelist[(i)]);
        m <<= (i64)1;
L95 :;
    }L96 :;
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
    L97 :;
    for (i=(i64)1;i<=(i64)500000;i+=(i64)1) {
L98 :;
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
L99 :;
    }L100 :;
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
    L101 :;
    for (i=(i64)1;i<=(i64)500000;i+=(i64)1) {
L102 :;
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
L103 :;
    }L104 :;
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
        L105 :;
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
        }L106 :;
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
    L107 :;
    while (((p >= buffer) && (((i64)((*p)) == (i64)13) || ((i64)((*p)) == (i64)10)))) {
        if ((((i64)((*p)) == (i64)13) || ((i64)((*p)) == (i64)10))) {
            crseen = (u64)((i64)1);
        };
        (*p--) = (u64)0u;
L108 :;
    }L109 :;
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
L110 :;
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L111 :;
    }L112 :;
    ;
}

void mlib_iconvucn(byte * s,i64 n) {
    i64 av_1;
    av_1 = n;
    while (av_1-- > 0) {
L113 :;
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L114 :;
    }L115 :;
    ;
}

void mlib_convlcstring(byte * s) {
    L116 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L117 :;
    }L118 :;
    ;
}

void mlib_convucstring(byte * s) {
    L119 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L120 :;
    }L121 :;
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
    L122 :;
    while ((u >= t)) {
        if (((u64)((*u)) == '.')) {
            if (((i64)((*(u + (i64)1))) == (i64)0)) {
                return (!!(period)?(byte*)".":(byte*)"");
            };
            return (u + (i64)1);
        };
        --u;
L123 :;
    }L124 :;
    ;
    return (byte*)"";
}

byte * mlib_extractpath(byte * s) {
    static byte str[260];
    byte *  t;
    i64 n;
    t = ((s + (i64)(strlen((i8 *)(s)))) - (i64)1);
    L125 :;
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
L126 :;
    }L127 :;
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
    L128 :;
    for (i=(i64)2;i<=(i64)2;i+=(i64)1) {
L129 :;
        p = mlib_freelist[(i)];
        L132 :;
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
L133 :;
        }L134 :;
        ;
L130 :;
    }L131 :;
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
        L135 :;
        for (i=(i64)1;i<=n;i+=(i64)1) {
L136 :;
            str[((slen + i))-1] = (u64)(padch);
L137 :;
        }L138 :;
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
    L139 :;
    for (i=(i64)1;i<=n;i+=(i64)1) {
L140 :;
        str[(i)-1] = (u64)(ch);
L141 :;
    }L142 :;
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
L143 :;
;
    (*value) = (byte *)(0);
    (*name) = (byte *)(0);
    if (!!(infile)) {
        if ((mlib_readnextfileitem(&fileptr,&item) == (i64)0)) {
            free((void *)(filestart));
            infile = (i64)0;
            goto L143 ;
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
            goto L143 ;
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
L144 :;
;
    L145 :;
    while (1) {
        if (((i64)((*p))==(i64)32) || ((i64)((*p))==(i64)9) || ((i64)((*p))==(i64)13) || ((i64)((*p))==(i64)10)) {
            ++p;
        }else if (((i64)((*p))==(i64)26) || ((i64)((*p))==(i64)0)) {
            return (i64)0;
        } else {
            goto L146 ;
        };
    }L146 :;
    ;
    if (((i64)((*p))==(i64)33) || ((i64)((*p))==(i64)35)) {
        ++p;
        L147 :;
        if (((i64)((*p++))==(i64)10)) {
            goto L144 ;
;
        }else if (((i64)((*p++))==(i64)26) || ((i64)((*p++))==(i64)0)) {
            (*fileptr) = (p - (i64)1);
            return (i64)0;
        } else {
        }goto L147 ;
L148 :;
        ;
    };
    if (((i64)((*p))==(i64)34)) {
        pstart = ++p;
        L149 :;
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
                goto L150 ;
            };
            ++p;
        }L150 :;
        ;
    } else {
        pstart = p;
        L151 :;
        while (1) {
            if (((i64)((*p))==(i64)0) || ((i64)((*p))==(i64)26)) {
                pend = p;
                goto L152 ;
            }else if (((i64)((*p))==(i64)32) || ((i64)((*p))==(i64)9) || ((i64)((*p))==(i64)44) || ((i64)((*p))==(i64)13) || ((i64)((*p))==(i64)10)) {
                pend = p++;
                goto L152 ;
            };
            ++p;
        }L152 :;
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
L153 :;
        strcat((i8 *)(s),(i8 *)(padchar));
L154 :;
    }L155 :;
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
    L156 :;
    do {
        x = ((double)(mlib_mrandomp()) / (double)9223372036854775800.);
L157 :;
    } while (!(x != (double)1.));L158 :;
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
            L159 :;
            do {
                count = (i64)0;
                ReadConsoleInputA(oswindows_hconsolein,(void *)(&oswindows_lastkey),(u64)1u,(void *)(&count));
L160 :;
            } while (!(((i64)((u64)(oswindows_lastkey.eventtype)) == (i64)1) && ((i64)((u64)(oswindows_lastkey.keydown)) == (i64)1)));L161 :;
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

i64 pci_runpcl(byte * filename,i64 filetype) {
    byte *  pcfile;
    i64 stopcode;
    byte *  pcfilename;
    if (((pci_dispatch_type == (i64)5) && !(!!(pc_assemc_asmavailable())))) {
        pci_dispatch_type = (i64)2;
    };
    pci_run_initdata();
    if ((filetype==(i64)1)) {
        pcfile = mlib_readfile(filename);
        if ((pcfile == 0)) {
            return (i64)-1;
        };
    }else if ((filetype==(i64)4)) {
        pcfile = (byte *)(filename);
    };
    if (!(!!(pci_loadpcfile(pcfile)))) {
        return (i64)-3;
    };
    if ((filetype == (i64)3)) {
        remove((i8 *)(pcfilename));
    };
    stopcode = pci_runpcprogram();
    return stopcode;
}

void pci_run_initdata(void) {
    static i64 initialised = (i64)0;
    if (!!(initialised)) {
        return;
    };
    if ((pc_decls_ncmdparams == (i64)0)) {
        pc_decls_cmdparamtable[((i64)0)] = msysnewc_sysparams[((i64)1)-1];
    };
    mlib_pcm_init();
    pc_support_inittypetables();
    pci_initpcldata();
    pc_jhandlers_initcalltables();
}

i64 pci_runpcprogram(void) {
    pci_initbytecode();
    pc_decls_varstack = (struct pc_decls_varrec (*)[])(malloc((u64)((i64)1048576)));
    if (!(!!(pc_decls_varstack))) {
        mlib_abortprogram((byte*)"varstack?");
    };
    pc_decls_sptr = &(*pc_decls_varstack)[((i64)65536)];
    --pc_decls_sptr;
    pc_decls_stacklimit = &(*pc_decls_varstack)[((i64)1000)];
    pc_decls_frameptr = (byte *)(pc_decls_sptr);
    pc_decls_pcptr = &(*pc_decls_moduletable[((i64)0)].pccode)[((i64)1)-1];
    pc_decls_pccode = (u64 (*)[])(&(*pc_decls_moduletable[((i64)0)].pccode)[((i64)0)-1]);
    pc_khandlers_stopped = (u64)((i64)0);
    pci_disploop();
    if (((i64)((u64)((*pc_decls_sptr).tagx)) == (i64)0)) {
        return (i64)0;
    } else {
        return (*pc_decls_sptr).value;
    };
}

static void pci_initbytecode(void) {
    oswindows_os_initwindows();
    pci_allocatestrings();
    pci_pclinit();
    pci_pcl_initusertypes();
    pci_fixup_all_pc();
}

static void pci_disploop(void) {
    i64 k;
    if ((pci_dispatch_type==(i64)2)) {
        pci_disploop_fn((i64)0);
    }else if ((pci_dispatch_type==(i64)3) || (pci_dispatch_type==(i64)4)) {
        pci_disploop_deb();
    }else if ((pci_dispatch_type==(i64)5)) {
        k = oswindows_os_clock();
        pc_assemc_disploop_asm();
        k = (oswindows_os_clock() - k);
    };
}

static void pci_pclinit(void) {
    i64 i;
    L162 :;
    for (i=(i64)1;i<=(i64)217;i+=(i64)1) {
L163 :;
        if ((pci_dispatch_type==(i64)2)) {
            pq_common_cmdmap[(i)] = (void *)(pci_disploop_fn(i));
        }else if ((pci_dispatch_type==(i64)3) || (pci_dispatch_type==(i64)4)) {
            pq_common_cmdmap[(i)] = (void *)(i);
        }else if ((pci_dispatch_type==(i64)5)) {
            pq_common_cmdmap[(i)] = (void *)(i);
        };
L164 :;
    }L165 :;
    ;
}

static void pci_fixup_all_pc(void) {
    i64 mx;
    L166 :;
    for (mx=(i64)1;mx<=pc_decls_nmodules;mx+=(i64)1) {
L167 :;
        pci_fixup_module_pc(mx);
L168 :;
    }L169 :;
    ;
    pci_fixup_module_pc((i64)0);
}

static void pci_fixup_module_pc(i64 mx) {
    i64 i;
    i64 cmd;
    i64 m;
    u64 *  p;
    u64 *  pccode;
    struct pc_decls_strec *  d;
    struct pc_decls_strec *  owner;
    static i64 scount = (i64)0;
    struct pc_decls_varrec *  v;
    i64 av_1;
    pccode = (p = (u64 *)(pc_decls_moduletable[(mx)].pccode));
    L170 :;
    while (1) {
        cmd = (i64)((*p));
        if ((mx == (i64)0)) {
            if (((cmd == (i64)73) && (pc_decls_stopseq == 0))) {
                pc_decls_stopseq = (p - (i64)2);
            } else if (((cmd == (i64)208) && (pc_decls_raiseseq == 0))) {
                pc_decls_raiseseq = p;
            };
        };
        (*p++) = (u64)(pq_common_cmdmap[(cmd)]);
        L172 :;
        for (i=(i64)1;i<=pc_decls_cmdnopnds[(cmd)];i+=(i64)1) {
L173 :;
            switch ((i64)(pq_common_cmdfmt[(cmd)][(i)-1])) {
            case 8:;
            {
                (*p) = (u64)(((pccode + (i64)((*p))) - (i64)1));
            }break;
            case 3:;
            {
                d = &(*pc_decls_pcsymboltable)[((i64)((*p)))-1];
                if (((*d).address == 0)) {
                    owner = (*d).owner;
                    L176 :;
                    while (((i64)((u64)((*owner).nameid)) == (i64)4)) {
                        owner = (*owner).owner;
L177 :;
                    }L178 :;
                    ;
                    m = (i64)((*owner).ax_moduleno);
                    (*d).address = (void *)(&(*pc_decls_moduletable[(m)].pccode)[((i64)((*d).index))-1]);
                };
                (*p) = (u64)((*d).address);
            }break;
            case 1:;
            {
                d = &(*pc_decls_pcsymboltable)[((i64)((*p)))-1];
                if (((*d).address == 0)) {
                    v = (struct pc_decls_varrec *)(mlib_pcm_alloc((i64)16));
                    (*v).tagx = (u64)((i64)0);
                    (*d).address = (void *)(v);
                };
                (*p) = (u64)((*d).address);
            }break;
            case 13:;
            {
                (*p) = (u64)(&(*pc_decls_stringobjtable)[((i64)((*p)))]);
            }break;
            default: {
            }
            } //SW
;
            ++p;
L174 :;
        }L175 :;
        ;
        if (((cmd == (i64)4) || (cmd == (i64)0))) {
            goto L171 ;
        };
    }L171 :;
    ;
    if ((pci_dispatch_type == (i64)5)) {
        pc_assemc_fixup_asm(mx);
    };
}

static i64 * pci_disploop_fn(i64 n) {
    i64 count;
    u64 lastticks;
    u64 ticks;
    if (!!(n)) {
        return (i64 *)(pci_handlertable[(n)]);
    };
    count = (i64)1;
    lastticks = (u64)(oswindows_os_clock());
    L179 :;
    do {
        pc_decls_pcptr = ((*(*(u64 * (**)(void))(pc_decls_pcptr))))();
L180 :;
    } while (!!!((u64)(pc_khandlers_stopped)));L181 :;
    ;
    ticks = (u64)((oswindows_os_clock() - (i64)(lastticks)));
    return (i64 *)(0);
}

static void pci_disploop_deb(void) {
    i64 lastcmd;
    i64 cmd;
    i64 index;
    i64 line;
    i64 moduleno;
    i64 count;
    u64 lastticks;
    count = (i64)1;
    lastticks = (u64)(oswindows_os_clock());
    lastcmd = (i64)1;
    L182 :;
    do {
        cmd = (i64)((*pc_decls_pcptr));
        if (((pci_dispatch_type == (i64)3) || !!(mlib_fdebug))) {
            pc_support_findlinenumber(pc_decls_pcptr,&line,&moduleno);
            index = (&(*pc_decls_varstack)[((i64)65536)] - pc_decls_sptr);
            msysnewc_m_print_startcon();
            msysnewc_m_print_ptr(pc_decls_pcptr,NULL);
            msysnewc_m_print_str((byte*)"<",NULL);
            msysnewc_m_print_nogap();
            msysnewc_m_print_str(pq_common_cmdnames[(cmd)],NULL);
            msysnewc_m_print_nogap();
            msysnewc_m_print_str((byte*)">",NULL);
            msysnewc_m_print_str((byte*)"LINE=",NULL);
            msysnewc_m_print_i64(line,NULL);
            msysnewc_m_print_str((byte*)"SPTR=",NULL);
            msysnewc_m_print_ptr(pc_decls_sptr,NULL);
            msysnewc_m_print_str((byte*)"INDEX=",NULL);
            msysnewc_m_print_i64(index,NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            pc_support_showlinenumber();
        };
        pc_decls_pcptr = (u64 *)(((*(i64 * (*)(void))(pci_handlertable[((i64)((*pc_decls_pcptr)))])))());
L183 :;
    } while (!!!((u64)(pc_khandlers_stopped)));L184 :;
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"DEB STOPPED",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

void pci_runproc(void * fnptr,struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * dest) {
    struct pc_decls_varrec *  oldsptr;
    byte *  oldframeptr;
    u64 *  oldpcptr;
    byte oldstopped;
    i64 nparams;
    (*dest).tagx = (u64)((i64)39);
    (*dest).value = (i64)0;
    oldstopped = (u64)(pc_khandlers_stopped);
    oldpcptr = pc_decls_pcptr;
    oldsptr = pc_decls_sptr;
    oldframeptr = pc_decls_frameptr;
    (*--pc_decls_sptr).tagx = (u64)((i64)999);
    if ((!!(b) && !!((u64)((*b).tag)))) {
        nparams = (i64)2;
        (*--pc_decls_sptr) = (*b);
        (*--pc_decls_sptr) = (*a);
    } else if ((!!(a) && !!((u64)((*a).tag)))) {
        nparams = (i64)1;
        (*--pc_decls_sptr) = (*a);
    } else {
        nparams = (i64)0;
    };
    (*--pc_decls_sptr).tagx = (u64)((i64)16);
    (*pc_decls_sptr).uret.retaddr = pc_decls_stopseq;
    (*pc_decls_sptr).uret.frameptr_low = (i64)(*(i32*)&pc_decls_frameptr);
    (*pc_decls_sptr).uret.stackadj = (u64)((nparams * (i64)16));
    pc_decls_frameptr = (byte *)(pc_decls_sptr);
    pc_decls_pcptr = (u64 *)(fnptr);
    pci_disploop();
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == (i64)16)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"RUNPROC: STOP used",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        (*dest) = (*pc_decls_sptr);
    } else {
        ++pc_decls_sptr;
        (*dest) = (*pc_decls_sptr);
        if (((i64)((u64)((*dest).tag)) == (i64)0)) {
            (*dest).tagx = (u64)((i64)39);
            (*dest).value = (i64)0;
        };
    };
    pc_decls_pcptr = oldpcptr;
    pc_khandlers_stopped = (u64)(oldstopped);
    pc_decls_sptr = oldsptr;
    pc_decls_frameptr = oldframeptr;
    pc_khandlers_stopped = (u64)(oldstopped);
}

static void pci_allocatestrings(void) {
    struct pc_decls_objrec *  p;
    i64 i;
    byte *  s;
    pc_decls_stringobjtable = (struct pc_decls_objrec (*)[])(mlib_zalloctable(pc_decls_nstrings,(i64)32));
    L185 :;
    for (i=(i64)1;i<=pc_decls_nstrings;i+=(i64)1) {
L186 :;
        s = (*pc_decls_stringtable)[(i)-1];
        p = &(*pc_decls_stringobjtable)[(i)];
        (*p).refcount = (u64)((i64)5);
        (*p).ustr.strptr = s;
        (*p).ustr.length = (*pc_decls_stringlentable)[(i)-1];
        (*p).ustr.objtype = (u64)((i64)2);
L187 :;
    }L188 :;
    ;
}

byte * pci_compileq(byte * qfilename) {
    mlib_abortprogram((byte*)"PCI:COMPILEQ NEEDS REVISING");
    return (byte*)"";
}

static i64 pci_loadpcfile(byte * s) {
    byte *  str;
    byte *  str2;
    struct pc_decls_modulerec m;
    i64 i;
    i64 j;
    i64 a;
    i64 b;
    i64 dir;
    i64 symtype;
    i64 x;
    i64 id;
    i64 t;
    i64 modno;
    i64 n;
    i64 cmd;
    i64 recordtype;
    i64 length;
    struct pc_decls_strec *  d;
    u16 (*linetable)[];
    u64 *  pccode;
    double xvalue;
    i64 av_1;
    mlib_pcm_clearmem((void *)(&pc_decls_moduletable[((i64)0)]),(i64)110);
    pc_decls_moduletable[((i64)0)].name = (byte*)"PROGRAM";
    pc_decls_moduletable[((i64)0)].filename = (byte*)"<->";
    pc_decls_moduletable[((i64)0)].sourcecode = (byte*)"<program>";
    pc_decls_moduletable[((i64)0)].sourcelen = (i64)(strlen((i8 *)(pc_decls_moduletable[((i64)0)].sourcecode)));
    a = (i64)((*s++));
    b = (i64)((*s++));
    if (((a != (i64)80) || (b != (i64)67))) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"PC: bad sig",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        return (i64)0;
    };
    s += (i64)2;
    L189 :;
    while (1) {
        dir = pc_support_readzint(&s);
        switch (dir) {
        case 1:;
        {
            pc_decls_strpclversion = pc_support_readzstring(&s,(i64 *)(0));
        }break;
        case 2:;
        {
            pc_decls_nmodules = pc_support_readzint(&s);
            L191 :;
            for (i=(i64)1;i<=pc_decls_nmodules;i+=(i64)1) {
L192 :;
                memset((void *)(&m),(i64)0,(u64)((i64)110));
                m.name = mlib_pcm_copyheapstring(pc_support_readzstring(&s,(i64 *)(0)));
                m.filename = m.name;
                m.sourcecode = (byte*)"<no source>";
                m.sourcelen = (i64)0;
                pc_decls_moduletable[(i)] = m;
L193 :;
            }L194 :;
            ;
        }break;
        case 3:;
        {
            pc_decls_ndlltable = pc_support_readzint(&s);
            L195 :;
            for (i=(i64)1;i<=pc_decls_ndlltable;i+=(i64)1) {
L196 :;
                pc_decls_dlltable[(i)-1] = mlib_pcm_copyheapstring(pc_support_readzstring(&s,(i64 *)(0)));
                pc_decls_dllinsttable[(i)-1] = (u64)((i64)0);
L197 :;
            }L198 :;
            ;
        }break;
        case 4:;
        {
            pc_decls_ndllproctable = pc_support_readzint(&s);
            L199 :;
            for (i=(i64)1;i<=pc_decls_ndllproctable;i+=(i64)1) {
L200 :;
                pc_decls_dllproctable[(i)-1].name = mlib_pcm_copyheapstring(pc_support_readzstring(&s,(i64 *)(0)));
                pc_decls_dllproctable[(i)-1].dllindex = pc_support_readzint(&s);
L201 :;
            }L202 :;
            ;
        }break;
        case 14:;
        {
            pc_decls_napplproctable = pc_support_readzint(&s);
            L203 :;
            for (i=(i64)1;i<=pc_decls_napplproctable;i+=(i64)1) {
L204 :;
                pc_decls_applproctable[(i)-1].name = mlib_pcm_copyheapstring(pc_support_readzstring(&s,(i64 *)(0)));
L205 :;
            }L206 :;
            ;
        }break;
        case 5:;
        {
            pc_decls_nsymbols = pc_support_readzint(&s);
            pc_decls_pcsymboltable = (struct pc_decls_strec (*)[])(mlib_zalloctable(pc_decls_nsymbols,(i64)60));
            L207 :;
            for (i=(i64)1;i<=pc_decls_nsymbols;i+=(i64)1) {
L208 :;
                symtype = pc_support_readzint(&s);
                if ((symtype==(i64)80)) {
                    id = (i64)5;
                }else if ((symtype==(i64)83)) {
                    id = (i64)10;
                }else if ((symtype==(i64)77)) {
                    id = (i64)2;
                }else if ((symtype==(i64)84)) {
                    id = (i64)4;
                };
                str = mlib_pcm_copyheapstring(pc_support_readzstring(&s,(i64 *)(0)));
                x = pc_support_readzint(&s);
                a = pc_support_readzint(&s);
                b = pc_support_readzint(&s);
                d = pci_createstentry(i,str,x,id);
                (*d).index = a;
                if ((id==(i64)2)) {
                    (*d).ax_moduleno = (u64)(a);
                }else if ((id==(i64)5)) {
                    str = pc_support_readzstring(&s,(i64 *)(0));
                    if (!!((u64)((*str)))) {
                        (*d).metadata = mlib_pcm_copyheapstring(str);
                    };
                };
L209 :;
            }L210 :;
            ;
        }break;
        case 6:;
        {
            n = pc_support_readzint(&s);
            pc_decls_ntypes = ((n + (i64)53) - (i64)1);
            L211 :;
            for (i=(i64)1;i<=n;i+=(i64)1) {
L212 :;
                t = pc_support_readzint(&s);
                pc_decls_ttname[(t)] = mlib_pcm_copyheapstring(pc_support_readzstring(&s,(i64 *)(0)));
                pc_decls_ttnamedef[(t)] = &(*pc_decls_pcsymboltable)[(pc_support_readzint(&s))-1];
                pc_decls_ttbasetype[(t)] = pc_support_readzint(&s);
                pc_decls_tttarget[(t)] = pc_support_readzint(&s);
                pc_decls_ttlower[(t)] = pc_support_readzint(&s);
                pc_decls_ttlength[(t)] = (u64)(pc_support_readzint(&s));
                pc_decls_ttsize[(t)] = pc_support_readzint(&s);
L213 :;
            }L214 :;
            ;
        }break;
        case 7:;
        {
            pc_decls_ngenfieldnames = pc_support_readzint(&s);
            L215 :;
            for (i=(i64)1;i<=pc_decls_ngenfieldnames;i+=(i64)1) {
L216 :;
                pc_decls_genfieldnames[(i)-1].name = mlib_pcm_copyheapstring(pc_support_readzstring(&s,(i64 *)(0)));
                pc_decls_genfieldnames[(i)-1].dataindex = pc_support_readzint(&s);
                pc_decls_genfieldnames[(i)-1].datalength = pc_support_readzint(&s);
L217 :;
            }L218 :;
            ;
        }break;
        case 8:;
        {
            pc_decls_ngenfielddata = pc_support_readzint(&s);
            if ((pc_decls_ngenfielddata > (i64)1000)) {
                pc_support_loaderror((byte*)"Too many genfields",(byte*)"");
            };
            L219 :;
            for (i=(i64)1;i<=pc_decls_ngenfielddata;i+=(i64)1) {
L220 :;
                pc_decls_genfielddata[(i)-1].fieldindex = pc_support_readzint(&s);
                pc_decls_genfielddata[(i)-1].recordtype = pc_support_readzint(&s);
                pc_decls_genfielddata[(i)-1].fieldtype = pc_support_readzint(&s);
                a = pc_support_readzint(&s);
                if (((i64)(pc_decls_genfielddata[(i)-1].fieldtype) == (i64)18)) {
                    pc_decls_genfieldpcaddress[(i)-1] = pci_getprocaddr(a);
                } else {
                    pc_decls_genfielddata[(i)-1].offset = a;
                };
L221 :;
            }L222 :;
            ;
        }break;
        case 13:;
        {
            pc_decls_nstrings = pc_support_readzint(&s);
            pc_decls_stringtable = (byte * (*)[])(mlib_alloctable(pc_decls_nstrings,(i64)8));
            pc_decls_stringlentable = (i64 (*)[])(mlib_alloctable(pc_decls_nstrings,(i64)8));
            L223 :;
            for (i=(i64)1;i<=pc_decls_nstrings;i+=(i64)1) {
L224 :;
                length = pc_support_readzint(&s);
                str = (byte *)(pc_support_readzblock(&s,length));
                (*pc_decls_stringlentable)[(i)-1] = length;
                str2 = (byte *)(mlib_pcm_alloc((length + (i64)1)));
                memcpy((void *)(str2),(void *)(str),(u64)(length));
                (*(str2 + length)) = (u64)0u;
                (*pc_decls_stringtable)[(i)-1] = str2;
L225 :;
            }L226 :;
            ;
        }break;
        case 10:;
        {
            pc_decls_nstructfields = pc_support_readzint(&s);
            pc_decls_pcfieldtable = (struct pc_decls_fieldrec (*)[])(mlib_zalloctable(pc_decls_nstructfields,(i64)60));
            t = (i64)0;
            n = (i64)0;
            L227 :;
            for (i=(i64)1;i<=pc_decls_nstructfields;i+=(i64)1) {
L228 :;
                recordtype = pc_support_readzint(&s);
                if ((recordtype != t)) {
                    if (!!(t)) {
                        pc_decls_ttstructfields[(t)] = n;
                    };
                    t = recordtype;
                    pc_decls_ttstartfield[(t)] = i;
                    n = (i64)0;
                };
                ++n;
                (*pc_decls_pcfieldtable)[(i)-1].recordtype = recordtype;
                (*pc_decls_pcfieldtable)[(i)-1].name = mlib_pcm_copyheapstring(pc_support_readzstring(&s,(i64 *)(0)));
                (*pc_decls_pcfieldtable)[(i)-1].fieldtype = pc_support_readzint(&s);
                (*pc_decls_pcfieldtable)[(i)-1].fieldoffset = pc_support_readzint(&s);
L229 :;
            }L230 :;
            ;
            if (!!(t)) {
                pc_decls_ttstructfields[(t)] = n;
            };
        }break;
        case 11:;
        {
            modno = pc_support_readzint(&s);
            n = pc_support_readzint(&s);
            pc_decls_moduletable[(modno)].linetable = (linetable = (u16 (*)[])(mlib_zalloctable(n,(i64)2)));
            pc_decls_moduletable[(modno)].pccode = (u64 (*)[])(mlib_zalloctable(n,(i64)8));
            pccode = (u64 *)(pc_decls_moduletable[(modno)].pccode);
            pc_decls_moduletable[(modno)].pcindex = n;
            pc_decls_moduletable[(modno)].npccode = n;
            i = (i64)0;
            L231 :;
            while ((++i <= n)) {
                (*linetable)[(i)] = (u64)(pc_support_readzint(&s));
                cmd = pc_support_readzint(&s);
                (*pccode++) = (u64)(cmd);
                L234 :;
                for (j=(i64)1;j<=pc_decls_cmdnopnds[(cmd)];j+=(i64)1) {
L235 :;
                    ++i;
                    if (((i64)(pq_common_cmdfmt[(cmd)][(j)-1])==(i64)11)) {
                        xvalue = pc_support_readzreal(&s);
                        (*pccode++) = *(u64*)&xvalue;
                    } else {
                        (*pccode++) = (u64)(pc_support_readzint(&s));
                    };
L236 :;
                }L237 :;
                ;
L232 :;
            }L233 :;
            ;
        }break;
        case 12:;
        {
            goto L190 ;
        }break;
        default: {
            pc_support_loaderror((byte*)"PCDIR?",(byte*)"");
        }
        } //SW
;
    }L190 :;
    ;
    return (i64)1;
}

void pci_initpcldata(void) {
    i64 i;
    i64 j;
    i64 nn;
    L238 :;
    for (i=(i64)1;i<=(i64)217;i+=(i64)1) {
L239 :;
        nn = (i64)0;
        L242 :;
        for (j=(i64)1;j<=(i64)4;j+=(i64)1) {
L243 :;
            if (((i64)((u64)(pq_common_cmdfmt[(i)][(j)-1])) == (i64)0)) {
                goto L245 ;
            };
            ++nn;
L244 :;
        }L245 :;
        ;
        pc_decls_cmdnopnds[(i)] = nn;
L240 :;
    }L241 :;
    ;
}

static struct pc_decls_strec * pci_createstentry(i64 index,byte * name,i64 owner,i64 id) {
    struct pc_decls_strec *  p;
    if (!!(index)) {
        p = &(*pc_decls_pcsymboltable)[(index)-1];
    } else {
        p = (struct pc_decls_strec *)(mlib_pcm_allocz((i64)60));
    };
    (*p).name = name;
    (*p).nameid = (u64)(id);
    if (!!(owner)) {
        (*p).owner = &(*pc_decls_pcsymboltable)[(owner)-1];
    } else {
        (*p).owner = pc_decls_stprogram;
    };
    return p;
}

static u64 * pci_getprocaddr(i64 n) {
    struct pc_decls_strec *  d;
    struct pc_decls_strec *  owner;
    i64 m;
    d = &(*pc_decls_pcsymboltable)[(n)-1];
    if (((*d).address == 0)) {
        owner = (*d).owner;
        L246 :;
        while (((i64)((u64)((*owner).nameid)) == (i64)4)) {
            owner = (*owner).owner;
L247 :;
        }L248 :;
        ;
        m = (i64)((*owner).ax_moduleno);
        (*d).address = (void *)(&(*pc_decls_moduletable[(m)].pccode)[((i64)((*d).index))-1]);
    };
    return (u64 *)((*d).address);
}

void pci_pcl_initusertypes(void) {
    i64 t;
    i64 sig;
    i64 basesig;
    L249 :;
    for (t=(i64)53;t<=pc_decls_ntypes;t+=(i64)1) {
L250 :;
        if (((i64)(pc_decls_ttbasetype[(t)])==(i64)32)) {
            pc_decls_free_table[(t)] = (void (*)(struct pc_decls_varrec *))(&pc_pcfns_j_free_m);
            pc_decls_dupl_table[(t)] = (void (*)(struct pc_decls_varrec *))(&pc_pcfns_j_dupl_l_m_d);
            pc_decls_tostr_table[(t)] = (void (*)(struct pc_decls_varrec *,struct pc_decls_varrec *,struct pc_decls_fmtrec *,struct pc_decls_objrec *))(&pc_print_j_tostr_l_m);
            pc_decls_len_table[(t)] = pc_decls_len_table[((i64)32)];
            pc_decls_lwb_table[(t)] = pc_decls_lwb_table[((i64)32)];
            pc_decls_upb_table[(t)] = pc_decls_upb_table[((i64)32)];
            pc_decls_bounds_table[(t)] = pc_decls_bounds_table[((i64)32)];
            basesig = pc_support_gettypesig((i64)32,(i64)1);
            sig = pc_support_gettypesig(t,(i64)1);
            pc_decls_pushix_dtable[(sig)] = pc_decls_pushix_dtable[(basesig)];
            pc_decls_pushdotix_dtable[(sig)] = pc_decls_pushdotix_dtable[(basesig)];
            pc_decls_pushdotixref_dtable[(sig)] = pc_decls_pushdotixref_dtable[(basesig)];
        }else if (((i64)(pc_decls_ttbasetype[(t)])==(i64)33)) {
            pc_decls_free_table[(t)] = pc_decls_free_table[((i64)33)];
            pc_decls_dupl_table[(t)] = pc_decls_dupl_table[((i64)33)];
            pc_decls_tostr_table[(t)] = pc_decls_tostr_table[((i64)33)];
        }else if (((i64)(pc_decls_ttbasetype[(t)])==(i64)30)) {
            pc_decls_free_table[(t)] = pc_decls_free_table[((i64)30)];
            pc_decls_dupl_table[(t)] = pc_decls_dupl_table[((i64)30)];
            pc_decls_tostr_table[(t)] = pc_decls_tostr_table[((i64)30)];
            pc_decls_len_table[(t)] = pc_decls_len_table[((i64)30)];
            pc_decls_lwb_table[(t)] = pc_decls_lwb_table[((i64)30)];
            pc_decls_upb_table[(t)] = pc_decls_upb_table[((i64)30)];
            basesig = pc_support_gettypesig((i64)30,(i64)1);
            sig = pc_support_gettypesig(t,(i64)1);
            pc_decls_pushix_dtable[(sig)] = pc_decls_pushix_dtable[(basesig)];
            pc_decls_pushixref_dtable[(sig)] = pc_decls_pushixref_dtable[(basesig)];
        };
L251 :;
    }L252 :;
    ;
}

void pci_setcmdparam(i64 index,byte * s) {
    if ((s == 0)) {
        pc_decls_ncmdparams = index;
    } else if ((index <= (i64)32)) {
        pc_decls_cmdparamtable[(index)] = mlib_pcm_copyheapstring(s);
        pc_decls_ncmdparams=(pc_decls_ncmdparams>index?pc_decls_ncmdparams:index);
;
    };
}

void pc_support_prterror(byte * mess) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"Print error:",NULL);
    msysnewc_m_print_str(mess,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    oswindows_os_getch();
    exit((i64)1);
}

i64 pc_support_testelem(byte (*p)[],i64 n) {
    return (!!(((u64)((*p)[((n >> (i64)3))]) & (u64)(pc_support_bytemasks[((n & (i64)7))])))?(i64)1:(i64)0);
}

void pc_support_setelem(byte (*p)[],i64 n) {
    (*p)[((n >> (i64)3))] |= pc_support_bytemasks[((n & (i64)7))];
}

void pc_support_pcustype_def(byte * mess,struct pc_decls_varrec * x) {
    i64 t;
    t = (i64)((*x).tag);
    pc_support_showlinenumber();
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"USTYPE:Type not supported: ",NULL);
    msysnewc_m_print_str(mess,NULL);
    msysnewc_m_print_str((byte*)":",NULL);
    msysnewc_m_print_str(pc_decls_ttname[(t)],NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    mlib_abortprogram((byte*)"Stopping");
}

u64 * pc_support_pcustype(byte * mess,struct pc_decls_varrec * x) {
    pc_decls_err_message = mess;
    pc_decls_err_var1 = (*x);
    pc_decls_err_pcptr = pc_decls_pcptr;
    pc_support_pcustype_def(mess,x);
    return pc_decls_pcptr;
}

u64 * pc_support_pcustypet(byte * mess,i64 t) {
    static struct pc_decls_varrec v;
    v.tagx = (u64)(t);
    return pc_support_pcustype(mess,&v);
}

void pc_support_pcmxtypes_def(byte * mess,struct pc_decls_varrec * x,struct pc_decls_varrec * y) {
    i64 s;
    i64 t;
    s = (i64)((*x).tag);
    t = (i64)((*y).tag);
    pc_support_showlinenumber();
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"MXTYPES:Mixed Types not supported:/",NULL);
    msysnewc_m_print_str(mess,NULL);
    msysnewc_m_print_str((byte*)"/:",NULL);
    msysnewc_m_print_str(pc_decls_ttname[(s)],NULL);
    msysnewc_m_print_str((byte*)":",NULL);
    msysnewc_m_print_str(pc_decls_ttname[(t)],NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    mlib_abortprogram((byte*)"Stopping");
}

u64 * pc_support_pcmxtypes(byte * mess,struct pc_decls_varrec * x,struct pc_decls_varrec * y) {
    pc_decls_err_message = mess;
    pc_decls_err_var1 = (*x);
    pc_decls_err_var2 = (*y);
    pc_decls_err_pcptr = pc_decls_pcptr;
    pc_support_pcmxtypes_def(mess,x,y);
    return pc_decls_pcptr;
}

u64 * pc_support_pcmxtypestt(byte * mess,i64 s,i64 t) {
    static struct pc_decls_varrec u;
    static struct pc_decls_varrec v;
    u.tagx = (u64)(s);
    v.tagx = (u64)(t);
    return pc_support_pcmxtypes(mess,&u,&v);
}

byte * pc_support_gettypename(i64 t) {
    return pc_decls_ttname[(t)];
}

void pc_support_inittypetables(void) {
    i64 i;
    i64 size;
    i64 bitsize;
    i64 av_1;
    L253 :;
    for (i=(i64)0;i<=(i64)52;i+=(i64)1) {
L254 :;
        pc_decls_ttname[(i)] = pc_types_stdtypenames[(i)];
        pc_decls_ttbasetype[(i)] = i;
        if ((i==(i64)50) || (i==(i64)51) || (i==(i64)52)) {
            bitsize = (i64)64;
        } else {
            bitsize = pc_types_stdtypewidths[(i)];
        };
        switch (bitsize) {
        case 0:;
        {
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
        pc_decls_ttsize[(i)] = size;
        pc_decls_ttbitwidth[(i)] = bitsize;
        pc_decls_ttlower[(i)] = (i64)1;
L255 :;
    }L256 :;
    ;
    pc_decls_ntypes = (i64)52;
    pc_decls_tttarget[((i64)22)] = (i64)35;
}

u64 * pc_support_pcerror(byte * mess) {
    pc_support_showlinenumber();
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"PCERROR:",NULL);
    msysnewc_m_print_str(mess,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    oswindows_os_getch();
    exit((i64)1);
    return (u64 *)(0);
}

void pc_support_vxunimpl(byte * mess) {
    pc_support_showlinenumber();
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"Unimplemented VX op:",NULL);
    msysnewc_m_print_str(mess,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    oswindows_os_getch();
    exit((i64)1);
}

void pc_support_pclunimpl(i64 cmd) {
    pc_support_showlinenumber();
    if ((cmd != (i64)217)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"Unimplemented cmd:",NULL);
        msysnewc_m_print_str(pq_common_cmdnames[(cmd)],NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    } else {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"J-opcode not allowed with -LAB or -FN",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    };
    mlib_abortprogram((byte*)"Stopping");
}

byte * pc_support_convcstring(byte * svalue,i64 length) {
    static byte strbuffer1[2000];
    static byte strbuffer2[2000];
    static byte strbuffer3[2000];
    static byte strbuffer4[2000];
    static byte strbuffer5[2000];
    static byte strbuffer6[2000];
    static i64 strindex = (i64)0;
    static byte (*table[6])[] = {&strbuffer1,&strbuffer2,&strbuffer3,&strbuffer4,&strbuffer5,&strbuffer6};
    byte (*p)[];
    if ((length >= (i64)2000)) {
        pc_support_pcerror((byte*)"ConvCstring>=2000");
    };
    if ((svalue == 0)) {
        return (byte*)"";
    };
    if ((++strindex == (i64)6)) {
        strindex = (i64)0;
    };
    p = table[(strindex)];
    memcpy((void *)(p),(void *)(svalue),(u64)(length));
    (*p)[(length)] = (u64)0u;
    return (byte *)(p);
}

i64 pc_support_getintvalue(struct pc_decls_varrec * p) {
    switch ((i64)((*p).tag)) {
    case 1:;
    case 13:;
    {
        return (*p).value;
    }break;
    case 3:;
    {
        return (i64)((*p).xvalue);
    }break;
    default: {
        pc_support_pcustype((byte*)"getintvalue",p);
    }
    } //SW
;
    return (i64)0;
}

i64 pc_support_nextpoweroftwo(i64 x) {
    i64 a;
    if ((x == (i64)0)) {
        return (i64)0;
    };
    a = (i64)1;
    L257 :;
    while ((a < x)) {
        a <<= (i64)1;
L258 :;
    }L259 :;
    ;
    return a;
}

void pc_support_showlinenumber(void) {
    i64 lineno;
    i64 moduleno;
    i64 count;
    u64 *  ptr;
    struct pc_decls_varrec *  s;
    struct pc_decls_varrec *  send;
    pc_support_findlinenumber(pc_decls_pcptr,&lineno,&moduleno);
    pc_support_printlinenumber(lineno,moduleno,(byte*)"");
    s = pc_decls_sptr;
    send = &(*pc_decls_varstack)[((i64)65536)];
    count = (i64)0;
    L260 :;
    while (((s <= send) && (count < (i64)15))) {
        if (((i64)((u64)((*s).tag)) == (i64)16)) {
            ptr = ((*s).uret.retaddr - (i64)3);
            pc_support_findlinenumber(ptr,&lineno,&moduleno);
            pc_support_printlinenumber(lineno,moduleno,(byte*)"Called from:");
            ++count;
        };
        ++s;
L261 :;
    }L262 :;
    ;
}

static void pc_support_printlinenumber(i64 lineno,i64 moduleno,byte * calledfrom) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str(calledfrom,NULL);
    msysnewc_m_print_str((byte*)"LINE:",NULL);
    msysnewc_m_print_i64(lineno,NULL);
    msysnewc_m_print_str((byte*)"in FILE:",NULL);
    msysnewc_m_print_str(pc_decls_moduletable[(moduleno)].filename,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

void pc_support_findlinenumber(u64 * ptr,i64 * lineno,i64 * moduleno) {
    i64 pcindex;
    i64 i;
    struct pc_decls_modulerec m;
    (*lineno) = (i64)0;
    pcindex = pc_support_findpcindex(ptr,moduleno);
    if (!!(pcindex)) {
        m = pc_decls_moduletable[((*moduleno))];
        L263 :;
        for (i=pcindex;i>=(i64)1;i-=(i64)1) {
L264 :;
            (*lineno) = (i64)((*m.linetable)[(i)]);
            if (!!((*lineno))) {
                return;
            };
L265 :;
        }L266 :;
        ;
    };
}

i64 pc_support_findpcindex(u64 * ptr,i64 * moduleno) {
    i64 i;
    u64 *  p;
    u64 *  q;
    L267 :;
    for (i=(i64)0;i<=pc_decls_nmodules;i+=(i64)1) {
L268 :;
        p = (u64 *)(pc_decls_moduletable[(i)].pccode);
        q = (p + (i64)(pc_decls_moduletable[(i)].pcindex));
        if (((ptr >= p) && (ptr < q))) {
            (*moduleno) = i;
            return ((ptr - p) + (i64)1);
        };
L269 :;
    }L270 :;
    ;
    return (i64)0;
}

void pc_support_showlinetable(byte * caption,i64 i) {
    i64 j;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"MODULE",NULL);
    msysnewc_m_print_i64(i,NULL);
    msysnewc_m_print_i64(pc_decls_moduletable[(i)].pcindex,NULL);
    msysnewc_m_print_str(caption,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    L271 :;
    for (j=(i64)7;j<=(i64)12;j+=(i64)1) {
L272 :;
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"\tLINE",NULL);
        msysnewc_m_print_i64(j,NULL);
        msysnewc_m_print_u64((*pc_decls_moduletable[(i)].linetable)[(j)],NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
L273 :;
    }L274 :;
    ;
}

void pc_support_writezstring(void * f,byte * s) {
    i64 n;
    i64 av_1;
    mlib_outbyte(f,(i64)254);
    n = (i64)(strlen((i8 *)(s)));
    av_1 = n;
    while (av_1-- > 0) {
L275 :;
        mlib_outbyte(f,(i64)((*s++)));
L276 :;
    }L277 :;
    ;
    mlib_outbyte(f,(i64)0);
}

void pc_support_writezint(void * f,i64 x) {
    byte *  p;
    i64 av_1;
    if (((x >= (i64)0) && (x <= (i64)239))) {
        mlib_outbyte(f,x);
    } else if (((x >= (i64)240) && (x < (i64)480))) {
        mlib_outbyte(f,(i64)245);
        mlib_outbyte(f,(x - (i64)240));
    } else if (((x >= (i64)480) && (x < (i64)720))) {
        mlib_outbyte(f,(i64)246);
        mlib_outbyte(f,(x - (i64)480));
    } else if (((x >= (i64)720) && (x < (i64)960))) {
        mlib_outbyte(f,(i64)247);
        mlib_outbyte(f,(x - (i64)720));
    } else if (((x >= (i64)-127) && (x < (i64)0))) {
        mlib_outbyte(f,(i64)248);
        mlib_outbyte(f,-(x));
    } else if (((x >= (i64)-32768) && (x <= (i64)32767))) {
        mlib_outbyte(f,(i64)249);
        mlib_outword16(f,(u64)(x));
    } else if (((x > (i64)-2147483648) && (x <= (i64)2147483647))) {
        mlib_outbyte(f,(i64)250);
        mlib_outword(f,(u64)(x));
    } else {
        p = (byte *)(&x);
        mlib_outbyte(f,(i64)251);
        av_1 = (i64)8;
        while (av_1-- > 0) {
L278 :;
            mlib_outbyte(f,(i64)((*p++)));
L279 :;
        }L280 :;
        ;
    };
}

void pc_support_writezint4(void * f,i64 x) {
    mlib_outbyte(f,(i64)250);
    mlib_outword(f,(u64)(x));
}

void pc_support_writezrange(void * f,byte * p) {
    i64 av_1;
    mlib_outbyte(f,(i64)251);
    av_1 = (i64)8;
    while (av_1-- > 0) {
L281 :;
        mlib_outbyte(f,(i64)((*p++)));
L282 :;
    }L283 :;
    ;
}

void pc_support_writezreal(void * f,double x) {
    byte *  p;
    i64 *  q;
    i64 av_1;
    i64 av_2;
    p = (byte *)(&x);
    q = (i64 *)(&x);
    if ((q != 0)) {
        mlib_outbyte(f,(i64)253);
        av_1 = (i64)8;
        while (av_1-- > 0) {
L284 :;
            mlib_outbyte(f,(i64)((*p++)));
L285 :;
        }L286 :;
        ;
    } else {
        mlib_outbyte(f,(i64)252);
        p += (i64)4;
        av_2 = (i64)4;
        while (av_2-- > 0) {
L287 :;
            mlib_outbyte(f,(i64)((*p++)));
L288 :;
        }L289 :;
        ;
    };
}

void pc_support_writezeof(void * f) {
    mlib_outbyte(f,(i64)255);
}

static void pc_support_zerror(byte * mess) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"Z error:",NULL);
    msysnewc_m_print_str(mess,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    exit((i64)1);
}

i64 pc_support_readzvalue(byte * * pp,i32 * dest,i32 * dest2) {
    byte *  p;
    i64 *  destint;
    void * *  destptr;
    i64 t;
    byte bb;
    byte c;
    i32 length;
    i32 dummy;
    p = (*pp);
    bb = (u64)((*p++));
    t = (i64)1;
    if ((dest2 == 0)) {
        dest2 = &dummy;
    };
    switch ((i64)(bb)) {
    case 245:;
    {
        (*dest) = ((i64)((u64)((*p++))) + (i64)240);
    }break;
    case 246:;
    {
        (*dest) = ((i64)((u64)((*p++))) + (i64)480);
    }break;
    case 247:;
    {
        (*dest) = ((i64)((u64)((*p++))) + (i64)720);
    }break;
    case 248:;
    {
        (*dest) = -((i64)((*(i8 *)(p))));
        ++p;
    }break;
    case 249:;
    {
        (*dest) = (i64)((*(i16 *)(p)));
        p += (i64)2;
    }break;
    case 250:;
    {
        (*dest) = (i64)((*(i32 *)(p)));
        p += (i64)4;
    }break;
    case 251:;
    {
        destint = (i64 *)(dest);
        (*destint) = (*(i64 *)(p));
        p += (i64)8;
        t = (i64)2;
    }break;
    case 252:;
    {
        (*dest++) = (i64)0;
        (*dest) = (i64)((*(i32 *)(p)));
        p += (i64)4;
        t = (i64)3;
    }break;
    case 253:;
    {
        destint = (i64 *)(dest);
        (*destint) = (*(i64 *)(p));
        p += (i64)8;
        t = (i64)3;
    }break;
    case 254:;
    {
        destptr = (void * *)(dest);
        (*destptr) = (void *)((u64)(p));
        length = (i64)0;
        L290 :;
        do {
            c = (u64)((*p++));
            ++length;
L291 :;
        } while (!!(!!((u64)(c))));L292 :;
        ;
        (*dest2) = (i64)(--length);
        t = (i64)4;
    }break;
    case 244:;
    {
        pc_support_zerror((byte*)"Can't deal with ZBYTES yet");
        exit((i64)1);
    }break;
    case 255:;
    {
        return (i64)0;
    }break;
    default: {
        (*dest) = (i64)(bb);
    }
    } //SW
;
    (*pp) = p;
    return t;
}

i64 pc_support_readzint(byte * * p) {
    i64 aa;
    i64 status;
    aa = (i64)0;
    status = pc_support_readzvalue(p,(i32 *)(&aa),(i32 *)(0));
    if ((status==(i64)1)) {
        if (((i64)((i32)(aa)) < (i64)0)) {
            aa |= (i64)((u64)18446744069414584320u);
        };
    }else if ((status==(i64)2)) {
    } else {
        pc_support_zerror((byte*)"Z:Int32 Expected");
    };
    return aa;
}

i64 pc_support_readzdint(byte * * p) {
    i64 aa;
    i64 status;
    aa = (i64)0;
    if (((status = pc_support_readzvalue(p,(i32 *)(&aa),(i32 *)(0))) != (i64)2)) {
        if ((status == (i64)1)) {
            if ((aa > (i64)2147483647)) {
                aa |= (i64)((u64)18446744071562067968u);
            };
        } else {
            pc_support_zerror((byte*)"ZformatD");
        };
    };
    return aa;
}

double pc_support_readzreal(byte * * p) {
    double x;
    i64 status;
    if (((status = pc_support_readzvalue(p,(i32 *)(&x),(i32 *)(0))) != (i64)3)) {
        pc_support_zerror((byte*)"ZformatR");
    };
    return x;
}

byte * pc_support_readzstring(byte * * p,i64 * ilength) {
    i64 aa;
    i32 length;
    i32 status;
    if (((i64)((status = pc_support_readzvalue(p,(i32 *)(&aa),&length))) != (i64)4)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"STATUS=",NULL);
        msysnewc_m_print_i64(status,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_zerror((byte*)"ZformatS");
    };
    if (!!(ilength)) {
        (*ilength) = (i64)(length);
    };
    return (byte *)(aa);
}

byte * pc_support_readzblock(byte * * pp,i64 length) {
    byte *  pdata;
    pdata = (*pp);
    (*pp) = (pdata + length);
    return pdata;
}

void pc_support_checkmt(i64 id) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"CHECKMT",NULL);
    msysnewc_m_print_i64(id,NULL);
    msysnewc_m_print_str((byte*)":",NULL);
    msysnewc_m_print_ptr(pc_decls_moduletable[((i64)1)].pccode,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

i64 pc_support_ipower(i64 a,i64 n) {
    if ((n <= (i64)0)) {
        return (i64)0;
    } else if ((n == (i64)0)) {
        return (i64)1;
    } else if ((n == (i64)1)) {
        return a;
    } else if (((n & (i64)1) == (i64)0)) {
        return pc_support_ipower((a * a),(n / (i64)2));
    } else {
        return (a * pc_support_ipower((a * a),((n - (i64)1) / (i64)2)));
    };
}

void pc_support_loaderror(byte * mess,byte * mess2) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"Load Error:",NULL);
    msysnewc_m_print_str(mess,NULL);
    msysnewc_m_print_str(mess2,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    exit((i64)1);
}

i64 pc_support_gettypesig(i64 s,i64 t) {
    i64 typesig;
    typesig = (i64)(pc_decls_sigmap[(s)][(t)]);
    if ((typesig == (i64)0)) {
        typesig = ++pc_decls_nexttypesig;
        pc_decls_sigmap[(s)][(t)] = (u64)(typesig);
    };
    return typesig;
}

byte * pc_support_getfnname(void * fnaddr) {
    i64 i;
    i64 n;
    n = msysnewc_m_get_nprocs();
    L293 :;
    for (i=(i64)1;i<=n;i+=(i64)1) {
L294 :;
        if ((msysnewc_m_get_procaddr(i) == fnaddr)) {
            return msysnewc_m_get_procname(i);
        };
L295 :;
    }L296 :;
    ;
    return (byte*)"<FUNCTION NOT FOUND>";
}

void pc_support_junimpl(byte * s) {
    byte mess[100];
    strcpy((i8 *)(mess),(i8 *)((byte*)"J handler unimpl: "));
    strcat((i8 *)(mess),(i8 *)(s));
    pc_support_pcerror(mess);
}

u64 * pc_misc_raiseexception(i64 exceptno) {
    struct pc_decls_varrec *  stackend;
    struct pc_decls_varrec *  oldsptr;
    stackend = &(*pc_decls_varstack)[((i64)65536)];
    oldsptr = pc_decls_sptr;
    L297 :;
    while (1) {
        if ((pc_decls_sptr >= stackend)) {
            pc_decls_sptr = oldsptr;
            pc_misc_default_exception(exceptno);
        };
        if ((((i64)((u64)((*pc_decls_sptr).tag)) == (i64)17) && ((exceptno == (i64)0) || ((i64)((u64)((*pc_decls_sptr).uexcept.exceptiontype)) == exceptno)))) {
            goto L298 ;
        };
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            pc_pcfns_pc_unshare(pc_decls_sptr);
        };
        ++pc_decls_sptr;
    }L298 :;
    ;
    pc_decls_frameptr = ((byte *)(pc_decls_sptr) + (i64)((*pc_decls_sptr).uexcept.frameoffset));
    return (u64 *)((*pc_decls_sptr).refptr);
}

void pc_misc_raise_error(i64 error_no) {
    (*--pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = error_no;
    pc_decls_err_pcptr = pc_decls_pcptr;
    pc_decls_pcptr = pc_decls_raiseseq;
}

static void pc_misc_default_exception(i64 exceptno) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"DEFAULT EXCEPTION HANDLER",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    if ((exceptno==(i64)1)) {
        pc_support_pcerror((byte*)"PC/ERROR");
    }else if ((exceptno==(i64)2)) {
        pc_support_pcerror((byte*)"USER/ERROR");
    }else if ((exceptno==(i64)3)) {
        pc_decls_pcptr = pc_decls_err_pcptr;
        pc_support_pcustype_def(pc_decls_err_message,&pc_decls_err_var1);
    }else if ((exceptno==(i64)4)) {
        pc_decls_pcptr = pc_decls_err_pcptr;
        pc_support_pcmxtypes_def(pc_decls_err_message,&pc_decls_err_var1,&pc_decls_err_var2);
    }else if ((exceptno==(i64)5)) {
        pc_decls_pcptr = pc_decls_err_pcptr;
        pc_support_pcerror((byte*)"EXCEPTION/DIVIDE BY ZERO");
    }else if ((exceptno==(i64)6)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"STOPMODULEERROR",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    }else if ((exceptno==(i64)7)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"BOUNDSERROR",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    } else {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"Exception:",NULL);
        msysnewc_m_print_str(pq_common_errornames[(exceptno)-1],NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    };
    exit((i64)1);
}

void pc_pcfns_pc_unshare(struct pc_decls_varrec * p) {
    if (((i64)((u64)((*(*p).objptr).refcount)) <= (i64)0)) {
        pc_support_pcerror((byte*)"UNSHARE/REF COUNT ERROR");
    };
    if (((i64)((u64)(--(*(*p).objptr).refcount)) == (i64)0)) {
        pc_pcfns_pc_free(p);
    };
}

void pc_pcfns_pc_free(struct pc_decls_varrec * p) {
    struct pc_decls_varrec v;
    struct pc_decls_objrec *  pa;
    pa = (*p).objptr;
    if (((i64)((u64)((*pa).refcount)) > (i64)0)) {
        pc_support_pcerror((byte*)"FREE: REFCOUNT NOT ZERO");
    };
    switch ((i64)((*pa).objtype)) {
    case 0:;
    {
        ((*pc_decls_free_table[((i64)((*pa).tag))]))(p);
    }break;
    case 1:;
    {
        v.tagx = (u64)((*pa).tag);
        v.objptr = (*pa).objptr2;
        pc_pcfns_pc_unshare(&v);
    }break;
    default: {
        pc_objlib_freeobject(pa);
    }
    } //SW
;
    (*p).tag = (u64)((i64)0);
}

struct pc_decls_varrec * pc_pcfns_pc_share(struct pc_decls_varrec * p) {
    ++(*(*p).objptr).refcount;
    return p;
}

void pc_pcfns_pc_dupl(struct pc_decls_varrec * p) {
    struct pc_decls_varrec v;
    if (!!((u64)((*p).hasref))) {
        v = (*p);
        ((*pc_decls_dupl_table[((i64)(v.tag))]))(p);
        pc_pcfns_pc_unshare(&v);
    };
}

void pc_pcfns_j_free_s(struct pc_decls_varrec * p) {
    struct pc_decls_objrec *  pa;
    pa = (*p).objptr;
    if (!!((i64)((*pa).ustr.length))) {
        mlib_pcm_free((void *)((*pa).ustr.strptr),(i64)((*pa).ustr.allocated));
    };
    pc_objlib_freeobject(pa);
}

void pc_pcfns_j_free_m(struct pc_decls_varrec * p) {
    i64 n;
    struct pc_decls_objrec *  r;
    struct pc_decls_varrec *  q;
    i64 av_1;
    r = (*p).objptr;
    n = (i64)(pc_decls_ttlength[((i64)((*p).tag))]);
    q = (*r).urec.vptr;
    av_1 = n;
    while (av_1-- > 0) {
L299 :;
        if (!!((u64)((*q).hasref))) {
            pc_pcfns_pc_unshare(q);
        };
        ++q;
L300 :;
    }L301 :;
    ;
    if (!!(n)) {
        pc_objlib_record_free(r);
    };
    pc_objlib_freeobject(r);
}

void pc_pcfns_j_free_l_d(struct pc_decls_varrec * p) {
    i64 n;
    struct pc_decls_objrec *  r;
    struct pc_decls_varrec *  q;
    i64 av_1;
    r = (*p).objptr;
    n = (i64)((*r).ulist.length);
    q = (*r).ulist.vptr;
    av_1 = n;
    while (av_1-- > 0) {
L302 :;
        if (!!((u64)((*q).hasref))) {
            pc_pcfns_pc_unshare(q);
        };
        ++q;
L303 :;
    }L304 :;
    ;
    if (!!(n)) {
        pc_objlib_list_free(r);
    };
    pc_objlib_freeobject(r);
}

void pc_pcfns_j_free_k(struct pc_decls_varrec * p) {
    struct pc_decls_objrec *  r;
    r = (*p).objptr;
    pc_objlib_array_free(r);
    pc_objlib_freeobject(r);
}

void pc_pcfns_j_free_a(struct pc_decls_varrec * p) {
    struct pc_decls_objrec *  r;
    r = (*p).objptr;
    pc_objlib_array_free(r);
    pc_objlib_freeobject(r);
}

void pc_pcfns_j_free_j(struct pc_decls_varrec * p) {
    pc_bignum_bx_free(p);
}

void pc_pcfns_j_free_b_e(struct pc_decls_varrec * p) {
    struct pc_decls_objrec *  r;
    r = (*p).objptr;
    pc_objlib_bits_free(r);
    pc_objlib_freeobject(r);
}

void pc_pcfns_j_dupl_s(struct pc_decls_varrec * p) {
    struct pc_decls_objrec *  pa;
    pa = (*p).objptr;
    pc_pcfns_pc_makestring((*pa).ustr.strptr,(i64)((*pa).ustr.length),p);
}

void pc_pcfns_j_dupl_l_m_d(struct pc_decls_varrec * p) {
    i64 n;
    struct pc_decls_varrec *  r;
    struct pc_decls_objrec *  oldp;
    struct pc_decls_objrec *  newp;
    i64 av_1;
    oldp = (*p).objptr;
    if (((i64)((u64)((*oldp).refcount)) < (i64)0)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"CIRC",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcerror((byte*)"DUPL/LIST CIRC");
        return;
    };
    if (((i64)(pc_decls_ttbasetype[((i64)((*p).tag))]) == (i64)32)) {
        n = (i64)(pc_decls_ttlength[((i64)((*p).tag))]);
        newp = pc_objlib_record_new((i64)((*p).tag));
    } else {
        n = (i64)((*oldp).ulist.length);
        newp = pc_objlib_list_new(n,(i64)((*oldp).ulist.lower),(struct pc_decls_varrec *)(0));
    };
    (*p).objptr = newp;
    (*oldp).refcount = -((u64)((*oldp).refcount));
    if (!!(n)) {
        r = (*newp).ulist.vptr;
        mlib_pcm_copymem4((void *)(r),(void *)((*oldp).ulist.vptr),(n * (i64)16));
        av_1 = n;
        while (av_1-- > 0) {
L305 :;
            if (!!((u64)((*r).hasref))) {
                if (((i64)(pc_decls_ttbasetype[((i64)((*r).tag))]) != (i64)32)) {
                    ++(*(*r).objptr).refcount;
                    pc_pcfns_pc_dupl(r);
                } else {
                    ++(*(*r).objptr).refcount;
                };
            };
            ++r;
L306 :;
        }L307 :;
        ;
    };
    (*oldp).refcount = -((u64)((*oldp).refcount));
}

void pc_pcfns_j_dupl_a(struct pc_decls_varrec * p) {
    i64 n;
    struct pc_decls_objrec *  oldp;
    struct pc_decls_objrec *  newp;
    oldp = (*p).objptr;
    n = (i64)((*oldp).uarray.length);
    newp = pc_objlib_array_new((i64)((*p).tag),(i64)((*oldp).uarray.elemtag),n,(i64)((*oldp).uarray.lower));
    (*p).objptr = newp;
    if (!!(n)) {
        mlib_pcm_copymem4((void *)((*newp).uarray.ptr),(void *)((*oldp).uarray.ptr),(n * pc_decls_ttsize[((i64)((*oldp).uarray.elemtag))]));
    };
}

void pc_pcfns_j_dupl_j(struct pc_decls_varrec * p) {
    pc_bignum_bx_dupl(p);
}

void pc_pcfns_j_dupl_b(struct pc_decls_varrec * p) {
    i64 n;
    struct pc_decls_objrec *  oldp;
    struct pc_decls_objrec *  newp;
    oldp = (*p).objptr;
    n = (i64)((*oldp).ubits.length);
    newp = pc_objlib_bits_new((i64)((*oldp).ubits.elemtag),n,(i64)((*oldp).ubits.lower));
    (*p).objptr = newp;
    if (!!(n)) {
        mlib_pcm_copymem4((void *)((*newp).ubits.ptr),(void *)((*oldp).ubits.ptr),pc_objlib_bits_bytesize(oldp));
    };
}

void pc_pcfns_j_dupl_e(struct pc_decls_varrec * p) {
    i64 n;
    struct pc_decls_objrec *  oldp;
    struct pc_decls_objrec *  newp;
    oldp = (*p).objptr;
    n = (i64)((*oldp).uset.allocated64);
    newp = pc_objlib_set_new((i64)((*oldp).uset.length),(i64)((*oldp).uset.lower));
    (*p).objptr = newp;
    if (!!(n)) {
        mlib_pcm_copymem4((void *)((*newp).uset.ptr),(void *)((*oldp).uset.ptr),pc_objlib_bits_bytesize(oldp));
    };
}

void pc_pcfns_j_dupl_k(struct pc_decls_varrec * p) {
    i64 nbytes;
    struct pc_decls_objrec *  oldp;
    struct pc_decls_objrec *  newp;
    oldp = (*p).objptr;
    nbytes = pc_decls_ttsize[((i64)((*p).tag))];
    newp = pc_objlib_struct_new((i64)((*p).tag));
    (*p).objptr = newp;
    mlib_pcm_copymem4((void *)((*newp).ustruct.ptr),(void *)((*oldp).ustruct.ptr),nbytes);
}

void pc_pcfns_pc_makelist(i64 n,struct pc_decls_varrec * a,struct pc_decls_varrec * b,i64 lower) {
    struct pc_decls_varrec *  p;
    struct pc_decls_varrec *  q;
    struct pc_decls_objrec *  l;
    i64 av_1;
    a += (n - (i64)1);
    l = pc_objlib_list_new(n,lower,(struct pc_decls_varrec *)(0));
    (*l).ulist.mutable = (u64)((i64)0);
    p = (*l).ulist.vptr;
    q = ((p + (i64)((*l).ulist.allocated)) - (i64)1);
    av_1 = n;
    while (av_1-- > 0) {
L308 :;
        (*p) = (*a--);
        if (!!((u64)((*p).hasref))) {
            pc_pcfns_pc_share(p);
        };
        ++p;
L309 :;
    }L310 :;
    ;
    L311 :;
    while ((p <= q)) {
        (*p).tagx = (u64)((i64)0);
        ++p;
L312 :;
    }L313 :;
    ;
    pc_objlib_objtovar(l,b);
}

void pc_pcfns_pc_makerecord(i64 n,i64 t,struct pc_decls_varrec * a,struct pc_decls_varrec * b) {
    struct pc_decls_varrec *  p;
    struct pc_decls_objrec *  r;
    i64 av_1;
    a += (n - (i64)1);
    r = pc_objlib_record_new(t);
    p = (*r).urec.vptr;
    av_1 = n;
    while (av_1-- > 0) {
L314 :;
        (*p) = (*a--);
        if (!!((u64)((*p).hasref))) {
            pc_pcfns_pc_share(p);
        };
        ++p;
L315 :;
    }L316 :;
    ;
    pc_objlib_objtovar(r,b);
}

void pc_pcfns_pc_makearray(i64 n,i64 arraytype,i64 elemtype,i64 lower,struct pc_decls_varrec * a,struct pc_decls_varrec * b) {
    byte *  p;
    i64 esize;
    i64 basetag;
    struct pc_decls_objrec *  l;
    i64 av_1;
    a += (n - (i64)1);
    if ((elemtype == (i64)0)) {
        elemtype = (i64)((*a).tag);
        basetag = (i64)(pc_decls_ttbasetype[(elemtype)]);
        if ((basetag==(i64)30) || (basetag==(i64)33)) {
        } else {
            if ((basetag > (i64)35)) {
            } else {
                pc_support_pcerror((byte*)"makearray elem");
            };
        };
    };
    l = pc_objlib_array_new(arraytype,elemtype,n,lower);
    p = (*l).uarray.ptr;
    esize = pc_decls_ttsize[(elemtype)];
    av_1 = n;
    while (av_1-- > 0) {
L317 :;
        pc_pcfns_pc_storepacked(p,a,elemtype);
        p += esize;
        --a;
L318 :;
    }L319 :;
    ;
    pc_objlib_objtovar(l,b);
}

void pc_pcfns_pc_makerange(struct pc_decls_varrec * x,struct pc_decls_varrec * y,struct pc_decls_varrec * z) {
    if ((((i64)((u64)((*x).tag)) == (i64)1) && ((i64)((u64)((*y).tag)) == (i64)1))) {
        (*z).tagx = (u64)((i64)4);
        (*z).range_upper = (*y).value;
        (*z).range_lower = (*x).value;
    } else {
        pc_support_pcmxtypes((byte*)"vxmakerange",x,y);
    };
}

void pc_pcfns_pc_makeset(i64 n,struct pc_decls_varrec * data,struct pc_decls_varrec * dest) {
    struct pc_decls_varrec *  q;
    i64 top;
    i64 a;
    i64 b;
    i64 j;
    i64 t;
    struct pc_decls_objrec *  s;
    static i64 count = (i64)0;
    i64 av_1;
    i64 av_2;
    top = (i64)0;
    q = data;
    av_1 = n;
    while (av_1-- > 0) {
L320 :;
        switch ((i64)((*q).tag)) {
        case 4:;
        {
            a = (i64)((*q).range_lower);
            b = (i64)((*q).range_upper);
        }break;
        case 1:;
        {
            a = (*q).value;
            b = a;
        }break;
        default: {
            b = (a = pc_support_getintvalue(q));
        }
        } //SW
;
        if (((a < (i64)0) || (b < (i64)0))) {
            pc_support_pcerror((byte*)"Neg set element");
        };
        if ((a > top)) {
            top = a;
        };
        if ((b > top)) {
            top = b;
        };
        ++q;
L321 :;
    }L322 :;
    ;
    s = pc_objlib_set_new((top + (i64)1),(i64)0);
    q = data;
    av_2 = n;
    while (av_2-- > 0) {
L323 :;
        switch ((i64)((*q).tag)) {
        case 4:;
        {
            a = (i64)((*q).range_lower);
            b = (i64)((*q).range_upper);
            if ((a > b)) {
                t = a;
                a = b;
                b = t;
            };
        }break;
        case 1:;
        {
            b = (a = (*q).value);
        }break;
        default: {
            b = (a = pc_support_getintvalue(q));
        }
        } //SW
;
        L326 :;
        for (j=a;j<=b;j+=(i64)1) {
L327 :;
            pc_support_setelem((byte (*)[])((*s).uset.ptr),j);
L328 :;
        }L329 :;
        ;
        ++q;
L324 :;
    }L325 :;
    ;
    pc_objlib_objtovar(s,dest);
}

void pc_pcfns_pc_makestruct(i64 n,i64 t,struct pc_decls_varrec * a,struct pc_decls_varrec * b) {
    byte *  p;
    i64 i;
    i64 nfields;
    i64 index;
    struct pc_decls_objrec *  l;
    l = pc_objlib_struct_new(t);
    p = (*l).ustruct.ptr;
    index = (i64)(pc_decls_ttstartfield[(t)]);
    nfields = (i64)(pc_decls_ttstructfields[(t)]);
    if ((nfields != n)) {
        pc_support_pcerror((byte*)"makestruct: wrong # fields");
    };
    L330 :;
    for (i=nfields;i>=(i64)1;i-=(i64)1) {
L331 :;
        pc_pcfns_pc_storepacked((p + (i64)((*pc_decls_pcfieldtable)[(((index + i) - (i64)1))-1].fieldoffset)),a,(i64)((*pc_decls_pcfieldtable)[(((index + i) - (i64)1))-1].fieldtype));
        ++a;
L332 :;
    }L333 :;
    ;
    pc_objlib_objtovar(l,b);
}

void pc_pcfns_pc_makedict(i64 n,struct pc_decls_varrec * a,struct pc_decls_varrec * b) {
    struct pc_decls_varrec v;
    struct pc_decls_varrec *  p;
    struct pc_decls_varrec *  q;
    i64 m;
    struct pc_decls_objrec *  l;
    i64 av_1;
    m = (n * (i64)2);
    a += (m - (i64)1);
    l = pc_objlib_dict_new(n);
    v.tagx = (u64)((i64)65546);
    v.objptr = l;
    p = (*l).udict.vptr;
    q = ((p + (i64)((*l).udict.allocated)) - (i64)1);
    av_1 = n;
    while (av_1-- > 0) {
L334 :;
        pc_pcfns_adddictitem(&v,a,(a - (i64)1));
        a -= (i64)2;
L335 :;
    }L336 :;
    ;
    (*b) = v;
}

void pc_pcfns_pc_storepacked(byte * p,struct pc_decls_varrec * q,i64 t) {
    i64 plength;
    i64 qlength;
    i64 s;
    i64 sbase;
    i64 tbase;
    struct pc_decls_objrec *  qa;
    sbase = (i64)(pc_decls_ttbasetype[((s = (i64)((*q).tag)))]);
    tbase = (i64)(pc_decls_ttbasetype[(t)]);
    switch (sbase) {
    case 1:;
    case 2:;
    {
        switch (tbase) {
        case 37:;
        case 44:;
        {
            (*p) = (u64)((*q).value);
            return;
        }break;
        case 38:;
        case 45:;
        {
            (*(u16 *)(p)) = (u64)((*q).value);
            return;
        }break;
        case 39:;
        case 46:;
        {
            (*(i32 *)(p)) = (*q).value;
            return;
        }break;
        case 40:;
        case 47:;
        case 1:;
        case 2:;
        {
            (*(i64 *)(p)) = (*q).value;
            return;
        }break;
        case 48:;
        {
            (*(float *)(p)) = (float)((*q).value);
            return;
        }break;
        case 49:;
        {
            (*(double *)(p)) = (double)((*q).value);
            return;
        }break;
        default: {
        }
        } //SW
;
    }break;
    case 3:;
    {
        switch (tbase) {
        case 39:;
        case 46:;
        {
            (*(i32 *)(p)) = (i64)((*q).xvalue);
            return;
        }break;
        case 48:;
        {
            (*(float *)(p)) = (float)((*q).xvalue);
            return;
        }break;
        case 49:;
        {
            (*(double *)(p)) = (*q).xvalue;
            return;
        }break;
        case 38:;
        case 45:;
        {
            (*(i16 *)(p)) = (i64)((*q).xvalue);
            return;
        }break;
        default: {
        }
        } //SW
;
    }break;
    case 5:;
    {
        qa = (*q).objptr;
        plength = (i64)(pc_decls_ttlength[(t)]);
        qlength = (i64)((*qa).ustr.length);
        switch (tbase) {
        case 5:;
        {
            if ((t == tbase)) {
                if ((qlength != (i64)1)) {
                    pc_support_pcerror((byte*)"Str not len 1");
                };
                (*(byte *)(p)) = (u64)((*(*qa).ustr.strptr));
                return;
            };
            if ((qlength > plength)) {
                qlength = plength;
            };
            memcpy((void *)(p),(void *)((*qa).ustr.strptr),(u64)(qlength));
            pc_pcfns_setfslength((byte *)(p),plength,qlength);
            return;
        }break;
        case 21:;
        {
            if ((qlength >= plength)) {
                memcpy((void *)(p),(void *)((*qa).ustr.strptr),(u64)(plength));
            } else {
                memcpy((void *)(p),(void *)((*qa).ustr.strptr),(u64)(qlength));
                (*(p + qlength)) = (u64)((i64)0);
            };
            return;
        }break;
        default: {
        }
        } //SW
;
    }break;
    case 33:;
    {
        if ((s != t)) {
            pc_support_pcmxtypestt((byte*)"spack struct",s,t);
        };
        memcpy((void *)(p),(void *)((*(*q).objptr).ustruct.ptr),(u64)(pc_decls_ttsize[(t)]));
        return;
    }break;
    case 30:;
    {
        if ((s != t)) {
            pc_support_pcmxtypestt((byte*)"spack array",s,t);
        };
        memcpy((void *)(p),(void *)((*(*q).objptr).uarray.ptr),(u64)(pc_decls_ttsize[(t)]));
        return;
    }break;
    default: {
    }
    } //SW
;
    pc_support_pcmxtypestt((byte*)"storepacked (source->dest)",s,t);
}

static void pc_pcfns_adddictitem(struct pc_decls_varrec * d,struct pc_decls_varrec * p,struct pc_decls_varrec * q) {
    struct pc_decls_objrec *  da;
    struct pc_decls_varrec *  r;
    da = (*d).objptr;
    if (((i64)((u64)((*da).udict.length)) == (i64)0)) {
        pc_support_pcerror((byte*)"NULL DICT");
    };
    r = pc_pcfns_finddictitem(d,p,(i64)1);
    if (!!((u64)((*r).hasref))) {
        pc_pcfns_pc_unshare(r);
    };
    (*r) = (*q);
    if (!!((u64)((*r).hasref))) {
        pc_pcfns_pc_share(r);
    };
}

i64 pc_pcfns_gethashvalue(struct pc_decls_varrec * p) {
    i64 hsum;
    i64 c;
    i64 n;
    i64 result;
    byte *  s;
    i64 av_1;
    switch ((i64)((*p).tag)) {
    case 5:;
    {
        n = (i64)((*(*p).objptr).ustr.length);
        if (!(!!(n))) {
            return (i64)0;
        };
        hsum = (i64)0;
        s = (*(*p).objptr).ustr.strptr;
        av_1 = n;
        while (av_1-- > 0) {
L337 :;
            c = (i64)((*s++));
            hsum = (((hsum << (i64)4) - hsum) + c);
L338 :;
        }L339 :;
        ;
        result = ((hsum << (i64)5) - hsum);
        return (result & (i64)((u64)9223372036854775807u));
    }break;
    case 1:;
    case 2:;
    case 3:;
    case 4:;
    {
        return (*p).value;
    }break;
    default: {
        msysnewc_m_print_startcon();
        msysnewc_m_print_u64((*p).tag,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcustype((byte*)"Can't hash:",p);
    }
    } //SW
;
    return (i64)0;
}

struct pc_decls_varrec * pc_pcfns_finddictitem(struct pc_decls_varrec * vd,struct pc_decls_varrec * p,i64 doins) {
    i64 index;
    i64 size;
    i64 keytag;
    i64 wrapped;
    i64 limit;
    i64 keyvalue;
    struct pc_decls_varrec *  q;
    struct pc_decls_objrec *  pa;
    struct pc_decls_objrec *  qa;
    struct pc_decls_objrec *  d;
    //retry:
L340 :;
;
    d = (*vd).objptr;
    size = ((i64)((u64)((*d).udict.length)) / (i64)2);
    index = (pc_pcfns_gethashvalue(p) & (size - (i64)1));
    q = ((*d).udict.vptr + (index * (i64)2));
    wrapped = (i64)0;
    keytag = (i64)((*p).tag);
    keyvalue = (*p).value;
    pa = (*p).objptr;
    L341 :;
    while (1) {
        if (((i64)((u64)((*q).tag)) == (i64)0)) {
            goto L342 ;
        } else if (((i64)((u64)((*q).tag)) == keytag)) {
            if ((keytag==(i64)1) || (keytag==(i64)3) || (keytag==(i64)2) || (keytag==(i64)4)) {
                if (((*q).value == keyvalue)) {
                    ++q;
                    if (!!((u64)((*q).hasref))) {
                        pc_pcfns_pc_share(q);
                    };
                    return q;
                };
            }else if ((keytag==(i64)5)) {
                qa = (*q).objptr;
                if (((i64)((*pa).ustr.length) == (i64)((*qa).ustr.length))) {
                    if (((i64)(memcmp((void *)((*pa).ustr.strptr),(void *)((*qa).ustr.strptr),(u64)((*pa).ustr.length))) == (i64)0)) {
                        ++q;
                        if (!!((u64)((*q).hasref))) {
                            pc_pcfns_pc_share(q);
                        };
                        return q;
                    };
                };
            };
        };
        ++pc_pcfns_clashes;
        ++index;
        q += (i64)2;
        if ((index >= size)) {
            if (!!(wrapped)) {
                pc_support_pcerror((byte*)"DICT FULL?");
            };
            wrapped = (i64)1;
            index = (i64)0;
            q = (*d).udict.vptr;
        };
    }L342 :;
    ;
    if (!!(doins)) {
        limit = ((size * (i64)3) / (i64)4);
        if (((i64)((u64)((*d).udict.dictitems)) >= limit)) {
            pc_pcfns_expanddict(vd);
            goto L340 ;
;
        };
        (*q) = (*p);
        if (!!((u64)((*q).hasref))) {
            pc_pcfns_pc_share(q);
        };
        ++(*d).udict.dictitems;
        return (q + (i64)1);
    } else {
        return (struct pc_decls_varrec *)(0);
    };
}

static void pc_pcfns_expanddict(struct pc_decls_varrec * vd) {
    i64 n;
    i64 m;
    i64 i;
    struct pc_decls_objrec *  d;
    struct pc_decls_objrec *  e;
    struct pc_decls_varrec *  p;
    struct pc_decls_varrec *  q;
    struct pc_decls_varrec *  r;
    struct pc_decls_varrec ev;
    d = (*vd).objptr;
    n = (i64)((*d).udict.allocated);
    m = (n / (i64)2);
    p = (*d).udict.vptr;
    e = pc_objlib_dict_new((m * (i64)2));
    pc_objlib_objtovar(e,&ev);
    q = p;
    L343 :;
    for (i=(i64)1;i<=m;i+=(i64)1) {
L344 :;
        if (((i64)((u64)((*q).tag)) != (i64)0)) {
            r = pc_pcfns_finddictitem(&ev,q,(i64)1);
            if (!!((u64)((*q).hasref))) {
                pc_pcfns_pc_unshare(q);
            };
            ++q;
            (*r) = (*q++);
        } else {
            q += (i64)2;
        };
L345 :;
    }L346 :;
    ;
    pc_objlib_dict_free(d);
    (*vd).objptr = e;
}

static void pc_pcfns_setfslength(byte * s,i64 m,i64 n) {
    if ((m == n)) {
    } else if ((n == (m - (i64)1))) {
        (*((s + m) - (i64)1)) = (u64)0u;
    } else {
        (*((s + m) - (i64)2)) = (u64)0u;
        (*((s + m) - (i64)1)) = (u64)(n);
    };
}

i64 pc_pcfns_getfslength(byte * s,i64 m) {
    s += (m - (i64)1);
    if (((i64)((*(s - (i64)1))) == (i64)0)) {
        return (i64)((*s));
    } else if (((i64)((*s)) == (i64)0)) {
        return (m - (i64)1);
    } else {
        return m;
    };
}

void pc_pcfns_pc_storeptr(struct pc_decls_varrec * p,struct pc_decls_varrec * q) {
    struct pc_decls_varrec *  dest;
    switch ((i64)((*p).tag)) {
    case 22:;
    {
        dest = (*p).varptr;
        if (!!((u64)((*dest).hasref))) {
            pc_pcfns_pc_unshare(dest);
        };
        if (!!((u64)((*q).hasref))) {
            pc_pcfns_pc_share(q);
        };
        (*dest) = (*q);
    }break;
    case 23:;
    {
        pc_pcfns_pc_storepacked((*p).uref.ptr,q,(i64)((*p).uref.elemtag));
        if (!!((u64)((*q).hasref))) {
            pc_pcfns_pc_unshare(q);
        };
    }break;
    case 24:;
    {
        pc_pcfns_pc_storebit((*p).uref.ptr,(i64)((*p).uref.bitoffset),q,(i64)((*p).uref.elemtag),(i64)((*p).uref.bitlength));
    }break;
    case 29:;
    {
        pc_pcfns_pc_popptrlist(p,q);
        if (!!((u64)((*p).hasref))) {
            pc_pcfns_pc_unshare(p);
        };
        if (!!((u64)((*q).hasref))) {
            pc_pcfns_pc_unshare(q);
        };
    }break;
    default: {
        pc_support_pcustype((byte*)"pc_popptr",p);
    }
    } //SW
;
}

void pc_pcfns_pc_storebit(byte * p,i64 shift,struct pc_decls_varrec * q,i64 t,i64 bitlength) {
    u64 *  pd;
    u64 mask1;
    u64 mask2;
    u64 newvalue;
    if (((i64)((u64)((*q).tag)) != (i64)1)) {
        pc_support_pcerror((byte*)"storebit not int");
    };
    switch (t) {
    case 41:;
    {
        if ((bitlength == (i64)0)) {
            (*p) = (u64)((((i64)((u64)((*p))) & ~(((i64)1 << shift))) | (((*q).value & (i64)1) << shift)));
        } else {
            pd = (u64 *)(p);
            mask1 = (u64)18446744073709551614u;
            if ((bitlength==(i64)1)) {
            }else if ((bitlength==(i64)64)) {
                mask1 = (u64)((i64)0);
            } else {
                mask1 <<= (u64)((bitlength - (i64)1));
            };
            mask1 = ~(mask1);
            if (!!(shift)) {
                mask1 <<= (u64)(shift);
            };
            mask2 = ~(mask1);
            newvalue = (u64)((*q).value);
            if (!!(shift)) {
                newvalue <<= (u64)(shift);
            };
            (*pd) = (((*pd) & mask2) | (newvalue & mask1));
        };
    }break;
    case 42:;
    {
        (*p) = (u64)((((i64)((u64)((*p))) & ~(((i64)3 << shift))) | (((*q).value & (i64)3) << shift)));
    }break;
    case 43:;
    {
        (*p) = (u64)((((i64)((u64)((*p))) & ~(((i64)15 << shift))) | (((*q).value & (i64)15) << shift)));
    }break;
    default: {
        pc_support_pcustypet((byte*)"storebit",t);
    }
    } //SW
;
}

void pc_pcfns_pc_popptrlist(struct pc_decls_varrec * p,struct pc_decls_varrec * q) {
    i64 i;
    i64 nleft;
    i64 nright;
    struct pc_decls_varrec v;
    struct pc_decls_varrec *  pdata;
    struct pc_decls_varrec *  qdata;
    struct pc_decls_objrec *  pp;
    struct pc_decls_objrec *  qq;
    pp = (*p).objptr;
    nleft = (i64)((*pp).ulist.length);
    pdata = (*pp).ulist.vptr;
    v.tagx = (u64)((i64)0);
    switch ((i64)(pc_decls_ttbasetype[((i64)((*q).tag))])) {
    case 29:;
    {
        qq = (*q).objptr;
        nright = (i64)((*qq).ulist.length);
        //dolist:
L347 :;
;
        qdata = (*qq).ulist.vptr;
        L348 :;
        for (i=(i64)1;i<=nleft;i+=(i64)1) {
L349 :;
            if ((i <= nright)) {
                pc_pcfns_pc_storeptr(pdata,qdata++);
            } else {
                pc_pcfns_pc_storeptr(pdata,&v);
            };
            ++pdata;
L350 :;
        }L351 :;
        ;
    }break;
    case 4:;
    {
        L352 :;
        for (i=(i64)1;i<=nleft;i+=(i64)1) {
L353 :;
            if ((i <= (i64)2)) {
                v.tagx = (u64)((i64)1);
                v.value = ((i == (i64)1)?(i64)((*q).range_lower):(i64)((*q).range_upper));
                pc_pcfns_pc_storeptr(pdata,&v);
            } else {
                v.tagx = (u64)((i64)0);
                pc_pcfns_pc_storeptr(pdata,&v);
            };
            ++pdata;
L354 :;
        }L355 :;
        ;
    }break;
    case 32:;
    {
        qq = (*q).objptr;
        nright = (i64)(pc_decls_ttlength[((i64)((*q).tag))]);
        goto L347 ;
;
    }break;
    case 30:;
    {
        pc_support_pcerror((byte*)"POPPTRLIST ARRAY");
    }break;
    default: {
        pc_support_pcustype((byte*)"popptrlist",q);
    }
    } //SW
;
}

void pc_pcfns_pc_loadpacked(void * p,i64 t,struct pc_decls_varrec * dest,struct pc_decls_objrec * ownerobj) {
    i64 length;
    struct pc_decls_objrec *  s;
    byte *  ss;
    i64 av_1;
    switch ((i64)(pc_decls_ttbasetype[(t)])) {
    case 37:;
    {
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = (i64)((*(i8 *)(p)));
    }break;
    case 38:;
    {
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = (i64)((*(i16 *)(p)));
    }break;
    case 39:;
    {
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = (i64)((*(i32 *)(p)));
    }break;
    case 40:;
    case 1:;
    {
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = (*(i64 *)(p));
    }break;
    case 44:;
    {
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = (i64)((*(byte *)(p)));
    }break;
    case 45:;
    {
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = (i64)((*(u16 *)(p)));
    }break;
    case 46:;
    {
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = (i64)((*(u32 *)(p)));
    }break;
    case 47:;
    {
        (*dest).tagx = (u64)((i64)2);
        (*dest).value = (i64)((*(u32 *)(p)));
    }break;
    case 49:;
    {
        (*dest).tagx = (u64)((i64)3);
        (*dest).xvalue = (*(double *)(p));
    }break;
    case 48:;
    {
        (*dest).tagx = (u64)((i64)3);
        (*dest).xvalue = (double)((*(float *)(p)));
    }break;
    case 5:;
    {
        (*dest).tagx = (u64)((i64)65541);
        length = (i64)(pc_decls_ttlength[(t)]);
        if ((length >= (i64)2)) {
            length = pc_pcfns_getfslength((byte *)(p),length);
        } else {
            length = (i64)1;
        };
        s = pc_objlib_make_strslicexobj((byte *)(p),length);
        (*dest).objptr = s;
    }break;
    case 21:;
    {
        (*dest).tagx = (u64)((i64)65541);
        ss = (byte *)(p);
        av_1 = (i64)(pc_decls_ttlength[(t)]);
        while (av_1-- > 0) {
L356 :;
            if (((i64)((*ss)) == (i64)0)) {
                goto L358 ;
            };
            ++ss;
L357 :;
        }L358 :;
        ;
        s = pc_objlib_make_strslicexobj((byte *)(p),(ss - (byte *)(p)));
        (*dest).objptr = s;
    }break;
    case 23:;
    {
        (*dest).tagx = (u64)((i64)23);
        (*dest).uref.ptr = (byte *)((*(i32 * *)(p)));
        (*dest).uref.elemtag = (u64)(pc_decls_tttarget[(t)]);
    }break;
    case 33:;
    {
        s = pc_objlib_obj_new(t);
        (*s).ustruct.mutable = (u64)((i64)1);
        (*s).ustruct.ptr = (byte *)(p);
        //dostruct:
L359 :;
;
        (*dest).objptr = s;
        (*dest).tagx = (u64)((t | (i64)65536));
        if (!!(ownerobj)) {
            (*s).objtype = (u64)((i64)1);
            (*s).ustruct.objptr2 = ownerobj;
            ++(*ownerobj).refcount;
        } else {
            (*s).objtype = (u64)((i64)2);
        };
    }break;
    case 30:;
    {
        s = pc_objlib_array_new(t,(i64)(pc_decls_tttarget[(t)]),(i64)(pc_decls_ttlength[(t)]),(i64)(pc_decls_ttlower[(t)]));
        (*s).uarray.mutable = (u64)((i64)1);
        (*s).uarray.lower = (i64)(pc_decls_ttlower[(t)]);
        (*s).uarray.ptr = (byte *)(p);
        (*s).uarray.length = (u64)(pc_decls_ttlength[(t)]);
        (*s).uarray.elemtag = (i64)(pc_decls_tttarget[(t)]);
        goto L359 ;
;
    }break;
    default: {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"T=",NULL);
        msysnewc_m_print_i64(t,NULL);
        msysnewc_m_print_i64(pc_decls_ttbasetype[(t)],NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcmxtypestt((byte*)"loadpacked",t,(i64)(pc_decls_ttbasetype[(t)]));
    }
    } //SW
;
}

void pc_pcfns_pc_loadbit(byte * p,i64 shift,i64 t,i64 bitlength,struct pc_decls_varrec * dest) {
    u64 *  pd;
    u64 mask;
    (*dest).tagx = (u64)((i64)1);
    switch (t) {
    case 41:;
    {
        if ((bitlength == (i64)0)) {
            (*dest).value = !!(((i64)((u64)((*p))) & ((i64)1 << shift)));
        } else {
            pd = (u64 *)(p);
            mask = (u64)18446744073709551614u;
            msysnewc_m_print_startcon();
            msysnewc_m_print_str((byte*)"PD=",NULL);
            msysnewc_m_print_u64((*pd),(byte*)"Z64BS,");
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            msysnewc_m_print_startcon();
            msysnewc_m_print_str((byte*)"MASK=",NULL);
            msysnewc_m_print_u64(mask,NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            msysnewc_m_print_startcon();
            msysnewc_m_print_str((byte*)"SHIFT=",NULL);
            msysnewc_m_print_i64(shift,NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            msysnewc_m_print_startcon();
            msysnewc_m_print_str((byte*)"BITLENGTH=",NULL);
            msysnewc_m_print_i64(bitlength,NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            if ((bitlength==(i64)1)) {
            }else if ((bitlength==(i64)64)) {
                mask = (u64)((i64)0);
            } else {
                mask <<= (u64)((bitlength - (i64)1));
            };
            msysnewc_m_print_startcon();
            msysnewc_m_print_str((byte*)"MASK",NULL);
            msysnewc_m_print_u64(mask,(byte*)"Z64BS,");
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            msysnewc_m_print_startcon();
            msysnewc_m_print_str((byte*)"INOT MASK",NULL);
            msysnewc_m_print_u64(~(mask),(byte*)"Z64BS,");
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            (*dest).value = (i64)((((*pd) >> shift) & ~(mask)));
        };
    }break;
    case 42:;
    {
        (*dest).value = (((i64)((u64)((*p))) & ((i64)3 << shift)) >> shift);
    }break;
    case 43:;
    {
        (*dest).value = (((i64)((u64)((*p))) & ((i64)15 << shift)) >> shift);
    }break;
    default: {
        pc_support_pcustypet((byte*)"loadbit",t);
    }
    } //SW
;
}

void pc_pcfns_pc_loadptr(struct pc_decls_varrec * x,struct pc_decls_varrec * y) {
    switch ((i64)((*x).tag)) {
    case 22:;
    {
        (*y) = (*(*x).varptr);
        if (!!((u64)((*y).hasref))) {
            ++(*(*y).objptr).refcount;
        };
    }break;
    case 23:;
    {
        pc_pcfns_pc_loadpacked((void *)((*x).uref.ptr),(i64)((*x).uref.elemtag),y,(struct pc_decls_objrec *)(0));
    }break;
    default: {
        pc_support_pcustype((byte*)"pc_loadptr",x);
    }
    } //SW
;
}

void pc_pcfns_pc_storestring(struct pc_decls_varrec * p,struct pc_decls_varrec * q) {
    struct pc_decls_objrec *  pp;
    struct pc_decls_objrec *  qq;
    pp = (*p).objptr;
    qq = (*q).objptr;
    if (((i64)((u64)((*pp).objtype)) == (i64)0)) {
        pc_support_pcerror((byte*)"popstr not slice");
    };
    if (((i64)((u64)((*q).tag)) != (i64)5)) {
        pc_support_pcerror((byte*)"popstr not str");
    };
    if (((i64)((*pp).ustr.length) != (i64)((*qq).ustr.length))) {
        pc_support_pcerror((byte*)"popstr diff lengths");
    };
    if (!(!!((u64)((*pp).ustr.mutable)))) {
        pc_support_pcerror((byte*)"popstr not mut");
    };
    if (!!((i64)((*pp).ustr.length))) {
        memcpy((void *)((*pp).ustr.strptr),(void *)((*qq).ustr.strptr),(u64)((*pp).ustr.length));
    };
    pc_pcfns_pc_unshare(p);
    pc_pcfns_pc_unshare(q);
}

void pc_pcfns_pc_iconvert(i64 t,struct pc_decls_varrec * x) {
    i64 s;
    i64 tbase;
    i64 aa;
    struct pc_decls_varrec bn;
    s = (i64)((*x).tag);
    if (((s == t) && (s < (i64)29))) {
        return;
    };
    tbase = (i64)(pc_decls_ttbasetype[(t)]);
    (*x).tag = (u64)(t);
    switch ((i64)(pc_decls_ttbasetype[(s)])) {
    case 1:;
    {
        switch (tbase) {
        case 1:;
        {
        }break;
        case 3:;
        {
            (*x).xvalue = (double)((*x).value);
        }break;
        case 2:;
        {
        }break;
        case 7:;
        {
            pc_bignum_bx_makeint((*pc_decls_sptr).value,pc_decls_sptr);
        }break;
        default: {
            pc_support_pcustypet((byte*)"conv dint=>",t);
        }
        } //SW
;
    }break;
    case 2:;
    {
        switch (tbase) {
        case 1:;
        {
        }break;
        case 2:;
        {
        }break;
        case 3:;
        {
        }break;
        default: {
            pc_support_pcustypet((byte*)"conv dint=>",t);
        }
        } //SW
;
    }break;
    case 3:;
    {
        switch (tbase) {
        case 1:;
        {
            (*x).value = (i64)((*x).xvalue);
        }break;
        default: {
            pc_support_pcustypet((byte*)"conv real=>",t);
        }
        } //SW
;
    }break;
    case 23:;
    case 22:;
    case 24:;
    case 18:;
    {
        switch (tbase) {
        case 1:;
        case 2:;
        {
        }break;
        default: {
            pc_support_pcustypet((byte*)"conv ptr=>",t);
        }
        } //SW
;
    }break;
    case 5:;
    {
        switch (tbase) {
        case 7:;
        {
            pc_bignum_bx_makestr((*(*x).objptr).ustr.strptr,(i64)((*(*x).objptr).ustr.length),&bn);
            (*x).tagx = (u64)((i64)65541);
            pc_pcfns_pc_unshare(x);
            (*x) = bn;
        }break;
        case 5:;
        {
        }break;
        default: {
            pc_support_pcustypet((byte*)"string=>",t);
        }
        } //SW
;
    }break;
    case 13:;
    {
        if ((tbase != (i64)1)) {
            pc_support_pcustypet((byte*)"type=>",t);
        };
    }break;
    case 7:;
    {
        switch (tbase) {
        case 1:;
        {
            aa = pc_bignum_bx_int(x);
            (*x).tagx = (u64)((i64)65543);
            pc_pcfns_pc_unshare(x);
            (*x).tagx = (u64)((i64)1);
            (*x).value = aa;
            (*x).tagx = (u64)(t);
        }break;
        default: {
            pc_support_pcustypet((byte*)"bignum=>",t);
        }
        } //SW
;
    }break;
    default: {
        pc_support_pcmxtypestt((byte*)"HARDCONV s^.t",s,t);
    }
    } //SW
;
}

void pc_pcfns_pc_iconvcase(struct pc_decls_varrec * a,struct pc_decls_varrec * b,i64 upper) {
    i64 n;
    byte *  s;
    struct pc_decls_objrec *  pa;
    i64 av_1;
    i64 av_2;
    pa = (*a).objptr;
    if (((i64)((u64)((*b).tag)) > (i64)0)) {
        n = pc_support_getintvalue(b);
    } else {
        n = (i64)((*pa).ustr.length);
    };
    if (((i64)((u64)((*a).tag)) != (i64)5)) {
        pc_support_pcerror((byte*)"convcase/notstr");
    };
    if ((n < (i64)0)) {
        pc_support_pcerror((byte*)"CONVCASE N<0");
    };
    if ((n == (i64)0)) {
        return;
    };
    if ((n > (i64)((*pa).ustr.length))) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"N=",NULL);
        msysnewc_m_print_i64(n,NULL);
        msysnewc_m_print_i64((*pa).ustr.length,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcerror((byte*)"convcase/N?");
    };
    s = (*pa).ustr.strptr;
    if (!!(upper)) {
        av_1 = n;
        while (av_1-- > 0) {
L360 :;
            (*s) = (u64)(toupper((i64)((i32)((*s)))));
            ++s;
L361 :;
        }L362 :;
        ;
    } else {
        av_2 = n;
        while (av_2-- > 0) {
L363 :;
            (*s) = (u64)(tolower((i64)((i32)((*s)))));
            ++s;
L364 :;
        }L365 :;
        ;
    };
}

i64 pc_pcfns_pc_eqstring_nf(struct pc_decls_varrec * x,struct pc_decls_varrec * y) {
    i64 n;
    struct pc_decls_objrec *  px;
    struct pc_decls_objrec *  py;
    px = (*x).objptr;
    py = (*y).objptr;
    n = (i64)((*px).ustr.length);
    if ((n != (i64)((*py).ustr.length))) {
        return (i64)0;
    };
    if ((n == (i64)0)) {
        return (i64)1;
    };
    return (mlib_cmpstringn((*px).ustr.strptr,(*py).ustr.strptr,n) == (i64)0);
}

i64 pc_pcfns_pc_equal_nf(struct pc_decls_varrec * x,struct pc_decls_varrec * y,i64 shallow) {
    i64 xt;
    i64 yt;
    i64 xbase;
    i64 ybase;
    i64 nbytes;
    i64 n;
    struct pc_decls_varrec *  p;
    struct pc_decls_varrec *  q;
    struct pc_decls_objrec *  px;
    struct pc_decls_objrec *  py;
    i64 av_1;
    i64 av_2;
    xbase = (i64)(pc_decls_ttbasetype[((xt = (i64)((*x).tag)))]);
    ybase = (i64)(pc_decls_ttbasetype[((yt = (i64)((*y).tag)))]);
    if ((ybase == (i64)0)) {
        pc_support_pcerror((byte*)"pcequal/void");
    };
    px = (*x).objptr;
    py = (*y).objptr;
    switch (xbase) {
    case 1:;
    {
        switch (ybase) {
        case 1:;
        case 2:;
        {
            return (((*x).value == (*y).value)?(i64)1:(i64)0);
        }break;
        case 3:;
        {
            return (((double)((*x).value) == (*y).xvalue)?(i64)1:(i64)0);
        }break;
        default: {
        }
        } //SW
;
    }break;
    case 2:;
    {
        switch (ybase) {
        case 46:;
        {
            return (((*x).uvalue == (*y).uvalue)?(i64)1:(i64)0);
        }break;
        default: {
        }
        } //SW
;
    }break;
    case 3:;
    {
        switch (ybase) {
        case 1:;
        {
            return (((*x).xvalue == (double)((*y).value))?(i64)1:(i64)0);
        }break;
        case 3:;
        {
            return (((*x).xvalue == (*y).xvalue)?(i64)1:(i64)0);
        }break;
        default: {
        }
        } //SW
;
    }break;
    case 4:;
    {
        if ((ybase == (i64)4)) {
            return (((*x).value == (*y).value)?(i64)1:(i64)0);
        };
    }break;
    case 22:;
    {
        switch (ybase) {
        case 22:;
        case 1:;
        {
            return ((*x).value == (*y).value);
        }break;
        default: {
        }
        } //SW
;
    }break;
    case 23:;
    {
        switch (ybase) {
        case 23:;
        case 1:;
        {
            return ((*x).value == (*y).value);
        }break;
        default: {
        }
        } //SW
;
    }break;
    case 18:;
    {
        switch (ybase) {
        case 18:;
        case 1:;
        {
            return ((*x).value == (*y).value);
        }break;
        default: {
        }
        } //SW
;
    }break;
    case 29:;
    {
        if ((ybase == (i64)29)) {
            if (!!(shallow)) {
                return (px == py);
            };
            if (((u64)((*px).ulist.length) != (u64)((*py).ulist.length))) {
                return (i64)0;
            };
            p = (*px).ulist.vptr;
            q = (*py).ulist.vptr;
            av_1 = (i64)((*px).ulist.length);
            while (av_1-- > 0) {
L366 :;
                if ((pc_pcfns_pc_equal_nf(p++,q++,shallow) == (i64)0)) {
                    return (i64)0;
                };
L367 :;
            }L368 :;
            ;
            return (i64)1;
        };
    }break;
    case 5:;
    {
        switch (ybase) {
        case 5:;
        {
            return pc_pcfns_pc_eqstring_nf(x,y);
        }break;
        default: {
        }
        } //SW
;
    }break;
    case 33:;
    {
        if ((xt != yt)) {
            return (i64)0;
        };
        return pc_pcfns_comparebytes((*px).ustruct.ptr,(*py).ustruct.ptr,pc_decls_ttsize[(xt)]);
    }break;
    case 9:;
    {
        if ((ybase != (i64)9)) {
            return (i64)0;
        };
        if (((u64)((*px).uset.length) != (u64)((*py).uset.length))) {
            return (i64)0;
        };
        nbytes = ((((i64)((u64)((*px).uset.length)) - (i64)1) / (i64)64) + (i64)1);
        return pc_pcfns_comparebytes((*px).uset.ptr,(*py).uset.ptr,nbytes);
    }break;
    case 0:;
    {
        pc_support_pcerror((byte*)"Comparing void types");
    }break;
    case 7:;
    {
        if ((ybase != (i64)7)) {
            return (i64)0;
        };
        return (pc_bignum_bx_equal(x,y) == (i64)1);
    }break;
    case 32:;
    {
        if ((xt != yt)) {
            return (i64)0;
        };
        if (!!(shallow)) {
            return (px == py);
        };
        p = (*px).urec.vptr;
        q = (*py).urec.vptr;
        n = (i64)(pc_decls_ttlength[(xt)]);
        av_2 = n;
        while (av_2-- > 0) {
L369 :;
            if ((pc_pcfns_pc_equal_nf(p++,q++,shallow) == (i64)0)) {
                return (i64)0;
            };
L370 :;
        }L371 :;
        ;
        return (i64)1;
    }break;
    case 30:;
    {
        if ((((xt != yt) || (xbase != ybase)) || ((i64)((*px).uarray.elemtag) != (i64)((*py).uarray.elemtag)))) {
            return (i64)0;
        };
        if (((u64)((*px).uarray.length) != (u64)((*py).uarray.length))) {
            return (i64)0;
        };
        return pc_pcfns_comparebytes((*px).uarray.ptr,(*py).uarray.ptr,((i64)((u64)((*px).uarray.length)) * pc_decls_ttsize[((i64)((*px).uarray.elemtag))]));
    }break;
    case 13:;
    {
        if (((ybase == (i64)13) && ((*x).value == (*y).value))) {
            return (i64)1;
        };
    }break;
    default: {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"DIFF TYPES",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        return (i64)0;
    }
    } //SW
;
    return (i64)0;
}

i64 pc_pcfns_comparebytes(byte * p,byte * q,i64 n) {
    return ((i64)(memcmp((void *)(p),(void *)(q),(u64)(n))) == (i64)0);
}

i64 pc_pcfns_pc_compare_nf(struct pc_decls_varrec * x,struct pc_decls_varrec * y) {
    i64 xt;
    i64 yt;
    i64 xbase;
    i64 ybase;
    struct pc_decls_objrec *  px;
    struct pc_decls_objrec *  py;
    ybase = (i64)(pc_decls_ttbasetype[((yt = (i64)((*y).tag)))]);
    xbase = (i64)(pc_decls_ttbasetype[((xt = (i64)((*x).tag)))]);
    switch (xbase) {
    case 1:;
    {
        switch (ybase) {
        case 1:;
        {
            return (((*x).value < (*y).value)?(i64)-1:(((*x).value > (*y).value)?(i64)1:(i64)0));
        }break;
        case 3:;
        {
            return (((double)((*x).value) < (*y).xvalue)?(i64)-1:(((double)((*x).value) > (*y).xvalue)?(i64)1:(i64)0));
        }break;
        default: {
            goto L372 ;
;
        }
        } //SW
;
    }break;
    case 2:;
    {
        switch (ybase) {
        case 2:;
        {
            return (((*x).uvalue < (*y).uvalue)?(i64)-1:(((*x).uvalue > (*y).uvalue)?(i64)1:(i64)0));
        }break;
        default: {
            goto L372 ;
;
        }
        } //SW
;
    }break;
    case 3:;
    {
        switch (ybase) {
        case 1:;
        {
            return (((*x).xvalue < (double)((*y).value))?(i64)-1:(((*x).xvalue > (double)((*y).value))?(i64)1:(i64)0));
        }break;
        case 3:;
        {
            return (((*x).xvalue < (*y).xvalue)?(i64)-1:(((*x).xvalue > (*y).xvalue)?(i64)1:(i64)0));
        }break;
        default: {
            goto L372 ;
;
        }
        } //SW
;
    }break;
    case 23:;
    {
        switch (ybase) {
        case 23:;
        case 1:;
        {
            return (((*x).value < (*y).value)?(i64)-1:(((*x).value > (*y).value)?(i64)1:(i64)0));
        }break;
        default: {
            goto L372 ;
;
        }
        } //SW
;
    }break;
    case 22:;
    {
        switch (ybase) {
        case 22:;
        case 1:;
        {
            return (((*x).value < (*y).value)?(i64)-1:(((*x).value > (*y).value)?(i64)1:(i64)0));
        }break;
        default: {
            goto L372 ;
;
        }
        } //SW
;
    }break;
    case 5:;
    {
        switch (ybase) {
        case 5:;
        {
            px = (*x).objptr;
            py = (*y).objptr;
            return pc_pcfns_cmpstring_len((*px).ustr.strptr,(*py).ustr.strptr,(i64)((*px).ustr.length),(i64)((*py).ustr.length));
        }break;
        default: {
            goto L372 ;
;
        }
        } //SW
;
    }break;
    case 7:;
    {
        if ((ybase == (i64)7)) {
            return pc_bignum_bx_cmp(x,y);
        } else {
            goto L372 ;
;
        };
    }break;
    default: {
        //badcmp:
L372 :;
;
        pc_support_pcmxtypes((byte*)"pc_compare",x,y);
    }
    } //SW
;
    return (i64)0;
}

i64 pc_pcfns_cmpstring_len(byte * s,byte * t,i64 slen,i64 tlen) {
    if ((slen == (i64)0)) {
        if ((tlen == (i64)0)) {
            return (i64)0;
        } else {
            return (i64)-1;
        };
    } else if ((tlen == (i64)0)) {
        return (i64)1;
    } else {
        if ((slen == tlen)) {
            if ((slen == (i64)1)) {
                if (((u64)((*s)) < (u64)((*t)))) {
                    return (i64)-1;
                } else if (((u64)((*s)) > (u64)((*t)))) {
                    return (i64)1;
                } else {
                    return (i64)0;
                };
            };
            return mlib_cmpstringn(s,t,slen);
        } else {
            return mlib_cmpstring(pc_support_convcstring(s,slen),pc_support_convcstring(t,tlen));
        };
    };
}

i64 pc_pcfns_pc_eqstring(struct pc_decls_varrec * x,struct pc_decls_varrec * y) {
    struct pc_decls_objrec *  px;
    struct pc_decls_objrec *  py;
    i64 res;
    res = pc_pcfns_pc_eqstring_nf(x,y);
    px = (*x).objptr;
    py = (*y).objptr;
    if (!!((u64)((*x).hasref))) {
        pc_pcfns_pc_unshare(x);
    };
    if (!!((u64)((*y).hasref))) {
        pc_pcfns_pc_unshare(y);
    };
    return res;
}

i64 pc_pcfns_pc_equal(struct pc_decls_varrec * x,struct pc_decls_varrec * y,i64 shallow) {
    i64 res;
    if ((((u64)((*x).tag) != (u64)((*y).tag)) && (((i64)((u64)((*x).tag)) == (i64)7) || ((i64)((u64)((*y).tag)) == (i64)7)))) {
        pc_support_pcerror((byte*)"pcequal/mixed bignum");
    };
    res = pc_pcfns_pc_equal_nf(x,y,shallow);
    if (!!((u64)((*x).hasref))) {
        pc_pcfns_pc_unshare(x);
    };
    if (!!((u64)((*y).hasref))) {
        pc_pcfns_pc_unshare(y);
    };
    return res;
}

i64 pc_pcfns_pc_compare(struct pc_decls_varrec * x,struct pc_decls_varrec * y) {
    i64 res;
    res = pc_pcfns_pc_compare_nf(x,y);
    if (!!((u64)((*x).hasref))) {
        pc_pcfns_pc_unshare(x);
    };
    if (!!((u64)((*y).hasref))) {
        pc_pcfns_pc_unshare(y);
    };
    return res;
}

i64 pc_pcfns_u8inarray(byte a,struct pc_decls_objrec * p) {
    i64 i;
    byte *  q;
    i64 av_1;
    i = (i64)((*p).uarray.lower);
    q = (*p).uarray.ptr;
    av_1 = (i64)((*p).uarray.length);
    while (av_1-- > 0) {
L373 :;
        if (((u64)((*q)) == (u64)(a))) {
            return i;
        };
        ++q;
        ++i;
L374 :;
    }L375 :;
    ;
    return ((i64)((*p).uarray.lower) - (i64)1);
}

i64 pc_pcfns_u16inarray(u16 a,struct pc_decls_objrec * p) {
    i64 i;
    u16 *  q;
    i64 av_1;
    i = (i64)((*p).uarray.lower);
    q = (u16 *)((*p).uarray.ptr);
    av_1 = (i64)((*p).uarray.length);
    while (av_1-- > 0) {
L376 :;
        if (((u64)((*q)) == (u64)(a))) {
            return i;
        };
        ++q;
        ++i;
L377 :;
    }L378 :;
    ;
    return ((i64)((*p).uarray.lower) - (i64)1);
}

i64 pc_pcfns_u32inarray(u32 a,struct pc_decls_objrec * p) {
    i64 i;
    u32 *  q;
    i64 av_1;
    i = (i64)((*p).uarray.lower);
    q = (u32 *)((*p).uarray.ptr);
    av_1 = (i64)((*p).uarray.length);
    while (av_1-- > 0) {
L379 :;
        if (((u64)((*q)) == (u64)(a))) {
            return i;
        };
        ++q;
        ++i;
L380 :;
    }L381 :;
    ;
    return ((i64)((*p).uarray.lower) - (i64)1);
}

i64 pc_pcfns_u64inarray(u64 a,struct pc_decls_objrec * p) {
    i64 i;
    u64 *  q;
    i64 av_1;
    i = (i64)((*p).uarray.lower);
    q = (u64 *)((*p).uarray.ptr);
    av_1 = (i64)((*p).uarray.length);
    while (av_1-- > 0) {
L382 :;
        if (((*q) == a)) {
            return i;
        };
        ++q;
        ++i;
L383 :;
    }L384 :;
    ;
    return ((i64)((*p).uarray.lower) - (i64)1);
}

i64 pc_pcfns_bitinbits(byte a,struct pc_decls_objrec * p) {
    i64 i;
    i64 offset;
    i64 mask;
    byte *  q;
    i64 av_1;
    i = (i64)((*p).ubits.lower);
    q = (*p).ubits.ptr;
    offset = ((i64)((u64)((*p).ubits.bitoffset)) - (i64)1);
    mask = (i64)1;
    if (!!(offset)) {
        mask = (mask << offset);
    };
    av_1 = (i64)((*p).ubits.length);
    while (av_1-- > 0) {
L385 :;
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"Q^=",NULL);
        msysnewc_m_print_u64((*q),NULL);
        msysnewc_m_print_i64(((i64)((u64)((*q))) & mask),NULL);
        msysnewc_m_print_str((byte*)"MASK=",NULL);
        msysnewc_m_print_i64(mask,NULL);
        msysnewc_m_print_i64(i,NULL);
        msysnewc_m_print_str((byte*)"A=",NULL);
        msysnewc_m_print_u64(a,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        if (!!(((i64)((u64)((*q))) & mask))) {
            if (!!((u64)(a))) {
                return i;
            };
        } else if (((i64)((u64)(a)) == (i64)0)) {
            return i;
        };
        ++i;
        mask <<= (i64)1;
        if ((mask >= (i64)256)) {
            mask = (i64)1;
            ++q;
        };
L386 :;
    }L387 :;
    ;
    return ((i64)((*p).ubits.lower) - (i64)1);
}

i64 pc_pcfns_pc_strinstr(struct pc_decls_varrec * x,struct pc_decls_varrec * y) {
    i64 xlen;
    i64 ylen;
    i64 i;
    i64 j;
    i64 k;
    byte *  sx;
    byte *  sy;
    struct pc_decls_objrec *  px;
    struct pc_decls_objrec *  py;
    px = (*x).objptr;
    py = (*y).objptr;
    xlen = (i64)((*px).ustr.length);
    ylen = (i64)((*py).ustr.length);
    if (((xlen == (i64)0) || (ylen == (i64)0))) {
        return (i64)0;
    };
    k = (ylen - xlen);
    L388 :;
    for (i=(i64)0;i<=k;i+=(i64)1) {
L389 :;
        sx = (*px).ustr.strptr;
        sy = ((*py).ustr.strptr + i);
        L392 :;
        for (j=(i64)1;j<=xlen;j+=(i64)1) {
L393 :;
            if (((u64)((*sx)) != (u64)((*sy)))) {
                goto L396 ;
;
            };
            ++sx;
            ++sy;
L394 :;
        }L395 :;
        ;
        return (i + (i64)1);
        //nextpos:
L396 :;
;
L390 :;
    }L391 :;
    ;
    return (i64)0;
}

byte * pc_pcfns_getbitoffset(byte * p,i64 offset,i64 index,i64 t,byte * newoffset) {
    switch (t) {
    case 41:;
    {
        index += offset;
        p += (index >> (i64)3);
        (*newoffset) = (u64)((index & (i64)7));
    }break;
    case 42:;
    {
        index += (offset >> (i64)1);
        p += (index >> (i64)2);
        (*newoffset) = (u64)(((index & (i64)3) * (i64)2));
    }break;
    case 43:;
    {
        index += (offset >> (i64)2);
        p += (index >> (i64)1);
        (*newoffset) = (u64)(((index & (i64)1) * (i64)4));
    }break;
    default: {
    }
    } //SW
;
    return p;
}

void pc_pcfns_pc_iappendlist(struct pc_decls_varrec * a,struct pc_decls_varrec * b) {
    i64 n;
    struct pc_decls_varrec *  q;
    struct pc_decls_objrec *  p;
    p = (*a).objptr;
    if (((i64)((u64)((*p).objtype)) != (i64)0)) {
        pc_support_pcerror((byte*)"Can't extend slice");
    };
    if (!(!!((u64)((*p).ulist.mutable)))) {
        p = pc_objlib_copyonwrite(p,(i64)29);
    };
    n = ((i64)((u64)((*p).ulist.length)) + (i64)1);
    if ((n > (i64)((u64)((*p).ulist.allocated)))) {
        pc_objlib_list_resize(p,n);
    } else {
        (*p).ulist.length = (u64)(n);
    };
    (*(((*p).ulist.vptr + n) - (i64)1)).tagx = (u64)((i64)0);
    (*a).objptr = p;
    q = (((*p).ulist.vptr + (i64)((*p).ulist.length)) - (i64)1);
    if (!!(b)) {
        (*q) = (*b);
    } else {
        (*q).tagx = (u64)((i64)0);
    };
}

void pc_pcfns_pc_iappendarray(struct pc_decls_varrec * a,struct pc_decls_varrec * b) {
    i64 n;
    byte *  q;
    struct pc_decls_objrec *  p;
    p = (*a).objptr;
    if (((i64)((u64)((*p).objtype)) != (i64)0)) {
        pc_support_pcerror((byte*)"Can't extend slice");
    };
    if (!(!!((u64)((*p).uarray.mutable)))) {
        p = pc_objlib_copyonwrite(p,(i64)((*a).tag));
    };
    n = ((i64)((u64)((*p).uarray.length)) + (i64)1);
    if ((n > (i64)((u64)((*p).uarray.allocated)))) {
        pc_objlib_array_resize(p,n);
    } else {
        (*p).uarray.length = (u64)(n);
    };
    (*a).objptr = p;
    q = ((*p).uarray.ptr + (((i64)((u64)((*p).uarray.length)) - (i64)1) * pc_decls_ttsize[((i64)((*p).uarray.elemtag))]));
    if (!!(b)) {
        pc_pcfns_pc_storepacked(q,b,(i64)((*p).uarray.elemtag));
    };
}

void pc_pcfns_pc_mul_listi(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c) {
    struct pc_decls_varrec *  newptr;
    struct pc_decls_varrec *  oldptr;
    struct pc_decls_varrec *  q;
    i64 newlength;
    i64 oldlength;
    struct pc_decls_varrec d;
    struct pc_decls_objrec *  pa;
    struct pc_decls_objrec *  pc;
    i64 av_1;
    d = (*a);
    pa = d.objptr;
    oldlength = (i64)((*pa).ulist.length);
    newlength = (oldlength * (*b).value);
    oldptr = (*pa).ulist.vptr;
    if (!(!!(oldlength))) {
        return;
    };
    if ((newlength < (i64)0)) {
        pc_support_pcerror((byte*)"mullist 0");
    } else if ((newlength == (i64)0)) {
        if (!!((u64)((*a).hasref))) {
            pc_pcfns_pc_unshare(a);
        };
        (*c).tagx = (u64)((i64)29);
        (*c).objptr = pc_objlib_emptylist;
        ++(*pc_objlib_emptylist).refcount;
        return;
    };
    if ((oldlength == (i64)1)) {
        pc = pc_objlib_list_new(newlength,(i64)((*pa).ulist.lower),(struct pc_decls_varrec *)(0));
        newptr = (*pc).ulist.vptr;
        (*c).tagx = (u64)(d.tagx);
        (*c).objptr = pc;
        q = (*d.objptr).ulist.vptr;
        av_1 = newlength;
        while (av_1-- > 0) {
L397 :;
            (*newptr) = (*q);
            if (!!((u64)((*newptr).hasref))) {
                ++(*(*newptr).objptr).refcount;
                pc_pcfns_pc_dupl(newptr);
            };
            ++newptr;
L398 :;
        }L399 :;
        ;
        if (!!((u64)(d.hasref))) {
            pc_pcfns_pc_unshare(&d);
        };
    } else {
        pc_support_pcerror((byte*)"MULLISTINT/COMPLEX");
    };
}

void pc_pcfns_pc_mul_stri(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c) {
    i64 m;
    i64 oldlen;
    i64 newlen;
    byte *  p;
    struct pc_decls_varrec v;
    struct pc_decls_objrec *  pa;
    i64 av_1;
    m = pc_support_getintvalue(b);
    if ((m < (i64)0)) {
        pc_support_pcerror((byte*)"neg str mul");
    } else if ((m == (i64)0)) {
        pc_pcfns_pc_emptystring(c);
        return;
    } else if ((m == (i64)1)) {
        if ((a != c)) {
            (*c) = (*a);
            if (!!((u64)((*c).hasref))) {
                pc_pcfns_pc_share(c);
            };
        };
        return;
    } else {
        pa = (*a).objptr;
        oldlen = (i64)((*pa).ustr.length);
        if (!!(oldlen)) {
            newlen = (oldlen * m);
            pc_pcfns_pc_makestringn(newlen,&v);
            p = (*v.objptr).ustr.strptr;
            av_1 = m;
            while (av_1-- > 0) {
L400 :;
                memcpy((void *)(p),(void *)((*pa).ustr.strptr),(u64)(oldlen));
                p += oldlen;
L401 :;
            }L402 :;
            ;
            pc_pcfns_pc_unshare(a);
            (*c) = v;
        } else {
            (*c) = (*a);
            pc_pcfns_pc_share(a);
            return;
        };
    };
}

void pc_pcfns_pc_duplvar(struct pc_decls_varrec * p) {
    if (!!((u64)((*p).hasref))) {
        ((*pc_decls_dupl_table[((i64)((*p).tag))]))(p);
    };
}

void pc_pcfns_pc_iconcatlist(struct pc_decls_varrec * a,struct pc_decls_varrec * b) {
    struct pc_decls_varrec *  d;
    i64 alen;
    i64 blen;
    i64 newlen;
    struct pc_decls_objrec *  pa;
    struct pc_decls_objrec *  pb;
    i64 av_1;
    i64 av_2;
    pa = (*a).objptr;
    if (!(!!((u64)((*pa).ulist.mutable)))) {
        pa = pc_objlib_copyonwrite(pa,(i64)((*a).tag));
        (*a).objptr = pa;
    };
    pb = (*b).objptr;
    alen = (i64)((*pa).ulist.length);
    blen = (i64)((*pb).ulist.length);
    if ((alen == (i64)0)) {
        if (!!(blen)) {
            pc_objlib_list_resize(pa,blen);
            d = (*pa).ulist.vptr;
            memcpy((void *)(d),(void *)((*pb).ulist.vptr),(u64)((blen * (i64)16)));
            av_1 = blen;
            while (av_1-- > 0) {
L403 :;
                if (!!((u64)((*d).hasref))) {
                    pc_pcfns_pc_share(d);
                };
                ++d;
L404 :;
            }L405 :;
            ;
        };
    } else if (!!(blen)) {
        newlen = (alen + blen);
        pc_objlib_list_resize(pa,newlen);
        d = ((*pa).ulist.vptr + alen);
        memcpy((void *)(d),(void *)((*pb).ulist.vptr),(u64)((blen * (i64)16)));
        av_2 = blen;
        while (av_2-- > 0) {
L406 :;
            if (!!((u64)((*d).hasref))) {
                pc_pcfns_pc_share(d);
            };
            ++d;
L407 :;
        }L408 :;
        ;
    };
}

void pc_pcfns_pc_iappendbits(struct pc_decls_varrec * a,struct pc_decls_varrec * b) {
    i64 elemtype;
    i64 n;
    byte bitoffset;
    byte *  q;
    struct pc_decls_objrec *  p;
    p = (*a).objptr;
    if (!(!!((u64)((*p).ubits.mutable)))) {
        (*a).objptr = (p = pc_objlib_copyonwrite(p,(i64)((*a).tag)));
    };
    n = ((i64)((u64)((*p).ubits.length)) + (i64)1);
    if ((n > (i64)((*p).ubits.allocated64))) {
        pc_objlib_bits_resize(p,n);
    } else {
        (*p).ubits.length = (u64)(n);
    };
    elemtype = (i64)((*p).ubits.elemtag);
    q = pc_pcfns_getbitoffset((*p).ubits.ptr,((i64)((u64)((*p).ubits.bitoffset)) - (i64)1),((i64)((u64)((*p).ubits.length)) - (i64)1),elemtype,&bitoffset);
    if (!!(b)) {
        pc_pcfns_pc_storebit(q,(i64)(bitoffset),b,elemtype,(i64)0);
    };
}

void pc_pcfns_pc_makestring(byte * s,i64 length,struct pc_decls_varrec * dest) {
    byte *  t;
    if ((s == 0)) {
        pc_pcfns_pc_makestringx((byte *)(0),(i64)0,(i64)0,dest);
        return;
    };
    if ((length == (i64)-1)) {
        length = (i64)(strlen((i8 *)(s)));
    };
    if ((length == (i64)0)) {
        pc_pcfns_pc_makestringx(t,(i64)0,(i64)0,dest);
    } else {
        t = (byte *)(mlib_pcm_alloc(length));
        memcpy((void *)(t),(void *)(s),(u64)(length));
        pc_pcfns_pc_makestringx(t,length,mlib_allocbytes,dest);
    };
}

void pc_pcfns_pc_makestringx(byte * s,i64 length,i64 allocated,struct pc_decls_varrec * dest) {
    struct pc_decls_objrec *  p;
    if ((length == (i64)-1)) {
        length = (i64)(strlen((i8 *)(s)));
    };
    (*dest).tagx = (u64)((i64)65541);
    (*dest).objptr = (p = pc_objlib_obj_new((i64)5));
    if ((length == (i64)0)) {
        (*p).ustr.strptr = (byte *)(0);
    } else {
        (*p).ustr.strptr = s;
        (*p).ustr.length = length;
        (*p).ustr.allocated = (u64)(allocated);
    };
    (*p).ustr.mutable = (u64)((i64)1);
}

void pc_pcfns_pc_makestringn(i64 length,struct pc_decls_varrec * dest) {
    struct pc_decls_objrec *  p;
    (*dest).tagx = (u64)((i64)65541);
    (*dest).objptr = (p = pc_objlib_obj_new((i64)5));
    if ((length > (i64)((u64)4000000000u))) {
        pc_support_pcerror((byte*)"String*n too long");
    };
    (*p).ustr.strptr = (byte *)(mlib_pcm_alloc(length));
    (*p).ustr.mutable = (u64)((i64)1);
    (*p).ustr.length = length;
    (*p).ustr.allocated = (u64)(mlib_allocbytes);
}

void pc_pcfns_pc_emptystring(struct pc_decls_varrec * dest) {
    (*dest).tagx = (u64)((i64)65541);
    (*dest).objptr = pc_objlib_emptystring;
    ++(*pc_objlib_emptystring).refcount;
}

void pc_pcfns_pc_makechar(i64 ch,struct pc_decls_varrec * dest) {
    struct pc_decls_varrec v;
    byte str[10];
    struct pc_decls_objrec *  p;
    (*dest).tagx = (u64)((i64)65541);
    p = pc_decls_chrtable[(ch)];
    if ((p == 0)) {
        str[((i64)1)-1] = (u64)(ch);
        str[((i64)2)-1] = (u64)0u;
        pc_pcfns_pc_makestring(str,(i64)1,&v);
        p = v.objptr;
        (*p).ustr.mutable = (u64)((i64)0);
        pc_decls_chrtable[(ch)] = p;
    };
    ++(*p).refcount;
    (*dest).objptr = p;
}

static void pc_objlib_Dinit(void) {
    pc_objlib_zeroobj = (struct pc_decls_objrec *)(mlib_pcm_allocz((i64)32));
    (*pc_objlib_zeroobj).refcount = (u64)((i64)1);
    pc_objlib_emptylist = pc_objlib_obj_new((i64)29);
    (*pc_objlib_emptylist).ulist.lower = (i64)1;
    (*pc_objlib_emptylist).objtype = (u64)((i64)0);
    pc_objlib_emptystring = pc_objlib_obj_new((i64)5);
    (*pc_objlib_emptystring).objtype = (u64)((i64)0);
    pc_objlib_emptyset = pc_objlib_obj_new((i64)9);
    (*pc_objlib_emptyset).objtype = (u64)((i64)0);
}

struct pc_decls_objrec * pc_objlib_obj_new(i64 tag) {
    struct pc_decls_objrec *  p;
    p = (struct pc_decls_objrec *)(mlib_pcm_alloc32());
    (*p) = (*pc_objlib_zeroobj);
    (*p).tag = (u64)(tag);
    return p;
}

void pc_objlib_freeobject(struct pc_decls_objrec * p) {
    mlib_pcm_free32((void *)(p));
}

struct pc_decls_objrec * pc_objlib_array_new(i64 ta,i64 elemtype,i64 length,i64 lower) {
    struct pc_decls_objrec *  p;
    byte *  q;
    i64 elemsize;
    elemsize = pc_decls_ttsize[(elemtype)];
    p = pc_objlib_obj_new(ta);
    (*p).uarray.mutable = (u64)((i64)1);
    (*p).uarray.lower = lower;
    (*p).uarray.length = (u64)(length);
    (*p).uarray.objtype = (u64)((i64)0);
    (*p).uarray.elemtag = elemtype;
    if (!!(length)) {
        q = ((*p).uarray.ptr = (byte *)(mlib_pcm_allocz((length * elemsize))));
        (*p).uarray.allocated = (u64)((mlib_allocbytes / elemsize));
    };
    return p;
}

struct pc_decls_objrec * pc_objlib_list_new(i64 length,i64 lower,struct pc_decls_varrec * defval) {
    struct pc_decls_objrec *  p;
    struct pc_decls_varrec *  q;
    i64 av_1;
    p = pc_objlib_obj_new((i64)29);
    (*p).ulist.mutable = (u64)((i64)1);
    (*p).ulist.lower = lower;
    (*p).ulist.length = (u64)(length);
    (*p).ulist.objtype = (u64)((i64)0);
    if (!!(length)) {
        q = ((*p).ulist.vptr = (struct pc_decls_varrec *)(mlib_pcm_alloc((length * (i64)16))));
        (*p).ulist.allocated = (u64)((mlib_allocbytes / (i64)16));
        av_1 = length;
        while (av_1-- > 0) {
L409 :;
            if (!!(defval)) {
                (*q) = (*pc_pcfns_pc_share(defval));
            } else {
                (*q).tagx = (u64)((i64)0);
            };
            ++q;
L410 :;
        }L411 :;
        ;
    };
    return p;
}

void pc_objlib_objtovar(struct pc_decls_objrec * p,struct pc_decls_varrec * q) {
    (*q).tagx = (u64)(((i64)((u64)((*p).tag)) | (i64)65536));
    (*q).objptr = p;
}

struct pc_decls_objrec * pc_objlib_set_new(i64 length,i64 lower) {
    struct pc_decls_objrec *  p;
    p = pc_objlib_bits_new((i64)41,length,lower);
    (*p).tag = (u64)((i64)9);
    return p;
}

struct pc_decls_objrec * pc_objlib_bits_new(i64 elemtype,i64 length,i64 lower) {
    struct pc_decls_objrec *  p;
    i64 bitwidthx;
    i64 nbits;
    i64 nbytes;
    p = pc_objlib_obj_new((i64)31);
    (*p).ubits.mutable = (u64)((i64)1);
    (*p).ubits.lower = lower;
    (*p).ubits.length = (u64)(length);
    (*p).ubits.objtype = (u64)((i64)0);
    (*p).ubits.elemtag = (u64)(elemtype);
    bitwidthx = (i64)(pc_decls_ttbitwidth[(elemtype)]);
    nbits = (length * bitwidthx);
    nbytes = ((((nbits - (i64)1) / (i64)64) + (i64)1) * (i64)8);
    if (!!(length)) {
        (*p).ubits.ptr = (byte *)(mlib_pcm_alloc(nbytes));
        (*p).ubits.allocated64 = (u64)(((i64)((u64)(mlib_allocbytes)) * ((i64)8 / bitwidthx)));
        mlib_pcm_clearmem((void *)((*p).ubits.ptr),mlib_allocbytes);
    } else {
        (*p).ubits.ptr = (byte *)(0);
    };
    return p;
}

struct pc_decls_objrec * pc_objlib_struct_new(i64 t) {
    struct pc_decls_objrec *  p;
    p = pc_objlib_obj_new(t);
    (*p).ustruct.mutable = (u64)((i64)1);
    (*p).ustruct.ptr = (byte *)(mlib_pcm_allocz(pc_decls_ttsize[(t)]));
    (*p).ustruct.allocated = (u64)(mlib_allocbytes);
    return p;
}

struct pc_decls_objrec * pc_objlib_dict_new(i64 n) {
    struct pc_decls_objrec *  p;
    i64 m;
    m = msysnewc_m_imax((i64)16,pc_support_nextpoweroftwo((n * (i64)2)));
    p = pc_objlib_list_new(m,(i64)1,(struct pc_decls_varrec *)(0));
    (*p).tag = (u64)((i64)10);
    (*p).udict.dictitems = (u64)((i64)0);
    return p;
}

struct pc_decls_objrec * pc_objlib_record_new(i64 rectype) {
    struct pc_decls_objrec *  p;
    p = pc_objlib_obj_new(rectype);
    (*p).urec.mutable = (u64)((i64)1);
    (*p).urec.vptr = (struct pc_decls_varrec *)(mlib_pcm_allocz(((i64)((u64)(pc_decls_ttlength[(rectype)])) * (i64)16)));
    return p;
}

void pc_objlib_list_free(struct pc_decls_objrec * p) {
    if (!!((u64)((*p).ulist.length))) {
        mlib_pcm_free((void *)((*p).ulist.vptr),((i64)((u64)((*p).ulist.allocated)) * (i64)16));
    };
}

void pc_objlib_record_free(struct pc_decls_objrec * p) {
    mlib_pcm_free((void *)((*p).urec.vptr),((i64)((u64)(pc_decls_ttlength[((i64)((*p).tag))])) * (i64)16));
}

void pc_objlib_array_free(struct pc_decls_objrec * p) {
    if (!!((u64)((*p).ulist.length))) {
        mlib_pcm_free((void *)((*p).uarray.ptr),((i64)((u64)((*p).uarray.allocated)) * pc_decls_ttsize[((i64)((*p).uarray.elemtag))]));
    };
}

void pc_objlib_bits_free(struct pc_decls_objrec * p) {
    if (!!((u64)((*p).ulist.length))) {
        mlib_pcm_free((void *)((*p).ubits.ptr),pc_objlib_bits_bytesize(p));
    };
}

void pc_objlib_dict_free(struct pc_decls_objrec * p) {
    if (!!((u64)((*p).udict.length))) {
        mlib_pcm_free((void *)((*p).udict.vptr),((i64)((u64)((*p).udict.allocated)) * (i64)16));
    };
}

i64 pc_objlib_bits_bytesize(struct pc_decls_objrec * p) {
    i64 elemtype;
    i64 nbits;
    elemtype = (i64)((*p).ubits.elemtag);
    if ((elemtype==(i64)41) || (elemtype==(i64)42) || (elemtype==(i64)43)) {
        nbits = ((i64)(pc_decls_ttbitwidth[(elemtype)]) * (i64)((u64)((*p).ubits.length)));
        if (!!((nbits & (i64)7))) {
            return ((nbits / (i64)8) + (i64)1);
        } else {
            return (nbits / (i64)8);
        };
    };
    return (pc_decls_ttsize[(elemtype)] * (i64)((u64)((*p).uarray.length)));
}

void pc_objlib_list_resize(struct pc_decls_objrec * p,i64 n) {
    struct pc_decls_varrec *  q;
    if ((n <= (i64)((u64)((*p).ulist.allocated)))) {
        (*p).ulist.length = (u64)(n);
    } else {
        q = (struct pc_decls_varrec *)(mlib_pcm_alloc((n * (i64)16)));
        if (!!((u64)((*p).ulist.length))) {
            memcpy((void *)(q),(void *)((*p).ulist.vptr),(u64)(((i64)((u64)((*p).ulist.length)) * (i64)16)));
            mlib_pcm_free((void *)((*p).ulist.vptr),((i64)((u64)((*p).ulist.allocated)) * (i64)16));
        };
        (*p).ulist.vptr = q;
        (*p).ulist.length = (u64)(n);
        (*p).ulist.allocated = (u64)((mlib_allocbytes / (i64)16));
    };
}

void pc_objlib_array_resize(struct pc_decls_objrec * p,i64 n) {
    byte *  q;
    i64 elemsize;
    elemsize = pc_decls_ttsize[((i64)((*p).uarray.elemtag))];
    if ((n <= (i64)((u64)((*p).uarray.allocated)))) {
        (*p).uarray.length = (u64)(n);
    } else {
        q = (byte *)(mlib_pcm_alloc((n * elemsize)));
        if (!!((u64)((*p).uarray.length))) {
            memcpy((void *)(q),(void *)((*p).uarray.ptr),(u64)(((i64)((u64)((*p).uarray.length)) * elemsize)));
            mlib_pcm_free((void *)((*p).uarray.ptr),((i64)((u64)((*p).uarray.allocated)) * elemsize));
        };
        (*p).uarray.ptr = q;
        (*p).uarray.length = (u64)(n);
        (*p).uarray.allocated = (u64)((mlib_allocbytes / elemsize));
    };
}

void pc_objlib_bits_resize(struct pc_decls_objrec * p,i64 n) {
    struct pc_decls_objrec *  pnew;
    i64 oldrefcount;
    if ((n <= (i64)((*p).ubits.allocated64))) {
        (*p).ubits.length = (u64)(n);
        return;
    };
    pnew = pc_objlib_bits_new((i64)((*p).ubits.elemtag),(i64)((*p).ubits.length),(i64)((*p).ubits.lower));
    memcpy((void *)((*pnew).ubits.ptr),(void *)((*p).ubits.ptr),(u64)(pc_objlib_bits_bytesize(p)));
    oldrefcount = (i64)((*p).ubits.refcount);
    pc_objlib_bits_free(p);
    (*p) = (*pnew);
    (*p).refcount = (u64)(oldrefcount);
}

void pc_objlib_string_resize(struct pc_decls_objrec * p,i64 n) {
    byte *  q;
    if ((n <= (i64)((u64)((*p).ustr.allocated)))) {
        (*p).ustr.length = n;
    } else {
        q = (byte *)(mlib_pcm_alloc(n));
        if (!!((i64)((*p).ustr.length))) {
            memcpy((void *)(q),(void *)((*p).ustr.strptr),(u64)((*p).ustr.length));
            mlib_pcm_free((void *)((*p).ustr.strptr),(i64)((*p).ustr.allocated));
        };
        (*p).ustr.strptr = q;
        (*p).ustr.length = n;
        (*p).ustr.allocated = (u64)(mlib_allocbytes);
    };
}

struct pc_decls_objrec * pc_objlib_copyonwrite(struct pc_decls_objrec * p,i64 tag) {
    struct pc_decls_objrec *  q;
    struct pc_decls_varrec v;
    if (!!((u64)((*p).ulist.mutable))) {
        return p;
    };
    v.tagx = (u64)((tag + (i64)65536));
    v.objptr = p;
    pc_pcfns_pc_dupl(&v);
    q = v.objptr;
    (*q).ulist.mutable = (u64)((i64)1);
    return q;
}

struct pc_decls_objrec * pc_objlib_make_strslicexobj(byte * s,i64 length) {
    struct pc_decls_objrec *  p;
    if ((length == (i64)0)) {
        s = (byte *)(0);
    };
    p = pc_objlib_obj_new((i64)5);
    (*p).ustr.strptr = s;
    (*p).ustr.mutable = (u64)((i64)1);
    (*p).ustr.length = length;
    (*p).objtype = (u64)((i64)2);
    return p;
}

struct pc_decls_objrec * pc_objlib_bignum_make(void * bn) {
    struct pc_decls_objrec *  p;
    p = pc_objlib_obj_new((i64)7);
    (*p).udec.bnptr = bn;
    return p;
}

void pc_bignum_bx_makestr(byte * s,i64 length,struct pc_decls_varrec * p) {
    pc_bignum_makebnvar(p,mbignum_bn_makestr(s,length));
}

byte * pc_bignum_bx_tostring(struct pc_decls_varrec * a,i64 fmt) {
    return mbignum_bn_tostring((struct mbignum_bignumrec *)((*(*a).objptr).udec.bnptr),fmt);
}

void pc_bignum_bx_dupl(struct pc_decls_varrec * p) {
    struct mbignum_bignumrec *  a;
    a = mbignum_bn_init();
    mbignum_bn_dupl(a,(struct mbignum_bignumrec *)((*(*p).objptr).udec.bnptr));
    pc_bignum_makebnvar(p,a);
}

void pc_bignum_bx_negto(struct pc_decls_varrec * p) {
    mbignum_bn_negto((struct mbignum_bignumrec *)((*(*p).objptr).udec.bnptr));
}

void pc_bignum_bx_absto(struct pc_decls_varrec * p) {
    mbignum_bn_absto((struct mbignum_bignumrec *)((*(*p).objptr).udec.bnptr));
}

static struct mbignum_bignumrec * pc_bignum_makebnvar(struct pc_decls_varrec * dest,struct mbignum_bignumrec * bn) {
    (*dest).tagx = (u64)((i64)65543);
    if ((bn == 0)) {
        bn = mbignum_bn_init();
    };
    (*dest).objptr = pc_objlib_bignum_make((void *)(bn));
    return bn;
}

void pc_bignum_bx_free(struct pc_decls_varrec * a) {
    mbignum_bn_free((struct mbignum_bignumrec *)((*(*a).objptr).udec.bnptr));
    pc_objlib_freeobject((*a).objptr);
}

void pc_bignum_bx_makeint(i64 aa,struct pc_decls_varrec * dest) {
    pc_bignum_makebnvar(dest,mbignum_bn_makeint(aa));
}

void pc_bignum_bx_add(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c) {
    struct pc_decls_varrec v;
    mbignum_bn_add(pc_bignum_makebnvar(&v,(struct mbignum_bignumrec *)(0)),(struct mbignum_bignumrec *)((*(*a).objptr).udec.bnptr),(struct mbignum_bignumrec *)((*(*b).objptr).udec.bnptr));
    (*c) = v;
}

void pc_bignum_bx_sub(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c) {
    struct pc_decls_varrec v;
    mbignum_bn_sub(pc_bignum_makebnvar(&v,(struct mbignum_bignumrec *)(0)),(struct mbignum_bignumrec *)((*(*a).objptr).udec.bnptr),(struct mbignum_bignumrec *)((*(*b).objptr).udec.bnptr));
    (*c) = v;
}

void pc_bignum_bx_mul(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c) {
    struct pc_decls_varrec v;
    mbignum_bn_mulp(pc_bignum_makebnvar(&v,(struct mbignum_bignumrec *)(0)),(struct mbignum_bignumrec *)((*(*a).objptr).udec.bnptr),(struct mbignum_bignumrec *)((*(*b).objptr).udec.bnptr),(i64)1000);
    (*c) = v;
}

void pc_bignum_bx_div(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c) {
    struct pc_decls_varrec v;
    mbignum_bn_div(pc_bignum_makebnvar(&v,(struct mbignum_bignumrec *)(0)),(struct mbignum_bignumrec *)((*(*a).objptr).udec.bnptr),(struct mbignum_bignumrec *)((*(*b).objptr).udec.bnptr),(i64)1000);
    (*c) = v;
}

void pc_bignum_bx_idiv(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c) {
    struct pc_decls_varrec v;
    mbignum_bn_idiv(pc_bignum_makebnvar(&v,(struct mbignum_bignumrec *)(0)),(struct mbignum_bignumrec *)((*(*a).objptr).udec.bnptr),(struct mbignum_bignumrec *)((*(*b).objptr).udec.bnptr));
    (*c) = v;
}

void pc_bignum_bx_irem(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c) {
    struct pc_decls_varrec v;
    mbignum_bn_irem(pc_bignum_makebnvar(&v,(struct mbignum_bignumrec *)(0)),(struct mbignum_bignumrec *)((*(*a).objptr).udec.bnptr),(struct mbignum_bignumrec *)((*(*b).objptr).udec.bnptr));
    (*c) = v;
}

i64 pc_bignum_bx_equal(struct pc_decls_varrec * a,struct pc_decls_varrec * b) {
    return mbignum_bn_equal((struct mbignum_bignumrec *)((*(*a).objptr).udec.bnptr),(struct mbignum_bignumrec *)((*(*b).objptr).udec.bnptr));
}

i64 pc_bignum_bx_cmp(struct pc_decls_varrec * a,struct pc_decls_varrec * b) {
    return mbignum_bn_cmp((struct mbignum_bignumrec *)((*(*a).objptr).udec.bnptr),(struct mbignum_bignumrec *)((*(*b).objptr).udec.bnptr));
}

i64 pc_bignum_bx_int(struct pc_decls_varrec * p) {
    struct mbignum_bignumrec *  a;
    struct mbignum_bignumrec *  b;
    i64 x;
    a = (struct mbignum_bignumrec *)((*(*p).objptr).udec.bnptr);
    if (!!(mbignum_bn_isint(a))) {
        return mbignum_bn_toint(a);
    };
    b = mbignum_bn_init();
    mbignum_bn_fix(b,a);
    x = mbignum_bn_toint(b);
    mbignum_bn_free(b);
    return x;
}

void pc_bignum_bx_power(struct pc_decls_varrec * a,i64 n,struct pc_decls_varrec * dest) {
    struct pc_decls_varrec e;
    mbignum_bn_ipower(pc_bignum_makebnvar(&e,(struct mbignum_bignumrec *)(0)),(struct mbignum_bignumrec *)((*(*a).objptr).udec.bnptr),n);
    (*dest) = e;
}

void pc_bignum_bx_reduce(struct pc_decls_varrec * bn) {
    pc_support_pcerror((byte*)"BX_REDUCE");
}

i64 pc_bignum_bx_length(struct pc_decls_varrec * bn) {
    return mbignum_bn_digits((struct mbignum_bignumrec *)((*(*bn).objptr).udec.bnptr));
}

struct mbignum_bignumrec * mbignum_bn_init(void) {
    struct mbignum_bignumrec *  a;
    a = mbignum_makebignum((i64)0);
    return a;
}

static i64 mbignum_readexpon(byte * s) {
    i64 neg;
    i64 expon;
    neg = (expon = (i64)0);
    if (((i64)((*s))==(i64)43)) {
        ++s;
    }else if (((i64)((*s))==(i64)45)) {
        neg = (i64)1;
        ++s;
    };
    L412 :;
    switch ((i64)((*s))) {
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
        expon = ((expon * (i64)10) + ((u64)((*s)) - '0'));
        ++s;
    }break;
    case 95:;
    case 39:;
    case 96:;
    case 32:;
    {
        ++s;
    }break;
    case 0:;
    {
        goto L413 ;
    }break;
    default: {
        mbignum_bn_error((byte*)"make expon?");
    }
    } //SW
goto L412 ;
L413 :;
    ;
    return (!!(neg)?-(expon):expon);
}

void mbignum_bn_print(struct mbignum_bignumrec * a,i64 format) {
    byte *  s;
    s = mbignum_bn_tostring(a,format);
    msysnewc_m_print_startcon();
    msysnewc_m_print_str(s,NULL);
    msysnewc_m_print_end();
    ;
}

void mbignum_bn_println(struct mbignum_bignumrec * a,i64 format) {
    mbignum_bn_print(a,format);
    msysnewc_m_print_startcon();
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

static i64 mbignum_getbintype(struct mbignum_bignumrec * a,struct mbignum_bignumrec * b) {
    i64 atype;
    i64 btype;
    atype = (i64)((*a).numtype);
    btype = (i64)((*b).numtype);
    if (((atype == (i64)3) || (btype == (i64)3))) {
        return (i64)4;
    };
    if ((atype==(i64)1)) {
        if ((btype==(i64)1)) {
            return (i64)1;
        }else if ((btype==(i64)0)) {
            return (i64)5;
        } else {
            return (i64)6;
        };
    }else if ((atype==(i64)0)) {
        if ((btype==(i64)1)) {
            return (i64)7;
        }else if ((btype==(i64)0)) {
            return (i64)2;
        } else {
            return (i64)9;
        };
    } else {
        if ((btype==(i64)1)) {
            return (i64)8;
        }else if ((btype==(i64)0)) {
            return (i64)10;
        } else {
            return (i64)3;
        };
    };
}

static struct mbignum_bignumrec * mbignum_makebignum(i64 length) {
    struct mbignum_bignumrec *  a;
    a = (struct mbignum_bignumrec *)(mbignum_bn_alloc((i64)32));
    if (!!(length)) {
        (*a).num = (i32 (*)[])(mbignum_bn_alloc((length * (i64)4)));
        (*a).numtype = (i64)1;
    } else {
        (*a).num = (i32 (*)[])(0);
        (*a).numtype = (i64)0;
    };
    (*a).length = length;
    (*a).expon = (i64)0;
    (*a).neg = (i64)0;
    return a;
}

static i32 * mbignum_makesmallnum(i64 length) {
    return (i32 *)(mbignum_bn_alloc((length * (i64)4)));
}

static struct mbignum_bignumrec * mbignum_smalltobig(struct mbignum_bignumrec * c,i32 * a,i64 length,i64 alloc,i64 offset) {
    i32 *  p;
    i64 leadingzeros;
    i64 trailingzeros;
    i64 nonzeros;
    i64 newlength;
    i64 av_1;
    mbignum_bn_setzero(c);
    p = a;
    leadingzeros = (trailingzeros = (nonzeros = (i64)0));
    av_1 = length;
    while (av_1-- > 0) {
L414 :;
        if (!!((i64)((*p++)))) {
            nonzeros = (i64)1;
            trailingzeros = (i64)0;
        } else {
            if (!!(nonzeros)) {
                ++trailingzeros;
            } else {
                ++leadingzeros;
            };
        };
L415 :;
    }L416 :;
    ;
    mbignum_stblz = leadingzeros;
    if (!!(nonzeros)) {
        newlength = ((length - trailingzeros) - leadingzeros);
        if (((newlength == length) && (length == alloc))) {
            (*c).num = (i32 (*)[])(a);
        } else {
            (*c).num = (i32 (*)[])(mbignum_makesmallnum(newlength));
            memcpy((void *)((*c).num),(void *)((a + leadingzeros)),(u64)((newlength * (i64)4)));
            mbignum_freesmall((a + offset),alloc);
        };
        (*c).length = newlength;
        (*c).numtype = (i64)1;
        (*c).expon = ((length - (i64)1) - leadingzeros);
    } else if (!!(alloc)) {
        mbignum_freesmall((a + offset),alloc);
    };
    return c;
}

static void mbignum_freesmall(i32 * p,i64 length) {
    mbignum_freemem((void *)(p),(length * (i64)4));
}

void * mbignum_bn_alloc(i64 size) {
    void *  p;
    p = mlib_pcm_alloc(size);
    if ((p == 0)) {
        mlib_abortprogram((byte*)"bignum:out of memory");
    };
    return p;
}

void * mbignum_checkedmalloc(i64 size) {
    void *  p;
    p = malloc((u64)(size));
    if ((p == 0)) {
        mlib_abortprogram((byte*)"CM:Out of memory");
    };
    return p;
}

void mbignum_bn_free(struct mbignum_bignumrec * a) {
    if (!!(a)) {
        mbignum_bn_setzero(a);
        mbignum_freemem((void *)(a),(i64)32);
    };
}

static void mbignum_freemem(void * p,i64 size) {
    mlib_pcm_free(p,size);
}

void mbignum_bn_setzero(struct mbignum_bignumrec * a) {
    if (!!(a)) {
        if (!!((*a).num)) {
            mbignum_freesmall((i32 *)((*a).num),(*a).length);
        };
        (*a).num = (i32 (*)[])(0);
        (*a).length = (i64)0;
        (*a).neg = (i64)0;
        (*a).expon = (i64)0;
        (*a).numtype = (i64)0;
    };
}

void mbignum_bn_move(struct mbignum_bignumrec * a,struct mbignum_bignumrec * b) {
    mbignum_bn_setzero(a);
    (*a) = (*b);
    memset((void *)(b),(i64)0,(u64)((i64)32));
}

void mbignum_bn_dupl(struct mbignum_bignumrec * a,struct mbignum_bignumrec * b) {
    struct mbignum_bignumrec *  c;
    i64 size;
    c = mbignum_bn_init();
    (*c) = (*b);
    if (!!((*c).length)) {
        (*c).num = (i32 (*)[])(mbignum_makesmallnum((size = (*c).length)));
        memcpy((void *)((*c).num),(void *)((*b).num),(u64)((size * (i64)4)));
    };
    mbignum_bn_move(a,c);
    mbignum_bn_free(c);
}

void mbignum_bn_setinf(struct mbignum_bignumrec * dest) {
    mbignum_bn_setzero(dest);
    (*dest).numtype = (i64)2;
}

void mbignum_bn_setnan(struct mbignum_bignumrec * dest) {
    mbignum_bn_setzero(dest);
    (*dest).numtype = (i64)3;
}

static void mbignum_bn_error(byte * mess) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"BN:",NULL);
    msysnewc_m_print_end();
    ;
    mlib_abortprogram(mess);
}

i64 mbignum_bn_iszero(struct mbignum_bignumrec * a) {
    return ((i64)((*a).numtype) == (i64)0);
}

void mbignum_bn_negto(struct mbignum_bignumrec * a) {
    if (!(!!(mbignum_bn_iszero(a)))) {
        (*a).neg = !((*a).neg);
    };
}

void mbignum_bn_absto(struct mbignum_bignumrec * a) {
    (*a).neg = (i64)0;
}

i64 mbignum_bn_isint(struct mbignum_bignumrec * a) {
    return ((*a).length <= ((*a).expon + (i64)1));
}

i64 mbignum_bn_getprec(struct mbignum_bignumrec * a) {
    return ((*a).length * (i64)9);
}

void mbignum_bn_setprec(struct mbignum_bignumrec * a,i64 prec) {
    i64 oldlength;
    i64 newlength;
    struct mbignum_bignumrec *  c;
    i64 av_1;
    i64 i;
    if (((i64)((*a).numtype) != (i64)1)) {
        return;
    };
    if (((prec < (i64)1) || (prec > (i64)10000000))) {
        return;
    };
    prec = ((((prec - (i64)1) / (i64)9) + (i64)1) * (i64)9);
    newlength = (prec / (i64)9);
    oldlength = (*a).length;
    if ((oldlength <= newlength)) {
        return;
    };
    c = mbignum_makebignum(newlength);
    (*c).neg = (i64)((*a).neg);
    (*c).expon = (*a).expon;
    L417 :;
    for (i=(i64)0;i<=(newlength - (i64)1);i+=(i64)1) {
L418 :;
        if ((i < oldlength)) {
            (*(*c).num)[(i)] = (i64)((*(*a).num)[(i)]);
        } else {
            (*(*c).num)[(i)] = (i64)0;
        };
L419 :;
    }L420 :;
    ;
    mbignum_bn_move(a,c);
    mbignum_bn_free(c);
}

i64 mbignum_bn_getglobalprec(void) {
    return (mbignum_currprec * (i64)9);
}

void mbignum_bn_setglobalprec(i64 prec) {
    mbignum_currprec = (((prec - (i64)1) / (i64)9) + (i64)1);
}

struct mbignum_bignumrec * mbignum_bn_makeint(i64 x) {
    struct mbignum_bignumrec *  a;
    byte str[256];
    if ((x == (i64)0)) {
        a = mbignum_makebignum((i64)0);
    } else if ((x>=(i64)0 && x<=(i64)999999999)) {
        a = mbignum_makebignum((i64)1);
        (*(*a).num)[((i64)0)] = x;
    } else if ((-(x)>=(i64)0 && -(x)<=(i64)999999999)) {
        a = mbignum_makebignum((i64)1);
        (*(*a).num)[((i64)0)] = -(x);
        (*a).neg = (i64)1;
    } else {
        sprintf((i8 *)(str),(i8 *)((byte*)"%lld"),x);
        a = mbignum_bn_makestr(str,(i64)0);
    };
    return a;
}

struct mbignum_bignumrec * mbignum_bn_makefloat(double x) {
    byte str[2048];
    sprintf((i8 *)(str),(i8 *)((byte*)"%.30g"),x);
    return mbignum_bn_makestr(str,(i64)0);
}

void mbignum_bn_ipower(struct mbignum_bignumrec * d,struct mbignum_bignumrec * a,i64 n) {
    struct mbignum_bignumrec *  e;
    struct mbignum_bignumrec *  f;
    if ((n < (i64)0)) {
        mbignum_bn_setzero(d);
    } else if ((n == (i64)0)) {
        mbignum_bn_move(d,mbignum_bn_makeint((i64)1));
    } else if ((n == (i64)1)) {
        mbignum_bn_dupl(d,a);
    } else if (((n & (i64)1) == (i64)0)) {
        e = mbignum_bn_init();
        mbignum_bn_mulu(e,a,a);
        mbignum_bn_ipower(d,e,(n / (i64)2));
        mbignum_bn_free(e);
    } else {
        e = mbignum_bn_init();
        f = mbignum_bn_init();
        mbignum_bn_mulu(e,a,a);
        mbignum_bn_ipower(f,e,((n - (i64)1) / (i64)2));
        mbignum_bn_mulu(d,a,f);
        mbignum_bn_free(e);
        mbignum_bn_free(f);
    };
}

static i64 mbignum_smallsubto(i32 * p,i32 * q,i64 plen,i64 qlen) {
    i32 *  pp;
    i32 *  qq;
    i64 carry;
    i64 diff;
    i64 z;
    i64 av_1;
    i64 av_2;
    pp = ((p + plen) - (i64)1);
    qq = ((q + qlen) - (i64)1);
    carry = (i64)0;
    z = (i64)0;
    av_1 = plen;
    while (av_1-- > 0) {
L421 :;
        if ((qq >= q)) {
            diff = (((i64)((*pp)) - (i64)((*qq))) - carry);
            --qq;
        } else {
            diff = ((i64)((*pp)) - carry);
        };
        if ((diff < (i64)0)) {
            carry = (i64)1;
            (*pp) = (diff + (i64)1000000000);
        } else {
            (*pp) = diff;
            carry = (i64)0;
        };
        if (!!((i64)((*pp)))) {
            z = (i64)0;
        } else {
            ++z;
        };
        --pp;
L422 :;
    }L423 :;
    ;
    if (!!(carry)) {
        mbignum_bn_error((byte*)"SSUBTO/CARRY?");
    };
    if ((z == plen)) {
        --z;
    };
    if (!!(z)) {
        plen -= z;
        pp = p;
        qq = (p + z);
        av_2 = plen;
        while (av_2-- > 0) {
L424 :;
            (*pp++) = (i64)((*qq++));
L425 :;
        }L426 :;
        ;
    };
    return plen;
}

static i64 mbignum_smallmulto(i32 * p,i32 * q,i64 plen,i64 m) {
    i32 *  pp;
    i32 *  qq;
    i64 carry;
    i64 d;
    i64 av_1;
    i64 av_2;
    if ((m==(i64)0)) {
        (*p) = (i64)0;
        return (i64)1;
    }else if ((m==(i64)1)) {
        memcpy((void *)(p),(void *)(q),(u64)((plen * (i64)4)));
        return plen;
    };
    pp = ((p + plen) - (i64)1);
    qq = ((q + plen) - (i64)1);
    carry = (i64)0;
    av_1 = plen;
    while (av_1-- > 0) {
L427 :;
        d = (((i64)((*qq)) * m) + carry);
        (*pp) = (d % (i64)1000000000);
        carry = (d / (i64)1000000000);
        --qq;
        --pp;
L428 :;
    }L429 :;
    ;
    if (!!(carry)) {
        pp = (p + plen);
        av_2 = plen;
        while (av_2-- > 0) {
L430 :;
            (*pp) = (i64)((*(pp - (i64)1)));
            --pp;
L431 :;
        }L432 :;
        ;
        (*pp) = carry;
        ++plen;
    };
    return plen;
}

i64 mbignum_bn_equal(struct mbignum_bignumrec * a,struct mbignum_bignumrec * b) {
    if ((((((*a).length != (*b).length) || ((i64)((*a).numtype) != (i64)((*b).numtype))) || ((i64)((*a).neg) != (i64)((*b).neg))) || ((*a).expon != (*b).expon))) {
        return (i64)0;
    };
    if (((*a).length == (i64)0)) {
        return (i64)1;
    };
    return mlib_eqbytes((void *)((*a).num),(void *)((*b).num),((*a).length * (i64)4));
}

void mbignum_bn_addu(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b) {
    i64 preca;
    i64 precb;
    i64 precc;
    i64 uppera;
    i64 upperb;
    i64 upperc;
    i64 offset;
    i64 carry;
    i64 expona;
    i64 exponb;
    i64 dc;
    u64 j;
    i32 (*pa)[];
    i32 (*pb)[];
    i32 *  c;
    i32 *  c2;
    i64 i;
    if (((*a).expon < (*b).expon)) {
        {struct mbignum_bignumrec *  temp = a; a = b; b = temp; };
    };
    expona = (*a).expon;
    exponb = (*b).expon;
    preca = (*a).length;
    precb = (*b).length;
    offset = (expona - exponb);
    uppera = (preca - (i64)1);
    upperb = (precb - (i64)1);
    if ((uppera > (upperb + offset))) {
        upperc = uppera;
    } else {
        upperc = (upperb + offset);
    };
    precc = (upperc + (i64)1);
    c = mbignum_makesmallnum(precc);
    carry = (i64)0;
    pa = (*a).num;
    pb = (*b).num;
    L433 :;
    for (i=upperc;i>=(i64)0;i-=(i64)1) {
L434 :;
        j = (u64)((i - offset));
        if (((i <= uppera) && (j <= (u64)(upperb)))) {
            dc = (((i64)((*pa)[(i)]) + (i64)((*pb)[((i64)(j))])) + carry);
        } else if ((i <= uppera)) {
            dc = ((i64)((*pa)[(i)]) + carry);
        } else if ((j <= (u64)(upperb))) {
            dc = ((i64)((*pb)[((i64)(j))]) + carry);
        } else {
            dc = carry;
        };
        if ((dc >= (i64)1000000000)) {
            carry = (i64)1;
            (*(c + i)) = (dc - (i64)1000000000);
        } else {
            (*(c + i)) = dc;
            carry = (i64)0;
        };
L435 :;
    }L436 :;
    ;
    if (!!(carry)) {
        c2 = mbignum_makesmallnum((precc + (i64)1));
        (*c2) = carry;
        memcpy((void *)((c2 + (i64)1)),(void *)(c),(u64)((precc * (i64)4)));
        mbignum_freesmall(c,precc);
        c = c2;
        ++precc;
    };
    mbignum_smalltobig(dest,c,precc,precc,(i64)0);
    (*dest).expon = (expona + carry);
}

static void mbignum_bn_subu(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b) {
    i64 preca;
    i64 precb;
    i64 precc;
    i64 uppera;
    i64 upperb;
    i64 upperc;
    i64 offset;
    i64 carry;
    i64 expona;
    i64 isneg;
    i64 diff;
    u64 j;
    i32 (*pa)[];
    i32 (*pb)[];
    i32 *  c;
    i64 i;
    isneg = (i64)0;
    if (((*a).expon < (*b).expon)) {
        {struct mbignum_bignumrec *  temp = a; a = b; b = temp; };
        isneg = (i64)1;
    };
    //retry:
L437 :;
;
    expona = (*a).expon;
    preca = (*a).length;
    precb = (*b).length;
    offset = (expona - (*b).expon);
    uppera = (preca - (i64)1);
    upperb = (precb - (i64)1);
    if ((uppera > (upperb + offset))) {
        upperc = uppera;
    } else {
        upperc = (upperb + offset);
    };
    precc = (upperc + (i64)1);
    c = mbignum_makesmallnum(precc);
    carry = (i64)0;
    pa = (*a).num;
    pb = (*b).num;
    L438 :;
    for (i=upperc;i>=(i64)0;i-=(i64)1) {
L439 :;
        j = (u64)((i - offset));
        if (((i <= uppera) && (j <= (u64)(upperb)))) {
            diff = (((i64)((*pa)[(i)]) - (i64)((*pb)[((i64)(j))])) - carry);
        } else if ((i <= uppera)) {
            diff = ((i64)((*pa)[(i)]) - carry);
        } else if ((j <= (u64)(upperb))) {
            diff = (-((i64)((*pb)[((i64)(j))])) - carry);
        } else {
            diff = -(carry);
        };
        if ((diff < (i64)0)) {
            carry = (i64)1;
            (*(c + i)) = (diff + (i64)1000000000);
        } else {
            (*(c + i)) = diff;
            carry = (i64)0;
        };
L440 :;
    }L441 :;
    ;
    if (!!(carry)) {
        if (!!(isneg)) {
            mbignum_bn_error((byte*)"SUBU/CARRY");
        };
        {struct mbignum_bignumrec *  temp = a; a = b; b = temp; };
        isneg = (i64)1;
        mbignum_freesmall(c,precc);
        goto L437 ;
;
    };
    mbignum_smalltobig(dest,c,precc,precc,(i64)0);
    (*dest).neg = isneg;
    (*dest).expon = (expona - mbignum_stblz);
    if (!!(mbignum_bn_iszero(dest))) {
        (*dest).expon = (i64)0;
    };
}

static void mbignum_bn_mulu(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b) {
    i64 uppera;
    i64 upperb;
    i64 precc;
    i64 expona;
    i64 exponb;
    i64 ax;
    i64 bx;
    i64 cx;
    i64 cx1;
    i64 nc2;
    i64 p;
    i64 carry;
    i64 x;
    i32 *  c;
    i64 pdquot;
    expona = (*a).expon;
    exponb = (*b).expon;
    uppera = ((*a).length - (i64)1);
    upperb = ((*b).length - (i64)1);
    precc = ((uppera + upperb) + (i64)2);
    nc2 = precc;
    c = mbignum_makesmallnum(nc2);
    memset((void *)(c),(i64)0,(u64)((precc * (i64)4)));
    cx = (precc - (i64)1);
    L442 :;
    for (bx=upperb;bx>=(i64)0;bx-=(i64)1) {
L443 :;
        carry = (i64)0;
        cx1 = cx;
        L446 :;
        for (ax=uppera;ax>=(i64)0;ax-=(i64)1) {
L447 :;
            p = (((i64)((*(*a).num)[(ax)]) * (i64)((*(*b).num)[(bx)])) + carry);
            pdquot = (p / (i64)1000000000);
            x = ((i64)((*(c + cx1))) + (p % (i64)1000000000));
            if ((x > (i64)999999999)) {
                carry = (pdquot + (x / (i64)1000000000));
                (*(c + cx1--)) = (x % (i64)1000000000);
            } else {
                carry = pdquot;
                (*(c + cx1--)) = x;
            };
L448 :;
        }L449 :;
        ;
        (*(c + cx1)) = carry;
        --cx;
L444 :;
    }L445 :;
    ;
    mbignum_smalltobig(dest,c,precc,nc2,(i64)0);
    (*dest).expon = (((expona + exponb) + (i64)1) - mbignum_stblz);
}

static i32 mbignum_smalldiv(i32 * x,i32 * b,i64 * xlen,i64 nb) {
    i64 k;
    i64 count;
    i64 xx;
    i64 y;
    i32 xi;
    i32 bi;
    i32 *  e;
    i64 esize;
    i64 ne;
    i64 nx;
    i64 av_1;
    i64 i;
    nx = (*xlen);
    k = (i64)0;
    count = (i64)0;
    e = mbignum_makesmallnum((esize = (nb + (i64)1)));
    L450 :;
    while (1) {
        if ((nx < nb)) {
            goto L451 ;
        } else if ((nx > nb)) {
            xx = (((i64)((*x)) * (i64)1000000000) + (i64)((*(x + (i64)1))));
            y = (xx / ((i64)((*b)) + (i64)1));
        } else {
            if (((i64)((*x)) >= ((i64)((*b)) + (i64)1))) {
                y = ((i64)((*x)) / ((i64)((*b)) + (i64)1));
            } else {
                y = (i64)1;
                L452 :;
                for (i=(i64)0;i<=(nb - (i64)1);i+=(i64)1) {
L453 :;
                    xi = (i64)((*(x + i)));
                    bi = (i64)((*(b + i)));
                    if (((i64)(xi) < (i64)(bi))) {
                        y = (i64)0;
                        goto L451 ;
                    } else if (((i64)(xi) > (i64)(bi))) {
                        goto L455 ;
                    };
L454 :;
                }L455 :;
                ;
            };
        };
        k += y;
        if ((y > (i64)1)) {
            ne = mbignum_smallmulto(e,b,nb,y);
            nx = mbignum_smallsubto(x,e,nx,ne);
        } else if (!!(y)) {
            nx = mbignum_smallsubto(x,b,nx,nb);
        } else {
            mbignum_bn_error((byte*)"smalldiv:Y=0");
        };
    }L451 :;
    ;
    mbignum_freesmall(e,esize);
    (*xlen) = nx;
    return (i32)(k);
}

void mbignum_bn_idivu(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b,struct mbignum_bignumrec * rm) {
    i32 *  c;
    i32 *  x;
    i64 expona;
    i64 exponb;
    i64 badjust;
    i64 na;
    i64 nb;
    i64 nc;
    i64 nx;
    i64 nx2;
    i64 cx;
    i64 nupper;
    i64 uppera;
    i64 upperb;
    i64 upperc;
    i64 n;
    i64 k;
    i64 nexta;
    i32 *  pa;
    i32 *  pb;
    struct mbignum_bignumrec *  d;
    i64 i;
    na = (*a).length;
    nb = (*b).length;
    expona = (*a).expon;
    exponb = (*b).expon;
    badjust = ((exponb + (i64)1) - nb);
    if (((na > (expona + (i64)1)) || (nb > (exponb + (i64)1)))) {
        mbignum_bn_error((byte*)"idivu:a or b not int");
    };
    nc = (expona + (i64)1);
    if ((expona < exponb)) {
        mbignum_bn_setzero(dest);
        if (!!(rm)) {
            mbignum_bn_dupl(rm,a);
        };
        return;
    };
    uppera = (na - (i64)1);
    upperb = (nb - (i64)1);
    upperc = (nc - (i64)1);
    pa = (i32 *)((*a).num);
    pb = (i32 *)((*b).num);
    n = nb;
    x = mbignum_makesmallnum((nx2 = (n + (i64)1)));
    nx = n;
    nupper = (nc - badjust);
    L456 :;
    for (i=(i64)0;i<=upperb;i+=(i64)1) {
L457 :;
        if ((i <= uppera)) {
            (*(x + i)) = (i64)((*(pa + i)));
        } else {
            (*(x + i)) = (i64)0;
        };
L458 :;
    }L459 :;
    ;
    c = mbignum_makesmallnum(nc);
    cx = (i64)0;
    L460 :;
    while (1) {
        k = (i64)(mbignum_smalldiv(x,pb,&nx,nb));
        (*(c + cx++)) = k;
        if ((n >= nupper)) {
            goto L461 ;
        };
        nexta = ((n > uppera)?(i64)0:(i64)((*(pa + n))));
        ++n;
        if (((nx == (i64)1) && ((i64)((*x)) == (i64)0))) {
            (*x) = nexta;
        } else {
            (*(x + nx)) = nexta;
            ++nx;
        };
    }L461 :;
    ;
    if ((!!(rm) && (exponb < nb))) {
        mbignum_smalltobig(rm,x,nx,nx2,(i64)0);
    } else {
        mbignum_freesmall(x,nx2);
    };
    if (((cx == (i64)1) && ((i64)((*c)) == (i64)0))) {
        mbignum_freesmall(c,nc);
        mbignum_bn_setzero(dest);
        if (!!(rm)) {
            mbignum_bn_dupl(rm,a);
        };
        return;
    };
    if ((((i64)((*c)) == (i64)0) && (cx >= (i64)2))) {
        mbignum_smalltobig(dest,(c + (i64)1),(cx - (i64)1),nc,(i64)-1);
    } else {
        mbignum_smalltobig(dest,c,cx,nc,(i64)0);
    };
    if ((!!(rm) && (exponb >= nb))) {
        d = mbignum_bn_init();
        mbignum_bn_mulu(d,b,dest);
        mbignum_bn_subu(rm,a,d);
        mbignum_bn_free(d);
    };
}

static i64 mbignum_strvaln(byte * s,i64 n) {
    i64 a;
    i64 av_1;
    a = (i64)0;
    av_1 = n;
    while (av_1-- > 0) {
L462 :;
        if (((u64)((*s)) != '_')) {
            a = (((a * (i64)10) + (i64)((*s))) - (i64)48);
        };
        ++s;
L463 :;
    }L464 :;
    ;
    return a;
}

struct mbignum_bignumrec * mbignum_bn_makestr(byte * s,i64 length) {
    byte *  t;
    byte *  u;
    i64 neg;
    i64 dpindex;
    i64 expon;
    i64 nonzeros;
    i64 talloc;
    i64 dpseen;
    i64 leadingzeros;
    i64 trailingzeros;
    i64 zerosafterdp;
    i64 d;
    i64 n;
    i64 wd;
    i64 dp;
    i64 wdp;
    i64 w;
    i64 d2;
    i64 na;
    i64 nb;
    struct mbignum_bignumrec *  a;
    i64 av_1;
    i64 av_2;
    i64 i;
    if ((length == (i64)0)) {
        length = (i64)(strlen((i8 *)(s)));
    };
    if ((length <= (i64)0)) {
        return mbignum_badnumber();
    };
    talloc = ((length + (i64)1) + (i64)10);
    neg = (i64)0;
    if (((i64)((*s))==(i64)43)) {
        ++s;
    }else if (((i64)((*s))==(i64)45)) {
        neg = (i64)1;
        ++s;
    };
    t = (u = (byte *)(mbignum_bn_alloc(talloc)));
    dpindex = (i64)-1;
    dpseen = (zerosafterdp = (i64)0);
    nonzeros = (i64)0;
    leadingzeros = (trailingzeros = (i64)0);
    expon = (i64)0;
    L465 :;
    switch ((i64)((*s))) {
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
        (*u++) = (u64)((*s++));
        trailingzeros = (i64)0;
        nonzeros = (i64)1;
    }break;
    case 48:;
    {
        if (!!(nonzeros)) {
            ++trailingzeros;
            (*u++) = (u64)((*s++));
        } else {
            ++leadingzeros;
            if (!!(dpseen)) {
                ++zerosafterdp;
            };
            ++s;
        };
    }break;
    case 95:;
    case 39:;
    case 96:;
    case 32:;
    case 13:;
    case 10:;
    {
        ++s;
    }break;
    case 46:;
    {
        if ((!!(dpseen) || (dpindex >= (i64)0))) {
            return mbignum_badnumber();
        };
        if (!!(nonzeros)) {
            dpindex = (u - t);
        } else {
            dpseen = (i64)1;
        };
        ++s;
    }break;
    case 0:;
    {
        goto L466 ;
    }break;
    case 101:;
    case 69:;
    {
        expon = mbignum_readexpon((s + (i64)1));
        goto L466 ;
    }break;
    default: {
        return mbignum_badnumber();
    }
    } //SW
goto L465 ;
L466 :;
    ;
    (*u) = (u64)0u;
    length = (u - t);
    if ((dpindex < (i64)0)) {
        if (!!(dpseen)) {
            dpindex = -(zerosafterdp);
        } else {
            dpindex = length;
        };
    };
    length -= trailingzeros;
    (*(t + length)) = (u64)0u;
    if ((length == (i64)0)) {
        return mbignum_bn_makeint((i64)0);
    };
    d = ((dpindex - (i64)1) + expon);
    n = length;
    dp = (i64)0;
    na = (i64)1;
    nb = (n - na);
    w = (i64)9;
    if ((d >= (i64)0)) {
        wd = (d / w);
        wdp = (d % w);
    } else {
        d2 = labs((d + (i64)1));
        wd = -(((d2 / w) + (i64)1));
        wdp = ((w - (i64)1) - (d2 % w));
    };
    na = (wdp + (i64)1);
    nb = msysnewc_m_imax((n - na),(i64)0);
    L467 :;
    while (!!((nb % w))) {
        ++nb;
L468 :;
    }L469 :;
    ;
    length = ((nb / w) + (i64)1);
    u = (t + n);
    av_1 = ((na + nb) - n);
    while (av_1-- > 0) {
L470 :;
        (*u++) = '0';
L471 :;
    }L472 :;
    ;
    n = (na + nb);
    (*(t + n)) = (u64)0u;
    a = mbignum_makebignum(length);
    (*a).neg = neg;
    (*a).expon = wd;
    u = t;
    (*(*a).num)[((i64)0)] = mbignum_strvaln(u,na);
    u += na;
    L473 :;
    for (i=(i64)1;i<=(length - (i64)1);i+=(i64)1) {
L474 :;
        (*(*a).num)[(i)] = mbignum_strvaln(u,w);
        u += w;
L475 :;
    }L476 :;
    ;
    mbignum_freemem((void *)(t),talloc);
    return a;
}

static void mbignum_bn_fdivu(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b,i64 precision) {
    i32 *  c;
    i32 *  x;
    i64 expona;
    i64 exponb;
    i64 na;
    i64 nb;
    i64 nc;
    i64 nx;
    i64 nx2;
    i64 cx;
    i64 nc2;
    i64 uppera;
    i64 upperb;
    i64 upperc;
    i64 n;
    i64 k;
    i64 nexta;
    i32 *  pa;
    i32 *  pb;
    i64 i;
    na = (*a).length;
    nb = (*b).length;
    expona = (*a).expon;
    exponb = (*b).expon;
    if (!!(precision)) {
        precision = (((precision - (i64)1) / (i64)9) + (i64)1);
    } else {
        precision = mbignum_currprec;
    };
    nc = precision;
    uppera = (na - (i64)1);
    upperb = (nb - (i64)1);
    upperc = (nc - (i64)1);
    pa = (i32 *)((*a).num);
    pb = (i32 *)((*b).num);
    n = nb;
    x = mbignum_makesmallnum((nx2 = (n + (i64)1)));
    nx = n;
    L477 :;
    for (i=(i64)0;i<=upperb;i+=(i64)1) {
L478 :;
        if ((i <= uppera)) {
            (*(x + i)) = (i64)((*(pa + i)));
        } else {
            (*(x + i)) = (i64)0;
        };
L479 :;
    }L480 :;
    ;
    c = mbignum_makesmallnum((nc2 = (nc + (i64)1)));
    cx = (i64)0;
    L481 :;
    while (1) {
        k = (i64)(mbignum_smalldiv(x,pb,&nx,nb));
        (*(c + cx++)) = k;
        if ((cx > nc)) {
            goto L482 ;
        };
        nexta = ((n > uppera)?(i64)0:(i64)((*(pa + n))));
        ++n;
        if (((nx == (i64)1) && ((i64)((*x)) == (i64)0))) {
            (*x) = nexta;
        } else {
            (*(x + nx)) = nexta;
            ++nx;
        };
    }L482 :;
    ;
    mbignum_freesmall(x,nx2);
    if (((cx == (i64)1) && ((i64)((*c)) == (i64)0))) {
        mbignum_freesmall(c,nc2);
        mbignum_bn_setzero(dest);
        return;
    };
    if ((((i64)((*c)) == (i64)0) && (cx >= (i64)2))) {
        mbignum_smalltobig(dest,(c + (i64)1),(cx - (i64)1),nc2,(i64)-1);
        (*dest).expon = ((expona - exponb) - (i64)1);
    } else {
        mbignum_smalltobig(dest,c,cx,nc2,(i64)0);
        (*dest).expon = (expona - exponb);
    };
}

static byte * mbignum_tostring_float(struct mbignum_bignumrec * a,i64 fmt) {
    i64 expon;
    i64 upper;
    i64 nchars;
    i64 w;
    i64 prel;
    i64 n;
    i64 showdot;
    byte *  s;
    byte *  t;
    i64 av_1;
    i64 av_2;
    i64 av_3;
    i64 av_4;
    i64 i;
    expon = (*a).expon;
    upper = ((*a).length - (i64)1);
    if (((fmt == (i64)73) && !!(mbignum_bn_isint(a)))) {
        showdot = (i64)0;
    } else {
        showdot = (i64)1;
    };
    w = (i64)9;
    nchars = (i64)3;
    if ((expon < (i64)0)) {
        nchars += (labs((expon - (i64)1)) * w);
    };
    nchars += ((*a).length * w);
    if (((expon - upper) > (i64)0)) {
        nchars += ((expon - upper) * w);
    };
    nchars += (i64)8;
    s = (t = (byte *)(mbignum_checkedmalloc(nchars)));
    if (!!((i64)((*a).neg))) {
        (*t++) = '-';
    };
    prel = (i64)0;
    if ((expon < (i64)0)) {
        prel = (i64)1;
        (*t++) = '0';
        (*t++) = '.';
        av_2 = (labs(expon) - (i64)1);
        while (av_2-- > 0) {
L483 :;
            av_1 = (i64)9;
            while (av_1-- > 0) {
L486 :;
                (*t++) = '0';
L487 :;
            }L488 :;
            ;
L484 :;
        }L485 :;
        ;
    };
    L489 :;
    for (i=(i64)0;i<=upper;i+=(i64)1) {
L490 :;
        n = (i64)(sprintf((i8 *)(t),(((i > (i64)0) || !!(prel))?(i8 *)((byte*)"%09lld"):(i8 *)((byte*)"%lld")),(i64)((*(*a).num)[(i)])));
        t += n;
        if ((((expon == i) && (i < upper)) && !!(showdot))) {
            (*t++) = '.';
        };
L491 :;
    }L492 :;
    ;
    av_4 = (expon - upper);
    while (av_4-- > 0) {
L493 :;
        av_3 = (i64)9;
        while (av_3-- > 0) {
L496 :;
            (*t++) = '0';
L497 :;
        }L498 :;
        ;
L494 :;
    }L495 :;
    ;
    if (((expon >= upper) && !!(showdot))) {
        (*t++) = '.';
        (*t++) = '0';
    };
    (*t) = (u64)0u;
    return s;
}

byte * mbignum_bn_tostring(struct mbignum_bignumrec * a,i64 fmt) {
    byte *  s;
    byte *  t;
    t = (byte *)(0);
    if ((a == 0)) {
        t = (byte*)"<void>";
    } else {
        if (((i64)((*a).numtype)==(i64)0)) {
            t = (((fmt == (i64)69) || (fmt == (i64)70))?(byte*)"0.0":(byte*)"0");
        }else if (((i64)((*a).numtype)==(i64)2)) {
            t = (byte*)"<inf>";
        }else if (((i64)((*a).numtype)==(i64)3)) {
            t = (byte*)"<nan>";
        };
    };
    if (!!(t)) {
        s = (byte *)(mbignum_checkedmalloc(((i64)(strlen((i8 *)(t))) + (i64)1)));
        strcpy((i8 *)(s),(i8 *)(t));
        return s;
    };
    if (((fmt == (i64)0) || (fmt == (i64)65))) {
        if ((!!(mbignum_bn_isint(a)) && ((((*a).expon - (*a).length) * (i64)9) < (i64)60))) {
            fmt = (i64)73;
        } else if ((labs(((*a).expon * (i64)9)) < (i64)60)) {
            fmt = (i64)70;
        } else {
            fmt = (i64)69;
        };
    };
    if ((fmt == (i64)69)) {
        s = mbignum_tostring_scient(a);
    } else {
        s = mbignum_tostring_float(a,fmt);
    };
    return s;
}

static byte * mbignum_tostring_scient(struct mbignum_bignumrec * a) {
    byte *  s;
    byte *  t;
    i64 expon;
    i64 nchars;
    i64 shift;
    i64 x;
    i64 scale;
    i64 av_1;
    i64 i;
    nchars = (i64)3;
    expon = ((*a).expon * (i64)9);
    x = (i64)((*(*a).num)[((i64)0)]);
    scale = (i64)1;
    shift = (i64)0;
    L499 :;
    while ((x >= (i64)10)) {
        x = (x / (i64)10);
        scale *= (i64)10;
        ++expon;
        ++shift;
L500 :;
    }L501 :;
    ;
    nchars = (((*a).length * (i64)9) + (i64)16);
    s = (t = (byte *)(mbignum_checkedmalloc(nchars)));
    if (!!((i64)((*a).neg))) {
        (*t++) = '-';
    };
    msysnewc_m_print_startstr(t);
    msysnewc_m_print_i64(x,NULL);
    msysnewc_m_print_nogap();
    msysnewc_m_print_str((byte*)".",NULL);
    msysnewc_m_print_end();
    ;
    t += (i64)(strlen((i8 *)(t)));
    if (!!(shift)) {
        msysnewc_m_print_startstr(t);
        msysnewc_m_print_i64(shift,(byte*)"v");
        msysnewc_m_print_nogap();
        msysnewc_m_print_i64(((i64)((*(*a).num)[((i64)0)]) - (x * scale)),(byte*)"z*");
        msysnewc_m_print_end();
        ;
        t += (i64)(strlen((i8 *)(t)));
    };
    L502 :;
    for (i=(i64)1;i<=((*a).length - (i64)1);i+=(i64)1) {
L503 :;
        msysnewc_m_print_startstr(t);
        msysnewc_m_print_i64((*(*a).num)[(i)],(byte*)"z9");
        msysnewc_m_print_end();
        ;
        t += (i64)(strlen((i8 *)(t)));
L504 :;
    }L505 :;
    ;
    L506 :;
    while ((((u64)((*(t - (i64)1))) == '0') && ((u64)((*(t - (i64)2))) != '.'))) {
        --t;
L507 :;
    }L508 :;
    ;
    msysnewc_m_print_startstr(t);
    msysnewc_m_print_str((byte*)"e",NULL);
    msysnewc_m_print_nogap();
    msysnewc_m_print_i64(expon,NULL);
    msysnewc_m_print_end();
    ;
    t += (i64)(strlen((i8 *)(t)));
    (*t) = (u64)0u;
    return s;
}

i64 mbignum_bn_add(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b) {
    i64 nega;
    i64 negb;
    switch (mbignum_getbintype(a,b)) {
    case 1:;
    {
    }break;
    case 2:;
    {
        mbignum_bn_setzero(dest);
        return (i64)1;
    }break;
    case 5:;
    {
        mbignum_bn_dupl(dest,a);
        return (i64)1;
    }break;
    case 7:;
    {
        mbignum_bn_dupl(dest,b);
        return (i64)1;
    }break;
    default: {
        mbignum_bn_setnan(dest);
        return (i64)0;
    }
    } //SW
;
    nega = (i64)((*a).neg);
    negb = (i64)((*b).neg);
    if ((!(!!(nega)) && !(!!(negb)))) {
        mbignum_bn_addu(dest,a,b);
    } else if ((!!(nega) && !!(negb))) {
        mbignum_bn_addu(dest,a,b);
        mbignum_bn_negto(dest);
    } else if ((!(!!(nega)) && !!(negb))) {
        mbignum_bn_subu(dest,a,b);
    } else {
        mbignum_bn_subu(dest,b,a);
    };
    return (i64)1;
}

i64 mbignum_bn_sub(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b) {
    i64 nega;
    i64 negb;
    switch (mbignum_getbintype(a,b)) {
    case 1:;
    {
    }break;
    case 2:;
    {
        mbignum_bn_setzero(dest);
        return (i64)1;
    }break;
    case 5:;
    {
        mbignum_bn_dupl(dest,a);
        return (i64)1;
    }break;
    case 7:;
    {
        mbignum_bn_dupl(dest,b);
        mbignum_bn_negto(dest);
        return (i64)1;
    }break;
    default: {
        mbignum_bn_setnan(dest);
        return (i64)0;
    }
    } //SW
;
    nega = (i64)((*a).neg);
    negb = (i64)((*b).neg);
    if ((!(!!(nega)) && !(!!(negb)))) {
        mbignum_bn_subu(dest,a,b);
    } else if ((!!(nega) && !!(negb))) {
        mbignum_bn_subu(dest,b,a);
    } else if ((!(!!(nega)) && !!(negb))) {
        mbignum_bn_addu(dest,a,b);
    } else {
        mbignum_bn_subu(dest,b,a);
    };
    return (i64)1;
}

i64 mbignum_bn_mul(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b) {
    i64 neg;
    switch (mbignum_getbintype(a,b)) {
    case 1:;
    {
    }break;
    case 2:;
    case 5:;
    case 7:;
    {
        mbignum_bn_setzero(dest);
        return (i64)1;
    }break;
    default: {
        mbignum_bn_setnan(dest);
        return (i64)0;
    }
    } //SW
;
    neg = ((i64)((*a).neg) != (i64)((*b).neg));
    mbignum_bn_mulu(dest,a,b);
    if (!!(neg)) {
        mbignum_bn_negto(dest);
    };
    return (i64)1;
}

i64 mbignum_bn_mulp(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b,i64 prec) {
    i64 res;
    res = mbignum_bn_mul(dest,a,b);
    if (!!(res)) {
        mbignum_bn_setprec(dest,((prec == (i64)0)?mbignum_currprec:prec));
    };
    return res;
}

i64 mbignum_bn_div(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b,i64 prec) {
    i64 neg;
    switch (mbignum_getbintype(a,b)) {
    case 1:;
    {
    }break;
    case 7:;
    {
        mbignum_bn_setzero(dest);
        return (i64)1;
    }break;
    case 2:;
    case 5:;
    {
        mbignum_bn_setinf(dest);
        return (i64)0;
    }break;
    default: {
        mbignum_bn_setnan(dest);
        return (i64)0;
    }
    } //SW
;
    neg = ((i64)((*a).neg) != (i64)((*b).neg));
    mbignum_bn_fdivu(dest,a,b,prec);
    if (!!(neg)) {
        mbignum_bn_negto(dest);
    };
    return (i64)1;
}

i64 mbignum_bn_idiv(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b) {
    i64 neg;
    switch (mbignum_getbintype(a,b)) {
    case 1:;
    {
    }break;
    case 7:;
    {
        mbignum_bn_setzero(dest);
        return (i64)1;
    }break;
    case 2:;
    case 5:;
    {
        mbignum_bn_setinf(dest);
        return (i64)0;
    }break;
    default: {
        mbignum_bn_setnan(dest);
        return (i64)0;
    }
    } //SW
;
    neg = ((i64)((*a).neg) != (i64)((*b).neg));
    mbignum_bn_idivu(dest,a,b,(struct mbignum_bignumrec *)(0));
    if (!!(neg)) {
        mbignum_bn_negto(dest);
    };
    return (i64)1;
}

i64 mbignum_bn_idivrem(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * rm,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b) {
    i64 nega;
    i64 negb;
    switch (mbignum_getbintype(a,b)) {
    case 1:;
    {
    }break;
    case 7:;
    {
        mbignum_bn_setzero(dest);
        mbignum_bn_setzero(rm);
        return (i64)1;
    }break;
    case 2:;
    case 5:;
    {
        mbignum_bn_setinf(dest);
        mbignum_bn_setzero(rm);
        return (i64)0;
    }break;
    default: {
        mbignum_bn_setnan(dest);
        return (i64)0;
    }
    } //SW
;
    nega = (i64)((*a).neg);
    negb = (i64)((*b).neg);
    mbignum_bn_idivu(dest,a,b,rm);
    if ((nega != negb)) {
        mbignum_bn_negto(dest);
    };
    if (!!(nega)) {
        mbignum_bn_negto(rm);
    };
    return (i64)1;
}

i64 mbignum_bn_irem(struct mbignum_bignumrec * dest,struct mbignum_bignumrec * a,struct mbignum_bignumrec * b) {
    struct mbignum_bignumrec *  d;
    i64 nega;
    switch (mbignum_getbintype(a,b)) {
    case 1:;
    {
    }break;
    case 7:;
    {
        mbignum_bn_dupl(dest,b);
        return (i64)1;
    }break;
    case 2:;
    case 5:;
    {
        mbignum_bn_setinf(dest);
        mbignum_bn_setzero(dest);
        return (i64)0;
    }break;
    default: {
        mbignum_bn_setnan(dest);
        return (i64)0;
    }
    } //SW
;
    nega = (i64)((*a).neg);
    d = mbignum_bn_init();
    mbignum_bn_idivu(d,a,b,dest);
    if (!!(nega)) {
        mbignum_bn_negto(dest);
    };
    mbignum_bn_free(d);
    return (i64)1;
}

i64 mbignum_bn_cmp(struct mbignum_bignumrec * a,struct mbignum_bignumrec * b) {
    struct mbignum_bignumrec *  d;
    i64 neg;
    if (!!(mbignum_bn_equal(a,b))) {
        return (i64)0;
    };
    d = mbignum_bn_init();
    mbignum_bn_sub(d,a,b);
    neg = (i64)((*d).neg);
    mbignum_bn_free(d);
    return (!!(neg)?(i64)-1:(i64)1);
}

struct mbignum_bignumrec * mbignum_bn_const(i64 value) {
    struct mbignum_constrec *  p;
    p = mbignum_constlist;
    L509 :;
    while (!!(p)) {
        if (((*p).value == value)) {
            return (*p).bnvalue;
        };
        p = (*p).nextconst;
L510 :;
    }L511 :;
    ;
    p = (struct mbignum_constrec *)(mbignum_bn_alloc((i64)24));
    (*p).bnvalue = mbignum_bn_makeint(value);
    (*p).value = value;
    (*p).nextconst = mbignum_constlist;
    mbignum_constlist = p;
    return (*p).bnvalue;
}

i64 mbignum_bn_sign(struct mbignum_bignumrec * a) {
    if (!!(mbignum_bn_iszero(a))) {
        return (i64)0;
    } else if (!!((i64)((*a).neg))) {
        return (i64)-1;
    } else {
        return (i64)0;
    };
}

static struct mbignum_bignumrec * mbignum_badnumber(void) {
    struct mbignum_bignumrec *  c;
    c = mbignum_makebignum((i64)0);
    (*c).numtype = (i64)3;
    return c;
}

i64 mbignum_bn_digits(struct mbignum_bignumrec * a) {
    i64 n;
    byte str[32];
    if (!(!!(mbignum_bn_isint(a)))) {
        return (i64)0;
    };
    if (!!(mbignum_bn_iszero(a))) {
        return (i64)1;
    };
    n = (i64)(sprintf((i8 *)(str),(i8 *)((byte*)"%lld"),(i64)((*(*a).num)[((i64)0)])));
    return (n + ((*a).expon * (i64)9));
}

i64 mbignum_bn_toint(struct mbignum_bignumrec * a) {
    i64 x;
    i64 av_1;
    i64 i;
    if (!(!!(mbignum_bn_isint(a)))) {
        return (i64)0;
    };
    if (!!(mbignum_bn_iszero(a))) {
        return (i64)0;
    };
    x = (i64)0;
    L512 :;
    for (i=(i64)0;i<=((*a).length - (i64)1);i+=(i64)1) {
L513 :;
        x = ((x * (i64)1000000000) + (i64)((*(*a).num)[(i)]));
L514 :;
    }L515 :;
    ;
    if (!!((i64)((*a).neg))) {
        return -(x);
    } else {
        return x;
    };
}

double mbignum_bn_tofloat(struct mbignum_bignumrec * a) {
    double x;
    byte *  s;
    if (!!(mbignum_bn_iszero(a))) {
        return (double)0.;
    };
    s = mbignum_bn_tostring(a,(i64)69);
    sscanf((i8 *)(s),(i8 *)((byte*)"%lf"),&x);
    return x;
}

void mbignum_bn_fix(struct mbignum_bignumrec * c,struct mbignum_bignumrec * a) {
    if ((!!(mbignum_bn_iszero(a)) || ((*a).expon < (i64)0))) {
        mbignum_bn_setzero(c);
        return;
    };
    mbignum_bn_dupl(c,a);
    if (!(!!(mbignum_bn_isint(c)))) {
        mbignum_bn_setprec(c,((*c).expon + (i64)1));
    };
}

void mbignum_bntest(struct mbignum_bignumrec * a) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"BNTEST",NULL);
    msysnewc_m_print_str((byte*)"A.NUMTYPE=",NULL);
    msysnewc_m_print_i64((*a).numtype,NULL);
    msysnewc_m_print_str((byte*)"A.LENGTH=",NULL);
    msysnewc_m_print_i64((*a).length,NULL);
    msysnewc_m_print_str((byte*)"A.EXPON=",NULL);
    msysnewc_m_print_i64((*a).expon,NULL);
    msysnewc_m_print_str((byte*)"A.NEG=",NULL);
    msysnewc_m_print_i64((*a).neg,NULL);
    msysnewc_m_print_str((byte*)"A.NUM=",NULL);
    msysnewc_m_print_ptr((*a).num,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

void pc_print_pch_print(struct pc_decls_varrec * p,struct pc_decls_varrec * fmt) {
    struct pc_decls_varrec v;
    struct pc_decls_varrec emptyfmt;
    if ((fmt == 0)) {
        fmt = &emptyfmt;
        emptyfmt.tagx = (u64)((i64)0);
    };
    if ((pc_print_mfmtstr == 0)) {
        if (!!((u64)(pc_print_mgapneeded))) {
            pc_print_printstr_n((byte*)" ",(i64)1);
        } else {
            pc_print_mgapneeded = (u64)((i64)1);
        };
    } else {
        pc_print_printnextfmtchars((i64)0);
    };
    switch ((i64)(pc_decls_ttbasetype[((i64)((*p).tag))])) {
    case 5:;
    {
        if (((fmt == 0) || ((i64)((u64)((*fmt).tag)) == (i64)0))) {
            pc_print_printstr_n((*(*p).objptr).ustr.strptr,(i64)((*(*p).objptr).ustr.length));
            return;
        };
    }break;
    case 1:;
    case 3:;
    case 4:;
    case 2:;
    {
        pc_print_pch_tostr(p,fmt,&v);
        pc_print_printstr_n((*v.objptr).ustr.strptr,(i64)((*v.objptr).ustr.length));
        pc_pcfns_pc_unshare(&v);
        return;
    }break;
    default: {
    }
    } //SW
;
    pc_print_pch_tostr(p,fmt,&v);
    pc_print_printstr_n((*v.objptr).ustr.strptr,(i64)((*v.objptr).ustr.length));
    pc_pcfns_pc_unshare(&v);
}

void pc_print_pch_println(void) {
    if (!!(pc_print_mfmtstr)) {
        pc_print_printnextfmtchars((i64)1);
    };
    pc_print_printstrz((byte*)"\r\n");
}

void pc_print_pch_startprintcon(void) {
    struct pc_decls_varrec v;
    v.tagx = (u64)((i64)1);
    v.value = (i64)0;
    pc_print_pch_startprint(&v);
}

void pc_print_pch_startprint(struct pc_decls_varrec * p) {
    struct pc_decls_objrec *  s;
    switch (++pc_print_noclevels) {
    case 0:;
    case 1:;
    {
    }break;
    case 7:;
    {
        pc_print_printerror((byte*)"print #x overflow");
    }break;
    default: {
        pc_print_moutdevstack[((pc_print_noclevels - (i64)1))] = pc_print_moutdev;
        pc_print_moutchanstack[((pc_print_noclevels - (i64)1))] = pc_print_moutchan;
        pc_print_moutvarstack[((pc_print_noclevels - (i64)1))] = pc_print_moutvar;
        pc_print_mfmtstrstack[((pc_print_noclevels - (i64)1))] = pc_print_mfmtstr;
        pc_print_mfmtcurrstack[((pc_print_noclevels - (i64)1))] = pc_print_mfmtcurr;
        pc_print_mgapstack[((pc_print_noclevels - (i64)1))] = (u64)(pc_print_mgapneeded);
    }
    } //SW
;
    pc_print_mfmtstr = (byte *)(0);
    pc_print_mfmtcurr = (byte *)(0);
    if ((p == 0)) {
        goto L516 ;
;
    };
    switch ((i64)((*p).tag)) {
    case 1:;
    {
        switch ((*p).value) {
        case 0:;
        {
            //doconsole:
L516 :;
;
            pc_print_moutdev = (i64)0;
            pc_print_moutchan = 0;
        }break;
        case 1:;
        {
            pc_print_moutdev = (i64)2;
            pc_print_moutchan = 0;
            pc_print_moutvar.tagx = (u64)((i64)65541);
            s = pc_objlib_obj_new((i64)5);
            (*s).ustr.mutable = (u64)((i64)1);
            pc_print_moutvar.objptr = s;
        }break;
        case 2:;
        {
            if ((pc_print_testfilech == 0)) {
                pc_support_prterror((byte*)"@2: file not open");
            };
            pc_print_moutdev = (i64)1;
            pc_print_moutchan = pc_print_testfilech;
        }break;
        default: {
            pc_print_moutdev = (i64)1;
            pc_print_moutchan = (void *)((*p).value);
        }
        } //SW
;
    }break;
    case 22:;
    {
        p = (*p).varptr;
        switch ((i64)((*p).tag)) {
        case 5:;
        {
            pc_print_moutdev = (i64)4;
            pc_print_moutchan = 0;
            pc_print_moutvar.tagx = (u64)((i64)22);
            pc_print_moutvar.varptr = p;
        }break;
        default: {
            msysnewc_m_print_startcon();
            msysnewc_m_print_str(pc_decls_ttname[((i64)((*p).tag))],NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            pc_support_prterror((byte*)"Print@^?");
        }
        } //SW
;
    }break;
    default: {
        switch ((i64)(pc_decls_ttbasetype[((i64)((*p).tag))])) {
        case 32:;
        case 33:;
        {
            pc_print_moutdev = (i64)0;
        }break;
        default: {
            msysnewc_m_print_startcon();
            msysnewc_m_print_str(pc_decls_ttname[((i64)((*p).tag))],NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            pc_print_printerror((byte*)"Can't do startprint...");
        }
        } //SW
;
    }
    } //SW
;
    pc_print_mgapneeded = (u64)((i64)0);
}

void pc_print_pch_endprint(void) {
    struct pc_decls_varrec *  p;
    if (!!(pc_print_mfmtstr)) {
        pc_print_printnextfmtchars((i64)1);
    };
    switch (pc_print_moutdev) {
    case 4:;
    {
        p = pc_print_moutvar.varptr;
    }break;
    default: {
    }
    } //SW
;
    if ((pc_print_mfmtstr != 0)) {
        mlib_pcm_free((void *)(pc_print_mfmtstr),((i64)(strlen((i8 *)(pc_print_mfmtstr))) + (i64)1));
    };
    if ((--pc_print_noclevels == (i64)-1)) {
        pc_print_printerror((byte*)"resetoc??");
    };
    if ((pc_print_noclevels == (i64)0)) {
        pc_print_moutdev = (i64)0;
    } else {
        pc_print_moutdev = (i64)(pc_print_moutdevstack[(pc_print_noclevels)]);
        pc_print_moutchan = pc_print_moutchanstack[(pc_print_noclevels)];
        pc_print_moutvar = pc_print_moutvarstack[(pc_print_noclevels)];
        pc_print_mgapneeded = (u64)(pc_print_mgapstack[(pc_print_noclevels)]);
        pc_print_mfmtstr = pc_print_mfmtstrstack[(pc_print_noclevels)];
        pc_print_mfmtcurr = pc_print_mfmtcurrstack[(pc_print_noclevels)];
    };
}

void pc_print_pch_strstartprint(void) {
    struct pc_decls_varrec p;
    p.tagx = (u64)((i64)1);
    p.value = (i64)1;
    pc_print_pch_startprint(&p);
}

void pc_print_pch_strendprint(struct pc_decls_varrec * dest) {
    if (!!(pc_print_mfmtstr)) {
        pc_print_printnextfmtchars((i64)1);
    };
    if ((pc_print_moutdev != (i64)2)) {
        pc_support_prterror((byte*)"STRendPRT/NOT STR");
    };
    (*dest) = pc_print_moutvar;
    pc_print_moutvar.tagx = (u64)((i64)0);
    pc_print_pch_endprint();
}

void pc_print_pch_setformat(struct pc_decls_varrec * p) {
    i64 n;
    byte *  s;
    if (((i64)((u64)((*p).tag)) != (i64)5)) {
        pc_support_prterror((byte*)"(str)");
    };
    if (!!(pc_print_mfmtstr)) {
        pc_support_prterror((byte*)"Setfmt?");
    };
    n = (i64)((*(*p).objptr).ustr.length);
    pc_print_mfmtstr = (byte *)(mlib_pcm_alloc((n + (i64)1)));
    if (!!(n)) {
        memcpy((void *)(pc_print_mfmtstr),(void *)((*(*p).objptr).ustr.strptr),(u64)(n));
    };
    s = (pc_print_mfmtstr + n);
    (*s) = (u64)0u;
    pc_print_mfmtcurr = pc_print_mfmtstr;
}

void pc_print_pch_setformat2(struct pc_decls_varrec * p) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"PC/SETFORMAT2",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

void pc_print_pch_dprint(struct pc_decls_varrec * p,struct pc_decls_varrec * fmt) {
    pc_print_pch_print(p,fmt);
    switch ((i64)((*p).tag)) {
    case 1:;
    {
        pc_print_printstrz((byte*)"d");
    }break;
    case 2:;
    {
        pc_print_printstrz((byte*)"u");
    }break;
    default: {
    }
    } //SW
;
}

void pc_print_pch_printnogap(void) {
    pc_print_mgapneeded = (u64)((i64)0);
}

static void pc_print_initfmtcode(struct pc_decls_fmtrec * f) {
    (*f) = pc_print_defaultfmt;
}

static i64 pc_print_i64mintostr(byte * s,i64 base,i64 sep) {
    byte t[1024];
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
    L517 :;
    while (!!(i)) {
        --s;
        (*s) = (u64)(t[((i-- - (i64)1))]);
        if (((!!(sep) && !!(i)) && (++k == g))) {
            --s;
            (*s) = (u64)(sep);
            k = (i64)0;
        };
L518 :;
    }L519 :;
    ;
    return (i64)(strlen((i8 *)(s)));
}

static i64 pc_print_u64tostr(u64 aa,byte * s,u64 base,i64 sep) {
    byte t[1024];
    i64 i;
    i64 j;
    i64 k;
    i64 g;
    byte *  s0;
    i = (i64)0;
    k = (i64)0;
    g = (((i64)(base) == (i64)10)?(i64)3:(i64)4);
    L520 :;
    do {
        t[(++i)] = (u64)(pc_print_digits[((i64)((aa % base)))]);
        aa = (aa / base);
        if (((!!(sep) && ((i64)(aa) != (i64)0)) && (++k == g))) {
            t[(++i)] = (u64)(sep);
            k = (i64)0;
        };
L521 :;
    } while (!((i64)(aa) == (i64)0));L522 :;
    ;
    j = i;
    s0 = s;
    L523 :;
    while (!!(i)) {
        (*s) = (u64)(t[(i--)]);
        ++s;
L524 :;
    }L525 :;
    ;
    (*s) = (u64)0u;
    return j;
}

static i64 pc_print_i64tostrfmt(i64 aa,byte * s,struct pc_decls_fmtrec * fmt,i64 usigned) {
    byte str[1024];
    i64 n;
    static u64 mindint = (u64)9223372036854775808u;
    if (!!((u64)((*fmt).usigned))) {
        usigned = (i64)1;
    };
    if (((aa == (i64)(mindint)) && !(!!(usigned)))) {
        str[((i64)0)] = '-';
        n = (pc_print_i64mintostr(&str[((i64)1)],(i64)((*fmt).base),(i64)((*fmt).sepchar)) + (i64)1);
    } else {
        if (((!(!!(usigned)) && (aa < (i64)0)) || !!((u64)((*fmt).plus)))) {
            if ((aa < (i64)0)) {
                aa = -(aa);
                str[((i64)0)] = '-';
            } else {
                str[((i64)0)] = '+';
            };
            n = (pc_print_u64tostr((u64)(aa),&str[((i64)1)],(u64)((*fmt).base),(i64)((*fmt).sepchar)) + (i64)1);
        } else {
            n = pc_print_u64tostr((u64)(aa),str,(u64)((*fmt).base),(i64)((*fmt).sepchar));
        };
    };
    if (!!((u64)((*fmt).suffix))) {
        str[(n)] = (u64)((*fmt).suffix);
        str[(++n)] = (u64)0u;
    };
    if ((((i64)((u64)((*fmt).base)) > (i64)10) || (!!((u64)((*fmt).suffix)) && ((u64)((*fmt).lettercase) == 'a')))) {
        mlib_convlcstring(str);
    };
    return pc_print_expandstr(str,s,n,fmt);
}

static i64 pc_print_u64tostrfmt(i64 aa,byte * s,struct pc_decls_fmtrec * fmt) {
    byte str[1024];
    i64 n;
    n = pc_print_u64tostr((u64)(aa),str,(u64)((*fmt).base),(i64)((*fmt).sepchar));
    if (!!((u64)((*fmt).suffix))) {
        str[(n)] = (u64)((*fmt).suffix);
        str[(++n)] = (u64)0u;
    };
    if ((((i64)((u64)((*fmt).base)) > (i64)10) || (!!((u64)((*fmt).suffix)) && ((u64)((*fmt).lettercase) == 'a')))) {
        mlib_convlcstring(str);
    };
    return pc_print_expandstr(str,s,n,fmt);
}

static i64 pc_print_strtostrfmt(byte * s,byte * t,i64 n,struct pc_decls_fmtrec * fmt) {
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
            mlib_convlcstring(u);
        }break;
        case 65:;
        {
            mlib_convucstring(u);
        }break;
        default: {
        }
        } //SW
;
        s = u;
    };
    w = (i64)((*fmt).minwidth);
    if ((w > n)) {
        n = pc_print_expandstr(s,t,n,fmt);
    } else {
        memcpy((void *)(t),(void *)(s),(u64)(n));
    };
    if (!!(nheap)) {
        mlib_pcm_free((void *)(u),nheap);
    };
    return n;
}

static i64 pc_print_expandstr(byte * s,byte * t,i64 n,struct pc_decls_fmtrec * fmt) {
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
        L526 :;
        for (i=(i64)1;i<=(w - n);i+=(i64)1) {
L527 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L528 :;
        }L529 :;
        ;
        (*t) = (u64)0u;
    } else if (((u64)((*fmt).justify) == 'R')) {
        if (((((u64)((*fmt).padchar) == '0') && !!((u64)((*fmt).base))) && (((u64)((*s)) == '-') || ((u64)((*s)) == '+')))) {
            (*t) = (u64)((*s));
            ++t;
            av_2 = (w - n);
            while (av_2-- > 0) {
L530 :;
                (*t) = (u64)((*fmt).padchar);
                ++t;
L531 :;
            }L532 :;
            ;
            strncpy((i8 *)(t),(i8 *)((s + (i64)1)),(u64)((n - (i64)1)));
            (*((t + n) - (i64)1)) = (u64)0u;
        } else {
            av_3 = (w - n);
            while (av_3-- > 0) {
L533 :;
                (*t) = (u64)((*fmt).padchar);
                ++t;
L534 :;
            }L535 :;
            ;
            strncpy((i8 *)(t),(i8 *)(s),(u64)(n));
            (*(t + n)) = (u64)0u;
        };
    } else {
        m = (((w - n) + (i64)1) / (i64)2);
        av_4 = m;
        while (av_4-- > 0) {
L536 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L537 :;
        }L538 :;
        ;
        strncpy((i8 *)(t),(i8 *)(s),(u64)(n));
        t += n;
        av_5 = ((w - n) - m);
        while (av_5-- > 0) {
L539 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L540 :;
        }L541 :;
        ;
        (*t) = (u64)0u;
    };
    return w;
}

void pc_print_pc_strtofmt(byte * s,i64 slen,struct pc_decls_fmtrec * fmt) {
    i64 c;
    byte wset;
    i64 n;
    byte str[100];
    pc_print_initfmtcode(fmt);
    memcpy((void *)(str),(void *)(s),(u64)(slen));
    str[(slen)] = (u64)0u;
    s = str;
    wset = (u64)((i64)0);
    L542 :;
    while (!!((u64)((*s)))) {
        c = (i64)((*s));
        ++s;
        switch (c) {
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
            c = (i64)((*s));
            if (!!(c)) {
                switch (c) {
                case 50:;
                case 51:;
                case 52:;
                case 53:;
                case 54:;
                case 55:;
                case 56:;
                case 57:;
                {
                    c = (c - (i64)48);
                }break;
                case 49:;
                {
                    ++s;
                    c = (i64)((*s));
                    if ((c>=(u64)48u && c<=(u64)54u)) {
                        c = ((c - (i64)48) + (i64)10);
                    };
                }break;
                default: {
                    c = (i64)10;
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
        case 85:;
        case 117:;
        {
            (*fmt).usigned = 'U';
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
        case 77:;
        case 109:;
        {
            (*fmt).charmode = 'M';
        }break;
        case 67:;
        case 99:;
        {
            (*fmt).charmode = 'C';
        }break;
        case 89:;
        case 121:;
        {
            (*fmt).showtype = 'Y';
        }break;
        default: {
            if (((c >= (i64)48) && (c <= (i64)57))) {
                n = (c - (i64)48);
                L545 :;
                while (1) {
                    c = (i64)((*s));
                    if (((i64)((*s)) == (i64)0)) {
                        goto L546 ;
                    };
                    if (((c >= (i64)48) && (c <= (i64)57))) {
                        ++s;
                        n = (((n * (i64)10) + c) - (i64)48);
                    } else {
                        goto L546 ;
                    };
                }L546 :;
                ;
                if (!(!!((u64)(wset)))) {
                    (*fmt).minwidth = (u64)((n<(i64)1023?n:(i64)1023));
                    wset = (u64)((i64)1);
                } else {
                    (*fmt).precision = (n<(i64)100?n:(i64)100);
                };
            };
        }
        } //SW
;
L543 :;
    }L544 :;
    ;
}

static void pc_print_printstrz(byte * s) {
    switch (pc_print_moutdev) {
    case 0:;
    {
        printf((i8 *)((byte*)"%s"),s);
    }break;
    case 1:;
    {
        fprintf(pc_print_moutchan,(i8 *)((byte*)"%s"),s);
    }break;
    case 2:;
    {
        pc_print_addstring(pc_print_moutvar.objptr,s,(i64)-1);
    }break;
    case 4:;
    {
        pc_print_printstr_n(s,(i64)(strlen((i8 *)(s))));
    }break;
    case 3:;
    {
    }break;
    default: {
    }
    } //SW
;
}

static void pc_print_printstr_n(byte * s,i64 n) {
    struct pc_decls_varrec *  p;
    if ((n == (i64)-1)) {
        n = (i64)(strlen((i8 *)(s)));
    };
    if ((n == (i64)0)) {
        return;
    };
    switch (pc_print_moutdev) {
    case 0:;
    {
        msysnewc_printstrn_app(s,n,0);
    }break;
    case 1:;
    {
        msysnewc_printstrn_app(s,n,pc_print_moutchan);
    }break;
    case 2:;
    {
        pc_print_addstring(pc_print_moutvar.objptr,s,n);
    }break;
    case 4:;
    {
        p = pc_print_moutvar.varptr;
        if (((i64)((u64)((*p).tag)) != (i64)5)) {
            pc_support_prterror((byte*)"prtstrn1");
        };
        pc_print_addstring(pc_print_moutvar.objptr,s,n);
    }break;
    case 3:;
    {
    }break;
    default: {
    }
    } //SW
;
}

void pc_print_printerror(byte * s) {
    pc_support_prterror(s);
}

void pc_print_addstring(struct pc_decls_objrec * p,byte * t,i64 n) {
    i64 oldlen;
    i64 newlen;
    i64 oldbytes;
    i64 newbytes;
    byte *  newptr;
    if (((n == (i64)0) || ((i64)((*t)) == (i64)0))) {
        return;
    };
    if ((n < (i64)0)) {
        n = (i64)(strlen((i8 *)(t)));
    };
    oldlen = (i64)((*p).ustr.length);
    if (((i64)((u64)((*p).refcount)) == (i64)0)) {
        if ((oldlen == (i64)0)) {
            memcpy((void *)((*p).ustr.strptr),(void *)(t),(u64)(n));
            (*p).ustr.length = n;
        } else {
            memcpy((void *)(((*p).ustr.strptr + oldlen)),(void *)(t),(u64)(n));
            (*p).ustr.length = (oldlen + n);
        };
        return;
    };
    if ((oldlen == (i64)0)) {
        (*p).ustr.strptr = (byte *)(mlib_pcm_alloc(n));
        (*p).ustr.length = n;
        (*p).ustr.allocated = (u64)(mlib_allocbytes);
        memcpy((void *)((*p).ustr.strptr),(void *)(t),(u64)(n));
    } else {
        newlen = (oldlen + n);
        oldbytes = (i64)((*p).ustr.allocated);
        newbytes = (oldlen + n);
        if ((newbytes <= oldbytes)) {
            memcpy((void *)(((*p).ustr.strptr + oldlen)),(void *)(t),(u64)(n));
        } else {
            newptr = (byte *)(mlib_pcm_alloc(newbytes));
            memcpy((void *)(newptr),(void *)((*p).ustr.strptr),(u64)(oldlen));
            memcpy((void *)((newptr + oldlen)),(void *)(t),(u64)(n));
            (*p).ustr.allocated = (u64)(mlib_allocbytes);
            mlib_pcm_free((void *)((*p).ustr.strptr),oldbytes);
            (*p).ustr.strptr = newptr;
        };
        (*p).ustr.length = newlen;
    };
}

void pc_print_j_tostr_i(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest) {
    byte str[1024];
    switch ((i64)((*fmt).charmode)) {
    case 77:;
    {
        pc_print_domultichar((byte *)(&(*p).value),(i64)8,str,fmt);
    }break;
    case 67:;
    {
        str[((i64)1)] = (u64)((*p).value);
        str[((i64)2)] = (u64)0u;
    }break;
    default: {
        pc_print_i64tostrfmt((*p).value,str,fmt,(i64)0);
    }
    } //SW
;
    if (!!((u64)((*fmt).showtype))) {
        pc_print_addstring(dest,(byte*)"I:",(i64)2);
    };
    pc_print_addstring(dest,str,(i64)(strlen((i8 *)(str))));
}

void pc_print_j_tostr_r(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest) {
    byte str[1024];
    byte str2[1024];
    byte cfmt[10];
    i64 n;
    cfmt[((i64)0)] = '%';
    if (!!((i64)((*fmt).precision))) {
        cfmt[((i64)1)] = '.';
        cfmt[((i64)2)] = '*';
        cfmt[((i64)3)] = (u64)((*fmt).realfmt);
        cfmt[((i64)4)] = (u64)0u;
        sprintf((i8 *)(str),(i8 *)(cfmt),(i64)((*fmt).precision),(*p).xvalue);
    } else {
        cfmt[((i64)1)] = (u64)((*fmt).realfmt);
        cfmt[((i64)2)] = (u64)0u;
        sprintf((i8 *)(str),(i8 *)(cfmt),(*p).xvalue);
    };
    n = (i64)(strlen((i8 *)(str)));
    if ((n < (i64)((u64)((*fmt).minwidth)))) {
        pc_print_expandstr(str,str2,n,fmt);
        strcpy((i8 *)(str),(i8 *)(str2));
    };
    pc_print_addstring(dest,str,(i64)(strlen((i8 *)(str))));
}

void pc_print_j_tostr_w(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest) {
    byte str[1024];
    switch ((i64)((*fmt).charmode)) {
    case 77:;
    {
        pc_print_domultichar((byte *)(&(*p).uvalue),(i64)8,str,fmt);
    }break;
    case 67:;
    {
        str[((i64)1)] = (u64)((*p).uvalue);
        str[((i64)2)] = (u64)0u;
    }break;
    default: {
        pc_print_u64tostrfmt((*p).value,str,fmt);
    }
    } //SW
;
    if (!!((u64)((*fmt).showtype))) {
        pc_print_addstring(dest,(byte*)"W:",(i64)2);
    };
    pc_print_addstring(dest,str,(i64)(strlen((i8 *)(str))));
}

void pc_print_j_tostr_n(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest) {
    byte str[1024];
    pc_print_i64tostrfmt((i64)((*p).range_lower),str,fmt,(i64)0);
    strcat((i8 *)(str),(i8 *)((byte*)".."));
    pc_print_addstring(dest,str,(i64)-1);
    pc_print_i64tostrfmt((i64)((*p).range_upper),str,fmt,(i64)0);
    pc_print_addstring(dest,str,(i64)-1);
}

void pc_print_j_tostr_s(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest) {
    i64 oldlen;
    i64 newlen;
    byte *  s;
    struct pc_decls_objrec *  q;
    q = (*p).objptr;
    oldlen = (i64)((*q).ustr.length);
    newlen = oldlen;
    if ((!!((u64)((*fmt).quotechar)) || ((i64)((u64)((*fmt).minwidth)) > newlen))) {
        if (!!((u64)((*fmt).quotechar))) {
            newlen += (i64)2;
        };
        if (((i64)((u64)((*fmt).minwidth)) > newlen)) {
            newlen = (i64)((*fmt).minwidth);
        };
        s = (byte *)(mlib_pcm_alloc((newlen + (i64)1)));
        pc_print_strtostrfmt((*q).ustr.strptr,s,oldlen,fmt);
        pc_print_addstring(dest,s,newlen);
        mlib_pcm_free((void *)(s),(newlen + (i64)1));
    } else {
        pc_print_addstring(dest,(*q).ustr.strptr,oldlen);
    };
}

void pc_print_j_tostr_l_m(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest) {
    struct pc_decls_varrec *  q;
    i64 i;
    i64 n;
    struct pc_decls_objrec *  r;
    ++pc_print_listdepth;
    r = (*p).objptr;
    if ((((i64)((u64)((*r).refcount)) < (i64)0) || (pc_print_listdepth > (i64)4))) {
        pc_print_addstring(dest,(byte*)"...",(i64)3);
        --pc_print_listdepth;
        return;
    };
    pc_print_addstring(dest,(byte*)"(",(i64)1);
    (*r).refcount = -((u64)((*r).refcount));
    q = (*r).ulist.vptr;
    if (((i64)((u64)((*p).tag)) == (i64)29)) {
        n = (i64)((*(*p).objptr).ulist.length);
    } else {
        n = (i64)(pc_decls_ttlength[((i64)((*p).tag))]);
    };
    L547 :;
    for (i=n;i>=(i64)1;i-=(i64)1) {
L548 :;
        pc_print_calltostrtable(q,fmtstr,fmt,dest);
        ++q;
        if ((i != (i64)1)) {
            pc_print_addstring(dest,(byte*)",",(i64)1);
        };
L549 :;
    }L550 :;
    ;
    pc_print_addstring(dest,(byte*)")",(i64)1);
    (*r).refcount = -((u64)((*r).refcount));
    --pc_print_listdepth;
}

void pc_print_j_tostr_a(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest) {
    byte str[1024];
    byte *  q;
    i64 i;
    i64 m;
    i64 elemtype;
    i64 a;
    i64 b;
    struct pc_decls_varrec v;
    struct pc_decls_objrec *  pa;
    if ((fmt == 0)) {
        fmt = &pc_print_defaultfmt;
    };
    m = (i64)((*p).tag);
    pa = (*p).objptr;
    a = (i64)((*pa).uarray.lower);
    elemtype = (i64)((*pa).uarray.elemtag);
    b = (((i64)((u64)((*pa).uarray.length)) + a) - (i64)1);
    q = (*pa).uarray.ptr;
    msysnewc_m_print_startstr(str);
    msysnewc_m_print_setfmt((byte*)"#[#:#]");
    msysnewc_m_print_str(pc_decls_ttname[(m)],NULL);
    msysnewc_m_print_i64((*pa).uarray.lower,NULL);
    msysnewc_m_print_str(pc_decls_ttname[(elemtype)],NULL);
    msysnewc_m_print_end();
    ;
    pc_print_addstring(dest,str,(i64)-1);
    pc_print_addstring(dest,(byte*)"A(",(i64)-1);
    L551 :;
    for (i=a;i<=b;i+=(i64)1) {
L552 :;
        pc_pcfns_pc_loadpacked((void *)(q),elemtype,&v,(struct pc_decls_objrec *)(0));
        q += pc_decls_ttsize[(elemtype)];
        pc_print_calltostrtable(&v,fmtstr,fmt,dest);
        if ((i < b)) {
            pc_print_addstring(dest,(byte*)",",(i64)1);
        };
L553 :;
    }L554 :;
    ;
    pc_print_addstring(dest,(byte*)")",(i64)1);
}

void pc_print_j_tostr_b(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest) {
    byte *  q;
    i64 i;
    i64 m;
    i64 elemtype;
    i64 a;
    i64 b;
    i64 offset;
    i64 bitwidthx;
    struct pc_decls_varrec v;
    struct pc_decls_objrec *  pa;
    if ((fmt == 0)) {
        fmt = &pc_print_defaultfmt;
    };
    m = (i64)((*p).tag);
    pa = (*p).objptr;
    a = (i64)((*pa).ubits.lower);
    elemtype = (i64)((*pa).ubits.elemtag);
    offset = (i64)((*pa).ubits.bitoffset);
    b = (((i64)((u64)((*pa).ubits.length)) + a) - (i64)1);
    bitwidthx = (i64)(pc_decls_ttbitwidth[(elemtype)]);
    pc_print_addstring(dest,(byte*)"(",(i64)-1);
    q = (byte *)((*pa).ubits.ptr);
    L555 :;
    for (i=a;i<=b;i+=(i64)1) {
L556 :;
        pc_pcfns_pc_loadbit((byte *)(q),offset,elemtype,(i64)0,&v);
        offset += bitwidthx;
        if ((offset >= (i64)8)) {
            offset = (i64)0;
            ++q;
        };
        pc_print_calltostrtable(&v,fmtstr,fmt,dest);
        if ((i < b)) {
            pc_print_addstring(dest,(byte*)",",(i64)1);
        };
L557 :;
    }L558 :;
    ;
    pc_print_addstring(dest,(byte*)")",(i64)1);
}

void pc_print_j_tostr_e(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest) {
    i64 i;
    i64 j;
    i64 first;
    struct pc_decls_varrec v;
    struct pc_decls_objrec *  s;
    if ((fmt == 0)) {
        fmt = &pc_print_defaultfmt;
    };
    pc_print_addstring(dest,(byte*)"[",(i64)1);
    s = (*p).objptr;
    first = (i64)1;
    i = (i64)0;
    L559 :;
    while ((i < (i64)((u64)((*s).uset.length)))) {
        if (!!(pc_support_testelem((byte (*)[])((*s).uset.ptr),i))) {
            j = (i + (i64)1);
            L562 :;
            while (((j < (i64)((u64)((*s).uset.length))) && !!(pc_support_testelem((byte (*)[])((*s).uset.ptr),j)))) {
                ++j;
L563 :;
            }L564 :;
            ;
            --j;
            if (!(!!(first))) {
                pc_print_addstring(dest,(byte*)",",(i64)1);
            };
            first = (i64)0;
            if ((i == j)) {
                v.tagx = (u64)((i64)1);
                v.value = i;
            } else {
                v.tagx = (u64)((i64)4);
                v.range_lower = i;
                v.range_upper = j;
            };
            pc_print_calltostrtable(&v,fmtstr,fmt,dest);
            i = (j + (i64)1);
        } else {
            ++i;
        };
L560 :;
    }L561 :;
    ;
    pc_print_addstring(dest,(byte*)"]",(i64)1);
}

void pc_print_j_tostr_k(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest) {
    byte needcomma;
    i64 i;
    i64 stag;
    i64 ftype;
    i64 offset;
    i64 index;
    byte *  ptr;
    struct pc_decls_varrec v;
    struct pc_decls_objrec *  pa;
    i64 fieldtypes[100];
    i64 fieldoffsets[100];
    i64 nfields;
    if ((fmt == 0)) {
        fmt = &pc_print_defaultfmt;
    };
    stag = (i64)((*p).tag);
    index = (i64)(pc_decls_ttstartfield[(stag)]);
    nfields = (i64)(pc_decls_ttstructfields[(stag)]);
    L565 :;
    for (i=(i64)1;i<=nfields;i+=(i64)1) {
L566 :;
        fieldtypes[(((nfields - i) + (i64)1))-1] = (i64)((*pc_decls_pcfieldtable)[(((index + i) - (i64)1))-1].fieldtype);
        fieldoffsets[(((nfields - i) + (i64)1))-1] = (i64)((*pc_decls_pcfieldtable)[(((index + i) - (i64)1))-1].fieldoffset);
L567 :;
    }L568 :;
    ;
    pa = (*p).objptr;
    ptr = (*pa).ustruct.ptr;
    pc_print_addstring(dest,(byte*)"(",(i64)-1);
    needcomma = (u64)((i64)0);
    L569 :;
    for (i=nfields;i>=(i64)1;i-=(i64)1) {
L570 :;
        ftype = fieldtypes[(i)-1];
        offset = fieldoffsets[(i)-1];
        pc_pcfns_pc_loadpacked((void *)((ptr + offset)),ftype,&v,(struct pc_decls_objrec *)(0));
        if (!!((u64)(needcomma))) {
            pc_print_addstring(dest,(byte*)",",(i64)1);
        };
        needcomma = (u64)((i64)1);
        pc_print_calltostrtable(&v,fmtstr,fmt,dest);
L571 :;
    }L572 :;
    ;
    pc_print_addstring(dest,(byte*)")",(i64)1);
}

void pc_print_j_tostr_j(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest) {
    byte *  s;
    s = pc_bignum_bx_tostring(p,(i64)0);
    pc_print_addstring(dest,s,(i64)-1);
    free((void *)(s));
}

void pc_print_j_tostr_d(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest) {
    struct pc_decls_varrec *  q;
    i64 i;
    i64 length;
    i64 needcomma;
    struct pc_decls_objrec *  pa;
    needcomma = (i64)0;
    if ((fmt == 0)) {
        fmt = &pc_print_defaultfmt;
    };
    pc_print_addstring(dest,(byte*)"[",(i64)-1);
    pa = (*p).objptr;
    q = (*pa).udict.vptr;
    length = ((i64)((u64)((*pa).udict.length)) / (i64)2);
    L573 :;
    for (i=length;i>=(i64)1;i-=(i64)1) {
L574 :;
        if (((i64)((u64)((*q).tag)) == (i64)0)) {
            q += (i64)2;
            goto L575 ;
        };
        if (!!(needcomma)) {
            pc_print_addstring(dest,(byte*)",",(i64)1);
        };
        needcomma = (i64)1;
        pc_print_calltostrtable(q,fmtstr,fmt,dest);
        ++q;
        pc_print_addstring(dest,(byte*)":",(i64)1);
        pc_print_calltostrtable(q,fmtstr,fmt,dest);
        ++q;
L575 :;
    }L576 :;
    ;
    pc_print_addstring(dest,(byte*)"]",(i64)1);
}

void pc_print_j_tostr_z(struct pc_decls_varrec * a,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest) {
    i64 cmd;
    byte str[1024];
    switch ((i64)((*a).tag)) {
    case 0:;
    {
        pc_print_addstring(dest,(byte*)"<Void>",(i64)-1);
    }break;
    case 23:;
    {
        msysnewc_m_print_startstr(str);
        msysnewc_m_print_setfmt((byte*)"Ref #:#");
        msysnewc_m_print_str(pc_decls_ttname[((i64)((*a).uref.elemtag))],NULL);
        msysnewc_m_print_ptr((*a).uref.ptr,NULL);
        msysnewc_m_print_end();
        ;
        pc_print_addstring(dest,str,(i64)-1);
    }break;
    case 24:;
    {
        msysnewc_m_print_startstr(str);
        msysnewc_m_print_setfmt((byte*)"Refbit #:# @# [*#]");
        msysnewc_m_print_str(pc_decls_ttname[((i64)((*a).uref.elemtag))],NULL);
        msysnewc_m_print_ptr((*a).uref.ptr,NULL);
        msysnewc_m_print_u64((*a).uref.bitoffset,NULL);
        msysnewc_m_print_u64((*a).uref.bitlength,NULL);
        msysnewc_m_print_end();
        ;
        pc_print_addstring(dest,str,(i64)-1);
    }break;
    case 22:;
    {
        msysnewc_m_print_startstr(str);
        msysnewc_m_print_str((byte*)"Refvar:",NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_ptr((*a).varptr,NULL);
        msysnewc_m_print_end();
        ;
        pc_print_addstring(dest,str,(i64)-1);
        if (!!((*a).varptr)) {
            msysnewc_m_print_startstr(str);
            msysnewc_m_print_setfmt((byte*)" <#>");
            msysnewc_m_print_str(pc_decls_ttname[((i64)((*(*a).varptr).tag))],NULL);
            msysnewc_m_print_end();
            ;
            pc_print_addstring(dest,str,(i64)-1);
        };
    }break;
    case 25:;
    {
        msysnewc_m_print_startstr(str);
        msysnewc_m_print_setfmt((byte*)"Link:<#>");
        msysnewc_m_print_str(pc_decls_ttname[((i64)((*a).uref.elemtag))],NULL);
        msysnewc_m_print_end();
        ;
        pc_print_addstring(dest,str,(i64)-1);
    }break;
    case 18:;
    {
        msysnewc_m_print_startstr(str);
        msysnewc_m_print_str((byte*)"Refproc:",NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_ptr((*a).refptr,NULL);
        msysnewc_m_print_end();
        ;
        pc_print_addstring(dest,str,(i64)-1);
    }break;
    case 20:;
    {
        msysnewc_m_print_startstr(str);
        msysnewc_m_print_str((byte*)"Reflabel:",NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_ptr((*a).refptr,NULL);
        msysnewc_m_print_end();
        ;
        pc_print_addstring(dest,str,(i64)-1);
    }break;
    case 13:;
    {
        pc_print_addstring(dest,(byte*)"<",(i64)1);
        pc_print_addstring(dest,(pc_decls_ttname[((*a).value)] + (i64)1),(i64)-1);
        pc_print_addstring(dest,(byte*)">",(i64)1);
    }break;
    case 14:;
    {
        pc_print_addstring(dest,(byte*)"<OP:",(i64)-1);
        cmd = (*a).value;
        pc_print_addstring(dest,(pq_common_cmdnames[(cmd)] + (i64)1),(i64)-1);
        pc_print_addstring(dest,(((i64)((u64)((*a).uop.opdims)) == (i64)1)?(byte*)":1":(byte*)":2"),(i64)2);
        pc_print_addstring(dest,(byte*)">",(i64)1);
    }break;
    default: {
        pc_support_pcustype((byte*)"tostr_def",a);
    }
    } //SW
;
}

static void pc_print_printnextfmtchars(i64 lastx) {
    byte c;
    byte *  pstart;
    i64 n;
    pstart = pc_print_mfmtcurr;
    n = (i64)0;
    L577 :;
    while (!!((i64)1)) {
        c = (u64)((*pc_print_mfmtcurr));
        switch ((i64)(c)) {
        case 35:;
        {
            if (!!(lastx)) {
                goto L580 ;
;
            };
            ++pc_print_mfmtcurr;
            if (!!(n)) {
                pc_print_printstr_n(pstart,n);
            };
            return;
        }break;
        case 0:;
        {
            if (!!(n)) {
                pc_print_printstr_n(pstart,n);
            } else if (!(!!(lastx))) {
                pc_print_printstr_n((byte*)"|",(i64)1);
            };
            return;
        }break;
        case 126:;
        {
            if (!!(n)) {
                pc_print_printstr_n(pstart,n);
                n = (i64)0;
            };
            ++pc_print_mfmtcurr;
            c = (u64)((*pc_print_mfmtcurr));
            if (!!((u64)(c))) {
                ++pc_print_mfmtcurr;
                pc_print_printstr_n(&c,(i64)1);
            };
            pstart = pc_print_mfmtcurr;
        }break;
        default: {
            //skip:
L580 :;
;
            ++n;
            ++pc_print_mfmtcurr;
        }
        } //SW
;
L578 :;
    }L579 :;
    ;
}

static i64 pc_print_getreadfmtcode(struct pc_decls_varrec * p) {
    byte c;
    if (((p == 0) || ((i64)((u64)((*p).tag)) == (i64)0))) {
        return (i64)65;
    };
    if (((i64)((u64)((*p).tag)) != (i64)5)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"P=%s",NULL);
        msysnewc_m_print_str(pc_decls_ttname[((i64)((*p).tag))],NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_prterror((byte*)"Readfmt?");
    };
    if (((i64)((*(*p).objptr).ustr.length) == (i64)0)) {
        return (i64)65;
    };
    c = (u64)(toupper((i64)((i32)((*(*(*p).objptr).ustr.strptr)))));
    switch ((i64)(c)) {
    case 73:;
    case 82:;
    case 78:;
    case 83:;
    case 70:;
    case 84:;
    case 90:;
    case 67:;
    case 76:;
    case 72:;
    case 66:;
    case 65:;
    case 69:;
    {
        return (i64)(c);
    }break;
    default: {
    }
    } //SW
;
    pc_support_prterror((byte*)"Readfmt2?");
    return (i64)0;
}

void pc_print_pch_sreadln(struct pc_decls_varrec * dev,struct pc_decls_varrec * dest) {
    pc_print_pch_readln(dev);
    pc_pcfns_pc_makestring(pc_print_kb_start,pc_print_kb_length,dest);
}

void pc_print_pch_strtoval(struct pc_decls_varrec * p,struct pc_decls_varrec * fmt,struct pc_decls_varrec * dest) {
    i64 fmtcode;
    i64 length;
    struct pc_decls_objrec *  q;
    byte str[1024];
    byte *  s;
    s = str;
    q = (*p).objptr;
    if (((i64)((*q).ustr.length) < (i64)1024)) {
        memcpy((void *)(s),(void *)((*q).ustr.strptr),(u64)((*q).ustr.length));
        str[(((i64)((*q).ustr.length) + (i64)1))-1] = (u64)0u;
    } else {
        pc_support_pcerror((byte*)"STRTOVAL/string too long");
    };
    fmtcode = pc_print_getreadfmtcode(fmt);
    if (((i64)((u64)((*p).tag)) != (i64)5)) {
        pc_support_prterror((byte*)"strval");
    };
    length = (i64)((*(*p).objptr).ustr.length);
    switch (fmtcode) {
    case 73:;
    {
        pc_print_readint(s,length,dest);
    }break;
    case 82:;
    {
        pc_print_readreal(s,length,dest);
    }break;
    case 78:;
    {
        pc_print_readname(s,length,dest);
    }break;
    case 83:;
    {
        pc_print_readstring(s,length,dest);
    }break;
    case 72:;
    {
        pc_print_readhex(s,length,dest);
    }break;
    case 66:;
    {
        pc_print_readbin(s,length,dest);
    }break;
    case 65:;
    {
        pc_print_readany(s,length,dest);
    }break;
    default: {
        pc_support_prterror((byte*)"strval:fmt?");
    }
    } //SW
;
}

void pc_print_pch_reread(void) {
    pc_print_kb_pos = pc_print_kb_lastpos;
    pc_print_kb_length = pc_print_kb_lastlength;
}

void pc_print_pch_rereadln(void) {
    pc_print_kb_pos = pc_print_kb_start;
    pc_print_kb_length = pc_print_kb_linelength;
}

static byte * pc_print_readname(byte * s,i64 length,struct pc_decls_varrec * dest) {
    byte *  send;
    byte *  itemstr;
    i64 itemlength;
    send = pc_print_readitem(s,length,&itemstr,&itemlength);
    pc_pcfns_pc_makestring(itemstr,itemlength,dest);
    mlib_iconvlcn((*(*dest).objptr).ustr.strptr,(i64)((*(*dest).objptr).ustr.length));
    return send;
}

static byte * pc_print_readstring(byte * s,i64 length,struct pc_decls_varrec * dest) {
    byte *  send;
    byte *  itemstr;
    i64 itemlength;
    send = pc_print_readitem(s,length,&itemstr,&itemlength);
    pc_pcfns_pc_makestring(itemstr,itemlength,dest);
    return send;
}

static byte * pc_print_readint(byte * sold,i64 length,struct pc_decls_varrec * dest) {
    byte *  s;
    byte *  send;
    i64 itemlength;
    send = pc_print_readitem(sold,length,&s,&itemlength);
    pc_print_strtoint(s,itemlength,dest);
    return send;
}

static byte * pc_print_readhex(byte * sold,i64 length,struct pc_decls_varrec * dest) {
    byte str[256];
    byte *  p;
    byte *  s;
    i64 aa;
    i64 t;
    i64 nalloc;
    byte c;
    if ((length == (i64)0)) {
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = (i64)0;
        pc_print_termchar = (u64)0u;
        return sold;
    };
    L581 :;
    while ((!!(length) && (((u64)((*sold)) == ' ') || ((i64)((*sold)) == (i64)9)))) {
        ++sold;
        --length;
L582 :;
    }L583 :;
    ;
    if ((length <= (i64)256)) {
        s = str;
        nalloc = (i64)0;
    } else {
        nalloc = (length + (i64)1);
        s = (byte *)(mlib_pcm_alloc(nalloc));
    };
    p = s;
    L584 :;
    while (!!(length)) {
        c = (u64)(toupper((i64)((i32)((*sold)))));
        ++sold;
        --length;
        if ((((u64)(c) >= '0') && ((u64)(c) <= '9'))) {
            (*p) = (u64)(c);
            ++p;
        } else if ((((u64)(c) >= 'A') && ((u64)(c) <= 'F'))) {
            (*p) = (u64)(c);
            ++p;
        } else if (((u64)(c) == '_')) {
        } else {
            pc_print_termchar = (u64)(c);
            goto L586 ;
        };
L585 :;
    }L586 :;
    ;
    (*p) = (u64)0u;
    length = (p - s);
    if ((length <= (i64)16)) {
        t = (i64)1;
    } else {
        t = (i64)7;
    };
    p = s;
    switch (t) {
    case 1:;
    {
        aa = (i64)0;
        L587 :;
        while (!!((i64)1)) {
            c = (u64)((*p));
            ++p;
            if (((i64)(c) == (i64)0)) {
                goto L589 ;
            };
            if (((u64)(c) < 'A')) {
                aa = (((aa * (i64)16) + (i64)(c)) - (i64)48);
            } else {
                aa = (((aa * (i64)16) + ((u64)(c) - 'A')) + (i64)10);
            };
L588 :;
        }L589 :;
        ;
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = aa;
    }break;
    default: {
        pc_support_prterror((byte*)"Readhex/long");
    }
    } //SW
;
    if (!!(nalloc)) {
        mlib_pcm_free((void *)(s),nalloc);
    };
    return sold;
}

static byte * pc_print_readbin(byte * sold,i64 length,struct pc_decls_varrec * dest) {
    byte str[256];
    byte *  p;
    byte *  s;
    i64 aa;
    i64 t;
    i64 nalloc;
    byte c;
    if ((length == (i64)0)) {
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = (i64)0;
        pc_print_termchar = (u64)0u;
        return sold;
    };
    L590 :;
    while ((!!(length) && (((u64)((*sold)) == ' ') || ((i64)((*sold)) == (i64)9)))) {
        ++sold;
        --length;
L591 :;
    }L592 :;
    ;
    if ((length <= (i64)256)) {
        s = str;
        nalloc = (i64)0;
    } else {
        nalloc = (length + (i64)1);
        s = (byte *)(mlib_pcm_alloc(nalloc));
    };
    p = s;
    L593 :;
    while (!!(length)) {
        c = (u64)(toupper((i64)((i32)((*sold)))));
        ++sold;
        --length;
        if ((((u64)(c) >= '0') && ((u64)(c) <= '1'))) {
            (*p) = (u64)(c);
            ++p;
        } else if (((u64)(c) == '_')) {
        } else {
            pc_print_termchar = (u64)(c);
            goto L595 ;
        };
L594 :;
    }L595 :;
    ;
    (*p) = (u64)0u;
    length = (p - s);
    if ((length <= (i64)64)) {
        t = (i64)1;
    } else {
        t = (i64)7;
    };
    p = s;
    switch (t) {
    case 1:;
    {
        aa = (i64)0;
        L596 :;
        while (!!((i64)1)) {
            c = (u64)((*p));
            ++p;
            if (((i64)(c) == (i64)0)) {
                goto L598 ;
            };
            aa = (((aa * (i64)2) + (i64)(c)) - (i64)48);
L597 :;
        }L598 :;
        ;
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = aa;
    }break;
    default: {
        pc_support_prterror((byte*)"Readbin/long");
    }
    } //SW
;
    if (!!(nalloc)) {
        mlib_pcm_free((void *)(s),nalloc);
    };
    return sold;
}

static byte * pc_print_readreal(byte * sold,i64 length,struct pc_decls_varrec * dest) {
    byte *  send;
    byte *  itemstr;
    i64 itemlength;
    send = pc_print_readitem(sold,length,&itemstr,&itemlength);
    pc_print_strtoreal(itemstr,itemlength,dest);
    return send;
}

void pc_print_pch_readln(struct pc_decls_varrec * dev) {
    void *  ch;
    i64 length;
    struct pc_decls_objrec *  pdev;
    if ((pc_print_kb_start == 0)) {
        pc_print_kb_start = (byte *)(mlib_pcm_alloc((i64)262144));
        pc_print_kb_size = (i64)262144;
        pc_print_kb_lastpos = pc_print_kb_start;
        pc_print_kb_pos = pc_print_kb_start;
        pc_print_kb_length = (i64)0;
        pc_print_kb_lastlength = (i64)0;
        pc_print_kb_linelength = (i64)0;
    };
    switch ((i64)((*dev).tag)) {
    case 0:;
    {
        //doconsole:
L599 :;
;
        mlib_readlinen(0,pc_print_kb_start,pc_print_kb_size);
        pc_print_kb_length = (i64)(strlen((i8 *)(pc_print_kb_start)));
    }break;
    case 1:;
    {
        switch ((*dev).value) {
        case 0:;
        {
            goto L599 ;
;
        }break;
        case 1:;
        {
            if ((pc_print_testfilech == 0)) {
                pc_support_prterror((byte*)"R@2: file not open");
            };
            ch = pc_print_testfilech;
        }break;
        default: {
            ch = (void *)((*dev).value);
        }
        } //SW
;
        pc_print_pc_readlinen(ch,pc_print_kb_start,pc_print_kb_size);
        pc_print_kb_length = (i64)(strlen((i8 *)(pc_print_kb_start)));
    }break;
    case 5:;
    {
        pdev = (*dev).objptr;
        length = (i64)((*pdev).ustr.length);
        if ((length == (i64)0)) {
            pc_print_kb_length = (i64)0;
            (*pc_print_kb_start) = (u64)0u;
        } else if ((length >= pc_print_kb_size)) {
            pc_support_prterror((byte*)"KB overflow");
        } else {
            pc_print_kb_length = length;
            memcpy((void *)(pc_print_kb_start),(void *)((*pdev).ustr.strptr),(u64)(length));
        };
    }break;
    default: {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str(pc_support_gettypename((i64)((*dev).tag)),NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_prterror((byte*)"readln@");
    }
    } //SW
;
    pc_print_kb_pos = pc_print_kb_start;
    pc_print_kb_lastpos = pc_print_kb_pos;
    pc_print_kb_linelength = pc_print_kb_length;
}

static void pc_print_stepkbpos(byte * s) {
    i64 newlen;
    newlen = (s - pc_print_kb_pos);
    if ((newlen == (i64)0)) {
        return;
    };
    if ((newlen >= pc_print_kb_length)) {
        pc_print_kb_pos = (pc_print_kb_pos + pc_print_kb_length);
        pc_print_kb_length = (i64)0;
    } else {
        pc_print_kb_pos = (pc_print_kb_pos + newlen);
        pc_print_kb_length -= newlen;
    };
}

void pc_print_pch_sread(struct pc_decls_varrec * fmt,struct pc_decls_varrec * dest) {
    i64 fmtcode;
    fmtcode = pc_print_getreadfmtcode(fmt);
    pc_print_kb_lastpos = pc_print_kb_pos;
    pc_print_kb_lastlength = pc_print_kb_length;
    switch (fmtcode) {
    case 73:;
    {
        pc_print_stepkbpos(pc_print_readint(pc_print_kb_pos,pc_print_kb_length,dest));
    }break;
    case 82:;
    {
        pc_print_stepkbpos(pc_print_readreal(pc_print_kb_pos,pc_print_kb_length,dest));
    }break;
    case 78:;
    {
        pc_print_stepkbpos(pc_print_readname(pc_print_kb_pos,pc_print_kb_length,dest));
    }break;
    case 83:;
    {
        pc_print_stepkbpos(pc_print_readstring(pc_print_kb_pos,pc_print_kb_length,dest));
    }break;
    case 72:;
    {
        pc_print_stepkbpos(pc_print_readhex(pc_print_kb_pos,pc_print_kb_length,dest));
    }break;
    case 66:;
    {
        pc_print_stepkbpos(pc_print_readbin(pc_print_kb_pos,pc_print_kb_length,dest));
    }break;
    case 65:;
    {
        pc_print_stepkbpos(pc_print_readany(pc_print_kb_pos,pc_print_kb_length,dest));
    }break;
    case 76:;
    {
        if ((pc_print_kb_length == (i64)0)) {
            pc_pcfns_pc_emptystring(dest);
        } else {
            pc_pcfns_pc_makestring(pc_print_kb_pos,pc_print_kb_length,dest);
            pc_print_kb_pos += pc_print_kb_length;
            pc_print_kb_length = (i64)0;
        };
    }break;
    case 67:;
    {
        if ((pc_print_kb_length == (i64)0)) {
            pc_pcfns_pc_emptystring(dest);
        } else {
            pc_print_termchar = (u64)((*pc_print_kb_pos));
            //dochar:
L600 :;
;
            (*dest).tagx = (u64)((i64)1);
            (*dest).value = (i64)(pc_print_termchar);
            ++pc_print_kb_pos;
            --pc_print_kb_length;
        };
    }break;
    case 90:;
    {
        goto L600 ;
;
    }break;
    case 69:;
    {
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = pc_print_itemerror;
    }break;
    default: {
        pc_support_prterror((byte*)"SREAD/FMT?");
    }
    } //SW
;
}

static void pc_print_domultichar(byte * p,i64 n,byte * dest,struct pc_decls_fmtrec * fmt) {
    byte str[20];
    byte *  q;
    i64 nchars;
    i64 av_1;
    q = str;
    nchars = n;
    av_1 = n;
    while (av_1-- > 0) {
L601 :;
        if (((i64)((*p)) == (i64)0)) {
            goto L603 ;
        };
        (*q) = (u64)((*p));
        ++q;
        ++p;
L602 :;
    }L603 :;
    ;
    (*q) = (u64)0u;
    pc_print_expandstr(str,dest,nchars,fmt);
}

void pc_print_pch_tostr(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result) {
    struct pc_decls_fmtrec fmt;
    struct pc_decls_fmtrec *  ifmt;
    struct pc_decls_objrec *  p;
    ifmt = pc_print_pc_getfmt(b,&fmt);
    p = pc_objlib_obj_new((i64)5);
    (*p).ustr.mutable = (u64)((i64)1);
    pc_print_listdepth = (i64)0;
    pc_print_calltostrtable(a,b,ifmt,p);
    (*result).tagx = (u64)((i64)65541);
    (*result).objptr = p;
}

struct pc_decls_fmtrec * pc_print_pc_getfmt(struct pc_decls_varrec * p,struct pc_decls_fmtrec * fmt) {
    if (((p == 0) || ((i64)((u64)((*p).tag)) == (i64)0))) {
        return &pc_print_defaultfmt;
    } else {
        if (((i64)((u64)((*p).tag)) != (i64)5)) {
            pc_support_prterror((byte*)"pc_getfmt/not str?");
        };
        if (((*(*p).objptr).ustr.strptr == 0)) {
            return &pc_print_defaultfmt;
        } else {
            pc_print_pc_strtofmt((*(*p).objptr).ustr.strptr,(i64)((*(*p).objptr).ustr.length),fmt);
            return fmt;
        };
    };
}

static void pc_print_pc_readlinen(void * handlex,byte * buffer,i64 size) {
    byte *  p;
    i64 n;
    byte crseen;
    (*buffer) = (u64)0u;
    fgets((i8 *)(buffer),(size - (i64)2),handlex);
    n = (i64)(strlen((i8 *)(buffer)));
    if ((n == (i64)0)) {
        return;
    };
    p = ((buffer + n) - (i64)1);
    crseen = (u64)((i64)0);
    L604 :;
    while (((p >= buffer) && (((i64)((*p)) == (i64)13) || ((i64)((*p)) == (i64)10)))) {
        if ((((i64)((*p)) == (i64)13) || ((i64)((*p)) == (i64)10))) {
            crseen = (u64)((i64)1);
        };
        (*p--) = (u64)0u;
L605 :;
    }L606 :;
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

static byte * pc_print_readitem(byte * s,i64 length,byte * * itemstr,i64 * itemlength) {
    byte *  p;
    byte quotechar;
    byte c;
    L607 :;
    while ((!!(length) && (((u64)((*s)) == ' ') || ((i64)((*s)) == (i64)9)))) {
        ++s;
        --length;
L608 :;
    }L609 :;
    ;
    (*itemstr) = s;
    if ((length == (i64)0)) {
        pc_print_termchar = (u64)0u;
        (*itemlength) = (i64)0;
        return s;
    };
    quotechar = (u64)0u;
    if (((u64)((*s)) == '"')) {
        quotechar = '"';
        ++s;
        --length;
    } else if (((u64)((*s)) == (u64)39u)) {
        quotechar = (u64)39u;
        ++s;
        --length;
    };
    p = ((*itemstr) = s);
    L610 :;
    while (!!(length)) {
        c = (u64)((*s++));
        --length;
        switch ((i64)(c)) {
        case 32:;
        case 9:;
        case 44:;
        case 61:;
        {
            if ((!!((u64)(quotechar)) || (p == s))) {
                goto L613 ;
;
            };
            pc_print_termchar = (u64)(c);
            goto L612 ;
        }break;
        default: {
            //normalchar:
L613 :;
;
            if (((u64)(c) == (u64)(quotechar))) {
                if ((!!(length) && ((u64)((*s)) == (u64)(quotechar)))) {
                    (*p) = (u64)(c);
                    ++s;
                    ++p;
                } else {
                    pc_print_termchar = (u64)((*s));
                    if ((((u64)(pc_print_termchar) == ',') || ((u64)(pc_print_termchar) == '='))) {
                        ++s;
                        pc_print_termchar = (u64)((*s));
                    };
                    goto L612 ;
                };
            } else {
                (*p) = (u64)(c);
                ++p;
            };
        }
        } //SW
;
L611 :;
    }L612 :;
    ;
    if ((length == (i64)0)) {
        pc_print_termchar = (u64)0u;
    };
    (*itemlength) = (p - (*itemstr));
    return s;
}

static byte * pc_print_readany(byte * sold,i64 length,struct pc_decls_varrec * dest) {
    byte *  p;
    byte *  s;
    i64 digits;
    i64 expon;
    i64 other;
    byte *  send;
    i64 itemlength;
    i64 av_1;
    pc_print_itemerror = (i64)0;
    send = pc_print_readitem(sold,length,&s,&itemlength);
    p = s;
    digits = (expon = (other = (i64)0));
    av_1 = itemlength;
    while (av_1-- > 0) {
L614 :;
        switch ((i64)((*p++))) {
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
        case 43:;
        case 45:;
        case 95:;
        {
            digits = (i64)1;
        }break;
        case 69:;
        case 101:;
        case 46:;
        {
            expon = (i64)1;
        }break;
        default: {
            other = (i64)1;
        }
        } //SW
;
L615 :;
    }L616 :;
    ;
    (*dest).tagx = (u64)((i64)1);
    if ((!!(other) || (itemlength == (i64)0))) {
        (*dest).value = (i64)5395539;
        pc_pcfns_pc_makestring(s,itemlength,dest);
    } else if (!!(expon)) {
        pc_print_strtoreal(s,itemlength,dest);
    } else {
        pc_print_strtoint(s,itemlength,dest);
    };
    return send;
}

static void pc_print_strtoreal(byte * s,i64 length,struct pc_decls_varrec * dest) {
    byte str[512];
    double x;
    i32 numlength;
    (*dest).tagx = (u64)((i64)3);
    if (((length >= (i64)512) || (length == (i64)0))) {
        (*dest).xvalue = (double)0.;
        return;
    };
    memcpy((void *)(str),(void *)(s),(u64)(length));
    str[((length + (i64)1))-1] = (u64)0u;
    pc_print_itemerror = (i64)0;
    if ((((i64)(sscanf((i8 *)(str),(i8 *)((byte*)"%lf%n"),&x,&numlength)) == (i64)0) || ((i64)(numlength) != length))) {
        if (((i64)(numlength) == length)) {
            x = (double)0.;
        };
        pc_print_itemerror = (i64)1;
    };
    (*dest).xvalue = x;
}

static void pc_print_strtoint(byte * s,i64 length,struct pc_decls_varrec * dest) {
    byte *  p;
    byte *  q;
    byte signd;
    i64 aa;
    i64 cat;
    i64 t;
    byte c;
    pc_print_itemerror = (i64)0;
    if ((length == (i64)0)) {
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = (i64)0;
        return;
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
    L617 :;
    while ((((u64)((*s)) == '0') && (length > (i64)1))) {
        ++s;
        --length;
L618 :;
    }L619 :;
    ;
    p = (q = s);
    L620 :;
    while (!!(length)) {
        c = (u64)((*q++));
        --length;
        if ((((u64)(c) >= '0') && ((u64)(c) <= '9'))) {
            (*p) = (u64)(c);
            ++p;
        } else {
            if (((u64)(c) == '_')) {
            } else {
                pc_print_itemerror = (i64)1;
                goto L622 ;
            };
        };
L621 :;
    }L622 :;
    ;
    (*p) = (u64)0u;
    length = (p - s);
    if ((length <= (i64)18)) {
        cat = (i64)65;
    } else if ((length == (i64)19)) {
        if ((mlib_cmpstring(s,(byte*)"9223372036854775808")==(i64)-1)) {
            cat = (i64)65;
        }else if ((mlib_cmpstring(s,(byte*)"9223372036854775808")==(i64)0)) {
            cat = (i64)66;
        } else {
            cat = (i64)67;
        };
    } else if ((length == (i64)20)) {
        if ((mlib_cmpstring(s,(byte*)"18446744073709551615") <= (i64)0)) {
            cat = (i64)67;
        } else {
            cat = (i64)68;
        };
    } else {
        cat = (i64)68;
    };
    if (!!((u64)(signd))) {
        if ((cat==(i64)66)) {
            cat = (i64)65;
        }else if ((cat==(i64)67)) {
            cat = (i64)68;
        };
    };
    if ((cat==(i64)65)) {
        t = (i64)1;
    }else if ((cat==(i64)66) || (cat==(i64)67)) {
        t = (i64)2;
    } else {
        t = (i64)7;
    };
    p = s;
    if ((t != (i64)7)) {
        aa = (i64)0;
        L623 :;
        while (1) {
            c = (u64)((*p));
            ++p;
            if (((i64)(c) == (i64)0)) {
                goto L624 ;
            };
            aa = ((aa * (i64)10) + ((u64)(c) - '0'));
        }L624 :;
        ;
        if (!!((u64)(signd))) {
            aa = -(aa);
        };
        (*dest).tagx = (u64)(t);
        (*dest).value = aa;
    } else {
        pc_bignum_bx_makestr(s,length,dest);
    };
}

static void pc_print_calltostrtable(struct pc_decls_varrec * q,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest) {
    struct pc_decls_varrec v;
    pc_decls_overloadtype = (i64)((*q).tag);
    if (((i64)((u64)((*fmtstr).tag)) == (i64)0)) {
        pc_pcfns_pc_emptystring(&v);
        fmtstr = &v;
    };
    ((*pc_decls_tostr_table[(pc_decls_overloadtype)]))(q,fmtstr,fmt,dest);
}

static byte * pc_print_printbn(struct pc_decls_varrec * a0,struct pc_decls_fmtrec * fmt,i64 * length) {
    static struct mlib_strbuffer destx;
    static struct mlib_strbuffer *  dest = &destx;
    pc_support_pcerror((byte*)"PRINTBN");
    return (byte*)"XXX";
}

void pc_jhandlers_initcalltables(void) {
    i64 n;
    i64 i;
    i64 j;
    i64 slen;
    byte *  name;
    void *  fnaddr;
    void * (*stable)[];
    void * (*dtable)[];
    byte localmixedmap[301];
    i64 av_1;
    pc_jhandlers_mixedmap = &localmixedmap;
    memset((void *)(pc_jhandlers_mixedmap),(i64)0,(u64)((i64)301));
    n = msysnewc_m_get_nprocs();
    L625 :;
    for (i=(i64)1;i<=n;i+=(i64)1) {
L626 :;
        name = msysnewc_m_get_procname(i);
        if ((((u64)((*name)) == 'j') && ((u64)((*(name + (i64)1))) == '_'))) {
            pc_jhandlers_initjhandler(name,msysnewc_m_get_procaddr(i));
        };
L627 :;
    }L628 :;
    ;
    L629 :;
    for (i=(i64)1;i<=(i64)66;i+=(i64)1) {
L630 :;
        stable = (void * (*)[])(pc_jhandlers_singletable[(i)-1]);
        dtable = (void * (*)[])(pc_jhandlers_doubletable[(i)-1]);
        if (!!(stable)) {
            fnaddr = (*stable)[((i64)0)];
            if ((fnaddr == 0)) {
                fnaddr = (void *)(&pc_jhandlers_def_handler);
            };
            L633 :;
            for (j=(i64)0;j<=(i64)300;j+=(i64)1) {
L634 :;
                if (((*stable)[(j)] == 0)) {
                    (*stable)[(j)] = fnaddr;
                };
L635 :;
            }L636 :;
            ;
        };
        if (!!(dtable)) {
            fnaddr = (*dtable)[((i64)0)];
            if ((fnaddr == 0)) {
                fnaddr = (void *)(&pc_jhandlers_ddef_handler);
            };
            L637 :;
            for (j=(i64)0;j<=(i64)300;j+=(i64)1) {
L638 :;
                if (((*dtable)[(j)] == 0)) {
                    if (!!((u64)(localmixedmap[(j)]))) {
                        name = pc_jhandlers_tabnames[(i)-1];
                        slen = (i64)(strlen((i8 *)(name)));
                        if (!!(mlib_eqstring(((name + slen) - (i64)2),(byte*)"to"))) {
                            goto L641 ;
;
                        };
                        (*dtable)[(j)] = (void *)(pc_decls_mixed_dtable[(j)]);
                    } else {
                        //donormal:
L641 :;
;
                        (*dtable)[(j)] = fnaddr;
                    };
                };
L639 :;
            }L640 :;
            ;
        };
L631 :;
    }L632 :;
    ;
}

static void pc_jhandlers_initjhandler(byte * p,void * fnaddr) {
    byte opname[32];
    byte *  q;
    byte c;
    byte d;
    i64 t;
    i64 u;
    i64 i;
    void * (*stable)[];
    void * (*dtable)[];
    i64 av_1;
    p += (i64)2;
    q = p;
    L642 :;
    while ((((u64)((*q)) != '_') && ((i64)((*q)) != (i64)0))) {
        ++q;
L643 :;
    }L644 :;
    ;
    memcpy((void *)(opname),(void *)(p),(u64)((q - p)));
    opname[(((q - p) + (i64)1))-1] = (u64)0u;
    if (((i64)((*q)) == (i64)0)) {
        return;
    };
    L645 :;
    for (i=(i64)1;i<=(i64)66;i+=(i64)1) {
L646 :;
        if (!!(mlib_eqstring(pc_jhandlers_tabnames[(i)-1],opname))) {
            stable = (void * (*)[])(pc_jhandlers_singletable[(i)-1]);
            dtable = (void * (*)[])(pc_jhandlers_doubletable[(i)-1]);
            goto L648 ;
        };
L647 :;
    }
    {
        pc_support_loaderror((byte*)"Init: Can't find Jhandler op:",opname);
        exit(0);
    }L648 :;
    ;
    L649 :;
    while (((u64)((*q)) == '_')) {
        c = (u64)((*++q));
        if ((((u64)(c) < 'a') || ((u64)(c) > 'z'))) {
            return;
        };
        d = (u64)((*++q));
        if ((((u64)(d) >= 'a') && ((u64)(d) <= 'z'))) {
            ++q;
        } else {
            d = (u64)0u;
        };
        t = (i64)(pc_jhandlers_typemap[((i64)(c))-97]);
        if (!!((u64)(d))) {
            u = (i64)(pc_jhandlers_typemap[((i64)(d))-97]);
        } else {
            u = (i64)0;
        };
        if (!!((u64)(d))) {
            if ((dtable == 0)) {
                pc_support_loaderror((byte*)"No d-calltable for:",(p - (i64)2));
            };
            pc_jhandlers_add_dtable_entry((u64 * (*(*)[])(void))(dtable),t,u,fnaddr);
        } else {
            if ((stable == 0)) {
                pc_support_loaderror((byte*)"No s-calltable for:",(p - (i64)2));
            };
            pc_jhandlers_add_stable_entry(stable,t,fnaddr);
        };
L650 :;
    }L651 :;
    ;
}

static void pc_jhandlers_add_stable_entry(void * (*table)[],i64 t,void * fnaddr) {
    if ((t == (i64)35)) {
        t = (i64)0;
    };
    (*table)[(t)] = fnaddr;
}

static void pc_jhandlers_add_dtable_entry(u64 * (*(*table)[])(void),i64 s,i64 t,void * fnaddr) {
    i64 typesig;
    if ((s == (i64)35)) {
        typesig = (i64)0;
    } else {
        typesig = pc_support_gettypesig(s,t);
        if ((table == &pc_decls_mixed_dtable)) {
            (*pc_jhandlers_mixedmap)[(typesig)] = (u64)((i64)1);
        };
    };
    (*table)[(typesig)] = (u64 * (*)(void))(fnaddr);
}

static void pc_jhandlers_def_handler(void) {
    pc_support_pcerror((byte*)"Single disp: no handler");
}

static void pc_jhandlers_ddef_handler(void) {
    pc_support_pcerror((byte*)"Double disp: no handler");
}

u64 * pc_jhandlers_j_add_i_w(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).value += (*y).value;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_add_r(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).xvalue += (*y).xvalue;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_add_s(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 xlen;
    i64 ylen;
    i64 newlen;
    byte *  s;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    xlen = (i64)((*(*x).objptr).ustr.length);
    ylen = (i64)((*(*y).objptr).ustr.length);
    if ((xlen == (i64)0)) {
        if (!!(ylen)) {
            (*pc_decls_sptr) = (*y);
        };
    } else if ((ylen == (i64)0)) {
    } else {
        newlen = (xlen + ylen);
        s = (byte *)(mlib_pcm_alloc(newlen));
        memcpy((void *)(s),(void *)((*(*x).objptr).ustr.strptr),(u64)(xlen));
        memcpy((void *)((s + xlen)),(void *)((*(*y).objptr).ustr.strptr),(u64)(ylen));
        pc_pcfns_pc_unshare(x);
        pc_pcfns_pc_unshare(y);
        pc_pcfns_pc_makestringx(s,newlen,mlib_allocbytes,pc_decls_sptr);
    };
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_add_j(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    struct pc_decls_varrec result;
    y = pc_decls_sptr++;
    x = pc_decls_sptr;
    pc_bignum_bx_add(x,y,&result);
    pc_pcfns_pc_unshare(x);
    pc_pcfns_pc_unshare(y);
    (*pc_decls_sptr) = result;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_add_e(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    pc_dxfns_dx_iorset(x,y);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_add_z(void) {
    pc_support_pcmxtypes((byte*)"add_def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_add_iw_wi(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).value += (*y).value;
    (*pc_decls_sptr).tag = (u64)((i64)1);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_add_zz(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    y = pc_decls_sptr++;
    yt = (i64)((*y).tag);
    switch ((i64)((*pc_decls_sptr).tag)) {
    case 23:;
    {
        switch (yt) {
        case 1:;
        {
            (*pc_decls_sptr).uref.ptr = ((*pc_decls_sptr).uref.ptr + ((*y).value * pc_decls_ttsize[((i64)((*pc_decls_sptr).uref.elemtag))]));
            return (pc_decls_pcptr + (i64)1);
        }break;
        default: {
        }
        } //SW
;
    }break;
    case 22:;
    {
        switch (yt) {
        case 1:;
        {
            (*pc_decls_sptr).varptr = ((*pc_decls_sptr).varptr + (*y).value);
            return (pc_decls_pcptr + (i64)1);
        }break;
        default: {
        }
        } //SW
;
    }break;
    default: {
    }
    } //SW
;
    pc_support_pcmxtypes((byte*)"add/mixed_def",pc_decls_sptr,y);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_sub_i_w(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).value -= (*y).value;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_sub_r(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).xvalue -= (*y).xvalue;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_sub_p(void) {
    struct pc_decls_varrec *  y;
    byte *  p;
    byte *  q;
    i64 elemsize;
    y = pc_decls_sptr++;
    p = (*pc_decls_sptr).uref.ptr;
    q = (*y).uref.ptr;
    if (((elemsize = pc_decls_ttsize[((i64)((*pc_decls_sptr).uref.elemtag))])==(i64)1)) {
        (*pc_decls_sptr).value = (p - q);
    }else if (((elemsize = pc_decls_ttsize[((i64)((*pc_decls_sptr).uref.elemtag))])==(i64)2)) {
        (*pc_decls_sptr).value = ((p - q) >> (i64)1);
    }else if (((elemsize = pc_decls_ttsize[((i64)((*pc_decls_sptr).uref.elemtag))])==(i64)4)) {
        (*pc_decls_sptr).value = ((p - q) >> (i64)2);
    } else {
        (*pc_decls_sptr).value = ((p - q) / elemsize);
    };
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_sub_j(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    struct pc_decls_varrec result;
    y = pc_decls_sptr++;
    x = pc_decls_sptr;
    pc_bignum_bx_sub(x,y,&result);
    pc_pcfns_pc_unshare(x);
    pc_pcfns_pc_unshare(y);
    (*pc_decls_sptr) = result;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_sub_e(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    pc_dxfns_dx_subset(x,y);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_sub_z(void) {
    pc_support_pcmxtypes((byte*)"sub_def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_sub_zz(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    y = pc_decls_sptr++;
    yt = (i64)((*y).tag);
    switch ((i64)((*pc_decls_sptr).tag)) {
    case 23:;
    {
        switch (yt) {
        case 1:;
        {
            (*pc_decls_sptr).uref.ptr = ((*pc_decls_sptr).uref.ptr - ((*y).value * pc_decls_ttsize[((i64)((*pc_decls_sptr).uref.elemtag))]));
            return (pc_decls_pcptr + (i64)1);
        }break;
        default: {
        }
        } //SW
;
    }break;
    case 22:;
    {
        switch (yt) {
        case 1:;
        {
            (*pc_decls_sptr).varptr = ((*pc_decls_sptr).varptr - (*y).value);
            return (pc_decls_pcptr + (i64)1);
        }break;
        default: {
        }
        } //SW
;
    }break;
    default: {
    }
    } //SW
;
    pc_support_pcmxtypes((byte*)"sub/mixed_def",pc_decls_sptr,y);
    return (pc_decls_pcptr + (i64)1);
}

static u64 * pc_jhandlers_j_mixed_iw_wi(void) {
    (*pc_decls_sptr).tag = (u64)((i64)1);
    return ((*(*pc_decls_opc_tableptr)[((i64)1)]))();
}

static u64 * pc_jhandlers_j_mixed_ir(void) {
    struct pc_decls_varrec *  x;
    x = (pc_decls_sptr + (i64)1);
    (*x).tag = (u64)((i64)3);
    (*x).xvalue = (double)((*x).value);
    return ((*(*pc_decls_opc_tableptr)[((i64)3)]))();
}

static u64 * pc_jhandlers_j_mixed_ri(void) {
    (*pc_decls_sptr).tag = (u64)((i64)3);
    (*pc_decls_sptr).xvalue = (double)((*pc_decls_sptr).value);
    return ((*(*pc_decls_opc_tableptr)[((i64)3)]))();
}

static u64 * pc_jhandlers_j_mixed_ij(void) {
    pc_bignum_bx_makeint((*(pc_decls_sptr + (i64)1)).value,(pc_decls_sptr + (i64)1));
    return ((*(*pc_decls_opc_tableptr)[((i64)7)]))();
}

static u64 * pc_jhandlers_j_mixed_ji(void) {
    pc_bignum_bx_makeint((*pc_decls_sptr).value,pc_decls_sptr);
    return ((*(*pc_decls_opc_tableptr)[((i64)7)]))();
}

u64 * pc_jhandlers_j_mul_i_w(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).value = ((*pc_decls_sptr).value * (*y).value);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_mul_r(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).xvalue *= (*y).xvalue;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_mul_j(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    struct pc_decls_varrec result;
    y = pc_decls_sptr++;
    x = pc_decls_sptr;
    pc_bignum_bx_mul(x,y,&result);
    pc_pcfns_pc_unshare(x);
    pc_pcfns_pc_unshare(y);
    (*pc_decls_sptr) = result;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_mul_z(void) {
    pc_support_pcmxtypes((byte*)"mul_def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_mul_li(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    pc_pcfns_pc_mul_listi(x,y,pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_mul_si(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    pc_pcfns_pc_mul_stri(x,y,pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_mul_e(void) {
    return pc_jhandlers_j_iand_e();
}

u64 * pc_jhandlers_j_mul_zz(void) {
    pc_support_pcmxtypes((byte*)"mul/zz",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_div_i(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).xvalue = ((double)((*pc_decls_sptr).value) / (double)((*y).value));
    (*pc_decls_sptr).tagx = (u64)((i64)3);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_div_r(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).xvalue = ((*pc_decls_sptr).xvalue / (*y).xvalue);
    (*pc_decls_sptr).tagx = (u64)((i64)3);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_div_j(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    struct pc_decls_varrec result;
    y = pc_decls_sptr++;
    x = pc_decls_sptr;
    pc_bignum_bx_div(x,y,&result);
    pc_pcfns_pc_unshare(x);
    pc_pcfns_pc_unshare(y);
    (*pc_decls_sptr) = result;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_div_z(void) {
    pc_support_pcmxtypes((byte*)"div_def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_jumple_i(void) {
    if (((*(pc_decls_sptr + (i64)1)).value <= (*pc_decls_sptr).value)) {
        pc_decls_sptr += (i64)2;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    pc_decls_sptr += (i64)2;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumple_r(void) {
    if (((*(pc_decls_sptr + (i64)1)).xvalue <= (*pc_decls_sptr).xvalue)) {
        pc_decls_sptr += (i64)2;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    pc_decls_sptr += (i64)2;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumple_s(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 res;
    struct pc_decls_objrec *  px;
    struct pc_decls_objrec *  py;
    y = pc_decls_sptr++;
    x = pc_decls_sptr++;
    px = (*x).objptr;
    py = (*y).objptr;
    res = pc_pcfns_cmpstring_len((*px).ustr.strptr,(*py).ustr.strptr,(i64)((*px).ustr.length),(i64)((*py).ustr.length));
    pc_pcfns_pc_unshare(x);
    pc_pcfns_pc_unshare(y);
    if ((res <= (i64)0)) {
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumple_z(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 res;
    y = pc_decls_sptr++;
    x = pc_decls_sptr++;
    res = pc_pcfns_pc_compare(x,y);
    if ((res <= (i64)0)) {
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpeq_i_r_t_o(void) {
    if (((*(pc_decls_sptr + (i64)1)).value == (*pc_decls_sptr).value)) {
        pc_decls_sptr += (i64)2;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    pc_decls_sptr += (i64)2;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpeq_v_p_f_g(void) {
    if (((*(pc_decls_sptr + (i64)1)).refptr == (*pc_decls_sptr).refptr)) {
        pc_decls_sptr += (i64)2;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    pc_decls_sptr += (i64)2;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpeq_s(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 res;
    y = pc_decls_sptr++;
    x = pc_decls_sptr++;
    res = pc_pcfns_pc_eqstring(x,y);
    if (!!(res)) {
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpeq_z(void) {
    i64 res;
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = pc_decls_sptr++;
    res = pc_pcfns_pc_equal(x,y,(i64)0);
    if (!!(res)) {
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpeq_zz(void) {
    return pc_jhandlers_j_jumpeq_z();
}

u64 * pc_jhandlers_j_jumpne_i_r_t_o(void) {
    if (((*(pc_decls_sptr + (i64)1)).value != (*pc_decls_sptr).value)) {
        pc_decls_sptr += (i64)2;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    pc_decls_sptr += (i64)2;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpne_v_p_f_g(void) {
    if (((*(pc_decls_sptr + (i64)1)).refptr != (*pc_decls_sptr).refptr)) {
        pc_decls_sptr += (i64)2;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    pc_decls_sptr += (i64)2;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpne_s(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    struct pc_decls_objrec *  px;
    struct pc_decls_objrec *  py;
    i64 res;
    i64 n;
    y = pc_decls_sptr++;
    x = pc_decls_sptr++;
    px = (*x).objptr;
    py = (*y).objptr;
    res = (i64)0;
    n = (i64)((*px).ustr.length);
    if ((n != (i64)((*py).ustr.length))) {
    } else if ((n == (i64)0)) {
        res = (i64)1;
    } else {
        if ((mlib_cmpstringn((*px).ustr.strptr,(*py).ustr.strptr,n) == (i64)0)) {
            res = (i64)1;
        };
    };
    pc_pcfns_pc_unshare(x);
    pc_pcfns_pc_unshare(y);
    if ((res == (i64)0)) {
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpne_z(void) {
    i64 res;
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = pc_decls_sptr++;
    res = pc_pcfns_pc_equal(x,y,(i64)0);
    if (!(!!(res))) {
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpne_zz(void) {
    return pc_jhandlers_j_jumpne_z();
}

u64 * pc_jhandlers_j_jumpge_i(void) {
    if (((*(pc_decls_sptr + (i64)1)).value >= (*pc_decls_sptr).value)) {
        pc_decls_sptr += (i64)2;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    pc_decls_sptr += (i64)2;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpge_w(void) {
    if (((*(pc_decls_sptr + (i64)1)).uvalue >= (*pc_decls_sptr).uvalue)) {
        pc_decls_sptr += (i64)2;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    pc_decls_sptr += (i64)2;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpge_r(void) {
    if (((*(pc_decls_sptr + (i64)1)).xvalue >= (*pc_decls_sptr).xvalue)) {
        pc_decls_sptr += (i64)2;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    pc_decls_sptr += (i64)2;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpge_z(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 res;
    y = pc_decls_sptr++;
    x = pc_decls_sptr++;
    res = pc_pcfns_pc_compare(x,y);
    if ((res >= (i64)0)) {
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpgt_i(void) {
    if (((*(pc_decls_sptr + (i64)1)).value > (*pc_decls_sptr).value)) {
        pc_decls_sptr += (i64)2;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    pc_decls_sptr += (i64)2;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpgt_w(void) {
    if (((*(pc_decls_sptr + (i64)1)).uvalue > (*pc_decls_sptr).uvalue)) {
        pc_decls_sptr += (i64)2;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    pc_decls_sptr += (i64)2;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpgt_r(void) {
    if (((*(pc_decls_sptr + (i64)1)).xvalue > (*pc_decls_sptr).xvalue)) {
        pc_decls_sptr += (i64)2;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    pc_decls_sptr += (i64)2;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpgt_z(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 res;
    y = pc_decls_sptr++;
    x = pc_decls_sptr++;
    res = pc_pcfns_pc_compare(x,y);
    if ((res > (i64)0)) {
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumplt_i(void) {
    if (((*(pc_decls_sptr + (i64)1)).value < (*pc_decls_sptr).value)) {
        pc_decls_sptr += (i64)2;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    pc_decls_sptr += (i64)2;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumplt_v_p(void) {
    if (((*(pc_decls_sptr + (i64)1)).refptr < (*pc_decls_sptr).refptr)) {
        pc_decls_sptr += (i64)2;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    pc_decls_sptr += (i64)2;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumplt_r(void) {
    if (((*(pc_decls_sptr + (i64)1)).xvalue < (*pc_decls_sptr).xvalue)) {
        pc_decls_sptr += (i64)2;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    pc_decls_sptr += (i64)2;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumplt_z(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 res;
    y = pc_decls_sptr++;
    x = pc_decls_sptr++;
    res = pc_pcfns_pc_compare(x,y);
    if ((res < (i64)0)) {
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_pushix_li(void) {
    struct pc_decls_objrec *  p;
    struct pc_decls_varrec va;
    i64 index;
    va = (*(pc_decls_sptr + (i64)1));
    p = va.objptr;
    index = ((*pc_decls_sptr).value - (i64)((*p).ulist.lower));
    if (((u64)((u32)(index)) >= (u64)((*p).ulist.length))) {
        pc_support_pcerror((byte*)"list[int] bounds");
    };
    (*++pc_decls_sptr) = (*((*p).ulist.vptr + index));
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        ++(*(*pc_decls_sptr).objptr).ulist.refcount;
    };
    if (!!((u64)(va.hasref))) {
        pc_pcfns_pc_unshare(&va);
    };
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushix_mi(void) {
    struct pc_decls_objrec *  p;
    struct pc_decls_varrec va;
    i64 index;
    va = (*(pc_decls_sptr + (i64)1));
    p = va.objptr;
    index = ((*pc_decls_sptr).value - (i64)1);
    if (((u64)((u32)(index)) >= (u64)(pc_decls_ttlength[((i64)(va.tag))]))) {
        pc_support_pcerror((byte*)"rec[int] bounds");
    };
    (*++pc_decls_sptr) = (*((*p).urec.vptr + index));
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        ++(*(*pc_decls_sptr).objptr).refcount;
    };
    pc_pcfns_pc_unshare(&va);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushix_vi(void) {
    i64 index;
    index = (*pc_decls_sptr).value;
    ++pc_decls_sptr;
    (*pc_decls_sptr) = (*((*pc_decls_sptr).varptr + index));
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushix_ln(void) {
    struct pc_decls_varrec v;
    struct pc_decls_varrec *  a;
    struct pc_decls_varrec *  x;
    i64 i;
    i64 j;
    i64 alower;
    struct pc_decls_objrec *  p;
    struct pc_decls_objrec *  q;
    x = pc_decls_sptr++;
    a = pc_decls_sptr;
    p = (*a).objptr;
    i = (i64)((*x).range_lower);
    j = (i64)((*x).range_upper);
    alower = (i64)((*p).ulist.lower);
    if ((((i < alower) || (j > (((i64)((u64)((*p).ulist.length)) + alower) - (i64)1))) || (i > j))) {
        pc_support_pcerror((byte*)"list/slice bounds");
    };
    (*pc_decls_sptr).tagx = (u64)((i64)65565);
    q = pc_objlib_obj_new((i64)29);
    (*pc_decls_sptr).objptr = q;
    (*q).objtype = (u64)((i64)1);
    (*q).ulist.mutable = (u64)((*p).ulist.mutable);
    (*q).ulist.lower = (i64)1;
    if (((i64)((*p).objtype)==(i64)1)) {
        (*q).ulist.objptr2 = (*p).ulist.objptr2;
        ++(*(*q).objptr2).ulist.refcount;
        (*q).ulist.vptr = (((*p).ulist.vptr + i) - alower);
        v.tagx = (u64)((i64)65565);
        v.objptr = p;
        pc_pcfns_pc_unshare(&v);
    }else if (((i64)((*p).objtype)==(i64)2)) {
        (*q).objptr2 = (struct pc_decls_objrec *)(0);
        (*q).objtype = (u64)((i64)2);
        (*q).ulist.vptr = (((*p).ulist.vptr + i) - alower);
    } else {
        (*q).ulist.objptr2 = p;
        (*q).ulist.vptr = (((*p).ulist.vptr + i) - alower);
    };
    (*q).ulist.length = (u64)(((j - i) + (i64)1));
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushix_ll(void) {
    pc_support_junimpl((byte*)"pushix_listlist");
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushix_le(void) {
    pc_support_junimpl((byte*)"pushix_list_set");
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushix_ai(void) {
    struct pc_decls_objrec *  p;
    i64 index;
    i64 elemtype;
    struct pc_decls_varrec va;
    va = (*(pc_decls_sptr + (i64)1));
    p = va.objptr;
    index = ((*pc_decls_sptr).value - (i64)((*p).uarray.lower));
    if (((u64)((u32)(index)) >= (u64)((*p).uarray.length))) {
        pc_support_pcerror((byte*)"ax[int] bounds");
    };
    if (((elemtype = (i64)((*p).uarray.elemtag)) == (i64)44)) {
        ++pc_decls_sptr;
        (*pc_decls_sptr).value = (i64)((*((*p).uarray.ptr + index)));
        (*pc_decls_sptr).tagx = (u64)((i64)1);
    } else {
        pc_pcfns_pc_loadpacked((void *)(((*p).uarray.ptr + (index * pc_decls_ttsize[(elemtype)]))),elemtype,++pc_decls_sptr,p);
    };
    if (!!((u64)(va.hasref))) {
        pc_pcfns_pc_unshare(&va);
    };
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushix_bi_ei(void) {
    struct pc_decls_objrec *  p;
    i64 index;
    i64 offset;
    i64 shift;
    byte *  q;
    struct pc_decls_varrec va;
    va = (*(pc_decls_sptr + (i64)1));
    p = va.objptr;
    index = ((*pc_decls_sptr).value - (i64)((*p).ubits.lower));
    offset = (i64)((*p).ubits.bitoffset);
    q = (*p).ubits.ptr;
    if (((u64)((u32)(index)) >= (u64)((*p).ubits.length))) {
        pc_support_pcerror((byte*)"bits[int] bounds");
    };
    ++pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    switch ((i64)((*p).ubits.elemtag)) {
    case 41:;
    {
        index += offset;
        (*pc_decls_sptr).value = !!(((i64)((u64)((*(q + (index >> (i64)3))))) & ((i64)1 << (index & (i64)7))));
    }break;
    case 42:;
    {
        index += (offset >> (i64)1);
        shift = ((index & (i64)3) * (i64)2);
        (*pc_decls_sptr).value = (((i64)((u64)((*(q + (index >> (i64)2))))) & ((i64)3 << shift)) >> shift);
    }break;
    case 43:;
    {
        index += (offset >> (i64)2);
        shift = ((index & (i64)1) * (i64)4);
        (*pc_decls_sptr).value = (((i64)((u64)((*(q + (index >> (i64)1))))) & ((i64)15 << shift)) >> shift);
    }break;
    default: {
        pc_support_pcustypet((byte*)"bitix",(i64)((*p).ubits.elemtag));
    }
    } //SW
;
    if (!!((u64)(va.hasref))) {
        pc_pcfns_pc_unshare(&va);
    };
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushix_an(void) {
    struct pc_decls_varrec v;
    struct pc_decls_varrec *  a;
    struct pc_decls_varrec *  x;
    i64 i;
    i64 j;
    i64 offset;
    struct pc_decls_objrec *  p;
    struct pc_decls_objrec *  q;
    x = pc_decls_sptr++;
    a = pc_decls_sptr;
    p = (*a).objptr;
    i = (i64)((*x).range_lower);
    j = (i64)((*x).range_upper);
    if ((((i < (i64)((*p).uarray.lower)) || (j > (i64)((u64)((*p).uarray.length)))) || (i > j))) {
        pc_support_pcerror((byte*)"ax[slice] bounds");
    };
    (*pc_decls_sptr).tagx = (u64)((*a).tagx);
    q = pc_objlib_obj_new((i64)((*a).tagx));
    (*pc_decls_sptr).objptr = q;
    (*q).objtype = (u64)((i64)1);
    (*q).uarray.mutable = (u64)((*p).uarray.mutable);
    (*q).uarray.elemtag = (i64)((*p).uarray.elemtag);
    (*q).uarray.lower = (i64)((*p).uarray.lower);
    offset = ((i - (i64)((*p).uarray.lower)) * pc_decls_ttsize[((i64)((*p).uarray.elemtag))]);
    if (((i64)((*p).objtype)==(i64)1)) {
        (*q).uarray.objptr2 = (*p).uarray.objptr2;
        ++(*(*q).objptr2).refcount;
        (*q).uarray.ptr = ((*p).uarray.ptr + offset);
        v.tagx = (u64)((i64)65541);
        v.objptr = p;
        pc_pcfns_pc_unshare(&v);
    }else if (((i64)((*p).objtype)==(i64)2)) {
        (*q).uarray.objptr2 = (struct pc_decls_objrec *)(0);
        (*q).objtype = (u64)((i64)2);
        (*q).uarray.ptr = ((*p).uarray.ptr + offset);
    } else {
        (*q).uarray.objptr2 = p;
        (*q).uarray.ptr = ((*p).uarray.ptr + offset);
    };
    (*q).uarray.length = (u64)(((j - i) + (i64)1));
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushix_si(void) {
    struct pc_decls_varrec v;
    struct pc_decls_varrec *  a;
    struct pc_decls_varrec *  x;
    i64 i;
    struct pc_decls_objrec *  p;
    struct pc_decls_objrec *  q;
    x = pc_decls_sptr++;
    a = pc_decls_sptr;
    p = (*a).objptr;
    i = ((*x).value - (i64)1);
    if (((u64)((u32)(i)) >= (u64)((u32)((*p).ustr.length)))) {
        pc_support_pcerror((byte*)"string[int] bounds");
    };
    (*pc_decls_sptr).tagx = (u64)((i64)65541);
    q = pc_objlib_obj_new((i64)5);
    (*pc_decls_sptr).objptr = q;
    (*q).objtype = (u64)((i64)1);
    (*q).ustr.mutable = (u64)((*p).ustr.mutable);
    if (((i64)((*p).objtype)==(i64)1)) {
        (*q).ustr.objptr2 = (*p).ustr.objptr2;
        ++(*(*q).objptr2).refcount;
        (*q).ustr.strptr = ((*p).ustr.strptr + i);
        v.tagx = (u64)((i64)65541);
        v.objptr = p;
        pc_pcfns_pc_unshare(&v);
    }else if (((i64)((*p).objtype)==(i64)2)) {
        (*q).ustr.objptr2 = (struct pc_decls_objrec *)(0);
        (*q).objtype = (u64)((i64)2);
        (*q).ustr.strptr = ((*p).ustr.strptr + i);
    } else {
        (*q).ustr.objptr2 = p;
        (*q).ustr.strptr = ((*p).ustr.strptr + i);
    };
    (*q).ustr.length = (i64)1;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushix_sn(void) {
    return pc_jhandlers_j_pushdotix_sn();
}

u64 * pc_jhandlers_j_pushix_bn(void) {
    struct pc_decls_varrec v;
    struct pc_decls_varrec *  a;
    struct pc_decls_varrec *  x;
    i64 i;
    i64 j;
    struct pc_decls_objrec *  p;
    struct pc_decls_objrec *  q;
    x = pc_decls_sptr++;
    a = pc_decls_sptr;
    p = (*a).objptr;
    i = (i64)((*x).range_lower);
    j = (i64)((*x).range_upper);
    if ((((i < (i64)((*p).ubits.lower)) || (j > (i64)((u64)((*p).ubits.length)))) || (i > j))) {
        pc_support_pcerror((byte*)"bits[slice] bounds");
    };
    (*pc_decls_sptr).tagx = (u64)((*a).tagx);
    q = pc_objlib_obj_new((i64)31);
    (*pc_decls_sptr).objptr = q;
    (*q).objtype = (u64)((i64)1);
    (*q).ubits.mutable = (u64)((*p).ubits.mutable);
    (*q).ubits.elemtag = (u64)((*p).ubits.elemtag);
    (*q).ubits.lower = (i64)((*p).ubits.lower);
    (*q).ubits.ptr = pc_pcfns_getbitoffset((*p).ubits.ptr,(i64)((*p).ubits.bitoffset),(i - (i64)((*p).ubits.lower)),(i64)((*p).ubits.elemtag),&(*q).ubits.bitoffset);
    ++(*q).ubits.bitoffset;
    if (((i64)((*p).objtype)==(i64)1)) {
        (*q).ubits.objptr2 = (*p).ubits.objptr2;
        ++(*(*q).objptr2).refcount;
        v.tagx = (u64)((i64)65541);
        v.objptr = p;
        pc_pcfns_pc_unshare(&v);
    }else if (((i64)((*p).objtype)==(i64)2)) {
        (*q).ubits.objptr2 = (struct pc_decls_objrec *)(0);
        (*q).objtype = (u64)((i64)2);
    } else {
        (*q).ubits.objptr2 = p;
    };
    (*q).ubits.length = (u64)(((j - i) + (i64)1));
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushix_di(void) {
    struct pc_decls_varrec *  a;
    struct pc_decls_varrec *  x;
    i64 i;
    i64 j;
    i64 alower;
    struct pc_decls_objrec *  p;
    struct pc_decls_objrec *  q;
    x = pc_decls_sptr++;
    a = pc_decls_sptr;
    p = (*a).objptr;
    i = (*x).value;
    j = (i64)((*x).range_upper);
    alower = (i64)((*p).udict.lower);
    if (((i < (i64)1) || (j > (i64)((u64)((*p).udict.allocated))))) {
        pc_support_pcerror((byte*)"dict[] bounds");
    };
    (*pc_decls_sptr).tagx = (u64)((i64)65565);
    q = pc_objlib_obj_new((i64)10);
    (*pc_decls_sptr).objptr = q;
    (*q).objtype = (u64)((i64)1);
    (*q).udict.lower = (i64)1;
    (*q).udict.length = (u64)((i64)2);
    (*q).udict.objptr2 = p;
    (*q).udict.vptr = ((*p).udict.vptr + ((i - (i64)1) * (i64)2));
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushix_zz(void) {
    pc_support_pcmxtypes((byte*)"pushix/def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (u64 *)(0);
}

u64 * pc_jhandlers_j_pushixref_li(void) {
    struct pc_decls_objrec *  p;
    struct pc_decls_varrec *  a;
    i64 index;
    a = (*(pc_decls_sptr + (i64)1)).varptr;
    p = (*a).objptr;
    if (!(!!((u64)((*p).ulist.mutable)))) {
        p = ((*a).objptr = pc_objlib_copyonwrite(p,(i64)29));
    };
    index = ((*pc_decls_sptr).value - (i64)((*p).ulist.lower));
    if (((u64)((u32)(index)) >= (u64)((*p).ulist.length))) {
        if ((index < (i64)0)) {
            pc_support_pcerror((byte*)"LWB");
        } else {
            if ((((u64)((u32)(index)) == (u64)((*p).ulist.length)) && ((i64)((u64)((*a).tag)) == (i64)29))) {
                pc_pcfns_pc_iappendlist(a,(struct pc_decls_varrec *)(0));
                p = (*a).objptr;
            } else {
                pc_support_pcerror((byte*)"&pushix list[i] bounds");
            };
        };
    };
    (*++pc_decls_sptr).tagx = (u64)((i64)22);
    (*pc_decls_sptr).varptr = ((*p).ulist.vptr + index);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushixref_ln(void) {
    pc_support_junimpl((byte*)"pushixref_list_range");
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushixref_ai(void) {
    i64 index;
    i64 elemtype;
    struct pc_decls_objrec *  p;
    struct pc_decls_varrec *  a;
    a = (*(pc_decls_sptr + (i64)1)).varptr;
    p = (*a).objptr;
    index = ((*pc_decls_sptr).value - (i64)((*p).uarray.lower));
    if (((u64)((u32)(index)) >= (u64)((*p).uarray.length))) {
        if ((index < (i64)0)) {
            pc_support_pcerror((byte*)"&AXLWB");
        } else {
            if (((u64)((u32)(index)) == (u64)((*p).uarray.length))) {
                pc_pcfns_pc_iappendarray(a,(struct pc_decls_varrec *)(0));
                p = (*a).objptr;
            } else {
                pc_support_pcerror((byte*)"&AXBOUNDS");
            };
        };
    };
    elemtype = (i64)((*p).uarray.elemtag);
    ++pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)23);
    (*pc_decls_sptr).uref.elemtag = (u64)(elemtype);
    (*pc_decls_sptr).uref.ptr = ((*p).uarray.ptr + (index * pc_decls_ttsize[(elemtype)]));
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushixref_si(void) {
    struct pc_decls_varrec *  a;
    i64 index;
    struct pc_decls_objrec *  pa;
    a = (*(pc_decls_sptr + (i64)1)).varptr;
    pa = (*a).objptr;
    index = ((*pc_decls_sptr).value - (i64)1);
    if (!(!!((u64)((*pa).ustr.mutable)))) {
        (*a).objptr = (pa = pc_objlib_copyonwrite(pa,(i64)5));
    };
    if (((u64)((u32)(index)) >= (u64)((u32)((*pa).ustr.length)))) {
        pc_support_pcerror((byte*)"&str[int] bounds");
    };
    (*++pc_decls_sptr).tagx = (u64)((i64)23);
    (*pc_decls_sptr).uref.elemtag = (u64)((i64)5);
    (*pc_decls_sptr).uref.ptr = ((byte *)((*pa).ustr.strptr) + index);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushixref_bi(void) {
    i64 index;
    i64 elemtype;
    struct pc_decls_objrec *  p;
    struct pc_decls_varrec *  a;
    a = (*(pc_decls_sptr + (i64)1)).varptr;
    p = (*a).objptr;
    index = ((*pc_decls_sptr).value - (i64)((*p).ubits.lower));
    if (((u64)((u32)(index)) >= (u64)((*p).ubits.length))) {
        if ((index < (i64)0)) {
            pc_support_pcerror((byte*)"&BITSLWB");
        } else {
            if (((u64)((u32)(index)) == (u64)((*p).ubits.length))) {
                pc_pcfns_pc_iappendbits(a,(struct pc_decls_varrec *)(0));
                p = (*a).objptr;
            } else {
                pc_support_pcerror((byte*)"&BITSBOUNDS");
            };
        };
    };
    elemtype = (i64)((*p).ubits.elemtag);
    ++pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)24);
    (*pc_decls_sptr).uref.elemtag = (u64)(elemtype);
    (*pc_decls_sptr).uref.ptr = pc_pcfns_getbitoffset((*p).ubits.ptr,(i64)((*p).ubits.bitoffset),index,elemtype,&(*pc_decls_sptr).uref.bitoffset);
    (*pc_decls_sptr).uref.bitlength = (u64)((i64)0);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushixref_zz(void) {
    pc_support_pcerror((byte*)"JPUSHIXREF/DEF");
    return (u64 *)(0);
}

u64 * pc_jhandlers_j_pushdotix_si(void) {
    struct pc_decls_varrec *  a;
    struct pc_decls_varrec *  x;
    i64 index;
    i64 value;
    struct pc_decls_objrec *  p;
    x = pc_decls_sptr++;
    a = pc_decls_sptr;
    p = (*a).objptr;
    index = ((*x).value - (i64)1);
    if (((u64)((u32)(index)) >= (u64)((u32)((*p).ustr.length)))) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"INDEX=",NULL);
        msysnewc_m_print_i64(index,NULL);
        msysnewc_m_print_str((byte*)"P.USTR.LENGTH=",NULL);
        msysnewc_m_print_i64((*p).ustr.length,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcerror((byte*)"string.[int] bounds");
    };
    value = (i64)((*((*p).ustr.strptr + index)));
    if (!!((u64)((*a).hasref))) {
        pc_pcfns_pc_unshare(a);
    };
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = value;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushdotix_sn(void) {
    struct pc_decls_varrec v;
    struct pc_decls_varrec *  a;
    struct pc_decls_varrec *  x;
    i64 i;
    i64 j;
    struct pc_decls_objrec *  p;
    struct pc_decls_objrec *  q;
    x = pc_decls_sptr++;
    a = pc_decls_sptr;
    p = (*a).objptr;
    i = (i64)((*x).range_lower);
    j = (i64)((*x).range_upper);
    if ((((i < (i64)1) || (j > (i64)((*p).ustr.length))) || (i > j))) {
        if ((i == (j + (i64)1))) {
            (*pc_decls_sptr).objptr = pc_objlib_emptystring;
            (*pc_decls_sptr).tagx = (u64)((i64)65541);
            ++(*pc_objlib_emptystring).refcount;
            return (pc_decls_pcptr + (i64)1);
        };
        pc_support_pcerror((byte*)"string[slice] bounds");
    };
    (*pc_decls_sptr).tagx = (u64)((i64)65541);
    q = pc_objlib_obj_new((i64)5);
    (*pc_decls_sptr).objptr = q;
    (*q).objtype = (u64)((i64)1);
    (*q).ustr.mutable = (u64)((*p).ustr.mutable);
    if (((i64)((*p).objtype)==(i64)1)) {
        (*q).ustr.objptr2 = (*p).ustr.objptr2;
        ++(*(*q).objptr2).refcount;
        (*q).ustr.strptr = (((*p).ustr.strptr + i) - (i64)1);
        v.tagx = (u64)((i64)65541);
        v.objptr = p;
        pc_pcfns_pc_unshare(&v);
    }else if (((i64)((*p).objtype)==(i64)2)) {
        (*q).ustr.objptr2 = (struct pc_decls_objrec *)(0);
        (*q).objtype = (u64)((i64)2);
        (*q).ustr.strptr = (((*p).ustr.strptr + i) - (i64)1);
    } else {
        (*q).ustr.objptr2 = p;
        (*q).ustr.strptr = (((*p).ustr.strptr + i) - (i64)1);
    };
    (*q).ustr.length = ((j - i) + (i64)1);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushdotix_ii(void) {
    i64 index;
    index = (*pc_decls_sptr).value;
    ++pc_decls_sptr;
    if (((index < (i64)0) || (index >= (i64)64))) {
        pc_support_pcerror((byte*)"int.[int] bounds");
    };
    (*pc_decls_sptr).value = (((*pc_decls_sptr).value >> index) & (i64)1);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushdotix_in(void) {
    i64 i;
    i64 j;
    i = (i64)((*pc_decls_sptr).range_lower);
    j = (i64)((*pc_decls_sptr).range_upper);
    ++pc_decls_sptr;
    if ((j < i)) {
        {i64 temp = i; i = j; j = temp; };
    };
    if (((i >= (i64)64) || (j >= (i64)64))) {
        pc_support_pcerror((byte*)"int.[slice] bounds");
    };
    (*pc_decls_sptr).value = (((*pc_decls_sptr).value >> i) & (i64)(~(((u64)18446744073709551615u << ((j - i) + (i64)1)))));
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushdotix_ei(void) {
    return pc_jhandlers_j_pushix_bi_ei();
}

u64 * pc_jhandlers_j_pushdotix_mi(void) {
    return pc_jhandlers_j_pushix_mi();
}

u64 * pc_jhandlers_j_pushdotix_zz(void) {
    pc_support_pcmxtypes((byte*)"pushdotix/def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (u64 *)(0);
}

u64 * pc_jhandlers_j_pushdotixref_si(void) {
    struct pc_decls_varrec *  a;
    i64 index;
    struct pc_decls_objrec *  pa;
    a = (*(pc_decls_sptr + (i64)1)).varptr;
    pa = (*a).objptr;
    index = ((*pc_decls_sptr).value - (i64)1);
    if (((u64)((u32)(index)) >= (u64)((u32)((*pa).ustr.length)))) {
        pc_support_pcerror((byte*)"&str.[int] bounds");
    };
    (*++pc_decls_sptr).tagx = (u64)((i64)23);
    (*pc_decls_sptr).uref.elemtag = (u64)((i64)44);
    (*pc_decls_sptr).uref.ptr = ((byte *)((*pa).ustr.strptr) + index);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushdotixref_sn(void) {
    pc_support_pcerror((byte*)"JPUSHDOTIXREF/SRANGE");
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushdotixref_ii(void) {
    i64 index;
    byte *  p;
    index = (*pc_decls_sptr).value;
    ++pc_decls_sptr;
    if (((index < (i64)0) || (index >= (i64)64))) {
        pc_support_pcerror((byte*)"int.[int] bounds");
    };
    p = (*pc_decls_sptr).uref.ptr;
    (*pc_decls_sptr).tagx = (u64)((i64)24);
    (*pc_decls_sptr).uref.elemtag = (u64)((i64)41);
    (*pc_decls_sptr).uref.bitoffset = (u64)((index & (i64)7));
    (*pc_decls_sptr).uref.ptr = ((p + (i64)8) + (index >> (i64)3));
    (*pc_decls_sptr).uref.bitlength = (u64)((i64)0);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushdotixref_in_wn(void) {
    i64 i;
    i64 j;
    i = (i64)((*pc_decls_sptr).range_lower);
    j = (i64)((*pc_decls_sptr).range_upper);
    if ((i > j)) {
        {i64 temp = i; i = j; j = temp; };
    };
    ++pc_decls_sptr;
    if (((i < (i64)0) || (j >= (i64)64))) {
        pc_support_pcerror((byte*)"int.[slice] bounds");
    };
    (*pc_decls_sptr).tagx = (u64)((i64)24);
    (*pc_decls_sptr).uref.bitlength = (u64)(((j - i) + (i64)1));
    (*pc_decls_sptr).uref.elemtag = (u64)((i64)41);
    (*pc_decls_sptr).uref.bitoffset = (u64)(i);
    ++(*pc_decls_sptr).uref.ptr64;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_pushdotixref_ei(void) {
    return pc_support_pcerror((byte*)"PUSHDOTIXREF/SET");
}

u64 * pc_jhandlers_j_pushdotixref_zz(void) {
    pc_support_pcmxtypes((byte*)"ZZpushdotixref/def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (u64 *)(0);
}

u64 * pc_jhandlers_j_addto_i(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).value += (*y).value;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_addto_r(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).xvalue += (*y).xvalue;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_addto_s(void) {
    i64 xlen;
    i64 ylen;
    i64 newlen;
    struct pc_decls_objrec *  px;
    struct pc_decls_objrec *  py;
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    px = (*x).objptr;
    py = (*y).objptr;
    if (!(!!((u64)((*px).ustr.mutable)))) {
        (*x).objptr = (px = pc_objlib_copyonwrite(px,(i64)5));
    };
    if (((i64)((u64)((*px).objtype)) != (i64)0)) {
        pc_support_pcerror((byte*)"extending string slice");
    };
    xlen = (i64)((*px).ustr.length);
    ylen = (i64)((*py).ustr.length);
    if ((xlen == (i64)0)) {
        if (!!(ylen)) {
            (*px).ustr.strptr = (byte *)(mlib_pcm_alloc(ylen));
            (*px).ustr.allocated = (u64)(mlib_allocbytes);
            (*px).ustr.length = ylen;
            memcpy((void *)((*px).ustr.strptr),(void *)((*py).ustr.strptr),(u64)(ylen));
        };
    } else if ((ylen == (i64)1)) {
        if ((++xlen > (i64)((u64)((*px).ustr.allocated)))) {
            pc_objlib_string_resize(px,xlen);
        };
        (*px).ustr.length = xlen;
        (*(((*px).ustr.strptr + xlen) - (i64)1)) = (u64)((*(*py).ustr.strptr));
    } else if (!!(ylen)) {
        newlen = (xlen + ylen);
        if ((newlen > (i64)((u64)((*px).ustr.allocated)))) {
            pc_objlib_string_resize(px,newlen);
        };
        (*px).ustr.length = newlen;
        memcpy((void *)(((*px).ustr.strptr + xlen)),(void *)((*py).ustr.strptr),(u64)(ylen));
    };
    pc_pcfns_pc_unshare(y);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_addto_p(void) {
    pc_support_pcerror((byte*)"ADDTO REF PACK");
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_addto_z(void) {
    struct pc_decls_objrec *  px;
    struct pc_decls_objrec *  py;
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    struct pc_decls_varrec result;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    px = (*x).objptr;
    py = (*y).objptr;
    if (((i64)((*y).tag)==(i64)7)) {
        pc_bignum_bx_add(x,y,&result);
        pc_pcfns_pc_unshare(x);
        pc_pcfns_pc_unshare(y);
        (*x) = result;
    } else {
        pc_support_pcmxtypes((byte*)"addto_def",x,y);
    };
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_addto_si(void) {
    i64 ch;
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 xlen;
    struct pc_decls_objrec *  px;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    ch = (*y).value;
    px = (*x).objptr;
    if (!(!!((u64)((*px).ustr.mutable)))) {
        (*x).objptr = (px = pc_objlib_copyonwrite(px,(i64)5));
    };
    xlen = (i64)((*px).ustr.length);
    if ((xlen == (i64)0)) {
        (*px).ustr.strptr = (byte *)(mlib_pcm_alloc((i64)1));
        (*px).ustr.allocated = (u64)(mlib_allocbytes);
        (*px).ustr.length = (i64)1;
        (*(*px).ustr.strptr) = (u64)(ch);
    } else {
        if ((++xlen > (i64)((u64)((*px).ustr.allocated)))) {
            pc_objlib_string_resize(px,xlen);
        };
        (*px).ustr.length = xlen;
        (*(((*px).ustr.strptr + xlen) - (i64)1)) = (u64)(ch);
    };
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_addto_ir(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).value += (i64)((*y).xvalue);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_addto_ri(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).xvalue += (double)((*y).value);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_addto_zz(void) {
    pc_support_pcmxtypes((byte*)"addto_zz",(*(pc_decls_sptr + (i64)1)).varptr,pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_subto_i(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).value -= (*y).value;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_subto_r(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).xvalue -= (*y).xvalue;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_subto_z(void) {
    pc_support_pcmxtypes((byte*)"subto_def",(*(pc_decls_sptr + (i64)1)).varptr,pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_subto_ir(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).value -= (i64)((*y).xvalue);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_subto_ri(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).xvalue -= (double)((*y).value);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_subto_zz(void) {
    pc_support_pcmxtypes((byte*)"subto_zz",(*(pc_decls_sptr + (i64)1)).varptr,pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_multo_i_w(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).value *= (*y).value;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_multo_r(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).xvalue *= (*y).xvalue;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_multo_z(void) {
    pc_support_pcmxtypes((byte*)"multo_def",(*(pc_decls_sptr + (i64)1)).varptr,pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_divto_i_w(void) {
    pc_support_pcerror((byte*)"divto int?");
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_divto_r(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).xvalue /= (*y).xvalue;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_divto_z(void) {
    pc_support_pcmxtypes((byte*)"divto_def",(*(pc_decls_sptr + (i64)1)).varptr,pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_idivto_i(void) {
    pc_support_pcustype((byte*)"idivto",pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_idivto_z(void) {
    pc_support_pcmxtypes((byte*)"idivto_def",(*(pc_decls_sptr + (i64)1)).varptr,pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_iand_i_w(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).value &= (*y).value;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_iand_e(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    pc_dxfns_dx_iandset(x,y);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_iand_z(void) {
    pc_support_pcmxtypes((byte*)"iand_def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_ior_i_w(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).value |= (*y).value;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_ior_e(void) {
    return pc_jhandlers_j_add_e();
}

u64 * pc_jhandlers_j_ior_z(void) {
    pc_support_pcmxtypes((byte*)"ior_def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_ixor_i_w(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).value ^= (*y).value;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_ixor_e(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = pc_decls_sptr;
    pc_dxfns_dx_ixorset(x,y);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_ixor_z(void) {
    pc_support_pcmxtypes((byte*)"ixor_def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_iandto_i_w(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).value &= (*y).value;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_iandto_z(void) {
    pc_support_pcmxtypes((byte*)"iandto_def",(*(pc_decls_sptr + (i64)1)).varptr,pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_iorto_i_w(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).value |= (*y).value;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_iorto_z(void) {
    pc_support_pcmxtypes((byte*)"iorto_def",(*(pc_decls_sptr + (i64)1)).varptr,pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_ixorto_i_w(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).value ^= (*y).value;
    return (pc_decls_pcptr + (i64)1);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_ixorto_z(void) {
    pc_support_pcmxtypes((byte*)"ixorto_def",(*(pc_decls_sptr + (i64)1)).varptr,pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_shlto_i(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).value = ((*x).value << (*y).value);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_shlto_z(void) {
    pc_support_pcmxtypes((byte*)"shlto_def",(*(pc_decls_sptr + (i64)1)).varptr,pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_shrto_i(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).value = ((*x).value >> (*y).value);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_shrto_z(void) {
    pc_support_pcmxtypes((byte*)"shrto_def",(*(pc_decls_sptr + (i64)1)).varptr,pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_concat_s(void) {
    return pc_jhandlers_j_add_s();
}

u64 * pc_jhandlers_j_concat_l(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    pc_pcfns_pc_duplvar(x);
    pc_pcfns_pc_iconcatlist(x,y);
    if (!!((u64)((*y).hasref))) {
        pc_pcfns_pc_unshare(y);
    };
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_concat_z(void) {
    pc_support_pcmxtypes((byte*)"concat_def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_concatto_s(void) {
    pc_support_junimpl((byte*)"concatto_string");
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_concatto_l(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    pc_pcfns_pc_iconcatlist(x,y);
    if (!!((u64)((*y).hasref))) {
        pc_pcfns_pc_unshare(y);
    };
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_concatto_z(void) {
    pc_support_pcmxtypes((byte*)"concatto_def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_append_s(void) {
    if (((i64)((u64)((*pc_decls_sptr).tag)) != (i64)5)) {
        pc_support_pcustype((byte*)"append/s",pc_decls_sptr);
    };
    return pc_jhandlers_j_add_s();
}

u64 * pc_jhandlers_j_append_l(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    pc_pcfns_pc_duplvar(x);
    pc_pcfns_pc_iappendlist(x,y);
    if (!!((u64)((*y).hasref))) {
        pc_pcfns_pc_unshare(y);
    };
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_append_a(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    pc_pcfns_pc_duplvar(x);
    pc_pcfns_pc_iappendarray(x,y);
    if (!!((u64)((*y).hasref))) {
        pc_pcfns_pc_unshare(y);
    };
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_append_b(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    pc_pcfns_pc_duplvar(x);
    pc_pcfns_pc_iappendbits(x,y);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_append_z(void) {
    pc_support_pcmxtypes((byte*)"append_def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_appendto_s(void) {
    return pc_jhandlers_j_addto_s();
}

u64 * pc_jhandlers_j_appendto_l(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    pc_pcfns_pc_iappendlist(x,y);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_appendto_a(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    pc_pcfns_pc_iappendarray(x,y);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_appendto_b_e(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    pc_pcfns_pc_iappendbits(x,y);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_appendto_z(void) {
    pc_support_pcmxtypes((byte*)"appendto_def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_max_i(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).value=((*pc_decls_sptr).value>(*y).value?(*pc_decls_sptr).value:(*y).value);
;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_max_r(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).xvalue=((*pc_decls_sptr).xvalue>(*y).xvalue?(*pc_decls_sptr).xvalue:(*y).xvalue);
;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_max_z(void) {
    i64 res;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    res = pc_pcfns_pc_compare_nf(pc_decls_sptr,y);
    if ((res >= (i64)0)) {
        if (!!((u64)((*y).hasref))) {
            pc_pcfns_pc_unshare(y);
        };
    } else {
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            pc_pcfns_pc_unshare(pc_decls_sptr);
        };
        (*pc_decls_sptr) = (*y);
    };
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_min_z(void) {
    i64 res;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    res = pc_pcfns_pc_compare_nf(pc_decls_sptr,y);
    if ((res <= (i64)0)) {
        if (!!((u64)((*y).hasref))) {
            pc_pcfns_pc_unshare(y);
        };
    } else {
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            pc_pcfns_pc_unshare(pc_decls_sptr);
        };
        (*pc_decls_sptr) = (*y);
    };
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_len_l_a_e_s_b_d(void) {
    i64 length;
    length = (i64)((*(*pc_decls_sptr).objptr).ulist.length);
    pc_pcfns_pc_unshare(pc_decls_sptr);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = length;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_len_m_k(void) {
    i64 length;
    length = (i64)(pc_decls_ttlength[((i64)((*pc_decls_sptr).tag))]);
    pc_pcfns_pc_unshare(pc_decls_sptr);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = length;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_len_n(void) {
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = (((i64)((*pc_decls_sptr).range_upper) - (i64)((*pc_decls_sptr).range_lower)) + (i64)1);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_len_z(void) {
    if (((i64)((*pc_decls_sptr).tag)==(i64)7)) {
        (*pc_decls_sptr).value = pc_bignum_bx_length(pc_decls_sptr);
    }else if (((i64)((*pc_decls_sptr).tag)==(i64)10)) {
        (*pc_decls_sptr).value = ((i64)((u64)((*(*pc_decls_sptr).objptr).udict.allocated)) / (i64)2);
    }else if (((i64)((*pc_decls_sptr).tag)==(i64)1)) {
        (*pc_decls_sptr).value = (i64)64;
        (*pc_decls_sptr).tagx = (u64)((i64)1);
    } else {
        pc_support_pcustype((byte*)"len_def",pc_decls_sptr);
    };
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_lwb_l(void) {
    i64 n;
    struct pc_decls_objrec *  p;
    p = (*pc_decls_sptr).objptr;
    n = (i64)((*p).ulist.lower);
    pc_pcfns_pc_unshare(pc_decls_sptr);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = n;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_lwb_a_b(void) {
    i64 n;
    struct pc_decls_objrec *  p;
    p = (*pc_decls_sptr).objptr;
    n = (i64)((*p).uarray.lower);
    pc_pcfns_pc_unshare(pc_decls_sptr);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = n;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_lwb_s_m_k_d(void) {
    pc_pcfns_pc_unshare(pc_decls_sptr);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = (i64)1;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_lwb_e(void) {
    pc_pcfns_pc_unshare(pc_decls_sptr);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = (i64)0;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_lwb_n(void) {
    (*pc_decls_sptr).value = (i64)((*pc_decls_sptr).range_lower);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_lwb_z(void) {
    if (((i64)((*pc_decls_sptr).tag)==(i64)1)) {
        (*pc_decls_sptr).value = (i64)0;
        (*pc_decls_sptr).tagx = (u64)((i64)1);
    } else {
        pc_support_pcustype((byte*)"lwb_def",pc_decls_sptr);
    };
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_upb_l(void) {
    i64 n;
    struct pc_decls_objrec *  p;
    p = (*pc_decls_sptr).objptr;
    n = (i64)((u32)((((i64)((u64)((*p).ulist.length)) + (i64)((*p).ulist.lower)) - (i64)1)));
    pc_pcfns_pc_unshare(pc_decls_sptr);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = n;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_upb_a_b(void) {
    i64 n;
    struct pc_decls_objrec *  p;
    p = (*pc_decls_sptr).objptr;
    n = (i64)((u32)((((i64)((u64)((*p).uarray.length)) + (i64)((*p).uarray.lower)) - (i64)1)));
    pc_pcfns_pc_unshare(pc_decls_sptr);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = n;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_upb_s_d(void) {
    i64 n;
    n = (i64)((*(*pc_decls_sptr).objptr).ustr.length);
    pc_pcfns_pc_unshare(pc_decls_sptr);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = n;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_upb_m_k(void) {
    i64 n;
    n = (i64)(pc_decls_ttlength[((i64)((*pc_decls_sptr).tag))]);
    pc_pcfns_pc_unshare(pc_decls_sptr);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = n;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_upb_e(void) {
    i64 n;
    n = ((i64)((u64)((*(*pc_decls_sptr).objptr).uset.length)) - (i64)1);
    pc_pcfns_pc_unshare(pc_decls_sptr);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = n;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_upb_n(void) {
    (*pc_decls_sptr).value = (i64)((*pc_decls_sptr).range_upper);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_upb_z(void) {
    if (((i64)((*pc_decls_sptr).tag)==(i64)10)) {
        (*pc_decls_sptr).value = ((i64)((u64)((*(*pc_decls_sptr).objptr).udict.allocated)) / (i64)2);
    }else if (((i64)((*pc_decls_sptr).tag)==(i64)1)) {
        (*pc_decls_sptr).value = (i64)63;
    } else {
        pc_support_pcustype((byte*)"upb_def",pc_decls_sptr);
    };
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_bounds_l_a_b_s_e(void) {
    i64 a;
    i64 b;
    struct pc_decls_objrec *  r;
    r = (*pc_decls_sptr).objptr;
    if (((i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))])==(i64)5) || ((i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))])==(i64)32) || ((i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))])==(i64)33)) {
        a = (i64)1;
    }else if (((i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))])==(i64)9)) {
        a = (i64)0;
    }else if (((i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))])==(i64)30) || ((i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))])==(i64)31)) {
        a = (i64)((*r).uarray.lower);
    } else {
        a = (i64)((*r).ulist.lower);
    };
    b = (((i64)((u64)((*r).ulist.length)) + a) - (i64)1);
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        pc_pcfns_pc_unshare(pc_decls_sptr);
    };
    (*pc_decls_sptr).tagx = (u64)((i64)4);
    (*pc_decls_sptr).range_lower = a;
    (*pc_decls_sptr).range_upper = b;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_bounds_m_k(void) {
    i64 b;
    b = (i64)(pc_decls_ttlength[((i64)((*pc_decls_sptr).tag))]);
    pc_pcfns_pc_unshare(pc_decls_sptr);
    (*pc_decls_sptr).tagx = (u64)((i64)4);
    (*pc_decls_sptr).range_lower = (i64)1;
    (*pc_decls_sptr).range_upper = b;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_bounds_n(void) {
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_bounds_z(void) {
    if (((i64)((*pc_decls_sptr).tag)==(i64)1)) {
        (*pc_decls_sptr).tagx = (u64)((i64)4);
        (*pc_decls_sptr).range_lower = (i64)0;
        (*pc_decls_sptr).range_upper = (i64)63;
    } else {
        pc_support_pcustype((byte*)"bounds_def",pc_decls_sptr);
    };
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_minto_i(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).value=((*x).value<(*y).value?(*x).value:(*y).value);
;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_minto_r(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).xvalue=((*x).xvalue<(*y).xvalue?(*x).xvalue:(*y).xvalue);
;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_minto_z(void) {
    pc_support_pcmxtypes((byte*)"minto_def",(*(pc_decls_sptr + (i64)1)).varptr,pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_maxto_i(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).value=((*x).value>(*y).value?(*x).value:(*y).value);
;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_maxto_r(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    x = (*pc_decls_sptr).varptr;
    ++pc_decls_sptr;
    (*x).xvalue=((*x).xvalue>(*y).xvalue?(*x).xvalue:(*y).xvalue);
;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_maxto_z(void) {
    pc_support_pcmxtypes((byte*)"maxto_def",(*(pc_decls_sptr + (i64)1)).varptr,pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_neg_i_w(void) {
    (*pc_decls_sptr).value = -((*pc_decls_sptr).value);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_neg_r(void) {
    (*pc_decls_sptr).xvalue = -((*pc_decls_sptr).xvalue);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_neg_j(void) {
    pc_pcfns_pc_duplvar(pc_decls_sptr);
    pc_bignum_bx_negto(pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_neg_e(void) {
    pc_dxfns_dx_inotset(pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_neg_z(void) {
    pc_support_pcustype((byte*)"neg_def",pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_abs_i_w(void) {
    (*pc_decls_sptr).value = labs((*pc_decls_sptr).value);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_abs_r(void) {
    (*pc_decls_sptr).xvalue = fabs((*pc_decls_sptr).xvalue);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_abs_j(void) {
    pc_pcfns_pc_duplvar(pc_decls_sptr);
    pc_bignum_bx_absto(pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_abs_z(void) {
    pc_support_pcustype((byte*)"abs_def",pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_inot_i_w(void) {
    (*pc_decls_sptr).value = ~((*pc_decls_sptr).value);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_inot_e(void) {
    pc_dxfns_dx_inotset(pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_inot_z(void) {
    pc_support_pcustype((byte*)"inot_def",pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_istrue_i_w_r(void) {
    (*pc_decls_sptr).tag = (u64)((i64)1);
    (*pc_decls_sptr).value = !!((*pc_decls_sptr).value);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_istrue_l_a_e_s_b(void) {
    i64 res;
    res = ((i64)((u64)((*(*pc_decls_sptr).objptr).ulist.length)) != (i64)0);
    pc_pcfns_pc_unshare(pc_decls_sptr);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = res;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_istrue_k_m_h(void) {
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        pc_pcfns_pc_unshare(pc_decls_sptr);
    };
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = (i64)1;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_istrue_j(void) {
    pc_support_junimpl((byte*)"istrue_longint");
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_istrue_z(void) {
    pc_support_pcustype((byte*)"istrue_def",pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_jumpfalse_i_w_r_v_p_f(void) {
    if (!(!!((*pc_decls_sptr).value))) {
        ++pc_decls_sptr;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    ++pc_decls_sptr;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpfalse_s_l_e_a_b(void) {
    i64 n;
    n = (i64)((*(*pc_decls_sptr).objptr).ulist.length);
    pc_pcfns_pc_unshare(pc_decls_sptr);
    ++pc_decls_sptr;
    if ((n == (i64)0)) {
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumpfalse_z(void) {
    switch ((i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))])) {
    case 32:;
    case 33:;
    case 25:;
    {
        ++pc_decls_sptr;
        return (pc_decls_pcptr + (i64)2);
    }break;
    default: {
    }
    } //SW
;
    pc_support_pcustype((byte*)"jumpfalse_def",pc_decls_sptr);
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumptrue_i_r_w_v_p_f(void) {
    if (!!((*pc_decls_sptr).value)) {
        ++pc_decls_sptr;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    ++pc_decls_sptr;
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumptrue_s_l_e_a_b(void) {
    i64 n;
    n = (i64)((*(*pc_decls_sptr).objptr).ulist.length);
    pc_pcfns_pc_unshare(pc_decls_sptr);
    ++pc_decls_sptr;
    if (!!(n)) {
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    };
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_jumptrue_z(void) {
    switch ((i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))])) {
    case 32:;
    case 33:;
    case 25:;
    {
        ++pc_decls_sptr;
        return (u64 *)((*(pc_decls_pcptr + (i64)1)));
    }break;
    default: {
    }
    } //SW
;
    pc_support_pcustype((byte*)"jumptrue_def",pc_decls_sptr);
    return (pc_decls_pcptr + (i64)2);
}

u64 * pc_jhandlers_j_shl_i_w(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).value = ((*pc_decls_sptr).value << (*y).value);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_shl_z(void) {
    pc_support_pcmxtypes((byte*)"shl_def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_shr_i_w(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).value = ((*pc_decls_sptr).value >> (*y).value);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_shr_j_i(void) {
    pc_support_pcerror((byte*)"SHR/J/I");
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_shr_z(void) {
    pc_support_pcmxtypes((byte*)"shr_def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_shr_wi(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).uvalue = ((*pc_decls_sptr).uvalue >> (*y).value);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_shr_zz(void) {
    return pc_support_pcmxtypes((byte*)"shr_zz",(pc_decls_sptr + (i64)1),pc_decls_sptr);
}

u64 * pc_jhandlers_j_idiv_w(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).uvalue = ((*pc_decls_sptr).uvalue / (*y).uvalue);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_idiv_j(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    struct pc_decls_varrec result;
    y = pc_decls_sptr++;
    x = pc_decls_sptr;
    pc_bignum_bx_idiv(x,y,&result);
    if (!!((u64)((*x).hasref))) {
        pc_pcfns_pc_unshare(x);
    };
    if (!!((u64)((*y).hasref))) {
        pc_pcfns_pc_unshare(y);
    };
    (*pc_decls_sptr) = result;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_idiv_z(void) {
    pc_support_pcmxtypes((byte*)"idiv_def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_rem_w(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    (*pc_decls_sptr).uvalue = ((*pc_decls_sptr).uvalue % (*y).uvalue);
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_rem_j(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    struct pc_decls_varrec result;
    y = pc_decls_sptr++;
    x = pc_decls_sptr;
    pc_bignum_bx_irem(x,y,&result);
    if (!!((u64)((*x).hasref))) {
        pc_pcfns_pc_unshare(x);
    };
    if (!!((u64)((*y).hasref))) {
        pc_pcfns_pc_unshare(y);
    };
    (*pc_decls_sptr) = result;
    return (pc_decls_pcptr + (i64)1);
}

u64 * pc_jhandlers_j_remz(void) {
    pc_support_pcmxtypes((byte*)"rem_def",(pc_decls_sptr + (i64)1),pc_decls_sptr);
    return (pc_decls_pcptr + (i64)1);
}

i64 pc_oslayer_runproc_m(void * amsg) {
    struct pc_decls_varrec a;
    struct pc_decls_varrec dest;
    static i64 rmsg_typeno;
    i64 i;
    i64 result;
    struct pc_decls_objrec obj;
    if ((rmsg_typeno == (i64)0)) {
        L652 :;
        for (i=(i64)1;i<=pc_decls_ntypes;i+=(i64)1) {
L653 :;
            if (!!(mlib_eqstring(pc_decls_ttname[(i)],(((i64)64 == (i64)32)?(byte*)"ws_msg32":(byte*)"ws_msg64")))) {
                rmsg_typeno = i;
                goto L655 ;
            };
L654 :;
        }L655 :;
        ;
    };
    if ((rmsg_typeno == (i64)0)) {
        mlib_abortprogram((byte*)"mainwndproc: can't find rmsg");
    };
    memset((void *)(&obj),(i64)0,(u64)((i64)32));
    obj.refcount = (u64)((i64)99);
    obj.ustruct.ptr = (byte *)(amsg);
    a.tagx = (u64)((rmsg_typeno | (i64)65536));
    a.objptr = &obj;
    pci_runproc((void *)(pc_decls_pcl_callbackfn),&a,(struct pc_decls_varrec *)(0),&dest);
    result = dest.value;
    result = (i64)0;
    return result;
}

void pc_oslayer_os_getconsize(struct pc_decls_varrec * result) {
    pc_support_pcerror((byte*)"GETCONSIZE");
}

void pc_oslayer_pch_setmesshandler(struct pc_decls_varrec * fn) {
    if (((i64)((u64)((*fn).tag)) != (i64)18)) {
        pc_support_pcerror((byte*)"Not refproc");
    };
    pc_decls_pcl_callbackfn = (void (*)(void))((*fn).refptr);
    oswindows_os_setmesshandler((void *)(&pc_oslayer_runproc_m));
}

void pc_oslayer_pch_gethostname(struct pc_decls_varrec * result) {
    static byte name[256];
    strcpy((i8 *)(name),(i8 *)(oswindows_os_gethostname()));
    pc_pcfns_pc_makestring(name,(i64)-1,result);
}

void pc_oslayer_os_initdllmodules(void) {
    i64 i;
    i64 hinst;
    byte *  dllname;
    L656 :;
    for (i=(i64)1;i<=pc_decls_ndlltable;i+=(i64)1) {
L657 :;
        dllname = pc_decls_dlltable[(i)-1];
        hinst = pc_oslayer_os_loaddllmodule(dllname);
        if ((hinst == (i64)0)) {
        };
        pc_decls_dllinsttable[(i)-1] = (u64)(hinst);
L658 :;
    }L659 :;
    ;
}

i64 pc_oslayer_os_loaddllmodule(byte * dllname) {
    i64 hinst;
    if (!!(mlib_eqstring(dllname,(byte*)"jpeglib"))) {
        dllname = (byte*)"jpeglib64";
    } else if (!!(mlib_eqstring(dllname,(byte*)"jpeglibc"))) {
        dllname = (byte*)"jpeglibc64";
    };
    hinst = (i64)(oswindows_os_getdllinst(dllname));
    return hinst;
}

void pc_oslayer_os_initdllfunctions(void) {
    void (*fnaddr)(void);
    i64 hinst;
    i64 i;
    i64 dllmodno;
    L660 :;
    for (i=(i64)1;i<=pc_decls_ndllproctable;i+=(i64)1) {
L661 :;
        dllmodno = (i64)(pc_decls_dllproctable[(i)-1].dllindex);
        hinst = (i64)(pc_decls_dllinsttable[(dllmodno)-1]);
        fnaddr = (void (*)(void))(oswindows_os_getdllprocaddr(hinst,pc_decls_dllproctable[(i)-1].name));
        if ((fnaddr == 0)) {
            if ((hinst == (i64)0)) {
                goto L662 ;
            };
            msysnewc_m_print_startcon();
            msysnewc_m_print_str((byte*)"dllfns: fnaddr=0:",NULL);
            msysnewc_m_print_str(pc_decls_dllproctable[(i)-1].name,NULL);
            msysnewc_m_print_str((byte*)"from",NULL);
            msysnewc_m_print_str(pc_decls_dlltable[(dllmodno)-1],NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
        };
        pc_decls_dllproctable[(i)-1].address = fnaddr;
L662 :;
    }L663 :;
    ;
}

void * pc_oslayer_os_loaddllfunction(i64 fnindex) {
    i64 dllmodno;
    i64 hinst;
    void (*fnaddr)(void);
    dllmodno = (i64)(pc_decls_dllproctable[(fnindex)-1].dllindex);
    hinst = (i64)(pc_decls_dllinsttable[(dllmodno)-1]);
    if (!(!!(hinst))) {
        hinst = pc_oslayer_os_loaddllmodule(pc_decls_dlltable[(dllmodno)-1]);
        if (!(!!(hinst))) {
        } else {
            pc_decls_dllinsttable[(dllmodno)-1] = (u64)(hinst);
        };
    };
    fnaddr = (void (*)(void))(oswindows_os_getdllprocaddr(hinst,pc_decls_dllproctable[(fnindex)-1].name));
    if (!(!!(fnaddr))) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str(pc_decls_dllproctable[(fnindex)-1].name,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcerror((byte*)"Can't find DLL function");
        exit((i64)1);
    } else {
        pc_decls_dllproctable[(fnindex)-1].address = fnaddr;
    };
    return (void *)(fnaddr);
}

void pc_oslayer_pch_getos(struct pc_decls_varrec * result) {
    pc_pcfns_pc_makestring(oswindows_os_getos(),(i64)-1,result);
}

void pc_oslayer_pch_gethostsize(struct pc_decls_varrec * result) {
    (*result).tagx = (u64)((i64)1);
    (*result).value = oswindows_os_gethostsize();
}

void pc_oslayer_pch_iswindows(struct pc_decls_varrec * result) {
    (*result).tagx = (u64)((i64)1);
    (*result).value = oswindows_os_iswindows();
}

void pc_oslayer_os_calldll(i64 calltype,i64 fnindex,i64 offset,i64 nparams,i64 restype,struct pc_decls_varrec * dest) {
    i64 baserestype;
    void (*fnaddr)(void);
    i64 retval;
    i64 retcode;
    i64 (*iparams)[];
    i16 (*iparamtypes)[];
    byte paramcodes[100];
    i64 i;
    fnaddr = pc_decls_dllproctable[(fnindex)-1].address;
    if (!(!!(fnaddr))) {
        fnaddr = (void (*)(void))(pc_oslayer_os_loaddllfunction(fnindex));
        if (!!(fnaddr)) {
            pc_decls_dllproctable[(fnindex)-1].address = fnaddr;
        } else {
            msysnewc_m_print_startcon();
            msysnewc_m_print_str(pc_decls_dllproctable[(fnindex)-1].name,NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            pc_support_pcerror((byte*)"Calldll nil fn:");
        };
    };
    iparams = (i64 (*)[])(&pc_decls_dllparams[((offset + (i64)1))-1]);
    iparamtypes = (i16 (*)[])(&pc_decls_dlltypes[((offset + (i64)1))-1]);
    L664 :;
    for (i=(i64)1;i<=nparams;i+=(i64)1) {
L665 :;
        if (((i64)(pc_decls_ttbasetype[((i64)((*iparamtypes)[(i)-1]))]) == (i64)3)) {
            paramcodes[(i)-1] = (u64)82u;
        } else {
            paramcodes[(i)-1] = (u64)73u;
        };
L666 :;
    }L667 :;
    ;
    baserestype = (i64)(pc_decls_ttbasetype[(restype)]);
    retcode = ((baserestype == (i64)49)?(i64)82:(i64)73);
    retval = (i64)(oswindllc_os_calldllfunction(fnaddr,retcode,nparams,iparams,&paramcodes));
    switch (baserestype) {
    case 0:;
    {
    }break;
    case 3:;
    case 49:;
    {
        (*dest).tagx = (u64)((i64)3);
        (*dest).xvalue = *(double*)&retval;
    }break;
    case 48:;
    {
        pc_support_pcerror((byte*)"dll/r32ret");
    }break;
    case 40:;
    case 47:;
    case 50:;
    case 51:;
    case 52:;
    {
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = retval;
    }break;
    case 39:;
    {
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = (i64)((i32)(retval));
    }break;
    case 46:;
    {
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = (i64)((u32)(retval));
    }break;
    case 38:;
    {
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = (i64)((i16)(retval));
    }break;
    case 45:;
    {
        (*dest).tagx = (u64)((i64)1);
        (*dest).value = (i64)((u16)(retval));
    }break;
    case 23:;
    {
        (*dest).tagx = (u64)((i64)23);
        (*dest).uref.ptr = (byte *)(retval);
        (*dest).uref.elemtag = (u64)(pc_decls_tttarget[(restype)]);
    }break;
    default: {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str(pc_decls_ttname[(baserestype)],NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcerror((byte*)"Rettype not supported");
    }
    } //SW
;
}

u64 oswindllc_os_calldllfunction(void (*fnaddr)(void),i64 retcode,i64 nargs,i64 (*args)[],byte (*argcodes)[]) {
    if ((retcode == (i64)73)) {
        return (u64)(oswindllc_calldll_cint(fnaddr,args,nargs));
    } else {
        return (u64)(oswindllc_calldll_creal(fnaddr,args,nargs));
    };
}

u64 oswindllc_os_pushargs(u64 (*args)[],i64 nargs,i64 nextra,void (*fnaddr)(void),i64 isfloat) {
    return oswindllc_os_calldllfunction(fnaddr,(!!(isfloat)?(i64)0:(i64)73),nargs,(i64 (*)[])(args),(byte (*)[])(0));
}

static i64 oswindllc_calldll_cint(void (*fnaddr)(void),i64 (*params)[],i64 nparams) {
    switch (nparams) {
    case 0:;
    {
        return ((*(i64 (*)(void))(fnaddr)))();
    }break;
    case 1:;
    {
        return ((*(i64 (*)(i64))(fnaddr)))((*params)[((i64)1)-1]);
    }break;
    case 2:;
    {
        return ((*(i64 (*)(i64,i64))(fnaddr)))((*params)[((i64)1)-1],(*params)[((i64)2)-1]);
    }break;
    case 3:;
    {
        return ((*(i64 (*)(i64,i64,i64))(fnaddr)))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1]);
    }break;
    case 4:;
    {
        return ((*(i64 (*)(i64,i64,i64,i64))(fnaddr)))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1]);
    }break;
    case 5:;
    {
        return ((*(i64 (*)(i64,i64,i64,i64,i64))(fnaddr)))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1]);
    }break;
    case 6:;
    {
        return ((*(i64 (*)(i64,i64,i64,i64,i64,i64))(fnaddr)))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1]);
    }break;
    case 9:;
    {
        return ((*(i64 (*)(i64,i64,i64,i64,i64,i64,i64,i64,i64))(fnaddr)))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1],(*params)[((i64)7)-1],(*params)[((i64)8)-1],(*params)[((i64)9)-1]);
    }break;
    case 10:;
    {
        return ((*(i64 (*)(i64,i64,i64,i64,i64,i64,i64,i64,i64,i64))(fnaddr)))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1],(*params)[((i64)7)-1],(*params)[((i64)8)-1],(*params)[((i64)9)-1],(*params)[((i64)10)-1]);
    }break;
    case 11:;
    {
        return ((*(i64 (*)(i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64))(fnaddr)))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1],(*params)[((i64)7)-1],(*params)[((i64)8)-1],(*params)[((i64)9)-1],(*params)[((i64)10)-1],(*params)[((i64)11)-1]);
    }break;
    case 12:;
    {
        return ((*(i64 (*)(i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64))(fnaddr)))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1],(*params)[((i64)7)-1],(*params)[((i64)8)-1],(*params)[((i64)9)-1],(*params)[((i64)10)-1],(*params)[((i64)11)-1],(*params)[((i64)12)-1]);
    }break;
    case 14:;
    {
        return ((*(i64 (*)(i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64,i64))(fnaddr)))((*params)[((i64)1)-1],(*params)[((i64)2)-1],(*params)[((i64)3)-1],(*params)[((i64)4)-1],(*params)[((i64)5)-1],(*params)[((i64)6)-1],(*params)[((i64)7)-1],(*params)[((i64)8)-1],(*params)[((i64)9)-1],(*params)[((i64)10)-1],(*params)[((i64)11)-1],(*params)[((i64)12)-1],(*params)[((i64)13)-1],(*params)[((i64)14)-1]);
    }break;
    default: {
        msysnewc_m_print_startcon();
        msysnewc_m_print_i64(nparams,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"calldll/c/int unsupported # of params",NULL);
        msysnewc_m_print_i64(nparams,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        exit((i64)1);
    }
    } //SW
;
    return (i64)0;
}

static i64 oswindllc_calldll_creal(void (*fnaddr)(void),i64 (*params)[],i64 nparams) {
    double x;
    switch (nparams) {
    case 0:;
    {
        return (i64)(((*(double (*)(void))(fnaddr)))());
    }break;
    case 1:;
    {
        oswindllc_os_dummycall((double)((*params)[((i64)1)-1]),(double)((*params)[((i64)2)-1]),(double)((*params)[((i64)3)-1]),(double)((*params)[((i64)4)-1]));
        x = ((*(double (*)(i64))(fnaddr)))((*params)[((i64)1)-1]);
    }break;
    case 2:;
    {
        x = ((*(double (*)(i64,i64))(fnaddr)))((*params)[((i64)1)-1],(*params)[((i64)2)-1]);
    }break;
    default: {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"calldll/c/real too many params",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        exit((i64)1);
    }
    } //SW
;
    return *(i64*)&x;
}

void oswindllc_os_dummycall(double a,double b,double c,double d) {
}

void pc_host_callhostfunction(i64 hostfn,i64 calledasfn) {
    void (*fnaddr)(void);
    i64 nparams;
    i64 isfn;
    i64 av_1;
    fnaddr = (void (*)(void))(pc_host_hosttable[(hostfn)-1]);
    nparams = pq_common_hostnparams[(hostfn)];
    isfn = pq_common_hostisfn[(hostfn)];
    if (!!(isfn)) {
        switch (nparams) {
        case 0:;
        {
            ((*(void (*)(struct pc_decls_varrec *))(fnaddr)))(pc_decls_sptr);
        }break;
        case 1:;
        {
            ((*(void (*)(struct pc_decls_varrec *,struct pc_decls_varrec *))(fnaddr)))(pc_decls_sptr,(pc_decls_sptr + (i64)1));
        }break;
        case 2:;
        {
            ((*(void (*)(struct pc_decls_varrec *,struct pc_decls_varrec *,struct pc_decls_varrec *))(fnaddr)))(pc_decls_sptr,(pc_decls_sptr + (i64)1),(pc_decls_sptr + (i64)2));
        }break;
        case 3:;
        {
            ((*(void (*)(struct pc_decls_varrec *,struct pc_decls_varrec *,struct pc_decls_varrec *,struct pc_decls_varrec *))(fnaddr)))(pc_decls_sptr,(pc_decls_sptr + (i64)1),(pc_decls_sptr + (i64)2),(pc_decls_sptr + (i64)3));
        }break;
        case 4:;
        {
            ((*(void (*)(struct pc_decls_varrec *,struct pc_decls_varrec *,struct pc_decls_varrec *,struct pc_decls_varrec *,struct pc_decls_varrec *))(fnaddr)))(pc_decls_sptr,(pc_decls_sptr + (i64)1),(pc_decls_sptr + (i64)2),(pc_decls_sptr + (i64)3),(pc_decls_sptr + (i64)4));
        }break;
        default: {
            pc_support_pcerror((byte*)"callhost/fn");
        }
        } //SW
;
    } else {
        switch (nparams) {
        case 0:;
        {
            ((*fnaddr))();
        }break;
        case 1:;
        {
            ((*(void (*)(struct pc_decls_varrec *))(fnaddr)))(pc_decls_sptr);
        }break;
        case 2:;
        {
            ((*(void (*)(struct pc_decls_varrec *,struct pc_decls_varrec *))(fnaddr)))(pc_decls_sptr,(pc_decls_sptr + (i64)1));
        }break;
        case 3:;
        {
            ((*(void (*)(struct pc_decls_varrec *,struct pc_decls_varrec *,struct pc_decls_varrec *))(fnaddr)))(pc_decls_sptr,(pc_decls_sptr + (i64)1),(pc_decls_sptr + (i64)2));
        }break;
        case 4:;
        {
            ((*(void (*)(struct pc_decls_varrec *,struct pc_decls_varrec *,struct pc_decls_varrec *,struct pc_decls_varrec *))(fnaddr)))(pc_decls_sptr,(pc_decls_sptr + (i64)1),(pc_decls_sptr + (i64)2),(pc_decls_sptr + (i64)3));
        }break;
        default: {
            pc_support_pcerror((byte*)"callhost/proc");
        }
        } //SW
;
    };
    av_1 = nparams;
    while (av_1-- > 0) {
L668 :;
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            pc_pcfns_pc_unshare(pc_decls_sptr);
        };
        ++pc_decls_sptr;
L669 :;
    }L670 :;
    ;
}

static void pc_host_pch_leftstr(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c,struct pc_decls_varrec * result) {
    i64 n;
    i64 length;
    i64 padchar;
    byte *  s;
    struct pc_decls_objrec *  pa;
    padchar = (i64)32;
    if (((i64)((*c).tag)==(i64)0)) {
    }else if (((i64)((*c).tag)==(i64)5)) {
        if (((i64)((*(*c).objptr).ustr.length) == (i64)1)) {
            padchar = (i64)((*(*(*c).objptr).ustr.strptr));
        } else {
            pc_support_pcerror((byte*)"left/padx");
        };
    }else if (((i64)((*c).tag)==(i64)1)) {
        padchar = (*c).value;
    } else {
        pc_support_pcerror((byte*)"left/pad?");
    };
    if (((i64)((*b).tag)==(i64)0)) {
        n = (i64)1;
    }else if (((i64)((*b).tag)==(i64)1)) {
        n = (*b).value;
    } else {
        pc_support_pcerror((byte*)"left:bad n");
    };
    if (((i64)((u64)((*a).tag)) != (i64)5)) {
        pc_support_pcerror((byte*)"left:not str");
    };
    pa = (*a).objptr;
    length = (i64)((*pa).ustr.length);
    s = (*pa).ustr.strptr;
    if ((n == (i64)0)) {
        pc_pcfns_pc_emptystring(result);
        return;
    };
    (*result).tagx = (u64)((i64)65541);
    if ((n > (i64)0)) {
        if ((n <= length)) {
            pc_host_leftstring(a,n,result);
        } else {
            pc_host_padstring_right(a,n,padchar,result);
        };
    } else {
        n = -(n);
        if ((n < length)) {
            pc_host_leftstring(a,(length - n),result);
        } else {
            pc_pcfns_pc_emptystring(result);
        };
    };
}

static void pc_host_pch_rightstr(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c,struct pc_decls_varrec * result) {
    i64 n;
    i64 length;
    i64 padchar;
    byte *  s;
    struct pc_decls_objrec *  pa;
    padchar = (i64)32;
    if (((i64)((*c).tag)==(i64)0)) {
    }else if (((i64)((*c).tag)==(i64)5)) {
        if (((i64)((*(*c).objptr).ustr.length) == (i64)1)) {
            padchar = (i64)((*(*(*c).objptr).ustr.strptr));
        } else {
            pc_support_pcerror((byte*)"right/padx");
        };
    }else if (((i64)((*c).tag)==(i64)1)) {
        padchar = (*c).value;
    } else {
        pc_support_pcerror((byte*)"right/pad?");
    };
    if (((i64)((*b).tag)==(i64)0)) {
        n = (i64)1;
    }else if (((i64)((*b).tag)==(i64)1)) {
        n = (*b).value;
    } else {
        pc_support_pcerror((byte*)"right:bad n");
    };
    pa = (*a).objptr;
    if (((i64)((u64)((*a).tag)) != (i64)5)) {
        pc_support_pcerror((byte*)"right:not str");
    };
    length = (i64)((*pa).ustr.length);
    s = (*pa).ustr.strptr;
    (*result).tagx = (u64)((i64)65541);
    if ((n == (i64)0)) {
        pc_pcfns_pc_emptystring(result);
        return;
    };
    if ((n > (i64)0)) {
        if ((n <= length)) {
            pc_host_rightstring(a,n,result);
        } else {
            pc_host_padstring_left(a,n,padchar,result);
        };
    } else {
        n = -(n);
        if ((n < length)) {
            pc_host_rightstring(a,(length - n),result);
        } else {
            pc_pcfns_pc_emptystring(result);
        };
    };
}

static void pc_host_pch_convlc(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result) {
    pc_host_checkparam(a,(i64)5,(i64)-999999);
    (*result) = (*a);
    ++(*(*result).objptr).refcount;
    if (!!((u64)((*result).hasref))) {
        pc_pcfns_pc_dupl(result);
    };
    pc_pcfns_pc_iconvcase(result,b,(i64)0);
}

static void pc_host_pch_convuc(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result) {
    pc_host_checkparam(a,(i64)5,(i64)-999999);
    (*result) = (*a);
    ++(*(*result).objptr).refcount;
    if (!!((u64)((*result).hasref))) {
        pc_pcfns_pc_dupl(result);
    };
    pc_pcfns_pc_iconvcase(result,b,(i64)1);
}

static void pc_host_pch_iconvlc(struct pc_decls_varrec * a,struct pc_decls_varrec * b) {
    pc_host_checkparam(a,(i64)22,(i64)-999999);
    pc_pcfns_pc_iconvcase((*a).varptr,b,(i64)0);
}

static void pc_host_pch_iconvuc(struct pc_decls_varrec * a,struct pc_decls_varrec * b) {
    pc_host_checkparam(a,(i64)22,(i64)-999999);
    pc_pcfns_pc_iconvcase((*a).varptr,b,(i64)1);
}

static void pc_host_pch_stop(void) {
    pc_support_pcerror((byte*)"host_stop not impl");
}

static void pc_host_pch_stopx(struct pc_decls_varrec * a) {
    pc_support_pcerror((byte*)"host_stopx not impl");
}

static void pc_host_pch_ismain(struct pc_decls_varrec * a,struct pc_decls_varrec * result) {
    i64 mainmod;
    i64 ismain;
    pc_host_checkparam(a,(i64)5,(i64)-999999);
    (*result).tagx = (u64)((i64)1);
    if (!!(mlib_eqstring(pc_decls_strpclversion,(byte*)"404"))) {
        mainmod = (i64)1;
    } else {
        mainmod = pc_decls_nmodules;
    };
    ismain = (pc_pcfns_cmpstring_len((*(*a).objptr).ustr.strptr,pc_decls_moduletable[(mainmod)].name,(i64)((*(*a).objptr).ustr.length),(i64)(strlen((i8 *)(pc_decls_moduletable[(mainmod)].name)))) == (i64)0);
    (*result).value = ismain;
}

static void pc_host_pch_waitkey(struct pc_decls_varrec * result) {
    (*result).tagx = (u64)((i64)1);
    (*result).value = oswindows_os_getch();
}

static void pc_host_pch_testkey(struct pc_decls_varrec * result) {
    (*result).tagx = (u64)((i64)1);
    (*result).value = oswindows_os_kbhit();
}

static void pc_host_pch_execwait(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c,struct pc_decls_varrec * result) {
    byte *  workdir;
    i64 flag;
    struct pc_decls_objrec *  pa;
    pc_host_checkparam(a,(i64)5,(i64)-999999);
    pa = (*a).objptr;
    flag = pc_host_checkparam(b,(i64)1,(i64)0);
    if (((i64)((u64)((*c).tag)) == (i64)0)) {
        workdir = (byte *)(0);
    } else {
        pc_host_checkparam(c,(i64)5,(i64)-999999);
        workdir = pc_support_convcstring((*(*c).objptr).ustr.strptr,(i64)((*(*c).objptr).ustr.length));
    };
    (*result).tagx = (u64)((i64)1);
    (*result).value = oswindows_os_execwait(pc_support_convcstring((*pa).ustr.strptr,(i64)((*pa).ustr.length)),flag,workdir);
}

static void pc_host_pch_execcmd(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c,struct pc_decls_varrec * result) {
    byte *  workdir;
    i64 flag;
    struct pc_decls_objrec *  pa;
    pc_host_checkparam(a,(i64)5,(i64)-999999);
    pa = (*a).objptr;
    flag = pc_host_checkparam(b,(i64)1,(i64)0);
    if (((i64)((u64)((*c).tag)) == (i64)0)) {
        workdir = (byte *)(0);
    } else {
        pc_host_checkparam(c,(i64)5,(i64)-999999);
        workdir = pc_support_convcstring((*(*c).objptr).ustr.strptr,(i64)((*(*c).objptr).ustr.length));
    };
    (*result).tagx = (u64)((i64)1);
    (*result).value = oswindows_os_execcmd(pc_support_convcstring((*pa).ustr.strptr,(i64)((*pa).ustr.length)),flag);
}

static void pc_host_pch_makestr(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result) {
    i64 n;
    struct pc_decls_objrec *  s;
    switch ((i64)((*a).tag)) {
    case 23:;
    {
    }break;
    case 1:;
    {
    }break;
    default: {
        pc_support_pcerror((byte*)"makestr");
    }
    } //SW
;
    n = pc_support_getintvalue(b);
    (*result).tagx = (u64)((i64)65541);
    s = pc_objlib_make_strslicexobj((byte *)((*a).uref.ptr),n);
    (*result).objptr = s;
}

static void pc_host_pch_makestrslice(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result) {
    pc_support_pcerror((byte*)"MAKESTRSLICE");
}

static void pc_host_pch_makeref(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result) {
    byte *  ptr;
    switch ((i64)(pc_decls_ttbasetype[((i64)((*a).tag))])) {
    case 22:;
    case 23:;
    case 1:;
    {
        ptr = (*a).uref.ptr;
    }break;
    case 5:;
    case 30:;
    case 29:;
    case 9:;
    {
        ptr = (*(*a).objptr).uarray.ptr;
    }break;
    default: {
        pc_support_pcerror((byte*)"makeref");
    }
    } //SW
;
    (*result).tagx = (u64)((i64)23);
    (*result).uref.ptr = ptr;
    (*result).uref.elemtag = (u64)(pc_support_getintvalue(b));
    if (((i64)((*result).uref.elemtag)==(i64)41) || ((i64)((*result).uref.elemtag)==(i64)42) || ((i64)((*result).uref.elemtag)==(i64)43)) {
        (*result).tag = (u64)((i64)24);
        (*result).uref.bitoffset = (u64)((i64)0);
        (*result).uref.bitlength = (u64)((i64)0);
    };
}

static void pc_host_pch_new(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c,struct pc_decls_varrec * d,struct pc_decls_varrec * result) {
    struct pc_decls_varrec v;
    i64 t;
    i64 offset;
    i64 elemtype;
    i64 n;
    struct pc_host_dimrec dims;
    struct pc_decls_varrec *  qvar;
    byte *  qbyte;
    struct pc_decls_objrec *  p;
    i64 av_1;
    i64 av_2;
    i64 av_3;
    i64 av_4;
    t = pc_support_getintvalue(a);
    if (((t < (i64)0) || (t > pc_decls_ntypes))) {
        pc_support_pcustypet((byte*)"New:bad type",t);
    };
    v.tagx = (u64)((t | (i64)65536));
    switch ((i64)(pc_decls_ttbasetype[(t)])) {
    case 29:;
    {
        pc_host_getbounds(b,&dims,(i64)1);
        p = pc_objlib_list_new(dims.length,dims.lbound,(struct pc_decls_varrec *)(0));
        v.objptr = p;
        if (!!(dims.length)) {
            if ((!!(c) && ((i64)((u64)((*c).tag)) != (i64)0))) {
                qvar = (*p).ulist.vptr;
                av_1 = dims.length;
                while (av_1-- > 0) {
L671 :;
                    (*qvar) = (*c);
                    if (!!((u64)((*qvar).hasref))) {
                        pc_pcfns_pc_share(qvar);
                    };
                    ++qvar;
L672 :;
                }L673 :;
                ;
            };
        };
    }break;
    case 30:;
    {
        if (!!((u64)(pc_decls_ttlength[(t)]))) {
            elemtype = (i64)(pc_decls_tttarget[(t)]);
            dims.length = (i64)(pc_decls_ttlength[(t)]);
            dims.lbound = (i64)(pc_decls_ttlower[(t)]);
            dims.upper = ((dims.length + dims.lbound) - (i64)1);
            d = b;
            goto L674 ;
;
        };
        elemtype = pc_support_getintvalue(b);
        pc_host_getbounds(c,&dims,(i64)1);
        if (((elemtype >= (i64)41) && (elemtype <= (i64)43))) {
            v.tag = (u64)((t = (i64)31));
            goto L675 ;
;
        };
        //doarray2:
L674 :;
;
        p = pc_objlib_array_new(t,elemtype,dims.length,dims.lbound);
        v.objptr = p;
        if (!!(dims.length)) {
            if ((!!(d) && ((i64)((u64)((*d).tag)) != (i64)0))) {
                qbyte = (*p).uarray.ptr;
                av_2 = dims.length;
                while (av_2-- > 0) {
L676 :;
                    pc_pcfns_pc_storepacked(qbyte,d,elemtype);
                    qbyte += pc_decls_ttsize[(elemtype)];
L677 :;
                }L678 :;
                ;
            };
        };
    }break;
    case 31:;
    {
        if (!!((u64)(pc_decls_ttlength[(t)]))) {
            elemtype = (i64)(pc_decls_tttarget[(t)]);
            dims.length = (i64)(pc_decls_ttlength[(t)]);
            dims.lbound = (i64)(pc_decls_ttlower[(t)]);
            dims.upper = ((dims.length + dims.lbound) - (i64)1);
            d = b;
            goto L675 ;
;
        };
        elemtype = pc_support_getintvalue(b);
        if (((elemtype < (i64)41) || (elemtype > (i64)43))) {
            pc_support_pcerror((byte*)"new: bad bits elem");
        };
        pc_host_getbounds(c,&dims,(i64)1);
        //dobits2:
L675 :;
;
        p = pc_objlib_bits_new(elemtype,dims.length,dims.lbound);
        v.objptr = p;
        if (!!(dims.length)) {
            if ((!!(d) && ((i64)((u64)((*d).tag)) != (i64)0))) {
                qbyte = (*p).ubits.ptr;
                offset = (i64)0;
                av_3 = dims.length;
                while (av_3-- > 0) {
L679 :;
                    pc_pcfns_pc_storebit(qbyte,offset,d,elemtype,(i64)0);
                    offset += (i64)(pc_decls_ttbitwidth[(elemtype)]);
                    if ((offset >= (i64)8)) {
                        offset = (i64)0;
                        ++qbyte;
                    };
L680 :;
                }L681 :;
                ;
            };
        };
    }break;
    case 9:;
    {
        pc_host_getbounds(b,&dims,(i64)0);
        if ((dims.lbound < (i64)0)) {
            pc_support_pcerror((byte*)"new:set:lwb");
        };
        if ((dims.lbound != (i64)0)) {
            dims.lbound = (i64)0;
            dims.length = (dims.upper + (i64)1);
        };
        p = pc_objlib_set_new(dims.length,(i64)0);
        v.objptr = p;
    }break;
    case 32:;
    {
        p = pc_objlib_record_new(t);
        pc_objlib_objtovar(p,&v);
        if ((!!(b) && ((i64)((u64)((*b).tag)) != (i64)0))) {
            qvar = (*p).urec.vptr;
            av_4 = (i64)(pc_decls_ttlength[(t)]);
            while (av_4-- > 0) {
L682 :;
                (*qvar) = (*b);
                if (!!((u64)((*qvar).hasref))) {
                    pc_pcfns_pc_share(qvar);
                };
                ++qvar;
L683 :;
            }L684 :;
            ;
        };
    }break;
    case 33:;
    {
        p = pc_objlib_struct_new(t);
        pc_objlib_objtovar(p,&v);
        if ((!!(b) && ((i64)((u64)((*b).tag)) != (i64)0))) {
            pc_support_pcerror((byte*)"New: struct init");
        };
    }break;
    case 1:;
    case 2:;
    case 3:;
    case 18:;
    case 22:;
    {
        v.value = (i64)0;
        v.hasref = (u64)((i64)0);
        if ((!!(b) && ((i64)((u64)((*b).tag)) != (i64)0))) {
            pc_support_pcerror((byte*)"NEW(int/value)");
        };
    }break;
    case 5:;
    {
        pc_host_getbounds(b,&dims,(i64)0);
        pc_pcfns_pc_makestringn(dims.length,&v);
    }break;
    case 10:;
    {
        pc_host_getbounds(b,&dims,(i64)1);
        n = pc_support_nextpoweroftwo(dims.length);
        p = pc_objlib_dict_new(n);
        (*p).udict.dictitems = (u64)((i64)0);
        v.objptr = p;
    }break;
    default: {
        pc_support_pcustypet((byte*)"new",t);
    }
    } //SW
;
    //finish:
L685 :;
;
    (*result) = v;
}

static void pc_host_pch_newheap(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c,struct pc_decls_varrec * d,struct pc_decls_varrec * result) {
    struct pc_decls_varrec *  p;
    p = (struct pc_decls_varrec *)(mlib_pcm_alloc((i64)16));
    pc_host_pch_new(a,b,c,d,p);
    (*result).tagx = (u64)((i64)22);
    (*result).varptr = p;
}

static void pc_host_pch_heapvar(struct pc_decls_varrec * a,struct pc_decls_varrec * result) {
    (*result).tagx = (u64)((i64)1);
    (*result).value = (i64)12345678;
}

static void pc_host_pch_freeheap(struct pc_decls_varrec * a) {
    pc_support_pcerror((byte*)"FREEHEAP");
}

static void pc_host_pch_getcmdparam(struct pc_decls_varrec * a,struct pc_decls_varrec * result) {
    i64 n;
    if (((i64)((u64)((*a).tag)) == (i64)0)) {
        (*result).tagx = (u64)((i64)1);
        (*result).value = (pc_decls_ncmdparams + (i64)1);
        return;
    };
    n = pc_support_getintvalue(a);
    pc_pcfns_pc_makestring(pc_decls_cmdparamtable[(n)],(i64)-1,result);
}

static void pc_host_pch_setpcerror(struct pc_decls_varrec * a) {
    struct pc_decls_objrec *  pa;
    pc_host_checkparam(a,(i64)5,(i64)-999999);
    pa = (*a).objptr;
    if (!!(pc_decls_pcerror_mess)) {
        free((void *)(pc_decls_pcerror_mess));
        pc_decls_pcerror_mess = (byte *)(0);
    };
    if (!!((i64)((*pa).ustr.length))) {
        pc_decls_pcerror_mess = (byte *)(malloc((u64)(((i64)((*pa).ustr.length) + (i64)1))));
        memcpy((void *)(pc_decls_pcerror_mess),(void *)((*pa).ustr.strptr),(u64)((*pa).ustr.length));
        (*(pc_decls_pcerror_mess + (i64)((*pa).ustr.length))) = (u64)0u;
    };
}

static void pc_host_pch_setdebug(struct pc_decls_varrec * a) {
    pc_host_checkparam(a,(i64)1,(i64)-999999);
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"SETDEBUG.................",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    mlib_fdebug = (*a).value;
}

static void pc_host_pch_setfprintf(struct pc_decls_varrec * a,struct pc_decls_varrec * b) {
    pc_host_checkparam(a,(i64)19,(i64)-999999);
    pc_host_checkparam(b,(i64)19,(i64)-999999);
    pc_decls_fprintf_ptr = (void (*)(void))((*a).refptr);
    pc_decls_fgets_ptr = (void (*)(void))((*b).refptr);
}

static void pc_host_pch_ticks(struct pc_decls_varrec * result) {
    (*result).tagx = (u64)((i64)1);
    (*result).value = oswindows_os_clock();
}

static void pc_host_pch_sleep(struct pc_decls_varrec * a) {
    pc_host_checkparam(a,(i64)1,(i64)-999999);
    oswindows_os_sleep((*a).value);
}

static void pc_host_pch_random(struct pc_decls_varrec * a,struct pc_decls_varrec * result) {
    i64 n;
    i64 x;
    (*result).tagx = (u64)((i64)1);
    if (((i64)((u64)((*a).tag)) == (i64)4)) {
        x = mlib_mrandomrange((i64)((*a).range_lower),(i64)((*a).range_upper));
    } else {
        pc_host_checkparam(a,(i64)1,(i64)-999999);
        n = (*a).value;
        if ((n > (i64)1)) {
            x = mlib_mrandomint(n);
        } else if ((n == (i64)0)) {
            x = (i64)(mlib_mrandom());
        } else if ((n == (i64)1)) {
            (*result).tagx = (u64)((i64)3);
            (*result).xvalue = mlib_mrandomreal();
            return;
        } else {
        };
    };
    (*result).value = x;
}

static void pc_host_pch_findmetafunction(struct pc_decls_varrec * a,struct pc_decls_varrec * result) {
    byte *  sdata;
    struct pc_decls_objrec *  pa;
    struct pc_decls_strec *  d;
    i64 av_1;
    pc_host_checkparam(a,(i64)5,(i64)-999999);
    pa = (*a).objptr;
    (*result).tagx = (u64)((i64)18);
    (*result).value = (i64)0;
    if (!!((i64)((*pa).ustr.length))) {
        sdata = pc_support_convcstring((*pa).ustr.strptr,(i64)((*pa).ustr.length));
    } else {
        return;
    };
    d = &(*pc_decls_pcsymboltable)[((i64)1)-1];
    av_1 = pc_decls_nsymbols;
    while (av_1-- > 0) {
L686 :;
        if (((i64)((u64)((*d).nameid)) == (i64)5)) {
            if ((!!((*d).metadata) && (strstr((i8 *)((*d).metadata),(i8 *)(sdata)) != 0))) {
                (*result).refptr = (byte *)((*d).address);
                return;
            };
        };
        ++d;
L687 :;
    }L688 :;
    ;
}

static void pc_host_pch_loadpcl(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result) {
    pc_support_pcerror((byte*)"host_loadpcl not impl");
}

static void pc_host_pch_runpcl(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result) {
    pc_support_pcerror((byte*)"host_runpcl not impl");
}

static void pc_host_pch_runtask(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result) {
    pc_support_pcerror((byte*)"host_runtask not impl");
}

static void pc_host_pch_callext(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c) {
    pc_support_pcerror((byte*)"host_callext not impl");
}

static void pc_host_pch_system(struct pc_decls_varrec * a,struct pc_decls_varrec * result) {
    pc_host_checkparam(a,(i64)5,(i64)-999999);
    (*result).tagx = (u64)((i64)1);
    (*result).value = (i64)(system((i8 *)(pc_support_convcstring((*(*a).objptr).ustr.strptr,(i64)((*(*a).objptr).ustr.length)))));
}

static void pc_host_pch_shellexec(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result) {
    struct pc_decls_objrec *  pa;
    struct pc_decls_objrec *  pb;
    pc_host_checkparam(a,(i64)5,(i64)-999999);
    pc_host_checkparam(b,(i64)5,(i64)-999999);
    pa = (*a).objptr;
    pb = (*b).objptr;
    (*result).tagx = (u64)((i64)1);
    (*result).value = oswindows_os_shellexec(pc_support_convcstring((*pa).ustr.strptr,(i64)((*pa).ustr.length)),pc_support_convcstring((*pb).ustr.strptr,(i64)((*pb).ustr.length)));
}

static void pc_host_pch_gethash(struct pc_decls_varrec * a,struct pc_decls_varrec * result) {
    (*result).tagx = (u64)((i64)1);
    (*result).value = pc_pcfns_gethashvalue(a);
}

static void pc_host_pch_test(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result) {
    struct pc_decls_objrec *  p;
    static i64 lastalloc = (i64)0;
    p = (*a).objptr;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"TEST>>>",NULL);
    msysnewc_m_print_str((byte*)"P.UARRAY.LENGTH=",NULL);
    msysnewc_m_print_u64((*p).uarray.length,NULL);
    msysnewc_m_print_u64((*p).uarray.allocated,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    (*result).tagx = (u64)((i64)1);
}

static void pc_host_pch_pcldata(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * result) {
    i64 res;
    i64 length;
    i64 i;
    struct pc_decls_strec *  d;
    i64 av_1;
    pc_host_checkparam(a,(i64)1,(i64)-999999);
    res = (i64)0;
    if (((*a).value==(i64)6073471649054675536)) {
        if ((pc_decls_proclist == 0)) {
            d = &(*pc_decls_pcsymboltable)[((i64)1)-1];
            av_1 = pc_decls_nsymbols;
            while (av_1-- > 0) {
L689 :;
                if (((i64)((u64)((*d).nameid)) == (i64)5)) {
                    pc_host_addtoproclist(d);
                };
                ++d;
L690 :;
            }L691 :;
            ;
        };
        pc_host_proclistptr = pc_decls_proclist;
    }else if (((*a).value==(i64)1129271888)) {
        if (!!(pc_host_proclistptr)) {
            pc_host_getproctabledata(pc_host_proclistptr,result);
            pc_host_proclistptr = (*pc_host_proclistptr).nextproc;
            return;
        };
    }else if (((*a).value==(i64)76194150371149)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"MODULE",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    }else if (((*a).value==(i64)19506716274347331)) {
        pc_pcfns_pc_makestring(pq_common_cmdnames[((*b).value)],(i64)-1,result);
        return;
    }else if (((*a).value==(i64)19496880614690115)) {
        L692 :;
        for (i=(i64)1;i<=(i64)217;i+=(i64)1) {
L693 :;
            length = (i64)(strlen((i8 *)(pq_common_cmdnames[(i)])));
            if ((((i64)((*(*b).objptr).ustr.length) == length) && ((i64)(memcmp((void *)((*(*b).objptr).ustr.strptr),(void *)(pq_common_cmdnames[(i)]),(u64)(length))) == (i64)0))) {
                res = i;
                goto L695 ;
            };
L694 :;
        }L695 :;
        ;
    }else if (((*a).value==(i64)91552836767566)) {
        res = pc_decls_cmdnopnds[((*b).value)];
    } else {
        pc_support_pcerror((byte*)"pcldata/bad table");
    };
    (*result).tagx = (u64)((i64)1);
    (*result).value = res;
}

static void pc_host_pch_getcstring(struct pc_decls_varrec * a,struct pc_decls_varrec * result) {
    pc_support_pcerror((byte*)"PCH/GETCSTRING");
}

static void pc_host_pch_getparam(struct pc_decls_varrec * a,struct pc_decls_varrec * result) {
    pc_host_checkparam(a,(i64)1,(i64)-999999);
    (*result) = (*(struct pc_decls_varrec *)((pc_decls_frameptr + ((*a).value * (i64)16))));
    if (!!((u64)((*result).hasref))) {
        ++(*(*result).objptr).refcount;
    };
}

static void pc_host_pch_clearlist(struct pc_decls_varrec * a) {
    pc_support_pcerror((byte*)"PCH CLEARLIST");
}

static void pc_host_pch_makelink(struct pc_decls_varrec * a,struct pc_decls_varrec * result) {
    if (((i64)(pc_decls_ttbasetype[((i64)((*a).tag))])==(i64)32)) {
        (*result).tagx = (u64)((i64)25);
        (*result).uref.elemtag = (u64)((*a).tag);
        (*result).objptr = (*a).objptr;
    }else if (((i64)(pc_decls_ttbasetype[((i64)((*a).tag))])==(i64)1)) {
        if (!!((*a).value)) {
            pc_support_pcerror((byte*)"makelink/int");
        };
        (*result).tagx = (u64)((i64)1);
        (*result).value = (i64)0;
    }else if (((i64)(pc_decls_ttbasetype[((i64)((*a).tag))])==(i64)25)) {
    } else {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str(pc_decls_ttname[((i64)((*a).tag))],NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        msysnewc_m_print_startcon();
        msysnewc_m_print_str(pc_decls_ttname[((i64)(pc_decls_ttbasetype[((i64)((*a).tag))]))],NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcerror((byte*)"makelink: not record/list");
    };
}

static void pc_host_pch_allparams(struct pc_decls_varrec * a,struct pc_decls_varrec * result) {
    struct pc_decls_objrec *  p;
    i64 nparams;
    u64 *  fnptr;
    pc_host_checkparam(a,(i64)18,(i64)-999999);
    fnptr = (u64 *)((*a).refptr);
    nparams = (i64)((*(fnptr - (i64)1)));
    p = pc_objlib_obj_new((i64)29);
    (*p).objtype = (u64)((i64)2);
    (*p).ulist.length = (u64)(nparams);
    (*p).ulist.lower = (i64)1;
    (*p).ulist.vptr = ((struct pc_decls_varrec *)(pc_decls_frameptr) + (i64)1);
    (*result).tagx = (u64)((i64)65565);
    (*result).objptr = p;
}

static void pc_host_pch_stackvars(struct pc_decls_varrec * result) {
    pc_support_pcerror((byte*)"STACKVARS");
}

static void pc_host_pch_makeempty(struct pc_decls_varrec * a,struct pc_decls_varrec * result) {
    struct pc_decls_objrec *  p;
    i64 t;
    t = (i64)(pc_decls_ttbasetype[((i64)((*a).tag))]);
    if ((t == (i64)13)) {
        t = (*a).value;
    };
    if ((t==(i64)29)) {
        p = pc_objlib_emptylist;
        ++(*p).refcount;
    }else if ((t==(i64)5)) {
        pc_pcfns_pc_emptystring(result);
        return;
    }else if ((t==(i64)30)) {
        p = pc_objlib_array_new((i64)30,(i64)((*(*a).objptr).uarray.elemtag),(i64)0,(i64)1);
    } else {
        pc_support_pcustypet((byte*)"makeempty?",t);
    };
    (*result).tagx = (u64)((t | (i64)65536));
    (*result).objptr = p;
}

static void pc_host_pch_readlines(struct pc_decls_varrec * a,struct pc_decls_varrec * result) {
    byte *  p;
    byte *  q;
    byte *  pstart;
    struct pc_decls_varrec v;
    struct pc_decls_varrec *  r;
    struct pc_decls_objrec *  l;
    i64 nlines;
    i64 n;
    pc_host_checkparam(a,(i64)5,(i64)-999999);
    if (((i64)((*(*a).objptr).ustr.length) == (i64)0)) {
        //error:
L696 :;
;
        (*result).tagx = (u64)((i64)1);
        (*result).value = (i64)0;
        return;
    };
    p = mlib_readfile((*(*a).objptr).ustr.strptr);
    if ((p == 0)) {
        goto L696 ;
;
    };
    q = p;
    nlines = (i64)0;
    L697 :;
    switch ((i64)((*q++))) {
    case 26:;
    {
        goto L698 ;
    }break;
    case 13:;
    {
        ++nlines;
        if (((i64)((u64)((*q))) == (i64)10)) {
            ++q;
        };
    }break;
    case 10:;
    {
        ++nlines;
    }break;
    default: {
    }
    } //SW
goto L697 ;
L698 :;
    ;
    v.tagx = (u64)((i64)65565);
    l = pc_objlib_list_new(nlines,(i64)1,(struct pc_decls_varrec *)(0));
    v.objptr = l;
    r = (*v.objptr).ulist.vptr;
    q = p;
    pstart = q;
    L699 :;
    switch ((i64)((*q++))) {
    case 26:;
    {
        goto L700 ;
    }break;
    case 13:;
    {
        n = ((q - pstart) - (i64)1);
        if (((i64)((u64)((*q))) == (i64)10)) {
            ++q;
        };
        //addline:
L701 :;
;
        pc_pcfns_pc_makestring((byte *)(pstart),n,r);
        ++r;
        pstart = q;
    }break;
    case 10:;
    {
        n = ((q - pstart) - (i64)1);
        goto L701 ;
;
    }break;
    default: {
    }
    } //SW
goto L699 ;
L700 :;
    ;
    (*result) = v;
    free((void *)(p));
}

static void pc_host_pch_dictitems(struct pc_decls_varrec * a,struct pc_decls_varrec * result) {
    if (!!((u64)((*a).hasref))) {
        (*result).tagx = (u64)((i64)1);
        (*result).value = (i64)((*(*a).objptr).udict.dictitems);
    } else {
        pc_support_pcerror((byte*)".alloclen/not heap");
    };
}

static void pc_host_pch_setoverload(struct pc_decls_varrec * a,struct pc_decls_varrec * b,struct pc_decls_varrec * c) {
    void * (*tableptr)[];
    void (*handlerptr)(void);
    struct pc_host_overloadrec * *  ovptr;
    byte str[256];
    i64 i;
    i64 t;
    i64 cmd;
    static struct pc_host_pch_setoverload_rec table[2] = {
    {(i64)151,(void *)(&pc_decls_tostr_table),(void *)(&pc_host_tostr_handler),&pc_host_tostr_list},
    {(i64)85,(void *)(&pc_decls_convert_dtable),(void *)(&pc_host_convert_handler),&pc_host_convert_list}
};
    i64 av_1;
    pc_host_checkparam(a,(i64)14,(i64)-999999);
    pc_host_checkparam(b,(i64)13,(i64)-999999);
    pc_host_checkparam(c,(i64)18,(i64)-999999);
    cmd = (*a).value;
    L702 :;
    for (i=(i64)1;i<=(i64)2;i+=(i64)1) {
L703 :;
        if ((table[(i)-1].cmd == cmd)) {
            tableptr = (void * (*)[])(table[(i)-1].tableptr);
            handlerptr = (void (*)(void))(table[(i)-1].handleptr);
            ovptr = table[(i)-1].ovlist;
            goto L705 ;
        };
L704 :;
    }
    {
        msysnewc_m_print_startstr(str);
        msysnewc_m_print_str((byte*)"Setoverload: can't find calltable:",NULL);
        msysnewc_m_print_str((pq_common_cmdnames[(cmd)] + (i64)1),NULL);
        msysnewc_m_print_end();
        ;
        pc_support_pcerror(str);
    }L705 :;
    ;
    t = (*b).value;
    (*tableptr)[(t)] = (void *)(handlerptr);
    pc_host_addovrecord(ovptr,t,(u64 *)((*c).refptr));
}

static void pc_host_pch_errorinfo(struct pc_decls_varrec * a,struct pc_decls_varrec * result) {
    pc_host_checkparam(a,(i64)1,(i64)-999999);
    (*result).tagx = (u64)((i64)1);
    (*result).value = (i64)(pc_decls_err_pcptr);
}

static void pc_host_getbounds(struct pc_decls_varrec * p,struct pc_host_dimrec * dims,i64 lower) {
    i64 n;
    if (!(!!(p))) {
        pc_support_pcerror((byte*)"New: no bounds");
    };
    switch ((i64)((*p).tag)) {
    case 0:;
    {
        (*dims).lbound = lower;
        (*dims).upper = (i64)0;
        (*dims).length = (i64)0;
    }break;
    case 4:;
    {
        (*dims).lbound = (i64)((*p).range_lower);
        (*dims).upper = (i64)((*p).range_upper);
        (*dims).length = (((i64)((*p).range_upper) - (i64)((*p).range_lower)) + (i64)1);
        if (((*dims).length < (i64)0)) {
            (*dims).length = (i64)0;
            (*dims).upper = ((*dims).lbound - (i64)1);
        };
    }break;
    default: {
        n = pc_support_getintvalue(p);
        (*dims).lbound = lower;
        (*dims).upper = ((*dims).length = n);
    }
    } //SW
;
}

static i64 pc_host_checkparam(struct pc_decls_varrec * p,i64 tag,i64 defaultx) {
    if (((i64)((*p).tag)==(i64)0)) {
        if ((defaultx == (i64)-999999)) {
            pc_support_pcerror((byte*)"Missing host param");
        };
        return defaultx;
    }else if (((i64)((*p).tag)==tag)) {
        return (*p).value;
    };
    if ((tag == (i64)1)) {
        if (((i64)((*p).tag)==(i64)3)) {
            return (i64)((*p).xvalue);
        };
    };
    msysnewc_m_print_startcon();
    msysnewc_m_print_str(pc_decls_ttname[((i64)((*p).tag))],NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    pc_support_pcerror((byte*)"Host param wrong type");
    return (i64)0;
}

static void pc_host_leftstring(struct pc_decls_varrec * a,i64 n,struct pc_decls_varrec * result) {
    pc_pcfns_pc_makestring((*(*a).objptr).ustr.strptr,n,result);
}

static void pc_host_rightstring(struct pc_decls_varrec * a,i64 n,struct pc_decls_varrec * result) {
    pc_pcfns_pc_makestring(((*(*a).objptr).ustr.strptr + ((i64)((*(*a).objptr).ustr.length) - n)),n,result);
}

static void pc_host_padstring_right(struct pc_decls_varrec * a,i64 n,i64 fillchar,struct pc_decls_varrec * result) {
    byte *  s;
    i64 length;
    i64 av_1;
    length = (i64)((*(*a).objptr).ustr.length);
    pc_pcfns_pc_makestringn(n,result);
    s = (*(*result).objptr).ustr.strptr;
    if (!!(length)) {
        memcpy((void *)(s),(void *)((*(*a).objptr).ustr.strptr),(u64)(length));
        s += length;
    };
    av_1 = (n - length);
    while (av_1-- > 0) {
L706 :;
        (*s) = (u64)(fillchar);
        ++s;
L707 :;
    }L708 :;
    ;
}

static void pc_host_padstring_left(struct pc_decls_varrec * a,i64 n,i64 fillchar,struct pc_decls_varrec * result) {
    byte *  s;
    i64 length;
    i64 padlen;
    i64 av_1;
    length = (i64)((*(*a).objptr).ustr.length);
    padlen = (n - length);
    pc_pcfns_pc_makestringn(n,result);
    s = (*(*result).objptr).ustr.strptr;
    s += padlen;
    if (!!(length)) {
        memcpy((void *)(s),(void *)((*(*a).objptr).ustr.strptr),(u64)(length));
    };
    av_1 = padlen;
    while (av_1-- > 0) {
L709 :;
        --s;
        (*s) = (u64)(fillchar);
L710 :;
    }L711 :;
    ;
}

static void pc_host_pcld_makevint(struct pc_decls_varrec * p,i64 a) {
    (*p).tagx = (u64)((i64)1);
    (*p).value = a;
}

static void pc_host_pcld_makelist(struct pc_decls_varrec * p,struct pc_decls_varrec * result,i64 n) {
    struct pc_decls_varrec *  q;
    struct pc_decls_objrec *  r;
    i64 av_1;
    (*result).tagx = (u64)((i64)65565);
    r = pc_objlib_list_new(n,(i64)1,(struct pc_decls_varrec *)(0));
    (*result).objptr = r;
    q = (*r).ulist.vptr;
    av_1 = n;
    while (av_1-- > 0) {
L712 :;
        (*q) = (*p);
        pc_pcfns_pc_dupl(q);
        ++q;
        ++p;
L713 :;
    }L714 :;
    ;
}

static void pc_host_getproctabledata(struct pc_decls_procrec * p,struct pc_decls_varrec * result) {
    struct pc_decls_varrec table[4];
    struct pc_decls_strec *  d;
    i64 moduleno;
    d = (*p).def;
    pc_pcfns_pc_makestring((*d).name,(i64)-1,&table[((i64)1)-1]);
    pc_pcfns_pc_makestring((*d).metadata,(i64)-1,&table[((i64)2)-1]);
    moduleno = (i64)((*d).ax_moduleno);
    pc_host_pcld_makevint(&table[((i64)3)-1],moduleno);
    table[((i64)4)-1].tagx = (u64)((i64)18);
    table[((i64)4)-1].refptr = (byte *)((*d).pcaddress);
    pc_host_pcld_makelist(&table[((i64)1)-1],result,(i64)4);
}

static u64 * pc_host_convert_handler(i64 _1) {
    return (u64 *)(0);
}

static void pc_host_addtoproclist(struct pc_decls_strec * d) {
    struct pc_decls_procrec *  pp;
    ++pc_decls_nproclist;
    pp = (struct pc_decls_procrec *)(mlib_pcm_alloc((i64)16));
    (*pp).nextproc = pc_decls_proclist;
    pc_decls_proclist = pp;
    (*pp).def = d;
}

static u64 * pc_host_tostr_handler(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest) {
    struct pc_host_overloadrec *  q;
    struct pc_decls_varrec vdest;
    struct pc_decls_objrec *  vp;
    q = pc_host_tostr_list;
    q = pc_host_tostr_list;
    L715 :;
    while (!!(q)) {
        if (((*q).optype == pc_decls_overloadtype)) {
            goto L717 ;
        };
        q = (*q).nextrec;
L716 :;
    }L717 :;
    ;
    if ((q == 0)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str(pc_decls_ttname[((i64)((*p).tag))],NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcerror((byte*)"tostr/overload");
    };
    vdest.tagx = (u64)((i64)0);
    pci_runproc((void *)((*q).pchandler),p,fmtstr,&vdest);
    if (((i64)((u64)(vdest.tag)) != (i64)5)) {
        pc_support_pcerror((byte*)"custom tostr needs string result");
    };
    vp = vdest.objptr;
    if (!!((i64)((*vp).ustr.length))) {
        pc_print_addstring(dest,(*vp).ustr.strptr,(i64)((*vp).ustr.length));
    };
    pc_pcfns_pc_unshare(&vdest);
    return (u64 *)(0);
}

static u64 * pc_host_add_handler(struct pc_decls_varrec * p,struct pc_decls_varrec * fmtstr,struct pc_decls_fmtrec * fmt,struct pc_decls_objrec * dest) {
    struct pc_host_overloadrec *  q;
    struct pc_decls_varrec vdest;
    struct pc_decls_objrec *  vp;
    q = pc_host_tostr_list;
    q = pc_host_tostr_list;
    L718 :;
    while (!!(q)) {
        if (((*q).optype == pc_decls_overloadtype)) {
            goto L720 ;
        };
        q = (*q).nextrec;
L719 :;
    }L720 :;
    ;
    if ((q == 0)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str(pc_decls_ttname[((i64)((*p).tag))],NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcerror((byte*)"tostr/overload");
    };
    vdest.tagx = (u64)((i64)0);
    pci_runproc((void *)((*q).pchandler),p,fmtstr,&vdest);
    if (((i64)((u64)(vdest.tag)) != (i64)5)) {
        pc_support_pcerror((byte*)"custom tostr needs string result");
    };
    vp = vdest.objptr;
    if (!!((i64)((*vp).ustr.length))) {
        pc_print_addstring(dest,(*vp).ustr.strptr,(i64)((*vp).ustr.length));
    };
    pc_pcfns_pc_unshare(&vdest);
    return (u64 *)(0);
}

static void pc_host_addovrecord(struct pc_host_overloadrec * * p,i64 t,u64 * fnptr) {
    struct pc_host_overloadrec *  q;
    q = (struct pc_host_overloadrec *)(mlib_pcm_allocz((i64)32));
    (*q).optype = t;
    (*q).pchandler = fnptr;
    (*q).nextrec = (*p);
    (*p) = q;
}

static void (*pc_host_findapplproc(i64 fnindex))(void) {
    pc_support_pcerror((byte*)"EXPORTS NOT DONE");
    return (void (*)(void))(0);
}

void pc_host_do_callapplproc(i64 fnindex,i64 nargs,struct pc_decls_varrec * result) {
    void (*fnaddr)(void);
    byte (*paramlist)[12];
    i64 nparams;
    i64 rettype;
    i64 nextra;
    i64 na;
    u64 a;
    u64 wordargs[100];
    struct pc_decls_varrec *  args;
    struct pc_decls_varrec *  retv;
    struct msysnewc_procinforec *  info;
    i64 i;
    fnaddr = pc_decls_applproctable[(fnindex)-1].address;
    if ((fnaddr == 0)) {
        fnaddr = pc_host_findapplproc(fnindex);
    };
    info = pc_decls_applproctable[(fnindex)-1].info;
    rettype = (i64)((*info).rettype);
    nparams = (i64)((*info).nparams);
    paramlist = &(*info).paramlist;
    args = (result - (i64)1);
    nextra = (i64)0;
    if ((nparams > nargs)) {
        L721 :;
        for (i=(nargs + (i64)1);i<=nparams;i+=(i64)1) {
L722 :;
            if (!(!!(((i64)((u64)((*paramlist)[(i)-1])) & (i64)64)))) {
                pc_support_pcerror((byte*)"callappl: too few args or not optional");
            };
L723 :;
        }L724 :;
        ;
        nextra = (nparams - nargs);
    } else if ((nparams < nargs)) {
        pc_support_pcerror((byte*)"callappl: too many args");
    };
    na = (i64)0;
    L725 :;
    for (i=nargs;i>=(i64)1;i-=(i64)1) {
L726 :;
        wordargs[(++na)-1] = (u64)(pc_host_vartopack(args,((i64)((u64)((*paramlist)[(i)-1])) & (i64)63)));
        --args;
L727 :;
    }L728 :;
    ;
    a = oswindllc_os_pushargs(&wordargs,na,nextra,fnaddr,(rettype == (i64)3));
    (*result).tagx = (u64)(pc_host_packconvtypes[(rettype)]);
    (*result).value = (i64)(a);
    switch (rettype) {
    case 5:;
    case 6:;
    case 7:;
    case 8:;
    {
        (*result).uref.elemtag = (u64)(((rettype - (i64)5) + (i64)37));
    }break;
    case 10:;
    case 11:;
    case 12:;
    case 13:;
    {
        (*result).uref.elemtag = (u64)(((rettype - (i64)10) + (i64)44));
    }break;
    case 19:;
    {
        retv = (struct pc_decls_varrec *)(a);
        (*result) = (*retv);
    }break;
    default: {
    }
    } //SW
;
}

static i64 pc_host_vartopack(struct pc_decls_varrec * p,i64 tp) {
    i64 tag;
    i64 a;
    byte *  ss;
    double xx;
    tag = (i64)((*p).tag);
    a = (*p).value;
    switch (tp) {
    case 19:;
    {
        return (i64)(p);
    }break;
    case 1:;
    {
        if ((tag==(i64)1) || (tag==(i64)2)) {
            return a;
        }else if ((tag==(i64)3)) {
            return (i64)((*p).xvalue);
        };
    }break;
    case 3:;
    {
        if ((tag==(i64)1) || (tag==(i64)2)) {
            xx = (double)(a);
            return *(i64*)&xx;
        }else if ((tag==(i64)3)) {
            return *(i64*)&(*p).xvalue;
        };
    }break;
    case 18:;
    {
        if (((i64)((*p).tag)==(i64)5)) {
            ss = pc_support_convcstring((*(*p).objptr).ustr.strptr,(i64)((*(*p).objptr).ustr.length));
            return (i64)(*(u64*)&ss);
        };
    }break;
    case 5:;
    case 6:;
    case 7:;
    case 8:;
    case 9:;
    case 10:;
    case 11:;
    case 12:;
    case 13:;
    case 14:;
    case 15:;
    case 16:;
    {
        if (((i64)(pc_decls_ttbasetype[((i64)((*p).tag))])==(i64)30)) {
            return (i64)((*(*p).objptr).uarray.ptr);
        }else if (((i64)(pc_decls_ttbasetype[((i64)((*p).tag))])==(i64)23)) {
            return a;
        };
    }break;
    case 0:;
    {
        return (i64)0;
    }break;
    default: {
    }
    } //SW
;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str(pc_decls_ttname[(tag)],NULL);
    msysnewc_m_print_str((byte*)"=>",NULL);
    msysnewc_m_print_str(pc_host_packtypenames[(tp)],NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    pc_support_pcerror((byte*)"vartopack?");
    return (i64)0;
}

i64 pc_host_dummyfn(i64 a,i64 b,i64 c) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"DUMMY FN",NULL);
    msysnewc_m_print_i64(a,NULL);
    msysnewc_m_print_i64(b,NULL);
    msysnewc_m_print_i64(c,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    return ((a + b) + c);
}

struct pc_decls_varrec * pc_host_new_random(struct pc_decls_varrec * a) {
    struct pc_decls_varrec *  result;
    i64 n;
    i64 x;
    result = &pc_host_applresult;
    (*result).tagx = (u64)((i64)1);
    if (((i64)((u64)((*a).tag)) == (i64)4)) {
        x = mlib_mrandomrange((i64)((*a).range_lower),(i64)((*a).range_upper));
    } else {
        pc_host_checkparam(a,(i64)1,(i64)-999999);
        n = (*a).value;
        if ((n > (i64)1)) {
            x = mlib_mrandomint(n);
        } else if ((n == (i64)0)) {
            x = (i64)(mlib_mrandom());
        } else if ((n == (i64)1)) {
            (*result).tagx = (u64)((i64)3);
            (*result).xvalue = mlib_mrandomreal();
            return result;
        } else {
        };
    };
    (*result).value = x;
    return result;
}

struct pc_decls_varrec * pc_host_new_heapvar(struct pc_decls_varrec * a) {
    struct pc_decls_varrec *  result;
    result = &pc_host_applresult;
    (*result).tagx = (u64)((i64)1);
    (*result).value = (i64)12345679;
    return result;
}

i64 pc_host_mfib(i64 n) {
    if ((n < (i64)3)) {
        return (i64)1;
    } else {
        return (pc_host_mfib((n - (i64)1)) + pc_host_mfib((n - (i64)2)));
    };
}

void pc_dxfns_dx_iorset(struct pc_decls_varrec * x,struct pc_decls_varrec * y) {
    i64 xlen;
    i64 ylen;
    struct pc_decls_objrec *  px;
    struct pc_decls_objrec *  py;
    px = (*x).objptr;
    py = (*y).objptr;
    xlen = (i64)((*px).uset.length);
    ylen = (i64)((*py).uset.length);
    if ((ylen == (i64)0)) {
    } else if ((xlen == (i64)0)) {
        (*x) = (*y);
    } else {
        pc_pcfns_pc_duplvar(x);
        px = (*x).objptr;
        pc_dxfns_iresizeset(x,ylen);
        pc_dxfns_iorsetbits((i64 *)((*px).uset.ptr),(i64 *)((*py).uset.ptr),ylen);
        pc_pcfns_pc_unshare(y);
    };
}

void pc_dxfns_dx_iandset(struct pc_decls_varrec * x,struct pc_decls_varrec * y) {
    i64 xlen;
    i64 ylen;
    struct pc_decls_objrec *  px;
    struct pc_decls_objrec *  py;
    px = (*x).objptr;
    py = (*y).objptr;
    xlen = (i64)((*px).uset.length);
    ylen = (i64)((*py).uset.length);
    if ((ylen == (i64)0)) {
    } else if ((xlen == (i64)0)) {
        (*x) = (*y);
    } else {
        pc_pcfns_pc_duplvar(x);
        px = (*x).objptr;
        pc_dxfns_iresizeset(x,ylen);
        pc_dxfns_iandsetbits((i64 *)((*px).uset.ptr),(i64 *)((*py).uset.ptr),ylen);
        pc_pcfns_pc_unshare(y);
    };
}

void pc_dxfns_dx_ixorset(struct pc_decls_varrec * x,struct pc_decls_varrec * y) {
    i64 xlen;
    i64 ylen;
    struct pc_decls_objrec *  px;
    struct pc_decls_objrec *  py;
    px = (*x).objptr;
    py = (*y).objptr;
    xlen = (i64)((*px).uset.length);
    ylen = (i64)((*py).uset.length);
    if ((ylen == (i64)0)) {
    } else if ((xlen == (i64)0)) {
        (*x) = (*y);
    } else {
        pc_pcfns_pc_duplvar(x);
        px = (*x).objptr;
        pc_dxfns_iresizeset(x,ylen);
        pc_dxfns_ixorsetbits((i64 *)((*px).uset.ptr),(i64 *)((*py).uset.ptr),ylen);
        pc_pcfns_pc_unshare(y);
    };
}

void pc_dxfns_dx_inotset(struct pc_decls_varrec * x) {
    struct pc_decls_objrec *  px;
    px = (*x).objptr;
    if (!!((u64)((*px).uset.length))) {
        pc_pcfns_pc_duplvar(x);
        px = (*x).objptr;
        pc_dxfns_inotsetbits((i64 *)((*px).uset.ptr),(i64)((*px).uset.length));
    };
}

void pc_dxfns_dx_subset(struct pc_decls_varrec * x,struct pc_decls_varrec * y) {
    i64 xlen;
    i64 ylen;
    struct pc_decls_objrec *  px;
    struct pc_decls_objrec *  py;
    px = (*x).objptr;
    py = (*y).objptr;
    xlen = (i64)((*px).uset.length);
    ylen = (i64)((*py).uset.length);
    if ((!!(xlen) && !!(ylen))) {
        pc_pcfns_pc_dupl(x);
        px = (*x).objptr;
        pc_dxfns_iresizeset(x,ylen);
        pc_dxfns_subsetbits((i64 *)((*px).uset.ptr),(i64 *)((*py).uset.ptr),ylen);
    };
    pc_pcfns_pc_unshare(y);
}

void pc_dxfns_inotsetbits(i64 * p,i64 n) {
    i64 av_1;
    av_1 = (((n - (i64)1) / (i64)64) + (i64)1);
    while (av_1-- > 0) {
L729 :;
        (*p) = ~((*p));
        ++p;
L730 :;
    }L731 :;
    ;
}

void pc_dxfns_iorsetbits(i64 * p,i64 * q,i64 n) {
    i64 av_1;
    av_1 = (((n - (i64)1) / (i64)64) + (i64)1);
    while (av_1-- > 0) {
L732 :;
        (*p++) |= (*q++);
L733 :;
    }L734 :;
    ;
}

void pc_dxfns_iandsetbits(i64 * p,i64 * q,i64 n) {
    i64 av_1;
    av_1 = (((n - (i64)1) / (i64)64) + (i64)1);
    while (av_1-- > 0) {
L735 :;
        (*p++) &= (*q++);
L736 :;
    }L737 :;
    ;
}

void pc_dxfns_ixorsetbits(i64 * p,i64 * q,i64 n) {
    i64 av_1;
    av_1 = (((n - (i64)1) / (i64)64) + (i64)1);
    while (av_1-- > 0) {
L738 :;
        (*p++) ^= (*q++);
L739 :;
    }L740 :;
    ;
}

void pc_dxfns_subsetbits(i64 * p,i64 * q,i64 n) {
    i64 av_1;
    av_1 = (((n - (i64)1) / (i64)64) + (i64)1);
    while (av_1-- > 0) {
L741 :;
        (*p) |= (*q);
        (*p) ^= (*q);
        ++p;
        ++q;
L742 :;
    }L743 :;
    ;
}

void pc_dxfns_iresizeset(struct pc_decls_varrec * p,i64 n) {
    struct pc_decls_objrec *  pp;
    pp = (*p).objptr;
    if (((i64)((u64)((*pp).uset.length)) >= n)) {
        return;
    };
    pc_objlib_bits_resize(pp,n);
}

i64 pc_dxfns_dx_varinvar(struct pc_decls_varrec * x,struct pc_decls_varrec * y) {
    i64 i;
    i64 xt;
    i64 yt;
    i64 n;
    i64 a;
    struct pc_decls_varrec *  p;
    struct pc_decls_objrec *  px;
    struct pc_decls_objrec *  py;
    i64 av_1;
    xt = (i64)((*x).tag);
    yt = (i64)(pc_decls_ttbasetype[((i64)((*y).tag))]);
    px = (*x).objptr;
    py = (*y).objptr;
    switch (xt) {
    case 1:;
    {
        //doi64invar:
L744 :;
;
        switch (yt) {
        case 9:;
        {
            n = (*x).value;
            //doi64inset:
L745 :;
;
            if (((u64)((u32)(n)) >= (u64)((*py).uset.length))) {
                if (!!((u64)((*y).hasref))) {
                    pc_pcfns_pc_unshare(y);
                };
                return (i64)0;
            };
            n = pc_support_testelem((byte (*)[])((*py).uset.ptr),n);
            pc_pcfns_pc_unshare(y);
            return n;
        }break;
        case 29:;
        {
            a = (*x).value;
            n = (i64)((*py).ulist.length);
            p = (*py).ulist.vptr;
            L746 :;
            for (i=(i64)1;i<=n;i+=(i64)1) {
L747 :;
                if ((((i64)((u64)((*p).tag)) == (i64)1) && ((*p).value == a))) {
                    if (!!((u64)((*y).hasref))) {
                        pc_pcfns_pc_unshare(y);
                    };
                    return i;
                };
                ++p;
L748 :;
            }L749 :;
            ;
            pc_pcfns_pc_unshare(y);
            return (i64)0;
        }break;
        case 30:;
        {
            if (((i64)((*py).uarray.elemtag)==(i64)37) || ((i64)((*py).uarray.elemtag)==(i64)44)) {
                n = pc_pcfns_u8inarray((u64)((byte)((*x).value)),py);
            }else if (((i64)((*py).uarray.elemtag)==(i64)38) || ((i64)((*py).uarray.elemtag)==(i64)45)) {
                n = pc_pcfns_u16inarray((u64)((u16)((*x).value)),py);
            }else if (((i64)((*py).uarray.elemtag)==(i64)39) || ((i64)((*py).uarray.elemtag)==(i64)46)) {
                n = pc_pcfns_u32inarray((u64)((u32)((*x).value)),py);
            }else if (((i64)((*py).uarray.elemtag)==(i64)40) || ((i64)((*py).uarray.elemtag)==(i64)47)) {
                n = pc_pcfns_u64inarray((u64)((*x).value),py);
            } else {
                pc_support_pcustypet((byte*)"x in array",(i64)((*py).uarray.elemtag));
            };
            pc_pcfns_pc_unshare(y);
            return n;
        }break;
        case 31:;
        {
            if (((i64)((*py).ubits.elemtag)==(i64)41)) {
                n = pc_pcfns_bitinbits((u64)((byte)((*x).value)),py);
            } else {
                pc_support_pcustypet((byte*)"x in bits",(i64)((*py).ubits.elemtag));
            };
            pc_pcfns_pc_unshare(y);
            return n;
        }break;
        case 4:;
        {
            n = (*x).value;
            return ((n >= (i64)((*y).range_lower)) && (n <= (i64)((*y).range_upper)));
        }break;
        default: {
        }
        } //SW
;
    }break;
    case 5:;
    {
        switch (yt) {
        case 5:;
        {
            n = pc_pcfns_pc_strinstr(x,y);
            if (!!((u64)((*x).hasref))) {
                pc_pcfns_pc_unshare(x);
            };
            if (!!((u64)((*y).hasref))) {
                pc_pcfns_pc_unshare(y);
            };
            return n;
        }break;
        case 29:;
        {
            n = (i64)((*py).ulist.length);
            p = (*py).ulist.vptr;
            i = (i64)((*py).ulist.lower);
            av_1 = n;
            while (av_1-- > 0) {
L750 :;
                if (((i64)((u64)((*p).tag)) == (i64)5)) {
                    if (!!(pc_pcfns_pc_eqstring_nf(x,p))) {
                        if (!!((u64)((*x).hasref))) {
                            pc_pcfns_pc_unshare(x);
                        };
                        if (!!((u64)((*y).hasref))) {
                            pc_pcfns_pc_unshare(y);
                        };
                        return i;
                    };
                };
                ++p;
                ++i;
L751 :;
            }L752 :;
            ;
            if (!!((u64)((*x).hasref))) {
                pc_pcfns_pc_unshare(x);
            };
            if (!!((u64)((*y).hasref))) {
                pc_pcfns_pc_unshare(y);
            };
            return (i64)0;
        }break;
        default: {
        }
        } //SW
;
    }break;
    default: {
        switch (yt) {
        case 29:;
        {
            n = (i64)((*py).ulist.length);
            p = (*py).ulist.vptr;
            L753 :;
            for (i=(i64)1;i<=n;i+=(i64)1) {
L754 :;
                if ((pc_pcfns_pc_equal_nf(x,p,(i64)1) == (i64)1)) {
                    if (!!((u64)((*x).hasref))) {
                        pc_pcfns_pc_unshare(x);
                    };
                    if (!!((u64)((*y).hasref))) {
                        pc_pcfns_pc_unshare(y);
                    };
                    return i;
                };
                ++p;
L755 :;
            }L756 :;
            ;
            if (!!((u64)((*x).hasref))) {
                pc_pcfns_pc_unshare(x);
            };
            if (!!((u64)((*y).hasref))) {
                pc_pcfns_pc_unshare(y);
            };
            return (i64)0;
        }break;
        default: {
        }
        } //SW
;
    }
    } //SW
;
    pc_support_pcmxtypes((byte*)"varinvar:",x,y);
    return (i64)0;
}

i64 pc_dxfns_dx_mixed(struct pc_decls_varrec * x,struct pc_decls_varrec * y) {
    switch ((i64)((*x).tag)) {
    case 1:;
    {
        switch ((i64)((*y).tag)) {
        case 3:;
        {
            (*x).xvalue = (double)((*x).value);
            (*x).tagx = (u64)((i64)3);
            return (i64)3;
        }break;
        case 2:;
        {
            return (i64)1;
        }break;
        case 7:;
        {
            pc_bignum_bx_makeint((*x).value,x);
            return (i64)7;
        }break;
        case 0:;
        {
            goto L757 ;
;
        }break;
        default: {
            return (i64)0;
        }
        } //SW
;
    }break;
    case 3:;
    {
        switch ((i64)((*y).tag)) {
        case 1:;
        {
            (*y).xvalue = (double)((*y).value);
            (*y).tagx = (u64)((i64)3);
            return (i64)3;
        }break;
        case 0:;
        {
            goto L757 ;
;
        }break;
        default: {
            return (i64)0;
        }
        } //SW
;
    }break;
    case 2:;
    {
        switch ((i64)((*y).tag)) {
        case 1:;
        {
            return (i64)2;
        }break;
        case 0:;
        {
            goto L757 ;
;
        }break;
        default: {
            return (i64)0;
        }
        } //SW
;
    }break;
    case 7:;
    {
        switch ((i64)((*y).tag)) {
        case 1:;
        {
            pc_bignum_bx_makeint((*y).value,y);
            return (i64)7;
        }break;
        default: {
            return (i64)0;
        }
        } //SW
;
    }break;
    case 0:;
    {
        //dxvoid:
L757 :;
;
        pc_support_pcerror((byte*)"dxmix/void");
    }break;
    default: {
        if (((i64)((u64)((*y).tag)) == (i64)0)) {
            goto L757 ;
;
        };
        return (i64)0;
    }
    } //SW
;
    return (i64)0;
}

void * pc_khandlers_k_zero(void) {
    pc_support_pclunimpl((i64)0);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_nop(void) {
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_procstart(void) {
    pc_support_pclunimpl((i64)2);
    return (void *)((pc_decls_pcptr + (i64)3));
}

void * pc_khandlers_k_procend(void) {
    pc_support_pclunimpl((i64)3);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_endmodule(void) {
    pc_support_pclunimpl((i64)4);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_push_m(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr) = (*(struct pc_decls_varrec *)((*(pc_decls_pcptr + (i64)1))));
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        ++(*(*pc_decls_sptr).objptr).refcount;
    };
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_push_f(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr) = (*(struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)1))))));
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        ++(*(*pc_decls_sptr).objptr).refcount;
    };
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_push_am(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)22);
    (*pc_decls_sptr).varptr = (struct pc_decls_varrec *)((*(pc_decls_pcptr + (i64)1)));
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_push_af(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)22);
    (*pc_decls_sptr).varptr = (struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)1)))));
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_push_ap(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)18);
    (*pc_decls_sptr).refptr = (byte *)((*(pc_decls_pcptr + (i64)1)));
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_push_al(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)20);
    (*pc_decls_sptr).refptr = (byte *)((*(pc_decls_pcptr + (i64)1)));
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_push_ci(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = (i64)((*(pc_decls_pcptr + (i64)1)));
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_push_cw(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)2);
    (*pc_decls_sptr).uvalue = (*(pc_decls_pcptr + (i64)1));
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_push_cr(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)3);
    (*pc_decls_sptr).uvalue = (*(pc_decls_pcptr + (i64)1));
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_push_cn(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)4);
    (*pc_decls_sptr).uvalue = (*(pc_decls_pcptr + (i64)1));
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_push_cs(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)65541);
    (*pc_decls_sptr).objptr = (struct pc_decls_objrec *)((*(pc_decls_pcptr + (i64)1)));
    ++(*(*pc_decls_sptr).objptr).refcount;
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_push_t(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)13);
    (*pc_decls_sptr).value = (i64)((*(pc_decls_pcptr + (i64)1)));
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_push_op(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)14);
    (*pc_decls_sptr).value = (i64)((*(pc_decls_pcptr + (i64)1)));
    (*pc_decls_sptr).uop.opdims = (*(pc_decls_pcptr + (i64)2));
    return (void *)((pc_decls_pcptr + (i64)3));
}

void * pc_khandlers_k_pushz(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).tagx = (*(pc_decls_pcptr + (i64)1));
    (*pc_decls_sptr).value = (i64)0;
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_pushz_void(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)0);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_pushz_str(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).objptr = pc_objlib_emptystring;
    (*pc_decls_sptr).tagx = (u64)((i64)65541);
    ++(*pc_objlib_emptystring).refcount;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_pushz_list(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)65565);
    (*pc_decls_sptr).objptr = pc_objlib_emptylist;
    ++(*pc_objlib_emptylist).refcount;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_pushz_listl(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)65565);
    (*pc_decls_sptr).objptr = pc_objlib_list_new((i64)0,(i64)((*(pc_decls_pcptr + (i64)1))),(struct pc_decls_varrec *)(0));
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_pushz_set(void) {
    --pc_decls_sptr;
    (*pc_decls_sptr).tagx = (u64)((i64)65545);
    (*pc_decls_sptr).objptr = pc_objlib_emptyset;
    ++(*pc_objlib_emptyset).refcount;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_pushz_arrayl(void) {
    pc_support_pclunimpl((i64)24);
    return (void *)((pc_decls_pcptr + (i64)3));
}

void * pc_khandlers_k_pop_m(void) {
    struct pc_decls_varrec *  a;
    a = (struct pc_decls_varrec *)((*(pc_decls_pcptr + (i64)1)));
    if (!!((u64)((*a).hasref))) {
        pc_pcfns_pc_unshare(a);
    };
    (*a) = (*pc_decls_sptr++);
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_pop_f(void) {
    struct pc_decls_varrec *  a;
    a = (struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)1)))));
    if (!!((u64)((*a).hasref))) {
        pc_pcfns_pc_unshare(a);
    };
    (*a) = (*pc_decls_sptr++);
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_store_m(void) {
    struct pc_decls_varrec *  a;
    a = (struct pc_decls_varrec *)((*(pc_decls_pcptr + (i64)1)));
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        pc_pcfns_pc_share(pc_decls_sptr);
    };
    if (!!((u64)((*a).hasref))) {
        pc_pcfns_pc_unshare(a);
    };
    (*a) = (*pc_decls_sptr);
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_store_f(void) {
    struct pc_decls_varrec *  a;
    a = (struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)1)))));
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        pc_pcfns_pc_share(pc_decls_sptr);
    };
    if (!!((u64)((*a).hasref))) {
        pc_pcfns_pc_unshare(a);
    };
    (*a) = (*pc_decls_sptr);
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_pushptr(void) {
    switch ((i64)((*pc_decls_sptr).tag)) {
    case 22:;
    {
        (*pc_decls_sptr) = (*(*pc_decls_sptr).varptr);
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            ++(*(*pc_decls_sptr).objptr).refcount;
        };
    }break;
    case 25:;
    {
        (*pc_decls_sptr).tagx = (u64)(((i64)((u64)((*pc_decls_sptr).uref.elemtag)) | (i64)65536));
        ++(*(*pc_decls_sptr).objptr).refcount;
    }break;
    case 23:;
    {
        pc_pcfns_pc_loadpacked((void *)((*pc_decls_sptr).uref.ptr),(i64)((*pc_decls_sptr).uref.elemtag),pc_decls_sptr,(struct pc_decls_objrec *)(0));
    }break;
    case 24:;
    {
        pc_pcfns_pc_loadbit((*pc_decls_sptr).uref.ptr,(i64)((*pc_decls_sptr).uref.bitoffset),(i64)((*pc_decls_sptr).uref.elemtag),(i64)((*pc_decls_sptr).uref.bitlength),pc_decls_sptr);
    }break;
    default: {
        pc_support_pcustype((byte*)"pushptr",pc_decls_sptr);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_popptr(void) {
    struct pc_decls_varrec *  p;
    struct pc_decls_varrec *  q;
    p = pc_decls_sptr++;
    switch ((i64)((*p).tag)) {
    case 22:;
    {
        q = (*p).varptr;
        if (!!((u64)((*q).hasref))) {
            pc_pcfns_pc_unshare(q);
        };
        (*q) = (*pc_decls_sptr++);
    }break;
    case 5:;
    {
        pc_pcfns_pc_storestring(p,pc_decls_sptr);
        ++pc_decls_sptr;
    }break;
    default: {
        pc_pcfns_pc_storeptr(p,pc_decls_sptr);
        ++pc_decls_sptr;
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_storeptr(void) {
    struct pc_decls_varrec *  p;
    struct pc_decls_varrec *  q;
    p = pc_decls_sptr++;
    if (((i64)((u64)((*p).tag)) == (i64)22)) {
        q = (*p).varptr;
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            pc_pcfns_pc_share(pc_decls_sptr);
        };
        if (!!((u64)((*q).hasref))) {
            pc_pcfns_pc_unshare(q);
        };
        (*q) = (*pc_decls_sptr);
    } else {
        pc_pcfns_pc_storeptr(p,pc_decls_sptr);
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            pc_pcfns_pc_unshare(pc_decls_sptr);
        };
    };
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_zpop_m(void) {
    struct pc_decls_varrec *  a;
    a = (struct pc_decls_varrec *)((*(pc_decls_pcptr + (i64)1)));
    (*a) = (*pc_decls_sptr++);
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_zpop_f(void) {
    struct pc_decls_varrec *  a;
    a = (struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)1)))));
    (*a) = (*pc_decls_sptr++);
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_zstore_m(void) {
    pc_support_pclunimpl((i64)34);
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_zstore_f(void) {
    struct pc_decls_varrec *  a;
    a = (struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)1)))));
    (*a) = (*pc_decls_sptr);
    if (!!((u64)((*a).hasref))) {
        pc_pcfns_pc_share(a);
    };
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        ++(*(*pc_decls_sptr).objptr).refcount;
    };
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_copy(void) {
    if (!(!!((u64)((*pc_decls_sptr).hasref)))) {
        return (void *)((pc_decls_pcptr + (i64)1));
    };
    pc_pcfns_pc_dupl(pc_decls_sptr);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_swap(void) {
    byte tempbuffer[1024];
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    struct pc_decls_varrec v;
    i64 xt;
    i64 yt;
    i64 s;
    i64 t;
    i64 n;
    byte *  p;
    byte *  q;
    i64 a;
    x = pc_decls_sptr++;
    y = pc_decls_sptr++;
    xt = (i64)((*x).tag);
    yt = (i64)((*y).tag);
    if (((xt == (i64)22) && (yt == (i64)22))) {
        v = (*(*x).varptr);
        (*(*x).varptr) = (*(*y).varptr);
        (*(*y).varptr) = v;
    } else if (((xt == (i64)23) && (yt == (i64)23))) {
        s = (i64)((*x).uref.elemtag);
        t = (i64)((*y).uref.elemtag);
        if ((s != t)) {
            goto L758 ;
;
        };
        n = pc_decls_ttsize[(s)];
        if ((n==(i64)1)) {
            p = (*x).uref.ptr;
            q = (*y).uref.ptr;
            a = (i64)((*p));
            (*p) = (u64)((*q));
            (*q) = (u64)(a);
        } else {
            if ((pc_decls_ttsize[(s)] <= (i64)1024)) {
                memcpy((void *)(&tempbuffer),(void *)((*x).uref.ptr),(u64)(n));
                memcpy((void *)((*x).uref.ptr),(void *)((*y).uref.ptr),(u64)(n));
                memcpy((void *)((*y).uref.ptr),(void *)(&tempbuffer),(u64)(n));
            } else {
                goto L758 ;
;
            };
        };
    } else {
        //swaperror:
L758 :;
;
        pc_support_pcmxtypes((byte*)"SWAP",x,y);
    };
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_convptr(void) {
    struct pc_decls_varrec *  a;
    i64 elemtype;
    void *  p;
    struct pc_decls_objrec *  pa;
    switch ((i64)((*pc_decls_sptr).tag)) {
    case 22:;
    {
        a = (*pc_decls_sptr).varptr;
        pa = (*a).objptr;
        switch ((i64)(pc_decls_ttbasetype[((i64)((*a).tag))])) {
        case 1:;
        case 2:;
        {
            p = (void *)(&(*a).value);
            elemtype = (i64)40;
        }break;
        case 3:;
        {
            p = (void *)(&(*a).value);
            elemtype = (i64)49;
        }break;
        case 30:;
        {
            p = (void *)((*pa).uarray.ptr);
            elemtype = (i64)((*(*a).objptr).uarray.elemtag);
        }break;
        case 5:;
        {
            p = (void *)((*pa).ustr.strptr);
            elemtype = (i64)44;
            if ((p == 0)) {
                p = (void *)((byte*)"");
            };
        }break;
        case 33:;
        {
            p = (void *)((*pa).ustruct.ptr);
            elemtype = (i64)((*a).tag);
        }break;
        default: {
            if (!!((u64)((*a).hasref))) {
                p = (void *)(pa);
                elemtype = (i64)0;
                goto L759 ;
;
            };
            msysnewc_m_print_startcon();
            msysnewc_m_print_str(pc_support_gettypename((i64)(pc_decls_ttbasetype[((i64)((*a).tag))])),NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            pc_support_pcustype((byte*)"Getrefpack1",a);
            return (void *)(pc_decls_pcptr);
        }
        } //SW
;
    }break;
    case 23:;
    case 24:;
    {
        return (void *)((pc_decls_pcptr + (i64)1));
    }break;
    default: {
        pc_support_pcustype((byte*)"Getrefpack2",pc_decls_sptr);
        return (void *)(pc_decls_pcptr);
    }
    } //SW
;
    //done:
L759 :;
;
    (*pc_decls_sptr).tagx = (u64)((i64)23);
    (*pc_decls_sptr).uref.ptr = (byte *)(p);
    (*pc_decls_sptr).uref.elemtag = (u64)(elemtype);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_jump(void) {
    return (void *)((i64 *)((*(pc_decls_pcptr + (i64)1))));
}

void * pc_khandlers_k_jumpptr(void) {
    if (((i64)((u64)((*pc_decls_sptr).tag)) != (i64)20)) {
        pc_support_pcerror((byte*)"Bad label ptr");
    };
    pc_decls_pcptr = (u64 *)((*pc_decls_sptr).refptr);
    ++pc_decls_sptr;
    return (void *)(pc_decls_pcptr);
}

void * pc_khandlers_k_jumptrue(void) {
    return (void *)(((*pc_decls_jumptrue_table[((i64)((*pc_decls_sptr).tag))]))());
}

void * pc_khandlers_k_jumpfalse(void) {
    return (void *)(((*pc_decls_jumpfalse_table[((i64)((*pc_decls_sptr).tag))]))());
}

void * pc_khandlers_k_jumpdef(void) {
    if (((i64)((u64)((*pc_decls_sptr).tag)) != (i64)0)) {
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            pc_pcfns_pc_unshare(pc_decls_sptr);
        };
        ++pc_decls_sptr;
        return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
    };
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        pc_pcfns_pc_unshare(pc_decls_sptr);
    };
    ++pc_decls_sptr;
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_jumpvoid(void) {
    if (((i64)((u64)((*pc_decls_sptr).tag)) == (i64)0)) {
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            pc_pcfns_pc_unshare(pc_decls_sptr);
        };
        ++pc_decls_sptr;
        return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
    };
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        pc_pcfns_pc_unshare(pc_decls_sptr);
    };
    ++pc_decls_sptr;
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_jumpeq(void) {
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        return (void *)(((*pc_decls_jumpeq_table[(yt)]))());
    };
    pc_decls_opc_tableptr = &pc_decls_jumpeq_table;
    return (void *)(((*pc_decls_jumpeq_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_jumpne(void) {
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        return (void *)(((*pc_decls_jumpne_table[(yt)]))());
    };
    pc_decls_opc_tableptr = &pc_decls_jumpne_table;
    return (void *)(((*pc_decls_jumpne_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_jumplt(void) {
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        return (void *)(((*pc_decls_jumplt_table[(yt)]))());
    };
    pc_decls_opc_tableptr = &pc_decls_jumplt_table;
    return (void *)(((*pc_decls_jumplt_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_jumple(void) {
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        return (void *)(((*pc_decls_jumple_table[(yt)]))());
    };
    pc_decls_opc_tableptr = &pc_decls_jumple_table;
    return (void *)(((*pc_decls_jumple_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_jumpge(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        switch (yt) {
        case 1:;
        {
            y = pc_decls_sptr++;
            if (((*pc_decls_sptr).value >= (*y).value)) {
                ++pc_decls_sptr;
                return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
            };
            ++pc_decls_sptr;
            return (void *)((pc_decls_pcptr + (i64)2));
        }break;
        default: {
            return (void *)(((*pc_decls_jumpge_table[(yt)]))());
        }
        } //SW
;
    };
    pc_decls_opc_tableptr = &pc_decls_jumpge_table;
    return (void *)(((*pc_decls_jumpge_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_jumpgt(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        switch (yt) {
        case 1:;
        {
            y = pc_decls_sptr++;
            if (((*pc_decls_sptr).value > (*y).value)) {
                ++pc_decls_sptr;
                return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
            };
            ++pc_decls_sptr;
            return (void *)((pc_decls_pcptr + (i64)2));
        }break;
        default: {
            return (void *)(((*pc_decls_jumpgt_table[(yt)]))());
        }
        } //SW
;
    };
    pc_decls_opc_tableptr = &pc_decls_jumpgt_table;
    return (void *)(((*pc_decls_jumpgt_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_jumptesteq(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 xt;
    i64 yt;
    i64 res;
    struct pc_decls_objrec *  py;
    y = pc_decls_sptr++;
    x = pc_decls_sptr;
    xt = (i64)((*x).tag);
    yt = (i64)((*y).tag);
    if ((xt != yt)) {
        if (!!((xt = pc_dxfns_dx_mixed(x,y)))) {
            goto L760 ;
;
        };
        xt = (i64)((*x).tag);
        yt = (i64)((*y).tag);
        switch (xt) {
        case 1:;
        {
            switch (yt) {
            case 4:;
            {
                if ((((*x).value < (i64)((*y).range_lower)) || ((*x).value > (i64)((*y).range_upper)))) {
                    return (void *)((pc_decls_pcptr + (i64)2));
                };
            }break;
            case 9:;
            {
                py = (*y).objptr;
                if (((((i64)((u64)((*py).uset.length)) == (i64)0) || ((*x).value >= (i64)((u64)((*py).uset.length)))) || !(!!(pc_support_testelem((byte (*)[])((*py).uset.ptr),(*x).value))))) {
                    return (void *)((pc_decls_pcptr + (i64)2));
                };
            }break;
            default: {
                return (void *)((pc_decls_pcptr + (i64)2));
            }
            } //SW
;
        }break;
        default: {
            return (void *)((pc_decls_pcptr + (i64)2));
        }
        } //SW
;
        ++pc_decls_sptr;
        return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
    };
    //retry:
L760 :;
;
    switch (xt) {
    case 1:;
    case 13:;
    {
        if (((*x).value == (*y).value)) {
            ++pc_decls_sptr;
            return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
        };
    }break;
    case 3:;
    {
        if (((*x).xvalue == (*y).xvalue)) {
            ++pc_decls_sptr;
            return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
        };
    }break;
    case 4:;
    {
        if (((*x).value == (*y).value)) {
            ++pc_decls_sptr;
            return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
        };
    }break;
    case 5:;
    {
        res = pc_pcfns_pc_eqstring_nf(x,y);
        if (!!((u64)((*y).hasref))) {
            pc_pcfns_pc_unshare(y);
        };
        if (!!(res)) {
            if (!!((u64)((*x).hasref))) {
                pc_pcfns_pc_unshare(x);
            };
            ++pc_decls_sptr;
            return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
        };
    }break;
    default: {
        res = pc_pcfns_pc_equal_nf(x,y,(i64)0);
        if (!!((u64)((*x).hasref))) {
            pc_pcfns_pc_unshare(x);
        };
        if (!!((u64)((*y).hasref))) {
            pc_pcfns_pc_unshare(y);
        };
        if (!!(res)) {
            ++pc_decls_sptr;
            return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
        };
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_jumptestne(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 xt;
    i64 yt;
    i64 res;
    struct pc_decls_objrec *  py;
    y = pc_decls_sptr++;
    x = pc_decls_sptr;
    xt = (i64)((*x).tag);
    yt = (i64)((*y).tag);
    if ((xt != yt)) {
        if (!!((xt = pc_dxfns_dx_mixed(x,y)))) {
            goto L761 ;
;
        };
        xt = (i64)((*x).tag);
        yt = (i64)((*y).tag);
        switch (xt) {
        case 1:;
        {
            switch (yt) {
            case 4:;
            {
                if ((((*x).value >= (i64)((*y).range_lower)) && ((*x).value <= (i64)((*y).range_upper)))) {
                    ++pc_decls_sptr;
                    return (void *)((pc_decls_pcptr + (i64)2));
                };
            }break;
            case 9:;
            {
                py = (*y).objptr;
                if ((((*x).value < (i64)((u64)((*py).uset.length))) && !!(pc_support_testelem((byte (*)[])((*py).uset.ptr),(*x).value)))) {
                    ++pc_decls_sptr;
                    return (void *)((pc_decls_pcptr + (i64)2));
                };
            }break;
            default: {
            }
            } //SW
;
        }break;
        default: {
        }
        } //SW
;
        return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
    };
    //retry:
L761 :;
;
    switch (xt) {
    case 1:;
    case 13:;
    {
        if (((*x).value != (*y).value)) {
            return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
        };
        ++pc_decls_sptr;
    }break;
    case 3:;
    {
        if (((*x).xvalue != (*y).xvalue)) {
            return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
        };
        ++pc_decls_sptr;
    }break;
    case 4:;
    {
        if (((*x).value != (*y).value)) {
            return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
        };
        ++pc_decls_sptr;
    }break;
    case 5:;
    {
        res = pc_pcfns_pc_eqstring_nf(x,y);
        if (!!((u64)((*y).hasref))) {
            pc_pcfns_pc_unshare(y);
        };
        if (!(!!(res))) {
            return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
        };
        if (!!((u64)((*x).hasref))) {
            pc_pcfns_pc_unshare(x);
        };
        ++pc_decls_sptr;
    }break;
    default: {
        res = pc_pcfns_pc_equal_nf(x,y,(i64)0);
        if (!!((u64)((*y).hasref))) {
            pc_pcfns_pc_unshare(y);
        };
        if (!(!!(res))) {
            return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
        };
        if (!!((u64)((*x).hasref))) {
            pc_pcfns_pc_unshare(x);
        };
        ++pc_decls_sptr;
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_jumplabel(void) {
    pc_support_pclunimpl((i64)53);
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_jumpclabel(void) {
    pc_support_pclunimpl((i64)54);
    return (void *)((pc_decls_pcptr + (i64)3));
}

void * pc_khandlers_k_switch(void) {
    i64 index;
    i64 n;
    i64 lower;
    n = (i64)((*(pc_decls_pcptr + (i64)1)));
    lower = (i64)((*(pc_decls_pcptr + (i64)2)));
    if (((i64)((*pc_decls_sptr).tag)==(i64)1) || ((i64)((*pc_decls_sptr).tag)==(i64)13)) {
    } else {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str(pc_decls_ttname[((i64)((*pc_decls_sptr).tag))],NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcerror((byte*)"switch not int");
    };
    index = ((*pc_decls_sptr++).value - lower);
    if (((u64)((u32)(index)) >= (u64)((u32)(n)))) {
        return (void *)((i64 *)((*((pc_decls_pcptr + (n * (i64)2)) + (i64)4))));
    } else {
        return (void *)((i64 *)((*((pc_decls_pcptr + (index * (i64)2)) + (i64)4))));
    };
}

void * pc_khandlers_k_cswitch(void) {
    pc_support_pclunimpl((i64)56);
    return (void *)((pc_decls_pcptr + (i64)4));
}

void * pc_khandlers_k_new(void) {
    pc_support_pclunimpl((i64)57);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_to_f(void) {
    if (!!(--(*(struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)2)))))).value)) {
        return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
    } else {
        return (void *)((pc_decls_pcptr + (i64)3));
    };
}

void * pc_khandlers_k_for_fci(void) {
    if ((++(*(struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)2)))))).value <= (i64)((*(pc_decls_pcptr + (i64)3))))) {
        return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
    } else {
        return (void *)((pc_decls_pcptr + (i64)4));
    };
}

void * pc_khandlers_k_for_ff(void) {
    if ((++(*(struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)2)))))).value <= (*(struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)3)))))).value)) {
        return (void *)((i64 *)((*(pc_decls_pcptr + (i64)1))));
    } else {
        return (void *)((pc_decls_pcptr + (i64)4));
    };
}

void * pc_khandlers_k_ford_fci(void) {
    if ((--(*(struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)2)))))).value >= (i64)((*(pc_decls_pcptr + (i64)3))))) {
        return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
    } else {
        return (void *)((pc_decls_pcptr + (i64)4));
    };
}

void * pc_khandlers_k_ford_ff(void) {
    if ((--(*(struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)2)))))).value >= (*(struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)3)))))).value)) {
        return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
    } else {
        return (void *)((pc_decls_pcptr + (i64)4));
    };
}

void * pc_khandlers_k_call(void) {
    static i64 count = (i64)10;
    if ((--count == (i64)0)) {
        count = (i64)10;
        oswindows_os_peek();
    };
    if ((pc_decls_sptr <= pc_decls_stacklimit)) {
        pc_support_pcerror((byte*)"STACK OVERFLOW");
    };
    (*--pc_decls_sptr).tagx = (u64)((i64)16);
    (*pc_decls_sptr).uret.retaddr = (pc_decls_pcptr + (i64)3);
    (*pc_decls_sptr).uret.frameptr_low = (i64)(*(i32*)&pc_decls_frameptr);
    (*pc_decls_sptr).uret.stackadj = (*(pc_decls_pcptr + (i64)2));
    pc_decls_frameptr = (byte *)(pc_decls_sptr);
    return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
}

void * pc_khandlers_k_callptr(void) {
    u64 *  newpc;
    if (((i64)((u64)((*pc_decls_sptr).tag)) != (i64)18)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str(pc_decls_ttname[((i64)((*pc_decls_sptr).tag))],NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcerror((byte*)"callptr: not refproc");
    };
    newpc = (u64 *)((*pc_decls_sptr).value);
    if (((*(pc_decls_pcptr + (i64)1)) != (*(newpc - (i64)1)))) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_u64((*(pc_decls_pcptr + (i64)1)),NULL);
        msysnewc_m_print_u64((*(newpc - (i64)1)),NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcerror((byte*)"callptr wrong # params");
    };
    (*pc_decls_sptr).tagx = (u64)((i64)16);
    (*pc_decls_sptr).uret.retaddr = (pc_decls_pcptr + (i64)3);
    (*pc_decls_sptr).uret.frameptr_low = (i64)(*(i32*)&pc_decls_frameptr);
    (*pc_decls_sptr).uret.stackadj = (*(pc_decls_pcptr + (i64)2));
    pc_decls_frameptr = (byte *)(pc_decls_sptr);
    return (void *)(newpc);
}

void * pc_khandlers_k_return(void) {
    if (((i64)((u64)((*pc_decls_sptr).tag)) != (i64)16)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str(pc_decls_ttname[((i64)((*pc_decls_sptr).tag))],NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcerror((byte*)"Return error");
    };
    (*pc_decls_sptr).tag = (u64)((i64)0);
    pc_decls_pcptr = (*pc_decls_sptr).uret.retaddr;
    (*(i32 *)(&pc_decls_frameptr)) = (i64)((*pc_decls_sptr).uret.frameptr_low);
    pc_decls_sptr = (struct pc_decls_varrec *)(((byte *)(pc_decls_sptr) + (i64)((*pc_decls_sptr).uret.stackadj)));
    ++pc_decls_sptr;
    return (void *)(pc_decls_pcptr);
}

void * pc_khandlers_k_startdll(void) {
    if ((++pc_decls_dllcallindex > (i64)30)) {
        pc_support_pcerror((byte*)"nested dll max");
    };
    pc_decls_dllcallstack[(pc_decls_dllcallindex)-1] = pc_decls_dllindex;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_pushdll(void) {
    i64 s;
    i64 t;
    i64 u;
    struct pc_decls_objrec *  p;
    if ((++pc_decls_dllindex > (i64)29)) {
        pc_support_pcerror((byte*)"dll params");
    };
    s = (i64)((*pc_decls_sptr).tag);
    if ((s == (i64)0)) {
        pc_support_pcerror((byte*)"pushdll void arg");
    };
    t = (i64)(pc_decls_ttbasetype[((i64)((*(pc_decls_pcptr + (i64)1))))]);
    u = (i64)2;
    switch (t) {
    case 0:;
    {
        t = s;
        switch (s) {
        case 5:;
        {
            pc_decls_dllparams[(pc_decls_dllindex)-1] = (i64)(pc_support_convcstring((*(*pc_decls_sptr).objptr).ustr.strptr,(i64)((*(*pc_decls_sptr).objptr).ustr.length)));
        }break;
        default: {
            pc_decls_dllparams[(pc_decls_dllindex)-1] = (*pc_decls_sptr).value;
        }
        } //SW
;
        if ((s == (i64)3)) {
            u = (i64)3;
        };
    }break;
    case 40:;
    case 47:;
    case 39:;
    case 46:;
    {
        if (((i64)64 == (i64)32)) {
        };
        if ((s==(i64)1) || (s==(i64)2)) {
        }else if ((s==(i64)3)) {
            (*pc_decls_sptr).value = (i64)((*pc_decls_sptr).xvalue);
        }else if ((s==(i64)23)) {
            (*pc_decls_sptr).value = (i64)((*pc_decls_sptr).uref.ptr);
        } else {
            //error:
L762 :;
;
            msysnewc_m_print_startcon();
            msysnewc_m_print_str(pc_decls_ttname[(s)],NULL);
            msysnewc_m_print_str((byte*)"should be",NULL);
            msysnewc_m_print_str(pc_decls_ttname[(t)],NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            pc_support_pcerror((byte*)"DLL: param wrong type");
        };
        pc_decls_dllparams[(pc_decls_dllindex)-1] = (*pc_decls_sptr).value;
    }break;
    case 49:;
    {
        if (((i64)64 == (i64)32)) {
        };
        if ((s==(i64)1) || (s==(i64)2)) {
            (*pc_decls_sptr).xvalue = (double)((*pc_decls_sptr).value);
        }else if ((s==(i64)3)) {
        } else {
            goto L762 ;
;
        };
        u = (i64)3;
        pc_decls_dllparams[(pc_decls_dllindex)-1] = (*pc_decls_sptr).value;
    }break;
    case 5:;
    {
        if ((s==(i64)5)) {
            p = (*pc_decls_sptr).objptr;
            pc_decls_dllparams[(pc_decls_dllindex)-1] = (i64)(pc_support_convcstring((*p).ustr.strptr,(i64)((*p).ustr.length)));
        }else if ((s==(i64)1)) {
            if (((*pc_decls_sptr).value != (i64)0)) {
                goto L762 ;
;
            };
            pc_decls_dllparams[(pc_decls_dllindex)-1] = (i64)0;
        } else {
            goto L762 ;
;
        };
    }break;
    case 23:;
    {
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            pc_decls_dllparams[(pc_decls_dllindex)-1] = (i64)((*(*pc_decls_sptr).objptr).uarray.ptr);
        } else {
            pc_decls_dllparams[(pc_decls_dllindex)-1] = (i64)((*pc_decls_sptr).refptr);
        };
    }break;
    case 52:;
    case 50:;
    case 51:;
    case 45:;
    {
        pc_decls_dllparams[(pc_decls_dllindex)-1] = (*pc_decls_sptr).value;
    }break;
    default: {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str(pc_decls_ttname[(t)],NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcerror((byte*)"OTHER DLL PARAM");
        pc_decls_dllparams[(pc_decls_dllindex)-1] = (*pc_decls_sptr).value;
    }
    } //SW
;
    pc_decls_dlltypes[(pc_decls_dllindex)-1] = u;
    if (((i64)((u64)((*pc_decls_sptr).tag)) == (i64)5)) {
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            pc_pcfns_pc_unshare(pc_decls_sptr);
        };
    };
    ++pc_decls_sptr;
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_calldll(void) {
    i64 nparams;
    i64 offset;
    if ((pc_decls_dllcallindex <= (i64)0)) {
        pc_support_pcerror((byte*)"calldll??");
    };
    offset = pc_decls_dllcallstack[(pc_decls_dllcallindex)-1];
    nparams = (pc_decls_dllindex - offset);
    pc_oslayer_os_calldll((i64)((*(pc_decls_pcptr + (i64)2))),(i64)((*(pc_decls_pcptr + (i64)1))),offset,nparams,(i64)((*(pc_decls_pcptr + (i64)3))),pc_decls_sptr);
    pc_decls_dllindex = pc_decls_dllcallstack[(pc_decls_dllcallindex)-1];
    --pc_decls_dllcallindex;
    return (void *)((pc_decls_pcptr + (i64)4));
}

void * pc_khandlers_k_callhost(void) {
    pc_host_callhostfunction((i64)((*(pc_decls_pcptr + (i64)1))),(i64)0);
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_stackframe(void) {
    i64 n;
    i64 av_1;
    n = (i64)((*(pc_decls_pcptr + (i64)1)));
    av_1 = n;
    while (av_1-- > 0) {
L763 :;
        (*--pc_decls_sptr).tagx = (u64)((i64)0);
        (*pc_decls_sptr).value = (i64)0;
L764 :;
    }L765 :;
    ;
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_free(void) {
    i64 n;
    i64 av_1;
    n = (i64)((*(pc_decls_pcptr + (i64)1)));
    av_1 = n;
    while (av_1-- > 0) {
L766 :;
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            pc_pcfns_pc_unshare(pc_decls_sptr);
        };
        ++pc_decls_sptr;
L767 :;
    }L768 :;
    ;
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_addsp(void) {
    pc_decls_sptr = (struct pc_decls_varrec *)(((byte *)(pc_decls_sptr) + (i64)((*(pc_decls_pcptr + (i64)1)))));
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_stop(void) {
    pc_khandlers_stopped = (u64)((i64)1);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_test(void) {
    pc_support_pclunimpl((i64)74);
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_makelist(void) {
    i64 n;
    i64 lower;
    n = (i64)((*(pc_decls_pcptr + (i64)1)));
    lower = (i64)((*(pc_decls_pcptr + (i64)2)));
    pc_pcfns_pc_makelist(n,pc_decls_sptr,((pc_decls_sptr + n) - (i64)1),lower);
    pc_decls_sptr += (n - (i64)1);
    return (void *)((pc_decls_pcptr + (i64)3));
}

void * pc_khandlers_k_makerecord(void) {
    i64 n;
    i64 t;
    n = (i64)((*(pc_decls_pcptr + (i64)1)));
    t = (i64)((*(pc_decls_pcptr + (i64)2)));
    pc_pcfns_pc_makerecord(n,t,pc_decls_sptr,((pc_decls_sptr + n) - (i64)1));
    pc_decls_sptr += (n - (i64)1);
    return (void *)((pc_decls_pcptr + (i64)3));
}

void * pc_khandlers_k_makearray(void) {
    i64 n;
    i64 lower;
    i64 t;
    n = (i64)((*(pc_decls_pcptr + (i64)1)));
    lower = (i64)((*(pc_decls_pcptr + (i64)2)));
    t = (i64)((*(pc_decls_pcptr + (i64)3)));
    if (((i64)(pc_decls_ttbasetype[(t)]) == (i64)30)) {
        pc_pcfns_pc_makearray(n,t,(i64)((*(pc_decls_pcptr + (i64)4))),lower,pc_decls_sptr,((pc_decls_sptr + n) - (i64)1));
    } else {
        pc_support_pcerror((byte*)"MAKEBITS");
    };
    pc_decls_sptr += (n - (i64)1);
    return (void *)((pc_decls_pcptr + (i64)5));
}

void * pc_khandlers_k_makestruct(void) {
    i64 n;
    i64 t;
    n = (i64)((*(pc_decls_pcptr + (i64)1)));
    t = (i64)((*(pc_decls_pcptr + (i64)2)));
    pc_pcfns_pc_makestruct(n,t,pc_decls_sptr,((pc_decls_sptr + n) - (i64)1));
    pc_decls_sptr += (n - (i64)1);
    return (void *)((pc_decls_pcptr + (i64)3));
}

void * pc_khandlers_k_makeset(void) {
    i64 n;
    n = (i64)((*(pc_decls_pcptr + (i64)1)));
    pc_pcfns_pc_makeset(n,pc_decls_sptr,((pc_decls_sptr + n) - (i64)1));
    pc_decls_sptr += (n - (i64)1);
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_makerange(void) {
    struct pc_decls_varrec *  y;
    y = pc_decls_sptr++;
    pc_pcfns_pc_makerange(pc_decls_sptr,y,pc_decls_sptr);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_makedict(void) {
    i64 n;
    n = (i64)((*(pc_decls_pcptr + (i64)1)));
    pc_pcfns_pc_makedict(n,pc_decls_sptr,((pc_decls_sptr + (n * (i64)2)) - (i64)1));
    pc_decls_sptr += ((n * (i64)2) - (i64)1);
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_pushdot(void) {
    i64 n;
    i64 fieldtype;
    byte *  xptr;
    i64 dx;
    i64 ix;
    struct pc_decls_genfielddatarec *  gd;
    struct pc_decls_genfielddatarec *  gd0;
    struct pc_decls_varrec v;
    i64 av_1;
    v = (*pc_decls_sptr);
    if (((i64)(v.tag)==(i64)25)) {
        v.tagx = (u64)(v.uref.elemtag);
    } else {
        if (!(!!((u64)(v.hasref)))) {
            pc_support_pcerror((byte*)"pushdot/not record");
        };
    };
    xptr = (byte *)((*v.objptr).urec.vptr);
    gd0 = (gd = &pc_decls_genfielddata[((ix = (i64)(pc_decls_genfieldnames[((i64)((*(pc_decls_pcptr + (i64)1))))-1].dataindex)))-1]);
    n = (i64)(pc_decls_genfieldnames[((i64)((*(pc_decls_pcptr + (i64)1))))-1].datalength);
    av_1 = n;
    while (av_1-- > 0) {
L769 :;
        if (((i64)((*gd).recordtype) == (i64)((u64)(v.tag)))) {
            fieldtype = (i64)((*gd).fieldtype);
            if ((fieldtype == (i64)35)) {
                (*pc_decls_sptr) = (*(struct pc_decls_varrec *)((xptr + (i64)((*gd).offset))));
                if (!!((u64)((*pc_decls_sptr).hasref))) {
                    ++(*(*pc_decls_sptr).objptr).refcount;
                };
                if (!!((u64)(v.hasref))) {
                    pc_pcfns_pc_unshare(&v);
                };
                return (void *)((pc_decls_pcptr + (i64)2));
            } else if ((fieldtype == (i64)18)) {
                dx = (gd - gd0);
                (*pc_decls_sptr).tagx = (u64)((i64)18);
                (*pc_decls_sptr).refptr = (byte *)(pc_decls_genfieldpcaddress[((ix + (gd - gd0)))-1]);
                if (!!((u64)(v.hasref))) {
                    pc_pcfns_pc_unshare(&v);
                };
                return (void *)((pc_decls_pcptr + (i64)2));
            } else {
                pc_pcfns_pc_loadpacked((void *)((xptr + (i64)((*gd).offset))),fieldtype,pc_decls_sptr,(struct pc_decls_objrec *)(0));
                if (!!((u64)(v.hasref))) {
                    pc_pcfns_pc_unshare(&v);
                };
                return (void *)((pc_decls_pcptr + (i64)2));
            };
        };
        ++gd;
L770 :;
    }L771 :;
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"Field:",NULL);
    msysnewc_m_print_i64(pc_decls_ngenfieldnames,NULL);
    msysnewc_m_print_str(pc_decls_genfieldnames[((i64)((*(pc_decls_pcptr + (i64)1))))-1].name,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    pc_support_pcustypet((byte*)"Dotg: wrong record type",(i64)(v.tag));
    return (void *)(pc_decls_pcptr);
}

void * pc_khandlers_k_pushdotref(void) {
    struct pc_decls_varrec v;
    struct pc_decls_varrec *  p;
    i64 n;
    i64 fieldtype;
    i64 isrefvar;
    i64 rectype;
    i64 offset;
    struct pc_decls_genfielddatarec *  gd;
    i64 av_1;
    v = (*pc_decls_sptr);
    if (((i64)(v.tag)==(i64)22)) {
        p = v.varptr;
        isrefvar = (i64)1;
        rectype = (i64)((*p).tag);
        if ((rectype == (i64)25)) {
            rectype = (i64)((*p).uref.elemtag);
        };
        if (!(!!((u64)((*(*p).objptr).urec.mutable)))) {
            (*p).objptr = pc_objlib_copyonwrite((*p).objptr,(i64)((*p).tag));
        };
    }else if (((i64)(v.tag)==(i64)23)) {
        isrefvar = (i64)0;
        rectype = (i64)(v.uref.elemtag);
    } else {
        pc_support_pcustype((byte*)"&dotg not ref",&v);
        return (void *)(pc_decls_pcptr);
    };
    gd = &pc_decls_genfielddata[((i64)(pc_decls_genfieldnames[((i64)((*(pc_decls_pcptr + (i64)1))))-1].dataindex))-1];
    n = (i64)(pc_decls_genfieldnames[((i64)((*(pc_decls_pcptr + (i64)1))))-1].datalength);
    av_1 = n;
    while (av_1-- > 0) {
L772 :;
        if (((i64)((*gd).recordtype) == rectype)) {
            fieldtype = (i64)((*gd).fieldtype);
            offset = (i64)((*gd).offset);
            if (!!(isrefvar)) {
                if ((fieldtype == (i64)35)) {
                    (*pc_decls_sptr).refptr = ((byte *)((*(*p).objptr).urec.vptr) + offset);
                } else {
                    (*pc_decls_sptr).tagx = (u64)((i64)23);
                    (*pc_decls_sptr).refptr = ((*(*p).objptr).uarray.ptr + offset);
                    (*pc_decls_sptr).uref.elemtag = (u64)(fieldtype);
                };
            } else {
                (*pc_decls_sptr).uref.ptr += offset;
                (*pc_decls_sptr).uref.elemtag = (u64)(fieldtype);
            };
            return (void *)((pc_decls_pcptr + (i64)2));
        };
        ++gd;
L773 :;
    }L774 :;
    ;
    pc_support_pcustypet((byte*)"&Dotg: wrong record type",rectype);
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_softconv(void) {
    i64 t;
    t = (i64)((*(pc_decls_pcptr + (i64)1)));
    (*pc_decls_sptr).tagx = (u64)(t);
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_hardconv(void) {
    i64 s;
    i64 t;
    s = (i64)((*pc_decls_sptr).tag);
    t = (i64)((*(pc_decls_pcptr + (i64)1)));
    if (((i64)((u64)((*pc_decls_sptr).tag)) != t)) {
        pc_pcfns_pc_iconvert(t,pc_decls_sptr);
    };
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_mixed(void) {
    pc_support_pclunimpl((i64)86);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_incrptr(void) {
    struct pc_decls_varrec *  p;
    p = pc_decls_sptr++;
    switch ((i64)(pc_decls_ttbasetype[((i64)((*p).tag))])) {
    case 22:;
    {
        p = (*p).varptr;
        switch ((i64)((*p).tag)) {
        case 1:;
        {
            ++(*p).value;
        }break;
        case 22:;
        {
            ++(*p).varptr;
        }break;
        case 23:;
        {
            (*p).uref.ptr += pc_decls_ttsize[((i64)((*p).uref.elemtag))];
        }break;
        default: {
            pc_support_pcustype((byte*)"incrptr/refvar",p);
        }
        } //SW
;
    }break;
    case 23:;
    {
        switch ((i64)((*p).uref.elemtag)) {
        case 44:;
        case 37:;
        {
            ++(*(*p).uref.ptr);
        }break;
        default: {
            pc_support_pcustypet((byte*)"incrptr/ref",(i64)((*p).uref.elemtag));
        }
        } //SW
;
    }break;
    default: {
        pc_support_pcustype((byte*)"incrptr",p);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_incrto_m(void) {
    struct pc_decls_varrec *  a;
    a = (struct pc_decls_varrec *)((*(pc_decls_pcptr + (i64)1)));
    switch ((i64)(pc_decls_ttbasetype[((i64)((*a).tag))])) {
    case 1:;
    {
        ++(*a).value;
    }break;
    case 22:;
    {
        ++(*a).varptr;
    }break;
    case 23:;
    {
        (*a).uref.ptr += pc_decls_ttsize[((i64)((*a).uref.elemtag))];
    }break;
    default: {
        pc_support_pcustype((byte*)"INCRTO_M",a);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_incrto_f(void) {
    struct pc_decls_varrec *  a;
    i64 offset;
    a = (struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)1)))));
    switch ((i64)((*a).tag)) {
    case 1:;
    {
        ++(*a).value;
        return (void *)((pc_decls_pcptr + (i64)2));
    }break;
    case 22:;
    {
        ++(*a).varptr;
    }break;
    case 23:;
    {
        (*a).uref.ptr += pc_decls_ttsize[((i64)((*a).uref.elemtag))];
    }break;
    case 24:;
    {
        if (!!((u64)((*a).uref.bitlength))) {
            pc_support_pcerror((byte*)"INCR/BITFIELD");
        };
        offset = ((i64)((u64)((*a).uref.bitoffset)) + pc_types_stdtypewidths[((i64)((*a).uref.elemtag))]);
        if ((offset >= (i64)8)) {
            offset = (i64)0;
            ++(*a).uref.ptr;
        };
        (*a).uref.bitoffset = (u64)(offset);
    }break;
    default: {
        pc_support_pcustype((byte*)"INCRTO_F",a);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_loadincr(void) {
    struct pc_decls_varrec ptr;
    ptr = (*pc_decls_sptr);
    pc_pcfns_pc_loadptr(pc_decls_sptr,pc_decls_sptr);
    --pc_decls_sptr;
    (*pc_decls_sptr) = ptr;
    return pc_khandlers_k_incrptr();
}

void * pc_khandlers_k_incrload(void) {
    i64 *  pc;
    struct pc_decls_varrec ptr;
    ptr = (*pc_decls_sptr);
    pc = (i64 *)(pc_khandlers_k_incrptr());
    pc_pcfns_pc_loadptr(&ptr,--pc_decls_sptr);
    return (void *)(pc);
}

void * pc_khandlers_k_decrptr(void) {
    struct pc_decls_varrec *  p;
    p = pc_decls_sptr++;
    switch ((i64)(pc_decls_ttbasetype[((i64)((*p).tag))])) {
    case 22:;
    {
        p = (*p).varptr;
        switch ((i64)((*p).tag)) {
        case 1:;
        {
            --(*p).value;
        }break;
        case 22:;
        {
            --(*p).varptr;
        }break;
        case 23:;
        {
            (*p).uref.ptr -= pc_decls_ttsize[((i64)((*p).uref.elemtag))];
        }break;
        default: {
            pc_support_pcustype((byte*)"decrptr/refvar",p);
        }
        } //SW
;
    }break;
    case 23:;
    {
        switch ((i64)((*p).uref.elemtag)) {
        case 39:;
        {
            --(*(*p).uref.ptr64);
        }break;
        case 44:;
        case 37:;
        {
            --(*(*p).uref.ptr);
        }break;
        default: {
            pc_support_pcustypet((byte*)"decrptr/ref",(i64)((*p).uref.elemtag));
        }
        } //SW
;
    }break;
    default: {
        pc_support_pcustype((byte*)"decrptr",p);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_decrto_m(void) {
    struct pc_decls_varrec *  a;
    a = (struct pc_decls_varrec *)((*(pc_decls_pcptr + (i64)1)));
    switch ((i64)((*a).tag)) {
    case 1:;
    {
        --(*a).value;
    }break;
    case 22:;
    {
        --(*a).varptr;
    }break;
    case 23:;
    {
        (*a).uref.ptr -= pc_decls_ttsize[((i64)((*a).uref.elemtag))];
    }break;
    default: {
        pc_support_pcustype((byte*)"DECRTO_M",a);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_decrto_f(void) {
    struct pc_decls_varrec *  a;
    a = (struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)1)))));
    switch ((i64)((*a).tag)) {
    case 1:;
    {
        --(*a).value;
    }break;
    case 22:;
    {
        --(*a).varptr;
    }break;
    case 23:;
    {
        (*a).uref.ptr -= pc_decls_ttsize[((i64)((*a).uref.elemtag))];
    }break;
    default: {
        pc_support_pcustype((byte*)"DECRTO_F",a);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_loaddecr(void) {
    struct pc_decls_varrec ptr;
    ptr = (*pc_decls_sptr);
    pc_pcfns_pc_loadptr(pc_decls_sptr,pc_decls_sptr);
    (*--pc_decls_sptr) = ptr;
    return pc_khandlers_k_decrptr();
}

void * pc_khandlers_k_decrload(void) {
    i64 *  pc;
    struct pc_decls_varrec ptr;
    ptr = (*pc_decls_sptr);
    pc = (i64 *)(pc_khandlers_k_decrptr());
    pc_pcfns_pc_loadptr(&ptr,--pc_decls_sptr);
    return (void *)(pc);
}

void * pc_khandlers_k_incr(void) {
    pc_support_pclunimpl((i64)97);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_decr(void) {
    if (((i64)((*pc_decls_sptr).tag)==(i64)1)) {
        --(*pc_decls_sptr).value;
    } else {
        pc_support_pcustype((byte*)"decr",pc_decls_sptr);
    };
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_neg(void) {
    return (void *)(((*pc_decls_neg_table[((i64)((*pc_decls_sptr).tag))]))());
}

void * pc_khandlers_k_abs(void) {
    return (void *)(((*pc_decls_abs_table[((i64)((*pc_decls_sptr).tag))]))());
}

void * pc_khandlers_k_not(void) {
    (*pc_decls_sptr).value = !((*pc_decls_sptr).value);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_inot(void) {
    return (void *)(((*pc_decls_inot_table[((i64)((*pc_decls_sptr).tag))]))());
}

void * pc_khandlers_k_istrue(void) {
    return (void *)(((*pc_decls_istrue_table[((i64)((*pc_decls_sptr).tag))]))());
}

void * pc_khandlers_k_asc(void) {
    i64 a;
    struct pc_decls_objrec *  s;
    switch ((i64)((*pc_decls_sptr).tag)) {
    case 5:;
    {
        s = (*pc_decls_sptr).objptr;
        if (((i64)((*s).ustr.length) == (i64)0)) {
            a = (i64)0;
        } else {
            a = (i64)((*(*s).ustr.strptr));
            if (!!((u64)((*pc_decls_sptr).hasref))) {
                pc_pcfns_pc_unshare(pc_decls_sptr);
            };
        };
        (*pc_decls_sptr).tagx = (u64)((i64)1);
        (*pc_decls_sptr).value = a;
    }break;
    default: {
        pc_support_pcustype((byte*)"ASC",pc_decls_sptr);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_chr(void) {
    switch ((i64)((*pc_decls_sptr).tag)) {
    case 1:;
    {
        if (((*pc_decls_sptr).uvalue > (u64)255u)) {
            pc_support_pcerror((byte*)"chr>255");
        };
        pc_pcfns_pc_makechar((*pc_decls_sptr).value,pc_decls_sptr);
    }break;
    default: {
        pc_support_pcustype((byte*)"CHR",pc_decls_sptr);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_sqrt(void) {
    pc_decls_pcptr += (i64)1;
    switch ((i64)((*pc_decls_sptr).tag)) {
    case 1:;
    {
        (*pc_decls_sptr).tagx = (u64)((i64)3);
        (*pc_decls_sptr).xvalue = sqrt(((double)((*pc_decls_sptr).value)));
    }break;
    case 3:;
    {
        (*pc_decls_sptr).xvalue = sqrt(((*pc_decls_sptr).xvalue));
    }break;
    default: {
        pc_support_pcustype((byte*)"SQRT",pc_decls_sptr);
    }
    } //SW
;
    return (void *)(pc_decls_pcptr);
}

void * pc_khandlers_k_sqr(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec result;
    x = pc_decls_sptr;
    switch ((i64)((*x).tag)) {
    case 1:;
    {
        (*pc_decls_sptr).value = ((*x).value * (*x).value);
    }break;
    case 3:;
    {
        (*pc_decls_sptr).xvalue = ((*x).xvalue * (*x).xvalue);
    }break;
    case 7:;
    {
        pc_bignum_bx_mul(x,x,&result);
        pc_pcfns_pc_unshare(x);
        (*pc_decls_sptr) = result;
    }break;
    default: {
        pc_support_pcustype((byte*)"SQR",x);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_cube(void) {
    pc_support_pclunimpl((i64)108);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_sin(void) {
    struct pc_decls_varrec *  x;
    x = pc_decls_sptr;
    switch ((i64)((*x).tag)) {
    case 3:;
    {
        (*pc_decls_sptr).xvalue = sin((*x).xvalue);
    }break;
    default: {
        pc_support_pcustype((byte*)"SIN",x);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_cos(void) {
    struct pc_decls_varrec *  x;
    x = pc_decls_sptr;
    switch ((i64)((*x).tag)) {
    case 3:;
    {
        (*pc_decls_sptr).xvalue = cos((*x).xvalue);
    }break;
    default: {
        pc_support_pcustype((byte*)"COS",x);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_tan(void) {
    pc_support_pclunimpl((i64)111);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_asin(void) {
    pc_support_pclunimpl((i64)112);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_acos(void) {
    pc_support_pclunimpl((i64)113);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_atan(void) {
    struct pc_decls_varrec *  x;
    x = pc_decls_sptr;
    switch ((i64)((*x).tag)) {
    case 1:;
    {
        (*pc_decls_sptr).xvalue = atan((double)((*x).value));
        (*pc_decls_sptr).tagx = (u64)((i64)3);
    }break;
    case 3:;
    {
        (*pc_decls_sptr).xvalue = atan((*x).xvalue);
    }break;
    default: {
        pc_support_pcustype((byte*)"ATAN",x);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_sign(void) {
    pc_support_pclunimpl((i64)115);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_ln(void) {
    switch ((i64)((*pc_decls_sptr).tag)) {
    case 3:;
    {
        (*pc_decls_sptr).xvalue = log((*pc_decls_sptr).xvalue);
    }break;
    case 1:;
    {
        (*pc_decls_sptr).xvalue = log((double)((*pc_decls_sptr).value));
        (*pc_decls_sptr).tag = (u64)((i64)3);
    }break;
    default: {
        pc_support_pcustype((byte*)"LN",pc_decls_sptr);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_log(void) {
    pc_support_pclunimpl((i64)117);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_lg(void) {
    pc_support_pclunimpl((i64)118);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_exp(void) {
    switch ((i64)((*pc_decls_sptr).tag)) {
    case 3:;
    {
        (*pc_decls_sptr).xvalue = exp((*pc_decls_sptr).xvalue);
    }break;
    case 1:;
    {
        (*pc_decls_sptr).xvalue = exp((double)((*pc_decls_sptr).value));
        (*pc_decls_sptr).tag = (u64)((i64)3);
    }break;
    default: {
        pc_support_pcustype((byte*)"EXP",pc_decls_sptr);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_round(void) {
    struct pc_decls_varrec *  x;
    x = pc_decls_sptr;
    switch ((i64)((*x).tag)) {
    case 3:;
    {
        if (((*x).xvalue >= (double)0.)) {
            (*pc_decls_sptr).xvalue = floor(((*x).xvalue + (double)0.5));
        } else {
            (*pc_decls_sptr).xvalue = ceil(((*x).xvalue - (double)0.5));
        };
    }break;
    case 1:;
    {
    }break;
    default: {
        pc_support_pcustype((byte*)"ROUND",x);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_floor(void) {
    struct pc_decls_varrec *  x;
    x = pc_decls_sptr;
    switch ((i64)((*x).tag)) {
    case 3:;
    {
        if (((*x).xvalue >= (double)0.)) {
            (*pc_decls_sptr).xvalue = floor((*x).xvalue);
        } else {
            (*pc_decls_sptr).xvalue = ceil((*x).xvalue);
        };
    }break;
    default: {
        pc_support_pcustype((byte*)"ROUND",x);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_ceil(void) {
    pc_support_pclunimpl((i64)122);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_fract(void) {
    pc_support_pclunimpl((i64)123);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_negto(void) {
    pc_support_pclunimpl((i64)124);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_absto(void) {
    pc_support_pclunimpl((i64)125);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_notto(void) {
    pc_support_pclunimpl((i64)126);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_inotto(void) {
    pc_support_pclunimpl((i64)127);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_len(void) {
    return (void *)(((*pc_decls_len_table[((i64)((*pc_decls_sptr).tag))]))());
}

void * pc_khandlers_k_lwb(void) {
    return (void *)(((*pc_decls_lwb_table[((i64)((*pc_decls_sptr).tag))]))());
}

void * pc_khandlers_k_upb(void) {
    return (void *)(((*pc_decls_upb_table[((i64)((*pc_decls_sptr).tag))]))());
}

void * pc_khandlers_k_bounds(void) {
    return (void *)(((*pc_decls_bounds_table[((i64)((*pc_decls_sptr).tag))]))());
}

void * pc_khandlers_k_bits(void) {
    if (((i64)((*pc_decls_sptr).tag)==(i64)13)) {
        (*pc_decls_sptr).value = (i64)(pc_decls_ttbitwidth[((*pc_decls_sptr).value)]);
    } else {
        (*pc_decls_sptr).value = (i64)(pc_decls_ttbitwidth[((i64)((*pc_decls_sptr).tag))]);
    };
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_bytes(void) {
    i64 m;
    i64 n;
    struct pc_decls_objrec *  p;
    m = (i64)((*pc_decls_sptr).tag);
    if ((m == (i64)13)) {
        m = (*pc_decls_sptr).value;
    };
    p = (*pc_decls_sptr).objptr;
    if (((i64)(pc_decls_ttbasetype[(m)])==(i64)5)) {
        n = (i64)((*p).ustr.length);
    }else if (((i64)(pc_decls_ttbasetype[(m)])==(i64)30)) {
        n = ((i64)((u64)((*p).uarray.length)) * pc_decls_ttsize[((i64)((*p).uarray.elemtag))]);
    }else if (((i64)(pc_decls_ttbasetype[(m)])==(i64)9)) {
        n = ((i64)((u64)((*p).uset.length)) / (i64)8);
    }else if (((i64)(pc_decls_ttbasetype[(m)])==(i64)31)) {
        if (((i64)((*p).ubits.elemtag)==(i64)41)) {
            n = ((i64)((u64)((*p).ubits.length)) / (i64)8);
        }else if (((i64)((*p).ubits.elemtag)==(i64)42)) {
            n = ((i64)((u64)((*p).ubits.length)) / (i64)4);
        }else if (((i64)((*p).ubits.elemtag)==(i64)43)) {
            n = ((i64)((u64)((*p).ubits.length)) / (i64)2);
        };
    }else if (((i64)(pc_decls_ttbasetype[(m)])==(i64)29)) {
        n = ((i64)((u64)((*p).ulist.length)) * (i64)16);
    } else {
        n = pc_decls_ttsize[(m)];
    };
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        pc_pcfns_pc_unshare(pc_decls_sptr);
    };
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = n;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_type(void) {
    i64 res;
    res = (i64)((*pc_decls_sptr).tag);
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        pc_pcfns_pc_unshare(pc_decls_sptr);
    };
    (*pc_decls_sptr).tagx = (u64)((i64)13);
    (*pc_decls_sptr).value = res;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_elemtype(void) {
    i64 res;
    if (((i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))])==(i64)30)) {
        res = (i64)((*(*pc_decls_sptr).objptr).uarray.elemtag);
        pc_pcfns_pc_unshare(pc_decls_sptr);
    }else if (((i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))])==(i64)31)) {
        res = (i64)((*(*pc_decls_sptr).objptr).ubits.elemtag);
        pc_pcfns_pc_unshare(pc_decls_sptr);
    }else if (((i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))])==(i64)23) || ((i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))])==(i64)24)) {
        res = (i64)((*pc_decls_sptr).uref.elemtag);
    } else {
        pc_support_pcerror((byte*)"elemtype");
    };
    (*pc_decls_sptr).tagx = (u64)((i64)13);
    (*pc_decls_sptr).value = res;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_basetype(void) {
    i64 res;
    if (((i64)((u64)((*pc_decls_sptr).tag)) == (i64)13)) {
        res = (i64)(pc_decls_ttbasetype[((*pc_decls_sptr).value)]);
    } else {
        res = (i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))]);
    };
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        pc_pcfns_pc_unshare(pc_decls_sptr);
    };
    (*pc_decls_sptr).tagx = (u64)((i64)13);
    (*pc_decls_sptr).value = res;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_minval(void) {
    i64 t;
    i64 a;
    switch ((i64)((*pc_decls_sptr).tag)) {
    case 1:;
    {
        t = (i64)40;
    }break;
    case 3:;
    {
        t = (i64)49;
    }break;
    case 13:;
    {
        t = (*pc_decls_sptr).value;
    }break;
    case 7:;
    {
        t = (i64)7;
    }break;
    default: {
        pc_support_pcustype((byte*)"Maxval",pc_decls_sptr);
    }
    } //SW
;
    if ((t==(i64)44) || (t==(i64)45) || (t==(i64)46) || (t==(i64)47)) {
        a = (i64)0;
    }else if ((t==(i64)37)) {
        a = (i64)-128;
    }else if ((t==(i64)38)) {
        a = (i64)-32768;
    }else if ((t==(i64)39)) {
        a = (i64)-2147483648;
    }else if ((t==(i64)40)) {
        a = (i64)((u64)9223372036854775808u);
    }else if ((t==(i64)7)) {
        a = (i64)((u64)9223372036854775808u);
    } else {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str(pc_support_gettypename(t),NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcerror((byte*)"MINVALUE");
    };
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = a;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_maxval(void) {
    i64 t;
    i64 a;
    switch ((i64)((*pc_decls_sptr).tag)) {
    case 1:;
    {
        t = (i64)40;
    }break;
    case 3:;
    {
        t = (i64)49;
    }break;
    case 13:;
    {
        t = (*pc_decls_sptr).value;
    }break;
    default: {
        pc_support_pcustype((byte*)"Maxval",pc_decls_sptr);
    }
    } //SW
;
    if ((t==(i64)44)) {
        a = (i64)255;
    }else if ((t==(i64)45)) {
        a = (i64)65536;
    }else if ((t==(i64)46)) {
        a = (i64)4294967295;
    }else if ((t==(i64)47)) {
        a = (i64)((u64)18446744073709551615u);
    }else if ((t==(i64)37)) {
        a = (i64)127;
    }else if ((t==(i64)38)) {
        a = (i64)32767;
    }else if ((t==(i64)39)) {
        a = (i64)2147483647;
    }else if ((t==(i64)40)) {
        a = (i64)((u64)9223372036854775807u);
    } else {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str(pc_support_gettypename(t),NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcerror((byte*)"MAXVALUE");
    };
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = a;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_isint(void) {
    if ((((i64)((u64)((*pc_decls_sptr).tag)) == (i64)1) || ((i64)((u64)((*pc_decls_sptr).tag)) == (i64)2))) {
        (*pc_decls_sptr).value = (i64)1;
    } else {
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            pc_pcfns_pc_unshare(pc_decls_sptr);
        };
        (*pc_decls_sptr).value = (i64)0;
    };
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_isreal(void) {
    if (((i64)((u64)((*pc_decls_sptr).tag)) == (i64)3)) {
        (*pc_decls_sptr).value = (i64)1;
    } else {
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            pc_pcfns_pc_unshare(pc_decls_sptr);
        };
        (*pc_decls_sptr).value = (i64)0;
    };
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_isstring(void) {
    i64 n;
    n = ((i64)((u64)((*pc_decls_sptr).tag)) == (i64)5);
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        pc_pcfns_pc_unshare(pc_decls_sptr);
    };
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = n;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_isrange(void) {
    i64 n;
    n = ((i64)((u64)((*pc_decls_sptr).tag)) == (i64)4);
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        pc_pcfns_pc_unshare(pc_decls_sptr);
    };
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = n;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_isnumber(void) {
    pc_support_pclunimpl((i64)143);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_isarray(void) {
    i64 n;
    switch ((i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))])) {
    case 29:;
    case 30:;
    case 31:;
    {
        n = (i64)1;
    }break;
    default: {
        n = (i64)0;
    }
    } //SW
;
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        pc_pcfns_pc_unshare(pc_decls_sptr);
    };
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = n;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_isrecord(void) {
    i64 n;
    n = (i64)0;
    switch ((i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))])) {
    case 32:;
    case 33:;
    {
        n = (i64)1;
    }break;
    default: {
    }
    } //SW
;
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        pc_pcfns_pc_unshare(pc_decls_sptr);
    };
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = n;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_ispointer(void) {
    i64 n;
    switch ((i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))])) {
    case 23:;
    case 22:;
    case 24:;
    case 18:;
    case 20:;
    {
        n = (i64)1;
    }break;
    default: {
        n = (i64)0;
    }
    } //SW
;
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = n;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_ismutable(void) {
    pc_support_pclunimpl((i64)147);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_isset(void) {
    i64 n;
    n = ((i64)(pc_decls_ttbasetype[((i64)((*pc_decls_sptr).tag))]) == (i64)9);
    if (!!((u64)((*pc_decls_sptr).hasref))) {
        pc_pcfns_pc_unshare(pc_decls_sptr);
    };
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = n;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_isvoid(void) {
    if (((i64)((u64)((*pc_decls_sptr).tag)) == (i64)0)) {
        (*pc_decls_sptr).tagx = (u64)((i64)1);
        (*pc_decls_sptr).value = (i64)1;
    } else {
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            pc_pcfns_pc_unshare(pc_decls_sptr);
        };
        (*pc_decls_sptr).tagx = (u64)((i64)1);
        (*pc_decls_sptr).value = (i64)0;
    };
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_isdef(void) {
    if (((i64)((u64)((*pc_decls_sptr).tag)) != (i64)0)) {
        (*pc_decls_sptr).tagx = (u64)((i64)1);
        (*pc_decls_sptr).value = (i64)1;
    } else {
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            pc_pcfns_pc_unshare(pc_decls_sptr);
        };
        (*pc_decls_sptr).tagx = (u64)((i64)1);
        (*pc_decls_sptr).value = (i64)0;
    };
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_tostr(void) {
    pc_support_pclunimpl((i64)151);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_isequal(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 xt;
    i64 yt;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    xt = (i64)((*x).tag);
    yt = (i64)((*y).tag);
    if ((xt == (i64)25)) {
        xt = (i64)((*x).uref.elemtag);
        (*x).hasref = (u64)((i64)1);
    };
    if ((yt == (i64)25)) {
        yt = (i64)((*y).uref.elemtag);
    };
    if (((xt == yt) && !!((u64)((*x).hasref)))) {
        (*pc_decls_sptr).tagx = (u64)((i64)1);
        (*pc_decls_sptr).value = ((*x).objptr == (*y).objptr);
        return (void *)((pc_decls_pcptr + (i64)1));
    };
    if ((((((xt == (i64)1) && ((*x).value == (i64)0)) && !!((u64)((*y).hasref))) || (((yt == (i64)1) && ((*y).value == (i64)0)) && !!((u64)((*x).hasref)))) || ((((xt == (i64)1) && (yt == (i64)1)) && ((*x).value == (i64)0)) && !!((*y).value)))) {
        (*pc_decls_sptr).tagx = (u64)((i64)1);
        (*pc_decls_sptr).value = (i64)0;
        return (void *)((pc_decls_pcptr + (i64)1));
    };
    pc_support_pcmxtypes((byte*)"ISEQUAL",x,y);
    return 0;
}

void * pc_khandlers_k_add(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        switch (yt) {
        case 1:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).value += (*y).value;
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        case 3:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).xvalue += (*y).xvalue;
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        default: {
            return (void *)(((*pc_decls_add_table[(yt)]))());
        }
        } //SW
;
    };
    pc_decls_opc_tableptr = &pc_decls_add_table;
    return (void *)(((*pc_decls_add_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_sub(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        switch (yt) {
        case 1:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).value -= (*y).value;
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        case 3:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).xvalue -= (*y).xvalue;
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        default: {
            return (void *)(((*pc_decls_sub_table[(yt)]))());
        }
        } //SW
;
    };
    pc_decls_opc_tableptr = &pc_decls_sub_table;
    return (void *)(((*pc_decls_sub_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_mul(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        switch (yt) {
        case 1:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).value *= (*y).value;
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        case 3:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).xvalue *= (*y).xvalue;
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        default: {
            return (void *)(((*pc_decls_mul_table[(yt)]))());
        }
        } //SW
;
    };
    pc_decls_opc_tableptr = &pc_decls_mul_table;
    return (void *)(((*pc_decls_mul_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_div(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        switch (yt) {
        case 1:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).xvalue = ((double)((*pc_decls_sptr).value) / (double)((*y).value));
            (*pc_decls_sptr).tag = (u64)((i64)3);
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        case 3:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).xvalue /= (*y).xvalue;
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        default: {
            return (void *)(((*pc_decls_div_table[(yt)]))());
        }
        } //SW
;
    };
    pc_decls_opc_tableptr = &pc_decls_div_table;
    return (void *)(((*pc_decls_div_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_idiv(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        switch (yt) {
        case 1:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).value = ((*pc_decls_sptr).value / (*y).value);
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        default: {
            return (void *)(((*pc_decls_idiv_table[(yt)]))());
        }
        } //SW
;
    };
    pc_decls_opc_tableptr = &pc_decls_idiv_table;
    return (void *)(((*pc_decls_idiv_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_rem(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        switch (yt) {
        case 1:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).value = ((*pc_decls_sptr).value % (*y).value);
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        default: {
            return (void *)(((*pc_decls_rem_table[(yt)]))());
        }
        } //SW
;
    };
    pc_decls_opc_tableptr = &pc_decls_rem_table;
    return (void *)(((*pc_decls_rem_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_divrem(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 d;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    d = (*x).value;
    if ((((u64)((*x).tag) == (u64)((*y).tag)) && ((i64)((u64)((*y).tag)) == (i64)1))) {
        (*x).range_lower = (d / (*y).value);
        (*x).range_upper = (d % (*y).value);
        (*x).tagx = (u64)((i64)4);
    } else {
        pc_support_pcmxtypes((byte*)"DIVREM",x,y);
    };
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_iand(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        switch (yt) {
        case 1:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).value &= (*y).value;
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        default: {
            return (void *)(((*pc_decls_iand_table[(yt)]))());
        }
        } //SW
;
    };
    pc_decls_opc_tableptr = &pc_decls_iand_table;
    return (void *)(((*pc_decls_iand_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_ior(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        switch (yt) {
        case 1:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).value |= (*y).value;
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        default: {
            return (void *)(((*pc_decls_ior_table[(yt)]))());
        }
        } //SW
;
    };
    pc_decls_opc_tableptr = &pc_decls_ior_table;
    return (void *)(((*pc_decls_ior_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_ixor(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        switch (yt) {
        case 1:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).value ^= (*y).value;
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        default: {
            return (void *)(((*pc_decls_ixor_table[(yt)]))());
        }
        } //SW
;
    };
    pc_decls_opc_tableptr = &pc_decls_ixor_table;
    return (void *)(((*pc_decls_ixor_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_shl(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        switch (yt) {
        case 1:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).value <<= (*y).value;
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        default: {
            return (void *)(((*pc_decls_shl_table[(yt)]))());
        }
        } //SW
;
    };
    pc_decls_opc_tableptr = &pc_decls_shl_table;
    return (void *)(((*pc_decls_shl_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_shr(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        switch (yt) {
        case 1:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).value >>= (*y).value;
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        default: {
            return (void *)(((*pc_decls_shr_table[(yt)]))());
        }
        } //SW
;
    };
    pc_decls_opc_tableptr = &pc_decls_shr_table;
    return (void *)(((*pc_decls_shr_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_in(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 n;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    n = pc_dxfns_dx_varinvar(x,y);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = n;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_notin(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 n;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    n = pc_dxfns_dx_varinvar(x,y);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = !(n);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_inrev(void) {
    pc_support_pclunimpl((i64)167);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_eq(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 res;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    res = pc_pcfns_pc_equal(x,y,(i64)0);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = res;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_ne(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 res;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    res = pc_pcfns_pc_equal(x,y,(i64)0);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = !(res);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_lt(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 res;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    res = pc_pcfns_pc_compare(x,y);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = (res < (i64)0);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_le(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 res;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    res = pc_pcfns_pc_compare(x,y);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = (res <= (i64)0);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_ge(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 res;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    res = pc_pcfns_pc_compare(x,y);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = (res >= (i64)0);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_gt(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 res;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    res = pc_pcfns_pc_compare(x,y);
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = (res > (i64)0);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_min(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        switch (yt) {
        case 1:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).value=((*pc_decls_sptr).value<(*y).value?(*pc_decls_sptr).value:(*y).value);
;
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        case 3:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).xvalue=((*pc_decls_sptr).xvalue<(*y).xvalue?(*pc_decls_sptr).xvalue:(*y).xvalue);
;
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        default: {
            return (void *)(((*pc_decls_min_table[(yt)]))());
        }
        } //SW
;
    };
    pc_decls_opc_tableptr = &pc_decls_min_table;
    return (void *)(((*pc_decls_min_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_max(void) {
    struct pc_decls_varrec *  y;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        switch (yt) {
        case 1:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).value=((*pc_decls_sptr).value>(*y).value?(*pc_decls_sptr).value:(*y).value);
;
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        case 3:;
        {
            y = pc_decls_sptr++;
            (*pc_decls_sptr).xvalue=((*pc_decls_sptr).xvalue>(*y).xvalue?(*pc_decls_sptr).xvalue:(*y).xvalue);
;
            return (void *)((pc_decls_pcptr + (i64)1));
        }break;
        default: {
            return (void *)(((*pc_decls_max_table[(yt)]))());
        }
        } //SW
;
    };
    pc_decls_opc_tableptr = &pc_decls_max_table;
    return (void *)(((*pc_decls_max_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][(yt)]))]))());
}

void * pc_khandlers_k_concat(void) {
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == yt)) {
        return (void *)(((*pc_decls_concat_table[(yt)]))());
    };
    pc_support_pcmxtypes((byte*)"CONCAT",pc_decls_sptr,pc_decls_sptr);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_append(void) {
    return (void *)(((*pc_decls_append_table[((i64)((*(pc_decls_sptr + (i64)1)).tag))]))());
}

void * pc_khandlers_k_power(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  y;
    i64 xt;
    i64 yt;
    struct pc_decls_varrec result;
    y = pc_decls_sptr;
    x = ++pc_decls_sptr;
    xt = (i64)((*x).tag);
    yt = (i64)((*y).tag);
    if ((xt != yt)) {
        if (((xt = pc_dxfns_dx_mixed(x,y)) == (i64)0)) {
            pc_support_pcmxtypes((byte*)"**MIXED",x,y);
        };
    };
    switch (xt) {
    case 1:;
    {
        (*pc_decls_sptr).value = pc_support_ipower((*x).value,(*y).value);
    }break;
    case 3:;
    {
        (*pc_decls_sptr).xvalue = pow((*x).xvalue,(*y).xvalue);
    }break;
    case 7:;
    {
        pc_bignum_bx_power(x,pc_bignum_bx_int(y),&result);
        pc_pcfns_pc_unshare(x);
        if (!!((u64)((*y).hasref))) {
            pc_pcfns_pc_unshare(y);
        };
        (*pc_decls_sptr) = result;
    }break;
    default: {
        pc_support_pcustype((byte*)"**",x);
    }
    } //SW
;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_atan2(void) {
    pc_support_pclunimpl((i64)179);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_addto(void) {
    struct pc_decls_varrec *  x;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == (i64)22)) {
        x = (*(pc_decls_sptr + (i64)1)).varptr;
        if (((i64)((u64)((*x).tag)) == yt)) {
            return (void *)(((*pc_decls_addto_table[(yt)]))());
        };
        pc_decls_opc_tableptr = &pc_decls_addto_table;
        return (void *)(((*pc_decls_addto_dtable[((i64)(pc_decls_sigmap[((i64)((*x).tag))][(yt)]))]))());
    } else {
        pc_support_pcerror((byte*)"addto not ptr");
    };
    pc_support_pcmxtypes((byte*)"addto",x,pc_decls_sptr);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_subto(void) {
    struct pc_decls_varrec *  x;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == (i64)22)) {
        x = (*(pc_decls_sptr + (i64)1)).varptr;
        if (((i64)((u64)((*x).tag)) == yt)) {
            return (void *)(((*pc_decls_subto_table[(yt)]))());
        };
        pc_decls_opc_tableptr = &pc_decls_subto_table;
        return (void *)(((*pc_decls_subto_dtable[((i64)(pc_decls_sigmap[((i64)((*x).tag))][(yt)]))]))());
    } else {
        pc_support_pcerror((byte*)"subto not ptr");
    };
    pc_support_pcmxtypes((byte*)"subto",x,pc_decls_sptr);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_multo(void) {
    struct pc_decls_varrec *  x;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == (i64)22)) {
        x = (*(pc_decls_sptr + (i64)1)).varptr;
        if (((i64)((u64)((*x).tag)) == yt)) {
            return (void *)(((*pc_decls_multo_table[(yt)]))());
        };
    } else {
        pc_support_pcerror((byte*)"multo not ptr");
    };
    pc_support_pcmxtypes((byte*)"multo",x,pc_decls_sptr);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_divto(void) {
    struct pc_decls_varrec *  x;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == (i64)22)) {
        x = (*(pc_decls_sptr + (i64)1)).varptr;
        if (((i64)((u64)((*x).tag)) == yt)) {
            return (void *)(((*pc_decls_divto_table[(yt)]))());
        };
    } else {
        pc_support_pcerror((byte*)"divto not ptr");
    };
    pc_support_pcmxtypes((byte*)"divto",x,pc_decls_sptr);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_idivto(void) {
    struct pc_decls_varrec *  x;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == (i64)22)) {
        x = (*(pc_decls_sptr + (i64)1)).varptr;
        if (((i64)((u64)((*x).tag)) == yt)) {
            return (void *)(((*pc_decls_idivto_table[(yt)]))());
        };
    } else {
        pc_support_pcerror((byte*)"idivto not ptr");
    };
    pc_support_pcmxtypes((byte*)"idivto",x,pc_decls_sptr);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_iandto(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  p;
    struct pc_decls_varrec ptr;
    i64 yt;
    u64 *  pc;
    yt = (i64)((*pc_decls_sptr).tag);
    p = (pc_decls_sptr + (i64)1);
    if (((i64)((u64)((*p).tag)) == (i64)22)) {
        x = (*p).varptr;
        if (((i64)((u64)((*x).tag)) == yt)) {
            return (void *)(((*pc_decls_iandto_table[(yt)]))());
        };
        pc_support_pcmxtypes((byte*)"iandto",x,pc_decls_sptr);
    } else {
        ptr = (*p);
        pc_pcfns_pc_loadptr(p,p);
        pc = (u64 *)(pc_khandlers_k_iand());
        pc_pcfns_pc_storeptr(&ptr,pc_decls_sptr++);
        return (void *)(pc);
    };
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_iorto(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  p;
    struct pc_decls_varrec ptr;
    i64 yt;
    u64 *  pc;
    yt = (i64)((*pc_decls_sptr).tag);
    p = (pc_decls_sptr + (i64)1);
    if (((i64)((u64)((*p).tag)) == (i64)22)) {
        x = (*p).varptr;
        if (((i64)((u64)((*x).tag)) == yt)) {
            return (void *)(((*pc_decls_iorto_table[(yt)]))());
        };
        pc_support_pcmxtypes((byte*)"iorto",x,pc_decls_sptr);
    } else {
        ptr = (*p);
        pc_pcfns_pc_loadptr(p,p);
        pc = (u64 *)(pc_khandlers_k_ior());
        pc_pcfns_pc_storeptr(&ptr,pc_decls_sptr++);
        return (void *)(pc);
    };
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_ixorto(void) {
    struct pc_decls_varrec *  x;
    struct pc_decls_varrec *  p;
    struct pc_decls_varrec ptr;
    i64 yt;
    u64 *  pc;
    yt = (i64)((*pc_decls_sptr).tag);
    p = (pc_decls_sptr + (i64)1);
    if (((i64)((u64)((*p).tag)) == (i64)22)) {
        x = (*p).varptr;
        if (((i64)((u64)((*x).tag)) == yt)) {
            return (void *)(((*pc_decls_ixorto_table[(yt)]))());
        };
        pc_support_pcmxtypes((byte*)"ixorto",x,pc_decls_sptr);
    } else {
        ptr = (*p);
        pc_pcfns_pc_loadptr(p,p);
        pc = (u64 *)(pc_khandlers_k_ixor());
        pc_pcfns_pc_storeptr(&ptr,pc_decls_sptr++);
        return (void *)(pc);
    };
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_shlto(void) {
    struct pc_decls_varrec *  x;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == (i64)22)) {
        x = (*(pc_decls_sptr + (i64)1)).varptr;
        if (((i64)((u64)((*x).tag)) == yt)) {
            return (void *)(((*pc_decls_shlto_table[(yt)]))());
        };
    } else {
        pc_support_pcerror((byte*)"Shlto not ptr");
    };
    pc_support_pcmxtypes((byte*)"Shlto",x,pc_decls_sptr);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_shrto(void) {
    struct pc_decls_varrec *  x;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == (i64)22)) {
        x = (*(pc_decls_sptr + (i64)1)).varptr;
        if (((i64)((u64)((*x).tag)) == yt)) {
            return (void *)(((*pc_decls_shrto_table[(yt)]))());
        };
    } else {
        pc_support_pcerror((byte*)"Shrto not ptr");
    };
    pc_support_pcmxtypes((byte*)"Shrto",x,pc_decls_sptr);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_minto(void) {
    struct pc_decls_varrec *  x;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == (i64)22)) {
        x = (*(pc_decls_sptr + (i64)1)).varptr;
        if (((i64)((u64)((*x).tag)) == yt)) {
            return (void *)(((*pc_decls_minto_table[(yt)]))());
        };
    } else {
        pc_support_pcerror((byte*)"minto not ptr");
    };
    pc_support_pcmxtypes((byte*)"minto",x,pc_decls_sptr);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_maxto(void) {
    struct pc_decls_varrec *  x;
    i64 yt;
    yt = (i64)((*pc_decls_sptr).tag);
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == (i64)22)) {
        x = (*(pc_decls_sptr + (i64)1)).varptr;
        if (((i64)((u64)((*x).tag)) == yt)) {
            return (void *)(((*pc_decls_maxto_table[(yt)]))());
        };
    } else {
        pc_support_pcerror((byte*)"maxto not ptr");
    };
    pc_support_pcmxtypes((byte*)"maxto",x,pc_decls_sptr);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_concatto(void) {
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == (i64)22)) {
        return (void *)(((*pc_decls_concatto_table[((i64)((*(*(pc_decls_sptr + (i64)1)).varptr).tag))]))());
    };
    pc_support_pcerror((byte*)"Concatto not ptr");
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_appendto(void) {
    if (((i64)((u64)((*(pc_decls_sptr + (i64)1)).tag)) == (i64)22)) {
        return (void *)(((*pc_decls_appendto_table[((i64)((*(*(pc_decls_sptr + (i64)1)).varptr).tag))]))());
    };
    pc_support_pcerror((byte*)"Appendto not ptr");
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_pushix(void) {
    return (void *)(((*pc_decls_pushix_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][((i64)((*pc_decls_sptr).tag))]))]))());
}

void * pc_khandlers_k_pushdotix(void) {
    return (void *)(((*pc_decls_pushdotix_dtable[((i64)(pc_decls_sigmap[((i64)((*(pc_decls_sptr + (i64)1)).tag))][((i64)((*pc_decls_sptr).tag))]))]))());
}

void * pc_khandlers_k_pushkeyix(void) {
    struct pc_decls_varrec *  d;
    struct pc_decls_varrec *  k;
    struct pc_decls_varrec *  p;
    d = pc_decls_sptr++;
    k = pc_decls_sptr;
    if (((i64)((u64)((*d).tag)) != (i64)10)) {
        pc_support_pcustype((byte*)"keyix",d);
    };
    p = pc_pcfns_finddictitem(d,k,(i64)0);
    pc_pcfns_pc_unshare(d);
    if (!!((u64)((*k).hasref))) {
        pc_pcfns_pc_unshare(k);
    };
    if (!!(p)) {
        (*pc_decls_sptr) = (*p);
        return (void *)((pc_decls_pcptr + (i64)1));
    };
    (*pc_decls_sptr).tagx = (u64)((i64)0);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_pushkeyixd(void) {
    struct pc_decls_varrec *  d;
    struct pc_decls_varrec *  k;
    struct pc_decls_varrec *  p;
    struct pc_decls_varrec *  def;
    def = pc_decls_sptr++;
    d = pc_decls_sptr++;
    k = pc_decls_sptr;
    if (((i64)((u64)((*d).tag)) != (i64)10)) {
        pc_support_pcustype((byte*)"keyix",d);
    };
    p = pc_pcfns_finddictitem(d,k,(i64)0);
    pc_pcfns_pc_unshare(d);
    if (!!((u64)((*k).hasref))) {
        pc_pcfns_pc_unshare(k);
    };
    if (!!(p)) {
        (*pc_decls_sptr) = (*p);
        if (!!((u64)((*def).hasref))) {
            pc_pcfns_pc_unshare(def);
        };
        return (void *)((pc_decls_pcptr + (i64)1));
    };
    (*pc_decls_sptr) = (*def);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_pushixref(void) {
    struct pc_decls_varrec *  p;
    p = (pc_decls_sptr + (i64)1);
    if (((i64)((u64)((*p).tag)) == (i64)22)) {
        return (void *)(((*pc_decls_pushixref_dtable[((i64)(pc_decls_sigmap[((i64)((*(*p).varptr).tag))][((i64)((*pc_decls_sptr).tag))]))]))());
    };
    return (void *)(pc_support_pcerror((byte*)"pushixref/not ptr"));
}

void * pc_khandlers_k_pushdotixref(void) {
    struct pc_decls_varrec *  p;
    p = (pc_decls_sptr + (i64)1);
    if (((i64)((u64)((*p).tag)) == (i64)22)) {
        return (void *)(((*pc_decls_pushdotixref_dtable[((i64)(pc_decls_sigmap[((i64)((*(*p).varptr).tag))][((i64)((*pc_decls_sptr).tag))]))]))());
    };
    pc_support_pcerror((byte*)"pushdotixref/not ptr");
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_pushkeyixref(void) {
    struct pc_decls_varrec *  d;
    struct pc_decls_varrec *  k;
    struct pc_decls_varrec *  p;
    d = (*pc_decls_sptr).varptr;
    k = ++pc_decls_sptr;
    if (((i64)((u64)((*d).tag)) != (i64)10)) {
        pc_support_pcustype((byte*)"keyixref",d);
    };
    p = pc_pcfns_finddictitem(d,k,(i64)1);
    if (!!((u64)((*k).hasref))) {
        pc_pcfns_pc_unshare(k);
    };
    (*pc_decls_sptr).tagx = (u64)((i64)22);
    (*pc_decls_sptr).varptr = p;
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_pushbyteix(void) {
    i64 a;
    u64 index;
    if (((i64)((u64)((*pc_decls_sptr).tag)) != (i64)1)) {
        pc_support_pcerror((byte*)"byteix/bad index");
    };
    index = (u64)((*pc_decls_sptr).value);
    ++pc_decls_sptr;
    if (((i64)((u64)((*pc_decls_sptr).tag)) != (i64)1)) {
        pc_support_pcerror((byte*)"byteix/not int");
    };
    a = (*pc_decls_sptr).value;
    if (((*(pc_decls_pcptr + (i64)1))==(u64)((i64)44))) {
        if (((i64)(index) >= (i64)8)) {
            if (((i64)(index) >= (i64)12)) {
                pc_support_pcerror((byte*)"byteix bounds");
            };
            a = (i64)((*pc_decls_sptr).uret.frameptr_low);
            (*pc_decls_sptr).value = ((a >> (((i64)(index) - (i64)8) * (i64)8)) & (i64)255);
        } else {
            (*pc_decls_sptr).value = ((a >> ((i64)(index) * (i64)8)) & (i64)255);
        };
    } else {
        pc_support_pcerror((byte*)"byteix/bad type");
    };
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_pushbyteixref(void) {
    u64 index;
    struct pc_decls_varrec *  p;
    if (((i64)((u64)((*pc_decls_sptr).tag)) != (i64)1)) {
        pc_support_pcerror((byte*)"&byteix/bad index");
    };
    index = (u64)((*pc_decls_sptr).value);
    ++pc_decls_sptr;
    if (((i64)((u64)((*pc_decls_sptr).tag)) != (i64)22)) {
        pc_support_pcerror((byte*)"&byteix/not ptr");
    };
    p = (*pc_decls_sptr).varptr;
    if (((i64)((u64)((*p).tag)) != (i64)1)) {
        pc_support_pcerror((byte*)"&bytix/not int");
    };
    (*pc_decls_sptr).tagx = (u64)((i64)23);
    if (((*(pc_decls_pcptr + (i64)1))==(u64)((i64)44))) {
        if (((i64)(index) >= (i64)8)) {
            if (((i64)(index) >= (i64)12)) {
                pc_support_pcerror((byte*)"&byteix bounds");
            };
            (*pc_decls_sptr).uref.ptr = (((byte *)(p) + (i64)(index)) - (i64)4);
        } else {
            (*pc_decls_sptr).uref.ptr = (((byte *)(p) + (i64)8) + (i64)(index));
        };
        (*pc_decls_sptr).uref.elemtag = (u64)((i64)44);
    } else {
        pc_support_pcerror((byte*)"&byteix/bad type");
    };
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_appendset(void) {
    pc_support_pclunimpl((i64)203);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_pushdotm(void) {
    pc_support_pclunimpl((i64)204);
    return (void *)((pc_decls_pcptr + (i64)3));
}

void * pc_khandlers_k_pushdott(void) {
    pc_support_pclunimpl((i64)205);
    return (void *)((pc_decls_pcptr + (i64)3));
}

void * pc_khandlers_k_push_ad(void) {
    (*--pc_decls_sptr).tagx = (u64)((i64)19);
    (*pc_decls_sptr).refptr = (byte *)((void *)(pc_decls_dllproctable[((i64)((*(pc_decls_pcptr + (i64)1))))-1].address));
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_push_try(void) {
    (*--pc_decls_sptr).tagx = (u64)((i64)17);
    (*pc_decls_sptr).refptr = (byte *)((*(pc_decls_pcptr + (i64)1)));
    (*pc_decls_sptr).uexcept.frameoffset = (pc_decls_frameptr - (byte *)(pc_decls_sptr));
    (*pc_decls_sptr).uexcept.exceptiontype = (*(pc_decls_pcptr + (i64)2));
    (*pc_decls_sptr).uexcept.nexceptions = (i64)((*(pc_decls_pcptr + (i64)3)));
    return (void *)((pc_decls_pcptr + (i64)4));
}

void * pc_khandlers_k_raise(void) {
    if (((i64)((u64)((*pc_decls_sptr).tag)) != (i64)1)) {
        pc_support_pcerror((byte*)"Raise: not Int on stack [not proceeding direct to RAISE]");
    };
    return (void *)(pc_misc_raiseexception((*pc_decls_sptr).value));
}

void * pc_khandlers_k_applyop(void) {
    static u64 codeseq[10];
    codeseq[((i64)1)-1] = (u64)(pq_common_cmdmap[((*pc_decls_sptr).value)]);
    if (((i64)((u64)((*pc_decls_sptr).tag)) != (i64)14)) {
        pc_support_pcerror((byte*)"Apply:no op");
    };
    if (((u64)((*pc_decls_sptr).uop.opdims) != (*(pc_decls_pcptr + (i64)1)))) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_u64((*(pc_decls_pcptr + (i64)1)),NULL);
        msysnewc_m_print_u64((*pc_decls_sptr).uop.opdims,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        pc_support_pcerror((byte*)"Apply:wrong #opnds");
    };
    ++pc_decls_sptr;
    codeseq[((i64)2)-1] = (*(pc_decls_pcptr + (i64)2));
    codeseq[((i64)3)-1] = (*(pc_decls_pcptr + (i64)3));
    return (void *)(&codeseq[((i64)1)-1]);
}

void * pc_khandlers_k_makeiter(void) {
    struct pc_decls_objrec *  p;
    p = (*pc_decls_sptr).objptr;
    (*pc_decls_sptr).uiter.itcount = (u64)(((i64)((u64)((*p).ulist.length)) + (i64)1));
    if (((i64)((*pc_decls_sptr).tag)==(i64)29)) {
        (*pc_decls_sptr).varptr = (*p).ulist.vptr;
        (*pc_decls_sptr).tagx = (u64)((i64)22);
        (*pc_decls_sptr).uiter.ittype = (u64)((i64)29);
    }else if (((i64)((*pc_decls_sptr).tag)==(i64)5)) {
        (*pc_decls_sptr).uref.ptr = (byte *)((*p).ustr.strptr);
        (*pc_decls_sptr).tagx = (u64)((i64)23);
        (*pc_decls_sptr).uiter.ittype = (u64)((i64)5);
    } else {
        pc_support_pcustype((byte*)"makeiter",pc_decls_sptr);
    };
    return (void *)((pc_decls_pcptr + (i64)2));
}

void * pc_khandlers_k_forall(void) {
    struct pc_decls_varrec *  pit;
    struct pc_decls_varrec *  ploopvar;
    struct pc_decls_varrec *  pelem;
    pit = (struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)2)))));
    ploopvar = (struct pc_decls_varrec *)((pc_decls_frameptr + (i64)((*(pc_decls_pcptr + (i64)3)))));
    if (((i64)((u64)(--(*pit).uiter.itcount)) <= (i64)0)) {
        return (void *)((pc_decls_pcptr + (i64)4));
    };
    if (!!((u64)((*ploopvar).hasref))) {
        --(*(*ploopvar).objptr).refcount;
    };
    if (((i64)((*pit).uiter.ittype)==(i64)29)) {
        pelem = (*pit).varptr;
        (*ploopvar) = (*pelem);
        if (!!((u64)((*ploopvar).hasref))) {
            ++(*(*ploopvar).objptr).refcount;
        };
        ++(*pit).varptr;
    }else if (((i64)((*pit).uiter.ittype)==(i64)5)) {
        pc_pcfns_pc_makechar((i64)((*(*pit).uref.ptr)),ploopvar);
        ++(*pit).uref.ptr;
    } else {
        pc_support_pcerror((byte*)"forall/type?");
    };
    return (void *)((u64 *)((*(pc_decls_pcptr + (i64)1))));
}

void * pc_khandlers_k_forallx(void) {
    pc_support_pclunimpl((i64)212);
    return (void *)((pc_decls_pcptr + (i64)5));
}

void * pc_khandlers_k_foreach(void) {
    pc_support_pclunimpl((i64)213);
    return (void *)((pc_decls_pcptr + (i64)4));
}

void * pc_khandlers_k_foreachx(void) {
    pc_support_pclunimpl((i64)214);
    return (void *)((pc_decls_pcptr + (i64)5));
}

void * pc_khandlers_k_expandrange(void) {
    struct pc_decls_varrec *  x;
    x = pc_decls_sptr--;
    (*pc_decls_sptr).tagx = (u64)((i64)1);
    (*pc_decls_sptr).value = (i64)((*x).range_upper);
    (*x).value = (i64)((*x).range_lower);
    (*x).tagx = (u64)((i64)1);
    return (void *)((pc_decls_pcptr + (i64)1));
}

void * pc_khandlers_k_callappl(void) {
    i64 index;
    i64 nargs;
    i64 av_1;
    index = (i64)((*(pc_decls_pcptr + (i64)1)));
    nargs = (i64)((*(pc_decls_pcptr + (i64)2)));
    pc_host_do_callapplproc(index,nargs,(pc_decls_sptr + nargs));
    av_1 = nargs;
    while (av_1-- > 0) {
L775 :;
        if (!!((u64)((*pc_decls_sptr).hasref))) {
            pc_pcfns_pc_unshare(pc_decls_sptr);
        };
        ++pc_decls_sptr;
L776 :;
    }L777 :;
    ;
    return (void *)((pc_decls_pcptr + (i64)3));
}

void pc_assemc_fixup_asm(i64 mx) {
}

i64 pc_assemc_asmavailable(void) {
    return (i64)0;
}

void pc_assemc_addcountint(void * cmd) {
}

void pc_assemc_addcountext(void) {
}

void pc_assemc_showasmcmd(void * cmd) {
}

i64 * pc_assemc_disploop_asm(void) {
    return (i64 *)(0);
}

u64 pc_assemc_getasmjump(i64 cmd) {
    return (u64)((i64)0);
}


/* ********** End of C Code ********** */
