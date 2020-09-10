/*
  M to C  Whole Program Translator
  Input:  ax.m plus imported modules
  Output: \bench\c\ax.c (this file, or renamed from that)
          File represents entire program
  Target: C 64-bit
  OS:     Windows

  Modules:
  Module 1: ax.m
  Module 2: <Built-in: msysnewc.m>
  Module 3: <Built-in: clibnewc.m>
  Module 4: <Built-in: mlib.m>
  Module 5: <Built-in: oswindows.m>
  Module 6: ./ax_tables.m
  Module 7: ./ax_decls.m
  Module 8: ./ax_lex.m
  Module 9: ./ax_parse.m
  Module 10: ./ax_lib.m
  Module 11: ./ax_genss.m
  Module 12: ./ax_objdecls.m
  Module 13: ./ax_writeexe.m
  Module 14: ./ax_disasm.m
  Module 15: ./ax_writeobj.m

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
struct ax_decls_fwdrec;
struct ax_decls_opndrec;
struct ax_decls_strec;
struct ax_decls_relocrec;
struct ax_decls_dbuffer;
struct ax_decls_modulerec;
struct ax_decls_stlistrec;
struct ax_lib_mclrec;
struct ax_objdecls_imagefileheader;
struct ax_objdecls_imagedir;
struct ax_objdecls_optionalheader;
struct ax_objdecls_imagesectionheader;
struct ax_objdecls_imagesymbol;
struct ax_objdecls_importdirrec;
struct ax_objdecls_coffrelocrec;
struct ax_objdecls_auxsectionrec;
struct ax_writeexe_sectionrec;
struct ax_writeexe_importrec;
struct ax_writeexe_dllrec;

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

struct ax_decls_fwdrec {
    struct ax_decls_fwdrec* nextfwd;
    i32 offset;
    i16 reltype;
    i16 seg;
};

struct ax_decls_opndrec {
    struct ax_decls_strec *  labeldef;
    union {
        i64 value;
        double xvalue;
        byte *  svalue;
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

struct ax_decls_strec {
    byte *  name;
    struct ax_decls_fwdrec *  fwdrefs;
    struct ax_decls_opndrec *  expr;
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
    struct ax_decls_strec* basedef;
    struct ax_decls_strec* nextdef;
    struct ax_decls_strec* nextdupl;
    i32 moduleno;
    u32 htindex;
    u32 htfirstindex;
    byte spare[48];
};

struct ax_decls_relocrec {
    struct ax_decls_relocrec* nextreloc;
    i64 reloctype;
    i64 offset;
    i64 stindex;
};

struct ax_decls_dbuffer {
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

struct ax_decls_modulerec {
    byte *  filename;
    byte *  name;
    byte *  source;
};

struct ax_decls_stlistrec {
    struct ax_decls_strec *  def;
    struct ax_decls_stlistrec* nextitem;
};

struct ax_lib_mclrec {
    struct ax_lib_mclrec* nextmcl;
    struct ax_decls_opndrec *  a;
    struct ax_decls_opndrec *  b;
    u16 opcode;
    u16 c;
    i64 lineno;
};

struct ax_objdecls_imagefileheader {
    u16 machine;
    u16 nsections;
    u32 timedatestamp;
    u32 symtaboffset;
    u32 nsymbols;
    u16 optheadersize;
    u16 characteristics;
};

struct ax_objdecls_imagedir {
    u32 virtualaddr;
    u32 size;
};

struct ax_objdecls_optionalheader {
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
    struct ax_objdecls_imagedir exporttable;
    struct ax_objdecls_imagedir importtable;
    struct ax_objdecls_imagedir resourcetable;
    struct ax_objdecls_imagedir exceptiontable;
    struct ax_objdecls_imagedir certtable;
    struct ax_objdecls_imagedir basereloctable;
    struct ax_objdecls_imagedir debug;
    struct ax_objdecls_imagedir architecture;
    struct ax_objdecls_imagedir globalptr;
    struct ax_objdecls_imagedir tlstable;
    struct ax_objdecls_imagedir loadconfigtable;
    struct ax_objdecls_imagedir boundimport;
    struct ax_objdecls_imagedir iat;
    struct ax_objdecls_imagedir delayimportdescr;
    struct ax_objdecls_imagedir clrheader;
    struct ax_objdecls_imagedir reserved;
};

struct ax_objdecls_imagesectionheader {
    byte name[8];
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

struct ax_objdecls_imagesymbol {
    union {
        byte shortname[8];
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

struct ax_objdecls_importdirrec {
    u32 implookuprva;
    u32 timedatestamp;
    u32 fwdchain;
    u32 namerva;
    u32 impaddressrva;
};

struct ax_objdecls_coffrelocrec {
    i32 virtualaddr;
    i32 stindex;
    i16 reloctype;
};

struct ax_objdecls_auxsectionrec {
    i32 length;
    i16 nrelocs;
    i16 nlines;
    i32 checksum;
    i16 sectionno;
    i32 dummy;
};

struct ax_writeexe_sectionrec {
    union {
        struct ax_decls_dbuffer *  data;
        byte *  bytedata;
    };
    byte *  name;
    i64 segtype;
    i64 rawsize;
    i64 rawoffset;
    i64 virtsize;
    i64 virtoffset;
    struct ax_decls_relocrec *  relocs;
    i64 nrelocs;
};

struct ax_writeexe_importrec {
    struct ax_decls_strec *  def;
    i64 libno;
    byte *  name;
    i64 hintnameoffset;
    i64 iatoffset;
    i64 thunkoffset;
};

struct ax_writeexe_dllrec {
    byte *  name;
    i64 nprocs;
    i64 nametableoffset;
    i64 addrtableoffset;
    i64 dllnameoffset;
    i64 dllextraoffset;
};


/* PROCDECLS */
void start(void);
static void ax_loadsourcefiles(void);
static void ax_parsemodules(void);
static void ax_fixopnd(struct ax_decls_opndrec * a);
static void ax_initlogfile(void);
static void ax_closelogfile(void);
static void ax_initall(void);
static void ax_lextest(byte * file);
static void ax_getinputoptions(void);
static void ax_do_option(i64 sw,byte * value);
static void ax_showhelp(void);
static void ax_showcaption(void);
static void ax_loaderror(byte * mess);
static void ax_loaderror_s(byte * mess,byte * s);
static void ax_addmodule(byte * name);
static void ax_addsearchlib(byte * name);
static void ax_showmodules(void);
static struct ax_decls_strec * ax_getemptyst(struct ax_decls_strec * d);
static struct ax_decls_strec * ax_findduplname(struct ax_decls_strec * d);
static void ax_adddupl(struct ax_decls_strec * d);
static void ax_scanglobals(void);
static void ax_resethashtable(void);
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
void ax_lex_lex(void);
void ax_lex_initlex(void);
static void ax_lex_readreal(byte (*s)[],i64 slen,i64 intlen,i64 exponseen);
static void ax_lex_readnumber(i64 c);
static void ax_lex_readbinary(void);
static void ax_lex_readhex(void);
void ax_lex_ps(byte * caption);
void ax_lex_printsymbol(void * dev);
static void ax_lex_clearhashtable(void);
static void ax_lex_inithashtable(void);
static void ax_lex_addreservedword(byte * name,i64 symbol,i64 subcode);
void ax_lex_printhashtable(void * devx,byte * caption);
static i64 ax_lex_lookuplex(byte * name,i64 length);
void ax_lex_initsourcefile(byte * source);
struct ax_decls_strec * ax_lex_addnamestr(byte * name);
void ax_lex_lxerror(byte * m);
static i64 ax_lex_gethashvalue(byte * s);
void ax_lex_skiptoeol(void);
static byte * ax_lex_makestring(byte * p,i64 length);
void ax_parse_readmodule(i64 moduleno);
void ax_parse_checkundefined(void);
static void ax_parse_checksymbol(i64 symbol);
static void ax_parse_readinstr(void);
static void ax_parse_readcondinstr(i64 opc);
static struct ax_decls_opndrec * ax_parse_readoperand(void);
static struct ax_decls_opndrec * ax_parse_readexpression(void);
static void ax_parse_readterm(void);
static void ax_parse_readreg(i64 * reg,i64 * regsize,i64 * scale);
static struct ax_decls_opndrec * ax_parse_readaddrmode(i64 size);
void ax_lib_initlib(void);
void ax_lib_genmc(i64 opcode,struct ax_decls_opndrec * a,struct ax_decls_opndrec * b);
void ax_lib_genmcstr(i64 opcode,byte * s);
static struct ax_decls_opndrec * ax_lib_newopnd(i64 mode);
struct ax_decls_opndrec * ax_lib_genxreg(i64 xreg);
struct ax_decls_opndrec * ax_lib_genindex(i64 areg,i64 ireg,i64 scale,struct ax_decls_opndrec * x,i64 size,i64 addrsize);
struct mlib_strbuffer * ax_lib_writemclblock(void);
void ax_lib_gencomment(byte * s);
struct ax_decls_opndrec * ax_lib_genstrimm(byte * s);
static byte * ax_lib_getsizetag(i64 size);
static void ax_lib_writemcl(i64 index,struct ax_lib_mclrec * mcl);
byte * ax_lib_strmcl(struct ax_lib_mclrec * mcl);
byte * ax_lib_stropnd(struct ax_decls_opndrec * a,i64 sizeprefix);
static byte * ax_lib_strdef(struct ax_decls_strec * def);
void ax_lib_setsegment(i64 seg);
static byte * ax_lib_getsizeprefix(i64 size,i64 enable);
static i64 ax_lib_needsizeprefix(i64 opcode,struct ax_decls_opndrec * a,struct ax_decls_opndrec * b);
struct ax_decls_opndrec * ax_lib_genimm_expr(struct ax_decls_strec * d,i64 value,i64 t,i64 size);
struct ax_decls_opndrec * ax_lib_genint(i64 x,i64 size);
struct ax_decls_opndrec * ax_lib_genlab(struct ax_decls_strec * d,i64 size);
struct ax_decls_opndrec * ax_lib_genmem(struct ax_decls_strec * d,i64 size);
struct ax_decls_opndrec * ax_lib_genreg0(i64 reg,i64 size);
byte * ax_lib_getfullname(struct ax_decls_strec * d);
byte * ax_lib_getregname(i64 reg,i64 size);
byte * ax_lib_xgetregname(i64 reg);
void ax_lib_printst(void * f);
void ax_lib_printstrec(void * f,struct ax_decls_strec * d);
void ax_lib_adddef(struct ax_decls_strec * d);
void ax_lib_addimport(struct ax_decls_strec * d);
void ax_lib_createlabel(struct ax_decls_strec * symptr,i64 symbol);
void ax_lib_createnamedconst(struct ax_decls_strec * symptr,struct ax_decls_opndrec * expr);
void ax_lib_createregalias(struct ax_decls_strec * symptr,i64 regindex,i64 regsize);
void ax_lib_createxregalias(struct ax_decls_strec * symptr,i64 regindex);
void ax_lib_gerror(byte * mess);
void ax_lib_serror(byte * mess);
void ax_lib_serror_s(byte * mess,byte * param);
static byte * ax_lib_inttostr(i64 a);
static byte * ax_lib_realtostr(double a);
struct ax_decls_dbuffer * ax_lib_buffercreate(i64 size);
static void ax_lib_bufferexpand(struct ax_decls_dbuffer * a);
void ax_lib_buffercheck(struct ax_decls_dbuffer * a,i64 n);
i64 ax_lib_bufferlength(struct ax_decls_dbuffer * a);
void * ax_lib_bufferelemptr(struct ax_decls_dbuffer * a,i64 offset);
void ax_lib_addbyte(struct ax_decls_dbuffer * a,i64 x);
void ax_lib_addword(struct ax_decls_dbuffer * a,i64 x);
void ax_lib_adddword(struct ax_decls_dbuffer * a,i64 x);
void ax_lib_addqword(struct ax_decls_dbuffer * a,i64 x);
void ax_lib_printmodulesymbols(void * f);
void ax_lib_printimportsymbols(void * f);
void ax_lib_printdupltable(void * f);
void ax_genss_genss(void);
static void ax_genss_doinstr(struct ax_lib_mclrec * m,i64 index);
static void ax_genss_genbyte(i64 x);
static void ax_genss_genword(i64 x);
static void ax_genss_gendword(i64 x);
static void ax_genss_genqword(i64 x);
static void ax_genss_genopnd(struct ax_decls_opndrec * a,i64 size);
static void ax_genss_addrelocitem(i64 reloctype,struct ax_decls_strec * d);
static i64 ax_genss_getstindex(struct ax_decls_strec * d);
static void ax_genss_genrel32(struct ax_decls_opndrec * a);
static void ax_genss_genabs32(struct ax_decls_opndrec * a);
static void ax_genss_genabs64(struct ax_decls_opndrec * a);
static i64 ax_genss_getrel32(struct ax_decls_strec * d,i64 offset);
static void ax_genss_dofwdrefs(struct ax_decls_strec * d);
static void ax_genss_genrex(void);
static i64 ax_genss_isbytesized(i64 x);
static i64 ax_genss_isdwordsized(i64 x);
static void ax_genss_do_push(struct ax_decls_opndrec * a);
static void ax_genss_do_pop(struct ax_decls_opndrec * a);
static void ax_genss_do_inc(struct ax_decls_opndrec * a,i64 code);
static void ax_genss_do_neg(struct ax_decls_opndrec * a,i64 code);
static void ax_genss_genamode(struct ax_decls_opndrec * a,i64 am);
static i64 ax_genss_makemodrm(i64 mode,i64 opc,i64 rm);
static void ax_genss_setopsize(struct ax_decls_opndrec * a);
static void ax_genss_setaddrsize(struct ax_decls_opndrec * a);
static i64 ax_genss_getdispsize(struct ax_decls_opndrec * a,i64 mand);
static i64 ax_genss_genrm(struct ax_decls_opndrec * a,i64 opc);
static void ax_genss_genrmbyte(i64 mode,i64 opc,i64 rm);
static i64 ax_genss_makeam(i64 m,i64 s,i64 d);
static void ax_genss_do_arith(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 code);
static void ax_genss_do_mov(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b);
static i64 ax_genss_getregcoder(i64 reg);
static i64 ax_genss_getregcodeb(i64 reg);
static i64 ax_genss_getregcodebx(i64 reg);
static i64 ax_genss_getregcoderx(i64 reg);
static void ax_genss_do_lea(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b);
static void ax_genss_do_movsx(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 opc);
static void ax_genss_checkhighreg(struct ax_decls_opndrec * a);
static void ax_genss_do_exch(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b);
static void ax_genss_do_movsxd(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b);
static void ax_genss_do_imul2(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b);
static void ax_genss_do_imul3(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,struct ax_decls_opndrec * c);
static void ax_genss_do_shift(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 opc);
static void ax_genss_do_test(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b);
static void ax_genss_do_loop(struct ax_decls_opndrec * a,i64 opc);
static void ax_genss_do_jcxz(struct ax_decls_opndrec * a,i64 opsize);
static void ax_genss_do_setcc(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b);
static void ax_genss_do_movxmm(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 size);
static void ax_genss_do_arithxmm(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 prefix,i64 opc);
static void ax_genss_do_logicxmm(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 opc,i64 size);
static void ax_genss_do_convertfloat(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 prefix);
static void ax_genss_do_fix(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 prefix,i64 opc);
static void ax_genss_do_float(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 prefix);
static void ax_genss_do_call(struct ax_decls_opndrec * a);
static void ax_genss_do_jmp(struct ax_decls_opndrec * a,struct ax_lib_mclrec * m);
static i64 ax_genss_getcurrdatalen(i64 id);
static void ax_genss_do_cmovcc(struct ax_decls_opndrec * c,struct ax_decls_opndrec * a,struct ax_decls_opndrec * b);
static void ax_genss_do_fmem(struct ax_decls_opndrec * a,i64 freal,i64 code);
static i64 ax_genss_getr32bits(double x);
static void ax_genss_genrel8(struct ax_decls_opndrec * a);
static i64 ax_genss_checkshortjump(struct ax_lib_mclrec * m,struct ax_decls_strec * d);
static struct ax_decls_fwdrec * ax_genss_addfwdref(struct ax_decls_fwdrec * p,i64 offset,i64 reltype,i64 seg);
static void ax_genss_switchseg(i64 newseg);
static void ax_genss_do_movdqx(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 opc);
static void ax_genss_do_popcnt(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b);
static void ax_genss_do_bsf(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 opc);
static void ax_genss_extendsymboltable(void);
static void ax_genss_do_pcmpistri(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 c,i64 opc);
void ax_writeexe_writeexe(byte * outfile);
void ax_writeexe_genexe(byte * entrypoint);
static void ax_writeexe_loadlibs(void);
struct mlib_strbuffer * ax_writeexe_writessdata(i64 fexe);
void ax_writeexe_initsectiontable(void);
static void ax_writeexe_showssdata(i64 fexe);
static void ax_writeexe_showsectiondata(struct ax_writeexe_sectionrec * d);
static void ax_writeexe_showsectioncode(struct ax_writeexe_sectionrec * p);
static void ax_writeexe_showsectionrelocs2(byte * caption,struct ax_decls_relocrec * relocs,i64 nrelocs);
static void ax_writeexe_gs_value(byte * caption,i64 value);
static void ax_writeexe_showsymboltable2(void);
static void ax_writeexe_showimporttable(void);
static i64 ax_writeexe_roundtoblock(i64 n,i64 align);
static void ax_writeexe_showsections(void);
static byte * ax_writeexe_extractlibname(byte * name,i64 * libno,i64 moduleno);
static void ax_writeexe_scanst(void);
static void ax_writeexe_relocdata(struct ax_writeexe_sectionrec * s);
static void ax_writeexe_writerecordx(void * r,i64 length);
static void ax_writeexe_writedosstub(void);
static void ax_writeexe_writepesig(void);
static void ax_writeexe_writepadding(i64 offset);
static void ax_writeexe_writefileheader(void);
static void ax_writeexe_writeoptheader(void);
static void ax_writeexe_writesectionheader(struct ax_writeexe_sectionrec * s);
static void ax_writeexe_writesectiondata(struct ax_writeexe_sectionrec * s);
static void ax_writeexe_getoffsets(void);
byte * ax_disasm_decodeinstr(byte * * cptr,byte * baseaddr);
static void ax_disasm_decodetwobyteinstr(void);
static void ax_disasm_decodeaddr(i64 w);
static i64 ax_disasm_readbyte(void);
static i64 ax_disasm_readsbyte(void);
static u64 ax_disasm_readword16(void);
static i64 ax_disasm_readint16(void);
static u64 ax_disasm_readword32(void);
static i64 ax_disasm_readint32(void);
static i64 ax_disasm_readint64(void);
static i64 ax_disasm_getreg(i64 regcode,i64 upper);
static byte * ax_disasm_strreg(i64 reg,i64 opsize);
static byte * ax_disasm_strfreg(i64 freg);
static void ax_disasm_printaddrmode(i64 xmm);
static void ax_disasm_genstr(byte * s);
static void ax_disasm_genintd(i64 a);
static void ax_disasm_genhex(i64 a);
static i64 ax_disasm_readimm(void);
static i64 ax_disasm_readimm8(void);
static byte * ax_disasm_strxmm(i64 reg);
static byte * ax_disasm_strmmx(i64 reg);
static void ax_disasm_decode8087(i64 ttt);
static void ax_disasm_do87arith(byte * opcstr,i64 ttt,i64 freg);
static void ax_disasm_do87mem(byte * opcstr,i64 mf);
static void ax_disasm_getsil(i64 * reg);
static void ax_disasm_getsilx(i64 * reg);
void ax_writeobj_writess(byte * outfile);
static void ax_writeobj_writerecord(void * r,i64 length);
static void ax_writeobj_writerelocs(struct ax_decls_relocrec * r,i64 nrelocs);
static void ax_writeobj_writedata(struct ax_decls_dbuffer * data);
static void ax_writeobj_writesymboltable(void);
static void ax_writeobj_writestringtable(void);
static struct ax_objdecls_imagesymbol * ax_writeobj_makesymbol(byte * name,i64 namelen,i64 value,i64 sectionno,i64 symtype,i64 storage,i64 naux);
static void ax_writeobj_addsymbol(struct ax_objdecls_imagesymbol * r);
static void ax_writeobj_initsymboltable(byte * filename);
static struct ax_objdecls_imagesymbol * ax_writeobj_strtoaux(byte * s);
static struct ax_objdecls_auxsectionrec * ax_writeobj_sectiontoaux(struct ax_decls_dbuffer * data,i64 nrelocs);
static i64 ax_writeobj_addstringentry(byte * s,i64 length);
static void ax_writeobj_convertsymboltable(void);
static void ax_writeobj_writecoff(byte * outfile);

/* VARS */
i64 ax_logdest = (i64)0;
static byte ax_fshowmcl;
static byte ax_fshowss;
static byte ax_fshowsx;
static byte ax_fshowtiming;
static byte *  ax_optionnames[17] = {
    (byte*)"lex",
    (byte*)"parse",
    (byte*)"gen",
    (byte*)"obj",
    (byte*)"exe",
    (byte*)"mcl",
    (byte*)"ss",
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
static i64 ax_axlevel = (i64)5;
static byte *  ax_inputfile;
static byte *  ax_outputfile;
static void *  msysnewc__fnaddresses[]= {
    &start,
    &ax_loadsourcefiles,
    &ax_parsemodules,
    &ax_fixopnd,
    &ax_initlogfile,
    &ax_closelogfile,
    &ax_initall,
    &ax_lextest,
    &ax_getinputoptions,
    &ax_do_option,
    &ax_showhelp,
    &ax_showcaption,
    &ax_loaderror,
    &ax_loaderror_s,
    &ax_addmodule,
    &ax_addsearchlib,
    &ax_showmodules,
    &ax_getemptyst,
    &ax_findduplname,
    &ax_adddupl,
    &ax_scanglobals,
    &ax_resethashtable,
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
    &ax_lex_lex,
    &ax_lex_initlex,
    &ax_lex_readreal,
    &ax_lex_readnumber,
    &ax_lex_readbinary,
    &ax_lex_readhex,
    &ax_lex_ps,
    &ax_lex_printsymbol,
    &ax_lex_clearhashtable,
    &ax_lex_inithashtable,
    &ax_lex_addreservedword,
    &ax_lex_printhashtable,
    &ax_lex_lookuplex,
    &ax_lex_initsourcefile,
    &ax_lex_addnamestr,
    &ax_lex_lxerror,
    &ax_lex_gethashvalue,
    &ax_lex_skiptoeol,
    &ax_lex_makestring,
    &ax_parse_readmodule,
    &ax_parse_checkundefined,
    &ax_parse_checksymbol,
    &ax_parse_readinstr,
    &ax_parse_readcondinstr,
    &ax_parse_readoperand,
    &ax_parse_readexpression,
    &ax_parse_readterm,
    &ax_parse_readreg,
    &ax_parse_readaddrmode,
    &ax_lib_initlib,
    &ax_lib_genmc,
    &ax_lib_genmcstr,
    &ax_lib_newopnd,
    &ax_lib_genxreg,
    &ax_lib_genindex,
    &ax_lib_writemclblock,
    &ax_lib_gencomment,
    &ax_lib_genstrimm,
    &ax_lib_getsizetag,
    &ax_lib_writemcl,
    &ax_lib_strmcl,
    &ax_lib_stropnd,
    &ax_lib_strdef,
    &ax_lib_setsegment,
    &ax_lib_getsizeprefix,
    &ax_lib_needsizeprefix,
    &ax_lib_genimm_expr,
    &ax_lib_genint,
    &ax_lib_genlab,
    &ax_lib_genmem,
    &ax_lib_genreg0,
    &ax_lib_getfullname,
    &ax_lib_getregname,
    &ax_lib_xgetregname,
    &ax_lib_printst,
    &ax_lib_printstrec,
    &ax_lib_adddef,
    &ax_lib_addimport,
    &ax_lib_createlabel,
    &ax_lib_createnamedconst,
    &ax_lib_createregalias,
    &ax_lib_createxregalias,
    &ax_lib_gerror,
    &ax_lib_serror,
    &ax_lib_serror_s,
    &ax_lib_inttostr,
    &ax_lib_realtostr,
    &ax_lib_buffercreate,
    &ax_lib_bufferexpand,
    &ax_lib_buffercheck,
    &ax_lib_bufferlength,
    &ax_lib_bufferelemptr,
    &ax_lib_addbyte,
    &ax_lib_addword,
    &ax_lib_adddword,
    &ax_lib_addqword,
    &ax_lib_printmodulesymbols,
    &ax_lib_printimportsymbols,
    &ax_lib_printdupltable,
    &ax_genss_genss,
    &ax_genss_doinstr,
    &ax_genss_genbyte,
    &ax_genss_genword,
    &ax_genss_gendword,
    &ax_genss_genqword,
    &ax_genss_genopnd,
    &ax_genss_addrelocitem,
    &ax_genss_getstindex,
    &ax_genss_genrel32,
    &ax_genss_genabs32,
    &ax_genss_genabs64,
    &ax_genss_getrel32,
    &ax_genss_dofwdrefs,
    &ax_genss_genrex,
    &ax_genss_isbytesized,
    &ax_genss_isdwordsized,
    &ax_genss_do_push,
    &ax_genss_do_pop,
    &ax_genss_do_inc,
    &ax_genss_do_neg,
    &ax_genss_genamode,
    &ax_genss_makemodrm,
    &ax_genss_setopsize,
    &ax_genss_setaddrsize,
    &ax_genss_getdispsize,
    &ax_genss_genrm,
    &ax_genss_genrmbyte,
    &ax_genss_makeam,
    &ax_genss_do_arith,
    &ax_genss_do_mov,
    &ax_genss_getregcoder,
    &ax_genss_getregcodeb,
    &ax_genss_getregcodebx,
    &ax_genss_getregcoderx,
    &ax_genss_do_lea,
    &ax_genss_do_movsx,
    &ax_genss_checkhighreg,
    &ax_genss_do_exch,
    &ax_genss_do_movsxd,
    &ax_genss_do_imul2,
    &ax_genss_do_imul3,
    &ax_genss_do_shift,
    &ax_genss_do_test,
    &ax_genss_do_loop,
    &ax_genss_do_jcxz,
    &ax_genss_do_setcc,
    &ax_genss_do_movxmm,
    &ax_genss_do_arithxmm,
    &ax_genss_do_logicxmm,
    &ax_genss_do_convertfloat,
    &ax_genss_do_fix,
    &ax_genss_do_float,
    &ax_genss_do_call,
    &ax_genss_do_jmp,
    &ax_genss_getcurrdatalen,
    &ax_genss_do_cmovcc,
    &ax_genss_do_fmem,
    &ax_genss_getr32bits,
    &ax_genss_genrel8,
    &ax_genss_checkshortjump,
    &ax_genss_addfwdref,
    &ax_genss_switchseg,
    &ax_genss_do_movdqx,
    &ax_genss_do_popcnt,
    &ax_genss_do_bsf,
    &ax_genss_extendsymboltable,
    &ax_genss_do_pcmpistri,
    &ax_writeexe_writeexe,
    &ax_writeexe_genexe,
    &ax_writeexe_loadlibs,
    &ax_writeexe_writessdata,
    &ax_writeexe_initsectiontable,
    &ax_writeexe_showssdata,
    &ax_writeexe_showsectiondata,
    &ax_writeexe_showsectioncode,
    &ax_writeexe_showsectionrelocs2,
    &ax_writeexe_gs_value,
    &ax_writeexe_showsymboltable2,
    &ax_writeexe_showimporttable,
    &ax_writeexe_roundtoblock,
    &ax_writeexe_showsections,
    &ax_writeexe_extractlibname,
    &ax_writeexe_scanst,
    &ax_writeexe_relocdata,
    &ax_writeexe_writerecordx,
    &ax_writeexe_writedosstub,
    &ax_writeexe_writepesig,
    &ax_writeexe_writepadding,
    &ax_writeexe_writefileheader,
    &ax_writeexe_writeoptheader,
    &ax_writeexe_writesectionheader,
    &ax_writeexe_writesectiondata,
    &ax_writeexe_getoffsets,
    &ax_disasm_decodeinstr,
    &ax_disasm_decodetwobyteinstr,
    &ax_disasm_decodeaddr,
    &ax_disasm_readbyte,
    &ax_disasm_readsbyte,
    &ax_disasm_readword16,
    &ax_disasm_readint16,
    &ax_disasm_readword32,
    &ax_disasm_readint32,
    &ax_disasm_readint64,
    &ax_disasm_getreg,
    &ax_disasm_strreg,
    &ax_disasm_strfreg,
    &ax_disasm_printaddrmode,
    &ax_disasm_genstr,
    &ax_disasm_genintd,
    &ax_disasm_genhex,
    &ax_disasm_readimm,
    &ax_disasm_readimm8,
    &ax_disasm_strxmm,
    &ax_disasm_strmmx,
    &ax_disasm_decode8087,
    &ax_disasm_do87arith,
    &ax_disasm_do87mem,
    &ax_disasm_getsil,
    &ax_disasm_getsilx,
    &ax_writeobj_writess,
    &ax_writeobj_writerecord,
    &ax_writeobj_writerelocs,
    &ax_writeobj_writedata,
    &ax_writeobj_writesymboltable,
    &ax_writeobj_writestringtable,
    &ax_writeobj_makesymbol,
    &ax_writeobj_addsymbol,
    &ax_writeobj_initsymboltable,
    &ax_writeobj_strtoaux,
    &ax_writeobj_sectiontoaux,
    &ax_writeobj_addstringentry,
    &ax_writeobj_convertsymboltable,
    &ax_writeobj_writecoff,
0};
static byte *  msysnewc__fnnames[]= {
    (byte*)"start",
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
    (byte*)"showmodules",
    (byte*)"getemptyst",
    (byte*)"findduplname",
    (byte*)"adddupl",
    (byte*)"scanglobals",
    (byte*)"resethashtable",
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
    (byte*)"setaddrsize",
    (byte*)"getdispsize",
    (byte*)"genrm",
    (byte*)"genrmbyte",
    (byte*)"makeam",
    (byte*)"do_arith",
    (byte*)"do_mov",
    (byte*)"getregcoder",
    (byte*)"getregcodeb",
    (byte*)"getregcodebx",
    (byte*)"getregcoderx",
    (byte*)"do_lea",
    (byte*)"do_movsx",
    (byte*)"checkhighreg",
    (byte*)"do_exch",
    (byte*)"do_movsxd",
    (byte*)"do_imul2",
    (byte*)"do_imul3",
    (byte*)"do_shift",
    (byte*)"do_test",
    (byte*)"do_loop",
    (byte*)"do_jcxz",
    (byte*)"do_setcc",
    (byte*)"do_movxmm",
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
    (byte*)"do_movdqx",
    (byte*)"do_popcnt",
    (byte*)"do_bsf",
    (byte*)"extendsymboltable",
    (byte*)"do_pcmpistri",
    (byte*)"writeexe",
    (byte*)"genexe",
    (byte*)"loadlibs",
    (byte*)"writessdata",
    (byte*)"initsectiontable",
    (byte*)"showssdata",
    (byte*)"showsectiondata",
    (byte*)"showsectioncode",
    (byte*)"showsectionrelocs2",
    (byte*)"gs_value",
    (byte*)"showsymboltable2",
    (byte*)"showimporttable",
    (byte*)"roundtoblock",
    (byte*)"showsections",
    (byte*)"extractlibname",
    (byte*)"scanst",
    (byte*)"relocdata",
    (byte*)"writerecordx",
    (byte*)"writedosstub",
    (byte*)"writepesig",
    (byte*)"writepadding",
    (byte*)"writefileheader",
    (byte*)"writeoptheader",
    (byte*)"writesectionheader",
    (byte*)"writesectiondata",
    (byte*)"getoffsets",
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
(byte*)""};
static struct msysnewc_procinforec msysnewc__fnexports[]= {
	{0, 0,0, {0,0,0, 0,0,0, 0,0,0, 0,0,0}}}
;
static i64 msysnewc__fnnprocs=435;
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
byte *  ax_tables_symbolnames[33] = {
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
    (byte*)"kdummysym"
};
byte *  ax_tables_mclnames[144] = {
    (byte*)"m_comment",
    (byte*)"m_blank",
    (byte*)"m_end",
    (byte*)"m_label",
    (byte*)"m_nop",
    (byte*)"m_param",
    (byte*)"m_assem",
    (byte*)"m_proc",
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
    (byte*)"m_and",
    (byte*)"m_or",
    (byte*)"m_xor",
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
    (byte*)"m_halt"
};
byte ax_tables_mclnopnds[144] = {
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
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
    (u8)0u
};
byte ax_tables_mclcodes[144] = {
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
    (u8)232u,
    (u8)195u,
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
    (u8)244u
};
byte *  ax_tables_regnames[21] = {
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
byte ax_tables_regcodes[21] = {
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
byte *  ax_tables_dregnames[136] = {
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
byte ax_tables_regsizes[136] = {
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
byte ax_tables_regindices[136] = {
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
byte *  ax_tables_xregnames[16] = {
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
byte *  ax_tables_fregnames[8] = {(byte*)"st0",(byte*)"st1",(byte*)"st2",(byte*)"st3",(byte*)"st4",(byte*)"st5",(byte*)"st6",(byte*)"st7"};
byte *  ax_tables_mregnames[8] = {(byte*)"mmx0",(byte*)"mmx1",(byte*)"mmx2",(byte*)"mmx3",(byte*)"mmx4",(byte*)"mmx5",(byte*)"mmx6",(byte*)"mmx7"};
byte *  ax_tables_condnames[16] = {
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
byte *  ax_tables_jmpccnames[18] = {
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
byte ax_tables_jmpcccodes[18] = {
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
byte *  ax_tables_setccnames[18] = {
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
byte ax_tables_setcccodes[18] = {
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
byte *  ax_tables_cmovccnames[18] = {
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
byte ax_tables_cmovcccodes[18] = {
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
byte *  ax_tables_prefixnames[8] = {(byte*)"byte",(byte*)"word",(byte*)"word16",(byte*)"word32",(byte*)"dword",(byte*)"word64",(byte*)"qword",(byte*)"word128"};
byte ax_tables_prefixsizes[8] = {(u8)1u,(u8)2u,(u8)2u,(u8)4u,(u8)4u,(u8)8u,(u8)8u,(u8)16u};
byte *  ax_tables_reftypenames[3] = {(byte*)"extern_ref",(byte*)"fwd_ref",(byte*)"back_ref"};
byte *  ax_tables_segmentnames[5] = {(byte*)"code_seg",(byte*)"idata_seg",(byte*)"zdata_seg",(byte*)"rodata_seg",(byte*)"impdata_seg"};
i64 ax_decls_lxfileno = (i64)0;
i64 ax_decls_lxlineno = (i64)0;
i64 ax_decls_nsourcefiles = (i64)0;
struct ax_decls_modulerec ax_decls_moduletable[200];
byte *  ax_decls_searchlibs[30];
i64 ax_decls_nmodules;
i64 ax_decls_nsearchlibs;
struct ax_decls_strec *  ax_decls_lexhashtable[65536];
struct ax_decls_strec *  ax_decls_dupltable[65536];
void *  ax_decls_logdev;
i64 ax_decls_fverbose = (i64)0;
i64 ax_decls_fquiet = (i64)0;
i64 ax_decls_linecount = (i64)0;
i64 ax_decls_nundefined = (i64)0;
i64 ax_decls_alineno = (i64)0;
i64 ax_decls_ss_zdatalen;
struct ax_decls_dbuffer *  ax_decls_ss_zdata;
struct ax_decls_dbuffer *  ax_decls_ss_idata;
struct ax_decls_dbuffer *  ax_decls_ss_code;
struct ax_decls_relocrec *  ax_decls_ss_idatarelocs;
struct ax_decls_relocrec *  ax_decls_ss_coderelocs;
i64 ax_decls_ss_nidatarelocs;
i64 ax_decls_ss_ncoderelocs;
struct ax_decls_strec * (*ax_decls_ss_symboltable)[];
i64 ax_decls_ss_nsymbols;
i64 ax_decls_ss_symboltablesize;
struct ax_decls_stlistrec *  ax_decls_globalimportlist;
struct ax_decls_strec *  ax_decls_modulenamelist;
i64 ax_decls_currmoduleno;
i64 ax_decls_nmclasm;
i64 ax_decls_nmclopndsasm;
i64 ax_lex_lxsymbol;
i64 ax_lex_lxsubcode;
i64 ax_lex_lxvalue;
double ax_lex_lxxvalue;
byte *  ax_lex_lxsvalue;
i64 ax_lex_lxlength;
static i64 ax_lex_lxhashvalue;
byte *  ax_lex_lxsptr;
static byte *  ax_lex_lxstart;
struct ax_decls_strec *  ax_lex_lxsymptr;
static byte ax_lex_alphamap[256];
static byte ax_lex_digitmap[256];
static byte ax_lex_commentmap[256];
static struct ax_decls_strec *  ax_parse_exprlabeldef;
static i64 ax_parse_exprvalue;
static i64 ax_parse_exprtype;
static struct ax_decls_fwdrec ax_lib_dummy1;
byte *  ax_lib_opndnames[7] = {(byte*)"a_none",(byte*)"a_reg",(byte*)"a_imm",(byte*)"a_mem",(byte*)"a_cond",(byte*)"a_xreg",(byte*)"a_string"};
i64 ax_lib_currsegment = (i64)0;
struct ax_decls_opndrec ax_lib_dstackopnd;
struct ax_decls_opndrec ax_lib_dframeopnd;
i64 ax_lib_labelno = (i64)0;
struct ax_decls_opndrec *  ax_lib_zero_opnd = 0;
struct ax_lib_mclrec *  ax_lib_mccode;
struct ax_lib_mclrec *  ax_lib_mccodex;
static struct mlib_strbuffer ax_lib_destv;
struct mlib_strbuffer *  ax_lib_dest = &ax_lib_destv;
struct ax_decls_opndrec *  ax_lib_regtable[20][8];
static i64 ax_genss_rex;
static i64 ax_genss_sizeoverride;
static i64 ax_genss_addroverride;
static i64 ax_genss_f2override;
static i64 ax_genss_f3override;
static struct ax_decls_opndrec *  ax_genss_extraparam;
static i64 ax_genss_currseg = (i64)0;
static struct ax_decls_dbuffer *  ax_genss_currdata;
static struct ax_decls_relocrec *  ax_genss_currrelocs;
static i64 ax_genss_nrelocs;
static struct ax_lib_mclrec *  ax_genss_currmcl;
byte *  ax_objdecls_relocnames[7] = {(byte*)"abs_rel",(byte*)"addr64_rel",(byte*)"addr32_rel",(byte*)"addr32nb_rel",(byte*)"rel32_rel",(byte*)"rel321_rel",(byte*)"rel8_rel"};
byte *  ax_objdecls_coffscopenames[3] = {(byte*)"cofflocal_scope",(byte*)"export_scope",(byte*)"import_scope"};
static i64 ax_writeexe_libinsttable[30];
static byte *  ax_writeexe_libinstnames[30];
static i64 ax_writeexe_libnotable[30];
static i64 ax_writeexe_imagesize;
static i64 ax_writeexe_filesize;
static i64 (*ax_writeexe_thunktable)[];
static i64 ax_writeexe_fileiatoffset;
static i64 ax_writeexe_fileiatsize;
static struct ax_decls_strec *  ax_writeexe_stentrypoint;
static struct ax_decls_strec *  ax_writeexe_stentrypoint2;
static struct ax_decls_strec *  ax_writeexe_stentrypoint3;
static struct ax_writeexe_sectionrec ax_writeexe_sectiontable[10];
static i64 ax_writeexe_nsections;
static byte *  ax_writeexe_importdir;
static struct ax_writeexe_importrec ax_writeexe_importtable[3000];
static i64 ax_writeexe_nimports;
static struct ax_writeexe_dllrec ax_writeexe_dlltable[50];
static i64 ax_writeexe_ndlls;
static byte *  ax_writeexe_datastart;
static byte *  ax_writeexe_dataptr;
static byte *  ax_writeexe_userentrypoint;
static i64 ax_disasm_abc;
static double ax_disasm_xyz;
static i64 ax_disasm_res2;
static i64 ax_disasm_lx;
static i64 ax_disasm_nmodules;
static i64 ax_disasm_xfchsmask_pd;
static byte *  ax_disasm_opnames[8] = {(byte*)"add",(byte*)"or",(byte*)"adc",(byte*)"sbb",(byte*)"and",(byte*)"sub",(byte*)"xor",(byte*)"cmp"};
static byte *  ax_disasm_condnames[16] = {
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
static byte *  ax_disasm_addrmodenames[3] = {(byte*)"amreg",(byte*)"ammem",(byte*)"amrel"};
static i64 ax_disasm_rex;
static i64 ax_disasm_addrmode;
static i64 ax_disasm_rmreg;
static i64 ax_disasm_rmopc;
static i64 ax_disasm_basereg;
static i64 ax_disasm_indexreg;
static i64 ax_disasm_scale;
static i64 ax_disasm_opsize;
static i64 ax_disasm_offset;
static i64 ax_disasm_offsetsize;
static i64 ax_disasm_sizeoverride;
static i64 ax_disasm_addroverride;
static i64 ax_disasm_f2override;
static i64 ax_disasm_f3override;
static byte ax_disasm_deststr[256];
static byte *  ax_disasm_destptr;
static byte *  ax_disasm_codeptr;
static i64 ax_writeobj_symtaboffset;
static byte *  ax_writeobj_datastart;
static byte *  ax_writeobj_dataptr;
static struct ax_objdecls_imagesymbol ax_writeobj_symboltable[10001];
static i64 ax_writeobj_nsymbols;
static i64 ax_writeobj_stoffset = (i64)0;
static byte *  ax_writeobj_stringtable[5000];
static i64 ax_writeobj_stringlengths[5000];
static i64 ax_writeobj_nextstringoffset = (i64)0;
static i64 ax_writeobj_nstrings = (i64)0;

/* PROCDEFS */
// START
void start(void) {
    struct mlib_strbuffer *  ss;
    i64 t;
    t = (i64)(clock());
    ax_initall();
    ax_getinputoptions();
    ax_inputfile = ax_decls_moduletable[((i64)1)-1].filename;
    ax_initlogfile();
    if ((ax_axlevel == (i64)1)) {
        if ((ax_decls_nmodules > (i64)1)) {
            ax_loaderror((byte*)"lex test/multi files");
        };
        ax_lextest(ax_inputfile);
    } else {
        if ((ax_outputfile == 0)) {
            ax_outputfile = mlib_pcm_copyheapstring(mlib_changeext(ax_inputfile,((ax_axlevel == (i64)5)?(byte*)"exe":(byte*)"obj")));
        };
        if (!(!!(ax_decls_fquiet))) {
            if ((ax_axlevel == (i64)4)) {
                msysnewc_m_print_startcon();
                msysnewc_m_print_str((byte*)"Assembling",NULL);
                msysnewc_m_print_str(ax_inputfile,NULL);
                msysnewc_m_print_str((byte*)"to",NULL);
                msysnewc_m_print_str(ax_outputfile,NULL);
                msysnewc_m_print_newline();
                msysnewc_m_print_end();
                ;
            } else {
                msysnewc_m_print_startcon();
                msysnewc_m_print_str((byte*)"Assembling/linking to",NULL);
                msysnewc_m_print_str(ax_outputfile,NULL);
                msysnewc_m_print_newline();
                msysnewc_m_print_end();
                ;
            };
        };
        if (!!(ax_decls_fverbose)) {
            ax_showcaption();
            msysnewc_m_print_startcon();
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
        };
        ax_loadsourcefiles();
        ax_parsemodules();
        if ((ax_axlevel==(i64)4)) {
            ax_genss_genss();
            if ((!!((u64)(ax_fshowss)) || !!((u64)(ax_fshowsx)))) {
                ax_writeexe_initsectiontable();
                ss = ax_writeexe_writessdata((i64)0);
                mlib_gs_println(ss,ax_decls_logdev);
            };
            ax_writeobj_writess(ax_outputfile);
        }else if ((ax_axlevel==(i64)5)) {
            ax_genss_genss();
            ax_writeexe_initsectiontable();
            if (!!((u64)(ax_fshowss))) {
                ss = ax_writeexe_writessdata((i64)0);
                mlib_gs_println(ss,ax_decls_logdev);
            };
            ax_writeexe_genexe((byte *)(0));
            if (!!((u64)(ax_fshowsx))) {
                ss = ax_writeexe_writessdata((i64)1);
                mlib_gs_println(ss,ax_decls_logdev);
            };
            ax_writeexe_writeexe(ax_outputfile);
        };
        if (!!((u64)(ax_fshowmcl))) {
            ss = ax_lib_writemclblock();
            mlib_gs_println(ss,ax_decls_logdev);
        };
    };
    if (!!((u64)(ax_fshowtiming))) {
        t = ((i64)(clock()) - t);
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"Time",NULL);
        msysnewc_m_print_i64(t,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    };
    ax_closelogfile();
    exit((i64)0);
}

int main(int nargs, char** args) {
int i;
	msysnewc_nsysparams=nargs;
	if (msysnewc_nsysparams>nargs) {puts("Too many params"); exit(1);}
	for (i=1; i<=nargs; ++i) msysnewc_sysparams[i-1]=(byte*)args[i-1];


	start();
	return 0;
}

static void ax_loadsourcefiles(void) {
    i64 i;
    byte *  source;
    L1 :;
    for (i=(i64)1;i<=ax_decls_nmodules;i+=(i64)1) {
L2 :;
        source = (byte *)(mlib_readfile(ax_decls_moduletable[(i)-1].filename));
        if ((source == 0)) {
            ax_loaderror_s((byte*)"Can't load file: %s",ax_decls_moduletable[(i)-1].filename);
        };
        ax_decls_moduletable[(i)-1].source = source;
L3 :;
    }L4 :;
    ;
}

static void ax_parsemodules(void) {
    i64 i;
    struct ax_lib_mclrec *  m;
    L5 :;
    for (i=(i64)1;i<=ax_decls_nmodules;i+=(i64)1) {
L6 :;
        ax_decls_currmoduleno = i;
        ax_decls_modulenamelist = (struct ax_decls_strec *)(0);
        ax_parse_readmodule(i);
        ax_parse_checkundefined();
        if (!!(ax_decls_nundefined)) {
            msysnewc_m_print_startcon();
            msysnewc_m_print_str((byte*)"Couldn't assemble - press key",NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            oswindows_os_getch();
            exit((i64)1);
        };
        ax_scanglobals();
        if (!!((u64)(ax_fshowsx))) {
        };
        if ((i != ax_decls_nmodules)) {
            ax_resethashtable();
        };
L7 :;
    }L8 :;
    ;
    if (!!((u64)(ax_fshowsx))) {
    };
    m = ax_lib_mccode;
    L9 :;
    while (!!(m)) {
        ax_fixopnd((*m).a);
        ax_fixopnd((*m).b);
        m = (*m).nextmcl;
L10 :;
    }L11 :;
    ;
}

static void ax_fixopnd(struct ax_decls_opndrec * a) {
    struct ax_decls_strec *  d;
    if ((a == 0)) {
        return;
    };
    if (!!((*a).labeldef)) {
        d = (*a).labeldef;
        if (!!((*d).basedef)) {
            (*a).labeldef = (*d).basedef;
        };
    };
}

static void ax_initlogfile(void) {
    if ((ax_logdest==(i64)2)) {
        remove((i8 *)((byte*)"mx.log"));
        ax_decls_logdev = fopen((i8 *)((byte*)"mx.log"),(i8 *)((byte*)"w"));
    }else if ((ax_logdest==(i64)0) || (ax_logdest==(i64)1)) {
        ax_decls_logdev = 0;
    };
}

static void ax_closelogfile(void) {
    byte str[512];
    if ((ax_logdest == (i64)2)) {
        fclose(ax_decls_logdev);
        msysnewc_m_print_startstr(str);
        msysnewc_m_print_str((byte*)"\\m\\ed.bat",NULL);
        msysnewc_m_print_str((byte*)"mx.log",NULL);
        msysnewc_m_print_end();
        ;
        oswindows_os_execwait(str,(i64)1,(byte *)(0));
    };
}

static void ax_initall(void) {
    mlib_pcm_init();
    ax_lex_initlex();
    ax_lib_initlib();
}

static void ax_lextest(byte * file) {
    ax_loadsourcefiles();
    ax_lex_initsourcefile(ax_decls_moduletable[((i64)1)-1].source);
    ax_lex_lxsymbol = (i64)11;
    L12 :;
    while ((ax_lex_lxsymbol != (i64)12)) {
        ax_lex_lex();
L13 :;
    }L14 :;
    ;
}

static void ax_getinputoptions(void) {
    i64 paramno;
    i64 pmtype;
    i64 sw;
    byte *  name;
    byte *  value;
    i64 av_1;
    paramno = (i64)2;
    L15 :;
    while (!!((pmtype = mlib_nextcmdparam(&paramno,&name,&value,(byte*)".asm")))) {
        if ((pmtype==(i64)1)) {
            mlib_convlcstring(name);
            L18 :;
            for (sw=(i64)1;sw<=(i64)17;sw+=(i64)1) {
L19 :;
                if (!!(mlib_eqstring(name,ax_optionnames[(sw)-1]))) {
                    ax_do_option(sw,value);
                    goto L21 ;
                };
L20 :;
            }
            {
                msysnewc_m_print_startcon();
                msysnewc_m_print_str((byte*)"Unknown option:",NULL);
                msysnewc_m_print_str(name,NULL);
                msysnewc_m_print_newline();
                msysnewc_m_print_end();
                ;
                exit((i64)1);
            }L21 :;
            ;
        }else if ((pmtype==(i64)2)) {
            ax_addmodule(name);
        }else if ((pmtype==(i64)3)) {
            ax_addsearchlib(name);
        };
L16 :;
    }L17 :;
    ;
    if (((ax_decls_nmodules == (i64)0) && (ax_decls_nsearchlibs == (i64)0))) {
        ax_showcaption();
        msysnewc_m_print_startcon();
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"Usage:",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"\t",NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_str(msysnewc_sysparams[((i64)1)-1],NULL);
        msysnewc_m_print_str((byte*)"filename[.asm]           # Assemble filename.asm to filename.obj",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"\t",NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_str(msysnewc_sysparams[((i64)1)-1],NULL);
        msysnewc_m_print_str((byte*)"-help                    # Show options",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        exit((i64)1);
    };
    if (((!!((u64)(ax_fshowss)) || !!((u64)(ax_fshowsx))) || !!((u64)(ax_fshowmcl)))) {
        if ((ax_logdest == (i64)0)) {
            ax_logdest = (i64)2;
        };
    };
    if ((ax_decls_nsearchlibs == (i64)0)) {
        ax_decls_searchlibs[((i64)1)-1] = (byte*)"msvcrt";
        ax_decls_searchlibs[((i64)2)-1] = (byte*)"gdi32";
        ax_decls_searchlibs[((i64)3)-1] = (byte*)"user32";
        ax_decls_searchlibs[((i64)4)-1] = (byte*)"kernel32";
        ax_decls_nsearchlibs = (i64)4;
    };
    if ((ax_decls_nmodules == (i64)0)) {
        ax_loaderror((byte*)"No input files specified");
    };
}

static void ax_do_option(i64 sw,byte * value) {
    if ((sw==(i64)1) || (sw==(i64)2) || (sw==(i64)3) || (sw==(i64)4) || (sw==(i64)5)) {
        ax_axlevel = sw;
    }else if ((sw==(i64)6)) {
        ax_fshowmcl = (u64)((i64)1);
    }else if ((sw==(i64)7)) {
        ax_fshowss = (u64)((i64)1);
    }else if ((sw==(i64)8)) {
        ax_fshowsx = (u64)((i64)1);
    }else if ((sw==(i64)9)) {
        ax_fshowtiming = (u64)((i64)1);
    }else if ((sw==(i64)10)) {
        ax_logdest = (i64)1;
    }else if ((sw==(i64)11)) {
        ax_logdest = (i64)2;
    }else if ((sw==(i64)12)) {
        ax_decls_fverbose = (i64)1;
    }else if ((sw==(i64)13)) {
        ax_decls_fquiet = (i64)1;
    }else if ((sw==(i64)14)) {
        ax_showhelp();
    }else if ((sw==(i64)15)) {
        ax_outputfile = mlib_pcm_copyheapstring(value);
    }else if ((sw==(i64)16)) {
    }else if ((sw==(i64)17)) {
    };
}

static void ax_showhelp(void) {
    ax_showcaption();
    msysnewc_m_print_startcon();
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"'AX' Assembler-Linker for Win64\r\n\r\nAssembles ASM files written in a special syntax to OBJ or EXE format.\r\n\r\nUsage:\r\n\r\n\tax prog            Assemble prog.asm to prog.exe\r\n\tax prog -obj       Assemble prog.asm to prog.obj (needs ext. linker)\r\n\r\n\tax a b c           Assemble&link modules a.asm, b.asm, c.asm into a.exe\r\n\r\nOptions:\r\n\r\n\t-out:file          Name output file (default is .exe applied to 1st module)\r\n\t-out:exe           Generate executable (default)\r\n\t-out:obj           Generate object file (one .obj file for multiple i/p files)\r\n\tfile.dll           Include library in list of DLLs to search\r\n\r\nCan only link to external DLL libraries; not other .o/.obj/.lib/.a files.\r\n\r\nDLLs msvcrt.dll, user32.dll, gdi32.dll, user32.dll are automatically included.\r\nBut if at least one .dll file is specified, the list is cleared, so may need\r\nto specify msvcrt.dll etc as needed.\r\n\r\n",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    exit((i64)1);
}

static void ax_showcaption(void) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"AX Assembler/Linker",NULL);
    msysnewc_m_print_str((byte*)"9-Sep-2020",NULL);
    msysnewc_m_print_end();
    ;
}

static void ax_loaderror(byte * mess) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"Error:",NULL);
    msysnewc_m_print_str(mess,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    exit((i64)1);
}

static void ax_loaderror_s(byte * mess,byte * s) {
    byte str[256];
    sprintf((i8 *)(str),(i8 *)(mess),s);
    ax_loaderror(str);
}

static void ax_addmodule(byte * name) {
    if ((ax_decls_nmodules >= (i64)200)) {
        ax_loaderror((byte*)"Too many modules");
    };
    ++ax_decls_nmodules;
    ax_decls_moduletable[(ax_decls_nmodules)-1].filename = mlib_pcm_copyheapstring(name);
    ax_decls_moduletable[(ax_decls_nmodules)-1].name = mlib_pcm_copyheapstring(mlib_extractfile(name));
    ax_decls_moduletable[(ax_decls_nmodules)-1].source = (byte*)"<empty>";
}

static void ax_addsearchlib(byte * name) {
    byte str[300];
    if ((ax_decls_nsearchlibs >= (i64)30)) {
        ax_loaderror((byte*)"Too many libraries");
    };
    ++ax_decls_nsearchlibs;
    strcpy((i8 *)(str),(i8 *)(name));
    str[(((i64)(strlen((i8 *)(name))) - (i64)3))-1] = (u64)0u;
    ax_decls_searchlibs[(ax_decls_nsearchlibs)-1] = mlib_pcm_copyheapstring(str);
}

static void ax_showmodules(void) {
    i64 i;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"Modules:",NULL);
    msysnewc_m_print_i64(ax_decls_nmodules,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    L22 :;
    for (i=(i64)1;i<=ax_decls_nmodules;i+=(i64)1) {
L23 :;
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"  ",NULL);
        msysnewc_m_print_i64(i,NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_str((byte*)":",NULL);
        msysnewc_m_print_str(mlib_padstr(ax_decls_moduletable[(i)-1].name,(i64)13,(byte*)" "),NULL);
        msysnewc_m_print_str(mlib_padstr(ax_decls_moduletable[(i)-1].filename,(i64)25,(byte*)" "),NULL);
        msysnewc_m_print_u64(strlen((i8 *)(ax_decls_moduletable[(i)-1].source)),NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
L24 :;
    }L25 :;
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"Search Libs:",NULL);
    msysnewc_m_print_i64(ax_decls_nsearchlibs,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    L26 :;
    for (i=(i64)1;i<=ax_decls_nsearchlibs;i+=(i64)1) {
L27 :;
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"  ",NULL);
        msysnewc_m_print_i64(i,NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_str((byte*)":",NULL);
        msysnewc_m_print_str(ax_decls_searchlibs[(i)-1],NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
L28 :;
    }L29 :;
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

static struct ax_decls_strec * ax_getemptyst(struct ax_decls_strec * d) {
    struct ax_decls_strec *  dnew;
    if (!!((u64)((*d).ksymbol))) {
        dnew = (struct ax_decls_strec *)(mlib_pcm_allocz((i64)128));
        (*dnew).name = (*d).name;
        (*dnew).namelen = (u64)((*d).namelen);
        (*dnew).ksymbol = (u64)((*d).ksymbol);
        (*dnew).subcode = (u64)((*d).subcode);
        (*dnew).regsize = (u64)((*d).regsize);
        return dnew;
    };
    return (struct ax_decls_strec *)(0);
}

static struct ax_decls_strec * ax_findduplname(struct ax_decls_strec * d) {
    struct ax_decls_strec *  e;
    if (!!((*d).basedef)) {
        return (*d).basedef;
    };
    e = ax_decls_dupltable[((i64)((*d).htfirstindex))];
    L30 :;
    while (!!(e)) {
        if ((((u64)((*d).namelen) == (u64)((*e).namelen)) && ((i64)(memcmp((void *)((*d).name),(void *)((*e).name),(u64)((*d).namelen))) == (i64)0))) {
            (*d).basedef = e;
            return e;
        };
        e = (*e).nextdupl;
L31 :;
    }L32 :;
    ;
    return (struct ax_decls_strec *)(0);
}

static void ax_adddupl(struct ax_decls_strec * d) {
    (*d).nextdupl = ax_decls_dupltable[((i64)((*d).htfirstindex))];
    ax_decls_dupltable[((i64)((*d).htfirstindex))] = d;
}

static void ax_scanglobals(void) {
    struct ax_decls_strec *  d;
    struct ax_decls_strec *  e;
    d = ax_decls_modulenamelist;
    L33 :;
    while (!!(d)) {
        if (((i64)((*d).symbol)==(i64)21)) {
            e = ax_findduplname(d);
            if (!!(e)) {
                if (((i64)((*e).symbol)==(i64)21)) {
                }else if (((i64)((*e).symbol)==(i64)22)) {
                    (*d).symbol = (u64)((i64)22);
                    (*d).reftype = (u64)(((*e).reftype = (u64)((i64)1)));
                };
            } else {
                ax_lib_addimport(d);
                ax_adddupl(d);
            };
        }else if (((i64)((*d).symbol)==(i64)22)) {
            e = ax_findduplname(d);
            if (!!(e)) {
                if (((i64)((*e).symbol)==(i64)21)) {
                    (*e).symbol = (u64)((i64)22);
                    (*d).reftype = (u64)(((*e).reftype = (u64)((i64)1)));
                }else if (((i64)((*e).symbol)==(i64)22)) {
                    msysnewc_m_print_startcon();
                    msysnewc_m_print_str(ax_decls_moduletable[((i64)((*d).moduleno))-1].name,NULL);
                    msysnewc_m_print_str((*d).name,NULL);
                    msysnewc_m_print_u64((*d).htindex,NULL);
                    msysnewc_m_print_newline();
                    msysnewc_m_print_end();
                    ;
                    msysnewc_m_print_startcon();
                    msysnewc_m_print_str(ax_decls_moduletable[((i64)((*e).moduleno))-1].name,NULL);
                    msysnewc_m_print_str((*e).name,NULL);
                    msysnewc_m_print_u64((*e).htindex,NULL);
                    msysnewc_m_print_newline();
                    msysnewc_m_print_end();
                    ;
                    ax_lib_serror_s((byte*)"Multiply-defined global: %s",(*d).name);
                };
            } else {
                e = d;
                ax_lib_addimport(d);
                ax_adddupl(d);
            };
        };
        d = (*d).nextdef;
L34 :;
    }L35 :;
    ;
}

static void ax_resethashtable(void) {
    struct ax_decls_strec *  d;
    d = ax_decls_modulenamelist;
    L36 :;
    while (!!(d)) {
        ax_decls_lexhashtable[((i64)((*d).htindex))] = ax_getemptyst(d);
        d = (*d).nextdef;
L37 :;
    }L38 :;
    ;
    ax_decls_modulenamelist = (struct ax_decls_strec *)(0);
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
    L39 :;
    while (!!((i64)1)) {
        c = (u64)((*msysnewc_fmtstr));
        switch ((i64)(c)) {
        case 35:;
        {
            if (!!(lastx)) {
                goto L42 ;
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
L42 :;
;
            ++n;
            ++msysnewc_fmtstr;
        }
        } //SW
;
L40 :;
    }L41 :;
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
    L43 :;
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
            goto L46 ;
;
        }break;
        default: {
            if ((((u64)(c) >= '0') && ((u64)(c) <= '9'))) {
                n = ((u64)(c) - '0');
                L47 :;
                while (1) {
                    c = (u64)((*s));
                    if (((i64)((*s)) == (i64)0)) {
                        goto L48 ;
                    };
                    if ((((u64)(c) >= '0') && ((u64)(c) <= '9'))) {
                        ++s;
                        n = (((n * (i64)10) + (i64)(c)) - (i64)48);
                    } else {
                        goto L48 ;
                    };
                }L48 :;
                ;
                //gotwidth:
L46 :;
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
L44 :;
    }L45 :;
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
L49 :;
        if (((i64)((*p)) == (i64)0)) {
            goto L51 ;
        };
        (*q) = (u64)((*p));
        ++q;
        ++p;
L50 :;
    }L51 :;
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
        L52 :;
        for (i=(i64)1;i<=(w - n);i+=(i64)1) {
L53 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L54 :;
        }L55 :;
        ;
        (*t) = (u64)0u;
    } else if (((u64)((*fmt).justify) == 'R')) {
        if (((((u64)((*fmt).padchar) == '0') && !!((u64)((*fmt).base))) && (((u64)((*s)) == '-') || ((u64)((*s)) == '+')))) {
            (*t) = (u64)((*s));
            ++t;
            av_2 = (w - n);
            while (av_2-- > 0) {
L56 :;
                (*t) = (u64)((*fmt).padchar);
                ++t;
L57 :;
            }L58 :;
            ;
            strncpy((i8 *)(t),(i8 *)((s + (i64)1)),(u64)((n - (i64)1)));
            (*((t + n) - (i64)1)) = (u64)0u;
        } else {
            av_3 = (w - n);
            while (av_3-- > 0) {
L59 :;
                (*t) = (u64)((*fmt).padchar);
                ++t;
L60 :;
            }L61 :;
            ;
            strncpy((i8 *)(t),(i8 *)(s),(u64)(n));
            (*(t + n)) = (u64)0u;
        };
    } else {
        m = (((w - n) + (i64)1) / (i64)2);
        av_4 = m;
        while (av_4-- > 0) {
L62 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L63 :;
        }L64 :;
        ;
        strncpy((i8 *)(t),(i8 *)(s),(u64)(n));
        t += n;
        av_5 = ((w - n) - m);
        while (av_5-- > 0) {
L65 :;
            (*t) = (u64)((*fmt).padchar);
            ++t;
L66 :;
        }L67 :;
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
    L68 :;
    do {
        t[(++i)] = (u64)(msysnewc_digits[((i64)((aa % base)))]);
        aa = (aa / base);
        ++k;
        if (((!!(sep) && ((i64)(aa) != (i64)0)) && (k == g))) {
            t[(++i)] = (u64)(sep);
            k = (i64)0;
        };
L69 :;
    } while (!((i64)(aa) == (i64)0));L70 :;
    ;
    j = i;
    s0 = s;
    L71 :;
    while (!!(i)) {
        (*s) = (u64)(t[(i--)]);
        ++s;
L72 :;
    }L73 :;
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
    L74 :;
    while (!!(i)) {
        --s;
        (*s) = (u64)(t[((i-- - (i64)1))]);
        if (((!!(sep) && !!(i)) && (++k == g))) {
            --s;
            (*s) = (u64)(sep);
            k = (i64)0;
        };
L75 :;
    }L76 :;
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
    L77 :;
    while ((((u64)((*s)) == ' ') || ((i64)((*s)) == (i64)9))) {
        ++s;
L78 :;
    }L79 :;
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
    L80 :;
    while (!!((u64)((*s)))) {
        c = (u64)((*s++));
        switch ((i64)(c)) {
        case 32:;
        case 9:;
        case 44:;
        case 61:;
        {
            if ((!!((u64)(quotechar)) || (p == s))) {
                goto L83 ;
;
            };
            msysnewc_termchar = (i64)(c);
            goto L82 ;
        }break;
        default: {
            //normalchar:
L83 :;
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
                    goto L82 ;
                };
            } else {
                (*p) = (u64)(c);
                ++p;
            };
        }
        } //SW
;
L81 :;
    }L82 :;
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
    L84 :;
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
            goto L85 ;
        }break;
        default: {
            msysnewc_itemerror = (i64)1;
            goto L86 ;
        }
        } //SW
;
        if (((i64)(d) >= base)) {
            msysnewc_itemerror = (i64)1;
            goto L86 ;
        };
        aa = (u64)((((i64)(aa) * base) + (i64)(d)));
L85 :;
    }L86 :;
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
L87 :;
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L88 :;
    }L89 :;
    ;
}

static void msysnewc_iconvucn(byte * s,i64 n) {
    i64 av_1;
    av_1 = n;
    while (av_1-- > 0) {
L90 :;
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L91 :;
    }L92 :;
    ;
}

static void msysnewc_convlcstring(byte * s) {
    L93 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L94 :;
    }L95 :;
    ;
}

static void msysnewc_convucstring(byte * s) {
    L96 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L97 :;
    }L98 :;
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
    L99 :;
    for (i=(i64)1;i<=(i64)2048;i+=(i64)1) {
L100 :;
        j = (i64)1;
        k = (i64)16;
        L103 :;
        while ((i > k)) {
            k = (k << (i64)1);
            ++j;
L104 :;
        }L105 :;
        ;
        mlib_sizeindextable[(i)] = (u64)(j);
L101 :;
    }L102 :;
    ;
    mlib_allocupper[((i64)1)] = (u64)((i64)16);
    size = (i64)16;
    L106 :;
    for (i=(i64)2;i<=(i64)27;i+=(i64)1) {
L107 :;
        size *= (i64)2;
        mlib_allocupper[(i)] = (u64)(size);
        if ((size >= (i64)33554432)) {
            k = i;
            goto L109 ;
        };
L108 :;
    }L109 :;
    ;
    L110 :;
    for (i=(k + (i64)1);i<=(i64)300;i+=(i64)1) {
L111 :;
        size += (i64)33554432;
        if ((size < (i64)8589934592)) {
            mlib_allocupper[(i)] = (u64)(size);
            mlib_maxmemory = (u64)(size);
        } else {
            mlib_maxalloccode = (i - (i64)1);
            goto L113 ;
        };
L112 :;
    }L113 :;
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
        L114 :;
        while ((n > m)) {
            m <<= (i64)1;
L115 :;
        }L116 :;
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
    L117 :;
    while (!!(p)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)" ",NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_ptr(p,(byte*)"h");
        msysnewc_m_print_end();
        ;
        p = (u64 *)((i64)((*p)));
L118 :;
    }L119 :;
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
    L120 :;
    for (i=(i64)1;i<=(i64)8;i+=(i64)1) {
L121 :;
        mlib_pcm_printfreelist(m,mlib_freelist[(i)]);
        m <<= (i64)1;
L122 :;
    }L123 :;
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
    L124 :;
    for (i=(i64)1;i<=(i64)500000;i+=(i64)1) {
L125 :;
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
L126 :;
    }L127 :;
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
    L128 :;
    for (i=(i64)1;i<=(i64)500000;i+=(i64)1) {
L129 :;
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
L130 :;
    }L131 :;
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
        L132 :;
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
        }L133 :;
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
    L134 :;
    while (((p >= buffer) && (((i64)((*p)) == (i64)13) || ((i64)((*p)) == (i64)10)))) {
        if ((((i64)((*p)) == (i64)13) || ((i64)((*p)) == (i64)10))) {
            crseen = (u64)((i64)1);
        };
        (*p--) = (u64)0u;
L135 :;
    }L136 :;
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
L137 :;
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L138 :;
    }L139 :;
    ;
}

void mlib_iconvucn(byte * s,i64 n) {
    i64 av_1;
    av_1 = n;
    while (av_1-- > 0) {
L140 :;
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L141 :;
    }L142 :;
    ;
}

void mlib_convlcstring(byte * s) {
    L143 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(tolower((i64)((i32)((*s)))));
        ++s;
L144 :;
    }L145 :;
    ;
}

void mlib_convucstring(byte * s) {
    L146 :;
    while (!!((u64)((*s)))) {
        (*s) = (u64)(toupper((i64)((i32)((*s)))));
        ++s;
L147 :;
    }L148 :;
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
    L149 :;
    while ((u >= t)) {
        if (((u64)((*u)) == '.')) {
            if (((i64)((*(u + (i64)1))) == (i64)0)) {
                return (!!(period)?(byte*)".":(byte*)"");
            };
            return (u + (i64)1);
        };
        --u;
L150 :;
    }L151 :;
    ;
    return (byte*)"";
}

byte * mlib_extractpath(byte * s) {
    static byte str[260];
    byte *  t;
    i64 n;
    t = ((s + (i64)(strlen((i8 *)(s)))) - (i64)1);
    L152 :;
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
L153 :;
    }L154 :;
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
    L155 :;
    for (i=(i64)2;i<=(i64)2;i+=(i64)1) {
L156 :;
        p = mlib_freelist[(i)];
        L159 :;
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
L160 :;
        }L161 :;
        ;
L157 :;
    }L158 :;
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
        L162 :;
        for (i=(i64)1;i<=n;i+=(i64)1) {
L163 :;
            str[((slen + i))-1] = (u64)(padch);
L164 :;
        }L165 :;
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
    L166 :;
    for (i=(i64)1;i<=n;i+=(i64)1) {
L167 :;
        str[(i)-1] = (u64)(ch);
L168 :;
    }L169 :;
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
L170 :;
;
    (*value) = (byte *)(0);
    (*name) = (byte *)(0);
    if (!!(infile)) {
        if ((mlib_readnextfileitem(&fileptr,&item) == (i64)0)) {
            free((void *)(filestart));
            infile = (i64)0;
            goto L170 ;
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
            goto L170 ;
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
L171 :;
;
    L172 :;
    while (1) {
        if (((i64)((*p))==(i64)32) || ((i64)((*p))==(i64)9) || ((i64)((*p))==(i64)13) || ((i64)((*p))==(i64)10)) {
            ++p;
        }else if (((i64)((*p))==(i64)26) || ((i64)((*p))==(i64)0)) {
            return (i64)0;
        } else {
            goto L173 ;
        };
    }L173 :;
    ;
    if (((i64)((*p))==(i64)33) || ((i64)((*p))==(i64)35)) {
        ++p;
        L174 :;
        if (((i64)((*p++))==(i64)10)) {
            goto L171 ;
;
        }else if (((i64)((*p++))==(i64)26) || ((i64)((*p++))==(i64)0)) {
            (*fileptr) = (p - (i64)1);
            return (i64)0;
        } else {
        }goto L174 ;
L175 :;
        ;
    };
    if (((i64)((*p))==(i64)34)) {
        pstart = ++p;
        L176 :;
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
                goto L177 ;
            };
            ++p;
        }L177 :;
        ;
    } else {
        pstart = p;
        L178 :;
        while (1) {
            if (((i64)((*p))==(i64)0) || ((i64)((*p))==(i64)26)) {
                pend = p;
                goto L179 ;
            }else if (((i64)((*p))==(i64)32) || ((i64)((*p))==(i64)9) || ((i64)((*p))==(i64)44) || ((i64)((*p))==(i64)13) || ((i64)((*p))==(i64)10)) {
                pend = p++;
                goto L179 ;
            };
            ++p;
        }L179 :;
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
L180 :;
        strcat((i8 *)(s),(i8 *)(padchar));
L181 :;
    }L182 :;
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
    L183 :;
    do {
        x = ((double)(mlib_mrandomp()) / (double)9223372036854775800.);
L184 :;
    } while (!(x != (double)1.));L185 :;
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
            L186 :;
            do {
                count = (i64)0;
                ReadConsoleInputA(oswindows_hconsolein,(void *)(&oswindows_lastkey),(u64)1u,(void *)(&count));
L187 :;
            } while (!(((i64)((u64)(oswindows_lastkey.eventtype)) == (i64)1) && ((i64)((u64)(oswindows_lastkey.keydown)) == (i64)1)));L188 :;
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

void ax_lex_lex(void) {
    i64 i;
    i64 c;
    i64 hsum;
    i64 csum;
    i64 length;
    byte *  pstart;
    ax_lex_lxsubcode = (i64)0;
    L189 :;
    switch ((c = (i64)((*ax_lex_lxsptr++)))) {
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
        pstart = (ax_lex_lxsptr - (i64)1);
        //doname:
L191 :;
;
        hsum = (csum = c);
        L192 :;
        switch ((c = (i64)((*ax_lex_lxsptr++)))) {
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
            csum += c;
            hsum = ((hsum << (i64)3) + csum);
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
            (*(ax_lex_lxsptr - (i64)1)) = (u64)((c + (i64)32));
            csum += (c + (i64)32);
            hsum = ((hsum << (i64)3) + csum);
        }break;
        default: {
            --ax_lex_lxsptr;
            goto L193 ;
        }
        } //SW
goto L192 ;
L193 :;
        ;
        ax_lex_lxlength = (ax_lex_lxsptr - pstart);
        ax_lex_lxhashvalue = ((hsum << (i64)5) ^ csum);
        if (!!(ax_lex_lookuplex((byte *)(pstart),ax_lex_lxlength))) {
            if (!!((u64)((*ax_lex_lxsymptr).ksymbol))) {
                ax_lex_lxsymbol = (i64)((*ax_lex_lxsymptr).ksymbol);
                ax_lex_lxsubcode = (i64)((*ax_lex_lxsymptr).subcode);
            } else {
                ax_lex_lxsymbol = (i64)((*ax_lex_lxsymptr).symbol);
            };
        } else {
            ax_lex_lxsymbol = (i64)17;
        };
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
        pstart = (ax_lex_lxsptr - (i64)1);
        c = (i64)((u64)(((*pstart) = (u64)(((i64)((u64)((*pstart))) + (i64)32)))));
        goto L191 ;
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
        ax_lex_readnumber(c);
        return;
    }break;
    case 96:;
    {
        pstart = ax_lex_lxsptr;
        hsum = (csum = (i64)0);
        L194 :;
        switch ((c = (i64)((*ax_lex_lxsptr)))) {
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
            ++ax_lex_lxsptr;
            csum += c;
            hsum = ((hsum << (i64)3) + csum);
        }break;
        default: {
            goto L195 ;
        }
        } //SW
goto L194 ;
L195 :;
        ;
        ax_lex_lxsymbol = (i64)17;
        if ((pstart == ax_lex_lxsptr)) {
            ax_lex_lxerror((byte*)"NULL ` name");
        };
        ax_lex_lxlength = (ax_lex_lxsptr - pstart);
        ax_lex_lxhashvalue = ((hsum << (i64)5) ^ csum);
        if (!!(ax_lex_lookuplex((byte *)(pstart),ax_lex_lxlength))) {
            ax_lex_lxsymbol = (i64)((*ax_lex_lxsymptr).symbol);
            if ((ax_lex_lxsymbol == (i64)0)) {
                ax_lex_lxsymbol = (i64)((u64)(((*ax_lex_lxsymptr).symbol = (u64)((i64)17))));
            };
        };
        return;
    }break;
    case 33:;
    case 59:;
    case 35:;
    {
        L196 :;
        while (!!((u64)(ax_lex_commentmap[((i64)((*ax_lex_lxsptr++)))]))) {
L197 :;
        }L198 :;
        ;
        if (((i64)((u64)((*(ax_lex_lxsptr - (i64)1)))) == (i64)0)) {
            --ax_lex_lxsptr;
        };
        ++ax_decls_lxlineno;
        ax_lex_lxsymbol = (i64)11;
        return;
    }break;
    case 44:;
    {
        ax_lex_lxsymbol = (i64)2;
        return;
    }break;
    case 58:;
    {
        if (((u64)((*ax_lex_lxsptr)) == (u64)58u)) {
            ax_lex_lxsymbol = (i64)4;
            ++ax_lex_lxsptr;
        } else {
            ax_lex_lxsymbol = (i64)3;
        };
        return;
    }break;
    case 91:;
    {
        ax_lex_lxsymbol = (i64)5;
        return;
    }break;
    case 93:;
    {
        ax_lex_lxsymbol = (i64)6;
        return;
    }break;
    case 43:;
    {
        ax_lex_lxsymbol = (i64)7;
        return;
    }break;
    case 45:;
    {
        ax_lex_lxsymbol = (i64)8;
        return;
    }break;
    case 42:;
    {
        ax_lex_lxsymbol = (i64)9;
        return;
    }break;
    case 61:;
    {
        ax_lex_lxsymbol = (i64)10;
        return;
    }break;
    case 39:;
    {
        pstart = ax_lex_lxsptr;
        L199 :;
        while (1) {
            switch ((i64)((*ax_lex_lxsptr++))) {
            case 39:;
            {
                goto L200 ;
            }break;
            case 13:;
            case 10:;
            {
                ax_lex_lxerror((byte*)"String not terminated");
            }break;
            default: {
            }
            } //SW
;
        }L200 :;
        ;
        length = ((ax_lex_lxsptr - pstart) - (i64)1);
        ax_lex_lxvalue = (i64)0;
        L201 :;
        for (i=length;i>=(i64)1;i-=(i64)1) {
L202 :;
            ax_lex_lxvalue = ((ax_lex_lxvalue << (i64)8) + (i64)((u64)((*((pstart + i) - (i64)1)))));
L203 :;
        }L204 :;
        ;
        ax_lex_lxsymbol = (i64)14;
        return;
    }break;
    case 34:;
    {
        pstart = ax_lex_lxsptr;
        L205 :;
        while (1) {
            switch ((i64)((*ax_lex_lxsptr++))) {
            case 34:;
            {
                ax_lex_lxsvalue = (byte *)(pstart);
                ax_lex_lxlength = ((ax_lex_lxsptr - pstart) - (i64)1);
                (*(ax_lex_lxsvalue + ax_lex_lxlength)) = (u64)0u;
                ax_lex_lxsymbol = (i64)16;
                return;
            }break;
            case 13:;
            case 10:;
            case 26:;
            case 0:;
            {
                ax_lex_lxerror((byte*)"String not terminated");
            }break;
            default: {
            }
            } //SW
;
        }L206 :;
        ;
    }break;
    case 32:;
    case 9:;
    {
    }break;
    case 13:;
    {
    }break;
    case 10:;
    {
        ++ax_decls_lxlineno;
        ax_lex_lxsymbol = (i64)11;
        return;
    }break;
    case 0:;
    case 26:;
    {
        ax_lex_lxsymbol = (i64)12;
        --ax_lex_lxsptr;
        return;
    }break;
    default: {
        ax_lex_lxsymbol = (i64)1;
        ax_lex_lxvalue = c;
        return;
    }
    } //SW
goto L189 ;
L190 :;
    ;
}

void ax_lex_initlex(void) {
    i64 i;
    ax_lex_lxsubcode = (i64)0;
    ax_lex_lxsymbol = (i64)1;
    ax_decls_lxlineno = (i64)0;
    L207 :;
    for (i=(i64)0;i<=(i64)255;i+=(i64)1) {
L208 :;
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
            ax_lex_alphamap[(i)] = (u64)1u;
        }break;
        default: {
        }
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
            ax_lex_digitmap[(i)] = (u64)1u;
        }break;
        default: {
        }
        } //SW
;
        ax_lex_commentmap[(i)] = (u64)1u;
L209 :;
    }L210 :;
    ;
    ax_lex_commentmap[((i64)0)] = (u64)0u;
    ax_lex_commentmap[((i64)10)] = (u64)0u;
    ax_lex_inithashtable();
}

static void ax_lex_readreal(byte (*s)[],i64 slen,i64 intlen,i64 exponseen) {
    i64 i;
    i64 fractlen;
    i64 expon;
    i64 exponsign;
    i64 c;
    i64 digs;
    i64 av_1;
    i64 av_2;
    if (((intlen == (i64)0) || (intlen == slen))) {
        fractlen = (i64)0;
    } else {
        fractlen = (slen - intlen);
    };
    expon = (i64)0;
    exponsign = (i64)0;
    if (!!(exponseen)) {
        if (((c = (i64)((*ax_lex_lxsptr++)))==(i64)43)) {
        }else if (((c = (i64)((*ax_lex_lxsptr++)))==(i64)45)) {
            exponsign = (i64)1;
        } else {
            --ax_lex_lxsptr;
        };
        digs = (i64)0;
        L211 :;
        switch ((c = (i64)((*ax_lex_lxsptr++)))) {
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
            ++digs;
        }break;
        default: {
            --ax_lex_lxsptr;
            goto L212 ;
        }
        } //SW
goto L211 ;
L212 :;
        ;
        if ((digs == (i64)0)) {
            ax_lex_lxerror((byte*)"Exponent error");
        };
        if (!!(exponsign)) {
            expon = -(expon);
        };
    };
    expon = (expon - fractlen);
    ax_lex_lxxvalue = (double)0.;
    L213 :;
    for (i=(i64)1;i<=slen;i+=(i64)1) {
L214 :;
        c = (i64)((*s)[(i)-1]);
        ax_lex_lxxvalue = ((ax_lex_lxxvalue * (double)10.) + (double)((c - (i64)48)));
L215 :;
    }L216 :;
    ;
    if ((expon > (i64)0)) {
        av_1 = expon;
        while (av_1-- > 0) {
L217 :;
            ax_lex_lxxvalue = (ax_lex_lxxvalue * (double)10.);
L218 :;
        }L219 :;
        ;
    } else if ((expon < (i64)0)) {
        av_2 = -(expon);
        while (av_2-- > 0) {
L220 :;
            ax_lex_lxxvalue = (ax_lex_lxxvalue / (double)10.);
L221 :;
        }L222 :;
        ;
    };
    ax_lex_lxsymbol = (i64)15;
}

static void ax_lex_readnumber(i64 c) {
    byte str[256];
    i64 i;
    i64 d;
    i64 intlen;
    i64 slen;
    d = (i64)((*ax_lex_lxsptr));
    if ((d==(i64)120) || (d==(i64)88)) {
        if ((c==(i64)48)) {
            ++ax_lex_lxsptr;
            ax_lex_readhex();
            return;
        }else if ((c==(i64)50)) {
            ++ax_lex_lxsptr;
            ax_lex_readbinary();
            return;
        } else {
            msysnewc_m_print_startcon();
            msysnewc_m_print_i64(c,NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            ax_lex_lxerror((byte*)"Base not supported");
        };
    };
    str[((i64)1)-1] = (u64)(c);
    slen = (i64)1;
    intlen = (i64)0;
    L223 :;
    switch ((c = (i64)((*ax_lex_lxsptr++)))) {
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
        str[(++slen)-1] = (u64)(c);
    }break;
    case 95:;
    case 39:;
    case 96:;
    {
    }break;
    case 46:;
    {
        intlen = slen;
    }break;
    case 101:;
    case 69:;
    {
        ax_lex_readreal(&str,slen,intlen,(i64)1);
        return;
    }break;
    default: {
        --ax_lex_lxsptr;
        goto L224 ;
    }
    } //SW
goto L223 ;
L224 :;
    ;
    if (!!(intlen)) {
        ax_lex_readreal(&str,slen,intlen,(i64)0);
        return;
    };
    if (((slen > (i64)20) || ((slen == (i64)20) && (mlib_cmpstring(str,(byte*)"18446744073709551615") > (i64)0)))) {
        ax_lex_lxerror((byte*)"Overflow in 64-bit value");
    };
    ax_lex_lxsymbol = (i64)14;
    ax_lex_lxvalue = (i64)0;
    L225 :;
    for (i=(i64)1;i<=slen;i+=(i64)1) {
L226 :;
        ax_lex_lxvalue = (((ax_lex_lxvalue * (i64)10) + (i64)(str[(i)-1])) - (i64)48);
L227 :;
    }L228 :;
    ;
}

static void ax_lex_readbinary(void) {
    i64 ndigs;
    ndigs = (i64)0;
    ax_lex_lxvalue = (i64)0;
    L229 :;
    switch ((i64)((*ax_lex_lxsptr++))) {
    case 48:;
    {
        ax_lex_lxvalue = (ax_lex_lxvalue * (i64)2);
        ++ndigs;
    }break;
    case 49:;
    {
        ax_lex_lxvalue = ((ax_lex_lxvalue * (i64)2) + (i64)1);
        ++ndigs;
    }break;
    case 50:;
    case 51:;
    case 52:;
    case 53:;
    case 54:;
    case 55:;
    case 56:;
    case 57:;
    {
        ax_lex_lxerror((byte*)"Bad binary digit");
    }break;
    case 95:;
    case 39:;
    case 96:;
    {
    }break;
    default: {
        --ax_lex_lxsptr;
        goto L230 ;
    }
    } //SW
goto L229 ;
L230 :;
    ;
    if ((ndigs == (i64)0)) {
        ax_lex_lxerror((byte*)"No bin digits");
    } else if ((ndigs > (i64)64)) {
        ax_lex_lxerror((byte*)"Overflow in binary number");
    };
    ax_lex_lxsymbol = (i64)14;
}

static void ax_lex_readhex(void) {
    i64 ndigs;
    i64 c;
    ndigs = (i64)0;
    ax_lex_lxvalue = (i64)0;
    L231 :;
    switch ((c = (i64)((*ax_lex_lxsptr++)))) {
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
        ax_lex_lxvalue = (((ax_lex_lxvalue * (i64)16) + c) - (i64)48);
        ++ndigs;
    }break;
    case 65:;
    case 66:;
    case 67:;
    case 68:;
    case 69:;
    case 70:;
    {
        ax_lex_lxvalue = ((ax_lex_lxvalue * (i64)16) + ((c - (i64)65) + (i64)10));
        ++ndigs;
    }break;
    case 97:;
    case 98:;
    case 99:;
    case 100:;
    case 101:;
    case 102:;
    {
        ax_lex_lxvalue = ((ax_lex_lxvalue * (i64)16) + ((c - (i64)97) + (i64)10));
        ++ndigs;
    }break;
    case 95:;
    case 39:;
    case 96:;
    {
    }break;
    default: {
        --ax_lex_lxsptr;
        goto L232 ;
    }
    } //SW
goto L231 ;
L232 :;
    ;
    if ((ndigs == (i64)0)) {
        ax_lex_lxerror((byte*)"No hex digits");
    } else if ((ndigs > (i64)16)) {
        ax_lex_lxerror((byte*)"Overflow in hex number");
    };
    ax_lex_lxsymbol = (i64)14;
}

void ax_lex_ps(byte * caption) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str(caption,NULL);
    msysnewc_m_print_str((byte*)":",NULL);
    msysnewc_m_print_end();
    ;
    ax_lex_printsymbol(0);
}

void ax_lex_printsymbol(void * dev) {
    byte str[256];
    i64 av_1;
    strcpy((i8 *)(str),(i8 *)(ax_tables_symbolnames[(ax_lex_lxsymbol)-1]));
    str[(((i64)(strlen((i8 *)(str))) - (i64)2))-1] = (u64)0u;
    msysnewc_m_print_startfile(dev);
    msysnewc_m_print_str(str,NULL);
    msysnewc_m_print_end();
    ;
    av_1 = ((i64)14 - (i64)(strlen((i8 *)(str))));
    while (av_1-- > 0) {
L233 :;
        msysnewc_m_print_startfile(dev);
        msysnewc_m_print_str((byte*)" ",NULL);
        msysnewc_m_print_end();
        ;
L234 :;
    }L235 :;
    ;
    if ((ax_lex_lxsymbol==(i64)17)) {
        msysnewc_m_print_startfile(dev);
        msysnewc_m_print_str((*ax_lex_lxsymptr).name,NULL);
        msysnewc_m_print_end();
        ;
    }else if ((ax_lex_lxsymbol==(i64)14)) {
        msysnewc_m_print_startfile(dev);
        msysnewc_m_print_i64(ax_lex_lxvalue,NULL);
        msysnewc_m_print_end();
        ;
    }else if ((ax_lex_lxsymbol==(i64)15)) {
        msysnewc_m_print_startfile(dev);
        msysnewc_m_print_r64(ax_lex_lxxvalue,NULL);
        msysnewc_m_print_end();
        ;
    }else if ((ax_lex_lxsymbol==(i64)16)) {
        msysnewc_m_print_startfile(dev);
        msysnewc_m_print_str((byte*)"\"",NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_str(ax_lex_lxsvalue,NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_str((byte*)"\"",NULL);
        msysnewc_m_print_end();
        ;
    }else if ((ax_lex_lxsymbol==(i64)1)) {
        msysnewc_m_print_startfile(dev);
        msysnewc_m_print_i64(ax_lex_lxvalue,NULL);
        msysnewc_m_print_end();
        ;
    } else {
        msysnewc_m_print_startfile(dev);
        msysnewc_m_print_str(ax_tables_symbolnames[(ax_lex_lxsymbol)-1],NULL);
        msysnewc_m_print_end();
        ;
        if (!!(ax_lex_lxsubcode)) {
            msysnewc_m_print_startcon();
            msysnewc_m_print_str((byte*)" ",NULL);
            msysnewc_m_print_nogap();
            msysnewc_m_print_i64(ax_lex_lxsubcode,NULL);
            msysnewc_m_print_end();
            ;
        };
    };
    msysnewc_m_print_startfile(dev);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

static void ax_lex_clearhashtable(void) {
}

static void ax_lex_inithashtable(void) {
    byte str[32];
    i64 i;
    i64 av_1;
    i64 av_2;
    i64 av_3;
    i64 av_4;
    i64 av_5;
    i64 av_6;
    i64 av_7;
    i64 av_8;
    i64 av_9;
    i64 av_10;
    if (((i64)65536 > (i64)65536)) {
    };
    ax_lex_clearhashtable();
    L236 :;
    for (i=(i64)1;i<=(i64)144;i+=(i64)1) {
L237 :;
        ax_lex_addreservedword((ax_tables_mclnames[(i)-1] + (i64)2),(i64)23,i);
L238 :;
    }L239 :;
    ;
    L240 :;
    for (i=(i64)1;i<=(i64)136;i+=(i64)1) {
L241 :;
        ax_lex_addreservedword(ax_tables_dregnames[(i)-1],(i64)24,(i64)(ax_tables_regindices[(i)-1]));
        (*ax_lex_lxsymptr).regsize = (u64)(ax_tables_regsizes[(i)-1]);
L242 :;
    }L243 :;
    ;
    L244 :;
    for (i=(i64)1;i<=(i64)16;i+=(i64)1) {
L245 :;
        ax_lex_addreservedword(ax_tables_xregnames[(i)-1],(i64)25,i);
L246 :;
    }L247 :;
    ;
    L248 :;
    for (i=(i64)1;i<=(i64)8;i+=(i64)1) {
L249 :;
        ax_lex_addreservedword(ax_tables_fregnames[(i)-1],(i64)26,i);
L250 :;
    }L251 :;
    ;
    L252 :;
    for (i=(i64)1;i<=(i64)8;i+=(i64)1) {
L253 :;
        ax_lex_addreservedword(ax_tables_mregnames[(i)-1],(i64)27,i);
L254 :;
    }L255 :;
    ;
    L256 :;
    for (i=(i64)1;i<=(i64)18;i+=(i64)1) {
L257 :;
        ax_lex_addreservedword(ax_tables_jmpccnames[(i)-1],(i64)28,(i64)(ax_tables_jmpcccodes[(i)-1]));
L258 :;
    }L259 :;
    ;
    L260 :;
    for (i=(i64)1;i<=(i64)18;i+=(i64)1) {
L261 :;
        ax_lex_addreservedword(ax_tables_setccnames[(i)-1],(i64)29,(i64)(ax_tables_setcccodes[(i)-1]));
L262 :;
    }L263 :;
    ;
    L264 :;
    for (i=(i64)1;i<=(i64)18;i+=(i64)1) {
L265 :;
        ax_lex_addreservedword(ax_tables_cmovccnames[(i)-1],(i64)30,(i64)(ax_tables_cmovcccodes[(i)-1]));
L266 :;
    }L267 :;
    ;
    L268 :;
    for (i=(i64)1;i<=(i64)8;i+=(i64)1) {
L269 :;
        ax_lex_addreservedword(ax_tables_prefixnames[(i)-1],(i64)31,(i64)(ax_tables_prefixsizes[(i)-1]));
L270 :;
    }L271 :;
    ;
    L272 :;
    for (i=(i64)1;i<=(i64)5;i+=(i64)1) {
L273 :;
        strcpy((i8 *)(str),(i8 *)(ax_tables_segmentnames[(i)-1]));
        str[(((i64)(strlen((i8 *)(str))) - (i64)3))-1] = (u64)0u;
        ax_lex_addreservedword(mlib_pcm_copyheapstring(str),(i64)32,i);
L274 :;
    }L275 :;
    ;
    ax_lex_addreservedword((byte*)"aframe",(i64)24,(i64)15);
    (*ax_lex_lxsymptr).regsize = (u64)((i64)4);
    ax_lex_addreservedword((byte*)"dframe",(i64)24,(i64)15);
    (*ax_lex_lxsymptr).regsize = (u64)((i64)8);
    ax_lex_addreservedword((byte*)"astack",(i64)24,(i64)16);
    (*ax_lex_lxsymptr).regsize = (u64)((i64)4);
    ax_lex_addreservedword((byte*)"dstack",(i64)24,(i64)16);
    (*ax_lex_lxsymptr).regsize = (u64)((i64)8);
    ax_lex_addreservedword((byte*)"dprog",(i64)24,(i64)9);
    (*ax_lex_lxsymptr).regsize = (u64)((i64)8);
    ax_lex_addreservedword((byte*)"dsptr",(i64)24,(i64)10);
    (*ax_lex_lxsymptr).regsize = (u64)((i64)8);
}

static void ax_lex_addreservedword(byte * name,i64 symbol,i64 subcode) {
    ax_lex_lxhashvalue = ax_lex_gethashvalue(name);
    if (!!(ax_lex_lookuplex(name,(i64)0))) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"NAME=",NULL);
        msysnewc_m_print_str(name,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        ax_lex_lxerror((byte*)"DUPL NAME");
    };
    (*ax_lex_lxsymptr).symbol = (u64)((i64)0);
    (*ax_lex_lxsymptr).ksymbol = (u64)(symbol);
    (*ax_lex_lxsymptr).subcode = (u64)(subcode);
}

void ax_lex_printhashtable(void * devx,byte * caption) {
    struct ax_decls_strec *  r;
    i64 count;
    i64 i;
    i64 av_1;
    msysnewc_m_print_startfile(devx);
    msysnewc_m_print_str(caption,NULL);
    msysnewc_m_print_str((byte*)":",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    count = (i64)0;
    L276 :;
    for (i=(i64)0;i<=(i64)65535;i+=(i64)1) {
L277 :;
        r = ax_decls_lexhashtable[(i)];
        if ((!!(r) && !!((*r).name))) {
            count += (i64)1;
        };
L278 :;
    }L279 :;
    ;
    msysnewc_m_print_startfile(devx);
    msysnewc_m_print_i64(count,NULL);
    msysnewc_m_print_str((byte*)" items in table",NULL);
    msysnewc_m_print_i64((i64)65536,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

static i64 ax_lex_lookuplex(byte * name,i64 length) {
    i64 j;
    i64 wrapped;
    i64 insource;
    i64 firstj;
    insource = length;
    if ((length == (i64)0)) {
        length = (i64)(strlen((i8 *)(name)));
    };
    firstj = (j = (ax_lex_lxhashvalue & (i64)65535));
    wrapped = (i64)0;
    L280 :;
    while (1) {
        ax_lex_lxsymptr = ax_decls_lexhashtable[(j)];
        if ((ax_lex_lxsymptr == 0)) {
            goto L281 ;
        };
        if ((((i64)((u64)((*ax_lex_lxsymptr).namelen)) == length) && ((i64)(memcmp((void *)((*ax_lex_lxsymptr).name),(void *)(name),(u64)(length))) == (i64)0))) {
            return (i64)1;
        };
        if ((++j > (i64)65536)) {
            if (!!(wrapped)) {
                msysnewc_m_print_startcon();
                msysnewc_m_print_str((byte*)"???????HASHTABLE FULL",NULL);
                msysnewc_m_print_i64((i64)65536,NULL);
                msysnewc_m_print_i64(ax_decls_lxlineno,NULL);
                msysnewc_m_print_newline();
                msysnewc_m_print_end();
                ;
                exit((i64)1);
            };
            wrapped = (i64)1;
            j = (i64)1;
        };
    }L281 :;
    ;
    if (!!(insource)) {
        name = ax_lex_makestring(name,length);
    };
    if ((ax_lex_lxsymptr == 0)) {
        ax_lex_lxsymptr = (struct ax_decls_strec *)(mlib_pcm_allocz((i64)128));
        ax_decls_lexhashtable[(j)] = ax_lex_lxsymptr;
    };
    (*ax_lex_lxsymptr).name = name;
    (*ax_lex_lxsymptr).namelen = (u64)(length);
    (*ax_lex_lxsymptr).symbol = (u64)((i64)17);
    (*ax_lex_lxsymptr).ksymbol = (u64)((i64)0);
    (*ax_lex_lxsymptr).htindex = (u64)(j);
    (*ax_lex_lxsymptr).htfirstindex = (u64)(firstj);
    (*ax_lex_lxsymptr).moduleno = ax_decls_currmoduleno;
    return (i64)0;
}

void ax_lex_initsourcefile(byte * source) {
    ax_lex_lxstart = (ax_lex_lxsptr = (byte *)(source));
    ax_decls_lxlineno = (i64)1;
}

struct ax_decls_strec * ax_lex_addnamestr(byte * name) {
    ax_lex_lxhashvalue = ax_lex_gethashvalue(name);
    ax_lex_lookuplex(mlib_pcm_copyheapstring(name),(i64)0);
    return ax_lex_lxsymptr;
}

void ax_lex_lxerror(byte * m) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_setfmt((byte*)"\r\n\r\n Lexical Error\n*** # *** on line #");
    msysnewc_m_print_str(m,NULL);
    msysnewc_m_print_i64(ax_decls_lxlineno,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    exit((i64)1);
}

static i64 ax_lex_gethashvalue(byte * s) {
    i64 hsum;
    i64 csum;
    i64 c;
    hsum = (csum = (i64)0);
    L282 :;
    while (!!((c = (i64)((*s++))))) {
        csum += c;
        hsum = ((hsum << (i64)3) + csum);
L283 :;
    }L284 :;
    ;
    return ((hsum << (i64)5) ^ csum);
}

void ax_lex_skiptoeol(void) {
    L285 :;
    do {
        ax_lex_lex();
L286 :;
    } while (!((ax_lex_lxsymbol == (i64)11) || (ax_lex_lxsymbol == (i64)12)));L287 :;
    ;
}

static byte * ax_lex_makestring(byte * p,i64 length) {
    byte *  s;
    s = (byte *)(mlib_pcm_alloc((length + (i64)1)));
    memcpy((void *)(s),(void *)(p),(u64)(length));
    (*(s + length)) = (u64)0u;
    return s;
}

void ax_parse_readmodule(i64 moduleno) {
    struct ax_decls_strec *  symptr;
    i64 sym;
    ax_lex_initsourcefile(ax_decls_moduletable[(moduleno)-1].source);
    ax_lex_lxsymbol = (i64)11;
    ax_lib_genmc((i64)115,ax_lib_genint((i64)1,(i64)4),(struct ax_decls_opndrec *)(0));
    L288 :;
    while ((ax_lex_lxsymbol == (i64)11)) {
        ax_lex_lex();
        switch (ax_lex_lxsymbol) {
        case 23:;
        {
            ax_parse_readinstr();
        }break;
        case 17:;
        {
            symptr = ax_lex_lxsymptr;
            ax_lex_lex();
            sym = ax_lex_lxsymbol;
            if ((sym==(i64)10)) {
                ax_lex_lex();
                if ((ax_lex_lxsymbol==(i64)24)) {
                    ax_lib_createregalias(symptr,(i64)((*ax_lex_lxsymptr).subcode),(i64)((*ax_lex_lxsymptr).regsize));
                    ax_lex_lex();
                }else if ((ax_lex_lxsymbol==(i64)25)) {
                    ax_lib_createxregalias(symptr,(i64)((*ax_lex_lxsymptr).subcode));
                    ax_lex_lex();
                } else {
                    ax_lib_createnamedconst(symptr,ax_parse_readexpression());
                };
            }else if ((sym==(i64)3) || (sym==(i64)4)) {
                ax_lib_createlabel(symptr,((sym == (i64)3)?(i64)20:(i64)22));
                ax_lib_genmc((i64)4,ax_lib_genlab(symptr,(i64)4),(struct ax_decls_opndrec *)(0));
                (*symptr).reftype = (u64)((i64)1);
                ax_lex_lxsymbol = (i64)11;
                goto L288 ;
            } else {
                msysnewc_m_print_startcon();
                msysnewc_m_print_str((*symptr).name,NULL);
                msysnewc_m_print_newline();
                msysnewc_m_print_end();
                ;
                ax_lib_serror((byte*)"colon expected after label");
            };
        }break;
        case 19:;
        {
            symptr = ax_lex_lxsymptr;
            ax_lex_lex();
            if ((ax_lex_lxsymbol==(i64)10)) {
                ax_lib_serror_s((byte*)"Redefining label as const: %s",(*symptr).name);
            }else if ((ax_lex_lxsymbol==(i64)3) || (ax_lex_lxsymbol==(i64)4)) {
                (*symptr).fwdrefs = (struct ax_decls_fwdrec *)(0);
                ax_lib_genmc((i64)4,ax_lib_genlab(symptr,(i64)4),(struct ax_decls_opndrec *)(0));
                (*symptr).symbol = ((ax_lex_lxsymbol == (i64)3)?(u64)((i64)20):(u64)((i64)22));
                (*symptr).reftype = (u64)((i64)1);
                ax_lex_lxsymbol = (i64)11;
                goto L288 ;
            } else {
                ax_lib_serror((byte*)"Instruction expected");
            };
        }break;
        case 21:;
        {
            ax_lib_serror_s((byte*)"Defining imported name: %s",(*symptr).name);
        }break;
        case 20:;
        case 22:;
        {
            ax_lib_serror_s((byte*)"Redefining symbol: %s",(*symptr).name);
        }break;
        case 18:;
        {
            ax_lib_serror_s((byte*)"2:Const redefined: %s",(*symptr).name);
        }break;
        case 28:;
        {
            ax_parse_readcondinstr((i64)23);
        }break;
        case 29:;
        {
            ax_parse_readcondinstr((i64)55);
        }break;
        case 30:;
        {
            ax_parse_readcondinstr((i64)13);
        }break;
        case 11:;
        {
        }break;
        case 12:;
        {
            return;
        }break;
        default: {
            msysnewc_m_print_startcon();
            msysnewc_m_print_str((byte*)"Unknown symbol:",NULL);
            msysnewc_m_print_str(ax_tables_symbolnames[(ax_lex_lxsymbol)-1],NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
        }
        } //SW
;
L289 :;
    }L290 :;
    ;
    ax_lib_serror((byte*)"EOL expected");
}

void ax_parse_checkundefined(void) {
    struct ax_decls_strec *  d;
    d = ax_decls_modulenamelist;
    L291 :;
    while (!!(d)) {
        if (((i64)((u64)((*d).symbol)) == (i64)19)) {
            msysnewc_m_print_startcon();
            msysnewc_m_print_str((byte*)"Undefined:",NULL);
            msysnewc_m_print_str(mlib_padstr((*d).name,(i64)20,(byte*)" "),NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            ++ax_decls_nundefined;
        };
        d = (*d).nextdef;
L292 :;
    }L293 :;
    ;
}

static void ax_parse_checksymbol(i64 symbol) {
    byte str[265];
    if ((ax_lex_lxsymbol != symbol)) {
        msysnewc_m_print_startstr(str);
        msysnewc_m_print_setfmt((byte*)"# expected not #");
        msysnewc_m_print_str(ax_tables_symbolnames[(symbol)-1],NULL);
        msysnewc_m_print_str(ax_tables_symbolnames[(ax_lex_lxsymbol)-1],NULL);
        msysnewc_m_print_end();
        ;
        ax_lib_serror(str);
    };
}

static void ax_parse_readinstr(void) {
    i64 opcode;
    struct ax_decls_opndrec *  a;
    struct ax_decls_opndrec *  b;
    struct ax_decls_opndrec *  c;
    opcode = ax_lex_lxsubcode;
    ax_lex_lex();
    switch (opcode) {
    case 110:;
    case 111:;
    case 112:;
    case 113:;
    case 114:;
    {
        L294 :;
        while (1) {
            if ((ax_lex_lxsymbol == (i64)16)) {
                a = ax_lib_genstrimm(ax_lex_lxsvalue);
                ax_lex_lex();
                ax_lib_genmc(opcode,a,(struct ax_decls_opndrec *)(0));
            } else {
                a = ax_parse_readoperand();
                ax_lib_genmc(opcode,a,(struct ax_decls_opndrec *)(0));
            };
            if ((ax_lex_lxsymbol == (i64)2)) {
                ax_lex_lex();
            } else {
                goto L295 ;
            };
        }L295 :;
        ;
    }break;
    case 115:;
    {
        ax_parse_checksymbol((i64)32);
        ax_lib_genmc((i64)115,ax_lib_genint(ax_lex_lxsubcode,(i64)4),(struct ax_decls_opndrec *)(0));
        ax_lex_lex();
    }break;
    case 116:;
    {
        ax_lib_genmc((i64)115,ax_lib_genint((i64)2,(i64)4),(struct ax_decls_opndrec *)(0));
    }break;
    case 117:;
    {
        ax_lib_genmc((i64)115,ax_lib_genint((i64)3,(i64)4),(struct ax_decls_opndrec *)(0));
    }break;
    case 118:;
    {
        ax_lib_genmc((i64)115,ax_lib_genint((i64)1,(i64)4),(struct ax_decls_opndrec *)(0));
    }break;
    case 32:;
    {
        a = ax_parse_readoperand();
        ax_parse_checksymbol((i64)2);
        ax_lex_lex();
        b = ax_parse_readoperand();
        ax_parse_checksymbol((i64)2);
        ax_lex_lex();
        c = ax_parse_readoperand();
        ax_lib_serror((byte*)"IMUL3 CAN'T DO 3 OPNDS");
    }break;
    case 86:;
    case 87:;
    {
        a = ax_parse_readoperand();
        ax_parse_checksymbol((i64)2);
        ax_lex_lex();
        b = ax_parse_readoperand();
        ax_parse_checksymbol((i64)2);
        ax_lex_lex();
        c = ax_parse_readoperand();
        if (((i64)((u64)((*c).mode)) != (i64)2)) {
            ax_lib_serror((byte*)"pcmpistr/not int");
        };
        ax_lib_genmc(opcode,a,b);
        (*ax_lib_mccodex).c = (u64)((*c).value);
    }break;
    case 8:;
    {
        L296 :;
        do {
            ax_lex_lex();
L297 :;
        } while (!(ax_lex_lxsymbol == (i64)11));L298 :;
        ;
    }break;
    default: {
        a = (b = (struct ax_decls_opndrec *)(0));
        if ((ax_lex_lxsymbol != (i64)11)) {
            a = ax_parse_readoperand();
            if ((ax_lex_lxsymbol == (i64)2)) {
                ax_lex_lex();
                b = ax_parse_readoperand();
            };
        };
        ax_lib_genmc(opcode,a,b);
    }
    } //SW
;
}

static void ax_parse_readcondinstr(i64 opc) {
    struct ax_decls_opndrec *  a;
    struct ax_decls_opndrec *  b;
    a = ax_lib_genint(ax_lex_lxsubcode,(i64)4);
    ax_lex_lex();
    b = ax_parse_readoperand();
    if (((ax_lex_lxsymbol == (i64)2) && (opc == (i64)13))) {
        ax_lib_genmc((i64)6,b,(struct ax_decls_opndrec *)(0));
        ax_lex_lex();
        b = ax_parse_readoperand();
    };
    ax_lib_genmc(opc,a,b);
}

static struct ax_decls_opndrec * ax_parse_readoperand(void) {
    struct ax_decls_opndrec *  p;
    i64 size;
    switch (ax_lex_lxsymbol) {
    case 24:;
    {
        p = ax_lib_regtable[(ax_lex_lxsubcode)-1][((i64)((*ax_lex_lxsymptr).regsize))-1];
        ax_lex_lex();
        return p;
    }break;
    case 5:;
    {
        ax_lex_lex();
        return ax_parse_readaddrmode((i64)0);
    }break;
    case 25:;
    {
        p = ax_lib_genxreg(ax_lex_lxsubcode);
        ax_lex_lex();
        return p;
    }break;
    case 31:;
    {
        size = ax_lex_lxsubcode;
        ax_lex_lex();
        ax_parse_checksymbol((i64)5);
        ax_lex_lex();
        return ax_parse_readaddrmode(size);
    }break;
    default: {
        return ax_parse_readexpression();
    }
    } //SW
;
    return (struct ax_decls_opndrec *)(0);
}

static struct ax_decls_opndrec * ax_parse_readexpression(void) {
    struct ax_decls_strec *  labelx;
    i64 valuex;
    i64 typex;
    ax_parse_readterm();
    L299 :;
    if ((ax_lex_lxsymbol==(i64)7)) {
        labelx = ax_parse_exprlabeldef;
        valuex = ax_parse_exprvalue;
        typex = ax_parse_exprtype;
        ax_lex_lex();
        ax_parse_readterm();
        if (!!(ax_parse_exprlabeldef)) {
            ax_lib_serror((byte*)"+label?");
        };
        ax_parse_exprlabeldef = labelx;
        if ((!!(typex) || !!(ax_parse_exprtype))) {
            ax_lib_serror((byte*)"add real");
        };
        ax_parse_exprvalue += valuex;
    }else if ((ax_lex_lxsymbol==(i64)8)) {
        labelx = ax_parse_exprlabeldef;
        valuex = ax_parse_exprvalue;
        typex = ax_parse_exprtype;
        ax_lex_lex();
        ax_parse_readterm();
        if (!!(ax_parse_exprlabeldef)) {
            ax_lib_serror((byte*)"+label?");
        };
        ax_parse_exprlabeldef = labelx;
        if ((!!(typex) || !!(ax_parse_exprtype))) {
            ax_lib_serror((byte*)"sub real");
        };
        ax_parse_exprvalue = (valuex - ax_parse_exprvalue);
    } else {
        goto L300 ;
    }goto L299 ;
L300 :;
    ;
    return ax_lib_genimm_expr(ax_parse_exprlabeldef,ax_parse_exprvalue,ax_parse_exprtype,(i64)4);
}

static void ax_parse_readterm(void) {
    struct ax_decls_strec *  symptr;
    double x;
    ax_parse_exprlabeldef = (struct ax_decls_strec *)(0);
    ax_parse_exprvalue = (i64)0;
    ax_parse_exprtype = (i64)0;
    switch (ax_lex_lxsymbol) {
    case 19:;
    case 20:;
    case 22:;
    {
        ax_parse_exprlabeldef = ax_lex_lxsymptr;
        ax_lex_lex();
        if ((ax_lex_lxsymbol == (i64)9)) {
            ax_lib_serror((byte*)"* applied to non-extern label or applied inconsistently");
        };
    }break;
    case 21:;
    {
        ax_parse_exprlabeldef = ax_lex_lxsymptr;
        ax_lex_lex();
        if ((ax_lex_lxsymbol != (i64)9)) {
            ax_lib_serror((byte*)"* missing or applied inconsistently");
        };
        ax_lex_lex();
    }break;
    case 18:;
    {
        ax_parse_exprlabeldef = (*(*ax_lex_lxsymptr).expr).labeldef;
        ax_parse_exprvalue = (*(*ax_lex_lxsymptr).expr).value;
        ax_parse_exprtype = (i64)((*(*ax_lex_lxsymptr).expr).valtype);
        ax_lex_lex();
    }break;
    case 17:;
    {
        symptr = ax_lex_lxsymptr;
        ax_parse_exprlabeldef = symptr;
        ax_lex_lex();
        if ((ax_lex_lxsymbol == (i64)9)) {
            ax_lib_createlabel(symptr,(i64)21);
            ax_lex_lex();
        } else {
            ax_lib_createlabel(symptr,(i64)19);
        };
    }break;
    case 14:;
    {
        ax_parse_exprvalue = ax_lex_lxvalue;
        ax_lex_lex();
    }break;
    case 15:;
    {
        ax_parse_exprvalue = *(i64*)&ax_lex_lxxvalue;
        ax_parse_exprtype = (i64)82;
        ax_lex_lex();
    }break;
    case 8:;
    {
        ax_lex_lex();
        ax_parse_readterm();
        if (!(!!(ax_parse_exprlabeldef))) {
            if (!(!!(ax_parse_exprtype))) {
                ax_parse_exprvalue = -(ax_parse_exprvalue);
            } else {
                x = -(*(double*)&ax_parse_exprvalue);
                ax_parse_exprvalue = *(i64*)&x;
            };
        } else {
            ax_lib_serror((byte*)"neg/label");
        };
    }break;
    case 7:;
    {
        ax_lex_lex();
        ax_parse_readterm();
    }break;
    default: {
        ax_lib_serror((byte*)"READTERM");
    }
    } //SW
;
}

static void ax_parse_readreg(i64 * reg,i64 * regsize,i64 * scale) {
    (*reg) = ax_lex_lxsubcode;
    (*regsize) = (i64)((*ax_lex_lxsymptr).regsize);
    ax_lex_lex();
    if ((ax_lex_lxsymbol == (i64)9)) {
        ax_lex_lex();
        ax_parse_checksymbol((i64)14);
        if ((ax_lex_lxvalue==(i64)1) || (ax_lex_lxvalue==(i64)2) || (ax_lex_lxvalue==(i64)4) || (ax_lex_lxvalue==(i64)8)) {
        } else {
            ax_lib_serror((byte*)"*n must be 1,2,4,8");
        };
        (*scale) = ax_lex_lxvalue;
        ax_lex_lex();
    } else {
        (*scale) = (i64)0;
    };
}

static struct ax_decls_opndrec * ax_parse_readaddrmode(i64 size) {
    i64 reg;
    i64 regsize;
    i64 scale;
    i64 regix;
    i64 regixsize;
    i64 scaleix;
    struct ax_decls_opndrec *  x;
    struct ax_decls_opndrec *  p;
    reg = (regix = (i64)0);
    regsize = (regixsize = (i64)0);
    scale = (scaleix = (i64)0);
    x = (struct ax_decls_opndrec *)(0);
    if ((ax_lex_lxsymbol == (i64)24)) {
        ax_parse_readreg(&reg,&regsize,&scale);
        if ((ax_lex_lxsymbol==(i64)7)) {
            ax_lex_lex();
            if ((ax_lex_lxsymbol == (i64)24)) {
                ax_parse_readreg(&regix,&regixsize,&scaleix);
                if ((ax_lex_lxsymbol==(i64)7) || (ax_lex_lxsymbol==(i64)8)) {
                    x = ax_parse_readexpression();
                };
            } else {
                x = ax_parse_readexpression();
            };
        }else if ((ax_lex_lxsymbol==(i64)8)) {
            x = ax_parse_readexpression();
        };
    } else {
        x = ax_parse_readexpression();
    };
    if ((!!(scale) && !!(scaleix))) {
        ax_lib_serror((byte*)"Two *N scales");
    };
    if ((((reg == (i64)0) && (regix == (i64)0)) && !!((i64)0))) {
        ax_lib_serror((byte*)"Empty address mode");
    };
    ax_parse_checksymbol((i64)6);
    ax_lex_lex();
    if ((!!(scale) && !(!!(scaleix)))) {
        {i64 temp = reg; reg = regix; regix = temp; };
        {i64 temp = regsize; regsize = regixsize; regixsize = temp; };
        {i64 temp = scale; scale = scaleix; scaleix = temp; };
    };
    if ((scaleix == (i64)0)) {
        scaleix = (i64)1;
    };
    if (((!!(regsize) && !!(regixsize)) && (regsize != regixsize))) {
        ax_lib_serror((byte*)"Addr reg size mismatch");
    };
    p = ax_lib_genindex(reg,regix,scaleix,x,size,(((regsize == (i64)4) || (regixsize == (i64)4))?(i64)4:(i64)8));
    return p;
}

void ax_lib_initlib(void) {
    i64 reg;
    i64 size;
    ax_lib_zero_opnd = ax_lib_genint((i64)0,(i64)4);
    L301 :;
    for (reg=(i64)1;reg<=(i64)16;reg+=(i64)1) {
L302 :;
        L305 :;
        for (size=(i64)1;size<=(i64)8;size+=(i64)1) {
L306 :;
            if ((size==(i64)1) || (size==(i64)2) || (size==(i64)4) || (size==(i64)8)) {
                ax_lib_regtable[(reg)-1][(size)-1] = ax_lib_genreg0(reg,size);
            };
L307 :;
        }L308 :;
        ;
L303 :;
    }L304 :;
    ;
    L309 :;
    for (reg=(i64)17;reg<=(i64)20;reg+=(i64)1) {
L310 :;
        ax_lib_regtable[(reg)-1][((i64)1)-1] = ax_lib_genreg0(reg,(i64)1);
L311 :;
    }L312 :;
    ;
    ax_decls_ss_symboltable = (struct ax_decls_strec * (*)[])(mlib_pcm_alloc((i64)131072));
    ax_decls_ss_symboltablesize = (i64)16384;
    ax_decls_ss_nsymbols = (i64)0;
}

void ax_lib_genmc(i64 opcode,struct ax_decls_opndrec * a,struct ax_decls_opndrec * b) {
    struct ax_lib_mclrec *  m;
    i64 nopnds;
    m = (struct ax_lib_mclrec *)(mlib_pcm_alloc((i64)36));
    ++ax_decls_nmclasm;
    (*m).nextmcl = (struct ax_lib_mclrec *)(0);
    if ((ax_lex_lxsymbol == (i64)11)) {
        (*m).lineno = (ax_decls_lxlineno - (i64)1);
    } else {
        (*m).lineno = ax_decls_lxlineno;
    };
    (*m).opcode = (u64)(opcode);
    nopnds = ((a == 0)?(i64)0:((b == 0)?(i64)1:(i64)2));
    if (((nopnds == (i64)2) && ((opcode == (i64)86) || (opcode == (i64)87)))) {
        nopnds = (i64)3;
    };
    if ((nopnds < (i64)((u64)(ax_tables_mclnopnds[(opcode)-1])))) {
        ax_lib_serror((byte*)"Too few operands");
    } else if ((nopnds > (i64)((u64)(ax_tables_mclnopnds[(opcode)-1])))) {
        ax_lib_serror((byte*)"Too many operands");
    };
    (*m).a = a;
    (*m).b = b;
    if (!!(ax_lib_mccode)) {
        (*ax_lib_mccodex).nextmcl = m;
        ax_lib_mccodex = m;
    } else {
        ax_lib_mccode = (ax_lib_mccodex = m);
    };
}

void ax_lib_genmcstr(i64 opcode,byte * s) {
    ax_lib_genmc(opcode,ax_lib_genstrimm(s),(struct ax_decls_opndrec *)(0));
}

static struct ax_decls_opndrec * ax_lib_newopnd(i64 mode) {
    struct ax_decls_opndrec *  a;
    ++ax_decls_nmclopndsasm;
    a = (struct ax_decls_opndrec *)(mlib_pcm_allocz((i64)24));
    (*a).mode = (u64)(mode);
    return a;
}

struct ax_decls_opndrec * ax_lib_genxreg(i64 xreg) {
    struct ax_decls_opndrec *  a;
    a = ax_lib_newopnd((i64)5);
    (*a).reg = (u64)(xreg);
    (*a).size = (u64)((i64)16);
    return a;
}

struct ax_decls_opndrec * ax_lib_genindex(i64 areg,i64 ireg,i64 scale,struct ax_decls_opndrec * x,i64 size,i64 addrsize) {
    struct ax_decls_opndrec *  a;
    if (!!(x)) {
        a = x;
        (*x).mode = (u64)((i64)3);
    } else {
        a = ax_lib_newopnd((i64)3);
    };
    (*a).reg = (u64)(areg);
    (*a).regix = (u64)(ireg);
    (*a).scale = (u64)(scale);
    (*a).size = (u64)(size);
    (*a).addrsize = (u64)(addrsize);
    return a;
}

struct mlib_strbuffer * ax_lib_writemclblock(void) {
    i64 i;
    struct ax_lib_mclrec *  m;
    mlib_gs_init(ax_lib_dest);
    mlib_gs_strln(ax_lib_dest,(byte*)"MC CODE");
    m = ax_lib_mccode;
    i = (i64)1;
    L313 :;
    while (!!(m)) {
        ax_lib_writemcl(i,m);
        m = (*m).nextmcl;
        ++i;
L314 :;
    }L315 :;
    ;
    return ax_lib_dest;
}

void ax_lib_gencomment(byte * s) {
    if ((s == 0)) {
        ax_lib_genmc((i64)2,(struct ax_decls_opndrec *)(0),(struct ax_decls_opndrec *)(0));
    } else {
        ax_lib_genmcstr((i64)1,s);
    };
}

struct ax_decls_opndrec * ax_lib_genstrimm(byte * s) {
    struct ax_decls_opndrec *  a;
    a = ax_lib_newopnd((i64)6);
    (*a).svalue = s;
    return a;
}

static byte * ax_lib_getsizetag(i64 size) {
    if ((size==(i64)1)) {
        return (byte*)"b";
    }else if ((size==(i64)2)) {
        return (byte*)"h";
    }else if ((size==(i64)4)) {
        return (byte*)"w";
    }else if ((size==(i64)8)) {
        return (byte*)"d";
    };
    ax_lib_gerror((byte*)"GETSIZETAG?");
    return (byte *)(0);
}

static void ax_lib_writemcl(i64 index,struct ax_lib_mclrec * mcl) {
    byte mclstr[512];
    byte str[512];
    byte *  semi;
    strcpy((i8 *)(mclstr),(i8 *)(ax_lib_strmcl(mcl)));
    if (((i64)(mclstr[((i64)1)-1]) == (i64)0)) {
        return;
    };
    if (((i64)((*mcl).opcode)==(i64)1)) {
        semi = (byte*)";";
    } else {
        semi = (byte*)" ";
    };
    msysnewc_m_print_startstr(str);
    msysnewc_m_print_str(semi,(byte*)"z3");
    msysnewc_m_print_i64(index,(byte*)"z4");
    msysnewc_m_print_nogap();
    msysnewc_m_print_str((byte*)" ",NULL);
    msysnewc_m_print_end();
    ;
    mlib_gs_str(ax_lib_dest,str);
    mlib_gs_strln(ax_lib_dest,mclstr);
}

byte * ax_lib_strmcl(struct ax_lib_mclrec * mcl) {
    static byte str[512];
    byte str2[128];
    i64 opcode;
    i64 sizepref;
    opcode = (i64)((*mcl).opcode);
    if ((opcode==(i64)7)) {
        return (*(*mcl).a).svalue;
    }else if ((opcode==(i64)2)) {
        return (byte*)"";
    }else if ((opcode==(i64)1)) {
        strcpy((i8 *)(str),(i8 *)((byte*)";"));
        strcat((i8 *)(str),(i8 *)((*(*mcl).a).svalue));
        return str;
    }else if ((opcode==(i64)4)) {
        strcpy((i8 *)(str),(i8 *)((*(*(*mcl).a).labeldef).name));
        strcat((i8 *)(str),(i8 *)((byte*)":"));
        return str;
    };
    strcpy((i8 *)(str),(i8 *)((byte*)"\t\t"));
    if ((opcode==(i64)23)) {
        strcat((i8 *)(str),(i8 *)((byte*)"j"));
        strcat((i8 *)(str),(i8 *)(ax_tables_condnames[((*(*mcl).a).value)]));
    }else if ((opcode==(i64)55)) {
        strcat((i8 *)(str),(i8 *)((byte*)"set"));
        strcat((i8 *)(str),(i8 *)(ax_tables_condnames[((*(*mcl).a).value)]));
    }else if ((opcode==(i64)13)) {
        strcat((i8 *)(str),(i8 *)((byte*)"cmov"));
        strcat((i8 *)(str),(i8 *)(ax_tables_condnames[((*(*mcl).a).value)]));
    } else {
        strcat((i8 *)(str),(i8 *)((ax_tables_mclnames[(opcode)-1] + (i64)2)));
    };
    mlib_ipadstr(str,(i64)12,(byte*)" ");
    if ((!!((*mcl).a) && !!((*mcl).b))) {
        sizepref = ax_lib_needsizeprefix((i64)((*mcl).opcode),(*mcl).a,(*mcl).b);
        strcat((i8 *)(str),(i8 *)(ax_lib_stropnd((*mcl).a,sizepref)));
        strcat((i8 *)(str),(i8 *)((byte*)",\t"));
        strcat((i8 *)(str),(i8 *)(ax_lib_stropnd((*mcl).b,sizepref)));
    } else if (!!((*mcl).a)) {
        if (((i64)((u64)((*mcl).opcode)) == (i64)19)) {
            strcat((i8 *)(str),(i8 *)(ax_lib_stropnd((*mcl).a,(i64)0)));
        } else {
            strcat((i8 *)(str),(i8 *)(ax_lib_stropnd((*mcl).a,(i64)1)));
        };
    };
    if ((opcode==(i64)86) || (opcode==(i64)87)) {
        msysnewc_m_print_startstr(str2);
        msysnewc_m_print_setfmt((byte*)", #");
        msysnewc_m_print_u64((*mcl).c,NULL);
        msysnewc_m_print_end();
        ;
        strcat((i8 *)(str),(i8 *)(str2));
    };
    return str;
}

byte * ax_lib_stropnd(struct ax_decls_opndrec * a,i64 sizeprefix) {
    static byte str[256];
    byte *  plus;
    byte *  s;
    i64 value;
    struct ax_decls_strec *  d;
    if (((i64)((*a).mode)==(i64)1)) {
        return ax_lib_getregname((i64)((*a).reg),(i64)((*a).size));
    }else if (((i64)((*a).mode)==(i64)2)) {
        d = (*a).labeldef;
        value = (*a).value;
        if (!!(d)) {
            if (((i64)((u64)((*d).symbol)) == (i64)18)) {
                return ax_lib_inttostr((*(*d).expr).value);
            };
            s = ax_lib_getfullname(d);
            if (!!(value)) {
                if ((value > (i64)0)) {
                    strcpy((i8 *)(str),(i8 *)(s));
                    strcat((i8 *)(str),(i8 *)((byte*)"+"));
                    strcat((i8 *)(str),(i8 *)(ax_lib_inttostr(value)));
                } else {
                    strcpy((i8 *)(str),(i8 *)(s));
                    strcat((i8 *)(str),(i8 *)(ax_lib_inttostr(value)));
                };
                return str;
            } else {
                strcpy((i8 *)(str),(i8 *)(s));
                return str;
            };
        };
        if (((i64)((u64)((*a).valtype)) == (i64)0)) {
            return ax_lib_inttostr(value);
        } else {
            return ax_lib_realtostr(*(double*)&value);
        };
    }else if (((i64)((*a).mode)==(i64)3)) {
        str[((i64)1)-1] = (u64)0u;
        strcat((i8 *)(str),(i8 *)(ax_lib_getsizeprefix((i64)((*a).size),sizeprefix)));
        strcat((i8 *)(str),(i8 *)((byte*)"["));
        plus = (byte*)"";
        if (!!((u64)((*a).reg))) {
            strcat((i8 *)(str),(i8 *)(ax_lib_getregname((i64)((*a).reg),(i64)((*a).addrsize))));
            plus = (byte*)"+";
        };
        if (!!((u64)((*a).regix))) {
            strcat((i8 *)(str),(i8 *)(plus));
            strcat((i8 *)(str),(i8 *)(ax_lib_getregname((i64)((*a).regix),(i64)((*a).addrsize))));
            plus = (byte*)"+";
            if (((i64)((u64)((*a).scale)) > (i64)1)) {
                strcat((i8 *)(str),(i8 *)((byte*)"*"));
                strcat((i8 *)(str),(i8 *)(ax_lib_inttostr((i64)((*a).scale))));
            };
        };
        if (!!((*a).labeldef)) {
            strcat((i8 *)(str),(i8 *)(plus));
            strcat((i8 *)(str),(i8 *)(ax_lib_strdef((*a).labeldef)));
            plus = (byte*)"+";
        };
        if (((*a).value > (i64)0)) {
            strcat((i8 *)(str),(i8 *)(plus));
            strcat((i8 *)(str),(i8 *)(ax_lib_inttostr((*a).value)));
        } else if (((*a).value < (i64)0)) {
            strcat((i8 *)(str),(i8 *)(ax_lib_inttostr((*a).value)));
        };
        strcat((i8 *)(str),(i8 *)((byte*)"]"));
    }else if (((i64)((*a).mode)==(i64)6)) {
        if (((i64)(strlen((i8 *)((*a).svalue))) >= (i64)256)) {
            msysnewc_m_print_startstr(str);
            msysnewc_m_print_str((byte*)"\"<Long string>\"",NULL);
            msysnewc_m_print_end();
            ;
        } else {
            msysnewc_m_print_startstr(str);
            msysnewc_m_print_str((byte*)"\"",NULL);
            msysnewc_m_print_nogap();
            msysnewc_m_print_str((*a).svalue,NULL);
            msysnewc_m_print_nogap();
            msysnewc_m_print_str((byte*)"\"",NULL);
            msysnewc_m_print_end();
            ;
        };
    }else if (((i64)((*a).mode)==(i64)4)) {
        return ax_lib_opndnames[((*a).value)];
    }else if (((i64)((*a).mode)==(i64)5)) {
        return ax_lib_xgetregname((i64)((*a).reg));
    } else {
        return (byte*)"<BAD OPND>";
    };
    return str;
}

static byte * ax_lib_strdef(struct ax_decls_strec * def) {
    if (((i64)((u64)((*def).symbol)) == (i64)18)) {
        return ax_lib_inttostr((*(*def).expr).value);
    };
    return ax_lib_getfullname(def);
}

void ax_lib_setsegment(i64 seg) {
    if ((seg == ax_lib_currsegment)) {
        return;
    };
    if ((seg==(i64)68)) {
        ax_lib_genmcstr((i64)115,(byte*)".data");
    }else if ((seg==(i64)90)) {
        ax_lib_genmcstr((i64)115,(byte*)".bss");
    }else if ((seg==(i64)67)) {
        ax_lib_genmcstr((i64)115,(byte*)".text");
    }else if ((seg==(i64)82)) {
        ax_lib_genmcstr((i64)115,(byte*)".rodata");
    };
    ax_lib_currsegment = seg;
}

static byte * ax_lib_getsizeprefix(i64 size,i64 enable) {
    if (!(!!(enable))) {
        return (byte*)"";
    };
    if ((size==(i64)1)) {
        return (byte*)"byte ";
    }else if ((size==(i64)2)) {
        return (byte*)"word ";
    }else if ((size==(i64)4)) {
        return (byte*)"dword ";
    }else if ((size==(i64)8)) {
        return (byte*)"qword ";
    }else if ((size==(i64)0)) {
        return (byte*)"";
    };
    return (byte*)"N:";
}

static i64 ax_lib_needsizeprefix(i64 opcode,struct ax_decls_opndrec * a,struct ax_decls_opndrec * b) {
    if ((opcode==(i64)16) || (opcode==(i64)17)) {
        return (i64)1;
    }else if ((opcode==(i64)80) || (opcode==(i64)81)) {
        return (i64)1;
    };
    if ((((((i64)((u64)((*a).mode)) == (i64)1) || ((i64)((u64)((*a).mode)) == (i64)5)) || ((i64)((u64)((*b).mode)) == (i64)1)) || ((i64)((u64)((*b).mode)) == (i64)5))) {
        return (i64)0;
    };
    return (i64)1;
}

struct ax_decls_opndrec * ax_lib_genimm_expr(struct ax_decls_strec * d,i64 value,i64 t,i64 size) {
    struct ax_decls_opndrec *  a;
    a = ax_lib_newopnd((i64)2);
    (*a).size = (u64)(size);
    (*a).labeldef = d;
    (*a).value = value;
    (*a).valtype = (u64)(t);
    return a;
}

struct ax_decls_opndrec * ax_lib_genint(i64 x,i64 size) {
    struct ax_decls_opndrec *  a;
    a = ax_lib_newopnd((i64)2);
    (*a).size = (u64)(size);
    (*a).value = x;
    return a;
}

struct ax_decls_opndrec * ax_lib_genlab(struct ax_decls_strec * d,i64 size) {
    struct ax_decls_opndrec *  a;
    a = ax_lib_newopnd((i64)2);
    (*a).size = (u64)(size);
    (*a).labeldef = d;
    return a;
}

struct ax_decls_opndrec * ax_lib_genmem(struct ax_decls_strec * d,i64 size) {
    struct ax_decls_opndrec *  a;
    a = ax_lib_genlab(d,size);
    (*a).mode = (u64)((i64)3);
    return a;
}

struct ax_decls_opndrec * ax_lib_genreg0(i64 reg,i64 size) {
    struct ax_decls_opndrec *  a;
    a = ax_lib_newopnd((i64)1);
    (*a).reg = (u64)(reg);
    (*a).size = (u64)(size);
    return a;
}

byte * ax_lib_getfullname(struct ax_decls_strec * d) {
    static byte str[256];
    byte *  ms;
    ms = (byte*)"";
    if (!!((*d).basedef)) {
        ms = (*(*d).basedef).name;
    };
    msysnewc_m_print_startstr(str);
    msysnewc_m_print_setfmt((byte*)"<# : ## &:# SYM:## M:#>");
    msysnewc_m_print_str((*d).name,NULL);
    msysnewc_m_print_str((byte*)"#",NULL);
    msysnewc_m_print_i64((*d).moduleno,NULL);
    msysnewc_m_print_ptr(d,(byte*)"8");
    msysnewc_m_print_i64(((i64)(strlen((i8 *)(ax_tables_symbolnames[((i64)((*d).symbol))-1]))) - (i64)3),(byte*)"v");
    msysnewc_m_print_str(ax_tables_symbolnames[((i64)((*d).symbol))-1],(byte*)".*");
    msysnewc_m_print_str(ms,NULL);
    msysnewc_m_print_end();
    ;
    return str;
    return (*d).name;
}

byte * ax_lib_getregname(i64 reg,i64 size) {
    byte *  prefix;
    byte *  rs;
    static byte str[32];
    if ((reg==(i64)0)) {
        return (byte*)"-";
    }else if ((reg==(i64)15)) {
        rs = (byte*)"frame";
    }else if ((reg==(i64)16)) {
        rs = (byte*)"stack";
    } else {
        rs = ax_lib_inttostr((reg - (i64)1));
    };
    if ((size==(i64)1)) {
        prefix = (byte*)"B";
    }else if ((size==(i64)2)) {
        prefix = (byte*)"W";
    }else if ((size==(i64)4)) {
        prefix = (byte*)"A";
    } else {
        prefix = (byte*)"D";
    };
    strcpy((i8 *)(str),(i8 *)(prefix));
    strcat((i8 *)(str),(i8 *)(rs));
    return str;
}

byte * ax_lib_xgetregname(i64 reg) {
    static byte str[16];
    msysnewc_m_print_startstr(str);
    msysnewc_m_print_str((byte*)"xmm",NULL);
    msysnewc_m_print_nogap();
    msysnewc_m_print_i64((reg - (i64)1),NULL);
    msysnewc_m_print_end();
    ;
    return str;
}

void ax_lib_printst(void * f) {
    struct ax_decls_strec *  r;
    r = ax_decls_modulenamelist;
    L316 :;
    while (!!(r)) {
        ax_lib_printstrec(f,r);
        r = (*r).nextdef;
L317 :;
    }L318 :;
    ;
}

void ax_lib_printstrec(void * f,struct ax_decls_strec * d) {
    if (((i64)((*d).symbol)==(i64)19) || ((i64)((*d).symbol)==(i64)20) || ((i64)((*d).symbol)==(i64)22)) {
        msysnewc_m_print_startfile(f);
        msysnewc_m_print_str((byte*)"Label:       ",NULL);
        msysnewc_m_print_str(mlib_padstr((*d).name,(i64)16,(byte*)" "),NULL);
        msysnewc_m_print_str((((i64)((u64)((*d).scope)) == (i64)1)?(byte*)"U":(byte*)"-"),NULL);
        msysnewc_m_print_str(ax_tables_symbolnames[((i64)((*d).symbol))-1],NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_str((byte*)"\t",NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_str(mlib_padstr((!!((u64)((*d).segment))?ax_tables_segmentnames[((i64)((*d).segment))-1]:(byte*)"no seg"),(i64)12,(byte*)" "),NULL);
        msysnewc_m_print_i64((*d).offset,NULL);
        msysnewc_m_print_ptr((*d).fwdrefs,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    }else if (((i64)((*d).symbol)==(i64)21)) {
        msysnewc_m_print_startfile(f);
        msysnewc_m_print_str((byte*)"Label:       ",NULL);
        msysnewc_m_print_str(mlib_padstr((*d).name,(i64)16,(byte*)" "),NULL);
        msysnewc_m_print_str((byte*)"EXTERN",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    }else if (((i64)((*d).symbol)==(i64)18)) {
        msysnewc_m_print_startfile(f);
        msysnewc_m_print_str((byte*)"Named const: ",NULL);
        msysnewc_m_print_str(mlib_padstr((*d).name,(i64)16,(byte*)" "),NULL);
        msysnewc_m_print_str((byte*)"=",NULL);
        msysnewc_m_print_str(ax_lib_stropnd((*d).expr,(i64)0),NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    } else {
        msysnewc_m_print_startfile(f);
        msysnewc_m_print_str((byte*)"??",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    };
}

void ax_lib_adddef(struct ax_decls_strec * d) {
    (*d).nextdef = ax_decls_modulenamelist;
    ax_decls_modulenamelist = d;
}

void ax_lib_addimport(struct ax_decls_strec * d) {
    struct ax_decls_stlistrec *  p;
    p = (struct ax_decls_stlistrec *)(mlib_pcm_alloc((i64)16));
    (*p).def = d;
    (*p).nextitem = ax_decls_globalimportlist;
    ax_decls_globalimportlist = p;
}

void ax_lib_createlabel(struct ax_decls_strec * symptr,i64 symbol) {
    (*symptr).symbol = (u64)(symbol);
    (*symptr).stindex = (i64)0;
    (*symptr).moduleno = ax_decls_currmoduleno;
    ax_lib_adddef(symptr);
}

void ax_lib_createnamedconst(struct ax_decls_strec * symptr,struct ax_decls_opndrec * expr) {
    (*symptr).symbol = (u64)((i64)18);
    (*symptr).expr = expr;
    ax_lib_adddef(symptr);
}

void ax_lib_createregalias(struct ax_decls_strec * symptr,i64 regindex,i64 regsize) {
    (*symptr).symbol = (u64)((i64)24);
    (*symptr).ksymbol = (u64)((i64)24);
    (*symptr).subcode = (u64)(regindex);
    (*symptr).regsize = (u64)(regsize);
    ax_lib_adddef(symptr);
}

void ax_lib_createxregalias(struct ax_decls_strec * symptr,i64 regindex) {
    (*symptr).symbol = (u64)((i64)25);
    (*symptr).ksymbol = (u64)((i64)25);
    (*symptr).subcode = (u64)(regindex);
    ax_lib_adddef(symptr);
}

void ax_lib_gerror(byte * mess) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"SS code gen error:",NULL);
    msysnewc_m_print_str(mess,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"On line:",NULL);
    msysnewc_m_print_i64(ax_decls_alineno,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    msysnewc_m_print_startcon();
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    exit((i64)1);
}

void ax_lib_serror(byte * mess) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"Syntax error: '",NULL);
    msysnewc_m_print_nogap();
    msysnewc_m_print_str(mess,NULL);
    msysnewc_m_print_nogap();
    msysnewc_m_print_str((byte*)"' on line",NULL);
    msysnewc_m_print_i64(ax_decls_lxlineno,NULL);
    msysnewc_m_print_str(ax_decls_moduletable[(ax_decls_currmoduleno)-1].name,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    exit((i64)1);
}

void ax_lib_serror_s(byte * mess,byte * param) {
    byte str[256];
    sprintf((i8 *)(str),(i8 *)(mess),param);
    ax_lib_serror(str);
}

static byte * ax_lib_inttostr(i64 a) {
    static byte str[64];
    msysnewc_getstrint(a,str);
    return str;
}

static byte * ax_lib_realtostr(double a) {
    static byte str[64];
    strcpy((i8 *)(str),(i8 *)(msysnewc_strreal(a,(byte *)(0))));
    return str;
}

struct ax_decls_dbuffer * ax_lib_buffercreate(i64 size) {
    struct ax_decls_dbuffer *  a;
    a = (struct ax_decls_dbuffer *)(mlib_pcm_alloc((i64)32));
    (*a).alloc = size;
    (*a).pstart = ((*a).pcurr = (byte *)(mlib_pcm_alloc((*a).alloc)));
    (*a).pend = ((*a).pstart + (*a).alloc);
    return a;
}

static void ax_lib_bufferexpand(struct ax_decls_dbuffer * a) {
    i64 newalloc;
    i64 usedbytes;
    byte *  p;
    newalloc = ((*a).alloc * (i64)2);
    usedbytes = ((*a).pcurr - (*a).pstart);
    if ((usedbytes > (*a).alloc)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"dbuffer error",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        exit(0);
    };
    p = (byte *)(mlib_pcm_alloc(newalloc));
    memcpy((void *)(p),(void *)((*a).pstart),(u64)(usedbytes));
    (*a).pstart = p;
    (*a).pcurr = (p + usedbytes);
    (*a).alloc = newalloc;
    (*a).pend = (p + newalloc);
}

void ax_lib_buffercheck(struct ax_decls_dbuffer * a,i64 n) {
    L319 :;
    while ((((*a).pend - (*a).pcurr) < n)) {
        ax_lib_bufferexpand(a);
L320 :;
    }L321 :;
    ;
}

i64 ax_lib_bufferlength(struct ax_decls_dbuffer * a) {
    return ((*a).pcurr - (*a).pstart);
}

void * ax_lib_bufferelemptr(struct ax_decls_dbuffer * a,i64 offset) {
    return (void *)(((*a).pstart + offset));
}

void ax_lib_addbyte(struct ax_decls_dbuffer * a,i64 x) {
    (*(*a).pcurr) = (u64)(x);
    ++(*a).pcurr;
}

void ax_lib_addword(struct ax_decls_dbuffer * a,i64 x) {
    (*(*a).pcurr16) = (u64)(x);
    ++(*a).pcurr16;
}

void ax_lib_adddword(struct ax_decls_dbuffer * a,i64 x) {
    (*(*a).pcurr32) = (u64)(x);
    ++(*a).pcurr32;
}

void ax_lib_addqword(struct ax_decls_dbuffer * a,i64 x) {
    (*(*a).pcurr64) = (u64)(x);
    ++(*a).pcurr64;
}

void ax_lib_printmodulesymbols(void * f) {
    struct ax_decls_strec *  d;
    struct ax_decls_strec *  e;
    msysnewc_m_print_startfile(f);
    msysnewc_m_print_str((byte*)"MODULE SYMBOLS IN",NULL);
    msysnewc_m_print_str(ax_decls_moduletable[(ax_decls_currmoduleno)-1].name,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    d = ax_decls_modulenamelist;
    L322 :;
    while (!!(d)) {
        msysnewc_m_print_startfile(f);
        msysnewc_m_print_str((byte*)"   ",NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_str(mlib_padstr((*d).name,(i64)14,(byte*)" "),NULL);
        msysnewc_m_print_str(mlib_padstr(ax_tables_symbolnames[((i64)((*d).symbol))-1],(i64)12,(byte*)" "),NULL);
        msysnewc_m_print_end();
        ;
        msysnewc_m_print_startfile(f);
        msysnewc_m_print_setfmt((byte*)"|| # # #");
        msysnewc_m_print_u64((*d).htfirstindex,(byte*)"6");
        msysnewc_m_print_u64((*d).htindex,(byte*)"6");
        msysnewc_m_print_ptr(d,(byte*)"8H");
        msysnewc_m_print_end();
        ;
        e = ax_decls_dupltable[((i64)((*d).htfirstindex))];
        if (!!(e)) {
            msysnewc_m_print_startfile(f);
            msysnewc_m_print_str((byte*)"||",NULL);
            msysnewc_m_print_end();
            ;
            L325 :;
            while (!!(e)) {
                msysnewc_m_print_startfile(f);
                msysnewc_m_print_str((byte*)"(",NULL);
                msysnewc_m_print_nogap();
                msysnewc_m_print_str((*e).name,NULL);
                msysnewc_m_print_nogap();
                msysnewc_m_print_str((byte*)")",NULL);
                msysnewc_m_print_end();
                ;
                e = (*e).nextdupl;
L326 :;
            }L327 :;
            ;
        };
        msysnewc_m_print_startfile(f);
        msysnewc_m_print_str((byte*)" BASE:",NULL);
        msysnewc_m_print_str((!!((*d).basedef)?(*(*d).basedef).name:(byte*)""),NULL);
        msysnewc_m_print_ptr((*d).basedef,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        d = (*d).nextdef;
L323 :;
    }L324 :;
    ;
    msysnewc_m_print_startfile(f);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

void ax_lib_printimportsymbols(void * f) {
    struct ax_decls_strec *  d;
    struct ax_decls_stlistrec *  p;
    msysnewc_m_print_startfile(f);
    msysnewc_m_print_str((byte*)"GLOBAL IMPORT TABLE",NULL);
    msysnewc_m_print_ptr(ax_decls_globalimportlist,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    p = ax_decls_globalimportlist;
    L328 :;
    while (!!(p)) {
        d = (*p).def;
        msysnewc_m_print_startfile(f);
        msysnewc_m_print_str((byte*)"   ",NULL);
        msysnewc_m_print_nogap();
        msysnewc_m_print_str(mlib_padstr((*d).name,(i64)14,(byte*)" "),NULL);
        msysnewc_m_print_str(mlib_padstr(ax_tables_symbolnames[((i64)((*d).symbol))-1],(i64)12,(byte*)" "),NULL);
        msysnewc_m_print_end();
        ;
        msysnewc_m_print_startfile(f);
        msysnewc_m_print_str((byte*)"D^.OFFSET=",NULL);
        msysnewc_m_print_i64((*d).offset,NULL);
        msysnewc_m_print_str(ax_tables_reftypenames[((i64)((*d).reftype))],NULL);
        msysnewc_m_print_ptr((void *)(d),NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        p = (*p).nextitem;
L329 :;
    }L330 :;
    ;
    msysnewc_m_print_startfile(f);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

void ax_lib_printdupltable(void * f) {
    byte str[256];
    struct ax_decls_strec *  d;
    i64 i;
    i64 av_1;
    msysnewc_m_print_startfile(f);
    msysnewc_m_print_str((byte*)"DUPL TABLE",NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    L331 :;
    for (i=(i64)0;i<=(i64)65535;i+=(i64)1) {
L332 :;
        if (!!(ax_decls_dupltable[(i)])) {
            d = ax_decls_dupltable[(i)];
            msysnewc_m_print_startfile(f);
            msysnewc_m_print_str((byte*)"\t",NULL);
            msysnewc_m_print_u64((*d).htfirstindex,NULL);
            msysnewc_m_print_nogap();
            msysnewc_m_print_str((byte*)":",NULL);
            msysnewc_m_print_end();
            ;
            L335 :;
            while (!!(d)) {
                msysnewc_m_print_startstr(str);
                msysnewc_m_print_setfmt((byte*)"(# # (#) #) ");
                msysnewc_m_print_u64((*d).htindex,(byte*)"6");
                msysnewc_m_print_str((*d).name,NULL);
                msysnewc_m_print_str(ax_decls_moduletable[((i64)((*d).moduleno))-1].name,NULL);
                msysnewc_m_print_ptr(d,(byte*)"8H");
                msysnewc_m_print_end();
                ;
                d = (*d).nextdupl;
L336 :;
            }L337 :;
            ;
            msysnewc_m_print_startfile(f);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
        };
L333 :;
    }L334 :;
    ;
    msysnewc_m_print_startfile(f);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}

void ax_genss_genss(void) {
    i64 index;
    struct ax_lib_mclrec *  m;
    ax_decls_ss_zdatalen = (i64)0;
    ax_decls_ss_zdata = ax_lib_buffercreate((i64)1024);
    ax_decls_ss_idata = ax_lib_buffercreate((i64)1024);
    ax_decls_ss_code = ax_lib_buffercreate((i64)1024);
    ax_decls_ss_idatarelocs = (struct ax_decls_relocrec *)(0);
    ax_decls_ss_coderelocs = (struct ax_decls_relocrec *)(0);
    ax_decls_ss_nsymbols = (i64)0;
    ax_genss_switchseg((i64)1);
    ax_decls_alineno = (i64)9999;
    ax_genss_extraparam = (struct ax_decls_opndrec *)(0);
    m = ax_lib_mccode;
    index = (i64)0;
    L338 :;
    while (!!(m)) {
        ax_decls_alineno = (*m).lineno;
        ax_genss_doinstr(m,++index);
        m = (*m).nextmcl;
L339 :;
    }L340 :;
    ;
    ax_genss_switchseg((i64)0);
    if (!!(ax_lib_bufferlength(ax_decls_ss_zdata))) {
        ax_lib_gerror((byte*)"Zdata contains code or data");
    };
}

static void ax_genss_doinstr(struct ax_lib_mclrec * m,i64 index) {
    struct ax_decls_opndrec *  a;
    struct ax_decls_opndrec *  b;
    struct ax_decls_strec *  d;
    i64 x;
    i64 offset;
    i64 shortjmp;
    i64 n;
    i64 av_1;
    i64 av_2;
    ax_genss_currmcl = m;
    ax_lib_buffercheck(ax_genss_currdata,(i64)1024);
    ax_genss_rex = (ax_genss_sizeoverride = (ax_genss_addroverride = (ax_genss_f2override = (ax_genss_f3override = (i64)0))));
    a = (*m).a;
    b = (*m).b;
    switch ((i64)((*m).opcode)) {
    case 4:;
    {
        d = (*a).labeldef;
        (*d).reftype = (u64)((i64)2);
        (*d).segment = (u64)(ax_genss_currseg);
        (*d).offset = ax_genss_getcurrdatalen((i64)6);
        if (((i64)((u64)((*d).symbol)) == (i64)22)) {
            ax_genss_getstindex(d);
        };
        ax_genss_dofwdrefs(d);
    }break;
    case 19:;
    {
        ax_genss_do_call(a);
    }break;
    case 22:;
    {
        ax_genss_do_jmp(a,m);
    }break;
    case 23:;
    {
        offset = ax_genss_getrel32((*b).labeldef,(ax_genss_getcurrdatalen((i64)7) + (i64)1));
        if ((offset < (i64)0)) {
            if ((offset < (i64)-126)) {
                ax_genss_genbyte((i64)15);
                ax_genss_genbyte(((i64)128 + (*a).value));
                ax_genss_gendword((offset - (i64)4));
            } else {
                ax_genss_genbyte(((i64)112 + (*(*m).a).value));
                ax_genss_genbyte(offset);
            };
        } else {
            shortjmp = ax_genss_checkshortjump(m,(*b).labeldef);
            if (!(!!(shortjmp))) {
                ax_genss_genbyte((i64)15);
                ax_genss_genbyte(((i64)128 + (*a).value));
                ax_genss_genrel32(b);
            } else {
                ax_genss_genbyte(((i64)112 + (*a).value));
                ax_genss_genrel8(b);
            };
        };
    }break;
    case 110:;
    {
        ax_genss_genopnd(a,(i64)1);
    }break;
    case 111:;
    {
        ax_genss_genopnd(a,(i64)2);
    }break;
    case 112:;
    {
        ax_genss_genopnd(a,(i64)4);
    }break;
    case 113:;
    {
        ax_genss_genopnd(a,(i64)8);
    }break;
    case 114:;
    {
        ax_genss_genrel32(a);
    }break;
    case 115:;
    {
        ax_genss_switchseg((*a).value);
    }break;
    case 5:;
    case 144:;
    {
        ax_genss_genbyte((i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    case 51:;
    {
        ax_genss_genbyte((i64)102);
        ax_genss_genbyte((i64)152);
    }break;
    case 52:;
    {
        ax_genss_genbyte((i64)102);
        ax_genss_genbyte((i64)153);
    }break;
    case 53:;
    {
        ax_genss_genbyte((i64)153);
    }break;
    case 54:;
    {
        ax_genss_genbyte((i64)72);
        ax_genss_genbyte((i64)153);
    }break;
    case 20:;
    {
        ax_genss_genbyte((i64)195);
    }break;
    case 21:;
    {
        if (((i64)((u64)((*a).mode)) != (i64)2)) {
            ax_lib_gerror((byte*)"retn?");
        };
        ax_genss_genbyte((i64)194);
        ax_genss_genword((*a).value);
    }break;
    case 10:;
    {
        ax_genss_do_push(a);
    }break;
    case 11:;
    {
        ax_genss_do_pop(a);
    }break;
    case 49:;
    case 50:;
    {
        ax_genss_do_inc(a,(i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    case 47:;
    case 48:;
    case 30:;
    case 29:;
    case 34:;
    case 33:;
    {
        ax_genss_do_neg(a,(i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    case 25:;
    case 26:;
    case 35:;
    case 36:;
    case 37:;
    case 27:;
    case 28:;
    case 39:;
    {
        ax_genss_do_arith(a,b,(i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    case 9:;
    {
        ax_genss_do_mov(a,b);
    }break;
    case 12:;
    {
        ax_genss_do_lea(a,b);
    }break;
    case 16:;
    {
        ax_genss_do_movsx(a,b,(i64)190);
    }break;
    case 17:;
    {
        ax_genss_do_movsx(a,b,(i64)182);
    }break;
    case 18:;
    {
        ax_genss_do_movsxd(a,b);
    }break;
    case 24:;
    {
        ax_genss_do_exch(a,b);
    }break;
    case 31:;
    {
        ax_genss_do_imul2(a,b);
    }break;
    case 120:;
    case 121:;
    case 122:;
    case 123:;
    {
        if (((i64)((u64)((*a).mode)) == (i64)2)) {
            n = ((*a).value * (i64)((u64)(ax_tables_mclcodes[((i64)((*m).opcode))-1])));
            if ((ax_genss_currseg==(i64)1)) {
                av_1 = n;
                while (av_1-- > 0) {
L341 :;
                    ax_genss_genbyte((i64)144);
L342 :;
                }L343 :;
                ;
            }else if ((ax_genss_currseg==(i64)2)) {
                av_2 = n;
                while (av_2-- > 0) {
L344 :;
                    ax_genss_genbyte((i64)0);
L345 :;
                }L346 :;
                ;
            } else {
                ax_decls_ss_zdatalen += n;
            };
        } else {
            ax_lib_gerror((byte*)"resb?");
        };
    }break;
    case 119:;
    {
        if (((i64)((u64)((*a).mode)) == (i64)2)) {
            x = (*a).value;
            if (((x < (i64)1) || (x > (i64)16384))) {
                ax_lib_gerror((byte*)"align2");
            };
            if ((ax_genss_currseg != (i64)3)) {
                L347 :;
                while (!!((ax_lib_bufferlength(ax_genss_currdata) % x))) {
                    ax_genss_genbyte(((ax_genss_currseg == (i64)1)?(i64)144:(i64)0));
L348 :;
                }L349 :;
                ;
            } else {
                L350 :;
                while (!!((ax_decls_ss_zdatalen % x))) {
                    ++ax_decls_ss_zdatalen;
L351 :;
                }L352 :;
                ;
            };
        } else {
            ax_lib_gerror((byte*)"align?");
        };
    }break;
    case 40:;
    case 42:;
    case 41:;
    case 43:;
    case 44:;
    case 45:;
    case 46:;
    {
        ax_genss_do_shift(a,b,(i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    case 38:;
    {
        ax_genss_do_test(a,b);
    }break;
    case 127:;
    case 126:;
    case 125:;
    {
        ax_genss_do_loop(a,(i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    case 128:;
    {
        ax_genss_do_jcxz(a,(i64)4);
    }break;
    case 129:;
    {
        ax_genss_do_jcxz(a,(i64)8);
    }break;
    case 124:;
    {
        ax_genss_genbyte((i64)215);
    }break;
    case 55:;
    {
        ax_genss_do_setcc(a,b);
    }break;
    case 14:;
    {
        ax_genss_do_movxmm(a,b,(i64)4);
    }break;
    case 15:;
    {
        ax_genss_do_movxmm(a,b,(i64)8);
    }break;
    case 60:;
    case 61:;
    case 62:;
    case 63:;
    case 59:;
    case 106:;
    case 107:;
    {
        ax_genss_do_arithxmm(a,b,(i64)243,(i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    case 64:;
    case 65:;
    case 66:;
    case 67:;
    case 58:;
    case 108:;
    case 109:;
    {
        ax_genss_do_arithxmm(a,b,(i64)242,(i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    case 73:;
    case 71:;
    {
        ax_genss_do_logicxmm(a,b,(i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]),(i64)4);
    }break;
    case 72:;
    case 70:;
    case 75:;
    case 74:;
    {
        ax_genss_do_logicxmm(a,b,(i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]),(i64)8);
    }break;
    case 86:;
    case 87:;
    {
        ax_genss_do_pcmpistri(a,b,(i64)((*m).c),(i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    case 68:;
    {
        ax_genss_do_arithxmm(a,b,(i64)0,(i64)47);
    }break;
    case 69:;
    {
        ax_genss_do_arithxmm(a,b,(i64)102,(i64)47);
    }break;
    case 83:;
    {
        ax_genss_do_convertfloat(a,b,(i64)243);
    }break;
    case 82:;
    {
        ax_genss_do_convertfloat(a,b,(i64)242);
    }break;
    case 76:;
    {
        ax_genss_do_fix(a,b,(i64)243,(i64)45);
    }break;
    case 77:;
    {
        ax_genss_do_fix(a,b,(i64)242,(i64)45);
    }break;
    case 78:;
    {
        ax_genss_do_fix(a,b,(i64)243,(i64)44);
    }break;
    case 79:;
    {
        ax_genss_do_fix(a,b,(i64)242,(i64)44);
    }break;
    case 80:;
    {
        ax_genss_do_float(a,b,(i64)243);
    }break;
    case 81:;
    {
        ax_genss_do_float(a,b,(i64)242);
    }break;
    case 6:;
    {
        ax_genss_extraparam = a;
    }break;
    case 13:;
    {
        ax_genss_do_cmovcc(a,ax_genss_extraparam,b);
    }break;
    case 98:;
    case 99:;
    case 100:;
    case 101:;
    case 102:;
    case 103:;
    case 104:;
    case 105:;
    {
        ax_genss_genbyte((i64)217);
        ax_genss_genbyte((i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    case 88:;
    case 89:;
    case 90:;
    {
        ax_genss_do_fmem(a,(i64)1,(i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    case 91:;
    case 92:;
    case 93:;
    {
        ax_genss_do_fmem(a,(i64)0,(i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    case 94:;
    case 95:;
    case 96:;
    case 97:;
    {
        ax_genss_genbyte((i64)222);
        ax_genss_genbyte((i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    case 130:;
    {
        ax_genss_genbyte((i64)166);
    }break;
    case 131:;
    {
        ax_genss_genbyte((i64)102);
        ax_genss_genbyte((i64)167);
    }break;
    case 132:;
    {
        ax_genss_genbyte((i64)167);
    }break;
    case 133:;
    {
        ax_genss_genbyte((i64)72);
        ax_genss_genbyte((i64)167);
    }break;
    case 134:;
    {
        ax_genss_genbyte((i64)15);
        ax_genss_genbyte((i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    case 84:;
    case 85:;
    {
        ax_genss_do_movdqx(a,b,(i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    case 136:;
    {
        ax_genss_genbyte((i64)219);
        ax_genss_genbyte((i64)227);
    }break;
    case 137:;
    case 138:;
    case 139:;
    case 140:;
    case 141:;
    case 142:;
    case 143:;
    {
        ax_genss_genbyte((i64)217);
        ax_genss_genbyte((i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    case 135:;
    {
        ax_genss_do_popcnt(a,b);
    }break;
    case 56:;
    case 57:;
    {
        ax_genss_do_bsf(a,b,(i64)(ax_tables_mclcodes[((i64)((*m).opcode))-1]));
    }break;
    default: {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"*** CAN'T DO OPCODE",NULL);
        msysnewc_m_print_str(ax_tables_mclnames[((i64)((*m).opcode))-1],NULL);
        msysnewc_m_print_str((byte*)"line",NULL);
        msysnewc_m_print_i64(ax_decls_alineno,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    }
    } //SW
;
}

static void ax_genss_genbyte(i64 x) {
    (*(*ax_genss_currdata).pcurr++) = (u64)(x);
}

static void ax_genss_genword(i64 x) {
    ax_lib_addword(ax_genss_currdata,x);
}

static void ax_genss_gendword(i64 x) {
    ax_lib_adddword(ax_genss_currdata,x);
}

static void ax_genss_genqword(i64 x) {
    ax_lib_addqword(ax_genss_currdata,x);
}

static void ax_genss_genopnd(struct ax_decls_opndrec * a,i64 size) {
    byte *  s;
    i64 x;
    i64 length;
    if ((size == (i64)0)) {
        size = (i64)((*a).size);
    };
    switch ((i64)((*a).mode)) {
    case 2:;
    case 3:;
    {
    }break;
    case 6:;
    {
        s = (*a).svalue;
        length = (i64)(strlen((i8 *)(s)));
        if ((length > (i64)100)) {
            ax_lib_buffercheck(ax_genss_currdata,msysnewc_m_imax((i64)1024,(length + (i64)1)));
        };
        L353 :;
        while (!!((u64)((*s)))) {
            ax_genss_genbyte((i64)((*s++)));
L354 :;
        }L355 :;
        ;
        return;
    }break;
    default: {
        ax_lib_gerror((byte*)"GENOPND/bad opnd");
    }
    } //SW
;
    if ((!!((*a).labeldef) && (size <= (i64)2))) {
        ax_lib_gerror((byte*)"8/16-BIT RELOC");
    };
    if ((size==(i64)1)) {
        ax_genss_genbyte((*a).value);
    }else if ((size==(i64)2)) {
        ax_genss_genword((*a).value);
    }else if ((size==(i64)4)) {
        if (!!((*a).labeldef)) {
            ax_genss_genabs32(a);
        } else {
            if (!!((u64)((*a).valtype))) {
                ax_genss_gendword(ax_genss_getr32bits((*a).xvalue));
            } else {
                ax_genss_gendword((*a).value);
            };
        };
    }else if ((size==(i64)8)) {
        if (!!((*a).labeldef)) {
            ax_genss_genabs64(a);
        } else {
            x = (*a).value;
            if (!!((u64)((*a).valtype))) {
                ax_genss_genqword(*(i64*)&x);
            } else {
                ax_genss_genqword(x);
            };
        };
    };
}

static void ax_genss_addrelocitem(i64 reloctype,struct ax_decls_strec * d) {
    struct ax_decls_relocrec *  r;
    i64 stindex;
    i64 adjust;
    stindex = ax_genss_getstindex(d);
    adjust = (i64)4;
    if ((reloctype == (i64)1)) {
        adjust = (i64)8;
    };
    r = (struct ax_decls_relocrec *)(mlib_pcm_alloc((i64)32));
    (*r).nextreloc = ax_genss_currrelocs;
    (*r).reloctype = reloctype;
    (*r).offset = (ax_genss_getcurrdatalen((i64)1) - adjust);
    (*r).stindex = stindex;
    ++ax_genss_nrelocs;
    ax_genss_currrelocs = r;
}

static i64 ax_genss_getstindex(struct ax_decls_strec * d) {
    if (((i64)((*d).stindex) == (i64)0)) {
        if ((ax_decls_ss_nsymbols >= ax_decls_ss_symboltablesize)) {
            ax_genss_extendsymboltable();
        };
        (*d).stindex = ++ax_decls_ss_nsymbols;
        (*ax_decls_ss_symboltable)[((i64)((*d).stindex))-1] = d;
    };
    return (i64)((*d).stindex);
}

static void ax_genss_genrel32(struct ax_decls_opndrec * a) {
    struct ax_decls_strec *  d;
    d = (*a).labeldef;
    if ((d == 0)) {
        ax_genss_gendword((*a).value);
        return;
    };
    if (((i64)((*d).reftype)==(i64)2)) {
        if (((i64)((u64)((*d).segment)) != ax_genss_currseg)) {
            ax_lib_gerror((byte*)"Rel label across segments");
        };
        ax_genss_gendword(((i64)((*d).offset) - (ax_genss_getcurrdatalen((i64)2) + (i64)4)));
    }else if (((i64)((*d).reftype)==(i64)1)) {
        (*d).fwdrefs = ax_genss_addfwdref((*d).fwdrefs,ax_genss_getcurrdatalen((i64)3),(i64)4,(i64)0);
        ax_genss_gendword((i64)0);
    } else {
        ax_genss_gendword((*a).value);
        ax_genss_addrelocitem((i64)4,d);
    };
}

static void ax_genss_genabs32(struct ax_decls_opndrec * a) {
    struct ax_decls_strec *  d;
    d = (*a).labeldef;
    if (((i64)((*d).reftype)==(i64)2)) {
        ax_genss_gendword(((i64)((*d).offset) + (*a).value));
        ax_genss_addrelocitem((i64)2,d);
    }else if (((i64)((*d).reftype)==(i64)1)) {
        (*d).fwdrefs = ax_genss_addfwdref((*d).fwdrefs,ax_genss_getcurrdatalen((i64)4),(i64)2,ax_genss_currseg);
        ax_genss_gendword((*a).value);
        ax_genss_addrelocitem((i64)2,d);
    } else {
        ax_genss_gendword((*a).value);
        ax_genss_addrelocitem((i64)2,d);
    };
}

static void ax_genss_genabs64(struct ax_decls_opndrec * a) {
    struct ax_decls_strec *  d;
    d = (*a).labeldef;
    if (((i64)((*d).reftype)==(i64)2)) {
        ax_genss_genqword(((i64)((*d).offset) + (*a).value));
        ax_genss_addrelocitem((i64)1,d);
    }else if (((i64)((*d).reftype)==(i64)1)) {
        (*d).fwdrefs = ax_genss_addfwdref((*d).fwdrefs,ax_genss_getcurrdatalen((i64)5),(i64)2,ax_genss_currseg);
        ax_genss_genqword((*a).value);
        ax_genss_addrelocitem((i64)1,d);
    } else {
        ax_genss_genqword((*a).value);
        ax_genss_addrelocitem((i64)1,d);
    };
}

static i64 ax_genss_getrel32(struct ax_decls_strec * d,i64 offset) {
    if (((i64)((u64)((*d).reftype)) == (i64)2)) {
        if (((i64)((u64)((*d).segment)) != ax_genss_currseg)) {
            ax_lib_gerror((byte*)"Rel label across segments2");
        };
        return ((i64)((*d).offset) - (offset + (i64)1));
    } else {
        return (i64)2147483647;
    };
}

static void ax_genss_dofwdrefs(struct ax_decls_strec * d) {
    struct ax_decls_fwdrec *  f;
    i64 offset;
    byte *  p8;
    i32 *  p32;
    i64 *  p64;
    struct ax_decls_dbuffer *  data;
    if (((*d).fwdrefs == 0)) {
        return;
    };
    f = (*d).fwdrefs;
    L356 :;
    while (!!(f)) {
        offset = (i64)((*f).offset);
        if (((i64)((*f).reltype)==(i64)4)) {
            p32 = (i32 *)(ax_lib_bufferelemptr(ax_genss_currdata,offset));
            (*p32) = (((i64)((*d).offset) - offset) - (i64)4);
        }else if (((i64)((*f).reltype)==(i64)2) || ((i64)((*f).reltype)==(i64)1)) {
            if (((i64)((*f).seg)==(i64)1)) {
                data = ax_decls_ss_code;
            }else if (((i64)((*f).seg)==(i64)3)) {
                ax_lib_gerror((byte*)"Fwd ref in zdata");
            }else if (((i64)((*f).seg)==(i64)2)) {
                data = ax_decls_ss_idata;
            };
            p32 = (i32 *)(ax_lib_bufferelemptr(data,offset));
            if (((i64)((*f).reltype) == (i64)2)) {
                (*p32) = ((i64)((*p32)) + (i64)((*d).offset));
            } else {
                p64 = (i64 *)(p32);
                (*p64) = ((*p64) + (i64)((*d).offset));
            };
        }else if (((i64)((*f).reltype)==(i64)6)) {
            p8 = (byte *)(ax_lib_bufferelemptr(ax_genss_currdata,offset));
            (*p8) = (u64)((((i64)((*d).offset) - offset) - (i64)1));
        } else {
            msysnewc_m_print_startcon();
            msysnewc_m_print_str(ax_objdecls_relocnames[((i64)((*f).reltype))],NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            ax_lib_gerror((byte*)"DOFWDREFS/CAN'T DO RELTYPE");
        };
        f = (*f).nextfwd;
L357 :;
    }L358 :;
    ;
}

static void ax_genss_genrex(void) {
    if (!!(ax_genss_sizeoverride)) {
        ax_genss_genbyte((i64)102);
    };
    if (!!(ax_genss_addroverride)) {
        ax_genss_genbyte((i64)103);
    };
    if (!!(ax_genss_rex)) {
        if ((ax_genss_rex < (i64)64)) {
            ax_genss_genbyte(((i64)64 + ax_genss_rex));
        } else {
            ax_genss_genbyte(ax_genss_rex);
        };
    };
}

static i64 ax_genss_isbytesized(i64 x) {
    return (((i64)-128 <= x) && (x <= (i64)127));
}

static i64 ax_genss_isdwordsized(i64 x) {
    return (((i64)-2147483648 <= x) && (x <= (i64)2147483647));
}

static void ax_genss_do_push(struct ax_decls_opndrec * a) {
    i64 code;
    i64 am;
    if (((i64)((*a).mode)==(i64)1)) {
        if (((i64)((u64)((*a).size)) != (i64)8)) {
            ax_lib_gerror((byte*)"pushreg not 64-bit");
        };
        code = (i64)(ax_tables_regcodes[((i64)((*a).reg))]);
        if ((code >= (i64)8)) {
            ax_genss_rex = (i64)1;
            code &= (i64)7;
        };
        ax_genss_genrex();
        ax_genss_genbyte(((i64)80 + code));
    }else if (((i64)((*a).mode)==(i64)2)) {
        if (!!((*a).labeldef)) {
            ax_genss_genbyte((i64)104);
            ax_genss_genopnd(a,(i64)4);
        } else if (!!(ax_genss_isbytesized((*a).value))) {
            ax_genss_genbyte((i64)106);
            ax_genss_genbyte((*a).value);
        } else if (!!(ax_genss_isdwordsized((*a).value))) {
            ax_genss_genbyte((i64)104);
            ax_genss_gendword((*a).value);
        } else {
            ax_lib_gerror((byte*)"push imm value too large");
        };
    }else if (((i64)((*a).mode)==(i64)3)) {
        if (((i64)((u64)((*a).size)) != (i64)8)) {
            ax_lib_gerror((byte*)"push not 64-bit");
        };
        am = ax_genss_genrm(a,(i64)6);
        ax_genss_genrex();
        ax_genss_genbyte((i64)255);
        ax_genss_genamode(a,am);
    } else {
        ax_lib_gerror((byte*)"push opnd?");
    };
}

static void ax_genss_do_pop(struct ax_decls_opndrec * a) {
    i64 code;
    i64 am;
    if (((i64)((*a).mode)==(i64)1)) {
        if (((i64)((u64)((*a).size)) != (i64)8)) {
            ax_lib_gerror((byte*)"popreg not 64-bit");
        };
        code = (i64)(ax_tables_regcodes[((i64)((*a).reg))]);
        if ((code >= (i64)8)) {
            ax_genss_rex = (i64)1;
            code &= (i64)7;
        };
        ax_genss_genrex();
        ax_genss_genbyte(((i64)88 + code));
    }else if (((i64)((*a).mode)==(i64)3)) {
        if (((i64)((u64)((*a).size)) != (i64)8)) {
            ax_lib_gerror((byte*)"pop not 64-bit");
        };
        am = ax_genss_genrm(a,(i64)0);
        ax_genss_genrex();
        ax_genss_genbyte((i64)143);
        ax_genss_genamode(a,am);
    } else {
        ax_lib_gerror((byte*)"pop opnd?");
    };
}

static void ax_genss_do_inc(struct ax_decls_opndrec * a,i64 code) {
    i64 opc;
    i64 am;
    opc = (((i64)((u64)((*a).size)) == (i64)1)?(i64)254:(i64)255);
    if (((i64)((*a).mode)==(i64)1) || ((i64)((*a).mode)==(i64)3)) {
        am = ax_genss_genrm(a,code);
        ax_genss_checkhighreg(a);
        ax_genss_setopsize(a);
        ax_genss_genrex();
        ax_genss_genbyte(opc);
        ax_genss_genamode(a,am);
    } else {
        ax_lib_gerror((byte*)"inc/opnd?");
    };
}

static void ax_genss_do_neg(struct ax_decls_opndrec * a,i64 code) {
    i64 opc;
    i64 am;
    opc = (((i64)((u64)((*a).size)) == (i64)1)?(i64)246:(i64)247);
    if (((i64)((*a).mode)==(i64)1) || ((i64)((*a).mode)==(i64)3)) {
        am = ax_genss_genrm(a,code);
        ax_genss_checkhighreg(a);
        ax_genss_setopsize(a);
        ax_genss_genrex();
        ax_genss_genbyte(opc);
        ax_genss_genamode(a,am);
    } else {
        ax_lib_gerror((byte*)"neg/div/etc opnd?");
    };
}

static void ax_genss_genamode(struct ax_decls_opndrec * a,i64 am) {
    i64 sib;
    i64 mode;
    i64 dispsize;
    sib = (am >> (i64)16);
    mode = ((am >> (i64)8) & (i64)255);
    dispsize = (am & (i64)255);
    ax_genss_genbyte(mode);
    if ((sib >= (i64)0)) {
        ax_genss_genbyte(sib);
    };
    if ((dispsize==(i64)0)) {
    }else if ((dispsize==(i64)1)) {
        ax_genss_genbyte((*a).value);
    }else if ((dispsize==(i64)4)) {
        if (!!((*a).labeldef)) {
            ax_genss_genabs32(a);
        } else {
            ax_genss_gendword((*a).value);
        };
    } else {
        ax_lib_gerror((byte*)"genamode size 2/8");
    };
}

static i64 ax_genss_makemodrm(i64 mode,i64 opc,i64 rm) {
    return (((mode << (i64)6) + (opc << (i64)3)) + rm);
}

static void ax_genss_setopsize(struct ax_decls_opndrec * a) {
    if (((i64)((*a).size)==(i64)1)) {
    }else if (((i64)((*a).size)==(i64)2)) {
        ax_genss_sizeoverride = (i64)1;
    }else if (((i64)((*a).size)==(i64)8)) {
        ax_genss_rex |= (i64)8;
    }else if (((i64)((*a).size)==(i64)4)) {
    } else {
        ax_lib_gerror((byte*)"Operand size not set");
    };
}

static void ax_genss_setaddrsize(struct ax_decls_opndrec * a) {
    if ((((i64)((u64)((*a).mode)) == (i64)3) && ((i64)((u64)((*a).addrsize)) == (i64)4))) {
        ax_genss_addroverride = (i64)1;
    };
}

static i64 ax_genss_getdispsize(struct ax_decls_opndrec * a,i64 mand) {
    if (!!((*a).labeldef)) {
        return (i64)4;
    };
    if ((!!((*a).value) || !!(mand))) {
        return (!!(ax_genss_isbytesized((*a).value))?(i64)1:(i64)4);
    } else {
        return (i64)0;
    };
}

static i64 ax_genss_genrm(struct ax_decls_opndrec * a,i64 opc) {
    static i64 scaletable[8] = {(i64)0,(i64)1,(i64)0,(i64)2,(i64)0,(i64)0,(i64)0,(i64)3};
    i64 mode;
    i64 rm;
    i64 scale;
    i64 dispsize;
    i64 needsib;
    i64 sib;
    i64 index;
    i64 base;
    i64 reg;
    i64 regix;
    i64 code;
    mode = (rm = (i64)0);
    scale = (i64)0;
    dispsize = (i64)0;
    needsib = (i64)0;
    sib = (i64)-1;
    if ((((i64)((u64)((*a).mode)) == (i64)3) && ((i64)((u64)((*a).addrsize)) == (i64)4))) {
        ax_genss_addroverride = (i64)1;
    };
    if (((i64)((*a).mode)==(i64)1)) {
        code = ax_genss_getregcodeb((i64)((*a).reg));
        return ax_genss_makeam(ax_genss_makemodrm((i64)3,opc,code),sib,dispsize);
    }else if (((i64)((*a).mode)==(i64)3)) {
    }else if (((i64)((*a).mode)==(i64)5)) {
        code = ax_genss_getregcodebx((i64)((*a).reg));
        return ax_genss_makeam(ax_genss_makemodrm((i64)3,opc,code),sib,dispsize);
    } else {
        ax_lib_gerror((byte*)"genrm not mem");
    };
    reg = (i64)((*a).reg);
    regix = (i64)((*a).regix);
    if (((reg == regix) && (regix == (i64)0))) {
        mode = (i64)0;
        rm = (i64)4;
        scale = (i64)1;
        index = (i64)4;
        base = (i64)5;
        dispsize = (i64)4;
    } else if ((((i64)((u64)((*a).scale)) <= (i64)1) && (regix == (i64)0))) {
        dispsize = ax_genss_getdispsize(a,(i64)0);
        if (!!(dispsize)) {
            mode = ((dispsize == (i64)1)?(i64)1:(i64)2);
        };
        rm = (i64)(ax_tables_regcodes[(reg)]);
        if (((rm != (i64)4) && (rm != (i64)12))) {
            base = rm;
            if ((((rm == (i64)5) || (rm == (i64)13)) && (dispsize == (i64)0))) {
                mode = (i64)1;
                dispsize = (i64)1;
            };
            index = (i64)0;
        } else {
            index = (i64)4;
            base = rm;
            scale = (i64)1;
        };
    } else if ((!!(regix) && (reg == (i64)0))) {
        dispsize = (i64)4;
        mode = (i64)0;
        rm = (i64)4;
        scale = (!!((u64)((*a).scale))?(i64)((*a).scale):(i64)1);
        base = (i64)5;
        index = (i64)(ax_tables_regcodes[(regix)]);
        if ((regix == (i64)16)) {
            ax_lib_gerror((byte*)"Scaled rstack?");
        };
    } else {
        dispsize = ax_genss_getdispsize(a,(i64)0);
        if (!!(dispsize)) {
            mode = ((dispsize == (i64)1)?(i64)1:(i64)2);
        };
        rm = (i64)4;
        scale = (!!((u64)((*a).scale))?(i64)((*a).scale):(i64)1);
        if ((reg == (i64)0)) {
            base = (i64)5;
        } else {
            if (((reg == (i64)15) && (dispsize == (i64)0))) {
                mode = (i64)1;
                dispsize = (i64)1;
            };
            base = (i64)(ax_tables_regcodes[(reg)]);
        };
        if ((regix == (i64)0)) {
            index = (i64)4;
        } else {
            index = (i64)(ax_tables_regcodes[(regix)]);
        };
        if ((!!(regix) && !(!!(reg)))) {
            dispsize = (i64)4;
        };
        if (((regix == (i64)16) && (scale > (i64)1))) {
            ax_lib_gerror((byte*)"Can't scale rstack");
        };
    };
    if ((index >= (i64)8)) {
        ax_genss_rex |= (i64)2;
        index &= (i64)7;
    };
    if ((base >= (i64)8)) {
        ax_genss_rex |= (i64)1;
        base &= (i64)7;
    };
    if (!!(scale)) {
        sib = (((scaletable[(scale)-1] << (i64)6) + (index << (i64)3)) + base);
    };
    rm &= (i64)7;
    return ax_genss_makeam(ax_genss_makemodrm(mode,opc,rm),sib,dispsize);
}

static void ax_genss_genrmbyte(i64 mode,i64 opc,i64 rm) {
    ax_genss_genbyte((((mode << (i64)6) + (opc << (i64)3)) + rm));
}

static i64 ax_genss_makeam(i64 m,i64 s,i64 d) {
    return (((s << (i64)16) + (m << (i64)8)) + d);
}

static void ax_genss_do_arith(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 code) {
    i64 am;
    i64 regcode;
    i64 opc;
    i64 dispsize;
    i64 x;
    if (((i64)((*a).mode)==(i64)1)) {
        if (((i64)((*b).mode)==(i64)1) || ((i64)((*b).mode)==(i64)3)) {
            regcode = ax_genss_getregcoder((i64)((*a).reg));
            am = ax_genss_genrm(b,regcode);
            ax_genss_checkhighreg(a);
            ax_genss_checkhighreg(b);
            ax_genss_setopsize(a);
            opc = ((code << (i64)3) | (((i64)((u64)((*a).size)) == (i64)1)?(i64)2:(i64)3));
            ax_genss_genrex();
            ax_genss_genbyte(opc);
            ax_genss_genamode(b,am);
        }else if (((i64)((*b).mode)==(i64)2)) {
            //doregimm:
L359 :;
;
            if (!!((*b).labeldef)) {
                if (((code < (i64)0) || (code > (i64)7))) {
                    ax_lib_gerror((byte*)"non-add arith/label");
                };
                if (((i64)((u64)((*a).size)) < (i64)4)) {
                    ax_lib_gerror((byte*)"add imm/size");
                };
                am = ax_genss_genrm(a,code);
                ax_genss_setopsize(a);
                ax_genss_genrex();
                ax_genss_genbyte((i64)129);
                ax_genss_genamode(a,am);
                ax_genss_genopnd(b,(i64)4);
                return;
            };
            x = (*b).value;
            dispsize = (i64)1;
            if (((i64)((u64)((*a).size)) == (i64)1)) {
                opc = (i64)128;
            } else if ((((i64)-128 <= x) && (x <= (i64)127))) {
                opc = (i64)131;
            } else {
                if (!((((i64)-2147483648 <= x) && (x <= (i64)4294967295)))) {
                    ax_lib_gerror((byte*)"3:exceeding word32 value");
                };
                opc = (i64)129;
                dispsize = (((i64)((u64)((*a).size)) == (i64)2)?(i64)2:(i64)4);
            };
            am = ax_genss_genrm(a,code);
            ax_genss_checkhighreg(a);
            ax_genss_setopsize(a);
            ax_genss_genrex();
            ax_genss_genbyte(opc);
            ax_genss_genamode(a,am);
            if ((dispsize==(i64)1)) {
                ax_genss_genbyte(x);
            }else if ((dispsize==(i64)2)) {
                ax_genss_genword(x);
            }else if ((dispsize==(i64)4)) {
                ax_genss_gendword(x);
            };
        } else {
            ax_lib_gerror((byte*)"ADD reg,???");
        };
    }else if (((i64)((*a).mode)==(i64)3)) {
        if (((i64)((*b).mode)==(i64)1)) {
            regcode = ax_genss_getregcoder((i64)((*b).reg));
            am = ax_genss_genrm(a,regcode);
            ax_genss_checkhighreg(b);
            ax_genss_setopsize(b);
            opc = ((code << (i64)3) | (((i64)((u64)((*b).size)) == (i64)1)?(i64)0:(i64)1));
            ax_genss_genrex();
            ax_genss_genbyte(opc);
            ax_genss_genamode(a,am);
        }else if (((i64)((*b).mode)==(i64)2)) {
            goto L359 ;
;
        } else {
            ax_lib_gerror((byte*)"ADD mem,???");
        };
    } else {
        ax_lib_gerror((byte*)"Can't add to this opnd");
    };
}

static void ax_genss_do_mov(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b) {
    i64 regcode;
    i64 am;
    i64 value;
    if (((i64)((*a).mode)==(i64)1)) {
        if (((i64)((*b).mode)==(i64)1) || ((i64)((*b).mode)==(i64)3)) {
            if ((((u64)((*a).size) != (u64)((*b).size)) && !!((u64)((*b).size)))) {
                ax_lib_gerror((byte*)"Opnd size mismatch");
            };
            ax_genss_checkhighreg(a);
            ax_genss_checkhighreg(b);
            regcode = ax_genss_getregcoder((i64)((*a).reg));
            am = ax_genss_genrm(b,regcode);
            ax_genss_setopsize(a);
            ax_genss_genrex();
            ax_genss_genbyte((((i64)((u64)((*a).size)) == (i64)1)?(i64)138:(i64)139));
            ax_genss_genamode(b,am);
        }else if (((i64)((*b).mode)==(i64)2)) {
            value = (*b).value;
            regcode = ax_genss_getregcodeb((i64)((*a).reg));
            if ((!!((*b).labeldef) && ((i64)((u64)((*a).size)) <= (i64)2))) {
                ax_lib_gerror((byte*)"mov imm?");
            };
            if (((i64)((*a).size)==(i64)1)) {
                ax_genss_checkhighreg(a);
                if (((i64)((*a).reg)==(i64)6) || ((i64)((*a).reg)==(i64)4) || ((i64)((*a).reg)==(i64)15) || ((i64)((*a).reg)==(i64)16)) {
                    ax_genss_rex |= (i64)64;
                };
                if (!((((i64)-128 <= value) && (value <= (i64)255)))) {
                    ax_lib_gerror((byte*)"exceeding byte value");
                };
                ax_genss_genrex();
                ax_genss_genbyte(((i64)176 + regcode));
                ax_genss_genbyte(value);
            }else if (((i64)((*a).size)==(i64)2)) {
                if (!((((i64)-32768 <= value) && (value <= (i64)65535)))) {
                    ax_lib_gerror((byte*)"exceeding word16 value");
                };
                ax_genss_genbyte((i64)102);
                ax_genss_genrex();
                ax_genss_genbyte(((i64)184 + regcode));
                ax_genss_genword(value);
            }else if (((i64)((*a).size)==(i64)4)) {
                if (!!((*b).labeldef)) {
                    ax_genss_genrex();
                    ax_genss_genbyte(((i64)184 + regcode));
                    ax_genss_genopnd(b,(i64)4);
                } else {
                    if (!((((i64)-2147483648 <= value) && (value <= (i64)((u64)4294967295u))))) {
                        msysnewc_m_print_startcon();
                        msysnewc_m_print_i64(value,NULL);
                        msysnewc_m_print_ptr((void *)(value),NULL);
                        msysnewc_m_print_newline();
                        msysnewc_m_print_end();
                        ;
                        ax_lib_gerror((byte*)"1:exceeding word32 value");
                    };
                    //doreg32:
L360 :;
;
                    ax_genss_genrex();
                    ax_genss_genbyte(((i64)184 + regcode));
                    ax_genss_gendword(value);
                };
            } else {
                if (!!((*b).labeldef)) {
                    ax_genss_rex |= (i64)8;
                    ax_genss_genrex();
                    ax_genss_genbyte(((i64)184 + regcode));
                    ax_genss_genopnd(b,(i64)8);
                } else {
                    if (((value >= (i64)0) && (value <= (i64)4294967295))) {
                        goto L360 ;
;
                    };
                    ax_genss_rex |= (i64)8;
                    ax_genss_genrex();
                    ax_genss_genbyte(((i64)184 + regcode));
                    ax_genss_genqword(value);
                };
            };
        } else {
            ax_lib_gerror((byte*)"MOV REG/??");
        };
    }else if (((i64)((*a).mode)==(i64)3)) {
        if (((i64)((*b).mode)==(i64)1)) {
            if ((((u64)((*a).size) != (u64)((*b).size)) && !!((u64)((*a).size)))) {
                ax_lib_gerror((byte*)"Opnd size mismatch");
            };
            regcode = ax_genss_getregcoder((i64)((*b).reg));
            ax_genss_checkhighreg(b);
            am = ax_genss_genrm(a,regcode);
            ax_genss_setopsize(b);
            ax_genss_genrex();
            ax_genss_genbyte((((i64)((u64)((*b).size)) == (i64)1)?(i64)136:(i64)137));
            ax_genss_genamode(a,am);
        }else if (((i64)((*b).mode)==(i64)2)) {
            value = (*b).value;
            am = ax_genss_genrm(a,(i64)0);
            if ((!!((*b).labeldef) && ((i64)((u64)((*a).size)) <= (i64)2))) {
                ax_lib_gerror((byte*)"mov imm?");
            };
            if (((i64)((u64)((*a).size)) == (i64)0)) {
                (*a).size = (u64)((i64)1);
            };
            if (((i64)((*a).size)==(i64)0) || ((i64)((*a).size)==(i64)1)) {
                if (!((((i64)-128 <= value) && (value <= (i64)255)))) {
                    ax_lib_gerror((byte*)"exceeding byte value");
                };
                ax_genss_setopsize(a);
                ax_genss_genrex();
                ax_genss_genbyte((i64)198);
                ax_genss_genamode(a,am);
                ax_genss_genbyte(value);
            }else if (((i64)((*a).size)==(i64)2)) {
                if (!((((i64)-32768 <= value) && (value <= (i64)65535)))) {
                    ax_lib_gerror((byte*)"exceeding word16 value");
                };
                ax_genss_setopsize(a);
                ax_genss_genrex();
                ax_genss_genbyte((i64)199);
                ax_genss_genamode(a,am);
                ax_genss_genword(value);
            }else if (((i64)((*a).size)==(i64)4) || ((i64)((*a).size)==(i64)8)) {
                if (!(!!((*b).labeldef))) {
                    if (!((((i64)-2147483647 <= value) && (value <= (i64)4294967295)))) {
                        ax_lib_gerror((byte*)"2:exceeding word32 value");
                    };
                };
                ax_genss_setopsize(a);
                ax_genss_genrex();
                ax_genss_genbyte((i64)199);
                ax_genss_genamode(a,am);
                ax_genss_genopnd(b,(i64)4);
            };
        } else {
            ax_lib_gerror((byte*)"MOV MEM/?");
        };
    } else {
        ax_lib_gerror((byte*)"MOV ?/..");
    };
}

static i64 ax_genss_getregcoder(i64 reg) {
    i64 regcode;
    regcode = (i64)(ax_tables_regcodes[(reg)]);
    if ((regcode >= (i64)8)) {
        regcode -= (i64)8;
        ax_genss_rex |= (i64)4;
    };
    return regcode;
}

static i64 ax_genss_getregcodeb(i64 reg) {
    i64 regcode;
    regcode = (i64)(ax_tables_regcodes[(reg)]);
    if ((regcode >= (i64)8)) {
        regcode -= (i64)8;
        ax_genss_rex |= (i64)1;
    };
    return regcode;
}

static i64 ax_genss_getregcodebx(i64 reg) {
    i64 regcode;
    regcode = (reg - (i64)1);
    if ((regcode >= (i64)8)) {
        regcode -= (i64)8;
        ax_genss_rex |= (i64)1;
    };
    return regcode;
}

static i64 ax_genss_getregcoderx(i64 reg) {
    i64 regcode;
    regcode = (reg - (i64)1);
    if ((regcode >= (i64)8)) {
        regcode -= (i64)8;
        ax_genss_rex |= (i64)4;
    };
    return regcode;
}

static void ax_genss_do_lea(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b) {
    i64 regcode;
    i64 am;
    if (!((((i64)((u64)((*a).mode)) == (i64)1) && ((i64)((u64)((*b).mode)) == (i64)3)))) {
        ax_lib_gerror((byte*)"LEA not reg/mem");
    };
    if (((i64)((u64)((*a).size)) < (i64)4)) {
        ax_lib_gerror((byte*)"LEA size error");
    };
    regcode = ax_genss_getregcoder((i64)((*a).reg));
    am = ax_genss_genrm(b,regcode);
    ax_genss_setopsize(a);
    ax_genss_genrex();
    ax_genss_genbyte((i64)141);
    ax_genss_genamode(b,am);
}

static void ax_genss_do_movsx(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 opc) {
    i64 am;
    i64 regcode;
    if (((i64)((u64)((*a).mode)) != (i64)1)) {
        ax_lib_gerror((byte*)"movsx not reg");
    };
    if ((((i64)((u64)((*a).size)) == (i64)8) && ((i64)((u64)((*b).size)) == (i64)4))) {
        if ((opc == (i64)190)) {
            ax_genss_do_movsxd(a,b);
        } else {
            a = ax_lib_regtable[((i64)((*a).reg))-1][((i64)4)-1];
            ax_genss_do_mov(a,b);
        };
        return;
    };
    if ((((i64)((u64)((*a).size)) == (i64)1) || ((u64)((*a).size) <= (u64)((*b).size)))) {
        ax_lib_gerror((byte*)"movsx size error");
    };
    if (((opc == (i64)182) && ((i64)((u64)((*b).size)) == (i64)4))) {
        ax_lib_gerror((byte*)"movsx 4=>8 bytes?");
    };
    if (((i64)((*b).mode)==(i64)1)) {
    }else if (((i64)((*b).mode)==(i64)3)) {
        if (((i64)((u64)((*b).size)) == (i64)0)) {
            ax_lib_gerror((byte*)"movsx need size prefix");
        };
        if (((i64)((u64)((*b).size)) == (i64)8)) {
            ax_lib_gerror((byte*)"movsx size 8");
        };
    } else {
        ax_lib_gerror((byte*)"movsx not reg/mem");
    };
    regcode = ax_genss_getregcoder((i64)((*a).reg));
    am = ax_genss_genrm(b,regcode);
    ax_genss_setopsize(a);
    ax_genss_checkhighreg(b);
    ax_genss_genrex();
    ax_genss_genbyte((i64)15);
    ax_genss_genbyte((((i64)((u64)((*b).size)) == (i64)1)?opc:(opc + (i64)1)));
    ax_genss_genamode(b,am);
}

static void ax_genss_checkhighreg(struct ax_decls_opndrec * a) {
    if (((i64)((u64)((*a).mode)) == (i64)1)) {
        if (((i64)((*a).reg)==(i64)6) || ((i64)((*a).reg)==(i64)4) || ((i64)((*a).reg)==(i64)15) || ((i64)((*a).reg)==(i64)16)) {
            ax_genss_rex |= (i64)64;
        };
    };
}

static void ax_genss_do_exch(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b) {
    i64 regcode;
    i64 am;
    if ((((((i64)((u64)((*a).mode)) == (i64)1) && ((i64)((u64)((*b).mode)) == (i64)1)) && (((i64)((u64)((*a).reg)) == (i64)1) || ((i64)((u64)((*b).reg)) == (i64)1))) && ((i64)((u64)((*a).size)) != (i64)1))) {
        if (((i64)((u64)((*a).reg)) != (i64)1)) {
            {struct ax_decls_opndrec *  temp = a; a = b; b = temp; };
        };
        if (((u64)((*a).size) != (u64)((*b).size))) {
            ax_lib_gerror((byte*)"exch size");
        };
        ax_genss_setopsize(a);
        regcode = ax_genss_getregcodeb((i64)((*b).reg));
        ax_genss_genrex();
        ax_genss_genbyte(((i64)144 + regcode));
        return;
    };
    if (((i64)((u64)((*a).mode)) == (i64)3)) {
        {struct ax_decls_opndrec *  temp = a; a = b; b = temp; };
    };
    if (!((((i64)((u64)((*a).mode)) == (i64)1) && (((i64)((u64)((*b).mode)) == (i64)1) || ((i64)((u64)((*b).mode)) == (i64)3))))) {
        ax_lib_gerror((byte*)"exch opnds");
    };
    if ((((i64)((u64)((*b).size)) == (i64)0) && ((i64)((u64)((*b).mode)) == (i64)3))) {
        (*b).size = (u64)((*a).size);
    };
    if (((u64)((*a).size) != (u64)((*b).size))) {
        ax_lib_gerror((byte*)"exch size");
    };
    if (((i64)((u64)((*a).size)) == (i64)1)) {
        ax_genss_checkhighreg(a);
        ax_genss_checkhighreg(b);
    };
    regcode = ax_genss_getregcoder((i64)((*a).reg));
    am = ax_genss_genrm(b,regcode);
    ax_genss_setopsize(a);
    ax_genss_genrex();
    ax_genss_genbyte((((i64)((u64)((*a).size)) == (i64)1)?(i64)134:(i64)135));
    ax_genss_genamode(b,am);
}

static void ax_genss_do_movsxd(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b) {
    i64 regcode;
    i64 am;
    if ((((i64)((u64)((*b).mode)) == (i64)3) && ((i64)((u64)((*b).size)) == (i64)0))) {
        (*b).size = (u64)((i64)4);
    };
    if ((((i64)((u64)((*a).size)) != (i64)8) || ((i64)((u64)((*b).size)) > (i64)4))) {
        ax_lib_gerror((byte*)"movsxd size");
    };
    if ((((i64)((u64)((*a).mode)) != (i64)1) || (((i64)((u64)((*b).mode)) != (i64)1) && ((i64)((u64)((*b).mode)) != (i64)3)))) {
        ax_lib_gerror((byte*)"movsxd opnds");
    };
    regcode = ax_genss_getregcoder((i64)((*a).reg));
    am = ax_genss_genrm(b,regcode);
    ax_genss_setopsize(a);
    ax_genss_genrex();
    ax_genss_genbyte((i64)99);
    ax_genss_genamode(b,am);
}

static void ax_genss_do_imul2(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b) {
    i64 regcode;
    i64 am;
    i64 opc;
    i64 value;
    if (((i64)((u64)((*a).mode)) != (i64)1)) {
        ax_lib_gerror((byte*)"imul2 opnds");
    };
    if (((i64)((u64)((*b).size)) == (i64)0)) {
        (*b).size = (u64)((*a).size);
    };
    if (((i64)((u64)((*a).size)) == (i64)1)) {
        ax_lib_gerror((byte*)"imul2 byte");
    };
    if (((i64)((*b).mode)==(i64)1) || ((i64)((*b).mode)==(i64)3)) {
        if (((u64)((*a).size) != (u64)((*b).size))) {
            ax_lib_gerror((byte*)"imul2 size");
        };
        regcode = ax_genss_getregcoder((i64)((*a).reg));
        am = ax_genss_genrm(b,regcode);
        ax_genss_setopsize(a);
        ax_genss_genrex();
        ax_genss_genbyte((i64)15);
        ax_genss_genbyte((i64)175);
        ax_genss_genamode(b,am);
    }else if (((i64)((*b).mode)==(i64)2)) {
        if (!!((*b).labeldef)) {
            ax_lib_gerror((byte*)"mul/label");
        };
        value = (*b).value;
        regcode = ax_genss_getregcoder((i64)((*a).reg));
        regcode = ax_genss_getregcodeb((i64)((*a).reg));
        opc = (((i64)192 + (regcode << (i64)3)) + regcode);
        ax_genss_setopsize(a);
        ax_genss_genrex();
        if ((((i64)-128 <= value) && (value <= (i64)127))) {
            ax_genss_genbyte((i64)107);
            ax_genss_genbyte(opc);
            ax_genss_genbyte(value);
        } else if (((i64)((u64)((*a).size)) == (i64)2)) {
            ax_genss_genbyte((i64)105);
            ax_genss_genbyte(opc);
            ax_genss_genword(value);
        } else {
            ax_genss_genbyte((i64)105);
            ax_genss_genbyte(opc);
            ax_genss_gendword(value);
        };
    } else {
        ax_lib_gerror((byte*)"imul2 opnds");
    };
}

static void ax_genss_do_imul3(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,struct ax_decls_opndrec * c) {
    i64 value;
    i64 regcode1;
    i64 regcode2;
    i64 opc;
    if ((((i64)((u64)((*a).mode)) != (i64)1) || ((i64)((u64)((*b).mode)) != (i64)1))) {
        ax_lib_gerror((byte*)"imul3 opnds");
    };
    if (((i64)((u64)((*a).size)) == (i64)1)) {
        ax_lib_gerror((byte*)"imul3 byte");
    };
    if (((i64)((u64)((*c).mode)) != (i64)2)) {
        ax_lib_gerror((byte*)"imul3 not imm");
    };
    value = (*c).value;
    regcode1 = ax_genss_getregcoder((i64)((*a).reg));
    regcode2 = ax_genss_getregcodeb((i64)((*b).reg));
    opc = (((i64)192 + (regcode1 << (i64)3)) + regcode2);
    ax_genss_setopsize(a);
    ax_genss_genrex();
    if ((((i64)-128 <= value) && (value <= (i64)127))) {
        ax_genss_genbyte((i64)107);
        ax_genss_genbyte(opc);
        ax_genss_genbyte(value);
    } else if (((i64)((u64)((*a).size)) == (i64)2)) {
        ax_genss_genbyte((i64)105);
        ax_genss_genbyte(opc);
        ax_genss_genword(value);
    } else {
        ax_genss_genbyte((i64)105);
        ax_genss_genbyte(opc);
        ax_genss_gendword(value);
    };
}

static void ax_genss_do_shift(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 opc) {
    i64 am;
    i64 w;
    if ((((i64)((u64)((*a).mode)) != (i64)1) && ((i64)((u64)((*a).mode)) != (i64)3))) {
        ax_lib_gerror((byte*)"shift opnds1?");
    };
    am = ax_genss_genrm(a,opc);
    ax_genss_checkhighreg(a);
    ax_genss_setopsize(a);
    ax_genss_genrex();
    w = (((i64)((u64)((*a).size)) == (i64)1)?(i64)0:(i64)1);
    if (((i64)((*b).mode)==(i64)2)) {
        if (!!((*b).labeldef)) {
            ax_lib_gerror((byte*)"shift/label");
        };
        if (((*b).value == (i64)1)) {
            ax_genss_genbyte(((i64)208 + w));
            ax_genss_genamode(a,am);
        } else {
            ax_genss_genbyte(((i64)192 + w));
            ax_genss_genamode(a,am);
            ax_genss_genbyte((*b).value);
        };
    }else if (((i64)((*b).mode)==(i64)1)) {
        if ((((i64)((u64)((*b).reg)) != (i64)11) || ((i64)((u64)((*b).size)) != (i64)1))) {
            ax_lib_gerror((byte*)"cl or b10 needed");
        };
        ax_genss_genbyte(((i64)210 + w));
        ax_genss_genamode(a,am);
    } else {
        ax_lib_gerror((byte*)"shift opnds2?");
    };
}

static void ax_genss_do_test(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b) {
    i64 value;
    i64 opc;
    i64 am;
    i64 regcode;
    if (((((i64)((u64)((*a).mode)) == (i64)1) && ((i64)((u64)((*a).reg)) == (i64)1)) && ((i64)((u64)((*b).mode)) == (i64)2))) {
        value = (*b).value;
        if (((i64)((*a).size)==(i64)1)) {
            ax_genss_genbyte((i64)168);
            ax_genss_genbyte(value);
        }else if (((i64)((*a).size)==(i64)2)) {
            ax_genss_genbyte((i64)102);
            ax_genss_genbyte((i64)169);
            ax_genss_genword(value);
        }else if (((i64)((*a).size)==(i64)4)) {
            ax_genss_genbyte((i64)169);
            ax_genss_gendword(value);
        } else {
            ax_genss_genbyte((i64)72);
            ax_genss_genbyte((i64)169);
            ax_genss_gendword(value);
        };
    } else if (((((i64)((u64)((*a).mode)) == (i64)1) || ((i64)((u64)((*a).mode)) == (i64)3)) && ((i64)((u64)((*b).mode)) == (i64)2))) {
        opc = (((i64)((u64)((*a).size)) == (i64)1)?(i64)246:(i64)247);
        value = (*b).value;
        am = ax_genss_genrm(a,(i64)0);
        ax_genss_checkhighreg(a);
        ax_genss_setopsize(a);
        ax_genss_genrex();
        ax_genss_genbyte(opc);
        ax_genss_genamode(a,am);
        if (((i64)((*a).size)==(i64)1)) {
            ax_genss_genbyte(value);
        }else if (((i64)((*a).size)==(i64)2)) {
            ax_genss_genword(value);
        } else {
            ax_genss_gendword(value);
        };
    } else if ((((i64)((u64)((*a).mode)) == (i64)1) && (((i64)((u64)((*b).mode)) == (i64)1) || ((i64)((u64)((*b).mode)) == (i64)3)))) {
        //doregmem:
L361 :;
;
        regcode = ax_genss_getregcoder((i64)((*a).reg));
        am = ax_genss_genrm(b,regcode);
        ax_genss_checkhighreg(a);
        ax_genss_checkhighreg(b);
        ax_genss_setopsize(a);
        ax_genss_genrex();
        ax_genss_genbyte((((i64)((u64)((*a).size)) == (i64)1)?(i64)132:(i64)133));
        ax_genss_genamode(b,am);
    } else if ((((i64)((u64)((*a).mode)) == (i64)3) && ((i64)((u64)((*b).mode)) == (i64)1))) {
        {struct ax_decls_opndrec *  temp = a; a = b; b = temp; };
        goto L361 ;
;
    } else {
        ax_lib_gerror((byte*)"test opnds");
    };
}

static void ax_genss_do_loop(struct ax_decls_opndrec * a,i64 opc) {
    i64 offset;
    offset = ax_genss_getrel32((*a).labeldef,(ax_genss_getcurrdatalen((i64)9) + (i64)1));
    if ((offset < (i64)0)) {
        if ((offset < (i64)-126)) {
            ax_lib_gerror((byte*)"loop jmp out of range");
        };
        ax_genss_genbyte(opc);
        ax_genss_genbyte(offset);
    } else {
        ax_lib_gerror((byte*)"Can't do loopxx fwd jump");
    };
}

static void ax_genss_do_jcxz(struct ax_decls_opndrec * a,i64 opsize) {
    i64 offset;
    offset = ax_genss_getrel32((*a).labeldef,(ax_genss_getcurrdatalen((i64)10) + (i64)1));
    if ((offset < (i64)0)) {
        if ((offset < (i64)-126)) {
            ax_lib_gerror((byte*)"jcxz jmp out of range");
        };
        if ((opsize == (i64)4)) {
            ax_genss_genbyte((i64)103);
        };
        ax_genss_genbyte((i64)227);
        ax_genss_genbyte(offset);
    } else {
        ax_lib_gerror((byte*)"Can't do jcxz fwd jump");
    };
}

static void ax_genss_do_setcc(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b) {
    i64 am;
    if (((((i64)((u64)((*b).mode)) != (i64)1) && ((i64)((u64)((*b).reg)) != (i64)3)) || ((i64)((u64)((*b).size)) > (i64)1))) {
        ax_lib_gerror((byte*)"setcc opnd/size");
    };
    am = ax_genss_genrm(b,(i64)0);
    ax_genss_checkhighreg(b);
    ax_genss_genrex();
    ax_genss_genbyte((i64)15);
    ax_genss_genbyte(((i64)144 + (*a).value));
    ax_genss_genamode(b,am);
}

static void ax_genss_do_movxmm(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 size) {
    i64 am;
    i64 regcode;
    i64 regcode1;
    i64 regcode2;
    if (((i64)((*a).mode)==(i64)1)) {
        if (((i64)((*b).mode)==(i64)5)) {
            if (((i64)((u64)((*a).size)) != size)) {
                ax_lib_gerror((byte*)"1:movdq size");
            };
            regcode = ax_genss_getregcoderx((i64)((*b).reg));
            am = ax_genss_genrm(a,regcode);
            ax_genss_setopsize(a);
            ax_genss_genbyte((i64)102);
            ax_genss_genrex();
            ax_genss_genbyte((i64)15);
            ax_genss_genbyte((i64)126);
            ax_genss_genamode(b,am);
        } else {
            ax_lib_gerror((byte*)"movdq reg,?");
        };
    }else if (((i64)((*a).mode)==(i64)5)) {
        if (((i64)((*b).mode)==(i64)1)) {
            if (((i64)((u64)((*b).size)) != size)) {
                ax_lib_gerror((byte*)"3:movdq size");
            };
            regcode = ax_genss_getregcoderx((i64)((*a).reg));
            am = ax_genss_genrm(b,regcode);
            ax_genss_setopsize(b);
            ax_genss_genbyte((i64)102);
            ax_genss_genrex();
            ax_genss_genbyte((i64)15);
            ax_genss_genbyte((i64)110);
            ax_genss_genamode(a,am);
        }else if (((i64)((*b).mode)==(i64)5)) {
            regcode1 = ax_genss_getregcoderx((i64)((*a).reg));
            regcode2 = ax_genss_getregcodebx((i64)((*b).reg));
            ax_genss_genbyte((i64)243);
            ax_genss_genrex();
            ax_genss_genbyte((i64)15);
            ax_genss_genbyte((i64)126);
            ax_genss_genbyte((((i64)192 + (regcode1 << (i64)3)) + regcode2));
        }else if (((i64)((*b).mode)==(i64)3)) {
            if ((!!((u64)((*b).size)) && ((i64)((u64)((*b).size)) != size))) {
                ax_lib_gerror((byte*)"4:movdq size");
            };
            regcode = ax_genss_getregcoderx((i64)((*a).reg));
            am = ax_genss_genrm(b,regcode);
            if ((size == (i64)4)) {
                ax_genss_genbyte((i64)102);
                ax_genss_genrex();
                ax_genss_genbyte((i64)15);
                ax_genss_genbyte((i64)110);
            } else {
                ax_genss_genbyte((i64)243);
                ax_genss_genrex();
                ax_genss_genbyte((i64)15);
                ax_genss_genbyte((i64)126);
            };
            ax_genss_genamode(b,am);
        } else {
            ax_lib_gerror((byte*)"movdq xreg,?");
        };
    }else if (((i64)((*a).mode)==(i64)3)) {
        if (((i64)((*b).mode)==(i64)5)) {
            if ((!!((u64)((*a).size)) && ((i64)((u64)((*a).size)) != size))) {
                ax_lib_gerror((byte*)"5:movdq size");
            };
            regcode = ax_genss_getregcoderx((i64)((*b).reg));
            am = ax_genss_genrm(a,regcode);
            if ((size == (i64)4)) {
                ax_genss_genbyte((i64)102);
                ax_genss_genrex();
                ax_genss_genbyte((i64)15);
                ax_genss_genbyte((i64)126);
            } else {
                ax_genss_genbyte((i64)102);
                ax_genss_genrex();
                ax_genss_genbyte((i64)15);
                ax_genss_genbyte((i64)214);
            };
            ax_genss_genamode(a,am);
        } else {
            ax_lib_gerror((byte*)"movdq mem,?");
        };
    } else {
        ax_lib_gerror((byte*)"movdq opnds");
    };
}

static void ax_genss_do_arithxmm(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 prefix,i64 opc) {
    i64 am;
    i64 regcode;
    if ((((i64)((u64)((*a).mode)) != (i64)5) || (((i64)((u64)((*b).mode)) != (i64)5) && ((i64)((u64)((*b).mode)) != (i64)3)))) {
        ax_lib_gerror((byte*)"arithxmm opnds");
    };
    if (((i64)((u64)((*b).mode)) == (i64)5)) {
        regcode = ax_genss_getregcoderx((i64)((*a).reg));
        am = ax_genss_genrm(b,regcode);
        if (!!(prefix)) {
            ax_genss_genbyte(prefix);
        };
        ax_genss_genrex();
        ax_genss_genbyte((i64)15);
        ax_genss_genbyte(opc);
        ax_genss_genamode(a,am);
    } else {
        regcode = ax_genss_getregcoderx((i64)((*a).reg));
        am = ax_genss_genrm(b,regcode);
        if (!!(prefix)) {
            ax_genss_genbyte(prefix);
        };
        ax_genss_genrex();
        ax_genss_genbyte((i64)15);
        ax_genss_genbyte(opc);
        ax_genss_genamode(b,am);
    };
}

static void ax_genss_do_logicxmm(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 opc,i64 size) {
    i64 am;
    i64 regcode;
    if ((((i64)((u64)((*a).mode)) != (i64)5) || (((i64)((u64)((*b).mode)) != (i64)5) && ((i64)((u64)((*b).mode)) != (i64)3)))) {
        ax_lib_gerror((byte*)"logicxmm opnds");
    };
    if ((size == (i64)8)) {
        ax_genss_genbyte((i64)102);
    };
    if (((i64)((u64)((*b).mode)) == (i64)5)) {
        regcode = ax_genss_getregcoderx((i64)((*a).reg));
        am = ax_genss_genrm(b,regcode);
        ax_genss_genrex();
        ax_genss_genbyte((i64)15);
        ax_genss_genbyte(opc);
        ax_genss_genamode(b,am);
    } else {
        regcode = ax_genss_getregcoderx((i64)((*a).reg));
        am = ax_genss_genrm(b,regcode);
        ax_genss_genrex();
        ax_genss_genbyte((i64)15);
        ax_genss_genbyte(opc);
        ax_genss_genamode(b,am);
    };
}

static void ax_genss_do_convertfloat(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 prefix) {
    i64 am;
    i64 regcode;
    if ((((i64)((u64)((*a).mode)) != (i64)5) || (((i64)((u64)((*b).mode)) != (i64)5) && ((i64)((u64)((*b).mode)) != (i64)3)))) {
        ax_lib_gerror((byte*)"convertfloat opnds");
    };
    ax_genss_genbyte(prefix);
    if (((i64)((u64)((*a).mode)) == (i64)5)) {
        regcode = ax_genss_getregcoderx((i64)((*a).reg));
        am = ax_genss_genrm(b,regcode);
        ax_genss_genrex();
        ax_genss_genbyte((i64)15);
        ax_genss_genbyte((i64)90);
        ax_genss_genamode(b,am);
    } else {
        regcode = ax_genss_getregcoderx((i64)((*b).reg));
        am = ax_genss_genrm(a,regcode);
        ax_genss_genrex();
        ax_genss_genbyte((i64)15);
        ax_genss_genbyte((i64)90);
        ax_genss_genamode(b,am);
    };
}

static void ax_genss_do_fix(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 prefix,i64 opc) {
    i64 am;
    i64 regcode;
    if ((((i64)((u64)((*a).mode)) != (i64)1) || (((i64)((u64)((*b).mode)) != (i64)5) && ((i64)((u64)((*b).mode)) != (i64)3)))) {
        ax_lib_gerror((byte*)"fix opnds");
    };
    ax_genss_genbyte(prefix);
    if (((i64)((u64)((*b).mode)) == (i64)5)) {
        regcode = ax_genss_getregcoder((i64)((*a).reg));
        am = ax_genss_genrm(b,regcode);
        ax_genss_setopsize(a);
    } else {
        regcode = ax_genss_getregcoder((i64)((*a).reg));
        am = ax_genss_genrm(b,regcode);
        ax_genss_setopsize(a);
    };
    ax_genss_genrex();
    ax_genss_genbyte((i64)15);
    ax_genss_genbyte(opc);
    ax_genss_genamode(b,am);
}

static void ax_genss_do_float(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 prefix) {
    i64 am;
    i64 regcode;
    if ((((i64)((u64)((*a).mode)) != (i64)5) || (((i64)((u64)((*b).mode)) != (i64)1) && ((i64)((u64)((*b).mode)) != (i64)3)))) {
        ax_lib_gerror((byte*)"float opnds");
    };
    if (((i64)((u64)((*b).mode)) == (i64)3)) {
        if (((i64)((u64)((*b).size)) == (i64)0)) {
            (*b).size = (u64)((i64)4);
        };
        if ((((i64)((u64)((*b).size)) != (i64)4) && ((i64)((u64)((*b).size)) != (i64)8))) {
            ax_lib_gerror((byte*)"float size");
        };
    };
    ax_genss_genbyte(prefix);
    regcode = ax_genss_getregcoderx((i64)((*a).reg));
    am = ax_genss_genrm(b,regcode);
    ax_genss_setopsize(b);
    ax_genss_genrex();
    ax_genss_genbyte((i64)15);
    ax_genss_genbyte((i64)42);
    ax_genss_genamode(b,am);
}

static void ax_genss_do_call(struct ax_decls_opndrec * a) {
    i64 am;
    if (((i64)((*a).mode)==(i64)2)) {
        ax_genss_genbyte((i64)232);
        ax_genss_genrel32(a);
    } else {
        if (((i64)((*a).size)==(i64)0)) {
            (*a).size = (u64)((i64)8);
        }else if (((i64)((*a).size)==(i64)1) || ((i64)((*a).size)==(i64)2) || ((i64)((*a).size)==(i64)4)) {
            ax_lib_gerror((byte*)"call[]size");
        };
        am = ax_genss_genrm(a,(i64)2);
        ax_genss_setopsize(a);
        ax_genss_setaddrsize(a);
        ax_genss_genrex();
        ax_genss_genbyte((i64)255);
        ax_genss_genamode(a,am);
    };
}

static void ax_genss_do_jmp(struct ax_decls_opndrec * a,struct ax_lib_mclrec * m) {
    i64 am;
    i64 offset;
    i64 shortjmp;
    if (((i64)((*a).mode)==(i64)2)) {
        offset = ax_genss_getrel32((*a).labeldef,(ax_genss_getcurrdatalen((i64)11) + (i64)1));
        if (((offset < (i64)0) && (offset > (i64)-126))) {
            ax_genss_genbyte((i64)235);
            ax_genss_genbyte(offset);
        } else {
            shortjmp = (i64)0;
            if ((offset > (i64)0)) {
                shortjmp = ax_genss_checkshortjump(m,(*a).labeldef);
            };
            if (!(!!(shortjmp))) {
                ax_genss_genbyte((i64)233);
                ax_genss_genrel32(a);
            } else {
                ax_genss_genbyte((i64)235);
                ax_genss_genrel8(a);
            };
        };
    } else {
        if (((i64)((*a).size)==(i64)0)) {
            (*a).size = (u64)((i64)8);
        }else if (((i64)((*a).size)==(i64)1) || ((i64)((*a).size)==(i64)2) || ((i64)((*a).size)==(i64)4)) {
            ax_lib_gerror((byte*)"jmp[]size");
        };
        am = ax_genss_genrm(a,(i64)4);
        ax_genss_setopsize(a);
        ax_genss_setaddrsize(a);
        ax_genss_genrex();
        ax_genss_genbyte((i64)255);
        ax_genss_genamode(a,am);
    };
}

static i64 ax_genss_getcurrdatalen(i64 id) {
    if ((ax_genss_currseg == (i64)3)) {
        return ax_decls_ss_zdatalen;
    };
    return ax_lib_bufferlength(ax_genss_currdata);
}

static void ax_genss_do_cmovcc(struct ax_decls_opndrec * c,struct ax_decls_opndrec * a,struct ax_decls_opndrec * b) {
    i64 am;
    i64 regcode;
    if ((((u64)((*a).size) != (u64)((*b).size)) && !!((u64)((*b).size)))) {
        ax_lib_gerror((byte*)"Opnd size mismatch");
    };
    if (((i64)((u64)((*a).size)) == (i64)1)) {
        ax_lib_gerror((byte*)"cmov/byte");
    };
    regcode = ax_genss_getregcoder((i64)((*a).reg));
    am = ax_genss_genrm(b,regcode);
    ax_genss_setopsize(a);
    ax_genss_genrex();
    ax_genss_genbyte((i64)15);
    ax_genss_genbyte(((i64)64 + (*c).value));
    ax_genss_genamode(b,am);
}

static void ax_genss_do_fmem(struct ax_decls_opndrec * a,i64 freal,i64 code) {
    i64 am;
    i64 mf;
    if (((i64)((u64)((*a).mode)) != (i64)3)) {
        ax_lib_gerror((byte*)"fmem/not mem");
    };
    if (!!(freal)) {
        if (((i64)((*a).size)==(i64)4)) {
            mf = (i64)0;
        }else if (((i64)((*a).size)==(i64)8)) {
            mf = (i64)2;
        }else if (((i64)((*a).size)==(i64)16)) {
            mf = (i64)1;
            if ((code==(i64)0)) {
                code = (i64)5;
            }else if ((code==(i64)3)) {
                code = (i64)7;
            } else {
                ax_lib_gerror((byte*)"r80 not allowed");
            };
        } else {
            msysnewc_m_print_startcon();
            msysnewc_m_print_str((byte*)"SIZE=",NULL);
            msysnewc_m_print_u64((*a).size,NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            ax_lib_gerror((byte*)"fmem size");
        };
    } else {
        if (((i64)((*a).size)==(i64)2)) {
            mf = (i64)3;
        }else if (((i64)((*a).size)==(i64)4)) {
            mf = (i64)1;
        }else if (((i64)((*a).size)==(i64)8)) {
            mf = (i64)3;
            if ((code==(i64)0)) {
                code = (i64)5;
            }else if ((code==(i64)3)) {
                code = (i64)7;
            } else {
                ax_lib_gerror((byte*)"fst i64?");
            };
        } else {
            ax_lib_gerror((byte*)"fmem int size");
        };
    };
    am = ax_genss_genrm(a,code);
    ax_genss_genrex();
    ax_genss_genbyte(((i64)217 + (mf << (i64)1)));
    ax_genss_genamode(a,am);
}

static i64 ax_genss_getr32bits(double x) {
    float sx;
    sx = (float)(x);
    return (i64)(*(i32*)&sx);
}

static void ax_genss_genrel8(struct ax_decls_opndrec * a) {
    struct ax_decls_strec *  d;
    d = (*a).labeldef;
    if (((i64)((u64)((*d).reftype)) == (i64)1)) {
        (*d).fwdrefs = ax_genss_addfwdref((*d).fwdrefs,ax_genss_getcurrdatalen((i64)3),(i64)6,(i64)0);
        ax_genss_genbyte((i64)0);
    } else {
        ax_lib_gerror((byte*)"genrel8");
    };
}

static i64 ax_genss_checkshortjump(struct ax_lib_mclrec * m,struct ax_decls_strec * d) {
    i64 n;
    n = (i64)0;
    m = (*m).nextmcl;
    L362 :;
    while ((!!(m) && (n <= (i64)8))) {
        ++n;
        if ((((i64)((u64)((*m).opcode)) == (i64)4) && ((*(*m).a).labeldef == d))) {
            return (i64)1;
        };
        m = (*m).nextmcl;
L363 :;
    }L364 :;
    ;
    return (i64)0;
}

static struct ax_decls_fwdrec * ax_genss_addfwdref(struct ax_decls_fwdrec * p,i64 offset,i64 reltype,i64 seg) {
    struct ax_decls_fwdrec *  q;
    q = (struct ax_decls_fwdrec *)(mlib_pcm_alloc((i64)16));
    (*q).nextfwd = p;
    (*q).offset = offset;
    (*q).reltype = reltype;
    (*q).seg = seg;
    return q;
}

static void ax_genss_switchseg(i64 newseg) {
    if ((newseg == ax_genss_currseg)) {
        return;
    };
    if ((ax_genss_currseg==(i64)1)) {
        ax_decls_ss_coderelocs = ax_genss_currrelocs;
        ax_decls_ss_ncoderelocs = ax_genss_nrelocs;
    }else if ((ax_genss_currseg==(i64)2)) {
        ax_decls_ss_idatarelocs = ax_genss_currrelocs;
        ax_decls_ss_nidatarelocs = ax_genss_nrelocs;
    };
    ax_genss_currseg = newseg;
    if ((ax_genss_currseg==(i64)1)) {
        ax_genss_currdata = ax_decls_ss_code;
        ax_genss_currrelocs = ax_decls_ss_coderelocs;
        ax_genss_nrelocs = ax_decls_ss_ncoderelocs;
    }else if ((ax_genss_currseg==(i64)2)) {
        ax_genss_currdata = ax_decls_ss_idata;
        ax_genss_currrelocs = ax_decls_ss_idatarelocs;
        ax_genss_nrelocs = ax_decls_ss_nidatarelocs;
    }else if ((ax_genss_currseg==(i64)3)) {
        ax_genss_currdata = ax_decls_ss_zdata;
    };
}

static void ax_genss_do_movdqx(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 opc) {
    i64 am;
    i64 regcode;
    if (((i64)((*a).mode)==(i64)5)) {
        if (((i64)((*b).mode)==(i64)5)) {
            regcode = ax_genss_getregcodebx((i64)((*b).reg));
            am = ax_genss_genrm(a,regcode);
            ax_genss_genbyte(opc);
            ax_genss_genrex();
            ax_genss_genbyte((i64)15);
            ax_genss_genbyte((i64)111);
            ax_genss_genamode(a,am);
        }else if (((i64)((*b).mode)==(i64)3)) {
            regcode = ax_genss_getregcoderx((i64)((*a).reg));
            am = ax_genss_genrm(b,regcode);
            ax_genss_genbyte(opc);
            ax_genss_genrex();
            ax_genss_genbyte((i64)15);
            ax_genss_genbyte((i64)111);
            ax_genss_genamode(b,am);
        } else {
            ax_lib_gerror((byte*)"movdqx?");
        };
    }else if (((i64)((*a).mode)==(i64)3)) {
        if (((i64)((*b).mode)==(i64)5)) {
            regcode = ax_genss_getregcoderx((i64)((*b).reg));
            am = ax_genss_genrm(a,regcode);
            ax_genss_genbyte(opc);
            ax_genss_genrex();
            ax_genss_genbyte((i64)15);
            ax_genss_genbyte((i64)127);
            ax_genss_genamode(a,am);
        } else {
            ax_lib_gerror((byte*)"movdqx");
        };
    } else {
        ax_lib_gerror((byte*)"movdqx");
    };
}

static void ax_genss_do_popcnt(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b) {
    i64 am;
    i64 regcode;
    if (((i64)((u64)((*b).mode)) == (i64)3)) {
        if (((i64)((u64)((*b).size)) == (i64)0)) {
            (*b).size = (u64)((i64)8);
        };
    };
    ax_genss_genbyte((i64)243);
    regcode = ax_genss_getregcodebx((i64)((*a).reg));
    am = ax_genss_genrm(b,regcode);
    ax_genss_setopsize(a);
    ax_genss_genrex();
    ax_genss_genbyte((i64)15);
    ax_genss_genbyte((i64)184);
    ax_genss_genamode(b,am);
}

static void ax_genss_do_bsf(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 opc) {
    i64 am;
    i64 regcode;
    if (((i64)((u64)((*b).mode)) == (i64)3)) {
        if (((i64)((u64)((*b).size)) == (i64)0)) {
            (*b).size = (u64)((i64)8);
        };
    };
    if (((u64)((*a).size) != (u64)((*b).size))) {
        ax_lib_gerror((byte*)"bsf size");
    };
    regcode = ax_genss_getregcodebx((i64)((*a).reg));
    am = ax_genss_genrm(b,regcode);
    ax_genss_setopsize(a);
    ax_genss_genrex();
    ax_genss_genbyte((i64)15);
    ax_genss_genbyte(opc);
    ax_genss_genamode(b,am);
}

static void ax_genss_extendsymboltable(void) {
    struct ax_decls_strec * (*oldsymboltable)[];
    i64 oldsymboltablesize;
    i64 i;
    oldsymboltablesize = ax_decls_ss_symboltablesize;
    oldsymboltable = ax_decls_ss_symboltable;
    ax_decls_ss_symboltablesize *= (i64)2;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"EXTENDING SYMBOL TABLE TO",NULL);
    msysnewc_m_print_i64(ax_decls_ss_symboltablesize,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    ax_decls_ss_symboltable = (struct ax_decls_strec * (*)[])(mlib_pcm_alloc(((i64)8 * ax_decls_ss_symboltablesize)));
    L365 :;
    for (i=(i64)1;i<=ax_decls_ss_nsymbols;i+=(i64)1) {
L366 :;
        (*ax_decls_ss_symboltable)[(i)-1] = (*oldsymboltable)[(i)-1];
L367 :;
    }L368 :;
    ;
    mlib_pcm_free((void *)(oldsymboltable),((i64)8 * oldsymboltablesize));
}

static void ax_genss_do_pcmpistri(struct ax_decls_opndrec * a,struct ax_decls_opndrec * b,i64 c,i64 opc) {
    i64 am;
    i64 regcode;
    if ((((i64)((u64)((*a).mode)) != (i64)5) || (((i64)((u64)((*b).mode)) != (i64)5) && ((i64)((u64)((*b).mode)) != (i64)3)))) {
        ax_lib_gerror((byte*)"pcmpistrx opnds");
    };
    ax_genss_genbyte((i64)102);
    if (((i64)((u64)((*b).mode)) == (i64)5)) {
        {struct ax_decls_opndrec *  temp = a; a = b; b = temp; };
        regcode = ax_genss_getregcoderx((i64)((*b).reg));
        am = ax_genss_genrm(a,regcode);
        ax_genss_genrex();
        ax_genss_genbyte((i64)15);
        ax_genss_genbyte((i64)58);
        ax_genss_genbyte(opc);
        ax_genss_genamode(a,am);
    } else {
        regcode = ax_genss_getregcoderx((i64)((*a).reg));
        am = ax_genss_genrm(b,regcode);
        ax_genss_genrex();
        ax_genss_genbyte((i64)15);
        ax_genss_genbyte((i64)58);
        ax_genss_genbyte(opc);
        ax_genss_genamode(b,am);
    };
    ax_genss_genbyte(c);
}

void ax_writeexe_writeexe(byte * outfile) {
    i64 i;
    ax_writeexe_datastart = (ax_writeexe_dataptr = (byte *)(mlib_pcm_allocz(ax_writeexe_filesize)));
    ax_writeexe_writedosstub();
    ax_writeexe_writepesig();
    ax_writeexe_writefileheader();
    ax_writeexe_writeoptheader();
    L369 :;
    for (i=(i64)1;i<=ax_writeexe_nsections;i+=(i64)1) {
L370 :;
        ax_writeexe_writesectionheader(&ax_writeexe_sectiontable[(i)-1]);
L371 :;
    }L372 :;
    ;
    ax_writeexe_writepadding(ax_writeexe_sectiontable[((i64)1)-1].rawoffset);
    L373 :;
    for (i=(i64)1;i<=ax_writeexe_nsections;i+=(i64)1) {
L374 :;
        ax_writeexe_writesectiondata(&ax_writeexe_sectiontable[(i)-1]);
L375 :;
    }L376 :;
    ;
    if (!!(ax_decls_fverbose)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"Writing file:",NULL);
        msysnewc_m_print_str(outfile,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    };
    if ((mlib_writefile(outfile,ax_writeexe_datastart,(ax_writeexe_dataptr - ax_writeexe_datastart)) == (i64)0)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"Error writing exe file (possibly still running)",NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        exit((i64)1);
    };
}

void ax_writeexe_genexe(byte * entrypoint) {
    ax_writeexe_userentrypoint = entrypoint;
    ax_writeexe_loadlibs();
    ax_writeexe_scanst();
    ax_writeexe_getoffsets();
    ax_writeexe_relocdata(&ax_writeexe_sectiontable[((i64)1)-1]);
    ax_writeexe_relocdata(&ax_writeexe_sectiontable[((i64)2)-1]);
}

static void ax_writeexe_loadlibs(void) {
    i64 i;
    i64 hinst;
    byte filename[300];
    L377 :;
    for (i=(i64)1;i<=ax_decls_nsearchlibs;i+=(i64)1) {
L378 :;
        strcpy((i8 *)(filename),(i8 *)(ax_decls_searchlibs[(i)-1]));
        strcat((i8 *)(filename),(i8 *)((byte*)".dll"));
        hinst = (i64)(oswindows_os_getdllinst(filename));
        if ((hinst == (i64)0)) {
            msysnewc_m_print_startcon();
            msysnewc_m_print_str(ax_decls_searchlibs[(i)-1],NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            ax_lib_gerror((byte*)"Can't load search lib");
        };
        ax_writeexe_libinsttable[(i)-1] = hinst;
        ax_writeexe_libinstnames[(i)-1] = mlib_pcm_copyheapstring(filename);
L379 :;
    }L380 :;
    ;
}

struct mlib_strbuffer * ax_writeexe_writessdata(i64 fexe) {
    mlib_gs_init(ax_lib_dest);
    ax_writeexe_showssdata(fexe);
    mlib_gs_line(ax_lib_dest);
    return ax_lib_dest;
}

void ax_writeexe_initsectiontable(void) {
    ax_writeexe_sectiontable[((i64)1)-1].name = (byte*)".text";
    ax_writeexe_sectiontable[((i64)1)-1].segtype = (i64)1;
    ax_writeexe_sectiontable[((i64)1)-1].data = ax_decls_ss_code;
    ax_writeexe_sectiontable[((i64)1)-1].virtsize = ax_lib_bufferlength(ax_decls_ss_code);
    if ((ax_lib_bufferlength(ax_decls_ss_idata) == (i64)0)) {
        ax_lib_addqword(ax_decls_ss_idata,(i64)0);
    };
    ax_writeexe_sectiontable[((i64)2)-1].name = (byte*)".data";
    ax_writeexe_sectiontable[((i64)2)-1].segtype = (i64)2;
    ax_writeexe_sectiontable[((i64)2)-1].data = ax_decls_ss_idata;
    ax_writeexe_sectiontable[((i64)2)-1].virtsize = ax_lib_bufferlength(ax_decls_ss_idata);
    ax_writeexe_sectiontable[((i64)2)-1].rawsize = ax_writeexe_roundtoblock(ax_writeexe_sectiontable[((i64)2)-1].virtsize,(i64)512);
    ax_writeexe_sectiontable[((i64)2)-1].nrelocs = ax_decls_ss_nidatarelocs;
    ax_writeexe_sectiontable[((i64)2)-1].relocs = ax_decls_ss_idatarelocs;
    if ((ax_decls_ss_zdatalen == (i64)0)) {
        ax_decls_ss_zdatalen = (i64)16;
    };
    ax_writeexe_sectiontable[((i64)3)-1].name = (byte*)".bss";
    ax_writeexe_sectiontable[((i64)3)-1].segtype = (i64)3;
    ax_writeexe_sectiontable[((i64)3)-1].virtsize = ax_decls_ss_zdatalen;
    ax_writeexe_sectiontable[((i64)1)-1].rawsize = ax_writeexe_roundtoblock(ax_writeexe_sectiontable[((i64)1)-1].virtsize,(i64)512);
    ax_writeexe_sectiontable[((i64)1)-1].nrelocs = ax_decls_ss_ncoderelocs;
    ax_writeexe_sectiontable[((i64)1)-1].relocs = ax_decls_ss_coderelocs;
    ax_writeexe_sectiontable[((i64)4)-1].name = (byte*)".idata";
    ax_writeexe_sectiontable[((i64)4)-1].segtype = (i64)5;
    ax_writeexe_sectiontable[((i64)4)-1].virtsize = (i64)0;
    ax_writeexe_sectiontable[((i64)4)-1].rawsize = (i64)0;
    ax_writeexe_nsections = (i64)4;
}

static void ax_writeexe_showssdata(i64 fexe) {
    mlib_gs_strln(ax_lib_dest,(!!(fexe)?(byte*)"EXE FORMAT":(byte*)"AFTER GENSS"));
    ax_writeexe_showsections();
    mlib_gs_line(ax_lib_dest);
    ax_writeexe_showsectionrelocs2((byte*)"Idata",ax_decls_ss_idatarelocs,ax_decls_ss_nidatarelocs);
    ax_writeexe_showsectionrelocs2((byte*)"Code",ax_decls_ss_coderelocs,ax_decls_ss_ncoderelocs);
    mlib_gs_str(ax_lib_dest,(byte*)"proc Section Zdata: ");
    mlib_gs_strint(ax_lib_dest,ax_decls_ss_zdatalen);
    mlib_gs_line(ax_lib_dest);
    ax_writeexe_showsectiondata(&ax_writeexe_sectiontable[((i64)2)-1]);
    ax_writeexe_showsectioncode(&ax_writeexe_sectiontable[((i64)1)-1]);
    if (!!(fexe)) {
        ax_writeexe_showsectiondata(&ax_writeexe_sectiontable[((i64)4)-1]);
    };
    ax_writeexe_showsymboltable2();
    ax_writeexe_showimporttable();
    mlib_gs_strln(ax_lib_dest,(byte*)"END OF GENSS");
}

static void ax_writeexe_showsectiondata(struct ax_writeexe_sectionrec * d) {
    i64 i;
    i64 k;
    i64 length;
    i64 bb;
    byte str[128];
    byte str2[128];
    byte *  p;
    byte *  baseaddr;
    i64 av_1;
    mlib_gs_str(ax_lib_dest,(byte*)"proc Section ");
    mlib_gs_str(ax_lib_dest,(*d).name);
    mlib_gs_str(ax_lib_dest,(byte*)" Size:");
    mlib_gs_strint(ax_lib_dest,(*d).virtsize);
    mlib_gs_line(ax_lib_dest);
    mlib_gs_line(ax_lib_dest);
    k = (i64)0;
    if (((*d).segtype != (i64)5)) {
        p = (byte *)(ax_lib_bufferelemptr((*d).data,(i64)0));
    } else {
        p = (*d).bytedata;
    };
    length = (*d).virtsize;
    str[((i64)1)-1] = (u64)0u;
    baseaddr = (byte *)(((i64)4194304 + (*d).virtoffset));
    msysnewc_m_print_startstr(str2);
    msysnewc_m_print_ptr(baseaddr,(byte*)"Z8H");
    msysnewc_m_print_nogap();
    msysnewc_m_print_str((byte*)": ",NULL);
    msysnewc_m_print_end();
    ;
    mlib_gs_str(ax_lib_dest,str2);
    L381 :;
    for (i=(i64)1;i<=length;i+=(i64)1) {
L382 :;
        bb = (i64)((*p++));
        msysnewc_m_print_startstr(str2);
        msysnewc_m_print_i64(bb,(byte*)"z2H");
        msysnewc_m_print_nogap();
        msysnewc_m_print_str((byte*)" ",NULL);
        msysnewc_m_print_end();
        ;
        mlib_gs_str(ax_lib_dest,str2);
        if ((((i64)32 <= bb) && (bb <= (i64)127))) {
            str2[((i64)1)-1] = (u64)(bb);
            str2[((i64)2)-1] = (u64)0u;
            strcat((i8 *)(str),(i8 *)(str2));
        } else {
            strcat((i8 *)(str),(i8 *)((byte*)"."));
        };
        if (((++k == (i64)16) || (i == length))) {
            if ((k < (i64)16)) {
                av_1 = ((i64)16 - k);
                while (av_1-- > 0) {
L385 :;
                    mlib_gs_str(ax_lib_dest,(byte*)"   ");
                    strcat((i8 *)(str),(i8 *)((byte*)" "));
L386 :;
                }L387 :;
                ;
            };
            mlib_gs_str(ax_lib_dest,(byte*)"\t[");
            mlib_gs_str(ax_lib_dest,str);
            mlib_gs_strln(ax_lib_dest,(byte*)"]");
            k = (i64)0;
            str[((i64)1)-1] = (u64)0u;
            baseaddr += (i64)16;
            msysnewc_m_print_startstr(str2);
            msysnewc_m_print_ptr(baseaddr,(byte*)"z8h");
            msysnewc_m_print_nogap();
            msysnewc_m_print_str((byte*)": ",NULL);
            msysnewc_m_print_end();
            ;
            mlib_gs_str(ax_lib_dest,str2);
        };
L383 :;
    }L384 :;
    ;
    if ((k == (i64)0)) {
        mlib_gs_line(ax_lib_dest);
    };
    mlib_gs_line(ax_lib_dest);
    if (!!(k)) {
        mlib_gs_line(ax_lib_dest);
    };
}

static void ax_writeexe_showsectioncode(struct ax_writeexe_sectionrec * p) {
    byte *  codeptr;
    byte *  codeend;
    byte *  codestart;
    i64 length;
    i64 offset;
    byte *  s;
    byte str[16];
    byte *  baseaddr;
    mlib_gs_strln(ax_lib_dest,(byte*)"proc Section Code");
    length = (*p).virtsize;
    codestart = (codeptr = (byte *)(ax_lib_bufferelemptr((*p).data,(i64)0)));
    codeend = (codeptr + length);
    baseaddr = (byte *)(((i64)4194304 + (*p).virtoffset));
    L388 :;
    while ((codeptr < codeend)) {
        offset = (codeptr - codestart);
        s = ax_disasm_decodeinstr(&codeptr,(baseaddr + offset));
        if ((s == 0)) {
            goto L390 ;
        };
        msysnewc_m_print_startstr(str);
        msysnewc_m_print_i64(offset,(byte*)"4");
        msysnewc_m_print_nogap();
        msysnewc_m_print_str((byte*)" ",NULL);
        msysnewc_m_print_end();
        ;
        mlib_gs_str(ax_lib_dest,str);
        mlib_gs_strln(ax_lib_dest,s);
L389 :;
    }L390 :;
    ;
    mlib_gs_line(ax_lib_dest);
}

static void ax_writeexe_showsectionrelocs2(byte * caption,struct ax_decls_relocrec * relocs,i64 nrelocs) {
    struct ax_decls_relocrec *  r;
    mlib_gs_str(ax_lib_dest,(byte*)"proc Section Relocs: ");
    mlib_gs_str(ax_lib_dest,caption);
    mlib_gs_str(ax_lib_dest,(byte*)" ");
    mlib_gs_strint(ax_lib_dest,nrelocs);
    mlib_gs_line(ax_lib_dest);
    r = relocs;
    L391 :;
    while (!!(r)) {
        mlib_gs_str(ax_lib_dest,(byte*)"Reloc: ");
        mlib_gs_str(ax_lib_dest,ax_objdecls_relocnames[((*r).reloctype)]);
        mlib_gs_str(ax_lib_dest,(byte*)" Offset: ");
        mlib_gs_strint(ax_lib_dest,(*r).offset);
        mlib_gs_str(ax_lib_dest,(byte*)" ST Index: ");
        mlib_gs_strint(ax_lib_dest,(*r).stindex);
        mlib_gs_str(ax_lib_dest,(byte*)" ");
        mlib_gs_str(ax_lib_dest,(*(*ax_decls_ss_symboltable)[((*r).stindex)-1]).name);
        mlib_gs_line(ax_lib_dest);
        r = (*r).nextreloc;
L392 :;
    }L393 :;
    ;
    mlib_gs_line(ax_lib_dest);
}

static void ax_writeexe_gs_value(byte * caption,i64 value) {
    byte str[256];
    strcpy((i8 *)(str),(i8 *)(caption));
    strcat((i8 *)(str),(i8 *)((byte*)":"));
    mlib_ipadstr(str,(i64)20,(byte*)" ");
    mlib_gs_str(ax_lib_dest,str);
    msysnewc_m_print_startstr(str);
    msysnewc_m_print_setfmt((byte*)"0x# #");
    msysnewc_m_print_i64(value,(byte*)"H");
    msysnewc_m_print_i64(value,NULL);
    msysnewc_m_print_end();
    ;
    mlib_gs_strln(ax_lib_dest,str);
}

static void ax_writeexe_showsymboltable2(void) {
    i64 i;
    mlib_gs_strln(ax_lib_dest,(byte*)"Proc Symbol Table");
    L394 :;
    for (i=(i64)1;i<=ax_decls_ss_nsymbols;i+=(i64)1) {
L395 :;
        mlib_gs_strint(ax_lib_dest,i);
        mlib_gs_str(ax_lib_dest,(byte*)": ");
        mlib_gs_strln(ax_lib_dest,(*(*ax_decls_ss_symboltable)[(i)-1]).name);
L396 :;
    }L397 :;
    ;
    mlib_gs_line(ax_lib_dest);
}

static void ax_writeexe_showimporttable(void) {
    byte str[256];
    struct ax_writeexe_importrec p;
    i64 i;
    mlib_gs_strln(ax_lib_dest,(byte*)"Proc Dll List");
    L398 :;
    for (i=(i64)1;i<=ax_writeexe_ndlls;i+=(i64)1) {
L399 :;
        mlib_gs_strint(ax_lib_dest,i);
        mlib_gs_str(ax_lib_dest,(byte*)": ");
        mlib_gs_str(ax_lib_dest,ax_writeexe_dlltable[(i)-1].name);
        mlib_gs_str(ax_lib_dest,(byte*)" ");
        mlib_gs_strint(ax_lib_dest,ax_writeexe_dlltable[(i)-1].nprocs);
        mlib_gs_line(ax_lib_dest);
        ax_writeexe_gs_value((byte*)"\t\tName Table Offset",ax_writeexe_dlltable[(i)-1].nametableoffset);
        ax_writeexe_gs_value((byte*)"\t\tAddr Table Offset",ax_writeexe_dlltable[(i)-1].addrtableoffset);
        ax_writeexe_gs_value((byte*)"\t\tDLL Name Offset  ",ax_writeexe_dlltable[(i)-1].dllnameoffset);
L400 :;
    }L401 :;
    ;
    mlib_gs_line(ax_lib_dest);
    mlib_gs_strln(ax_lib_dest,(byte*)"Proc Import List");
    L402 :;
    for (i=(i64)1;i<=ax_writeexe_nimports;i+=(i64)1) {
L403 :;
        p = ax_writeexe_importtable[(i)-1];
        mlib_gs_strint(ax_lib_dest,i);
        mlib_gs_str(ax_lib_dest,(byte*)": ");
        if (!!(p.libno)) {
            strcpy((i8 *)(str),(i8 *)(p.name));
            mlib_ipadstr(str,(i64)16,(byte*)" ");
            mlib_gs_str(ax_lib_dest,str);
            mlib_gs_str(ax_lib_dest,(byte*)" (");
            mlib_gs_str(ax_lib_dest,ax_writeexe_dlltable[(p.libno)-1].name);
            mlib_gs_strln(ax_lib_dest,(byte*)")");
            ax_writeexe_gs_value((byte*)"\tIAT Offset        ",p.iatoffset);
            ax_writeexe_gs_value((byte*)"\tThunk Offset      ",p.thunkoffset);
            ax_writeexe_gs_value((byte*)"\tHint/Name Offset  ",p.hintnameoffset);
        } else {
            strcpy((i8 *)(str),(i8 *)(p.name));
            mlib_ipadstr(str,(i64)20,(byte*)" ");
            mlib_gs_str(ax_lib_dest,str);
            mlib_gs_strln(ax_lib_dest,(byte*)" (---)");
        };
L404 :;
    }L405 :;
    ;
    mlib_gs_line(ax_lib_dest);
}

static i64 ax_writeexe_roundtoblock(i64 n,i64 align) {
    if (((n & (align - (i64)1)) == (i64)0)) {
        return n;
    };
    return (n + (align - (n & (align - (i64)1))));
}

static void ax_writeexe_showsections(void) {
    struct ax_writeexe_sectionrec s;
    i64 i;
    mlib_gs_strln(ax_lib_dest,(byte*)"proc Section Headersxxx");
    mlib_gs_line(ax_lib_dest);
    L406 :;
    for (i=(i64)1;i<=ax_writeexe_nsections;i+=(i64)1) {
L407 :;
        s = ax_writeexe_sectiontable[(i)-1];
        mlib_gs_str(ax_lib_dest,(byte*)"Section ");
        mlib_gs_strint(ax_lib_dest,i);
        mlib_gs_str(ax_lib_dest,(byte*)": ");
        mlib_gs_str(ax_lib_dest,s.name);
        mlib_gs_str(ax_lib_dest,(byte*)"  (");
        mlib_gs_str(ax_lib_dest,ax_tables_segmentnames[(s.segtype)-1]);
        mlib_gs_strln(ax_lib_dest,(byte*)")");
        ax_writeexe_gs_value((byte*)"    Raw Offset",s.rawoffset);
        ax_writeexe_gs_value((byte*)"    Raw Size",s.rawsize);
        ax_writeexe_gs_value((byte*)"    Virtual Offset",s.virtoffset);
        ax_writeexe_gs_value((byte*)"    Virtual Size",s.virtsize);
        ax_writeexe_gs_value((byte*)"    Nrelocs",s.nrelocs);
        ax_writeexe_gs_value((byte*)"    Data",(i64)(s.data));
        mlib_gs_line(ax_lib_dest);
L408 :;
    }L409 :;
    ;
}

static byte * ax_writeexe_extractlibname(byte * name,i64 * libno,i64 moduleno) {
    byte *  s;
    byte *  name2;
    byte str[256];
    i64 i;
    i64 n;
    name2 = (byte *)(0);
    //reenter:
L410 :;
;
    s = name;
    (*libno) = (i64)0;
    L411 :;
    while (!!((u64)((*s)))) {
        if (((u64)((*s)) == '.')) {
            memcpy((void *)(str),(void *)(name),(u64)((s - name)));
            str[(((s - name) + (i64)1))-1] = (u64)0u;
            strcat((i8 *)(str),(i8 *)((byte*)".dll"));
            L414 :;
            for (i=(i64)1;i<=ax_writeexe_ndlls;i+=(i64)1) {
L415 :;
                if (!!(mlib_eqstring(str,ax_writeexe_dlltable[(i)-1].name))) {
                    (*libno) = i;
                    ++ax_writeexe_dlltable[((*libno))-1].nprocs;
                    return (!!(name2)?name2:(s + (i64)1));
                };
L416 :;
            }L417 :;
            ;
            if ((ax_writeexe_ndlls >= (i64)50)) {
                ax_lib_gerror((byte*)"Too many libs");
            };
            (*libno) = ++ax_writeexe_ndlls;
            ax_writeexe_dlltable[((*libno))-1].name = mlib_pcm_copyheapstring(str);
            ax_writeexe_dlltable[((*libno))-1].nprocs = (i64)1;
            return (!!(name2)?name2:(s + (i64)1));
        };
        ++s;
L412 :;
    }L413 :;
    ;
    L418 :;
    for (i=(i64)1;i<=ax_decls_nsearchlibs;i+=(i64)1) {
L419 :;
        if (!!(oswindows_os_getdllprocaddr(ax_writeexe_libinsttable[(i)-1],name))) {
            n = i;
            goto L421 ;
        };
L420 :;
    }
    {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str(name,NULL);
        msysnewc_m_print_str(ax_decls_moduletable[(moduleno)-1].name,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
        ax_lib_gerror((byte*)"Can't find external function");
    }L421 :;
    ;
    if (!!(((*libno) = ax_writeexe_libnotable[(n)-1]))) {
        ++ax_writeexe_dlltable[((*libno))-1].nprocs;
        return name;
    };
    strcpy((i8 *)(str),(i8 *)(ax_decls_searchlibs[(n)-1]));
    strcat((i8 *)(str),(i8 *)((byte*)".dll"));
    if ((ax_writeexe_ndlls >= (i64)50)) {
        ax_lib_gerror((byte*)"2:Too many libs");
    };
    (*libno) = ++ax_writeexe_ndlls;
    ax_writeexe_dlltable[((*libno))-1].name = mlib_pcm_copyheapstring(str);
    ax_writeexe_dlltable[((*libno))-1].nprocs = (i64)1;
    ax_writeexe_libnotable[(n)-1] = (*libno);
    return name;
}

static void ax_writeexe_scanst(void) {
    i64 i;
    i64 libno;
    struct ax_decls_strec *  d;
    byte *  name;
    L422 :;
    for (i=(i64)1;i<=ax_decls_ss_nsymbols;i+=(i64)1) {
L423 :;
        d = (*ax_decls_ss_symboltable)[(i)-1];
        if (((i64)((*d).symbol)==(i64)21)) {
            if ((ax_writeexe_nimports >= (i64)3000)) {
                ax_lib_gerror((byte*)"genexe: Too many imports");
            };
            ++ax_writeexe_nimports;
            name = ax_writeexe_extractlibname((*d).name,&libno,(i64)((*d).moduleno));
            ax_writeexe_importtable[(ax_writeexe_nimports)-1].libno = libno;
            ax_writeexe_importtable[(ax_writeexe_nimports)-1].name = name;
            ax_writeexe_importtable[(ax_writeexe_nimports)-1].def = d;
            (*d).importindex = ax_writeexe_nimports;
        }else if (((i64)((*d).symbol)==(i64)22)) {
            if (!!(ax_writeexe_userentrypoint)) {
                if (!!(mlib_eqstring((*d).name,ax_writeexe_userentrypoint))) {
                    ax_writeexe_stentrypoint = d;
                };
            } else {
                if (!!(mlib_eqstring((*d).name,(byte*)"main"))) {
                    ax_writeexe_stentrypoint = d;
                } else if (!!(mlib_eqstring((*d).name,(byte*)"start"))) {
                    ax_writeexe_stentrypoint2 = d;
                };
            };
        };
L424 :;
    }L425 :;
    ;
}

static void ax_writeexe_relocdata(struct ax_writeexe_sectionrec * s) {
    struct ax_writeexe_sectionrec *  u;
    struct ax_decls_relocrec *  r;
    byte *  p;
    u32 *  p32;
    struct ax_decls_strec *  d;
    i64 index;
    i64 thunkoffset;
    p = (byte *)(ax_lib_bufferelemptr((*s).data,(i64)0));
    r = (*s).relocs;
    L426 :;
    while (!!(r)) {
        d = (*ax_decls_ss_symboltable)[((*r).stindex)-1];
        index = (i64)((*d).importindex);
        thunkoffset = ax_writeexe_importtable[(index)-1].thunkoffset;
        if (((*r).reloctype==(i64)4)) {
            if (((i64)((u64)((*d).symbol)) != (i64)21)) {
                ax_lib_gerror((byte*)"rel32/not imported");
            };
            (*(u32 *)((p + (*r).offset))) = (u64)(((thunkoffset - (*r).offset) - (i64)4));
        }else if (((*r).reloctype==(i64)2) || ((*r).reloctype==(i64)1)) {
            if (((i64)((u64)((*d).symbol)) == (i64)21)) {
                (*(u32 *)((p + (*r).offset))) = (u64)((((i64)4194304 + thunkoffset) + ax_writeexe_sectiontable[((i64)1)-1].virtoffset));
            } else {
                if (((i64)((*d).segment)==(i64)3)) {
                    u = &ax_writeexe_sectiontable[((i64)3)-1];
                }else if (((i64)((*d).segment)==(i64)2)) {
                    u = &ax_writeexe_sectiontable[((i64)2)-1];
                }else if (((i64)((*d).segment)==(i64)1)) {
                    u = &ax_writeexe_sectiontable[((i64)1)-1];
                };
                p32 = (u32 *)((p + (*r).offset));
                (*p32) = (u64)((((i64)((u64)((*p32))) + (*u).virtoffset) + (i64)4194304));
            };
        } else {
            msysnewc_m_print_startcon();
            msysnewc_m_print_str(ax_objdecls_relocnames[((*r).reloctype)],NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            ax_lib_gerror((byte*)"Can't do this rel type");
        };
        r = (*r).nextreloc;
L427 :;
    }L428 :;
    ;
}

static void ax_writeexe_writerecordx(void * r,i64 length) {
    memcpy((void *)(ax_writeexe_dataptr),r,(u64)(length));
    ax_writeexe_dataptr += length;
}

static void ax_writeexe_writedosstub(void) {
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
    ax_writeexe_writerecordx((void *)(&stubdata),(i64)128);
}

static void ax_writeexe_writepesig(void) {
    (*ax_writeexe_dataptr++) = (u64)80u;
    (*ax_writeexe_dataptr++) = (u64)69u;
    (*ax_writeexe_dataptr++) = (u64)((i64)0);
    (*ax_writeexe_dataptr++) = (u64)((i64)0);
}

static void ax_writeexe_writepadding(i64 offset) {
    ax_writeexe_dataptr = (ax_writeexe_datastart + offset);
}

static void ax_writeexe_writefileheader(void) {
    struct ax_objdecls_imagefileheader header;
    memset((void *)(&header),(i64)0,(u64)((i64)20));
    header.machine = (u64)((i64)34404);
    header.nsections = (u64)(ax_writeexe_nsections);
    header.optheadersize = (u64)((i64)240);
    header.characteristics = (u64)((i64)559);
    ax_writeexe_writerecordx((void *)(&header),(i64)20);
}

static void ax_writeexe_writeoptheader(void) {
    struct ax_objdecls_optionalheader header;
    memset((void *)(&header),(i64)0,(u64)((i64)240));
    header.magic = (u64)((i64)523);
    header.majorlv = (u64)((i64)1);
    header.minorlv = (u64)((i64)0);
    header.codesize = (u64)(ax_writeexe_sectiontable[((i64)1)-1].rawsize);
    header.idatasize = (u64)((ax_writeexe_sectiontable[((i64)2)-1].rawsize + ax_writeexe_sectiontable[((i64)4)-1].rawsize));
    header.zdatasize = (u64)(ax_writeexe_roundtoblock(ax_writeexe_sectiontable[((i64)3)-1].virtsize,(i64)512));
    if ((ax_writeexe_stentrypoint == 0)) {
        ax_writeexe_stentrypoint = ax_writeexe_stentrypoint2;
        if ((ax_writeexe_stentrypoint == 0)) {
            ax_writeexe_stentrypoint = ax_writeexe_stentrypoint3;
            if (!!(ax_writeexe_stentrypoint)) {
                msysnewc_m_print_startcon();
                msysnewc_m_print_str((byte*)"Using tertiary 'WinMain' entry point",NULL);
                msysnewc_m_print_newline();
                msysnewc_m_print_end();
                ;
            };
        };
    };
    if ((ax_writeexe_stentrypoint == 0)) {
        if (!!(ax_writeexe_userentrypoint)) {
            msysnewc_m_print_startcon();
            msysnewc_m_print_str(ax_writeexe_userentrypoint,NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
            ax_lib_gerror((byte*)"User entry point not found");
        } else {
            ax_lib_gerror((byte*)"Entry point not found: main or start");
        };
    };
    header.entrypoint = (u64)((ax_writeexe_sectiontable[((i64)1)-1].virtoffset + (i64)((*ax_writeexe_stentrypoint).offset)));
    header.codebase = (u64)((i64)4096);
    header.imagebase = (u64)((i64)4194304);
    header.sectionalignment = (u64)((i64)4096);
    header.filealignment = (u64)((i64)512);
    header.majorosv = (u64)((i64)4);
    header.minorosv = (u64)((i64)0);
    header.majorssv = (u64)((i64)5);
    header.minorssv = (u64)((i64)2);
    header.imagesize = (u64)(ax_writeexe_imagesize);
    header.headerssize = (u64)(ax_writeexe_sectiontable[((i64)1)-1].rawoffset);
    header.subsystem = (u64)((i64)3);
    header.stackreserve = (u64)((i64)4194304);
    header.stackcommit = (u64)((i64)2097152);
    header.heapreserve = (u64)((i64)1048576);
    header.heapcommit = (u64)((i64)4096);
    header.rvadims = (u64)((i64)16);
    header.importtable.virtualaddr = (u64)(ax_writeexe_sectiontable[((i64)4)-1].virtoffset);
    header.importtable.size = (u64)((i64)128);
    header.iat.virtualaddr = (u64)(ax_writeexe_fileiatoffset);
    header.iat.size = (u64)(ax_writeexe_fileiatsize);
    ax_writeexe_writerecordx((void *)(&header),(i64)240);
}

static void ax_writeexe_writesectionheader(struct ax_writeexe_sectionrec * s) {
    struct ax_objdecls_imagesectionheader sheader;
    memset((void *)(&sheader),(i64)0,(u64)((i64)40));
    strcpy((i8 *)(&sheader.name[((i64)1)-1]),(i8 *)((*s).name));
    sheader.virtual_size = (u64)((*s).virtsize);
    sheader.virtual_address = (u64)((*s).virtoffset);
    sheader.rawdata_offset = (u64)((*s).rawoffset);
    sheader.rawdata_size = (u64)((*s).rawsize);
    if (((*s).segtype==(i64)3)) {
        sheader.characteristics = (u64)((i64)3226468480);
    }else if (((*s).segtype==(i64)2)) {
        sheader.characteristics = (u64)((i64)3226468416);
    }else if (((*s).segtype==(i64)1)) {
        sheader.characteristics = (u64)((i64)1615855648);
    }else if (((*s).segtype==(i64)5)) {
        sheader.characteristics = (u64)((i64)3224371264);
    };
    ax_writeexe_writerecordx((void *)(&sheader),(i64)40);
}

static void ax_writeexe_writesectiondata(struct ax_writeexe_sectionrec * s) {
    if (((*s).segtype==(i64)5)) {
        ax_writeexe_writerecordx((void *)((*s).bytedata),(*s).virtsize);
        if (((*s).rawsize > (*s).virtsize)) {
            ax_writeexe_dataptr += ((*s).rawsize - (*s).virtsize);
        };
    }else if (((*s).segtype==(i64)3)) {
    } else {
        ax_writeexe_writerecordx(ax_lib_bufferelemptr((*s).data,(i64)0),(*s).rawsize);
    };
}

static void ax_writeexe_getoffsets(void) {
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
    struct ax_objdecls_importdirrec *  pdir;
    i64 *  paddr;
    i64 *  pname;
    i64 iatoffset;
    byte *  phint;
    u32 *  pextra;
    i64 xxx;
    i64 av_1;
    byte *  thunkptr;
    byte *  codebase;
    i64 thunkaddr;
    fileoffset = (i64)392;
    fileoffset += ((i64)40 * ax_writeexe_nsections);
    fileoffset = ax_writeexe_roundtoblock(fileoffset,(i64)512);
    imageoffset = (i64)4096;
    codesize = ax_writeexe_sectiontable[((i64)1)-1].virtsize;
    pcode = (byte *)(ax_lib_bufferelemptr(ax_decls_ss_code,codesize));
    L429 :;
    while (!!((codesize & (i64)7))) {
        (*pcode++) = (u64)((i64)144);
        ++codesize;
L430 :;
    }L431 :;
    ;
    thunkoffset = codesize;
    codesize += (ax_writeexe_nimports * (i64)8);
    ax_writeexe_sectiontable[((i64)1)-1].virtsize = codesize;
    ax_writeexe_sectiontable[((i64)1)-1].rawsize = ax_writeexe_roundtoblock(codesize,(i64)512);
    ax_lib_buffercheck(ax_decls_ss_code,((codesize - thunkoffset) + (i64)16));
    L432 :;
    for (i=(i64)1;i<=ax_writeexe_nsections;i+=(i64)1) {
L433 :;
        if ((ax_writeexe_sectiontable[(i)-1].segtype != (i64)3)) {
            ax_writeexe_sectiontable[(i)-1].rawoffset = fileoffset;
        };
        if ((ax_writeexe_sectiontable[(i)-1].segtype != (i64)3)) {
            fileoffset = ax_writeexe_roundtoblock((fileoffset + ax_writeexe_sectiontable[(i)-1].virtsize),(i64)512);
        };
        ax_writeexe_sectiontable[(i)-1].virtoffset = imageoffset;
        if ((ax_writeexe_sectiontable[(i)-1].segtype == (i64)5)) {
            diroffset = imageoffset;
            impdirno = i;
        };
        imageoffset = ax_writeexe_roundtoblock((imageoffset + ax_writeexe_sectiontable[(i)-1].virtsize),(i64)4096);
L434 :;
    }L435 :;
    ;
    diroffset += ((ax_writeexe_ndlls + (i64)1) * (i64)20);
    L436 :;
    for (i=(i64)1;i<=ax_writeexe_ndlls;i+=(i64)1) {
L437 :;
        ax_writeexe_dlltable[(i)-1].nametableoffset = diroffset;
        diroffset += ((ax_writeexe_dlltable[(i)-1].nprocs + (i64)1) * (i64)8);
L438 :;
    }L439 :;
    ;
    ax_writeexe_fileiatoffset = diroffset;
    L440 :;
    for (i=(i64)1;i<=ax_writeexe_ndlls;i+=(i64)1) {
L441 :;
        ax_writeexe_dlltable[(i)-1].addrtableoffset = diroffset;
        diroffset += ((ax_writeexe_dlltable[(i)-1].nprocs + (i64)1) * (i64)8);
L442 :;
    }L443 :;
    ;
    ax_writeexe_fileiatsize = (diroffset - ax_writeexe_fileiatoffset);
    hinttableoffset = diroffset;
    L444 :;
    for (i=(i64)1;i<=ax_writeexe_nimports;i+=(i64)1) {
L445 :;
        length = ((i64)(strlen((i8 *)(ax_writeexe_importtable[(i)-1].name))) + (i64)3);
        if (!!((length & (i64)1))) {
            ++length;
        };
        ax_writeexe_importtable[(i)-1].hintnameoffset = diroffset;
        diroffset += length;
L446 :;
    }L447 :;
    ;
    diroffset = ax_writeexe_roundtoblock(diroffset,(i64)4);
    L448 :;
    for (i=(i64)1;i<=ax_writeexe_ndlls;i+=(i64)1) {
L449 :;
        length = ((i64)(strlen((i8 *)(ax_writeexe_dlltable[(i)-1].name))) + (i64)1);
        if (!!((length & (i64)1))) {
            ++length;
        };
        ax_writeexe_dlltable[(i)-1].dllextraoffset = diroffset;
        diroffset += (ax_writeexe_dlltable[(i)-1].nprocs * (i64)4);
        ax_writeexe_dlltable[(i)-1].dllnameoffset = diroffset;
        diroffset += length;
L450 :;
    }L451 :;
    ;
    dirstartoffset = ax_writeexe_sectiontable[(impdirno)-1].virtoffset;
    offset = (diroffset - dirstartoffset);
    ax_writeexe_sectiontable[(impdirno)-1].virtsize = offset;
    ax_writeexe_sectiontable[(impdirno)-1].rawsize = ax_writeexe_roundtoblock(offset,(i64)512);
    ax_writeexe_filesize = ax_writeexe_roundtoblock((fileoffset + offset),(i64)512);
    ax_writeexe_imagesize = ax_writeexe_roundtoblock((imageoffset + (diroffset - dirstartoffset)),(i64)4096);
    pimpdir = (ax_writeexe_sectiontable[(impdirno)-1].bytedata = (byte *)(mlib_pcm_allocz(offset)));
    pdir = (struct ax_objdecls_importdirrec *)(pimpdir);
    L452 :;
    for (i=(i64)1;i<=ax_writeexe_ndlls;i+=(i64)1) {
L453 :;
        (*pdir).implookuprva = (u64)(ax_writeexe_dlltable[(i)-1].nametableoffset);
        (*pdir).impaddressrva = (u64)(ax_writeexe_dlltable[(i)-1].addrtableoffset);
        (*pdir).namerva = (u64)(ax_writeexe_dlltable[(i)-1].dllnameoffset);
        ++pdir;
        iatoffset = ax_writeexe_dlltable[(i)-1].addrtableoffset;
        paddr = (i64 *)(((pimpdir + iatoffset) - dirstartoffset));
        pname = (i64 *)(((pimpdir + ax_writeexe_dlltable[(i)-1].nametableoffset) - dirstartoffset));
        L456 :;
        for (j=(i64)1;j<=ax_writeexe_nimports;j+=(i64)1) {
L457 :;
            if ((ax_writeexe_importtable[(j)-1].libno == i)) {
                (*pname) = ((*paddr) = ax_writeexe_importtable[(j)-1].hintnameoffset);
                ax_writeexe_importtable[(j)-1].iatoffset = iatoffset;
                iatoffset += (i64)8;
                ++pname;
                ++paddr;
            };
L458 :;
        }L459 :;
        ;
L454 :;
    }L455 :;
    ;
    L460 :;
    for (i=(i64)1;i<=ax_writeexe_nimports;i+=(i64)1) {
L461 :;
        phint = ((pimpdir + ax_writeexe_importtable[(i)-1].hintnameoffset) - dirstartoffset);
        phint += (i64)2;
        strcpy((i8 *)(phint),(i8 *)(ax_writeexe_importtable[(i)-1].name));
L462 :;
    }L463 :;
    ;
    xxx = dirstartoffset;
    L464 :;
    for (i=(i64)1;i<=ax_writeexe_ndlls;i+=(i64)1) {
L465 :;
        pextra = (u32 *)(((pimpdir + ax_writeexe_dlltable[(i)-1].dllextraoffset) - dirstartoffset));
        L468 :;
        for (j=(i64)1;j<=ax_writeexe_dlltable[(i)-1].nprocs;j+=(i64)1) {
L469 :;
            (*pextra) = (u64)(xxx);
            ++pextra;
L470 :;
        }L471 :;
        ;
        xxx += (i64)20;
        phint = ((pimpdir + ax_writeexe_dlltable[(i)-1].dllnameoffset) - dirstartoffset);
        strcpy((i8 *)(phint),(i8 *)(ax_writeexe_dlltable[(i)-1].name));
L466 :;
    }L467 :;
    ;
    thunkptr = (byte *)(ax_lib_bufferelemptr(ax_decls_ss_code,thunkoffset));
    codebase = (byte *)(ax_lib_bufferelemptr(ax_decls_ss_code,(i64)0));
    L472 :;
    for (i=(i64)1;i<=ax_writeexe_nimports;i+=(i64)1) {
L473 :;
        ax_writeexe_importtable[(i)-1].thunkoffset = (thunkptr - codebase);
        (*thunkptr++) = (u64)((i64)72);
        (*thunkptr++) = (u64)((i64)255);
        (*thunkptr++) = (u64)((i64)36);
        (*thunkptr++) = (u64)((i64)37);
        thunkaddr = ((i64)4194304 + ax_writeexe_importtable[(i)-1].iatoffset);
        (*(i32 *)(thunkptr)) = thunkaddr;
        thunkptr += (i64)4;
L474 :;
    }L475 :;
    ;
}

byte * ax_disasm_decodeinstr(byte * * cptr,byte * baseaddr) {
    i64 n;
    i64 w;
    i64 opc;
    i64 reg;
    i64 op;
    byte *  pstart;
    static byte str[256];
    byte str2[128];
    i64 av_1;
    i64 av_2;
    ax_disasm_deststr[((i64)1)-1] = (u64)0u;
    pstart = (ax_disasm_codeptr = (*cptr));
    ax_disasm_rex = (i64)0;
    ax_disasm_opsize = (i64)1;
    ax_disasm_f2override = (ax_disasm_f3override = (ax_disasm_sizeoverride = (ax_disasm_addroverride = (i64)0)));
    ax_disasm_basereg = (ax_disasm_indexreg = (ax_disasm_offset = (i64)0));
    //retry:
L476 :;
;
    switch ((opc = (i64)((*ax_disasm_codeptr++)))) {
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
        ax_disasm_decodeaddr((opc & (i64)1));
        ax_disasm_getsilx(&ax_disasm_basereg);
        ax_disasm_getsil(&ax_disasm_rmreg);
        ax_disasm_genstr(ax_disasm_opnames[(op)]);
        ax_disasm_printaddrmode((i64)0);
        ax_disasm_genstr((byte*)", ");
        ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,ax_disasm_opsize));
    }break;
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
        ax_disasm_decodeaddr((opc & (i64)1));
        ax_disasm_genstr(ax_disasm_opnames[(op)]);
        ax_disasm_genstr((byte*)" ");
        ax_disasm_getsil(&ax_disasm_rmreg);
        ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,ax_disasm_opsize));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_printaddrmode((i64)0);
    }break;
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
        ax_disasm_genstr(ax_disasm_opnames[((opc >> (i64)3))]);
        ax_disasm_genstr((byte*)" ");
        if (!!((opc & (i64)1))) {
            ax_disasm_opsize = (i64)4;
            if (!!(ax_disasm_sizeoverride)) {
                ax_disasm_opsize = (i64)2;
            };
            if (!!((ax_disasm_rex & (i64)8))) {
                ax_disasm_opsize = (i64)8;
            };
        };
        ax_disasm_genstr(ax_disasm_strreg((i64)1,ax_disasm_opsize));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_genintd(ax_disasm_readimm());
    }break;
    case 15:;
    {
        ax_disasm_decodetwobyteinstr();
    }break;
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
        ax_disasm_rex = opc;
        goto L476 ;
;
    }break;
    case 80:;
    case 81:;
    case 82:;
    case 83:;
    case 84:;
    case 85:;
    case 86:;
    case 87:;
    {
        reg = ax_disasm_getreg((opc & (i64)7),(ax_disasm_rex & (i64)1));
        ax_disasm_genstr((byte*)"push ");
        ax_disasm_genstr(ax_disasm_strreg(reg,(i64)8));
    }break;
    case 88:;
    case 89:;
    case 90:;
    case 91:;
    case 92:;
    case 93:;
    case 94:;
    case 95:;
    {
        reg = ax_disasm_getreg((opc & (i64)7),(ax_disasm_rex & (i64)1));
        ax_disasm_genstr((byte*)"pop ");
        ax_disasm_genstr(ax_disasm_strreg(reg,(i64)8));
    }break;
    case 99:;
    {
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_genstr((byte*)"movsxd ");
        ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,ax_disasm_opsize));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_opsize = (i64)4;
        ax_disasm_printaddrmode((i64)0);
    }break;
    case 102:;
    {
        ax_disasm_sizeoverride = (i64)1;
        goto L476 ;
;
    }break;
    case 103:;
    {
        ax_disasm_addroverride = (i64)1;
        goto L476 ;
;
    }break;
    case 104:;
    {
        ax_disasm_genstr((byte*)"push ");
        ax_disasm_genintd(ax_disasm_readint32());
    }break;
    case 106:;
    {
        ax_disasm_genstr((byte*)"push ");
        ax_disasm_genintd(ax_disasm_readsbyte());
    }break;
    case 105:;
    case 107:;
    {
        ax_disasm_decodeaddr((i64)1);
        if ((ax_disasm_basereg != ax_disasm_rmreg)) {
            ax_disasm_genstr((byte*)"imul3");
            ax_disasm_genstr((byte*)" ");
            ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,ax_disasm_opsize));
            ax_disasm_genstr((byte*)", ");
        } else {
            ax_disasm_genstr((byte*)"imul2");
        };
        ax_disasm_printaddrmode((i64)0);
        ax_disasm_genstr((byte*)", ");
        ax_disasm_opsize = (!!((opc & (i64)2))?(i64)1:ax_disasm_opsize);
        ax_disasm_genintd(ax_disasm_readimm());
    }break;
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
        ax_disasm_genstr((byte*)"j");
        ax_disasm_genstr(ax_disasm_condnames[((opc & (i64)15))]);
        ax_disasm_genstr((byte*)" ");
        ax_disasm_genintd(ax_disasm_readsbyte());
    }break;
    case 128:;
    case 129:;
    case 130:;
    case 131:;
    {
        ax_disasm_decodeaddr((opc & (i64)1));
        ax_disasm_genstr(ax_disasm_opnames[(ax_disasm_rmopc)]);
        ax_disasm_getsilx(&ax_disasm_basereg);
        ax_disasm_printaddrmode((i64)0);
        ax_disasm_genstr((byte*)", ");
        if ((opc != (i64)131)) {
            ax_disasm_genintd(ax_disasm_readimm());
        } else {
            ax_disasm_genintd(ax_disasm_readsbyte());
        };
    }break;
    case 132:;
    case 133:;
    {
        ax_disasm_decodeaddr((opc & (i64)1));
        ax_disasm_getsilx(&ax_disasm_basereg);
        ax_disasm_getsil(&ax_disasm_rmreg);
        ax_disasm_genstr((byte*)"test ");
        ax_disasm_printaddrmode((i64)0);
        ax_disasm_genstr((byte*)", ");
        ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,ax_disasm_opsize));
    }break;
    case 134:;
    case 135:;
    {
        ax_disasm_decodeaddr((opc & (i64)1));
        ax_disasm_genstr((byte*)"exch2 ");
        ax_disasm_getsilx(&ax_disasm_basereg);
        ax_disasm_getsil(&ax_disasm_rmreg);
        ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,ax_disasm_opsize));
        ax_disasm_genstr((byte*)",");
        ax_disasm_printaddrmode((i64)0);
    }break;
    case 136:;
    case 137:;
    {
        ax_disasm_decodeaddr((opc & (i64)1));
        ax_disasm_genstr((byte*)"mov");
        ax_disasm_getsilx(&ax_disasm_basereg);
        ax_disasm_getsil(&ax_disasm_rmreg);
        ax_disasm_printaddrmode((i64)0);
        ax_disasm_genstr((byte*)", ");
        ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,ax_disasm_opsize));
    }break;
    case 138:;
    case 139:;
    {
        ax_disasm_decodeaddr((opc & (i64)1));
        ax_disasm_genstr((byte*)"mov ");
        ax_disasm_getsilx(&ax_disasm_basereg);
        ax_disasm_getsil(&ax_disasm_rmreg);
        ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,ax_disasm_opsize));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_printaddrmode((i64)0);
    }break;
    case 141:;
    {
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_genstr((byte*)"lea ");
        ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,ax_disasm_opsize));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_printaddrmode((i64)0);
    }break;
    case 143:;
    {
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_opsize = (i64)1;
        ax_disasm_genstr((byte*)"pop");
        ax_disasm_printaddrmode((i64)0);
    }break;
    case 144:;
    {
        if (!!(ax_disasm_rex)) {
            goto L477 ;
;
        };
        ax_disasm_genstr((byte*)"nop");
    }break;
    case 145:;
    case 146:;
    case 147:;
    case 148:;
    case 149:;
    case 150:;
    case 151:;
    {
        //doexch:
L477 :;
;
        reg = ((opc & (i64)7) + (i64)1);
        if (!!((ax_disasm_rex & (i64)1))) {
            reg += (i64)8;
        };
        ax_disasm_opsize = (!!(ax_disasm_sizeoverride)?(i64)2:(i64)4);
        if (!!((ax_disasm_rex & (i64)8))) {
            ax_disasm_opsize = (i64)8;
        };
        ax_disasm_genstr((byte*)"xchg ");
        ax_disasm_genstr(ax_disasm_strreg((i64)1,ax_disasm_opsize));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_genstr(ax_disasm_strreg(reg,ax_disasm_opsize));
    }break;
    case 152:;
    {
        if (!!(ax_disasm_sizeoverride)) {
            ax_disasm_genstr((byte*)"cbw");
        } else {
            ax_disasm_genstr((byte*)"cbw???");
        };
    }break;
    case 153:;
    {
        if (!!(ax_disasm_sizeoverride)) {
            ax_disasm_genstr((byte*)"cwd");
        } else if (!!((ax_disasm_rex & (i64)8))) {
            ax_disasm_genstr((byte*)"cqo");
        } else {
            ax_disasm_genstr((byte*)"cdq");
        };
    }break;
    case 155:;
    {
        ax_disasm_genstr((byte*)"wait");
    }break;
    case 156:;
    {
        ax_disasm_genstr((byte*)"pushf");
    }break;
    case 157:;
    {
        ax_disasm_genstr((byte*)"popf");
    }break;
    case 158:;
    {
        ax_disasm_genstr((byte*)"sahf");
    }break;
    case 159:;
    {
        ax_disasm_genstr((byte*)"lahf");
    }break;
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
        ax_disasm_genstr((((opc >> (i64)1) & (i64)7)==1?(byte*)"?":(((opc >> (i64)1) & (i64)7)==2?(byte*)"movs":(((opc >> (i64)1) & (i64)7)==3?(byte*)"cmps":(((opc >> (i64)1) & (i64)7)==4?(byte*)"?":(((opc >> (i64)1) & (i64)7)==5?(byte*)"stos":(((opc >> (i64)1) & (i64)7)==6?(byte*)"lods":(((opc >> (i64)1) & (i64)7)==7?(byte*)"scas":(byte*)"?"))))))));
        if (((opc & (i64)1) == (i64)0)) {
            ax_disasm_genstr((byte*)"b");
        } else {
            if (!!((ax_disasm_rex & (i64)8))) {
                ax_disasm_genstr((byte*)"q");
            } else if (!!(ax_disasm_sizeoverride)) {
                ax_disasm_genstr((byte*)"w");
            } else {
                ax_disasm_genstr((byte*)"d");
            };
        };
    }break;
    case 168:;
    case 169:;
    {
        ax_disasm_genstr((byte*)"test ");
        if (!!((opc & (i64)1))) {
            ax_disasm_opsize = (!!(ax_disasm_sizeoverride)?(i64)2:(i64)4);
            if (!!((ax_disasm_rex & (i64)8))) {
                ax_disasm_opsize = (i64)8;
            };
        };
        ax_disasm_genstr(ax_disasm_strreg((i64)1,ax_disasm_opsize));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_genintd(ax_disasm_readimm());
    }break;
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
        if (!!((ax_disasm_rex & (i64)1))) {
            reg += (i64)8;
        };
        if (!!((opc & (i64)8))) {
            ax_disasm_opsize = (!!(ax_disasm_sizeoverride)?(i64)2:(i64)4);
            if (!!((ax_disasm_rex & (i64)8))) {
                ax_disasm_opsize = (i64)8;
            };
        };
        ax_disasm_genstr((byte*)"mov ");
        ax_disasm_getsil(&reg);
        ax_disasm_genstr(ax_disasm_strreg(reg,ax_disasm_opsize));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_genintd(ax_disasm_readimm8());
    }break;
    case 192:;
    case 193:;
    case 208:;
    case 209:;
    case 210:;
    case 211:;
    {
        ax_disasm_decodeaddr((opc & (i64)1));
        ax_disasm_getsilx(&ax_disasm_basereg);
        ax_disasm_genstr(((ax_disasm_rmopc + (i64)1)==1?(byte*)"rol":((ax_disasm_rmopc + (i64)1)==2?(byte*)"ror":((ax_disasm_rmopc + (i64)1)==3?(byte*)"rcl":((ax_disasm_rmopc + (i64)1)==4?(byte*)"rcr":((ax_disasm_rmopc + (i64)1)==5?(byte*)"shl":((ax_disasm_rmopc + (i64)1)==6?(byte*)"shr":((ax_disasm_rmopc + (i64)1)==7?(byte*)"?":((ax_disasm_rmopc + (i64)1)==8?(byte*)"sar":(byte*)"?")))))))));
        ax_disasm_printaddrmode((i64)0);
        if ((opc <= (i64)193)) {
            ax_disasm_genstr((byte*)", ");
            ax_disasm_genintd(ax_disasm_readbyte());
        } else {
            ax_disasm_genstr((!!((opc & (i64)2))?(byte*)", cl":(byte*)", 1"));
        };
    }break;
    case 194:;
    {
        ax_disasm_genstr((byte*)"retn ");
        ax_disasm_genintd((i64)(ax_disasm_readword16()));
    }break;
    case 195:;
    {
        ax_disasm_genstr((byte*)"ret");
    }break;
    case 198:;
    case 199:;
    {
        ax_disasm_decodeaddr((opc & (i64)1));
        ax_disasm_genstr((byte*)"mov");
        ax_disasm_printaddrmode((i64)0);
        ax_disasm_genstr((byte*)", ");
        ax_disasm_genintd(ax_disasm_readimm());
    }break;
    case 215:;
    {
        ax_disasm_genstr((byte*)"xlat");
    }break;
    case 216:;
    case 217:;
    case 218:;
    case 219:;
    case 220:;
    case 221:;
    case 222:;
    case 223:;
    {
        ax_disasm_decode8087((opc & (i64)7));
    }break;
    case 224:;
    {
        ax_disasm_genstr((byte*)"loopnz ");
        ax_disasm_genintd(ax_disasm_readsbyte());
    }break;
    case 225:;
    {
        ax_disasm_genstr((byte*)"loopz ");
        ax_disasm_genintd(ax_disasm_readsbyte());
    }break;
    case 226:;
    {
        ax_disasm_genstr((byte*)"loop ");
        ax_disasm_genintd(ax_disasm_readsbyte());
    }break;
    case 227:;
    {
        if (!!(ax_disasm_addroverride)) {
            ax_disasm_genstr((byte*)"jecxz ");
        } else {
            ax_disasm_genstr((byte*)"jrcxz ");
        };
        ax_disasm_genintd(ax_disasm_readsbyte());
    }break;
    case 232:;
    {
        ax_disasm_genstr((byte*)"call ");
        ax_disasm_genintd(ax_disasm_readint32());
    }break;
    case 233:;
    {
        ax_disasm_genstr((byte*)"[4] jmp ");
        ax_disasm_genintd(ax_disasm_readint32());
    }break;
    case 235:;
    {
        ax_disasm_genstr((byte*)"jmp ");
        ax_disasm_genintd(ax_disasm_readsbyte());
    }break;
    case 242:;
    {
        if ((((i64)((u64)((*ax_disasm_codeptr))) != (i64)15) && (((i64)((u64)((*ax_disasm_codeptr))) < (i64)64) && ((i64)((u64)((*ax_disasm_codeptr))) > (i64)79)))) {
            ax_disasm_genstr((byte*)"repne");
        } else {
            ax_disasm_f2override = (i64)1;
            goto L476 ;
;
        };
    }break;
    case 243:;
    {
        if ((((i64)((u64)((*ax_disasm_codeptr))) != (i64)15) && (((i64)((u64)((*ax_disasm_codeptr))) < (i64)64) && ((i64)((u64)((*ax_disasm_codeptr))) > (i64)79)))) {
            ax_disasm_genstr((byte*)"repe");
        } else {
            ax_disasm_f3override = (i64)1;
            goto L476 ;
;
        };
    }break;
    case 244:;
    {
        return (byte *)(0);
    }break;
    case 246:;
    case 247:;
    {
        ax_disasm_decodeaddr((opc & (i64)1));
        ax_disasm_getsilx(&ax_disasm_basereg);
        ax_disasm_genstr(((ax_disasm_rmopc + (i64)1)==1?(byte*)"test":((ax_disasm_rmopc + (i64)1)==2?(byte*)"?":((ax_disasm_rmopc + (i64)1)==3?(byte*)"not":((ax_disasm_rmopc + (i64)1)==4?(byte*)"neg":((ax_disasm_rmopc + (i64)1)==5?(byte*)"mul":((ax_disasm_rmopc + (i64)1)==6?(byte*)"imul":((ax_disasm_rmopc + (i64)1)==7?(byte*)"div":((ax_disasm_rmopc + (i64)1)==8?(byte*)"idiv":(byte*)"?")))))))));
        ax_disasm_printaddrmode((i64)0);
        if ((ax_disasm_rmopc == (i64)0)) {
            if ((ax_disasm_opsize == (i64)8)) {
                ax_disasm_opsize = (i64)4;
            };
            ax_disasm_genstr((byte*)", ");
            ax_disasm_genintd(ax_disasm_readimm());
        };
    }break;
    case 254:;
    {
        w = (i64)0;
        goto L478 ;
;
    }break;
    case 255:;
    {
        w = (i64)1;
        //doff:
L478 :;
;
        ax_disasm_decodeaddr(w);
        if ((ax_disasm_rmopc==(i64)0)) {
            ax_disasm_getsilx(&ax_disasm_basereg);
            ax_disasm_genstr((byte*)"inc");
        }else if ((ax_disasm_rmopc==(i64)1)) {
            ax_disasm_getsilx(&ax_disasm_basereg);
            ax_disasm_genstr((byte*)"dec");
        }else if ((ax_disasm_rmopc==(i64)2)) {
            ax_disasm_opsize = (i64)8;
            ax_disasm_genstr((byte*)"icall");
        }else if ((ax_disasm_rmopc==(i64)4)) {
            ax_disasm_opsize = (i64)8;
            ax_disasm_genstr((byte*)"jmp");
        }else if ((ax_disasm_rmopc==(i64)6)) {
            ax_disasm_opsize = (i64)8;
            ax_disasm_genstr((byte*)"push");
        } else {
            msysnewc_m_print_startcon();
            msysnewc_m_print_str((byte*)"FFxx?",NULL);
            msysnewc_m_print_newline();
            msysnewc_m_print_end();
            ;
        };
        ax_disasm_printaddrmode((i64)0);
    }break;
    default: {
        ax_disasm_genstr((byte*)"Unknown opcode: ");
        ax_disasm_genhex(opc);
    }
    } //SW
;
    if (!!(baseaddr)) {
        msysnewc_m_print_startstr(str);
        msysnewc_m_print_ptr(baseaddr,(byte*)"z6h");
        msysnewc_m_print_nogap();
        msysnewc_m_print_str((byte*)": ",NULL);
        msysnewc_m_print_end();
        ;
    } else {
        msysnewc_m_print_startstr(str);
        msysnewc_m_print_ptr(pstart,(byte*)"z6h");
        msysnewc_m_print_nogap();
        msysnewc_m_print_str((byte*)": ",NULL);
        msysnewc_m_print_end();
        ;
    };
    n = (ax_disasm_codeptr - pstart);
    av_1 = n;
    while (av_1-- > 0) {
L479 :;
        msysnewc_m_print_startstr(str2);
        msysnewc_m_print_i64((i64)((*pstart++)),(byte*)"z2H");
        msysnewc_m_print_nogap();
        msysnewc_m_print_str((byte*)" ",NULL);
        msysnewc_m_print_end();
        ;
        strcat((i8 *)(str),(i8 *)(str2));
L480 :;
    }L481 :;
    ;
    av_2 = ((i64)14 - n);
    while (av_2-- > 0) {
L482 :;
        strcat((i8 *)(str),(i8 *)((byte*)"-- "));
L483 :;
    }L484 :;
    ;
    strcat((i8 *)(str),(i8 *)(ax_disasm_deststr));
    (*cptr) = ax_disasm_codeptr;
    return str;
}

static void ax_disasm_decodetwobyteinstr(void) {
    i64 opc;
    i64 rhssize;
    i64 third;
    i64 imm;
    byte *  opcstr;
    switch ((opc = (i64)((*ax_disasm_codeptr++)))) {
    case 42:;
    {
        ax_disasm_decodeaddr((i64)1);
        if (!!(ax_disasm_f3override)) {
            ax_disasm_genstr((byte*)"cvtsi2ss ");
        } else {
            ax_disasm_genstr((byte*)"cvtsi2sd ");
        };
        ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_printaddrmode((i64)0);
    }break;
    case 44:;
    {
        ax_disasm_decodeaddr((i64)1);
        if (!!(ax_disasm_f3override)) {
            ax_disasm_genstr((byte*)"cvttss2si ");
            rhssize = (i64)4;
        } else {
            ax_disasm_genstr((byte*)"cvttsd2si ");
            rhssize = (i64)8;
        };
        if (!!((ax_disasm_rex & (i64)8))) {
            ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,(i64)8));
        } else {
            ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,(i64)4));
        };
        ax_disasm_genstr((byte*)", ");
        ax_disasm_opsize = rhssize;
        ax_disasm_printaddrmode((i64)1);
    }break;
    case 45:;
    {
        ax_disasm_decodeaddr((i64)1);
        if (!!(ax_disasm_f3override)) {
            ax_disasm_genstr((byte*)"cvtss2si ");
            rhssize = (i64)4;
        } else {
            ax_disasm_genstr((byte*)"cvtsd2si ");
            rhssize = (i64)8;
        };
        if (!!((ax_disasm_rex & (i64)8))) {
            ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,(i64)8));
        } else {
            ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,(i64)4));
        };
        ax_disasm_genstr((byte*)", ");
        ax_disasm_opsize = rhssize;
        ax_disasm_printaddrmode((i64)1);
    }break;
    case 47:;
    {
        ax_disasm_decodeaddr((i64)1);
        if (!!(ax_disasm_sizeoverride)) {
            ax_disasm_opsize = (i64)8;
            ax_disasm_genstr((byte*)"comisd ");
        } else {
            ax_disasm_opsize = (i64)4;
            ax_disasm_genstr((byte*)"comiss ");
        };
        ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_printaddrmode((i64)1);
    }break;
    case 58:;
    {
        third = (i64)((*ax_disasm_codeptr++));
        if ((third==(i64)99)) {
            ax_disasm_genstr((byte*)"pcmpistri ");
        }else if ((third==(i64)98)) {
            ax_disasm_genstr((byte*)"pcmpistrm ");
        } else {
            ax_disasm_genstr((byte*)"Unknown opcode 2-byte opcode: 0F ");
            ax_disasm_genhex(opc);
            return;
        };
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_printaddrmode((i64)1);
        ax_disasm_genstr((byte*)", ");
        imm = (i64)((*ax_disasm_codeptr++));
        ax_disasm_genintd(imm);
    }break;
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
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_genstr((byte*)"cmov");
        ax_disasm_genstr(ax_disasm_condnames[((opc & (i64)15))]);
        ax_disasm_genstr((byte*)" ");
        ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,ax_disasm_opsize));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_printaddrmode((i64)0);
    }break;
    case 81:;
    {
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_opsize = (!!(ax_disasm_f3override)?(i64)4:(i64)8);
        ax_disasm_genstr(((ax_disasm_opsize == (i64)4)?(byte*)"sqrtss ":(byte*)"sqrtsd "));
        ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_printaddrmode((i64)1);
    }break;
    case 84:;
    {
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_genstr((!!(ax_disasm_sizeoverride)?(byte*)"andpd ":(byte*)"andps "));
        ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_opsize = (!!(ax_disasm_sizeoverride)?(i64)8:(i64)4);
        ax_disasm_printaddrmode((i64)1);
    }break;
    case 87:;
    {
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_genstr((!!(ax_disasm_sizeoverride)?(byte*)"xorpd ":(byte*)"xorps "));
        ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_opsize = (!!(ax_disasm_sizeoverride)?(i64)8:(i64)4);
        ax_disasm_printaddrmode((i64)1);
    }break;
    case 88:;
    {
        opcstr = (byte*)"adds";
        //doarith:
L485 :;
;
        ax_disasm_genstr(opcstr);
        ax_disasm_decodeaddr((i64)1);
        if (!!(ax_disasm_f2override)) {
            ax_disasm_opsize = (i64)8;
            ax_disasm_genstr((byte*)"d ");
        } else {
            ax_disasm_opsize = (i64)4;
            ax_disasm_genstr((byte*)"s ");
        };
        ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_printaddrmode((i64)1);
    }break;
    case 89:;
    {
        opcstr = (byte*)"muls";
        goto L485 ;
;
    }break;
    case 90:;
    {
        ax_disasm_decodeaddr((i64)1);
        if (!!(ax_disasm_f3override)) {
            ax_disasm_genstr((byte*)"cvtss2sd ");
            rhssize = (i64)4;
        } else {
            ax_disasm_genstr((byte*)"cvtsd2ss ");
            rhssize = (i64)8;
        };
        ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_opsize = rhssize;
        ax_disasm_printaddrmode((i64)1);
    }break;
    case 92:;
    {
        opcstr = (byte*)"subs";
        goto L485 ;
;
    }break;
    case 93:;
    {
        opcstr = (byte*)"mins";
        goto L485 ;
;
    }break;
    case 94:;
    {
        opcstr = (byte*)"divs";
        goto L485 ;
;
    }break;
    case 95:;
    {
        opcstr = (byte*)"maxs";
        goto L485 ;
;
    }break;
    case 110:;
    {
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_opsize = (!!((ax_disasm_rex & (i64)8))?(i64)8:(i64)4);
        ax_disasm_genstr(((ax_disasm_opsize == (i64)4)?(byte*)"movd ":(byte*)"movq "));
        if (!!(ax_disasm_sizeoverride)) {
            ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
        } else {
            ax_disasm_genstr(ax_disasm_strmmx(ax_disasm_rmreg));
        };
        ax_disasm_genstr((byte*)", ");
        ax_disasm_printaddrmode((i64)0);
    }break;
    case 111:;
    {
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_opsize = (i64)16;
        if (!!(ax_disasm_sizeoverride)) {
            ax_disasm_genstr((byte*)"movdqa ");
        } else if (!!(ax_disasm_f3override)) {
            ax_disasm_genstr((byte*)"movdqu ");
        } else {
            ax_disasm_genstr((byte*)"No 66/F3 ");
        };
        ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_printaddrmode((i64)1);
    }break;
    case 126:;
    {
        ax_disasm_decodeaddr((i64)1);
        if (!!(ax_disasm_f3override)) {
            ax_disasm_opsize = (i64)8;
            ax_disasm_genstr((byte*)"movq ");
            ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
            ax_disasm_genstr((byte*)", ");
            ax_disasm_printaddrmode((i64)1);
        } else if (!!((ax_disasm_rex & (i64)8))) {
            ax_disasm_opsize = (i64)8;
            ax_disasm_genstr((byte*)"movq ");
            ax_disasm_printaddrmode((i64)0);
            ax_disasm_genstr((byte*)", ");
            ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
        } else {
            ax_disasm_opsize = (i64)4;
            ax_disasm_genstr((byte*)"movd ");
            ax_disasm_printaddrmode((i64)0);
            ax_disasm_genstr((byte*)", ");
            if (!!(ax_disasm_sizeoverride)) {
                ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
            } else {
                ax_disasm_genstr(ax_disasm_strmmx(ax_disasm_rmreg));
            };
        };
    }break;
    case 127:;
    {
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_opsize = (i64)16;
        if (!!(ax_disasm_sizeoverride)) {
            ax_disasm_genstr((byte*)"movdqa ");
        } else if (!!(ax_disasm_f3override)) {
            ax_disasm_genstr((byte*)"movdqu ");
        } else {
            ax_disasm_genstr((byte*)"No 66/F3 ");
        };
        ax_disasm_printaddrmode((i64)1);
        ax_disasm_genstr((byte*)", ");
        ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
    }break;
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
        ax_disasm_genstr((byte*)"[long] j");
        ax_disasm_genstr(ax_disasm_condnames[((opc & (i64)15))]);
        ax_disasm_genstr((byte*)" ");
        if (!!(ax_disasm_sizeoverride)) {
            ax_disasm_genintd(ax_disasm_readint16());
        } else {
            ax_disasm_genintd(ax_disasm_readint32());
        };
    }break;
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
        ax_disasm_decodeaddr((i64)0);
        ax_disasm_genstr((byte*)"set");
        ax_disasm_genstr(ax_disasm_condnames[((opc & (i64)15))]);
        ax_disasm_genstr((byte*)" ");
        ax_disasm_getsilx(&ax_disasm_basereg);
        ax_disasm_printaddrmode((i64)0);
    }break;
    case 175:;
    {
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_genstr((byte*)"imul ");
        ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,ax_disasm_opsize));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_printaddrmode((i64)0);
    }break;
    case 182:;
    case 183:;
    case 190:;
    case 191:;
    {
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_genstr(((opc < (i64)190)?(byte*)"movzx ":(byte*)"movsx "));
        ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,ax_disasm_opsize));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_opsize = (!!((opc & (i64)1))?(i64)2:(i64)1);
        ax_disasm_printaddrmode((i64)0);
    }break;
    case 184:;
    {
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_genstr((byte*)"popcnt ");
        ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,ax_disasm_opsize));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_printaddrmode((i64)0);
    }break;
    case 188:;
    case 189:;
    {
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_genstr(((opc == (i64)188)?(byte*)"bsf ":(byte*)"bsr "));
        ax_disasm_genstr(ax_disasm_strreg(ax_disasm_rmreg,ax_disasm_opsize));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_printaddrmode((i64)0);
    }break;
    case 214:;
    {
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_opsize = (i64)8;
        ax_disasm_genstr((byte*)"movq ");
        ax_disasm_printaddrmode((i64)1);
        ax_disasm_genstr((byte*)",");
        ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
    }break;
    case 219:;
    {
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_genstr((byte*)"pand ");
        ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_opsize = (i64)8;
        ax_disasm_printaddrmode((i64)1);
    }break;
    case 239:;
    {
        ax_disasm_decodeaddr((i64)1);
        ax_disasm_genstr((byte*)"pxor ");
        ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_rmreg));
        ax_disasm_genstr((byte*)", ");
        ax_disasm_opsize = (i64)8;
        ax_disasm_printaddrmode((i64)1);
    }break;
    default: {
        //error:
L486 :;
;
        ax_disasm_genstr((byte*)"Unknown opcode 2-byte opcode: 0F ");
        ax_disasm_genhex(opc);
    }
    } //SW
;
}

static void ax_disasm_decodeaddr(i64 w) {
    i64 modrm;
    i64 xxx;
    i64 mode;
    i64 sib;
    i64 rm;
    ax_disasm_basereg = (ax_disasm_indexreg = (i64)0);
    ax_disasm_scale = (i64)1;
    ax_disasm_offset = (i64)0;
    if (!!(w)) {
        ax_disasm_opsize = (!!(ax_disasm_sizeoverride)?(i64)2:(i64)4);
        if (!!((ax_disasm_rex & (i64)8))) {
            ax_disasm_opsize = (i64)8;
        };
    } else {
        ax_disasm_opsize = (i64)1;
    };
    modrm = (i64)((*ax_disasm_codeptr++));
    mode = (modrm >> (i64)6);
    xxx = ((modrm >> (i64)3) & (i64)7);
    rm = (modrm & (i64)7);
    if ((mode == (i64)3)) {
        ax_disasm_basereg = (rm + (i64)1);
        ax_disasm_addrmode = (i64)1;
    } else if ((rm != (i64)4)) {
        if (((mode == (i64)0) && (rm == (i64)5))) {
            ax_disasm_offset = ax_disasm_readint32();
            ax_disasm_addrmode = (i64)2;
        } else {
            ax_disasm_basereg = (rm + (i64)1);
            ax_disasm_addrmode = (i64)2;
            if ((mode==(i64)1)) {
                ax_disasm_offset = ax_disasm_readsbyte();
            }else if ((mode==(i64)2)) {
                ax_disasm_offset = ax_disasm_readint32();
            };
        };
    } else {
        ax_disasm_addrmode = (i64)2;
        sib = ax_disasm_readbyte();
        ax_disasm_indexreg = (((sib >> (i64)3) & (i64)7) + (i64)1);
        ax_disasm_basereg = ((sib & (i64)7) + (i64)1);
        ax_disasm_scale = (((sib >> (i64)6) + (i64)1)==1?(i64)1:(((sib >> (i64)6) + (i64)1)==2?(i64)2:(((sib >> (i64)6) + (i64)1)==3?(i64)4:(((sib >> (i64)6) + (i64)1)==4?(i64)8:(i64)0))));
        if (((mode == (i64)0) && (ax_disasm_basereg == (i64)6))) {
            ax_disasm_basereg = (i64)0;
            ax_disasm_offset = ax_disasm_readint32();
        } else {
            if ((mode==(i64)1)) {
                ax_disasm_offset = ax_disasm_readsbyte();
            }else if ((mode==(i64)2)) {
                ax_disasm_offset = ax_disasm_readint32();
            };
        };
        if ((ax_disasm_indexreg == (i64)5)) {
            ax_disasm_indexreg = (i64)0;
        };
    };
    if ((!!(ax_disasm_basereg) && !!((ax_disasm_rex & (i64)1)))) {
        ax_disasm_basereg += (i64)8;
    };
    if ((!!(ax_disasm_indexreg) && !!((ax_disasm_rex & (i64)2)))) {
        ax_disasm_indexreg += (i64)8;
    };
    ax_disasm_rmreg = (xxx + (i64)1);
    if (!!((ax_disasm_rex & (i64)4))) {
        ax_disasm_rmreg += (i64)8;
    };
    ax_disasm_rmopc = xxx;
}

static i64 ax_disasm_readbyte(void) {
    return (i64)((*ax_disasm_codeptr++));
}

static i64 ax_disasm_readsbyte(void) {
    return (i64)((*(i8 *)(ax_disasm_codeptr++)));
}

static u64 ax_disasm_readword16(void) {
    u64 a;
    a = (u64)((*(u16 *)(ax_disasm_codeptr)));
    ax_disasm_codeptr += (i64)2;
    return a;
}

static i64 ax_disasm_readint16(void) {
    i64 a;
    a = (i64)((*(i16 *)(ax_disasm_codeptr)));
    ax_disasm_codeptr += (i64)2;
    return a;
}

static u64 ax_disasm_readword32(void) {
    u64 a;
    a = (u64)((*(u32 *)(ax_disasm_codeptr)));
    ax_disasm_codeptr += (i64)4;
    return a;
}

static i64 ax_disasm_readint32(void) {
    i64 a;
    a = (i64)((*(i32 *)(ax_disasm_codeptr)));
    ax_disasm_codeptr += (i64)4;
    return a;
}

static i64 ax_disasm_readint64(void) {
    i64 a;
    a = (*(i64 *)(ax_disasm_codeptr));
    ax_disasm_codeptr += (i64)8;
    return a;
}

static i64 ax_disasm_getreg(i64 regcode,i64 upper) {
    if (!!(upper)) {
        return ((regcode + (i64)8) + (i64)1);
    };
    return (regcode + (i64)1);
}

static byte * ax_disasm_strreg(i64 reg,i64 opsize) {
    static byte *  regnames8[20] = {
    (byte*)"al",
    (byte*)"cl",
    (byte*)"dl",
    (byte*)"bl",
    (byte*)"ah",
    (byte*)"ch",
    (byte*)"dh",
    (byte*)"bh",
    (byte*)"r8b",
    (byte*)"r9b",
    (byte*)"r10b",
    (byte*)"r11b",
    (byte*)"r12b",
    (byte*)"r13b",
    (byte*)"r14b",
    (byte*)"r15b",
    (byte*)"spl",
    (byte*)"bpl",
    (byte*)"sil",
    (byte*)"dil"
};
    static byte *  regnames16[16] = {
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
    static byte *  regnames32[16] = {
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
    static byte *  regnames64[16] = {
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
    static byte *  mregnames8[20] = {
    (byte*)"B0",
    (byte*)"B10",
    (byte*)"B11",
    (byte*)"B4",
    (byte*)"B16",
    (byte*)"B18",
    (byte*)"B19",
    (byte*)"B17",
    (byte*)"B12",
    (byte*)"B13",
    (byte*)"B1",
    (byte*)"B2",
    (byte*)"B6",
    (byte*)"B7",
    (byte*)"B8",
    (byte*)"B9",
    (byte*)"B14",
    (byte*)"B15",
    (byte*)"B2",
    (byte*)"B3"
};
    static byte *  mregnames16[16] = {
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
    static byte *  mregnames32[16] = {
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
    static byte *  mregnames64[16] = {
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
    };
    if (!!((i64)0)) {
        if ((opsize==(i64)1)) {
            return mregnames8[(reg)-1];
        }else if ((opsize==(i64)2)) {
            return mregnames16[(reg)-1];
        }else if ((opsize==(i64)4)) {
            return mregnames32[(reg)-1];
        }else if ((opsize==(i64)8)) {
            return mregnames64[(reg)-1];
        };
    } else {
        if ((opsize==(i64)1)) {
            return regnames8[(reg)-1];
        }else if ((opsize==(i64)2)) {
            return regnames16[(reg)-1];
        }else if ((opsize==(i64)4)) {
            return regnames32[(reg)-1];
        }else if ((opsize==(i64)8)) {
            return regnames64[(reg)-1];
        };
    };
    return (byte*)"";
}

static byte * ax_disasm_strfreg(i64 freg) {
    static byte *  fregnames[8] = {(byte*)"st0",(byte*)"st1",(byte*)"st2",(byte*)"st3",(byte*)"st4",(byte*)"st5",(byte*)"st6",(byte*)"st7"};
    return fregnames[(freg)-1];
}

static void ax_disasm_printaddrmode(i64 xmm) {
    byte *  plus;
    i64 addrsize;
    ax_disasm_genstr((byte*)" ");
    if ((ax_disasm_addrmode==(i64)1)) {
        if (!!(xmm)) {
            ax_disasm_genstr(ax_disasm_strxmm(ax_disasm_basereg));
        } else {
            ax_disasm_getsilx(&ax_disasm_basereg);
            ax_disasm_genstr(ax_disasm_strreg(ax_disasm_basereg,ax_disasm_opsize));
        };
        return;
    };
    if ((ax_disasm_opsize==(i64)1)) {
        ax_disasm_genstr((byte*)"byte ");
    }else if ((ax_disasm_opsize==(i64)2)) {
        ax_disasm_genstr((byte*)"word ");
    }else if ((ax_disasm_opsize==(i64)4)) {
        ax_disasm_genstr((byte*)"dword ");
    }else if ((ax_disasm_opsize==(i64)8)) {
        ax_disasm_genstr((byte*)"qword ");
    }else if ((ax_disasm_opsize==(i64)10)) {
        ax_disasm_genstr((byte*)"tword ");
    }else if ((ax_disasm_opsize==(i64)16)) {
        ax_disasm_genstr((byte*)"oword ");
    } else {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"///OPSIZE",NULL);
        msysnewc_m_print_i64(ax_disasm_opsize,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    };
    ax_disasm_genstr((byte*)"[");
    plus = (byte*)"";
    addrsize = (!!(ax_disasm_addroverride)?(i64)4:(i64)8);
    if (!!(ax_disasm_basereg)) {
        ax_disasm_genstr(ax_disasm_strreg(ax_disasm_basereg,addrsize));
        plus = (byte*)"+";
    };
    if (!!(ax_disasm_indexreg)) {
        ax_disasm_genstr(plus);
        ax_disasm_genstr(ax_disasm_strreg(ax_disasm_indexreg,addrsize));
        if ((ax_disasm_scale > (i64)1)) {
            ax_disasm_genstr((byte*)"*");
            ax_disasm_genintd(ax_disasm_scale);
        };
        plus = (byte*)"+";
    };
    if ((!!(ax_disasm_offset) || ((ax_disasm_basereg == (i64)0) && (ax_disasm_indexreg == (i64)0)))) {
        if (((ax_disasm_basereg == (i64)0) && (ax_disasm_indexreg == (i64)0))) {
            ax_disasm_genhex(ax_disasm_offset);
        } else {
            if ((ax_disasm_offset > (i64)0)) {
                ax_disasm_genstr(plus);
            };
            ax_disasm_genintd(ax_disasm_offset);
        };
    };
    ax_disasm_genstr((byte*)"]");
    if ((ax_disasm_addrmode == (i64)3)) {
        ax_disasm_genstr((byte*)"+RIP");
    };
}

static void ax_disasm_genstr(byte * s) {
    strcat((i8 *)(ax_disasm_deststr),(i8 *)(s));
}

static void ax_disasm_genintd(i64 a) {
    ax_disasm_genstr(msysnewc_strint(a,(byte *)(0)));
}

static void ax_disasm_genhex(i64 a) {
    ax_disasm_genstr(msysnewc_strint(a,(byte*)"h"));
}

static i64 ax_disasm_readimm(void) {
    if ((ax_disasm_opsize==(i64)1)) {
        return ax_disasm_readsbyte();
    }else if ((ax_disasm_opsize==(i64)2)) {
        return ax_disasm_readint16();
    }else if ((ax_disasm_opsize==(i64)4) || (ax_disasm_opsize==(i64)8)) {
        return ax_disasm_readint32();
    };
    return (i64)0;
}

static i64 ax_disasm_readimm8(void) {
    if ((ax_disasm_opsize < (i64)8)) {
        return ax_disasm_readimm();
    };
    return ax_disasm_readint64();
}

static byte * ax_disasm_strxmm(i64 reg) {
    static byte str[32];
    msysnewc_m_print_startstr(str);
    msysnewc_m_print_str((byte*)"xmm",NULL);
    msysnewc_m_print_nogap();
    msysnewc_m_print_i64((reg - (i64)1),NULL);
    msysnewc_m_print_end();
    ;
    return str;
}

static byte * ax_disasm_strmmx(i64 reg) {
    static byte str[32];
    msysnewc_m_print_startstr(str);
    msysnewc_m_print_str((byte*)"mmx",NULL);
    msysnewc_m_print_nogap();
    msysnewc_m_print_i64((reg - (i64)1),NULL);
    msysnewc_m_print_end();
    ;
    return str;
}

static void ax_disasm_decode8087(i64 ttt) {
    byte bb;
    i64 longopc;
    i64 freg;
    i64 shortopc;
    bb = (u64)((*ax_disasm_codeptr++));
    longopc = ((ttt << (i64)8) + (i64)((u64)(bb)));
    freg = (((i64)((u64)(bb)) & (i64)7) + (i64)1);
    if ((longopc==(i64)1753)) {
        ax_disasm_genstr((byte*)"fcompp");
    }else if ((longopc==(i64)484)) {
        ax_disasm_genstr((byte*)"ftst");
    }else if ((longopc==(i64)485)) {
        ax_disasm_genstr((byte*)"fxam");
    }else if ((longopc==(i64)494)) {
        ax_disasm_genstr((byte*)"fldz");
    }else if ((longopc==(i64)488)) {
        ax_disasm_genstr((byte*)"fld1");
    }else if ((longopc==(i64)491)) {
        ax_disasm_genstr((byte*)"fldpi");
    }else if ((longopc==(i64)489)) {
        ax_disasm_genstr((byte*)"fldl2t");
    }else if ((longopc==(i64)490)) {
        ax_disasm_genstr((byte*)"fldl2e");
    }else if ((longopc==(i64)492)) {
        ax_disasm_genstr((byte*)"fldlg2");
    }else if ((longopc==(i64)493)) {
        ax_disasm_genstr((byte*)"fldln2");
    }else if ((longopc==(i64)506)) {
        ax_disasm_genstr((byte*)"fsqrt");
    }else if ((longopc==(i64)510)) {
        ax_disasm_genstr((byte*)"fsin");
    }else if ((longopc==(i64)511)) {
        ax_disasm_genstr((byte*)"fcos");
    }else if ((longopc==(i64)507)) {
        ax_disasm_genstr((byte*)"fsincos");
    }else if ((longopc==(i64)509)) {
        ax_disasm_genstr((byte*)"fscale");
    }else if ((longopc==(i64)504)) {
        ax_disasm_genstr((byte*)"fprem");
    }else if ((longopc==(i64)508)) {
        ax_disasm_genstr((byte*)"frndint");
    }else if ((longopc==(i64)500)) {
        ax_disasm_genstr((byte*)"fxtract");
    }else if ((longopc==(i64)481)) {
        ax_disasm_genstr((byte*)"fabs");
    }else if ((longopc==(i64)480)) {
        ax_disasm_genstr((byte*)"fchs");
    }else if ((longopc==(i64)498)) {
        ax_disasm_genstr((byte*)"fptan");
    }else if ((longopc==(i64)499)) {
        ax_disasm_genstr((byte*)"fpatan");
    }else if ((longopc==(i64)496)) {
        ax_disasm_genstr((byte*)"f2xm1");
    }else if ((longopc==(i64)497)) {
        ax_disasm_genstr((byte*)"fyl2x");
    }else if ((longopc==(i64)505)) {
        ax_disasm_genstr((byte*)"fyl2xp1");
    }else if ((longopc==(i64)995)) {
        ax_disasm_genstr((byte*)"finit");
    }else if ((longopc==(i64)992)) {
        ax_disasm_genstr((byte*)"feni");
    }else if ((longopc==(i64)993)) {
        ax_disasm_genstr((byte*)"fdisi");
    }else if ((longopc==(i64)994)) {
        ax_disasm_genstr((byte*)"fclex");
    }else if ((longopc==(i64)503)) {
        ax_disasm_genstr((byte*)"fincstp");
    }else if ((longopc==(i64)502)) {
        ax_disasm_genstr((byte*)"fdecstp");
    }else if ((longopc==(i64)464)) {
        ax_disasm_genstr((byte*)"fnop");
    } else {
        if (((longopc & (i64)2040)==(i64)448)) {
            ax_disasm_genstr((byte*)"fld ");
            ax_disasm_genstr(ax_disasm_strfreg(freg));
        }else if (((longopc & (i64)2040)==(i64)1488)) {
            ax_disasm_genstr((byte*)"fst ");
            ax_disasm_genstr(ax_disasm_strfreg(freg));
        }else if (((longopc & (i64)2040)==(i64)1496)) {
            ax_disasm_genstr((byte*)"fstp ");
            ax_disasm_genstr(ax_disasm_strfreg(freg));
        }else if (((longopc & (i64)2040)==(i64)456)) {
            ax_disasm_genstr((byte*)"fxch ");
            ax_disasm_genstr(ax_disasm_strfreg(freg));
        }else if (((longopc & (i64)2040)==(i64)208)) {
            ax_disasm_genstr((byte*)"fcom ");
            ax_disasm_genstr(ax_disasm_strfreg(freg));
        }else if (((longopc & (i64)2040)==(i64)216)) {
            ax_disasm_genstr((byte*)"fcomp ");
            ax_disasm_genstr(ax_disasm_strfreg(freg));
        }else if (((longopc & (i64)2040)==(i64)1472)) {
            ax_disasm_genstr((byte*)"ffree ");
            ax_disasm_genstr(ax_disasm_strfreg(freg));
        } else {
            if (((longopc & (i64)504)==(i64)192)) {
                ax_disasm_do87arith((byte*)"fadd",ttt,freg);
            }else if (((longopc & (i64)504)==(i64)224)) {
                ax_disasm_do87arith((byte*)"fsub",ttt,freg);
            }else if (((longopc & (i64)504)==(i64)232)) {
                ax_disasm_do87arith((byte*)"fsubr",ttt,freg);
            }else if (((longopc & (i64)504)==(i64)200)) {
                ax_disasm_do87arith((byte*)"fmul",ttt,freg);
            }else if (((longopc & (i64)504)==(i64)240)) {
                ax_disasm_do87arith((byte*)"fdiv",ttt,freg);
            }else if (((longopc & (i64)504)==(i64)248)) {
                ax_disasm_do87arith((byte*)"fdivr",ttt,freg);
            } else {
                --ax_disasm_codeptr;
                ax_disasm_decodeaddr((i64)0);
                shortopc = ((ttt << (i64)3) + ax_disasm_rmopc);
                if ((shortopc==(i64)61)) {
                    ax_disasm_do87mem((byte*)"fld",(i64)4);
                }else if ((shortopc==(i64)29)) {
                    ax_disasm_do87mem((byte*)"fld",(i64)5);
                }else if ((shortopc==(i64)60)) {
                    ax_disasm_do87mem((byte*)"fldbcd",(i64)-1);
                }else if ((shortopc==(i64)63)) {
                    ax_disasm_do87mem((byte*)"fstp",(i64)4);
                }else if ((shortopc==(i64)31)) {
                    ax_disasm_do87mem((byte*)"fstp",(i64)5);
                }else if ((shortopc==(i64)62)) {
                    ax_disasm_do87mem((byte*)"fstpbcd",(i64)-1);
                }else if ((shortopc==(i64)13)) {
                    ax_disasm_do87mem((byte*)"fldcw",(i64)-1);
                }else if ((shortopc==(i64)15)) {
                    ax_disasm_do87mem((byte*)"fstcw",(i64)-1);
                }else if ((shortopc==(i64)47)) {
                    ax_disasm_do87mem((byte*)"fstsw",(i64)-1);
                }else if ((shortopc==(i64)14)) {
                    ax_disasm_do87mem((byte*)"fstenv",(i64)-1);
                }else if ((shortopc==(i64)12)) {
                    ax_disasm_do87mem((byte*)"fldenv",(i64)-1);
                }else if ((shortopc==(i64)46)) {
                    ax_disasm_do87mem((byte*)"fsave",(i64)-1);
                }else if ((shortopc==(i64)44)) {
                    ax_disasm_do87mem((byte*)"frstor",(i64)-1);
                } else {
                    if (((shortopc & (i64)15)==(i64)8)) {
                        ax_disasm_do87mem((byte*)"fld",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)10)) {
                        ax_disasm_do87mem((byte*)"fst",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)11)) {
                        ax_disasm_do87mem((byte*)"fstp",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)2)) {
                        ax_disasm_do87mem((byte*)"fcom",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)3)) {
                        ax_disasm_do87mem((byte*)"fcomp",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)0)) {
                        ax_disasm_do87mem((byte*)"fadd",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)4)) {
                        ax_disasm_do87mem((byte*)"fsub",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)5)) {
                        ax_disasm_do87mem((byte*)"fsubr",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)1)) {
                        ax_disasm_do87mem((byte*)"fmul",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)6)) {
                        ax_disasm_do87mem((byte*)"fdiv",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)7)) {
                        ax_disasm_do87mem((byte*)"fdivr",(ttt >> (i64)1));
                    } else {
                        ax_disasm_genstr((byte*)"UNKNOWN x87 OPCODE");
                    };
                };
            };
        };
    };
}

static void ax_disasm_do87arith(byte * opcstr,i64 ttt,i64 freg) {
    i64 d;
    i64 p;
    d = (ttt & (i64)4);
    p = (ttt & (i64)2);
    ax_disasm_genstr(opcstr);
    if (!!(p)) {
        ax_disasm_genstr((byte*)"p");
    };
    ax_disasm_genstr((byte*)" ");
    if ((d == (i64)0)) {
        ax_disasm_genstr((byte*)"st0, ");
        ax_disasm_genstr(ax_disasm_strfreg(freg));
    } else {
        ax_disasm_genstr(ax_disasm_strfreg(freg));
        ax_disasm_genstr((byte*)", st0");
    };
}

static void ax_disasm_do87mem(byte * opcstr,i64 mf) {
    ax_disasm_genstr((byte*)"f");
    if ((mf==(i64)0)) {
        ax_disasm_opsize = (i64)4;
    }else if ((mf==(i64)1)) {
        ax_disasm_genstr((byte*)"i");
        ax_disasm_opsize = (i64)4;
    }else if ((mf==(i64)2)) {
        ax_disasm_opsize = (i64)8;
    }else if ((mf==(i64)3)) {
        ax_disasm_genstr((byte*)"i");
        ax_disasm_opsize = (i64)2;
    }else if ((mf==(i64)4)) {
        ax_disasm_genstr((byte*)"i");
        ax_disasm_opsize = (i64)8;
    }else if ((mf==(i64)5)) {
        ax_disasm_opsize = (i64)10;
    };
    ax_disasm_genstr((opcstr + (i64)1));
    ax_disasm_genstr((byte*)" ");
    ax_disasm_printaddrmode((i64)0);
}

static void ax_disasm_getsil(i64 * reg) {
    if (((((ax_disasm_opsize == (i64)1) && !!(ax_disasm_rex)) && ((*reg) >= (i64)5)) && ((*reg) <= (i64)8))) {
        (*reg) += (i64)12;
    };
}

static void ax_disasm_getsilx(i64 * reg) {
    if ((((((ax_disasm_addrmode == (i64)1) && (ax_disasm_opsize == (i64)1)) && !!(ax_disasm_rex)) && ((*reg) >= (i64)5)) && ((*reg) <= (i64)8))) {
        (*reg) += (i64)12;
    };
}

void ax_writeobj_writess(byte * outfile) {
    ax_writeobj_writecoff(outfile);
}

static void ax_writeobj_writerecord(void * r,i64 length) {
    memcpy((void *)(ax_writeobj_dataptr),r,(u64)(length));
    ax_writeobj_dataptr += length;
}

static void ax_writeobj_writerelocs(struct ax_decls_relocrec * r,i64 nrelocs) {
    static struct ax_objdecls_coffrelocrec s;
    struct ax_decls_strec *  d;
    if ((nrelocs == (i64)0)) {
        return;
    };
    L487 :;
    while (!!(r)) {
        if (((*r).reloctype==(i64)2) || ((*r).reloctype==(i64)1)) {
            d = (*ax_decls_ss_symboltable)[((*r).stindex)-1];
            if (((i64)((*d).segment)==(i64)3)) {
                s.stindex = (i64)2;
            }else if (((i64)((*d).segment)==(i64)2)) {
                s.stindex = (i64)4;
            }else if (((i64)((*d).segment)==(i64)1)) {
                s.stindex = (i64)6;
            }else if (((i64)((*d).segment)==(i64)0)) {
                s.stindex = ((*r).stindex + ax_writeobj_stoffset);
            } else {
                ax_lib_gerror((byte*)"wrelocs/bad seg");
            };
        } else {
            s.stindex = ((*r).stindex + ax_writeobj_stoffset);
        };
        s.reloctype = (*r).reloctype;
        s.virtualaddr = (*r).offset;
        memcpy((void *)(ax_writeobj_dataptr),(void *)(&s),(u64)((i64)10));
        ax_writeobj_dataptr += (i64)10;
        r = (*r).nextreloc;
L488 :;
    }L489 :;
    ;
}

static void ax_writeobj_writedata(struct ax_decls_dbuffer * data) {
    memcpy((void *)(ax_writeobj_dataptr),ax_lib_bufferelemptr(data,(i64)0),(u64)(ax_lib_bufferlength(data)));
    ax_writeobj_dataptr += ax_lib_bufferlength(data);
}

static void ax_writeobj_writesymboltable(void) {
    i64 i;
    L490 :;
    for (i=(i64)1;i<=ax_writeobj_nsymbols;i+=(i64)1) {
L491 :;
        ax_writeobj_writerecord((void *)(&ax_writeobj_symboltable[(i)]),(i64)18);
L492 :;
    }L493 :;
    ;
}

static void ax_writeobj_writestringtable(void) {
    i32 *  p;
    i64 i;
    i64 n;
    p = (i32 *)(ax_writeobj_dataptr);
    (*p) = ax_writeobj_nextstringoffset;
    ax_writeobj_dataptr += (i64)4;
    L494 :;
    for (i=(i64)1;i<=ax_writeobj_nstrings;i+=(i64)1) {
L495 :;
        n = (ax_writeobj_stringlengths[(i)-1] + (i64)1);
        memcpy((void *)(ax_writeobj_dataptr),(void *)(ax_writeobj_stringtable[(i)-1]),(u64)(n));
        ax_writeobj_dataptr += n;
L496 :;
    }L497 :;
    ;
}

static struct ax_objdecls_imagesymbol * ax_writeobj_makesymbol(byte * name,i64 namelen,i64 value,i64 sectionno,i64 symtype,i64 storage,i64 naux) {
    static struct ax_objdecls_imagesymbol r;
    if ((namelen == (i64)0)) {
        namelen = (i64)(strlen((i8 *)(name)));
    };
    if ((namelen < (i64)8)) {
        strcpy((i8 *)(&r.shortname[((i64)1)-1]),(i8 *)(name));
    } else if ((namelen == (i64)8)) {
        memcpy((void *)(&r.shortname[((i64)1)-1]),(void *)(name),(u64)(namelen));
    } else {
        r.shortx = (u64)((i64)0);
        r.longx = (u64)(ax_writeobj_addstringentry(name,namelen));
    };
    r.value = (u64)(value);
    r.sectionno = sectionno;
    r.symtype = (u64)(symtype);
    r.storageclass = (u64)(storage);
    r.nauxsymbols = (u64)(naux);
    return &r;
}

static void ax_writeobj_addsymbol(struct ax_objdecls_imagesymbol * r) {
    if ((ax_writeobj_nsymbols >= (i64)10001)) {
        ax_lib_gerror((byte*)"as:Too many symbols");
    };
    memcpy((void *)(&ax_writeobj_symboltable[(++ax_writeobj_nsymbols)]),(void *)(r),(u64)((i64)18));
}

static void ax_writeobj_initsymboltable(byte * filename) {
    ax_writeobj_nsymbols = (i64)0;
    ax_writeobj_addsymbol(ax_writeobj_makesymbol((byte*)".file",(i64)0,(i64)0,(i64)-2,(i64)0,(i64)103,(i64)1));
    ax_writeobj_addsymbol(ax_writeobj_strtoaux(filename));
    ax_writeobj_addsymbol(ax_writeobj_makesymbol((byte*)".bss",(i64)0,(i64)0,(i64)1,(i64)0,(i64)3,(i64)1));
    ax_writeobj_addsymbol((struct ax_objdecls_imagesymbol *)(ax_writeobj_sectiontoaux((struct ax_decls_dbuffer *)(0),(i64)0)));
    ax_writeobj_addsymbol(ax_writeobj_makesymbol((byte*)".data",(i64)0,(i64)0,(i64)2,(i64)0,(i64)3,(i64)1));
    ax_writeobj_addsymbol((struct ax_objdecls_imagesymbol *)(ax_writeobj_sectiontoaux(ax_decls_ss_idata,ax_decls_ss_nidatarelocs)));
    ax_writeobj_addsymbol(ax_writeobj_makesymbol((byte*)".text",(i64)0,(i64)0,(i64)3,(i64)0,(i64)3,(i64)1));
    ax_writeobj_addsymbol((struct ax_objdecls_imagesymbol *)(ax_writeobj_sectiontoaux(ax_decls_ss_code,ax_decls_ss_ncoderelocs)));
}

static struct ax_objdecls_imagesymbol * ax_writeobj_strtoaux(byte * s) {
    static struct ax_objdecls_imagesymbol r;
    byte *  p;
    i64 n;
    p = (byte *)(&r);
    memset((void *)(p),(i64)0,(u64)((i64)18));
    n = (i64)0;
    L498 :;
    while ((((i64)((*s)) != (i64)0) && (n < (i64)18))) {
        (*p++) = (u64)((*s++));
        ++n;
L499 :;
    }L500 :;
    ;
    return &r;
}

static struct ax_objdecls_auxsectionrec * ax_writeobj_sectiontoaux(struct ax_decls_dbuffer * data,i64 nrelocs) {
    static struct ax_objdecls_auxsectionrec r;
    memset((void *)(&r),(i64)0,(u64)((i64)18));
    if ((data == 0)) {
        r.length = ax_decls_ss_zdatalen;
    } else {
        r.length = ax_lib_bufferlength(data);
    };
    r.nrelocs = nrelocs;
    return &r;
}

static i64 ax_writeobj_addstringentry(byte * s,i64 length) {
    i64 offset;
    offset = ax_writeobj_nextstringoffset;
    if ((ax_writeobj_nstrings > (i64)5000)) {
        ax_lib_gerror((byte*)"W:too many strings");
    };
    ax_writeobj_stringtable[(++ax_writeobj_nstrings)-1] = s;
    ax_writeobj_stringlengths[(ax_writeobj_nstrings)-1] = length;
    ax_writeobj_nextstringoffset += (length + (i64)1);
    return offset;
}

static void ax_writeobj_convertsymboltable(void) {
    struct ax_decls_strec *  s;
    byte *  name;
    i64 i;
    i64 sect;
    i64 scope;
    ax_writeobj_stoffset = (ax_writeobj_nsymbols - (i64)1);
    ax_writeobj_nstrings = (i64)0;
    ax_writeobj_nextstringoffset = (i64)4;
    L501 :;
    for (i=(i64)1;i<=ax_decls_ss_nsymbols;i+=(i64)1) {
L502 :;
        s = (*ax_decls_ss_symboltable)[(i)-1];
        name = (*s).name;
        if (((i64)((*s).segment)==(i64)3)) {
            sect = (i64)1;
        }else if (((i64)((*s).segment)==(i64)2)) {
            sect = (i64)2;
        }else if (((i64)((*s).segment)==(i64)1)) {
            sect = (i64)3;
        } else {
            sect = (i64)0;
        };
        if (((i64)((*s).symbol)==(i64)19) || ((i64)((*s).symbol)==(i64)20)) {
            scope = (i64)3;
        }else if (((i64)((*s).symbol)==(i64)21) || ((i64)((*s).symbol)==(i64)22)) {
            scope = (i64)2;
        } else {
            scope = (i64)0;
        };
        ax_writeobj_addsymbol(ax_writeobj_makesymbol((*s).name,(i64)((*s).namelen),(i64)((*s).offset),sect,(i64)0,scope,(i64)0));
L503 :;
    }L504 :;
    ;
}

static void ax_writeobj_writecoff(byte * outfile) {
    struct ax_objdecls_imagefileheader header;
    struct ax_objdecls_imagesectionheader zsection;
    struct ax_objdecls_imagesectionheader isection;
    struct ax_objdecls_imagesectionheader csection;
    i64 offset;
    memset((void *)(&header),(i64)0,(u64)((i64)20));
    memset((void *)(&zsection),(i64)0,(u64)((i64)40));
    memset((void *)(&isection),(i64)0,(u64)((i64)40));
    memset((void *)(&csection),(i64)0,(u64)((i64)40));
    header.machine = (u64)((i64)34404);
    header.nsections = (u64)((i64)3);
    strcpy((i8 *)(&zsection.name[((i64)1)-1]),(i8 *)((byte*)".bss"));
    zsection.rawdata_size = (u64)(ax_decls_ss_zdatalen);
    zsection.characteristics = (u64)((i64)3225419904);
    if (((ax_decls_ss_nidatarelocs >= (i64)65536) || (ax_decls_ss_ncoderelocs >= (i64)65536))) {
        ax_lib_gerror((byte*)"Too many relocs (exceeds 16-bit field)");
    };
    strcpy((i8 *)(&isection.name[((i64)1)-1]),(i8 *)((byte*)".data"));
    isection.rawdata_size = (u64)(ax_lib_bufferlength(ax_decls_ss_idata));
    isection.nrelocs = (u64)(ax_decls_ss_nidatarelocs);
    isection.characteristics = (u64)((i64)3226468416);
    strcpy((i8 *)(&csection.name[((i64)1)-1]),(i8 *)((byte*)".text"));
    csection.rawdata_size = (u64)(ax_lib_bufferlength(ax_decls_ss_code));
    csection.nrelocs = (u64)(ax_decls_ss_ncoderelocs);
    csection.characteristics = (u64)((i64)1615855648);
    ax_writeobj_initsymboltable(outfile);
    ax_writeobj_convertsymboltable();
    offset = (i64)20;
    offset += (i64)120;
    if (!!((u64)(isection.nrelocs))) {
        isection.relocations_ptr = (u64)(offset);
        offset += ((i64)((u64)(isection.nrelocs)) * (i64)10);
    };
    if (!!((u64)(csection.nrelocs))) {
        csection.relocations_ptr = (u64)(offset);
        offset += ((i64)((u64)(csection.nrelocs)) * (i64)10);
    };
    isection.rawdata_offset = (u64)(offset);
    offset += (i64)(isection.rawdata_size);
    csection.rawdata_offset = (u64)(offset);
    offset += (i64)(csection.rawdata_size);
    header.symtaboffset = (u64)(offset);
    offset += (ax_writeobj_nsymbols * (i64)18);
    header.nsymbols = (u64)(ax_writeobj_nsymbols);
    offset += ax_writeobj_nextstringoffset;
    ax_writeobj_datastart = (ax_writeobj_dataptr = (byte *)(malloc((u64)(offset))));
    ax_writeobj_writerecord((void *)(&header),(i64)20);
    ax_writeobj_writerecord((void *)(&zsection),(i64)40);
    ax_writeobj_writerecord((void *)(&isection),(i64)40);
    ax_writeobj_writerecord((void *)(&csection),(i64)40);
    ax_writeobj_writerelocs(ax_decls_ss_idatarelocs,ax_decls_ss_nidatarelocs);
    ax_writeobj_writerelocs(ax_decls_ss_coderelocs,ax_decls_ss_ncoderelocs);
    ax_writeobj_writedata(ax_decls_ss_idata);
    ax_writeobj_writedata(ax_decls_ss_code);
    ax_writeobj_writesymboltable();
    ax_writeobj_writestringtable();
    if (!!(ax_decls_fverbose)) {
        msysnewc_m_print_startcon();
        msysnewc_m_print_str((byte*)"Writing file:",NULL);
        msysnewc_m_print_str(outfile,NULL);
        msysnewc_m_print_newline();
        msysnewc_m_print_end();
        ;
    };
    mlib_writefile(outfile,ax_writeobj_datastart,(ax_writeobj_dataptr - ax_writeobj_datastart));
}


/* ********** End of C Code ********** */
