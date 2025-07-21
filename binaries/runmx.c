// Generated C
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"

extern double sqrt(double);
extern double sin(double);
extern double cos(double);
extern double tan(double);
extern double asin(double);
extern double acos(double);
extern double atan(double);
extern double log(double);
extern double log10(double);
extern double exp(double);
extern double floor(double);
extern double ceil(double);
extern double atan2(double, double);
extern double fmod(double, double);
extern double pow(double, double);

typedef signed char		i8;
typedef short			i16;
typedef int				i32;
typedef long long int	i64;
typedef unsigned char			u8;
typedef unsigned short			u16;
typedef unsigned int			u32;
typedef unsigned long long int	u64;

typedef unsigned char byte;

typedef float r32;
typedef double r64;

extern void exit(i32);
extern void memset(u64, i32, u64);

#define asi8(x)  *(i8*)&x
#define asi16(x) *(i16*)&x
#define asi32(x) *(i32*)&x
#define asi64(x) *(i64*)&x

#define asu8(x)  *(u8*)&x
#define asu16(x) *(u16*)&x
#define asu32(x) *(u32*)&x
#define asu64(x) *(u64*)&x

#define asr32(x) *(r32*)&x
#define asr64(x) *(r64*)&x


#define toi8(x)  (i8)x
#define toi16(x) (i16)x
#define toi32(x) (i32)x
#define toi64(x) (i64)x

#define tou8(x)  (u8)x
#define tou16(x) (u16)x
#define tou32(x) (u32)x
#define tou64(x) (u64)x

#define tor32(x) (r32)x
#define tor64(x) (r64)x


#define toi8p(x)  (i8*)x
#define toi16p(x) (i16*)x
#define toi32p(x) (i32*)x
#define toi64p(x) (i64*)x

#define tou8p(x)  (u8*)x
#define tou16p(x) (u16*)x
#define tou32p(x) (u32*)x
#define tou64p(x) (u64*)x

#define tor32p(x) (r32*)x
#define tor64p(x) (r64*)x


i64 Getdotindex(u64 a, int i);
u64 Setdotindex(u64 a, i64 i, i64 x);
i64 Getdotslice(u64 a, i64 i, i64 j);
u64 Setdotslice(u64 a, i64 i, i64 j, u64 x);
i64 Poweri64(i64 a, i64 n);

#define Min(x, y) (x<=y ? x : y)
#define Max(x, y) (x>=y ? x : y)

i64 ncmdparams;
i64 nenvstrings;
char* (*cmdparams)[];
i64 $cmdskip;

/*
PROC START
*/
void runmx_main();
void (*entrypoint)(void) = runmx_main;

// ***** Types *****
struct $B1 {u64 a[10];};   // mem:80;
struct $B2 {u16 a[5];};   // mem:10;
struct $B3 {u64 a[2];};   // mem:16;
struct $B4 {u64 a[512];};   // mem:4096;
struct $B5 {u32 a[25];};   // mem:100;
struct $B6 {u64 a[40];};   // mem:320;
struct $B7 {u64 a[301];};   // mem:2408;
struct $B8 {u64 a[3];};   // mem:24;
struct $B9 {u32 a[3];};   // mem:12;
struct $B10 {u8 a[2049];};   // mem:2049;
struct $B11 {u64 a[9];};   // mem:72;
struct $B12 {u64 a[6];};   // mem:48;
struct $B13 {u32 a[9];};   // mem:36;
struct $B14 {u32 a[65];};   // mem:260;
struct $B15 {u32 a[75];};   // mem:300;
struct $B16 {u64 a[32];};   // mem:256;
struct $B17 {u32 a[5];};   // mem:20;
struct $B18 {u64 a[14];};   // mem:112;
struct $B19 {u64 a[20];};   // mem:160;
struct $B20 {u64 a[3000];};   // mem:24000;
struct $B21 {u64 a[375];};   // mem:3000;
struct $B22 {u64 a[750];};   // mem:6000;
struct $B23 {u64 a[8];};   // mem:64;
struct $B24 {u64 a[16];};   // mem:128;
struct $B25 {u64 a[4];};   // mem:32;
struct $B26 {u64 a[13];};   // mem:104;
struct $B27 {u64 a[27];};   // mem:216;
struct $B28 {u8 a[1];};   // mem:0;
struct $B29 {u64 a[5];};   // mem:40;
struct $B30 {u64 a[45];};   // mem:360;
struct $B31 {u64 a[64];};   // mem:512;
struct $B32 {u64 a[320];};   // mem:2560;
struct $B33 {u64 a[41];};   // mem:328;

// Function Ptr Types:
typedef i64 (*F1)(u64);
typedef i64 (*F2)();
typedef i64 (*F3)(i64);
typedef i64 (*F4)(i64, i64);
typedef i64 (*F5)(i64, i64, i64);
typedef i64 (*F6)(i64, i64, i64, i64);
typedef i64 (*F7)(i64, i64, i64, i64, i64);
typedef i64 (*F8)(i64, i64, i64, i64, i64, i64);
typedef i64 (*F9)(i64, i64, i64, i64, i64, i64, i64, i64);
typedef i64 (*F10)(i64, i64, i64, i64, i64, i64, i64, i64, i64);
typedef i64 (*F11)(i64, i64, i64, i64, i64, i64, i64, i64, i64, i64);
typedef i64 (*F12)(i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64);
typedef i64 (*F13)(i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64);
typedef r64 (*F14)();
typedef r64 (*F15)(i64);
typedef r64 (*F16)(i64, i64);
typedef void (*F17)();

// ***** Variables *****
static i64 msysc_fmtparam;
i64 msysc_$cmdskip;
static i64 msysc_needgap;
static i64 msysc_outdev;
static u64 msysc_outchan;
static u64 msysc_fmtstr;
static struct $B1 msysc_outchan_stack;
static struct $B1 msysc_outdev_stack;
static struct $B1 msysc_fmtstr_stack;
static struct $B2 msysc_needgap_stack;
static struct $B1 msysc_ptr_stack;
static i64 msysc_niostack;
static struct $B3 msysc_digits;
static struct $B3 msysc_defaultfmt;
static u64 msysc_rd_buffer;
static i64 msysc_rd_length;
static u64 msysc_rd_pos;
static u64 msysc_rd_lastpos;
static i64 msysc_termchar;
static i64 msysc_itemerror;
static struct $B4 msysc_printbuffer;
static u64 msysc_printptr;
static i64 msysc_printlen;
static i64 msysc_ncmdparams;
static u64 msysc_cmdparams;
static struct $B3 msysc_getfmt_fmt;
static struct $B5 msysc_strint_str;
static struct $B5 msysc_strword_str;
static struct $B6 msysc_strreal_str;
static struct $B7 mlib_allocupper;
static i64 mlib_alloccode;
static i64 mlib_allocbytes;
static i64 mlib_fdebug;
static i64 mlib_rfsize;
static u64 mlib_maxmemory;
static i64 mlib_maxalloccode;
static u8 mlib_pcm_setup;
static i64 mlib_show;
static i64 mlib_memtotal;
static i64 mlib_smallmemtotal;
static i64 mlib_smallmemobjs;
static i64 mlib_maxmemtotal;
static struct $B8 mlib_memalloctable;
static struct $B9 mlib_memallocsize;
static u64 mlib_pcheapstart;
static u64 mlib_pcheapend;
static u64 mlib_pcheapptr;
static struct $B10 mlib_sizeindextable;
static struct $B11 mlib_freelist;
static struct $B12 mlib_pmnames;
static struct $B3 mlib_seed;
static i64 mlib_pcm_newblock_totalheapsize;
static struct $B14 mlib_changeext_newfile;
static struct $B14 mlib_extractpath_str;
static struct $B5 mlib_extractbasefile_str;
static struct $B15 mlib_nextcmdparamnew_str;
static struct $B16 mlib_readnextfileitem_str;
static struct $B16 mlib_padstr_str;
static u64 mlib_chr_str;
static u64 mwindows_hconsole;
static u64 mwindows_hconsolein;
static struct $B17 mwindows_lastkey;
static struct $B17 mwindows_pendkey;
static i64 mwindows_keypending;
static i64 mwindows_hpfreq;
static u64 mwindows_wndproc_callbackfn;
static i64 mwindows_init_flag;
static u8 mwindows_os_gxregisterclass_registered;
static struct $B15 mwindows_os_gethostname_name;
static i64 mwindows_os_gethostname_n;
static i64 mwindows_os_peek_lastticks;
static i64 runmxshow_logdest;
static u64 runmxshow_logdev;
static struct $B3 runmxshow_destv;
static u64 runmxshow_dest;
struct $B18 mx_decls_mcxdirnames;
struct $B12 mx_decls_mcxrelocnames;
static struct $B19 mx_decls_dllnametable;
static struct $B19 mx_decls_dllinsttable;
static i64 mx_decls_ndlllibs;
static struct $B19 mx_decls_libnametable;
static struct $B19 mx_decls_libtable;
static struct $B17 mx_decls_librelocated;
static struct $B17 mx_decls_libinitdone;
static i64 mx_decls_nlibs;
static struct $B20 mx_decls_symbolnametable;
static struct $B21 mx_decls_symboldefined;
static struct $B20 mx_decls_symboladdress;
static struct $B22 mx_decls_symbollibindex;
static struct $B21 mx_decls_symboldllindex;
static i64 mx_decls_nsymbols;
i64 mx_decls_nsymimports;
i64 mx_decls_nsymexports;
static struct $B12 mx_lib_rsegmentnames;
static i64 mc_disasm_nmodules;
static i64 mc_disasm_xfchsmask_pd;
static struct $B23 mc_disasm_opnames;
static struct $B24 mc_disasm_condnames;
static struct $B8 mc_disasm_addrmodenames;
static i64 mc_disasm_rex;
static i64 mc_disasm_addrmode;
static i64 mc_disasm_rmreg;
static i64 mc_disasm_rmopc;
static i64 mc_disasm_ripmode;
static i64 mc_disasm_basereg;
static i64 mc_disasm_indexreg;
static i64 mc_disasm_scale;
static i64 mc_disasm_opsize;
static i64 mc_disasm_offset;
static i64 mc_disasm_offsetsize;
static i64 mc_disasm_sizeoverride;
static i64 mc_disasm_addroverride;
static i64 mc_disasm_f2override;
static i64 mc_disasm_f3override;
static struct $B16 mc_disasm_deststr;
static u64 mc_disasm_destptr;
static u64 mc_disasm_codeptr;
static struct $B16 mc_disasm_decodeinstr_str;
static struct $B5 mc_disasm_printaddrmode_str;
static struct $B25 mc_disasm_strxmm_str;
static struct $B25 mc_disasm_strmmx_str;
static i64 $nprocs;
static struct $B28 $procname;
static struct $B28 $procaddr;

// ***** Imported Functions *****
extern u64 malloc(u64 $1);
extern u64 realloc(u64 $1, u64 $2);
extern void free(u64 $1);
extern void memset(u64 $1, i32 $2, u64 $3);
extern void memcpy(u64 $1, u64 $2, u64 $3);
extern void memmove(u64 $1, u64 $2, u64 $3);
extern i32 clock();
extern i32 ftell(u64 $1);
extern i32 fseek(u64 $1, i32 $2, i32 $3);
extern u64 fread(u64 $1, u64 $2, u64 $3, u64 $4);
extern u64 fwrite(u64 $1, u64 $2, u64 $3, u64 $4);
extern i32 getc(u64 $1);
extern i32 ungetc(i32 $1, u64 $2);
extern u64 fopen(u64 a, u64 b);
extern i32 fclose(u64 $1);
extern u64 fgets(u64 $1, i64 $2, u64 $3);
extern i32 remove(u64 $1);
extern i32 rename(u64 $1, u64 $2);
extern i32 getchar();
extern void putchar(i32 $1);
extern void setbuf(u64 $1, u64 $2);
extern i64 strlen(u64 $1);
extern u64 strcpy(u64 $1, u64 $2);
extern i32 strcmp(u64 $1, u64 $2);
extern i32 strncmp(u64 $1, u64 $2, u64 $3);
extern u64 strncpy(u64 $1, u64 $2, u64 $3);
extern i32 memcmp(u64 $1, u64 $2, u64 $3);
extern u64 strcat(u64 $1, u64 $2);
extern i32 tolower(i32 $1);
extern i32 toupper(i32 $1);
extern i32 isalpha(i32 $1);
extern i32 isupper(i32 $1);
extern i32 islower(i32 $1);
extern i32 isalnum(i32 $1);
extern i32 isspace(i32 $1);
extern u64 strstr(u64 $1, u64 $2);
extern i64 atol(u64 $1);
extern i32 atoi(u64 $1);
extern r64 strtod(u64 $1, u64 $2);
extern u64 _strdup(u64 $1);
extern i32 puts(u64 $1);
extern i32 printf(u64 $1, ...);
extern i32 sprintf(u64 $1, u64 $2, ...);
extern i32 sscanf(u64 $1, u64 $2, ...);
extern i32 scanf(u64 $1, ...);
extern i32 rand();
extern void srand(u32 $1);
extern i32 system(u64 $1);
extern i32 fgetc(u64 $1);
extern i32 fputc(i32 $1, u64 $2);
extern i32 fprintf(u64 $1, u64 $2, ...);
extern i32 fputs(u64 $1, u64 $2);
extern i32 feof(u64 $1);
extern i32 getch();
extern i32 _getch();
extern i32 kbhit();
extern i32 _mkdir(u64 $1);
extern i32 mkdir(u64 $1);
extern u64 strchr(u64 $1, i32 $2);
extern i32 _setmode(i32 $1, i32 $2);
extern void _exit(i32 $1);
extern void exit(i32 $1);
extern r64 pow(r64 $1, r64 $2);
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
extern void qsort(u64 $1, u64 $2, u64 $3, u64 $4);
extern i32 __getmainargs(u64 $1, u64 $2, u64 $3, i64 $4, u64 $5);
extern u64 GetStdHandle(u32 $1);
extern i64 GetConsoleScreenBufferInfo(u64 $1, u64 $2);
extern i64 SetConsoleCtrlHandler(u64 $1, i64 $2);
extern i64 SetConsoleMode(u64 $1, u32 $2);
extern i64 CreateProcessA(u64 $1, u64 $2, u64 $3, u64 $4, i64 $5, u32 $6, u64 $7, u64 $8, u64 $9, u64 $10);
extern u32 GetLastError();
extern u32 WaitForSingleObject(u64 $1, u32 $2);
extern i64 GetExitCodeProcess(u64 $1, u64 $2);
extern i64 CloseHandle(u64 $1);
extern i64 GetNumberOfConsoleInputEvents(u64 $1, u64 $2);
extern i64 FlushConsoleInputBuffer(u64 $1);
extern u64 LoadLibraryA(u64 $1);
extern u64 GetProcAddress(u64 $1, u64 $2);
extern u64 LoadCursorA(u64 $1, u64 $2);
extern u32 RegisterClassExA(u64 $1);
extern i64 DefWindowProcA(u64 $1, u32 $2, u64 $3, u64 $4);
extern i64 ReadConsoleInputA(u64 $1, u64 $2, u32 $3, u64 $4);
extern void Sleep(u32 $1);
extern u32 GetModuleFileNameA(u64 $1, u64 $2, u32 $3);
extern void ExitProcess(u32 $1);
extern void PostQuitMessage(i32 $1);
extern void MessageBoxA(i32 x, u64 message, u64 caption, i32 y);
extern u32 QueryPerformanceCounter(u64 $1);
extern u32 QueryPerformanceFrequency(u64 $1);
extern u64 CreateFileA(u64 $1, u32 $2, u32 $3, u64 $4, u32 $5, u32 $6, u64 $7);
extern u32 GetFileTime(u64 $1, u64 $2, u64 $3, u64 $4);
extern void GetSystemTime(u64 $1);
extern void GetLocalTime(u64 $1);
extern u64 GetTickCount64();
extern u32 PeekMessageA(u64 $1, u64 $2, u32 $3, u32 $4, u32 $5);
extern u64 GetCommandLineA();
extern u64 VirtualAlloc(u64 $1, u32 $2, u32 $3, u32 $4);
extern u32 VirtualProtect(u64 $1, u32 $2, u32 $3, u64 $4);
extern u32 WriteConsoleA(u64 $1, u64 $2, i32 $3, u64 $4, u64 $5);
extern u64 FindFirstFileA(u64 $1, u64 $2);
extern u32 FindNextFileA(u64 $1, u64 $2);
extern u32 FindClose(u64 $1);
extern u32 MessageBeep(i32 $1);
extern u32 Beep(i32 freq, i32 dur);

// ***** Function Declarations *****
static u64 msysc_getfmt(u64 fmtstyle);
static u64 msysc_strint(i64 a, u64 fmtstyle);
static u64 msysc_strword(u64 a, u64 fmtstyle);
static u64 msysc_strreal(r64 a, u64 fmtstyle);
static u64 mlib_pcm_newblock(i64 itemsize);
static i64 mlib_pcm_round(i64 n);
static u64 mlib_changeext(u64 s, u64 newext);
static u64 mlib_extractpath(u64 s);
static u64 mlib_extractbasefile(u64 s);
static i64 mlib_nextcmdparamnew(u64 paramno, u64 name, u64 value, u64 defext);
static i64 mlib_readnextfileitem(u64 fileptr, u64 item);
static u64 mlib_padstr(u64 s, i64 width, u64 padchar);
static u64 mlib_chr(i64 c);
static void mwindows_os_gxregisterclass(u64 classname);
static i64 mwindows_mainwndproc(u64 hwnd, u32 message, u64 wparam, u64 lparam);
static u64 mwindows_os_gethostname();
static void mwindows_os_peek();
static u64 mc_disasm_decodeinstr(u64 cptr, u64 baseaddr);
static u64 mc_disasm_strreg(i64 reg, i64 opsize);
static u64 mc_disasm_strfreg(i64 freg);
static void mc_disasm_printaddrmode(i64 xmm);
static u64 mc_disasm_strxmm(i64 reg);
static u64 mc_disasm_strmmx(i64 reg);
static void msysc_$getcommands(i64 ncmd, u64 cmds, i64 cmdskipl);
static void msysc_pushio();
static void msysc_m$print_startfile(u64 dev);
static void msysc_resetprintbuffer();
static void msysc_m$print_startstr(u64 s);
static void msysc_m$print_startptr(u64 p);
static void msysc_m$print_startcon();
static void msysc_m$print_setfmt(u64 format);
static void msysc_m$print_end();
static void msysc_nextfmtchars(i64 lastx);
static void msysc_dumpprintbuffer();
static void msysc_m$print_ptr(u64 a, u64 fmtstyle);
static void msysc_m$print_u64(u64 a, u64 fmtstyle);
static void msysc_m$print_ptr_nf(u64 a);
static void msysc_m$print_i64(i64 a, u64 fmtstyle);
static i64 msysc_u64tostr(u64 aa, u64 s, u64 base, i64 sep);
static void msysc_printstr_n(u64 s, i64 n);
static void msysc_strtofmt(u64 s, i64 slen, u64 fmt);
static void msysc_tostr_i64(i64 a, u64 fmt);
static void msysc_m$print_i64_nf(i64 a);
static void msysc_m$print_bool(i64 a, u64 fmtstyle);
static void msysc_m$print_str(u64 s, u64 fmtstyle);
static void msysc_printstr(u64 s);
static void msysc_tostr_u64(u64 a, u64 fmt);
static void msysc_m$print_r64(r64 x, u64 fmtstyle);
static void msysc_tostr_r64(r64 x, u64 fmt);
static void msysc_m$print_r32(r32 x, u64 fmtstyle);
static void msysc_m$print_c8(i64 a, u64 fmtstyle);
static i64 msysc_domultichar(u64 p, i64 n, u64 dest, u64 fmt);
static i64 msysc_getutfsize(u64 s);
static void msysc_tostr_str(u64 s, i64 oldlen, u64 fmt);
static void msysc_m$print_strn(u64 s, i64 length, u64 fmtstyle);
static void msysc_m$print_str_nf(u64 s);
static void msysc_m$print_strsl(struct $B3 s, u64 fmtstyle);
static void mlib_abortprogram(u64 s);
static void msysc_m$print_newline();
static void msysc_m$print_nogap();
static void msysc_m$print_space();
static void msysc_dumpstr(u64 s, i64 n, i64 fbuffer);
static void msysc_printstrn_app(u64 s, i64 length, u64 f);
static void msysc_printchar(i64 ch);
static i64 msysc_expandstr(u64 s, u64 t, i64 n, u64 fmt);
static i64 msysc_i64tostrfmt(i64 aa, u64 s, u64 fmt);
static i64 msysc_i64mintostr(u64 s, i64 base, i64 sep);
static u64 mlib_convlcstring(u64 s);
static i64 msysc_u64tostrfmt(i64 aa, u64 s, u64 fmt);
static i64 msysc_strtostrfmt(u64 s, u64 t, i64 n, u64 fmt);
static u64 mlib_pcm_alloc(i64 n);
static u64 mlib_convucstring(u64 s);
static void mlib_pcm_free(u64 p, i64 n);
static u64 msysc_getstr(u64 s, u64 fmt);
static void msysc_getstrint(i64 a, u64 dest);
static u64 mlib_pcm_copyheapstring(u64 s);
static void msysc_initreadbuffer();
static void msysc_m$read_conline();
static void mlib_readlinen(u64 handlex, u64 buffer, i64 size);
static void msysc_m$read_fileline(u64 f);
static void msysc_m$read_strline(u64 s);
static u64 msysc_readitem(u64 itemlength);
static i64 msysc_strtoint(u64 s, i64 length, u64 base);
static i64 msysc_m$read_i64(i64 fmt);
static r64 msysc_m$read_r64(i64 fmt);
static void msysc_m$read_str(u64 dest, i64 destlen, i64 fmt);
static void mlib_iconvlcn(u64 s, i64 n);
static void msysc_readstr(u64 dest, i64 fmt, i64 destlen);
static void msysc_rereadln();
static void msysc_reread();
static i64 msysc_valint(u64 s, i64 fmt);
static r64 msysc_valreal(u64 s);
static void msysc_mclunimpl(u64 mess);
static void msysc_addtobuffer(u64 s, i64 n);
static i64 msysc_m$sign_i64(i64 a);
static r64 msysc_m$sign_r64(r64 x);
static void mlib_pcm_init();
static i64 mlib_pcm_getac(i64 size);
static u64 mlib_allocmem(i64 n);
static void mlib_pcm_freeac(u64 p, i64 alloc);
static void mlib_pcm_clearmem(u64 p, i64 n);
static u64 mlib_pcm_allocz(i64 n);
static u64 mlib_pcm_copyheapstringn(u64 s, i64 n);
static u64 mlib_pcm_copyheapblock(u64 s, i64 length);
static u64 mlib_reallocmem(u64 p, i64 n);
static i64 mlib_getfilesize(u64 handlex);
static void mlib_readrandom(u64 handlex, u64 memx, i64 offset, i64 size);
static i64 mlib_writerandom(u64 handlex, u64 memx, i64 offset, i64 size);
static i64 mlib_setfilepos(u64 file, i64 offset);
static i64 mlib_getfilepos(u64 file);
static u64 mlib_readfile(u64 filename);
static i64 mlib_writefile(u64 filename, u64 data, i64 size);
static i64 mlib_checkfile(u64 file);
static u64 mwindows_os_getstdin();
static void mlib_iconvucn(u64 s, i64 n);
static u64 mlib_extractext(u64 s, i64 period);
static u64 mlib_extractfile(u64 s);
static u64 mlib_addext(u64 s, u64 newext);
static u64 mlib_pcm_alloc32();
static void mlib_pcm_free32(u64 p);
static void mlib_outbyte(u64 f, i64 x);
static void mlib_outu16(u64 f, u64 x);
static void mlib_outu32(u64 f, u64 x);
static void mlib_outu64(u64 f, u64 x);
static void mlib_outstring(u64 f, u64 s);
static void mlib_outblock(u64 f, u64 p, i64 n);
static i64 mlib_myeof(u64 f);
static void mlib_strbuffer_add(u64 dest, u64 s, i64 n);
static void mlib_gs_init(u64 dest);
static void mlib_gs_free(u64 dest);
static void mlib_gs_str(u64 dest, u64 s);
static void mlib_gs_char(u64 dest, i64 c);
static void mlib_gs_strn(u64 dest, u64 s, i64 length);
static void mlib_gs_strvar(u64 dest, u64 s);
static void mlib_gs_strint(u64 dest, i64 a);
static void mlib_gs_strln(u64 dest, u64 s);
static void mlib_gs_line(u64 dest);
static void mlib_gs_strsp(u64 dest, u64 s);
static i64 mlib_gs_getcol(u64 dest);
static void mlib_gs_leftstr(u64 dest, u64 s, i64 w, i64 padch);
static void mlib_gs_leftint(u64 dest, i64 a, i64 w, i64 padch);
static void mlib_gs_padto(u64 dest, i64 col, i64 ch);
static void mlib_gs_println(u64 dest, u64 f);
static i64 mlib_eqstring(u64 s, u64 t);
static void mlib_ipadstr(u64 s, i64 width, u64 padchar);
static i64 mlib_cmpstring(u64 s, u64 t);
static i64 mlib_cmpstringn(u64 s, u64 t, i64 n);
static i64 mlib_cmpbytes(u64 p, u64 q, i64 n);
static i64 mlib_eqbytes(u64 p, u64 q, i64 n);
static void mlib_mseed(u64 a, u64 b);
static u64 mlib_mrandom();
static i64 mlib_mrandomp();
static i64 mlib_mrandomint(i64 n);
static i64 mlib_mrandomrange(i64 a, i64 b);
static r64 mlib_mrandomreal();
static r64 mlib_mrandomreal1();
static u64 mlib_readline();
static u64 mlib_findfunction(u64 name);
static i64 mlib_roundtoblock(i64 n, i64 align);
static u64 mlib_pcm_allocnfz(i64 n);
static void mwindows_os_init();
static i64 mwindows_os_execwait(u64 cmdline, i64 newconsole, u64 workdir);
static i64 mwindows_os_execcmd(u64 cmdline, i64 newconsole);
static i64 mwindows_os_getch();
static i64 mwindows_os_getchx();
static i64 mwindows_os_kbhit();
static u64 mwindows_os_getdllinst(u64 name);
static u64 mwindows_os_getdllprocaddr(i64 hinst, u64 name);
static void mwindows_os_initwindows();
static void mwindows_os_setmesshandler(u64 addr);
static u64 mwindows_os_getos();
static i64 mwindows_os_gethostsize();
static i64 mwindows_os_shellexec(u64 opc, u64 file);
static void mwindows_os_sleep(i64 a);
static u64 mwindows_os_getstdout();
static u64 mwindows_os_getmpath();
static i64 mwindows_os_clock();
static i64 mwindows_os_hpcounter();
static i64 mwindows_os_ticks();
static i64 mwindows_os_iswindows();
static void mwindows_os_getsystime(u64 tm);
static u64 mwindows_os_allocexecmem(i64 n);
static i64 mwindows_dirlist(u64 filespec, u64 dest, i64 capacity, i64 t);
static i64 mwindows_os_hpfreq();
static u64 mwindllc_os_calldllfunction(u64 fnaddr, i64 retcode, i64 nargs, u64 args, u64 argcodes);
static i64 mwindllc_calldll_cint(u64 fnaddr, u64 params, i64 nparams);
static i64 mwindllc_calldll_creal(u64 fnaddr, u64 params, i64 nparams);
static u64 mwindllc_os_pushargs(u64 args, i64 nargs, i64 nextra, u64 fnaddr, i64 isfloat);
static void mwindllc_os_dummycall(r64 a, r64 b, r64 c, r64 d);
void runmx_main();
static void runmxshow_initlogfile();
static u64 mx_lib_loadmx(u64 filename);
static void mx_lib_fixuplib(u64 lib);
static void mx_lib_runprogram(u64 lib, i64 cmdskip);
static void runmxshow_showlibs();
static void runmxshow_closelogfile();
static void runmxshow_showglobals(u64 logdev);
static void runmxshow_showlib(u64 lib, u64 logdev);
static void runmxshow_showstrln(u64 str);
static void runmxshow_showstr(u64 str);
static void runmxshow_showsectiondata(u64 p, i64 length);
static void runmxshow_showsectioncode(u64 p, i64 length, i64 extra);
static void runmxshow_showrelocs(u64 lib);
static void runmxshow_shownames(u64 names, i64 n);
static void runmxshow_showstrint(i64 a);
static u64 mx_lib_readlibfile(u64 filespec, u64 p);
static u64 mx_lib_readu32(u64 p);
static i64 mx_lib_readbyte(u64 p);
static u64 mx_lib_readstring(u64 p);
static void mx_lib_alloclibdata(u64 lib);
static void mx_lib_error(u64 mess, u64 param);
static void mx_lib_loadmemmcu(u64 lib);
static void mx_lib_checknew(u64 name, u64 filename);
static i64 mx_lib_mxaddlib(u64 name);
static void mx_lib_loadimports(u64 plib);
static i64 mx_lib_findlib(u64 name);
static void mx_lib_loaddlls();
static void mx_lib_checksymbols();
static void mx_lib_dorelocations();
static u64 mx_lib_finddllsymbol(u64 name, u64 dllindex);
static void mx_lib_reloclib(u64 lib);
static void mx_lib_dosublib(u64 name);
static void mx_lib_dosymbols(u64 lib);
static u64 mx_lib_loadlibfile(u64 filename, i64 libno);
static u64 mx_lib_readmxfile(u64 filename);
static void mx_lib_adddll(u64 name);
static i64 mx_lib_addsymbol(u64 name);
static void mx_lib_setspecialglobals(i64 cmdskip);
static void mx_lib_calllibinit(u64 lib);
static u64 mx_lib_findsymbol(u64 name);
static u64 mx_lib_loadmemmcb(u64 filename, u64 p);
static void mc_disasm_decodeaddr(i64 w);
static void mc_disasm_getsilx(u64 reg);
static void mc_disasm_getsil(u64 reg);
static void mc_disasm_genstr(u64 s);
static i64 mc_disasm_readimm();
static void mc_disasm_genintd(i64 a);
static void mc_disasm_decodetwobyteinstr();
static i64 mc_disasm_getreg(i64 regcode, i64 upper);
static i64 mc_disasm_readint16();
static i64 mc_disasm_readint32();
static i64 mc_disasm_readsbyte();
static i64 mc_disasm_readimm8();
static i64 mc_disasm_readbyte();
static u64 mc_disasm_readword16();
static void mc_disasm_decode8087(i64 ttt);
static void mc_disasm_genhex(i64 a);
static u64 mc_disasm_readword32();
static i64 mc_disasm_readi64();
static void mc_disasm_do87arith(u64 opcstr, i64 ttt, i64 freg);
static void mc_disasm_do87mem(u64 opcstr, i64 mf);


int main(int nargs, char** args) {
    msysc_$getcommands(nargs, (u64)args, 0);
    runmx_main();
}

// **************************************************
static i64 msysc_fmtparam;
i64 msysc_$cmdskip;
static i64 msysc_needgap = 0;

static i64 msysc_outdev = 1;

static u64 msysc_outchan = 0;

static u64 msysc_fmtstr = 0;

static struct $B1 msysc_outchan_stack;
static struct $B1 msysc_outdev_stack;
static struct $B1 msysc_fmtstr_stack;
static struct $B2 msysc_needgap_stack;
static struct $B1 msysc_ptr_stack;
static i64 msysc_niostack = 0;

static struct $B3 msysc_digits = {{
3978425819141910832,
5063528411713059128}};

static struct $B3 msysc_defaultfmt = {{
112287625641984,
20992}};

static u64 msysc_rd_buffer;
static i64 msysc_rd_length;
static u64 msysc_rd_pos;
static u64 msysc_rd_lastpos;
static i64 msysc_termchar;
static i64 msysc_itemerror;
static struct $B4 msysc_printbuffer;
static u64 msysc_printptr;
static i64 msysc_printlen;
static i64 msysc_ncmdparams;
static u64 msysc_cmdparams;
static struct $B3 msysc_getfmt_fmt;
static struct $B5 msysc_strint_str;
static struct $B5 msysc_strword_str;
static struct $B6 msysc_strreal_str;
static struct $B7 mlib_allocupper;
static i64 mlib_alloccode;
static i64 mlib_allocbytes;
static i64 mlib_fdebug = 0;

static i64 mlib_rfsize;
static u64 mlib_maxmemory;
static i64 mlib_maxalloccode;
static u8 mlib_pcm_setup = 0;

static i64 mlib_show = 0;

static i64 mlib_memtotal = 0;

static i64 mlib_smallmemtotal = 0;

static i64 mlib_smallmemobjs = 0;

static i64 mlib_maxmemtotal = 0;

static struct $B8 mlib_memalloctable;
static struct $B9 mlib_memallocsize;
static u64 mlib_pcheapstart;
static u64 mlib_pcheapend;
static u64 mlib_pcheapptr;
static struct $B10 mlib_sizeindextable;
static struct $B11 mlib_freelist;
static struct $B12 mlib_pmnames = {{
(u64)"pm_end",
(u64)"pm_option",
(u64)"pm_sourcefile",
(u64)"pm_libfile",
(u64)"pm_colon",
(u64)"pm_extra"}};

static struct $B3 mlib_seed = {{
2993073034246558322,
1617678968452121188}};

static i64 mlib_pcm_newblock_totalheapsize;
// Istatic skipped:mlib.pcm_round.allocbytes

static struct $B14 mlib_changeext_newfile;
static struct $B14 mlib_extractpath_str;
static struct $B5 mlib_extractbasefile_str;
// Istatic skipped:mlib.nextcmdparamnew.infile

// Istatic skipped:mlib.nextcmdparamnew.filestart

// Istatic skipped:mlib.nextcmdparamnew.fileptr

// Istatic skipped:mlib.nextcmdparamnew.colonseen

static struct $B15 mlib_nextcmdparamnew_str;
static struct $B16 mlib_readnextfileitem_str;
static struct $B16 mlib_padstr_str;
static u64 mlib_chr_str;
static u64 mwindows_hconsole;
static u64 mwindows_hconsolein;
static struct $B17 mwindows_lastkey;
static struct $B17 mwindows_pendkey;
static i64 mwindows_keypending;
static i64 mwindows_hpfreq;
static u64 mwindows_wndproc_callbackfn = 0;

static i64 mwindows_init_flag = 0;

static u8 mwindows_os_gxregisterclass_registered;
// Istatic skipped:mwindows.mainwndproc.count

static struct $B15 mwindows_os_gethostname_name;
static i64 mwindows_os_gethostname_n;
static i64 mwindows_os_peek_lastticks;
static i64 runmxshow_logdest = 2;

static u64 runmxshow_logdev;
static struct $B3 runmxshow_destv;
static u64 runmxshow_dest = (u64)&runmxshow_destv;

struct $B18 mx_decls_mcxdirnames = {{
(u64)"pad_dir",
(u64)"version_dir",
(u64)"code_dir",
(u64)"idata_dir",
(u64)"zdata_dir",
(u64)"reloc_dir",
(u64)"dlls_dir",
(u64)"libs_dir",
(u64)"importsymbols_dir",
(u64)"exportsymbols_dir",
(u64)"exportsegs_dir",
(u64)"exportoffsets_dir",
(u64)"entry_dir",
(u64)"end_dir"}};

struct $B12 mx_decls_mcxrelocnames = {{
(u64)"no_rel",
(u64)"locabs32",
(u64)"locabs64",
(u64)"impabs32",
(u64)"impabs64",
(u64)"imprel32"}};

static struct $B19 mx_decls_dllnametable;
static struct $B19 mx_decls_dllinsttable;
static i64 mx_decls_ndlllibs;
static struct $B19 mx_decls_libnametable;
static struct $B19 mx_decls_libtable;
static struct $B17 mx_decls_librelocated;
static struct $B17 mx_decls_libinitdone;
static i64 mx_decls_nlibs;
static struct $B20 mx_decls_symbolnametable;
static struct $B21 mx_decls_symboldefined;
static struct $B20 mx_decls_symboladdress;
static struct $B22 mx_decls_symbollibindex;
static struct $B21 mx_decls_symboldllindex;
static i64 mx_decls_nsymbols;
i64 mx_decls_nsymimports = 0;

i64 mx_decls_nsymexports = 0;

static struct $B12 mx_lib_rsegmentnames = {{
(u64)"no_seg",
(u64)"code_rseg",
(u64)"idata_rseg",
(u64)"zdata_rseg",
(u64)"rodata_rseg",
(u64)"impdata_rseg"}};

static i64 mc_disasm_nmodules;
static i64 mc_disasm_xfchsmask_pd;
static struct $B23 mc_disasm_opnames = {{
(u64)"add",
(u64)"or",
(u64)"adc",
(u64)"sbb",
(u64)"and",
(u64)"sub",
(u64)"xor",
(u64)"cmp"}};

static struct $B24 mc_disasm_condnames = {{
(u64)"o",
(u64)"no",
(u64)"b",
(u64)"ae",
(u64)"z",
(u64)"nz",
(u64)"be",
(u64)"a",
(u64)"s",
(u64)"ns",
(u64)"p",
(u64)"np",
(u64)"l",
(u64)"ge",
(u64)"le",
(u64)"g"}};

static struct $B8 mc_disasm_addrmodenames = {{
(u64)"amreg",
(u64)"ammem",
(u64)"amrel"}};

static i64 mc_disasm_rex;
static i64 mc_disasm_addrmode;
static i64 mc_disasm_rmreg;
static i64 mc_disasm_rmopc;
static i64 mc_disasm_ripmode;
static i64 mc_disasm_basereg;
static i64 mc_disasm_indexreg;
static i64 mc_disasm_scale;
static i64 mc_disasm_opsize;
static i64 mc_disasm_offset;
static i64 mc_disasm_offsetsize;
static i64 mc_disasm_sizeoverride;
static i64 mc_disasm_addroverride;
static i64 mc_disasm_f2override;
static i64 mc_disasm_f3override;
static struct $B16 mc_disasm_deststr;
static u64 mc_disasm_destptr;
static u64 mc_disasm_codeptr;
static struct $B16 mc_disasm_decodeinstr_str;
// Istatic skipped:mc_disasm.strreg.regnames8

// Istatic skipped:mc_disasm.strreg.regnames16

// Istatic skipped:mc_disasm.strreg.regnames32

// Istatic skipped:mc_disasm.strreg.regnames64

// Istatic skipped:mc_disasm.strreg.mregnames8

// Istatic skipped:mc_disasm.strreg.mregnames16

// Istatic skipped:mc_disasm.strreg.mregnames32

// Istatic skipped:mc_disasm.strreg.mregnames64

// Istatic skipped:mc_disasm.strfreg.fregnames

static struct $B5 mc_disasm_printaddrmode_str;
static struct $B25 mc_disasm_strxmm_str;
static struct $B25 mc_disasm_strmmx_str;
static void msysc_$getcommands(i64 ncmd, u64 cmds, i64 cmdskipl) {
    u64 R1, R2; 
	asi64(R1) = ncmd;
	R2 = 1;
	asi64(R1) -= asi64(R2);
	msysc_ncmdparams = asi64(R1);
	asu64(R1) = cmds;
	msysc_cmdparams = asu64(R1);
	return;
}

static void msysc_pushio() {
    u64 R1, R2, R3; 
	asi64(R1) = msysc_niostack;
	R2 = 10;
	if (asi64(R1) < asi64(R2)) goto L4;
	R1 = tou64("Too many io levels\n");
	asi32(R1) = printf(asu64(R1));
	R1 = 53;
	exit(R1);
L4:
	R1 = (u64)&msysc_niostack;
	(*toi64p(R1)) += 1;
	asu64(R1) = msysc_outchan;
	R2 = (u64)&msysc_outchan_stack;
	asi64(R3) = msysc_niostack;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
	asi64(R1) = msysc_outdev;
	R2 = (u64)&msysc_outdev_stack;
	asi64(R3) = msysc_niostack;
	*toi64p(((i64)R2+(i64)R3*8-8)) = asi64(R1);
	asu64(R1) = msysc_fmtstr;
	R2 = (u64)&msysc_fmtstr_stack;
	asi64(R3) = msysc_niostack;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
	asi64(R1) = msysc_needgap;
	R2 = (u64)&msysc_needgap_stack;
	asi64(R3) = msysc_niostack;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	R1 = 0;
	msysc_needgap = asi64(R1);
	R1 = 0;
	msysc_fmtstr = asu64(R1);
	R1 = 0;
	msysc_outchan = asu64(R1);
	return;
}

static void msysc_m$print_startfile(u64 dev) {
    u64 R1; 
	msysc_pushio();
	asu64(R1) = dev;
	msysc_outchan = asu64(R1);
	asu64(R1) = dev;
	if (!asu64(R1)) goto L7;
	R1 = 2;
	msysc_outdev = asi64(R1);
	goto L6;
L7:
	R1 = 1;
	msysc_outdev = asi64(R1);
L6:
	msysc_resetprintbuffer();
	return;
}

static void msysc_m$print_startstr(u64 s) {
    u64 R1, R2, R3; 
	u64 p;
	msysc_pushio();
	asu64(R1) = s;
	R2 = (u64)&msysc_ptr_stack;
	asi64(R3) = msysc_niostack;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
	R1 = (u64)&msysc_ptr_stack;
	asi64(R2) = msysc_niostack;
	R1 += (i64)R2*8-8;
	p = asu64(R1);
	asu64(R1) = p;
	msysc_outchan = asu64(R1);
	R1 = 3;
	msysc_outdev = asi64(R1);
	return;
}

static void msysc_m$print_startptr(u64 p) {
    u64 R1; 
	msysc_pushio();
	asu64(R1) = p;
	msysc_outchan = asu64(R1);
	R1 = 3;
	msysc_outdev = asi64(R1);
	return;
}

static void msysc_m$print_startcon() {
    u64 R1; 
	msysc_pushio();
	R1 = 1;
	msysc_outdev = asi64(R1);
	msysc_resetprintbuffer();
	return;
}

static void msysc_m$print_setfmt(u64 format) {
    u64 R1; 
	asu64(R1) = format;
	msysc_fmtstr = asu64(R1);
	return;
}

static void msysc_m$print_end() {
    u64 R1, R2; 
	R1 = 0;
	msysc_needgap = asi64(R1);
	R1 = 1;
	msysc_nextfmtchars(asi64(R1));
	asi64(R1) = msysc_niostack;
	R2 = 1;
	if (asi64(R1) != asi64(R2)) goto L14;
	asi64(R1) = msysc_outdev;
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L15;
	R2 = 2;
	if (asi64(R1) != asi64(R2)) goto L14;
L15:
	msysc_dumpprintbuffer();
L14:
	asi64(R1) = msysc_niostack;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L17;
	goto L12;
L17:
	R1 = (u64)&msysc_outchan_stack;
	asi64(R2) = msysc_niostack;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	msysc_outchan = asu64(R1);
	R1 = (u64)&msysc_outdev_stack;
	asi64(R2) = msysc_niostack;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	msysc_outdev = asi64(R1);
	R1 = (u64)&msysc_fmtstr_stack;
	asi64(R2) = msysc_niostack;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	msysc_fmtstr = asu64(R1);
	R1 = (u64)&msysc_needgap_stack;
	asi64(R2) = msysc_niostack;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2-1));
	R1 = toi64(tou8(R1));
	msysc_needgap = asi64(R1);
	R1 = (u64)&msysc_niostack;
	(*toi64p(R1)) -=1;
L12:
	return;
}

static void msysc_m$print_ptr(u64 a, u64 fmtstyle) {
    u64 R1, R2; 
	asu64(R1) = fmtstyle;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L20;
	R1 = tou64("z8H");
	fmtstyle = asu64(R1);
L20:
	asu64(R1) = fmtstyle;
	asu64(R2) = a;
	msysc_m$print_u64(asu64(R2), asu64(R1));
	return;
}

static void msysc_m$print_ptr_nf(u64 a) {
    u64 R1, R2; 
	R1 = 0;
	asu64(R2) = a;
	msysc_m$print_ptr(asu64(R2), asu64(R1));
	return;
}

static void msysc_m$print_i64(i64 a, u64 fmtstyle) {
    u64 R1, R2, R3, R4; struct $B3 R1_B3; 
	struct $B29 s;
	struct $B3 fmt;
	i64 n;
	R1 = 0;
	msysc_nextfmtchars(asi64(R1));
	asu64(R1) = fmtstyle;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L24;
	asi64(R1) = a;
	R2 = 0;
	if (asi64(R1) < asi64(R2)) goto L26;
	R1 = 0;
	R2 = 10;
	R3 = (u64)&s;
	asi64(R4) = a;
	asi64(R1) = msysc_u64tostr(asu64(R4), asu64(R3), asu64(R2), asi64(R1));
	n = asi64(R1);
	goto L25;
L26:
	asi64(R1) = a;
	R2 = 0x8000000000000000;
	if (asi64(R1) != asi64(R2)) goto L27;
	(R1_B3) = msysc_defaultfmt;
	fmt = (R1_B3);
	goto L28;
	goto L25;
L27:
	R1 = 45;
	R2 = (u64)&s;
	R3 = 1;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	R1 = 0;
	R2 = 10;
	R3 = (u64)&s;
	R4 = 1;
	R3 += (i64)R4;
	asi64(R4) = a;
	asi64(R4) = -asi64(R4);
	asi64(R1) = msysc_u64tostr(asu64(R4), asu64(R3), asu64(R2), asi64(R1));
	R2 = 1;
	asi64(R1) += asi64(R2);
	n = asi64(R1);
L25:
	asi64(R1) = n;
	R2 = (u64)&s;
	msysc_printstr_n(asu64(R2), asi64(R1));
	goto L23;
L24:
	R1 = (u64)&fmt;
	R2 = -1;
	asu64(R3) = fmtstyle;
	msysc_strtofmt(asu64(R3), asi64(R2), asu64(R1));
	R1 = (u64)&fmt;
	R2 = 14;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = tou64(tou8(R1));
	R2 = 86;
	if (asu64(R1) != asu64(R2)) goto L30;
	asi64(R1) = a;
	msysc_fmtparam = asi64(R1);
	R1 = 0;
	msysc_needgap = asi64(R1);
	goto L29;
L30:
// msysc.m$print_i64.dofmt:
L28:
	R1 = (u64)&fmt;
	asi64(R2) = a;
	msysc_tostr_i64(asi64(R2), asu64(R1));
L29:
L23:
	R1 = 1;
	msysc_needgap = asi64(R1);
	return;
}

static void msysc_m$print_i64_nf(i64 a) {
    u64 R1, R2; 
	R1 = 0;
	asi64(R2) = a;
	msysc_m$print_i64(asi64(R2), asu64(R1));
	return;
}

static void msysc_m$print_bool(i64 a, u64 fmtstyle) {
    u64 R1, R2; 
	asi64(R1) = a;
	if (!asi64(R1)) goto L34;
	asu64(R1) = fmtstyle;
	R2 = tou64("True");
	msysc_m$print_str(asu64(R2), asu64(R1));
	goto L33;
L34:
	asu64(R1) = fmtstyle;
	R2 = tou64("False");
	msysc_m$print_str(asu64(R2), asu64(R1));
L33:
	return;
}

static void msysc_m$print_u64(u64 a, u64 fmtstyle) {
    u64 R1, R2, R3; 
	struct $B29 s;
	struct $B3 fmt;
	R1 = 0;
	msysc_nextfmtchars(asi64(R1));
	asu64(R1) = fmtstyle;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L37;
	asu64(R1) = a;
	R2 = tou64("%llu");
	R3 = (u64)&s;
	asi32(R1) = sprintf(asu64(R3), asu64(R2), asu64(R1));
	R1 = (u64)&s;
	msysc_printstr(asu64(R1));
	goto L36;
L37:
	R1 = (u64)&fmt;
	R2 = -1;
	asu64(R3) = fmtstyle;
	msysc_strtofmt(asu64(R3), asi64(R2), asu64(R1));
	R1 = (u64)&fmt;
	asu64(R2) = a;
	msysc_tostr_u64(asu64(R2), asu64(R1));
L36:
	R1 = 1;
	msysc_needgap = asi64(R1);
	return;
}

static void msysc_m$print_r64(r64 x, u64 fmtstyle) {
    u64 R1, R2, R3; 
	struct $B30 s;
	struct $B3 fmt;
	R1 = 0;
	msysc_nextfmtchars(asi64(R1));
	asu64(R1) = fmtstyle;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L40;
	asr64(R1) = x;
	R2 = tou64("%f");
	R3 = (u64)&s;
	asi32(R1) = sprintf(asu64(R3), asu64(R2), asr64(R1));
	R1 = (u64)&s;
	msysc_printstr(asu64(R1));
	goto L39;
L40:
	R1 = (u64)&fmt;
	R2 = -1;
	asu64(R3) = fmtstyle;
	msysc_strtofmt(asu64(R3), asi64(R2), asu64(R1));
	R1 = (u64)&fmt;
	asr64(R2) = x;
	msysc_tostr_r64(asr64(R2), asu64(R1));
L39:
	R1 = 1;
	msysc_needgap = asi64(R1);
	return;
}

static void msysc_m$print_r32(r32 x, u64 fmtstyle) {
    u64 R1, R2; 
	asu64(R1) = fmtstyle;
	asr32(R2) = x;
    asr64(R2) = tor64(asr32(R2));
	msysc_m$print_r64(asr64(R2), asu64(R1));
	return;
}

static void msysc_m$print_c8(i64 a, u64 fmtstyle) {
    u64 R1, R2, R3, R4; 
	struct $B25 s;
	struct $B3 fmt;
	i64 n;
	u8 charmode;
	R1 = 0;
	charmode = asu8(R1);
	R1 = 0;
	msysc_nextfmtchars(asi64(R1));
	asu64(R1) = fmtstyle;
	if (!asu64(R1)) goto L44;
	R1 = (u64)&fmt;
	R2 = -1;
	asu64(R3) = fmtstyle;
	msysc_strtofmt(asu64(R3), asi64(R2), asu64(R1));
	R1 = (u64)&fmt;
	R2 = 12;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	charmode = asu8(R1);
L44:
	asu8(R1) = charmode;
	R1 = toi64(tou8(R1));
	R2 = 77;
	if (asi64(R1) != asi64(R2)) goto L46;
	R1 = (u64)&fmt;
	R2 = (u64)&s;
	R3 = 8;
	R4 = (u64)&a;
	asi64(R1) = msysc_domultichar(asu64(R4), asi64(R3), asu64(R2), asu64(R1));
	n = asi64(R1);
	goto L45;
L46:
	asi64(R1) = a;
	R2 = (u64)&s;
	*toi64p(R2) = asi64(R1);
	R1 = 0;
	R2 = (u64)&s;
	R3 = 9;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	R1 = (u64)&s;
	asi64(R1) = msysc_getutfsize(asu64(R1));
	n = asi64(R1);
L45:
	asi64(R1) = n;
	R2 = (u64)&s;
	msysc_printstr_n(asu64(R2), asi64(R1));
	R1 = 1;
	msysc_needgap = asi64(R1);
	return;
}

static void msysc_m$print_str(u64 s, u64 fmtstyle) {
    u64 R1, R2, R3; 
	struct $B3 fmt;
	R1 = 0;
	msysc_nextfmtchars(asi64(R1));
	asu64(R1) = s;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L49;
	R1 = tou64("<null>");
	msysc_printstr(asu64(R1));
	goto L47;
L49:
	asu64(R1) = fmtstyle;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L51;
	asu64(R1) = s;
	msysc_printstr(asu64(R1));
	goto L50;
L51:
	R1 = (u64)&fmt;
	R2 = -1;
	asu64(R3) = fmtstyle;
	msysc_strtofmt(asu64(R3), asi64(R2), asu64(R1));
	R1 = (u64)&fmt;
	R2 = -1;
	asu64(R3) = s;
	msysc_tostr_str(asu64(R3), asi64(R2), asu64(R1));
L50:
	R1 = 1;
	msysc_needgap = asi64(R1);
L47:
	return;
}

static void msysc_m$print_strn(u64 s, i64 length, u64 fmtstyle) {
    u64 R1, R2, R3; 
	struct $B3 fmt;
	R1 = 0;
	msysc_nextfmtchars(asi64(R1));
	asu64(R1) = s;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L54;
	R1 = tou64("<null>");
	msysc_printstr(asu64(R1));
	goto L52;
L54:
	asu64(R1) = fmtstyle;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L56;
	asi64(R1) = length;
	asu64(R2) = s;
	msysc_printstr_n(asu64(R2), asi64(R1));
	goto L55;
L56:
	R1 = (u64)&fmt;
	R2 = -1;
	asu64(R3) = fmtstyle;
	msysc_strtofmt(asu64(R3), asi64(R2), asu64(R1));
	R1 = (u64)&fmt;
	asi64(R2) = length;
	asu64(R3) = s;
	msysc_tostr_str(asu64(R3), asi64(R2), asu64(R1));
L55:
	R1 = 1;
	msysc_needgap = asi64(R1);
L52:
	return;
}

static void msysc_m$print_str_nf(u64 s) {
    u64 R1, R2; 
	R1 = 0;
	asu64(R2) = s;
	msysc_m$print_str(asu64(R2), asu64(R1));
	return;
}

static void msysc_m$print_strsl(struct $B3 s, u64 fmtstyle) {
    u64 R1; 
	R1 = tou64("PRTSL");
	mlib_abortprogram(asu64(R1));
	return;
}

static void msysc_m$print_newline() {
    u64 R1; 
	R1 = 0;
	msysc_needgap = asi64(R1);
	R1 = 1;
	msysc_nextfmtchars(asi64(R1));
	R1 = tou64("\r\n");
	msysc_printstr(asu64(R1));
	return;
}

static void msysc_m$print_nogap() {
    u64 R1; 
	R1 = 0;
	msysc_needgap = asi64(R1);
	return;
}

static void msysc_m$print_space() {
    u64 R1; 
	R1 = 0;
	msysc_needgap = asi64(R1);
	R1 = tou64(" ");
	msysc_printstr(asu64(R1));
	return;
}

static void msysc_printstr(u64 s) {
    u64 R1, R2; 
	asu64(R1) = s;
	asi64(R1) = strlen(asu64(R1));
	asu64(R2) = s;
	msysc_printstr_n(asu64(R2), asi64(R1));
	return;
}

static void msysc_printstr_n(u64 s, i64 n) {
    u64 R1, R2, R3; 
	R1 = 0;
	asi64(R2) = n;
	asu64(R3) = s;
	msysc_dumpstr(asu64(R3), asi64(R2), asi64(R1));
	return;
}

static void msysc_printstrn_app(u64 s, i64 length, u64 f) {
    u64 R1, R2, R3, R4; 
	asi64(R1) = length;
	if (!asi64(R1)) goto L66;
	asu64(R1) = f;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L68;
	asu64(R1) = s;
	asi64(R2) = length;
	R3 = tou64("%.*s");
	asi32(R1) = printf(asu64(R3), asi64(R2), asu64(R1));
	goto L67;
L68:
	asu64(R1) = s;
	asi64(R2) = length;
	R3 = tou64("%.*s");
	asu64(R4) = f;
	asi32(R1) = fprintf(asu64(R4), asu64(R3), asi64(R2), asu64(R1));
L67:
L66:
	return;
}

static void msysc_printchar(i64 ch) {
    u64 R1, R2, R3; 
	u32 str;
	asi64(R1) = ch;
	R2 = (u64)&str;
	R3 = 1;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	R1 = 0;
	R2 = (u64)&str;
	R3 = 2;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	R1 = 1;
	R2 = (u64)&str;
	msysc_printstr_n(asu64(R2), asi64(R1));
	return;
}

static void msysc_nextfmtchars(i64 lastx) {
    u64 R1, R2; 
	u8 c;
	u64 pstart;
	i64 n;
	asu64(R1) = msysc_fmtstr;
	if (asu64(R1)) goto L72;
	asi64(R1) = msysc_needgap;
	if (!asi64(R1)) goto L74;
	R1 = 32;
	msysc_printchar(asi64(R1));
L74:
	R1 = 0;
	msysc_needgap = asi64(R1);
	goto L70;
L72:
	asu64(R1) = msysc_fmtstr;
	pstart = asu64(R1);
	R1 = 0;
	n = asi64(R1);
L75:
	asu64(R1) = msysc_fmtstr;
	asu8(R1) = *tou8p(R1);
	c = asu8(R1);
	asu8(R1) = c;
	R1 = tou64(tou8(R1));
	R2 = 35;
	if (asu64(R1) == asu64(R2)) goto L78;
	R2 = 0;
	if (asu64(R1) == asu64(R2)) goto L79;
	R2 = 126;
	if (asu64(R1) == asu64(R2)) goto L80;
	goto L81;
L78:
	asi64(R1) = lastx;
	if (!asi64(R1)) goto L83;
	goto L84;
L83:
	R1 = (u64)&msysc_fmtstr;
	(*tou64p(R1)) += 1;
	asi64(R1) = n;
	if (!asi64(R1)) goto L86;
	asi64(R1) = n;
	asu64(R2) = pstart;
	msysc_printstr_n(asu64(R2), asi64(R1));
L86:
	goto L70;
	goto L77;
L79:
	asi64(R1) = n;
	if (!asi64(R1)) goto L88;
	asi64(R1) = n;
	asu64(R2) = pstart;
	msysc_printstr_n(asu64(R2), asi64(R1));
	goto L87;
L88:
	asi64(R1) = lastx;
	if (asi64(R1)) goto L89;
	R1 = 1;
	R2 = tou64("|");
	msysc_printstr_n(asu64(R2), asi64(R1));
L89:
L87:
	goto L70;
	goto L77;
L80:
	asi64(R1) = n;
	if (!asi64(R1)) goto L91;
	asi64(R1) = n;
	asu64(R2) = pstart;
	msysc_printstr_n(asu64(R2), asi64(R1));
	R1 = 0;
	n = asi64(R1);
L91:
	R1 = (u64)&msysc_fmtstr;
	(*tou64p(R1)) += 1;
	asu64(R1) = msysc_fmtstr;
	asu8(R1) = *tou8p(R1);
	c = asu8(R1);
	asu8(R1) = c;
	if (!asu8(R1)) goto L93;
	R1 = (u64)&msysc_fmtstr;
	(*tou64p(R1)) += 1;
	asu8(R1) = c;
	R1 = toi64(tou8(R1));
	msysc_printchar(asi64(R1));
L93:
	asu64(R1) = msysc_fmtstr;
	pstart = asu64(R1);
	goto L77;
L81:
// msysc.nextfmtchars.skip:
L84:
	R1 = (u64)&n;
	(*toi64p(R1)) += 1;
	R1 = (u64)&msysc_fmtstr;
	(*tou64p(R1)) += 1;
L77:
	goto L75;
L70:
	return;
}

static void msysc_strtofmt(u64 s, i64 slen, u64 fmt) {
    u64 R1, R2, R3; struct $B3 R1_B3; 
	i64 c;
	i64 base;
	u8 wset;
	i64 n;
	struct $B5 str;
	(R1_B3) = msysc_defaultfmt;
	asu64(R2) = fmt;
	*(struct $B3*)(R2) = (R1_B3);
	asu64(R1) = s;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L96;
	goto L94;
L96:
	asi64(R1) = slen;
	R2 = -1;
	if (asi64(R1) != asi64(R2)) goto L98;
	asu64(R1) = s;
	asi64(R1) = strlen(asu64(R1));
	slen = asi64(R1);
L98:
	asi64(R1) = slen;
	asu64(R2) = s;
	R3 = (u64)&str;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	R1 = 0;
	R2 = (u64)&str;
	asi64(R3) = slen;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	R1 = (u64)&str;
	s = asu64(R1);
	R1 = 0;
	wset = asu8(R1);
	goto L100;
L99:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	c = asi64(R1);
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
	asi64(R1) = c;
	R2 = 65;
	if (asi64(R1) != asi64(R2)) goto L103;
	R1 = 65;
	asu64(R2) = fmt;
	R3 = 8;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L102;
L103:
	asi64(R1) = c;
	R2 = 97;
	if (asi64(R1) != asi64(R2)) goto L104;
	R1 = 97;
	asu64(R2) = fmt;
	R3 = 8;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L102;
L104:
	asi64(R1) = c;
	asi32(R1) = toupper(asi32(R1));
	R1 = toi64(toi32(R1));
	switch (asi64(R1)) {
	case 66: goto L108;
	case 67: goto L137;
	case 68: goto L136;
	case 69: goto L133;
	case 70: goto L134;
	case 71: goto L135;
	case 72: goto L109;
	case 73: case 75: case 76: case 82: case 87: goto L107;
	case 74: goto L119;
	case 77: goto L138;
	case 78: goto L141;
	case 79: goto L110;
	case 80: goto L126;
	case 81: goto L118;
	case 83: goto L123;
	case 84: goto L129;
	case 85: goto L132;
	case 86: goto L139;
	case 88: goto L111;
	case 89: goto L140;
	case 90: goto L122;
	default: goto L107;
    };
// SWITCH
L108:
	R1 = 2;
	asu64(R2) = fmt;
	R3 = 2;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L105;
L109:
	R1 = 16;
	asu64(R2) = fmt;
	R3 = 2;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L105;
L110:
	R1 = 8;
	asu64(R2) = fmt;
	R3 = 2;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L105;
L111:
	R1 = 0;
	base = asi64(R1);
L112:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	c = asi64(R1);
	asi64(R1) = c;
	R2 = 48;
	if (asi64(R1) < asi64(R2)) goto L115;
	R2 = 57;
	if (asi64(R1) > asi64(R2)) goto L115;
	asi64(R1) = base;
	R2 = 10;
	asi64(R1) *= asi64(R2);
	asi64(R2) = c;
	asi64(R1) += asi64(R2);
	R2 = 48;
	asi64(R1) -= asi64(R2);
	base = asi64(R1);
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
	goto L114;
L115:
	goto L113;
L114:
	goto L112;
L113:
	asi64(R1) = base;
	R2 = 2;
	if (asi64(R1) < asi64(R2)) goto L117;
	R2 = 16;
	if (asi64(R1) > asi64(R2)) goto L117;
	asi64(R1) = base;
	asu64(R2) = fmt;
	R3 = 2;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
L117:
	goto L105;
L118:
	R1 = 34;
	asu64(R2) = fmt;
	R3 = 3;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L105;
L119:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	asi32(R1) = toupper(asi32(R1));
	asu64(R2) = fmt;
	R3 = 9;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	if (!asu8(R1)) goto L121;
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
L121:
	goto L105;
L122:
	R1 = 48;
	asu64(R2) = fmt;
	R3 = 4;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L105;
L123:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	asu64(R2) = fmt;
	R3 = 7;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	if (!asu8(R1)) goto L125;
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
L125:
	goto L105;
L126:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	asu64(R2) = fmt;
	R3 = 4;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	if (!asu8(R1)) goto L128;
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
L128:
	goto L105;
L129:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	asu64(R2) = fmt;
	R3 = 10;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	if (!asu8(R1)) goto L131;
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
L131:
	goto L105;
L132:
	R1 = 87;
	asu64(R2) = fmt;
	R3 = 11;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L105;
L133:
	R1 = 101;
	asu64(R2) = fmt;
	R3 = 5;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L105;
L134:
	R1 = 102;
	asu64(R2) = fmt;
	R3 = 5;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L105;
L135:
	R1 = 103;
	asu64(R2) = fmt;
	R3 = 5;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L105;
L136:
	R1 = 68;
	asu64(R2) = fmt;
	R3 = 13;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L105;
L137:
	R1 = 67;
	asu64(R2) = fmt;
	R3 = 12;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L105;
L138:
	R1 = 77;
	asu64(R2) = fmt;
	R3 = 12;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L105;
L139:
	R1 = 86;
	asu64(R2) = fmt;
	R3 = 14;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L105;
L140:
	R1 = 1;
	asu64(R2) = fmt;
	R3 = 15;
	R2 += (i64)R3;
	R3 = 0;
    *toi64p(R2) = Setdotindex(*toi64p(R2), (i64)R3, (i64)R1);
	goto L105;
L141:
	R1 = 1;
	asu64(R2) = fmt;
	R3 = 15;
	R2 += (i64)R3;
	R3 = 1;
    *toi64p(R2) = Setdotindex(*toi64p(R2), (i64)R3, (i64)R1);
	goto L105;
L107:
	asi64(R1) = c;
	R2 = 46;
	if (asi64(R1) == asi64(R2)) goto L143;
	R2 = 44;
	if (asi64(R1) == asi64(R2)) goto L144;
	R2 = 95;
	if (asi64(R1) == asi64(R2)) goto L144;
	R2 = 43;
	if (asi64(R1) == asi64(R2)) goto L145;
	R2 = 126;
	if (asi64(R1) == asi64(R2)) goto L146;
	R2 = 42;
	if (asi64(R1) == asi64(R2)) goto L147;
	goto L148;
L143:
	R1 = 1;
	wset = asu8(R1);
	goto L142;
L144:
	asi64(R1) = c;
	asu64(R2) = fmt;
	R3 = 7;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L142;
L145:
	R1 = 43;
	asu64(R2) = fmt;
	R3 = 6;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L142;
L146:
	R1 = 126;
	asu64(R2) = fmt;
	R3 = 3;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L142;
L147:
	asi64(R1) = msysc_fmtparam;
	n = asi64(R1);
	goto L149;
	goto L142;
L148:
	asi64(R1) = c;
	R2 = 48;
	if (asi64(R1) < asi64(R2)) goto L151;
	asi64(R1) = c;
	R2 = 57;
	if (asi64(R1) > asi64(R2)) goto L151;
	asi64(R1) = c;
	R2 = 48;
	asi64(R1) -= asi64(R2);
	n = asi64(R1);
L152:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	c = asi64(R1);
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L155;
	goto L153;
L155:
	asi64(R1) = c;
	R2 = 48;
	if (asi64(R1) < asi64(R2)) goto L157;
	asi64(R1) = c;
	R2 = 57;
	if (asi64(R1) > asi64(R2)) goto L157;
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
	asi64(R1) = n;
	R2 = 10;
	asi64(R1) *= asi64(R2);
	asi64(R2) = c;
	asi64(R1) += asi64(R2);
	R2 = 48;
	asi64(R1) -= asi64(R2);
	n = asi64(R1);
	goto L156;
L157:
	goto L153;
L156:
	goto L152;
L153:
// msysc.strtofmt.gotwidth:
L149:
	asu8(R1) = wset;
	if (asu8(R1)) goto L159;
	asi64(R1) = n;
	asu64(R2) = fmt;
	R3 = 0;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	R1 = 1;
	wset = asu8(R1);
	goto L158;
L159:
	asi64(R1) = n;
	asu64(R2) = fmt;
	R3 = 1;
	*toi8p(((i64)R2+(i64)R3)) = asi8(R1);
L158:
L151:
L142:
L105:
L102:
L100:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	if (asu8(R1)) goto L99;
L94:
	return;
}

static i64 msysc_domultichar(u64 p, i64 n, u64 dest, u64 fmt) {
    u64 R1, R2, R3, R4; 
	struct $B17 str;
	u64 q;
	i64 nchars;
	i64 av_1;
	R1 = (u64)&str;
	q = asu64(R1);
	asi64(R1) = n;
	nchars = asi64(R1);
	asi64(R1) = n;
	av_1 = asi64(R1);
	asi64(R1) = av_1;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L163;
L161:
	asu64(R1) = p;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L165;
	goto L163;
L165:
	asu64(R1) = p;
	asu8(R1) = *tou8p(R1);
	asu64(R2) = q;
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&q;
	(*tou64p(R1)) += 1;
	R1 = (u64)&p;
	(*tou64p(R1)) += 1;
	if (--asi64(av_1)) goto L161;
L163:
	R1 = 0;
	asu64(R2) = q;
	*tou8p(R2) = asu8(R1);
	asu64(R1) = fmt;
	R2 = (u64)&str;
	asi64(R2) = strlen(asu64(R2));
	asu64(R3) = dest;
	R4 = (u64)&str;
	asi64(R1) = msysc_expandstr(asu64(R4), asu64(R3), asi64(R2), asu64(R1));
	goto L160;
L160:
	return asi64(R1);
}

static i64 msysc_expandstr(u64 s, u64 t, i64 n, u64 fmt) {
    u64 R1, R2, R3; 
	i64 i;
	i64 w;
	i64 m;
	i64 av_1;
	i64 av_2;
	i64 av_3;
	i64 av_4;
	i64 av_5;
	asu64(R1) = fmt;
	R2 = 0;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	w = asi64(R1);
	asi64(R1) = w;
	R2 = 0;
	if (asi64(R1) == asi64(R2)) goto L169;
	asi64(R1) = w;
	asi64(R2) = n;
	if (asi64(R1) > asi64(R2)) goto L168;
L169:
	asi64(R1) = n;
	asu64(R2) = s;
	asu64(R3) = t;
	asu64(R1) = strncpy(asu64(R3), asu64(R2), asu64(R1));
	R1 = 0;
	asu64(R2) = t;
	asi64(R3) = n;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	asi64(R1) = n;
	goto L166;
L168:
	asu64(R1) = fmt;
	R2 = 9;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = tou64(tou8(R1));
	R2 = 76;
	if (asu64(R1) != asu64(R2)) goto L171;
	asi64(R1) = n;
	asu64(R2) = s;
	asu64(R3) = t;
	asu64(R1) = strncpy(asu64(R3), asu64(R2), asu64(R1));
	asi64(R1) = n;
	R2 = (u64)&t;
	*tou64p(R2) += asu64(R1);
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = w;
	asi64(R2) = n;
	asi64(R1) -= asi64(R2);
	av_1 = asi64(R1);
	asi64(R1) = av_1;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L174;
L172:
	asu64(R1) = fmt;
	R2 = 4;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	asu64(R2) = t;
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&t;
	(*tou64p(R1)) += 1;
	i += 1; if (i <= av_1) goto L172;
L174:
	R1 = 0;
	asu64(R2) = t;
	*tou8p(R2) = asu8(R1);
	goto L170;
L171:
	asu64(R1) = fmt;
	R2 = 9;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = tou64(tou8(R1));
	R2 = 82;
	if (asu64(R1) != asu64(R2)) goto L175;
	asu64(R1) = fmt;
	R2 = 4;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = tou64(tou8(R1));
	R2 = 48;
	if (asu64(R1) != asu64(R2)) goto L177;
	asu64(R1) = fmt;
	R2 = 2;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	if (!asu8(R1)) goto L177;
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 45;
	if (asu64(R1) == asu64(R2)) goto L178;
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 43;
	if (asu64(R1) != asu64(R2)) goto L177;
L178:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	asu64(R2) = t;
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&t;
	(*tou64p(R1)) += 1;
	asi64(R1) = w;
	asi64(R2) = n;
	asi64(R1) -= asi64(R2);
	av_2 = asi64(R1);
	asi64(R1) = av_2;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L181;
L179:
	asu64(R1) = fmt;
	R2 = 4;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	asu64(R2) = t;
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&t;
	(*tou64p(R1)) += 1;
	if (--asi64(av_2)) goto L179;
L181:
	asi64(R1) = n;
	R2 = 1;
	asu64(R1) -= asu64(R2);
	asu64(R2) = s;
	R3 = 1;
	R2 += (i64)R3;
	asu64(R3) = t;
	asu64(R1) = strncpy(asu64(R3), asu64(R2), asu64(R1));
	R1 = 0;
	asu64(R2) = t;
	asi64(R3) = n;
	R2 += (i64)R3;
	R3 = 1;
	R2 -= (i64)R3;
	*tou8p(R2) = asu8(R1);
	goto L176;
L177:
	asi64(R1) = w;
	asi64(R2) = n;
	asi64(R1) -= asi64(R2);
	av_3 = asi64(R1);
	asi64(R1) = av_3;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L184;
L182:
	asu64(R1) = fmt;
	R2 = 4;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	asu64(R2) = t;
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&t;
	(*tou64p(R1)) += 1;
	if (--asi64(av_3)) goto L182;
L184:
	asi64(R1) = n;
	asu64(R2) = s;
	asu64(R3) = t;
	asu64(R1) = strncpy(asu64(R3), asu64(R2), asu64(R1));
	R1 = 0;
	asu64(R2) = t;
	asi64(R3) = n;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
L176:
	goto L170;
L175:
	asi64(R1) = w;
	asi64(R2) = n;
	asi64(R1) -= asi64(R2);
	R2 = 1;
	asi64(R1) += asi64(R2);
	R2 = 2;
   if (asi64(R2) == 0) {puts((u64)"Divide by zero"); exit(1);}
	asi64(R1) /= asi64(R2);
	m = asi64(R1);
	asi64(R1) = m;
	av_4 = asi64(R1);
	asi64(R1) = av_4;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L187;
L185:
	asu64(R1) = fmt;
	R2 = 4;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	asu64(R2) = t;
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&t;
	(*tou64p(R1)) += 1;
	if (--asi64(av_4)) goto L185;
L187:
	asi64(R1) = n;
	asu64(R2) = s;
	asu64(R3) = t;
	asu64(R1) = strncpy(asu64(R3), asu64(R2), asu64(R1));
	asi64(R1) = n;
	R2 = (u64)&t;
	*tou64p(R2) += asu64(R1);
	asi64(R1) = w;
	asi64(R2) = n;
	asi64(R1) -= asi64(R2);
	asi64(R2) = m;
	asi64(R1) -= asi64(R2);
	av_5 = asi64(R1);
	asi64(R1) = av_5;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L190;
L188:
	asu64(R1) = fmt;
	R2 = 4;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	asu64(R2) = t;
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&t;
	(*tou64p(R1)) += 1;
	if (--asi64(av_5)) goto L188;
L190:
	R1 = 0;
	asu64(R2) = t;
	*tou8p(R2) = asu8(R1);
L170:
	asi64(R1) = w;
	goto L166;
L166:
	return asi64(R1);
}

static i64 msysc_u64tostr(u64 aa, u64 s, u64 base, i64 sep) {
    u64 R1, R2, R3; 
	struct $B30 t;
	u64 dd;
	i64 i;
	i64 j;
	i64 k;
	i64 g;
	u64 s0;
	R1 = 0;
	i = asi64(R1);
	R1 = 0;
	k = asi64(R1);
	asu64(R1) = base;
	R2 = 10;
	if (asi64(R1) != asi64(R2)) goto L193;
	R1 = 3;
	goto L192;
L193:
	R1 = 4;
L192:
	g = asi64(R1);
L194:
	asu64(R1) = aa;
	asu64(R2) = base;
	asu64(R1) %= asu64(R2);
	dd = asu64(R1);
	asu64(R1) = aa;
	asu64(R2) = base;
   if (asu64(R2) == 0) {puts((u64)"Divide by zero"); exit(1);}
	asu64(R1) /= asu64(R2);
	aa = asu64(R1);
	R1 = (u64)&msysc_digits;
	asu64(R2) = dd;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R2 = (u64)&t;
	R3 = (u64)&i;
	asi64(R3) = *(toi64p(R3)) += 1;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	R1 = (u64)&k;
	(*toi64p(R1)) += 1;
	asi64(R1) = sep;
	if (!asi64(R1)) goto L198;
	asu64(R1) = aa;
	R2 = 0;
	if (asi64(R1) == asi64(R2)) goto L198;
	asi64(R1) = k;
	asi64(R2) = g;
	if (asi64(R1) != asi64(R2)) goto L198;
	asi64(R1) = sep;
	R2 = (u64)&t;
	R3 = (u64)&i;
	asi64(R3) = *(toi64p(R3)) += 1;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	R1 = 0;
	k = asi64(R1);
L198:
	asu64(R1) = aa;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L194;
	asi64(R1) = i;
	j = asi64(R1);
	asu64(R1) = s;
	s0 = asu64(R1);
	goto L200;
L199:
	R1 = (u64)&t;
	R2 = (u64)&i;
	asi64(R3) = *toi64p(R2); *(toi64p(R2)) -= 1; asi64(R2) = asi64(R3);
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	asu64(R2) = s;
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
L200:
	asi64(R1) = i;
	if (asi64(R1)) goto L199;
	R1 = 0;
	asu64(R2) = s;
	*tou8p(R2) = asu8(R1);
	asi64(R1) = j;
	goto L191;
L191:
	return asi64(R1);
}

static i64 msysc_i64tostrfmt(i64 aa, u64 s, u64 fmt) {
    u64 R1, R2, R3, R4; 
	struct $B30 str;
	i64 n;
	i64 usigned;
	R1 = 0;
	usigned = asi64(R1);
	asu64(R1) = fmt;
	R2 = 11;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	if (!asu8(R1)) goto L204;
	R1 = 1;
	usigned = asi64(R1);
L204:
	asi64(R1) = aa;
	R2 = 0x8000000000000000;
	if (asi64(R1) != asi64(R2)) goto L206;
	asi64(R1) = usigned;
	if (asi64(R1)) goto L206;
	R1 = 45;
	R2 = (u64)&str;
	R3 = 0;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	asu64(R1) = fmt;
	R2 = 7;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	asu64(R2) = fmt;
	R3 = 2;
	asu8(R2) = *tou8p(((i64)R2+(i64)R3));
	R2 = toi64(tou8(R2));
	R3 = (u64)&str;
	R4 = 1;
	R3 += (i64)R4;
	asi64(R1) = msysc_i64mintostr(asu64(R3), asi64(R2), asi64(R1));
	R2 = 1;
	asi64(R1) += asi64(R2);
	n = asi64(R1);
	goto L205;
L206:
	asi64(R1) = usigned;
	if (asi64(R1)) goto L210;
	asi64(R1) = aa;
	R2 = 0;
	if (asi64(R1) < asi64(R2)) goto L209;
L210:
	asu64(R1) = fmt;
	R2 = 6;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	if (!asu8(R1)) goto L208;
L209:
	asi64(R1) = aa;
	R2 = 0;
	if (asi64(R1) >= asi64(R2)) goto L212;
	asi64(R1) = aa;
	asi64(R1) = -asi64(R1);
	aa = asi64(R1);
	R1 = 45;
	R2 = (u64)&str;
	R3 = 0;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L211;
L212:
	R1 = 43;
	R2 = (u64)&str;
	R3 = 0;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
L211:
	asu64(R1) = fmt;
	R2 = 7;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	asu64(R2) = fmt;
	R3 = 2;
	asu8(R2) = *tou8p(((i64)R2+(i64)R3));
	R2 = tou64(tou8(R2));
	R3 = (u64)&str;
	R4 = 1;
	R3 += (i64)R4;
	asi64(R4) = aa;
	asi64(R1) = msysc_u64tostr(asu64(R4), asu64(R3), asu64(R2), asi64(R1));
	R2 = 1;
	asi64(R1) += asi64(R2);
	n = asi64(R1);
	goto L207;
L208:
	asu64(R1) = fmt;
	R2 = 7;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	asu64(R2) = fmt;
	R3 = 2;
	asu8(R2) = *tou8p(((i64)R2+(i64)R3));
	R2 = tou64(tou8(R2));
	R3 = (u64)&str;
	asi64(R4) = aa;
	asi64(R1) = msysc_u64tostr(asu64(R4), asu64(R3), asu64(R2), asi64(R1));
	n = asi64(R1);
L207:
L205:
	asu64(R1) = fmt;
	R2 = 10;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	if (!asu8(R1)) goto L214;
	asu64(R1) = fmt;
	R2 = 10;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R2 = (u64)&str;
	asi64(R3) = n;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	R1 = 0;
	R2 = (u64)&str;
	R3 = (u64)&n;
	asi64(R3) = *(toi64p(R3)) += 1;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
L214:
	asu64(R1) = fmt;
	R2 = 2;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	R2 = 10;
	if (asi64(R1) > asi64(R2)) goto L217;
	asu64(R1) = fmt;
	R2 = 10;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	if (!asu8(R1)) goto L216;
L217:
	asu64(R1) = fmt;
	R2 = 8;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = tou64(tou8(R1));
	R2 = 97;
	if (asu64(R1) != asu64(R2)) goto L216;
	R1 = (u64)&str;
	asu64(R1) = mlib_convlcstring(asu64(R1));
L216:
	asu64(R1) = fmt;
	asi64(R2) = n;
	asu64(R3) = s;
	R4 = (u64)&str;
	asi64(R1) = msysc_expandstr(asu64(R4), asu64(R3), asi64(R2), asu64(R1));
	goto L202;
L202:
	return asi64(R1);
}

static i64 msysc_u64tostrfmt(i64 aa, u64 s, u64 fmt) {
    u64 R1, R2, R3, R4; 
	struct $B30 str;
	i64 n;
	asu64(R1) = fmt;
	R2 = 7;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	asu64(R2) = fmt;
	R3 = 2;
	asu8(R2) = *tou8p(((i64)R2+(i64)R3));
	R2 = tou64(tou8(R2));
	R3 = (u64)&str;
	asi64(R4) = aa;
	asi64(R1) = msysc_u64tostr(asu64(R4), asu64(R3), asu64(R2), asi64(R1));
	n = asi64(R1);
	asu64(R1) = fmt;
	R2 = 10;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	if (!asu8(R1)) goto L220;
	asu64(R1) = fmt;
	R2 = 10;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R2 = (u64)&str;
	asi64(R3) = n;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	R1 = 0;
	R2 = (u64)&str;
	R3 = (u64)&n;
	asi64(R3) = *(toi64p(R3)) += 1;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
L220:
	asu64(R1) = fmt;
	R2 = 2;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	R2 = 10;
	if (asi64(R1) > asi64(R2)) goto L223;
	asu64(R1) = fmt;
	R2 = 10;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	if (!asu8(R1)) goto L222;
	asu64(R1) = fmt;
	R2 = 8;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = tou64(tou8(R1));
	R2 = 97;
	if (asu64(R1) != asu64(R2)) goto L222;
L223:
L222:
	asu64(R1) = fmt;
	asi64(R2) = n;
	asu64(R3) = s;
	R4 = (u64)&str;
	asi64(R1) = msysc_expandstr(asu64(R4), asu64(R3), asi64(R2), asu64(R1));
	goto L218;
L218:
	return asi64(R1);
}

static i64 msysc_i64mintostr(u64 s, i64 base, i64 sep) {
    u64 R1, R2, R3; 
	struct $B30 t;
	i64 i;
	i64 j;
	i64 k;
	i64 g;
	asi64(R1) = base;
	R2 = 10;
	if (asi64(R1) == asi64(R2)) goto L226;
	R2 = 16;
	if (asi64(R1) == asi64(R2)) goto L227;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L228;
	goto L229;
L226:
	R1 = tou64("9223372036854775808");
	R2 = (u64)&t;
	R3 = 0;
	R2 += (i64)R3;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
	R1 = 3;
	j = asi64(R1);
	goto L225;
L227:
	R1 = tou64("8000000000000000");
	R2 = (u64)&t;
	R3 = 0;
	R2 += (i64)R3;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
	R1 = 1;
	j = asi64(R1);
	goto L225;
L228:
	R1 = tou64("1000000000000000000000000000000000000000000000000000000000000000");
	R2 = (u64)&t;
	R3 = 0;
	R2 += (i64)R3;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
	R1 = 7;
	j = asi64(R1);
	goto L225;
L229:
	R1 = tou64("<mindint>");
	R2 = (u64)&t;
	R3 = 0;
	R2 += (i64)R3;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
L225:
	R1 = (u64)&t;
	R2 = 0;
	R1 += (i64)R2;
	asi64(R1) = strlen(asu64(R1));
	i = asi64(R1);
	asi64(R1) = i;
	R2 = (u64)&s;
	*tou64p(R2) += asu64(R1);
	asi64(R1) = sep;
	if (!asi64(R1)) goto L231;
	asi64(R1) = j;
	R2 = (u64)&s;
	*tou64p(R2) += asu64(R1);
L231:
	R1 = 0;
	asu64(R2) = s;
	*tou8p(R2) = asu8(R1);
	R1 = 0;
	k = asi64(R1);
	asi64(R1) = base;
	R2 = 10;
	if (asi64(R1) != asi64(R2)) goto L233;
	R1 = 3;
	goto L232;
L233:
	R1 = 4;
L232:
	g = asi64(R1);
	goto L235;
L234:
	R1 = (u64)&s;
	(*tou64p(R1)) -=1;
	R1 = (u64)&t;
	R2 = (u64)&i;
	asi64(R3) = *toi64p(R2); *(toi64p(R2)) -= 1; asi64(R2) = asi64(R3);
	asu8(R1) = *tou8p(((i64)R1+(i64)R2-1));
	asu64(R2) = s;
	*tou8p(R2) = asu8(R1);
	asi64(R1) = sep;
	if (!asi64(R1)) goto L238;
	asi64(R1) = i;
	if (!asi64(R1)) goto L238;
	R1 = (u64)&k;
	asi64(R1) = *(toi64p(R1)) += 1;
	asi64(R2) = g;
	if (asi64(R1) != asi64(R2)) goto L238;
	R1 = (u64)&s;
	(*tou64p(R1)) -=1;
	asi64(R1) = sep;
	asu64(R2) = s;
	*tou8p(R2) = asu8(R1);
	R1 = 0;
	k = asi64(R1);
L238:
L235:
	asi64(R1) = i;
	if (asi64(R1)) goto L234;
	asu64(R1) = s;
	asi64(R1) = strlen(asu64(R1));
	goto L224;
L224:
	return asi64(R1);
}

static i64 msysc_strtostrfmt(u64 s, u64 t, i64 n, u64 fmt) {
    u64 R1, R2, R3, R4; 
	u64 u;
	u64 v;
	struct $B16 str;
	i64 w;
	i64 nheap;
	R1 = 0;
	nheap = asi64(R1);
	asu64(R1) = fmt;
	R2 = 3;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	if (asu8(R1)) goto L242;
	asu64(R1) = fmt;
	R2 = 8;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	if (!asu8(R1)) goto L241;
L242:
	asi64(R1) = n;
	R2 = 256;
	if (asi64(R1) >= asi64(R2)) goto L244;
	R1 = (u64)&str;
	u = asu64(R1);
	goto L243;
L244:
	asi64(R1) = n;
	R2 = 3;
	asi64(R1) += asi64(R2);
	nheap = asi64(R1);
	asi64(R1) = nheap;
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	u = asu64(R1);
L243:
	asu64(R1) = fmt;
	R2 = 3;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	if (!asu8(R1)) goto L246;
	asu64(R1) = u;
	v = asu64(R1);
	asu64(R1) = fmt;
	R2 = 3;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	asu64(R2) = v;
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&v;
	(*tou64p(R1)) += 1;
	asi64(R1) = n;
	if (!asi64(R1)) goto L248;
	asu64(R1) = s;
	asu64(R2) = v;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
	asi64(R1) = n;
	R2 = (u64)&v;
	*tou64p(R2) += asu64(R1);
L248:
	asu64(R1) = fmt;
	R2 = 3;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	asu64(R2) = v;
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&v;
	(*tou64p(R1)) += 1;
	R1 = 0;
	asu64(R2) = v;
	*tou8p(R2) = asu8(R1);
	R1 = 2;
	R2 = (u64)&n;
	*toi64p(R2) += asi64(R1);
	goto L245;
L246:
	asi64(R1) = n;
	asu64(R2) = s;
	asu64(R3) = u;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
L245:
	asu64(R1) = fmt;
	R2 = 8;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = tou64(tou8(R1));
	R2 = 97;
	if (asu64(R1) == asu64(R2)) goto L250;
	R2 = 65;
	if (asu64(R1) == asu64(R2)) goto L251;
	goto L252;
L250:
	asu64(R1) = u;
	asu64(R1) = mlib_convlcstring(asu64(R1));
	goto L249;
L251:
	asu64(R1) = u;
	asu64(R1) = mlib_convucstring(asu64(R1));
	goto L249;
L252:
L249:
	asu64(R1) = u;
	s = asu64(R1);
L241:
	asu64(R1) = fmt;
	R2 = 0;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	w = asi64(R1);
	asi64(R1) = w;
	asi64(R2) = n;
	if (asi64(R1) <= asi64(R2)) goto L254;
	asu64(R1) = fmt;
	asi64(R2) = n;
	asu64(R3) = t;
	asu64(R4) = s;
	asi64(R1) = msysc_expandstr(asu64(R4), asu64(R3), asi64(R2), asu64(R1));
	n = asi64(R1);
	goto L253;
L254:
	asi64(R1) = n;
	asu64(R2) = s;
	asu64(R3) = t;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
L253:
	asi64(R1) = nheap;
	if (!asi64(R1)) goto L256;
	asi64(R1) = nheap;
	asu64(R2) = u;
	mlib_pcm_free(asu64(R2), asi64(R1));
L256:
	asi64(R1) = n;
	goto L239;
L239:
	return asi64(R1);
}

static void msysc_tostr_i64(i64 a, u64 fmt) {
    u64 R1, R2, R3, R4; 
	struct $B30 str;
	i64 n;
	asu64(R1) = fmt;
	R2 = 12;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = tou64(tou8(R1));
	R2 = 0;
	if (asu64(R1) == asu64(R2)) goto L259;
	R2 = 77;
	if (asu64(R1) == asu64(R2)) goto L260;
	goto L261;
L259:
	asu64(R1) = fmt;
	R2 = (u64)&str;
	asi64(R3) = a;
	asi64(R1) = msysc_i64tostrfmt(asi64(R3), asu64(R2), asu64(R1));
	n = asi64(R1);
	goto L258;
L260:
	asu64(R1) = fmt;
	R2 = (u64)&str;
	R3 = 8;
	R4 = (u64)&a;
	asi64(R1) = msysc_domultichar(asu64(R4), asi64(R3), asu64(R2), asu64(R1));
	n = asi64(R1);
	goto L258;
L261:
	R1 = 0;
	asi64(R2) = a;
	msysc_m$print_c8(asi64(R2), asu64(R1));
	goto L257;
L258:
	asi64(R1) = n;
	R2 = (u64)&str;
	msysc_printstr_n(asu64(R2), asi64(R1));
L257:
	return;
}

static void msysc_tostr_u64(u64 a, u64 fmt) {
    u64 R1, R2, R3, R4; 
	struct $B30 str;
	i64 n;
	asu64(R1) = fmt;
	R2 = 12;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = tou64(tou8(R1));
	R2 = 77;
	if (asu64(R1) == asu64(R2)) goto L264;
	R2 = 67;
	if (asu64(R1) == asu64(R2)) goto L265;
	goto L266;
L264:
	asu64(R1) = fmt;
	R2 = (u64)&str;
	R3 = 8;
	R4 = (u64)&a;
	asi64(R1) = msysc_domultichar(asu64(R4), asi64(R3), asu64(R2), asu64(R1));
	n = asi64(R1);
	goto L263;
L265:
	R1 = 0;
	asu64(R2) = a;
	msysc_m$print_c8(asi64(R2), asu64(R1));
	goto L262;
	goto L263;
L266:
	asu64(R1) = fmt;
	R2 = (u64)&str;
	asu64(R3) = a;
	asi64(R1) = msysc_u64tostrfmt(asi64(R3), asu64(R2), asu64(R1));
	n = asi64(R1);
L263:
	asi64(R1) = n;
	R2 = (u64)&str;
	msysc_printstr_n(asu64(R2), asi64(R1));
L262:
	return;
}

static void msysc_tostr_r64(r64 x, u64 fmt) {
    u64 R1, R2, R3, R4; 
	struct $B30 str;
	struct $B30 str2;
	struct $B2 cfmt;
	i64 n;
	R1 = 37;
	R2 = (u64)&cfmt;
	R3 = 0;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	asu64(R1) = fmt;
	R2 = 1;
	asi8(R1) = *toi8p(((i64)R1+(i64)R2));
	if (!asi8(R1)) goto L269;
	R1 = 46;
	R2 = (u64)&cfmt;
	R3 = 1;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	R1 = 42;
	R2 = (u64)&cfmt;
	R3 = 2;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	asu64(R1) = fmt;
	R2 = 5;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R2 = (u64)&cfmt;
	R3 = 3;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	R1 = 0;
	R2 = (u64)&cfmt;
	R3 = 4;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	asr64(R1) = x;
	asu64(R2) = fmt;
	R3 = 1;
	asi8(R2) = *toi8p(((i64)R2+(i64)R3));
	R2 = toi64(toi8(R2));
	R3 = (u64)&cfmt;
	R4 = (u64)&str;
	asi32(R1) = sprintf(asu64(R4), asu64(R3), asi64(R2), asr64(R1));
	goto L268;
L269:
	asu64(R1) = fmt;
	R2 = 5;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R2 = (u64)&cfmt;
	R3 = 1;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	R1 = 0;
	R2 = (u64)&cfmt;
	R3 = 2;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	asr64(R1) = x;
	R2 = (u64)&cfmt;
	R3 = (u64)&str;
	asi32(R1) = sprintf(asu64(R3), asu64(R2), asr64(R1));
L268:
	R1 = (u64)&str;
	asi64(R1) = strlen(asu64(R1));
	n = asi64(R1);
	asi64(R1) = n;
	asu64(R2) = fmt;
	R3 = 0;
	asu8(R2) = *tou8p(((i64)R2+(i64)R3));
	R2 = toi64(tou8(R2));
	if (asi64(R1) >= asi64(R2)) goto L271;
	asu64(R1) = fmt;
	asi64(R2) = n;
	R3 = (u64)&str2;
	R4 = (u64)&str;
	asi64(R1) = msysc_expandstr(asu64(R4), asu64(R3), asi64(R2), asu64(R1));
	n = asi64(R1);
	R1 = (u64)&str2;
	R2 = (u64)&str;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
L271:
	asi64(R1) = n;
	R2 = (u64)&str;
	msysc_printstr_n(asu64(R2), asi64(R1));
	return;
}

static void msysc_tostr_str(u64 s, i64 oldlen, u64 fmt) {
    u64 R1, R2, R3, R4; 
	i64 newlen;
	i64 n;
	u64 t;
	asi64(R1) = oldlen;
	R2 = -1;
	if (asi64(R1) != asi64(R2)) goto L274;
	asu64(R1) = s;
	asi64(R1) = strlen(asu64(R1));
	oldlen = asi64(R1);
L274:
	asi64(R1) = oldlen;
	newlen = asi64(R1);
	asu64(R1) = fmt;
	R2 = 3;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	if (asu8(R1)) goto L277;
	asu64(R1) = fmt;
	R2 = 0;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	asi64(R2) = newlen;
	if (asi64(R1) > asi64(R2)) goto L277;
	asu64(R1) = fmt;
	R2 = 8;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	if (asu8(R1)) goto L277;
	asu64(R1) = fmt;
	R2 = 1;
	asi8(R1) = *toi8p(((i64)R1+(i64)R2));
	if (!asi8(R1)) goto L276;
L277:
	asu64(R1) = fmt;
	R2 = 3;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	if (!asu8(R1)) goto L279;
	R1 = 2;
	R2 = (u64)&newlen;
	*toi64p(R2) += asi64(R1);
L279:
	asu64(R1) = fmt;
	R2 = 0;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	asi64(R2) = newlen;
	if (asi64(R1) <= asi64(R2)) goto L281;
	asu64(R1) = fmt;
	R2 = 0;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	newlen = asi64(R1);
L281:
	asi64(R1) = newlen;
	R2 = 1;
	asi64(R1) += asi64(R2);
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	t = asu64(R1);
	asu64(R1) = fmt;
	asi64(R2) = oldlen;
	asu64(R3) = t;
	asu64(R4) = s;
	asi64(R1) = msysc_strtostrfmt(asu64(R4), asu64(R3), asi64(R2), asu64(R1));
	n = asi64(R1);
	asu64(R1) = fmt;
	R2 = 1;
	asi8(R1) = *toi8p(((i64)R1+(i64)R2));
	if (!asi8(R1)) goto L283;
	asu64(R1) = fmt;
	R2 = 1;
	asi8(R1) = *toi8p(((i64)R1+(i64)R2));
	R1 = toi64(toi8(R1));
	R2 = (u64)&n;
    *toi64p(R2) = Min(*toi64p(R2), asi64(R1));
L283:
	asi64(R1) = n;
	asu64(R2) = t;
	msysc_printstr_n(asu64(R2), asi64(R1));
	asi64(R1) = newlen;
	R2 = 1;
	asi64(R1) += asi64(R2);
	asu64(R2) = t;
	mlib_pcm_free(asu64(R2), asi64(R1));
	goto L275;
L276:
	asi64(R1) = oldlen;
	asu64(R2) = s;
	msysc_printstr_n(asu64(R2), asi64(R1));
L275:
	return;
}

static u64 msysc_getfmt(u64 fmtstyle) {
    u64 R1, R2, R3; 
	asu64(R1) = fmtstyle;
	if (!asu64(R1)) goto L286;
	R1 = (u64)&msysc_getfmt_fmt;
	R2 = -1;
	asu64(R3) = fmtstyle;
	msysc_strtofmt(asu64(R3), asi64(R2), asu64(R1));
	R1 = (u64)&msysc_getfmt_fmt;
	goto L285;
L286:
	R1 = (u64)&msysc_defaultfmt;
L285:
	goto L284;
L284:
	return asu64(R1);
}

static u64 msysc_strint(i64 a, u64 fmtstyle) {
    u64 R1, R2; 
	u64 fmt;
	R1 = (u64)&msysc_strint_str;
	msysc_m$print_startstr(asu64(R1));
	asu64(R1) = fmtstyle;
	asu64(R1) = msysc_getfmt(asu64(R1));
	R2 = R1;
	fmt = asu64(R2);
	asi64(R2) = a;
	msysc_tostr_i64(asi64(R2), asu64(R1));
	msysc_m$print_end();
	asu64(R1) = fmt;
	R2 = (u64)&msysc_strint_str;
	asu64(R1) = msysc_getstr(asu64(R2), asu64(R1));
	goto L287;
L287:
	return asu64(R1);
}

static void msysc_getstrint(i64 a, u64 dest) {
    u64 R1, R2; 
	asu64(R1) = dest;
	msysc_m$print_startstr(asu64(R1));
	R1 = 0;
	asu64(R1) = msysc_getfmt(asu64(R1));
	asi64(R2) = a;
	msysc_tostr_i64(asi64(R2), asu64(R1));
	msysc_m$print_end();
	return;
}

static u64 msysc_strword(u64 a, u64 fmtstyle) {
    u64 R1, R2; 
	u64 fmt;
	R1 = (u64)&msysc_strword_str;
	msysc_m$print_startstr(asu64(R1));
	asu64(R1) = fmtstyle;
	asu64(R1) = msysc_getfmt(asu64(R1));
	R2 = R1;
	fmt = asu64(R2);
	asu64(R2) = a;
	msysc_tostr_u64(asu64(R2), asu64(R1));
	msysc_m$print_end();
	asu64(R1) = fmt;
	R2 = (u64)&msysc_strword_str;
	asu64(R1) = msysc_getstr(asu64(R2), asu64(R1));
	goto L289;
L289:
	return asu64(R1);
}

static u64 msysc_strreal(r64 a, u64 fmtstyle) {
    u64 R1, R2; 
	u64 fmt;
	R1 = (u64)&msysc_strreal_str;
	msysc_m$print_startstr(asu64(R1));
	asu64(R1) = fmtstyle;
	asu64(R1) = msysc_getfmt(asu64(R1));
	R2 = R1;
	fmt = asu64(R2);
	asr64(R2) = a;
	msysc_tostr_r64(asr64(R2), asu64(R1));
	msysc_m$print_end();
	asu64(R1) = fmt;
	R2 = (u64)&msysc_strreal_str;
	asu64(R1) = msysc_getstr(asu64(R2), asu64(R1));
	goto L290;
L290:
	return asu64(R1);
}

static u64 msysc_getstr(u64 s, u64 fmt) {
    u64 R1, R2; 
	asu64(R1) = fmt;
	R2 = 13;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	if (!asu8(R1)) goto L293;
	asu64(R1) = s;
	asu64(R1) = mlib_pcm_copyheapstring(asu64(R1));
	goto L292;
L293:
	asu64(R1) = s;
L292:
	goto L291;
L291:
	return asu64(R1);
}

static void msysc_initreadbuffer() {
    u64 R1, R2; 
	asu64(R1) = msysc_rd_buffer;
	if (!asu64(R1)) goto L296;
	goto L294;
L296:
	R1 = 16384;
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	msysc_rd_buffer = asu64(R1);
	R1 = 0;
	asu64(R2) = msysc_rd_buffer;
	*tou8p(R2) = asu8(R1);
	asu64(R1) = msysc_rd_buffer;
	R2 = R1;
	msysc_rd_lastpos = asu64(R2);
	msysc_rd_pos = asu64(R1);
L294:
	return;
}

static void msysc_m$read_conline() {
    u64 R1, R2, R3; 
	msysc_initreadbuffer();
	R1 = 16384;
	asu64(R2) = msysc_rd_buffer;
	R3 = 0;
	mlib_readlinen(asu64(R3), asu64(R2), asi64(R1));
	asu64(R1) = msysc_rd_buffer;
	asi64(R1) = strlen(asu64(R1));
	msysc_rd_length = asi64(R1);
	asu64(R1) = msysc_rd_buffer;
	msysc_rd_pos = asu64(R1);
	R1 = 0;
	msysc_rd_lastpos = asu64(R1);
	return;
}

static void msysc_m$read_fileline(u64 f) {
    u64 R1, R2, R3; 
	msysc_initreadbuffer();
	asu64(R1) = f;
	R2 = 1;
	if (asu64(R1) != asu64(R2)) goto L300;
	R1 = tou64("READ CMDLINE");
	mlib_abortprogram(asu64(R1));
	goto L298;
L300:
	R1 = 16384;
	asu64(R2) = msysc_rd_buffer;
	asu64(R3) = f;
	mlib_readlinen(asu64(R3), asu64(R2), asi64(R1));
	asu64(R1) = msysc_rd_buffer;
	asi64(R1) = strlen(asu64(R1));
	msysc_rd_length = asi64(R1);
	asu64(R1) = msysc_rd_buffer;
	msysc_rd_pos = asu64(R1);
	R1 = 0;
	msysc_rd_lastpos = asu64(R1);
L298:
	return;
}

static void msysc_m$read_strline(u64 s) {
    u64 R1, R2, R3; 
	i64 n;
	msysc_initreadbuffer();
	asu64(R1) = s;
	asi64(R1) = strlen(asu64(R1));
	n = asi64(R1);
	asi64(R1) = n;
	R2 = 16384;
	if (asi64(R1) >= asi64(R2)) goto L303;
	asu64(R1) = s;
	asu64(R2) = msysc_rd_buffer;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
	goto L302;
L303:
	R1 = 16383;
	asu64(R2) = s;
	asu64(R3) = msysc_rd_buffer;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	R1 = 0;
	asu64(R2) = msysc_rd_buffer;
	R3 = 16384;
	R2 += (i64)R3;
	R3 = 1;
	R2 -= (i64)R3;
	*tou8p(R2) = asu8(R1);
L302:
	asi64(R1) = n;
	msysc_rd_length = asi64(R1);
	asu64(R1) = msysc_rd_buffer;
	msysc_rd_pos = asu64(R1);
	R1 = 0;
	msysc_rd_lastpos = asu64(R1);
	return;
}

static u64 msysc_readitem(u64 itemlength) {
    u64 R1, R2; 
	u64 p;
	u64 s;
	u64 itemstr;
	u8 quotechar;
	u8 c;
	asu64(R1) = msysc_rd_buffer;
	if (asu64(R1)) goto L306;
	msysc_initreadbuffer();
L306:
	asu64(R1) = msysc_rd_pos;
	s = asu64(R1);
	goto L308;
L307:
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
L308:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 32;
	if (asu64(R1) == asu64(R2)) goto L307;
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 9;
	if (asi64(R1) == asi64(R2)) goto L307;
	asu64(R1) = s;
	itemstr = asu64(R1);
	asu64(R1) = s;
	R2 = R1;
	msysc_rd_pos = asu64(R2);
	msysc_rd_lastpos = asu64(R1);
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L311;
	R1 = 0;
	msysc_termchar = asi64(R1);
	R1 = 0;
	asu64(R2) = itemlength;
	*toi64p(R2) = asi64(R1);
	asu64(R1) = s;
	goto L304;
L311:
	R1 = 0;
	quotechar = asu8(R1);
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 34;
	if (asu64(R1) != asu64(R2)) goto L313;
	R1 = 34;
	quotechar = asu8(R1);
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
	goto L312;
L313:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 39;
	if (asu64(R1) != asu64(R2)) goto L314;
	R1 = 39;
	quotechar = asu8(R1);
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
L314:
L312:
	asu64(R1) = s;
	R2 = R1;
	itemstr = asu64(R2);
	p = asu64(R1);
	goto L316;
L315:
	R1 = (u64)&s;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	asu8(R1) = *tou8p(R1);
	c = asu8(R1);
	asu8(R1) = c;
	R1 = tou64(tou8(R1));
	R2 = 32;
	if (asu64(R1) == asu64(R2)) goto L319;
	R2 = 9;
	if (asu64(R1) == asu64(R2)) goto L319;
	R2 = 44;
	if (asu64(R1) == asu64(R2)) goto L319;
	R2 = 61;
	if (asu64(R1) == asu64(R2)) goto L319;
	goto L320;
L319:
	asu8(R1) = quotechar;
	if (asu8(R1)) goto L323;
	asu64(R1) = p;
	asu64(R2) = s;
	if (asu64(R1) != asu64(R2)) goto L322;
L323:
	goto L324;
L322:
	asu8(R1) = c;
	R1 = toi64(tou8(R1));
	msysc_termchar = asi64(R1);
	goto L317;
	goto L318;
L320:
// msysc.readitem.normalchar:
L324:
	asu8(R1) = c;
	R1 = tou64(tou8(R1));
	asu8(R2) = quotechar;
	R2 = tou64(tou8(R2));
	if (asu64(R1) != asu64(R2)) goto L326;
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	asu8(R2) = quotechar;
	R2 = tou64(tou8(R2));
	if (asu64(R1) != asu64(R2)) goto L328;
	asu8(R1) = c;
	asu64(R2) = p;
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
	R1 = (u64)&p;
	(*tou64p(R1)) += 1;
	goto L327;
L328:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	msysc_termchar = asi64(R1);
	asi64(R1) = msysc_termchar;
	R2 = 44;
	if (asi64(R1) == asi64(R2)) goto L331;
	R2 = 61;
	if (asi64(R1) != asi64(R2)) goto L330;
L331:
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	msysc_termchar = asi64(R1);
L330:
	goto L317;
L327:
	goto L325;
L326:
	asu8(R1) = c;
	asu64(R2) = p;
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&p;
	(*tou64p(R1)) += 1;
L325:
L318:
L316:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	if (asu8(R1)) goto L315;
L317:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L333;
	R1 = 0;
	msysc_termchar = asi64(R1);
L333:
	asu64(R1) = p;
	asu64(R2) = itemstr;
	asi64(R1) -= asi64(R2);
	asu64(R2) = itemlength;
	*toi64p(R2) = asi64(R1);
	asu64(R1) = s;
	msysc_rd_pos = asu64(R1);
	asu64(R1) = itemstr;
	goto L304;
L304:
	return asu64(R1);
}

static i64 msysc_strtoint(u64 s, i64 length, u64 base) {
    u64 R1, R2; 
	u8 signd;
	u64 aa;
	u64 c;
	u64 d;
	R1 = 0;
	msysc_itemerror = asi64(R1);
	asi64(R1) = length;
	R2 = -1;
	if (asi64(R1) != asi64(R2)) goto L336;
	asu64(R1) = s;
	asi64(R1) = strlen(asu64(R1));
	length = asi64(R1);
L336:
	R1 = 0;
	signd = asu8(R1);
	asi64(R1) = length;
	if (!asi64(R1)) goto L338;
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 45;
	if (asu64(R1) != asu64(R2)) goto L338;
	R1 = 1;
	signd = asu8(R1);
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
	R1 = (u64)&length;
	(*toi64p(R1)) -=1;
	goto L337;
L338:
	asi64(R1) = length;
	if (!asi64(R1)) goto L339;
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 43;
	if (asu64(R1) != asu64(R2)) goto L339;
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
	R1 = (u64)&length;
	(*toi64p(R1)) -=1;
L339:
L337:
	R1 = 0;
	aa = asu64(R1);
	goto L341;
L340:
	R1 = (u64)&s;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	c = asu64(R1);
	R1 = (u64)&length;
	(*toi64p(R1)) -=1;
	asu64(R1) = c;
	R2 = 65;
	if (asi64(R1) < asi64(R2)) goto L344;
	R2 = 70;
	if (asi64(R1) > asi64(R2)) goto L344;
	asu64(R1) = c;
	R2 = 65;
	asi64(R1) -= asi64(R2);
	R2 = 10;
	asu64(R1) += asu64(R2);
	d = asu64(R1);
	goto L343;
L344:
	asu64(R1) = c;
	R2 = 97;
	if (asi64(R1) < asi64(R2)) goto L345;
	R2 = 102;
	if (asi64(R1) > asi64(R2)) goto L345;
	asu64(R1) = c;
	R2 = 97;
	asi64(R1) -= asi64(R2);
	R2 = 10;
	asu64(R1) += asu64(R2);
	d = asu64(R1);
	goto L343;
L345:
	asu64(R1) = c;
	R2 = 48;
	if (asi64(R1) < asi64(R2)) goto L346;
	R2 = 57;
	if (asi64(R1) > asi64(R2)) goto L346;
	asu64(R1) = c;
	R2 = 48;
	asu64(R1) -= asu64(R2);
	d = asu64(R1);
	goto L343;
L346:
	asu64(R1) = c;
	R2 = 95;
	if (asi64(R1) == asi64(R2)) goto L348;
	R2 = 39;
	if (asi64(R1) != asi64(R2)) goto L347;
L348:
	goto L341;
	goto L343;
L347:
	R1 = 1;
	msysc_itemerror = asi64(R1);
	goto L342;
L343:
	asu64(R1) = d;
	asu64(R2) = base;
	if (asu64(R1) < asu64(R2)) goto L350;
	R1 = 1;
	msysc_itemerror = asi64(R1);
	goto L342;
L350:
	asu64(R1) = aa;
	asu64(R2) = base;
	asu64(R1) *= asu64(R2);
	asu64(R2) = d;
	asu64(R1) += asu64(R2);
	aa = asu64(R1);
L341:
	asi64(R1) = length;
	if (asi64(R1)) goto L340;
L342:
	asu8(R1) = signd;
	if (!asu8(R1)) goto L352;
	asu64(R1) = aa;
	asu64(R1) = -asu64(R1);
	goto L351;
L352:
	asu64(R1) = aa;
L351:
	goto L334;
L334:
	return asi64(R1);
}

static i64 msysc_m$read_i64(i64 fmt) {
    u64 R1, R2, R3; 
	u64 s;
	i64 length;
	asi64(R1) = fmt;
	asi32(R1) = toupper(asi32(R1));
	R1 = toi64(toi32(R1));
	fmt = asi64(R1);
	asi64(R1) = fmt;
	R2 = 67;
	if (asi64(R1) == asi64(R2)) goto L355;
	R2 = 84;
	if (asi64(R1) == asi64(R2)) goto L356;
	R2 = 69;
	if (asi64(R1) == asi64(R2)) goto L357;
	goto L358;
L355:
	asu64(R1) = msysc_rd_pos;
	msysc_rd_lastpos = asu64(R1);
	asu64(R1) = msysc_rd_pos;
	asu8(R1) = *tou8p(R1);
	if (!asu8(R1)) goto L360;
	R1 = (u64)&msysc_rd_pos;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	goto L353;
	goto L359;
L360:
	R1 = 0;
	goto L353;
L359:
	goto L354;
L356:
	asi64(R1) = msysc_termchar;
	goto L353;
	goto L354;
L357:
	asi64(R1) = msysc_itemerror;
	goto L353;
	goto L354;
L358:
L354:
	R1 = (u64)&length;
	asu64(R1) = msysc_readitem(asu64(R1));
	s = asu64(R1);
	asi64(R1) = fmt;
	R2 = 0;
	if (asi64(R1) == asi64(R2)) goto L362;
	R2 = 73;
	if (asi64(R1) == asi64(R2)) goto L362;
	R2 = 66;
	if (asi64(R1) == asi64(R2)) goto L363;
	R2 = 72;
	if (asi64(R1) == asi64(R2)) goto L364;
	goto L365;
L362:
	R1 = 10;
	asi64(R2) = length;
	asu64(R3) = s;
	asi64(R1) = msysc_strtoint(asu64(R3), asi64(R2), asu64(R1));
	goto L353;
	goto L361;
L363:
	R1 = 2;
	asi64(R2) = length;
	asu64(R3) = s;
	asi64(R1) = msysc_strtoint(asu64(R3), asi64(R2), asu64(R1));
	goto L353;
	goto L361;
L364:
	R1 = 16;
	asi64(R2) = length;
	asu64(R3) = s;
	asi64(R1) = msysc_strtoint(asu64(R3), asi64(R2), asu64(R1));
	goto L353;
	goto L361;
L365:
L361:
	R1 = 0;
	goto L353;
L353:
	return asi64(R1);
}

static r64 msysc_m$read_r64(i64 fmt) {
    u64 R1, R2, R3, R4; 
	struct $B31 str;
	u64 s;
	i64 length;
	i32 numlength;
	r64 x;
	R1 = (u64)&length;
	asu64(R1) = msysc_readitem(asu64(R1));
	s = asu64(R1);
	asi64(R1) = length;
	R2 = 0;
	if (asi64(R1) == asi64(R2)) goto L369;
	asi64(R1) = length;
	R2 = 512;
	if (asi64(R1) < asi64(R2)) goto L368;
L369:
	asr64(R1) = 0.000000000000000000e+000;
	goto L366;
L368:
	asi64(R1) = length;
	asu64(R2) = s;
	R3 = (u64)&str;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	R1 = 0;
	R2 = (u64)&str;
	asi64(R3) = length;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	R1 = 0;
	msysc_itemerror = asi64(R1);
	R1 = (u64)&numlength;
	R2 = (u64)&x;
	R3 = tou64("%lf%n");
	R4 = (u64)&str;
	asi32(R1) = sscanf(asu64(R4), asu64(R3), asu64(R2), asu64(R1));
	R1 = toi64(toi32(R1));
	R2 = 0;
	if (asi64(R1) == asi64(R2)) goto L372;
	asi32(R1) = numlength;
	R1 = toi64(toi32(R1));
	asi64(R2) = length;
	if (asi64(R1) == asi64(R2)) goto L371;
L372:
	asr64(R1) = 0.000000000000000000e+000;
	x = asr64(R1);
	R1 = 1;
	msysc_itemerror = asi64(R1);
L371:
	asr64(R1) = x;
	goto L366;
L366:
	return asr64(R1);
}

static void msysc_m$read_str(u64 dest, i64 destlen, i64 fmt) {
    u64 R1, R2, R3; 
	u64 s;
	i64 length;
	R1 = 0;
	msysc_itemerror = asi64(R1);
	asi64(R1) = fmt;
	R2 = 76;
	if (asi64(R1) == asi64(R2)) goto L376;
	R2 = 108;
	if (asi64(R1) != asi64(R2)) goto L375;
L376:
	asu64(R1) = msysc_rd_pos;
	s = asu64(R1);
	asu64(R1) = msysc_rd_buffer;
	asi64(R2) = msysc_rd_length;
	R1 += (i64)R2;
	asu64(R2) = msysc_rd_pos;
	asi64(R1) -= asi64(R2);
	length = asi64(R1);
	goto L374;
L375:
	R1 = (u64)&length;
	asu64(R1) = msysc_readitem(asu64(R1));
	s = asu64(R1);
	asi64(R1) = fmt;
	R2 = 78;
	if (asi64(R1) == asi64(R2)) goto L379;
	R2 = 110;
	if (asi64(R1) != asi64(R2)) goto L378;
L379:
	asi64(R1) = length;
	asu64(R2) = s;
	mlib_iconvlcn(asu64(R2), asi64(R1));
L378:
L374:
	asi64(R1) = destlen;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L381;
	asi64(R1) = length;
	asi64(R2) = destlen;
	if (asi64(R1) < asi64(R2)) goto L383;
	asi64(R1) = destlen;
	R2 = 1;
	asi64(R1) -= asi64(R2);
	length = asi64(R1);
	R1 = 1;
	msysc_itemerror = asi64(R1);
L383:
L381:
	asi64(R1) = length;
	asu64(R2) = s;
	asu64(R3) = dest;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	R1 = 0;
	asu64(R2) = dest;
	asi64(R3) = length;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	return;
}

static void msysc_readstr(u64 dest, i64 fmt, i64 destlen) {
    u64 R1, R2, R3; 
	asi64(R1) = fmt;
	asi64(R2) = destlen;
	asu64(R3) = dest;
	msysc_m$read_str(asu64(R3), asi64(R2), asi64(R1));
	return;
}

static void msysc_rereadln() {
    u64 R1; 
	asu64(R1) = msysc_rd_buffer;
	msysc_rd_pos = asu64(R1);
	asu64(R1) = msysc_rd_pos;
	msysc_rd_lastpos = asu64(R1);
	return;
}

static void msysc_reread() {
    u64 R1; 
	asu64(R1) = msysc_rd_lastpos;
	msysc_rd_pos = asu64(R1);
	return;
}

static i64 msysc_valint(u64 s, i64 fmt) {
    u64 R1; 
	u64 old_pos;
	u64 old_lastpos;
	i64 aa;
	msysc_initreadbuffer();
	asu64(R1) = msysc_rd_pos;
	old_pos = asu64(R1);
	asu64(R1) = msysc_rd_lastpos;
	old_lastpos = asu64(R1);
	asu64(R1) = s;
	msysc_rd_pos = asu64(R1);
	asi64(R1) = fmt;
	asi64(R1) = msysc_m$read_i64(asi64(R1));
	aa = asi64(R1);
	asu64(R1) = old_pos;
	msysc_rd_pos = asu64(R1);
	asu64(R1) = old_lastpos;
	msysc_rd_lastpos = asu64(R1);
	asi64(R1) = aa;
	goto L387;
L387:
	return asi64(R1);
}

static r64 msysc_valreal(u64 s) {
    u64 R1; 
	u64 old_pos;
	u64 old_lastpos;
	r64 x;
	msysc_initreadbuffer();
	asu64(R1) = msysc_rd_pos;
	old_pos = asu64(R1);
	asu64(R1) = msysc_rd_lastpos;
	old_lastpos = asu64(R1);
	asu64(R1) = s;
	msysc_rd_pos = asu64(R1);
	R1 = 0;
	asr64(R1) = msysc_m$read_r64(asi64(R1));
	x = asr64(R1);
	asu64(R1) = old_pos;
	msysc_rd_pos = asu64(R1);
	asu64(R1) = old_lastpos;
	msysc_rd_lastpos = asu64(R1);
	asr64(R1) = x;
	goto L388;
L388:
	return asr64(R1);
}

static void msysc_mclunimpl(u64 mess) {
    u64 R1, R2; 
	asu64(R1) = mess;
	R2 = tou64("MCL-UNIMPL: %s\n");
	asi32(R1) = printf(asu64(R2), asu64(R1));
	R1 = 1;
	exit(R1);
	return;
}

static void msysc_dumpstr(u64 s, i64 n, i64 fbuffer) {
    u64 R1, R2, R3, R4; 
	u64 p;
	asi64(R1) = msysc_outdev;
	R2 = 3;
	if (asi64(R1) != asi64(R2)) goto L392;
	asu64(R1) = msysc_outchan;
	p = asu64(R1);
	asi64(R1) = n;
	if (!asi64(R1)) goto L394;
	asi64(R1) = n;
	asu64(R2) = s;
	asu64(R3) = p;
	asu64(R3) = *tou64p(R3);
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	asi64(R1) = n;
	asu64(R2) = p;
	*tou64p(R2) += asu64(R1);
L394:
	R1 = 0;
	asu64(R2) = p;
	asu64(R2) = *tou64p(R2);
	*tou8p(R2) = asu8(R1);
	goto L390;
L392:
	asi64(R1) = n;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L396;
	goto L390;
L396:
	asi64(R1) = fbuffer;
	if (!asi64(R1)) goto L398;
	asi64(R1) = n;
	R2 = 2;
	if (asi64(R1) < asi64(R2)) goto L398;
	asi64(R1) = msysc_outdev;
	R2 = 1;
	if (asi64(R1) != asi64(R2)) goto L398;
	R1 = (u64)&msysc_printptr;
	(*tou64p(R1)) -=1;
	asu64(R1) = msysc_printptr;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 10;
	if (asi64(R1) != asi64(R2)) goto L400;
	asu64(R1) = msysc_printptr;
	R2 = 1;
	R1 -= (i64)R2;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 13;
	if (asi64(R1) != asi64(R2)) goto L402;
	R1 = 0;
	asu64(R2) = msysc_printptr;
	R3 = 1;
	R2 -= (i64)R3;
	*tou8p(R2) = asu8(R1);
	goto L401;
L402:
	R1 = 0;
	asu64(R2) = msysc_printptr;
	*tou8p(R2) = asu8(R1);
L401:
	R1 = (u64)&msysc_printbuffer;
	asi32(R1) = puts(asu64(R1));
	goto L390;
L400:
L398:
	asi64(R1) = msysc_outdev;
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L404;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L405;
	goto L406;
L404:
	asu64(R1) = s;
	asi64(R2) = n;
	R3 = tou64("%.*s");
	asi32(R1) = printf(asu64(R3), asi64(R2), asu64(R1));
	goto L403;
L405:
	asu64(R1) = s;
	asi64(R2) = n;
	R3 = tou64("%.*s");
	asu64(R4) = msysc_outchan;
	asi32(R1) = fprintf(asu64(R4), asu64(R3), asi64(R2), asu64(R1));
	goto L403;
L406:
L403:
L390:
	return;
}

static void msysc_dumpprintbuffer() {
    u64 R1, R2, R3; 
	asi64(R1) = msysc_printlen;
	if (!asi64(R1)) goto L409;
	R1 = 1;
	asi64(R2) = msysc_printlen;
	R3 = (u64)&msysc_printbuffer;
	msysc_dumpstr(asu64(R3), asi64(R2), asi64(R1));
L409:
	msysc_resetprintbuffer();
	return;
}

static void msysc_resetprintbuffer() {
    u64 R1; 
	R1 = (u64)&msysc_printbuffer;
	msysc_printptr = asu64(R1);
	R1 = 0;
	msysc_printlen = asi64(R1);
	return;
}

static void msysc_addtobuffer(u64 s, i64 n) {
    u64 R1, R2, R3; 
	asi64(R1) = msysc_printlen;
	asi64(R2) = n;
	asi64(R1) += asi64(R2);
	R2 = 4088;
	if (asi64(R1) < asi64(R2)) goto L413;
	msysc_dumpprintbuffer();
L413:
	asi64(R1) = n;
	R2 = 4096;
	if (asi64(R1) >= asi64(R2)) goto L415;
	asi64(R1) = n;
	asu64(R2) = s;
	asu64(R3) = msysc_printptr;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	asi64(R1) = n;
	R2 = (u64)&msysc_printptr;
	*tou64p(R2) += asu64(R1);
	asi64(R1) = n;
	R2 = (u64)&msysc_printlen;
	*toi64p(R2) += asi64(R1);
	goto L411;
L415:
	R1 = 0;
	asi64(R2) = n;
	asu64(R3) = s;
	msysc_dumpstr(asu64(R3), asi64(R2), asi64(R1));
L411:
	return;
}

static i64 msysc_getutfsize(u64 s) {
    u64 R1, R2, R3; 
	i64 a;
	R1 = (u64)&s;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	a = asi64(R1);
	asi64(R1) = a;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L418;
	R1 = 0;
	goto L417;
L418:
	asi64(R1) = a;
	R2 = 7;
    asi64(R1) = Getdotindex(asu64(R1), asi64(R2));
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L419;
	R1 = 1;
	goto L417;
L419:
	asi64(R1) = a;
	R2 = 5;
	R3 = 7;
    asi64(R1) = Getdotslice((u64)R1, (i64)R2, (i64)R3);
	R2 = 6;
	if (asi64(R1) != asi64(R2)) goto L420;
	R1 = 2;
	goto L417;
L420:
	asi64(R1) = a;
	R2 = 4;
	R3 = 7;
    asi64(R1) = Getdotslice((u64)R1, (i64)R2, (i64)R3);
	R2 = 14;
	if (asi64(R1) != asi64(R2)) goto L421;
	R1 = 3;
	goto L417;
L421:
	asi64(R1) = a;
	R2 = 3;
	R3 = 7;
    asi64(R1) = Getdotslice((u64)R1, (i64)R2, (i64)R3);
	R2 = 30;
	if (asi64(R1) != asi64(R2)) goto L422;
	R1 = 4;
	goto L417;
L422:
	R1 = 1;
L417:
	goto L416;
L416:
	return asi64(R1);
}

static i64 msysc_m$sign_i64(i64 a) {
    u64 R1, R2; 
	asi64(R1) = a;
	R2 = 0;
	if (asi64(R1) >= asi64(R2)) goto L425;
	R1 = -1;
	goto L424;
L425:
	asi64(R1) = a;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L427;
	R1 = 1;
	goto L426;
L427:
	R1 = 0;
L426:
L424:
	goto L423;
L423:
	return asi64(R1);
}

static r64 msysc_m$sign_r64(r64 x) {
    u64 R1, R2; 
	asr64(R1) = x;
	asr64(R2) = 0.000000000000000000e+000;
	if (asr64(R1) >= asr64(R2)) goto L430;
	asr64(R1) = -1.000000000000000000e+000;
	goto L428;
L430:
	asr64(R1) = x;
	asr64(R2) = 0.000000000000000000e+000;
	if (asr64(R1) <= asr64(R2)) goto L432;
	asr64(R1) = 1.000000000000000000e+000;
	goto L428;
L432:
	asr64(R1) = 0.000000000000000000e+000;
	goto L428;
L428:
	return asr64(R1);
}

static u64 mlib_pcm_alloc(i64 n) {
    u64 R1, R2, R3; 
	u64 p;
	asu8(R1) = mlib_pcm_setup;
	if (asu8(R1)) goto L435;
	mlib_pcm_init();
L435:
	asi64(R1) = n;
	R2 = 2048;
	if (asi64(R1) <= asi64(R2)) goto L437;
	asi64(R1) = n;
	asi64(R1) = mlib_pcm_getac(asi64(R1));
	mlib_alloccode = asi64(R1);
	R1 = (u64)&mlib_allocupper;
	asi64(R2) = mlib_alloccode;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8));
	mlib_allocbytes = asi64(R1);
	asi64(R1) = mlib_allocbytes;
	asu64(R1) = mlib_allocmem(asi64(R1));
	p = asu64(R1);
	asu64(R1) = p;
	if (asu64(R1)) goto L439;
	R1 = tou64("pcm_alloc failure");
	mlib_abortprogram(asu64(R1));
L439:
	asu64(R1) = p;
	goto L433;
L437:
	R1 = (u64)&mlib_sizeindextable;
	asi64(R2) = n;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	mlib_alloccode = asi64(R1);
	R1 = (u64)&mlib_allocupper;
	asi64(R2) = mlib_alloccode;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8));
	mlib_allocbytes = asi64(R1);
	R1 = (u64)&mlib_freelist;
	asi64(R2) = mlib_alloccode;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	R2 = R1;
	p = asu64(R2);
	if (!asu64(R1)) goto L441;
	R1 = (u64)&mlib_freelist;
	asi64(R2) = mlib_alloccode;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	asu64(R1) = *tou64p(R1);
	R2 = (u64)&mlib_freelist;
	asi64(R3) = mlib_alloccode;
	*tou64p(((i64)R2+(i64)R3*8)) = asu64(R1);
	asu64(R1) = p;
	goto L433;
L441:
	asu64(R1) = mlib_pcheapptr;
	p = asu64(R1);
	asi64(R1) = mlib_allocbytes;
	R2 = (u64)&mlib_pcheapptr;
	*tou64p(R2) += asu64(R1);
	asu64(R1) = mlib_pcheapptr;
	asu64(R2) = mlib_pcheapend;
	if (asu64(R1) < asu64(R2)) goto L443;
	asi64(R1) = mlib_allocbytes;
	asu64(R1) = mlib_pcm_newblock(asi64(R1));
	p = asu64(R1);
	asu64(R1) = p;
	goto L433;
L443:
	asu64(R1) = p;
	goto L433;
L433:
	return asu64(R1);
}

static void mlib_pcm_free(u64 p, i64 n) {
    u64 R1, R2, R3; 
	i64 acode;
	asi64(R1) = n;
	R2 = 0;
	if (asi64(R1) == asi64(R2)) goto L447;
	asu64(R1) = p;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L446;
L447:
	goto L444;
L446:
	asi64(R1) = n;
	R2 = 2048;
	if (asi64(R1) <= asi64(R2)) goto L449;
	asu64(R1) = p;
	free(asu64(R1));
	goto L448;
L449:
	R1 = (u64)&mlib_sizeindextable;
	asi64(R2) = n;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	acode = asi64(R1);
	R1 = (u64)&mlib_freelist;
	asi64(R2) = acode;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	asu64(R2) = p;
	*tou64p(R2) = asu64(R1);
	asu64(R1) = p;
	R2 = (u64)&mlib_freelist;
	asi64(R3) = acode;
	*tou64p(((i64)R2+(i64)R3*8)) = asu64(R1);
L448:
L444:
	return;
}

static void mlib_pcm_freeac(u64 p, i64 alloc) {
    u64 R1, R2; 
	R1 = (u64)&mlib_allocupper;
	asi64(R2) = alloc;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8));
	asu64(R2) = p;
	mlib_pcm_free(asu64(R2), asi64(R1));
	return;
}

static void mlib_pcm_clearmem(u64 p, i64 n) {
    u64 R1, R2, R3; 
	asi64(R1) = n;
	R2 = 0;
	asu64(R3) = p;
	memset(asu64(R3), asi32(R2), asu64(R1));
	return;
}

static void mlib_pcm_init() {
    u64 R1, R2, R3; 
	i64 j;
	i64 k;
	i64 size;
	i64 av_1;
	i64 i;
	R1 = 0;
	mlib_alloccode = asi64(R1);
	asu8(R1) = mlib_pcm_setup;
	if (!asu8(R1)) goto L454;
	goto L452;
L454:
	R1 = 0;
	asu64(R1) = mlib_pcm_newblock(asi64(R1));
	R1 = 1;
	i = asi64(R1);
L455:
	R1 = 1;
	j = asi64(R1);
	R1 = 16;
	k = asi64(R1);
	goto L459;
L458:
	asi64(R1) = k;
	R2 = 1;
	asi64(R1) <<= asi64(R2);
	k = asi64(R1);
	R1 = (u64)&j;
	(*toi64p(R1)) += 1;
L459:
	asi64(R1) = i;
	asi64(R2) = k;
	if (asi64(R1) > asi64(R2)) goto L458;
	asi64(R1) = j;
	R2 = (u64)&mlib_sizeindextable;
	asi64(R3) = i;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	i += 1; if (i <= 2048) goto L455;
	R1 = 16;
	R2 = (u64)&mlib_allocupper;
	R3 = 1;
	*tou64p(((i64)R2+(i64)R3*8)) = asu64(R1);
	R1 = 16;
	size = asi64(R1);
	R1 = 2;
	i = asi64(R1);
L461:
	R1 = 2;
	R2 = (u64)&size;
	*toi64p(R2) *= asi64(R1);
	asi64(R1) = size;
	R2 = (u64)&mlib_allocupper;
	asi64(R3) = i;
	*tou64p(((i64)R2+(i64)R3*8)) = asu64(R1);
	asi64(R1) = size;
	R2 = 33554432;
	if (asi64(R1) < asi64(R2)) goto L465;
	asi64(R1) = i;
	k = asi64(R1);
	goto L463;
L465:
	i += 1; if (i <= 27) goto L461;
L463:
	asi64(R1) = k;
	R2 = 1;
	asi64(R1) += asi64(R2);
	i = asi64(R1);
	R1 = 300;
	av_1 = asi64(R1);
	asi64(R1) = i;
	asi64(R2) = av_1;
	if (asi64(R1) > asi64(R2)) goto L468;
L466:
	R1 = 33554432;
	R2 = (u64)&size;
	*toi64p(R2) += asi64(R1);
	asi64(R1) = size;
	R2 = 8589934592;
	if (asi64(R1) >= asi64(R2)) goto L470;
	asi64(R1) = size;
	R2 = (u64)&mlib_allocupper;
	asi64(R3) = i;
	*tou64p(((i64)R2+(i64)R3*8)) = asu64(R1);
	asi64(R1) = size;
	mlib_maxmemory = asu64(R1);
	goto L469;
L470:
	asi64(R1) = i;
	R2 = 1;
	asi64(R1) -= asi64(R2);
	mlib_maxalloccode = asi64(R1);
	goto L468;
L469:
	i += 1; if (i <= av_1) goto L466;
L468:
	R1 = 1;
	mlib_pcm_setup = asu8(R1);
L452:
	return;
}

static i64 mlib_pcm_getac(i64 size) {
    u64 R1, R2; 
	asi64(R1) = size;
	R2 = 2048;
	if (asi64(R1) > asi64(R2)) goto L473;
	R1 = (u64)&mlib_sizeindextable;
	asi64(R2) = size;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	goto L471;
L473:
	asi64(R1) = size;
	R2 = 255;
	asi64(R1) += asi64(R2);
	R2 = 8;
	asi64(R1) >>= asi64(R2);
	size = asi64(R1);
	asi64(R1) = size;
	R2 = 2048;
	if (asi64(R1) > asi64(R2)) goto L475;
	R1 = (u64)&mlib_sizeindextable;
	asi64(R2) = size;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	R2 = 8;
	asi64(R1) += asi64(R2);
	goto L471;
L475:
	asi64(R1) = size;
	R2 = 63;
	asi64(R1) += asi64(R2);
	R2 = 6;
	asi64(R1) >>= asi64(R2);
	size = asi64(R1);
	asi64(R1) = size;
	R2 = 2048;
	if (asi64(R1) > asi64(R2)) goto L477;
	R1 = (u64)&mlib_sizeindextable;
	asi64(R2) = size;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	R2 = 14;
	asi64(R1) += asi64(R2);
	goto L471;
L477:
	asi64(R1) = size;
	R2 = 2048;
	asi64(R1) -= asi64(R2);
	R2 = 2047;
	asi64(R1) += asi64(R2);
	R2 = 2048;
   if (asi64(R2) == 0) {puts((u64)"Divide by zero"); exit(1);}
	asi64(R1) /= asi64(R2);
	R2 = 22;
	asi64(R1) += asi64(R2);
	size = asi64(R1);
	asi64(R1) = size;
	goto L471;
L471:
	return asi64(R1);
}

static u64 mlib_pcm_newblock(i64 itemsize) {
    u64 R1, R2, R3; 
	u64 p;
	R1 = 2097152;
	R2 = (u64)&mlib_pcm_newblock_totalheapsize;
	*toi64p(R2) += asi64(R1);
	R1 = 0;
	mlib_alloccode = asi64(R1);
	R1 = 2097152;
	asu64(R1) = mlib_allocmem(asi64(R1));
	p = asu64(R1);
	asu64(R1) = p;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L480;
	R1 = tou64("Can't alloc pc heap");
	mlib_abortprogram(asu64(R1));
L480:
	R1 = 2097152;
	R2 = 0;
	asu64(R3) = p;
	memset(asu64(R3), asi32(R2), asu64(R1));
	asu64(R1) = p;
	mlib_pcheapptr = asu64(R1);
	asu64(R1) = p;
	R2 = 2097152;
	R1 += (i64)R2;
	mlib_pcheapend = asu64(R1);
	asu64(R1) = mlib_pcheapstart;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L482;
	asu64(R1) = p;
	mlib_pcheapstart = asu64(R1);
L482:
	asi64(R1) = itemsize;
	R2 = (u64)&mlib_pcheapptr;
	*tou64p(R2) += asu64(R1);
	asu64(R1) = p;
	goto L478;
L478:
	return asu64(R1);
}

static i64 mlib_pcm_round(i64 n) {
    u64 R1, R2, R3; 
// PROC LOCAL STATICS GO HERE
	static struct $B13 mlib_pcm_round_allocbytes = {{
0,
	16,
	32,
	64,
	128,
	256,
	512,
	1024,
	2048    }};
	asi64(R1) = n;
	R2 = 2048;
	if (asi64(R1) <= asi64(R2)) goto L485;
	asi64(R1) = n;
	goto L484;
L485:
	R1 = (u64)&mlib_pcm_round_allocbytes;
	R2 = (u64)&mlib_sizeindextable;
	asi64(R3) = n;
	asu8(R2) = *tou8p(((i64)R2+(i64)R3));
	R2 = toi64(tou8(R2));
	asi32(R1) = *toi32p(((i64)R1+(i64)R2*4));
	R1 = toi64(toi32(R1));
L484:
	goto L483;
L483:
	return asi64(R1);
}

static u64 mlib_pcm_allocz(i64 n) {
    u64 R1, R2, R3; 
	u64 p;
	asi64(R1) = n;
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	p = asu64(R1);
	asi64(R1) = n;
	R2 = 0;
	asu64(R3) = p;
	memset(asu64(R3), asi32(R2), asu64(R1));
	asu64(R1) = p;
	goto L486;
L486:
	return asu64(R1);
}

static u64 mlib_pcm_copyheapstring(u64 s) {
    u64 R1, R2, R3; 
	u64 q;
	i64 n;
	asu64(R1) = s;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L489;
	R1 = 0;
	goto L487;
L489:
	asu64(R1) = s;
	asi64(R1) = strlen(asu64(R1));
	R2 = 1;
	asi64(R1) += asi64(R2);
	n = asi64(R1);
	asi64(R1) = n;
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	q = asu64(R1);
	asi64(R1) = n;
	asu64(R2) = s;
	asu64(R3) = q;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	asu64(R1) = q;
	goto L487;
L487:
	return asu64(R1);
}

static u64 mlib_pcm_copyheapstringn(u64 s, i64 n) {
    u64 R1, R2, R3; 
	u64 q;
	asu64(R1) = s;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L492;
	R1 = 0;
	goto L490;
L492:
	asi64(R1) = n;
	R2 = 1;
	asi64(R1) += asi64(R2);
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	q = asu64(R1);
	asi64(R1) = n;
	asu64(R2) = s;
	asu64(R3) = q;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	R1 = 0;
	asu64(R2) = q;
	asi64(R3) = n;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	asu64(R1) = q;
	goto L490;
L490:
	return asu64(R1);
}

static u64 mlib_pcm_copyheapblock(u64 s, i64 length) {
    u64 R1, R2, R3; 
	u64 q;
	asi64(R1) = length;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L495;
	R1 = 0;
	goto L493;
L495:
	asi64(R1) = length;
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	q = asu64(R1);
	asi64(R1) = length;
	asu64(R2) = s;
	asu64(R3) = q;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	asu64(R1) = q;
	goto L493;
L493:
	return asu64(R1);
}

static u64 mlib_allocmem(i64 n) {
    u64 R1; 
	u64 p;
	asi64(R1) = n;
	asu64(R1) = malloc(asu64(R1));
	p = asu64(R1);
	asu64(R1) = p;
	if (!asu64(R1)) goto L498;
	asu64(R1) = p;
	goto L496;
L498:
	msysc_m$print_startcon();
	asi64(R1) = n;
	msysc_m$print_i64_nf(asi64(R1));
	asi64(R1) = mlib_memtotal;
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	R1 = tou64("Alloc mem failure");
	mlib_abortprogram(asu64(R1));
	R1 = 0;
	goto L496;
L496:
	return asu64(R1);
}

static u64 mlib_reallocmem(u64 p, i64 n) {
    u64 R1, R2; 
	asi64(R1) = n;
	asu64(R2) = p;
	asu64(R1) = realloc(asu64(R2), asu64(R1));
	p = asu64(R1);
	asu64(R1) = p;
	if (!asu64(R1)) goto L501;
	asu64(R1) = p;
	goto L499;
L501:
	msysc_m$print_startcon();
	asi64(R1) = n;
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	R1 = tou64("Realloc mem failure");
	mlib_abortprogram(asu64(R1));
	R1 = 0;
	goto L499;
L499:
	return asu64(R1);
}

static void mlib_abortprogram(u64 s) {
    u64 R1; 
	msysc_m$print_startcon();
	asu64(R1) = s;
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	msysc_m$print_startcon();
	R1 = tou64("ABORTING: Press key...");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_end();
	R1 = 5;
	exit(R1);
	return;
}

static i64 mlib_getfilesize(u64 handlex) {
    u64 R1, R2, R3; 
	u32 p;
	u32 size;
	asu64(R1) = handlex;
	asi32(R1) = ftell(asu64(R1));
	p = asu32(R1);
	R1 = 2;
	R2 = 0;
	asu64(R3) = handlex;
	asi32(R1) = fseek(asu64(R3), asi32(R2), asi32(R1));
	asu64(R1) = handlex;
	asi32(R1) = ftell(asu64(R1));
	size = asu32(R1);
	R1 = 0;
	asu32(R2) = p;
	R2 = toi64(tou32(R2));
	asu64(R3) = handlex;
	asi32(R1) = fseek(asu64(R3), asi32(R2), asi32(R1));
	asu32(R1) = size;
	R1 = toi64(tou32(R1));
	goto L503;
L503:
	return asi64(R1);
}

static void mlib_readrandom(u64 handlex, u64 memx, i64 offset, i64 size) {
    u64 R1, R2, R3, R4; 
	i64 a;
	R1 = 0;
	asi64(R2) = offset;
	asu64(R3) = handlex;
	asi32(R1) = fseek(asu64(R3), asi32(R2), asi32(R1));
	asu64(R1) = handlex;
	asi64(R2) = size;
	R3 = 1;
	asu64(R4) = memx;
	asi64(R1) = fread(asu64(R4), asu64(R3), asu64(R2), asu64(R1));
	a = asi64(R1);
	return;
}

static i64 mlib_writerandom(u64 handlex, u64 memx, i64 offset, i64 size) {
    u64 R1, R2, R3, R4; 
	R1 = 0;
	asi64(R2) = offset;
	asu64(R3) = handlex;
	asi32(R1) = fseek(asu64(R3), asi32(R2), asi32(R1));
	asu64(R1) = handlex;
	asi64(R2) = size;
	R3 = 1;
	asu64(R4) = memx;
	asi64(R1) = fwrite(asu64(R4), asu64(R3), asu64(R2), asu64(R1));
	goto L505;
L505:
	return asi64(R1);
}

static i64 mlib_setfilepos(u64 file, i64 offset) {
    u64 R1, R2, R3; 
	R1 = 0;
	asi64(R2) = offset;
	asu64(R3) = file;
	asi32(R1) = fseek(asu64(R3), asi32(R2), asi32(R1));
	R1 = toi64(toi32(R1));
	goto L506;
L506:
	return asi64(R1);
}

static i64 mlib_getfilepos(u64 file) {
    u64 R1; 
	asu64(R1) = file;
	asi32(R1) = ftell(asu64(R1));
	R1 = toi64(toi32(R1));
	goto L507;
L507:
	return asi64(R1);
}

static u64 mlib_readfile(u64 filename) {
    u64 R1, R2, R3, R4; 
	u64 f;
	i64 size;
	u64 m;
	u64 p;
	R1 = tou64("rb");
	asu64(R2) = filename;
	asu64(R1) = fopen(asu64(R2), asu64(R1));
	f = asu64(R1);
	asu64(R1) = f;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L510;
	R1 = 0;
	goto L508;
L510:
	asu64(R1) = f;
	asi64(R1) = mlib_getfilesize(asu64(R1));
	R2 = R1;
	size = asi64(R2);
	mlib_rfsize = asi64(R1);
	asi64(R1) = size;
	R2 = 2;
	asu64(R1) += asu64(R2);
	asu64(R1) = malloc(asu64(R1));
	m = asu64(R1);
	asu64(R1) = m;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L512;
	R1 = 0;
	goto L508;
L512:
	asi64(R1) = size;
	R2 = 0;
	asu64(R3) = m;
	asu64(R4) = f;
	mlib_readrandom(asu64(R4), asu64(R3), asi64(R2), asi64(R1));
	asu64(R1) = m;
	asi64(R2) = size;
	R1 += (i64)R2;
	p = asu64(R1);
	R1 = 0;
	asu64(R2) = p;
	*tou16p(R2) = asu16(R1);
	asu64(R1) = f;
	asi32(R1) = fclose(asu64(R1));
	asu64(R1) = m;
	goto L508;
L508:
	return asu64(R1);
}

static i64 mlib_writefile(u64 filename, u64 data, i64 size) {
    u64 R1, R2, R3, R4; 
	u64 f;
	i64 n;
	R1 = tou64("wb");
	asu64(R2) = filename;
	asu64(R1) = fopen(asu64(R2), asu64(R1));
	f = asu64(R1);
	asu64(R1) = f;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L515;
	R1 = 0;
	goto L513;
L515:
	asi64(R1) = size;
	R2 = 0;
	asu64(R3) = data;
	asu64(R4) = f;
	asi64(R1) = mlib_writerandom(asu64(R4), asu64(R3), asi64(R2), asi64(R1));
	n = asi64(R1);
	asu64(R1) = f;
	asi32(R1) = fclose(asu64(R1));
	asi64(R1) = n;
	goto L513;
L513:
	return asi64(R1);
}

static i64 mlib_checkfile(u64 file) {
    u64 R1, R2; 
	u64 f;
	R1 = tou64("rb");
	asu64(R2) = file;
	asu64(R1) = fopen(asu64(R2), asu64(R1));
	R2 = R1;
	f = asu64(R2);
	if (!asu64(R1)) goto L518;
	asu64(R1) = f;
	asi32(R1) = fclose(asu64(R1));
	R1 = 1;
	goto L516;
L518:
	R1 = 0;
	goto L516;
L516:
	return asi64(R1);
}

static void mlib_readlinen(u64 handlex, u64 buffer, i64 size) {
    u64 R1, R2, R3; 
	i64 ch;
	u64 p;
	i64 n;
	u8 crseen;
	asu64(R1) = handlex;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L521;
	asu64(R1) = mwindows_os_getstdin();
	handlex = asu64(R1);
L521:
	asu64(R1) = handlex;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L523;
	R1 = 0;
	n = asi64(R1);
	asu64(R1) = buffer;
	p = asu64(R1);
L524:
	asi32(R1) = getchar();
	R1 = toi64(toi32(R1));
	ch = asi64(R1);
	asi64(R1) = ch;
	R2 = 13;
	if (asi64(R1) == asi64(R2)) goto L528;
	asi64(R1) = ch;
	R2 = 10;
	if (asi64(R1) == asi64(R2)) goto L528;
	asi64(R1) = ch;
	R2 = -1;
	if (asi64(R1) != asi64(R2)) goto L527;
L528:
	R1 = 0;
	asu64(R2) = p;
	*tou8p(R2) = asu8(R1);
	goto L519;
L527:
	asi64(R1) = ch;
	R2 = (u64)&p;
	asu64(R3) = *tou64p(R2); *(tou64p(R2)) += 1; asu64(R2) = asu64(R3);
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&n;
	(*toi64p(R1)) += 1;
	asi64(R1) = n;
	asi64(R2) = size;
	R3 = 2;
	asi64(R2) -= asi64(R3);
	if (asi64(R1) < asi64(R2)) goto L530;
	R1 = 0;
	asu64(R2) = p;
	*tou8p(R2) = asu8(R1);
	goto L519;
L530:
	goto L524;
L523:
	R1 = 0;
	asu64(R2) = buffer;
	*tou8p(R2) = asu8(R1);
	asu64(R1) = handlex;
	asi64(R2) = size;
	R3 = 2;
	asi64(R2) -= asi64(R3);
	asu64(R3) = buffer;
	asu64(R1) = fgets(asu64(R3), asi64(R2), asu64(R1));
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L532;
	goto L519;
L532:
	asu64(R1) = buffer;
	asi64(R1) = strlen(asu64(R1));
	n = asi64(R1);
	asi64(R1) = n;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L534;
	goto L519;
L534:
	asu64(R1) = buffer;
	asi64(R2) = n;
	R1 += (i64)R2;
	R2 = 1;
	R1 -= (i64)R2;
	p = asu64(R1);
	R1 = 0;
	crseen = asu8(R1);
	goto L536;
L535:
	asu64(R1) = p;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 13;
	if (asi64(R1) == asi64(R2)) goto L540;
	asu64(R1) = p;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 10;
	if (asi64(R1) != asi64(R2)) goto L539;
L540:
	R1 = 1;
	crseen = asu8(R1);
L539:
	R1 = 0;
	R2 = (u64)&p;
	asu64(R3) = *tou64p(R2); *(tou64p(R2)) -= 1; asu64(R2) = asu64(R3);
	*tou8p(R2) = asu8(R1);
L536:
	asu64(R1) = p;
	asu64(R2) = buffer;
	if (asu64(R1) < asu64(R2)) goto L541;
	asu64(R1) = p;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 13;
	if (asi64(R1) == asi64(R2)) goto L535;
	asu64(R1) = p;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 10;
	if (asi64(R1) == asi64(R2)) goto L535;
L541:
	asu8(R1) = crseen;
	if (asu8(R1)) goto L543;
	asi64(R1) = n;
	R2 = 4;
	asi64(R1) += asi64(R2);
	asi64(R2) = size;
	if (asi64(R1) <= asi64(R2)) goto L543;
	msysc_m$print_startcon();
	asi64(R1) = size;
	msysc_m$print_i64_nf(asi64(R1));
	asi64(R1) = n;
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	R1 = tou64("line too long");
	mlib_abortprogram(asu64(R1));
L543:
L519:
	return;
}

static void mlib_iconvlcn(u64 s, i64 n) {
    u64 R1, R2; 
	i64 av_1;
	asi64(R1) = n;
	av_1 = asi64(R1);
	asi64(R1) = av_1;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L547;
L545:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	asi32(R1) = tolower(asi32(R1));
	asu64(R2) = s;
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
	if (--asi64(av_1)) goto L545;
L547:
	return;
}

static void mlib_iconvucn(u64 s, i64 n) {
    u64 R1, R2; 
	i64 av_1;
	asi64(R1) = n;
	av_1 = asi64(R1);
	asi64(R1) = av_1;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L551;
L549:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	asi32(R1) = toupper(asi32(R1));
	asu64(R2) = s;
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
	if (--asi64(av_1)) goto L549;
L551:
	return;
}

static u64 mlib_convlcstring(u64 s) {
    u64 R1, R2; 
	u64 s0;
	asu64(R1) = s;
	s0 = asu64(R1);
	goto L554;
L553:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	asi32(R1) = tolower(asi32(R1));
	asu64(R2) = s;
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
L554:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	if (asu8(R1)) goto L553;
	asu64(R1) = s0;
	goto L552;
L552:
	return asu64(R1);
}

static u64 mlib_convucstring(u64 s) {
    u64 R1, R2; 
	u64 s0;
	asu64(R1) = s;
	s0 = asu64(R1);
	goto L558;
L557:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	asi32(R1) = toupper(asi32(R1));
	asu64(R2) = s;
	*tou8p(R2) = asu8(R1);
	R1 = (u64)&s;
	(*tou64p(R1)) += 1;
L558:
	asu64(R1) = s;
	asu8(R1) = *tou8p(R1);
	if (asu8(R1)) goto L557;
	asu64(R1) = s0;
	goto L556;
L556:
	return asu64(R1);
}

static u64 mlib_changeext(u64 s, u64 newext) {
    u64 R1, R2, R3; 
	struct $B25 newext2;
	u64 sext;
	i64 n;
	asu64(R1) = s;
	R2 = (u64)&mlib_changeext_newfile;
	R3 = 0;
	R2 += (i64)R3;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
	asu64(R1) = newext;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 0;
	if (asu64(R1) == asu64(R2)) goto L562;
	R2 = 46;
	if (asu64(R1) == asu64(R2)) goto L563;
	goto L564;
L562:
	R1 = 0;
	R2 = (u64)&newext2;
	R3 = 1;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	R1 = 0;
	R2 = (u64)&newext2;
	R3 = 2;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	goto L561;
L563:
	asu64(R1) = newext;
	R2 = (u64)&newext2;
	R3 = 0;
	R2 += (i64)R3;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
	goto L561;
L564:
	R1 = tou64(".");
	R2 = (u64)&newext2;
	R3 = 0;
	R2 += (i64)R3;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
	asu64(R1) = newext;
	R2 = (u64)&newext2;
	R3 = 0;
	R2 += (i64)R3;
	asu64(R1) = strcat(asu64(R2), asu64(R1));
L561:
	R1 = 1;
	asu64(R2) = s;
	asu64(R1) = mlib_extractext(asu64(R2), asi64(R1));
	sext = asu64(R1);
	asu64(R1) = sext;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 0;
	if (asu64(R1) == asu64(R2)) goto L566;
	R2 = 46;
	if (asu64(R1) == asu64(R2)) goto L567;
	goto L568;
L566:
	R1 = (u64)&newext2;
	R2 = 0;
	R1 += (i64)R2;
	R2 = (u64)&mlib_changeext_newfile;
	R3 = 0;
	R2 += (i64)R3;
	asu64(R1) = strcat(asu64(R2), asu64(R1));
	goto L565;
L567:
	R1 = (u64)&newext2;
	R2 = 1;
	R1 += (i64)R2;
	R2 = (u64)&mlib_changeext_newfile;
	R3 = 0;
	R2 += (i64)R3;
	asu64(R1) = strcat(asu64(R2), asu64(R1));
	goto L565;
L568:
	asu64(R1) = sext;
	asu64(R2) = s;
	asi64(R1) -= asi64(R2);
	R2 = 2;
	asi64(R1) -= asi64(R2);
	n = asi64(R1);
	R1 = (u64)&newext2;
	R2 = 0;
	R1 += (i64)R2;
	R2 = (u64)&mlib_changeext_newfile;
	R3 = 0;
	R2 += (i64)R3;
	asi64(R3) = n;
	R2 += (i64)R3+1;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
L565:
	R1 = (u64)&mlib_changeext_newfile;
	R2 = 0;
	R1 += (i64)R2;
	goto L560;
L560:
	return asu64(R1);
}

static u64 mlib_extractext(u64 s, i64 period) {
    u64 R1, R2; 
	u64 t;
	u64 u;
	asu64(R1) = s;
	asu64(R1) = mlib_extractfile(asu64(R1));
	t = asu64(R1);
	asu64(R1) = t;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L571;
	R1 = tou64("");
	goto L569;
L571:
	asu64(R1) = t;
	asu64(R2) = t;
	asi64(R2) = strlen(asu64(R2));
	R1 += (i64)R2;
	R2 = 1;
	R1 -= (i64)R2;
	u = asu64(R1);
	goto L573;
L572:
	asu64(R1) = u;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 46;
	if (asu64(R1) != asu64(R2)) goto L576;
	asu64(R1) = u;
	R2 = 1;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L578;
	asi64(R1) = period;
	if (!asi64(R1)) goto L580;
	R1 = tou64(".");
	goto L579;
L580:
	R1 = tou64("");
L579:
	goto L569;
L578:
	asu64(R1) = u;
	R2 = 1;
	R1 += (i64)R2;
	goto L569;
L576:
	R1 = (u64)&u;
	(*tou64p(R1)) -=1;
L573:
	asu64(R1) = u;
	asu64(R2) = t;
	if (asu64(R1) >= asu64(R2)) goto L572;
	R1 = tou64("");
	goto L569;
L569:
	return asu64(R1);
}

static u64 mlib_extractpath(u64 s) {
    u64 R1, R2, R3; 
	u64 t;
	i64 n;
	asu64(R1) = s;
	asu64(R2) = s;
	asi64(R2) = strlen(asu64(R2));
	R1 += (i64)R2;
	R2 = 1;
	R1 -= (i64)R2;
	t = asu64(R1);
	goto L583;
L582:
	asu64(R1) = t;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 92;
	if (asu64(R1) == asu64(R2)) goto L586;
	R2 = 47;
	if (asu64(R1) == asu64(R2)) goto L586;
	R2 = 58;
	if (asu64(R1) == asu64(R2)) goto L586;
	goto L587;
L586:
	asu64(R1) = t;
	asu64(R2) = s;
	asi64(R1) -= asi64(R2);
	R2 = 1;
	asi64(R1) += asi64(R2);
	n = asi64(R1);
	asi64(R1) = n;
	asu64(R2) = s;
	R3 = (u64)&mlib_extractpath_str;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	R1 = 0;
	R2 = (u64)&mlib_extractpath_str;
	asi64(R3) = n;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	R1 = (u64)&mlib_extractpath_str;
	goto L581;
	goto L585;
L587:
L585:
	R1 = (u64)&t;
	(*tou64p(R1)) -=1;
L583:
	asu64(R1) = t;
	asu64(R2) = s;
	if (asu64(R1) >= asu64(R2)) goto L582;
	R1 = tou64("");
	goto L581;
L581:
	return asu64(R1);
}

static u64 mlib_extractfile(u64 s) {
    u64 R1, R2; 
	u64 t;
	asu64(R1) = s;
	asu64(R1) = mlib_extractpath(asu64(R1));
	t = asu64(R1);
	asu64(R1) = t;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L590;
	asu64(R1) = s;
	goto L588;
L590:
	asu64(R1) = s;
	asu64(R2) = t;
	asi64(R2) = strlen(asu64(R2));
	R1 += (i64)R2;
	goto L588;
L588:
	return asu64(R1);
}

static u64 mlib_extractbasefile(u64 s) {
    u64 R1, R2, R3; 
	u64 f;
	u64 e;
	i64 n;
	i64 flen;
	asu64(R1) = s;
	asu64(R1) = mlib_extractfile(asu64(R1));
	f = asu64(R1);
	asu64(R1) = f;
	asi64(R1) = strlen(asu64(R1));
	flen = asi64(R1);
	asi64(R1) = flen;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L593;
	R1 = tou64("");
	goto L591;
L593:
	R1 = 0;
	asu64(R2) = f;
	asu64(R1) = mlib_extractext(asu64(R2), asi64(R1));
	e = asu64(R1);
	asu64(R1) = e;
	asu8(R1) = *tou8p(R1);
	if (!asu8(R1)) goto L595;
	asi64(R1) = flen;
	asu64(R2) = e;
	asi64(R2) = strlen(asu64(R2));
	asi64(R1) -= asi64(R2);
	R2 = 1;
	asi64(R1) -= asi64(R2);
	n = asi64(R1);
	asi64(R1) = n;
	asu64(R2) = f;
	R3 = (u64)&mlib_extractbasefile_str;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	R1 = 0;
	R2 = (u64)&mlib_extractbasefile_str;
	asi64(R3) = n;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	R1 = (u64)&mlib_extractbasefile_str;
	goto L591;
L595:
	asu64(R1) = f;
	asi64(R2) = flen;
	R1 += (i64)R2;
	R2 = 1;
	R1 -= (i64)R2;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 46;
	if (asu64(R1) != asu64(R2)) goto L597;
	asi64(R1) = flen;
	R2 = 1;
	asu64(R1) -= asu64(R2);
	asu64(R2) = f;
	R3 = (u64)&mlib_extractbasefile_str;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	R1 = 0;
	R2 = (u64)&mlib_extractbasefile_str;
	asi64(R3) = flen;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	R1 = (u64)&mlib_extractbasefile_str;
	goto L591;
L597:
	asu64(R1) = f;
	goto L591;
L591:
	return asu64(R1);
}

static u64 mlib_addext(u64 s, u64 newext) {
    u64 R1, R2; 
	u64 sext;
	R1 = 1;
	asu64(R2) = s;
	asu64(R1) = mlib_extractext(asu64(R2), asi64(R1));
	sext = asu64(R1);
	asu64(R1) = sext;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L600;
	asu64(R1) = newext;
	asu64(R2) = s;
	asu64(R1) = mlib_changeext(asu64(R2), asu64(R1));
	goto L598;
L600:
	asu64(R1) = s;
	goto L598;
L598:
	return asu64(R1);
}

static u64 mlib_pcm_alloc32() {
    u64 R1, R2, R3; 
	u64 p;
	R1 = 32;
	mlib_allocbytes = asi64(R1);
	R1 = (u64)&mlib_freelist;
	R2 = 2;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	R2 = R1;
	p = asu64(R2);
	if (!asu64(R1)) goto L603;
	R1 = (u64)&mlib_freelist;
	R2 = 2;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	asu64(R1) = *tou64p(R1);
	R2 = (u64)&mlib_freelist;
	R3 = 2;
	*tou64p(((i64)R2+(i64)R3*8)) = asu64(R1);
	asu64(R1) = p;
	goto L601;
L603:
	R1 = 32;
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	goto L601;
L601:
	return asu64(R1);
}

static void mlib_pcm_free32(u64 p) {
    u64 R1, R2, R3; 
	R1 = (u64)&mlib_freelist;
	R2 = 2;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	asu64(R2) = p;
	*tou64p(R2) = asu64(R1);
	asu64(R1) = p;
	R2 = (u64)&mlib_freelist;
	R3 = 2;
	*tou64p(((i64)R2+(i64)R3*8)) = asu64(R1);
	return;
}

static void mlib_outbyte(u64 f, i64 x) {
    u64 R1, R2, R3, R4; 
	asu64(R1) = f;
	R2 = 1;
	R3 = 1;
	R4 = (u64)&x;
	asu64(R1) = fwrite(asu64(R4), asu64(R3), asu64(R2), asu64(R1));
	return;
}

static void mlib_outu16(u64 f, u64 x) {
    u64 R1, R2, R3, R4; 
	asu64(R1) = f;
	R2 = 1;
	R3 = 2;
	R4 = (u64)&x;
	asu64(R1) = fwrite(asu64(R4), asu64(R3), asu64(R2), asu64(R1));
	return;
}

static void mlib_outu32(u64 f, u64 x) {
    u64 R1, R2, R3, R4; 
	asu64(R1) = f;
	R2 = 1;
	R3 = 4;
	R4 = (u64)&x;
	asu64(R1) = fwrite(asu64(R4), asu64(R3), asu64(R2), asu64(R1));
	return;
}

static void mlib_outu64(u64 f, u64 x) {
    u64 R1, R2, R3, R4; 
	asu64(R1) = f;
	R2 = 1;
	R3 = 8;
	R4 = (u64)&x;
	asu64(R1) = fwrite(asu64(R4), asu64(R3), asu64(R2), asu64(R1));
	return;
}

static void mlib_outstring(u64 f, u64 s) {
    u64 R1, R2, R3, R4; 
	asu64(R1) = f;
	R2 = 1;
	asu64(R3) = s;
	asi64(R3) = strlen(asu64(R3));
	R4 = 1;
	asu64(R3) += asu64(R4);
	asu64(R4) = s;
	asu64(R1) = fwrite(asu64(R4), asu64(R3), asu64(R2), asu64(R1));
	return;
}

static void mlib_outblock(u64 f, u64 p, i64 n) {
    u64 R1, R2, R3, R4; 
	asu64(R1) = f;
	R2 = 1;
	asi64(R3) = n;
	asu64(R4) = p;
	asu64(R1) = fwrite(asu64(R4), asu64(R3), asu64(R2), asu64(R1));
	return;
}

static i64 mlib_myeof(u64 f) {
    u64 R1, R2; 
	i64 c;
	asu64(R1) = f;
	asi32(R1) = fgetc(asu64(R1));
	R1 = toi64(toi32(R1));
	c = asi64(R1);
	asi64(R1) = c;
	R2 = -1;
	if (asi64(R1) != asi64(R2)) goto L613;
	R1 = 1;
	goto L611;
L613:
	asu64(R1) = f;
	asi64(R2) = c;
	asi32(R1) = ungetc(asi32(R2), asu64(R1));
	R1 = 0;
	goto L611;
L611:
	return asi64(R1);
}

static void mlib_strbuffer_add(u64 dest, u64 s, i64 n) {
    u64 R1, R2, R3, R4; 
	i64 newlen;
	i64 oldlen;
	u64 newptr;
	asi64(R1) = n;
	R2 = -1;
	if (asi64(R1) != asi64(R2)) goto L616;
	asu64(R1) = s;
	asi64(R1) = strlen(asu64(R1));
	n = asi64(R1);
L616:
	asu64(R1) = dest;
	R2 = 8;
	asi32(R1) = *toi32p(((i64)R1+(i64)R2));
	R1 = toi64(toi32(R1));
	oldlen = asi64(R1);
	asi64(R1) = oldlen;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L618;
	asi64(R1) = n;
	R2 = 1;
	asi64(R1) += asi64(R2);
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	asu64(R2) = dest;
	R3 = 0;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	asi64(R1) = mlib_allocbytes;
	asu64(R2) = dest;
	R3 = 12;
	*toi32p(((i64)R2+(i64)R3)) = asi32(R1);
	asi64(R1) = n;
	asu64(R2) = dest;
	R3 = 8;
	*toi32p(((i64)R2+(i64)R3)) = asi32(R1);
	asi64(R1) = n;
	asu64(R2) = s;
	asu64(R3) = dest;
	R4 = 0;
	asu64(R3) = *tou64p(((i64)R3+(i64)R4));
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	R1 = 0;
	asu64(R2) = dest;
	R3 = 0;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asi64(R3) = n;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	goto L614;
L618:
	asi64(R1) = oldlen;
	asi64(R2) = n;
	asi64(R1) += asi64(R2);
	newlen = asi64(R1);
	asi64(R1) = newlen;
	R2 = 1;
	asi64(R1) += asi64(R2);
	asu64(R2) = dest;
	R3 = 12;
	asi32(R2) = *toi32p(((i64)R2+(i64)R3));
	R2 = toi64(toi32(R2));
	if (asi64(R1) <= asi64(R2)) goto L620;
	asi64(R1) = newlen;
	R2 = 1;
	asi64(R1) += asi64(R2);
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	newptr = asu64(R1);
	asi64(R1) = oldlen;
	asu64(R2) = dest;
	R3 = 0;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asu64(R3) = newptr;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	asu64(R1) = newptr;
	asu64(R2) = dest;
	R3 = 0;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	asi64(R1) = mlib_allocbytes;
	asu64(R2) = dest;
	R3 = 12;
	*toi32p(((i64)R2+(i64)R3)) = asi32(R1);
L620:
	asi64(R1) = n;
	asu64(R2) = s;
	asu64(R3) = dest;
	R4 = 0;
	asu64(R3) = *tou64p(((i64)R3+(i64)R4));
	asi64(R4) = oldlen;
	R3 += (i64)R4;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	R1 = 0;
	asu64(R2) = dest;
	R3 = 0;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asi64(R3) = newlen;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	asi64(R1) = newlen;
	asu64(R2) = dest;
	R3 = 8;
	*toi32p(((i64)R2+(i64)R3)) = asi32(R1);
L614:
	return;
}

static void mlib_gs_init(u64 dest) {
    u64 R1, R2; 
	R1 = 16;
	asu64(R2) = dest;
	mlib_pcm_clearmem(asu64(R2), asi64(R1));
	return;
}

static void mlib_gs_free(u64 dest) {
    u64 R1, R2, R3; 
	asu64(R1) = dest;
	R2 = 12;
	asi32(R1) = *toi32p(((i64)R1+(i64)R2));
	if (!asi32(R1)) goto L624;
	asu64(R1) = dest;
	R2 = 12;
	asi32(R1) = *toi32p(((i64)R1+(i64)R2));
	R1 = toi64(toi32(R1));
	asu64(R2) = dest;
	R3 = 0;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	mlib_pcm_free(asu64(R2), asi64(R1));
L624:
	return;
}

static void mlib_gs_str(u64 dest, u64 s) {
    u64 R1, R2, R3; 
	R1 = -1;
	asu64(R2) = s;
	asu64(R3) = dest;
	mlib_strbuffer_add(asu64(R3), asu64(R2), asi64(R1));
	return;
}

static void mlib_gs_char(u64 dest, i64 c) {
    u64 R1, R2, R3; 
	struct $B3 s;
	asi64(R1) = c;
	R2 = (u64)&s;
	R3 = 1;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	R1 = 0;
	R2 = (u64)&s;
	R3 = 2;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	R1 = 1;
	R2 = (u64)&s;
	asu64(R3) = dest;
	mlib_strbuffer_add(asu64(R3), asu64(R2), asi64(R1));
	return;
}

static void mlib_gs_strn(u64 dest, u64 s, i64 length) {
    u64 R1, R2, R3; 
	asi64(R1) = length;
	asu64(R2) = s;
	asu64(R3) = dest;
	mlib_strbuffer_add(asu64(R3), asu64(R2), asi64(R1));
	return;
}

static void mlib_gs_strvar(u64 dest, u64 s) {
    u64 R1, R2, R3; 
	R1 = -1;
	asu64(R2) = s;
	R3 = 0;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asu64(R3) = dest;
	mlib_strbuffer_add(asu64(R3), asu64(R2), asi64(R1));
	return;
}

static void mlib_gs_strint(u64 dest, i64 a) {
    u64 R1, R2, R3; 
	R1 = -1;
	R2 = 0;
	asi64(R3) = a;
	asu64(R2) = msysc_strint(asi64(R3), asu64(R2));
	asu64(R3) = dest;
	mlib_strbuffer_add(asu64(R3), asu64(R2), asi64(R1));
	return;
}

static void mlib_gs_strln(u64 dest, u64 s) {
    u64 R1, R2; 
	asu64(R1) = s;
	asu64(R2) = dest;
	mlib_gs_str(asu64(R2), asu64(R1));
	asu64(R1) = dest;
	mlib_gs_line(asu64(R1));
	return;
}

static void mlib_gs_strsp(u64 dest, u64 s) {
    u64 R1, R2; 
	asu64(R1) = s;
	asu64(R2) = dest;
	mlib_gs_str(asu64(R2), asu64(R1));
	R1 = tou64(" ");
	asu64(R2) = dest;
	mlib_gs_str(asu64(R2), asu64(R1));
	return;
}

static void mlib_gs_line(u64 dest) {
    u64 R1, R2, R3; 
	R1 = -1;
	R2 = tou64("\n");
	asu64(R3) = dest;
	mlib_strbuffer_add(asu64(R3), asu64(R2), asi64(R1));
	return;
}

static i64 mlib_gs_getcol(u64 dest) {
    u64 R1, R2; 
	asu64(R1) = dest;
	R2 = 8;
	asi32(R1) = *toi32p(((i64)R1+(i64)R2));
	R1 = toi64(toi32(R1));
	goto L633;
L633:
	return asi64(R1);
}

static void mlib_gs_leftstr(u64 dest, u64 s, i64 w, i64 padch) {
    u64 R1, R2, R3, R4; 
	i64 col;
	i64 i;
	i64 n;
	i64 slen;
	struct $B32 str;
	asu64(R1) = dest;
	R2 = 8;
	asi32(R1) = *toi32p(((i64)R1+(i64)R2));
	R1 = toi64(toi32(R1));
	col = asi64(R1);
	asu64(R1) = s;
	R2 = (u64)&str;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
	asu64(R1) = s;
	asi64(R1) = strlen(asu64(R1));
	slen = asi64(R1);
	asi64(R1) = w;
	asi64(R2) = slen;
	asi64(R1) -= asi64(R2);
	n = asi64(R1);
	asi64(R1) = n;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L636;
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = n;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L639;
L637:
	asi64(R1) = padch;
	R2 = (u64)&str;
	asi64(R3) = slen;
	asi64(R4) = i;
	asi64(R3) += asi64(R4);
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	i += 1; if (i <= n) goto L637;
L639:
	R1 = 0;
	R2 = (u64)&str;
	asi64(R3) = slen;
	asi64(R4) = n;
	asi64(R3) += asi64(R4);
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
L636:
	R1 = (u64)&str;
	asu64(R2) = dest;
	mlib_gs_str(asu64(R2), asu64(R1));
	return;
}

static void mlib_gs_leftint(u64 dest, i64 a, i64 w, i64 padch) {
    u64 R1, R2, R3, R4; 
	asi64(R1) = padch;
	asi64(R2) = w;
	R3 = 0;
	asi64(R4) = a;
	asu64(R3) = msysc_strint(asi64(R4), asu64(R3));
	asu64(R4) = dest;
	mlib_gs_leftstr(asu64(R4), asu64(R3), asi64(R2), asi64(R1));
	return;
}

static void mlib_gs_padto(u64 dest, i64 col, i64 ch) {
    u64 R1, R2, R3; 
	i64 n;
	struct $B32 str;
	i64 i;
	asi64(R1) = col;
	asu64(R2) = dest;
	R3 = 8;
	asi32(R2) = *toi32p(((i64)R2+(i64)R3));
	R2 = toi64(toi32(R2));
	asi64(R1) -= asi64(R2);
	n = asi64(R1);
	asi64(R1) = n;
	R2 = 0;
	if (asi64(R1) > asi64(R2)) goto L643;
	goto L641;
L643:
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = n;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L646;
L644:
	asi64(R1) = ch;
	R2 = (u64)&str;
	asi64(R3) = i;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	i += 1; if (i <= n) goto L644;
L646:
	R1 = 0;
	R2 = (u64)&str;
	asi64(R3) = n;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	R1 = (u64)&str;
	asu64(R2) = dest;
	mlib_gs_str(asu64(R2), asu64(R1));
L641:
	return;
}

static void mlib_gs_println(u64 dest, u64 f) {
    u64 R1, R2, R3, R4; 
	asu64(R1) = dest;
	R2 = 8;
	asi32(R1) = *toi32p(((i64)R1+(i64)R2));
	R1 = toi64(toi32(R1));
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L649;
	goto L647;
L649:
	R1 = 0;
	asu64(R2) = dest;
	R3 = 0;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asu64(R3) = dest;
	R4 = 8;
	asi32(R3) = *toi32p(((i64)R3+(i64)R4));
	R3 = toi64(toi32(R3));
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	asu64(R1) = f;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L651;
	msysc_m$print_startcon();
	asu64(R1) = dest;
	R2 = 0;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_nogap();
	R1 = tou64("\r");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	goto L650;
L651:
	asu64(R1) = f;
	msysc_m$print_startfile(asu64(R1));
	asu64(R1) = dest;
	R2 = 0;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_nogap();
	R1 = tou64("\r");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
L650:
L647:
	return;
}

static i64 mlib_nextcmdparamnew(u64 paramno, u64 name, u64 value, u64 defext) {
    u64 R1, R2; 
	u64 q;
	u64 item;
	u64 fileext;
	i64 length;
// PROC LOCAL STATICS GO HERE
	static i64 mlib_nextcmdparamnew_infile = 0;
	static u64 mlib_nextcmdparamnew_filestart = 0;
	static u64 mlib_nextcmdparamnew_fileptr = 0;
	static u8 mlib_nextcmdparamnew_colonseen = 0;
// mlib.nextcmdparamnew.reenter:
L653:
	R1 = 0;
	asu64(R2) = value;
	*tou64p(R2) = asu64(R1);
	R1 = 0;
	asu64(R2) = name;
	*tou64p(R2) = asu64(R1);
	asi64(R1) = mlib_nextcmdparamnew_infile;
	if (!asi64(R1)) goto L655;
	R1 = (u64)&item;
	R2 = (u64)&mlib_nextcmdparamnew_fileptr;
	asi64(R1) = mlib_readnextfileitem(asu64(R2), asu64(R1));
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L657;
	asu64(R1) = mlib_nextcmdparamnew_filestart;
	free(asu64(R1));
	R1 = 0;
	mlib_nextcmdparamnew_infile = asi64(R1);
	goto L653;
L657:
	goto L654;
L655:
	asu64(R1) = paramno;
	asi64(R1) = *toi64p(R1);
	asi64(R2) = msysc_ncmdparams;
	if (asi64(R1) <= asi64(R2)) goto L659;
	R1 = 0;
	goto L652;
L659:
	asu64(R1) = msysc_cmdparams;
	asu64(R2) = paramno;
	asi64(R2) = *toi64p(R2);
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	item = asu64(R1);
	asu64(R1) = paramno;
	(*toi64p(R1)) += 1;
	asu64(R1) = item;
	asi64(R1) = strlen(asu64(R1));
	length = asi64(R1);
	asu64(R1) = item;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 64;
	if (asu64(R1) != asu64(R2)) goto L661;
	asu64(R1) = item;
	R2 = 1;
	R1 += (i64)R2;
	asu64(R1) = mlib_readfile(asu64(R1));
	R2 = R1;
	mlib_nextcmdparamnew_fileptr = asu64(R2);
	mlib_nextcmdparamnew_filestart = asu64(R1);
	asu64(R1) = mlib_nextcmdparamnew_filestart;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L663;
	msysc_m$print_startcon();
	R1 = tou64("Can't open");
	msysc_m$print_str_nf(asu64(R1));
	asu64(R1) = item;
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	R1 = 7;
	exit(R1);
L663:
	R1 = 1;
	mlib_nextcmdparamnew_infile = asi64(R1);
	goto L653;
L661:
	asu64(R1) = item;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 58;
	if (asu64(R1) != asu64(R2)) goto L665;
	R1 = 1;
	mlib_nextcmdparamnew_colonseen = asu8(R1);
	R1 = 4;
	goto L652;
L665:
L654:
	R1 = 0;
	asu64(R2) = value;
	*tou64p(R2) = asu64(R1);
	asu64(R1) = item;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 45;
	if (asu64(R1) != asu64(R2)) goto L667;
	asu64(R1) = item;
	asu8(R2) = mlib_nextcmdparamnew_colonseen;
	if (!asu8(R2)) goto L669;
	R2 = 0;
	goto L668;
L669:
	R2 = 1;
L668:
	R1 += (i64)R2;
	asu64(R2) = name;
	*tou64p(R2) = asu64(R1);
	R1 = 58;
	asu64(R2) = item;
	asu64(R1) = strchr(asu64(R2), asi32(R1));
	q = asu64(R1);
	asu64(R1) = q;
	if (asu64(R1)) goto L671;
	R1 = 61;
	asu64(R2) = item;
	asu64(R1) = strchr(asu64(R2), asi32(R1));
	q = asu64(R1);
L671:
	asu64(R1) = q;
	if (!asu64(R1)) goto L673;
	asu64(R1) = q;
	R2 = 1;
	R1 += (i64)R2;
	asu64(R2) = value;
	*tou64p(R2) = asu64(R1);
	R1 = 0;
	asu64(R2) = q;
	*tou8p(R2) = asu8(R1);
L673:
	asu8(R1) = mlib_nextcmdparamnew_colonseen;
	if (!asu8(R1)) goto L675;
	R1 = 5;
	goto L674;
L675:
	R1 = 1;
L674:
	goto L652;
L667:
	R1 = 0;
	asu64(R2) = item;
	asu64(R1) = mlib_extractext(asu64(R2), asi64(R1));
	fileext = asu64(R1);
	asu64(R1) = item;
	asu64(R2) = name;
	*tou64p(R2) = asu64(R1);
	asu64(R1) = fileext;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L677;
	asu64(R1) = name;
	asu64(R1) = *tou64p(R1);
	R2 = (u64)&mlib_nextcmdparamnew_str;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
	asu64(R1) = defext;
	if (!asu64(R1)) goto L679;
	asu8(R1) = mlib_nextcmdparamnew_colonseen;
	if (asu8(R1)) goto L679;
	asu64(R1) = defext;
	R2 = (u64)&mlib_nextcmdparamnew_str;
	asu64(R1) = mlib_addext(asu64(R2), asu64(R1));
	asu64(R2) = name;
	*tou64p(R2) = asu64(R1);
L679:
	goto L676;
L677:
	R1 = tou64("dll");
	asu64(R2) = fileext;
	asi64(R1) = mlib_eqstring(asu64(R2), asu64(R1));
	if (asi64(R1)) goto L681;
	R1 = tou64("mcx");
	asu64(R2) = fileext;
	asi64(R1) = mlib_eqstring(asu64(R2), asu64(R1));
	if (!asi64(R1)) goto L680;
L681:
	asu8(R1) = mlib_nextcmdparamnew_colonseen;
	if (!asu8(R1)) goto L683;
	R1 = 5;
	goto L682;
L683:
	R1 = 3;
L682:
	goto L652;
L680:
L676:
	asu8(R1) = mlib_nextcmdparamnew_colonseen;
	if (!asu8(R1)) goto L685;
	R1 = 5;
	goto L684;
L685:
	R1 = 2;
L684:
	goto L652;
L652:
	return asi64(R1);
}

static i64 mlib_readnextfileitem(u64 fileptr, u64 item) {
    u64 R1, R2, R3; 
	u64 p;
	u64 pstart;
	u64 pend;
	i64 n;
	asu64(R1) = fileptr;
	asu64(R1) = *tou64p(R1);
	p = asu64(R1);
// mlib.readnextfileitem.reenter:
L687:
L688:
	asu64(R1) = p;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 32;
	if (asu64(R1) == asu64(R2)) goto L691;
	R2 = 9;
	if (asu64(R1) == asu64(R2)) goto L691;
	R2 = 13;
	if (asu64(R1) == asu64(R2)) goto L691;
	R2 = 10;
	if (asu64(R1) == asu64(R2)) goto L691;
	R2 = 26;
	if (asu64(R1) == asu64(R2)) goto L692;
	R2 = 0;
	if (asu64(R1) == asu64(R2)) goto L692;
	goto L693;
L691:
	R1 = (u64)&p;
	(*tou64p(R1)) += 1;
	goto L690;
L692:
	R1 = 0;
	goto L686;
	goto L690;
L693:
	goto L689;
L690:
	goto L688;
L689:
	asu64(R1) = p;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 33;
	if (asu64(R1) == asu64(R2)) goto L695;
	R2 = 35;
	if (asu64(R1) == asu64(R2)) goto L695;
	goto L696;
L695:
	R1 = (u64)&p;
	(*tou64p(R1)) += 1;
L697:
	R1 = (u64)&p;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 10;
	if (asu64(R1) == asu64(R2)) goto L699;
	R2 = 26;
	if (asu64(R1) == asu64(R2)) goto L700;
	R2 = 0;
	if (asu64(R1) == asu64(R2)) goto L700;
	goto L701;
L699:
	goto L687;
	goto L697;
L700:
	asu64(R1) = p;
	R2 = 1;
	R1 -= (i64)R2;
	asu64(R2) = fileptr;
	*tou64p(R2) = asu64(R1);
	R1 = 0;
	goto L686;
	goto L697;
L701:
	goto L697;
	goto L694;
L696:
L694:
	asu64(R1) = p;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 34;
	if (asu64(R1) == asu64(R2)) goto L703;
	goto L704;
L703:
	R1 = (u64)&p;
	asu64(R1) = *(tou64p(R1)) += 1;
	pstart = asu64(R1);
L705:
	asu64(R1) = p;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 0;
	if (asu64(R1) == asu64(R2)) goto L708;
	R2 = 26;
	if (asu64(R1) == asu64(R2)) goto L708;
	R2 = 34;
	if (asu64(R1) == asu64(R2)) goto L709;
	goto L710;
L708:
	msysc_m$print_startcon();
	R1 = tou64("Unexpected EOF in @file");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	R1 = 8;
	exit(R1);
	goto L707;
L709:
	R1 = (u64)&p;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	pend = asu64(R1);
	asu64(R1) = p;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 44;
	if (asu64(R1) != asu64(R2)) goto L712;
	R1 = (u64)&p;
	(*tou64p(R1)) += 1;
L712:
	goto L706;
	goto L707;
L710:
L707:
	R1 = (u64)&p;
	(*tou64p(R1)) += 1;
	goto L705;
L706:
	goto L702;
L704:
	asu64(R1) = p;
	pstart = asu64(R1);
L713:
	asu64(R1) = p;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 0;
	if (asu64(R1) == asu64(R2)) goto L716;
	R2 = 26;
	if (asu64(R1) == asu64(R2)) goto L716;
	R2 = 32;
	if (asu64(R1) == asu64(R2)) goto L717;
	R2 = 9;
	if (asu64(R1) == asu64(R2)) goto L717;
	R2 = 44;
	if (asu64(R1) == asu64(R2)) goto L717;
	R2 = 13;
	if (asu64(R1) == asu64(R2)) goto L717;
	R2 = 10;
	if (asu64(R1) == asu64(R2)) goto L717;
	goto L718;
L716:
	asu64(R1) = p;
	pend = asu64(R1);
	goto L714;
	goto L715;
L717:
	R1 = (u64)&p;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	pend = asu64(R1);
	goto L714;
	goto L715;
L718:
L715:
	R1 = (u64)&p;
	(*tou64p(R1)) += 1;
	goto L713;
L714:
L702:
	asu64(R1) = pend;
	asu64(R2) = pstart;
	asi64(R1) -= asi64(R2);
	n = asi64(R1);
	asi64(R1) = n;
	R2 = 256;
	if (asi64(R1) < asi64(R2)) goto L720;
	msysc_m$print_startcon();
	R1 = tou64("@file item too long");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	R1 = 9;
	exit(R1);
L720:
	asi64(R1) = n;
	asu64(R2) = pstart;
	R3 = (u64)&mlib_readnextfileitem_str;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	R1 = 0;
	R2 = (u64)&mlib_readnextfileitem_str;
	asi64(R3) = n;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	R1 = (u64)&mlib_readnextfileitem_str;
	asu64(R2) = item;
	*tou64p(R2) = asu64(R1);
	asu64(R1) = p;
	asu64(R2) = fileptr;
	*tou64p(R2) = asu64(R1);
	R1 = 1;
	goto L686;
L686:
	return asi64(R1);
}

static void mlib_ipadstr(u64 s, i64 width, u64 padchar) {
    u64 R1, R2; 
	i64 n;
	i64 av_1;
	asu64(R1) = s;
	asi64(R1) = strlen(asu64(R1));
	n = asi64(R1);
	asi64(R1) = width;
	asi64(R2) = n;
	asi64(R1) -= asi64(R2);
	av_1 = asi64(R1);
	asi64(R1) = av_1;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L724;
L722:
	asu64(R1) = padchar;
	asu64(R2) = s;
	asu64(R1) = strcat(asu64(R2), asu64(R1));
	if (--asi64(av_1)) goto L722;
L724:
	return;
}

static u64 mlib_padstr(u64 s, i64 width, u64 padchar) {
    u64 R1, R2, R3; 
	asu64(R1) = s;
	R2 = (u64)&mlib_padstr_str;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
	asu64(R1) = padchar;
	asi64(R2) = width;
	R3 = (u64)&mlib_padstr_str;
	mlib_ipadstr(asu64(R3), asi64(R2), asu64(R1));
	R1 = (u64)&mlib_padstr_str;
	goto L725;
L725:
	return asu64(R1);
}

static u64 mlib_chr(i64 c) {
    u64 R1, R2, R3; 
	asi64(R1) = c;
	R2 = (u64)&mlib_chr_str;
	R3 = 1;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	R1 = 0;
	R2 = (u64)&mlib_chr_str;
	R3 = 2;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	R1 = (u64)&mlib_chr_str;
	goto L726;
L726:
	return asu64(R1);
}

static i64 mlib_cmpstring(u64 s, u64 t) {
    u64 R1, R2; 
	i64 res;
	asu64(R1) = t;
	asu64(R2) = s;
	asi32(R1) = strcmp(asu64(R2), asu64(R1));
	R1 = toi64(toi32(R1));
	R2 = R1;
	res = asi64(R2);
	R2 = 0;
	if (asi64(R1) >= asi64(R2)) goto L729;
	R1 = -1;
	goto L728;
L729:
	asi64(R1) = res;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L730;
	R1 = 1;
	goto L728;
L730:
	R1 = 0;
L728:
	goto L727;
L727:
	return asi64(R1);
}

static i64 mlib_cmpstringn(u64 s, u64 t, i64 n) {
    u64 R1, R2, R3; 
	i64 res;
	asi64(R1) = n;
	asu64(R2) = t;
	asu64(R3) = s;
	asi32(R1) = strncmp(asu64(R3), asu64(R2), asu64(R1));
	R1 = toi64(toi32(R1));
	R2 = R1;
	res = asi64(R2);
	R2 = 0;
	if (asi64(R1) >= asi64(R2)) goto L733;
	R1 = -1;
	goto L732;
L733:
	asi64(R1) = res;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L734;
	R1 = 1;
	goto L732;
L734:
	R1 = 0;
L732:
	goto L731;
L731:
	return asi64(R1);
}

static i64 mlib_eqstring(u64 s, u64 t) {
    u64 R1, R2; 
	asu64(R1) = t;
	asu64(R2) = s;
	asi32(R1) = strcmp(asu64(R2), asu64(R1));
	R1 = toi64(toi32(R1));
	R2 = 0;
	asi64(R1) = asi64(R1)  ==  asi64(R2);
	goto L735;
L735:
	return asi64(R1);
}

static i64 mlib_cmpbytes(u64 p, u64 q, i64 n) {
    u64 R1, R2, R3; 
	i64 res;
	asi64(R1) = n;
	asu64(R2) = q;
	asu64(R3) = p;
	asi32(R1) = memcmp(asu64(R3), asu64(R2), asu64(R1));
	R1 = toi64(toi32(R1));
	R2 = R1;
	res = asi64(R2);
	R2 = 0;
	if (asi64(R1) >= asi64(R2)) goto L738;
	R1 = -1;
	goto L737;
L738:
	asi64(R1) = res;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L739;
	R1 = 1;
	goto L737;
L739:
	R1 = 0;
L737:
	goto L736;
L736:
	return asi64(R1);
}

static i64 mlib_eqbytes(u64 p, u64 q, i64 n) {
    u64 R1, R2, R3; 
	asi64(R1) = n;
	asu64(R2) = q;
	asu64(R3) = p;
	asi32(R1) = memcmp(asu64(R3), asu64(R2), asu64(R1));
	R1 = toi64(toi32(R1));
	R2 = 0;
	asi64(R1) = asi64(R1)  ==  asi64(R2);
	goto L740;
L740:
	return asi64(R1);
}

static void mlib_mseed(u64 a, u64 b) {
    u64 R1, R2, R3; 
	asu64(R1) = a;
	R2 = (u64)&mlib_seed;
	R3 = 1;
	*toi64p(((i64)R2+(i64)R3*8-8)) = asi64(R1);
	asu64(R1) = b;
	if (!asu64(R1)) goto L743;
	asu64(R1) = b;
	R2 = (u64)&mlib_seed;
	R3 = 2;
	*toi64p(((i64)R2+(i64)R3*8-8)) = asi64(R1);
	goto L742;
L743:
	asu64(R1) = a;
	R2 = (u64)&mlib_seed;
	R3 = 2;
	R2 += (i64)R3*8-8;
	*toi64p(R2) ^= asi64(R1);
L742:
	return;
}

static u64 mlib_mrandom() {
    u64 R1, R2, R3; 
	i64 x;
	i64 y;
	R1 = (u64)&mlib_seed;
	R2 = 1;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	x = asi64(R1);
	R1 = (u64)&mlib_seed;
	R2 = 2;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	y = asi64(R1);
	asi64(R1) = y;
	R2 = (u64)&mlib_seed;
	R3 = 1;
	*toi64p(((i64)R2+(i64)R3*8-8)) = asi64(R1);
	asi64(R1) = x;
	R2 = 23;
	asi64(R1) <<= asi64(R2);
	R2 = (u64)&x;
	*toi64p(R2) ^= asi64(R1);
	asi64(R1) = x;
	asi64(R2) = y;
	asi64(R1) ^= asi64(R2);
	asi64(R2) = x;
	R3 = 17;
	asi64(R2) >>= asi64(R3);
	asi64(R1) ^= asi64(R2);
	asi64(R2) = y;
	R3 = 26;
	asi64(R2) >>= asi64(R3);
	asi64(R1) ^= asi64(R2);
	R2 = (u64)&mlib_seed;
	R3 = 2;
	*toi64p(((i64)R2+(i64)R3*8-8)) = asi64(R1);
	R1 = (u64)&mlib_seed;
	R2 = 2;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	asi64(R2) = y;
	asu64(R1) += asu64(R2);
	goto L744;
L744:
	return asu64(R1);
}

static i64 mlib_mrandomp() {
    u64 R1, R2; 
	asi64(R1) = mlib_mrandom();
	R2 = 9223372036854775807;
	asi64(R1) &= asi64(R2);
	goto L745;
L745:
	return asi64(R1);
}

static i64 mlib_mrandomint(i64 n) {
    u64 R1, R2; 
	asi64(R1) = mlib_mrandomp();
	asi64(R2) = n;
	asi64(R1) %= asi64(R2);
	goto L746;
L746:
	return asi64(R1);
}

static i64 mlib_mrandomrange(i64 a, i64 b) {
    u64 R1, R2; 
	i64 span;
	asi64(R1) = b;
	asi64(R2) = a;
	asi64(R1) -= asi64(R2);
	R2 = 1;
	asi64(R1) += asi64(R2);
	span = asi64(R1);
	asi64(R1) = span;
	R2 = 0;
	if (asi64(R1) > asi64(R2)) goto L749;
	R1 = 0;
	goto L747;
L749:
	asi64(R1) = mlib_mrandomp();
	asi64(R2) = span;
	asi64(R1) %= asi64(R2);
	asi64(R2) = a;
	asi64(R1) += asi64(R2);
	goto L747;
L747:
	return asi64(R1);
}

static r64 mlib_mrandomreal() {
    u64 R1, R2; 
	r64 x;
L751:
	asi64(R1) = mlib_mrandomp();
	asr64(R1) = tor64(asi64(R1));
	asr64(R2) = 9.223372036854775800e+018;
	asr64(R1) /= asr64(R2);
	x = asr64(R1);
	asr64(R1) = x;
	asr64(R2) = 1.000000000000000000e+000;
	if (asr64(R1) == asr64(R2)) goto L751;
	asr64(R1) = x;
	goto L750;
L750:
	return asr64(R1);
}

static r64 mlib_mrandomreal1() {
    u64 R1, R2; 
	asi64(R1) = mlib_mrandomp();
	asr64(R1) = tor64(asi64(R1));
	asr64(R2) = 9.223372036854775800e+018;
	asr64(R1) /= asr64(R2);
	goto L754;
L754:
	return asr64(R1);
}

static u64 mlib_readline() {
    u64 R1; 
	msysc_m$read_conline();
	asu64(R1) = msysc_rd_buffer;
	goto L755;
L755:
	return asu64(R1);
}

static u64 mlib_findfunction(u64 name) {
    u64 R1, R2, R3; 
	i64 av_1;
	i64 i;
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = $nprocs;
	av_1 = asi64(R1);
	asi64(R1) = av_1;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L759;
L757:
	asu64(R1) = name;
	R2 = (u64)&$procname;
	asi64(R3) = i;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3*8-8));
	asi64(R1) = mlib_eqstring(asu64(R2), asu64(R1));
	if (!asi64(R1)) goto L761;
	R1 = (u64)&$procaddr;
	asi64(R2) = i;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	goto L756;
L761:
	i += 1; if (i <= av_1) goto L757;
L759:
	R1 = 0;
	goto L756;
L756:
	return asu64(R1);
}

static i64 mlib_roundtoblock(i64 n, i64 align) {
    u64 R1, R2, R3, R4, R5; 
	asi64(R1) = n;
	asi64(R2) = align;
	R3 = 1;
	asi64(R2) -= asi64(R3);
	asi64(R1) &= asi64(R2);
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L764;
	asi64(R1) = n;
	goto L762;
L764:
	asi64(R1) = n;
	asi64(R2) = align;
	asi64(R3) = n;
	asi64(R4) = align;
	R5 = 1;
	asi64(R4) -= asi64(R5);
	asi64(R3) &= asi64(R4);
	asi64(R2) -= asi64(R3);
	asi64(R1) += asi64(R2);
	goto L762;
L762:
	return asi64(R1);
}

static u64 mlib_pcm_allocnfz(i64 n) {
    u64 R1, R2, R3, R4; 
	u64 p;
	asi64(R1) = n;
	R2 = 7;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L767;
	asi64(R1) = n;
	R2 = 8;
	asi64(R3) = n;
	R4 = 7;
	asi64(R3) &= asi64(R4);
	asi64(R2) -= asi64(R3);
	asi64(R1) += asi64(R2);
	n = asi64(R1);
L767:
	asu64(R1) = mlib_pcheapptr;
	p = asu64(R1);
	asi64(R1) = n;
	R2 = (u64)&mlib_pcheapptr;
	*tou64p(R2) += asu64(R1);
	asu64(R1) = mlib_pcheapptr;
	asu64(R2) = mlib_pcheapend;
	if (asu64(R1) < asu64(R2)) goto L769;
	asi64(R1) = n;
	asu64(R1) = mlib_pcm_newblock(asi64(R1));
	p = asu64(R1);
L769:
	asu64(R1) = p;
	goto L765;
L765:
	return asu64(R1);
}

static void mwindows_os_init() {
    u64 R1, R2, R3; 
	R1 = -11;
	asi64(R1) = toi64(tou32(R1));
	asu64(R1) = GetStdHandle(asu32(R1));
	mwindows_hconsole = asu64(R1);
	R1 = -10;
	asi64(R1) = toi64(tou32(R1));
	asu64(R1) = GetStdHandle(asu32(R1));
	mwindows_hconsolein = asu64(R1);
	R1 = 0;
	R2 = (u64)&mwindows_lastkey;
	R3 = 8;
	*tou16p(((i64)R2+(i64)R3)) = asu16(R1);
	R1 = 0;
	mwindows_keypending = asi64(R1);
	R1 = 1;
	R2 = 0;
	asi64(R1) = SetConsoleCtrlHandler(asu64(R2), asi64(R1));
	R1 = 3;
	asu64(R2) = mwindows_hconsole;
	asi64(R1) = SetConsoleMode(asu64(R2), asu32(R1));
	R1 = 1;
	mwindows_init_flag = asi64(R1);
	return;
}

static i64 mwindows_os_execwait(u64 cmdline, i64 newconsole, u64 workdir) {
    u64 R1, R2, R3, R4, R5, R6, R7, R8, R9, R10; 
	u32 exitcode;
	i64 status;
	i64 cflags;
	struct $B26 si;
	struct $B8 xpi;
	R1 = 0;
	cflags = asi64(R1);
	R1 = (u64)&si;
	memset(R1, 0, 104);
	R1 = (u64)&xpi;
	memset(R1, 0, 24);
	asi64(R1) = newconsole;
	R2 = 0;
	if (asi64(R1) == asi64(R2)) goto L773;
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L774;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L775;
	goto L776;
L773:
	R1 = 32;
	cflags = asi64(R1);
	goto L772;
L774:
	R1 = 48;
	cflags = asi64(R1);
	goto L772;
L775:
	R1 = 48;
	cflags = asi64(R1);
	goto L772;
L776:
L772:
	R1 = 104;
	R2 = (u64)&si;
	R3 = 0;
	*tou32p(((i64)R2+(i64)R3)) = asu32(R1);
	R1 = (u64)&xpi;
	R2 = (u64)&si;
	R3 = 0;
	R4 = 0;
	asi64(R5) = cflags;
	R6 = 1;
	R7 = 0;
	R8 = 0;
	asu64(R9) = cmdline;
	R10 = 0;
	asi64(R1) = CreateProcessA(asu64(R10), asu64(R9), asu64(R8), asu64(R7), asi64(R6), asu32(R5), asu64(R4), asu64(R3), asu64(R2), asu64(R1));
	status = asi64(R1);
	asi64(R1) = status;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L778;
	asu32(R1) = GetLastError();
	R1 = toi64(tou32(R1));
	status = asi64(R1);
	asi64(R1) = status;
	R2 = tou64("Winexec error: %lld\n");
	asi32(R1) = printf(asu64(R2), asi64(R1));
	R1 = -1;
	goto L771;
L778:
	R1 = 4294967295;
	R2 = (u64)&xpi;
	R3 = 0;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asu32(R1) = WaitForSingleObject(asu64(R2), asu32(R1));
	R1 = (u64)&exitcode;
	R2 = (u64)&xpi;
	R3 = 0;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asi64(R1) = GetExitCodeProcess(asu64(R2), asu64(R1));
	R1 = (u64)&xpi;
	R2 = 0;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R1) = CloseHandle(asu64(R1));
	R1 = (u64)&xpi;
	R2 = 8;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R1) = CloseHandle(asu64(R1));
	asu32(R1) = exitcode;
	R1 = toi64(tou32(R1));
	goto L771;
L771:
	return asi64(R1);
}

static i64 mwindows_os_execcmd(u64 cmdline, i64 newconsole) {
    u64 R1, R2, R3, R4, R5, R6, R7, R8, R9, R10; 
	struct $B26 si;
	struct $B8 xpi;
	R1 = (u64)&si;
	memset(R1, 0, 104);
	R1 = (u64)&xpi;
	memset(R1, 0, 24);
	R1 = 104;
	R2 = (u64)&si;
	R3 = 0;
	*tou32p(((i64)R2+(i64)R3)) = asu32(R1);
	R1 = (u64)&xpi;
	R2 = (u64)&si;
	R3 = 0;
	R4 = 0;
	R5 = 32;
	asi64(R6) = newconsole;
	if (!asi64(R6)) goto L781;
	R6 = 16;
	goto L780;
L781:
	R6 = 0;
L780:
	asi64(R5) |= asi64(R6);
	R6 = 1;
	R7 = 0;
	R8 = 0;
	asu64(R9) = cmdline;
	R10 = 0;
	asi64(R1) = CreateProcessA(asu64(R10), asu64(R9), asu64(R8), asu64(R7), asi64(R6), asu32(R5), asu64(R4), asu64(R3), asu64(R2), asu64(R1));
	R1 = (u64)&xpi;
	R2 = 0;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R1) = CloseHandle(asu64(R1));
	R1 = (u64)&xpi;
	R2 = 8;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R1) = CloseHandle(asu64(R1));
	R1 = 1;
	goto L779;
L779:
	return asi64(R1);
}

static i64 mwindows_os_getch() {
    u64 R1, R2; 
	i64 k;
	asi64(R1) = mwindows_os_getchx();
	R2 = 255;
	asi64(R1) &= asi64(R2);
	k = asi64(R1);
	asi64(R1) = k;
	goto L782;
L782:
	return asi64(R1);
}

static i64 mwindows_os_kbhit() {
    u64 R1, R2; 
	u32 count;
	asi64(R1) = mwindows_init_flag;
	if (asi64(R1)) goto L785;
	mwindows_os_init();
L785:
	R1 = (u64)&count;
	asu64(R2) = mwindows_hconsolein;
	asi64(R1) = GetNumberOfConsoleInputEvents(asu64(R2), asu64(R1));
	asu32(R1) = count;
	R1 = toi64(tou32(R1));
	R2 = 1;
	asi64(R1) = asi64(R1)  >  asi64(R2);
	goto L783;
L783:
	return asi64(R1);
}

static u64 mwindows_os_getdllinst(u64 name) {
    u64 R1; 
	u64 hinst;
	asu64(R1) = name;
	asu64(R1) = LoadLibraryA(asu64(R1));
	hinst = asu64(R1);
	asu64(R1) = hinst;
	goto L786;
L786:
	return asu64(R1);
}

static u64 mwindows_os_getdllprocaddr(i64 hinst, u64 name) {
    u64 R1, R2; 
	asu64(R1) = name;
	asi64(R2) = hinst;
	asu64(R1) = GetProcAddress(asu64(R2), asu64(R1));
	goto L787;
L787:
	return asu64(R1);
}

static void mwindows_os_initwindows() {
    u64 R1; 
	mwindows_os_init();
	R1 = tou64("pcc001");
	mwindows_os_gxregisterclass(asu64(R1));
	return;
}

static void mwindows_os_gxregisterclass(u64 classname) {
    u64 R1, R2, R3; 
	struct $B1 r;
	asu8(R1) = mwindows_os_gxregisterclass_registered;
	if (!asu8(R1)) goto L791;
	goto L789;
L791:
	R1 = (u64)&r;
	memset(R1, 0, 80);
	R1 = 80;
	R2 = (u64)&r;
	R3 = 0;
	*tou32p(((i64)R2+(i64)R3)) = asu32(R1);
	R1 = 40;
	R2 = (u64)&r;
	R3 = 4;
	*tou32p(((i64)R2+(i64)R3)) = asu32(R1);
	R1 = (u64)&mwindows_mainwndproc;
	R2 = (u64)&r;
	R3 = 8;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	R1 = 0;
	R2 = (u64)&r;
	R3 = 24;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	R1 = 0;
	R2 = (u64)&r;
	R3 = 32;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	R1 = 32512;
	R2 = 0;
	asu64(R1) = LoadCursorA(asu64(R2), asu64(R1));
	R2 = (u64)&r;
	R3 = 40;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	R1 = 16;
	R2 = (u64)&r;
	R3 = 48;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	R1 = 0;
	R2 = (u64)&r;
	R3 = 56;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	asu64(R1) = classname;
	R2 = (u64)&r;
	R3 = 64;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	R1 = 0;
	R2 = (u64)&r;
	R3 = 72;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	R1 = (u64)&r;
	asu32(R1) = RegisterClassExA(asu64(R1));
	R1 = toi64(tou32(R1));
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L793;
	asu32(R1) = GetLastError();
	R1 = toi64(tou32(R1));
	asu64(R2) = classname;
	R3 = tou64("Regclass error: %lld %lld\n");
	asi32(R1) = printf(asu64(R3), asu64(R2), asi64(R1));
	R1 = 1;
	exit(R1);
L793:
	R1 = 1;
	mwindows_os_gxregisterclass_registered = asu8(R1);
L789:
	return;
}

static i64 mwindows_mainwndproc(u64 hwnd, u32 message, u64 wparam, u64 lparam) {
    u64 R1, R2, R3, R4; 
	struct $B12 m;
	i64 result;
// PROC LOCAL STATICS GO HERE
	static i64 mwindows_mainwndproc_count = 0;
	asu64(R1) = hwnd;
	R2 = (u64)&m;
	R3 = 0;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	asu32(R1) = message;
	R2 = (u64)&m;
	R3 = 8;
	*tou32p(((i64)R2+(i64)R3)) = asu32(R1);
	asu64(R1) = wparam;
	R2 = (u64)&m;
	R3 = 16;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	asu64(R1) = lparam;
	R2 = (u64)&m;
	R3 = 24;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	R1 = 0;
	R2 = (u64)&m;
	R3 = 40;
	*toi32p(((i64)R2+(i64)R3)) = asi32(R1);
	R1 = 0;
	R2 = (u64)&m;
	R3 = 44;
	*toi32p(((i64)R2+(i64)R3)) = asi32(R1);
	asu64(R1) = mwindows_wndproc_callbackfn;
	if (!asu64(R1)) goto L796;
	R1 = (u64)&m;
	asu64(R2) = mwindows_wndproc_callbackfn;
	asi64(R1) = ((F1)R2)(asu64(R1));
	result = asi64(R1);
	goto L795;
L796:
	R1 = 0;
	result = asi64(R1);
L795:
	R1 = (u64)&m;
	R2 = 8;
	asu32(R1) = *tou32p(((i64)R1+(i64)R2));
	R1 = toi64(tou32(R1));
	R2 = 2;
	if (asi64(R1) != asi64(R2)) goto L798;
	R1 = 0;
	goto L794;
L798:
	asi64(R1) = result;
	if (asi64(R1)) goto L800;
	asu64(R1) = lparam;
	asu64(R2) = wparam;
	asu32(R3) = message;
	R3 = toi64(tou32(R3));
	asu64(R4) = hwnd;
	asi64(R1) = DefWindowProcA(asu64(R4), asu32(R3), asu64(R2), asu64(R1));
	goto L799;
L800:
	R1 = 0;
L799:
	goto L794;
L794:
	return asi64(R1);
}

static void mwindows_os_setmesshandler(u64 addr) {
    u64 R1; 
	asu64(R1) = addr;
	mwindows_wndproc_callbackfn = asu64(R1);
	return;
}

static i64 mwindows_os_getchx() {
    u64 R1, R2, R3, R4; struct $B17 R1_B17; 
	i64 count;
	i64 charcode;
	i64 keyshift;
	i64 keycode;
	i64 altdown;
	i64 ctrldown;
	i64 shiftdown;
	i64 capslock;
	asi64(R1) = mwindows_init_flag;
	if (asi64(R1)) goto L804;
	mwindows_os_init();
L804:
	asi64(R1) = mwindows_keypending;
	if (!asi64(R1)) goto L806;
	(R1_B17) = mwindows_pendkey;
	mwindows_lastkey = (R1_B17);
	R1 = 0;
	mwindows_keypending = asi64(R1);
	goto L805;
L806:
	R1 = (u64)&mwindows_lastkey;
	R2 = 8;
	asu16(R1) = *tou16p(((i64)R1+(i64)R2));
	R1 = toi64(tou16(R1));
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L808;
L809:
	R1 = 0;
	count = asi64(R1);
	R1 = (u64)&count;
	R2 = 1;
	R3 = (u64)&mwindows_lastkey;
	asu64(R4) = mwindows_hconsolein;
	asi64(R1) = ReadConsoleInputA(asu64(R4), asu64(R3), asu32(R2), asu64(R1));
	R1 = (u64)&mwindows_lastkey;
	R2 = 0;
	asu16(R1) = *tou16p(((i64)R1+(i64)R2));
	R1 = toi64(tou16(R1));
	R2 = 1;
	if (asi64(R1) != asi64(R2)) goto L809;
	R1 = (u64)&mwindows_lastkey;
	R2 = 4;
	asu32(R1) = *tou32p(((i64)R1+(i64)R2));
	R1 = toi64(tou32(R1));
	R2 = 1;
	if (asi64(R1) != asi64(R2)) goto L809;
L808:
L805:
	R1 = (u64)&mwindows_lastkey;
	R2 = 16;
	asu32(R1) = *tou32p(((i64)R1+(i64)R2));
	R1 = toi64(tou32(R1));
	R2 = 3;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L813;
	R1 = 1;
	goto L812;
L813:
	R1 = 0;
L812:
	altdown = asi64(R1);
	R1 = (u64)&mwindows_lastkey;
	R2 = 16;
	asu32(R1) = *tou32p(((i64)R1+(i64)R2));
	R1 = toi64(tou32(R1));
	R2 = 12;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L815;
	R1 = 1;
	goto L814;
L815:
	R1 = 0;
L814:
	ctrldown = asi64(R1);
	R1 = (u64)&mwindows_lastkey;
	R2 = 16;
	asu32(R1) = *tou32p(((i64)R1+(i64)R2));
	R1 = toi64(tou32(R1));
	R2 = 16;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L817;
	R1 = 1;
	goto L816;
L817:
	R1 = 0;
L816:
	shiftdown = asi64(R1);
	R1 = (u64)&mwindows_lastkey;
	R2 = 16;
	asu32(R1) = *tou32p(((i64)R1+(i64)R2));
	R1 = toi64(tou32(R1));
	R2 = 128;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L819;
	R1 = 1;
	goto L818;
L819:
	R1 = 0;
L818:
	capslock = asi64(R1);
	R1 = (u64)&mwindows_lastkey;
	R2 = 8;
	R1 += (i64)R2;
	(*tou16p(R1)) -=1;
	R1 = (u64)&mwindows_lastkey;
	R2 = 14;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	charcode = asi64(R1);
	R1 = (u64)&mwindows_lastkey;
	R2 = 10;
	asu16(R1) = *tou16p(((i64)R1+(i64)R2));
	R1 = toi64(tou16(R1));
	R2 = 255;
	asi64(R1) &= asi64(R2);
	keycode = asi64(R1);
	asi64(R1) = charcode;
	R2 = 0;
	if (asi64(R1) >= asi64(R2)) goto L821;
	asi64(R1) = charcode;
	R2 = -128;
	if (asi64(R1) >= asi64(R2)) goto L823;
	R1 = 0;
	charcode = asi64(R1);
	goto L822;
L823:
	R1 = 256;
	R2 = (u64)&charcode;
	*toi64p(R2) += asi64(R1);
L822:
L821:
	asi64(R1) = altdown;
	if (!asi64(R1)) goto L825;
	asi64(R1) = ctrldown;
	if (!asi64(R1)) goto L825;
	asi64(R1) = charcode;
	R2 = 166;
	if (asi64(R1) != asi64(R2)) goto L825;
	R1 = 0;
	R2 = R1;
	ctrldown = asi64(R2);
	altdown = asi64(R1);
	goto L824;
L825:
	asi64(R1) = altdown;
	if (asi64(R1)) goto L828;
	asi64(R1) = ctrldown;
	if (!asi64(R1)) goto L827;
L828:
	R1 = 0;
	charcode = asi64(R1);
	asi64(R1) = keycode;
	R2 = 65;
	if (asi64(R1) < asi64(R2)) goto L830;
	asi64(R1) = keycode;
	R2 = 90;
	if (asi64(R1) > asi64(R2)) goto L830;
	asi64(R1) = keycode;
	R2 = 64;
	asi64(R1) -= asi64(R2);
	charcode = asi64(R1);
L830:
L827:
L824:
	asi64(R1) = capslock;
	R2 = 3;
	asi64(R1) <<= asi64(R2);
	asi64(R2) = altdown;
	R3 = 2;
	asi64(R2) <<= asi64(R3);
	asi64(R1) |= asi64(R2);
	asi64(R2) = ctrldown;
	R3 = 1;
	asi64(R2) <<= asi64(R3);
	asi64(R1) |= asi64(R2);
	asi64(R2) = shiftdown;
	asi64(R1) |= asi64(R2);
	keyshift = asi64(R1);
	asi64(R1) = keyshift;
	R2 = 24;
	asi64(R1) <<= asi64(R2);
	asi64(R2) = keycode;
	R3 = 16;
	asi64(R2) <<= asi64(R3);
	asi64(R1) |= asi64(R2);
	asi64(R2) = charcode;
	asi64(R1) |= asi64(R2);
	goto L802;
L802:
	return asi64(R1);
}

static u64 mwindows_os_getos() {
    u64 R1; 
	R1 = tou64("W64");
	goto L831;
L831:
	return asu64(R1);
}

static i64 mwindows_os_gethostsize() {
    u64 R1; 
	R1 = 64;
	goto L832;
L832:
	return asi64(R1);
}

static i64 mwindows_os_shellexec(u64 opc, u64 file) {
    u64 R1; 
	asu64(R1) = file;
	asi32(R1) = system(asu64(R1));
	R1 = toi64(toi32(R1));
	goto L833;
L833:
	return asi64(R1);
}

static void mwindows_os_sleep(i64 a) {
    u64 R1; 
	asi64(R1) = a;
	Sleep(asu32(R1));
	return;
}

static u64 mwindows_os_getstdin() {
    u64 R1, R2; 
	R1 = tou64("rb");
	R2 = tou64("con");
	asu64(R1) = fopen(asu64(R2), asu64(R1));
	goto L835;
L835:
	return asu64(R1);
}

static u64 mwindows_os_getstdout() {
    u64 R1, R2; 
	R1 = tou64("wb");
	R2 = tou64("con");
	asu64(R1) = fopen(asu64(R2), asu64(R1));
	goto L836;
L836:
	return asu64(R1);
}

static u64 mwindows_os_gethostname() {
    u64 R1, R2, R3; 
	R1 = 300;
	R2 = (u64)&mwindows_os_gethostname_name;
	R3 = 0;
	asu32(R1) = GetModuleFileNameA(asu64(R3), asu64(R2), asu32(R1));
	R1 = (u64)&mwindows_os_gethostname_name;
	goto L837;
L837:
	return asu64(R1);
}

static u64 mwindows_os_getmpath() {
    u64 R1; 
	R1 = tou64("C:@@@@\\m\\\"");
	goto L838;
L838:
	return asu64(R1);
}

static i64 mwindows_os_clock() {
    u64 R1; 
	asi64(R1) = mwindows_os_hpcounter();
	goto L839;
L839:
	return asi64(R1);
}

static i64 mwindows_os_ticks() {
    u64 R1; 
	asi64(R1) = GetTickCount64();
	goto L840;
L840:
	return asi64(R1);
}

static i64 mwindows_os_iswindows() {
    u64 R1; 
	R1 = 1;
	goto L841;
L841:
	return asi64(R1);
}

static void mwindows_os_getsystime(u64 tm) {
    u64 R1; 
	asu64(R1) = tm;
	GetLocalTime(asu64(R1));
	return;
}

static void mwindows_os_peek() {
    u64 R1, R2, R3, R4, R5; 
	i64 ticks;
	struct $B5 m;
	asi64(R1) = GetTickCount64();
	ticks = asi64(R1);
	asi64(R1) = ticks;
	asi64(R2) = mwindows_os_peek_lastticks;
	asi64(R1) -= asi64(R2);
	R2 = 1000;
	if (asi64(R1) < asi64(R2)) goto L845;
	asi64(R1) = ticks;
	mwindows_os_peek_lastticks = asi64(R1);
	R1 = 0;
	R2 = 0;
	R3 = 0;
	R4 = 0;
	R5 = (u64)&m;
	asu32(R1) = PeekMessageA(asu64(R5), asu64(R4), asu32(R3), asu32(R2), asu32(R1));
L845:
	return;
}

static u64 mwindows_os_allocexecmem(i64 n) {
    u64 R1, R2, R3, R4; 
	u64 p;
	u32 oldprot;
	i64 status;
	R1 = 1;
	R2 = 12288;
	asi64(R3) = n;
	R4 = 0;
	asu64(R1) = VirtualAlloc(asu64(R4), asu32(R3), asu32(R2), asu32(R1));
	p = asu64(R1);
	asu64(R1) = p;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L848;
	R1 = 0;
	goto L846;
L848:
	R1 = (u64)&oldprot;
	R2 = 64;
	asi64(R3) = n;
	asu64(R4) = p;
	asu32(R1) = VirtualProtect(asu64(R4), asu32(R3), asu32(R2), asu64(R1));
	R1 = toi64(tou32(R1));
	status = asi64(R1);
	asi64(R1) = status;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L850;
	R1 = 0;
	goto L846;
L850:
	asu64(R1) = p;
	goto L846;
L846:
	return asu64(R1);
}

static i64 mwindows_dirlist(u64 filespec, u64 dest, i64 capacity, i64 t) {
    u64 R1, R2, R3; 
	u64 hfind;
	struct $B33 file;
	i64 nfiles;
	struct $B15 path;
	struct $B15 fullfilename;
	R1 = 0;
	nfiles = asi64(R1);
	asu64(R1) = filespec;
	asu64(R1) = mlib_extractpath(asu64(R1));
	R2 = (u64)&path;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
	R1 = (u64)&file;
	asu64(R2) = filespec;
	asu64(R1) = FindFirstFileA(asu64(R2), asu64(R1));
	R2 = R1;
	hfind = asu64(R2);
	R2 = -1;
	if (asu64(R1) == asu64(R2)) goto L853;
L854:
	R1 = (u64)&file;
	R2 = 0;
	asu32(R1) = *tou32p(((i64)R1+(i64)R2));
	R1 = toi64(tou32(R1));
	R2 = 16;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L858;
	asi64(R1) = t;
	R2 = 2;
	asi64(R1) &= asi64(R2);
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L860;
	goto L855;
L860:
	goto L857;
L858:
	asi64(R1) = t;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L862;
	goto L855;
L862:
L857:
	asi64(R1) = nfiles;
	asi64(R2) = capacity;
	if (asi64(R1) < asi64(R2)) goto L864;
	R1 = -1;
	nfiles = asi64(R1);
	goto L856;
L864:
	asi64(R1) = t;
	R2 = 4;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L866;
	R1 = (u64)&file;
	R2 = 44;
	R1 += (i64)R2;
	asu64(R1) = mlib_convlcstring(asu64(R1));
L866:
	R1 = (u64)&path;
	R2 = (u64)&fullfilename;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
	R1 = (u64)&file;
	R2 = 44;
	R1 += (i64)R2;
	R2 = (u64)&fullfilename;
	asu64(R1) = strcat(asu64(R2), asu64(R1));
	R1 = (u64)&fullfilename;
	asu64(R1) = mlib_pcm_copyheapstring(asu64(R1));
	asu64(R2) = dest;
	R3 = (u64)&nfiles;
	asi64(R3) = *(toi64p(R3)) += 1;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
L855:
	R1 = (u64)&file;
	asu64(R2) = hfind;
	asu32(R1) = FindNextFileA(asu64(R2), asu64(R1));
	if (asu32(R1)) goto L854;
L856:
	asu64(R1) = hfind;
	asu32(R1) = FindClose(asu64(R1));
L853:
	asi64(R1) = nfiles;
	goto L851;
L851:
	return asi64(R1);
}

static i64 mwindows_os_hpcounter() {
    u64 R1, R2; 
	i64 a;
	asi64(R1) = mwindows_hpfreq;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L869;
	asi64(R1) = mwindows_os_hpfreq();
	R2 = 1000;
   if (asi64(R2) == 0) {puts((u64)"Divide by zero"); exit(1);}
	asi64(R1) /= asi64(R2);
	mwindows_hpfreq = asi64(R1);
L869:
	R1 = (u64)&a;
	asu32(R1) = QueryPerformanceCounter(asu64(R1));
	asi64(R1) = a;
	asi64(R2) = mwindows_hpfreq;
   if (asi64(R2) == 0) {puts((u64)"Divide by zero"); exit(1);}
	asi64(R1) /= asi64(R2);
	goto L867;
L867:
	return asi64(R1);
}

static i64 mwindows_os_hpfreq() {
    u64 R1; 
	i64 a;
	R1 = (u64)&a;
	asu32(R1) = QueryPerformanceFrequency(asu64(R1));
	asi64(R1) = a;
	goto L870;
L870:
	return asi64(R1);
}

static u64 mwindllc_os_calldllfunction(u64 fnaddr, i64 retcode, i64 nargs, u64 args, u64 argcodes) {
    u64 R1, R2, R3; 
	u64 a;
	r64 x;
	i64 oddstack;
	i64 nextra;
	i64 pushedbytes;
	asi64(R1) = retcode;
	R2 = 73;
	if (asi64(R1) != asi64(R2)) goto L873;
	asi64(R1) = nargs;
	asu64(R2) = args;
	asu64(R3) = fnaddr;
	asu64(R1) = mwindllc_calldll_cint(asu64(R3), asu64(R2), asi64(R1));
	goto L872;
L873:
	asi64(R1) = nargs;
	asu64(R2) = args;
	asu64(R3) = fnaddr;
	asu64(R1) = mwindllc_calldll_creal(asu64(R3), asu64(R2), asi64(R1));
L872:
	goto L871;
L871:
	return asu64(R1);
}

static u64 mwindllc_os_pushargs(u64 args, i64 nargs, i64 nextra, u64 fnaddr, i64 isfloat) {
    u64 R1, R2, R3, R4, R5; 
	u64 a;
	r64 x;
	R1 = 0;
	asu64(R2) = args;
	asi64(R3) = nargs;
	asi64(R4) = isfloat;
	if (!asi64(R4)) goto L876;
	R4 = 0;
	goto L875;
L876:
	R4 = 73;
L875:
	asu64(R5) = fnaddr;
	asu64(R1) = mwindllc_os_calldllfunction(asu64(R5), asi64(R4), asi64(R3), asu64(R2), asu64(R1));
	goto L874;
L874:
	return asu64(R1);
}

static i64 mwindllc_calldll_cint(u64 fnaddr, u64 params, i64 nparams) {
    u64 R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13; 
	asi64(R1) = nparams;
	switch (asi64(R1)) {
	case 0: goto L881;
	case 1: goto L882;
	case 2: goto L883;
	case 3: goto L884;
	case 4: goto L885;
	case 5: goto L886;
	case 6: goto L887;
	case 7: goto L880;
	case 8: goto L888;
	case 9: goto L889;
	case 10: goto L890;
	case 11: goto L891;
	case 12: goto L892;
	default: goto L880;
    };
// SWITCH
L881:
	asu64(R1) = fnaddr;
	asi64(R1) = ((F2)R1)();
	goto L877;
	goto L878;
L882:
	asu64(R1) = params;
	R2 = 1;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = fnaddr;
	asi64(R1) = ((F3)R2)(asi64(R1));
	goto L877;
	goto L878;
L883:
	asu64(R1) = params;
	R2 = 2;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = params;
	R3 = 1;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3*8-8));
	asu64(R3) = fnaddr;
	asi64(R1) = ((F4)R3)(asi64(R2), asi64(R1));
	goto L877;
	goto L878;
L884:
	asu64(R1) = params;
	R2 = 3;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = params;
	R3 = 2;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3*8-8));
	asu64(R3) = params;
	R4 = 1;
	asi64(R3) = *toi64p(((i64)R3+(i64)R4*8-8));
	asu64(R4) = fnaddr;
	asi64(R1) = ((F5)R4)(asi64(R3), asi64(R2), asi64(R1));
	goto L877;
	goto L878;
L885:
	asu64(R1) = params;
	R2 = 4;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = params;
	R3 = 3;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3*8-8));
	asu64(R3) = params;
	R4 = 2;
	asi64(R3) = *toi64p(((i64)R3+(i64)R4*8-8));
	asu64(R4) = params;
	R5 = 1;
	asi64(R4) = *toi64p(((i64)R4+(i64)R5*8-8));
	asu64(R5) = fnaddr;
	asi64(R1) = ((F6)R5)(asi64(R4), asi64(R3), asi64(R2), asi64(R1));
	goto L877;
	goto L878;
L886:
	asu64(R1) = params;
	R2 = 5;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = params;
	R3 = 4;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3*8-8));
	asu64(R3) = params;
	R4 = 3;
	asi64(R3) = *toi64p(((i64)R3+(i64)R4*8-8));
	asu64(R4) = params;
	R5 = 2;
	asi64(R4) = *toi64p(((i64)R4+(i64)R5*8-8));
	asu64(R5) = params;
	R6 = 1;
	asi64(R5) = *toi64p(((i64)R5+(i64)R6*8-8));
	asu64(R6) = fnaddr;
	asi64(R1) = ((F7)R6)(asi64(R5), asi64(R4), asi64(R3), asi64(R2), asi64(R1));
	goto L877;
	goto L878;
L887:
	asu64(R1) = params;
	R2 = 6;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = params;
	R3 = 5;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3*8-8));
	asu64(R3) = params;
	R4 = 4;
	asi64(R3) = *toi64p(((i64)R3+(i64)R4*8-8));
	asu64(R4) = params;
	R5 = 3;
	asi64(R4) = *toi64p(((i64)R4+(i64)R5*8-8));
	asu64(R5) = params;
	R6 = 2;
	asi64(R5) = *toi64p(((i64)R5+(i64)R6*8-8));
	asu64(R6) = params;
	R7 = 1;
	asi64(R6) = *toi64p(((i64)R6+(i64)R7*8-8));
	asu64(R7) = fnaddr;
	asi64(R1) = ((F8)R7)(asi64(R6), asi64(R5), asi64(R4), asi64(R3), asi64(R2), asi64(R1));
	goto L877;
	goto L878;
L888:
	asu64(R1) = params;
	R2 = 8;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = params;
	R3 = 7;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3*8-8));
	asu64(R3) = params;
	R4 = 6;
	asi64(R3) = *toi64p(((i64)R3+(i64)R4*8-8));
	asu64(R4) = params;
	R5 = 5;
	asi64(R4) = *toi64p(((i64)R4+(i64)R5*8-8));
	asu64(R5) = params;
	R6 = 4;
	asi64(R5) = *toi64p(((i64)R5+(i64)R6*8-8));
	asu64(R6) = params;
	R7 = 3;
	asi64(R6) = *toi64p(((i64)R6+(i64)R7*8-8));
	asu64(R7) = params;
	R8 = 2;
	asi64(R7) = *toi64p(((i64)R7+(i64)R8*8-8));
	asu64(R8) = params;
	R9 = 1;
	asi64(R8) = *toi64p(((i64)R8+(i64)R9*8-8));
	asu64(R9) = fnaddr;
	asi64(R1) = ((F9)R9)(asi64(R8), asi64(R7), asi64(R6), asi64(R5), asi64(R4), asi64(R3), asi64(R2), asi64(R1));
	goto L877;
	goto L878;
L889:
	asu64(R1) = params;
	R2 = 9;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = params;
	R3 = 8;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3*8-8));
	asu64(R3) = params;
	R4 = 7;
	asi64(R3) = *toi64p(((i64)R3+(i64)R4*8-8));
	asu64(R4) = params;
	R5 = 6;
	asi64(R4) = *toi64p(((i64)R4+(i64)R5*8-8));
	asu64(R5) = params;
	R6 = 5;
	asi64(R5) = *toi64p(((i64)R5+(i64)R6*8-8));
	asu64(R6) = params;
	R7 = 4;
	asi64(R6) = *toi64p(((i64)R6+(i64)R7*8-8));
	asu64(R7) = params;
	R8 = 3;
	asi64(R7) = *toi64p(((i64)R7+(i64)R8*8-8));
	asu64(R8) = params;
	R9 = 2;
	asi64(R8) = *toi64p(((i64)R8+(i64)R9*8-8));
	asu64(R9) = params;
	R10 = 1;
	asi64(R9) = *toi64p(((i64)R9+(i64)R10*8-8));
	asu64(R10) = fnaddr;
	asi64(R1) = ((F10)R10)(asi64(R9), asi64(R8), asi64(R7), asi64(R6), asi64(R5), asi64(R4), asi64(R3), asi64(R2), asi64(R1));
	goto L877;
	goto L878;
L890:
	asu64(R1) = params;
	R2 = 10;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = params;
	R3 = 9;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3*8-8));
	asu64(R3) = params;
	R4 = 8;
	asi64(R3) = *toi64p(((i64)R3+(i64)R4*8-8));
	asu64(R4) = params;
	R5 = 7;
	asi64(R4) = *toi64p(((i64)R4+(i64)R5*8-8));
	asu64(R5) = params;
	R6 = 6;
	asi64(R5) = *toi64p(((i64)R5+(i64)R6*8-8));
	asu64(R6) = params;
	R7 = 5;
	asi64(R6) = *toi64p(((i64)R6+(i64)R7*8-8));
	asu64(R7) = params;
	R8 = 4;
	asi64(R7) = *toi64p(((i64)R7+(i64)R8*8-8));
	asu64(R8) = params;
	R9 = 3;
	asi64(R8) = *toi64p(((i64)R8+(i64)R9*8-8));
	asu64(R9) = params;
	R10 = 2;
	asi64(R9) = *toi64p(((i64)R9+(i64)R10*8-8));
	asu64(R10) = params;
	R11 = 1;
	asi64(R10) = *toi64p(((i64)R10+(i64)R11*8-8));
	asu64(R11) = fnaddr;
	asi64(R1) = ((F11)R11)(asi64(R10), asi64(R9), asi64(R8), asi64(R7), asi64(R6), asi64(R5), asi64(R4), asi64(R3), asi64(R2), asi64(R1));
	goto L877;
	goto L878;
L891:
	asu64(R1) = params;
	R2 = 11;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = params;
	R3 = 10;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3*8-8));
	asu64(R3) = params;
	R4 = 9;
	asi64(R3) = *toi64p(((i64)R3+(i64)R4*8-8));
	asu64(R4) = params;
	R5 = 8;
	asi64(R4) = *toi64p(((i64)R4+(i64)R5*8-8));
	asu64(R5) = params;
	R6 = 7;
	asi64(R5) = *toi64p(((i64)R5+(i64)R6*8-8));
	asu64(R6) = params;
	R7 = 6;
	asi64(R6) = *toi64p(((i64)R6+(i64)R7*8-8));
	asu64(R7) = params;
	R8 = 5;
	asi64(R7) = *toi64p(((i64)R7+(i64)R8*8-8));
	asu64(R8) = params;
	R9 = 4;
	asi64(R8) = *toi64p(((i64)R8+(i64)R9*8-8));
	asu64(R9) = params;
	R10 = 3;
	asi64(R9) = *toi64p(((i64)R9+(i64)R10*8-8));
	asu64(R10) = params;
	R11 = 2;
	asi64(R10) = *toi64p(((i64)R10+(i64)R11*8-8));
	asu64(R11) = params;
	R12 = 1;
	asi64(R11) = *toi64p(((i64)R11+(i64)R12*8-8));
	asu64(R12) = fnaddr;
	asi64(R1) = ((F12)R12)(asi64(R11), asi64(R10), asi64(R9), asi64(R8), asi64(R7), asi64(R6), asi64(R5), asi64(R4), asi64(R3), asi64(R2), asi64(R1));
	goto L877;
	goto L878;
L892:
	asu64(R1) = params;
	R2 = 12;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = params;
	R3 = 11;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3*8-8));
	asu64(R3) = params;
	R4 = 10;
	asi64(R3) = *toi64p(((i64)R3+(i64)R4*8-8));
	asu64(R4) = params;
	R5 = 9;
	asi64(R4) = *toi64p(((i64)R4+(i64)R5*8-8));
	asu64(R5) = params;
	R6 = 8;
	asi64(R5) = *toi64p(((i64)R5+(i64)R6*8-8));
	asu64(R6) = params;
	R7 = 7;
	asi64(R6) = *toi64p(((i64)R6+(i64)R7*8-8));
	asu64(R7) = params;
	R8 = 6;
	asi64(R7) = *toi64p(((i64)R7+(i64)R8*8-8));
	asu64(R8) = params;
	R9 = 5;
	asi64(R8) = *toi64p(((i64)R8+(i64)R9*8-8));
	asu64(R9) = params;
	R10 = 4;
	asi64(R9) = *toi64p(((i64)R9+(i64)R10*8-8));
	asu64(R10) = params;
	R11 = 3;
	asi64(R10) = *toi64p(((i64)R10+(i64)R11*8-8));
	asu64(R11) = params;
	R12 = 2;
	asi64(R11) = *toi64p(((i64)R11+(i64)R12*8-8));
	asu64(R12) = params;
	R13 = 1;
	asi64(R12) = *toi64p(((i64)R12+(i64)R13*8-8));
	asu64(R13) = fnaddr;
	asi64(R1) = ((F13)R13)(asi64(R12), asi64(R11), asi64(R10), asi64(R9), asi64(R8), asi64(R7), asi64(R6), asi64(R5), asi64(R4), asi64(R3), asi64(R2), asi64(R1));
	goto L877;
	goto L878;
L880:
	msysc_m$print_startcon();
	asi64(R1) = nparams;
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	msysc_m$print_startcon();
	R1 = tou64("calldll/c/int unsupported # of params");
	msysc_m$print_str_nf(asu64(R1));
	asi64(R1) = nparams;
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	R1 = 1;
	exit(R1);
L878:
	R1 = 0;
	goto L877;
L877:
	return asi64(R1);
}

static i64 mwindllc_calldll_creal(u64 fnaddr, u64 params, i64 nparams) {
    u64 R1, R2, R3, R4, R5; 
	r64 x;
	asi64(R1) = nparams;
	R2 = 0;
	if (asi64(R1) == asi64(R2)) goto L895;
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L896;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L897;
	goto L898;
L895:
	asu64(R1) = fnaddr;
	asr64(R1) = ((F14)R1)();
	R1 = toi64(asr64(R1));
	goto L893;
	goto L894;
L896:
	asu64(R1) = params;
	R2 = 4;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	asr64(R1) = tor64(asi64(R1));
	asu64(R2) = params;
	R3 = 3;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3*8-8));
	asr64(R2) = tor64(asi64(R2));
	asu64(R3) = params;
	R4 = 2;
	asi64(R3) = *toi64p(((i64)R3+(i64)R4*8-8));
	asr64(R3) = tor64(asi64(R3));
	asu64(R4) = params;
	R5 = 1;
	asi64(R4) = *toi64p(((i64)R4+(i64)R5*8-8));
	asr64(R4) = tor64(asi64(R4));
	mwindllc_os_dummycall(asr64(R4), asr64(R3), asr64(R2), asr64(R1));
	asu64(R1) = params;
	R2 = 1;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = fnaddr;
	asr64(R1) = ((F15)R2)(asi64(R1));
	x = asr64(R1);
	goto L894;
L897:
	asu64(R1) = params;
	R2 = 2;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = params;
	R3 = 1;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3*8-8));
	asu64(R3) = fnaddr;
	asr64(R1) = ((F16)R3)(asi64(R2), asi64(R1));
	x = asr64(R1);
	goto L894;
L898:
	msysc_m$print_startcon();
	R1 = tou64("calldll/c/real too many params");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	R1 = 1;
	exit(R1);
L894:
	asr64(R1) = x;
	asi64(R1) = asi64(R1);
	goto L893;
L893:
	return asi64(R1);
}

static void mwindllc_os_dummycall(r64 a, r64 b, r64 c, r64 d) {
	return;
}

void runmx_main() {
    u64 R1, R2; 
	u64 ss;
	u64 filename;
	u64 p;
	u64 plib;
	i64 cmdskip;
	i64 dorun;
	i64 i;
	R1 = 1;
	dorun = asi64(R1);
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = msysc_ncmdparams;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L904;
L901:
	asu64(R1) = msysc_cmdparams;
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	filename = asu64(R1);
	asu64(R1) = filename;
	asu8(R1) = *tou8p(R1);
	R1 = tou64(tou8(R1));
	R2 = 45;
	if (asu64(R1) != asu64(R2)) goto L906;
	R1 = tou64("-show");
	asu64(R2) = filename;
	asi64(R1) = mlib_eqstring(asu64(R2), asu64(R1));
	if (!asi64(R1)) goto L908;
	R1 = 0;
	dorun = asi64(R1);
	goto L907;
L908:
	msysc_m$print_startcon();
	R1 = tou64("Unknown option:");
	msysc_m$print_str_nf(asu64(R1));
	asu64(R1) = filename;
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	R1 = 1;
	exit(R1);
L907:
	goto L905;
L906:
	R1 = tou64("mx");
	asu64(R2) = filename;
	asu64(R1) = mlib_addext(asu64(R2), asu64(R1));
	asu64(R1) = mlib_pcm_copyheapstring(asu64(R1));
	filename = asu64(R1);
	asi64(R1) = i;
	asi64(R2) = msysc_$cmdskip;
	asi64(R1) += asi64(R2);
	cmdskip = asi64(R1);
	goto L903;
L905:
	i += 1; if (i <= msysc_ncmdparams) goto L901;
L904:
	msysc_m$print_startcon();
	R1 = tou64("Usage:");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	msysc_m$print_startcon();
	R1 = tou64("      #        filename[.mx]      Load and run .mx program");
	msysc_m$print_setfmt(asu64(R1));
	asu64(R1) = msysc_cmdparams;
	R2 = 0;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	msysc_m$print_startcon();
	R1 = tou64("      #  -show filename[.mx/.ml]  Load and display mx/ml file");
	msysc_m$print_setfmt(asu64(R1));
	asu64(R1) = msysc_cmdparams;
	R2 = 0;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	R1 = 1;
	exit(R1);
L903:
	msysc_m$print_startcon();
	R1 = tou64("$CMDSKIP=");
	msysc_m$print_str_nf(asu64(R1));
	asi64(R1) = msysc_$cmdskip;
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	msysc_m$print_startcon();
	R1 = tou64("CMDSKIP=");
	msysc_m$print_str_nf(asu64(R1));
	asi64(R1) = cmdskip;
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	runmxshow_initlogfile();
	asu64(R1) = filename;
	asu64(R1) = mx_lib_loadmx(asu64(R1));
	plib = asu64(R1);
	asu64(R1) = plib;
	mx_lib_fixuplib(asu64(R1));
	asi64(R1) = dorun;
	if (!asi64(R1)) goto L910;
	asi64(R1) = cmdskip;
	asu64(R2) = plib;
	mx_lib_runprogram(asu64(R2), asi64(R1));
	goto L909;
L910:
	runmxshow_showlibs();
	runmxshow_closelogfile();
L909:
	R1 = 0;
	exit(R1);
	return;
}

static void runmxshow_initlogfile() {
    u64 R1, R2; 
	asi64(R1) = runmxshow_logdest;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L913;
	R2 = 0;
	if (asi64(R1) == asi64(R2)) goto L914;
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L914;
	goto L915;
L913:
	R1 = tou64("rx.log");
	asi32(R1) = remove(asu64(R1));
	R1 = tou64("w");
	R2 = tou64("rx.log");
	asu64(R1) = fopen(asu64(R2), asu64(R1));
	runmxshow_logdev = asu64(R1);
	goto L912;
L914:
	R1 = 0;
	runmxshow_logdev = asu64(R1);
	goto L912;
L915:
L912:
	return;
}

static void runmxshow_closelogfile() {
    u64 R1, R2, R3; 
	struct $B31 str;
	asi64(R1) = runmxshow_logdest;
	R2 = 2;
	if (asi64(R1) != asi64(R2)) goto L918;
	asu64(R1) = runmxshow_logdev;
	asi32(R1) = fclose(asu64(R1));
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("\\m\\ed.bat\"");
	msysc_m$print_str_nf(asu64(R1));
	R1 = tou64("rx.log");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_end();
	R1 = 0;
	R2 = 0;
	R3 = (u64)&str;
	asi64(R1) = mwindows_os_execwait(asu64(R3), asi64(R2), asu64(R1));
L918:
	return;
}

static void runmxshow_showlibs() {
    u64 R1, R2, R3; 
	i64 i;
	asu64(R1) = runmxshow_logdev;
	runmxshow_showglobals(asu64(R1));
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = mx_decls_nlibs;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L922;
L920:
	asu64(R1) = runmxshow_logdev;
	R2 = (u64)&mx_decls_libtable;
	asi64(R3) = i;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8-8));
	runmxshow_showlib(asu64(R2), asu64(R1));
	i += 1; if (i <= mx_decls_nlibs) goto L920;
L922:
	return;
}

static void runmxshow_showlib(u64 lib, u64 logdev) {
    u64 R1, R2, R3, R4; 
	struct $B15 str;
	u64 sig;
	i64 dir;
	i64 n;
	u64 q;
	u64 names;
	i64 i;
	asu64(R1) = runmxshow_dest;
	mlib_gs_init(asu64(R1));
	R1 = tou64("-------------------------");
	runmxshow_showstrln(asu64(R1));
	R1 = tou64("LIBFILE: ");
	runmxshow_showstr(asu64(R1));
	asu64(R1) = lib;
	R2 = 192;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	runmxshow_showstr(asu64(R1));
	R1 = tou64(" ");
	runmxshow_showstr(asu64(R1));
	asu64(R1) = lib;
	R2 = 184;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	runmxshow_showstrln(asu64(R1));
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("Version:");
	msysc_m$print_str_nf(asu64(R1));
	asu64(R1) = lib;
	R2 = 0;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	R1 = tou64("");
	runmxshow_showstrln(asu64(R1));
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("Zdata size: # #");
	msysc_m$print_setfmt(asu64(R1));
	asu64(R1) = lib;
	R2 = 24;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	msysc_m$print_i64_nf(asi64(R1));
	asu64(R1) = lib;
	R2 = 152;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	msysc_m$print_ptr_nf(asu64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("Idata size: # #");
	msysc_m$print_setfmt(asu64(R1));
	asu64(R1) = lib;
	R2 = 16;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	msysc_m$print_i64_nf(asi64(R1));
	asu64(R1) = lib;
	R2 = 80;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	msysc_m$print_ptr_nf(asu64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	asu64(R1) = lib;
	R2 = 16;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	asu64(R2) = lib;
	R3 = 80;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	runmxshow_showsectiondata(asu64(R2), asi64(R1));
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("Code size: # # Extra:#");
	msysc_m$print_setfmt(asu64(R1));
	asu64(R1) = lib;
	R2 = 8;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	msysc_m$print_i64_nf(asi64(R1));
	asu64(R1) = lib;
	R2 = 72;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	msysc_m$print_ptr_nf(asu64(R1));
	asu64(R1) = lib;
	R2 = 160;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	asu64(R1) = lib;
	R2 = 160;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	asu64(R2) = lib;
	R3 = 8;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3));
	asu64(R3) = lib;
	R4 = 72;
	asu64(R3) = *tou64p(((i64)R3+(i64)R4));
	runmxshow_showsectioncode(asu64(R3), asi64(R2), asi64(R1));
	asu64(R1) = lib;
	runmxshow_showrelocs(asu64(R1));
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("DLL Libs #");
	msysc_m$print_setfmt(asu64(R1));
	asu64(R1) = lib;
	R2 = 40;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	R2 = R1;
	n = asi64(R2);
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	asi64(R1) = n;
	asu64(R2) = lib;
	R3 = 96;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	runmxshow_shownames(asu64(R2), asi64(R1));
	R1 = tou64("");
	runmxshow_showstrln(asu64(R1));
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("Libs #");
	msysc_m$print_setfmt(asu64(R1));
	asu64(R1) = lib;
	R2 = 48;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	R2 = R1;
	n = asi64(R2);
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	asi64(R1) = n;
	asu64(R2) = lib;
	R3 = 104;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	runmxshow_shownames(asu64(R2), asi64(R1));
	R1 = tou64("");
	runmxshow_showstrln(asu64(R1));
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("Imports #");
	msysc_m$print_setfmt(asu64(R1));
	asu64(R1) = lib;
	R2 = 56;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	R2 = R1;
	n = asi64(R2);
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	asu64(R1) = lib;
	R2 = 112;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	names = asu64(R1);
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = n;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L926;
L924:
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("   #: #");
	msysc_m$print_setfmt(asu64(R1));
	asi64(R1) = i;
	msysc_m$print_i64_nf(asi64(R1));
	R1 = tou64("20jl");
	asu64(R2) = names;
	asi64(R3) = i;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8-8));
	msysc_m$print_str(asu64(R2), asu64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	i += 1; if (i <= n) goto L924;
L926:
	R1 = tou64("");
	runmxshow_showstrln(asu64(R1));
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("Exports #");
	msysc_m$print_setfmt(asu64(R1));
	asu64(R1) = lib;
	R2 = 64;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	R2 = R1;
	n = asi64(R2);
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	asu64(R1) = lib;
	R2 = 120;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	names = asu64(R1);
	R1 = tou64("     Name                 Seg      Offset");
	runmxshow_showstrln(asu64(R1));
	R1 = tou64("--------------------------------------------");
	runmxshow_showstrln(asu64(R1));
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = n;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L929;
L927:
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("#: # # #");
	msysc_m$print_setfmt(asu64(R1));
	R1 = tou64("3");
	asi64(R2) = i;
	msysc_m$print_i64(asi64(R2), asu64(R1));
	R1 = tou64("20jl");
	asu64(R2) = names;
	asi64(R3) = i;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8-8));
	msysc_m$print_str(asu64(R2), asu64(R1));
	R1 = tou64("8jl");
	R2 = (u64)&mx_lib_rsegmentnames;
	asu64(R3) = lib;
	R4 = 128;
	asu64(R3) = *tou64p(((i64)R3+(i64)R4));
	asi64(R4) = i;
	asu8(R3) = *tou8p(((i64)R3+(i64)R4-1));
	R3 = toi64(tou8(R3));
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8));
	msysc_m$print_str(asu64(R2), asu64(R1));
	R1 = tou64("8zh");
	asu64(R2) = lib;
	R3 = 136;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asi64(R3) = i;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8-8));
	msysc_m$print_u64(asu64(R2), asu64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	i += 1; if (i <= n) goto L927;
L929:
	R1 = tou64("");
	runmxshow_showstrln(asu64(R1));
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("Entry point offset:  #");
	msysc_m$print_setfmt(asu64(R1));
	R1 = 0;
	asu64(R2) = lib;
	R3 = 144;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	msysc_m$print_u64(asu64(R2), asi64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("Entry point address: #");
	msysc_m$print_setfmt(asu64(R1));
	asu64(R1) = lib;
	R2 = 200;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	msysc_m$print_ptr_nf(asu64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	R1 = tou64("");
	runmxshow_showstrln(asu64(R1));
// runmxshow.showlib.finish:
	asu64(R1) = logdev;
	asu64(R2) = runmxshow_dest;
	mlib_gs_println(asu64(R2), asu64(R1));
	return;
}

static void runmxshow_showstr(u64 str) {
    u64 R1, R2; 
	asu64(R1) = str;
	asu64(R2) = runmxshow_dest;
	mlib_gs_str(asu64(R2), asu64(R1));
	return;
}

static void runmxshow_showstrln(u64 str) {
    u64 R1, R2; 
	asu64(R1) = str;
	asu64(R2) = runmxshow_dest;
	mlib_gs_strln(asu64(R2), asu64(R1));
	return;
}

static void runmxshow_showstrint(i64 a) {
    u64 R1, R2; 
	asi64(R1) = a;
	asu64(R2) = runmxshow_dest;
	mlib_gs_strint(asu64(R2), asi64(R1));
	return;
}

static void runmxshow_shownames(u64 names, i64 n) {
    u64 R1, R2; 
	struct $B15 str;
	i64 i;
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = n;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L937;
L935:
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("   #: #");
	msysc_m$print_setfmt(asu64(R1));
	asi64(R1) = i;
	msysc_m$print_i64_nf(asi64(R1));
	asu64(R1) = names;
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	i += 1; if (i <= n) goto L935;
L937:
	return;
}

static void runmxshow_showrelocs(u64 lib) {
    u64 R1, R2, R3, R4; 
	struct $B15 str;
	u64 r;
	i64 n;
	i64 m;
	u64 targetoffset;
	u64 baseptr64;
	i64 i;
	asu64(R1) = lib;
	R2 = 32;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	n = asi64(R1);
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("Relocs #");
	msysc_m$print_setfmt(asu64(R1));
	asu64(R1) = lib;
	R2 = 32;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	R2 = R1;
	n = asi64(R2);
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	R1 = tou64("     Type       Seg      Offset    Symbol/Target+Offset");
	runmxshow_showstrln(asu64(R1));
	R1 = tou64("---------------------------------------------------------");
	runmxshow_showstrln(asu64(R1));
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = n;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L941;
L939:
	asu64(R1) = lib;
	R2 = 88;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	r = asu64(R1);
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("#: # # ##");
	msysc_m$print_setfmt(asu64(R1));
	R1 = tou64("3");
	asi64(R2) = i;
	msysc_m$print_i64(asi64(R2), asu64(R1));
	R1 = tou64("10jl");
	R2 = (u64)&mx_decls_mcxrelocnames;
	R3 = (u64)&r;
	R4 = 7;
	asu8(R3) = *tou8p(((i64)R3+(i64)R4));
	R3 = toi64(tou8(R3));
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8));
	msysc_m$print_str(asu64(R2), asu64(R1));
	R1 = tou64("8jl");
	R2 = (u64)&mx_lib_rsegmentnames;
	R3 = (u64)&r;
	R4 = 6;
	asu8(R3) = *tou8p(((i64)R3+(i64)R4));
	R3 = toi64(tou8(R3));
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8));
	msysc_m$print_str(asu64(R2), asu64(R1));
	R1 = tou64("8zh");
	R2 = (u64)&r;
	R3 = 0;
	asu32(R2) = *tou32p(((i64)R2+(i64)R3));
	R2 = toi64(tou32(R2));
	msysc_m$print_i64(asi64(R2), asu64(R1));
	msysc_m$print_nogap();
	R1 = tou64("  ");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	asi64(R1) = strlen(asu64(R1));
	m = asi64(R1);
	R1 = (u64)&r;
	R2 = 7;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L943;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L943;
	goto L944;
L943:
	R1 = (u64)&r;
	R2 = 6;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L946;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L947;
	goto L948;
L946:
	asu64(R1) = lib;
	R2 = 72;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	R2 = (u64)&r;
	R3 = 0;
	asu32(R2) = *tou32p(((i64)R2+(i64)R3));
	R2 = toi64(tou32(R2));
	R1 += (i64)R2;
	baseptr64 = asu64(R1);
	goto L945;
L947:
	asu64(R1) = lib;
	R2 = 80;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	R2 = (u64)&r;
	R3 = 0;
	asu32(R2) = *tou32p(((i64)R2+(i64)R3));
	R2 = toi64(tou32(R2));
	R1 += (i64)R2;
	baseptr64 = asu64(R1);
	goto L945;
L948:
L945:
	R1 = (u64)&r;
	R2 = 7;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	R2 = 1;
	if (asi64(R1) != asi64(R2)) goto L950;
	asu64(R1) = baseptr64;
	asu32(R1) = *tou32p(R1);
	R1 = tou64(tou32(R1));
	targetoffset = asu64(R1);
	goto L949;
L950:
	asu64(R1) = baseptr64;
	asu64(R1) = *tou64p(R1);
	targetoffset = asu64(R1);
L949:
	R1 = (u64)&str;
	asi64(R2) = m;
	R1 += (i64)R2;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("6jlt:");
	R2 = (u64)&mx_lib_rsegmentnames;
	R3 = (u64)&r;
	R4 = 4;
	asu8(R3) = *tou8p(((i64)R3+(i64)R4));
	R3 = toi64(tou8(R3));
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8));
	msysc_m$print_str(asu64(R2), asu64(R1));
	msysc_m$print_nogap();
	R1 = tou64("8zh");
	asu64(R2) = targetoffset;
	msysc_m$print_u64(asu64(R2), asu64(R1));
	msysc_m$print_end();
	goto L942;
L944:
	R1 = (u64)&str;
	asi64(R2) = m;
	R1 += (i64)R2;
	msysc_m$print_startstr(asu64(R1));
	asu64(R1) = lib;
	R2 = 112;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	R2 = (u64)&r;
	R3 = 4;
	asu16(R2) = *tou16p(((i64)R2+(i64)R3));
	R2 = toi64(tou16(R2));
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_end();
L942:
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	i += 1; if (i <= n) goto L939;
L941:
	R1 = tou64("");
	runmxshow_showstrln(asu64(R1));
	return;
}

static void runmxshow_showsectiondata(u64 p, i64 length) {
    u64 R1, R2, R3; 
	i64 i;
	i64 k;
	i64 bb;
	struct $B24 str;
	struct $B24 str2;
	u64 baseaddr;
	i64 av_1;
	R1 = tou64("proc Section ");
	runmxshow_showstr(asu64(R1));
	R1 = tou64("Idata:");
	runmxshow_showstr(asu64(R1));
	R1 = tou64(" Size:");
	runmxshow_showstr(asu64(R1));
	asi64(R1) = length;
	runmxshow_showstrint(asi64(R1));
	asu64(R1) = runmxshow_dest;
	mlib_gs_line(asu64(R1));
	asu64(R1) = runmxshow_dest;
	mlib_gs_line(asu64(R1));
	R1 = 0;
	k = asi64(R1);
	R1 = 0;
	R2 = (u64)&str;
	R3 = 1;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	R1 = 0;
	baseaddr = asu64(R1);
	R1 = (u64)&str2;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("Z8H");
	asu64(R2) = baseaddr;
	msysc_m$print_ptr(asu64(R2), asu64(R1));
	msysc_m$print_nogap();
	R1 = tou64(": ");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_end();
	R1 = (u64)&str2;
	runmxshow_showstr(asu64(R1));
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = length;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L954;
L952:
	R1 = (u64)&p;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	bb = asi64(R1);
	R1 = (u64)&str2;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("z2H");
	asi64(R2) = bb;
	msysc_m$print_i64(asi64(R2), asu64(R1));
	msysc_m$print_nogap();
	R1 = tou64(" ");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_end();
	R1 = (u64)&str2;
	runmxshow_showstr(asu64(R1));
	R1 = 32;
	asi64(R2) = bb;
    {u64 temp = R1; R1 = R2; R2 = temp;}
	if (asi64(R1) < asi64(R2)) goto L956;
	R2 = 127;
	if (asi64(R1) > asi64(R2)) goto L956;
	asi64(R1) = bb;
	R2 = (u64)&str2;
	R3 = 1;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	R1 = 0;
	R2 = (u64)&str2;
	R3 = 2;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	R1 = (u64)&str2;
	R2 = (u64)&str;
	asu64(R1) = strcat(asu64(R2), asu64(R1));
	goto L955;
L956:
	R1 = tou64(".");
	R2 = (u64)&str;
	asu64(R1) = strcat(asu64(R2), asu64(R1));
L955:
	R1 = (u64)&k;
	asi64(R1) = *(toi64p(R1)) += 1;
	R2 = 16;
	if (asi64(R1) == asi64(R2)) goto L959;
	asi64(R1) = i;
	asi64(R2) = length;
	if (asi64(R1) != asi64(R2)) goto L958;
L959:
	asi64(R1) = k;
	R2 = 16;
	if (asi64(R1) >= asi64(R2)) goto L961;
	R1 = 16;
	asi64(R2) = k;
	asi64(R1) -= asi64(R2);
	av_1 = asi64(R1);
	asi64(R1) = av_1;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L964;
L962:
	R1 = tou64("   ");
	runmxshow_showstr(asu64(R1));
	R1 = tou64(" ");
	R2 = (u64)&str;
	asu64(R1) = strcat(asu64(R2), asu64(R1));
	if (--asi64(av_1)) goto L962;
L964:
L961:
	R1 = tou64("\t[");
	runmxshow_showstr(asu64(R1));
	R1 = (u64)&str;
	runmxshow_showstr(asu64(R1));
	R1 = tou64("]");
	runmxshow_showstrln(asu64(R1));
	R1 = 0;
	k = asi64(R1);
	R1 = 0;
	R2 = (u64)&str;
	R3 = 1;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	R1 = 16;
	R2 = (u64)&baseaddr;
	*tou64p(R2) += asu64(R1);
	R1 = (u64)&str2;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("z8h");
	asu64(R2) = baseaddr;
	msysc_m$print_ptr(asu64(R2), asu64(R1));
	msysc_m$print_nogap();
	R1 = tou64(": ");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_end();
	R1 = (u64)&str2;
	runmxshow_showstr(asu64(R1));
L958:
	i += 1; if (i <= length) goto L952;
L954:
	asi64(R1) = k;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L966;
	asu64(R1) = runmxshow_dest;
	mlib_gs_line(asu64(R1));
L966:
	asu64(R1) = runmxshow_dest;
	mlib_gs_line(asu64(R1));
	asi64(R1) = k;
	if (!asi64(R1)) goto L968;
	asu64(R1) = runmxshow_dest;
	mlib_gs_line(asu64(R1));
L968:
	return;
}

static void runmxshow_showsectioncode(u64 p, i64 length, i64 extra) {
    u64 R1, R2; 
	u64 codeptr;
	u64 codeend;
	u64 codeendx;
	u64 codestart;
	i64 offset;
	u64 s;
	struct $B3 str;
	u64 baseaddr;
	R1 = tou64("proc Section Code");
	runmxshow_showstrln(asu64(R1));
	asu64(R1) = p;
	R2 = R1;
	codeptr = asu64(R2);
	codestart = asu64(R1);
	asu64(R1) = codeptr;
	asi64(R2) = length;
	R1 += (i64)R2;
	codeend = asu64(R1);
	asu64(R1) = codeend;
	asi64(R2) = extra;
	R1 += (i64)R2;
	codeendx = asu64(R1);
	R1 = 0;
	baseaddr = asu64(R1);
	goto L971;
L970:
	asu64(R1) = codeptr;
	asu64(R2) = codeend;
	if (asu64(R1) != asu64(R2)) goto L974;
	R1 = tou64("");
	runmxshow_showstrln(asu64(R1));
L974:
	asu64(R1) = codeptr;
	asu64(R2) = codestart;
	asi64(R1) -= asi64(R2);
	offset = asi64(R1);
	asu64(R1) = baseaddr;
	asi64(R2) = offset;
	R1 += (i64)R2;
	R2 = (u64)&codeptr;
	asu64(R1) = mc_disasm_decodeinstr(asu64(R2), asu64(R1));
	s = asu64(R1);
	asu64(R1) = s;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L976;
	goto L972;
L976:
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("4");
	asi64(R2) = offset;
	msysc_m$print_i64(asi64(R2), asu64(R1));
	msysc_m$print_nogap();
	R1 = tou64(" ");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstr(asu64(R1));
	asu64(R1) = s;
	runmxshow_showstrln(asu64(R1));
L971:
	asu64(R1) = codeptr;
	asu64(R2) = codeendx;
	if (asu64(R1) < asu64(R2)) goto L970;
L972:
	asu64(R1) = runmxshow_dest;
	mlib_gs_line(asu64(R1));
	return;
}

static void runmxshow_showglobals(u64 logdev) {
    u64 R1, R2, R3, R4; 
	struct $B15 str;
	struct $B15 name;
	i64 i;
	asu64(R1) = runmxshow_dest;
	mlib_gs_init(asu64(R1));
	R1 = tou64("Global Tables\n");
	runmxshow_showstrln(asu64(R1));
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("DLLs:");
	msysc_m$print_str_nf(asu64(R1));
	asi64(R1) = mx_decls_ndlllibs;
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = mx_decls_ndlllibs;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L980;
L978:
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	asi64(R1) = i;
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_nogap();
	R1 = tou64(":");
	msysc_m$print_str_nf(asu64(R1));
	R1 = tou64("16jl");
	R2 = (u64)&mx_decls_dllnametable;
	asi64(R3) = i;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8-8));
	msysc_m$print_str(asu64(R2), asu64(R1));
	R1 = tou64("h");
	R2 = (u64)&mx_decls_dllinsttable;
	asi64(R3) = i;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8-8));
	msysc_m$print_u64(asu64(R2), asu64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	i += 1; if (i <= mx_decls_ndlllibs) goto L978;
L980:
	R1 = tou64("");
	runmxshow_showstrln(asu64(R1));
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("LIBs:");
	msysc_m$print_str_nf(asu64(R1));
	asi64(R1) = mx_decls_nlibs;
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = mx_decls_nlibs;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L983;
L981:
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	asi64(R1) = i;
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_nogap();
	R1 = tou64(":");
	msysc_m$print_str_nf(asu64(R1));
	R1 = tou64("20jl");
	R2 = (u64)&mx_decls_libnametable;
	asi64(R3) = i;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8-8));
	msysc_m$print_str(asu64(R2), asu64(R1));
	R1 = (u64)&mx_decls_librelocated;
	asi64(R2) = i;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2-1));
	if (!asu8(R1)) goto L985;
	R1 = tou64("Relocated");
	goto L984;
L985:
	R1 = tou64("-");
L984:
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	i += 1; if (i <= mx_decls_nlibs) goto L981;
L983:
	R1 = tou64("");
	runmxshow_showstrln(asu64(R1));
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("Global Symbols:");
	msysc_m$print_str_nf(asu64(R1));
	asi64(R1) = mx_decls_nsymbols;
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	R1 = tou64("     Name              Def Address       Lib        Dll");
	runmxshow_showstrln(asu64(R1));
	R1 = tou64("-----------------------------------------------------------");
	runmxshow_showstrln(asu64(R1));
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = mx_decls_nsymbols;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L988;
L986:
	R1 = (u64)&mx_decls_symbolnametable;
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	R2 = (u64)&name;
	asu64(R1) = strcpy(asu64(R2), asu64(R1));
	R1 = (u64)&name;
	asi64(R1) = strlen(asu64(R1));
	R2 = 17;
	if (asi64(R1) <= asi64(R2)) goto L990;
	R1 = tou64("\n                      ");
	R2 = (u64)&name;
	asu64(R1) = strcat(asu64(R2), asu64(R1));
L990:
	R1 = (u64)&str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("#: # # #  # #");
	msysc_m$print_setfmt(asu64(R1));
	R1 = tou64("3");
	asi64(R2) = i;
	msysc_m$print_i64(asi64(R2), asu64(R1));
	R1 = tou64("17jl");
	R2 = (u64)&name;
	msysc_m$print_str(asu64(R2), asu64(R1));
	R1 = tou64("3JL");
	R2 = (u64)&mx_decls_symboldefined;
	asi64(R3) = i;
	asu8(R2) = *tou8p(((i64)R2+(i64)R3-1));
	if (!asu8(R2)) goto L992;
	R2 = tou64("Y");
	goto L991;
L992:
	R2 = tou64("-");
L991:
	msysc_m$print_str(asu64(R2), asu64(R1));
	R1 = tou64("Z12H");
	R2 = (u64)&mx_decls_symboladdress;
	asi64(R3) = i;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8-8));
	msysc_m$print_ptr(asu64(R2), asu64(R1));
	R1 = tou64("10jl");
	R2 = (u64)&mx_decls_symbollibindex;
	asi64(R3) = i;
	asi16(R2) = *toi16p(((i64)R2+(i64)R3*2-2));
	if (!asi16(R2)) goto L994;
	R2 = (u64)&mx_decls_libnametable;
	R3 = (u64)&mx_decls_symbollibindex;
	asi64(R4) = i;
	asi16(R3) = *toi16p(((i64)R3+(i64)R4*2-2));
	R3 = toi64(toi16(R3));
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8-8));
	goto L993;
L994:
	R2 = tou64("-");
L993:
	msysc_m$print_str(asu64(R2), asu64(R1));
	R1 = tou64("10jl");
	R2 = (u64)&mx_decls_symboldllindex;
	asi64(R3) = i;
	asu8(R2) = *tou8p(((i64)R2+(i64)R3-1));
	if (!asu8(R2)) goto L996;
	R2 = (u64)&mx_decls_dllnametable;
	R3 = (u64)&mx_decls_symboldllindex;
	asi64(R4) = i;
	asu8(R3) = *tou8p(((i64)R3+(i64)R4-1));
	R3 = toi64(tou8(R3));
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8-8));
	goto L995;
L996:
	R2 = tou64("-");
L995:
	msysc_m$print_str(asu64(R2), asu64(R1));
	msysc_m$print_end();
	R1 = (u64)&str;
	runmxshow_showstrln(asu64(R1));
	i += 1; if (i <= mx_decls_nsymbols) goto L986;
L988:
	R1 = tou64("");
	runmxshow_showstrln(asu64(R1));
	asu64(R1) = logdev;
	asu64(R2) = runmxshow_dest;
	mlib_gs_println(asu64(R2), asu64(R1));
	return;
}

static u64 mx_lib_readlibfile(u64 filespec, u64 p) {
    u64 R1, R2, R3, R4; 
	u64 plib;
	struct $B27 lib;
	u64 sig;
	i64 dir;
	i64 n;
	i64 tablesize;
	u64 q;
	i64 i;
	R1 = (u64)&lib;
	memset(R1, 0, 216);
	R1 = (u64)&p;
	asu64(R1) = mx_lib_readu32(asu64(R1));
	sig = asu64(R1);
	asu64(R1) = sig;
	R2 = 441992013;
	if (asu64(R1) == asu64(R2)) goto L999;
	msysc_m$print_startcon();
	R1 = tou64("Bad sig - not MCX file");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	R1 = 1;
	exit(R1);
L999:
	asu64(R1) = filespec;
	asu64(R1) = mlib_pcm_copyheapstring(asu64(R1));
	R2 = (u64)&lib;
	R3 = 184;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	asu64(R1) = filespec;
	asu64(R1) = mlib_extractbasefile(asu64(R1));
	asu64(R1) = mlib_pcm_copyheapstring(asu64(R1));
	R2 = (u64)&lib;
	R3 = 192;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
L1000:
	R1 = (u64)&p;
	asi64(R1) = mx_lib_readbyte(asu64(R1));
	R2 = R1;
	dir = asi64(R2);
	switch (asi64(R1)) {
	case 0: goto L1035;
	case 1: goto L1004;
	case 2: goto L1007;
	case 3: goto L1006;
	case 4: goto L1005;
	case 5: goto L1032;
	case 6: goto L1008;
	case 7: goto L1012;
	case 8: goto L1016;
	case 9: goto L1020;
	case 10: goto L1024;
	case 11: goto L1028;
	case 12: goto L1033;
	case 13: goto L1034;
	default: goto L1003;
    };
// SWITCH
L1004:
	R1 = (u64)&p;
	asu64(R1) = mx_lib_readstring(asu64(R1));
	R2 = (u64)&lib;
	R3 = 0;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	goto L1000;
L1005:
	R1 = (u64)&p;
	asi64(R1) = mx_lib_readu32(asu64(R1));
	R2 = (u64)&lib;
	R3 = 24;
	*toi64p(((i64)R2+(i64)R3)) = asi64(R1);
	goto L1000;
L1006:
	R1 = (u64)&p;
	asi64(R1) = mx_lib_readu32(asu64(R1));
	R2 = R1;
	n = asi64(R2);
	R2 = (u64)&lib;
	R3 = 16;
	*toi64p(((i64)R2+(i64)R3)) = asi64(R1);
	asi64(R1) = n;
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	R2 = (u64)&lib;
	R3 = 80;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	asi64(R1) = n;
	asu64(R2) = p;
	R3 = (u64)&lib;
	R4 = 80;
	asu64(R3) = *tou64p(((i64)R3+(i64)R4));
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	asi64(R1) = n;
	R2 = (u64)&p;
	*tou64p(R2) += asu64(R1);
	goto L1000;
L1007:
	R1 = (u64)&p;
	asi64(R1) = mx_lib_readu32(asu64(R1));
	R2 = R1;
	n = asi64(R2);
	R2 = (u64)&lib;
	R3 = 8;
	*toi64p(((i64)R2+(i64)R3)) = asi64(R1);
	asu64(R1) = p;
	R2 = (u64)&lib;
	R3 = 72;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	asi64(R1) = n;
	R2 = (u64)&p;
	*tou64p(R2) += asu64(R1);
	goto L1000;
L1008:
	R1 = (u64)&p;
	asi64(R1) = mx_lib_readu32(asu64(R1));
	R2 = R1;
	n = asi64(R2);
	R2 = (u64)&lib;
	R3 = 40;
	*toi64p(((i64)R2+(i64)R3)) = asi64(R1);
	R1 = 8;
	asi64(R2) = n;
	asi64(R1) *= asi64(R2);
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	R2 = (u64)&lib;
	R3 = 96;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = n;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1011;
L1009:
	R1 = (u64)&p;
	asu64(R1) = mx_lib_readstring(asu64(R1));
	R2 = (u64)&lib;
	R3 = 96;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asi64(R3) = i;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
	i += 1; if (i <= n) goto L1009;
L1011:
	goto L1000;
L1012:
	R1 = (u64)&p;
	asi64(R1) = mx_lib_readu32(asu64(R1));
	R2 = R1;
	n = asi64(R2);
	R2 = (u64)&lib;
	R3 = 48;
	*toi64p(((i64)R2+(i64)R3)) = asi64(R1);
	R1 = 8;
	asi64(R2) = n;
	asi64(R1) *= asi64(R2);
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	R2 = (u64)&lib;
	R3 = 104;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = n;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1015;
L1013:
	R1 = (u64)&p;
	asu64(R1) = mx_lib_readstring(asu64(R1));
	R2 = (u64)&lib;
	R3 = 104;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asi64(R3) = i;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
	i += 1; if (i <= n) goto L1013;
L1015:
	goto L1000;
L1016:
	R1 = (u64)&p;
	asi64(R1) = mx_lib_readu32(asu64(R1));
	R2 = R1;
	n = asi64(R2);
	R2 = (u64)&lib;
	R3 = 56;
	*toi64p(((i64)R2+(i64)R3)) = asi64(R1);
	R1 = 8;
	asi64(R2) = n;
	asi64(R1) *= asi64(R2);
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	R2 = (u64)&lib;
	R3 = 112;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = n;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1019;
L1017:
	R1 = (u64)&p;
	asu64(R1) = mx_lib_readstring(asu64(R1));
	R2 = (u64)&lib;
	R3 = 112;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asi64(R3) = i;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
	i += 1; if (i <= n) goto L1017;
L1019:
	goto L1000;
L1020:
	R1 = (u64)&p;
	asi64(R1) = mx_lib_readu32(asu64(R1));
	R2 = R1;
	n = asi64(R2);
	R2 = (u64)&lib;
	R3 = 64;
	*toi64p(((i64)R2+(i64)R3)) = asi64(R1);
	R1 = 8;
	asi64(R2) = n;
	asi64(R1) *= asi64(R2);
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	R2 = (u64)&lib;
	R3 = 120;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = n;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1023;
L1021:
	R1 = (u64)&p;
	asu64(R1) = mx_lib_readstring(asu64(R1));
	R2 = (u64)&lib;
	R3 = 120;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asi64(R3) = i;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
	i += 1; if (i <= n) goto L1021;
L1023:
	goto L1000;
L1024:
	R1 = (u64)&p;
	asi64(R1) = mx_lib_readu32(asu64(R1));
	n = asi64(R1);
	asi64(R1) = n;
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	R2 = (u64)&lib;
	R3 = 128;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = n;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1027;
L1025:
	R1 = (u64)&p;
	asi64(R1) = mx_lib_readbyte(asu64(R1));
	R2 = (u64)&lib;
	R3 = 128;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asi64(R3) = i;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	i += 1; if (i <= n) goto L1025;
L1027:
	goto L1000;
L1028:
	R1 = (u64)&p;
	asi64(R1) = mx_lib_readu32(asu64(R1));
	n = asi64(R1);
	R1 = 8;
	asi64(R2) = n;
	asi64(R1) *= asi64(R2);
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	R2 = (u64)&lib;
	R3 = 136;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = n;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1031;
L1029:
	R1 = (u64)&p;
	asu64(R1) = mx_lib_readu32(asu64(R1));
	R2 = (u64)&lib;
	R3 = 136;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asi64(R3) = i;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
	i += 1; if (i <= n) goto L1029;
L1031:
	goto L1000;
L1032:
	R1 = (u64)&p;
	asi64(R1) = mx_lib_readu32(asu64(R1));
	R2 = R1;
	n = asi64(R2);
	R2 = (u64)&lib;
	R3 = 32;
	*toi64p(((i64)R2+(i64)R3)) = asi64(R1);
	R1 = (u64)&lib;
	R2 = 32;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	R2 = 8;
	asi64(R1) *= asi64(R2);
	n = asi64(R1);
	asi64(R1) = n;
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	R2 = (u64)&lib;
	R3 = 88;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	asi64(R1) = n;
	asu64(R2) = p;
	R3 = (u64)&lib;
	R4 = 88;
	asu64(R3) = *tou64p(((i64)R3+(i64)R4));
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	asi64(R1) = n;
	R2 = (u64)&p;
	*tou64p(R2) += asu64(R1);
	goto L1000;
L1033:
	R1 = (u64)&p;
	asu64(R1) = mx_lib_readu32(asu64(R1));
	R2 = (u64)&lib;
	R3 = 144;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	goto L1000;
L1034:
	goto L1001;
	goto L1000;
L1035:
	goto L1000;
L1003:
	msysc_m$print_startcon();
	R1 = tou64("Unknown directive:");
	msysc_m$print_str_nf(asu64(R1));
	R1 = (u64)&mx_decls_mcxdirnames;
	asi64(R2) = dir;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	R1 = 0;
	exit(R1);
	goto L1000;
L1001:
	R1 = 216;
	asu64(R1) = mlib_pcm_allocnfz(asi64(R1));
	plib = asu64(R1);
	R1 = 216;
	R2 = (u64)&lib;
	asu64(R3) = plib;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	asu64(R1) = plib;
	goto L997;
L997:
	return asu64(R1);
}

static i64 mx_lib_readbyte(u64 p) {
    u64 R1, R2; 
	asu64(R1) = p;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	goto L1036;
L1036:
	return asi64(R1);
}

static u64 mx_lib_readu32(u64 p) {
    u64 R1, R2; 
	u64 x;
	asu64(R1) = p;
	asu64(R1) = *tou64p(R1);
	asu32(R1) = *tou32p(R1);
	R1 = tou64(tou32(R1));
	x = asu64(R1);
	R1 = 4;
	asu64(R2) = p;
	*tou64p(R2) += asu64(R1);
	asu64(R1) = x;
	goto L1037;
L1037:
	return asu64(R1);
}

static u64 mx_lib_readstring(u64 p) {
    u64 R1; 
	u64 s;
	asu64(R1) = p;
	asu64(R1) = *tou64p(R1);
	asu64(R1) = mlib_pcm_copyheapstring(asu64(R1));
	s = asu64(R1);
	goto L1040;
L1039:
L1040:
	asu64(R1) = p;
	asu64(R1) = *(tou64p(R1)) += 1;
	asu8(R1) = *tou8p(R1);
	if (asu8(R1)) goto L1039;
	asu64(R1) = p;
	(*tou64p(R1)) += 1;
	asu64(R1) = s;
	goto L1038;
L1038:
	return asu64(R1);
}

static void mx_lib_alloclibdata(u64 lib) {
    u64 R1, R2, R3, R4; 
	i64 tablesize;
	i64 n;
	u64 p;
	asu64(R1) = lib;
	R2 = 24;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	asu64(R1) = mlib_pcm_allocz(asi64(R1));
	asu64(R2) = lib;
	R3 = 152;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	asu64(R1) = lib;
	R2 = 56;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	R2 = 16;
	asi64(R1) *= asi64(R2);
	tablesize = asi64(R1);
	asu64(R1) = lib;
	R2 = 8;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	n = asi64(R1);
	asi64(R1) = n;
	asi64(R2) = tablesize;
	asi64(R1) += asi64(R2);
	asu64(R1) = mwindows_os_allocexecmem(asi64(R1));
	p = asu64(R1);
	asu64(R1) = p;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L1044;
	R1 = tou64("");
	R2 = tou64("Can't alloc code memory");
	mx_lib_error(asu64(R2), asu64(R1));
L1044:
	asi64(R1) = n;
	asu64(R2) = lib;
	R3 = 72;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asu64(R3) = p;
	memcpy(asu64(R3), asu64(R2), asu64(R1));
	asi64(R1) = tablesize;
	R2 = 0;
	asu64(R3) = p;
	asi64(R4) = n;
	R3 += (i64)R4;
	memset(asu64(R3), asi32(R2), asu64(R1));
	asu64(R1) = p;
	asu64(R2) = lib;
	R3 = 72;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	asi64(R1) = tablesize;
	asu64(R2) = lib;
	R3 = 160;
	*toi64p(((i64)R2+(i64)R3)) = asi64(R1);
	R1 = 8;
	asu64(R2) = lib;
	R3 = 64;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3));
	asi64(R1) *= asi64(R2);
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	asu64(R2) = lib;
	R3 = 168;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	R1 = 2;
	asu64(R2) = lib;
	R3 = 56;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3));
	asi64(R1) *= asi64(R2);
	asu64(R1) = mlib_pcm_alloc(asi64(R1));
	asu64(R2) = lib;
	R3 = 176;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
	asu64(R1) = lib;
	R2 = 144;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	R2 = 4294967295;
	if (asi64(R1) == asi64(R2)) goto L1046;
	asu64(R1) = lib;
	R2 = 72;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asu64(R2) = lib;
	R3 = 144;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3));
	R1 += (i64)R2;
	asu64(R2) = lib;
	R3 = 200;
	*tou64p(((i64)R2+(i64)R3)) = asu64(R1);
L1046:
	return;
}

static void mx_lib_error(u64 mess, u64 param) {
    u64 R1; 
	asu64(R1) = param;
	asu8(R1) = *tou8p(R1);
	if (!asu8(R1)) goto L1049;
	msysc_m$print_startcon();
	asu64(R1) = mess;
	msysc_m$print_setfmt(asu64(R1));
	asu64(R1) = param;
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	goto L1048;
L1049:
	msysc_m$print_startcon();
	asu64(R1) = mess;
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
L1048:
	msysc_m$print_startcon();
	R1 = tou64("Aborting");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	R1 = 1;
	exit(R1);
	return;
}

static void mx_lib_loadmemmcu(u64 lib) {
    u64 R1, R2, R3; 
	i64 newlib;
	u64 name;
	asu64(R1) = lib;
	R2 = 192;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	name = asu64(R1);
	asu64(R1) = lib;
	R2 = 184;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asu64(R2) = name;
	mx_lib_checknew(asu64(R2), asu64(R1));
	asu64(R1) = name;
	asi64(R1) = mx_lib_mxaddlib(asu64(R1));
	newlib = asi64(R1);
	asu64(R1) = lib;
	R2 = (u64)&mx_decls_libtable;
	asi64(R3) = newlib;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
	asu64(R1) = lib;
	mx_lib_loadimports(asu64(R1));
	return;
}

static void mx_lib_checknew(u64 name, u64 filename) {
    u64 R1, R2; 
	asu64(R1) = name;
	asi64(R1) = mx_lib_findlib(asu64(R1));
	if (!asi64(R1)) goto L1053;
	asu64(R1) = filename;
	R2 = tou64("Lib already exists:");
	mx_lib_error(asu64(R2), asu64(R1));
L1053:
	return;
}

static i64 mx_lib_findlib(u64 name) {
    u64 R1, R2; 
	i64 n;
	i64 i;
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = mx_decls_nlibs;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1057;
L1055:
	R1 = (u64)&mx_decls_libnametable;
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = name;
	asi64(R1) = mlib_eqstring(asu64(R2), asu64(R1));
	if (!asi64(R1)) goto L1059;
	asi64(R1) = i;
	goto L1054;
L1059:
	i += 1; if (i <= mx_decls_nlibs) goto L1055;
L1057:
	R1 = 0;
	goto L1054;
L1054:
	return asi64(R1);
}

static i64 mx_lib_mxaddlib(u64 name) {
    u64 R1, R2, R3; 
	i64 n;
	asi64(R1) = mx_decls_nlibs;
	R2 = 20;
	if (asi64(R1) < asi64(R2)) goto L1062;
	R1 = tou64("");
	R2 = tou64("Too many libs");
	mx_lib_error(asu64(R2), asu64(R1));
L1062:
	asu64(R1) = name;
	R2 = (u64)&mx_decls_libnametable;
	R3 = (u64)&mx_decls_nlibs;
	asi64(R3) = *(toi64p(R3)) += 1;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
	asi64(R1) = mx_decls_nlibs;
	goto L1060;
L1060:
	return asi64(R1);
}

static void mx_lib_fixuplib(u64 lib) {
	mx_lib_loaddlls();
	mx_lib_checksymbols();
	mx_lib_dorelocations();
	return;
}

static void mx_lib_loaddlls() {
    u64 R1, R2, R3; 
	u64 inst;
	i64 i;
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = mx_decls_ndlllibs;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1067;
L1065:
	R1 = (u64)&mx_decls_dllinsttable;
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	if (asu64(R1)) goto L1069;
	R1 = (u64)&mx_decls_dllnametable;
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	asu64(R1) = mwindows_os_getdllinst(asu64(R1));
	inst = asu64(R1);
	asu64(R1) = inst;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L1071;
	R1 = (u64)&mx_decls_dllnametable;
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	R2 = tou64("Can't find DLL: #");
	mx_lib_error(asu64(R2), asu64(R1));
L1071:
	asu64(R1) = inst;
	R2 = (u64)&mx_decls_dllinsttable;
	asi64(R3) = i;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
L1069:
	i += 1; if (i <= mx_decls_ndlllibs) goto L1065;
L1067:
	return;
}

static u64 mx_lib_finddllsymbol(u64 name, u64 dllindex) {
    u64 R1, R2, R3; 
	u64 p;
	i64 i;
	R1 = 0;
	asu64(R2) = dllindex;
	*toi64p(R2) = asi64(R1);
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = mx_decls_ndlllibs;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1075;
L1073:
	asu64(R1) = name;
	R2 = (u64)&mx_decls_dllinsttable;
	asi64(R3) = i;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3*8-8));
	asu64(R1) = mwindows_os_getdllprocaddr(asi64(R2), asu64(R1));
	p = asu64(R1);
	asu64(R1) = p;
	if (!asu64(R1)) goto L1077;
	asi64(R1) = i;
	asu64(R2) = dllindex;
	*toi64p(R2) = asi64(R1);
	asu64(R1) = p;
	goto L1072;
L1077:
	i += 1; if (i <= mx_decls_ndlllibs) goto L1073;
L1075:
	R1 = 0;
	goto L1072;
L1072:
	return asu64(R1);
}

static void mx_lib_checksymbols() {
    u64 R1, R2, R3; 
	i64 dllindex;
	i64 undef;
	u64 p;
	i64 i;
	R1 = 0;
	undef = asi64(R1);
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = mx_decls_nsymbols;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1081;
L1079:
	R1 = (u64)&mx_decls_symboldefined;
	asi64(R2) = i;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2-1));
	if (asu8(R1)) goto L1083;
	R1 = (u64)&dllindex;
	R2 = (u64)&mx_decls_symbolnametable;
	asi64(R3) = i;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8-8));
	asu64(R1) = mx_lib_finddllsymbol(asu64(R2), asu64(R1));
	p = asu64(R1);
	asu64(R1) = p;
	if (!asu64(R1)) goto L1085;
	asu64(R1) = p;
	R2 = (u64)&mx_decls_symboladdress;
	asi64(R3) = i;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
	asi64(R1) = dllindex;
	R2 = (u64)&mx_decls_symboldllindex;
	asi64(R3) = i;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	R1 = 1;
	R2 = (u64)&mx_decls_symboldefined;
	asi64(R3) = i;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	goto L1084;
L1085:
	msysc_m$print_startcon();
	R1 = tou64("Undef");
	msysc_m$print_str_nf(asu64(R1));
	R1 = (u64)&mx_decls_symbolnametable;
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	R1 = (u64)&undef;
	(*toi64p(R1)) += 1;
L1084:
L1083:
	i += 1; if (i <= mx_decls_nsymbols) goto L1079;
L1081:
	asi64(R1) = undef;
	if (!asi64(R1)) goto L1087;
L1087:
	return;
}

static void mx_lib_dorelocations() {
    u64 R1, R2; 
	i64 i;
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = mx_decls_nlibs;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1091;
L1089:
	R1 = (u64)&mx_decls_librelocated;
	asi64(R2) = i;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2-1));
	if (asu8(R1)) goto L1093;
	R1 = (u64)&mx_decls_libtable;
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	mx_lib_reloclib(asu64(R1));
L1093:
	i += 1; if (i <= mx_decls_nlibs) goto L1089;
L1091:
	return;
}

static void mx_lib_reloclib(u64 lib) {
    u64 R1, R2, R3, R4; 
	i64 index;
	i64 targetoffset;
	u64 name;
	u64 p;
	u64 q;
	u64 qaddr;
	u64 r;
	i64 av_1;
	i64 av_2;
	i64 i;
	asu64(R1) = lib;
	R2 = 72;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asu64(R2) = lib;
	R3 = 8;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3));
	R1 += (i64)R2;
	p = asu64(R1);
	asu64(R1) = p;
	asu64(R2) = lib;
	R3 = 56;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3));
	R3 = 8;
	asi64(R2) *= asi64(R3);
	R1 += (i64)R2;
	qaddr = asu64(R1);
	R1 = 1;
	i = asi64(R1);
	asu64(R1) = lib;
	R2 = 56;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	av_1 = asi64(R1);
	asi64(R1) = av_1;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1097;
L1095:
	asu64(R1) = lib;
	R2 = 112;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	name = asu64(R1);
	R1 = 72;
	R2 = (u64)&p;
	asu64(R3) = *tou64p(R2); *(tou64p(R2)) += 1; asu64(R2) = asu64(R3);
	*tou8p(R2) = asu8(R1);
	R1 = 255;
	R2 = (u64)&p;
	asu64(R3) = *tou64p(R2); *(tou64p(R2)) += 1; asu64(R2) = asu64(R3);
	*tou8p(R2) = asu8(R1);
	R1 = 36;
	R2 = (u64)&p;
	asu64(R3) = *tou64p(R2); *(tou64p(R2)) += 1; asu64(R2) = asu64(R3);
	*tou8p(R2) = asu8(R1);
	R1 = 37;
	R2 = (u64)&p;
	asu64(R3) = *tou64p(R2); *(tou64p(R2)) += 1; asu64(R2) = asu64(R3);
	*tou8p(R2) = asu8(R1);
	asu64(R1) = qaddr;
	asu64(R2) = p;
	*tou32p(R2) = asu32(R1);
	R1 = 4;
	R2 = (u64)&p;
	*tou64p(R2) += asu64(R1);
	asu64(R1) = lib;
	R2 = 176;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R2) = i;
	asi16(R1) = *toi16p(((i64)R1+(i64)R2*2-2));
	R1 = toi64(toi16(R1));
	index = asi64(R1);
	R1 = (u64)&mx_decls_symboladdress;
	asi64(R2) = index;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	R2 = (u64)&qaddr;
	asu64(R3) = *tou64p(R2); *(tou64p(R2)) += 8; asu64(R2) = asu64(R3);
	*tou64p(R2) = asu64(R1);
	i += 1; if (i <= av_1) goto L1095;
L1097:
	R1 = 1;
	i = asi64(R1);
	asu64(R1) = lib;
	R2 = 32;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	av_2 = asi64(R1);
	asi64(R1) = av_2;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1100;
L1098:
	asu64(R1) = lib;
	R2 = 88;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	r = asu64(R1);
	R1 = (u64)&r;
	R2 = 6;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L1102;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L1103;
	R2 = 3;
	if (asi64(R1) == asi64(R2)) goto L1104;
	goto L1105;
L1102:
	asu64(R1) = lib;
	R2 = 72;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	R2 = (u64)&r;
	R3 = 0;
	asu32(R2) = *tou32p(((i64)R2+(i64)R3));
	R2 = toi64(tou32(R2));
	R1 += (i64)R2;
	p = asu64(R1);
	goto L1101;
L1103:
	asu64(R1) = lib;
	R2 = 80;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	R2 = (u64)&r;
	R3 = 0;
	asu32(R2) = *tou32p(((i64)R2+(i64)R3));
	R2 = toi64(tou32(R2));
	R1 += (i64)R2;
	p = asu64(R1);
	goto L1101;
L1104:
	asu64(R1) = lib;
	R2 = 152;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	R2 = (u64)&r;
	R3 = 0;
	asu32(R2) = *tou32p(((i64)R2+(i64)R3));
	R2 = toi64(tou32(R2));
	R1 += (i64)R2;
	p = asu64(R1);
	goto L1101;
L1105:
L1101:
	R1 = (u64)&r;
	R2 = 7;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L1107;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L1108;
	R2 = 4;
	if (asi64(R1) == asi64(R2)) goto L1109;
	R2 = 3;
	if (asi64(R1) == asi64(R2)) goto L1110;
	R2 = 5;
	if (asi64(R1) == asi64(R2)) goto L1111;
	goto L1112;
L1107:
	asu64(R1) = p;
	asu32(R1) = *tou32p(R1);
	R1 = toi64(tou32(R1));
	targetoffset = asi64(R1);
	R1 = (u64)&r;
	R2 = 4;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L1114;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L1115;
	R2 = 3;
	if (asi64(R1) == asi64(R2)) goto L1116;
	goto L1117;
L1114:
	asu64(R1) = lib;
	R2 = 72;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R2) = targetoffset;
	R1 += (i64)R2;
	asu64(R2) = p;
	*tou32p(R2) = asu32(R1);
	goto L1113;
L1115:
	asu64(R1) = lib;
	R2 = 80;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R2) = targetoffset;
	R1 += (i64)R2;
	asu64(R2) = p;
	*tou32p(R2) = asu32(R1);
	goto L1113;
L1116:
	asu64(R1) = lib;
	R2 = 152;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R2) = targetoffset;
	R1 += (i64)R2;
	asu64(R2) = p;
	*tou32p(R2) = asu32(R1);
	goto L1113;
L1117:
L1113:
	goto L1106;
L1108:
	asu64(R1) = p;
	asu32(R1) = *tou32p(R1);
	R1 = toi64(tou32(R1));
	targetoffset = asi64(R1);
	R1 = (u64)&r;
	R2 = 4;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L1119;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L1120;
	R2 = 3;
	if (asi64(R1) == asi64(R2)) goto L1121;
	goto L1122;
L1119:
	asu64(R1) = lib;
	R2 = 72;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R2) = targetoffset;
	R1 += (i64)R2;
	asu64(R2) = p;
	*tou64p(R2) = asu64(R1);
	goto L1118;
L1120:
	asu64(R1) = lib;
	R2 = 80;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R2) = targetoffset;
	R1 += (i64)R2;
	asu64(R2) = p;
	*tou64p(R2) = asu64(R1);
	goto L1118;
L1121:
	asu64(R1) = lib;
	R2 = 152;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R2) = targetoffset;
	R1 += (i64)R2;
	asu64(R2) = p;
	*tou64p(R2) = asu64(R1);
	goto L1118;
L1122:
L1118:
	goto L1106;
L1109:
	asu64(R1) = lib;
	R2 = 176;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	R2 = (u64)&r;
	R3 = 4;
	asu16(R2) = *tou16p(((i64)R2+(i64)R3));
	R2 = toi64(tou16(R2));
	asi16(R1) = *toi16p(((i64)R1+(i64)R2*2-2));
	R1 = toi64(toi16(R1));
	index = asi64(R1);
	R1 = (u64)&mx_decls_symboladdress;
	asi64(R2) = index;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = p;
	*tou64p(R2) += asu64(R1);
	goto L1106;
L1110:
	asu64(R1) = lib;
	R2 = 176;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	R2 = (u64)&r;
	R3 = 4;
	asu16(R2) = *tou16p(((i64)R2+(i64)R3));
	R2 = toi64(tou16(R2));
	asi16(R1) = *toi16p(((i64)R1+(i64)R2*2-2));
	R1 = toi64(toi16(R1));
	index = asi64(R1);
	R1 = (u64)&mx_decls_symboladdress;
	asi64(R2) = index;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = p;
	*tou32p(R2) += asu32(R1);
	goto L1106;
L1111:
	R1 = (u64)&r;
	R2 = 6;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2));
	R1 = toi64(tou8(R1));
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L1124;
	R1 = tou64("");
	R2 = tou64("imprel32?");
	mx_lib_error(asu64(R2), asu64(R1));
L1124:
	R1 = (u64)&r;
	R2 = 4;
	asu16(R1) = *tou16p(((i64)R1+(i64)R2));
	R1 = toi64(tou16(R1));
	index = asi64(R1);
	asu64(R1) = lib;
	R2 = 72;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asu64(R2) = lib;
	R3 = 8;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3));
	R1 += (i64)R2;
	asi64(R2) = index;
	R3 = 1;
	asi64(R2) -= asi64(R3);
	R3 = 8;
	asi64(R2) *= asi64(R3);
	R1 += (i64)R2;
	q = asu64(R1);
	asu64(R1) = q;
	asu64(R2) = p;
	R3 = 4;
	R2 += (i64)R3;
	asi64(R1) -= asi64(R2);
	asu64(R2) = p;
	*tou32p(R2) = asu32(R1);
	goto L1106;
L1112:
L1106:
	i += 1; if (i <= av_2) goto L1098;
L1100:
	R1 = 1;
	R2 = (u64)&mx_decls_librelocated;
	asu64(R3) = lib;
	R4 = 208;
	asi64(R3) = *toi64p(((i64)R3+(i64)R4));
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	return;
}

static void mx_lib_loadimports(u64 plib) {
    u64 R1, R2; 
	u64 qlib;
	u64 name;
	i64 av_1;
	i64 i;
	R1 = 1;
	i = asi64(R1);
	asu64(R1) = plib;
	R2 = 48;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	av_1 = asi64(R1);
	asi64(R1) = av_1;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1128;
L1126:
	asu64(R1) = plib;
	R2 = 104;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	mx_lib_dosublib(asu64(R1));
	i += 1; if (i <= av_1) goto L1126;
L1128:
	asu64(R1) = plib;
	mx_lib_alloclibdata(asu64(R1));
	asu64(R1) = plib;
	mx_lib_dosymbols(asu64(R1));
	return;
}

static void mx_lib_dosublib(u64 name) {
    u64 R1, R2, R3; 
	u64 qlib;
	i64 n;
	asu64(R1) = name;
	asi64(R1) = mx_lib_findlib(asu64(R1));
	n = asi64(R1);
	asi64(R1) = n;
	if (asi64(R1)) goto L1131;
	asu64(R1) = name;
	asi64(R1) = mx_lib_mxaddlib(asu64(R1));
	n = asi64(R1);
	msysc_m$print_startcon();
	R1 = tou64("Loading sublib");
	msysc_m$print_str_nf(asu64(R1));
	asu64(R1) = name;
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	asi64(R1) = n;
	R2 = tou64("ml");
	asu64(R3) = name;
	asu64(R2) = mlib_addext(asu64(R3), asu64(R2));
	asu64(R1) = mx_lib_loadlibfile(asu64(R2), asi64(R1));
	qlib = asu64(R1);
	asu64(R1) = qlib;
	mx_lib_loadimports(asu64(R1));
L1131:
	return;
}

static u64 mx_lib_loadlibfile(u64 filename, i64 libno) {
    u64 R1, R2, R3, R4; 
	u64 plib;
	u64 p;
	asu64(R1) = filename;
	asu64(R1) = mx_lib_readmxfile(asu64(R1));
	p = asu64(R1);
	asu64(R1) = p;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L1134;
	asu64(R1) = filename;
	R2 = tou64("Can't find #");
	mx_lib_error(asu64(R2), asu64(R1));
L1134:
	asu64(R1) = p;
	asu64(R2) = filename;
	asu64(R1) = mx_lib_readlibfile(asu64(R2), asu64(R1));
	plib = asu64(R1);
	asi64(R1) = libno;
	asu64(R2) = plib;
	R3 = 208;
	*toi64p(((i64)R2+(i64)R3)) = asi64(R1);
	asu64(R1) = plib;
	R2 = R1;
	R3 = (u64)&mx_decls_libtable;
	asi64(R4) = libno;
	*tou64p(((i64)R3+(i64)R4*8-8)) = asu64(R2);
	goto L1132;
L1132:
	return asu64(R1);
}

static void mx_lib_dosymbols(u64 lib) {
    u64 R1, R2, R3; 
	i64 ix;
	i64 libx;
	i64 dllx;
	u64 baseaddr;
	i64 av_1;
	i64 av_2;
	i64 av_3;
	i64 i;
	R1 = 1;
	i = asi64(R1);
	asu64(R1) = lib;
	R2 = 40;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	av_1 = asi64(R1);
	asi64(R1) = av_1;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1138;
L1136:
	asu64(R1) = lib;
	R2 = 96;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	mx_lib_adddll(asu64(R1));
	i += 1; if (i <= av_1) goto L1136;
L1138:
	R1 = 1;
	i = asi64(R1);
	asu64(R1) = lib;
	R2 = 56;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	av_2 = asi64(R1);
	asi64(R1) = av_2;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1141;
L1139:
	asu64(R1) = lib;
	R2 = 112;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	asi64(R1) = mx_lib_addsymbol(asu64(R1));
	ix = asi64(R1);
	asi64(R1) = ix;
	asu64(R2) = lib;
	R3 = 176;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asi64(R3) = i;
	*toi16p(((i64)R2+(i64)R3*2-2)) = asi16(R1);
	i += 1; if (i <= av_2) goto L1139;
L1141:
	R1 = 1;
	i = asi64(R1);
	asu64(R1) = lib;
	R2 = 64;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	av_3 = asi64(R1);
	asi64(R1) = av_3;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1144;
L1142:
	asu64(R1) = lib;
	R2 = 120;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	asi64(R1) = mx_lib_addsymbol(asu64(R1));
	ix = asi64(R1);
	R1 = (u64)&mx_decls_symboldefined;
	asi64(R2) = ix;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2-1));
	if (!asu8(R1)) goto L1146;
	msysc_m$print_startcon();
	R1 = tou64("Dupl symbol:");
	msysc_m$print_str_nf(asu64(R1));
	asu64(R1) = lib;
	R2 = 120;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
	goto L1143;
L1146:
	R1 = 1;
	R2 = (u64)&mx_decls_symboldefined;
	asi64(R3) = ix;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	asu64(R1) = lib;
	R2 = 128;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	asi64(R2) = i;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2-1));
	R1 = toi64(tou8(R1));
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L1148;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L1149;
	R2 = 3;
	if (asi64(R1) == asi64(R2)) goto L1150;
	goto L1151;
L1148:
	asu64(R1) = lib;
	R2 = 72;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	baseaddr = asu64(R1);
	goto L1147;
L1149:
	asu64(R1) = lib;
	R2 = 80;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	baseaddr = asu64(R1);
	goto L1147;
L1150:
	asu64(R1) = lib;
	R2 = 152;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	baseaddr = asu64(R1);
	goto L1147;
L1151:
	R1 = 0;
	baseaddr = asu64(R1);
L1147:
	asu64(R1) = baseaddr;
	asu64(R2) = lib;
	R3 = 136;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3));
	asi64(R3) = i;
	asi64(R2) = *toi64p(((i64)R2+(i64)R3*8-8));
	R1 += (i64)R2;
	R2 = (u64)&mx_decls_symboladdress;
	asi64(R3) = ix;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
	asu64(R1) = lib;
	R2 = 208;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	R2 = (u64)&mx_decls_symbollibindex;
	asi64(R3) = ix;
	*toi16p(((i64)R2+(i64)R3*2-2)) = asi16(R1);
L1143:
	i += 1; if (i <= av_3) goto L1142;
L1144:
	return;
}

static u64 mx_lib_readmxfile(u64 filename) {
    u64 R1, R2, R3; 
	u64 p;
	asu64(R1) = filename;
	asu64(R1) = mlib_readfile(asu64(R1));
	p = asu64(R1);
	asu64(R1) = p;
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L1154;
	R1 = 0;
	goto L1152;
L1154:
	R1 = 13;
	asu64(R2) = p;
	asi64(R3) = mlib_rfsize;
	*tou8p(((i64)R2+(i64)R3)) = asu8(R1);
	asu64(R1) = p;
	goto L1152;
L1152:
	return asu64(R1);
}

static void mx_lib_adddll(u64 name) {
    u64 R1, R2, R3; 
	i64 i;
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = mx_decls_ndlllibs;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1158;
L1156:
	R1 = (u64)&mx_decls_dllnametable;
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = name;
	asi64(R1) = mlib_eqstring(asu64(R2), asu64(R1));
	if (!asi64(R1)) goto L1160;
	goto L1155;
L1160:
	i += 1; if (i <= mx_decls_ndlllibs) goto L1156;
L1158:
	asi64(R1) = mx_decls_ndlllibs;
	R2 = 20;
	if (asi64(R1) < asi64(R2)) goto L1162;
	R1 = tou64("");
	R2 = tou64("Too many DLLs");
	mx_lib_error(asu64(R2), asu64(R1));
L1162:
	asu64(R1) = name;
	R2 = (u64)&mx_decls_dllnametable;
	R3 = (u64)&mx_decls_ndlllibs;
	asi64(R3) = *(toi64p(R3)) += 1;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
L1155:
	return;
}

static i64 mx_lib_addsymbol(u64 name) {
    u64 R1, R2, R3; 
	i64 i;
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = mx_decls_nsymbols;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1166;
L1164:
	R1 = (u64)&mx_decls_symbolnametable;
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	asu64(R2) = name;
	asi64(R1) = mlib_eqstring(asu64(R2), asu64(R1));
	if (!asi64(R1)) goto L1168;
	asi64(R1) = i;
	goto L1163;
L1168:
	i += 1; if (i <= mx_decls_nsymbols) goto L1164;
L1166:
	asi64(R1) = mx_decls_nsymbols;
	R2 = 3000;
	if (asi64(R1) < asi64(R2)) goto L1170;
	R1 = tou64("");
	R2 = tou64("Too many Imports");
	mx_lib_error(asu64(R2), asu64(R1));
L1170:
	asu64(R1) = name;
	R2 = (u64)&mx_decls_symbolnametable;
	R3 = (u64)&mx_decls_nsymbols;
	asi64(R3) = *(toi64p(R3)) += 1;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
	asi64(R1) = mx_decls_nsymbols;
	goto L1163;
L1163:
	return asi64(R1);
}

static void mx_lib_setspecialglobals(i64 cmdskip) {
    u64 R1, R2, R3; 
	i64 i;
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = mx_decls_nsymbols;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1174;
L1172:
	R1 = tou64("msys.$cmdskip");
	R2 = (u64)&mx_decls_symbolnametable;
	asi64(R3) = i;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8-8));
	asi64(R1) = mlib_eqstring(asu64(R2), asu64(R1));
	if (asi64(R1)) goto L1177;
	R1 = tou64("$cmdskip");
	R2 = (u64)&mx_decls_symbolnametable;
	asi64(R3) = i;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8-8));
	asi64(R1) = mlib_eqstring(asu64(R2), asu64(R1));
	if (!asi64(R1)) goto L1176;
L1177:
	asi64(R1) = cmdskip;
	R2 = (u64)&mx_decls_symboladdress;
	asi64(R3) = i;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8-8));
	*tou8p(R2) = asu8(R1);
L1176:
	i += 1; if (i <= mx_decls_nsymbols) goto L1172;
L1174:
	return;
}

static void mx_lib_runprogram(u64 lib, i64 cmdskip) {
    u64 R1, R2, R3; 
	u64 fnptr;
	i64 libno;
	i64 i;
	asu64(R1) = lib;
	R2 = 208;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	libno = asi64(R1);
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = mx_decls_nlibs;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1181;
L1179:
	asi64(R1) = i;
	asi64(R2) = libno;
	if (asi64(R1) == asi64(R2)) goto L1183;
	R1 = (u64)&mx_decls_libinitdone;
	asi64(R2) = i;
	asu8(R1) = *tou8p(((i64)R1+(i64)R2-1));
	if (asu8(R1)) goto L1183;
	R1 = (u64)&mx_decls_libtable;
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	mx_lib_calllibinit(asu64(R1));
L1183:
	i += 1; if (i <= mx_decls_nlibs) goto L1179;
L1181:
	asu64(R1) = lib;
	R2 = 200;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	R2 = 0;
	if (asu64(R1) != asu64(R2)) goto L1185;
	R1 = tou64("");
	R2 = tou64("No entry point found");
	mx_lib_error(asu64(R2), asu64(R1));
L1185:
	asi64(R1) = cmdskip;
	mx_lib_setspecialglobals(asi64(R1));
	asu64(R1) = lib;
	R2 = 200;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	fnptr = asu64(R1);
	asu64(R1) = fnptr;
	((F17)R1)();
	R1 = 1;
	R2 = (u64)&mx_decls_libinitdone;
	asi64(R3) = libno;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	return;
}

static void mx_lib_calllibinit(u64 lib) {
    u64 R1, R2, R3, R4; 
	u64 fnptr;
	i64 libno;
	asu64(R1) = lib;
	R2 = 208;
	asi64(R1) = *toi64p(((i64)R1+(i64)R2));
	libno = asi64(R1);
	asu64(R1) = lib;
	R2 = 200;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	if (!asu64(R1)) goto L1188;
	asu64(R1) = lib;
	R2 = 200;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2));
	fnptr = asu64(R1);
	asu64(R1) = fnptr;
	((F17)R1)();
L1188:
	R1 = 1;
	R2 = (u64)&mx_decls_libinitdone;
	asu64(R3) = lib;
	R4 = 208;
	asi64(R3) = *toi64p(((i64)R3+(i64)R4));
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	return;
}

static u64 mx_lib_findsymbol(u64 name) {
    u64 R1, R2, R3; 
	i64 i;
	R1 = 1;
	i = asi64(R1);
	asi64(R1) = mx_decls_nsymbols;
	R2 = 1;
	if (asi64(R1) < asi64(R2)) goto L1192;
L1190:
	asu64(R1) = name;
	R2 = (u64)&mx_decls_symbolnametable;
	asi64(R3) = i;
	asu64(R2) = *tou64p(((i64)R2+(i64)R3*8-8));
	asi64(R1) = mlib_eqstring(asu64(R2), asu64(R1));
	if (!asi64(R1)) goto L1194;
	R1 = (u64)&mx_decls_symboladdress;
	asi64(R2) = i;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	goto L1189;
L1194:
	i += 1; if (i <= mx_decls_nsymbols) goto L1190;
L1192:
	R1 = 0;
	goto L1189;
L1189:
	return asu64(R1);
}

static u64 mx_lib_loadmx(u64 filename) {
    u64 R1, R2; 
	u64 plib;
	i64 newlib;
	u64 name;
	asu64(R1) = filename;
	asu64(R1) = mlib_extractbasefile(asu64(R1));
	asu64(R1) = mlib_convlcstring(asu64(R1));
	asu64(R1) = mlib_pcm_copyheapstring(asu64(R1));
	name = asu64(R1);
	asu64(R1) = filename;
	asu64(R2) = name;
	mx_lib_checknew(asu64(R2), asu64(R1));
	asu64(R1) = name;
	asi64(R1) = mx_lib_mxaddlib(asu64(R1));
	newlib = asi64(R1);
	asi64(R1) = newlib;
	asu64(R2) = filename;
	asu64(R1) = mx_lib_loadlibfile(asu64(R2), asi64(R1));
	plib = asu64(R1);
	asu64(R1) = plib;
	mx_lib_loadimports(asu64(R1));
	asu64(R1) = plib;
	goto L1195;
L1195:
	return asu64(R1);
}

static u64 mx_lib_loadmemmcb(u64 filename, u64 p) {
    u64 R1, R2, R3; 
	u64 plib;
	i64 newlib;
	u64 name;
	asu64(R1) = filename;
	asu64(R1) = mlib_extractbasefile(asu64(R1));
	asu64(R1) = mlib_convlcstring(asu64(R1));
	asu64(R1) = mlib_pcm_copyheapstring(asu64(R1));
	name = asu64(R1);
	asu64(R1) = filename;
	asu64(R2) = name;
	mx_lib_checknew(asu64(R2), asu64(R1));
	asu64(R1) = name;
	asi64(R1) = mx_lib_mxaddlib(asu64(R1));
	newlib = asi64(R1);
	asu64(R1) = p;
	asu64(R2) = filename;
	asu64(R1) = mx_lib_readlibfile(asu64(R2), asu64(R1));
	plib = asu64(R1);
	asi64(R1) = newlib;
	asu64(R2) = plib;
	R3 = 208;
	*toi64p(((i64)R2+(i64)R3)) = asi64(R1);
	asu64(R1) = plib;
	R2 = (u64)&mx_decls_libtable;
	asi64(R3) = newlib;
	*tou64p(((i64)R2+(i64)R3*8-8)) = asu64(R1);
	asu64(R1) = plib;
	mx_lib_loadimports(asu64(R1));
	asu64(R1) = plib;
	goto L1196;
L1196:
	return asu64(R1);
}

static u64 mc_disasm_decodeinstr(u64 cptr, u64 baseaddr) {
    u64 R1, R2, R3; 
	i64 n;
	i64 w;
	i64 opc;
	i64 reg;
	i64 op;
	i64 xxx;
	i64 oldopsize;
	i64 dispsize;
	u64 pstart;
	struct $B24 str2;
	u64 s;
	i64 av_1;
	i64 av_2;
	R1 = 0;
	R2 = (u64)&mc_disasm_deststr;
	R3 = 1;
	*tou8p(((i64)R2+(i64)R3-1)) = asu8(R1);
	asu64(R1) = cptr;
	asu64(R1) = *tou64p(R1);
	R2 = R1;
	mc_disasm_codeptr = asu64(R2);
	pstart = asu64(R1);
	R1 = 0;
	mc_disasm_rex = asi64(R1);
	R1 = 1;
	mc_disasm_opsize = asi64(R1);
	R1 = 0;
	R2 = R1;
	mc_disasm_addroverride = asi64(R2);
	R2 = R1;
	mc_disasm_sizeoverride = asi64(R2);
	R2 = R1;
	mc_disasm_f3override = asi64(R2);
	mc_disasm_f2override = asi64(R1);
	R1 = 0;
	R2 = R1;
	mc_disasm_offset = asi64(R2);
	R2 = R1;
	mc_disasm_indexreg = asi64(R2);
	mc_disasm_basereg = asi64(R1);
// mc_disasm.decodeinstr.retry:
L1198:
	R1 = (u64)&mc_disasm_codeptr;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = R1;
	opc = asi64(R2);
	switch (asi64(R1)) {
	case 0: case 1: case 8: case 9: case 16: case 17: case 24: case 25: case 32: case 33: case 40: case 41: case 48: case 49: case 56: case 57: goto L1202;
	case 2: case 3: case 10: case 11: case 18: case 19: case 26: case 27: case 34: case 35: case 42: case 43: case 50: case 51: case 58: case 59: goto L1203;
	case 4: case 5: case 12: case 13: case 20: case 21: case 28: case 29: case 36: case 37: case 44: case 45: case 52: case 53: case 60: case 61: goto L1204;
	case 6: case 7: case 14: case 22: case 23: case 30: case 31: case 38: case 39: case 46: case 47: case 54: case 55: case 62: case 63: case 96: case 97: case 98: case 100: case 101: case 108: case 109: case 110: case 111: case 140: case 142: case 154: case 160: case 161: case 162: case 163: case 196: case 197: case 200: case 201: case 202: case 203: case 204: case 205: case 206: case 207: case 212: case 213: case 214: case 228: case 229: case 230: case 231: case 234: case 236: case 237: case 238: case 239: case 240: case 241: case 245: case 248: case 249: case 250: case 251: case 252: case 253: goto L1201;
	case 15: goto L1211;
	case 64: case 65: case 66: case 67: case 68: case 69: case 70: case 71: case 72: case 73: case 74: case 75: case 76: case 77: case 78: case 79: goto L1212;
	case 80: case 81: case 82: case 83: case 84: case 85: case 86: case 87: goto L1213;
	case 88: case 89: case 90: case 91: case 92: case 93: case 94: case 95: goto L1214;
	case 99: goto L1215;
	case 102: goto L1216;
	case 103: goto L1217;
	case 104: goto L1218;
	case 105: case 107: goto L1222;
	case 106: goto L1221;
	case 112: case 113: case 114: case 115: case 116: case 117: case 118: case 119: case 120: case 121: case 122: case 123: case 124: case 125: case 126: case 127: goto L1227;
	case 128: case 129: case 130: case 131: goto L1228;
	case 132: case 133: goto L1231;
	case 134: case 135: goto L1232;
	case 136: case 137: goto L1233;
	case 138: case 139: goto L1234;
	case 141: goto L1235;
	case 143: goto L1236;
	case 144: goto L1237;
	case 145: case 146: case 147: case 148: case 149: case 150: case 151: goto L1241;
	case 152: goto L1248;
	case 153: goto L1251;
	case 155: goto L1255;
	case 156: goto L1256;
	case 157: goto L1257;
	case 158: goto L1258;
	case 159: goto L1259;
	case 164: case 165: case 166: case 167: case 170: case 171: case 172: case 173: case 174: case 175: goto L1260;
	case 168: case 169: goto L1276;
	case 176: case 177: case 178: case 179: case 180: case 181: case 182: case 183: case 184: case 185: case 186: case 187: case 188: case 189: case 190: case 191: goto L1283;
	case 192: case 193: case 208: case 209: case 210: case 211: goto L1292;
	case 194: goto L1308;
	case 195: goto L1309;
	case 198: case 199: goto L1310;
	case 215: goto L1311;
	case 216: case 217: case 218: case 219: case 220: case 221: case 222: case 223: goto L1312;
	case 224: goto L1313;
	case 225: goto L1314;
	case 226: goto L1315;
	case 227: goto L1316;
	case 232: goto L1319;
	case 233: goto L1320;
	case 235: goto L1321;
	case 242: goto L1322;
	case 243: goto L1325;
	case 244: goto L1328;
	case 246: case 247: goto L1329;
	case 254: goto L1345;
	case 255: goto L1347;
	default: goto L1201;
    };
// SWITCH
L1202:
	asi64(R1) = opc;
	R2 = 3;
	asi64(R1) >>= asi64(R2);
	op = asi64(R1);
	asi64(R1) = opc;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	mc_disasm_decodeaddr(asi64(R1));
	R1 = (u64)&mc_disasm_basereg;
	mc_disasm_getsilx(asu64(R1));
	R1 = (u64)&mc_disasm_rmreg;
	mc_disasm_getsil(asu64(R1));
	R1 = (u64)&mc_disasm_opnames;
	asi64(R2) = op;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1199;
L1203:
	asi64(R1) = opc;
	R2 = 3;
	asi64(R1) >>= asi64(R2);
	op = asi64(R1);
	asi64(R1) = opc;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	mc_disasm_decodeaddr(asi64(R1));
	R1 = (u64)&mc_disasm_opnames;
	asi64(R2) = op;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(" ");
	mc_disasm_genstr(asu64(R1));
	R1 = (u64)&mc_disasm_rmreg;
	mc_disasm_getsil(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1199;
L1204:
	R1 = (u64)&mc_disasm_opnames;
	asi64(R2) = opc;
	R3 = 3;
	asi64(R2) >>= asi64(R3);
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(" ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = opc;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1206;
	R1 = 4;
	mc_disasm_opsize = asi64(R1);
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1208;
	R1 = 2;
	mc_disasm_opsize = asi64(R1);
L1208:
	asi64(R1) = mc_disasm_rex;
	R2 = 8;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1210;
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
L1210:
L1206:
	asi64(R1) = mc_disasm_opsize;
	R2 = 1;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_readimm();
	mc_disasm_genintd(asi64(R1));
	goto L1199;
L1211:
	mc_disasm_decodetwobyteinstr();
	goto L1199;
L1212:
	asi64(R1) = opc;
	mc_disasm_rex = asi64(R1);
	goto L1198;
	goto L1199;
L1213:
	asi64(R1) = mc_disasm_rex;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	asi64(R2) = opc;
	R3 = 7;
	asi64(R2) &= asi64(R3);
	asi64(R1) = mc_disasm_getreg(asi64(R2), asi64(R1));
	reg = asi64(R1);
	R1 = tou64("push ");
	mc_disasm_genstr(asu64(R1));
	R1 = 8;
	asi64(R2) = reg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1199;
L1214:
	asi64(R1) = mc_disasm_rex;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	asi64(R2) = opc;
	R3 = 7;
	asi64(R2) &= asi64(R3);
	asi64(R1) = mc_disasm_getreg(asi64(R2), asi64(R1));
	reg = asi64(R1);
	R1 = tou64("pop ");
	mc_disasm_genstr(asu64(R1));
	R1 = 8;
	asi64(R2) = reg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1199;
L1215:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	R1 = tou64("movsxd ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 4;
	mc_disasm_opsize = asi64(R1);
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1199;
L1216:
	R1 = 1;
	mc_disasm_sizeoverride = asi64(R1);
	goto L1198;
	goto L1199;
L1217:
	R1 = 1;
	mc_disasm_addroverride = asi64(R1);
	goto L1198;
	goto L1199;
L1218:
	R1 = tou64("push ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1220;
	asi64(R1) = mc_disasm_readint16();
	mc_disasm_genintd(asi64(R1));
	goto L1219;
L1220:
	asi64(R1) = mc_disasm_readint32();
	mc_disasm_genintd(asi64(R1));
L1219:
	goto L1199;
L1221:
	R1 = tou64("push ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_readsbyte();
	mc_disasm_genintd(asi64(R1));
	goto L1199;
L1222:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = mc_disasm_basereg;
	asi64(R2) = mc_disasm_rmreg;
	if (asi64(R1) == asi64(R2)) goto L1224;
	R1 = tou64("imul3");
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(" ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	goto L1223;
L1224:
	R1 = tou64("imul2");
	mc_disasm_genstr(asu64(R1));
L1223:
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = opc;
	R2 = 2;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1226;
	R1 = 1;
	goto L1225;
L1226:
	asi64(R1) = mc_disasm_opsize;
L1225:
	mc_disasm_opsize = asi64(R1);
	asi64(R1) = mc_disasm_readimm();
	mc_disasm_genintd(asi64(R1));
	goto L1199;
L1227:
	R1 = tou64("j");
	mc_disasm_genstr(asu64(R1));
	R1 = (u64)&mc_disasm_condnames;
	asi64(R2) = opc;
	R3 = 15;
	asi64(R2) &= asi64(R3);
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(" ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_readsbyte();
	mc_disasm_genintd(asi64(R1));
	goto L1199;
L1228:
	asi64(R1) = opc;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	mc_disasm_decodeaddr(asi64(R1));
	R1 = (u64)&mc_disasm_opnames;
	asi64(R2) = mc_disasm_rmopc;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	mc_disasm_genstr(asu64(R1));
	R1 = (u64)&mc_disasm_basereg;
	mc_disasm_getsilx(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = opc;
	R2 = 131;
	if (asi64(R1) == asi64(R2)) goto L1230;
	asi64(R1) = mc_disasm_readimm();
	mc_disasm_genintd(asi64(R1));
	goto L1229;
L1230:
	asi64(R1) = mc_disasm_readsbyte();
	mc_disasm_genintd(asi64(R1));
L1229:
	goto L1199;
L1231:
	asi64(R1) = opc;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	mc_disasm_decodeaddr(asi64(R1));
	R1 = (u64)&mc_disasm_basereg;
	mc_disasm_getsilx(asu64(R1));
	R1 = (u64)&mc_disasm_rmreg;
	mc_disasm_getsil(asu64(R1));
	R1 = tou64("test ");
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1199;
L1232:
	asi64(R1) = opc;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	mc_disasm_decodeaddr(asi64(R1));
	R1 = tou64("exch2 ");
	mc_disasm_genstr(asu64(R1));
	R1 = (u64)&mc_disasm_basereg;
	mc_disasm_getsilx(asu64(R1));
	R1 = (u64)&mc_disasm_rmreg;
	mc_disasm_getsil(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(",");
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1199;
L1233:
	asi64(R1) = opc;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	mc_disasm_decodeaddr(asi64(R1));
	R1 = tou64("mov");
	mc_disasm_genstr(asu64(R1));
	R1 = (u64)&mc_disasm_basereg;
	mc_disasm_getsilx(asu64(R1));
	R1 = (u64)&mc_disasm_rmreg;
	mc_disasm_getsil(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1199;
L1234:
	asi64(R1) = opc;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	mc_disasm_decodeaddr(asi64(R1));
	R1 = tou64("mov ");
	mc_disasm_genstr(asu64(R1));
	R1 = (u64)&mc_disasm_basereg;
	mc_disasm_getsilx(asu64(R1));
	R1 = (u64)&mc_disasm_rmreg;
	mc_disasm_getsil(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1199;
L1235:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	R1 = tou64("lea ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1199;
L1236:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	R1 = 1;
	mc_disasm_opsize = asi64(R1);
	R1 = tou64("pop");
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1199;
L1237:
	asi64(R1) = mc_disasm_rex;
	if (!asi64(R1)) goto L1239;
	goto L1240;
L1239:
	R1 = tou64("nop");
	mc_disasm_genstr(asu64(R1));
	goto L1199;
L1241:
// mc_disasm.decodeinstr.doexch:
L1240:
	asi64(R1) = opc;
	R2 = 7;
	asi64(R1) &= asi64(R2);
	R2 = 1;
	asi64(R1) += asi64(R2);
	reg = asi64(R1);
	asi64(R1) = mc_disasm_rex;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1243;
	R1 = 8;
	R2 = (u64)&reg;
	*toi64p(R2) += asi64(R1);
L1243:
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1245;
	R1 = 2;
	goto L1244;
L1245:
	R1 = 4;
L1244:
	mc_disasm_opsize = asi64(R1);
	asi64(R1) = mc_disasm_rex;
	R2 = 8;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1247;
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
L1247:
	R1 = tou64("xchg ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	R2 = 1;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = reg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1199;
L1248:
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1250;
	R1 = tou64("cbw");
	mc_disasm_genstr(asu64(R1));
	goto L1249;
L1250:
	R1 = tou64("cbw???");
	mc_disasm_genstr(asu64(R1));
L1249:
	goto L1199;
L1251:
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1253;
	R1 = tou64("cwd");
	mc_disasm_genstr(asu64(R1));
	goto L1252;
L1253:
	asi64(R1) = mc_disasm_rex;
	R2 = 8;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1254;
	R1 = tou64("cqo");
	mc_disasm_genstr(asu64(R1));
	goto L1252;
L1254:
	R1 = tou64("cdq");
	mc_disasm_genstr(asu64(R1));
L1252:
	goto L1199;
L1255:
	R1 = tou64("wait");
	mc_disasm_genstr(asu64(R1));
	goto L1199;
L1256:
	R1 = tou64("pushf");
	mc_disasm_genstr(asu64(R1));
	goto L1199;
L1257:
	R1 = tou64("popf");
	mc_disasm_genstr(asu64(R1));
	goto L1199;
L1258:
	R1 = tou64("sahf");
	mc_disasm_genstr(asu64(R1));
	goto L1199;
L1259:
	R1 = tou64("lahf");
	mc_disasm_genstr(asu64(R1));
	goto L1199;
L1260:
	asi64(R1) = opc;
	R2 = 1;
	asi64(R1) >>= asi64(R2);
	R2 = 7;
	asi64(R1) &= asi64(R2);
	switch (asi64(R1)) {
	case 1: goto L1264;
	case 2: goto L1265;
	case 3: goto L1266;
	case 4: goto L1267;
	case 5: goto L1268;
	case 6: goto L1269;
	case 7: goto L1270;
	default: goto L1263;
    };
// SWITCH
L1264:
	R1 = tou64("?");
	goto L1261;
L1265:
	R1 = tou64("movs");
	goto L1261;
L1266:
	R1 = tou64("cmps");
	goto L1261;
L1267:
	R1 = tou64("?");
	goto L1261;
L1268:
	R1 = tou64("stos");
	goto L1261;
L1269:
	R1 = tou64("lods");
	goto L1261;
L1270:
	R1 = tou64("scas");
	goto L1261;
L1263:
	R1 = tou64("?");
L1261:
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = opc;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L1272;
	R1 = tou64("b");
	mc_disasm_genstr(asu64(R1));
	goto L1271;
L1272:
	asi64(R1) = mc_disasm_rex;
	R2 = 8;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1274;
	R1 = tou64("q");
	mc_disasm_genstr(asu64(R1));
	goto L1273;
L1274:
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1275;
	R1 = tou64("w");
	mc_disasm_genstr(asu64(R1));
	goto L1273;
L1275:
	R1 = tou64("d");
	mc_disasm_genstr(asu64(R1));
L1273:
L1271:
	goto L1199;
L1276:
	R1 = tou64("test ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = opc;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1278;
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1280;
	R1 = 2;
	goto L1279;
L1280:
	R1 = 4;
L1279:
	mc_disasm_opsize = asi64(R1);
	asi64(R1) = mc_disasm_rex;
	R2 = 8;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1282;
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
L1282:
L1278:
	asi64(R1) = mc_disasm_opsize;
	R2 = 1;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_readimm();
	mc_disasm_genintd(asi64(R1));
	goto L1199;
L1283:
	asi64(R1) = opc;
	R2 = 7;
	asi64(R1) &= asi64(R2);
	R2 = 1;
	asi64(R1) += asi64(R2);
	reg = asi64(R1);
	asi64(R1) = mc_disasm_rex;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1285;
	R1 = 8;
	R2 = (u64)&reg;
	*toi64p(R2) += asi64(R1);
L1285:
	asi64(R1) = opc;
	R2 = 8;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1287;
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1289;
	R1 = 2;
	goto L1288;
L1289:
	R1 = 4;
L1288:
	mc_disasm_opsize = asi64(R1);
	asi64(R1) = mc_disasm_rex;
	R2 = 8;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1291;
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
L1291:
L1287:
	R1 = tou64("mov ");
	mc_disasm_genstr(asu64(R1));
	R1 = (u64)&reg;
	mc_disasm_getsil(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = reg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_readimm8();
	mc_disasm_genintd(asi64(R1));
	goto L1199;
L1292:
	asi64(R1) = opc;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	mc_disasm_decodeaddr(asi64(R1));
	R1 = (u64)&mc_disasm_basereg;
	mc_disasm_getsilx(asu64(R1));
	asi64(R1) = mc_disasm_rmopc;
	R2 = 1;
	asi64(R1) += asi64(R2);
	switch (asi64(R1)) {
	case 1: goto L1296;
	case 2: goto L1297;
	case 3: goto L1298;
	case 4: goto L1299;
	case 5: goto L1300;
	case 6: goto L1301;
	case 7: goto L1302;
	case 8: goto L1303;
	default: goto L1295;
    };
// SWITCH
L1296:
	R1 = tou64("rol");
	goto L1293;
L1297:
	R1 = tou64("ror");
	goto L1293;
L1298:
	R1 = tou64("rcl");
	goto L1293;
L1299:
	R1 = tou64("rcr");
	goto L1293;
L1300:
	R1 = tou64("shl");
	goto L1293;
L1301:
	R1 = tou64("shr");
	goto L1293;
L1302:
	R1 = tou64("?");
	goto L1293;
L1303:
	R1 = tou64("sar");
	goto L1293;
L1295:
	R1 = tou64("?");
L1293:
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	asi64(R1) = opc;
	R2 = 193;
	if (asi64(R1) > asi64(R2)) goto L1305;
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_readbyte();
	mc_disasm_genintd(asi64(R1));
	goto L1304;
L1305:
	asi64(R1) = opc;
	R2 = 2;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1307;
	R1 = tou64(", cl");
	goto L1306;
L1307:
	R1 = tou64(", 1");
L1306:
	mc_disasm_genstr(asu64(R1));
L1304:
	goto L1199;
L1308:
	R1 = tou64("retn ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_readword16();
	mc_disasm_genintd(asi64(R1));
	goto L1199;
L1309:
	R1 = tou64("ret");
	mc_disasm_genstr(asu64(R1));
	goto L1199;
L1310:
	asi64(R1) = opc;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	mc_disasm_decodeaddr(asi64(R1));
	R1 = tou64("mov");
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_readimm();
	mc_disasm_genintd(asi64(R1));
	goto L1199;
L1311:
	R1 = tou64("xlat");
	mc_disasm_genstr(asu64(R1));
	goto L1199;
L1312:
	asi64(R1) = opc;
	R2 = 7;
	asi64(R1) &= asi64(R2);
	mc_disasm_decode8087(asi64(R1));
	goto L1199;
L1313:
	R1 = tou64("loopnz ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_readsbyte();
	mc_disasm_genintd(asi64(R1));
	goto L1199;
L1314:
	R1 = tou64("loopz ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_readsbyte();
	mc_disasm_genintd(asi64(R1));
	goto L1199;
L1315:
	R1 = tou64("loop ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_readsbyte();
	mc_disasm_genintd(asi64(R1));
	goto L1199;
L1316:
	asi64(R1) = mc_disasm_addroverride;
	if (!asi64(R1)) goto L1318;
	R1 = tou64("jecxz ");
	mc_disasm_genstr(asu64(R1));
	goto L1317;
L1318:
	R1 = tou64("jrcxz ");
	mc_disasm_genstr(asu64(R1));
L1317:
	asi64(R1) = mc_disasm_readsbyte();
	mc_disasm_genintd(asi64(R1));
	goto L1199;
L1319:
	R1 = tou64("call ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_readint32();
	mc_disasm_genintd(asi64(R1));
	goto L1199;
L1320:
	R1 = tou64("[4] jmp ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_readint32();
	mc_disasm_genintd(asi64(R1));
	goto L1199;
L1321:
	R1 = tou64("jmp ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_readsbyte();
	mc_disasm_genintd(asi64(R1));
	goto L1199;
L1322:
	asu64(R1) = mc_disasm_codeptr;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 15;
	if (asi64(R1) == asi64(R2)) goto L1324;
	asu64(R1) = mc_disasm_codeptr;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 64;
	if (asi64(R1) >= asi64(R2)) goto L1324;
	asu64(R1) = mc_disasm_codeptr;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 79;
	if (asi64(R1) <= asi64(R2)) goto L1324;
	R1 = tou64("repne");
	mc_disasm_genstr(asu64(R1));
	goto L1323;
L1324:
	R1 = 1;
	mc_disasm_f2override = asi64(R1);
	goto L1198;
L1323:
	goto L1199;
L1325:
	asu64(R1) = mc_disasm_codeptr;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 15;
	if (asi64(R1) == asi64(R2)) goto L1327;
	asu64(R1) = mc_disasm_codeptr;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 64;
	if (asi64(R1) >= asi64(R2)) goto L1327;
	asu64(R1) = mc_disasm_codeptr;
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = 79;
	if (asi64(R1) <= asi64(R2)) goto L1327;
	R1 = tou64("repe");
	mc_disasm_genstr(asu64(R1));
	goto L1326;
L1327:
	R1 = 1;
	mc_disasm_f3override = asi64(R1);
	goto L1198;
L1326:
	goto L1199;
L1328:
	goto L1199;
L1329:
	asi64(R1) = opc;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	mc_disasm_decodeaddr(asi64(R1));
	R1 = (u64)&mc_disasm_basereg;
	mc_disasm_getsilx(asu64(R1));
	asi64(R1) = mc_disasm_rmopc;
	R2 = 1;
	asi64(R1) += asi64(R2);
	switch (asi64(R1)) {
	case 1: goto L1333;
	case 2: goto L1334;
	case 3: goto L1335;
	case 4: goto L1336;
	case 5: goto L1337;
	case 6: goto L1338;
	case 7: goto L1339;
	case 8: goto L1340;
	default: goto L1332;
    };
// SWITCH
L1333:
	R1 = tou64("test");
	goto L1330;
L1334:
	R1 = tou64("?");
	goto L1330;
L1335:
	R1 = tou64("not");
	goto L1330;
L1336:
	R1 = tou64("neg");
	goto L1330;
L1337:
	R1 = tou64("mul");
	goto L1330;
L1338:
	R1 = tou64("imul");
	goto L1330;
L1339:
	R1 = tou64("div");
	goto L1330;
L1340:
	R1 = tou64("idiv");
	goto L1330;
L1332:
	R1 = tou64("?");
L1330:
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	asi64(R1) = mc_disasm_rmopc;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L1342;
	asi64(R1) = mc_disasm_opsize;
	R2 = 8;
	if (asi64(R1) != asi64(R2)) goto L1344;
	R1 = 4;
	mc_disasm_opsize = asi64(R1);
L1344:
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_readimm();
	mc_disasm_genintd(asi64(R1));
L1342:
	goto L1199;
L1345:
	R1 = 0;
	w = asi64(R1);
	goto L1346;
	goto L1199;
L1347:
	R1 = 1;
	w = asi64(R1);
// mc_disasm.decodeinstr.doff:
L1346:
	asi64(R1) = w;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = mc_disasm_rmopc;
	R2 = 0;
	if (asi64(R1) == asi64(R2)) goto L1349;
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L1350;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L1351;
	R2 = 4;
	if (asi64(R1) == asi64(R2)) goto L1352;
	R2 = 6;
	if (asi64(R1) == asi64(R2)) goto L1353;
	goto L1354;
L1349:
	R1 = (u64)&mc_disasm_basereg;
	mc_disasm_getsilx(asu64(R1));
	R1 = tou64("inc");
	mc_disasm_genstr(asu64(R1));
	goto L1348;
L1350:
	R1 = (u64)&mc_disasm_basereg;
	mc_disasm_getsilx(asu64(R1));
	R1 = tou64("dec");
	mc_disasm_genstr(asu64(R1));
	goto L1348;
L1351:
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
	R1 = tou64("icall");
	mc_disasm_genstr(asu64(R1));
	goto L1348;
L1352:
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
	R1 = tou64("jmp");
	mc_disasm_genstr(asu64(R1));
	goto L1348;
L1353:
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
	R1 = tou64("push");
	mc_disasm_genstr(asu64(R1));
	goto L1348;
L1354:
	msysc_m$print_startcon();
	R1 = tou64("FFxx?");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
L1348:
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1199;
L1201:
	R1 = tou64("Unknown opcode: ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = opc;
	mc_disasm_genhex(asi64(R1));
L1199:
	R1 = (u64)&mc_disasm_decodeinstr_str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("z6h");
	asu64(R2) = baseaddr;
	msysc_m$print_ptr(asu64(R2), asu64(R1));
	msysc_m$print_nogap();
	R1 = tou64(": ");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_end();
	asu64(R1) = mc_disasm_codeptr;
	asu64(R2) = pstart;
	asi64(R1) -= asi64(R2);
	n = asi64(R1);
	asi64(R1) = n;
	av_1 = asi64(R1);
	asi64(R1) = av_1;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L1357;
L1355:
	R1 = (u64)&str2;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("z2H");
	R2 = (u64)&pstart;
	asu64(R3) = *tou64p(R2); *(tou64p(R2)) += 1; asu64(R2) = asu64(R3);
	asu8(R2) = *tou8p(R2);
	R2 = toi64(tou8(R2));
	msysc_m$print_i64(asi64(R2), asu64(R1));
	msysc_m$print_nogap();
	R1 = tou64(" ");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_end();
	R1 = (u64)&str2;
	R2 = (u64)&mc_disasm_decodeinstr_str;
	asu64(R1) = strcat(asu64(R2), asu64(R1));
	if (--asi64(av_1)) goto L1355;
L1357:
	R1 = 14;
	asi64(R2) = n;
	asi64(R1) -= asi64(R2);
	av_2 = asi64(R1);
	asi64(R1) = av_2;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L1360;
L1358:
	R1 = tou64("-- ");
	R2 = (u64)&mc_disasm_decodeinstr_str;
	asu64(R1) = strcat(asu64(R2), asu64(R1));
	if (--asi64(av_2)) goto L1358;
L1360:
	R1 = (u64)&mc_disasm_deststr;
	R2 = (u64)&mc_disasm_decodeinstr_str;
	asu64(R1) = strcat(asu64(R2), asu64(R1));
	asu64(R1) = mc_disasm_codeptr;
	asu64(R2) = cptr;
	*tou64p(R2) = asu64(R1);
	R1 = (u64)&mc_disasm_decodeinstr_str;
	goto L1197;
L1197:
	return asu64(R1);
}

static void mc_disasm_decodetwobyteinstr() {
    u64 R1, R2, R3; 
	i64 opc;
	i64 rhssize;
	i64 third;
	i64 imm;
	u64 opcstr;
	R1 = (u64)&mc_disasm_codeptr;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	R2 = R1;
	opc = asi64(R2);
	switch (asi64(R1)) {
	case 42: goto L1365;
	case 43: case 46: case 48: case 49: case 50: case 51: case 52: case 53: case 54: case 55: case 56: case 57: case 59: case 60: case 61: case 62: case 63: case 80: case 82: case 83: case 85: case 86: case 91: case 96: case 97: case 98: case 99: case 100: case 101: case 102: case 103: case 104: case 105: case 106: case 107: case 108: case 109: case 112: case 113: case 114: case 115: case 116: case 117: case 118: case 119: case 120: case 121: case 122: case 123: case 124: case 125: case 160: case 161: case 162: case 163: case 164: case 165: case 166: case 167: case 168: case 169: case 170: case 171: case 172: case 173: case 174: case 176: case 177: case 178: case 179: case 180: case 181: case 185: case 186: case 187: case 192: case 193: case 194: case 195: case 196: case 197: case 198: case 199: case 200: case 201: case 202: case 203: case 204: case 205: case 206: case 207: case 208: case 209: case 210: case 211: case 212: case 213: case 215: case 216: case 217: case 218: case 220: case 221: case 222: case 223: case 224: case 225: case 226: case 227: case 228: case 229: case 230: case 231: case 232: case 233: case 234: case 235: case 236: case 237: case 238: goto L1364;
	case 44: goto L1368;
	case 45: goto L1373;
	case 47: goto L1378;
	case 58: goto L1381;
	case 64: case 65: case 66: case 67: case 68: case 69: case 70: case 71: case 72: case 73: case 74: case 75: case 76: case 77: case 78: case 79: goto L1386;
	case 81: goto L1387;
	case 84: goto L1392;
	case 87: goto L1397;
	case 88: goto L1402;
	case 89: goto L1406;
	case 90: goto L1407;
	case 92: goto L1410;
	case 93: goto L1411;
	case 94: goto L1412;
	case 95: goto L1413;
	case 110: goto L1414;
	case 111: goto L1421;
	case 126: goto L1425;
	case 127: goto L1431;
	case 128: case 129: case 130: case 131: case 132: case 133: case 134: case 135: case 136: case 137: case 138: case 139: case 140: case 141: case 142: case 143: goto L1435;
	case 144: case 145: case 146: case 147: case 148: case 149: case 150: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: goto L1438;
	case 175: goto L1439;
	case 182: case 183: case 190: case 191: goto L1440;
	case 184: goto L1445;
	case 188: case 189: goto L1446;
	case 214: goto L1449;
	case 219: goto L1450;
	case 239: goto L1451;
	default: goto L1364;
    };
// SWITCH
L1365:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = mc_disasm_f3override;
	if (!asi64(R1)) goto L1367;
	R1 = tou64("cvtsi2ss ");
	mc_disasm_genstr(asu64(R1));
	goto L1366;
L1367:
	R1 = tou64("cvtsi2sd ");
	mc_disasm_genstr(asu64(R1));
L1366:
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1368:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = mc_disasm_f3override;
	if (!asi64(R1)) goto L1370;
	R1 = tou64("cvttss2si ");
	mc_disasm_genstr(asu64(R1));
	R1 = 4;
	rhssize = asi64(R1);
	goto L1369;
L1370:
	R1 = tou64("cvttsd2si ");
	mc_disasm_genstr(asu64(R1));
	R1 = 8;
	rhssize = asi64(R1);
L1369:
	asi64(R1) = mc_disasm_rex;
	R2 = 8;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1372;
	R1 = 8;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1371;
L1372:
	R1 = 4;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
L1371:
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = rhssize;
	mc_disasm_opsize = asi64(R1);
	R1 = 1;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1373:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = mc_disasm_f3override;
	if (!asi64(R1)) goto L1375;
	R1 = tou64("cvtss2si ");
	mc_disasm_genstr(asu64(R1));
	R1 = 4;
	rhssize = asi64(R1);
	goto L1374;
L1375:
	R1 = tou64("cvtsd2si ");
	mc_disasm_genstr(asu64(R1));
	R1 = 8;
	rhssize = asi64(R1);
L1374:
	asi64(R1) = mc_disasm_rex;
	R2 = 8;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1377;
	R1 = 8;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1376;
L1377:
	R1 = 4;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
L1376:
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = rhssize;
	mc_disasm_opsize = asi64(R1);
	R1 = 1;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1378:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1380;
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
	R1 = tou64("comisd ");
	mc_disasm_genstr(asu64(R1));
	goto L1379;
L1380:
	R1 = 4;
	mc_disasm_opsize = asi64(R1);
	R1 = tou64("comiss ");
	mc_disasm_genstr(asu64(R1));
L1379:
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 1;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1381:
	R1 = (u64)&mc_disasm_codeptr;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	third = asi64(R1);
	asi64(R1) = third;
	R2 = 99;
	if (asi64(R1) == asi64(R2)) goto L1383;
	R2 = 98;
	if (asi64(R1) == asi64(R2)) goto L1384;
	goto L1385;
L1383:
	R1 = tou64("pcmpistri ");
	mc_disasm_genstr(asu64(R1));
	goto L1382;
L1384:
	R1 = tou64("pcmpistrm ");
	mc_disasm_genstr(asu64(R1));
	goto L1382;
L1385:
	R1 = tou64("Unknown opcode 2-byte opcode: 0F ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = opc;
	mc_disasm_genhex(asi64(R1));
	goto L1361;
L1382:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 1;
	mc_disasm_printaddrmode(asi64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = (u64)&mc_disasm_codeptr;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	imm = asi64(R1);
	asi64(R1) = imm;
	mc_disasm_genintd(asi64(R1));
	goto L1362;
L1386:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	R1 = tou64("cmov");
	mc_disasm_genstr(asu64(R1));
	R1 = (u64)&mc_disasm_condnames;
	asi64(R2) = opc;
	R3 = 15;
	asi64(R2) &= asi64(R3);
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(" ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1387:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = mc_disasm_f3override;
	if (!asi64(R1)) goto L1389;
	R1 = 4;
	goto L1388;
L1389:
	R1 = 8;
L1388:
	mc_disasm_opsize = asi64(R1);
	asi64(R1) = mc_disasm_opsize;
	R2 = 4;
	if (asi64(R1) != asi64(R2)) goto L1391;
	R1 = tou64("sqrtss ");
	goto L1390;
L1391:
	R1 = tou64("sqrtsd ");
L1390:
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 1;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1392:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1394;
	R1 = tou64("andpd ");
	goto L1393;
L1394:
	R1 = tou64("andps ");
L1393:
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1396;
	R1 = 8;
	goto L1395;
L1396:
	R1 = 4;
L1395:
	mc_disasm_opsize = asi64(R1);
	R1 = 1;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1397:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1399;
	R1 = tou64("xorpd ");
	goto L1398;
L1399:
	R1 = tou64("xorps ");
L1398:
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1401;
	R1 = 8;
	goto L1400;
L1401:
	R1 = 4;
L1400:
	mc_disasm_opsize = asi64(R1);
	R1 = 1;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1402:
	R1 = tou64("adds");
	opcstr = asu64(R1);
// mc_disasm.decodetwobyteinstr.doarith:
L1403:
	asu64(R1) = opcstr;
	mc_disasm_genstr(asu64(R1));
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = mc_disasm_f2override;
	if (!asi64(R1)) goto L1405;
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
	R1 = tou64("d ");
	mc_disasm_genstr(asu64(R1));
	goto L1404;
L1405:
	R1 = 4;
	mc_disasm_opsize = asi64(R1);
	R1 = tou64("s ");
	mc_disasm_genstr(asu64(R1));
L1404:
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 1;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1406:
	R1 = tou64("muls");
	opcstr = asu64(R1);
	goto L1403;
	goto L1362;
L1407:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = mc_disasm_f3override;
	if (!asi64(R1)) goto L1409;
	R1 = tou64("cvtss2sd ");
	mc_disasm_genstr(asu64(R1));
	R1 = 4;
	rhssize = asi64(R1);
	goto L1408;
L1409:
	R1 = tou64("cvtsd2ss ");
	mc_disasm_genstr(asu64(R1));
	R1 = 8;
	rhssize = asi64(R1);
L1408:
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = rhssize;
	mc_disasm_opsize = asi64(R1);
	R1 = 1;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1410:
	R1 = tou64("subs");
	opcstr = asu64(R1);
	goto L1403;
	goto L1362;
L1411:
	R1 = tou64("mins");
	opcstr = asu64(R1);
	goto L1403;
	goto L1362;
L1412:
	R1 = tou64("divs");
	opcstr = asu64(R1);
	goto L1403;
	goto L1362;
L1413:
	R1 = tou64("maxs");
	opcstr = asu64(R1);
	goto L1403;
	goto L1362;
L1414:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = mc_disasm_rex;
	R2 = 8;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1416;
	R1 = 8;
	goto L1415;
L1416:
	R1 = 4;
L1415:
	mc_disasm_opsize = asi64(R1);
	asi64(R1) = mc_disasm_opsize;
	R2 = 4;
	if (asi64(R1) != asi64(R2)) goto L1418;
	R1 = tou64("movd ");
	goto L1417;
L1418:
	R1 = tou64("movq ");
L1417:
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1420;
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1419;
L1420:
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strmmx(asi64(R1));
	mc_disasm_genstr(asu64(R1));
L1419:
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1421:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	R1 = 16;
	mc_disasm_opsize = asi64(R1);
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1423;
	R1 = tou64("movdqa ");
	mc_disasm_genstr(asu64(R1));
	goto L1422;
L1423:
	asi64(R1) = mc_disasm_f3override;
	if (!asi64(R1)) goto L1424;
	R1 = tou64("movdqu ");
	mc_disasm_genstr(asu64(R1));
	goto L1422;
L1424:
	R1 = tou64("No 66/F3 ");
	mc_disasm_genstr(asu64(R1));
L1422:
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 1;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1425:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = mc_disasm_f3override;
	if (!asi64(R1)) goto L1427;
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
	R1 = tou64("movq ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 1;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1426;
L1427:
	asi64(R1) = mc_disasm_rex;
	R2 = 8;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1428;
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
	R1 = tou64("movq ");
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1426;
L1428:
	R1 = 4;
	mc_disasm_opsize = asi64(R1);
	R1 = tou64("movd ");
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1430;
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1429;
L1430:
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strmmx(asi64(R1));
	mc_disasm_genstr(asu64(R1));
L1429:
L1426:
	goto L1362;
L1431:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	R1 = 16;
	mc_disasm_opsize = asi64(R1);
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1433;
	R1 = tou64("movdqa ");
	mc_disasm_genstr(asu64(R1));
	goto L1432;
L1433:
	asi64(R1) = mc_disasm_f3override;
	if (!asi64(R1)) goto L1434;
	R1 = tou64("movdqu ");
	mc_disasm_genstr(asu64(R1));
	goto L1432;
L1434:
	R1 = tou64("No 66/F3 ");
	mc_disasm_genstr(asu64(R1));
L1432:
	R1 = 1;
	mc_disasm_printaddrmode(asi64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1362;
L1435:
	R1 = tou64("[long] j");
	mc_disasm_genstr(asu64(R1));
	R1 = (u64)&mc_disasm_condnames;
	asi64(R2) = opc;
	R3 = 15;
	asi64(R2) &= asi64(R3);
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(" ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1437;
	asi64(R1) = mc_disasm_readint16();
	mc_disasm_genintd(asi64(R1));
	goto L1436;
L1437:
	asi64(R1) = mc_disasm_readint32();
	mc_disasm_genintd(asi64(R1));
L1436:
	goto L1362;
L1438:
	R1 = 0;
	mc_disasm_decodeaddr(asi64(R1));
	R1 = tou64("set");
	mc_disasm_genstr(asu64(R1));
	R1 = (u64)&mc_disasm_condnames;
	asi64(R2) = opc;
	R3 = 15;
	asi64(R2) &= asi64(R3);
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(" ");
	mc_disasm_genstr(asu64(R1));
	R1 = (u64)&mc_disasm_basereg;
	mc_disasm_getsilx(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1439:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	R1 = tou64("imul ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1440:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = opc;
	R2 = 190;
	if (asi64(R1) >= asi64(R2)) goto L1442;
	R1 = tou64("movzx ");
	goto L1441;
L1442:
	R1 = tou64("movsx ");
L1441:
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = opc;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1444;
	R1 = 2;
	goto L1443;
L1444:
	R1 = 1;
L1443:
	mc_disasm_opsize = asi64(R1);
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1445:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	R1 = tou64("popcnt ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1446:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = opc;
	R2 = 188;
	if (asi64(R1) != asi64(R2)) goto L1448;
	R1 = tou64("bsf ");
	goto L1447;
L1448:
	R1 = tou64("bsr ");
L1447:
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1449:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
	R1 = tou64("movq ");
	mc_disasm_genstr(asu64(R1));
	R1 = 1;
	mc_disasm_printaddrmode(asi64(R1));
	R1 = tou64(",");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1362;
L1450:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	R1 = tou64("pand ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
	R1 = 1;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1451:
	R1 = 1;
	mc_disasm_decodeaddr(asi64(R1));
	R1 = tou64("pxor ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_rmreg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", ");
	mc_disasm_genstr(asu64(R1));
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
	R1 = 1;
	mc_disasm_printaddrmode(asi64(R1));
	goto L1362;
L1364:
// mc_disasm.decodetwobyteinstr.error:
	R1 = tou64("Unknown opcode 2-byte opcode: 0F ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = opc;
	mc_disasm_genhex(asi64(R1));
L1362:
L1361:
	return;
}

static void mc_disasm_decodeaddr(i64 w) {
    u64 R1, R2; 
	i64 modrm;
	i64 xxx;
	i64 mode;
	i64 sib;
	i64 rm;
	R1 = 0;
	R2 = R1;
	mc_disasm_indexreg = asi64(R2);
	mc_disasm_basereg = asi64(R1);
	R1 = 1;
	mc_disasm_scale = asi64(R1);
	R1 = 0;
	mc_disasm_offset = asi64(R1);
	R1 = 0;
	mc_disasm_ripmode = asi64(R1);
	asi64(R1) = w;
	if (!asi64(R1)) goto L1455;
	asi64(R1) = mc_disasm_sizeoverride;
	if (!asi64(R1)) goto L1457;
	R1 = 2;
	goto L1456;
L1457:
	R1 = 4;
L1456:
	mc_disasm_opsize = asi64(R1);
	asi64(R1) = mc_disasm_rex;
	R2 = 8;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1459;
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
L1459:
	goto L1454;
L1455:
	R1 = 1;
	mc_disasm_opsize = asi64(R1);
L1454:
	R1 = (u64)&mc_disasm_codeptr;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	modrm = asi64(R1);
	asi64(R1) = modrm;
	R2 = 6;
	asi64(R1) >>= asi64(R2);
	mode = asi64(R1);
	asi64(R1) = modrm;
	R2 = 3;
	asi64(R1) >>= asi64(R2);
	R2 = 7;
	asi64(R1) &= asi64(R2);
	xxx = asi64(R1);
	asi64(R1) = modrm;
	R2 = 7;
	asi64(R1) &= asi64(R2);
	rm = asi64(R1);
	asi64(R1) = mode;
	R2 = 3;
	if (asi64(R1) != asi64(R2)) goto L1461;
	asi64(R1) = rm;
	R2 = 1;
	asi64(R1) += asi64(R2);
	mc_disasm_basereg = asi64(R1);
	R1 = 1;
	mc_disasm_addrmode = asi64(R1);
	goto L1460;
L1461:
	asi64(R1) = rm;
	R2 = 4;
	if (asi64(R1) == asi64(R2)) goto L1462;
	asi64(R1) = mode;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L1464;
	asi64(R1) = rm;
	R2 = 5;
	if (asi64(R1) != asi64(R2)) goto L1464;
	asi64(R1) = mc_disasm_readint32();
	mc_disasm_offset = asi64(R1);
	R1 = 1;
	mc_disasm_ripmode = asi64(R1);
	R1 = 2;
	mc_disasm_addrmode = asi64(R1);
	goto L1463;
L1464:
	asi64(R1) = rm;
	R2 = 1;
	asi64(R1) += asi64(R2);
	mc_disasm_basereg = asi64(R1);
	R1 = 2;
	mc_disasm_addrmode = asi64(R1);
	asi64(R1) = mode;
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L1466;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L1467;
	goto L1468;
L1466:
	asi64(R1) = mc_disasm_readsbyte();
	mc_disasm_offset = asi64(R1);
	goto L1465;
L1467:
	asi64(R1) = mc_disasm_readint32();
	mc_disasm_offset = asi64(R1);
	goto L1465;
L1468:
L1465:
L1463:
	goto L1460;
L1462:
	R1 = 2;
	mc_disasm_addrmode = asi64(R1);
	asi64(R1) = mc_disasm_readbyte();
	sib = asi64(R1);
	asi64(R1) = sib;
	R2 = 3;
	asi64(R1) >>= asi64(R2);
	R2 = 7;
	asi64(R1) &= asi64(R2);
	R2 = 1;
	asi64(R1) += asi64(R2);
	mc_disasm_indexreg = asi64(R1);
	asi64(R1) = sib;
	R2 = 7;
	asi64(R1) &= asi64(R2);
	R2 = 1;
	asi64(R1) += asi64(R2);
	mc_disasm_basereg = asi64(R1);
	asi64(R1) = sib;
	R2 = 6;
	asi64(R1) >>= asi64(R2);
	R2 = 1;
	asi64(R1) += asi64(R2);
	switch (asi64(R1)) {
	case 1: goto L1472;
	case 2: goto L1473;
	case 3: goto L1474;
	case 4: goto L1475;
	default: goto L1471;
    };
// SWITCH
L1472:
	R1 = 1;
	goto L1469;
L1473:
	R1 = 2;
	goto L1469;
L1474:
	R1 = 4;
	goto L1469;
L1475:
	R1 = 8;
	goto L1469;
L1471:
	R1 = 0;
L1469:
	mc_disasm_scale = asi64(R1);
	asi64(R1) = mode;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L1477;
	asi64(R1) = mc_disasm_basereg;
	R2 = 6;
	if (asi64(R1) != asi64(R2)) goto L1477;
	asi64(R1) = mc_disasm_indexreg;
	R2 = 5;
	if (asi64(R1) != asi64(R2)) goto L1477;
	R1 = 0;
	R2 = R1;
	mc_disasm_basereg = asi64(R2);
	mc_disasm_indexreg = asi64(R1);
	asi64(R1) = mc_disasm_readint32();
	mc_disasm_offset = asi64(R1);
	goto L1476;
L1477:
	asi64(R1) = mode;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L1478;
	asi64(R1) = mc_disasm_basereg;
	R2 = 6;
	if (asi64(R1) != asi64(R2)) goto L1478;
	R1 = 0;
	mc_disasm_basereg = asi64(R1);
	asi64(R1) = mc_disasm_readint32();
	mc_disasm_offset = asi64(R1);
	goto L1476;
L1478:
	asi64(R1) = mode;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L1479;
	asi64(R1) = mc_disasm_indexreg;
	R2 = 5;
	if (asi64(R1) != asi64(R2)) goto L1479;
	R1 = 0;
	mc_disasm_indexreg = asi64(R1);
	goto L1476;
L1479:
	asi64(R1) = mode;
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L1481;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L1482;
	goto L1483;
L1481:
	asi64(R1) = mc_disasm_readsbyte();
	mc_disasm_offset = asi64(R1);
	goto L1480;
L1482:
	asi64(R1) = mc_disasm_readint32();
	mc_disasm_offset = asi64(R1);
	goto L1480;
L1483:
L1480:
	asi64(R1) = mc_disasm_indexreg;
	R2 = 5;
	if (asi64(R1) != asi64(R2)) goto L1485;
	R1 = 0;
	mc_disasm_indexreg = asi64(R1);
L1485:
L1476:
L1460:
	asi64(R1) = mc_disasm_basereg;
	if (!asi64(R1)) goto L1487;
	asi64(R1) = mc_disasm_rex;
	R2 = 1;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1487;
	R1 = 8;
	R2 = (u64)&mc_disasm_basereg;
	*toi64p(R2) += asi64(R1);
L1487:
	asi64(R1) = mc_disasm_indexreg;
	if (!asi64(R1)) goto L1489;
	asi64(R1) = mc_disasm_rex;
	R2 = 2;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1489;
	R1 = 8;
	R2 = (u64)&mc_disasm_indexreg;
	*toi64p(R2) += asi64(R1);
L1489:
	asi64(R1) = xxx;
	R2 = 1;
	asi64(R1) += asi64(R2);
	mc_disasm_rmreg = asi64(R1);
	asi64(R1) = mc_disasm_rex;
	R2 = 4;
	asi64(R1) &= asi64(R2);
	if (!asi64(R1)) goto L1491;
	R1 = 8;
	R2 = (u64)&mc_disasm_rmreg;
	*toi64p(R2) += asi64(R1);
L1491:
	asi64(R1) = xxx;
	mc_disasm_rmopc = asi64(R1);
	return;
}

static i64 mc_disasm_readbyte() {
    u64 R1, R2; 
	R1 = (u64)&mc_disasm_codeptr;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	asu8(R1) = *tou8p(R1);
	R1 = toi64(tou8(R1));
	goto L1492;
L1492:
	return asi64(R1);
}

static i64 mc_disasm_readsbyte() {
    u64 R1, R2; 
	R1 = (u64)&mc_disasm_codeptr;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	asi8(R1) = *toi8p(R1);
	R1 = toi64(toi8(R1));
	goto L1493;
L1493:
	return asi64(R1);
}

static u64 mc_disasm_readword16() {
    u64 R1, R2; 
	u64 a;
	asu64(R1) = mc_disasm_codeptr;
	asu16(R1) = *tou16p(R1);
	R1 = tou64(tou16(R1));
	a = asu64(R1);
	R1 = 2;
	R2 = (u64)&mc_disasm_codeptr;
	*tou64p(R2) += asu64(R1);
	asu64(R1) = a;
	goto L1494;
L1494:
	return asu64(R1);
}

static i64 mc_disasm_readint16() {
    u64 R1, R2; 
	i64 a;
	asu64(R1) = mc_disasm_codeptr;
	asi16(R1) = *toi16p(R1);
	R1 = toi64(toi16(R1));
	a = asi64(R1);
	R1 = 2;
	R2 = (u64)&mc_disasm_codeptr;
	*tou64p(R2) += asu64(R1);
	asi64(R1) = a;
	goto L1495;
L1495:
	return asi64(R1);
}

static u64 mc_disasm_readword32() {
    u64 R1, R2; 
	u64 a;
	asu64(R1) = mc_disasm_codeptr;
	asu32(R1) = *tou32p(R1);
	R1 = tou64(tou32(R1));
	a = asu64(R1);
	R1 = 4;
	R2 = (u64)&mc_disasm_codeptr;
	*tou64p(R2) += asu64(R1);
	asu64(R1) = a;
	goto L1496;
L1496:
	return asu64(R1);
}

static i64 mc_disasm_readint32() {
    u64 R1, R2; 
	i64 a;
	asu64(R1) = mc_disasm_codeptr;
	asi32(R1) = *toi32p(R1);
	R1 = toi64(toi32(R1));
	a = asi64(R1);
	R1 = 4;
	R2 = (u64)&mc_disasm_codeptr;
	*tou64p(R2) += asu64(R1);
	asi64(R1) = a;
	goto L1497;
L1497:
	return asi64(R1);
}

static i64 mc_disasm_readi64() {
    u64 R1, R2; 
	i64 a;
	asu64(R1) = mc_disasm_codeptr;
	asi64(R1) = *toi64p(R1);
	a = asi64(R1);
	R1 = 8;
	R2 = (u64)&mc_disasm_codeptr;
	*tou64p(R2) += asu64(R1);
	asi64(R1) = a;
	goto L1498;
L1498:
	return asi64(R1);
}

static i64 mc_disasm_getreg(i64 regcode, i64 upper) {
    u64 R1, R2; 
	asi64(R1) = upper;
	if (!asi64(R1)) goto L1501;
	asi64(R1) = regcode;
	R2 = 8;
	asi64(R1) += asi64(R2);
	R2 = 1;
	asi64(R1) += asi64(R2);
	goto L1499;
L1501:
	asi64(R1) = regcode;
	R2 = 1;
	asi64(R1) += asi64(R2);
	goto L1499;
L1499:
	return asi64(R1);
}

static u64 mc_disasm_strreg(i64 reg, i64 opsize) {
    u64 R1, R2; 
// PROC LOCAL STATICS GO HERE
	static struct $B19 mc_disasm_strreg_regnames8 = {{
	(u64)"al",
	(u64)"cl",
	(u64)"dl",
	(u64)"bl",
	(u64)"spl",
	(u64)"bpl",
	(u64)"sil",
	(u64)"dil",
	(u64)"r8b",
	(u64)"r9b",
	(u64)"r10b",
	(u64)"r11b",
	(u64)"r12b",
	(u64)"r13b",
	(u64)"r14b",
	(u64)"r15b",
	(u64)"ah",
	(u64)"bh",
	(u64)"ch",
	(u64)"dh"    }};
	static struct $B24 mc_disasm_strreg_regnames16 = {{
	(u64)"ax",
	(u64)"cx",
	(u64)"dx",
	(u64)"bx",
	(u64)"sp",
	(u64)"bp",
	(u64)"si",
	(u64)"di",
	(u64)"r8w",
	(u64)"r9w",
	(u64)"r10w",
	(u64)"r11w",
	(u64)"r12w",
	(u64)"r13w",
	(u64)"r14w",
	(u64)"r15w"    }};
	static struct $B24 mc_disasm_strreg_regnames32 = {{
	(u64)"eax",
	(u64)"ecx",
	(u64)"edx",
	(u64)"ebx",
	(u64)"esp",
	(u64)"ebp",
	(u64)"esi",
	(u64)"edi",
	(u64)"r8d",
	(u64)"r9d",
	(u64)"r10d",
	(u64)"r11d",
	(u64)"r12d",
	(u64)"r13d",
	(u64)"r14d",
	(u64)"r15d"    }};
	static struct $B24 mc_disasm_strreg_regnames64 = {{
	(u64)"rax",
	(u64)"rcx",
	(u64)"rdx",
	(u64)"rbx",
	(u64)"rsp",
	(u64)"rbp",
	(u64)"rsi",
	(u64)"rdi",
	(u64)"r8",
	(u64)"r9",
	(u64)"r10",
	(u64)"r11",
	(u64)"r12",
	(u64)"r13",
	(u64)"r14",
	(u64)"r15"    }};
	static struct $B19 mc_disasm_strreg_mregnames8 = {{
	(u64)"B0",
	(u64)"B10",
	(u64)"B11",
	(u64)"B4",
	(u64)"B15",
	(u64)"B14",
	(u64)"B5",
	(u64)"B3",
	(u64)"B12",
	(u64)"B13",
	(u64)"B1",
	(u64)"B2",
	(u64)"B6",
	(u64)"B7",
	(u64)"B8",
	(u64)"B9",
	(u64)"B16",
	(u64)"B18",
	(u64)"B19",
	(u64)"B17"    }};
	static struct $B24 mc_disasm_strreg_mregnames16 = {{
	(u64)"W0",
	(u64)"W10",
	(u64)"W11",
	(u64)"W4",
	(u64)"Wsp",
	(u64)"Wbp",
	(u64)"W5",
	(u64)"W3",
	(u64)"W12",
	(u64)"W13",
	(u64)"W1",
	(u64)"W2",
	(u64)"W6",
	(u64)"W7",
	(u64)"W8",
	(u64)"W9"    }};
	static struct $B24 mc_disasm_strreg_mregnames32 = {{
	(u64)"A0",
	(u64)"A10",
	(u64)"A11",
	(u64)"A4",
	(u64)"Astack",
	(u64)"Aframe",
	(u64)"A5",
	(u64)"A3",
	(u64)"A12",
	(u64)"A13",
	(u64)"A1",
	(u64)"A2",
	(u64)"A6",
	(u64)"A7",
	(u64)"A8",
	(u64)"A9"    }};
	static struct $B24 mc_disasm_strreg_mregnames64 = {{
	(u64)"D0",
	(u64)"D10",
	(u64)"D11",
	(u64)"D4",
	(u64)"Dstack",
	(u64)"Dframe",
	(u64)"D5",
	(u64)"D3",
	(u64)"D12",
	(u64)"D13",
	(u64)"D1",
	(u64)"D2",
	(u64)"D6",
	(u64)"D7",
	(u64)"D8",
	(u64)"D9"    }};
	asi64(R1) = reg;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L1504;
	R1 = tou64("<>");
	goto L1502;
L1504:
	asi64(R1) = opsize;
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L1506;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L1507;
	R2 = 4;
	if (asi64(R1) == asi64(R2)) goto L1508;
	R2 = 8;
	if (asi64(R1) == asi64(R2)) goto L1509;
	goto L1510;
L1506:
	R1 = (u64)&mc_disasm_strreg_mregnames8;
	asi64(R2) = reg;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	goto L1502;
	goto L1505;
L1507:
	R1 = (u64)&mc_disasm_strreg_mregnames16;
	asi64(R2) = reg;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	goto L1502;
	goto L1505;
L1508:
	R1 = (u64)&mc_disasm_strreg_mregnames32;
	asi64(R2) = reg;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	goto L1502;
	goto L1505;
L1509:
	R1 = (u64)&mc_disasm_strreg_mregnames64;
	asi64(R2) = reg;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	goto L1502;
	goto L1505;
L1510:
L1505:
	R1 = tou64("");
	goto L1502;
L1502:
	return asu64(R1);
}

static u64 mc_disasm_strfreg(i64 freg) {
    u64 R1, R2; 
// PROC LOCAL STATICS GO HERE
	static struct $B23 mc_disasm_strfreg_fregnames = {{
	(u64)"st0",
	(u64)"st1",
	(u64)"st2",
	(u64)"st3",
	(u64)"st4",
	(u64)"st5",
	(u64)"st6",
	(u64)"st7"    }};
	R1 = (u64)&mc_disasm_strfreg_fregnames;
	asi64(R2) = freg;
	asu64(R1) = *tou64p(((i64)R1+(i64)R2*8-8));
	goto L1511;
L1511:
	return asu64(R1);
}

static void mc_disasm_printaddrmode(i64 xmm) {
    u64 R1, R2; 
	u64 plus;
	i64 addrsize;
	R1 = tou64(" ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_addrmode;
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L1514;
	goto L1515;
L1514:
	asi64(R1) = xmm;
	if (!asi64(R1)) goto L1517;
	asi64(R1) = mc_disasm_basereg;
	asu64(R1) = mc_disasm_strxmm(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1516;
L1517:
	R1 = (u64)&mc_disasm_basereg;
	mc_disasm_getsilx(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	asi64(R2) = mc_disasm_basereg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
L1516:
	goto L1512;
	goto L1513;
L1515:
L1513:
	asi64(R1) = mc_disasm_opsize;
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L1519;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L1520;
	R2 = 4;
	if (asi64(R1) == asi64(R2)) goto L1521;
	R2 = 8;
	if (asi64(R1) == asi64(R2)) goto L1522;
	R2 = 10;
	if (asi64(R1) == asi64(R2)) goto L1523;
	R2 = 16;
	if (asi64(R1) == asi64(R2)) goto L1524;
	goto L1525;
L1519:
	R1 = tou64("byte ");
	mc_disasm_genstr(asu64(R1));
	goto L1518;
L1520:
	R1 = tou64("word ");
	mc_disasm_genstr(asu64(R1));
	goto L1518;
L1521:
	R1 = tou64("dword ");
	mc_disasm_genstr(asu64(R1));
	goto L1518;
L1522:
	R1 = tou64("qword ");
	mc_disasm_genstr(asu64(R1));
	goto L1518;
L1523:
	R1 = tou64("tword ");
	mc_disasm_genstr(asu64(R1));
	goto L1518;
L1524:
	R1 = tou64("oword ");
	mc_disasm_genstr(asu64(R1));
	goto L1518;
L1525:
	msysc_m$print_startcon();
	R1 = tou64("///OPSIZE");
	msysc_m$print_str_nf(asu64(R1));
	asi64(R1) = mc_disasm_opsize;
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_newline();
	msysc_m$print_end();
L1518:
	asi64(R1) = mc_disasm_ripmode;
	if (!asi64(R1)) goto L1527;
	R1 = tou64("rip:");
	mc_disasm_genstr(asu64(R1));
L1527:
	R1 = tou64("[");
	mc_disasm_genstr(asu64(R1));
	R1 = tou64("");
	plus = asu64(R1);
	asi64(R1) = mc_disasm_addroverride;
	if (!asi64(R1)) goto L1529;
	R1 = 4;
	goto L1528;
L1529:
	R1 = 8;
L1528:
	addrsize = asi64(R1);
	asi64(R1) = mc_disasm_basereg;
	if (!asi64(R1)) goto L1531;
	asi64(R1) = addrsize;
	asi64(R2) = mc_disasm_basereg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64("+");
	plus = asu64(R1);
L1531:
	asi64(R1) = mc_disasm_indexreg;
	if (!asi64(R1)) goto L1533;
	asu64(R1) = plus;
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = addrsize;
	asi64(R2) = mc_disasm_indexreg;
	asu64(R1) = mc_disasm_strreg(asi64(R2), asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64("<INDEX>");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_scale;
	R2 = 1;
	if (asi64(R1) <= asi64(R2)) goto L1535;
	R1 = tou64("*");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mc_disasm_scale;
	mc_disasm_genintd(asi64(R1));
L1535:
	R1 = tou64("+");
	plus = asu64(R1);
L1533:
	asi64(R1) = mc_disasm_offset;
	if (asi64(R1)) goto L1538;
	asi64(R1) = mc_disasm_basereg;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L1537;
	asi64(R1) = mc_disasm_indexreg;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L1537;
L1538:
	asi64(R1) = mc_disasm_basereg;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L1540;
	asi64(R1) = mc_disasm_indexreg;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L1540;
	asi64(R1) = mc_disasm_offset;
	mc_disasm_genhex(asi64(R1));
	goto L1539;
L1540:
	asi64(R1) = mc_disasm_offset;
	R2 = 0;
	if (asi64(R1) <= asi64(R2)) goto L1542;
	asu64(R1) = plus;
	mc_disasm_genstr(asu64(R1));
L1542:
	asi64(R1) = mc_disasm_offset;
	mc_disasm_genintd(asi64(R1));
L1539:
L1537:
	R1 = tou64("]");
	mc_disasm_genstr(asu64(R1));
L1512:
	return;
}

static void mc_disasm_genstr(u64 s) {
    u64 R1, R2; 
	asu64(R1) = s;
	R2 = (u64)&mc_disasm_deststr;
	asu64(R1) = strcat(asu64(R2), asu64(R1));
	return;
}

static void mc_disasm_genintd(i64 a) {
    u64 R1, R2; 
	R1 = 0;
	asi64(R2) = a;
	asu64(R1) = msysc_strint(asi64(R2), asu64(R1));
	mc_disasm_genstr(asu64(R1));
	return;
}

static void mc_disasm_genhex(i64 a) {
    u64 R1, R2; 
	R1 = tou64("h");
	asi64(R2) = a;
	asu64(R1) = msysc_strint(asi64(R2), asu64(R1));
	mc_disasm_genstr(asu64(R1));
	return;
}

static i64 mc_disasm_readimm() {
    u64 R1, R2; 
	asi64(R1) = mc_disasm_opsize;
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L1548;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L1549;
	R2 = 4;
	if (asi64(R1) == asi64(R2)) goto L1550;
	R2 = 8;
	if (asi64(R1) == asi64(R2)) goto L1550;
	goto L1551;
L1548:
	asi64(R1) = mc_disasm_readsbyte();
	goto L1546;
	goto L1547;
L1549:
	asi64(R1) = mc_disasm_readint16();
	goto L1546;
	goto L1547;
L1550:
	asi64(R1) = mc_disasm_readint32();
	goto L1546;
	goto L1547;
L1551:
L1547:
	R1 = 0;
	goto L1546;
L1546:
	return asi64(R1);
}

static i64 mc_disasm_readimm8() {
    u64 R1, R2; 
	asi64(R1) = mc_disasm_opsize;
	R2 = 8;
	if (asi64(R1) >= asi64(R2)) goto L1554;
	asi64(R1) = mc_disasm_readimm();
	goto L1552;
L1554:
	asi64(R1) = mc_disasm_readi64();
	goto L1552;
L1552:
	return asi64(R1);
}

static u64 mc_disasm_strxmm(i64 reg) {
    u64 R1, R2; 
	R1 = (u64)&mc_disasm_strxmm_str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("xmm");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_nogap();
	asi64(R1) = reg;
	R2 = 1;
	asi64(R1) -= asi64(R2);
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_end();
	R1 = (u64)&mc_disasm_strxmm_str;
	goto L1555;
L1555:
	return asu64(R1);
}

static u64 mc_disasm_strmmx(i64 reg) {
    u64 R1, R2; 
	R1 = (u64)&mc_disasm_strmmx_str;
	msysc_m$print_startstr(asu64(R1));
	R1 = tou64("mmx");
	msysc_m$print_str_nf(asu64(R1));
	msysc_m$print_nogap();
	asi64(R1) = reg;
	R2 = 1;
	asi64(R1) -= asi64(R2);
	msysc_m$print_i64_nf(asi64(R1));
	msysc_m$print_end();
	R1 = (u64)&mc_disasm_strmmx_str;
	goto L1556;
L1556:
	return asu64(R1);
}

static void mc_disasm_decode8087(i64 ttt) {
    u64 R1, R2, R3; 
	u8 bb;
	i64 longopc;
	i64 freg;
	i64 shortopc;
	i64 code;
	R1 = (u64)&mc_disasm_codeptr;
	asu64(R2) = *tou64p(R1); *(tou64p(R1)) += 1; asu64(R1) = asu64(R2);
	asu8(R1) = *tou8p(R1);
	bb = asu8(R1);
	asi64(R1) = ttt;
	R2 = 8;
	asi64(R1) <<= asi64(R2);
	asu8(R2) = bb;
	R2 = toi64(tou8(R2));
	asi64(R1) += asi64(R2);
	longopc = asi64(R1);
	asu8(R1) = bb;
	R1 = toi64(tou8(R1));
	R2 = 7;
	asi64(R1) &= asi64(R2);
	R2 = 1;
	asi64(R1) += asi64(R2);
	freg = asi64(R1);
	asi64(R1) = longopc;
	R2 = 1753;
	if (asi64(R1) == asi64(R2)) goto L1559;
	R2 = 484;
	if (asi64(R1) == asi64(R2)) goto L1560;
	R2 = 485;
	if (asi64(R1) == asi64(R2)) goto L1561;
	R2 = 494;
	if (asi64(R1) == asi64(R2)) goto L1562;
	R2 = 488;
	if (asi64(R1) == asi64(R2)) goto L1563;
	R2 = 491;
	if (asi64(R1) == asi64(R2)) goto L1564;
	R2 = 489;
	if (asi64(R1) == asi64(R2)) goto L1565;
	R2 = 490;
	if (asi64(R1) == asi64(R2)) goto L1566;
	R2 = 492;
	if (asi64(R1) == asi64(R2)) goto L1567;
	R2 = 493;
	if (asi64(R1) == asi64(R2)) goto L1568;
	R2 = 506;
	if (asi64(R1) == asi64(R2)) goto L1569;
	R2 = 510;
	if (asi64(R1) == asi64(R2)) goto L1570;
	R2 = 511;
	if (asi64(R1) == asi64(R2)) goto L1571;
	R2 = 507;
	if (asi64(R1) == asi64(R2)) goto L1572;
	R2 = 509;
	if (asi64(R1) == asi64(R2)) goto L1573;
	R2 = 504;
	if (asi64(R1) == asi64(R2)) goto L1574;
	R2 = 508;
	if (asi64(R1) == asi64(R2)) goto L1575;
	R2 = 500;
	if (asi64(R1) == asi64(R2)) goto L1576;
	R2 = 481;
	if (asi64(R1) == asi64(R2)) goto L1577;
	R2 = 480;
	if (asi64(R1) == asi64(R2)) goto L1578;
	R2 = 498;
	if (asi64(R1) == asi64(R2)) goto L1579;
	R2 = 499;
	if (asi64(R1) == asi64(R2)) goto L1580;
	R2 = 496;
	if (asi64(R1) == asi64(R2)) goto L1581;
	R2 = 497;
	if (asi64(R1) == asi64(R2)) goto L1582;
	R2 = 505;
	if (asi64(R1) == asi64(R2)) goto L1583;
	R2 = 995;
	if (asi64(R1) == asi64(R2)) goto L1584;
	R2 = 992;
	if (asi64(R1) == asi64(R2)) goto L1585;
	R2 = 993;
	if (asi64(R1) == asi64(R2)) goto L1586;
	R2 = 994;
	if (asi64(R1) == asi64(R2)) goto L1587;
	R2 = 503;
	if (asi64(R1) == asi64(R2)) goto L1588;
	R2 = 502;
	if (asi64(R1) == asi64(R2)) goto L1589;
	R2 = 464;
	if (asi64(R1) == asi64(R2)) goto L1590;
	goto L1591;
L1559:
	R1 = tou64("fcompp");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1560:
	R1 = tou64("ftst");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1561:
	R1 = tou64("fxam");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1562:
	R1 = tou64("fldz");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1563:
	R1 = tou64("fld1");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1564:
	R1 = tou64("fldpi");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1565:
	R1 = tou64("fldl2t");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1566:
	R1 = tou64("fldl2e");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1567:
	R1 = tou64("fldlg2");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1568:
	R1 = tou64("fldln2");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1569:
	R1 = tou64("fsqrt");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1570:
	R1 = tou64("fsin");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1571:
	R1 = tou64("fcos");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1572:
	R1 = tou64("fsincos");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1573:
	R1 = tou64("fscale");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1574:
	R1 = tou64("fprem");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1575:
	R1 = tou64("frndint");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1576:
	R1 = tou64("fxtract");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1577:
	R1 = tou64("fabs");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1578:
	R1 = tou64("fchs");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1579:
	R1 = tou64("fptan");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1580:
	R1 = tou64("fpatan");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1581:
	R1 = tou64("f2xm1");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1582:
	R1 = tou64("fyl2x");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1583:
	R1 = tou64("fyl2xp1");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1584:
	R1 = tou64("finit");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1585:
	R1 = tou64("feni");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1586:
	R1 = tou64("fdisi");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1587:
	R1 = tou64("fclex");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1588:
	R1 = tou64("fincstp");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1589:
	R1 = tou64("fdecstp");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1590:
	R1 = tou64("fnop");
	mc_disasm_genstr(asu64(R1));
	goto L1558;
L1591:
	asi64(R1) = longopc;
	R2 = 2040;
	asi64(R1) &= asi64(R2);
	R2 = 448;
	if (asi64(R1) == asi64(R2)) goto L1593;
	R2 = 1488;
	if (asi64(R1) == asi64(R2)) goto L1594;
	R2 = 1496;
	if (asi64(R1) == asi64(R2)) goto L1595;
	R2 = 456;
	if (asi64(R1) == asi64(R2)) goto L1596;
	R2 = 208;
	if (asi64(R1) == asi64(R2)) goto L1597;
	R2 = 216;
	if (asi64(R1) == asi64(R2)) goto L1598;
	R2 = 1472;
	if (asi64(R1) == asi64(R2)) goto L1599;
	goto L1600;
L1593:
	R1 = tou64("fld ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = freg;
	asu64(R1) = mc_disasm_strfreg(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1592;
L1594:
	R1 = tou64("fst ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = freg;
	asu64(R1) = mc_disasm_strfreg(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1592;
L1595:
	R1 = tou64("fstp ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = freg;
	asu64(R1) = mc_disasm_strfreg(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1592;
L1596:
	R1 = tou64("fxch ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = freg;
	asu64(R1) = mc_disasm_strfreg(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1592;
L1597:
	R1 = tou64("fcom ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = freg;
	asu64(R1) = mc_disasm_strfreg(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1592;
L1598:
	R1 = tou64("fcomp ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = freg;
	asu64(R1) = mc_disasm_strfreg(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1592;
L1599:
	R1 = tou64("ffree ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = freg;
	asu64(R1) = mc_disasm_strfreg(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1592;
L1600:
	asi64(R1) = longopc;
	R2 = 504;
	asi64(R1) &= asi64(R2);
	R2 = 192;
	if (asi64(R1) == asi64(R2)) goto L1602;
	R2 = 224;
	if (asi64(R1) == asi64(R2)) goto L1603;
	R2 = 232;
	if (asi64(R1) == asi64(R2)) goto L1604;
	R2 = 200;
	if (asi64(R1) == asi64(R2)) goto L1605;
	R2 = 240;
	if (asi64(R1) == asi64(R2)) goto L1606;
	R2 = 248;
	if (asi64(R1) == asi64(R2)) goto L1607;
	goto L1608;
L1602:
	asi64(R1) = freg;
	asi64(R2) = ttt;
	R3 = tou64("fadd");
	mc_disasm_do87arith(asu64(R3), asi64(R2), asi64(R1));
	goto L1601;
L1603:
	asi64(R1) = freg;
	asi64(R2) = ttt;
	R3 = tou64("fsub");
	mc_disasm_do87arith(asu64(R3), asi64(R2), asi64(R1));
	goto L1601;
L1604:
	asi64(R1) = freg;
	asi64(R2) = ttt;
	R3 = tou64("fsubr");
	mc_disasm_do87arith(asu64(R3), asi64(R2), asi64(R1));
	goto L1601;
L1605:
	asi64(R1) = freg;
	asi64(R2) = ttt;
	R3 = tou64("fmul");
	mc_disasm_do87arith(asu64(R3), asi64(R2), asi64(R1));
	goto L1601;
L1606:
	asi64(R1) = freg;
	asi64(R2) = ttt;
	R3 = tou64("fdiv");
	mc_disasm_do87arith(asu64(R3), asi64(R2), asi64(R1));
	goto L1601;
L1607:
	asi64(R1) = freg;
	asi64(R2) = ttt;
	R3 = tou64("fdivr");
	mc_disasm_do87arith(asu64(R3), asi64(R2), asi64(R1));
	goto L1601;
L1608:
	R1 = (u64)&mc_disasm_codeptr;
	(*tou64p(R1)) -=1;
	R1 = 0;
	mc_disasm_decodeaddr(asi64(R1));
	asi64(R1) = ttt;
	R2 = 3;
	asi64(R1) <<= asi64(R2);
	asi64(R2) = mc_disasm_rmopc;
	asi64(R1) += asi64(R2);
	shortopc = asi64(R1);
	asi64(R1) = shortopc;
	R2 = 61;
	if (asi64(R1) == asi64(R2)) goto L1610;
	R2 = 29;
	if (asi64(R1) == asi64(R2)) goto L1611;
	R2 = 60;
	if (asi64(R1) == asi64(R2)) goto L1612;
	R2 = 63;
	if (asi64(R1) == asi64(R2)) goto L1613;
	R2 = 31;
	if (asi64(R1) == asi64(R2)) goto L1614;
	R2 = 62;
	if (asi64(R1) == asi64(R2)) goto L1615;
	R2 = 13;
	if (asi64(R1) == asi64(R2)) goto L1616;
	R2 = 15;
	if (asi64(R1) == asi64(R2)) goto L1617;
	R2 = 47;
	if (asi64(R1) == asi64(R2)) goto L1618;
	R2 = 14;
	if (asi64(R1) == asi64(R2)) goto L1619;
	R2 = 12;
	if (asi64(R1) == asi64(R2)) goto L1620;
	R2 = 46;
	if (asi64(R1) == asi64(R2)) goto L1621;
	R2 = 44;
	if (asi64(R1) == asi64(R2)) goto L1622;
	goto L1623;
L1610:
	R1 = 4;
	R2 = tou64("fld");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1609;
L1611:
	R1 = 5;
	R2 = tou64("fld");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1609;
L1612:
	R1 = -1;
	R2 = tou64("fldbcd");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1609;
L1613:
	R1 = 4;
	R2 = tou64("fstp");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1609;
L1614:
	R1 = 5;
	R2 = tou64("fstp");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1609;
L1615:
	R1 = -1;
	R2 = tou64("fstpbcd");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1609;
L1616:
	R1 = -1;
	R2 = tou64("fldcw");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1609;
L1617:
	R1 = -1;
	R2 = tou64("fstcw");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1609;
L1618:
	R1 = -1;
	R2 = tou64("fstsw");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1609;
L1619:
	R1 = -1;
	R2 = tou64("fstenv");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1609;
L1620:
	R1 = -1;
	R2 = tou64("fldenv");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1609;
L1621:
	R1 = -1;
	R2 = tou64("fsave");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1609;
L1622:
	R1 = -1;
	R2 = tou64("frstor");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1609;
L1623:
	asi64(R1) = shortopc;
	R2 = 15;
	asi64(R1) &= asi64(R2);
	R2 = 8;
	if (asi64(R1) == asi64(R2)) goto L1625;
	R2 = 10;
	if (asi64(R1) == asi64(R2)) goto L1626;
	R2 = 11;
	if (asi64(R1) == asi64(R2)) goto L1627;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L1628;
	R2 = 3;
	if (asi64(R1) == asi64(R2)) goto L1629;
	R2 = 0;
	if (asi64(R1) == asi64(R2)) goto L1630;
	R2 = 4;
	if (asi64(R1) == asi64(R2)) goto L1631;
	R2 = 5;
	if (asi64(R1) == asi64(R2)) goto L1632;
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L1633;
	R2 = 6;
	if (asi64(R1) == asi64(R2)) goto L1634;
	R2 = 7;
	if (asi64(R1) == asi64(R2)) goto L1635;
	goto L1636;
L1625:
	asi64(R1) = ttt;
	R2 = 1;
	asi64(R1) >>= asi64(R2);
	R2 = tou64("fld");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1624;
L1626:
	asi64(R1) = ttt;
	R2 = 1;
	asi64(R1) >>= asi64(R2);
	R2 = tou64("fst");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1624;
L1627:
	asi64(R1) = ttt;
	R2 = 1;
	asi64(R1) >>= asi64(R2);
	R2 = tou64("fstp");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1624;
L1628:
	asi64(R1) = ttt;
	R2 = 1;
	asi64(R1) >>= asi64(R2);
	R2 = tou64("fcom");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1624;
L1629:
	asi64(R1) = ttt;
	R2 = 1;
	asi64(R1) >>= asi64(R2);
	R2 = tou64("fcomp");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1624;
L1630:
	asi64(R1) = ttt;
	R2 = 1;
	asi64(R1) >>= asi64(R2);
	R2 = tou64("fadd");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1624;
L1631:
	asi64(R1) = ttt;
	R2 = 1;
	asi64(R1) >>= asi64(R2);
	R2 = tou64("fsub");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1624;
L1632:
	asi64(R1) = ttt;
	R2 = 1;
	asi64(R1) >>= asi64(R2);
	R2 = tou64("fsubr");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1624;
L1633:
	asi64(R1) = ttt;
	R2 = 1;
	asi64(R1) >>= asi64(R2);
	R2 = tou64("fmul");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1624;
L1634:
	asi64(R1) = ttt;
	R2 = 1;
	asi64(R1) >>= asi64(R2);
	R2 = tou64("fdiv");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1624;
L1635:
	asi64(R1) = ttt;
	R2 = 1;
	asi64(R1) >>= asi64(R2);
	R2 = tou64("fdivr");
	mc_disasm_do87mem(asu64(R2), asi64(R1));
	goto L1624;
L1636:
	R1 = tou64("UNKNOWN x87 OPCODE");
	mc_disasm_genstr(asu64(R1));
L1624:
L1609:
L1601:
L1592:
L1558:
	return;
}

static void mc_disasm_do87arith(u64 opcstr, i64 ttt, i64 freg) {
    u64 R1, R2; 
	i64 d;
	i64 p;
	asi64(R1) = ttt;
	R2 = 4;
	asi64(R1) &= asi64(R2);
	d = asi64(R1);
	asi64(R1) = ttt;
	R2 = 2;
	asi64(R1) &= asi64(R2);
	p = asi64(R1);
	asu64(R1) = opcstr;
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = p;
	if (!asi64(R1)) goto L1639;
	R1 = tou64("p");
	mc_disasm_genstr(asu64(R1));
L1639:
	R1 = tou64(" ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = d;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L1641;
	R1 = tou64("st0, ");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = freg;
	asu64(R1) = mc_disasm_strfreg(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	goto L1640;
L1641:
	asi64(R1) = freg;
	asu64(R1) = mc_disasm_strfreg(asi64(R1));
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(", st0");
	mc_disasm_genstr(asu64(R1));
L1640:
	return;
}

static void mc_disasm_do87mem(u64 opcstr, i64 mf) {
    u64 R1, R2; 
	R1 = tou64("f");
	mc_disasm_genstr(asu64(R1));
	asi64(R1) = mf;
	R2 = 0;
	if (asi64(R1) == asi64(R2)) goto L1644;
	R2 = 1;
	if (asi64(R1) == asi64(R2)) goto L1645;
	R2 = 2;
	if (asi64(R1) == asi64(R2)) goto L1646;
	R2 = 3;
	if (asi64(R1) == asi64(R2)) goto L1647;
	R2 = 4;
	if (asi64(R1) == asi64(R2)) goto L1648;
	R2 = 5;
	if (asi64(R1) == asi64(R2)) goto L1649;
	goto L1650;
L1644:
	R1 = 4;
	mc_disasm_opsize = asi64(R1);
	goto L1643;
L1645:
	R1 = tou64("i");
	mc_disasm_genstr(asu64(R1));
	R1 = 4;
	mc_disasm_opsize = asi64(R1);
	goto L1643;
L1646:
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
	goto L1643;
L1647:
	R1 = tou64("i");
	mc_disasm_genstr(asu64(R1));
	R1 = 2;
	mc_disasm_opsize = asi64(R1);
	goto L1643;
L1648:
	R1 = tou64("i");
	mc_disasm_genstr(asu64(R1));
	R1 = 8;
	mc_disasm_opsize = asi64(R1);
	goto L1643;
L1649:
	R1 = 10;
	mc_disasm_opsize = asi64(R1);
	goto L1643;
L1650:
L1643:
	asu64(R1) = opcstr;
	R2 = 1;
	R1 += (i64)R2;
	mc_disasm_genstr(asu64(R1));
	R1 = tou64(" ");
	mc_disasm_genstr(asu64(R1));
	R1 = 0;
	mc_disasm_printaddrmode(asi64(R1));
	return;
}

static void mc_disasm_getsil(u64 reg) {
    u64 R1, R2; 
	asi64(R1) = mc_disasm_opsize;
	R2 = 1;
	if (asi64(R1) != asi64(R2)) goto L1653;
	asi64(R1) = mc_disasm_rex;
	if (asi64(R1)) goto L1653;
	asu64(R1) = reg;
	asi64(R1) = *toi64p(R1);
	R2 = 5;
	if (asi64(R1) < asi64(R2)) goto L1653;
	asu64(R1) = reg;
	asi64(R1) = *toi64p(R1);
	R2 = 8;
	if (asi64(R1) > asi64(R2)) goto L1653;
	asu64(R1) = reg;
	asi64(R1) = *toi64p(R1);
	R2 = 5;
	if (asi64(R1) == asi64(R2)) goto L1655;
	R2 = 6;
	if (asi64(R1) == asi64(R2)) goto L1656;
	R2 = 7;
	if (asi64(R1) == asi64(R2)) goto L1657;
	R2 = 8;
	if (asi64(R1) == asi64(R2)) goto L1658;
	goto L1659;
L1655:
	R1 = 17;
	asu64(R2) = reg;
	*toi64p(R2) = asi64(R1);
	goto L1654;
L1656:
	R1 = 19;
	asu64(R2) = reg;
	*toi64p(R2) = asi64(R1);
	goto L1654;
L1657:
	R1 = 20;
	asu64(R2) = reg;
	*toi64p(R2) = asi64(R1);
	goto L1654;
L1658:
	R1 = 18;
	asu64(R2) = reg;
	*toi64p(R2) = asi64(R1);
	goto L1654;
L1659:
L1654:
L1653:
	return;
}

static void mc_disasm_getsilx(u64 reg) {
    u64 R1, R2; 
	asi64(R1) = mc_disasm_addrmode;
	R2 = 1;
	if (asi64(R1) != asi64(R2)) goto L1662;
	asi64(R1) = mc_disasm_opsize;
	R2 = 1;
	if (asi64(R1) != asi64(R2)) goto L1662;
	asi64(R1) = mc_disasm_rex;
	R2 = 0;
	if (asi64(R1) != asi64(R2)) goto L1662;
	asu64(R1) = reg;
	asi64(R1) = *toi64p(R1);
	R2 = 5;
	if (asi64(R1) < asi64(R2)) goto L1662;
	asu64(R1) = reg;
	asi64(R1) = *toi64p(R1);
	R2 = 8;
	if (asi64(R1) > asi64(R2)) goto L1662;
	asu64(R1) = reg;
	asi64(R1) = *toi64p(R1);
	R2 = 5;
	if (asi64(R1) == asi64(R2)) goto L1664;
	R2 = 6;
	if (asi64(R1) == asi64(R2)) goto L1665;
	R2 = 7;
	if (asi64(R1) == asi64(R2)) goto L1666;
	R2 = 8;
	if (asi64(R1) == asi64(R2)) goto L1667;
	goto L1668;
L1664:
	R1 = 17;
	asu64(R2) = reg;
	*toi64p(R2) = asi64(R1);
	goto L1663;
L1665:
	R1 = 19;
	asu64(R2) = reg;
	*toi64p(R2) = asi64(R1);
	goto L1663;
L1666:
	R1 = 20;
	asu64(R2) = reg;
	*toi64p(R2) = asi64(R1);
	goto L1663;
L1667:
	R1 = 18;
	asu64(R2) = reg;
	*toi64p(R2) = asi64(R1);
	goto L1663;
L1668:
L1663:
L1662:
	return;
}

static struct $B28 $procaddr;

static struct $B28 $procname;

static i64 $nprocs = 0;

// ***** PCL Support Library *****

i64 Getdotindex(u64 a, int i) {
	return (a & (1LL<<i))>>i;
}

u64 Setdotindex(u64 a, i64 i, i64 x) {
	return (a & ~(1LL<<i)) | ((u64)(x)<<i);
}

i64 Getdotslice(u64 a, i64 i, i64 j) {
	if (i>=j)
		return (a>>j) & ~(0xFFFFFFFFFFFFFFFF<<(i-j+1));
	else
		return (a>>i) & ~(0xFFFFFFFFFFFFFFFF<<(j-i+1));
}

u64 Setdotslice(u64 a, i64 i, i64 j, u64 x) {
	u64 mask64;
	if (i>j) {i64 t=i; i=j; j=t;}

	mask64=~((0xFFFFFFFFFFFFFFFF<<(j-i+1)))<<i;
	return (a & ~mask64) ^ (x<<i);
}

i64 Poweri64(i64 a, i64 n) {
	if (n<0)
		return 0;
	else if (n==0)
		return 1;
	else if ((n&1)==0)
		return Poweri64(a*a, n/2);
	else
		return Poweri64(a*a, (n-1)/2)*a;
}

// End of C Code

