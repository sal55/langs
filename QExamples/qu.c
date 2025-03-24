/*
Q Interpreter

Requirements:

* 64-bit compiler
* C compiler with label pointers extensions

Linux build instructions for qu.c:

 gcc qu.c -o qu -lm -ldl -fno-builtin

(Add -O3 etc as needed)

Run hello.q for example using:

 ./qu hello




*/


#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"

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

i64 $rtemp;


/* Forward Struct Declarations */
struct qq_decls$procrec;
struct qq_decls$userxrec;
struct qq_decls$strec;
struct qq_decls$lexrec;
struct qq_decls$uflagsrec;
struct qq_decls$fieldrec;
struct qq_decls$unitrec;
struct qq_decls$locrec;
struct qq_decls$genfieldrec;
struct qq_decls$filerec;
struct qq_decls$subprogrec;
struct qq_decls$varrec;
struct qq_decls$objrec;
struct qq_decimal$constrec;
struct qq_host$dimrec;
struct qq_parse$readterm$dummy;
struct qq_pcltabs$pclrec;
struct qq_pcltabs$bintorec;
struct msysc$procinforec;
struct msysc$fmtrec;
struct mlib$strbuffer;
struct mlinux$termios;
struct mlinux$timeval;
struct mlinux$tm_rec;
struct mlinux$rsystemtime;

/* Struct Definitions */
struct qq_decls$procrec {
    struct qq_decls$strec *  def;
    struct qq_decls$procrec* nextproc;
};

struct qq_decls$userxrec {
    struct qq_decls$strec *  owner;
    i16 *  pmode;
    struct qq_decls$userxrec* nextmode;
};

struct qq_decls$strec {
    u8 *  name;
    struct qq_decls$strec* owner;
    struct qq_decls$strec* deflist;
    struct qq_decls$strec* deflistx;
    struct qq_decls$strec* nextdef;
    struct qq_decls$strec* nextdupl;
    struct qq_decls$strec* firstdupl;
    struct qq_decls$strec* alias;
    struct qq_decls$strec* nextstatic;
    struct qq_decls$strec* nextproc;
    union {
        u64 a;
        struct qq_pcltabs$pclrec *  labelref;
        struct qq_decls$varrec *  varptr;
        u8 *  truename;
        struct qq_decls$strec* atfield;
        i64 labelno;
    };
    union {
        u64 b;
        struct qq_decls$unitrec *  code;
        struct qq_decls$strec** topfieldlist;
    };
    union {
        u64 c;
        struct {
            i32 index;
            i32 capindex;
        };
    };
    union {
        u64 d;
        struct {
            i16 nparams;
            i16 nlocals;
        };
        struct {
            i16 nfields;
            i16 maxalign;
            i16 fieldoffset;
            i16 baseclassindex;
        };
        i64 genfieldindex;
    };
    u16 subcode;
    byte moduleno;
    byte subprogno;
    i16 mode;
    i16 hint;
    u16 flags;
    byte forindex;
    byte symbolcode;
    byte nameid;
    byte mutable;
    byte namelen;
    byte procfixed;
};

struct qq_decls$lexrec {
    union {
        i64 value;
        r64 xvalue;
        u64 uvalue;
        u8 *  svalue;
        struct qq_decls$strec *  symptr;
    };
    i32 pos;
    byte symbol;
    byte subcode;
    u16 slength;
};

struct qq_decls$uflagsrec {
    byte codes[7];
    byte ulength;
};

struct qq_decls$fieldrec {
    u8 *  name;
    i16 recordtype;
    i16 fieldtype;
    i32 fieldoffset;
};

struct qq_decls$unitrec {
    union {
        struct {
            byte tag;
            union {
                byte elemtype;
                byte flag;
                byte condcode;
                byte mathsop;
                byte pclop;
                byte loopcode;
                byte jsubcode;
            };
            byte spare[2];
            i32 pos;
        };
        void *  word1;
    };
    struct qq_decls$unitrec* nextunit;
    union {
        struct qq_decls$unitrec* a;
        struct qq_decls$strec *  def;
        struct qq_decls$strec *  labeldef;
        i64 value;
        u64 uvalue;
        r64 xvalue;
        u8 *  svalue;
        i64 range_lower;
    };
    union {
        struct qq_decls$unitrec* b;
        i64 range_upper;
        i64 slength;
        i16 mode;
        byte cmpconds[4];
        struct {
            i32 length;
            i32 lower;
        };
        i64 index;
    };
};

struct qq_decls$locrec {
    struct qq_decls$subprogrec *  sp;
    struct qq_decls$filerec *  pm;
    struct qq_decls$strec *  def;
    u8 *  startline;
    i64 lineno;
    i64 column;
};

struct qq_decls$genfieldrec {
    struct qq_decls$strec *  def;
    struct qq_decls$genfieldrec* nextdef;
};

struct qq_decls$filerec {
    u8 *  name;
    u8 *  path;
    u8 *  filespec;
    u8 *  text;
    i64 size;
    byte isstring;
    byte issyslib;
    byte issupport;
    byte compiled;
    byte subprogno;
    byte islead;
    union {
        i16 moduleno;
        i16 fileno;
    };
    struct qq_decls$unitrec *  ast;
    struct qq_pcltabs$pclrec *  pcstart;
    struct qq_pcltabs$pclrec *  pcend;
    i64 pcsize;
    i32 *  pcsourcestart;
    union {
        struct qq_decls$strec *  stmodule;
        struct qq_decls$strec *  def;
    };
    struct qq_decls$strec *  stsubprog;
    struct qq_decls$strec *  startfn;
    struct qq_decls$strec *  mainfn;
};

struct qq_decls$subprogrec {
    u8 *  name;
    u8 *  path;
    u8 *  filespec;
    i16 firstmodule;
    i16 lastmodule;
    i16 compiled;
    byte issyslib;
    byte subprogno;
};

struct qq_decls$varrec {
    union {
        struct {
            union {
                struct {
                    byte tag;
                    byte hasref;
                    byte bitoffset;
                    union {
                        byte bitlength;
                        byte exceptiontype;
                        byte genmarker;
                    };
                };
                u32 tagx;
            };
            union {
                u32 elemtag;
                u32 frameptr_low;
                struct {
                    i16 frameoffset;
                    i16 nexceptions;
                };
            };
        };
        i64 dummy;
    };
    union {
        i64 value;
        r64 xvalue;
        u64 uvalue;
        u64 range_upper;
        struct qq_decls$objrec *  objptr;
        struct qq_decls$varrec* varptr;
        byte *  ptr;
        struct qq_decls$strec *  def;
        struct qq_pcltabs$pclrec *  retaddr;
    };
};

struct qq_decls$objrec {
    u32 refcount;
    struct {
        byte flags;
        byte objtype;
        union {
            u16 elemtag;
            u16 usertag;
            u16 itertag;
            struct {
                byte bitoffset;
                byte indexoffset;
            };
            i16 lower16;
        };
    };
    union {
        struct {
            union {
                i64 value;
                r64 xvalue;
                u64 uvalue;
                u8 *  strptr;
                struct qq_decls$varrec *  varptr;
                struct qq_decls$varrec *  genstack;
                byte *  ptr;
                i32 (*num)[];
                u64 b;
                i64 *  retaddr;
            };
            union {
                i64 length;
                i64 lower64;
                struct {
                    u32 rows;
                    u32 columns;
                };
                u64 c;
                byte *  frameptr;
                struct {
                    i32 iterpos;
                    i32 iterupper;
                };
            };
            union {
                i64 alloc64;
                struct qq_decls$objrec* objptr2;
                struct {
                    i16 neg;
                    i16 numtype;
                    i32 expon;
                };
                struct {
                    u32 alloc32;
                    u32 dictitems;
                };
                struct {
                    u16 genstacksize;
                    byte ngenparams;
                };
                u64 d;
            };
        };
        byte bignumdescr[24];
    };
};

struct qq_decimal$constrec {
    i64 value;
    struct qq_decls$objrec *  bnvalue;
    struct qq_decimal$constrec* nextconst;
};

struct qq_host$dimrec {
    i64 lbound;
    i64 upper;
    i64 length;
};

struct qq_parse$readterm$dummy {
    union {
        u8 str[20];
        i64 sa;
    };
};

struct qq_pcltabs$pclrec {
    union {
        byte opcode;
        int * labaddr;
    };
    byte xx;
    byte n;
    byte mode;
    byte flags;
    union {
        struct {
            i16 x;
            i16 y;
        };
        i32 xy;
    };
    union {
        struct qq_decls$strec *  def;
        i64 value;
        u64 uvalue;
        r64 xvalue;
        u8 *  svalue;
        i64 labelno;
        i64 index;
        struct qq_pcltabs$pclrec* labelref;
        i64 offset;
        struct qq_decls$objrec *  objptr;
        i64 typecode;
        byte mathscode;
        byte pclop;
        byte bintoindex;
        i64 hostindex;
        struct qq_decls$varrec *  varptr;
        struct {
            i32 usertag;
            i32 usertag2;
        };
    };
    i64 dummy;
};

struct qq_pcltabs$bintorec {
    i64 pclop;
    void (*fnadd)(void);
    void (*fnaddmixed)(void);
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


/* PROCDECLS */
int main(int, char**);
static void qq_cli$getinputoptions(void);
static void qq_cli$do_option(i64 sw,u8 *value);
void qq_cli$compile_sp(u8 *filename,u8 *source);
static void qq_cli$setcli(u8 *(*cmds)[],i64 ncmds);
static void qq_cli$writeqafile(void);
static void qq_cli$initdata(void);
static void qq_cli$loadsyslib(void);
static void qq_cli$resetcompiler(void);
static void qq_cli$setcmdparam(i64 index,u8 *s);
static void qq_cli$fixup_sp(struct qq_decls$subprogrec *sp);
static void qq_cli$fixproc(struct qq_decls$strec *d);
static void qq_cli$fixupmodule(struct qq_decls$filerec *pm);
static void qq_cli$optimise_module(struct qq_decls$filerec *pm);
static struct qq_pcltabs$pclrec *qq_cli$optim(struct qq_pcltabs$pclrec *pc);
void qq_cli$start(void);
void qq_arrays$var_empty_array(i64 tag,i64 elemtype,i64 lower,struct qq_decls$varrec *dest);
void qq_arrays$obj_free_array(struct qq_decls$objrec *p);
void qq_arrays$obj_free_vector(struct qq_decls$objrec *p);
void qq_arrays$var_make_array(struct qq_decls$varrec *a,struct qq_decls$varrec *dest,i64 lower,i64 n,i64 axtype,i64 elemtype);
struct qq_decls$objrec *qq_arrays$obj_newarray(i64 elemtype,i64 lower,i64 length);
struct qq_decls$objrec *qq_arrays$obj_newarray_u(i64 usertag);
void qq_arrays$var_getix_array(struct qq_decls$varrec *a,i64 index);
void qq_arrays$var_putix_array(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *x);
void qq_arrays$var_getixref_array(struct qq_decls$varrec *a,i64 index);
static void qq_arrays$obj_append_array(struct qq_decls$objrec *a,struct qq_decls$varrec *x);
void qq_arrays$var_appendto_array(struct qq_decls$varrec *a,struct qq_decls$varrec *x);
void qq_arrays$obj_resize_array(struct qq_decls$objrec *p,i64 n);
void qq_arrays$var_dupl_array(struct qq_decls$varrec *a);
void qq_arrays$var_dupl_vector(struct qq_decls$varrec *a);
i64 qq_arrays$var_equal_array(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_arrays$var_concatto_array(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_arrays$var_getslice_array(struct qq_decls$varrec *a,i64 i,i64 j);
static i64 qq_arrays$u8inarray(byte a,struct qq_decls$objrec *p);
i64 qq_arrays$u16inarray(u16 a,struct qq_decls$objrec *p);
i64 qq_arrays$u32inarray(u32 a,struct qq_decls$objrec *p);
i64 qq_arrays$u64inarray(u64 a,struct qq_decls$objrec *p);
i64 qq_arrays$var_inx_array(struct qq_decls$varrec *a,struct qq_decls$varrec *b,i64 usertag);
void qq_arrays$var_expand_array(struct qq_decls$varrec *p,struct qq_decls$varrec *dest,i64 m);
void qq_arrays$start(void);
void qq_bits$obj_free_bits(struct qq_decls$objrec *p,i64 tag);
void qq_bits$var_make_bits(struct qq_decls$varrec *a,struct qq_decls$varrec *dest,i64 lower,i64 n,i64 bxtype,i64 elemtype);
struct qq_decls$objrec *qq_bits$obj_newbits(i64 elemtype,i64 lower,i64 length);
void qq_bits$var_getix_bits(struct qq_decls$varrec *a,i64 index);
void qq_bits$var_putix_bits(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *x);
void qq_bits$var_getixref_bits(struct qq_decls$varrec *a,i64 index);
static byte *qq_bits$getindexoffset(byte *p,i64 offset,i64 index,i64 t,i64 *newoffset);
static void qq_bits$obj_append_bits(struct qq_decls$objrec *a,struct qq_decls$varrec *x);
void qq_bits$var_appendto_bits(struct qq_decls$varrec *a,struct qq_decls$varrec *x);
void qq_bits$obj_resize_bits(struct qq_decls$objrec *p,i64 n);
void qq_bits$var_dupl_bits(struct qq_decls$varrec *a);
i64 qq_bits$var_equal_bits(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_bits$var_concatto_bits(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_bits$var_getslice_bits(struct qq_decls$varrec *a,i64 i,i64 j);
i64 qq_bits$bits_bytesize(struct qq_decls$objrec *p);
i64 qq_bits$getbitssize(i64 n,i64 t);
void qq_bits$start(void);
void qq_calldll$calldll(struct qq_decls$strec *d,struct qq_decls$varrec *args,struct qq_decls$varrec *result,i64 nargs);
static void (*qq_calldll$getlibprocaddr(struct qq_decls$strec *d))(void);
static u64 qq_calldll$vartopacked(struct qq_decls$varrec *p,struct qq_decls$strec *d);
static void qq_calldll$packedtovar(u64 retval,i64 t,struct qq_decls$varrec *dest);
static void (*qq_calldll$loaddllfunction(struct qq_decls$strec *d))(void);
void qq_calldll$start(void);
void qq_decls$start(void);
void qq_decimal$obj_free_dec(struct qq_decls$objrec *p);
void qq_decimal$var_dupl_dec(struct qq_decls$varrec *a);
void qq_decimal$var_empty_dec(struct qq_decls$varrec *dest);
void qq_decimal$var_make_dec_str(u8 *s,i64 length,struct qq_decls$varrec *dest);
void qq_decimal$var_make_dec_int(i64 a,struct qq_decls$varrec *dest);
static struct qq_decls$objrec *qq_decimal$badnumber(void);
static struct qq_decls$objrec *qq_decimal$bn_makestr(u8 *s,i64 length);
static i64 qq_decimal$readexpon(u8 *s);
static struct qq_decls$objrec *qq_decimal$bn_makeint(i64 x);
u8 *qq_decimal$var_tostr_dec(struct qq_decls$varrec *a,i64 fmt);
static u8 *qq_decimal$obj_tostr_dec(struct qq_decls$objrec *a,i64 fmt);
static u8 *qq_decimal$tostring_scient(struct qq_decls$objrec *a);
static u8 *qq_decimal$tostring_float(struct qq_decls$objrec *a,i64 fmt);
static i64 qq_decimal$strvaln(u8 *s,i64 n);
static i64 qq_decimal$bn_isint(struct qq_decls$objrec *a);
i64 qq_decimal$obj_len_dec(struct qq_decls$objrec *a);
i64 qq_decimal$bn_iszero(struct qq_decls$objrec *a);
i64 qq_decimal$var_equal_dec(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_decimal$var_add_dec(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_decimal$var_sub_dec(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_decimal$var_mul_dec(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_decimal$var_div_dec(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_decimal$var_idiv_dec(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_decimal$var_irem_dec(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_decimal$var_neg_dec(struct qq_decls$varrec *a);
void qq_decimal$var_abs_dec(struct qq_decls$varrec *a);
i64 qq_decimal$var_compare_dec(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
static i64 qq_decimal$bn_cmp(struct qq_decls$objrec *a,struct qq_decls$objrec *b);
static i64 qq_decimal$bn_equal(struct qq_decls$objrec *a,struct qq_decls$objrec *b);
static i64 qq_decimal$bn_add(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b);
static i64 qq_decimal$bn_sub(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b);
static void qq_decimal$bn_addu(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b);
static void qq_decimal$bn_subu(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b);
static struct qq_decls$objrec *qq_decimal$makebignum(i64 length);
static i32 *qq_decimal$makesmallnum(i64 length);
static struct qq_decls$objrec *qq_decimal$smalltobig(struct qq_decls$objrec *c,i32 *a,i64 length,i64 alloc,i64 offset);
static void qq_decimal$freesmall(i32 *p,i64 length);
struct qq_decls$objrec *qq_decimal$bn_init(void);
static void qq_decimal$bn_setzero(struct qq_decls$objrec *a);
static void qq_decimal$bn_move(struct qq_decls$objrec *a,struct qq_decls$objrec *b);
static void qq_decimal$bn_dupl(struct qq_decls$objrec *a,struct qq_decls$objrec *b);
static void qq_decimal$bn_setinf(struct qq_decls$objrec *dest);
static void qq_decimal$bn_setnan(struct qq_decls$objrec *dest);
void qq_decimal$var_setnan(struct qq_decls$varrec *dest);
void qq_decimal$var_setinf(struct qq_decls$varrec *dest);
static i64 qq_decimal$getbintype(struct qq_decls$objrec *a,struct qq_decls$objrec *b);
static void qq_decimal$bn_negto(struct qq_decls$objrec *a);
static void qq_decimal$bn_absto(struct qq_decls$objrec *a);
static i64 qq_decimal$bn_mul(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b);
static i64 qq_decimal$bn_mulp(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b,i64 prec);
static void qq_decimal$bn_mulu(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b);
static i64 qq_decimal$smallmulto(i32 *p,i32 *q,i64 plen,i64 m);
static i64 qq_decimal$bn_div(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b,i64 prec);
static i64 qq_decimal$bn_idiv(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b);
static i64 qq_decimal$bn_idivrem(struct qq_decls$objrec *dest,struct qq_decls$objrec *rm,struct qq_decls$objrec *a,struct qq_decls$objrec *b);
static i64 qq_decimal$bn_irem(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b);
static void qq_decimal$bn_idivu(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b,struct qq_decls$objrec *rm);
static void qq_decimal$bn_fdivu(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b,i64 precision);
static i64 qq_decimal$smalldiv(i32 *x,i32 *b,i64 *xlen,i64 nb);
static i64 qq_decimal$smallsubto(i32 *p,i32 *q,i64 plen,i64 qlen);
static i64 qq_decimal$bn_getprec(struct qq_decls$objrec *a);
static void qq_decimal$bn_setprec(struct qq_decls$objrec *a,i64 prec);
static i64 qq_decimal$bn_getglobalprec(void);
static void qq_decimal$bn_setglobalprec(i64 prec);
static struct qq_decls$objrec *qq_decimal$bn_makefloat(r64 x);
struct qq_decls$varrec *qq_decimal$dectemp(struct qq_decls$varrec *a);
void qq_decimal$freedectemp(void);
static void qq_decimal$bn_ipower(struct qq_decls$objrec *d,struct qq_decls$objrec *a,i64 n);
void qq_decimal$var_power_dec(struct qq_decls$varrec *a,i64 n);
i64 qq_decimal$var_convert_dec_int(struct qq_decls$varrec *a);
static i64 qq_decimal$bn_toint(struct qq_decls$objrec *a);
void qq_decimal$start(void);
void qq_dicts$var_make_dict(struct qq_decls$varrec *a,struct qq_decls$varrec *dest,i64 n);
struct qq_decls$objrec *qq_dicts$obj_new_dict(i64 n);
void qq_dicts$obj_free_dict(struct qq_decls$objrec *p,i64 internal);
void qq_dicts$var_dupl_dict(struct qq_decls$varrec *a);
i64 qq_dicts$var_equal_dict(struct qq_decls$varrec *x,struct qq_decls$varrec *y);
struct qq_decls$varrec *qq_dicts$var_finddictitem(struct qq_decls$varrec *vd,struct qq_decls$varrec *p,i64 doins);
static void qq_dicts$expanddict(struct qq_decls$varrec *vd);
static void qq_dicts$adddictitem(struct qq_decls$varrec *d,struct qq_decls$varrec *p,struct qq_decls$varrec *q);
void qq_dicts$start(void);
struct qq_decls$varrec *qq_host$callhostfunction(i64 hostfn,struct qq_decls$varrec *sp);
void qq_host$pch_leftstr(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *c,struct qq_decls$varrec *result);
void qq_host$pch_rightstr(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *c,struct qq_decls$varrec *result);
void qq_host$pch_convlc(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *result);
void qq_host$pch_convuc(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *result);
void qq_host$pch_waitkey(struct qq_decls$varrec *result);
void qq_host$pch_execwait(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *c,struct qq_decls$varrec *result);
void qq_host$pch_execcmd(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *c,struct qq_decls$varrec *result);
void qq_host$pch_makestr(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *result);
void qq_host$pch_makeref(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *result);
void qq_host$pch_getcmdparam(struct qq_decls$varrec *a,struct qq_decls$varrec *result);
void qq_host$pch_clock(struct qq_decls$varrec *result);
void qq_host$pch_allocexec(struct qq_decls$varrec *a,struct qq_decls$varrec *result);
void qq_host$pch_runnative(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *result);
void qq_host$pch_setlwb(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_host$pch_ticks(struct qq_decls$varrec *result);
void qq_host$pch_sleep(struct qq_decls$varrec *a);
void qq_host$pch_random(struct qq_decls$varrec *a,struct qq_decls$varrec *result);
void qq_host$pch_system(struct qq_decls$varrec *a,struct qq_decls$varrec *result);
void qq_host$pch_$getparam(struct qq_decls$varrec *a,struct qq_decls$varrec *result);
static i64 qq_host$checkparam(struct qq_decls$varrec *p,i64 tag,i64 defaultx);
static void qq_host$leftstring(struct qq_decls$varrec *a,i64 n,struct qq_decls$varrec *result);
static void qq_host$rightstring(struct qq_decls$varrec *a,i64 n,struct qq_decls$varrec *result);
static void qq_host$padstring_right(struct qq_decls$varrec *a,i64 n,i64 fillchar,struct qq_decls$varrec *result);
static void qq_host$padstring_left(struct qq_decls$varrec *a,i64 n,i64 fillchar,struct qq_decls$varrec *result);
static void qq_host$getbounds(struct qq_decls$varrec *p,struct qq_host$dimrec *dims,i64 lower);
void qq_host$pch_new(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *c,struct qq_decls$varrec *d,struct qq_decls$varrec *result);
void qq_host$pch_gethostname(struct qq_decls$varrec *result);
void qq_host$pch_getprogname(struct qq_decls$varrec *result);
void qq_host$pch_$test(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *c,struct qq_decls$varrec *result);
void qq_host$pch_$test2(struct qq_decls$varrec *a,struct qq_decls$varrec *result);
void qq_host$pch_$refcount(struct qq_decls$varrec *a,struct qq_decls$varrec *result);
void qq_host$pch_testkey(struct qq_decls$varrec *result);
void qq_host$pch_getos(struct qq_decls$varrec *result);
void qq_host$pch_setmesshandler(struct qq_decls$varrec *fn);
void qq_host$pch_$smallmemtotal(struct qq_decls$varrec *result);
void qq_host$pch_$id(struct qq_decls$varrec *a,struct qq_decls$varrec *result);
void qq_host$pch_iswindows(struct qq_decls$varrec *result);
void qq_host$pch_$setdebug(struct qq_decls$varrec *a);
void qq_host$pch_copy(struct qq_decls$varrec *a,struct qq_decls$varrec *dest);
void qq_host$pch_gethash(struct qq_decls$varrec *a,struct qq_decls$varrec *result);
void qq_host$pch_makeempty(struct qq_decls$varrec *a,struct qq_decls$varrec *result);
void qq_host$pch_$infinity(struct qq_decls$varrec *dest);
void qq_host$pch_$nan(struct qq_decls$varrec *dest);
void qq_host$setcmdparam(i64 index,u8 *s);
void qq_host$pch_$nprocs(struct qq_decls$varrec *result);
static void qq_host$initprocrefs(void);
void qq_host$pch_$procname(struct qq_decls$varrec *a,struct qq_decls$varrec *result);
void qq_host$pch_$procref(struct qq_decls$varrec *a,struct qq_decls$varrec *result);
void qq_host$start(void);
void qq_lex$lexreadtoken(void);
static void qq_lex$lxreadstring(i64 termchar);
static i64 qq_lex$readhexcode(u8 **s,i64 n,i64 sp);
static i64 qq_lex$getutf8(i64 c,u8 *s);
void qq_lex$lexinit(void);
static void qq_lex$readrawstring(void);
i64 qq_lex$lookup(u8 *name,i64 length,i64 hashindex);
i64 qq_lex$gethashvaluez(u8 *s);
void qq_lex$start(void);
static void qq_lex$inithashtable(void);
static void qq_lex$addstname(u8 *name,i64 symbol,i64 subcode);
void qq_lex$startlex(struct qq_decls$filerec *pm);
struct qq_decls$strec *qq_lex$addnamestr(u8 *name);
void qq_lex$ps(u8 *caption);
void qq_lex$psnext(u8 *caption);
void qq_lex$lex(void);
void qq_lex$lxerror_s(u8 *mess,u8 *a);
static void qq_lex$makedecimal(u8 *s,i64 length,i64 base);
static void qq_lex$readdec(void);
static void qq_lex$readhex(void);
static void qq_lex$readbin(void);
static void qq_lex$readreal(void);
static void qq_lex$readrawxname(void);
void qq_lib$reportcterror(u8 *errortype,u8 *mess,i64 pos,struct qq_decls$strec *currproc);
struct qq_decls$locrec qq_lib$geterrorinfo(u64 pos,struct qq_decls$strec *currproc);
static void qq_lib$showerrorsource(struct qq_decls$locrec loc);
void qq_lib$stopcompiler(struct qq_decls$locrec loc);
void qq_lib$gerror(u8 *mess,struct qq_decls$unitrec *p);
void qq_lib$gerror_s(u8 *mess,u8 *param,struct qq_decls$unitrec *p);
void qq_lib$serror(u8 *mess);
void qq_lib$serror_s(u8 *mess,u8 *param);
void qq_lib$rxerror(u8 *mess,struct qq_decls$unitrec *p);
void qq_lib$rxerror_s(u8 *mess,u8 *param,struct qq_decls$unitrec *p);
void qq_lib$lxerror(u8 *mess);
void qq_lib$loaderror(u8 *mess,u8 *mess2);
void qq_lib$prterror(u8 *mess);
static struct qq_decls$unitrec *qq_lib$allocunitrec(void);
struct qq_decls$unitrec *qq_lib$createintunit(i64 a);
struct qq_decls$unitrec *qq_lib$createrealunit(r64 x);
struct qq_decls$unitrec *qq_lib$createstringunit(u8 *s,i64 slength);
struct qq_decls$unitrec *qq_lib$createunit0(i64 tag);
struct qq_decls$unitrec *qq_lib$createunit1(i64 tag,struct qq_decls$unitrec *p);
struct qq_decls$unitrec *qq_lib$createunit2(i64 tag,struct qq_decls$unitrec *p,struct qq_decls$unitrec *q);
struct qq_decls$unitrec *qq_lib$createname(struct qq_decls$strec *p);
void qq_lib$addlistunit(struct qq_decls$unitrec **ulist,struct qq_decls$unitrec **ulistx,struct qq_decls$unitrec *p);
struct qq_decls$unitrec *qq_lib$createavname(void);
u8 *qq_lib$convtostringz(u8 *svalue,i64 length);
u8 *qq_lib$findprocname(void (*fnptr)(void));
struct mlib$strbuffer *qq_lib$strexpr(struct qq_decls$unitrec *p);
u8 *qq_lib$strexpr_s(struct qq_decls$unitrec *p);
static void qq_lib$jeval(struct qq_decls$unitrec *p);
static void qq_lib$jevallist(struct qq_decls$unitrec *p);
void qq_lib$additem(u8 *s);
static i64 qq_lib$isalphanum(i64 c);
u8 *qq_lib$getopcname(i64 opc);
void qq_lib$convertstring(u8 *s,u8 *t);
struct qq_decls$unitrec *qq_lib$createavnamex(struct qq_decls$strec *owner);
void qq_lib$storemode(struct qq_decls$strec *owner,i64 m,i16 *p);
i64 qq_lib$nextpoweroftwo(i64 x);
i64 qq_lib$testelem(byte (*p)[],i64 n);
void qq_lib$setelem(byte (*p)[],i64 n);
void qq_lib$setelemblock(byte (*p)[],i64 a,i64 b);
i64 qq_lib$ispoweroftwo(i64 x);
void qq_lib$deleteunit(struct qq_decls$unitrec *p,struct qq_decls$unitrec *q);
void qq_lib$skipsemi(void);
void qq_lib$checksymbol(i64 symbol);
void qq_lib$skipsymbol(i64 symbol);
void qq_lib$pcnotmut(void);
void qq_lib$start(void);
void qq_lists$start(void);
void qq_lists$var_empty_list(i64 lower,struct qq_decls$varrec *dest);
void qq_lists$var_make_list(struct qq_decls$varrec *a,struct qq_decls$varrec *dest,i64 n,i64 lower);
struct qq_decls$objrec *qq_lists$obj_newlist(i64 n,i64 lower,struct qq_decls$varrec *defval);
void qq_lists$obj_free_list(struct qq_decls$objrec *p);
void qq_lists$var_getix_list(struct qq_decls$varrec *a,i64 index);
void qq_lists$var_getslice_list(struct qq_decls$varrec *a,i64 i,i64 j);
void qq_lists$var_getixref_list(struct qq_decls$varrec *a,i64 index);
void qq_lists$var_putix_list(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *x);
void qq_lists$var_putslice_list(struct qq_decls$varrec *a,i64 i,i64 j,struct qq_decls$varrec *x);
static void qq_lists$obj_append_list(struct qq_decls$objrec *a,struct qq_decls$varrec *x);
void qq_lists$obj_resize_list(struct qq_decls$objrec *p,i64 n);
void qq_lists$var_appendto_list(struct qq_decls$varrec *a,struct qq_decls$varrec *x);
void qq_lists$var_dupl_list(struct qq_decls$varrec *a);
void qq_lists$var_mul_list(struct qq_decls$varrec *p,i64 m);
i64 qq_lists$var_equal_list(struct qq_decls$varrec *x,struct qq_decls$varrec *y);
void qq_lists$var_concatto_list(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
i64 qq_lists$var_inx_list(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
struct qq_decls$subprogrec *qq_modules$loadsp(u8 *filename,u8 *source);
static u8 *qq_modules$getmodulefilename(u8 *path,u8 *name);
struct qq_decls$filerec *qq_modules$loadsourcefile(u8 *filespec,i64 issyslib);
struct qq_decls$filerec *qq_modules$loadstring(u8 *name,u8 *source);
static u8 *qq_modules$readfileline(u8 *s);
static u8 *qq_modules$findnextlineheader(u8 *s);
static i64 qq_modules$loadqafile(struct qq_decls$filerec *pm);
void qq_modules$readqabundle(void);
void qq_modules$start(void);
struct qq_decls$strec *qq_names$addglobalname(u8 *name);
static struct qq_decls$strec *qq_names$newstrec(void);
struct qq_decls$strec *qq_names$addsymbol(struct qq_decls$strec *owner,struct qq_decls$strec *d,i64 id,i64 isglobal);
void qq_names$addproc(struct qq_decls$strec *d);
i64 qq_names$newusertypex(struct qq_decls$strec *d,struct qq_decls$strec *e);
struct qq_decls$strec *qq_names$resolvedottedname(struct qq_decls$strec *owner,struct qq_decls$strec *d);
void qq_names$addgenfield(struct qq_decls$strec *d);
i64 qq_names$makereftype(i64 target,struct qq_decls$strec *owner);
i64 qq_names$makeaxtype(i64 target,struct qq_decls$unitrec *plower,struct qq_decls$unitrec *plength);
i64 qq_names$makestrtype(i64 m,struct qq_decls$unitrec *pwidth);
i64 qq_names$addanontype(void);
void qq_names$createusertype(struct qq_decls$strec *d,i64 m);
i64 qq_names$getalignment(i64 m);
void qq_names$duplfield(struct qq_decls$strec *p,struct qq_decls$strec *q);
static void qq_names$writesig(struct qq_decls$strec *d,void *dev);
struct qq_decls$strec *qq_names$createdupldef(struct qq_decls$strec *owner,struct qq_decls$strec *symptr,i64 id);
void qq_names$start(void);
void qq_packed$var_loadpacked(void *p,i64 t,struct qq_decls$varrec *dest,struct qq_decls$objrec *ownerobj);
void qq_packed$var_storepacked(byte *p,struct qq_decls$varrec *q,i64 t);
static void qq_packed$setfslength(u8 *s,i64 m,i64 n);
i64 qq_packed$getfslength(u8 *s,i64 m);
void qq_packed$var_make_struct(struct qq_decls$varrec *a,struct qq_decls$varrec *dest,i64 n,i64 rectype);
struct qq_decls$objrec *qq_packed$obj_new_struct(i64 m);
void qq_packed$var_dupl_struct(struct qq_decls$varrec *a);
void qq_packed$obj_free_struct(struct qq_decls$objrec *p);
i64 qq_packed$var_equal_struct(struct qq_decls$varrec *x,struct qq_decls$varrec *y);
void qq_packed$var_getix_struct(struct qq_decls$varrec *a,i64 index);
void qq_packed$start(void);
void qq_parse$parsemodule(struct qq_decls$filerec *pm);
static struct qq_decls$unitrec *qq_parse$readexpression(void);
static struct qq_decls$unitrec *qq_parse$readassignment(struct qq_decls$unitrec *p);
static struct qq_decls$unitrec *qq_parse$readorterms(struct qq_decls$unitrec *p);
static struct qq_decls$unitrec *qq_parse$readandterms(struct qq_decls$unitrec *p);
static struct qq_decls$unitrec *qq_parse$readcmpterms(struct qq_decls$unitrec *p);
static struct qq_decls$unitrec *qq_parse$readinterms(struct qq_decls$unitrec *p);
static struct qq_decls$unitrec *qq_parse$readrangeterm(struct qq_decls$unitrec *p);
static struct qq_decls$unitrec *qq_parse$readaddterms(struct qq_decls$unitrec *p);
static struct qq_decls$unitrec *qq_parse$readmulterms(struct qq_decls$unitrec *p);
static struct qq_decls$unitrec *qq_parse$readpowerterms(struct qq_decls$unitrec *p);
static struct qq_decls$unitrec *qq_parse$readterm2(void);
static struct qq_decls$unitrec *qq_parse$readtermsuffix(struct qq_decls$unitrec *p,i64 pos);
static struct qq_decls$unitrec *qq_parse$readterm(void);
static struct qq_decls$unitrec *qq_parse$readsunit(i64 inwhile);
static void qq_parse$checkequals(void);
static struct qq_decls$unitrec *qq_parse$readindex(struct qq_decls$unitrec *p,i64 dot);
static struct qq_decls$unitrec *qq_parse$readdotsuffix(struct qq_decls$unitrec *p);
static struct qq_decls$unitrec *qq_parse$readslist(i64 *nparams,i64 ftrailing);
static struct qq_decls$unitrec *qq_parse$readcondsuffix(struct qq_decls$unitrec *p);
static struct qq_decls$unitrec *qq_parse$readkeyindex(struct qq_decls$unitrec *p);
static struct qq_decls$unitrec *qq_parse$readlbrack(void);
static struct qq_decls$unitrec *qq_parse$readif(void);
static void qq_parse$checkend(i64 endkwd1,i64 endkwd2,i64 startline);
static struct qq_decls$unitrec *qq_parse$readunless(void);
static struct qq_decls$unitrec *qq_parse$readwhile(void);
static struct qq_decls$unitrec *qq_parse$readrepeat(void);
static struct qq_decls$unitrec *qq_parse$readfor(void);
static struct qq_decls$unitrec *qq_parse$readdo(void);
static struct qq_decls$unitrec *qq_parse$readto(void);
static struct qq_decls$unitrec *qq_parse$makeblock(struct qq_decls$unitrec *p);
static struct qq_decls$unitrec *qq_parse$readvardef(i64 isglobal,i64 isstatic);
static void qq_parse$readconstdef(i64 isglobal);
static struct qq_decls$unitrec *qq_parse$readreturn(void);
static struct qq_decls$unitrec *qq_parse$readprint(void);
static struct qq_decls$unitrec *qq_parse$readread(void);
static struct qq_decls$unitrec *qq_parse$readloopcontrol(void);
static struct qq_decls$unitrec *qq_parse$readintunit(void);
static struct qq_decls$unitrec *qq_parse$readswitchcase(void);
static struct qq_decls$unitrec *qq_parse$readgoto(void);
static struct qq_decls$unitrec *qq_parse$readstop(void);
static struct qq_decls$unitrec *qq_parse$readcast(void);
static struct qq_decls$unitrec *qq_parse$readset(void);
void qq_parse$readtabledef(i64 isglobal);
static struct qq_decls$unitrec *qq_parse$readtry(void);
static struct qq_decls$unitrec *qq_parse$readsprint(void);
static struct qq_decls$unitrec *qq_parse$readsread(void);
static void qq_parse$readimportdll(void);
static void qq_parse$readffiparams(struct qq_decls$strec *stproc);
static void qq_parse$readtypeparams(struct qq_decls$strec *stproc,i64 ptype);
static void qq_parse$readtypenameparams(struct qq_decls$strec *stproc,i64 ptype);
void qq_parse$readrecorddef(i64 isglobal,struct qq_decls$strec *d);
static i64 qq_parse$readrecordbody(struct qq_decls$strec *owner);
static void qq_parse$readrecordfields(struct qq_decls$strec *owner);
static i64 qq_parse$readstructbody(struct qq_decls$strec *owner,i64 caligned);
static void qq_parse$addstructflag(struct qq_decls$strec *owner,i64 id);
static void qq_parse$readprocdef(i64 isglobal);
static struct qq_decls$strec *qq_parse$readatfield(void);
static i64 qq_parse$istypestarter(void);
static void qq_parse$readmacrodef(i64 isglobal);
static struct qq_decls$unitrec *qq_parse$readhostparams(struct qq_decls$unitrec *lhs,i64 isfn);
static void qq_parse$pushlisttype(i64 ltype);
static void qq_parse$poplisttype(void);
static struct qq_decls$unitrec *qq_parse$readcompilervar(void);
static struct qq_decls$unitrec *qq_parse$readpair(i64 tag,i64 pclop);
void qq_parse$lexchecksymbol(i64 symbol);
static void qq_parse$readtypedef(i64 isglobal);
static i64 qq_parse$readtypespec(i64 allowvar,struct qq_decls$strec *owner);
static void qq_parse$readparams(struct qq_decls$strec *stproc);
static struct qq_decls$unitrec *qq_parse$checkoperator(void);
static struct qq_decls$unitrec *qq_parse$readlambda(void);
static void qq_parse$readpackvars(struct qq_decls$strec *owner,i64 id);
void qq_parse$start(void);
void qq_pcltabs$start(void);
void qq_pclgen$evalunit(struct qq_decls$unitrec *p,i64 res);
void qq_pclgen$gencodemodule(struct qq_decls$subprogrec *sp,i64 moduleno);
static void qq_pclgen$do_procdef(struct qq_decls$strec *p);
static void qq_pclgen$genprocentry(struct qq_decls$strec *p,i64 *nfreevars,i64 *nnofreevars);
static void qq_pclgen$genprocexit(i64 nfree,i64 nnofree,i64 isfunc);
static void qq_pclgen$evalref(struct qq_decls$unitrec *p);
static void qq_pclgen$genjumpcond(i64 opc,struct qq_decls$unitrec *p,i64 lab);
static void qq_pclgen$gcomparejump(i64 opc,i64 cond,i64 lab);
static void qq_pclgen$genjumpl(i64 lab);
void qq_pclgen$stacklooplabels(i64 a,i64 b,i64 c);
void qq_pclgen$unstacklooplabels(void);
i64 qq_pclgen$findlooplabel(i64 k,i64 n);
static void qq_pclgen$do_assign(struct qq_decls$unitrec *a,struct qq_decls$unitrec *b,i64 res,i64 deepcopy);
static void qq_pclgen$do_bin(struct qq_decls$unitrec *a,struct qq_decls$unitrec *b,i64 opc);
static void qq_pclgen$do_binref(struct qq_decls$unitrec *a,struct qq_decls$unitrec *b,i64 opc);
static void qq_pclgen$do_unary(struct qq_decls$unitrec *a,i64 opc);
static void qq_pclgen$do_unaryref(struct qq_decls$unitrec *a,i64 opc);
static void qq_pclgen$do_pushlist(struct qq_decls$unitrec *a,i64 n);
static void qq_pclgen$do_makedict(struct qq_decls$unitrec *a,i64 n);
static void qq_pclgen$do_call(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,struct qq_decls$unitrec *b,i64 res,i64 *procflag);
static i64 qq_pclgen$pushparams(struct qq_decls$strec *d,struct qq_decls$unitrec *(*arglist)[],i64 nargs,i64 kwdindex);
static void qq_pclgen$evalparam(struct qq_decls$unitrec *a,i64 byref);
static void qq_pclgen$pushkwdparams(struct qq_decls$strec *d,struct qq_decls$unitrec *(*arglist)[],i64 nargs,i64 kwdindex);
static void qq_pclgen$do_if(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,struct qq_decls$unitrec *b,struct qq_decls$unitrec *pelse,i64 res);
static void qq_pclgen$do_do(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a);
static void qq_pclgen$do_loop(struct qq_decls$unitrec *p);
static void qq_pclgen$do_to(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pcount,struct qq_decls$unitrec *pbody);
static void qq_pclgen$do_while(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pcond,struct qq_decls$unitrec *pbody);
static void qq_pclgen$do_repeat(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,struct qq_decls$unitrec *b);
static void qq_pclgen$do_for(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pvar,struct qq_decls$unitrec *pbody);
static void qq_pclgen$do_forx(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pvar,struct qq_decls$unitrec *pbody);
static void qq_pclgen$do_print(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,struct qq_decls$unitrec *b);
static void qq_pclgen$do_fprint(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,struct qq_decls$unitrec *b,struct qq_decls$unitrec *c);
static void qq_pclgen$do_read(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,struct qq_decls$unitrec *b);
static void qq_pclgen$do_forall(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pindex,struct qq_decls$unitrec *pbody);
static void qq_pclgen$do_case(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pindex,struct qq_decls$unitrec *pwhenthen,i64 res);
static void qq_pclgen$do_case_nc(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pindex,struct qq_decls$unitrec *pwhenthen,i64 res);
static void qq_pclgen$do_try(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,struct qq_decls$unitrec *b);
static i64 qq_pclgen$unitstoarray(struct qq_decls$unitrec *p,struct qq_decls$unitrec *(*plist)[],i64 maxunits);
static void qq_pclgen$do_select(struct qq_decls$unitrec *pindex,struct qq_decls$unitrec *pplist,i64 res);
static void qq_pclgen$do_andl(struct qq_decls$unitrec *x,struct qq_decls$unitrec *y);
static void qq_pclgen$do_orl(struct qq_decls$unitrec *x,struct qq_decls$unitrec *y);
static void qq_pclgen$do_incr(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,i64 res);
static void qq_pclgen$do_callhost(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,i64 res);
static void qq_pclgen$callhostfn(i64 fnindex,i64 calledasfn);
static void qq_pclgen$genfree(i64 n);
static void qq_pclgen$do_return(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a);
static void qq_pclgen$do_multassign(struct qq_decls$unitrec *a,struct qq_decls$unitrec *b,i64 deepcopy,i64 res);
static void qq_pclgen$do_store(struct qq_decls$unitrec *a,i64 res);
static i64 qq_pclgen$getconstvalue(struct qq_decls$unitrec *p);
static void qq_pclgen$do_convert(struct qq_decls$unitrec *pconv);
static void qq_pclgen$checkelems(i64 n,i64 length,struct qq_decls$unitrec *p);
static void qq_pclgen$do_switch(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pindex,struct qq_decls$unitrec *pwhenthen,i64 res);
static void qq_pclgen$do_simpleswitch(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pindex,struct qq_decls$unitrec *pwhenthen,struct qq_decls$unitrec *pelse,i64 a,i64 b,i64 res);
static void qq_pclgen$do_makerecordkv(i64 m,i64 nkeyvals,struct qq_decls$unitrec *(*kvlist)[]);
static void qq_pclgen$do_idiv(struct qq_decls$unitrec *a,struct qq_decls$unitrec *b);
static void qq_pclgen$do_irem(struct qq_decls$unitrec *a,struct qq_decls$unitrec *b);
static void qq_pclgen$do_map(struct qq_decls$unitrec *p,struct qq_decls$unitrec *popcode,struct qq_decls$unitrec *x);
static void qq_pclgen$pushstring(u8 *s);
static i64 qq_pclgen$checkblockreturn(struct qq_decls$unitrec *p);
void qq_pclgen$start(void);
void qq_pcllib$start(void);
void qq_pcllib$resetpcl(i64 sourcesize);
void qq_pcllib$genpc(i64 opc);
void qq_pcllib$genpc_int(i64 opc,i64 a);
void qq_pcllib$genpc_n(i64 opc,i64 n);
void qq_pcllib$genpc_xy(i64 opc,i64 x,i64 y);
void qq_pcllib$genpc_name(i64 opc,struct qq_decls$strec *d);
void qq_pcllib$genopnd_strz(u8 *s);
void qq_pcllib$genopnd_str(struct qq_decls$objrec *s);
void qq_pcllib$genopnd_obj(struct qq_decls$objrec *p);
void qq_pcllib$genpc_real(i64 opc,r64 x);
void qq_pcllib$genpc_lab(i64 opc,i64 lab);
void qq_pcllib$gencomment(u8 *s);
static void qq_pcllib$extendpcldata(void);
void qq_pcllib$extendlabeltable(void);
i64 qq_pcllib$definelabel(void);
i64 qq_pcllib$createfwdlabel(void);
void qq_pcllib$definefwdlabel(i64 lab);
void qq_pcllib$genxy(i64 x,i64 y);
void qq_print$pch_print(struct qq_decls$varrec *p,struct qq_decls$varrec *fmt);
void qq_print$pch_print_nf(struct qq_decls$varrec *p);
void qq_print$pch_printnogap(void);
void qq_print$pch_println(void);
void qq_print$pch_reread(void);
void qq_print$pch_rereadln(void);
void qq_print$pch_startprint(struct qq_decls$varrec *p);
void qq_print$pch_startprintcon(void);
void qq_print$pch_endprint(void);
void qq_print$pch_strstartprint(void);
void qq_print$pch_strendprint(struct qq_decls$varrec *dest);
void qq_print$pch_printspace(void);
void qq_print$pch_readln(struct qq_decls$varrec *dev);
void qq_print$pch_sread(struct qq_decls$varrec *fmt,struct qq_decls$varrec *dest);
void qq_print$pch_sreadln(struct qq_decls$varrec *dev,struct qq_decls$varrec *dest);
static u8 *qq_print$readname(u8 *s,i64 length,struct qq_decls$varrec *dest);
static u8 *qq_print$readstring(u8 *s,i64 length,struct qq_decls$varrec *dest);
static u8 *qq_print$readint(u8 *sold,i64 length,struct qq_decls$varrec *dest,i64 dodec);
static u8 *qq_print$readhex(u8 *sold,i64 length,struct qq_decls$varrec *dest);
static u8 *qq_print$readbin(u8 *sold,i64 length,struct qq_decls$varrec *dest);
static u8 *qq_print$readreal(u8 *sold,i64 length,struct qq_decls$varrec *dest);
i64 qq_print$getreadfmtcode(struct qq_decls$varrec *p);
static void qq_print$stepkbpos(u8 *s);
static u8 *qq_print$readany(u8 *sold,i64 length,struct qq_decls$varrec *dest);
static u8 *qq_print$readitem(u8 *s,i64 length,u8 **itemstr,i64 *itemlength);
static void qq_print$strtoreal(u8 *s,i64 length,struct qq_decls$varrec *dest);
static void qq_print$strtoint(u8 *s,i64 length,struct qq_decls$varrec *dest,i64 dodec);
static void qq_print$printnextfmtchars(i64 lastx);
void qq_print$pch_setformat(struct qq_decls$varrec *p);
struct msysc$fmtrec *qq_print$pc_getfmt(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt);
void qq_print$addstring(struct qq_decls$objrec *p,u8 *t,i64 n);
static void qq_print$domultichar(u8 *p,i64 n,u8 *dest,struct msysc$fmtrec *fmt);
static void qq_print$printstr_n(u8 *s,i64 n);
void qq_print$pch_strtoval(struct qq_decls$varrec *p,struct qq_decls$varrec *fmt,struct qq_decls$varrec *dest);
static void qq_print$tostr_int(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest);
static void qq_print$tostr_real(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest);
static void qq_print$tostr_str(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest);
void qq_print$pch_tostr(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *result);
static void qq_print$tostr_range(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest);
static void qq_print$tostr_array(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest);
static void qq_print$tostr_bits(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest);
static void qq_print$tostr_struct(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest);
static void qq_print$tostr_set(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest);
static void qq_print$tostr_dict(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest);
static void qq_print$tostr_decimal(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest);
static void qq_print$tostr(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest);
static void qq_print$tostr_list(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest);
void qq_print$start(void);
void qq_records$var_make_record(struct qq_decls$varrec *a,struct qq_decls$varrec *dest,i64 n,i64 rectype);
struct qq_decls$objrec *qq_records$obj_new_record(i64 m,struct qq_decls$varrec *defval);
void qq_records$obj_free_record(struct qq_decls$objrec *p);
void qq_records$var_dupl_record(struct qq_decls$varrec *a);
i64 qq_records$var_equal_record(struct qq_decls$varrec *x,struct qq_decls$varrec *y);
void qq_records$var_getix_record(struct qq_decls$varrec *a,i64 index);
void qq_records$var_putix_record(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *x);
void qq_records$var_getixref_record(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *dest);
void qq_records$start(void);
void qq_resolve$rx_module(struct qq_decls$filerec *pm);
void qq_resolve$rx_passdef(struct qq_decls$strec *owner,struct qq_decls$strec *p);
void qq_resolve$rx_deflist(struct qq_decls$strec *owner,struct qq_decls$strec *p,i64 doanon);
void qq_resolve$rx_unit(struct qq_decls$strec *owner,struct qq_decls$unitrec *p);
static void qq_resolve$rx_unitlist(struct qq_decls$strec *owner,struct qq_decls$unitrec *p);
static void qq_resolve$evalmonop(struct qq_decls$unitrec *p);
static void qq_resolve$evalbinop(struct qq_decls$unitrec *p,struct qq_decls$unitrec *lhs,struct qq_decls$unitrec *rhs);
static void qq_resolve$makeintconst(struct qq_decls$unitrec *p,i64 value);
static void qq_resolve$makerealconst(struct qq_decls$unitrec *p,r64 xvalue);
void qq_resolve$resolvename(struct qq_decls$strec *owner,struct qq_decls$unitrec *p,i64 mode);
struct qq_decls$strec *qq_resolve$resolvetopname(struct qq_decls$strec *owner,struct qq_decls$strec *stnewname,i64 moduleno,i64 allowmod);
static void qq_resolve$resolvedot(struct qq_decls$strec *owner,struct qq_decls$unitrec *p);
static void qq_resolve$resolvedot_sym(struct qq_decls$strec *owner,struct qq_decls$unitrec *p);
struct qq_decls$strec *qq_resolve$finddupl(struct qq_decls$strec *d,struct qq_decls$strec *pdupl);
static void qq_resolve$expandmacro(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,struct qq_decls$unitrec *b);
static struct qq_decls$unitrec *qq_resolve$copylistunit(struct qq_decls$unitrec *p);
static struct qq_decls$unitrec *qq_resolve$copyunit(struct qq_decls$unitrec *p);
static void qq_resolve$replaceunit(struct qq_decls$unitrec *p,struct qq_decls$unitrec *q);
static void qq_resolve$fixmode(struct qq_decls$strec *owner,struct qq_decls$strec *p);
static i64 qq_resolve$fixmode2(struct qq_decls$strec *owner,i64 m);
void qq_resolve$fixusertypes(void);
void qq_resolve$tx_typetable(void);
static i64 qq_resolve$getconstint(struct qq_decls$strec *owner,struct qq_decls$unitrec *a,i64 ownerid);
void qq_resolve$converttype(i64 m);
static void qq_resolve$scanstruct(i64 smode,struct qq_decls$strec *(*fields)[],i64 *index,i64 *isize,i64 offset,i64 calign,i64 *maxalign,i64 countmode);
static void qq_resolve$dobaseclass(i64 baseclassindex);
void qq_resolve$start(void);
void qq_runlab$disploop(void);
void qq_runlab$start(void);
void qq_runlab$fixupcode(struct qq_decls$filerec *pm);
i64 qq_runlab$runqprogram(struct qq_decls$subprogrec *sp,i64 ismain);
void qq_runaux$pcerror(u8 *mess,u8 *param);
void qq_runaux$pcustype(u8 *mess,struct qq_decls$varrec *x);
void qq_runaux$pcustype_t(u8 *mess,i64 t);
void qq_runaux$pcmxtypes(u8 *mess,struct qq_decls$varrec *x,struct qq_decls$varrec *y);
void qq_runaux$pcmxtypestt(u8 *mess,i64 t,i64 u);
void qq_runaux$reportpcerror(struct qq_pcltabs$pclrec *pcptr,u8 *mess,u8 *param);
static struct qq_decls$locrec qq_runaux$getpcerrorpos(struct qq_pcltabs$pclrec *pc);
static i64 qq_runaux$findmodulefrompc(struct qq_pcltabs$pclrec *pc);
struct qq_decls$varrec *qq_runaux$k_makelist(struct qq_decls$varrec *sp,i64 lower,i64 n);
void qq_runaux$k_len(struct qq_decls$varrec *sp);
void qq_runaux$k_maths(struct qq_decls$varrec *sp,i64 opc);
void qq_runaux$k_lwb(struct qq_decls$varrec *sp);
void qq_runaux$k_upb(struct qq_decls$varrec *sp);
void qq_runaux$k_swap(struct qq_decls$varrec *x,struct qq_decls$varrec *y);
void qq_runaux$k_bounds(struct qq_decls$varrec *sp,i64 *lower,i64 *upper);
i64 qq_runaux$k_type(struct qq_decls$varrec *sp,i64 n);
void qq_runaux$k_dot(struct qq_decls$varrec *sp,i64 index);
void qq_runaux$k_dotref(struct qq_decls$varrec *sp,i64 index);
struct qq_decls$varrec *qq_runaux$k_popdot(struct qq_decls$varrec *sp,i64 index);
static struct qq_decls$strec *qq_runaux$resolvefield(i64 index,i64 rectype);
void qq_runaux$k_convrefpack(struct qq_decls$varrec *sp);
void qq_runaux$k_incrptr(struct qq_decls$varrec *p,i64 step);
i64 qq_runaux$k_cmp(i64 cc,struct qq_decls$varrec *x,struct qq_decls$varrec *y);
i64 qq_runaux$k_bytesize(struct qq_decls$varrec *sp);
i64 qq_runaux$k_when(struct qq_decls$varrec *x,struct qq_decls$varrec *y);
struct qq_pcltabs$pclrec *qq_runaux$raiseexception(i64 exceptno,struct qq_decls$varrec **sp,byte **fp);
i64 qq_runaux$runproc_m(void *amsg);
void qq_runaux$runproc(void *fnptr,struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *dest);
struct qq_decls$varrec *qq_runaux$k_keyindex(struct qq_decls$varrec *sp);
struct qq_decls$varrec *qq_runaux$k_popkeyindex(struct qq_decls$varrec *sp);
struct qq_decls$varrec *qq_runaux$k_keyindexref(struct qq_decls$varrec *sp);
struct qq_pcltabs$pclrec *qq_runaux$k_map(struct qq_decls$varrec *sp,struct qq_pcltabs$pclrec *pc,struct qq_decls$varrec **newsp);
void qq_runaux$start(void);
void qq_sets$obj_free_set(struct qq_decls$objrec *p);
void qq_sets$var_dupl_set(struct qq_decls$varrec *a);
i64 qq_sets$var_equal_set(struct qq_decls$varrec *x,struct qq_decls$varrec *y);
static i64 qq_sets$getsetbytes(struct qq_decls$varrec *x);
void qq_sets$var_make_set(struct qq_decls$varrec *data,struct qq_decls$varrec *dest,i64 n);
struct qq_decls$objrec *qq_sets$obj_newset(i64 length);
void qq_sets$var_emptyset(struct qq_decls$varrec *dest);
void qq_sets$var_getix_set(struct qq_decls$varrec *a,i64 index);
void qq_sets$var_putix_set(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *x);
void qq_sets$var_getixref_set(struct qq_decls$varrec *a,i64 index);
static byte *qq_sets$getoffset(byte *p,i64 index,i64 *newoffset);
i64 qq_sets$var_in_set(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_sets$iresizeset(struct qq_decls$varrec *p,i64 n);
void qq_sets$obj_resize_set(struct qq_decls$objrec *p,i64 n);
void qq_sets$iorsetbits(i64 *p,i64 *q,i64 n);
void qq_sets$ixorsetbits(i64 *p,i64 *q,i64 n);
void qq_sets$iandsetbits(u64 *p,u64 *q,i64 n);
void qq_sets$inotsetbits(u64 *p,i64 n);
void qq_sets$var_iorto_set(struct qq_decls$varrec *x,struct qq_decls$varrec *y);
void qq_sets$var_iandto_set(struct qq_decls$varrec *x,struct qq_decls$varrec *y);
void qq_sets$var_ixorto_set(struct qq_decls$varrec *x,struct qq_decls$varrec *y);
void qq_sets$var_inotto_set(struct qq_decls$varrec *x);
void qq_sets$start(void);
void qq_strings$start(void);
void qq_strings$var_empty_string(struct qq_decls$varrec *dest,i64 mutable);
void qq_strings$var_make_string(u8 *s,struct qq_decls$varrec *dest,i64 mutable);
void qq_strings$var_make_stringn(u8 *s,i64 length,struct qq_decls$varrec *dest,i64 mutable);
struct qq_decls$objrec *qq_strings$obj_new_string(i64 n);
struct qq_decls$objrec *qq_strings$obj_make_string(u8 *s,i64 mutable);
struct qq_decls$objrec *qq_strings$obj_make_stringn(u8 *s,i64 length,i64 mutable);
void qq_strings$obj_free_string(struct qq_decls$objrec *p);
void qq_strings$var_dupl_string(struct qq_decls$varrec *a);
void qq_strings$var_getix_string(struct qq_decls$varrec *a,i64 index);
void qq_strings$var_getixref_string(struct qq_decls$varrec *a,i64 index);
void qq_strings$var_getdotix_string(struct qq_decls$varrec *a,i64 index);
void qq_strings$var_getdotixref_string(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *dest);
void qq_strings$var_getslice_string(struct qq_decls$varrec *a,i64 i,i64 j);
static void qq_strings$stringslice(struct qq_decls$varrec *a,i64 i,i64 j,struct qq_decls$varrec *dest);
void qq_strings$var_putix_string(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *x);
void qq_strings$var_putslice_string(struct qq_decls$varrec *a,i64 i,i64 j,struct qq_decls$varrec *x);
void qq_strings$var_putdotix_string(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *x);
void qq_strings$obj_resize_string(struct qq_decls$objrec *p,i64 n);
void qq_strings$var_add_string(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_strings$var_addto_string(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_strings$var_addto_string_ch(struct qq_decls$varrec *a,i64 ch);
i64 qq_strings$var_equal_string(struct qq_decls$varrec *x,struct qq_decls$varrec *y);
i64 qq_strings$var_compare_string(struct qq_decls$varrec *x,struct qq_decls$varrec *y);
static i64 qq_strings$cmpstring_len(u8 *s,u8 *t,i64 slen,i64 tlen);
i64 qq_strings$var_inx_string(struct qq_decls$varrec *x,struct qq_decls$varrec *y);
void qq_strings$var_iconvcase(struct qq_decls$varrec *a,struct qq_decls$varrec *b,i64 upper);
void qq_strings$var_makestrslicexobj(u8 *s,i64 length,struct qq_decls$varrec *dest);
struct qq_decls$objrec *qq_strings$obj_make_strslicexobj(u8 *s,i64 length);
static i64 qq_strings$var_asc(struct qq_decls$varrec *a);
void qq_strings$var_new_string(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *dest);
void qq_strings$var_new_stringn(i64 length,struct qq_decls$varrec *dest);
void qq_strings$var_mul_string(struct qq_decls$varrec *a,i64 m);
void qq_strings$var_convert_string_list(struct qq_decls$varrec *a,i64 t,struct qq_decls$varrec *dest);
void qq_strings$var_expand_string(struct qq_decls$varrec *a,struct qq_decls$varrec *dest,i64 m);
void qq_strings$var_makechar(i64 ch,struct qq_decls$varrec *dest);
i64 qq_syslibsdummy$loadsysmodule(struct qq_decls$filerec *pm);
void qq_syslibsdummy$start(void);
void qq_tables$start(void);
void qq_show$printunit(struct qq_decls$unitrec *p,i64 level,u8 *prefix,void *dev);
static void qq_show$printunitlist(void *dev,struct qq_decls$unitrec *p,i64 level,u8 *prefix);
static u8 *qq_show$getprefix(i64 level,u8 *prefix,struct qq_decls$unitrec *p);
static u8 *qq_show$getlineinfok(void);
void qq_show$printglobalsymbols(void *f);
void qq_show$printst(void *f,struct qq_decls$strec *p,i64 level);
static void qq_show$printstrec(void *f,struct qq_decls$strec *p,i64 level);
void qq_show$printtypetables(void *f);
void qq_show$showsttree(void);
void qq_show$showtypes(void);
void qq_show$showast(struct qq_decls$subprogrec *sp,u8 *file);
void qq_show$showast2(void *f,struct qq_decls$subprogrec *sp);
void qq_show$showlogfile(void);
static void qq_show$addtolog(u8 *filename,void *logdest);
void qq_show$showstflat(void);
void qq_show$showmoduleinfo(void *dev);
void qq_show$printsymbol(struct qq_decls$lexrec *lp);
u8 *qq_show$strmode(i64 t,i64 expand);
static void qq_show$istrmode(i64 t,u8 *dest,i64 expand);
void qq_show$deletetempfiles(void);
void qq_show$start(void);
static void qq_showpcl$writepcl(struct qq_pcltabs$pclrec *pcstart,struct qq_pcltabs$pclrec *pc,i32 *pclsource,i64 pass,u8 *sourcecode);
static u8 *qq_showpcl$writepclopnd(struct qq_pcltabs$pclrec *pcstart,struct qq_pcltabs$pclrec *pc,i64 pass);
void qq_showpcl$writeallpcl(struct qq_decls$filerec *pm,i64 pass);
void qq_showpcl$showpcl(struct qq_decls$subprogrec *sp,i64 pass);
void qq_showpcl$showpcl2(struct qq_decls$subprogrec *sp,i64 pass);
void qq_showpcl$gstr(u8 *s);
void qq_showpcl$gstrln(u8 *s);
void qq_showpcl$gline(void);
void qq_showpcl$gstrint(i64 a);
void qq_showpcl$glabeldef(struct qq_pcltabs$pclrec *pcstart,struct qq_pcltabs$pclrec *pc);
void qq_showpcl$start(void);
void qq_vars$var_unshareu(struct qq_decls$varrec *p);
void qq_vars$obj_shareu(struct qq_decls$objrec *p);
struct qq_decls$varrec *qq_vars$void_new(void);
struct qq_decls$objrec *qq_vars$obj_new(void);
i64 qq_vars$var_getintvalue(struct qq_decls$varrec *p);
void qq_vars$var_fromobj(i64 tag,struct qq_decls$objrec *p,struct qq_decls$varrec *dest);
void qq_vars$var_free(struct qq_decls$varrec *a);
void qq_vars$var_duplu(struct qq_decls$varrec *a);
void qq_vars$var_neg(struct qq_decls$varrec *a);
void qq_vars$var_abs(struct qq_decls$varrec *a);
void qq_vars$var_inot(struct qq_decls$varrec *a);
i64 qq_vars$var_istruel(struct qq_decls$varrec *a);
void qq_vars$var_add(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_addmixed(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
i64 qq_vars$var_addto(struct qq_decls$varrec *p,struct qq_decls$varrec *b);
void qq_vars$var_sub(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_submixed(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_mul(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_mulmixed(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_div(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_divmixed(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_idiv(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_irem(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_iand(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_ior(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_ixor(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_shl(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_shr(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
i64 qq_vars$var_in(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
i64 qq_vars$var_inx(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
i64 qq_vars$var_equal(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
i64 qq_vars$var_equalmixed(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
i64 qq_vars$var_compare(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
i64 qq_vars$var_comparemixed(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_concat(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_append(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_min(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_max(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
i64 qq_vars$var_concatto(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
i64 qq_vars$var_appendto(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_getix(struct qq_decls$varrec *a,i64 index);
void qq_vars$var_putix(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *x);
void qq_vars$var_getixref(struct qq_decls$varrec *a,i64 index);
void qq_vars$var_getslice(struct qq_decls$varrec *a,i64 i,i64 j);
void qq_vars$var_putslice(struct qq_decls$varrec *a,i64 i,i64 j,struct qq_decls$varrec *x);
void qq_vars$var_getdotix(struct qq_decls$varrec *a,i64 index);
void qq_vars$var_putdotix(struct qq_decls$varrec *p,i64 index,struct qq_decls$varrec *x);
void qq_vars$var_getdotixref(struct qq_decls$varrec *p,i64 index);
void qq_vars$var_getdotslice(struct qq_decls$varrec *a,i64 i,i64 j);
void qq_vars$var_putdotslice(struct qq_decls$varrec *p,i64 i,i64 j,struct qq_decls$varrec *x);
void qq_vars$var_getdotsliceref(struct qq_decls$varrec *p,i64 i,i64 j);
void qq_vars$var_expand(struct qq_decls$varrec *a,struct qq_decls$varrec *dest,i64 m);
void qq_vars$var_inplace(i64 index,struct qq_decls$varrec *px,struct qq_decls$varrec *y);
void qq_vars$var_inplace_unary(struct qq_decls$varrec *px,void (*fnneg)(struct qq_decls$varrec *,...));
void qq_vars$var_loadptr(struct qq_decls$varrec *x,struct qq_decls$varrec *y);
void qq_vars$var_storeptr(struct qq_decls$varrec *p,struct qq_decls$varrec *q);
void qq_vars$var_loadbit(byte *p,i64 shift,i64 t,i64 bitlength,struct qq_decls$varrec *dest);
void qq_vars$var_storebit(byte *p,i64 shift,struct qq_decls$varrec *q,i64 t,i64 bitlength);
void qq_vars$var_convert(struct qq_decls$varrec *x,i64 t,struct qq_decls$varrec *dest);
i64 qq_vars$var_gethashvalue(struct qq_decls$varrec *p);
void qq_vars$var_objtovar(i64 tag,struct qq_decls$objrec *p,struct qq_decls$varrec *q);
void qq_vars$var_putdotix_intint(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *b);
void qq_vars$var_power(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$var_powermixed(struct qq_decls$varrec *a,struct qq_decls$varrec *b);
void qq_vars$start(void);
void msysc$m_init(i64 nargs,u8 *(*args)[]);
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
i64 msysc$expandstr(u8 *s,u8 *t,i64 n,struct msysc$fmtrec *fmt);
static i64 msysc$u64tostr(u64 aa,u8 *s,u64 base,i64 sep);
i64 msysc$i64tostrfmt(i64 aa,u8 *s,struct msysc$fmtrec *fmt);
i64 msysc$u64tostrfmt(i64 aa,u8 *s,struct msysc$fmtrec *fmt);
static i64 msysc$i64mintostr(u8 *s,i64 base,i64 sep);
i64 msysc$strtostrfmt(u8 *s,u8 *t,i64 n,struct msysc$fmtrec *fmt);
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
void *mlib$pcm_allocnfz(i64 n);
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
i64 mlinux$dirlist(u8 *filespec,u8 *(*dest)[],i64 capacity,i64 t);
void mlinux$start(void);
u64 mwindllc$os_calldllfunction(void (*fnaddr)(void),i64 retcode,i64 nargs,i64 (*args)[],byte (*argcodes)[]);
u64 mwindllc$os_pushargs(u64 (*args)[],i64 nargs,i64 nextra,void (*fnaddr)(void),i64 isfloat);
static i64 mwindllc$calldll_cint(void (*fnaddr)(void),i64 (*params)[],i64 nparams);
static i64 mwindllc$calldll_creal(void (*fnaddr)(void),i64 (*params)[],i64 nparams);
void mwindllc$os_dummycall(r64 a,r64 b,r64 c,r64 d);
void mwindllc$start(void);

/* VARS */
static i64 qq_cli$nalldot;
static i64 qq_cli$nalldot1field;
static u8 *  qq_cli$runnames[6] = {(byte*)"load_cc",(byte*)"parse_cc",(byte*)"names_cc",(byte*)"gencode_cc",(byte*)"fixup_cc",(byte*)"run_cc"};
static byte qq_cli$fshowpcl1;
static byte qq_cli$fshowpcl2;
static byte qq_cli$fshowast1;
static byte qq_cli$fshowast2;
static byte qq_cli$fshowst;
static byte qq_cli$fshowstflat;
static byte qq_cli$fshowtypes;
static byte qq_cli$foptimise = (byte)(i64)1;
static byte qq_cli$fwriteqa;
static byte qq_cli$fshowmodules;
static byte qq_cli$fallsp;
static byte qq_cli$runcode = (byte)(i64)6;
static u8 *  qq_cli$sourcestr;
static u8 *  qq_cli$inputfile;
static struct qq_decls$strec *  qq_cli$allprocdefs;
static struct qq_decls$strec *  qq_cli$allstaticdefs;
static struct mlib$strbuffer *  qq_cli$pclstr;
static i64 qq_cli$cmdstartindex;
static struct qq_decls$lexrec qq_decls$nextlx;
static struct qq_decls$lexrec qq_decls$lx;
static i64 qq_decls$qpos;
static i64 qq_decls$pcerrorpos;
static struct qq_decls$filerec *  qq_decls$pcerrormodule;
static struct qq_decls$varrec qq_decls$varstack[70000];
static struct qq_decls$varrec *  qq_decls$sptr;
static struct qq_decls$varrec *  qq_decls$stacklimit;
static byte *  qq_decls$frameptr;
static struct qq_pcltabs$pclrec *  qq_decls$pcptr;
static i64 qq_decls$stopped;
static struct qq_decls$strec *  qq_decls$stprogram;
static struct qq_decls$strec *  qq_decls$stmodule;
static struct qq_decls$strec *  qq_decls$stsubprog;
static struct qq_decls$strec *  qq_decls$stcurrmodule;
static struct qq_decls$strec *  qq_decls$stcurrproc;
static struct qq_decls$filerec *  qq_decls$currmodule;
static i64 qq_decls$debug;
static i64 qq_decls$inproc;
static struct qq_decls$genfieldrec *  qq_decls$genfieldtable[1000];
static i64 qq_decls$ngenfields;
static i64 qq_decls$nlibfiles;
static struct qq_decls$strec *  qq_decls$libtable[50];
static byte qq_decls$libtypes[50];
static u64 qq_decls$dllinsttable[50];
static i64 qq_decls$ndllprocs;
static struct qq_decls$strec *  qq_decls$dllproctable[2000];
static byte qq_decls$dllproclibindex[2000];
static void *  qq_decls$dllprocaddr[2000];
static byte qq_decls$usebundled = (byte)(i64)1;
static u8 *  qq_decls$dispatchnames[6] = {(byte*)"-lab",(byte*)"-sw",(byte*)"-fn",(byte*)"-debug",(byte*)"-fdebug",(byte*)"-asm"};
static i64 qq_decls$nqparams;
static u8 *  qq_decls$qparamtable[32];
static struct qq_decls$procrec *  qq_decls$proclist;
static struct qq_decls$procrec *  qq_decls$proclistx;
static i64 qq_decls$nproclist;
static void (*qq_decls$pcl_callbackfn)(void) = 0;
static struct qq_decls$objrec *  qq_decls$chrtable[256];
static byte qq_decls$fnosys;
static byte qq_decls$fverbose;
static i16 qq_decls$baseclasstable[256];
static struct qq_decls$strec *  qq_decls$baseclassdef[256];
static i64 qq_decls$nbaseclasses;
static i64 qq_decls$lastretindex;
static struct qq_decls$filerec *  qq_decls$modules[201];
static struct qq_decls$subprogrec *  qq_decls$subprogs[30];
static i64 qq_decls$nmodules;
static i64 qq_decls$nsubprogs;
static i64 qq_decls$nalllines;
static u8 *  qq_decls$qafilenames[100];
static u8 *  qq_decls$qatext[100];
static i64 qq_decls$qasize[100];
static i64 qq_decls$nqafiles;
static u8 *  qq_decls$optionnames[23] = {
    (byte*)"load",
    (byte*)"parse",
    (byte*)"names",
    (byte*)"gen",
    (byte*)"fixup",
    (byte*)"run",
    (byte*)"ast1",
    (byte*)"ast2",
    (byte*)"pcl1",
    (byte*)"pcl2",
    (byte*)"allsp",
    (byte*)"st",
    (byte*)"stflat",
    (byte*)"types",
    (byte*)"modules",
    (byte*)"opt",
    (byte*)"no",
    (byte*)"ext",
    (byte*)"qa",
    (byte*)"qas",
    (byte*)"v",
    (byte*)"nosys",
    (byte*)"sys"
};
static byte *  qq_decls$optionvars[23] = {
    &qq_cli$runcode,
    &qq_cli$runcode,
    &qq_cli$runcode,
    &qq_cli$runcode,
    &qq_cli$runcode,
    &qq_cli$runcode,
    &qq_cli$fshowast1,
    &qq_cli$fshowast2,
    &qq_cli$fshowpcl1,
    &qq_cli$fshowpcl2,
    &qq_cli$fallsp,
    &qq_cli$fshowst,
    &qq_cli$fshowstflat,
    &qq_cli$fshowtypes,
    &qq_cli$fshowmodules,
    &qq_cli$foptimise,
    &qq_cli$foptimise,
    &qq_decls$usebundled,
    &qq_cli$fwriteqa,
    &qq_cli$fwriteqa,
    &qq_decls$fverbose,
    &qq_decls$fnosys,
    &qq_decls$fnosys
};
static byte qq_decls$optionvalues[23] = {
    (u8)1u,
    (u8)2u,
    (u8)3u,
    (u8)4u,
    (u8)5u,
    (u8)6u,
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
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)0u
};
static struct qq_pcltabs$pclrec *  qq_decls$stopseq;
static struct qq_pcltabs$pclrec *  qq_decls$raiseseq;
static i64 qq_decls$nproclocals;
static struct qq_pcltabs$pclrec *  qq_decls$pproclocals;
static i64 qq_decls$pclcounts[193];
static i64 qq_decls$nallpcl;
static u8 *  qq_decimal$fpnames[4] = {(byte*)"zero_type",(byte*)"normal_type",(byte*)"inf_type",(byte*)"nan_type"};
static i64 qq_decimal$currprec = (i64)55;
static i64 qq_decimal$stblz;
static struct qq_decimal$constrec *  qq_decimal$constlist = 0;
static i64 qq_decimal$decstrsize;
static struct qq_decls$varrec qq_decimal$vtemp;
static struct qq_decls$strec *(*qq_host$procrefs)[];
static u8 *  qq_lex$lxsource;
static u8 *  qq_lex$lxstart;
static u8 *  qq_lex$lxsptr;
static i64 qq_lex$lxifcond;
static i64 qq_lex$longsuffix;
static i64 qq_lex$lxlineno;
static i64 qq_lex$nextlxlength;
static i64 qq_lex$lxlength;
static struct qq_decls$strec qq_lex$hashtable[32768];
static struct qq_decls$strec *  qq_lex$hashtablelast;
static u8 *  qq_lex$u64maxstr = (byte*)"18446744073709551615";
static byte qq_lex$namemap[256];
static i64 qq_lib$currlineno;
static i64 qq_lib$nextavindex = (i64)0;
static struct mlib$strbuffer qq_lib$exprstrvar;
static struct mlib$strbuffer *  qq_lib$exprstr = &qq_lib$exprstrvar;
static i64 qq_lib$nlocalunits;
static u8 *  qq_lib$errormess;
static byte qq_lib$bytemasks[8] = {(u8)1u,(u8)2u,(u8)4u,(u8)8u,(u8)16u,(u8)32u,(u8)64u,(u8)128u};
static struct qq_decls$objrec *  qq_lists$emptylist;
static i64 qq_names$sdsize;
static i64 qq_names$sdoffset;
static i64 qq_names$sdaligned;
static i64 qq_names$sdlevel;
static i64 qq_names$sdmode;
static i64 qq_names$sdnfields;
static i64 qq_names$sdmaxalign;
static byte qq_names$sdunion[10];
static i64 qq_names$sdmaxsize[10];
static i64 qq_parse$intabledata;
static u8 *  qq_parse$tabledataname = 0;
static struct qq_decls$unitrec *  qq_parse$dollarstack[10];
static i64 qq_parse$ndollar = (i64)0;
static byte qq_parse$yieldseen;
static i64 qq_parse$currdllindex;
static i64 qq_parse$nextlambdaindex;
static i64 qq_parse$listtypestack[20];
static i64 qq_parse$nlisttype;
static i64 qq_parse$listtype;
static u8 *  qq_pcltabs$opndnames[18] = {
    (byte*)"cnone",
    (byte*)"cstatic",
    (byte*)"cframe",
    (byte*)"cproc",
    (byte*)"cdllproc",
    (byte*)"cgenfield",
    (byte*)"clabel",
    (byte*)"cint",
    (byte*)"creal",
    (byte*)"cstring",
    (byte*)"cstringz",
    (byte*)"ctype",
    (byte*)"csymbol",
    (byte*)"coperator",
    (byte*)"cmaths",
    (byte*)"chost",
    (byte*)"cbinto",
    (byte*)"?"
};
static u8 *  qq_pcltabs$pclnames[193] = {
    (byte*)"knop",
    (byte*)"kskip",
    (byte*)"kprocdef",
    (byte*)"kprocent",
    (byte*)"kprocend",
    (byte*)"kendmod",
    (byte*)"kcomment",
    (byte*)"kpushm",
    (byte*)"kpushf",
    (byte*)"kpushmref",
    (byte*)"kpushfref",
    (byte*)"kpopm",
    (byte*)"kpopf",
    (byte*)"kpushci",
    (byte*)"kpushvoid",
    (byte*)"kpushnil",
    (byte*)"kpushcr",
    (byte*)"kpushcs",
    (byte*)"kpushtype",
    (byte*)"kpushopc",
    (byte*)"kpushsym",
    (byte*)"kpushptr",
    (byte*)"kpopptr",
    (byte*)"kzpopm",
    (byte*)"kzpopf",
    (byte*)"kdupl",
    (byte*)"kcopy",
    (byte*)"kswap",
    (byte*)"kconvrefp",
    (byte*)"kjump",
    (byte*)"kjumpptr",
    (byte*)"kjumpt",
    (byte*)"kjumpf",
    (byte*)"kjumpeq",
    (byte*)"kjumpne",
    (byte*)"kjumplt",
    (byte*)"kjumple",
    (byte*)"kjumpge",
    (byte*)"kjumpgt",
    (byte*)"kwheneq",
    (byte*)"kwhenne",
    (byte*)"kjumplab",
    (byte*)"kswitch",
    (byte*)"ktom",
    (byte*)"ktof",
    (byte*)"kformci",
    (byte*)"kforfci",
    (byte*)"kformm",
    (byte*)"kforff",
    (byte*)"kcallproc",
    (byte*)"kcallptr",
    (byte*)"kretproc",
    (byte*)"kretfn",
    (byte*)"kmodcall",
    (byte*)"kmodret",
    (byte*)"kcalldll",
    (byte*)"kcallhost",
    (byte*)"kunshare",
    (byte*)"kaddsp",
    (byte*)"kstop",
    (byte*)"kmakelist",
    (byte*)"kmakevrec",
    (byte*)"kmakeax",
    (byte*)"kmakebits",
    (byte*)"kmaketrec",
    (byte*)"kmakeset",
    (byte*)"kmakerang",
    (byte*)"kmakedict",
    (byte*)"kmakedec",
    (byte*)"kincrptr",
    (byte*)"kincrtom",
    (byte*)"kincrtof",
    (byte*)"kloadincr",
    (byte*)"kincrload",
    (byte*)"kneg",
    (byte*)"kabs",
    (byte*)"knotl",
    (byte*)"kinot",
    (byte*)"kistruel",
    (byte*)"kasc",
    (byte*)"kchr",
    (byte*)"ksqr",
    (byte*)"kmaths",
    (byte*)"kmaths2",
    (byte*)"kunaryto",
    (byte*)"knotlto",
    (byte*)"klen",
    (byte*)"klwb",
    (byte*)"kupb",
    (byte*)"kbounds",
    (byte*)"kbytesize",
    (byte*)"ktype",
    (byte*)"kdictsize",
    (byte*)"kisfound",
    (byte*)"kminval",
    (byte*)"kmaxval",
    (byte*)"kistype",
    (byte*)"kisvoid",
    (byte*)"kconvert",
    (byte*)"ktypepun",
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
    (byte*)"kinx",
    (byte*)"kcmp",
    (byte*)"kmin",
    (byte*)"kmax",
    (byte*)"kconcat",
    (byte*)"kappend",
    (byte*)"ksame",
    (byte*)"kpower",
    (byte*)"kbinto",
    (byte*)"kandlto",
    (byte*)"korlto",
    (byte*)"kconcatto",
    (byte*)"kappendto",
    (byte*)"kdot",
    (byte*)"kdot1",
    (byte*)"kpopdot",
    (byte*)"kpopdot1",
    (byte*)"kdotref",
    (byte*)"kindex",
    (byte*)"kpopix",
    (byte*)"kindexref",
    (byte*)"kkeyindex",
    (byte*)"kpopkeyix",
    (byte*)"kkeyixref",
    (byte*)"kdotix",
    (byte*)"kpopdotix",
    (byte*)"kdotixref",
    (byte*)"kexpand",
    (byte*)"kpushtry",
    (byte*)"kraise",
    (byte*)"kmap",
    (byte*)"kpushfff",
    (byte*)"kpushff",
    (byte*)"kpushmm",
    (byte*)"kpushfm",
    (byte*)"kpushmf",
    (byte*)"kpushmci",
    (byte*)"kpushfci",
    (byte*)"kmoveff",
    (byte*)"kmovemm",
    (byte*)"kmovefm",
    (byte*)"kmovemf",
    (byte*)"kzmoveff",
    (byte*)"kmovefci",
    (byte*)"kmovemci",
    (byte*)"kzmovefci",
    (byte*)"kpushv2",
    (byte*)"kpushv3",
    (byte*)"kjmpeqfci",
    (byte*)"kjmpnefci",
    (byte*)"kjmpltfci",
    (byte*)"kjmplefci",
    (byte*)"kjmpgefci",
    (byte*)"kjmpgtfci",
    (byte*)"kjmpeqff",
    (byte*)"kjmpneff",
    (byte*)"kjmpltff",
    (byte*)"kjmpleff",
    (byte*)"kjmpgeff",
    (byte*)"kjmpgtff",
    (byte*)"kaddfci",
    (byte*)"ksubfci",
    (byte*)"kaddff",
    (byte*)"ksubff",
    (byte*)"kaddci",
    (byte*)"kindexmf",
    (byte*)"kindexff",
    (byte*)"kswitchf",
    (byte*)"kpushptrf",
    (byte*)"kpushipm",
    (byte*)"kpushipf",
    (byte*)"kpopipm",
    (byte*)"kpopipf",
    (byte*)"kupbm",
    (byte*)"kupbf",
    (byte*)"klenf",
    (byte*)"kstoref",
    (byte*)"kwheneqci",
    (byte*)"kwhenneci",
    (byte*)"klastpcl"
};
static byte qq_pcltabs$pclopnd[193] = {
    (u8)0u,
    (u8)0u,
    (u8)12u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)1u,
    (u8)2u,
    (u8)1u,
    (u8)2u,
    (u8)7u,
    (u8)0u,
    (u8)0u,
    (u8)8u,
    (u8)9u,
    (u8)11u,
    (u8)13u,
    (u8)12u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)6u,
    (u8)0u,
    (u8)6u,
    (u8)6u,
    (u8)6u,
    (u8)6u,
    (u8)6u,
    (u8)6u,
    (u8)6u,
    (u8)6u,
    (u8)6u,
    (u8)6u,
    (u8)6u,
    (u8)0u,
    (u8)6u,
    (u8)6u,
    (u8)6u,
    (u8)6u,
    (u8)6u,
    (u8)6u,
    (u8)3u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)12u,
    (u8)0u,
    (u8)4u,
    (u8)15u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
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
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)14u,
    (u8)14u,
    (u8)13u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)11u,
    (u8)0u,
    (u8)11u,
    (u8)11u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
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
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)5u,
    (u8)5u,
    (u8)5u,
    (u8)5u,
    (u8)5u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)6u,
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
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u
};
static u32 qq_pcltabs$pclattrs[193] = {
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976366u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538999160u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976366u,
    (u32)(u64)538976366u,
    (u32)(u64)538998894u,
    (u32)(u64)544831598u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976366u,
    (u32)(u64)538976288u,
    (u32)(u64)538976366u,
    (u32)(u64)538976366u,
    (u32)(u64)538976366u,
    (u32)(u64)538999160u,
    (u32)(u64)538998136u,
    (u32)(u64)1987410296u,
    (u32)(u64)1987410296u,
    (u32)(u64)538998136u,
    (u32)(u64)538976376u,
    (u32)(u64)538976288u,
    (u32)(u64)538976376u,
    (u32)(u64)538976288u,
    (u32)(u64)538976376u,
    (u32)(u64)538976376u,
    (u32)(u64)538976376u,
    (u32)(u64)538976376u,
    (u32)(u64)538976376u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976366u,
    (u32)(u64)538976288u,
    (u32)(u64)538976366u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976366u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976366u,
    (u32)(u64)538976288u,
    (u32)(u64)538976355u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976366u,
    (u32)(u64)538999160u,
    (u32)(u64)538976376u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u,
    (u32)(u64)538976288u
};
static struct qq_pcltabs$bintorec qq_pcltabs$bintotable[11] = {
    {(i64)100,(void (*)(void))qq_vars$var_add,(void (*)(void))qq_vars$var_addmixed},
    {(i64)101,(void (*)(void))qq_vars$var_sub,(void (*)(void))qq_vars$var_submixed},
    {(i64)102,(void (*)(void))qq_vars$var_mul,(void (*)(void))qq_vars$var_mulmixed},
    {(i64)103,(void (*)(void))qq_vars$var_div,(void (*)(void))qq_vars$var_divmixed},
    {(i64)107,(void (*)(void))qq_vars$var_iand,0},
    {(i64)108,(void (*)(void))qq_vars$var_ior,0},
    {(i64)109,(void (*)(void))qq_vars$var_ixor,0},
    {(i64)115,(void (*)(void))qq_vars$var_min,0},
    {(i64)116,(void (*)(void))qq_vars$var_max,0},
    {(i64)110,(void (*)(void))qq_vars$var_shl,0},
    {(i64)111,(void (*)(void))qq_vars$var_shr,0}
};
static i64 qq_pclgen$loopstack[20][4];
static i64 qq_pclgen$trylevelstack[20];
static i64 qq_pclgen$loopindex = (i64)0;
static i64 qq_pclgen$looptrylevel;
static i64 qq_pclgen$trylevel = (i64)0;
static i64 qq_pclgen$retindex;
static i64 qq_pclgen$retvaloffset;
static i64 qq_pclgen$nprocparams;
static struct qq_pcltabs$pclrec *  qq_pclgen$pprocentry;
static i64 qq_pclgen$procskiplabel;
static struct qq_pcltabs$pclrec *  qq_pcllib$pcstart;
static struct qq_pcltabs$pclrec *  qq_pcllib$pccurr;
static struct qq_pcltabs$pclrec *  qq_pcllib$pcend;
static i64 qq_pcllib$pcalloc;
static i32 *  qq_pcllib$pcsourcestart;
static i32 *  qq_pcllib$pcsourcecurr;
static i64 qq_pcllib$pclcurrlineno;
static struct qq_pcltabs$pclrec *(*qq_pcllib$labelpctable)[];
static i64 qq_pcllib$labelalloc;
static i64 qq_pcllib$nextlabelno;
static i64 qq_print$mindev;
static i64 qq_print$moutdev;
static i64 *  qq_print$minchan;
static void *  qq_print$moutchan;
static struct qq_decls$varrec qq_print$minvar;
static struct qq_decls$varrec qq_print$moutvar;
static i32 qq_print$moutdevstack[6];
static void *  qq_print$moutchanstack[6];
static struct qq_decls$varrec qq_print$moutvarstack[6];
static byte qq_print$mgapstack[6];
static u8 *  qq_print$mfmtstrstack[6];
static u8 *  qq_print$mfmtcurrstack[6];
static i64 qq_print$noclevels;
static u8 *  qq_print$mfmtstr;
static u8 *  qq_print$mfmtcurr;
static struct msysc$fmtrec qq_print$defaultfmt = {
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
static byte qq_print$mgapneeded;
static u8 *  qq_print$kb_start;
static u8 *  qq_print$kb_pos;
static u8 *  qq_print$kb_lastpos;
static i64 qq_print$kb_size;
static i64 qq_print$kb_linelength;
static i64 qq_print$kb_length;
static i64 qq_print$kb_lastlength;
static u8 qq_print$termchar;
static i64 qq_print$itemerror;
static void *  qq_print$testfilech;
static i64 qq_print$listdepth = (i64)0;
static i64 qq_resolve$nprocs;
static i64 qq_resolve$noexpand;
static i64 qq_resolve$symbolmode;
static i64 qq_resolve$macrolevels;
static i64 qq_resolve$allowmodname;
static struct qq_decls$strec *  qq_resolve$macroparams[50];
static struct qq_decls$strec *  qq_resolve$macroparamsgen[50];
static struct qq_decls$unitrec *  qq_resolve$macroargs[50];
static i64 qq_resolve$nmacroparams;
static i64 qq_resolve$nmacroargs;
static struct qq_decls$strec *  qq_resolve$structfields[100];
static i64 qq_resolve$ntopfields;
static i64 qq_resolve$nallfields;
static byte qq_runlab$getjt;
static int * (*qq_runlab$jumptable)[];
static struct qq_decls$objrec *  qq_strings$emptystring;
static u8 *  qq_tables$stdtypenames[41] = {
    (byte*)"void",
    (byte*)"int",
    (byte*)"real",
    (byte*)"decimal",
    (byte*)"range",
    (byte*)"set",
    (byte*)"dict",
    (byte*)"vector",
    (byte*)"bits",
    (byte*)"string",
    (byte*)"list",
    (byte*)"array",
    (byte*)"record",
    (byte*)"struct",
    (byte*)"refvar",
    (byte*)"refbit",
    (byte*)"refpack",
    (byte*)"symbol",
    (byte*)"type",
    (byte*)"operator",
    (byte*)"retaddr",
    (byte*)"except",
    (byte*)"number",
    (byte*)"i8",
    (byte*)"i16",
    (byte*)"i32",
    (byte*)"i64",
    (byte*)"u8",
    (byte*)"u16",
    (byte*)"u32",
    (byte*)"u64",
    (byte*)"r32",
    (byte*)"r64",
    (byte*)"u1",
    (byte*)"u2",
    (byte*)"u4",
    (byte*)"packstrc",
    (byte*)"packstrz",
    (byte*)"stringz",
    (byte*)"refproc",
    (byte*)"slice"
};
static byte qq_tables$stdtypewidths[41] = {
    (u8)0u,
    (u8)64u,
    (u8)64u,
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
    (u8)64u,
    (u8)128u,
    (u8)64u,
    (u8)64u,
    (u8)64u,
    (u8)64u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)8u,
    (u8)16u,
    (u8)32u,
    (u8)64u,
    (u8)8u,
    (u8)16u,
    (u8)32u,
    (u8)64u,
    (u8)32u,
    (u8)64u,
    (u8)1u,
    (u8)2u,
    (u8)4u,
    (u8)0u,
    (u8)0u,
    (u8)64u,
    (u8)64u,
    (u8)0u
};
static u8 *  qq_tables$jtagnames[92] = {
    (byte*)"jnone",
    (byte*)"jlabeldef",
    (byte*)"jassign",
    (byte*)"jkeyword",
    (byte*)"jkeyvalue",
    (byte*)"joperator",
    (byte*)"jblock",
    (byte*)"jif",
    (byte*)"jselect",
    (byte*)"jwhenthen",
    (byte*)"jcase",
    (byte*)"jdocase",
    (byte*)"jswitch",
    (byte*)"jdoswitch",
    (byte*)"jrecase",
    (byte*)"jfor",
    (byte*)"jforx",
    (byte*)"jforall",
    (byte*)"jforeach",
    (byte*)"jdo",
    (byte*)"jto",
    (byte*)"jwhile",
    (byte*)"jrepeat",
    (byte*)"jtry",
    (byte*)"jexcept",
    (byte*)"jraise",
    (byte*)"jcall",
    (byte*)"jcallhost",
    (byte*)"jnil",
    (byte*)"jswap",
    (byte*)"jgoto",
    (byte*)"jstop",
    (byte*)"jreturn",
    (byte*)"jeval",
    (byte*)"jtypeconst",
    (byte*)"jconvert",
    (byte*)"jtypepun",
    (byte*)"jmap",
    (byte*)"jcmpchain",
    (byte*)"jname",
    (byte*)"jsymbol",
    (byte*)"jintconst",
    (byte*)"jrealconst",
    (byte*)"jstringconst",
    (byte*)"jdecimal",
    (byte*)"jstrinclude",
    (byte*)"jdot",
    (byte*)"jindex",
    (byte*)"jdotindex",
    (byte*)"jkeyindex",
    (byte*)"jloop",
    (byte*)"jptr",
    (byte*)"jaddrof",
    (byte*)"jvoid",
    (byte*)"jprint",
    (byte*)"jfprint",
    (byte*)"jnogap",
    (byte*)"jspace",
    (byte*)"jfmtitem",
    (byte*)"jread",
    (byte*)"jincrload",
    (byte*)"jloadincr",
    (byte*)"junary",
    (byte*)"jbin",
    (byte*)"jmaths",
    (byte*)"jmaths2",
    (byte*)"jproperty",
    (byte*)"jbounds",
    (byte*)"jgettype",
    (byte*)"jistype",
    (byte*)"jisvoid",
    (byte*)"jcmp",
    (byte*)"jandl",
    (byte*)"jorl",
    (byte*)"jnotl",
    (byte*)"jistruel",
    (byte*)"jin",
    (byte*)"jinx",
    (byte*)"junaryto",
    (byte*)"jbinto",
    (byte*)"jandlto",
    (byte*)"jorlto",
    (byte*)"jnotlto",
    (byte*)"jistruelto",
    (byte*)"jappendto",
    (byte*)"jconcatto",
    (byte*)"jidivrem",
    (byte*)"jmakerange",
    (byte*)"jmakelist",
    (byte*)"jmakeset",
    (byte*)"jmakedict",
    (byte*)"jcvattr"
};
static byte qq_tables$jflags[92] = {
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
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
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)2u,
    (u8)1u,
    (u8)0u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)2u,
    (u8)1u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)2u,
    (u8)2u,
    (u8)0u,
    (u8)0u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)2u,
    (u8)1u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)0u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u
};
static byte qq_tables$jhasvalue[92] = {
    (u8)0u,
    (u8)0u,
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)2u,
    (u8)0u,
    (u8)2u,
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
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)2u,
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
    (u8)2u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)1u
};
static u64 qq_tables$symbolnames[132] = {
    (u64)491496043109u,
    (u64)7630692u,
    (u64)418447716195u,
    (u64)1768777075u,
    (u64)474315779939u,
    (u64)121390429860705u,
    (u64)122545691846003u,
    (u64)1701865840u,
    (u64)118074580820588u,
    (u64)118074580820594u,
    (u64)7435116u,
    (u64)7435122u,
    (u64)133506683724652u,
    (u64)133506683724658u,
    (u64)7500912u,
    (u64)7496034u,
    (u64)29793u,
    (u64)7957695015460107633u,
    (u64)1919181921u,
    (u64)435526984050u,
    (u64)8316305113558576229u,
    (u64)6579297u,
    (u64)6452595u,
    (u64)7107949u,
    (u64)7760228u,
    (u64)1986618473u,
    (u64)1835364969u,
    (u64)30792314748757097u,
    (u64)1818521185u,
    (u64)7107183u,
    (u64)1684955497u,
    (u64)7499625u,
    (u64)1919907945u,
    (u64)7104627u,
    (u64)7497843u,
    (u64)132359843113325u,
    (u64)110425311047777u,
    (u64)127961628831587u,
    (u64)28265u,
    (u64)7892585u,
    (u64)491328597872u,
    (u64)1701667187u,
    (u64)29029u,
    (u64)25966u,
    (u64)29804u,
    (u64)25964u,
    (u64)25959u,
    (u64)29799u,
    (u64)1819570030u,
    (u64)1953459817u,
    (u64)30510852590564201u,
    (u64)7561825u,
    (u64)6517601u,
    (u64)7497827u,
    (u64)495673696621u,
    (u64)55471255085421u,
    (u64)1886351984u,
    (u64)111533748417385u,
    (u64)110404004377449u,
    (u64)1919118953u,
    (u64)7106405u,
    (u64)6713189u,
    (u64)8391171955410366057u,
    (u64)8391171955409249636u,
    (u64)32778015450820710u,
    (u64)8391171955410233443u,
    (u64)8391171955410236531u,
    (u64)7308604897319546485u,
    (u64)1701667182u,
    (u64)28552639593870451u,
    (u64)491260502889u,
    (u64)26217u,
    (u64)1852139636u,
    (u64)439855836261u,
    (u64)1702063205u,
    (u64)7310293699684166757u,
    (u64)131337507007589u,
    (u64)30510843782458469u,
    (u64)6581861u,
    (u64)126939460038261u,
    (u64)1702060387u,
    (u64)111546229548402u,
    (u64)1852139639u,
    (u64)7499622u,
    (u64)28532u,
    (u64)31074u,
    (u64)28516u,
    (u64)435610544247u,
    (u64)127961662514546u,
    (u64)465625706101u,
    (u64)121437875889522u,
    (u64)1886352499u,
    (u64)1886351212u,
    (u64)1869901671u,
    (u64)114776364119923u,
    (u64)500068610672u,
    (u64)128017564332147u,
    (u64)1684104562u,
    (u64)1668248176u,
    (u64)1668183398u,
    (u64)465557414252u,
    (u64)110442657834354u,
    (u64)127970521019507u,
    (u64)474315583093u,
    (u64)111516500389741u,
    (u64)128034844732777u,
    (u64)28275532515798377u,
    (u64)1701869940u,
    (u64)6710642u,
    (u64)7496054u,
    (u64)478660485485u,
    (u64)28783u,
    (u64)500152823651u,
    (u64)119165519096935u,
    (u64)109304575259763u,
    (u64)121390429397347u,
    (u64)7959156u,
    (u64)128026086176869u,
    (u64)435727982962u,
    (u64)1953718627u,
    (u64)32195308665270115u,
    (u64)125762756439908u,
    (u64)1818326629u,
    (u64)27431034385752436u,
    (u64)7364973u,
    (u64)482871438435u,
    (u64)1885435763u,
    (u64)8391171955410303347u,
    (u64)121386319441768u,
    (u64)7104878u,
    (u64)30508623351411827u,
    (u64)123623711273075u
};
static u8 *  qq_tables$namenames[27] = {
    (byte*)"genericid",
    (byte*)"programid",
    (byte*)"subprogid",
    (byte*)"moduleid",
    (byte*)"dllmoduleid",
    (byte*)"procid",
    (byte*)"anonprocid",
    (byte*)"dllprocid",
    (byte*)"dllvarid",
    (byte*)"recordid",
    (byte*)"typeid",
    (byte*)"fieldid",
    (byte*)"structfieldid",
    (byte*)"staticid",
    (byte*)"frameid",
    (byte*)"paramid",
    (byte*)"dllparamid",
    (byte*)"labelid",
    (byte*)"constid",
    (byte*)"enumid",
    (byte*)"aliasid",
    (byte*)"linkid",
    (byte*)"macroid",
    (byte*)"macroparamid",
    (byte*)"structblockid",
    (byte*)"unionblockid",
    (byte*)"endblockid"
};
static u8 *  qq_tables$objtypenames[3] = {(byte*)"normal_obj",(byte*)"slice_obj",(byte*)"extslice_obj"};
static u8 *  qq_tables$scopenames[3] = {(byte*)"local_scope",(byte*)"global_scope",(byte*)"export_scope"};
static u8 *  qq_tables$stnames[198] = {
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
    (byte*)"stop",
    (byte*)"redoloop",
    (byte*)"nextloop",
    (byte*)"exit",
    (byte*)"goto",
    (byte*)"switch",
    (byte*)"doswitch",
    (byte*)"tabledata",
    (byte*)"enumdata",
    (byte*)"clamp",
    (byte*)"maps",
    (byte*)"mapss",
    (byte*)"eval",
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
    (byte*)"proc",
    (byte*)"sub",
    (byte*)"function",
    (byte*)"func",
    (byte*)"fun",
    (byte*)"method",
    (byte*)"type",
    (byte*)"record",
    (byte*)"struct",
    (byte*)"union",
    (byte*)"ref",
    (byte*)"var",
    (byte*)"macro",
    (byte*)"static",
    (byte*)"$caligned",
    (byte*)"const",
    (byte*)"module",
    (byte*)"import",
    (byte*)"importdll",
    (byte*)"strinclude",
    (byte*)"unless",
    (byte*)"try",
    (byte*)"except",
    (byte*)"raise",
    (byte*)"global",
    (byte*)"export",
    (byte*)"swap",
    (byte*)"void",
    (byte*)"int",
    (byte*)"real",
    (byte*)"string",
    (byte*)"list",
    (byte*)"array",
    (byte*)"vector",
    (byte*)"bits",
    (byte*)"set",
    (byte*)"dict",
    (byte*)"decimal",
    (byte*)"longint",
    (byte*)"typetype",
    (byte*)"range",
    (byte*)"recordtype",
    (byte*)"cvoid",
    (byte*)"i8",
    (byte*)"i16",
    (byte*)"i32",
    (byte*)"i64",
    (byte*)"bit",
    (byte*)"u1",
    (byte*)"u2",
    (byte*)"u4",
    (byte*)"byte",
    (byte*)"u8",
    (byte*)"u16",
    (byte*)"u32",
    (byte*)"u64",
    (byte*)"r32",
    (byte*)"r64",
    (byte*)"int8",
    (byte*)"int16",
    (byte*)"int32",
    (byte*)"int64",
    (byte*)"word8",
    (byte*)"word16",
    (byte*)"word32",
    (byte*)"word64",
    (byte*)"real32",
    (byte*)"real64",
    (byte*)"stringc",
    (byte*)"stringz",
    (byte*)"cstring",
    (byte*)"ichar",
    (byte*)"million",
    (byte*)"billion",
    (byte*)"as",
    (byte*)"$lineno",
    (byte*)"$strlineno",
    (byte*)"$filename",
    (byte*)"$modulename",
    (byte*)"$function",
    (byte*)"$date",
    (byte*)"$time",
    (byte*)"$",
    (byte*)"and",
    (byte*)"or",
    (byte*)"iand",
    (byte*)"ior",
    (byte*)"ixor",
    (byte*)"in",
    (byte*)"inx",
    (byte*)"rem",
    (byte*)"divrem",
    (byte*)"min",
    (byte*)"max",
    (byte*)"not",
    (byte*)"istrue",
    (byte*)"inot",
    (byte*)"abs",
    (byte*)"asc",
    (byte*)"chr",
    (byte*)"sqrt",
    (byte*)"sqr",
    (byte*)"cos",
    (byte*)"sin",
    (byte*)"tan",
    (byte*)"asin",
    (byte*)"acos",
    (byte*)"atan",
    (byte*)"atan2",
    (byte*)"sign",
    (byte*)"log",
    (byte*)"log10",
    (byte*)"exp",
    (byte*)"round",
    (byte*)"floor",
    (byte*)"ceil",
    (byte*)"fract",
    (byte*)"fmod",
    (byte*)"append",
    (byte*)"concat",
    (byte*)"len",
    (byte*)"lwb",
    (byte*)"upb",
    (byte*)"bounds",
    (byte*)"bytes",
    (byte*)"isfound",
    (byte*)"dictitems",
    (byte*)"basetype",
    (byte*)"elemtype",
    (byte*)"isvoid",
    (byte*)"isdef",
    (byte*)"defined",
    (byte*)"isint",
    (byte*)"isreal",
    (byte*)"islist",
    (byte*)"isstring",
    (byte*)"isrange",
    (byte*)"ispointer",
    (byte*)"isarray",
    (byte*)"isrecord",
    (byte*)"isset",
    (byte*)"isnumber",
    (byte*)"fi",
    (byte*)"esac",
    (byte*)"od",
    (byte*)"nil",
    (byte*)"con",
    (byte*)"pi",
    (byte*)"true",
    (byte*)"false",
    (byte*)"$neg",
    (byte*)"$$dummy"
};
static byte qq_tables$stsymbols[198] = {
    (u8)72u,
    (u8)73u,
    (u8)74u,
    (u8)75u,
    (u8)76u,
    (u8)77u,
    (u8)81u,
    (u8)81u,
    (u8)82u,
    (u8)83u,
    (u8)84u,
    (u8)84u,
    (u8)85u,
    (u8)85u,
    (u8)86u,
    (u8)87u,
    (u8)79u,
    (u8)88u,
    (u8)89u,
    (u8)90u,
    (u8)90u,
    (u8)91u,
    (u8)92u,
    (u8)93u,
    (u8)93u,
    (u8)93u,
    (u8)94u,
    (u8)95u,
    (u8)95u,
    (u8)124u,
    (u8)124u,
    (u8)126u,
    (u8)125u,
    (u8)125u,
    (u8)123u,
    (u8)96u,
    (u8)96u,
    (u8)96u,
    (u8)96u,
    (u8)97u,
    (u8)97u,
    (u8)96u,
    (u8)96u,
    (u8)98u,
    (u8)98u,
    (u8)120u,
    (u8)99u,
    (u8)99u,
    (u8)100u,
    (u8)100u,
    (u8)100u,
    (u8)100u,
    (u8)108u,
    (u8)102u,
    (u8)103u,
    (u8)104u,
    (u8)109u,
    (u8)110u,
    (u8)111u,
    (u8)115u,
    (u8)116u,
    (u8)113u,
    (u8)105u,
    (u8)106u,
    (u8)107u,
    (u8)131u,
    (u8)80u,
    (u8)117u,
    (u8)118u,
    (u8)119u,
    (u8)114u,
    (u8)114u,
    (u8)127u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)70u,
    (u8)68u,
    (u8)68u,
    (u8)68u,
    (u8)121u,
    (u8)121u,
    (u8)121u,
    (u8)121u,
    (u8)121u,
    (u8)121u,
    (u8)121u,
    (u8)122u,
    (u8)29u,
    (u8)30u,
    (u8)31u,
    (u8)32u,
    (u8)33u,
    (u8)39u,
    (u8)40u,
    (u8)27u,
    (u8)28u,
    (u8)36u,
    (u8)36u,
    (u8)49u,
    (u8)51u,
    (u8)50u,
    (u8)52u,
    (u8)53u,
    (u8)54u,
    (u8)55u,
    (u8)55u,
    (u8)55u,
    (u8)55u,
    (u8)55u,
    (u8)55u,
    (u8)55u,
    (u8)55u,
    (u8)56u,
    (u8)55u,
    (u8)55u,
    (u8)55u,
    (u8)55u,
    (u8)55u,
    (u8)55u,
    (u8)55u,
    (u8)55u,
    (u8)56u,
    (u8)37u,
    (u8)38u,
    (u8)57u,
    (u8)57u,
    (u8)57u,
    (u8)57u,
    (u8)57u,
    (u8)57u,
    (u8)57u,
    (u8)59u,
    (u8)59u,
    (u8)59u,
    (u8)59u,
    (u8)59u,
    (u8)58u,
    (u8)58u,
    (u8)58u,
    (u8)58u,
    (u8)58u,
    (u8)58u,
    (u8)58u,
    (u8)58u,
    (u8)58u,
    (u8)58u,
    (u8)79u,
    (u8)79u,
    (u8)79u,
    (u8)130u,
    (u8)128u,
    (u8)128u,
    (u8)128u,
    (u8)128u,
    (u8)132u,
    (u8)0u
};
static byte qq_tables$stsubcodes[198] = {
    (u8)0u,
    (u8)0u,
    (u8)7u,
    (u8)0u,
    (u8)10u,
    (u8)12u,
    (u8)0u,
    (u8)1u,
    (u8)14u,
    (u8)0u,
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
    (u8)2u,
    (u8)3u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)3u,
    (u8)4u,
    (u8)6u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)1u,
    (u8)13u,
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
    (byte)'D',
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)9u,
    (u8)10u,
    (u8)11u,
    (u8)7u,
    (u8)8u,
    (u8)5u,
    (u8)6u,
    (u8)3u,
    (u8)3u,
    (u8)18u,
    (u8)4u,
    (u8)12u,
    (u8)0u,
    (u8)23u,
    (u8)24u,
    (u8)25u,
    (u8)26u,
    (u8)33u,
    (u8)33u,
    (u8)34u,
    (u8)35u,
    (u8)27u,
    (u8)27u,
    (u8)28u,
    (u8)29u,
    (u8)30u,
    (u8)31u,
    (u8)32u,
    (u8)23u,
    (u8)24u,
    (u8)25u,
    (u8)26u,
    (u8)27u,
    (u8)28u,
    (u8)29u,
    (u8)30u,
    (u8)31u,
    (u8)32u,
    (u8)36u,
    (u8)37u,
    (u8)37u,
    (u8)38u,
    (u8)2u,
    (u8)3u,
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)3u,
    (u8)4u,
    (u8)5u,
    (u8)6u,
    (u8)7u,
    (u8)0u,
    (u8)72u,
    (u8)73u,
    (u8)107u,
    (u8)108u,
    (u8)109u,
    (u8)0u,
    (u8)0u,
    (u8)105u,
    (u8)106u,
    (u8)115u,
    (u8)116u,
    (u8)74u,
    (u8)75u,
    (u8)77u,
    (u8)75u,
    (u8)79u,
    (u8)80u,
    (u8)1u,
    (u8)2u,
    (u8)4u,
    (u8)3u,
    (u8)5u,
    (u8)6u,
    (u8)7u,
    (u8)8u,
    (u8)18u,
    (u8)9u,
    (u8)10u,
    (u8)11u,
    (u8)12u,
    (u8)13u,
    (u8)14u,
    (u8)15u,
    (u8)16u,
    (u8)17u,
    (u8)118u,
    (u8)117u,
    (u8)86u,
    (u8)87u,
    (u8)88u,
    (u8)89u,
    (u8)90u,
    (u8)93u,
    (u8)92u,
    (byte)'b',
    (byte)'e',
    (byte)'v',
    (byte)'d',
    (byte)'d',
    (u8)1u,
    (u8)2u,
    (u8)10u,
    (u8)9u,
    (u8)4u,
    (u8)14u,
    (u8)11u,
    (u8)12u,
    (u8)5u,
    (u8)22u,
    (u8)72u,
    (u8)81u,
    (u8)87u,
    (u8)0u,
    (u8)3u,
    (u8)1u,
    (u8)4u,
    (u8)5u,
    (byte)'-',
    (u8)0u
};
static u8 *  qq_tables$hostfnnames[60] = {
    (byte*)"h_dummy",
    (byte*)"h_startprint",
    (byte*)"h_startprintcon",
    (byte*)"h_strstartprint",
    (byte*)"h_setformat",
    (byte*)"h_endprint",
    (byte*)"h_strendprint",
    (byte*)"h_print",
    (byte*)"h_print_nf",
    (byte*)"h_println",
    (byte*)"h_printnogap",
    (byte*)"h_printspace",
    (byte*)"h_readln",
    (byte*)"h_sreadln",
    (byte*)"h_sread",
    (byte*)"h_rereadln",
    (byte*)"h_reread",
    (byte*)"h_strtoval",
    (byte*)"h_tostr",
    (byte*)"h_leftstr",
    (byte*)"h_rightstr",
    (byte*)"h_convlc",
    (byte*)"h_convuc",
    (byte*)"h_waitkey",
    (byte*)"h_testkey",
    (byte*)"h_execwait",
    (byte*)"h_execcmd",
    (byte*)"h_system",
    (byte*)"h_makestr",
    (byte*)"h_makeref",
    (byte*)"h_new",
    (byte*)"h_getcmdparam",
    (byte*)"h_gethostname",
    (byte*)"h_getprogname",
    (byte*)"h_$setdebug",
    (byte*)"h_$test2",
    (byte*)"h_$test",
    (byte*)"h_$refcount",
    (byte*)"h_ticks",
    (byte*)"h_clock",
    (byte*)"h_sleep",
    (byte*)"h_random",
    (byte*)"h_gethash",
    (byte*)"h_getos",
    (byte*)"h_iswindows",
    (byte*)"h_setmesshandler",
    (byte*)"h_$getparam",
    (byte*)"h_makeempty",
    (byte*)"h_$smallmemtotal",
    (byte*)"h_$id",
    (byte*)"h_copy",
    (byte*)"h_$nan",
    (byte*)"h_$infinity",
    (byte*)"h_$nprocs",
    (byte*)"h_$procname",
    (byte*)"h_$procref",
    (byte*)"h_allocexec",
    (byte*)"h_runnative",
    (byte*)"h_setlwb",
    (byte*)"h_last"
};
static byte qq_tables$hostnparams[60] = {
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)2u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)2u,
    (u8)2u,
    (u8)3u,
    (u8)3u,
    (u8)2u,
    (u8)2u,
    (u8)0u,
    (u8)0u,
    (u8)3u,
    (u8)3u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)4u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)2u,
    (u8)3u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)0u,
    (u8)0u,
    (u8)0u,
    (u8)1u,
    (u8)1u,
    (u8)1u,
    (u8)2u,
    (u8)2u,
    (u8)0u
};
static byte qq_tables$hostisfn[60] = {
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
    (u8)1u,
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
    (u8)0u,
    (u8)0u
};
static byte qq_tables$hostinternal[60] = {
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
    (u8)1u
};
static void (*qq_tables$hosthandlers[60])(void) = {
    0,
    (void (*)(void))qq_print$pch_startprint,
    (void (*)(void))qq_print$pch_startprintcon,
    (void (*)(void))qq_print$pch_strstartprint,
    (void (*)(void))qq_print$pch_setformat,
    (void (*)(void))qq_print$pch_endprint,
    (void (*)(void))qq_print$pch_strendprint,
    (void (*)(void))qq_print$pch_print,
    (void (*)(void))qq_print$pch_print_nf,
    (void (*)(void))qq_print$pch_println,
    (void (*)(void))qq_print$pch_printnogap,
    (void (*)(void))qq_print$pch_printspace,
    (void (*)(void))qq_print$pch_readln,
    (void (*)(void))qq_print$pch_sreadln,
    (void (*)(void))qq_print$pch_sread,
    (void (*)(void))qq_print$pch_rereadln,
    (void (*)(void))qq_print$pch_reread,
    (void (*)(void))qq_print$pch_strtoval,
    (void (*)(void))qq_print$pch_tostr,
    (void (*)(void))qq_host$pch_leftstr,
    (void (*)(void))qq_host$pch_rightstr,
    (void (*)(void))qq_host$pch_convlc,
    (void (*)(void))qq_host$pch_convuc,
    (void (*)(void))qq_host$pch_waitkey,
    (void (*)(void))qq_host$pch_testkey,
    (void (*)(void))qq_host$pch_execwait,
    (void (*)(void))qq_host$pch_execcmd,
    (void (*)(void))qq_host$pch_system,
    (void (*)(void))qq_host$pch_makestr,
    (void (*)(void))qq_host$pch_makeref,
    (void (*)(void))qq_host$pch_new,
    (void (*)(void))qq_host$pch_getcmdparam,
    (void (*)(void))qq_host$pch_gethostname,
    (void (*)(void))qq_host$pch_getprogname,
    (void (*)(void))qq_host$pch_$setdebug,
    (void (*)(void))qq_host$pch_$test2,
    (void (*)(void))qq_host$pch_$test,
    (void (*)(void))qq_host$pch_$refcount,
    (void (*)(void))qq_host$pch_ticks,
    (void (*)(void))qq_host$pch_clock,
    (void (*)(void))qq_host$pch_sleep,
    (void (*)(void))qq_host$pch_random,
    (void (*)(void))qq_host$pch_gethash,
    (void (*)(void))qq_host$pch_getos,
    (void (*)(void))qq_host$pch_iswindows,
    (void (*)(void))qq_host$pch_setmesshandler,
    (void (*)(void))qq_host$pch_$getparam,
    (void (*)(void))qq_host$pch_makeempty,
    (void (*)(void))qq_host$pch_$smallmemtotal,
    (void (*)(void))qq_host$pch_$id,
    (void (*)(void))qq_host$pch_copy,
    (void (*)(void))qq_host$pch_$nan,
    (void (*)(void))qq_host$pch_$infinity,
    (void (*)(void))qq_host$pch_$nprocs,
    (void (*)(void))qq_host$pch_$procname,
    (void (*)(void))qq_host$pch_$procref,
    (void (*)(void))qq_host$pch_allocexec,
    (void (*)(void))qq_host$pch_runnative,
    (void (*)(void))qq_host$pch_setlwb,
    0
};
static byte qq_tables$d_binopset[27] = {
    (u8)29u,
    (u8)30u,
    (u8)43u,
    (u8)44u,
    (u8)45u,
    (u8)46u,
    (u8)48u,
    (u8)47u,
    (u8)22u,
    (u8)23u,
    (u8)24u,
    (u8)25u,
    (u8)26u,
    (u8)27u,
    (u8)31u,
    (u8)32u,
    (u8)33u,
    (u8)34u,
    (u8)35u,
    (u8)36u,
    (u8)38u,
    (u8)41u,
    (u8)28u,
    (u8)56u,
    (u8)37u,
    (u8)19u,
    (u8)42u
};
static byte qq_tables$binopset[133];
static byte qq_tables$d_unaryopset[7] = {(u8)49u,(u8)50u,(u8)52u,(u8)51u,(u8)53u,(u8)54u,(u8)55u};
static byte qq_tables$unaryopset[133];
static byte qq_tables$d_addopset[10] = {(u8)22u,(u8)23u,(u8)31u,(u8)32u,(u8)33u,(u8)38u,(u8)37u,(u8)36u,(u8)19u,(u8)42u};
static byte qq_tables$d_cmpopset[6] = {(u8)43u,(u8)44u,(u8)45u,(u8)46u,(u8)47u,(u8)48u};
static byte qq_tables$d_mulopset[7] = {(u8)24u,(u8)25u,(u8)26u,(u8)27u,(u8)34u,(u8)35u,(u8)28u};
static byte qq_tables$addopset[133];
static byte qq_tables$cmpopset[133];
static byte qq_tables$mulopset[133];
static byte qq_tables$exprendset[133];
static i64 qq_tables$d_exprstarterset[26] = {
    (i64)9,
    (i64)11,
    (i64)15,
    (i64)19,
    (i64)69,
    (i64)60,
    (i64)63,
    (i64)64,
    (i64)65,
    (i64)66,
    (i64)67,
    (i64)70,
    (i64)125,
    (i64)13,
    (i64)97,
    (i64)122,
    (i64)121,
    (i64)126,
    (i64)109,
    (i64)120,
    (i64)21,
    (i64)130,
    (i64)129,
    (i64)72,
    (i64)102,
    (i64)103
};
static byte qq_tables$exprstarterset[132];
static u8 *  qq_tables$ttname[251];
static struct qq_decls$strec *  qq_tables$ttnamedef[251];
static i16 qq_tables$ttbasetype[251];
static i16 qq_tables$tttarget[251];
static i64 qq_tables$ttlower[251];
static i64 qq_tables$ttlength[251];
static struct qq_decls$unitrec *  qq_tables$ttlowerexpr[251];
static struct qq_decls$unitrec *  qq_tables$ttlengthexpr[251];
static i64 qq_tables$ttsize[251];
static byte qq_tables$ttbitwidth[251];
static struct qq_decls$strec *  qq_tables$ttfields[251];
static byte qq_tables$ttcaligned[251];
static struct qq_decls$strec *  qq_tables$ttowner[251];
static i64 qq_tables$ntypes;
static i64 qq_tables$firstusertype;
static i64 qq_tables$nuserxtypes;
static i64 qq_tables$userxtypebase;
static struct qq_decls$userxrec *  qq_tables$userxmodelist;
static struct qq_decls$strec *  qq_tables$ttnamedefx[5000];
static i64 qq_tables$ttxmap[5000];
static byte qq_tables$ttxmoduleno[5000];
static byte qq_tables$hostlvset[60];
static u8 *  qq_tables$cvnames[7] = {(byte*)"cv_lineno",(byte*)"cv_strlineno",(byte*)"cv_filename",(byte*)"cv_modulename",(byte*)"cv_function",(byte*)"cv_date",(byte*)"cv_time"};
static u8 *  qq_tables$loopnames[3] = {(byte*)"loop_redo",(byte*)"loop_next",(byte*)"loop_exit"};
static u8 *  qq_tables$mathsnames[18] = {
    (byte*)"mm_sqrt",
    (byte*)"mm_sqr",
    (byte*)"mm_sin",
    (byte*)"mm_cos",
    (byte*)"mm_tan",
    (byte*)"mm_asin",
    (byte*)"mm_acos",
    (byte*)"mm_atan",
    (byte*)"mm_sign",
    (byte*)"mm_log",
    (byte*)"mm_log10",
    (byte*)"mm_exp",
    (byte*)"mm_round",
    (byte*)"mm_floor",
    (byte*)"mm_ceil",
    (byte*)"mm_fract",
    (byte*)"mm_fmod",
    (byte*)"mm_atan2"
};
static u8 *  qq_tables$condnames[6] = {(byte*)"eq",(byte*)"ne",(byte*)"lt",(byte*)"le",(byte*)"ge",(byte*)"gt"};
static byte qq_tables$revconds[6] = {(u8)1u,(u8)0u,(u8)4u,(u8)5u,(u8)2u,(u8)3u};
static i64 (*qq_show$labelmap)[];
static i64 qq_show$currlineno;
static struct qq_decls$strec *  qq_show$currpclproc;
static struct mlib$strbuffer qq_show$pclv;
static struct mlib$strbuffer *  qq_show$pcldest = (struct mlib$strbuffer *)&qq_show$pclv;
static i64 qq_showpcl$currlineno;
static struct qq_decls$strec *  qq_showpcl$currpclproc;
static struct qq_decls$objrec qq_vars$zeroobj;
static void *  msysc$_fnaddresses[]= {
    &main,
    &qq_cli$getinputoptions,
    &qq_cli$do_option,
    &qq_cli$compile_sp,
    &qq_cli$setcli,
    &qq_cli$writeqafile,
    &qq_cli$initdata,
    &qq_cli$loadsyslib,
    &qq_cli$resetcompiler,
    &qq_cli$setcmdparam,
    &qq_cli$fixup_sp,
    &qq_cli$fixproc,
    &qq_cli$fixupmodule,
    &qq_cli$optimise_module,
    &qq_cli$optim,
    &qq_cli$start,
    &qq_arrays$var_empty_array,
    &qq_arrays$obj_free_array,
    &qq_arrays$obj_free_vector,
    &qq_arrays$var_make_array,
    &qq_arrays$obj_newarray,
    &qq_arrays$obj_newarray_u,
    &qq_arrays$var_getix_array,
    &qq_arrays$var_putix_array,
    &qq_arrays$var_getixref_array,
    &qq_arrays$obj_append_array,
    &qq_arrays$var_appendto_array,
    &qq_arrays$obj_resize_array,
    &qq_arrays$var_dupl_array,
    &qq_arrays$var_dupl_vector,
    &qq_arrays$var_equal_array,
    &qq_arrays$var_concatto_array,
    &qq_arrays$var_getslice_array,
    &qq_arrays$u8inarray,
    &qq_arrays$u16inarray,
    &qq_arrays$u32inarray,
    &qq_arrays$u64inarray,
    &qq_arrays$var_inx_array,
    &qq_arrays$var_expand_array,
    &qq_arrays$start,
    &qq_bits$obj_free_bits,
    &qq_bits$var_make_bits,
    &qq_bits$obj_newbits,
    &qq_bits$var_getix_bits,
    &qq_bits$var_putix_bits,
    &qq_bits$var_getixref_bits,
    &qq_bits$getindexoffset,
    &qq_bits$obj_append_bits,
    &qq_bits$var_appendto_bits,
    &qq_bits$obj_resize_bits,
    &qq_bits$var_dupl_bits,
    &qq_bits$var_equal_bits,
    &qq_bits$var_concatto_bits,
    &qq_bits$var_getslice_bits,
    &qq_bits$bits_bytesize,
    &qq_bits$getbitssize,
    &qq_bits$start,
    &qq_calldll$calldll,
    &qq_calldll$getlibprocaddr,
    &qq_calldll$vartopacked,
    &qq_calldll$packedtovar,
    &qq_calldll$loaddllfunction,
    &qq_calldll$start,
    &qq_decls$start,
    &qq_decimal$obj_free_dec,
    &qq_decimal$var_dupl_dec,
    &qq_decimal$var_empty_dec,
    &qq_decimal$var_make_dec_str,
    &qq_decimal$var_make_dec_int,
    &qq_decimal$badnumber,
    &qq_decimal$bn_makestr,
    &qq_decimal$readexpon,
    &qq_decimal$bn_makeint,
    &qq_decimal$var_tostr_dec,
    &qq_decimal$obj_tostr_dec,
    &qq_decimal$tostring_scient,
    &qq_decimal$tostring_float,
    &qq_decimal$strvaln,
    &qq_decimal$bn_isint,
    &qq_decimal$obj_len_dec,
    &qq_decimal$bn_iszero,
    &qq_decimal$var_equal_dec,
    &qq_decimal$var_add_dec,
    &qq_decimal$var_sub_dec,
    &qq_decimal$var_mul_dec,
    &qq_decimal$var_div_dec,
    &qq_decimal$var_idiv_dec,
    &qq_decimal$var_irem_dec,
    &qq_decimal$var_neg_dec,
    &qq_decimal$var_abs_dec,
    &qq_decimal$var_compare_dec,
    &qq_decimal$bn_cmp,
    &qq_decimal$bn_equal,
    &qq_decimal$bn_add,
    &qq_decimal$bn_sub,
    &qq_decimal$bn_addu,
    &qq_decimal$bn_subu,
    &qq_decimal$makebignum,
    &qq_decimal$makesmallnum,
    &qq_decimal$smalltobig,
    &qq_decimal$freesmall,
    &qq_decimal$bn_init,
    &qq_decimal$bn_setzero,
    &qq_decimal$bn_move,
    &qq_decimal$bn_dupl,
    &qq_decimal$bn_setinf,
    &qq_decimal$bn_setnan,
    &qq_decimal$var_setnan,
    &qq_decimal$var_setinf,
    &qq_decimal$getbintype,
    &qq_decimal$bn_negto,
    &qq_decimal$bn_absto,
    &qq_decimal$bn_mul,
    &qq_decimal$bn_mulp,
    &qq_decimal$bn_mulu,
    &qq_decimal$smallmulto,
    &qq_decimal$bn_div,
    &qq_decimal$bn_idiv,
    &qq_decimal$bn_idivrem,
    &qq_decimal$bn_irem,
    &qq_decimal$bn_idivu,
    &qq_decimal$bn_fdivu,
    &qq_decimal$smalldiv,
    &qq_decimal$smallsubto,
    &qq_decimal$bn_getprec,
    &qq_decimal$bn_setprec,
    &qq_decimal$bn_getglobalprec,
    &qq_decimal$bn_setglobalprec,
    &qq_decimal$bn_makefloat,
    &qq_decimal$dectemp,
    &qq_decimal$freedectemp,
    &qq_decimal$bn_ipower,
    &qq_decimal$var_power_dec,
    &qq_decimal$var_convert_dec_int,
    &qq_decimal$bn_toint,
    &qq_decimal$start,
    &qq_dicts$var_make_dict,
    &qq_dicts$obj_new_dict,
    &qq_dicts$obj_free_dict,
    &qq_dicts$var_dupl_dict,
    &qq_dicts$var_equal_dict,
    &qq_dicts$var_finddictitem,
    &qq_dicts$expanddict,
    &qq_dicts$adddictitem,
    &qq_dicts$start,
    &qq_host$callhostfunction,
    &qq_host$pch_leftstr,
    &qq_host$pch_rightstr,
    &qq_host$pch_convlc,
    &qq_host$pch_convuc,
    &qq_host$pch_waitkey,
    &qq_host$pch_execwait,
    &qq_host$pch_execcmd,
    &qq_host$pch_makestr,
    &qq_host$pch_makeref,
    &qq_host$pch_getcmdparam,
    &qq_host$pch_clock,
    &qq_host$pch_allocexec,
    &qq_host$pch_runnative,
    &qq_host$pch_setlwb,
    &qq_host$pch_ticks,
    &qq_host$pch_sleep,
    &qq_host$pch_random,
    &qq_host$pch_system,
    &qq_host$pch_$getparam,
    &qq_host$checkparam,
    &qq_host$leftstring,
    &qq_host$rightstring,
    &qq_host$padstring_right,
    &qq_host$padstring_left,
    &qq_host$getbounds,
    &qq_host$pch_new,
    &qq_host$pch_gethostname,
    &qq_host$pch_getprogname,
    &qq_host$pch_$test,
    &qq_host$pch_$test2,
    &qq_host$pch_$refcount,
    &qq_host$pch_testkey,
    &qq_host$pch_getos,
    &qq_host$pch_setmesshandler,
    &qq_host$pch_$smallmemtotal,
    &qq_host$pch_$id,
    &qq_host$pch_iswindows,
    &qq_host$pch_$setdebug,
    &qq_host$pch_copy,
    &qq_host$pch_gethash,
    &qq_host$pch_makeempty,
    &qq_host$pch_$infinity,
    &qq_host$pch_$nan,
    &qq_host$setcmdparam,
    &qq_host$pch_$nprocs,
    &qq_host$initprocrefs,
    &qq_host$pch_$procname,
    &qq_host$pch_$procref,
    &qq_host$start,
    &qq_lex$lexreadtoken,
    &qq_lex$lxreadstring,
    &qq_lex$readhexcode,
    &qq_lex$getutf8,
    &qq_lex$lexinit,
    &qq_lex$readrawstring,
    &qq_lex$lookup,
    &qq_lex$gethashvaluez,
    &qq_lex$start,
    &qq_lex$inithashtable,
    &qq_lex$addstname,
    &qq_lex$startlex,
    &qq_lex$addnamestr,
    &qq_lex$ps,
    &qq_lex$psnext,
    &qq_lex$lex,
    &qq_lex$lxerror_s,
    &qq_lex$makedecimal,
    &qq_lex$readdec,
    &qq_lex$readhex,
    &qq_lex$readbin,
    &qq_lex$readreal,
    &qq_lex$readrawxname,
    &qq_lib$reportcterror,
    &qq_lib$geterrorinfo,
    &qq_lib$showerrorsource,
    &qq_lib$stopcompiler,
    &qq_lib$gerror,
    &qq_lib$gerror_s,
    &qq_lib$serror,
    &qq_lib$serror_s,
    &qq_lib$rxerror,
    &qq_lib$rxerror_s,
    &qq_lib$lxerror,
    &qq_lib$loaderror,
    &qq_lib$prterror,
    &qq_lib$allocunitrec,
    &qq_lib$createintunit,
    &qq_lib$createrealunit,
    &qq_lib$createstringunit,
    &qq_lib$createunit0,
    &qq_lib$createunit1,
    &qq_lib$createunit2,
    &qq_lib$createname,
    &qq_lib$addlistunit,
    &qq_lib$createavname,
    &qq_lib$convtostringz,
    &qq_lib$findprocname,
    &qq_lib$strexpr,
    &qq_lib$strexpr_s,
    &qq_lib$jeval,
    &qq_lib$jevallist,
    &qq_lib$additem,
    &qq_lib$isalphanum,
    &qq_lib$getopcname,
    &qq_lib$convertstring,
    &qq_lib$createavnamex,
    &qq_lib$storemode,
    &qq_lib$nextpoweroftwo,
    &qq_lib$testelem,
    &qq_lib$setelem,
    &qq_lib$setelemblock,
    &qq_lib$ispoweroftwo,
    &qq_lib$deleteunit,
    &qq_lib$skipsemi,
    &qq_lib$checksymbol,
    &qq_lib$skipsymbol,
    &qq_lib$pcnotmut,
    &qq_lib$start,
    &qq_lists$start,
    &qq_lists$var_empty_list,
    &qq_lists$var_make_list,
    &qq_lists$obj_newlist,
    &qq_lists$obj_free_list,
    &qq_lists$var_getix_list,
    &qq_lists$var_getslice_list,
    &qq_lists$var_getixref_list,
    &qq_lists$var_putix_list,
    &qq_lists$var_putslice_list,
    &qq_lists$obj_append_list,
    &qq_lists$obj_resize_list,
    &qq_lists$var_appendto_list,
    &qq_lists$var_dupl_list,
    &qq_lists$var_mul_list,
    &qq_lists$var_equal_list,
    &qq_lists$var_concatto_list,
    &qq_lists$var_inx_list,
    &qq_modules$loadsp,
    &qq_modules$getmodulefilename,
    &qq_modules$loadsourcefile,
    &qq_modules$loadstring,
    &qq_modules$readfileline,
    &qq_modules$findnextlineheader,
    &qq_modules$loadqafile,
    &qq_modules$readqabundle,
    &qq_modules$start,
    &qq_names$addglobalname,
    &qq_names$newstrec,
    &qq_names$addsymbol,
    &qq_names$addproc,
    &qq_names$newusertypex,
    &qq_names$resolvedottedname,
    &qq_names$addgenfield,
    &qq_names$makereftype,
    &qq_names$makeaxtype,
    &qq_names$makestrtype,
    &qq_names$addanontype,
    &qq_names$createusertype,
    &qq_names$getalignment,
    &qq_names$duplfield,
    &qq_names$writesig,
    &qq_names$createdupldef,
    &qq_names$start,
    &qq_packed$var_loadpacked,
    &qq_packed$var_storepacked,
    &qq_packed$setfslength,
    &qq_packed$getfslength,
    &qq_packed$var_make_struct,
    &qq_packed$obj_new_struct,
    &qq_packed$var_dupl_struct,
    &qq_packed$obj_free_struct,
    &qq_packed$var_equal_struct,
    &qq_packed$var_getix_struct,
    &qq_packed$start,
    &qq_parse$parsemodule,
    &qq_parse$readexpression,
    &qq_parse$readassignment,
    &qq_parse$readorterms,
    &qq_parse$readandterms,
    &qq_parse$readcmpterms,
    &qq_parse$readinterms,
    &qq_parse$readrangeterm,
    &qq_parse$readaddterms,
    &qq_parse$readmulterms,
    &qq_parse$readpowerterms,
    &qq_parse$readterm2,
    &qq_parse$readtermsuffix,
    &qq_parse$readterm,
    &qq_parse$readsunit,
    &qq_parse$checkequals,
    &qq_parse$readindex,
    &qq_parse$readdotsuffix,
    &qq_parse$readslist,
    &qq_parse$readcondsuffix,
    &qq_parse$readkeyindex,
    &qq_parse$readlbrack,
    &qq_parse$readif,
    &qq_parse$checkend,
    &qq_parse$readunless,
    &qq_parse$readwhile,
    &qq_parse$readrepeat,
    &qq_parse$readfor,
    &qq_parse$readdo,
    &qq_parse$readto,
    &qq_parse$makeblock,
    &qq_parse$readvardef,
    &qq_parse$readconstdef,
    &qq_parse$readreturn,
    &qq_parse$readprint,
    &qq_parse$readread,
    &qq_parse$readloopcontrol,
    &qq_parse$readintunit,
    &qq_parse$readswitchcase,
    &qq_parse$readgoto,
    &qq_parse$readstop,
    &qq_parse$readcast,
    &qq_parse$readset,
    &qq_parse$readtabledef,
    &qq_parse$readtry,
    &qq_parse$readsprint,
    &qq_parse$readsread,
    &qq_parse$readimportdll,
    &qq_parse$readffiparams,
    &qq_parse$readtypeparams,
    &qq_parse$readtypenameparams,
    &qq_parse$readrecorddef,
    &qq_parse$readrecordbody,
    &qq_parse$readrecordfields,
    &qq_parse$readstructbody,
    &qq_parse$addstructflag,
    &qq_parse$readprocdef,
    &qq_parse$readatfield,
    &qq_parse$istypestarter,
    &qq_parse$readmacrodef,
    &qq_parse$readhostparams,
    &qq_parse$pushlisttype,
    &qq_parse$poplisttype,
    &qq_parse$readcompilervar,
    &qq_parse$readpair,
    &qq_parse$lexchecksymbol,
    &qq_parse$readtypedef,
    &qq_parse$readtypespec,
    &qq_parse$readparams,
    &qq_parse$checkoperator,
    &qq_parse$readlambda,
    &qq_parse$readpackvars,
    &qq_parse$start,
    &qq_pcltabs$start,
    &qq_pclgen$evalunit,
    &qq_pclgen$gencodemodule,
    &qq_pclgen$do_procdef,
    &qq_pclgen$genprocentry,
    &qq_pclgen$genprocexit,
    &qq_pclgen$evalref,
    &qq_pclgen$genjumpcond,
    &qq_pclgen$gcomparejump,
    &qq_pclgen$genjumpl,
    &qq_pclgen$stacklooplabels,
    &qq_pclgen$unstacklooplabels,
    &qq_pclgen$findlooplabel,
    &qq_pclgen$do_assign,
    &qq_pclgen$do_bin,
    &qq_pclgen$do_binref,
    &qq_pclgen$do_unary,
    &qq_pclgen$do_unaryref,
    &qq_pclgen$do_pushlist,
    &qq_pclgen$do_makedict,
    &qq_pclgen$do_call,
    &qq_pclgen$pushparams,
    &qq_pclgen$evalparam,
    &qq_pclgen$pushkwdparams,
    &qq_pclgen$do_if,
    &qq_pclgen$do_do,
    &qq_pclgen$do_loop,
    &qq_pclgen$do_to,
    &qq_pclgen$do_while,
    &qq_pclgen$do_repeat,
    &qq_pclgen$do_for,
    &qq_pclgen$do_forx,
    &qq_pclgen$do_print,
    &qq_pclgen$do_fprint,
    &qq_pclgen$do_read,
    &qq_pclgen$do_forall,
    &qq_pclgen$do_case,
    &qq_pclgen$do_case_nc,
    &qq_pclgen$do_try,
    &qq_pclgen$unitstoarray,
    &qq_pclgen$do_select,
    &qq_pclgen$do_andl,
    &qq_pclgen$do_orl,
    &qq_pclgen$do_incr,
    &qq_pclgen$do_callhost,
    &qq_pclgen$callhostfn,
    &qq_pclgen$genfree,
    &qq_pclgen$do_return,
    &qq_pclgen$do_multassign,
    &qq_pclgen$do_store,
    &qq_pclgen$getconstvalue,
    &qq_pclgen$do_convert,
    &qq_pclgen$checkelems,
    &qq_pclgen$do_switch,
    &qq_pclgen$do_simpleswitch,
    &qq_pclgen$do_makerecordkv,
    &qq_pclgen$do_idiv,
    &qq_pclgen$do_irem,
    &qq_pclgen$do_map,
    &qq_pclgen$pushstring,
    &qq_pclgen$checkblockreturn,
    &qq_pclgen$start,
    &qq_pcllib$start,
    &qq_pcllib$resetpcl,
    &qq_pcllib$genpc,
    &qq_pcllib$genpc_int,
    &qq_pcllib$genpc_n,
    &qq_pcllib$genpc_xy,
    &qq_pcllib$genpc_name,
    &qq_pcllib$genopnd_strz,
    &qq_pcllib$genopnd_str,
    &qq_pcllib$genopnd_obj,
    &qq_pcllib$genpc_real,
    &qq_pcllib$genpc_lab,
    &qq_pcllib$gencomment,
    &qq_pcllib$extendpcldata,
    &qq_pcllib$extendlabeltable,
    &qq_pcllib$definelabel,
    &qq_pcllib$createfwdlabel,
    &qq_pcllib$definefwdlabel,
    &qq_pcllib$genxy,
    &qq_print$pch_print,
    &qq_print$pch_print_nf,
    &qq_print$pch_printnogap,
    &qq_print$pch_println,
    &qq_print$pch_reread,
    &qq_print$pch_rereadln,
    &qq_print$pch_startprint,
    &qq_print$pch_startprintcon,
    &qq_print$pch_endprint,
    &qq_print$pch_strstartprint,
    &qq_print$pch_strendprint,
    &qq_print$pch_printspace,
    &qq_print$pch_readln,
    &qq_print$pch_sread,
    &qq_print$pch_sreadln,
    &qq_print$readname,
    &qq_print$readstring,
    &qq_print$readint,
    &qq_print$readhex,
    &qq_print$readbin,
    &qq_print$readreal,
    &qq_print$getreadfmtcode,
    &qq_print$stepkbpos,
    &qq_print$readany,
    &qq_print$readitem,
    &qq_print$strtoreal,
    &qq_print$strtoint,
    &qq_print$printnextfmtchars,
    &qq_print$pch_setformat,
    &qq_print$pc_getfmt,
    &qq_print$addstring,
    &qq_print$domultichar,
    &qq_print$printstr_n,
    &qq_print$pch_strtoval,
    &qq_print$tostr_int,
    &qq_print$tostr_real,
    &qq_print$tostr_str,
    &qq_print$pch_tostr,
    &qq_print$tostr_range,
    &qq_print$tostr_array,
    &qq_print$tostr_bits,
    &qq_print$tostr_struct,
    &qq_print$tostr_set,
    &qq_print$tostr_dict,
    &qq_print$tostr_decimal,
    &qq_print$tostr,
    &qq_print$tostr_list,
    &qq_print$start,
    &qq_records$var_make_record,
    &qq_records$obj_new_record,
    &qq_records$obj_free_record,
    &qq_records$var_dupl_record,
    &qq_records$var_equal_record,
    &qq_records$var_getix_record,
    &qq_records$var_putix_record,
    &qq_records$var_getixref_record,
    &qq_records$start,
    &qq_resolve$rx_module,
    &qq_resolve$rx_passdef,
    &qq_resolve$rx_deflist,
    &qq_resolve$rx_unit,
    &qq_resolve$rx_unitlist,
    &qq_resolve$evalmonop,
    &qq_resolve$evalbinop,
    &qq_resolve$makeintconst,
    &qq_resolve$makerealconst,
    &qq_resolve$resolvename,
    &qq_resolve$resolvetopname,
    &qq_resolve$resolvedot,
    &qq_resolve$resolvedot_sym,
    &qq_resolve$finddupl,
    &qq_resolve$expandmacro,
    &qq_resolve$copylistunit,
    &qq_resolve$copyunit,
    &qq_resolve$replaceunit,
    &qq_resolve$fixmode,
    &qq_resolve$fixmode2,
    &qq_resolve$fixusertypes,
    &qq_resolve$tx_typetable,
    &qq_resolve$getconstint,
    &qq_resolve$converttype,
    &qq_resolve$scanstruct,
    &qq_resolve$dobaseclass,
    &qq_resolve$start,
    &qq_runlab$disploop,
    &qq_runlab$start,
    &qq_runlab$fixupcode,
    &qq_runlab$runqprogram,
    &qq_runaux$pcerror,
    &qq_runaux$pcustype,
    &qq_runaux$pcustype_t,
    &qq_runaux$pcmxtypes,
    &qq_runaux$pcmxtypestt,
    &qq_runaux$reportpcerror,
    &qq_runaux$getpcerrorpos,
    &qq_runaux$findmodulefrompc,
    &qq_runaux$k_makelist,
    &qq_runaux$k_len,
    &qq_runaux$k_maths,
    &qq_runaux$k_lwb,
    &qq_runaux$k_upb,
    &qq_runaux$k_swap,
    &qq_runaux$k_bounds,
    &qq_runaux$k_type,
    &qq_runaux$k_dot,
    &qq_runaux$k_dotref,
    &qq_runaux$k_popdot,
    &qq_runaux$resolvefield,
    &qq_runaux$k_convrefpack,
    &qq_runaux$k_incrptr,
    &qq_runaux$k_cmp,
    &qq_runaux$k_bytesize,
    &qq_runaux$k_when,
    &qq_runaux$raiseexception,
    &qq_runaux$runproc_m,
    &qq_runaux$runproc,
    &qq_runaux$k_keyindex,
    &qq_runaux$k_popkeyindex,
    &qq_runaux$k_keyindexref,
    &qq_runaux$k_map,
    &qq_runaux$start,
    &qq_sets$obj_free_set,
    &qq_sets$var_dupl_set,
    &qq_sets$var_equal_set,
    &qq_sets$getsetbytes,
    &qq_sets$var_make_set,
    &qq_sets$obj_newset,
    &qq_sets$var_emptyset,
    &qq_sets$var_getix_set,
    &qq_sets$var_putix_set,
    &qq_sets$var_getixref_set,
    &qq_sets$getoffset,
    &qq_sets$var_in_set,
    &qq_sets$iresizeset,
    &qq_sets$obj_resize_set,
    &qq_sets$iorsetbits,
    &qq_sets$ixorsetbits,
    &qq_sets$iandsetbits,
    &qq_sets$inotsetbits,
    &qq_sets$var_iorto_set,
    &qq_sets$var_iandto_set,
    &qq_sets$var_ixorto_set,
    &qq_sets$var_inotto_set,
    &qq_sets$start,
    &qq_strings$start,
    &qq_strings$var_empty_string,
    &qq_strings$var_make_string,
    &qq_strings$var_make_stringn,
    &qq_strings$obj_new_string,
    &qq_strings$obj_make_string,
    &qq_strings$obj_make_stringn,
    &qq_strings$obj_free_string,
    &qq_strings$var_dupl_string,
    &qq_strings$var_getix_string,
    &qq_strings$var_getixref_string,
    &qq_strings$var_getdotix_string,
    &qq_strings$var_getdotixref_string,
    &qq_strings$var_getslice_string,
    &qq_strings$stringslice,
    &qq_strings$var_putix_string,
    &qq_strings$var_putslice_string,
    &qq_strings$var_putdotix_string,
    &qq_strings$obj_resize_string,
    &qq_strings$var_add_string,
    &qq_strings$var_addto_string,
    &qq_strings$var_addto_string_ch,
    &qq_strings$var_equal_string,
    &qq_strings$var_compare_string,
    &qq_strings$cmpstring_len,
    &qq_strings$var_inx_string,
    &qq_strings$var_iconvcase,
    &qq_strings$var_makestrslicexobj,
    &qq_strings$obj_make_strslicexobj,
    &qq_strings$var_asc,
    &qq_strings$var_new_string,
    &qq_strings$var_new_stringn,
    &qq_strings$var_mul_string,
    &qq_strings$var_convert_string_list,
    &qq_strings$var_expand_string,
    &qq_strings$var_makechar,
    &qq_syslibsdummy$loadsysmodule,
    &qq_syslibsdummy$start,
    &qq_tables$start,
    &qq_show$printunit,
    &qq_show$printunitlist,
    &qq_show$getprefix,
    &qq_show$getlineinfok,
    &qq_show$printglobalsymbols,
    &qq_show$printst,
    &qq_show$printstrec,
    &qq_show$printtypetables,
    &qq_show$showsttree,
    &qq_show$showtypes,
    &qq_show$showast,
    &qq_show$showast2,
    &qq_show$showlogfile,
    &qq_show$addtolog,
    &qq_show$showstflat,
    &qq_show$showmoduleinfo,
    &qq_show$printsymbol,
    &qq_show$strmode,
    &qq_show$istrmode,
    &qq_show$deletetempfiles,
    &qq_show$start,
    &qq_showpcl$writepcl,
    &qq_showpcl$writepclopnd,
    &qq_showpcl$writeallpcl,
    &qq_showpcl$showpcl,
    &qq_showpcl$showpcl2,
    &qq_showpcl$gstr,
    &qq_showpcl$gstrln,
    &qq_showpcl$gline,
    &qq_showpcl$gstrint,
    &qq_showpcl$glabeldef,
    &qq_showpcl$start,
    &qq_vars$var_unshareu,
    &qq_vars$obj_shareu,
    &qq_vars$void_new,
    &qq_vars$obj_new,
    &qq_vars$var_getintvalue,
    &qq_vars$var_fromobj,
    &qq_vars$var_free,
    &qq_vars$var_duplu,
    &qq_vars$var_neg,
    &qq_vars$var_abs,
    &qq_vars$var_inot,
    &qq_vars$var_istruel,
    &qq_vars$var_add,
    &qq_vars$var_addmixed,
    &qq_vars$var_addto,
    &qq_vars$var_sub,
    &qq_vars$var_submixed,
    &qq_vars$var_mul,
    &qq_vars$var_mulmixed,
    &qq_vars$var_div,
    &qq_vars$var_divmixed,
    &qq_vars$var_idiv,
    &qq_vars$var_irem,
    &qq_vars$var_iand,
    &qq_vars$var_ior,
    &qq_vars$var_ixor,
    &qq_vars$var_shl,
    &qq_vars$var_shr,
    &qq_vars$var_in,
    &qq_vars$var_inx,
    &qq_vars$var_equal,
    &qq_vars$var_equalmixed,
    &qq_vars$var_compare,
    &qq_vars$var_comparemixed,
    &qq_vars$var_concat,
    &qq_vars$var_append,
    &qq_vars$var_min,
    &qq_vars$var_max,
    &qq_vars$var_concatto,
    &qq_vars$var_appendto,
    &qq_vars$var_getix,
    &qq_vars$var_putix,
    &qq_vars$var_getixref,
    &qq_vars$var_getslice,
    &qq_vars$var_putslice,
    &qq_vars$var_getdotix,
    &qq_vars$var_putdotix,
    &qq_vars$var_getdotixref,
    &qq_vars$var_getdotslice,
    &qq_vars$var_putdotslice,
    &qq_vars$var_getdotsliceref,
    &qq_vars$var_expand,
    &qq_vars$var_inplace,
    &qq_vars$var_inplace_unary,
    &qq_vars$var_loadptr,
    &qq_vars$var_storeptr,
    &qq_vars$var_loadbit,
    &qq_vars$var_storebit,
    &qq_vars$var_convert,
    &qq_vars$var_gethashvalue,
    &qq_vars$var_objtovar,
    &qq_vars$var_putdotix_intint,
    &qq_vars$var_power,
    &qq_vars$var_powermixed,
    &qq_vars$start,
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
    &mlib$pcm_allocnfz,
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
    &mlinux$dirlist,
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
    (byte*)"getinputoptions",
    (byte*)"do_option",
    (byte*)"compile_sp",
    (byte*)"setcli",
    (byte*)"writeqafile",
    (byte*)"initdata",
    (byte*)"loadsyslib",
    (byte*)"resetcompiler",
    (byte*)"setcmdparam",
    (byte*)"fixup_sp",
    (byte*)"fixproc",
    (byte*)"fixupmodule",
    (byte*)"optimise_module",
    (byte*)"optim",
    (byte*)"start",
    (byte*)"var_empty_array",
    (byte*)"obj_free_array",
    (byte*)"obj_free_vector",
    (byte*)"var_make_array",
    (byte*)"obj_newarray",
    (byte*)"obj_newarray_u",
    (byte*)"var_getix_array",
    (byte*)"var_putix_array",
    (byte*)"var_getixref_array",
    (byte*)"obj_append_array",
    (byte*)"var_appendto_array",
    (byte*)"obj_resize_array",
    (byte*)"var_dupl_array",
    (byte*)"var_dupl_vector",
    (byte*)"var_equal_array",
    (byte*)"var_concatto_array",
    (byte*)"var_getslice_array",
    (byte*)"u8inarray",
    (byte*)"u16inarray",
    (byte*)"u32inarray",
    (byte*)"u64inarray",
    (byte*)"var_inx_array",
    (byte*)"var_expand_array",
    (byte*)"start",
    (byte*)"obj_free_bits",
    (byte*)"var_make_bits",
    (byte*)"obj_newbits",
    (byte*)"var_getix_bits",
    (byte*)"var_putix_bits",
    (byte*)"var_getixref_bits",
    (byte*)"getindexoffset",
    (byte*)"obj_append_bits",
    (byte*)"var_appendto_bits",
    (byte*)"obj_resize_bits",
    (byte*)"var_dupl_bits",
    (byte*)"var_equal_bits",
    (byte*)"var_concatto_bits",
    (byte*)"var_getslice_bits",
    (byte*)"bits_bytesize",
    (byte*)"getbitssize",
    (byte*)"start",
    (byte*)"calldll",
    (byte*)"getlibprocaddr",
    (byte*)"vartopacked",
    (byte*)"packedtovar",
    (byte*)"loaddllfunction",
    (byte*)"start",
    (byte*)"start",
    (byte*)"obj_free_dec",
    (byte*)"var_dupl_dec",
    (byte*)"var_empty_dec",
    (byte*)"var_make_dec_str",
    (byte*)"var_make_dec_int",
    (byte*)"badnumber",
    (byte*)"bn_makestr",
    (byte*)"readexpon",
    (byte*)"bn_makeint",
    (byte*)"var_tostr_dec",
    (byte*)"obj_tostr_dec",
    (byte*)"tostring_scient",
    (byte*)"tostring_float",
    (byte*)"strvaln",
    (byte*)"bn_isint",
    (byte*)"obj_len_dec",
    (byte*)"bn_iszero",
    (byte*)"var_equal_dec",
    (byte*)"var_add_dec",
    (byte*)"var_sub_dec",
    (byte*)"var_mul_dec",
    (byte*)"var_div_dec",
    (byte*)"var_idiv_dec",
    (byte*)"var_irem_dec",
    (byte*)"var_neg_dec",
    (byte*)"var_abs_dec",
    (byte*)"var_compare_dec",
    (byte*)"bn_cmp",
    (byte*)"bn_equal",
    (byte*)"bn_add",
    (byte*)"bn_sub",
    (byte*)"bn_addu",
    (byte*)"bn_subu",
    (byte*)"makebignum",
    (byte*)"makesmallnum",
    (byte*)"smalltobig",
    (byte*)"freesmall",
    (byte*)"bn_init",
    (byte*)"bn_setzero",
    (byte*)"bn_move",
    (byte*)"bn_dupl",
    (byte*)"bn_setinf",
    (byte*)"bn_setnan",
    (byte*)"var_setnan",
    (byte*)"var_setinf",
    (byte*)"getbintype",
    (byte*)"bn_negto",
    (byte*)"bn_absto",
    (byte*)"bn_mul",
    (byte*)"bn_mulp",
    (byte*)"bn_mulu",
    (byte*)"smallmulto",
    (byte*)"bn_div",
    (byte*)"bn_idiv",
    (byte*)"bn_idivrem",
    (byte*)"bn_irem",
    (byte*)"bn_idivu",
    (byte*)"bn_fdivu",
    (byte*)"smalldiv",
    (byte*)"smallsubto",
    (byte*)"bn_getprec",
    (byte*)"bn_setprec",
    (byte*)"bn_getglobalprec",
    (byte*)"bn_setglobalprec",
    (byte*)"bn_makefloat",
    (byte*)"dectemp",
    (byte*)"freedectemp",
    (byte*)"bn_ipower",
    (byte*)"var_power_dec",
    (byte*)"var_convert_dec_int",
    (byte*)"bn_toint",
    (byte*)"start",
    (byte*)"var_make_dict",
    (byte*)"obj_new_dict",
    (byte*)"obj_free_dict",
    (byte*)"var_dupl_dict",
    (byte*)"var_equal_dict",
    (byte*)"var_finddictitem",
    (byte*)"expanddict",
    (byte*)"adddictitem",
    (byte*)"start",
    (byte*)"callhostfunction",
    (byte*)"pch_leftstr",
    (byte*)"pch_rightstr",
    (byte*)"pch_convlc",
    (byte*)"pch_convuc",
    (byte*)"pch_waitkey",
    (byte*)"pch_execwait",
    (byte*)"pch_execcmd",
    (byte*)"pch_makestr",
    (byte*)"pch_makeref",
    (byte*)"pch_getcmdparam",
    (byte*)"pch_clock",
    (byte*)"pch_allocexec",
    (byte*)"pch_runnative",
    (byte*)"pch_setlwb",
    (byte*)"pch_ticks",
    (byte*)"pch_sleep",
    (byte*)"pch_random",
    (byte*)"pch_system",
    (byte*)"pch_$getparam",
    (byte*)"checkparam",
    (byte*)"leftstring",
    (byte*)"rightstring",
    (byte*)"padstring_right",
    (byte*)"padstring_left",
    (byte*)"getbounds",
    (byte*)"pch_new",
    (byte*)"pch_gethostname",
    (byte*)"pch_getprogname",
    (byte*)"pch_$test",
    (byte*)"pch_$test2",
    (byte*)"pch_$refcount",
    (byte*)"pch_testkey",
    (byte*)"pch_getos",
    (byte*)"pch_setmesshandler",
    (byte*)"pch_$smallmemtotal",
    (byte*)"pch_$id",
    (byte*)"pch_iswindows",
    (byte*)"pch_$setdebug",
    (byte*)"pch_copy",
    (byte*)"pch_gethash",
    (byte*)"pch_makeempty",
    (byte*)"pch_$infinity",
    (byte*)"pch_$nan",
    (byte*)"setcmdparam",
    (byte*)"pch_$nprocs",
    (byte*)"initprocrefs",
    (byte*)"pch_$procname",
    (byte*)"pch_$procref",
    (byte*)"start",
    (byte*)"lexreadtoken",
    (byte*)"lxreadstring",
    (byte*)"readhexcode",
    (byte*)"getutf8",
    (byte*)"lexinit",
    (byte*)"readrawstring",
    (byte*)"lookup",
    (byte*)"gethashvaluez",
    (byte*)"start",
    (byte*)"inithashtable",
    (byte*)"addstname",
    (byte*)"startlex",
    (byte*)"addnamestr",
    (byte*)"ps",
    (byte*)"psnext",
    (byte*)"lex",
    (byte*)"lxerror_s",
    (byte*)"makedecimal",
    (byte*)"readdec",
    (byte*)"readhex",
    (byte*)"readbin",
    (byte*)"readreal",
    (byte*)"readrawxname",
    (byte*)"reportcterror",
    (byte*)"geterrorinfo",
    (byte*)"showerrorsource",
    (byte*)"stopcompiler",
    (byte*)"gerror",
    (byte*)"gerror_s",
    (byte*)"serror",
    (byte*)"serror_s",
    (byte*)"rxerror",
    (byte*)"rxerror_s",
    (byte*)"lxerror",
    (byte*)"loaderror",
    (byte*)"prterror",
    (byte*)"allocunitrec",
    (byte*)"createintunit",
    (byte*)"createrealunit",
    (byte*)"createstringunit",
    (byte*)"createunit0",
    (byte*)"createunit1",
    (byte*)"createunit2",
    (byte*)"createname",
    (byte*)"addlistunit",
    (byte*)"createavname",
    (byte*)"convtostringz",
    (byte*)"findprocname",
    (byte*)"strexpr",
    (byte*)"strexpr_s",
    (byte*)"jeval",
    (byte*)"jevallist",
    (byte*)"additem",
    (byte*)"isalphanum",
    (byte*)"getopcname",
    (byte*)"convertstring",
    (byte*)"createavnamex",
    (byte*)"storemode",
    (byte*)"nextpoweroftwo",
    (byte*)"testelem",
    (byte*)"setelem",
    (byte*)"setelemblock",
    (byte*)"ispoweroftwo",
    (byte*)"deleteunit",
    (byte*)"skipsemi",
    (byte*)"checksymbol",
    (byte*)"skipsymbol",
    (byte*)"pcnotmut",
    (byte*)"start",
    (byte*)"start",
    (byte*)"var_empty_list",
    (byte*)"var_make_list",
    (byte*)"obj_newlist",
    (byte*)"obj_free_list",
    (byte*)"var_getix_list",
    (byte*)"var_getslice_list",
    (byte*)"var_getixref_list",
    (byte*)"var_putix_list",
    (byte*)"var_putslice_list",
    (byte*)"obj_append_list",
    (byte*)"obj_resize_list",
    (byte*)"var_appendto_list",
    (byte*)"var_dupl_list",
    (byte*)"var_mul_list",
    (byte*)"var_equal_list",
    (byte*)"var_concatto_list",
    (byte*)"var_inx_list",
    (byte*)"loadsp",
    (byte*)"getmodulefilename",
    (byte*)"loadsourcefile",
    (byte*)"loadstring",
    (byte*)"readfileline",
    (byte*)"findnextlineheader",
    (byte*)"loadqafile",
    (byte*)"readqabundle",
    (byte*)"start",
    (byte*)"addglobalname",
    (byte*)"newstrec",
    (byte*)"addsymbol",
    (byte*)"addproc",
    (byte*)"newusertypex",
    (byte*)"resolvedottedname",
    (byte*)"addgenfield",
    (byte*)"makereftype",
    (byte*)"makeaxtype",
    (byte*)"makestrtype",
    (byte*)"addanontype",
    (byte*)"createusertype",
    (byte*)"getalignment",
    (byte*)"duplfield",
    (byte*)"writesig",
    (byte*)"createdupldef",
    (byte*)"start",
    (byte*)"var_loadpacked",
    (byte*)"var_storepacked",
    (byte*)"setfslength",
    (byte*)"getfslength",
    (byte*)"var_make_struct",
    (byte*)"obj_new_struct",
    (byte*)"var_dupl_struct",
    (byte*)"obj_free_struct",
    (byte*)"var_equal_struct",
    (byte*)"var_getix_struct",
    (byte*)"start",
    (byte*)"parsemodule",
    (byte*)"readexpression",
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
    (byte*)"readtermsuffix",
    (byte*)"readterm",
    (byte*)"readsunit",
    (byte*)"checkequals",
    (byte*)"readindex",
    (byte*)"readdotsuffix",
    (byte*)"readslist",
    (byte*)"readcondsuffix",
    (byte*)"readkeyindex",
    (byte*)"readlbrack",
    (byte*)"readif",
    (byte*)"checkend",
    (byte*)"readunless",
    (byte*)"readwhile",
    (byte*)"readrepeat",
    (byte*)"readfor",
    (byte*)"readdo",
    (byte*)"readto",
    (byte*)"makeblock",
    (byte*)"readvardef",
    (byte*)"readconstdef",
    (byte*)"readreturn",
    (byte*)"readprint",
    (byte*)"readread",
    (byte*)"readloopcontrol",
    (byte*)"readintunit",
    (byte*)"readswitchcase",
    (byte*)"readgoto",
    (byte*)"readstop",
    (byte*)"readcast",
    (byte*)"readset",
    (byte*)"readtabledef",
    (byte*)"readtry",
    (byte*)"readsprint",
    (byte*)"readsread",
    (byte*)"readimportdll",
    (byte*)"readffiparams",
    (byte*)"readtypeparams",
    (byte*)"readtypenameparams",
    (byte*)"readrecorddef",
    (byte*)"readrecordbody",
    (byte*)"readrecordfields",
    (byte*)"readstructbody",
    (byte*)"addstructflag",
    (byte*)"readprocdef",
    (byte*)"readatfield",
    (byte*)"istypestarter",
    (byte*)"readmacrodef",
    (byte*)"readhostparams",
    (byte*)"pushlisttype",
    (byte*)"poplisttype",
    (byte*)"readcompilervar",
    (byte*)"readpair",
    (byte*)"lexchecksymbol",
    (byte*)"readtypedef",
    (byte*)"readtypespec",
    (byte*)"readparams",
    (byte*)"checkoperator",
    (byte*)"readlambda",
    (byte*)"readpackvars",
    (byte*)"start",
    (byte*)"start",
    (byte*)"evalunit",
    (byte*)"gencodemodule",
    (byte*)"do_procdef",
    (byte*)"genprocentry",
    (byte*)"genprocexit",
    (byte*)"evalref",
    (byte*)"genjumpcond",
    (byte*)"gcomparejump",
    (byte*)"genjumpl",
    (byte*)"stacklooplabels",
    (byte*)"unstacklooplabels",
    (byte*)"findlooplabel",
    (byte*)"do_assign",
    (byte*)"do_bin",
    (byte*)"do_binref",
    (byte*)"do_unary",
    (byte*)"do_unaryref",
    (byte*)"do_pushlist",
    (byte*)"do_makedict",
    (byte*)"do_call",
    (byte*)"pushparams",
    (byte*)"evalparam",
    (byte*)"pushkwdparams",
    (byte*)"do_if",
    (byte*)"do_do",
    (byte*)"do_loop",
    (byte*)"do_to",
    (byte*)"do_while",
    (byte*)"do_repeat",
    (byte*)"do_for",
    (byte*)"do_forx",
    (byte*)"do_print",
    (byte*)"do_fprint",
    (byte*)"do_read",
    (byte*)"do_forall",
    (byte*)"do_case",
    (byte*)"do_case_nc",
    (byte*)"do_try",
    (byte*)"unitstoarray",
    (byte*)"do_select",
    (byte*)"do_andl",
    (byte*)"do_orl",
    (byte*)"do_incr",
    (byte*)"do_callhost",
    (byte*)"callhostfn",
    (byte*)"genfree",
    (byte*)"do_return",
    (byte*)"do_multassign",
    (byte*)"do_store",
    (byte*)"getconstvalue",
    (byte*)"do_convert",
    (byte*)"checkelems",
    (byte*)"do_switch",
    (byte*)"do_simpleswitch",
    (byte*)"do_makerecordkv",
    (byte*)"do_idiv",
    (byte*)"do_irem",
    (byte*)"do_map",
    (byte*)"pushstring",
    (byte*)"checkblockreturn",
    (byte*)"start",
    (byte*)"start",
    (byte*)"resetpcl",
    (byte*)"genpc",
    (byte*)"genpc_int",
    (byte*)"genpc_n",
    (byte*)"genpc_xy",
    (byte*)"genpc_name",
    (byte*)"genopnd_strz",
    (byte*)"genopnd_str",
    (byte*)"genopnd_obj",
    (byte*)"genpc_real",
    (byte*)"genpc_lab",
    (byte*)"gencomment",
    (byte*)"extendpcldata",
    (byte*)"extendlabeltable",
    (byte*)"definelabel",
    (byte*)"createfwdlabel",
    (byte*)"definefwdlabel",
    (byte*)"genxy",
    (byte*)"pch_print",
    (byte*)"pch_print_nf",
    (byte*)"pch_printnogap",
    (byte*)"pch_println",
    (byte*)"pch_reread",
    (byte*)"pch_rereadln",
    (byte*)"pch_startprint",
    (byte*)"pch_startprintcon",
    (byte*)"pch_endprint",
    (byte*)"pch_strstartprint",
    (byte*)"pch_strendprint",
    (byte*)"pch_printspace",
    (byte*)"pch_readln",
    (byte*)"pch_sread",
    (byte*)"pch_sreadln",
    (byte*)"readname",
    (byte*)"readstring",
    (byte*)"readint",
    (byte*)"readhex",
    (byte*)"readbin",
    (byte*)"readreal",
    (byte*)"getreadfmtcode",
    (byte*)"stepkbpos",
    (byte*)"readany",
    (byte*)"readitem",
    (byte*)"strtoreal",
    (byte*)"strtoint",
    (byte*)"printnextfmtchars",
    (byte*)"pch_setformat",
    (byte*)"pc_getfmt",
    (byte*)"addstring",
    (byte*)"domultichar",
    (byte*)"printstr_n",
    (byte*)"pch_strtoval",
    (byte*)"tostr_int",
    (byte*)"tostr_real",
    (byte*)"tostr_str",
    (byte*)"pch_tostr",
    (byte*)"tostr_range",
    (byte*)"tostr_array",
    (byte*)"tostr_bits",
    (byte*)"tostr_struct",
    (byte*)"tostr_set",
    (byte*)"tostr_dict",
    (byte*)"tostr_decimal",
    (byte*)"tostr",
    (byte*)"tostr_list",
    (byte*)"start",
    (byte*)"var_make_record",
    (byte*)"obj_new_record",
    (byte*)"obj_free_record",
    (byte*)"var_dupl_record",
    (byte*)"var_equal_record",
    (byte*)"var_getix_record",
    (byte*)"var_putix_record",
    (byte*)"var_getixref_record",
    (byte*)"start",
    (byte*)"rx_module",
    (byte*)"rx_passdef",
    (byte*)"rx_deflist",
    (byte*)"rx_unit",
    (byte*)"rx_unitlist",
    (byte*)"evalmonop",
    (byte*)"evalbinop",
    (byte*)"makeintconst",
    (byte*)"makerealconst",
    (byte*)"resolvename",
    (byte*)"resolvetopname",
    (byte*)"resolvedot",
    (byte*)"resolvedot_sym",
    (byte*)"finddupl",
    (byte*)"expandmacro",
    (byte*)"copylistunit",
    (byte*)"copyunit",
    (byte*)"replaceunit",
    (byte*)"fixmode",
    (byte*)"fixmode2",
    (byte*)"fixusertypes",
    (byte*)"tx_typetable",
    (byte*)"getconstint",
    (byte*)"converttype",
    (byte*)"scanstruct",
    (byte*)"dobaseclass",
    (byte*)"start",
    (byte*)"disploop",
    (byte*)"start",
    (byte*)"fixupcode",
    (byte*)"runqprogram",
    (byte*)"pcerror",
    (byte*)"pcustype",
    (byte*)"pcustype_t",
    (byte*)"pcmxtypes",
    (byte*)"pcmxtypestt",
    (byte*)"reportpcerror",
    (byte*)"getpcerrorpos",
    (byte*)"findmodulefrompc",
    (byte*)"k_makelist",
    (byte*)"k_len",
    (byte*)"k_maths",
    (byte*)"k_lwb",
    (byte*)"k_upb",
    (byte*)"k_swap",
    (byte*)"k_bounds",
    (byte*)"k_type",
    (byte*)"k_dot",
    (byte*)"k_dotref",
    (byte*)"k_popdot",
    (byte*)"resolvefield",
    (byte*)"k_convrefpack",
    (byte*)"k_incrptr",
    (byte*)"k_cmp",
    (byte*)"k_bytesize",
    (byte*)"k_when",
    (byte*)"raiseexception",
    (byte*)"runproc_m",
    (byte*)"runproc",
    (byte*)"k_keyindex",
    (byte*)"k_popkeyindex",
    (byte*)"k_keyindexref",
    (byte*)"k_map",
    (byte*)"start",
    (byte*)"obj_free_set",
    (byte*)"var_dupl_set",
    (byte*)"var_equal_set",
    (byte*)"getsetbytes",
    (byte*)"var_make_set",
    (byte*)"obj_newset",
    (byte*)"var_emptyset",
    (byte*)"var_getix_set",
    (byte*)"var_putix_set",
    (byte*)"var_getixref_set",
    (byte*)"getoffset",
    (byte*)"var_in_set",
    (byte*)"iresizeset",
    (byte*)"obj_resize_set",
    (byte*)"iorsetbits",
    (byte*)"ixorsetbits",
    (byte*)"iandsetbits",
    (byte*)"inotsetbits",
    (byte*)"var_iorto_set",
    (byte*)"var_iandto_set",
    (byte*)"var_ixorto_set",
    (byte*)"var_inotto_set",
    (byte*)"start",
    (byte*)"start",
    (byte*)"var_empty_string",
    (byte*)"var_make_string",
    (byte*)"var_make_stringn",
    (byte*)"obj_new_string",
    (byte*)"obj_make_string",
    (byte*)"obj_make_stringn",
    (byte*)"obj_free_string",
    (byte*)"var_dupl_string",
    (byte*)"var_getix_string",
    (byte*)"var_getixref_string",
    (byte*)"var_getdotix_string",
    (byte*)"var_getdotixref_string",
    (byte*)"var_getslice_string",
    (byte*)"stringslice",
    (byte*)"var_putix_string",
    (byte*)"var_putslice_string",
    (byte*)"var_putdotix_string",
    (byte*)"obj_resize_string",
    (byte*)"var_add_string",
    (byte*)"var_addto_string",
    (byte*)"var_addto_string_ch",
    (byte*)"var_equal_string",
    (byte*)"var_compare_string",
    (byte*)"cmpstring_len",
    (byte*)"var_inx_string",
    (byte*)"var_iconvcase",
    (byte*)"var_makestrslicexobj",
    (byte*)"obj_make_strslicexobj",
    (byte*)"var_asc",
    (byte*)"var_new_string",
    (byte*)"var_new_stringn",
    (byte*)"var_mul_string",
    (byte*)"var_convert_string_list",
    (byte*)"var_expand_string",
    (byte*)"var_makechar",
    (byte*)"loadsysmodule",
    (byte*)"start",
    (byte*)"start",
    (byte*)"printunit",
    (byte*)"printunitlist",
    (byte*)"getprefix",
    (byte*)"getlineinfok",
    (byte*)"printglobalsymbols",
    (byte*)"printst",
    (byte*)"printstrec",
    (byte*)"printtypetables",
    (byte*)"showsttree",
    (byte*)"showtypes",
    (byte*)"showast",
    (byte*)"showast2",
    (byte*)"showlogfile",
    (byte*)"addtolog",
    (byte*)"showstflat",
    (byte*)"showmoduleinfo",
    (byte*)"printsymbol",
    (byte*)"strmode",
    (byte*)"istrmode",
    (byte*)"deletetempfiles",
    (byte*)"start",
    (byte*)"writepcl",
    (byte*)"writepclopnd",
    (byte*)"writeallpcl",
    (byte*)"showpcl",
    (byte*)"showpcl2",
    (byte*)"gstr",
    (byte*)"gstrln",
    (byte*)"gline",
    (byte*)"gstrint",
    (byte*)"glabeldef",
    (byte*)"start",
    (byte*)"var_unshareu",
    (byte*)"obj_shareu",
    (byte*)"void_new",
    (byte*)"obj_new",
    (byte*)"var_getintvalue",
    (byte*)"var_fromobj",
    (byte*)"var_free",
    (byte*)"var_duplu",
    (byte*)"var_neg",
    (byte*)"var_abs",
    (byte*)"var_inot",
    (byte*)"var_istruel",
    (byte*)"var_add",
    (byte*)"var_addmixed",
    (byte*)"var_addto",
    (byte*)"var_sub",
    (byte*)"var_submixed",
    (byte*)"var_mul",
    (byte*)"var_mulmixed",
    (byte*)"var_div",
    (byte*)"var_divmixed",
    (byte*)"var_idiv",
    (byte*)"var_irem",
    (byte*)"var_iand",
    (byte*)"var_ior",
    (byte*)"var_ixor",
    (byte*)"var_shl",
    (byte*)"var_shr",
    (byte*)"var_in",
    (byte*)"var_inx",
    (byte*)"var_equal",
    (byte*)"var_equalmixed",
    (byte*)"var_compare",
    (byte*)"var_comparemixed",
    (byte*)"var_concat",
    (byte*)"var_append",
    (byte*)"var_min",
    (byte*)"var_max",
    (byte*)"var_concatto",
    (byte*)"var_appendto",
    (byte*)"var_getix",
    (byte*)"var_putix",
    (byte*)"var_getixref",
    (byte*)"var_getslice",
    (byte*)"var_putslice",
    (byte*)"var_getdotix",
    (byte*)"var_putdotix",
    (byte*)"var_getdotixref",
    (byte*)"var_getdotslice",
    (byte*)"var_putdotslice",
    (byte*)"var_getdotsliceref",
    (byte*)"var_expand",
    (byte*)"var_inplace",
    (byte*)"var_inplace_unary",
    (byte*)"var_loadptr",
    (byte*)"var_storeptr",
    (byte*)"var_loadbit",
    (byte*)"var_storebit",
    (byte*)"var_convert",
    (byte*)"var_gethashvalue",
    (byte*)"var_objtovar",
    (byte*)"var_putdotix_intint",
    (byte*)"var_power",
    (byte*)"var_powermixed",
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
    (byte*)"pcm_allocnfz",
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
    (byte*)"dirlist",
    (byte*)"start",
    (byte*)"os_calldllfunction",
    (byte*)"os_pushargs",
    (byte*)"calldll_cint",
    (byte*)"calldll_creal",
    (byte*)"os_dummycall",
    (byte*)"start",
(byte*)""};
static i64 msysc$_fnnprocs=960;
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
int main(int _nargs, char** _args) {
    msysc$m_init(_nargs, (void*)_args);

// call main-start() routines...
    msysc$start();
    qq_cli$start();

        i64 i;
        i64 stopcode;
    qq_cli$initdata();
    qq_cli$getinputoptions();
    qq_modules$readqabundle();
    qq_cli$loadsyslib();
    qq_cli$compile_sp(qq_cli$inputfile,0);
    if (!!((i64)qq_cli$fallsp)) {
        if ((!!((i64)qq_cli$fshowast1) && ((i64)qq_cli$runcode == (i64)2))) {
            qq_show$showast(0,(byte*)"AST1");
        }
;
        if ((!!((i64)qq_cli$fshowast2) && ((i64)qq_cli$runcode > (i64)2))) {
            qq_show$showast(0,(byte*)"AST2");
        }
;
        if ((!!((i64)qq_cli$fshowpcl2) && ((i64)qq_cli$runcode == (i64)5))) {
            qq_showpcl$showpcl(0,(i64)2);
        }
;
    }
;
    qq_cli$writeqafile();
    for (i=(i64)1;i<=qq_decls$nsubprogs;++i) {
L1 :;
        stopcode = qq_runlab$runqprogram(qq_decls$subprogs[(i)-1],(i64)(i == qq_decls$nsubprogs));
L2 :;
    }
L3 :;
    ;
    qq_show$showlogfile();
    exit(stopcode);
    return 0;
}

static void qq_cli$getinputoptions(void) {
        i64 paramno;
        i64 pmtype;
        u8 *  name;
        u8 *  value;
        u8 *  appstr;
        u8 *(*fnaddr)(void);
        i64 sw;
    fnaddr = (u8 *(*)(void))mlib$findfunction((byte*)"getbuiltin_app");
    paramno = (i64)1;
    L4 :;
    while (!!((pmtype = mlib$nextcmdparamnew(&paramno,&name,&value,(byte*)"q")))) {
        if ((pmtype==(i64)1)) {
            mlib$convlcstring(name);
            for (sw=(i64)1;sw<=(i64)23;++sw) {
L7 :;
                if (!!(mlib$eqstring(name,qq_decls$optionnames[(sw)-1]))) {
                    qq_cli$do_option(sw,value);
                    goto L9 ;
                }
;
L8 :;
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
L9 :;
            ;
        }
        else if ((pmtype==(i64)2)) {
            if (!!(fnaddr)) {
                --(paramno);
                goto L6 ;
            }
;
            qq_cli$inputfile = mlib$pcm_copyheapstring(name);
            goto L6 ;
        }
;
L5 :;
    }
L6 :;
    ;
    if (!!(fnaddr)) {
        appstr = ((*fnaddr))();
        qq_lib$loaderror((byte*)"DO BUILT-IN",(byte*)"");
    }
    else if (!(!!(qq_cli$inputfile))) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"Q5.2 Interpreter",NULL);
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
        msysc$m_print_str(msysc$sysparams[((i64)1)-1],NULL);
        msysc$m_print_str((byte*)"filename[.q]",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        exit(0);
    }
;
    qq_cli$cmdstartindex = paramno;
    qq_cli$setcli((u8 *(*)[])&(*msysc$cmdparams)[(qq_cli$cmdstartindex)],((msysc$ncmdparams - qq_cli$cmdstartindex) + (i64)1));
}

static void qq_cli$do_option(i64 sw,u8 *value) {
        byte *  p;
    p = qq_decls$optionvars[(sw)-1];
    if (!!(p)) {
        (*p) = (i64)qq_decls$optionvalues[(sw)-1];
        return;
    }
;
}

void qq_cli$compile_sp(u8 *filename,u8 *source) {
        struct qq_decls$subprogrec *  sp;
        i64 a;
        i64 b;
        i64 tt;
        i64 m;
    tt = clock();
    sp = qq_modules$loadsp(filename,source);
    if (((i64)qq_cli$runcode < (i64)2)) {
        return;
    }
;
    a = (i64)(*sp).firstmodule;
    b = (i64)(*sp).lastmodule;
    for (m=a;m<=b;++m) {
L10 :;
        qq_parse$parsemodule(qq_decls$modules[(m)]);
L11 :;
    }
L12 :;
    ;
    qq_resolve$fixusertypes();
    if ((!!((i64)qq_cli$fshowast1) && !(!!((i64)qq_cli$fallsp)))) {
        qq_show$showast(sp,(byte*)"AST1");
    }
;
    if (((i64)qq_cli$runcode < (i64)3)) {
        return;
    }
;
    qq_resolve$tx_typetable();
    for (m=a;m<=b;++m) {
L13 :;
        qq_resolve$rx_module(qq_decls$modules[(m)]);
L14 :;
    }
L15 :;
    ;
    if ((!!((i64)qq_cli$fshowast2) && !(!!((i64)qq_cli$fallsp)))) {
        qq_show$showast(sp,(byte*)"AST2");
    }
;
    if (((i64)qq_cli$runcode < (i64)4)) {
        return;
    }
;
    for (m=a;m<=b;++m) {
L16 :;
        qq_pclgen$gencodemodule(sp,m);
L17 :;
    }
L18 :;
    ;
    if ((!!((i64)qq_cli$fshowpcl1) && !(!!((i64)qq_cli$fallsp)))) {
        qq_showpcl$showpcl(sp,(i64)1);
    }
;
    qq_cli$fixup_sp(sp);
    if (((!!((i64)qq_cli$fshowpcl2) && ((i64)qq_cli$runcode == (i64)5)) && !(!!((i64)qq_cli$fallsp)))) {
        qq_showpcl$showpcl(sp,(i64)2);
    }
;
    qq_cli$resetcompiler();
}

static void qq_cli$setcli(u8 *(*cmds)[],i64 ncmds) {
        i64 i;
    for (i=(i64)1;i<=ncmds;++i) {
L19 :;
        qq_cli$setcmdparam(i,(*cmds)[(i)-1]);
L20 :;
    }
L21 :;
    ;
}

static void qq_cli$writeqafile(void) {
        u8 filename[300];
        struct qq_decls$filerec *  sflist[200];
        void *  f;
        i64 offset;
        i64 nfiles;
        struct qq_decls$filerec *  pm;
        i64 leadmod;
        i64 i;
    if (!(!!((i64)qq_cli$fwriteqa))) {
        return;
    }
;
    strcpy(filename,mlib$changeext(qq_cli$inputfile,(byte*)"qa"));
    nfiles = (i64)0;
    leadmod = (i64)(*qq_decls$subprogs[(qq_decls$nsubprogs)-1]).firstmodule;
    sflist[(++(nfiles))-1] = qq_decls$modules[(leadmod)];
    for (i=(i64)1;i<=qq_decls$nmodules;++i) {
L22 :;
        if ((i != leadmod)) {
            pm = qq_decls$modules[(i)];
            if ((!!((i64)(*pm).issyslib) && ((i64)qq_cli$fwriteqa == (i64)1))) {
                goto L23 ;
            }
;
            sflist[(++(nfiles))-1] = pm;
        }
;
L23 :;
    }
L24 :;
    ;
    if ((nfiles == (i64)0)) {
        qq_lib$loaderror((byte*)"QA:no files",(byte*)"");
    }
;
    f = fopen(filename,(byte*)"wb");
    if (!(!!(f))) {
        qq_lib$loaderror((byte*)"Can't create qa file #",filename);
    }
;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Writing ",NULL);
    msysc$m_print_str(filename,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_setfmt((byte*)"=== QA # ===");
    msysc$m_print_i64(nfiles,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=nfiles;++i) {
L25 :;
        pm = sflist[(i)-1];
        msysc$m_print_startfile(f);
        msysc$m_print_setfmt((byte*)"=== #.q # # #/# ===");
        msysc$m_print_str((*pm).name,NULL);
        msysc$m_print_i64((i64)(*pm).issyslib,NULL);
        msysc$m_print_i64((i64)(*pm).issupport,NULL);
        msysc$m_print_i64(i,NULL);
        msysc$m_print_i64(nfiles,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        offset = mlib$getfilepos(f);
        mlib$writerandom(f,(byte *)(*pm).text,offset,(*pm).size);
L26 :;
    }
L27 :;
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"=== END ===",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=nfiles;++i) {
L28 :;
        msysc$m_print_startfile(f);
        msysc$m_print_setfmt((byte*)"# #.q");
        msysc$m_print_i64(i,NULL);
        msysc$m_print_str((*sflist[(i)-1]).name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L29 :;
    }
L30 :;
    ;
    fclose(f);
    exit(0);
}

static void qq_cli$initdata(void) {
    qq_lex$lexinit();
    qq_decls$stprogram = qq_names$createdupldef(0,qq_lex$addnamestr((byte*)"$prog"),(i64)1);
    mlinux$os_initwindows();
    qq_tables$firstusertype = (i64)41;
    qq_show$deletetempfiles();
    qq_decls$fnosys = (i64)!(!!(mlinux$os_iswindows()));
}

static void qq_cli$loadsyslib(void) {
        u8 str[300];
    if (!!((i64)qq_decls$fnosys)) {
        return;
    }
;
    if ((u64)1u) {
        qq_decls$usebundled = (i64)0;
    }
;
    if (!!((i64)qq_decls$usebundled)) {
        qq_cli$compile_sp((byte*)"sysp.q",0);
    }
    else {
        strcpy(str,(byte*)"c:/bx/");
        strcat(str,(byte*)"sysp.q");
        qq_cli$compile_sp(str,0);
    }
;
}

static void qq_cli$resetcompiler(void) {
        struct qq_decls$userxrec *  userxmodelist;
    qq_tables$nuserxtypes = (i64)0;
    qq_tables$userxtypebase = (i64)0;
    userxmodelist = 0;
    memset(&(qq_tables$ttxmap),0,40000);
    qq_tables$firstusertype = (qq_tables$ntypes + (i64)1);
}

static void qq_cli$setcmdparam(i64 index,u8 *s) {
    if ((s == 0)) {
        qq_decls$nqparams = index;
    }
    else if ((index <= (i64)32)) {
        qq_decls$qparamtable[(index)-1] = mlib$pcm_copyheapstring(s);
        qq_decls$nqparams=(qq_decls$nqparams>index?qq_decls$nqparams:index);
;
    }
;
}

static void qq_cli$fixup_sp(struct qq_decls$subprogrec *sp) {
        i64 $av_1;
        i64 i;
    if (((i64)qq_cli$runcode < (i64)5)) {
        return;
    }
;
        ($av_1 = (i64)(*sp).lastmodule);
    for (i=(i64)(*sp).firstmodule;i<=$av_1;++i) {
L31 :;
        qq_cli$fixupmodule(qq_decls$modules[(i)]);
        if (!!((i64)qq_cli$foptimise)) {
            qq_cli$optimise_module(qq_decls$modules[(i)]);
        }
;
        qq_runlab$fixupcode(qq_decls$modules[(i)]);
L32 :;
    }
L33 :;
    ;
}

static void qq_cli$fixproc(struct qq_decls$strec *d) {
    if (!(!!((i64)(*d).procfixed))) {
        (*d).nextproc = qq_cli$allprocdefs;
        qq_cli$allprocdefs = d;
        (*d).procfixed = (i64)1;
    }
;
}

static void qq_cli$fixupmodule(struct qq_decls$filerec *pm) {
        struct qq_pcltabs$pclrec *  pc;
        struct qq_pcltabs$pclrec *  pcstart;
        i64 cmd;
        struct qq_decls$strec *  d;
    pc = (pcstart = (*pm).pcstart);
    L34 :;
    do {
        cmd = (i64)(*pc).opcode;
        if ((cmd==(i64)2)) {
            qq_cli$fixproc((*pc).def);
        }
;
                {i64 $temp = (i64)qq_pcltabs$pclopnd[(cmd)];
if (($temp==(i64)3)) {
            qq_cli$fixproc((d = (*pc).def));
            (*pc).labelref = (*d).labelref;
        }
        else if (($temp==(i64)1)) {
            d = (*pc).def;
            if (!!(msysc$m_getdotindex((i64)(*d).flags,(i64)9))) {
                (*pc).offset = ((i64)(*d).index * (i64)16);
                ++((*pc).opcode);
            }
            else {
                if (((*d).varptr == 0)) {
                    (*d).nextstatic = qq_cli$allstaticdefs;
                    qq_cli$allstaticdefs = d;
                    (*d).varptr = (struct qq_decls$varrec *)mlib$pcm_allocz((i64)16);
                }
;
                (*pc).varptr = (*d).varptr;
            }
;
        }
        else if (($temp==(i64)5)) {
            (*pc).index = (*(*pc).def).genfieldindex;
        }
        else if (($temp==(i64)9)) {
            (*pc).objptr = qq_strings$obj_make_string((*pc).svalue,(i64)0);
        }
        };
        ++(pc);
L35 :;
    }
    while (!(cmd == (i64)5));
L36 :;
    ;
}

static void qq_cli$optimise_module(struct qq_decls$filerec *pm) {
        struct qq_pcltabs$pclrec *  pc;
    pc = (*pm).pcstart;
    L37 :;
    while (1) {
        if (((i64)(*pc).opcode == (i64)5)) {
            goto L38 ;
        }
;
        pc = qq_cli$optim(pc);
    }
L38 :;
    ;
    pc = (*pm).pcstart;
    L39 :;
    while (((i64)(*pc).opcode != (i64)5)) {
        ++(qq_decls$pclcounts[((i64)(*pc).opcode)]);
L40 :;
        ++(pc);
L42 :;
            }
L41 :;
    ;
}

static struct qq_pcltabs$pclrec *qq_cli$optim(struct qq_pcltabs$pclrec *pc) {
        i64 skip;
        struct qq_decls$genfieldrec *  g;
        byte cmd;
        byte abc;
        byte aux;
        i64 i;
    cmd = (i64)(*pc).opcode;
    skip = (i64)0;
    if (!!(msysc$m_getdotindex((i64)(*(pc + (i64)1)).flags,(i64)0))) {
        return (pc + (i64)1);
    }
;
    abc = (i64)1;
    if (!!(msysc$m_getdotindex((i64)(*(pc + (i64)2)).flags,(i64)0))) {
        abc = (i64)0;
    }
;
    if (((i64)cmd==(i64)8)) {
                {i64 $temp = (i64)(*(pc + (i64)1)).opcode;
if (($temp==(i64)8)) {
            if (!(!!((i64)abc))) {
                goto L43 ;
;
            }
;
                        {i64 $temp = (i64)(*(pc + (i64)2)).opcode;
if (($temp==(i64)8)) {
                (*pc).opcode = (i64)144;
                skip = (i64)2;
            }
            else if (($temp==(i64)100)) {
                (*pc).opcode = (i64)175;
                skip = (i64)2;
            }
            else if (($temp==(i64)101)) {
                skip = (i64)2;
            }
            else if (($temp==(i64)33)) {
                (*pc).opcode = (i64)167;
                skip = (i64)2;
            }
            else if (($temp==(i64)34)) {
                (*pc).opcode = (i64)168;
                skip = (i64)2;
            }
            else if (($temp==(i64)35)) {
                (*pc).opcode = (i64)169;
                skip = (i64)2;
            }
            else if (($temp==(i64)36)) {
                (*pc).opcode = (i64)170;
                skip = (i64)2;
            }
            else if (($temp==(i64)37)) {
                (*pc).opcode = (i64)171;
                skip = (i64)2;
            }
            else if (($temp==(i64)38)) {
                (*pc).opcode = (i64)172;
                skip = (i64)2;
            }
            else if (($temp==(i64)131)) {
                (*pc).opcode = (i64)179;
                skip = (i64)2;
            }
            else {
                //dopushff:
L43 :;
;
                (*pc).opcode = (i64)145;
                skip = (i64)1;
            }
            };
        }
        else if (($temp==(i64)7)) {
        }
        else if (($temp==(i64)13)) {
            if (!(!!((i64)abc))) {
                goto L44 ;
;
            }
;
                        {i64 $temp = (i64)(*(pc + (i64)2)).opcode;
if (($temp==(i64)100)) {
                (*pc).opcode = (i64)173;
                skip = (i64)2;
            }
            else if (($temp==(i64)101)) {
                skip = (i64)2;
            }
            else if (($temp==(i64)35)) {
                (*pc).opcode = (i64)163;
                skip = (i64)2;
            }
            else if (($temp==(i64)36)) {
                (*pc).opcode = (i64)164;
                skip = (i64)2;
            }
            else if (($temp==(i64)37)) {
                (*pc).opcode = (i64)165;
                skip = (i64)2;
            }
            else if (($temp==(i64)38)) {
                (*pc).opcode = (i64)166;
                skip = (i64)2;
            }
            else if (($temp==(i64)33)) {
                (*pc).opcode = (i64)161;
                skip = (i64)2;
            }
            else if (($temp==(i64)34)) {
                (*pc).opcode = (i64)162;
                skip = (i64)2;
            }
            else {
                //dopushfci:
L44 :;
;
                (*pc).opcode = (i64)150;
                skip = (i64)1;
            }
            };
        }
        else if (($temp==(i64)11)) {
            skip = (i64)1;
        }
        else if (($temp==(i64)12)) {
            (*pc).opcode = (i64)151;
            skip = (i64)1;
        }
        else if (($temp==(i64)24)) {
            skip = (i64)1;
        }
        else if (($temp==(i64)42)) {
            skip = (i64)1;
        }
        else if (($temp==(i64)86)) {
            skip = (i64)1;
        }
        else if (($temp==(i64)88)) {
            skip = (i64)1;
        }
        else if (($temp==(i64)21)) {
            (*pc).opcode = (i64)181;
            skip = (i64)1;
        }
        };
    }
    else if (((i64)cmd==(i64)7)) {
                {i64 $temp = (i64)(*(pc + (i64)1)).opcode;
if (($temp==(i64)8)) {
            if ((!!((i64)abc) && ((i64)(*(pc + (i64)2)).opcode == (i64)131))) {
                (*pc).opcode = (i64)178;
                skip = (i64)2;
            }
            else {
                skip = (i64)1;
            }
;
        }
        else if (($temp==(i64)11)) {
            skip = (i64)1;
        }
        else if (($temp==(i64)12)) {
            skip = (i64)1;
        }
        else if (($temp==(i64)13)) {
            (*pc).opcode = (i64)149;
            skip = (i64)1;
        }
        };
    }
    else if (((i64)cmd==(i64)13)) {
                {i64 $temp = (i64)(*(pc + (i64)1)).opcode;
if (($temp==(i64)11)) {
            skip = (i64)1;
        }
        else if (($temp==(i64)12)) {
            (*pc).opcode = (i64)156;
            skip = (i64)1;
        }
        else if (($temp==(i64)24)) {
            skip = (i64)1;
        }
        else if (($temp==(i64)100)) {
            (*pc).opcode = (i64)177;
            skip = (i64)1;
        }
        else if (($temp==(i64)39)) {
            (*pc).opcode = (i64)190;
            skip = (i64)1;
        }
        else if (($temp==(i64)40)) {
            (*pc).opcode = (i64)191;
            skip = (i64)1;
        }
        };
    }
    else if (((i64)cmd==(i64)14)) {
                {i64 $temp = (i64)(*(pc + (i64)1)).opcode;
if (($temp==(i64)14)) {
            if ((!!((i64)abc) && ((i64)(*(pc + (i64)2)).opcode == (i64)14))) {
                skip = (i64)2;
            }
            else {
                skip = (i64)1;
            }
;
        }
        };
    }
    else if (((i64)cmd==(i64)10)) {
                {i64 $temp = (i64)(*(pc + (i64)1)).opcode;
if (($temp==(i64)72)) {
            if (!!((i64)abc)) {
                                {i64 $temp = (i64)(*(pc + (i64)2)).opcode;
if (($temp==(i64)21)) {
                    skip = (i64)2;
                }
                else if (($temp==(i64)22)) {
                    skip = (i64)2;
                }
                };
            }
;
        }
        };
    }
    else if (((i64)cmd==(i64)9)) {
                {i64 $temp = (i64)(*(pc + (i64)1)).opcode;
if (($temp==(i64)72)) {
            if (!!((i64)abc)) {
                                {i64 $temp = (i64)(*(pc + (i64)2)).opcode;
if (($temp==(i64)21)) {
                    (*pc).opcode = (i64)182;
                    skip = (i64)2;
                }
                else if (($temp==(i64)22)) {
                    (*pc).opcode = (i64)184;
                    skip = (i64)2;
                }
                };
            }
;
        }
        };
    }
    else if (((i64)cmd==(i64)126) || ((i64)cmd==(i64)128)) {
        g = (struct qq_decls$genfieldrec *)qq_decls$genfieldtable[((*pc).index)-1];
        if ((((*g).nextdef == 0) && ((i64)(*(*g).def).nameid == (i64)11))) {
            (*pc).opcode = (((i64)cmd == (i64)126) ? (i64)127 : (i64)129);
        }
;
    }
;
    //finish:
L45 :;
;
    if (!!(skip)) {
        aux = (i64)((i64)(*pc).opcode != (i64)cmd);
        for (i=(i64)1;i<=skip;++i) {
L46 :;
            ++(pc);
            (*pc).flags = msysc$m_setdotindex((*pc).flags,(i64)1,(u64)(i64)aux);
L47 :;
        }
L48 :;
        ;
    }
;
    return (pc + (i64)1);
}

// START
void qq_cli$start(void) {
    qq_arrays$start();
    qq_bits$start();
    qq_calldll$start();
    qq_decls$start();
    qq_decimal$start();
    qq_dicts$start();
    qq_host$start();
    qq_lex$start();
    qq_lib$start();
    qq_lists$start();
    qq_modules$start();
    qq_names$start();
    qq_packed$start();
    qq_parse$start();
    qq_pcltabs$start();
    qq_pclgen$start();
    qq_pcllib$start();
    qq_print$start();
    qq_records$start();
    qq_resolve$start();
    qq_runlab$start();
    qq_runaux$start();
    qq_sets$start();
    qq_strings$start();
    qq_syslibsdummy$start();
    qq_tables$start();
    qq_show$start();
    qq_showpcl$start();
    qq_vars$start();

}

void qq_arrays$var_empty_array(i64 tag,i64 elemtype,i64 lower,struct qq_decls$varrec *dest) {
    (*dest).objptr = qq_arrays$obj_newarray(elemtype,lower,(i64)0);
    (*dest).tagx = (tag | (i64)256);
}

void qq_arrays$obj_free_array(struct qq_decls$objrec *p) {
    if (!!((*p).length)) {
        mlib$pcm_free((*p).ptr,((*p).alloc64 * qq_tables$ttsize[((i64)(*p).elemtag)]));
    }
;
    mlib$pcm_free32(p);
}

void qq_arrays$obj_free_vector(struct qq_decls$objrec *p) {
    if (!!((*p).length)) {
        mlib$pcm_free((*p).ptr,qq_tables$ttsize[((i64)(*p).usertag)]);
    }
;
    mlib$pcm_free32(p);
}

void qq_arrays$var_make_array(struct qq_decls$varrec *a,struct qq_decls$varrec *dest,i64 lower,i64 n,i64 axtype,i64 elemtype) {
        struct qq_decls$objrec *  p;
        byte *  q;
        i64 m;
        i64 $av_1;
    if ((axtype != (i64)11)) {
        m = qq_tables$ttlength[(axtype)];
        if ((n != m)) {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"N=",NULL);
            msysc$m_print_i64(n,NULL);
            msysc$m_print_str((byte*)"M=",NULL);
            msysc$m_print_i64(m,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            qq_runaux$pcerror((byte*)"Too few/too many elements",(byte*)"");
        }
;
    }
    else if (!!(n)) {
        if ((elemtype == (i64)0)) {
                        {i64 $temp = (i64)(*((a + n) - (i64)1)).tag;
if (($temp==(i64)1)) {
                elemtype = (i64)26;
            }
            else if (($temp==(i64)2)) {
                elemtype = (i64)32;
            }
            else {
                elemtype = (i64)26;
            }
            };
        }
;
    }
    else if ((elemtype == (i64)0)) {
        elemtype = (i64)26;
    }
;
    p = qq_arrays$obj_newarray(elemtype,lower,n);
    q = (*p).ptr;
    $av_1 = n;
    while ($av_1-- > 0) {
L49 :;
        qq_packed$var_storepacked(q,a,elemtype);
        q += qq_tables$ttsize[(elemtype)];
        ++(a);
L50 :;
    }
L51 :;
    ;
    if ((axtype == (i64)11)) {
        (*dest).tagx = (i64)267;
    }
    else {
        (*dest).tagx = (i64)263;
        (*p).usertag = axtype;
    }
;
    (*dest).objptr = p;
}

struct qq_decls$objrec *qq_arrays$obj_newarray(i64 elemtype,i64 lower,i64 length) {
        struct qq_decls$objrec *  p;
        i64 elemsize;
    p = qq_vars$obj_new();
    (*p).flags = msysc$m_setdotindex((*p).flags,(i64)1,(u64)1u);
    if ((($rtemp=lower, $rtemp >= (i64)0 && $rtemp <= (i64)1))) {
        (*p).flags = msysc$m_setdotindex((*p).flags,(i64)0,(u64)lower);
    }
    else {
        qq_runaux$pcerror((byte*)"Lwb not 0/1",(byte*)"");
    }
;
    (*p).length = length;
    (*p).objtype = (i64)0;
    (*p).elemtag = elemtype;
    elemsize = qq_tables$ttsize[(elemtype)];
    if (!!(length)) {
        (*p).ptr = (byte *)mlib$pcm_allocz((length * elemsize));
        (*p).alloc64 = (mlib$allocbytes / elemsize);
    }
;
    return p;
}

struct qq_decls$objrec *qq_arrays$obj_newarray_u(i64 usertag) {
        struct qq_decls$objrec *  p;
        i64 elemsize;
    p = qq_vars$obj_new();
    (*p).flags = msysc$m_setdotindex((*p).flags,(i64)1,(u64)1u);
    (*p).objtype = (i64)0;
    (*p).usertag = usertag;
    elemsize = qq_tables$ttsize[((i64)qq_tables$tttarget[(usertag)])];
    if (!!(qq_tables$ttlength[(usertag)])) {
        (*p).ptr = (byte *)mlib$pcm_allocz(qq_tables$ttsize[(usertag)]);
        (*p).alloc64 = (mlib$allocbytes / elemsize);
    }
;
    return p;
}

void qq_arrays$var_getix_array(struct qq_decls$varrec *a,i64 index) {
        struct qq_decls$varrec v;
        struct qq_decls$objrec *  p;
        i64 elemtype;
        i64 length;
    v = (*a);
    p = (*a).objptr;
    if (((i64)v.tag == (i64)7)) {
        length = qq_tables$ttlength[((i64)(*p).usertag)];
        index -= qq_tables$ttlower[((i64)(*p).usertag)];
        elemtype = (i64)qq_tables$tttarget[((i64)(*p).usertag)];
    }
    else {
        length = (*p).length;
        elemtype = (i64)(*p).elemtag;
        index -= (i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0);
    }
;
    if (((u64)index >= (u64)length)) {
        qq_runaux$pcerror((byte*)"ax[int] bounds",(byte*)"");
    }
;
    if ((elemtype == (i64)27)) {
        (*a).tagx = (i64)1;
        (*a).value = (i64)(*((*p).ptr + index));
    }
    else {
        qq_packed$var_loadpacked(((*p).ptr + (index * qq_tables$ttsize[(elemtype)])),elemtype,a,0);
    }
;
}

void qq_arrays$var_putix_array(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *x) {
        struct qq_decls$varrec v;
        struct qq_decls$objrec *  p;
        i64 elemtype;
        i64 length;
        i64 lower;
    v = (*a);
    p = v.objptr;
    if (((i64)v.tag == (i64)7)) {
        length = qq_tables$ttlength[((i64)(*p).usertag)];
        lower = qq_tables$ttlower[((i64)(*p).usertag)];
        elemtype = (i64)qq_tables$tttarget[((i64)(*p).usertag)];
    }
    else {
        length = (*p).length;
        lower = (i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0);
        elemtype = (i64)(*p).elemtag;
    }
;
    index -= lower;
    if (((u64)index >= (u64)length)) {
        if ((index < (i64)0)) {
            qq_runaux$pcerror((byte*)"lwb",(byte*)"");
        }
        else if ((index == length)) {
            if (((i64)v.tag == (i64)7)) {
                qq_runaux$pcerror((byte*)"Can't append user type",(byte*)"");
            }
;
            qq_arrays$obj_append_array(p,x);
        }
        else {
            qq_runaux$pcerror((byte*)"ax[i]:=x bounds",(byte*)"");
        }
;
    }
;
    if ((elemtype == (i64)27)) {
        if (((i64)(*x).tag != (i64)1)) {
            qq_runaux$pcerror((byte*)"rhs not int",(byte*)"");
        }
;
        (*a).tagx = (i64)1;
        (*((*p).ptr + index)) = (*x).value;
    }
    else {
        qq_packed$var_storepacked(((*p).ptr + (index * qq_tables$ttsize[(elemtype)])),x,elemtype);
    }
;
}

void qq_arrays$var_getixref_array(struct qq_decls$varrec *a,i64 index) {
        struct qq_decls$varrec v;
        struct qq_decls$objrec *  p;
        i64 elemtype;
        i64 length;
        i64 lower;
    v = (*a);
    p = v.objptr;
    if (((i64)v.tag == (i64)7)) {
        length = qq_tables$ttlength[((i64)(*p).usertag)];
        lower = qq_tables$ttlower[((i64)(*p).usertag)];
        elemtype = (i64)qq_tables$tttarget[((i64)(*p).usertag)];
    }
    else {
        length = (*p).length;
        lower = (i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0);
        elemtype = (i64)(*p).elemtag;
    }
;
    index -= lower;
    if (((u64)index >= (u64)length)) {
        if ((index < (i64)0)) {
            qq_runaux$pcerror((byte*)"lwb",(byte*)"");
        }
        else {
            if (((u64)index == (u64)length)) {
                qq_runaux$pcerror((byte*)"PUTIXREF NEEDS IAPPEND",(byte*)"");
                p = (*a).objptr;
            }
            else {
                qq_runaux$pcerror((byte*)"ax[i]:=x bounds",(byte*)"");
            }
;
        }
;
    }
;
    (*a).tagx = (i64)16;
    (*a).elemtag = elemtype;
    (*a).ptr = ((*p).ptr + (index * qq_tables$ttsize[(elemtype)]));
}

static void qq_arrays$obj_append_array(struct qq_decls$objrec *a,struct qq_decls$varrec *x) {
        i64 n;
        byte *  q;
    if (((i64)(*a).objtype != (i64)0)) {
        qq_runaux$pcerror((byte*)"Can't extend slice",(byte*)"");
    }
;
    if (!(!!(msysc$m_getdotindex((i64)(*a).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    n = ((*a).length + (i64)1);
    if ((n > (*a).alloc64)) {
        qq_arrays$obj_resize_array(a,n);
    }
    else {
        (*a).length = n;
    }
;
    q = ((*a).ptr + ((n - (i64)1) * qq_tables$ttsize[((i64)(*a).elemtag)]));
    qq_packed$var_storepacked(q,x,(i64)(*a).elemtag);
}

void qq_arrays$var_appendto_array(struct qq_decls$varrec *a,struct qq_decls$varrec *x) {
    qq_arrays$obj_append_array((*a).objptr,x);
}

void qq_arrays$obj_resize_array(struct qq_decls$objrec *p,i64 n) {
        byte *  q;
        i64 elemsize;
    elemsize = qq_tables$ttsize[((i64)(*p).elemtag)];
    if ((n <= (*p).alloc64)) {
        (*p).length = n;
    }
    else {
        q = (byte *)mlib$pcm_alloc((n * elemsize));
        if (!!((*p).length)) {
            memcpy(q,(*p).ptr,(u64)((*p).length * elemsize));
            mlib$pcm_free((*p).ptr,((*p).alloc64 * elemsize));
        }
;
        (*p).ptr = q;
        (*p).length = n;
        (*p).alloc64 = (mlib$allocbytes / elemsize);
    }
;
}

void qq_arrays$var_dupl_array(struct qq_decls$varrec *a) {
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
    p = (*a).objptr;
    q = qq_arrays$obj_newarray((i64)(*p).elemtag,(i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0),(*p).length);
    (*a).objptr = q;
    if (!!((*p).length)) {
        memcpy((*q).ptr,(*p).ptr,(u64)((*p).length * qq_tables$ttsize[((i64)(*p).elemtag)]));
    }
;
}

void qq_arrays$var_dupl_vector(struct qq_decls$varrec *a) {
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
        i64 length;
    p = (*a).objptr;
    length = qq_tables$ttlength[((i64)(*p).usertag)];
    q = qq_arrays$obj_newarray_u((i64)(*p).usertag);
    (*a).objptr = q;
    if (!!(length)) {
        memcpy((*q).ptr,(*p).ptr,(u64)qq_tables$ttsize[((i64)(*p).usertag)]);
    }
;
}

i64 qq_arrays$var_equal_array(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
        i64 length;
        i64 elemsize;
    p = (*a).objptr;
    q = (*b).objptr;
    elemsize = (i64)(*p).elemtag;
    if (((i64)(*p).elemtag != (i64)(*q).elemtag)) {
        return (i64)0;
    }
;
    length = (*p).length;
    if ((length != (*q).length)) {
        return (i64)0;
    }
;
    if ((length == (i64)0)) {
        return (i64)1;
    }
;
    return mlib$eqbytes((*p).ptr,(*q).ptr,(qq_tables$ttsize[((i64)(*p).elemtag)] * length));
}

void qq_arrays$var_concatto_array(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        byte *  d;
        i64 alen;
        i64 blen;
        i64 newlen;
        i64 elemsize;
        struct qq_decls$objrec *  pa;
        struct qq_decls$objrec *  pb;
    pa = (*a).objptr;
    pb = (*b).objptr;
    if (!(!!(msysc$m_getdotindex((i64)(*pa).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    if (((i64)(*pa).elemtag != (i64)(*pb).elemtag)) {
        qq_runaux$pcerror((byte*)"concat/not compat",(byte*)"");
    }
;
    elemsize = qq_tables$ttsize[((i64)(*pa).elemtag)];
    alen = (*pa).length;
    blen = (*pb).length;
    if ((alen == (i64)0)) {
        if (!!(blen)) {
            qq_arrays$obj_resize_array(pa,blen);
            d = (*pa).ptr;
            memcpy(d,(*pb).ptr,(u64)(blen * elemsize));
        }
;
    }
    else if (!!(blen)) {
        newlen = (alen + blen);
        qq_arrays$obj_resize_array(pa,newlen);
        d = ((*pa).ptr + (alen * elemsize));
        memcpy(d,(*pb).ptr,(u64)(blen * elemsize));
    }
;
}

void qq_arrays$var_getslice_array(struct qq_decls$varrec *a,i64 i,i64 j) {
        i64 alower;
        i64 elemsize;
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
    p = (*a).objptr;
    alower = (i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0);
    elemsize = qq_tables$ttsize[((i64)(*p).elemtag)];
    if ((((i < alower) || (j > (((*p).length + alower) - (i64)1))) || (i > j))) {
        qq_runaux$pcerror((byte*)"array/slice bounds",(byte*)"");
    }
;
    q = qq_vars$obj_new();
    (*q).objtype = (i64)1;
    (*q).flags = msysc$m_setdotindex((*q).flags,(i64)1,msysc$m_getdotindex((i64)(*p).flags,(i64)1));
    (*q).flags = msysc$m_setdotindex((*q).flags,(i64)0,(u64)1u);
    (*q).ptr = ((*p).ptr + ((i - alower) * elemsize));
    (*q).elemtag = (i64)(*p).elemtag;
        {i64 $temp = (i64)(*p).objtype;
if (($temp==(i64)1)) {
        (*q).objptr2 = (*p).objptr2;
        qq_vars$obj_shareu((*q).objptr2);
    }
    else if (($temp==(i64)2)) {
        (*q).objptr2 = 0;
        (*q).objtype = (i64)2;
    }
    else {
        (*q).objptr2 = p;
        ++((*p).refcount);
    }
    };
    (*q).length = ((j - i) + (i64)1);
    (*a).objptr = q;
}

static i64 qq_arrays$u8inarray(byte a,struct qq_decls$objrec *p) {
        i64 i;
        byte *  q;
        i64 $av_1;
    i = (i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0);
    q = (*p).ptr;
    $av_1 = (*p).length;
    while ($av_1-- > 0) {
L52 :;
        if (((i64)(*q) == (i64)a)) {
            return i;
        }
;
        ++(q);
        ++(i);
L53 :;
    }
L54 :;
    ;
    return (i64)(-9223372036854775807-1);
}

i64 qq_arrays$u16inarray(u16 a,struct qq_decls$objrec *p) {
        i64 i;
        u16 *  q;
        i64 $av_1;
    i = (i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0);
    q = (u16 *)(*p).ptr;
    $av_1 = (*p).length;
    while ($av_1-- > 0) {
L55 :;
        if (((i64)(*q) == (i64)a)) {
            return i;
        }
;
        ++(q);
        ++(i);
L56 :;
    }
L57 :;
    ;
    return (i64)(-9223372036854775807-1);
}

i64 qq_arrays$u32inarray(u32 a,struct qq_decls$objrec *p) {
        i64 i;
        u32 *  q;
        i64 $av_1;
    i = (i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0);
    q = (u32 *)(*p).ptr;
    $av_1 = (*p).length;
    while ($av_1-- > 0) {
L58 :;
        if (((i64)(*q) == (i64)a)) {
            return i;
        }
;
        ++(q);
        ++(i);
L59 :;
    }
L60 :;
    ;
    return (i64)(-9223372036854775807-1);
}

i64 qq_arrays$u64inarray(u64 a,struct qq_decls$objrec *p) {
        i64 i;
        u64 *  q;
        i64 $av_1;
    i = (i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0);
    q = (u64 *)(*p).ptr;
    $av_1 = (*p).length;
    while ($av_1-- > 0) {
L61 :;
        if (((*q) == a)) {
            return i;
        }
;
        ++(q);
        ++(i);
L62 :;
    }
L63 :;
    ;
    return (i64)(-9223372036854775807-1);
}

i64 qq_arrays$var_inx_array(struct qq_decls$varrec *a,struct qq_decls$varrec *b,i64 usertag) {
        i64 n;
        struct qq_decls$objrec *  q;
        i64 elemtag;
    q = (*b).objptr;
    if (!!(usertag)) {
        elemtag = (i64)qq_tables$tttarget[(usertag)];
    }
    else {
        elemtag = (i64)(*q).elemtag;
    }
;
    if ((elemtag==(i64)23) || (elemtag==(i64)27)) {
        n = qq_arrays$u8inarray((byte)(*a).value,q);
    }
    else if ((elemtag==(i64)24) || (elemtag==(i64)28)) {
        n = qq_arrays$u16inarray((u16)(*a).value,q);
    }
    else if ((elemtag==(i64)25) || (elemtag==(i64)29)) {
        n = qq_arrays$u32inarray((u32)(*a).value,q);
    }
    else if ((elemtag==(i64)26) || (elemtag==(i64)30)) {
        n = qq_arrays$u64inarray((u64)(*a).value,q);
    }
    else {
        qq_runaux$pcustype((byte*)"x in array",b);
    }
;
    return n;
}

void qq_arrays$var_expand_array(struct qq_decls$varrec *p,struct qq_decls$varrec *dest,i64 m) {
        byte *  q;
        i64 n;
        i64 elemtype;
        i64 length;
        struct qq_decls$objrec *  pa;
        i64 $av_1;
    pa = (*p).objptr;
    if (((i64)(*p).tag == (i64)11)) {
        length = (*pa).length;
        elemtype = (i64)(*pa).elemtag;
    }
    else {
        length = qq_tables$ttlength[((i64)(*pa).usertag)];
        elemtype = (i64)qq_tables$tttarget[((i64)(*pa).usertag)];
    }
;
    q = (*pa).ptr;
    n = (i64)1;
    $av_1 = m;
    while ($av_1-- > 0) {
L64 :;
        if ((n > length)) {
            (*dest).tagx = (i64)0;
        }
        else {
            qq_packed$var_loadpacked(q,elemtype,dest,0);
            q += qq_tables$ttsize[(elemtype)];
        }
;
        ++(n);
        --(dest);
L65 :;
    }
L66 :;
    ;
}

// START
void qq_arrays$start(void) {

}

void qq_bits$obj_free_bits(struct qq_decls$objrec *p,i64 tag) {
    if (!!((*p).length)) {
        mlib$pcm_free((*p).ptr,qq_bits$getbitssize((*p).alloc64,(i64)(*p).elemtag));
    }
;
    mlib$pcm_free32(p);
}

void qq_bits$var_make_bits(struct qq_decls$varrec *a,struct qq_decls$varrec *dest,i64 lower,i64 n,i64 bxtype,i64 elemtype) {
        struct qq_decls$objrec *  p;
        byte *  q;
        i64 bitwidthx;
        i64 offset;
        i64 $av_1;
    p = qq_bits$obj_newbits(elemtype,lower,n);
    q = (*p).ptr;
    bitwidthx = (i64)qq_tables$ttbitwidth[(elemtype)];
    offset = (i64)0;
    $av_1 = n;
    while ($av_1-- > 0) {
L67 :;
        qq_vars$var_storebit(q,offset,a,elemtype,bitwidthx);
        offset += bitwidthx;
        if ((offset >= (i64)8)) {
            ++(q);
            offset = (i64)0;
        }
;
        ++(a);
L68 :;
    }
L69 :;
    ;
    (*dest).tagx = (bxtype | (i64)256);
    (*dest).objptr = p;
}

struct qq_decls$objrec *qq_bits$obj_newbits(i64 elemtype,i64 lower,i64 length) {
        struct qq_decls$objrec *  p;
        i64 nbytes;
    p = qq_vars$obj_new();
    (*p).flags = msysc$m_setdotindex((*p).flags,(i64)1,(u64)1u);
    (*p).flags = msysc$m_setdotindex((*p).flags,(i64)0,(u64)lower);
    (*p).length = length;
    (*p).objtype = (i64)0;
    (*p).elemtag = elemtype;
    if (!!(length)) {
        nbytes = qq_bits$getbitssize(length,elemtype);
        (*p).ptr = (byte *)mlib$pcm_allocz(nbytes);
        (*p).alloc64 = (mlib$allocbytes * ((i64)8 / (i64)qq_tables$ttbitwidth[(elemtype)]));
    }
;
    return p;
}

void qq_bits$var_getix_bits(struct qq_decls$varrec *a,i64 index) {
        struct qq_decls$objrec *  p;
        byte *  q;
        i64 elemtype;
        i64 shift;
    p = (*a).objptr;
    elemtype = (i64)(*p).elemtag;
    index -= (i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0);
    if (((u64)index >= (u64)(*p).length)) {
        qq_runaux$pcerror((byte*)"ax[int] bounds",(byte*)"");
    }
;
    q = (*p).ptr;
    (*a).tagx = (i64)1;
    index += (i64)(*p).indexoffset;
        {i64 $temp = (i64)(*p).elemtag;
if (($temp==(i64)33)) {
        (*a).value = (i64)!!(((i64)(*(q + (index >> (i64)3))) & ((i64)1 << (index & (i64)7))));
    }
    else if (($temp==(i64)34)) {
        shift = ((index & (i64)3) * (i64)2);
        (*a).value = (((i64)(*(q + (index >> (i64)2))) & ((i64)3 << shift)) >> shift);
    }
    else if (($temp==(i64)35)) {
        shift = ((index & (i64)1) * (i64)4);
        (*a).value = (((i64)(*(q + (index >> (i64)1))) & ((i64)15 << shift)) >> shift);
    }
    else {
        qq_runaux$pcustype_t((byte*)"bitix",(i64)(*p).elemtag);
    }
    };
}

void qq_bits$var_putix_bits(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *x) {
        struct qq_decls$objrec *  p;
        byte *  q;
        i64 elemtype;
        i64 newoffset;
    p = (*a).objptr;
    elemtype = (i64)(*p).elemtag;
    index -= (i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0);
    if (((u64)index >= (u64)(*p).length)) {
        if ((index < (i64)0)) {
            qq_runaux$pcerror((byte*)"lwb",(byte*)"");
        }
        else if ((index == (*p).length)) {
            qq_bits$obj_append_bits(p,x);
        }
        else {
            qq_runaux$pcerror((byte*)"bx[i]:=x bounds",(byte*)"");
        }
;
    }
;
    q = qq_bits$getindexoffset((*p).ptr,(i64)(*p).indexoffset,index,elemtype,&newoffset);
    qq_vars$var_storebit(q,(newoffset * (i64)qq_tables$ttbitwidth[(elemtype)]),x,elemtype,(i64)0);
}

void qq_bits$var_getixref_bits(struct qq_decls$varrec *a,i64 index) {
        struct qq_decls$objrec *  p;
        byte *  q;
        i64 newoffset;
        i64 elemtype;
    p = (*a).objptr;
    elemtype = (i64)(*p).elemtag;
    index -= (i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0);
    if (((u64)index >= (u64)(*p).length)) {
        qq_runaux$pcerror((byte*)"&bx[i] bounds",(byte*)"");
    }
;
    q = qq_bits$getindexoffset((*p).ptr,(i64)(*p).indexoffset,index,elemtype,&newoffset);
    (*a).tagx = (i64)15;
    (*a).elemtag = elemtype;
    (*a).ptr = q;
    (*a).bitoffset = (newoffset * (i64)qq_tables$ttbitwidth[(elemtype)]);
}

static byte *qq_bits$getindexoffset(byte *p,i64 offset,i64 index,i64 t,i64 *newoffset) {
    index += offset;
    if ((t==(i64)33)) {
        p += (index >> (i64)3);
        (*newoffset) = (index & (i64)7);
    }
    else if ((t==(i64)34)) {
        p += (index >> (i64)2);
        (*newoffset) = (index & (i64)3);
    }
    else if ((t==(i64)35)) {
        index += (offset >> (i64)2);
        p += (index >> (i64)1);
        (*newoffset) = (index & (i64)1);
    }
;
    return p;
}

static void qq_bits$obj_append_bits(struct qq_decls$objrec *a,struct qq_decls$varrec *x) {
        i64 n;
        i64 newoffset;
        i64 elemtype;
        byte *  q;
    if (((i64)(*a).objtype != (i64)0)) {
        qq_runaux$pcerror((byte*)"Can't extend slice",(byte*)"");
    }
;
    if (!(!!(msysc$m_getdotindex((i64)(*a).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    n = ((*a).length + (i64)1);
    elemtype = (i64)(*a).elemtag;
    if ((n > (*a).alloc64)) {
        qq_bits$obj_resize_bits(a,n);
    }
    else {
        (*a).length = n;
    }
;
    q = qq_bits$getindexoffset((*a).ptr,(i64)(*a).indexoffset,(n - (i64)msysc$m_getdotindex((i64)(*a).flags,(i64)0)),elemtype,&newoffset);
    qq_vars$var_storebit(q,(newoffset * (i64)qq_tables$ttbitwidth[(elemtype)]),x,elemtype,(i64)0);
}

void qq_bits$var_appendto_bits(struct qq_decls$varrec *a,struct qq_decls$varrec *x) {
    qq_bits$obj_append_bits((*a).objptr,x);
}

void qq_bits$obj_resize_bits(struct qq_decls$objrec *p,i64 n) {
        byte *  q;
        i64 newsize;
        i64 elemtype;
    elemtype = (i64)(*p).elemtag;
    if ((n <= (*p).alloc64)) {
        (*p).length = n;
    }
    else {
        newsize = qq_bits$getbitssize(n,elemtype);
        q = (byte *)mlib$pcm_alloc(newsize);
        if (!!((*p).length)) {
            memcpy(q,(*p).ptr,(u64)qq_bits$bits_bytesize(p));
            mlib$pcm_free((*p).ptr,qq_bits$getbitssize((*p).alloc64,elemtype));
        }
;
        (*p).ptr = q;
        (*p).length = n;
        (*p).alloc64 = (mlib$allocbytes * ((i64)8 / (i64)qq_tables$ttbitwidth[(elemtype)]));
    }
;
}

void qq_bits$var_dupl_bits(struct qq_decls$varrec *a) {
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
    p = (*a).objptr;
    q = qq_bits$obj_newbits((i64)(*p).elemtag,(i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0),(*p).length);
    (*q).indexoffset = (i64)(*p).indexoffset;
    (*a).objptr = q;
    if (!!((*p).length)) {
        memcpy((*q).ptr,(*p).ptr,(u64)qq_bits$bits_bytesize(p));
    }
;
}

i64 qq_bits$var_equal_bits(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
        i64 length;
        i64 elemsize;
    p = (*a).objptr;
    q = (*b).objptr;
    elemsize = (i64)(*p).elemtag;
    if (((i64)(*p).elemtag != (i64)(*q).elemtag)) {
        return (i64)0;
    }
;
    length = (*p).length;
    if ((length != (*q).length)) {
        return (i64)0;
    }
;
    if ((length == (i64)0)) {
        return (i64)1;
    }
;
    return mlib$eqbytes((*p).ptr,(*q).ptr,qq_bits$bits_bytesize(p));
}

void qq_bits$var_concatto_bits(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        byte *  d;
        i64 alen;
        i64 blen;
        i64 newlen;
        i64 elemsize;
        struct qq_decls$objrec *  pa;
        struct qq_decls$objrec *  pb;
    qq_runaux$pcerror((byte*)"VAR/BITS/NOT READY",(byte*)"var_concatto_bits");
    pa = (*a).objptr;
    pb = (*b).objptr;
    if (!(!!(msysc$m_getdotindex((i64)(*pa).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    if (((i64)(*pa).elemtag != (i64)(*pb).elemtag)) {
        qq_runaux$pcerror((byte*)"concat/not compat",(byte*)"");
    }
;
    elemsize = qq_tables$ttsize[((i64)(*pa).elemtag)];
    alen = (*pa).length;
    blen = (*pb).length;
    if ((alen == (i64)0)) {
        if (!!(blen)) {
            qq_bits$obj_resize_bits(pa,blen);
            d = (*pa).ptr;
            memcpy(d,(*pb).ptr,(u64)(blen * elemsize));
        }
;
    }
    else if (!!(blen)) {
        newlen = (alen + blen);
        qq_bits$obj_resize_bits(pa,newlen);
        d = ((*pa).ptr + (alen * elemsize));
        memcpy(d,(*pb).ptr,(u64)(blen * elemsize));
    }
;
}

void qq_bits$var_getslice_bits(struct qq_decls$varrec *a,i64 i,i64 j) {
        i64 alower;
        i64 elemtype;
        i64 newoffset;
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
    p = (*a).objptr;
    alower = (i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0);
    elemtype = (i64)(*p).elemtag;
    if ((((i < alower) || (j > (((*p).length + alower) - (i64)1))) || (i > j))) {
        qq_runaux$pcerror((byte*)"bits/slice bounds",(byte*)"");
    }
;
    q = qq_vars$obj_new();
    (*q).objtype = (i64)1;
    (*q).flags = msysc$m_setdotindex((*q).flags,(i64)1,msysc$m_getdotindex((i64)(*p).flags,(i64)1));
    (*q).flags = msysc$m_setdotindex((*q).flags,(i64)0,(u64)1u);
    (*q).elemtag = elemtype;
    (*q).ptr = qq_bits$getindexoffset((*p).ptr,(i64)(*p).indexoffset,(i - alower),elemtype,&newoffset);
    (*q).indexoffset = newoffset;
        {i64 $temp = (i64)(*p).objtype;
if (($temp==(i64)1)) {
        (*q).objptr2 = (*p).objptr2;
        qq_vars$obj_shareu((*q).objptr2);
    }
    else if (($temp==(i64)2)) {
        (*q).objptr2 = 0;
        (*q).objtype = (i64)2;
    }
    else {
        (*q).objptr2 = p;
        ++((*p).refcount);
    }
    };
    (*q).length = ((j - i) + (i64)1);
    (*a).objptr = q;
}

i64 qq_bits$bits_bytesize(struct qq_decls$objrec *p) {
    return qq_bits$getbitssize((*p).length,(i64)(*p).elemtag);
}

i64 qq_bits$getbitssize(i64 n,i64 t) {
        i64 nbits;
    nbits = (n * (i64)qq_tables$ttbitwidth[(t)]);
    return ((((nbits - (i64)1) / (i64)64) + (i64)1) * (i64)8);
}

// START
void qq_bits$start(void) {

}

void qq_calldll$calldll(struct qq_decls$strec *d,struct qq_decls$varrec *args,struct qq_decls$varrec *result,i64 nargs) {
        struct qq_decls$strec *  e;
        i64 arglist[100];
        i64 n;
        i64 retcode;
        i64 retval;
        void (*fnaddr)(void);
        i64 i;
    if ((nargs > (i64)100)) {
        qq_runaux$pcerror((byte*)"Too many dll args",(byte*)"");
    }
;
    e = (*d).deflist;
    n = (i64)0;
    for (i=(i64)1;i<=nargs;++i) {
L70 :;
        if ((e == 0)) {
            if (!!(msysc$m_getdotindex((i64)(*d).flags,(i64)8))) {
                arglist[(i)-1] = (i64)qq_calldll$vartopacked(args,0);
                ++(args);
            }
            else {
                qq_runaux$pcerror((byte*)"Too many dll args",(byte*)"");
            }
;
        }
        else {
            arglist[(i)-1] = (i64)qq_calldll$vartopacked(args,e);
            ++(args);
            e = (*e).nextdef;
        }
;
L71 :;
    }
L72 :;
    ;
    if (((i64)(*d).mode == (i64)32)) {
        retcode = (i64)82;
    }
    else {
        retcode = (i64)73;
    }
;
    fnaddr = (void (*)(void))qq_calldll$getlibprocaddr(d);
    retval = (i64)mwindllc$os_calldllfunction((void (*)(void))fnaddr,retcode,nargs,(i64 (*)[])&arglist,0);
    if (!!((i64)(*d).mode)) {
        qq_calldll$packedtovar((u64)retval,(i64)(*d).mode,result);
    }
;
}

static void (*qq_calldll$getlibprocaddr(struct qq_decls$strec *d))(void) {
        void (*fnaddr)(void);
    fnaddr = (void (*)(void))qq_decls$dllprocaddr[((i64)(*d).index)-1];
    if ((fnaddr == 0)) {
        fnaddr = (void (*)(void))qq_calldll$loaddllfunction(d);
    }
;
    return fnaddr;
}

static u64 qq_calldll$vartopacked(struct qq_decls$varrec *p,struct qq_decls$strec *d) {
        i64 s;
        i64 t;
        struct qq_decls$objrec *  a;
    s = (i64)(*p).tag;
    if ((d == 0)) {
        if ((s==(i64)9)) {
            a = (*p).objptr;
            return (u64)qq_lib$convtostringz((*a).strptr,(*a).length);
        }
        else if ((s==(i64)1) || (s==(i64)2) || (s==(i64)16)) {
            return (u64)(*p).value;
        }
        else {
            qq_runaux$pcerror((byte*)"Bad variadic param",(byte*)"");
        }
;
    }
;
    t = (i64)(*d).mode;
        {i64 $temp = (i64)qq_tables$ttbasetype[(t)];
if (($temp==(i64)26) || ($temp==(i64)30) || ($temp==(i64)25) || ($temp==(i64)29) || ($temp==(i64)24) || ($temp==(i64)28)) {
        if ((s==(i64)1) || (s==(i64)16) || (s==(i64)14)) {
            return (u64)(*p).value;
        }
        else if ((s==(i64)2)) {
            return (u64)(i64)(*p).xvalue;
        }
        else {
            //error:
L73 :;
;
            msysc$m_print_startcon();
            msysc$m_print_setfmt((byte*)"'#' should be '#' (param # #)");
            msysc$m_print_str(qq_show$strmode(s,(i64)1),NULL);
            msysc$m_print_str(qq_show$strmode(t,(i64)0),NULL);
            msysc$m_print_str((*d).name,NULL);
            msysc$m_print_i64((i64)(*d).index,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            qq_runaux$pcerror((byte*)"DLL: wrong param type",(byte*)"");
        }
;
    }
    else if (($temp==(i64)32)) {
        if ((s==(i64)1)) {
            return (u64)msysc$m_tp_r64toi64((r64)(*p).value);
        }
        else if ((s==(i64)2)) {
            return (u64)msysc$m_tp_r64toi64((*p).xvalue);
        }
        else {
            goto L73 ;
;
        }
;
    }
    else if (($temp==(i64)38)) {
        if ((s==(i64)9)) {
            a = (*p).objptr;
            return (u64)qq_lib$convtostringz((*a).strptr,(*a).length);
        }
        else if ((s==(i64)16)) {
            return (u64)(*p).ptr;
        }
        else {
            goto L73 ;
;
        }
;
    }
    else if (($temp==(i64)16)) {
        if ((s==(i64)16)) {
            return (u64)(*p).ptr;
        }
        else if ((s==(i64)11) || (s==(i64)7)) {
            return (u64)(*(*p).objptr).ptr;
        }
        else {
            goto L73 ;
;
        }
;
    }
    else if (($temp==(i64)40)) {
        return (u64)((byte *)(*p).objptr + (i64)8);
    }
    else {
        qq_runaux$pcmxtypestt((byte*)"DLL params:",s,t);
    }
    };
    return (u64)0u;
}

static void qq_calldll$packedtovar(u64 retval,i64 t,struct qq_decls$varrec *dest) {
        i64 tbase;
    tbase = (i64)qq_tables$ttbasetype[(t)];
    if ((tbase==(i64)0)) {
    }
    else if ((tbase==(i64)32)) {
        (*dest).tagx = (i64)2;
        (*dest).xvalue = *(r64*)&retval;
    }
    else if ((tbase==(i64)31)) {
        qq_runaux$pcerror((byte*)"dll/r32ret",(byte*)"");
    }
    else if ((tbase==(i64)26) || (tbase==(i64)30)) {
        (*dest).tagx = (i64)1;
        (*dest).value = (i64)retval;
    }
    else if ((tbase==(i64)25)) {
        (*dest).tagx = (i64)1;
        (*dest).value = (i64)(i32)retval;
    }
    else if ((tbase==(i64)29)) {
        (*dest).tagx = (i64)1;
        (*dest).value = (i64)(u32)retval;
    }
    else if ((tbase==(i64)24)) {
        (*dest).tagx = (i64)1;
        (*dest).value = (i64)(i16)retval;
    }
    else if ((tbase==(i64)28)) {
        (*dest).tagx = (i64)1;
        (*dest).value = (i64)(u16)retval;
    }
    else if ((tbase==(i64)16)) {
        (*dest).tagx = (i64)16;
        (*dest).ptr = (byte *)retval;
        (*dest).elemtag = (i64)qq_tables$tttarget[(t)];
    }
    else if ((tbase==(i64)38)) {
        if (!!(retval)) {
            qq_strings$var_make_string((u8 *)retval,dest,(i64)0);
        }
        else {
            qq_strings$var_make_string((byte*)"",dest,(i64)0);
        }
;
    }
    else {
        qq_runaux$pcerror((byte*)"Rettype not supported:",qq_tables$ttname[(t)]);
    }
;
}

static void (*qq_calldll$loaddllfunction(struct qq_decls$strec *d))(void) {
        i64 fnindex;
        i64 libindex;
        u64 dllinst;
        void (*fnaddr)(void);
        u8 *  name;
    fnindex = (i64)(*d).index;
    fnaddr = (void (*)(void))qq_decls$dllprocaddr[(fnindex)-1];
    if (!!(fnaddr)) {
        return (void (*)(void))fnaddr;
    }
;
    libindex = (i64)qq_decls$dllproclibindex[(fnindex)-1];
    dllinst = qq_decls$dllinsttable[(libindex)-1];
    if (((i64)dllinst == (i64)0)) {
        dllinst = mlinux$os_getdllinst((*qq_decls$libtable[(libindex)-1]).name);
        if (((i64)dllinst == (i64)0)) {
            qq_runaux$pcerror((byte*)"Can't load DLL:",(*qq_decls$libtable[(libindex)-1]).name);
        }
;
        qq_decls$dllinsttable[(libindex)-1] = dllinst;
    }
;
    name = (!!((*d).truename) ? (*d).truename : (*d).name);
    fnaddr = (void (*)(void))mlinux$os_getdllprocaddr((i64)dllinst,name);
    if ((fnaddr == 0)) {
        qq_runaux$pcerror((byte*)"Can't find DLL func:",name);
    }
;
    qq_decls$dllprocaddr[(fnindex)-1] = fnaddr;
    return (void (*)(void))fnaddr;
}

// START
void qq_calldll$start(void) {

}

// START
void qq_decls$start(void) {

}

void qq_decimal$obj_free_dec(struct qq_decls$objrec *p) {
    if (!!((*p).length)) {
        mlib$pcm_free((*p).num,((*p).length * (i64)4));
    }
;
    mlib$pcm_free32(p);
}

void qq_decimal$var_dupl_dec(struct qq_decls$varrec *a) {
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
        i64 size;
    q = (*a).objptr;
    p = qq_vars$obj_new();
    (*p).length = (*q).length;
    (*p).expon = (i64)(*q).expon;
    (*p).neg = (i64)(*q).neg;
    (*p).numtype = (i64)(*q).numtype;
    size = ((*q).length * (i64)4);
    if (!!(size)) {
        (*p).num = (i32 (*)[])mlib$pcm_alloc(size);
        memcpy((*p).num,(*q).num,(u64)size);
    }
;
    (*a).objptr = p;
}

void qq_decimal$var_empty_dec(struct qq_decls$varrec *dest) {
    (*dest).tagx = (i64)259;
    (*dest).objptr = qq_decimal$makebignum((i64)0);
}

void qq_decimal$var_make_dec_str(u8 *s,i64 length,struct qq_decls$varrec *dest) {
    (*dest).tagx = (i64)259;
    (*dest).objptr = qq_decimal$bn_makestr(s,length);
}

void qq_decimal$var_make_dec_int(i64 a,struct qq_decls$varrec *dest) {
    (*dest).tagx = (i64)259;
    (*dest).objptr = qq_decimal$bn_makeint(a);
}

static struct qq_decls$objrec *qq_decimal$badnumber(void) {
        struct qq_decls$objrec *  c;
    c = qq_decimal$makebignum((i64)0);
    (*c).numtype = (i64)3;
    return c;
}

static struct qq_decls$objrec *qq_decimal$bn_makestr(u8 *s,i64 length) {
        u8 *  t;
        u8 *  u;
        u8 *  oldt;
        i64 tlength;
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
        i64 c;
        struct qq_decls$objrec *  a;
        i64 $av_1;
        i64 $av_2;
        i64 i;
    if ((length == (i64)0)) {
        length = strlen(s);
    }
;
    if ((length <= (i64)0)) {
        return qq_decimal$badnumber();
    }
;
    t = (u8 *)malloc((u64)(length + (i64)1));
    memcpy((void *)t,(void *)s,(u64)length);
    (*(t + length)) = (u64)0u;
    oldt = t;
    tlength = (length + (i64)1);
    s = t;
    talloc = ((length + (i64)1) + (i64)10);
    neg = (i64)0;
        {u64 $temp = (u64)(*s);
if (($temp=='+')) {
        ++(s);
    }
    else if (($temp=='-')) {
        neg = (i64)1;
        ++(s);
    }
    };
    t = (u = (u8 *)mlib$pcm_alloc(talloc));
    dpindex = (i64)-1;
    dpseen = (zerosafterdp = (i64)0);
    nonzeros = (i64)0;
    leadingzeros = (trailingzeros = (i64)0);
    expon = (i64)0;
    L74 :;
    while (1) {
        if ((($rtemp=(c = (i64)(u64)(*s)), $rtemp >= (i64)49 && $rtemp <= (i64)57))) {
            (*(u)++) = (u64)(*(s)++);
            trailingzeros = (i64)0;
            nonzeros = (i64)1;
        }
        else {
            if ((c==(i64)48)) {
                if (!!(nonzeros)) {
                    ++(trailingzeros);
                    (*(u)++) = (u64)(*(s)++);
                }
                else {
                    ++(leadingzeros);
                    if (!!(dpseen)) {
                        ++(zerosafterdp);
                    }
;
                    ++(s);
                }
;
            }
            else if ((c==(i64)95) || (c==(i64)39) || (c==(i64)96) || (c==(i64)32) || (c==(i64)13) || (c==(i64)10)) {
                ++(s);
            }
            else if ((c==(i64)46)) {
                if ((!!(dpseen) || (dpindex >= (i64)0))) {
                    return qq_decimal$badnumber();
                }
;
                if (!!(nonzeros)) {
                    dpindex = (u - t);
                }
                else {
                    dpseen = (i64)1;
                }
;
                ++(s);
            }
            else if ((c==(i64)0)) {
                goto L75 ;
            }
            else if ((c==(i64)101) || (c==(i64)69)) {
                expon = qq_decimal$readexpon((s + (i64)1));
                goto L75 ;
            }
            else {
                return qq_decimal$badnumber();
            }
;
        }
;
    }
L75 :;
    ;
    (*u) = (u64)0u;
    length = (u - t);
    if ((dpindex < (i64)0)) {
        if (!!(dpseen)) {
            dpindex = -(zerosafterdp);
        }
        else {
            dpindex = length;
        }
;
    }
;
    length -= trailingzeros;
    (*(t + length)) = (u64)0u;
    if ((length == (i64)0)) {
        return qq_decimal$bn_makeint((i64)0);
    }
;
    d = ((dpindex - (i64)1) + expon);
    n = length;
    dp = (i64)0;
    na = (i64)1;
    nb = (n - na);
    w = (i64)9;
    if ((d >= (i64)0)) {
        wd = (d / w);
        wdp = (d % w);
    }
    else {
        d2 = m$llabs((d + (i64)1));
        wd = -(((d2 / w) + (i64)1));
        wdp = ((w - (i64)1) - (d2 % w));
    }
;
    na = (wdp + (i64)1);
    nb = msysc$m_imax((n - na),(i64)0);
    L76 :;
    while (!!((nb % w))) {
        ++(nb);
L77 :;
    }
L78 :;
    ;
    length = ((nb / w) + (i64)1);
    u = (t + n);
    $av_1 = ((na + nb) - n);
    while ($av_1-- > 0) {
L79 :;
        (*(u)++) = '0';
L80 :;
    }
L81 :;
    ;
    n = (na + nb);
    (*(t + n)) = (u64)0u;
    a = qq_decimal$makebignum(length);
    (*a).neg = neg;
    (*a).expon = wd;
    u = t;
    (*(*a).num)[((i64)0)] = qq_decimal$strvaln(u,na);
    u += na;
        ($av_2 = (length - (i64)1));
    for (i=(i64)1;i<=$av_2;++i) {
L82 :;
        (*(*a).num)[(i)] = qq_decimal$strvaln(u,w);
        u += w;
L83 :;
    }
L84 :;
    ;
    mlib$pcm_free((void *)t,talloc);
    free((void *)oldt);
    return a;
}

static i64 qq_decimal$readexpon(u8 *s) {
        i64 neg;
        i64 expon;
        i64 c;
    neg = (expon = (i64)0);
        {u64 $temp = (u64)(*s);
if (($temp=='+')) {
        ++(s);
    }
    else if (($temp=='-')) {
        neg = (i64)1;
        ++(s);
    }
    };
    L85 :;
    while (1) {
        if ((($rtemp=(c = (i64)(u64)(*s)), $rtemp >= (i64)48 && $rtemp <= (i64)57))) {
            expon = ((expon * (i64)10) + (i64)((u64)(*s) - '0'));
            ++(s);
        }
        else {
            if ((c==(i64)95) || (c==(i64)39) || (c==(i64)96) || (c==(i64)32)) {
                ++(s);
            }
            else if ((c==(i64)0)) {
                goto L86 ;
            }
            else {
                qq_runaux$pcerror((byte*)"make expon?",(byte*)"");
            }
;
        }
;
    }
L86 :;
    ;
    if (!!(neg)) {
        return -(expon);
    }
    else {
        return expon;
    }
;
}

static struct qq_decls$objrec *qq_decimal$bn_makeint(i64 x) {
        struct qq_decls$objrec *  a;
        u8 str[256];
    if ((x == (i64)0)) {
        a = qq_decimal$makebignum((i64)0);
    }
    else if ((($rtemp=x, $rtemp >= (i64)0 && $rtemp <= (i64)999999999))) {
        a = qq_decimal$makebignum((i64)1);
        (*(*a).num)[((i64)0)] = x;
    }
    else if ((($rtemp=-(x), $rtemp >= (i64)0 && $rtemp <= (i64)999999999))) {
        a = qq_decimal$makebignum((i64)1);
        (*(*a).num)[((i64)0)] = -(x);
        (*a).neg = (i64)1;
    }
    else {
        msysc$m_print_startstr(str);
        msysc$m_print_i64(x,NULL);
        msysc$m_print_end();
        ;
        a = qq_decimal$bn_makestr(str,(i64)0);
    }
;
    return a;
}

u8 *qq_decimal$var_tostr_dec(struct qq_decls$varrec *a,i64 fmt) {
    return qq_decimal$obj_tostr_dec((*a).objptr,fmt);
}

static u8 *qq_decimal$obj_tostr_dec(struct qq_decls$objrec *a,i64 fmt) {
        u8 *  s;
        u8 *  t;
    t = 0;
    if ((a == 0)) {
        t = (byte*)"<void>";
    }
    else {
                {i64 $temp = (i64)(*a).numtype;
if (($temp==(i64)0)) {
            t = (((fmt == (i64)69) || (fmt == (i64)70)) ? (byte*)"0.0" : (byte*)"0");
        }
        else if (($temp==(i64)2)) {
            t = (!!((i64)(*a).neg) ? (byte*)"-Infinity" : (byte*)"Infinity");
        }
        else if (($temp==(i64)3)) {
            t = (byte*)"<NaN>";
        }
        };
    }
;
    if (!!(t)) {
        s = (u8 *)mlib$pcm_alloc((qq_decimal$decstrsize = (strlen(t) + (i64)1)));
        strcpy(s,t);
        return s;
    }
;
    if (((fmt == (i64)0) || (fmt == (i64)65))) {
        if ((!!(qq_decimal$bn_isint(a)) && ((((i64)(*a).expon - (*a).length) * (i64)9) < (i64)60))) {
            fmt = (i64)73;
        }
        else if ((m$llabs(((i64)(*a).expon * (i64)9)) < (i64)60)) {
            fmt = (i64)70;
        }
        else {
            fmt = (i64)69;
        }
;
    }
;
    if ((fmt == (i64)69)) {
        s = qq_decimal$tostring_scient(a);
    }
    else {
        s = qq_decimal$tostring_float(a,fmt);
    }
;
    return s;
}

static u8 *qq_decimal$tostring_scient(struct qq_decls$objrec *a) {
        u8 *  s;
        u8 *  t;
        i64 expon;
        i64 nchars;
        i64 shift;
        i64 x;
        i64 scale;
        i64 $av_1;
        i64 i;
    nchars = (i64)3;
    expon = ((i64)(*a).expon * (i64)9);
    x = (i64)(*(*a).num)[((i64)0)];
    scale = (i64)1;
    shift = (i64)0;
    L87 :;
    while ((x >= (i64)10)) {
        x = (x / (i64)10);
        scale *= (i64)10;
        ++(expon);
        ++(shift);
L88 :;
    }
L89 :;
    ;
    nchars = (((*a).length * (i64)9) + (i64)16);
    s = (t = (u8 *)mlib$pcm_alloc((qq_decimal$decstrsize = nchars)));
    if (!!((i64)(*a).neg)) {
        (*(t)++) = '-';
    }
;
    msysc$m_print_startstr(t);
    msysc$m_print_i64(x,NULL);
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)".",NULL);
    msysc$m_print_end();
    ;
    t += strlen(t);
    if (!!(shift)) {
        msysc$m_print_startstr(t);
        msysc$m_print_i64(shift,(byte*)"v");
        msysc$m_print_nogap();
        msysc$m_print_i64(((i64)(*(*a).num)[((i64)0)] - (x * scale)),(byte*)"z*");
        msysc$m_print_end();
        ;
        t += strlen(t);
    }
;
        ($av_1 = ((*a).length - (i64)1));
    for (i=(i64)1;i<=$av_1;++i) {
L90 :;
        msysc$m_print_startstr(t);
        msysc$m_print_i64((i64)(*(*a).num)[(i)],(byte*)"z9");
        msysc$m_print_end();
        ;
        t += strlen(t);
L91 :;
    }
L92 :;
    ;
    L93 :;
    while ((((u64)(*(t - (i64)1)) == '0') && ((u64)(*(t - (i64)2)) != '.'))) {
        --(t);
L94 :;
    }
L95 :;
    ;
    msysc$m_print_startstr(t);
    msysc$m_print_str((byte*)"e",NULL);
    msysc$m_print_nogap();
    msysc$m_print_i64(expon,NULL);
    msysc$m_print_end();
    ;
    t += strlen(t);
    (*t) = (u64)0u;
    return s;
}

static u8 *qq_decimal$tostring_float(struct qq_decls$objrec *a,i64 fmt) {
        i64 expon;
        i64 upper;
        i64 nchars;
        i64 w;
        i64 prel;
        i64 showdot;
        u8 *  s;
        u8 *  t;
        i64 $av_1;
        i64 $av_2;
        i64 $av_3;
        i64 $av_4;
        i64 i;
    expon = (i64)(*a).expon;
    upper = ((*a).length - (i64)1);
    if (((fmt == (i64)73) && !!(qq_decimal$bn_isint(a)))) {
        showdot = (i64)0;
    }
    else {
        showdot = (i64)1;
    }
;
    w = (i64)9;
    nchars = (i64)3;
    if ((expon < (i64)0)) {
        nchars += (m$llabs((expon - (i64)1)) * w);
    }
;
    nchars += ((*a).length * w);
    if (((expon - upper) > (i64)0)) {
        nchars += ((expon - upper) * w);
    }
;
    nchars += (i64)8;
    s = (t = (u8 *)mlib$pcm_alloc((qq_decimal$decstrsize = nchars)));
    if (!!((i64)(*a).neg)) {
        (*(t)++) = '-';
    }
;
    prel = (i64)0;
    if ((expon < (i64)0)) {
        prel = (i64)1;
        (*(t)++) = '0';
        (*(t)++) = '.';
        $av_2 = (m$llabs(expon) - (i64)1);
        while ($av_2-- > 0) {
L96 :;
            $av_1 = (i64)9;
            while ($av_1-- > 0) {
L99 :;
                (*(t)++) = '0';
L100 :;
            }
L101 :;
            ;
L97 :;
        }
L98 :;
        ;
    }
;
    for (i=(i64)0;i<=upper;++i) {
L102 :;
        msysc$m_print_startstr(t);
        msysc$m_print_i64((i64)(*(*a).num)[(i)],(((i > (i64)0) || !!(prel)) ? (byte*)"z9" : (byte*)""));
        msysc$m_print_end();
        ;
        t += strlen(t);
        if ((((expon == i) && (i < upper)) && !!(showdot))) {
            (*(t)++) = '.';
        }
;
L103 :;
    }
L104 :;
    ;
    $av_4 = (expon - upper);
    while ($av_4-- > 0) {
L105 :;
        $av_3 = (i64)9;
        while ($av_3-- > 0) {
L108 :;
            (*(t)++) = '0';
L109 :;
        }
L110 :;
        ;
L106 :;
    }
L107 :;
    ;
    if (((expon >= upper) && !!(showdot))) {
        (*(t)++) = '.';
        (*(t)++) = '0';
    }
;
    (*t) = (u64)0u;
    return s;
}

static i64 qq_decimal$strvaln(u8 *s,i64 n) {
        i64 a;
        i64 $av_1;
    a = (i64)0;
    $av_1 = n;
    while ($av_1-- > 0) {
L111 :;
        if (((u64)(*s) != '_')) {
            a = (((a * (i64)10) + (i64)(u64)(*s)) - (i64)48);
        }
;
        ++(s);
L112 :;
    }
L113 :;
    ;
    return a;
}

static i64 qq_decimal$bn_isint(struct qq_decls$objrec *a) {
    return (i64)((*a).length <= ((i64)(*a).expon + (i64)1));
}

i64 qq_decimal$obj_len_dec(struct qq_decls$objrec *a) {
    return qq_decimal$bn_getprec(a);
    if (!(!!(qq_decimal$bn_isint(a)))) {
        return (i64)0;
    }
;
    if (!!(qq_decimal$bn_iszero(a))) {
        return (i64)1;
    }
;
    return (strlen(msysc$strint((i64)(*(*a).num)[((i64)0)],0)) + ((i64)(*a).expon * (i64)9));
}

i64 qq_decimal$bn_iszero(struct qq_decls$objrec *a) {
    return (i64)((i64)(*a).numtype == (i64)0);
}

i64 qq_decimal$var_equal_dec(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    return qq_decimal$bn_equal((*a).objptr,(*b).objptr);
}

void qq_decimal$var_add_dec(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        struct qq_decls$objrec *  dest;
    dest = qq_decimal$bn_init();
    qq_decimal$bn_add(dest,(*a).objptr,(*b).objptr);
    (*a).objptr = dest;
}

void qq_decimal$var_sub_dec(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        struct qq_decls$objrec *  dest;
    dest = qq_decimal$bn_init();
    qq_decimal$bn_sub(dest,(*a).objptr,(*b).objptr);
    (*a).objptr = dest;
}

void qq_decimal$var_mul_dec(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        struct qq_decls$objrec *  dest;
    dest = qq_decimal$bn_init();
    qq_decimal$bn_mul(dest,(*a).objptr,(*b).objptr);
    (*a).objptr = dest;
}

void qq_decimal$var_div_dec(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        struct qq_decls$objrec *  dest;
    dest = qq_decimal$bn_init();
    qq_decimal$bn_div(dest,(*a).objptr,(*b).objptr,(i64)0);
    (*a).objptr = dest;
}

void qq_decimal$var_idiv_dec(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        struct qq_decls$objrec *  dest;
    dest = qq_decimal$bn_init();
    qq_decimal$bn_idiv(dest,(*a).objptr,(*b).objptr);
    (*a).objptr = dest;
}

void qq_decimal$var_irem_dec(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        struct qq_decls$objrec *  dest;
    dest = qq_decimal$bn_init();
    qq_decimal$bn_irem(dest,(*a).objptr,(*b).objptr);
    (*a).objptr = dest;
}

void qq_decimal$var_neg_dec(struct qq_decls$varrec *a) {
    qq_decimal$bn_negto((*a).objptr);
}

void qq_decimal$var_abs_dec(struct qq_decls$varrec *a) {
    qq_decimal$bn_absto((*a).objptr);
}

i64 qq_decimal$var_compare_dec(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    return qq_decimal$bn_cmp((*a).objptr,(*b).objptr);
}

static i64 qq_decimal$bn_cmp(struct qq_decls$objrec *a,struct qq_decls$objrec *b) {
        struct qq_decls$objrec *  d;
        i64 neg;
    if (!!(qq_decimal$bn_equal(a,b))) {
        return (i64)0;
    }
;
    d = qq_decimal$bn_init();
    qq_decimal$bn_sub(d,a,b);
    neg = (i64)(*d).neg;
    qq_decimal$obj_free_dec(d);
    if (!!(neg)) {
        return (i64)-1;
    }
    else {
        return (i64)1;
    }
;
}

static i64 qq_decimal$bn_equal(struct qq_decls$objrec *a,struct qq_decls$objrec *b) {
    if ((((i64)(*a).numtype != (i64)1) && ((i64)(*a).numtype == (i64)(*b).numtype))) {
        return (i64)((i64)(*a).neg == (i64)(*b).neg);
    }
;
    if ((((((*a).length != (*b).length) || ((i64)(*a).numtype != (i64)(*b).numtype)) || ((i64)(*a).neg != (i64)(*b).neg)) || ((i64)(*a).expon != (i64)(*b).expon))) {
        return (i64)0;
    }
;
    if (((*a).length == (i64)0)) {
        return (i64)1;
    }
;
    return mlib$eqbytes((*a).num,(*b).num,((*a).length * (i64)4));
}

static i64 qq_decimal$bn_add(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b) {
        i64 nega;
        i64 negb;
        {i64 $temp = qq_decimal$getbintype(a,b);
if (($temp==(i64)1)) {
    }
    else if (($temp==(i64)2)) {
        qq_decimal$bn_setzero(dest);
        return (i64)1;
    }
    else if (($temp==(i64)5)) {
        qq_decimal$bn_dupl(dest,a);
        return (i64)1;
    }
    else if (($temp==(i64)7)) {
        qq_decimal$bn_dupl(dest,b);
        return (i64)1;
    }
    else {
        qq_decimal$bn_setnan(dest);
        return (i64)0;
    }
    };
    nega = (i64)(*a).neg;
    negb = (i64)(*b).neg;
    if ((!(!!(nega)) && !(!!(negb)))) {
        qq_decimal$bn_addu(dest,a,b);
    }
    else if ((!!(nega) && !!(negb))) {
        qq_decimal$bn_addu(dest,a,b);
        qq_decimal$bn_negto(dest);
    }
    else if ((!(!!(nega)) && !!(negb))) {
        qq_decimal$bn_subu(dest,a,b);
    }
    else {
        qq_decimal$bn_subu(dest,b,a);
    }
;
    return (i64)1;
}

static i64 qq_decimal$bn_sub(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b) {
        i64 nega;
        i64 negb;
        {i64 $temp = qq_decimal$getbintype(a,b);
if (($temp==(i64)1)) {
    }
    else if (($temp==(i64)2)) {
        qq_decimal$bn_setzero(dest);
        return (i64)1;
    }
    else if (($temp==(i64)5)) {
        qq_decimal$bn_dupl(dest,a);
        return (i64)1;
    }
    else if (($temp==(i64)7)) {
        qq_decimal$bn_dupl(dest,b);
        qq_decimal$bn_negto(dest);
        return (i64)1;
    }
    else {
        qq_decimal$bn_setnan(dest);
        return (i64)0;
    }
    };
    nega = (i64)(*a).neg;
    negb = (i64)(*b).neg;
    if ((!(!!(nega)) && !(!!(negb)))) {
        qq_decimal$bn_subu(dest,a,b);
    }
    else if ((!!(nega) && !!(negb))) {
        qq_decimal$bn_subu(dest,b,a);
    }
    else if ((!(!!(nega)) && !!(negb))) {
        qq_decimal$bn_addu(dest,a,b);
    }
    else {
        qq_decimal$bn_dupl(dest,b);
        qq_decimal$bn_negto(dest);
        qq_decimal$bn_add(dest,a,dest);
    }
;
    return (i64)1;
}

static void qq_decimal$bn_addu(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b) {
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
    if (((i64)(*a).expon < (i64)(*b).expon)) {
        {struct qq_decls$objrec *  temp = a; a = b; b = temp; };
    }
;
    expona = (i64)(*a).expon;
    exponb = (i64)(*b).expon;
    preca = (*a).length;
    precb = (*b).length;
    offset = (expona - exponb);
    uppera = (preca - (i64)1);
    upperb = (precb - (i64)1);
    if ((uppera > (upperb + offset))) {
        upperc = uppera;
    }
    else {
        upperc = (upperb + offset);
    }
;
    precc = (upperc + (i64)1);
    c = (i32 *)qq_decimal$makesmallnum(precc);
    carry = (i64)0;
    pa = (i32 (*)[])(*a).num;
    pb = (i32 (*)[])(*b).num;
    for (i=upperc;i>=(i64)0;--i) {
L114 :;
        j = (u64)(i - offset);
        if (((i <= uppera) && (j <= (u64)upperb))) {
            dc = (((i64)(*pa)[(i)] + (i64)(*pb)[((i64)j)]) + carry);
        }
        else if ((i <= uppera)) {
            dc = ((i64)(*pa)[(i)] + carry);
        }
        else if ((j <= (u64)upperb)) {
            dc = ((i64)(*pb)[((i64)j)] + carry);
        }
        else {
            dc = carry;
        }
;
        if ((dc >= (i64)1000000000)) {
            carry = (i64)1;
            (*(c + i)) = (dc - (i64)1000000000);
        }
        else {
            (*(c + i)) = dc;
            carry = (i64)0;
        }
;
L115 :;
    }
L116 :;
    ;
    if (!!(carry)) {
        c2 = (i32 *)qq_decimal$makesmallnum((precc + (i64)1));
        (*c2) = carry;
        memcpy((c2 + (i64)1),c,(u64)(precc * (i64)4));
        qq_decimal$freesmall((i32 *)c,precc);
        c = c2;
        ++(precc);
    }
;
    qq_decimal$smalltobig(dest,(i32 *)c,precc,precc,(i64)0);
    (*dest).expon = (expona + carry);
}

static void qq_decimal$bn_subu(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b) {
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
    if (((i64)(*a).expon < (i64)(*b).expon)) {
        {struct qq_decls$objrec *  temp = a; a = b; b = temp; };
        isneg = (i64)1;
    }
;
    //retry:
L117 :;
;
    expona = (i64)(*a).expon;
    preca = (*a).length;
    precb = (*b).length;
    offset = (expona - (i64)(*b).expon);
    uppera = (preca - (i64)1);
    upperb = (precb - (i64)1);
    if ((uppera > (upperb + offset))) {
        upperc = uppera;
    }
    else {
        upperc = (upperb + offset);
    }
;
    precc = (upperc + (i64)1);
    c = (i32 *)qq_decimal$makesmallnum(precc);
    carry = (i64)0;
    pa = (i32 (*)[])(*a).num;
    pb = (i32 (*)[])(*b).num;
    for (i=upperc;i>=(i64)0;--i) {
L118 :;
        j = (u64)(i - offset);
        if (((i <= uppera) && (j <= (u64)upperb))) {
            diff = (((i64)(*pa)[(i)] - (i64)(*pb)[((i64)j)]) - carry);
        }
        else if ((i <= uppera)) {
            diff = ((i64)(*pa)[(i)] - carry);
        }
        else if ((j <= (u64)upperb)) {
            diff = (-((i64)(*pb)[((i64)j)]) - carry);
        }
        else {
            diff = -(carry);
        }
;
        if ((diff < (i64)0)) {
            carry = (i64)1;
            (*(c + i)) = (diff + (i64)1000000000);
        }
        else {
            (*(c + i)) = diff;
            carry = (i64)0;
        }
;
L119 :;
    }
L120 :;
    ;
    if (!!(carry)) {
        if (!!(isneg)) {
            qq_runaux$pcerror((byte*)"SUBU/CARRY",(byte*)"");
        }
;
        {struct qq_decls$objrec *  temp = a; a = b; b = temp; };
        isneg = (i64)1;
        qq_decimal$freesmall((i32 *)c,precc);
        goto L117 ;
;
    }
;
    qq_decimal$smalltobig(dest,(i32 *)c,precc,precc,(i64)0);
    (*dest).neg = isneg;
    (*dest).expon = (expona - qq_decimal$stblz);
}

static struct qq_decls$objrec *qq_decimal$makebignum(i64 length) {
        struct qq_decls$objrec *  a;
    a = qq_vars$obj_new();
    if (!!(length)) {
        (*a).num = (i32 (*)[])mlib$pcm_alloc((length * (i64)4));
        (*a).numtype = (i64)1;
    }
    else {
        (*a).num = 0;
        (*a).numtype = (i64)0;
    }
;
    (*a).length = length;
    (*a).expon = (i64)0;
    (*a).neg = (i64)0;
    return a;
}

static i32 *qq_decimal$makesmallnum(i64 length) {
    return (i32 *)mlib$pcm_alloc((length * (i64)4));
}

static struct qq_decls$objrec *qq_decimal$smalltobig(struct qq_decls$objrec *c,i32 *a,i64 length,i64 alloc,i64 offset) {
        i32 *  p;
        i64 leadingzeros;
        i64 trailingzeros;
        i64 nonzeros;
        i64 newlength;
        i64 $av_1;
    qq_decimal$bn_setzero(c);
    p = (i32 *)a;
    leadingzeros = (trailingzeros = (nonzeros = (i64)0));
    $av_1 = length;
    while ($av_1-- > 0) {
L121 :;
        if (!!((i64)(*(p)++))) {
            nonzeros = (i64)1;
            trailingzeros = (i64)0;
        }
        else {
            if (!!(nonzeros)) {
                ++(trailingzeros);
            }
            else {
                ++(leadingzeros);
            }
;
        }
;
L122 :;
    }
L123 :;
    ;
    qq_decimal$stblz = leadingzeros;
    if (!!(nonzeros)) {
        newlength = ((length - trailingzeros) - leadingzeros);
        if ((newlength==length && length==alloc)) {
            (*c).num = (i32 (*)[])a;
        }
        else {
            (*c).num = (i32 (*)[])qq_decimal$makesmallnum(newlength);
            memcpy((*c).num,(a + leadingzeros),(u64)(newlength * (i64)4));
            qq_decimal$freesmall((i32 *)(a + offset),alloc);
        }
;
        (*c).length = newlength;
        (*c).numtype = (i64)1;
        (*c).expon = ((length - (i64)1) - leadingzeros);
    }
    else if (!!(alloc)) {
        qq_decimal$freesmall((i32 *)(a + offset),alloc);
    }
;
    return c;
}

static void qq_decimal$freesmall(i32 *p,i64 length) {
    mlib$pcm_free(p,(length * (i64)4));
}

struct qq_decls$objrec *qq_decimal$bn_init(void) {
        struct qq_decls$objrec *  a;
    a = qq_decimal$makebignum((i64)0);
    return a;
}

static void qq_decimal$bn_setzero(struct qq_decls$objrec *a) {
    if (!!(a)) {
        if (!!((*a).num)) {
            qq_decimal$freesmall((i32 *)(*a).num,(*a).length);
        }
;
        (*a).num = 0;
        (*a).length = (i64)0;
        (*a).neg = (i64)0;
        (*a).expon = (i64)0;
        (*a).numtype = (i64)0;
    }
;
}

static void qq_decimal$bn_move(struct qq_decls$objrec *a,struct qq_decls$objrec *b) {
    qq_decimal$bn_setzero(a);
    memcpy(&(*a).bignumdescr,&(*b).bignumdescr,24);
    memset(&((*b).bignumdescr),0,24);
}

static void qq_decimal$bn_dupl(struct qq_decls$objrec *a,struct qq_decls$objrec *b) {
        struct qq_decls$objrec *  c;
        i64 size;
    c = qq_decimal$bn_init();
    (*c) = (*b);
    if (!!((*c).length)) {
        (*c).num = (i32 (*)[])qq_decimal$makesmallnum((size = (*c).length));
        memcpy((*c).num,(*b).num,(u64)(size * (i64)4));
    }
;
    qq_decimal$bn_move(a,c);
    qq_decimal$obj_free_dec(c);
}

static void qq_decimal$bn_setinf(struct qq_decls$objrec *dest) {
    qq_decimal$bn_setzero(dest);
    (*dest).numtype = (i64)2;
}

static void qq_decimal$bn_setnan(struct qq_decls$objrec *dest) {
    qq_decimal$bn_setzero(dest);
    (*dest).numtype = (i64)3;
}

void qq_decimal$var_setnan(struct qq_decls$varrec *dest) {
    (*dest).tagx = (i64)259;
    (*dest).objptr = qq_decimal$makebignum((i64)0);
    qq_decimal$bn_setnan((*dest).objptr);
}

void qq_decimal$var_setinf(struct qq_decls$varrec *dest) {
    (*dest).tagx = (i64)259;
    (*dest).objptr = qq_decimal$makebignum((i64)0);
    qq_decimal$bn_setinf((*dest).objptr);
}

static i64 qq_decimal$getbintype(struct qq_decls$objrec *a,struct qq_decls$objrec *b) {
        i64 atype;
        i64 btype;
    atype = (i64)(*a).numtype;
    btype = (i64)(*b).numtype;
    if (((atype == (i64)3) || (btype == (i64)3))) {
        return (i64)4;
    }
;
    if ((atype==(i64)1)) {
        if ((btype==(i64)1)) {
            return (i64)1;
        }
        else if ((btype==(i64)0)) {
            return (i64)5;
        }
        else {
            return (i64)6;
        }
;
    }
    else if ((atype==(i64)0)) {
        if ((btype==(i64)1)) {
            return (i64)7;
        }
        else if ((btype==(i64)0)) {
            return (i64)2;
        }
        else {
            return (i64)9;
        }
;
    }
    else {
        if ((btype==(i64)1)) {
            return (i64)8;
        }
        else if ((btype==(i64)0)) {
            return (i64)10;
        }
        else {
            return (i64)3;
        }
;
    }
;
}

static void qq_decimal$bn_negto(struct qq_decls$objrec *a) {
    if (!(!!(qq_decimal$bn_iszero(a)))) {
        (*a).neg = (i64)!(!!((i64)(*a).neg));
    }
;
}

static void qq_decimal$bn_absto(struct qq_decls$objrec *a) {
    (*a).neg = (i64)0;
}

static i64 qq_decimal$bn_mul(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b) {
        i64 neg;
        {i64 $temp = qq_decimal$getbintype(a,b);
if (($temp==(i64)1)) {
    }
    else if (($temp==(i64)2) || ($temp==(i64)5) || ($temp==(i64)7)) {
        qq_decimal$bn_setzero(dest);
        return (i64)1;
    }
    else {
        qq_decimal$bn_setnan(dest);
        return (i64)0;
    }
    };
    neg = (i64)((i64)(*a).neg != (i64)(*b).neg);
    qq_decimal$bn_mulu(dest,a,b);
    if (!!(neg)) {
        qq_decimal$bn_negto(dest);
    }
;
    return (i64)1;
}

static i64 qq_decimal$bn_mulp(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b,i64 prec) {
        i64 res;
    res = qq_decimal$bn_mul(dest,a,b);
    if (!!(res)) {
        qq_decimal$bn_setprec(dest,((prec == (i64)0) ? qq_decimal$currprec : prec));
    }
;
    return res;
}

static void qq_decimal$bn_mulu(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b) {
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
        i64 pd;
        i64 pr;
        i64 p;
        i64 carry;
        i64 x;
        i32 *  c;
    expona = (i64)(*a).expon;
    exponb = (i64)(*b).expon;
    uppera = ((*a).length - (i64)1);
    upperb = ((*b).length - (i64)1);
    precc = ((uppera + upperb) + (i64)2);
    nc2 = precc;
    c = (i32 *)qq_decimal$makesmallnum(nc2);
    memset(c,(i32)(i64)0,(u64)(precc * (i64)4));
    cx = (precc - (i64)1);
    for (bx=upperb;bx>=(i64)0;--bx) {
L124 :;
        carry = (i64)0;
        cx1 = cx;
        for (ax=uppera;ax>=(i64)0;--ax) {
L127 :;
            p = (((i64)(*(*a).num)[(ax)] * (i64)(*(*b).num)[(bx)]) + carry);
            pd = (p / (i64)1000000000);
            pr = (p - (pd * (i64)1000000000));
            x = ((i64)(*(c + cx1)) + pr);
            if ((x > (i64)999999999)) {
                carry = (pd + (i64)1);
                (*(c + (cx1)--)) = (x - (i64)1000000000);
            }
            else {
                carry = pd;
                (*(c + (cx1)--)) = x;
            }
;
L128 :;
        }
L129 :;
        ;
        (*(c + cx1)) = carry;
        --(cx);
L125 :;
    }
L126 :;
    ;
    qq_decimal$smalltobig(dest,(i32 *)c,precc,nc2,(i64)0);
    (*dest).expon = (((expona + exponb) + (i64)1) - qq_decimal$stblz);
}

static i64 qq_decimal$smallmulto(i32 *p,i32 *q,i64 plen,i64 m) {
        i32 *  pp;
        i32 *  qq;
        i64 carry;
        i64 d;
        i64 $av_1;
        i64 $av_2;
    if ((m==(i64)0)) {
        (*p) = (i64)0;
        return (i64)1;
    }
    else if ((m==(i64)1)) {
        memcpy(p,q,(u64)(plen * (i64)4));
        return plen;
    }
;
    pp = (i32 *)((p + plen) - (i64)1);
    qq = (i32 *)((q + plen) - (i64)1);
    carry = (i64)0;
    $av_1 = plen;
    while ($av_1-- > 0) {
L130 :;
        d = (((i64)(*qq) * m) + carry);
        (*pp) = (d % (i64)1000000000);
        carry = (d / (i64)1000000000);
        --(qq);
        --(pp);
L131 :;
    }
L132 :;
    ;
    if (!!(carry)) {
        pp = (i32 *)(p + plen);
        $av_2 = plen;
        while ($av_2-- > 0) {
L133 :;
            (*pp) = (i64)(*(pp - (i64)1));
            --(pp);
L134 :;
        }
L135 :;
        ;
        (*pp) = carry;
        ++(plen);
    }
;
    return plen;
}

static i64 qq_decimal$bn_div(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b,i64 prec) {
        i64 neg;
        {i64 $temp = qq_decimal$getbintype(a,b);
if (($temp==(i64)1)) {
    }
    else if (($temp==(i64)7)) {
        qq_decimal$bn_setzero(dest);
        return (i64)1;
    }
    else if (($temp==(i64)2) || ($temp==(i64)5)) {
        qq_decimal$bn_setinf(dest);
        return (i64)0;
    }
    else {
        qq_decimal$bn_setnan(dest);
        return (i64)0;
    }
    };
    neg = (i64)((i64)(*a).neg != (i64)(*b).neg);
    qq_decimal$bn_fdivu(dest,a,b,prec);
    if (!!(neg)) {
        qq_decimal$bn_negto(dest);
    }
;
    return (i64)1;
}

static i64 qq_decimal$bn_idiv(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b) {
        i64 neg;
        {i64 $temp = qq_decimal$getbintype(a,b);
if (($temp==(i64)1)) {
    }
    else if (($temp==(i64)7)) {
        qq_decimal$bn_setzero(dest);
        return (i64)1;
    }
    else if (($temp==(i64)2) || ($temp==(i64)5)) {
        qq_decimal$bn_setinf(dest);
        return (i64)0;
    }
    else {
        qq_decimal$bn_setnan(dest);
        return (i64)0;
    }
    };
    neg = (i64)((i64)(*a).neg != (i64)(*b).neg);
    qq_decimal$bn_idivu(dest,a,b,0);
    if (!!(neg)) {
        qq_decimal$bn_negto(dest);
    }
;
    return (i64)1;
}

static i64 qq_decimal$bn_idivrem(struct qq_decls$objrec *dest,struct qq_decls$objrec *rm,struct qq_decls$objrec *a,struct qq_decls$objrec *b) {
        i64 nega;
        i64 negb;
        {i64 $temp = qq_decimal$getbintype(a,b);
if (($temp==(i64)1)) {
    }
    else if (($temp==(i64)7)) {
        qq_decimal$bn_setzero(dest);
        qq_decimal$bn_setzero(rm);
        return (i64)1;
    }
    else if (($temp==(i64)2) || ($temp==(i64)5)) {
        qq_decimal$bn_setinf(dest);
        qq_decimal$bn_setzero(rm);
        return (i64)0;
    }
    else {
        qq_decimal$bn_setnan(dest);
        return (i64)0;
    }
    };
    nega = (i64)(*a).neg;
    negb = (i64)(*b).neg;
    qq_decimal$bn_idivu(dest,a,b,rm);
    if ((nega != negb)) {
        qq_decimal$bn_negto(dest);
    }
;
    if (!!(nega)) {
        qq_decimal$bn_negto(rm);
    }
;
    return (i64)1;
}

static i64 qq_decimal$bn_irem(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b) {
        struct qq_decls$objrec *  d;
        i64 nega;
        {i64 $temp = qq_decimal$getbintype(a,b);
if (($temp==(i64)1)) {
    }
    else if (($temp==(i64)7)) {
        qq_decimal$bn_dupl(dest,b);
        return (i64)1;
    }
    else if (($temp==(i64)2) || ($temp==(i64)5)) {
        qq_decimal$bn_setinf(dest);
        qq_decimal$bn_setzero(dest);
        return (i64)0;
    }
    else {
        qq_decimal$bn_setnan(dest);
        return (i64)0;
    }
    };
    nega = (i64)(*a).neg;
    d = qq_decimal$bn_init();
    qq_decimal$bn_idivu(d,a,b,dest);
    if (!!(nega)) {
        qq_decimal$bn_negto(dest);
    }
;
    qq_decimal$obj_free_dec(d);
    return (i64)1;
}

static void qq_decimal$bn_idivu(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b,struct qq_decls$objrec *rm) {
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
        struct qq_decls$objrec *  d;
        i64 i;
    na = (*a).length;
    nb = (*b).length;
    expona = (i64)(*a).expon;
    exponb = (i64)(*b).expon;
    badjust = ((exponb + (i64)1) - nb);
    if (((na > (expona + (i64)1)) || (nb > (exponb + (i64)1)))) {
        qq_runaux$pcerror((byte*)"idivu:a or b not int",(byte*)"");
    }
;
    nc = (expona + (i64)1);
    if ((expona < exponb)) {
        qq_decimal$bn_setzero(dest);
        if (!!(rm)) {
            qq_decimal$bn_dupl(rm,a);
        }
;
        return;
    }
;
    uppera = (na - (i64)1);
    upperb = (nb - (i64)1);
    upperc = (nc - (i64)1);
    pa = (i32 *)(*a).num;
    pb = (i32 *)(*b).num;
    n = nb;
    x = (i32 *)qq_decimal$makesmallnum((nx2 = (n + (i64)1)));
    nx = n;
    nupper = (nc - badjust);
    for (i=(i64)0;i<=upperb;++i) {
L136 :;
        if ((i <= uppera)) {
            (*(x + i)) = (i64)(*(pa + i));
        }
        else {
            (*(x + i)) = (i64)0;
        }
;
L137 :;
    }
L138 :;
    ;
    c = (i32 *)qq_decimal$makesmallnum(nc);
    cx = (i64)0;
    L139 :;
    while (1) {
        k = qq_decimal$smalldiv((i32 *)x,(i32 *)pb,&nx,nb);
        (*(c + (cx)++)) = k;
        if ((n >= nupper)) {
            goto L140 ;
        }
;
        nexta = ((n > uppera) ? (i64)0 : (i64)(*(pa + n)));
        ++(n);
        if (((nx == (i64)1) && ((i64)(*x) == (i64)0))) {
            (*x) = nexta;
        }
        else {
            (*(x + nx)) = nexta;
            ++(nx);
        }
;
    }
L140 :;
    ;
    if ((!!(rm) && (exponb < nb))) {
        qq_decimal$smalltobig(rm,(i32 *)x,nx,nx2,(i64)0);
    }
    else {
        qq_decimal$freesmall((i32 *)x,nx2);
    }
;
    if (((cx == (i64)1) && ((i64)(*c) == (i64)0))) {
        qq_decimal$freesmall((i32 *)c,nc);
        qq_decimal$bn_setzero(dest);
        if (!!(rm)) {
            qq_decimal$bn_dupl(rm,a);
        }
;
        return;
    }
;
    if ((((i64)(*c) == (i64)0) && (cx >= (i64)2))) {
        qq_decimal$smalltobig(dest,(i32 *)(c + (i64)1),(cx - (i64)1),nc,(i64)-1);
    }
    else {
        qq_decimal$smalltobig(dest,(i32 *)c,cx,nc,(i64)0);
    }
;
    if ((!!(rm) && (exponb >= nb))) {
        d = qq_decimal$bn_init();
        qq_decimal$bn_mulu(d,b,dest);
        qq_decimal$bn_subu(rm,a,d);
        qq_decimal$obj_free_dec(d);
    }
;
}

static void qq_decimal$bn_fdivu(struct qq_decls$objrec *dest,struct qq_decls$objrec *a,struct qq_decls$objrec *b,i64 precision) {
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
    expona = (i64)(*a).expon;
    exponb = (i64)(*b).expon;
    if (!!(precision)) {
        precision = (((precision - (i64)1) / (i64)9) + (i64)1);
    }
    else {
        precision = qq_decimal$currprec;
    }
;
    nc = precision;
    uppera = (na - (i64)1);
    upperb = (nb - (i64)1);
    upperc = (nc - (i64)1);
    pa = (i32 *)(*a).num;
    pb = (i32 *)(*b).num;
    n = nb;
    x = (i32 *)qq_decimal$makesmallnum((nx2 = (n + (i64)1)));
    nx = n;
    for (i=(i64)0;i<=upperb;++i) {
L141 :;
        if ((i <= uppera)) {
            (*(x + i)) = (i64)(*(pa + i));
        }
        else {
            (*(x + i)) = (i64)0;
        }
;
L142 :;
    }
L143 :;
    ;
    c = (i32 *)qq_decimal$makesmallnum((nc2 = (nc + (i64)1)));
    cx = (i64)0;
    L144 :;
    while (1) {
        k = qq_decimal$smalldiv((i32 *)x,(i32 *)pb,&nx,nb);
        (*(c + (cx)++)) = k;
        if ((cx > nc)) {
            goto L145 ;
        }
;
        nexta = ((n > uppera) ? (i64)0 : (i64)(*(pa + n)));
        ++(n);
        if (((nx == (i64)1) && ((i64)(*x) == (i64)0))) {
            (*x) = nexta;
        }
        else {
            (*(x + nx)) = nexta;
            ++(nx);
        }
;
    }
L145 :;
    ;
    qq_decimal$freesmall((i32 *)x,nx2);
    if (((cx == (i64)1) && ((i64)(*c) == (i64)0))) {
        qq_decimal$freesmall((i32 *)c,nc2);
        qq_decimal$bn_setzero(dest);
        return;
    }
;
    if ((((i64)(*c) == (i64)0) && (cx >= (i64)2))) {
        qq_decimal$smalltobig(dest,(i32 *)(c + (i64)1),(cx - (i64)1),nc2,(i64)-1);
        (*dest).expon = ((expona - exponb) - (i64)1);
    }
    else {
        qq_decimal$smalltobig(dest,(i32 *)c,cx,nc2,(i64)0);
        (*dest).expon = (expona - exponb);
    }
;
}

static i64 qq_decimal$smalldiv(i32 *x,i32 *b,i64 *xlen,i64 nb) {
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
        i64 $av_1;
        i64 i;
    nx = (*xlen);
    k = (i64)0;
    count = (i64)0;
    e = (i32 *)qq_decimal$makesmallnum((esize = (nb + (i64)1)));
    L146 :;
    while (1) {
        if ((nx < nb)) {
            goto L147 ;
        }
        else if ((nx > nb)) {
            xx = (((i64)(*x) * (i64)1000000000) + (i64)(*(x + (i64)1)));
            y = (xx / ((i64)(*b) + (i64)1));
        }
        else {
            if (((i64)(*x) >= ((i64)(*b) + (i64)1))) {
                y = ((i64)(*x) / ((i64)(*b) + (i64)1));
            }
            else {
                y = (i64)1;
                                ($av_1 = (nb - (i64)1));
                for (i=(i64)0;i<=$av_1;++i) {
L148 :;
                    xi = (i64)(*(x + i));
                    bi = (i64)(*(b + i));
                    if (((i64)xi < (i64)bi)) {
                        y = (i64)0;
                        goto L147 ;
                    }
                    else if (((i64)xi > (i64)bi)) {
                        goto L150 ;
                    }
;
L149 :;
                }
L150 :;
                ;
            }
;
        }
;
        k += y;
        if ((y > (i64)1)) {
            ne = qq_decimal$smallmulto((i32 *)e,(i32 *)b,nb,y);
            nx = qq_decimal$smallsubto((i32 *)x,(i32 *)e,nx,ne);
        }
        else if (!!(y)) {
            nx = qq_decimal$smallsubto((i32 *)x,(i32 *)b,nx,nb);
        }
        else {
            qq_runaux$pcerror((byte*)"smalldiv:Y=0",(byte*)"");
        }
;
    }
L147 :;
    ;
    qq_decimal$freesmall((i32 *)e,esize);
    (*xlen) = nx;
    return k;
}

static i64 qq_decimal$smallsubto(i32 *p,i32 *q,i64 plen,i64 qlen) {
        i32 *  pp;
        i32 *  qq;
        i64 carry;
        i64 diff;
        i64 z;
        i64 $av_1;
        i64 $av_2;
    pp = (i32 *)((p + plen) - (i64)1);
    qq = (i32 *)((q + qlen) - (i64)1);
    carry = (i64)0;
    z = (i64)0;
    $av_1 = plen;
    while ($av_1-- > 0) {
L151 :;
        if ((qq >= q)) {
            diff = (((i64)(*pp) - (i64)(*qq)) - carry);
            --(qq);
        }
        else {
            diff = ((i64)(*pp) - carry);
        }
;
        if ((diff < (i64)0)) {
            carry = (i64)1;
            (*pp) = (diff + (i64)1000000000);
        }
        else {
            (*pp) = diff;
            carry = (i64)0;
        }
;
        if (!!((i64)(*pp))) {
            z = (i64)0;
        }
        else {
            ++(z);
        }
;
        --(pp);
L152 :;
    }
L153 :;
    ;
    if (!!(carry)) {
        qq_runaux$pcerror((byte*)"SSUBTO/CARRY?",(byte*)"");
    }
;
    if ((z == plen)) {
        --(z);
    }
;
    if (!!(z)) {
        plen -= z;
        pp = (i32 *)p;
        qq = (i32 *)(p + z);
        $av_2 = plen;
        while ($av_2-- > 0) {
L154 :;
            (*(pp)++) = (i64)(*(qq)++);
L155 :;
        }
L156 :;
        ;
    }
;
    return plen;
}

static i64 qq_decimal$bn_getprec(struct qq_decls$objrec *a) {
    return ((*a).length * (i64)9);
}

static void qq_decimal$bn_setprec(struct qq_decls$objrec *a,i64 prec) {
        i64 oldlength;
        i64 newlength;
        struct qq_decls$objrec *  c;
        i64 $av_1;
        i64 i;
    if (((i64)(*a).numtype != (i64)1)) {
        return;
    }
;
    if (((prec < (i64)1) || (prec > (i64)10000000))) {
        return;
    }
;
    prec = ((((prec - (i64)1) / (i64)9) + (i64)1) * (i64)9);
    newlength = (prec / (i64)9);
    oldlength = (*a).length;
    if ((oldlength <= newlength)) {
        return;
    }
;
    c = qq_decimal$makebignum(newlength);
    (*c).neg = (i64)(*a).neg;
    (*c).expon = (i64)(*a).expon;
        ($av_1 = (newlength - (i64)1));
    for (i=(i64)0;i<=$av_1;++i) {
L157 :;
        if ((i < oldlength)) {
            (*(*c).num)[(i)] = (i64)(*(*a).num)[(i)];
        }
        else {
            (*(*c).num)[(i)] = (i64)0;
        }
;
L158 :;
    }
L159 :;
    ;
    qq_decimal$bn_move(a,c);
    qq_decimal$obj_free_dec(c);
}

static i64 qq_decimal$bn_getglobalprec(void) {
    return (qq_decimal$currprec * (i64)9);
}

static void qq_decimal$bn_setglobalprec(i64 prec) {
    qq_decimal$currprec = (((prec - (i64)1) / (i64)9) + (i64)1);
}

static struct qq_decls$objrec *qq_decimal$bn_makefloat(r64 x) {
        u8 str[2048];
    msysc$m_print_startstr(str);
    msysc$m_print_r64(x,(byte*)".15g");
    msysc$m_print_end();
    ;
    return qq_decimal$bn_makestr((u8 *)str,(i64)0);
}

struct qq_decls$varrec *qq_decimal$dectemp(struct qq_decls$varrec *a) {
    qq_decimal$vtemp.tagx = (i64)259;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        qq_decimal$vtemp.objptr = qq_decimal$bn_makeint((*a).value);
    }
    else if (($temp==(i64)2)) {
        qq_decimal$vtemp.objptr = qq_decimal$bn_makefloat((*a).xvalue);
    }
    else {
        qq_runaux$pcerror((byte*)"dectemp",(byte*)"");
    }
    };
    (*a) = qq_decimal$vtemp;
    return a;
}

void qq_decimal$freedectemp(void) {
    qq_decimal$obj_free_dec(qq_decimal$vtemp.objptr);
}

static void qq_decimal$bn_ipower(struct qq_decls$objrec *d,struct qq_decls$objrec *a,i64 n) {
        struct qq_decls$objrec *  e;
        struct qq_decls$objrec *  f;
    if ((n < (i64)0)) {
        qq_decimal$bn_setzero(d);
    }
    else if ((n == (i64)0)) {
        qq_decimal$bn_move(d,qq_decimal$bn_makeint((i64)1));
    }
    else if ((n == (i64)1)) {
        qq_decimal$bn_dupl(d,a);
    }
    else if (((n & (i64)1) == (i64)0)) {
        e = qq_decimal$bn_init();
        qq_decimal$bn_mulu(e,a,a);
        qq_decimal$bn_ipower(d,e,(n / (i64)2));
        qq_decimal$obj_free_dec(e);
    }
    else {
        e = qq_decimal$bn_init();
        f = qq_decimal$bn_init();
        qq_decimal$bn_mulu(e,a,a);
        qq_decimal$bn_ipower(f,e,((n - (i64)1) / (i64)2));
        qq_decimal$bn_mulu(d,a,f);
        qq_decimal$obj_free_dec(e);
        qq_decimal$obj_free_dec(f);
    }
;
}

void qq_decimal$var_power_dec(struct qq_decls$varrec *a,i64 n) {
        struct qq_decls$objrec *  dest;
    dest = qq_decimal$bn_init();
    qq_decimal$bn_ipower(dest,(*a).objptr,n);
    (*a).objptr = dest;
}

i64 qq_decimal$var_convert_dec_int(struct qq_decls$varrec *a) {
    return qq_decimal$bn_toint((*a).objptr);
}

static i64 qq_decimal$bn_toint(struct qq_decls$objrec *a) {
        i64 x;
        i64 $av_1;
        i64 $av_2;
        i64 i;
    if (!(!!(qq_decimal$bn_isint(a)))) {
        qq_runaux$pcerror((byte*)"dec-float->int not ready",(byte*)"");
        return (i64)0;
    }
;
    if (!!(qq_decimal$bn_iszero(a))) {
        return (i64)0;
    }
;
    x = (i64)0;
        ($av_1 = ((*a).length - (i64)1));
    for (i=(i64)0;i<=$av_1;++i) {
L160 :;
        x = ((x * (i64)1000000000) + (i64)(*(*a).num)[(i)]);
L161 :;
    }
L162 :;
    ;
        ($av_2 = (i64)(*a).expon);
    for (i=(*a).length;i<=$av_2;++i) {
L163 :;
        x *= (i64)1000000000;
L164 :;
    }
L165 :;
    ;
    if (!!((i64)(*a).neg)) {
        return -(x);
    }
    else {
        return x;
    }
;
}

// START
void qq_decimal$start(void) {

}

void qq_dicts$var_make_dict(struct qq_decls$varrec *a,struct qq_decls$varrec *dest,i64 n) {
        struct qq_decls$objrec *  p;
        struct qq_decls$varrec *  b;
        struct qq_decls$varrec v;
        i64 $av_1;
    p = qq_dicts$obj_new_dict(n);
    b = (*p).varptr;
    v.tagx = (i64)262;
    v.objptr = p;
    $av_1 = n;
    while ($av_1-- > 0) {
L166 :;
        qq_dicts$adddictitem(&v,a,(a + (i64)1));
        a += (i64)2;
L167 :;
    }
L168 :;
    ;
    (*p).dictitems = n;
    (*dest) = v;
}

struct qq_decls$objrec *qq_dicts$obj_new_dict(i64 n) {
        struct qq_decls$objrec *  p;
        i64 m;
    m = msysc$m_imax((i64)16,qq_lib$nextpoweroftwo((n * (i64)2)));
    p = qq_lists$obj_newlist(m,(i64)1,0);
    (*p).dictitems = (i64)0;
    return p;
}

void qq_dicts$obj_free_dict(struct qq_decls$objrec *p,i64 internal) {
        struct qq_decls$varrec *  q;
        i64 $av_1;
    q = (*p).varptr;
    $av_1 = (*p).length;
    while ($av_1-- > 0) {
L169 :;
        if (!!((i64)(*q).hasref)) {
            qq_vars$var_unshareu(q);
        }
;
        ++(q);
L170 :;
    }
L171 :;
    ;
    if (!!((*p).length)) {
        mlib$pcm_free((*p).varptr,((i64)(*p).alloc32 * (i64)16));
    }
;
    if (!(!!(internal))) {
        mlib$pcm_free32(p);
    }
;
}

void qq_dicts$var_dupl_dict(struct qq_decls$varrec *a) {
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
        struct qq_decls$varrec *  plist;
        struct qq_decls$varrec *  qlist;
        i64 $av_1;
    p = (*a).objptr;
    q = qq_dicts$obj_new_dict((i64)(*p).dictitems);
    (*q) = (*p);
    (*q).refcount = (i64)1;
    (*q).flags = msysc$m_setdotindex((*q).flags,(i64)1,(u64)1u);
    (*a).objptr = q;
    if (((*q).length == (i64)0)) {
        return;
    }
;
    qlist = ((*q).varptr = (struct qq_decls$varrec *)mlib$pcm_alloc(((*p).length * (i64)16)));
    (*q).alloc32 = (mlib$allocbytes / (i64)16);
    plist = (*p).varptr;
    $av_1 = (*q).length;
    while ($av_1-- > 0) {
L172 :;
        (*qlist) = (*plist);
        if (!!((i64)(*qlist).hasref)) {
            qq_vars$var_duplu(qlist);
        }
;
        ++(qlist);
        ++(plist);
L173 :;
    }
L174 :;
    ;
}

i64 qq_dicts$var_equal_dict(struct qq_decls$varrec *x,struct qq_decls$varrec *y) {
    qq_runaux$pcerror((byte*)"EQUALDICT",(byte*)"");
    return (i64)1;
}

struct qq_decls$varrec *qq_dicts$var_finddictitem(struct qq_decls$varrec *vd,struct qq_decls$varrec *p,i64 doins) {
        i64 index;
        i64 size;
        i64 keytag;
        i64 wrapped;
        i64 limit;
        i64 keyvalue;
        struct qq_decls$varrec *  q;
        struct qq_decls$objrec *  pa;
        struct qq_decls$objrec *  qa;
        struct qq_decls$objrec *  d;
    //retry:
L175 :;
;
    d = (*vd).objptr;
    size = ((*d).length / (i64)2);
    index = (qq_vars$var_gethashvalue(p) & (size - (i64)1));
    q = ((*d).varptr + (index * (i64)2));
    wrapped = (i64)0;
    keytag = (i64)(*p).tag;
    keyvalue = (*p).value;
    pa = (*p).objptr;
    L176 :;
    while (1) {
        if (((i64)(*q).tag == (i64)0)) {
            goto L177 ;
        }
        else if (((i64)(*q).tag == keytag)) {
            if ((keytag==(i64)1) || (keytag==(i64)2) || (keytag==(i64)4)) {
                if (((*q).value == keyvalue)) {
                    ++(q);
                    if (!!((i64)(*q).hasref)) {
                        ++((*(*q).objptr).refcount);
                    }
;
                    return q;
                }
;
            }
            else if ((keytag==(i64)9)) {
                qa = (*q).objptr;
                if (((*pa).length == (*qa).length)) {
                    if ((memcmp((void *)(*pa).strptr,(void *)(*qa).strptr,(u64)(*pa).length) == (i64)0)) {
                        ++(q);
                        if (!!((i64)(*q).hasref)) {
                            ++((*(*q).objptr).refcount);
                        }
;
                        return q;
                    }
;
                }
;
            }
            else if ((keytag==(i64)12)) {
                if (!!(qq_records$var_equal_record(p,q))) {
                    ++(q);
                    if (!!((i64)(*q).hasref)) {
                        ++((*(*q).objptr).refcount);
                    }
;
                    return q;
                }
;
            }
;
        }
;
        ++(index);
        q += (i64)2;
        if ((index >= size)) {
            if (!!(wrapped)) {
                qq_runaux$pcerror((byte*)"DICT FULL?",(byte*)"");
            }
;
            wrapped = (i64)1;
            index = (i64)0;
            q = (*d).varptr;
        }
;
    }
L177 :;
    ;
    if (!!(doins)) {
        limit = ((size * (i64)3) / (i64)4);
        if (((i64)(*d).dictitems >= limit)) {
            qq_dicts$expanddict(vd);
            goto L175 ;
;
        }
;
        (*q) = (*p);
        if (!!((i64)(*q).hasref)) {
            ++((*(*q).objptr).refcount);
        }
;
        ++((*d).dictitems);
        return (q + (i64)1);
    }
    else {
        return 0;
    }
;
}

static void qq_dicts$expanddict(struct qq_decls$varrec *vd) {
        i64 n;
        i64 m;
        i64 i;
        i64 oldrefcount;
        struct qq_decls$objrec *  d;
        struct qq_decls$objrec *  e;
        struct qq_decls$varrec *  p;
        struct qq_decls$varrec *  q;
        struct qq_decls$varrec *  r;
        struct qq_decls$varrec ev;
        static byte inuse;
    if (!!((i64)inuse)) {
        qq_runaux$pcerror((byte*)"expanddict?",(byte*)"");
    }
;
    inuse = (i64)1;
    d = (*vd).objptr;
    n = (i64)(*d).alloc32;
    m = (n / (i64)2);
    p = (*d).varptr;
    e = qq_dicts$obj_new_dict((m * (i64)2));
    qq_vars$var_objtovar((i64)6,e,&ev);
    q = p;
    for (i=(i64)1;i<=m;++i) {
L178 :;
        if (((i64)(*q).tag != (i64)0)) {
            r = qq_dicts$var_finddictitem(&ev,q,(i64)1);
            ++(q);
            (*r) = (*(q)++);
            if (!!((i64)(*r).hasref)) {
                ++((*(*r).objptr).refcount);
            }
;
        }
        else {
            q += (i64)2;
        }
;
L179 :;
    }
L180 :;
    ;
    qq_dicts$obj_free_dict(d,(i64)1);
    oldrefcount = (i64)(*d).refcount;
    (*d) = (*e);
    mlib$pcm_free(e,(i64)8);
    (*d).refcount = oldrefcount;
    inuse = (i64)0;
}

static void qq_dicts$adddictitem(struct qq_decls$varrec *d,struct qq_decls$varrec *p,struct qq_decls$varrec *q) {
        struct qq_decls$objrec *  da;
        struct qq_decls$varrec *  r;
    da = (*d).objptr;
    if (((*da).length == (i64)0)) {
        qq_runaux$pcerror((byte*)"NULL DICT",(byte*)"");
    }
;
    r = qq_dicts$var_finddictitem(d,p,(i64)1);
    if (!!((i64)(*q).hasref)) {
        ++((*(*q).objptr).refcount);
    }
;
    if (!!((i64)(*r).hasref)) {
        qq_vars$var_unshareu(r);
    }
;
    (*r) = (*q);
}

// START
void qq_dicts$start(void) {

}

struct qq_decls$varrec *qq_host$callhostfunction(i64 hostfn,struct qq_decls$varrec *sp) {
        void (*fnaddr)(void);
        i64 nparams;
        i64 isfn;
        i64 $av_1;
    fnaddr = (void (*)(void))qq_tables$hosthandlers[(hostfn)];
    nparams = (i64)qq_tables$hostnparams[(hostfn)];
    isfn = (i64)qq_tables$hostisfn[(hostfn)];
    if ((fnaddr == 0)) {
        qq_runaux$pcerror((byte*)"Hostfn not implemented:",qq_tables$hostfnnames[(hostfn)]);
    }
;
        {i64 $temp = (nparams + isfn);
if (($temp==(i64)0)) {
        ((*(void (*)(void))fnaddr))();
    }
    else if (($temp==(i64)1)) {
        ((*(void (*)(struct qq_decls$varrec *,...))fnaddr))(sp);
    }
    else if (($temp==(i64)2)) {
        ((*(void (*)(struct qq_decls$varrec *,struct qq_decls$varrec *,...))fnaddr))(sp,(sp - (i64)1));
    }
    else if (($temp==(i64)3)) {
        ((*(void (*)(struct qq_decls$varrec *,struct qq_decls$varrec *,struct qq_decls$varrec *,...))fnaddr))(sp,(sp - (i64)1),(sp - (i64)2));
    }
    else if (($temp==(i64)4)) {
        ((*(void (*)(struct qq_decls$varrec *,struct qq_decls$varrec *,struct qq_decls$varrec *,struct qq_decls$varrec *,...))fnaddr))(sp,(sp - (i64)1),(sp - (i64)2),(sp - (i64)3));
    }
    else if (($temp==(i64)5)) {
        ((*(void (*)(struct qq_decls$varrec *,struct qq_decls$varrec *,struct qq_decls$varrec *,struct qq_decls$varrec *,struct qq_decls$varrec *,...))fnaddr))(sp,(sp - (i64)1),(sp - (i64)2),(sp - (i64)3),(sp - (i64)4));
    }
    else {
        qq_runaux$pcerror((byte*)"callhost/proc",(byte*)"");
    }
    };
    $av_1 = nparams;
    while ($av_1-- > 0) {
L181 :;
        if (!!((i64)(*sp).hasref)) {
            if (!!((i64)(*sp).hasref)) {
                qq_vars$var_unshareu(sp);
            }
;
        }
;
        --(sp);
L182 :;
    }
L183 :;
    ;
    return sp;
}

void qq_host$pch_leftstr(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *c,struct qq_decls$varrec *result) {
        i64 n;
        i64 length;
        i64 padchar;
        u8 *  s;
        struct qq_decls$objrec *  pa;
    padchar = (i64)32;
        {i64 $temp = (i64)(*c).tag;
if (($temp==(i64)0)) {
    }
    else if (($temp==(i64)9)) {
        if (((*(*c).objptr).length == (i64)1)) {
            padchar = (i64)(u64)(*(*(*c).objptr).strptr);
        }
        else {
            qq_runaux$pcerror((byte*)"left/padx",(byte*)"");
        }
;
    }
    else if (($temp==(i64)1)) {
        padchar = (*c).value;
    }
    else {
        qq_runaux$pcerror((byte*)"left/pad?",(byte*)"");
    }
    };
        {i64 $temp = (i64)(*b).tag;
if (($temp==(i64)0)) {
        n = (i64)1;
    }
    else if (($temp==(i64)1)) {
        n = (*b).value;
    }
    else {
        qq_runaux$pcerror((byte*)"left:bad n",(byte*)"");
    }
    };
    if (((i64)(*a).tag != (i64)9)) {
        qq_runaux$pcerror((byte*)"left:not str",(byte*)"");
    }
;
    pa = (*a).objptr;
    length = (*pa).length;
    s = (*pa).strptr;
    if ((n == (i64)0)) {
        qq_strings$var_empty_string(result,(i64)1);
        return;
    }
;
    (*result).tagx = (i64)265;
    if ((n > (i64)0)) {
        if ((n <= length)) {
            qq_host$leftstring(a,n,result);
        }
        else {
            qq_host$padstring_right(a,n,padchar,result);
        }
;
    }
    else {
        n = -(n);
        if ((n < length)) {
            qq_host$leftstring(a,(length - n),result);
        }
        else {
            qq_strings$var_empty_string(result,(i64)1);
        }
;
    }
;
}

void qq_host$pch_rightstr(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *c,struct qq_decls$varrec *result) {
        i64 n;
        i64 length;
        i64 padchar;
        u8 *  s;
        struct qq_decls$objrec *  pa;
    padchar = (i64)32;
        {i64 $temp = (i64)(*c).tag;
if (($temp==(i64)0)) {
    }
    else if (($temp==(i64)9)) {
        if (((*(*c).objptr).length == (i64)1)) {
            padchar = (i64)(u64)(*(*(*c).objptr).strptr);
        }
        else {
            qq_runaux$pcerror((byte*)"right/padx",(byte*)"");
        }
;
    }
    else if (($temp==(i64)1)) {
        padchar = (*c).value;
    }
    else {
        qq_runaux$pcerror((byte*)"right/pad?",(byte*)"");
    }
    };
        {i64 $temp = (i64)(*b).tag;
if (($temp==(i64)0)) {
        n = (i64)1;
    }
    else if (($temp==(i64)1)) {
        n = (*b).value;
    }
    else {
        qq_runaux$pcerror((byte*)"right:bad n",(byte*)"");
    }
    };
    pa = (*a).objptr;
    if (((i64)(*a).tag != (i64)9)) {
        qq_runaux$pcerror((byte*)"right:not str",(byte*)"");
    }
;
    length = (*pa).length;
    s = (*pa).strptr;
    (*result).tagx = (i64)265;
    if ((n == (i64)0)) {
        qq_strings$var_empty_string(result,(i64)1);
        return;
    }
;
    if ((n > (i64)0)) {
        if ((n <= length)) {
            qq_host$rightstring(a,n,result);
        }
        else {
            qq_host$padstring_left(a,n,padchar,result);
        }
;
    }
    else {
        n = -(n);
        if ((n < length)) {
            qq_host$rightstring(a,(length - n),result);
        }
        else {
            qq_strings$var_empty_string(result,(i64)1);
        }
;
    }
;
}

void qq_host$pch_convlc(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *result) {
    qq_host$checkparam(a,(i64)9,(i64)-999999);
    (*result) = (*a);
    ++((*(*result).objptr).refcount);
    qq_vars$var_duplu(result);
    qq_strings$var_iconvcase(result,b,(i64)0);
}

void qq_host$pch_convuc(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *result) {
    qq_host$checkparam(a,(i64)9,(i64)-999999);
    (*result) = (*a);
    ++((*(*result).objptr).refcount);
    if (!!((i64)(*result).hasref)) {
        if (!!((i64)(*result).hasref)) {
            qq_vars$var_duplu(result);
        }
;
    }
;
    qq_strings$var_iconvcase(result,b,(i64)1);
}

void qq_host$pch_waitkey(struct qq_decls$varrec *result) {
    (*result).tagx = (i64)1;
    (*result).value = mlinux$os_getch();
}

void qq_host$pch_execwait(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *c,struct qq_decls$varrec *result) {
        u8 *  workdir;
        i64 flag;
        struct qq_decls$objrec *  pa;
    qq_host$checkparam(a,(i64)9,(i64)-999999);
    pa = (*a).objptr;
    flag = qq_host$checkparam(b,(i64)1,(i64)0);
    if (((i64)(*c).tag == (i64)0)) {
        workdir = 0;
    }
    else {
        qq_host$checkparam(c,(i64)9,(i64)-999999);
        workdir = qq_lib$convtostringz((*(*c).objptr).strptr,(*(*c).objptr).length);
    }
;
    (*result).tagx = (i64)1;
    (*result).value = mlinux$os_execwait(qq_lib$convtostringz((*pa).strptr,(*pa).length),flag,workdir);
}

void qq_host$pch_execcmd(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *c,struct qq_decls$varrec *result) {
        u8 *  workdir;
        i64 flag;
        struct qq_decls$objrec *  pa;
    qq_host$checkparam(a,(i64)9,(i64)-999999);
    pa = (*a).objptr;
    flag = qq_host$checkparam(b,(i64)1,(i64)0);
    if (((i64)(*c).tag == (i64)0)) {
        workdir = 0;
    }
    else {
        qq_host$checkparam(c,(i64)9,(i64)-999999);
        workdir = qq_lib$convtostringz((*(*c).objptr).strptr,(*(*c).objptr).length);
    }
;
    (*result).tagx = (i64)1;
    (*result).value = mlinux$os_execcmd(qq_lib$convtostringz((*pa).strptr,(*pa).length),flag);
}

void qq_host$pch_makestr(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *result) {
        i64 n;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)16)) {
    }
    else if (($temp==(i64)1)) {
    }
    else {
        qq_runaux$pcerror((byte*)"makestr",(byte*)"");
    }
    };
    n = qq_vars$var_getintvalue(b);
    (*result).tagx = (i64)265;
    (*result).objptr = qq_strings$obj_make_strslicexobj((u8 *)(*a).ptr,n);
}

void qq_host$pch_makeref(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *result) {
        byte *  ptr;
        {i64 $temp = (i64)qq_tables$ttbasetype[((i64)(*a).tag)];
if (($temp==(i64)14) || ($temp==(i64)16) || ($temp==(i64)1)) {
        ptr = (*a).ptr;
    }
    else if (($temp==(i64)9) || ($temp==(i64)11) || ($temp==(i64)10) || ($temp==(i64)5)) {
        ptr = (*(*a).objptr).ptr;
    }
    else {
        qq_runaux$pcerror((byte*)"makeref",(byte*)"");
    }
    };
    (*result).tagx = (i64)16;
    (*result).ptr = ptr;
    (*result).elemtag = qq_vars$var_getintvalue(b);
        {i64 $temp = (i64)(*result).elemtag;
if (($temp==(i64)33) || ($temp==(i64)34) || ($temp==(i64)35)) {
        (*result).tag = (i64)15;
        (*result).bitoffset = (i64)0;
        (*result).bitlength = (i64)0;
    }
    };
}

void qq_host$pch_getcmdparam(struct qq_decls$varrec *a,struct qq_decls$varrec *result) {
        i64 n;
    if (((i64)(*a).tag == (i64)0)) {
        (*result).tagx = (i64)1;
        (*result).value = qq_decls$nqparams;
        return;
    }
;
    n = qq_vars$var_getintvalue(a);
    if (!((($rtemp=n, $rtemp >= (i64)1 && $rtemp <= qq_decls$nqparams)))) {
        qq_runaux$pcerror((byte*)"getcmdpm",(byte*)"");
    }
;
    qq_strings$var_make_string(qq_decls$qparamtable[(n)-1],result,(i64)0);
}

void qq_host$pch_clock(struct qq_decls$varrec *result) {
    (*result).tagx = (i64)1;
    (*result).value = mlinux$os_clock();
}

void qq_host$pch_allocexec(struct qq_decls$varrec *a,struct qq_decls$varrec *result) {
        i64 n;
        byte *  p;
    n = qq_vars$var_getintvalue(a);
    p = mlinux$os_allocexecmem(n);
    (*result).tagx = (i64)16;
    (*result).ptr = p;
    (*result).elemtag = (i64)27;
}

void qq_host$pch_runnative(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *result) {
        i64 (*fnptr)(i64,...);
    if (((i64)(*a).tag != (i64)16)) {
        qq_runaux$pcerror((byte*)"runnative?",(byte*)"");
    }
;
    fnptr = (i64 (*)(i64,...))(*a).ptr;
    (*result).value = ((*fnptr))((*b).value);
    (*result).tagx = (i64)1;
}

void qq_host$pch_setlwb(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        i64 n;
        struct qq_decls$objrec *  p;
    if (!(!!((i64)(*a).hasref))) {
        goto L184 ;
;
    }
;
    p = (*a).objptr;
    if (!(!!(msysc$m_getdotindex((i64)(*p).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    n = qq_host$checkparam(b,(i64)1,(i64)-999999);
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)10)) {
        if (!((($rtemp=n, $rtemp >= (i64)-32768 && $rtemp <= (i64)32767)))) {
            qq_runaux$pcerror((byte*)"lwb not i16",(byte*)"");
        }
;
        (*p).lower16 = n;
    }
    else if (($temp==(i64)11) || ($temp==(i64)8)) {
        if (!((($rtemp=n, $rtemp >= (i64)0 && $rtemp <= (i64)1)))) {
            qq_runaux$pcerror((byte*)"lwb not 0/1",(byte*)"");
        }
;
        (*p).flags = msysc$m_setdotindex((*p).flags,(i64)0,(u64)n);
    }
    else {
        //error:
L184 :;
;
        qq_runaux$pcerror((byte*)"Can't set lwb",(byte*)"");
    }
    };
}

void qq_host$pch_ticks(struct qq_decls$varrec *result) {
    (*result).tagx = (i64)1;
    (*result).value = mlinux$os_ticks();
}

void qq_host$pch_sleep(struct qq_decls$varrec *a) {
    qq_host$checkparam(a,(i64)1,(i64)-999999);
    mlinux$os_sleep((*a).value);
}

void qq_host$pch_random(struct qq_decls$varrec *a,struct qq_decls$varrec *result) {
        i64 n;
        i64 x;
    (*result).tagx = (i64)1;
    if (((i64)(*a).tag == (i64)4)) {
        x = mlib$mrandomrange(msysc$m_getdotslice((*a).dummy,(i64)16,(i64)63),(i64)(*a).range_upper);
    }
    else {
        qq_host$checkparam(a,(i64)1,(i64)-999999);
        n = (*a).value;
        if ((n > (i64)1)) {
            x = mlib$mrandomint(n);
        }
        else if ((n == (i64)0)) {
            x = (i64)mlib$mrandom();
        }
        else if ((n == (i64)1)) {
            (*result).tagx = (i64)2;
            (*result).xvalue = mlib$mrandomreal();
            return;
        }
        else {
            mlib$mseed((u64)-(n),(u64)0u);
            x = (i64)0;
        }
;
    }
;
    (*result).value = x;
}

void qq_host$pch_system(struct qq_decls$varrec *a,struct qq_decls$varrec *result) {
    qq_host$checkparam(a,(i64)9,(i64)-999999);
    (*result).tagx = (i64)1;
    (*result).value = system(qq_lib$convtostringz((*(*a).objptr).strptr,(*(*a).objptr).length));
}

void qq_host$pch_$getparam(struct qq_decls$varrec *a,struct qq_decls$varrec *result) {
    qq_host$checkparam(a,(i64)1,(i64)-999999);
    (*result) = (*(struct qq_decls$varrec *)(qq_decls$frameptr - ((*a).value * (i64)16)));
    if (!!((i64)(*result).hasref)) {
        ++((*(*result).objptr).refcount);
    }
;
}

static i64 qq_host$checkparam(struct qq_decls$varrec *p,i64 tag,i64 defaultx) {
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)0)) {
        if ((defaultx == (i64)-999999)) {
            qq_runaux$pcerror((byte*)"Missing host param",(byte*)"");
        }
;
        return defaultx;
    }
    else if (($temp==tag)) {
        return (*p).value;
    }
    };
    if ((tag == (i64)1)) {
                {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)2)) {
            return (i64)(*p).xvalue;
        }
        };
    }
;
    msysc$m_print_startcon();
    msysc$m_print_str(qq_tables$ttname[((i64)(*p).tag)],NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    qq_runaux$pcerror((byte*)"Host param wrong type",(byte*)"");
    return (i64)0;
}

static void qq_host$leftstring(struct qq_decls$varrec *a,i64 n,struct qq_decls$varrec *result) {
    qq_strings$var_make_stringn((*(*a).objptr).strptr,n,result,(i64)1);
}

static void qq_host$rightstring(struct qq_decls$varrec *a,i64 n,struct qq_decls$varrec *result) {
    qq_strings$var_make_stringn(((*(*a).objptr).strptr + ((*(*a).objptr).length - n)),n,result,(i64)1);
}

static void qq_host$padstring_right(struct qq_decls$varrec *a,i64 n,i64 fillchar,struct qq_decls$varrec *result) {
        u8 *  s;
        i64 length;
        i64 $av_1;
    length = (*(*a).objptr).length;
    qq_strings$var_new_stringn(n,result);
    s = (*(*result).objptr).strptr;
    if (!!(length)) {
        memcpy((void *)s,(void *)(*(*a).objptr).strptr,(u64)length);
        s += length;
    }
;
    $av_1 = (n - length);
    while ($av_1-- > 0) {
L185 :;
        (*s) = (u64)fillchar;
        ++(s);
L186 :;
    }
L187 :;
    ;
}

static void qq_host$padstring_left(struct qq_decls$varrec *a,i64 n,i64 fillchar,struct qq_decls$varrec *result) {
        u8 *  s;
        i64 length;
        i64 padlen;
        i64 $av_1;
    length = (*(*a).objptr).length;
    padlen = (n - length);
    qq_strings$var_make_stringn(0,n,result,(i64)0);
    s = (*(*result).objptr).strptr;
    s += padlen;
    if (!!(length)) {
        memcpy((void *)s,(void *)(*(*a).objptr).strptr,(u64)length);
    }
;
    $av_1 = padlen;
    while ($av_1-- > 0) {
L188 :;
        --(s);
        (*s) = (u64)fillchar;
L189 :;
    }
L190 :;
    ;
}

static void qq_host$getbounds(struct qq_decls$varrec *p,struct qq_host$dimrec *dims,i64 lower) {
        i64 n;
    if (!(!!(p))) {
        qq_runaux$pcerror((byte*)"New: no bounds",(byte*)"");
    }
;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)0)) {
        (*dims).lbound = lower;
        (*dims).upper = (i64)0;
        (*dims).length = (i64)0;
    }
    else if (($temp==(i64)4)) {
        (*dims).lbound = msysc$m_getdotslice((*p).dummy,(i64)16,(i64)63);
        (*dims).upper = (i64)(*p).range_upper;
        (*dims).length = (((i64)(*p).range_upper - msysc$m_getdotslice((*p).dummy,(i64)16,(i64)63)) + (i64)1);
        if (((*dims).length < (i64)0)) {
            (*dims).length = (i64)0;
            (*dims).upper = ((*dims).lbound - (i64)1);
        }
;
    }
    else {
        n = qq_vars$var_getintvalue(p);
        (*dims).lbound = lower;
        (*dims).upper = ((*dims).length = n);
    }
    };
}

void qq_host$pch_new(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *c,struct qq_decls$varrec *d,struct qq_decls$varrec *result) {
        struct qq_decls$varrec v;
        i64 t;
        i64 offset;
        i64 elemtype;
        i64 usertag;
        struct qq_host$dimrec dims;
        byte *  qbyte;
        struct qq_decls$objrec *  p;
        i64 $av_1;
        i64 $av_2;
    t = qq_vars$var_getintvalue(a);
    if (((t < (i64)0) || (t > qq_tables$ntypes))) {
        qq_runaux$pcustype_t((byte*)"New:bad type",t);
    }
;
    v.tagx = (t | (i64)256);
    usertag = (i64)0;
    switch ((i64)qq_tables$ttbasetype[(t)]) {
    case 9:;
        {
            qq_strings$var_new_string(b,c,result);
            return;
        }
        break;
    case 10:;
        {
            qq_host$getbounds(b,&dims,(i64)1);
            p = qq_lists$obj_newlist(dims.length,dims.lbound,c);
            v.objptr = p;
        }
        break;
    case 11:;
        {
            elemtype = qq_vars$var_getintvalue(b);
            qq_host$getbounds(c,&dims,(i64)1);
            if (((elemtype >= (i64)33) && (elemtype <= (i64)35))) {
                v.tag = (t = (i64)8);
                goto L191 ;
;
            }
;
            p = qq_arrays$obj_newarray(elemtype,dims.lbound,dims.length);
            //doarray2:
L192 :;
;
            v.objptr = p;
            if (!!(dims.length)) {
                if ((!!(d) && ((i64)(*d).tag != (i64)0))) {
                    qbyte = (*p).ptr;
                    $av_1 = dims.length;
                    while ($av_1-- > 0) {
L193 :;
                        qq_packed$var_storepacked(qbyte,d,elemtype);
                        qbyte += qq_tables$ttsize[(elemtype)];
L194 :;
                    }
L195 :;
                    ;
                }
;
            }
;
        }
        break;
    case 7:;
        {
            usertag = t;
            v.tag = (i64)7;
            elemtype = (i64)qq_tables$tttarget[(t)];
            dims.length = qq_tables$ttlength[(t)];
            dims.lbound = qq_tables$ttlower[(t)];
            dims.upper = ((dims.length + dims.lbound) - (i64)1);
            d = b;
            p = qq_arrays$obj_newarray_u(t);
            goto L192 ;
;
        }
        break;
    case 8:;
        {
            elemtype = qq_vars$var_getintvalue(b);
            if (!((($rtemp=elemtype, $rtemp >= (i64)33 && $rtemp <= (i64)35)))) {
                qq_runaux$pcerror((byte*)"new: bad bits elem",(byte*)"");
            }
;
            qq_host$getbounds(c,&dims,(i64)1);
            //dobits2:
L191 :;
;
            p = qq_bits$obj_newbits(elemtype,dims.lbound,dims.length);
            v.objptr = p;
            if (!!(dims.length)) {
                if ((!!(d) && ((i64)(*d).tag != (i64)0))) {
                    qbyte = (*p).ptr;
                    offset = (i64)0;
                    $av_2 = dims.length;
                    while ($av_2-- > 0) {
L196 :;
                        qq_vars$var_storebit(qbyte,offset,d,elemtype,(i64)0);
                        offset += (i64)qq_tables$ttbitwidth[(elemtype)];
                        if ((offset >= (i64)8)) {
                            offset = (i64)0;
                            ++(qbyte);
                        }
;
L197 :;
                    }
L198 :;
                    ;
                }
;
            }
;
        }
        break;
    case 5:;
        {
            qq_host$getbounds(b,&dims,(i64)0);
            if ((dims.lbound < (i64)0)) {
                qq_runaux$pcerror((byte*)"new:set:lwb",(byte*)"");
            }
;
            if ((dims.lbound != (i64)0)) {
                dims.lbound = (i64)0;
                dims.length = (dims.upper + (i64)1);
            }
;
            p = qq_sets$obj_newset(dims.length);
            v.objptr = p;
        }
        break;
    case 12:;
        {
            p = qq_records$obj_new_record(t,b);
            qq_vars$var_fromobj(t,p,&v);
            v.tag = (i64)12;
            usertag = t;
        }
        break;
    case 13:;
        {
            p = qq_packed$obj_new_struct(t);
            qq_vars$var_objtovar(t,p,&v);
            v.tag = (i64)13;
            usertag = t;
            if ((!!(b) && ((i64)(*b).tag != (i64)0))) {
                qq_runaux$pcerror((byte*)"New: struct init",(byte*)"");
            }
;
        }
        break;
    case 1:;
    case 2:;
    case 14:;
        {
            v.value = (i64)0;
            v.hasref = (i64)0;
            if ((!!(b) && ((i64)(*b).tag != (i64)0))) {
                qq_runaux$pcerror((byte*)"NEW(int/value)",(byte*)"");
            }
;
        }
        break;
    case 6:;
        {
            qq_host$getbounds(b,&dims,(i64)1);
            if ((dims.lbound != (i64)1)) {
                qq_runaux$pcerror((byte*)"new:dict:lwb",(byte*)"");
            }
;
            p = qq_dicts$obj_new_dict(dims.length);
            v.objptr = p;
        }
        break;
    case 3:;
        {
            qq_decimal$var_empty_dec(result);
            return;
        }
        break;
    default: {
        qq_runaux$pcustype_t((byte*)"new",t);
    }
    } //SW
;
    //finish:
L199 :;
;
    if (!!(usertag)) {
        (*v.objptr).usertag = usertag;
    }
;
    (*result) = v;
}

void qq_host$pch_gethostname(struct qq_decls$varrec *result) {
        static u8 name[256];
    strcpy(name,mlinux$os_gethostname());
    qq_strings$var_make_string(name,result,(i64)0);
}

void qq_host$pch_getprogname(struct qq_decls$varrec *result) {
        static u8 name[256];
    strcpy(name,qq_cli$inputfile);
    qq_strings$var_make_string(name,result,(i64)0);
}

void qq_host$pch_$test(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *c,struct qq_decls$varrec *result) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"$TEST:",NULL);
    msysc$m_print_str(qq_tables$ttname[((i64)(*a).tag)],NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    (*result).tagx = (i64)1;
    (*result).value = (((*a).value + (*b).value) + (*c).value);
}

void qq_host$pch_$test2(struct qq_decls$varrec *a,struct qq_decls$varrec *result) {
    (*result).tagx = (i64)0;
}

void qq_host$pch_$refcount(struct qq_decls$varrec *a,struct qq_decls$varrec *result) {
    (*result).tagx = (i64)1;
    if (!!((i64)(*a).hasref)) {
        (*result).value = ((i64)(*(*a).objptr).refcount - (i64)1);
    }
    else {
        (*result).value = (i64)0;
    }
;
}

void qq_host$pch_testkey(struct qq_decls$varrec *result) {
    (*result).tagx = (i64)1;
    (*result).value = mlinux$os_kbhit();
}

void qq_host$pch_getos(struct qq_decls$varrec *result) {
    qq_strings$var_make_string(mlinux$os_getos(),result,(i64)0);
}

void qq_host$pch_setmesshandler(struct qq_decls$varrec *fn) {
    if ((((i64)(*fn).tag != (i64)17) || ((i64)(*(*fn).def).nameid != (i64)5))) {
        qq_runaux$pcerror((byte*)"Not proc ref",(byte*)"");
    }
;
    qq_decls$pcl_callbackfn = (void (*)(void))(*(*fn).def).labelref;
    mlinux$os_setmesshandler(&qq_runaux$runproc_m);
}

void qq_host$pch_$smallmemtotal(struct qq_decls$varrec *result) {
    (*result).tagx = (i64)1;
    (*result).value = (mlib$smallmemtotal / (i64)16);
}

void qq_host$pch_$id(struct qq_decls$varrec *a,struct qq_decls$varrec *result) {
    (*result).tagx = (i64)1;
    (*result).value = (*a).value;
}

void qq_host$pch_iswindows(struct qq_decls$varrec *result) {
    (*result).tagx = (i64)1;
    (*result).value = mlinux$os_iswindows();
}

void qq_host$pch_$setdebug(struct qq_decls$varrec *a) {
    qq_host$checkparam(a,(i64)1,(i64)-999999);
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"SETDEBUG.................",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mlib$fdebug = (*a).value;
}

void qq_host$pch_copy(struct qq_decls$varrec *a,struct qq_decls$varrec *dest) {
    (*dest) = (*a);
    if (!!((i64)(*dest).hasref)) {
        qq_vars$var_duplu(dest);
    }
;
}

void qq_host$pch_gethash(struct qq_decls$varrec *a,struct qq_decls$varrec *result) {
    (*result).tagx = (i64)1;
    (*result).value = qq_vars$var_gethashvalue(a);
}

void qq_host$pch_makeempty(struct qq_decls$varrec *a,struct qq_decls$varrec *result) {
        struct qq_decls$objrec *  p;
        i64 t;
    t = (i64)qq_tables$ttbasetype[((i64)(*a).tag)];
    if ((t == (i64)18)) {
        t = (*a).value;
    }
;
    p = (*a).objptr;
    if ((t==(i64)10)) {
        qq_lists$var_empty_list((i64)(*p).lower16,result);
        return;
    }
    else if ((t==(i64)9)) {
        p = qq_strings$emptystring;
        ++((*p).refcount);
    }
    else if ((t==(i64)11)) {
        qq_arrays$var_empty_array(t,(i64)(*p).elemtag,(i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0),result);
        return;
    }
    else {
        qq_runaux$pcustype_t((byte*)"makeempty?",t);
    }
;
    (*result).tagx = (t | (i64)256);
    (*result).objptr = p;
}

void qq_host$pch_$infinity(struct qq_decls$varrec *dest) {
    qq_decimal$var_setinf(dest);
}

void qq_host$pch_$nan(struct qq_decls$varrec *dest) {
    qq_decimal$var_setnan(dest);
}

void qq_host$setcmdparam(i64 index,u8 *s) {
    if ((s == 0)) {
        qq_decls$nqparams = index;
    }
    else if ((index <= (i64)32)) {
        qq_decls$qparamtable[(index)-1] = mlib$pcm_copyheapstring(s);
        qq_decls$nqparams=(qq_decls$nqparams>index?qq_decls$nqparams:index);
;
    }
;
}

void qq_host$pch_$nprocs(struct qq_decls$varrec *result) {
    (*result).tagx = (i64)1;
    (*result).value = qq_decls$nproclist;
}

static void qq_host$initprocrefs(void) {
        struct qq_decls$procrec *  pp;
        static i64 oldnprocs;
        i64 i;
    if ((oldnprocs == qq_decls$nproclist)) {
        return;
    }
;
    qq_host$procrefs = (struct qq_decls$strec *(*)[])mlib$pcm_alloc((qq_decls$nproclist * (i64)8));
    pp = (struct qq_decls$procrec *)qq_decls$proclist;
    for (i=(i64)1;i<=qq_decls$nproclist;++i) {
L200 :;
        (*qq_host$procrefs)[(i)-1] = (*pp).def;
        pp = (struct qq_decls$procrec *)(*pp).nextproc;
L201 :;
    }
L202 :;
    ;
    oldnprocs = qq_decls$nproclist;
}

void qq_host$pch_$procname(struct qq_decls$varrec *a,struct qq_decls$varrec *result) {
        i64 n;
    n = qq_host$checkparam(a,(i64)1,(i64)-999999);
    qq_host$initprocrefs();
    qq_strings$var_make_string((*(*qq_host$procrefs)[(n)-1]).name,result,(i64)0);
}

void qq_host$pch_$procref(struct qq_decls$varrec *a,struct qq_decls$varrec *result) {
        i64 n;
    n = qq_host$checkparam(a,(i64)1,(i64)-999999);
    qq_host$initprocrefs();
    (*result).tagx = (i64)17;
    (*result).def = (*qq_host$procrefs)[(n)-1];
}

// START
void qq_host$start(void) {

}

void qq_lex$lexreadtoken(void) {
        i64 c;
        i64 hsum;
        i64 commentseen;
    qq_decls$nextlx.subcode = (i64)0;
    L203 :;
    switch ((qq_lex$lxstart = qq_lex$lxsptr, (i64)(u64)(*(qq_lex$lxsptr)++))) {
    case 97:;
    case 98:;
    case 99:;
    case 100:;
    case 101:;
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
            //dolower:
L205 :;
;
            qq_decls$nextlx.svalue = (qq_lex$lxsptr - (i64)1);
            //doname:
L206 :;
;
            hsum = (i64)(u64)(*qq_decls$nextlx.svalue);
            L207 :;
                        {i64 $temp = (i64)qq_lex$namemap[((c = (i64)(u64)(*(qq_lex$lxsptr)++)))];
if (($temp==(i64)1)) {
                hsum = (((hsum << (i64)4) - hsum) + c);
            }
            else if (($temp==(i64)2)) {
                (*(qq_lex$lxsptr - (i64)1)) = (u64)(c + (i64)32);
                hsum = ((((hsum << (i64)4) - hsum) + c) + (i64)32);
            }
            else {
                --(qq_lex$lxsptr);
                goto L208 ;
            }
            }goto L207 ;
L208 :;
            ;
            qq_lex$lookup(qq_decls$nextlx.svalue,(qq_lex$lxsptr - qq_decls$nextlx.svalue),(((hsum << (i64)5) - hsum) & (i64)32767));
            return;
        }
        break;
    case 65:;
    case 66:;
    case 67:;
    case 68:;
    case 69:;
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
            //doupper:
L209 :;
;
            qq_decls$nextlx.svalue = (qq_lex$lxsptr - (i64)1);
            (*qq_decls$nextlx.svalue) += (u8)(i64)32;
            goto L206 ;
;
        }
        break;
    case 102:;
        {
            if (((u64)(*qq_lex$lxsptr) != '"')) {
                goto L205 ;
;
            }
;
            qq_lex$readrawstring();
            return;
        }
        break;
    case 70:;
        {
            if (((u64)(*qq_lex$lxsptr) != '"')) {
                goto L209 ;
;
            }
;
            qq_lex$readrawstring();
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
                        {u64 $temp = (u64)(*qq_lex$lxsptr);
if (($temp==')') || ($temp==(u64)13u) || ($temp==',') || ($temp==' ')) {
                qq_decls$nextlx.symbol = (i64)63;
                qq_decls$nextlx.subcode = (i64)1;
                qq_decls$nextlx.value = (i64)((u64)(*qq_lex$lxstart) - '0');
            }
            else if (($temp=='x') || ($temp=='X')) {
                                {u64 $temp = (u64)(*qq_lex$lxstart);
if (($temp=='0')) {
                    ++(qq_lex$lxsptr);
                    qq_lex$readhex();
                }
                else if (($temp=='2')) {
                    ++(qq_lex$lxsptr);
                    qq_lex$readbin();
                }
                else {
                    qq_lib$lxerror((byte*)"Bad base");
                }
                };
            }
            else {
                --(qq_lex$lxsptr);
                qq_lex$readdec();
            }
            };
            return;
        }
        break;
    case 33:;
    case 35:;
        {
            //docomment:
L210 :;
;
            L211 :;
                        {i64 $temp = (c = (i64)(u64)(*(qq_lex$lxsptr)++));
if (($temp==(i64)13)) {
                ++(qq_lex$lxsptr);
                ++(qq_lex$lxlineno);
                goto L212 ;
            }
            else if (($temp==(i64)10)) {
                ++(qq_lex$lxlineno);
                goto L212 ;
            }
            else if (($temp==(i64)26) || ($temp==(i64)0)) {
                --(qq_lex$lxsptr);
                goto L212 ;
            }
            }goto L211 ;
L212 :;
            ;
            qq_decls$nextlx.symbol = (i64)61;
            return;
        }
        break;
    case 92:;
        {
            commentseen = (i64)0;
            L213 :;
                        {u64 $temp = (u64)(*(qq_lex$lxsptr)++);
if (($temp==(u64)13u)) {
                ++(qq_lex$lxsptr);
                ++(qq_lex$lxlineno);
                goto L214 ;
            }
            else if (($temp==(u64)10u)) {
                ++(qq_lex$lxlineno);
                goto L214 ;
            }
            else if (($temp==(u64)26u) || ($temp==(u64)0u)) {
                qq_decls$nextlx.symbol = (i64)62;
                --(qq_lex$lxsptr);
                return;
            }
            else if (($temp==' ') || ($temp==(u64)9u)) {
            }
            else if (($temp=='!')) {
                commentseen = (i64)1;
            }
            else {
                if (!(!!(commentseen))) {
                    qq_lib$lxerror((byte*)"\\ not followed by eol");
                }
;
            }
            }goto L213 ;
L214 :;
            ;
            L215 :;
                        {u64 $temp = (u64)(*(qq_lex$lxsptr)++);
if (($temp==(u64)13u)) {
                ++(qq_lex$lxsptr);
                ++(qq_lex$lxlineno);
            }
            else if (($temp==(u64)10u)) {
                ++(qq_lex$lxlineno);
            }
            else if (($temp==' ') || ($temp==(u64)9u)) {
            }
            else {
                --(qq_lex$lxsptr);
                goto L216 ;
            }
            }goto L215 ;
L216 :;
            ;
        }
        break;
    case 123:;
        {
            qq_decls$nextlx.symbol = (i64)13;
            return;
        }
        break;
    case 125:;
        {
            qq_decls$nextlx.symbol = (i64)14;
            return;
        }
        break;
    case 46:;
        {
                        {u64 $temp = (u64)(*qq_lex$lxsptr);
if (($temp=='.')) {
                ++(qq_lex$lxsptr);
                if (((u64)(*qq_lex$lxsptr) == '.')) {
                    ++(qq_lex$lxsptr);
                    qq_decls$nextlx.symbol = (i64)21;
                }
                else {
                    qq_decls$nextlx.symbol = (i64)20;
                    qq_decls$nextlx.subcode = (i64)87;
                }
;
                return;
            }
            else {
                if ((($rtemp=(u64)(*qq_lex$lxsptr), $rtemp >= (i64)48 && $rtemp <= (i64)57))) {
                    --(qq_lex$lxsptr);
                    qq_lex$readreal();
                    return;
                }
                else {
                    qq_decls$nextlx.symbol = (i64)2;
                    return;
                }
;
            }
            };
        }
        break;
    case 44:;
        {
            qq_decls$nextlx.symbol = (i64)3;
            return;
        }
        break;
    case 59:;
        {
            qq_decls$nextlx.symbol = (i64)4;
            return;
        }
        break;
    case 58:;
        {
                        {u64 $temp = (u64)(*qq_lex$lxsptr);
if (($temp=='=')) {
                ++(qq_lex$lxsptr);
                qq_decls$nextlx.symbol = (i64)6;
            }
            else if (($temp==':')) {
                ++(qq_lex$lxsptr);
                                {u64 $temp = (u64)(*qq_lex$lxsptr);
if (($temp=='=')) {
                    ++(qq_lex$lxsptr);
                    qq_decls$nextlx.symbol = (i64)6;
                    qq_decls$nextlx.subcode = (i64)1;
                }
                else {
                    goto L217 ;
;
                }
                };
            }
            else {
                qq_decls$nextlx.symbol = (i64)5;
            }
            };
            return;
        }
        break;
    case 40:;
        {
            qq_decls$nextlx.symbol = (i64)9;
            return;
        }
        break;
    case 41:;
        {
            qq_decls$nextlx.symbol = (i64)10;
            return;
        }
        break;
    case 91:;
        {
            qq_decls$nextlx.symbol = (i64)11;
            return;
        }
        break;
    case 93:;
        {
            qq_decls$nextlx.symbol = (i64)12;
            return;
        }
        break;
    case 124:;
        {
            qq_decls$nextlx.symbol = (i64)16;
            return;
        }
        break;
    case 94:;
        {
            qq_decls$nextlx.symbol = (i64)15;
            qq_decls$nextlx.subcode = (i64)1;
            return;
        }
        break;
    case 64:;
        {
            qq_decls$nextlx.symbol = (i64)17;
            return;
        }
        break;
    case 63:;
        {
            qq_decls$nextlx.symbol = (i64)18;
            return;
        }
        break;
    case 43:;
        {
            qq_decls$nextlx.symbol = (i64)22;
            qq_decls$nextlx.subcode = (i64)100;
            if (((u64)(*qq_lex$lxsptr) == '+')) {
                ++(qq_lex$lxsptr);
                qq_decls$nextlx.symbol = (i64)60;
                qq_decls$nextlx.subcode = (i64)0;
            }
;
            return;
        }
        break;
    case 45:;
        {
            qq_decls$nextlx.symbol = (i64)23;
            qq_decls$nextlx.subcode = (i64)101;
                        {u64 $temp = (u64)(*qq_lex$lxsptr);
if (($temp=='-')) {
                ++(qq_lex$lxsptr);
                qq_decls$nextlx.symbol = (i64)60;
                qq_decls$nextlx.subcode = (i64)1;
            }
            else if (($temp=='>')) {
                ++(qq_lex$lxsptr);
                qq_decls$nextlx.symbol = (i64)8;
            }
            };
            return;
        }
        break;
    case 42:;
        {
            qq_decls$nextlx.symbol = (i64)24;
            qq_decls$nextlx.subcode = (i64)102;
            if (((u64)(*qq_lex$lxsptr) == '*')) {
                ++(qq_lex$lxsptr);
                qq_decls$nextlx.symbol = (i64)41;
                qq_decls$nextlx.subcode = (i64)120;
            }
;
            return;
        }
        break;
    case 47:;
        {
            qq_decls$nextlx.symbol = (i64)25;
            qq_decls$nextlx.subcode = (i64)103;
            return;
        }
        break;
    case 37:;
        {
            qq_decls$nextlx.symbol = (i64)26;
            qq_decls$nextlx.subcode = (i64)104;
            return;
        }
        break;
    case 61:;
        {
                        {u64 $temp = (u64)(*qq_lex$lxsptr);
if (($temp=='>')) {
                qq_decls$nextlx.symbol = (i64)7;
                ++(qq_lex$lxsptr);
            }
            else if (($temp=='=')) {
                qq_decls$nextlx.symbol = (i64)42;
                qq_decls$nextlx.subcode = (i64)119;
                ++(qq_lex$lxsptr);
            }
            else {
                qq_decls$nextlx.symbol = (i64)43;
                qq_decls$nextlx.subcode = (i64)0;
            }
            };
            return;
        }
        break;
    case 60:;
        {
                        {u64 $temp = (u64)(*qq_lex$lxsptr);
if (($temp=='=')) {
                ++(qq_lex$lxsptr);
                qq_decls$nextlx.symbol = (i64)46;
                qq_decls$nextlx.subcode = (i64)3;
            }
            else if (($temp=='>')) {
                ++(qq_lex$lxsptr);
                qq_decls$nextlx.symbol = (i64)44;
                qq_decls$nextlx.subcode = (i64)1;
            }
            else if (($temp=='<')) {
                ++(qq_lex$lxsptr);
                qq_decls$nextlx.symbol = (i64)34;
                qq_decls$nextlx.subcode = (i64)110;
            }
            else {
                qq_decls$nextlx.symbol = (i64)45;
                qq_decls$nextlx.subcode = (i64)2;
            }
            };
            return;
        }
        break;
    case 62:;
        {
                        {u64 $temp = (u64)(*qq_lex$lxsptr);
if (($temp=='=')) {
                ++(qq_lex$lxsptr);
                qq_decls$nextlx.symbol = (i64)47;
                qq_decls$nextlx.subcode = (i64)4;
            }
            else if (($temp=='>')) {
                ++(qq_lex$lxsptr);
                qq_decls$nextlx.symbol = (i64)35;
                qq_decls$nextlx.subcode = (i64)111;
            }
            else {
                qq_decls$nextlx.symbol = (i64)48;
                qq_decls$nextlx.subcode = (i64)5;
            }
            };
            return;
        }
        break;
    case 38:;
        {
                        {u64 $temp = (u64)(*qq_lex$lxsptr);
if (($temp=='&')) {
                ++(qq_lex$lxsptr);
                qq_decls$nextlx.symbol = (i64)38;
                qq_decls$nextlx.subcode = (i64)117;
            }
            else {
                qq_decls$nextlx.symbol = (i64)19;
                qq_decls$nextlx.subcode = (i64)0;
            }
            };
            return;
        }
        break;
    case 39:;
        {
            qq_lex$lxreadstring((i64)39);
            return;
        }
        break;
    case 34:;
        {
            qq_lex$lxreadstring((i64)34);
            return;
        }
        break;
    case 96:;
        {
            qq_lex$readrawxname();
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
            ++(qq_lex$lxsptr);
            ++(qq_lex$lxlineno);
            qq_decls$nextlx.symbol = (i64)61;
            return;
        }
        break;
    case 10:;
        {
            qq_decls$nextlx.symbol = (i64)61;
            ++(qq_lex$lxlineno);
            return;
        }
        break;
    case 26:;
    case 0:;
        {
            qq_decls$nextlx.symbol = (i64)62;
            --(qq_lex$lxsptr);
            return;
        }
        break;
    default: {
        c = (i64)(u64)(*(qq_lex$lxsptr - (i64)1));
        if ((((c == (i64)226) && ((i64)(u64)(*qq_lex$lxsptr) == (i64)136)) && ((i64)(u64)(*(qq_lex$lxsptr + (i64)1)) == (i64)154))) {
            qq_lex$lxsptr += (i64)2;
            qq_decls$nextlx.symbol = (i64)55;
            qq_decls$nextlx.subcode = (i64)1;
            return;
        }
;
        if ((c >= (i64)128)) {
            goto L206 ;
;
        }
;
        //error:
L217 :;
;
        qq_decls$nextlx.symbol = (i64)1;
        qq_decls$nextlx.value = c;
        return;
    }
    } //SW
goto L203 ;
L204 :;
    ;
}

static void qq_lex$lxreadstring(i64 termchar) {
        u8 *  s;
        u8 *  t;
        i64 c;
        i64 length;
        i64 hasescape;
        u8 str[8];
        i64 pass;
    if ((termchar == (i64)34)) {
        qq_decls$nextlx.symbol = (i64)67;
    }
    else {
        qq_decls$nextlx.symbol = (i64)66;
        qq_decls$nextlx.subcode = (i64)1;
    }
;
    length = (i64)0;
    hasescape = (i64)0;
    t = 0;
    for (pass=(i64)1;pass<=(i64)2;++pass) {
L218 :;
        s = qq_lex$lxsptr;
        L221 :;
        while (1) {
                        {i64 $temp = (c = (i64)(u64)(*(s)++));
if (($temp==(i64)92)) {
                hasescape = (i64)1;
                c = (i64)(u64)(*s);
                if (((c >= (i64)65) && (c <= (i64)90))) {
                    c += (i64)32;
                }
;
                ++(s);
                if ((c==(i64)97)) {
                    c = (i64)7;
                }
                else if ((c==(i64)98)) {
                    c = (i64)8;
                }
                else if ((c==(i64)99) || (c==(i64)114)) {
                    c = (i64)13;
                }
                else if ((c==(i64)101)) {
                    c = (i64)27;
                }
                else if ((c==(i64)102)) {
                    c = (i64)12;
                }
                else if ((c==(i64)104)) {
                    L223 :;
                    while (((u64)(*s) != (u64)92u)) {
                        c = qq_lex$readhexcode(&s,(i64)2,(i64)1);
                        if ((pass == (i64)2)) {
                            (*t) = (u64)c;
                        }
;
                        ++(t);
L224 :;
                    }
L225 :;
                    ;
                    ++(s);
                    --(t);
                }
                else if ((c==(i64)108) || (c==(i64)110)) {
                    c = (i64)10;
                }
                else if ((c==(i64)116)) {
                    c = (i64)9;
                }
                else if ((c==(i64)117) || (c==(i64)118)) {
                    t += qq_lex$getutf8(qq_lex$readhexcode(&s,((c == (i64)117) ? (i64)4 : (i64)6),(i64)0),((pass == (i64)2) ? t : 0));
                    goto L221 ;
                }
                else if ((c==(i64)119)) {
                    if ((pass == (i64)2)) {
                        (*t) = (u64)13u;
                    }
;
                    ++(t);
                    c = (i64)10;
                }
                else if ((c==(i64)120)) {
                    c = qq_lex$readhexcode(&s,(i64)2,(i64)0);
                }
                else if ((c==(i64)121)) {
                    c = (i64)16;
                }
                else if ((c==(i64)122)) {
                    c = (i64)0;
                }
                else {
                    if ((c==(i64)34)) {
                        c = (i64)34;
                    }
                    else if ((c==(i64)92)) {
                        c = (i64)92;
                    }
                    else if ((c==(i64)39)) {
                        c = (i64)39;
                    }
                    else if ((c==(i64)48)) {
                        c = (i64)0;
                    }
                    else {
                        str[((i64)1)-1] = (u64)c;
                        str[((i64)2)-1] = (u64)0u;
                        qq_lex$lxerror_s((byte*)"Unknown string escape: \\%s",(u8 *)str);
                    }
;
                }
;
            }
            else if (($temp==(i64)34) || ($temp==(i64)39)) {
                if ((c == termchar)) {
                    if (((i64)(u64)(*s) == c)) {
                        ++(s);
                    }
                    else {
                        goto L222 ;
                    }
;
                }
;
                hasescape = (i64)1;
            }
            else if (($temp==(i64)13) || ($temp==(i64)10) || ($temp==(i64)0)) {
                qq_lib$lxerror((byte*)"String not terminated");
            }
            };
            if ((pass == (i64)2)) {
                (*t) = (u64)c;
            }
;
            ++(t);
        }
L222 :;
        ;
        if ((pass == (i64)1)) {
            length = (i64)t;
            qq_decls$nextlx.slength = (length + (i64)1);
            if (!!(hasescape)) {
                qq_decls$nextlx.svalue = (t = (u8 *)mlib$pcm_alloc((length + (i64)1)));
            }
            else if ((length == (i64)0)) {
                qq_decls$nextlx.svalue = (byte*)"";
                qq_lex$lxsptr = s;
                return;
            }
            else {
                qq_decls$nextlx.svalue = mlib$pcm_copyheapstringn(qq_lex$lxsptr,length);
                qq_lex$lxsptr = s;
                return;
            }
;
        }
        else {
            (*t) = (u64)0u;
            qq_lex$lxsptr = s;
        }
;
L219 :;
    }
L220 :;
    ;
}

static i64 qq_lex$readhexcode(u8 **s,i64 n,i64 sp) {
        i64 a;
        i64 c;
        i64 i;
    a = (i64)0;
    for (i=(i64)1;i<=n;++i) {
L226 :;
        if ((!!(sp) && !!(msysc$m_getdotindex(i,(i64)0)))) {
            L229 :;
            do {
                c = (i64)(u64)(*((*s))++);
L230 :;
            }
            while (!(c != (i64)32));
L231 :;
            ;
        }
        else {
            c = (i64)(u64)(*((*s))++);
        }
;
        if ((($rtemp=c, $rtemp >= (i64)65 && $rtemp <= (i64)70))) {
            a = ((((a * (i64)16) + c) - (i64)65) + (i64)10);
        }
        else if ((($rtemp=c, $rtemp >= (i64)97 && $rtemp <= (i64)102))) {
            a = ((((a * (i64)16) + c) - (i64)97) + (i64)10);
        }
        else if ((($rtemp=c, $rtemp >= (i64)48 && $rtemp <= (i64)57))) {
            a = (((a * (i64)16) + c) - (i64)48);
        }
        else {
            qq_lib$lxerror((byte*)"Bad hex digit");
        }
;
L227 :;
    }
L228 :;
    ;
    return a;
}

static i64 qq_lex$getutf8(i64 c,u8 *s) {
        i64 n;
        u8 str[16];
    if ((s == 0)) {
        s = str;
    }
;
    if ((c <= (i64)127)) {
        n = (i64)1;
        (*(s)++) = (u64)c;
    }
    else if ((c <= (i64)2047)) {
        n = (i64)2;
        (*(s)++) = (u64)((i64)192 + (i64)msysc$m_getdotslice(c,(i64)6,(i64)10));
        (*(s)++) = (u64)((i64)128 + (i64)msysc$m_getdotslice(c,(i64)0,(i64)5));
    }
    else if ((c <= (i64)65535)) {
        n = (i64)3;
        (*(s)++) = (u64)((i64)224 + (i64)msysc$m_getdotslice(c,(i64)12,(i64)15));
        (*(s)++) = (u64)((i64)128 + (i64)msysc$m_getdotslice(c,(i64)6,(i64)11));
        (*(s)++) = (u64)((i64)128 + (i64)msysc$m_getdotslice(c,(i64)0,(i64)5));
    }
    else if ((c <= (i64)1114111)) {
        n = (i64)4;
        (*(s)++) = (u64)((i64)240 + (i64)msysc$m_getdotslice(c,(i64)18,(i64)20));
        (*(s)++) = (u64)((i64)128 + (i64)msysc$m_getdotslice(c,(i64)12,(i64)17));
        (*(s)++) = (u64)((i64)128 + (i64)msysc$m_getdotslice(c,(i64)6,(i64)11));
        (*(s)++) = (u64)((i64)128 + (i64)msysc$m_getdotslice(c,(i64)0,(i64)5));
    }
    else {
        n = (i64)0;
    }
;
    (*s) = (u64)0u;
    return n;
}

void qq_lex$lexinit(void) {
    memset(&qq_lex$hashtable,(i32)(i64)0,(u64)4194304u);
    qq_lex$hashtablelast = &qq_lex$hashtable[((i64)32767)];
    qq_lex$inithashtable();
}

static void qq_lex$readrawstring(void) {
        u8 *  pstart;
        i64 length;
    qq_decls$nextlx.symbol = (i64)67;
    pstart = ++(qq_lex$lxsptr);
    length = (i64)0;
    L232 :;
        {u64 $temp = (u64)(*(qq_lex$lxsptr)++);
if (($temp=='"')) {
        goto L233 ;
    }
    else if (($temp==(u64)13u) || ($temp==(u64)10u) || ($temp==(u64)0u)) {
        qq_lib$lxerror((byte*)"Raw string not terminated");
        --(qq_lex$lxsptr);
        goto L233 ;
    }
    else {
        ++(length);
    }
    }goto L232 ;
L233 :;
    ;
    qq_lex$nextlxlength = length;
    qq_decls$nextlx.svalue = mlib$pcm_copyheapstringn(pstart,length);
}

i64 qq_lex$lookup(u8 *name,i64 length,i64 hashindex) {
        i64 wrapped;
        i64 n;
        struct qq_decls$strec *  d;
    d = &qq_lex$hashtable[(hashindex)];
    wrapped = (i64)0;
    L234 :;
    while (1) {
        if ((((n = (i64)(*d).namelen) == length) && (memcmp((void *)(*d).name,(void *)name,(u64)n) == (i64)0))) {
            qq_decls$nextlx.symptr = (struct qq_decls$strec *)d;
            qq_decls$nextlx.symbol = (i64)(*d).symbolcode;
            qq_decls$nextlx.subcode = (i64)(*d).subcode;
            return (i64)1;
        }
        else if ((n == (i64)0)) {
            goto L235 ;
        }
;
        if ((++(d) > qq_lex$hashtablelast)) {
            if (!!(wrapped)) {
                mlib$abortprogram((byte*)"HASHTABLE FULL");
            }
;
            wrapped = (i64)1;
            d = &qq_lex$hashtable[((i64)0)];
        }
;
    }
L235 :;
    ;
    (*d).name = mlib$pcm_copyheapstringn(name,length);
    (*d).namelen = length;
    (*d).symbolcode = (i64)69;
    qq_decls$nextlx.symptr = (struct qq_decls$strec *)d;
    qq_decls$nextlx.symbol = (i64)(*d).symbolcode;
    qq_decls$nextlx.subcode = (i64)(*d).subcode;
    return (i64)0;
}

i64 qq_lex$gethashvaluez(u8 *s) {
        i64 c;
        i64 hsum;
    if (((i64)(u64)(*s) == (i64)0)) {
        return (i64)0;
    }
;
    hsum = (i64)(u64)(*(s)++);
    L236 :;
    while (1) {
        c = (i64)(u64)(*(s)++);
        if ((c == (i64)0)) {
            goto L237 ;
        }
;
        hsum = (((hsum << (i64)4) - hsum) + c);
    }
L237 :;
    ;
    return (((hsum << (i64)5) - hsum) & (i64)32767);
}

// START
void qq_lex$start(void) {

        i64 c;
    for (c=(i64)0;c<=(i64)255;++c) {
L238 :;
        if (((((($rtemp=c, $rtemp >= (i64)97 && $rtemp <= (i64)122)) || (($rtemp=c, $rtemp >= (i64)48 && $rtemp <= (i64)57))) || (c == '_' || c == '$')) || (($rtemp=c, $rtemp >= (i64)128 && $rtemp <= (i64)255)))) {
            qq_lex$namemap[(c)] = (i64)1;
        }
        else if ((($rtemp=c, $rtemp >= (i64)65 && $rtemp <= (i64)90))) {
            qq_lex$namemap[(c)] = (i64)2;
        }
;
L239 :;
    }
L240 :;
    ;
}

static void qq_lex$inithashtable(void) {
        i64 i;
        u8 *  name;
    for (i=(i64)1;i<=(i64)198;++i) {
L241 :;
        qq_lex$addstname(qq_tables$stnames[(i)-1],(i64)qq_tables$stsymbols[(i)-1],(i64)qq_tables$stsubcodes[(i)-1]);
L242 :;
    }
L243 :;
    ;
    for (i=(i64)1;i<=(i64)59;++i) {
L244 :;
        if (!(!!((i64)qq_tables$hostinternal[(i)]))) {
            name = (qq_tables$hostfnnames[(i)] + (i64)2);
            qq_lex$addstname(name,(i64)129,i);
        }
;
L245 :;
    }
L246 :;
    ;
}

static void qq_lex$addstname(u8 *name,i64 symbol,i64 subcode) {
    if (!!(qq_lex$lookup(name,strlen(name),qq_lex$gethashvaluez(name)))) {
        msysc$m_print_startcon();
        msysc$m_print_str(name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        mlib$abortprogram((byte*)"Dupl ST entry");
    }
;
    (*qq_decls$nextlx.symptr).symbolcode = symbol;
    (*qq_decls$nextlx.symptr).subcode = subcode;
}

void qq_lex$startlex(struct qq_decls$filerec *pm) {
    qq_lex$lxsource = (qq_lex$lxsptr = mlib$pcm_copyheapstring((*pm).text));
    qq_decls$nextlx.symbol = (i64)4;
    qq_decls$nextlx.subcode = (i64)0;
    qq_decls$nextlx.pos = msysc$m_setdotslice(qq_decls$nextlx.pos,(i64)24,(i64)31,(i64)(*pm).moduleno);
    qq_lex$lxlineno = (i64)1;
}

struct qq_decls$strec *qq_lex$addnamestr(u8 *name) {
        struct qq_decls$lexrec oldlx;
        struct qq_decls$strec *  symptr;
    oldlx = qq_decls$nextlx;
    qq_lex$nextlxlength = strlen(name);
    qq_decls$nextlx.svalue = (u8 *)mlib$pcm_alloc((qq_lex$nextlxlength + (i64)1));
    memcpy((void *)qq_decls$nextlx.svalue,(void *)name,(u64)(qq_lex$nextlxlength + (i64)1));
    qq_lex$lookup(qq_decls$nextlx.svalue,qq_lex$nextlxlength,qq_lex$gethashvaluez(name));
    symptr = (struct qq_decls$strec *)qq_decls$nextlx.symptr;
    qq_decls$nextlx = oldlx;
    return symptr;
}

void qq_lex$ps(u8 *caption) {
    msysc$m_print_startcon();
    msysc$m_print_str(caption,NULL);
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)":::",NULL);
    msysc$m_print_end();
    ;
    qq_show$printsymbol(&qq_decls$lx);
}

void qq_lex$psnext(u8 *caption) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"\t",NULL);
    msysc$m_print_nogap();
    msysc$m_print_str(caption,NULL);
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)":##",NULL);
    msysc$m_print_end();
    ;
    qq_show$printsymbol(&qq_decls$nextlx);
}

void qq_lex$lex(void) {
    qq_decls$lx = qq_decls$nextlx;
    qq_decls$lx.pos = msysc$m_setdotslice(qq_decls$lx.pos,(i64)0,(i64)23,(u64)qq_lex$lxlineno);
    qq_lex$lxlength = qq_lex$nextlxlength;
    //reenter:
L247 :;
;
    qq_lex$lexreadtoken();
    //reenter2:
L248 :;
;
        {i64 $temp = (i64)qq_decls$nextlx.symbol;
if (($temp==(i64)68)) {
                {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)63)) {
                        {i64 $temp = (i64)(*qq_decls$nextlx.symptr).subcode;
if (($temp==(i64)2)) {
                qq_decls$lx.value *= (i64)1000000;
            }
            else if (($temp==(i64)3)) {
                qq_decls$lx.value *= (i64)1000000000;
            }
            else if (($temp==(i64)1)) {
                qq_decls$lx.value *= (i64)1000;
            }
            else {
                qq_lib$lxerror((byte*)"Can't do this unit index");
            }
            };
            qq_decls$lx.subcode = (i64)1;
            goto L247 ;
;
        }
        else if (($temp==(i64)65)) {
            qq_lib$lxerror((byte*)"unit symbol after float?");
        }
        else {
            qq_decls$nextlx.symbol = (i64)69;
        }
        };
    }
    else if (($temp==(i64)128)) {
                {i64 $temp = (i64)qq_decls$nextlx.subcode;
if (($temp==(i64)3)) {
            qq_decls$nextlx.symbol = (i64)63;
            qq_decls$nextlx.value = (i64)0;
            qq_decls$nextlx.subcode = (i64)1;
        }
        else if (($temp==(i64)1)) {
            qq_decls$nextlx.symbol = (i64)65;
            qq_decls$nextlx.xvalue = (double)3.1415926535897931;
            qq_decls$nextlx.subcode = (i64)2;
        }
        else if (($temp==(i64)2)) {
            qq_decls$nextlx.symbol = (i64)67;
            qq_decls$nextlx.svalue = (byte*)"\t";
            qq_lex$nextlxlength = (i64)1;
        }
        else if (($temp==(i64)4)) {
            qq_decls$nextlx.symbol = (i64)63;
            qq_decls$nextlx.value = (i64)1;
            qq_decls$nextlx.subcode = (i64)1;
        }
        else if (($temp==(i64)5)) {
            qq_decls$nextlx.symbol = (i64)63;
            qq_decls$nextlx.value = (i64)0;
            qq_decls$nextlx.subcode = (i64)1;
        }
        else {
            qq_lib$lxerror((byte*)"sysconst?");
        }
        };
    }
    else if (($temp==(i64)61)) {
                {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)3) || ($temp==(i64)11) || ($temp==(i64)9) || ($temp==(i64)6) || ($temp==(i64)4)) {
            qq_lex$lexreadtoken();
            goto L248 ;
;
            goto L247 ;
;
        }
        else {
            if ((!!((i64)qq_tables$binopset[((i64)qq_decls$lx.symbol)]) && ((i64)qq_decls$lx.symbol != (i64)36))) {
                qq_lex$lexreadtoken();
                goto L248 ;
;
            }
;
        }
        };
        qq_decls$nextlx.symbol = (i64)4;
    }
    else if (($temp==(i64)39)) {
        if (((i64)qq_decls$lx.symbol == (i64)49)) {
            qq_decls$lx.symbol = (i64)39;
            qq_decls$lx.subcode = (i64)1;
            goto L247 ;
;
        }
;
    }
    };
}

void qq_lex$lxerror_s(u8 *mess,u8 *a) {
        u8 str[256];
    msysc$m_print_startstr(str);
    msysc$m_print_setfmt(mess);
    msysc$m_print_str(a,NULL);
    msysc$m_print_end();
    ;
    qq_lib$lxerror((u8 *)str);
}

static void qq_lex$makedecimal(u8 *s,i64 length,i64 base) {
    if ((base != (i64)10)) {
        qq_lib$lxerror((byte*)"MAKEDECIMAL/16/2");
    }
;
    qq_decls$nextlx.symbol = (i64)64;
    qq_decls$nextlx.subcode = (i64)3;
    qq_decls$nextlx.svalue = mlib$pcm_copyheapstringn(s,length);
    qq_lex$nextlxlength = length;
}

static void qq_lex$readdec(void) {
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
    pstart = qq_lex$lxsptr;
    dest = (u8 *)str;
    destend = ((dest + (i64)1024) - (i64)10);
    a = (u64)0u;
    L249 :;
    while (1) {
        if ((($rtemp=(c = (i64)(u64)(*(qq_lex$lxsptr)++)), $rtemp >= (i64)48 && $rtemp <= (i64)57))) {
            a = (u64)((((i64)a * (i64)10) + c) - (i64)48);
            (*(dest)++) = (u64)c;
        }
        else {
            if ((c==(i64)101) || (c==(i64)69)) {
                qq_lex$lxsptr = pstart;
                qq_lex$readreal();
                return;
            }
            else if ((c==(i64)46)) {
                if (((u64)(*qq_lex$lxsptr) != '.')) {
                    qq_lex$lxsptr = pstart;
                    qq_lex$readreal();
                    return;
                }
;
                --(qq_lex$lxsptr);
                goto L250 ;
            }
            else if ((c==(i64)95) || (c==(i64)39)) {
            }
            else if ((c==(i64)108) || (c==(i64)76)) {
                (*dest) = (u64)0u;
                qq_lex$makedecimal((u8 *)str,(dest - str),(i64)10);
                return;
            }
            else if ((c==(i64)98) || (c==(i64)66)) {
                length = (dest - str);
                if ((length > (i64)64)) {
                    qq_lib$lxerror((byte*)"bin overflow");
                }
;
                dest = (u8 *)str;
                a = (u64)0u;
                $av_1 = length;
                while ($av_1-- > 0) {
L251 :;
                    if (((u64)(*dest) >= '2')) {
                        qq_lib$lxerror((byte*)"bad bin digit");
                    }
;
                    a = (u64)((((i64)a * (i64)2) + (i64)(u64)(*(dest)++)) - (i64)48);
L252 :;
                }
L253 :;
                ;
                goto L254 ;
;
            }
            else {
                --(qq_lex$lxsptr);
                goto L250 ;
            }
;
        }
;
        if ((dest >= destend)) {
            qq_lib$lxerror((byte*)"Numlit too long");
        }
;
    }
L250 :;
    ;
    length = (dest - str);
    if (((length > (i64)20) || ((length == (i64)20) && (strncmp((u8 *)str,qq_lex$u64maxstr,(u64)20u) > (i64)0)))) {
        qq_lex$makedecimal((u8 *)str,length,(i64)10);
        return;
    }
;
    //finish:
L254 :;
;
    qq_decls$nextlx.symbol = (i64)63;
    qq_decls$nextlx.subcode = (i64)1;
    qq_decls$nextlx.value = (i64)a;
}

static void qq_lex$readhex(void) {
        i64 c;
        u8 *  dest;
        u8 *  destend;
        u8 *  pstart;
        i64 length;
        byte str[1024];
        u64 a;
    pstart = qq_lex$lxsptr;
    dest = (u8 *)str;
    destend = ((dest + (i64)1024) - (i64)10);
    a = (u64)0u;
    L255 :;
    while (1) {
        if ((($rtemp=(c = (i64)(u64)(*(qq_lex$lxsptr)++)), $rtemp >= (i64)48 && $rtemp <= (i64)57))) {
            a = (u64)((((i64)a * (i64)16) + c) - (i64)48);
            (*(dest)++) = (u64)c;
        }
        else if ((($rtemp=c, $rtemp >= (i64)65 && $rtemp <= (i64)70))) {
            (*(dest)++) = (u64)c;
            a = (u64)(((((i64)a * (i64)16) + c) - (i64)65) + (i64)10);
        }
        else if ((($rtemp=c, $rtemp >= (i64)97 && $rtemp <= (i64)102))) {
            (*(dest)++) = (u64)(c - (i64)32);
            a = (u64)(((((i64)a * (i64)16) + c) - (i64)97) + (i64)10);
        }
        else {
            if ((c==(i64)95) || (c==(i64)39)) {
            }
            else if ((c==(i64)108) || (c==(i64)76)) {
                (*dest) = (u64)0u;
                qq_lex$makedecimal((u8 *)str,(dest - str),(i64)16);
                return;
            }
            else if ((c==(i64)46)) {
                --(qq_lex$lxsptr);
                goto L256 ;
            }
            else {
                --(qq_lex$lxsptr);
                goto L256 ;
            }
;
        }
;
        if ((dest >= destend)) {
            qq_lib$lxerror((byte*)"Numlit too long");
        }
;
    }
L256 :;
    ;
    length = (dest - str);
    if ((length > (i64)16)) {
        qq_lex$makedecimal((u8 *)str,length,(i64)16);
        return;
    }
;
    qq_decls$nextlx.symbol = (i64)63;
    qq_decls$nextlx.subcode = (i64)1;
    qq_decls$nextlx.value = (i64)a;
}

static void qq_lex$readbin(void) {
        i64 c;
        u8 *  dest;
        u8 *  destend;
        u8 *  pstart;
        i64 length;
        byte str[1024];
        u64 a;
    pstart = qq_lex$lxsptr;
    dest = (u8 *)str;
    destend = ((dest + (i64)1024) - (i64)10);
    a = (u64)0u;
    L257 :;
    while (1) {
                {i64 $temp = (c = (i64)(u64)(*(qq_lex$lxsptr)++));
if (($temp==(i64)48) || ($temp==(i64)49)) {
            a = (u64)((((i64)a * (i64)2) + c) - (i64)48);
            (*(dest)++) = (u64)c;
        }
        else if (($temp==(i64)95) || ($temp==(i64)39)) {
        }
        else if (($temp==(i64)108) || ($temp==(i64)76)) {
            (*dest) = (u64)0u;
            qq_lex$makedecimal((u8 *)str,(dest - str),(i64)2);
            return;
        }
        else if (($temp==(i64)46)) {
            --(qq_lex$lxsptr);
            goto L258 ;
        }
        else {
            if ((($rtemp=c, $rtemp >= (i64)50 && $rtemp <= (i64)57))) {
                qq_lib$lxerror((byte*)"bin bad digit");
            }
            else {
                --(qq_lex$lxsptr);
                goto L258 ;
            }
;
        }
        };
        if ((dest >= destend)) {
            qq_lib$lxerror((byte*)"bin overflow");
        }
;
    }
L258 :;
    ;
    length = (dest - str);
    if ((length > (i64)64)) {
        qq_lex$makedecimal((u8 *)str,length,(i64)2);
        return;
    }
;
    qq_decls$nextlx.symbol = (i64)63;
    qq_decls$nextlx.subcode = (i64)1;
    qq_decls$nextlx.value = (i64)a;
}

static void qq_lex$readreal(void) {
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
    L259 :;
    while (1) {
        if ((($rtemp=(c = (i64)(u64)(*(qq_lex$lxsptr)++)), $rtemp >= (i64)48 && $rtemp <= (i64)57))) {
            (*(dest)++) = (u64)c;
            ++(length);
            if (!!(dotseen)) {
                ++(fractlen);
            }
;
        }
        else {
            if ((c==(i64)46)) {
                if (!!(dotseen)) {
                    --(qq_lex$lxsptr);
                    goto L260 ;
                }
;
                dotseen = (i64)1;
                (*(dest)++) = (u64)c;
            }
            else if ((c==(i64)101) || (c==(i64)69)) {
                if (!!(expseen)) {
                    qq_lib$lxerror((byte*)"double expon");
                }
;
                expseen = (i64)1;
                (*(dest)++) = (u64)c;
                L261 :;
                while (((u64)(*qq_lex$lxsptr) == ' ')) {
                    ++(qq_lex$lxsptr);
L262 :;
                }
L263 :;
                ;
                if (((u64)(*qq_lex$lxsptr) == '+' || (u64)(*qq_lex$lxsptr) == '-')) {
                    if (((u64)(*qq_lex$lxsptr) == '-')) {
                        negexpon = (i64)1;
                    }
;
                    (*(dest)++) = (u64)(*(qq_lex$lxsptr)++);
                }
;
                expon = (i64)0;
                L264 :;
                while (1) {
                    if ((($rtemp=(c = (i64)(u64)(*(qq_lex$lxsptr)++)), $rtemp >= (i64)48 && $rtemp <= (i64)57))) {
                        expon = (((expon * (i64)10) + c) - (i64)48);
                        (*(dest)++) = (u64)c;
                        if ((dest >= destend)) {
                            qq_lib$lxerror((byte*)"expon?");
                        }
;
                    }
                    else {
                        if ((c==(i64)95) || (c==(i64)39)) {
                        }
                        else if ((c==(i64)108) || (c==(i64)76)) {
                            (*dest) = (u64)0u;
                            qq_lex$makedecimal((u8 *)str,(dest - str),(i64)10);
                            return;
                        }
                        else {
                            --(qq_lex$lxsptr);
                            goto L260 ;
                        }
;
                    }
;
                }
L265 :;
                ;
            }
            else if ((c==(i64)95) || (c==(i64)39)) {
            }
            else if ((c==(i64)108) || (c==(i64)76)) {
                qq_lex$makedecimal((u8 *)str,(dest - str),(i64)10);
                return;
            }
            else {
                --(qq_lex$lxsptr);
                goto L260 ;
            }
;
        }
;
        if ((dest >= destend)) {
            qq_lib$lxerror((byte*)"r64lit too long");
        }
;
    }
L260 :;
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
L266 :;
        c = (i64)(u64)str[(i)-1];
        if ((c != (i64)46)) {
            x = (((x * (double)10.) + (r64)c) - (r64)'0');
        }
;
L267 :;
    }
L268 :;
    ;
    if ((expon >= (i64)0)) {
        $av_2 = expon;
        while ($av_2-- > 0) {
L269 :;
            x *= (double)10.;
L270 :;
        }
L271 :;
        ;
    }
    else {
        $av_3 = -(expon);
        while ($av_3-- > 0) {
L272 :;
            x /= (double)10.;
L273 :;
        }
L274 :;
        ;
    }
;
    qq_decls$nextlx.xvalue = x;
    qq_decls$nextlx.symbol = (i64)65;
    qq_decls$nextlx.subcode = (i64)2;
}

static void qq_lex$readrawxname(void) {
        i64 c;
        i64 hsum;
    qq_decls$nextlx.svalue = qq_lex$lxsptr;
    hsum = (i64)0;
    L275 :;
    while (!!((i64)qq_lex$namemap[((c = (i64)(u64)(*(qq_lex$lxsptr)++)))])) {
        hsum = (((hsum << (i64)4) - hsum) + c);
L276 :;
    }
L277 :;
    ;
    --(qq_lex$lxsptr);
    qq_lex$lookup(qq_decls$nextlx.svalue,(qq_lex$lxsptr - qq_decls$nextlx.svalue),(((hsum << (i64)5) - hsum) & (i64)32767));
    return;
}

void qq_lib$reportcterror(u8 *errortype,u8 *mess,i64 pos,struct qq_decls$strec *currproc) {
        struct qq_decls$locrec loc;
    loc = qq_lib$geterrorinfo((u64)pos,currproc);
    msysc$m_print_startcon();
    msysc$m_print_str(errortype,NULL);
    msysc$m_print_str((byte*)"Error:",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"    ",NULL);
    msysc$m_print_nogap();
    msysc$m_print_str(mess,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    qq_lib$showerrorsource(loc);
    qq_lib$stopcompiler(loc);
}

struct qq_decls$locrec qq_lib$geterrorinfo(u64 pos,struct qq_decls$strec *currproc) {
        i64 moduleno;
        struct qq_decls$locrec loc;
    memset(&(loc),0,48);
    loc.lineno = (i64)msysc$m_getdotslice(pos,(i64)0,(i64)23);
    moduleno = (i64)msysc$m_getdotslice(pos,(i64)24,(i64)31);
    if ((moduleno == (i64)0)) {
        mlib$abortprogram((byte*)"GETERRORINFO: no module");
    }
;
    if ((currproc == 0)) {
        mlib$abortprogram((byte*)"GETERRORINFO: no currproc");
    }
;
    loc.pm = qq_decls$modules[(moduleno)];
    loc.sp = qq_decls$subprogs[((i64)(*loc.pm).subprogno)-1];
    loc.def = currproc;
    return loc;
}

static void qq_lib$showerrorsource(struct qq_decls$locrec loc) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Line:",NULL);
    msysc$m_print_i64(loc.lineno,NULL);
    msysc$m_print_str((byte*)"in Module",NULL);
    msysc$m_print_str((*loc.pm).name,NULL);
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)".q:",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    if (!!(loc.def)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"In function:",NULL);
        msysc$m_print_str((*loc.def).name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
}

void qq_lib$stopcompiler(struct qq_decls$locrec loc) {
        void *  f;
    f = fopen((byte*)"$error.tmp",(byte*)"w");
    msysc$m_print_startfile(f);
    msysc$m_print_str((*loc.pm).filespec,NULL);
    msysc$m_print_i64(loc.lineno,NULL);
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

void qq_lib$gerror(u8 *mess,struct qq_decls$unitrec *p) {
    qq_lib$reportcterror((byte*)"Code Gen",mess,(!!(p) ? (i64)(*p).pos : qq_decls$qpos),qq_decls$stcurrproc);
}

void qq_lib$gerror_s(u8 *mess,u8 *param,struct qq_decls$unitrec *p) {
        u8 str[300];
    msysc$m_print_startstr(str);
    msysc$m_print_str(mess,NULL);
    msysc$m_print_str(param,NULL);
    msysc$m_print_end();
    ;
    qq_lib$reportcterror((byte*)"Code Gen",(u8 *)str,(!!(p) ? (i64)(*p).pos : qq_decls$qpos),qq_decls$stcurrproc);
}

void qq_lib$serror(u8 *mess) {
    qq_lib$reportcterror((byte*)"Syntax",mess,(i64)qq_decls$lx.pos,qq_decls$stcurrproc);
}

void qq_lib$serror_s(u8 *mess,u8 *param) {
        u8 str[300];
    strcpy((u8 *)str,mess);
    strcat((u8 *)str,(byte*)" ");
    strcat((u8 *)str,param);
    qq_lib$reportcterror((byte*)"Syntax",str,(i64)qq_decls$lx.pos,qq_decls$stcurrproc);
}

void qq_lib$rxerror(u8 *mess,struct qq_decls$unitrec *p) {
    qq_lib$reportcterror((byte*)"Resolve",mess,(!!(p) ? (i64)(*p).pos : qq_decls$qpos),qq_decls$stcurrproc);
}

void qq_lib$rxerror_s(u8 *mess,u8 *param,struct qq_decls$unitrec *p) {
        u8 str[300];
    strcpy((u8 *)str,mess);
    strcat((u8 *)str,(byte*)" ");
    strcat((u8 *)str,param);
    qq_lib$rxerror(str,p);
}

void qq_lib$lxerror(u8 *mess) {
    qq_lib$reportcterror((byte*)"Lex",mess,(i64)qq_decls$lx.pos,qq_decls$stcurrproc);
}

void qq_lib$loaderror(u8 *mess,u8 *mess2) {
        u8 str[512];
    if (!!(strchr(mess,(i32)'#'))) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt(mess);
        msysc$m_print_str(mess2,NULL);
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

void qq_lib$prterror(u8 *mess) {
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Print error:",NULL);
    msysc$m_print_str(mess,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    mlinux$os_getch();
    exit((i64)1);
}

static struct qq_decls$unitrec *qq_lib$allocunitrec(void) {
        struct qq_decls$unitrec *  p;
    p = (struct qq_decls$unitrec *)mlib$pcm_allocnfz((i64)32);
    (*p).pos = (i64)qq_decls$lx.pos;
    return p;
}

struct qq_decls$unitrec *qq_lib$createintunit(i64 a) {
        struct qq_decls$unitrec *  u;
    u = qq_lib$allocunitrec();
    (*u).tag = (i64)41;
    (*u).value = a;
    return u;
}

struct qq_decls$unitrec *qq_lib$createrealunit(r64 x) {
        struct qq_decls$unitrec *  u;
    u = qq_lib$allocunitrec();
    (*u).tag = (i64)42;
    (*u).xvalue = x;
    return u;
}

struct qq_decls$unitrec *qq_lib$createstringunit(u8 *s,i64 slength) {
        struct qq_decls$unitrec *  u;
    if ((slength == (i64)-1)) {
        slength = strlen(s);
    }
;
    u = qq_lib$allocunitrec();
    (*u).tag = (i64)43;
    (*u).svalue = (u8 *)mlib$pcm_alloc((slength + (i64)1));
    if (!!(slength)) {
        memcpy((void *)(*u).svalue,(void *)s,(u64)slength);
    }
;
    (*((*u).svalue + slength)) = (u64)0u;
    (*u).slength = slength;
    return u;
}

struct qq_decls$unitrec *qq_lib$createunit0(i64 tag) {
        struct qq_decls$unitrec *  u;
    u = qq_lib$allocunitrec();
    (*u).tag = tag;
    return u;
}

struct qq_decls$unitrec *qq_lib$createunit1(i64 tag,struct qq_decls$unitrec *p) {
        struct qq_decls$unitrec *  u;
    u = qq_lib$allocunitrec();
    (*u).tag = tag;
    (*u).a = p;
    return u;
}

struct qq_decls$unitrec *qq_lib$createunit2(i64 tag,struct qq_decls$unitrec *p,struct qq_decls$unitrec *q) {
        struct qq_decls$unitrec *  u;
    u = qq_lib$allocunitrec();
    (*u).tag = tag;
    (*u).a = p;
    (*u).b = q;
    return u;
}

struct qq_decls$unitrec *qq_lib$createname(struct qq_decls$strec *p) {
        struct qq_decls$unitrec *  u;
    u = (struct qq_decls$unitrec *)qq_lib$allocunitrec();
    (*u).tag = (i64)39;
    (*u).def = (struct qq_decls$strec *)p;
    return (struct qq_decls$unitrec *)u;
}

void qq_lib$addlistunit(struct qq_decls$unitrec **ulist,struct qq_decls$unitrec **ulistx,struct qq_decls$unitrec *p) {
    L278 :;
    while (!!(p)) {
        if (((*ulist) == 0)) {
            (*ulist) = ((*ulistx) = p);
        }
        else {
            (*(*ulistx)).nextunit = p;
        }
;
        (*ulistx) = p;
        p = (*p).nextunit;
L279 :;
    }
L280 :;
    ;
}

struct qq_decls$unitrec *qq_lib$createavname(void) {
        struct qq_decls$strec *  p;
        u8 str[32];
        u8 *  name;
    msysc$m_print_startstr(str);
    msysc$m_print_str((byte*)"av$",NULL);
    msysc$m_print_nogap();
    msysc$m_print_i64(++(qq_lib$nextavindex),NULL);
    msysc$m_print_end();
    ;
    name = mlib$pcm_copyheapstring((u8 *)str);
    p = qq_lex$addnamestr(name);
    return (struct qq_decls$unitrec *)qq_lib$createname((struct qq_decls$strec *)p);
}

u8 *qq_lib$convtostringz(u8 *svalue,i64 length) {
        static u8 strbuffer1[2000];
        static u8 strbuffer2[2000];
        static u8 strbuffer3[2000];
        static u8 strbuffer4[2000];
        static u8 strbuffer5[2000];
        static u8 strbuffer6[2000];
        static i64 strindex = (i64)0;
        static u8 (*table[6])[] = {(u8 (*)[])&strbuffer1,(u8 (*)[])&strbuffer2,(u8 (*)[])&strbuffer3,(u8 (*)[])&strbuffer4,(u8 (*)[])&strbuffer5,(u8 (*)[])&strbuffer6};
        u8 (*p)[];
        static u8 *  longstr = 0;
    if ((length >= (i64)2000)) {
        if (!!(longstr)) {
            free((void *)longstr);
        }
;
        longstr = (u8 *)malloc((u64)(length + (i64)1));
        memcpy((void *)longstr,(void *)svalue,(u64)length);
        (*(longstr + length)) = (u64)0u;
        return longstr;
    }
;
    if ((svalue == 0)) {
        return (byte*)"";
    }
;
    if ((++(strindex) == (i64)6)) {
        strindex = (i64)0;
    }
;
    p = (u8 (*)[])table[(strindex)];
    memcpy(p,(void *)svalue,(u64)length);
    (*p)[(length)] = (u64)0u;
    return (u8 *)p;
}

u8 *qq_lib$findprocname(void (*fnptr)(void)) {
        i64 n;
        i64 i;
    n = msysc$m_get_nprocs();
    for (i=(i64)1;i<=n;++i) {
L281 :;
        if ((msysc$m_get_procaddr(i) == fnptr)) {
            return msysc$m_get_procname(i);
        }
;
L282 :;
    }
L283 :;
    ;
    return (byte*)"?";
}

struct mlib$strbuffer *qq_lib$strexpr(struct qq_decls$unitrec *p) {
    mlib$gs_init((struct mlib$strbuffer *)qq_lib$exprstr);
    qq_lib$jeval(p);
    return (struct mlib$strbuffer *)qq_lib$exprstr;
}

u8 *qq_lib$strexpr_s(struct qq_decls$unitrec *p) {
    if ((p == 0)) {
        return (byte*)"";
    }
;
    mlib$gs_init((struct mlib$strbuffer *)qq_lib$exprstr);
    qq_lib$jeval(p);
    return (*qq_lib$exprstr).strptr;
}

static void qq_lib$jeval(struct qq_decls$unitrec *p) {
        struct qq_decls$unitrec *  q;
        u8 str[500];
        i64 i;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)41)) {
        qq_lib$additem(msysc$strint((*p).value,0));
    }
    else if (($temp==(i64)42)) {
        qq_lib$additem(msysc$strreal((r64)(*p).value,0));
    }
    else if (($temp==(i64)43)) {
        if (((*p).slength > (i64)250)) {
            strcpy((u8 *)str,(byte*)"LONGSTR)");
        }
        else {
            qq_lib$convertstring((*p).svalue,(u8 *)str);
        }
;
        qq_lib$additem((byte*)"\"");
        qq_lib$additem((u8 *)str);
        qq_lib$additem((byte*)"\"");
    }
    else if (($temp==(i64)39)) {
        qq_lib$additem((*(*p).def).name);
    }
    else if (($temp==(i64)27)) {
        qq_lib$additem((byte*)"Host<");
        qq_lib$additem((qq_tables$hostfnnames[((*p).index)] + (i64)2));
        qq_lib$additem((byte*)">(");
        q = (*p).a;
        L284 :;
        while (!!(q)) {
            qq_lib$jeval(q);
            q = (*q).nextunit;
            if (!!(q)) {
                qq_lib$additem((byte*)",");
            }
;
L285 :;
        }
L286 :;
        ;
        qq_lib$additem((byte*)")");
    }
    else if (($temp==(i64)47) || ($temp==(i64)48)) {
        qq_lib$jeval((*p).a);
        if (((i64)(*p).tag == (i64)48)) {
            qq_lib$additem((byte*)".");
        }
;
        qq_lib$additem((byte*)"[");
        qq_lib$jeval((*p).b);
        qq_lib$additem((byte*)"]");
    }
    else if (($temp==(i64)49)) {
        qq_lib$jeval((*p).a);
        qq_lib$additem((byte*)"{");
        qq_lib$jeval((*p).b);
        qq_lib$additem((byte*)"}");
    }
    else if (($temp==(i64)46)) {
        qq_lib$jeval((*p).a);
        qq_lib$additem((byte*)".");
        qq_lib$jeval((*p).b);
    }
    else if (($temp==(i64)2)) {
        qq_lib$jeval((*p).a);
        qq_lib$additem((byte*)":=");
        qq_lib$jeval((*p).b);
    }
    else if (($temp==(i64)34)) {
        qq_lib$additem(qq_show$strmode((i64)(*p).mode,(i64)0));
    }
    else if (($temp==(i64)35)) {
        qq_lib$additem(qq_show$strmode((i64)(*p).mode,(i64)0));
        qq_lib$additem((byte*)"(");
        qq_lib$jeval((*p).a);
        qq_lib$additem((byte*)")");
    }
    else if (($temp==(i64)4)) {
        qq_lib$jeval((*p).a);
        qq_lib$additem((byte*)":");
        qq_lib$jeval((*p).b);
    }
    else if (($temp==(i64)28)) {
        qq_lib$additem((byte*)"nil");
    }
    else if (($temp==(i64)40)) {
        qq_lib$jeval((*p).a);
        qq_lib$additem((byte*)".$");
    }
    else if (($temp==(i64)38)) {
        qq_lib$additem((byte*)"CMPCHAIN:");
        q = (*p).a;
        qq_lib$jeval(q);
        for (i=(i64)1;i<=(i64)4;++i) {
L287 :;
            q = (*q).nextunit;
            if (((i64)(*p).cmpconds[(i)-1] == (i64)0)) {
                goto L289 ;
            }
;
            qq_lib$additem(qq_tables$jtagnames[((i64)(*p).cmpconds[(i)-1])]);
            qq_lib$jeval(q);
L288 :;
        }
L289 :;
        ;
    }
    else {
        if (((i64)qq_tables$jflags[((i64)(*p).tag)] == (i64)2)) {
            strcpy((u8 *)str,qq_lib$getopcname((i64)(*p).tag));
            qq_lib$additem((byte*)"(");
            qq_lib$jevallist((*p).a);
            qq_lib$additem((u8 *)str);
            qq_lib$jevallist((*p).b);
            qq_lib$additem((byte*)")");
        }
        else if (((i64)qq_tables$jflags[((i64)(*p).tag)] == (i64)1)) {
            strcpy((u8 *)str,qq_lib$getopcname((i64)(*p).tag));
            qq_lib$additem((u8 *)str);
            qq_lib$additem((byte*)"(");
            qq_lib$jevallist((*p).a);
            qq_lib$additem((byte*)")");
        }
        else {
            msysc$m_print_startcon();
            msysc$m_print_str(qq_tables$jtagnames[((i64)(*p).tag)],NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            qq_lib$loaderror((byte*)"CAN'T DO JEVAL:",qq_tables$jtagnames[((i64)(*p).tag)]);
        }
;
    }
    };
}

static void qq_lib$jevallist(struct qq_decls$unitrec *p) {
        struct qq_decls$unitrec *  q;
    if (!(!!(p))) {
        return;
    }
;
    if (!!((*p).nextunit)) {
        qq_lib$additem((byte*)"(");
        q = p;
        L290 :;
        while (!!(q)) {
            qq_lib$jeval(q);
            q = (*q).nextunit;
            if (!!(q)) {
                qq_lib$additem((byte*)",");
            }
;
L291 :;
        }
L292 :;
        ;
        qq_lib$additem((byte*)")");
        return;
    }
    else {
        qq_lib$jeval(p);
    }
;
}

void qq_lib$additem(u8 *s) {
        u8 *  d;
        i64 lastchar;
        i64 nextchar;
    d = (*qq_lib$exprstr).strptr;
    if (!!((i64)(*qq_lib$exprstr).length)) {
        lastchar = (i64)(u64)(*((d + (i64)(*qq_lib$exprstr).length) - (i64)1));
        nextchar = (i64)(u64)(*s);
        if ((!!(qq_lib$isalphanum(lastchar)) && !!(qq_lib$isalphanum(nextchar)))) {
            mlib$strbuffer_add((struct mlib$strbuffer *)qq_lib$exprstr,(byte*)" ",(i64)-1);
        }
;
    }
;
    mlib$strbuffer_add((struct mlib$strbuffer *)qq_lib$exprstr,s,(i64)-1);
}

static i64 qq_lib$isalphanum(i64 c) {
    if (((((c >= (i64)65) && (c <= (i64)90)) || ((c >= (i64)97) && (c <= (i64)122))) || ((c >= (i64)48) && (c <= (i64)57)))) {
        return (i64)1;
    }
;
    return (i64)0;
}

u8 *qq_lib$getopcname(i64 opc) {
    return qq_tables$jtagnames[(opc)];
}

void qq_lib$convertstring(u8 *s,u8 *t) {
        i64 c;
    L293 :;
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
L294 :;
    }
L295 :;
    ;
    (*t) = (u64)0u;
}

struct qq_decls$unitrec *qq_lib$createavnamex(struct qq_decls$strec *owner) {
        struct qq_decls$unitrec *  p;
        struct qq_decls$strec *  d;
    p = qq_lib$createavname();
    qq_resolve$resolvename(owner,p,(i64)0);
    d = (*p).def;
    if (((i64)(*d).nameid == (i64)14)) {
        ++(qq_decls$nproclocals);
        (*d).index = qq_decls$nproclocals;
        (*qq_pclgen$pprocentry).n = qq_decls$nproclocals;
    }
;
    return p;
}

void qq_lib$storemode(struct qq_decls$strec *owner,i64 m,i16 *p) {
        struct qq_decls$userxrec *  q;
    (*p) = m;
    if ((m >= (i64)0)) {
        return;
    }
;
    q = (struct qq_decls$userxrec *)mlib$pcm_alloc((i64)24);
    (*q).owner = owner;
    if ((owner == 0)) {
        qq_lib$serror((byte*)"STOREMODE/OWNER=0");
    }
;
    (*q).pmode = p;
    (*q).nextmode = (struct qq_decls$userxrec *)qq_tables$userxmodelist;
    qq_tables$userxmodelist = (struct qq_decls$userxrec *)q;
}

i64 qq_lib$nextpoweroftwo(i64 x) {
        i64 a;
    if ((x == (i64)0)) {
        return (i64)0;
    }
;
    a = (i64)1;
    L296 :;
    while ((a < x)) {
        a <<= (i64)1;
L297 :;
    }
L298 :;
    ;
    return a;
}

i64 qq_lib$testelem(byte (*p)[],i64 n) {
    if (!!(((i64)(*p)[((n >> (i64)3))] & (i64)qq_lib$bytemasks[((n & (i64)7))]))) {
        return (i64)1;
    }
    else {
        return (i64)0;
    }
;
}

void qq_lib$setelem(byte (*p)[],i64 n) {
    (*p)[((n >> (i64)3))] |= (byte)(i64)qq_lib$bytemasks[((n & (i64)7))];
}

void qq_lib$setelemblock(byte (*p)[],i64 a,i64 b) {
        i64 ax;
        i64 bx;
        i64 nwords;
        i64 nx;
        i64 alast;
        i64 bfirst;
        u64 *  q;
        i64 $av_1;
        i64 i;
    if ((a > b)) {
        return;
    }
;
    ax = (a & (i64)-64);
    bx = ((b & (i64)-64) + (i64)64);
    nx = ax;
    alast = (bfirst = (i64)-1);
    nwords = ((bx - ax) / (i64)64);
    if ((nwords == (i64)1)) {
        if (((ax != a) || (b != (bx - (i64)1)))) {
            for (i=a;i<=b;++i) {
L299 :;
                qq_lib$setelem(p,i);
L300 :;
            }
L301 :;
            ;
            return;
        }
;
    }
    else {
        if ((ax != a)) {
            --(nwords);
            nx = (ax + (i64)64);
            alast = (nx - (i64)1);
        }
;
        if ((b != (bx - (i64)1))) {
            --(nwords);
            bfirst = (b & (i64)-64);
        }
;
    }
;
    if ((alast >= (i64)0)) {
        for (i=a;i<=alast;++i) {
L302 :;
            qq_lib$setelem(p,i);
L303 :;
        }
L304 :;
        ;
    }
;
    q = (u64 *)&(*p)[((nx >> (i64)3))];
    $av_1 = nwords;
    while ($av_1-- > 0) {
L305 :;
        (*q) = (u64)18446744073709551615u;
        ++(q);
L306 :;
    }
L307 :;
    ;
    if ((bfirst >= (i64)0)) {
        for (i=bfirst;i<=b;++i) {
L308 :;
            qq_lib$setelem(p,i);
L309 :;
        }
L310 :;
        ;
    }
;
}

i64 qq_lib$ispoweroftwo(i64 x) {
        i64 a;
        i64 n;
        i64 $av_1;
    a = (i64)1;
    n = (i64)0;
    $av_1 = (i64)60;
    while ($av_1-- > 0) {
L311 :;
        ++(n);
        a = (a << (i64)1);
        if ((a == x)) {
            return n;
        }
;
L312 :;
    }
L313 :;
    ;
    return (i64)0;
}

void qq_lib$deleteunit(struct qq_decls$unitrec *p,struct qq_decls$unitrec *q) {
        struct qq_decls$unitrec *  r;
    r = (*p).nextunit;
    (*p) = (*q);
    (*p).nextunit = r;
}

void qq_lib$skipsemi(void) {
    L314 :;
    while (((i64)qq_decls$lx.symbol == (i64)4)) {
        qq_lex$lex();
L315 :;
    }
L316 :;
    ;
}

void qq_lib$checksymbol(i64 symbol) {
        u8 str[100];
    if (((i64)qq_decls$lx.symbol != symbol)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"# expected, not #");
        msysc$m_print_u64(qq_tables$symbolnames[(symbol)-1],(byte*)"m");
        msysc$m_print_u64(qq_tables$symbolnames[((i64)qq_decls$lx.symbol)-1],(byte*)"m");
        msysc$m_print_end();
        ;
        qq_lib$serror((u8 *)str);
    }
;
}

void qq_lib$skipsymbol(i64 symbol) {
    qq_lib$checksymbol(symbol);
    qq_lex$lex();
}

void qq_lib$pcnotmut(void) {
    qq_runaux$pcerror((byte*)"Not mutable",(byte*)"");
}

// START
void qq_lib$start(void) {

}

// START
void qq_lists$start(void) {

    qq_lists$emptylist = qq_vars$obj_new();
    (*qq_lists$emptylist).lower16 = (i64)1;
    (*qq_lists$emptylist).objtype = (i64)0;
}

void qq_lists$var_empty_list(i64 lower,struct qq_decls$varrec *dest) {
    (*dest).objptr = qq_lists$obj_newlist((i64)0,lower,0);
    (*dest).tagx = (i64)266;
}

void qq_lists$var_make_list(struct qq_decls$varrec *a,struct qq_decls$varrec *dest,i64 n,i64 lower) {
        struct qq_decls$objrec *  p;
        struct qq_decls$varrec *  b;
        i64 $av_1;
    p = qq_lists$obj_newlist(n,lower,0);
    b = (*p).varptr;
    if ((!!(n) && !!(a))) {
        $av_1 = n;
        while ($av_1-- > 0) {
L317 :;
            (*b) = (*a);
            ++(a);
            ++(b);
L318 :;
        }
L319 :;
        ;
    }
;
    (*dest).tagx = (i64)266;
    (*dest).objptr = p;
}

struct qq_decls$objrec *qq_lists$obj_newlist(i64 n,i64 lower,struct qq_decls$varrec *defval) {
        struct qq_decls$objrec *  p;
        struct qq_decls$varrec *  a;
        i64 $av_1;
        i64 $av_2;
    p = qq_vars$obj_new();
    (*p).flags = msysc$m_setdotindex((*p).flags,(i64)1,(u64)1u);
    if (!((($rtemp=lower, $rtemp >= (i64)-32768 && $rtemp <= (i64)32767)))) {
        qq_runaux$pcerror((byte*)"List LWB not 16-bit",(byte*)"");
    }
;
    (*p).lower16 = lower;
    (*p).length = n;
    (*p).objtype = (i64)0;
    if (!!(n)) {
        (*p).varptr = (a = (struct qq_decls$varrec *)mlib$pcm_alloc((n * (i64)16)));
        (*p).alloc64 = (mlib$allocbytes / (i64)16);
        if ((!!(defval) && ((i64)(*defval).tag != (i64)0))) {
            $av_1 = n;
            while ($av_1-- > 0) {
L320 :;
                if (!!((i64)(*defval).hasref)) {
                    ++((*(*defval).objptr).refcount);
                }
;
                (*a) = (*defval);
                ++(a);
L321 :;
            }
L322 :;
            ;
        }
        else {
            $av_2 = n;
            while ($av_2-- > 0) {
L323 :;
                (*a).tagx = (i64)0;
                ++(a);
L324 :;
            }
L325 :;
            ;
        }
;
    }
;
    return p;
}

void qq_lists$obj_free_list(struct qq_decls$objrec *p) {
        struct qq_decls$varrec *  q;
        i64 $av_1;
    q = (*p).varptr;
    $av_1 = (*p).length;
    while ($av_1-- > 0) {
L326 :;
        if (!!((i64)(*q).hasref)) {
            qq_vars$var_unshareu(q);
        }
;
        ++(q);
L327 :;
    }
L328 :;
    ;
    if (!!((*p).length)) {
        mlib$pcm_free((*p).varptr,((*p).alloc64 * (i64)16));
    }
;
    mlib$pcm_free32(p);
}

void qq_lists$var_getix_list(struct qq_decls$varrec *a,i64 index) {
        struct qq_decls$objrec *  q;
        u64 offset;
        i64 lower;
    q = (*a).objptr;
    lower = (i64)(*q).lower16;
    offset = (u64)(index - lower);
    if ((offset >= (u64)(*q).length)) {
        qq_runaux$pcerror((byte*)"getlist[int] bounds",(byte*)"");
    }
;
    (*a) = (*((*q).varptr + (i64)offset));
    if (!!((i64)(*a).hasref)) {
        ++((*(*a).objptr).refcount);
    }
;
}

void qq_lists$var_getslice_list(struct qq_decls$varrec *a,i64 i,i64 j) {
        struct qq_decls$varrec v;
        i64 alower;
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
    p = (*a).objptr;
    alower = (i64)(*p).lower16;
    if ((((i < alower) || (j > (((*p).length + alower) - (i64)1))) || (i > j))) {
        qq_runaux$pcerror((byte*)"list/slice bounds",(byte*)"");
    }
;
    q = qq_vars$obj_new();
    v.objptr = q;
    (*q).objtype = (i64)1;
    (*q).flags = msysc$m_setdotindex((*q).flags,(i64)1,msysc$m_getdotindex((i64)(*p).flags,(i64)1));
    (*q).lower16 = (i64)1;
    (*q).varptr = (((*p).varptr + i) - alower);
        {i64 $temp = (i64)(*p).objtype;
if (($temp==(i64)1)) {
        (*q).objptr2 = (*p).objptr2;
        qq_vars$obj_shareu((*q).objptr2);
    }
    else if (($temp==(i64)2)) {
        (*q).objptr2 = 0;
        (*q).objtype = (i64)2;
    }
    else {
        (*q).objptr2 = p;
        ++((*p).refcount);
    }
    };
    (*q).length = ((j - i) + (i64)1);
    (*a).objptr = q;
}

void qq_lists$var_getixref_list(struct qq_decls$varrec *a,i64 index) {
        struct qq_decls$varrec *  p;
        struct qq_decls$objrec *  q;
        u64 offset;
        struct qq_decls$varrec v;
    q = (*a).objptr;
    offset = (u64)(index - (i64)(*q).lower16);
    if ((offset >= (u64)(*q).length)) {
        if (((i64)offset < (i64)0)) {
            qq_runaux$pcerror((byte*)"&list[int] lwb",(byte*)"");
        }
        else if (((i64)offset == (*q).length)) {
            if (((i64)(*q).objtype != (i64)0)) {
                qq_runaux$pcerror((byte*)"Can't extend slice/ext",(byte*)"");
            }
;
            v.tagx = (i64)0;
            qq_lists$obj_append_list(q,&v);
        }
        else {
            qq_runaux$pcerror((byte*)"putlist[int] bounds",(byte*)"");
        }
;
    }
;
    p = ((*q).varptr + (i64)offset);
    (*a).tagx = (i64)14;
    (*a).varptr = p;
}

void qq_lists$var_putix_list(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *x) {
        struct qq_decls$varrec *  dest;
        struct qq_decls$objrec *  q;
        u64 offset;
    q = (*a).objptr;
    if (!(!!(msysc$m_getdotindex((i64)(*q).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    offset = (u64)(index - (i64)(*q).lower16);
    if ((offset >= (u64)(*q).length)) {
        if (((i64)offset < (i64)0)) {
            qq_runaux$pcerror((byte*)"putlist[int] lwb",(byte*)"");
        }
        else if (((i64)offset == (*q).length)) {
            if (((i64)(*q).objtype != (i64)0)) {
                qq_runaux$pcerror((byte*)"Can't extend slice/ext",(byte*)"");
            }
;
            qq_lists$obj_append_list(q,x);
            return;
        }
        else {
            qq_runaux$pcerror((byte*)"putlist[int] bounds",(byte*)"");
        }
;
    }
;
    dest = ((*q).varptr + (i64)offset);
    if (!!((i64)(*dest).hasref)) {
        qq_vars$var_unshareu(dest);
    }
;
    (*dest) = (*x);
}

void qq_lists$var_putslice_list(struct qq_decls$varrec *a,i64 i,i64 j,struct qq_decls$varrec *x) {
        struct qq_decls$varrec *  r;
        struct qq_decls$varrec *  s;
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
        i64 length;
        i64 sublength;
        i64 $av_1;
    p = (*a).objptr;
    if (!(!!(msysc$m_getdotindex((i64)(*p).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    length = (*p).length;
    if ((((i < (i64)1) || (j > (*p).length)) || (i > j))) {
        qq_runaux$pcerror((byte*)"list/slice bounds",(byte*)"");
    }
;
    sublength = ((j - i) + (i64)1);
    q = (*x).objptr;
    if (((*q).length < sublength)) {
        qq_runaux$pcerror((byte*)"substr too short",(byte*)"");
    }
;
    r = (((*p).varptr + i) - (i64)1);
    s = (*q).varptr;
    $av_1 = sublength;
    while ($av_1-- > 0) {
L329 :;
        (*r) = (*s);
        if (!!((i64)(*r).hasref)) {
            ++((*(*r).objptr).refcount);
        }
;
        ++(r);
        ++(s);
L330 :;
    }
L331 :;
    ;
}

static void qq_lists$obj_append_list(struct qq_decls$objrec *a,struct qq_decls$varrec *x) {
        i64 n;
    if (((i64)(*a).objtype != (i64)0)) {
        qq_runaux$pcerror((byte*)"Can't extend slice",(byte*)"");
    }
;
    if (!(!!(msysc$m_getdotindex((i64)(*a).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    n = ((*a).length + (i64)1);
    if ((n > (*a).alloc64)) {
        qq_lists$obj_resize_list(a,n);
    }
    else {
        (*a).length = n;
    }
;
    if (!!(x)) {
        (*(((*a).varptr + n) - (i64)1)) = (*x);
    }
;
}

void qq_lists$obj_resize_list(struct qq_decls$objrec *p,i64 n) {
        struct qq_decls$varrec *  q;
        u32 allocated;
    if ((n <= (*p).alloc64)) {
        (*p).length = n;
    }
    else {
        q = (struct qq_decls$varrec *)mlib$pcm_alloc((n * (i64)16));
        allocated = (mlib$allocbytes / (i64)16);
        if (!!((*p).length)) {
            memcpy(q,(*p).varptr,(u64)((*p).length * (i64)16));
            mlib$pcm_free((*p).varptr,((*p).alloc64 * (i64)16));
        }
;
        (*p).varptr = q;
        (*p).length = n;
        (*p).alloc64 = (i64)allocated;
    }
;
}

void qq_lists$var_appendto_list(struct qq_decls$varrec *a,struct qq_decls$varrec *x) {
    qq_lists$obj_append_list((*a).objptr,x);
}

void qq_lists$var_dupl_list(struct qq_decls$varrec *a) {
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
        struct qq_decls$varrec *  plist;
        struct qq_decls$varrec *  qlist;
        i64 $av_1;
    p = (*a).objptr;
    q = qq_vars$obj_new();
    (*q) = (*p);
    (*q).refcount = (i64)1;
    (*q).flags = msysc$m_setdotindex((*q).flags,(i64)1,(u64)1u);
    (*q).objtype = (i64)0;
    (*a).objptr = q;
    if (((*q).length == (i64)0)) {
        return;
    }
;
    qlist = ((*q).varptr = (struct qq_decls$varrec *)mlib$pcm_alloc(((*p).length * (i64)16)));
    (*q).alloc64 = (mlib$allocbytes / (i64)16);
    plist = (*p).varptr;
    $av_1 = (*q).length;
    while ($av_1-- > 0) {
L332 :;
        (*qlist) = (*plist);
        if (((i64)(*qlist).tag == (i64)12)) {
            if (!!((i64)(*qlist).hasref)) {
                ++((*(*qlist).objptr).refcount);
            }
;
        }
        else {
            if (!!((i64)(*qlist).hasref)) {
                qq_vars$var_duplu(qlist);
            }
;
        }
;
        ++(qlist);
        ++(plist);
L333 :;
    }
L334 :;
    ;
}

void qq_lists$var_mul_list(struct qq_decls$varrec *p,i64 m) {
        i64 oldlength;
        i64 newlength;
        i64 n;
        struct qq_decls$objrec *  q;
        struct qq_decls$objrec *  r;
        struct qq_decls$varrec *  a;
        struct qq_decls$varrec *  b;
        i64 $av_1;
    q = (*p).objptr;
    oldlength = (*q).length;
    newlength = (oldlength * m);
    if ((oldlength == (i64)0)) {
        return;
    }
;
    if ((newlength < (i64)0)) {
        qq_runaux$pcerror((byte*)"list*int <0",(byte*)"");
    }
    else if ((newlength == (i64)0)) {
        (*p).objptr = qq_lists$obj_newlist((i64)0,(i64)(*q).lower16,0);
        return;
    }
;
    r = qq_lists$obj_newlist(newlength,(i64)(*q).lower16,0);
    a = (*r).varptr;
    b = (*q).varptr;
    n = (i64)0;
    $av_1 = newlength;
    while ($av_1-- > 0) {
L335 :;
        (*a) = (*b);
        if (!!((i64)(*a).hasref)) {
            ++((*(*a).objptr).refcount);
        }
;
        ++(a);
        if ((oldlength > (i64)1)) {
            ++(b);
            if ((++(n) == oldlength)) {
                b = (*q).varptr;
                n = (i64)0;
            }
;
        }
;
L336 :;
    }
L337 :;
    ;
    (*p).objptr = r;
}

i64 qq_lists$var_equal_list(struct qq_decls$varrec *x,struct qq_decls$varrec *y) {
        i64 xlen;
        i64 ylen;
        struct qq_decls$objrec *  px;
        struct qq_decls$objrec *  py;
        struct qq_decls$varrec *  a;
        struct qq_decls$varrec *  b;
        i64 $av_1;
    px = (*x).objptr;
    py = (*y).objptr;
    if ((px == py)) {
        return (i64)1;
    }
;
    xlen = (*px).length;
    ylen = (*py).length;
    if ((xlen != ylen)) {
        return (i64)0;
    }
;
    if ((xlen == (i64)0)) {
        return (i64)1;
    }
;
    a = (*px).varptr;
    b = (*py).varptr;
    $av_1 = xlen;
    while ($av_1-- > 0) {
L338 :;
        if ((qq_vars$var_equal(a,b) == (i64)0)) {
            return (i64)0;
        }
;
        ++(a);
        ++(b);
L339 :;
    }
L340 :;
    ;
    return (i64)1;
}

void qq_lists$var_concatto_list(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        struct qq_decls$varrec *  d;
        i64 alen;
        i64 blen;
        i64 newlen;
        struct qq_decls$objrec *  pa;
        struct qq_decls$objrec *  pb;
        i64 $av_1;
        i64 $av_2;
    pa = (*a).objptr;
    if (!(!!(msysc$m_getdotindex((i64)(*pa).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    pb = (*b).objptr;
    alen = (*pa).length;
    blen = (*pb).length;
    if ((alen == (i64)0)) {
        if (!!(blen)) {
            qq_lists$obj_resize_list(pa,blen);
            d = (*pa).varptr;
            memcpy(d,(*pb).varptr,(u64)(blen * (i64)16));
            $av_1 = blen;
            while ($av_1-- > 0) {
L341 :;
                if (!!((i64)(*d).hasref)) {
                    ++((*(*d).objptr).refcount);
                }
;
                ++(d);
L342 :;
            }
L343 :;
            ;
        }
;
    }
    else if (!!(blen)) {
        newlen = (alen + blen);
        qq_lists$obj_resize_list(pa,newlen);
        d = ((*pa).varptr + alen);
        memcpy(d,(*pb).varptr,(u64)(blen * (i64)16));
        $av_2 = blen;
        while ($av_2-- > 0) {
L344 :;
            if (!!((i64)(*d).hasref)) {
                ++((*(*d).objptr).refcount);
            }
;
            ++(d);
L345 :;
        }
L346 :;
        ;
    }
;
}

i64 qq_lists$var_inx_list(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        i64 n;
        i64 lowerm1;
        struct qq_decls$varrec *  x;
        i64 i;
    n = (*(*b).objptr).length;
    lowerm1 = ((i64)(*(*b).objptr).lower16 - (i64)1);
    x = (*(*b).objptr).varptr;
    for (i=(i64)1;i<=n;++i) {
L347 :;
        if ((qq_vars$var_equal(a,x) == (i64)1)) {
            return (i + lowerm1);
        }
;
        ++(x);
L348 :;
    }
L349 :;
    ;
    return (i64)(-9223372036854775807-1);
}

struct qq_decls$subprogrec *qq_modules$loadsp(u8 *filename,u8 *source) {
        struct qq_decls$subprogrec *  sp;
        u8 *  modnames[100];
        u8 *  subnames[100];
        i64 nmods;
        i64 nsubs;
        i64 firstmod;
        i64 lastmod;
        i64 issyslib;
        struct qq_decls$filerec *  pm;
        struct qq_decls$strec *  d;
        u8 path[300];
        i64 i;
    nmods = (i64)0;
    nsubs = (i64)0;
    issyslib = (i64)0;
    if (!!(source)) {
        pm = qq_modules$loadstring(filename,source);
        path[((i64)1)-1] = (u64)0u;
    }
    else {
        if (!!(mlib$eqstring(mlib$extractbasefile(filename),(byte*)"sysp"))) {
            issyslib = (i64)1;
        }
;
        pm = qq_modules$loadsourcefile(filename,issyslib);
        if ((pm == 0)) {
            qq_lib$loaderror((byte*)"Can't load lead module: #",filename);
        }
;
        strcpy(path,(*pm).path);
    }
;
    for (i=(i64)1;i<=qq_decls$nsubprogs;++i) {
L350 :;
        if (!!(mlib$eqstring((*pm).name,(*qq_decls$subprogs[(i)-1]).name))) {
            qq_lib$loaderror((byte*)"Subprog already loaded: #",(*sp).name);
        }
;
L351 :;
    }
L352 :;
    ;
    qq_lex$startlex(pm);
    L353 :;
    while (1) {
        qq_lex$lex();
        qq_lib$skipsemi();
                {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)105)) {
            qq_lex$lex();
            qq_lib$checksymbol((i64)69);
            if (!(!!(mlib$eqstring((*qq_decls$lx.symptr).name,(*pm).name)))) {
                if ((nmods >= (i64)100)) {
                    qq_lib$loaderror((byte*)"Too many modules in header",(byte*)"");
                }
;
                modnames[(++(nmods))-1] = (*qq_decls$lx.symptr).name;
            }
;
        }
        else if (($temp==(i64)106)) {
            qq_lex$lex();
            qq_lib$checksymbol((i64)69);
            if ((nsubs >= (i64)100)) {
                qq_lib$loaderror((byte*)"Too many imports in header",(byte*)"");
            }
;
            subnames[(++(nsubs))-1] = (*qq_decls$lx.symptr).name;
        }
        else if (($temp==(i64)4)) {
        }
        else {
            goto L354 ;
        }
        };
    }
L354 :;
    ;
    for (i=(i64)1;i<=nsubs;++i) {
L355 :;
        if (!!(mlib$eqstring(subnames[(i)-1],(*pm).name))) {
            qq_lib$loaderror((byte*)"Importing self",(byte*)"");
        }
;
        qq_cli$compile_sp(qq_modules$getmodulefilename((byte*)"",subnames[(i)-1]),0);
L356 :;
    }
L357 :;
    ;
    if ((qq_decls$nsubprogs >= (i64)30)) {
        qq_lib$loaderror((byte*)"Too many subprogs",(byte*)"");
    }
;
    sp = (struct qq_decls$subprogrec *)mlib$pcm_allocz((i64)32);
    qq_decls$subprogs[(++(qq_decls$nsubprogs))-1] = sp;
    (*sp).subprogno = qq_decls$nsubprogs;
    firstmod = (qq_decls$nmodules + (i64)1);
    lastmod = (firstmod + nmods);
    if ((lastmod > (i64)200)) {
        qq_lib$loaderror((byte*)"Too many modules",(byte*)"");
    }
;
    qq_decls$nmodules = lastmod;
    (*pm).subprogno = qq_decls$nsubprogs;
    (*pm).islead = (i64)1;
    (*pm).moduleno = firstmod;
    (*pm).stmodule = (d = qq_names$createdupldef(qq_decls$stprogram,qq_lex$addnamestr((*pm).name),(i64)3));
    (*d).moduleno = firstmod;
    (*sp).name = (*pm).name;
    (*sp).path = (*pm).path;
    (*sp).filespec = (*pm).filespec;
    (*sp).firstmodule = firstmod;
    (*sp).lastmodule = lastmod;
    (*sp).issyslib = issyslib;
    qq_decls$modules[(firstmod)] = pm;
    for (i=(i64)1;i<=nmods;++i) {
L358 :;
        pm = qq_modules$loadsourcefile(qq_modules$getmodulefilename(path,modnames[(i)-1]),issyslib);
        if (!(!!(pm))) {
            qq_lib$loaderror((byte*)"Can't load: ##",modnames[(i)-1]);
        }
;
        qq_decls$modules[((firstmod + i))] = pm;
        (*pm).stmodule = (d = qq_names$createdupldef(qq_decls$stprogram,qq_lex$addnamestr((*pm).name),(i64)3));
        (*pm).subprogno = qq_decls$nsubprogs;
        (*d).moduleno = ((*pm).moduleno = (firstmod + i));
L359 :;
    }
L360 :;
    ;
    return sp;
}

static u8 *qq_modules$getmodulefilename(u8 *path,u8 *name) {
        static u8 str[300];
    strcpy(str,path);
    strcat(str,name);
    strcat(str,(byte*)".q");
    return str;
}

struct qq_decls$filerec *qq_modules$loadsourcefile(u8 *filespec,i64 issyslib) {
        struct qq_decls$filerec *  pm;
        u8 *  s;
        u8 *  basefilename;
        u8 str[300];
    pm = (struct qq_decls$filerec *)mlib$pcm_allocz((i64)120);
    basefilename = mlib$extractbasefile(filespec);
    (*pm).filespec = mlib$pcm_copyheapstring(filespec);
    (*pm).path = mlib$pcm_copyheapstring(mlib$extractpath(filespec));
    (*pm).name = mlib$pcm_copyheapstring(basefilename);
    (*pm).issyslib = issyslib;
    if ((!!(qq_decls$nqafiles) && !!(qq_modules$loadqafile(pm)))) {
        return pm;
    }
;
    if ((!!(issyslib) && !!((i64)qq_decls$usebundled))) {
        (*pm).issyslib = issyslib;
        if (!(!!(qq_syslibsdummy$loadsysmodule(pm)))) {
            qq_lib$loaderror((byte*)"LS:Can't load syslib",filespec);
        }
;
        return pm;
    }
;
    s = (u8 *)mlib$readfile(filespec);
    if (!(!!(s))) {
        strcpy(str,(byte*)"c:/m/libs/");
        strcat(str,basefilename);
        strcat(str,(byte*)".q");
        s = (u8 *)mlib$readfile(str);
        if (!(!!(s))) {
            return 0;
        }
;
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"LOADED FROM MLIBS",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    (*pm).text = s;
    (*pm).size = mlib$rfsize;
    (*(s + mlib$rfsize)) = (u64)0u;
    return pm;
}

struct qq_decls$filerec *qq_modules$loadstring(u8 *name,u8 *source) {
        struct qq_decls$filerec *  pm;
        u8 str[16];
        static i64 nextstrname = (i64)0;
    if ((name == 0)) {
        msysc$m_print_startstr(str);
        msysc$m_print_str((byte*)"S$",NULL);
        msysc$m_print_nogap();
        msysc$m_print_i64(++(nextstrname),NULL);
        msysc$m_print_end();
        ;
        name = mlib$pcm_copyheapstring(str);
    }
;
    pm = (struct qq_decls$filerec *)mlib$pcm_allocz((i64)120);
    (*pm).filespec = (byte*)"<string>";
    (*pm).path = (byte*)"";
    (*pm).name = name;
    (*pm).text = source;
    (*pm).size = strlen(source);
    return pm;
}

static u8 *qq_modules$readfileline(u8 *s) {
        u8 str[2048];
        u8 *  t;
        i64 n;
        i64 c;
    t = str;
    n = (i64)0;
    L361 :;
        {i64 $temp = (c = (i64)(u64)(*(s)++));
if (($temp==(i64)0)) {
        --(s);
        goto L362 ;
    }
    else if (($temp==(i64)10)) {
        goto L362 ;
    }
    else {
        if ((n < (i64)2048)) {
            (*(t)++) = (u64)c;
        }
;
    }
    }goto L361 ;
L362 :;
    ;
    (*t) = (u64)0u;
    msysc$m_read_strline(str);
    ;
    return s;
}

static u8 *qq_modules$findnextlineheader(u8 *s) {
        i64 c;
    L363 :;
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
    }goto L363 ;
L364 :;
    ;
    return (u8 *)0;
}

static i64 qq_modules$loadqafile(struct qq_decls$filerec *pm) {
        u8 filename[300];
        i64 i;
    strcpy(filename,mlib$extractfile((*pm).filespec));
    for (i=(i64)1;i<=qq_decls$nqafiles;++i) {
L365 :;
        if (!!(mlib$eqstring(filename,qq_decls$qafilenames[(i)-1]))) {
            (*pm).text = qq_decls$qatext[(i)-1];
            (*pm).size = qq_decls$qasize[(i)-1];
            return (i64)1;
        }
;
L366 :;
    }
L367 :;
    ;
    return (i64)0;
}

void qq_modules$readqabundle(void) {
        u8 name[100];
        u8 *  s;
        u8 *  t;
        i64 sys;
        i64 support;
        i64 i;
    s = mlib$extractext(qq_cli$inputfile,(i64)0);
    mlib$convlcstring(s);
    if (!(!!(mlib$eqstring(s,(byte*)"qa")))) {
        return;
    }
;
    s = (u8 *)mlib$readfile(qq_cli$inputfile);
    if ((s == 0)) {
        qq_lib$loaderror((byte*)"Can't find QA file ##",qq_cli$inputfile);
    }
;
    qq_cli$inputfile = mlib$pcm_copyheapstring(mlib$changeext(qq_cli$inputfile,(byte*)"q"));
    s = qq_modules$readfileline((s + (i64)3));
    msysc$readstr(name,(i64)110,(i64)0);
    if (!(!!(mlib$eqstring(name,(byte*)"qa")))) {
        qq_lib$loaderror((byte*)"QA: bad header",(byte*)"");
    }
;
    --(s);
    s = qq_modules$findnextlineheader(s);
    L368 :;
    while (1) {
        if ((s == 0)) {
            qq_lib$loaderror((byte*)"Unexpected EOF in QA file",(byte*)"");
            goto L369 ;
        }
;
        s = qq_modules$readfileline(s);
        msysc$readstr(name,(i64)110,(i64)0);
        {
            sys = msysc$m_read_i64((i64)0);
            ;
            support = msysc$m_read_i64((i64)0);
            ;
        }
        if (!!(mlib$eqstring(name,(byte*)"end"))) {
            goto L369 ;
        }
;
        if ((qq_decls$nqafiles >= (i64)100)) {
            qq_lib$loaderror((byte*)"Too many QA files",(byte*)"");
        }
;
        t = qq_modules$findnextlineheader(s);
        if ((t == 0)) {
            qq_lib$loaderror((byte*)"QA error",(byte*)"");
        }
;
        ++(qq_decls$nqafiles);
        qq_decls$qafilenames[(qq_decls$nqafiles)-1] = mlib$pcm_copyheapstring(name);
        qq_decls$qasize[(qq_decls$nqafiles)-1] = ((t - s) - (i64)3);
        qq_decls$qatext[(qq_decls$nqafiles)-1] = s;
        s = t;
    }
L369 :;
    ;
    for (i=(i64)1;i<=qq_decls$nqafiles;++i) {
L370 :;
        (*(qq_decls$qatext[(i)-1] + qq_decls$qasize[(i)-1])) = (u64)0u;
L371 :;
    }
L372 :;
    ;
}

// START
void qq_modules$start(void) {

}

struct qq_decls$strec *qq_names$addglobalname(u8 *name) {
        struct qq_decls$lexrec oldlx;
        struct qq_decls$strec *  d;
    oldlx = qq_decls$nextlx;
    qq_lex$lookup(name,strlen(name),qq_lex$gethashvaluez(name));
    d = (struct qq_decls$strec *)qq_decls$nextlx.symptr;
    qq_decls$nextlx = oldlx;
    return d;
}

static struct qq_decls$strec *qq_names$newstrec(void) {
        struct qq_decls$strec *  p;
    p = (struct qq_decls$strec *)mlib$pcm_alloc((i64)128);
    memset(p,(i32)(i64)0,(u64)128u);
    return p;
}

struct qq_decls$strec *qq_names$addsymbol(struct qq_decls$strec *owner,struct qq_decls$strec *d,i64 id,i64 isglobal) {
        struct qq_decls$strec *  e;
        struct qq_decls$strec *  f;
    e = qq_names$newstrec();
    (*e).name = (*d).name;
    (*e).namelen = (i64)(*d).namelen;
    (*e).owner = owner;
    (*e).nameid = id;
    (*e).flags = msysc$m_setdotindex((*e).flags,(i64)9,(u64)((id == (i64)14) || (id == (i64)15)));
    if (!!(qq_decls$currmodule)) {
        (*e).moduleno = (i64)(*qq_decls$currmodule).moduleno;
    }
;
    (*e).firstdupl = d;
    (*e).flags = msysc$m_setdotslice((*e).flags,(i64)0,(i64)1,(u64)isglobal);
    if (!(!!(owner))) {
        return e;
    }
;
    if (!(((i64)(*owner).nameid == (i64)5 || (i64)(*owner).nameid == (i64)6))) {
        (*e).nextdupl = (*d).nextdupl;
        (*d).nextdupl = e;
        if ((!!((*e).nextdupl) && ((*(*e).nextdupl).owner == owner))) {
            msysc$m_print_startcon();
            msysc$m_print_str((*e).name,NULL);
            msysc$m_print_str((byte*)"in",NULL);
            msysc$m_print_str((*owner).name,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            qq_lib$serror((byte*)"AS:Duplicate name");
        }
;
    }
    else {
        f = (*owner).deflist;
        L373 :;
        while (!!(f)) {
            if (((*f).firstdupl == (*e).firstdupl)) {
                msysc$m_print_startcon();
                msysc$m_print_str((*e).name,NULL);
                msysc$m_print_str((byte*)"in",NULL);
                msysc$m_print_str((*owner).name,NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                qq_lib$serror((byte*)"AS2:Duplicate name");
            }
;
            f = (*f).nextdef;
L374 :;
        }
L375 :;
        ;
    }
;
    if (((*owner).deflist == 0)) {
        (*owner).deflist = e;
    }
    else {
        (*(*owner).deflistx).nextdef = e;
    }
;
    (*owner).deflistx = e;
    return e;
}

void qq_names$addproc(struct qq_decls$strec *d) {
        struct qq_decls$procrec *  p;
    p = (struct qq_decls$procrec *)mlib$pcm_allocz((i64)16);
    (*p).def = d;
    if ((qq_decls$proclist == 0)) {
        qq_decls$proclist = (struct qq_decls$procrec *)p;
    }
    else {
        (*qq_decls$proclistx).nextproc = (struct qq_decls$procrec *)p;
    }
;
    qq_decls$proclistx = (struct qq_decls$procrec *)p;
    ++(qq_decls$nproclist);
}

i64 qq_names$newusertypex(struct qq_decls$strec *d,struct qq_decls$strec *e) {
    if ((qq_tables$nuserxtypes >= (i64)5000)) {
        qq_lib$serror((byte*)"Too many external user types");
    }
;
    ++(qq_tables$nuserxtypes);
    qq_tables$ttnamedefx[(qq_tables$nuserxtypes)] = (struct qq_decls$strec *)d;
    qq_tables$ttxmoduleno[(qq_tables$nuserxtypes)] = (i64)(*qq_decls$stcurrmodule).moduleno;
    return -(qq_tables$nuserxtypes);
}

struct qq_decls$strec *qq_names$resolvedottedname(struct qq_decls$strec *owner,struct qq_decls$strec *d) {
        struct qq_decls$strec *  e;
    e = (*d).nextdupl;
    L376 :;
    while ((!!(e) && ((*e).owner != owner))) {
        e = (*e).nextdupl;
L377 :;
    }
L378 :;
    ;
    return e;
}

void qq_names$addgenfield(struct qq_decls$strec *d) {
        i64 index;
        struct qq_decls$strec *  dgen;
        struct qq_decls$genfieldrec *  g;
    dgen = (*d).firstdupl;
    index = (*dgen).genfieldindex;
    if ((index == (i64)0)) {
        if ((qq_decls$ngenfields >= (i64)1000)) {
            qq_lib$gerror((byte*)"Too many genfields",0);
        }
;
        (*dgen).genfieldindex = (index = ++(qq_decls$ngenfields));
    }
;
    g = (struct qq_decls$genfieldrec *)mlib$pcm_alloc((i64)16);
    (*g).def = d;
    (*g).nextdef = (struct qq_decls$genfieldrec *)qq_decls$genfieldtable[(index)-1];
    qq_decls$genfieldtable[(index)-1] = (struct qq_decls$genfieldrec *)g;
}

i64 qq_names$makereftype(i64 target,struct qq_decls$strec *owner) {
        i64 newtype;
        i64 i;
    if ((owner == 0)) {
        for (i=(i64)41;i<=qq_tables$ntypes;++i) {
L379 :;
            if ((((i64)qq_tables$ttbasetype[(i)] == (i64)16) && ((i64)qq_tables$tttarget[(i)] == target))) {
                return i;
            }
;
L380 :;
        }
L381 :;
        ;
    }
;
    newtype = qq_names$addanontype();
    qq_tables$ttbasetype[(newtype)] = (i64)16;
    qq_lib$storemode(qq_decls$stcurrproc,target,&qq_tables$tttarget[(newtype)]);
    qq_tables$ttsize[(newtype)] = (i64)8;
    qq_tables$ttbitwidth[(newtype)] = (i64)64;
    return newtype;
}

i64 qq_names$makeaxtype(i64 target,struct qq_decls$unitrec *plower,struct qq_decls$unitrec *plength) {
        i64 newtype;
    newtype = qq_names$addanontype();
    qq_tables$ttbasetype[(newtype)] = (i64)7;
    qq_lib$storemode(qq_decls$stcurrproc,target,&qq_tables$tttarget[(newtype)]);
    qq_tables$ttlower[(newtype)] = (i64)1;
    qq_tables$ttlengthexpr[(newtype)] = plength;
    qq_tables$ttlowerexpr[(newtype)] = plower;
    return newtype;
}

i64 qq_names$makestrtype(i64 m,struct qq_decls$unitrec *pwidth) {
        i64 newtype;
    newtype = qq_names$addanontype();
    qq_tables$ttbasetype[(newtype)] = m;
    qq_tables$ttlengthexpr[(newtype)] = pwidth;
    qq_tables$ttlower[(newtype)] = (i64)1;
    qq_tables$ttowner[(newtype)] = qq_decls$stcurrproc;
    return newtype;
}

i64 qq_names$addanontype(void) {
        u8 str[32];
    if ((qq_tables$ntypes >= (i64)250)) {
        qq_lib$gerror((byte*)"Too many types",0);
    }
;
    ++(qq_tables$ntypes);
    msysc$m_print_startstr(str);
    msysc$m_print_str((byte*)"$T",NULL);
    msysc$m_print_nogap();
    msysc$m_print_i64(qq_tables$ntypes,NULL);
    msysc$m_print_end();
    ;
    qq_tables$ttname[(qq_tables$ntypes)] = mlib$pcm_copyheapstring(str);
    qq_tables$ttowner[(qq_tables$ntypes)] = qq_decls$stcurrproc;
    return qq_tables$ntypes;
}

void qq_names$createusertype(struct qq_decls$strec *d,i64 m) {
    qq_lib$storemode(qq_decls$stcurrproc,m,&(*d).mode);
    if (((m > (i64)40) && (qq_tables$ttnamedef[(m)] == 0))) {
        qq_tables$ttnamedef[(m)] = d;
        qq_tables$ttname[(m)] = (*d).name;
        qq_tables$ttowner[(m)] = (*d).owner;
    }
;
}

i64 qq_names$getalignment(i64 m) {
        i64 a;
        {i64 $temp = (i64)qq_tables$ttbasetype[(m)];
if (($temp==(i64)7)) {
        return qq_names$getalignment((i64)qq_tables$tttarget[(m)]);
    }
    else if (($temp==(i64)13)) {
    }
    };
    a = qq_tables$ttsize[(m)];
    if ((a==(i64)1) || (a==(i64)2) || (a==(i64)4) || (a==(i64)8)) {
        return a;
    }
;
    msysc$m_print_startcon();
    msysc$m_print_str(qq_tables$ttname[(m)],NULL);
    msysc$m_print_i64(a,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    qq_lib$gerror((byte*)"Getalign not 1248",0);
    return (i64)0;
}

void qq_names$duplfield(struct qq_decls$strec *p,struct qq_decls$strec *q) {
    if (!!((*p).code)) {
        qq_lib$serror((byte*)"DUPLFIELD");
    }
;
    (*q).atfield = (*p).atfield;
    (*q).index = (i64)(*p).index;
    (*q).fieldoffset = (i64)(*p).fieldoffset;
}

static void qq_names$writesig(struct qq_decls$strec *d,void *dev) {
        struct qq_decls$strec *  e;
        i64 n;
    msysc$m_print_startfile(dev);
    msysc$m_print_setfmt((byte*)"# #(");
    msysc$m_print_str((!!(msysc$m_getdotindex((i64)(*d).flags,(i64)4)) ? (byte*)"function" : (byte*)"proc"),NULL);
    msysc$m_print_str((*d).name,NULL);
    msysc$m_print_end();
    ;
    e = (*d).deflist;
    n = (i64)0;
    L382 :;
    while (!!(e)) {
        if (((i64)(*e).nameid == (i64)15)) {
            ++(n);
            if ((!!(msysc$m_getdotindex((i64)(*e).flags,(i64)7)) && !!((*e).code))) {
                msysc$m_print_startfile(dev);
                msysc$m_print_setfmt((byte*)"#=#");
                msysc$m_print_str((*e).name,NULL);
                msysc$m_print_str((*qq_lib$strexpr((*e).code)).strptr,NULL);
                msysc$m_print_end();
                ;
            }
            else if (!!(msysc$m_getdotindex((i64)(*e).flags,(i64)7))) {
                msysc$m_print_startfile(dev);
                msysc$m_print_str((byte*)"?",NULL);
                msysc$m_print_nogap();
                msysc$m_print_str((*e).name,NULL);
                msysc$m_print_end();
                ;
            }
            else {
                msysc$m_print_startfile(dev);
                msysc$m_print_str((*e).name,NULL);
                msysc$m_print_end();
                ;
            }
;
            if ((n < (i64)(*d).nparams)) {
                msysc$m_print_startfile(dev);
                msysc$m_print_str((byte*)", ",NULL);
                msysc$m_print_end();
                ;
            }
;
        }
;
L383 :;
        e = (*e).nextdef;
L385 :;
            }
L384 :;
    ;
    msysc$m_print_startfile(dev);
    msysc$m_print_setfmt((byte*)")\t[#]");
    msysc$m_print_str((*(*d).owner).name,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

struct qq_decls$strec *qq_names$createdupldef(struct qq_decls$strec *owner,struct qq_decls$strec *symptr,i64 id) {
        struct qq_decls$strec *  p;
    p = qq_names$newstrec();
    (*p).name = (*symptr).name;
    (*p).namelen = (i64)(*symptr).namelen;
    (*p).symbolcode = (i64)69;
    (*p).owner = owner;
    (*p).nameid = id;
    (*p).nextdupl = (*symptr).nextdupl;
    (*symptr).nextdupl = p;
    if (!!(owner)) {
        if (((*owner).deflist == 0)) {
            (*owner).deflist = ((*owner).deflistx = p);
        }
        else {
            (*(*owner).deflistx).nextdef = p;
            (*owner).deflistx = p;
        }
;
    }
;
    return p;
}

// START
void qq_names$start(void) {

}

void qq_packed$var_loadpacked(void *p,i64 t,struct qq_decls$varrec *dest,struct qq_decls$objrec *ownerobj) {
        i64 length;
        struct qq_decls$objrec *  s;
        u8 *  ss;
        i64 $av_1;
    (*dest).tagx = (i64)1;
    switch ((i64)qq_tables$ttbasetype[(t)]) {
    case 23:;
        {
            (*dest).value = (i64)(*(i8 *)p);
        }
        break;
    case 24:;
        {
            (*dest).value = (i64)(*(i16 *)p);
        }
        break;
    case 25:;
        {
            (*dest).value = (i64)(*(i32 *)p);
        }
        break;
    case 26:;
        {
            (*dest).value = (*(i64 *)p);
        }
        break;
    case 27:;
        {
            (*dest).value = (i64)(*(byte *)p);
        }
        break;
    case 28:;
        {
            (*dest).value = (i64)(*(u16 *)p);
        }
        break;
    case 29:;
        {
            (*dest).value = (i64)(*(u32 *)p);
        }
        break;
    case 30:;
        {
            (*dest).tagx = (i64)1;
            (*dest).uvalue = (*(u64 *)p);
        }
        break;
    case 32:;
        {
            (*dest).tagx = (i64)2;
            (*dest).xvalue = (*(r64 *)p);
        }
        break;
    case 31:;
        {
            (*dest).tagx = (i64)2;
            (*dest).xvalue = (r64)(*(r32 *)p);
        }
        break;
    case 36:;
        {
            (*dest).tagx = (i64)265;
            length = qq_tables$ttlength[(t)];
            if ((length >= (i64)2)) {
                length = qq_packed$getfslength((u8 *)p,length);
            }
            else {
                length = (i64)1;
            }
;
            s = qq_strings$obj_make_strslicexobj((u8 *)p,length);
            (*dest).objptr = s;
        }
        break;
    case 37:;
        {
            (*dest).tagx = (i64)265;
            ss = (u8 *)p;
            $av_1 = qq_tables$ttlength[(t)];
            while ($av_1-- > 0) {
L386 :;
                if (((i64)(u64)(*ss) == (i64)0)) {
                    goto L388 ;
                }
;
                ++(ss);
L387 :;
            }
L388 :;
            ;
            s = qq_strings$obj_make_strslicexobj((u8 *)p,(ss - (u8 *)p));
            (*dest).objptr = s;
        }
        break;
    default: {
                {i64 $temp = (i64)qq_tables$ttbasetype[(t)];
if (($temp==(i64)16)) {
            (*dest).tagx = (i64)16;
            (*dest).ptr = (byte *)(*(i64 *)p);
            (*dest).elemtag = (i64)qq_tables$tttarget[(t)];
        }
        else if (($temp==(i64)13)) {
            s = qq_vars$obj_new();
            (*s).flags = msysc$m_setdotindex((*s).flags,(i64)1,(u64)1u);
            (*s).ptr = (byte *)p;
            (*dest).objptr = s;
            (*dest).tagx = (i64)269;
            (*s).usertag = t;
            if (!!(ownerobj)) {
                (*s).objtype = (i64)1;
                (*s).objptr2 = ownerobj;
                ++((*ownerobj).refcount);
            }
            else {
                (*s).objtype = (i64)2;
            }
;
        }
        else if (($temp==(i64)7)) {
            s = qq_arrays$obj_newarray((i64)qq_tables$tttarget[(t)],qq_tables$ttlower[(t)],qq_tables$ttlength[(t)]);
            (*s).flags = msysc$m_setdotindex((*s).flags,(i64)1,(u64)1u);
            (*s).ptr = (byte *)p;
            (*dest).objptr = s;
            (*dest).tagx = (i64)263;
            (*s).usertag = t;
            if (!!(ownerobj)) {
                (*s).objtype = (i64)1;
                (*s).objptr2 = ownerobj;
                ++((*ownerobj).refcount);
            }
            else {
                (*s).objtype = (i64)2;
            }
;
        }
        else {
            qq_runaux$pcmxtypestt((byte*)"loadpacked",(i64)qq_tables$ttbasetype[(t)],t);
        }
        };
    }
    } //SW
;
}

void qq_packed$var_storepacked(byte *p,struct qq_decls$varrec *q,i64 t) {
        i64 plength;
        i64 qlength;
        i64 s;
        i64 sbase;
        i64 tbase;
        struct qq_decls$objrec *  qa;
    s = (sbase = (i64)(*q).tag);
    tbase = (i64)qq_tables$ttbasetype[(t)];
    switch (sbase) {
    case 1:;
    case 16:;
        {
            switch (tbase) {
            case 23:;
            case 27:;
                {
                    (*p) = (*q).value;
                    return;
                }
                break;
            case 24:;
            case 28:;
                {
                    (*(u16 *)p) = (*q).value;
                    return;
                }
                break;
            case 25:;
            case 29:;
                {
                    (*(i32 *)p) = (*q).value;
                    return;
                }
                break;
            case 26:;
            case 30:;
            case 16:;
                {
                    (*(i64 *)p) = (*q).value;
                    return;
                }
                break;
            case 31:;
                {
                    (*(r32 *)p) = (r32)(*q).value;
                    return;
                }
                break;
            case 32:;
                {
                    (*(r64 *)p) = (r64)(*q).value;
                    return;
                }
                break;
            } //SW
;
        }
        break;
    case 2:;
        {
            switch (tbase) {
            case 25:;
            case 29:;
                {
                    (*(i32 *)p) = (i64)(*q).xvalue;
                    return;
                }
                break;
            case 26:;
            case 30:;
                {
                    (*(i64 *)p) = (i64)(*q).xvalue;
                    return;
                }
                break;
            case 31:;
                {
                    (*(r32 *)p) = (r32)(*q).xvalue;
                    return;
                }
                break;
            case 32:;
                {
                    (*(r64 *)p) = (*q).xvalue;
                    return;
                }
                break;
            case 24:;
            case 28:;
                {
                    (*(i16 *)p) = (i64)(*q).xvalue;
                    return;
                }
                break;
            } //SW
;
        }
        break;
    case 9:;
        {
            qa = (*q).objptr;
            plength = qq_tables$ttlength[(t)];
            qlength = (*qa).length;
            switch (tbase) {
            case 36:;
                {
                    if ((t == tbase)) {
                        if ((qlength != (i64)1)) {
                            qq_runaux$pcerror((byte*)"Str not len 1",(byte*)"");
                        }
;
                        (*(u8 *)p) = (u64)(*(*qa).strptr);
                        return;
                    }
;
                    if ((qlength > plength)) {
                        qlength = plength;
                    }
;
                    memcpy(p,(void *)(*qa).strptr,(u64)qlength);
                    qq_packed$setfslength((u8 *)p,plength,qlength);
                    return;
                }
                break;
            case 37:;
                {
                    if ((qlength >= plength)) {
                        memcpy(p,(void *)(*qa).strptr,(u64)plength);
                        (*((p + plength) - (i64)1)) = (i64)0;
                    }
                    else {
                        memcpy(p,(void *)(*qa).strptr,(u64)qlength);
                        (*(p + qlength)) = (i64)0;
                    }
;
                    return;
                }
                break;
            } //SW
;
        }
        break;
    case 13:;
        {
            s = (i64)(*(*q).objptr).usertag;
            if ((s != t)) {
                qq_runaux$pcmxtypestt((byte*)"spack struct",s,t);
            }
;
            memcpy(p,(*(*q).objptr).ptr,(u64)qq_tables$ttsize[(t)]);
            return;
        }
        break;
    case 7:;
        {
            s = (i64)(*(*q).objptr).usertag;
            if ((s != t)) {
                qq_runaux$pcmxtypestt((byte*)"spack array",s,t);
            }
;
            memcpy(p,(*(*q).objptr).ptr,(u64)qq_tables$ttsize[(t)]);
            return;
        }
        break;
    } //SW
;
    qq_runaux$pcmxtypestt((byte*)"storepacked (source->dest)",s,t);
}

static void qq_packed$setfslength(u8 *s,i64 m,i64 n) {
    if ((m == n)) {
    }
    else if ((n == (m - (i64)1))) {
        (*((s + m) - (i64)1)) = (u64)0u;
    }
    else {
        (*((s + m) - (i64)2)) = (u64)0u;
        (*((s + m) - (i64)1)) = (u64)n;
    }
;
}

i64 qq_packed$getfslength(u8 *s,i64 m) {
    s += (m - (i64)1);
    if (((i64)(u64)(*(s - (i64)1)) == (i64)0)) {
        return (i64)(u64)(*s);
    }
    else if (((i64)(u64)(*s) == (i64)0)) {
        return (m - (i64)1);
    }
    else {
        return m;
    }
;
}

void qq_packed$var_make_struct(struct qq_decls$varrec *a,struct qq_decls$varrec *dest,i64 n,i64 rectype) {
        struct qq_decls$strec *  d;
        struct qq_decls$strec **  r;
        struct qq_decls$objrec *  p;
        struct qq_decls$varrec *  b;
        i64 m;
        byte *  q;
        i64 $av_1;
    p = qq_packed$obj_new_struct(rectype);
    b = (*p).varptr;
    m = qq_tables$ttlength[(rectype)];
    d = qq_tables$ttnamedef[(rectype)];
    r = (struct qq_decls$strec **)(*d).topfieldlist;
    if ((n < m)) {
        qq_runaux$pcerror((byte*)"Too few elements",(byte*)"");
    }
    else if ((n > m)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"N=",NULL);
        msysc$m_print_i64(n,NULL);
        msysc$m_print_str((byte*)"M=",NULL);
        msysc$m_print_i64(m,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        qq_runaux$pcerror((byte*)"Too many elements",(byte*)"");
    }
;
    q = (*p).ptr;
    $av_1 = n;
    while ($av_1-- > 0) {
L389 :;
        qq_packed$var_storepacked(q,a,(i64)(*(*r)).mode);
        q += qq_tables$ttsize[((i64)(*(*r)).mode)];
        ++(r);
        ++(a);
L390 :;
    }
L391 :;
    ;
    (*dest).tagx = (i64)269;
    (*p).usertag = rectype;
    (*dest).objptr = p;
}

struct qq_decls$objrec *qq_packed$obj_new_struct(i64 m) {
        struct qq_decls$objrec *  p;
        i64 size;
    p = qq_vars$obj_new();
    (*p).flags = msysc$m_setdotindex((*p).flags,(i64)1,(u64)1u);
    (*p).usertag = m;
    size = qq_tables$ttsize[(m)];
    if (!!(size)) {
        (*p).ptr = (byte *)mlib$pcm_allocz(size);
    }
;
    return p;
}

void qq_packed$var_dupl_struct(struct qq_decls$varrec *a) {
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
        i64 size;
    p = (*a).objptr;
    size = qq_tables$ttsize[((i64)(*p).usertag)];
    q = qq_packed$obj_new_struct((i64)(*p).usertag);
    (*a).objptr = q;
    memcpy((*q).ptr,(*p).ptr,(u64)size);
}

void qq_packed$obj_free_struct(struct qq_decls$objrec *p) {
    mlib$pcm_free((*p).ptr,qq_tables$ttsize[((i64)(*p).usertag)]);
    mlib$pcm_free32(p);
}

i64 qq_packed$var_equal_struct(struct qq_decls$varrec *x,struct qq_decls$varrec *y) {
    return mlib$eqbytes((*(*x).objptr).ptr,(*(*y).objptr).ptr,qq_tables$ttsize[((i64)(*x).tag)]);
}

void qq_packed$var_getix_struct(struct qq_decls$varrec *a,i64 index) {
        struct qq_decls$strec *  d;
        struct qq_decls$strec **  r;
        struct qq_decls$varrec v;
        struct qq_decls$objrec *  p;
    v = (*a);
    p = (*a).objptr;
    if (((index < (i64)1) || (index > qq_tables$ttlength[((i64)(*a).tag)]))) {
        qq_runaux$pcerror((byte*)"struct[int] bounds",(byte*)"");
    }
;
    d = qq_tables$ttnamedef[((i64)(*p).usertag)];
    r = (struct qq_decls$strec **)(((*d).topfieldlist + index) - (i64)1);
    qq_packed$var_loadpacked(((*p).ptr + (i64)(*(*r)).fieldoffset),(i64)(*(*r)).mode,a,0);
}

// START
void qq_packed$start(void) {

}

void qq_parse$parsemodule(struct qq_decls$filerec *pm) {
        struct qq_decls$unitrec *  p;
    if (!!((i64)(*pm).compiled)) {
        return;
    }
;
    qq_decls$currmodule = pm;
    qq_decls$stcurrmodule = (*qq_decls$currmodule).def;
    qq_lex$startlex(qq_decls$currmodule);
    qq_lex$lex();
    qq_lex$lex();
    qq_decls$stcurrproc = qq_decls$stcurrmodule;
    p = qq_parse$readsunit((i64)0);
    (*qq_decls$stcurrmodule).code = ((*pm).ast = p);
    qq_lib$skipsemi();
        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)3)) {
        qq_lib$serror((byte*)"Comma seq not allowed");
    }
    else if (($temp==(i64)62)) {
    }
    else {
        qq_lex$ps((byte*)"EOF");
        qq_lib$serror((byte*)"Bad symbol at eof");
    }
    };
}

static struct qq_decls$unitrec *qq_parse$readexpression(void) {
        struct qq_decls$unitrec *  p;
    p = qq_parse$readterm2();
    if (!!((i64)qq_tables$exprendset[((i64)qq_decls$lx.symbol)])) {
        return p;
    }
;
    if (((i64)qq_decls$lx.symbol == (i64)6)) {
        return qq_parse$readassignment(p);
    }
    else {
        return qq_parse$readorterms(p);
    }
;
}

static struct qq_decls$unitrec *qq_parse$readassignment(struct qq_decls$unitrec *p) {
        i64 pos;
        i64 isdeep;
    if (!!((i64)qq_tables$exprendset[((i64)qq_decls$lx.symbol)])) {
        return p;
    }
;
    p = qq_parse$readorterms(p);
    if (((i64)qq_decls$lx.symbol == (i64)6)) {
        isdeep = (i64)qq_decls$lx.subcode;
        pos = (i64)qq_decls$lx.pos;
        qq_lex$lex();
        p = qq_lib$createunit2((i64)2,p,qq_parse$readassignment(qq_parse$readterm2()));
        (*p).flag = isdeep;
        (*p).pos = pos;
    }
;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readorterms(struct qq_decls$unitrec *p) {
        i64 pos;
        struct qq_decls$unitrec *  q;
        struct qq_decls$unitrec *  r;
    if (!!((i64)qq_tables$exprendset[((i64)qq_decls$lx.symbol)])) {
        return p;
    }
;
    p = qq_parse$readandterms(p);
    L392 :;
    while (((i64)qq_decls$lx.symbol == (i64)30)) {
        pos = (i64)qq_decls$lx.pos;
        qq_lex$lex();
        if (((i64)qq_decls$lx.symbol == (i64)6)) {
            qq_lex$lex();
            p = qq_lib$createunit2((i64)81,p,qq_parse$readexpression());
            (*p).pos = pos;
            goto L394 ;
        }
;
        p = qq_lib$createunit2((i64)73,p,qq_parse$readandterms(qq_parse$readterm2()));
        (*p).pos = pos;
L393 :;
    }
L394 :;
    ;
    L395 :;
    while (((i64)qq_decls$lx.symbol == (i64)8)) {
        qq_lex$lex();
        q = (r = qq_parse$readterm2());
        if (((i64)(*q).tag == (i64)26)) {
            r = (*q).b;
            L398 :;
            while (!!((*r).nextunit)) {
L399 :;
                r = (*r).nextunit;
L401 :;
                            }
L400 :;
            ;
            (*r).nextunit = p;
            p = q;
        }
        else {
            p = qq_lib$createunit2((i64)26,q,p);
        }
;
L396 :;
    }
L397 :;
    ;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readandterms(struct qq_decls$unitrec *p) {
        i64 pos;
    p = qq_parse$readcmpterms(p);
    L402 :;
    while (((i64)qq_decls$lx.symbol == (i64)29)) {
        pos = (i64)qq_decls$lx.pos;
        qq_lex$lex();
        if (((i64)qq_decls$lx.symbol == (i64)6)) {
            qq_lex$lex();
            p = qq_lib$createunit2((i64)80,p,qq_parse$readexpression());
            (*p).pos = pos;
            goto L404 ;
        }
;
        p = qq_lib$createunit2((i64)72,p,qq_parse$readcmpterms(qq_parse$readterm2()));
        (*p).pos = pos;
L403 :;
    }
L404 :;
    ;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readcmpterms(struct qq_decls$unitrec *p) {
        i64 pos;
        i64 n;
        struct qq_decls$unitrec *  px;
        struct qq_decls$unitrec *  q;
        byte conds[4];
    p = qq_parse$readinterms(p);
    if (!(!!((i64)qq_tables$cmpopset[((i64)qq_decls$lx.symbol)]))) {
        return p;
    }
;
    memset(&(conds),0,4);
    px = p;
    p = qq_lib$createunit1((i64)38,p);
    n = (i64)0;
    L405 :;
    while (!!((i64)qq_tables$cmpopset[((i64)qq_decls$lx.symbol)])) {
        ++(n);
        if ((n > (i64)4)) {
            qq_lib$serror((byte*)"cmpchain: Too many items");
        }
;
        conds[(n)-1] = (i64)qq_decls$lx.subcode;
        pos = (i64)qq_decls$lx.pos;
        qq_lex$lex();
        q = qq_parse$readinterms(qq_parse$readterm2());
        (*px).nextunit = q;
        px = q;
        (*q).pos = pos;
L406 :;
    }
L407 :;
    ;
    if ((n == (i64)1)) {
        (*p).tag = (i64)71;
        (*p).condcode = (i64)conds[((i64)1)-1];
        q = (*p).a;
        (*p).b = (*q).nextunit;
        (*q).nextunit = 0;
    }
    else {
        memcpy(&(*p).cmpconds,&conds,4);
    }
;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readinterms(struct qq_decls$unitrec *p) {
        i64 pos;
        i64 tag;
        i64 flag;
    p = qq_parse$readrangeterm(p);
    L408 :;
        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)39) || ($temp==(i64)40)) {
        tag = (((i64)qq_decls$lx.symbol == (i64)39) ? (i64)76 : (i64)77);
        flag = (i64)qq_decls$lx.subcode;
        pos = (i64)qq_decls$lx.pos;
        qq_lex$lex();
        p = qq_lib$createunit2(tag,p,qq_parse$readrangeterm(qq_parse$readterm2()));
        (*p).flag = flag;
        (*p).pos = pos;
    }
    else {
        goto L409 ;
    }
    }goto L408 ;
L409 :;
    ;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readrangeterm(struct qq_decls$unitrec *p) {
        i64 pos;
    p = qq_parse$readaddterms(p);
    if (((i64)qq_decls$lx.symbol == (i64)20)) {
        pos = (i64)qq_decls$lx.pos;
        qq_lex$lex();
        p = qq_lib$createunit2((i64)87,p,qq_parse$readaddterms(qq_parse$readterm2()));
        (*p).pos = pos;
    }
;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readaddterms(struct qq_decls$unitrec *p) {
        i64 pos;
        i64 opc;
        i64 tag;
        struct qq_decls$unitrec *  q;
    p = qq_parse$readmulterms(p);
    L410 :;
    while (!!((i64)qq_tables$addopset[((i64)qq_decls$lx.symbol)])) {
        opc = (i64)qq_decls$lx.subcode;
        if (((i64)qq_decls$lx.symbol == (i64)19)) {
            opc = (i64)118;
        }
;
        pos = (i64)qq_decls$lx.pos;
        qq_lex$lex();
        if (((i64)qq_decls$lx.symbol == (i64)6)) {
            qq_lex$lex();
            if ((opc == (i64)118)) {
                tag = (i64)84;
            }
            else if ((opc == (i64)117)) {
                tag = (i64)85;
            }
            else {
                tag = (i64)79;
            }
;
            p = qq_lib$createunit2(tag,p,qq_parse$readassignment(qq_parse$readterm2()));
            (*p).pclop = opc;
            (*p).pos = pos;
            goto L412 ;
        }
;
        q = qq_parse$readmulterms(qq_parse$readterm2());
        p = qq_lib$createunit2((i64)63,p,q);
        (*p).pclop = opc;
        (*p).pos = pos;
L411 :;
    }
L412 :;
    ;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readmulterms(struct qq_decls$unitrec *p) {
        i64 pos;
        i64 opc;
    p = qq_parse$readpowerterms(p);
    L413 :;
    while (!!((i64)qq_tables$mulopset[((i64)qq_decls$lx.symbol)])) {
        opc = (i64)qq_decls$lx.subcode;
        pos = (i64)qq_decls$lx.pos;
        qq_lex$lex();
        if (((i64)qq_decls$lx.symbol == (i64)6)) {
            qq_lex$lex();
            p = qq_lib$createunit2((i64)79,p,qq_parse$readassignment(qq_parse$readterm2()));
            (*p).pclop = opc;
            (*p).pos = pos;
            goto L415 ;
        }
;
        p = qq_lib$createunit2((i64)63,p,qq_parse$readpowerterms(qq_parse$readterm2()));
        (*p).pclop = opc;
        (*p).pos = pos;
L414 :;
    }
L415 :;
    ;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readpowerterms(struct qq_decls$unitrec *p) {
        i64 pos;
    L416 :;
    while (((i64)qq_decls$lx.symbol == (i64)41)) {
        pos = (i64)qq_decls$lx.pos;
        qq_lex$lex();
        p = qq_lib$createunit2((i64)63,p,qq_parse$readpowerterms(qq_parse$readterm2()));
        (*p).pclop = (i64)120;
        (*p).pos = pos;
L417 :;
    }
L418 :;
    ;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readterm2(void) {
        struct qq_decls$unitrec *  p;
        i64 pos;
    pos = (i64)qq_decls$lx.pos;
    p = qq_parse$readterm();
    p = qq_parse$readtermsuffix(p,pos);
    return p;
}

static struct qq_decls$unitrec *qq_parse$readtermsuffix(struct qq_decls$unitrec *p,i64 pos) {
        struct qq_decls$unitrec *  q;
        i64 nparams;
    L419 :;
        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)9)) {
        qq_lex$lex();
        q = qq_parse$readslist(&nparams,(i64)1);
        qq_lib$skipsymbol((i64)10);
        p = qq_lib$createunit2((i64)26,p,q);
        p = qq_parse$readcondsuffix(p);
    }
    else if (($temp==(i64)15)) {
        p = qq_lib$createunit1((i64)51,p);
        qq_lex$lex();
    }
    else if (($temp==(i64)11)) {
        p = qq_parse$readindex(p,(i64)0);
    }
    else if (($temp==(i64)2)) {
        p = qq_parse$readdotsuffix(p);
    }
    else if (($temp==(i64)13)) {
        p = qq_parse$readkeyindex(p);
    }
    else if (($temp==(i64)5)) {
        if ((qq_parse$listtype==(i64)331808391504)) {
            qq_lex$lex();
            p = qq_lib$createunit2((i64)3,p,qq_parse$readexpression());
        }
        else if ((qq_parse$listtype==(i64)1413695812)) {
            qq_lex$lex();
            p = qq_lib$createunit2((i64)4,p,qq_parse$readexpression());
        }
        else {
            goto L420 ;
        }
;
    }
    else if (($temp==(i64)60)) {
        p = qq_lib$createunit1((i64)61,p);
        (*p).flag = (i64)qq_decls$lx.subcode;
        qq_lex$lex();
    }
    else {
        goto L420 ;
    }
    }goto L419 ;
L420 :;
    ;
    (*p).pos = pos;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readterm(void) {
        struct qq_decls$unitrec *  p;
        struct qq_decls$unitrec *  q;
        struct qq_decls$unitrec *  r;
        i64 opc;
        i64 pos;
        i64 length;
        byte flag;
        u8 *  s;
        struct qq_parse$readterm$dummy ustr;
    pos = (i64)qq_decls$lx.pos;
    switch ((i64)qq_decls$lx.symbol) {
    case 69:;
        {
            p = (struct qq_decls$unitrec *)qq_lib$createname((struct qq_decls$strec *)qq_decls$lx.symptr);
            (*p).pos = (i64)qq_decls$lx.pos;
            qq_lex$lex();
        }
        break;
    case 63:;
        {
            p = qq_lib$createintunit(qq_decls$lx.value);
            qq_lex$lex();
        }
        break;
    case 65:;
        {
            p = qq_lib$createrealunit(qq_decls$lx.xvalue);
            qq_lex$lex();
        }
        break;
    case 67:;
        {
            p = qq_lib$createstringunit(qq_decls$lx.svalue,(i64)-1);
            qq_lex$lex();
        }
        break;
    case 64:;
        {
            p = qq_lib$createstringunit(qq_decls$lx.svalue,(i64)-1);
            (*p).tag = (i64)44;
            qq_lex$lex();
        }
        break;
    case 66:;
        {
            length = strlen(qq_decls$lx.svalue);
            ustr.sa = (i64)0;
            if ((length > (i64)8)) {
                qq_lib$serror((byte*)"char const too long");
            }
;
            memcpy(ustr.str,(void *)qq_decls$lx.svalue,(u64)length);
            p = qq_lib$createintunit(ustr.sa);
            qq_lex$lex();
        }
        break;
    case 9:;
        {
            p = qq_parse$readlbrack();
        }
        break;
    case 70:;
        {
            if (((i64)qq_decls$lx.subcode == (i64)0)) {
                qq_lex$lex();
                if ((((i64)qq_decls$lx.symbol == (i64)2) && ((i64)qq_decls$nextlx.symbol == (i64)108))) {
                    qq_lex$lex();
                    qq_lex$lex();
                    p = qq_lib$createunit0((i64)34);
                    (*p).mode = (i64)0;
                }
                else {
                    p = qq_lib$createunit0((i64)53);
                }
;
            }
            else {
                p = qq_parse$readcast();
            }
;
        }
        break;
    case 22:;
        {
            p = qq_parse$checkoperator();
            if (!(!!(p))) {
                qq_lex$lex();
                p = qq_parse$readterm2();
            }
;
        }
        break;
    case 23:;
        {
            p = qq_parse$checkoperator();
            if (!(!!(p))) {
                qq_lex$lex();
                if (((i64)qq_decls$lx.symbol == (i64)6)) {
                    opc = (i64)74;
                    goto L421 ;
;
                }
;
                p = qq_parse$readterm2();
                if (((i64)(*p).tag == (i64)41)) {
                    (*p).value = -((*p).value);
                }
                else {
                    p = qq_lib$createunit1((i64)62,p);
                    (*p).pclop = (i64)74;
                }
;
            }
;
        }
        break;
    case 50:;
    case 52:;
    case 53:;
    case 54:;
        {
            p = qq_parse$checkoperator();
            if (!(!!(p))) {
                opc = (i64)qq_decls$lx.subcode;
                qq_lex$lex();
                if (((i64)qq_decls$lx.symbol == (i64)6)) {
                    //dounaryto:
L421 :;
;
                    qq_lex$lex();
                    p = qq_lib$createunit1((i64)78,qq_parse$readterm2());
                    (*p).pclop = opc;
                }
                else {
                    p = qq_lib$createunit1((i64)62,qq_parse$readterm2());
                    (*p).pclop = opc;
                }
;
            }
;
        }
        break;
    case 49:;
    case 51:;
        {
            p = qq_parse$checkoperator();
            if (!(!!(p))) {
                opc = (i64)qq_decls$lx.subcode;
                qq_lex$lex();
                if (((i64)qq_decls$lx.symbol == (i64)6)) {
                    opc = ((opc == (i64)74) ? (i64)82 : (i64)83);
                    qq_lex$lex();
                }
;
                p = qq_lib$createunit1(opc,qq_parse$readterm2());
            }
;
        }
        break;
    case 60:;
        {
            p = qq_parse$checkoperator();
            if (!(!!(p))) {
                opc = (i64)qq_decls$lx.subcode;
                qq_lex$lex();
                p = qq_lib$createunit1((i64)60,qq_parse$readterm2());
                (*p).flag = opc;
            }
;
        }
        break;
    case 55:;
        {
            opc = (i64)qq_decls$lx.subcode;
            qq_lex$lex();
            p = qq_lib$createunit1((i64)64,qq_parse$readterm2());
            (*p).pclop = opc;
        }
        break;
    case 24:;
    case 25:;
    case 26:;
    case 27:;
    case 28:;
    case 29:;
    case 30:;
    case 31:;
    case 32:;
    case 33:;
    case 34:;
    case 35:;
    case 39:;
    case 40:;
    case 43:;
    case 44:;
    case 45:;
    case 46:;
    case 47:;
    case 48:;
    case 41:;
    case 37:;
    case 38:;
    case 57:;
    case 132:;
        {
            if (!(!!((p = qq_parse$checkoperator())))) {
                qq_lib$serror((byte*)"Operator?");
            }
;
        }
        break;
    case 11:;
        {
            p = qq_parse$readset();
        }
        break;
    case 36:;
        {
            if (!!((p = qq_parse$checkoperator()))) {
            }
            else {
                p = qq_parse$readpair((i64)63,(i64)qq_decls$lx.subcode);
            }
;
        }
        break;
    case 56:;
        {
            if (!!((p = qq_parse$checkoperator()))) {
            }
            else {
                p = qq_parse$readpair((i64)65,(i64)qq_decls$lx.subcode);
            }
;
        }
        break;
    case 97:;
        {
            p = qq_parse$readsprint();
        }
        break;
    case 19:;
    case 15:;
        {
            flag = (i64)qq_decls$lx.subcode;
            qq_lex$lex();
            p = qq_lib$createunit1((i64)52,qq_parse$readterm2());
            (*p).flag = (i64)flag;
            if (((i64)(*(*p).a).tag == (i64)26)) {
                if (!!((*(*p).a).b)) {
                    qq_lib$serror((byte*)"Params not allowed");
                }
;
                (*p).a = (*(*p).a).a;
            }
;
        }
        break;
    case 121:;
        {
            p = qq_parse$readcompilervar();
            qq_lex$lex();
        }
        break;
    case 122:;
        {
            if (!!(qq_parse$intabledata)) {
                if ((qq_parse$tabledataname == 0)) {
                    qq_lib$serror((byte*)"$:No enum");
                }
;
                s = qq_parse$tabledataname;
                if (((i64)qq_decls$nextlx.symbol == (i64)22)) {
                    qq_lex$lex();
                    qq_lex$lex();
                    qq_lib$checksymbol((i64)63);
                    s += qq_decls$lx.value;
                }
;
                p = qq_lib$createstringunit(s,(i64)-1);
            }
            else {
                if ((qq_parse$ndollar <= (i64)0)) {
                    qq_lib$serror((byte*)"[$] No array");
                }
;
                p = qq_lib$createunit1((i64)66,qq_parse$dollarstack[(qq_parse$ndollar)-1]);
                (*p).pclop = (i64)88;
            }
;
            qq_lex$lex();
        }
        break;
    case 2:;
    case 114:;
        {
            qq_parse$lexchecksymbol((i64)69);
            p = (struct qq_decls$unitrec *)qq_lib$createname((struct qq_decls$strec *)qq_decls$lx.symptr);
            (*p).pos = (i64)qq_decls$lx.pos;
            qq_lex$lex();
        }
        break;
    case 125:;
        {
            p = qq_parse$readpair((i64)37,(i64)0);
        }
        break;
    case 126:;
        {
            qq_parse$lexchecksymbol((i64)9);
            qq_lex$lex();
            p = qq_parse$readexpression();
            qq_lib$skipsymbol((i64)3);
            q = qq_parse$readexpression();
            if ((((i64)qq_decls$lx.symbol == (i64)10) && ((i64)(*q).tag == (i64)87))) {
                r = (*q).b;
                q = (*q).a;
            }
            else {
                qq_lib$skipsymbol((i64)3);
                r = qq_parse$readexpression();
                qq_lib$checksymbol((i64)10);
            }
;
            qq_lex$lex();
            q = qq_lib$createunit2((i64)63,p,q);
            (*q).pclop = (i64)116;
            p = qq_lib$createunit2((i64)63,q,r);
            (*p).pclop = (i64)115;
        }
        break;
    case 94:;
        {
            p = qq_parse$readgoto();
        }
        break;
    case 72:;
        {
            p = qq_parse$readif();
        }
        break;
    case 80:;
        {
            p = qq_parse$readunless();
        }
        break;
    case 81:;
    case 95:;
        {
            p = qq_parse$readswitchcase();
        }
        break;
    case 84:;
        {
            p = qq_parse$readfor();
        }
        break;
    case 85:;
        {
            p = qq_parse$readto();
        }
        break;
    case 87:;
        {
            p = qq_parse$readdo();
        }
        break;
    case 88:;
        {
            p = qq_parse$readwhile();
        }
        break;
    case 89:;
        {
            p = qq_parse$readrepeat();
        }
        break;
    case 93:;
        {
            p = qq_parse$readloopcontrol();
        }
        break;
    case 91:;
        {
            p = qq_parse$readreturn();
        }
        break;
    case 92:;
        {
            p = qq_parse$readstop();
        }
        break;
    case 96:;
    case 18:;
        {
            p = qq_parse$readprint();
        }
        break;
    case 98:;
        {
            p = qq_parse$readread();
        }
        break;
    case 117:;
        {
            p = qq_parse$readtry();
        }
        break;
    case 119:;
        {
            qq_lex$lex();
            p = qq_lib$createunit1((i64)25,qq_parse$readexpression());
        }
        break;
    case 127:;
        {
            p = qq_parse$readpair((i64)29,(i64)0);
        }
        break;
    case 129:;
        {
            p = qq_parse$readhostparams(0,(i64)1);
        }
        break;
    case 130:;
        {
            p = qq_lib$createunit0((i64)28);
            qq_lex$lex();
        }
        break;
    case 131:;
        {
            qq_lex$lex();
            p = qq_lib$createunit1((i64)45,qq_parse$readterm2());
        }
        break;
    case 123:;
        {
            qq_lex$lex();
            p = qq_lib$createunit1((i64)33,qq_parse$readexpression());
        }
        break;
    case 13:;
        {
            p = qq_parse$readlambda();
        }
        break;
    default: {
        //error:
L422 :;
;
        msysc$m_print_startcon();
        msysc$m_print_u64(qq_tables$symbolnames[((i64)qq_decls$lx.symbol)-1],(byte*)"d");
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        qq_lib$serror((byte*)"readterm?");
    }
    } //SW
;
    (*p).pos = pos;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readsunit(i64 inwhile) {
        i64 lineno;
        i64 globalflag;
        i64 staticflag;
        struct qq_decls$unitrec *  ulist;
        struct qq_decls$unitrec *  ulistx;
        struct qq_decls$unitrec *  p;
        struct qq_decls$unitrec *  q;
        struct qq_decls$unitrec *  r;
    lineno = (i64)qq_decls$lx.pos;
    ulist = (ulistx = 0);
    globalflag = (i64)0;
    staticflag = (i64)0;
    L423 :;
    do {
        L426 :;
        while (((i64)qq_decls$lx.symbol == (i64)4)) {
            qq_lex$lex();
L427 :;
        }
L428 :;
        ;
        switch ((i64)qq_decls$lx.symbol) {
        case 115:;
            {
                qq_lex$lex();
                staticflag = (i64)1;
                goto L423 ;
            }
            break;
        case 114:;
            {
                if (!!(globalflag)) {
                    qq_lib$serror((byte*)"global global?");
                }
;
                globalflag = (i64)qq_decls$lx.subcode;
                qq_lex$lex();
                goto L423 ;
            }
            break;
        case 99:;
        case 100:;
            {
                qq_parse$readprocdef(globalflag);
                globalflag = (i64)0;
            }
            break;
        case 110:;
            {
                q = qq_parse$readvardef(globalflag,staticflag);
                L429 :;
                while (!!(q)) {
                    r = (*q).nextunit;
                    (*q).nextunit = 0;
                    qq_lib$addlistunit(&ulist,&ulistx,q);
                    q = r;
L430 :;
                }
L431 :;
                ;
                globalflag = (staticflag = (i64)0);
            }
            break;
        case 113:;
            {
                if (!!(staticflag)) {
                    qq_lib$serror((byte*)"static?");
                }
;
                qq_parse$readconstdef(globalflag);
                globalflag = (i64)0;
            }
            break;
        case 108:;
            {
                qq_parse$readtypedef(globalflag);
                globalflag = (i64)0;
            }
            break;
        case 102:;
        case 103:;
            {
                qq_parse$readrecorddef(globalflag,0);
                globalflag = (i64)0;
            }
            break;
        case 124:;
            {
                qq_parse$readtabledef(globalflag);
                globalflag = (i64)0;
            }
            break;
        case 107:;
            {
                qq_parse$readimportdll();
            }
            break;
        case 111:;
            {
                qq_parse$readmacrodef(globalflag);
                globalflag = (i64)0;
            }
            break;
        case 62:;
            {
                goto L425 ;
            }
            break;
        case 10:;
        case 73:;
        case 74:;
        case 75:;
        case 90:;
        case 83:;
        case 7:;
        case 76:;
        case 77:;
        case 118:;
        case 79:;
        case 14:;
            {
                goto L425 ;
            }
            break;
        case 69:;
            {
                                {i64 $temp = (i64)qq_decls$nextlx.symbol;
if (($temp==(i64)5)) {
                    p = qq_lib$createunit1((i64)1,(struct qq_decls$unitrec *)qq_lib$createname((struct qq_decls$strec *)qq_names$addsymbol(qq_decls$stcurrproc,(struct qq_decls$strec *)qq_decls$lx.symptr,(i64)17,(i64)0)));
                    qq_lex$lex();
                    qq_decls$lx.symbol = (i64)4;
                    qq_lib$addlistunit(&ulist,&ulistx,p);
                }
                else {
                    goto L432 ;
;
                }
                };
            }
            break;
        case 87:;
            {
                if (!!(inwhile)) {
                    goto L425 ;
                }
;
                goto L432 ;
;
            }
            break;
        case 105:;
        case 106:;
            {
                L433 :;
                do {
                    qq_lex$lex();
L434 :;
                }
                while (!((i64)qq_decls$lx.symbol == (i64)4));
L435 :;
                ;
            }
            break;
        case 4:;
            {
            }
            break;
        case 11:;
            {
                goto L432 ;
;
            }
            break;
        default: {
            //doexec:
L432 :;
;
            p = qq_parse$readexpression();
            if ((((i64)(*p).tag == (i64)39) && ((i64)qq_decls$lx.symbol == (i64)69))) {
                qq_lib$serror((byte*)"Possibly var/let needed");
            }
;
            qq_lib$addlistunit(&ulist,&ulistx,p);
            if (((i64)qq_decls$lx.symbol == (i64)87)) {
                goto L425 ;
            }
;
        }
        } //SW
;
L424 :;
    }
    while (!((i64)qq_decls$lx.symbol != (i64)4));
L425 :;
    ;
        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)10) || ($temp==(i64)73) || ($temp==(i64)74) || ($temp==(i64)75) || ($temp==(i64)90) || ($temp==(i64)83) || ($temp==(i64)87) || ($temp==(i64)7) || ($temp==(i64)76) || ($temp==(i64)77) || ($temp==(i64)118) || ($temp==(i64)79) || ($temp==(i64)14) || ($temp==(i64)3) || ($temp==(i64)16) || ($temp==(i64)62)) {
    }
    else {
        qq_lib$serror((byte*)"Readsunit: \";\" expected, or bad unit starter");
    }
    };
    if (((ulist == 0) || !!((*ulist).nextunit))) {
        return qq_lib$createunit1((i64)6,ulist);
    }
    else {
        return ulist;
    }
;
}

static void qq_parse$checkequals(void) {
    if (((i64)qq_decls$lx.symbol != (i64)43)) {
        qq_lib$serror((byte*)"\"=\" expected");
    }
;
}

static struct qq_decls$unitrec *qq_parse$readindex(struct qq_decls$unitrec *p,i64 dot) {
        struct qq_decls$unitrec *  q;
    qq_lex$lex();
    L436 :;
    while (1) {
        if ((qq_parse$ndollar >= (i64)10)) {
            qq_lib$serror((byte*)"Too many nested a[$]");
        }
;
        qq_parse$dollarstack[(++(qq_parse$ndollar))-1] = p;
        q = qq_parse$readexpression();
        --(qq_parse$ndollar);
        p = qq_lib$createunit2((!!(dot) ? (i64)48 : (i64)47),p,q);
        if (((i64)qq_decls$lx.symbol != (i64)3)) {
            goto L437 ;
        }
;
        qq_lex$lex();
    }
L437 :;
    ;
    qq_lib$skipsymbol((i64)12);
    return p;
}

static struct qq_decls$unitrec *qq_parse$readdotsuffix(struct qq_decls$unitrec *p) {
        byte flag;
    L438 :;
    while (((i64)qq_decls$lx.symbol == (i64)2)) {
        qq_lex$lex();
                {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)11)) {
            p = qq_parse$readindex(p,(i64)1);
        }
        else if (($temp==(i64)69)) {
            p = qq_lib$createunit2((i64)46,p,(struct qq_decls$unitrec *)qq_lib$createname((struct qq_decls$strec *)qq_decls$lx.symptr));
            qq_lex$lex();
        }
        else if (($temp==(i64)57)) {
            //doprop:
L441 :;
;
            p = qq_lib$createunit1((i64)66,p);
            (*p).pclop = (i64)qq_decls$lx.subcode;
            qq_lex$lex();
        }
        else if (($temp==(i64)108)) {
            if (((i64)(*p).tag != (i64)34)) {
                flag = (i64)1;
                //dogettype:
L442 :;
;
                p = qq_lib$createunit1((i64)68,p);
                (*p).pclop = (i64)flag;
            }
;
            qq_lex$lex();
        }
        else if (($temp==(i64)36)) {
            qq_decls$lx.subcode = (((i64)qq_decls$lx.subcode == (i64)115) ? (i64)94 : (i64)95);
            goto L441 ;
;
        }
        else if (($temp==(i64)59)) {
                        {i64 $temp = (i64)qq_decls$lx.subcode;
if (($temp==(i64)98)) {
                flag = (i64)0;
                goto L442 ;
;
            }
            else if (($temp==(i64)101)) {
                flag = (i64)2;
                goto L442 ;
;
            }
            else {
                p = qq_lib$createunit1((i64)70,p);
                (*p).flag = (i64)((i64)qq_decls$lx.subcode != (i64)118);
                qq_lex$lex();
            }
            };
        }
        else if (($temp==(i64)58)) {
            p = qq_lib$createunit1((i64)69,p);
            (*p).mode = (i64)qq_decls$lx.subcode;
            qq_lex$lex();
        }
        else if (($temp==(i64)122)) {
            if (!(((i64)(*p).tag == (i64)39 || (i64)(*p).tag == (i64)46))) {
                qq_lib$serror((byte*)"...name.$ needed");
            }
;
            p = qq_lib$createunit1((i64)40,p);
            qq_lex$lex();
        }
        else {
            qq_lib$serror((byte*)"Unknown dot suffix");
        }
        };
L439 :;
    }
L440 :;
    ;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readslist(i64 *nparams,i64 ftrailing) {
        struct qq_decls$unitrec *  ulist;
        struct qq_decls$unitrec *  ulistx;
        i64 donulls;
    ulist = (ulistx = 0);
    (*nparams) = (i64)0;
    qq_lib$skipsemi();
    if (((i64)qq_decls$lx.symbol == (i64)10)) {
        return ulist;
    }
;
    qq_parse$pushlisttype((i64)331808391504);
    donulls = (i64)1;
    L443 :;
    while (1) {
        qq_lib$skipsemi();
                {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)3)) {
            qq_lib$serror((byte*)"null comma expr not allowed");
        }
        else if (($temp==(i64)10)) {
            goto L444 ;
        }
        else {
            qq_lib$addlistunit(&ulist,&ulistx,qq_parse$readexpression());
            ++((*nparams));
            if (((i64)qq_decls$lx.symbol == (i64)3)) {
                qq_lex$lex();
                if (((i64)qq_decls$lx.symbol == (i64)10)) {
                    if ((((*nparams) != (i64)1) || !(!!(ftrailing)))) {
                        qq_lib$serror((byte*)"Trailing comma");
                    }
;
                    goto L444 ;
                }
;
            }
            else {
                qq_lib$skipsemi();
                if (((i64)qq_decls$lx.symbol == (i64)10)) {
                    goto L444 ;
                }
;
                qq_lib$serror((byte*)"SLIST?");
            }
;
        }
        };
    }
L444 :;
    ;
    qq_parse$poplisttype();
    return ulist;
}

static struct qq_decls$unitrec *qq_parse$readcondsuffix(struct qq_decls$unitrec *p) {
        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)83)) {
        qq_lex$lex();
        return qq_lib$createunit2((i64)7,qq_parse$readexpression(),qq_lib$createunit1((i64)6,p));
    }
    else if (($temp==(i64)80)) {
        qq_lex$lex();
        return qq_lib$createunit2((i64)7,qq_lib$createunit1((i64)74,qq_parse$readexpression()),qq_lib$createunit1((i64)6,p));
    }
    else {
        return p;
    }
    };
}

static struct qq_decls$unitrec *qq_parse$readkeyindex(struct qq_decls$unitrec *p) {
        struct qq_decls$unitrec *  q;
    qq_lex$lex();
    q = qq_parse$readexpression();
    if (((i64)qq_decls$lx.symbol == (i64)3)) {
        qq_lex$lex();
        (*q).nextunit = qq_parse$readexpression();
    }
;
    p = qq_lib$createunit2((i64)49,p,q);
    qq_lib$skipsymbol((i64)14);
    return p;
}

static struct qq_decls$unitrec *qq_parse$readlbrack(void) {
        struct qq_decls$unitrec *  ulist;
        struct qq_decls$unitrec *  ulistx;
        struct qq_decls$unitrec *  p;
        struct qq_decls$unitrec *  q;
        struct qq_decls$unitrec *  r;
        i64 length;
        i64 lower;
        i64 lowerseen;
        i64 elemtype;
        i64 opc;
    qq_lex$lex();
    ulist = (ulistx = 0);
    length = (i64)0;
    lower = (i64)1;
    lowerseen = (i64)0;
    elemtype = (i64)0;
    if ((((i64)qq_decls$lx.symbol == (i64)70) && ((i64)qq_decls$nextlx.symbol == (i64)5))) {
        elemtype = (i64)qq_decls$lx.subcode;
        qq_lex$lex();
        qq_lex$lex();
    }
;
    if ((((i64)qq_decls$lx.symbol == (i64)63) && ((i64)qq_decls$nextlx.symbol == (i64)5))) {
        lower = qq_decls$lx.value;
        lowerseen = (i64)1;
        qq_lex$lex();
        qq_lex$lex();
    }
;
        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)10)) {
        qq_lex$lex();
        p = qq_lib$createunit0((i64)88);
        (*p).length = (i64)0;
        (*p).lower = lower;
        (*p).elemtype = elemtype;
        return p;
    }
    else {
        if ((((!!((i64)qq_tables$binopset[((i64)qq_decls$lx.symbol)]) || !!((i64)qq_tables$unaryopset[((i64)qq_decls$lx.symbol)])) || ((i64)qq_decls$lx.symbol == (i64)57)) && ((i64)qq_decls$nextlx.symbol == (i64)10))) {
            if (((i64)qq_decls$lx.symbol == (i64)19)) {
                opc = (i64)118;
            }
            else {
                opc = (i64)qq_decls$lx.subcode;
            }
;
            //doopc:
L445 :;
;
            p = qq_lib$createunit0((i64)5);
            (*p).pclop = opc;
            qq_lex$lex();
            qq_lib$skipsymbol((i64)10);
            return p;
        }
        else {
                        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)132)) {
                                {i64 $temp = (i64)qq_decls$lx.subcode;
if (($temp==(i64)45)) {
                    opc = (i64)74;
                }
                else if (($temp==(i64)23899)) {
                    opc = (i64)131;
                }
                else {
                    opc = (i64)0;
                }
                };
                goto L445 ;
;
            }
            else if (($temp==(i64)39)) {
                opc = (i64)112;
                goto L445 ;
;
            }
            else if (($temp==(i64)40)) {
                opc = (i64)113;
                goto L445 ;
;
            }
            else {
                p = qq_parse$readexpression();
            }
            };
        }
;
    }
    };
        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)10)) {
        qq_lex$lex();
        if (!!(lowerseen)) {
            p = qq_lib$createunit2((i64)4,qq_lib$createintunit(lower),p);
        }
;
        return p;
    }
    else if (($temp==(i64)3)) {
        length = (i64)1;
        if (((i64)qq_decls$nextlx.symbol == (i64)10)) {
            qq_lex$lex();
            qq_lex$lex();
            p = qq_lib$createunit1((i64)88,p);
            (*p).length = length;
            (*p).lower = lower;
            (*p).elemtype = elemtype;
            return p;
        }
;
        ulist = (ulistx = p);
        L446 :;
        do {
            qq_lex$lex();
            if (((i64)qq_decls$lx.symbol == (i64)10)) {
                goto L448 ;
            }
;
            if (((i64)qq_decls$lx.symbol == (i64)3)) {
                qq_lib$serror((byte*)", , null expr not allowed");
            }
;
            qq_lib$addlistunit(&ulist,&ulistx,qq_parse$readexpression());
            ++(length);
            qq_lib$skipsemi();
L447 :;
        }
        while (!((i64)qq_decls$lx.symbol != (i64)3));
L448 :;
        ;
        qq_lib$skipsymbol((i64)10);
        p = qq_lib$createunit1((i64)88,ulist);
        (*p).length = length;
        (*p).lower = lower;
        (*p).elemtype = elemtype;
        return p;
    }
    else if (($temp==(i64)16)) {
        qq_lex$lex();
        q = qq_parse$readexpression();
                {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)16)) {
            qq_lex$lex();
            r = qq_parse$readsunit((i64)0);
            qq_lib$skipsymbol((i64)10);
            (*q).nextunit = r;
            return qq_lib$createunit2((i64)7,p,q);
        }
        else if (($temp==(i64)10)) {
            qq_lex$lex();
            return qq_lib$createunit2((i64)7,p,q);
        }
        };
        qq_lib$addlistunit(&ulist,&ulistx,q);
        qq_lib$checksymbol((i64)3);
        if (((i64)qq_decls$nextlx.symbol != (i64)16)) {
            L449 :;
            do {
                qq_lex$lex();
                qq_lib$addlistunit(&ulist,&ulistx,qq_parse$readexpression());
L450 :;
            }
            while (!((i64)qq_decls$lx.symbol != (i64)3));
L451 :;
            ;
            qq_lib$checksymbol((i64)16);
        }
        else {
            qq_lex$lex();
        }
;
        qq_lex$lex();
        r = qq_parse$readexpression();
        qq_lib$skipsymbol((i64)10);
        (*p).nextunit = r;
        return qq_lib$createunit2((i64)8,p,ulist);
    }
    else if (($temp==(i64)4)) {
        ulist = (ulistx = p);
        L452 :;
        do {
            qq_lib$skipsemi();
            if (((i64)qq_decls$lx.symbol == (i64)10)) {
                goto L454 ;
            }
;
            qq_lib$addlistunit(&ulist,&ulistx,qq_parse$readexpression());
L453 :;
        }
        while (!((i64)qq_decls$lx.symbol != (i64)4));
L454 :;
        ;
        qq_lib$skipsymbol((i64)10);
        return qq_parse$makeblock(ulist);
    }
    else {
        qq_lib$serror((byte*)"(x ...");
    }
    };
    return (struct qq_decls$unitrec *)0;
}

static struct qq_decls$unitrec *qq_parse$readif(void) {
        i64 line;
        i64 kwd;
        struct qq_decls$unitrec *  pthen;
        struct qq_decls$unitrec *  pcond;
        struct qq_decls$unitrec *  pelse;
        struct qq_decls$unitrec *  p;
    line = (i64)qq_decls$lx.pos;
    kwd = (i64)qq_decls$lx.symbol;
    qq_lex$lex();
    pcond = qq_parse$readsunit((i64)0);
    qq_lib$skipsemi();
    qq_lib$skipsymbol((i64)73);
    pthen = qq_parse$readsunit((i64)0);
        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)74)) {
        qq_decls$lx.symbol = (i64)72;
        pelse = qq_parse$readif();
    }
    else if (($temp==(i64)75)) {
        qq_lex$lex();
        pelse = qq_parse$readsunit((i64)0);
        qq_parse$checkend(kwd,(i64)0,(i64)0);
    }
    else if (($temp==(i64)76) || ($temp==(i64)77)) {
        qq_decls$lx.symbol = kwd;
        pelse = qq_parse$makeblock(qq_parse$readswitchcase());
    }
    else {
        pelse = 0;
        qq_parse$checkend(kwd,(i64)0,(i64)0);
    }
    };
    (*pthen).nextunit = pelse;
    p = qq_lib$createunit2((i64)7,pcond,pthen);
    (*p).pos = line;
    return p;
}

static void qq_parse$checkend(i64 endkwd1,i64 endkwd2,i64 startline) {
        u8 str[256];
    qq_lib$skipsemi();
    if (((i64)qq_decls$lx.symbol != (i64)79)) {
        qq_lib$serror((byte*)"'End' expected");
    }
;
    if (!!((i64)qq_decls$lx.subcode)) {
        if (((i64)qq_decls$lx.subcode == endkwd1 || (i64)qq_decls$lx.subcode == endkwd2)) {
            qq_lex$lex();
            return;
        }
        else {
            //error:
L455 :;
;
            strcpy(str,(byte*)"Mismatched end ");
            if (!!(startline)) {
                msysc$m_print_startstr((str + strlen((u8 *)str)));
                msysc$m_print_setfmt((byte*)" (from line #)");
                msysc$m_print_i64(startline,NULL);
                msysc$m_print_end();
                ;
            }
;
            qq_lib$serror((u8 *)str);
        }
;
    }
;
    qq_lex$lex();
    if (((i64)qq_decls$lx.symbol == endkwd1 || (i64)qq_decls$lx.symbol == endkwd2)) {
        qq_lex$lex();
    }
    else if (((i64)qq_decls$lx.symbol != (i64)4)) {
        goto L455 ;
;
    }
;
}

static struct qq_decls$unitrec *qq_parse$readunless(void) {
        i64 line;
        struct qq_decls$unitrec *  pcond;
        struct qq_decls$unitrec *  pthen;
        struct qq_decls$unitrec *  pelse;
        struct qq_decls$unitrec *  p;
    line = (i64)qq_decls$lx.pos;
    qq_lex$lex();
    pcond = qq_parse$readsunit((i64)0);
    qq_lib$skipsymbol((i64)73);
    pthen = qq_parse$readsunit((i64)0);
    if (((i64)qq_decls$lx.symbol == (i64)75)) {
        qq_lex$lex();
        pelse = qq_parse$readsunit((i64)0);
    }
    else {
        pelse = 0;
    }
;
    qq_parse$checkend((i64)80,(i64)0,(i64)0);
    (*pthen).nextunit = pelse;
    p = qq_lib$createunit2((i64)7,qq_lib$createunit1((i64)74,pcond),pthen);
    (*p).pos = line;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readwhile(void) {
        i64 pos;
        struct qq_decls$unitrec *  pcond;
        struct qq_decls$unitrec *  pbody;
        struct qq_decls$unitrec *  p;
    pos = (i64)qq_decls$lx.pos;
    qq_lex$lex();
    pcond = qq_parse$readsunit((i64)1);
    if (((i64)qq_decls$lx.symbol == (i64)3)) {
        qq_lex$lex();
        (*pcond).nextunit = qq_parse$readsunit((i64)1);
    }
;
    qq_lib$skipsymbol((i64)87);
    pbody = qq_parse$readsunit((i64)0);
    qq_parse$checkend((i64)88,(i64)87,(i64)0);
    p = qq_lib$createunit2((i64)21,pcond,pbody);
    (*p).pos = pos;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readrepeat(void) {
        i64 pos;
        struct qq_decls$unitrec *  pbody;
        struct qq_decls$unitrec *  pcond;
        struct qq_decls$unitrec *  p;
    pos = (i64)qq_decls$lx.pos;
    qq_lex$lex();
    pbody = qq_parse$readsunit((i64)0);
    qq_lib$skipsymbol((i64)90);
    pcond = qq_parse$readexpression();
    p = qq_lib$createunit2((i64)22,pbody,pcond);
    (*p).pos = pos;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readfor(void) {
        i64 line;
        i64 opc;
        i64 down;
        i64 isforeach;
        struct qq_decls$unitrec *  pstep;
        struct qq_decls$unitrec *  pvar;
        struct qq_decls$unitrec *  pcond;
        struct qq_decls$unitrec *  pfrom;
        struct qq_decls$unitrec *  pto;
        struct qq_decls$unitrec *  pelse;
        struct qq_decls$unitrec *  pbody;
        struct qq_decls$unitrec *  p;
        struct qq_decls$unitrec *  plist;
        struct qq_decls$unitrec *  pvar2;
    line = (i64)qq_decls$lx.pos;
    isforeach = (i64)qq_decls$lx.subcode;
    qq_lex$lex();
    pvar = qq_parse$readterm2();
    if (((i64)(*pvar).tag != (i64)39)) {
        qq_lib$serror((byte*)"For: name expected");
    }
    else {
        (*(*pvar).def).forindex = (i64)1;
    }
;
    opc = (i64)15;
    pstep = 0;
    pcond = 0;
    pvar2 = 0;
    down = (i64)0;
    if (((i64)qq_decls$lx.symbol == (i64)3)) {
        qq_lex$lex();
        pvar2 = qq_parse$readterm2();
    }
;
    if (((i64)qq_decls$lx.symbol == (i64)39)) {
        qq_lex$lex();
        plist = qq_parse$readexpression();
                {i64 $temp = (i64)(*plist).tag;
if (($temp==(i64)87)) {
            pfrom = (*plist).a;
            pto = (*plist).b;
        }
        else if (($temp==(i64)67)) {
            (*plist).flag = (i64)1;
            opc = (i64)16;
        }
        else {
            opc = (i64)17;
        }
        };
    }
    else {
        if (((i64)qq_decls$lx.symbol == (i64)6)) {
            qq_lex$lex();
            pfrom = qq_parse$readexpression();
        }
        else {
            pfrom = qq_lib$createintunit((i64)1);
        }
;
        qq_lib$checksymbol((i64)85);
        down = (i64)((i64)qq_decls$lx.subcode == (i64)1);
        qq_lex$lex();
        pto = qq_parse$readexpression();
        if (((i64)qq_decls$lx.symbol == (i64)86)) {
            qq_lex$lex();
            pstep = qq_parse$readexpression();
            if (((i64)(*pstep).tag != (i64)41)) {
                qq_lib$serror((byte*)"BY needs int constant");
            }
;
            if (((*pstep).value < (i64)0)) {
                qq_lib$serror((byte*)"Step must be positive");
            }
            else if (((*pstep).value == (i64)0)) {
                qq_lib$serror((byte*)"Zero step");
            }
;
            (*pstep).value = m$llabs((*pstep).value);
            if (((*pstep).value == (i64)1)) {
                pstep = 0;
            }
;
        }
;
    }
;
    if (((i64)qq_decls$lx.symbol == (i64)83)) {
        qq_lex$lex();
        pcond = qq_parse$readexpression();
    }
;
    qq_lib$skipsymbol((i64)87);
    pbody = qq_parse$readsunit((i64)0);
    if ((pcond != 0)) {
        pbody = qq_parse$makeblock(qq_lib$createunit2((i64)7,pcond,pbody));
    }
;
    if (((i64)qq_decls$lx.symbol == (i64)75)) {
        qq_lex$lex();
        pelse = qq_parse$readsunit((i64)0);
        (*pbody).nextunit = pelse;
    }
    else {
        pelse = 0;
    }
;
    qq_parse$checkend((i64)84,(i64)87,(i64)0);
    if ((opc==(i64)17)) {
        (*pvar).nextunit = plist;
        (*plist).nextunit = pvar2;
        p = qq_lib$createunit2(opc,pvar,pbody);
    }
    else if ((opc==(i64)16)) {
        (*pvar).nextunit = plist;
        p = qq_lib$createunit2(opc,pvar,pbody);
    }
    else {
        (*pvar).nextunit = pfrom;
        (*pfrom).nextunit = pto;
        (*pto).nextunit = pstep;
        p = qq_lib$createunit2(opc,pvar,pbody);
    }
;
    (*p).flag = down;
    if (!!(isforeach)) {
        if (((i64)(*p).tag == (i64)17)) {
            (*p).tag = (i64)18;
        }
        else {
            qq_lib$serror((byte*)"Foreach?");
        }
;
    }
;
    (*p).pos = line;
    if ((!!(pvar2) && (opc != (i64)17))) {
        qq_lib$serror((byte*)"for i, j not allowed");
    }
;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readdo(void) {
        struct qq_decls$unitrec *  p;
        i64 line;
    line = (i64)qq_decls$lx.pos;
    qq_lex$lex();
    p = qq_parse$readsunit((i64)0);
    qq_parse$checkend((i64)87,(i64)0,(i64)0);
    p = qq_lib$createunit1((i64)19,p);
    (*p).pos = line;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readto(void) {
        i64 line;
        struct qq_decls$unitrec *  p;
        struct qq_decls$unitrec *  pcount;
        struct qq_decls$unitrec *  pbody;
    line = (i64)qq_decls$lx.pos;
    qq_lex$lex();
    pcount = qq_parse$readexpression();
    qq_lib$skipsymbol((i64)87);
    pbody = qq_parse$readsunit((i64)0);
    qq_parse$checkend((i64)85,(i64)87,(i64)0);
    (*pcount).nextunit = qq_lib$createavname();
    p = qq_lib$createunit2((i64)20,pcount,pbody);
    (*p).pos = line;
    return p;
}

static struct qq_decls$unitrec *qq_parse$makeblock(struct qq_decls$unitrec *p) {
    return qq_lib$createunit1((i64)6,p);
}

static struct qq_decls$unitrec *qq_parse$readvardef(i64 isglobal,i64 isstatic) {
        i64 nvars;
        i64 varid;
        i64 opc;
        struct qq_decls$strec *  d;
        struct qq_decls$unitrec *  ulist;
        struct qq_decls$unitrec *  ulistx;
        struct qq_decls$unitrec *  p;
    qq_lex$lex();
    if (((i64)(*qq_decls$stcurrproc).nameid == (i64)5 || (i64)(*qq_decls$stcurrproc).nameid == (i64)6)) {
        varid = (!!(isstatic) ? (i64)13 : (i64)14);
    }
    else {
        varid = (i64)13;
    }
;
    nvars = (i64)0;
    ulist = (ulistx = 0);
    L456 :;
    while (((i64)qq_decls$lx.symbol == (i64)69)) {
        ++(nvars);
        d = qq_names$addsymbol(qq_decls$stcurrproc,(struct qq_decls$strec *)qq_decls$lx.symptr,varid,isglobal);
        qq_lex$lex();
                {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)6)) {
            opc = (i64)qq_decls$lx.subcode;
            if ((varid == (i64)13)) {
                if (((i64)(*qq_decls$stcurrproc).nameid == (i64)5 || (i64)(*qq_decls$stcurrproc).nameid == (i64)6)) {
                    qq_lib$serror((byte*)"Need '=' for static in proc");
                }
;
            }
;
            (*d).flags = msysc$m_setdotslice((*d).flags,(i64)11,(i64)12,(u64)(opc + (i64)2));
            qq_lex$lex();
            (*d).code = qq_parse$readexpression();
            if ((varid == (i64)14)) {
                p = qq_lib$createunit2((i64)2,(struct qq_decls$unitrec *)qq_lib$createname((struct qq_decls$strec *)d),(*d).code);
                (*p).flag = opc;
                qq_lib$addlistunit(&ulist,&ulistx,p);
            }
;
        }
        else if (($temp==(i64)43)) {
            if ((varid != (i64)13)) {
                qq_lib$serror((byte*)"Need ':=' for non-static");
            }
;
            qq_lex$lex();
            (*d).flags = msysc$m_setdotslice((*d).flags,(i64)11,(i64)12,(u64)1u);
            (*d).code = qq_parse$readexpression();
        }
        };
        if (((i64)qq_decls$lx.symbol != (i64)3)) {
            goto L458 ;
        }
;
        qq_lex$lex();
L457 :;
    }
L458 :;
    ;
    if ((nvars == (i64)0)) {
        qq_lib$serror((byte*)"No vars declared");
    }
;
    return ulist;
}

static void qq_parse$readconstdef(i64 isglobal) {
        i64 nvars;
        struct qq_decls$strec *  d;
    qq_lex$lex();
    nvars = (i64)0;
    L459 :;
    while (((i64)qq_decls$lx.symbol == (i64)69)) {
        ++(nvars);
        d = qq_names$addsymbol(qq_decls$stcurrproc,(struct qq_decls$strec *)qq_decls$lx.symptr,(i64)18,isglobal);
        qq_parse$lexchecksymbol((i64)43);
        qq_lex$lex();
        (*d).code = qq_parse$readexpression();
        if (((i64)qq_decls$lx.symbol != (i64)3)) {
            goto L461 ;
        }
;
        qq_lex$lex();
L460 :;
    }
L461 :;
    ;
    if ((nvars == (i64)0)) {
        qq_lib$serror((byte*)"No consts declared");
    }
;
}

static struct qq_decls$unitrec *qq_parse$readreturn(void) {
        struct qq_decls$unitrec *  p;
        struct qq_decls$unitrec *  q;
    qq_lex$lex();
    q = 0;
    if (!!((i64)qq_tables$exprstarterset[((i64)qq_decls$lx.symbol)])) {
        q = qq_parse$readexpression();
    }
;
    p = qq_lib$createunit1((i64)32,q);
    return qq_parse$readcondsuffix(p);
}

static struct qq_decls$unitrec *qq_parse$readprint(void) {
        i64 opc;
        i64 flags;
        i64 fshowname;
        struct qq_decls$unitrec *  pformat;
        struct qq_decls$unitrec *  pdev;
        struct qq_decls$unitrec *  printlist;
        struct qq_decls$unitrec *  printlistx;
        struct qq_decls$unitrec *  p;
        struct qq_decls$unitrec *  q;
        struct mlib$strbuffer *  expr;
        u8 *  s;
    qq_parse$pushlisttype((i64)362090680912);
    opc = (i64)54;
    flags = (i64)qq_decls$lx.subcode;
    if (((i64)qq_decls$lx.symbol == (i64)18)) {
        flags = (i64)1;
    }
    else if (!!((flags & (i64)2))) {
        opc = (i64)55;
    }
;
    qq_lex$lex();
    printlist = (printlistx = 0);
    pformat = (pdev = 0);
    if (((i64)qq_decls$lx.symbol == (i64)17)) {
        qq_lex$lex();
        pdev = qq_parse$readexpression();
        if (((i64)qq_decls$lx.symbol == (i64)3)) {
            qq_lex$lex();
        }
        else {
            goto L462 ;
;
        }
;
    }
;
    if ((opc == (i64)55)) {
        pformat = qq_parse$readexpression();
        if (((i64)qq_decls$lx.symbol == (i64)3)) {
            qq_lex$lex();
        }
        else {
            goto L462 ;
;
        }
;
    }
;
    if (!(!!((i64)qq_tables$exprstarterset[((i64)qq_decls$lx.symbol)]))) {
        goto L462 ;
;
    }
;
    L463 :;
    while (1) {
                {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)3)) {
            qq_lib$addlistunit(&printlist,&printlistx,qq_lib$createunit0((i64)56));
        }
        else if (($temp==(i64)122)) {
            qq_lib$addlistunit(&printlist,&printlistx,qq_lib$createunit0((i64)57));
            qq_lex$lex();
        }
        else {
            fshowname = (i64)0;
            if (((i64)qq_decls$lx.symbol == (i64)43)) {
                fshowname = (i64)1;
                qq_lex$lex();
            }
;
            p = qq_parse$readexpression();
            if (((i64)qq_decls$lx.symbol == (i64)5)) {
                qq_lex$lex();
                p = qq_lib$createunit2((i64)58,p,qq_parse$readexpression());
            }
;
            if (!!(fshowname)) {
                expr = (struct mlib$strbuffer *)qq_lib$strexpr(p);
                mlib$strbuffer_add((struct mlib$strbuffer *)expr,(byte*)"=",(i64)-1);
                s = (*expr).strptr;
                mlib$iconvucn((*expr).strptr,(i64)(*expr).length);
                qq_lib$addlistunit(&printlist,&printlistx,(q = qq_lib$createstringunit(s,(i64)(*expr).length)));
            }
;
            qq_lib$addlistunit(&printlist,&printlistx,p);
        }
        };
        if (((i64)qq_decls$lx.symbol != (i64)3)) {
            goto L464 ;
        }
;
        qq_lex$lex();
    }
L464 :;
    ;
    //finish:
L462 :;
;
    if (!(!!((flags & (i64)1)))) {
        if ((((opc == (i64)54) && (printlist == 0)) || (((opc == (i64)55) && (printlist == 0)) && (pformat == 0)))) {
            qq_lib$serror((byte*)"No print items");
        }
;
    }
;
    qq_parse$poplisttype();
    if ((opc == (i64)55)) {
        if ((pformat == 0)) {
            qq_lib$serror((byte*)"No fmt str");
        }
;
        if ((pformat == 0)) {
            pformat = qq_parse$makeblock(pformat);
        }
;
        (*pformat).nextunit = printlist;
        p = qq_lib$createunit2(opc,pdev,pformat);
    }
    else {
        p = qq_lib$createunit2(opc,pdev,printlist);
    }
;
    (*p).flag = flags;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readread(void) {
        i64 opc;
        i64 flags;
        struct qq_decls$unitrec *  pformat;
        struct qq_decls$unitrec *  pdev;
        struct qq_decls$unitrec *  readlist;
        struct qq_decls$unitrec *  readlistx;
        struct qq_decls$unitrec *  p;
    qq_parse$pushlisttype((i64)362090680912);
    flags = (i64)qq_decls$lx.subcode;
    qq_lex$lex();
    readlist = (readlistx = 0);
    pformat = (pdev = 0);
    if (((i64)qq_decls$lx.symbol == (i64)17)) {
        if ((opc == (i64)59)) {
            qq_lib$serror((byte*)"@ on read");
        }
;
        qq_lex$lex();
        pdev = qq_parse$readexpression();
        if (((i64)qq_decls$lx.symbol == (i64)3)) {
            qq_lex$lex();
        }
        else {
            goto L465 ;
;
        }
;
    }
;
    if (!(!!((i64)qq_tables$exprstarterset[((i64)qq_decls$lx.symbol)]))) {
        goto L465 ;
;
    }
;
    L466 :;
    while (1) {
        p = qq_parse$readexpression();
        if (((i64)qq_decls$lx.symbol == (i64)5)) {
            qq_lex$lex();
            p = qq_lib$createunit2((i64)58,p,qq_parse$readexpression());
        }
;
        qq_lib$addlistunit(&readlist,&readlistx,p);
        if (((i64)qq_decls$lx.symbol != (i64)3)) {
            goto L467 ;
        }
;
        qq_lex$lex();
    }
L467 :;
    ;
    //finish:
L465 :;
;
    if (((opc == (i64)59) && (readlist == 0))) {
        qq_lib$serror((byte*)"No read items");
    }
;
    qq_parse$poplisttype();
    p = qq_lib$createunit2((i64)59,pdev,readlist);
    (*p).flag = flags;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readloopcontrol(void) {
        i64 opc;
        struct qq_decls$unitrec *  p;
    opc = (i64)qq_decls$lx.subcode;
    qq_lex$lex();
    if ((((i64)qq_decls$lx.symbol == (i64)69) && !!(mlib$eqstring((*qq_decls$lx.symptr).name,(byte*)"all")))) {
        qq_lex$lex();
        p = qq_lib$createunit1(opc,qq_lib$createintunit((i64)0));
    }
    else if (!!((i64)qq_tables$exprstarterset[((i64)qq_decls$lx.symbol)])) {
        p = qq_lib$createunit1((i64)50,qq_parse$readintunit());
    }
    else {
        p = qq_lib$createunit1((i64)50,qq_lib$createintunit((i64)1));
    }
;
    (*p).loopcode = opc;
    return qq_parse$readcondsuffix(p);
}

static struct qq_decls$unitrec *qq_parse$readintunit(void) {
        struct qq_decls$unitrec *  p;
    p = qq_parse$readexpression();
    if (((i64)(*p).tag != (i64)41)) {
        qq_lib$serror((byte*)"int expr needed");
    }
;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readswitchcase(void) {
        i64 pos;
        i64 kwd;
        i64 opc;
        i64 rangeused;
        i64 nwhen;
        struct qq_decls$unitrec *  pexpr;
        struct qq_decls$unitrec *  pwhenlist;
        struct qq_decls$unitrec *  pwhenlistx;
        struct qq_decls$unitrec *  pwhen;
        struct qq_decls$unitrec *  pwhenx;
        struct qq_decls$unitrec *  pelse;
        struct qq_decls$unitrec *  p;
        struct qq_decls$unitrec *  pthen;
        struct qq_decls$unitrec *  pwhenthen;
    pos = (i64)qq_decls$lx.pos;
    kwd = (i64)qq_decls$lx.symbol;
    if ((kwd == (i64)81)) {
        opc = (!!((i64)qq_decls$lx.subcode) ? (i64)11 : (i64)10);
    }
    else {
        opc = (!!((i64)qq_decls$lx.subcode) ? (i64)13 : (i64)12);
    }
;
    qq_lex$lex();
    qq_lib$skipsemi();
    if (((i64)qq_decls$lx.symbol == (i64)83)) {
        if ((kwd == (i64)95)) {
            qq_lib$serror((byte*)"switch expr missing");
        }
;
        pexpr = qq_lib$createunit0((i64)0);
    }
    else {
        pexpr = qq_parse$readsunit((i64)0);
    }
;
    pwhenlist = (pwhenlistx = 0);
    rangeused = (i64)0;
    nwhen = (i64)0;
    qq_lib$skipsemi();
    L468 :;
    while (((i64)qq_decls$lx.symbol == (i64)83)) {
        pos = (i64)qq_decls$lx.pos;
        qq_lex$lex();
        pwhen = (pwhenx = 0);
        L471 :;
        while (1) {
            p = qq_parse$readexpression();
            ++(nwhen);
            (*p).pos = pos;
            if (((i64)(*p).tag == (i64)87)) {
                rangeused = (i64)1;
            }
;
            qq_lib$addlistunit(&pwhen,&pwhenx,p);
            if (((i64)qq_decls$lx.symbol != (i64)3)) {
                goto L472 ;
            }
;
            qq_lex$lex();
        }
L472 :;
        ;
        if (((i64)qq_decls$lx.symbol != (i64)73)) {
            qq_lib$checksymbol((i64)7);
        }
;
        qq_lex$lex();
        pthen = qq_parse$readsunit((i64)0);
        pwhenthen = qq_lib$createunit2((i64)9,pwhen,pthen);
        (*pwhenthen).pos = pos;
        qq_lib$addlistunit(&pwhenlist,&pwhenlistx,pwhenthen);
L469 :;
    }
L470 :;
    ;
        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)75)) {
        qq_lex$lex();
        pelse = qq_parse$readsunit((i64)0);
        qq_parse$checkend(kwd,(i64)0,(i64)0);
    }
    else if (($temp==(i64)74)) {
        qq_decls$lx.symbol = kwd;
        pelse = qq_parse$makeblock(qq_parse$readif());
    }
    else if (($temp==(i64)76) || ($temp==(i64)77)) {
        qq_decls$lx.symbol = kwd;
        pelse = qq_parse$readswitchcase();
    }
    else {
        pelse = 0;
        qq_parse$checkend(kwd,(i64)0,(i64)0);
    }
    };
    (*pexpr).nextunit = pelse;
    p = qq_lib$createunit2(opc,pexpr,pwhenlist);
    (*p).pos = pos;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readgoto(void) {
    qq_lex$lex();
    return qq_parse$readcondsuffix(qq_lib$createunit1((i64)30,qq_parse$readexpression()));
}

static struct qq_decls$unitrec *qq_parse$readstop(void) {
        struct qq_decls$unitrec *  p;
    qq_lex$lex();
    if (!!((i64)qq_tables$exprstarterset[((i64)qq_decls$lx.symbol)])) {
        p = qq_lib$createunit1((i64)31,qq_parse$readexpression());
    }
    else {
        p = qq_lib$createunit1((i64)31,qq_lib$createintunit((i64)0));
    }
;
    return qq_parse$readcondsuffix(p);
}

static struct qq_decls$unitrec *qq_parse$readcast(void) {
        struct qq_decls$unitrec *  p;
        i64 t;
        i64 opc;
        i64 pclop;
    t = (i64)qq_decls$lx.subcode;
    qq_lex$lex();
    if (((t == (i64)4) && ((i64)qq_decls$lx.symbol == (i64)9))) {
        qq_lex$lex();
        p = qq_parse$readexpression();
        if (((i64)(*p).tag == (i64)4 || (i64)(*p).tag == (i64)3)) {
            qq_lib$serror((byte*)"MAKERANGELEN");
        }
        else if (((i64)(*p).tag == (i64)87)) {
        }
        else {
            qq_lib$serror((byte*)"need a..b or a:n");
        }
;
        qq_lib$skipsymbol((i64)10);
        return p;
    }
;
        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)17) || ($temp==(i64)9)) {
    }
    else {
        p = qq_lib$createunit0((i64)34);
        (*p).mode = t;
        return p;
    }
    };
    if (((i64)qq_decls$lx.symbol == (i64)17)) {
        qq_lex$lex();
        opc = (i64)36;
        pclop = (i64)99;
    }
    else {
        opc = (i64)35;
        pclop = (i64)98;
    }
;
    qq_lib$checksymbol((i64)9);
    p = qq_parse$readterm();
    p = qq_lib$createunit1(opc,p);
    (*p).pclop = pclop;
    qq_lib$storemode(qq_decls$stcurrproc,t,&(*p).mode);
    return p;
}

static struct qq_decls$unitrec *qq_parse$readset(void) {
        i64 length;
        i64 nkeyvalues;
        struct qq_decls$unitrec *  p;
        struct qq_decls$unitrec *  ulist;
        struct qq_decls$unitrec *  ulistx;
    qq_lex$lex();
        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)12)) {
        qq_lex$lex();
        return qq_lib$createunit1((i64)89,0);
    }
    else if (($temp==(i64)5)) {
        qq_parse$lexchecksymbol((i64)12);
        qq_lex$lex();
        return qq_lib$createunit1((i64)90,0);
    }
    };
    qq_parse$pushlisttype((i64)1413695812);
    p = qq_parse$readexpression();
    length = (i64)1;
    nkeyvalues = (i64)0;
    if (((i64)(*p).tag == (i64)4)) {
        ++(nkeyvalues);
    }
;
    ulist = (ulistx = p);
    L473 :;
    while (((i64)qq_decls$lx.symbol == (i64)3)) {
        qq_lex$lex();
        if (((i64)qq_decls$lx.symbol == (i64)12)) {
            goto L475 ;
        }
;
        qq_lib$addlistunit(&ulist,&ulistx,(p = qq_parse$readexpression()));
        if (((i64)(*p).tag == (i64)4)) {
            ++(nkeyvalues);
        }
;
        ++(length);
        qq_lib$skipsemi();
L474 :;
    }
L475 :;
    ;
    qq_lib$skipsymbol((i64)12);
    if (!!(nkeyvalues)) {
        if ((length > nkeyvalues)) {
            qq_lib$serror((byte*)"dict: mixed elements");
        }
;
        p = qq_lib$createunit1((i64)90,ulist);
    }
    else {
        p = qq_lib$createunit1((i64)89,ulist);
    }
;
    (*p).length = length;
    qq_parse$poplisttype();
    return p;
}

void qq_parse$readtabledef(i64 isglobal) {
        i64 i;
        i64 ncols;
        i64 nrows;
        i64 enums;
        i64 nextenumvalue;
        i64 startline;
        i64 firstvalue;
        byte commas;
        byte semis;
        struct qq_decls$unitrec *  ulist;
        struct qq_decls$unitrec *  ulistx;
        struct qq_decls$unitrec *  p;
        struct qq_decls$strec *  varnames[20];
        struct qq_decls$unitrec *  plist[20];
        struct qq_decls$unitrec *  plistx[20];
        struct qq_decls$strec *  d;
    commas = (i64)0;
    semis = (i64)0;
    enums = (i64)qq_decls$lx.subcode;
    qq_lex$lex();
    firstvalue = (nextenumvalue = (i64)1);
    nrows = (i64)0;
    ncols = (i64)0;
    L476 :;
    while (((i64)qq_decls$lx.symbol == (i64)69)) {
        if ((++(ncols) > (i64)20)) {
            qq_lib$serror((byte*)"tabledata/too many columns");
        }
;
        varnames[(ncols)-1] = (struct qq_decls$strec *)qq_decls$lx.symptr;
        qq_lex$lex();
        if (((i64)qq_decls$lx.symbol == (i64)3)) {
            qq_lex$lex();
        }
        else {
            goto L478 ;
        }
;
L477 :;
    }
L478 :;
    ;
    qq_parse$checkequals();
    qq_lex$lex();
    qq_lib$skipsemi();
    startline = (i64)qq_decls$lx.pos;
    qq_lib$skipsemi();
    for (i=(i64)1;i<=ncols;++i) {
L479 :;
        plist[(i)-1] = (plistx[(i)-1] = 0);
L480 :;
    }
L481 :;
    ;
    ulist = (ulistx = 0);
    qq_parse$intabledata = (i64)1;
    L482 :;
    while (1) {
        qq_lib$skipsemi();
        if ((ncols > (i64)0)) {
            qq_lib$skipsymbol((i64)9);
        }
;
        if ((++(nrows) > (i64)500)) {
            qq_lib$serror((byte*)"tabledata:too many rows");
        }
;
        if (!!(enums)) {
            qq_lib$checksymbol((i64)69);
            d = qq_names$addsymbol(qq_decls$stcurrproc,(struct qq_decls$strec *)qq_decls$lx.symptr,(i64)19,isglobal);
            qq_lex$lex();
                        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)43)) {
                if ((nrows > (i64)1)) {
                    qq_lib$serror((byte*)"tabledata '=' not 1st");
                }
;
                qq_lex$lex();
                p = qq_parse$readexpression();
                if (((i64)(*p).tag == (i64)41)) {
                    firstvalue = (nextenumvalue = (*p).value);
                }
                else {
                    qq_lib$serror((byte*)"TABLEDATA: COMPLEX ENUM VAL");
                }
;
            }
            };
            (*d).index = (nextenumvalue)++;
            qq_parse$tabledataname = (*d).name;
            if (!!(ncols)) {
                qq_lib$skipsymbol((i64)3);
            }
;
        }
;
        for (i=(i64)1;i<=ncols;++i) {
L484 :;
            qq_lib$addlistunit(&plist[(i)-1],&plistx[(i)-1],qq_parse$readexpression());
            if ((i == ncols)) {
                qq_lib$skipsymbol((i64)10);
            }
            else {
                qq_lib$skipsymbol((i64)3);
            }
;
L485 :;
        }
L486 :;
        ;
                {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)3)) {
            ++(commas);
            qq_lex$lex();
            if (((i64)qq_decls$lx.symbol == (i64)79)) {
                goto L483 ;
            }
;
        }
        else {
            qq_lib$skipsemi();
            if (((i64)qq_decls$lx.symbol == (i64)79)) {
                goto L483 ;
            }
;
            ++(semis);
        }
        };
    }
L483 :;
    ;
    if ((!!((i64)semis) && !!((i64)commas))) {
        qq_lib$serror((byte*)"mixed commas");
    }
;
    qq_parse$intabledata = (i64)0;
    qq_lib$skipsemi();
    qq_parse$checkend((i64)124,(i64)0,startline);
    if ((nrows == (i64)0)) {
        qq_lib$serror((byte*)"No table data");
    }
;
    for (i=(i64)1;i<=ncols;++i) {
L487 :;
        d = qq_names$addsymbol(qq_decls$stcurrproc,varnames[(i)-1],(i64)13,isglobal);
        p = ((*d).code = qq_lib$createunit1((i64)88,plist[(i)-1]));
        (*p).length = nrows;
        (*p).lower = firstvalue;
L488 :;
    }
L489 :;
    ;
}

static struct qq_decls$unitrec *qq_parse$readtry(void) {
        struct qq_decls$unitrec *  ptry;
        struct qq_decls$unitrec *  pexceptlist;
        struct qq_decls$unitrec *  pexceptlistx;
        struct qq_decls$unitrec *  px;
        struct qq_decls$unitrec *  exlist;
        struct qq_decls$unitrec *  exlistx;
    qq_lex$lex();
    ptry = qq_parse$readsunit((i64)0);
    pexceptlist = (pexceptlistx = 0);
    L490 :;
    while (((i64)qq_decls$lx.symbol == (i64)118)) {
        qq_lex$lex();
        exlist = (exlistx = 0);
        L493 :;
        while (1) {
            qq_lib$addlistunit(&exlist,&exlistx,qq_parse$readexpression());
            if (((i64)qq_decls$lx.symbol != (i64)3)) {
                goto L494 ;
            }
;
            qq_lex$lex();
        }
L494 :;
        ;
        qq_lib$skipsymbol((i64)73);
        px = qq_parse$readsunit((i64)0);
        qq_lib$addlistunit(&pexceptlist,&pexceptlistx,qq_lib$createunit2((i64)24,exlist,px));
L491 :;
    }
L492 :;
    ;
    qq_parse$checkend((i64)117,(i64)0,(i64)0);
    return qq_lib$createunit2((i64)23,ptry,pexceptlist);
}

static struct qq_decls$unitrec *qq_parse$readsprint(void) {
        i64 opc;
        i64 flags;
        i64 isfprint;
        struct qq_decls$unitrec *  pformat;
        struct qq_decls$unitrec *  pdev;
        struct qq_decls$unitrec *  printlist;
        struct qq_decls$unitrec *  printlistx;
        struct qq_decls$unitrec *  p;
    qq_parse$pushlisttype((i64)362090680912);
    opc = (i64)54;
    flags = (i64)qq_decls$lx.subcode;
    qq_parse$lexchecksymbol((i64)9);
    qq_lex$lex();
    isfprint = (flags & (i64)2);
    printlist = (printlistx = 0);
    pformat = (pdev = 0);
    if (((i64)qq_decls$lx.symbol == (i64)17)) {
        qq_lex$lex();
        pdev = qq_parse$readexpression();
        if (((i64)qq_decls$lx.symbol == (i64)3)) {
            qq_lex$lex();
        }
        else {
            goto L495 ;
;
        }
;
    }
;
    if (!!(isfprint)) {
        pformat = qq_parse$readexpression();
        if (((i64)qq_decls$lx.symbol == (i64)3)) {
            qq_lex$lex();
        }
        else {
            goto L495 ;
;
        }
;
    }
;
    if (((i64)qq_decls$lx.symbol == (i64)10)) {
        goto L495 ;
;
    }
;
    L496 :;
    while (1) {
        if (((i64)qq_decls$lx.symbol == (i64)3)) {
            qq_lib$addlistunit(&printlist,&printlistx,qq_lib$createunit0((i64)56));
        }
        else {
            p = qq_parse$readexpression();
            if (((i64)qq_decls$lx.symbol == (i64)5)) {
                qq_lex$lex();
                p = qq_lib$createunit2((i64)58,p,qq_parse$readexpression());
            }
;
            qq_lib$addlistunit(&printlist,&printlistx,p);
        }
;
        if (((i64)qq_decls$lx.symbol != (i64)3)) {
            goto L497 ;
        }
;
        qq_lex$lex();
    }
L497 :;
    ;
    qq_lib$checksymbol((i64)10);
    //finish:
L495 :;
;
    qq_lex$lex();
    if ((((opc == (i64)54) || (opc == (i64)55)) && (printlist == 0))) {
        qq_lib$serror((byte*)"No print items");
    }
;
    qq_parse$poplisttype();
    if (!!(isfprint)) {
        if ((pformat == 0)) {
            qq_lib$serror((byte*)"No fmt str");
        }
;
        (*pformat).nextunit = printlist;
        p = qq_lib$createunit2(opc,pdev,pformat);
    }
    else {
        p = qq_lib$createunit2(opc,pdev,printlist);
    }
;
    (*p).flag = flags;
    return p;
}

static struct qq_decls$unitrec *qq_parse$readsread(void) {
        i64 opc;
        struct qq_decls$unitrec *  pformat;
        struct qq_decls$unitrec *  pdev;
        struct qq_decls$unitrec *  p;
        struct qq_decls$unitrec *  readlist;
        struct qq_decls$unitrec *  readlistx;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"SREAD",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    qq_parse$pushlisttype((i64)362090680912);
    opc = (i64)qq_decls$lx.subcode;
    qq_parse$lexchecksymbol((i64)9);
    qq_lex$lex();
    readlist = (readlistx = 0);
    pformat = (pdev = 0);
    if (((i64)qq_decls$lx.symbol == (i64)17)) {
        if ((opc == (i64)59)) {
            qq_lib$serror((byte*)"@ on read");
        }
;
        qq_lex$lex();
        pdev = qq_parse$readexpression();
        if (((i64)qq_decls$lx.symbol == (i64)3)) {
            qq_lex$lex();
        }
        else {
            goto L498 ;
;
        }
;
    }
;
    if (((i64)qq_decls$lx.symbol == (i64)10)) {
        goto L498 ;
;
    }
;
    L499 :;
    while (1) {
        p = qq_parse$readexpression();
        if (((i64)qq_decls$lx.symbol == (i64)5)) {
            qq_lex$lex();
            p = qq_lib$createunit2((i64)58,p,qq_parse$readexpression());
        }
;
        qq_lib$addlistunit(&readlist,&readlistx,p);
        if (((i64)qq_decls$lx.symbol != (i64)3)) {
            goto L500 ;
        }
;
        qq_lex$lex();
    }
L500 :;
    ;
    qq_lib$checksymbol((i64)10);
    //finish:
L498 :;
;
    qq_lex$lex();
    if (((opc == (i64)59) && (readlist == 0))) {
        qq_lib$serror((byte*)"No read items");
    }
;
    qq_parse$poplisttype();
    return qq_lib$createunit2(opc,pdev,readlist);
}

static void qq_parse$readimportdll(void) {
        u8 str[256];
        struct qq_decls$strec *  stproc;
        struct qq_decls$strec *  d;
        struct qq_decls$strec *  stname;
        i64 startpos;
        i64 isfunc;
        i64 isnew;
        i64 libtype;
        i64 i;
    libtype = (i64)qq_decls$lx.subcode;
    qq_parse$lexchecksymbol((i64)69);
    stname = (struct qq_decls$strec *)qq_decls$lx.symptr;
    qq_parse$lexchecksymbol((i64)43);
    qq_lex$lex();
    isnew = (i64)1;
    d = (*stname).nextdupl;
    for (i=(i64)1;i<=qq_decls$nlibfiles;++i) {
L501 :;
        if (!!(mlib$eqstring((*qq_decls$libtable[(i)-1]).name,(*stname).name))) {
            stname = qq_decls$libtable[(i)-1];
            isnew = (i64)0;
            goto L503 ;
        }
;
L502 :;
    }
L503 :;
    ;
    if (!!(isnew)) {
        stname = qq_names$addsymbol(0,stname,(i64)4,(i64)0);
        if ((qq_decls$nlibfiles >= (i64)50)) {
            qq_lib$serror((byte*)"Too many DLL libs");
        }
;
        qq_decls$libtable[(++(qq_decls$nlibfiles))-1] = stname;
        qq_decls$libtypes[(qq_decls$nlibfiles)-1] = libtype;
        (*stname).index = qq_decls$nlibfiles;
    }
;
    qq_parse$currdllindex = (i64)(*stname).index;
    startpos = (i64)qq_decls$lx.pos;
    L504 :;
    while (1) {
        qq_lib$skipsemi();
                {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)99) || ($temp==(i64)100)) {
            isfunc = (i64)((i64)qq_decls$lx.symbol == (i64)100);
            qq_lex$lex();
                        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)69)) {
                stproc = qq_names$addsymbol(qq_decls$stcurrproc,(struct qq_decls$strec *)qq_decls$lx.symptr,(i64)7,(i64)1);
            }
            else if (($temp==(i64)67)) {
                strcpy(str,qq_decls$lx.svalue);
                mlib$convlcstring(str);
                stproc = qq_names$addsymbol(qq_decls$stcurrproc,qq_names$addglobalname(str),(i64)7,(i64)1);
                (*stproc).truename = mlib$pcm_copyheapstring(qq_decls$lx.svalue);
            }
            else {
                qq_lib$serror((byte*)"fn name expected");
            }
            };
            (*stproc).flags = msysc$m_setdotindex((*stproc).flags,(i64)4,(u64)isfunc);
            (*stproc).flags = msysc$m_setdotindex((*stproc).flags,(i64)2,(u64)1u);
            if ((qq_decls$ndllprocs >= (i64)2000)) {
                qq_lib$serror((byte*)"Too many DLL procs");
            }
;
            qq_decls$dllproctable[(++(qq_decls$ndllprocs))-1] = stproc;
            qq_decls$dllproclibindex[(qq_decls$ndllprocs)-1] = qq_parse$currdllindex;
            (*stproc).index = qq_decls$ndllprocs;
            qq_lex$lex();
            if ((((i64)qq_decls$lx.symbol == (i64)69) && !!(mlib$eqstring((*qq_decls$lx.symptr).name,(byte*)"as")))) {
                qq_parse$lexchecksymbol((i64)69);
                d = qq_names$addsymbol((*stproc).owner,(struct qq_decls$strec *)qq_decls$lx.symptr,(i64)20,(i64)1);
                (*d).alias = stproc;
                qq_lex$lex();
            }
;
            qq_parse$readffiparams(stproc);
        }
        else if (($temp==(i64)108)) {
            qq_parse$readtypedef((i64)1);
        }
        else if (($temp==(i64)79)) {
            goto L505 ;
        }
        else {
            qq_parse$readpackvars(qq_decls$stcurrproc,(i64)8);
        }
        };
    }
L505 :;
    ;
    qq_parse$checkend((i64)107,(i64)0,startpos);
}

static void qq_parse$readffiparams(struct qq_decls$strec *stproc) {
        i64 pret;
        i64 ptype;
    if (((i64)qq_decls$lx.symbol == (i64)9)) {
        qq_lex$lex();
        if (((i64)qq_decls$lx.symbol == (i64)10)) {
            qq_lex$lex();
        }
        else {
            ptype = qq_parse$readtypespec((i64)0,0);
            if (((i64)qq_decls$lx.symbol == (i64)3 || (i64)qq_decls$lx.symbol == (i64)10)) {
                qq_parse$readtypeparams(stproc,ptype);
            }
            else {
                qq_parse$readtypenameparams(stproc,ptype);
            }
;
        }
;
    }
;
    if (((i64)qq_decls$lx.symbol == (i64)5 || (i64)qq_decls$lx.symbol == (i64)7)) {
        if (!(!!(msysc$m_getdotindex((i64)(*stproc).flags,(i64)4)))) {
            qq_lib$serror((byte*)"Return type for proc?");
        }
;
        qq_lex$lex();
    }
;
    pret = (i64)0;
    if (!!(msysc$m_getdotindex((i64)(*stproc).flags,(i64)4))) {
        if (((i64)qq_decls$lx.symbol == (i64)4)) {
            qq_lib$serror((byte*)"Return type missing");
        }
;
        pret = qq_parse$readtypespec((i64)0,0);
    }
;
    qq_lib$storemode((*stproc).owner,pret,&(*stproc).mode);
}

static void qq_parse$readtypeparams(struct qq_decls$strec *stproc,i64 ptype) {
        u8 str[32];
        i64 nparams;
        struct qq_decls$strec *  stname;
    nparams = (i64)0;
    L506 :;
    while (1) {
        ++(nparams);
        msysc$m_print_startstr(str);
        msysc$m_print_str((byte*)"$",NULL);
        msysc$m_print_nogap();
        msysc$m_print_i64(nparams,NULL);
        msysc$m_print_end();
        ;
        stname = qq_names$addsymbol(stproc,qq_names$addglobalname(str),(i64)16,(i64)0);
        qq_lib$storemode(stproc,ptype,&(*stname).mode);
        ++((*stproc).nparams);
        if (((i64)qq_decls$lx.symbol == (i64)3)) {
            qq_lex$lex();
            if (((i64)qq_decls$lx.symbol == (i64)21)) {
                (*stproc).flags = msysc$m_setdotindex((*stproc).flags,(i64)8,(u64)1u);
                qq_lex$lex();
                goto L507 ;
            }
;
            ptype = qq_parse$readtypespec((i64)0,0);
        }
        else {
            goto L507 ;
        }
;
    }
L507 :;
    ;
    qq_lib$skipsymbol((i64)10);
}

static void qq_parse$readtypenameparams(struct qq_decls$strec *stproc,i64 ptype) {
        struct qq_decls$strec *  stname;
    qq_lib$checksymbol((i64)69);
    stname = qq_names$addsymbol(stproc,(struct qq_decls$strec *)qq_decls$lx.symptr,(i64)16,(i64)0);
    qq_lib$storemode(stproc,ptype,&(*stname).mode);
    ++((*stproc).nparams);
    qq_lex$lex();
    L508 :;
    while (1) {
        if (((i64)qq_decls$lx.symbol == (i64)43)) {
            qq_lex$lex();
            (*stname).code = qq_parse$readexpression();
            (*stname).flags = msysc$m_setdotindex((*stname).flags,(i64)7,(u64)1u);
        }
;
                {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)3)) {
            qq_lex$lex();
            if (((i64)qq_decls$lx.symbol == (i64)21)) {
                (*stproc).flags = msysc$m_setdotindex((*stproc).flags,(i64)8,(u64)1u);
                qq_lex$lex();
                goto L509 ;
            }
;
            if (!!(qq_parse$istypestarter())) {
                ptype = qq_parse$readtypespec((i64)0,0);
            }
;
            qq_lib$checksymbol((i64)69);
            stname = qq_names$addsymbol(stproc,(struct qq_decls$strec *)qq_decls$lx.symptr,(i64)16,(i64)0);
            qq_lib$storemode(stproc,ptype,&(*stname).mode);
            ++((*stproc).nparams);
            qq_lex$lex();
        }
        else {
            goto L509 ;
        }
        };
    }
L509 :;
    ;
    qq_lib$skipsymbol((i64)10);
}

void qq_parse$readrecorddef(i64 isglobal,struct qq_decls$strec *d) {
        i64 kwd;
        i64 baseclass;
        i64 m;
        i64 startline;
        i64 caligned;
        byte lbopening;
        struct qq_decls$strec *  nameptr;
    lbopening = (i64)0;
    baseclass = (i64)0;
    if (!!(d)) {
        kwd = (i64)108;
        goto L510 ;
;
    }
;
    kwd = (i64)qq_decls$lx.symbol;
    qq_parse$lexchecksymbol((i64)69);
    nameptr = (struct qq_decls$strec *)qq_decls$lx.symptr;
    qq_lex$lex();
    if (((i64)qq_decls$lx.symbol == (i64)9)) {
        qq_lex$lex();
        baseclass = qq_parse$readtypespec((i64)0,0);
        qq_lib$skipsymbol((i64)10);
    }
;
    qq_parse$checkequals();
    qq_lex$lex();
    d = qq_names$addsymbol(qq_decls$stcurrproc,nameptr,((kwd == (i64)102) ? (i64)9 : (i64)10),isglobal);
    if (!!(baseclass)) {
        if ((baseclass > (i64)0)) {
            qq_lib$serror((byte*)"baseclass?");
        }
;
        if ((qq_decls$nbaseclasses >= (i64)255)) {
            qq_lib$serror((byte*)"Too many base classes");
        }
;
        ++(qq_decls$nbaseclasses);
        qq_lib$storemode(qq_decls$stcurrproc,baseclass,&qq_decls$baseclasstable[(qq_decls$nbaseclasses)]);
        (*d).baseclassindex = qq_decls$nbaseclasses;
        qq_decls$baseclassdef[(qq_decls$nbaseclasses)] = (struct qq_decls$strec *)d;
    }
;
    //gotname:
L510 :;
;
    qq_lib$skipsemi();
    startline = (i64)qq_decls$lx.pos;
    if (((i64)qq_decls$lx.symbol == (i64)9)) {
        lbopening = (i64)1;
        qq_lex$lex();
    }
;
    if ((kwd == (i64)102)) {
        m = qq_parse$readrecordbody(d);
    }
    else {
        caligned = (i64)0;
        m = qq_parse$readstructbody(d,caligned);
    }
;
    if (!!((i64)lbopening)) {
        qq_lib$checksymbol((i64)10);
        qq_lex$lex();
    }
    else {
        qq_parse$checkend((i64)102,(i64)0,startline);
    }
;
}

static i64 qq_parse$readrecordbody(struct qq_decls$strec *owner) {
        struct qq_decls$strec *  oldstcurrproc;
        struct qq_decls$strec *  e;
        i64 m;
        i64 nfields;
    m = qq_names$addanontype();
    oldstcurrproc = qq_decls$stcurrproc;
    qq_decls$stcurrproc = owner;
    L511 :;
        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)113)) {
        qq_parse$readconstdef((i64)0);
    }
    else if (($temp==(i64)110)) {
        qq_parse$readrecordfields(owner);
    }
    else if (($temp==(i64)100) || ($temp==(i64)99)) {
        qq_parse$readprocdef((i64)0);
    }
    else if (($temp==(i64)102)) {
        qq_parse$readrecorddef((i64)0,0);
    }
    else if (($temp==(i64)108)) {
        qq_lex$lex();
        qq_lib$serror((byte*)"CLASS TYPE");
    }
    else if (($temp==(i64)79) || ($temp==(i64)10) || ($temp==(i64)14)) {
        goto L512 ;
    }
    else if (($temp==(i64)62)) {
        qq_lib$serror((byte*)"Class eof?");
        goto L512 ;
    }
    else if (($temp==(i64)4)) {
        qq_lex$lex();
    }
    else {
        qq_lib$serror((byte*)"Unknown record field decl");
    }
    }goto L511 ;
L512 :;
    ;
    e = (*owner).deflist;
    nfields = (i64)0;
    L513 :;
    while (!!(e)) {
        if ((((i64)(*e).nameid == (i64)11) && !(!!((*e).atfield)))) {
            ++(nfields);
        }
;
L514 :;
        e = (*e).nextdef;
L516 :;
            }
L515 :;
    ;
    (*owner).nfields = nfields;
    qq_tables$ttfields[(m)] = (*owner).deflist;
    qq_tables$ttlength[(m)] = nfields;
    qq_tables$ttlower[(m)] = (i64)1;
    qq_tables$ttbasetype[(m)] = (i64)12;
    qq_names$createusertype(owner,m);
    e = (*owner).deflist;
    L517 :;
    while (!!(e)) {
        qq_names$addgenfield(e);
        e = (*e).nextdef;
L518 :;
    }
L519 :;
    ;
    qq_tables$ttsize[(m)] = ((i64)16 * (i64)(*owner).nfields);
    qq_decls$stcurrproc = oldstcurrproc;
    return m;
}

static void qq_parse$readrecordfields(struct qq_decls$strec *owner) {
        i64 nvars;
        i64 offset;
        i64 index;
        struct qq_decls$strec *  d;
    qq_lex$lex();
    nvars = (i64)0;
    index = (i64)(*owner).nfields;
    d = (*owner).deflist;
    offset = (i64)0;
    L520 :;
    while (!!(d)) {
        if ((((i64)(*d).nameid == (i64)11) && !(!!((*d).atfield)))) {
            offset += (i64)16;
        }
;
L521 :;
        d = (*d).nextdef;
L523 :;
            }
L522 :;
    ;
    L524 :;
    while (((i64)qq_decls$lx.symbol == (i64)69)) {
        ++(nvars);
        d = qq_names$addsymbol(qq_decls$stcurrproc,(struct qq_decls$strec *)qq_decls$lx.symptr,(i64)11,(i64)0);
        (*d).atfield = 0;
        qq_lex$lex();
        if (((i64)qq_decls$lx.symbol == (i64)17)) {
            qq_lex$lex();
            (*d).atfield = qq_parse$readatfield();
            (*d).fieldoffset = (i64)(*(*d).atfield).fieldoffset;
            (*d).index = (i64)(*(*d).atfield).index;
        }
        else {
            (*d).fieldoffset = offset;
            offset += (i64)16;
            (*d).index = ++(index);
        }
;
        if (((i64)qq_decls$lx.symbol != (i64)3)) {
            goto L526 ;
        }
;
        qq_lex$lex();
L525 :;
    }
L526 :;
    ;
    if ((nvars == (i64)0)) {
        qq_lib$serror((byte*)"No fields");
    }
;
}

static i64 qq_parse$readstructbody(struct qq_decls$strec *owner,i64 caligned) {
        i64 m;
        i64 ngroups;
        struct qq_decls$strec *  e;
    m = qq_names$addanontype();
    ngroups = (i64)0;
    L527 :;
    while (1) {
        qq_lib$skipsemi();
                {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)103)) {
            ++(ngroups);
            qq_lex$lex();
            qq_parse$addstructflag(owner,(i64)24);
        }
        else if (($temp==(i64)104)) {
            ++(ngroups);
            qq_lex$lex();
            qq_parse$addstructflag(owner,(i64)25);
        }
        else if (($temp==(i64)79)) {
            if (((i64)qq_decls$nextlx.symbol == (i64)103 || (i64)qq_decls$nextlx.symbol == (i64)104)) {
                qq_lex$lex();
            }
;
            //doend:
L529 :;
;
            if (!!(ngroups)) {
                --(ngroups);
                qq_lex$lex();
                qq_parse$addstructflag(owner,(i64)26);
            }
            else {
                goto L528 ;
            }
;
        }
        else if (($temp==(i64)10)) {
            goto L529 ;
;
        }
        else {
            qq_parse$readpackvars(owner,(i64)12);
        }
        };
    }
L528 :;
    ;
    qq_tables$ttfields[(m)] = (*owner).deflist;
    qq_tables$ttlength[(m)] = (i64)(*owner).nfields;
    qq_tables$ttlower[(m)] = (i64)1;
    qq_tables$ttcaligned[(m)] = caligned;
    qq_tables$ttbasetype[(m)] = (i64)13;
    qq_names$createusertype(owner,m);
    e = (*owner).deflist;
    L530 :;
    while (!!(e)) {
                {i64 $temp = (i64)(*e).nameid;
if (($temp==(i64)24) || ($temp==(i64)25) || ($temp==(i64)26)) {
        }
        else {
            qq_names$addgenfield(e);
        }
        };
        e = (*e).nextdef;
L531 :;
    }
L532 :;
    ;
    return m;
}

static void qq_parse$addstructflag(struct qq_decls$strec *owner,i64 id) {
        static i64 structseqno;
        u8 str[32];
    msysc$m_print_startstr(str);
    msysc$m_print_setfmt((byte*)"$$#");
    msysc$m_print_i64(++(structseqno),NULL);
    msysc$m_print_end();
    ;
    qq_names$addsymbol(owner,qq_names$addglobalname(str),id,(i64)0);
}

static void qq_parse$readprocdef(i64 isglobal) {
        i64 kwd;
        i64 startline;
        i64 shortfun;
        struct qq_decls$strec *  d;
        struct qq_decls$strec *  oldstcurrproc;
    kwd = (i64)qq_decls$lx.symbol;
    shortfun = (i64)qq_decls$lx.subcode;
    qq_parse$lexchecksymbol((i64)69);
    if (((i64)(*qq_decls$stcurrproc).nameid == (i64)5 || (i64)(*qq_decls$stcurrproc).nameid == (i64)6)) {
        qq_lib$serror((byte*)"Nested proc");
    }
;
    oldstcurrproc = qq_decls$stcurrproc;
    qq_decls$stcurrproc = (d = qq_names$addsymbol(qq_decls$stcurrproc,(struct qq_decls$strec *)qq_decls$lx.symptr,(i64)5,isglobal));
    qq_names$addproc(d);
    qq_lex$lex();
    (*d).mode = (i64)0;
    if (((i64)qq_decls$lx.symbol == (i64)9)) {
        qq_lex$lex();
        if (((i64)qq_decls$lx.symbol != (i64)10)) {
            qq_parse$readparams(d);
        }
        else {
            qq_lex$lex();
        }
;
    }
;
    qq_parse$checkequals();
    qq_lex$lex();
    startline = (i64)qq_decls$lx.pos;
    if (!(!!(shortfun))) {
        (*d).code = qq_parse$readsunit((i64)0);
        qq_parse$checkend(kwd,(i64)0,startline);
    }
    else {
        (*d).code = qq_parse$readexpression();
        qq_lib$checksymbol((i64)4);
    }
;
    if (!!(mlib$eqstring((*d).name,(byte*)"start"))) {
        (*qq_decls$currmodule).startfn = d;
    }
    else if (!!(mlib$eqstring((*d).name,(byte*)"main"))) {
        (*qq_decls$currmodule).mainfn = d;
    }
;
    (*qq_decls$stcurrproc).flags = msysc$m_setdotindex((*qq_decls$stcurrproc).flags,(i64)4,(u64)(kwd == (i64)100));
    qq_decls$stcurrproc = oldstcurrproc;
}

static struct qq_decls$strec *qq_parse$readatfield(void) {
        struct qq_decls$strec *  p;
        struct qq_decls$strec *  d;
    qq_lib$checksymbol((i64)69);
    d = (struct qq_decls$strec *)qq_decls$lx.symptr;
    qq_lex$lex();
    p = (*qq_decls$stcurrproc).deflist;
    L533 :;
    while (!!(p)) {
        if (!!(mlib$eqstring((*p).name,(*d).name))) {
            return p;
        }
;
        p = (*p).nextdef;
L534 :;
    }
L535 :;
    ;
    qq_lib$serror_s((byte*)"Can't find @ field",(*d).name);
    return (struct qq_decls$strec *)0;
}

static i64 qq_parse$istypestarter(void) {
        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)70) || ($temp==(i64)109) || ($temp==(i64)110) || ($temp==(i64)11)) {
        return (i64)1;
    }
    else {
        if (((i64)qq_decls$lx.symbol == (i64)69)) {
            if (((i64)qq_decls$nextlx.symbol == (i64)69)) {
                return (i64)1;
            }
;
        }
;
    }
    };
    return (i64)0;
}

static void qq_parse$readmacrodef(i64 isglobal) {
        struct qq_decls$strec *  stmacro;
        struct qq_decls$strec *  stname;
        struct qq_decls$strec *  owner;
    qq_parse$lexchecksymbol((i64)69);
    stmacro = qq_names$addsymbol(qq_decls$stcurrproc,(struct qq_decls$strec *)qq_decls$lx.symptr,(i64)22,isglobal);
    owner = stmacro;
    qq_lex$lex();
    if (((i64)qq_decls$lx.symbol == (i64)9)) {
        qq_lex$lex();
        if (((i64)qq_decls$lx.symbol != (i64)10)) {
            L536 :;
            while (1) {
                                {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)69)) {
                    stname = qq_names$addsymbol(owner,(struct qq_decls$strec *)qq_decls$lx.symptr,(i64)23,(i64)0);
                    (*stname).firstdupl = (struct qq_decls$strec *)qq_decls$lx.symptr;
                    qq_lex$lex();
                    if (((i64)qq_decls$lx.symbol == (i64)10)) {
                        goto L537 ;
                    }
;
                    qq_lib$skipsymbol((i64)3);
                }
                else {
                    qq_lib$serror((byte*)"macro def params");
                }
                };
            }
L537 :;
            ;
        }
;
        qq_lex$lex();
    }
;
    qq_parse$checkequals();
    qq_lex$lex();
    (*stmacro).code = qq_parse$readexpression();
}

static struct qq_decls$unitrec *qq_parse$readhostparams(struct qq_decls$unitrec *lhs,i64 isfn) {
        i64 fnindex;
        i64 nargs;
        struct qq_decls$unitrec *  p;
        struct qq_decls$unitrec *  q;
    fnindex = (i64)qq_decls$lx.subcode;
    qq_parse$lexchecksymbol((i64)9);
    qq_lex$lex();
    q = qq_parse$readslist(&nargs,(i64)0);
    qq_lib$skipsymbol((i64)10);
    if (!!(lhs)) {
        (*lhs).nextunit = q;
        q = lhs;
    }
;
    p = qq_lib$createunit1((i64)27,q);
    (*p).index = fnindex;
    return p;
}

static void qq_parse$pushlisttype(i64 ltype) {
    if ((qq_parse$nlisttype >= (i64)20)) {
        qq_lib$serror((byte*)"listtype overflow");
    }
;
    qq_parse$listtypestack[(++(qq_parse$nlisttype))-1] = qq_parse$listtype;
    qq_parse$listtype = ltype;
}

static void qq_parse$poplisttype(void) {
    qq_parse$listtype = qq_parse$listtypestack[((qq_parse$nlisttype)--)-1];
}

static struct qq_decls$unitrec *qq_parse$readcompilervar(void) {
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
        {i64 $temp = (i64)qq_decls$lx.subcode;
if (($temp==(i64)1)) {
        return qq_lib$createintunit((i64)msysc$m_getdotslice((i64)qq_decls$lx.pos,(i64)0,(i64)23));
    }
    else if (($temp==(i64)2)) {
        strcpy(str,msysc$strint((i64)msysc$m_getdotslice((i64)qq_decls$lx.pos,(i64)0,(i64)23),0));
    }
    else if (($temp==(i64)4)) {
        strcpy((u8 *)str,(*qq_decls$currmodule).name);
    }
    else if (($temp==(i64)3)) {
        strcpy((u8 *)str,(*qq_decls$currmodule).filespec);
    }
    else if (($temp==(i64)5)) {
        strcpy((u8 *)str,(!!(qq_decls$stcurrproc) ? (*qq_decls$stcurrproc).name : (byte*)"<none>"));
    }
    else if (($temp==(i64)6)) {
        mlinux$os_getsystime(&tm);
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"#-#-#");
        msysc$m_print_i64((i64)tm.day,NULL);
        msysc$m_print_str(monthnames[((i64)tm.month)-1],NULL);
        msysc$m_print_i64((i64)tm.year,(byte*)"4");
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)7)) {
        mlinux$os_getsystime(&tm);
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"#:#:#");
        msysc$m_print_i64((i64)tm.hour,(byte*)"2");
        msysc$m_print_i64((i64)tm.minute,(byte*)"z2");
        msysc$m_print_i64((i64)tm.second,(byte*)"z2");
        msysc$m_print_end();
        ;
    }
    else {
        qq_lib$serror((byte*)"compiler var not impl");
    }
    };
    return qq_lib$createstringunit(mlib$pcm_copyheapstring((u8 *)str),(i64)-1);
}

static struct qq_decls$unitrec *qq_parse$readpair(i64 tag,i64 pclop) {
        struct qq_decls$unitrec *  p;
        struct qq_decls$unitrec *  a;
        struct qq_decls$unitrec *  b;
    qq_parse$lexchecksymbol((i64)9);
    qq_lex$lex();
    a = (struct qq_decls$unitrec *)qq_parse$readexpression();
    qq_lib$skipsymbol((i64)3);
    b = (struct qq_decls$unitrec *)qq_parse$readexpression();
    if ((((i64)qq_decls$lx.symbol == (i64)3) && (tag == (i64)37))) {
        qq_lex$lex();
        (*b).nextunit = qq_parse$readexpression();
    }
;
    qq_lib$skipsymbol((i64)10);
    p = qq_lib$createunit2(tag,(struct qq_decls$unitrec *)a,(struct qq_decls$unitrec *)b);
    (*p).pclop = pclop;
    return p;
}

void qq_parse$lexchecksymbol(i64 symbol) {
    qq_lex$lex();
    qq_lib$checksymbol(symbol);
}

static void qq_parse$readtypedef(i64 isglobal) {
        i64 ptype;
        struct qq_decls$strec *  d;
    qq_parse$lexchecksymbol((i64)69);
    d = qq_names$addsymbol(qq_decls$stcurrproc,(struct qq_decls$strec *)qq_decls$lx.symptr,(i64)10,isglobal);
    qq_parse$lexchecksymbol((i64)43);
    qq_lex$lex();
    if (((i64)qq_decls$lx.symbol == (i64)102)) {
        qq_lex$lex();
        (*d).nameid = (i64)9;
        qq_parse$readrecorddef(isglobal,d);
        return;
    }
;
    ptype = qq_parse$readtypespec((i64)0,d);
    qq_names$createusertype(d,ptype);
}

static i64 qq_parse$readtypespec(i64 allowvar,struct qq_decls$strec *owner) {
        i64 t;
        i64 startline;
        i64 caligned;
        struct qq_decls$strec *  d;
        struct qq_decls$unitrec *  lowerdims[10];
        struct qq_decls$unitrec *  lengthdims[10];
        i64 ndims;
        struct qq_decls$unitrec *  x;
        struct qq_decls$unitrec *  lowerx;
        struct qq_decls$unitrec *  upperx;
        struct qq_decls$unitrec *  lengthx;
        i64 i;
        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)11)) {
        //dolsq:
L538 :;
;
        qq_lex$lex();
        ndims = (i64)0;
        qq_parse$pushlisttype((i64)0);
        L539 :;
        while (1) {
            lowerx = (lengthx = 0);
            if ((((i64)qq_decls$lx.symbol == (i64)12) || ((i64)qq_decls$lx.symbol == (i64)3))) {
            }
            else {
                x = qq_parse$readexpression();
                if (((i64)(*x).tag == (i64)87)) {
                    lowerx = (*x).a;
                    upperx = (*x).b;
                    if ((((i64)(*lowerx).tag == (i64)41) && ((i64)(*upperx).tag == (i64)41))) {
                        lengthx = qq_lib$createintunit((((*upperx).value - (*lowerx).value) + (i64)1));
                    }
                    else {
                        lengthx = qq_lib$createunit2((i64)63,upperx,lowerx);
                        (*lengthx).pclop = (i64)101;
                        lengthx = qq_lib$createunit2((i64)63,lengthx,qq_lib$createintunit((i64)1));
                        (*lengthx).pclop = (i64)100;
                    }
;
                }
                else {
                                        {i64 $temp = (i64)qq_decls$lx.symbol;
if (($temp==(i64)12) || ($temp==(i64)3)) {
                        lengthx = x;
                    }
                    else if (($temp==(i64)5)) {
                        lowerx = x;
                        qq_lex$lex();
                        if (!((((i64)qq_decls$lx.symbol == (i64)3) || ((i64)qq_decls$lx.symbol == (i64)12)))) {
                            lengthx = qq_parse$readexpression();
                        }
;
                    }
                    };
                }
;
            }
;
            lowerdims[(++(ndims))-1] = lowerx;
            lengthdims[(ndims)-1] = lengthx;
            if (((i64)qq_decls$lx.symbol != (i64)3)) {
                goto L540 ;
            }
;
            qq_lex$lex();
        }
L540 :;
        ;
        qq_lib$skipsymbol((i64)12);
        qq_parse$poplisttype();
        t = qq_parse$readtypespec((i64)0,0);
        for (i=ndims;i>=(i64)1;--i) {
L541 :;
            t = qq_names$makeaxtype(t,lowerdims[(i)-1],lengthdims[(i)-1]);
L542 :;
        }
L543 :;
        ;
        return t;
    }
    else if (($temp==(i64)109)) {
        qq_lex$lex();
        if ((((i64)qq_decls$lx.symbol == (i64)70) && ((i64)qq_decls$lx.subcode == (i64)0))) {
            qq_lex$lex();
            return qq_names$makereftype((i64)0,owner);
        }
        else {
            return qq_names$makereftype(qq_parse$readtypespec((i64)0,0),owner);
        }
;
    }
    else if (($temp==(i64)69)) {
        d = (struct qq_decls$strec *)qq_decls$lx.symptr;
        qq_lex$lex();
        if (((i64)qq_decls$lx.symbol == (i64)2)) {
            qq_parse$lexchecksymbol((i64)69);
            t = qq_names$newusertypex((struct qq_decls$strec *)d,(struct qq_decls$strec *)qq_decls$lx.symptr);
            qq_lex$lex();
            return t;
        }
        else {
            return qq_names$newusertypex((struct qq_decls$strec *)d,0);
        }
;
    }
    else if (($temp==(i64)70)) {
                {i64 $temp = (i64)qq_decls$lx.subcode;
if (($temp==(i64)37)) {
            qq_lex$lex();
            if (((i64)qq_decls$lx.symbol == (i64)24)) {
                qq_lex$lex();
                return qq_names$makestrtype((i64)37,qq_parse$readexpression());
            }
            else {
                return (i64)38;
            }
;
        }
        else if (($temp==(i64)36)) {
            qq_parse$lexchecksymbol((i64)24);
            qq_lex$lex();
            return qq_names$makestrtype((i64)36,qq_parse$readexpression());
        }
        else if (($temp==(i64)11)) {
            qq_parse$lexchecksymbol((i64)11);
            goto L538 ;
;
        }
        else {
            t = (i64)qq_decls$lx.subcode;
            if ((t==(i64)1)) {
                t = (i64)26;
            }
            else if ((t==(i64)2)) {
                t = (i64)32;
            }
;
            qq_lex$lex();
            return t;
        }
        };
    }
    else if (($temp==(i64)102)) {
        if ((owner == 0)) {
            qq_lib$serror((byte*)"anon record");
        }
;
        qq_lex$lex();
        startline = (i64)qq_decls$lx.pos;
        t = qq_parse$readrecordbody(owner);
        qq_parse$checkend((i64)102,(i64)0,startline);
        return t;
    }
    else if (($temp==(i64)103)) {
        if ((owner == 0)) {
            qq_lib$serror((byte*)"anon struct");
        }
;
        qq_lex$lex();
        caligned = (i64)0;
        if (((i64)qq_decls$lx.symbol == (i64)116)) {
            caligned = (i64)1;
            qq_lex$lex();
        }
;
        startline = (i64)qq_decls$lx.pos;
        t = qq_parse$readstructbody(owner,caligned);
        qq_parse$checkend((i64)103,(i64)0,startline);
        return t;
    }
    else {
        qq_lib$serror((byte*)"Type expected");
    }
    };
    return t;
}

static void qq_parse$readparams(struct qq_decls$strec *stproc) {
        i64 isbyref;
        i64 isoptional;
        struct qq_decls$strec *  d;
    isbyref = (isoptional = (i64)0);
    L544 :;
    while (1) {
        if (((i64)qq_decls$lx.symbol == (i64)19)) {
            ++(isbyref);
            qq_lex$lex();
        }
;
        if (((i64)qq_decls$lx.symbol == (i64)18)) {
            ++(isoptional);
            qq_lex$lex();
        }
;
        qq_lib$checksymbol((i64)69);
        d = qq_names$addsymbol(stproc,(struct qq_decls$strec *)qq_decls$lx.symptr,(i64)15,(i64)0);
        ++((*stproc).nparams);
        qq_lex$lex();
        if (((i64)qq_decls$lx.symbol == (i64)43)) {
            isoptional = (i64)1;
            qq_lex$lex();
            (*d).code = qq_parse$readexpression();
        }
;
        if ((!!(isbyref) && !!(isoptional))) {
            qq_lib$serror((byte*)"Mixed byref/optional");
        }
;
        (*d).flags = msysc$m_setdotindex((*d).flags,(i64)5,(u64)isbyref);
        (*d).flags = msysc$m_setdotindex((*d).flags,(i64)7,(u64)isoptional);
        isbyref = (isoptional = (i64)0);
        if (((i64)qq_decls$lx.symbol == (i64)3)) {
            qq_lex$lex();
            if (((i64)qq_decls$lx.symbol == (i64)21)) {
                (*stproc).flags = msysc$m_setdotindex((*stproc).flags,(i64)8,(u64)1u);
                qq_lex$lex();
                goto L545 ;
            }
;
        }
        else {
            goto L545 ;
        }
;
    }
L545 :;
    ;
    qq_lib$skipsymbol((i64)10);
}

static struct qq_decls$unitrec *qq_parse$checkoperator(void) {
        struct qq_decls$unitrec *  p;
        i64 opc;
    if (((i64)qq_decls$nextlx.symbol == (i64)3 || (i64)qq_decls$nextlx.symbol == (i64)10 || (i64)qq_decls$nextlx.symbol == (i64)4)) {
        p = qq_lib$createunit0((i64)5);
        if (((i64)qq_decls$lx.symbol == (i64)132)) {
                        {i64 $temp = (i64)qq_decls$lx.subcode;
if (($temp==(i64)45)) {
                opc = (i64)74;
            }
            else if (($temp==(i64)23899)) {
                opc = (i64)131;
            }
            else {
                opc = (i64)0;
            }
            };
            (*p).pclop = opc;
        }
        else if (!!((i64)qq_tables$cmpopset[((i64)qq_decls$lx.symbol)])) {
            qq_lib$serror((byte*)"(CMP OP) NOT READY");
        }
        else {
            (*p).pclop = (i64)qq_decls$lx.subcode;
        }
;
        qq_lex$lex();
        return p;
    }
;
    return (struct qq_decls$unitrec *)0;
}

static struct qq_decls$unitrec *qq_parse$readlambda(void) {
        struct qq_decls$unitrec *  p;
        struct qq_decls$strec *  params[100];
        struct qq_decls$strec *  oldstcurrproc;
        struct qq_decls$strec *  stproc;
        struct qq_decls$strec *  d;
        u8 str[20];
        i64 nparams;
        byte byref;
        {i64 $temp = (i64)(*qq_decls$stcurrproc).nameid;
if (($temp==(i64)5)) {
    }
    else if (($temp==(i64)6)) {
        qq_lib$serror((byte*)"Nested {}");
    }
    else {
        qq_lib$serror((byte*)"{} not in fn");
    }
    };
    oldstcurrproc = qq_decls$stcurrproc;
    msysc$m_print_startstr(str);
    msysc$m_print_str((byte*)"$F",NULL);
    msysc$m_print_nogap();
    msysc$m_print_i64(++(qq_parse$nextlambdaindex),NULL);
    msysc$m_print_end();
    ;
    stproc = qq_names$addsymbol(qq_decls$stcurrproc,qq_lex$addnamestr(str),(i64)6,(i64)0);
    qq_decls$stcurrproc = stproc;
    qq_names$addproc(stproc);
    qq_lex$lex();
    nparams = (i64)0;
    byref = (i64)0;
    if (((i64)qq_decls$lx.symbol == (i64)19)) {
        qq_lex$lex();
        byref = (i64)1;
    }
;
    if ((((i64)qq_decls$lx.symbol == (i64)69) && ((i64)qq_decls$nextlx.symbol == (i64)3 || (i64)qq_decls$nextlx.symbol == (i64)5))) {
        L546 :;
        while (1) {
            qq_lib$checksymbol((i64)69);
            d = qq_names$addsymbol(stproc,(struct qq_decls$strec *)qq_decls$lx.symptr,(i64)15,(i64)0);
            params[(++(nparams))-1] = d;
            (*d).flags = msysc$m_setdotindex((*d).flags,(i64)5,(u64)(i64)byref);
            byref = (i64)0;
            qq_lex$lex();
            if (((i64)qq_decls$lx.symbol != (i64)3)) {
                goto L547 ;
            }
;
            qq_lex$lex();
        }
L547 :;
        ;
        qq_lib$checksymbol((i64)5);
        qq_lex$lex();
    }
;
    (*stproc).nparams = nparams;
    (*stproc).flags = msysc$m_setdotindex((*stproc).flags,(i64)4,(u64)1u);
    (*stproc).code = qq_parse$readsunit((i64)0);
    qq_lib$skipsymbol((i64)14);
    p = (struct qq_decls$unitrec *)qq_lib$createname((struct qq_decls$strec *)stproc);
    qq_decls$stcurrproc = oldstcurrproc;
    return p;
}

static void qq_parse$readpackvars(struct qq_decls$strec *owner,i64 id) {
        i64 t;
        i64 nvars;
        struct qq_decls$strec *  d;
    t = qq_parse$readtypespec((i64)0,0);
    nvars = (i64)0;
    L548 :;
    while (((i64)qq_decls$lx.symbol == (i64)69)) {
        ++(nvars);
        d = qq_names$addsymbol(owner,(struct qq_decls$strec *)qq_decls$lx.symptr,id,(i64)0);
        qq_lib$storemode(owner,t,&(*d).mode);
        qq_lex$lex();
        if (((i64)qq_decls$lx.symbol != (i64)3)) {
            goto L550 ;
        }
;
        qq_parse$lexchecksymbol((i64)69);
L549 :;
    }
L550 :;
    ;
    if ((nvars == (i64)0)) {
        qq_lib$serror((byte*)"bad decl?");
    }
;
}

// START
void qq_parse$start(void) {

}

// START
void qq_pcltabs$start(void) {

}

void qq_pclgen$evalunit(struct qq_decls$unitrec *p,i64 res) {
        struct qq_decls$unitrec *  a;
        struct qq_decls$unitrec *  b;
        struct qq_decls$strec *  d;
        i64 procflag;
        i64 index;
        i64 i;
    qq_decls$qpos = (i64)(*p).pos;
    a = (*p).a;
    b = (*p).b;
    switch ((i64)(*p).tag) {
    case 41:;
        {
            qq_pcllib$genpc_int((i64)13,(*p).value);
        }
        break;
    case 42:;
        {
            qq_pcllib$genpc_real((i64)16,(*p).xvalue);
        }
        break;
    case 0:;
        {
        }
        break;
    case 43:;
        {
            qq_pclgen$pushstring((*p).svalue);
        }
        break;
    case 39:;
        {
            d = (*p).def;
                        {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)15)) {
                qq_pcllib$genpc_name((i64)7,(struct qq_decls$strec *)d);
                if (!!(msysc$m_getdotindex((i64)(*d).flags,(i64)5))) {
                    qq_pcllib$genpc((i64)21);
                }
;
            }
            else if (($temp==(i64)14) || ($temp==(i64)13)) {
                qq_pcllib$genpc_name((i64)7,(struct qq_decls$strec *)d);
            }
            else if (($temp==(i64)17)) {
                if (!(!!(res))) {
                    if (((*d).labelno == (i64)0)) {
                        (*d).labelno = qq_pcllib$createfwdlabel();
                    }
;
                    qq_pcllib$genpc_lab((i64)29,(*d).labelno);
                    return;
                }
                else {
                    qq_pcllib$genpc_name((i64)20,(struct qq_decls$strec *)d);
                }
;
            }
            else {
                qq_pcllib$genpc_name((i64)20,(struct qq_decls$strec *)d);
            }
            };
        }
        break;
    case 40:;
        {
            if (((i64)(*a).tag == (i64)39)) {
                qq_pcllib$genpc_name((i64)20,(struct qq_decls$strec *)(*a).def);
            }
            else {
                qq_lib$gerror((byte*)".$ name expected",0);
            }
;
        }
        break;
    case 6:;
        {
            if (!!(a)) {
                L551 :;
                while ((!!(a) && !!((*a).nextunit))) {
                    qq_pclgen$evalunit(a,(i64)0);
                    a = (*a).nextunit;
L552 :;
                }
L553 :;
                ;
                if (!!(a)) {
                    qq_pclgen$evalunit(a,res);
                }
;
            }
            else {
            }
;
        }
        break;
    case 44:;
        {
            qq_pclgen$pushstring((*p).svalue);
            qq_pcllib$genpc((i64)68);
        }
        break;
    case 26:;
        {
            qq_pclgen$do_call(p,a,b,res,&procflag);
        }
        break;
    case 32:;
        {
            qq_pclgen$do_return(p,a);
        }
        break;
    case 27:;
        {
            qq_pclgen$do_callhost(p,a,res);
        }
        break;
    case 2:;
        {
            qq_pclgen$do_assign(a,b,res,(i64)(*p).flag);
        }
        break;
    case 20:;
        {
            qq_pclgen$do_to(p,a,b);
        }
        break;
    case 7:;
        {
            qq_pclgen$do_if(p,a,b,(*b).nextunit,res);
        }
        break;
    case 15:;
        {
            qq_pclgen$do_for(p,a,b);
        }
        break;
    case 16:;
        {
            qq_pclgen$do_forx(p,a,b);
        }
        break;
    case 17:;
    case 18:;
        {
            qq_pclgen$do_forall(p,a,b);
        }
        break;
    case 21:;
        {
            qq_pclgen$do_while(p,a,b);
        }
        break;
    case 22:;
        {
            qq_pclgen$do_repeat(p,a,b);
        }
        break;
    case 30:;
        {
            if ((((i64)(*a).tag == (i64)39) && ((i64)(*(*a).def).nameid == (i64)17))) {
                d = (*a).def;
                if (((*d).labelno == (i64)0)) {
                    (*d).labelno = qq_pcllib$createfwdlabel();
                }
;
                qq_pcllib$genpc_lab((i64)29,(*d).labelno);
            }
            else {
                qq_pclgen$evalunit(a,(i64)1);
                qq_pcllib$genpc((i64)30);
            }
;
        }
        break;
    case 1:;
        {
            d = (*a).def;
            qq_pcllib$gencomment((*d).name);
            if (((*d).labelno == (i64)0)) {
                (*d).labelno = qq_pcllib$definelabel();
            }
            else {
                index = (*d).labelno;
                qq_pcllib$definefwdlabel(index);
            }
;
        }
        break;
    case 50:;
        {
            qq_pclgen$do_loop(p);
        }
        break;
    case 19:;
        {
            qq_pclgen$do_do(p,a);
        }
        break;
    case 10:;
    case 11:;
        {
            qq_pclgen$do_case(p,a,b,res);
        }
        break;
    case 12:;
    case 13:;
        {
            qq_pclgen$do_switch(p,a,b,res);
        }
        break;
    case 29:;
        {
            qq_pclgen$evalref(a);
            qq_pclgen$evalref(b);
            qq_pcllib$genpc((i64)27);
        }
        break;
    case 8:;
        {
            qq_pclgen$do_select(a,b,res);
        }
        break;
    case 54:;
        {
            qq_pclgen$do_print(p,a,b);
        }
        break;
    case 55:;
        {
            qq_pclgen$do_fprint(p,a,b,(*b).nextunit);
        }
        break;
    case 59:;
        {
            qq_pclgen$do_read(p,a,b);
        }
        break;
    case 31:;
        {
            if (!!(a)) {
                qq_pclgen$evalunit(a,(i64)1);
            }
            else {
                qq_pcllib$genpc_int((i64)13,(i64)0);
            }
;
            qq_pcllib$genpc((i64)59);
        }
        break;
    case 23:;
        {
            qq_pclgen$do_try(p,a,b);
        }
        break;
    case 72:;
        {
            qq_pclgen$do_andl(a,b);
        }
        break;
    case 73:;
        {
            qq_pclgen$do_orl(a,b);
        }
        break;
    case 88:;
        {
            qq_pclgen$do_pushlist(a,(i64)(*p).length);
            qq_pcllib$genpc_xy((i64)60,(i64)(*p).length,(i64)(*p).lower);
        }
        break;
    case 89:;
        {
            qq_pclgen$do_pushlist(a,(i64)(*p).length);
            qq_pcllib$genpc_xy((i64)65,(i64)(*p).length,(i64)0);
        }
        break;
    case 90:;
        {
            qq_pclgen$do_makedict(a,(i64)(*p).length);
        }
        break;
    case 87:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pclgen$evalunit(b,(i64)1);
            qq_pcllib$genpc((i64)66);
        }
        break;
    case 4:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pclgen$evalunit(b,(i64)1);
        }
        break;
    case 37:;
        {
            qq_pclgen$do_map(p,a,b);
        }
        break;
    case 63:;
        {
                        {i64 $temp = (i64)(*p).pclop;
if (($temp==(i64)104)) {
                qq_pclgen$do_idiv(a,b);
            }
            else if (($temp==(i64)105)) {
                qq_pclgen$do_irem(a,b);
            }
            else {
                qq_pclgen$evalunit(a,(i64)1);
                qq_pclgen$evalunit(b,(i64)1);
                qq_pcllib$genpc((i64)(*p).pclop);
            }
            };
        }
        break;
    case 79:;
        {
            qq_pclgen$evalref(a);
            qq_pclgen$evalunit(b,(i64)1);
            qq_pcllib$genpc((i64)121);
            for (i=(i64)1;i<=(i64)11;++i) {
L554 :;
                if ((qq_pcltabs$bintotable[(i)-1].pclop == (i64)(*p).pclop)) {
                    (*qq_pcllib$pccurr).bintoindex = i;
                    goto L556 ;
                }
;
L555 :;
            }
            {
                qq_lib$gerror((byte*)"No binto entry",0);
            }
L556 :;
            ;
        }
        break;
    case 62:;
    case 66:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pcllib$genpc((i64)(*p).pclop);
            if (((i64)(*p).pclop == (i64)89)) {
                (*qq_pcllib$pccurr).n = (i64)1;
            }
;
        }
        break;
    case 78:;
        {
            qq_pclgen$evalref(a);
            qq_pcllib$genpc((i64)84);
            (*qq_pcllib$pccurr).pclop = (i64)(*p).pclop;
        }
        break;
    case 84:;
    case 85:;
        {
            qq_pclgen$evalref(a);
            qq_pclgen$evalunit(b,(i64)1);
            qq_pcllib$genpc((((i64)(*p).tag == (i64)84) ? (i64)125 : (i64)124));
        }
        break;
    case 74:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pcllib$genpc((i64)76);
        }
        break;
    case 75:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pcllib$genpc((i64)78);
        }
        break;
    case 69:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pcllib$genpc((i64)96);
            (*qq_pcllib$pccurr).usertag = (i64)(*p).mode;
        }
        break;
    case 70:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pcllib$genpc((i64)97);
            (*qq_pcllib$pccurr).n = (i64)(*p).flag;
        }
        break;
    case 46:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pcllib$genpc_name((i64)126,(struct qq_decls$strec *)(*b).def);
        }
        break;
    case 47:;
        {
            qq_pclgen$do_bin(a,b,(i64)131);
        }
        break;
    case 48:;
        {
            qq_pclgen$do_bin(a,b,(i64)137);
        }
        break;
    case 49:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pclgen$evalunit(b,(i64)1);
            if (!!((*b).nextunit)) {
                qq_pclgen$evalunit((*b).nextunit,(i64)1);
            }
            else {
                qq_pcllib$genpc((i64)14);
            }
;
            qq_pcllib$genpc((i64)134);
        }
        break;
    case 51:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pcllib$genpc((i64)21);
        }
        break;
    case 52:;
        {
            if (((i64)(*p).flag == (i64)1)) {
                if (((i64)(*a).tag == (i64)51)) {
                    qq_pclgen$evalunit((*a).a,(i64)1);
                }
                else {
                    qq_pclgen$evalref(a);
                }
;
            }
            else {
                qq_pclgen$evalref(a);
                qq_pcllib$genpc((i64)28);
            }
;
        }
        break;
    case 35:;
        {
            qq_pclgen$do_convert(p);
        }
        break;
    case 36:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pcllib$genpc_int((i64)99,(i64)(*p).mode);
        }
        break;
    case 34:;
        {
            qq_pcllib$genpc_int((i64)18,(i64)(*p).mode);
        }
        break;
    case 5:;
        {
            qq_pcllib$genpc_int((i64)19,(i64)(*p).pclop);
        }
        break;
    case 60:;
    case 61:;
        {
            qq_pclgen$do_incr(p,a,res);
        }
        break;
    case 28:;
        {
            qq_pcllib$genpc((i64)15);
        }
        break;
    case 25:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pcllib$genpc((i64)142);
        }
        break;
    case 53:;
        {
            qq_pcllib$genpc((i64)14);
        }
        break;
    case 33:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pcllib$genpc_n((i64)57,(i64)1);
        }
        break;
    case 68:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pcllib$genpc_n((i64)91,(i64)(*p).flag);
        }
        break;
    case 76:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pclgen$evalunit(b,(i64)1);
            qq_pcllib$genpc((i64)112);
            (*qq_pcllib$pccurr).n = (i64)(*p).flag;
        }
        break;
    case 77:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pclgen$evalunit(b,(i64)1);
            qq_pcllib$genpc((i64)113);
            (*qq_pcllib$pccurr).n = (i64)(*p).flag;
        }
        break;
    case 71:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pclgen$evalunit(b,(i64)1);
            qq_pcllib$genpc_n((i64)114,(i64)(*p).condcode);
        }
        break;
    case 64:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            if (((i64)(*p).mathsop == (i64)2)) {
                qq_pcllib$genpc((i64)81);
            }
            else {
                qq_pcllib$genpc((i64)82);
                (*qq_pcllib$pccurr).mathscode = (i64)(*p).mathsop;
            }
;
        }
        break;
    case 65:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pclgen$evalunit(b,(i64)1);
            qq_pcllib$genpc((i64)83);
            (*qq_pcllib$pccurr).mathscode = (i64)(*p).mathsop;
        }
        break;
    default: {
        qq_lib$gerror_s((byte*)"UNSUPPORTED TAG:",qq_tables$jtagnames[((i64)(*p).tag)],p);
    }
    } //SW
;
        {i64 $temp = (i64)qq_tables$jhasvalue[((i64)(*p).tag)];
if (($temp==(i64)0)) {
        if (!!(res)) {
            if (!((((i64)(*p).tag == (i64)54 || (i64)(*p).tag == (i64)55) && !!(((i64)(*p).flag & (i64)4))))) {
                qq_lib$gerror_s((byte*)"Value expected:",qq_tables$jtagnames[((i64)(*p).tag)],0);
            }
;
        }
;
    }
    else if (($temp==(i64)1)) {
        if (!(!!(res))) {
            if ((((i64)(*p).tag == (i64)26) && (procflag == (i64)1))) {
            }
            else if (((i64)(*p).tag == (i64)60 || (i64)(*p).tag == (i64)61)) {
            }
            else if ((((i64)(*p).tag == (i64)27) && ((i64)qq_tables$hostisfn[((*p).index)] == (i64)0))) {
            }
            else {
                qq_pcllib$genpc_n((i64)57,(i64)1);
            }
;
        }
;
    }
    };
}

void qq_pclgen$gencodemodule(struct qq_decls$subprogrec *sp,i64 moduleno) {
        struct qq_decls$strec *  anonprocs[100];
        i64 nanonprocs;
        struct qq_decls$strec *  d;
        struct qq_decls$strec *  e;
        i64 lab;
        i64 a;
        i64 b;
        struct qq_decls$filerec *  pm;
        struct qq_pcltabs$pclrec *  pc;
        struct qq_pcltabs$pclrec *  pctarget;
        byte (*labelmap)[];
        i64 $av_1;
        i64 $av_2;
        i64 i;
    nanonprocs = (i64)0;
    a = (i64)(*sp).firstmodule;
    b = (i64)(*sp).lastmodule;
    pm = qq_decls$modules[(moduleno)];
    qq_decls$currmodule = pm;
    qq_decls$stcurrproc = (qq_decls$stcurrmodule = (*qq_decls$currmodule).def);
    qq_pcllib$resetpcl((*pm).size);
    qq_pcllib$gencomment((byte*)"Module data init code:");
    qq_decls$qpos = (i64)0;
    qq_decls$qpos = msysc$m_setdotslice(qq_decls$qpos,(i64)24,(i64)31,(u64)moduleno);
    if ((moduleno == a)) {
        lab = qq_pcllib$createfwdlabel();
        qq_pcllib$genpc_lab((i64)29,lab);
        qq_pcllib$genpc_n((i64)59,(i64)1);
        qq_decls$stopseq = qq_pcllib$pccurr;
        qq_decls$raiseseq = (qq_pcllib$pccurr + (i64)1);
        qq_pcllib$genpc_int((i64)13,(i64)0);
        qq_pcllib$genpc((i64)142);
        qq_pcllib$definefwdlabel(lab);
    }
;
    d = (*qq_decls$stcurrmodule).deflist;
    L557 :;
    while (!!(d)) {
        if ((((i64)(*d).nameid == (i64)13) && !!((*d).code))) {
            qq_pclgen$evalunit((*d).code,(i64)1);
            if (((i64)msysc$m_getdotslice((i64)(*d).flags,(i64)11,(i64)12) == (i64)3)) {
                qq_pcllib$genpc((i64)26);
            }
;
            qq_pcllib$genpc_name((i64)23,(struct qq_decls$strec *)d);
        }
        else if (((i64)(*d).nameid == (i64)5)) {
            e = (*d).deflist;
            L560 :;
            while (!!(e)) {
                if ((((i64)(*e).nameid == (i64)13) && !!((*e).code))) {
                    qq_pclgen$evalunit((*e).code,(i64)1);
                    qq_pcllib$genpc_name((i64)23,(struct qq_decls$strec *)e);
                }
                else if (((i64)(*e).nameid == (i64)6)) {
                    if ((nanonprocs >= (i64)100)) {
                        qq_lib$gerror((byte*)"Too many anons",0);
                    }
;
                    anonprocs[(++(nanonprocs))-1] = e;
                }
;
                e = (*e).nextdef;
L561 :;
            }
L562 :;
            ;
        }
;
        d = (*d).nextdef;
L558 :;
    }
L559 :;
    ;
    if ((moduleno == a)) {
                ($av_1 = (a + (i64)1));
        for (i=b;i>=$av_1;--i) {
L563 :;
            qq_pcllib$genpc_name((i64)53,(struct qq_decls$strec *)(*qq_decls$modules[(i)]).def);
L564 :;
        }
L565 :;
        ;
                ($av_2 = (a + (i64)1));
        for (i=b;i>=$av_2;--i) {
L566 :;
            if (!!((*qq_decls$modules[(i)]).startfn)) {
                qq_pcllib$genpc_name((i64)49,(struct qq_decls$strec *)(*qq_decls$modules[(i)]).startfn);
            }
;
L567 :;
        }
L568 :;
        ;
        if (!!((*qq_decls$currmodule).startfn)) {
            qq_pcllib$genpc_name((i64)49,(struct qq_decls$strec *)(*qq_decls$currmodule).startfn);
        }
;
        if (!!((*qq_decls$currmodule).mainfn)) {
            qq_pcllib$genpc_name((i64)49,(struct qq_decls$strec *)(*qq_decls$currmodule).mainfn);
        }
;
        qq_pclgen$evalunit((*qq_decls$stcurrmodule).code,(i64)0);
        qq_pcllib$genpc_int((i64)13,(i64)0);
        qq_pcllib$genpc((i64)59);
    }
    else {
        qq_pclgen$evalunit((*qq_decls$stcurrmodule).code,(i64)0);
        qq_pcllib$genpc((i64)54);
    }
;
    qq_pcllib$gencomment((byte*)"Procs:");
    d = (*qq_decls$stcurrmodule).deflist;
    L569 :;
    while (!!(d)) {
        switch ((i64)(*d).nameid) {
        case 5:;
        case 6:;
            {
                qq_pclgen$do_procdef(d);
            }
            break;
        case 13:;
            {
            }
            break;
        case 9:;
            {
                e = (*d).deflist;
                L572 :;
                while (!!(e)) {
                    if (((i64)(*e).nameid == (i64)5)) {
                        qq_pclgen$do_procdef(e);
                    }
;
L573 :;
                    e = (*e).nextdef;
L575 :;
                                    }
L574 :;
                ;
            }
            break;
        case 18:;
            {
            }
            break;
        case 19:;
            {
            }
            break;
        case 17:;
            {
            }
            break;
        case 10:;
            {
            }
            break;
        case 7:;
            {
            }
            break;
        case 20:;
            {
            }
            break;
        case 22:;
            {
            }
            break;
        case 8:;
            {
            }
            break;
        default: {
            qq_lib$gerror_s((byte*)"?Module def:",qq_tables$namenames[((i64)(*d).nameid)],0);
        }
        } //SW
;
        d = (*d).nextdef;
L570 :;
    }
L571 :;
    ;
    for (i=(i64)1;i<=nanonprocs;++i) {
L576 :;
        qq_pclgen$do_procdef(anonprocs[(i)-1]);
L577 :;
    }
L578 :;
    ;
    qq_pcllib$genpc((i64)5);
    labelmap = (byte (*)[])mlib$pcm_allocz(qq_pcllib$nextlabelno);
    pc = qq_pcllib$pcstart;
    L579 :;
    while ((pc <= qq_pcllib$pccurr)) {
        if (((i64)qq_pcltabs$pclopnd[((i64)(*pc).opcode)] == (i64)6)) {
            lab = (*pc).labelno;
            pctarget = (*qq_pcllib$labelpctable)[(lab)-1];
            if ((pctarget == 0)) {
                qq_lib$gerror_s((byte*)"Lab undef:",msysc$strint(lab,0),0);
            }
;
            (*labelmap)[(lab)-1] = (i64)1;
            (*pc).labelref = pctarget;
        }
;
L580 :;
        ++(pc);
L582 :;
            }
L581 :;
    ;
    for (i=(i64)1;i<=qq_pcllib$nextlabelno;++i) {
L583 :;
        if (!!((i64)(*labelmap)[(i)-1])) {
            (*(*qq_pcllib$labelpctable)[(i)-1]).flags = msysc$m_setdotindex((*(*qq_pcllib$labelpctable)[(i)-1]).flags,(i64)0,(u64)1u);
        }
;
L584 :;
    }
L585 :;
    ;
    mlib$pcm_free(labelmap,qq_pcllib$nextlabelno);
    (*pm).pcstart = qq_pcllib$pcstart;
    (*pm).pcend = qq_pcllib$pccurr;
    (*pm).pcsize = ((qq_pcllib$pccurr - qq_pcllib$pcstart) + (i64)1);
    (*pm).pcsourcestart = qq_pcllib$pcsourcestart;
}

static void qq_pclgen$do_procdef(struct qq_decls$strec *p) {
        i64 nfreevars;
        i64 nnofreevars;
        i64 isfunc;
        struct qq_decls$strec *  oldcurrproc;
    oldcurrproc = qq_decls$stcurrproc;
    qq_decls$stcurrproc = p;
    qq_pclgen$retindex = qq_pcllib$createfwdlabel();
    isfunc = (i64)msysc$m_getdotindex((i64)(*p).flags,(i64)4);
    qq_pclgen$genprocentry(p,&nfreevars,&nnofreevars);
    if (((*p).code == 0)) {
        qq_lib$gerror_s((byte*)"Empty proc body",(*p).name,0);
    }
    else {
        qq_pclgen$evalunit((*p).code,isfunc);
        if (!!(isfunc)) {
            if (!(!!(qq_pclgen$checkblockreturn((*p).code)))) {
                qq_lib$gerror((byte*)"Func: return value missing",0);
            }
;
        }
;
    }
;
    qq_pcllib$definefwdlabel(qq_pclgen$retindex);
    qq_pclgen$genprocexit(nfreevars,nnofreevars,isfunc);
    qq_pcllib$genpc((i64)4);
    if (((i64)(*qq_pclgen$pprocentry).n == (i64)0)) {
        ++(qq_pclgen$pprocentry);
    }
;
    (*p).labelref = qq_pclgen$pprocentry;
    qq_decls$stcurrproc = oldcurrproc;
}

static void qq_pclgen$genprocentry(struct qq_decls$strec *p,i64 *nfreevars,i64 *nnofreevars) {
        i64 n;
        struct qq_decls$strec *  d;
    qq_pcllib$genpc_name((i64)2,(struct qq_decls$strec *)p);
    qq_pclgen$nprocparams = (qq_decls$nproclocals = (i64)0);
    d = (*p).deflist;
    L586 :;
    while (!!(d)) {
                {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)14)) {
            ++(qq_decls$nproclocals);
            (*d).index = qq_decls$nproclocals;
        }
        else if (($temp==(i64)15)) {
            ++(qq_pclgen$nprocparams);
        }
        };
        d = (*d).nextdef;
L587 :;
    }
L588 :;
    ;
    d = (*p).deflist;
    n = qq_pclgen$nprocparams;
    L589 :;
    while (!!(d)) {
                {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)15)) {
            --(n);
            (*d).index = -((n + (i64)1));
        }
        };
L590 :;
        d = (*d).nextdef;
L592 :;
            }
L591 :;
    ;
    qq_pclgen$retvaloffset = -((qq_pclgen$nprocparams + (i64)1));
    qq_pcllib$genpc_n((i64)3,qq_decls$nproclocals);
    qq_pclgen$pprocentry = qq_pcllib$pccurr;
    d = (*p).deflist;
    L593 :;
    while (!!(d)) {
                {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)14)) {
            if (!!((*d).code)) {
                qq_pclgen$evalunit((*d).code,(i64)1);
                if (((i64)msysc$m_getdotslice((i64)(*d).flags,(i64)11,(i64)12) == (i64)3)) {
                    qq_pcllib$genpc((i64)26);
                }
;
                qq_pcllib$genpc_name((i64)23,(struct qq_decls$strec *)d);
            }
;
        }
        };
        d = (*d).nextdef;
L594 :;
    }
L595 :;
    ;
}

static void qq_pclgen$genprocexit(i64 nfree,i64 nnofree,i64 isfunc) {
        i64 offset;
    if (!!(isfunc)) {
        offset = (-((qq_pclgen$nprocparams + (i64)1)) * (i64)16);
        qq_pcllib$genpc_xy((i64)52,qq_decls$nproclocals,offset);
        (*qq_pcllib$pccurr).n = qq_pclgen$nprocparams;
    }
    else {
        qq_pcllib$genpc_n((i64)51,qq_pclgen$nprocparams);
        (*qq_pcllib$pccurr).x = qq_decls$nproclocals;
    }
;
}

static void qq_pclgen$evalref(struct qq_decls$unitrec *p) {
        struct qq_decls$unitrec *  a;
        struct qq_decls$unitrec *  b;
        struct qq_decls$strec *  d;
        i64 lab1;
        i64 lab2;
    a = (*p).a;
    b = (*p).b;
    switch ((i64)(*p).tag) {
    case 39:;
        {
            d = (*p).def;
            if (((i64)(*d).nameid == (i64)5 || (i64)(*d).nameid == (i64)7)) {
                qq_lib$gerror((byte*)"^ not allowed",0);
            }
;
            if ((((i64)(*d).nameid == (i64)15) && !!(msysc$m_getdotindex((i64)(*d).flags,(i64)5)))) {
                qq_pcllib$genpc_name((i64)7,(struct qq_decls$strec *)d);
            }
            else {
                qq_pcllib$genpc_name((i64)9,(struct qq_decls$strec *)d);
            }
;
        }
        break;
    case 46:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pcllib$genpc_name((i64)130,(struct qq_decls$strec *)(*b).def);
        }
        break;
    case 47:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pclgen$evalunit(b,(i64)1);
            qq_pcllib$genpc((i64)133);
        }
        break;
    case 48:;
        {
            qq_pclgen$evalref(a);
            qq_pclgen$evalunit(b,(i64)1);
            qq_pcllib$genpc((i64)139);
        }
        break;
    case 49:;
        {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pclgen$evalunit(b,(i64)1);
            if (!!((*b).nextunit)) {
                qq_lib$gerror((byte*)"Def val not allowed",0);
            }
;
            qq_pcllib$genpc((i64)136);
        }
        break;
    case 51:;
        {
            qq_pclgen$evalunit(a,(i64)1);
        }
        break;
    case 7:;
        {
            lab1 = qq_pcllib$createfwdlabel();
            lab2 = qq_pcllib$createfwdlabel();
            qq_pclgen$genjumpcond((i64)32,(*p).a,lab1);
            qq_pclgen$evalref((*p).b);
            qq_pclgen$genjumpl(lab2);
            qq_pcllib$definefwdlabel(lab1);
            qq_pclgen$evalref((*(*p).b).nextunit);
            qq_pcllib$definefwdlabel(lab2);
        }
        break;
    default: {
        qq_lib$gerror_s((byte*)"evalref",qq_tables$jtagnames[((i64)(*p).tag)],0);
    }
    } //SW
;
}

static void qq_pclgen$genjumpcond(i64 opc,struct qq_decls$unitrec *p,i64 lab) {
        struct qq_decls$unitrec *  q;
        struct qq_decls$unitrec *  r;
        i64 oldpos;
        i64 lab2;
        i64 i;
    q = (*p).a;
    r = (*p).b;
    switch ((i64)(*p).tag) {
    case 72:;
        {
            if ((opc==(i64)32)) {
                qq_pclgen$genjumpcond((i64)32,q,lab);
                qq_pclgen$genjumpcond((i64)32,r,lab);
            }
            else if ((opc==(i64)31)) {
                lab2 = qq_pcllib$createfwdlabel();
                qq_pclgen$genjumpcond((i64)32,q,lab2);
                qq_pclgen$genjumpcond((i64)31,r,lab);
                qq_pcllib$definefwdlabel(lab2);
            }
;
        }
        break;
    case 73:;
        {
            if ((opc==(i64)32)) {
                lab2 = qq_pcllib$createfwdlabel();
                qq_pclgen$genjumpcond((i64)31,q,lab2);
                qq_pclgen$genjumpcond((i64)32,r,lab);
                qq_pcllib$definefwdlabel(lab2);
            }
            else if ((opc==(i64)31)) {
                qq_pclgen$genjumpcond((i64)31,q,lab);
                qq_pclgen$genjumpcond((i64)31,r,lab);
            }
;
        }
        break;
    case 74:;
        {
            if ((opc==(i64)32)) {
                qq_pclgen$genjumpcond((i64)31,q,lab);
            }
            else if ((opc==(i64)31)) {
                qq_pclgen$genjumpcond((i64)32,q,lab);
            }
;
        }
        break;
    case 75:;
        {
            qq_pclgen$genjumpcond(opc,q,lab);
        }
        break;
    case 6:;
        {
            L596 :;
            while ((!!(q) && !!((*q).nextunit))) {
                qq_pclgen$evalunit(q,(i64)1);
                q = (*q).nextunit;
L597 :;
            }
L598 :;
            ;
            qq_pclgen$genjumpcond(opc,q,lab);
        }
        break;
    case 71:;
        {
            qq_pclgen$evalunit(q,(i64)1);
            qq_pclgen$evalunit(r,(i64)1);
            qq_pclgen$gcomparejump(opc,(i64)(*p).condcode,lab);
        }
        break;
    case 38:;
        {
            r = (*q).nextunit;
            i = (i64)1;
            if ((opc == (i64)32)) {
                L599 :;
                while (!!(r)) {
                    qq_pclgen$evalunit(q,(i64)1);
                    qq_pclgen$evalunit(r,(i64)1);
                    qq_pclgen$gcomparejump((i64)31,(i64)qq_tables$revconds[((i64)(*p).cmpconds[(i)-1])],lab);
                    ++(i);
                    q = r;
                    r = (*r).nextunit;
L600 :;
                }
L601 :;
                ;
            }
            else {
                lab2 = qq_pcllib$createfwdlabel();
                L602 :;
                while (!!(r)) {
                    qq_pclgen$evalunit(q,(i64)1);
                    qq_pclgen$evalunit(r,(i64)1);
                    if (!!((*r).nextunit)) {
                        qq_pclgen$gcomparejump((i64)31,(i64)qq_tables$revconds[((i64)(*p).cmpconds[(i)-1])],lab2);
                    }
                    else {
                        qq_pclgen$gcomparejump((i64)31,(i64)(*p).cmpconds[(i)-1],lab);
                    }
;
                    ++(i);
                    q = r;
                    r = (*r).nextunit;
L603 :;
                }
L604 :;
                ;
                qq_pcllib$definefwdlabel(lab2);
            }
;
        }
        break;
    default: {
        qq_pclgen$evalunit(p,(i64)1);
        qq_pcllib$genpc_lab(opc,lab);
    }
    } //SW
;
    qq_decls$qpos = oldpos;
}

static void qq_pclgen$gcomparejump(i64 opc,i64 cond,i64 lab) {
    if ((opc == (i64)32)) {
        cond = (i64)qq_tables$revconds[(cond)];
    }
;
    qq_pcllib$genpc_lab(((i64)33 + cond),lab);
}

static void qq_pclgen$genjumpl(i64 lab) {
    qq_pcllib$genpc_lab((i64)29,lab);
}

void qq_pclgen$stacklooplabels(i64 a,i64 b,i64 c) {
    if ((qq_pclgen$loopindex >= (i64)20)) {
        qq_lib$gerror((byte*)"Too many nested loops",0);
    }
;
    ++(qq_pclgen$loopindex);
    qq_pclgen$loopstack[(qq_pclgen$loopindex)-1][((i64)1)-1] = a;
    qq_pclgen$loopstack[(qq_pclgen$loopindex)-1][((i64)2)-1] = b;
    qq_pclgen$loopstack[(qq_pclgen$loopindex)-1][((i64)3)-1] = c;
}

void qq_pclgen$unstacklooplabels(void) {
    --(qq_pclgen$loopindex);
}

i64 qq_pclgen$findlooplabel(i64 k,i64 n) {
        i64 i;
    if ((n == (i64)0)) {
        i = (i64)1;
    }
    else {
        i = (qq_pclgen$loopindex - (n - (i64)1));
    }
;
    if (((i < (i64)1) || (i > qq_pclgen$loopindex))) {
        qq_lib$gerror((byte*)"Bad loop index",0);
    }
;
    qq_pclgen$looptrylevel = qq_pclgen$trylevelstack[(i)-1];
    return qq_pclgen$loopstack[(i)-1][(k)-1];
}

static void qq_pclgen$do_assign(struct qq_decls$unitrec *a,struct qq_decls$unitrec *b,i64 res,i64 deepcopy) {
    if (((i64)(*a).tag==(i64)(*b).tag && (i64)(*b).tag==(i64)88)) {
        if (!!(res)) {
            qq_lib$gerror((byte*)"mult/ass::=",0);
        }
;
        qq_pclgen$do_multassign(a,b,deepcopy,res);
        return;
    }
;
    qq_pclgen$evalunit(b,(i64)1);
    if (!!(deepcopy)) {
        qq_pcllib$genpc((i64)26);
    }
;
    qq_pclgen$do_store(a,res);
}

static void qq_pclgen$do_bin(struct qq_decls$unitrec *a,struct qq_decls$unitrec *b,i64 opc) {
    qq_pclgen$evalunit(a,(i64)1);
    qq_pclgen$evalunit(b,(i64)1);
    qq_pcllib$genpc(opc);
}

static void qq_pclgen$do_binref(struct qq_decls$unitrec *a,struct qq_decls$unitrec *b,i64 opc) {
    qq_pclgen$evalref(a);
    qq_pclgen$evalunit(b,(i64)1);
    qq_pcllib$genpc(opc);
}

static void qq_pclgen$do_unary(struct qq_decls$unitrec *a,i64 opc) {
    qq_pclgen$evalunit(a,(i64)1);
    qq_pcllib$genpc(opc);
}

static void qq_pclgen$do_unaryref(struct qq_decls$unitrec *a,i64 opc) {
    qq_pclgen$evalref(a);
    qq_pcllib$genpc(opc);
}

static void qq_pclgen$do_pushlist(struct qq_decls$unitrec *a,i64 n) {
    L605 :;
    while (!!(a)) {
        qq_pclgen$evalunit(a,(i64)1);
L606 :;
        a = (*a).nextunit;
L608 :;
            }
L607 :;
    ;
}

static void qq_pclgen$do_makedict(struct qq_decls$unitrec *a,i64 n) {
        i64 $av_1;
    $av_1 = n;
    while ($av_1-- > 0) {
L609 :;
        if (((i64)(*a).tag == (i64)4)) {
            qq_pclgen$evalunit((*a).a,(i64)1);
            qq_pclgen$evalunit((*a).b,(i64)1);
        }
        else {
            qq_lib$gerror((byte*)"dict not key:val",0);
        }
;
        a = (*a).nextunit;
L610 :;
    }
L611 :;
    ;
    qq_pcllib$genpc_xy((i64)67,n,(i64)0);
}

static void qq_pclgen$do_call(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,struct qq_decls$unitrec *b,i64 res,i64 *procflag) {
        i64 nargs;
        i64 nsimple;
        i64 isfunc;
        i64 kwdindex;
        struct qq_decls$strec *  d;
        struct qq_decls$unitrec *  c;
        struct qq_decls$unitrec *  arglist[100];
        i64 i;
    isfunc = (i64)1;
    nargs = (nsimple = (i64)0);
    kwdindex = (i64)0;
    c = b;
    L612 :;
    while (!!(c)) {
        arglist[(++(nargs))-1] = c;
        if (((i64)(*c).tag == (i64)41 || (i64)(*c).tag == (i64)42)) {
            ++(nsimple);
        }
;
        if (((i64)(*c).tag == (i64)3)) {
            if ((kwdindex == (i64)0)) {
                kwdindex = nargs;
            }
;
        }
        else if (!!(kwdindex)) {
            qq_lib$gerror((byte*)"Non-kwd follows kwd arg",0);
        }
;
        c = (*c).nextunit;
L613 :;
    }
L614 :;
    ;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)39)) {
        d = (*a).def;
        //retry:
L615 :;
;
                {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)5) || ($temp==(i64)6)) {
            if (!!(msysc$m_getdotindex((i64)(*d).flags,(i64)4))) {
                qq_pcllib$genpc((i64)14);
                nargs = qq_pclgen$pushparams(d,&arglist,nargs,kwdindex);
                qq_pcllib$genpc_name((i64)49,(struct qq_decls$strec *)d);
            }
            else {
                isfunc = (i64)0;
                nargs = qq_pclgen$pushparams(d,&arglist,nargs,kwdindex);
                qq_pcllib$genpc_name((i64)49,(struct qq_decls$strec *)d);
            }
;
            (*qq_pcllib$pccurr).n = nargs;
        }
        else if (($temp==(i64)7)) {
            if (!(!!(msysc$m_getdotindex((i64)(*d).flags,(i64)4)))) {
                isfunc = (i64)0;
            }
            else {
                qq_pcllib$genpc((i64)14);
            }
;
            nargs = qq_pclgen$pushparams(d,&arglist,nargs,kwdindex);
            qq_pcllib$genpc_name((i64)55,(struct qq_decls$strec *)d);
            (*qq_pcllib$pccurr).n = nargs;
        }
        else if (($temp==(i64)20)) {
            d = (*d).alias;
            goto L615 ;
;
        }
        else if (($temp==(i64)13) || ($temp==(i64)14) || ($temp==(i64)15)) {
            goto L616 ;
;
        }
        else {
            qq_lib$gerror_s((byte*)"CAN'T CALL:",qq_tables$namenames[((i64)(*d).nameid)],0);
        }
        };
    }
    else if (($temp==(i64)46)) {
        if (!!(kwdindex)) {
            goto L616 ;
;
        }
;
        qq_pcllib$genpc((i64)14);
        qq_pclgen$evalref((*a).a);
        for (i=(i64)1;i<=nargs;++i) {
L617 :;
            qq_pclgen$evalunit(arglist[(i)-1],(i64)1);
L618 :;
        }
L619 :;
        ;
        qq_pclgen$evalunit(a,(i64)1);
        qq_pcllib$genpc_n((i64)50,++(nargs));
    }
    else {
        //docallptr:
L616 :;
;
        if (!!(kwdindex)) {
            qq_lib$gerror((byte*)"Kwd params not allowed for fnptr",0);
        }
;
        qq_pcllib$genpc((i64)14);
        for (i=(i64)1;i<=nargs;++i) {
L620 :;
            qq_pclgen$evalunit(arglist[(i)-1],(i64)1);
L621 :;
        }
L622 :;
        ;
        qq_pclgen$evalunit(a,(i64)1);
        qq_pcllib$genpc_n((i64)50,nargs);
    }
    };
    if ((!!(res) && !(!!(isfunc)))) {
        qq_lib$gerror((byte*)"Func ret value expected",0);
    }
;
    (*procflag) = (i64)!(!!(isfunc));
}

static i64 qq_pclgen$pushparams(struct qq_decls$strec *d,struct qq_decls$unitrec *(*arglist)[],i64 nargs,i64 kwdindex) {
        i64 nparams;
        i64 extra;
        i64 n;
        struct qq_decls$strec *  paramlist[100];
        byte byreflist[100];
        struct qq_decls$strec *  e;
        struct qq_decls$strec *  p;
        i64 i;
    nparams = (i64)(*d).nparams;
    e = (*d).deflist;
    n = (i64)0;
    L623 :;
    while (!!(e)) {
        ++(n);
        paramlist[(n)-1] = e;
        byreflist[(n)-1] = (i64)msysc$m_getdotindex((i64)(*e).flags,(i64)5);
        e = (*e).nextdef;
L624 :;
    }
L625 :;
    ;
    if (!!(kwdindex)) {
        qq_pclgen$pushkwdparams(d,arglist,nargs,kwdindex);
        return (i64)(*d).nparams;
    }
;
    extra = (i64)0;
    if ((nargs == nparams)) {
        for (i=(i64)1;i<=nargs;++i) {
L626 :;
            qq_pclgen$evalparam((*arglist)[(i)-1],(i64)byreflist[(i)-1]);
L627 :;
        }
L628 :;
        ;
        return nargs;
    }
    else if ((nargs < nparams)) {
        for (i=(i64)1;i<=nargs;++i) {
L629 :;
            qq_pclgen$evalparam((*arglist)[(i)-1],(i64)byreflist[(i)-1]);
L630 :;
        }
L631 :;
        ;
        for (i=(nargs + (i64)1);i<=nparams;++i) {
L632 :;
            p = paramlist[(i)-1];
            if ((!(!!((*p).code)) && !(!!(msysc$m_getdotindex((i64)(*p).flags,(i64)7))))) {
                qq_lib$gerror_s((byte*)"Param not optional:",msysc$strint(i,0),0);
            }
;
            if (!!((*p).code)) {
                if (!!((i64)byreflist[(i)-1])) {
                    qq_lib$gerror((byte*)"byref with default val",0);
                }
;
                qq_pclgen$evalunit((*p).code,(i64)1);
            }
            else {
                qq_pcllib$genpc((i64)14);
            }
;
L633 :;
        }
L634 :;
        ;
        return nparams;
    }
    else {
        for (i=(i64)1;i<=nparams;++i) {
L635 :;
            qq_pclgen$evalparam((*arglist)[(i)-1],(i64)byreflist[(i)-1]);
L636 :;
        }
L637 :;
        ;
        if (!(!!(msysc$m_getdotindex((i64)(*d).flags,(i64)8)))) {
            qq_lib$gerror((byte*)"Too many args",0);
        }
;
        for (i=(nparams + (i64)1);i<=nargs;++i) {
L638 :;
            qq_pclgen$evalunit((*arglist)[(i)-1],(i64)1);
L639 :;
        }
L640 :;
        ;
        return nargs;
    }
;
}

static void qq_pclgen$evalparam(struct qq_decls$unitrec *a,i64 byref) {
    if (!!(byref)) {
        qq_pclgen$evalref(a);
    }
    else {
        qq_pclgen$evalunit(a,(i64)1);
    }
;
}

static void qq_pclgen$pushkwdparams(struct qq_decls$strec *d,struct qq_decls$unitrec *(*arglist)[],i64 nargs,i64 kwdindex) {
        i64 nparams;
        i64 i;
        i64 j;
        i64 k;
        struct qq_decls$strec *  paramlist[100];
        byte byreflist[100];
        struct qq_decls$unitrec *  keyunits[100];
        struct qq_decls$unitrec *  p;
        struct qq_decls$unitrec *  q;
        struct qq_decls$strec *  e;
        i64 $av_1;
    nparams = (i64)(*d).nparams;
    e = (*d).deflist;
    for (i=(i64)1;i<=nparams;++i) {
L641 :;
        paramlist[(i)-1] = e;
        byreflist[(i)-1] = (i64)msysc$m_getdotindex((i64)(*e).flags,(i64)5);
        e = (*e).nextdef;
L642 :;
    }
L643 :;
    ;
    if ((nargs > nparams)) {
        qq_lib$gerror((byte*)"Too many args",0);
    }
;
    for (i=kwdindex;i<=nparams;++i) {
L644 :;
        keyunits[(i)-1] = 0;
L645 :;
    }
L646 :;
    ;
        ($av_1 = (kwdindex - (i64)1));
    for (i=(i64)1;i<=$av_1;++i) {
L647 :;
        qq_pclgen$evalparam((*arglist)[(i)-1],(i64)byreflist[(i)-1]);
L648 :;
    }
L649 :;
    ;
    for (i=kwdindex;i<=nargs;++i) {
L650 :;
        p = (*arglist)[(i)-1];
        q = (*p).a;
        if (((i64)(*q).tag != (i64)39)) {
            qq_lib$gerror((byte*)"kwd not a name",0);
        }
;
        e = (*q).def;
        k = (i64)0;
        for (j=(i64)1;j<=nparams;++j) {
L653 :;
            if (!!(mlib$eqstring((*e).name,(*paramlist[(j)-1]).name))) {
                k = j;
                goto L655 ;
            }
;
L654 :;
        }
L655 :;
        ;
        if ((k == (i64)0)) {
            qq_lib$gerror_s((byte*)"Can't find kwd param:",(*e).name,0);
        }
;
        if ((k < kwdindex)) {
            qq_lib$gerror_s((byte*)"Kwd arg already positional:",(*e).name,0);
        }
;
        if (!!(keyunits[(k)-1])) {
            qq_lib$gerror_s((byte*)"Repeating kwd arg:",(*e).name,0);
        }
;
        keyunits[(k)-1] = (*p).b;
L651 :;
    }
L652 :;
    ;
    for (i=kwdindex;i<=nparams;++i) {
L656 :;
        if ((keyunits[(i)-1] == 0)) {
            q = (*paramlist[(i)-1]).code;
            if (((q == 0) && !(!!(msysc$m_getdotindex((i64)(*paramlist[(i)-1]).flags,(i64)7))))) {
                qq_lib$gerror_s((byte*)"Param not optional:",msysc$strint(i,0),0);
            }
;
            keyunits[(i)-1] = q;
        }
;
L657 :;
    }
L658 :;
    ;
    for (i=kwdindex;i<=nparams;++i) {
L659 :;
        if (!!(keyunits[(i)-1])) {
            qq_pclgen$evalparam(keyunits[(i)-1],(i64)byreflist[(i)-1]);
        }
        else if (!!((i64)byreflist[(i)-1])) {
            qq_lib$gerror((byte*)"byref param not optional",0);
        }
        else {
            qq_pcllib$genpc((i64)14);
        }
;
L660 :;
    }
L661 :;
    ;
}

static void qq_pclgen$do_if(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,struct qq_decls$unitrec *b,struct qq_decls$unitrec *pelse,i64 res) {
        i64 lab1;
        i64 lab2;
    lab1 = qq_pcllib$createfwdlabel();
    if ((!!(pelse) || !!(res))) {
        lab2 = qq_pcllib$createfwdlabel();
    }
;
    qq_pclgen$genjumpcond((i64)32,a,lab1);
    qq_pclgen$evalunit(b,res);
    if ((!!(pelse) || !!(res))) {
        qq_pclgen$genjumpl(lab2);
        qq_pcllib$definefwdlabel(lab1);
        if (!!(pelse)) {
            qq_pclgen$evalunit(pelse,res);
        }
        else {
            qq_pcllib$genpc((i64)14);
        }
;
        qq_pcllib$definefwdlabel(lab2);
    }
    else {
        qq_pcllib$definefwdlabel(lab1);
    }
;
}

static void qq_pclgen$do_do(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a) {
        i64 lab_abc;
        i64 lab_d;
    lab_abc = qq_pcllib$definelabel();
    lab_d = qq_pcllib$createfwdlabel();
    qq_pclgen$stacklooplabels(lab_abc,lab_abc,lab_d);
    qq_pclgen$evalunit(a,(i64)0);
    qq_pclgen$genjumpl(lab_abc);
    qq_pcllib$definefwdlabel(lab_d);
    qq_pclgen$unstacklooplabels();
}

static void qq_pclgen$do_loop(struct qq_decls$unitrec *p) {
        i64 n;
        i64 index;
    index = (*(*p).a).value;
    if ((index == (i64)0)) {
        index = qq_pclgen$loopindex;
    }
;
    n = qq_pclgen$findlooplabel((i64)(*p).loopcode,index);
    if ((n == (i64)0)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"BAD LOOP",NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
    else {
        qq_pclgen$genjumpl(n);
    }
;
}

static void qq_pclgen$do_to(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pcount,struct qq_decls$unitrec *pbody) {
        i64 lab_b;
        i64 lab_c;
        i64 lab_d;
        struct qq_decls$strec *  temp;
        struct qq_decls$unitrec *  pav;
    pav = (*pcount).nextunit;
    temp = (*pav).def;
    qq_pclgen$evalunit(pcount,(i64)1);
    qq_pcllib$genpc_name((i64)23,(struct qq_decls$strec *)temp);
    lab_b = qq_pcllib$createfwdlabel();
    lab_c = qq_pcllib$createfwdlabel();
    lab_d = qq_pcllib$createfwdlabel();
    qq_pclgen$stacklooplabels(lab_b,lab_c,lab_d);
    if (((i64)(*pcount).tag != (i64)41)) {
        qq_pcllib$genpc_name((i64)7,(struct qq_decls$strec *)temp);
        qq_pcllib$genpc_int((i64)13,(i64)0);
        qq_pcllib$genpc_lab((i64)36,lab_d);
    }
    else if (((*pcount).value <= (i64)0)) {
        qq_pcllib$genpc_lab((i64)29,lab_d);
    }
;
    qq_pcllib$definefwdlabel(lab_b);
    qq_pclgen$evalunit(pbody,(i64)0);
    qq_pcllib$definefwdlabel(lab_c);
    qq_pcllib$genpc_lab(((i64)43 + (i64)msysc$m_getdotindex((i64)(*temp).flags,(i64)9)),lab_b);
    qq_pcllib$genpc_name((i64)7,(struct qq_decls$strec *)temp);
    qq_pcllib$definefwdlabel(lab_d);
    qq_pclgen$unstacklooplabels();
}

static void qq_pclgen$do_while(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pcond,struct qq_decls$unitrec *pbody) {
        i64 lab_b;
        i64 lab_c;
        i64 lab_d;
        i64 lab_incr;
        struct qq_decls$unitrec *  pincr;
    pincr = (*pcond).nextunit;
    lab_b = qq_pcllib$createfwdlabel();
    lab_c = qq_pcllib$createfwdlabel();
    lab_d = qq_pcllib$createfwdlabel();
    if (!!(pincr)) {
        lab_incr = qq_pcllib$createfwdlabel();
    }
    else {
        lab_incr = lab_c;
    }
;
    qq_pclgen$stacklooplabels(lab_b,lab_c,lab_d);
    qq_pclgen$genjumpl(lab_incr);
    qq_pcllib$definefwdlabel(lab_b);
    qq_pclgen$evalunit(pbody,(i64)0);
    qq_pcllib$definefwdlabel(lab_c);
    if (!!(pincr)) {
        qq_pclgen$evalunit(pincr,(i64)1);
        qq_pcllib$definefwdlabel(lab_incr);
    }
;
    qq_pclgen$genjumpcond((i64)31,pcond,lab_b);
    qq_pcllib$definefwdlabel(lab_d);
    --(qq_pclgen$loopindex);
}

static void qq_pclgen$do_repeat(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,struct qq_decls$unitrec *b) {
        i64 lab_b;
        i64 lab_c;
        i64 lab_d;
    lab_b = qq_pcllib$definelabel();
    lab_c = qq_pcllib$createfwdlabel();
    lab_d = qq_pcllib$createfwdlabel();
    qq_pclgen$stacklooplabels(lab_b,lab_c,lab_d);
    qq_pclgen$evalunit(a,(i64)0);
    qq_pcllib$definefwdlabel(lab_c);
    if (!((((i64)(*b).tag == (i64)41) && ((*b).value == (i64)0)))) {
        qq_pclgen$genjumpcond((i64)32,b,lab_b);
    }
;
    qq_pcllib$definefwdlabel(lab_d);
    --(qq_pclgen$loopindex);
}

static void qq_pclgen$do_for(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pvar,struct qq_decls$unitrec *pbody) {
        struct qq_decls$unitrec *  pfrom;
        struct qq_decls$unitrec *  pto;
        struct qq_decls$unitrec *  pstep;
        struct qq_decls$unitrec *  pelse;
        struct qq_decls$unitrec *  pautovar;
        struct qq_decls$strec *  dvar;
        struct qq_decls$strec *  limitvar;
        i64 lab_b;
        i64 lab_c;
        i64 lab_d;
        i64 lab_e;
        i64 opc;
        i64 oldqpos;
        i64 step;
        i64 fromval;
        i64 limit;
        i64 jumpinto;
    pfrom = (*pvar).nextunit;
    pto = (*pfrom).nextunit;
    pstep = (*pto).nextunit;
    pautovar = 0;
    if (!!(pstep)) {
        qq_lib$gerror((byte*)"By N not implem",0);
    }
;
    pelse = (*pbody).nextunit;
    dvar = (*pvar).def;
    if ((!(((i64)(*pto).tag == (i64)41 || (i64)(*pto).tag == (i64)39)) || (((i64)(*pto).tag == (i64)39) && (msysc$m_getdotindex((i64)(*(*pto).def).flags,(i64)9) != msysc$m_getdotindex((i64)(*dvar).flags,(i64)9))))) {
        pautovar = qq_lib$createavnamex(qq_decls$stcurrproc);
    }
;
    if (!!((i64)(*p).flag)) {
        step = (i64)-1;
    }
    else {
        step = (i64)1;
    }
;
    jumpinto = (i64)1;
    lab_b = qq_pcllib$createfwdlabel();
    lab_c = qq_pcllib$createfwdlabel();
    lab_d = qq_pcllib$createfwdlabel();
    lab_e = (!!(pelse) ? qq_pcllib$createfwdlabel() : lab_d);
    qq_pclgen$stacklooplabels(lab_b,lab_c,lab_d);
    if (((i64)(*pfrom).tag == (i64)41)) {
        fromval = (*pfrom).value;
        if (((i64)(*pto).tag == (i64)41)) {
            limit = (*pto).value;
            if ((((step == (i64)-1) && (fromval >= limit)) || ((step == (i64)1) && (fromval <= limit)))) {
                jumpinto = (i64)0;
            }
;
        }
;
        if (!!(jumpinto)) {
            if ((step < (i64)0)) {
                ++(fromval);
            }
            else {
                --(fromval);
            }
;
            (*pfrom).value = fromval;
        }
;
        qq_pcllib$genpc_int((i64)13,(*pfrom).value);
        qq_pcllib$genpc_name((i64)11,(struct qq_decls$strec *)dvar);
    }
    else {
        qq_pclgen$evalunit(pfrom,(i64)1);
        qq_pcllib$genpc_name((i64)11,(struct qq_decls$strec *)dvar);
        qq_pcllib$genpc_name((i64)70,(struct qq_decls$strec *)dvar);
        (*qq_pcllib$pccurr).x = -(step);
    }
;
    if (!!(pautovar)) {
        qq_pclgen$evalunit(pto,(i64)1);
        limitvar = (*pautovar).def;
        qq_pcllib$genpc_name((i64)23,(struct qq_decls$strec *)limitvar);
        pto = pautovar;
    }
    else {
        limitvar = (*pto).def;
    }
;
    if (!!(jumpinto)) {
        qq_pclgen$genjumpl(lab_c);
    }
;
    qq_pcllib$definefwdlabel(lab_b);
    qq_pclgen$evalunit(pbody,(i64)0);
    qq_pcllib$definefwdlabel(lab_c);
    if ((step > (i64)0)) {
        if (((i64)(*pto).tag == (i64)41)) {
            opc = ((i64)45 + (i64)msysc$m_getdotindex((i64)(*dvar).flags,(i64)9));
        }
        else if ((msysc$m_getdotindex((i64)(*dvar).flags,(i64)9) == msysc$m_getdotindex((i64)(*limitvar).flags,(i64)9))) {
            opc = ((i64)47 + (i64)msysc$m_getdotindex((i64)(*dvar).flags,(i64)9));
        }
        else {
            qq_lib$gerror((byte*)"for:mixed m/f vars",0);
        }
;
        oldqpos = qq_decls$qpos;
        qq_decls$qpos = (i64)(*p).pos;
        qq_pcllib$genpc_lab(opc,lab_b);
        qq_decls$qpos = oldqpos;
        qq_pcllib$genpc_name((i64)7,(struct qq_decls$strec *)dvar);
        if (((i64)(*pto).tag == (i64)41)) {
            qq_pcllib$genpc_int((i64)13,(*pto).value);
        }
        else {
            qq_pcllib$genpc_name((i64)7,(struct qq_decls$strec *)limitvar);
        }
;
    }
    else {
        qq_pcllib$genpc_name((i64)70,(struct qq_decls$strec *)dvar);
        (*qq_pcllib$pccurr).x = (i64)-1;
        qq_pcllib$genpc_name((i64)7,(struct qq_decls$strec *)dvar);
        if (((i64)(*pto).tag == (i64)41)) {
            qq_pcllib$genpc_int((i64)13,(*pto).value);
        }
        else {
            qq_pcllib$genpc_name((i64)7,(struct qq_decls$strec *)limitvar);
        }
;
        qq_pcllib$genpc_lab((i64)37,lab_b);
    }
;
    if (!!(pelse)) {
        qq_pcllib$definefwdlabel(lab_e);
        qq_pclgen$evalunit(pelse,(i64)0);
    }
;
    qq_pcllib$definefwdlabel(lab_d);
    qq_pclgen$unstacklooplabels();
}

static void qq_pclgen$do_forx(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pvar,struct qq_decls$unitrec *pbody) {
        struct qq_decls$unitrec *  pbounds;
        struct qq_decls$unitrec *  pelse;
        struct qq_decls$unitrec *  pautovar;
        struct qq_decls$strec *  dvar;
        struct qq_decls$strec *  limitvar;
        i64 lab_b;
        i64 lab_c;
        i64 lab_d;
        i64 lab_e;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"FORX",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    pbounds = (*pvar).nextunit;
    pautovar = qq_lib$createavnamex(qq_decls$stcurrproc);
    pelse = (*pbody).nextunit;
    dvar = (*pvar).def;
    lab_b = qq_pcllib$createfwdlabel();
    lab_c = qq_pcllib$createfwdlabel();
    lab_d = qq_pcllib$createfwdlabel();
    lab_e = (!!(pelse) ? qq_pcllib$createfwdlabel() : lab_d);
    qq_pclgen$stacklooplabels(lab_b,lab_c,lab_d);
    qq_pclgen$evalunit(pbounds,(i64)1);
    limitvar = (*pautovar).def;
    qq_pcllib$genpc_name((i64)23,(struct qq_decls$strec *)limitvar);
    qq_pcllib$genpc_int((i64)13,(i64)1);
    qq_pcllib$genpc((i64)101);
    qq_pcllib$genpc_name((i64)11,(struct qq_decls$strec *)dvar);
    qq_pclgen$genjumpl(lab_c);
    qq_pcllib$definefwdlabel(lab_b);
    qq_pclgen$evalunit(pbody,(i64)0);
    qq_pcllib$definefwdlabel(lab_c);
    if ((msysc$m_getdotindex((i64)(*dvar).flags,(i64)9) == msysc$m_getdotindex((i64)(*limitvar).flags,(i64)9))) {
        qq_pcllib$genpc_lab(((i64)47 + (i64)msysc$m_getdotindex((i64)(*dvar).flags,(i64)9)),lab_b);
    }
    else {
        qq_lib$gerror((byte*)"forx:mixed m/f",0);
    }
;
    qq_pcllib$genpc_name((i64)7,(struct qq_decls$strec *)dvar);
    qq_pcllib$genpc_name((i64)7,(struct qq_decls$strec *)limitvar);
    if (!!(pelse)) {
        qq_pcllib$definefwdlabel(lab_e);
        qq_pclgen$evalunit(pelse,(i64)0);
    }
;
    qq_pcllib$definefwdlabel(lab_d);
    qq_pclgen$unstacklooplabels();
}

static void qq_pclgen$do_print(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,struct qq_decls$unitrec *b) {
        i64 issprint;
        struct qq_decls$unitrec *  x;
    issprint = ((i64)(*p).flag & (i64)4);
    if (!!(issprint)) {
        qq_pclgen$callhostfn((i64)3,(i64)0);
    }
    else {
        if (!!(a)) {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pclgen$callhostfn((i64)1,(i64)0);
        }
        else {
            qq_pclgen$callhostfn((i64)2,(i64)0);
        }
;
    }
;
    x = b;
    L662 :;
    while (!!(x)) {
                {i64 $temp = (i64)(*x).tag;
if (($temp==(i64)58)) {
            qq_pclgen$evalunit((*x).b,(i64)1);
            qq_pclgen$evalunit((*x).a,(i64)1);
            qq_pclgen$callhostfn((i64)7,(i64)0);
        }
        else if (($temp==(i64)56)) {
            qq_pclgen$callhostfn((i64)10,(i64)0);
        }
        else if (($temp==(i64)57)) {
            qq_pclgen$callhostfn((i64)11,(i64)0);
        }
        else {
            qq_pclgen$evalunit(x,(i64)1);
            qq_pclgen$callhostfn((i64)8,(i64)0);
        }
        };
        x = (*x).nextunit;
L663 :;
    }
L664 :;
    ;
    if (!!(((i64)(*p).flag & (i64)1))) {
        qq_pclgen$callhostfn((i64)9,(i64)0);
    }
;
    if (!!(issprint)) {
        qq_pcllib$genpc((i64)14);
        qq_pclgen$callhostfn((i64)6,(i64)0);
    }
    else {
        qq_pclgen$callhostfn((i64)5,(i64)0);
    }
;
}

static void qq_pclgen$do_fprint(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,struct qq_decls$unitrec *b,struct qq_decls$unitrec *c) {
        i64 issfprint;
        struct qq_decls$unitrec *  x;
    issfprint = ((i64)(*p).flag & (i64)4);
    if (!!(issfprint)) {
        qq_pclgen$callhostfn((i64)3,(i64)0);
    }
    else {
        if (!!(a)) {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pclgen$callhostfn((i64)1,(i64)0);
        }
        else {
            qq_pclgen$callhostfn((i64)2,(i64)0);
        }
;
    }
;
    qq_pclgen$evalunit(b,(i64)1);
    qq_pclgen$callhostfn((i64)4,(i64)0);
    x = c;
    L665 :;
    while (!!(x)) {
                {i64 $temp = (i64)(*x).tag;
if (($temp==(i64)58)) {
            qq_pclgen$evalunit((*x).b,(i64)1);
            qq_pclgen$evalunit((*x).a,(i64)1);
            qq_pclgen$callhostfn((i64)7,(i64)0);
        }
        else if (($temp==(i64)56)) {
            qq_pclgen$callhostfn((i64)10,(i64)0);
        }
        else {
            qq_pcllib$genpc((i64)14);
            qq_pclgen$evalunit(x,(i64)1);
            qq_pclgen$callhostfn((i64)7,(i64)0);
        }
        };
        x = (*x).nextunit;
L666 :;
    }
L667 :;
    ;
    if (!!(((i64)(*p).flag & (i64)1))) {
        qq_pclgen$callhostfn((i64)9,(i64)0);
    }
;
    if (!!(issfprint)) {
        qq_pcllib$genpc((i64)14);
        qq_pclgen$callhostfn((i64)6,(i64)0);
    }
    else {
        qq_pclgen$callhostfn((i64)5,(i64)0);
    }
;
}

static void qq_pclgen$do_read(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,struct qq_decls$unitrec *b) {
        struct qq_decls$unitrec *  x;
        struct qq_decls$unitrec *  xloop;
    if (!!(((i64)(*p).flag & (i64)1))) {
        if (!!(a)) {
            qq_pclgen$evalunit(a,(i64)1);
            qq_pclgen$callhostfn((i64)12,(i64)0);
        }
        else {
            qq_pcllib$genpc((i64)14);
            qq_pclgen$callhostfn((i64)12,(i64)0);
        }
;
    }
;
    xloop = b;
    L668 :;
    while (!!(xloop)) {
        x = xloop;
        qq_pcllib$genpc((i64)14);
        if (((i64)(*x).tag == (i64)58)) {
            qq_pclgen$evalunit((*x).b,(i64)1);
            qq_pclgen$callhostfn((i64)14,(i64)0);
            x = (*x).a;
        }
        else {
            qq_pcllib$genpc((i64)14);
            qq_pclgen$callhostfn((i64)14,(i64)0);
        }
;
        if (((i64)(*x).tag == (i64)39)) {
            qq_pcllib$genpc_name((i64)11,(struct qq_decls$strec *)(*x).def);
        }
        else {
            qq_pclgen$evalref(x);
            qq_pcllib$genpc((i64)22);
        }
;
        xloop = (*xloop).nextunit;
L669 :;
    }
L670 :;
    ;
}

static void qq_pclgen$do_forall(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pindex,struct qq_decls$unitrec *pbody) {
        i64 lab_b;
        i64 lab_c;
        i64 lab_d;
        i64 lab_e;
        struct qq_decls$unitrec *  ploopvar;
        struct qq_decls$unitrec *  plist;
        struct qq_decls$unitrec *  pelse;
        struct qq_decls$unitrec *  plimitvar;
        struct qq_decls$unitrec *  plistvar;
        struct qq_decls$strec *  indexvar;
        struct qq_decls$strec *  limitvar;
        struct qq_decls$strec *  loopvar;
        struct qq_decls$strec *  listvar;
    plist = (*pindex).nextunit;
    ploopvar = (*plist).nextunit;
    if ((ploopvar == 0)) {
        ploopvar = pindex;
        pindex = qq_lib$createavnamex(qq_decls$stcurrproc);
    }
;
    loopvar = (*ploopvar).def;
    plimitvar = qq_lib$createavnamex(qq_decls$stcurrproc);
    limitvar = (*plimitvar).def;
    indexvar = (*pindex).def;
    if ((((i64)(*plist).tag != (i64)39) || (msysc$m_getdotindex((i64)(*(*plist).def).flags,(i64)9) != msysc$m_getdotindex((i64)(*loopvar).flags,(i64)9)))) {
        plistvar = qq_lib$createavnamex(qq_decls$stcurrproc);
        listvar = (*plistvar).def;
        qq_pclgen$evalunit(plist,(i64)1);
        qq_pcllib$genpc_name((i64)23,(struct qq_decls$strec *)listvar);
    }
    else {
        plistvar = plist;
        listvar = (*plistvar).def;
    }
;
    if (!((msysc$m_getdotindex((i64)(*indexvar).flags,(i64)9)==msysc$m_getdotindex((i64)(*loopvar).flags,(i64)9) && msysc$m_getdotindex((i64)(*loopvar).flags,(i64)9)==msysc$m_getdotindex((i64)(*listvar).flags,(i64)9)))) {
        qq_lib$gerror((byte*)"forall: mixed vars",0);
    }
;
    pelse = (*pbody).nextunit;
    lab_b = qq_pcllib$createfwdlabel();
    lab_c = qq_pcllib$createfwdlabel();
    lab_d = qq_pcllib$createfwdlabel();
    lab_e = (!!(pelse) ? qq_pcllib$createfwdlabel() : lab_d);
    qq_pclgen$stacklooplabels(lab_b,lab_c,lab_d);
    qq_pcllib$genpc_name((i64)7,(struct qq_decls$strec *)listvar);
    qq_pcllib$genpc_n((i64)89,(i64)2);
    qq_pcllib$genpc_name((i64)23,(struct qq_decls$strec *)limitvar);
    qq_pcllib$genpc_int((i64)13,(i64)1);
    qq_pcllib$genpc((i64)101);
    qq_pcllib$genpc_name((i64)23,(struct qq_decls$strec *)indexvar);
    qq_pclgen$genjumpl(lab_c);
    qq_pcllib$definefwdlabel(lab_b);
    qq_pcllib$genpc_name((i64)7,(struct qq_decls$strec *)listvar);
    qq_pclgen$evalunit(pindex,(i64)1);
    qq_pcllib$genpc((((i64)(*p).tag == (i64)17) ? (i64)131 : (i64)137));
    qq_pcllib$genpc_name((i64)11,(struct qq_decls$strec *)loopvar);
    qq_pclgen$evalunit(pbody,(i64)0);
    qq_pcllib$definefwdlabel(lab_c);
    if ((msysc$m_getdotindex((i64)(*indexvar).flags,(i64)9) == msysc$m_getdotindex((i64)(*limitvar).flags,(i64)9))) {
        qq_pcllib$genpc_lab(((i64)47 + (i64)msysc$m_getdotindex((i64)(*indexvar).flags,(i64)9)),lab_b);
    }
    else {
        qq_lib$gerror((byte*)"forall:mixed m/f",0);
    }
;
    qq_pcllib$genpc_name((i64)7,(struct qq_decls$strec *)indexvar);
    qq_pcllib$genpc_name((i64)7,(struct qq_decls$strec *)limitvar);
    if (!!(pelse)) {
        qq_pcllib$definefwdlabel(lab_e);
        qq_pclgen$evalunit(pelse,(i64)0);
    }
;
    qq_pcllib$definefwdlabel(lab_d);
    qq_pclgen$unstacklooplabels();
}

static void qq_pclgen$do_case(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pindex,struct qq_decls$unitrec *pwhenthen,i64 res) {
        i64 lab_a;
        i64 lab_d;
        i64 loopsw;
        i64 labnextwhen;
        i64 labstmtstart;
        i64 fmult;
        struct qq_decls$unitrec *  w;
        struct qq_decls$unitrec *  wt;
        struct qq_decls$unitrec *  pelse;
    if (((i64)(*pindex).tag == (i64)0)) {
        qq_pclgen$do_case_nc(p,pindex,pwhenthen,res);
        return;
    }
;
    loopsw = (i64)(((i64)(*p).tag == (i64)11) || ((i64)(*p).tag == (i64)13));
    pelse = (*pindex).nextunit;
    if (!!(loopsw)) {
        lab_a = qq_pcllib$definelabel();
        lab_d = qq_pcllib$createfwdlabel();
        qq_pclgen$stacklooplabels(lab_a,lab_a,lab_d);
    }
    else {
        lab_d = qq_pcllib$createfwdlabel();
    }
;
    qq_pclgen$evalunit(pindex,(i64)1);
    wt = pwhenthen;
    L671 :;
    while (!!(wt)) {
        w = (*wt).a;
        fmult = (i64)((*w).nextunit != 0);
        labnextwhen = qq_pcllib$createfwdlabel();
        if (!!(fmult)) {
            labstmtstart = qq_pcllib$createfwdlabel();
        }
;
        L674 :;
        while (!!(w)) {
            qq_pclgen$evalunit(w,(i64)1);
            w = (*w).nextunit;
            if (!!(w)) {
                qq_pcllib$genpc_lab((i64)39,labstmtstart);
            }
            else {
                qq_pcllib$genpc_lab((i64)40,labnextwhen);
            }
;
L675 :;
        }
L676 :;
        ;
        if (!!(fmult)) {
            qq_pcllib$definefwdlabel(labstmtstart);
        }
;
        qq_pclgen$evalunit((*wt).b,res);
        if (!(!!(loopsw))) {
            qq_pclgen$genjumpl(lab_d);
        }
        else {
            qq_pclgen$genjumpl(lab_a);
        }
;
        qq_pcllib$definefwdlabel(labnextwhen);
        wt = (*wt).nextunit;
L672 :;
    }
L673 :;
    ;
    qq_pcllib$genpc_n((i64)57,(i64)1);
    if (!!(pelse)) {
        qq_pclgen$evalunit(pelse,res);
    }
    else if (!!(res)) {
        qq_pcllib$genpc((i64)14);
    }
;
    if (!!(loopsw)) {
        qq_pclgen$genjumpl(lab_a);
        qq_pcllib$definefwdlabel(lab_d);
        qq_pclgen$unstacklooplabels();
    }
    else {
        qq_pcllib$definefwdlabel(lab_d);
    }
;
}

static void qq_pclgen$do_case_nc(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pindex,struct qq_decls$unitrec *pwhenthen,i64 res) {
        i64 lab_d;
        i64 labnextwhen;
        i64 labstmtstart;
        i64 fmult;
        struct qq_decls$unitrec *  w;
        struct qq_decls$unitrec *  wt;
        struct qq_decls$unitrec *  pelse;
    if (((i64)(*p).tag != (i64)10)) {
        qq_lib$gerror((byte*)"case-nc",0);
    }
;
    pelse = (*pindex).nextunit;
    lab_d = qq_pcllib$createfwdlabel();
    wt = pwhenthen;
    L677 :;
    while (!!(wt)) {
        w = (*wt).a;
        fmult = (i64)((*w).nextunit != 0);
        labnextwhen = qq_pcllib$createfwdlabel();
        if (!!(fmult)) {
            labstmtstart = qq_pcllib$createfwdlabel();
        }
;
        L680 :;
        while (!!(w)) {
            qq_pclgen$evalunit(w,(i64)1);
            w = (*w).nextunit;
            if (!!(w)) {
                qq_pcllib$genpc_lab((i64)31,labstmtstart);
            }
            else {
                qq_pcllib$genpc_lab((i64)31,labnextwhen);
            }
;
L681 :;
        }
L682 :;
        ;
        if (!!(fmult)) {
            qq_pcllib$definefwdlabel(labstmtstart);
        }
;
        qq_pclgen$evalunit((*wt).b,res);
        qq_pclgen$genjumpl(lab_d);
        qq_pcllib$definefwdlabel(labnextwhen);
        wt = (*wt).nextunit;
L678 :;
    }
L679 :;
    ;
    if (!!(pelse)) {
        qq_pclgen$evalunit(pelse,res);
    }
    else if (!!(res)) {
        qq_lib$gerror((byte*)"Needs Else branch",0);
    }
;
    qq_pcllib$definefwdlabel(lab_d);
}

static void qq_pclgen$do_try(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,struct qq_decls$unitrec *b) {
        i64 labend;
        i64 labx;
        struct qq_decls$unitrec *  ptry;
        struct qq_decls$unitrec *  pexcept;
        struct qq_decls$unitrec *  pexcode;
    ++(qq_pclgen$trylevel);
    labend = qq_pcllib$createfwdlabel();
    ptry = a;
    labx = qq_pcllib$createfwdlabel();
    pexcept = b;
    if ((pexcept == 0)) {
        qq_lib$gerror((byte*)"try: no except",0);
    }
    else if (!!((*pexcept).nextunit)) {
        qq_lib$gerror((byte*)"Try:multiple except block not implemented",0);
    }
;
    L683 :;
    while (!!(pexcept)) {
        pexcode = (*pexcept).a;
        if (((pexcode == 0) || !!((*pexcode).nextunit))) {
            qq_lib$gerror((byte*)"Try:multiple except codes not implemented",0);
        }
;
        qq_pcllib$genpc_lab((i64)141,labx);
        qq_pcllib$genxy(qq_pclgen$getconstvalue(pexcode),(i64)1);
        qq_pclgen$evalunit(ptry,(i64)0);
        qq_pclgen$genjumpl(labend);
        qq_pcllib$definefwdlabel(labx);
        qq_pclgen$evalunit((*pexcept).b,(i64)0);
        qq_pcllib$definefwdlabel(labend);
        pexcept = (*pexcept).nextunit;
L684 :;
    }
L685 :;
    ;
    qq_pcllib$genpc_n((i64)58,(i64)1);
    --(qq_pclgen$trylevel);
}

static i64 qq_pclgen$unitstoarray(struct qq_decls$unitrec *p,struct qq_decls$unitrec *(*plist)[],i64 maxunits) {
        i64 n;
    n = (i64)0;
    L686 :;
    while (!!(p)) {
        if ((n >= maxunits)) {
            qq_lib$gerror((byte*)"UTA Too many units",0);
        }
;
        (*plist)[(++(n))-1] = p;
        p = (*p).nextunit;
L687 :;
    }
L688 :;
    ;
    return n;
}

static void qq_pclgen$do_select(struct qq_decls$unitrec *pindex,struct qq_decls$unitrec *pplist,i64 res) {
        i64 n;
        i64 labend;
        i64 i;
        i64 lab;
        i64 elselab;
        struct qq_decls$unitrec *  x;
        struct qq_decls$unitrec *  pelse;
        struct qq_decls$unitrec *  plist[512];
        struct qq_pcltabs$pclrec *  labels[513];
    pelse = (*pindex).nextunit;
    n = qq_pclgen$unitstoarray(pplist,(struct qq_decls$unitrec *(*)[])&plist,(i64)512);
    if ((n > (i64)512)) {
        qq_lib$gerror((byte*)"Selectx too complex",0);
    }
;
    labend = qq_pcllib$createfwdlabel();
    qq_pclgen$evalunit(pindex,(i64)1);
    qq_pcllib$genpc_xy((i64)42,(i64)1,n);
    for (i=(i64)1;i<=n;++i) {
L689 :;
        qq_pcllib$genpc_lab((i64)41,(i64)0);
        labels[(i)-1] = qq_pcllib$pccurr;
L690 :;
    }
L691 :;
    ;
    qq_pcllib$genpc_lab((i64)41,(i64)0);
    labels[((n + (i64)1))-1] = qq_pcllib$pccurr;
    i = (i64)1;
    for (i=(i64)1;i<=n;++i) {
L692 :;
        x = plist[(i)-1];
        lab = qq_pcllib$definelabel();
        (*labels[(i)-1]).labelno = lab;
        qq_pclgen$evalunit(x,res);
        qq_pclgen$genjumpl(labend);
L693 :;
    }
L694 :;
    ;
    elselab = qq_pcllib$definelabel();
    (*labels[((n + (i64)1))-1]).labelno = elselab;
    if (!!(pelse)) {
        qq_pclgen$evalunit(pelse,res);
    }
    else if (!!(res)) {
        qq_pcllib$genpc((i64)14);
    }
;
    qq_pcllib$genpc((i64)0);
    qq_pcllib$definefwdlabel(labend);
}

static void qq_pclgen$do_andl(struct qq_decls$unitrec *x,struct qq_decls$unitrec *y) {
        i64 a;
        i64 b;
    a = qq_pcllib$createfwdlabel();
    b = qq_pcllib$createfwdlabel();
    qq_pclgen$genjumpcond((i64)32,x,a);
    qq_pclgen$genjumpcond((i64)32,y,a);
    qq_pcllib$genpc_int((i64)13,(i64)1);
    qq_pclgen$genjumpl(b);
    qq_pcllib$definefwdlabel(a);
    qq_pcllib$genpc_int((i64)13,(i64)0);
    qq_pcllib$genpc((i64)0);
    qq_pcllib$definefwdlabel(b);
}

static void qq_pclgen$do_orl(struct qq_decls$unitrec *x,struct qq_decls$unitrec *y) {
        i64 a;
        i64 b;
    a = qq_pcllib$createfwdlabel();
    b = qq_pcllib$createfwdlabel();
    qq_pclgen$genjumpcond((i64)31,x,a);
    qq_pclgen$genjumpcond((i64)31,y,a);
    qq_pcllib$genpc_int((i64)13,(i64)0);
    qq_pclgen$genjumpl(b);
    qq_pcllib$definefwdlabel(a);
    qq_pcllib$genpc_int((i64)13,(i64)1);
    qq_pcllib$genpc((i64)0);
    qq_pcllib$definefwdlabel(b);
}

static void qq_pclgen$do_incr(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,i64 res) {
        struct qq_decls$strec *  d;
        i64 opc;
    opc = (((i64)(*p).tag == (i64)60) ? (i64)73 : (i64)72);
    if (!!(res)) {
        qq_pclgen$do_unaryref(a,opc);
    }
    else if (((i64)(*a).tag == (i64)39)) {
        d = (*a).def;
        if ((((i64)(*d).nameid == (i64)15) && !!(msysc$m_getdotindex((i64)(*d).flags,(i64)5)))) {
            qq_pclgen$do_unaryref(a,(i64)69);
        }
        else {
            qq_pcllib$genpc_name((i64)70,(struct qq_decls$strec *)(*a).def);
        }
;
    }
    else {
        qq_pclgen$do_unaryref(a,(i64)69);
    }
;
    (*qq_pcllib$pccurr).x = (!!((i64)(*p).flag) ? (i64)-1 : (i64)1);
}

static void qq_pclgen$do_callhost(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,i64 res) {
        i64 index;
        i64 isfunc;
        i64 nargs;
        i64 nparams;
        i64 fparams;
        struct qq_decls$unitrec *  plist[10];
        struct qq_decls$unitrec *  q;
        i64 $av_1;
        i64 i;
    index = (*p).index;
    isfunc = (i64)qq_tables$hostisfn[(index)];
    if ((!!(res) && !(!!(isfunc)))) {
        qq_lib$gerror((byte*)"Host proc not a function",0);
    }
;
    if (!!(isfunc)) {
        qq_pcllib$genpc((i64)14);
    }
;
    nargs = (i64)0;
    q = a;
    L695 :;
    while (!!(q)) {
        if ((nargs > (i64)10)) {
            qq_lib$gerror((byte*)"Too many host args",0);
        }
;
        plist[(++(nargs))-1] = q;
        q = (*q).nextunit;
L696 :;
    }
L697 :;
    ;
    nparams = nargs;
    if (((nparams == (i64)0) && !!((i64)qq_tables$hostlvset[(index)]))) {
        qq_lib$gerror((byte*)"LV hostfn: needs 1+ params",0);
    }
;
    fparams = (i64)qq_tables$hostnparams[(index)];
    if ((nparams > fparams)) {
        qq_lib$gerror((byte*)"Hostfn too many params",0);
    }
;
    $av_1 = (fparams - nparams);
    while ($av_1-- > 0) {
L698 :;
        qq_pcllib$genpc((i64)14);
L699 :;
    }
L700 :;
    ;
    for (i=nparams;i>=(i64)1;--i) {
L701 :;
        if (((i == (i64)1) && !!((i64)qq_tables$hostlvset[(index)]))) {
            qq_pclgen$evalref(plist[(i)-1]);
        }
        else {
            qq_pclgen$evalunit(plist[(i)-1],(i64)1);
        }
;
L702 :;
    }
L703 :;
    ;
    qq_pclgen$callhostfn(index,res);
}

static void qq_pclgen$callhostfn(i64 fnindex,i64 calledasfn) {
    qq_pcllib$genpc_int((i64)56,fnindex);
}

static void qq_pclgen$genfree(i64 n) {
    qq_pcllib$genpc_n((i64)57,n);
}

static void qq_pclgen$do_return(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a) {
    if (!!(a)) {
        if (!(!!(msysc$m_getdotindex((i64)(*qq_decls$stcurrproc).flags,(i64)4)))) {
            qq_lib$gerror((byte*)"Proc can't return a value",0);
        }
;
        qq_pclgen$evalunit(a,(i64)1);
    }
    else {
        if (!!(msysc$m_getdotindex((i64)(*qq_decls$stcurrproc).flags,(i64)4))) {
            qq_lib$gerror((byte*)"Func needs return value",0);
        }
;
    }
;
    qq_pclgen$genjumpl(qq_pclgen$retindex);
}

static void qq_pclgen$do_multassign(struct qq_decls$unitrec *a,struct qq_decls$unitrec *b,i64 deepcopy,i64 res) {
        struct qq_decls$unitrec *  p;
        struct qq_decls$unitrec *  q;
        struct qq_decls$unitrec *  plist[100];
        i64 n;
        i64 i;
    p = (*a).a;
    q = (*b).a;
    n = (i64)0;
    L704 :;
    while (!!(p)) {
        if ((q == 0)) {
            qq_lib$gerror((byte*)"Too few RHS elems",0);
        }
;
        qq_pclgen$evalunit(q,(i64)1);
        if ((n >= (i64)100)) {
            qq_lib$gerror((byte*)"Too many elems",0);
        }
;
        plist[(++(n))-1] = p;
        p = (*p).nextunit;
        q = (*q).nextunit;
L705 :;
    }
L706 :;
    ;
    if (!!(q)) {
        qq_lib$gerror((byte*)"Too few LHS elems",0);
    }
;
    for (i=n;i>=(i64)1;--i) {
L707 :;
        if (!!(deepcopy)) {
            qq_pcllib$genpc((i64)26);
        }
;
        qq_pclgen$do_store(plist[(i)-1],(i64)0);
L708 :;
    }
L709 :;
    ;
}

static void qq_pclgen$do_store(struct qq_decls$unitrec *a,i64 res) {
        struct qq_decls$strec *  d;
        struct qq_decls$unitrec *  p;
        struct qq_decls$unitrec *  plist[100];
        i64 n;
        i64 i;
    if ((!!(res) && ((i64)(*a).tag != (i64)39))) {
        qq_pcllib$genpc((i64)25);
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)39)) {
        d = (*a).def;
        if ((((i64)(*d).nameid == (i64)15) && !!(msysc$m_getdotindex((i64)(*d).flags,(i64)5)))) {
            if (!!(res)) {
                qq_pcllib$genpc((i64)25);
            }
;
            qq_pcllib$genpc_name((i64)7,(struct qq_decls$strec *)d);
            qq_pcllib$genpc((i64)22);
        }
        else if (!!(res)) {
            qq_pcllib$genpc((i64)25);
            qq_pcllib$genpc_name((i64)11,(struct qq_decls$strec *)d);
        }
        else if (((i64)(*d).nameid == (i64)5 || (i64)(*d).nameid == (i64)7)) {
            qq_lib$gerror((byte*)"Not lvalue",0);
        }
        else {
            qq_pcllib$genpc_name((i64)11,(struct qq_decls$strec *)d);
        }
;
    }
    else if (($temp==(i64)46)) {
        qq_pclgen$evalunit((*a).a,(i64)1);
        qq_pcllib$genpc_name((i64)128,(struct qq_decls$strec *)(*(*a).b).def);
    }
    else if (($temp==(i64)47)) {
        qq_pclgen$do_bin((*a).a,(*a).b,(i64)132);
    }
    else if (($temp==(i64)48)) {
        qq_pclgen$evalref((*a).a);
        qq_pclgen$evalunit((*a).b,(i64)1);
        qq_pcllib$genpc((i64)138);
    }
    else if (($temp==(i64)51)) {
        qq_pclgen$evalunit((*a).a,(i64)1);
        qq_pcllib$genpc((i64)22);
    }
    else if (($temp==(i64)49)) {
        qq_pclgen$do_bin((*a).a,(*a).b,(i64)135);
    }
    else if (($temp==(i64)88)) {
        n = (i64)0;
        p = (*a).a;
        L710 :;
        while (!!(p)) {
            if ((n >= (i64)100)) {
                qq_lib$gerror((byte*)"Too many elems",0);
            }
;
            plist[(++(n))-1] = p;
            p = (*p).nextunit;
L711 :;
        }
L712 :;
        ;
        if ((n == (i64)0)) {
            qq_lib$gerror((byte*)"Empty lhs list",0);
        }
;
        qq_pcllib$genpc_n((i64)140,n);
        for (i=(i64)1;i<=n;++i) {
L713 :;
            qq_pclgen$do_store(plist[(i)-1],(i64)0);
L714 :;
        }
L715 :;
        ;
    }
    else if (($temp==(i64)7)) {
        qq_pclgen$evalref(a);
        qq_pcllib$genpc((i64)22);
    }
    else {
        qq_lib$gerror_s((byte*)"Can't store to this unit yet:",qq_tables$jtagnames[((i64)(*a).tag)],a);
    }
    };
}

static i64 qq_pclgen$getconstvalue(struct qq_decls$unitrec *p) {
    if ((!!(p) && ((i64)(*p).tag == (i64)41))) {
        return (*p).value;
    }
;
    qq_lib$gerror((byte*)"gcv Not const",0);
    return (i64)0;
}

static void qq_pclgen$do_convert(struct qq_decls$unitrec *pconv) {
        i64 n;
        i64 elemmode;
        i64 i;
        i64 lowerx;
        i64 m;
        i64 mbase;
        i64 nfields;
        struct qq_decls$unitrec *  plist[400];
        struct qq_decls$unitrec *  p;
        i64 $av_1;
    m = (i64)(*pconv).mode;
    p = (*pconv).a;
    mbase = (i64)qq_tables$ttbasetype[(m)];
    if ((((i64)(*p).tag != (i64)88) || (mbase == (i64)16))) {
        if (((i64)(*p).tag == (i64)88)) {
            qq_lib$deleteunit(p,(*p).a);
        }
;
        qq_pclgen$evalunit(p,(i64)1);
        qq_pcllib$genpc_int((i64)98,m);
        return;
    }
;
    n = qq_pclgen$unitstoarray((*p).a,(struct qq_decls$unitrec *(*)[])&plist,(i64)400);
    if ((!!(n) && ((i64)(*plist[((i64)1)-1]).tag == (i64)4))) {
        if ((mbase==(i64)12) || (mbase==(i64)13)) {
            qq_pclgen$do_makerecordkv(m,n,&plist);
        }
        else {
            qq_lib$gerror((byte*)"key:value not allowed",0);
        }
;
        return;
    }
;
    for (i=(i64)1;i<=n;++i) {
L716 :;
        qq_pclgen$evalunit(plist[(i)-1],(i64)1);
L717 :;
    }
L718 :;
    ;
    if ((mbase==(i64)12) || (mbase==(i64)13)) {
        nfields = qq_tables$ttlength[(m)];
        if (!!(n)) {
            qq_pclgen$checkelems(n,nfields,p);
        }
        else {
            $av_1 = nfields;
            while ($av_1-- > 0) {
L719 :;
                qq_pcllib$genpc_int((i64)13,(i64)0);
L720 :;
            }
L721 :;
            ;
            n = nfields;
        }
;
        qq_pcllib$genpc_xy(((mbase == (i64)12) ? (i64)61 : (i64)64),n,(i64)0);
        (*qq_pcllib$pccurr).usertag = m;
    }
    else if ((mbase==(i64)10)) {
        lowerx = (i64)(*p).lower;
        qq_pcllib$genpc_xy((i64)60,n,lowerx);
    }
    else if ((mbase==(i64)11)) {
        qq_pcllib$genpc_xy((i64)62,n,(i64)(*p).lower);
        (*qq_pcllib$pccurr).usertag = (i64)11;
        (*qq_pcllib$pccurr).usertag2 = (i64)(*p).elemtype;
    }
    else if ((mbase==(i64)7)) {
        elemmode = (i64)qq_tables$tttarget[(m)];
        lowerx = qq_tables$ttlower[(m)];
        qq_pclgen$checkelems(n,qq_tables$ttlength[(m)],p);
        qq_pcllib$genpc_xy((i64)62,lowerx,n);
        (*qq_pcllib$pccurr).usertag = m;
        (*qq_pcllib$pccurr).usertag2 = elemmode;
    }
    else if ((mbase==(i64)8)) {
        if ((m == (i64)8)) {
            qq_pcllib$genpc_xy((i64)63,n,(i64)(*p).lower);
            (*qq_pcllib$pccurr).usertag = (i64)8;
            (*qq_pcllib$pccurr).usertag2 = (((i64)(*p).elemtype == (i64)0) ? (i64)33 : (i64)(*p).elemtype);
        }
        else {
            qq_lib$gerror((byte*)"user-define bit array not ready",0);
        }
;
    }
    else if ((mbase==(i64)5)) {
        qq_pcllib$genpc_xy((i64)65,n,(i64)0);
    }
    else {
        qq_lib$gerror_s((byte*)"Convert list",qq_show$strmode(mbase,(i64)0),0);
    }
;
}

static void qq_pclgen$checkelems(i64 n,i64 length,struct qq_decls$unitrec *p) {
    if ((n < length)) {
        qq_lib$gerror((byte*)"Too few elements",0);
    }
    else if ((n > length)) {
        qq_lib$gerror((byte*)"Too many elements",0);
    }
;
}

static void qq_pclgen$do_switch(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pindex,struct qq_decls$unitrec *pwhenthen,i64 res) {
        i64 minlab;
        i64 maxlab;
        i64 x;
        i64 y;
        i64 i;
        i64 n;
        struct qq_decls$unitrec *  w;
        struct qq_decls$unitrec *  wt;
        struct qq_decls$unitrec *  pelse;
    pelse = (*pindex).nextunit;
    minlab = (i64)1000000;
    maxlab = (i64)-1000000;
    n = (i64)0;
    wt = pwhenthen;
    L722 :;
    while (!!(wt)) {
        w = (*wt).a;
        L725 :;
        while (!!(w)) {
                        {i64 $temp = (i64)(*w).tag;
if (($temp==(i64)87)) {
                x = qq_pclgen$getconstvalue((*w).a);
                y = qq_pclgen$getconstvalue((*w).b);
                //dorange:
L728 :;
;
                for (i=x;i<=y;++i) {
L729 :;
                    minlab = (minlab<i?minlab:i);
                    maxlab = (maxlab>i?maxlab:i);
L730 :;
                }
L731 :;
                ;
            }
            else if (($temp==(i64)41)) {
                x = (y = (*w).value);
                goto L728 ;
;
            }
            else if (($temp==(i64)34)) {
                x = (y = (i64)(*w).mode);
                goto L728 ;
;
            }
            else {
                qq_lib$gerror_s((byte*)"Switch when2: not const",(*qq_lib$strexpr(w)).strptr,0);
            }
            };
            w = (*w).nextunit;
L726 :;
        }
L727 :;
        ;
        wt = (*wt).nextunit;
L723 :;
    }
L724 :;
    ;
    if (((maxlab - minlab) <= (i64)512)) {
        qq_pclgen$do_simpleswitch(p,pindex,pwhenthen,pelse,minlab,maxlab,res);
        return;
    }
;
    qq_lib$gerror((byte*)"COMPLEX SWITCH/NOT COMPLETE",0);
}

static void qq_pclgen$do_simpleswitch(struct qq_decls$unitrec *p,struct qq_decls$unitrec *pindex,struct qq_decls$unitrec *pwhenthen,struct qq_decls$unitrec *pelse,i64 a,i64 b,i64 res) {
        struct qq_decls$unitrec *  w;
        struct qq_decls$unitrec *  wt;
        i64 loopsw;
        i64 n;
        i64 offset;
        i64 x;
        i64 y;
        i64 x0;
        i64 i;
        i64 labstmt;
        i64 elselab;
        struct qq_pcltabs$pclrec *  labels[513];
        i64 lab_a;
        i64 lab_d;
    loopsw = (i64)((i64)(*p).tag == (i64)13);
    n = ((b - a) + (i64)1);
    offset = (a - (i64)1);
    if (!!(loopsw)) {
        lab_a = qq_pcllib$definelabel();
        lab_d = qq_pcllib$createfwdlabel();
        qq_pclgen$stacklooplabels(lab_a,lab_a,lab_d);
    }
    else {
        lab_d = qq_pcllib$createfwdlabel();
    }
;
    elselab = qq_pcllib$createfwdlabel();
    qq_pclgen$evalunit(pindex,(i64)1);
    qq_pcllib$genpc_xy((i64)42,a,b);
    for (i=(i64)1;i<=n;++i) {
L732 :;
        qq_pcllib$genpc_lab((i64)41,(i64)0);
        labels[(i)-1] = qq_pcllib$pccurr;
L733 :;
    }
L734 :;
    ;
    qq_pcllib$genpc_lab((i64)41,(i64)0);
    labels[((n + (i64)1))-1] = qq_pcllib$pccurr;
    wt = pwhenthen;
    L735 :;
    while (!!(wt)) {
        labstmt = qq_pcllib$definelabel();
        w = (*wt).a;
        L738 :;
        while (!!(w)) {
                        {i64 $temp = (i64)(*w).tag;
if (($temp==(i64)87)) {
                x0 = qq_pclgen$getconstvalue((*w).a);
                y = qq_pclgen$getconstvalue((*w).b);
            }
            else if (($temp==(i64)41)) {
                x0 = (y = (*w).value);
            }
            else if (($temp==(i64)34)) {
                x0 = (y = (i64)(*w).mode);
            }
            };
            for (x=x0;x<=y;++x) {
L741 :;
                i = (x - offset);
                if (!!((*labels[(i)-1]).labelno)) {
                    msysc$m_print_startcon();
                    msysc$m_print_i64(x,NULL);
                    msysc$m_print_c8((u64)(u8)x,NULL);
                    msysc$m_print_newline();
                    msysc$m_print_end();
                    ;
                    qq_lib$gerror((byte*)"Dupl switch value",0);
                }
;
                (*labels[(i)-1]).labelno = labstmt;
L742 :;
            }
L743 :;
            ;
            w = (*w).nextunit;
L739 :;
        }
L740 :;
        ;
        qq_pclgen$evalunit((*wt).b,res);
        if (!(!!(loopsw))) {
            qq_pclgen$genjumpl(lab_d);
        }
        else {
            qq_pclgen$genjumpl(lab_a);
        }
;
        wt = (*wt).nextunit;
L736 :;
    }
L737 :;
    ;
    qq_pcllib$definefwdlabel(elselab);
    if (!!(pelse)) {
        qq_pclgen$evalunit(pelse,res);
    }
;
    if (!!(loopsw)) {
        qq_pclgen$genjumpl(lab_a);
        qq_pcllib$definefwdlabel(lab_d);
        qq_pclgen$unstacklooplabels();
    }
    else {
        qq_pcllib$definefwdlabel(lab_d);
    }
;
    for (i=(i64)1;i<=n;++i) {
L744 :;
        if (((*labels[(i)-1]).labelno == (i64)0)) {
            (*labels[(i)-1]).labelno = elselab;
        }
;
L745 :;
    }
L746 :;
    ;
    (*labels[((n + (i64)1))-1]).labelno = elselab;
}

static void qq_pclgen$do_makerecordkv(i64 m,i64 nkeyvals,struct qq_decls$unitrec *(*kvlist)[]) {
        struct qq_decls$unitrec *  p;
        struct qq_decls$unitrec *  plist[400];
        i64 nfields;
        i64 index;
        struct qq_decls$strec *  d;
        struct qq_decls$strec *  e;
        struct qq_decls$strec *  f;
        struct qq_decls$strec *  k;
        i64 i;
    d = qq_tables$ttnamedef[(m)];
    e = (*d).deflist;
    nfields = (i64)0;
    L747 :;
    while (!!(e)) {
        if ((((i64)(*e).nameid == (i64)11 || (i64)(*e).nameid == (i64)12) && ((*e).atfield == 0))) {
            ++(nfields);
            plist[(nfields)-1] = 0;
        }
;
L748 :;
        e = (*e).nextdef;
L750 :;
            }
L749 :;
    ;
    for (i=(i64)1;i<=nkeyvals;++i) {
L751 :;
        k = (*(*(*kvlist)[(i)-1]).a).def;
        p = (*(*kvlist)[(i)-1]).b;
        e = (*d).deflist;
        f = 0;
        L754 :;
        while (!!(e)) {
            if ((((i64)(*e).nameid == (i64)11 || (i64)(*e).nameid == (i64)12) && ((*e).firstdupl == k))) {
                f = e;
                goto L756 ;
            }
;
L755 :;
            e = (*e).nextdef;
L757 :;
                    }
L756 :;
        ;
        if (!(!!(f))) {
            qq_lib$gerror_s((byte*)"Can't find field:",(*k).name,0);
        }
;
        index = (i64)(*f).index;
        if (!!(plist[(index)-1])) {
            qq_lib$gerror_s((byte*)"Dupl key:",(*k).name,0);
        }
;
        plist[(index)-1] = p;
L752 :;
    }
L753 :;
    ;
    for (i=(i64)1;i<=nfields;++i) {
L758 :;
        if (!!(plist[(i)-1])) {
            qq_pclgen$evalunit(plist[(i)-1],(i64)1);
        }
        else {
            qq_pcllib$genpc_int((i64)13,(i64)0);
        }
;
L759 :;
    }
L760 :;
    ;
    qq_pcllib$genpc_xy((i64)61,nfields,(i64)0);
    (*qq_pcllib$pccurr).usertag = m;
}

static void qq_pclgen$do_idiv(struct qq_decls$unitrec *a,struct qq_decls$unitrec *b) {
        i64 n;
    qq_pclgen$evalunit(a,(i64)1);
    if ((((i64)(*b).tag == (i64)41) && !!((n = qq_lib$ispoweroftwo((*b).value))))) {
        qq_pcllib$genpc_int((i64)13,n);
        qq_pcllib$genpc((i64)111);
    }
    else {
        qq_pclgen$evalunit(b,(i64)1);
        qq_pcllib$genpc((i64)104);
    }
;
}

static void qq_pclgen$do_irem(struct qq_decls$unitrec *a,struct qq_decls$unitrec *b) {
        i64 n;
        u64 m;
    qq_pclgen$evalunit(a,(i64)1);
    if ((((i64)(*b).tag == (i64)41) && !!((n = qq_lib$ispoweroftwo((*b).value))))) {
        m = ~(((u64)18446744073709551615u << n));
        qq_pcllib$genpc_int((i64)13,(i64)m);
        qq_pcllib$genpc((i64)107);
    }
    else {
        qq_pclgen$evalunit(b,(i64)1);
        qq_pcllib$genpc((i64)105);
    }
;
}

static void qq_pclgen$do_map(struct qq_decls$unitrec *p,struct qq_decls$unitrec *popcode,struct qq_decls$unitrec *x) {
        i64 lab;
    qq_pclgen$evalunit(x,(i64)1);
    if (!!((*x).nextunit)) {
        qq_pclgen$evalunit((*x).nextunit,(i64)1);
    }
;
    qq_pclgen$evalunit(popcode,(i64)1);
    qq_pcllib$genpc((i64)143);
    lab = qq_pcllib$createfwdlabel();
    qq_pcllib$genpc_lab((i64)29,lab);
    qq_pcllib$genpc((i64)0);
    qq_pcllib$definefwdlabel(lab);
}

static void qq_pclgen$pushstring(u8 *s) {
    qq_pcllib$genpc((i64)17);
    qq_pcllib$genopnd_strz(s);
}

static i64 qq_pclgen$checkblockreturn(struct qq_decls$unitrec *p) {
        struct qq_decls$unitrec *  q;
        struct qq_decls$unitrec *  r;
    if ((p == 0)) {
        return (i64)0;
    }
;
        {i64 $temp = (i64)qq_tables$jhasvalue[((i64)(*p).tag)];
if (($temp==(i64)0)) {
        return (i64)0;
    }
    else if (($temp==(i64)1)) {
        return (i64)1;
    }
    };
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)6)) {
        q = (struct qq_decls$unitrec *)(*p).a;
        if ((q == 0)) {
            return (i64)0;
        }
;
        L761 :;
        while (!!((r = (struct qq_decls$unitrec *)(*q).nextunit))) {
            q = r;
L762 :;
        }
L763 :;
        ;
        return qq_pclgen$checkblockreturn((struct qq_decls$unitrec *)q);
    }
    else if (($temp==(i64)7)) {
        return (i64)(!!(qq_pclgen$checkblockreturn((*p).b)) && !!(qq_pclgen$checkblockreturn((*(*p).b).nextunit)));
    }
    else {
        return (i64)1;
    }
    };
    return (i64)0;
}

// START
void qq_pclgen$start(void) {

}

// START
void qq_pcllib$start(void) {

    mlib$pcm_init();
    qq_pcllib$labelalloc = (i64)8192;
    qq_pcllib$labelpctable = (struct qq_pcltabs$pclrec *(*)[])mlib$pcm_alloc(((i64)8 * qq_pcllib$labelalloc));
}

void qq_pcllib$resetpcl(i64 sourcesize) {
        i64 pclsize;
    qq_decls$qpos = (i64)0;
    qq_pcllib$nextlabelno = (i64)0;
    qq_pcllib$pclcurrlineno = (i64)0;
    pclsize = sourcesize;
    qq_pcllib$pcalloc = (i64)1024;
    L764 :;
    while ((qq_pcllib$pcalloc < pclsize)) {
        qq_pcllib$pcalloc <<= (i64)1;
L765 :;
    }
L766 :;
    ;
    qq_pcllib$pcstart = (struct qq_pcltabs$pclrec *)mlib$pcm_allocz((qq_pcllib$pcalloc * (i64)32));
    qq_pcllib$pccurr = (qq_pcllib$pcstart - (i64)1);
    qq_pcllib$pcend = ((qq_pcllib$pcstart + qq_pcllib$pcalloc) - (i64)8);
    qq_pcllib$pcsourcestart = (i32 *)mlib$pcm_alloc((qq_pcllib$pcalloc * (i64)4));
    qq_pcllib$pcsourcecurr = qq_pcllib$pcsourcestart;
    mlib$pcm_clearmem(qq_pcllib$labelpctable,((i64)8 * qq_pcllib$labelalloc));
}

void qq_pcllib$genpc(i64 opc) {
    ++(qq_pcllib$pccurr);
    if ((qq_pcllib$pccurr >= qq_pcllib$pcend)) {
        qq_pcllib$extendpcldata();
    }
;
    (*qq_pcllib$pccurr).opcode = opc;
    ++(qq_pcllib$pcsourcecurr);
    (*qq_pcllib$pcsourcecurr) = qq_decls$qpos;
}

void qq_pcllib$genpc_int(i64 opc,i64 a) {
    qq_pcllib$genpc(opc);
    (*qq_pcllib$pccurr).value = a;
}

void qq_pcllib$genpc_n(i64 opc,i64 n) {
    qq_pcllib$genpc(opc);
    (*qq_pcllib$pccurr).n = n;
}

void qq_pcllib$genpc_xy(i64 opc,i64 x,i64 y) {
    qq_pcllib$genpc(opc);
    (*qq_pcllib$pccurr).x = x;
    (*qq_pcllib$pccurr).y = y;
}

void qq_pcllib$genpc_name(i64 opc,struct qq_decls$strec *d) {
    qq_pcllib$genpc(opc);
    (*qq_pcllib$pccurr).def = (struct qq_decls$strec *)d;
}

void qq_pcllib$genopnd_strz(u8 *s) {
    (*qq_pcllib$pccurr).svalue = s;
}

void qq_pcllib$genopnd_str(struct qq_decls$objrec *s) {
    (*qq_pcllib$pccurr).objptr = s;
}

void qq_pcllib$genopnd_obj(struct qq_decls$objrec *p) {
    (*qq_pcllib$pccurr).objptr = p;
}

void qq_pcllib$genpc_real(i64 opc,r64 x) {
    qq_pcllib$genpc(opc);
    (*qq_pcllib$pccurr).xvalue = x;
}

void qq_pcllib$genpc_lab(i64 opc,i64 lab) {
    qq_pcllib$genpc(opc);
    (*qq_pcllib$pccurr).labelno = lab;
}

void qq_pcllib$gencomment(u8 *s) {
    qq_pcllib$genpc((i64)6);
    qq_pcllib$genopnd_strz(mlib$pcm_copyheapstring(s));
}

static void qq_pcllib$extendpcldata(void) {
        i64 newpcalloc;
        struct qq_pcltabs$pclrec *  newpcstart;
        i32 *  newpcsourcestart;
    newpcalloc = (qq_pcllib$pcalloc * (i64)2);
    newpcstart = (struct qq_pcltabs$pclrec *)mlib$pcm_alloc(((i64)32 * newpcalloc));
    newpcsourcestart = (i32 *)mlib$pcm_alloc(((i64)4 * newpcalloc));
    memcpy(newpcstart,qq_pcllib$pcstart,(u64)((qq_pcllib$pccurr - qq_pcllib$pcstart) * (i64)32));
    memcpy(newpcsourcestart,qq_pcllib$pcsourcestart,(u64)((qq_pcllib$pccurr - qq_pcllib$pcstart) * (i64)4));
    qq_pcllib$pccurr = (newpcstart + (qq_pcllib$pccurr - qq_pcllib$pcstart));
    qq_pcllib$pcend = ((newpcstart + newpcalloc) - (i64)10);
    qq_pcllib$pcsourcecurr = (newpcsourcestart + (qq_pcllib$pcsourcecurr - qq_pcllib$pcsourcestart));
    mlib$pcm_free(qq_pcllib$pcstart,(qq_pcllib$pcalloc * (i64)32));
    mlib$pcm_free(qq_pcllib$pcsourcestart,(qq_pcllib$pcalloc * (i64)4));
    qq_pcllib$pcstart = newpcstart;
    qq_pcllib$pcalloc = newpcalloc;
    qq_pcllib$pcsourcestart = newpcsourcestart;
}

void qq_pcllib$extendlabeltable(void) {
        i64 newlabelalloc;
        struct qq_pcltabs$pclrec *(*newlabeltable)[];
    newlabelalloc = (qq_pcllib$labelalloc * (i64)2);
    newlabeltable = (struct qq_pcltabs$pclrec *(*)[])mlib$pcm_alloc(((i64)8 * newlabelalloc));
    memcpy(newlabeltable,qq_pcllib$labelpctable,(u64)(qq_pcllib$labelalloc * (i64)8));
    mlib$pcm_free(qq_pcllib$labelpctable,(qq_pcllib$labelalloc * (i64)8));
    qq_pcllib$labelpctable = (struct qq_pcltabs$pclrec *(*)[])newlabeltable;
    qq_pcllib$labelalloc = newlabelalloc;
}

i64 qq_pcllib$definelabel(void) {
    if ((qq_pcllib$nextlabelno >= qq_pcllib$labelalloc)) {
        qq_pcllib$extendlabeltable();
    }
;
    ++(qq_pcllib$nextlabelno);
    (*qq_pcllib$labelpctable)[(qq_pcllib$nextlabelno)-1] = (qq_pcllib$pccurr + (i64)1);
    return qq_pcllib$nextlabelno;
}

i64 qq_pcllib$createfwdlabel(void) {
    if ((qq_pcllib$nextlabelno >= qq_pcllib$labelalloc)) {
        qq_pcllib$extendlabeltable();
    }
;
    ++(qq_pcllib$nextlabelno);
    (*qq_pcllib$labelpctable)[(qq_pcllib$nextlabelno)-1] = 0;
    return qq_pcllib$nextlabelno;
}

void qq_pcllib$definefwdlabel(i64 lab) {
    if (!!((*qq_pcllib$labelpctable)[(lab)-1])) {
        qq_lib$serror((byte*)"dupl label?");
    }
;
    (*qq_pcllib$labelpctable)[(lab)-1] = (qq_pcllib$pccurr + (i64)1);
}

void qq_pcllib$genxy(i64 x,i64 y) {
    (*qq_pcllib$pccurr).x = x;
    (*qq_pcllib$pccurr).y = y;
}

void qq_print$pch_print(struct qq_decls$varrec *p,struct qq_decls$varrec *fmt) {
        struct qq_decls$varrec v;
        struct qq_decls$varrec emptyfmt;
    if ((fmt == 0)) {
        fmt = &emptyfmt;
        emptyfmt.tagx = (i64)0;
    }
;
    if ((qq_print$mfmtstr == 0)) {
        if (!!((i64)qq_print$mgapneeded)) {
            qq_print$printstr_n((byte*)" ",(i64)1);
        }
        else {
            qq_print$mgapneeded = (i64)1;
        }
;
    }
    else {
        qq_print$printnextfmtchars((i64)0);
    }
;
    qq_print$listdepth = (i64)0;
    qq_print$pch_tostr(p,fmt,&v);
    qq_print$printstr_n((*v.objptr).strptr,(*v.objptr).length);
    if (!!((i64)v.hasref)) {
        qq_vars$var_unshareu(&v);
    }
;
}

void qq_print$pch_print_nf(struct qq_decls$varrec *p) {
    qq_print$pch_print(p,0);
}

void qq_print$pch_printnogap(void) {
    qq_print$mgapneeded = (i64)0;
}

void qq_print$pch_println(void) {
    if (!!(qq_print$mfmtstr)) {
        qq_print$printnextfmtchars((i64)1);
    }
;
    qq_print$mgapneeded = (i64)0;
    qq_print$printstr_n((byte*)"\n",(i64)-1);
}

void qq_print$pch_reread(void) {
    qq_print$kb_pos = qq_print$kb_lastpos;
    qq_print$kb_length = qq_print$kb_lastlength;
}

void qq_print$pch_rereadln(void) {
    qq_print$kb_pos = qq_print$kb_start;
    qq_print$kb_length = qq_print$kb_linelength;
}

void qq_print$pch_startprint(struct qq_decls$varrec *p) {
        struct qq_decls$objrec *  s;
        {i64 $temp = ++(qq_print$noclevels);
if (($temp==(i64)0) || ($temp==(i64)1)) {
    }
    else if (($temp==(i64)7)) {
        qq_lib$prterror((byte*)"print #x overflow");
    }
    else {
        qq_print$moutdevstack[((qq_print$noclevels - (i64)1))] = qq_print$moutdev;
        qq_print$moutchanstack[((qq_print$noclevels - (i64)1))] = qq_print$moutchan;
        qq_print$moutvarstack[((qq_print$noclevels - (i64)1))] = qq_print$moutvar;
        qq_print$mfmtstrstack[((qq_print$noclevels - (i64)1))] = qq_print$mfmtstr;
        qq_print$mfmtcurrstack[((qq_print$noclevels - (i64)1))] = qq_print$mfmtcurr;
        qq_print$mgapstack[((qq_print$noclevels - (i64)1))] = (i64)qq_print$mgapneeded;
    }
    };
    qq_print$mfmtstr = 0;
    qq_print$mfmtcurr = 0;
    if ((p == 0)) {
        goto L767 ;
;
    }
;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)1)) {
        switch ((*p).value) {
        case 0:;
            {
                //doconsole:
L767 :;
;
                qq_print$moutdev = (i64)0;
                qq_print$moutchan = 0;
            }
            break;
        case 1:;
            {
                qq_print$moutdev = (i64)2;
                qq_print$moutchan = 0;
                qq_print$moutvar.tagx = (i64)265;
                s = qq_vars$obj_new();
                (*s).flags = msysc$m_setdotindex((*s).flags,(i64)1,(u64)1u);
                qq_print$moutvar.objptr = s;
            }
            break;
        case 2:;
            {
                if ((qq_print$testfilech == 0)) {
                    qq_lib$prterror((byte*)"@2: file not open");
                }
;
                qq_print$moutdev = (i64)1;
                qq_print$moutchan = qq_print$testfilech;
            }
            break;
        default: {
            qq_print$moutdev = (i64)1;
            qq_print$moutchan = (void *)(*p).value;
        }
        } //SW
;
    }
    else if (($temp==(i64)14)) {
        p = (*p).varptr;
                {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)9)) {
            qq_print$moutdev = (i64)4;
            qq_print$moutchan = 0;
            qq_print$moutvar.tagx = (i64)14;
            qq_print$moutvar.varptr = p;
        }
        else {
            msysc$m_print_startcon();
            msysc$m_print_str(qq_tables$ttname[((i64)(*p).tag)],NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            qq_lib$prterror((byte*)"Print@^?");
        }
        };
    }
    else {
                {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)12) || ($temp==(i64)13)) {
            qq_print$moutdev = (i64)0;
        }
        else {
            msysc$m_print_startcon();
            msysc$m_print_str(qq_tables$ttname[((i64)(*p).tag)],NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            qq_lib$prterror((byte*)"Can't do startprint...");
        }
        };
    }
    };
    qq_print$mgapneeded = (i64)0;
}

void qq_print$pch_startprintcon(void) {
        struct qq_decls$varrec v;
    v.tagx = (i64)1;
    v.value = (i64)0;
    qq_print$pch_startprint(&v);
}

void qq_print$pch_endprint(void) {
        struct qq_decls$varrec *  p;
    if (!!(qq_print$mfmtstr)) {
        qq_print$printnextfmtchars((i64)1);
    }
;
    if ((qq_print$moutdev==(i64)4)) {
        p = qq_print$moutvar.varptr;
    }
;
    if ((qq_print$mfmtstr != 0)) {
        mlib$pcm_free((void *)qq_print$mfmtstr,(strlen(qq_print$mfmtstr) + (i64)1));
    }
;
    if ((--(qq_print$noclevels) == (i64)-1)) {
        qq_lib$prterror((byte*)"resetoc??");
    }
;
    if ((qq_print$noclevels == (i64)0)) {
        qq_print$moutdev = (i64)0;
    }
    else {
        qq_print$moutdev = (i64)qq_print$moutdevstack[(qq_print$noclevels)];
        qq_print$moutchan = qq_print$moutchanstack[(qq_print$noclevels)];
        qq_print$moutvar = qq_print$moutvarstack[(qq_print$noclevels)];
        qq_print$mgapneeded = (i64)qq_print$mgapstack[(qq_print$noclevels)];
        qq_print$mfmtstr = qq_print$mfmtstrstack[(qq_print$noclevels)];
        qq_print$mfmtcurr = qq_print$mfmtcurrstack[(qq_print$noclevels)];
    }
;
    qq_print$mgapneeded = (i64)0;
}

void qq_print$pch_strstartprint(void) {
        struct qq_decls$varrec p;
    p.tagx = (i64)1;
    p.value = (i64)1;
    qq_print$pch_startprint(&p);
}

void qq_print$pch_strendprint(struct qq_decls$varrec *dest) {
    if (!!(qq_print$mfmtstr)) {
        qq_print$printnextfmtchars((i64)1);
    }
;
    if ((qq_print$moutdev != (i64)2)) {
        qq_lib$prterror((byte*)"STRENDPRT/NOT STR");
    }
;
    (*dest) = qq_print$moutvar;
    qq_print$moutvar.tagx = (i64)0;
    qq_print$pch_endprint();
}

void qq_print$pch_printspace(void) {
    qq_print$mgapneeded = (i64)0;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)" ",NULL);
    msysc$m_print_end();
    ;
}

void qq_print$pch_readln(struct qq_decls$varrec *dev) {
        void *  ch;
        i64 length;
        struct qq_decls$objrec *  pdev;
    if ((qq_print$kb_start == 0)) {
        qq_print$kb_start = (u8 *)mlib$pcm_alloc((i64)1048576);
        qq_print$kb_size = (i64)1048576;
        qq_print$kb_lastpos = qq_print$kb_start;
        qq_print$kb_pos = qq_print$kb_start;
        qq_print$kb_length = (i64)0;
        qq_print$kb_lastlength = (i64)0;
        qq_print$kb_linelength = (i64)0;
    }
;
        {i64 $temp = (i64)(*dev).tag;
if (($temp==(i64)0)) {
        //doconsole:
L768 :;
;
        mlib$readlinen(0,qq_print$kb_start,qq_print$kb_size);
        qq_print$kb_length = strlen(qq_print$kb_start);
    }
    else if (($temp==(i64)1)) {
                {i64 $temp = (*dev).value;
if (($temp==(i64)0)) {
            goto L768 ;
;
        }
        else if (($temp==(i64)1)) {
            if ((qq_print$testfilech == 0)) {
                qq_lib$prterror((byte*)"R@2: file not open");
            }
;
            ch = qq_print$testfilech;
        }
        else {
            ch = (void *)(*dev).value;
        }
        };
        mlib$readlinen(ch,qq_print$kb_start,qq_print$kb_size);
        qq_print$kb_length = strlen(qq_print$kb_start);
    }
    else if (($temp==(i64)9)) {
        pdev = (*dev).objptr;
        length = (*pdev).length;
        if ((length == (i64)0)) {
            qq_print$kb_length = (i64)0;
            (*qq_print$kb_start) = (u64)0u;
        }
        else if ((length >= qq_print$kb_size)) {
            qq_lib$prterror((byte*)"KB overflow");
        }
        else {
            qq_print$kb_length = length;
            memcpy((void *)qq_print$kb_start,(void *)(*pdev).strptr,(u64)length);
        }
;
    }
    else {
        qq_runaux$pcustype((byte*)"readln@",dev);
    }
    };
    qq_print$kb_pos = qq_print$kb_start;
    qq_print$kb_lastpos = qq_print$kb_pos;
    qq_print$kb_linelength = qq_print$kb_length;
}

void qq_print$pch_sread(struct qq_decls$varrec *fmt,struct qq_decls$varrec *dest) {
        i64 fmtcode;
    fmtcode = qq_print$getreadfmtcode(fmt);
    qq_print$kb_lastpos = qq_print$kb_pos;
    qq_print$kb_lastlength = qq_print$kb_length;
    if ((fmtcode==(i64)73)) {
        qq_print$stepkbpos(qq_print$readint(qq_print$kb_pos,qq_print$kb_length,dest,(i64)0));
    }
    else if ((fmtcode==(i64)82)) {
        qq_print$stepkbpos(qq_print$readreal(qq_print$kb_pos,qq_print$kb_length,dest));
    }
    else if ((fmtcode==(i64)78)) {
        qq_print$stepkbpos(qq_print$readname(qq_print$kb_pos,qq_print$kb_length,dest));
    }
    else if ((fmtcode==(i64)83)) {
        qq_print$stepkbpos(qq_print$readstring(qq_print$kb_pos,qq_print$kb_length,dest));
    }
    else if ((fmtcode==(i64)72)) {
        qq_print$stepkbpos(qq_print$readhex(qq_print$kb_pos,qq_print$kb_length,dest));
    }
    else if ((fmtcode==(i64)66)) {
        qq_print$stepkbpos(qq_print$readbin(qq_print$kb_pos,qq_print$kb_length,dest));
    }
    else if ((fmtcode==(i64)65)) {
        qq_print$stepkbpos(qq_print$readany(qq_print$kb_pos,qq_print$kb_length,dest));
    }
    else if ((fmtcode==(i64)76)) {
        if ((qq_print$kb_length == (i64)0)) {
            qq_strings$var_empty_string(dest,(i64)0);
        }
        else {
            qq_strings$var_make_stringn(qq_print$kb_pos,qq_print$kb_length,dest,(i64)1);
            qq_print$kb_pos += qq_print$kb_length;
            qq_print$kb_length = (i64)0;
        }
;
    }
    else if ((fmtcode==(i64)67)) {
        if ((qq_print$kb_length == (i64)0)) {
            qq_strings$var_empty_string(dest,(i64)0);
        }
        else {
            qq_print$termchar = (u64)(*qq_print$kb_pos);
            //dochar:
L769 :;
;
            (*dest).tagx = (i64)1;
            (*dest).value = (i64)(u64)qq_print$termchar;
            ++(qq_print$kb_pos);
            --(qq_print$kb_length);
        }
;
    }
    else if ((fmtcode==(i64)90)) {
        (*dest).tagx = (i64)1;
        (*dest).value = (i64)(u64)qq_print$termchar;
    }
    else if ((fmtcode==(i64)69)) {
        (*dest).tagx = (i64)1;
        (*dest).value = qq_print$itemerror;
    }
    else if ((fmtcode==(i64)68)) {
        qq_print$stepkbpos(qq_print$readint(qq_print$kb_pos,qq_print$kb_length,dest,(i64)1));
    }
    else {
        qq_lib$prterror((byte*)"SREAD/FMT?");
    }
;
}

void qq_print$pch_sreadln(struct qq_decls$varrec *dev,struct qq_decls$varrec *dest) {
    qq_print$pch_readln(dev);
    qq_strings$var_make_stringn(qq_print$kb_start,qq_print$kb_length,dest,(i64)1);
}

static u8 *qq_print$readname(u8 *s,i64 length,struct qq_decls$varrec *dest) {
        u8 *  send;
        u8 *  itemstr;
        i64 itemlength;
    send = qq_print$readitem(s,length,&itemstr,&itemlength);
    qq_strings$var_make_stringn(itemstr,itemlength,dest,(i64)1);
    mlib$iconvlcn((*(*dest).objptr).strptr,(*(*dest).objptr).length);
    return send;
}

static u8 *qq_print$readstring(u8 *s,i64 length,struct qq_decls$varrec *dest) {
        u8 *  send;
        u8 *  itemstr;
        i64 itemlength;
    send = qq_print$readitem(s,length,&itemstr,&itemlength);
    qq_strings$var_make_stringn(itemstr,itemlength,dest,(i64)1);
    return send;
}

static u8 *qq_print$readint(u8 *sold,i64 length,struct qq_decls$varrec *dest,i64 dodec) {
        u8 *  s;
        u8 *  send;
        i64 itemlength;
    send = qq_print$readitem(sold,length,&s,&itemlength);
    qq_print$strtoint(s,itemlength,dest,dodec);
    return send;
}

static u8 *qq_print$readhex(u8 *sold,i64 length,struct qq_decls$varrec *dest) {
        u8 str[256];
        u8 *  p;
        u8 *  s;
        i64 aa;
        i64 t;
        i64 nalloc;
        u8 c;
    if ((length == (i64)0)) {
        (*dest).tagx = (i64)1;
        (*dest).value = (i64)0;
        qq_print$termchar = (u64)0u;
        return sold;
    }
;
    L770 :;
    while ((!!(length) && (((u64)(*sold) == ' ') || ((i64)(u64)(*sold) == (i64)9)))) {
        ++(sold);
        --(length);
L771 :;
    }
L772 :;
    ;
    if ((length <= (i64)256)) {
        s = (u8 *)str;
        nalloc = (i64)0;
    }
    else {
        nalloc = (length + (i64)1);
        s = (u8 *)mlib$pcm_alloc(nalloc);
    }
;
    p = s;
    L773 :;
    while (!!(length)) {
        c = (u64)toupper((i32)(u64)(*sold));
        ++(sold);
        --(length);
        if ((((u64)c >= '0') && ((u64)c <= '9'))) {
            (*p) = (u64)c;
            ++(p);
        }
        else if ((((u64)c >= 'A') && ((u64)c <= 'F'))) {
            (*p) = (u64)c;
            ++(p);
        }
        else if (((u64)c == '_')) {
        }
        else {
            qq_print$termchar = (u64)c;
            goto L775 ;
        }
;
L774 :;
    }
L775 :;
    ;
    (*p) = (u64)0u;
    length = (p - s);
    if ((length <= (i64)16)) {
        t = (i64)1;
    }
    else {
        t = (i64)3;
    }
;
    p = s;
    if ((t==(i64)1)) {
        aa = (i64)0;
        L776 :;
        while (1) {
            c = (u64)(*p);
            ++(p);
            if (((i64)(u64)c == (i64)0)) {
                goto L777 ;
            }
;
            if (((u64)c < 'A')) {
                aa = (((aa * (i64)16) + (i64)(u64)c) - (i64)48);
            }
            else {
                aa = (((aa * (i64)16) + (i64)((u64)c - 'A')) + (i64)10);
            }
;
        }
L777 :;
        ;
        (*dest).tagx = (i64)1;
        (*dest).value = aa;
    }
    else {
        qq_lib$prterror((byte*)"Readhex/long");
    }
;
    if (!!(nalloc)) {
        mlib$pcm_free((void *)s,nalloc);
    }
;
    return sold;
}

static u8 *qq_print$readbin(u8 *sold,i64 length,struct qq_decls$varrec *dest) {
        u8 str[256];
        u8 *  p;
        u8 *  s;
        i64 aa;
        i64 t;
        i64 nalloc;
        u8 c;
    if ((length == (i64)0)) {
        (*dest).tagx = (i64)1;
        (*dest).value = (i64)0;
        qq_print$termchar = (u64)0u;
        return sold;
    }
;
    L778 :;
    while ((!!(length) && (((u64)(*sold) == ' ') || ((i64)(u64)(*sold) == (i64)9)))) {
        ++(sold);
        --(length);
L779 :;
    }
L780 :;
    ;
    if ((length <= (i64)256)) {
        s = (u8 *)str;
        nalloc = (i64)0;
    }
    else {
        nalloc = (length + (i64)1);
        s = (u8 *)mlib$pcm_alloc(nalloc);
    }
;
    p = s;
    L781 :;
    while (!!(length)) {
        c = (u64)toupper((i32)(u64)(*sold));
        ++(sold);
        --(length);
        if ((((u64)c >= '0') && ((u64)c <= '1'))) {
            (*p) = (u64)c;
            ++(p);
        }
        else if (((u64)c == '_')) {
        }
        else {
            qq_print$termchar = (u64)c;
            goto L783 ;
        }
;
L782 :;
    }
L783 :;
    ;
    (*p) = (u64)0u;
    length = (p - s);
    if ((length <= (i64)64)) {
        t = (i64)1;
    }
    else {
        t = (i64)3;
    }
;
    p = s;
    if ((t==(i64)1)) {
        aa = (i64)0;
        L784 :;
        while (1) {
            c = (u64)(*p);
            ++(p);
            if (((i64)(u64)c == (i64)0)) {
                goto L785 ;
            }
;
            aa = (((aa * (i64)2) + (i64)(u64)c) - (i64)48);
        }
L785 :;
        ;
        (*dest).tagx = (i64)1;
        (*dest).value = aa;
    }
    else {
        qq_lib$prterror((byte*)"Readbin/long");
    }
;
    if (!!(nalloc)) {
        mlib$pcm_free((void *)s,nalloc);
    }
;
    return sold;
}

static u8 *qq_print$readreal(u8 *sold,i64 length,struct qq_decls$varrec *dest) {
        u8 *  send;
        u8 *  itemstr;
        i64 itemlength;
    send = qq_print$readitem(sold,length,&itemstr,&itemlength);
    qq_print$strtoreal(itemstr,itemlength,dest);
    return send;
}

i64 qq_print$getreadfmtcode(struct qq_decls$varrec *p) {
        u8 c;
    if (((p == 0) || ((i64)(*p).tag == (i64)0))) {
        return (i64)65;
    }
;
    if (((i64)(*p).tag != (i64)9)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"P=%s",NULL);
        msysc$m_print_str(qq_tables$ttname[((i64)(*p).tag)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        qq_lib$prterror((byte*)"Readfmt?");
    }
;
    if (((*(*p).objptr).length == (i64)0)) {
        return (i64)65;
    }
;
    c = (u64)toupper((i32)(u64)(*(*(*p).objptr).strptr));
    if (((u64)c=='I') || ((u64)c=='R') || ((u64)c=='N') || ((u64)c=='S') || ((u64)c=='F') || ((u64)c=='T') || ((u64)c=='Z') || ((u64)c=='C') || ((u64)c=='L') || ((u64)c=='H') || ((u64)c=='B') || ((u64)c=='A') || ((u64)c=='E') || ((u64)c=='D')) {
        return (i64)(u64)c;
    }
;
    qq_lib$prterror((byte*)"Readfmt2?");
    return (i64)0;
}

static void qq_print$stepkbpos(u8 *s) {
        i64 newlen;
    newlen = (s - qq_print$kb_pos);
    if ((newlen == (i64)0)) {
        return;
    }
;
    if ((newlen >= qq_print$kb_length)) {
        qq_print$kb_pos = (qq_print$kb_pos + qq_print$kb_length);
        qq_print$kb_length = (i64)0;
    }
    else {
        qq_print$kb_pos = (qq_print$kb_pos + newlen);
        qq_print$kb_length -= newlen;
    }
;
}

static u8 *qq_print$readany(u8 *sold,i64 length,struct qq_decls$varrec *dest) {
        u8 *  p;
        u8 *  s;
        i64 digits;
        i64 expon;
        i64 other;
        u8 *  send;
        i64 itemlength;
        i64 $av_1;
    qq_print$itemerror = (i64)0;
    send = qq_print$readitem(sold,length,&s,&itemlength);
    p = s;
    digits = (expon = (other = (i64)0));
    $av_1 = itemlength;
    while ($av_1-- > 0) {
L786 :;
        switch ((i64)(u64)(*(p)++)) {
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
            }
            break;
        case 69:;
        case 101:;
        case 46:;
            {
                expon = (i64)1;
            }
            break;
        default: {
            other = (i64)1;
        }
        } //SW
;
L787 :;
    }
L788 :;
    ;
    (*dest).tagx = (i64)1;
    if ((!!(other) || (itemlength == (i64)0))) {
        (*dest).value = (i64)5395539;
        qq_strings$var_make_stringn(s,itemlength,dest,(i64)1);
    }
    else if (!!(expon)) {
        qq_print$strtoreal(s,itemlength,dest);
    }
    else {
        qq_print$strtoint(s,itemlength,dest,(i64)0);
    }
;
    return send;
}

static u8 *qq_print$readitem(u8 *s,i64 length,u8 **itemstr,i64 *itemlength) {
        u8 *  p;
        u8 quotechar;
        u8 c;
    L789 :;
    while ((!!(length) && (((u64)(*s) == ' ') || ((i64)(u64)(*s) == (i64)9)))) {
        ++(s);
        --(length);
L790 :;
    }
L791 :;
    ;
    (*itemstr) = s;
    if ((length == (i64)0)) {
        qq_print$termchar = (u64)0u;
        (*itemlength) = (i64)0;
        return s;
    }
;
    quotechar = (u64)0u;
    if (((u64)(*s) == '"')) {
        quotechar = '"';
        ++(s);
        --(length);
    }
    else if (((u64)(*s) == (u64)39u)) {
        quotechar = (u64)39u;
        ++(s);
        --(length);
    }
;
    p = ((*itemstr) = s);
    L792 :;
    while (!!(length)) {
        c = (u64)(*(s)++);
        --(length);
        if (((u64)c==' ') || ((u64)c==(u64)9u) || ((u64)c==',') || ((u64)c=='=') || ((u64)c==';')) {
            if ((!!((u64)quotechar) || (p == s))) {
                goto L795 ;
;
            }
;
            qq_print$termchar = (u64)c;
            goto L794 ;
        }
        else {
            //normalchar:
L795 :;
;
            if (((u64)c == (u64)quotechar)) {
                if ((!!(length) && ((u64)(*s) == (u64)quotechar))) {
                    (*p) = (u64)c;
                    ++(s);
                    ++(p);
                }
                else {
                    qq_print$termchar = (u64)(*s);
                    if ((((u64)qq_print$termchar == ',') || ((u64)qq_print$termchar == '='))) {
                        ++(s);
                        qq_print$termchar = (u64)(*s);
                    }
;
                    goto L794 ;
                }
;
            }
            else {
                (*p) = (u64)c;
                ++(p);
            }
;
        }
;
L793 :;
    }
L794 :;
    ;
    if ((length == (i64)0)) {
        qq_print$termchar = (u64)0u;
    }
;
    (*itemlength) = (p - (*itemstr));
    return s;
}

static void qq_print$strtoreal(u8 *s,i64 length,struct qq_decls$varrec *dest) {
        u8 str[512];
        r64 x;
        i32 numlength;
    (*dest).tagx = (i64)2;
    if (((length >= (i64)512) || (length == (i64)0))) {
        (*dest).xvalue = (double)0.;
        return;
    }
;
    memcpy(str,(void *)s,(u64)length);
    str[((length + (i64)1))-1] = (u64)0u;
    qq_print$itemerror = (i64)0;
    if (((sscanf((u8 *)str,(byte*)"%lf%n",&x,&numlength) == (i64)0) || ((i64)numlength != length))) {
        if (((i64)numlength == length)) {
            x = (double)0.;
        }
;
        qq_print$itemerror = (i64)1;
    }
;
    (*dest).xvalue = x;
}

static void qq_print$strtoint(u8 *s,i64 length,struct qq_decls$varrec *dest,i64 dodec) {
        u8 *  p;
        u8 *  q;
        byte signd;
        i64 aa;
        i64 cat;
        i64 t;
        u8 c;
    qq_print$itemerror = (i64)0;
    if ((length == (i64)0)) {
        (*dest).tagx = (i64)1;
        (*dest).value = (i64)0;
        return;
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
    L796 :;
    while ((((u64)(*s) == '0') && (length > (i64)1))) {
        ++(s);
        --(length);
L797 :;
    }
L798 :;
    ;
    p = (q = s);
    L799 :;
    while (!!(length)) {
        c = (u64)(*(q)++);
        --(length);
        if ((((u64)c >= '0') && ((u64)c <= '9'))) {
            (*p) = (u64)c;
            ++(p);
        }
        else {
            if (((u64)c == '_')) {
            }
            else {
                qq_print$itemerror = (i64)1;
                goto L801 ;
            }
;
        }
;
L800 :;
    }
L801 :;
    ;
    (*p) = (u64)0u;
    length = (p - s);
    if ((length <= (i64)18)) {
        cat = (i64)65;
    }
    else if ((length == (i64)19)) {
                {i64 $temp = mlib$cmpstring(s,(byte*)"9223372036854775808");
if (($temp==(i64)-1)) {
            cat = (i64)65;
        }
        else if (($temp==(i64)0)) {
            cat = (i64)66;
        }
        else {
            cat = (i64)67;
        }
        };
    }
    else if ((length == (i64)20)) {
        if ((mlib$cmpstring(s,(byte*)"18446744073709551615") <= (i64)0)) {
            cat = (i64)67;
        }
        else {
            cat = (i64)68;
        }
;
    }
    else {
        cat = (i64)68;
    }
;
    if (!!(dodec)) {
        cat = (i64)68;
    }
;
    if (!!((i64)signd)) {
        if ((cat==(i64)66)) {
            cat = (i64)65;
        }
        else if ((cat==(i64)67)) {
            cat = (i64)68;
        }
;
    }
;
    if ((cat==(i64)65)) {
        t = (i64)1;
    }
    else {
        t = (i64)3;
    }
;
    p = s;
    if ((t != (i64)3)) {
        aa = (i64)0;
        L802 :;
        while (1) {
            c = (u64)(*p);
            ++(p);
            if (((i64)(u64)c == (i64)0)) {
                goto L803 ;
            }
;
            aa = ((aa * (i64)10) + (i64)((u64)c - '0'));
        }
L803 :;
        ;
        if (!!((i64)signd)) {
            aa = -(aa);
        }
;
        (*dest).tagx = t;
        (*dest).value = aa;
    }
    else {
        qq_decimal$var_make_dec_str(s,length,dest);
    }
;
}

static void qq_print$printnextfmtchars(i64 lastx) {
        u8 c;
        u8 *  pstart;
        i64 n;
    pstart = qq_print$mfmtcurr;
    n = (i64)0;
    L804 :;
    while (1) {
        c = (u64)(*qq_print$mfmtcurr);
        if (((u64)c=='#')) {
            if (!!(lastx)) {
                goto L806 ;
;
            }
;
            ++(qq_print$mfmtcurr);
            if (!!(n)) {
                qq_print$printstr_n(pstart,n);
            }
;
            return;
        }
        else if (((u64)c==(u64)0u)) {
            if (!!(n)) {
                qq_print$printstr_n(pstart,n);
            }
            else if (!(!!(lastx))) {
                qq_print$printstr_n((byte*)"|",(i64)1);
            }
;
            return;
        }
        else if (((u64)c=='~')) {
            if (!!(n)) {
                qq_print$printstr_n(pstart,n);
                n = (i64)0;
            }
;
            ++(qq_print$mfmtcurr);
            c = (u64)(*qq_print$mfmtcurr);
            if (!!((u64)c)) {
                ++(qq_print$mfmtcurr);
                qq_print$printstr_n((u8 *)&c,(i64)1);
            }
;
            pstart = qq_print$mfmtcurr;
        }
        else {
            //skip:
L806 :;
;
            ++(n);
            ++(qq_print$mfmtcurr);
        }
;
    }
L805 :;
    ;
}

void qq_print$pch_setformat(struct qq_decls$varrec *p) {
        i64 n;
        u8 *  s;
    if (((i64)(*p).tag != (i64)9)) {
        qq_lib$prterror((byte*)"(str)");
    }
;
    if (!!(qq_print$mfmtstr)) {
        qq_lib$prterror((byte*)"Setfmt?");
    }
;
    n = (*(*p).objptr).length;
    qq_print$mfmtstr = (u8 *)mlib$pcm_alloc((n + (i64)1));
    if (!!(n)) {
        memcpy((void *)qq_print$mfmtstr,(void *)(*(*p).objptr).strptr,(u64)n);
    }
;
    s = (qq_print$mfmtstr + n);
    (*s) = (u64)0u;
    qq_print$mfmtcurr = qq_print$mfmtstr;
}

struct msysc$fmtrec *qq_print$pc_getfmt(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt) {
    if (((p == 0) || ((i64)(*p).tag == (i64)0))) {
        return (struct msysc$fmtrec *)&qq_print$defaultfmt;
    }
    else {
        if (((i64)(*p).tag != (i64)9)) {
            qq_lib$prterror((byte*)"pc_getfmt/not str?");
        }
;
        if (((*(*p).objptr).strptr == 0)) {
            return (struct msysc$fmtrec *)&qq_print$defaultfmt;
        }
        else {
            msysc$strtofmt((*(*p).objptr).strptr,(*(*p).objptr).length,(struct msysc$fmtrec *)fmt);
            return (struct msysc$fmtrec *)fmt;
        }
;
    }
;
}

void qq_print$addstring(struct qq_decls$objrec *p,u8 *t,i64 n) {
        i64 oldlen;
        i64 newlen;
        i64 oldbytes;
        i64 newbytes;
        u8 *  newptr;
    if (((n == (i64)0) || ((i64)(u64)(*t) == (i64)0))) {
        return;
    }
;
    if ((n < (i64)0)) {
        n = strlen(t);
    }
;
    oldlen = (*p).length;
    if (((i64)(*p).refcount == (i64)0)) {
        if ((oldlen == (i64)0)) {
            memcpy((void *)(*p).strptr,(void *)t,(u64)n);
            (*p).length = n;
        }
        else {
            memcpy((void *)((*p).strptr + oldlen),(void *)t,(u64)n);
            (*p).length = (oldlen + n);
        }
;
        return;
    }
;
    if ((oldlen == (i64)0)) {
        (*p).strptr = (u8 *)mlib$pcm_alloc(n);
        (*p).length = n;
        (*p).alloc64 = mlib$allocbytes;
        memcpy((void *)(*p).strptr,(void *)t,(u64)n);
    }
    else {
        newlen = (oldlen + n);
        oldbytes = (*p).alloc64;
        newbytes = (oldlen + n);
        if ((newbytes <= oldbytes)) {
            memcpy((void *)((*p).strptr + oldlen),(void *)t,(u64)n);
        }
        else {
            newptr = (u8 *)mlib$pcm_alloc(newbytes);
            memcpy((void *)newptr,(void *)(*p).strptr,(u64)oldlen);
            memcpy((void *)(newptr + oldlen),(void *)t,(u64)n);
            (*p).alloc64 = mlib$allocbytes;
            mlib$pcm_free((void *)(*p).strptr,oldbytes);
            (*p).strptr = newptr;
        }
;
        (*p).length = newlen;
    }
;
}

static void qq_print$domultichar(u8 *p,i64 n,u8 *dest,struct msysc$fmtrec *fmt) {
        u8 str[20];
        u8 *  q;
        i64 nchars;
        i64 $av_1;
    q = (u8 *)str;
    nchars = n;
    $av_1 = n;
    while ($av_1-- > 0) {
L807 :;
        if (((i64)(u64)(*p) == (i64)0)) {
            goto L809 ;
        }
;
        (*q) = (u64)(*p);
        ++(q);
        ++(p);
L808 :;
    }
L809 :;
    ;
    (*q) = (u64)0u;
    msysc$expandstr(str,dest,nchars,(struct msysc$fmtrec *)fmt);
}

static void qq_print$printstr_n(u8 *s,i64 n) {
        struct qq_decls$varrec *  p;
    if ((n == (i64)-1)) {
        n = strlen(s);
    }
;
    if ((n == (i64)0)) {
        return;
    }
;
    if ((qq_print$moutdev==(i64)0)) {
        msysc$printstrn_app(s,n,0);
    }
    else if ((qq_print$moutdev==(i64)1)) {
        msysc$printstrn_app(s,n,qq_print$moutchan);
    }
    else if ((qq_print$moutdev==(i64)2)) {
        qq_print$addstring(qq_print$moutvar.objptr,s,n);
    }
    else if ((qq_print$moutdev==(i64)4)) {
        p = qq_print$moutvar.varptr;
        if (((i64)(*p).tag != (i64)9)) {
            qq_lib$prterror((byte*)"prtstrn1");
        }
;
        qq_print$addstring(qq_print$moutvar.objptr,s,n);
    }
    else if ((qq_print$moutdev==(i64)3)) {
    }
;
}

void qq_print$pch_strtoval(struct qq_decls$varrec *p,struct qq_decls$varrec *fmt,struct qq_decls$varrec *dest) {
        i64 fmtcode;
        i64 length;
        struct qq_decls$objrec *  q;
        u8 str[1024];
        u8 *  s;
    s = (u8 *)str;
    q = (*p).objptr;
    if (((*q).length < (i64)1024)) {
        memcpy((void *)s,(void *)(*q).strptr,(u64)(*q).length);
        str[(((*q).length + (i64)1))-1] = (u64)0u;
    }
    else {
        qq_runaux$pcerror((byte*)"STRTOVAL/string too long",(byte*)"");
    }
;
    fmtcode = qq_print$getreadfmtcode(fmt);
    if (((i64)(*p).tag != (i64)9)) {
        qq_lib$prterror((byte*)"strval");
    }
;
    length = (*(*p).objptr).length;
    if ((fmtcode==(i64)73)) {
        qq_print$readint(s,length,dest,(i64)0);
    }
    else if ((fmtcode==(i64)68)) {
        qq_print$readint(s,length,dest,(i64)1);
    }
    else if ((fmtcode==(i64)82)) {
        qq_print$readreal(s,length,dest);
    }
    else if ((fmtcode==(i64)78)) {
        qq_print$readname(s,length,dest);
    }
    else if ((fmtcode==(i64)83)) {
        qq_print$readstring(s,length,dest);
    }
    else if ((fmtcode==(i64)72)) {
        qq_print$readhex(s,length,dest);
    }
    else if ((fmtcode==(i64)66)) {
        qq_print$readbin(s,length,dest);
    }
    else if ((fmtcode==(i64)65)) {
        qq_print$readany(s,length,dest);
    }
    else {
        qq_lib$prterror((byte*)"strval:fmt?");
    }
;
}

static void qq_print$tostr_int(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest) {
        u8 str[1024];
        {u64 $temp = (u64)(*fmt).charmode;
if (($temp=='M') || ($temp=='D')) {
        qq_print$domultichar((u8 *)&(*p).value,(i64)8,str,(struct msysc$fmtrec *)fmt);
    }
    else if (($temp=='C')) {
        str[((i64)0)] = (u64)(*p).value;
        str[((i64)1)] = (u64)0u;
    }
    else {
        msysc$i64tostrfmt((*p).value,str,(struct msysc$fmtrec *)fmt);
    }
    };
    if (!!(msysc$m_getdotindex((i64)(*fmt).spare,(i64)0))) {
        qq_print$addstring(dest,(byte*)"I:",(i64)2);
    }
;
    qq_print$addstring(dest,str,strlen(str));
}

static void qq_print$tostr_real(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest) {
        u8 str[1024];
        u8 str2[1024];
        u8 cfmt[10];
        i64 n;
    if (!!((i64)(*fmt).precision)) {
        cfmt[((i64)1)-1] = '.';
        cfmt[((i64)2)-1] = '*';
        cfmt[((i64)3)-1] = (u64)(*fmt).realfmt;
        cfmt[((i64)4)-1] = (u64)0u;
        msysc$m_print_startstr(str);
        msysc$m_print_i64((i64)(*fmt).precision,(byte*)"v");
        msysc$m_print_r64((*p).xvalue,cfmt);
        msysc$m_print_end();
        ;
    }
    else {
        msysc$m_print_startstr(str);
        msysc$m_print_r64((*p).xvalue,(byte*)"fmt");
        msysc$m_print_end();
        ;
    }
;
    n = strlen(str);
    if ((n < (i64)(*fmt).minwidth)) {
        msysc$expandstr(str,str2,n,(struct msysc$fmtrec *)fmt);
        strcpy(str,str2);
    }
;
    qq_print$addstring(dest,str,strlen(str));
}

static void qq_print$tostr_str(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest) {
        i64 oldlen;
        i64 newlen;
        u8 *  s;
        struct qq_decls$objrec *  q;
    q = (*p).objptr;
    oldlen = (*q).length;
    newlen = oldlen;
    if ((!!((u64)(*fmt).quotechar) || ((i64)(*fmt).minwidth > newlen))) {
        if (!!((u64)(*fmt).quotechar)) {
            newlen += (i64)2;
        }
;
        if (((i64)(*fmt).minwidth > newlen)) {
            newlen = (i64)(*fmt).minwidth;
        }
;
        s = (u8 *)mlib$pcm_alloc((newlen + (i64)1));
        msysc$strtostrfmt((*q).strptr,s,oldlen,(struct msysc$fmtrec *)fmt);
        qq_print$addstring(dest,s,newlen);
        mlib$pcm_free((void *)s,(newlen + (i64)1));
    }
    else {
        qq_print$addstring(dest,(*q).strptr,oldlen);
    }
;
}

void qq_print$pch_tostr(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *result) {
        struct msysc$fmtrec fmt;
        struct msysc$fmtrec *  ifmt;
        struct qq_decls$objrec *  p;
    ifmt = (struct msysc$fmtrec *)qq_print$pc_getfmt(b,&fmt);
    p = qq_strings$obj_new_string((i64)0);
    qq_print$listdepth = (i64)0;
    qq_print$tostr(a,(struct msysc$fmtrec *)ifmt,p);
    (*result).tagx = (i64)265;
    (*result).objptr = p;
}

static void qq_print$tostr_range(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest) {
        u8 str[1024];
    msysc$i64tostrfmt(msysc$m_getdotslice((*p).dummy,(i64)16,(i64)63),str,(struct msysc$fmtrec *)fmt);
    strcat(str,(byte*)"..");
    qq_print$addstring(dest,str,(i64)-1);
    msysc$i64tostrfmt((i64)(*p).range_upper,str,(struct msysc$fmtrec *)fmt);
    qq_print$addstring(dest,str,(i64)-1);
}

static void qq_print$tostr_array(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest) {
        u8 str[1024];
        byte *  q;
        i64 i;
        i64 m;
        i64 elemtype;
        i64 a;
        i64 b;
        i64 lower;
        i64 length;
        struct qq_decls$varrec v;
        struct qq_decls$objrec *  pa;
    m = (i64)(*p).tag;
    pa = (*p).objptr;
    if (((i64)(*p).tag == (i64)11)) {
        length = (*pa).length;
        lower = (i64)msysc$m_getdotindex((i64)(*pa).flags,(i64)0);
        elemtype = (i64)(*pa).elemtag;
    }
    else {
        length = qq_tables$ttlength[((i64)(*pa).usertag)];
        lower = qq_tables$ttlower[((i64)(*pa).usertag)];
        elemtype = (i64)qq_tables$tttarget[((i64)(*pa).usertag)];
    }
;
    a = lower;
    b = ((length + lower) - (i64)1);
    q = (*pa).ptr;
    if (!!(msysc$m_getdotindex((i64)(*fmt).spare,(i64)0))) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"#[#:#]A");
        msysc$m_print_str(qq_tables$ttname[(m)],NULL);
        msysc$m_print_i64(lower,NULL);
        msysc$m_print_str(qq_tables$ttname[(elemtype)],NULL);
        msysc$m_print_end();
        ;
        qq_print$addstring(dest,str,(i64)-1);
    }
;
    qq_print$addstring(dest,(byte*)"(",(i64)-1);
    for (i=a;i<=b;++i) {
L810 :;
        qq_packed$var_loadpacked(q,elemtype,&v,0);
        q += qq_tables$ttsize[(elemtype)];
        qq_print$tostr(&v,(struct msysc$fmtrec *)fmt,dest);
        if ((i < b)) {
            qq_print$addstring(dest,(byte*)",",(i64)1);
        }
;
L811 :;
    }
L812 :;
    ;
    qq_print$addstring(dest,(byte*)")",(i64)1);
}

static void qq_print$tostr_bits(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest) {
        u8 str[1024];
        byte *  q;
        i64 i;
        i64 m;
        i64 elemtype;
        i64 a;
        i64 b;
        i64 bitwidthx;
        i64 offset;
        struct qq_decls$varrec v;
        struct qq_decls$objrec *  pa;
    m = (i64)(*p).tag;
    pa = (*p).objptr;
    a = (i64)(*pa).lower16;
    elemtype = (i64)(*pa).elemtag;
    b = (((*pa).length + a) - (i64)1);
    bitwidthx = (i64)qq_tables$ttbitwidth[(elemtype)];
    offset = ((i64)(*pa).indexoffset * bitwidthx);
    q = (*pa).ptr;
    if (!!(msysc$m_getdotindex((i64)(*fmt).spare,(i64)0))) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"#[#:#]A");
        msysc$m_print_str(qq_tables$ttname[(m)],NULL);
        msysc$m_print_i64((i64)(*pa).lower16,NULL);
        msysc$m_print_str(qq_tables$ttname[(elemtype)],NULL);
        msysc$m_print_end();
        ;
        qq_print$addstring(dest,str,(i64)-1);
    }
;
    qq_print$addstring(dest,(byte*)"(",(i64)-1);
    for (i=a;i<=b;++i) {
L813 :;
        qq_vars$var_loadbit(q,offset,elemtype,(i64)0,&v);
        offset += bitwidthx;
        if ((offset >= (i64)8)) {
            offset = (i64)0;
            ++(q);
        }
;
        qq_print$tostr(&v,(struct msysc$fmtrec *)fmt,dest);
        if ((i < b)) {
            qq_print$addstring(dest,(byte*)",",(i64)1);
        }
;
L814 :;
    }
L815 :;
    ;
    qq_print$addstring(dest,(byte*)")",(i64)1);
}

static void qq_print$tostr_struct(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest) {
        i64 i;
        i64 m;
        i64 nfields;
        i64 needcomma;
        struct qq_decls$varrec v;
        struct qq_decls$objrec *  pa;
        struct qq_decls$strec *  d;
        struct qq_decls$strec **  r;
    pa = (*p).objptr;
    m = (i64)(*pa).usertag;
    d = qq_tables$ttnamedef[(m)];
    r = (struct qq_decls$strec **)(*d).topfieldlist;
    nfields = qq_tables$ttlength[(m)];
    needcomma = (i64)0;
    qq_print$addstring(dest,(byte*)"(",(i64)-1);
    for (i=(i64)1;i<=nfields;++i) {
L816 :;
        qq_packed$var_loadpacked(((*pa).ptr + (i64)(*(*r)).fieldoffset),(i64)(*(*r)).mode,&v,0);
        if (!!(needcomma)) {
            qq_print$addstring(dest,(byte*)",",(i64)-1);
        }
;
        needcomma = (i64)1;
        qq_print$tostr(&v,(struct msysc$fmtrec *)fmt,dest);
        ++(r);
L817 :;
    }
L818 :;
    ;
    qq_print$addstring(dest,(byte*)")",(i64)-1);
}

static void qq_print$tostr_set(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest) {
        i64 i;
        i64 j;
        i64 first;
        struct qq_decls$varrec v;
        struct qq_decls$objrec *  s;
    if ((fmt == 0)) {
        fmt = (struct msysc$fmtrec *)&qq_print$defaultfmt;
    }
;
    qq_print$addstring(dest,(byte*)"[",(i64)1);
    s = (*p).objptr;
    first = (i64)1;
    i = (i64)0;
    L819 :;
    while ((i < (*s).length)) {
        if (!!(qq_lib$testelem((byte (*)[])(*s).ptr,i))) {
            j = (i + (i64)1);
            L822 :;
            while (((j < (*s).length) && !!(qq_lib$testelem((byte (*)[])(*s).ptr,j)))) {
                ++(j);
L823 :;
            }
L824 :;
            ;
            --(j);
            if (!(!!(first))) {
                qq_print$addstring(dest,(byte*)",",(i64)1);
            }
;
            first = (i64)0;
            if ((i == j)) {
                v.tagx = (i64)1;
                v.value = i;
            }
            else {
                v.tagx = (i64)4;
                v.dummy = msysc$m_setdotslice(v.dummy,(i64)16,(i64)63,i);
                v.range_upper = (u64)j;
            }
;
            qq_print$tostr(&v,(struct msysc$fmtrec *)fmt,dest);
            i = (j + (i64)1);
        }
        else {
            ++(i);
        }
;
L820 :;
    }
L821 :;
    ;
    qq_print$addstring(dest,(byte*)"]",(i64)1);
}

static void qq_print$tostr_dict(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest) {
        struct qq_decls$varrec *  q;
        i64 i;
        i64 length;
        i64 needcomma;
        struct qq_decls$objrec *  pa;
    needcomma = (i64)0;
    if ((fmt == 0)) {
        fmt = (struct msysc$fmtrec *)&qq_print$defaultfmt;
    }
;
    qq_print$addstring(dest,(byte*)"[",(i64)-1);
    pa = (*p).objptr;
    q = (*pa).varptr;
    length = ((*pa).length / (i64)2);
    for (i=length;i>=(i64)1;--i) {
L825 :;
        if (((i64)(*q).tag == (i64)0)) {
            q += (i64)2;
            goto L826 ;
        }
;
        if (!!(needcomma)) {
            qq_print$addstring(dest,(byte*)",",(i64)1);
        }
;
        needcomma = (i64)1;
        qq_print$tostr(q,(struct msysc$fmtrec *)fmt,dest);
        ++(q);
        qq_print$addstring(dest,(byte*)":",(i64)1);
        qq_print$tostr(q,(struct msysc$fmtrec *)fmt,dest);
        ++(q);
L826 :;
    }
L827 :;
    ;
    qq_print$addstring(dest,(byte*)"]",(i64)1);
}

static void qq_print$tostr_decimal(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest) {
        u8 *  s;
    s = qq_decimal$var_tostr_dec(p,(i64)0);
    qq_print$addstring(dest,s,(i64)-1);
    mlib$pcm_free((void *)s,qq_decimal$decstrsize);
}

static void qq_print$tostr(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest) {
        u8 str[1024];
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)1)) {
        qq_print$tostr_int(p,(struct msysc$fmtrec *)fmt,dest);
    }
    else if (($temp==(i64)2)) {
        qq_print$tostr_real(p,(struct msysc$fmtrec *)fmt,dest);
    }
    else if (($temp==(i64)9)) {
        qq_print$tostr_str(p,(struct msysc$fmtrec *)fmt,dest);
    }
    else if (($temp==(i64)4)) {
        qq_print$tostr_range(p,(struct msysc$fmtrec *)fmt,dest);
    }
    else if (($temp==(i64)10) || ($temp==(i64)12)) {
        qq_print$tostr_list(p,(struct msysc$fmtrec *)fmt,dest);
    }
    else if (($temp==(i64)11) || ($temp==(i64)7)) {
        qq_print$tostr_array(p,(struct msysc$fmtrec *)fmt,dest);
    }
    else if (($temp==(i64)8)) {
        qq_print$tostr_bits(p,(struct msysc$fmtrec *)fmt,dest);
    }
    else if (($temp==(i64)5)) {
        qq_print$tostr_set(p,(struct msysc$fmtrec *)fmt,dest);
    }
    else if (($temp==(i64)13)) {
        qq_print$tostr_struct(p,(struct msysc$fmtrec *)fmt,dest);
    }
    else if (($temp==(i64)3)) {
        qq_print$tostr_decimal(p,(struct msysc$fmtrec *)fmt,dest);
    }
    else if (($temp==(i64)6)) {
        qq_print$tostr_dict(p,(struct msysc$fmtrec *)fmt,dest);
    }
    else if (($temp==(i64)0)) {
        qq_print$addstring(dest,(byte*)"<Void>",(i64)-1);
    }
    else if (($temp==(i64)14)) {
        if (!!(msysc$m_getdotindex((i64)(*fmt).spare,(i64)0))) {
            msysc$m_print_startstr(str);
            msysc$m_print_setfmt((byte*)"#<#>:");
            msysc$m_print_str(qq_tables$ttname[((i64)(*p).tag)],NULL);
            msysc$m_print_str((!!((*p).varptr) ? qq_tables$ttname[((i64)(*(*p).varptr).tag)] : (byte*)""),NULL);
            msysc$m_print_end();
            ;
            qq_print$addstring(dest,str,(i64)-1);
        }
;
        //showptr:
L828 :;
;
        if (((*p).varptr == 0)) {
            qq_print$addstring(dest,(byte*)"nil",(i64)-1);
        }
        else {
            qq_print$addstring(dest,msysc$strint((i64)(*p).varptr,(byte*)"H"),(i64)-1);
        }
;
    }
    else if (($temp==(i64)16)) {
        if (!!(msysc$m_getdotindex((i64)(*fmt).spare,(i64)0))) {
            msysc$m_print_startstr(str);
            msysc$m_print_setfmt((byte*)"#<#>:");
            msysc$m_print_str(qq_tables$ttname[((i64)(*p).tag)],NULL);
            msysc$m_print_str((!!((*p).varptr) ? qq_tables$ttname[((i64)(*p).elemtag)] : (byte*)""),NULL);
            msysc$m_print_end();
            ;
            qq_print$addstring(dest,str,(i64)-1);
        }
;
        goto L828 ;
;
    }
    else if (($temp==(i64)15)) {
        if (!!(msysc$m_getdotindex((i64)(*fmt).spare,(i64)0))) {
            msysc$m_print_startstr(str);
            msysc$m_print_setfmt((byte*)"#<#>(#,#):");
            msysc$m_print_str(qq_tables$ttname[((i64)(*p).tag)],NULL);
            msysc$m_print_str((!!((*p).varptr) ? qq_tables$ttname[((i64)(*p).elemtag)] : (byte*)""),NULL);
            msysc$m_print_i64((i64)(*p).bitoffset,NULL);
            msysc$m_print_i64((i64)(*p).bitlength,NULL);
            msysc$m_print_end();
            ;
            qq_print$addstring(dest,str,(i64)-1);
        }
;
        goto L828 ;
;
    }
    else if (($temp==(i64)17)) {
        if (!!((*p).def)) {
            msysc$m_print_startstr(str);
            msysc$m_print_setfmt((byte*)"<#:\"#\">");
            msysc$m_print_str(qq_tables$namenames[((i64)(*(*p).def).nameid)],NULL);
            msysc$m_print_str((*(*p).def).name,NULL);
            msysc$m_print_end();
            ;
            qq_print$addstring(dest,str,(i64)-1);
        }
        else {
            qq_print$addstring(dest,(byte*)"<nil>",(i64)-1);
        }
;
    }
    else if (($temp==(i64)18)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"#");
        msysc$m_print_str(qq_tables$ttname[((*p).value)],NULL);
        msysc$m_print_end();
        ;
        qq_print$addstring(dest,str,(i64)-1);
    }
    else if (($temp==(i64)19)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"(#)");
        msysc$m_print_str((qq_pcltabs$pclnames[((*p).value)] + (i64)1),NULL);
        msysc$m_print_end();
        ;
        qq_print$addstring(dest,str,(i64)-1);
    }
    else {
        qq_runaux$pcustype((byte*)"Tostr:",p);
    }
    };
}

static void qq_print$tostr_list(struct qq_decls$varrec *p,struct msysc$fmtrec *fmt,struct qq_decls$objrec *dest) {
        struct qq_decls$varrec *  q;
        i64 i;
        i64 n;
        struct qq_decls$objrec *  r;
        i64 $av_1;
    ++(qq_print$listdepth);
    r = (*p).objptr;
    if ((((i64)(*r).refcount < (i64)0) || (qq_print$listdepth > (i64)4))) {
        qq_print$addstring(dest,(byte*)"...",(i64)3);
        --(qq_print$listdepth);
        return;
    }
;
    (*r).refcount = -((i64)(*r).refcount);
    q = (*r).varptr;
    if (((i64)(*p).tag == (i64)10)) {
        n = (*(*p).objptr).length;
    }
    else {
        n = qq_tables$ttlength[((i64)(*r).usertag)];
    }
;
    if (!!(msysc$m_getdotindex((i64)(*fmt).spare,(i64)1))) {
        $av_1 = n;
        while ($av_1-- > 0) {
L829 :;
            qq_print$tostr(q,(struct msysc$fmtrec *)fmt,dest);
            qq_print$addstring(dest,(byte*)"\n",(i64)-1);
            ++(q);
L830 :;
        }
L831 :;
        ;
    }
    else {
        qq_print$addstring(dest,(byte*)"(",(i64)1);
        for (i=n;i>=(i64)1;--i) {
L832 :;
            qq_print$tostr(q,(struct msysc$fmtrec *)fmt,dest);
            ++(q);
            if ((i != (i64)1)) {
                qq_print$addstring(dest,(byte*)",",(i64)1);
            }
;
L833 :;
        }
L834 :;
        ;
        qq_print$addstring(dest,(byte*)")",(i64)1);
    }
;
    (*r).refcount = -((i64)(*r).refcount);
    --(qq_print$listdepth);
}

// START
void qq_print$start(void) {

}

void qq_records$var_make_record(struct qq_decls$varrec *a,struct qq_decls$varrec *dest,i64 n,i64 rectype) {
        struct qq_decls$objrec *  p;
        struct qq_decls$varrec *  b;
        i64 m;
        i64 $av_1;
    p = qq_records$obj_new_record(rectype,0);
    b = (*p).varptr;
    m = qq_tables$ttlength[(rectype)];
    if ((n < m)) {
        qq_runaux$pcerror((byte*)"Too few elements",(byte*)"");
    }
    else if ((n > m)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"N=",NULL);
        msysc$m_print_i64(n,NULL);
        msysc$m_print_str((byte*)"M=",NULL);
        msysc$m_print_i64(m,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        qq_runaux$pcerror((byte*)"Too many elements",(byte*)"");
    }
;
    $av_1 = n;
    while ($av_1-- > 0) {
L835 :;
        (*b) = (*a);
        ++(a);
        ++(b);
L836 :;
    }
L837 :;
    ;
    (*dest).tagx = (i64)268;
    (*p).usertag = rectype;
    (*dest).objptr = p;
}

struct qq_decls$objrec *qq_records$obj_new_record(i64 m,struct qq_decls$varrec *defval) {
        struct qq_decls$objrec *  p;
        struct qq_decls$varrec *  a;
        i64 n;
        i64 $av_1;
        i64 $av_2;
    p = qq_vars$obj_new();
    (*p).flags = msysc$m_setdotindex((*p).flags,(i64)1,(u64)1u);
    n = qq_tables$ttlength[(m)];
    (*p).objtype = (i64)0;
    if (!!(n)) {
        (*p).varptr = (a = (struct qq_decls$varrec *)mlib$pcm_alloc((n * (i64)16)));
        if ((!!(defval) && ((i64)(*defval).tag != (i64)0))) {
            a = (*p).varptr;
            $av_1 = n;
            while ($av_1-- > 0) {
L838 :;
                (*a) = (*defval);
                if (!!((i64)(*a).hasref)) {
                    ++((*(*a).objptr).refcount);
                }
;
                ++(a);
L839 :;
            }
L840 :;
            ;
        }
        else {
            $av_2 = n;
            while ($av_2-- > 0) {
L841 :;
                (*a).tagx = (i64)1;
                (*a).value = (i64)0;
                ++(a);
L842 :;
            }
L843 :;
            ;
        }
;
    }
;
    return p;
}

void qq_records$obj_free_record(struct qq_decls$objrec *p) {
        struct qq_decls$varrec *  q;
        i64 $av_1;
    q = (*p).varptr;
    $av_1 = (*p).length;
    while ($av_1-- > 0) {
L844 :;
        if (!!((i64)(*q).hasref)) {
            qq_vars$var_unshareu(q);
        }
;
        ++(q);
L845 :;
    }
L846 :;
    ;
    if (!!((*p).length)) {
        mlib$pcm_free((*p).varptr,((*p).length * (i64)16));
    }
;
    mlib$pcm_free32(p);
}

void qq_records$var_dupl_record(struct qq_decls$varrec *a) {
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
        struct qq_decls$varrec *  plist;
        struct qq_decls$varrec *  qlist;
        i64 length;
        i64 $av_1;
    p = (*a).objptr;
    q = qq_vars$obj_new();
    (*q) = (*p);
    (*q).refcount = (i64)1;
    (*q).flags = msysc$m_setdotindex((*q).flags,(i64)1,(u64)1u);
    (*a).objptr = q;
    length = qq_tables$ttlength[((i64)(*p).usertag)];
    if ((length == (i64)0)) {
        return;
    }
;
    qlist = ((*q).varptr = (struct qq_decls$varrec *)mlib$pcm_alloc((length * (i64)16)));
    plist = (*p).varptr;
    $av_1 = length;
    while ($av_1-- > 0) {
L847 :;
        (*qlist) = (*plist);
        if (((i64)(*qlist).tag == (i64)12)) {
            if (!!((i64)(*qlist).hasref)) {
                ++((*(*qlist).objptr).refcount);
            }
;
        }
        else {
            if (!!((i64)(*qlist).hasref)) {
                qq_vars$var_duplu(qlist);
            }
;
        }
;
        ++(qlist);
        ++(plist);
L848 :;
    }
L849 :;
    ;
}

i64 qq_records$var_equal_record(struct qq_decls$varrec *x,struct qq_decls$varrec *y) {
        struct qq_decls$objrec *  px;
        struct qq_decls$objrec *  py;
        struct qq_decls$varrec *  a;
        struct qq_decls$varrec *  b;
        i64 $av_1;
    px = (*x).objptr;
    py = (*y).objptr;
    if (((i64)(*px).usertag != (i64)(*py).usertag)) {
        return (i64)0;
    }
;
    if ((px == py)) {
        return (i64)1;
    }
;
    a = (*px).varptr;
    b = (*py).varptr;
    $av_1 = qq_tables$ttlength[((i64)(*px).usertag)];
    while ($av_1-- > 0) {
L850 :;
        if ((qq_vars$var_equal(a,b) == (i64)0)) {
            return (i64)0;
        }
;
        ++(a);
        ++(b);
L851 :;
    }
L852 :;
    ;
    return (i64)1;
}

void qq_records$var_getix_record(struct qq_decls$varrec *a,i64 index) {
        struct qq_decls$objrec *  q;
        u64 offset;
    q = (*a).objptr;
    offset = (u64)(index - (i64)1);
    if ((offset >= (u64)qq_tables$ttlength[((i64)(*q).usertag)])) {
        qq_runaux$pcerror((byte*)"record[int] bounds",(byte*)"");
    }
;
    (*a) = (*((*q).varptr + (i64)offset));
    if (!!((i64)(*a).hasref)) {
        ++((*(*a).objptr).refcount);
    }
;
}

void qq_records$var_putix_record(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *x) {
        struct qq_decls$varrec *  dest;
        struct qq_decls$objrec *  q;
        u64 offset;
    q = (*a).objptr;
    if (!(!!(msysc$m_getdotindex((i64)(*q).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    offset = (u64)(index - (i64)1);
    if ((offset >= (u64)qq_tables$ttlength[((i64)(*q).usertag)])) {
        qq_runaux$pcerror((byte*)"rec[int] bounds",(byte*)"");
    }
;
    dest = ((*q).varptr + (i64)offset);
    if (!!((i64)(*dest).hasref)) {
        qq_vars$var_unshareu(dest);
    }
;
    (*dest) = (*x);
}

void qq_records$var_getixref_record(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *dest) {
        struct qq_decls$varrec *  p;
        struct qq_decls$objrec *  q;
        u64 offset;
    q = (*a).objptr;
    offset = (u64)(index - (i64)1);
    if ((offset >= (u64)(*q).length)) {
        qq_runaux$pcerror((byte*)"^rec[int] bounds",(byte*)"");
    }
;
    p = ((*q).varptr + (i64)offset);
    (*dest).tagx = (i64)14;
    (*dest).varptr = p;
}

// START
void qq_records$start(void) {

}

void qq_resolve$rx_module(struct qq_decls$filerec *pm) {
    qq_decls$currmodule = pm;
    qq_decls$stcurrproc = (qq_decls$stcurrmodule = (*qq_decls$currmodule).def);
    qq_resolve$nprocs = (i64)0;
    qq_resolve$rx_passdef(qq_decls$stprogram,qq_decls$stcurrmodule);
    if ((qq_resolve$nprocs == (i64)0)) {
        qq_resolve$rx_unit(qq_decls$stcurrmodule,(*qq_decls$currmodule).ast);
    }
    else if (!!((*qq_decls$currmodule).ast)) {
        qq_resolve$rx_unit(qq_decls$stcurrmodule,(*qq_decls$currmodule).ast);
    }
;
}

void qq_resolve$rx_passdef(struct qq_decls$strec *owner,struct qq_decls$strec *p) {
        {i64 $temp = (i64)(*p).nameid;
if (($temp==(i64)3)) {
        qq_resolve$rx_deflist(p,(*p).deflist,(i64)0);
    }
    else if (($temp==(i64)5) || ($temp==(i64)6)) {
        ++(qq_resolve$nprocs);
        qq_resolve$fixmode((struct qq_decls$strec *)owner,(struct qq_decls$strec *)p);
        qq_resolve$rx_deflist(p,(*p).deflist,(i64)0);
        qq_decls$stcurrproc = p;
        qq_resolve$rx_unit(p,(*p).code);
        qq_decls$stcurrproc = qq_decls$stcurrmodule;
        qq_resolve$rx_deflist(p,(*p).deflist,(i64)1);
    }
    else if (($temp==(i64)7)) {
        qq_resolve$fixmode((struct qq_decls$strec *)owner,(struct qq_decls$strec *)p);
        qq_resolve$rx_deflist(p,(*p).deflist,(i64)0);
    }
    else if (($temp==(i64)18) || ($temp==(i64)13) || ($temp==(i64)14) || ($temp==(i64)15)) {
        qq_resolve$fixmode((struct qq_decls$strec *)owner,(struct qq_decls$strec *)p);
        if (!!((*p).code)) {
            qq_resolve$rx_unit(owner,(*p).code);
        }
;
    }
    else if (($temp==(i64)10) || ($temp==(i64)9)) {
        qq_resolve$fixmode((struct qq_decls$strec *)owner,(struct qq_decls$strec *)p);
        qq_resolve$rx_deflist(p,(*p).deflist,(i64)0);
    }
    };
}

void qq_resolve$rx_deflist(struct qq_decls$strec *owner,struct qq_decls$strec *p,i64 doanon) {
    L853 :;
    while (!!(p)) {
        if (((!!(doanon) && ((i64)(*p).nameid == (i64)6)) || ((doanon == (i64)0) && ((i64)(*p).nameid != (i64)6)))) {
            qq_resolve$rx_passdef(owner,p);
        }
;
        p = (*p).nextdef;
L854 :;
    }
L855 :;
    ;
}

void qq_resolve$rx_unit(struct qq_decls$strec *owner,struct qq_decls$unitrec *p) {
        struct qq_decls$strec *  d;
        struct qq_decls$unitrec *  a;
        struct qq_decls$unitrec *  b;
        i64 n;
        i64 flags;
        i64 oldnoexpand;
        i64 oldsymbolmode;
        i64 nk;
        struct qq_decls$filerec *  pm;
    a = (*p).a;
    b = (*p).b;
    qq_decls$qpos = (i64)(*p).pos;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)39)) {
        qq_resolve$resolvename(owner,p,(i64)0);
        if (((((i64)(*p).tag == (i64)39) && ((i64)(*(*p).def).nameid == (i64)22)) && !(!!(qq_resolve$noexpand)))) {
            ++(qq_resolve$macrolevels);
            qq_resolve$expandmacro(p,p,0);
            qq_resolve$rx_unit(owner,p);
            --(qq_resolve$macrolevels);
        }
;
    }
    else if (($temp==(i64)3)) {
        qq_resolve$rx_unit(owner,b);
    }
    else if (($temp==(i64)46)) {
        qq_resolve$resolvedot(owner,p);
    }
    else if (($temp==(i64)26)) {
        if (((i64)(*a).tag == (i64)39)) {
            oldnoexpand = qq_resolve$noexpand;
            qq_resolve$noexpand = (i64)1;
            qq_resolve$rx_unit(owner,a);
            qq_resolve$noexpand = oldnoexpand;
        }
        else {
            qq_resolve$rx_unit(owner,a);
        }
;
        qq_resolve$rx_unitlist(owner,b);
        if (((i64)(*a).tag == (i64)34)) {
            (*p).tag = (i64)35;
            (*p).a = b;
            (*p).b = 0;
            (*p).mode = (i64)(*a).mode;
            nk = (i64)0;
            (*p).a = qq_lib$createunit1((i64)88,b);
            n = (i64)0;
            L856 :;
            while (!!(b)) {
                if (((i64)(*b).tag == (i64)3)) {
                    ++(nk);
                    (*b).tag = (i64)4;
                }
;
                ++(n);
                b = (*b).nextunit;
L857 :;
            }
L858 :;
            ;
            if ((!!(nk) && (nk != n))) {
                qq_lib$rxerror((byte*)"Mixed key:value",0);
            }
;
            if (!!((*a).nextunit)) {
                n = -(n);
            }
;
            (*(*p).a).length = n;
        }
        else if ((((i64)(*a).tag == (i64)39) && ((i64)(*(*a).def).nameid == (i64)22))) {
            ++(qq_resolve$macrolevels);
            qq_resolve$expandmacro(p,a,b);
            qq_resolve$rx_unit(owner,p);
            --(qq_resolve$macrolevels);
        }
;
    }
    else if (($temp==(i64)63) || ($temp==(i64)87)) {
        qq_resolve$rx_unit(owner,a);
        if (!(!!(b))) {
            qq_lib$rxerror((byte*)"Binop missing opnd",0);
        }
;
        qq_resolve$rx_unit(owner,b);
        qq_resolve$evalbinop(p,a,b);
    }
    else if (($temp==(i64)62) || ($temp==(i64)66)) {
        qq_resolve$rx_unit(owner,a);
        qq_resolve$evalmonop(p);
    }
    else if (($temp==(i64)15)) {
        qq_resolve$resolvename(owner,a,(i64)1);
        a = (*a).nextunit;
        goto L859 ;
;
    }
    else if (($temp==(i64)35)) {
        qq_resolve$rx_unit(owner,a);
        qq_resolve$evalmonop(p);
    }
    else if (($temp==(i64)40)) {
        oldnoexpand = qq_resolve$noexpand;
        oldsymbolmode = qq_resolve$symbolmode;
        qq_resolve$noexpand = (i64)1;
        qq_resolve$symbolmode = (i64)1;
        qq_resolve$rx_unit(owner,a);
        qq_resolve$noexpand = oldnoexpand;
        qq_resolve$symbolmode = oldsymbolmode;
                {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)39)) {
        }
        else if (($temp==(i64)34)) {
            d = qq_tables$ttnamedef[((i64)(*a).mode)];
            if (!!(d)) {
                (*a).def = d;
                (*a).tag = (i64)39;
            }
            else {
                qq_lib$rxerror((byte*)"T.$?",0);
            }
;
        }
        else {
            qq_show$printunit((struct qq_decls$unitrec *)a,(i64)0,(byte*)"*",0);
            qq_lib$rxerror((byte*)".$ not name",0);
        }
        };
    }
    else if (($temp==(i64)45)) {
        qq_resolve$rx_unit(owner,a);
        if (((i64)(*a).tag != (i64)43)) {
            qq_lib$rxerror((byte*)"Not strconst",0);
        }
;
        pm = qq_modules$loadsourcefile((*a).svalue,(i64)0);
        (*a).svalue = (*pm).text;
        (*a).slength = ((*pm).size - (i64)1);
        qq_lib$deleteunit(p,a);
    }
    else {
        //doabc:
L859 :;
;
        flags = (i64)qq_tables$jflags[((i64)(*p).tag)];
        if ((flags >= (i64)1)) {
            qq_resolve$rx_unitlist(owner,a);
        }
;
        if ((flags == (i64)2)) {
            qq_resolve$rx_unitlist(owner,b);
        }
;
    }
    };
}

static void qq_resolve$rx_unitlist(struct qq_decls$strec *owner,struct qq_decls$unitrec *p) {
    L860 :;
    while (!!(p)) {
        qq_resolve$rx_unit(owner,p);
        p = (*p).nextunit;
L861 :;
    }
L862 :;
    ;
}

static void qq_resolve$evalmonop(struct qq_decls$unitrec *p) {
        i64 a;
        i64 c;
        r64 x;
        r64 z;
    {
                {i64 $temp = (i64)(*(*p).a).tag;
if (($temp==(i64)41)) {
            a = (*(*p).a).value;
                        {i64 $temp = (i64)(*p).pclop;
if (($temp==(i64)74)) {
                c = -(a);
            }
            else if (($temp==(i64)75)) {
                c = m$llabs(a);
            }
            else {
                return;
            }
            };
            //newint:
L863 :;
;
            qq_resolve$makeintconst((struct qq_decls$unitrec *)p,c);
        }
        else if (($temp==(i64)42)) {
            x = (*(*p).a).xvalue;
                        {i64 $temp = (i64)(*p).pclop;
if (($temp==(i64)74)) {
                z = -(x);
            }
            else if (($temp==(i64)75)) {
                z = fabs(x);
            }
            else {
                return;
            }
            };
            qq_resolve$makerealconst((struct qq_decls$unitrec *)p,z);
        }
        else {
            return;
        }
        };
    }
;
}

static void qq_resolve$evalbinop(struct qq_decls$unitrec *p,struct qq_decls$unitrec *lhs,struct qq_decls$unitrec *rhs) {
        i64 a;
        i64 b;
        i64 c;
        r64 x;
        r64 y;
        r64 z;
        {i64 $temp = (((i64)(*lhs).tag << (i64)16) | (i64)(*rhs).tag);
if (($temp==(i64)2687017)) {
        a = (*lhs).value;
        b = (*rhs).value;
                {i64 $temp = (i64)(*p).pclop;
if (($temp==(i64)100)) {
            c = (a + b);
        }
        else if (($temp==(i64)101)) {
            c = (a - b);
        }
        else if (($temp==(i64)102)) {
            c = (a * b);
        }
        else if (($temp==(i64)104)) {
            if ((b == (i64)0)) {
                qq_lib$rxerror((byte*)"x/0",0);
            }
;
            c = (a / b);
        }
        else if (($temp==(i64)120)) {
            c = msysc$m_power_i64(b,a);
        }
        else {
            return;
        }
        };
        qq_resolve$makeintconst((struct qq_decls$unitrec *)p,c);
    }
    else if (($temp==(i64)2752554)) {
        x = (*lhs).xvalue;
        y = (*rhs).xvalue;
                {i64 $temp = (i64)(*p).pclop;
if (($temp==(i64)100)) {
            z = (x + y);
        }
        else if (($temp==(i64)101)) {
            z = (x - y);
        }
        else if (($temp==(i64)102)) {
            z = (x * y);
        }
        else if (($temp==(i64)103)) {
            z = (x / y);
        }
        else {
            return;
        }
        };
        qq_resolve$makerealconst((struct qq_decls$unitrec *)p,z);
    }
    else {
        return;
    }
    };
}

static void qq_resolve$makeintconst(struct qq_decls$unitrec *p,i64 value) {
    (*p).tag = (i64)41;
    (*p).a = ((*p).b = 0);
    (*p).value = value;
    (*p).mode = (i64)1;
}

static void qq_resolve$makerealconst(struct qq_decls$unitrec *p,r64 xvalue) {
    (*p).tag = (i64)42;
    (*p).a = ((*p).b = 0);
    (*p).xvalue = xvalue;
    (*p).mode = (i64)2;
}

void qq_resolve$resolvename(struct qq_decls$strec *owner,struct qq_decls$unitrec *p,i64 mode) {
        struct qq_decls$strec *  d;
        struct qq_decls$strec *  e;
        struct qq_decls$unitrec *  q;
        i64 moduleno;
    d = (*p).def;
    moduleno = msysc$m_getdotslice((i64)(*p).pos,(i64)24,(i64)31);
    if (((i64)(*d).nameid != (i64)0)) {
        return;
    }
;
    e = qq_resolve$resolvetopname(owner,d,moduleno,qq_resolve$allowmodname);
    if (!(!!(e))) {
                {i64 $temp = (i64)(*owner).nameid;
if (($temp==(i64)5) || ($temp==(i64)6)) {
            e = ((*p).def = qq_names$addsymbol(owner,d,(i64)14,(i64)0));
        }
        else if (($temp==(i64)3)) {
            e = ((*p).def = qq_names$addsymbol(owner,d,(i64)13,(i64)0));
        }
        else {
            qq_lib$rxerror_s((byte*)"Undefined: #",(*d).name,p);
        }
        };
    }
    else {
        //$else:
L864 :;
;
        //retry:
L865 :;
;
        (*p).def = e;
                {i64 $temp = (i64)(*e).nameid;
if (($temp==(i64)18)) {
            if (!!(qq_resolve$symbolmode)) {
                return;
            }
;
            q = (*e).code;
            qq_resolve$rx_unit(owner,q);
            if (!(((i64)(*q).tag == (i64)41 || (i64)(*q).tag == (i64)42 || (i64)(*q).tag == (i64)43))) {
                qq_lib$rxerror_s((byte*)"Not const expr: #",qq_tables$jtagnames[((i64)(*q).tag)],0);
            }
;
            (*e).mode = (i64)(*q).mode;
            (*p).tag = (i64)(*q).tag;
            (*p).value = (*q).value;
            (*p).mode = (i64)(*q).mode;
            (*p).slength = (*q).slength;
        }
        else if (($temp==(i64)19)) {
            if (!!(qq_resolve$symbolmode)) {
                return;
            }
;
            (*p).tag = (i64)41;
            (*p).value = (i64)(*e).index;
            (*p).mode = (i64)1;
        }
        else if (($temp==(i64)13)) {
        }
        else if (($temp==(i64)10) || ($temp==(i64)9)) {
            (*p).tag = (i64)34;
            (*p).mode = (i64)(*(*p).def).mode;
        }
        else if (($temp==(i64)21)) {
            qq_lib$rxerror((byte*)"FOUND LINK",p);
        }
        else if (($temp==(i64)14) || ($temp==(i64)15)) {
            if ((((i64)(*qq_decls$stcurrproc).nameid == (i64)6) && ((i64)(*(*e).owner).nameid != (i64)6))) {
                qq_lib$rxerror((byte*)"Accessing transient vars from {}",0);
            }
;
        }
        };
    }
;
}

struct qq_decls$strec *qq_resolve$resolvetopname(struct qq_decls$strec *owner,struct qq_decls$strec *stnewname,i64 moduleno,i64 allowmod) {
        i64 extcount;
        i64 subprogno;
        struct qq_decls$strec *  p;
        struct qq_decls$strec *  q;
        struct qq_decls$strec *  powner;
        struct qq_decls$strec *  extdef;
        struct qq_decls$strec *  moddef;
        struct qq_decls$strec *  ambiglist[10];
        i64 i;
    if (((i64)(*owner).nameid == (i64)6)) {
        q = (*owner).deflist;
        L866 :;
        while (!!(q)) {
            if (((*q).firstdupl == stnewname)) {
                return q;
            }
;
L867 :;
            q = (*q).nextdef;
L869 :;
                    }
L868 :;
        ;
        owner = (*owner).owner;
    }
;
    if (((i64)(*owner).nameid == (i64)5)) {
        q = (*owner).deflist;
        L870 :;
        while (!!(q)) {
            if (((*q).firstdupl == stnewname)) {
                return q;
            }
;
L871 :;
            q = (*q).nextdef;
L873 :;
                    }
L872 :;
        ;
    }
;
    p = (*stnewname).nextdupl;
    subprogno = (i64)(*qq_decls$modules[(moduleno)]).subprogno;
    extcount = (i64)0;
    extdef = (moddef = 0);
    L874 :;
    while (!!(p)) {
        powner = (*p).owner;
                {i64 $temp = (i64)(*powner).nameid;
if (($temp==(i64)3)) {
            if (((i64)(*powner).moduleno == moduleno)) {
                return p;
            }
            else if (!!(msysc$m_getdotslice((i64)(*p).flags,(i64)0,(i64)1))) {
                if (((((i64)(*qq_decls$modules[((i64)(*powner).moduleno)]).subprogno == subprogno) || ((i64)msysc$m_getdotslice((i64)(*p).flags,(i64)0,(i64)1) == (i64)2)) || !!(msysc$m_getdotindex((i64)(*p).flags,(i64)2)))) {
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
        else if (($temp==(i64)10)) {
            if (((powner == owner) || (powner == (*owner).owner))) {
                return p;
            }
;
        }
        else if (($temp==(i64)1)) {
                        {i64 $temp = (i64)(*p).nameid;
if (($temp==(i64)3) || ($temp==(i64)2)) {
                if (!!(allowmod)) {
                    moddef = p;
                }
;
            }
            else if (($temp==(i64)22)) {
                return p;
            }
            };
        }
        };
L875 :;
        p = (*p).nextdupl;
L877 :;
            }
L876 :;
    ;
    if (!!(extdef)) {
        if ((extcount > (i64)1)) {
            for (i=(i64)1;i<=extcount;++i) {
L878 :;
                extdef = ambiglist[(i)-1];
                msysc$m_print_startcon();
                msysc$m_print_i64(i,NULL);
                msysc$m_print_str((*(*extdef).owner).name,NULL);
                msysc$m_print_str(qq_tables$namenames[((i64)(*(*extdef).owner).nameid)],NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
L879 :;
            }
L880 :;
            ;
            qq_lib$rxerror_s((byte*)"Ambiguous ext name: #",(*extdef).name,0);
        }
;
        return extdef;
    }
;
    return moddef;
}

static void qq_resolve$resolvedot(struct qq_decls$strec *owner,struct qq_decls$unitrec *p) {
        struct qq_decls$strec *  rdef;
        struct qq_decls$strec *  d;
        struct qq_decls$strec *  newd;
        struct qq_decls$strec *  e;
        struct qq_decls$strec *  fielddef;
        struct qq_decls$unitrec *  q;
        struct qq_decls$unitrec *  r;
        i64 nfields;
        i64 oldallowmod;
    if (!!(qq_resolve$symbolmode)) {
        qq_resolve$resolvedot_sym(owner,p);
        return;
    }
;
    q = (*p).a;
    r = (*p).b;
    rdef = (*r).def;
    oldallowmod = qq_resolve$allowmodname;
    qq_resolve$allowmodname = (i64)((i64)(*q).tag == (i64)39);
    qq_resolve$rx_unit(owner,q);
    qq_resolve$allowmodname = oldallowmod;
        {i64 $temp = (i64)(*q).tag;
if (($temp==(i64)39)) {
        d = (*q).def;
    }
    else if (($temp==(i64)34)) {
        d = (*q).def;
        goto L881 ;
;
    }
    else {
        rdef = (*r).def;
        goto L882 ;
;
    }
    };
        {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)3) || ($temp==(i64)10) || ($temp==(i64)5) || ($temp==(i64)7)) {
        //dotype:
L881 :;
;
        newd = qq_resolve$finddupl(d,rdef);
        if (!!(newd)) {
            switch ((i64)(*newd).nameid) {
            case 19:;
                {
                    (*p).tag = (i64)41;
                    (*p).value = (i64)(*newd).index;
                    (*p).mode = (i64)1;
                }
                break;
            case 18:;
                {
                    q = (*newd).code;
                                        {i64 $temp = (i64)(*q).tag;
if (($temp==(i64)41)) {
                        (*p).tag = (i64)41;
                        (*p).a = ((*p).b = 0);
                        (*p).value = (*q).value;
                        (*p).mode = (i64)(*newd).mode;
                    }
                    else {
                        qq_lib$rxerror((byte*)"Rxdot:const?",p);
                    }
                    };
                }
                break;
            case 10:;
                {
                    (*p).tag = (i64)34;
                    (*p).mode = (i64)(*newd).mode;
                    (*p).def = newd;
                }
                break;
            case 13:;
                {
                    (*p).tag = (i64)39;
                    (*p).def = newd;
                }
                break;
            case 5:;
            case 7:;
                {
                    (*p).tag = (i64)39;
                    (*p).a = ((*p).b = 0);
                    (*p).def = newd;
                }
                break;
            case 22:;
                {
                    if ((((i64)(*e).nameid == (i64)22) && !(!!(qq_resolve$noexpand)))) {
                        ++(qq_resolve$macrolevels);
                        qq_resolve$expandmacro(p,p,0);
                        qq_resolve$rx_unit(owner,p);
                        --(qq_resolve$macrolevels);
                    }
;
                }
                break;
            default: {
                msysc$m_print_startcon();
                msysc$m_print_str(qq_tables$namenames[((i64)(*newd).nameid)],NULL);
                msysc$m_print_nogap();
                msysc$m_print_str((byte*)".",NULL);
                msysc$m_print_nogap();
                msysc$m_print_str((*newd).name,NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                qq_lib$rxerror((byte*)"Rxdot:.name not allowed here",p);
            }
            } //SW
;
        }
        else {
            msysc$m_print_startcon();
            msysc$m_print_str((*d).name,NULL);
            msysc$m_print_nogap();
            msysc$m_print_str((byte*)".",NULL);
            msysc$m_print_nogap();
            msysc$m_print_str((*rdef).name,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            qq_lib$rxerror((byte*)"Can't resolve",p);
        }
;
    }
    else if (($temp==(i64)14) || ($temp==(i64)13) || ($temp==(i64)15) || ($temp==(i64)11) || ($temp==(i64)12)) {
        //doexprdot:
L882 :;
;
        nfields = (i64)0;
        fielddef = 0;
        e = (*rdef).nextdupl;
        L883 :;
        while (!!(e)) {
                        {i64 $temp = (i64)(*e).nameid;
if (($temp==(i64)11) || ($temp==(i64)12) || ($temp==(i64)18) || ($temp==(i64)5) || ($temp==(i64)10) || ($temp==(i64)13) || ($temp==(i64)7)) {
                ++(nfields);
                fielddef = e;
            }
            };
            e = (*e).nextdupl;
L884 :;
        }
L885 :;
        ;
        if ((nfields==(i64)0)) {
            msysc$m_print_startcon();
            msysc$m_print_str((*rdef).name,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            qq_lib$rxerror((byte*)"Can't find field",0);
        }
        else {
            if (((i64)(*rdef).nameid != (i64)0)) {
                qq_lib$rxerror((byte*)"Field name not generic",0);
            }
;
        }
;
    }
    else {
        msysc$m_print_startcon();
        msysc$m_print_str(qq_tables$namenames[((i64)(*d).nameid)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        qq_lib$rxerror((byte*)"RXDOT:Unknown nameid",p);
    }
    };
}

static void qq_resolve$resolvedot_sym(struct qq_decls$strec *owner,struct qq_decls$unitrec *p) {
        struct qq_decls$strec *  rdef;
        struct qq_decls$strec *  d;
        struct qq_decls$strec *  newd;
        struct qq_decls$unitrec *  q;
        struct qq_decls$unitrec *  r;
        i64 oldallowmod;
    q = (*p).a;
    r = (*p).b;
    rdef = (*r).def;
    oldallowmod = qq_resolve$allowmodname;
    qq_resolve$allowmodname = (i64)((i64)(*q).tag == (i64)39);
    qq_resolve$rx_unit(owner,q);
    qq_resolve$allowmodname = oldallowmod;
        {i64 $temp = (i64)(*q).tag;
if (($temp==(i64)39)) {
        d = (*q).def;
    }
    else if (($temp==(i64)34)) {
        d = (*q).def;
        if (!!(qq_resolve$symbolmode)) {
            newd = qq_resolve$finddupl(d,rdef);
            if ((newd == 0)) {
                qq_lib$rxerror_s((byte*)"Can't resolve .",(*rdef).name,0);
            }
;
                        {i64 $temp = (i64)(*newd).nameid;
if (($temp==(i64)11) || ($temp==(i64)12)) {
                msysc$m_print_startcon();
                msysc$m_print_str((byte*)"*******FIELD.$",NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
            }
            else {
                qq_lib$rxerror_s((byte*)".$ ON type:",qq_tables$namenames[((i64)(*newd).nameid)],0);
            }
            };
        }
;
        goto L886 ;
;
    }
    else {
        qq_lib$rxerror((byte*)"RXDOTSYM?",0);
    }
    };
        {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)3) || ($temp==(i64)10) || ($temp==(i64)5) || ($temp==(i64)7)) {
        //dotype:
L886 :;
;
        newd = qq_resolve$finddupl(d,rdef);
        if (!!(newd)) {
            (*p).tag = (i64)39;
            (*p).a = ((*p).b = 0);
            (*p).def = newd;
        }
        else {
            qq_lib$rxerror_s((byte*)".$ Can't resolve",(*d).name,0);
        }
;
    }
    else {
        qq_lib$rxerror_s((byte*)"RX.$: Unknown nameid:",qq_tables$namenames[((i64)(*d).nameid)],p);
    }
    };
}

struct qq_decls$strec *qq_resolve$finddupl(struct qq_decls$strec *d,struct qq_decls$strec *pdupl) {
    if (((i64)(*pdupl).nameid != (i64)0)) {
        return pdupl;
    }
;
    pdupl = (*pdupl).nextdupl;
    L887 :;
    while (!!(pdupl)) {
        if (((*pdupl).owner == d)) {
            return pdupl;
        }
;
        pdupl = (*pdupl).nextdupl;
L888 :;
    }
L889 :;
    ;
    return (struct qq_decls$strec *)0;
}

static void qq_resolve$expandmacro(struct qq_decls$unitrec *p,struct qq_decls$unitrec *a,struct qq_decls$unitrec *b) {
        struct qq_decls$strec *  d;
        struct qq_decls$strec *  pm;
        struct qq_decls$unitrec *  pnew;
        i64 ignoreargs;
    if ((qq_resolve$macrolevels > (i64)10)) {
        qq_lib$rxerror((byte*)"Too many macro levels (recursive macro?)",0);
    }
;
    d = (*a).def;
    pm = (*d).deflist;
    qq_resolve$nmacroparams = (i64)0;
    L890 :;
    while (!!(pm)) {
        if ((qq_resolve$nmacroparams >= (i64)50)) {
            qq_lib$rxerror((byte*)"macro param overflow",0);
        }
;
        qq_resolve$macroparams[(++(qq_resolve$nmacroparams))-1] = pm;
        qq_resolve$macroparamsgen[(qq_resolve$nmacroparams)-1] = (*pm).firstdupl;
        pm = (*pm).nextdef;
L891 :;
    }
L892 :;
    ;
    qq_resolve$nmacroargs = (i64)0;
    L893 :;
    while (!!(b)) {
        if ((qq_resolve$nmacroargs >= (i64)50)) {
            qq_lib$rxerror((byte*)"macro arg overflow",0);
        }
;
        qq_resolve$macroargs[(++(qq_resolve$nmacroargs))-1] = b;
        b = (*b).nextunit;
L894 :;
    }
L895 :;
    ;
    if ((qq_resolve$nmacroargs < qq_resolve$nmacroparams)) {
        qq_lib$rxerror((byte*)"Too few macro args",0);
    }
;
    ignoreargs = (i64)0;
    if (((qq_resolve$nmacroargs > (i64)0) && (qq_resolve$nmacroparams == (i64)0))) {
        ignoreargs = (i64)1;
        qq_resolve$nmacroargs = (qq_resolve$nmacroparams = (i64)0);
    }
    else if ((qq_resolve$nmacroargs > qq_resolve$nmacroparams)) {
        qq_lib$rxerror((byte*)"Too many macro args",0);
    }
;
    pnew = qq_resolve$copyunit((*d).code);
    if (!(!!(ignoreargs))) {
        qq_resolve$replaceunit(p,pnew);
    }
    else {
        (*p).a = pnew;
    }
;
}

static struct qq_decls$unitrec *qq_resolve$copylistunit(struct qq_decls$unitrec *p) {
        struct qq_decls$unitrec *  q;
        struct qq_decls$unitrec *  plist;
        struct qq_decls$unitrec *  plistx;
    plist = (plistx = 0);
    L896 :;
    while (!!(p)) {
        q = qq_resolve$copyunit(p);
        qq_lib$addlistunit(&plist,&plistx,q);
        p = (*p).nextunit;
L897 :;
    }
L898 :;
    ;
    return plist;
}

static struct qq_decls$unitrec *qq_resolve$copyunit(struct qq_decls$unitrec *p) {
        struct qq_decls$unitrec *  q;
        struct qq_decls$strec *  d;
        i64 i;
    if ((p == 0)) {
        return 0;
    }
;
    if (((i64)(*p).tag == (i64)39)) {
        d = (*p).def;
        for (i=(i64)1;i<=qq_resolve$nmacroparams;++i) {
L899 :;
            if ((qq_resolve$macroparamsgen[(i)-1] == d)) {
                return qq_resolve$copyunit(qq_resolve$macroargs[(i)-1]);
                goto L901 ;
            }
;
L900 :;
        }
L901 :;
        ;
    }
;
    q = qq_lib$createunit0((i64)(*p).tag);
    (*q) = (*p);
    (*q).nextunit = 0;
    if (!!((i64)qq_tables$jflags[((i64)(*q).tag)])) {
        (*q).a = qq_resolve$copylistunit((*q).a);
        if (((i64)qq_tables$jflags[((i64)(*q).tag)] == (i64)2)) {
            (*q).b = qq_resolve$copylistunit((*q).b);
        }
;
    }
;
    return q;
}

static void qq_resolve$replaceunit(struct qq_decls$unitrec *p,struct qq_decls$unitrec *q) {
        struct qq_decls$unitrec *  pnext;
    pnext = (*p).nextunit;
    (*p) = (*q);
    (*p).nextunit = pnext;
}

static void qq_resolve$fixmode(struct qq_decls$strec *owner,struct qq_decls$strec *p) {
        struct qq_decls$strec *  d;
        struct qq_decls$strec *  e;
        i64 m;
    m = (i64)(*p).mode;
    if ((m >= (i64)0)) {
        return;
    }
;
    m = -(m);
    if (!!(qq_tables$ttxmap[(m)])) {
        (*p).mode = qq_tables$ttxmap[(m)];
        return;
    }
;
    d = (struct qq_decls$strec *)qq_tables$ttnamedefx[(m)];
    e = (struct qq_decls$strec *)qq_resolve$resolvetopname((struct qq_decls$strec *)owner,(struct qq_decls$strec *)d,(i64)qq_tables$ttxmoduleno[(m)],(i64)0);
    if (!!(e)) {
        qq_tables$ttxmap[(m)] = (i64)(*e).mode;
        (*p).mode = (i64)(*e).mode;
    }
    else {
        qq_lib$rxerror_s((byte*)"Can't resolve type: #",(*d).name,0);
    }
;
}

static i64 qq_resolve$fixmode2(struct qq_decls$strec *owner,i64 m) {
        struct qq_decls$strec *  d;
        struct qq_decls$strec *  e;
        u8 str[256];
    if ((m >= (i64)0)) {
        return m;
    }
;
    m = -(m);
    if (!!(qq_tables$ttxmap[(m)])) {
        return qq_tables$ttxmap[(m)];
    }
;
    d = (struct qq_decls$strec *)qq_tables$ttnamedefx[(m)];
    if ((owner == 0)) {
        qq_lib$rxerror((byte*)"FM2/owner",0);
    }
;
    e = (struct qq_decls$strec *)qq_resolve$resolvetopname((struct qq_decls$strec *)owner,(struct qq_decls$strec *)d,(i64)qq_tables$ttxmoduleno[(m)],(i64)0);
    if (!!(e)) {
        qq_tables$ttxmap[(m)] = (i64)(*e).mode;
        return (i64)(*e).mode;
    }
    else {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"# in module #, line:#");
        msysc$m_print_str((*d).name,NULL);
        msysc$m_print_str((*qq_decls$modules[((i64)qq_tables$ttxmoduleno[(m)])]).name,NULL);
        msysc$m_print_end();
        ;
        qq_lib$rxerror_s((byte*)"2:Can't resolve type: #",(u8 *)str,0);
    }
;
    return (i64)0;
}

void qq_resolve$fixusertypes(void) {
        struct qq_decls$userxrec *  p;
        i64 m;
        i64 rescan;
        i64 i;
    for (i=(i64)1;i<=(i64)2;++i) {
L902 :;
        p = (struct qq_decls$userxrec *)qq_tables$userxmodelist;
        rescan = (i64)0;
        L905 :;
        while (!!(p)) {
            m = (i64)(*(*p).pmode);
            if ((m < (i64)0)) {
                m = qq_resolve$fixmode2((struct qq_decls$strec *)(*p).owner,m);
                if ((((m < (i64)0) && (i == (i64)2)) && !!(qq_tables$ttxmap[(m$llabs(m))]))) {
                    m = qq_tables$ttxmap[(m$llabs(m))];
                }
;
                if ((m < (i64)0)) {
                    rescan = (i64)1;
                }
                else {
                    (*(*p).pmode) = m;
                    if (((i64)qq_tables$tttarget[(m)] == m)) {
                        qq_lib$rxerror_s((byte*)"recursive type?",qq_tables$ttname[(m)],0);
                    }
;
                }
;
            }
;
            p = (struct qq_decls$userxrec *)(*p).nextmode;
L906 :;
        }
L907 :;
        ;
        if (!(!!(rescan))) {
            goto L904 ;
        }
;
L903 :;
    }
L904 :;
    ;
    if (!!(rescan)) {
        qq_lib$rxerror((byte*)"FUT Phase Error",0);
    }
;
    for (i=(i64)1;i<=qq_decls$nbaseclasses;++i) {
L908 :;
        qq_resolve$dobaseclass(i);
L909 :;
    }
L910 :;
    ;
}

void qq_resolve$tx_typetable(void) {
        i64 i;
    for (i=(i64)41;i<=qq_tables$ntypes;++i) {
L911 :;
        qq_resolve$converttype(i);
L912 :;
    }
L913 :;
    ;
}

static i64 qq_resolve$getconstint(struct qq_decls$strec *owner,struct qq_decls$unitrec *a,i64 ownerid) {
    qq_resolve$rx_unit(owner,a);
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)41)) {
        return (*a).value;
    }
    else if (($temp==(i64)42)) {
        return (i64)(*a).xvalue;
    }
    else {
        qq_lib$rxerror_s((byte*)"Getconstint: not int/real",qq_tables$jtagnames[((i64)(*a).tag)],0);
    }
    };
    return (i64)0;
}

void qq_resolve$converttype(i64 m) {
        struct qq_decls$strec *  d;
        struct qq_decls$strec *  f;
        struct qq_decls$strec *  owner;
        i64 index;
        i64 elemtype;
        i64 nbits;
        struct qq_decls$strec *  fieldlist[257];
        i64 ownerid;
        i64 maxalign;
        i64 nfields;
        i64 size;
        struct qq_decls$unitrec *  plength;
        struct qq_decls$unitrec *  plower;
    if (!!(qq_tables$ttsize[(m)])) {
        return;
    }
;
    owner = qq_tables$ttowner[(m)];
    plower = qq_tables$ttlowerexpr[(m)];
    plength = qq_tables$ttlengthexpr[(m)];
        {i64 $temp = (i64)qq_tables$ttbasetype[(m)];
if (($temp==(i64)36) || ($temp==(i64)37)) {
        qq_tables$ttsize[(m)] = (qq_tables$ttlength[(m)] = qq_resolve$getconstint(owner,plength,(i64)0));
    }
    else if (($temp==(i64)7)) {
        if ((m == (i64)11)) {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"CT:ARRAY/ARRAY",NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
;
        if (!!(qq_tables$ttowner[(m)])) {
            ownerid = (i64)(*qq_tables$ttowner[(m)]).nameid;
        }
        else {
            ownerid = (i64)0;
        }
;
        if (!!(plower)) {
            qq_tables$ttlower[(m)] = qq_resolve$getconstint(owner,plower,ownerid);
        }
        else {
            qq_tables$ttlower[(m)] = (i64)1;
        }
;
        if (!!(plength)) {
            qq_tables$ttlength[(m)] = qq_resolve$getconstint(owner,plength,ownerid);
        }
        else {
            qq_tables$ttlength[(m)] = (i64)0;
        }
;
        elemtype = (i64)qq_tables$tttarget[(m)];
        if ((elemtype==(i64)33) || (elemtype==(i64)34) || (elemtype==(i64)35)) {
            nbits = (qq_tables$ttlength[(m)] * (i64)qq_tables$ttbitwidth[((i64)qq_tables$tttarget[(m)])]);
            qq_tables$ttsize[(m)] = (((nbits - (i64)1) / (i64)8) + (i64)1);
        }
        else {
            qq_resolve$converttype((i64)qq_tables$tttarget[(m)]);
            qq_tables$ttsize[(m)] = (qq_tables$ttlength[(m)] * qq_tables$ttsize[((i64)qq_tables$tttarget[(m)])]);
        }
;
    }
    else if (($temp==(i64)13)) {
        d = qq_tables$ttnamedef[(m)];
        f = (*d).deflist;
        nfields = (i64)0;
        L914 :;
        while (!!(f)) {
            if ((nfields >= (i64)256)) {
                qq_lib$rxerror((byte*)"Too many fields",0);
            }
;
            fieldlist[(++(nfields))-1] = f;
            f = (*f).nextdef;
L915 :;
        }
L916 :;
        ;
        fieldlist[((nfields + (i64)1))-1] = 0;
        qq_resolve$ntopfields = (qq_resolve$nallfields = (i64)0);
        maxalign = (i64)1;
        index = (i64)1;
        qq_resolve$scanstruct((i64)1,&fieldlist,&index,&size,(i64)0,(i64)qq_tables$ttcaligned[(m)],&maxalign,(i64)2);
        if (!!((i64)qq_tables$ttcaligned[(m)])) {
            size = mlib$roundtoblock(size,maxalign);
            (*d).maxalign = maxalign;
        }
        else {
            (*d).maxalign = (i64)1;
        }
;
        qq_tables$ttsize[(m)] = size;
        qq_tables$ttlower[(m)] = (i64)1;
        qq_tables$ttlength[(m)] = qq_resolve$ntopfields;
        (*d).topfieldlist = (struct qq_decls$strec **)mlib$pcm_alloc(((i64)8 * qq_resolve$ntopfields));
        memcpy((*d).topfieldlist,&qq_resolve$structfields,(u64)((i64)8 * qq_resolve$ntopfields));
    }
    else if (($temp==(i64)12)) {
    }
    else {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"CAN'T DO:",NULL);
        msysc$m_print_str(qq_show$strmode(m,(i64)0),NULL);
        msysc$m_print_str(qq_show$strmode((i64)qq_tables$ttbasetype[(m)],(i64)0),NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
    };
}

static void qq_resolve$scanstruct(i64 smode,struct qq_decls$strec *(*fields)[],i64 *index,i64 *isize,i64 offset,i64 calign,i64 *maxalign,i64 countmode) {
        struct qq_decls$strec *  f;
        i64 newoffset;
        i64 fieldsize;
        i64 alignment;
        i64 size;
    size = (i64)0;
    L917 :;
    while (!!((f = (*fields)[(((*index))++)-1]))) {
                {i64 $temp = (i64)(*f).nameid;
if (($temp==(i64)12)) {
            qq_resolve$converttype((i64)(*f).mode);
            fieldsize = qq_tables$ttsize[((i64)(*f).mode)];
            if (!!(calign)) {
                alignment = qq_names$getalignment((i64)(*f).mode);
                (*maxalign)=((*maxalign)>alignment?(*maxalign):alignment);
;
                newoffset = mlib$roundtoblock(offset,alignment);
                size += (newoffset - offset);
            }
            else {
                newoffset = offset;
            }
;
            (*f).fieldoffset = newoffset;
            (*f).index = ((*index) - (i64)1);
            offset = newoffset;
            //countfields:
L920 :;
;
            ++(qq_resolve$nallfields);
            if (!!(countmode)) {
                qq_resolve$structfields[(++(qq_resolve$ntopfields))-1] = f;
            }
;
        }
        else if (($temp==(i64)24)) {
            qq_resolve$scanstruct((i64)1,fields,index,&fieldsize,offset,calign,maxalign,countmode);
        }
        else if (($temp==(i64)25)) {
            qq_resolve$scanstruct((i64)0,fields,index,&fieldsize,offset,calign,maxalign,(!!(countmode) ? (i64)1 : (i64)0));
        }
        else if (($temp==(i64)26)) {
            (*isize) = size;
            return;
        }
        };
        if (!!(smode)) {
            offset += fieldsize;
            size += fieldsize;
        }
        else {
            size = (size>fieldsize?size:fieldsize);
            countmode = (i64)0;
        }
;
L918 :;
    }
L919 :;
    ;
    (*isize) = size;
}

static void qq_resolve$dobaseclass(i64 baseclassindex) {
        struct qq_decls$strec *  sttype;
        struct qq_decls$strec *  d;
        struct qq_decls$strec *  e;
        struct qq_decls$strec *  newd;
        i64 baseclass;
        i64 normalexit;
    baseclass = (i64)qq_decls$baseclasstable[(baseclassindex)];
    sttype = (struct qq_decls$strec *)qq_decls$baseclassdef[(baseclassindex)];
    d = (struct qq_decls$strec *)(*qq_tables$ttnamedef[(baseclass)]).deflist;
    L921 :;
    while (!!(d)) {
        e = (struct qq_decls$strec *)(*sttype).deflist;
        normalexit = (i64)1;
        L924 :;
        while (!!(e)) {
            if (!!(mlib$eqstring((*d).name,(*e).name))) {
                normalexit = (i64)0;
                goto L926 ;
            }
;
            e = (struct qq_decls$strec *)(*e).nextdef;
L925 :;
        }
L926 :;
        ;
        if (!!(normalexit)) {
                        {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)5) || ($temp==(i64)21)) {
                newd = (struct qq_decls$strec *)qq_names$addsymbol((struct qq_decls$strec *)sttype,(*d).firstdupl,(i64)21,(i64)0);
                (*newd).alias = (struct qq_decls$strec *)d;
            }
            else {
                newd = (struct qq_decls$strec *)qq_names$addsymbol((struct qq_decls$strec *)sttype,(*d).firstdupl,(i64)(*d).nameid,(i64)0);
                qq_names$duplfield((struct qq_decls$strec *)d,(struct qq_decls$strec *)newd);
                ++((*sttype).nfields);
                qq_tables$ttlength[((i64)(*sttype).mode)] = (i64)(*sttype).nfields;
                (*newd).index = (i64)(*sttype).nfields;
                (*newd).fieldoffset = (((i64)(*newd).index - (i64)1) * (i64)16);
            }
            };
            qq_names$addgenfield((struct qq_decls$strec *)newd);
        }
;
        d = (struct qq_decls$strec *)(*d).nextdef;
L922 :;
    }
L923 :;
    ;
}

// START
void qq_resolve$start(void) {

}

void qq_runlab$disploop(void) {
        struct qq_pcltabs$pclrec *  pc;
        struct qq_decls$varrec *  sp;
        byte *  fp;
        struct qq_decls$varrec *  x;
        struct qq_decls$varrec *  y;
        struct qq_decls$varrec *  z;
        i64 n;
        i64 index;
        struct qq_decls$varrec *  px;
        struct qq_decls$strec *  d;
        struct qq_decls$objrec *  pp;
        i64 res;
        i64 lower;
        i64 upper;
        i64 moduleno;
        struct qq_decls$varrec *  newsp;
        struct qq_decls$strec *  e;
        struct qq_decls$varrec vx;
        static int * localjumptable[193] = {
    &&L927,
    &&L928,
    &&L929,
    &&L930,
    &&L931,
    &&L932,
    &&L933,
    &&L934,
    &&L935,
    &&L936,
    &&L937,
    &&L938,
    &&L939,
    &&L940,
    &&L941,
    &&L942,
    &&L943,
    &&L944,
    &&L945,
    &&L946,
    &&L947,
    &&L948,
    &&L949,
    &&L950,
    &&L951,
    &&L952,
    &&L953,
    &&L954,
    &&L955,
    &&L956,
    &&L957,
    &&L958,
    &&L959,
    &&L960,
    &&L961,
    &&L962,
    &&L963,
    &&L964,
    &&L965,
    &&L966,
    &&L967,
    &&L968,
    &&L969,
    &&L970,
    &&L971,
    &&L972,
    &&L973,
    &&L974,
    &&L975,
    &&L976,
    &&L977,
    &&L978,
    &&L979,
    &&L980,
    &&L981,
    &&L982,
    &&L983,
    &&L984,
    &&L985,
    &&L986,
    &&L987,
    &&L988,
    &&L989,
    &&L990,
    &&L991,
    &&L992,
    &&L993,
    &&L994,
    &&L995,
    &&L996,
    &&L997,
    &&L998,
    &&L999,
    &&L1000,
    &&L1001,
    &&L1002,
    &&L1003,
    &&L1004,
    &&L1005,
    &&L1006,
    &&L1007,
    &&L1008,
    &&L1009,
    &&L1010,
    &&L1011,
    &&L1012,
    &&L1013,
    &&L1014,
    &&L1015,
    &&L1016,
    &&L1017,
    &&L1018,
    &&L1019,
    &&L1020,
    &&L1021,
    &&L1022,
    &&L1023,
    &&L1024,
    &&L1025,
    &&L1026,
    &&L1027,
    &&L1028,
    &&L1029,
    &&L1030,
    &&L1031,
    &&L1032,
    &&L1033,
    &&L1034,
    &&L1035,
    &&L1036,
    &&L1037,
    &&L1038,
    &&L1039,
    &&L1040,
    &&L1041,
    &&L1042,
    &&L1043,
    &&L1044,
    &&L1045,
    &&L1046,
    &&L1047,
    &&L1048,
    &&L1049,
    &&L1050,
    &&L1051,
    &&L1052,
    &&L1053,
    &&L1054,
    &&L1055,
    &&L1056,
    &&L1057,
    &&L1058,
    &&L1059,
    &&L1060,
    &&L1061,
    &&L1062,
    &&L1063,
    &&L1064,
    &&L1065,
    &&L1066,
    &&L1067,
    &&L1068,
    &&L1069,
    &&L1070,
    &&L1071,
    &&L1072,
    &&L1073,
    &&L1074,
    &&L1075,
    &&L1076,
    &&L1077,
    &&L1078,
    &&L1079,
    &&L1080,
    &&L1081,
    &&L1082,
    &&L1083,
    &&L1084,
    &&L1085,
    &&L1086,
    &&L1087,
    &&L1088,
    &&L1089,
    &&L1090,
    &&L1091,
    &&L1092,
    &&L1093,
    &&L1094,
    &&L1095,
    &&L1096,
    &&L1097,
    &&L1098,
    &&L1099,
    &&L1100,
    &&L1101,
    &&L1102,
    &&L1103,
    &&L1104,
    &&L1105,
    &&L1106,
    &&L1107,
    &&L1108,
    &&L1109,
    &&L1110,
    &&L1111,
    &&L1112,
    &&L1113,
    &&L1114,
    &&L1115,
    &&L1116,
    &&L1117,
    &&L1118,
    &&L1119
};
        i64 $av_1;
        static i64 count = (i64)100;
        i64 $av_2;
        i64 $av_3;
        i64 $av_4;
    if (!!((i64)qq_runlab$getjt)) {
        qq_runlab$jumptable = (int * (*)[])&localjumptable;
        return;
    }
;
    sp = qq_decls$sptr;
    pc = qq_decls$pcptr;
    fp = qq_decls$frameptr;
    goto *(*pc).labaddr;
;
    //jnop:
L927 :;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jskip:
L928 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jprocdef:
L929 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jprocent:
L930 :;
;
    $av_1 = (i64)(*pc).n;
    while ($av_1-- > 0) {
L1121 :;
        ++(sp);
        (*sp).tagx = (i64)0;
L1122 :;
    }
L1123 :;
    ;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jprocend:
L931 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jendmod:
L932 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jcomment:
L933 :;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpushm:
L934 :;
;
    ++(sp);
    (*sp) = (*(*pc).varptr);
    if (!!((i64)(*sp).hasref)) {
        ++((*(*sp).objptr).refcount);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpushf:
L935 :;
;
    ++(sp);
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    (*sp) = (*x);
    if (!!((i64)(*sp).hasref)) {
        ++((*(*sp).objptr).refcount);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpushmref:
L936 :;
;
    ++(sp);
    (*sp).tagx = (i64)14;
    (*sp).varptr = (*pc).varptr;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpushfref:
L937 :;
;
    ++(sp);
    (*sp).tagx = (i64)14;
    (*sp).varptr = (struct qq_decls$varrec *)(fp + (*pc).offset);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpopm:
L938 :;
;
    x = (*pc).varptr;
    if (!!((i64)(*x).hasref)) {
        qq_vars$var_unshareu(x);
    }
;
    (*x) = (*sp);
    --(sp);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpopf:
L939 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if (!!((i64)(*x).hasref)) {
        qq_vars$var_unshareu(x);
    }
;
    (*x) = (*sp);
    --(sp);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpushci:
L940 :;
;
    ++(sp);
    (*sp).tagx = (i64)1;
    (*sp).value = (*pc).value;
    ++(pc);
    //jpushcix:
L1124 :;
;
    goto *(*pc).labaddr;
;
    //jpushvoid:
L941 :;
;
    ++(sp);
    (*sp).tagx = (i64)0;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpushnil:
L942 :;
;
    ++(sp);
    (*sp).tagx = (i64)16;
    (*sp).elemtag = (i64)0;
    (*sp).ptr = 0;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpushcr:
L943 :;
;
    ++(sp);
    (*sp).tagx = (i64)2;
    (*sp).xvalue = (*pc).xvalue;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpushcs:
L944 :;
;
    ++(sp);
    (*sp).tagx = (i64)265;
    (*sp).objptr = (*pc).objptr;
    ++((*(*sp).objptr).refcount);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpushtype:
L945 :;
;
    ++(sp);
    (*sp).tagx = (i64)18;
    (*sp).value = (*pc).typecode;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpushopc:
L946 :;
;
    ++(sp);
    (*sp).tagx = (i64)19;
    (*sp).value = (i64)(*pc).pclop;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpushsym:
L947 :;
;
    ++(sp);
    (*sp).tagx = (i64)17;
    (*sp).def = (*pc).def;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpushptr:
L948 :;
;
    x = sp;
    //jpushptr2:
L1125 :;
;
        {i64 $temp = (i64)(*x).tag;
if (($temp==(i64)14)) {
        (*sp) = (*(*x).varptr);
    }
    else if (($temp==(i64)16)) {
                {i64 $temp = (i64)(*x).elemtag;
if (($temp==(i64)27)) {
            (*sp).tagx = (i64)1;
            (*sp).value = (i64)(*(*x).ptr);
            goto L1126 ;
;
        }
        else {
            qq_decls$pcptr = pc;
            qq_packed$var_loadpacked((*x).ptr,(i64)(*x).elemtag,sp,0);
        }
        };
    }
    else if (($temp==(i64)15)) {
        qq_decls$pcptr = pc;
        qq_vars$var_loadbit((*x).ptr,(i64)(*x).bitoffset,(i64)(*x).elemtag,(i64)(*x).bitlength,sp);
    }
    else {
        qq_decls$pcptr = pc;
        qq_runaux$pcustype((byte*)"Pushptr",x);
    }
    };
    if (!!((i64)(*sp).hasref)) {
        ++((*(*sp).objptr).refcount);
    }
;
    //refpackend:
L1126 :;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpushptrf:
L1108 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    ++(sp);
    ++(pc);
    goto L1125 ;
;
    //jpopptr:
L949 :;
;
    y = (sp)--;
    x = (sp)--;
        {i64 $temp = (i64)(*y).tag;
if (($temp==(i64)14)) {
        if (!!((i64)(*(*y).varptr).hasref)) {
            qq_vars$var_unshareu((*y).varptr);
        }
;
        (*(*y).varptr) = (*x);
    }
    else if (($temp==(i64)16)) {
        qq_decls$pcptr = pc;
        qq_packed$var_storepacked((*y).ptr,x,(i64)(*y).elemtag);
    }
    else if (($temp==(i64)15)) {
        qq_decls$pcptr = pc;
        qq_vars$var_storebit((*y).ptr,(i64)(*y).bitoffset,x,(i64)(*y).elemtag,(i64)(*y).bitlength);
    }
    else {
        qq_decls$pcptr = pc;
        qq_runaux$pcustype((byte*)"Popptr",y);
    }
    };
    ++(pc);
    goto *(*pc).labaddr;
;
    //jzpopm:
L950 :;
;
    (*(*pc).varptr) = (*sp);
    --(sp);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jzpopf:
L951 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    (*x) = (*sp);
    --(sp);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jdupl:
L952 :;
;
    ++(sp);
    (*sp) = (*(sp - (i64)1));
    if (!!((i64)(*sp).hasref)) {
        ++((*(*sp).objptr).refcount);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jcopy:
L953 :;
;
    if (!!((i64)(*sp).hasref)) {
        vx = (*sp);
        qq_decls$pcptr = pc;
        qq_vars$var_duplu(sp);
        qq_vars$var_unshareu(&vx);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jswap:
L954 :;
;
    x = (sp)--;
    y = (sp)--;
    if (((i64)(*x).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)14)) {
        vx = (*(*x).varptr);
        (*(*x).varptr) = (*(*y).varptr);
        (*(*y).varptr) = vx;
    }
    else {
        qq_decls$pcptr = pc;
        qq_runaux$k_swap(x,y);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jconvrefp:
L955 :;
;
    qq_decls$pcptr = pc;
    qq_runaux$k_convrefpack(sp);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jjump:
L956 :;
;
    pc = (*pc).labelref;
    goto *(*pc).labaddr;
;
    //jjumpptr:
L957 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jjumpt:
L958 :;
;
    x = (sp)--;
    if (((i64)(*x).tag == (i64)1)) {
        if (!!((*x).value)) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
    else {
        qq_decls$pcptr = pc;
        if (!!(qq_vars$var_istruel(x))) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
        if (!!((i64)(*x).hasref)) {
            qq_vars$var_unshareu(x);
        }
;
    }
;
    goto *(*pc).labaddr;
;
    //jjumpf:
L959 :;
;
    x = (sp)--;
    if (((i64)(*x).tag == (i64)1)) {
        if (!(!!((*x).value))) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
    else {
        qq_decls$pcptr = pc;
        if (!(!!(qq_vars$var_istruel(x)))) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
        if (!!((i64)(*x).hasref)) {
            qq_vars$var_unshareu(x);
        }
;
    }
;
    goto *(*pc).labaddr;
;
    //jjumpeq:
L960 :;
;
    y = (sp)--;
    x = (sp)--;
    if (((i64)(*x).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        if (((*x).value == (*y).value)) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
    else {
        qq_decls$pcptr = pc;
        if (!!(qq_vars$var_equal(x,y))) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
;
    goto *(*pc).labaddr;
;
    //jjumpne:
L961 :;
;
    y = (sp)--;
    x = (sp)--;
    if (((i64)(*x).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        if (((*x).value != (*y).value)) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
    else {
        qq_decls$pcptr = pc;
        if (!(!!(qq_vars$var_equal(x,y)))) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
;
    goto *(*pc).labaddr;
;
    //jjumplt:
L962 :;
;
    y = (sp)--;
    x = (sp)--;
    if (((i64)(*x).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        if (((*x).value < (*y).value)) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
    else if (((i64)(*x).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)2)) {
        if (((*x).xvalue < (*y).xvalue)) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
    else {
        qq_decls$pcptr = pc;
        if ((qq_vars$var_compare(x,y) < (i64)0)) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
;
    goto *(*pc).labaddr;
;
    //jjumple:
L963 :;
;
    y = (sp)--;
    x = (sp)--;
    if (((i64)(*x).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        if (((*x).value <= (*y).value)) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
    else {
        qq_decls$pcptr = pc;
        if ((qq_vars$var_compare(x,y) <= (i64)0)) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
;
    goto *(*pc).labaddr;
;
    //jjumpge:
L964 :;
;
    y = (sp)--;
    x = (sp)--;
    if (((i64)(*x).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        if (((*x).value >= (*y).value)) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
    else {
        qq_decls$pcptr = pc;
        if ((qq_vars$var_compare(x,y) >= (i64)0)) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
;
    goto *(*pc).labaddr;
;
    //jjumpgt:
L965 :;
;
    y = (sp)--;
    x = (sp)--;
    if (((i64)(*x).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        if (((*x).value > (*y).value)) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
    else {
        qq_decls$pcptr = pc;
        if ((qq_vars$var_compare(x,y) > (i64)0)) {
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
;
    goto *(*pc).labaddr;
;
    //jjmpeqfci:
L1088 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if (((i64)(*x).tag != (i64)1)) {
        goto L935 ;
;
    }
;
    if (((*x).value == (*(pc + (i64)1)).value)) {
        pc = (*(pc + (i64)2)).labelref;
    }
    else {
        pc += (i64)3;
    }
;
    goto *(*pc).labaddr;
;
    //jjmpnefci:
L1089 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if (((i64)(*x).tag != (i64)1)) {
        goto L935 ;
;
    }
;
    if (((*x).value != (*(pc + (i64)1)).value)) {
        pc = (*(pc + (i64)2)).labelref;
    }
    else {
        pc += (i64)3;
    }
;
    goto *(*pc).labaddr;
;
    //jjmpltfci:
L1090 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if (((i64)(*x).tag != (i64)1)) {
        goto L935 ;
;
    }
;
    if (((*x).value < (*(pc + (i64)1)).value)) {
        pc = (*(pc + (i64)2)).labelref;
    }
    else {
        pc += (i64)3;
    }
;
    goto *(*pc).labaddr;
;
    //jjmplefci:
L1091 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if (((i64)(*x).tag != (i64)1)) {
        goto L935 ;
;
    }
;
    if (((*x).value <= (*(pc + (i64)1)).value)) {
        pc = (*(pc + (i64)2)).labelref;
    }
    else {
        pc += (i64)3;
    }
;
    goto *(*pc).labaddr;
;
    //jjmpgefci:
L1092 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if (((i64)(*x).tag != (i64)1)) {
        goto L935 ;
;
    }
;
    if (((*x).value >= (*(pc + (i64)1)).value)) {
        pc = (*(pc + (i64)2)).labelref;
    }
    else {
        pc += (i64)3;
    }
;
    goto *(*pc).labaddr;
;
    //jjmpgtfci:
L1093 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if (((i64)(*x).tag != (i64)1)) {
        goto L935 ;
;
    }
;
    if (((*x).value > (*(pc + (i64)1)).value)) {
        pc = (*(pc + (i64)2)).labelref;
    }
    else {
        pc += (i64)3;
    }
;
    goto *(*pc).labaddr;
;
    //jjmpeqff:
L1094 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if (((i64)(*x).tag != (i64)1)) {
        goto L935 ;
;
    }
;
    y = (struct qq_decls$varrec *)(fp + (*(pc + (i64)1)).offset);
    if (((*x).value == (*y).value)) {
        pc = (*(pc + (i64)2)).labelref;
    }
    else {
        pc += (i64)3;
    }
;
    goto *(*pc).labaddr;
;
    //jjmpneff:
L1095 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if (((i64)(*x).tag != (i64)1)) {
        goto L935 ;
;
    }
;
    y = (struct qq_decls$varrec *)(fp + (*(pc + (i64)1)).offset);
    if (((*x).value != (*y).value)) {
        pc = (*(pc + (i64)2)).labelref;
    }
    else {
        pc += (i64)3;
    }
;
    goto *(*pc).labaddr;
;
    //jjmpltff:
L1096 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if (((i64)(*x).tag != (i64)1)) {
        goto L935 ;
;
    }
;
    y = (struct qq_decls$varrec *)(fp + (*(pc + (i64)1)).offset);
    if (((*x).value < (*y).value)) {
        pc = (*(pc + (i64)2)).labelref;
    }
    else {
        pc += (i64)3;
    }
;
    goto *(*pc).labaddr;
;
    //jjmpleff:
L1097 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if (((i64)(*x).tag != (i64)1)) {
        goto L935 ;
;
    }
;
    y = (struct qq_decls$varrec *)(fp + (*(pc + (i64)1)).offset);
    if (((*x).value <= (*y).value)) {
        pc = (*(pc + (i64)2)).labelref;
    }
    else {
        pc += (i64)3;
    }
;
    goto *(*pc).labaddr;
;
    //jjmpgeff:
L1098 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if (((i64)(*x).tag != (i64)1)) {
        goto L935 ;
;
    }
;
    y = (struct qq_decls$varrec *)(fp + (*(pc + (i64)1)).offset);
    if (((*x).value >= (*y).value)) {
        pc = (*(pc + (i64)2)).labelref;
    }
    else {
        pc += (i64)3;
    }
;
    goto *(*pc).labaddr;
;
    //jjmpgtff:
L1099 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if (((i64)(*x).tag != (i64)1)) {
        goto L935 ;
;
    }
;
    y = (struct qq_decls$varrec *)(fp + (*(pc + (i64)1)).offset);
    if (((*x).value > (*y).value)) {
        pc = (*(pc + (i64)2)).labelref;
    }
    else {
        pc += (i64)3;
    }
;
    goto *(*pc).labaddr;
;
    //jwheneq:
L966 :;
;
    y = (sp)--;
    x = sp;
    if (((i64)(*x).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        if (((*x).value == (*y).value)) {
            --(sp);
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
    else {
        qq_decls$pcptr = pc;
        res = qq_runaux$k_when(x,y);
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
        if (!!(res)) {
            if (!!((i64)(*x).hasref)) {
                qq_vars$var_unshareu(x);
            }
;
            --(sp);
            pc = (*pc).labelref;
        }
        else {
            ++(pc);
        }
;
    }
;
    goto *(*pc).labaddr;
;
    //jwhenne:
L967 :;
;
    y = (sp)--;
    x = sp;
    if (((i64)(*x).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        if (((*x).value != (*y).value)) {
            pc = (*pc).labelref;
        }
        else {
            --(sp);
            ++(pc);
        }
;
    }
    else {
        qq_decls$pcptr = pc;
        res = qq_runaux$k_when(x,y);
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
        if (!(!!(res))) {
            pc = (*pc).labelref;
        }
        else {
            if (!!((i64)(*x).hasref)) {
                qq_vars$var_unshareu(x);
            }
;
            --(sp);
            ++(pc);
        }
;
    }
;
    goto *(*pc).labaddr;
;
    //jjumplab:
L968 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jswitch:
L969 :;
;
    lower = (i64)(*pc).x;
    n = (((i64)(*pc).y - lower) + (i64)1);
        {i64 $temp = (i64)(*sp).tag;
if (($temp==(i64)1) || ($temp==(i64)18)) {
    }
    else {
        qq_decls$pcptr = pc;
        qq_runaux$pcerror((byte*)"switch not int",qq_tables$ttname[((i64)(*sp).tag)]);
    }
    };
    index = ((*sp).value - lower);
    --(sp);
    if (((u64)index >= (u64)n)) {
        pc = (*((pc + n) + (i64)1)).labelref;
    }
    else {
        pc = (*((pc + index) + (i64)1)).labelref;
    }
;
    goto *(*pc).labaddr;
;
    //jtom:
L970 :;
;
    x = (*(pc + (i64)1)).varptr;
    goto L1127 ;
;
    //jtof:
L971 :;
;
    x = (struct qq_decls$varrec *)(fp + (*(pc + (i64)1)).offset);
    //doto:
L1127 :;
;
    if (!!(--((*x).value))) {
        pc = (*pc).labelref;
    }
    else {
        pc += (i64)2;
    }
;
    goto *(*pc).labaddr;
;
    //jformci:
L972 :;
;
    x = (*(pc + (i64)1)).varptr;
    goto L1128 ;
;
    //jforfci:
L973 :;
;
    x = (struct qq_decls$varrec *)(fp + (*(pc + (i64)1)).offset);
    //doforfci:
L1128 :;
;
    ++((*x).value);
    if (((*x).value <= (*(pc + (i64)2)).value)) {
        pc = (*pc).labelref;
    }
    else {
        pc += (i64)3;
    }
;
    goto *(*pc).labaddr;
;
    //jformm:
L974 :;
;
    x = (*(pc + (i64)1)).varptr;
    y = (*(pc + (i64)2)).varptr;
    goto L1129 ;
;
    //jforff:
L975 :;
;
    x = (struct qq_decls$varrec *)(fp + (*(pc + (i64)1)).offset);
    y = (struct qq_decls$varrec *)(fp + (*(pc + (i64)2)).offset);
    //doforff:
L1129 :;
;
    ++((*x).value);
    if (((*x).value <= (*y).value)) {
        pc = (*pc).labelref;
    }
    else {
        pc += (i64)3;
    }
;
    goto *(*pc).labaddr;
;
    //jcallproc:
L976 :;
;
    if ((--(count) == (i64)0)) {
        count = (i64)100;
        mlinux$os_peek();
    }
;
    if ((sp >= qq_decls$stacklimit)) {
        qq_decls$pcptr = pc;
        qq_runaux$pcerror((byte*)"Stack Overflow",(byte*)"");
    }
;
    ++(sp);
    (*sp).tagx = (i64)20;
    (*sp).retaddr = (pc + (i64)1);
    (*sp).frameptr_low = (i64)(u64)fp;
    fp = (byte *)sp;
    pc = (*pc).labelref;
    goto *(*pc).labaddr;
;
    //jcallptr:
L977 :;
;
    if (((i64)(*sp).tag != (i64)17)) {
        qq_decls$pcptr = pc;
        qq_runaux$pcerror((byte*)"Probably undefined function",(byte*)"");
    }
;
    d = (*sp).def;
    if (((i64)(*d).nameid == (i64)21)) {
        d = (*d).alias;
    }
;
    if (((i64)(*d).nparams != (i64)(*pc).n)) {
        qq_decls$pcptr = pc;
        qq_runaux$pcerror((byte*)"Callptr: wrong # params; need:",msysc$strint((i64)(*d).nparams,0));
    }
;
    (*sp).tagx = (i64)20;
    (*sp).retaddr = (pc + (i64)1);
    (*sp).frameptr_low = (i64)(u64)fp;
    fp = (byte *)sp;
    pc = (*d).labelref;
    goto *(*pc).labaddr;
;
    //jretproc:
L978 :;
;
    //doretproc:
L1130 :;
;
    $av_2 = (i64)(*pc).x;
    while ($av_2-- > 0) {
L1131 :;
        if (!!((i64)(*sp).hasref)) {
            qq_vars$var_unshareu(sp);
        }
;
        --(sp);
L1132 :;
    }
L1133 :;
    ;
    n = (i64)(*pc).n;
    pc = (*sp).retaddr;
    fp = (byte *)((i64)((u64)fp & (u64)18446744069414584320u) | (i64)(*sp).frameptr_low);
    --(sp);
    $av_3 = n;
    while ($av_3-- > 0) {
L1134 :;
        if (!!((i64)(*sp).hasref)) {
            qq_vars$var_unshareu(sp);
        }
;
        --(sp);
L1135 :;
    }
L1136 :;
    ;
    goto *(*pc).labaddr;
;
    //jretfn:
L979 :;
;
    x = (struct qq_decls$varrec *)(fp + (i64)(*pc).y);
    (*x) = (*sp);
    --(sp);
    goto L1130 ;
;
    goto *(*pc).labaddr;
;
    //jmodcall:
L980 :;
;
    d = (*pc).def;
    moduleno = (i64)(*d).moduleno;
    ++(sp);
    (*sp).tagx = (i64)20;
    (*sp).retaddr = (pc + (i64)1);
    pc = (*qq_decls$modules[(moduleno)]).pcstart;
    goto *(*pc).labaddr;
;
    //jmodret:
L981 :;
;
    pc = (*sp).retaddr;
    goto *(*pc).labaddr;
;
    //jcalldll:
L982 :;
;
    n = (i64)(*pc).n;
    qq_decls$pcptr = pc;
    qq_decls$sptr = sp;
    qq_calldll$calldll((*pc).def,((sp - n) + (i64)1),(sp - n),n);
    sp -= n;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jcallhost:
L983 :;
;
    qq_decls$pcptr = pc;
    sp = qq_host$callhostfunction((*pc).hostindex,sp);
    ++(pc);
    goto *(*pc).labaddr;
;
    //junshare:
L984 :;
;
    $av_4 = (i64)(*pc).n;
    while ($av_4-- > 0) {
L1137 :;
        if (!!((i64)(*sp).hasref)) {
            qq_vars$var_unshareu(sp);
        }
;
        --(sp);
L1138 :;
    }
L1139 :;
    ;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jstop:
L986 :;
;
    qq_decls$stopped = (i64)1;
    qq_decls$sptr = sp;
    return;
    //jmakelist:
L987 :;
;
    qq_decls$pcptr = pc;
    sp = qq_runaux$k_makelist(sp,(i64)(*pc).y,(i64)(*pc).x);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jmakevrec:
L988 :;
;
    n = (i64)(*pc).x;
    x = ((sp - (i64)(*pc).x) + (i64)1);
    qq_decls$pcptr = pc;
    qq_records$var_make_record(x,x,(i64)(*pc).x,(i64)(*pc).usertag);
    sp = x;
    (*(*sp).objptr).flags = msysc$m_setdotindex((*(*sp).objptr).flags,(i64)1,(u64)0u);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jmakeax:
L989 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jmakebits:
L990 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jmaketrec:
L991 :;
;
    n = (i64)(*pc).x;
    x = ((sp - n) + (i64)1);
    qq_decls$pcptr = pc;
    qq_packed$var_make_struct(x,x,n,(i64)(*pc).usertag);
    sp = x;
    (*(*sp).objptr).flags = msysc$m_setdotindex((*(*sp).objptr).flags,(i64)1,(u64)0u);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jmakeset:
L992 :;
;
    n = (i64)(*pc).x;
    x = ((sp - n) + (i64)1);
    qq_decls$pcptr = pc;
    qq_sets$var_make_set(x,x,n);
    sp = x;
    (*(*sp).objptr).flags = msysc$m_setdotindex((*(*sp).objptr).flags,(i64)1,(u64)0u);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jmakerang:
L993 :;
;
    y = (sp)--;
    x = sp;
    if (!(((i64)(*x).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1))) {
        qq_decls$pcptr = pc;
        qq_runaux$pcerror((byte*)"makerange/not int",(byte*)"");
    }
;
    (*sp).tagx = (i64)4;
    lower = (*x).value;
    upper = (*y).value;
    if (!((($rtemp=lower, $rtemp >= (i64)-281474976710656 && $rtemp <= (i64)281474976710655)))) {
        qq_decls$pcptr = pc;
        qq_runaux$pcerror((byte*)"Range lwb bounds",(byte*)"");
    }
;
    (*sp).range_upper = (u64)upper;
    (*sp).dummy = msysc$m_setdotslice((*sp).dummy,(i64)16,(i64)63,lower);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jmakedict:
L994 :;
;
    n = (i64)(*pc).x;
    x = ((sp - (n * (i64)2)) + (i64)1);
    qq_decls$pcptr = pc;
    qq_dicts$var_make_dict(x,x,n);
    sp = x;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jmakedec:
L995 :;
;
    vx = (*sp);
    if (((i64)vx.tag != (i64)9)) {
        qq_decls$pcptr = pc;
        qq_runaux$pcerror((byte*)"Not str",(byte*)"");
    }
;
    pp = vx.objptr;
    if (((*pp).length == (i64)0)) {
        qq_decls$pcptr = pc;
        qq_runaux$pcerror((byte*)"Null str",(byte*)"");
    }
;
    qq_decls$pcptr = pc;
    qq_decimal$var_make_dec_str((*pp).strptr,(*pp).length,sp);
    if (!!((i64)vx.hasref)) {
        qq_vars$var_unshareu(&vx);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jincrptr:
L996 :;
;
    qq_decls$pcptr = pc;
    qq_runaux$k_incrptr(sp,(i64)(*pc).x);
    --(sp);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jincrtom:
L997 :;
;
    x = (*pc).varptr;
    goto L1140 ;
;
    //jincrtof:
L998 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    //doincrto:
L1140 :;
;
        {i64 $temp = (i64)(*x).tag;
if (($temp==(i64)1)) {
        (*x).value += (i64)(*pc).x;
    }
    else if (($temp==(i64)14)) {
        (*x).varptr += (i64)(*pc).x;
    }
    else if (($temp==(i64)16)) {
        (*x).ptr += (qq_tables$ttsize[((i64)(*x).elemtag)] * (i64)(*pc).x);
    }
    else if (($temp==(i64)2)) {
        (*x).xvalue += (r64)(i64)(*pc).x;
    }
    else {
        qq_decls$pcptr = pc;
        qq_runaux$pcustype((byte*)"incrto",x);
    }
    };
    ++(pc);
    //jincrtofx:
L1141 :;
;
    goto *(*pc).labaddr;
;
    //jloadincr:
L999 :;
;
    vx = (*sp);
    qq_decls$pcptr = pc;
    qq_vars$var_loadptr(sp,sp);
    ++(sp);
    (*sp) = vx;
    qq_runaux$k_incrptr(sp,(i64)(*pc).x);
    --(sp);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jincrload:
L1000 :;
;
    vx = (*sp);
    qq_decls$pcptr = pc;
    qq_runaux$k_incrptr(sp,(i64)(*pc).x);
    --(sp);
    qq_vars$var_loadptr(&vx,++(sp));
    ++(pc);
    goto *(*pc).labaddr;
;
    //jneg:
L1001 :;
;
    vx = (*sp);
    qq_decls$pcptr = pc;
    qq_vars$var_neg(sp);
    if (!!((i64)vx.hasref)) {
        qq_vars$var_unshareu(&vx);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jabs:
L1002 :;
;
    vx = (*sp);
    qq_decls$pcptr = pc;
    qq_vars$var_abs(sp);
    if (!!((i64)vx.hasref)) {
        qq_vars$var_unshareu(&vx);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jnotl:
L1003 :;
;
    qq_decls$pcptr = pc;
    res = (i64)!(!!(qq_vars$var_istruel(sp)));
    if (!!((i64)(*sp).hasref)) {
        qq_vars$var_unshareu(sp);
    }
;
    (*sp).tagx = (i64)1;
    (*sp).value = res;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jinot:
L1004 :;
;
    if (((i64)(*sp).tag == (i64)1)) {
        (*sp).value = ~((*sp).value);
    }
    else {
        vx = (*sp);
        qq_decls$pcptr = pc;
        qq_vars$var_inot(sp);
        if (!!((i64)vx.hasref)) {
            qq_vars$var_unshareu(&vx);
        }
;
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jistruel:
L1005 :;
;
    qq_decls$pcptr = pc;
    n = qq_vars$var_istruel(sp);
    if (!!((i64)(*sp).hasref)) {
        qq_vars$var_unshareu(sp);
    }
;
    (*sp).tagx = (i64)1;
    (*sp).value = n;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jasc:
L1006 :;
;
        {i64 $temp = (i64)(*sp).tag;
if (($temp==(i64)9)) {
        if (!!((*(*sp).objptr).length)) {
            n = (i64)(u64)(*(*(*sp).objptr).strptr);
        }
        else {
            n = (i64)0;
        }
;
        qq_vars$var_unshareu(sp);
        (*sp).tagx = (i64)1;
        (*sp).value = n;
    }
    else {
        qq_runaux$pcustype((byte*)"ASC",sp);
    }
    };
    ++(pc);
    goto *(*pc).labaddr;
;
    //jchr:
L1007 :;
;
    if (((i64)(*sp).tag == (i64)1)) {
        qq_decls$pcptr = pc;
        qq_strings$var_makechar((*sp).value,sp);
    }
    else {
        qq_decls$pcptr = pc;
        qq_runaux$pcustype((byte*)"CHR",sp);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jsqr:
L1008 :;
;
        {i64 $temp = (i64)(*sp).tag;
if (($temp==(i64)1)) {
        (*sp).value = ((*sp).value*(*sp).value);
    }
    else if (($temp==(i64)2)) {
        (*sp).xvalue = ((*sp).xvalue*(*sp).xvalue);
    }
    else {
        qq_decls$pcptr = pc;
        qq_runaux$pcustype((byte*)"sqr",sp);
    }
    };
    ++(pc);
    goto *(*pc).labaddr;
;
    //jmaths:
L1009 :;
;
    qq_decls$pcptr = pc;
    qq_runaux$k_maths(sp,(i64)(*pc).mathscode);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jmaths2:
L1010 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //junaryto:
L1011 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jnotlto:
L1012 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jlen:
L1013 :;
;
    qq_decls$pcptr = pc;
    qq_runaux$k_len(sp);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jlwb:
L1014 :;
;
    qq_decls$pcptr = pc;
    qq_runaux$k_lwb(sp);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jupb:
L1015 :;
;
    qq_decls$pcptr = pc;
    qq_runaux$k_upb(sp);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jbounds:
L1016 :;
;
    qq_decls$pcptr = pc;
    qq_runaux$k_bounds(sp,&lower,&upper);
    if (((i64)(*pc).n == (i64)2)) {
        if (!!((i64)(*sp).hasref)) {
            qq_vars$var_unshareu(sp);
        }
;
        (*sp).tagx = (i64)1;
        (*sp).value = lower;
        ++(sp);
        (*sp).tagx = (i64)1;
        (*sp).value = upper;
    }
    else {
        if (!!((i64)(*sp).hasref)) {
            qq_vars$var_unshareu(sp);
        }
;
        (*sp).tagx = (i64)4;
        (*sp).dummy = msysc$m_setdotslice((*sp).dummy,(i64)16,(i64)63,lower);
        (*sp).range_upper = (u64)upper;
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jbytesize:
L1017 :;
;
    qq_decls$pcptr = pc;
    res = qq_runaux$k_bytesize(sp);
    if (!!((i64)(*sp).hasref)) {
        qq_vars$var_unshareu(sp);
    }
;
    (*sp).tagx = (i64)1;
    (*sp).value = res;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jtype:
L1018 :;
;
    qq_decls$pcptr = pc;
    n = qq_runaux$k_type(sp,(i64)(*pc).n);
    if (!!((i64)(*sp).hasref)) {
        qq_vars$var_unshareu(sp);
    }
;
    (*sp).tagx = (i64)18;
    (*sp).value = n;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jdictsize:
L1019 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jisfound:
L1020 :;
;
    if (((i64)(*sp).tag != (i64)1)) {
        qq_decls$pcptr = pc;
        qq_runaux$pcerror((byte*)"isfound",(byte*)"");
    }
;
    (*sp).value = (i64)((*sp).value != (i64)(-9223372036854775807-1));
    ++(pc);
    goto *(*pc).labaddr;
;
    //jminval:
L1021 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jmaxval:
L1022 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jistype:
L1023 :;
;
    n = (i64)0;
    if (((*pc).typecode == (i64)14)) {
        if (((i64)(*sp).tag == (i64)14 || (i64)(*sp).tag == (i64)16 || (i64)(*sp).tag == (i64)15)) {
            n = (i64)1;
        }
;
    }
    else {
        if (((*pc).typecode == (i64)(*sp).tag)) {
            n = (i64)1;
        }
;
    }
;
    if (!!((i64)(*sp).hasref)) {
        qq_vars$var_unshareu(sp);
    }
;
    (*sp).tagx = (i64)1;
    (*sp).value = n;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jisvoid:
L1024 :;
;
    res = (i64)((i64)(*sp).tag == (i64)0);
    if (!!((i64)(*sp).hasref)) {
        qq_vars$var_unshareu(sp);
    }
;
    (*sp).tagx = (i64)1;
    (*sp).value = (res ^ (i64)(*pc).n);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jconvert:
L1025 :;
;
    if (((i64)(*sp).tag != (i64)(*pc).usertag)) {
        vx = (*sp);
        qq_decls$pcptr = pc;
        qq_vars$var_convert(&vx,(i64)(*pc).usertag,sp);
        if (!!((i64)vx.hasref)) {
            qq_vars$var_unshareu(&vx);
        }
;
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jtypepun:
L1026 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jadd:
L1027 :;
;
    y = (sp)--;
    if (((i64)(*sp).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        (*sp).value += (*y).value;
    }
    else if (((i64)(*sp).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)2)) {
        (*sp).xvalue += (*y).xvalue;
    }
    else {
        vx = (*sp);
        qq_decls$pcptr = pc;
        qq_vars$var_add(sp,y);
        if (!!((i64)vx.hasref)) {
            qq_vars$var_unshareu(&vx);
        }
;
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jsub:
L1028 :;
;
    y = (sp)--;
    if (((i64)(*sp).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        (*sp).value -= (*y).value;
    }
    else if (((i64)(*sp).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)2)) {
        (*sp).xvalue -= (*y).xvalue;
    }
    else {
        vx = (*sp);
        qq_decls$pcptr = pc;
        qq_vars$var_sub(sp,y);
        if (!!((i64)vx.hasref)) {
            qq_vars$var_unshareu(&vx);
        }
;
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jmul:
L1029 :;
;
    y = (sp)--;
    if (((i64)(*sp).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        (*sp).value *= (*y).value;
    }
    else if (((i64)(*sp).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)2)) {
        (*sp).xvalue *= (*y).xvalue;
    }
    else {
        vx = (*sp);
        qq_decls$pcptr = pc;
        qq_vars$var_mul(sp,y);
        if (!!((i64)vx.hasref)) {
            qq_vars$var_unshareu(&vx);
        }
;
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jdiv:
L1030 :;
;
    y = (sp)--;
    vx = (*sp);
    if (((i64)(*sp).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)2)) {
        (*sp).xvalue /= (*y).xvalue;
    }
    else {
        qq_decls$pcptr = pc;
        qq_vars$var_div(sp,y);
        if (!!((i64)vx.hasref)) {
            qq_vars$var_unshareu(&vx);
        }
;
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jidiv:
L1031 :;
;
    y = (sp)--;
    vx = (*sp);
    if (((i64)(*sp).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        (*sp).value /= (*y).value;
    }
    else {
        qq_decls$pcptr = pc;
        qq_vars$var_idiv(sp,y);
        if (!!((i64)vx.hasref)) {
            qq_vars$var_unshareu(&vx);
        }
;
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jirem:
L1032 :;
;
    y = (sp)--;
    vx = (*sp);
    qq_decls$pcptr = pc;
    qq_vars$var_irem(sp,y);
    if (!!((i64)vx.hasref)) {
        qq_vars$var_unshareu(&vx);
    }
;
    if (!!((i64)(*y).hasref)) {
        qq_vars$var_unshareu(y);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jidivrem:
L1033 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jiand:
L1034 :;
;
    y = (sp)--;
    if (((i64)(*sp).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        (*sp).value &= (*y).value;
    }
    else {
        vx = (*sp);
        qq_decls$pcptr = pc;
        qq_vars$var_iand(sp,y);
        if (!!((i64)vx.hasref)) {
            qq_vars$var_unshareu(&vx);
        }
;
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jior:
L1035 :;
;
    y = (sp)--;
    if (((i64)(*sp).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        (*sp).value |= (*y).value;
    }
    else {
        vx = (*sp);
        qq_decls$pcptr = pc;
        qq_vars$var_ior(sp,y);
        if (!!((i64)vx.hasref)) {
            qq_vars$var_unshareu(&vx);
        }
;
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jixor:
L1036 :;
;
    y = (sp)--;
    if (((i64)(*sp).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        (*sp).value ^= (*y).value;
    }
    else {
        vx = (*sp);
        qq_decls$pcptr = pc;
        qq_vars$var_ixor(sp,y);
        if (!!((i64)vx.hasref)) {
            qq_vars$var_unshareu(&vx);
        }
;
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jshl:
L1037 :;
;
    y = (sp)--;
    if (((i64)(*sp).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        (*sp).value <<= (*y).value;
    }
    else {
        vx = (*sp);
        qq_decls$pcptr = pc;
        qq_vars$var_shl(sp,y);
        if (!!((i64)vx.hasref)) {
            qq_vars$var_unshareu(&vx);
        }
;
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jshr:
L1038 :;
;
    y = (sp)--;
    if (((i64)(*sp).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        (*sp).value >>= (*y).value;
    }
    else {
        vx = (*sp);
        qq_decls$pcptr = pc;
        qq_vars$var_shr(sp,y);
        if (!!((i64)vx.hasref)) {
            qq_vars$var_unshareu(&vx);
        }
;
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jin:
L1039 :;
;
    y = sp;
    x = --(sp);
    qq_decls$pcptr = pc;
    n = (qq_vars$var_in(x,y) ^ (i64)(*pc).n);
    if (!!((i64)(*x).hasref)) {
        qq_vars$var_unshareu(x);
    }
;
    if (!!((i64)(*y).hasref)) {
        qq_vars$var_unshareu(y);
    }
;
    (*sp).tagx = (i64)1;
    (*sp).value = n;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jinx:
L1040 :;
;
    y = sp;
    x = --(sp);
    qq_decls$pcptr = pc;
    n = qq_vars$var_inx(x,y);
    if (!!((i64)(*x).hasref)) {
        qq_vars$var_unshareu(x);
    }
;
    if (!!((i64)(*y).hasref)) {
        qq_vars$var_unshareu(y);
    }
;
    (*sp).tagx = (i64)1;
    (*sp).value = n;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jcmp:
L1041 :;
;
    y = sp;
    x = --(sp);
    qq_decls$pcptr = pc;
    res = qq_runaux$k_cmp((i64)(*pc).n,x,y);
    if (!!((i64)(*x).hasref)) {
        qq_vars$var_unshareu(x);
    }
;
    if (!!((i64)(*y).hasref)) {
        qq_vars$var_unshareu(y);
    }
;
    (*sp).tagx = (i64)1;
    (*sp).value = res;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jmin:
L1042 :;
;
    y = (sp)--;
    x = sp;
    qq_decls$pcptr = pc;
    if ((qq_vars$var_compare(x,y) < (i64)0)) {
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
    else {
        if (!!((i64)(*x).hasref)) {
            qq_vars$var_unshareu(x);
        }
;
        (*sp) = (*y);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jmax:
L1043 :;
;
    y = (sp)--;
    x = sp;
    qq_decls$pcptr = pc;
    if ((qq_vars$var_compare(x,y) >= (i64)0)) {
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
    else {
        if (!!((i64)(*x).hasref)) {
            qq_vars$var_unshareu(x);
        }
;
        (*sp) = (*y);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jconcat:
L1044 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jappend:
L1045 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jsame:
L1046 :;
;
    y = (sp)--;
    x = sp;
    if (((!!((i64)(*x).hasref) && !!((i64)(*y).hasref)) && ((*x).objptr == (*y).objptr))) {
        res = (i64)1;
    }
    else {
        res = (i64)0;
    }
;
    if (!!((i64)(*x).hasref)) {
        qq_vars$var_unshareu(x);
    }
;
    if (!!((i64)(*y).hasref)) {
        qq_vars$var_unshareu(y);
    }
;
    (*sp).tagx = (i64)1;
    (*sp).value = res;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpower:
L1047 :;
;
    y = (sp)--;
    vx = (*sp);
    qq_decls$pcptr = pc;
    qq_vars$var_power(sp,y);
    if (!!((i64)vx.hasref)) {
        qq_vars$var_unshareu(&vx);
    }
;
    if (!!((i64)(*y).hasref)) {
        qq_vars$var_unshareu(y);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jbinto:
L1048 :;
;
    y = (sp)--;
    x = (sp)--;
    z = (*x).varptr;
    if (((((i64)(*pc).bintoindex == (i64)1) && ((i64)(*x).tag == (i64)14)) && ((i64)(*z).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1))) {
        (*z).value += (*y).value;
    }
    else {
        qq_decls$pcptr = pc;
        qq_vars$var_inplace((i64)(*pc).bintoindex,x,y);
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jandlto:
L1049 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jorlto:
L1050 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jappendto:
L1052 :;
;
    y = (sp)--;
    px = (sp)--;
        {i64 $temp = (i64)(*px).tag;
if (($temp==(i64)14)) {
        qq_decls$pcptr = pc;
        qq_vars$var_appendto((*px).varptr,y);
    }
    else {
        qq_decls$pcptr = pc;
        qq_runaux$pcustype((byte*)"Appendto",px);
    }
    };
    ++(pc);
    goto *(*pc).labaddr;
;
    //jconcatto:
L1051 :;
;
    y = (sp)--;
    px = (sp)--;
        {i64 $temp = (i64)(*px).tag;
if (($temp==(i64)14)) {
        qq_decls$pcptr = pc;
        qq_vars$var_concatto((*px).varptr,y);
    }
    else {
        qq_decls$pcptr = pc;
        qq_runaux$pcustype((byte*)"Concatto",px);
    }
    };
    ++(pc);
    goto *(*pc).labaddr;
;
    //jdot:
L1053 :;
;
    qq_decls$pcptr = pc;
    qq_runaux$k_dot(sp,(*pc).index);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jdot1:
L1054 :;
;
    if (((i64)(*sp).tag != (i64)12)) {
        qq_decls$pcptr = pc;
        qq_runaux$pcerror((byte*)"Dot1: not rec",(byte*)"");
    }
;
    d = (*qq_decls$genfieldtable[((*pc).index)-1]).def;
    if (((i64)(*(*sp).objptr).usertag != (i64)(*(*d).owner).mode)) {
        qq_decls$pcptr = pc;
        qq_runaux$pcerror((byte*)"Dot1: wrong type",(byte*)"");
    }
;
    x = ((*(*sp).objptr).varptr + ((i64)(*d).fieldoffset / (i64)16));
    if (!!((i64)(*x).hasref)) {
        ++((*(*x).objptr).refcount);
    }
;
    if (!!((i64)(*sp).hasref)) {
        qq_vars$var_unshareu(sp);
    }
;
    (*sp) = (*x);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpopdot:
L1055 :;
;
    qq_decls$pcptr = pc;
    sp = qq_runaux$k_popdot(sp,(*pc).index);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpopdot1:
L1056 :;
;
    x = (sp)--;
    y = (sp)--;
    if (((i64)(*x).tag != (i64)12)) {
        qq_decls$pcptr = pc;
        qq_runaux$pcerror((byte*)"Popdot1: not rec",(byte*)"");
    }
;
    if (!(!!(msysc$m_getdotindex((i64)(*(*x).objptr).flags,(i64)1)))) {
        qq_decls$pcptr = pc;
        qq_lib$pcnotmut();
    }
;
    e = (*qq_decls$genfieldtable[((*pc).index)-1]).def;
    if (((i64)(*(*x).objptr).usertag != (i64)(*(*e).owner).mode)) {
        qq_decls$pcptr = pc;
        qq_runaux$pcerror((byte*)"Popdot1: wrong type",(byte*)"");
    }
;
    z = ((*(*x).objptr).varptr + ((i64)(*e).fieldoffset / (i64)16));
    if (!!((i64)(*z).hasref)) {
        qq_vars$var_unshareu(z);
    }
;
    (*z) = (*y);
    if (!!((i64)(*x).hasref)) {
        qq_vars$var_unshareu(x);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jdotref:
L1057 :;
;
    qq_decls$pcptr = pc;
    qq_runaux$k_dotref(sp,(*pc).index);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jindex:
L1058 :;
;
    y = (sp)--;
    vx = (*sp);
    qq_decls$pcptr = pc;
        {i64 $temp = (i64)(*y).tag;
if (($temp==(i64)1)) {
        qq_vars$var_getix(sp,(*y).value);
    }
    else if (($temp==(i64)4)) {
        qq_vars$var_getslice(sp,msysc$m_getdotslice((*y).dummy,(i64)16,(i64)63),(i64)(*y).range_upper);
    }
    else {
        qq_decls$pcptr = pc;
        qq_runaux$pcmxtypes((byte*)"Index",&vx,y);
    }
    };
    if (!!((i64)vx.hasref)) {
        qq_vars$var_unshareu(&vx);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpopix:
L1059 :;
;
    z = (sp)--;
    y = (sp)--;
    x = (sp)--;
    qq_decls$pcptr = pc;
        {i64 $temp = (i64)(*z).tag;
if (($temp==(i64)1)) {
        qq_vars$var_putix(y,(*z).value,x);
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
    else if (($temp==(i64)4)) {
        qq_vars$var_putslice(y,msysc$m_getdotslice((*z).dummy,(i64)16,(i64)63),(i64)(*z).range_upper,x);
        if (!!((i64)(*x).hasref)) {
            qq_vars$var_unshareu(x);
        }
;
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
    else {
        qq_decls$pcptr = pc;
        qq_runaux$pcmxtypes((byte*)"Popix",y,z);
    }
    };
    ++(pc);
    goto *(*pc).labaddr;
;
    //jindexref:
L1060 :;
;
    y = (sp)--;
    vx = (*sp);
    qq_decls$pcptr = pc;
        {i64 $temp = (i64)(*y).tag;
if (($temp==(i64)1)) {
        qq_vars$var_getixref(sp,(*y).value);
    }
    else {
        qq_decls$pcptr = pc;
        qq_runaux$pcmxtypes((byte*)"Indexref",sp,y);
    }
    };
    if (!!((i64)vx.hasref)) {
        qq_vars$var_unshareu(&vx);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jkeyindex:
L1061 :;
;
    qq_decls$pcptr = pc;
    sp = qq_runaux$k_keyindex(sp);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpopkeyix:
L1062 :;
;
    qq_decls$pcptr = pc;
    sp = qq_runaux$k_popkeyindex(sp);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jkeyixref:
L1063 :;
;
    qq_decls$pcptr = pc;
    sp = qq_runaux$k_keyindexref(sp);
    ++(pc);
    goto *(*pc).labaddr;
;
    //jdotix:
L1064 :;
;
    y = (sp)--;
    vx = (*sp);
    qq_decls$pcptr = pc;
        {i64 $temp = (i64)(*y).tag;
if (($temp==(i64)1)) {
        qq_vars$var_getdotix(sp,(*y).value);
    }
    else if (($temp==(i64)4)) {
        qq_vars$var_getdotslice(sp,msysc$m_getdotslice((*y).dummy,(i64)16,(i64)63),(i64)(*y).range_upper);
    }
    else {
        qq_runaux$pcmxtypes((byte*)"Dotindex",&vx,y);
    }
    };
    if (!!((i64)vx.hasref)) {
        qq_vars$var_unshareu(&vx);
    }
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpopdotix:
L1065 :;
;
    z = (sp)--;
    y = (sp)--;
    x = (sp)--;
    qq_decls$pcptr = pc;
        {i64 $temp = (i64)(*z).tag;
if (($temp==(i64)1)) {
        qq_vars$var_putdotix(y,(*z).value,x);
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
    else if (($temp==(i64)4)) {
        qq_vars$var_putdotslice(y,msysc$m_getdotslice((*z).dummy,(i64)16,(i64)63),(i64)(*z).range_upper,x);
        if (!!((i64)(*x).hasref)) {
            qq_vars$var_unshareu(x);
        }
;
        if (!!((i64)(*y).hasref)) {
            qq_vars$var_unshareu(y);
        }
;
    }
    else {
        qq_decls$pcptr = pc;
        qq_runaux$pcmxtypes((byte*)"Popdotindex",y,z);
    }
    };
    ++(pc);
    goto *(*pc).labaddr;
;
    //jdotixref:
L1066 :;
;
    goto L1120 ;
;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jexpand:
L1067 :;
;
    x = ((sp + (i64)(*pc).n) - (i64)1);
    qq_decls$pcptr = pc;
    qq_vars$var_expand(sp,x,(i64)(*pc).n);
    sp = x;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpushtry:
L1068 :;
;
    (*++(sp)).tagx = (i64)21;
    (*sp).ptr = (byte *)(*pc).labelref;
    (*sp).frameoffset = (fp - (byte *)sp);
    (*sp).exceptiontype = (i64)(*pc).x;
    (*sp).nexceptions = (i64)(*pc).y;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jraise:
L1069 :;
;
    if (((i64)(*sp).tag != (i64)1)) {
        qq_runaux$pcerror((byte*)"Raise: not Int",(byte*)"");
    }
;
    {
        qq_decls$pcptr = pc;
        qq_runaux$pcerror((byte*)"RAISE",(byte*)"");
    }
    goto *(*pc).labaddr;
;
    //jmap:
L1070 :;
;
    qq_decls$pcptr = pc;
    pc = qq_runaux$k_map(sp,pc,&newsp);
    sp = newsp;
    goto *(*pc).labaddr;
;
    //jaddsp:
L985 :;
;
    sp -= (i64)(*pc).n;
    ++(pc);
    goto *(*pc).labaddr;
;
    //jpushff:
L1072 :;
;
    ++(sp);
    (*sp) = (*(struct qq_decls$varrec *)(fp + (*pc).offset));
    if (!!((i64)(*sp).hasref)) {
        ++((*(*sp).objptr).refcount);
    }
;
    ++(sp);
    (*sp) = (*(struct qq_decls$varrec *)(fp + (*(pc + (i64)1)).offset));
    if (!!((i64)(*sp).hasref)) {
        ++((*(*sp).objptr).refcount);
    }
;
    pc += (i64)2;
    goto *(*pc).labaddr;
;
    //jpushfff:
L1071 :;
;
    ++(sp);
    (*sp) = (*(struct qq_decls$varrec *)(fp + (*pc).offset));
    if (!!((i64)(*sp).hasref)) {
        ++((*(*sp).objptr).refcount);
    }
;
    ++(sp);
    (*sp) = (*(struct qq_decls$varrec *)(fp + (*(pc + (i64)1)).offset));
    if (!!((i64)(*sp).hasref)) {
        ++((*(*sp).objptr).refcount);
    }
;
    ++(sp);
    (*sp) = (*(struct qq_decls$varrec *)(fp + (*(pc + (i64)2)).offset));
    if (!!((i64)(*sp).hasref)) {
        ++((*(*sp).objptr).refcount);
    }
;
    pc += (i64)3;
    goto *(*pc).labaddr;
;
    //jpushmci:
L1076 :;
;
    ++(sp);
    (*sp) = (*(*pc).varptr);
    if (!!((i64)(*sp).hasref)) {
        ++((*(*sp).objptr).refcount);
    }
;
    ++(sp);
    (*sp).tagx = (i64)1;
    (*sp).value = (*(pc + (i64)1)).value;
    pc += (i64)2;
    goto *(*pc).labaddr;
;
    //jpushfci:
L1077 :;
;
    ++(sp);
    (*sp) = (*(struct qq_decls$varrec *)(fp + (*pc).offset));
    if (!!((i64)(*sp).hasref)) {
        ++((*(*sp).objptr).refcount);
    }
;
    ++(sp);
    (*sp).tagx = (i64)1;
    (*sp).value = (*(pc + (i64)1)).value;
    pc += (i64)2;
    goto *(*pc).labaddr;
;
    //jaddff:
L1102 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    y = (struct qq_decls$varrec *)(fp + (*(pc + (i64)1)).offset);
    if (((i64)(*x).tag==(i64)(*y).tag && (i64)(*y).tag==(i64)1)) {
        ++(sp);
        (*sp).tagx = (i64)1;
        (*sp).value = ((*x).value + (*y).value);
        pc += (i64)3;
    }
    else {
        goto L935 ;
;
    }
;
    goto *(*pc).labaddr;
;
    //jaddfci:
L1100 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if (((i64)(*x).tag == (i64)1)) {
        ++(sp);
        (*sp).tagx = (i64)1;
        (*sp).value = ((*x).value + (*(pc + (i64)1)).value);
        pc += (i64)3;
    }
    else {
        goto L935 ;
;
    }
;
    goto *(*pc).labaddr;
;
    //jaddci:
L1104 :;
;
    if (((i64)(*sp).tag == (i64)1)) {
        (*sp).value += (*pc).value;
        pc += (i64)2;
    }
    else {
        goto L940 ;
;
    }
;
    goto *(*pc).labaddr;
;
    //jmovefci:
L1083 :;
;
    x = (struct qq_decls$varrec *)(fp + (*(pc + (i64)1)).offset);
    if (!!((i64)(*x).hasref)) {
        qq_vars$var_unshareu(x);
    }
;
    (*x).tagx = (i64)1;
    (*x).value = (*pc).value;
    pc += (i64)2;
    goto *(*pc).labaddr;
;
    //jmoveff:
L1078 :;
;
    x = (struct qq_decls$varrec *)(fp + (*(pc + (i64)1)).offset);
    y = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if (!!((i64)(*y).hasref)) {
        ++((*(*y).objptr).refcount);
    }
;
    if (!!((i64)(*x).hasref)) {
        qq_vars$var_unshareu(x);
    }
;
    (*x) = (*y);
    pc += (i64)2;
    goto *(*pc).labaddr;
;
    //jindexff:
L1106 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    //doindexff:
L1142 :;
;
    y = (struct qq_decls$varrec *)(fp + (*(pc + (i64)1)).offset);
    ++(sp);
    (*sp) = (*x);
    qq_decls$pcptr = pc;
        {i64 $temp = (i64)(*y).tag;
if (($temp==(i64)1)) {
        qq_vars$var_getix(sp,(*y).value);
    }
    else if (($temp==(i64)4)) {
        qq_vars$var_getslice(sp,msysc$m_getdotslice((*y).dummy,(i64)16,(i64)63),(i64)(*y).range_upper);
    }
    else {
        qq_decls$pcptr = pc;
        qq_runaux$pcmxtypes((byte*)"Indexff",x,y);
    }
    };
    pc += (i64)3;
    goto *(*pc).labaddr;
;
    //jindexmf:
L1105 :;
;
    x = (*pc).varptr;
    goto L1142 ;
;
    //jwheneqci:
L1117 :;
;
    x = sp;
    if (((i64)(*x).tag == (i64)1)) {
        if (((*x).value == (*pc).value)) {
            --(sp);
            pc = (*(pc + (i64)1)).labelref;
        }
        else {
            pc += (i64)2;
        }
;
    }
    else {
        goto L940 ;
;
    }
;
    goto *(*pc).labaddr;
;
    //jwhenneci:
L1118 :;
;
    x = sp;
    if (((i64)(*x).tag == (i64)1)) {
        if (((*x).value != (*pc).value)) {
            pc = (*(pc + (i64)1)).labelref;
        }
        else {
            --(sp);
            pc += (i64)2;
        }
;
    }
    else {
        goto L940 ;
;
    }
;
    goto *(*pc).labaddr;
;
    //jupbm:
L1113 :;
;
    ++(sp);
    (*sp) = (*(*pc).varptr);
    if (!!((i64)(*sp).hasref)) {
        ++((*(*sp).objptr).refcount);
    }
;
    qq_decls$pcptr = pc;
    qq_runaux$k_upb(sp);
    pc += (i64)2;
    goto *(*pc).labaddr;
;
    //jpushipm:
L1109 :;
;
    x = (*pc).varptr;
    if ((((i64)(*x).tag != (i64)16) || ((i64)(*x).elemtag != (i64)27))) {
        goto L936 ;
;
    }
;
    goto L1143 ;
;
    //jpushipf:
L1110 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if ((((i64)(*x).tag != (i64)16) || ((i64)(*x).elemtag != (i64)27))) {
        goto L937 ;
;
    }
;
    //dopushipf:
L1143 :;
;
    ++(sp);
    (*sp).tagx = (i64)1;
        {i64 $temp = (i64)(*x).elemtag;
if (($temp==(i64)27)) {
        (*sp).value = (i64)(*(*x).ptr);
        (*x).ptr += (i64)(*(pc + (i64)1)).x;
    }
    };
    pc += (i64)3;
    goto *(*pc).labaddr;
;
    //jpopipm:
L1111 :;
;
    x = (*pc).varptr;
    if (((((i64)(*x).tag != (i64)16) || ((i64)(*x).elemtag != (i64)27)) || ((i64)(*sp).tag != (i64)1))) {
        goto L936 ;
;
    }
;
    goto L1144 ;
;
    //jpopipf:
L1112 :;
;
    x = (struct qq_decls$varrec *)(fp + (*pc).offset);
    if (((((i64)(*x).tag != (i64)16) || ((i64)(*x).elemtag != (i64)27)) || ((i64)(*sp).tag != (i64)1))) {
        goto L937 ;
;
    }
;
    //dopopipf:
L1144 :;
;
        {i64 $temp = (i64)(*x).elemtag;
if (($temp==(i64)27)) {
        (*(*x).ptr) = (*sp).value;
        (*x).ptr += (i64)(*(pc + (i64)1)).x;
    }
    };
    --(sp);
    pc += (i64)3;
    goto *(*pc).labaddr;
;
    //jlastpcl:
L1119 :;
;
    //jpushmm:
L1073 :;
;
    //jpushmf:
L1075 :;
;
    //jpushfm:
L1074 :;
;
    //jmovemm:
L1079 :;
;
    //jmovefm:
L1080 :;
;
    //jmovemf:
L1081 :;
;
    //jzmoveff:
L1082 :;
;
    //jzmovemci:
L1145 :;
;
    //jzmovefci:
L1085 :;
;
    //jmovemci:
L1084 :;
;
    //jpushv2:
L1086 :;
;
    //jpushv3:
L1087 :;
;
    //jsubfci:
L1101 :;
;
    //jsubff:
L1103 :;
;
    //jswitchf:
L1107 :;
;
    //jupbf:
L1114 :;
;
    //jlenf:
L1115 :;
;
    //jstoref:
L1116 :;
;
    //unimpl:
L1120 :;
;
    {
        qq_decls$pcptr = pc;
        qq_runaux$pcerror((byte*)"Unimpl op:",qq_pcltabs$pclnames[((i64)(*pc).opcode)]);
    }
    exit((i64)1);
}

// START
void qq_runlab$start(void) {

    qq_runlab$getjt = (i64)1;
    qq_runlab$disploop();
    qq_runlab$getjt = (i64)0;
}

void qq_runlab$fixupcode(struct qq_decls$filerec *pm) {
        struct qq_pcltabs$pclrec *  pc;
    pc = (*pm).pcstart;
    L1146 :;
    while (((i64)(*pc).opcode != (i64)5)) {
        (*pc).labaddr = (*qq_runlab$jumptable)[((i64)(*pc).opcode)];
L1147 :;
        ++(pc);
L1149 :;
            }
L1148 :;
    ;
}

i64 qq_runlab$runqprogram(struct qq_decls$subprogrec *sp,i64 ismain) {
        i64 tt;
    if (((i64)qq_cli$runcode < (i64)6)) {
        return (i64)0;
    }
;
    qq_decls$sptr = &qq_decls$varstack[((i64)1)-1];
    qq_decls$stacklimit = &qq_decls$varstack[((i64)69900)-1];
    qq_decls$pcptr = (*qq_decls$modules[((i64)(*sp).firstmodule)]).pcstart;
    qq_decls$stopped = (i64)0;
    tt = clock();
    qq_runlab$disploop();
    tt = (clock() - tt);
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Time:",NULL);
    msysc$m_print_i64(tt,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    if (!(!!(ismain))) {
        return (*qq_decls$sptr).value;
    }
;
    return (*qq_decls$sptr).value;
}

void qq_runaux$pcerror(u8 *mess,u8 *param) {
    qq_runaux$reportpcerror(qq_decls$pcptr,mess,param);
}

void qq_runaux$pcustype(u8 *mess,struct qq_decls$varrec *x) {
    qq_runaux$pcustype_t(mess,(i64)(*x).tag);
}

void qq_runaux$pcustype_t(u8 *mess,i64 t) {
        u8 str[256];
    msysc$m_print_startstr(str);
    msysc$m_print_setfmt((byte*)"Type not supported: # : #");
    msysc$m_print_str(mess,NULL);
    msysc$m_print_str(qq_tables$ttname[(t)],NULL);
    msysc$m_print_end();
    ;
    qq_runaux$reportpcerror(qq_decls$pcptr,str,(byte*)"");
}

void qq_runaux$pcmxtypes(u8 *mess,struct qq_decls$varrec *x,struct qq_decls$varrec *y) {
    qq_runaux$pcmxtypestt(mess,(i64)(*x).tag,(i64)(*y).tag);
}

void qq_runaux$pcmxtypestt(u8 *mess,i64 t,i64 u) {
        u8 str[256];
    msysc$m_print_startstr(str);
    msysc$m_print_setfmt((byte*)"Types not supported: # : #/#");
    msysc$m_print_str(mess,NULL);
    msysc$m_print_str(qq_tables$ttname[(t)],NULL);
    msysc$m_print_str(qq_tables$ttname[(u)],NULL);
    msysc$m_print_end();
    ;
    qq_runaux$reportpcerror(qq_decls$pcptr,str,(byte*)"");
}

void qq_runaux$reportpcerror(struct qq_pcltabs$pclrec *pcptr,u8 *mess,u8 *param) {
        struct qq_decls$varrec *  s;
        struct qq_decls$varrec *  send;
        struct qq_pcltabs$pclrec *  pc;
        i64 count;
        struct qq_decls$filerec *  pm;
        struct qq_decls$locrec loc;
        struct qq_decls$locrec loc2;
    loc = qq_runaux$getpcerrorpos(pcptr);
    pm = loc.pm;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)" ",(byte*)"80p*");
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"PC Error:",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"    ",NULL);
    msysc$m_print_nogap();
    msysc$m_print_str(mess,NULL);
    msysc$m_print_nogap();
    msysc$m_print_str(param,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Line:",NULL);
    msysc$m_print_i64(loc.lineno,NULL);
    msysc$m_print_str((byte*)"in Module",NULL);
    msysc$m_print_str((*loc.pm).name,NULL);
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)".q:",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    if (!!(loc.def)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"In function:",NULL);
        msysc$m_print_str((*loc.def).name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
    }
;
    s = qq_decls$sptr;
    send = &qq_decls$varstack[((i64)1)-1];
    count = (i64)0;
    L1150 :;
    while (((s >= send) && (count < (i64)5))) {
        if (((i64)(*s).tag == (i64)20)) {
            pc = ((*s).retaddr - (i64)1);
            loc2 = qq_runaux$getpcerrorpos(pc);
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"Called from line",NULL);
            msysc$m_print_i64(loc2.lineno,NULL);
            msysc$m_print_str((byte*)"in",NULL);
            msysc$m_print_str((*loc2.pm).name,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            ++(count);
        }
;
        --(s);
L1151 :;
    }
L1152 :;
    ;
    qq_lib$stopcompiler(loc);
}

static struct qq_decls$locrec qq_runaux$getpcerrorpos(struct qq_pcltabs$pclrec *pc) {
        i64 offset;
        i64 pos;
        i64 soffset;
        i64 moduleno;
        struct qq_pcltabs$pclrec *  pcstart;
        i32 *  pcsrcstart;
        struct qq_decls$filerec *  pm;
        struct qq_decls$locrec loc;
    memset(&(loc),0,48);
    pm = qq_decls$modules[(qq_runaux$findmodulefrompc(pc))];
    pcstart = (*pm).pcstart;
    pcsrcstart = (*pm).pcsourcestart;
    offset = (pc - pcstart);
    pos = (i64)(*(pcsrcstart + offset));
    loc.lineno = (i64)msysc$m_getdotslice(pos,(i64)0,(i64)23);
    moduleno = (i64)msysc$m_getdotslice(pos,(i64)24,(i64)31);
    if ((moduleno == (i64)0)) {
        moduleno = (i64)1;
        soffset = (i64)0;
    }
;
    loc.pm = qq_decls$modules[(moduleno)];
    loc.sp = qq_decls$subprogs[((i64)(*pm).subprogno)-1];
    loc.def = 0;
    return loc;
}

static i64 qq_runaux$findmodulefrompc(struct qq_pcltabs$pclrec *pc) {
        i64 i;
    for (i=(i64)1;i<=qq_decls$nmodules;++i) {
L1153 :;
        if (((pc >= (*qq_decls$modules[(i)]).pcstart) && (pc < (*qq_decls$modules[(i)]).pcend))) {
            return i;
        }
;
L1154 :;
    }
L1155 :;
    ;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"Can't find pcptr module",NULL);
    msysc$m_print_ptr(pc,NULL);
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

struct qq_decls$varrec *qq_runaux$k_makelist(struct qq_decls$varrec *sp,i64 lower,i64 n) {
        struct qq_decls$varrec *  x;
    x = ((sp - n) + (i64)1);
    sp = x;
    qq_lists$var_make_list(x,sp,n,lower);
    (*(*sp).objptr).flags = msysc$m_setdotindex((*(*sp).objptr).flags,(i64)1,(u64)0u);
    return sp;
}

void qq_runaux$k_len(struct qq_decls$varrec *sp) {
        struct qq_decls$objrec *  p;
        i64 n;
        i64 t;
    p = (*sp).objptr;
        {i64 $temp = (i64)(*sp).tag;
if (($temp==(i64)10) || ($temp==(i64)11) || ($temp==(i64)6) || ($temp==(i64)8)) {
        n = (*p).length;
    }
    else if (($temp==(i64)9)) {
        n = (*p).length;
    }
    else if (($temp==(i64)12) || ($temp==(i64)7) || ($temp==(i64)13)) {
        n = qq_tables$ttlength[((i64)(*p).usertag)];
    }
    else if (($temp==(i64)5)) {
        n = (*p).length;
    }
    else if (($temp==(i64)4)) {
        n = (((i64)(*sp).range_upper - msysc$m_getdotslice((*sp).dummy,(i64)16,(i64)63)) + (i64)1);
    }
    else if (($temp==(i64)3)) {
        n = qq_decimal$obj_len_dec(p);
    }
    else if (($temp==(i64)18)) {
        t = (*sp).value;
                {i64 $temp = (i64)qq_tables$ttbasetype[(t)];
if (($temp==(i64)12) || ($temp==(i64)7) || ($temp==(i64)13)) {
            n = qq_tables$ttlength[(t)];
        }
        else {
            n = qq_tables$ttlength[(t)];
        }
        };
    }
    else {
        qq_runaux$pcustype((byte*)"Len",sp);
    }
    };
    if (!!((i64)(*sp).hasref)) {
        qq_vars$var_unshareu(sp);
    }
;
    (*sp).tagx = (i64)1;
    (*sp).value = n;
}

void qq_runaux$k_maths(struct qq_decls$varrec *sp,i64 opc) {
        r64 x;
    x = (*sp).xvalue;
        {i64 $temp = (i64)(*sp).tag;
if (($temp==(i64)1)) {
        if ((opc == (i64)2)) {
            (*sp).value = ((*sp).value*(*sp).value);
            return;
        }
;
        (*sp).tagx = (i64)2;
        x = (r64)(*sp).value;
    }
    else if (($temp==(i64)2)) {
    }
    else {
        qq_runaux$pcustype((byte*)"Maths:",sp);
    }
    };
    if ((opc==(i64)2)) {
        x = (x*x);
    }
    else if ((opc==(i64)1)) {
        x = sqrt(x);
    }
    else if ((opc==(i64)3)) {
        x = sin(x);
    }
    else if ((opc==(i64)4)) {
        x = cos(x);
    }
    else if ((opc==(i64)13)) {
        if ((x >= (double)0.)) {
            x = floor((x + (double)0.5));
        }
        else {
            x = ceil((x - (double)0.5));
        }
;
    }
    else if ((opc==(i64)14)) {
        x = floor(x);
    }
    else {
        qq_runaux$pcerror((byte*)"Maths op:",qq_tables$mathsnames[(opc)-1]);
    }
;
    (*sp).xvalue = x;
}

void qq_runaux$k_lwb(struct qq_decls$varrec *sp) {
        struct qq_decls$objrec *  p;
        i64 n;
    p = (*sp).objptr;
        {i64 $temp = (i64)(*sp).tag;
if (($temp==(i64)10)) {
        n = (i64)(*p).lower16;
    }
    else if (($temp==(i64)9) || ($temp==(i64)6)) {
        n = (i64)1;
    }
    else if (($temp==(i64)11) || ($temp==(i64)8)) {
        n = (i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0);
    }
    else if (($temp==(i64)12) || ($temp==(i64)13)) {
        n = (i64)1;
    }
    else if (($temp==(i64)7)) {
        n = qq_tables$ttlower[((i64)(*p).usertag)];
    }
    else if (($temp==(i64)5)) {
        n = (i64)0;
    }
    else if (($temp==(i64)4)) {
        n = msysc$m_getdotslice((*sp).dummy,(i64)16,(i64)63);
    }
    else {
        qq_runaux$pcustype((byte*)"Lwb",sp);
    }
    };
    if (!!((i64)(*sp).hasref)) {
        qq_vars$var_unshareu(sp);
    }
;
    (*sp).tagx = (i64)1;
    (*sp).value = n;
}

void qq_runaux$k_upb(struct qq_decls$varrec *sp) {
        struct qq_decls$objrec *  p;
        i64 n;
        i64 t;
    p = (*sp).objptr;
        {i64 $temp = (i64)(*sp).tag;
if (($temp==(i64)10)) {
        n = (((*p).length + (i64)(*p).lower16) - (i64)1);
    }
    else if (($temp==(i64)9) || ($temp==(i64)6)) {
        n = (*p).length;
    }
    else if (($temp==(i64)11) || ($temp==(i64)8)) {
        n = (((*p).length + (i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0)) - (i64)1);
    }
    else if (($temp==(i64)12) || ($temp==(i64)13)) {
        n = qq_tables$ttlength[((i64)(*p).usertag)];
    }
    else if (($temp==(i64)7)) {
        t = (i64)(*p).usertag;
        goto L1156 ;
;
    }
    else if (($temp==(i64)5)) {
        n = ((*p).length - (i64)1);
    }
    else if (($temp==(i64)4)) {
        n = (i64)(*sp).range_upper;
    }
    else if (($temp==(i64)18)) {
        t = (*sp).value;
        //dotype:
L1156 :;
;
                {i64 $temp = (i64)qq_tables$ttbasetype[(t)];
if (($temp==(i64)7)) {
            n = ((qq_tables$ttlength[(t)] + qq_tables$ttlower[(t)]) - (i64)1);
        }
        else {
            qq_runaux$pcustype((byte*)"t.upb",sp);
        }
        };
    }
    else {
        qq_runaux$pcustype((byte*)"Upb",sp);
    }
    };
    if (!!((i64)(*sp).hasref)) {
        qq_vars$var_unshareu(sp);
    }
;
    (*sp).tagx = (i64)1;
    (*sp).value = n;
}

void qq_runaux$k_swap(struct qq_decls$varrec *x,struct qq_decls$varrec *y) {
        byte tempbuffer[1024];
        struct qq_decls$varrec v;
        i64 s;
        i64 t;
        i64 n;
        byte *  p;
        byte *  q;
        i64 a;
    if (((i64)(*x).tag != (i64)(*y).tag)) {
        qq_runaux$pcerror((byte*)"Swap mismatch",(byte*)"");
    }
;
        {i64 $temp = (i64)(*x).tag;
if (($temp==(i64)14)) {
        v = (*(*x).varptr);
        (*(*x).varptr) = (*(*y).varptr);
        (*(*y).varptr) = v;
    }
    else if (($temp==(i64)16)) {
        s = (i64)(*x).elemtag;
        t = (i64)(*y).elemtag;
        if ((s != t)) {
            goto L1157 ;
;
        }
;
        n = qq_tables$ttsize[(s)];
        if ((n==(i64)1)) {
            p = (*x).ptr;
            q = (*y).ptr;
            a = (i64)(*p);
            (*p) = (i64)(*q);
            (*q) = a;
        }
        else {
            if ((qq_tables$ttsize[(s)] <= (i64)1024)) {
                memcpy(&tempbuffer,(*x).ptr,(u64)n);
                memcpy((*x).ptr,(*y).ptr,(u64)n);
                memcpy((*y).ptr,&tempbuffer,(u64)n);
            }
            else {
                goto L1157 ;
;
            }
;
        }
;
    }
    else {
        //swaperror:
L1157 :;
;
        qq_runaux$pcmxtypes((byte*)"Swap",x,y);
    }
    };
}

void qq_runaux$k_bounds(struct qq_decls$varrec *sp,i64 *lower,i64 *upper) {
        i64 a;
        i64 b;
        i64 m;
        i64 t;
        struct qq_decls$objrec *  p;
    m = (i64)(*sp).tag;
    p = (*sp).objptr;
    if ((m==(i64)10)) {
        a = (i64)(*p).lower16;
        b = (((*p).length + a) - (i64)1);
    }
    else if ((m==(i64)11) || (m==(i64)8)) {
        a = (i64)msysc$m_getdotindex((i64)(*p).flags,(i64)0);
        b = (((*p).length + a) - (i64)1);
    }
    else if ((m==(i64)9) || (m==(i64)6)) {
        a = (i64)1;
        b = (*p).length;
    }
    else if ((m==(i64)4)) {
        a = msysc$m_getdotslice((*sp).dummy,(i64)16,(i64)63);
        b = (i64)(*sp).range_upper;
    }
    else if ((m==(i64)13) || (m==(i64)12)) {
        a = (i64)1;
        b = qq_tables$ttlength[((i64)(*p).usertag)];
    }
    else if ((m==(i64)7)) {
        t = (i64)(*p).usertag;
        goto L1158 ;
;
    }
    else if ((m==(i64)5)) {
        a = (i64)0;
        b = ((*p).length - (i64)1);
    }
    else if ((m==(i64)18)) {
        t = (*sp).value;
        //dotype:
L1158 :;
;
                {i64 $temp = (i64)qq_tables$ttbasetype[(t)];
if (($temp==(i64)7) || ($temp==(i64)13)) {
            a = qq_tables$ttlower[(t)];
            b = ((qq_tables$ttlength[(t)] + a) - (i64)1);
        }
        else {
            qq_runaux$pcustype((byte*)"t.bounds",sp);
        }
        };
    }
    else {
        qq_runaux$pcustype((byte*)"Bounds",sp);
    }
;
    (*lower) = a;
    (*upper) = b;
}

i64 qq_runaux$k_type(struct qq_decls$varrec *sp,i64 n) {
        i64 t;
    t = (i64)(*sp).tag;
    if ((n==(i64)0)) {
    }
    else if ((n==(i64)1)) {
        if ((t==(i64)12) || (t==(i64)13) || (t==(i64)7)) {
            t = (i64)(*(*sp).objptr).usertag;
        }
;
    }
    else {
        if ((t==(i64)11) || (t==(i64)8)) {
            t = (i64)(*(*sp).objptr).elemtag;
        }
        else if ((t==(i64)16) || (t==(i64)14) || (t==(i64)15)) {
            t = (i64)(*sp).elemtag;
        }
        else if ((t==(i64)5)) {
            t = (i64)33;
        }
        else if ((t==(i64)7)) {
            t = (i64)qq_tables$tttarget[((i64)(*(*sp).objptr).usertag)];
        }
        else {
            t = (i64)0;
        }
;
    }
;
    return t;
}

void qq_runaux$k_dot(struct qq_decls$varrec *sp,i64 index) {
        struct qq_decls$strec *  d;
        struct qq_decls$varrec *  p;
        i64 rectype;
        struct qq_decls$varrec v;
    //restart:
L1159 :;
;
        {i64 $temp = (i64)(*sp).tag;
if (($temp==(i64)12) || ($temp==(i64)13)) {
    }
    else if (($temp==(i64)14)) {
        sp = (*sp).varptr;
        goto L1159 ;
;
    }
    else {
        qq_runaux$pcustype((byte*)"1:dot/not record",sp);
    }
    };
    rectype = (i64)(*(*sp).objptr).usertag;
    d = qq_runaux$resolvefield(index,rectype);
        {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)11)) {
        p = ((*(*sp).objptr).varptr + ((i64)(*d).fieldoffset / (i64)16));
        if (!!((i64)(*p).hasref)) {
            ++((*(*p).objptr).refcount);
        }
;
        if (!!((i64)(*sp).hasref)) {
            qq_vars$var_unshareu(sp);
        }
;
        (*sp) = (*p);
    }
    else if (($temp==(i64)12)) {
        qq_packed$var_loadpacked(((*(*sp).objptr).ptr + (i64)(*d).fieldoffset),(i64)(*d).mode,&v,0);
        if (!!((i64)(*sp).hasref)) {
            qq_vars$var_unshareu(sp);
        }
;
        (*sp) = v;
    }
    else if (($temp==(i64)5)) {
        (*sp).tagx = (i64)17;
        (*sp).def = d;
    }
    else if (($temp==(i64)21)) {
        (*sp).tagx = (i64)17;
        (*sp).def = (*d).alias;
    }
    else {
        qq_runaux$pcerror((byte*)"DOT: can't do this fieldtype:",qq_tables$namenames[((i64)(*d).nameid)]);
    }
    };
}

void qq_runaux$k_dotref(struct qq_decls$varrec *sp,i64 index) {
        struct qq_decls$strec *  d;
        struct qq_decls$varrec *  p;
        byte *  q;
        i64 rectype;
    //restart:
L1160 :;
;
        {i64 $temp = (i64)(*sp).tag;
if (($temp==(i64)12) || ($temp==(i64)13)) {
    }
    else if (($temp==(i64)14)) {
        sp = (*sp).varptr;
        goto L1160 ;
;
    }
    else {
        qq_runaux$pcustype((byte*)"2:dot/not record",sp);
    }
    };
    rectype = (i64)(*(*sp).objptr).usertag;
    d = qq_runaux$resolvefield(index,rectype);
        {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)11)) {
        p = ((*(*sp).objptr).varptr + ((i64)(*d).fieldoffset / (i64)16));
        if (!!((i64)(*sp).hasref)) {
            qq_vars$var_unshareu(sp);
        }
;
        (*sp).tagx = (i64)14;
        (*sp).varptr = p;
    }
    else if (($temp==(i64)12)) {
        q = ((*(*sp).objptr).ptr + (i64)(*d).fieldoffset);
        if (!!((i64)(*sp).hasref)) {
            qq_vars$var_unshareu(sp);
        }
;
        (*sp).tagx = (i64)16;
        (*sp).ptr = q;
        (*sp).elemtag = (i64)(*d).mode;
    }
    else {
        qq_runaux$pcerror((byte*)"DOTREF: can't do this fieldtype:",qq_tables$namenames[((i64)(*d).nameid)]);
    }
    };
}

struct qq_decls$varrec *qq_runaux$k_popdot(struct qq_decls$varrec *sp,i64 index) {
        struct qq_decls$strec *  d;
        struct qq_decls$varrec *  p;
        struct qq_decls$varrec *  x;
        struct qq_decls$varrec *  y;
    x = (sp)--;
    y = (sp)--;
        {i64 $temp = (i64)(*x).tag;
if (($temp==(i64)12) || ($temp==(i64)13)) {
    }
    else {
        qq_runaux$pcustype((byte*)"3:dot/not record",x);
    }
    };
    d = qq_runaux$resolvefield(index,(i64)(*(*x).objptr).usertag);
    if (!(!!((i64)(*x).hasref))) {
        qq_runaux$pcerror((byte*)"POPDOT",(byte*)"");
    }
;
    if (!(!!(msysc$m_getdotindex((i64)(*(*x).objptr).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
        {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)11)) {
        p = ((*(*x).objptr).varptr + ((i64)(*d).fieldoffset / (i64)16));
        if (!!((i64)(*p).hasref)) {
            qq_vars$var_unshareu(p);
        }
;
        (*p) = (*y);
        if (!!((i64)(*x).hasref)) {
            qq_vars$var_unshareu(x);
        }
;
    }
    else if (($temp==(i64)12)) {
        qq_packed$var_storepacked(((*(*x).objptr).ptr + (i64)(*d).fieldoffset),y,(i64)(*d).mode);
        if (!!((i64)(*x).hasref)) {
            qq_vars$var_unshareu(x);
        }
;
    }
    else {
        qq_runaux$pcerror((byte*)"POPDOT: can't do this fieldtype:",qq_tables$namenames[((i64)(*d).nameid)]);
    }
    };
    return sp;
}

static struct qq_decls$strec *qq_runaux$resolvefield(i64 index,i64 rectype) {
        struct qq_decls$strec *  d;
        struct qq_decls$genfieldrec *  g;
    if ((index == (i64)0)) {
        qq_runaux$pcerror((byte*)"Not a field",(byte*)"");
    }
;
    g = (struct qq_decls$genfieldrec *)qq_decls$genfieldtable[(index)-1];
    L1161 :;
    while (!!(g)) {
        d = (*g).def;
        if (((i64)(*(*d).owner).mode == rectype)) {
            return d;
        }
;
        g = (struct qq_decls$genfieldrec *)(*g).nextdef;
L1162 :;
    }
L1163 :;
    ;
    qq_runaux$pcerror((byte*)"Can't resolve field:",(*d).name);
    return (struct qq_decls$strec *)0;
}

void qq_runaux$k_convrefpack(struct qq_decls$varrec *sp) {
        struct qq_decls$varrec *  a;
        i64 elemtype;
        void *  p;
        struct qq_decls$objrec *  pa;
        {i64 $temp = (i64)(*sp).tag;
if (($temp==(i64)14)) {
        a = (*sp).varptr;
        pa = (*a).objptr;
                {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1) || ($temp==(i64)16)) {
            p = &(*a).value;
            elemtype = (i64)26;
        }
        else if (($temp==(i64)2)) {
            p = &(*a).value;
            elemtype = (i64)32;
        }
        else if (($temp==(i64)11)) {
            p = (*pa).ptr;
            elemtype = (i64)(*pa).elemtag;
        }
        else if (($temp==(i64)8)) {
            (*sp).ptr = (*pa).ptr;
            (*sp).bitoffset = ((i64)(*pa).indexoffset * (i64)qq_tables$ttbitwidth[((i64)(*pa).elemtag)]);
            (*sp).bitlength = (i64)0;
            (*sp).tagx = (i64)15;
            (*sp).elemtag = (i64)(*pa).elemtag;
            return;
        }
        else if (($temp==(i64)5)) {
            (*sp).ptr = (*pa).ptr;
            (*sp).bitoffset = (i64)0;
            (*sp).bitlength = (i64)0;
            (*sp).tagx = (i64)15;
            (*sp).elemtag = (i64)33;
            return;
        }
        else if (($temp==(i64)9)) {
            p = (void *)(*pa).strptr;
            elemtype = (i64)27;
            if ((p == 0)) {
                p = (byte*)"";
            }
;
        }
        else if (($temp==(i64)13)) {
            p = (*pa).ptr;
            elemtype = (i64)(*pa).usertag;
        }
        else if (($temp==(i64)7)) {
            p = (*pa).ptr;
            elemtype = (i64)(*pa).usertag;
        }
        else if (($temp==(i64)3)) {
            p = (*pa).num;
            elemtype = (i64)25;
        }
        else {
            qq_runaux$pcustype((byte*)"Getrefpack1",a);
        }
        };
    }
    else if (($temp==(i64)16) || ($temp==(i64)15)) {
        return;
    }
    else {
        qq_runaux$pcustype((byte*)"Getrefpack2",sp);
    }
    };
    //done:
L1164 :;
;
    (*sp).tagx = (i64)16;
    (*sp).ptr = (byte *)p;
    (*sp).elemtag = elemtype;
}

void qq_runaux$k_incrptr(struct qq_decls$varrec *p,i64 step) {
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)14)) {
        p = (*p).varptr;
                {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)1)) {
            (*p).value += step;
        }
        else if (($temp==(i64)14)) {
            (*p).varptr += step;
        }
        else if (($temp==(i64)16)) {
            (*p).ptr += (qq_tables$ttsize[((i64)(*p).elemtag)] * step);
        }
        else if (($temp==(i64)2)) {
            (*p).xvalue += (r64)step;
        }
        else {
            qq_runaux$pcustype((byte*)"incrptr/refvar",p);
        }
        };
    }
    else if (($temp==(i64)16)) {
                {i64 $temp = (i64)(*p).elemtag;
if (($temp==(i64)27) || ($temp==(i64)23)) {
            (*(*p).ptr) += (byte)step;
        }
        else if (($temp==(i64)28) || ($temp==(i64)24)) {
            (*(u16 *)(*p).ptr) += (u16)step;
        }
        else {
            qq_runaux$pcustype_t((byte*)"incrptr/ref",(i64)(*p).elemtag);
        }
        };
    }
    else {
        qq_runaux$pcustype((byte*)"incrptr",p);
    }
    };
}

i64 qq_runaux$k_cmp(i64 cc,struct qq_decls$varrec *x,struct qq_decls$varrec *y) {
        i64 res;
    if ((cc==(i64)0)) {
        return qq_vars$var_equal(x,y);
    }
    else if ((cc==(i64)1)) {
        return (i64)!(!!(qq_vars$var_equal(x,y)));
    }
    else {
        res = qq_vars$var_compare(x,y);
        if ((cc==(i64)2)) {
            return (i64)(res < (i64)0);
        }
        else if ((cc==(i64)3)) {
            return (i64)(res <= (i64)0);
        }
        else if ((cc==(i64)4)) {
            return (i64)(res >= (i64)0);
        }
        else {
            return (i64)(res > (i64)0);
        }
;
    }
;
}

i64 qq_runaux$k_bytesize(struct qq_decls$varrec *sp) {
        i64 t;
        struct qq_decls$objrec *  p;
    p = (*sp).objptr;
    t = (i64)(*sp).tag;
    if ((t==(i64)18)) {
        t = (*sp).value;
    }
    else if ((t==(i64)13) || (t==(i64)12) || (t==(i64)7)) {
        t = (i64)(*(*sp).objptr).usertag;
    }
;
    if ((t==(i64)11)) {
        return ((*p).length * qq_tables$ttsize[((i64)(*p).elemtag)]);
    }
    else if ((t==(i64)5)) {
        return qq_bits$getbitssize((*p).length,(i64)33);
    }
    else if ((t==(i64)9)) {
        return (*p).length;
    }
    else if ((t==(i64)8)) {
        return qq_bits$bits_bytesize(p);
    }
    else if ((t==(i64)10) || (t==(i64)6)) {
        return ((*p).length * (i64)16);
    }
    else if ((t==(i64)12) || (t==(i64)13) || (t==(i64)7)) {
        return qq_tables$ttsize[(t)];
    }
    else if ((t==(i64)3)) {
        return ((*p).length * (i64)4);
    }
    else {
        return qq_tables$ttsize[(t)];
    }
;
}

i64 qq_runaux$k_when(struct qq_decls$varrec *x,struct qq_decls$varrec *y) {
        {i64 $temp = (((i64)(*x).tag << (i64)16) | (i64)(*y).tag);
if (($temp==(i64)65540)) {
        if ((($rtemp=(*x).value, $rtemp >= msysc$m_getdotslice((*y).dummy,(i64)16,(i64)63) && $rtemp <= (i64)(*y).range_upper))) {
            return (i64)1;
        }
        else {
            return (i64)0;
        }
;
    }
    else if (($temp==(i64)65541) || ($temp==(i64)1179653)) {
        return qq_sets$var_in_set(x,y);
    }
    else {
        return qq_vars$var_equal(x,y);
    }
    };
}

struct qq_pcltabs$pclrec *qq_runaux$raiseexception(i64 exceptno,struct qq_decls$varrec **sp,byte **fp) {
        struct qq_decls$varrec *  stackend;
        struct qq_decls$varrec *  oldsptr;
    stackend = &qq_decls$varstack[((i64)1)-1];
    oldsptr = (*sp);
    L1165 :;
    while (1) {
        if (((*sp) <= stackend)) {
            qq_decls$sptr = oldsptr;
            qq_runaux$pcerror((byte*)"DEFAULT EXCEPTION",(byte*)"");
        }
;
        if ((((i64)(*(*sp)).tag == (i64)21) && ((exceptno == (i64)0) || ((i64)(*(*sp)).exceptiontype == exceptno)))) {
            goto L1166 ;
        }
;
        if (!!((i64)(*(*sp)).hasref)) {
            qq_vars$var_unshareu((*sp));
        }
;
        --((*sp));
    }
L1166 :;
    ;
    (*fp) = ((byte *)(*sp) + (i64)(*(*sp)).frameoffset);
    return (struct qq_pcltabs$pclrec *)(*qq_decls$sptr).ptr;
}

i64 qq_runaux$runproc_m(void *amsg) {
        struct qq_decls$varrec a;
        struct qq_decls$varrec dest;
        static i64 rmsg_typeno;
        i64 i;
        i64 result;
        struct qq_decls$objrec obj;
    if ((rmsg_typeno == (i64)0)) {
        for (i=(i64)1;i<=qq_tables$ntypes;++i) {
L1167 :;
            if (!!(mlib$eqstring(qq_tables$ttname[(i)],(byte*)"ws_msg64"))) {
                rmsg_typeno = i;
                goto L1169 ;
            }
;
L1168 :;
        }
L1169 :;
        ;
    }
;
    if ((rmsg_typeno == (i64)0)) {
        mlib$abortprogram((byte*)"mainwndproc: can't find rmsg");
    }
;
    memset(&obj,(i32)(i64)0,(u64)32u);
    obj.refcount = (i64)99;
    obj.ptr = (byte *)amsg;
    obj.usertag = rmsg_typeno;
    a.tagx = (i64)269;
    a.objptr = &obj;
    qq_runaux$runproc(qq_decls$pcl_callbackfn,&a,0,&dest);
    result = dest.value;
    result = (i64)0;
    return result;
}

void qq_runaux$runproc(void *fnptr,struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *dest) {
        struct qq_decls$varrec *  oldsptr;
        byte *  oldframeptr;
        struct qq_pcltabs$pclrec *  oldpcptr;
        byte oldstopped;
        i64 nparams;
    (*dest).tagx = (i64)1;
    (*dest).value = (i64)0;
    oldstopped = qq_decls$stopped;
    oldpcptr = qq_decls$pcptr;
    oldsptr = qq_decls$sptr;
    oldframeptr = qq_decls$frameptr;
    (*++(qq_decls$sptr)).tagx = (i64)999;
    if ((!!(b) && !!((i64)(*b).tag))) {
        nparams = (i64)2;
        (*++(qq_decls$sptr)) = (*a);
        (*++(qq_decls$sptr)) = (*b);
    }
    else if ((!!(a) && !!((i64)(*a).tag))) {
        nparams = (i64)1;
        (*++(qq_decls$sptr)) = (*a);
    }
    else {
        nparams = (i64)0;
    }
;
    (*++(qq_decls$sptr)).tagx = (i64)20;
    (*qq_decls$sptr).retaddr = qq_decls$stopseq;
    (*qq_decls$sptr).frameptr_low = (i64)qq_decls$frameptr;
    qq_decls$frameptr = (byte *)qq_decls$sptr;
    qq_decls$pcptr = (struct qq_pcltabs$pclrec *)fnptr;
    qq_runlab$disploop();
    if (((i64)(*(qq_decls$sptr - (i64)11)).tag == (i64)20)) {
        (*dest) = (*qq_decls$sptr);
    }
    else {
        --(qq_decls$sptr);
        (*dest) = (*qq_decls$sptr);
        if (((i64)(*dest).tag == (i64)0)) {
            (*dest).tagx = (i64)1;
            (*dest).value = (i64)0;
        }
;
    }
;
    qq_decls$pcptr = oldpcptr;
    qq_decls$stopped = (i64)oldstopped;
    qq_decls$sptr = oldsptr;
    qq_decls$frameptr = oldframeptr;
    qq_decls$stopped = (i64)oldstopped;
}

struct qq_decls$varrec *qq_runaux$k_keyindex(struct qq_decls$varrec *sp) {
        struct qq_decls$varrec *  d;
        struct qq_decls$varrec *  k;
        struct qq_decls$varrec *  p;
        struct qq_decls$varrec *  def;
    def = (sp)--;
    k = (sp)--;
    d = sp;
    if (((i64)(*d).tag != (i64)6)) {
        qq_runaux$pcustype((byte*)"dict{}",d);
    }
;
    p = qq_dicts$var_finddictitem(d,k,(i64)0);
    if (!!((i64)(*d).hasref)) {
        qq_vars$var_unshareu(d);
    }
;
    if (!!((i64)(*k).hasref)) {
        qq_vars$var_unshareu(k);
    }
;
    if (!!(p)) {
        (*sp) = (*p);
        if (!!((i64)(*def).hasref)) {
            qq_vars$var_unshareu(def);
        }
;
    }
    else {
        (*sp) = (*def);
    }
;
    return sp;
}

struct qq_decls$varrec *qq_runaux$k_popkeyindex(struct qq_decls$varrec *sp) {
        struct qq_decls$varrec *  d;
        struct qq_decls$varrec *  k;
        struct qq_decls$varrec *  p;
        struct qq_decls$varrec *  x;
    k = (sp)--;
    d = (sp)--;
    x = (sp)--;
    if (((i64)(*d).tag != (i64)6)) {
        qq_runaux$pcustype((byte*)"dict{}:=",d);
    }
;
    p = qq_dicts$var_finddictitem(d,k,(i64)1);
    if (((i64)(*p).tag != (i64)0)) {
        if (!!((i64)(*p).hasref)) {
            qq_vars$var_unshareu(p);
        }
;
    }
;
    (*p) = (*x);
    if (!!((i64)(*d).hasref)) {
        qq_vars$var_unshareu(d);
    }
;
    if (!!((i64)(*k).hasref)) {
        qq_vars$var_unshareu(k);
    }
;
    return sp;
}

struct qq_decls$varrec *qq_runaux$k_keyindexref(struct qq_decls$varrec *sp) {
        struct qq_decls$varrec *  d;
        struct qq_decls$varrec *  k;
        struct qq_decls$varrec *  p;
    k = (sp)--;
    d = sp;
    if (((i64)(*d).tag != (i64)6)) {
        qq_runaux$pcustype((byte*)"&dict{}",d);
    }
;
    p = qq_dicts$var_finddictitem(d,k,(i64)0);
    if ((p == 0)) {
        qq_runaux$pcerror((byte*)"&dict{} not found",(byte*)"");
    }
;
    if (!!((i64)(*p).hasref)) {
        ++((*(*p).objptr).refcount);
    }
;
    if (!!((i64)(*k).hasref)) {
        qq_vars$var_unshareu(k);
    }
;
    if (!!((i64)(*d).hasref)) {
        qq_vars$var_unshareu(d);
    }
;
    (*sp).tagx = (i64)14;
    (*sp).varptr = p;
    return sp;
}

struct qq_pcltabs$pclrec *qq_runaux$k_map(struct qq_decls$varrec *sp,struct qq_pcltabs$pclrec *pc,struct qq_decls$varrec **newsp) {
        static struct qq_pcltabs$pclrec codeseq[10];
        {i64 $temp = (i64)(*sp).tag;
if (($temp==(i64)19)) {
        if ((qq_runlab$jumptable == 0)) {
            codeseq[((i64)1)-1].opcode = (*sp).value;
        }
        else {
            codeseq[((i64)1)-1].labaddr = (*qq_runlab$jumptable)[((*sp).value)];
        }
;
        --(sp);
        codeseq[((i64)2)-1] = (*(pc + (i64)1));
    }
    else {
        qq_runaux$pcerror((byte*)"Apply:no op",(byte*)"");
    }
    };
    (*newsp) = sp;
    return &codeseq[((i64)1)-1];
}

// START
void qq_runaux$start(void) {

}

void qq_sets$obj_free_set(struct qq_decls$objrec *p) {
    if (!!((*p).length)) {
        mlib$pcm_free((*p).ptr,qq_bits$getbitssize((*p).alloc64,(i64)33));
    }
;
    mlib$pcm_free32(p);
}

void qq_sets$var_dupl_set(struct qq_decls$varrec *a) {
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
        i64 nbits;
    p = (*a).objptr;
    nbits = (*p).length;
    q = qq_sets$obj_newset(nbits);
    if (!!(nbits)) {
        memcpy((*q).ptr,(*p).ptr,(u64)qq_bits$getbitssize(nbits,(i64)33));
    }
;
    (*a).objptr = q;
}

i64 qq_sets$var_equal_set(struct qq_decls$varrec *x,struct qq_decls$varrec *y) {
        i64 xbytes;
        i64 ybytes;
    xbytes = qq_sets$getsetbytes(x);
    ybytes = qq_sets$getsetbytes(y);
    if ((xbytes != ybytes)) {
        return (i64)0;
    }
;
    return mlib$eqbytes((*(*x).objptr).ptr,(*(*y).objptr).ptr,xbytes);
}

static i64 qq_sets$getsetbytes(struct qq_decls$varrec *x) {
        i64 nbits;
    nbits = (*(*x).objptr).length;
    if (!!(nbits)) {
        if (!!((nbits & (i64)7))) {
            return ((nbits / (i64)8) + (i64)1);
        }
        else {
            return (nbits / (i64)8);
        }
;
    }
    else {
        return (i64)0;
    }
;
}

void qq_sets$var_make_set(struct qq_decls$varrec *data,struct qq_decls$varrec *dest,i64 n) {
        struct qq_decls$varrec *  q;
        i64 top;
        i64 a;
        i64 b;
        struct qq_decls$objrec *  s;
        static i64 count = (i64)0;
        i64 $av_1;
        i64 $av_2;
    if ((n == (i64)0)) {
        qq_sets$var_emptyset(dest);
        return;
    }
;
    top = (i64)0;
    q = data;
    $av_1 = n;
    while ($av_1-- > 0) {
L1170 :;
        switch ((i64)(*q).tag) {
        case 4:;
            {
                a = msysc$m_getdotslice((*q).dummy,(i64)16,(i64)63);
                b = (i64)(*q).range_upper;
            }
            break;
        case 1:;
            {
                a = (*q).value;
                if ((a < (i64)0)) {
                    a = (-(a) - (i64)1);
                    if ((a > top)) {
                        top = a;
                    }
;
                    goto L1171 ;
                }
;
                b = a;
            }
            break;
        default: {
            b = (a = qq_vars$var_getintvalue(q));
        }
        } //SW
;
        if (((a < (i64)0) || (b < (i64)0))) {
            qq_runaux$pcerror((byte*)"Neg range element",(byte*)"");
        }
;
        top=(top>a?top:a);
;
        top=(top>b?top:b);
;
        ++(q);
L1171 :;
    }
L1172 :;
    ;
    s = qq_sets$obj_newset((top + (i64)1));
    q = data;
    $av_2 = n;
    while ($av_2-- > 0) {
L1173 :;
        switch ((i64)(*q).tag) {
        case 4:;
            {
                a = msysc$m_getdotslice((*q).dummy,(i64)16,(i64)63);
                b = (i64)(*q).range_upper;
                if ((a > b)) {
                    {i64 temp = a; a = b; b = temp; };
                }
;
            }
            break;
        case 1:;
            {
                b = (a = (*q).value);
                if ((a < (i64)0)) {
                    goto L1174 ;
                }
;
            }
            break;
        default: {
            b = (a = qq_vars$var_getintvalue(q));
        }
        } //SW
;
        qq_lib$setelemblock((byte (*)[])(*s).ptr,a,b);
        ++(q);
L1174 :;
    }
L1175 :;
    ;
    qq_vars$var_objtovar((i64)5,s,dest);
}

struct qq_decls$objrec *qq_sets$obj_newset(i64 length) {
        struct qq_decls$objrec *  p;
        i64 nbytes;
    p = qq_vars$obj_new();
    (*p).flags = msysc$m_setdotindex((*p).flags,(i64)1,(u64)1u);
    (*p).length = length;
    nbytes = ((((length - (i64)1) / (i64)64) + (i64)1) * (i64)8);
    if (!!(length)) {
        (*p).ptr = (byte *)mlib$pcm_alloc(nbytes);
        (*p).alloc64 = ((i64)(u64)mlib$allocbytes * (i64)8);
        mlib$pcm_clearmem((*p).ptr,mlib$allocbytes);
    }
    else {
        (*p).ptr = 0;
    }
;
    return p;
}

void qq_sets$var_emptyset(struct qq_decls$varrec *dest) {
    qq_vars$var_objtovar((i64)5,qq_sets$obj_newset((i64)0),dest);
}

void qq_sets$var_getix_set(struct qq_decls$varrec *a,i64 index) {
        struct qq_decls$objrec *  p;
    p = (*a).objptr;
    if (((u64)index >= (u64)(*p).length)) {
        qq_runaux$pcerror((byte*)"set[int] bounds",(byte*)"");
    }
;
    (*a).tagx = (i64)1;
    (*a).value = (i64)!!(((i64)(*((*p).ptr + (index >> (i64)3))) & ((i64)1 << (index & (i64)7))));
}

void qq_sets$var_putix_set(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *x) {
        struct qq_decls$objrec *  p;
        byte *  q;
        i64 newoffset;
    p = (*a).objptr;
    if (!(!!(msysc$m_getdotindex((i64)(*p).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    if (((u64)index >= (u64)(*p).length)) {
        if ((index < (i64)0)) {
            qq_runaux$pcerror((byte*)"lwb",(byte*)"");
        }
        else {
            qq_runaux$pcerror((byte*)"set[i]:=x bounds",(byte*)"");
        }
;
    }
;
    q = qq_sets$getoffset((*p).ptr,index,&newoffset);
    qq_vars$var_storebit(q,newoffset,x,(i64)33,(i64)0);
}

void qq_sets$var_getixref_set(struct qq_decls$varrec *a,i64 index) {
        struct qq_decls$objrec *  p;
        byte *  q;
        i64 newoffset;
    p = (*a).objptr;
    if (!(!!(msysc$m_getdotindex((i64)(*p).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    if (((u64)index >= (u64)(*p).length)) {
        qq_runaux$pcerror((byte*)"&set[i] bounds",(byte*)"");
    }
;
    q = qq_sets$getoffset((*p).ptr,index,&newoffset);
    (*a).tagx = (i64)15;
    (*a).elemtag = (i64)33;
    (*a).ptr = q;
    (*a).bitoffset = newoffset;
}

static byte *qq_sets$getoffset(byte *p,i64 index,i64 *newoffset) {
    p += (index >> (i64)3);
    (*newoffset) = (index & (i64)7);
    return p;
}

i64 qq_sets$var_in_set(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        i64 i;
        static byte masks[8] = {(u8)1u,(u8)2u,(u8)4u,(u8)8u,(u8)16u,(u8)32u,(u8)64u,(u8)128u};
        struct qq_decls$objrec *  p;
    i = (*a).value;
    p = (*b).objptr;
    if (((u64)i >= (u64)(*p).length)) {
        return (i64)0;
    }
;
    if (!!(((i64)(*((*p).ptr + (i >> (i64)3))) & (i64)masks[((i & (i64)7))]))) {
        return (i64)1;
    }
    else {
        return (i64)0;
    }
;
}

void qq_sets$iresizeset(struct qq_decls$varrec *p,i64 n) {
        struct qq_decls$objrec *  pp;
    pp = (*p).objptr;
    if (((*pp).length >= n)) {
        return;
    }
;
    qq_sets$obj_resize_set(pp,n);
}

void qq_sets$obj_resize_set(struct qq_decls$objrec *p,i64 n) {
        byte *  q;
        i64 newsize;
        i64 elemtype;
    elemtype = (i64)(*p).elemtag;
    if ((n <= (*p).alloc64)) {
        (*p).length = n;
    }
    else {
        newsize = qq_bits$getbitssize(n,(i64)33);
        q = (byte *)mlib$pcm_allocz(newsize);
        if (!!((*p).length)) {
            memcpy(q,(*p).ptr,(u64)qq_bits$getbitssize((*p).length,(i64)33));
            mlib$pcm_free((*p).ptr,qq_bits$getbitssize((*p).alloc64,(i64)33));
        }
;
        (*p).ptr = q;
        (*p).length = n;
        (*p).alloc64 = (mlib$allocbytes * (i64)8);
    }
;
}

void qq_sets$iorsetbits(i64 *p,i64 *q,i64 n) {
        i64 $av_1;
    $av_1 = (((n - (i64)1) / (i64)64) + (i64)1);
    while ($av_1-- > 0) {
L1176 :;
        (*(p)++) |= (*(q)++);
L1177 :;
    }
L1178 :;
    ;
}

void qq_sets$ixorsetbits(i64 *p,i64 *q,i64 n) {
        i64 $av_1;
    $av_1 = (((n - (i64)1) / (i64)64) + (i64)1);
    while ($av_1-- > 0) {
L1179 :;
        (*(p)++) ^= (*(q)++);
L1180 :;
    }
L1181 :;
    ;
}

void qq_sets$iandsetbits(u64 *p,u64 *q,i64 n) {
        i64 $av_1;
    $av_1 = (((n - (i64)1) / (i64)64) + (i64)1);
    while ($av_1-- > 0) {
L1182 :;
        (*(p)++) &= (*(q)++);
L1183 :;
    }
L1184 :;
    ;
}

void qq_sets$inotsetbits(u64 *p,i64 n) {
        i64 $av_1;
    $av_1 = (((n - (i64)1) / (i64)64) + (i64)1);
    while ($av_1-- > 0) {
L1185 :;
        (*p) = ~((*p));
        ++(p);
L1186 :;
    }
L1187 :;
    ;
}

void qq_sets$var_iorto_set(struct qq_decls$varrec *x,struct qq_decls$varrec *y) {
        i64 xlen;
        i64 ylen;
        struct qq_decls$objrec *  px;
        struct qq_decls$objrec *  py;
    px = (*x).objptr;
    py = (*y).objptr;
    xlen = (*px).length;
    ylen = (*py).length;
    if ((ylen == (i64)0)) {
    }
    else if ((xlen == (i64)0)) {
        (*x) = (*y);
        qq_sets$var_dupl_set(x);
    }
    else {
        px = (*x).objptr;
        qq_sets$iresizeset(x,ylen);
        qq_sets$iorsetbits((i64 *)(*px).ptr,(i64 *)(*py).ptr,ylen);
    }
;
}

void qq_sets$var_iandto_set(struct qq_decls$varrec *x,struct qq_decls$varrec *y) {
        i64 xlen;
        i64 ylen;
        struct qq_decls$objrec *  px;
        struct qq_decls$objrec *  py;
    px = (*x).objptr;
    py = (*y).objptr;
    xlen = (*px).length;
    ylen = (*py).length;
    if ((ylen == (i64)0)) {
        qq_sets$var_emptyset(x);
    }
    else if ((xlen == (i64)0)) {
    }
    else {
        px = (*x).objptr;
        qq_sets$iresizeset(x,ylen);
        qq_sets$iandsetbits((u64 *)(*px).ptr,(u64 *)(*py).ptr,ylen);
    }
;
}

void qq_sets$var_ixorto_set(struct qq_decls$varrec *x,struct qq_decls$varrec *y) {
        i64 xlen;
        i64 ylen;
        struct qq_decls$objrec *  px;
        struct qq_decls$objrec *  py;
    px = (*x).objptr;
    py = (*y).objptr;
    xlen = (*px).length;
    ylen = (*py).length;
    if ((ylen == (i64)0)) {
        qq_sets$var_emptyset(x);
    }
    else if ((xlen == (i64)0)) {
        (*x) = (*y);
        qq_sets$var_dupl_set(x);
    }
    else {
        px = (*x).objptr;
        qq_sets$iresizeset(x,ylen);
        qq_sets$ixorsetbits((i64 *)(*px).ptr,(i64 *)(*py).ptr,ylen);
    }
;
}

void qq_sets$var_inotto_set(struct qq_decls$varrec *x) {
        i64 xlen;
        struct qq_decls$objrec *  px;
    px = (*x).objptr;
    xlen = (*px).length;
    if (!!(xlen)) {
        qq_sets$inotsetbits((u64 *)(*px).ptr,xlen);
    }
;
}

// START
void qq_sets$start(void) {

}

// START
void qq_strings$start(void) {

    qq_strings$emptystring = qq_vars$obj_new();
    (*qq_strings$emptystring).refcount = (i64)1;
    (*qq_strings$emptystring).objtype = (i64)0;
}

void qq_strings$var_empty_string(struct qq_decls$varrec *dest,i64 mutable) {
    (*dest).tagx = (i64)265;
    if (!(!!(mutable))) {
        (*dest).objptr = qq_strings$emptystring;
        ++((*qq_strings$emptystring).refcount);
    }
    else {
        (*dest).objptr = qq_strings$obj_make_stringn(0,(i64)0,(i64)1);
    }
;
}

void qq_strings$var_make_string(u8 *s,struct qq_decls$varrec *dest,i64 mutable) {
    (*dest).tagx = (i64)265;
    (*dest).objptr = qq_strings$obj_make_string(s,mutable);
}

void qq_strings$var_make_stringn(u8 *s,i64 length,struct qq_decls$varrec *dest,i64 mutable) {
    (*dest).tagx = (i64)265;
    (*dest).objptr = qq_strings$obj_make_stringn(s,length,mutable);
}

struct qq_decls$objrec *qq_strings$obj_new_string(i64 n) {
        struct qq_decls$objrec *  p;
    p = qq_vars$obj_new();
    (*p).flags = msysc$m_setdotindex((*p).flags,(i64)1,(u64)1u);
    (*p).length = n;
    (*p).objtype = (i64)0;
    if (!!(n)) {
        (*p).strptr = (u8 *)mlib$pcm_alloc(n);
        (*p).alloc64 = mlib$allocbytes;
    }
;
    return p;
}

struct qq_decls$objrec *qq_strings$obj_make_string(u8 *s,i64 mutable) {
        struct qq_decls$objrec *  p;
        i64 n;
    p = qq_strings$obj_new_string((n = strlen(s)));
    (*p).flags = msysc$m_setdotindex((*p).flags,(i64)1,(u64)mutable);
    if (!!(n)) {
        memcpy((void *)(*p).strptr,(void *)s,(u64)n);
    }
;
    return p;
}

struct qq_decls$objrec *qq_strings$obj_make_stringn(u8 *s,i64 length,i64 mutable) {
        struct qq_decls$objrec *  p;
    p = qq_strings$obj_new_string(length);
    (*p).flags = msysc$m_setdotindex((*p).flags,(i64)1,(u64)mutable);
    if (!!(length)) {
        if (!!(s)) {
            memcpy((void *)(*p).strptr,(void *)s,(u64)length);
        }
        else {
            memset((void *)(*p).strptr,(i32)(i64)0,(u64)length);
        }
;
    }
;
    return p;
}

void qq_strings$obj_free_string(struct qq_decls$objrec *p) {
    if (!!((*p).length)) {
        mlib$pcm_free((void *)(*p).strptr,(*p).alloc64);
    }
;
    mlib$pcm_free32(p);
}

void qq_strings$var_dupl_string(struct qq_decls$varrec *a) {
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
    p = (*a).objptr;
    q = qq_strings$obj_new_string((*p).length);
    (*a).objptr = q;
    if (!!((*q).length)) {
        memcpy((void *)(*q).strptr,(void *)(*p).strptr,(u64)(*q).length);
    }
;
}

void qq_strings$var_getix_string(struct qq_decls$varrec *a,i64 index) {
        struct qq_decls$objrec *  q;
    q = (*a).objptr;
    if (((u64)(index - (i64)1) >= (u64)(*q).length)) {
        qq_runaux$pcerror((byte*)"getstring[int] bounds",(byte*)"");
    }
;
    qq_strings$stringslice(a,index,index,a);
}

void qq_strings$var_getixref_string(struct qq_decls$varrec *a,i64 index) {
        struct qq_decls$objrec *  q;
    q = (*a).objptr;
    if (((u64)(index - (i64)1) >= (u64)(*q).length)) {
        qq_runaux$pcerror((byte*)"getixref[int] bounds",(byte*)"");
    }
;
    (*a).tagx = (i64)16;
    (*a).elemtag = (i64)27;
    (*a).ptr = (byte *)(((*q).strptr + index) - (i64)1);
}

void qq_strings$var_getdotix_string(struct qq_decls$varrec *a,i64 index) {
        struct qq_decls$objrec *  q;
    q = (*a).objptr;
    if (((u64)(index - (i64)1) >= (u64)(*q).length)) {
        qq_runaux$pcerror((byte*)"x.[] bounds",(byte*)"");
    }
;
    (*a).tagx = (i64)1;
    (*a).value = (i64)(u64)(*(((*q).strptr + index) - (i64)1));
}

void qq_strings$var_getdotixref_string(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *dest) {
        struct qq_decls$objrec *  q;
    q = (*a).objptr;
    --(index);
    if (((u64)index >= (u64)(*q).length)) {
        qq_runaux$pcerror((byte*)"x.[] bounds",(byte*)"");
    }
;
    (*dest).tagx = (i64)16;
    (*dest).elemtag = (i64)27;
    (*dest).ptr = (byte *)((*q).strptr + index);
}

void qq_strings$var_getslice_string(struct qq_decls$varrec *a,i64 i,i64 j) {
        struct qq_decls$objrec *  p;
    p = (*a).objptr;
    if ((((i < (i64)1) || (j > (*p).length)) || (i > j))) {
        qq_runaux$pcerror((byte*)"string/slice bounds",(byte*)"");
    }
;
    qq_strings$stringslice(a,i,j,a);
}

static void qq_strings$stringslice(struct qq_decls$varrec *a,i64 i,i64 j,struct qq_decls$varrec *dest) {
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
    p = (*a).objptr;
    q = qq_vars$obj_new();
    (*q).flags = msysc$m_setdotindex((*q).flags,(i64)1,msysc$m_getdotindex((i64)(*p).flags,(i64)1));
    (*q).length = ((j - i) + (i64)1);
    (*q).objtype = (i64)1;
        {i64 $temp = (i64)(*p).objtype;
if (($temp==(i64)1)) {
        (*q).objptr2 = (*p).objptr2;
        ++((*(*q).objptr2).refcount);
    }
    else if (($temp==(i64)2)) {
        (*q).objptr2 = 0;
        (*q).objtype = (i64)2;
    }
    else {
        ++((*p).refcount);
        (*q).objptr2 = p;
    }
    };
    (*q).strptr = (((*p).strptr + i) - (i64)1);
    (*dest).tagx = (i64)(*a).tagx;
    (*dest).objptr = q;
}

void qq_strings$var_putix_string(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *x) {
        u8 *  s;
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
        i64 length;
    p = (*a).objptr;
    if (!(!!(msysc$m_getdotindex((i64)(*p).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    length = (*p).length;
    if (!((($rtemp=index, $rtemp >= (i64)1 && $rtemp <= length)))) {
        if ((index == (length + (i64)1))) {
            qq_strings$var_addto_string(a,x);
            return;
        }
        else {
            qq_runaux$pcerror((byte*)"putstring[int] bounds",(byte*)"");
        }
;
    }
;
    s = (((*p).strptr + index) - (i64)1);
    if (((i64)(*x).tag != (i64)9)) {
        qq_runaux$pcerror((byte*)"s[i]:= not str",(byte*)"");
    }
;
    q = (*x).objptr;
    if (((*q).length == (i64)0)) {
        qq_runaux$pcerror((byte*)"s[i]:=\"\"",(byte*)"");
    }
;
    (*s) = (u64)(*(*q).strptr);
}

void qq_strings$var_putslice_string(struct qq_decls$varrec *a,i64 i,i64 j,struct qq_decls$varrec *x) {
        u8 *  s;
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
        i64 length;
        i64 sublength;
    p = (*a).objptr;
    if (!(!!(msysc$m_getdotindex((i64)(*p).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    length = (*p).length;
    if ((((i < (i64)1) || (j > (*p).length)) || (i > j))) {
        qq_runaux$pcerror((byte*)"string/slice bounds",(byte*)"");
    }
;
    sublength = ((j - i) + (i64)1);
    s = (((*p).strptr + i) - (i64)1);
    if (((i64)(*x).tag != (i64)9)) {
        qq_runaux$pcerror((byte*)"s[i..j]:= not str",(byte*)"");
    }
;
    q = (*x).objptr;
    if (((*q).length < sublength)) {
        qq_runaux$pcerror((byte*)"substr too short",(byte*)"");
    }
;
    memcpy((void *)s,(void *)(*q).strptr,(u64)sublength);
}

void qq_strings$var_putdotix_string(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *x) {
        struct qq_decls$objrec *  p;
        i64 length;
        i64 ch;
    if (((i64)(*x).tag != (i64)1)) {
        qq_runaux$pcerror((byte*)"s.[i]:= not int",(byte*)"");
    }
;
    ch = (*x).value;
    p = (*a).objptr;
    if (!(!!(msysc$m_getdotindex((i64)(*p).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    length = (*p).length;
    if (!((($rtemp=index, $rtemp >= (i64)1 && $rtemp <= length)))) {
        if ((index == (length + (i64)1))) {
            qq_strings$var_addto_string_ch(a,ch);
            return;
        }
        else {
            qq_runaux$pcerror((byte*)"str.[int] bounds",(byte*)"");
        }
;
    }
;
    (*(((*p).strptr + index) - (i64)1)) = (u64)ch;
}

void qq_strings$obj_resize_string(struct qq_decls$objrec *p,i64 n) {
        u8 *  s;
        i64 oldalloc;
    if ((n <= (*p).alloc64)) {
        (*p).length = n;
    }
    else {
        oldalloc = (*p).alloc64;
        s = (u8 *)mlib$pcm_alloc(n);
        (*p).alloc64 = mlib$allocbytes;
        if (!!((*p).length)) {
            memcpy((void *)s,(void *)(*p).strptr,(u64)(*p).length);
            mlib$pcm_free((void *)(*p).strptr,oldalloc);
        }
;
        (*p).strptr = s;
        (*p).length = n;
    }
;
}

void qq_strings$var_add_string(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
        struct qq_decls$objrec *  r;
        i64 alen;
        i64 blen;
        i64 newlen;
    p = (*a).objptr;
    q = (*b).objptr;
    alen = (*p).length;
    blen = (*q).length;
    if ((blen == (i64)0)) {
        ++((*(*a).objptr).refcount);
        return;
    }
    else if ((alen == (i64)0)) {
        qq_strings$var_make_stringn((*q).strptr,blen,a,(i64)1);
        return;
    }
;
    newlen = (alen + blen);
    r = qq_strings$obj_new_string(newlen);
    memcpy((void *)(*r).strptr,(void *)(*p).strptr,(u64)alen);
    memcpy((void *)((*r).strptr + alen),(void *)(*q).strptr,(u64)blen);
    (*a).objptr = r;
}

void qq_strings$var_addto_string(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        struct qq_decls$objrec *  p;
        struct qq_decls$objrec *  q;
        i64 alen;
        i64 blen;
        i64 newlen;
    p = (*a).objptr;
    q = (*b).objptr;
    alen = (*p).length;
    blen = (*q).length;
    if (!(!!(msysc$m_getdotindex((i64)(*p).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    if ((blen == (i64)0)) {
        return;
    }
    else if ((alen == (i64)0)) {
        qq_vars$var_unshareu(a);
        (*a) = (*b);
        qq_vars$var_duplu(a);
        return;
    }
;
    newlen = (alen + blen);
    qq_strings$obj_resize_string(p,newlen);
    memcpy((void *)((*p).strptr + alen),(void *)(*q).strptr,(u64)blen);
}

void qq_strings$var_addto_string_ch(struct qq_decls$varrec *a,i64 ch) {
        struct qq_decls$objrec *  p;
        i64 alen;
    p = (*a).objptr;
    alen = (*p).length;
    if (!(!!(msysc$m_getdotindex((i64)(*p).flags,(i64)1)))) {
        qq_lib$pcnotmut();
    }
;
    qq_strings$obj_resize_string(p,(alen + (i64)1));
    (*((*p).strptr + alen)) = (u64)ch;
}

i64 qq_strings$var_equal_string(struct qq_decls$varrec *x,struct qq_decls$varrec *y) {
        i64 n;
        struct qq_decls$objrec *  px;
        struct qq_decls$objrec *  py;
    px = (*x).objptr;
    py = (*y).objptr;
    if ((px == py)) {
        return (i64)1;
    }
;
    n = (*px).length;
    if ((n != (*py).length)) {
        return (i64)0;
    }
    else if ((n == (i64)0)) {
        return (i64)1;
    }
    else {
        return mlib$eqbytes((void *)(*px).strptr,(void *)(*py).strptr,n);
    }
;
}

i64 qq_strings$var_compare_string(struct qq_decls$varrec *x,struct qq_decls$varrec *y) {
        i64 res;
        struct qq_decls$objrec *  px;
        struct qq_decls$objrec *  py;
    px = (*x).objptr;
    py = (*y).objptr;
    res = qq_strings$cmpstring_len((*px).strptr,(*py).strptr,(*px).length,(*py).length);
    return res;
}

static i64 qq_strings$cmpstring_len(u8 *s,u8 *t,i64 slen,i64 tlen) {
    if ((slen == (i64)0)) {
        if ((tlen == (i64)0)) {
            return (i64)0;
        }
        else {
            return (i64)-1;
        }
;
    }
    else if ((tlen == (i64)0)) {
        return (i64)1;
    }
    else {
        if ((slen == tlen)) {
            if ((slen == (i64)1)) {
                if (((u64)(*s) < (u64)(*t))) {
                    return (i64)-1;
                }
                else if (((u64)(*s) > (u64)(*t))) {
                    return (i64)1;
                }
                else {
                    return (i64)0;
                }
;
            }
;
            return mlib$cmpstringn(s,t,slen);
        }
        else {
            return mlib$cmpstring(qq_lib$convtostringz(s,slen),qq_lib$convtostringz(t,tlen));
        }
;
    }
;
}

i64 qq_strings$var_inx_string(struct qq_decls$varrec *x,struct qq_decls$varrec *y) {
        i64 xlen;
        i64 ylen;
        i64 i;
        i64 j;
        i64 k;
        u8 *  sx;
        u8 *  sy;
        struct qq_decls$objrec *  px;
        struct qq_decls$objrec *  py;
    px = (*x).objptr;
    py = (*y).objptr;
    xlen = (*px).length;
    ylen = (*py).length;
    if (((xlen == (i64)0) || (ylen == (i64)0))) {
        return (i64)(-9223372036854775807-1);
    }
;
    k = (ylen - xlen);
    for (i=(i64)0;i<=k;++i) {
L1188 :;
        sx = (*px).strptr;
        sy = ((*py).strptr + i);
        for (j=(i64)1;j<=xlen;++j) {
L1191 :;
            if (((u64)(*sx) != (u64)(*sy))) {
                goto L1194 ;
;
            }
;
            ++(sx);
            ++(sy);
L1192 :;
        }
L1193 :;
        ;
        return (i + (i64)1);
        //nextpos:
L1194 :;
;
L1189 :;
    }
L1190 :;
    ;
    return (i64)(-9223372036854775807-1);
}

void qq_strings$var_iconvcase(struct qq_decls$varrec *a,struct qq_decls$varrec *b,i64 upper) {
        i64 n;
        u8 *  s;
        struct qq_decls$objrec *  pa;
        i64 $av_1;
        i64 $av_2;
    pa = (*a).objptr;
    if (((i64)(*b).tag > (i64)0)) {
        n = qq_vars$var_getintvalue(b);
    }
    else {
        n = (*pa).length;
    }
;
    if (((i64)(*a).tag != (i64)9)) {
        qq_runaux$pcerror((byte*)"convcase/notstr",(byte*)"");
    }
;
    if ((n < (i64)0)) {
        qq_runaux$pcerror((byte*)"CONVCASE N<0",(byte*)"");
    }
;
    if ((n == (i64)0)) {
        return;
    }
;
    if ((n > (*pa).length)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"N=",NULL);
        msysc$m_print_i64(n,NULL);
        msysc$m_print_i64((*pa).length,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        qq_runaux$pcerror((byte*)"convcase/N?",(byte*)"");
    }
;
    s = (*pa).strptr;
    if (!!(upper)) {
        $av_1 = n;
        while ($av_1-- > 0) {
L1195 :;
            (*s) = (u64)toupper((i32)(u64)(*s));
            ++(s);
L1196 :;
        }
L1197 :;
        ;
    }
    else {
        $av_2 = n;
        while ($av_2-- > 0) {
L1198 :;
            (*s) = (u64)tolower((i32)(u64)(*s));
            ++(s);
L1199 :;
        }
L1200 :;
        ;
    }
;
}

void qq_strings$var_makestrslicexobj(u8 *s,i64 length,struct qq_decls$varrec *dest) {
    (*dest).tagx = (i64)265;
    (*dest).objptr = qq_strings$obj_make_strslicexobj(s,length);
}

struct qq_decls$objrec *qq_strings$obj_make_strslicexobj(u8 *s,i64 length) {
        struct qq_decls$objrec *  p;
    if ((length == (i64)0)) {
        s = 0;
    }
;
    p = qq_vars$obj_new();
    (*p).strptr = s;
    (*p).flags = msysc$m_setdotindex((*p).flags,(i64)1,(u64)1u);
    (*p).length = length;
    (*p).objtype = (i64)2;
    return p;
}

static i64 qq_strings$var_asc(struct qq_decls$varrec *a) {
        struct qq_decls$objrec *  p;
    if (((i64)(*a).tag != (i64)9)) {
        qq_runaux$pcerror((byte*)"Asc:not str",(byte*)"");
    }
;
    p = (*a).objptr;
    if (((*p).length < (i64)1)) {
        qq_runaux$pcerror((byte*)"Asc:empty",(byte*)"");
    }
;
    return (i64)(u64)(*(*p).strptr);
}

void qq_strings$var_new_string(struct qq_decls$varrec *a,struct qq_decls$varrec *b,struct qq_decls$varrec *dest) {
        i64 length;
        i64 ch;
    length = qq_vars$var_getintvalue(a);
    if ((length < (i64)0)) {
        qq_runaux$pcerror((byte*)"Length<0",(byte*)"");
    }
;
    qq_strings$var_make_stringn(0,length,dest,(i64)0);
        {i64 $temp = (i64)(*b).tag;
if (($temp==(i64)1)) {
        ch = (*b).value;
    }
    else if (($temp==(i64)9)) {
        ch = qq_strings$var_asc(b);
    }
    else if (($temp==(i64)0)) {
        ch = (i64)32;
    }
    else {
        qq_runaux$pcerror((byte*)"Not int/str",(byte*)"");
    }
    };
    if (!!(length)) {
        memset((void *)(*(*dest).objptr).strptr,(i32)ch,(u64)length);
    }
;
}

void qq_strings$var_new_stringn(i64 length,struct qq_decls$varrec *dest) {
    if ((length < (i64)0)) {
        qq_runaux$pcerror((byte*)"Length<0",(byte*)"");
    }
;
    qq_strings$var_make_stringn(0,length,dest,(i64)0);
}

void qq_strings$var_mul_string(struct qq_decls$varrec *a,i64 m) {
        i64 oldlen;
        i64 newlen;
        u8 *  p;
        struct qq_decls$varrec v;
        struct qq_decls$objrec *  pa;
        i64 $av_1;
    if ((m < (i64)0)) {
        qq_runaux$pcerror((byte*)"neg str mul",(byte*)"");
    }
    else if ((m == (i64)0)) {
        qq_strings$var_empty_string(a,(i64)0);
        return;
    }
    else if ((m == (i64)1)) {
        ++((*(*a).objptr).refcount);
        return;
    }
    else {
        pa = (*a).objptr;
        oldlen = (*pa).length;
        if (!!(oldlen)) {
            newlen = (oldlen * m);
            v.objptr = qq_strings$obj_new_string(newlen);
            v.tagx = (i64)265;
            p = (*v.objptr).strptr;
            if ((oldlen == (i64)1)) {
                memset((void *)p,(i32)(u64)(*(*pa).strptr),(u64)m);
            }
            else {
                $av_1 = m;
                while ($av_1-- > 0) {
L1201 :;
                    memcpy((void *)p,(void *)(*pa).strptr,(u64)oldlen);
                    p += oldlen;
L1202 :;
                }
L1203 :;
                ;
            }
;
            (*a) = v;
        }
        else {
            qq_strings$var_empty_string(a,(i64)0);
            return;
        }
;
    }
;
}

void qq_strings$var_convert_string_list(struct qq_decls$varrec *a,i64 t,struct qq_decls$varrec *dest) {
        struct qq_decls$objrec *  p;
        struct qq_decls$varrec *  q;
        i64 length;
        u8 *  s;
        i64 $av_1;
    p = (*a).objptr;
    length = (*p).length;
    qq_lists$var_make_list(0,dest,length,(i64)1);
    q = (*(*dest).objptr).varptr;
    s = (*p).strptr;
    $av_1 = length;
    while ($av_1-- > 0) {
L1204 :;
        qq_strings$var_make_stringn(s,(i64)1,q,(i64)1);
        ++(s);
        ++(q);
L1205 :;
    }
L1206 :;
    ;
}

void qq_strings$var_expand_string(struct qq_decls$varrec *a,struct qq_decls$varrec *dest,i64 m) {
        struct qq_decls$varrec *  b;
        struct qq_decls$objrec *  p;
        u8 *  s;
        i64 n;
        i64 $av_1;
    p = (*a).objptr;
    b = dest;
    s = (*p).strptr;
    n = (i64)1;
    $av_1 = m;
    while ($av_1-- > 0) {
L1207 :;
        if ((n > (*p).length)) {
            qq_strings$var_empty_string(dest,(i64)0);
        }
        else {
            qq_strings$var_make_stringn(s,(i64)1,dest,(i64)1);
            ++(s);
        }
;
        ++(n);
        --(dest);
L1208 :;
    }
L1209 :;
    ;
}

void qq_strings$var_makechar(i64 ch,struct qq_decls$varrec *dest) {
        struct qq_decls$varrec v;
        u8 str[8];
        struct qq_decls$objrec *  p;
    if (!((($rtemp=ch, $rtemp >= (i64)0 && $rtemp <= (i64)255)))) {
        qq_runaux$pcerror((byte*)"chr range",(byte*)"");
    }
;
    p = qq_decls$chrtable[(ch)];
    if ((p == 0)) {
        str[((i64)1)-1] = (u64)ch;
        str[((i64)2)-1] = (u64)0u;
        qq_strings$var_make_stringn(str,(i64)1,&v,(i64)0);
        qq_decls$chrtable[(ch)] = (p = v.objptr);
    }
;
    ++((*p).refcount);
    (*dest).tagx = (i64)265;
    (*dest).objptr = p;
}

i64 qq_syslibsdummy$loadsysmodule(struct qq_decls$filerec *pm) {
    return (i64)0;
}

// START
void qq_syslibsdummy$start(void) {

}

// START
void qq_tables$start(void) {

        i64 i;
    for (i=(i64)1;i<=(i64)27;++i) {
L1210 :;
        qq_tables$binopset[((i64)qq_tables$d_binopset[(i)-1])] = (i64)1;
        qq_tables$exprstarterset[((i64)qq_tables$d_binopset[(i)-1])] = (i64)1;
L1211 :;
    }
L1212 :;
    ;
    for (i=(i64)1;i<=(i64)7;++i) {
L1213 :;
        qq_tables$unaryopset[((i64)qq_tables$d_unaryopset[(i)-1])] = (i64)1;
        qq_tables$exprstarterset[((i64)qq_tables$d_unaryopset[(i)-1])] = (i64)1;
L1214 :;
    }
L1215 :;
    ;
    for (i=(i64)1;i<=(i64)26;++i) {
L1216 :;
        qq_tables$exprstarterset[(qq_tables$d_exprstarterset[(i)-1])] = (i64)1;
L1217 :;
    }
L1218 :;
    ;
    qq_tables$exprendset[((i64)4)] = (i64)1;
    qq_tables$exprendset[((i64)3)] = (i64)1;
    qq_tables$exprendset[((i64)12)] = (i64)1;
    qq_tables$exprendset[((i64)10)] = (i64)1;
    qq_tables$exprendset[((i64)79)] = (i64)1;
    qq_tables$exprendset[((i64)87)] = (i64)1;
    qq_tables$exprendset[((i64)85)] = (i64)1;
    for (i=(i64)1;i<=(i64)10;++i) {
L1219 :;
        qq_tables$addopset[((i64)qq_tables$d_addopset[(i)-1])] = (i64)1;
L1220 :;
    }
L1221 :;
    ;
    for (i=(i64)1;i<=(i64)7;++i) {
L1222 :;
        qq_tables$mulopset[((i64)qq_tables$d_mulopset[(i)-1])] = (i64)1;
L1223 :;
    }
L1224 :;
    ;
    for (i=(i64)1;i<=(i64)6;++i) {
L1225 :;
        qq_tables$cmpopset[((i64)qq_tables$d_cmpopset[(i)-1])] = (i64)1;
L1226 :;
    }
L1227 :;
    ;
    for (i=(i64)0;i<=(i64)40;++i) {
L1228 :;
        qq_tables$ttname[(i)] = qq_tables$stdtypenames[(i)];
        qq_tables$ttbasetype[(i)] = i;
        qq_tables$ttlower[(i)] = (i64)1;
        qq_tables$ttbitwidth[(i)] = (i64)qq_tables$stdtypewidths[(i)];
        qq_tables$ttsize[(i)] = ((i64)qq_tables$stdtypewidths[(i)] / (i64)8);
L1229 :;
    }
L1230 :;
    ;
    qq_tables$ntypes = (i64)40;
}

void qq_show$printunit(struct qq_decls$unitrec *p,i64 level,u8 *prefix,void *dev) {
        struct qq_decls$strec *  d;
        i64 flags;
        u8 *  idname;
        i64 i;
    if ((p == 0)) {
        return;
    }
;
    qq_show$currlineno = ((i64)(*p).pos & (i64)16777215);
    msysc$m_print_startfile(dev);
    msysc$m_print_ptr(p,NULL);
    msysc$m_print_str((byte*)":",NULL);
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(dev);
    msysc$m_print_str(qq_show$getprefix(level,prefix,(struct qq_decls$unitrec *)p),NULL);
    msysc$m_print_end();
    ;
    idname = qq_tables$jtagnames[((i64)(*p).tag)];
    if (((u64)(*idname) == 'j')) {
        ++(idname);
    }
;
    msysc$m_print_startfile(dev);
    msysc$m_print_str(idname,NULL);
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)": ",NULL);
    msysc$m_print_end();
    ;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)39)) {
        d = (*p).def;
        if (!!((*d).owner)) {
            msysc$m_print_startfile(dev);
            msysc$m_print_str((*(*d).owner).name,NULL);
            msysc$m_print_nogap();
            msysc$m_print_str((byte*)".",NULL);
            msysc$m_print_end();
            ;
        }
;
        msysc$m_print_startfile(dev);
        msysc$m_print_str((*d).name,NULL);
        msysc$m_print_str(qq_tables$namenames[((i64)(*d).nameid)],NULL);
        msysc$m_print_str((byte*)"Module:",NULL);
        msysc$m_print_i64(msysc$m_getdotslice((i64)(*p).pos,(i64)24,(i64)31),NULL);
        msysc$m_print_end();
        ;
        if ((!!((*d).truename) && ((i64)(*d).nameid == (i64)7))) {
            msysc$m_print_startfile(dev);
            msysc$m_print_str((byte*)" ",NULL);
            msysc$m_print_str((*d).truename,NULL);
            msysc$m_print_end();
            ;
        }
;
    }
    else if (($temp==(i64)41)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_i64((*p).value,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)42)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_r64((*p).xvalue,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)43)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_setfmt((byte*)"\"#\"");
        msysc$m_print_str((*p).svalue,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)44)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_str((*p).svalue,NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)"L",NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)71)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_str(qq_tables$condnames[((i64)(*p).condcode)],NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)38)) {
        for (i=(i64)1;i<=(i64)4;++i) {
L1231 :;
            if (((i64)(*p).cmpconds[(i)-1] == (i64)0)) {
                goto L1233 ;
            }
;
            msysc$m_print_startfile(dev);
            msysc$m_print_str(qq_tables$condnames[((i64)(*p).cmpconds[(i)-1])],NULL);
            msysc$m_print_space();
            msysc$m_print_end();
            ;
L1232 :;
        }
L1233 :;
        ;
    }
    else if (($temp==(i64)5)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_str(qq_pcltabs$pclnames[((i64)(*p).pclop)],NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)2) || ($temp==(i64)76) || ($temp==(i64)68) || ($temp==(i64)70) || ($temp==(i64)60) || ($temp==(i64)61) || ($temp==(i64)52) || ($temp==(i64)15) || ($temp==(i64)16) || ($temp==(i64)54) || ($temp==(i64)55) || ($temp==(i64)59)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_i64((i64)(*p).flag,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)63) || ($temp==(i64)62) || ($temp==(i64)66) || ($temp==(i64)79) || ($temp==(i64)78)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_setfmt((byte*)"<#>");
        msysc$m_print_str((qq_pcltabs$pclnames[((i64)(*p).pclop)] + (i64)1),NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)64) || ($temp==(i64)65)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_setfmt((byte*)"<#>");
        msysc$m_print_str((qq_tables$mathsnames[((i64)(*p).mathsop)-1] + (i64)3),NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)88)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_i64((i64)(*p).lower,NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)":",NULL);
        msysc$m_print_str((byte*)"P.LENGTH=",NULL);
        msysc$m_print_i64((i64)(*p).length,NULL);
        msysc$m_print_str(qq_tables$ttname[((i64)(*p).elemtype)],NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)34) || ($temp==(i64)35) || ($temp==(i64)36)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_str(qq_tables$ttname[((i64)(*p).mode)],NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)27)) {
        msysc$m_print_startfile(dev);
        msysc$m_print_str((qq_tables$hostfnnames[((*p).index)] + (i64)2),NULL);
        msysc$m_print_end();
        ;
    }
    };
    msysc$m_print_startfile(dev);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    flags = (i64)qq_tables$jflags[((i64)(*p).tag)];
    if ((flags >= (i64)1)) {
        qq_show$printunitlist(dev,(struct qq_decls$unitrec *)(*p).a,(level + (i64)1),(byte*)"1");
    }
;
    if ((flags == (i64)2)) {
        qq_show$printunitlist(dev,(struct qq_decls$unitrec *)(*p).b,(level + (i64)1),(byte*)"2");
    }
;
}

static void qq_show$printunitlist(void *dev,struct qq_decls$unitrec *p,i64 level,u8 *prefix) {
    if ((p == 0)) {
        return;
    }
;
    L1234 :;
    while (!!(p)) {
        qq_show$printunit((struct qq_decls$unitrec *)p,level,prefix,dev);
        p = (struct qq_decls$unitrec *)(*p).nextunit;
L1235 :;
    }
L1236 :;
    ;
}

static u8 *qq_show$getprefix(i64 level,u8 *prefix,struct qq_decls$unitrec *p) {
        static u8 str[1024];
        u8 indentstr[1024];
        i64 $av_1;
    indentstr[((i64)1)-1] = (u64)0u;
    if ((level > (i64)20)) {
        level = (i64)10;
    }
;
    $av_1 = level;
    while ($av_1-- > 0) {
L1237 :;
        strcat((u8 *)indentstr,(byte*)"- ");
L1238 :;
    }
L1239 :;
    ;
    strcpy((u8 *)str,qq_show$getlineinfok());
    strcat((u8 *)str,(u8 *)indentstr);
    strcat((u8 *)str,prefix);
    if (!!((u64)(*prefix))) {
        strcat((u8 *)str,(byte*)" ");
    }
;
    return (u8 *)str;
}

static u8 *qq_show$getlineinfok(void) {
        static u8 str[40];
    strcpy(str,msysc$strint(qq_show$currlineno,(byte*)"z4"));
    return (u8 *)str;
}

void qq_show$printglobalsymbols(void *f) {
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"PROC Global Symbol Table",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    qq_show$printst(f,qq_decls$stprogram,(i64)0);
}

void qq_show$printst(void *f,struct qq_decls$strec *p,i64 level) {
        struct qq_decls$strec *  q;
    qq_show$printstrec(f,p,level);
    q = (struct qq_decls$strec *)(*p).deflist;
    L1240 :;
    while ((q != 0)) {
        qq_show$printst(f,(struct qq_decls$strec *)q,(level + (i64)1));
        q = (struct qq_decls$strec *)(*q).nextdef;
L1241 :;
    }
L1242 :;
    ;
}

static void qq_show$printstrec(void *f,struct qq_decls$strec *p,i64 level) {
        struct qq_decls$strec dd;
        struct mlib$strbuffer v;
        struct mlib$strbuffer *  d;
        i64 col;
        i64 offset;
        u8 str[256];
        u8 *  s;
        i64 $av_1;
        u8 *  tab;
    d = (struct mlib$strbuffer *)&v;
    offset = (i64)0;
    $av_1 = level;
    while ($av_1-- > 0) {
L1243 :;
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"    ",NULL);
        msysc$m_print_end();
        ;
        offset += (i64)4;
        col += (i64)4;
L1244 :;
    }
L1245 :;
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_str(mlib$padstr((*p).name,((i64)22 - offset),(byte*)"-"),NULL);
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_str(mlib$padstr(qq_tables$namenames[((i64)(*p).nameid)],(i64)12,(byte*)"."),NULL);
    msysc$m_print_end();
    ;
    col = (i64)40;
    dd = (*p);
    if (!!(msysc$m_getdotindex((i64)dd.flags,(i64)2))) {
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"Imp ",NULL);
        msysc$m_print_end();
        ;
    }
    else if (!!(msysc$m_getdotslice((i64)dd.flags,(i64)0,(i64)1))) {
        msysc$m_print_startfile(f);
        msysc$m_print_str(((i64)msysc$m_getdotslice((i64)dd.flags,(i64)0,(i64)1)==1?(byte*)"Glob ":((i64)msysc$m_getdotslice((i64)dd.flags,(i64)0,(i64)1)==2?(byte*)"Exp ":(byte*)"Local ")),NULL);
        msysc$m_print_end();
        ;
    }
;
    if (!!(msysc$m_getdotindex((i64)dd.flags,(i64)5))) {
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"byref ",NULL);
        msysc$m_print_end();
        ;
    }
;
    if (!!(msysc$m_getdotindex((i64)dd.flags,(i64)7))) {
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"opt ",NULL);
        msysc$m_print_end();
        ;
    }
;
    if (!!((i64)dd.moduleno)) {
        msysc$m_print_startfile(f);
        msysc$m_print_setfmt((byte*)"Modno:#");
        msysc$m_print_i64((i64)dd.moduleno,NULL);
        msysc$m_print_end();
        ;
    }
;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"==========",NULL);
    msysc$m_print_end();
    ;
    if (!!(dd.owner)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"(#)");
        msysc$m_print_str((*dd.owner).name,NULL);
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str(mlib$padstr((u8 *)str,(i64)18,(byte*)"-"),NULL);
        msysc$m_print_end();
        ;
    }
    else {
        msysc$m_print_startfile(f);
        msysc$m_print_str(mlib$padstr((byte*)"()",(i64)18,(byte*)"-"),NULL);
        msysc$m_print_end();
        ;
    }
;
        {i64 $temp = (i64)dd.nameid;
if (($temp==(i64)11) || ($temp==(i64)14) || ($temp==(i64)15) || ($temp==(i64)19)) {
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)" Ix:",NULL);
        msysc$m_print_i64((i64)dd.index,NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)" ",NULL);
        msysc$m_print_end();
        ;
        if ((((i64)dd.nameid == (i64)11) && !!(dd.atfield))) {
            msysc$m_print_startfile(f);
            msysc$m_print_str((byte*)"@",NULL);
            msysc$m_print_str((*dd.atfield).name,NULL);
            msysc$m_print_space();
            msysc$m_print_end();
            ;
        }
;
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)" Offset:",NULL);
        msysc$m_print_i64((i64)dd.fieldoffset,NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)" ",NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)12)) {
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)" Offset:",NULL);
        msysc$m_print_i64((i64)dd.fieldoffset,NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)" Ix:",NULL);
        msysc$m_print_i64((i64)dd.index,NULL);
        msysc$m_print_space();
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)9)) {
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)" Nfields:",NULL);
        msysc$m_print_i64((i64)dd.nfields,NULL);
        msysc$m_print_nogap();
        msysc$m_print_str((byte*)" ",NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)5) || ($temp==(i64)7) || ($temp==(i64)6)) {
        msysc$m_print_startfile(f);
        msysc$m_print_setfmt((byte*)" Nparms:# ");
        msysc$m_print_i64((i64)dd.nparams,NULL);
        msysc$m_print_str((byte*)"DD.MISFUNC=",NULL);
        msysc$m_print_u64(msysc$m_getdotindex((i64)dd.flags,(i64)4),NULL);
        msysc$m_print_end();
        ;
    }
    };
        {i64 $temp = (i64)dd.nameid;
if (($temp==(i64)14) || ($temp==(i64)13) || ($temp==(i64)18) || ($temp==(i64)22) || ($temp==(i64)15) || ($temp==(i64)16)) {
        if (!!(dd.code)) {
                        {u64 $temp = msysc$m_getdotslice((i64)dd.flags,(i64)11,(i64)12);
if (($temp==(u64)3u)) {
                s = (byte*)"::=";
            }
            else if (($temp==(u64)2u)) {
                s = (byte*)":=";
            }
            else {
                s = (byte*)"=";
            }
            };
            msysc$m_print_startfile(f);
            msysc$m_print_str(s,NULL);
            msysc$m_print_str((*qq_lib$strexpr(dd.code)).strptr,NULL);
            msysc$m_print_space();
            msysc$m_print_end();
            ;
        }
;
    }
    };
    if (!!((i64)dd.mode)) {
        msysc$m_print_startfile(f);
        msysc$m_print_setfmt((byte*)"Mode:#");
        msysc$m_print_str(qq_show$strmode((i64)dd.mode,(i64)0),NULL);
        msysc$m_print_i64((i64)dd.mode,NULL);
        msysc$m_print_end();
        ;
    }
;
    msysc$m_print_startfile(f);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    tab = (byte*)"          ";
}

void qq_show$printtypetables(void *f) {
        struct qq_decls$strec *  d;
        struct qq_decls$userxrec *  p;
        i64 m;
        i64 i;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"PRINT TYPE TABLES",NULL);
    msysc$m_print_i64(qq_tables$nuserxtypes,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"PROC TYPE TABLES",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (m=(i64)0;m<=qq_tables$ntypes;++m) {
L1246 :;
        msysc$m_print_startfile(f);
        msysc$m_print_setfmt((byte*)"#: # ");
        msysc$m_print_i64(m,(byte*)"3");
        msysc$m_print_str(qq_tables$ttname[(m)],(byte*)"jl12");
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        d = qq_tables$ttnamedef[(m)];
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"\tST=",NULL);
        msysc$m_print_ptr(d,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"\tLen=",NULL);
        msysc$m_print_i64(qq_tables$ttlength[(m)],NULL);
        msysc$m_print_str((byte*)"Lower",NULL);
        msysc$m_print_i64(qq_tables$ttlower[(m)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"\tSize=",NULL);
        msysc$m_print_i64(qq_tables$ttsize[(m)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"\tBasetype=",NULL);
        msysc$m_print_i64((i64)qq_tables$ttbasetype[(m)],NULL);
        msysc$m_print_str(qq_tables$ttname[((i64)qq_tables$ttbasetype[(m)])],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"\tTarget=",NULL);
        msysc$m_print_i64((i64)qq_tables$tttarget[(m)],NULL);
        msysc$m_print_str(qq_tables$ttname[((i64)qq_tables$tttarget[(m)])],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"\tCaligned=",NULL);
        msysc$m_print_i64((i64)qq_tables$ttcaligned[(m)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        d = qq_tables$ttfields[(m)];
        if (!!(d)) {
            msysc$m_print_startfile(f);
            msysc$m_print_str((byte*)"\tFields:",NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            L1249 :;
            while (!!(d)) {
                msysc$m_print_startfile(f);
                msysc$m_print_str((byte*)"\t\t",NULL);
                msysc$m_print_str((*d).name,NULL);
                msysc$m_print_str((!!((i64)(*d).mode) ? qq_show$strmode((i64)(*d).mode,(i64)0) : (byte*)""),NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                d = (*d).nextdef;
L1250 :;
            }
L1251 :;
            ;
        }
;
L1247 :;
    }
L1248 :;
    ;
    p = (struct qq_decls$userxrec *)qq_tables$userxmodelist;
    for (i=(i64)1;i<=qq_tables$nuserxtypes;++i) {
L1252 :;
        msysc$m_print_startfile(f);
        msysc$m_print_i64(i,NULL);
        msysc$m_print_i64(-(i),NULL);
        msysc$m_print_str((*qq_tables$ttnamedefx[(i)]).name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L1253 :;
    }
L1254 :;
    ;
}

void qq_show$showsttree(void) {
        void *  f;
        struct qq_decls$filerec *  m;
        struct qq_decls$strec *  d;
        struct qq_decls$genfieldrec *  g;
        struct qq_decls$procrec *  p;
        i64 i;
    if (!(!!((i64)qq_cli$fshowst))) {
        return;
    }
;
    f = fopen((byte*)"ST",(byte*)"w");
    qq_show$printglobalsymbols(f);
    msysc$m_print_startfile(f);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"Modules",NULL);
    msysc$m_print_i64(qq_decls$nmodules,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=qq_decls$nmodules;++i) {
L1255 :;
        m = qq_decls$modules[(i)];
        if (!!(m)) {
            msysc$m_print_startfile(f);
            msysc$m_print_str((byte*)"\t",NULL);
            msysc$m_print_nogap();
            msysc$m_print_i64(i,NULL);
            msysc$m_print_nogap();
            msysc$m_print_str((byte*)":",NULL);
            msysc$m_print_str((*m).name,NULL);
            msysc$m_print_str((byte*)"M.COMPILED=",NULL);
            msysc$m_print_i64((i64)(*m).compiled,NULL);
            msysc$m_print_str((byte*)"M.PCSTART=",NULL);
            msysc$m_print_ptr((*m).pcstart,NULL);
            msysc$m_print_str((byte*)"M.PCSIZE=",NULL);
            msysc$m_print_i64((*m).pcsize,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
        else {
            msysc$m_print_startfile(f);
            msysc$m_print_str((byte*)"MODULE",NULL);
            msysc$m_print_i64(i,NULL);
            msysc$m_print_str((byte*)"MISSING",NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
;
L1256 :;
    }
L1257 :;
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"PROC Global GenField Table",NULL);
    msysc$m_print_i64(qq_decls$ngenfields,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=qq_decls$ngenfields;++i) {
L1258 :;
        g = (struct qq_decls$genfieldrec *)qq_decls$genfieldtable[(i)-1];
        if ((g == 0)) {
            goto L1259 ;
        }
;
        msysc$m_print_startfile(f);
        msysc$m_print_setfmt((byte*)"   #) #:");
        msysc$m_print_i64(i,NULL);
        msysc$m_print_str((*(*g).def).name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        L1261 :;
        while (!!(g)) {
            d = (*g).def;
            msysc$m_print_startfile(f);
            msysc$m_print_str((byte*)"      ",NULL);
            msysc$m_print_str((*d).name,NULL);
            msysc$m_print_str(qq_tables$namenames[((i64)(*d).nameid)],NULL);
            msysc$m_print_str((*(*d).owner).name,NULL);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
            g = (struct qq_decls$genfieldrec *)(*g).nextdef;
L1262 :;
        }
L1263 :;
        ;
L1259 :;
    }
L1260 :;
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"DLL Table",NULL);
    msysc$m_print_i64(qq_decls$nlibfiles,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=qq_decls$nlibfiles;++i) {
L1264 :;
        msysc$m_print_startfile(f);
        msysc$m_print_i64(i,NULL);
        msysc$m_print_str((byte*)":",NULL);
        msysc$m_print_str((*qq_decls$libtable[(i)-1]).name,NULL);
        msysc$m_print_u64(qq_decls$dllinsttable[(i)-1],NULL);
        msysc$m_print_i64((i64)qq_decls$libtypes[(i)-1],(byte*)"c");
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L1265 :;
    }
L1266 :;
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"DLL Proc Table",NULL);
    msysc$m_print_i64(qq_decls$ndllprocs,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=qq_decls$ndllprocs;++i) {
L1267 :;
        d = qq_decls$dllproctable[(i)-1];
        msysc$m_print_startfile(f);
        msysc$m_print_i64(i,NULL);
        msysc$m_print_str((byte*)":",NULL);
        msysc$m_print_str((*d).name,NULL);
        msysc$m_print_i64((i64)qq_decls$dllproclibindex[(i)-1],NULL);
        msysc$m_print_ptr(qq_decls$dllprocaddr[(i)-1],NULL);
        msysc$m_print_str((!!(msysc$m_getdotindex((i64)(*d).flags,(i64)8)) ? (byte*)"Variadic" : (byte*)""),NULL);
        msysc$m_print_i64((i64)qq_decls$libtypes[((i64)qq_decls$dllproclibindex[(i)-1])-1],(byte*)"c");
        msysc$m_print_str((byte*)"D.INDEX=",NULL);
        msysc$m_print_i64((i64)(*d).index,NULL);
        msysc$m_print_str((byte*)"DLLPROCTABLE[D.INDEX]=",NULL);
        msysc$m_print_ptr(qq_decls$dllproctable[((i64)(*d).index)-1],NULL);
        msysc$m_print_str((byte*)"D=",NULL);
        msysc$m_print_ptr(d,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L1268 :;
    }
L1269 :;
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"All Proc Table",NULL);
    msysc$m_print_i64(qq_decls$nproclist,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    p = (struct qq_decls$procrec *)qq_decls$proclist;
    L1270 :;
    while (!!(p)) {
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"Proc:",NULL);
        msysc$m_print_str((*(*p).def).name,NULL);
        msysc$m_print_str((*(*(*p).def).owner).name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        p = (struct qq_decls$procrec *)(*p).nextproc;
L1271 :;
    }
L1272 :;
    ;
    msysc$m_print_startfile(f);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    fclose(f);
}

void qq_show$showtypes(void) {
        void *  f;
    if (!(!!((i64)qq_cli$fshowtypes))) {
        return;
    }
;
    if (((i64)qq_cli$runcode == (i64)6)) {
        return;
    }
;
    f = fopen((byte*)"TYPES",(byte*)"w");
    qq_show$printtypetables(f);
    fclose(f);
}

void qq_show$showast(struct qq_decls$subprogrec *sp,u8 *file) {
        void *  f;
        i64 i;
    if (((i64)qq_cli$runcode == (i64)6)) {
        return;
    }
;
    f = fopen(file,(byte*)"w");
    if (!(!!(f))) {
        return;
    }
;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"PROC",NULL);
    msysc$m_print_str(file,NULL);
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)":",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    if (!!(sp)) {
        qq_show$showast2(f,sp);
    }
    else {
        for (i=(i64)1;i<=qq_decls$nsubprogs;++i) {
L1273 :;
            qq_show$showast2(f,qq_decls$subprogs[(i)-1]);
L1274 :;
        }
L1275 :;
        ;
    }
;
    fclose(f);
}

void qq_show$showast2(void *f,struct qq_decls$subprogrec *sp) {
        struct qq_decls$filerec *  pm;
        struct qq_decls$strec *  d;
        struct qq_decls$strec *  e;
        i64 i;
        i64 $av_1;
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"Proc Subprog",NULL);
    msysc$m_print_str((*sp).name,NULL);
    msysc$m_print_nogap();
    msysc$m_print_str((byte*)": ******\n",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
        ($av_1 = (i64)(*sp).lastmodule);
    for (i=(i64)(*sp).firstmodule;i<=$av_1;++i) {
L1276 :;
        pm = qq_decls$modules[(i)];
        msysc$m_print_startfile(f);
        msysc$m_print_str((byte*)"Module:",NULL);
        msysc$m_print_str((*pm).name,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        qq_show$printunit((struct qq_decls$unitrec *)(*pm).ast,(i64)0,(byte*)"*",f);
        d = (*(*pm).def).deflist;
        L1279 :;
        while (!!(d)) {
            if (((i64)(*d).nameid == (i64)5)) {
                msysc$m_print_startfile(f);
                msysc$m_print_str((byte*)"\n---PROC",NULL);
                msysc$m_print_str((*d).name,NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                qq_show$printunit((struct qq_decls$unitrec *)(*d).code,(i64)0,(byte*)"*",f);
                e = (*d).deflist;
                L1283 :;
                while (!!(e)) {
                    if (((i64)(*e).nameid == (i64)6)) {
                        msysc$m_print_startfile(f);
                        msysc$m_print_str((byte*)"\n---ANONPROC",NULL);
                        msysc$m_print_str((*e).name,NULL);
                        msysc$m_print_newline();
                        msysc$m_print_end();
                        ;
                        msysc$m_print_startcon();
                        msysc$m_print_str((byte*)"ANON",NULL);
                        msysc$m_print_ptr((*e).code,NULL);
                        msysc$m_print_newline();
                        msysc$m_print_end();
                        ;
                        qq_show$printunit((struct qq_decls$unitrec *)(*e).code,(i64)0,(byte*)"*",f);
                    }
;
L1284 :;
                    e = (*e).nextdef;
L1286 :;
                                    }
L1285 :;
                ;
            }
;
L1280 :;
            d = (*d).nextdef;
L1282 :;
                    }
L1281 :;
        ;
        msysc$m_print_startfile(f);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L1277 :;
    }
L1278 :;
    ;
}

void qq_show$showlogfile(void) {
        u8 str[256];
        void *  logdev;
    if ((((((((((i64)qq_cli$fshowpcl1 + (i64)qq_cli$fshowpcl2) + (i64)qq_cli$fshowast1) + (i64)qq_cli$fshowast2) + (i64)qq_cli$fshowst) + (i64)qq_cli$fshowtypes) + (i64)qq_cli$fshowmodules) + (i64)qq_cli$fshowstflat) == (i64)0)) {
        return;
    }
;
    if (((i64)qq_cli$runcode == (i64)6)) {
        return;
    }
;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"PRESS KEY",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    if ((mlinux$os_getch() == (i64)27)) {
        exit(0);
    }
;
    if (!!((i64)qq_cli$fshowst)) {
        qq_show$showsttree();
    }
;
    if (!!((i64)qq_cli$fshowstflat)) {
        qq_show$showstflat();
    }
;
    if (!!((i64)qq_cli$fshowtypes)) {
        qq_show$showtypes();
    }
;
    logdev = fopen((byte*)"qq.log",(byte*)"w");
    if (!!((i64)qq_cli$fshowmodules)) {
        qq_show$showmoduleinfo(logdev);
    }
;
    if ((((i64)qq_cli$runcode >= (i64)4) && !!((i64)qq_cli$fshowpcl2))) {
        qq_show$addtolog((byte*)"PCL2",logdev);
    }
;
    if ((((i64)qq_cli$runcode >= (i64)4) && !!((i64)qq_cli$fshowpcl1))) {
        qq_show$addtolog((byte*)"PCL1",logdev);
    }
;
    if ((((i64)qq_cli$runcode >= (i64)3) && !!((i64)qq_cli$fshowast2))) {
        qq_show$addtolog((byte*)"AST2",logdev);
    }
;
    if ((((i64)qq_cli$runcode >= (i64)2) && !!((i64)qq_cli$fshowast1))) {
        qq_show$addtolog((byte*)"AST1",logdev);
    }
;
    if (!!((i64)qq_cli$fshowst)) {
        qq_show$addtolog((byte*)"ST",logdev);
    }
;
    if (!!((i64)qq_cli$fshowstflat)) {
        qq_show$addtolog((byte*)"STFLAT",logdev);
    }
;
    if (!!((i64)qq_cli$fshowtypes)) {
        qq_show$addtolog((byte*)"TYPES",logdev);
    }
;
    fclose(logdev);
    msysc$m_print_startstr(str);
    msysc$m_print_setfmt((byte*)"c:/m/scripts/med.bat #");
    msysc$m_print_str((byte*)"qq.log",NULL);
    msysc$m_print_end();
    ;
    mlinux$os_execwait(str,(i64)0,0);
}

static void qq_show$addtolog(u8 *filename,void *logdest) {
        void *  f;
        i64 c;
    f = fopen(filename,(byte*)"rb");
    if ((f == 0)) {
        return;
    }
;
    L1287 :;
    while (1) {
        c = fgetc(f);
        if ((c == (i64)-1)) {
            goto L1288 ;
        }
;
        fputc((i32)c,logdest);
    }
L1288 :;
    ;
    fclose(f);
}

void qq_show$showstflat(void) {
        void *  f;
        struct qq_decls$strec *  p;
        i64 sym;
        i64 i;
    if (!(!!((i64)qq_cli$fshowstflat))) {
        return;
    }
;
    f = fopen((byte*)"STFLAT",(byte*)"w");
    msysc$m_print_startfile(f);
    msysc$m_print_str((byte*)"GLOBAL FLAT SYMBOL TABLE:",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)0;i<=(i64)32766;++i) {
L1289 :;
        p = &qq_lex$hashtable[(i)];
        if (!!((*p).name)) {
                        {i64 $temp = (i64)(*p).symbolcode;
if (($temp==(i64)69)) {
                msysc$m_print_startfile(f);
                msysc$m_print_i64(i,NULL);
                msysc$m_print_ptr(p,NULL);
                msysc$m_print_str((byte*)":",NULL);
                msysc$m_print_str((*p).name,NULL);
                msysc$m_print_u64(qq_tables$symbolnames[((i64)(*p).symbolcode)-1],(byte*)"m");
                msysc$m_print_str(qq_tables$namenames[((i64)(*p).nameid)],NULL);
                msysc$m_print_newline();
                msysc$m_print_end();
                ;
                p = (*p).nextdupl;
                L1292 :;
                while (!!(p)) {
                    sym = (i64)(*p).symbolcode;
                    if ((sym == (i64)0)) {
                        sym = (i64)1;
                    }
;
                    msysc$m_print_startfile(f);
                    msysc$m_print_str((byte*)"\t",NULL);
                    msysc$m_print_ptr(p,NULL);
                    msysc$m_print_str((*p).name,NULL);
                    msysc$m_print_u64(qq_tables$symbolnames[(sym)-1],(byte*)"m");
                    msysc$m_print_str(qq_tables$namenames[((i64)(*p).nameid)],NULL);
                    msysc$m_print_str((byte*)"(From",NULL);
                    msysc$m_print_str((!!((*p).owner) ? (*(*p).owner).name : (byte*)"-"),NULL);
                    msysc$m_print_nogap();
                    msysc$m_print_str((byte*)")",NULL);
                    msysc$m_print_newline();
                    msysc$m_print_end();
                    ;
                    p = (*p).nextdupl;
L1293 :;
                }
L1294 :;
                ;
            }
            };
        }
;
L1290 :;
    }
L1291 :;
    ;
    fclose(f);
}

void qq_show$showmoduleinfo(void *dev) {
        struct qq_decls$filerec *  pm;
        struct qq_decls$subprogrec *  ps;
        static u8 *  tab = (byte*)"    ";
        i64 $av_1;
        struct qq_decls$strec *  d;
        u8 *  id;
        i64 i;
        i64 j;
    msysc$m_print_startcon();
    msysc$m_print_str((byte*)"SMI0",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
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
    msysc$m_print_i64(qq_decls$nmodules,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=qq_decls$nmodules;++i) {
L1295 :;
        pm = qq_decls$modules[(i)];
        msysc$m_print_startfile(dev);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_i64(i,(byte*)"2");
        msysc$m_print_str((*pm).name,(byte*)"16jl");
        msysc$m_print_str((byte*)"Lead:",NULL);
        msysc$m_print_i64((i64)(*pm).islead,NULL);
        msysc$m_print_str((byte*)"Sys:",NULL);
        msysc$m_print_i64((i64)(*pm).issyslib,NULL);
        msysc$m_print_str((byte*)"Path:",NULL);
        msysc$m_print_str((*pm).path,NULL);
        msysc$m_print_str((byte*)"Sub:",NULL);
        msysc$m_print_str((*qq_decls$subprogs[((i64)(*pm).subprogno)-1]).name,NULL);
        msysc$m_print_str((byte*)"File:",NULL);
        msysc$m_print_str((*pm).filespec,NULL);
        msysc$m_print_end();
        ;
        msysc$m_print_startfile(dev);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L1296 :;
    }
L1297 :;
    ;
    msysc$m_print_startfile(dev);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    msysc$m_print_startfile(dev);
    msysc$m_print_str((byte*)"Subprograms",NULL);
    msysc$m_print_i64(qq_decls$nsubprogs,NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    for (i=(i64)1;i<=qq_decls$nsubprogs;++i) {
L1298 :;
        ps = (struct qq_decls$subprogrec *)qq_decls$subprogs[(i)-1];
        msysc$m_print_startfile(dev);
        msysc$m_print_str(tab,NULL);
        msysc$m_print_i64(i,NULL);
        msysc$m_print_str((*ps).name,NULL);
        msysc$m_print_str((byte*)"Sys:",NULL);
        msysc$m_print_i64((i64)(*ps).issyslib,NULL);
        msysc$m_print_str((byte*)"Path:",NULL);
        msysc$m_print_str((*ps).path,NULL);
        msysc$m_print_str((byte*)"Spec:",NULL);
        msysc$m_print_str((*ps).filespec,NULL);
        msysc$m_print_str((byte*)"Comp:",NULL);
        msysc$m_print_i64((i64)(*ps).compiled,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        if (!!((i64)(*ps).firstmodule)) {
            msysc$m_print_startfile(dev);
            msysc$m_print_str(tab,NULL);
            msysc$m_print_str(tab,NULL);
            msysc$m_print_i64((i64)(*ps).firstmodule,NULL);
            msysc$m_print_i64((i64)(*ps).lastmodule,NULL);
            msysc$m_print_nogap();
            msysc$m_print_str((byte*)": ",NULL);
            msysc$m_print_end();
            ;
                        ($av_1 = (i64)(*ps).lastmodule);
            for (j=(i64)(*ps).firstmodule;j<=$av_1;++j) {
L1301 :;
                msysc$m_print_startfile(dev);
                msysc$m_print_str((*qq_decls$modules[(j)]).name,NULL);
                msysc$m_print_space();
                msysc$m_print_end();
                ;
L1302 :;
            }
L1303 :;
            ;
            msysc$m_print_startfile(dev);
            msysc$m_print_newline();
            msysc$m_print_end();
            ;
        }
;
L1299 :;
    }
L1300 :;
    ;
    msysc$m_print_startfile(dev);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    if (!(!!(qq_decls$stprogram))) {
        return;
    }
;
    msysc$m_print_startfile(dev);
    msysc$m_print_str((byte*)"Symboltable:",NULL);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
    d = (*qq_decls$stprogram).deflist;
    L1304 :;
    while (!!(d)) {
                {i64 $temp = (i64)(*d).nameid;
if (($temp==(i64)3)) {
            id = (byte*)"Mod";
        }
        else if (($temp==(i64)2)) {
            id = (byte*)"Sub";
        }
        else {
            id = (byte*)"---";
        }
        };
        msysc$m_print_startfile(dev);
        msysc$m_print_setfmt((byte*)"    # # (m#, s#)");
        msysc$m_print_str((*d).name,NULL);
        msysc$m_print_str(id,NULL);
        msysc$m_print_i64((i64)(*d).moduleno,NULL);
        msysc$m_print_i64((i64)(*d).subprogno,NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
L1305 :;
        d = (*d).nextdef;
L1307 :;
            }
L1306 :;
    ;
    msysc$m_print_startfile(dev);
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

void qq_show$printsymbol(struct qq_decls$lexrec *lp) {
        struct qq_decls$lexrec l;
    l = (*lp);
    msysc$m_print_startcon();
    msysc$m_print_u64(qq_tables$symbolnames[((i64)l.symbol)-1],(byte*)"m 18 jl");
    msysc$m_print_end();
    ;
        {i64 $temp = (i64)l.symbol;
if (($temp==(i64)69)) {
        msysc$printstr_n((*l.symptr).name,(i64)(*l.symptr).namelen);
    }
    else if (($temp==(i64)63)) {
                {i64 $temp = (i64)l.subcode;
if (($temp==(i64)1)) {
            msysc$m_print_startcon();
            msysc$m_print_i64(l.value,NULL);
            msysc$m_print_str((byte*)"int",NULL);
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
    else if (($temp==(i64)65)) {
        msysc$m_print_startcon();
        msysc$m_print_r64(l.xvalue,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)67)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"\"",NULL);
        msysc$m_print_space();
        msysc$m_print_end();
        ;
        msysc$printstr(l.svalue);
        msysc$m_print_startcon();
        msysc$m_print_space();
        msysc$m_print_str((byte*)"\"",NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)66)) {
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"'",NULL);
        msysc$m_print_space();
        msysc$m_print_end();
        ;
        msysc$printstr(l.svalue);
        msysc$m_print_startcon();
        msysc$m_print_space();
        msysc$m_print_str((byte*)"'",NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)64)) {
        msysc$printstr(l.svalue);
        msysc$m_print_startcon();
        msysc$m_print_str((byte*)"L",NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)6) || ($temp==(i64)19) || ($temp==(i64)15) || ($temp==(i64)20)) {
        msysc$m_print_startcon();
        msysc$m_print_str(qq_tables$jtagnames[((i64)l.subcode)],NULL);
        msysc$m_print_end();
        ;
    }
    else {
        if (!!((i64)l.subcode)) {
            msysc$m_print_startcon();
            msysc$m_print_str((byte*)"#",NULL);
            msysc$m_print_i64((i64)l.subcode,NULL);
            msysc$m_print_end();
            ;
        }
;
    }
    };
    msysc$m_print_startcon();
    msysc$m_print_newline();
    msysc$m_print_end();
    ;
}

u8 *qq_show$strmode(i64 t,i64 expand) {
        static u8 str[2048];
    qq_show$istrmode(t,(u8 *)str,expand);
    return str;
}

static void qq_show$istrmode(i64 t,u8 *dest,i64 expand) {
        struct qq_decls$strec *  d;
    if ((t < (i64)0)) {
        strcpy(dest,(byte*)"*");
        strcat(dest,(*qq_tables$ttnamedefx[(-(t))]).name);
        return;
    }
;
    if ((t < (i64)40)) {
        strcpy(dest,qq_tables$ttname[(t)]);
        return;
    }
;
        {i64 $temp = (i64)qq_tables$ttbasetype[(t)];
if (($temp==(i64)16)) {
        strcpy(dest,(byte*)"ref ");
        qq_show$istrmode((i64)qq_tables$tttarget[(t)],(dest + strlen(dest)),(i64)0);
    }
    else if (($temp==(i64)7)) {
        msysc$m_print_startstr(dest);
        msysc$m_print_setfmt((byte*)"[#..#]");
        msysc$m_print_i64(qq_tables$ttlower[(t)],NULL);
        msysc$m_print_i64(((qq_tables$ttlength[(t)] + qq_tables$ttlower[(t)]) - (i64)1),NULL);
        msysc$m_print_end();
        ;
        qq_show$istrmode((i64)qq_tables$tttarget[(t)],(dest + strlen(dest)),(i64)0);
    }
    else if (($temp==(i64)13)) {
        if (!(!!(expand))) {
            goto L1308 ;
;
        }
;
        strcpy(dest,(byte*)"struct(");
        //dostruct:
L1309 :;
;
        d = qq_tables$ttfields[(t)];
        L1310 :;
        while (!!(d)) {
            qq_show$istrmode((i64)(*d).mode,(dest + strlen(dest)),(i64)0);
            strcat(dest,(byte*)" ");
            strcat(dest,(*d).name);
            if (!!((*d).nextdef)) {
                strcat(dest,(byte*)", ");
            }
;
L1311 :;
            d = (*d).nextdef;
L1313 :;
                    }
L1312 :;
        ;
        strcat(dest,(byte*)")");
    }
    else if (($temp==(i64)12)) {
        if (!(!!(expand))) {
            goto L1308 ;
;
        }
;
        strcpy(dest,(byte*)"record(");
        goto L1309 ;
;
    }
    else {
        //$else:
L1308 :;
;
        strcpy(dest,qq_tables$ttname[(t)]);
    }
    };
}

void qq_show$deletetempfiles(void) {
    remove((byte*)"PCL1");
    remove((byte*)"PCL2");
    remove((byte*)"PCL3");
    remove((byte*)"AST1");
    remove((byte*)"AST2");
    remove((byte*)"TYPES");
    remove((byte*)"STFLAT");
    remove((byte*)"ST");
}

// START
void qq_show$start(void) {

}

static void qq_showpcl$writepcl(struct qq_pcltabs$pclrec *pcstart,struct qq_pcltabs$pclrec *pc,i32 *pclsource,i64 pass,u8 *sourcecode) {
        u8 str[512];
        i64 cmdcode;
        i64 a;
        i64 soffset;
        i64 offset;
        i64 attrs;
        i64 $av_1;
    cmdcode = (i64)(*pc).opcode;
    if ((cmdcode==(i64)1)) {
        return;
    }
    else if ((cmdcode==(i64)2)) {
        qq_showpcl$currpclproc = (*pc).def;
        qq_showpcl$gstr((byte*)"!      ----------");
        qq_showpcl$gstr((byte*)"Procdef:");
        qq_showpcl$gstr((*qq_showpcl$currpclproc).name);
        qq_showpcl$gline();
        return;
    }
    else if ((cmdcode==(i64)4)) {
        qq_showpcl$gstr((byte*)"!      ----------");
        qq_showpcl$gstrln((byte*)"End");
        return;
    }
;
    if (!!(msysc$m_getdotindex((i64)(*pc).flags,(i64)0))) {
        qq_showpcl$gstr((byte*)"                 ");
        qq_showpcl$glabeldef(pcstart,pc);
    }
;
    offset = ((pc - pcstart) + (i64)1);
    soffset = (i64)(*(pclsource + offset));
    qq_showpcl$currlineno = (soffset & (i64)16777215);
    msysc$m_print_startstr(str);
    msysc$m_print_setfmt((byte*)"#: [#]: ");
    msysc$m_print_i64(((pc - pcstart) + (i64)1),(byte*)"4");
    msysc$m_print_i64(qq_showpcl$currlineno,(byte*)"05jr");
    msysc$m_print_end();
    ;
    qq_showpcl$gstr((u8 *)str);
    if ((cmdcode==(i64)2)) {
        qq_showpcl$currpclproc = (*pc).def;
        return;
    }
    else if ((cmdcode==(i64)6)) {
        qq_showpcl$gstr((byte*)"! ");
        qq_showpcl$gstrln((*pc).svalue);
        return;
    }
;
    str[((i64)1)-1] = (u64)0u;
    if (!!(msysc$m_getdotindex((i64)(*pc).flags,(i64)1))) {
        strcat(str,(byte*)"*");
    }
;
    strcat(str,(qq_pcltabs$pclnames[(cmdcode)] + (i64)1));
    a = (i64)1;
    mlib$gs_leftstr((struct mlib$strbuffer *)qq_show$pcldest,(byte*)" ",(i64)7,(i64)45);
    mlib$gs_leftstr((struct mlib$strbuffer *)qq_show$pcldest,(u8 *)str,(i64)11,(i64)32);
    if (!!((i64)qq_pcltabs$pclopnd[(cmdcode)])) {
        strcpy(str,qq_showpcl$writepclopnd(pcstart,pc,pass));
        qq_showpcl$gstr(str);
        qq_showpcl$gstr((byte*)" ");
    }
;
    attrs = (i64)qq_pcltabs$pclattrs[(cmdcode)];
    if ((attrs != (i64)538976288)) {
        qq_showpcl$gstr((byte*)"<");
        $av_1 = (i64)4;
        while ($av_1-- > 0) {
L1314 :;
                        {u64 $temp = msysc$m_getdotslice(attrs,(i64)0,(i64)7);
if (($temp==(u64)32u)) {
                goto L1316 ;
            }
            else if (($temp==(u64)110u) || ($temp==(u64)98u)) {
                qq_showpcl$gstrint((i64)(*pc).n);
            }
            else if (($temp==(u64)120u)) {
                qq_showpcl$gstrint((i64)(*pc).x);
            }
            else if (($temp==(u64)121u)) {
                qq_showpcl$gstrint((i64)(*pc).y);
            }
            else if (($temp==(u64)99u)) {
                qq_showpcl$gstr(qq_tables$condnames[((i64)(*pc).n)]);
            }
            else if (($temp==(u64)117u)) {
                qq_showpcl$gstr(qq_tables$ttname[((i64)(*pc).usertag)]);
            }
            else if (($temp==(u64)118u)) {
                qq_showpcl$gstr(qq_tables$ttname[((i64)(*pc).usertag2)]);
            }
            };
            attrs >>= (i64)8;
            if (((attrs & (i64)255) != (i64)32)) {
                qq_showpcl$gstr((byte*)" ");
            }
;
L1315 :;
        }
L1316 :;
        ;
        qq_showpcl$gstr((byte*)">");
    }
;
    qq_showpcl$gline();
}

static u8 *qq_showpcl$writepclopnd(struct qq_pcltabs$pclrec *pcstart,struct qq_pcltabs$pclrec *pc,i64 pass) {
        static u8 str[512];
        static u8 str2[512];
        struct qq_decls$strec *  d;
        u8 *  s;
        i64 slen;
        struct qq_decls$objrec *  p;
    d = (*pc).def;
        {i64 $temp = (i64)qq_pcltabs$pclopnd[((i64)(*pc).opcode)];
if (($temp==(i64)7)) {
        msysc$m_print_startstr(str);
        msysc$m_print_i64((*pc).value,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)8)) {
        msysc$m_print_startstr(str);
        msysc$m_print_r64((*pc).xvalue,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)9)) {
        if ((pass == (i64)1)) {
            goto L1317 ;
;
        }
;
        p = (*pc).objptr;
        if (((slen = (*p).length) == (i64)0)) {
            return (byte*)"\"";
        }
;
        s = (*p).strptr;
        goto L1318 ;
;
    }
    else if (($temp==(i64)10)) {
        //docstringz:
L1317 :;
;
        s = (*pc).svalue;
        slen = strlen(s);
        //dostring:
L1318 :;
;
        if ((slen >= (i64)255)) {
            slen = (i64)255;
        }
;
        memcpy(str,(void *)s,(u64)slen);
        str[((slen + (i64)1))-1] = (u64)0u;
        qq_lib$convertstring((u8 *)str,(u8 *)str2);
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"\"#\"");
        msysc$m_print_str(str2,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)1)) {
        if ((pass == (i64)1)) {
            strcpy(str,(*d).name);
        }
        else {
            d = qq_cli$allstaticdefs;
            L1319 :;
            while (!!(d)) {
                if (((*d).varptr == (*pc).varptr)) {
                    goto L1321 ;
                }
;
L1320 :;
                d = (*d).nextstatic;
L1322 :;
                            }
L1321 :;
            ;
            msysc$m_print_startstr(str);
            msysc$m_print_setfmt((byte*)"[#] (#:#)");
            msysc$m_print_ptr((*d).varptr,(byte*)"h");
            msysc$m_print_str((!!(d) ? (*(*d).owner).name : (byte*)"?"),NULL);
            msysc$m_print_str((!!(d) ? (*d).name : (byte*)"?"),NULL);
            msysc$m_print_end();
            ;
        }
;
    }
    else if (($temp==(i64)2)) {
        if ((pass == (i64)1)) {
            strcpy(str,(*d).name);
        }
        else {
            d = (*qq_showpcl$currpclproc).deflist;
            L1323 :;
            while (!!(d)) {
                if ((((i64)(*d).nameid == (i64)14 || (i64)(*d).nameid == (i64)15) && (((i64)(*d).index * (i64)16) == (*pc).offset))) {
                    msysc$m_print_startstr(str);
                    msysc$m_print_setfmt((byte*)"[#] (#)");
                    msysc$m_print_i64(((*pc).offset / (i64)16),NULL);
                    msysc$m_print_str((*d).name,NULL);
                    msysc$m_print_end();
                    ;
                    return str;
                }
;
                d = (*d).nextdef;
L1324 :;
            }
L1325 :;
            ;
        }
;
    }
    else if (($temp==(i64)12)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"#.$");
        msysc$m_print_str((*d).name,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)3)) {
        if ((pass == (i64)1)) {
            strcpy(str,(*d).name);
        }
        else {
            d = qq_cli$allprocdefs;
            L1326 :;
            while (!!(d)) {
                if (((*d).labelref == (*pc).labelref)) {
                    goto L1328 ;
                }
;
L1327 :;
                d = (*d).nextproc;
L1329 :;
                            }
L1328 :;
            ;
            msysc$m_print_startstr(str);
            msysc$m_print_setfmt((byte*)"[#] (#:#)");
            msysc$m_print_ptr((*pc).labelref,NULL);
            msysc$m_print_str((!!(d) ? (*(*d).owner).name : (byte*)"?"),NULL);
            msysc$m_print_str((!!(d) ? (*d).name : (byte*)"?"),NULL);
            msysc$m_print_end();
            ;
        }
;
    }
    else if (($temp==(i64)4)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"[DLL:#]");
        msysc$m_print_str((*d).name,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)15)) {
        msysc$m_print_startstr(str);
        msysc$m_print_i64((*pc).hostindex,NULL);
        msysc$m_print_str((qq_tables$hostfnnames[((*pc).hostindex)] + (i64)2),NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)5)) {
        if ((pass == (i64)1)) {
            msysc$m_print_startstr(str);
            msysc$m_print_setfmt((byte*)".#");
            msysc$m_print_str((*d).name,NULL);
            msysc$m_print_end();
            ;
        }
        else {
            msysc$m_print_startstr(str);
            msysc$m_print_setfmt((byte*)"## (#)");
            msysc$m_print_str((byte*)"#",NULL);
            msysc$m_print_i64((*pc).value,NULL);
            msysc$m_print_str((*(*qq_decls$genfieldtable[((*pc).value)-1]).def).name,NULL);
            msysc$m_print_end();
            ;
        }
;
    }
    else if (($temp==(i64)11)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"T:# (#)");
        msysc$m_print_str(qq_show$strmode((*pc).typecode,(i64)0),NULL);
        msysc$m_print_i64((*pc).typecode,NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)6)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"L#");
        msysc$m_print_i64((((*pc).labelref - pcstart) + (i64)1),NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)13)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"(#)");
        msysc$m_print_str(qq_pcltabs$pclnames[((i64)(*pc).pclop)],NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)16)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"(# #)");
        msysc$m_print_i64((i64)(*pc).bintoindex,NULL);
        msysc$m_print_str(qq_pcltabs$pclnames[(qq_pcltabs$bintotable[((i64)(*pc).bintoindex)-1].pclop)],NULL);
        msysc$m_print_end();
        ;
    }
    else if (($temp==(i64)14)) {
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"<#>");
        msysc$m_print_str((qq_tables$mathsnames[((i64)(*pc).mathscode)-1] + (i64)3),NULL);
        msysc$m_print_end();
        ;
    }
    else {
        //other:
L1330 :;
;
        msysc$m_print_startstr(str);
        msysc$m_print_setfmt((byte*)"<#>");
        msysc$m_print_str(qq_pcltabs$opndnames[((i64)qq_pcltabs$pclopnd[((i64)(*pc).opcode)])],NULL);
        msysc$m_print_end();
        ;
    }
    };
    return str;
}

void qq_showpcl$writeallpcl(struct qq_decls$filerec *pm,i64 pass) {
        i64 cmd;
        struct qq_pcltabs$pclrec *  pc;
        struct qq_pcltabs$pclrec *  pclcode;
        i32 *  pclsource;
        u8 *  sourcecode;
    qq_showpcl$currlineno = (i64)0;
    qq_showpcl$gstr((byte*)"PCL FOR MODULE:");
    qq_showpcl$gstrln((*pm).name);
    pc = (pclcode = (*pm).pcstart);
    pclsource = (*pm).pcsourcestart;
    sourcecode = (*pm).text;
    L1331 :;
    do {
        cmd = (i64)(*pc).opcode;
        qq_showpcl$writepcl(pclcode,pc,pclsource,pass,sourcecode);
        ++(pc);
L1332 :;
    }
    while (!(cmd == (i64)5));
L1333 :;
    ;
    qq_showpcl$gline();
}

void qq_showpcl$showpcl(struct qq_decls$subprogrec *sp,i64 pass) {
        void *  f;
        i64 i;
    if (((i64)qq_cli$runcode == (i64)6)) {
        return;
    }
;
    mlib$gs_init((struct mlib$strbuffer *)qq_show$pcldest);
    mlib$gs_str((struct mlib$strbuffer *)qq_show$pcldest,(byte*)"PROC ALL PCL pass:");
    mlib$gs_strint((struct mlib$strbuffer *)qq_show$pcldest,pass);
    mlib$gs_line((struct mlib$strbuffer *)qq_show$pcldest);
    if (!!(sp)) {
        qq_showpcl$showpcl2(sp,pass);
    }
    else {
        for (i=(i64)1;i<=qq_decls$nsubprogs;++i) {
L1334 :;
            qq_showpcl$showpcl2(qq_decls$subprogs[(i)-1],pass);
L1335 :;
        }
L1336 :;
        ;
    }
;
    f = fopen((pass==1?(byte*)"PCL1":(pass==2?(byte*)"PCL2":(byte*)"PCL3")),(byte*)"w");
    if (!(!!(f))) {
        return;
    }
;
    mlib$gs_println((struct mlib$strbuffer *)qq_show$pcldest,f);
    fclose(f);
}

void qq_showpcl$showpcl2(struct qq_decls$subprogrec *sp,i64 pass) {
        i64 $av_1;
        i64 i;
        ($av_1 = (i64)(*sp).lastmodule);
    for (i=(i64)(*sp).firstmodule;i<=$av_1;++i) {
L1337 :;
        qq_showpcl$writeallpcl(qq_decls$modules[(i)],pass);
L1338 :;
    }
L1339 :;
    ;
}

void qq_showpcl$gstr(u8 *s) {
    mlib$gs_str((struct mlib$strbuffer *)qq_show$pcldest,s);
}

void qq_showpcl$gstrln(u8 *s) {
    mlib$gs_strln((struct mlib$strbuffer *)qq_show$pcldest,s);
}

void qq_showpcl$gline(void) {
    mlib$gs_line((struct mlib$strbuffer *)qq_show$pcldest);
}

void qq_showpcl$gstrint(i64 a) {
    mlib$gs_strint((struct mlib$strbuffer *)qq_show$pcldest,a);
}

void qq_showpcl$glabeldef(struct qq_pcltabs$pclrec *pcstart,struct qq_pcltabs$pclrec *pc) {
    qq_showpcl$gstr((byte*)"L");
    qq_showpcl$gstrint(((pc - pcstart) + (i64)1));
    qq_showpcl$gstrln((byte*)": ");
}

// START
void qq_showpcl$start(void) {

}

void qq_vars$var_unshareu(struct qq_decls$varrec *p) {
    if ((--((*(*p).objptr).refcount) <= (i64)0)) {
        qq_vars$var_free(p);
    }
;
}

void qq_vars$obj_shareu(struct qq_decls$objrec *p) {
    ++((*p).refcount);
}

struct qq_decls$varrec *qq_vars$void_new(void) {
        struct qq_decls$varrec *  p;
    p = (struct qq_decls$varrec *)mlib$pcm_alloc((i64)16);
    (*p).tagx = (i64)0;
    return p;
}

struct qq_decls$objrec *qq_vars$obj_new(void) {
        struct qq_decls$objrec *  p;
    p = (struct qq_decls$objrec *)mlib$pcm_alloc((i64)32);
    (*p) = qq_vars$zeroobj;
    (*p).refcount = (i64)1;
    return p;
}

i64 qq_vars$var_getintvalue(struct qq_decls$varrec *p) {
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)1) || ($temp==(i64)18)) {
        return (*p).value;
    }
    else if (($temp==(i64)2)) {
        return (i64)(*p).xvalue;
    }
    else {
        qq_runaux$pcustype((byte*)"getintvalue",p);
    }
    };
    return (i64)0;
}

void qq_vars$var_fromobj(i64 tag,struct qq_decls$objrec *p,struct qq_decls$varrec *dest) {
    (*dest).tagx = (tag | (i64)256);
    (*dest).objptr = p;
}

void qq_vars$var_free(struct qq_decls$varrec *a) {
        struct qq_decls$varrec v;
        struct qq_decls$objrec *  q;
    q = (*a).objptr;
        {i64 $temp = (i64)(*q).objtype;
if (($temp==(i64)0)) {
                {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)10)) {
            qq_lists$obj_free_list(q);
        }
        else if (($temp==(i64)12)) {
            qq_records$obj_free_record(q);
        }
        else if (($temp==(i64)9)) {
            qq_strings$obj_free_string(q);
        }
        else if (($temp==(i64)11)) {
            qq_arrays$obj_free_array(q);
        }
        else if (($temp==(i64)7)) {
            qq_arrays$obj_free_vector(q);
        }
        else if (($temp==(i64)8)) {
            qq_bits$obj_free_bits(q,(i64)(*a).tag);
        }
        else if (($temp==(i64)13)) {
            qq_packed$obj_free_struct(q);
        }
        else if (($temp==(i64)6)) {
            qq_dicts$obj_free_dict(q,(i64)0);
        }
        else if (($temp==(i64)5)) {
            qq_sets$obj_free_set(q);
        }
        else if (($temp==(i64)3)) {
            qq_decimal$obj_free_dec(q);
        }
        else {
            qq_runaux$pcustype((byte*)"free",a);
        }
        };
    }
    else if (($temp==(i64)1)) {
        v.tagx = (i64)(*a).tag;
        v.objptr = (*q).objptr2;
        qq_vars$var_unshareu(&v);
        mlib$pcm_free32(q);
    }
    else {
        mlib$pcm_free32(q);
    }
    };
}

void qq_vars$var_duplu(struct qq_decls$varrec *a) {
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)9)) {
        qq_strings$var_dupl_string(a);
    }
    else if (($temp==(i64)5)) {
        qq_sets$var_dupl_set(a);
    }
    else if (($temp==(i64)10)) {
        qq_lists$var_dupl_list(a);
    }
    else if (($temp==(i64)6)) {
        qq_dicts$var_dupl_dict(a);
    }
    else if (($temp==(i64)11)) {
        qq_arrays$var_dupl_array(a);
    }
    else if (($temp==(i64)7)) {
        qq_arrays$var_dupl_vector(a);
    }
    else if (($temp==(i64)8)) {
        qq_bits$var_dupl_bits(a);
    }
    else if (($temp==(i64)12)) {
        qq_records$var_dupl_record(a);
    }
    else if (($temp==(i64)13)) {
        qq_packed$var_dupl_struct(a);
    }
    else if (($temp==(i64)3)) {
        qq_decimal$var_dupl_dec(a);
    }
    else {
        qq_runaux$pcustype_t((byte*)"dupl",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_neg(struct qq_decls$varrec *a) {
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        (*a).value = -((*a).value);
    }
    else if (($temp==(i64)2)) {
        (*a).xvalue = -((*a).xvalue);
    }
    else if (($temp==(i64)3)) {
        qq_decimal$var_dupl_dec(a);
        qq_decimal$var_neg_dec(a);
    }
    else if (($temp==(i64)5)) {
        qq_sets$var_dupl_set(a);
        qq_sets$var_inotto_set(a);
    }
    else {
        qq_runaux$pcustype_t((byte*)"neg",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_abs(struct qq_decls$varrec *a) {
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        (*a).value = m$llabs((*a).value);
    }
    else if (($temp==(i64)2)) {
        (*a).xvalue = fabs((*a).xvalue);
    }
    else if (($temp==(i64)3)) {
        qq_decimal$var_dupl_dec(a);
        qq_decimal$var_abs_dec(a);
    }
    else {
        qq_runaux$pcustype_t((byte*)"abs",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_inot(struct qq_decls$varrec *a) {
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        (*a).value = ~((*a).value);
    }
    else if (($temp==(i64)5)) {
        qq_sets$var_dupl_set(a);
        qq_sets$var_inotto_set(a);
    }
    else {
        qq_runaux$pcustype_t((byte*)"inot",(i64)(*a).tag);
    }
    };
}

i64 qq_vars$var_istruel(struct qq_decls$varrec *a) {
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1) || ($temp==(i64)14) || ($temp==(i64)16) || ($temp==(i64)15) || ($temp==(i64)18) || ($temp==(i64)17)) {
        return (i64)!!((*a).value);
    }
    else if (($temp==(i64)2)) {
        return (((*a).xvalue != (double)0.) ? (i64)1 : (i64)0);
    }
    else if (($temp==(i64)9) || ($temp==(i64)10) || ($temp==(i64)11) || ($temp==(i64)8) || ($temp==(i64)7)) {
        return (i64)((*(*a).objptr).length != (i64)0);
    }
    else if (($temp==(i64)5)) {
        return (i64)((*(*a).objptr).length != (i64)0);
    }
    else if (($temp==(i64)12) || ($temp==(i64)13)) {
        return (i64)1;
    }
    else if (($temp==(i64)3)) {
        return (i64)!(!!(qq_decimal$bn_iszero((*a).objptr)));
    }
    else if (($temp==(i64)0)) {
        return (i64)0;
    }
    else {
        qq_runaux$pcustype_t((byte*)"istruel",(i64)(*a).tag);
    }
    };
    return (i64)0;
}

void qq_vars$var_add(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        qq_vars$var_addmixed(a,b);
        return;
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        (*a).value += (*b).value;
    }
    else if (($temp==(i64)2)) {
        (*a).xvalue += (*b).xvalue;
    }
    else if (($temp==(i64)3)) {
        qq_decimal$var_add_dec(a,b);
    }
    else if (($temp==(i64)9)) {
        qq_strings$var_add_string(a,b);
    }
    else if (($temp==(i64)5)) {
        qq_sets$var_dupl_set(a);
        qq_sets$var_iorto_set(a,b);
    }
    else {
        qq_runaux$pcustype_t((byte*)"add",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_addmixed(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        i64 newtag;
    newtag = (i64)(*a).tag;
        {i64 $temp = (((i64)(*a).tag << (i64)16) | (i64)(*b).tag);
if (($temp==(i64)65538)) {
        newtag = (i64)2;
        (*a).xvalue = ((r64)(*a).value + (*b).xvalue);
    }
    else if (($temp==(i64)131073)) {
        (*a).xvalue += (r64)(*b).value;
    }
    else if (($temp==(i64)65539) || ($temp==(i64)131075)) {
        newtag = (i64)3;
        qq_decimal$var_add_dec(qq_decimal$dectemp(a),b);
        qq_decimal$freedectemp();
    }
    else if (($temp==(i64)196609) || ($temp==(i64)196610)) {
        qq_decimal$var_add_dec(a,qq_decimal$dectemp(b));
    }
    else if (($temp==(i64)1048577)) {
        if (((*a).ptr == 0)) {
            qq_runaux$pcerror((byte*)"Nil+x",(byte*)"");
        }
;
        (*a).ptr += (qq_tables$ttsize[((i64)(*a).elemtag)] * (*b).value);
    }
    else {
        qq_runaux$pcmxtypes((byte*)"Addmixed",a,b);
    }
    };
    (*a).tag = newtag;
}

i64 qq_vars$var_addto(struct qq_decls$varrec *p,struct qq_decls$varrec *b) {
        struct qq_decls$varrec *  a;
        i64 newtag;
    a = (*p).varptr;
    if (((i64)(*p).tag != (i64)14)) {
        return (i64)0;
    }
;
    newtag = (i64)(*a).tag;
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        if (((newtag == (i64)9) && ((i64)(*b).tag == (i64)1))) {
            qq_strings$var_addto_string_ch(a,(*b).value);
            return (i64)1;
        }
;
        return (i64)0;
    }
;
    switch ((i64)(*a).tag) {
    case 1:;
        {
            (*a).value += (*b).value;
        }
        break;
    case 2:;
        {
            (*a).xvalue += (*b).xvalue;
        }
        break;
    case 9:;
        {
            qq_strings$var_addto_string(a,b);
        }
        break;
    case 5:;
        {
            qq_sets$var_iorto_set(a,b);
        }
        break;
    default: {
        return (i64)0;
    }
    } //SW
;
    (*a).tag = newtag;
    return (i64)1;
}

void qq_vars$var_sub(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        byte *  p;
        byte *  q;
        i64 elemsize;
        i64 x;
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        qq_vars$var_submixed(a,b);
        return;
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        (*a).value -= (*b).value;
    }
    else if (($temp==(i64)2)) {
        (*a).xvalue -= (*b).xvalue;
    }
    else if (($temp==(i64)3)) {
        qq_decimal$var_sub_dec(a,b);
    }
    else if (($temp==(i64)16)) {
        p = (*a).ptr;
        q = (*b).ptr;
                {i64 $temp = (elemsize = qq_tables$ttsize[((i64)(*a).elemtag)]);
if (($temp==(i64)1)) {
            x = (p - q);
        }
        else if (($temp==(i64)2)) {
            x = ((p - q) >> (i64)1);
        }
        else if (($temp==(i64)4)) {
            x = ((p - q) >> (i64)2);
        }
        else {
            x = ((p - q) / elemsize);
        }
        };
        (*a).tagx = (i64)1;
        (*a).value = x;
    }
    else {
        qq_runaux$pcustype_t((byte*)"sub",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_submixed(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        i64 newtag;
    newtag = (i64)(*a).tag;
        {i64 $temp = (((i64)(*a).tag << (i64)16) | (i64)(*b).tag);
if (($temp==(i64)65538)) {
        newtag = (i64)2;
        (*a).xvalue = ((r64)(*a).value - (*b).xvalue);
    }
    else if (($temp==(i64)131073)) {
        (*a).xvalue -= (r64)(*b).value;
    }
    else if (($temp==(i64)65539) || ($temp==(i64)131075)) {
        newtag = (i64)3;
        qq_decimal$var_sub_dec(qq_decimal$dectemp(a),b);
        qq_decimal$freedectemp();
    }
    else if (($temp==(i64)196609) || ($temp==(i64)196610)) {
        qq_decimal$var_sub_dec(a,qq_decimal$dectemp(b));
    }
    else if (($temp==(i64)1048577)) {
        (*a).ptr -= (qq_tables$ttsize[((i64)(*a).elemtag)] * (*b).value);
    }
    else {
        qq_runaux$pcmxtypes((byte*)"Submixed",a,b);
    }
    };
    (*a).tag = newtag;
}

void qq_vars$var_mul(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        qq_vars$var_mulmixed(a,b);
        return;
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        (*a).value *= (*b).value;
    }
    else if (($temp==(i64)2)) {
        (*a).xvalue *= (*b).xvalue;
    }
    else if (($temp==(i64)3)) {
        qq_decimal$var_mul_dec(a,b);
    }
    else if (($temp==(i64)5)) {
        qq_sets$var_dupl_set(a);
        qq_sets$var_iandto_set(a,b);
    }
    else {
        qq_runaux$pcustype_t((byte*)"mul",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_mulmixed(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        i64 newtag;
    newtag = (i64)(*a).tag;
        {i64 $temp = (((i64)(*a).tag << (i64)16) | (i64)(*b).tag);
if (($temp==(i64)65538)) {
        newtag = (i64)2;
        (*a).xvalue = ((r64)(*a).value * (*b).xvalue);
    }
    else if (($temp==(i64)131073)) {
        (*a).xvalue *= (r64)(*b).value;
    }
    else if (($temp==(i64)65539) || ($temp==(i64)131075)) {
        newtag = (i64)3;
        qq_decimal$var_mul_dec(qq_decimal$dectemp(a),b);
        qq_decimal$freedectemp();
    }
    else if (($temp==(i64)196609) || ($temp==(i64)196610)) {
        qq_decimal$var_mul_dec(a,qq_decimal$dectemp(b));
    }
    else if (($temp==(i64)589825)) {
        qq_strings$var_mul_string(a,(*b).value);
    }
    else if (($temp==(i64)655361)) {
        qq_lists$var_mul_list(a,(*b).value);
    }
    else {
        qq_runaux$pcmxtypes((byte*)"Mulmixed",a,b);
    }
    };
    (*a).tag = newtag;
}

void qq_vars$var_div(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        qq_vars$var_divmixed(a,b);
        return;
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        (*a).tagx = (i64)2;
        (*a).xvalue = ((r64)(*a).value / (r64)(*b).value);
    }
    else if (($temp==(i64)2)) {
        (*a).xvalue /= (*b).xvalue;
    }
    else if (($temp==(i64)3)) {
        qq_decimal$var_div_dec(a,b);
    }
    else {
        qq_runaux$pcustype_t((byte*)"div",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_divmixed(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        i64 newtag;
    newtag = (i64)(*a).tag;
        {i64 $temp = (((i64)(*a).tag << (i64)16) | (i64)(*b).tag);
if (($temp==(i64)65538)) {
        newtag = (i64)2;
        (*a).xvalue = ((r64)(*a).value / (*b).xvalue);
    }
    else if (($temp==(i64)131073)) {
        (*a).xvalue /= (r64)(*b).value;
    }
    else {
        qq_runaux$pcmxtypes((byte*)"Divmixed",a,b);
    }
    };
    (*a).tag = newtag;
}

void qq_vars$var_idiv(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        qq_runaux$pcerror((byte*)"idivmixed",(byte*)"");
        return;
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        if (!!((*b).value)) {
            (*a).value = ((*a).value / (*b).value);
        }
        else {
            qq_runaux$pcerror((byte*)"Divide by 0",(byte*)"");
        }
;
    }
    else if (($temp==(i64)3)) {
        qq_decimal$var_idiv_dec(a,b);
    }
    else {
        qq_runaux$pcustype_t((byte*)"idiv",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_irem(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        qq_runaux$pcerror((byte*)"iremmixed",(byte*)"");
        return;
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        (*a).value = ((*a).value % (*b).value);
    }
    else if (($temp==(i64)3)) {
        qq_decimal$var_irem_dec(a,b);
    }
    else {
        qq_runaux$pcustype_t((byte*)"irem",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_iand(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        qq_runaux$pcerror((byte*)"iand mixed",(byte*)"");
        return;
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        (*a).value &= (*b).value;
    }
    else if (($temp==(i64)5)) {
        qq_sets$var_dupl_set(a);
        qq_sets$var_iandto_set(a,b);
    }
    else {
        qq_runaux$pcustype_t((byte*)"iand",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_ior(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        qq_runaux$pcerror((byte*)"ior mixed",(byte*)"");
        return;
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        (*a).value |= (*b).value;
    }
    else if (($temp==(i64)5)) {
        qq_sets$var_dupl_set(a);
        qq_sets$var_iorto_set(a,b);
    }
    else {
        qq_runaux$pcustype_t((byte*)"ior",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_ixor(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        qq_runaux$pcerror((byte*)"ixor mixed",(byte*)"");
        return;
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        (*a).value ^= (*b).value;
    }
    else if (($temp==(i64)5)) {
        qq_sets$var_dupl_set(a);
        qq_sets$var_ixorto_set(a,b);
    }
    else {
        qq_runaux$pcustype_t((byte*)"ixor",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_shl(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        qq_runaux$pcerror((byte*)"ishl mixed",(byte*)"");
        return;
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        (*a).value <<= (*b).value;
    }
    else {
        qq_runaux$pcustype_t((byte*)"shl",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_shr(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        qq_runaux$pcerror((byte*)"ishr mixed",(byte*)"");
        return;
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        (*a).value >>= (*b).value;
    }
    else {
        qq_runaux$pcustype_t((byte*)"shr",(i64)(*a).tag);
    }
    };
}

i64 qq_vars$var_in(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        i64 n;
        {i64 $temp = (((i64)(*a).tag << (i64)16) | (i64)(*b).tag);
if (($temp==(i64)65541) || ($temp==(i64)1179653)) {
        return qq_sets$var_in_set(a,b);
    }
    else if (($temp==(i64)65540)) {
        return ((($rtemp=(*a).value, $rtemp >= msysc$m_getdotslice((*b).dummy,(i64)16,(i64)63) && $rtemp <= (i64)(*b).range_upper)) ? (i64)1 : (i64)0);
    }
    else {
                {i64 $temp = (i64)(*b).tag;
if (($temp==(i64)10) || ($temp==(i64)9) || ($temp==(i64)11) || ($temp==(i64)7)) {
            n = qq_vars$var_inx(a,b);
            return ((n != (i64)(-9223372036854775807-1)) ? (i64)1 : (i64)0);
        }
        else {
            qq_runaux$pcmxtypes((byte*)"in",a,b);
        }
        };
    }
    };
    return (i64)0;
}

i64 qq_vars$var_inx(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        {i64 $temp = (((i64)(*a).tag << (i64)16) | (i64)(*b).tag);
if (($temp==(i64)589833)) {
        return qq_strings$var_inx_string(a,b);
    }
    else if (($temp==(i64)65547) || ($temp==(i64)131083)) {
        return qq_arrays$var_inx_array(a,b,(i64)0);
    }
    else if (($temp==(i64)65543) || ($temp==(i64)131079)) {
        return qq_arrays$var_inx_array(a,b,(i64)(*(*b).objptr).usertag);
    }
    else {
                {i64 $temp = (i64)(*b).tag;
if (($temp==(i64)10)) {
            return qq_lists$var_inx_list(a,b);
        }
        else {
            qq_runaux$pcmxtypes((byte*)"inx",a,b);
        }
        };
    }
    };
    return (i64)0;
}

i64 qq_vars$var_equal(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        return qq_vars$var_equalmixed(a,b);
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1) || ($temp==(i64)14) || ($temp==(i64)16) || ($temp==(i64)18) || ($temp==(i64)17) || ($temp==(i64)19)) {
        return (i64)((*a).value == (*b).value);
    }
    else if (($temp==(i64)2)) {
        return (((*a).xvalue == (*b).xvalue) ? (i64)1 : (i64)0);
    }
    else if (($temp==(i64)3)) {
        return qq_decimal$var_equal_dec(a,b);
    }
    else if (($temp==(i64)9)) {
        return qq_strings$var_equal_string(a,b);
    }
    else if (($temp==(i64)5)) {
        return qq_sets$var_equal_set(a,b);
    }
    else if (($temp==(i64)10)) {
        return qq_lists$var_equal_list(a,b);
    }
    else if (($temp==(i64)6)) {
        return qq_dicts$var_equal_dict(a,b);
    }
    else if (($temp==(i64)11) || ($temp==(i64)7)) {
        return qq_arrays$var_equal_array(a,b);
    }
    else if (($temp==(i64)8)) {
        return qq_bits$var_equal_bits(a,b);
    }
    else if (($temp==(i64)12)) {
        return qq_records$var_equal_record(a,b);
    }
    else if (($temp==(i64)13)) {
        return qq_packed$var_equal_struct(a,b);
    }
    else {
        qq_runaux$pcustype_t((byte*)"equal",(i64)(*a).tag);
    }
    };
    return (i64)0;
}

i64 qq_vars$var_equalmixed(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        i64 result;
        {i64 $temp = (((i64)(*a).tag << (i64)16) | (i64)(*b).tag);
if (($temp==(i64)65538)) {
        return (((r64)(*a).value == (*b).xvalue) ? (i64)1 : (i64)0);
    }
    else if (($temp==(i64)131073)) {
        return (((*a).xvalue == (r64)(*b).value) ? (i64)1 : (i64)0);
    }
    else if (($temp==(i64)65539) || ($temp==(i64)131075)) {
        result = qq_decimal$var_equal_dec(qq_decimal$dectemp(a),b);
        qq_decimal$freedectemp();
        return result;
    }
    else if (($temp==(i64)196609) || ($temp==(i64)196610)) {
        return qq_decimal$var_equal_dec(a,qq_decimal$dectemp(b));
    }
    else {
        return (i64)0;
    }
    };
    return (i64)0;
}

i64 qq_vars$var_compare(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        return qq_vars$var_comparemixed(a,b);
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1) || ($temp==(i64)16)) {
        return (((*a).value < (*b).value) ? (i64)-1 : (((*a).value > (*b).value) ? (i64)1 : (i64)0));
    }
    else if (($temp==(i64)2)) {
        return (((*a).xvalue < (*b).xvalue) ? (i64)-1 : (((*a).xvalue > (*b).xvalue) ? (i64)1 : (i64)0));
    }
    else if (($temp==(i64)3)) {
        return qq_decimal$var_compare_dec(a,b);
    }
    else if (($temp==(i64)9)) {
        return qq_strings$var_compare_string(a,b);
    }
    else {
        qq_runaux$pcustype_t((byte*)"compare",(i64)(*a).tag);
    }
    };
    return (i64)0;
}

i64 qq_vars$var_comparemixed(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        {i64 $temp = (((i64)(*a).tag << (i64)16) | (i64)(*b).tag);
if (($temp==(i64)65538)) {
        return (((r64)(*a).value < (*b).xvalue) ? (i64)-1 : (((r64)(*a).value > (*b).xvalue) ? (i64)1 : (i64)0));
    }
    else if (($temp==(i64)131073)) {
        return (((*a).xvalue < (r64)(*b).value) ? (i64)-1 : (((*a).xvalue > (r64)(*b).value) ? (i64)1 : (i64)0));
    }
    else {
        qq_runaux$pcmxtypes((byte*)"comparemixed",a,b);
    }
    };
    return (i64)0;
}

void qq_vars$var_concat(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        qq_runaux$pcmxtypes((byte*)"Concat",a,b);
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)9)) {
        qq_strings$var_add_string(a,b);
    }
    else if (($temp==(i64)10)) {
        qq_lists$var_dupl_list(a);
        qq_lists$var_concatto_list(a,b);
    }
    else if (($temp==(i64)11)) {
        qq_arrays$var_dupl_array(a);
        qq_arrays$var_concatto_array(a,b);
    }
    else {
        qq_runaux$pcustype_t((byte*)"concat",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_append(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
                {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)10)) {
            goto L1340 ;
;
        }
        else if (($temp==(i64)11)) {
            goto L1341 ;
;
        }
        else if (($temp==(i64)8)) {
            goto L1342 ;
;
        }
        };
        goto L1343 ;
;
    }
    else {
        switch ((i64)(*a).tag) {
        case 9:;
            {
                qq_strings$var_add_string(a,b);
                qq_vars$var_unshareu(b);
            }
            break;
        case 10:;
            {
                //dolist:
L1340 :;
;
                qq_lists$var_dupl_list(a);
                qq_lists$var_appendto_list(a,b);
            }
            break;
        case 11:;
            {
                //doarray:
L1341 :;
;
                qq_arrays$var_dupl_array(a);
                qq_arrays$var_appendto_array(a,b);
            }
            break;
        case 8:;
            {
                //dobits:
L1342 :;
;
                qq_bits$var_dupl_bits(a);
                qq_bits$var_appendto_bits(a,b);
            }
            break;
        default: {
            //error:
L1343 :;
;
            qq_runaux$pcustype_t((byte*)"append",(i64)(*a).tag);
        }
        } //SW
;
    }
;
}

void qq_vars$var_min(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        qq_runaux$pcerror((byte*)"VARMIN",(byte*)"");
        return;
    }
;
    if ((qq_vars$var_compare(a,b) < (i64)0)) {
        if (!!((i64)(*b).hasref)) {
            qq_vars$var_unshareu(b);
        }
;
    }
    else {
        if (!!((i64)(*a).hasref)) {
            qq_vars$var_unshareu(a);
        }
;
        (*a) = (*b);
    }
;
}

void qq_vars$var_max(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        qq_runaux$pcerror((byte*)"VARMAX",(byte*)"");
        return;
    }
;
    if ((qq_vars$var_compare(a,b) >= (i64)0)) {
        if (!!((i64)(*b).hasref)) {
            qq_vars$var_unshareu(b);
        }
;
    }
    else {
        if (!!((i64)(*a).hasref)) {
            qq_vars$var_unshareu(a);
        }
;
        (*a) = (*b);
    }
;
}

i64 qq_vars$var_concatto(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        qq_runaux$pcerror((byte*)"concatto/mixed",(byte*)"");
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)9)) {
        qq_strings$var_addto_string(a,b);
    }
    else if (($temp==(i64)10)) {
        qq_lists$var_concatto_list(a,b);
    }
    else if (($temp==(i64)11)) {
        qq_arrays$var_concatto_array(a,b);
    }
    else {
        qq_runaux$pcustype((byte*)"concat",a);
    }
    };
    return (i64)1;
}

i64 qq_vars$var_appendto(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
                {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)10)) {
            goto L1344 ;
;
        }
        else if (($temp==(i64)11)) {
            goto L1345 ;
;
        }
        else if (($temp==(i64)8)) {
            goto L1346 ;
;
        }
        else {
            qq_runaux$pcerror((byte*)"appendto/mixed",(byte*)"");
        }
        };
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)9)) {
        qq_strings$var_addto_string(a,b);
        qq_vars$var_unshareu(b);
    }
    else if (($temp==(i64)10)) {
        //dolist:
L1344 :;
;
        qq_lists$var_appendto_list(a,b);
    }
    else if (($temp==(i64)11)) {
        //doarray:
L1345 :;
;
        qq_arrays$var_appendto_array(a,b);
    }
    else if (($temp==(i64)8)) {
        //dobits:
L1346 :;
;
        qq_bits$var_appendto_bits(a,b);
    }
    else {
        qq_runaux$pcustype((byte*)"append",a);
        return (i64)0;
    }
    };
    return (i64)1;
}

void qq_vars$var_getix(struct qq_decls$varrec *a,i64 index) {
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)9)) {
        qq_strings$var_getix_string(a,index);
    }
    else if (($temp==(i64)10) || ($temp==(i64)6)) {
        qq_lists$var_getix_list(a,index);
    }
    else if (($temp==(i64)11) || ($temp==(i64)7)) {
        qq_arrays$var_getix_array(a,index);
    }
    else if (($temp==(i64)8)) {
        qq_bits$var_getix_bits(a,index);
    }
    else if (($temp==(i64)5)) {
        qq_sets$var_getix_set(a,index);
    }
    else if (($temp==(i64)12)) {
        qq_records$var_getix_record(a,index);
    }
    else if (($temp==(i64)4)) {
        if ((($rtemp=index, $rtemp >= msysc$m_getdotslice((*a).dummy,(i64)16,(i64)63) && $rtemp <= (i64)(*a).range_upper))) {
            (*a).tagx = (i64)1;
            (*a).value = index;
        }
        else {
            qq_runaux$pcerror((byte*)"range/bounds",(byte*)"");
        }
;
    }
    else {
        qq_runaux$pcustype_t((byte*)"getix",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_putix(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *x) {
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)9)) {
        qq_strings$var_putix_string(a,index,x);
        qq_vars$var_unshareu(x);
    }
    else if (($temp==(i64)10)) {
        qq_lists$var_putix_list(a,index,x);
    }
    else if (($temp==(i64)11) || ($temp==(i64)7)) {
        qq_arrays$var_putix_array(a,index,x);
    }
    else if (($temp==(i64)8)) {
        qq_bits$var_putix_bits(a,index,x);
    }
    else if (($temp==(i64)5)) {
        qq_sets$var_putix_set(a,index,x);
    }
    else if (($temp==(i64)12)) {
        qq_records$var_putix_record(a,index,x);
    }
    else {
        qq_runaux$pcustype_t((byte*)"putix",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_getixref(struct qq_decls$varrec *a,i64 index) {
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)9)) {
        qq_strings$var_getixref_string(a,index);
    }
    else if (($temp==(i64)10)) {
        qq_lists$var_getixref_list(a,index);
    }
    else if (($temp==(i64)11) || ($temp==(i64)7)) {
        qq_arrays$var_getixref_array(a,index);
    }
    else if (($temp==(i64)8)) {
        qq_bits$var_getixref_bits(a,index);
    }
    else if (($temp==(i64)5)) {
        qq_sets$var_getixref_set(a,index);
    }
    else if (($temp==(i64)12)) {
        qq_records$var_getixref_record(a,index,a);
    }
    else {
        qq_runaux$pcustype_t((byte*)"getixref",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_getslice(struct qq_decls$varrec *a,i64 i,i64 j) {
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)9)) {
        qq_strings$var_getslice_string(a,i,j);
    }
    else if (($temp==(i64)10)) {
        qq_lists$var_getslice_list(a,i,j);
    }
    else if (($temp==(i64)11)) {
        qq_arrays$var_getslice_array(a,i,j);
    }
    else if (($temp==(i64)8)) {
        qq_bits$var_getslice_bits(a,i,j);
    }
    else {
        qq_runaux$pcustype_t((byte*)"getslice",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_putslice(struct qq_decls$varrec *a,i64 i,i64 j,struct qq_decls$varrec *x) {
    if (((i64)(*a).tag != (i64)(*x).tag)) {
        qq_runaux$pcerror((byte*)"putslice: not compatible",(byte*)"");
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)9)) {
        qq_strings$var_putslice_string(a,i,j,x);
    }
    else if (($temp==(i64)10)) {
        qq_lists$var_putslice_list(a,i,j,x);
    }
    else {
        qq_runaux$pcustype_t((byte*)"putslice",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_getdotix(struct qq_decls$varrec *a,i64 index) {
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        if (!((($rtemp=index, $rtemp >= (i64)0 && $rtemp <= (i64)63)))) {
            qq_runaux$pcerror((byte*)"int.[int] bounds",(byte*)"");
        }
;
        (*a).value = (((*a).value >> index) & (i64)1);
    }
    else if (($temp==(i64)9)) {
        qq_strings$var_getdotix_string(a,index);
    }
    else if (($temp==(i64)12)) {
        qq_records$var_getix_record(a,index);
    }
    else {
        qq_runaux$pcustype_t((byte*)"getdotix",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_putdotix(struct qq_decls$varrec *p,i64 index,struct qq_decls$varrec *x) {
        struct qq_decls$varrec *  a;
    if (((i64)(*p).tag == (i64)14)) {
        a = (*p).varptr;
                {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
            if (!((($rtemp=index, $rtemp >= (i64)0 && $rtemp <= (i64)63)))) {
                qq_runaux$pcerror((byte*)"int.[int]:= bounds",(byte*)"");
            }
;
            qq_vars$var_storebit((byte *)&(*a).value,index,x,(i64)33,(i64)1);
        }
        else if (($temp==(i64)9)) {
            qq_strings$var_putdotix_string(a,index,x);
        }
        else if (($temp==(i64)12)) {
            qq_records$var_putix_record(a,index,x);
        }
        else {
            qq_runaux$pcustype((byte*)"putdotix",a);
        }
        };
    }
    else {
        qq_runaux$pcustype((byte*)"putdotix",p);
    }
;
}

void qq_vars$var_getdotixref(struct qq_decls$varrec *p,i64 index) {
        struct qq_decls$varrec *  a;
    if (((i64)(*p).tag == (i64)14)) {
        a = (*p).varptr;
                {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
            if (!((($rtemp=index, $rtemp >= (i64)0 && $rtemp <= (i64)63)))) {
                qq_runaux$pcerror((byte*)"&int.[int] bounds",(byte*)"");
            }
;
            (*p).ptr = (byte *)&(*a).value;
            (*p).tagx = (i64)15;
            (*p).elemtag = (i64)33;
            (*p).bitoffset = index;
            (*p).bitlength = (i64)1;
        }
        else if (($temp==(i64)9)) {
            qq_strings$var_getdotixref_string(a,index,p);
        }
        else if (($temp==(i64)12)) {
            qq_records$var_getixref_record(a,index,p);
        }
        else {
            qq_runaux$pcustype_t((byte*)"getdotixref",(i64)(*a).tag);
        }
        };
    }
    else {
        qq_runaux$pcustype((byte*)"not refvar",p);
    }
;
}

void qq_vars$var_getdotslice(struct qq_decls$varrec *a,i64 i,i64 j) {
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        if ((i > j)) {
            {i64 temp = i; i = j; j = temp; };
        }
;
        if (((i < (i64)0) || (j > (i64)63))) {
            qq_runaux$pcerror((byte*)"int.[slice] bounds",(byte*)"");
        }
;
        (*a).value = (((*a).value >> i) & (i64)~(((u64)18446744073709551615u << ((j - i) + (i64)1))));
    }
    else if (($temp==(i64)9)) {
        qq_strings$var_getslice_string(a,i,j);
    }
    else {
        qq_runaux$pcustype_t((byte*)"getdotslice",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_putdotslice(struct qq_decls$varrec *p,i64 i,i64 j,struct qq_decls$varrec *x) {
        struct qq_decls$varrec *  a;
    if (((i64)(*p).tag == (i64)14)) {
        a = (*p).varptr;
                {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
            if ((i > j)) {
                {i64 temp = i; i = j; j = temp; };
            }
;
            if (((i < (i64)0) || (j > (i64)63))) {
                qq_runaux$pcerror((byte*)"int.[slice]:= bounds",(byte*)"");
            }
;
            qq_vars$var_storebit((byte *)&(*a).value,i,x,(i64)33,((j - i) + (i64)1));
        }
        else if (($temp==(i64)9)) {
            qq_strings$var_putslice_string(a,i,j,x);
        }
        else {
            qq_runaux$pcustype((byte*)"putdotslice",a);
        }
        };
    }
    else {
        qq_runaux$pcustype((byte*)"not ref",p);
    }
;
}

void qq_vars$var_getdotsliceref(struct qq_decls$varrec *p,i64 i,i64 j) {
        struct qq_decls$varrec *  a;
    if (((i64)(*p).tag == (i64)14)) {
        a = (*p).varptr;
                {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
            if ((i > j)) {
                {i64 temp = i; i = j; j = temp; };
            }
;
            if (((i < (i64)0) || (j > (i64)63))) {
                qq_runaux$pcerror((byte*)"&int.[slice] bounds",(byte*)"");
            }
;
            (*p).ptr = (byte *)&(*a).value;
            (*p).tagx = (i64)15;
            (*p).elemtag = (i64)33;
            (*p).bitoffset = i;
            (*p).bitlength = ((j - i) + (i64)1);
        }
        else {
            qq_runaux$pcustype((byte*)"getdotsliceref",a);
        }
        };
    }
    else {
        qq_runaux$pcustype((byte*)"not ref",p);
    }
;
}

void qq_vars$var_expand(struct qq_decls$varrec *a,struct qq_decls$varrec *dest,i64 m) {
        struct qq_decls$varrec *  b;
        struct qq_decls$varrec *  c;
        struct qq_decls$objrec *  p;
        i64 n;
        i64 length;
        i64 $av_1;
        i64 $av_2;
    if ((m < (i64)2)) {
        qq_runaux$pcerror((byte*)"Expand: LHS too few",(byte*)"");
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)10)) {
        p = (*a).objptr;
        length = (*p).length;
        //dolist:
L1347 :;
;
        b = dest;
        c = (*p).varptr;
        n = (i64)1;
        $av_1 = m;
        while ($av_1-- > 0) {
L1348 :;
            if ((n > length)) {
                (*dest).tagx = (i64)0;
            }
            else {
                (*dest) = (*c);
                if (!!((i64)(*dest).hasref)) {
                    ++((*(*dest).objptr).refcount);
                }
;
                ++(c);
            }
;
            ++(n);
            --(dest);
L1349 :;
        }
L1350 :;
        ;
    }
    else if (($temp==(i64)4)) {
        (*dest).tagx = (i64)1;
        (*dest).value = msysc$m_getdotslice((*a).dummy,(i64)16,(i64)63);
        --(dest);
        (*dest).tagx = (i64)1;
        (*dest).value = (i64)(*a).range_upper;
        $av_2 = (m - (i64)2);
        while ($av_2-- > 0) {
L1351 :;
            --(dest);
            (*dest).tagx = (i64)0;
L1352 :;
        }
L1353 :;
        ;
    }
    else if (($temp==(i64)9)) {
        qq_strings$var_expand_string(a,dest,m);
    }
    else if (($temp==(i64)12)) {
        p = (*a).objptr;
        length = qq_tables$ttlength[((i64)(*p).usertag)];
        goto L1347 ;
;
    }
    else if (($temp==(i64)11) || ($temp==(i64)7)) {
        qq_arrays$var_expand_array(a,dest,m);
    }
    else {
        qq_runaux$pcustype((byte*)"expand",a);
    }
    };
}

void qq_vars$var_inplace(i64 index,struct qq_decls$varrec *px,struct qq_decls$varrec *y) {
        void (*fnadd)(struct qq_decls$varrec *,struct qq_decls$varrec *,...);
        void (*fnaddmixed)(struct qq_decls$varrec *,struct qq_decls$varrec *,...);
        struct qq_decls$varrec x;
    if ((qq_pcltabs$bintotable[(index)-1].pclop == (i64)100)) {
        if (!!(qq_vars$var_addto(px,y))) {
            return;
        }
;
    }
;
    fnadd = (void (*)(struct qq_decls$varrec *,struct qq_decls$varrec *,...))qq_pcltabs$bintotable[(index)-1].fnadd;
    fnaddmixed = (void (*)(struct qq_decls$varrec *,struct qq_decls$varrec *,...))qq_pcltabs$bintotable[(index)-1].fnaddmixed;
    qq_vars$var_loadptr(px,&x);
    if (((i64)x.tag == (i64)(*y).tag)) {
        ((*fnadd))(&x,y);
    }
    else if (!!(fnaddmixed)) {
        ((*fnaddmixed))(&x,y);
    }
    else {
        msysc$m_print_startcon();
        msysc$m_print_str(qq_pcltabs$pclnames[(qq_pcltabs$bintotable[(index)-1].pclop)],NULL);
        msysc$m_print_newline();
        msysc$m_print_end();
        ;
        qq_runaux$pcmxtypes((byte*)"Inplace mixed",&x,y);
    }
;
    qq_vars$var_storeptr(px,&x);
}

void qq_vars$var_inplace_unary(struct qq_decls$varrec *px,void (*fnneg)(struct qq_decls$varrec *,...)) {
        struct qq_decls$varrec x;
    qq_vars$var_loadptr(px,&x);
    ((*fnneg))(&x);
    qq_vars$var_storeptr(px,&x);
}

void qq_vars$var_loadptr(struct qq_decls$varrec *x,struct qq_decls$varrec *y) {
        {i64 $temp = (i64)(*x).tag;
if (($temp==(i64)14)) {
        (*y) = (*(*x).varptr);
        if (!!((i64)(*y).hasref)) {
            ++((*(*y).objptr).refcount);
        }
;
    }
    else if (($temp==(i64)16)) {
        qq_packed$var_loadpacked((*x).ptr,(i64)(*x).elemtag,y,0);
    }
    else if (($temp==(i64)15)) {
        qq_vars$var_loadbit((*x).ptr,(i64)(*x).bitoffset,(i64)(*x).elemtag,(i64)(*x).bitlength,y);
    }
    else {
        qq_runaux$pcustype((byte*)"var_loadptr",x);
    }
    };
}

void qq_vars$var_storeptr(struct qq_decls$varrec *p,struct qq_decls$varrec *q) {
        struct qq_decls$varrec *  dest;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)14)) {
        dest = (*p).varptr;
        if (!!((i64)(*dest).hasref)) {
            qq_vars$var_unshareu(dest);
        }
;
        (*dest) = (*q);
    }
    else if (($temp==(i64)16)) {
        qq_packed$var_storepacked((*p).ptr,q,(i64)(*p).elemtag);
    }
    else if (($temp==(i64)15)) {
        qq_vars$var_storebit((*p).ptr,(i64)(*p).bitoffset,q,(i64)(*p).elemtag,(i64)(*p).bitlength);
    }
    else {
        qq_runaux$pcustype((byte*)"var_popptr",p);
    }
    };
}

void qq_vars$var_loadbit(byte *p,i64 shift,i64 t,i64 bitlength,struct qq_decls$varrec *dest) {
        u64 *  pd;
        u64 mask;
    (*dest).tagx = (i64)1;
    if ((t==(i64)33)) {
        if ((bitlength == (i64)0)) {
            (*dest).value = (i64)!!(((i64)(*p) & ((i64)1 << shift)));
        }
        else {
            pd = (u64 *)p;
            mask = (u64)18446744073709551614u;
            if ((bitlength==(i64)1)) {
            }
            else if ((bitlength==(i64)64)) {
                mask = (u64)0u;
            }
            else {
                mask <<= (bitlength - (i64)1);
            }
;
            (*dest).value = (i64)(((*pd) >> shift) & ~(mask));
        }
;
    }
    else if ((t==(i64)34)) {
        (*dest).value = (((i64)(*p) & ((i64)3 << shift)) >> shift);
    }
    else if ((t==(i64)35)) {
        (*dest).value = (((i64)(*p) & ((i64)15 << shift)) >> shift);
    }
    else {
        qq_runaux$pcustype_t((byte*)"loadbit",t);
    }
;
}

void qq_vars$var_storebit(byte *p,i64 shift,struct qq_decls$varrec *q,i64 t,i64 bitlength) {
        u64 *  pd;
        u64 mask1;
        u64 mask2;
        u64 newvalue;
    if (((i64)(*q).tag != (i64)1)) {
        qq_runaux$pcerror((byte*)"storebit not int",(byte*)"");
    }
;
    if ((t==(i64)33)) {
        if ((bitlength == (i64)0)) {
            (*p) = (((i64)(*p) & ~(((i64)1 << shift))) | (((*q).value & (i64)1) << shift));
        }
        else {
            pd = (u64 *)p;
            mask1 = (u64)18446744073709551614u;
            if ((bitlength==(i64)1)) {
            }
            else if ((bitlength==(i64)64)) {
                mask1 = (u64)0u;
            }
            else {
                mask1 <<= (bitlength - (i64)1);
            }
;
            mask1 = ~(mask1);
            if (!!(shift)) {
                mask1 <<= shift;
            }
;
            mask2 = ~(mask1);
            newvalue = (u64)(*q).value;
            if (!!(shift)) {
                newvalue <<= shift;
            }
;
            (*pd) = (((*pd) & mask2) | (newvalue & mask1));
        }
;
    }
    else if ((t==(i64)34)) {
        (*p) = (((i64)(*p) & ~(((i64)3 << shift))) | (((*q).value & (i64)3) << shift));
    }
    else if ((t==(i64)35)) {
        (*p) = (((i64)(*p) & ~(((i64)15 << shift))) | (((*q).value & (i64)15) << shift));
    }
    else {
        qq_runaux$pcustype_t((byte*)"storebit",t);
    }
;
}

void qq_vars$var_convert(struct qq_decls$varrec *x,i64 t,struct qq_decls$varrec *dest) {
        i64 s;
        i64 tbase;
        i64 aa;
    (*dest) = (*x);
    s = (i64)(*x).tag;
    if ((s == t)) {
        return;
    }
;
    tbase = t;
    (*dest).tag = t;
    if ((s==(i64)1)) {
        if ((tbase==(i64)1)) {
        }
        else if ((tbase==(i64)2)) {
            (*dest).xvalue = (r64)(*x).value;
        }
        else if ((tbase==(i64)3)) {
            qq_decimal$var_make_dec_int((*x).value,dest);
        }
        else {
            qq_runaux$pcustype_t((byte*)"conv int=>",t);
        }
;
    }
    else if ((s==(i64)2)) {
        if ((tbase==(i64)1)) {
            (*dest).value = (i64)(*x).xvalue;
        }
        else {
            qq_runaux$pcustype_t((byte*)"conv real=>",t);
        }
;
    }
    else if ((s==(i64)16) || (s==(i64)14) || (s==(i64)15)) {
                {i64 $temp = (i64)qq_tables$ttbasetype[(tbase)];
if (($temp==(i64)1)) {
        }
        else if (($temp==(i64)16)) {
            (*dest).tag = (i64)16;
            (*dest).elemtag = (i64)qq_tables$tttarget[(t)];
        }
        else {
            qq_runaux$pcustype_t((byte*)"conv ptr=>",t);
        }
        };
    }
    else if ((s==(i64)9)) {
        if ((tbase==(i64)10)) {
            qq_strings$var_convert_string_list(x,t,dest);
        }
        else if ((tbase==(i64)3)) {
            qq_decimal$var_make_dec_str((*(*x).objptr).strptr,(*(*x).objptr).length,dest);
        }
        else if ((tbase==(i64)9)) {
        }
        else {
            qq_runaux$pcustype_t((byte*)"string=>",t);
        }
;
    }
    else if ((s==(i64)18)) {
        if ((tbase != (i64)1)) {
            qq_runaux$pcustype_t((byte*)"type=>",t);
        }
;
    }
    else if ((s==(i64)3)) {
        if ((tbase==(i64)1)) {
            aa = qq_decimal$var_convert_dec_int(x);
            (*dest).tagx = (i64)1;
            (*dest).value = aa;
        }
        else {
            qq_runaux$pcustype_t((byte*)"decimal=>",t);
        }
;
    }
    else {
        qq_runaux$pcmxtypestt((byte*)"Convert s.t",s,t);
    }
;
}

i64 qq_vars$var_gethashvalue(struct qq_decls$varrec *p) {
        i64 hsum;
        i64 c;
        i64 n;
        i64 result;
        u8 *  s;
        struct qq_decls$objrec *  q;
        i64 $av_1;
        {i64 $temp = (i64)(*p).tag;
if (($temp==(i64)9)) {
        n = (*(*p).objptr).length;
        if (!(!!(n))) {
            return (i64)0;
        }
;
        hsum = (i64)0;
        s = (*(*p).objptr).strptr;
        $av_1 = n;
        while ($av_1-- > 0) {
L1354 :;
            c = (i64)(u64)(*(s)++);
            hsum = (((hsum << (i64)4) - hsum) + c);
L1355 :;
        }
L1356 :;
        ;
        result = ((hsum << (i64)5) - hsum);
        return (result & (i64)9223372036854775807);
    }
    else if (($temp==(i64)1) || ($temp==(i64)2) || ($temp==(i64)4)) {
        return (*p).value;
    }
    else if (($temp==(i64)3)) {
        q = (*p).objptr;
        if (((*q).length == (i64)0)) {
            return (i64)0;
        }
        else {
            return (i64)(*(*q).num)[((i64)0)];
        }
;
    }
    else if (($temp==(i64)12)) {
        return (i64)(*p).objptr;
    }
    else {
        qq_runaux$pcustype((byte*)"Can't hash:",p);
    }
    };
    return (i64)0;
}

void qq_vars$var_objtovar(i64 tag,struct qq_decls$objrec *p,struct qq_decls$varrec *q) {
    (*q).tagx = (tag | (i64)256);
    (*q).objptr = p;
}

void qq_vars$var_putdotix_intint(struct qq_decls$varrec *a,i64 index,struct qq_decls$varrec *b) {
        u64 x;
        u64 y;
    x = (u64)(*a).value;
    y = (u64)(*b).value;
    if (!((($rtemp=index, $rtemp >= (i64)0 && $rtemp <= (i64)63)))) {
        qq_runaux$pcerror((byte*)"int.[int]:= bounds",(byte*)"");
    }
;
    (*a).value = (((i64)x & ~(((i64)1 << index))) | (i64)(y << index));
}

void qq_vars$var_power(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
    if (((i64)(*a).tag != (i64)(*b).tag)) {
        qq_vars$var_powermixed(a,b);
        return;
    }
;
        {i64 $temp = (i64)(*a).tag;
if (($temp==(i64)1)) {
        (*a).value = msysc$m_power_i64((*b).value,(*a).value);
    }
    else if (($temp==(i64)2)) {
        (*a).xvalue = pow((*a).xvalue,(*b).xvalue);
    }
    else if (($temp==(i64)3)) {
        qq_decimal$var_power_dec(a,qq_decimal$var_convert_dec_int(b));
    }
    else {
        qq_runaux$pcustype_t((byte*)"power",(i64)(*a).tag);
    }
    };
}

void qq_vars$var_powermixed(struct qq_decls$varrec *a,struct qq_decls$varrec *b) {
        i64 newtag;
    newtag = (i64)(*a).tag;
        {i64 $temp = (((i64)(*a).tag << (i64)16) | (i64)(*b).tag);
if (($temp==(i64)65538)) {
        newtag = (i64)2;
        (*a).xvalue = pow((r64)(*a).value,(*b).xvalue);
    }
    else if (($temp==(i64)131073)) {
        (*a).xvalue = pow((*a).xvalue,(r64)(*b).value);
    }
    else if (($temp==(i64)196609)) {
        qq_decimal$var_power_dec(a,(*b).value);
    }
    else {
        qq_runaux$pcmxtypes((byte*)"Powermixed",a,b);
    }
    };
    (*a).tag = newtag;
}

// START
void qq_vars$start(void) {

}

void msysc$m_init(i64 nargs,u8 *(*args)[]) {
        i64 j;
        i64 i;
    msysc$nsysparams = nargs;
    if ((msysc$nsysparams > (i64)128)) {
        printf((byte*)"Too many params\n");
        exit((i64)1);
    }
;
    for (i=(i64)1;i<=nargs;++i) {
L1357 :;
        msysc$sysparams[(i)-1] = (*args)[(i)-1];
L1358 :;
    }
L1359 :;
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
        msysc$strtofmt(fmtstyle,(i64)-1,(struct msysc$fmtrec *)&fmt);
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
        msysc$strtofmt(fmtstyle,(i64)-1,(struct msysc$fmtrec *)&fmt);
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
        msysc$strtofmt(fmtstyle,(i64)-1,(struct msysc$fmtrec *)&fmt);
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
        msysc$strtofmt(fmtstyle,(i64)-1,(struct msysc$fmtrec *)&fmt);
        msysc$tostr_str(s,(struct msysc$fmtrec *)&fmt);
    }
;
    msysc$needgap = (i64)1;
}

void msysc$m_print_newline(void) {
    msysc$needgap = (i64)0;
    msysc$nextfmtchars((i64)1);
    msysc$printstr((byte*)"\n");
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
    L1360 :;
    while (1) {
        c = (u64)(*msysc$fmtstr);
        switch ((i64)(u64)c) {
        case 35:;
            {
                if (!!(lastx)) {
                    goto L1362 ;
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
L1362 :;
;
            ++(n);
            ++(msysc$fmtstr);
        }
        } //SW
;
    }
L1361 :;
    ;
}

void msysc$strtofmt(u8 *s,i64 slen,struct msysc$fmtrec *fmt) {
        i64 c;
        i64 base;
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
    L1363 :;
    while (!!((u64)(*s))) {
        c = (i64)(u64)(*s);
        ++(s);
        if ((c == (i64)65)) {
            (*fmt).lettercase = 'A';
        }
        else if ((c == (i64)97)) {
            (*fmt).lettercase = 'a';
        }
        else {
            switch (toupper((i32)c)) {
            case 66:;
                {
                    (*fmt).base = (i64)2;
                }
                break;
            case 72:;
                {
                    (*fmt).base = (i64)16;
                }
                break;
            case 79:;
                {
                    (*fmt).base = (i64)8;
                }
                break;
            case 88:;
                {
                    base = (i64)0;
                    L1366 :;
                    while (1) {
                        c = (i64)(u64)(*s);
                        if ((($rtemp=c, $rtemp >= (i64)48 && $rtemp <= (i64)57))) {
                            base = (((base * (i64)10) + c) - (i64)48);
                            ++(s);
                        }
                        else {
                            goto L1367 ;
                        }
;
                    }
L1367 :;
                    ;
                    if ((($rtemp=base, $rtemp >= (i64)2 && $rtemp <= (i64)16))) {
                        (*fmt).base = base;
                    }
;
                }
                break;
            case 81:;
                {
                    (*fmt).quotechar = '"';
                }
                break;
            case 74:;
                {
                    (*fmt).justify = (u64)toupper((i32)(u64)(*s));
                    if (!!((u64)(*s))) {
                        ++(s);
                    }
;
                }
                break;
            case 90:;
                {
                    (*fmt).padchar = '0';
                }
                break;
            case 83:;
                {
                    (*fmt).sepchar = (u64)(*s);
                    if (!!((u64)(*s))) {
                        ++(s);
                    }
;
                }
                break;
            case 80:;
                {
                    (*fmt).padchar = (u64)(*s);
                    if (!!((u64)(*s))) {
                        ++(s);
                    }
;
                }
                break;
            case 84:;
                {
                    (*fmt).suffix = (u64)(*s);
                    if (!!((u64)(*s))) {
                        ++(s);
                    }
;
                }
                break;
            case 85:;
                {
                    (*fmt).usigned = 'W';
                }
                break;
            case 69:;
                {
                    (*fmt).realfmt = 'e';
                }
                break;
            case 70:;
                {
                    (*fmt).realfmt = 'f';
                }
                break;
            case 71:;
                {
                    (*fmt).realfmt = 'g';
                }
                break;
            case 68:;
                {
                    (*fmt).charmode = 'D';
                }
                break;
            case 67:;
                {
                    (*fmt).charmode = 'C';
                }
                break;
            case 77:;
                {
                    (*fmt).heapmode = 'M';
                }
                break;
            case 86:;
                {
                    (*fmt).param = 'V';
                }
                break;
            case 89:;
                {
                    (*fmt).spare = msysc$m_setdotindex((*fmt).spare,(i64)0,(u64)1u);
                }
                break;
            case 78:;
                {
                    (*fmt).spare = msysc$m_setdotindex((*fmt).spare,(i64)1,(u64)1u);
                }
                break;
            default: {
                if ((c==(i64)46)) {
                    wset = (i64)1;
                }
                else if ((c==(i64)44) || (c==(i64)95)) {
                    (*fmt).sepchar = (u64)c;
                }
                else if ((c==(i64)43)) {
                    (*fmt).plus = '+';
                }
                else if ((c==(i64)126)) {
                    (*fmt).quotechar = '~';
                }
                else if ((c==(i64)42)) {
                    n = msysc$fmtparam;
                    goto L1368 ;
;
                }
                else {
                    if (((c >= (i64)48) && (c <= (i64)57))) {
                        n = (c - (i64)48);
                        L1369 :;
                        while (1) {
                            c = (i64)(u64)(*s);
                            if (((i64)(u64)(*s) == (i64)0)) {
                                goto L1370 ;
                            }
;
                            if (((c >= (i64)48) && (c <= (i64)57))) {
                                ++(s);
                                n = (((n * (i64)10) + c) - (i64)48);
                            }
                            else {
                                goto L1370 ;
                            }
;
                        }
L1370 :;
                        ;
                        //gotwidth:
L1368 :;
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
;
            }
            } //SW
;
        }
;
L1364 :;
    }
L1365 :;
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
L1371 :;
        if (((i64)(u64)(*p) == (i64)0)) {
            goto L1373 ;
        }
;
        (*q) = (u64)(*p);
        ++(q);
        ++(p);
L1372 :;
    }
L1373 :;
    ;
    (*q) = (u64)0u;
    return msysc$expandstr((u8 *)str,dest,strlen((u8 *)str),(struct msysc$fmtrec *)fmt);
}

i64 msysc$expandstr(u8 *s,u8 *t,i64 n,struct msysc$fmtrec *fmt) {
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
L1374 :;
            (*t) = (u64)(*fmt).padchar;
            ++(t);
L1375 :;
        }
L1376 :;
        ;
        (*t) = (u64)0u;
    }
    else if (((u64)(*fmt).justify == 'R')) {
        if (((((u64)(*fmt).padchar == '0') && !!((i64)(*fmt).base)) && (((u64)(*s) == '-') || ((u64)(*s) == '+')))) {
            (*t) = (u64)(*s);
            ++(t);
            $av_2 = (w - n);
            while ($av_2-- > 0) {
L1377 :;
                (*t) = (u64)(*fmt).padchar;
                ++(t);
L1378 :;
            }
L1379 :;
            ;
            strncpy(t,(s + (i64)1),(u64)(n - (i64)1));
            (*((t + n) - (i64)1)) = (u64)0u;
        }
        else {
            $av_3 = (w - n);
            while ($av_3-- > 0) {
L1380 :;
                (*t) = (u64)(*fmt).padchar;
                ++(t);
L1381 :;
            }
L1382 :;
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
L1383 :;
            (*t) = (u64)(*fmt).padchar;
            ++(t);
L1384 :;
        }
L1385 :;
        ;
        strncpy(t,s,(u64)n);
        t += n;
        $av_5 = ((w - n) - m);
        while ($av_5-- > 0) {
L1386 :;
            (*t) = (u64)(*fmt).padchar;
            ++(t);
L1387 :;
        }
L1388 :;
        ;
        (*t) = (u64)0u;
    }
;
    return w;
}

static i64 msysc$u64tostr(u64 aa,u8 *s,u64 base,i64 sep) {
        u8 t[1024];
        i64 i;
        i64 j;
        i64 k;
        i64 g;
        u8 *  s0;
    i = (i64)0;
    k = (i64)0;
    g = (((i64)base == (i64)10) ? (i64)3 : (i64)4);
    L1389 :;
    do {
        t[(++(i))] = (u64)msysc$digits[((i64)(aa % base))];
        aa = (aa / base);
        ++(k);
        if (((!!(sep) && ((i64)aa != (i64)0)) && (k == g))) {
            t[(++(i))] = (u64)sep;
            k = (i64)0;
        }
;
L1390 :;
    }
    while (!((i64)aa == (i64)0));
L1391 :;
    ;
    j = i;
    s0 = s;
    L1392 :;
    while (!!(i)) {
        (*s) = (u64)t[((i)--)];
        ++(s);
L1393 :;
    }
L1394 :;
    ;
    (*s) = (u64)0u;
    return j;
}

i64 msysc$i64tostrfmt(i64 aa,u8 *s,struct msysc$fmtrec *fmt) {
        u8 str[1024];
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

i64 msysc$u64tostrfmt(i64 aa,u8 *s,struct msysc$fmtrec *fmt) {
        u8 str[1024];
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
        u8 t[1024];
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
    L1395 :;
    while (!!(i)) {
        --(s);
        (*s) = (u64)t[(((i)-- - (i64)1))];
        if (((!!(sep) && !!(i)) && (++(k) == g))) {
            --(s);
            (*s) = (u64)sep;
            k = (i64)0;
        }
;
L1396 :;
    }
L1397 :;
    ;
    return strlen(s);
}

i64 msysc$strtostrfmt(u8 *s,u8 *t,i64 n,struct msysc$fmtrec *fmt) {
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
        msysc$strtofmt(fmtstyle,(i64)-1,(struct msysc$fmtrec *)&fmt);
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
    L1398 :;
    while ((((u64)(*s) == ' ') || ((i64)(u64)(*s) == (i64)9))) {
        ++(s);
L1399 :;
    }
L1400 :;
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
    L1401 :;
    while (!!((u64)(*s))) {
        c = (u64)(*(s)++);
        switch ((i64)(u64)c) {
        case 32:;
        case 9:;
        case 44:;
        case 61:;
            {
                if ((!!((u64)quotechar) || (p == s))) {
                    goto L1404 ;
;
                }
;
                msysc$termchar = (i64)(u64)c;
                goto L1403 ;
            }
            break;
        default: {
            //normalchar:
L1404 :;
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
                    goto L1403 ;
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
L1402 :;
    }
L1403 :;
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
    L1405 :;
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
                goto L1406 ;
            }
            break;
        default: {
            msysc$itemerror = (i64)1;
            goto L1407 ;
        }
        } //SW
;
        if (((i64)(u64)d >= base)) {
            msysc$itemerror = (i64)1;
            goto L1407 ;
        }
;
        aa = (u64)(((i64)aa * base) + (i64)(u64)d);
L1406 :;
    }
L1407 :;
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
L1408 :;
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L1409 :;
    }
L1410 :;
    ;
}

static void msysc$iconvucn(u8 *s,i64 n) {
        i64 $av_1;
    $av_1 = n;
    while ($av_1-- > 0) {
L1411 :;
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L1412 :;
    }
L1413 :;
    ;
}

static void msysc$convlcstring(u8 *s) {
    L1414 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L1415 :;
    }
L1416 :;
    ;
}

static void msysc$convucstring(u8 *s) {
    L1417 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L1418 :;
    }
L1419 :;
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
L1420 :;
        j = (i64)1;
        k = (i64)16;
        L1423 :;
        while ((i > k)) {
            k = (k << (i64)1);
            ++(j);
L1424 :;
        }
L1425 :;
        ;
        mlib$sizeindextable[(i)] = j;
L1421 :;
    }
L1422 :;
    ;
    mlib$allocupper[((i64)1)] = (u64)16u;
    size = (i64)16;
    for (i=(i64)2;i<=(i64)27;++i) {
L1426 :;
        size *= (i64)2;
        mlib$allocupper[(i)] = (u64)size;
        if ((size >= (i64)33554432)) {
            k = i;
            goto L1428 ;
        }
;
L1427 :;
    }
L1428 :;
    ;
    for (i=(k + (i64)1);i<=(i64)300;++i) {
L1429 :;
        size += (i64)33554432;
        if ((size < (i64)8589934592)) {
            mlib$allocupper[(i)] = (u64)size;
            mlib$maxmemory = (u64)size;
        }
        else {
            mlib$maxalloccode = (i - (i64)1);
            goto L1431 ;
        }
;
L1430 :;
    }
L1431 :;
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
    memset(p,(i32)(i64)0,(u64)2097152u);
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
        L1432 :;
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
L1433 :;
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
    L1434 :;
    while (((p >= buffer) && (((i64)(u64)(*p) == (i64)13) || ((i64)(u64)(*p) == (i64)10)))) {
        if ((((i64)(u64)(*p) == (i64)13) || ((i64)(u64)(*p) == (i64)10))) {
            crseen = (i64)1;
        }
;
        (*(p)--) = (u64)0u;
L1435 :;
    }
L1436 :;
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
L1437 :;
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L1438 :;
    }
L1439 :;
    ;
}

void mlib$iconvucn(u8 *s,i64 n) {
        i64 $av_1;
    $av_1 = n;
    while ($av_1-- > 0) {
L1440 :;
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L1441 :;
    }
L1442 :;
    ;
}

u8 *mlib$convlcstring(u8 *s) {
        u8 *  s0;
    s0 = s;
    L1443 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)tolower((i32)(u64)(*s));
        ++(s);
L1444 :;
    }
L1445 :;
    ;
    return s0;
}

u8 *mlib$convucstring(u8 *s) {
        u8 *  s0;
    s0 = s;
    L1446 :;
    while (!!((u64)(*s))) {
        (*s) = (u64)toupper((i32)(u64)(*s));
        ++(s);
L1447 :;
    }
L1448 :;
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
    L1449 :;
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
L1450 :;
    }
L1451 :;
    ;
    return (byte*)"";
}

u8 *mlib$extractpath(u8 *s) {
        static u8 str[260];
        u8 *  t;
        i64 n;
    t = ((s + strlen(s)) - (i64)1);
    L1452 :;
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
L1453 :;
    }
L1454 :;
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
L1455 :;
            str[((slen + i))-1] = (u64)padch;
L1456 :;
        }
L1457 :;
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
L1458 :;
        str[(i)-1] = (u64)ch;
L1459 :;
    }
L1460 :;
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
L1461 :;
;
    (*value) = 0;
    (*name) = 0;
    if (!!(infile)) {
        if ((mlib$readnextfileitem(&fileptr,&item) == (i64)0)) {
            mlib$pcm_free((void *)filestart,atsize);
            infile = (i64)0;
            goto L1461 ;
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
            goto L1461 ;
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
L1462 :;
;
    L1463 :;
    while (1) {
                {u64 $temp = (u64)(*p);
if (($temp==' ') || ($temp==(u64)9u) || ($temp==(u64)13u) || ($temp==(u64)10u)) {
            ++(p);
        }
        else if (($temp==(u64)26u) || ($temp==(u64)0u)) {
            return (i64)0;
        }
        else {
            goto L1464 ;
        }
        };
    }
L1464 :;
    ;
        {u64 $temp = (u64)(*p);
if (($temp=='!') || ($temp=='#')) {
        ++(p);
        L1465 :;
                {u64 $temp = (u64)(*(p)++);
if (($temp==(u64)10u)) {
            goto L1462 ;
;
        }
        else if (($temp==(u64)26u) || ($temp==(u64)0u)) {
            (*fileptr) = (p - (i64)1);
            return (i64)0;
        }
        else {
        }
        }goto L1465 ;
L1466 :;
        ;
    }
    };
        {u64 $temp = (u64)(*p);
if (($temp=='"')) {
        pstart = ++(p);
        L1467 :;
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
                goto L1468 ;
            }
            };
            ++(p);
        }
L1468 :;
        ;
    }
    else {
        pstart = p;
        L1469 :;
        while (1) {
                        {u64 $temp = (u64)(*p);
if (($temp==(u64)0u) || ($temp==(u64)26u)) {
                pend = p;
                goto L1470 ;
            }
            else if (($temp==' ') || ($temp==(u64)9u) || ($temp==',') || ($temp==(u64)13u) || ($temp==(u64)10u)) {
                pend = (p)++;
                goto L1470 ;
            }
            };
            ++(p);
        }
L1470 :;
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
L1471 :;
        strcat(s,padchar);
L1472 :;
    }
L1473 :;
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
    L1474 :;
    do {
        x = ((r64)mlib$mrandomp() / (double)9223372036854775800.);
L1475 :;
    }
    while (!(x != (double)1.));
L1476 :;
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
L1477 :;
        if (!!(mlib$eqstring(msysc$m_get_procname(i),name))) {
            return msysc$m_get_procaddr(i);
        }
;
L1478 :;
    }
L1479 :;
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

void *mlib$pcm_allocnfz(i64 n) {
        byte *  p;
    if (!!((n & (i64)7))) {
        n = (n + ((i64)8 - (n & (i64)7)));
    }
;
    p = mlib$pcheapptr;
    mlib$pcheapptr += n;
    if ((mlib$pcheapptr >= mlib$pcheapend)) {
        p = (byte *)mlib$pcm_newblock(n);
    }
;
    return p;
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

i64 mlinux$dirlist(u8 *filespec,u8 *(*dest)[],i64 capacity,i64 t) {
    return (i64)0;
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

