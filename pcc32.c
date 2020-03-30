/* PCL Interpreter: execute .pc bytecode files produced by Q compiler

   Compile pcc64.c or pcc32.c C program, examples:

   gcc -m64 -O3 pcc64.c -opcc.exe             # Windows
   gcc -m64 -O3 pcc64.c -opcc -lm             # Linux

   See comments block at end of file for more details on building at
   testing.

   Any comments to bart4858@gmail.com.
*/

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

typedef int8_t int8;
typedef int16_t int16;
typedef int32_t int32;
typedef int64_t int64;

typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;
typedef uint64_t uint64;

typedef unsigned char byte;
typedef unsigned char uchar;
typedef char* ichar;

#ifdef __GNUC__
#define gcc_callback __attribute__ ((force_align_arg_pointer))
#else
#define gcc_callback
#endif

int64 m_imin(int64,int64);
int64 m_imax(int64,int64);
uint64 m_min(uint64,uint64);
uint64 m_max(uint64,uint64);

// From module: mm_clib
/* Named Constants */
enum {c_eof = -1};
enum {seek_set = 0};
enum {seek_curr = 1};
enum {seek_end = 2};

// From module: mm_mainc
/* Named Constants */
enum {maxparam = 32};

// From module: mm_nos

// From module: mm_mlib
/* Named Constants */
enum {mem_check = 0};
enum {threshold = 33554432};
enum {alloc_step = 33554432};
enum {maxmemalloc = 100000};
enum {pcheapsize = 2097152};
enum {maxblockindex = 8};
enum {maxblocksize = 2048};
enum {size16 = 1};
enum {size32 = 2};
enum {size64 = 3};
enum {size128 = 4};
enum {size256 = 5};
enum {size512 = 6};
enum {size1024 = 7};
enum {size2048 = 8};

// From module: var_types
/* Named Constants */
enum {tvoid = 0};
enum {tint = 1};
enum {tword = 2};
enum {treal = 3};
enum {trange = 4};
enum {tstring = 5};
enum {twstring = 6};
enum {tlongint = 7};
enum {trational = 8};
enum {tset = 9};
enum {tdict = 10};
enum {tword128 = 11};
enum {tenum = 12};
enum {ttype = 13};
enum {toperator = 14};
enum {tsymbol = 15};
enum {tretaddr = 16};
enum {texception = 17};
enum {trefproc = 18};
enum {trefdllproc = 19};
enum {treflabel = 20};
enum {tstringz = 21};
enum {trefvar = 22};
enum {trefpacked = 23};
enum {trefbit = 24};
enum {trecordlink = 25};
enum {treflist = 26};
enum {trefarray = 27};
enum {trefbits = 28};
enum {tlist = 29};
enum {tarray = 30};
enum {tbits = 31};
enum {trecord = 32};
enum {tstruct = 33};
enum {tuser = 34};
enum {tvariant = 35};
enum {tc8 = 36};
enum {ti8 = 37};
enum {ti16 = 38};
enum {ti32 = 39};
enum {ti64 = 40};
enum {tbit = 41};
enum {tbit2 = 42};
enum {tbit4 = 43};
enum {tu8 = 44};
enum {tu16 = 45};
enum {tu32 = 46};
enum {tu64 = 47};
enum {tr32 = 48};
enum {tr64 = 49};
enum {tintm = 50};
enum {twordm = 51};
enum {trefm = 52};
enum {tlast = 53};

// From module: pq_common
/* Named Constants */
enum {cnone = 0};
enum {cmemory = 1};
enum {cframe = 2};
enum {cproc = 3};
enum {cdllproc = 4};
enum {cdllvar = 5};
enum {cfield = 6};
enum {cgenfield = 7};
enum {clabel = 8};
enum {cint32 = 9};
enum {cint = 10};
enum {cword = 11};
enum {creal = 12};
enum {crange = 13};
enum {cstring = 14};
enum {ctype = 15};
enum {coperator = 16};
enum {clast = 17};
enum {m = cmemory};
enum {f = cframe};
enum {p = cproc};
enum {x = cdllproc};
enum {v = cdllvar};
enum {g = cgenfield};
enum {l = clabel};
enum {i = cint};
enum {h = cint32};
enum {u = cword};
enum {r = creal};
enum {n = crange};
enum {s = cstring};
enum {t = ctype};
enum {o = coperator};
enum {kzero = 0};
enum {knop = 1};
enum {kprocstart = 2};
enum {kprocend = 3};
enum {kendmodule = 4};
enum {kpush_m = 5};
enum {kpush_f = 6};
enum {kpush_am = 7};
enum {kpush_af = 8};
enum {kpush_ap = 9};
enum {kpush_al = 10};
enum {kpush_ci = 11};
enum {kpush_cw = 12};
enum {kpush_cr = 13};
enum {kpush_cn = 14};
enum {kpush_cs = 15};
enum {kpush_t = 16};
enum {kpush_op = 17};
enum {kpushz = 18};
enum {kpushz_void = 19};
enum {kpushz_str = 20};
enum {kpushz_list = 21};
enum {kpushz_listl = 22};
enum {kpushz_set = 23};
enum {kpushz_arrayl = 24};
enum {kpop_m = 25};
enum {kpop_f = 26};
enum {kstore_m = 27};
enum {kstore_f = 28};
enum {kpushptr = 29};
enum {kpopptr = 30};
enum {kstoreptr = 31};
enum {kzpop_m = 32};
enum {kzpop_f = 33};
enum {kzstore_m = 34};
enum {kzstore_f = 35};
enum {kcopy = 36};
enum {kswap = 37};
enum {kconvptr = 38};
enum {kjump = 39};
enum {kjumpptr = 40};
enum {kjumptrue = 41};
enum {kjumpfalse = 42};
enum {kjumpdef = 43};
enum {kjumpvoid = 44};
enum {kjumpeq = 45};
enum {kjumpne = 46};
enum {kjumplt = 47};
enum {kjumple = 48};
enum {kjumpge = 49};
enum {kjumpgt = 50};
enum {kjumptesteq = 51};
enum {kjumptestne = 52};
enum {kjumplabel = 53};
enum {kjumpclabel = 54};
enum {kswitch = 55};
enum {kcswitch = 56};
enum {knew = 57};
enum {kto_f = 58};
enum {kfor_fci = 59};
enum {kfor_ff = 60};
enum {kford_fci = 61};
enum {kford_ff = 62};
enum {kcall = 63};
enum {kcallptr = 64};
enum {kreturn = 65};
enum {kstartdll = 66};
enum {kpushdll = 67};
enum {kcalldll = 68};
enum {kcallhost = 69};
enum {kstackframe = 70};
enum {kfree = 71};
enum {kaddsp = 72};
enum {kstop = 73};
enum {ktest = 74};
enum {kmakelist = 75};
enum {kmakerecord = 76};
enum {kmakearray = 77};
enum {kmakestruct = 78};
enum {kmakeset = 79};
enum {kmakerange = 80};
enum {kmakedict = 81};
enum {kpushdot = 82};
enum {kpushdotref = 83};
enum {ksoftconv = 84};
enum {khardconv = 85};
enum {kmixed = 86};
enum {kincrptr = 87};
enum {kincrto_m = 88};
enum {kincrto_f = 89};
enum {kloadincr = 90};
enum {kincrload = 91};
enum {kdecrptr = 92};
enum {kdecrto_m = 93};
enum {kdecrto_f = 94};
enum {kloaddecr = 95};
enum {kdecrload = 96};
enum {kincr = 97};
enum {kdecr = 98};
enum {kneg = 99};
enum {kabs = 100};
enum {knot = 101};
enum {kinot = 102};
enum {kistrue = 103};
enum {kasc = 104};
enum {kchr = 105};
enum {ksqrt = 106};
enum {ksqr = 107};
enum {kcube = 108};
enum {ksin = 109};
enum {kcos = 110};
enum {ktan = 111};
enum {kasin = 112};
enum {kacos = 113};
enum {katan = 114};
enum {ksign = 115};
enum {kln = 116};
enum {klog = 117};
enum {klg = 118};
enum {kexp = 119};
enum {kround = 120};
enum {kfloor = 121};
enum {kceil = 122};
enum {kfract = 123};
enum {knegto = 124};
enum {kabsto = 125};
enum {knotto = 126};
enum {kinotto = 127};
enum {klen = 128};
enum {klwb = 129};
enum {kupb = 130};
enum {kbounds = 131};
enum {kbits = 132};
enum {kbytes = 133};
enum {ktype = 134};
enum {kelemtype = 135};
enum {kbasetype = 136};
enum {kminval = 137};
enum {kmaxval = 138};
enum {kisint = 139};
enum {kisreal = 140};
enum {kisstring = 141};
enum {kisrange = 142};
enum {kisnumber = 143};
enum {kisarray = 144};
enum {kisrecord = 145};
enum {kispointer = 146};
enum {kismutable = 147};
enum {kisset = 148};
enum {kisvoid = 149};
enum {kisdef = 150};
enum {ktostr = 151};
enum {kisequal = 152};
enum {kadd = 153};
enum {ksub = 154};
enum {kmul = 155};
enum {kdiv = 156};
enum {kidiv = 157};
enum {krem = 158};
enum {kdivrem = 159};
enum {kiand = 160};
enum {kior = 161};
enum {kixor = 162};
enum {kshl = 163};
enum {kshr = 164};
enum {kin = 165};
enum {knotin = 166};
enum {kinrev = 167};
enum {keq = 168};
enum {kne = 169};
enum {klt = 170};
enum {kle = 171};
enum {kge = 172};
enum {kgt = 173};
enum {kmin = 174};
enum {kmax = 175};
enum {kconcat = 176};
enum {kappend = 177};
enum {kpower = 178};
enum {katan2 = 179};
enum {kaddto = 180};
enum {ksubto = 181};
enum {kmulto = 182};
enum {kdivto = 183};
enum {kidivto = 184};
enum {kiandto = 185};
enum {kiorto = 186};
enum {kixorto = 187};
enum {kshlto = 188};
enum {kshrto = 189};
enum {kminto = 190};
enum {kmaxto = 191};
enum {kconcatto = 192};
enum {kappendto = 193};
enum {kpushix = 194};
enum {kpushdotix = 195};
enum {kpushkeyix = 196};
enum {kpushkeyixd = 197};
enum {kpushixref = 198};
enum {kpushdotixref = 199};
enum {kpushkeyixref = 200};
enum {kpushbyteix = 201};
enum {kpushbyteixref = 202};
enum {kappendset = 203};
enum {kpushdotm = 204};
enum {kpushdott = 205};
enum {kpush_ad = 206};
enum {kpush_try = 207};
enum {kraise = 208};
enum {kapplyop = 209};
enum {kmakeiter = 210};
enum {kforall = 211};
enum {kforallx = 212};
enum {kforeach = 213};
enum {kforeachx = 214};
enum {kexpandrange = 215};
enum {klastcmd = 216};
enum {kkpclversion = 1};
enum {kkmoduletable = 2};
enum {kkdlltable = 3};
enum {kkdllproctable = 4};
enum {kksymboltable = 5};
enum {kktypetable = 6};
enum {kkgenfieldnames = 7};
enum {kkgenfielddata = 8};
enum {kkstringtable = 9};
enum {kkstructtable = 10};
enum {kkpccode = 11};
enum {kkend = 12};
enum {host_dummy = 0};
enum {host_startprint = 1};
enum {host_startprintcon = 2};
enum {host_strstartprint = 3};
enum {host_setformat = 4};
enum {host_endprint = 5};
enum {host_strendprint = 6};
enum {host_print = 7};
enum {host_dprint = 8};
enum {host_println = 9};
enum {host_printnogap = 10};
enum {host_readln = 11};
enum {host_sreadln = 12};
enum {host_sread = 13};
enum {host_rereadln = 14};
enum {host_reread = 15};
enum {host_strtoval = 16};
enum {host_tostr = 17};
enum {host_leftstr = 18};
enum {host_rightstr = 19};
enum {host_convlc = 20};
enum {host_convuc = 21};
enum {host_iconvlc = 22};
enum {host_iconvuc = 23};
enum {host_stop = 24};
enum {host_stopx = 25};
enum {host_ismain = 26};
enum {host_waitkey = 27};
enum {host_testkey = 28};
enum {host_execwait = 29};
enum {host_execcmd = 30};
enum {host_shellexec = 31};
enum {host_system = 32};
enum {host_makestr = 33};
enum {host_makestrslice = 34};
enum {host_makeref = 35};
enum {host_new = 36};
enum {host_newheap = 37};
enum {host_readlines = 38};
enum {host_heapvar = 39};
enum {host_dictitems = 40};
enum {host_freeheap = 41};
enum {host_setoverload = 42};
enum {host_getcmdparam = 43};
enum {host_gethostname = 44};
enum {host_setpcerror = 45};
enum {host_setdebug = 46};
enum {host_test = 47};
enum {host_ticks = 48};
enum {host_sleep = 49};
enum {host_random = 50};
enum {host_findmetafunction = 51};
enum {host_gethash = 52};
enum {host_getos = 53};
enum {host_gethostsize = 54};
enum {host_iswindows = 55};
enum {host_setmesshandler = 56};
enum {host_setfprintf = 57};
enum {host_loadpcl = 58};
enum {host_runpcl = 59};
enum {host_runtask = 60};
enum {host_callext = 61};
enum {host_pcldata = 62};
enum {host_getcstring = 63};
enum {host_getparam = 64};
enum {host_clearlist = 65};
enum {host_makelink = 66};
enum {host_allparams = 67};
enum {host_stackvars = 68};
enum {host_makeempty = 69};
enum {host_errorinfo = 70};
enum {host_last = 71};
enum {nullid = 0};
enum {programid = 1};
enum {moduleid = 2};
enum {extmoduleid = 3};
enum {dllmoduleid = 4};
enum {typeid = 5};
enum {procid = 6};
enum {dllprocid = 7};
enum {dllvarid = 8};
enum {constid = 9};
enum {staticid = 10};
enum {frameid = 11};
enum {paramid = 12};
enum {fieldid = 13};
enum {genfieldid = 14};
enum {enumid = 15};
enum {labelid = 16};
enum {blockid = 17};
enum {aliasid = 18};
enum {linkid = 19};
enum {pc_error = 1};
enum {user_error = 2};
enum {type_error = 3};
enum {mixedtype_error = 4};
enum {divide_error = 5};
enum {stopmodule_error = 6};
enum {bounds_error = 7};

// From module: var_decls
/* Named Constants */
enum {maxmodule = 50};
enum {std_cat = 0};
enum {anon_cat = 1};
enum {user_cat = 2};
enum {maxtype = 300};
enum {hasrefmask = 65536};
enum {normal_obj = 0};
enum {slice_obj = 1};
enum {extslice_obj = 2};
enum {varsize = 16};
enum {objsize = 32};
enum {maxsearchdirs = 6};
enum {maxgenfields = 500};
enum {arraylbound = 1};
enum {maxlibpaths = 10};
enum {lab_dispatch = 1};
enum {fn_dispatch = 2};
enum {deb_dispatch = 3};
enum {asm_dispatch = 4};
enum {stacksize = 70000};
enum {maxdllindex = 30};
enum {maxdlllib = 50};
enum {maxdllproc = 500};
enum {maxcmdparam = 32};
enum {maxextra = 10};

// From module: pc_misc

// From module: support
/* Named Constants */
enum {zmax = 239};
enum {zint240 = 245};
enum {zint480 = 246};
enum {zint720 = 247};
enum {zint1 = 248};
enum {zint2 = 249};
enum {zint4 = 250};
enum {zint8 = 251};
enum {zreal4 = 252};
enum {zreal8 = 253};
enum {zstring = 254};
enum {zbytes = 244};
enum {zeof = 255};

// From module: var_objects

// From module: var_ops

// From module: pc_bigint
/* Named Constants */
enum {digitwidth = 6};
enum {digitbase = 1000000};
enum {digitmax = 999999};

// From module: var_lib

// From module: var_print
/* Named Constants */
enum {std_io = 0};
enum {file_io = 1};
enum {str_io = 2};
enum {wind_io = 3};
enum {istr_io = 4};
enum {maxstrlen = 128};
enum {onesixty = 360};
enum {comma = ','};
enum {maxoclevel = 6};
enum {maxlistdepth = 2};
enum {minkb_size = 262144};

// From module: mm_nosdll

// From module: pc_oslayer

// From module: pc_host
/* Named Constants */
enum {noparamtag = tvoid};
enum {nodefault = -999999};

// From module: pc_handlers

// From module: pc_assem

// From module: start
/* Named Constants */
enum {pxpass = 1};
enum {gxpass = 2};
enum {maxpcstrings = 5000};

// From module: mm_clib

// From module: mm_mainc

// From module: mm_nos
/* Struct Definitions */

typedef struct _rec {
    char *	name;
    void *	addr;
} rec;


// From module: mm_mlib

// From module: var_types

// From module: pq_common

// From module: var_decls
/* Struct Definitions */
typedef struct _uflagsrec uflagsrec;
typedef struct _attribrec attribrec;
typedef struct _strec strec;
typedef struct _unitrec unitrec;
typedef struct _varrec varrec;
typedef struct _objrec objrec;

struct _uflagsrec {
    byte	codes[7];
    byte	ulength;
};

struct _attribrec {
    byte	ax_global;
    byte	ax_static;
    byte	ax_equals;
    byte	ax_at;
    byte	ax_byrefmode;
    byte	ax_optional;
    byte	ax_varparams;
    byte	ax_used;
    byte	ax_forward;
    byte	ax_frame;
    byte	ax_autovar;
    byte	ax_nparams;
    byte	ax_fflang;
    byte	ax_moduleno;
    byte	ax_spare;
    union {
        byte	ax_align;
        byte	ax_dllindex;
        byte	ax_extmodno;
    };
};

struct _strec {
    char *	name;
    struct _strec* owner;
    struct _strec* deflist;
    struct _strec* nextdef;
    struct _strec* nextdupl;
    struct _strec* prevdupl;
    union {
        struct _strec* nextparam;
        unitrec *	callchain;
    };
    unitrec *	code;
    union {
        struct _strec* paramlist;
        uflagsrec	uflags;
    };
    struct _strec* equiv;
    union {
        char *	truename;
        char *	metadata;
        char *	macrovalue;
    };
    byte	namelen;
    byte	symbol;
    byte	nameid;
    byte	SPAREBYTE;
    int16	subcode;
    int16	mode;
    int32	index;
    union {
        void *	address;
        int32	offset;
        int64 *	pcaddress;
        int32	base_class;
        int32	bcindex;
    };
    int32	lineno;
    attribrec	attribs;
};

struct _unitrec {
    int32	tag;
    int32	lineno;
    struct _unitrec* a;
    struct _unitrec* b;
    struct _unitrec* c;
    struct _unitrec* nextunit;
    union {
        strec *	def;
        int64	value;
        uint64	uvalue;
        double	xvalue;
        char *	svalue;
        strec *	labeldef;
        struct {
            int32	range_lower;
            int32	range_upper;
        };
    };
    union {
        int32	opcode;
        int32	index;
        int32	trylevel;
        int32	slength;
        byte	dottedname;
    };
    int32	valuemode;
};

typedef struct _fieldrec {
    char *	name;
    int16	recordtype;
    int16	fieldtype;
    int32	fieldoffset;
} fieldrec;

struct _varrec {
    union {
        struct {
            uint16	tag;
            byte	hasref;
            union {
                byte	stackadj;
                byte	opdims;
                byte	ittype;
            };
        };
        uint32	tagx;
    };
    union {
        struct {
            uint16	refelemtag;
            byte	refbitoffset;
            byte	spare2;
        };
        struct {
            int16	frameoffset;
            byte	exceptiontype;
            byte	nexceptions;
        };
        int32	frameptr_low;
        int32	itcount;
        int32	bndigits;
    };
    union {
        objrec *	objptr;
        struct _varrec* varptr;
        byte *	packptr;
        byte *	ptr;
        int64 *	dptr;
        int64	value;
        uint64	uvalue;
        double	xvalue;
        int32 *	retaddr;
        struct {
            int32	range_lower;
            int32	range_upper;
        };
    };
};

struct _objrec {
    union {
        struct {
            int32	refcount;
            uint16	elemtag;
            union {
                byte	mutable;
                byte	bitoffset;
            };
            byte	objtype;
        };
        int64	refcountx;
    };
    union {
        byte *	ptr;
        varrec *	vptr;
        char *	strptr;
        int32 (*bnptr)[];
        int64	range64_lower;
    };
    union {
        struct {
            union {
                struct {
                    int32	length;
                    int32	lower;
                };
                int64	range64_upper;
            };
            union {
                struct _objrec* objptr2;
                struct {
                    int32	allocated;
                    int32	dictitems;
                };
                int64	range64_step;
            };
        };
        byte	data128[16];
    };
};

typedef struct _genfieldnamerec {
    char *	name;
    int32	dataindex;
    union {
        int32	datalength;
        int32	datalast;
    };
} genfieldnamerec;

typedef struct _genfielddatarec {
    int32	fieldindex;
    int32	recordtype;
    int32	fieldtype;
    union {
        int32	offset;
        uint32	index;
        uint32	procoffset;
    };
} genfielddatarec;

typedef struct _modulerec {
    char *	name;
    char *	filename;
    char *	sourcecode;
    strec *	stmodule;
    int64 (*pccode)[];
    uint16 (*linetable)[];
    int32	sourcelen;
    int32	npccode;
    int32	pcindex;
    int32	level;
    int32	exported;
    byte	importmap[50];
} modulerec;

typedef struct _dllprocrec {
    char *	name;
    void (*address)	(void);
    int32	dllindex;
} dllprocrec;

typedef struct _procrec {
    strec *	def;
    struct _procrec* nextproc;
} procrec;


// From module: pc_misc

// From module: support

// From module: var_objects

// From module: var_ops

// From module: pc_bigint

// From module: var_lib

// From module: var_print
/* Struct Definitions */

typedef struct _fmtrec {
    byte	minwidth;
    int8	precision;
    byte	base;
    char	quotechar;
    char	padchar;
    char	realfmt;
    char	plus;
    char	sepchar;
    char	lettercase;
    char	justify;
    char	suffix;
    char	usigned;
    char	charmode;
    byte	spare[3];
} fmtrec;


// From module: mm_nosdll

// From module: pc_oslayer

// From module: pc_host
/* Struct Definitions */

typedef struct _dimrec {
    int32	lbound;
    int32	upper;
    int32	length;
} dimrec;

typedef struct _overloadrec {
    int32	optype;
    int32	optype2;
    int64 *	pchandler;
    struct _overloadrec* nextrec;
} overloadrec;


// From module: pc_handlers

// From module: pc_assem

// From module: start
/* Struct Definitions */

typedef struct _switchrec {
    char *	switchname;
    int32 *	switchaddr;
} switchrec;

/* Imported Functions */
extern void	start	(void);


// From module: mm_clib

// From module: mm_mainc
/* Local Function Prototypes */
       int32	main	(int32,char * *);
       uint64	m_dotslice	(uint64,int32,int32);
       uint64	m_anddotslice	(uint64,int32,int32);
       int64	m_imin	(int64,int64);
       int64	m_imax	(int64,int64);
       uint64	m_min	(uint64,uint64);
       uint64	m_max	(uint64,uint64);

// From module: mm_nos
/* Local Function Prototypes */
       void	os_init	(void);
       int32	os_execwait	(char *,int32,char *);
       int32	os_execcmd	(char *,int32);
       int32	os_getch	(void);
       int32	os_kbhit	(void);
       void	os_flushkeys	(void);
       void *	os_getconsolein	(void);
       void *	os_getconsoleout	(void);
       void *	os_proginstance	(void);
       uint64	os_getdllinst	(char *);
       void (*os_getdllprocaddr	(int32,char *))	(void);
       void	os_initwindows	(void);
       int32	os_getchx	(void);
       char *	os_getos	(void);
       int32	os_getoscode	(void);
       int32	os_iswindows	(void);
       int32	os_shellexec	(char *,char *);
       void	os_sleep	(int32);
       void *	os_getstdin	(void);
       void *	os_getstdout	(void);
       char *	os_gethostname	(void);
       int32	os_gethostsize	(void);
       char *	os_getmpath	(void);
       void	os_exitprocess	(int32);
       int64	os_gettimestamp	(void);
       int64	os_gettickcount	(void);
       int64	os_clock	(void);
       int64	os_getclockspersec	(void);
       void	os_setmesshandler	(void *);
       int64	os_filelastwritetime	(char *);

// From module: mm_mlib
/* Local Function Prototypes */
       void *	pcm_alloc	(int32);
       void	pcm_free	(void *,int32);
       void	pcm_freeac	(void *,int32);
       void	pcm_copymem4	(void *,void *,int32);
       void	pcm_clearmem	(void *,int32);
       void	pcm_init	(void);
       int32	pcm_getac	(int32);
       uint32 *	pcm_newblock	(int32);
       int32	pcm_round	(int32);
       int32	pcm_array	(int32);
       void	pcm_printfreelist	(int32,uint32 *);
       void	pcm_diags	(char *);
       void *	pcm_allocz	(int32);
       char *	pcm_copyheapstring	(char *);
static void	addtomemalloc	(int32 *,int32);
static void	removefrommemalloc	(int32 *,int32);
       void *	allocmem	(int32);
       void *	reallocmem	(void *,int32);
       void	abortprogram	(char *);
       int32	getfilesize	(void *);
       void	readrandom	(void *,byte *,int32,int32);
       void	writerandom	(void *,byte *,int32,int32);
       byte *	readfile	(char *);
       int32	writefile	(char *,byte *,int32);
       int32	checkfile	(char *);
       void	readlinen	(void *,char *,int32);
       void	iconvlcn	(char *,int32);
       void	iconvucn	(char *,int32);
       void	convlcstring	(char *);
       void	convucstring	(char *);
       char *	changeext	(char *,char *);
       char *	extractext	(char *,int32);
       char *	extractpath	(char *);
       char *	extractfile	(char *);
       char *	extractbasefile	(char *);
       char *	addext	(char *,char *);
       void *	alloctable	(int32,int32);
       void *	zalloctable	(int32,int32);
       void	checkfreelists	(char *);
       void *	pcm_alloc32	(void);
       void	pcm_free32	(void *);
       void	outbyte	(void *,int32);
       void	outword16	(void *,uint32);
       void	outword	(void *,uint32);
       void	outdword	(void *,uint64);
       int32	myeof	(void *);
       void *	pcm_smallallocz	(int32);
       void *	pcm_fastalloc	(int32);

// From module: var_types

// From module: pq_common

// From module: var_decls

// From module: pc_misc
/* Local Function Prototypes */
       int64 *	raiseexception	(int32);
       void	raise_error	(int32);
static void	default_exception	(int32);

// From module: support
/* Local Function Prototypes */
       void	prterror	(char *);
       void	gerror	(char *,unitrec *);
       void	nxerror	(char *,unitrec *);
       int32	testelem	(byte (*)[],int32);
       void	setelem	(byte (*)[],int32);
       void	pcustype_def	(char *,varrec *);
       int64 *	pcustype	(char *,varrec *);
       int64 *	pcustypet	(char *,int32);
       void	pcmxtypes_def	(char *,varrec *,varrec *);
       int64 *	pcmxtypes	(char *,varrec *,varrec *);
       int64 *	pcmxtypestt	(char *,int32,int32);
       char *	gettypename	(int32);
       void	inittypetables	(void);
       void	pcerror	(char *);
       void	vxunimpl	(char *);
       void	pclunimpl	(int32);
       char *	convCstring	(char *,int32);
       int32	getintvalue	(varrec *);
       int32	nextpoweroftwo	(int32);
static void	showlinenumber	(void);
static void	printlinenumber	(int32,int32,char *);
       void	findlinenumber	(int64 *,int32 *,int32 *);
       int32	findpcindex	(int64 *,int32 *);
       void	showlinetable	(char *,int32);
       void	writezstring	(void *,char *);
       void	writezint	(void *,int64);
       void	writezint4	(void *,int32);
       void	writezrange	(void *,byte *);
       void	writezreal	(void *,double);
       void	writezeof	(void *);
static void	zerror	(char *);
       int32	readzvalue	(byte * *,int32 *,int32 *);
       int64	readzint	(byte * *);
       int64	readzdint	(byte * *);
       double	readzreal	(byte * *);
       char *	readzstring	(byte * *,int32 *);
       void	checkmt	(int32);
       int64	ipower	(int64,int32);

// From module: var_objects
/* Local Function Prototypes */
       objrec *	newobject	(void);
       void	freeobject	(objrec *);
       objrec *	addref_obj	(objrec *);
       void	makezobjects	(void);
       objrec *	make_str	(char *,int32);
       varrec *	make_listdata	(int32,int32 *,int32);
       void	free_listdata	(varrec *,int32);
       byte *	make_arraydata	(int32,int32,int32 *,int32);
       void	free_arraydata	(byte *,int32,int32);
       byte *	make_bitdata	(int32,int32,int32 *,int32);
       void	free_bitdata	(byte *,int32,int32);
       objrec *	make_listobj	(int32,int32);
       objrec *	make_stringobj	(int32);
       objrec *	make_arrayobj	(int32,int32,int32);
       objrec *	make_setobj	(int32);
       objrec *	make_bitsobj	(int32,int32,int32);
       void	resize_listobj	(objrec *,int32);
       void	resize_arrayobj	(objrec *,int32);
       void	resize_bitsobj	(objrec *,int32);
       void	addto_stringobj	(varrec *,varrec *);
       void	addto_stringobj_i	(varrec *,int32);
       void	addto_stringobj_s	(varrec *,char *);
       void	showstring	(char *,objrec *);
       objrec *	add_stringobj	(objrec *,objrec *);
       objrec *	make_strslicexobj	(char *,int32);
       objrec *	copyonwrite	(objrec *,int32);
       int32	get_objbytes	(objrec *);
       objrec *	make_char	(int32);
       void	objerror	(char *);

// From module: var_ops
/* Local Function Prototypes */
       void	vx_addstring	(varrec *,varrec *,varrec *);
       void	vx_addtostring	(varrec *,varrec *);
       void	vx_addtostring_imm	(varrec *,char *);
       int32	vx_eqstring	(varrec *,varrec *);
       void	vx_iorset	(varrec *,varrec *,varrec *);
       int32	vx_equal	(varrec *,varrec *,int32);
       int32	vx_compare	(varrec *,varrec *);
       void	vx_ufree	(varrec *);
       void	vx_cfree	(varrec *);
       void	vx_freex	(varrec *);
       void	vx_freeref	(objrec *,int32);
       void	vx_cshare	(varrec *);
       void	vx_ushare	(varrec *);
       void	vx_dupl	(varrec *);
       void	vx_free_dispatch	(varrec *);
       void	vx_dupl_dispatch	(varrec *);
       void	vx_free_string	(varrec *);
       void	vx_free_list	(varrec *);
       void	vx_free_struct	(varrec *);
       void	vx_free_array	(varrec *);
       void	vx_free_bits	(varrec *);
       void	vx_dupl_string	(varrec *);
       void	vx_dupl_list	(varrec *);
       void	vx_dupl_array	(varrec *);
       void	vx_dupl_bits	(varrec *);
       void	vx_dupl_set	(varrec *);
       void	vx_dupl_struct	(varrec *);
       void	vx_mul_listi	(varrec *,varrec *,varrec *);
       void	vx_mul_stri	(varrec *,varrec *,varrec *);
       void	vx_slicelist	(varrec *,varrec *,varrec *);
       void	vx_iappendlist	(varrec *,varrec *);
       void	vx_iconcatlist	(varrec *,varrec *);
       void	vx_indexstring	(varrec *,varrec *,varrec *);
       void	vx_slicestring	(varrec *,varrec *,varrec *);
       int32	vx_invar	(varrec *,varrec *);
static int32	u8inarray	(byte,objrec *);
static int32	u16inarray	(uint16,objrec *);
static int32	u32inarray	(uint32,objrec *);
static int32	u64inarray	(uint64,objrec *);
static int32	bitinbits	(byte,objrec *);
       int32	vx_strinstr	(varrec *,varrec *);
       int32	vx_eqstring_nf	(varrec *,varrec *);
       int32	vx_equal_nf	(varrec *,varrec *,int32);
       int32	comparebytes	(byte *,byte *,int32);
       int32	vx_compare_nf	(varrec *,varrec *);
       int32	cmpstring	(char *,char *,int32,int32);
       void	iorsetbits	(int32 *,int32 *,int32);
       void	pc_iandsetbits	(int32 *,int32 *,int32);
       void	pc_ixorsetbits	(int32 *,int32 *,int32);
       void	pc_subsetbits	(int32 *,int32 *,int32);
       void	iresizeset	(varrec *,int32);
       void	vx_storestring	(varrec *,varrec *);

// From module: pc_bigint
/* Local Function Prototypes */
       void	bn_makestr	(char *,int32,varrec *);
       void	bn_makeint	(int64,varrec *);
static void	bn_makeu	(char *,int32,int32,varrec *);
static int32	strvaln	(char *,int32);
static void	makebigint	(int32,varrec *);
static void	freebigint	(varrec *);
       void	bn_neg	(varrec *);
       void	bn_abs	(varrec *);
       void	bn_add	(varrec *,varrec *,varrec *);
static void	bn_addu	(varrec *,varrec *,varrec *);
       void	bn_sub	(varrec *,varrec *,varrec *);
static void	bn_subu	(varrec *,varrec *,varrec *);
       void	bn_mul	(varrec *,varrec *,varrec *);
static void	muldigit	(varrec *,int32,int32,varrec *);
static void	bn_zero	(varrec *);
       void	bn_mulu	(varrec *,varrec *,varrec *);
       void	bn_div	(varrec *,varrec *,varrec *);
       void	bn_divu	(varrec *,varrec *,varrec *);
       int32	bn_equal	(varrec *,varrec *);
       int32	bn_cmp	(varrec *,varrec *);
       int64	bn_int	(varrec *);
       int32	bn_digits	(varrec *);
       void	bn_power	(varrec *,int32,varrec *);

// From module: var_lib
/* Local Function Prototypes */
       void	vx_getptr	(varrec *);
       void	vx_storeptr	(varrec *,varrec *);
       void	vx_assign	(varrec *,varrec *);
       void	vx_swap	(varrec *,varrec *);
       void	vx_convptr	(varrec *);
       void	vx_istrue	(varrec *,varrec *);
       void	vx_isfalse	(varrec *,varrec *);
       void	vx_isdef	(varrec *,varrec *);
       void	vx_isvoid	(varrec *,varrec *);
       void	vx_eq	(varrec *,varrec *,varrec *);
       void	vx_ne	(varrec *,varrec *,varrec *);
       void	vx_lt	(varrec *,varrec *,varrec *);
       void	vx_le	(varrec *,varrec *,varrec *);
       void	vx_ge	(varrec *,varrec *,varrec *);
       void	vx_gt	(varrec *,varrec *,varrec *);
       void	vx_cmp	(varrec *,varrec *,varrec *);
       int32	ivx_istrue	(varrec *);
       int32	ivx_isfalse	(varrec *);
       int32	ivx_isdef	(varrec *);
       int32	ivx_isvoid	(varrec *);
       int32	ivx_eq	(varrec *,varrec *);
       int32	ivx_ne	(varrec *,varrec *);
       int32	ivx_lt	(varrec *,varrec *);
       int32	ivx_le	(varrec *,varrec *);
       int32	ivx_ge	(varrec *,varrec *);
       int32	ivx_gt	(varrec *,varrec *);
       int32	ivx_cmp	(varrec *,varrec *);
       void	vx_makelist	(int32,varrec *,varrec *,int32);
       void	vx_makerecord	(int32,int32,varrec *,varrec *);
       void	vx_makearray	(int32,int32,int32,int32,varrec *,varrec *);
       void	vx_makebits	(int32,int32,int32,int32,varrec *,varrec *);
       void	vx_makerange	(varrec *,varrec *,varrec *);
       void	vx_makeset	(int32,varrec *,varrec *);
       void	vx_makestruct	(int32,int32,varrec *,varrec *);
       void	vx_makedict	(int32,varrec *,varrec *);
static void	adddictitem	(varrec *,varrec *,varrec *);
       varrec *	finddictitem	(objrec *,varrec *,int32);
static void	expanddict	(objrec *);
       void	vx_makevoid	(varrec *);
       void	vx_makeint	(int64,varrec *);
       void	vx_makereal	(double,varrec *);
       void	vx_makerange_ab	(int32,int32,varrec *);
       void	vx_makestring	(char *,varrec *);
       void	vx_makerefvar	(varrec *,varrec *);
       int64	vx_getint	(varrec *);
       double	vx_getreal	(varrec *);
       char *	vx_getzstring	(varrec *);
       void	vx_typepun	(int32,varrec *,varrec *);
       void	vx_iconvert	(int32,varrec *);
       int32	ivx_mixed	(varrec *,varrec *);
       void	vx_incr	(varrec *);
       void	vx_decr	(varrec *);
       void	vx_loadincr	(varrec *,varrec *);
       void	vx_incrload	(varrec *,varrec *);
       void	vx_loaddecr	(varrec *,varrec *);
       void	vx_decrload	(varrec *,varrec *);
       void	vx_negto	(varrec *);
       void	vx_absto	(varrec *);
       void	vx_inotto	(varrec *);
       void	vx_notl	(varrec *,varrec *);
       void	vx_notlto	(varrec *);
       void	vx_asc	(varrec *,varrec *);
       void	vx_chr	(varrec *,varrec *);
       int32	ivx_asc	(varrec *);
       void	vx_round	(varrec *,varrec *);
       void	vx_maths	(int32,varrec *,varrec *);
       void	vx_maths2	(int32,varrec *,varrec *,varrec *);
       void	vx_bounds	(varrec *,varrec *);
       int32	ivx_len	(varrec *);
       int32	ivx_lwb	(varrec *);
       int32	ivx_upb	(varrec *);
       int32	ivx_gettype	(varrec *);
       int32	ivx_getbasetype	(varrec *);
       int32	ivx_getelemtype	(varrec *);
       int32	ivx_istype	(int32,varrec *);
       int32	ivx_isnumber	(varrec *);
       int32	ivx_isarray	(varrec *);
       int32	ivx_isrecord	(varrec *);
       int32	ivx_ispointer	(varrec *);
       int32	ivx_isset	(varrec *);
       int32	ivx_isequal	(varrec *,varrec *);
       void	vx_tostr	(varrec *,varrec *);
       void	vx_tostrfmt	(varrec *,varrec *,varrec *);
       void	vx_mul	(varrec *,varrec *,varrec *);
       void	vx_div	(varrec *,varrec *,varrec *);
       void	vx_intdiv	(varrec *,varrec *,varrec *);
       void	vx_rem	(varrec *,varrec *,varrec *);
       void	vx_iand	(varrec *,varrec *,varrec *);
       void	vx_ior	(varrec *,varrec *,varrec *);
       void	vx_ixor	(varrec *,varrec *,varrec *);
       void	vx_in	(varrec *,varrec *,varrec *);
       void	vx_notin	(varrec *,varrec *,varrec *);
       void	vx_inrev	(varrec *,varrec *,varrec *);
       void	vx_min	(varrec *,varrec *,varrec *);
       void	vx_max	(varrec *,varrec *,varrec *);
       void	vx_concat	(varrec *,varrec *,varrec *);
       void	vx_append	(varrec *,varrec *,varrec *);
       void	vx_addto	(varrec *,varrec *);
       void	vx_subto	(varrec *,varrec *);
       void	vx_multo	(varrec *,varrec *);
       void	vx_divto	(varrec *,varrec *);
       void	vx_idivto	(varrec *,varrec *);
       void	vx_irem	(varrec *,varrec *);
       void	vx_iandto	(varrec *,varrec *);
       void	vx_iorto	(varrec *,varrec *);
       void	vx_ixorto	(varrec *,varrec *);
       void	vx_shlto	(varrec *,varrec *);
       void	vx_shrto	(varrec *,varrec *);
       void	vx_minto	(varrec *,varrec *);
       void	vx_maxto	(varrec *,varrec *);
       void	vx_concatto	(varrec *,varrec *);
       void	vx_appendto	(varrec *,varrec *);
       void	vx_getdot	(int32,varrec *,varrec *);
       void	vx_getdotref	(int32,varrec *,varrec *);
       void	vx_indexref	(varrec *,varrec *,varrec *);
       void	vx_sliceref	(varrec *,varrec *,varrec *);
       void	vx_dotindexref	(varrec *,varrec *,varrec *);
       void	vx_dotslice	(varrec *,varrec *,varrec *);
       void	vx_dotsliceref	(varrec *,varrec *,varrec *);
       void	vx_getkeyix	(varrec *,varrec *,varrec *,varrec *);
       void	vx_getkeyixref	(varrec *,varrec *,varrec *);
       void	vx_iconvcase	(varrec *,varrec *,int32);
       void	vx_loadpacked	(void *,int32,varrec *,objrec *);
       void	vx_storepacked	(byte *,varrec *,int32);
       void	vx_loadbit	(byte *,int32,int32,varrec *);
       void	vx_storebit	(byte *,int32,varrec *,int32);
static void	setfslength	(char *,int32,int32);
static int32	getfslength	(char *,int32);
       int32	gethashvalue	(varrec *);
       int32	vx_mixed	(varrec *,varrec *);
       void	vx_loadptr	(varrec *,varrec *);
       void	vx_popptrlist	(varrec *,varrec *);
       void	vx_iappendarray	(varrec *,varrec *);

// From module: var_print
/* Local Function Prototypes */
       void	pch_print	(varrec *,varrec *);
       void	pch_println	(void);
       void	pch_startprintcon	(void);
       void	pch_startprint	(varrec *);
       void	pch_endprint	(void);
       void	pch_strstartprint	(void);
       void	pch_strendprint	(varrec *);
       void	pch_setformat	(varrec *);
       void	pch_setformat2	(varrec *);
       void	pch_dprint	(varrec *,varrec *);
       void	pch_printnogap	(void);
static void	initfmtcode	(fmtrec *);
static int32	i64mintostr	(char *,int32,int32);
static int32	u64tostr	(uint64,char *,uint32,int32);
static int32	i64tostrfmt	(int64,char *,fmtrec *,int32);
static int32	u64tostrfmt	(int64,char *,fmtrec *);
static int32	strtostrfmt	(char *,char *,int32,fmtrec *);
static int32	expandstr	(char *,char *,int32,fmtrec *);
       void	pc_strtofmt	(char *,int32,fmtrec *);
static void	printstrz	(char *);
static void	printstr_n	(char *,int32);
       void	printerror	(char *);
static void	addstring	(objrec *,char *,int32);
       void	tostr_i	(varrec *,varrec *,fmtrec *,objrec *);
       void	tostr_r	(varrec *,varrec *,fmtrec *,objrec *);
       void	tostr_u	(varrec *,varrec *,fmtrec *,objrec *);
       void	tostr_range	(varrec *,varrec *,fmtrec *,objrec *);
       void	tostr_s	(varrec *,varrec *,fmtrec *,objrec *);
       void	tostr_list	(varrec *,varrec *,fmtrec *,objrec *);
       void	tostr_ax	(varrec *,varrec *,fmtrec *,objrec *);
       void	tostr_bits	(varrec *,varrec *,fmtrec *,varrec *);
       void	tostr_set	(varrec *,varrec *,fmtrec *,objrec *);
       void	tostr_struct	(varrec *,varrec *,fmtrec *,objrec *);
       void	tostr_long	(varrec *,varrec *,fmtrec *,objrec *);
       void	tostr_dict	(varrec *,varrec *,fmtrec *,objrec *);
       void	do_tostr	(varrec *,varrec *,fmtrec *,objrec *);
static void	printnextfmtchars	(int32);
static int32	getreadfmtcode	(varrec *);
       void	pch_sreadln	(varrec *,varrec *);
       void	pch_strtoval	(varrec *,varrec *,varrec *);
       void	pch_reread	(void);
       void	pch_rereadln	(void);
static char *	readname	(char *,int32,varrec *);
static char *	readstring	(char *,int32,varrec *);
static char *	readint	(char *,int32,varrec *);
static char *	readhex	(char *,int32,varrec *);
static char *	readbin	(char *,int32,varrec *);
static char *	readreal	(char *,int32,varrec *);
       void	pch_readln	(varrec *);
static void	stepkbpos	(char *);
       void	pch_sread	(varrec *,varrec *);
static void	domultichar	(char *,int32,char *,fmtrec *);
       void	pch_tostr	(varrec *,varrec *,varrec *);
       fmtrec *	pc_getfmt	(varrec *,fmtrec *);
static void	pc_readlinen	(void *,char *,int32);
static void	makevarstr	(char *,int32,varrec *);
static char *	readitem	(char *,int32,char * *,int32 *);
static char *	readany	(char *,int32,varrec *);
static void	strtoreal	(char *,int32,varrec *);
static void	strtoint	(char *,int32,varrec *);

// From module: mm_nosdll
/* Local Function Prototypes */
       int64	os_calldll_wint	(void *,int64 (*)[],int32);
       double	os_calldll_wreal	(void *,int64 (*)[],int32);
       void	os_dummycall	(double,double,double,double);

// From module: pc_oslayer
/* Local Function Prototypes */
       int32	runproc_m	(void *);
       void	os_getconsize	(varrec *);
       void	pch_setmesshandler	(varrec *);
       void	pch_gethostname	(varrec *);
       void	os_initdllmodules	(void);
       int32	os_loaddllmodule	(char *);
       void	os_initdllfunctions	(void);
       void *	os_loaddllfunction	(int32);
       void	pch_getos	(varrec *);
       void	pch_gethostsize	(varrec *);
       void	pch_iswindows	(varrec *);
       void	os_calldll	(int32,int32,int32,int32,int32,varrec *);
static int64	calldll_mint	(void (*)	(void),int64 (*)[],int32);
static double	calldll_mreal	(void (*)	(void),int64 (*)[],int32);
static int64	calldll_cint	(void (*)	(void),int64 (*)[],int32);
static double	calldll_creal	(void (*)	(void),double (*)[],int32);

// From module: pc_host
/* Local Function Prototypes */
       void	callhostfunction	(int32,int32);
static void	pch_leftstr	(varrec *,varrec *,varrec *,varrec *);
static void	pch_rightstr	(varrec *,varrec *,varrec *,varrec *);
static void	pch_convlc	(varrec *,varrec *,varrec *);
static void	pch_convuc	(varrec *,varrec *,varrec *);
static void	pch_iconvlc	(varrec *,varrec *);
static void	pch_iconvuc	(varrec *,varrec *);
static void	pch_stop	(void);
static void	pch_stopx	(varrec *);
static void	pch_ismain	(varrec *,varrec *);
static void	pch_waitkey	(varrec *);
static void	pch_testkey	(varrec *);
static void	pch_execwait	(varrec *,varrec *,varrec *,varrec *);
static void	pch_execcmd	(varrec *,varrec *,varrec *,varrec *);
static void	pch_makestr	(varrec *,varrec *,varrec *);
static void	pch_makestrslice	(varrec *,varrec *,varrec *);
static void	pch_makeref	(varrec *,varrec *,varrec *);
static void	pch_new	(varrec *,varrec *,varrec *,varrec *,varrec *);
static void	pch_newheap	(varrec *,varrec *,varrec *,varrec *,varrec *);
static void	pch_heapvar	(varrec *,varrec *);
static void	pch_freeheap	(varrec *);
static void	pch_getcmdparam	(varrec *,varrec *);
static void	pch_setpcerror	(varrec *);
static void	pch_setdebug	(varrec *);
static void	pch_setfprintf	(varrec *,varrec *);
static void	pch_ticks	(varrec *);
static void	pch_sleep	(varrec *);
static void	pch_random	(varrec *,varrec *);
static void	pch_findmetafunction	(varrec *,varrec *);
static void	pch_loadpcl	(varrec *,varrec *,varrec *);
static void	pch_runpcl	(varrec *,varrec *,varrec *);
static void	pch_runtask	(varrec *,varrec *,varrec *);
static void	pch_callext	(varrec *,varrec *,varrec *);
static void	pch_system	(varrec *,varrec *);
static void	pch_shellexec	(varrec *,varrec *,varrec *);
static void	pch_gethash	(varrec *,varrec *);
static void	pch_test	(varrec *,varrec *,varrec *);
static void	pch_pcldata	(varrec *,varrec *,varrec *);
static void	pch_getcstring	(varrec *,varrec *);
static void	pch_getparam	(varrec *,varrec *);
static void	pch_clearlist	(varrec *);
static void	pch_makelink	(varrec *,varrec *);
static void	pch_allparams	(varrec *,varrec *);
static void	pch_stackvars	(varrec *);
static void	pch_makeempty	(varrec *,varrec *);
static void	pch_readlines	(varrec *,varrec *);
static void	pch_dictitems	(varrec *,varrec *);
static void	pch_setoverload	(varrec *,varrec *,varrec *);
static void	pch_errorinfo	(varrec *,varrec *);
static void	getbounds	(varrec *,dimrec *,int32);
static int64	checkparam	(varrec *,int32,int32);
static void	leftstring	(varrec *,int32,varrec *);
static void	rightstring	(varrec *,int32,varrec *);
static void	padstring_right	(varrec *,int32,int32,varrec *);
static void	padstring_left	(varrec *,int32,int32,varrec *);
static void	pcld_makevstr	(varrec *,char *);
static void	pcld_makevint	(varrec *,int64);
static void	pcld_makelist	(varrec *,varrec *,int32);
static void	getproctabledata	(procrec *,varrec *);
static int64 *	convert_handler	(int32);
static void	addtoproclist	(strec *);

// From module: pc_handlers
/* Local Function Prototypes */
       void *	k_zero	(void);
       void *	k_nop	(void);
       void *	k_procstart	(void);
       void *	k_procend	(void);
       void *	k_endmodule	(void);
       void *	k_push_m	(void);
       void *	k_push_f	(void);
       void *	k_push_am	(void);
       void *	k_push_af	(void);
       void *	k_push_ap	(void);
       void *	k_push_al	(void);
       void *	k_push_ci	(void);
       void *	k_push_cw	(void);
       void *	k_push_cr	(void);
       void *	k_push_cn	(void);
       void *	k_push_cs	(void);
       void *	k_push_t	(void);
       void *	k_push_op	(void);
       void *	k_pushz	(void);
       void *	k_pushz_void	(void);
       void *	k_pushz_str	(void);
       void *	k_pushz_list	(void);
       void *	k_pushz_listl	(void);
       void *	k_pushz_set	(void);
       void *	k_pushz_arrayl	(void);
       void *	k_pop_m	(void);
       void *	k_pop_f	(void);
       void *	k_store_m	(void);
       void *	k_store_f	(void);
       void *	k_pushptr	(void);
       void *	k_popptr	(void);
       void *	k_storeptr	(void);
       void *	k_zpop_m	(void);
       void *	k_zpop_f	(void);
       void *	k_zstore_m	(void);
       void *	k_zstore_f	(void);
       void *	k_copy	(void);
       void *	k_swap	(void);
       void *	k_convptr	(void);
       void *	k_jump	(void);
       void *	k_jumpptr	(void);
       void *	k_jumptrue	(void);
       void *	k_jumpfalse	(void);
       void *	k_jumpdef	(void);
       void *	k_jumpvoid	(void);
       void *	k_jumpeq	(void);
       void *	k_jumpne	(void);
       void *	k_jumplt	(void);
       void *	k_jumple	(void);
       void *	k_jumpge	(void);
       void *	k_jumpgt	(void);
       void *	k_jumptesteq	(void);
       void *	k_jumptestne	(void);
       void *	k_jumplabel	(void);
       void *	k_jumpclabel	(void);
       void *	k_switch	(void);
       void *	k_cswitch	(void);
       void *	k_new	(void);
       void *	k_to_f	(void);
       void *	k_for_fci	(void);
       void *	k_for_ff	(void);
       void *	k_ford_fci	(void);
       void *	k_ford_ff	(void);
       void *	k_call	(void);
       void *	k_callptr	(void);
       void *	k_return	(void);
       void *	k_startdll	(void);
       void *	k_pushdll	(void);
       void *	k_calldll	(void);
       void *	k_callhost	(void);
       void *	k_stackframe	(void);
       void *	k_free	(void);
       void *	k_addsp	(void);
       void *	k_stop	(void);
       void *	k_test	(void);
       void *	k_makelist	(void);
       void *	k_makerecord	(void);
       void *	k_makearray	(void);
       void *	k_makestruct	(void);
       void *	k_makeset	(void);
       void *	k_makerange	(void);
       void *	k_makedict	(void);
       void *	k_pushdot	(void);
       void *	k_pushdotref	(void);
       void *	k_softconv	(void);
       void *	k_hardconv	(void);
       void *	k_mixed	(void);
       void *	k_incrptr	(void);
       void *	k_incrto_m	(void);
       void *	k_incrto_f	(void);
       void *	k_loadincr	(void);
       void *	k_incrload	(void);
       void *	k_decrptr	(void);
       void *	k_decrto_m	(void);
       void *	k_decrto_f	(void);
       void *	k_loaddecr	(void);
       void *	k_decrload	(void);
       void *	k_incr	(void);
       void *	k_decr	(void);
       void *	k_neg	(void);
       void *	k_abs	(void);
       void *	k_not	(void);
       void *	k_inot	(void);
       void *	k_istrue	(void);
       void *	k_asc	(void);
       void *	k_chr	(void);
       void *	k_sqrt	(void);
       void *	k_sqr	(void);
       void *	k_cube	(void);
       void *	k_sin	(void);
       void *	k_cos	(void);
       void *	k_tan	(void);
       void *	k_asin	(void);
       void *	k_acos	(void);
       void *	k_atan	(void);
       void *	k_sign	(void);
       void *	k_ln	(void);
       void *	k_log	(void);
       void *	k_lg	(void);
       void *	k_exp	(void);
       void *	k_round	(void);
       void *	k_floor	(void);
       void *	k_ceil	(void);
       void *	k_fract	(void);
       void *	k_negto	(void);
       void *	k_absto	(void);
       void *	k_notto	(void);
       void *	k_inotto	(void);
       void *	k_len	(void);
       void *	k_lwb	(void);
       void *	k_upb	(void);
       void *	k_bounds	(void);
       void *	k_bits	(void);
       void *	k_bytes	(void);
       void *	k_type	(void);
       void *	k_elemtype	(void);
       void *	k_basetype	(void);
       void *	k_minval	(void);
       void *	k_maxval	(void);
       void *	k_isint	(void);
       void *	k_isreal	(void);
       void *	k_isstring	(void);
       void *	k_isrange	(void);
       void *	k_isnumber	(void);
       void *	k_isarray	(void);
       void *	k_isrecord	(void);
       void *	k_ispointer	(void);
       void *	k_ismutable	(void);
       void *	k_isset	(void);
       void *	k_isvoid	(void);
       void *	k_isdef	(void);
       void *	k_tostr	(void);
       void *	k_isequal	(void);
       void *	k_add	(void);
       void *	k_sub	(void);
       void *	k_mul	(void);
       void *	k_div	(void);
       void *	k_idiv	(void);
       void *	k_rem	(void);
       void *	k_divrem	(void);
       void *	k_iand	(void);
       void *	k_ior	(void);
       void *	k_ixor	(void);
       void *	k_shl	(void);
       void *	k_shr	(void);
       void *	k_in	(void);
       void *	k_notin	(void);
       void *	k_inrev	(void);
       void *	k_eq	(void);
       void *	k_ne	(void);
       void *	k_lt	(void);
       void *	k_le	(void);
       void *	k_ge	(void);
       void *	k_gt	(void);
       void *	k_min	(void);
       void *	k_max	(void);
       void *	k_concat	(void);
       void *	k_append	(void);
       void *	k_power	(void);
       void *	k_atan2	(void);
       void *	k_addto	(void);
       void *	k_subto	(void);
       void *	k_multo	(void);
       void *	k_divto	(void);
       void *	k_idivto	(void);
       void *	k_iandto	(void);
       void *	k_iorto	(void);
       void *	k_ixorto	(void);
       void *	k_shlto	(void);
       void *	k_shrto	(void);
       void *	k_minto	(void);
       void *	k_maxto	(void);
       void *	k_concatto	(void);
       void *	k_appendto	(void);
       void *	k_pushix	(void);
       void *	k_pushdotix	(void);
       void *	k_pushkeyix	(void);
       void *	k_pushkeyixd	(void);
       void *	k_pushixref	(void);
       void *	k_pushdotixref	(void);
       void *	k_pushkeyixref	(void);
       void *	k_pushbyteix	(void);
       void *	k_pushbyteixref	(void);
       void *	k_appendset	(void);
       void *	k_pushdotm	(void);
       void *	k_pushdott	(void);
       void *	k_push_ad	(void);
       void *	k_push_try	(void);
       void *	k_raise	(void);
       void *	k_applyop	(void);
       void *	k_makeiter	(void);
       void *	k_forall	(void);
       void *	k_forallx	(void);
       void *	k_foreach	(void);
       void *	k_foreachx	(void);
       void *	k_expandrange	(void);

// From module: pc_assem

// From module: start
/* Local Function Prototypes */
       void	start	(void);
static void	initlogfile	(void);
static void	closelogfile	(void);
static void	initdata	(void);
static char *	getinputfile	(int32 *);
static void	showpcl	(char *,int32,int32);
static void	showgenfields	(void);
static void	loaderror	(char *,char *);
static void	do_showprogdiags_pc	(char *);
       int32	runcode	(int32);
static void	initbytecode	(int32);
static void	disploop	(void);
static void	pclinit	(void);
static objrec *	newstringobject	(char *);
static void	fixup_all_pc	(void);
static void	fixup_module_pc	(int32);
static int32 *	disploop_fn	(int32);
static void	disploop_deb	(void);
static void	getsyscmdline	(void);
static void	showmodules	(void);
static void	starttiming	(void);
static void	showtiming	(void);
       void	runproc	(void *,varrec *,varrec *,varrec *);
static void	showinttiming	(char *);
static void	allocatestrings	(void);
static int32	loadprogram	(char *,int32 *);
static int32	loadpcfile	(char *);
       void	initpcldata	(void);
static strec *	createstentry	(int32,char *,int32,int32);
static void	findnewestfile	(char * *,int64 *,int32 *,char *,char *,int32);

// From module: mm_clib

// From module: mm_mainc
/* Local Static Variables */
       int32	nsysparams;
       char *	sysparams[32];

// From module: mm_nos
/* Local Static Variables */
static int32	init_flag=0;

// From module: mm_mlib
/* Local Static Variables */
       uint32	allocupper[256];
       int32	alloccode;
       int32	allocbytes;
       int32	fdebug=0;
       int32	rfsize;
static uint32	maxmemory;
static int32	maxalloccode;
static int32	show=0;
       int32	memtotal=0;
       int64	smallmemtotal=0;
       int32	smallmemobjs=0;
       int32	maxmemtotal=0;
static int32 *	memalloctable[100001];
static int32	memallocsize[100001];
static byte *	pcheapstart;
static byte *	pcheapend;
static byte *	pcheapptr;
static byte	sizeindextable[2049];
static uint32 *	freelist[9];

// From module: var_types
/* Local Static Variables */
       char *	stdtypenames[54]={"tvoid",
"tint",
"tword",
"treal",
"trange",
"tstring",
"twstring",
"tlongint",
"trational",
"tset",
"tdict",
"tword128",
"tenum",
"ttype",
"toperator",
"tsymbol",
"tretaddr",
"texception",
"trefproc",
"trefdllproc",
"treflabel",
"tstringz",
"trefvar",
"trefpacked",
"trefbit",
"trecordlink",
"treflist",
"trefarray",
"trefbits",
"tlist",
"tarray",
"tbits",
"trecord",
"tstruct",
"tuser",
"tvariant",
"tc8",
"ti8",
"ti16",
"ti32",
"ti64",
"tbit",
"tbit2",
"tbit4",
"tu8",
"tu16",
"tu32",
"tu64",
"tr32",
"tr64",
"tintm",
"twordm",
"trefm",
"tlast"
};
       int32	stdtypewidths[54]={128,
64,
64,
64,
64,
0,
0,
0,
0,
0,
0,
0,
0,
64,
64,
64,
0,
0,
64,
64,
64,
64,
64,
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
128,
8,
8,
16,
32,
64,
1,
2,
4,
8,
16,
32,
64,
32,
64,
32,
32,
64,
0
};

// From module: pq_common
/* Local Static Variables */
       char *	opndnames[18]={"cnone",
"cmemory",
"cframe",
"cproc",
"cdllproc",
"cdllvar",
"cfield",
"cgenfield",
"clabel",
"cint32",
"cint",
"cword",
"creal",
"crange",
"cstring",
"ctype",
"coperator",
"?"
};
       char *	cmdnames[217]={"kzero",
"knop",
"kprocstart",
"kprocend",
"kendmodule",
"kpush_m",
"kpush_f",
"kpush_am",
"kpush_af",
"kpush_ap",
"kpush_al",
"kpush_ci",
"kpush_cw",
"kpush_cr",
"kpush_cn",
"kpush_cs",
"kpush_t",
"kpush_op",
"kpushz",
"kpushz_void",
"kpushz_str",
"kpushz_list",
"kpushz_listl",
"kpushz_set",
"kpushz_arrayl",
"kpop_m",
"kpop_f",
"kstore_m",
"kstore_f",
"kpushptr",
"kpopptr",
"kstoreptr",
"kzpop_m",
"kzpop_f",
"kzstore_m",
"kzstore_f",
"kcopy",
"kswap",
"kconvptr",
"kjump",
"kjumpptr",
"kjumptrue",
"kjumpfalse",
"kjumpdef",
"kjumpvoid",
"kjumpeq",
"kjumpne",
"kjumplt",
"kjumple",
"kjumpge",
"kjumpgt",
"kjumptesteq",
"kjumptestne",
"kjumplabel",
"kjumpclabel",
"kswitch",
"kcswitch",
"knew",
"kto_f",
"kfor_fci",
"kfor_ff",
"kford_fci",
"kford_ff",
"kcall",
"kcallptr",
"kreturn",
"kstartdll",
"kpushdll",
"kcalldll",
"kcallhost",
"kstackframe",
"kfree",
"kaddsp",
"kstop",
"ktest",
"kmakelist",
"kmakerecord",
"kmakearray",
"kmakestruct",
"kmakeset",
"kmakerange",
"kmakedict",
"kpushdot",
"kpushdotref",
"ksoftconv",
"khardconv",
"kmixed",
"kincrptr",
"kincrto_m",
"kincrto_f",
"kloadincr",
"kincrload",
"kdecrptr",
"kdecrto_m",
"kdecrto_f",
"kloaddecr",
"kdecrload",
"kincr",
"kdecr",
"kneg",
"kabs",
"knot",
"kinot",
"kistrue",
"kasc",
"kchr",
"ksqrt",
"ksqr",
"kcube",
"ksin",
"kcos",
"ktan",
"kasin",
"kacos",
"katan",
"ksign",
"kln",
"klog",
"klg",
"kexp",
"kround",
"kfloor",
"kceil",
"kfract",
"knegto",
"kabsto",
"knotto",
"kinotto",
"klen",
"klwb",
"kupb",
"kbounds",
"kbits",
"kbytes",
"ktype",
"kelemtype",
"kbasetype",
"kminval",
"kmaxval",
"kisint",
"kisreal",
"kisstring",
"kisrange",
"kisnumber",
"kisarray",
"kisrecord",
"kispointer",
"kismutable",
"kisset",
"kisvoid",
"kisdef",
"ktostr",
"kisequal",
"kadd",
"ksub",
"kmul",
"kdiv",
"kidiv",
"krem",
"kdivrem",
"kiand",
"kior",
"kixor",
"kshl",
"kshr",
"kin",
"knotin",
"kinrev",
"keq",
"kne",
"klt",
"kle",
"kge",
"kgt",
"kmin",
"kmax",
"kconcat",
"kappend",
"kpower",
"katan2",
"kaddto",
"ksubto",
"kmulto",
"kdivto",
"kidivto",
"kiandto",
"kiorto",
"kixorto",
"kshlto",
"kshrto",
"kminto",
"kmaxto",
"kconcatto",
"kappendto",
"kpushix",
"kpushdotix",
"kpushkeyix",
"kpushkeyixd",
"kpushixref",
"kpushdotixref",
"kpushkeyixref",
"kpushbyteix",
"kpushbyteixref",
"kappendset",
"kpushdotm",
"kpushdott",
"kpush_ad",
"kpush_try",
"kraise",
"kapplyop",
"kmakeiter",
"kforall",
"kforallx",
"kforeach",
"kforeachx",
"kexpandrange",
"klastcmd"
};
       byte	cmdfmt[217][4]={{0,0,0,0},
{0,0,0,0},
{p,h,0,0},
{0,0,0,0},
{0,0,0,0},
{m,0,0,0},
{f,0,0,0},
{m,0,0,0},
{f,0,0,0},
{p,0,0,0},
{l,0,0,0},
{i,0,0,0},
{u,0,0,0},
{r,0,0,0},
{n,0,0,0},
{s,0,0,0},
{t,0,0,0},
{o,h,0,0},
{t,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{h,0,0,0},
{0,0,0,0},
{t,h,0,0},
{m,0,0,0},
{f,0,0,0},
{m,0,0,0},
{f,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{m,0,0,0},
{f,0,0,0},
{m,0,0,0},
{f,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{l,0,0,0},
{0,0,0,0},
{l,0,0,0},
{l,0,0,0},
{l,0,0,0},
{l,0,0,0},
{l,0,0,0},
{l,0,0,0},
{l,0,0,0},
{l,0,0,0},
{l,0,0,0},
{l,0,0,0},
{l,0,0,0},
{l,0,0,0},
{l,0,0,0},
{l,h,0,0},
{h,h,0,0},
{h,h,h,0},
{0,0,0,0},
{l,f,0,0},
{l,f,h,0},
{l,f,f,0},
{l,f,h,0},
{l,f,f,0},
{p,h,0,0},
{h,h,0,0},
{0,0,0,0},
{0,0,0,0},
{t,0,0,0},
{x,h,t,0},
{h,0,0,0},
{h,0,0,0},
{h,0,0,0},
{h,0,0,0},
{0,0,0,0},
{h,0,0,0},
{h,h,0,0},
{h,t,0,0},
{h,h,t,t},
{h,t,0,0},
{h,0,0,0},
{0,0,0,0},
{h,0,0,0},
{g,0,0,0},
{g,0,0,0},
{t,0,0,0},
{t,0,0,0},
{0,0,0,0},
{0,0,0,0},
{m,0,0,0},
{f,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{m,0,0,0},
{f,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0},
{t,0,0,0},
{t,0,0,0},
{0,0,0,0},
{t,p,0,0},
{t,t,0,0},
{x,0,0,0},
{l,h,h,0},
{0,0,0,0},
{h,0,0,0},
{h,0,0,0},
{l,f,f,0},
{l,f,f,f},
{l,f,f,0},
{l,f,f,f},
{0,0,0,0},
{0,0,0,0}
};
       void *	cmdmap[217];
       char *	bcdirnames[12]={"kkpclversion",
"kkmoduletable",
"kkdlltable",
"kkdllproctable",
"kksymboltable",
"kktypetable",
"kkgenfieldnames",
"kkgenfielddata",
"kkstringtable",
"kkstructtable",
"kkpccode",
"kkend"
};
       char *	hostfnnames[72]={"host_dummy",
"host_startprint",
"host_startprintcon",
"host_strstartprint",
"host_setformat",
"host_endprint",
"host_strendprint",
"host_print",
"host_dprint",
"host_println",
"host_printnogap",
"host_readln",
"host_sreadln",
"host_sread",
"host_rereadln",
"host_reread",
"host_strtoval",
"host_tostr",
"host_leftstr",
"host_rightstr",
"host_convlc",
"host_convuc",
"host_iconvlc",
"host_iconvuc",
"host_stop",
"host_stopx",
"host_ismain",
"host_waitkey",
"host_testkey",
"host_execwait",
"host_execcmd",
"host_shellexec",
"host_system",
"host_makestr",
"host_makestrslice",
"host_makeref",
"host_new",
"host_newheap",
"host_readlines",
"host_heapvar",
"host_dictitems",
"host_freeheap",
"host_setoverload",
"host_getcmdparam",
"host_gethostname",
"host_setpcerror",
"host_setdebug",
"host_test",
"host_ticks",
"host_sleep",
"host_random",
"host_findmetafunction",
"host_gethash",
"host_getos",
"host_gethostsize",
"host_iswindows",
"host_setmesshandler",
"host_setfprintf",
"host_loadpcl",
"host_runpcl",
"host_runtask",
"host_callext",
"host_pcldata",
"host_getcstring",
"host_getparam",
"host_clearlist",
"host_makelink",
"host_allparams",
"host_stackvars",
"host_makeempty",
"host_errorinfo",
"host_last"
};
       int32	hostnparams[72]={0,
1,
0,
0,
1,
0,
0,
2,
2,
0,
0,
1,
1,
1,
0,
0,
2,
2,
3,
3,
2,
2,
2,
2,
0,
1,
1,
0,
0,
3,
3,
2,
1,
2,
2,
2,
4,
4,
1,
1,
1,
1,
3,
1,
0,
1,
1,
2,
0,
1,
1,
1,
1,
0,
0,
0,
1,
2,
2,
2,
2,
3,
2,
1,
1,
1,
1,
1,
0,
1,
1,
0
};
       int32	hostisfn[72]={0,
0,
0,
0,
0,
0,
1,
0,
0,
0,
0,
0,
1,
1,
0,
0,
1,
1,
1,
1,
1,
1,
0,
0,
0,
0,
1,
1,
1,
1,
1,
1,
1,
1,
1,
1,
1,
1,
1,
1,
1,
0,
0,
1,
1,
0,
0,
1,
1,
0,
1,
1,
1,
1,
1,
1,
0,
0,
1,
1,
1,
0,
1,
1,
1,
0,
1,
1,
1,
1,
1,
0
};
       char *	namenames[20]={"nullid",
"programid",
"moduleid",
"extmoduleid",
"dllmoduleid",
"typeid",
"procid",
"dllprocid",
"dllvarid",
"constid",
"staticid",
"frameid",
"paramid",
"fieldid",
"genfieldid",
"enumid",
"labelid",
"blockid",
"aliasid",
"linkid"
};
       char *	errornames[7]={"pc_error","user_error","type_error","mixedtype_error","divide_error","stopmodule_error","bounds_error"};

// From module: var_decls
/* Local Static Variables */
       char *	usercatnames[3]={"std_cat","anon_cat","user_cat"};
       int32	ntypes;
       int32	ttmodule[300];
       strec *	ttnamedef[300];
       int32	ttbasetype[300];
       char *	ttname[300];
       int32	ttbitwidth[300];
       int32	ttsize[300];
       int32	ttlower[300];
       int32	ttlength[300];
       int32	ttstartfield[300];
       int32	ttstructfields[300];
       int32	tttarget[300];
       byte	ttusercat[300];
       byte	typestarterset[300];
       char *	objtypenames[3]={"normal_obj","slice_obj","extslice_obj"};
       objrec *	chrtable[256];
       modulerec	moduletable[51];
       int32	nmodules;
       char *	pendingmodules[50];
       int32	npendingmodules;
       int32	currmoduleno;
       modulerec *	currmodule;
       char *	searchdirs[6];
       int32	nsearchdirs=0;
       strec *	stprogram;
       strec *	stmodule;
       strec *	stsysmodule;
       strec *	alldeflist;
       void *	logdev;
       int32	optflag=0;
       genfieldnamerec	genfieldnames[500];
       genfielddatarec	genfielddata[500];
       int32	ngenfieldnames;
       int32	ngenfielddata;
       unitrec *	nullunit;
       char *	libpaths[10];
       int32	nlibpaths;
       int32	fverbose=0;
       int32	ftrace=0;
       int32	fdtrace=0;
       int32	foptimise=0;
       int32	mlineno=0;
       int32	tlex=0;
       int32	exportsprepass=0;
       int32	debug=0;
       int32	NNAMES;
       int32	NCHECKS;
       int32	FORCHECK;
       int32	NCLASHES;
       int32	NLOOKUPS;
       int32	ALLNAMES;
       int32	ALLFOUNDNAMES;
       int32	ALLNOTFOUNDNAMES;
       varrec	ttdeststrv;
       varrec *	ttdeststr=&ttdeststrv;
       int32	totalstrings=0;
       char *	dispatchnames[4]={"-lab","-fn","-deb","-asm"};
       varrec *	sptr;
       byte *	frameptr;
       int64 *	pcptr;
       varrec (*varstack)[];
       objrec (*stringobjtable)[];
       int32	dllindex;
       int32	dllcallindex;
       int64	dllparams[30];
       int64	dllcallstack[30];
       int32	ndlltable;
       int32	ndllproctable;
       char *	dlltable[50];
       uint64	dllinsttable[50];
       dllprocrec	dllproctable[500];
       void (*fprintf_ptr)	(void);
       void (*fgets_ptr)	(void);
       void (*pcl_callbackfn)	(void)=0;
       char *	pcerror_mess=0;
       int32	ncmdparams;
       char *	cmdparamtable[33];
       procrec *	proclist;
       int32	nproclist;
       int32	nstrings=0;
       int32	nsymbols=0;
       int32	nstructfields=0;
       char * (*stringtable)[];
       strec (*pcsymboltable)[];
       fieldrec (*pcfieldtable)[];
       int32	runfrompc=0;
       char *	extrafiles[10];
       char *	extratext[10];
       int32	extrasizes[10];
       int32	nextra;
       char *	err_message;
       varrec	err_var1;
       varrec	err_var2;
       int64 *	err_pcptr;
       int64 *	stopseq;
       int64 *	raiseseq;
       int32	NSTRINGEQ=0;
       int32	NSTRINGCMP=0;
       byte *	PPP;
       int64 (*pccode)[];
       int32	npccode=0;
       int32	pcindex;
       uint16 (*linetable)[];
       int32	cmdnopnds[217];
       int32	lastticks=0;

// From module: pc_misc

// From module: support
/* Local Static Variables */
       byte	bytemasks[8]={1,2,4,8,16,32,64,128};

// From module: var_objects
/* Local Static Variables */
       objrec *	emptylist=0;
       objrec *	emptystring=0;
       objrec *	emptyset=0;
static objrec	zeroobj;
static objrec	deflistobj;

// From module: var_ops

// From module: pc_bigint

// From module: var_lib

// From module: var_print
/* Local Static Variables */
       int32	mindev;
       int32	moutdev;
       int32 *	minchan;
       void *	moutchan;
       varrec	minvar;
       varrec	moutvar;
       char *	mfmtstr;
       char *	mfmtcurr;
       fmtrec	defaultfmt={0,
0,
10,
0,
' ',
'f',
0,
0,
0,
'R',
0,
0,
0,
{0,0,0}
};
       void *	testfilech;
static int32	moutdevstack[6];
static void *	moutchanstack[6];
static varrec	moutvarstack[6];
static byte	mgapstack[6];
static char *	mfmtstrstack[6];
static char *	mfmtcurrstack[6];
static int32	noclevels;
static byte	mgapneeded;
static int32	listdepth=0;
static char	digits[16]={'0',
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
static char *	kb_start;
static char *	kb_pos;
static char *	kb_lastpos;
static int32	kb_size;
static int32	kb_linelength;
static int32	kb_length;
static int32	kb_lastlength;
static char	termchar;
static int32	itemerror;

// From module: mm_nosdll

// From module: pc_oslayer

// From module: pc_host
/* Local Static Variables */
static procrec *	proclistptr;
static void (*hosttable[70])	(void)={(void (*)	(void))&pch_startprint,
(void (*)	(void))&pch_startprintcon,
(void (*)	(void))&pch_strstartprint,
(void (*)	(void))&pch_setformat,
(void (*)	(void))&pch_endprint,
(void (*)	(void))&pch_strendprint,
(void (*)	(void))&pch_print,
(void (*)	(void))&pch_dprint,
(void (*)	(void))&pch_println,
(void (*)	(void))&pch_printnogap,
(void (*)	(void))&pch_readln,
(void (*)	(void))&pch_sreadln,
(void (*)	(void))&pch_sread,
(void (*)	(void))&pch_rereadln,
(void (*)	(void))&pch_reread,
(void (*)	(void))&pch_strtoval,
(void (*)	(void))&pch_tostr,
(void (*)	(void))&pch_leftstr,
(void (*)	(void))&pch_rightstr,
(void (*)	(void))&pch_convlc,
(void (*)	(void))&pch_convuc,
(void (*)	(void))&pch_iconvlc,
(void (*)	(void))&pch_iconvuc,
(void (*)	(void))&pch_stop,
(void (*)	(void))&pch_stopx,
(void (*)	(void))&pch_ismain,
(void (*)	(void))&pch_waitkey,
(void (*)	(void))&pch_testkey,
(void (*)	(void))&pch_execwait,
(void (*)	(void))&pch_execcmd,
(void (*)	(void))&pch_shellexec,
(void (*)	(void))&pch_system,
(void (*)	(void))&pch_makestr,
(void (*)	(void))&pch_makestrslice,
(void (*)	(void))&pch_makeref,
(void (*)	(void))&pch_new,
(void (*)	(void))&pch_newheap,
(void (*)	(void))&pch_readlines,
(void (*)	(void))&pch_heapvar,
(void (*)	(void))&pch_dictitems,
(void (*)	(void))&pch_freeheap,
(void (*)	(void))&pch_setoverload,
(void (*)	(void))&pch_getcmdparam,
(void (*)	(void))&pch_gethostname,
(void (*)	(void))&pch_setpcerror,
(void (*)	(void))&pch_setdebug,
(void (*)	(void))&pch_test,
(void (*)	(void))&pch_ticks,
(void (*)	(void))&pch_sleep,
(void (*)	(void))&pch_random,
(void (*)	(void))&pch_findmetafunction,
(void (*)	(void))&pch_gethash,
(void (*)	(void))&pch_getos,
(void (*)	(void))&pch_gethostsize,
(void (*)	(void))&pch_iswindows,
(void (*)	(void))&pch_setmesshandler,
(void (*)	(void))&pch_setfprintf,
(void (*)	(void))&pch_loadpcl,
(void (*)	(void))&pch_runpcl,
(void (*)	(void))&pch_runtask,
(void (*)	(void))&pch_callext,
(void (*)	(void))&pch_pcldata,
(void (*)	(void))&pch_getcstring,
(void (*)	(void))&pch_getparam,
(void (*)	(void))&pch_clearlist,
(void (*)	(void))&pch_makelink,
(void (*)	(void))&pch_allparams,
(void (*)	(void))&pch_stackvars,
(void (*)	(void))&pch_makeempty,
(void (*)	(void))&pch_errorinfo
};

// From module: pc_handlers
/* Local Static Variables */
       byte	stopped;

// From module: pc_assem

// From module: start
/* Local Static Variables */
static int32	floadpc=0;
static int32	fshowlog=0;
static int32	fruncode=0;
static int32	fshowtimes=0;
static int32	fshowfixedup=0;
static int32	logdest=0;
static int32	dopause=0;
static int32	totalpclopcodes=0;
static int32	totallines=0;
static int32	nstringobjects=0;
static int32	parse_tx;
static int32	sttree_tx;
static int32	stflat_tx;
static int32	last_tx;
static int32	pclcode_tx;
static int32	genfields_tx;
static int32 *	txpos=&pclcode_tx;
static int32	clockstart;
static int32	modulelevel;
static int32	dispatch_type=asm_dispatch;
static void *	handlertable[216]={0,
(void *)&k_nop,
(void *)&k_procstart,
(void *)&k_procend,
(void *)&k_endmodule,
(void *)&k_push_m,
(void *)&k_push_f,
(void *)&k_push_am,
(void *)&k_push_af,
(void *)&k_push_ap,
(void *)&k_push_al,
(void *)&k_push_ci,
(void *)&k_push_cw,
(void *)&k_push_cr,
(void *)&k_push_cn,
(void *)&k_push_cs,
(void *)&k_push_t,
(void *)&k_push_op,
(void *)&k_pushz,
(void *)&k_pushz_void,
(void *)&k_pushz_str,
(void *)&k_pushz_list,
(void *)&k_pushz_listl,
(void *)&k_pushz_set,
(void *)&k_pushz_arrayl,
(void *)&k_pop_m,
(void *)&k_pop_f,
(void *)&k_store_m,
(void *)&k_store_f,
(void *)&k_pushptr,
(void *)&k_popptr,
(void *)&k_storeptr,
(void *)&k_zpop_m,
(void *)&k_zpop_f,
(void *)&k_zstore_m,
(void *)&k_zstore_f,
(void *)&k_copy,
(void *)&k_swap,
(void *)&k_convptr,
(void *)&k_jump,
(void *)&k_jumpptr,
(void *)&k_jumptrue,
(void *)&k_jumpfalse,
(void *)&k_jumpdef,
(void *)&k_jumpvoid,
(void *)&k_jumpeq,
(void *)&k_jumpne,
(void *)&k_jumplt,
(void *)&k_jumple,
(void *)&k_jumpge,
(void *)&k_jumpgt,
(void *)&k_jumptesteq,
(void *)&k_jumptestne,
(void *)&k_jumplabel,
(void *)&k_jumpclabel,
(void *)&k_switch,
(void *)&k_cswitch,
(void *)&k_new,
(void *)&k_to_f,
(void *)&k_for_fci,
(void *)&k_for_ff,
(void *)&k_ford_fci,
(void *)&k_ford_ff,
(void *)&k_call,
(void *)&k_callptr,
(void *)&k_return,
(void *)&k_startdll,
(void *)&k_pushdll,
(void *)&k_calldll,
(void *)&k_callhost,
(void *)&k_stackframe,
(void *)&k_free,
(void *)&k_addsp,
(void *)&k_stop,
(void *)&k_test,
(void *)&k_makelist,
(void *)&k_makerecord,
(void *)&k_makearray,
(void *)&k_makestruct,
(void *)&k_makeset,
(void *)&k_makerange,
(void *)&k_makedict,
(void *)&k_pushdot,
(void *)&k_pushdotref,
(void *)&k_softconv,
(void *)&k_hardconv,
(void *)&k_mixed,
(void *)&k_incrptr,
(void *)&k_incrto_m,
(void *)&k_incrto_f,
(void *)&k_loadincr,
(void *)&k_incrload,
(void *)&k_decrptr,
(void *)&k_decrto_m,
(void *)&k_decrto_f,
(void *)&k_loaddecr,
(void *)&k_decrload,
(void *)&k_incr,
(void *)&k_decr,
(void *)&k_neg,
(void *)&k_abs,
(void *)&k_not,
(void *)&k_inot,
(void *)&k_istrue,
(void *)&k_asc,
(void *)&k_chr,
(void *)&k_sqrt,
(void *)&k_sqr,
(void *)&k_cube,
(void *)&k_sin,
(void *)&k_cos,
(void *)&k_tan,
(void *)&k_asin,
(void *)&k_acos,
(void *)&k_atan,
(void *)&k_sign,
(void *)&k_ln,
(void *)&k_log,
(void *)&k_lg,
(void *)&k_exp,
(void *)&k_round,
(void *)&k_floor,
(void *)&k_ceil,
(void *)&k_fract,
(void *)&k_negto,
(void *)&k_absto,
(void *)&k_notto,
(void *)&k_inotto,
(void *)&k_len,
(void *)&k_lwb,
(void *)&k_upb,
(void *)&k_bounds,
(void *)&k_bits,
(void *)&k_bytes,
(void *)&k_type,
(void *)&k_elemtype,
(void *)&k_basetype,
(void *)&k_minval,
(void *)&k_maxval,
(void *)&k_isint,
(void *)&k_isreal,
(void *)&k_isstring,
(void *)&k_isrange,
(void *)&k_isnumber,
(void *)&k_isarray,
(void *)&k_isrecord,
(void *)&k_ispointer,
(void *)&k_ismutable,
(void *)&k_isset,
(void *)&k_isvoid,
(void *)&k_isdef,
(void *)&k_tostr,
(void *)&k_isequal,
(void *)&k_add,
(void *)&k_sub,
(void *)&k_mul,
(void *)&k_div,
(void *)&k_idiv,
(void *)&k_rem,
(void *)&k_divrem,
(void *)&k_iand,
(void *)&k_ior,
(void *)&k_ixor,
(void *)&k_shl,
(void *)&k_shr,
(void *)&k_in,
(void *)&k_notin,
(void *)&k_inrev,
(void *)&k_eq,
(void *)&k_ne,
(void *)&k_lt,
(void *)&k_le,
(void *)&k_ge,
(void *)&k_gt,
(void *)&k_min,
(void *)&k_max,
(void *)&k_concat,
(void *)&k_append,
(void *)&k_power,
(void *)&k_atan2,
(void *)&k_addto,
(void *)&k_subto,
(void *)&k_multo,
(void *)&k_divto,
(void *)&k_idivto,
(void *)&k_iandto,
(void *)&k_iorto,
(void *)&k_ixorto,
(void *)&k_shlto,
(void *)&k_shrto,
(void *)&k_minto,
(void *)&k_maxto,
(void *)&k_concatto,
(void *)&k_appendto,
(void *)&k_pushix,
(void *)&k_pushdotix,
(void *)&k_pushkeyix,
(void *)&k_pushkeyixd,
(void *)&k_pushixref,
(void *)&k_pushdotixref,
(void *)&k_pushkeyixref,
(void *)&k_pushbyteix,
(void *)&k_pushbyteixref,
(void *)&k_appendset,
(void *)&k_pushdotm,
(void *)&k_pushdott,
(void *)&k_push_ad,
(void *)&k_push_try,
(void *)&k_raise,
(void *)&k_applyop,
(void *)&k_makeiter,
(void *)&k_forall,
(void *)&k_forallx,
(void *)&k_foreach,
(void *)&k_foreachx,
(void *)&k_expandrange
};

// From module: mm_clib

// From module: mm_mainc
int32 main	(int32 nparams,char * * params0) {
int32	t;
int32	i;
char * (*params)[];
    params = (char * (*)[])params0;
    nsysparams = nparams;
    if (nsysparams > maxparam) {
        printf("%s\n","Too many params");
        exit(0);
    }
    for (i=1; i<=nparams; ++i) {
        sysparams[i-1] = (*params)[i-1];
    }
    t = clock();
    start();
    printf("%s %d\n","Time=",clock()-t);
    exit(0);
    return 0;
}

uint64 m_dotslice	(uint64 a,int32 i,int32 j) {
    if (i >= j) {
        return a>>j&~(18446744073709551615ull<<(i-j)+1);
    }
    else {
        return a>>i&~(18446744073709551615ull<<(j-i)+1);
    }
}

uint64 m_anddotslice	(uint64 a,int32 i,int32 j) {
    if (i >= j) {
        return (a>>j&~(18446744073709551615ull<<(i-j)+1))<<j;
    }
    else {
        return (a>>i&~(18446744073709551615ull<<(j-i)+1))<<i;
    }
}

int64 m_imin	(int64 a,int64 b) {
    if (a <= b) {
        return a;
    }
    else {
        return b;
    }
}

int64 m_imax	(int64 a,int64 b) {
    if (a >= b) {
        return a;
    }
    else {
        return b;
    }
}

uint64 m_min	(uint64 a,uint64 b) {
    if (a <= b) {
        return a;
    }
    else {
        return b;
    }
}

uint64 m_max	(uint64 a,uint64 b) {
    if (a >= b) {
        return a;
    }
    else {
        return b;
    }
}


// From module: mm_nos
void os_init	(void) {
    init_flag = 1;
}

int32 os_execwait	(char * cmdline,int32 newconsole,char * workdir) {
    system(cmdline);
    return 0;
}

int32 os_execcmd	(char * cmdline,int32 newconsole) {
    abortprogram("exitcmd");
    return 0;
}

int32 os_getch	(void) {
    return 0;
}

int32 os_kbhit	(void) {
    abortprogram("kbhit");
    return 0;
}

void os_flushkeys	(void) {
    abortprogram("flushkeys");
}

void * os_getconsolein	(void) {
    return 0;
}

void * os_getconsoleout	(void) {
    return 0;
}

void * os_proginstance	(void) {
    abortprogram("PROGINST");
    return 0;
}

uint64 os_getdllinst	(char * name) {
    return 0;
}

void (*os_getdllprocaddr	(int32 hinst,char * name))	(void) {
static rec	table[23]={{"malloc",(void (*)	(void))&malloc},
{"realloc",(void (*)	(void))&realloc},
{"free",(void (*)	(void))&free},
{"printf",(void (*)	(void))&printf},
{"puts",(void (*)	(void))&puts},
{"fgetc",(void (*)	(void))&fgetc},
{"fputc",(void (*)	(void))&fputc},
{"getchar",(void (*)	(void))&getchar},
{"fopen",(void (*)	(void))&fopen},
{"fclose",(void (*)	(void))&fclose},
{"fseek",(void (*)	(void))&fseek},
{"ftell",(void (*)	(void))&ftell},
{"fread",(void (*)	(void))&fread},
{"fwrite",(void (*)	(void))&fwrite},
{"clock",(void (*)	(void))&clock},
{"isalpha",(void (*)	(void))&isalpha},
{"tolower",(void (*)	(void))&tolower},
{"feof",(void (*)	(void))&feof},
{"memset",(void (*)	(void))&memset},
{"memcpy",(void (*)	(void))&memcpy},
{"ungetc",(void (*)	(void))&ungetc},
{"remove",(void (*)	(void))&remove},
{"system",(void (*)	(void))&system}
};
int32	i;
    for (i=1; i<=23; ++i) {
        if (strcmp(table[i-1].name,name) == 0) {
            return (void (*)	(void))table[i-1].addr;
        }
    }
    return 0;
}

void os_initwindows	(void) {
}

int32 os_getchx	(void) {
    abortprogram("getchx");
    return 0;
}

char * os_getos	(void) {
    return "NO-OS";
}

int32 os_getoscode	(void) {
    return 'N';
}

int32 os_iswindows	(void) {
static int32	iswindows=-1;
void *	f;
    if (iswindows == -1) {
        f = fopen("c:/windows/notepad.exe","rb");
        if (f) {
            fclose(f);
            iswindows = 1;
        }
        else {
            iswindows = 0;
        }
    }
    return iswindows;
}

int32 os_shellexec	(char * opc,char * file) {
    abortprogram("SHELL EXEC");
    return 0;
}

void os_sleep	(int32 a) {
    abortprogram("sleep");
}

void * os_getstdin	(void) {
    return fopen("con","rb");
}

void * os_getstdout	(void) {
    return fopen("con","rb");
}

char * os_gethostname	(void) {
    return "";
}

int32 os_gethostsize	(void) {
    return 32;
}

char * os_getmpath	(void) {
    return "";
}

void os_exitprocess	(int32 x) {
    exit(0);
}

int64 os_gettimestamp	(void) {
    return clock();
}

int64 os_gettickcount	(void) {
    return clock();
}

int64 os_clock	(void) {
    if (!!os_iswindows()) {
        return clock();
    }
    else {
        return (clock()/ 1000);
    }
}

int64 os_getclockspersec	(void) {
    return (!!os_iswindows()?1000:1000000);
}

void os_setmesshandler	(void * addr) {
    abortprogram("SETMESSHANDLER");
}

int64 os_filelastwritetime	(char * filename) {
    return 0;
}


// From module: mm_mlib
void * pcm_alloc	(int32 n) {
byte *	p;
int32	i;
    if (n > maxblocksize) {
        alloccode = pcm_getac(n);
        allocbytes = allocupper[alloccode];
        p = (byte *)allocmem(allocbytes);
        if (!p) {
            abortprogram("pcm_alloc failure");
        }
        return p;
    }
    alloccode = sizeindextable[n];
    if (alloccode == 0) {
        alloccode = 1;
    }
    allocbytes = allocupper[alloccode];
    if (!!(p = (byte *)freelist[alloccode])) {
        freelist[alloccode] = (uint32 *)*freelist[alloccode];
        return p;
    }
    p = pcheapptr;
    pcheapptr += allocbytes;
    if (pcheapptr >= pcheapend) {
        p = (byte *)pcm_newblock(allocbytes);
        return p;
    }
    return p;
}

void pcm_free	(void * p,int32 n) {
int32	acode;
    if (n > maxblocksize) {
        free(p);
        return;
    }
    if (p) {
        acode = sizeindextable[n];
        smallmemtotal -= allocupper[acode];
        *(uint32 *)p = (uint32)freelist[acode];
        freelist[acode] = (uint32 *)p;
    }
}

void pcm_freeac	(void * p,int32 alloc) {
    pcm_free(p,allocupper[alloc]);
}

void pcm_copymem4	(void * p,void * q,int32 n) {
    memcpy((int32 *)p,(int32 *)q,n);
}

void pcm_clearmem	(void * p,int32 n) {
    memset((int32 *)p,0,n);
}

void pcm_init	(void) {
int32	i;
int32	j;
int32	k;
int32	k1;
int32	k2;
int64	size;
static int64	limit=2147483648ll;
static int32	finit=0;
    if (finit) {
        return;
    }
    pcm_newblock(0);
    for (i=1; i<=maxblocksize; ++i) {
        j = 1;
        k = 16;
        while (i > k) {
            k = k<<1;
            ++j;
        }
        sizeindextable[i] = j;
    }
    allocupper[1] = 16;
    size = 16;
    for (i=2; i<=27; ++i) {
        size *= 2;
        allocupper[i] = size;
        if (size >= threshold) {
            k = i;
            goto L11;
        }
    }
L11:;
    for (i=k+1; i<=255; ++i) {
        size += alloc_step;
        if (size < limit) {
            allocupper[i] = size;
            maxmemory = size;
        }
        else {
            maxalloccode = i-1;
            goto L15;
        }
    }
L15:;
    finit = 1;
}

int32 pcm_getac	(int32 size) {
    if (size <= maxblocksize) {
        return sizeindextable[size];
    }
    size = size+255>>8;
    if (size <= maxblocksize) {
        return sizeindextable[size]+8;
    }
    size = size+63>>6;
    if (size <= maxblocksize) {
        return sizeindextable[size]+14;
    }
    size = ((size-2048)+2047)/ 2048+22;
    return size;
}

uint32 * pcm_newblock	(int32 itemsize) {
static int32	totalheapsize;
byte *	p;
    totalheapsize += pcheapsize;
    p = (byte *)allocmem(pcheapsize);
    if (p == 0) {
        abortprogram("Can't alloc pc heap");
    }
    pcheapptr = p;
    pcheapend = p+pcheapsize;
    if (pcheapstart == 0) {
        pcheapstart = p;
    }
    pcheapptr += itemsize;
    return (uint32 *)p;
}

int32 pcm_round	(int32 n) {
static int32	allocbytes[9]={0,16,32,64,128,256,512,1024,2048};
    if (n > maxblocksize) {
        return n;
    }
    else {
        return allocbytes[sizeindextable[n]];
    }
}

int32 pcm_array	(int32 n) {
int32	m;
    if (n <= maxblocksize) {
        return pcm_round(n);
    }
    else {
        m = 2048;
        while (n > m) {
            m <<= 1;
        }
        return m;
    }
}

void pcm_printfreelist	(int32 size,uint32 * p) {
    printf("%s %d\n","Size: ",size);
    while (p) {
        printf(" %X",(uint32)p);
        p = (uint32 *)*p;
    }
    puts("");
}

void pcm_diags	(char * caption) {
int32	i;
int32	m;
    printf("%s %s\n","HEAP FREELISTS:",caption);
    m = 16;
    for (i=1; i<=8; ++i) {
        pcm_printfreelist(m,freelist[i]);
        m <<= 1;
    }
}

void * pcm_allocz	(int32 n) {
void *	p;
    p = pcm_alloc(n);
    memset((int32 *)p,0,n);
    return p;
}

char * pcm_copyheapstring	(char * s) {
char *	q;
int32	n;
    n = strlen(s)+1;
    q = (char *)pcm_alloc(n);
    memcpy((int32 *)q,(int32 *)s,n);
    return q;
}

static void addtomemalloc	(int32 * ptr,int32 size) {
int32	i;
    printf("%s %p %d\n","***************ADD TO ALLOC:",(void*)(ptr),size);
    for (i=1; i<=maxmemalloc; ++i) {
        if (memalloctable[i-1] == ptr) {
            printf("%s %p %s\n","ALLOC ERROR:",(void*)(ptr),"ALREADY ALLOCATED\n\n\n");
            exit(0);
        }
        if (memalloctable[i-1] == 0) {
            memalloctable[i-1] = ptr;
            memallocsize[i-1] = size;
            return;
        }
    }
    printf("%s\n","MEMALLOCTABLE FULL\n\n\n\n");
    os_getch();
    exit(0);
}

static void removefrommemalloc	(int32 * ptr,int32 size) {
int32	i;
    printf("%s %p %d\n","------------------************REMOVE FROM ALLOC:",(void*)(ptr),size);
    for (i=1; i<=maxmemalloc; ++i) {
        if (memalloctable[i-1] == ptr) {
            if (memallocsize[i-1] != size) {
                printf("%s %p %s %d %s %d\n","REMOVE:FOUND",(void*)(ptr),"IN MEMALLOCTABLE, FREESIZE=",size,", BUT STORED AS BLOCK SIZE:",memallocsize[i-1]);
                abortprogram("MEMSIZE");
            }
            memalloctable[i-1] = 0;
            return;
        }
    }
    printf("%s %p %s %d\n","CAN'T FIND",(void*)(ptr),"IN MEMALLOCTABLE",size);
    abortprogram("MEM");
    os_getch();
    exit(0);
}

void * allocmem	(int32 n) {
void *	p;
    p = malloc(n);
    if (p) {
        return p;
    }
    printf("%d %d\n",n,memtotal);
    abortprogram("Alloc mem failure");
    return 0;
}

void * reallocmem	(void * p,int32 n) {
    p = realloc(p,n);
    if (p) {
        return p;
    }
    printf("%d\n",n);
    abortprogram("Realloc mem failure");
    return 0;
}

void abortprogram	(char * s) {
    printf("%s\n",s);
    printf("%s","ABORTING: Press key...");
    os_getch();
    exit(0);
}

int32 getfilesize	(void * handlex) {
uint32	p;
uint32	size;
    p = ftell(handlex);
    fseek(handlex,0,2);
    size = ftell(handlex);
    fseek(handlex,p,seek_set);
    return size;
}

void readrandom	(void * handlex,byte * mem,int32 offset,int32 size) {
    fseek(handlex,offset,seek_set);
    fread((int32 *)mem,1,size,handlex);
}

void writerandom	(void * handlex,byte * mem,int32 offset,int32 size) {
    fseek(handlex,offset,seek_set);
    fwrite((int32 *)mem,1,size,handlex);
}

byte * readfile	(char * filename) {
void *	f;
int32	size;
byte *	m;
byte *	p;
    f = fopen(filename,"rb");
    if (f == 0) {
        return 0;
    }
    rfsize = size = getfilesize(f);
    m = (byte *)malloc(size+4);
    if (m == 0) {
        return 0;
    }
    readrandom(f,m,0,size);
    p = m+size;
    *p = 26;
    *(p+1) = 0;
    fclose(f);
    return m;
}

int32 writefile	(char * filename,byte * data,int32 size) {
void *	f;
    f = fopen(filename,"wb");
    if (f == 0) {
        return 0;
    }
    writerandom(f,data,0,size);
    return fclose(f);
}

int32 checkfile	(char * file) {
void *	f;
    if (!!(f = fopen(file,"rb"))) {
        fclose(f);
        return 1;
    }
    return 0;
}

void readlinen	(void * handlex,char * buffer,int32 size) {
char *	p;
int32	n;
char	buff[100];
byte	crseen;
    if (handlex == 0) {
        handlex = os_getstdin();
    }
    *buffer = 0;
    if (fgets(buffer,size-2,handlex) == 0) {
        return;
    }
    n = strlen(buffer);
    if (n == 0) {
        return;
    }
    p = buffer+n-1;
    crseen = 0;
    while (p >= buffer && ((uchar)*p == 13 || (uchar)*p == 10)) {
        if ((uchar)*p == 13 || (uchar)*p == 10) {
            crseen = 1;
        }
        *p-- = 0;
    }
    if (!crseen && n+4 > size) {
        printf("%d %d\n",size,n);
        abortprogram("line too long");
    }
}

void iconvlcn	(char * s,int32 n) {
int32	av_1;
    av_1 = n;
    while (av_1--) {
        *s = tolower((uchar)*s);
        ++s;
    }
}

void iconvucn	(char * s,int32 n) {
int32	av_2;
    av_2 = n;
    while (av_2--) {
        *s = toupper((uchar)*s);
        ++s;
    }
}

void convlcstring	(char * s) {
    while ((uchar)*s) {
        *s = tolower((uchar)*s);
        ++s;
    }
}

void convucstring	(char * s) {
    while ((uchar)*s) {
        *s = toupper((uchar)*s);
        ++s;
    }
}

char * changeext	(char * s,char * newext) {
static char	newfile[260];
char *	sext;
int32	n;
    strcpy((char *)&newfile,s);
    sext = extractext(s,0);
    if ((uchar)*sext == 0) {
        strcat((char *)&newfile,newext);
    }
    else {
        n = (sext-s)-1;
        strcpy((char *)&newfile+n+1,newext);
    }
    return (char *)&newfile;
}

char * extractext	(char * s,int32 period) {
char *	t;
char *	u;
    t = extractfile(s);
    if ((uchar)*t == 0) {
        return "";
    }
    u = t+strlen(t)-1;
    while (u >= t) {
        if ((uchar)*u == '.') {
            if ((uchar)*(u+1) == 0) {
                return (period?".":"");
            }
            return u+1;
        }
        --u;
    }
    return "";
}

char * extractpath	(char * s) {
static char	str[260];
char *	t;
int32	n;
    t = s+strlen(s)-1;
    while (t >= s) {
        switch ((uchar)*t) {
        case '\\':
        case '/':
        case ':':
            n = (t-s)+1;
            memcpy((int32 *)&str,(int32 *)s,n);
            str[n] = 0;
            return (char *)&str;
            break;
        default:;
        }
        --t;
    }
    return "";
}

char * extractfile	(char * s) {
char *	t;
    t = extractpath(s);
    if ((uchar)*t == 0) {
        return s;
    }
    return s+strlen(t);
}

char * extractbasefile	(char * s) {
static char	str[100];
char *	f;
char *	e;
int32	n;
int32	flen;
    f = extractfile(s);
    flen = strlen(f);
    if (flen == 0) {
        return "";
    }
    e = extractext(f,0);
    if (!!(uchar)*e) {
        n = (flen-strlen(e))-1;
        memcpy((int32 *)&str,(int32 *)f,n);
        str[n] = 0;
        return (char *)&str;
    }
    if ((uchar)*(f+flen-1) == '.') {
        memcpy((int32 *)&str,(int32 *)f,flen-1);
        str[flen-1] = 0;
        return (char *)&str;
    }
    return f;
}

char * addext	(char * s,char * newext) {
static char	newfile[260];
char *	sext;
int32	n;
    strcpy((char *)&newfile,s);
    sext = extractext(s,0);
    if ((uchar)*sext == 0) {
        strcat((char *)&newfile,newext);
        return (char *)&newfile;
    }
    else {
        return s;
    }
}

void * alloctable	(int32 n,int32 size) {
void *	p;
    p = malloc((n+1)*size);
    if (!p) {
        abortprogram("Alloctable failure");
    }
    return p;
}

void * zalloctable	(int32 n,int32 size) {
int32 *	p;
    p = (int32 *)alloctable(n,size);
    pcm_clearmem(p,(n+1)*size);
    return p;
}

void checkfreelists	(char * s) {
int32	i;
uint32 *	p;
uint32 *	q;
int64	aa;
    for (i=2; i<=2; ++i) {
        p = freelist[i];
        while (p) {
            aa = (int64)(int32)p;
            if ((aa&18446744073441116160ull) != 0 || aa < 1048576) {
                printf("%s %s %d %p %p\n",s,"FREE LIST ERROR",i,(void*)(p),(void*)(q));
                os_getch();
                exit(0);
            }
            q = p;
            p = (uint32 *)*p;
        }
    }
}

void * pcm_alloc32	(void) {
byte *	p;
int32	i;
    allocbytes = 32;
    if (!!(p = (byte *)freelist[2])) {
        freelist[2] = (uint32 *)*freelist[2];
        return p;
    }
    return pcm_alloc(32);
}

void pcm_free32	(void * p) {
int32	acode;
    *(uint32 *)p = (uint32)freelist[2];
    freelist[2] = (uint32 *)p;
}

void outbyte	(void * f,int32 x) {
    fwrite(&x,1,1,f);
}

void outword16	(void * f,uint32 x) {
    fwrite((int32 *)&x,2,1,f);
}

void outword	(void * f,uint32 x) {
    fwrite((int32 *)&x,4,1,f);
}

void outdword	(void * f,uint64 x) {
    fwrite((int32 *)&x,8,1,f);
}

int32 myeof	(void * f) {
int32	c;
    c = fgetc(f);
    if (c == c_eof) {
        return 1;
    }
    ungetc(c,f);
    return 0;
}

void * pcm_smallallocz	(int32 n) {
byte *	p;
int32	i;
    if ((alloccode = sizeindextable[n]) == 0) {
        alloccode = 1;
    }
    allocbytes = allocupper[alloccode];
    p = pcheapptr;
    pcheapptr += allocbytes;
    if (pcheapptr >= pcheapend) {
        p = (byte *)pcm_newblock(allocbytes);
        memset((int32 *)p,0,n);
        return p;
    }
    memset((int32 *)p,0,n);
    return p;
}

void * pcm_fastalloc	(int32 n) {
byte *	p;
int32	i;
    if ((alloccode = sizeindextable[n]) == 0) {
        alloccode = 1;
    }
    allocbytes = allocupper[alloccode];
    p = pcheapptr;
    pcheapptr += allocbytes;
    if (pcheapptr >= pcheapend) {
        p = (byte *)pcm_newblock(allocbytes);
        return p;
    }
    return p;
}


// From module: var_types

// From module: pq_common

// From module: var_decls

// From module: pc_misc
int64 * raiseexception	(int32 exceptno) {
varrec *	stackend;
    stackend = &(*varstack)[stacksize];
    while (1) {
        if (sptr >= stackend) {
            default_exception(exceptno);
        }
        if (sptr->tag == texception && (exceptno == 0 || sptr->exceptiontype == exceptno)) {
            goto L2;
        }
        if (!!sptr->hasref) {
            vx_ufree(sptr);
        }
        ++sptr;
    }
L2:;
    frameptr = (byte *)sptr+sptr->frameoffset;
    return (int64 *)sptr->ptr;
}

void raise_error	(int32 error_no) {
    (--sptr)->tagx = tint;
    sptr->value = error_no;
    err_pcptr = pcptr;
    pcptr = raiseseq;
}

static void default_exception	(int32 exceptno) {
    if ((exceptno==pc_error)) {
        pcerror("PC/ERROR");
    } else if ((exceptno==user_error)) {
        pcerror("USER/ERROR");
    } else if ((exceptno==type_error)) {
        pcptr = err_pcptr;
        pcustype_def(err_message,&err_var1);
    } else if ((exceptno==mixedtype_error)) {
        pcptr = err_pcptr;
        pcmxtypes_def(err_message,&err_var1,&err_var2);
    } else if ((exceptno==divide_error)) {
        pcptr = err_pcptr;
        pcerror("EXCEPTION/DIVIDE BY ZERO");
    } else if ((exceptno==stopmodule_error)) {
        printf("%s\n","STOPMODULEERROR");
    } else if ((exceptno==bounds_error)) {
        printf("%s\n","BOUNDSERROR");
    }
    else {
        printf("%s %s\n","Exception:",errornames[exceptno-1]);
    }
    exit(0);
}


// From module: support
void prterror	(char * mess) {
    printf("%s %s\n","Print error:",mess);
    os_getch();
    exit(0);
}

void gerror	(char * mess,unitrec * p) {
int32	lineno;
    if (p) {
        lineno = p->lineno;
    }
    else {
        lineno = mlineno;
    }
    printf("%s %s %s %d %s\n","Code gen error:",mess,"on line",lineno,stmodule->name);
    os_getch();
    exit(0);
}

void nxerror	(char * mess,unitrec * p) {
int32	lineno;
    if (p) {
        lineno = p->lineno;
    }
    else {
        lineno = 0;
    }
    printf("%s %s %s %d %s\n","NX error:",mess,"on line",lineno,stmodule->name);
    os_getch();
    exit(0);
}

int32 testelem	(byte (*p)[],int32 n) {
    return (!!((*p)[n>>3]&bytemasks[n&7])?1:0);
}

void setelem	(byte (*p)[],int32 n) {
    (*p)[n>>3] |= bytemasks[n&7];
}

void pcustype_def	(char * mess,varrec * x) {
int32	t;
    t = x->tag;
    showlinenumber();
    printf("%s %s %s %s\n","DEF:Type not supported: ",mess,":",ttname[t]);
    abortprogram("Stopping");
}

int64 * pcustype	(char * mess,varrec * x) {
    err_message = mess;
    err_var1 = *x;
    err_pcptr = pcptr;
    raise_error(type_error);
    return pcptr;
}

int64 * pcustypet	(char * mess,int32 t) {
static varrec	v;
    v.tagx = t;
    return pcustype(mess,&v);
}

void pcmxtypes_def	(char * mess,varrec * x,varrec * y) {
int32	s;
int32	t;
    s = x->tag;
    t = y->tag;
    showlinenumber();
    printf("%s %s %s %s %s %s\n","DEF:Mixed Types not supported:/",mess,"/:",ttname[s],":",ttname[t]);
    abortprogram("Stopping");
}

int64 * pcmxtypes	(char * mess,varrec * x,varrec * y) {
    err_message = mess;
    err_var1 = *x;
    err_var2 = *y;
    err_pcptr = pcptr;
    raise_error(mixedtype_error);
    return pcptr;
}

int64 * pcmxtypestt	(char * mess,int32 s,int32 t) {
static varrec	u;
static varrec	v;
    u.tagx = s;
    v.tagx = t;
    return pcmxtypes(mess,&u,&v);
}

char * gettypename	(int32 t) {
    return ttname[t];
}

void inittypetables	(void) {
int32	i;
int32	size;
int32	bitsize;
    for (i=0; i<=52; ++i) {
        ttname[i] = stdtypenames[i];
        ttbasetype[i] = i;
        if ((i==tintm) || (i==twordm) || (i==trefm)) {
            bitsize = 32;
        }
        else {
            bitsize = stdtypewidths[i];
        }
        switch (bitsize) {
        case 0:
            break;
        case 1:
        case 2:
        case 4:
            size = 1;
            break;
        default:;
            size = bitsize/ 8;
        }
        ttsize[i] = size;
        ttbitwidth[i] = bitsize;
        ttlower[i] = 1;
    }
    ntypes = 52;
    tttarget[trefvar] = tvariant;
}

void pcerror	(char * mess) {
    showlinenumber();
    printf("%s %s\n","PCERROR:",mess);
    os_getch();
    exit(0);
}

void vxunimpl	(char * mess) {
    showlinenumber();
    printf("%s %s\n","Unimplemented VX op:",mess);
    os_getch();
    exit(0);
}

void pclunimpl	(int32 cmd) {
    showlinenumber();
    if (cmd != klastcmd) {
        printf("%s %s\n","Unimplemented cmd:",cmdnames[cmd]);
    }
    else {
        printf("%s\n","J-opcode not allowed with -LAB or -FN");
    }
    abortprogram("Stopping");
}

char * convCstring	(char * svalue,int32 length) {
enum {strbufflen = 1000};
static char	strbuffer1[1000];
static char	strbuffer2[1000];
static char	strbuffer3[1000];
static char	strbuffer4[1000];
static char	strbuffer5[1000];
static char	strbuffer6[1000];
static int32	strindex=0;
static char *	table[6]={(char *)&strbuffer1,(char *)&strbuffer2,(char *)&strbuffer3,(char *)&strbuffer4,(char *)&strbuffer5,(char *)&strbuffer6};
char *	p;
    if (length >= strbufflen) {
        pcerror("ConvCstring>=300");
    }
    if (svalue == 0) {
        return "";
    }
    if (++strindex == 6) {
        strindex = 0;
    }
    p = table[strindex];
    memcpy((int32 *)p,(int32 *)svalue,length);
    *(p+length) = 0;
    return p;
}

int32 getintvalue	(varrec * p) {
    switch (p->tag) {
    case tint:
    case ttype:
        return p->value;
        break;
    case treal:
        return p->xvalue;
        break;
    default:;
        pcustype("getintvalue",p);
    }
    return 0;
}

int32 nextpoweroftwo	(int32 x) {
int32	a=1;
    if (x == 0) {
        return 0;
    }
    while (a < x) {
        a <<= 1;
    }
    return a;
}

static void showlinenumber	(void) {
int32	lineno;
int32	moduleno;
int32	count;
int64 *	ptr;
varrec *	s;
varrec *	send;
    findlinenumber(pcptr,&lineno,&moduleno);
    printlinenumber(lineno,moduleno,"");
    s = sptr;
    send = &(*varstack)[stacksize];
    count = 0;
    while (s <= send && count < 15) {
        if (s->tag == tretaddr) {
            ptr = (int64 *)(s->retaddr-3);
            findlinenumber(ptr,&lineno,&moduleno);
            printlinenumber(lineno,moduleno,"Called from:");
            ++count;
        }
        ++s;
    }
}

static void printlinenumber	(int32 lineno,int32 moduleno,char * calledfrom) {
    printf("%s %s %d %s %s\n",calledfrom,"LINE:",lineno,"in FILE:",moduletable[moduleno].filename);
}

void findlinenumber	(int64 * ptr,int32 * lineno,int32 * moduleno) {
int32	pcindex;
int32	i;
modulerec	m;
    *lineno = 0;
    pcindex = findpcindex(ptr,moduleno);
    if (pcindex) {
        m = moduletable[*moduleno];
        for (i=pcindex; i>=1; --i) {
            *lineno = (*m.linetable)[i];
            if (!!*lineno) {
                return;
            }
        }
    }
}

int32 findpcindex	(int64 * ptr,int32 * moduleno) {
int32	i;
int32	j;
int64 *	p;
int64 *	q;
    for (i=0; i<=nmodules; ++i) {
        p = (int64 *)moduletable[i].pccode;
        q = p+moduletable[i].pcindex;
        if (ptr >= p && ptr < q) {
            *moduleno = i;
            return (ptr-p)+1;
        }
    }
    return 0;
}

void showlinetable	(char * caption,int32 i) {
int32	j;
    printf("%s %d %d %s\n","MODULE",i,moduletable[i].pcindex,caption);
    for (j=7; j<=12; ++j) {
        printf("%s %d %u\n","\tLINE",j,(*moduletable[i].linetable)[j]);
    }
}

void writezstring	(void * f,char * s) {
int32	i;
int32	n;
int32	av_1;
    outbyte(f,zstring);
    n = strlen(s);
    av_1 = n;
    while (av_1--) {
        outbyte(f,(uchar)*s++);
    }
    outbyte(f,0);
}

void writezint	(void * f,int64 x) {
byte *	p;
int32	av_2;
    if (x >= 0 && x <= zmax) {
        outbyte(f,x);
    }
    else if (x >= 240 && x < 480) {
        outbyte(f,zint240);
        outbyte(f,(x-240));
    }
    else if (x >= 480 && x < 720) {
        outbyte(f,zint480);
        outbyte(f,(x-480));
    }
    else if (x >= 720 && x < 960) {
        outbyte(f,zint720);
        outbyte(f,(x-720));
    }
    else if (x >= -127 && x < 0) {
        outbyte(f,zint1);
        outbyte(f,-x);
    }
    else if (x >= -32768 && x <= 32767) {
        outbyte(f,zint2);
        outword16(f,x);
    }
    else if (x > -2147483648ll && x <= 2147483647) {
        outbyte(f,zint4);
        outword(f,x);
    }
    else {
        p = (byte *)&x;
        outbyte(f,zint8);
        av_2 = 8;
        while (av_2--) {
            outbyte(f,*p++);
        }
    }
}

void writezint4	(void * f,int32 x) {
    outbyte(f,zint4);
    outword(f,x);
}

void writezrange	(void * f,byte * p) {
int32	av_3;
    outbyte(f,zint8);
    av_3 = 8;
    while (av_3--) {
        outbyte(f,*p++);
    }
}

void writezreal	(void * f,double x) {
byte *	p;
int32 *	q;
int32	av_4;
int32	av_5;
    p = (byte *)&x;
    q = (int32 *)&x;
    if (q != 0) {
        outbyte(f,zreal8);
        av_4 = 8;
        while (av_4--) {
            outbyte(f,*p++);
        }
    }
    else {
        outbyte(f,zreal4);
        p += 4;
        av_5 = 4;
        while (av_5--) {
            outbyte(f,*p++);
        }
    }
}

void writezeof	(void * f) {
    outbyte(f,zeof);
}

static void zerror	(char * mess) {
    printf("%s %s\n","Z error:",mess);
    exit(0);
}

int32 readzvalue	(byte * * pp,int32 * dest,int32 * dest2) {
byte *	p;
int8 *	sp;
int64 *	destint;
double *	destreal;
int32	t;
byte	bb;
byte	c;
int32	length;
int32	dummy;
    p = *pp;
    bb = *p++;
    t = 1;
    if (dest2 == 0) {
        dest2 = &dummy;
    }
    switch (bb) {
    case zint240:
        *dest = *p+++240;
        break;
    case zint480:
        *dest = *p+++480;
        break;
    case zint720:
        *dest = *p+++720;
        break;
    case zint1:
        *dest = -*(int8 *)p;
        ++p;
        break;
    case zint2:
        *dest = *(int16 *)p;
        p += 2;
        break;
    case zint4:
        *dest = *(int32 *)p;
        p += 4;
        break;
    case zint8:
        destint = (int64 *)dest;
        *destint = *(int64 *)p;
        p += 8;
        t = 2;
        break;
    case zreal4:
        *dest++ = 0;
        *dest = *(int32 *)p;
        p += 4;
        t = 3;
        break;
    case zreal8:
        destreal = (double *)dest;
        *destreal = *(double *)p;
        p += 8;
        t = 3;
        break;
    case zstring:
        *dest = (int64)(int32)p;
        length = 0;
        do {
            c = *p++;
            ++length;
        } while (!!c);
        *dest2 = --length;
        t = 4;
        break;
    case zbytes:
        zerror("Can't deal with ZBYTES yet");
        exit(0);
        break;
    case zeof:
        return 0;
        break;
    default:;
        *dest = bb;
    }
    *pp = p;
    return t;
}

int64 readzint	(byte * * p) {
int64	aa;
int32	status;
    aa = 0;
    status = readzvalue(p,(int32 *)&aa,0);
    if ((status==1)) {
        if ((int32)aa < 0) {
            aa |= 18446744069414584320ull;
        }
    } else if ((status==2)) {
    }
    else {
        zerror("Z:Int32 Expected");
    }
    return aa;
}

int64 readzdint	(byte * * p) {
int64	aa;
int32	status;
    aa = 0;
    if ((status = readzvalue(p,(int32 *)&aa,0)) != 2) {
        if (status == 1) {
            if (aa > 2147483647) {
                aa |= 18446744071562067968ull;
            }
        }
        else {
            zerror("ZformatD");
        }
    }
    return aa;
}

double readzreal	(byte * * p) {
double	x;
int32	status;
    if ((status = readzvalue(p,(int32 *)&x,0)) != 3) {
        zerror("ZformatR");
    }
    return x;
}

char * readzstring	(byte * * p,int32 * ilength) {
int64	aa;
int32	length;
int32	status;
    if ((status = readzvalue(p,(int32 *)&aa,&length)) != 4) {
        zerror("ZformatS");
    }
    if (ilength) {
        *ilength = length;
    }
    return (char *)(int32)aa;
}

void checkmt	(int32 id) {
    printf("%s %d %s %p\n","CHECKMT",id,":",(void*)(moduletable[1].pccode));
}

int64 ipower	(int64 a,int32 n) {
    if (n <= 0) {
        return 0;
    }
    else if (n == 0) {
        return 1;
    }
    else if (n == 1) {
        return a;
    }
    else if ((n&1) == 0) {
        return ipower(a*a,n/ 2);
    }
    else {
        return a*ipower(a*a,(n-1)/ 2);
    }
}


// From module: var_objects
objrec * newobject	(void) {
objrec *	p;
    p = (objrec *)pcm_alloc32();
    *p = zeroobj;
    return p;
}

void freeobject	(objrec * p) {
    pcm_free32(p);
}

objrec * addref_obj	(objrec * p) {
    ++p->refcount;
    return p;
}

void makezobjects	(void) {
    emptyset = newobject();
    emptyset->refcount = 5;
    emptylist = newobject();
    emptylist->refcount = 5;
    emptylist->lower = 1;
    emptystring = newobject();
    emptystring->refcount = 5;
    emptystring->lower = 1;
    emptystring->elemtag = tu8;
    deflistobj.refcount = 1;
    deflistobj.mutable = 1;
    deflistobj.elemtag = 0;
    deflistobj.lower = 1;
    zeroobj.refcount = 1;
}

objrec * make_str	(char * s,int32 length) {
objrec *	p;
    if (s == 0) {
        s = "";
    }
    if (length == -1) {
        length = strlen(s);
    }
    p = make_stringobj(length);
    memcpy((int32 *)p->strptr,(int32 *)s,length);
    return p;
}

varrec * make_listdata	(int32 length,int32 * allocated,int32 clear) {
byte *	p;
    if (length == 0) {
        *allocated = 0;
        return 0;
    }
    p = (byte *)pcm_alloc(length*varsize);
    if (clear) {
        pcm_clearmem(p,allocbytes);
    }
    *allocated = allocbytes/ varsize;
    return (varrec *)p;
}

void free_listdata	(varrec * p,int32 allocated) {
    free_arraydata((byte *)p,tvariant,allocated);
}

byte * make_arraydata	(int32 length,int32 elemtype,int32 * allocated,int32 clear) {
byte *	p;
int32	elemsize;
    if (length == 0) {
        *allocated = 0;
        return 0;
    }
    elemsize = ttsize[elemtype];
    p = (byte *)pcm_alloc(length*elemsize);
    if (clear) {
        pcm_clearmem(p,allocbytes);
    }
    *allocated = allocbytes/ elemsize;
    return p;
}

void free_arraydata	(byte * p,int32 elemtype,int32 allocated) {
    if (allocated) {
        pcm_free(p,allocated*ttsize[elemtype]);
    }
}

byte * make_bitdata	(int32 length,int32 elemtype,int32 * allocated,int32 clear) {
byte *	p;
int32	bitwidthx;
int32	nbits;
int32	nbytes;
    if (length == 0) {
        *allocated = 0;
        return 0;
    }
    bitwidthx = ttbitwidth[elemtype];
    nbits = length*bitwidthx;
    nbytes = ((nbits-1)/ 32+1)*4;
    p = (byte *)pcm_alloc(nbytes);
    if (clear) {
        pcm_clearmem(p,allocbytes);
    }
    *allocated = allocbytes*(8/ bitwidthx);
    return p;
}

void free_bitdata	(byte * p,int32 elemtype,int32 allocated) {
    if (allocated) {
        pcm_free(p,allocated*ttbitwidth[elemtype]/ 8);
    }
}

objrec * make_listobj	(int32 length,int32 lower) {
objrec *	p;
varrec *	q;
int32	av_1;
    p = (objrec *)pcm_alloc32();
    *p = deflistobj;
    if (length) {
        q = (varrec *)pcm_alloc(length*varsize);
        p->allocated = allocbytes/ varsize;
    }
    else {
        q = 0;
    }
    p->vptr = q;
    p->length = length;
    p->lower = lower;
    av_1 = length;
    while (av_1--) {
        q->tagx = tvoid;
        ++q;
    }
    return p;
}

objrec * make_stringobj	(int32 length) {
objrec *	p;
    p = newobject();
    p->vptr = (varrec *)make_arraydata(length,tu8,&(p)->allocated,0);
    p->mutable = 1;
    p->length = length;
    p->lower = 1;
    p->elemtag = tu8;
    return p;
}

objrec * make_arrayobj	(int32 length,int32 elemtype,int32 lower) {
objrec *	p;
    p = newobject();
    p->vptr = (varrec *)make_arraydata(length,elemtype,&(p)->allocated,1);
    p->mutable = 1;
    p->length = length;
    p->lower = lower;
    p->elemtag = elemtype;
    return p;
}

objrec * make_setobj	(int32 length) {
objrec *	p;
    p = newobject();
    p->ptr = make_bitdata(length,tbit,&(p)->allocated,1);
    p->mutable = 1;
    p->length = length;
    p->lower = 0;
    p->elemtag = tbit;
    return p;
}

objrec * make_bitsobj	(int32 length,int32 elemtype,int32 lower) {
objrec *	p;
    p = newobject();
    p->ptr = make_bitdata(length,elemtype,&(p)->allocated,1);
    p->mutable = 1;
    p->length = length;
    p->lower = lower;
    p->elemtag = elemtype;
    return p;
}

void resize_listobj	(objrec * p,int32 n) {
varrec *	q;
int32	allocated;
    if (n <= p->allocated) {
        p->length = n;
        return;
    }
    q = make_listdata(n,&allocated,0);
    if (!!p->length) {
        memcpy((int32 *)q,(int32 *)p->vptr,p->length*varsize);
    }
    p->vptr = q;
    p->allocated = allocated;
    p->length = n;
}

void resize_arrayobj	(objrec * p,int32 n) {
byte *	q;
int32	allocated;
    if (n <= p->allocated) {
        p->length = n;
        return;
    }
    q = make_arraydata(n,p->elemtag,&allocated,1);
    if (!!p->length) {
        memcpy((int32 *)q,(int32 *)p->vptr,p->length*ttsize[p->elemtag]);
        free_arraydata((byte *)p->vptr,p->elemtag,p->allocated);
    }
    p->vptr = (varrec *)q;
    p->allocated = allocated;
    p->length = n;
}

void resize_bitsobj	(objrec * p,int32 n) {
byte *	q;
int32	allocated;
    if (n <= p->allocated) {
        p->length = n;
        return;
    }
    q = make_bitdata(n,p->elemtag,&allocated,1);
    if (!!p->length) {
        memcpy((int32 *)q,(int32 *)p->ptr,get_objbytes(p));
        free_bitdata((byte *)p->vptr,p->elemtag,p->allocated);
    }
    p->vptr = (varrec *)q;
    p->allocated = allocated;
    p->length = n;
}

void addto_stringobj	(varrec * vx,varrec * vy) {
int32	xlen;
int32	ylen;
int32	newlen;
objrec *	x;
objrec *	y;
    x = vx->objptr;
    y = vy->objptr;
    if (!x->mutable) {
        vx->objptr = x = copyonwrite(x,tstring);
    }
    if (x->objtype != normal_obj) {
        objerror("extending string slice");
    }
    xlen = x->length;
    ylen = y->length;
    if (xlen == 0) {
        if (ylen) {
            x->strptr = (char *)make_arraydata(ylen,tu8,&(x)->allocated,0);
            x->length = ylen;
            memcpy((int32 *)x->strptr,(int32 *)y->strptr,ylen);
        }
    }
    else if (ylen == 1) {
        if (++xlen > x->allocated) {
            resize_arrayobj(x,xlen);
        }
        x->length = xlen;
        *(x->strptr+xlen-1) = (uchar)*y->strptr;
    }
    else if (ylen) {
        newlen = xlen+ylen;
        if (newlen > x->allocated) {
            resize_arrayobj(x,newlen);
        }
        x->length = newlen;
        memcpy((int32 *)(x->strptr+xlen),(int32 *)y->strptr,ylen);
    }
}

void addto_stringobj_i	(varrec * vx,int32 ch) {
int32	xlen;
objrec *	x;
    x = vx->objptr;
    if (!x->mutable) {
        vx->objptr = x = copyonwrite(x,tstring);
    }
    xlen = x->length;
    if (xlen == 0) {
        x->strptr = (char *)make_arraydata(1,tu8,&(x)->allocated,0);
        x->length = 1;
        *x->strptr = ch;
    }
    else {
        if (++xlen > x->allocated) {
            resize_arrayobj(x,xlen);
        }
        x->length = xlen;
        *(x->strptr+xlen-1) = ch;
    }
}

void addto_stringobj_s	(varrec * vx,char * s) {
int32	xlen;
int32	ylen;
int32	newlen;
objrec *	x;
    x = vx->objptr;
    if (!x->mutable) {
        vx->objptr = x = copyonwrite(x,tstring);
    }
    if (x->objtype != normal_obj) {
        objerror("extending string slice");
    }
    xlen = x->length;
    ylen = strlen(s);
    if (xlen == 0) {
        if (ylen) {
            x->strptr = (char *)make_arraydata(ylen,tu8,&(x)->allocated,0);
            x->length = ylen;
            memcpy((int32 *)x->strptr,(int32 *)s,ylen);
        }
    }
    else if (ylen == 1) {
        if (++xlen > x->allocated) {
            resize_arrayobj(x,xlen);
        }
        x->length = xlen;
        *(x->strptr+xlen-1) = (uchar)*s;
    }
    else if (ylen) {
        newlen = xlen+ylen;
        if (newlen > x->allocated) {
            resize_arrayobj(x,newlen);
        }
        x->length = newlen;
        memcpy((int32 *)(x->strptr+xlen),(int32 *)s,ylen);
    }
}

void showstring	(char * caption,objrec * s) {
    printf("%s:  <%.*s> LEN=%X MUT:%d REF:%d SLICE:%s, s:&%p: strptr:&%p alloc:%X lower:%X\n",caption,s->length,s->strptr,s->length,s->mutable,s->refcount,objtypenames[s->objtype],s,s->strptr,s->allocated,s->lower);
}

objrec * add_stringobj	(objrec * x,objrec * y) {
int32	xlen;
int32	ylen;
int32	newlen;
objrec *	z;
char *	s;
    xlen = x->length;
    ylen = y->length;
    if (xlen == 0) {
        if (ylen == 0) {
            return addref_obj(emptystring);
        }
        else {
            return addref_obj(y);
        }
    }
    else if (ylen == 0) {
        return addref_obj(x);
    }
    else {
        newlen = xlen+ylen;
        z = (objrec *)pcm_alloc(32);
        z->strptr = s = (char *)pcm_alloc(newlen);
        z->refcount = 1;
        z->objtype = normal_obj;
        z->mutable = 1;
        z->length = newlen;
        z->lower = 1;
        z->elemtag = tu8;
        z->allocated = allocbytes;
        memcpy((int32 *)s,(int32 *)x->strptr,xlen);
        memcpy((int32 *)(s+xlen),(int32 *)y->strptr,ylen);
        return z;
    }
}

objrec * make_strslicexobj	(char * s,int32 length) {
objrec *	p;
    if (length == 0) {
        s = 0;
    }
    p = newobject();
    p->strptr = s;
    p->mutable = 1;
    p->length = length;
    p->lower = 1;
    p->elemtag = tu8;
    p->objtype = extslice_obj;
    return p;
}

objrec * copyonwrite	(objrec * p,int32 tag) {
objrec *	q;
varrec	v;
    if (!!p->mutable) {
        return p;
    }
    v.tagx = (tag+hasrefmask);
    v.objptr = p;
    vx_dupl(&v);
    q = v.objptr;
    q->mutable = 1;
    return q;
}

int32 get_objbytes	(objrec * p) {
int32	elemtype;
int32	nbits;
    elemtype = p->elemtag;
    if ((elemtype==tbit) || (elemtype==tbit2) || (elemtype==tbit4)) {
        nbits = ttbitwidth[elemtype]*p->length;
        if (!!(nbits&7)) {
            return nbits/ 8+1;
        }
        else {
            return nbits/ 8;
        }
    }
    return ttsize[elemtype]*p->length;
}

objrec * make_char	(int32 ch) {
char	str[10];
objrec *	p;
    p = chrtable[ch];
    if (p == 0) {
        str[1-1] = ch;
        str[2-1] = 0;
        p = make_str((char *)&str,1);
        p->refcount = 6;
        chrtable[ch] = p;
    }
    else {
        ++p->refcount;
    }
    return p;
}

void objerror	(char * mess) {
    printf("%s %s\n","Object error:",mess);
    exit(0);
}


// From module: var_ops
void vx_addstring	(varrec * x,varrec * y,varrec * z) {
objrec *	za;
    za = add_stringobj(x->objptr,y->objptr);
    vx_ufree(x);
    vx_ufree(y);
    z->tagx = (tstring|hasrefmask);
    z->objptr = za;
}

void vx_addtostring	(varrec * x,varrec * y) {
    if (y->tag == tstring) {
        addto_stringobj(x,y);
        vx_ufree(y);
    }
    else {
        addto_stringobj_i(x,y->value);
    }
}

void vx_addtostring_imm	(varrec * x,char * s) {
    addto_stringobj_s(x,s);
}

int32 vx_eqstring	(varrec * x,varrec * y) {
objrec *	px;
objrec *	py;
int32	res;
int32	n;
    res = vx_eqstring_nf(x,y);
    px = x->objptr;
    py = y->objptr;
    if (--px->refcount == 0) {
        vx_freex(x);
    }
    if (--py->refcount == 0) {
        vx_freex(y);
    }
    return res;
}

void vx_iorset	(varrec * x,varrec * y,varrec * z) {
int32	xlen;
int32	ylen;
int32	n;
int32	i;
int32 *	p;
objrec *	px;
objrec *	py;
byte *	pp;
    px = x->objptr;
    py = y->objptr;
    xlen = px->length;
    ylen = py->length;
    if (ylen == 0) {
        *z = *x;
    }
    else if (xlen == 0) {
        *z = *y;
    }
    else {
        *z = *x;
        vx_dupl(z);
        px = z->objptr;
        iresizeset(z,ylen);
        iorsetbits((int32 *)px->ptr,(int32 *)py->ptr,ylen);
        vx_ufree(y);
    }
}

int32 vx_equal	(varrec * x,varrec * y,int32 shallow) {
int32	res;
int32	n;
    res = vx_equal_nf(x,y,shallow);
    if (!!x->hasref) {
        vx_ufree(x);
    }
    if (!!y->hasref) {
        vx_ufree(y);
    }
    return res;
}

int32 vx_compare	(varrec * x,varrec * y) {
int32	res;
int32	n;
    res = vx_compare_nf(x,y);
    if (!!x->hasref) {
        vx_ufree(x);
    }
    if (!!y->hasref) {
        vx_ufree(y);
    }
    return res;
}

void vx_ufree	(varrec * p) {
objrec *	pa;
int32	tag;
    pa = p->objptr;
    if (pa->refcount <= 0) {
        printf("%p %d\n",(void*)(pa),pa->refcount);
        pcerror("FREE:BAD REF COUNT");
    }
    if (--pa->refcount == 0) {
        switch (pa->objtype) {
        case normal_obj:
            vx_free_dispatch(p);
            p->tag = 0;
            break;
        case slice_obj:
            vx_freeref(pa->objptr2,p->tag);
            freeobject(pa);
            break;
        default:;
            freeobject(pa);
        }
    }
}

void vx_cfree	(varrec * p) {
    if (!!p->hasref) {
        vx_ufree(p);
    }
}

void vx_freex	(varrec * p) {
objrec *	pa;
    pa = p->objptr;
    switch (pa->objtype) {
    case normal_obj:
        vx_free_dispatch(p);
        break;
    case slice_obj:
        vx_freeref(pa->objptr2,p->tag);
        freeobject(pa);
        break;
    default:;
        freeobject(pa);
    }
}

void vx_freeref	(objrec * p,int32 tag) {
varrec	v;
    if (--p->refcount == 0) {
        v.tagx = tag;
        v.objptr = p;
        vx_free_dispatch(&v);
    }
}

void vx_cshare	(varrec * p) {
    if (!!p->hasref) {
        ++p->objptr->refcount;
    }
}

void vx_ushare	(varrec * p) {
    ++p->objptr->refcount;
}

void vx_dupl	(varrec * p) {
varrec	v;
    if (!!p->hasref) {
        v = *p;
        vx_dupl_dispatch(p);
        vx_ufree(&v);
    }
}

void vx_free_dispatch	(varrec * x) {
billy:
    switch (ttbasetype[x->tag]) {
    case tstring:
        vx_free_string(x);
        break;
    case tlist:
    case trecord:
    case tdict:
        vx_free_list(x);
        break;
    case tarray:
    case tlongint:
        vx_free_array(x);
        break;
    case tstruct:
        vx_free_struct(x);
        break;
    case tbits:
    case tset:
        vx_free_bits(x);
        break;
    default:;
        pcustype("FREE",x);
    }
}

void vx_dupl_dispatch	(varrec * x) {
    switch (ttbasetype[x->tag]) {
    case tstring:
        vx_dupl_string(x);
        break;
    case tlist:
    case trecord:
    case tdict:
        vx_dupl_list(x);
        break;
    case tarray:
    case tlongint:
        vx_dupl_array(x);
        break;
    case tstruct:
        vx_dupl_struct(x);
        break;
    case tset:
        vx_dupl_set(x);
        break;
    case tbits:
        vx_dupl_bits(x);
        break;
    default:;
        pcustype("DUPL",x);
    }
}

void vx_free_string	(varrec * p) {
objrec *	q;
    q = p->objptr;
    pcm_free(q->strptr,q->allocated);
    freeobject(q);
}

void vx_free_list	(varrec * p) {
int32	i;
int32	n;
objrec *	r;
varrec *	q;
int32	av_1;
    r = p->objptr;
    n = r->length;
    q = r->vptr;
    av_1 = n;
    while (av_1--) {
        if (!!q->hasref) {
            vx_ufree(q);
        }
        ++q;
    }
    if (n) {
        free_listdata(r->vptr,r->allocated);
    }
    freeobject(r);
}

void vx_free_struct	(varrec * p) {
objrec *	r;
varrec *	q;
    r = p->objptr;
    free_arraydata(r->ptr,tu8,r->allocated);
    freeobject(r);
}

void vx_free_array	(varrec * p) {
objrec *	r;
varrec *	q;
    r = p->objptr;
    free_arraydata(r->ptr,r->elemtag,r->allocated);
    freeobject(r);
}

void vx_free_bits	(varrec * p) {
objrec *	r;
varrec *	q;
    r = p->objptr;
    free_bitdata(r->ptr,r->elemtag,r->allocated);
    freeobject(r);
}

void vx_dupl_string	(varrec * p) {
int32	n;
objrec *	oldp;
objrec *	newp;
    oldp = p->objptr;
    n = oldp->length;
    newp = make_stringobj(n);
    p->objptr = newp;
    if (n) {
        memcpy((int32 *)newp->strptr,(int32 *)oldp->strptr,n);
    }
}

void vx_dupl_list	(varrec * p) {
int32	i;
int32	j;
int32	n;
int32	nbytes;
varrec *	q;
varrec *	r;
varrec *	e;
objrec *	oldp;
objrec *	newp;
int32	av_2;
    oldp = p->objptr;
    if (oldp->refcount < 0) {
        printf("%s\n","CIRC");
        pcerror("DUPL/LIST CIRC");
        return;
    }
    n = oldp->length;
    newp = make_listobj(n,oldp->lower);
    p->objptr = newp;
    oldp->refcount = -oldp->refcount;
    if (n) {
        r = newp->vptr;
        pcm_copymem4(r,oldp->vptr,n*varsize);
        av_2 = n;
        while (av_2--) {
            if (!!r->hasref) {
                if (ttbasetype[r->tag] != trecord) {
                    ++r->objptr->refcount;
                    vx_dupl(r);
                }
                else {
                    ++r->objptr->refcount;
                }
            }
            ++r;
        }
    }
    oldp->refcount = -oldp->refcount;
}

void vx_dupl_array	(varrec * p) {
int32	i;
int32	j;
int32	n;
int32	nbytes;
varrec *	q;
varrec *	r;
varrec *	e;
objrec *	oldp;
objrec *	newp;
    oldp = p->objptr;
    n = oldp->length;
    newp = make_arrayobj(n,oldp->elemtag,oldp->lower);
    p->objptr = newp;
    if (n) {
        pcm_copymem4(newp->ptr,oldp->ptr,n*ttsize[oldp->elemtag]);
    }
}

void vx_dupl_bits	(varrec * p) {
int32	i;
int32	j;
int32	n;
int32	nbytes;
varrec *	q;
varrec *	r;
varrec *	e;
objrec *	oldp;
objrec *	newp;
    oldp = p->objptr;
    n = oldp->length;
    newp = make_bitsobj(n,oldp->elemtag,oldp->lower);
    p->objptr = newp;
    if (n) {
        pcm_copymem4(newp->ptr,oldp->ptr,get_objbytes(oldp));
    }
}

void vx_dupl_set	(varrec * p) {
int32	i;
int32	j;
int32	n;
int32	nbytes;
varrec *	q;
varrec *	r;
varrec *	e;
objrec *	oldp;
objrec *	newp;
    oldp = p->objptr;
    n = oldp->allocated;
    newp = make_bitsobj(n,tbit,oldp->lower);
    newp->length = oldp->length;
    p->objptr = newp;
    if (n) {
        pcm_copymem4(newp->ptr,oldp->ptr,get_objbytes(oldp));
    }
}

void vx_dupl_struct	(varrec * p) {
int32	i;
int32	j;
int32	n;
int32	nbytes;
objrec *	oldp;
objrec *	newp;
    oldp = p->objptr;
    nbytes = ttsize[p->tag];
    newp = make_arrayobj(nbytes,tu8,1);
    newp->length = oldp->length;
    p->objptr = newp;
    pcm_copymem4(newp->ptr,oldp->ptr,nbytes);
}

void vx_mul_listi	(varrec * a,varrec * b,varrec * c) {
varrec *	newptr;
varrec *	oldptr;
varrec *	q;
int32	newlength;
int32	newalloc;
int32	oldlength;
int32	k;
int32	i;
int32	newtag;
int32	lwr;
int64	dvalue;
varrec	d;
objrec *	pa;
objrec *	pc;
int32	av_3;
    d = *a;
    pa = d.objptr;
    oldlength = pa->length;
    newlength = (oldlength*b->value);
    oldptr = pa->vptr;
    if (!oldlength) {
        return;
    }
    if (newlength < 0) {
        pcerror("mullist 0");
    }
    else if (newlength == 0) {
        if (!!a->hasref) {
            vx_ufree(a);
        }
        c->tagx = tlist;
        c->objptr = emptylist;
        ++emptylist->refcount;
        return;
    }
    if (oldlength == 1) {
        pc = make_listobj(newlength,pa->lower);
        newptr = pc->vptr;
        c->tagx = d.tagx;
        c->objptr = pc;
        q = d.objptr->vptr;
        av_3 = newlength;
        while (av_3--) {
            *newptr = *q;
            if (!!newptr->hasref) {
                ++newptr->objptr->refcount;
                vx_dupl(newptr);
            }
            ++newptr;
        }
        if (!!d.hasref) {
            vx_ufree(&d);
        }
    }
    else {
        pcerror("MULLISTINT/COMPLEX");
    }
}

void vx_mul_stri	(varrec * a,varrec * b,varrec * c) {
int32	i;
int32	m;
int32	oldlen;
int32	newlen;
char *	newptr;
char *	p;
varrec	v;
objrec *	pa;
objrec *	s;
int32	av_4;
    m = getintvalue(b);
    if (m < 0) {
        pcerror("neg str mul");
    }
    else if (m == 0) {
        s = emptystring;
        ++emptystring->refcount;
    }
    else if (m == 1) {
        if (a != c) {
            *c = *a;
            if (!!c->hasref) {
                vx_ushare(c);
            }
        }
        return;
    }
    else {
        pa = a->objptr;
        oldlen = pa->length;
        if (oldlen) {
            newlen = oldlen*m;
            s = make_stringobj(newlen);
            p = s->strptr;
            av_4 = m;
            while (av_4--) {
                memcpy((int32 *)p,(int32 *)pa->strptr,oldlen);
                p += oldlen;
            }
            if (!!a->hasref) {
                vx_ufree(a);
            }
        }
        else {
            *c = *a;
            if (!!a->hasref) {
                vx_ushare(a);
            }
            return;
        }
    }
    v.tagx = (tstring|hasrefmask);
    v.objptr = s;
    *c = v;
}

void vx_slicelist	(varrec * a,varrec * x,varrec * z) {
varrec	v;
int32	i;
int32	j;
int32	alower;
int32	ahasref;
objrec *	p;
objrec *	q;
    p = a->objptr;
    i = x->range_lower;
    j = x->range_upper;
    alower = p->lower;
    if (i < alower || j > p->length+alower-1 || i > j) {
        pcerror("vx/slice/list bounds");
    }
    z->tagx = (tlist|hasrefmask);
    q = newobject();
    z->objptr = q;
    q->objtype = slice_obj;
    q->mutable = p->mutable;
    q->lower = 1;
    if ((p->objtype==slice_obj)) {
        q->objptr2 = p->objptr2;
        ++q->objptr2->refcount;
        q->vptr = p->vptr+i-alower;
        v.tagx = (tlist|hasrefmask);
        v.objptr = p;
        vx_ufree(&v);
    } else if ((p->objtype==extslice_obj)) {
        q->objptr2 = 0;
        q->objtype = extslice_obj;
        q->vptr = p->vptr+i-alower;
    }
    else {
        q->objptr2 = p;
        q->vptr = p->vptr+i-alower;
    }
    q->length = (j-i)+1;
}

void vx_iappendlist	(varrec * a,varrec * b) {
int32	n;
int32	lower;
varrec *	q;
objrec *	p;
    p = a->objptr;
    if (p->objtype != normal_obj) {
        pcerror("Can't extend slice");
    }
    if (!p->mutable) {
        p = copyonwrite(p,tlist);
    }
    n = p->length+1;
    if (n > p->allocated) {
        resize_listobj(p,n);
    }
    else {
        p->length = n;
    }
    (p->vptr+n-1)->tagx = tvoid;
    a->objptr = p;
    q = p->vptr+p->length-1;
    if (b) {
        *q = *b;
    }
    else {
        q->tagx = tvoid;
    }
}

void vx_iconcatlist	(varrec * a,varrec * b) {
varrec *	newptr;
varrec *	c;
varrec *	d;
int32	n;
int32	alen;
int32	blen;
int32	newlen;
int32	oldbytes;
int32	newbytes;
varrec *	v;
objrec *	pa;
objrec *	pb;
int32	av_5;
int32	av_6;
    pa = a->objptr;
    if (!pa->mutable) {
        pa = copyonwrite(pa,a->tag);
        a->objptr = pa;
    }
    pb = b->objptr;
    alen = pa->length;
    blen = pb->length;
    if (alen == 0) {
        if (blen) {
            resize_listobj(pa,blen);
            d = pa->vptr;
            memcpy((int32 *)d,(int32 *)pb->vptr,blen*varsize);
            av_5 = blen;
            while (av_5--) {
                if (!!d->hasref) {
                    vx_ushare(d);
                }
                ++d;
            }
        }
    }
    else if (blen) {
        newlen = alen+blen;
        resize_listobj(pa,newlen);
        d = pa->vptr+alen;
        memcpy((int32 *)d,(int32 *)pb->vptr,blen*varsize);
        av_6 = blen;
        while (av_6--) {
            if (!!d->hasref) {
                vx_ushare(d);
            }
            ++d;
        }
    }
    vx_ufree(b);
}

void vx_indexstring	(varrec * a,varrec * x,varrec * z) {
varrec	v;
int32	index;
int32	length;
int32	i;
objrec *	p;
objrec *	q;
    p = a->objptr;
    i = (x->value-1);
    if ((uint32)i >= (uint32)p->length) {
        pcerror("string[int] bounds");
    }
    z->tagx = (tstring|hasrefmask);
    q = newobject();
    z->objptr = q;
    q->objtype = slice_obj;
    q->mutable = p->mutable;
    q->lower = 1;
    if ((p->objtype==slice_obj)) {
        q->objptr2 = p->objptr2;
        ++q->objptr2->refcount;
        q->strptr = p->strptr+i;
        v.tagx = (tstring|hasrefmask);
        v.objptr = p;
        vx_ufree(&v);
    } else if ((p->objtype==extslice_obj)) {
        q->objptr2 = 0;
        q->objtype = extslice_obj;
        q->strptr = p->strptr+i;
    }
    else {
        q->objptr2 = p;
        q->strptr = p->strptr+i;
    }
    q->length = 1;
}

void vx_slicestring	(varrec * a,varrec * x,varrec * z) {
varrec	v;
int32	i;
int32	j;
int32	value;
objrec *	p;
objrec *	q;
    p = a->objptr;
    i = x->range_lower;
    j = x->range_upper;
    if (i < 1 || j > p->length || i > j) {
        pcerror("string[slice] bounds");
    }
    z->tagx = (tstring|hasrefmask);
    q = newobject();
    sptr->objptr = q;
    q->objtype = slice_obj;
    q->mutable = p->mutable;
    q->lower = 1;
    if ((p->objtype==slice_obj)) {
        q->objptr2 = p->objptr2;
        ++q->objptr2->refcount;
        q->strptr = p->strptr+i-1;
        v.tagx = (tstring|hasrefmask);
        v.objptr = p;
        vx_ufree(&v);
    } else if ((p->objtype==extslice_obj)) {
        q->objptr2 = 0;
        q->objtype = extslice_obj;
        q->strptr = p->strptr+i-1;
    }
    else {
        q->objptr2 = p;
        q->strptr = p->strptr+i-1;
    }
    q->length = (j-i)+1;
}

int32 vx_invar	(varrec * x,varrec * y) {
int32	i;
int32	xt;
int32	yt;
int32	n;
int32	a;
int64	nn;
int64	aa;
varrec *	p;
varrec *	q;
objrec *	px;
objrec *	py;
int32	av_7;
    xt = x->tag;
    yt = ttbasetype[y->tag];
    px = x->objptr;
    py = y->objptr;
    switch (xt) {
    case tint:
doi64invar:
        switch (yt) {
        case tset:
            n = x->value;
doi64inset:
            if ((uint32)n >= (uint32)py->length) {
                if (!!y->hasref) {
                    vx_ufree(y);
                }
                return 0;
            }
            n = testelem((byte (*)[])py->ptr,n);
            vx_ufree(y);
            return n;
            break;
        case tlist:
            a = x->value;
            n = py->length;
            p = py->vptr;
            for (i=1; i<=n; ++i) {
                if (p->tag == tint && p->value == a) {
                    if (!!y->hasref) {
                        vx_ufree(y);
                    }
                    return i;
                }
                ++p;
            }
            vx_ufree(y);
            return 0;
            break;
        case tarray:
            if ((py->elemtag==ti8) || (py->elemtag==tu8)) {
                n = u8inarray(x->value,py);
            } else if ((py->elemtag==ti16) || (py->elemtag==tu16)) {
                n = u16inarray(x->value,py);
            } else if ((py->elemtag==ti32) || (py->elemtag==tu32)) {
                n = u32inarray(x->value,py);
            } else if ((py->elemtag==ti64) || (py->elemtag==tu64)) {
                n = u64inarray(x->value,py);
            }
            else {
                pcustypet("x in array",py->elemtag);
            }
            vx_ufree(y);
            return n;
            break;
        case tbits:
            if ((py->elemtag==tbit)) {
                n = bitinbits(x->value,py);
            }
            else {
                pcustypet("x in bits",py->elemtag);
            }
            vx_ufree(y);
            return n;
            break;
        case trange:
            n = x->value;
            return (int32)(n >= y->range_lower && n <= y->range_upper);
            break;
        default:;
        }
        break;
    case tstring:
        switch (yt) {
        case tstring:
            n = vx_strinstr(x,y);
            vx_ufree(x);
            vx_ufree(y);
            return n;
            break;
        case tlist:
            n = py->length;
            p = py->vptr;
            i = py->lower;
            av_7 = n;
            while (av_7--) {
                if (p->tag == tstring) {
                    if (!!vx_eqstring_nf(x,p)) {
                        vx_ufree(x);
                        vx_ufree(y);
                        return i;
                    }
                }
                ++p;
                ++i;
            }
            vx_ufree(x);
            vx_ufree(y);
            return 0;
            break;
        default:;
        }
        break;
    default:;
        switch (yt) {
        case tlist:
            n = py->length;
            p = py->vptr;
            for (i=1; i<=n; ++i) {
                if (vx_equal_nf(x,p,1) == 1) {
                    if (!!x->hasref) {
                        vx_ufree(x);
                    }
                    if (!!y->hasref) {
                        vx_ufree(y);
                    }
                    return i;
                }
                ++p;
            }
            if (!!x->hasref) {
                vx_ufree(x);
            }
            vx_ufree(y);
            return 0;
            break;
        default:;
        }
    }
    pcmxtypes("vx-invar:",x,y);
    return 0;
}

static int32 u8inarray	(byte a,objrec * p) {
int32	i;
byte *	q;
int32	av_8;
    i = p->lower;
    q = p->ptr;
    av_8 = p->length;
    while (av_8--) {
        if (*q == a) {
            return i;
        }
        ++q;
        ++i;
    }
    return p->lower-1;
}

static int32 u16inarray	(uint16 a,objrec * p) {
int32	i;
uint16 *	q;
int32	av_9;
    i = p->lower;
    q = (uint16 *)p->ptr;
    av_9 = p->length;
    while (av_9--) {
        if (*q == a) {
            return i;
        }
        ++q;
        ++i;
    }
    return p->lower-1;
}

static int32 u32inarray	(uint32 a,objrec * p) {
int32	i;
uint32 *	q;
int32	av_10;
    i = p->lower;
    q = (uint32 *)p->ptr;
    av_10 = p->length;
    while (av_10--) {
        if (*q == a) {
            return i;
        }
        ++q;
        ++i;
    }
    return p->lower-1;
}

static int32 u64inarray	(uint64 a,objrec * p) {
int32	i;
uint64 *	q;
int32	av_11;
    i = p->lower;
    q = (uint64 *)p->ptr;
    av_11 = p->length;
    while (av_11--) {
        if (*q == a) {
            return i;
        }
        ++q;
        ++i;
    }
    return p->lower-1;
}

static int32 bitinbits	(byte a,objrec * p) {
int32	i;
int32	offset;
int32	mask;
byte *	q;
int32	av_12;
    i = p->lower;
    q = p->ptr;
    offset = p->bitoffset-1;
    mask = 1;
    if (offset) {
        mask = mask<<offset;
    }
    av_12 = p->length;
    while (av_12--) {
        printf("%s %u %d %s %d %d %s %u\n","(Q)^=",*q,*q&mask,"MASK=",mask,i,"A=",a);
        if (!!(*q&mask)) {
            if (!!a) {
                return i;
            }
        }
        else if (a == 0) {
            return i;
        }
        ++i;
        mask <<= 1;
        if (mask >= 256) {
            mask = 1;
            ++q;
        }
    }
    return p->lower-1;
}

int32 vx_strinstr	(varrec * x,varrec * y) {
int32	xlen;
int32	ylen;
int32	result;
int32	i;
int32	j;
int32	k;
char *	sx;
char *	sy;
objrec *	px;
objrec *	py;
    px = x->objptr;
    py = y->objptr;
    xlen = px->length;
    ylen = py->length;
    if (xlen == 0 || ylen == 0) {
        return 0;
    }
    k = ylen-xlen;
    for (i=0; i<=k; ++i) {
        sx = px->strptr;
        sy = py->strptr+i;
        for (j=1; j<=xlen; ++j) {
            if ((uchar)*sx != (uchar)*sy) {
                goto L87;
            }
            ++sx;
            ++sy;
        }
        return i+1;
nextpos:
L87:;
    }
    return 0;
}

int32 vx_eqstring_nf	(varrec * x,varrec * y) {
int32	n;
objrec *	px;
objrec *	py;
    px = x->objptr;
    py = y->objptr;
    n = px->length;
    if (n != py->length) {
        return 0;
    }
    if (n == 0) {
        return 1;
    }
    return (int32)(strncmp(px->strptr,py->strptr,n) == 0);
}

int32 vx_equal_nf	(varrec * x,varrec * y,int32 shallow) {
int32	xt;
int32	yt;
int32	xbase;
int32	ybase;
int32	xval;
int32	yval;
int32	i;
int32	nbits;
int32	nbytes;
int32	n;
varrec *	p;
varrec *	q;
objrec *	px;
objrec *	py;
int32	av_13;
int32	av_14;
    xbase = ttbasetype[xt = x->tag];
    ybase = ttbasetype[yt = y->tag];
    if (ybase == tvoid) {
        pcerror("pcequal/void");
    }
    px = x->objptr;
    py = y->objptr;
    switch (xbase) {
    case tint:
        switch (ybase) {
        case tint:
        case tword:
            return ((x->value == y->value)?1:0);
            break;
        case treal:
            return ((x->value == y->xvalue)?1:0);
            break;
        default:;
        }
        break;
    case tword:
        switch (ybase) {
        case tu32:
            return ((x->uvalue == y->uvalue)?1:0);
            break;
        default:;
        }
        break;
    case treal:
        switch (ybase) {
        case tint:
            return ((x->xvalue == y->value)?1:0);
            break;
        case treal:
            return ((x->xvalue == y->xvalue)?1:0);
            break;
        default:;
        }
        break;
    case trange:
        if (ybase == trange) {
            return ((x->value == y->value)?1:0);
        }
        break;
    case trefvar:
        switch (ybase) {
        case trefvar:
        case tint:
            return (int32)(x->value == y->value);
            break;
        default:;
        }
        break;
    case trefpacked:
        switch (ybase) {
        case trefpacked:
        case tint:
            return (int32)(x->value == y->value);
            break;
        default:;
        }
        break;
    case trefproc:
        switch (ybase) {
        case trefproc:
        case tint:
            return (int32)(x->value == y->value);
            break;
        default:;
        }
        break;
    case tlist:
        if (ybase == tlist) {
            if (shallow) {
                return (int32)(px == py);
            }
            if (px->length != py->length) {
                return 0;
            }
            p = px->vptr;
            q = py->vptr;
            av_13 = px->length;
            while (av_13--) {
                if (vx_equal_nf(p++,q++,shallow) == 0) {
                    return 0;
                }
            }
            return 1;
        }
        break;
    case tstring:
        switch (ybase) {
        case tstring:
            return vx_eqstring_nf(x,y);
            break;
        default:;
        }
        break;
    case tstruct:
        if (xt != yt) {
            return 0;
        }
        return comparebytes(px->ptr,py->ptr,ttsize[xt]);
        break;
    case tset:
        if (ybase != tset) {
            return 0;
        }
        if (px->length != py->length) {
            return 0;
        }
        nbytes = (px->length-1)/ 32+1;
        return comparebytes(px->ptr,py->ptr,nbytes);
        break;
    case tvoid:
        pcerror("Comparing void types");
        break;
    case tlongint:
        pcerror("VXEQUAL/BIGINT");
        break;
    case trecord:
        if (xt != yt) {
            return 0;
        }
        if (shallow) {
            return (int32)(px == py);
        }
        p = px->vptr;
        q = py->vptr;
        n = ttlength[xt];
        av_14 = n;
        while (av_14--) {
            if (vx_equal_nf(p++,q++,shallow) == 0) {
                return 0;
            }
        }
        return 1;
        break;
    case tarray:
        if (ybase == tarray) {
            pcerror("vx_equal/array");
        }
        return 0;
        break;
    case ttype:
        if (ybase == ttype && x->value == y->value) {
            return 1;
        }
        break;
    default:;
        printf("%s\n","DIFF TYPES");
        return 0;
    }
    return 0;
}

int32 comparebytes	(byte * p,byte * q,int32 n) {
    return (int32)(memcmp(p,q,n) == 0);
}

int32 vx_compare_nf	(varrec * x,varrec * y) {
int32	xt;
int32	yt;
int32	xbase;
int32	ybase;
int32	xval;
int32	yval;
int32	i;
int32	nbits;
int32	nbytes;
varrec *	p;
varrec *	q;
objrec *	px;
objrec *	py;
    ybase = ttbasetype[yt = y->tag];
    xbase = ttbasetype[xt = x->tag];
    switch (xbase) {
    case tint:
        switch (ybase) {
        case tint:
            return ((x->value < y->value)?-1:((x->value > y->value)?1:0));
            break;
        case treal:
            return ((x->value < y->xvalue)?-1:((x->value > y->xvalue)?1:0));
            break;
        default:;
            goto L94;
        }
        break;
    case tword:
        switch (ybase) {
        case tword:
            return ((x->uvalue < y->uvalue)?-1:((x->uvalue > y->uvalue)?1:0));
            break;
        default:;
            goto L94;
        }
        break;
    case treal:
        switch (ybase) {
        case tint:
            return ((x->xvalue < y->value)?-1:((x->xvalue > y->value)?1:0));
            break;
        case treal:
            return ((x->xvalue < y->xvalue)?-1:((x->xvalue > y->xvalue)?1:0));
            break;
        default:;
            goto L94;
        }
        break;
    case trefpacked:
        switch (ybase) {
        case trefpacked:
        case tint:
            return ((x->value < y->value)?-1:((x->value > y->value)?1:0));
            break;
        default:;
            goto L94;
        }
        break;
    case trefvar:
        switch (ybase) {
        case trefvar:
        case tint:
            return ((x->value < y->value)?-1:((x->value > y->value)?1:0));
            break;
        default:;
            goto L94;
        }
        break;
    case tstring:
        switch (ybase) {
        case tstring:
            px = x->objptr;
            py = y->objptr;
            return cmpstring(px->strptr,py->strptr,px->length,py->length);
            break;
        default:;
            goto L94;
        }
        break;
    case tlongint:
        pcerror("CMP BIGINT");
        break;
    default:;
badcmp:
L94:;
        pcmxtypes("vx_compare",x,y);
    }
    return 0;
}

int32 cmpstring	(char * s,char * t,int32 slen,int32 tlen) {
    if (slen == 0) {
        if (tlen == 0) {
            return 0;
        }
        else {
            return -1;
        }
    }
    else if (tlen == 0) {
        return 1;
    }
    else {
        if (slen == tlen) {
            if (slen == 1) {
                if ((uchar)*s < (uchar)*t) {
                    return -1;
                }
                else if ((uchar)*s > (uchar)*t) {
                    return 1;
                }
                else {
                    return 0;
                }
            }
            return strncmp(s,t,slen);
        }
        else {
            return strcmp(convCstring(s,slen),convCstring(t,tlen));
        }
    }
}

void iorsetbits	(int32 * p,int32 * q,int32 n) {
int32	i;
int32	av_15;
    av_15 = (n-1)/ 32+1;
    while (av_15--) {
        *p++ |= *q++;
    }
}

void pc_iandsetbits	(int32 * p,int32 * q,int32 n) {
int32	i;
int32	av_16;
    av_16 = (n-1)/ 32+1;
    while (av_16--) {
        *p++ &= *q++;
    }
}

void pc_ixorsetbits	(int32 * p,int32 * q,int32 n) {
int32	i;
int32	av_17;
    av_17 = (n-1)/ 32+1;
    while (av_17--) {
        *p++ ^= *q++;
    }
}

void pc_subsetbits	(int32 * p,int32 * q,int32 n) {
int32	i;
int32	av_18;
    av_18 = (n-1)/ 32+1;
    while (av_18--) {
        *p |= *q;
        *p ^= *q;
        ++p;
        ++q;
    }
}

void iresizeset	(varrec * p,int32 n) {
objrec *	pp;
    pp = p->objptr;
    if (pp->length >= n) {
        return;
    }
    resize_bitsobj(pp,n);
    pp->length = n;
}

void vx_storestring	(varrec * p,varrec * q) {
objrec *	pp;
objrec *	qq;
    pp = p->objptr;
    qq = q->objptr;
    if (pp->objtype == normal_obj) {
        pcerror("popstr not slice");
    }
    if (q->tag != tstring) {
        pcerror("popstr not str");
    }
    if (pp->length != qq->length) {
        pcerror("popstr diff lengths");
    }
    if (!pp->mutable) {
        pcerror("popstr not mut");
    }
    if (!!pp->length) {
        memcpy((int32 *)pp->strptr,(int32 *)qq->strptr,pp->length);
    }
    vx_ufree(p);
    vx_ufree(q);
}


// From module: pc_bigint
void bn_makestr	(char * s,int32 length,varrec * dest) {
    if (length == 0) {
        pcerror("bnmakestr");
    }
    if ((uchar)*s == '-') {
        bn_makeu(s+1,length-1,1,dest);
    }
    else {
        bn_makeu(s,length,0,dest);
    }
}

void bn_makeint	(int64 aa,varrec * dest) {
char	s[100];
    sprintf((char *)&s,"%lld",aa);
    bn_makestr((char *)&s,strlen((char *)&s),dest);
}

static void bn_makeu	(char * s,int32 nchars,int32 neg,varrec * bn) {
int32	i;
int32	ndigits;
int32	n;
objrec *	p;
    if (nchars == 0) {
        pcerror("bnmakeu");
    }
    ndigits = (nchars-1)/ digitwidth+1;
    makebigint(ndigits,bn);
    p = bn->objptr;
    (*p->bnptr)[0] = neg;
    n = nchars;
    i = ndigits;
    while (n > 0) {
        if (n >= digitwidth) {
            (*p->bnptr)[i] = strvaln(s+n-digitwidth,digitwidth);
        }
        else {
            (*p->bnptr)[i] = strvaln(s,n);
        }
        --i;
        n -= digitwidth;
    }
    if (ndigits == 1 && (*p->bnptr)[1] == 0) {
        (*p->bnptr)[0] = 0;
    }
}

static int32 strvaln	(char * s,int32 n) {
int32	a;
int32	av_1;
    a = 0;
    av_1 = n;
    while (av_1--) {
        a = a*10+(uchar)*s-'0';
        ++s;
    }
    return a;
}

static void makebigint	(int32 ndigits,varrec * dest) {
varrec	l;
int32 *	p;
int32	i;
    l.tagx = (tlongint|hasrefmask);
    l.objptr = make_arrayobj(ndigits+1,ti32,0);
    l.bndigits = ndigits;
    *dest = l;
}

static void freebigint	(varrec * bn) {
    if (bn->tag != tvoid) {
        vx_ufree(bn);
    }
}

void bn_neg	(varrec * bn) {
objrec *	p;
    p = bn->objptr;
    if (bn->bndigits == 1 && (*p->bnptr)[1] == 0) {
        (*p->bnptr)[0] = 0;
    }
    else {
        (*p->bnptr)[0] ^= 1;
    }
}

void bn_abs	(varrec * bn) {
    (*bn->objptr->bnptr)[0] = 0;
}

void bn_add	(varrec * a,varrec * b,varrec * c) {
int32	nega;
int32	negb;
    nega = (*a->objptr->bnptr)[0];
    negb = (*b->objptr->bnptr)[0];
    if (!nega && !negb) {
        bn_addu(a,b,c);
        return;
    }
    if (nega && negb) {
        bn_addu(a,b,c);
        (*c->objptr->bnptr)[0] = 1;
        return;
    }
    if (!nega && negb) {
        bn_subu(a,b,c);
        return;
    }
    bn_subu(b,a,c);
}

static void bn_addu	(varrec * a,varrec * b,varrec * c) {
int32	da;
int32	db;
int32	d;
int32	aoffset;
int32	boffset;
int32	i;
int32	ia;
int32	ib;
int32	carry;
int32	sum;
varrec	e;
objrec *	pa;
objrec *	pb;
objrec *	pc;
objrec *	pe;
    pa = a->objptr;
    pb = b->objptr;
    da = a->bndigits;
    db = b->bndigits;
    if (da == db) {
        d = da;
        aoffset = boffset = 0;
    }
    else if (da > db) {
        d = da;
        aoffset = 0;
        boffset = db-da;
    }
    else {
        d = db;
        aoffset = da-db;
        boffset = 0;
    }
    makebigint(d,c);
    pc = c->objptr;
    carry = 0;
    for (i=d; i>=1; --i) {
        ia = i+aoffset;
        ib = i+boffset;
        if (ia >= 1 && ib >= 1) {
            sum = (*pa->bnptr)[ia]+(*pb->bnptr)[ib]+carry;
        }
        else if (ia < 1) {
            sum = (*pb->bnptr)[ib]+carry;
        }
        else {
            sum = (*pa->bnptr)[ia]+carry;
        }
        if (sum < digitbase) {
            carry = 0;
            (*pc->bnptr)[i] = sum;
        }
        else {
            carry = 1;
            (*pc->bnptr)[i] = sum-digitbase;
        }
    }
    if (carry) {
        makebigint(d+1,&e);
        pe = e.objptr;
        for (i=1; i<=d; ++i) {
            (*pe->bnptr)[i+1] = (*pc->bnptr)[i];
        }
        (*pe->bnptr)[1] = 1;
        (*pe->bnptr)[0] = 0;
        freebigint(c);
        *c = e;
        return;
    }
    (*pc->bnptr)[0] = 0;
}

void bn_sub	(varrec * a,varrec * b,varrec * c) {
int32	nega;
int32	negb;
    nega = (*a->objptr->bnptr)[0];
    negb = (*b->objptr->bnptr)[0];
    if (!nega && !negb) {
        bn_subu(a,b,c);
        return;
    }
    if (nega && negb) {
        bn_subu(b,a,c);
        return;
    }
    if (!nega && negb) {
        bn_addu(a,b,c);
        return;
    }
    bn_subu(a,b,c);
    (*c->objptr->bnptr)[0] = 1;
}

static void bn_subu	(varrec * a,varrec * b,varrec * c) {
int32	da;
int32	db;
int32	minus;
int32	d;
int32	carry;
int32	diff;
int32	aoffset;
int32	boffset;
int32	i;
int32	ia;
int32	ib;
int32	z;
varrec	e;
objrec *	pa;
objrec *	pb;
objrec *	pc;
objrec *	pe;
int32	av_2;
int32	av_3;
    pa = a->objptr;
    pb = b->objptr;
    da = a->bndigits;
    db = b->bndigits;
    minus = 0;
    if (da == db) {
        d = da;
        aoffset = boffset = 0;
        makebigint(da,c);
    }
    else if (da > db) {
        d = da;
        aoffset = 0;
        boffset = db-da;
        makebigint(da,c);
    }
    else {
        aoffset = da-db;
        makebigint(db,c);
        pc = c->objptr;
        goto L24;
    }
    pc = c->objptr;
    carry = 0;
    for (i=d; i>=1; --i) {
        ib = i+boffset;
        if (ib >= 1) {
            diff = ((*pa->bnptr)[i]-(*pb->bnptr)[ib])-carry;
        }
        else {
            diff = (*pa->bnptr)[i]-carry;
        }
        if (diff < 0) {
            carry = 1;
            (*pc->bnptr)[i] = diff+digitbase;
        }
        else {
            carry = 0;
            (*pc->bnptr)[i] = diff;
        }
    }
    if (carry) {
doreverse:
L24:;
        carry = 0;
        minus = 1;
        d = db;
        aoffset = da-db;
        for (i=d; i>=1; --i) {
            ia = i+aoffset;
            if (ia >= 1) {
                diff = ((*pb->bnptr)[i]-(*pa->bnptr)[ia])-carry;
            }
            else {
                diff = (*pb->bnptr)[i]-carry;
            }
            if (diff < 0) {
                carry = 1;
                (*pc->bnptr)[i] = diff+digitbase;
            }
            else {
                carry = 0;
                (*pc->bnptr)[i] = diff;
            }
        }
    }
    z = 0;
    for (i=1; i<=c->bndigits-1; ++i) {
        if (!!(*pc->bnptr)[i]) {
            goto L36;
        }
        ++z;
    }
L36:;
    if (z) {
        makebigint(c->bndigits-z,&e);
        pe = e.objptr;
        for (i=1; i<=e.bndigits; ++i) {
            (*pe->bnptr)[i] = (*pc->bnptr)[i+z];
        }
        (*pe->bnptr)[0] = minus;
        freebigint(c);
        *c = e;
        return;
    }
    (*pc->bnptr)[0] = minus;
}

void bn_mul	(varrec * a,varrec * b,varrec * c) {
objrec *	pa;
objrec *	pb;
objrec *	pc;
    pa = a->objptr;
    pb = b->objptr;
    if ((*pa->bnptr)[0] != (*pb->bnptr)[0]) {
        bn_mulu(a,b,c);
        pc = c->objptr;
        if (c->bndigits > 1 || !!(*pc->bnptr)[1]) {
            (*pc->bnptr)[0] = 0;
        }
        return;
    }
    bn_mulu(a,b,c);
}

static void muldigit	(varrec * a,int32 b,int32 d,varrec * c) {
int32	i;
int32	alen;
int64	p;
int64	carry;
int32	nb;
varrec	e;
objrec *	pa;
objrec *	pc;
objrec *	pe;
int32	av_4;
int32	av_5;
    pa = a->objptr;
    nb = b;
    alen = a->bndigits;
    if (nb == 0) {
        bn_zero(c);
        return;
    }
    if (nb == 1) {
        if (d == 0) {
            *c = *a;
            if (!!c->hasref) {
                vx_ushare(c);
            }
            return;
        }
        makebigint(alen+d,c);
        pc = c->objptr;
        (*pc->bnptr)[0] = 0;
        for (i=1; i<=alen; ++i) {
            (*pc->bnptr)[i] = (*pa->bnptr)[i];
        }
        for (i=alen+1; i<=alen+d; ++i) {
            (*pc->bnptr)[i] = 0;
        }
        return;
    }
    makebigint(alen+d+1,c);
    pc = c->objptr;
    carry = 0;
    for (i=alen; i>=1; --i) {
        p = (int64)(*pa->bnptr)[i]*(int64)nb+carry;
        (*pc->bnptr)[i+1] = (p% digitbase);
        carry = p/ digitbase;
    }
    (*pc->bnptr)[1] = carry;
    if ((*pc->bnptr)[1] == 0) {
        makebigint(c->bndigits-1,&e);
        pe = e.objptr;
        for (i=1; i<=e.bndigits; ++i) {
            (*pe->bnptr)[i] = (*pc->bnptr)[i+1];
        }
        freebigint(c);
        *c = e;
        return;
    }
}

static void bn_zero	(varrec * bn) {
    makebigint(1,bn);
}

void bn_mulu	(varrec * a,varrec * b,varrec * c) {
int32	awords;
int32	bwords;
int32	cwords;
int32	ax;
int32	bx;
int32	cx;
int32	i;
int32	cx1;
int64	p;
int64	carry;
int64	x;
varrec	d;
int64	pdquot;
int64	pdrem;
objrec *	pa;
objrec *	pb;
objrec *	pc;
objrec *	pd;
int32	av_6;
    pa = a->objptr;
    pb = b->objptr;
    awords = a->bndigits;
    bwords = b->bndigits;
    cwords = awords+bwords;
    makebigint(cwords,c);
    pc = c->objptr;
    cx = cwords;
    for (bx=bwords; bx>=1; --bx) {
        carry = 0;
        cx1 = cx;
        for (ax=awords; ax>=1; --ax) {
            p = (int64)(*pa->bnptr)[ax]*(int64)(*pb->bnptr)[bx]+carry;
            pdquot = p/ digitbase;
            x = (*pc->bnptr)[cx1]+p% digitbase;
            if (x > digitmax) {
                carry = pdquot+x/ digitbase;
                (*pc->bnptr)[cx1--] = (x% digitbase);
            }
            else {
                carry = pdquot;
                (*pc->bnptr)[cx1--] = x;
            }
        }
        (*pc->bnptr)[cx1] = carry;
        --cx;
    }
    if ((*pc->bnptr)[1] == 0) {
        makebigint(cwords-1,&d);
        pd = d.objptr;
        for (i=1; i<=d.bndigits; ++i) {
            (*pd->bnptr)[i] = (*pc->bnptr)[i+1];
        }
        freebigint(c);
        *c = d;
        return;
    }
}

void bn_div	(varrec * a,varrec * b,varrec * c) {
objrec *	pa;
objrec *	pb;
objrec *	pc;
    pa = a->objptr;
    pb = b->objptr;
    if ((*pa->bnptr)[0] != (*pb->bnptr)[0]) {
        bn_divu(a,b,c);
        pc = c->objptr;
        if (c->bndigits > 1 || !!(*pc->bnptr)[1]) {
            (*pc->bnptr)[0] = 0;
        }
        return;
    }
    bn_divu(a,b,c);
}

void bn_divu	(varrec * a,varrec * b,varrec * c) {
int32	na;
int32	nb;
int32	k;
int32	n;
int32	i;
int32	clen;
int64	xx;
int64	y;
varrec	x;
varrec	e;
varrec	f;
objrec *	pa;
objrec *	pb;
objrec *	pc;
static objrec *	pe=0;
static objrec *	px=0;
int32	av_7;
int32	av_8;
int32	av_9;
    pa = a->objptr;
    pb = b->objptr;
    if (b->bndigits == 1 && (*pb->bnptr)[1] == 0) {
        bn_zero(c);
        return;
    }
    if (b->bndigits == 1 && (*pb->bnptr)[1] == 1) {
        *c = *a;
        if (!!c->hasref) {
            vx_ushare(c);
        }
        return;
    }
    na = a->bndigits;
    nb = b->bndigits;
    if (nb > na) {
        bn_zero(c);
        return;
    }
    n = nb;
    makebigint(n,&x);
    px = x.objptr;
    e.tagx = tvoid;
    for (i=0; i<=n; ++i) {
        (*px->bnptr)[i] = (*pa->bnptr)[i];
    }
    makebigint(na,c);
    pc = c->objptr;
    clen = 0;
    while (1) {
        k = 0;
        while (1) {
            if (x.bndigits < nb) {
                goto L76;
            }
            else if (x.bndigits > nb) {
                xx = (int64)(*px->bnptr)[1]*digitbase+(int64)(*px->bnptr)[2];
                y = xx/ ((*pb->bnptr)[1]+1);
            }
            else {
                if ((*px->bnptr)[1] >= (*pb->bnptr)[1]+1) {
                    y = ((*px->bnptr)[1]/ ((*pb->bnptr)[1]+1));
                }
                else {
                    y = 1;
                    for (i=1; i<=nb; ++i) {
                        if ((*px->bnptr)[i] < (*pb->bnptr)[i]) {
                            y = 0;
                            goto L86;
                        }
                        else if ((*px->bnptr)[i] > (*pb->bnptr)[i]) {
                            goto L83;
                        }
                    }
L83:;
                }
            }
            k += y;
            if (y > 1) {
                muldigit(b,y,0,&e);
                bn_subu(&x,&e,&f);
                freebigint(&e);
                freebigint(&x);
                x = f;
                f.tag = tvoid;
            }
            else {
                bn_subu(&x,b,&f);
                freebigint(&x);
                x = f;
                f.tag = tvoid;
            }
            px = x.objptr;
        }
L76:;
exit2:
L86:;
        ++clen;
        (*pc->bnptr)[clen] = k;
        if (n == na) {
            goto L74;
        }
        ++n;
        if (x.bndigits == 1 && (*px->bnptr)[1] == 0) {
            (*px->bnptr)[1] = (*pa->bnptr)[n];
        }
        else {
            makebigint(x.bndigits+1,&e);
            pe = e.objptr;
            for (i=0; i<=x.bndigits; ++i) {
                (*pe->bnptr)[i] = (*px->bnptr)[i];
            }
            (*pe->bnptr)[e.bndigits] = (*pa->bnptr)[n];
            freebigint(&x);
            x = e;
            e.tag = tvoid;
        }
    }
L74:;
    freebigint(&e);
    freebigint(&x);
    if (clen == 0) {
        freebigint(c);
        bn_zero(c);
        return;
    }
    if (clen > 1 && (*pc->bnptr)[1] == 0) {
        makebigint(clen-1,&e);
        pe = e.objptr;
        for (i=1; i<=e.bndigits; ++i) {
            (*pe->bnptr)[i] = (*pc->bnptr)[i+1];
        }
    }
    else {
        makebigint(clen,&e);
        pe = e.objptr;
        for (i=1; i<=e.bndigits; ++i) {
            (*pe->bnptr)[i] = (*pc->bnptr)[i];
        }
    }
    freebigint(c);
    *c = e;
}

int32 bn_equal	(varrec * a,varrec * b) {
int32	i;
int32	n;
objrec *	pa;
objrec *	pb;
    pa = a->objptr;
    pb = b->objptr;
    n = a->bndigits;
    if (n != b->bndigits) {
        return 0;
    }
    for (i=0; i<=n; ++i) {
        if ((*pa->bnptr)[i] != (*pb->bnptr)[i]) {
            return 0;
        }
    }
    return 1;
}

int32 bn_cmp	(varrec * a,varrec * b) {
int32	i;
varrec	c;
    if (bn_equal(a,b) == 1) {
        return 0;
    }
    bn_sub(a,b,&c);
    if (!!(*c.objptr->bnptr)[0]) {
        freebigint(&c);
        return -1;
    }
    freebigint(&c);
    return 1;
}

int64 bn_int	(varrec * a) {
int32	i;
int64	x;
objrec *	pa;
int32	av_10;
    pa = a->objptr;
    x = 0;
    for (i=0; i<=a->bndigits; ++i) {
        x = x*digitbase+(*pa->bnptr)[i];
    }
    if (!!(*pa->bnptr)[0]) {
        return -x;
    }
    return x;
}

int32 bn_digits	(varrec * bn) {
char	s[31];
    sprintf((char *)&s,"%d",(*bn->objptr->bnptr)[1]);
    return strlen((char *)&s)+(bn->bndigits-1)*digitwidth;
}

void bn_power	(varrec * a,int32 n,varrec * dest) {
varrec	e;
varrec	f;
objrec *	pa;
objrec *	pdest;
    pa = a->objptr;
    if (n < 0) {
        bn_zero(dest);
    }
    else if (n == 0) {
        bn_zero(dest);
        (*dest->objptr->bnptr)[1] = 1;
    }
    else if (n == 1) {
        *dest = *a;
        if (!!dest->hasref) {
            vx_ushare(dest);
        }
    }
    else if ((n&1) == 0) {
        bn_mul(a,a,&e);
        bn_power(&e,n/ 2,dest);
        freebigint(&e);
    }
    else {
        bn_mul(a,a,&e);
        bn_power(&e,(n-1)/ 2,&f);
        freebigint(&e);
        bn_mul(a,&f,dest);
        freebigint(&f);
    }
}


// From module: var_lib
void vx_getptr	(varrec * x) {
    vxunimpl("vx_getptr");
}

void vx_storeptr	(varrec * p,varrec * q) {
varrec *	dest;
varrec *	pptr;
varrec *	qptr;
varrec	v;
int32	i;
int32	n;
int32	etag;
uint32	ii;
uint32	jj;
uint32	mask;
int32	poffset;
int32	qoffset;
int32	bitwidthx;
byte *	pp;
byte *	qq;
int32	aa;
int32	bb;
    switch (p->tag) {
    case trefvar:
        dest = p->varptr;
        if (!!dest->hasref) {
            vx_ufree(dest);
        }
        if (!!q->hasref) {
            vx_ushare(q);
        }
        *dest = *q;
        break;
    case trefpacked:
        vx_storepacked((byte *)(int32 *)p->ptr,q,p->refelemtag);
        if (!!q->hasref) {
            vx_ufree(q);
        }
        break;
    case trefbit:
        vx_storebit(p->ptr,p->refbitoffset,q,p->refelemtag);
        break;
    case tlist:
        vx_popptrlist(p,q);
        if (!!p->hasref) {
            vx_ufree(p);
        }
        if (!!q->hasref) {
            vx_ufree(q);
        }
        break;
    default:;
        pcustype("vx_popptr",p);
    }
}

void vx_assign	(varrec * x,varrec * y) {
    *y = *x;
    vx_cshare(y);
}

void vx_swap	(varrec * x,varrec * y) {
    vxunimpl("vx_swap");
}

void vx_convptr	(varrec * x) {
    vxunimpl("vx_convptr");
}

void vx_istrue	(varrec * x,varrec * y) {
    vxunimpl("vx_istrue");
}

void vx_isfalse	(varrec * x,varrec * y) {
    vxunimpl("vx_isfalse");
}

void vx_isdef	(varrec * x,varrec * y) {
    vxunimpl("vx_isdef");
}

void vx_isvoid	(varrec * x,varrec * y) {
    vxunimpl("vx_isvoid");
}

void vx_eq	(varrec * x,varrec * y,varrec * z) {
    vxunimpl("vx_eq");
}

void vx_ne	(varrec * x,varrec * y,varrec * z) {
    vxunimpl("vx_ne");
}

void vx_lt	(varrec * x,varrec * y,varrec * z) {
    vxunimpl("vx_lt");
}

void vx_le	(varrec * x,varrec * y,varrec * z) {
    vxunimpl("vx_le");
}

void vx_ge	(varrec * x,varrec * y,varrec * z) {
    vxunimpl("vx_ge");
}

void vx_gt	(varrec * x,varrec * y,varrec * z) {
    vxunimpl("vx_gt");
}

void vx_cmp	(varrec * x,varrec * y,varrec * z) {
    vxunimpl("vx_cmp");
}

int32 ivx_istrue	(varrec * x) {
    vxunimpl("ivx_istrue");
    return 0;
}

int32 ivx_isfalse	(varrec * x) {
    vxunimpl("ivx_isfalse");
    return 0;
}

int32 ivx_isdef	(varrec * x) {
    vxunimpl("ivx_isdef");
    return 0;
}

int32 ivx_isvoid	(varrec * x) {
    vxunimpl("ivx_isvoid");
    return 0;
}

int32 ivx_eq	(varrec * x,varrec * y) {
    vxunimpl("ivx_eq");
    return 0;
}

int32 ivx_ne	(varrec * x,varrec * y) {
    vxunimpl("ivx_ne");
    return 0;
}

int32 ivx_lt	(varrec * x,varrec * y) {
    vxunimpl("ivx_lt");
    return 0;
}

int32 ivx_le	(varrec * x,varrec * y) {
    vxunimpl("ivx_le");
    return 0;
}

int32 ivx_ge	(varrec * x,varrec * y) {
    vxunimpl("ivx_ge");
    return 0;
}

int32 ivx_gt	(varrec * x,varrec * y) {
    vxunimpl("ivx_gt");
    return 0;
}

int32 ivx_cmp	(varrec * x,varrec * y) {
    vxunimpl("ivx_cmp");
    return 0;
}

void vx_makelist	(int32 n,varrec * a,varrec * b,int32 lower) {
varrec	v;
varrec *	p;
varrec *	q;
int32	i;
objrec *	l;
int32	av_1;
    a += n-1;
    l = make_listobj(n,lower);
    l->mutable = 0;
    v.tagx = (tlist|hasrefmask);
    v.objptr = l;
    p = l->vptr;
    q = p+l->allocated-1;
    av_1 = n;
    while (av_1--) {
        *p = *a--;
        if (!!p->hasref) {
            vx_ushare(p);
        }
        ++p;
    }
    while (p <= q) {
        p->tagx = tvoid;
        ++p;
    }
    *b = v;
}

void vx_makerecord	(int32 n,int32 t,varrec * a,varrec * b) {
varrec	v;
varrec *	p;
varrec *	q;
int32	i;
objrec *	r;
int32	av_2;
    if (fdebug) {
        printf("%s\n","MAKERECORD");
    }
    a += n-1;
    r = newobject();
    p = r->vptr = make_listdata(n,&(r)->allocated,0);
    r->length = n;
    r->lower = 1;
    v.tagx = (t|hasrefmask);
    v.objptr = r;
    av_2 = n;
    while (av_2--) {
        *p = *a--;
        if (!!p->hasref) {
            vx_dupl(p);
        }
        ++p;
    }
    *b = v;
}

void vx_makearray	(int32 n,int32 arraytype,int32 elemtype,int32 lower,varrec * a,varrec * b) {
varrec	v;
byte *	p;
byte *	q;
int32	i;
int32	esize;
int32	nbytes;
int32	basetag;
objrec *	l;
int32	av_3;
    a += n-1;
    if (elemtype == tvoid) {
        elemtype = a->tag;
        basetag = ttbasetype[elemtype];
        if ((basetag==tarray) || (basetag==tstruct)) {
        }
        else {
            if (basetag > tvariant) {
            }
            else {
                pcerror("makearray elem");
            }
        }
    }
    l = make_arrayobj(n,elemtype,lower);
    v.tagx = (arraytype|hasrefmask);
    v.objptr = l;
    p = l->ptr;
    esize = ttsize[elemtype];
    av_3 = n;
    while (av_3--) {
        vx_storepacked(p,a,elemtype);
        p += esize;
        --a;
    }
    *b = v;
}

void vx_makebits	(int32 n,int32 t,int32 elemtag,int32 lower,varrec * a,varrec * b) {
    vxunimpl("vx_makebits");
}

void vx_makerange	(varrec * x,varrec * y,varrec * z) {
    if (x->tag == tint && y->tag == tint) {
        z->tagx = trange;
        z->range_upper = y->value;
        z->range_lower = x->value;
    }
    else {
        pcmxtypes("vxmakerange",x,y);
    }
}

void vx_makeset	(int32 n,varrec * data,varrec * dest) {
varrec	l;
varrec *	q;
byte *	p;
int32	top;
int32	a;
int32	b;
int32	i;
int32	j;
int32	t;
int32	size;
byte	alloc;
objrec *	s;
static int32	count=0;
int32	av_4;
int32	av_5;
    if (fdebug) {
        printf("%s %s %d %d\n","MAKESET","FDEBUG=",fdebug,++count);
        pcerror("XXX");
    }
    top = 0;
    q = data;
    av_4 = n;
    while (av_4--) {
        switch (q->tag) {
        case trange:
            a = q->range_lower;
            b = q->range_upper;
            break;
        case tint:
            a = q->value;
            b = a;
            break;
        default:;
            b = a = getintvalue(q);
        }
        if (a < 0 || b < 0) {
            pcerror("Neg set element");
        }
        if (a > top) {
            top = a;
        }
        if (b > top) {
            top = b;
        }
        ++q;
    }
    s = make_setobj(top+1);
    l.tagx = (tset|hasrefmask);
    l.objptr = s;
    q = data;
    av_5 = n;
    while (av_5--) {
        switch (q->tag) {
        case trange:
            a = q->range_lower;
            b = q->range_upper;
            if (a > b) {
                t = a;
                a = b;
                b = t;
            }
            break;
        case tint:
            b = a = q->value;
            break;
        default:;
            b = a = getintvalue(q);
        }
        for (j=a; j<=b; ++j) {
            setelem((byte (*)[])s->ptr,j);
        }
        ++q;
    }
    *dest = l;
}

void vx_makestruct	(int32 n,int32 t,varrec * a,varrec * b) {
varrec	v;
byte *	p;
byte *	q;
int32	i;
int32	nfields;
int32	index;
objrec *	l;
strec *	d;
strec *	f;
    if (fdebug) {
        printf("%s\n","MAKESTRUCT");
    }
    l = make_arrayobj(ttsize[t],tu8,1);
    v.tagx = (t|hasrefmask);
    v.objptr = l;
    p = l->ptr;
    if (!runfrompc) {
        d = ttnamedef[t];
        nfields = 0;
        f = d->deflist;
        while (f) {
            if (f->nameid == fieldid && !f->attribs.ax_at) {
                ++nfields;
                if (nfields > n) {
                    pcerror("Too few struct fields");
                }
                vx_storepacked(p+f->offset,a,f->mode);
                ++a;
            }
            f = f->nextdef;
        }
    }
    else {
        index = ttstartfield[t];
        nfields = ttstructfields[t];
        if (nfields != n) {
            pcerror("makestruct: wrong # fields");
        }
        for (i=nfields; i>=1; --i) {
            vx_storepacked(p+(*pcfieldtable)[index+i-1-1].fieldoffset,a,(*pcfieldtable)[index+i-1-1].fieldtype);
            ++a;
        }
    }
    *b = v;
}

void vx_makedict	(int32 n,varrec * a,varrec * b) {
varrec	v;
varrec *	p;
varrec *	q;
int32	i;
int32	m;
objrec *	l;
int32	av_6;
    m = n*2;
    a += m-1;
    l = make_listobj(m_imax(16,nextpoweroftwo(m)),1);
    v.tagx = (tdict|hasrefmask);
    v.objptr = l;
    p = l->vptr;
    q = p+l->allocated-1;
    av_6 = n;
    while (av_6--) {
        adddictitem(&v,a,a-1);
        a -= 2;
    }
    *b = v;
}

static void adddictitem	(varrec * d,varrec * p,varrec * q) {
objrec *	da;
varrec *	r;
    da = d->objptr;
    if (da->length == 0) {
        pcerror("NULL DICT");
    }
    r = finddictitem(da,p,1);
    if (!!r->hasref) {
        vx_ufree(r);
    }
    *r = *q;
    if (!!r->hasref) {
        vx_ushare(r);
    }
}

varrec * finddictitem	(objrec * d,varrec * p,int32 doins) {
int32	hash;
int32	index;
int32	size;
int32	keytag;
int32	keyvalue;
int32	wrapped;
int32	limit;
varrec *	q;
objrec *	pa;
objrec *	qa;
retry:
L35:;
    size = d->length/ 2;
    index = gethashvalue(p)&size-1;
    q = d->vptr+index*2;
    wrapped = 0;
    keytag = p->tag;
    keyvalue = p->value;
    pa = p->objptr;
    while (1) {
        if (q->tag == tvoid) {
            goto L37;
        }
        else if (q->tag == keytag) {
            if ((keytag==tint)) {
                if (q->value == keyvalue) {
                    return q+1;
                }
            } else if ((keytag==tstring)) {
                qa = q->objptr;
                if (pa->length == qa->length) {
                    if (memcmp(pa->strptr,qa->strptr,pa->length) == 0) {
                        return q+1;
                    }
                }
            }
        }
        ++index;
        q += 2;
        if (index >= size) {
            if (wrapped) {
                pcerror("DICT FULL?");
            }
            wrapped = 1;
            index = 1;
            q = d->vptr;
        }
    }
L37:;
    if (doins) {
        limit = size*15/ 16;
        if (d->dictitems >= limit) {
            expanddict(d);
            goto L35;
        }
        *q = *p;
        if (!!q->hasref) {
            vx_ushare(q);
        }
        ++d->dictitems;
        return q+1;
    }
    else {
        return 0;
    }
}

static void expanddict	(objrec * d) {
int32	n;
int32	m;
int32	i;
int32	j;
int32	k;
varrec *	p;
varrec *	q;
varrec *	r;
    n = d->allocated;
    m = n/ 2;
    p = d->vptr;
    d->vptr = make_listdata(n*2,&(d)->allocated,1);
    d->length *= 2;
    d->dictitems = 0;
    q = p;
    for (i=1; i<=m; ++i) {
        if (q->tag != tvoid) {
            r = finddictitem(d,q,1);
            if (!!q->hasref) {
                vx_ufree(q);
            }
            ++q;
            *r = *q++;
        }
        else {
            q += 2;
        }
    }
    free_listdata(p,n);
}

void vx_makevoid	(varrec * x) {
    vxunimpl("vx_makevoid");
}

void vx_makeint	(int64 a,varrec * x) {
    x->tagx = tint;
    x->value = a;
}

void vx_makereal	(double a,varrec * x) {
    x->tagx = treal;
    x->xvalue = a;
}

void vx_makerange_ab	(int32 a,int32 b,varrec * x) {
    x->tagx = treal;
    x->range_lower = a;
    x->range_upper = b;
}

void vx_makestring	(char * s,varrec * x) {
    x->tagx = 65541;
    x->objptr = make_str(s,-1);
}

void vx_makerefvar	(varrec * x,varrec * y) {
    vxunimpl("vx_makerefvar");
}

int64 vx_getint	(varrec * x) {
    vxunimpl("vx_getint");
    return 0;
}

double vx_getreal	(varrec * x) {
    vxunimpl("vx_getreal");
    return 0.0;
}

char * vx_getzstring	(varrec * x) {
    vxunimpl("vx_getzstring");
    return "";
}

void vx_typepun	(int32 t,varrec * x,varrec * y) {
    vxunimpl("vx_typepun");
}

void vx_iconvert	(int32 t,varrec * x) {
int32	s;
int32	tbase;
int64	aa;
varrec	bn;
    s = x->tag;
    if (s == t && s < tlist) {
        return;
    }
    tbase = ttbasetype[t];
    x->tag = t;
    switch (ttbasetype[s]) {
    case tint:
        switch (tbase) {
        case tint:
            break;
        case treal:
            x->xvalue = x->value;
            break;
        case tword:
            break;
        case tlongint:
            bn_makeint(sptr->value,sptr);
            break;
        default:;
            pcustypet("conv dint=>",t);
        }
        break;
    case tword:
        switch (tbase) {
        case tword:
            break;
        case treal:
            break;
        default:;
            pcustypet("conv dint=>",t);
        }
        break;
    case treal:
        switch (tbase) {
        case tint:
            x->value = x->xvalue;
            break;
        default:;
            pcustypet("conv real=>",t);
        }
        break;
    case trefpacked:
    case trefvar:
    case trefbit:
    case trefproc:
        switch (tbase) {
        case tint:
            break;
        default:;
            pcustypet("conv ptr=>",t);
        }
        break;
    case tstring:
        switch (tbase) {
        case tlongint:
            bn_makestr(x->objptr->strptr,x->objptr->length,&bn);
            vx_ufree(x);
            *x = bn;
            break;
        case tstring:
            break;
        default:;
            pcustypet("string=>",t);
        }
        break;
    case ttype:
        if (tbase != tint) {
            pcustypet("type=>",t);
        }
        break;
    case tlongint:
        switch (tbase) {
        case tint:
            aa = bn_int(x);
            vx_ufree(x);
            x->value = aa;
            x->tagx = t;
            break;
        default:;
            pcustypet("longint=>",t);
        }
        break;
    default:;
        pcmxtypestt("HARDCONV s^.t",s,t);
    }
}

int32 ivx_mixed	(varrec * x,varrec * y) {
    vxunimpl("ivx_mixed");
    return 0;
}

void vx_incr	(varrec * x) {
    vxunimpl("vx_incr");
}

void vx_decr	(varrec * x) {
    vxunimpl("vx_decr");
}

void vx_loadincr	(varrec * x,varrec * y) {
    vxunimpl("vx_loadincr");
}

void vx_incrload	(varrec * x,varrec * y) {
    vxunimpl("vx_incrload");
}

void vx_loaddecr	(varrec * x,varrec * y) {
    vxunimpl("vx_loaddecr");
}

void vx_decrload	(varrec * x,varrec * y) {
    vxunimpl("vx_decrload");
}

void vx_negto	(varrec * x) {
    vxunimpl("vx_negto");
}

void vx_absto	(varrec * x) {
    vxunimpl("vx_absto");
}

void vx_inotto	(varrec * x) {
    vxunimpl("vx_inotto");
}

void vx_notl	(varrec * x,varrec * y) {
    vxunimpl("vx_notl");
}

void vx_notlto	(varrec * x) {
    vxunimpl("vx_notlto");
}

void vx_asc	(varrec * x,varrec * y) {
    vxunimpl("vx_asc");
}

void vx_chr	(varrec * x,varrec * y) {
    vxunimpl("vx_chr");
}

int32 ivx_asc	(varrec * x) {
    vxunimpl("ivx_asc");
    return 0;
}

void vx_round	(varrec * x,varrec * y) {
    switch (x->tag) {
    case treal:
        if (x->xvalue >= 0.0) {
            y->xvalue = floor(x->xvalue+0.5);
        }
        else {
            y->xvalue = ceil(x->xvalue-0.5);
        }
        break;
    default:;
        pcustype("ROUND",x);
    }
}

void vx_maths	(int32 op,varrec * x,varrec * y) {
    vxunimpl("vx_maths");
}

void vx_maths2	(int32 op,varrec * x,varrec * y,varrec * z) {
    vxunimpl("vx_maths2");
}

void vx_bounds	(varrec * x,varrec * y) {
int32	xt;
int32	a;
int32	b;
objrec *	p;
    switch (ttbasetype[x->tag]) {
    case tlist:
    case tarray:
        p = x->objptr;
        a = p->lower;
        b = p->length+a-1;
        vx_ufree(x);
        break;
    case tstring:
        p = x->objptr;
        b = p->length;
        a = 1;
        vx_ufree(x);
        break;
    default:;
        pcustype("BOUNDS",x);
    }
    y->tagx = trange;
    y->range_lower = a;
    y->range_upper = b;
}

int32 ivx_len	(varrec * x) {
    vxunimpl("ivx_len");
    return 0;
}

int32 ivx_lwb	(varrec * x) {
    vxunimpl("ivx_lwb");
    return 0;
}

int32 ivx_upb	(varrec * x) {
    vxunimpl("ivx_upb");
    return 0;
}

int32 ivx_gettype	(varrec * x) {
    vxunimpl("ivx_gettype");
    return 0;
}

int32 ivx_getbasetype	(varrec * x) {
    vxunimpl("ivx_getbasetype");
    return 0;
}

int32 ivx_getelemtype	(varrec * x) {
    vxunimpl("ivx_getelemtype");
    return 0;
}

int32 ivx_istype	(int32 t,varrec * x) {
    vxunimpl("ivx_istype");
    return 0;
}

int32 ivx_isnumber	(varrec * x) {
    vxunimpl("ivx_isnumber");
    return 0;
}

int32 ivx_isarray	(varrec * x) {
    vxunimpl("ivx_isarray");
    return 0;
}

int32 ivx_isrecord	(varrec * x) {
    vxunimpl("ivx_isrecord");
    return 0;
}

int32 ivx_ispointer	(varrec * x) {
    vxunimpl("ivx_ispointer");
    return 0;
}

int32 ivx_isset	(varrec * x) {
    vxunimpl("ivx_isset");
    return 0;
}

int32 ivx_isequal	(varrec * x,varrec * y) {
    vxunimpl("ivx_isequal");
    return 0;
}

void vx_tostr	(varrec * x,varrec * y) {
    vxunimpl("vx_tostr");
}

void vx_tostrfmt	(varrec * x,varrec * y,varrec * z) {
    vxunimpl("vx_tostrfmt");
}

void vx_mul	(varrec * x,varrec * y,varrec * z) {
int32	xt;
int32	yt;
varrec	result;
    xt = x->tag;
    yt = y->tag;
    if (xt != yt) {
        if (!!(xt = vx_mixed(x,y))) {
            goto L47;
        }
        xt = x->tag;
        yt = y->tag;
        switch (xt) {
        case tlist:
            switch (yt) {
            case tint:
                vx_mul_listi(x,y,z);
                return;
                break;
            default:;
            }
            break;
        case tstring:
            switch (yt) {
            case tint:
                vx_mul_stri(x,y,z);
                return;
                break;
            default:;
            }
            break;
        default:;
        }
        pcmxtypes("MUL MIXED",x,y);
    }
retry:
L47:;
    switch (xt) {
    case tint:
        z->tagx = tint;
        z->value = x->value*y->value;
        break;
    case treal:
        z->tagx = treal;
        z->xvalue = x->xvalue*y->xvalue;
        break;
    case tword:
        z->tagx = tword;
        z->uvalue = x->uvalue*y->uvalue;
        break;
    case tlongint:
        bn_mul(x,y,&result);
        vx_ufree(x);
        vx_ufree(y);
        *z = result;
        break;
    default:;
        pcustype("MUL",x);
    }
}

void vx_div	(varrec * x,varrec * y,varrec * z) {
int32	xt;
int32	yt;
varrec	result;
    xt = x->tag;
    yt = y->tag;
    if (xt != yt) {
        if (!!(xt = vx_mixed(x,y))) {
            goto L48;
        }
        pcmxtypes("DIV MIXED",x,y);
    }
retry:
L48:;
    switch (xt) {
    case tint:
        z->tagx = treal;
        sptr->xvalue = (double)x->value/ (double)y->value;
        break;
    case treal:
        sptr->xvalue = x->xvalue/ y->xvalue;
        break;
    default:;
        pcustype("DIV",x);
    }
}

void vx_intdiv	(varrec * x,varrec * y,varrec * z) {
int32	xt;
int32	yt;
varrec	result;
    xt = x->tag;
    yt = y->tag;
    if (xt != yt) {
        if (!!(xt = vx_mixed(x,y))) {
            goto L49;
        }
        xt = x->tag;
        yt = y->tag;
        pcmxtypes("INTDIV",x,y);
    }
retry:
L49:;
    switch (xt) {
    case tint:
        if (!!y->value) {
            z->value = x->value/ y->value;
        }
        else {
            raise_error(divide_error);
        }
        break;
    case tword:
        z->uvalue = x->uvalue/ y->uvalue;
        break;
    case tlongint:
        bn_div(x,y,&result);
        vx_ufree(x);
        vx_ufree(y);
        *z = result;
        break;
    default:;
        pcustype("INTDIV",x);
    }
}

void vx_rem	(varrec * x,varrec * y,varrec * z) {
    vxunimpl("rem");
}

void vx_iand	(varrec * x,varrec * y,varrec * z) {
int32	xt;
int32	yt;
    xt = x->tag;
    yt = y->tag;
    if (xt != yt) {
        if (!!(xt = vx_mixed(x,y))) {
            goto L50;
        }
        pcmxtypes("IAND",x,y);
    }
retry:
L50:;
    switch (xt) {
    case tint:
    case tword:
        z->value = x->value&y->value;
        break;
    default:;
        pcustype("IAND",x);
    }
}

void vx_ior	(varrec * x,varrec * y,varrec * z) {
int32	xt;
int32	yt;
    xt = x->tag;
    yt = y->tag;
    if (xt != yt) {
        pcmxtypes("IOR",x,y);
    }
    switch (xt) {
    case tint:
    case tword:
        z->value = x->value|y->value;
        break;
    case tset:
        vx_iorset(x,y,z);
        break;
    default:;
        pcustype("IOR",x);
    }
}

void vx_ixor	(varrec * x,varrec * y,varrec * z) {
int32	xt;
int32	yt;
    xt = x->tag;
    yt = y->tag;
    if (xt != yt) {
        pcmxtypes("IXOR",x,y);
    }
    switch (xt) {
    case tint:
    case tword:
        z->value = x->value^y->value;
        break;
    default:;
        pcustype("IXOR",x);
    }
}

void vx_in	(varrec * x,varrec * y,varrec * z) {
int32	n;
    n = vx_invar(x,y);
    z->tagx = tint;
    z->value = n;
}

void vx_notin	(varrec * x,varrec * y,varrec * z) {
int32	n;
    n = (int32)!vx_invar(x,y);
    z->tagx = tint;
    z->value = n;
}

void vx_inrev	(varrec * x,varrec * y,varrec * z) {
    vxunimpl("inrev");
}

void vx_min	(varrec * x,varrec * y,varrec * z) {
int32	xt;
    xt = x->tag;
    if (xt != y->tag) {
        if ((xt = vx_mixed(x,y)) == 0) {
            pcmxtypes("MIN",x,y);
        }
    }
    switch (xt) {
    case tint:
        sptr->value = ((x->value <= y->value)?(x)->value:(y)->value);
        break;
    case treal:
        sptr->xvalue = ((x->xvalue <= y->xvalue)?(x)->xvalue:(y)->xvalue);
        break;
    default:;
        pcustype("MIN",x);
    }
}

void vx_max	(varrec * x,varrec * y,varrec * z) {
int32	xt;
    xt = x->tag;
    if (xt != y->tag) {
        if ((xt = vx_mixed(x,y)) == 0) {
            pcmxtypes("MAX",x,y);
        }
    }
    switch (xt) {
    case tint:
        sptr->value = ((x->value >= y->value)?(x)->value:(y)->value);
        break;
    case treal:
        sptr->xvalue = ((x->xvalue >= y->xvalue)?(x)->xvalue:(y)->xvalue);
        break;
    default:;
        pcustype("MAX",x);
    }
}

void vx_concat	(varrec * x,varrec * y,varrec * z) {
    z = x;
    vx_dupl(z);
    switch (ttbasetype[z->tag]) {
    case tlist:
        switch (y->tag) {
        case tlist:
            vx_iconcatlist(z,y);
            return;
            break;
        default:;
        }
        z = y;
        break;
    case tstring:
        switch (y->tag) {
        case tstring:
            vx_addtostring(z,y);
            return;
            break;
        default:;
        }
        z = y;
        break;
    default:;
    }
    pcustype("Concat",z);
}

void vx_append	(varrec * x,varrec * y,varrec * z) {
    z = x;
    vx_dupl(z);
    switch (ttbasetype[z->tag]) {
    case tlist:
        vx_iappendlist(z,y);
        return;
        break;
    case tstring:
        switch (y->tag) {
        case tstring:
            vx_addtostring(x,y);
            return;
            break;
        default:;
        }
        break;
    case tarray:
        vx_iappendarray(z,y);
        return;
        break;
    default:;
        pcustype("Append",z);
    }
}

void vx_addto	(varrec * x,varrec * y) {
int32	yt;
int32	ch;
    yt = y->tag;
    switch (x->tag) {
    case tint:
        switch (yt) {
        case tint:
            x->value += y->value;
            return;
            break;
        case treal:
            x->tagx = treal;
            x->xvalue = x->value+y->xvalue;
            return;
            break;
        default:;
        }
        break;
    case treal:
        switch (yt) {
        case tint:
            x->xvalue += y->value;
            return;
            break;
        case treal:
            x->xvalue += y->xvalue;
            return;
            break;
        default:;
        }
        break;
    case tstring:
        switch (yt) {
        case tstring:
        case tint:
            vx_addtostring(x,y);
            return;
            break;
        default:;
        }
        break;
    default:;
    }
    pcmxtypes("VXADDTO",x,y);
}

void vx_subto	(varrec * x,varrec * y) {
int32	yt;
int32	ch;
    yt = y->tag;
    switch (x->tag) {
    case tint:
        switch (yt) {
        case tint:
            x->value -= y->value;
            return;
            break;
        case treal:
            x->tagx = treal;
            x->xvalue = x->value-y->xvalue;
            return;
            break;
        default:;
        }
        break;
    case treal:
        switch (yt) {
        case tint:
            x->xvalue -= y->value;
            return;
            break;
        case treal:
            x->xvalue -= y->xvalue;
            return;
            break;
        default:;
        }
        break;
    default:;
    }
    pcmxtypes("VXSUBTO",x,y);
}

void vx_multo	(varrec * x,varrec * y) {
    vxunimpl("multo");
}

void vx_divto	(varrec * x,varrec * y) {
    vxunimpl("divto");
}

void vx_idivto	(varrec * x,varrec * y) {
    vxunimpl("idivto");
}

void vx_irem	(varrec * x,varrec * y) {
    vxunimpl("irem");
}

void vx_iandto	(varrec * x,varrec * y) {
    vxunimpl("iandto");
}

void vx_iorto	(varrec * x,varrec * y) {
    vxunimpl("iorto");
}

void vx_ixorto	(varrec * x,varrec * y) {
    vxunimpl("ixorto");
}

void vx_shlto	(varrec * x,varrec * y) {
    vxunimpl("shlto");
}

void vx_shrto	(varrec * x,varrec * y) {
    vxunimpl("shrto");
}

void vx_minto	(varrec * x,varrec * y) {
    vxunimpl("minto");
}

void vx_maxto	(varrec * x,varrec * y) {
    vxunimpl("maxto");
}

void vx_concatto	(varrec * x,varrec * y) {
int32	yt;
    yt = y->tag;
    switch (ttbasetype[x->tag]) {
    case tlist:
        switch (ttbasetype[yt]) {
        case tlist:
            vx_iconcatlist(x,y);
            return;
            break;
        default:;
        }
        break;
    case tstring:
        switch (ttbasetype[yt]) {
        case tstring:
            vx_addtostring(x,y);
            return;
            break;
        default:;
        }
        break;
    default:;
    }
    pcmxtypes("VXCONCATTO",x,y);
}

void vx_appendto	(varrec * x,varrec * y) {
    switch (ttbasetype[x->tag]) {
    case tlist:
        vx_iappendlist(x,y);
        return;
        break;
    case tstring:
        switch (y->tag) {
        case tstring:
            vx_addtostring(x,y);
            return;
            break;
        default:;
        }
        break;
    case tarray:
        vx_iappendarray(x,y);
        return;
        break;
    default:;
    }
    pcustype("VXAPPENDTO",x);
}

void vx_getdot	(int32 g,varrec * x,varrec * y) {
    vxunimpl("getdot");
}

void vx_getdotref	(int32 g,varrec * x,varrec * y) {
    vxunimpl("getdotref");
}

void vx_indexref	(varrec * a,varrec * y,varrec * z) {
objrec *	p;
int32	index;
int32	elemtype;
    if ((y->tag==tint)) {
    } else if ((y->tag==trange)) {
        vx_sliceref(a,y,z);
        return;
    }
    else {
        pcustype("&index?",y);
    }
    switch (ttbasetype[a->tag]) {
    case tlist:
    case trecord:
        p = a->objptr;
        if (!p->mutable) {
            p = a->objptr = copyonwrite(p,tlist);
        }
        index = (y->value-p->lower);
        if ((uint32)index >= (uint32)p->length) {
            if (index < 0) {
                pcerror("LWB");
            }
            else {
                if ((uint32)index == (uint32)p->length) {
                    vx_iappendlist(a,0);
                    p = a->objptr;
                }
                else {
                    pcerror("&getixref list[i] bounds");
                }
            }
        }
        z->tagx = trefvar;
        z->varptr = p->vptr+index;
        break;
    case tstring:
        p = a->objptr;
        index = (y->value-1);
        if (!p->mutable) {
            a->objptr = p = copyonwrite(p,tstring);
        }
        if ((uint32)index >= (uint32)p->length) {
            pcerror("&str[int] bounds");
        }
        z->tagx = trefpacked;
        z->refelemtag = tstring;
        z->ptr = (byte *)(p->strptr+index);
        break;
    case tarray:
        p = a->objptr;
        if (!p->mutable) {
            p = a->objptr = copyonwrite(p,a->tag);
        }
        index = (y->value-p->lower);
        if ((uint32)index >= (uint32)p->length) {
            if (index < 0) {
                pcerror("&AXLWB");
            }
            else {
                if ((uint32)index == (uint32)p->length) {
                    vx_iappendarray(a,0);
                    p = a->objptr;
                }
                else {
                    pcerror("&AXBOUNDS");
                }
            }
        }
        elemtype = p->elemtag;
        z->tagx = trefpacked;
        z->refelemtag = elemtype;
        z->ptr = p->ptr+index*ttsize[elemtype];
        break;
    default:;
        pcustype("&index[int]",a);
    }
}

void vx_sliceref	(varrec * x,varrec * y,varrec * z) {
    vxunimpl("getixsliceref");
}

void vx_dotindexref	(varrec * x,varrec * y,varrec * z) {
objrec *	p;
byte *	q;
int32	index;
    if ((y->tag==tint)) {
    } else if ((y->tag==trange)) {
        vx_dotsliceref(x,y,z);
        return;
    }
    else {
        pcustype("&.index?",y);
    }
    switch (ttbasetype[x->tag]) {
    case tstring:
        p = x->objptr;
        index = (y->value-1);
        if ((uint32)index >= (uint32)p->length) {
            pcerror("&str.[int] bounds");
        }
        z->tagx = trefpacked;
        z->refelemtag = tu8;
        z->ptr = (byte *)(p->strptr+index);
        break;
    case trecord:
        vx_indexref(x,y,z);
        break;
    case tint:
        index = y->value;
        if (index < 0 || index >= 64) {
            pcerror("int.[int] bounds");
        }
        q = (byte *)&(x)->value;
        z->packptr = q+(index>>3);
        z->tagx = trefbit;
        z->refelemtag = tbit;
        z->refbitoffset = (index&7);
        break;
    default:;
        pcustype("&.index[int]",x);
    }
}

void vx_dotslice	(varrec * x,varrec * y,varrec * z) {
int32	i;
int32	j;
    switch (x->tag) {
    case tstring:
        vx_slicestring(x,y,z);
        break;
    case tint:
        i = y->range_lower;
        j = y->range_upper;
        if (j < i) {
            {int32	temp;
                temp = i;
                i = j;
                j = temp;
            }
        }
        if (i >= 64 || j >= 64) {
            pcerror("int.[slice] bounds");
        }
        z->value = x->value>>i&~(18446744073709551615ull<<(j-i)+1);
        break;
    default:;
        pcustype(".slice",x);
    }
}

void vx_dotsliceref	(varrec * x,varrec * y,varrec * z) {
    vxunimpl("getdotixsliceref");
}

void vx_getkeyix	(varrec * d,varrec * k,varrec * def,varrec * z) {
varrec *	p;
    if (d->tag != tdict) {
        pcustype("keyix",d);
    }
    p = finddictitem(d->objptr,k,0);
    vx_ufree(d);
    if (!!k->hasref) {
        vx_ufree(k);
    }
    if (p) {
        *z = *p;
        return;
    }
    if (def) {
        *z = *def;
    }
    else {
        z->tagx = tvoid;
    }
}

void vx_getkeyixref	(varrec * x,varrec * y,varrec * z) {
    vxunimpl("getkeyixref");
}

void vx_iconvcase	(varrec * a,varrec * b,int32 upper) {
int32	i;
int32	n;
char *	s;
objrec *	pa;
int32	av_7;
int32	av_8;
    pa = a->objptr;
    if (b->tag > tvoid) {
        n = getintvalue(b);
    }
    else {
        n = pa->length;
    }
    if (a->tag != tstring) {
        pcerror("convcase/notstr");
    }
    if (n < 0) {
        pcerror("CONVCASE N<0");
    }
    if (n == 0) {
        return;
    }
    if (n > pa->length) {
        printf("%s %d %d\n","N=",n,pa->length);
        pcerror("convcase/N?");
    }
    s = pa->strptr;
    if (upper) {
        av_7 = n;
        while (av_7--) {
            *s = toupper((uchar)*s);
            ++s;
        }
    }
    else {
        av_8 = n;
        while (av_8--) {
            *s = tolower((uchar)*s);
            ++s;
        }
    }
}

void vx_loadpacked	(void * p,int32 t,varrec * dest,objrec * ownerobj) {
int32	length;
varrec *	q;
varrec *	r;
int32 *	pp;
objrec *	s;
    switch (ttbasetype[t]) {
    case ti8:
        dest->tagx = tint;
        dest->value = *(int8 *)p;
        break;
    case ti16:
        dest->tagx = tint;
        dest->value = *(int16 *)p;
        break;
    case ti32:
        dest->tagx = tint;
        dest->value = *(int32 *)p;
        break;
    case ti64:
    case tint:
        dest->tagx = tint;
        dest->value = *(int64 *)p;
        break;
    case tu8:
        dest->tagx = tint;
        dest->value = *(byte *)p;
        break;
    case tu16:
        dest->tagx = tint;
        dest->value = *(uint16 *)p;
        break;
    case tu32:
        dest->tagx = tint;
        dest->value = *(uint32 *)p;
        break;
    case tu64:
        dest->tagx = tword;
        dest->value = *(uint32 *)p;
        break;
    case tr64:
        dest->tagx = treal;
        dest->xvalue = *(double *)p;
        break;
    case tr32:
        dest->tagx = treal;
        dest->xvalue = *(float *)p;
        break;
    case tstring:
        dest->tagx = (tstring|hasrefmask);
        length = ttlength[t];
        if (length >= 2) {
            length = getfslength((char *)p,length);
        }
        else {
            length = 1;
        }
        s = make_strslicexobj((char *)p,length);
        dest->objptr = s;
        break;
    case tstringz:
        dest->tagx = (tstring|hasrefmask);
        length = strlen((char *)p);
        s = make_strslicexobj((char *)p,length);
        dest->objptr = s;
        break;
    case trefpacked:
        dest->tagx = trefpacked;
        dest->ptr = (byte *)*(int32 * *)p;
        dest->refelemtag = tttarget[t];
        break;
    case tstruct:
        s = newobject();
        s->mutable = 1;
        s->lower = 1;
        s->ptr = (byte *)p;
        s->length = ttlength[t];
        dest->objptr = s;
        dest->tagx = (t|hasrefmask);
        if (ownerobj) {
            s->objtype = slice_obj;
            s->objptr2 = ownerobj;
            ++ownerobj->refcount;
        }
        else {
            s->objtype = extslice_obj;
        }
        break;
    default:;
        printf("%s %d %d\n","T=",t,ttbasetype[t]);
        pcmxtypestt("loadpacked",t,ttbasetype[t]);
    }
}

void vx_storepacked	(byte * p,varrec * q,int32 t) {
int32	plength;
int32	qlength;
int32	s;
int32	sbase;
int32	tbase;
objrec *	qa;
    sbase = ttbasetype[s = q->tag];
    tbase = ttbasetype[t];
    switch (sbase) {
    case tint:
    case tword:
        switch (tbase) {
        case ti8:
        case tu8:
            *p = q->value;
            return;
            break;
        case ti16:
        case tu16:
            *(uint16 *)p = q->value;
            return;
            break;
        case ti32:
        case tu32:
            *(int32 *)p = q->value;
            return;
            break;
        case ti64:
        case tu64:
        case tint:
        case tword:
            *(int64 *)p = q->value;
            return;
            break;
        case tr32:
            *(float *)p = q->value;
            return;
            break;
        case tr64:
            *(double *)p = q->value;
            return;
            break;
        default:;
        }
        break;
    case treal:
        switch (tbase) {
        case ti32:
        case tu32:
            *(int32 *)p = q->xvalue;
            return;
            break;
        case tr32:
            *(float *)p = q->xvalue;
            return;
            break;
        case tr64:
            *(double *)p = q->xvalue;
            return;
            break;
        default:;
        }
        break;
    case tstring:
        qa = q->objptr;
        plength = ttlength[t];
        qlength = qa->length;
        switch (tbase) {
        case tstring:
            if (t == tbase) {
                if (qlength != 1) {
                    pcerror("Str not len 1");
                }
                *(char *)p = (uchar)*qa->strptr;
                return;
            }
            if (qlength > plength) {
                qlength = plength;
            }
            memcpy((int32 *)p,(int32 *)qa->strptr,qlength);
            setfslength((char *)p,plength,qlength);
            return;
            break;
        case tstringz:
            if (qlength > plength-1) {
                qlength = plength-1;
            }
            memcpy((int32 *)p,(int32 *)qa->strptr,qlength);
            *(p+qlength) = 0;
            return;
            break;
        default:;
        }
        break;
    case tstruct:
        if (s != t) {
            pcmxtypestt("spack struct",s,t);
        }
        memcpy((int32 *)p,(int32 *)q->objptr->ptr,ttsize[t]);
        return;
        break;
    default:;
    }
    pcmxtypestt("storepacked (source->dest)",s,t);
}

void vx_loadbit	(byte * p,int32 shift,int32 t,varrec * dest) {
    dest->tagx = tint;
    switch (t) {
    case tbit:
        dest->value = (int64)!!(*p&1<<shift);
        break;
    case tbit2:
        dest->value = ((*p&3<<shift)>>shift);
        break;
    case tbit4:
        dest->value = ((*p&15<<shift)>>shift);
        break;
    default:;
        pcustypet("loadbit",t);
    }
}

void vx_storebit	(byte * p,int32 shift,varrec * q,int32 t) {
byte	bb;
    if (q->tag != tint) {
        pcerror("storebit not int");
    }
    switch (t) {
    case tbit:
        *p = ((*p&~(1<<shift))|(q->value&1)<<shift);
        break;
    case tbit2:
        *p = ((*p&~(3<<shift))|(q->value&3)<<shift);
        break;
    case tbit4:
        *p = ((*p&~(15<<shift))|(q->value&15)<<shift);
        break;
    default:;
        pcustypet("storebit",t);
    }
}

static void setfslength	(char * s,int32 m,int32 n) {
    if (m == n) {
    }
    else if (n == m-1) {
        *(s+m-1) = 0;
    }
    else {
        *(s+m-2) = 0;
        *(s+m-1) = n;
    }
}

static int32 getfslength	(char * s,int32 m) {
    s += m-1;
    if ((uchar)*(s-1) == 0) {
        return (uchar)*s;
    }
    else if ((uchar)*s == 0) {
        return m-1;
    }
    else {
        return m;
    }
}

int32 gethashvalue	(varrec * p) {
int32	hsum;
int32	csum;
int32	c;
int32	n;
int32	i;
char *	s;
int32	av_9;
    switch (p->tag) {
    case tstring:
        n = p->objptr->length;
        if (!n) {
            return 0;
        }
        hsum = csum = 0;
        s = p->objptr->strptr;
        av_9 = n;
        while (av_9--) {
            c = (uchar)*s++;
            csum += c;
            hsum = (hsum<<2)+c;
        }
        return (c+(hsum<<3)^csum^c)&2147483647;
        break;
    case tint:
        return p->value;
        break;
    default:;
        printf("%u\n",p->tag);
        pcustype("Can't hash:",p);
    }
    return 0;
}

int32 vx_mixed	(varrec * x,varrec * y) {
    switch (x->tag) {
    case tint:
        switch (y->tag) {
        case treal:
            x->xvalue = x->value;
            x->tagx = treal;
            return treal;
            break;
        case tword:
            return tint;
            break;
        case tvoid:
            goto L70;
            break;
        default:;
            return 0;
        }
        break;
    case treal:
        switch (y->tag) {
        case tint:
            y->xvalue = y->value;
            y->tagx = treal;
            return treal;
            break;
        case tvoid:
            goto L70;
            break;
        default:;
            return 0;
        }
        break;
    case tword:
        switch (y->tag) {
        case tint:
            return tword;
            break;
        case tvoid:
            goto L70;
            break;
        default:;
            return 0;
        }
        break;
    case tvoid:
dxvoid:
L70:;
        pcerror("dxmix/void");
        break;
    default:;
        if (y->tag == tvoid) {
            goto L70;
        }
    }
    return 0;
}

void vx_loadptr	(varrec * x,varrec * y) {
    switch (x->tag) {
    case trefvar:
        *y = *x->varptr;
        if (!!y->hasref) {
            ++y->objptr->refcount;
        }
        break;
    case trefpacked:
        vx_loadpacked(x->ptr,x->refelemtag,y,0);
        break;
    default:;
        pcustype("vx_loadptr",x);
    }
}

void vx_popptrlist	(varrec * p,varrec * q) {
int32	i;
int32	nleft;
int32	nright;
varrec	v;
varrec *	pdata;
varrec *	qdata;
objrec *	pp;
objrec *	qq;
    pp = p->objptr;
    nleft = pp->length;
    pdata = pp->vptr;
    v.tagx = tvoid;
    switch (ttbasetype[q->tag]) {
    case tlist:
        qq = q->objptr;
        nright = qq->length;
dolist:
L71:;
        qdata = qq->vptr;
        for (i=1; i<=nleft; ++i) {
            if (i <= nright) {
                vx_storeptr(pdata,qdata++);
            }
            else {
                vx_storeptr(pdata,&v);
            }
            ++pdata;
        }
        break;
    case trange:
        for (i=1; i<=nleft; ++i) {
            if (i <= 2) {
                v.tagx = tint;
                v.value = ((i == 1)?(q->range_lower):(q->range_upper));
                vx_storeptr(pdata,&v);
            }
            else {
                v.tagx = tvoid;
                vx_storeptr(pdata,&v);
            }
            ++pdata;
        }
        break;
    case trecord:
        qq = q->objptr;
        nright = ttlength[q->tag];
        goto L71;
        break;
    case tarray:
        pcerror("POPPTRLIST ARRAY");
        break;
    default:;
        pcustype("popptrlist",q);
    }
}

void vx_iappendarray	(varrec * a,varrec * b) {
int32	n;
int32	lower;
byte *	q;
objrec *	p;
    p = a->objptr;
    if (p->objtype != normal_obj) {
        pcerror("Can't extend slice");
    }
    if (!p->mutable) {
        p = copyonwrite(p,a->tag);
    }
    n = p->length+1;
    if (n > p->allocated) {
        resize_arrayobj(p,n);
    }
    else {
        p->length = n;
    }
    a->objptr = p;
    q = (byte *)(p->strptr+(p->length-1)*ttsize[p->elemtag]);
    if (b) {
        vx_storepacked(q,b,p->elemtag);
    }
}


// From module: var_print
void pch_print	(varrec * p,varrec * fmt) {
char	str[360];
varrec	v;
    if (mfmtstr == 0) {
        if (!!mgapneeded) {
            printstr_n(" ",1);
        }
        else {
            mgapneeded = 1;
        }
    }
    else {
        printnextfmtchars(0);
    }
    switch (ttbasetype[p->tag]) {
    case tstring:
        if (fmt == 0 || fmt->tag == tvoid) {
            printstr_n((char *)p->objptr->ptr,p->objptr->length);
            return;
        }
        break;
    case tint:
    case treal:
    case trange:
    case tword:
        pch_tostr(p,fmt,&v);
        printstr_n(v.objptr->strptr,v.objptr->length);
        return;
        break;
    default:;
    }
    pch_tostr(p,fmt,&v);
    printstr_n(v.objptr->strptr,v.objptr->length);
}

void pch_println	(void) {
    if (mfmtstr) {
        printnextfmtchars(1);
    }
    printstrz("\r\n");
}

void pch_startprintcon	(void) {
varrec	v;
    v.tagx = tint;
    v.value = 0;
    pch_startprint(&v);
}

void pch_startprint	(varrec * p) {
objrec *	s;
    switch (++noclevels) {
    case 0:
    case 1:
        break;
    case 7:
        printerror("print #x overflow");
        break;
    default:;
        moutdevstack[noclevels-1] = moutdev;
        moutchanstack[noclevels-1] = moutchan;
        moutvarstack[noclevels-1] = moutvar;
        mfmtstrstack[noclevels-1] = mfmtstr;
        mfmtcurrstack[noclevels-1] = mfmtcurr;
        mgapstack[noclevels-1] = mgapneeded;
    }
    mfmtstr = 0;
    mfmtcurr = 0;
    if (p == 0) {
        goto L1;
    }
    switch (p->tag) {
    case tint:
        switch (p->value) {
        case 0:
doconsole:
L1:;
            moutdev = std_io;
            moutchan = 0;
            break;
        case 1:
            moutdev = str_io;
            moutchan = 0;
            moutvar.tagx = (tstring|hasrefmask);
            s = newobject();
            s->mutable = 1;
            s->length = 0;
            s->elemtag = tu8;
            moutvar.objptr = s;
            break;
        case 2:
            if (testfilech == 0) {
                prterror("@2: file not open");
            }
            moutdev = file_io;
            moutchan = testfilech;
            break;
        default:;
            moutdev = file_io;
            moutchan = (void *)(int32)p->value;
        }
        break;
    case trefvar:
        p = p->varptr;
        switch (p->tag) {
        case tstring:
            moutdev = istr_io;
            moutchan = 0;
            moutvar.tagx = trefvar;
            moutvar.varptr = p;
            break;
        default:;
            printf("%s\n",ttname[p->tag]);
            prterror("Print@^?");
        }
        break;
    default:;
        switch (ttbasetype[p->tag]) {
        case trecord:
        case tstruct:
            printf("%s %s\n","STARTPRINT:",ttname[p->tag]);
            moutdev = std_io;
            break;
        default:;
            printf("%s\n",ttname[p->tag]);
            printerror("Can't do startprint...");
        }
    }
    mgapneeded = 0;
}

void pch_endprint	(void) {
varrec *	p;
    if (mfmtstr) {
        printnextfmtchars(1);
    }
    switch (moutdev) {
    case istr_io:
        p = moutvar.varptr;
        break;
    default:;
    }
    if (mfmtstr != 0) {
        pcm_free(mfmtstr,strlen(mfmtstr)+1);
    }
    if (--noclevels == -1) {
        printerror("resetoc??");
    }
    if (noclevels == 0) {
        moutdev = std_io;
    }
    else {
        moutdev = moutdevstack[noclevels];
        moutchan = moutchanstack[noclevels];
        moutvar = moutvarstack[noclevels];
        mgapneeded = mgapstack[noclevels];
        mfmtstr = mfmtstrstack[noclevels];
        mfmtcurr = mfmtcurrstack[noclevels];
    }
}

void pch_strstartprint	(void) {
varrec	p;
    p.tagx = tint;
    p.value = 1;
    pch_startprint(&p);
}

void pch_strendprint	(varrec * dest) {
    if (mfmtstr) {
        printnextfmtchars(1);
    }
    if (moutdev != str_io) {
        prterror("STRendPRT/NOT STR");
    }
    *dest = moutvar;
    moutvar.tagx = tvoid;
    pch_endprint();
}

void pch_setformat	(varrec * p) {
int32	n;
char *	s;
    if (p->tag != tstring) {
        prterror("(str)");
    }
    if (mfmtstr) {
        prterror("Setfmt?");
    }
    n = p->objptr->length;
    mfmtstr = (char *)pcm_alloc(n+1);
    if (n) {
        memcpy((int32 *)mfmtstr,(int32 *)p->objptr->ptr,n);
    }
    s = mfmtstr+n;
    *s = 0;
    mfmtcurr = mfmtstr;
}

void pch_setformat2	(varrec * p) {
    printf("%s\n","PC/SETFORMAT2");
}

void pch_dprint	(varrec * p,varrec * fmt) {
    pch_print(p,fmt);
    switch (p->tag) {
    case tint:
        printstrz("d");
        break;
    case tword:
        printstrz("u");
        break;
    default:;
    }
}

void pch_printnogap	(void) {
    mgapneeded = 0;
}

static void initfmtcode	(fmtrec * f) {
    *f = defaultfmt;
}

static int32 i64mintostr	(char * s,int32 base,int32 sep) {
char	t[360];
int32	i;
int32	j;
int32	k;
int32	g;
int32	neg;
    switch (base) {
    case 10:
        strcpy((char *)&t,"9223372036854775808");
        j = 3;
        break;
    case 16:
        strcpy((char *)&t,"8000000000000000");
        j = 1;
        break;
    case 2:
        strcpy((char *)&t,"1000000000000000000000000000000000000000000000000000000000000000");
        j = 7;
        break;
    default:;
        strcpy((char *)&t,"<mindint>");
    }
    i = strlen((char *)&t);
    s += i;
    if (sep) {
        s += j;
    }
    *s = 0;
    k = 0;
    g = ((base == 10)?3:4);
    while (i) {
        --s;
        *s = (uchar)t[i---1];
        if (sep && i && ++k == g) {
            --s;
            *s = sep;
            k = 0;
        }
    }
    return strlen(s);
}

static int32 u64tostr	(uint64 aa,char * s,uint32 base,int32 sep) {
char	t[360];
int32	i;
int32	j;
int32	k;
int32	g;
int32	dummy;
char *	s0;
    i = 0;
    k = 0;
    g = ((base == 10)?3:4);
    do {
        t[++i] = (uchar)digits[(aa% base)];
        aa = aa/ base;
        if (sep && aa != 0 && ++k == g) {
            t[++i] = sep;
            k = 0;
        }
    } while (!(aa == 0));
    j = i;
    s0 = s;
    while (i) {
        *s = (uchar)t[i--];
        ++s;
    }
    *s = 0;
    return j;
}

static int32 i64tostrfmt	(int64 aa,char * s,fmtrec * fmt,int32 usigned) {
char	str[360];
int32	i;
int32	j;
int32	k;
int32	n;
int32	w;
static uint64	mindint=9223372036854775808ull;
    if (!!(uchar)fmt->usigned) {
        usigned = 1;
    }
    if (aa == mindint && !usigned) {
        str[0] = '-';
        n = i64mintostr(&str[1],fmt->base,(uchar)fmt->sepchar)+1;
    }
    else {
        if (!usigned && aa < 0 || !!(uchar)fmt->plus) {
            if (aa < 0) {
                aa = -aa;
                str[0] = '-';
            }
            else {
                str[0] = '+';
            }
            n = u64tostr(aa,&str[1],fmt->base,(uchar)fmt->sepchar)+1;
        }
        else {
            n = u64tostr(aa,&str[0],fmt->base,(uchar)fmt->sepchar);
        }
    }
    if (!!(uchar)fmt->suffix) {
        str[n] = (uchar)fmt->suffix;
        str[++n] = 0;
    }
    if (fmt->base > 10 || !!(uchar)fmt->suffix && (uchar)fmt->lettercase == 'a') {
        convlcstring((char *)&str);
    }
    return expandstr((char *)&str,s,n,fmt);
}

static int32 u64tostrfmt	(int64 aa,char * s,fmtrec * fmt) {
char	str[360];
int32	i;
int32	j;
int32	k;
int32	n;
int32	w;
    n = u64tostr(aa,&str[0],fmt->base,(uchar)fmt->sepchar);
    if (!!(uchar)fmt->suffix) {
        str[n] = (uchar)fmt->suffix;
        str[++n] = 0;
    }
    if (fmt->base > 10 || !!(uchar)fmt->suffix && (uchar)fmt->lettercase == 'a') {
        convlcstring((char *)&str);
    }
    return expandstr((char *)&str,s,n,fmt);
}

static int32 strtostrfmt	(char * s,char * t,int32 n,fmtrec * fmt) {
char *	u;
char *	v;
char	str[256];
int32	w;
int32	nheap;
    nheap = 0;
    if (!!(uchar)fmt->quotechar || !!(uchar)fmt->lettercase) {
        if (n < 256) {
            u = &str[1-1];
        }
        else {
            nheap = n+3;
            u = (char *)pcm_alloc(nheap);
        }
        if (!!(uchar)fmt->quotechar) {
            v = u;
            *v = (uchar)fmt->quotechar;
            ++v;
            if (n) {
                strcpy(v,s);
                v += n;
            }
            *v = (uchar)fmt->quotechar;
            ++v;
            *v = 0;
            n += 2;
        }
        switch ((uchar)fmt->lettercase) {
        case 'a':
            convlcstring(u);
            break;
        case 'A':
            convucstring(u);
            break;
        default:;
        }
        s = u;
    }
    w = fmt->minwidth;
    if (w > n) {
        n = expandstr(s,t,n,fmt);
    }
    else {
        memcpy((int32 *)t,(int32 *)s,n);
    }
    if (nheap) {
        pcm_free(u,nheap);
    }
    return n;
}

static int32 expandstr	(char * s,char * t,int32 n,fmtrec * fmt) {
int32	i;
int32	w;
int32	m;
int32	av_1;
int32	av_2;
int32	av_3;
int32	av_4;
int32	av_5;
    w = fmt->minwidth;
    if (w == 0 || w <= n) {
        strncpy(t,s,n);
        *(t+n) = 0;
        return n;
    }
    if ((uchar)fmt->justify == 'L') {
        strncpy(t,s,n);
        t += n;
        for (i=1; i<=w-n; ++i) {
            *t = (uchar)fmt->padchar;
            ++t;
        }
        *t = 0;
    }
    else if ((uchar)fmt->justify == 'R') {
        if ((uchar)fmt->padchar == '0' && !!fmt->base && ((uchar)*s == '-' || (uchar)*s == '+')) {
            *t = (uchar)*s;
            ++t;
            av_2 = w-n;
            while (av_2--) {
                *t = (uchar)fmt->padchar;
                ++t;
            }
            strncpy(t,s+1,n-1);
            *(t+n-1) = 0;
        }
        else {
            av_3 = w-n;
            while (av_3--) {
                *t = (uchar)fmt->padchar;
                ++t;
            }
            strncpy(t,s,n);
            *(t+n) = 0;
        }
    }
    else {
        m = ((w-n)+1)/ 2;
        av_4 = m;
        while (av_4--) {
            *t = (uchar)fmt->padchar;
            ++t;
        }
        strncpy(t,s,n);
        t += n;
        av_5 = (w-n)-m;
        while (av_5--) {
            *t = (uchar)fmt->padchar;
            ++t;
        }
        *t = 0;
    }
    return w;
}

void pc_strtofmt	(char * s,int32 slen,fmtrec * fmt) {
char	c;
byte	wset;
int32	n;
char	str[100];
    initfmtcode(fmt);
    memcpy((int32 *)&str,(int32 *)s,slen);
    str[slen] = 0;
    s = (char *)&str;
    wset = 0;
    while ((uchar)*s) {
        c = (uchar)*s;
        ++s;
        switch ((uchar)c) {
        case 'B':
        case 'b':
            fmt->base = 2;
            break;
        case 'H':
        case 'h':
            fmt->base = 16;
            break;
        case 'O':
        case 'o':
            fmt->base = 8;
            break;
        case 'X':
        case 'x':
            c = (uchar)*s;
            if (!!(uchar)c) {
                switch ((uchar)c) {
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    c = (uchar)c-'0';
                    break;
                case 'A':
                case 'B':
                case 'C':
                case 'D':
                case 'E':
                case 'F':
                    c = ((uchar)c-'A')+10;
                    break;
                case 'a':
                case 'b':
                case 'c':
                case 'd':
                case 'e':
                case 'f':
                    c = ((uchar)c-'a')+10;
                    break;
                default:;
                    c = 10;
                }
                fmt->base = (uchar)c;
                ++s;
            }
            break;
        case 'Q':
        case 'q':
            fmt->quotechar = '"';
            break;
        case '~':
            fmt->quotechar = '~';
            break;
        case 'J':
        case 'j':
            fmt->justify = toupper((uchar)*s);
            if (!!(uchar)*s) {
                ++s;
            }
            break;
        case 'A':
            fmt->lettercase = 'A';
            break;
        case 'a':
            fmt->lettercase = 'a';
            break;
        case 'Z':
        case 'z':
            fmt->padchar = '0';
            break;
        case 'S':
        case 's':
            fmt->sepchar = (uchar)*s;
            if (!!(uchar)*s) {
                ++s;
            }
            break;
        case 'P':
        case 'p':
            fmt->padchar = (uchar)*s;
            if (!!(uchar)*s) {
                ++s;
            }
            break;
        case 'T':
        case 't':
            fmt->suffix = (uchar)*s;
            if (!!(uchar)*s) {
                ++s;
            }
            break;
        case 'U':
        case 'u':
            fmt->usigned = 'U';
            break;
        case 'E':
        case 'e':
            fmt->realfmt = 'e';
            break;
        case 'F':
        case 'f':
            fmt->realfmt = 'f';
            break;
        case 'G':
        case 'g':
            fmt->realfmt = 'g';
            break;
        case '.':
            wset = 1;
            break;
        case comma:
        case '_':
            fmt->sepchar = (uchar)c;
            break;
        case '+':
            fmt->plus = '+';
            break;
        case 'M':
        case 'm':
            fmt->charmode = 'M';
            break;
        case 'C':
        case 'c':
            fmt->charmode = 'C';
            break;
        default:;
            if ((uchar)c >= '0' && (uchar)c <= '9') {
                n = ((uchar)c-'0');
                while (1) {
                    c = (uchar)*s;
                    if ((uchar)*s == 0) {
                        goto L34;
                    }
                    if ((uchar)c >= '0' && (uchar)c <= '9') {
                        ++s;
                        n = n*10+(uchar)c-'0';
                    }
                    else {
                        goto L34;
                    }
                }
L34:;
                if (!wset) {
                    fmt->minwidth = n;
                    wset = 1;
                }
                else {
                    fmt->precision = n;
                }
            }
        }
    }
}

static void printstrz	(char * s) {
int32	x;
    switch (moutdev) {
    case std_io:
        printf("%s",s);
        break;
    case file_io:
        fprintf(moutchan,"%s",s);
        break;
    case str_io:
        addstring((objrec *)&moutvar,s,-1);
        break;
    case istr_io:
        printstr_n(s,strlen(s));
        break;
    case wind_io:
        break;
    default:;
    }
}

static void printstr_n	(char * s,int32 n) {
varrec *	p;
int32	x;
    if (n == -1) {
        n = strlen(s);
    }
    if (n == 0) {
        return;
    }
    switch (moutdev) {
    case std_io:
        printf("%.*s",n,s);
        break;
    case file_io:
        fprintf(moutchan,"%.*s",n,s);
        break;
    case str_io:
        addstring(moutvar.objptr,s,n);
        break;
    case istr_io:
        p = moutvar.varptr;
        if (p->tag != tstring) {
            prterror("prtstrn1");
        }
        addstring((objrec *)moutvar.varptr,s,n);
        break;
    case wind_io:
        break;
    default:;
    }
}

void printerror	(char * s) {
    prterror(s);
}

static void addstring	(objrec * p,char * t,int32 n) {
int32	oldlen;
int32	newlen;
int32	oldbytes;
int32	newbytes;
char *	newptr;
    if (n == 0 || (uchar)*t == 0) {
        return;
    }
    if (n < 0) {
        n = strlen(t);
    }
    oldlen = p->length;
    if (p->refcount == 0) {
        if (oldlen == 0) {
            memcpy((int32 *)p->ptr,(int32 *)t,n);
            p->length = n;
        }
        else {
            memcpy((int32 *)(p->ptr+oldlen),(int32 *)t,n);
            p->length = oldlen+n;
        }
        p->lower = 1;
        return;
    }
    if (oldlen == 0) {
        p->ptr = (byte *)pcm_alloc(n);
        p->length = n;
        p->allocated = allocbytes;
        memcpy((int32 *)p->ptr,(int32 *)t,n);
    }
    else {
        newlen = oldlen+n;
        oldbytes = p->allocated;
        newbytes = oldlen+n;
        if (newbytes <= oldbytes) {
            memcpy((int32 *)(p->ptr+oldlen),(int32 *)t,n);
        }
        else {
            newptr = (char *)pcm_alloc(newbytes);
            memcpy((int32 *)newptr,(int32 *)p->ptr,oldlen);
            memcpy((int32 *)(newptr+oldlen),(int32 *)t,n);
            p->allocated = allocbytes;
            pcm_free(p->ptr,oldbytes);
            p->ptr = (byte *)newptr;
        }
        p->length = newlen;
    }
    p->lower = 1;
}

void tostr_i	(varrec * p,varrec * fmtstr,fmtrec * fmt,objrec * dest) {
char	str[360];
    switch ((uchar)fmt->charmode) {
    case 'M':
        domultichar((char *)&(p)->value,8,(char *)&str,fmt);
        break;
    case 'C':
        str[1] = p->value;
        str[2] = 0;
        break;
    default:;
        i64tostrfmt(p->value,(char *)&str,fmt,0);
    }
    addstring(dest,(char *)&str,strlen((char *)&str));
}

void tostr_r	(varrec * p,varrec * fmtstr,fmtrec * fmt,objrec * dest) {
char	str[360];
char	str2[360];
char	cfmt[10];
int32	n;
    cfmt[0] = '%';
    if (!!fmt->precision) {
        cfmt[1] = '.';
        cfmt[2] = '*';
        cfmt[3] = (uchar)fmt->realfmt;
        cfmt[4] = 0;
        sprintf((char *)&str,(char *)&cfmt,fmt->precision,p->xvalue);
    }
    else {
        cfmt[1] = (uchar)fmt->realfmt;
        cfmt[2] = 0;
        sprintf((char *)&str,(char *)&cfmt,p->xvalue);
    }
    n = strlen((char *)&str);
    if (n < fmt->minwidth) {
        expandstr((char *)&str,(char *)&str2,n,fmt);
        strcpy((char *)&str,(char *)&str2);
    }
    addstring(dest,(char *)&str,strlen((char *)&str));
}

void tostr_u	(varrec * p,varrec * fmtstr,fmtrec * fmt,objrec * dest) {
char	str[360];
    switch ((uchar)fmt->charmode) {
    case 'M':
        domultichar((char *)&(p)->uvalue,8,(char *)&str,fmt);
        break;
    case 'C':
        str[1] = p->uvalue;
        str[2] = 0;
        break;
    default:;
        u64tostrfmt(p->value,(char *)&str,fmt);
    }
    addstring(dest,(char *)&str,strlen((char *)&str));
}

void tostr_range	(varrec * p,varrec * fmtstr,fmtrec * fmt,objrec * dest) {
char	str[360];
char	str2[360];
char	cfmt[10];
int32	n;
    i64tostrfmt(p->range_lower,(char *)&str,fmt,0);
    strcat((char *)&str,"..");
    addstring(dest,(char *)&str,-1);
    i64tostrfmt(p->range_upper,(char *)&str,fmt,0);
    addstring(dest,(char *)&str,-1);
}

void tostr_s	(varrec * p,varrec * fmtstr,fmtrec * fmt,objrec * dest) {
int32	oldlen;
int32	newlen;
char *	s;
char	str[100];
objrec *	q;
    q = p->objptr;
    oldlen = q->length;
    newlen = oldlen;
    if (!!(uchar)fmt->quotechar || fmt->minwidth > newlen) {
        if (!!(uchar)fmt->quotechar) {
            newlen += 2;
        }
        if (fmt->minwidth > newlen) {
            newlen = fmt->minwidth;
        }
        s = (char *)pcm_alloc(newlen+1);
        strtostrfmt(q->strptr,s,oldlen,fmt);
        addstring(dest,s,newlen);
        pcm_free(s,newlen+1);
    }
    else {
        addstring(dest,q->strptr,oldlen);
    }
}

void tostr_list	(varrec * p,varrec * fmtstr,fmtrec * fmt,objrec * dest) {
varrec *	q;
int32	i;
char	c;
objrec *	r;
    ++listdepth;
    r = p->objptr;
    if (r->refcount < 0 || listdepth > maxlistdepth) {
        addstring(dest,"...",3);
        --listdepth;
        return;
    }
    addstring(dest,"(",1);
    r->refcount = -r->refcount;
    q = r->vptr;
    for (i=p->objptr->length; i>=1; --i) {
        do_tostr(q,fmtstr,fmt,dest);
        ++q;
        if (i != 1) {
            addstring(dest,",",1);
        }
    }
    addstring(dest,")",1);
    r->refcount = -r->refcount;
    --listdepth;
}

void tostr_ax	(varrec * p,varrec * fmtstr,fmtrec * fmt,objrec * dest) {
char	str[360];
byte *	q;
int32	i;
int32	m;
int32	elemtype;
int32	a;
int32	b;
varrec	v;
objrec *	pa;
byte *	ptr;
    printf("%s\n","TOSTR/AX");
    if (fmt == 0) {
        fmt = &defaultfmt;
    }
    m = p->tag;
    pa = p->objptr;
    a = pa->lower;
    elemtype = pa->elemtag;
    b = pa->length+a-1;
    q = pa->ptr;
    sprintf((char *)&str,"%s[%d:%s]",ttname[m],pa->lower,ttname[elemtype]);
    addstring(dest,(char *)&str,-1);
    addstring(dest,"A(",-1);
    for (i=a; i<=b; ++i) {
        vx_loadpacked(q,elemtype,&v,0);
        q += ttsize[elemtype];
        do_tostr(&v,fmtstr,fmt,dest);
        if (i < b) {
            addstring(dest,",",1);
        }
    }
    addstring(dest,")",1);
}

void tostr_bits	(varrec * p,varrec * fmtstr,fmtrec * fmt,varrec * dest) {
char	str[360];
char *	q;
int32	i;
int32	m;
int32	elemtype;
int32	a;
int32	b;
int32	offset;
int32	bitwidthx;
int32	n;
varrec	v;
byte *	pbyte;
objrec *	pa;
    if (fmt == 0) {
        fmt = &defaultfmt;
    }
    m = p->tag;
    pa = p->objptr;
    a = pa->lower;
    elemtype = pa->elemtag;
    offset = pa->bitoffset-1;
    b = pa->length+a-1;
    bitwidthx = ttbitwidth[elemtype];
    sprintf((char *)&str,"%s[%d:%s]",ttname[m],pa->lower,ttname[elemtype]);
    addstring((objrec *)dest,(char *)&str,-1);
    addstring((objrec *)dest,"B(",-1);
    q = (char *)pa->ptr;
    for (i=a; i<=b; ++i) {
        offset += bitwidthx;
        if (offset >= 8) {
            offset = 0;
            ++q;
        }
        do_tostr(&v,fmtstr,fmt,(objrec *)dest);
        if (i < b) {
            addstring((objrec *)dest,",",1);
        }
    }
    addstring((objrec *)dest,")",1);
}

void tostr_set	(varrec * p,varrec * fmtstr,fmtrec * fmt,objrec * dest) {
char	str[360];
varrec *	q;
int32	i;
int32	j;
int32	first;
varrec	v;
objrec *	s;
    if (fmt == 0) {
        fmt = &defaultfmt;
    }
    addstring(dest,"[",1);
    s = p->objptr;
    first = 1;
    i = 0;
    while (i < s->length) {
        if (!!testelem((byte (*)[])s->ptr,i)) {
            j = i+1;
            while (j < s->length && !!testelem((byte (*)[])s->ptr,j)) {
                ++j;
            }
            --j;
            if (!first) {
                addstring(dest,",",1);
            }
            first = 0;
            if (i == j) {
                v.tagx = tint;
                v.value = i;
            }
            else {
                v.tagx = trange;
                v.range_lower = i;
                v.range_upper = j;
            }
            do_tostr(&v,fmtstr,fmt,dest);
            i = j+1;
        }
        else {
            ++i;
        }
    }
    addstring(dest,"]",1);
}

void tostr_struct	(varrec * p,varrec * fmtstr,fmtrec * fmt,objrec * dest) {
char	str[360];
byte	needcomma;
int32	i;
int32	j;
int32	stag;
int32	ftype;
int32	offset;
int32	index;
byte *	ptr;
varrec	v;
objrec *	pa;
strec *	d;
strec *	f;
enum {maxfields = 100};
int32	fieldtypes[100];
int32	fieldoffsets[100];
int32	nfields;
    if (fmt == 0) {
        fmt = &defaultfmt;
    }
    stag = p->tag;
    if (!runfrompc) {
        d = ttnamedef[stag];
        nfields = 0;
        f = d->deflist;
        while (f) {
            if (f->nameid == fieldid && !f->attribs.ax_at) {
                ++nfields;
                fieldtypes[nfields-1] = f->mode;
                fieldoffsets[nfields-1] = f->offset;
            }
            f = f->nextdef;
        }
    }
    else {
        index = ttstartfield[stag];
        nfields = ttstructfields[stag];
        for (i=1; i<=nfields; ++i) {
            fieldtypes[(nfields-i)+1-1] = (*pcfieldtable)[index+i-1-1].fieldtype;
            fieldoffsets[(nfields-i)+1-1] = (*pcfieldtable)[index+i-1-1].fieldoffset;
        }
    }
    pa = p->objptr;
    ptr = pa->ptr;
    addstring(dest,"(",-1);
    needcomma = 0;
    for (i=nfields; i>=1; --i) {
        ftype = fieldtypes[i-1];
        offset = fieldoffsets[i-1];
        vx_loadpacked((ptr+offset),ftype,&v,0);
        if (!!needcomma) {
            addstring(dest,",",1);
        }
        needcomma = 1;
        do_tostr(&v,fmtstr,fmt,dest);
    }
    addstring(dest,")",1);
}

void tostr_long	(varrec * p,varrec * fmtstr,fmtrec * fmt,objrec * dest) {
int32	i;
int32	w;
char	str[100];
objrec *	pp;
int32	av_6;
    pp = p->objptr;
    if (!!(*pp->bnptr)[0]) {
        addstring(dest,"-",1);
    }
    w = 6;
    for (i=1; i<=p->bndigits; ++i) {
        if (i == 1) {
            sprintf((char *)&str,"%d",(*pp->bnptr)[1]);
            addstring(dest,(char *)&str,-1);
        }
        else {
            sprintf((char *)&str,"%0*d",w,(*pp->bnptr)[i]);
            addstring(dest,(char *)&str,w);
        }
    }
    addstring(dest,"L",1);
}

void tostr_dict	(varrec * p,varrec * fmtstr,fmtrec * fmt,objrec * dest) {
char	str[360];
varrec *	q;
int32	i;
int32	length;
int32	needcomma=0;
objrec *	pa;
    if (fmt == 0) {
        fmt = &defaultfmt;
    }
    addstring(dest,"[",-1);
    pa = p->objptr;
    q = pa->vptr;
    length = pa->length/ 2;
    for (i=length; i>=1; --i) {
        if (q->tag == tvoid) {
            q += 2;
            goto L70;
        }
        if (needcomma) {
            addstring(dest,",",1);
        }
        needcomma = 1;
        do_tostr(q,fmtstr,fmt,dest);
        q++;
        addstring(dest,":",1);
        do_tostr(q,fmtstr,fmt,dest);
        q++;
L70:;
    }
    addstring(dest,"]",1);
}

void do_tostr	(varrec * a,varrec * fmtstr,fmtrec * fmt,objrec * dest) {
int32	i;
int32	n;
int32	t;
int32	u;
int32	cmd;
char	str[360];
varrec *	q;
    switch (ttbasetype[a->tag]) {
    case tvoid:
        addstring(dest,"<Void>",-1);
        break;
    case tint:
        tostr_i(a,fmtstr,fmt,dest);
        break;
    case treal:
        tostr_r(a,fmtstr,fmt,dest);
        break;
    case tword:
        tostr_u(a,fmtstr,fmt,dest);
        break;
    case trange:
        tostr_range(a,fmtstr,fmt,dest);
        break;
    case tstring:
        tostr_s(a,fmtstr,fmt,dest);
        break;
    case tlist:
    case trecord:
        tostr_list(a,fmtstr,fmt,dest);
        break;
    case tarray:
        tostr_ax(a,fmtstr,fmt,dest);
        break;
    case tbits:
        tostr_bits(a,fmtstr,fmt,(varrec *)dest);
        break;
    case tset:
        tostr_set(a,fmtstr,fmt,dest);
        break;
    case tstruct:
        tostr_struct(a,fmtstr,fmt,dest);
        break;
    case tlongint:
        tostr_long(a,fmtstr,fmt,dest);
        break;
    case tdict:
        tostr_dict(a,fmtstr,fmt,dest);
        break;
    case trefpacked:
        sprintf((char *)&str,"Ref %s:%p",ttname[a->refelemtag],a->ptr);
        addstring(dest,(char *)&str,-1);
        break;
    case trefbit:
        sprintf((char *)&str,"Refbit %s:%p @%d",ttname[a->refelemtag],a->ptr,a->refbitoffset);
        addstring(dest,(char *)&str,-1);
        break;
    case trefvar:
        sprintf((char *)&str,"Refvar:%p",a->varptr);
        addstring(dest,(char *)&str,-1);
        if (!!a->varptr) {
            sprintf((char *)&str," <%s>",ttname[a->varptr->tag]);
            addstring(dest,(char *)&str,-1);
        }
        break;
    case trecordlink:
        sprintf((char *)&str,"Link:<%s>",ttname[a->refelemtag]);
        addstring(dest,(char *)&str,-1);
        break;
    case trefproc:
        sprintf((char *)&str,"Refproc:%p",a->ptr);
        addstring(dest,(char *)&str,-1);
        break;
    case treflabel:
        sprintf((char *)&str,"Reflabel:%p",a->ptr);
        addstring(dest,(char *)&str,-1);
        break;
    case ttype:
        addstring(dest,"<",1);
        addstring(dest,ttname[a->value],-1);
        addstring(dest,">",1);
        break;
    case toperator:
        addstring(dest,"<OP:",-1);
        cmd = a->value;
        addstring(dest,cmdnames[cmd]+1,-1);
        addstring(dest,((a->opdims == 1)?":1":":2"),2);
        addstring(dest,">",1);
        break;
    default:;
        pcustype("tostr_def",a);
    }
}

static void printnextfmtchars	(int32 last) {
char	c;
char *	pstart;
int32	n;
    pstart = mfmtcurr;
    n = 0;
    while (1) {
        c = (uchar)*mfmtcurr;
        switch ((uchar)c) {
        case '#':
            if (last) {
                goto L75;
            }
            ++mfmtcurr;
            if (n) {
                printstr_n(pstart,n);
            }
            return;
            break;
        case 0:
            if (n) {
                printstr_n(pstart,n);
            }
            else if (!last) {
                printstr_n("|",1);
            }
            return;
            break;
        case '~':
            if (n) {
                printstr_n(pstart,n);
                n = 0;
            }
            ++mfmtcurr;
            c = (uchar)*mfmtcurr;
            if (!!(uchar)c) {
                ++mfmtcurr;
                printstr_n(&c,1);
            }
            pstart = mfmtcurr;
            break;
        default:;
skip:
L75:;
            ++n;
            ++mfmtcurr;
        }
    }
}

static int32 getreadfmtcode	(varrec * p) {
char	c;
    if (p == 0 || p->tag == tvoid) {
        return 'A';
    }
    if (p->tag != tstring) {
        printf("%s %s\n","P=%s",ttname[p->tag]);
        prterror("Readfmt?");
    }
    if (p->objptr->length == 0) {
        return 'A';
    }
    c = toupper((uchar)*p->objptr->strptr);
    switch ((uchar)c) {
    case 'I':
    case 'R':
    case 'N':
    case 'S':
    case 'F':
    case 'T':
    case 'Z':
    case 'C':
    case 'L':
    case 'H':
    case 'B':
    case 'A':
    case 'E':
        return (uchar)c;
        break;
    default:;
    }
    prterror("Readfmt2?");
    return 0;
}

void pch_sreadln	(varrec * dev,varrec * dest) {
    pch_readln(dev);
    dest->tagx = (tstring|hasrefmask);
    dest->objptr = make_str(kb_start,kb_length);
}

void pch_strtoval	(varrec * p,varrec * fmt,varrec * dest) {
int32	fmtcode;
int32	length;
char *	s;
    fmtcode = getreadfmtcode(fmt);
    if (p->tag != tstring) {
        prterror("strval");
    }
    length = p->objptr->length;
    s = (char *)p->objptr->ptr;
    switch (fmtcode) {
    case 'I':
        readint(s,length,dest);
        break;
    case 'R':
        readreal(s,length,dest);
        break;
    case 'N':
        readname(s,length,dest);
        break;
    case 'S':
        readstring(s,length,dest);
        break;
    case 'H':
        readhex(s,length,dest);
        break;
    case 'B':
        readbin(s,length,dest);
        break;
    case 'A':
        readany(s,length,dest);
        break;
    default:;
        prterror("strval:fmt?");
    }
}

void pch_reread	(void) {
    kb_pos = kb_lastpos;
    kb_length = kb_lastlength;
}

void pch_rereadln	(void) {
    kb_pos = kb_start;
    kb_length = kb_linelength;
}

static char * readname	(char * s,int32 length,varrec * dest) {
char *	send;
char *	itemstr;
int32	itemlength;
    send = readitem(s,length,&itemstr,&itemlength);
    makevarstr(itemstr,itemlength,dest);
    iconvlcn(dest->objptr->strptr,dest->objptr->length);
    return send;
}

static char * readstring	(char * s,int32 length,varrec * dest) {
char *	send;
char *	itemstr;
int32	itemlength;
    send = readitem(s,length,&itemstr,&itemlength);
    makevarstr(itemstr,itemlength,dest);
    return send;
}

static char * readint	(char * sold,int32 length,varrec * dest) {
char *	p;
char *	s;
char *	send;
char *	itemstr;
int32	itemlength;
int32	numlength;
    send = readitem(sold,length,&s,&itemlength);
    strtoint(s,itemlength,dest);
    return send;
}

static char * readhex	(char * sold,int32 length,varrec * dest) {
char	str[128];
char *	p;
char *	s;
byte	res;
int64	aa;
int32	a;
int32	t;
int32	nalloc;
char	c;
    if (length == 0) {
        dest->tagx = tint;
        dest->value = 0;
        termchar = 0;
        return sold;
    }
    while (length && ((uchar)*sold == ' ' || (uchar)*sold == 9)) {
        ++sold;
        --length;
    }
    if (length <= maxstrlen) {
        s = (char *)&str;
        nalloc = 0;
    }
    else {
        nalloc = length+1;
        s = (char *)pcm_alloc(nalloc);
    }
    p = s;
    while (length) {
        c = toupper((uchar)*sold);
        ++sold;
        --length;
        if ((uchar)c >= '0' && (uchar)c <= '9') {
            *p = (uchar)c;
            ++p;
        }
        else if ((uchar)c >= 'A' && (uchar)c <= 'F') {
            *p = (uchar)c;
            ++p;
        }
        else if ((uchar)c == '_') {
        }
        else {
            termchar = (uchar)c;
            goto L84;
        }
    }
L84:;
    *p = 0;
    length = p-s;
    if (length <= 16) {
        t = tint;
    }
    else {
        t = tlongint;
    }
    p = s;
    switch (t) {
    case tint:
        aa = 0;
        while (1) {
            c = (uchar)*p;
            ++p;
            if ((uchar)c == 0) {
                goto L91;
            }
            if ((uchar)c < 'A') {
                aa = aa*16+(uchar)c-'0';
            }
            else {
                aa = aa*16+((uchar)c-'A')+10;
            }
        }
L91:;
        dest->tagx = tint;
        dest->value = aa;
        break;
    default:;
        prterror("Readhex/long");
    }
    if (nalloc) {
        pcm_free(s,nalloc);
    }
    return sold;
}

static char * readbin	(char * sold,int32 length,varrec * dest) {
char	str[128];
char *	p;
char *	s;
byte	res;
int64	aa;
int32	a;
int32	t;
int32	nalloc;
char	c;
    if (length == 0) {
        dest->tagx = tint;
        dest->value = 0;
        termchar = 0;
        return sold;
    }
    while (length && ((uchar)*sold == ' ' || (uchar)*sold == 9)) {
        ++sold;
        --length;
    }
    if (length <= maxstrlen) {
        s = (char *)&str;
        nalloc = 0;
    }
    else {
        nalloc = length+1;
        s = (char *)pcm_alloc(nalloc);
    }
    p = s;
    while (length) {
        c = toupper((uchar)*sold);
        ++sold;
        --length;
        if ((uchar)c >= '0' && (uchar)c <= '1') {
            *p = (uchar)c;
            ++p;
        }
        else if ((uchar)c == '_') {
        }
        else {
            termchar = (uchar)c;
            goto L97;
        }
    }
L97:;
    *p = 0;
    length = p-s;
    if (length <= 64) {
        t = tint;
    }
    else {
        t = tlongint;
    }
    p = s;
    switch (t) {
    case tint:
        aa = 0;
        while (1) {
            c = (uchar)*p;
            ++p;
            if ((uchar)c == 0) {
                goto L103;
            }
            aa = aa*2+(uchar)c-'0';
        }
L103:;
        dest->tagx = tint;
        dest->value = aa;
        break;
    default:;
        prterror("Readbin/long");
    }
    if (nalloc) {
        pcm_free(s,nalloc);
    }
    return sold;
}

static char * readreal	(char * sold,int32 length,varrec * dest) {
char	str[512];
double	x;
char *	send;
char *	itemstr;
int32	itemlength;
int32	numlength;
    send = readitem(sold,length,&itemstr,&itemlength);
    strtoreal(itemstr,itemlength,dest);
    return send;
}

void pch_readln	(varrec * dev) {
void *	ch;
int32	length;
objrec *	pdev;
    if (kb_start == 0) {
        kb_start = (char *)pcm_alloc(minkb_size);
        kb_size = minkb_size;
        kb_lastpos = kb_start;
        kb_pos = kb_start;
        kb_length = 0;
        kb_lastlength = 0;
        kb_linelength = 0;
    }
    switch (dev->tag) {
    case tvoid:
doconsole:
L104:;
        readlinen(0,kb_start,kb_size);
        kb_length = strlen(kb_start);
        break;
    case tint:
        switch (dev->value) {
        case 0:
            goto L104;
            break;
        case 1:
            if (testfilech == 0) {
                prterror("R@2: file not open");
            }
            ch = testfilech;
            break;
        default:;
            ch = (void *)(int32)dev->value;
        }
        pc_readlinen(ch,kb_start,kb_size);
        kb_length = strlen(kb_start);
        break;
    case tstring:
        pdev = dev->objptr;
        length = pdev->length;
        if (length == 0) {
            kb_length = 0;
            *kb_start = 0;
        }
        else if (length >= kb_size) {
            prterror("KB overflow");
        }
        else {
            kb_length = length;
            memcpy((int32 *)kb_start,(int32 *)pdev->strptr,length);
        }
        break;
    default:;
        printf("%s\n",gettypename(dev->tag));
        prterror("readln@");
    }
    kb_pos = kb_start;
    kb_lastpos = kb_pos;
    kb_linelength = kb_length;
}

static void stepkbpos	(char * s) {
int32	newlen;
    newlen = s-kb_pos;
    if (newlen == 0) {
        return;
    }
    if (newlen >= kb_length) {
        kb_pos = kb_pos+kb_length;
        kb_length = 0;
    }
    else {
        kb_pos = kb_pos+newlen;
        kb_length -= newlen;
    }
}

void pch_sread	(varrec * fmt,varrec * dest) {
int32	fmtcode;
char	c;
    fmtcode = getreadfmtcode(fmt);
    kb_lastpos = kb_pos;
    kb_lastlength = kb_length;
    switch (fmtcode) {
    case 'I':
        stepkbpos(readint(kb_pos,kb_length,dest));
        break;
    case 'R':
        stepkbpos(readreal(kb_pos,kb_length,dest));
        break;
    case 'N':
        stepkbpos(readname(kb_pos,kb_length,dest));
        break;
    case 'S':
        stepkbpos(readstring(kb_pos,kb_length,dest));
        break;
    case 'H':
        stepkbpos(readhex(kb_pos,kb_length,dest));
        break;
    case 'B':
        stepkbpos(readbin(kb_pos,kb_length,dest));
        break;
    case 'A':
        stepkbpos(readany(kb_pos,kb_length,dest));
        break;
    case 'L':
        if (kb_length == 0) {
doemptystring:
L108:;
            dest->tagx = (tstring|hasrefmask);
            dest->objptr = emptystring;
            ++emptystring->refcount;
        }
        else {
            dest->tagx = (tstring|hasrefmask);
            dest->objptr = make_str(kb_pos,kb_length);
            kb_pos += kb_length;
            kb_length = 0;
        }
        break;
    case 'C':
        if (kb_length == 0) {
            goto L108;
        }
        termchar = (uchar)*kb_pos;
dochar:
L109:;
        dest->tagx = tint;
        dest->value = (uchar)termchar;
        ++kb_pos;
        --kb_length;
        break;
    case 'Z':
        goto L109;
        break;
    case 'E':
        dest->tagx = tint;
        dest->value = itemerror;
        break;
    default:;
        prterror("SREAD/FMT?");
    }
}

static void domultichar	(char * p,int32 n,char * dest,fmtrec * fmt) {
char	str[20];
char *	q;
int32	i;
int32	nchars;
int32	av_7;
    q = (char *)&str;
    nchars = n;
    av_7 = n;
    while (av_7--) {
        if ((uchar)*p == 0) {
            goto L112;
        }
        *q = (uchar)*p;
        ++q;
        ++p;
    }
L112:;
    *q = 0;
    expandstr((char *)&str,dest,nchars,fmt);
}

void pch_tostr	(varrec * a,varrec * b,varrec * result) {
fmtrec	fmt;
fmtrec *	ifmt;
objrec *	p;
    ifmt = pc_getfmt(b,&fmt);
    p = newobject();
    p->mutable = 1;
    p->length = 0;
    p->elemtag = tu8;
    listdepth = 0;
    do_tostr(a,b,ifmt,p);
    result->tagx = (tstring|hasrefmask);
    result->objptr = p;
}

fmtrec * pc_getfmt	(varrec * p,fmtrec * fmt) {
    if (p == 0 || p->tag == tvoid) {
        return &defaultfmt;
    }
    else {
        if (p->tag != tstring) {
            prterror("pc_getfmt/not str?");
        }
        if (p->objptr->strptr == 0) {
            return &defaultfmt;
        }
        else {
            pc_strtofmt(p->objptr->strptr,p->objptr->length,fmt);
            return fmt;
        }
    }
}

static void pc_readlinen	(void * handlex,char * buffer,int32 size) {
char *	p;
int32	n;
int32	x;
char	buff[100];
byte	crseen;
int32	oldpos;
    *buffer = 0;
    fgets(buffer,size-2,handlex);
    n = strlen(buffer);
    if (n == 0) {
        return;
    }
    p = buffer+n-1;
    crseen = 0;
    while (p >= buffer && ((uchar)*p == 13 || (uchar)*p == 10)) {
        if ((uchar)*p == 13 || (uchar)*p == 10) {
            crseen = 1;
        }
        *p-- = 0;
    }
    if (!crseen && n+4 > size) {
        printf("%d %d\n",size,n);
        abortprogram("line too long");
    }
}

static void makevarstr	(char * itemstr,int32 length,varrec * dest) {
    dest->tagx = (tstring|hasrefmask);
    if (length) {
        dest->objptr = make_str(itemstr,length);
    }
    else {
        dest->objptr = emptystring;
        ++emptystring->refcount;
    }
}

static char * readitem	(char * s,int32 length,char * * itemstr,int32 * itemlength) {
char *	p;
char	quotechar;
char	c;
    while (length && ((uchar)*s == ' ' || (uchar)*s == 9)) {
        ++s;
        --length;
    }
    *itemstr = s;
    if (length == 0) {
        termchar = 0;
        *itemlength = 0;
        return s;
    }
    quotechar = 0;
    if ((uchar)*s == '"') {
        quotechar = '"';
        ++s;
        --length;
    }
    else if ((uchar)*s == '\'') {
        quotechar = '\'';
        ++s;
        --length;
    }
    p = *itemstr = s;
    while (length) {
        c = (uchar)*s++;
        --length;
        switch ((uchar)c) {
        case ' ':
        case 9:
        case comma:
        case '=':
            if (!!(uchar)quotechar || p == s) {
                goto L125;
            }
            termchar = (uchar)c;
            goto L124;
            break;
        default:;
normalchar:
L125:;
            if ((uchar)c == (uchar)quotechar) {
                if (length && (uchar)*s == (uchar)quotechar) {
                    *p = (uchar)c;
                    ++s;
                    ++p;
                }
                else {
                    termchar = (uchar)*s;
                    if ((uchar)termchar == ',' || (uchar)termchar == '=') {
                        ++s;
                        termchar = (uchar)*s;
                    }
                    goto L124;
                }
            }
            else {
                *p = (uchar)c;
                ++p;
            }
        }
    }
L124:;
    if (length == 0) {
        termchar = 0;
    }
    *itemlength = p-*itemstr;
    return s;
}

static char * readany	(char * sold,int32 length,varrec * dest) {
char	str[128];
char *	p;
char *	s;
byte	signd;
byte	res;
int64	aa;
int32	digits;
int32	expon;
int32	other;
int32	t;
int32	nalloc;
char	c;
char *	send;
char *	itemstr;
int32	itemlength;
int32	numlength;
int32	av_8;
    itemerror = 0;
    send = readitem(sold,length,&s,&itemlength);
    p = s;
    digits = expon = other = 0;
    av_8 = itemlength;
    while (av_8--) {
        switch ((uchar)*p++) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
        case '+':
        case '-':
        case '_':
            digits = 1;
            break;
        case 'E':
        case 'e':
        case '.':
            expon = 1;
            break;
        default:;
            other = 1;
        }
    }
    dest->tagx = tint;
    if (other || itemlength == 0) {
        dest->value = 5395539;
        makevarstr(s,itemlength,dest);
    }
    else if (expon) {
        strtoreal(s,itemlength,dest);
    }
    else {
        strtoint(s,itemlength,dest);
    }
    return send;
}

static void strtoreal	(char * s,int32 length,varrec * dest) {
char	str[512];
double	x;
int32	numlength;
    dest->tagx = treal;
    if (length >= 512) {
        dest->xvalue = 0.0;
        return;
    }
    memcpy((int32 *)&str,(int32 *)s,length);
    str[length+1-1] = 0;
    itemerror = 0;
    if (sscanf((char *)&str,"%lf%n",&x,&numlength) == 0 || numlength != length) {
        if (numlength == length) {
            x = 0.0;
        }
        itemerror = 1;
    }
    dest->xvalue = x;
}

static void strtoint	(char * s,int32 length,varrec * dest) {
char	str[128];
char *	p;
char *	q;
byte	signd;
int64	aa;
int32	a;
int32	res;
int32	t;
int32	nalloc;
char	c;
    itemerror = 0;
    signd = 0;
    if (length && (uchar)*s == '-') {
        signd = 1;
        ++s;
        --length;
    }
    else if (length && (uchar)*s == '+') {
        ++s;
        --length;
    }
    p = q = s;
    while (length) {
        c = (uchar)*q++;
        --length;
        if ((uchar)c >= '0' && (uchar)c <= '9') {
            *p = (uchar)c;
            ++p;
        }
        else {
            if ((uchar)c == '_') {
            }
            else {
                itemerror = 1;
                goto L137;
            }
        }
    }
L137:;
    *p = 0;
    length = p-s;
    if (length <= 18) {
        t = tint;
    }
    else if (length == 19 && !!(res = strcmp(s,"9223372036854775808"),res <= 0)) {
        if (res == 0 && !signd) {
            t = tlongint;
        }
        else {
            t = tint;
        }
    }
    else {
        t = tlongint;
    }
    p = s;
    if (t == tint) {
        aa = 0;
        while (1) {
            c = (uchar)*p;
            ++p;
            if ((uchar)c == 0) {
                goto L142;
            }
            aa = aa*10+((uchar)c-'0');
        }
L142:;
        if (!!signd) {
            aa = -aa;
        }
        dest->tagx = tint;
        dest->value = aa;
    }
    else {
        bn_makestr(s,length,dest);
    }
}


// From module: mm_nosdll
int64 os_calldll_wint	(void * fnaddr,int64 (*params)[],int32 nparams) {
    return 0;
}

double os_calldll_wreal	(void * fnaddr,int64 (*params)[],int32 nparams) {
    return 0.0;
}

void os_dummycall	(double a,double b,double c,double d) {
}


// From module: pc_oslayer
int32 runproc_m	(void * amsg) {
varrec	a;
varrec	b;
varrec	dest;
static int32	rmsg_typeno;
int32	i;
int32	result;
objrec	obj;
    if (rmsg_typeno == 0) {
        for (i=1; i<=ntypes; ++i) {
            if (strcmp(ttname[i],"ws_msg32") == 0) {
                rmsg_typeno = i;
                goto L4;
            }
        }
L4:;
    }
    if (rmsg_typeno == 0) {
        abortprogram("mainwndproc: can't find rmsg");
    }
    memset((int32 *)&obj,0,32);
    obj.refcount = 99;
    obj.ptr = (byte *)amsg;
    a.tagx = (rmsg_typeno|hasrefmask);
    a.objptr = &obj;
    runproc(pcl_callbackfn,&a,0,&dest);
    result = dest.value;
    result = 0;
    return result;
}

void os_getconsize	(varrec * result) {
uint64	aa;
    pcerror("GETCONSIZE");
}

void pch_setmesshandler	(varrec * fn) {
    if (fn->tag != trefproc) {
        pcerror("Not refproc");
    }
    pcl_callbackfn = (void (*)	(void))fn->ptr;
    os_setmesshandler(&runproc_m);
}

void pch_gethostname	(varrec * result) {
static char	name[256];
    strcpy((char *)&name,os_gethostname());
    result->tagx = (tstring|hasrefmask);
    result->objptr = make_str((char *)&name,-1);
}

void os_initdllmodules	(void) {
int32	i;
int32	hinst;
char *	dllname;
    for (i=1; i<=ndlltable; ++i) {
        dllname = dlltable[i-1];
        hinst = os_loaddllmodule(dllname);
        if (hinst == 0) {
        }
        dllinsttable[i-1] = hinst;
    }
}

int32 os_loaddllmodule	(char * dllname) {
int32	hinst;
    if (strcmp(dllname,"jpeglib") == 0) {
        dllname = "jpeglib64";
    }
    else if (strcmp(dllname,"jpeglibc") == 0) {
        dllname = "jpeglibc32";
    }
    hinst = os_getdllinst(dllname);
    return hinst;
}

void os_initdllfunctions	(void) {
void (*fnaddr)	(void);
int32	hinst;
int32	i;
int32	dllmodno;
    for (i=1; i<=ndllproctable; ++i) {
        dllmodno = dllproctable[i-1].dllindex;
        hinst = dllinsttable[dllmodno-1];
        fnaddr = (void (*)	(void))os_getdllprocaddr(hinst,dllproctable[i-1].name);
        if (fnaddr == 0) {
            if (hinst == 0) {
                goto L14;
            }
            printf("%s %s %s %s\n","dllfns: fnaddr=0:",dllproctable[i-1].name,"from",dlltable[dllmodno-1]);
        }
        dllproctable[i-1].address = (void (*)	(void))fnaddr;
L14:;
    }
}

void * os_loaddllfunction	(int32 fnindex) {
int32	dllmodno;
int32	hinst;
void (*fnaddr)	(void);
    dllmodno = dllproctable[fnindex-1].dllindex;
    hinst = dllinsttable[dllmodno-1];
    if (!hinst) {
        hinst = os_loaddllmodule(dlltable[dllmodno-1]);
        if (!hinst) {
        }
        else {
            dllinsttable[dllmodno-1] = hinst;
        }
    }
    fnaddr = (void (*)	(void))os_getdllprocaddr(hinst,dllproctable[fnindex-1].name);
    if (!fnaddr) {
        printf("%s\n",dllproctable[fnindex-1].name);
        pcerror("Can't find DLL function");
        exit(0);
    }
    else {
        dllproctable[fnindex-1].address = (void (*)	(void))fnaddr;
    }
    return fnaddr;
}

void pch_getos	(varrec * result) {
    result->tagx = (tstring|hasrefmask);
    result->objptr = make_str(os_getos(),-1);
}

void pch_gethostsize	(varrec * result) {
    result->tagx = tint;
    result->value = os_gethostsize();
}

void pch_iswindows	(varrec * result) {
    result->tagx = tint;
    result->value = os_iswindows();
}

void os_calldll	(int32 calltype,int32 fnindex,int32 offset,int32 nparams,int32 restype,varrec * dest) {
int32	baserestype;
void (*fnaddr)	(void);
int64	retval;
double	fretval;
int64 (*iparams)[];
    fnaddr = (void (*)	(void))dllproctable[fnindex-1].address;
    if (!fnaddr) {
        fnaddr = (void (*)	(void))os_loaddllfunction(fnindex);
        if (fnaddr) {
            dllproctable[fnindex-1].address = (void (*)	(void))fnaddr;
        }
        else {
            printf("%s\n",dllproctable[fnindex-1].name);
            pcerror("Calldll nil fn:");
        }
    }
    iparams = (int64 (*)[])&dllparams[offset+1-1];
    baserestype = ttbasetype[restype];
    if (32 == 64 && calltype == 'W') {
        calltype = 'C';
    }
    switch (baserestype) {
    case ti32:
    case ti64:
    case tvoid:
    case trefpacked:
    case tintm:
    case twordm:
    case trefm:
    case tu32:
    case tu16:
        if ((calltype=='C')) {
            retval = calldll_cint((void (*)	(void))fnaddr,iparams,nparams);
        } else if ((calltype=='M')) {
            retval = calldll_mint((void (*)	(void))fnaddr,iparams,nparams);
        } else if ((calltype=='W')) {
            retval = os_calldll_wint(fnaddr,iparams,nparams);
        }
        if ((baserestype==tvoid)) {
        } else if ((baserestype==ti64)) {
doi64:
L21:;
            dest->tagx = tint;
            dest->value = retval;
        } else if ((baserestype==ti32)) {
doi32:
L23:;
            dest->tagx = tint;
            dest->value = (int32)retval;
        } else if ((baserestype==trefpacked)) {
            dest->tagx = trefpacked;
            dest->ptr = (byte *)(void *)(int32)retval;
            dest->refelemtag = tttarget[restype];
        } else if ((baserestype==tintm) || (baserestype==twordm) || (baserestype==trefm)) {
            if (32 == 32) {
                goto L23;
            }
            else {
                goto L21;
            }
        } else if ((baserestype==tu32)) {
            dest->tagx = tint;
            dest->value = (uint32)retval;
        } else if ((baserestype==tu16)) {
            dest->tagx = tint;
            dest->value = (uint16)retval;
        }
        else {
            printf("%s\n",ttname[restype]);
            pcerror("xxCALLDLL can't convert rettype");
        }
        break;
    case tr64:
    case tr32:
    case treal:
        if ((calltype=='C')) {
            fretval = calldll_creal((void (*)	(void))fnaddr,(double (*)[])iparams,nparams);
        } else if ((calltype=='M')) {
            fretval = calldll_mreal((void (*)	(void))fnaddr,iparams,nparams);
        } else if ((calltype=='W')) {
            fretval = os_calldll_wreal(fnaddr,iparams,nparams);
        }
        dest->tagx = treal;
        dest->xvalue = fretval;
        break;
    default:;
        printf("%s\n",ttname[baserestype]);
        pcerror("CALLDLL unknown rettype");
    }
}

static int64 calldll_mint	(void (*fnaddr)	(void),int64 (*params)[],int32 nparams) {
    switch (nparams) {
    case 0:
        return ((int32 (*)	(void))fnaddr)();
        break;
    case 1:
        return ((int32 (*)	(int32))fnaddr)((*params)[1-1]);
        break;
    case 2:
        return ((int32 (*)	(int32,int32))fnaddr)((*params)[1-1],(*params)[2-1]);
        break;
    case 3:
        return ((int32 (*)	(int32,int32,int32))fnaddr)((*params)[1-1],(*params)[2-1],(*params)[3-1]);
        break;
    case 4:
        return ((int32 (*)	(int32,int32,int32,int32))fnaddr)((*params)[1-1],(*params)[2-1],(*params)[3-1],(*params)[4-1]);
        break;
    case 5:
        return ((int32 (*)	(int32,int32,int32,int32,int32))fnaddr)((*params)[1-1],(*params)[2-1],(*params)[3-1],(*params)[4-1],(*params)[5-1]);
        break;
    default:;
        pcerror("calldll/b/int too many params");
    }
    return 0;
}

static double calldll_mreal	(void (*fnaddr)	(void),int64 (*params)[],int32 nparams) {
    switch (nparams) {
    case 0:
        return ((double (*)	(void))fnaddr)();
        break;
    case 1:
        return ((double (*)	(int32))fnaddr)((*params)[1-1]);
        break;
    case 2:
        return ((double (*)	(int32,int32))fnaddr)((*params)[1-1],(*params)[2-1]);
        break;
    default:;
        pcerror("calldll/b/real too many params");
    }
    return 0.0;
}

static int64 calldll_cint	(void (*fnaddr)	(void),int64 (*params)[],int32 nparams) {
    switch (nparams) {
    case 0:
        return ((int32 (*)	(void))fnaddr)();
        break;
    case 1:
        return ((int32 (*)	(int32))fnaddr)((*params)[1-1]);
        break;
    case 2:
        return ((int32 (*)	(int32,int32))fnaddr)((*params)[1-1],(*params)[2-1]);
        break;
    case 3:
        return ((int32 (*)	(int32,int32,int32))fnaddr)((*params)[1-1],(*params)[2-1],(*params)[3-1]);
        break;
    case 4:
        return ((int32 (*)	(int32,int32,int32,int32))fnaddr)((*params)[1-1],(*params)[2-1],(*params)[3-1],(*params)[4-1]);
        break;
    case 5:
        return ((int32 (*)	(int32,int32,int32,int32,int32))fnaddr)((*params)[1-1],(*params)[2-1],(*params)[3-1],(*params)[4-1],(*params)[5-1]);
        break;
    case 6:
        return ((int32 (*)	(int32,int32,int32,int32,int32,int32))fnaddr)((*params)[1-1],(*params)[2-1],(*params)[3-1],(*params)[4-1],(*params)[5-1],(*params)[6-1]);
        break;
    case 9:
        return ((int32 (*)	(int32,int32,int32,int32,int32,int32,int32,int32,int32))fnaddr)((*params)[1-1],(*params)[2-1],(*params)[3-1],(*params)[4-1],(*params)[5-1],(*params)[6-1],(*params)[7-1],(*params)[8-1],(*params)[9-1]);
        break;
    case 10:
        return ((int32 (*)	(int32,int32,int32,int32,int32,int32,int32,int32,int32,int32))fnaddr)((*params)[1-1],(*params)[2-1],(*params)[3-1],(*params)[4-1],(*params)[5-1],(*params)[6-1],(*params)[7-1],(*params)[8-1],(*params)[9-1],(*params)[10-1]);
        break;
    case 11:
        return ((int32 (*)	(int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32))fnaddr)((*params)[1-1],(*params)[2-1],(*params)[3-1],(*params)[4-1],(*params)[5-1],(*params)[6-1],(*params)[7-1],(*params)[8-1],(*params)[9-1],(*params)[10-1],(*params)[11-1]);
        break;
    case 12:
        return ((int32 (*)	(int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32))fnaddr)((*params)[1-1],(*params)[2-1],(*params)[3-1],(*params)[4-1],(*params)[5-1],(*params)[6-1],(*params)[7-1],(*params)[8-1],(*params)[9-1],(*params)[10-1],(*params)[11-1],(*params)[12-1]);
        break;
    case 14:
        return ((int32 (*)	(int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32))fnaddr)((*params)[1-1],(*params)[2-1],(*params)[3-1],(*params)[4-1],(*params)[5-1],(*params)[6-1],(*params)[7-1],(*params)[8-1],(*params)[9-1],(*params)[10-1],(*params)[11-1],(*params)[12-1],(*params)[13-1],(*params)[14-1]);
        break;
    default:;
        printf("%d\n",nparams);
        pcerror("calldll/c/int unsupported # of params");
    }
    return 0;
}

static double calldll_creal	(void (*fnaddr)	(void),double (*params)[],int32 nparams) {
    switch (nparams) {
    case 0:
        return ((double (*)	(void))fnaddr)();
        break;
    case 1:
        printf("%s\n","DLL/CREAL/1");
        os_dummycall((*params)[1-1],(*params)[2-1],(*params)[3-1],(*params)[4-1]);
        return ((double (*)	(int32))fnaddr)((*params)[1-1]);
        break;
    case 2:
        return ((double (*)	(int32,int32))fnaddr)((*params)[1-1],(*params)[2-1]);
        break;
    default:;
        pcerror("calldll/c/real too many params");
    }
    return 0.0;
}


// From module: pc_host
void callhostfunction	(int32 hostfn,int32 calledasfn) {
void (*fnaddr)	(void);
int32	nparams;
int32	isfn;
objrec *	p;
int32	av_1;
    fnaddr = (void (*)	(void))hosttable[hostfn-1];
    nparams = hostnparams[hostfn];
    isfn = hostisfn[hostfn];
    if (isfn) {
        switch (nparams) {
        case 0:
            ((void (*)	(varrec *))fnaddr)(sptr);
            break;
        case 1:
            ((void (*)	(varrec *,varrec *))fnaddr)(sptr,sptr+1);
            break;
        case 2:
            ((void (*)	(varrec *,varrec *,varrec *))fnaddr)(sptr,sptr+1,sptr+2);
            break;
        case 3:
            ((void (*)	(varrec *,varrec *,varrec *,varrec *))fnaddr)(sptr,sptr+1,sptr+2,sptr+3);
            break;
        case 4:
            ((void (*)	(varrec *,varrec *,varrec *,varrec *,varrec *))fnaddr)(sptr,sptr+1,sptr+2,sptr+3,sptr+4);
            break;
        default:;
            pcerror("callhost/fn");
        }
    }
    else {
        switch (nparams) {
        case 0:
            ((void (*)	(void))fnaddr)();
            break;
        case 1:
            ((void (*)	(varrec *))fnaddr)(sptr);
            break;
        case 2:
            ((void (*)	(varrec *,varrec *))fnaddr)(sptr,sptr+1);
            break;
        case 3:
            ((void (*)	(varrec *,varrec *,varrec *))fnaddr)(sptr,sptr+1,sptr+2);
            break;
        case 4:
            ((void (*)	(varrec *,varrec *,varrec *,varrec *))fnaddr)(sptr,sptr+1,sptr+2,sptr+3);
            break;
        default:;
            pcerror("callhost/proc");
        }
    }
    av_1 = nparams;
    while (av_1--) {
        if (!!sptr->hasref) {
            vx_ufree(sptr);
        }
        ++sptr;
    }
}

static void pch_leftstr	(varrec * a,varrec * b,varrec * c,varrec * result) {
int32	n;
int32	length;
int32	padchar;
char *	s;
objrec *	pa;
    padchar = ' ';
    if ((c->tag==tvoid)) {
    } else if ((c->tag==tstring)) {
        if (c->objptr->length == 1) {
            padchar = (uchar)*c->objptr->strptr;
        }
        else {
            pcerror("left/padx");
        }
    } else if ((c->tag==tint)) {
        padchar = c->value;
    }
    else {
        pcerror("left/pad?");
    }
    if ((b->tag==tvoid)) {
        n = 1;
    } else if ((b->tag==tint)) {
        n = b->value;
    }
    else {
        pcerror("left:bad n");
    }
    if (a->tag != tstring) {
        pcerror("left:not str");
    }
    pa = a->objptr;
    length = pa->length;
    s = pa->strptr;
    result->tagx = (tstring|hasrefmask);
    if (n == 0) {
doemptystring:
L9:;
        result->objptr = emptystring;
        ++emptystring->refcount;
        return;
    }
    if (n > 0) {
        if (n <= length) {
            leftstring(a,n,result);
        }
        else {
            padstring_right(a,n,padchar,result);
        }
    }
    else {
        n = -n;
        if (n < length) {
            leftstring(a,length-n,result);
        }
        else {
            goto L9;
        }
    }
}

static void pch_rightstr	(varrec * a,varrec * b,varrec * c,varrec * result) {
int32	n;
int32	length;
int32	padchar;
char *	s;
objrec *	pa;
    padchar = ' ';
    if ((c->tag==tvoid)) {
    } else if ((c->tag==tstring)) {
        if (c->objptr->length == 1) {
            padchar = (uchar)*c->objptr->strptr;
        }
        else {
            pcerror("right/padx");
        }
    } else if ((c->tag==tint)) {
        padchar = c->value;
    }
    else {
        pcerror("right/pad?");
    }
    if ((b->tag==tvoid)) {
        n = 1;
    } else if ((b->tag==tint)) {
        n = b->value;
    }
    else {
        pcerror("right:bad n");
    }
    pa = a->objptr;
    if (a->tag != tstring) {
        pcerror("right:not str");
    }
    length = pa->length;
    s = pa->strptr;
    result->tagx = (tstring|hasrefmask);
    if (n == 0) {
doemptystring:
L15:;
        result->objptr = emptystring;
        ++emptystring->refcount;
        return;
    }
    if (n > 0) {
        if (n <= length) {
            rightstring(a,n,result);
        }
        else {
            padstring_left(a,n,padchar,result);
        }
    }
    else {
        n = -n;
        if (n < length) {
            rightstring(a,length-n,result);
        }
        else {
            goto L15;
        }
    }
}

static void pch_convlc	(varrec * a,varrec * b,varrec * result) {
    checkparam(a,tstring,nodefault);
    *result = *a;
    ++result->objptr->refcount;
    if (!!result->hasref) {
        vx_dupl(result);
    }
    vx_iconvcase(result,b,0);
}

static void pch_convuc	(varrec * a,varrec * b,varrec * result) {
    checkparam(a,tstring,nodefault);
    *result = *a;
    ++result->objptr->refcount;
    if (!!result->hasref) {
        vx_dupl(result);
    }
    vx_iconvcase(result,b,1);
}

static void pch_iconvlc	(varrec * a,varrec * b) {
    checkparam(a,trefvar,nodefault);
    vx_iconvcase(a->varptr,b,0);
}

static void pch_iconvuc	(varrec * a,varrec * b) {
    checkparam(a,trefvar,nodefault);
    vx_iconvcase(a->varptr,b,1);
}

static void pch_stop	(void) {
    pcerror("host_stop not impl");
}

static void pch_stopx	(varrec * a) {
    pcerror("host_stopx not impl");
}

static void pch_ismain	(varrec * a,varrec * result) {
    checkparam(a,tstring,nodefault);
    result->tagx = tint;
    result->value = (int64)(cmpstring(a->objptr->strptr,moduletable[nmodules].name,a->objptr->length,strlen(moduletable[nmodules].name)) == 0);
}

static void pch_waitkey	(varrec * result) {
    result->tagx = tint;
    result->value = os_getch();
}

static void pch_testkey	(varrec * result) {
    result->tagx = tint;
    result->value = os_kbhit();
}

static void pch_execwait	(varrec * a,varrec * b,varrec * c,varrec * result) {
char *	workdir;
int32	flag;
objrec *	pa;
    checkparam(a,tstring,nodefault);
    pa = a->objptr;
    flag = checkparam(b,tint,0);
    if (c->tag == tvoid) {
        workdir = 0;
    }
    else {
        checkparam(c,tstring,nodefault);
        workdir = convCstring(c->objptr->strptr,c->objptr->length);
    }
    result->tagx = tint;
    result->value = os_execwait(convCstring(pa->strptr,pa->length),flag,workdir);
}

static void pch_execcmd	(varrec * a,varrec * b,varrec * c,varrec * result) {
char *	workdir;
    checkparam(a,tstring,nodefault);
    checkparam(b,tint,0);
    if (c->tag == tvoid) {
        workdir = 0;
    }
    else {
        checkparam(c,tstring,nodefault);
        workdir = convCstring(c->objptr->strptr,c->objptr->length);
    }
    result->tagx = tint;
    result->value = os_execcmd(convCstring(a->objptr->strptr,a->objptr->length),b->value);
}

static void pch_makestr	(varrec * a,varrec * b,varrec * result) {
int32	n;
objrec *	s;
    switch (a->tag) {
    case trefpacked:
        break;
    case tint:
        break;
    default:;
        pcerror("makestr");
    }
    n = getintvalue(b);
    result->tagx = (tstring|hasrefmask);
    s = make_strslicexobj((char *)a->ptr,n);
    result->objptr = s;
}

static void pch_makestrslice	(varrec * a,varrec * b,varrec * result) {
    pcerror("MAKESTRSLICE");
}

static void pch_makeref	(varrec * a,varrec * b,varrec * result) {
byte *	ptr;
    switch (ttbasetype[a->tag]) {
    case trefvar:
    case trefpacked:
    case tint:
        ptr = a->ptr;
        break;
    case tstring:
    case tarray:
    case tlist:
    case tset:
        ptr = a->objptr->ptr;
        break;
    default:;
        pcerror("makeref");
    }
    result->tagx = trefpacked;
    result->ptr = ptr;
    result->refelemtag = getintvalue(b);
}

static void pch_new	(varrec * a,varrec * b,varrec * c,varrec * d,varrec * result) {
varrec	v;
int32	i;
int32	t;
int32	nbytes;
int32	ival;
int32	nwords;
int32	nbits;
int32	offset;
int32	elemtype;
int32	n;
dimrec	dims;
varrec *	qvar;
int64 *	qint;
byte *	qbyte;
byte *	ptr;
objrec *	p;
int32	av_2;
int32	av_3;
int32	av_4;
int32	av_5;
    t = getintvalue(a);
    if (t < 0 || t > ntypes) {
        pcustypet("New:bad type",t);
    }
    v.tagx = (t|hasrefmask);
    switch (ttbasetype[t]) {
    case tlist:
        getbounds(b,&dims,1);
        p = make_listobj(dims.length,dims.lbound);
        v.objptr = p;
        if (!!dims.length) {
            if (c && c->tag != tvoid) {
                qvar = p->vptr;
                av_2 = dims.length;
                while (av_2--) {
                    *qvar = *c;
                    if (!!qvar->hasref) {
                        vx_ushare(qvar);
                    }
                    ++qvar;
                }
            }
        }
        break;
    case tarray:
        if (!!ttlength[t]) {
            elemtype = tttarget[t];
            dims.length = ttlength[t];
            dims.lbound = ttlower[t];
            dims.upper = dims.length+dims.lbound-1;
            d = b;
            goto L19;
        }
        elemtype = getintvalue(b);
        getbounds(c,&dims,1);
        if (elemtype >= tbit && elemtype <= tbit4) {
            v.tagx = (t = tbits);
            goto L20;
        }
doarray2:
L19:;
        p = make_arrayobj(dims.length,elemtype,dims.lbound);
        v.objptr = p;
        if (!!dims.length) {
            if (d && d->tag != tvoid) {
                qbyte = p->ptr;
                av_3 = dims.length;
                while (av_3--) {
                    vx_storepacked(qbyte,d,elemtype);
                    qbyte += ttsize[elemtype];
                }
            }
        }
        break;
    case tbits:
        if (!!ttlength[t]) {
            elemtype = tttarget[t];
            dims.length = ttlength[t];
            dims.lbound = ttlower[t];
            dims.upper = dims.length+dims.lbound-1;
            d = b;
            goto L20;
        }
        elemtype = getintvalue(b);
        if (elemtype < tbit || elemtype > tbit4) {
            pcerror("new: bad bits elem");
        }
        getbounds(c,&dims,1);
dobits2:
L20:;
        p = make_bitsobj(dims.length,elemtype,dims.lbound);
        v.objptr = p;
        if (!!dims.length) {
            if (d && d->tag != tvoid) {
                qbyte = p->ptr;
                offset = 0;
                av_4 = dims.length;
                while (av_4--) {
                    vx_storebit(qbyte,offset,d,elemtype);
                    offset += ttbitwidth[elemtype];
                    if (offset >= 8) {
                        offset = 0;
                        ++qbyte;
                    }
                }
            }
        }
        break;
    case tset:
        getbounds(b,&dims,0);
        if (dims.lbound < 0) {
            pcerror("new:set:lwb");
        }
        if (dims.lbound != 0) {
            dims.lbound = 0;
            dims.length = dims.upper+1;
        }
        p = make_setobj(dims.length);
        v.objptr = p;
        break;
    case trecord:
        p = make_listobj(ttlength[t],1);
        v.objptr = p;
        if (b && b->tag != tvoid) {
            qvar = p->vptr;
            av_5 = p->length;
            while (av_5--) {
                *qvar = *b;
                if (!!qvar->hasref) {
                    vx_ushare(qvar);
                }
                ++qvar;
            }
        }
        break;
    case tstruct:
        p = newobject();
        p->ptr = make_arraydata(ttsize[t],tu8,&(p)->allocated,1);
        p->lower = 1;
        p->mutable = 1;
        p->length = ttlength[t];
        v.objptr = p;
        if (b && b->tag != tvoid) {
            pcerror("New: struct init");
        }
        break;
    case tint:
    case tword:
    case treal:
    case trefproc:
    case trefvar:
        v.value = 0;
        if (b && b->tag != tvoid) {
            pcerror("NEW(int/value)");
        }
        break;
    case tdict:
        getbounds(b,&dims,1);
        n = nextpoweroftwo(dims.length);
        p = make_listobj(m_imax(16,n*2),1);
        p->dictitems = 0;
        v.objptr = p;
        break;
    default:;
        pcustypet("new",t);
    }
finish:
    *result = v;
}

static void pch_newheap	(varrec * a,varrec * b,varrec * c,varrec * d,varrec * result) {
varrec *	p;
    p = (varrec *)pcm_alloc(varsize);
    pch_new(a,b,c,d,p);
    result->tagx = trefvar;
    result->varptr = p;
}

static void pch_heapvar	(varrec * a,varrec * result) {
varrec *	p;
    pcerror("HEAPVAR");
}

static void pch_freeheap	(varrec * a) {
    pcerror("FREEHEAP");
}

static void pch_getcmdparam	(varrec * a,varrec * result) {
int32	n;
char *	s;
    if (a->tag == noparamtag) {
        result->tagx = tint;
        result->value = (ncmdparams+1);
        return;
    }
    n = getintvalue(a);
    result->tagx = (tstring|hasrefmask);
    result->objptr = make_str(cmdparamtable[n],-1);
}

static void pch_setpcerror	(varrec * a) {
objrec *	pa;
    checkparam(a,tstring,nodefault);
    pa = a->objptr;
    if (pcerror_mess) {
        free(pcerror_mess);
        pcerror_mess = 0;
    }
    if (!!pa->length) {
        pcerror_mess = (char *)malloc(pa->length+1);
        memcpy((int32 *)pcerror_mess,(int32 *)pa->strptr,pa->length);
        *(pcerror_mess+pa->length) = 0;
    }
}

static void pch_setdebug	(varrec * a) {
    checkparam(a,tint,nodefault);
    printf("%s\n","SETDEBUG.................");
    fdebug = a->value;
}

static void pch_setfprintf	(varrec * a,varrec * b) {
    checkparam(a,trefdllproc,nodefault);
    checkparam(b,trefdllproc,nodefault);
    fprintf_ptr = (void (*)	(void))a->ptr;
    fgets_ptr = (void (*)	(void))b->ptr;
}

static void pch_ticks	(varrec * result) {
    result->tagx = tint;
    result->value = os_clock();
}

static void pch_sleep	(varrec * a) {
    checkparam(a,tint,nodefault);
    os_sleep(a->value);
}

static void pch_random	(varrec * a,varrec * result) {
int32	n;
int32	x;
int32	r;
    r = rand()<<15|rand();
    result->tagx = tint;
    if (a->tag == trange) {
        x = r% ((a->range_upper-a->range_lower)+1)+a->range_lower;
    }
    else {
        checkparam(a,tint,nodefault);
        n = a->value;
        if (n > 1) {
            x = r% n;
        }
        else if (n == 0) {
            x = r;
        }
        else if (n == 1) {
            result->tagx = treal;
            result->xvalue = (double)r/ 1073741824.0;
            return;
        }
        else {
            srand(-n);
            x = 0;
        }
    }
    result->value = x;
}

static void pch_findmetafunction	(varrec * a,varrec * result) {
int32	i;
char *	sdata;
objrec *	pa;
procrec *	pp;
strec *	d;
int32	av_6;
    checkparam(a,tstring,nodefault);
    pa = a->objptr;
    result->tagx = trefproc;
    result->value = 0;
    if (!!pa->length) {
        sdata = convCstring(pa->strptr,pa->length);
    }
    else {
        return;
    }
    if (runfrompc) {
        d = &(*pcsymboltable)[1-1];
        av_6 = nsymbols;
        while (av_6--) {
            if (d->nameid == procid) {
                if (!!d->metadata && strstr(d->metadata,sdata) != 0) {
                    result->ptr = (byte *)d->address;
                    return;
                }
            }
            ++d;
        }
    }
    else {
        pp = proclist;
        while (pp) {
            d = pp->def;
            if (!!d->metadata && strstr(d->metadata,sdata) != 0) {
                result->ptr = (byte *)d->address;
                return;
            }
            pp = pp->nextproc;
        }
    }
}

static void pch_loadpcl	(varrec * a,varrec * b,varrec * result) {
    pcerror("host_loadpcl not impl");
}

static void pch_runpcl	(varrec * a,varrec * b,varrec * result) {
    pcerror("host_runpcl not impl");
}

static void pch_runtask	(varrec * a,varrec * b,varrec * result) {
    pcerror("host_runtask not impl");
}

static void pch_callext	(varrec * a,varrec * b,varrec * c) {
    pcerror("host_callext not impl");
}

static void pch_system	(varrec * a,varrec * result) {
    checkparam(a,tstring,nodefault);
    result->tagx = tint;
    result->value = system(convCstring(a->objptr->strptr,a->objptr->length));
}

static void pch_shellexec	(varrec * a,varrec * b,varrec * result) {
objrec *	pa;
objrec *	pb;
    checkparam(a,tstring,nodefault);
    checkparam(b,tstring,nodefault);
    pa = a->objptr;
    pb = b->objptr;
    result->tagx = tint;
    result->value = os_shellexec(convCstring(pa->strptr,pa->length),convCstring(pb->strptr,pb->length));
}

static void pch_gethash	(varrec * a,varrec * result) {
    result->tagx = tint;
    result->value = gethashvalue(a);
}

static void pch_test	(varrec * a,varrec * b,varrec * result) {
int32	i;
    printf("%s","TEST:");
    PPP = a->ptr;
    for (i=0; i<=20; ++i) {
        printf("%u%s",*(a->ptr+i)," ");
    }
    printf("\n");
    result->tagx = tint;
    result->value = 0;
}

static void pch_pcldata	(varrec * a,varrec * b,varrec * result) {
int32	res;
int32	length;
int32	i;
strec *	d;
int32	av_7;
    checkparam(a,tint,nodefault);
    res = 0;
    if ((a->value==6073471649054675536ll)) {
        if (proclist == 0 && runfrompc) {
            d = &(*pcsymboltable)[1-1];
            av_7 = nsymbols;
            while (av_7--) {
                if (d->nameid == procid) {
                    addtoproclist(d);
                }
                ++d;
            }
        }
        proclistptr = proclist;
    } else if ((a->value==1129271888)) {
        if (proclistptr) {
            getproctabledata(proclistptr,result);
            proclistptr = proclistptr->nextproc;
            return;
        }
    } else if ((a->value==76194150371149ll)) {
        printf("%s\n","MODULE");
    } else if ((a->value==19506716274347331ll)) {
        result->tagx = (tstring|hasrefmask);
        result->objptr = make_str(cmdnames[b->value],-1);
        return;
    } else if ((a->value==19496880614690115ll)) {
        for (i=1; i<=klastcmd; ++i) {
            length = strlen(cmdnames[i]);
            if (b->objptr->length == length && memcmp(b->objptr->strptr,cmdnames[i],length) == 0) {
                res = i;
                goto L56;
            }
        }
L56:;
    } else if ((a->value==91552836767566ll)) {
        res = cmdnopnds[b->value];
    }
    else {
        pcerror("pcldata/bad table");
    }
    result->tagx = tint;
    result->value = res;
}

static void pch_getcstring	(varrec * a,varrec * result) {
    pcerror("PCH/GETCSTRING");
}

static void pch_getparam	(varrec * a,varrec * result) {
    checkparam(a,tint,nodefault);
    *result = *(varrec *)(frameptr+(a->value*varsize));
    if (!!result->hasref) {
        ++result->objptr->refcount;
    }
}

static void pch_clearlist	(varrec * a) {
int32	n;
    pcerror("PCH CLEARLIST");
}

static void pch_makelink	(varrec * a,varrec * result) {
    if ((ttbasetype[a->tag]==trecord)) {
        result->tagx = trecordlink;
        result->refelemtag = a->tag;
        result->varptr = (varrec *)a->objptr;
    } else if ((ttbasetype[a->tag]==tint)) {
        if (!!a->value) {
            pcerror("makelink/int");
        }
        result->tagx = tint;
        result->value = 0;
    }
    else {
        pcerror("makelink: not record/list");
    }
}

static void pch_allparams	(varrec * a,varrec * result) {
objrec *	p;
int32	nparams;
int32	isfn;
int32	i;
varrec *	q;
int64 *	fnptr;
    checkparam(a,trefproc,nodefault);
    fnptr = (int64 *)a->ptr;
    nparams = *(fnptr-1);
    p = newobject();
    p->objtype = extslice_obj;
    p->length = nparams;
    p->lower = 1;
    p->vptr = (varrec *)frameptr+1;
    result->tagx = (tlist|hasrefmask);
    result->objptr = p;
}

static void pch_stackvars	(varrec * result) {
    pcerror("STACKVARS");
}

static void pch_makeempty	(varrec * a,varrec * result) {
objrec *	p;
int32	t;
    t = ttbasetype[a->tag];
    if (t == ttype) {
        t = a->value;
    }
    if ((t==tlist)) {
        p = emptylist;
        ++p->refcount;
    } else if ((t==tstring)) {
        p = emptystring;
        ++p->refcount;
    } else if ((t==tarray)) {
        p = newobject();
        p->lower = 1;
        p->elemtag = a->objptr->elemtag;
    }
    else {
        pcustypet("makeempty?",t);
    }
    result->tagx = (t|hasrefmask);
    result->objptr = p;
}

static void pch_readlines	(varrec * a,varrec * result) {
byte *	p;
byte *	q;
byte *	pstart;
varrec	v;
varrec *	r;
objrec *	l;
int32	nlines;
int32	n;
    checkparam(a,tstring,nodefault);
    if (a->objptr->length == 0) {
error:
L63:;
        result->tagx = tint;
        result->value = 0;
        return;
    }
    p = readfile(a->objptr->strptr);
    if (p == 0) {
        goto L63;
    }
    q = p;
    nlines = 0;
L64:;
    switch (*q++) {
    case 26:
        goto L65;
        break;
    case 13:
        ++nlines;
        if (*q == 10) {
            ++q;
        }
        break;
    case 10:
        ++nlines;
        break;
    default:;
    }
    goto L64;
L65:;
    v.tagx = (tlist|hasrefmask);
    v.objptr = make_listobj(nlines,1);
    r = v.objptr->vptr;
    q = p;
    pstart = q;
L66:;
    switch (*q++) {
    case 26:
        goto L67;
        break;
    case 13:
        n = (q-pstart)-1;
        if (*q == 10) {
            ++q;
        }
addline:
L68:;
        r->tagx = (tstring|hasrefmask);
        r->objptr = make_str((char *)pstart,n);
        ++r;
        pstart = q;
        break;
    case 10:
        n = (q-pstart)-1;
        goto L68;
        break;
    default:;
    }
    goto L66;
L67:;
    *result = v;
    free(p);
}

static void pch_dictitems	(varrec * a,varrec * result) {
    if (!!a->hasref) {
        result->tagx = tint;
        result->value = a->objptr->dictitems;
    }
    else {
        pcerror(".alloclen/not heap");
    }
}

static void pch_setoverload	(varrec * a,varrec * b,varrec * c) {
    pcerror("SETOVERLOAD");
}

static void pch_errorinfo	(varrec * a,varrec * result) {
    checkparam(a,tint,nodefault);
    result->tagx = tint;
    result->value = (int32)err_pcptr;
}

static void getbounds	(varrec * p,dimrec * dims,int32 lower) {
int32	n;
    if (!p) {
        pcerror("New: no bounds");
    }
    switch (p->tag) {
    case noparamtag:
        dims->lbound = lower;
        dims->upper = 0;
        dims->length = 0;
        break;
    case trange:
        dims->lbound = p->range_lower;
        dims->upper = p->range_upper;
        dims->length = (p->range_upper-p->range_lower)+1;
        if (dims->length < 0) {
            dims->length = 0;
            dims->upper = dims->lbound-1;
        }
        break;
    default:;
        n = getintvalue(p);
        dims->lbound = lower;
        dims->upper = dims->length = n;
    }
}

static int64 checkparam	(varrec * p,int32 tag,int32 defaultx) {
    if ((p->tag==tvoid)) {
        if (defaultx == nodefault) {
            pcerror("Missing host param");
        }
        return defaultx;
    } else if ((p->tag==tag)) {
        return p->value;
    }
    if (tag == tint) {
        if ((p->tag==treal)) {
            return p->xvalue;
        }
    }
    printf("%s\n",ttname[p->tag]);
    pcerror("Host param wrong type");
    return 0;
}

static void leftstring	(varrec * a,int32 n,varrec * result) {
objrec *	p;
    result->tagx = (tstring|hasrefmask);
    p = make_str(a->objptr->strptr,n);
    result->objptr = p;
}

static void rightstring	(varrec * a,int32 n,varrec * result) {
objrec *	p;
    result->tagx = (tstring|hasrefmask);
    p = make_str(a->objptr->strptr+(a->objptr->length-n),n);
    result->objptr = p;
}

static void padstring_right	(varrec * a,int32 n,int32 fillchar,varrec * result) {
char *	s;
int32	length;
objrec *	p;
int32	av_8;
    length = a->objptr->length;
    result->tagx = (tstring|hasrefmask);
    p = make_stringobj(n);
    result->objptr = p;
    s = p->strptr;
    if (length) {
        memcpy((int32 *)s,(int32 *)a->objptr->strptr,length);
        s += length;
    }
    av_8 = n-length;
    while (av_8--) {
        *s = fillchar;
        ++s;
    }
}

static void padstring_left	(varrec * a,int32 n,int32 fillchar,varrec * result) {
char *	s;
int32	length;
int32	padlen;
objrec *	pa;
int32	av_9;
    s = (char *)pcm_alloc(n);
    pa = a->objptr;
    length = a->objptr->length;
    padlen = n-length;
    pa = make_stringobj(n);
    result->objptr = pa;
    s = pa->strptr;
    s += padlen;
    if (length) {
        memcpy((int32 *)s,(int32 *)a->objptr->strptr,length);
    }
    av_9 = padlen;
    while (av_9--) {
        --s;
        *s = fillchar;
    }
}

static void pcld_makevstr	(varrec * p,char * s) {
    p->tagx = (tstring|hasrefmask);
    p->objptr = make_str(s,-1);
}

static void pcld_makevint	(varrec * p,int64 a) {
    p->tagx = tint;
    p->value = a;
}

static void pcld_makelist	(varrec * p,varrec * result,int32 n) {
varrec *	q;
objrec *	r;
int32	av_10;
    result->tagx = (tlist|hasrefmask);
    r = make_listobj(n,1);
    result->objptr = r;
    q = r->vptr;
    av_10 = n;
    while (av_10--) {
        *q = *p;
        vx_dupl(q);
        ++q;
        ++p;
    }
}

static void getproctabledata	(procrec * p,varrec * result) {
varrec	table[4];
varrec	l;
strec *	d;
int32	moduleno;
    d = p->def;
    pcld_makevstr(&table[1-1],d->name);
    pcld_makevstr(&table[2-1],d->metadata);
    moduleno = d->attribs.ax_moduleno;
    pcld_makevint(&table[3-1],moduleno);
    table[4-1].tagx = trefproc;
    table[4-1].ptr = (byte *)d->pcaddress;
    pcld_makelist((varrec *)&table,result,4);
}

static int64 * convert_handler	(int32 _1) {
    return 0;
}

static void addtoproclist	(strec * d) {
procrec *	pp;
    ++nproclist;
    pp = (procrec *)pcm_alloc(8);
    pp->nextproc = proclist;
    proclist = pp;
    pp->def = d;
}


// From module: pc_handlers
void * k_zero	(void) {
    pclunimpl(kzero);
    return (pcptr+1);
}

void * k_nop	(void) {
    return (pcptr+1);
}

void * k_procstart	(void) {
    pclunimpl(kprocstart);
    return (pcptr+3);
}

void * k_procend	(void) {
    pclunimpl(kprocend);
    return (pcptr+1);
}

void * k_endmodule	(void) {
    pclunimpl(kendmodule);
    return (pcptr+1);
}

void * k_push_m	(void) {
    *--sptr = *(varrec *)(int32)*(pcptr+1);
    if (!!sptr->hasref) {
        ++sptr->objptr->refcount;
    }
    return (pcptr+2);
}

void * k_push_f	(void) {
    *--sptr = *(varrec *)(frameptr+*(pcptr+1));
    if (!!sptr->hasref) {
        ++sptr->objptr->refcount;
    }
    return (pcptr+2);
}

void * k_push_am	(void) {
    (--sptr)->tagx = trefvar;
    sptr->varptr = (varrec *)(int32)*(pcptr+1);
    return (pcptr+2);
}

void * k_push_af	(void) {
    (--sptr)->tagx = trefvar;
    sptr->varptr = (varrec *)(frameptr+*(pcptr+1));
    return (pcptr+2);
}

void * k_push_ap	(void) {
    (--sptr)->tagx = trefproc;
    sptr->ptr = (byte *)(int32)*(pcptr+1);
    return (pcptr+2);
}

void * k_push_al	(void) {
    (--sptr)->tagx = treflabel;
    sptr->ptr = (byte *)(int32)*(pcptr+1);
    return (pcptr+2);
}

void * k_push_ci	(void) {
    (--sptr)->tagx = tint;
    sptr->value = *(pcptr+1);
    return (pcptr+2);
}

void * k_push_cw	(void) {
    (--sptr)->tagx = tword;
    sptr->uvalue = *(pcptr+1);
    return (pcptr+2);
}

void * k_push_cr	(void) {
    (--sptr)->tagx = treal;
    sptr->uvalue = *(pcptr+1);
    return (pcptr+2);
}

void * k_push_cn	(void) {
void (*f)	(void);
    (--sptr)->tagx = trange;
    sptr->uvalue = *(pcptr+1);
    return (pcptr+2);
}

void * k_push_cs	(void) {
objrec *	p;
    (--sptr)->tagx = (tstring|hasrefmask);
    sptr->objptr = (objrec *)(int32)*(pcptr+1);
    ++sptr->objptr->refcount;
    return (pcptr+2);
}

void * k_push_t	(void) {
    (--sptr)->tagx = ttype;
    sptr->value = *(pcptr+1);
    return (pcptr+2);
}

void * k_push_op	(void) {
    (--sptr)->tagx = toperator;
    sptr->value = *(pcptr+1);
    sptr->opdims = *(pcptr+2);
    return (pcptr+3);
}

void * k_pushz	(void) {
    (--sptr)->tagx = *(pcptr+1);
    sptr->value = 0;
    return (pcptr+2);
}

void * k_pushz_void	(void) {
    (--sptr)->tagx = tvoid;
    return (pcptr+1);
}

void * k_pushz_str	(void) {
void (*f)	(void);
    (--sptr)->tagx = (tstring|hasrefmask);
    sptr->ptr = (byte *)emptystring;
    ++emptystring->refcount;
    return (pcptr+1);
}

void * k_pushz_list	(void) {
void (*f)	(void);
    (--sptr)->tagx = (tlist|hasrefmask);
    sptr->ptr = (byte *)emptylist;
    ++emptylist->refcount;
    return (pcptr+1);
}

void * k_pushz_listl	(void) {
objrec *	p;
    --sptr;
    sptr->tagx = (tlist|hasrefmask);
    sptr->objptr = make_listobj(0,*(pcptr+1));
    return (pcptr+2);
}

void * k_pushz_set	(void) {
    (--sptr)->tagx = (tset|hasrefmask);
    sptr->ptr = (byte *)emptyset;
    ++emptyset->refcount;
    return (pcptr+1);
}

void * k_pushz_arrayl	(void) {
    pclunimpl(kpushz_arrayl);
    return (pcptr+3);
}

void * k_pop_m	(void) {
varrec *	a;
    a = (varrec *)(int32)*(pcptr+1);
    if (!!a->hasref) {
        vx_ufree(a);
    }
    *a = *sptr++;
    return (pcptr+2);
}

void * k_pop_f	(void) {
varrec *	a;
    a = (varrec *)(frameptr+*(pcptr+1));
    if (!!a->hasref) {
        vx_ufree(a);
    }
    *a = *sptr++;
    return (pcptr+2);
}

void * k_store_m	(void) {
varrec *	a;
    a = (varrec *)(int32)*(pcptr+1);
    if (!!sptr->hasref) {
        vx_ushare(sptr);
    }
    if (!!a->hasref) {
        vx_ufree(a);
    }
    *a = *sptr;
    return (pcptr+2);
}

void * k_store_f	(void) {
varrec *	a;
    a = (varrec *)(frameptr+*(pcptr+1));
    if (!!sptr->hasref) {
        vx_ushare(sptr);
    }
    if (!!a->hasref) {
        vx_ufree(a);
    }
    *a = *sptr;
    return (pcptr+2);
}

void * k_pushptr	(void) {
    switch (sptr->tag) {
    case trefvar:
        *sptr = *sptr->varptr;
        if (!!sptr->hasref) {
            ++sptr->objptr->refcount;
        }
        break;
    case trecordlink:
        sptr->tagx = (sptr->refelemtag|hasrefmask);
        ++sptr->objptr->refcount;
        break;
    case trefpacked:
        vx_loadpacked(sptr->ptr,sptr->refelemtag,sptr,0);
        break;
    case trefbit:
        vx_loadbit(sptr->ptr,sptr->refbitoffset,sptr->refelemtag,sptr);
        break;
    default:;
        pcustype("pushptr",sptr);
    }
    return (pcptr+1);
}

void * k_popptr	(void) {
varrec *	p;
varrec *	q;
    p = sptr++;
    switch (p->tag) {
    case trefvar:
        q = p->varptr;
        if (!!q->hasref) {
            vx_ufree(q);
        }
        *q = *sptr++;
        break;
    case tstring:
        vx_storestring(p,sptr);
        ++sptr;
        break;
    default:;
        vx_storeptr(p,sptr);
        ++sptr;
    }
    return (pcptr+1);
}

void * k_storeptr	(void) {
varrec *	p;
varrec *	q;
    p = sptr++;
    if (p->tag == trefvar) {
        q = p->varptr;
        if (!!sptr->hasref) {
            vx_ushare(sptr);
        }
        if (!!q->hasref) {
            vx_ufree(q);
        }
        *q = *sptr;
    }
    else {
        vx_storeptr(p,sptr);
        vx_cfree(sptr);
    }
    return (pcptr+1);
}

void * k_zpop_m	(void) {
varrec *	a;
    a = (varrec *)(int32)*(pcptr+1);
    *a = *sptr++;
    return (pcptr+2);
}

void * k_zpop_f	(void) {
varrec *	a;
    a = (varrec *)(frameptr+*(pcptr+1));
    *a = *sptr++;
    return (pcptr+2);
}

void * k_zstore_m	(void) {
    pclunimpl(kzstore_m);
    return (pcptr+2);
}

void * k_zstore_f	(void) {
varrec *	a;
    a = (varrec *)(frameptr+*(pcptr+1));
    *a = *sptr;
    if (!!a->hasref) {
        vx_ushare(a);
    }
    if (!!sptr->hasref) {
        ++sptr->objptr->refcount;
    }
    return (pcptr+2);
}

void * k_copy	(void) {
    if (!sptr->hasref) {
        return (pcptr+1);
    }
    vx_dupl(sptr);
    return (pcptr+1);
}

void * k_swap	(void) {
varrec *	x;
varrec *	y;
varrec	v;
int32	xt;
int32	yt;
byte *	p;
byte *	q;
int32	a;
    x = sptr++;
    y = sptr++;
    xt = x->tag;
    yt = y->tag;
    if (xt == trefvar && yt == trefvar) {
        v = *x->varptr;
        *x->varptr = *y->varptr;
        *y->varptr = v;
    }
    else if (xt == trefpacked && yt == trefpacked) {
        if (x->refelemtag == y->refelemtag && y->refelemtag == tu8) {
            p = x->ptr;
            q = y->ptr;
            a = *p;
            *p = *q;
            *q = a;
        }
        else {
            goto L4;
        }
    }
    else {
swaperror:
L4:;
        pcmxtypes("SWAP",x,y);
    }
    return (pcptr+1);
}

void * k_convptr	(void) {
varrec *	a;
int32	tag;
int32	elemtype;
void *	p;
objrec *	pa;
    switch (sptr->tag) {
    case trefvar:
        a = sptr->varptr;
        pa = a->objptr;
        switch (ttbasetype[a->tag]) {
        case tint:
            p = &(a)->value;
            elemtype = ti64;
            break;
        case treal:
            p = &(a)->value;
            elemtype = tr64;
            break;
        case tarray:
            p = pa->ptr;
            elemtype = a->objptr->elemtag;
            break;
        case tstring:
            p = pa->strptr;
            elemtype = tu8;
            if (p == 0) {
                p = "";
            }
            break;
        case tstruct:
            p = pa->ptr;
            elemtype = a->tag;
            break;
        default:;
            printf("%s\n",gettypename(ttbasetype[a->tag]));
            pcustype("Getrefpack1",a);
            return pcptr;
        }
        break;
    case trefpacked:
    case trefbit:
        return (pcptr+1);
        break;
    default:;
        pcustype("Getrefpack2",sptr);
        return pcptr;
    }
    sptr->tagx = trefpacked;
    sptr->ptr = (byte *)p;
    sptr->refelemtag = elemtype;
    return (pcptr+1);
}

void * k_jump	(void) {
    return (int32 *)(int32)*(pcptr+1);
}

void * k_jumpptr	(void) {
    if (sptr->tag != treflabel) {
        pcerror("Bad label ptr");
    }
    pcptr = (int64 *)(int32 *)sptr->ptr;
    ++sptr;
    return pcptr;
}

void * k_jumptrue	(void) {
varrec *	x;
int32	xt;
    x = sptr++;
retry:
    xt = ttbasetype[x->tag];
    switch (xt) {
    case tint:
    case tword:
    case treal:
    case trefvar:
    case trefproc:
    case treflabel:
    case trefpacked:
    case trefbit:
        if (!!x->value) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    case tlist:
    case tarray:
    case tstring:
    case tset:
    case tbits:
        if (!!x->objptr->length) {
            vx_ufree(x);
            return (int32 *)(int32)*(pcptr+1);
        }
        vx_ufree(x);
        break;
    case trecord:
    case tstruct:
    case trecordlink:
        return (int32 *)(int32)*(pcptr+1);
        break;
    default:;
        pcustypet("JUMPTRUE",xt);
    }
    return (pcptr+2);
}

void * k_jumpfalse	(void) {
varrec *	x;
int32	xt;
    x = sptr++;
retry:
    xt = ttbasetype[x->tag];
    switch (xt) {
    case tint:
    case tword:
    case trefvar:
    case trefproc:
    case treflabel:
    case trefpacked:
    case trefbit:
        if (!x->value) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    case tlist:
    case tarray:
    case tstring:
    case tset:
    case tbits:
        if (!x->objptr->length) {
            vx_ufree(x);
            return (int32 *)(int32)*(pcptr+1);
        }
        vx_ufree(x);
        break;
    case trecord:
    case tstruct:
    case trecordlink:
        break;
    default:;
        pcustype("JUMPFALSE",x);
    }
    return (pcptr+2);
}

void * k_jumpdef	(void) {
    if (sptr->tag != tvoid) {
        if (!!sptr->hasref) {
            vx_ufree(sptr);
        }
        ++sptr;
        return (int64 *)(int32)*(pcptr+1);
    }
    if (!!sptr->hasref) {
        vx_ufree(sptr);
    }
    ++sptr;
    return (pcptr+2);
}

void * k_jumpvoid	(void) {
    if (sptr->tag == tvoid) {
        if (!!sptr->hasref) {
            vx_ufree(sptr);
        }
        ++sptr;
        return (int64 *)(int32)*(pcptr+1);
    }
    if (!!sptr->hasref) {
        vx_ufree(sptr);
    }
    ++sptr;
    return (pcptr+2);
}

void * k_jumpeq	(void) {
varrec *	x;
varrec *	y;
int32	xt;
int32	yt;
int32	res;
    y = sptr++;
    x = sptr++;
    xt = x->tag;
    if (y->tag != xt) {
        if (!!(xt = vx_mixed(x,y))) {
            goto L7;
        }
        xt = x->tag;
        yt = y->tag;
        switch (xt) {
        case trefpacked:
        case trefvar:
            if ((yt==tint)) {
                if (x->value == y->value) {
                    return (int32 *)(int32)*(pcptr+1);
                }
                return (pcptr+2);
            }
            break;
        default:;
        }
        if (!!vx_equal(x,y,0)) {
            return (int64 *)(int32)*(pcptr+1);
        }
        return (pcptr+2);
    }
retry:
L7:;
    switch (xt) {
    case tint:
    case treal:
    case tword:
        if (x->value == y->value) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    case tstring:
        if (!!vx_eqstring(x,y)) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    default:;
        if (!!vx_equal(x,y,0)) {
            return (int32 *)(int32)*(pcptr+1);
        }
    }
    return (pcptr+2);
}

void * k_jumpne	(void) {
varrec *	x;
varrec *	y;
int32	xt;
int32	yt;
int32	res;
    y = sptr++;
    x = sptr++;
    xt = x->tag;
    if (y->tag != xt) {
        if (!!(xt = vx_mixed(x,y))) {
            goto L9;
        }
        xt = x->tag;
        yt = y->tag;
        switch (xt) {
        case trefpacked:
        case trefvar:
            if ((yt==tint)) {
                if (x->value != y->value) {
                    return (int32 *)(int32)*(pcptr+1);
                }
                return (pcptr+2);
            }
            break;
        default:;
        }
        if (!vx_equal(x,y,0)) {
            return (int64 *)(int32)*(pcptr+1);
        }
        return (pcptr+2);
    }
retry:
L9:;
    switch (xt) {
    case tint:
    case treal:
    case tword:
        if (x->value != y->value) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    case tstring:
        if (vx_eqstring(x,y) == 0) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    default:;
        if (!vx_equal(x,y,0)) {
            return (int32 *)(int32)*(pcptr+1);
        }
    }
    return (pcptr+2);
}

void * k_jumplt	(void) {
varrec *	x;
varrec *	y;
int32	xt;
int32	res;
    y = sptr++;
    x = sptr++;
    xt = x->tag;
    if (xt != y->tag) {
        if (!!(xt = vx_mixed(x,y))) {
            goto L11;
        }
        pcmxtypes("LT/MIXED",x,y);
    }
retry:
L11:;
    switch (xt) {
    case tint:
        if (x->value < y->value) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    case treal:
        if (x->xvalue < y->xvalue) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    case tword:
        if (x->uvalue < y->uvalue) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    default:;
        if (vx_compare(x,y) < 0) {
            return (int32 *)(int32)*(pcptr+1);
        }
    }
    return (pcptr+2);
}

void * k_jumple	(void) {
varrec *	x;
varrec *	y;
int32	xt;
int32	res;
    y = sptr++;
    x = sptr++;
    xt = x->tag;
    if (xt != y->tag) {
        if (!!(xt = vx_mixed(x,y))) {
            goto L12;
        }
        pcmxtypes("LE/MIXED",x,y);
    }
retry:
L12:;
    switch (xt) {
    case tint:
        if (x->value <= y->value) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    case treal:
        if (x->xvalue <= y->xvalue) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    case tword:
        if (x->uvalue <= y->uvalue) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    default:;
        if (vx_compare(x,y) <= 0) {
            return (int32 *)(int32)*(pcptr+1);
        }
    }
    return (pcptr+2);
}

void * k_jumpge	(void) {
varrec *	x;
varrec *	y;
int32	xt;
int32	res;
    y = sptr++;
    x = sptr++;
    xt = x->tag;
    if (xt != y->tag) {
        if (!!(xt = vx_mixed(x,y))) {
            goto L13;
        }
        pcmxtypes("GE/MIXED",x,y);
    }
retry:
L13:;
    switch (xt) {
    case tint:
        if (x->value >= y->value) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    case treal:
        if (x->xvalue >= y->xvalue) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    case tword:
        if (x->uvalue >= y->uvalue) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    default:;
        if (vx_compare(x,y) >= 0) {
            return (int32 *)(int32)*(pcptr+1);
        }
    }
    return (pcptr+2);
}

void * k_jumpgt	(void) {
varrec *	x;
varrec *	y;
int32	xt;
int32	res;
    y = sptr++;
    x = sptr++;
    xt = x->tag;
    if (xt != y->tag) {
        if (!!(xt = vx_mixed(x,y))) {
            goto L14;
        }
        pcmxtypes("GT/MIXED",x,y);
    }
retry:
L14:;
    switch (xt) {
    case tint:
        if (x->value > y->value) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    case treal:
        if (x->xvalue > y->xvalue) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    case tword:
        if (x->uvalue > y->uvalue) {
            return (int32 *)(int32)*(pcptr+1);
        }
        break;
    default:;
        if (vx_compare(x,y) > 0) {
            return (int32 *)(int32)*(pcptr+1);
        }
    }
    return (pcptr+2);
}

void * k_jumptesteq	(void) {
varrec *	x;
varrec *	y;
int32	xt;
int32	yt;
int32	res;
objrec *	py;
    y = sptr++;
    x = sptr;
    xt = x->tag;
    yt = y->tag;
    if (xt != yt) {
        if (!!(xt = vx_mixed(x,y))) {
            goto L15;
        }
        xt = x->tag;
        yt = y->tag;
        switch (xt) {
        case tint:
            switch (yt) {
            case trange:
                if (x->value < y->range_lower || x->value > y->range_upper) {
                    return (pcptr+2);
                }
                break;
            case tset:
                py = y->objptr;
                if (py->length == 0 || x->value >= py->length || !testelem((byte (*)[])py->ptr,x->value)) {
                    return (pcptr+2);
                }
                break;
            default:;
                return (pcptr+2);
            }
            break;
        default:;
            return (pcptr+2);
        }
        ++sptr;
        return (int64 *)(int32)*(pcptr+1);
    }
retry:
L15:;
    switch (xt) {
    case tint:
    case ttype:
        if (x->value == y->value) {
            ++sptr;
            return (int64 *)(int32)*(pcptr+1);
        }
        break;
    case treal:
        if (x->xvalue == y->xvalue) {
            ++sptr;
            return (int64 *)(int32)*(pcptr+1);
        }
        break;
    case trange:
        if (x->value == y->value) {
            ++sptr;
            return (int64 *)(int32)*(pcptr+1);
        }
        break;
    case tstring:
        if (!!vx_eqstring_nf(x,y)) {
            vx_ufree(y);
            vx_ufree(x);
            ++sptr;
            return (int64 *)(int32)*(pcptr+1);
        }
        vx_ufree(y);
        break;
    default:;
        if (!!vx_equal_nf(x,y,0)) {
            if (!!y->hasref) {
                vx_ufree(y);
            }
            if (!!x->hasref) {
                vx_ufree(x);
            }
            ++sptr;
            return (int64 *)(int32)*(pcptr+1);
        }
        if (!!y->hasref) {
            vx_ufree(y);
        }
    }
    return (pcptr+2);
}

void * k_jumptestne	(void) {
varrec *	x;
varrec *	y;
int32	xt;
int32	yt;
int32	res;
objrec *	px;
objrec *	py;
    y = sptr++;
    x = sptr;
    xt = x->tag;
    yt = y->tag;
    if (xt != yt) {
        if (!!(xt = vx_mixed(x,y))) {
            goto L16;
        }
        xt = x->tag;
        yt = y->tag;
        switch (xt) {
        case tint:
            switch (yt) {
            case trange:
                if (x->value >= y->range_lower && x->value <= y->range_upper) {
                    ++sptr;
                    return (pcptr+2);
                }
                break;
            case tset:
                py = y->objptr;
                if (x->value < py->length && !!testelem((byte (*)[])py->ptr,x->value)) {
                    ++sptr;
                    return (pcptr+2);
                }
                break;
            default:;
            }
            break;
        default:;
        }
        return (int64 *)(int32)*(pcptr+1);
    }
retry:
L16:;
    switch (xt) {
    case tint:
    case ttype:
        if (x->value != y->value) {
            return (int64 *)(int32)*(pcptr+1);
        }
        ++sptr;
        break;
    case treal:
        if (x->xvalue != y->xvalue) {
            return (int64 *)(int32)*(pcptr+1);
        }
        ++sptr;
        break;
    case trange:
        if (x->value != y->value) {
            return (int64 *)(int32)*(pcptr+1);
        }
        ++sptr;
        break;
    case tstring:
        if (!vx_eqstring_nf(x,y)) {
            vx_ufree(y);
            return (int64 *)(int32)*(pcptr+1);
        }
        vx_ufree(y);
        vx_ufree(x);
        ++sptr;
        break;
    default:;
        if (!vx_equal_nf(x,y,0)) {
            if (!!y->hasref) {
                vx_ufree(y);
            }
            return (int64 *)(int32)*(pcptr+1);
        }
        if (!!y->hasref) {
            vx_ufree(y);
        }
        if (!!x->hasref) {
            vx_ufree(x);
        }
        ++sptr;
    }
    return (pcptr+2);
}

void * k_jumplabel	(void) {
    pclunimpl(kjumplabel);
    return (pcptr+2);
}

void * k_jumpclabel	(void) {
    pclunimpl(kjumpclabel);
    return (pcptr+3);
}

void * k_switch	(void) {
int32	index;
int32	n;
int32	lower;
    n = *(pcptr+1);
    lower = *(pcptr+2);
    if ((sptr->tag==tint) || (sptr->tag==ttype)) {
    }
    else {
        printf("%s\n",ttname[sptr->tag]);
        pcerror("switch not int");
    }
    index = ((sptr++)->value-lower);
    if ((uint32)index >= (uint32)n) {
        return (int32 *)(int32)*(pcptr+n*2+4);
    }
    else {
        return (int32 *)(int32)*(pcptr+index*2+4);
    }
}

void * k_cswitch	(void) {
    pclunimpl(kcswitch);
    return (pcptr+4);
}

void * k_new	(void) {
    pclunimpl(knew);
    return (pcptr+1);
}

void * k_to_f	(void) {
    if (!!--((varrec *)(frameptr+*(pcptr+2)))->value) {
        return (int64 *)(int32)*(pcptr+1);
    }
    else {
        return (pcptr+3);
    }
}

void * k_for_fci	(void) {
    if (++((varrec *)(frameptr+*(pcptr+2)))->value <= *(pcptr+3)) {
        return (int64 *)(int32)*(pcptr+1);
    }
    else {
        return (pcptr+4);
    }
}

void * k_for_ff	(void) {
    if (++((varrec *)(frameptr+*(pcptr+2)))->value <= ((varrec *)(frameptr+*(pcptr+3)))->value) {
        return (int32 *)(int32)*(pcptr+1);
    }
    else {
        return (pcptr+4);
    }
}

void * k_ford_fci	(void) {
    if (--((varrec *)(frameptr+*(pcptr+2)))->value >= *(pcptr+3)) {
        return (int64 *)(int32)*(pcptr+1);
    }
    else {
        return (pcptr+4);
    }
}

void * k_ford_ff	(void) {
    if (--((varrec *)(frameptr+*(pcptr+2)))->value >= ((varrec *)(frameptr+*(pcptr+3)))->value) {
        return (int64 *)(int32)*(pcptr+1);
    }
    else {
        return (pcptr+4);
    }
}

void * k_call	(void) {
enum {countinterval = 10};
static int32	count=countinterval;
int32	ticks;
byte	m[100];
    (--sptr)->tagx = tretaddr;
    sptr->retaddr = (int32 *)(pcptr+3);
    sptr->frameptr_low = *(int32*)&frameptr;
    sptr->stackadj = *(pcptr+2);
    frameptr = (byte *)sptr;
    return (int64 *)(int32)*(pcptr+1);
}

void * k_callptr	(void) {
int64 *	newpc;
    if (sptr->tag != trefproc) {
        printf("%s\n",ttname[sptr->tag]);
        pcerror("callptr: not refproc");
    }
    newpc = (int64 *)(int32)sptr->value;
    if (*(pcptr+1) != *(newpc-1)) {
        printf("%lld %lld\n",*(pcptr+1),*(newpc-1));
        pcerror("callptr wrong # params");
    }
    sptr->tagx = tretaddr;
    sptr->retaddr = (int32 *)(pcptr+3);
    sptr->frameptr_low = *(int32*)&frameptr;
    sptr->stackadj = *(pcptr+2);
    frameptr = (byte *)sptr;
    return newpc;
}

void * k_return	(void) {
    if (sptr->tag != tretaddr) {
        printf("%s\n",ttname[sptr->tag]);
        pcerror("Return error");
    }
    sptr->tag = 0;
    pcptr = (int64 *)sptr->retaddr;
    *(int32 *)&frameptr = sptr->frameptr_low;
    sptr = (varrec *)((byte *)sptr+sptr->stackadj);
    ++sptr;
    return pcptr;
}

void * k_startdll	(void) {
    if (++dllcallindex > maxdllindex) {
        pcerror("nested dll max");
    }
    dllcallstack[dllcallindex-1] = dllindex;
    return (pcptr+1);
}

void * k_pushdll	(void) {
int32	s;
int32	t;
objrec *	p;
    if (++dllindex > 29) {
        printf("%s %d\n","DLLIX %d",dllindex);
        pcerror("dll params");
    }
    s = sptr->tag;
    t = ttbasetype[*(pcptr+1)];
    switch (t) {
    case tvoid:
        t = s;
        switch (s) {
        case tstring:
            dllparams[dllindex-1] = (int64)(int32)convCstring(sptr->objptr->strptr,sptr->objptr->length);
            break;
        default:;
            dllparams[dllindex-1] = sptr->value;
        }
        break;
    case ti64:
    case tu64:
        if (32 == 32) {
            pcerror("pushdll i64=>i32,i32 needed");
        }
        if ((s==tint) || (s==tword)) {
        } else if ((s==18)) {
            sptr->value = sptr->xvalue;
        } else if ((s==trefpacked)) {
            sptr->value = (int64)(int32)sptr->ptr;
        }
        else {
error:
L23:;
            printf("%s %s %s\n",ttname[s],"should be",ttname[t]);
            pcerror("DLL: param wrong type");
        }
        dllparams[dllindex-1] = sptr->value;
        break;
    case tr64:
        if (32 == 32) {
            pcerror("pushdll r64=>i32,i32 needed");
        }
        if ((s==tint) || (s==tword)) {
            sptr->xvalue = sptr->value;
        } else if ((s==treal)) {
        }
        else {
            goto L23;
        }
        break;
    case tstring:
        if ((s==tstring)) {
            p = sptr->objptr;
            dllparams[dllindex-1] = (int64)(int32)convCstring(p->strptr,p->length);
        } else if ((s==tint)) {
            if (sptr->value != 0) {
                goto L23;
            }
            dllparams[dllindex-1] = 0;
        }
        else {
            goto L23;
        }
        break;
    case trefpacked:
        if (!!sptr->hasref) {
            dllparams[dllindex-1] = (int64)(int32)sptr->objptr->ptr;
        }
        else {
            dllparams[dllindex-1] = (int64)(int32)sptr->ptr;
        }
        break;
    default:;
        dllparams[dllindex-1] = sptr->value;
    }
    if (sptr->tag == tstring) {
        if (!!sptr->hasref) {
            vx_ufree(sptr);
        }
    }
    ++sptr;
    return (pcptr+2);
}

void * k_calldll	(void) {
int32	nparams;
int32	offset;
    if (dllcallindex <= 0) {
        pcerror("calldll??");
    }
    offset = dllcallstack[dllcallindex-1];
    nparams = dllindex-offset;
    os_calldll(*(pcptr+2),*(pcptr+1),offset,nparams,*(pcptr+3),sptr);
    dllindex = dllcallstack[dllcallindex-1];
    --dllcallindex;
    return (pcptr+4);
}

void * k_callhost	(void) {
    callhostfunction(*(pcptr+1),0);
    return (pcptr+2);
}

void * k_stackframe	(void) {
int32	i;
int32	n;
int32	av_1;
    n = *(pcptr+1);
    av_1 = n;
    while (av_1--) {
        (--sptr)->tagx = tvoid;
        sptr->value = 0;
    }
    return (pcptr+2);
}

void * k_free	(void) {
int32	i;
int32	n;
objrec *	p;
int32	av_2;
    n = *(pcptr+1);
    av_2 = n;
    while (av_2--) {
        if (!!sptr->hasref) {
            vx_ufree(sptr);
        }
        ++sptr;
    }
    return (pcptr+2);
}

void * k_addsp	(void) {
    sptr = (varrec *)((byte *)sptr+*(pcptr+1));
    return (pcptr+2);
}

void * k_stop	(void) {
int32	i;
int32	j;
int32	k;
int32	n;
int32	c;
int32	total;
int32	exitcode;
double	SPEED;
    stopped = 1;
    return (pcptr+1);
}

void * k_test	(void) {
    pclunimpl(ktest);
    return (pcptr+2);
}

void * k_makelist	(void) {
int32	n;
int32	lower;
    n = *(pcptr+1);
    lower = *(pcptr+2);
    vx_makelist(n,sptr,sptr+n-1,lower);
    sptr += n-1;
    return (pcptr+3);
}

void * k_makerecord	(void) {
int32	n;
int32	t;
    n = *(pcptr+1);
    t = *(pcptr+2);
    vx_makerecord(n,t,sptr,sptr+n-1);
    sptr += n-1;
    return (pcptr+3);
}

void * k_makearray	(void) {
int32	n;
int32	lower;
int32	t;
    n = *(pcptr+1);
    lower = *(pcptr+2);
    t = *(pcptr+3);
    if (ttbasetype[t] == tarray) {
        vx_makearray(n,t,*(pcptr+4),lower,sptr,sptr+n-1);
    }
    else {
        vx_makebits(n,t,*(pcptr+4),lower,sptr,sptr+n-1);
    }
    sptr += n-1;
    return (pcptr+5);
}

void * k_makestruct	(void) {
int32	n;
int32	t;
    n = *(pcptr+1);
    t = *(pcptr+2);
    vx_makestruct(n,t,sptr,sptr+n-1);
    sptr += n-1;
    return (pcptr+3);
}

void * k_makeset	(void) {
int32	n;
    n = *(pcptr+1);
    vx_makeset(n,sptr,sptr+n-1);
    sptr += n-1;
    return (pcptr+2);
}

void * k_makerange	(void) {
varrec *	x;
varrec *	y;
    y = sptr++;
    vx_makerange(sptr,y,sptr);
    return (pcptr+1);
}

void * k_makedict	(void) {
int32	n;
    n = *(pcptr+1);
    vx_makedict(n,sptr,sptr+n*2-1);
    sptr += n*2-1;
    return (pcptr+2);
}

void * k_pushdot	(void) {
varrec *	x;
varrec *	p;
int32	index;
int32	j;
int32	k;
int32	n;
int32	fieldtype;
int32	needfree;
byte *	xptr;
int32	i;
genfielddatarec *	gd;
varrec	v;
int32	av_3;
    v = *sptr;
    if ((v.tag==trecordlink)) {
        v.tagx = v.refelemtag;
    }
    else {
        if (!v.hasref) {
            pcerror("pushdot/not record");
        }
    }
    xptr = (byte *)v.objptr->vptr;
    gd = &genfielddata[genfieldnames[*(pcptr+1)-1].dataindex-1];
    n = genfieldnames[*(pcptr+1)-1].datalength;
    av_3 = n;
    while (av_3--) {
        if (gd->recordtype == v.tag) {
            fieldtype = gd->fieldtype;
            if (fieldtype == tvariant) {
                *sptr = *(varrec *)(xptr+gd->offset);
                if (!!sptr->hasref) {
                    ++sptr->objptr->refcount;
                }
                if (!!v.hasref) {
                    vx_ufree(&v);
                }
                return (pcptr+2);
            }
            else if (fieldtype == trefproc) {
                pcerror("PUSHDOT/METHOD");
            }
            else {
                vx_loadpacked((xptr+gd->offset),fieldtype,sptr,0);
                if (!!v.hasref) {
                    vx_ufree(&v);
                }
                return (pcptr+2);
            }
        }
        ++gd;
    }
    printf("%s %d %s\n","Field:",ngenfieldnames,genfieldnames[*(pcptr+1)-1].name);
    pcustypet("Dotg: wrong record type",v.tag);
    return pcptr;
}

void * k_pushdotref	(void) {
varrec	v;
varrec *	p;
int32	index;
int32	n;
int32	xtag;
int32	fieldtype;
int32	isrefvar;
int32	rectype;
int32	offset;
genfielddatarec *	gd;
int32	av_4;
    v = *sptr;
    if ((v.tag==trefvar)) {
        p = v.varptr;
        isrefvar = 1;
        rectype = p->tag;
        if (rectype == trecordlink) {
            rectype = p->refelemtag;
        }
        if (!p->objptr->mutable) {
            p->objptr = copyonwrite(p->objptr,p->tag);
        }
    } else if ((v.tag==trefpacked)) {
        isrefvar = 0;
        rectype = v.refelemtag;
    }
    else {
        pcustype("&dotg not ref",&v);
        return pcptr;
    }
    gd = &genfielddata[genfieldnames[*(pcptr+1)-1].dataindex-1];
    n = genfieldnames[*(pcptr+1)-1].datalength;
    av_4 = n;
    while (av_4--) {
        if (gd->recordtype == rectype) {
            fieldtype = gd->fieldtype;
            offset = gd->offset;
            if (isrefvar) {
                if (fieldtype == tvariant) {
                    sptr->ptr = p->objptr->ptr+offset;
                }
                else {
                    sptr->tagx = trefpacked;
                    sptr->ptr = p->objptr->ptr+offset;
                    sptr->refelemtag = fieldtype;
                }
            }
            else {
                sptr->ptr += offset;
                sptr->refelemtag = fieldtype;
            }
            return (pcptr+2);
        }
        ++gd;
    }
    pcustypet("&Dotg: wrong record type",rectype);
    return (pcptr+2);
}

void * k_softconv	(void) {
int32	t;
    t = *(pcptr+1);
    sptr->tagx = t;
    return (pcptr+2);
}

void * k_hardconv	(void) {
int32	s;
int32	t;
    s = sptr->tag;
    t = *(pcptr+1);
    if (sptr->tag != t) {
        vx_iconvert(t,sptr);
    }
    return (pcptr+2);
}

void * k_mixed	(void) {
    pclunimpl(kmixed);
    return (pcptr+1);
}

void * k_incrptr	(void) {
varrec *	p;
varrec	v;
    p = sptr++;
    switch (ttbasetype[p->tag]) {
    case trefvar:
        p = p->varptr;
        switch (p->tag) {
        case tint:
            ++p->value;
            break;
        case trefvar:
            ++p->varptr;
            break;
        case trefpacked:
            p->ptr += ttsize[p->refelemtag];
            break;
        default:;
            pcustype("incrptr/refvar",p);
        }
        break;
    case trefpacked:
        switch (p->refelemtag) {
        case tu8:
        case ti8:
            ++*p->ptr;
            break;
        default:;
            pcustypet("incrptr/ref",p->refelemtag);
        }
        break;
    default:;
        pcustype("incrptr",p);
    }
    return (pcptr+1);
}

void * k_incrto_m	(void) {
varrec *	a;
    a = (varrec *)(int32)*(pcptr+1);
    switch (ttbasetype[a->tag]) {
    case tint:
        ++a->value;
        break;
    case trefvar:
        ++a->varptr;
        break;
    case trefpacked:
        a->ptr += ttsize[a->refelemtag];
        break;
    default:;
        pcustype("INCRTO_M",a);
    }
    return (pcptr+2);
}

void * k_incrto_f	(void) {
varrec *	a;
    a = (varrec *)(frameptr+*(pcptr+1));
    switch (a->tag) {
    case tint:
        ++a->value;
        return (pcptr+2);
        break;
    case trefvar:
        ++a->varptr;
        break;
    case trefpacked:
        a->ptr += ttsize[a->refelemtag];
        break;
    default:;
        pcustype("INCRTO_F",a);
    }
    return (pcptr+2);
}

void * k_loadincr	(void) {
varrec	ptr;
    ptr = *sptr;
    vx_loadptr(sptr,sptr);
    *--sptr = ptr;
    return k_incrptr();
}

void * k_incrload	(void) {
int32 *	pc;
varrec	ptr;
    ptr = *sptr;
    pc = (int32 *)k_incrptr();
    vx_loadptr(&ptr,--sptr);
    return pc;
}

void * k_decrptr	(void) {
varrec *	p;
varrec	v;
    p = sptr++;
    switch (ttbasetype[p->tag]) {
    case trefvar:
        p = p->varptr;
        switch (p->tag) {
        case tint:
            --p->value;
            break;
        case trefvar:
            --p->varptr;
            break;
        case trefpacked:
            p->ptr -= ttsize[p->refelemtag];
            break;
        default:;
            pcustype("decrptr/refvar",p);
        }
        break;
    case trefpacked:
        switch (p->refelemtag) {
        case ti32:
            --*p->dptr;
            break;
        case tu8:
        case ti8:
            --*p->ptr;
            break;
        default:;
            pcustypet("decrptr/ref",p->refelemtag);
        }
        break;
    default:;
        pcustype("decrptr",p);
    }
    return (pcptr+1);
}

void * k_decrto_m	(void) {
varrec *	a;
    a = (varrec *)(int32)*(pcptr+1);
    switch (a->tag) {
    case tint:
        --a->value;
        break;
    case trefvar:
        --a->varptr;
        break;
    case trefpacked:
        a->ptr -= ttsize[a->refelemtag];
        break;
    default:;
        pcustype("DECRTO_M",a);
    }
    return (pcptr+2);
}

void * k_decrto_f	(void) {
varrec *	a;
    a = (varrec *)(frameptr+*(pcptr+1));
    switch (a->tag) {
    case tint:
        --a->value;
        break;
    case trefvar:
        --a->varptr;
        break;
    case trefpacked:
        a->ptr -= ttsize[a->refelemtag];
        break;
    default:;
        pcustype("DECRTO_F",a);
    }
    return (pcptr+2);
}

void * k_loaddecr	(void) {
varrec	ptr;
    ptr = *sptr;
    vx_loadptr(sptr,sptr);
    *--sptr = ptr;
    return k_decrptr();
}

void * k_decrload	(void) {
int32 *	pc;
varrec	ptr;
    ptr = *sptr;
    pc = (int32 *)k_decrptr();
    vx_loadptr(&ptr,--sptr);
    return pc;
}

void * k_incr	(void) {
    pclunimpl(kincr);
    return (pcptr+1);
}

void * k_decr	(void) {
    if ((sptr->tag==tint)) {
        --sptr->value;
    }
    else {
        pcustype("decr",sptr);
    }
    return (pcptr+1);
}

void * k_neg	(void) {
    switch (sptr->tag) {
    case tint:
    case tword:
        sptr->value = -sptr->value;
        break;
    case treal:
        sptr->xvalue = -sptr->xvalue;
        break;
    default:;
        pcustype("Neg",sptr);
    }
    return (pcptr+1);
}

void * k_abs	(void) {
    switch (sptr->tag) {
    case tint:
        sptr->value = abs(sptr->value);
        break;
    case treal:
        sptr->xvalue = fabs(sptr->xvalue);
        break;
    default:;
        pcustype("Abs",sptr);
    }
    return (pcptr+1);
}

void * k_not	(void) {
    sptr->value = (int64)!sptr->value;
    return (pcptr+1);
}

void * k_inot	(void) {
    switch (sptr->tag) {
    case tint:
    case tword:
        sptr->value = ~sptr->value;
        break;
    default:;
        pcustype("Inot",sptr);
    }
    return (pcptr+1);
}

void * k_istrue	(void) {
int32	res;
varrec *	x;
    x = sptr;
    switch (ttbasetype[x->tag]) {
    case tint:
        sptr->tagx = tint;
        sptr->value = (int64)!!x->value;
        break;
    case treal:
        sptr->tagx = tint;
        sptr->value = ((x->xvalue != 0.0)?1:0);
        break;
    case tset:
    case tstring:
    case tlist:
    case tarray:
    case tbits:
        res = (int32)(x->objptr->length != 0);
        vx_cfree(sptr);
        sptr->tagx = tint;
        sptr->value = res;
        break;
    case tstruct:
    case trecord:
    case trecordlink:
        vx_cfree(sptr);
        sptr->tagx = tint;
        sptr->value = 1;
        break;
    default:;
        pcustype("Istrue",x);
    }
    return (pcptr+1);
}

void * k_asc	(void) {
int32	a;
objrec *	s;
    switch (sptr->tag) {
    case tstring:
        s = sptr->objptr;
        if (s->length == 0) {
            a = 0;
        }
        else {
            a = (uchar)*s->strptr;
            if (!!sptr->hasref) {
                vx_ufree(sptr);
            }
        }
        sptr->tagx = tint;
        sptr->value = a;
        break;
    default:;
        pcustype("ASC",sptr);
    }
    return (pcptr+1);
}

void * k_chr	(void) {
    switch (sptr->tag) {
    case tint:
        if (sptr->uvalue > 255) {
            pcerror("chr>255");
        }
        sptr->tagx = (tstring|hasrefmask);
        sptr->objptr = make_char(sptr->value);
        break;
    default:;
        pcustype("CHR",sptr);
    }
    return (pcptr+1);
}

void * k_sqrt	(void) {
    pcptr += 1;
    switch (sptr->tag) {
    case tint:
        sptr->tagx = treal;
        sptr->xvalue = sqrt(sptr->value);
        break;
    case treal:
        sptr->xvalue = sqrt(sptr->xvalue);
        break;
    default:;
        pcustype("SQRT",sptr);
    }
    return pcptr;
}

void * k_sqr	(void) {
varrec *	x;
    x = sptr;
    switch (x->tag) {
    case tint:
        sptr->value = x->value*x->value;
        break;
    case treal:
        sptr->xvalue = x->xvalue*x->xvalue;
        break;
    default:;
        pcustype("SQR",x);
    }
    return (pcptr+1);
}

void * k_cube	(void) {
    pclunimpl(kcube);
    return (pcptr+1);
}

void * k_sin	(void) {
varrec *	x;
    x = sptr;
    switch (x->tag) {
    case treal:
billy:
        sptr->xvalue = sin(x->xvalue);
        break;
    default:;
        pcustype("SIN",x);
    }
    return (pcptr+1);
}

void * k_cos	(void) {
    pclunimpl(kcos);
    return (pcptr+1);
}

void * k_tan	(void) {
    pclunimpl(ktan);
    return (pcptr+1);
}

void * k_asin	(void) {
    pclunimpl(kasin);
    return (pcptr+1);
}

void * k_acos	(void) {
    pclunimpl(kacos);
    return (pcptr+1);
}

void * k_atan	(void) {
    pclunimpl(katan);
    return (pcptr+1);
}

void * k_sign	(void) {
    pclunimpl(ksign);
    return (pcptr+1);
}

void * k_ln	(void) {
    pclunimpl(kln);
    return (pcptr+1);
}

void * k_log	(void) {
    pclunimpl(klog);
    return (pcptr+1);
}

void * k_lg	(void) {
    pclunimpl(klg);
    return (pcptr+1);
}

void * k_exp	(void) {
    pclunimpl(kexp);
    return (pcptr+1);
}

void * k_round	(void) {
    vx_round(sptr,sptr);
    return (pcptr+1);
}

void * k_floor	(void) {
varrec *	x;
    x = sptr;
    switch (x->tag) {
    case treal:
        if (x->xvalue >= 0.0) {
            sptr->xvalue = floor(x->xvalue+0.5);
        }
        else {
            sptr->xvalue = ceil(x->xvalue-0.5);
        }
        break;
    default:;
        pcustype("ROUND",x);
    }
    return (pcptr+1);
}

void * k_ceil	(void) {
    pclunimpl(kceil);
    return (pcptr+1);
}

void * k_fract	(void) {
    pclunimpl(kfract);
    return (pcptr+1);
}

void * k_negto	(void) {
    pclunimpl(knegto);
    return (pcptr+1);
}

void * k_absto	(void) {
    pclunimpl(kabsto);
    return (pcptr+1);
}

void * k_notto	(void) {
    pclunimpl(knotto);
    return (pcptr+1);
}

void * k_inotto	(void) {
    pclunimpl(kinotto);
    return (pcptr+1);
}

void * k_len	(void) {
int32	n;
objrec *	p;
    switch (ttbasetype[sptr->tag]) {
    case tlist:
    case tstring:
    case trecord:
    case tarray:
    case tstruct:
        p = sptr->objptr;
        n = p->length;
        vx_ufree(sptr);
        break;
    case trange:
        n = (sptr->range_upper-sptr->range_lower)+1;
        break;
    default:;
        pcustype("LEN",sptr);
    }
    sptr->tagx = tint;
    sptr->value = n;
    return (pcptr+1);
}

void * k_lwb	(void) {
int32	xt;
int32	n;
objrec *	p;
    switch (ttbasetype[sptr->tag]) {
    case tlist:
    case tstring:
    case tarray:
        p = sptr->objptr;
        n = p->lower;
        vx_ufree(sptr);
        break;
    case trange:
        n = sptr->range_lower;
        break;
    default:;
        pcustype("LWB",sptr);
    }
    sptr->tagx = tint;
    sptr->value = n;
    return (pcptr+1);
}

void * k_upb	(void) {
int32	n;
objrec *	p;
    switch (ttbasetype[sptr->tag]) {
    case tlist:
    case tarray:
    case trecord:
        p = sptr->objptr;
        n = p->length+p->lower-1;
        vx_ufree(sptr);
        break;
    case tstring:
        p = sptr->objptr;
        n = p->length;
        vx_ufree(sptr);
        break;
    case trange:
        n = sptr->range_upper;
        break;
    default:;
        pcustype("UPB",sptr);
    }
    sptr->tagx = tint;
    sptr->value = n;
    return (pcptr+1);
}

void * k_bounds	(void) {
varrec *	p;
objrec *	r;
int32	a;
int32	b;
    p = sptr;
retry:
    switch (ttbasetype[p->tag]) {
    case tlist:
    case tarray:
    case tstring:
    case tbits:
    case trecord:
    case tstruct:
    case tset:
        r = p->objptr;
        a = r->lower;
        b = r->length+a-1;
        if (!!sptr->hasref) {
            vx_ufree(sptr);
        }
        sptr->tagx = trange;
        sptr->range_lower = a;
        sptr->range_upper = b;
        break;
    case tint:
        sptr->tagx = trange;
        sptr->range_lower = 0;
        sptr->range_upper = 63;
        break;
    case trange:
        break;
    default:;
        pcustype("BOUNDS",p);
    }
    return (pcptr+1);
}

void * k_bits	(void) {
    if ((sptr->tag==ttype)) {
        sptr->value = ttbitwidth[sptr->value];
    }
    else {
        sptr->value = ttbitwidth[sptr->tag];
    }
    sptr->tagx = tint;
    return (pcptr+1);
}

void * k_bytes	(void) {
    if ((sptr->tag==ttype)) {
        sptr->value = ttsize[sptr->value];
    }
    else {
        sptr->value = ttsize[sptr->tag];
    }
    sptr->tagx = tint;
    return (pcptr+1);
}

void * k_type	(void) {
int32	res;
    res = sptr->tag;
    if (!!sptr->hasref) {
        vx_ufree(sptr);
    }
    sptr->tagx = ttype;
    sptr->value = res;
    return (pcptr+1);
}

void * k_elemtype	(void) {
    pclunimpl(kelemtype);
    return (pcptr+1);
}

void * k_basetype	(void) {
int32	res;
    if (sptr->tag == ttype) {
        res = ttbasetype[sptr->value];
    }
    else {
        res = ttbasetype[sptr->tag];
    }
    if (!!sptr->hasref) {
        vx_ufree(sptr);
    }
    sptr->tagx = ttype;
    sptr->value = res;
    return (pcptr+1);
}

void * k_minval	(void) {
    pclunimpl(kminval);
    return (pcptr+1);
}

void * k_maxval	(void) {
int32	t;
int64	a;
    switch (sptr->tag) {
    case tint:
        t = ti64;
        break;
    case treal:
        t = tr64;
        break;
    case ttype:
        t = sptr->value;
        break;
    default:;
        pcustype("Maxval",sptr);
    }
    if ((t==tu8)) {
        a = 255;
    } else if ((t==tu16)) {
        a = 65536;
    }
    else {
        printf("%s\n",gettypename(t));
        pcerror("MAXVALUE");
    }
    sptr->tagx = tint;
    sptr->value = a;
    return (pcptr+1);
}

void * k_isint	(void) {
    if (sptr->tag == tint || sptr->tag == tword) {
        sptr->value = 1;
    }
    else {
        if (!!sptr->hasref) {
            vx_ufree(sptr);
        }
        sptr->value = 0;
    }
    sptr->tagx = tint;
    return (pcptr+1);
}

void * k_isreal	(void) {
    if (sptr->tag == treal) {
        sptr->value = 1;
    }
    else {
        if (!!sptr->hasref) {
            vx_ufree(sptr);
        }
        sptr->value = 0;
    }
    sptr->tagx = tint;
    return (pcptr+1);
}

void * k_isstring	(void) {
int32	n;
    n = (int32)(sptr->tag == tstring);
    if (!!sptr->hasref) {
        vx_ufree(sptr);
    }
    sptr->tagx = tint;
    sptr->value = n;
    return (pcptr+1);
}

void * k_isrange	(void) {
int32	n;
    n = (int32)(sptr->tag == trange);
    if (!!sptr->hasref) {
        vx_ufree(sptr);
    }
    sptr->tagx = tint;
    sptr->value = n;
    return (pcptr+1);
}

void * k_isnumber	(void) {
    pclunimpl(kisnumber);
    return (pcptr+1);
}

void * k_isarray	(void) {
int32	n;
    switch (ttbasetype[sptr->tag]) {
    case tlist:
    case tarray:
    case tbits:
        n = 1;
        break;
    default:;
        n = 0;
    }
    if (!!sptr->hasref) {
        vx_ufree(sptr);
    }
    sptr->tagx = tint;
    sptr->value = n;
    return (pcptr+1);
}

void * k_isrecord	(void) {
int32	n;
    n = 0;
    switch (ttbasetype[sptr->tag]) {
    case trecord:
    case tstruct:
        n = 1;
        break;
    default:;
    }
    if (!!sptr->hasref) {
        vx_ufree(sptr);
    }
    sptr->tagx = tint;
    sptr->value = n;
    return (pcptr+1);
}

void * k_ispointer	(void) {
int32	n;
    switch (ttbasetype[sptr->tag]) {
    case trefpacked:
    case trefvar:
    case trefbit:
    case trefproc:
    case treflabel:
        n = 1;
        break;
    default:;
        n = 0;
    }
    sptr->tagx = tint;
    sptr->value = n;
    return (pcptr+1);
}

void * k_ismutable	(void) {
    pclunimpl(kismutable);
    return (pcptr+1);
}

void * k_isset	(void) {
int32	n;
    n = (int32)(ttbasetype[sptr->tag] == tset);
    if (!!sptr->hasref) {
        vx_ufree(sptr);
    }
    sptr->tagx = tint;
    sptr->value = n;
    return (pcptr+1);
}

void * k_isvoid	(void) {
    if (sptr->tag == tvoid) {
        sptr->tagx = tint;
        sptr->value = 1;
    }
    else {
        if (!!sptr->hasref) {
            vx_ufree(sptr);
        }
        sptr->tagx = tint;
        sptr->value = 0;
    }
    return (pcptr+1);
}

void * k_isdef	(void) {
    if (sptr->tag != tvoid) {
        sptr->tagx = tint;
        sptr->value = 1;
    }
    else {
        if (!!sptr->hasref) {
            vx_ufree(sptr);
        }
        sptr->tagx = tint;
        sptr->value = 0;
    }
    return (pcptr+1);
}

void * k_tostr	(void) {
    pclunimpl(ktostr);
    return (pcptr+1);
}

void * k_isequal	(void) {
varrec *	x;
varrec *	y;
int32	xt;
int32	yt;
    y = sptr;
    x = ++sptr;
    xt = x->tag;
    yt = y->tag;
    if (xt == trecordlink) {
        xt = x->refelemtag;
        x->hasref = 1;
    }
    if (yt == trecordlink) {
        yt = y->refelemtag;
    }
    if (xt == yt && !!x->hasref) {
        sptr->tagx = tint;
        sptr->value = (int64)(x->objptr == y->objptr);
        return (pcptr+1);
    }
    if (xt == tint && x->value == 0 && !!y->hasref || yt == tint && y->value == 0 && !!x->hasref || xt == tint && yt == tint && x->value == 0 && !!y->value) {
        sptr->tagx = tint;
        sptr->value = 0;
        return (pcptr+1);
    }
    pcmxtypes("ISEQUAL",x,y);
    return 0;
}

void * k_add	(void) {
varrec *	x;
varrec *	y;
int32	xt;
int32	yt;
varrec	result;
    y = sptr++;
    xt = sptr->tag;
    yt = y->tag;
    if (xt != yt) {
        if (!!(xt = vx_mixed(sptr,y))) {
            goto L54;
        }
        xt = sptr->tag;
        yt = y->tag;
        switch (xt) {
        case trefpacked:
            switch (yt) {
            case tint:
                sptr->ptr = sptr->ptr+y->value;
                return (pcptr+1);
                break;
            default:;
            }
            break;
        case trefvar:
            switch (yt) {
            case tint:
                sptr->varptr = sptr->varptr+y->value;
                return (pcptr+1);
                break;
            default:;
            }
            break;
        default:;
        }
        pcmxtypes("ADD MIXED",sptr,y);
    }
retry:
L54:;
    switch (xt) {
    case tint:
    case tword:
        sptr->tagx = tint;
        sptr->value += y->value;
        break;
    case treal:
        sptr->tagx = treal;
        sptr->xvalue = sptr->xvalue+y->xvalue;
        break;
    case tstring:
        vx_addstring(sptr,y,sptr);
        break;
    case tlongint:
        bn_add(sptr,y,&result);
        vx_ufree(sptr);
        vx_ufree(y);
        *sptr = result;
        break;
    case tset:
        vx_iorset(sptr,y,sptr);
        break;
    default:;
        pcustype("ADD781",sptr);
    }
    return (pcptr+1);
}

void * k_sub	(void) {
varrec *	y;
int32	xt;
int32	yt;
int32	elemsize;
byte *	p;
byte *	q;
varrec	result;
    y = sptr++;
    xt = sptr->tag;
    yt = y->tag;
    if (xt != yt) {
        if (!!(xt = vx_mixed(sptr,y))) {
            goto L55;
        }
        xt = sptr->tag;
        yt = y->tag;
        switch (xt) {
        case trefpacked:
            switch (yt) {
            case tint:
                sptr->ptr = sptr->ptr-(int32)y->value;
                return (pcptr+1);
                break;
            default:;
            }
            break;
        case trefvar:
            switch (yt) {
            case tint:
                sptr->varptr = sptr->varptr-(int32)y->value;
                return (pcptr+1);
                break;
            default:;
            }
            break;
        default:;
        }
        pcmxtypes("sub/mixed",sptr,y);
    }
retry:
L55:;
    switch (xt) {
    case tint:
        sptr->tagx = tint;
        sptr->value = sptr->value-y->value;
        break;
    case treal:
        sptr->tagx = treal;
        sptr->xvalue = sptr->xvalue-y->xvalue;
        break;
    case tlongint:
        bn_sub(sptr,y,&result);
        vx_ufree(sptr);
        vx_ufree(y);
        *sptr = result;
        break;
    case trefpacked:
        p = sptr->ptr;
        q = y->ptr;
        if ((elemsize = ttsize[sptr->refelemtag]==1)) {
            sptr->value = (p-q);
        } else if ((elemsize = ttsize[sptr->refelemtag]==2)) {
            sptr->value = (p-q>>1);
        } else if ((elemsize = ttsize[sptr->refelemtag]==4)) {
            sptr->value = (p-q>>2);
        }
        else {
            sptr->value = ((p-q)/ elemsize);
        }
        sptr->tagx = tint;
        break;
    default:;
        pcustype("SUB",sptr);
    }
    return (pcptr+1);
}

void * k_mul	(void) {
varrec *	x;
varrec *	y;
int32	xt;
int32	yt;
varrec	result;
    y = sptr++;
    xt = sptr->tag;
    yt = y->tag;
    if (xt != yt) {
        if (!!(xt = vx_mixed(sptr,y))) {
            goto L59;
        }
        xt = sptr->tag;
        yt = y->tag;
        switch (xt) {
        case tlist:
            switch (yt) {
            case tint:
                vx_mul_listi(sptr,y,sptr);
                return (pcptr+1);
                break;
            default:;
            }
            break;
        case tstring:
            switch (yt) {
            case tint:
                vx_mul_stri(sptr,y,sptr);
                return (pcptr+1);
                break;
            default:;
            }
            break;
        default:;
        }
        pcmxtypes("MUL MIXED",sptr,y);
    }
retry:
L59:;
    switch (xt) {
    case tint:
        sptr->tagx = tint;
        sptr->value *= y->value;
        break;
    case treal:
        sptr->tagx = treal;
        sptr->xvalue = sptr->xvalue*y->xvalue;
        break;
    case tword:
        sptr->tagx = tword;
        sptr->uvalue = sptr->uvalue*y->uvalue;
        break;
    case tlongint:
        bn_mul(sptr,y,&result);
        vx_ufree(sptr);
        vx_ufree(y);
        *sptr = result;
        break;
    default:;
        pcustype("MUL",sptr);
    }
    return (pcptr+1);
}

void * k_div	(void) {
varrec *	x;
varrec *	y;
    y = sptr++;
    vx_div(sptr,y,sptr);
    return (pcptr+1);
}

void * k_idiv	(void) {
varrec *	x;
varrec *	y;
    y = sptr++;
    vx_intdiv(sptr,y,sptr);
    return (pcptr+1);
}

void * k_rem	(void) {
varrec *	x;
varrec *	y;
int32	xt;
int32	yt;
varrec	r1;
varrec	r2;
varrec	r3;
    y = sptr;
    x = ++sptr;
    xt = x->tag;
    yt = y->tag;
    if (xt != yt) {
        if (!!(xt = vx_mixed(x,y))) {
            goto L60;
        }
        xt = x->tag;
        yt = y->tag;
        pcmxtypestt("REM",xt,yt);
    }
retry:
L60:;
    switch (xt) {
    case tint:
        sptr->value = x->value% y->value;
        break;
    case treal:
        sptr->xvalue = fmod(x->xvalue,y->xvalue);
        break;
    default:;
        pcustypet("REM",xt);
    }
    return (pcptr+1);
}

void * k_divrem	(void) {
    pclunimpl(kdivrem);
    return (pcptr+1);
}

void * k_iand	(void) {
varrec *	y;
int32	xt;
    y = sptr++;
    if ((xt = sptr->tag) != y->tag) {
        if (!!(xt = vx_mixed(sptr,y))) {
            goto L61;
        }
        pcmxtypes("IAND",sptr,y);
    }
retry:
L61:;
    switch (xt) {
    case tint:
    case tword:
        sptr->value &= y->value;
        break;
    default:;
        pcustype("IAND",sptr);
    }
    return (pcptr+1);
}

void * k_ior	(void) {
varrec *	x;
varrec *	y;
    y = sptr++;
    vx_ior(sptr,y,sptr);
    return (pcptr+1);
}

void * k_ixor	(void) {
varrec *	x;
varrec *	y;
    y = sptr++;
    vx_ixor(sptr,y,sptr);
    return (pcptr+1);
}

void * k_shl	(void) {
varrec *	x;
varrec *	y;
    y = sptr++;
    x = sptr;
    switch (x->tag) {
    case tint:
        switch (y->tag) {
        case tint:
            sptr->value = sptr->value<<y->value;
            return (pcptr+1);
            break;
        default:;
        }
        break;
    default:;
        pcmxtypes("SHL",x,y);
    }
    return (pcptr+1);
}

void * k_shr	(void) {
varrec *	x;
varrec *	y;
    y = sptr++;
    x = sptr;
    switch (x->tag) {
    case tint:
        switch (y->tag) {
        case tint:
            sptr->value = sptr->value>>y->value;
            return (pcptr+1);
            break;
        default:;
        }
        break;
    default:;
    }
    pcmxtypes("SHR",x,y);
    return (pcptr+1);
}

void * k_in	(void) {
varrec *	y;
    y = sptr++;
    vx_in(sptr,y,sptr);
    return (pcptr+1);
}

void * k_notin	(void) {
varrec *	y;
    y = sptr++;
    vx_notin(sptr,y,sptr);
    return (pcptr+1);
}

void * k_inrev	(void) {
    pclunimpl(kinrev);
    return (pcptr+1);
}

void * k_eq	(void) {
varrec *	x;
varrec *	y;
int32	res;
    y = sptr;
    x = ++sptr;
    res = vx_equal(x,y,0);
    sptr->tagx = tint;
    sptr->value = res;
    return (pcptr+1);
}

void * k_ne	(void) {
varrec *	x;
varrec *	y;
int32	res;
    y = sptr;
    x = ++sptr;
    res = vx_equal(x,y,0);
    sptr->tagx = tint;
    sptr->value = (int64)!res;
    return (pcptr+1);
}

void * k_lt	(void) {
varrec *	x;
varrec *	y;
int32	res;
    y = sptr;
    x = ++sptr;
    res = vx_compare(x,y);
    sptr->tagx = tint;
    sptr->value = (int64)(res < 0);
    return (pcptr+1);
}

void * k_le	(void) {
varrec *	x;
varrec *	y;
int32	res;
    y = sptr;
    x = ++sptr;
    res = vx_compare(x,y);
    sptr->tagx = tint;
    sptr->value = (int64)(res <= 0);
    return (pcptr+1);
}

void * k_ge	(void) {
varrec *	x;
varrec *	y;
int32	res;
    y = sptr;
    x = ++sptr;
    res = vx_compare(x,y);
    sptr->tagx = tint;
    sptr->value = (int64)(res >= 0);
    return (pcptr+1);
}

void * k_gt	(void) {
varrec *	x;
varrec *	y;
int32	res;
    y = sptr;
    x = ++sptr;
    res = vx_compare(x,y);
    sptr->tagx = tint;
    sptr->value = (int64)(res > 0);
    return (pcptr+1);
}

void * k_min	(void) {
varrec *	y;
    y = sptr++;
    vx_min(sptr,y,sptr);
    return (pcptr+1);
}

void * k_max	(void) {
varrec *	y;
    y = sptr++;
    vx_max(sptr,y,sptr);
    return (pcptr+1);
}

void * k_concat	(void) {
varrec *	x;
varrec *	y;
    y = sptr++;
    vx_concat(sptr,y,sptr);
    return (pcptr+1);
}

void * k_append	(void) {
varrec *	x;
varrec *	y;
    y = sptr++;
    vx_append(sptr,y,sptr);
    return (pcptr+1);
}

void * k_power	(void) {
varrec *	x;
varrec *	y;
int32	xt;
int32	yt;
varrec	result;
    y = sptr;
    x = ++sptr;
    xt = x->tag;
    yt = y->tag;
    if (xt != yt) {
        if ((xt = vx_mixed(x,y)) == 0) {
            pcmxtypes("**MIXED",x,y);
        }
    }
    switch (xt) {
    case tint:
        sptr->value = ipower(x->value,y->value);
        break;
    case treal:
        sptr->xvalue = pow(x->xvalue,y->xvalue);
        break;
    case tlongint:
        bn_power(x,bn_int(y),&result);
        if (!!x->hasref) {
            vx_ufree(x);
        }
        if (!!y->hasref) {
            vx_ufree(y);
        }
        *sptr = result;
        break;
    default:;
        pcustype("**",x);
    }
    return (pcptr+1);
}

void * k_atan2	(void) {
    pclunimpl(katan2);
    return (pcptr+1);
}

void * k_addto	(void) {
varrec *	x;
varrec *	y;
    y = sptr++;
    x = sptr++;
    if (x->tag != trefvar) {
        pcerror("addto/not ref");
    }
    vx_addto(x->varptr,y);
    return (pcptr+1);
}

void * k_subto	(void) {
varrec *	x;
varrec *	y;
    y = sptr++;
    x = sptr++;
    if (x->tag != trefvar) {
        pcerror("subto/not ref");
    }
    vx_subto(x->varptr,y);
    return (pcptr+1);
}

void * k_multo	(void) {
int32 *	pc;
varrec	ptr;
    ptr = *(sptr+1);
    vx_loadptr(sptr+1,sptr+1);
    pc = (int32 *)k_mul();
    vx_storeptr(&ptr,sptr++);
    return pc;
}

void * k_divto	(void) {
int32 *	pc;
varrec	ptr;
    ptr = *(sptr+1);
    vx_loadptr(sptr+1,sptr+1);
    pc = (int32 *)k_div();
    vx_storeptr(&ptr,sptr++);
    return pc;
}

void * k_idivto	(void) {
    pclunimpl(kidivto);
    return (pcptr+1);
}

void * k_iandto	(void) {
int32 *	pc;
varrec	ptr;
    ptr = *(sptr+1);
    vx_loadptr(sptr+1,sptr+1);
    pc = (int32 *)k_iand();
    vx_storeptr(&ptr,sptr++);
    return pc;
}

void * k_iorto	(void) {
int32 *	pc;
varrec	ptr;
    ptr = *(sptr+1);
    vx_loadptr(sptr+1,sptr+1);
    pc = (int32 *)k_ior();
    vx_storeptr(&ptr,sptr++);
    return pc;
}

void * k_ixorto	(void) {
int32 *	pc;
varrec	ptr;
    ptr = *(sptr+1);
    vx_loadptr(sptr+1,sptr+1);
    pc = (int32 *)k_ixor();
    vx_storeptr(&ptr,sptr++);
    return pc;
}

void * k_shlto	(void) {
int32 *	pc;
varrec	ptr;
    ptr = *(sptr+1);
    vx_loadptr(sptr+1,sptr+1);
    pc = (int32 *)k_shl();
    vx_storeptr(&ptr,sptr++);
    return pc;
}

void * k_shrto	(void) {
int32 *	pc;
varrec	ptr;
    ptr = *(sptr+1);
    vx_loadptr(sptr+1,sptr+1);
    pc = (int32 *)k_shr();
    vx_storeptr(&ptr,sptr++);
    return pc;
}

void * k_minto	(void) {
int32 *	pc;
varrec	ptr;
    ptr = *(sptr+1);
    vx_loadptr(sptr+1,sptr+1);
    pc = (int32 *)k_min();
    vx_storeptr(&ptr,sptr++);
    return pc;
}

void * k_maxto	(void) {
int32 *	pc;
varrec	ptr;
    ptr = *(sptr+1);
    vx_loadptr(sptr+1,sptr+1);
    pc = (int32 *)k_max();
    vx_storeptr(&ptr,sptr++);
    return pc;
}

void * k_concatto	(void) {
varrec *	x;
varrec *	y;
    y = sptr++;
    x = sptr++;
    if (x->tag != trefvar) {
        pcerror("concatto/not ref");
    }
    vx_concatto(x->varptr,y);
    return (pcptr+1);
}

void * k_appendto	(void) {
varrec *	x;
varrec *	y;
    y = sptr++;
    x = sptr++;
    if (x->tag != trefvar) {
        pcerror("appendto/not ref");
    }
    vx_appendto(x->varptr,y);
    return (pcptr+1);
}

void * k_pushix	(void) {
varrec *	x;
varrec *	y;
objrec *	p;
varrec	va;
int32	index;
int32	length;
int32	acopy;
int32	elemtype;
    y = sptr++;
    x = sptr;
    if ((y->tag==tint)) {
    } else if ((y->tag==trange)) {
        switch (ttbasetype[x->tag]) {
        case tlist:
            vx_slicelist(x,y,x);
            break;
        case tstring:
            vx_slicestring(x,y,x);
            break;
        default:;
            pcustype("slice",x);
        }
        return (pcptr+1);
    }
    else {
        pcustype("index?",y);
    }
    switch (ttbasetype[x->tag]) {
    case tlist:
    case trecord:
        va = *x;
        p = va.objptr;
        index = (y->value-p->lower);
        if ((uint32)index >= (uint32)p->length) {
            pcerror("list[int] bounds");
        }
        *x = *(p->vptr+index);
        if (!!x->hasref) {
            ++x->objptr->refcount;
        }
        vx_ufree(&va);
        break;
    case tstring:
        vx_indexstring(x,y,x);
        break;
    case tarray:
        va = *x;
        p = va.objptr;
        index = (y->value-p->lower);
        if ((uint32)index >= (uint32)p->length) {
            pcerror("array[int] bounds");
        }
        if ((elemtype = p->elemtag) == tu8) {
            x->value = *(p->ptr+index);
            x->tagx = tint;
        }
        else {
            vx_loadpacked((p->ptr+index*ttsize[elemtype]),elemtype,x,p);
        }
        if (!!va.hasref) {
            vx_ufree(&va);
        }
        break;
    default:;
        pcustype("list[int]",x);
    }
    return (pcptr+1);
}

void * k_pushdotix	(void) {
varrec *	x;
varrec *	y;
objrec *	p;
varrec	va;
int32	index;
int32	length;
int32	acopy;
int32	value;
    y = sptr++;
    x = sptr;
    if ((y->tag==tint)) {
    } else if ((y->tag==trange)) {
        vx_dotslice(x,y,x);
        return (pcptr+1);
    }
    else {
        pcustype(".index?",y);
    }
    switch (ttbasetype[x->tag]) {
    case tstring:
        p = x->objptr;
        index = (y->value-1);
        if ((uint32)index >= (uint32)p->length) {
            pcerror("string.[int] bounds");
        }
        value = (uchar)*(p->strptr+index);
        vx_ufree(x);
        sptr->tagx = tint;
        sptr->value = value;
        break;
    case tint:
        index = y->value;
        if (index < 0 || index >= 64) {
            pcerror("int.[int] bounds");
        }
        sptr->value = x->value>>index&1;
        break;
    case trecord:
        --sptr;
        return k_pushix();
        break;
    default:;
        pcustype(".index[int]",x);
    }
    return (pcptr+1);
}

void * k_pushkeyix	(void) {
varrec *	y;
    y = sptr++;
    vx_getkeyix(y,sptr,0,sptr);
    return (pcptr+1);
}

void * k_pushkeyixd	(void) {
varrec *	y;
varrec *	def;
    def = sptr++;
    y = sptr++;
    vx_getkeyix(y,sptr,def,sptr);
    return (pcptr+1);
}

void * k_pushixref	(void) {
objrec *	p;
int32	index;
int32	elemtype;
varrec *	a;
varrec *	y;
    y = sptr++;
    if (sptr->tag != trefvar) {
        pcerror("ixref/not ref");
    }
    a = sptr->varptr;
    if ((y->tag==tint)) {
    } else if ((y->tag==trange)) {
        vx_sliceref(a,y,sptr);
        return (pcptr+1);
    }
    else {
        pcustype("&index?",y);
    }
    switch (ttbasetype[a->tag]) {
    case tlist:
    case trecord:
        p = a->objptr;
        if (!p->mutable) {
            p = a->objptr = copyonwrite(p,tlist);
        }
        index = (y->value-p->lower);
        if ((uint32)index >= (uint32)p->length) {
            if (index < 0) {
                pcerror("LWB");
            }
            else {
                if ((uint32)index == (uint32)p->length) {
                    vx_iappendlist(a,0);
                    p = a->objptr;
                }
                else {
                    pcerror("&getixref list[i] bounds");
                }
            }
        }
        sptr->tagx = trefvar;
        sptr->varptr = p->vptr+index;
        break;
    case tstring:
        p = a->objptr;
        index = (y->value-1);
        if (!p->mutable) {
            a->objptr = p = copyonwrite(p,tstring);
        }
        if ((uint32)index >= (uint32)p->length) {
            pcerror("&str[int] bounds");
        }
        sptr->tagx = trefpacked;
        sptr->refelemtag = tstring;
        sptr->ptr = (byte *)(p->strptr+index);
        break;
    case tarray:
        p = a->objptr;
        if (!p->mutable) {
            p = a->objptr = copyonwrite(p,a->tag);
        }
        index = (y->value-p->lower);
        if ((uint32)index >= (uint32)p->length) {
            if (index < 0) {
                pcerror("&AXLWB");
            }
            else {
                if ((uint32)index == (uint32)p->length) {
                    vx_iappendarray(a,0);
                    p = a->objptr;
                }
                else {
                    pcerror("&AXBOUNDS");
                }
            }
        }
        elemtype = p->elemtag;
        sptr->tagx = trefpacked;
        sptr->refelemtag = elemtype;
        sptr->ptr = p->ptr+index*ttsize[elemtype];
        break;
    default:;
        pcustype("&index[int]",a);
    }
    return (pcptr+1);
}

void * k_pushdotixref	(void) {
varrec *	y;
    y = sptr++;
    if (sptr->tag != trefvar) {
        pcerror(".ixref/not ref");
    }
    vx_dotindexref(sptr->varptr,y,sptr);
    return (pcptr+1);
}

void * k_pushkeyixref	(void) {
varrec *	d;
varrec *	k;
varrec *	p;
varrec *	pd;
    d = sptr->varptr;
    k = ++sptr;
    if (d->tag != tdict) {
        pcustype("keyixref",d);
    }
    p = finddictitem(d->objptr,k,1);
    if (!!k->hasref) {
        vx_ufree(k);
    }
    sptr->tagx = trefvar;
    sptr->varptr = p;
    return (pcptr+1);
}

void * k_pushbyteix	(void) {
int64	a;
uint32	index;
varrec *	x;
    if (sptr->tag != tint) {
        pcerror("byteix/bad index");
    }
    index = sptr->value;
    ++sptr;
    if (sptr->tag != tint) {
        pcerror("byteix/not int");
    }
    a = sptr->value;
    if ((*(pcptr+1)==tu8)) {
        if (index >= 8) {
            if (index >= 12) {
                pcerror("byteix bounds");
            }
            a = sptr->frameptr_low;
            sptr->value = a>>(index-8)*8&255;
        }
        else {
            sptr->value = a>>index*8&255;
        }
    }
    else {
        pcerror("byteix/bad type");
    }
    return (pcptr+2);
}

void * k_pushbyteixref	(void) {
byte *	a;
uint32	index;
varrec *	p;
    if (sptr->tag != tint) {
        pcerror("&byteix/bad index");
    }
    index = sptr->value;
    ++sptr;
    if (sptr->tag != trefvar) {
        pcerror("&byteix/not ptr");
    }
    p = sptr->varptr;
    if (p->tag != tint) {
        pcerror("&bytix/not int");
    }
    sptr->tagx = trefpacked;
    if ((*(pcptr+1)==tu8)) {
        if (index >= 8) {
            if (index >= 12) {
                pcerror("&byteix bounds");
            }
            sptr->packptr = (byte *)p+index-4;
        }
        else {
            sptr->packptr = (byte *)p+8+index;
        }
        sptr->refelemtag = tu8;
    }
    else {
        pcerror("&byteix/bad type");
    }
    return (pcptr+2);
}

void * k_appendset	(void) {
    pclunimpl(kappendset);
    return (pcptr+1);
}

void * k_pushdotm	(void) {
    pclunimpl(kpushdotm);
    return (pcptr+3);
}

void * k_pushdott	(void) {
    pclunimpl(kpushdott);
    return (pcptr+3);
}

void * k_push_ad	(void) {
    (--sptr)->tagx = trefdllproc;
    sptr->ptr = (byte *)(void *)dllproctable[*(pcptr+1)-1].address;
    return (pcptr+2);
}

void * k_push_try	(void) {
    (--sptr)->tagx = texception;
    sptr->ptr = (byte *)(int32)*(pcptr+1);
    sptr->frameoffset = frameptr-(byte *)sptr;
    sptr->exceptiontype = *(pcptr+2);
    sptr->nexceptions = *(pcptr+3);
    return (pcptr+4);
}

void * k_raise	(void) {
    if (sptr->tag != tint) {
        pcerror("Raise not int");
    }
    return raiseexception(sptr->value);
}

void * k_applyop	(void) {
static int64	codeseq[10];
    codeseq[1-1] = (int64)(int32)cmdmap[sptr->value];
    if (sptr->tag != toperator) {
        pcerror("Apply:no op");
    }
    if (sptr->opdims != *(pcptr+1)) {
        printf("%lld %u\n",*(pcptr+1),sptr->opdims);
        pcerror("Apply:wrong #opnds");
    }
    ++sptr;
    codeseq[2-1] = *(pcptr+2);
    codeseq[3-1] = *(pcptr+3);
    return &codeseq[1-1];
}

void * k_makeiter	(void) {
objrec *	p;
    p = sptr->objptr;
    sptr->itcount = p->length+1;
    if ((sptr->tag==tlist)) {
        sptr->varptr = p->vptr;
        sptr->tagx = trefvar;
        sptr->ittype = tlist;
    } else if ((sptr->tag==tstring)) {
        sptr->ptr = (byte *)p->strptr;
        sptr->tagx = trefpacked;
        sptr->ittype = tstring;
    }
    else {
        pcustype("makeiter",sptr);
    }
    return (pcptr+2);
}

void * k_forall	(void) {
varrec *	pit;
varrec *	ploopvar;
varrec *	pelem;
    pit = (varrec *)(frameptr+*(pcptr+2));
    ploopvar = (varrec *)(frameptr+*(pcptr+3));
    if (--pit->itcount <= 0) {
        return (pcptr+4);
    }
    if (!!ploopvar->hasref) {
        --ploopvar->objptr->refcount;
    }
    if ((pit->ittype==tlist)) {
        pelem = pit->varptr;
        *ploopvar = *pelem;
        if (!!ploopvar->hasref) {
            ++ploopvar->objptr->refcount;
        }
        ++pit->varptr;
    } else if ((pit->ittype==tstring)) {
        ploopvar->tagx = (tstring|hasrefmask);
        ploopvar->objptr = make_char(*pit->ptr);
        ++pit->ptr;
    }
    else {
        pcerror("forall/type?");
    }
    return (int64 *)(int32)*(pcptr+1);
}

void * k_forallx	(void) {
    pclunimpl(kforallx);
    return (pcptr+5);
}

void * k_foreach	(void) {
    pclunimpl(kforeach);
    return (pcptr+4);
}

void * k_foreachx	(void) {
    pclunimpl(kforeachx);
    return (pcptr+5);
}

void * k_expandrange	(void) {
varrec *	x;
    x = sptr--;
    sptr->tagx = tint;
    sptr->value = x->range_upper;
    x->value = x->range_lower;
    x->tagx = tint;
    return (pcptr+1);
}


// From module: pc_assem

// From module: start
void start	(void) {
int32	i;
int32	status;
int32	t;
int32	N;
int32	tx;
int32	stopcode;
char *	file;
int32	filetype;
varrec	v;
varrec *	ss=&v;
strec	S;
modulerec	m;
    if (dispatch_type == asm_dispatch) {
        dispatch_type = fn_dispatch;
    }
    printf("%s%s %s%s\n","PCL Interpreter 17.5.11 (",dispatchnames[dispatch_type-1],"C32",")");
    initdata();
    file = getinputfile(&filetype);
    if (file == 0) {
        exit(0);
    }
    if (fshowtimes) {
        starttiming();
    }
    initlogfile();
    t = clock();
    runfrompc = 1;
    status = loadprogram(file,&filetype);
    if (!status) {
        loaderror("Couldn't load:",file);
    }
    if (fverbose) {
        printf("%s %s\n","Loaded",file);
    }
    if (floadpc) {
        showmodules();
        exit(0);
    }
    stopcode = 0;
    stopcode = runcode(floadpc);
    exit(stopcode);
}

static void initlogfile	(void) {
    if (!fshowlog) {
        return;
    }
    if ((logdest==2)) {
        remove("rr.log");
        logdev = fopen("rr.log","w");
    } else if ((logdest==0) || (logdest==1)) {
        logdev = 0;
    }
    last_tx = 0;
}

static void closelogfile	(void) {
char	str[100];
int32	pos;
    printf("%s\n","CLOSELOGFILE");
    if (logdest == 2) {
        fclose(logdev);
        pos = last_tx;
        if (!!*txpos) {
            pos = *txpos;
        }
        sprintf((char *)&str,"\\m\\ed.bat  %s","rr.log");
        os_execwait((char *)&str,1,0);
    }
    else if (dopause && logdest != 0) {
        printf("%s","PrEss key...");
        os_getch();
        printf("\n");
    }
}

static void initdata	(void) {
    pcm_init();
    inittypetables();
    makezobjects();
    initpcldata();
}

static char * getinputfile	(int32 * filetype) {
enum {slash = '-'};
int32	i;
int32	j;
int32	k;
int32	status;
char	swstr[50];
char	filename[300];
int64	filetime;
char *	sw;
char *	file;
char *	fileext;
char *	file2;
static int32	loadsw=0;
static int32	fnsw=0;
static int32	labsw=0;
static int32	asmsw=0;
static int32	debsw=0;
static int32	tracesw=0;
static int32	dtracesw=0;
static int32	dsw=0;
static int32	ssw=0;
static int32	pcl2sw=0;
static int32	vsw=0;
static int32	v2sw=0;
static int32	timesw=0;
static int32	showhelp=0;
static switchrec	switchtable[14]={{"load",&loadsw},
{"fn",&fnsw},
{"lab",&labsw},
{"asm",&asmsw},
{"deb",&debsw},
{"trace",&tracesw},
{"dtrace",&dtracesw},
{"d",&dsw},
{"s",&ssw},
{"pcl2",&pcl2sw},
{"v",&vsw},
{"v2",&v2sw},
{"time",&timesw},
{"help",&showhelp}
};
    j = 0;
    file = 0;
    for (i=1; i<=nsysparams; ++i) {
        sw = sysparams[i-1];
        if ((uchar)*sw == slash) {
            strcpy((char *)&swstr,sw+1);
            convlcstring((char *)&swstr);
            for (k=1; k<=14; ++k) {
                if (strcmp(switchtable[k-1].switchname,(char *)&swstr) == 0) {
                    *switchtable[k-1].switchaddr = 1;
                    goto L14;
                }
            }
            loaderror("Unknownswitch:",(char *)&swstr);
L14:;
        }
        else {
storesw:
            ++j;
            if (i != j) {
                sysparams[j-1] = sysparams[i-1];
            }
        }
    }
    nsysparams = j;
    if (showhelp) {
        printf("%s\n","General usage:");
        printf("%s%s %s\n","\t",sysparams[1-1]," [options] filename[.pc/.q]\n");
        printf("%s\n","Selected Options:");
        printf("%s\n","\t-load         Load only, show module list");
        printf("%s\n","\t-s            Show tables, don't run");
        printf("%s\n","\t-d            Show tables to log file, don't run");
        printf("%s\n","\t-v            Verbose");
        printf("%s\n","\t-v2           More verbose");
        printf("%s\n","\t-time         Execution time");
        printf("%s\n","\t-fn           Function table dispatcher (default)");
        printf("%s\n","\t-lab          (Label table dispatcher; not available this version)");
        printf("%s\n","\t-asm          Fast ASM dispatcher; non-C version only");
        printf("%s\n","\t-deb [-trace] Debugging dispatcher [with trace on every byte-code]");
        return 0;
    }
    if (nsysparams >= 2) {
        file = (char *)&filename;
        strcpy(file,sysparams[2-1]);
    }
    else {
        printf("%s\n","Usage:");
        printf("%s%s %s\n","\t",sysparams[1-1]," [options] filename[.pc/.q]");
        printf("%s%s %s\n","\t",sysparams[1-1]," -help");
        return 0;
    }
    getsyscmdline();
    convlcstring(file);
    fileext = extractext(file,0);
    file2 = 0;
    if ((uchar)*fileext == 0) {
        if (os_filelastwritetime(0) == 0) {
            *filetype = 17232;
            file2 = pcm_copyheapstring(addext(file,".pc"));
            goto L18;
        }
        findnewestfile(&file2,&filetime,filetype,file,".pc",17232);
        filetime = 0;
        *filetype = 0;
        findnewestfile(&file2,&filetime,filetype,file,".pc",17232);
        findnewestfile(&file2,&filetime,filetype,file,".q",'Q');
        findnewestfile(&file2,&filetime,filetype,file,".qa",16721);
        if (file2 == 0) {
            loaderror("Can't load",file);
        }
        file = file2;
    }
    else if (strcmp(fileext,"pc") == 0) {
        *filetype = 17232;
    }
    else if (strcmp(fileext,"q") == 0) {
        *filetype = 'Q';
    }
    else if (strcmp(fileext,"qa") == 0) {
        *filetype = 16721;
    }
    else {
        loaderror("Can't find .pc, .q or .qa file",file);
    }
gotfile:
L18:;
    if (file2 == 0) {
        if (!checkfile(file)) {
            loaderror("Can't load",file);
        }
        file2 = pcm_copyheapstring(file);
    }
    if (dsw || ssw) {
        fruncode = 0;
        fshowlog = 1;
        logdest = (dsw?2:1);
        dopause = 1;
    }
    if (loadsw) {
        floadpc = 1;
    }
    if (fnsw) {
        dispatch_type = fn_dispatch;
    }
    if (labsw) {
        dispatch_type = lab_dispatch;
    }
    if (asmsw) {
        dispatch_type = asm_dispatch;
    }
    if (debsw) {
        dispatch_type = deb_dispatch;
    }
    if (tracesw) {
        ftrace = 1;
    }
    if (dtracesw) {
        fdtrace = 1;
    }
    if (vsw) {
        fverbose = 1;
    }
    if (v2sw) {
        fverbose = 2;
    }
    if (timesw) {
        fshowtimes = 1;
    }
    if (pcl2sw) {
        fshowfixedup = 1;
        if (dispatch_type == asm_dispatch) {
            dispatch_type = deb_dispatch;
        }
    }
    return pcm_copyheapstring(file2);
}

static void showpcl	(char * caption,int32 n,int32 phase) {
varrec *	pclstr;
    if (logdest) {
        if (logdest == 2) {
            pclcode_tx = last_tx = ftell(logdev);
        }
        loaderror("WRITEPCCODE","");
    }
}

static void showgenfields	(void) {
    if (logdest) {
        if (logdest == 2) {
            genfields_tx = last_tx = ftell(logdev);
        }
        loaderror("PRINT GENFIELDS","");
        fprintf(logdev,"\n");
    }
}

static void loaderror	(char * mess,char * mess2) {
    printf("%s %s %s\n","Load Error:",mess,mess2);
    printf("%s\n","Stopping");
    exit(0);
}

static void do_showprogdiags_pc	(char * caption) {
int32	i;
strec *	d;
procrec *	pp;
    if (logdest) {
        initbytecode(1);
        printf("%s\n","DONE INITPC");
        fprintf(logdev,"%s\n","SHOW ALL PC PCL2");
        fprintf(logdev,"%s\n",caption);
        showpcl("STARTUP CODE",0,2);
        showpcl("MAIN MODULE",nmodules,2);
    }
}

int32 runcode	(int32 frompc) {
int32	i;
int32	j;
int32	cmd;
int32	fmt;
int32 *	lab;
int32	exitcode;
int64	progticks;
    initbytecode(frompc);
    varstack = (varrec (*)[])malloc(1120000);
    if (!varstack) {
        abortprogram("varstack?");
    }
    sptr = &(*varstack)[stacksize];
    --sptr;
    frameptr = (byte *)sptr;
    pcptr = &(*moduletable[0].pccode)[1-1];
    pccode = (int64 (*)[])&(*moduletable[0].pccode)[0-1];
    stopped = 0;
    if (fverbose) {
        printf("%s\n","Starting Interpreter...");
    }
    progticks = os_clock();
    disploop();
    if (logdev) {
        fclose(logdev);
    }
    if (fverbose) {
        printf("%s\n","\nInterpreter stopped.");
    }
    progticks = os_clock()-progticks;
    if (fshowtimes) {
        printf("%s %lld %s\n","Run time:",progticks,"msec");
    }
    if (sptr->tagx == tvoid) {
        return 0;
    }
    else {
        return sptr->value;
    }
}

static void initbytecode	(int32 frompc) {
    os_initwindows();
    allocatestrings();
    pclinit();
    fixup_all_pc();
}

static void disploop	(void) {
int32	i;
int32	k;
    if ((dispatch_type==fn_dispatch)) {
        disploop_fn(0);
    } else if ((dispatch_type==deb_dispatch)) {
        disploop_deb();
    } else if ((dispatch_type==lab_dispatch)) {
        loaderror("-LAB not available",0);
    } else if ((dispatch_type==asm_dispatch)) {
        loaderror("-ASM not available",0);
    }
}

static void pclinit	(void) {
int32	i;
    for (i=1; i<=klastcmd; ++i) {
        if ((dispatch_type==fn_dispatch)) {
            cmdmap[i] = disploop_fn(i);
        } else if ((dispatch_type==deb_dispatch)) {
            cmdmap[i] = (void *)i;
        } else if ((dispatch_type==asm_dispatch)) {
            cmdmap[i] = (void *)i;
        }
    }
}

static objrec * newstringobject	(char * s) {
objrec *	p;
int32	i;
    i = ++nstringobjects;
    if (runfrompc && i > nstrings) {
        loaderror("nstringobjects>nstrings","");
    }
    p = &(*stringobjtable)[i];
    p->refcount = 5;
    p->strptr = s;
    p->length = strlen(s);
    p->lower = 1;
    p->elemtag = tu8;
    p->objtype = extslice_obj;
    return p;
}

static void fixup_all_pc	(void) {
int32	i;
int32	j;
int32	cmd;
int32	lab;
int32	a;
int32	index;
int32	startindex;
int32	recordtype;
int32	fieldtype;
int32	mx;
int32	pp;
int64	a64;
genfielddatarec	gd;
int64 *	p;
    for (mx=1; mx<=nmodules; ++mx) {
        fixup_module_pc(mx);
    }
    fixup_module_pc(0);
}

static void fixup_module_pc	(int32 mx) {
int32	i;
int32	cmd;
int32	lab;
int32	index;
int32	startindex;
int32	recordtype;
int32	fieldtype;
int32	pp;
int32	m;
int64	a;
genfielddatarec	gd;
int64 *	p;
int64 *	pccode;
strec *	d;
static int32	SCOUNT=0;
varrec *	v;
int32	av_1;
    pccode = p = (int64 *)moduletable[mx].pccode;
    while (1) {
        cmd = *p;
        if (mx == 0) {
            if (cmd == kstop && stopseq == 0) {
                stopseq = p-2;
            }
            else if (cmd == kraise && raiseseq == 0) {
                raiseseq = p;
            }
        }
        *p++ = (int64)(int32)cmdmap[cmd];
        for (i=1; i<=cmdnopnds[cmd]; ++i) {
            switch (cmdfmt[cmd][i-1]) {
            case clabel:
                *p = (int64)(int32)(pccode+*p-1);
                break;
            case cproc:
                d = &(*pcsymboltable)[*p-1];
                if (d->address == 0) {
                    m = d->owner->attribs.ax_moduleno;
                    d->address = (void *)&(*moduletable[m].pccode)[d->index-1];
                }
                *p = (int64)(int32)d->address;
                break;
            case cmemory:
                d = &(*pcsymboltable)[*p-1];
                if (d->address == 0) {
                    v = (varrec *)pcm_alloc(varsize);
                    v->tagx = tvoid;
                    d->address = v;
                }
                *p = (int64)(int32)d->address;
                break;
            case cstring:
                *p = (int64)(int32)&(*stringobjtable)[*p];
                break;
            default:;
            }
            ++p;
        }
        if (cmd == kendmodule || cmd == 0) {
            goto L38;
        }
    }
L38:;
    if (dispatch_type == asm_dispatch) {
        loaderror("fixup asm","");
    }
}

static int32 * disploop_fn	(int32 n) {
int32	count;
enum {intervalcount = 1};
uint32	lastticks;
uint32	ticks;
    if (n) {
        return (int32 *)handlertable[n];
    }
    count = intervalcount;
    lastticks = os_clock();
    do {
        pcptr = (int64 *)(*(int32 * (**)	(void))pcptr)();
    } while (!stopped);
    ticks = (os_clock()-lastticks);
    return 0;
}

static void disploop_deb	(void) {
int32	a;
int32	b;
int32	i;
int32	j;
int32	k;
int32	totalcounts;
int32	lastcmd;
int32	cmd;
int32	t;
int32	u;
int32	index;
int32	file;
int32	line;
int32	moduleno;
varrec *	p;
int32	count;
enum {intervalcount = 1};
int32 * (*fn)	(void);
uint32	lastticks;
uint32	ticks;
char *	filename;
    count = intervalcount;
    lastticks = os_clock();
    printf("%s\n","DISPLOOP/DEB");
    lastcmd = knop;
    do {
        cmd = *pcptr;
        if (fdebug && fdtrace || ftrace) {
            findlinenumber(pcptr,&line,&moduleno);
            printf("%s%s%s %s %d %s %s %d %s %p %s %s %d\n","<",cmdnames[cmd],">","LINE=",line,moduletable[moduleno].name,"CMD=",cmd,"SPTR=",(void*)(sptr),ttname[sptr->tag],"PPP^=",(PPP?(*PPP):0));
        }
        pcptr = (int64 *)((int32 * (*)	(void))handlertable[*pcptr])();
        if (fdebug) {
            printf("%s %p\n","NEXT PCPTR2:",(void*)(pcptr));
        }
    } while (!stopped);
    printf("%s\n","DEB STOPPED");
}

static void getsyscmdline	(void) {
int32	i;
    for (i=1; i<=nsysparams; ++i) {
        if (i <= maxcmdparam) {
            cmdparamtable[i-1] = pcm_copyheapstring(sysparams[i-1]);
        }
    }
    ncmdparams = nsysparams-1;
}

static void showmodules	(void) {
int32	i;
int32	j;
int32	k;
modulerec	m;
int32	av_2;
    printf("%s %d\n","Modules:",nmodules);
    for (i=nmodules; i>=1; --i) {
        m = moduletable[i];
        j = (nmodules-i)+1;
        printf("%d%s",j,":");
        av_2 = m.level;
        while (av_2--) {
            printf("%s","  ");
        }
        printf("%s %d\n",m.name,m.sourcelen);
        printf("%s","       ");
        for (j=1; j<=nmodules; ++j) {
            k = (m).importmap[j-1];
            if (k) {
                printf("%s%s",moduletable[j].name," ");
            }
        }
        printf("\n");
    }
    if (nextra) {
        printf("%s %d\n","Extra Modules",nextra);
        for (i=1; i<=nextra; ++i) {
            printf("%d %s %s %d %p\n",i,":",extrafiles[i-1],extrasizes[i-1],(void *)extratext[i-1]);
        }
    }
}

static void starttiming	(void) {
int32	t;
    t = os_clock();
    do {
    } while (!(os_clock() != t));
    clockstart = os_clock();
}

static void showtiming	(void) {
int32	t;
int32	i;
    t = (os_clock()-clockstart);
    printf("%lld %s\n",(t*1000)/ os_getclockspersec(),"msec\tLoad/compile time");
    printf("%d%s\n",totallines,"\tTotal lines compiled");
    if (t > 1) {
        printf("%d%s\n",(int32)((totallines/ (double)t)*os_getclockspersec()),"\tLines per second compile speed");
    }
    printf("%d%s\n",totalpclopcodes,"\tPCL byte-codes generated");
}

void runproc	(void * fnptr,varrec * a,varrec * b,varrec * dest) {
varrec *	oldsptr;
byte *	oldframeptr;
int32 *	oldpcptr;
byte	oldstopped;
int32	nparams;
    dest->tagx = ti32;
    dest->value = 0;
    oldstopped = stopped;
    oldpcptr = (int32 *)pcptr;
    oldsptr = sptr;
    oldframeptr = frameptr;
    (--sptr)->tagx = 999;
    if (b && !!b->tag) {
        nparams = 2;
        *--sptr = *b;
        *--sptr = *a;
    }
    else if (a && !!a->tag) {
        nparams = 1;
        *--sptr = *a;
    }
    else {
        nparams = 0;
    }
    (--sptr)->tagx = tretaddr;
    sptr->retaddr = (int32 *)stopseq;
    sptr->frameptr_low = *(int32*)&frameptr;
    sptr->stackadj = (nparams*varsize);
    frameptr = (byte *)sptr;
    pcptr = (int64 *)fnptr;
    disploop();
    if ((sptr+1)->tag == tretaddr) {
        *dest = *sptr;
    }
    else {
        sptr += nparams;
        *dest = *sptr;
        if (dest->tag == tvoid) {
            dest->tagx = ti32;
            dest->value = 0;
        }
        else {
            if (dest->tag == tstring) {
                printf("RESULT STR: %.*s\n",dest->objptr->length,dest->objptr->strptr);
            }
        }
    }
    pcptr = (int64 *)oldpcptr;
    stopped = oldstopped;
    sptr = oldsptr;
    frameptr = oldframeptr;
    stopped = oldstopped;
}

static void showinttiming	(char * caption) {
int32	t;
int32	i;
    t = (os_clock()-clockstart);
    printf("%lld %s %s\n",(t*1000)/ os_getclockspersec(),"msec",caption);
}

static void allocatestrings	(void) {
objrec *	p;
int32	i;
char *	s;
    stringobjtable = (objrec (*)[])zalloctable(nstrings,32);
    for (i=1; i<=nstrings; ++i) {
        s = (*stringtable)[i-1];
        p = &(*stringobjtable)[i];
        p->refcount = 5;
        p->strptr = s;
        p->length = strlen(s);
        p->lower = 1;
        p->elemtag = tu8;
        p->objtype = extslice_obj;
    }
}

static int32 loadprogram	(char * filespec,int32 * filetype) {
char	cmdstr[300];
char *	pcfile;
int32	stopcode;
    if ((*filetype==17232)) {
        return loadpcfile(filespec);
    }
    else {
        strcpy((char *)&cmdstr,"/m/qc ");
        strcat((char *)&cmdstr,filespec);
        printf("%s %s\n","Invoking Q compiler",(char*)(&cmdstr));
        stopcode = system((char *)&cmdstr);
        printf("%s\n","Finished compiling");
        if (stopcode) {
            loaderror("Error compiling",filespec);
        }
        pcfile = pcm_copyheapstring(changeext(filespec,"pc"));
        return loadpcfile(pcfile);
    }
    return 0;
}

static int32 loadpcfile	(char * filespec) {
char	modulename[100];
char	name[100];
byte *	s;
char *	str;
modulerec	m;
int32	i;
int32	j;
int32	a;
int32	b;
int32	dir;
int32	symtype;
int32	x;
int32	id;
int32	t;
int32	modno;
int32	n;
int32	cmd;
int32	recordtype;
strec *	d;
int16 (*linetable)[];
int64 *	pccode;
double	xvalue;
int32	av_3;
    pcm_clearmem(&moduletable[0],94);
    moduletable[0].name = "PROGRAM";
    moduletable[0].filename = "<->";
    moduletable[0].sourcecode = "<program>";
    moduletable[0].sourcelen = strlen(moduletable[0].sourcecode);
    moduletable[0].stmodule = stprogram = createstentry(0,"$prog",0,0);
    s = readfile(filespec);
    if (s == 0) {
        printf("%s %p %s\n","S=",(void*)(s),filespec);
        loaderror("Can't load pc file:",filespec);
        return 0;
    }
    a = *s++;
    b = *s++;
    if (a != 'P' || b != 'C') {
        printf("%s\n","PC: bad sig");
        return 0;
    }
    s += 2;
    while (1) {
        dir = readzint(&s);
        switch (dir) {
        case kkpclversion:
            str = readzstring(&s,0);
            break;
        case kkmoduletable:
            nmodules = readzint(&s);
            for (i=1; i<=nmodules; ++i) {
                memset((int32 *)&m,0,94);
                m.name = pcm_copyheapstring(readzstring(&s,0));
                m.filename = m.name;
                m.sourcecode = "<no source>";
                m.sourcelen = 0;
                moduletable[i] = m;
            }
            break;
        case kkdlltable:
            ndlltable = readzint(&s);
            for (i=1; i<=ndlltable; ++i) {
                dlltable[i-1] = pcm_copyheapstring(readzstring(&s,0));
                dllinsttable[i-1] = 0;
            }
            break;
        case kkdllproctable:
            ndllproctable = readzint(&s);
            for (i=1; i<=ndllproctable; ++i) {
                dllproctable[i-1].name = pcm_copyheapstring(readzstring(&s,0));
                dllproctable[i-1].dllindex = readzint(&s);
            }
            break;
        case kksymboltable:
            nsymbols = readzint(&s);
            pcsymboltable = (strec (*)[])zalloctable(nsymbols,84);
            for (i=1; i<=nsymbols; ++i) {
                symtype = readzint(&s);
                if ((symtype=='P')) {
                    id = procid;
                } else if ((symtype=='S')) {
                    id = staticid;
                } else if ((symtype=='M')) {
                    id = moduleid;
                } else if ((symtype=='T')) {
                    id = typeid;
                }
                str = pcm_copyheapstring(readzstring(&s,0));
                x = readzint(&s);
                a = readzint(&s);
                b = readzint(&s);
                d = createstentry(i,str,x,id);
                d->index = a;
                if ((id==moduleid)) {
                    moduletable[a].stmodule = d;
                    d->attribs.ax_moduleno = a;
                } else if ((id==procid)) {
                    str = readzstring(&s,0);
                    if (!!(uchar)*str) {
                        d->metadata = pcm_copyheapstring(str);
                    }
                }
            }
            break;
        case kktypetable:
            n = readzint(&s);
            ntypes = n+tlast-1;
            for (i=1; i<=n; ++i) {
                t = readzint(&s);
                ttname[t] = pcm_copyheapstring(readzstring(&s,0));
                ttnamedef[t] = &(*pcsymboltable)[readzint(&s)-1];
                ttbasetype[t] = readzint(&s);
                tttarget[t] = readzint(&s);
                ttlower[t] = readzint(&s);
                ttlength[t] = readzint(&s);
                ttsize[t] = readzint(&s);
            }
            break;
        case kkgenfieldnames:
            ngenfieldnames = readzint(&s);
            for (i=1; i<=ngenfieldnames; ++i) {
                genfieldnames[i-1].name = pcm_copyheapstring(readzstring(&s,0));
                genfieldnames[i-1].dataindex = readzint(&s);
                genfieldnames[i-1].datalength = readzint(&s);
            }
            break;
        case kkgenfielddata:
            ngenfielddata = readzint(&s);
            for (i=1; i<=ngenfielddata; ++i) {
                genfielddata[i-1].fieldindex = readzint(&s);
                genfielddata[i-1].recordtype = readzint(&s);
                genfielddata[i-1].fieldtype = readzint(&s);
                genfielddata[i-1].offset = readzint(&s);
            }
            break;
        case kkstringtable:
            nstrings = readzint(&s);
            stringtable = (char * (*)[])alloctable(nstrings,4);
            for (i=1; i<=nstrings; ++i) {
                (*stringtable)[i-1] = pcm_copyheapstring(readzstring(&s,0));
            }
            break;
        case kkstructtable:
            nstructfields = readzint(&s);
            pcfieldtable = (fieldrec (*)[])zalloctable(nstructfields,84);
            t = 0;
            n = 0;
            for (i=1; i<=nstructfields; ++i) {
                recordtype = readzint(&s);
                if (recordtype != t) {
                    if (t) {
                        ttstructfields[t] = n;
                    }
                    t = recordtype;
                    ttstartfield[t] = i;
                    n = 0;
                }
                ++n;
                (*pcfieldtable)[i-1].recordtype = recordtype;
                (*pcfieldtable)[i-1].name = pcm_copyheapstring(readzstring(&s,0));
                (*pcfieldtable)[i-1].fieldtype = readzint(&s);
                (*pcfieldtable)[i-1].fieldoffset = readzint(&s);
            }
            if (t) {
                ttstructfields[t] = n;
            }
            break;
        case kkpccode:
            modno = readzint(&s);
            n = readzint(&s);
            moduletable[modno].linetable = (uint16 (*)[])(linetable = (int16 (*)[])zalloctable(n,2));
            moduletable[modno].pccode = (int64 (*)[])(pccode = (int64 *)zalloctable(n,8));
            moduletable[modno].pcindex = n;
            moduletable[modno].npccode = n;
            i = 0;
            while (++i <= n) {
                (*linetable)[i-1] = readzint(&s);
                cmd = readzint(&s);
                *pccode++ = cmd;
                for (j=1; j<=cmdnopnds[cmd]; ++j) {
                    ++i;
                    if ((cmdfmt[cmd][j-1]==creal)) {
                        xvalue = readzreal(&s);
                        *pccode++ = *(int64*)&xvalue;
                    }
                    else {
                        *pccode++ = readzint(&s);
                    }
                }
            }
            break;
        case kkend:
            goto L83;
            break;
        default:;
            loaderror("PCDIR?","");
        }
    }
L83:;
    return 1;
}

void initpcldata	(void) {
int32	i;
int32	j;
int32	nn;
    for (i=1; i<=klastcmd; ++i) {
        nn = 0;
        for (j=1; j<=4; ++j) {
            if (cmdfmt[i][j-1] == 0) {
                goto L141;
            }
            ++nn;
        }
L141:;
        cmdnopnds[i] = nn;
    }
}

static strec * createstentry	(int32 index,char * name,int32 owner,int32 id) {
strec *	p;
    if (index) {
        p = &(*pcsymboltable)[index-1];
    }
    else {
        p = (strec *)pcm_allocz(84);
    }
    p->name = name;
    p->nameid = id;
    if (owner) {
        p->owner = &(*pcsymboltable)[owner-1];
    }
    else {
        p->owner = stprogram;
    }
    return p;
}

static void findnewestfile	(char * * lastfilename,int64 * lastfiletime,int32 * lastfiletype,char * filename,char * fileext,int32 filetype) {
char *	file;
int64	filetime;
    file = addext(filename,fileext);
    filetime = os_filelastwritetime(file);
    if (filetime == 0) {
        return;
    }
    if (filetime > *lastfiletime) {
        *lastfilename = pcm_copyheapstring(file);
        *lastfiletime = filetime;
        *lastfiletype = filetype;
    }
}

/* Notes:

   pcc64.c is intended to be compiled as a 64-bit application. pcc32.c 32-bit
   version on request.

   To test, download, compile and run Q compiler (qcc64.c or qcc32.c) and run that on:

       proc start =
          println "Hello, World."
       end

   to produce hello.pc.

   Then use:

       pcc hello                            # Windows
       ./pcc hello                          # Linux

   to execute hello.pc.

   (pcc can automatically invoke the Q compiler, but that feature is experimental.
   It currently expects the executable to be called 'qc' (qc.exe on Windows) and 
   to be located in path /m/. If the compiler is available, then try:

       pcc hello.q

   which could run the compiler to produce hello.pc that is then executed. Normal
   version of pcc is invoked as:

       pcc hello
 
   and it will choose the most recent of hello.q, hello.pc or hello.qa to compile
   and/or run. But the OS-neutral version in pcc64.c doesn't have the time-stamp
   routines available to make it possible.)

   This composite C file was created from the multiple modules,  auto-translated
   from the implementation language ('M') into C and then collated into a
   single file.

   The original modules are named:

        mm_clib
        mm_mainc
        mm_nos
        mm_mlib
        var_types
        pq_common
        var_decls
        pc_misc
        support
        var_objects
        var_ops
        pc_bigint
        var_lib
        var_print
        mm_nosdll
        pc_oslayer
        pc_host
        pc_handlers
        pc_assem
        start
*/
