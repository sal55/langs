/* Q Compiler: compile .q source files to .pc bytecode

   Compile qcc64.c or qcc32.c C program, examples:

   gcc -m64 -O3 qcc64.c -oqcc.exe             # Windows
   gcc -m32 -O3 qcc32.c -oqcc -lm             # Linux

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

// From module: mm_nosdll

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

// From module: qc_tables
/* Named Constants */
enum {j_none = 0};
enum {j_const = 1};
enum {j_null = 2};
enum {j_name = 3};
enum {j_block = 4};
enum {j_codeblock = 5};
enum {j_blockdef = 6};
enum {j_doblock = 7};
enum {j_typeval = 8};
enum {j_longint = 9};
enum {j_whenthen = 10};
enum {j_elsif = 11};
enum {j_fmtitem = 12};
enum {j_nogap = 13};
enum {j_callproc = 14};
enum {j_callmproc = 15};
enum {j_return = 16};
enum {j_assign = 17};
enum {j_shallowcopy = 18};
enum {j_deepcopy = 19};
enum {j_to = 20};
enum {j_if = 21};
enum {j_longif = 22};
enum {j_forup = 23};
enum {j_fordown = 24};
enum {j_forstep = 25};
enum {j_forall = 26};
enum {j_forallrev = 27};
enum {j_foreach = 28};
enum {j_foreachrev = 29};
enum {j_cfor = 30};
enum {j_while = 31};
enum {j_repeat = 32};
enum {j_goto = 33};
enum {j_gotoblock = 34};
enum {j_labeldef = 35};
enum {j_restart = 36};
enum {j_redo = 37};
enum {j_next = 38};
enum {j_exit = 39};
enum {j_break = 40};
enum {j_do = 41};
enum {j_case = 42};
enum {j_docase = 43};
enum {j_switch = 44};
enum {j_doswitch = 45};
enum {j_swap = 46};
enum {j_select = 47};
enum {j_print = 48};
enum {j_println = 49};
enum {j_fprint = 50};
enum {j_fprintln = 51};
enum {j_cprint = 52};
enum {j_cprintln = 53};
enum {j_sprint = 54};
enum {j_sfprint = 55};
enum {j_scprint = 56};
enum {j_read = 57};
enum {j_readln = 58};
enum {j_sread = 59};
enum {j_sreadln = 60};
enum {j_stop = 61};
enum {j_try = 62};
enum {j_except = 63};
enum {j_yield = 64};
enum {j_raise = 65};
enum {j_callhostproc = 66};
enum {j_eval = 67};
enum {j_listcomp = 68};
enum {j_appendlc = 69};
enum {j_startiter = 70};
enum {j_nextiter = 71};
enum {j_andl = 72};
enum {j_orl = 73};
enum {j_xorl = 74};
enum {j_notl = 75};
enum {j_istruel = 76};
enum {j_makelist = 77};
enum {j_makeconstr = 78};
enum {j_makesetlist = 79};
enum {j_makerange = 80};
enum {j_makedict = 81};
enum {j_exprlist = 82};
enum {j_keyword = 83};
enum {j_keyvalue = 84};
enum {j_assignx = 85};
enum {j_deepcopyx = 86};
enum {j_callfn = 87};
enum {j_callmfn = 88};
enum {j_ifx = 89};
enum {j_selectx = 90};
enum {j_callhostfn = 91};
enum {j_applyop = 92};
enum {j_applyopx = 93};
enum {j_andand = 94};
enum {j_eq = 95};
enum {j_ne = 96};
enum {j_lt = 97};
enum {j_le = 98};
enum {j_gt = 99};
enum {j_ge = 100};
enum {j_isequal = 101};
enum {j_add = 102};
enum {j_sub = 103};
enum {j_mul = 104};
enum {j_div = 105};
enum {j_idiv = 106};
enum {j_fdiv = 107};
enum {j_ddiv = 108};
enum {j_rem = 109};
enum {j_divrem = 110};
enum {j_iand = 111};
enum {j_ior = 112};
enum {j_ixor = 113};
enum {j_shl = 114};
enum {j_shr = 115};
enum {j_in = 116};
enum {j_notin = 117};
enum {j_inrev = 118};
enum {j_min = 119};
enum {j_max = 120};
enum {j_addptr = 121};
enum {j_subptr = 122};
enum {j_concat = 123};
enum {j_append = 124};
enum {j_clamp = 125};
enum {j_index = 126};
enum {j_indexref = 127};
enum {j_slice = 128};
enum {j_keyindex = 129};
enum {j_dotindex = 130};
enum {j_dotleft = 131};
enum {j_dotright = 132};
enum {j_dotslice = 133};
enum {j_dotkeyindex = 134};
enum {j_anddotindex = 135};
enum {j_anddotslice = 136};
enum {j_byteindex = 137};
enum {j_dot = 138};
enum {j_dotref = 139};
enum {j_dotattr = 140};
enum {j_atan2 = 141};
enum {j_power = 142};
enum {j_ptr = 143};
enum {j_ptrto = 144};
enum {j_addrof = 145};
enum {j_convert = 146};
enum {j_typepun = 147};
enum {j_typeconst = 148};
enum {j_operator = 149};
enum {j_packtypeconst = 150};
enum {j_classconst = 151};
enum {j_upper = 152};
enum {j_neg = 153};
enum {j_abs = 154};
enum {j_inot = 155};
enum {j_chr = 156};
enum {j_asc = 157};
enum {j_sqrt = 158};
enum {j_sqr = 159};
enum {j_cube = 160};
enum {j_sign = 161};
enum {j_sin = 162};
enum {j_cos = 163};
enum {j_tan = 164};
enum {j_asin = 165};
enum {j_acos = 166};
enum {j_atan = 167};
enum {j_ln = 168};
enum {j_lg = 169};
enum {j_log = 170};
enum {j_exp = 171};
enum {j_round = 172};
enum {j_floor = 173};
enum {j_ceil = 174};
enum {j_fract = 175};
enum {j_fmod = 176};
enum {j_lwb = 177};
enum {j_upb = 178};
enum {j_len = 179};
enum {j_bounds = 180};
enum {j_bitwidth = 181};
enum {j_bytesize = 182};
enum {j_dictitems = 183};
enum {j_gettype = 184};
enum {j_getbasetype = 185};
enum {j_getelemtype = 186};
enum {j_isvoid = 187};
enum {j_isnone = 188};
enum {j_isdef = 189};
enum {j_isint = 190};
enum {j_isreal = 191};
enum {j_isstring = 192};
enum {j_isrange = 193};
enum {j_islist = 194};
enum {j_isrecord = 195};
enum {j_isclass = 196};
enum {j_isarray = 197};
enum {j_isset = 198};
enum {j_istype = 199};
enum {j_ispointer = 200};
enum {j_ismutable = 201};
enum {j_minvalue = 202};
enum {j_maxvalue = 203};
enum {j_min1 = 204};
enum {j_max1 = 205};
enum {j_preincrx = 206};
enum {j_predecrx = 207};
enum {j_postincrx = 208};
enum {j_postdecrx = 209};
enum {j_addto = 210};
enum {j_subto = 211};
enum {j_multo = 212};
enum {j_divto = 213};
enum {j_idivto = 214};
enum {j_fdivto = 215};
enum {j_iandto = 216};
enum {j_iorto = 217};
enum {j_ixorto = 218};
enum {j_shlto = 219};
enum {j_shrto = 220};
enum {j_minto = 221};
enum {j_maxto = 222};
enum {j_concatto = 223};
enum {j_appendto = 224};
enum {j_negto = 225};
enum {j_absto = 226};
enum {j_inotto = 227};
enum {j_preincr = 228};
enum {j_predecr = 229};
enum {j_postincr = 230};
enum {j_postdecr = 231};
enum {j_cvlineno = 232};
enum {j_cvstrlineno = 233};
enum {j_cvmodulename = 234};
enum {j_cvfilename = 235};
enum {j_cvfunction = 236};
enum {j_cvdate = 237};
enum {j_cvtime = 238};
enum {j_cvversion = 239};
enum {j_cvpclversion = 240};
enum {j_new = 241};
enum {j_mixed = 242};
enum {j_tostr = 243};
enum {j_free = 244};
enum {j_dupl = 245};
enum {j_dummy = 246};
enum {noscope = 0};
enum {localscope = 1};
enum {importscope = 2};
enum {exportscope = 3};
enum {windowsff = 1};
enum {clangff = 2};
enum {qlangff = 3};
enum {mlangff = 4};
enum {dummyff = 5};
enum {linuxff = clangff};
enum {errorsym = 1};
enum {dotsym = 2};
enum {lexdotsym = 3};
enum {anddotsym = 4};
enum {commasym = 5};
enum {semisym = 6};
enum {colonsym = 7};
enum {dcolonsym = 8};
enum {assignsym = 9};
enum {deepcopysym = 10};
enum {sendtosym = 11};
enum {lbracksym = 12};
enum {rbracksym = 13};
enum {lsqsym = 14};
enum {rsqsym = 15};
enum {lcurlysym = 16};
enum {rcurlysym = 17};
enum {ptrsym = 18};
enum {barsym = 19};
enum {dbarsym = 20};
enum {atsym = 21};
enum {datsym = 22};
enum {questionsym = 23};
enum {addrsym = 24};
enum {daddrsym = 25};
enum {poundsym = 26};
enum {curlsym = 27};
enum {gatesym = 28};
enum {rangesym = 29};
enum {ellipsissym = 30};
enum {opsym = 31};
enum {eolsym = 32};
enum {eofsym = 33};
enum {rawnamesym = 34};
enum {docstringsym = 35};
enum {incrsym = 36};
enum {intconstsym = 37};
enum {longintconstsym = 38};
enum {realconstsym = 39};
enum {charconstsym = 40};
enum {wcharconstsym = 41};
enum {stringconstsym = 42};
enum {wstringconstsym = 43};
enum {unitnamesym = 44};
enum {namesym = 45};
enum {ksourcedirsym = 46};
enum {lexmacronamesym = 47};
enum {stdtypesym = 48};
enum {packtypesym = 49};
enum {kifsym = 50};
enum {kthensym = 51};
enum {kelsifsym = 52};
enum {kelsesym = 53};
enum {kelsecasesym = 54};
enum {kelseswitchsym = 55};
enum {kelseselectsym = 56};
enum {kendsym = 57};
enum {kunlesssym = 58};
enum {kcasesym = 59};
enum {kdocasesym = 60};
enum {kwhensym = 61};
enum {kforsym = 62};
enum {kforallsym = 63};
enum {ktosym = 64};
enum {kbysym = 65};
enum {kdosym = 66};
enum {kwhilesym = 67};
enum {krepeatsym = 68};
enum {kuntilsym = 69};
enum {kreturnsym = 70};
enum {kstopsym = 71};
enum {kloopsym = 72};
enum {kbreaksym = 73};
enum {kgotosym = 74};
enum {kswitchsym = 75};
enum {kdoswitchsym = 76};
enum {kprintsym = 77};
enum {ksprintsym = 78};
enum {kreadsym = 79};
enum {ksreadsym = 80};
enum {ksreadlnsym = 81};
enum {khostfnsym = 82};
enum {kprocsym = 83};
enum {kfunctionsym = 84};
enum {kmethodsym = 85};
enum {krecordsym = 86};
enum {kstructsym = 87};
enum {kunionsym = 88};
enum {kimportsym = 89};
enum {kimportmodulesym = 90};
enum {kmodulesym = 91};
enum {ktypesym = 92};
enum {ktypeattrsym = 93};
enum {krefsym = 94};
enum {kmacrosym = 95};
enum {kconstsym = 96};
enum {kvarsym = 97};
enum {klocalssym = 98};
enum {klabelsym = 99};
enum {kenumsym = 100};
enum {knewsym = 101};
enum {kclasssym = 102};
enum {kdoblocksym = 103};
enum {kblockdefsym = 104};
enum {kdirectivesym = 105};
enum {kfflangsym = 106};
enum {kglobalsym = 107};
enum {kstaticsym = 108};
enum {kbeginsym = 109};
enum {ktrysym = 110};
enum {kexceptsym = 111};
enum {kfinallysym = 112};
enum {kraisesym = 113};
enum {kyieldsym = 114};
enum {kextendsym = 115};
enum {kblocksym = 116};
enum {kcastsym = 117};
enum {ktypeconstsym = 118};
enum {compilervarsym = 119};
enum {dollarsym = 120};
enum {kevalsym = 121};
enum {ktabledatasym = 122};
enum {kmapsym = 123};
enum {kapplyopsym = 124};
enum {kstacksym = 125};
enum {kforwardsym = 126};
enum {kclampsym = 127};
enum {kswapsym = 128};
enum {kcondcompsym = 129};
enum {kerrorsym = 130};
enum {sysconstsym = 131};
enum {kdummysym = 132};
enum {definedir = 1};
enum {emitdir = 2};
enum {ifdir = 3};
enum {elsifdir = 4};
enum {elsedir = 5};
enum {endifdir = 6};
enum {debuglinedir = 7};
enum {includedir = 8};
enum {endincludedir = 9};
enum {exportdir = 10};
enum {endexportdir = 11};
enum {commentdir = 12};
enum {endcommentdir = 13};
enum {strincludedir = 14};
enum {modulenamedir = 15};
enum {targetlangdir = 16};
enum {cincludedir = 17};
enum {pyimportdir = 18};
enum {enddir = 19};
enum {nil_const = 1};
enum {pi_const = 2};
enum {tab_const = 3};
enum {con_const = 4};
enum {thousand_unit = 1};
enum {million_unit = 2};
enum {billion_unit = 3};

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

// From module: qc_lex
/* Named Constants */
enum {diagmode = 0};
enum {maxmacrodepth = 10};
enum {etx = 26};
enum {cr = 13};
enum {lf = 10};
enum {tab = 9};
enum {hstsize = 131072};
enum {hstmask = 131071};

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

// From module: qc_lib

// From module: qc_parselib

// From module: qc_parse
/* Named Constants */
enum {maxdollarstack = 10};

// From module: qc_pcllib
/* Named Constants */
enum {maxlabels = 1000};

// From module: qc_pclgen
/* Named Constants */
enum {maxswitchrange = 512};
enum {maxloopindex = 20};
enum {maxparams = 64};
enum {maxlocals = 256};

// From module: q_libs

// From module: start
/* Named Constants */
enum {mload = 1};
enum {mloadqa = 2};
enum {mwriteqa = 4};
enum {mlex = 8};
enum {mparse = 16};
enum {mcodegen = 32};
enum {mwritepc = 64};
enum {mcompile = 64};
enum {pm_end = 0};
enum {pm_option = 1};
enum {pm_sourcefile = 2};
enum {pm_libfile = 3};
enum {load_sw = 1};
enum {lex_sw = 2};
enum {qa_sw = 3};
enum {parse_sw = 4};
enum {gen_sw = 5};
enum {writepc_sw = 6};
enum {writepc2_sw = 7};
enum {options_sw = 8};
enum {modules_sw = 9};
enum {files_sw = 10};
enum {ast_sw = 11};
enum {st_sw = 12};
enum {stflat_sw = 13};
enum {dis_sw = 14};
enum {paths_sw = 15};
enum {types_sw = 16};
enum {pcl_sw = 17};
enum {s_sw = 18};
enum {d_sw = 19};
enum {time_sw = 20};
enum {v_sw = 21};
enum {v2_sw = 22};
enum {help_sw = 23};
enum {help2_sw = 24};
enum {ext_sw = 25};
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

// From module: mm_nosdll

// From module: var_types

// From module: pq_common

// From module: qc_tables

// From module: var_decls
/* Struct Definitions */
typedef struct _uflagsrec uflagsrec;
typedef struct _attribrec attribrec;
typedef struct _strec strec;
typedef struct _unitrec unitrec;

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

typedef struct _genfieldnamerec {
    union {
        strec *	def;
        char *	name;
    };
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

typedef struct _strbuffer {
    char *	strptr;
    int32	length;
    int32	allocated;
} strbuffer;


// From module: qc_lex
/* Struct Definitions */

typedef struct _lexrec {
    union {
        int64	value;
        double	xvalue;
        uint64	uvalue;
        char *	svalue;
        strec *	symptr;
    };
    int32	symbol;
    int32	subcode;
    int32	length;
    int32	lineno;
    int32	fileno;
    int32	hashvalue;
} lexrec;


// From module: support

// From module: qc_lib

// From module: qc_parselib

// From module: qc_parse

// From module: qc_pcllib

// From module: qc_pclgen

// From module: q_libs

// From module: start
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

// From module: mm_nosdll
/* Local Function Prototypes */
       int64	os_calldll_wint	(void *,int64 (*)[],int32);
       double	os_calldll_wreal	(void *,int64 (*)[],int32);
       void	os_dummycall	(double,double,double,double);

// From module: var_types

// From module: pq_common

// From module: qc_tables

// From module: var_decls

// From module: qc_lex
/* Local Function Prototypes */
       void	lexreadtoken	(void);
static void	lxreadstring	(int32);
static void	readnumber	(int32);
static void	readrealnumber	(char *,int32,int32);
static int32	readexponent	(int32);
static void	lxerror	(char *);
       void	printsymbol	(lexrec *);
static void	stringtonumber	(char *,int32,int32,int32);
       void	lexsetup	(void);
       void	printstrn	(char *,int32);
static byte *	scannumber	(int32);
static void	readrawstring	(void);
static int32	lookup	(void);
static int32	gethashvaluez	(char *);
static void	inithashtable	(void);
static int32	dolexdirective	(int32);
static void	lexreadline	(void);
       void	startlex	(char *,char *);
       char *	convertzstring	(char *,int32);
       strec *	addnamestr	(char *);
       void	PS1	(char *);
       void	PS2	(char *);
       void	PS	(char *);
       void	lex	(void);
       void	showhashtablesize	(void);
static void	addmacro	(strec *,char *,int32);
static void	stackmacro	(char *);
static void	unstackmacro	(void);
static int32	testlookup	(void);

// From module: support
/* Local Function Prototypes */
       void	prterror	(char *);
       void	serror	(char *);
       void	gerror	(char *,unitrec *);
       void	nxerror	(char *,unitrec *);
       int32	testelem	(byte (*)[],int32);
       void	setelem	(byte (*)[],int32);
       void	inittypetables	(void);
       int32	nextpoweroftwo	(int32);
       void	initpcdest	(void);
       int32	getpcpos	(void);
       void	setpcpos	(int32);
       int32	writepcdata	(char *);
       void	writezstring	(char *);
       void	writezint	(int64);
       void	writezint4	(int32);
       void	writezrange	(byte *);
       void	writezreal	(double);
       void	writezeof	(void);
       int64	ipower	(int64,int32);
       void	gs_additem	(strbuffer *,char *);
static int32	isalphanum	(int32);
       void	strbuffer_add	(strbuffer *,char *,int32);
       void	gs_init	(strbuffer *);
       void	gs_str	(strbuffer *,char *);
       void	gs_strn	(strbuffer *,char *,int32);
       void	gs_strvar	(strbuffer *,strbuffer *);
       void	gs_strint	(strbuffer *,int64);
       void	gs_strln	(strbuffer *,char *);
       void	gs_strsp	(strbuffer *,char *);
       void	gs_line	(strbuffer *);
       int32	gs_getcol	(strbuffer *);
       void	gs_leftstr	(strbuffer *,char *,int32,int32);
       void	gs_leftint	(strbuffer *,int32,int32,int32);
       void	gs_padto	(strbuffer *,int32,int32);
       void	gs_println	(strbuffer *,void *);
static void	gs_copytostr	(strbuffer *,char *);
static void	outpcbyte	(int32);
static void	outpcword	(int32);
static void	outpcword16	(int32);

// From module: qc_lib
/* Local Function Prototypes */
static strec *	newstrec	(void);
       void	initqclib	(void);
       strec *	getduplnameptr	(strec *,strec *,int32);
       void	adddef	(strec *,strec *);
       void	adddef_nodupl	(strec *,strec *);
       void	printst	(void *,strec *,int32);
static void	printstrec	(void *,strec *,int32);
       void	printstflat	(void *);
static unitrec *	newunitrec	(void);
       unitrec *	createname	(strec *);
       unitrec *	createunit0	(int32);
       unitrec *	createunit1	(int32,unitrec *);
       unitrec *	createunit2	(int32,unitrec *,unitrec *);
       unitrec *	createunit3	(int32,unitrec *,unitrec *,unitrec *);
       unitrec *	createconstunit	(uint64,int32);
       unitrec *	createstringconstunit	(char *,int32);
       int32	getoptocode	(int32);
       int32	checkpackedtype	(int32);
       void	checkunpackedtype	(int32);
       int32	checkdlltype	(int32);
       int32	createtype	(strec *);
       int32	createusertype	(strec *);
       int32	createusertypefromstr	(char *);
       int64	getconstvalue	(unitrec *,int32);
       int32	getrangelwb	(unitrec *);
       int32	getrangeupb	(unitrec *);
       unitrec *	getrangelwbunit	(unitrec *);
       unitrec *	getrangeupbunit	(unitrec *);
       int32	createarraymode	(int32,int32,int32,int32);
       char *	nextautotype	(void);
       int32	createstringmode	(int32,int32,int32);
       int32	createrefpackmode	(int32,int32);
       int32	getscope	(strec *);
       void	setnameptr	(unitrec *);
       void	printcode_all	(void *,char *);
       void	printcode	(void *,char *,int32);
       void	printunit	(void *,unitrec *,int32,char *);
static void	printunitlist	(void *,unitrec *,int32,char *);
static char *	getprefix	(int32,char *,unitrec *);
       char *	getdottedname	(strec *);
static char *	getlineinfok	(void);
       strec *	getavname	(strec *,int32);
       void	unionstr_clear	(uflagsrec *);
       void	unionstr_append	(uflagsrec *,int32);
       void	unionstr_concat	(uflagsrec *,uflagsrec *);
       int32	unionstr_last	(uflagsrec *);
       void	unionstr_copy	(uflagsrec *,uflagsrec *);
       void	unionstr_print	(uflagsrec *);
       int32	createrecordmode	(strec *,int32,int32);
       int32	createenummode	(strec *,int32);
       void	convertstring	(char *,char *);
       strbuffer *	strexpr	(unitrec *);
static void	jeval	(strbuffer *,unitrec *);
       char *	getopcjname	(int32);
       char *	strmode	(int32,int32);
       void	istrmode	(int32,int32,char *);
       int32	countunits	(unitrec *);
       strec *	finddefstr	(strec *,char *);
static void	purgesymbol	(strec *,strec *,int32);
       void	purgesymbollist	(strec *,int32,int32);
       void	purgeprocs	(strec *,int32);
       void	purgeproc	(strec *,int32);
       void	printmodelist	(void *);
       void	printgenfieldtable	(void *,char *);
       void	addtoproclist	(strec *);

// From module: qc_parselib
/* Local Function Prototypes */
       strec *	px_typecheck	(strec *,strec *,int32);
       strec *	resolvetopname	(strec *,strec *,int32);
       void	px_name	(strec *,strec *,unitrec *,int32);
       void	evalbinop	(unitrec *,unitrec *,unitrec *);
static void	evalbinop_real	(unitrec *,unitrec *,unitrec *);
static void	makenewconst	(unitrec *,int64,int32);
       strec *	finddupl	(strec *,strec *);
       void	px_dot	(strec *,strec *,unitrec *);
       void	evalmonop	(unitrec *);
       int32	checkdict	(unitrec *);
       void	checkconstlist	(unitrec *);

// From module: qc_parse
/* Local Function Prototypes */
       int32	parsemodule	(int32);
       int32	readmoduledefs	(strec *);
static void	initparser	(void);
static void	skipsemi	(void);
static void	addalias	(strec *,strec *);
static unitrec *	makeblock	(unitrec *);
static void	convertstmtexpr	(unitrec *);
static void	checkequals	(void);
static int32	getcurrline	(void);
static int32	checkbegin	(int32);
static void	checkbeginend	(int32,int32,int32);
static void	checkend	(int32,int32,int32,int32);
static void	addgenfield	(strec *);
static void	readvardef	(strec *,int32,int32,int32);
static void	readconstdef	(strec *,int32);
static unitrec *	readexpression	(void);
static unitrec *	readfactor	(int32);
static unitrec *	readterm	(void);
static unitrec *	readlbrack	(void);
static void	addlistunit	(unitrec * *,unitrec * *,unitrec * *);
static void	addlistparam	(strec * *,strec * *,strec * *);
static unitrec *	readlsqbrack	(void);
static unitrec *	readcast	(void);
static unitrec *	readopc	(void);
static unitrec *	readsprint	(void);
static unitrec *	readsread	(void);
static unitrec *	readcompilervar	(void);
static unitrec *	readcastx	(void);
       void	checksymbol	(int32);
static int32	readtypespec	(strec *,int32);
static unitrec *	readhostparams	(unitrec *,int32);
static unitrec *	readslist	(int32,int32);
static unitrec *	readindex	(unitrec *,int32);
static unitrec *	readkeyindex	(unitrec *,int32);
static unitrec *	readdotsuffix	(unitrec *);
       int32	isconstexpr	(unitrec *);
static unitrec *	readconstexpr	(unitrec *,int32);
static int64	readconstexprvalue	(unitrec *);
static int32	readconstint	(void);
static void	readprocdef	(strec *,int32,int32);
       strec *	readprocdecl	(strec *,int32,int32);
static strec *	readparams	(strec *,int32,int32 *,int32 *);
static unitrec *	readblock	(strec *);
static unitrec *	readexecstmt	(strec *);
static unitrec *	readstmtexpr	(strec *);
static unitrec *	readcondsuffix	(unitrec *);
static unitrec *	readif	(strec *);
static unitrec *	readgoto	(strec *,int32);
static unitrec *	readunless	(strec *);
static unitrec *	readswitchcase	(strec *);
static unitrec *	readstop	(strec *);
static unitrec *	readreturn	(strec *);
static unitrec *	readdo	(strec *);
static unitrec *	readto	(strec *);
static unitrec *	readwhile	(strec *);
static unitrec *	readrepeat	(strec *);
static unitrec *	readloopcontrol	(strec *);
static unitrec *	readprint	(strec *);
static unitrec *	readread	(strec *);
static unitrec *	readtry	(strec *);
static unitrec *	readraise	(strec *);
static unitrec *	readfor	(strec *);
static unitrec *	readforall	(strec *);
       void	readtypedef	(strec *,int32);
static int32	readstructdef	(strec *,int32,int32);
       void	readstructfields	(strec *,int32);
       void	readtabledef	(strec *,int32);
       void	readclassdef	(strec *,int32);
static void	readclassbody	(strec *,int32);
static int32	readenumtype	(strec *,int32,int32);
static void	duplfield	(strec *,strec *);
       void	readrecordfields	(strec *);
static void	readimportmodule	(strec *);
static void	readimportbody	(strec *);
static strec *	createlabel	(strec *,int32);
static strec *	createprocdef	(strec *,strec *,int32,char *);
static void	createproccall	(strec *,strec *,unitrec *);
static strec *	readequivfield	(strec *);
static unitrec *	testconstruct	(unitrec *);
static void	converteqeq	(unitrec *);
static unitrec *	readapplyop	(int32);

// From module: qc_pcllib
/* Local Function Prototypes */
       void	initpcl	(int32);
       void	initpcldata	(void);
       void	initpclgen	(void);
static void	writepcl3	(int32);
static char *	writepclopnd3	(int32,int64,int32,int32);
       strbuffer *	writepccode	(char *,int32);
       void	genpc	(int32);
       void	genopnd_int	(int64);
       void	genopnd_s	(strec *);
       void	genpc_int	(int32,int64);
       void	genpc_int2	(int32,int64,int64);
       void	genpc_int4	(int32,int64,int64,int64,int64);
       void	genpc_s	(int32,strec *);
       void	genpc_str	(int32,char *,int32);
       void	genopnd_str	(char *,int32);
       void	genpc_lab	(int32,int32);
       int32	isframe_s	(strec *);
       void	converttype	(int32);
static int32	scanstruct	(int32,strec * (*)[],int32,int32 *,int32,int32);
static int32	scanrecord	(strec * (*)[],int32);
static void	shiftflagsleft	(uflagsrec *);
static void	GSTEST	(int32);

// From module: qc_pclgen
/* Local Function Prototypes */
static void	do_tag	(unitrec *);
       int32	codegen	(int32);
       void	convertalltypes	(void);
static void	scanidata	(strec *);
static void	genidata	(strec *);
static void	initgenpcl	(int32);
       void	doprogramstartup	(void);
static void	do_block	(unitrec *);
static void	do_print	(unitrec *,unitrec *,unitrec *);
static void	do_fprint	(unitrec *,unitrec *,unitrec *,unitrec *);
static void	do_read	(unitrec *,unitrec *,unitrec *);
static void	do_assign	(unitrec *,unitrec *,unitrec *);
static void	do_to	(unitrec *,unitrec *,unitrec *,unitrec *);
static void	do_while	(unitrec *,unitrec *,unitrec *,unitrec *);
static void	do_repeat	(unitrec *,unitrec *,unitrec *);
static void	do_forstep	(unitrec *,unitrec *,unitrec *,unitrec *);
static void	do_forall	(unitrec *,unitrec *,unitrec *,unitrec *);
static void	do_do	(unitrec *,unitrec *);
static void	do_cfor	(unitrec *,unitrec *,unitrec *);
static void	do_if	(unitrec *,unitrec *,unitrec *,unitrec *);
static void	do_longif	(unitrec *,unitrec *,unitrec *);
static void	do_callproc	(unitrec *,unitrec *,unitrec *);
static void	do_callhostproc	(unitrec *,unitrec *);
static void	do_return	(unitrec *,unitrec *);
static void	genstartproc	(strec *);
static void	do_procdef	(strec *);
static void	genprocentry	(strec *,int32 *,int32 *);
static void	genprocexit	(int32,int32);
static void	do_preincr	(unitrec *,unitrec *);
static void	do_exit	(unitrec *,unitrec *);
static void	do_goto	(unitrec *,unitrec *);
static void	do_switch	(unitrec *,unitrec *,unitrec *,unitrec *);
static void	do_simpleswitch	(unitrec *,unitrec *,unitrec *,unitrec *,int32,int32);
static void	do_case	(unitrec *,unitrec *,unitrec *,unitrec *);
static void	do_try	(unitrec *,unitrec *,unitrec *);
static void	do_applyop	(unitrec *,unitrec *,unitrec *,unitrec *);
static void	evalref	(unitrec *);
static int32	getpclop	(int32);
static void	genjumpl	(int32);
static int32	definelabel	(void);
static int32	createfwdlabel	(void);
static void	definefwdlabel	(int32 *);
static void	stacklooplabels	(int32 *,int32 *,int32 *,int32 *);
static void	unstacklooplabels	(void);
static int32	findlooplabel	(int32,int32);
static int32	issimpleparam	(unitrec *);
static void	genjumpcond	(int32,unitrec *,int32);
static void	gcomparejump	(int32,unitrec *,unitrec *,unitrec *,int32);
static int32	reversecond	(int32);
static void	do_convert	(int32,unitrec *);
static void	do_selectx	(unitrec *,unitrec *,unitrec *);
static void	do_calldll	(unitrec *,unitrec *,unitrec *);
static int32	islogical	(unitrec *);
static void	do_and	(unitrec *,unitrec *);
static void	do_or	(unitrec *,unitrec *);
static void	do_callptr	(unitrec *,unitrec *,unitrec *);
static void	do_callmproc	(unitrec *,unitrec *,unitrec *,int32);
static int32	checkblockreturn	(unitrec *);
static void	genfree	(int32);
static void	do_clamp	(unitrec *,unitrec *,unitrec *);
static void	do_applyopx	(unitrec *,unitrec *,unitrec *);
static void	do_calldllvar	(unitrec *,unitrec *,unitrec *);
static void	callhostfn	(int32,int32);
static void	extractparams	(strec *,strec * (*)[]);
static int32	unitstoarray	(unitrec *,unitrec * (*)[],int32);
static void	do_idiv	(unitrec *,unitrec *);
static int32	ispoweroftwo	(int32);
static void	genpushint	(uint64);

// From module: q_libs
/* Local Function Prototypes */
       char *	getintlib	(char *);

// From module: start
/* Local Function Prototypes */
       void	start	(void);
static void	do_loadmodules	(void);
static int32	loadmainmodule	(char *);
static int32	loadmodule	(char *,char *,char *,int32,int32,int32 *);
static int32	loadimport	(char *,int32 *);
static int32	readimportlist	(modulerec *,char * (*)[],byte (*)[],int32);
static void	initlogfile	(void);
static void	closelogfile	(void);
static void	initdata	(void);
static void	initsearchdirs	(void);
static char *	getmodulestr	(char *,char *);
static char *	findmodule	(char *);
static int32	checkname	(char *,int32);
static void	getinputoptions	(void);
static void	do_option	(int32,char *);
static int32	nextparam	(int32 *,char * *,char * *);
static int32	readnextfileitem	(char * *,char * *);
static int32	do_parse	(int32);
static int32	do_codegen	(int32);
static void	showast	(void);
static void	showstflat	(char *);
static void	showsttree	(void);
static void	showpcl	(char *,int32);
static void	showgenfields	(void);
static void	loaderror	(char *,char *);
static void	do_showprogdiags_pc	(char *);
static void	getsyscmdline	(void);
static void	showmodules	(void);
static void	do_compilemodules	(void);
static int32	compilemodule	(int32);
static void	starttiming	(void);
static void	showtiming	(void);
static void	do_writeqafile	(void);
static void	checkkeyword	(char *);
static int32	readinttoken	(void);
static int32	do_loadqafile	(void);
static void	showinttiming	(char *);
static void	do_writepcfile	(void);
static void	writesymbols	(int32);
static void	showpcsymbol	(strec *);
static int32	addstringtotable	(char *);
static void	writepccode2pc	(int32);
static void	writestructfields	(void);
static void	fixup_genfields	(void);
static void	showoptions	(void);
static void	showoutput	(void);
static void	showhelp	(void);

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

// From module: mm_nosdll

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

// From module: qc_tables
/* Local Static Variables */
       char *	jtagnames[247]={"j_none",
"j_const",
"j_null",
"j_name",
"j_block",
"j_codeblock",
"j_blockdef",
"j_doblock",
"j_typeval",
"j_longint",
"j_whenthen",
"j_elsif",
"j_fmtitem",
"j_nogap",
"j_callproc",
"j_callmproc",
"j_return",
"j_assign",
"j_shallowcopy",
"j_deepcopy",
"j_to",
"j_if",
"j_longif",
"j_forup",
"j_fordown",
"j_forstep",
"j_forall",
"j_forallrev",
"j_foreach",
"j_foreachrev",
"j_cfor",
"j_while",
"j_repeat",
"j_goto",
"j_gotoblock",
"j_labeldef",
"j_restart",
"j_redo",
"j_next",
"j_exit",
"j_break",
"j_do",
"j_case",
"j_docase",
"j_switch",
"j_doswitch",
"j_swap",
"j_select",
"j_print",
"j_println",
"j_fprint",
"j_fprintln",
"j_cprint",
"j_cprintln",
"j_sprint",
"j_sfprint",
"j_scprint",
"j_read",
"j_readln",
"j_sread",
"j_sreadln",
"j_stop",
"j_try",
"j_except",
"j_yield",
"j_raise",
"j_callhostproc",
"j_eval",
"j_listcomp",
"j_appendlc",
"j_startiter",
"j_nextiter",
"j_andl",
"j_orl",
"j_xorl",
"j_notl",
"j_istruel",
"j_makelist",
"j_makeconstr",
"j_makesetlist",
"j_makerange",
"j_makedict",
"j_exprlist",
"j_keyword",
"j_keyvalue",
"j_assignx",
"j_deepcopyx",
"j_callfn",
"j_callmfn",
"j_ifx",
"j_selectx",
"j_callhostfn",
"j_applyop",
"j_applyopx",
"j_andand",
"j_eq",
"j_ne",
"j_lt",
"j_le",
"j_gt",
"j_ge",
"j_isequal",
"j_add",
"j_sub",
"j_mul",
"j_div",
"j_idiv",
"j_fdiv",
"j_ddiv",
"j_rem",
"j_divrem",
"j_iand",
"j_ior",
"j_ixor",
"j_shl",
"j_shr",
"j_in",
"j_notin",
"j_inrev",
"j_min",
"j_max",
"j_addptr",
"j_subptr",
"j_concat",
"j_append",
"j_clamp",
"j_index",
"j_indexref",
"j_slice",
"j_keyindex",
"j_dotindex",
"j_dotleft",
"j_dotright",
"j_dotslice",
"j_dotkeyindex",
"j_anddotindex",
"j_anddotslice",
"j_byteindex",
"j_dot",
"j_dotref",
"j_dotattr",
"j_atan2",
"j_power",
"j_ptr",
"j_ptrto",
"j_addrof",
"j_convert",
"j_typepun",
"j_typeconst",
"j_operator",
"j_packtypeconst",
"j_classconst",
"j_upper",
"j_neg",
"j_abs",
"j_inot",
"j_chr",
"j_asc",
"j_sqrt",
"j_sqr",
"j_cube",
"j_sign",
"j_sin",
"j_cos",
"j_tan",
"j_asin",
"j_acos",
"j_atan",
"j_ln",
"j_lg",
"j_log",
"j_exp",
"j_round",
"j_floor",
"j_ceil",
"j_fract",
"j_fmod",
"j_lwb",
"j_upb",
"j_len",
"j_bounds",
"j_bitwidth",
"j_bytesize",
"j_dictitems",
"j_gettype",
"j_getbasetype",
"j_getelemtype",
"j_isvoid",
"j_isnone",
"j_isdef",
"j_isint",
"j_isreal",
"j_isstring",
"j_isrange",
"j_islist",
"j_isrecord",
"j_isclass",
"j_isarray",
"j_isset",
"j_istype",
"j_ispointer",
"j_ismutable",
"j_minvalue",
"j_maxvalue",
"j_min1",
"j_max1",
"j_preincrx",
"j_predecrx",
"j_postincrx",
"j_postdecrx",
"j_addto",
"j_subto",
"j_multo",
"j_divto",
"j_idivto",
"j_fdivto",
"j_iandto",
"j_iorto",
"j_ixorto",
"j_shlto",
"j_shrto",
"j_minto",
"j_maxto",
"j_concatto",
"j_appendto",
"j_negto",
"j_absto",
"j_inotto",
"j_preincr",
"j_predecr",
"j_postincr",
"j_postdecr",
"j_cvlineno",
"j_cvstrlineno",
"j_cvmodulename",
"j_cvfilename",
"j_cvfunction",
"j_cvdate",
"j_cvtime",
"j_cvversion",
"j_cvpclversion",
"j_new",
"j_mixed",
"j_tostr",
"j_free",
"j_dupl",
"j_dummy"
};
       char *	scopenames[4]={"noscope","localscope","importscope","exportscope"};
       char *	fflangnames[5]={"windowsff","clangff","qlangff","mlangff","dummyff"};
       char *	symbolnames[132]={"errorsym",
"dotsym",
"lexdotsym",
"anddotsym",
"commasym",
"semisym",
"colonsym",
"dcolonsym",
"assignsym",
"deepcopysym",
"sendtosym",
"lbracksym",
"rbracksym",
"lsqsym",
"rsqsym",
"lcurlysym",
"rcurlysym",
"ptrsym",
"barsym",
"dbarsym",
"atsym",
"datsym",
"questionsym",
"addrsym",
"daddrsym",
"poundsym",
"curlsym",
"gatesym",
"rangesym",
"ellipsissym",
"opsym",
"eolsym",
"eofsym",
"rawnamesym",
"docstringsym",
"incrsym",
"intconstsym",
"longintconstsym",
"realconstsym",
"charconstsym",
"wcharconstsym",
"stringconstsym",
"wstringconstsym",
"unitnamesym",
"namesym",
"ksourcedirsym",
"lexmacronamesym",
"stdtypesym",
"packtypesym",
"kifsym",
"kthensym",
"kelsifsym",
"kelsesym",
"kelsecasesym",
"kelseswitchsym",
"kelseselectsym",
"kendsym",
"kunlesssym",
"kcasesym",
"kdocasesym",
"kwhensym",
"kforsym",
"kforallsym",
"ktosym",
"kbysym",
"kdosym",
"kwhilesym",
"krepeatsym",
"kuntilsym",
"kreturnsym",
"kstopsym",
"kloopsym",
"kbreaksym",
"kgotosym",
"kswitchsym",
"kdoswitchsym",
"kprintsym",
"ksprintsym",
"kreadsym",
"ksreadsym",
"ksreadlnsym",
"khostfnsym",
"kprocsym",
"kfunctionsym",
"kmethodsym",
"krecordsym",
"kstructsym",
"kunionsym",
"kimportsym",
"kimportmodulesym",
"kmodulesym",
"ktypesym",
"ktypeattrsym",
"krefsym",
"kmacrosym",
"kconstsym",
"kvarsym",
"klocalssym",
"klabelsym",
"kenumsym",
"knewsym",
"kclasssym",
"kdoblocksym",
"kblockdefsym",
"kdirectivesym",
"kfflangsym",
"kglobalsym",
"kstaticsym",
"kbeginsym",
"ktrysym",
"kexceptsym",
"kfinallysym",
"kraisesym",
"kyieldsym",
"kextendsym",
"kblocksym",
"kcastsym",
"ktypeconstsym",
"compilervarsym",
"dollarsym",
"kevalsym",
"ktabledatasym",
"kmapsym",
"kapplyopsym",
"kstacksym",
"kforwardsym",
"kclampsym",
"kswapsym",
"kcondcompsym",
"kerrorsym",
"sysconstsym",
"kdummysym"
};
       char *	sourcedirnames[19]={"definedir",
"emitdir",
"ifdir",
"elsifdir",
"elsedir",
"endifdir",
"debuglinedir",
"includedir",
"endincludedir",
"exportdir",
"endexportdir",
"commentdir",
"endcommentdir",
"strincludedir",
"modulenamedir",
"targetlangdir",
"cincludedir",
"pyimportdir",
"enddir"
};
       char *	stnames[323]={"if",
"then",
"elsif",
"else",
"elsecase",
"elseswitch",
"case",
"docase",
"when",
"for",
"forall",
"foreach",
"to",
"downto",
"by",
"do",
"end",
"while",
"repeat",
"until",
"always",
"return",
"yield",
"stop",
"restart",
"redo",
"loop",
"next",
"exit",
"break",
"goto",
"go",
"switch",
"doswitch",
"tabledata",
"map",
"mapl",
"mapr",
"maplr",
"applyop",
"clamp",
"eval",
"$windows",
"$linux",
"print",
"println",
"fprint",
"fprintln",
"cprint",
"cprintln",
"sprint",
"sfprint",
"scprint",
"cp",
"cpl",
"read",
"readln",
"cast",
"typeconst",
"proc",
"function",
"method",
"type",
"class",
"doblock",
"blockdef",
"record",
"struct",
"union",
"ref",
"module",
"cinclude",
"pyimport",
"include",
"strinclude",
"define",
"macro",
"export",
"endexport",
"static",
"const",
"var",
"variant",
"enum",
"importdll",
"import",
"begin",
"unless",
"try",
"except",
"finally",
"raise",
"global",
"qlang",
"clang",
"mlang",
"windows",
"swap",
"void",
"int",
"word",
"real",
"refvar",
"pointer",
"range",
"longint",
"string",
"set",
"list",
"dict",
"array",
"bits",
"recordtype",
"structtype",
"int8",
"int16",
"int32",
"int64",
"i8",
"i16",
"i32",
"i64",
"real32",
"real64",
"r32",
"r64",
"sreal",
"bit",
"bit2",
"bit4",
"byte",
"u8",
"u16",
"u32",
"u64",
"word8",
"word16",
"word32",
"word64",
"stringz",
"intm",
"wordm",
"refm",
"million",
"billion",
"thousand",
"$lineno",
"$strlineno",
"$filename",
"$modulename",
"$function",
"$date",
"$time",
"$version",
"$pclversion",
"$",
"and",
"or",
"xor",
"iand",
"ior",
"ixor",
"in",
"notin",
"inrev",
"rem",
"divrem",
"min",
"max",
"not",
"inot",
"istrue",
"abs",
"$neg",
"asc",
"chr",
"sqrt",
"sqr",
"cube",
"cos",
"sin",
"tan",
"asin",
"acos",
"atan",
"atan2",
"sign",
"ln",
"log",
"lg",
"exp",
"round",
"floor",
"ceil",
"fract",
"fmod",
"len",
"lwb",
"upb",
"bounds",
"bitwidth",
"bytes",
"basetype",
"dictitems",
"elemtype",
"defined",
"isdef",
"isvoid",
"isnone",
"isint",
"isreal",
"isarray",
"isset",
"islist",
"isrecord",
"isrange",
"isstring",
"ispointer",
"ismutable",
"minvalue",
"maxvalue",
"concat",
"append",
"$free",
"$dupl",
"$mixed",
"$index",
"$dotindex",
"$convert",
"$new",
"$tostr",
"$dot",
"$dotref",
"endif",
"fi",
"endcase",
"esac",
"enddocase",
"endswitch",
"enddoswitch",
"endfor",
"endforall",
"od",
"endproc",
"endfunction",
"endmethod",
"endwhile",
"endto",
"enddo",
"endunless",
"endmodule",
"endimportmodule",
"endtry",
"endrecord",
"endclass",
"endblock",
"nil",
"con",
"tab",
"pi",
"sreadln",
"sread",
"rereadln",
"reread",
"strtoval",
"tostr",
"leftstr",
"rightstr",
"convlc",
"convuc",
"iconvlc",
"iconvuc",
"ismain",
"waitkey",
"testkey",
"execwait",
"execcmd",
"shellexec",
"system",
"makestr",
"makestrslice",
"makeref",
"new",
"newheap",
"readlines",
"heapvar",
"freeheap",
"getcmdparam",
"gethostname",
"$setpcerror",
"$setdebug",
"$test",
"ticks",
"sleep",
"random",
"findmetafunction",
"gethash",
"getos",
"gethostsize",
"iswindows",
"setmesshandler",
"$setfprintf",
"clearlist",
"loadpcl",
"runpcl",
"runtask",
"callext",
"$pcldata",
"getcstring",
"$getparam",
"makelink",
"allparams",
"stackvars",
"makeempty",
"$errorinfo",
"$setoverload",
"pc_error",
"user_error",
"type_error",
"mixedtype_error",
"divide_error",
"stopmodule_error",
"bounds_error"
};
       int32	stsymbols[323]={kifsym,
kthensym,
kelsifsym,
kelsesym,
kelsecasesym,
kelseswitchsym,
kcasesym,
kdocasesym,
kwhensym,
kforsym,
kforallsym,
kforallsym,
ktosym,
ktosym,
kbysym,
kdosym,
kendsym,
kwhilesym,
krepeatsym,
kuntilsym,
kuntilsym,
kreturnsym,
kyieldsym,
kstopsym,
kloopsym,
kloopsym,
kloopsym,
kloopsym,
kloopsym,
kbreaksym,
kgotosym,
kgotosym,
kswitchsym,
kdoswitchsym,
ktabledatasym,
kmapsym,
kmapsym,
kmapsym,
kmapsym,
kapplyopsym,
kclampsym,
kevalsym,
kcondcompsym,
kcondcompsym,
kprintsym,
kprintsym,
kprintsym,
kprintsym,
kprintsym,
kprintsym,
ksprintsym,
ksprintsym,
ksprintsym,
kprintsym,
kprintsym,
kreadsym,
kreadsym,
kcastsym,
ktypeconstsym,
kprocsym,
kfunctionsym,
kmethodsym,
ktypesym,
kclasssym,
kdoblocksym,
kblockdefsym,
krecordsym,
kstructsym,
kunionsym,
krefsym,
kdirectivesym,
kdirectivesym,
kdirectivesym,
ksourcedirsym,
ksourcedirsym,
ksourcedirsym,
ksourcedirsym,
ksourcedirsym,
ksourcedirsym,
kstaticsym,
kconstsym,
kvarsym,
kvarsym,
kenumsym,
kimportmodulesym,
kimportsym,
kbeginsym,
kunlesssym,
ktrysym,
kexceptsym,
kfinallysym,
kraisesym,
kglobalsym,
kfflangsym,
kfflangsym,
kfflangsym,
kfflangsym,
kswapsym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
stdtypesym,
unitnamesym,
unitnamesym,
unitnamesym,
compilervarsym,
compilervarsym,
compilervarsym,
compilervarsym,
compilervarsym,
compilervarsym,
compilervarsym,
compilervarsym,
compilervarsym,
dollarsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
opsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
kendsym,
sysconstsym,
sysconstsym,
sysconstsym,
sysconstsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
khostfnsym,
kerrorsym,
kerrorsym,
kerrorsym,
kerrorsym,
kerrorsym,
kerrorsym,
kerrorsym
};
       int32	stsubcodes[323]={j_if,
0,
j_if,
0,
j_case,
j_switch,
j_case,
j_docase,
0,
0,
j_forall,
j_foreach,
0,
1,
0,
0,
0,
0,
0,
0,
1,
0,
0,
0,
j_restart,
j_redo,
j_redo,
j_next,
j_exit,
j_break,
0,
1,
j_switch,
j_doswitch,
0,
0,
1,
2,
3,
0,
0,
0,
windowsff,
linuxff,
j_print,
j_println,
j_fprint,
j_fprintln,
j_cprint,
j_cprintln,
j_sprint,
j_sfprint,
j_scprint,
j_print,
j_println,
j_read,
j_readln,
j_convert,
j_typeconst,
0,
0,
0,
0,
0,
0,
0,
trecord,
tstruct,
0,
0,
modulenamedir,
cincludedir,
pyimportdir,
includedir,
strincludedir,
definedir,
definedir,
exportdir,
endexportdir,
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
qlangff,
clangff,
mlangff,
windowsff,
0,
tvoid,
tint,
tword,
treal,
trefvar,
trefvar,
trange,
tlongint,
tstring,
tset,
tlist,
tdict,
tarray,
tbits,
trecord,
tstruct,
ti8,
ti16,
ti32,
ti64,
ti8,
ti16,
ti32,
ti64,
tr32,
tr64,
tr32,
tr64,
tr32,
tbit,
tbit2,
tbit4,
tu8,
tu8,
tu16,
tu32,
tu64,
tu8,
tu16,
tu32,
tu64,
tstringz,
tintm,
twordm,
trefm,
million_unit,
billion_unit,
thousand_unit,
j_cvlineno,
j_cvstrlineno,
j_cvfilename,
j_cvmodulename,
j_cvfunction,
j_cvdate,
j_cvtime,
j_cvversion,
j_cvpclversion,
0,
j_andl,
j_orl,
j_xorl,
j_iand,
j_ior,
j_ixor,
j_in,
j_notin,
j_inrev,
j_rem,
j_divrem,
j_min,
j_max,
j_notl,
j_inot,
j_istruel,
j_abs,
j_neg,
j_asc,
j_chr,
j_sqrt,
j_sqr,
j_cube,
j_cos,
j_sin,
j_tan,
j_asin,
j_acos,
j_atan,
j_atan2,
j_sign,
j_ln,
j_log,
j_lg,
j_exp,
j_round,
j_floor,
j_ceil,
j_fract,
j_fmod,
j_len,
j_lwb,
j_upb,
j_bounds,
j_bitwidth,
j_bytesize,
j_getbasetype,
j_dictitems,
j_getelemtype,
j_isdef,
j_isdef,
j_isvoid,
j_isnone,
j_isint,
j_isreal,
j_isarray,
j_isset,
j_isarray,
j_isrecord,
j_isrange,
j_isstring,
j_ispointer,
j_ismutable,
j_minvalue,
j_maxvalue,
j_concat,
j_append,
j_free,
j_dupl,
j_mixed,
j_index,
j_dotindex,
j_convert,
j_new,
j_tostr,
j_dot,
j_dotref,
kifsym,
kifsym,
kcasesym,
kcasesym,
kdocasesym,
kswitchsym,
kdoswitchsym,
kforsym,
kforallsym,
kdosym,
kprocsym,
kfunctionsym,
kmethodsym,
kwhilesym,
ktosym,
kdosym,
kunlesssym,
kmodulesym,
kimportmodulesym,
ktrysym,
krecordsym,
kclasssym,
kblocksym,
nil_const,
con_const,
tab_const,
pi_const,
host_sreadln,
host_sread,
host_rereadln,
host_reread,
host_strtoval,
host_tostr,
host_leftstr,
host_rightstr,
host_convlc,
host_convuc,
host_iconvlc,
host_iconvuc,
host_ismain,
host_waitkey,
host_testkey,
host_execwait,
host_execcmd,
host_shellexec,
host_system,
host_makestr,
host_makestrslice,
host_makeref,
host_new,
host_newheap,
host_readlines,
host_heapvar,
host_freeheap,
host_getcmdparam,
host_gethostname,
host_setpcerror,
host_setdebug,
host_test,
host_ticks,
host_sleep,
host_random,
host_findmetafunction,
host_gethash,
host_getos,
host_gethostsize,
host_iswindows,
host_setmesshandler,
host_setfprintf,
host_clearlist,
host_loadpcl,
host_runpcl,
host_runtask,
host_callext,
host_pcldata,
host_getcstring,
host_getparam,
host_makelink,
host_allparams,
host_stackvars,
host_makeempty,
host_errorinfo,
host_setoverload,
pc_error,
user_error,
type_error,
mixedtype_error,
divide_error,
stopmodule_error,
bounds_error
};
       int32	oplist[36]={j_add,
j_sub,
j_mul,
j_div,
j_idiv,
j_rem,
j_divrem,
j_andl,
j_orl,
j_xorl,
j_iand,
j_ior,
j_ixor,
j_shl,
j_shr,
j_in,
j_notin,
j_inrev,
j_eq,
j_ne,
j_lt,
j_ge,
j_le,
j_gt,
j_isequal,
j_min,
j_max,
j_power,
j_atan2,
j_addptr,
j_subptr,
j_concat,
j_append,
j_assignx,
j_deepcopyx,
j_makerange
};
       int32	oppriolist[36]={4,
4,
3,
3,
3,
3,
3,
7,
8,
6,
4,
4,
4,
3,
3,
6,
6,
6,
6,
6,
6,
6,
6,
6,
6,
4,
4,
2,
3,
4,
4,
4,
4,
1,
1,
5
};
       byte	jtagpriotable[247];
       int32	D_exprstarterset[23]={lbracksym,
lsqsym,
ptrsym,
addrsym,
opsym,
namesym,
incrsym,
intconstsym,
longintconstsym,
realconstsym,
charconstsym,
stringconstsym,
stdtypesym,
ksprintsym,
ksreadsym,
ksreadlnsym,
knewsym,
dollarsym,
compilervarsym,
kclampsym,
khostfnsym,
kapplyopsym,
kerrorsym
};
       int32	D_typestarterset[6]={stdtypesym,lsqsym,kvarsym,krefsym,kenumsym,krecordsym};
       byte	hostlvset[72];
       byte	condopset[247];

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
       byte	exprstarterset[132];
       modulerec	moduletable[51];
       int32	nmodules;
       char *	pendingmodules[50];
       int32	npendingmodules;
       int32	currmoduleno;
       modulerec *	currmodule;
       char *	inputfiles[51];
       int32	ninputfiles;
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
       int32	totalstrings=0;
       char *	dispatchnames[4]={"-LAB","-FN","-DEB","-ASM"};
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
       fieldrec (*pcfieldtable)[];
       int32	runfrompc=0;
       char *	extrafiles[10];
       char *	extratext[10];
       int32	extrasizes[10];
       int32	nextra;
       char *	err_message;
       int64 *	stopseq;
       int64 *	raiseseq;
       int32	NSTRINGEQ=0;
       int32	NSTRINGCMP=0;
       byte *	PPP;
       int32	progstart;
       int32	progend;
       int32	loadstart;
       int32	loadend;
       int32	parsestart;
       int32	parseend;
       int32	namesstart;
       int32	namesend;
       int32	genstart;
       int32	genend;

// From module: qc_lex
/* Local Static Variables */
       lexrec	lx;
       lexrec	nextlx;
static byte *	macrostack[10];
static int32	macrolevel=0;
static byte *	lxstart;
static byte *	lxsptr;
static strec *	lxsymptr;
       strec	hashtable[131072];
static char *	maxnumlist[16]={"",
"1111111111111111111111111111111111111111111111111111111111111111",
"11112220022122120101211020120210210211220",
"33333333333333333333333333333333",
"2214220303114400424121122430",
"3520522010102100444244423",
"45012021522523134134601",
"1777777777777777777777",
"145808576354216723756",
"18446744073709551615",
"335500516A429071284",
"839365134A2A240713",
"219505A9511A867B72",
"8681049ADB03DB171",
"2C1D56B648C6CD110",
"FFFFFFFFFFFFFFFF"
};
static int32	maxnumlen[16];

// From module: support
/* Local Static Variables */
       byte	bytemasks[8]={1,2,4,8,16,32,64,128};
static byte *	pcstart;
static byte *	pcdest;
static byte *	pcend;
static int32	pcalloc;

// From module: qc_lib
/* Local Static Variables */
static int32	autotypeno=0;
static int32	currlineno;
       int32	nextavindex=0;
static int32	nextsvindex=0;
static strbuffer	exprstrvar;
static strbuffer *	exprstr=&exprstrvar;
static int32	opc_codes[35]={j_add,
j_sub,
j_mul,
j_div,
j_idiv,
j_neg,
j_eq,
j_ne,
j_lt,
j_le,
j_gt,
j_ge,
j_iand,
j_ior,
j_ixor,
j_inot,
j_shl,
j_shr,
j_andl,
j_orl,
j_notl,
j_min1,
j_max1,
j_addto,
j_subto,
j_multo,
j_divto,
j_negto,
j_shlto,
j_shrto,
j_preincrx,
j_postincrx,
j_predecrx,
j_postdecrx,
0
};
static char *	opc_names[35]={"+",
"-",
"*",
"/",
"%",
"-",
"=",
"<>",
"<",
"<=",
">",
">=",
"iand",
"ior",
"ixor",
"inot",
"<<",
">>",
"and",
"or",
"not",
"min",
"max",
"+:=",
"-:=",
"*:=",
"/:=",
"-:=",
"<<:=",
">>:=",
"++",
"++",
"--",
"--",
""
};

// From module: qc_parselib
/* Local Static Variables */
static int32	typeallowed=0;

// From module: qc_parse
/* Local Static Variables */
static int32	NPROCS;
static int32	intabledata=0;
static int32	inreadprint=0;
static int32	inparamlist=0;
static int32	inrecordbody=0;
static int32	inimportmodule=0;
static int32	labelseen=0;
static char *	tabledataname=0;
static uflagsrec	unionstring;
static uflagsrec	unionpend;
static strec *	unionlastvar=0;
static strec *	currproc;
static int32	try_level=0;
static int32	varattribs=0;
static unitrec *	dollarstack[10];
static int32	ndollar=0;

// From module: qc_pcllib
/* Local Static Variables */
       int64 (*pccode)[];
       int32	npccode=0;
       int32	pcindex;
       uint16 (*linetable)[];
static byte (*labelmap)[];
       int64 *	lastopc;
       int32	cmdnopnds[217];
       int32	labeltable[1000];
       int32	nextfreelabel;
static int32	nfields;
static int32	nallfields;
       int32	nconvertedtypes=52;
       strec *	stcurrproc;
static strbuffer	pclv;
       strbuffer *	pcl=&pclv;

// From module: qc_pclgen
/* Local Static Variables */
static int32	nprocframevars;
static strec *	stretval;
static int32	retindex;
static int32	nprocparamvars;
static int32 *	loopstack[20][4];
static int32	trylevelstack[20];
static int32	loopindex=0;
static int32	looptrylevel;
static int32	trylevel=0;
static strec *	st_startproc;
static int32	pcl_jcodes[100]={j_add,
j_sub,
j_mul,
j_div,
j_idiv,
j_rem,
j_divrem,
j_eq,
j_ne,
j_lt,
j_le,
j_gt,
j_ge,
j_isequal,
j_iand,
j_ior,
j_ixor,
j_inot,
j_shl,
j_shr,
j_in,
j_notin,
j_inrev,
j_min,
j_max,
j_power,
j_atan2,
j_concat,
j_append,
j_neg,
j_abs,
j_notl,
j_istruel,
j_lwb,
j_upb,
j_len,
j_bounds,
j_isvoid,
j_isdef,
j_isint,
j_isreal,
j_isarray,
j_isrange,
j_isstring,
j_isrecord,
j_isset,
j_ispointer,
j_ismutable,
j_gettype,
j_getbasetype,
j_getelemtype,
j_bitwidth,
j_bytesize,
j_minvalue,
j_maxvalue,
j_chr,
j_asc,
j_sqr,
j_cube,
j_sqrt,
j_sign,
j_sin,
j_cos,
j_tan,
j_asin,
j_acos,
j_atan,
j_ln,
j_lg,
j_log,
j_exp,
j_round,
j_ceil,
j_fract,
j_floor,
j_addto,
j_subto,
j_multo,
j_divto,
j_negto,
j_iandto,
j_iorto,
j_ixorto,
j_shlto,
j_shrto,
j_minto,
j_maxto,
j_concatto,
j_appendto,
j_preincrx,
j_postincrx,
j_predecrx,
j_postdecrx,
j_tostr,
j_mixed,
j_new,
j_convert,
j_index,
j_dotindex,
0
};
static byte	pcl_nopnds[100]={2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
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
1,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
2,
1,
1,
1,
1,
1,
1,
1,
1,
2,
2,
0
};
static int32	pcl_kcodes[100]={kadd,
ksub,
kmul,
kdiv,
kidiv,
krem,
kdivrem,
keq,
kne,
klt,
kle,
kgt,
kge,
kisequal,
kiand,
kior,
kixor,
kinot,
kshl,
kshr,
kin,
knotin,
kinrev,
kmin,
kmax,
kpower,
katan2,
kconcat,
kappend,
kneg,
kabs,
knot,
kistrue,
klwb,
kupb,
klen,
kbounds,
kisvoid,
kisdef,
kisint,
kisreal,
kisarray,
kisrange,
kisstring,
kisrecord,
kisset,
kispointer,
kismutable,
ktype,
kbasetype,
kelemtype,
kbits,
kbytes,
kminval,
kmaxval,
kchr,
kasc,
ksqr,
kcube,
ksqrt,
ksign,
ksin,
kcos,
ktan,
kasin,
kacos,
katan,
kln,
klg,
klog,
kexp,
kround,
kceil,
kfract,
kfloor,
kaddto,
ksubto,
kmulto,
kdivto,
knegto,
kiandto,
kiorto,
kixorto,
kshlto,
kshrto,
kminto,
kmaxto,
kconcatto,
kappendto,
kincrload,
kloadincr,
kdecrload,
kloaddecr,
ktostr,
kmixed,
knew,
khardconv,
kpushix,
kpushdotix,
0
};
static int32	noperands;

// From module: q_libs
/* Local Static Variables */
static char *	l_sys="global type rkey=struct\t!key info as it's used locally\r\n\tword16\tcharcode\r\n\tbyte\tkeycode\r\n\tbyte\tshift\r\nend\r\n\r\nglobal var ncmdparams\r\nglobal var cmdparams\r\nglobal var stclock=0\r\n\r\nproc start=\t\t\t!START\r\n\r\nncmdparams:=getcmdparam()-1\r\ncmdparams:=new(list,0..ncmdparams)\r\n\r\nfor i:=0 to ncmdparams do\r\n\tcmdparams[i]:=getcmdparam(i)\r\nod\r\n\r\ncmdparams[0]:=gethostname()\r\nend\r\n\r\nproc main=\r\nCPL \"---------SYS MAIN\"\r\nstart()\r\n\r\nfiles:=cmd_getparams()\r\nswitches:=cmd_getswitches()\r\n\r\ncpl =files\r\ncpl =switches\r\n\r\nend\r\n\r\nglobal proc reporterror(m)=\t\t!REPORTERROR\r\nprintln \"Error: <<\",m,\">>\"\r\nprint \"Press key:\";waitkey()\r\n!stop\r\n\r\n!messagebox(0,m,\"Error\",0x20000)\t!flag sets mb_setforeground (make sure window is active)\r\n!end\r\n!\r\n!global proc showmessage(m)=\t\t!SHOWMESSAGE\r\n!!println \"Error: \",m\r\n!!stop\r\n!\r\n!messagebox(0,m,\"Message\",0x20000)\r\nend\r\n\r\nglobal function splitstring(s,sep)=\t\t!SPLITSTRING\r\n!split up the string s into strings separated by the sep sequence\r\n!return a list of all the individual strings, excluding the sep seq\r\n\r\na:=()\r\nns:=0\r\nif s=\"\" or sep=\"\" then return (s,) fi\r\ndo\r\n\tn:=sep in s\r\n\tif n=0 then\r\n\t\ta[++ns]:=s\r\n\t\treturn a\r\n\tfi\r\n\tt:=leftstr(s,n-1)\r\n\ta[++ns]:=t\r\n\ts:=rightstr(s,-(n+sep.len-1))\r\nod\r\nreturn \"\"\r\nend\r\n\r\nglobal function joinstrings(a,sep)=\t\t!JOINSTRINGS\r\n!join the strings in list, using the given separator string\r\n!return new single string\r\nif a.upb=0 then return \"\" fi\r\ns:=a[1]\r\nfor i:=2 to a.upb do\r\n\ts:=s+sep+a[i]\r\nod\r\nreturn s\r\nend\r\n\r\nglobal proc abort(s)=\t\t!ABORT\r\nprintln \"Abort:\",s,\"Error\"\r\n!messagebox(0,\"Abort \"+s,\"Error\",0x30+0x20000)\r\nwaitkey()\r\nstop\r\nend\r\n\r\nglobal function extractpath(fs)=\t\t!EXTRACTPATH\r\n!fs is a full filespec string\r\n!extract any path from it and return that; ie, strip the filename\r\n!otherwise return \"\"\r\nl:=fs.len\r\nfor i:=l downto 1 do\r\n\tif chr(fs.[i]) in \"\\\\/:\" then\r\n\t\treturn leftstr(fs,i)\r\n\tfi\r\nod\r\nreturn \"\"\r\nend\r\n\r\nglobal function extractfile(fs)=\t\t!EXTRACTFILE\r\n!return filename portion of path fs\r\np:=extractpath(fs)\r\nif p=\"\" then return fs fi\r\nreturn rightstr(fs,-p.len)\r\nend\r\n\r\nglobal function extractbasefile(fs)=\t\t!EXTRACTBASEFILE\r\n!return filename portion of path fs\r\nf:=extractfile(fs)\r\nif f=\"\" then return \"\" fi\r\ne:=extractext(f)\r\nif e.len then\r\n\tf:=leftstr(f,-e.len)\r\nfi\r\nif rightstr(f)=\".\" then\r\n\tf:=leftstr(f,-1)\r\nfi\r\nreturn f\r\nend\r\n\r\nglobal function extractext(fs,?period)=\t\t!EXTRACTEXT\r\n!extract extension part of filespec fs\r\n!endings of \"xxx\" (no extension) and \"xxx.\" both return \"\"\r\n!with period=1, then \"xxx\" returns \"\" and \"xxx.\" returns . (so can be used to\r\n!override default extensions)\r\n\r\nf:=extractfile(fs)\r\nif f=\"\" then return \"\" fi\r\ne:=\"\"\r\ndo\r\n\tn:=\".\" in f\r\n\tif n then\r\n\t\te:=rightstr(f,-n)\r\n\t\tif e=\"\" then\t\t!. ending\r\n\t\t\treturn (period.defined and period|\".\"|\"\")\r\n\t\tfi\r\n\r\n\t\tf:=e\r\n\telse\r\n\t\texit\r\n\tfi\r\nod\r\n\r\nreturn e\r\nend\r\n\r\nglobal function changeext(file,newext,?soft)=\t\t!CHANGEEXT\r\n!normally face a change of extension to the file spec\r\n!use soft=1 to only change extension if no extension is present (a \".\" ending is an extension)\r\n\r\nif not soft.defined then soft:=0 fi\r\n\r\next:=extractext(file)\r\n\r\np:=extractpath(file)\r\nbf:=extractbasefile(file)\r\nep:=extractext(file,1)\r\n\r\nif soft and ep<>\"\" then return file fi\t\t!has extension, don't change!\r\n\r\n!if newext=\"\" or newext=\".\" then\r\nif newext=\"\" then\r\n\treturn p+bf\r\nelsif leftstr(newext)=\".\" then\r\n\treturn p+bf+newext\r\nelse\r\n\treturn p+bf+\".\"+newext\r\nfi\r\nend\r\n\r\nglobal function addpath(path,file)=\t\t!ADDPATH\r\n\r\nif leftstr(file) in \"/\\\\.\" or file.len>=2 and file.[2]=\":\" then\r\n\treturn file\r\nfi\r\nreturn path+file\r\nend\r\n\r\nglobal function addext(file,ext)=\t\t!ADDEXT\r\n!add extension to filename, if it doesn't already have it's own extenstion\r\n\r\nif extractext(file,1)=\"\" then\r\n\treturn changeext(file,ext)\r\nfi\r\nreturn file\r\nend\r\n\r\nglobal function replacestr (s,a,b)=\t\t!REPLACESTR\r\n!if string a exists in s, then replace with b\r\n!return original or modified s\r\nn:=a in s\r\nif not n then return s fi\r\nreturn leftstr(s,n-1)+b+rightstr(s,1-n-a.len)\r\nend\r\n!=========================================\r\n\r\nglobal function parsecmdparams(cmd)=\t\t!PARSECMDPARAMS\r\n!cmd consists of:\r\n!blocks of text separated by whitespace or commas\r\n!each block is one of these formats\r\n! ...\t\tparam only\r\n! /...\t\tconsists of switches only\r\n! .../...\tparam followed by switches\r\n!return of (params,switches), where each is a list of strings\r\n!note that any correspondence between params and switches is lost; all switches assumed\r\n!to be global, but can appear anywhere\r\n!NOTE: cmd can also already be a list of blocks\r\n\r\n!CPL \"COMMAND=\",cmd,CMD.ISARRAY\r\n\r\nif cmd.isarray then\r\n\tblocks:=cmd\r\nelse\r\n\tsreadln(cmd)\r\n\tblocks:=()\r\n\tdo\r\n\t\tread a:\"s\"\r\n\t\tif a=\"\" then exit fi\r\n\t\tblocks append:=a\r\n\tod\r\nfi\r\n\r\nparams:=()\r\nswitches:=()\r\n\r\nforall x in blocks do\r\n\tn:=\"/\" in x\r\n\tif n=0 then\t\t!pure param\r\n\t\tparams append:=x\r\n\telsif n=1 then\t\t!pure switches\r\n\t\tswitches concat:=splitstring(convlc(rightstr(x,-1)),\"/\")\r\n\telse\t\t\t!param followed by switches\r\n\t\tparams append:=leftstr(x,n-1)\r\n\t\tswitches concat:=splitstring(convlc(rightstr(x,-n)),\"/\")\r\n\tfi\r\nod\r\n\r\nreturn (params,switches)\r\nend\r\n\r\nglobal proc waitsec(secs)=\t\t!WAITSEC\r\nsleep(int(secs*1000))\r\nend\r\n\r\nglobal function cmd_getswitches=\t\t!CMD_GETSWITCHES\r\n!params is a list of strings, which can start with \"/\" or not\r\n!read all switches, and return a list of switch names (minus the \"/\")\r\n!each string can have more than one switch\r\n!some switches can follow a name in a string\r\n\r\nswitches:=()\r\nfor i:=2 to cmdparams.upb do\t\t!use 1..len in case called on <cmdparams> which has lwb 0\r\n!CPL I,\":\",CMDPARAMS[I]\r\n\tsws:=readswitches(cmdparams[i])\r\n\r\n\tif sws then\r\n\t\tswitches concat:=sws\r\n\tfi\r\nod\r\nreturn switches\r\nend\r\n\r\nfunction readswitches(pm)=\t\t!READSWITCHES\r\n!pm is a single string\r\n!extract any /switches\r\n\r\nif leftstr(pm)=\"-\" then\r\n\ts:=\"-\"\r\nelse\r\n\ts:=\"/\"\r\nfi\r\n\r\nskipfirst:=leftstr(pm)<>s\r\n\r\nsw::=splitstring(pm,s)\r\n\r\nswitches:=()\r\n\r\nforall i,x in sw do\r\n\tif x and not (i=1 and skipfirst) then\r\n\t\tswitches append:=convlc(x)\r\n\tfi\r\nod\r\n\r\nreturn switches\r\nend\r\n\r\nglobal function cmd_getparams=\t\t!CMD_GETPARAMS\r\n!params is a list of strings\r\n!return list of actual params, not including any switches\r\n!switches are read separately using cmd_getswitches, but are not associated with\r\n!specific params. That would need to be done here (when / is detected in the middle\r\n!of a param, then make use readswitches. But to return that info, may be best to\r\n!create a parallel function)\r\n\r\ncmds:=()\r\n\r\nfor i:=2 to cmdparams.upb do\r\n\tpm:=cmdparams[i]\r\n!forall pm in params do\r\n\tif leftstr(pm)=\"/\" then\r\n\t\tnext\r\n\tfi\r\n\tn:=\"/\" in pm\r\n\tif n=0 then\r\n\t\tcmds append:=pm\r\n\telse\r\n\t\tcmds append:=leftstr(pm,n-1)\r\n\tfi\r\nod\r\nreturn cmds\r\nend\r\n\r\nglobal function starttimer=\t\t!STARTTIMER\r\nreturn stclock:=ticks()\r\nend\r\n\r\nglobal function stoptimer=\t\t!STOPTIMER\r\nreturn ticks()-stclock\r\nend\r\n\r\nglobal function bnfact(n)=\t\t!BNFACT\r\n!n is limited to 9 million million million\r\n\r\nif n<=2 then\r\n\treturn longint(n)\r\nfi\r\n\r\nf:=1L\r\ng:=2L\r\nto n-1 do\r\n\tf:=f*g\r\n\tg:=g+1\r\n\r\nod\r\nreturn f\r\nend\r\n\r\nglobal proc isort(a,?ll,?rr)=\r\n\r\nif ll.isvoid then\r\n\tll:=a.lwb\r\n\trr:=a.upb\r\nfi\r\n\r\ni:=ll\r\nj:=rr\r\n\r\npivot:=a[(ll+rr)%2]\r\n\r\nrepeat\r\n\twhile pivot>a[i] and i<rr do ++i od\r\n\twhile pivot<a[j] and j>ll do --j od\r\n\tif i<=j then\r\n\t\tswap(a[i],a[j])\r\n\t\t++i\r\n\t\t--j\r\n\tfi\r\nuntil i>j\r\nif ll<j then isort(a,ll,j) fi\r\nif i<rr then isort(a,i,rr) fi\r\nend\r\n\r\nglobal function sort(a)=\r\nb::=a\r\nisort(b)\r\nreturn b\r\nend\r\n\r\n!global proc pcerror(m)=\r\nglobal function pcerror(m)=\r\n\tprintln \"Internal error:\",m\r\n\ta:=b+c\r\n\treturn 0\r\nend\r\n\r\n!=========================================\r\nglobal proc insert(&a, b,var c)=\t\t!INSERT\r\n!insert value c just before index b\r\n!c is always a single value; to insert a sequence c, use insertn()\r\nn:=a.upb\r\na[n+1]:=c\r\nfor i:=n downto b do\r\n\tswap(a[i+1],a[i])\r\nod\r\nend\r\n\r\nglobal proc isort2(a,b,?ll,?rr)=\r\n\r\nif ll.isvoid then\r\n\tll:=a.lwb\r\n\trr:=a.upb\r\nfi\r\n\r\ni:=ll\r\nj:=rr\r\n\r\npivot:=a[(ll+rr)%2]\r\n\r\nrepeat\r\n\twhile pivot>a[i] and i<rr do ++i od\r\n\twhile pivot<a[j] and j>ll do --j od\r\n\tif i<=j then\r\n\t\tswap(a[i],a[j])\r\n\t\tswap(b[i],b[j])\r\n\t\t++i\r\n\t\t--j\r\n\tfi\r\nuntil i>j\r\nif ll<j then isort2(a,b,ll,j) fi\r\nif i<rr then isort2(a,b,i,rr) fi\r\nend\r\n\r\nglobal function left(a,n=1)=\r\n#return leftmost n elements of a (default left element)\r\n#when n is negative, all except rightmost -n\r\n\r\nif n>=0 then\r\n\treturn take(a,n)\r\nelse\r\n\treturn take(a,a.len+n)\r\nfi\r\nend\r\n\r\nglobal function right(a,n=1)=\r\n#return rightmost n elements of a (default right element)\r\n#when n is negative, all except leftmost -n\r\n\r\nif n>=0 then\r\n\treturn drop(a,a.len-n)\r\nelse\r\n\treturn drop(a,-n)\r\nfi\r\nend\r\n\r\nglobal function reverse(a)=\r\n#return reversed version of a\r\n#when 0, returns empty\r\n#when 1 element, returns a distinct, writeable copy\r\n\r\nif a.len=0 then\r\n\treturn makeempty(a)\r\nfi\r\nb::=a\r\nif a then\r\n\tfor i in a do\r\n\t\tb[a.upb-i+a.lwb]:=a[i]\r\n\tod\r\nfi\r\nreturn b\r\nend\r\n\r\nproc rotate(&a,middle)=\r\n\r\n\tfirst:=a.lwb\r\n\tlastx:=a.upb+1\r\n\tnxt:=middle\r\n\r\n\twhile first<>nxt do\r\n\t\tswap(a[first++],a[nxt++])\r\n\t\tif nxt=lastx then\r\n\t\t\tnxt:=middle\r\n\t\telsif first=middle then\r\n\t\t\tmiddle:=nxt\r\n\t\tfi\r\n\tod\r\nend\r\n\r\nfunction rotate2(a,middle)=\r\nreturn right(a,-middle) concat left(a,middle)\r\nend\r\n\r\nglobal function expandrange(a,step=1)=\r\n\r\nx:=()\r\ni:=a.lwb\r\nwhile i<=a.upb do\r\n\tx append:=i\r\n\ti+:=step\r\nod\r\nreturn x\r\nend\r\n\r\n\r\nglobal function head(a)=\r\n#return first element, or empty when empty\r\n\r\nif a.len then\r\n\treturn a[a.lwb]\r\nelse\r\n\treturn makeempty(a)\r\nfi\r\nend\r\n\r\n\r\nglobal function tail(a)=\r\n#return all except the first element\r\n#returns empty when only 0 or 1 elements\r\n\r\ncase a.len\r\nwhen 0,1 then\r\n\treturn makeempty(a)\r\nesac\r\nreturn a[2..$]\r\nend\r\n\r\nglobal function init(a)=\r\n#return all except last element\r\n#returns empty when only 0 or 1 elements\r\ncase a.len\r\nwhen 0,1 then\r\n\treturn makeempty(a)\r\nesac\r\nreturn a[a.lwb..$-1]\r\nend\r\n\r\nglobal function last(a)=\r\n#return last element, or empty\r\nif a.len then\r\n\treturn a[$]\r\nelse\r\n\treturn makeempty(a)\r\nfi\r\nend\r\n\r\nglobal function take(a,n)=\r\n#return first n elements from list/string a\r\n#returns () or \"\" when a is empty\r\n#n >= 0 (n<=0 returns empty)\r\n\r\nif a.len=0 or n<=0 then\r\n\treturn makeempty(a)\r\nfi\r\nif n>=a.len then\r\n\treturn a\r\nfi\r\nreturn a[a.lwb..a.lwb+n-1]\r\nend\r\n\r\nglobal function drop(a,n)=\r\n#skips first n elements of a then returns the rest\r\n#returns () when empty, or skipping the whole list\r\n#n >= 0\r\n\r\nif a.len=0 or n>=a.len then\r\n\treturn makeempty(a)\r\nfi\r\nif n<=0 then\r\n\treturn a\r\nfi\r\nreturn a[a.lwb+n..$]\r\nend\r\n\r\nglobal function zip(a,b)=\r\n#return a list consisting of alternate elements from a and b\r\n#uses smaller of the two dimensions\r\n\r\nn:=min(a.len,b.len)\r\nc:=()\r\nj:=a.lwb; k:=b.lwb\r\nto n do\r\n\tc append:=a[j++]\r\n\tc append:=b[k++]\r\nod\r\nreturn c\r\nend\r\n!=========================================\r\n\r\nglobal function repeatlist(a,n)=\r\n#duplicate a n times, and return the result\r\n#this ought to be built-in as a*n, but that's only implemented for a.len=1\r\n\r\nb:=makeempty(a)\r\nto n do\r\n\tb concat:=a\r\nod\r\nreturn b\r\nend\r\n\r\nglobal function minimum(a)=\r\nif not a then\r\n\treturn void\r\nfi\r\nx:=head(a)\r\nforall y in tail(a) do\r\n\tx min:=y\r\nod\r\nreturn x\r\nend\r\n\r\nglobal function maximum(a)=\r\nif not a then\r\n\treturn void\r\nfi\r\nx:=head(a)\r\nforall y in tail(a) do\r\n\tx max:=y\r\nod\r\nreturn x\r\nend\r\n\r\nglobal function sumlist(a)=\r\n# apply \"+\" between all elements of a, and return result\r\n# all elements must be compatble (all strings or all numbers for example)\r\n# returns void then a is empty, or head(a) when just one element\r\n\r\nif not a then\r\n\treturn void\r\nfi\r\nx:=head(a)\r\nforall y in tail(a) do\r\n\tx +:=y\r\nod\r\nreturn x\r\nend\r\n\r\nglobal function prepend(x,a)=\r\n#return a but with x inserted as the first element\r\nreturn (x,) concat a\r\nend\r\n\r\nglobal proc delete(&a,?b)=\t\t!DELETE\r\n!delete element b\r\nn:=a.upb\r\nif b.isvoid then b:=n fi\r\n\r\nif n=b=1 then\r\n\ta:=()\r\n\treturn\r\nfi\r\n\r\nif b>n then return fi\r\nif b<a.lwb then return fi\r\nfor i:=b to n-1 do\r\n\tswap(a[i],a[i+1])\t\t\t!swap is faster for complex elements\r\nod\r\na[n]:=0\t\t!don't leave any heap data beyond new end of list\r\n\r\nresize(a,n-1)\r\nend\r\n\r\nglobal proc resize(&a,n)=\r\n!change the upper bound of a to n\r\n\r\nif n<a.lwb then\r\n\ta:=makeempty(a)\r\n\treturn\r\nfi\r\n\r\na::=a[a.lwb..n]\t\t\t!duplication forces original to be freed\r\nend\r\n \r\nglobal function makebits(data,t=bit)=\r\n\r\na:=new(bits,t,data.bounds)\r\nfor i:=data.lwb to data.upb do\r\n\ta[i]:=data[i]\r\nod\r\nreturn a\r\nend\r\n\r\nglobal function makearray(data,t=int32)=\r\n\r\na:=new(array,t,data.bounds)\r\nfor i:=data.lwb to data.upb do\r\n\ta[i]:=data[i]\r\nod\r\nreturn a\r\nend\r\n\r\nglobal function tolist(a)=\r\ncase a.basetype\r\nwhen array,string,bits then\r\n\tb:=new(list,a.bounds)\r\n\tforall i,x in a do\r\n\t\tb[i]:=x\r\n\tod\r\n\treturn b\r\n!when string then\r\n!\tb:=new(list,a.len)\r\n!\ti:=1\r\n!\tforall i,x in a do\r\n!\t\tb[i++]:=x\r\n!\tod\r\n!\treturn b\r\n!\r\nwhen list then\r\n\treturn a\r\nelse\r\n\tpcerror(\"tolist:\"+tostr(a.type))\r\nesac\r\nreturn 0\r\nend\r\n\r\nglobal function toarray(a,?t)=\r\ncase a.basetype\r\nwhen list then\r\n\tif t.isvoid then\r\n\t\tif a then\r\n\t\t\tt:=a[a.lwb].type\r\n\t\telse\r\n\t\t\tt:=int32\r\n\t\tfi\r\n\tfi\r\n\r\nwhen bits then\r\n\tif t.isvoid then\r\n\t\tt:=byte\r\n\tfi\r\n\r\nwhen string then\r\n\tif t.isvoid then t:=byte fi\r\n\tb:=new(array,t,a.len)\r\n\tforeach i,x in a do\r\n\t\tb[i]:=x\r\n\tod\r\n\treturn b\r\nwhen array then\r\n\tif t.isvoid then\r\n\t\treturn a\r\n\tfi\r\n\tu:=e.elemtype\r\n\tif t=u then return a fi\r\nelse\r\n\tpcerror(\"toarray:\"+tostr(a.type))\r\nesac\r\nb:=new(array,t,a.bounds)\r\n\r\nforall i,x in a do\r\n\tb[i]:=x\r\nod\r\nreturn b\r\nend\r\n\r\nglobal function tobits(a,t=bit)=\r\ncase a.basetype\r\nwhen list,array then\r\n\r\nwhen bits then\r\n\tif a.elemtype=t then\r\n\t\treturn a\r\n\tfi\r\n\r\nelse\r\n\tpcerror(\"tobits:\"+tostr(a.type))\r\nesac\r\nb:=new(bits,t,a.bounds)\r\nforall i,x in a do\r\n\tb[i]:=x\r\nod\r\nreturn b\r\nend\r\n\r\nglobal function listtostring(a)=\r\n!a should be a list or array\r\n!interpreter elements as characters and form a single string\r\ns:=\"\"\r\nforall x in a do\r\n\ts+:=chr(x)\r\nod\r\nreturn s\r\nend\r\n\r\nglobal function qversion=\r\nreturn \"2.2\"\r\nend\r\n";
static char *	l_files="import sys\r\nimport clib\r\n\r\nglobal var readfilesize\r\n\r\nproc start=\t\t\t\t!START\r\nend\r\n\r\nproc main=\r\n\r\nCPL \"THIS IS MAIN\"\r\n\r\nend\r\n\r\nglobal function openfile(name,option=\"rb\")=\r\nif not name.isstring or name=\"\" then\r\n\treturn 0\r\nfi\r\nreturn fopen(name,option)\r\nend\r\n\r\nglobal function createfile(name,options=\"wb\")=\t\t!CREATEFILE\r\nif not options.defined then options:=\"wb\" fi\r\nif not name.isstring or name=\"\" then return 0 fi\r\n\r\nreturn fopen(name,options)\r\nend\r\n\r\nglobal function closefile(file)=\t\t!CLOSEFILE\r\nreturn fclose(file)\r\nend\r\n\r\nglobal function checkfile(name)=\t\t!CHECKFILE\r\nfile:=fopen(name,\"rb\")\r\nif file=0 then return 0 fi\r\nfclose(file)\r\nreturn 1\r\nend\r\n\r\nglobal function eof(file)=\t\t!EOF\r\n!CPL =FILE\r\nc:=fgetc(file)\r\nif c=-1 then return 1 fi\r\n\r\nungetc(c,file)\r\nreturn 0\r\nend\r\n\r\nglobal function getfilesize(file)=\t\t!GETFILESIZE\r\np:=ftell(file)\t\t!p=current position\r\nfseek(file,0,2)\t\t!get eof position\r\nsize:=ftell(file)\t\t!size in bytes\r\nfseek(file,p,0)\t\t!restore file position\r\nreturn size\r\nend\r\n\r\nglobal function setfilepos(file,offset)=\t\t!SETFILEPOS\r\nreturn fseek(file,offset,0)\r\nend\r\n\r\nglobal function getfilepos(file)=\t\t!GETFILEPOS\r\nreturn ftell(file)\r\nend\r\n\r\nglobal function readrandom(file,mem,offset,size)=\t\t!READRANDOM\r\nfseek(file,offset,0)\r\nreturn fread(mem,1,size,file)\r\nend\r\n\r\nglobal function writerandom(file,mem,offset,size)=\t\t!WRITERANDOM\r\nfseek(file,offset,0)\r\nreturn fwrite(mem,1,size,file)\r\nend\r\n\r\nglobal function readbytes(file,mem,size)=\t\t!READBYTES\r\nreturn fread(mem,1,size,file)\r\nend\r\n\r\nglobal function writebytes(file,mem,size)=\t\t!WRITEBYTES\r\nreturn fwrite(mem,1,size,file)\r\nend\r\n\r\nglobal function inbyte(file)=\t\t!INBYTE\r\nreturn fgetc(file)\r\nend\r\n\r\nglobal function inword(file)=\t\t!INWORD\r\nbb:=fgetc(file)\r\nreturn fgetc(file)<<8+bb\r\nend\r\n\r\nglobal function inlong(file)=\t\t!INLONG\r\nww:=inword(file)\r\nreturn inword(file)<<16+ww\r\nend\r\n\r\nglobal proc outbyte(file,x)=\t\t!OUTBYTE\r\n!writerandom(file,&x,getfilepos(file),1)\r\nfputc(x,file)\r\nend\r\n\r\nglobal proc outword(file,x)=\t\t!OUTWORD\r\noutbyte(file,x iand 255)\r\noutbyte(file,x.[15..8])\r\nend\r\n\r\nglobal proc outlong(file,x)=\t\t!OUTLONG\r\noutword(file,x iand 65535)\r\noutword(file,x>>16)\r\nend\r\n\r\nglobal function instring(file)=\t\t!INSTRING\r\ns:=\"\"\r\ndo\r\n\tc:=inbyte(file)\r\n\tif c=0 then return s fi\r\n\ts+:=c\r\nod\r\nreturn s\r\nend\r\n\r\nglobal function appendfile(a,b)=\t\t!APPENDFILE\r\n!append line-based text file a to file b\r\n\r\nf:=openfile(a)\r\nif f=0 then return 0 fi\r\n\r\nh:=openfile(b,\"ab\")\r\nif h=0 then return 0 fi\r\n\r\nwhile not eof(f) do\r\n\treadln @f,x:\"l\"\r\n\tprintln @h,x\r\nod\r\n\r\nclosefile(f)\r\nclosefile(h)\r\nreturn 1\r\nend\r\n\r\nglobal function readblockfile(filename,doetx=0)=\t\t!READBLOCKFILE\r\n!read text file into a memory block\r\n!block is allocated here\r\n!return byte pointer to start of block, or nil\r\n!doetx=1 to add etx byte to end\r\n\r\nf:=openfile(filename)\r\nif f=0 then return 0 fi\r\n\r\nn:=getfilesize(f)\r\nreadfilesize:=n\r\n\r\ns:=malloc(n+doetx)\r\nif s=0 then abort(\"Readfile/Malloc fails\") fi\r\nsptr:=makeref(s,byte)\r\n\r\n!readrandom(f,&s,0,n)\r\nreadrandom(f,s,0,n)\r\n\r\nif doetx then\r\n\t(sptr+n)^:=26\r\nfi\r\n\r\nclosefile(f)\r\nreturn sptr\r\nend\r\n\r\nglobal function readstrfile(filename,doetx=0)=\t\t!READSTRFILE\r\n!read text file into a single string\r\n!return string, or 0 if there was an error\r\n\r\nf:=openfile(filename)\r\nif f=0 then return 0 fi\r\n\r\nn:=getfilesize(f)\r\nreadfilesize:=n\r\n\r\nptr:=malloc(n+1+doetx)\r\nif ptr=0 then abort(\"Readfile/Malloc fails\") fi\r\n\r\nreadrandom(f,ptr,0,n)\r\nif doetx then\r\n\t(makeref(ptr,byte)+n)^:=26\r\nfi\r\n\r\nclosefile(f)\r\ns::=makestr(ptr,n+doetx)\r\nfree(ptr)\r\nreturn s\r\nend\r\n\r\nglobal function writestrfile(filename,s)=\t\t!WRITESTRFILE\r\n!read text file from a single string\r\n!return status\r\n\r\nf:=createfile(filename)\r\nif f=0 then return 0 fi\r\n\r\n!CPL =SPTR\r\n\r\n!writerandom(f,&s,0,s.len)\r\nwriterandom(f,makeref(s,byte),0,s.len)\r\n\r\nreturn closefile(f)\r\nend\r\n\r\nglobal function readbinfile(filename)=\t\t!READBINFILE\r\n!read binary file into byte array\r\n!return () (empty list not array) on error\r\n\r\nf:=openfile(filename)\r\n!if f=0 then return () fi\r\nif f=0 then return 0 fi\r\n\r\nn:=getfilesize(f)\r\nreadfilesize:=n\r\n\r\na:=new(array,byte,n)\r\nreadrandom(f,&a,0,n)\r\n\r\nclosefile(f)\r\nreturn a\r\nend\r\n\r\n!global function writebinfile(a,filename)=\t\t!WRITEBINFILE\r\nglobal function writebinfile(filename,a)=\t\t!WRITEBINFILE\r\n!write binary file from byte array a\r\n!return status 1/0\r\n\r\nf:=createfile(filename)\r\nif f=0 then return 0 fi\r\n\r\n!writerandom(f,int(&a),0,a.len)\r\nwriterandom(f,(&a),0,a.len)\r\n\r\nclosefile(f)\r\nreturn 1\r\nend\r\n\r\nglobal function erasefile(filename)=\t\t!ERASEFILE\r\nreturn remove(filename)\r\nend\r\n\r\nglobal function renamefile(oldfilename,newfilename)=\t\t!RENAMEFILE\r\nreturn rename(oldfilename,newfilename)\r\nend\r\n\r\nglobal function readtextfile(file)=\r\n!read text file into a list of strings; one per line\r\n!return list, or 0 on error\r\nf:=openfile(file)\r\nif not f then\r\n\treturn 0 \r\nfi\r\n\r\nreadfilesize:=getfilesize(f)\r\na::=()\r\n\r\nwhile not eof(f) do\r\n\ta append:= sreadln(f)\r\nod\r\nclosefile(f)\r\nreturn a\r\nend\r\n\r\nglobal function writetextfile(file,a)=\t\t!WRITETEXTFILE\r\n!write list of strings <a> as a text file <file>\r\nf:=createfile(file)\r\nif not f then return 0 fi\r\n\r\nfor i:=a.lwb to a.upb do\r\n\tprintln @f,a[i]\r\nod\r\nclosefile(f)\r\nreturn 1\r\nend\r\n\r\nglobal proc ftest=\r\nabort(\"TESTING\")\r\nend\r\n";
static char *	l_clib="importdll msvcrt=\r\n\tclang function\tmalloc\t\t(int32)intm\r\n\tclang function\trealloc\t\t(intm, int32)intm\r\n\tclang proc\t\tfree\t\t(intm)\r\n\tclang proc\t\tmemset\t\t(ref int32, int32, int32)\r\n\tclang proc\t\tmemcpy\t\t(ref int32, ref int32, int32)\r\n\tclang function\tclock\t\t:int32\r\n\tclang function\tftell\t\t(intm)int32\r\n\tclang function\tfseek\t\t(intm, int32, int32)int32\r\n\tclang function\tfread\t\t(ref int32, int32, int32, intm)int32\r\n\tclang function\tfwrite\t\t(ref int32, int32, int32, intm)int32\r\n\tclang function\tgetc\t\t(intm)int32\r\n\tclang function\tungetc\t\t(int32, intm)int32\r\n\tclang function\tfopen\t\t(string, string)intm\r\n\tclang function\tfclose\t\t(intm)int32\r\n!\tclang function\tfgets\t\t(ref byte, int32, intm)string\r\n\tclang function\tfgets\t\t(ref byte, int32, intm)ref byte\r\n\tclang function\tremove\t\t(string)int32\r\n\tclang function\trename\t\t(string, string)int32\r\n\tclang function\tgetchar\t\t:int32\r\n\tclang proc\t\tputchar\t\t(int32)\r\n\tclang proc\t\tsetbuf\t\t(intm, intm)\r\n\r\n\tclang function\tputs\t\t(string)int32\r\n\tclang function\tprintf\t\t(string, ...)int32\r\n!\tclang function\tprintf\t\t(string)int32\r\n\r\n\tclang function\tsprintf\t\t(string, string, ...)int32\r\n!\tclang function\t__mingw_sprintf\t\t(string, ...)int32\r\n\r\n\tclang function\tsscanf\t\t(string, string, ...)int32\r\n\tclang function\tisalpha\t\t(int32)int32\r\n\tclang function\ttolower\t\t(int32)int32\r\n\tclang function\tstrlen\t\t(ref byte)int32\r\n\r\n!\tclang function\tsystem\t\t(string)int32\r\n\r\n\tclang function\tfgetc\t\t(intm)int32\r\n\tclang function\tfputc\t\t(int32,  intm f)int32\r\n\tclang function\tfprintf\t\t(intm, string, ...)int32\r\n\tclang function\tfputs\t\t(string,  intm)int32\r\n\tclang function\tfeof\t\t(intm)int32\r\n\tclang function\tgetch\t\t:int32\r\n\r\n\tconst c_eof\t\t= -1\r\n\tconst seek_set\t= 0\r\n\tconst seek_curr\t= 1\r\n\tconst seek_end\t= 2\r\nend\r\n";
static char *	l_oslib="import sys\r\n!import winlib\r\n!define oslibsw=winlib\r\n\r\nimport oslibsw\r\n\r\nglobal var daynames=(\"Monday\",\"Tuesday\",\"Wednesday\",\"Thursday\",\"Friday\",\"Saturday\",\"Sunday\")\r\n\r\nglobal var Monthnames=(\"January\",\"February\",\"March\",\"April\",\"May\",\"June\",\"July\",\r\n\t\t\"August\",\"September\",\"October\",\"November\",\"December\")\r\n\r\nglobal var days=(31,28,31, 30,31,30, 31,31,30, 31,30,31)\r\n\r\nglobal class rdate=\r\n\tvar day,month,year\r\nend\r\n\r\nglobal class rdatetime = \r\n\tvar\tday\r\n\tvar\tmonth\r\n\tvar\tyear\r\n\tvar\thour\r\n\tvar\tminute\r\n\tvar\tsecond\r\n\tvar\tmilliseconds\r\n\tvar\tdayofweek\r\nend\r\n\r\nproc start=\r\nend\r\n\r\nproc main=\r\nCPL \"OSLIB 234\"\r\n\r\n!D:=PARSEDATE(\"11-may\",getdatetime())\r\n!CPL =D\r\n!cpl strdate(d)\r\n\r\n!files:=dirlist(\"*.c\")\r\n\r\n!cpl =files\r\n\r\n!CPL =DIREXISTS(\"lib\")\r\n!DIR:=GETCURRDIR()\r\n!\r\n!CPL =DIR\r\n\r\n!STATUS:=SETCURRDIR(\"/home/\")\r\n!STATUS:=SETCURRDIR(\"./lib\")\r\n!CPL =STATUS\r\n\r\nFILES:=DIRLIST(\"/home/osboxes/*.c\")\r\nFORALL i,f in files do\r\n\tcpl i,,\":\",f\r\nod\r\n!CPL =FILES\r\n\r\nend\r\n\r\nglobal function makedatetime(d,m,y, h=0, minute=0, s=0)=\r\n\r\nd:=rdatetime(d,m,y, h,minute,s,0,0)\r\nd.dayofweek:=getdow(d)\r\nreturn d\r\nend\r\n\r\nglobal proc setdow(&d)=\r\nd.dayofweek:=getdow(d)\r\nend\r\n\r\nglobal function strdate(d,sep=\"-\")=\r\n!return leftstr(daynames[d.dayofweek],3)+\" \"+tostr(d.day)+sep+leftstr(monthnames[d.month],3)+sep+tostr(d.year)\r\nreturn tostr(d.day)+sep+leftstr(monthnames[d.month],3)+sep+tostr(d.year)\r\nend\r\n\r\nglobal function strtime(d,sep=\":\")=\r\nreturn tostr(d.hour)+sep+tostr(d.minute,\"z2\")+sep+tostr(d.second,\"z2\")\r\nend\r\n\r\nglobal function strdow(d,n=0)=\r\nif n then\r\n\treturn leftstr(daynames[d.dayofweek],n)\r\nelse\r\n\treturn daynames[d.dayofweek]\r\nfi\r\nend\r\n\r\nglobal function strdatetime(d,dsep=\"-\",tsep=\":\")=\r\nreturn strdate(d,dsep)+\" \"+strtime(d,tsep)\r\nend\r\n\r\nglobal function confirm(m,?caption,?default)=\t\t!CONFIRM\r\n!default=1/2/3 for yes/no/cancel button\r\n\r\nflags:=0x20000+0x20\t!foreground window/question mark icon\r\nflags ior:=3\t\t!yes/no/cancel\r\n\r\nif default.isvoid then default:=1 fi\r\nflags ior:=(default|0,0x100,0x200|0)\r\n\r\nstatus:=messagebox(0,m,(caption.defined|caption|\"Confirm\"),flags)\r\nreturn status=6\r\nend\r\n\r\nglobal function parsedate(s,defdate)=\r\n!parse string s into a new date record\r\n!def = default date to work from, eg. for missing year\r\n!return date record obtained, or 0 if error\r\n\r\nday:=defdate.day\r\nmonth:=defdate.month\r\nyear:=defdate.year\r\nif s.[1]=\" \" then s:=rightstr(s,-1) fi\r\n\r\nsepset:=[' ', '-', '/', '.']\r\n\r\nseppos:=0\r\nfor i:=1 to s.len do if s.[i] in sepset then seppos:=i; exit fi od\r\n\r\nif not seppos then\t\t!day only\r\n\tday:=strtoval(s)\r\n\tgoto gotday\r\nfi\r\nday:=strtoval(leftstr(s,seppos-1))\r\n\r\ns:=rightstr(s,-seppos)\t\t!month and possible year\r\nseppos:=0\r\nfor i:=1 to s.len do if s.[i] in sepset then seppos:=i; exit fi od\r\n\r\nif seppos then\r\n\tmonthstr:=leftstr(s,seppos-1)\r\n\tyearstr:=rightstr(s,s.len-seppos)\r\nelse\r\n\tmonthstr:=s\r\n\tyearstr:=\"\"\r\nfi\r\n\r\nif asc(leftstr(monthstr)) in ['0'..'9'] then\t!numeric month\r\n\tmonth:=strtoval(monthstr)\r\n\tif month<1 or month>12 then\r\n\t\treturn 0\r\n\tfi\r\nelse\r\n\tmonth:=0\r\n\tfor i:=1 to 12 do\r\n\t\tif convlc(leftstr(monthnames[i],3))=convlc(leftstr(monthstr,3)) then\r\n\t\t\tmonth:=i\r\n\t\t\texit\r\n\t\tfi\r\n\tod\r\n\tif not month then\r\n\t\treturn 0\r\n\tfi\r\nfi\r\n\r\nif yearstr<>\"\" then\r\n\tyear:=strtoval(yearstr)\r\n\tif year<200 then\r\n\t\tif year in [00..89] then\r\n\t\t\tyear+:=2000\r\n\t\telse\r\n\t\t\tyear+:=1900\r\n\t\tfi\r\n\tfi\r\nfi\r\n\r\ngotday:\r\n!check the date, rather than correct using addday(d,0)\r\ndd:=days[month] \r\nif leapyear(year) and month=2 then dd+:=1 fi\r\nif day<1 or day>dd then return 0 fi\r\nif year<1990 or year>2089 then return 0 fi\r\nreturn makedatetime(day,month,year)\r\n!d:=daterec(day,month,year)\r\n!return addday(d,0)\r\nend\r\n\r\nglobal function leapyear(y)=\r\n!return true if y (eg. 1994) is a leap year\r\nreturn (y-1900) rem 4=0\r\nend\r\n\r\nglobal function getdow(d)=\r\n!return day of week for given date, returning 1..7 (monday..sunday)\r\nreturn ((getday(d)-1) rem 7)+1\r\nend\r\n\r\nglobal function getday(d)=\r\n!return day number for date d, measured from 1.1.90\r\nday:=0\r\nfor i:=1990 to d.year-1 do\r\n\tday+:=(leapyear(i)|366|365)\r\nod\r\n\r\nfor i:=1 to d.month-1 do\r\n\tday+:=(i=2|(leapyear(d.year)|29|28)|days[i])\r\nod\r\nday+:=d.day\r\nreturn day\r\nend\r\n\r\nglobal function getdays(m,y)=\r\n!return no. of days in month m, for year y\r\nif leapyear(y) and m=2 then return 29 fi\r\nreturn days[m]\r\nend\r\n\r\nglobal function getmonthname(m,?n)=\r\nif not m.isint then\r\n\tm:=m.month\r\nfi\r\nm:=monthnames[m]\r\nif n.defined then m:=leftstr(m,n) fi\r\nreturn m\r\nend\r\n\r\nglobal function getdayname(d,?n)=\r\nif not d.isint then\r\n\td:=getdow(d)\r\nfi\r\nd:=daynames[d]\r\nif n.defined then d:=leftstr(d,n) fi\r\nreturn d\r\nend\r\n\r\nglobal function addday(d0,i)=\r\nd:=d0\r\nif i>0 then\r\n\tto i do\r\n\t\td.day+:=1\r\n\t\tif d.day>getdays(d.month,d.year) then\r\n\t\t\td.day:=1\r\n\t\t\td.month+:=1\r\n\t\t\tif d.month>12 then\r\n\t\t\t\td.month:=1\r\n\t\t\t\td.year+:=1\r\n\t\t\tfi\r\n\t\tfi\r\n\tod\r\nelse\r\n\tto -i do\r\n\t\td.day-:=1\r\n\t\tif d.day<1 then\r\n\t\t\td.month-:=1\r\n\t\t\tif d.month<1 then\r\n\t\t\t\td.month:=12\r\n\t\t\t\td.year-:=1\r\n\t\t\tfi\r\n\t\t\td.day:=getdays(d.month,d.year)\r\n\t\tfi\r\n\tod\r\nfi\r\n\r\n!do checking\r\nif d.year<1990 then d:=makedatetime(1,1,1990) fi\r\nif d.year>2089 then d:=makedatetime(31,12,2089) fi\r\n\r\ndd:=getdays(d.month,d.year)\r\nif leapyear(d.year) and d.month=2 then dd+:=1 fi\r\nif d.day<1 then d.day:=1 fi\r\nif d.day>dd then d.day:=dd fi\r\nsetdow(d)\r\nreturn d\r\nend\r\n\r\nglobal function getdatetime=\r\ntm:=getsystime()\r\n\r\nreturn rdatetime(tm.day,tm.month,tm.year,\r\n\t\ttm.hour, tm.minute, tm.second, tm.milliseconds,tm.dayofweek)\r\nend\r\n\r\nglobal function messagebox(a,mess,caption,d)=\r\nreturn oslibsw.messagebox(a,mess,caption,d)\r\nend\r\n\r\nglobal function dirlist(s,?t)=\t\t!DIRLIST\r\nif t.isvoid then t:=1 fi\t\t\t!files only\r\nreturn oslibsw.dirlist(s,t)\r\nend\r\n\r\nglobal function setcurrdir(newdir)=\t\t!SETCURRDIR\r\nreturn oslibsw.setcurrdir(newdir)\r\nend\r\n\r\nglobal function getcurrdir=\t\t!GETCURRDIR\r\nreturn oslibsw.getcurrdir()\r\nend\r\n\r\nglobal function createdir(name)=\r\nreturn oslibsw.createdir(name)\r\nend\r\n\r\nglobal function direxists(path)=\r\nreturn oslibsw.direxists(path)\r\nend\r\n\r\nglobal function getsystime=\r\nreturn oslibsw.getsystime()\r\nend\r\n\r\nglobal proc beep1=\r\noslibsw.beep1()\r\nend\r\n";
static char *	l_oslibsw="!Interface module between oslib and winlib/linlib\r\n\r\nimport sys\r\nimport winlib\r\nimport linlib\r\n\r\n!var iswindows=leftstr(getos())=\"W\"\r\n!var iswindows=0\r\n\r\nglobal function messagebox(a,mess,caption,d)=\r\nif iswindows() then\r\n\treturn winlib.messagebox(a,mess,caption,d)\r\nelse\r\n\treturn linlib.messagebox(a,mess,caption,d)\r\nfi\r\nend\r\n\r\nglobal function dirlist(s,?t)=\t\t!DIRLIST\r\n!CPL \"DIRLISTSW\"\r\nif t.isvoid then t:=1 fi\t\t\t!files only\r\n\r\nif iswindows() then\r\n\treturn winlib.dirlist(s,t)\r\nelse\r\n\treturn linlib.dirlist(s,t)\r\nfi\r\nend\r\n\r\nglobal function setcurrdir(newdir)=\t\t!SETCURRDIR\r\nif iswindows() then\r\n\treturn winlib.setcurrdir(newdir)\r\nelse\r\n\treturn linlib.setcurrdir(newdir)\r\nfi\r\nend\r\n\r\nglobal function getcurrdir=\t\t!GETCURRDIR\r\nif iswindows() then\r\n\treturn winlib.getcurrdir()\r\nelse\r\n\treturn linlib.getcurrdir()\r\nfi\r\nend\r\n\r\nglobal function createdir(name)=\r\nif iswindows() then\r\n\treturn winlib.createdir(name)\r\nelse\r\n\treturn linlib.createdir(name)\r\nfi\r\nend\r\n\r\nglobal function direxists(path)=\r\nif iswindows() then\r\n\treturn winlib.direxists(path)\r\nelse\r\n\treturn linlib.direxists(path)\r\nfi\r\nend\r\n\r\nglobal function getsystime=\r\nif iswindows() then\r\n\treturn winlib.getsystime()\r\nelse\r\n\treturn linlib.getsystime()\r\nfi\r\nend\r\n\r\nglobal proc beep1=\r\nif iswindows() then\r\n\twinlib.beep1()\r\nelse\r\n\tlinlib.beep1()\r\nfi\r\nend\r\n";
static char *	l_winlib="!Miscellaneous functions for Windows\r\n!date/time handling, directory handling\r\n!Import indirectly via oslibsw\r\n\r\nimport sys\r\nimport winapi\r\n\r\nglobal function messagebox(a,mess,caption,d)=\r\nreturn messageboxa(a,mess,caption,d)\r\nend\r\n\r\nglobal function dirlist(s,t=1)=\t\t!DIRLIST\r\n!s is a global filename (eg. \"*.dwg\") with possible drive/path; scan\r\n!directory for all matching files and return as a list of names\r\n!also returns total no. of files so far\r\n!t= +1\tInclude normal files, no sub-directory names\r\n!t= +2  Include directories\r\n!t= +3  Include all files including directories\r\n!t= +4  Convert to lower case\r\n!t=  0  Defaults to +1\r\n\r\n!CPL \"DIRLIST/WINLIB\"\r\n\r\nif t.isvoid then t:=1 fi\t\t\t!files only\r\n\r\nnfiles:=0\r\ndata::=()\r\nfile:=new(ws_finddata)\r\n\r\nif (hfind:=findfirstfile(s,&file))<>-1 then\t!at least one file\r\n\trepeat\r\n\t\tif (file.fileattributes iand 16) then\t\t!this is a directory\r\n\t\t\tif (t iand 2)=0 then goto skip fi\t\t!no directories\r\n\t\telse\t\t\t\t\t\t!this is a file\r\n\t\t\tif (t iand 1)=0 then goto skip fi\r\n\t\tfi\r\n\t\t++nfiles\r\n\t\tif (t iand 4) then\t\t\t\t!to lower case\r\n\t\t\tdata[nfiles]:=convlc(file.filename)\r\n\t\telse\r\n\t\t\tdata[nfiles]::=file.filename\r\n\t\tfi\r\nskip:\r\n\tuntil not findnextfile(hfind,&file)\r\n\tfindclose(hfind)\r\nfi\r\nreturn data\r\nend\r\n\r\nglobal function setcurrdir(newdir)=\t\t!SETCURRDIR\r\nreturn setcurrentdirectory(newdir)\r\nend\r\n\r\nglobal function getcurrdir=\t\t!GETCURRDIR\r\na:=new(array,byte,256)\r\nn:=getcurrentdirectory(a.len,&a[1])\r\n\r\nif n then\r\n\tdir::=makestr(&a[1],n)\r\nelse\r\n\tdir:=\"\"\r\nfi\r\n\r\n!if not (rightstr(dir) in \"\\\\/\") then dir.iappend(\"\\\\\") fi\r\n!if not (rightstr(dir) in \"\\\\/\") then iappend(dir,\"\\\\\") fi\r\nif not (rightstr(dir) in \"\\\\/\") then dir +:= \"\\\\\" fi\r\nreturn dir\r\nend\r\n\r\nglobal function createdir(name)=\r\nreturn createdirectory(name,0)\r\nend\r\n\r\nglobal function direxists(path)=\r\nconst file_attribute_directory=16\r\nconst invalid_file_attributes=-1\r\n\r\nattrib := getfileattributesa(path)\r\n\r\nreturn attrib<>invalid_file_attributes and (attrib iand file_attribute_directory)\r\nend\r\n\r\nglobal function getsystime=\r\ntm:=new(ws_systemtime)\r\ngetsystemtime(&tm)\r\n\r\nif tm.dayofweek=0 then\r\n\ttm.dayofweek:=7\r\nfi\r\n\r\nreturn tm\r\nend\r\n\r\nglobal proc beep1=\r\n!beep(500,200)\r\nmessagebeep(0)\r\nend\r\n";
static char *	l_linlib="!Miscellaneous functions for Linux\r\n!date/time handling, directory handling\r\n!Import indirectly via oslibsw\r\n\r\nimport sys\r\nimport clib\r\n\r\nimportdll msvcrt=\r\n\tclang function \topendir(string)ref byte\r\n\tclang function \tclosedir(ref byte)ref byte\r\n\tclang function \tgetcwd(ref byte)ref byte\r\n\tclang function \tchdir(string)int32\r\n\tclang function \tmkdir(string,int32)int32\r\n\tclang function \treaddir(ref byte)ref byte\r\n\tclang function \tfnmatch(string,string,int32)int32\r\nend\r\n\r\ntype dirent32 = struct\r\n\t[10]byte d32_filler\r\n\tbyte d_type\r\n\tstringz*256 d_name\r\nend\r\n\r\ntype dirent64 = struct\r\n\t[18]byte d64_filler\r\n\tbyte d_type\r\n\tstringz*256 d_name\r\nend\r\n\r\nglobal function messagebox(a,mess,caption,d)=\r\nRETURN PCERROR(\"LINUX MESSAGEBOX\")\r\nend\r\n\r\nglobal function dirlist(filespec,?t)=\r\nif t.isvoid then t:=1 fi\t\t\t!files only\r\n\r\n!CPL \"DIRLIST1\"\r\n\r\npath:=extractpath(filespec)\r\nfilepattern:=extractfile(filespec)\r\n!CPL \"DIRLIST2\",path\r\n\r\nif path=\"\" then path:=\".\" fi\r\n!CPL \"DIRLIST3\",path\r\n\r\nd:=opendir(path)\r\n!CPL \"DIRLIST4\",path\r\nif not d then\r\n\treturn ()\r\nfi\r\n\r\nnfiles:=0\r\ndata:=()\r\n!CPL \"OPENED DIR\",gethostsize()\r\n\r\nwhile dir:=readdir(d) do\r\n\tif gethostsize()=32 then\r\n\t\tdir:=makeref(dir,dirent32)\r\n\telse\r\n\t\tdir:=makeref(dir,dirent64)\r\n\tfi\r\n\tif dir^.d_type=4 then\t\t!DIR\r\n\telsif fnmatch(filepattern,dir^.d_name,16)=0 then\r\n!CPL \"GOT FILE\",dir^.d_name\r\n\t\t++nfiles\r\n\t\tif (t iand 4) then\t\t\t\t!to lower case\r\n\t\t\tdata[nfiles]:=convlc(dir^.d_name)\r\n\t\telse\r\n\t\t\tdata[nfiles]::=dir^.d_name\r\n\t\tfi\r\n\tfi\r\nod\r\n\r\nclosedir(d)\r\n\r\nreturn data\r\nend\r\n\r\nglobal function setcurrdir(newdir)=\t\t!SETCURRDIR\r\nreturn chdir(newdir,8x700)<>0\r\nend\r\n\r\nglobal function getcurrdir=\t\t!GETCURRDIR\r\na:=new(array,byte,256)\r\np:=&a[1]\r\n\r\ns:=getcwd(p,a.len)\r\n\r\nif s then\r\n\tdir::=makestr(&a[1],strlen(p))\r\nelse\r\n\tdir:=\"\"\r\nfi\r\n\r\nif rightstr(dir)<>\"/\" then dir +:= \"/\" fi\r\nreturn dir\r\nend\r\n\r\nglobal function createdir(name)=\r\nreturn system(\"mkdir \"+name)=0\r\nend\r\n\r\nglobal function direxists(path)=\r\nd:=opendir(path)\r\nif d then\r\n\tclosedir(d)\r\n\treturn 1\r\nfi\r\nreturn 0\r\nend\r\n\r\nglobal function getsystime=\r\nRETURN PCERROR(\"LINUX GETSYSTIME\")\r\nend\r\n\r\nglobal proc beep1=\r\nPCERROR(\"LINUX BEEP1\")\r\nend\r\n";
static char *	l_console="import sys\r\nimport condata\r\n\r\n!import wincon\r\n!import lincon\r\n!define wincon=lincon\r\nimport consolesw\r\n\r\n\r\nglobal record winrec =\r\n\tvar posx,posy\r\n\tvar cols,rows\r\n\tvar fgnd,bgnd\t\t\t!default text/background colour\r\n\r\n\tvar columns\t\t\t!used when divided into columns\r\n\tvar itemcols\t\t\t!width of each column\r\n\tvar pagesize\t\t\t!columns*rows\r\n\r\n\tvar name\r\n\r\n\tvar hdata\t\t\t!pointer to data record, or is nil\r\nend\r\n\r\nglobal var wscreen\r\nglobal var screencols,screenrows\r\n\r\nglobal var chardata\t\t\t!string these two represent row of the console\r\nglobal var attrdata\t\t\t!string\r\n\r\nglobal var defscreenfgnd=con_black\r\nglobal var defscreenbgnd=con_grey\r\nglobal var rlkey=0\t\t!set by readline, when special key has been input\r\nglobal var rlbuffer\t\t\t!contents of readline buffer when special key pressed\r\n\r\nvar cmdindex,ncmds\r\nvar cmdhistory\r\n\r\nglobal const capsmask  = 0x8\t\t!shift states as they are in .keyshift\r\nglobal const altmask   = 0x4\r\nglobal const ctrlmask  = 0x2\r\nglobal const shiftmask = 0x1\r\n\r\nglobal const capsbit=3\r\nglobal const altbit=2\r\nglobal const ctrlbit=1\r\nglobal const shiftbit=0\r\n\r\nPROC START=\r\n!CPL \"CONSOLE START\"\r\n!STOP\r\n\r\ninit()\r\ninitcmds()\r\nEND\r\n\r\nPROC MAIN=\r\n\r\n!CPL \"CONSOLE MAIN\"\r\n!STOP\r\n\r\ninit()\r\n\r\ninitcmds()\r\n\r\n\r\n!keytest()\r\n!screentest()\r\n!keyscreentest()\r\n\r\n!setbold(0)\r\nfor i:=0 to 15 do\r\n\tsetcolour(i,15)\r\n!\tsetcolour(15,i)\r\n\tsetpos(1,i+1)\r\n\tCPL i,\"hello\"\r\nod\r\n\r\nwaitkey()\r\n\r\nEND\r\n\r\nproc keyscreentest=\r\n(cols,rows):=(screencols, screenrows)\r\nCPL =COLS,=ROWS\r\n\r\nrow:=rows%2\r\ncol:=cols%2\r\nch:=\"X\"\r\n\r\nsetcolour(6,1)\r\n\r\ndo\r\n\tsetpos(col,row)\r\n\tcp ch\r\n\tsetpos(col,row)\r\n\tk:=getkey().keycode\r\n\tcase k\r\n\twhen 27 then\r\n\t\texit\r\n\twhen vkleft then col:=max(1,col-1)\r\n\twhen vkright then col:=min(cols,col+1)\r\n\twhen vkup then row:=max(1,row-1)\r\n\twhen vkdown then row:=min(rows,row+1)\r\n\tesac\r\nod\r\n\r\n!waitkey()\r\n\r\nend\r\n\r\n\r\nglobal proc setpos(col,row)=\r\nconsolesw.setpos(col,row)\r\nend\r\n\r\nglobal function getpos=\r\nreturn consolesw.getpos()\r\nend\r\n\r\nglobal function getdims=\r\nreturn (screencols,screenrows)\r\nend\r\n\r\nglobal proc init(cols=100)=\r\nconsolesw.init(cols)\r\ncmdhistory::=()\t!\"one\",\"two\",\"three\",\"four\")\r\nncmds:=cmdhistory.upb\r\ncmdindex:=0\r\n\r\n\r\nscreencols:=consolesw.wscreencols\r\nscreenrows:=consolesw.wscreenrows\r\n\r\nCPL =CONSOLESW.WSCREENCOLS\r\n\r\nwscreen:=makewin((1,1),(screencols,screenrows),defscreencolour)\r\n\r\nchardata:=\" \"*screencols\r\nattrdata:=chr(0xf0)*screencols\r\n\r\n!CPL \"M:CONSOLE done\"\r\nend\r\n\r\nglobal proc setcolour(fgnd,bgnd)=\r\n!call with as (fgnd,bgnd) or as (fgnd..bgnd)\r\nconsolesw.setcolour(fgnd,bgnd)\r\nend\r\n\r\nglobal proc settitle(caption)=\r\nconsolesw.settitle(caption)\r\nend\r\n\r\n!global function getkey2=\r\n!return consolesw.getkey2()\r\n!end\r\n\r\nglobal function getkey=\r\n!calls icongetkey but doesn't return shift keys as discrete key presses\r\nreturn consolesw.getkey()\r\nend\r\n\r\n!global function getkeychar=\r\n!return consolesw.getkeychar()\r\n!end\r\n\r\nglobal function keyready=\r\nreturn consolesw.keyready()\r\nend\r\n\r\nglobal proc flushkbd=\r\nconsolesw.flushkeyboard()\r\nend\r\n\r\nglobal function readline(?cmdline,donewline=1)=\r\n!this function doesn't handle tabs properly\r\n!would need to maintain 2 buffers, one with tabs translated to spaces\r\n!or convert tabs to another char which is translated back to tabs on exit\r\n!return with input buffer set to the line, but also returns the complete line\r\n!newline=1 to end with a newline, 0 to leave it\r\n\r\n!readln\r\n!return\r\n\r\nbuffer:=\"\"\r\nnchars:=0\r\n!congetpos()\r\n\r\n!NOTE: getpos is dodgy using TERMCON; MAY NEED CALLER TO SPECIFY START POINT\r\n(startx,starty):=(getpos())\r\n\r\n!STARTX:=1\r\n!STARTY:=10\r\n\r\npos:=0\t\t!with nchars shown, pos can be 0 to nchars\r\n\r\nreenter:\r\nif cmdline.defined and cmdline<>\"\" then\r\n\tbuffer:=cmdline\r\nreenter2:\r\n\tpos:=nchars:=buffer.len\r\nfi\r\n\r\ndo\r\n! print \"_\"\r\n\trlkey:=0\t\t\t!normal input starts with \"*\" will expect rlkey to be a keyrec\r\n\tsetpos(startx,starty)\r\n\tprint buffer\r\n\tsetpos(startx+pos,starty)\r\n\r\n\tkey:=getkey()\r\n\tkeycode:=key.keycode\r\n\tkeyshift:=key.shift\r\n\r\n!CPL \"KEY=\",KEY\r\n\r\n\tcase keycode\r\n\twhen vkpageup,vkpagedown,vkup,vkdown,vkinsert,vkf1..vkf12 then\r\n\r\ndospecial:\r\n\trlbuffer:=buffer\r\n\t\toldbufferlen:=buffer.len\t\t!to help erase old buffer\r\n\t\tbuffer:=getkeyname(key)\r\n\t\trlkey:=key\t\t\t\t!allow caller to use key code rather than name\r\n\t\texit\r\n\r\n\twhen vkleft then\r\n\t\tif buffer=\"\" then goto dospecial fi\r\n\t\tif (keyshift iand 7) then goto dospecial fi\r\n\r\n\t\tif pos>0 then\r\n\t\t\t--pos\r\n\t\tfi\r\n\r\n\twhen vkhome then\r\n\t\tif buffer=\"\" then goto dospecial fi\r\n\t\tif (keyshift iand 7) then goto dospecial fi\r\n\t\tpos:=0\r\n\r\n\twhen vkend then\r\n\t\tif buffer=\"\" then goto dospecial fi\r\n\t\tif (keyshift iand 7) then goto dospecial fi\r\n\t\tpos:=nchars\r\n\r\n\twhen vkright then\r\n\t\tif buffer=\"\" then goto dospecial fi\r\n\t\tif (keyshift iand 7) then goto dospecial fi\r\n\t\tif pos<nchars then\r\n\t\t\t++pos\r\n\t\tfi\r\n\r\n\twhen vkenter then\r\n\r\n!  println\r\n\t\texit\r\n\r\n\twhen vkbackspace then\r\n\r\n\t\tif (keyshift iand 7) then goto dospecial fi\r\n\t\tif nchars then\r\n\t\t\tsetpos(startx,starty)\r\n\t\t\tprint \" \"*buffer.len\r\n\r\n\t\t\tcase pos\r\n\t\t\twhen 0 then\t\t\t!not allowed\r\n\t\t\twhen nchars then\t\t!at end\r\n\t\t\t\tbuffer:=leftstr(buffer,-1)\r\n\t\t\t\t--nchars\r\n\t\t\t\t--pos\r\n\t\t\telse\t\t\t\t!in middle\r\n\t\t\t\tbuffer:=leftstr(buffer,pos-1)+rightstr(buffer,-(pos))\r\n\t\t\t\t--nchars\r\n\t\t\t\t--pos\r\n\t\t\tesac\r\n\r\n\t\tfi\r\n\r\n\twhen vkdelete then\r\n\t\tif (keyshift iand 7) then goto dospecial fi\r\n\t\tif nchars and nchars=pos then\r\n\t\t\tgoto delline\r\n\t\tfi\r\n\t\tif nchars=0 then\r\n\t\t\tgoto dospecial\r\n\t\tfi\r\n\t\tif nchars then\r\n\t\t\tsetpos(startx,starty)\r\n\t\t\tprint \" \"*buffer.len\r\n\r\n\t\t\tcase pos\r\n\t\t\twhen nchars then\t\t!not allowed\r\n\t\t\twhen 0 then\t\t\t!at start\r\n\t\t\t\tbuffer:=leftstr(buffer,-1)\r\n\t\t\t\t--nchars\r\n\t\t\telse\t\t\t\t!in middle\r\n\t\t\t\tbuffer:=leftstr(buffer,pos)+rightstr(buffer,-(pos+1))\r\n\t\t\t\t--nchars\r\n!    --pos\r\n\t\t\tesac\r\n\r\n\t\tfi\r\n\r\n\twhen vkescape then\r\n\t\tif nchars=0 then\r\n\t\t\tgoto dospecial\r\n!   oldbufferlen:=buffer.len\r\n!   buffer:=\"*esc\"\r\n!   exit\r\n\t\tfi\r\ndelline:\r\n\t\tsetpos(startx,starty)\r\n\t\tprint \" \"*buffer.len\r\n\r\n\t\tbuffer:=\"\"\r\n\t\tnchars:=pos:=0\r\n\r\n\twhen vktab then\r\n!  if (keyshift iand 7) then goto dospecial fi\r\n!  if buffer=\"\" then goto dospecial fi\r\n\t\tgoto normalkey\r\n\r\n\telse\r\nnormalkey:\r\n!  if keychar='X'-64 then\r\n!   goto docancel\r\n!  fi\r\n\t\tif (key.charcode>=' ' or key.charcode=9) then\r\n\t\t\tif pos=0 then\r\n\t\t\t\tbuffer:=chr(key.charcode)+buffer\r\n\t\t\telsif pos=nchars then\r\n\t\t\t\tbuffer:=buffer+chr(key.charcode)\r\n\t\t\telse\r\n\t\t\t\tbuffer:=leftstr(buffer,pos)+chr(key.charcode)+rightstr(buffer,-(pos))\r\n\t\t\tfi\r\n\t\t\t++nchars\r\n\t\t\t++pos\r\n!  elsif key.charcode in (1..31) then\r\n!   print \"<Ctrlcode>\"\r\n!  elsif key.keycode in ['A'..'Z','0'..'9'] then\r\n!   GOTO DOSPECIAL\r\n!!   print \"<Ctrlcode>\"\r\n\t\telse\r\n\t\t\tGOTO DOSPECIAL\r\n\t\t\tprint \"<\",keycode,key.charcode,\">\"\r\n\t\tfi\r\n\r\n\tesac\r\nod\r\n\r\ncase buffer\r\nwhen \"*cup\",\"*cdown\" then\r\n\tif ncmds then\r\n\t\tsetpos(startx,starty)\r\n\t\tprint \" \"*oldbufferlen\r\n\r\n\t\tif cmdindex=0 then\t\t!get started on last\r\n\t\t\tcmdline:=cmdhistory[ncmds]\r\n\t\t\tcmdindex:=ncmds\r\n\t\t\tgoto reenter\r\n\t\tfi\r\n\r\n\t\tif buffer=\"*cup\" and cmdindex>1 then\r\n\t\t\t--cmdindex\r\n\t\telsif buffer=\"*cdown\" and cmdindex<ncmds then\r\n\t\t\t++cmdindex\r\n\t\tfi\r\n\t\tcmdline:=cmdhistory[cmdindex]\r\n\t\tgoto reenter\r\n\tfi\r\n\tbuffer:=\"\"\r\n\tgoto reenter2\r\nesac\r\n\r\nif buffer.len>1 and leftstr(buffer)<>\"*\" then\r\n\tif ncmds=0 or cmdhistory[ncmds]<>buffer then\r\n!CPL \"ADDING:<\",,BUFFER,,\">\"\r\n\t\tcmdhistory[++ncmds]:=buffer\r\n\tfi\r\n\tcmdindex:=0\r\nfi\r\n\r\nif donewline then println fi\r\n\r\nreturn sreadln(buffer)\r\nend\r\n\r\nglobal proc showtext(s,?x,?y)=\r\n\r\nif x.defined then\r\n\tconsolesw.setpos(x,y)\r\nfi\r\ncount:=0\r\nif s then\r\n\tconsolesw.showtext(s)\r\nfi\r\nend\r\n\r\n!global proc dummy(s,x,y)=\r\n!!CPL \"DUMMY\",S,X,Y\r\n!!if x.defined then\r\n!! consolesw.setpos(x,y)\r\n!!fi\r\n!!count:=0\r\n!!if s then\r\n!! consolesw.showtext(s)\r\n!!fi\r\n!end\r\n\r\nglobal proc setdims(cols,rows)=\r\n!set new size for console, by reinitialising\r\nconsolesw.setdims(cols,rows)\r\nend\r\n\r\nglobal function setcursor(?visible)=\r\nreturn consolesw.setcursor(visible)\r\nend\r\n\r\nproc initcmds=\r\ncmdhistory::=()\r\nncmds:=cmdhistory.upb\r\ncmdindex:=0\r\nend\r\n\r\nglobal proc clearscreen(?bgnd,?fgnd)=\r\n\r\nif bgnd.isvoid then bgnd:=defscreenbgnd fi\r\nif fgnd.isvoid then fgnd:=defscreenfgnd fi\r\nsetcolour(fgnd,bgnd)\r\n\r\nfor i:=1 to screenrows do\r\n\tconsolesw.setpos(1,i)\r\n\tconsolesw.showtext(\" \"*screencols)\r\nod\r\nsetpos(1,1)\r\nend\r\n\r\nglobal proc clearwin(w)=\r\n!clear region used by listbox\r\n!can clear multi-columns at once\r\nspaces:=\" \"*w.cols\r\n\r\nsetcolour(w.fgnd,w.bgnd)\r\nfor i:=1 to w.rows do\r\n\tshowtext(spaces,w.posx,w.posy+i-1)\r\nod\r\nsetpos(w.posx,w.posy)\r\nend\r\n\r\nglobal function getkeyname(key)=\r\ncase key.keycode\r\nwhen vkleft then name:=\"left\"\r\nwhen vkright then name:=\"right\"\r\nwhen vkup then name:=\"up\"\r\nwhen vkdown then name:=\"down\"\r\nwhen vkpageup then name:=\"pageup\"\r\nwhen vkpagedown then name:=\"pagedown\"\r\nwhen vkhome then name:=\"home\"\r\nwhen vkend then name:=\"end\"\r\nwhen vkinsert then name:=\"insert\"\r\nwhen vkdelete then name:=\"delete\"\r\nwhen vktab then name:=\"tab\"\r\nwhen vkescape then name:=\"escape\"\r\nwhen vkbackspace then name:=\"backspace\"\r\nwhen vkenter then name:=\"enter\"\r\nwhen vkf1..vkf12 then name:=\"f\"+tostr(key.keycode-vkf1+1)\r\nwhen vkspace then name:=\"ispace\"\r\nelse\r\n\tif key.charcode in [1..26] then\t!ctrl code\r\n\t\tname:=chr(key.charcode+'a'-1)\r\n\telsif key.charcode in ['!','\"','£','%','^','&','*','(',')','-','_','+','=','[',']',\r\n\t'{','}',':',';','\\'','@','~','#','<','>',',','.','/','¬','¦','|','\\\\','?'] then\r\n\t\tname:=chr(key.charcode)\r\n\t\tkey.shift iand:=inot shiftmask\t\t!ignore any shift press needed to get char\r\n\r\n\telsif key.keycode in ['A'..'Z','0'..'9'] then\r\n\t\tif (key.shift iand (ctrlmask ior altmask))=0 then\r\n\t\t\tname:=chr(key.charcode)\r\n\t\t\tkey.shift iand:=inot shiftmask\r\n\t\telse\r\n\t\t\tname:=convlc(chr(key.keycode))\r\n\t\tfi\r\n\telsif key.keycode in (186..223) then\r\n\t\tcase key.keycode\r\n\t\twhen vkminus then name:=\"-\"\r\n\t\twhen vkequals then name:=\"=\"\r\n\t\twhen vklsq then name:=\"[\"\r\n\t\twhen vkrsq then name:=\"]\"\r\n\t\twhen vksemi then name:=\";\"\r\n\t\twhen vkquote then name:=\"'\"\r\n\t\twhen vkhash then name:=\"#\"\r\n\t\twhen vkcomma then name:=\",\"\r\n\t\twhen vkperiod then name:=\".\"\r\n\t\twhen vkslash then name:=\"/\"\r\n\t\twhen vkbackslash then name:=\"\\\\\"\r\n\t\twhen vkbackquote then name:=\"`\"\r\n\t\telse\r\n\t\t\treturn \"?\"\r\n\t\tesac\r\n\telse\r\n\t\treturn \"?\"\r\n\tfi\r\nesac\r\n\r\nprefix:=\"*\"\r\nif key.shift iand shiftmask then prefix+:=\"s\" fi\r\nif key.shift iand ctrlmask then prefix+:=\"c\" fi\r\nif key.shift iand altmask then prefix+:=\"a\" fi\r\nreturn prefix+name\r\n\r\nend\r\n\r\nglobal function keynametokey(name)=\r\n!given a key name in the format \"*...\", reconstruct an rkey record, and return that\r\ncharcode:=shift:=keycode:=0\r\n\r\nname:=rightstr(name,-1)\t\t!get rid of \"*\"\r\n\r\nif name.len=1 then\t\t!simple printable key, no shifts\r\n\tcharcode:=asc(name)\r\n\tgoto simplekey\r\n\r\nelse\t\t\t\t!any letters s,c,a on left indicate a modifier\r\n\twhile name.len>1 do\r\n\t\tcase leftstr(name)\r\n\t\twhen \"s\" then\r\n\t\t\tshift ior:=shiftmask\r\n\t\t\tname:=rightstr(name,-1)\r\n\t\twhen \"c\" then\r\n\t\t\tshift ior:=ctrlmask\r\n\t\t\tname:=rightstr(name,-1)\r\n\t\twhen \"a\" then\r\n\t\t\tshift ior:=altmask\r\n\t\t\tname:=rightstr(name,-1)\r\n\t\telse\r\n\t\t\texit\r\n\t\tesac\r\n\tod\r\n\r\n\tcase name\r\n\twhen \"left\" then keycode:=vkleft\r\n\twhen \"right\" then keycode:=vkright\r\n\twhen \"up\" then keycode:=vkup\r\n\twhen \"down\" then keycode:=vkdown\r\n\twhen \"pageup\" then keycode:=vkpageup\r\n\twhen \"pagedown\" then keycode:=vkpagedown\r\n\twhen \"home\" then keycode:=vkhome\r\n\twhen \"end\" then keycode:=vkend\r\n\twhen \"insert\" then keycode:=vkinsert\r\n\twhen \"delete\" then keycode:=vkdelete\r\n\twhen \"tab\" then keycode:=charcode:=vktab\r\n\twhen \"escape\" then keycode:=vkescape\r\n\twhen \"backspace\" then keycode:=charcode:=vkbackspace\r\n\twhen \"enter\" then keycode:=charcode:=vkenter\r\n\twhen \"ispace\" then keycode:=charcode:=vkspace\r\n\telse\r\n\t\tif name.len>=2 and leftstr(name)=\"f\" then\t!function key\r\n\t\t\tkeycode:=vkf1+strtoval(rightstr(name,-1))-1\r\n\t\telsif name.len=1 then\t\t\t\t!ordinary key, but with shifts\r\nsimplekey:\r\n\t\t\tc:=asc(name)\r\n\t\t\tcase c\r\n\t\t\twhen ['A'..'Z'] then\r\n\t\t\t\tkeycode:=c\r\n\t\t\twhen ['a'..'z'] then\r\n\t\t\t\tkeycode:=c-' '\r\n\t\t\twhen ['0'..'9'] then\r\n\t\t\t\tkeycode:=c\r\n\t\t\twhen '-','_' then keycode:=vkminus\r\n\t\t\twhen '=','+' then keycode:=vkequals\r\n\t\t\twhen '[','{' then keycode:=vklsq\r\n\t\t\twhen ']','}' then keycode:=vkrsq\r\n\t\t\twhen ';',':' then keycode:=vksemi\r\n\t\t\twhen '\\'','@' then keycode:=vkquote\r\n\t\t\twhen ',','<' then keycode:=vkcomma\r\n\t\t\twhen '.','>' then keycode:=vkperiod\r\n\t\t\twhen '/','?' then keycode:=vkslash\r\n\t\t\twhen '\\\\','|' then keycode:=vkbackslash\r\n\t\t\twhen '`','¬' then keycode:=vkbackquote\r\n\t\t\twhen '#','~' then keycode:=vkhash\r\n\t\t\twhen '!' then keycode:='1'\r\n\t\t\twhen '\"' then keycode:='2'\r\n\t\t\twhen '£' then keycode:='3'\r\n\t\t\twhen '$' then keycode:='4'\r\n\t\t\twhen '%' then keycode:='5'\r\n\t\t\twhen '^' then keycode:='6'\r\n\t\t\twhen '&' then keycode:='7'\r\n\t\t\twhen '*' then keycode:='8'\r\n\t\t\twhen '(' then keycode:='9'\r\n\t\t\twhen ')' then keycode:='0'\r\n\t\t\telse\r\n\t\t\t\tpcerror(\"keynametokey\")\r\n\t\t\tend\r\n\t\tfi\r\n\tesac\r\nfi\r\n\r\nif shift iand (altmask ior ctrlmask) then\r\n\tcharcode:=0\r\n\tif keycode in 'A'..'Z' then\r\n\t\tcharcode:=keycode-'@'\r\n\tfi\r\nfi\r\n\r\nkey:=new(rkey)\t\t\t!convert to proper keyrec\r\nkey.charcode:=charcode\r\nkey.shift:=shift\r\nkey.keycode:=keycode\r\nreturn key\r\nend\r\n\r\nglobal function makewin(pos, dims, ?fgnd,?bgnd,name=\"Anon\")=\r\n!global function makewin(pos, dims, ?colour)=\r\n\r\nw:=new(winrec)\r\nw.posx:=pos[1]\r\nw.posy:=pos[2]\r\nw.cols:=dims[1]\r\nw.rows:=dims[2]\r\nw.columns:=1\r\nif dims.len>=3 then\r\n\tw.columns:=dims[3]\r\nfi\r\n\r\n!CPL =POS,=DIMS,=W.COLUMNS\r\n\r\nw.itemcols:=w.cols%w.columns\r\nw.pagesize:=w.rows*w.columns\r\nw.hdata:=nil\r\n\r\nw.fgnd:=fgnd\r\nw.bgnd:=bgnd\r\nw.name:=name\r\n\r\nreturn w\r\nend\r\n\r\nglobal proc wsetpos(w,col,row)=\r\nsetpos(w.posx+col-1,w.posy+row-1)\r\nend\r\n\r\nglobal proc wshowtext(w,s,?col,?row)=\r\nif col.defined then\r\n\tshowtext(s,w.posx+col-1,w.posy+row-1)\r\nelse\r\n\tshowtext(s)\r\nfi\r\nend\r\n\r\nglobal function wgetpos(w)=\r\npos:=consolesw.getpos()\r\nreturn (w.posx+pos.lwb-1)..(w.posy+pos.upb-1)\r\nend\r\n\r\nglobal proc wsetcolour(w,?fgnd,?bgnd)=\r\nif fgnd.defined then\r\n\tsetcolour(fgnd,bgnd)\r\nelse\r\n\tsetcolour(w.fgnd,w.bgnd)\r\nfi\r\nend\r\n\r\nglobal proc wsetcolumns(w,columns)=\r\nw.columns:=columns\r\nw.itemcols:=w.cols%w.columns\r\nw.pagesize:=w.rows*w.columns\r\nend\r\n\r\nglobal proc wshowtext_b(w,s,col,fgnd,bgnd)=\r\n!version of wshowtext that dumps into char/attr buffer.\r\n!w is used for absolute column number\r\n\r\nlength:=s.len\r\noffset:=w.posx-1\t!hoz offset\r\n\r\nchardata.[(col+offset)..(col-1+length+offset)]:=s\r\n\r\n!CPL =colourmap,attrdata\r\n\r\nattr:=consolesw.colourmap[bgnd]<<4+consolesw.colourmap[fgnd]\r\n!attr:=bgnd<<4+fgnd\r\n\r\nattrdata.[(col+offset)..(col-1+length+offset)]:=chr(attr)*length\r\nend\r\n\r\nglobal proc updateconsolerow(row)=\r\n!write out latest contents to chardata/attrdata to console\r\n!this represents an entire composite wlineno+wvgap+wedit row, for given row within wedit\r\n!etc\r\nconsolesw.w_writeconsolerow(chardata,attrdata,screencols,row)\r\nend\r\n\r\n";
static char *	l_consolesw="import sys\r\n\r\nimport wincon\r\nimport lincon\r\n\r\n!global var hconsole, hconsolein\r\nglobal var wscreencols,wscreenrows\r\n!global var currbgnd=-1,currfgnd=-1\r\n!!console attribute colours (x16 for background)\r\n\r\n!global var screencolour=con_dkred..con_grey\r\n\r\nglobal var colourmap\r\n\r\nvar iswin=iswindows()\r\n\r\nproc start=\r\nCPL \"CONSOLESW\",=iswin\r\n\r\nif iswin then\r\n\tcolourmap:=wincon.colourmap\r\n\twscreencols:=wincon.wscreencols\r\n\twscreenrows:=wincon.wscreenrows\r\nelse\r\n\tcolourmap:=lincon.colourmap\r\n\twscreencols:=lincon.wscreencols\r\n\twscreenrows:=lincon.wscreenrows\r\nfi\r\nend\r\n\r\nglobal proc init(?cols)=\r\nif iswin then\r\n\twincon.init(cols)\r\nelse\r\n\tlincon.init(cols)\r\nfi\r\nend\r\n\r\nglobal proc setpos(col,row)=\r\nif iswin then\r\n\twincon.setpos(col,row)\r\nelse\r\n\tlincon.setpos(col,row)\r\nfi\r\nend\r\n\r\nglobal function getpos=\r\nif iswin then\r\n\treturn wincon.getpos()\r\nelse\r\n\treturn lincon.getpos()\r\nfi\r\nend\r\n\r\nglobal function setcursor(?visible)=\r\nif iswin then\r\n\treturn wincon.setcursor(visible)\r\nelse\r\n\treturn lincon.setcursor(visible)\r\nfi\r\nend\r\n\r\nglobal proc setcolour(fgnd,bgnd)=\r\nif iswin then\r\n\twincon.setcolour(fgnd,bgnd)\r\nelse\r\n\tlincon.setcolour(fgnd,bgnd)\r\nfi\r\nend\r\n\r\nglobal proc settitle(caption)=\r\nif iswin then\r\n\twincon.settitle(caption)\r\nelse\r\n\tlincon.settitle(caption)\r\nfi\r\nend\r\n\r\n!global function getkeychar=\r\n!!wait for any key, return single char code; as returned by C's getch()\r\n!return waitkey()\r\n!end\r\n\r\nglobal function getkey2=\r\nreturn PCERROR(\"SW/GETKEY2\")\r\nend\r\n\r\nglobal function getkey=\r\nif iswin then\r\n\treturn wincon.getkey()\r\nelse\r\n\treturn lincon.getkey()\r\nfi\r\nend\r\n\r\nglobal function keyready=\r\nif iswin then\r\n\treturn wincon.keyready()\r\nelse\r\n\treturn lincon.keyready()\r\nfi\r\nend\r\n\r\nglobal proc showtext(s)=\r\nif iswin then\r\n\twincon.showtext(s)\r\nelse\r\n\tlincon.showtext(s)\r\nfi\r\nend\r\n\r\nglobal proc setdims(cols,rows)=\r\nPCERROR(\"SW/SETDIMS\")\r\nend\r\n\r\nglobal proc setpalette(index,colour)=\r\nif iswin then\r\n\twincon.setpalette(index,colour)\r\nelse\r\n\tlincon.setpalette(index,colour)\r\nfi\r\nend\r\n\r\nglobal proc writepalette=\r\npcerror(\"SW/WRITEPALETTE\")\r\nend\r\n\r\nglobal proc flushkeyboard=\r\nif iswin then\r\n\twincon.flushkeyboard()\r\nelse\r\n\tlincon.flushkeyboard()\r\nfi\r\nend\r\n\r\nglobal proc w_writeconsolerow(text, attributes, length, row)=\r\nif iswin then\r\n\twincon.w_writeconsolerow(text,attributes,length,row)\r\nelse\r\n\tlincon.w_writeconsolerow(text,attributes,length,row)\r\nfi\r\nend\r\n";
static char *	l_wincon="import sys\r\n\r\nimport winconsts\r\nimport winapi\r\n\r\nimport condata\r\n\r\nglobal VAR SUPPRESS=0\r\n\r\nvar keypending=0\r\nvar lastkey\r\nvar pendkey\r\nglobal var hconsole, hconsolein\r\nvar colourpalette\r\n\r\n!const stdoutputhandle=0xffff_fff5i\r\n!const stdinputhandle=0xfffffff6i\r\n!const stderrorputhandle=0xfffffff4i\r\n!const invalidhandlevalue=0xffffffffi\r\n\r\n!global var screenwin\r\nglobal var wscreencols,wscreenrows\r\nglobal var currbgnd=-1,currfgnd=-1\r\n!!console attribute colours (x16 for background)\r\n\r\nglobal var screencolour=con_dkred..con_grey\r\n\r\nglobal var colourmap\r\n\r\n!var capslock,altdown,ctrldown,shiftdown\r\n!var keycode\t\t!virtual key code\r\n!var keyshift\t\t!combined shift settings: caps/alt/ctrl/shift\r\n!var keyfull\t\t!keyshift<<8+keycode in one 16-bit value\r\n!var keychar\t\t!char represented, if any\r\n!var keyscan\r\n!const capsmask=8h\t\t!shift states as they are in keyfull\r\n!const altmask=4h\r\n!const ctrlmask=2h\r\n!const shiftmask=1h\r\n!var keynames\r\n!var cmdindex,ncmds\r\n!var cmdhistory\r\n\r\nVAR ALLCHARS\r\n\r\nproc START=\r\nCPL \"WINCON START\"\r\nif iswindows() then\r\n\tinit()\r\nfi\r\nend\r\n\r\nproc main=\r\n!CPL \"HELLO\"\r\n\r\ninit()\r\nCPL \"WC DONE\",HCONSOLE\r\n\r\n!CPL \"WINCON\",RCONSOLEX.BYTES\r\n!WAITKEY()\r\n!STOP\r\n\r\n!!CPL \"W:CONSOLE INIT...........\"\r\n!currfgnd:=currbgnd:=-1\r\n!\r\n!init()\r\n!\r\n!\r\n!text:=\"AAAAAAAAAAAAAAAAAAAAAAAAAAA\"\r\n!attrs:=\"AAAAAAAAAAAAAAAAAAAAAAAAAAA\"\r\n!for i:=1 to 255 do\r\n!\tattrs:=chr(i)*text.len\r\n!\tw_writeconsolerow(text,attrs,text.len,4)\r\n!!\tto 10 million do od\r\n!od\r\nsettitle(\"New Title\")\r\n\r\nsetcolour(2,15)\r\nsetpos(40,50)\r\n!cpl \"The quick brown fox jumps over the lazy dog.\"\r\n!showtext(\"The quick brown fox jumps over the lazy dog.\")\r\nS:=\"The quick brown fox jumps over the lazy dog.\"\r\ncount:=0\r\n!writeconsole(hconsole,&s,s.len,&count,0)\r\nwriteconsole(hconsole,s,s.len,&count,0)\r\n!\r\n!init()\r\n!SETCOLOUR(4,5)\r\nCPL \"KEY:\"; waitkey()\r\nend\r\n\r\nglobal function makerspoint(x,y)=\r\n!combine x,y into 32-bit value (rspoint)\r\nreturn y<<16 ior x\r\nend\r\n\r\nglobal proc setpos(col,row)=\r\nsetconsolecursorposition(hconsole,makerspoint(col-1,row-1))\r\nend\r\n\r\nglobal function getpos=\r\ninfo:=new(ws_console)\r\ngetconsolescreenbufferinfo(hconsole,&info)\r\nreturn (info.pos.x+1,info.pos.y+1)\r\nend\r\n\r\nglobal proc init(cols=100)=\r\n!static var setdimdone=0\r\n\r\nhconsole:=getstdhandle(-11)\r\nhconsolein:=getstdhandle(-10)\r\nlastkey:=new(ws_keyevent)\r\nlastkey.repeatcount:=0\r\npendkey:=new(ws_keyevent)\r\n\r\n\r\n!if not setdimdone then\r\n\r\n!*!\tsetdims(cols,60)\r\n\r\n!ELSE\r\n!PCERROR(\"REPEAT INIT\")\r\n!fi\r\n!setdimdone:=1\r\n\r\ngetdims()\r\n\r\n!CPL =WSCREENCOLS,=WSCREENROWS; WAITKEY()\r\n\r\ncolourmap:=(@0: \\\r\n\t0,\t!black\tdk versions\r\n\t1,\t!blue\r\n\t4,\t!red\r\n\t5,\t!magenta\r\n\t2,\t!green\r\n\t3,\t!cyan\r\n\t6,\t!yellow\r\n\t7,\t!green\r\n\r\n\t8,\t!dkgrey\r\n\t9,\t!blue\tbright versions\r\n\t12,\r\n\t13,\r\n\t10,\r\n\t11,\r\n\t14,\r\n\t15)\r\n\r\ncolourpalette:=new(ws_palette16)\r\n\r\nsetstdpalette()\r\nend\r\n\r\nglobal function setcursor(?visible)=\r\ncursor:=new(ws_cursor)\r\ngetconsolecursorinfo(hconsole,&cursor)\r\n\r\nif visible.defined then\r\n\tcursor.visible:=visible\r\n\tsetconsolecursorinfo(hconsole,&cursor)\r\nfi\r\nreturn cursor.visible\r\nend\r\n\r\nglobal proc setcolour(fgnd,bgnd)=\r\n!call with as (fgnd,bgnd) or as (fgnd..bgnd)\r\n\r\nif fgnd=currfgnd and bgnd=currbgnd then\r\n\treturn\r\nfi\r\n\r\ncurrfgnd:=fgnd\r\ncurrbgnd:=bgnd\r\n\r\n!CPL =HCONSOLE\r\nsetconsoletextattribute(hconsole,(colourmap[bgnd]*16 + colourmap[fgnd]))\r\nend\r\n\r\nglobal proc settitle(caption)=\r\nsetconsoletitle(caption)\r\nend\r\n\r\nglobal function getkeychar=\r\n!wait for any key, return single char code; as returned by C's getch()\r\nreturn waitkey()\r\nend\r\n\r\nglobal function getkey2=\r\n!wait for any key, return keyrec\r\n!includes shift key presses as discrete keys\r\n!use getkey() to ignore these\r\n\r\nreturn getchx()\r\n\r\nk:=getchx()\t\t\t!get keyrec, encoded as int\r\n\r\nkey:=new(rkey)\t\t\t!convert to proper keyrec\r\nkey.charcode:=k iand 65535\r\nkey.shift:=k>>24\r\nkey.keycode:=k.[23..16]\r\n!CPL \"GK2:\",KEY\r\n\r\nreturn key\r\nend\r\n\r\nglobal function getkey=\r\n!calls igetkey but doesn't return shift keys as discrete key presses\r\ndo\r\n\tk:=getkey2()\r\n\tcase k.keycode\r\n\twhen vkshift,vkctrl,vkalt,vkcapslock then\r\n\telse\r\n\t\texit\r\n\tesac\r\nod\r\nreturn k\r\nend\r\n\r\nglobal function keyready=\r\nreturn testkey()\r\nend\r\n\r\n!global proc flushkbd=\r\n!flushkeys()\r\n!end\r\n\r\nglobal proc showtext(s)=\r\nif s then\r\n\tcount:=0\r\n\tif not suppress then\r\n\t\twriteconsole(hconsole,s,s.len,&count,0)\r\n\tfi\r\nfi\r\nend\r\n\r\nproc setwindowsize(cols,rows)=\r\nr:=new(ws_srect)\r\nr.leftx:=0\r\nr.rightx:=cols-1\r\nr.top:=0\r\nr.bottom:=rows-1\r\nif not setconsolewindowinfo(hconsole,1,&r) then\r\nCPL \"WINDOW ERROR 1\"\r\n!\tabort(\"Window error 1\")\r\nfi\r\nend\r\n\r\nglobal proc setdims(cols,rows)=\r\n!set new size for console, by reinitialising\r\n\r\nmaxcol:=cols\r\nmaxrow:=rows\r\n\r\n!PRINTLN \"HANDLE=\",HCONSOLE,COLS,ROWS\r\ninfo:=new(ws_console)\r\n!CPL \"*****SD2\"\r\n\r\n!getconsolescreenbufferinfo(hconsole,&info)\r\n!CPL \"*****SD3\"\r\noldscreenattributes:=info.attributes\r\n!CPL \"*****SD4\"\r\n\r\n!CPL =INFO\r\noldscreensize:=info.size\r\n\r\noldcols:=info.window.rightx-info.window.leftx+1\r\noldrows:=info.window.bottom-info.window.top+1\r\n\r\nIF OLDSCREENSIZE.X>COLS OR OLDSCREENSIZE.Y>ROWS THEN\t!need to reduce window size first\r\n!CPL \"OSS\",OLDSCREENSIZE,COLS,ROWS\r\n\tsetwindowsize(oldscreensize.x min cols, oldscreensize.y min rows)\r\nfi\r\n!CPL \"OLDSCREENSIZE=\",OLDSCREENSIZE\r\n!CPL \"OLDCOLS,OLDROWS=\",OLDCOLS,OLDROWS,\"//\",COLS,ROWS\r\n!WAITKEY\r\n!Set the new size of the entire (virtual) console window\r\nif setconsolescreenbuffersize(hconsole,rows<<16+cols)=0 then\r\n\tabort(\"Buffer size error\")\r\nfi\r\n\r\n!now set the size of the displayed portion of it; in this case exactly the same\r\n!size as the buffer, with no scrollbars\r\nsetwindowsize(cols,rows)\r\n\r\nwscreencols:=cols\r\nwscreenrows:=rows\r\n\r\n!cursordrawn:=0\r\n!cursorcol:=1\r\n!cursorrow:=1\r\n\r\n!hide blinking cursor\r\n!CPL \"ISD\"\r\ncursor:=new(ws_cursor)\r\ncursor.size:=10\r\ncursor.visible:=1\r\n!setconsolecursorinfo(hconsole,&cursor)\r\n\r\n!clearscreen()\r\n\r\n!lastkey:=new(rkeyevent,0)\r\n!lastkey.repeatcount:=0\r\n\r\nend\r\n\r\nglobal proc setpalette(index,colour)=\r\n!index is 0..15; colour is an rgb value bbggrr\r\n!updates local palette array\r\n!to update actual console, use writepalette\r\n!CPL \"SETSP\",INDEX,COLOUR:\"H\"\r\ncolourpalette[index]:=colour\r\n\r\nend\r\n\r\nglobal proc writepalette=\r\n!r:=new(rconsoleex)\r\n!r.recsize:=rconsoleex.bytes\r\n!X:=getconsolescreenbufferinfoex(hconsole,&r)\r\n!\r\n!r.palette:=colourpalette\r\n!\r\n!R.WINDOW.RIGHTX:=R.WINDOW.RIGHTX+1\t\t!workaround off-by-one bug\r\n!R.WINDOW.BOTTOM:=R.WINDOW.BOTTOM+1\r\n!\r\n!X:=setconsolescreenbufferinfoex(hconsole,&r)\r\nend\r\n\r\n!global proc READPALETTE=\r\n!r:=new(rconsoleex)\r\n!r.recsize:=rconsoleex.bytes\r\n!x:=getconsolescreenbufferinfoex(hconsole,&r)\r\n!\r\n!CPL \"GCSBI X=\",X\r\n!FOR I:=0 TO 15 DO\r\n! CPL I,\":\",R.PALETTE[I]:\"H\"\r\n!OD\r\n!\r\n!end\r\n\r\nproc setstdpalette=\r\ncols:=(\\\r\n(0,0,0),\r\n(0,0,128),\r\n(0,128,0),\r\n(0,128,128),\r\n(160,0,0),\r\n(128,0,128),\r\n(128,128,0),\r\n(192,192,192),\r\n(128,128,128),\r\n(0,0,255),\r\n(0,255,0),\r\n(0,255,255),\r\n(255,0,0),\r\n(255,0,255),\r\n!(255,255,0),\t\t!YELLOW\r\n(255,255,255),\t\t!YELLOW\r\n!(100,100,0),\t\t!YELLOW\r\n(224,224,224))\r\n!(216,216,200))\r\n!(255,255,255))\r\n\r\nforall i,c in cols do\r\n\tsetpalette(i-1,c[3]<<16+c[2]<<8+c[1])\r\nod\r\n!WRITEPALETTE()\r\n\r\nend\r\n\r\nproc getdims=\r\ninfo:=new(ws_console)\r\ngetconsolescreenbufferinfo(hconsole,&info)\r\n!return (info.pos.x+1,info.pos.y+1)\r\n\r\nwscreencols:=info.window.rightx-info.window.leftx+1\r\nwscreenrows:=info.window.bottom-info.window.top+1\r\nend\r\n\r\nglobal function getchx=\r\nconst rightaltmask\t= 1\t\t\t\t!masks used by .controlkeystate\r\nconst leftaltmask\t= 2\r\nconst leftctrlmask\t= 8\r\nconst rightctrlmask\t= 4\r\nconst shiftmask\t\t= 16\r\nconst capsmask\t\t= 128\r\nconst scrollmask\t= 64\r\n\r\nconst leftctrlbit\t= 3\t\t!for c.l.p\r\nconst rightctrlbit\t= 2\r\n\r\nif keypending then\r\n\tlastkey:=pendkey\r\n\tkeypending:=0\r\nelse\r\n\tif lastkey.repeatcount=0 then\r\n\t\trepeat\r\n\t\t\tcount:=0\r\n\t\t\treadconsoleinput(hconsolein,&lastkey,1,&count)\r\n\t\tuntil lastkey.eventtype=1 and lastkey.keydown=1\r\n\tfi\r\nfi\r\n\r\naltdown\t\t:= (lastkey.controlkeystate iand (leftaltmask ior rightaltmask)|1|0)\r\nctrldown\t:= (lastkey.controlkeystate iand (leftctrlmask ior rightctrlmask)|1|0)\r\nshiftdown\t:= (lastkey.controlkeystate iand shiftmask|1|0)\r\ncapslock\t:= (lastkey.controlkeystate iand capsmask|1|0)\r\n\r\nlastkey.repeatcount:=lastkey.repeatcount-1\r\n\r\ncharcode:=lastkey.asciichar\r\nkeycode:=lastkey.virtualkeycode iand 255\r\n\r\n!for keycodes in range 186 to 223, which are all stand-alone punctuation keys, I might\r\n!wish to set charcode to the appropriate printed char code (currently charcode will be\r\n!zero, and keyboard handlers need to detect keycodes such as vkequals)\r\n!\r\nif altdown and ctrldown and charcode=166 then\r\n\taltdown:=ctrldown:=0;\r\nelse\r\n\tif altdown or ctrldown then\r\n\t\tcharcode:=0;\r\n\t\tif keycode>='A' and keycode<= 'Z' then\r\n\t\t\tcharcode:=keycode-'@'\r\n\t\tfi\r\n\tfi\r\nfi\r\n\r\nkeyshift:=capslock<<3 ior altdown<<2 ior ctrldown<<1 ior shiftdown\r\n\r\nkeyshift.[4]:=lastkey.controlkeystate.[leftctrlbit]\t\t!for c.l.p\r\nkeyshift.[5]:=lastkey.controlkeystate.[rightctrlbit]\r\n\r\n!need to be more ruthless with how keycoded and charcodes are combined.\r\n!More combinations need to have only charcode or keycode set, and the other zero\r\n\r\n!CP \"RAW KEY EVENT\",=charcode, = keycode\r\nswitch charcode\r\nwhen 'A'..'Z','a'..'z','0'..'9' then\r\nwhen 8,9,13,27 then\r\nwhen 0 then\t\t\t\t!already key-only event\r\nelse\r\n\tkeycode:=0\r\nendswitch\r\n\r\nreturn rkey(charcode,keycode,keyshift)\r\n\r\nend\r\n\r\nglobal proc flushkeyboard=\r\nflushconsoleinputbuffer(hconsolein)\r\nend\r\n\r\nglobal proc w_writeconsolerow(text, attributes, length, row)=\r\nbuffersize:=1<<16+length\r\ncoord:=0\r\n\r\n\r\n!setpos(1,row)\r\n!print text\r\n!return\r\n\r\n!setpos(1,row)\r\n!for i:=1 to length-1 do\r\n!\tattrs:=attributes.[i]\r\n!\tc:=text.[i]\r\n!!\tsetcolour(attrs>>4, attrs iand 15)\r\n!\tsetcolour(attrs iand 15, attrs>>4)\r\n!\tprint chr(c)\r\n!od\r\n!return\r\n\r\nbox:=ws_srect(0,row-1,length-1,row-1)\r\n\r\nbuffer:=new(array,ws_charinfo,length)\r\n\r\nfor i:=1 to length do\r\n\tx:=new(ws_charinfo)\r\n\tx.asciichar  := text.[i]\r\n\tx.attributes := attributes.[i]\r\n\tbuffer[i]:=x\r\nod\r\n\r\nwriteconsoleoutputa(hconsole, &buffer,buffersize,coord,&box)\r\nend\r\n";
static char *	l_lincon="import sys\r\nimport condata\r\n!import newconsole\r\n\r\nvar digits=['0'..'9']\r\nvar navkeys=['A':vkup, 'B':vkdown, 'C': vkright, 'D':vkleft, 'H':vkhome, 'F':vkend,\r\n\t\t\t\t'P':vkf1, 'Q':vkf2, 'R': vkf3, 'S':vkf4]\r\n\r\nvar fnkeys= [15:vkf5, 17:vkf6, 18:vkf7, 19:vkf8, 20:vkf9, 21:vkf10, 23:vkf11, 24:vkf12]\r\n\r\nconst capsmask  = 0x8\t\t!shift states as they are in .keyshift\r\nconst altmask   = 0x4\r\nconst ctrlmask  = 0x2\r\nconst shiftmask = 0x1\r\n\r\nconst capsbit=3\r\nconst altbit=2\r\nconst ctrlbit=1\r\nconst shiftbit=0\r\n\r\nvar shiftcodes = [5:ctrlmask, 2:shiftmask, 3:altmask, 4:shiftmask+altmask, 7:ctrlmask+altmask]\r\n\r\n!global var screenwin\r\nglobal var wscreencols,wscreenrows\r\nglobal var currbgnd=-1,currfgnd=-1\r\n\r\n!global var screencolour=con_dkred..con_grey\r\nglobal var colourmap\r\nvar colourxref\r\n\r\nglobal proc init(cols=100)=\r\n!static var setdimdone=0\r\n\r\n!hconsole:=getstdhandle(-11)\r\n!hconsolein:=getstdhandle(-10)\r\n!lastkey:=new(ws_keyevent)\r\n!lastkey.repeatcount:=0\r\n!pendkey:=new(ws_keyevent)\r\n\r\n!setdims(cols,60)\r\n\r\nCPL \"LINCON INIT\"\r\n\r\ngetdims()\r\n\r\ncolourmap:=(@0: \\\r\n\t0,\t!black\tdk versions\r\n\t1,\t!blue\r\n\t4,\t!red\r\n\t5,\t!magenta\r\n\t2,\t!green\r\n\t3,\t!cyan\r\n\t6,\t!yellow\r\n\t7,\t!green\r\n\r\n\t8,\t!dkgrey\r\n\t9,\t!blue\tbright versions\r\n\t12,\r\n\t13,\r\n\t10,\r\n\t11,\r\n\t14,\r\n\t15)\r\n\r\ncolourxref:=(0:\\\t\t!windows colour numbers to linux (or nearest equiv)\r\n\t0,\r\n\t4,\r\n\t1,\r\n\t5,\r\n\t2,\r\n\t14,\r\n\t3,\r\n\t7,\r\n\t8,\r\n\t12,\r\n\t9,\r\n\t13,\r\n\t10,\r\n\t14,\r\n\t5,\r\n\t15)\r\n\r\n\r\n\r\n\r\n!colourpalette:=new(ws_palette16)\r\n!\r\n!setstdpalette()\r\nend\r\n\r\nproc getdims=\r\n(wscreencols,wscreenrows):=getscreensize()\r\nend\r\n\r\nglobal proc setpos(column,row)=\r\nfprint \"\\s[#;#H\",row,column\r\nend\r\n\r\nproc setfgndcol(colour)=\r\nfprint \"\\s[#m\",colour+30\r\nend\r\n\r\nproc setbgndcol(colour)=\r\nfprint \"\\s[#m\",colour+40\r\nend\r\n\r\nglobal proc setbold(bold)=\r\nfprint \"\\s[#m\",(bold|1|21)\r\nend\r\n\r\nglobal proc setitalic(italic)=\r\nfprint \"\\s[#m\",(italic|3|23)\r\nend\r\n\r\nglobal function getpos=\r\nprint \"\\s[6n\"\r\nreadkey()\t\t!escape\r\nreadkey()\t\t![\r\n\r\n(row,column,c):=readkbdsequence()\r\nreturn (column,row)\r\nend\r\n\r\nglobal function setcursor(?visible)=\r\nreturn 1\r\nend\r\n\r\nglobal proc setcolour(fgnd,bgnd)=\r\n!call with as (fgnd,bgnd) or as (fgnd..bgnd)\r\n\r\nif fgnd=currfgnd and bgnd=currbgnd then\r\n\treturn\r\nfi\r\n\r\ncurrfgnd:=fgnd\r\ncurrbgnd:=bgnd\r\n\r\nfgnd:=colourxref[fgnd]\r\nbgnd:=colourxref[bgnd]\r\n\r\nsetbold(fgnd>7)\r\nsetfgndcol(fgnd iand 7)\r\nsetbgndcol(bgnd iand 7)\r\nend\r\n\r\nglobal proc settitle(caption)=\r\nend\r\n\r\nglobal function keyready=\r\nreturn pcerror(\"Linux/keyready\")\r\nend\r\n\r\nglobal proc showtext(s)=\r\nif s then\r\n\tprint s\r\nfi\r\nend\r\n\r\nglobal proc setdims(cols,rows)=\r\npcerror(\"linux/setdims\")\r\nend\r\n\r\nglobal proc setpalette(index,colour)=\r\npcerror(\"linux/setpallete\")\r\nend\r\n\r\nfunction getscreensize=\r\nsavepos()\r\nsetpos(999,999)\r\n(cols,rows):=getpos()\r\nrestorepos()\r\nreturn (cols,rows)\r\nend\r\n\r\nproc savepos=\r\nprint \"\\s[s\"\r\nend\r\n\r\nproc restorepos=\r\nprint \"\\s[u\"\r\nend\r\n\r\nfunction readkey=\r\nreturn waitkey()\r\nend\r\n\r\nfunction readintseq(c)=\r\n!c is '0' to '9'\r\n!read integer sequence up to first non-digit\r\n!return (number, terminator character)\r\nx:=c-'0'\r\ndo\r\n\tc:=readkey()\r\n\tif c in digits then\r\n\t\tx:=x*10+c-'0'\r\n\telse\r\n\t\texit\r\n\tfi\r\nod\r\nreturn (x,c)\r\nend\r\n\r\nfunction readkbdsequence=\r\n!Some key escape sequences for control chars in Linux look like this:\r\n! <esc> \"[\" [x[\";\"y] c/\"~\"\r\n!Parts in \"...\" are actual characters\r\n!x and y are optional integers, c is a capital letter\r\n!The sequence may have 0, 1 or 2 numbers (separated with ;) and end with\r\n!a capital letter, or \"~\"\r\n!the \"[\" has already been read\r\n!return (X, Y, C)\r\n!X or Y will be zero if not present. C will 'A' etc, or 0 if it ends with \"~\"\r\n!-1 is returned on error\r\n\r\nx:=y:=0\r\n\r\nc:=readkey()\r\n\r\nif c in digits then\r\n\t(x,c):=readintseq(c)\r\n\tif c=';' then\r\n\t\tc:=readkey()\r\n\t\tif c not in digits then return -1 fi\r\n\t\t(y,c):=readintseq(c)\r\n\tfi\r\nfi\r\n\r\nif c='~' then\r\n\treturn (x,y,0)\r\nfi\r\nreturn (x,y,c)\t\t\t\t!assume A-Z\r\nend\r\n\r\n!function keyname(k,shift=0)=\r\n!return getkeyname(rkey(0,k,shift))\r\n!end\r\n\r\nglobal function getkey=\r\n!read key events via readkey()\r\n!convert escape sequences to Windows virtual keys\r\n\r\nk:=readkey()\t\t\t\t!LINUX ONLY\r\n\r\ncase k\r\nwhen 10 then\r\n\treturn rkey(13,vkenter,0)\r\nwhen 8,127 then\r\n\treturn rkey(127,vkbackspace,0)\r\nwhen 9 then\r\nCPL \"TAB1\"\r\n\treturn rkey(vktab,vktab,0)\r\nwhen 'A'..'Z','0'..'9' then\r\n\treturn rkey(k,k,0)\r\nwhen 'a'..'z' then\r\n\treturn rkey(k,k-' ',0)\r\nwhen 27 then\r\nwhen 1..31 then\r\n\treturn rkey(k,0,ctrlmask)\r\nelse\r\n\tc:=k\r\n\tcase k\r\n\twhen '[','{' then k:=vklsq\r\n\twhen ']','}' then k:=vkrsq\r\n\telse\r\n\t\tk:=0\r\n\tesac\r\n\r\n\treturn rkey(c,k,0)\r\nesac\r\n\r\n!CPL \"ESC SEEN\"\r\n\r\n!escape seen; look at next key\r\nk:=readkey()\r\n\r\ncase k\r\nwhen 27 then\t\t\t!esc/esc => single escape\r\n\treturn rkey(0,k,0)\r\n\r\nwhen 10 then\t\t\t!esc/10 => alt enter\r\n\treturn rkey(0,vkenter,altmask)\r\n\r\nwhen 8,127 then\t\t\t!esc/bs => alt bs\r\n\treturn rkey(0,vkbackspace,altmask)\r\n\r\nwhen 'O' then\t\t\t!short set of function keys\r\n\t(x,y,c):=readkbdsequence()\r\n\r\nCPL \"O\",x,y,chr(c)\r\n\treturn rkey(0,navkeys{c},shiftcodes{y,0})\r\n\r\nwhen '[' then\r\n\t(x,y,c):=readkbdsequence()\r\n\tcase c\r\n\twhen 'Z' then\t\t\t\t\t\t!shift+tab\r\n\t\treturn rkey(9,9,shiftmask)\r\n!\t\treturn rkey(0,9,shiftmask)\r\n\twhen 'A','B','C','D','H','F','P','Q','R','S' then\t\t!cursor keys, fn1..4; assume x=1\r\n\t\treturn rkey(0,navkeys{c},shiftcodes{y,0})\r\n\tesac\r\n\r\n\tcase x\r\n\twhen 2,3,5,6 then\r\n\t\tshift:=0\r\n\t\tcase y\r\n\t\twhen 5 then shift:=ctrlmask\r\n\t\twhen 3 then shift:=altmask\r\n\t\twhen 7 then shift:=altmask+ctrlmask\r\n\t\tesac\r\n\t\treturn rkey(0,(x|0,vkinsert,vkdelete,0,vkpageup|vkpagedown),shift)\r\n\twhen 15..24 then\r\n\t\treturn rkey(0,fnkeys{x},shiftcodes{y,0})\r\n\tesac\r\n\r\nwhen 'A'..'Z' then\t\t\t!must have been alt version (some esc letter codes above)\r\n\treturn rkey(k-64,0,altmask) \r\n\r\nwhen 'a'..'z' then\t\t\t!must have been alt version (some esc letter codes above)\r\n\treturn rkey(k-96,0,altmask) \r\n\r\nwhen '0'..'9' then\r\n\treturn rkey(0,k,altmask) \r\n\r\nesac\r\nCPL \"ESC 91\"\r\n\r\n!Code 91 SEEN\r\nreturn rkey(0,'?',0)\r\nend\r\n\r\nproc screentest=\r\n\r\nsavepos()\r\nsetpos(10,10)\r\nsetfgndcol(5)\r\nsetbgndcol(3)\r\nsetbold(1)\r\nsetitalic(1)\r\nprintln \"\tHELLO\t\"\r\nsetbold(0)\r\nsetitalic(0)\r\nrestorepos()\r\nprintln \"\tGoodbye\t\"\r\n\r\n(cols,rows):=getscreensize()\r\ncpl =rows,=cols\r\nwaitkey()\r\nend\r\n\r\n!proc keytest=\r\n!\r\n!lastkey:=0\r\n!\r\n!!do\r\n!!\tk:=readkey()\r\n!!\tif k=27 and lastkey=27 then exit fi\r\n!!\tif k=27 then\r\n!!\t\tcpl\r\n!!\t\tcp \"ESC \"\r\n!!\telsif k in 32..126 then\r\n!!\t\tcp chr(k)\r\n!!\telse\r\n!!\t\tcp \"<\"+tostr(k)+\">\"\r\n!!\tfi\r\n!!\tlastkey:=k\r\n!!od\r\n!\r\n!do\r\n!\tk:=getkey()\r\n!\tcpl getkeyname(k),k\r\n!\tif k.keycode=27 then exit fi\r\n!od\r\n!\r\n!end\r\n\r\nproc keyscreentest=\r\n(cols,rows):=getscreensize()\r\nCPL =COLS,=ROWS\r\n\r\nrow:=rows%2\r\ncol:=cols%2\r\nch:=\"X\"\r\n\r\nsetfgndcol(6)\r\nsetbgndcol(1)\r\n\r\ndo\r\n\tsetpos(col,row)\r\n\tcp ch\r\n\tsetpos(col,row)\r\n\tk:=getkey().keycode\r\n\tcase k\r\n\twhen 27 then\r\n\t\texit\r\n\twhen vkleft then col:=max(1,col-1)\r\n\twhen vkright then col:=min(cols,col+1)\r\n\twhen vkup then row:=max(1,row-1)\r\n\twhen vkdown then row:=min(rows,row+1)\r\n\tesac\r\nod\r\n\r\n!waitkey()\r\n\r\nend\r\n\r\nproc main=\r\n\r\n!keytest()\r\n!screentest()\r\nkeyscreentest()\r\nend\r\n\r\nproc start=\r\nCPL \"LINCON START\"\r\nif not iswindows() then\r\n\tinit()\r\nfi\r\nend\r\n\r\nglobal proc w_writeconsolerow(text, attributes, length, row)=\r\n!pcerror(\"lincon/writeconsolerow\")\r\n!buffersize:=1<<16+length\r\n!coord:=0\r\n\r\n!setpos(1,row)\r\n!print leftstr(text,length)\r\n\r\nsetpos(1,row)\r\nfor i:=1 to length-1 do\r\n\tattrs:=attributes.[i]\r\n\tc:=text.[i]\r\n!\tsetcolour(attrs>>4, attrs iand 15)\r\n\tsetcolour(attrs iand 15, attrs>>4)\r\n\tprint chr(c)\r\nod\r\nreturn\r\n\r\nend\r\n\r\nglobal proc flushkeyboard=\r\n!pcerror(\"lincon/flushkeyboard\")\r\nend\r\n\r\n";
static char *	l_condata="!!Virtual keycodes\r\nglobal const vklbutton=1\t\t!note these are physical not logical buttons\r\nglobal const vkrbutton=2\r\nglobal const vkmbutton=4\t\t!middle button is correct\r\nglobal const vkbackspace=8\r\nglobal const vktab=9\r\nglobal const vkclear=12\r\nglobal const vkenter=13\r\nglobal const vkshift=16\r\nglobal const vkctrl=17\r\nglobal const vkalt=18\r\nglobal const vkbreak=19\r\nglobal const vkcapslock=20\r\n!global const vkrshift=21\r\nglobal const vkrctrl=22\r\nglobal const vkralt=23\r\nglobal const vkinslock=24\r\nglobal const vkescape=27\r\nglobal const vkspace=32\r\nglobal const vkpageup=33\r\nglobal const vkpagedown=34\r\nglobal const vkend=35\r\nglobal const vkhome=36\r\nglobal const vkleft=37\r\nglobal const vkup=38\r\nglobal const vkright=39\r\nglobal const vkdown=40\r\nglobal const vkinsert=45\r\nglobal const vkdelete=46\r\nglobal const vkhelp=47\r\nglobal const vk0='0'\r\nglobal const vka='A'\r\nglobal const vkwindows=91\r\nglobal const vkrightbutton=93\r\nglobal const vknumpad0=96\r\nglobal const vkmul=106\r\nglobal const vkadd=107\r\nglobal const vksub=109\r\nglobal const vkdecimal=110\r\nglobal const vkdiv=111\r\nglobal const vkf1=112\r\nglobal const vkf2=113\r\nglobal const vkf3=114\r\nglobal const vkf4=115\r\nglobal const vkf5=116\r\nglobal const vkf6=117\r\nglobal const vkf7=118\r\nglobal const vkf8=119\r\nglobal const vkf9=120\r\nglobal const vkf10=121\r\nglobal const vkf11=122\r\nglobal const vkf12=123\r\n!global const vklsq=128\r\n!global const vkrsq=129\r\n!global const vksemi=130\r\n!global const vkquote=131\r\n!global const vkstroke=132\r\n!global const vkdot=133\r\n!global const vkcomma=134\r\n!global const vkbackslash=135\r\n!global const vkquote2=136\r\n!global const vkequals=137\r\n!global const vkminus=138\r\n!global const vkhash=139\r\nglobal const vklshift=160\r\nglobal const vkrshift=161\r\nglobal const vklcontrol=162\r\nglobal const vkrcontrol=163\r\n\r\n!oem codes\r\nglobal const vkminus=189\r\nglobal const vkequals=187\r\nglobal const vklsq=219\r\nglobal const vkrsq=221\r\nglobal const vksemi=186\r\nglobal const vkquote=192\r\nglobal const vkhash=222\r\nglobal const vkcomma=188\r\nglobal const vkperiod=190\r\nglobal const vkslash=191\r\nglobal const vkbackslash=220\r\nglobal const vkbackquote=223\r\n\r\n!!for m system using ncurses, capskey shift is not used\r\n!!other shift values added to vk code to give a 16-bit virtual key code, but that\r\n!!is only used for non-printable characters\r\n\r\n!global const capskey=8h\t\t!shift states\r\n!global const altkey=4h\r\n!global const ctrlkey=2h\r\n!global const shiftkey=1h\r\n!\r\n\r\n\r\nglobal const con_black=0\r\nglobal const con_dkblue=1\r\nglobal const con_dkred=2\r\nglobal const con_dkmagenta=3\r\nglobal const con_dkgreen=4\r\nglobal const con_dkcyan=5\r\nglobal const con_dkyellow=6\r\nglobal const con_grey=7\r\nglobal const con_dkgrey=8\r\nglobal const con_blue=9\r\nglobal const con_red=10\r\nglobal const con_magenta=11\r\nglobal const con_green=12\r\nglobal const con_cyan=13\r\nglobal const con_yellow=14\r\nglobal const con_white=15\r\n";
static char *	l_winapi="!export\r\ndefine wt_word\t\t= word16\r\ndefine wt_bool\t\t= word32\r\ndefine wt_dword\t\t= word32\r\ndefine wt_wchar\t\t= word16\r\ndefine wt_char\t\t= byte\r\ndefine wt_ichar\t\t= string\r\ndefine wt_string\t= string\r\ntype wt_ptr\t\t= ref byte\r\n!type wt_wndproc\t= ref proc\r\n!type wt_wndproc\t= ref byte\r\ntype wt_wndproc\t= wordm\r\n\r\ndefine wt_handle\t= refm\r\ndefine wt_int\t\t= int32\r\ndefine wt_uint\t\t= word32\r\ndefine wt_long\t\t= int32\r\ndefine wt_wparam\t= wordm\r\ndefine wt_lparam\t= wordm\r\ndefine wt_point\t\t= rpoint\r\ndefine wt_size\t\t= wordm\r\n\r\ndefine wt_wparam32\t= word32\r\ndefine wt_lparam32\t= word32\r\ndefine wt_handle32\t= word32\r\ndefine wt_ptr32\t\t= word32\r\ndefine wt_string32\t= word32\r\ndefine wt_wndproc32\t= word32\r\n\r\ndefine wt_wparam64\t= word64\r\ndefine wt_lparam64\t= word64\r\ndefine wt_handle64\t= word64\r\ndefine wt_ptr64\t\t= word64\r\ndefine wt_string64\t= word64\r\ndefine wt_wndproc64\t= word64\r\n\r\n\r\ndefine wt_result\t= wordm\r\ndefine wt_intptr\t= wordm\r\ndefine wt_coord\t\t= wt_dword\r\n\r\n!define winfn=windows function\r\n!define windows proc=windows proc\r\n\r\n!endexport\r\n\r\nglobal type ws_spoint= struct\r\n\tint16 x,y\r\nend\r\n\r\nglobal type ws_srect=struct\t\t!rect record occupying 8 bytes\r\n\tint16 leftx,top, rightx,bottom\r\nend\r\n\r\nglobal type ws_charinfo=struct\r\n\tunion\r\n\t\twt_word\tunicodechar\r\n\t\twt_char\tasciichar\r\n\tend union\r\n\twt_word\t\tattributes\r\nend\r\n\r\nglobal type ws_palette16=[0..15]int32\r\n\r\nglobal type ws_console=struct\r\n\tws_spoint size,pos\r\n\twt_word attributes\r\n\tws_srect window\r\n\tws_spoint maxwindowsize\r\nend\r\n\r\nglobal type ws_consoleex=struct\r\n\tint32 recsize\r\n\tws_spoint size,pos\r\n\twt_word attributes\r\n\tws_srect window\r\n\tws_spoint maxwindowsize\r\n\twt_word wpopup\r\n\tint32 fullscreen\r\n\tws_palette16 palette\r\nend\r\n\r\nglobal type ws_keyevent = struct\r\n\twt_word\teventtype\r\n\t\twt_bool\tkeydown\t\t\t@@4\t!key event record (was inside 'Event' union in win32)\r\n\t\twt_word\trepeatcount\r\n\t\twt_word\tvirtualkeycode\r\n\t\twt_word\tvirtualscancode\r\n\t\tunion\r\n\t\t\twt_word unicodechar\r\n\t\t\twt_char asciichar\r\n\t\tend\r\n\t\twt_dword controlkeystate\r\nend\r\n\r\nglobal type ws_cursor=struct(int32 size,visible)\r\n\r\nglobal var hconsole, hconsolein\r\n\r\nglobal const stdoutputhandle=0xffff_fff5i\r\nglobal const stdinputhandle=0xfffffff6i\r\nglobal const stderrorputhandle=0xfffffff4i\r\nglobal const invalidhandlevalue=0xffffffffi\r\n\r\nglobal const maxpathlen=260\r\n\r\ntype spath=stringz*maxpathlen\r\ntype sshort=stringz*14\r\n!\r\nglobal type ws_filetime=struct\r\n\tint32 ftlow\r\n\tint32 fthigh\r\nend\r\n\r\nglobal type ws_finddata=struct\r\n\tint32\t\tfileattributes\r\n\tws_filetime\tcreationtime\r\n\tws_filetime\tlastaccesstime\r\n\tws_filetime\tlastwritetime\r\n\tint32\t\tfilesizehigh\r\n\tint32\t\tfilesizelow\r\n\tint32\t\treserved0\r\n\tint32\t\treserved1\r\n\tspath\t\tfilename\r\n\tsshort\t\tshortfilename\r\nend\r\n\r\nglobal type ws_systemtime = struct\r\n\tword16\tyear\r\n\tword16\tmonth\r\n\tword16\tdayofweek\r\n\tword16\tday\r\n\tword16\thour\r\n\tword16\tminute\r\n\tword16\tsecond\r\n\tword16\tmilliseconds\r\nend\r\n\r\nglobal type ws_msg32 = struct\r\n\tint32\thwnd\r\n\tint32\tmessage\r\n\tint32\twparam\r\n\tint32\tlparam\r\n\tint32\ttime\r\n\tint32\tptx\r\n\tint32\tpty\r\nend\r\n\r\nglobal type ws_msg64 = struct@@8\r\n\tint64\thwnd\r\n\tint32\tmessage\r\n\tint64\twparam@@0\r\n\tint64\tlparam\r\n\tint32\ttime\r\n\tint32\tptx\t@@8\r\n\tint32\tpty\r\nend\r\n\r\nglobal type ws_point = struct\r\n\tint32 x, y\r\nend\r\n\r\nglobal type ws_rect=struct\t\t!rect record occupying 16 bytes\r\n\tint32 leftx,top, rightx,bottom\r\n\tint32\tx@leftx\r\n\tint32\ty@top\r\n\tint32\tx2@rightx\r\n\tint32\ty2@bottom\r\n\tint32\tx1@leftx\r\n\tint32\ty1@top\r\nend\r\n\r\nglobal type ws_logbrush = struct\r\n\tint32 lbstyle\r\n\tint32 lbcolour\r\n\tint32 lbhatch\r\nend\r\n\r\nglobal type ws_textmetrics = struct\r\n\tint32\theight\r\n\tint32\tascent\r\n\tint32\tdescent\r\n\tint32\tint32ernalleading\r\n\tint32\texternalleading\r\n\tint32\tavecharwidth\r\n\tint32\tmaxcharwidth\r\n\tint32\tweight\r\n\tint32\toverhang\r\n\tint32\tdigitizedaspectx\r\n\tint32\tdigitizedaspecty\r\n\tbyte\tfirstchar\r\n\tbyte\tlastchar\r\n\tbyte\tdefaultchar\r\n\tbyte\tbreakchar\r\n\tbyte\titalic\r\n\tbyte\tunderlined\r\n\tbyte\tstruckout\r\n\tbyte\tpitchandfamily\r\n\tbyte\tcharset\r\nend\r\n!=========================================\r\n\r\nglobal type ws_bitmapv5header = struct\r\n\tint32\tsize\r\n\tint32\twidth\r\n\tint32\theight\r\n\tword16\tplanes\r\n\tword16\tbitcount\r\n\tint32\tcompression\r\n\tint32\tsizeimage\r\n\tint32\txpelspermeter\r\n\tint32\typelspermeter\r\n\tint32\tclrused\r\n\tint32\tclrimportant\r\n\tint32\tredmask\r\n\tint32\tgreenmask\r\n\tint32\tbluemask\r\n\tint32\talphamask\r\n\tint32\tcstype\r\n\t[1..9]int32 endpoints\r\n\tint32\tredgamma\r\n\tint32\tgreengamma\r\n\tint32\tbluegamma\r\n\tint32\tintent\r\n\tint32\tprofiledata\r\n\tint32\tprofilesize\r\n\tint32\treserved\r\nend\r\n\r\nglobal type ws_paintstruct = struct\r\n!\tintm\t\thdc\r\n\tint64\t\thdc\r\n\tint32\t\terase\r\n\tws_rect\t\tpaintrect\r\n\tint32\t\trestore\r\n\tint32\t\tincupdate\r\n\t[32]byte\trgbreserved\r\nend\r\n\r\n!32-BIT VERSION\r\nglobal type ws_openfilename32 = struct\r\n\twt_dword\t\tstructsize\r\n\twt_handle32\t\towner\r\n\twt_handle32\t\tinstance\r\n\twt_string32\t\tfilter\r\n\twt_string32\t\tcustomfilter\r\n\twt_dword\t\tmaxcustfilter\r\n\twt_dword\t\tfilterindex\r\n\twt_string32\t\tfile\r\n\twt_dword\t\tmaxfile\r\n\twt_string32\t\tfiletitle\r\n\twt_dword\t\tmaxfiletitle\r\n\twt_string32\t\tinitialdir\r\n\twt_string32\t\ttitle\r\n\twt_dword\t\tflags\r\n\twt_word\t\t\tfileoffset\r\n\twt_word\t\t\tfileextension\r\n\twt_string32\t\tdefext\r\n\twt_lparam32\t\tcustdata\r\n\twt_wndproc32\thook\r\n\twt_string32\t\ttemplatename\r\n\twt_ptr32\t\treserved1\r\n\twt_dword\t\treserved2\r\n\twt_dword\t\tflagsex\r\nend\r\n\r\n!64-BIT VERSION\r\nglobal type ws_openfilename64 = struct @@8\r\n\twt_dword\t\tstructsize\r\n\twt_handle64\t\towner@@0\r\n\twt_handle64\t\tinstance\r\n\twt_string64\t\tfilter\r\n\twt_string64\t\tcustomfilter\r\n\twt_dword\t\tmaxcustfilter\r\n\twt_dword\t\tfilterindex\r\n\twt_string64\t\tfile@@0\r\n\twt_dword\t\tmaxfile\r\n\twt_string64\t\tfiletitle@@0\r\n\twt_dword\t\tmaxfiletitle\r\n\twt_string64\t\tinitialdir@@0\r\n\twt_string64\t\ttitle\r\n\twt_dword\t\tflags\r\n\twt_word\t\t\tfileoffset\r\n\twt_word\t\t\tfileextension\r\n\twt_string64\t\tdefext@@0\r\n\twt_lparam64\t\tcustdata\r\n\twt_wndproc64\thook\r\n\twt_string64\t\ttemplatename\r\n\twt_ptr64\t\treserved1\r\n\twt_dword\t\treserved2\r\n\twt_dword\t\tflagsex\r\nend\r\n\r\n!export\r\nimportdll kernel32=\r\n\twindows function\t\"GetLastError\"\t\t\t\t\t:wt_dword\r\n\twindows function\t\"GetStdHandle\"\t\t\t\t\t(wt_dword)wt_handle\r\n\twindows function\t\"WriteConsoleA\"\t\t\t\t(wt_handle,wt_ptr,wt_dword,wt_ptr,wt_ptr)wt_bool\r\n\twindows function\t\"SetConsoleCursorPosition\"\t\t(wt_handle,wt_coord)wt_bool\r\n\twindows function\t\"GetConsoleScreenBufferInfo\"\t(wt_handle,wt_ptr)wt_bool\r\n\twindows function\t\"SetConsoleMode\"\t\t\t\t(wt_handle,wt_dword)wt_bool\r\n\twindows function\t\"WriteConsoleOutputA\"\t\t\t(wt_handle,wt_ptr,wt_coord,wt_coord,wt_ptr)wt_bool\r\n\r\n\twindows function\t\"GetConsoleScreenBufferInfoEx\"\t(wt_handle,wt_ptr)wt_bool\r\n\r\n\twindows function\t\"SetConsoleTextAttribute\"\t\t(wt_handle,wt_word)wt_bool\r\n\twindows function\t\"SetConsoleTitleA\"\t\t\t\t(wt_string)wt_bool\r\n\twindows function\t\"ReadConsoleInputA\"\t\t\t(wt_handle,wt_ptr,wt_dword,wt_ptr)wt_bool\r\n\twindows function\t\"PeekConsoleInputA\"\t\t\t(wt_handle,wt_ptr,wt_dword,wt_ptr)wt_bool\r\n\twindows function\t\"FlushConsoleInputBuffer\"\t\t(wt_handle)wt_bool\r\n\twindows function\t\"SetConsoleWindowInfo\"\t\t\t(wt_handle,wt_bool,wt_ptr)wt_bool\r\n\twindows function\t\"SetConsoleScreenBufferSize\"\t(wt_handle,wt_coord)wt_bool\r\n\twindows function\t\"GetConsoleCursorInfo\"\t\t\t(wt_handle,wt_ptr)wt_bool\r\n\twindows function\t\"SetConsoleCursorInfo\"\t\t\t(wt_handle,wt_ptr)wt_bool\r\n\twindows function\t\"GetNumberOfConsoleInputEvents\"(wt_handle,wt_ptr)wt_bool\r\n\r\n\tdefine writeconsole=writeconsolea\r\n\tdefine setconsoletitle=setconsoletitlea\r\n\tdefine readconsoleinput=readconsoleinputa\r\n\tdefine peekconsoleinput=peekconsoleinputa\r\n\twindows function\t\"FindFirstFileA\" as findfirstfile\t\t(string,ref int32)int32\r\n\twindows function\t\"FindNextFileA\"  as findnextfile\t\t\t(int32,ref int32)int32\r\n\twindows function\t\"FindClose\"\t\t\t\t\t(int32)int32\r\n\twindows function\t\"SetCurrentDirectoryA\" as setcurrentdirectory\t(string)int32\r\n\twindows function\t\"GetCurrentDirectoryA\" as getcurrentdirectory\t(int32,int32)int32\r\n\twindows function\t\"CreateDirectoryA\" as createdirectory\t\t(string,int32)int32\r\n\twindows function\t\"GetFileAttributesA\"\t\t\t(string)int32\r\n\twindows function\t\"GetModuleHandleA\" as getmodulehandle\t\t(wt_string)wt_handle\r\n\twindows function\t\"GetTickCount\"\t\t\t\t\t\t\t\t:wt_dword\r\n\twindows function\t\"GlobalAlloc\"\t\t\t\t\t\t\t\t\t(wt_uint,wt_size)wt_handle\r\n\twindows function\t\"GlobalLock\"\t\t\t\t\t\t\t\t\t(wt_handle)wt_ptr\r\n\twindows function\t\"GlobalUnlock\"\t\t\t\t\t\t\t\t(wt_handle)wt_bool\r\n\twindows function\t\"GlobalSize\"\t\t\t\t\t\t\t\t\t(wt_handle)wt_size\r\n\r\n\twindows function\t\"GetSystemTime\"(ref byte)int32\r\n\twindows function\t\"Beep\"\t\t\t\t\t\t\t(wt_dword, wt_dword)wt_bool\r\n\twindows function\t\"SetConsoleCP\"\t\t\t\t\t\t\t\t(wt_uint)wt_bool\r\nend\r\n\r\nimportdll user32=\r\n\twindows function\t\"CreateWindowExA\" as createwindowex\t\t(wt_dword, wt_string, wt_string, wt_dword, wt_int,wt_int,wt_int,wt_int,\r\n\t\t\t\t\t\t\t\t\t\t\t\t\t wt_handle, wt_handle, wt_handle, wt_ptr)wt_handle\r\n\r\n\twindows function\t\"GetMessageA\" as getmessage\t\t\t\t(wt_ptr, wt_handle, wt_uint, wt_uint)wt_bool\r\n\twindows function\t\"TranslateMessage\"\t\t\t\t\t\t(wt_ptr)wt_bool\r\n\twindows function\t\"DispatchMessageA\" as dispatchmessage\t\t(wt_ptr)wt_result\r\n\twindows function\t\"SetTimer\"\t\t\t\t\t\t\t\t(wt_handle,wt_intptr,wt_uint,wt_ptr)wt_intptr\r\n\twindows function\t\"KillTimer\"\t\t\t\t\t\t\t\t(wt_handle,wt_intptr)wt_bool\r\n\twindows function\t\"SystemParametersInfoA\"\t\t\t\t\t(wt_uint,wt_uint,wt_ptr,wt_uint)wt_bool\r\n\twindows function\t\"GetSystemMetrics\"\t\t\t\t\t\t(wt_int)wt_int\r\n!\twindows function\t\"CreateMenu\"\t\t\t\t\t\t\t\t:int\r\n\twindows function\t\"AppendMenuA\" as appendmenu\t\t\t\t(wt_handle,wt_uint,wt_intptr,wt_string)wt_bool\r\n\twindows function\t\"GetDC\"\t\t\t\t\t\t\t\t\t(wt_handle)wt_handle\r\n\twindows function\t\"ReleaseDC\"\t\t\t\t\t\t\t\t(wt_handle,wt_handle)wt_int\r\n\r\n\twindows function\t\"SendMessageA\" as sendmessage\t\t\t\t(wt_handle,wt_uint,wt_wparam,wt_lparam)wt_result\r\n\twindows function\t\"PostMessageA\" as postmessage\t\t\t\t(wt_handle,wt_uint,wt_wparam,wt_lparam)wt_bool\r\n\twindows function\t\"PeekMessageA\" as peekmessage\t\t\t\t(wt_ptr,wt_handle,wt_uint,wt_uint,wt_uint)wt_bool\r\n\twindows function\t\"BeginPaint\"\t\t\t\t\t\t\t\t(wt_handle,wt_ptr)wt_handle\r\n\twindows function\t\"EndPaint\"\t\t\t\t\t\t\t\t(wt_handle,wt_ptr)wt_bool\r\n\twindows proc\t\"PostQuitMessage\"\t\t\t\t\t(wt_int)\r\n\twindows function\t\"LoadIconA\" as loadicon\t\t\t\t\t(wt_handle,wt_string)wt_handle\r\n\twindows function\t\"LoadCursorA\" as loadcursor\t\t\t\t(wt_handle,wt_string)wt_handle\r\n\twindows function\t\"SetCursor\"\t\t\t\t\t\t\t\t(wt_handle)wt_handle\r\n\twindows function\t\"DrawMenuBar\"\t\t\t\t\t\t\t\t(wt_handle)wt_bool\r\n\twindows function\t\"GetSystemMenu\"\t\t\t\t\t\t\t(wt_handle,wt_bool)wt_handle\r\n\twindows function\t\"CreateMenu\"\t\t\t\t\t\t\t\t:wt_handle\r\n\twindows function\t\"CreatePopupMenu\"\t\t\t\t\t\t\t:wt_handle\r\n\twindows function\t\"DestroyMenu\"\t\t\t\t\t\t\t\t(wt_handle)wt_bool\r\n\twindows function\t\"CheckMenuItem\"\t\t\t\t\t\t\t(wt_handle,wt_uint,wt_uint)wt_dword\r\n\twindows function\t\"EnableMenuItem\"\t\t\t\t\t\t\t(wt_handle,wt_uint,wt_uint)wt_bool\r\n\twindows function\t\"GetSubMenu\"\t\t\t\t\t\t\t\t(wt_handle,wt_int)wt_handle\r\n\twindows function\t\"GetMenuItemID\"\t\t\t\t\t\t\t(wt_handle,wt_int)wt_uint\r\n\twindows function\t\"GetMenuItemCount\"\t\t\t\t\t\t(wt_handle)wt_int\r\n\twindows function\t\"InsertMenuA\" as insertmenu\t\t\t\t(wt_handle,wt_uint,wt_uint,wt_intptr,wt_strin)wt_bool\r\n\twindows function\t\"ModifyMenuA\" as modifymenu\t\t\t\t(wt_handle,wt_uint,wt_uint,wt_intptr,wt_string)wt_bool\r\n\twindows function\t\"RemoveMenu\"\t\t\t\t\t\t\t\t(wt_handle,wt_uint,wt_uint)wt_bool\r\n\twindows function\t\"DeleteMenu\"\t\t\t\t\t\t\t\t(wt_handle,wt_uint,wt_uint)wt_bool\r\n\r\n\twindows function\t\"DestroyWindow\"\t\t\t\t\t\t\t(wt_handle)wt_bool\r\n\twindows function\t\"InvalidateRect\"\t\t\t\t\t\t\t(wt_handle,wt_ptr,wt_bool)wt_bool\r\n\twindows function\t\"ValidateRect\"\t\t\t\t\t\t\t(wt_handle,wt_ptr)wt_bool\r\n\twindows function\t\"ShowWindow\"\t\t\t\t\t\t\t\t(wt_handle,wt_int)wt_bool\r\n\twindows function\t\"GetClassLongA\" as getclassint\t\t\t(wt_handle,wt_int)wt_word\r\n\twindows function\t\"SetClassLongA\" as setclasslong\t\t\t(wt_handle,wt_int,wt_dword)wt_word\r\n\twindows function\t\"SetWindowTextA\" as setwindowtext\t\t\t(wt_handle,wt_string)wt_bool\r\n\twindows function\t\"GetWindowTextA\" as getwindowtext\t\t\t(wt_handle,wt_string,wt_int)wt_int\r\n\twindows function\t\"GetWindowTextLengthA\" as getwindowtextlength\t(wt_handle)wt_int\r\n\twindows function\t\"GetKeyState\"\t\t\t\t\t\t\t\t(wt_int)wt_word\r\n\r\n!\twindows function\t\"GetWindowLongPtrA\" as getwindowlongptr\t(wt_handle,wt_int)intm\r\n!\twindows function\t\"SetWindowLongPtrA\" as setwindowlongptr\t(wt_handle,wt_int,wt_int)intm\r\n\twindows function\t\"GetWindowLongA\" as getwindowlongptr\t\t(wt_handle,wt_int)intm\r\n\twindows function\t\"SetWindowLongA\" as setwindowlongptr\t\t(wt_handle,wt_int,intm)intm\r\n\r\n\twindows function\t\"GetClientRect\"\t\t\t\t\t\t\t(wt_handle,wt_ptr)wt_bool\r\n\twindows function\t\"ClientToScreen\"\t\t\t\t\t\t\t(wt_handle,wt_ptr)wt_bool\r\n\twindows function\t\"ScreenToClient\"\t\t\t\t\t\t\t(wt_handle,wt_ptr)wt_bool\r\n\twindows function\t\"GetWindowRect\"\t\t\t\t\t\t\t(wt_handle,wt_ptr)wt_bool\r\n\twindows function\t\"GetSysColor\" as getsyscolour\t\t\t\t(wt_int)wt_dword\r\n\twindows function\t\"GetScrollInfo\"\t\t\t\t\t\t\t(wt_handle,wt_int,wt_ptr)wt_bool\r\n\twindows function\t\"GetMenu\"\t\t\t\t\t\t\t\t\t(wt_handle)wt_handle\r\n\twindows function\t\"SetMenu\"\t\t\t\t\t\t\t\t\t(wt_handle,wt_handle)wt_ptr\r\n\twindows function\t\"TrackPopupMenu\"\t\t\t\t\t\t\t(wt_handle,wt_uint,wt_int,wt_int,wt_int,wt_handle,wt_ptr)wt_bool\r\n\twindows function\t\"GetMenuState\"\t\t\t\t\t\t\t(wt_handle,wt_uint,wt_uint)wt_uint\r\n\twindows function\t\"MessageBoxA\"\t\t\t\t\t\t\t\t(wt_handle,wt_string,wt_string,wt_uint)wt_int\r\n\twindows function\t\"OpenClipboard\"\t\t\t\t\t\t\t(wt_handle)wt_bool\r\n\twindows function\t\"CloseClipboard\"\t\t\t\t\t\t\t:wt_bool\r\n\twindows function\t\"EmptyClipboard\"\t\t\t\t\t\t\t:wt_bool\r\n\twindows function\t\"GetClipboardData\"\t\t\t\t\t\t(wt_uint)wt_handle\r\n\twindows function\t\"SetClipboardData\"\t\t\t\t\t\t(wt_uint,wt_handle)wt_handle\r\n\twindows function\t\"MessageBeep\"\t\t\t\t\t\t\t(wt_uint=0)wt_bool\r\nend\r\n!=========================================\r\n\r\nimportdll gdi32=\r\n\twindows function\t\"Rectangle\"\t\t\t\t\t\t\t\t(wt_handle,wt_int,wt_int,wt_int,wt_int)wt_bool\r\n\twindows function\t\"RoundRect\"\t\t\t\t\t\t\t\t(wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int)wt_bool\r\n\twindows function\t\"Ellipse\"\t\t\t\t\t\t\t\t\t(wt_handle,wt_int,wt_int,wt_int,wt_int)wt_bool\r\n\twindows function\t\"Arc\"\t\t\t\t\t\t\t\t\t\t(wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int)wt_bool\r\n\twindows function\t\"Chord\"\t\t\t\t\t\t\t\t\t(wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int)wt_bool\r\n\twindows function\t\"Pie\"\t\t\t\t\t\t\t\t\t\t(wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int)wt_bool\r\n\twindows function\t\"Polygon\"\t\t\t\t\t\t\t\t\t(wt_handle,wt_handle,wt_int)wt_bool\r\n\twindows function\t\"TextOutA\" as textout\t\t\t\t\t\t(wt_handle,wt_int,wt_int,wt_string,wt_int)wt_bool\r\n\twindows function\t\"TextOutW\" \t\t\t\t\t\t(wt_handle,wt_int,wt_int,wt_ptr,wt_int)wt_bool\r\n\twindows function\t\"GetStockObject\"\t\t\t\t\t\t\t(wt_int)wt_handle\r\n\twindows function\t\"SelectObject\"\t\t\t\t\t\t\t(wt_handle,wt_handle)wt_handle\r\n\twindows function\t\"CreateDCA\" as createdc\t\t\t\t\t(wt_string,wt_string,wt_string,wt_ptr)wt_handle\r\n\twindows function\t\"MoveToEx\"\t\t\t\t\t\t\t\t(wt_handle,wt_int,wt_int,wt_ptr=0)wt_bool\r\n\twindows function\t\"CreatePen\"\t\t\t\t\t\t\t\t(wt_int,wt_int,wt_dword)wt_handle\r\n\twindows function\t\"CreateSolidBrush\"\t\t\t\t\t\t(wt_dword)wt_handle\r\n\twindows function\t\"CreateBrushIndirect\"\t\t\t\t\t\t(wt_ptr)wt_handle\r\n\twindows function\t\"LineTo\"\t\t\t\t\t\t\t\t\t(wt_handle,wt_int,wt_int)wt_bool\r\n\twindows function\t\"GetPixel\"\t\t\t\t\t\t\t\t(wt_handle,wt_int,wt_int)wt_dword\r\n\twindows function\t\"SetPixel\"\t\t\t\t\t\t\t\t(wt_handle,wt_int,wt_int,wt_dword)wt_dword\r\n\twindows function\t\"SetGraphicsMode\"\t\t\t\t\t\t\t(wt_handle,wt_int)wt_int\r\n\twindows function\t\"CreateFontIndirectA\" as createfontindirect\t(wt_ptr)wt_handle\r\n\twindows function\t\"CreateFontA\" as createfont \\\r\n\t\t\t(wt_int height, wt_int width=0, wt_int escapement=0, wt_int orientation=0, wt_int bold=0,\r\n\t\t\t wt_dword italic=0, wt_dword underline=0, wt_dword strikeout=0, wt_dword charset=0,\r\n\t\t\t wt_dword outprec=0, wt_dword clipprec=0, wt_dword quality=0, wt_dword pitch=0, wt_string facename)wt_handle\r\n\twindows function\t\"SaveDC\"\t\t\t\t\t\t\t\t\t(wt_handle)wt_int\r\n\twindows function\t\"GetTextMetricsA\" as gettextmetrics\t\t(wt_handle,wt_ptr)wt_bool\r\n\twindows function\t\"DeleteObject\"\t\t\t\t\t\t\t(wt_handle)wt_bool\r\n\twindows function\t\"RestoreDC\"\t\t\t\t\t\t\t\t(wt_handle,wt_int)wt_bool\r\n!\twindows function\t\"GetTextExtentPoint32A\" as gettextextentpoint32\t(wt_handle,wt_string,wt_int,wt_ptr)wt_bool\r\n\twindows function\t\"GetTextExtentPoint32A\" as gettextextentpoint32\t(wt_handle,wt_ptr,wt_int,wt_ptr)wt_bool\r\n\twindows function\t\"GetObjectA\" as getobject\t\t\t\t\t(wt_handle,wt_int,wt_ptr)wt_int\r\n\twindows function\t\"CreatePalette\"\t\t\t\t\t\t\t(wt_ptr)wt_handle\r\n\twindows function\t\"GetWindowExtEx\"\t\t\t\t\t\t\t(wt_handle,wt_ptr)wt_bool\r\n\twindows function\t\"CreateCompatibleBitmap\"\t\t\t\t\t(wt_handle,wt_int,wt_int)wt_handle\r\n\twindows function\t\"SetBitmapBits\"\t\t\t\t\t\t\t(wt_handle,wt_dword,wt_ptr)wt_long\r\n\twindows function\t\"SelectPalette\"\t\t\t\t\t\t\t(wt_handle,wt_handle,wt_bool)wt_handle\r\n\twindows function\t\"RealizePalette\"\t\t\t\t\t\t\t(wt_handle)wt_uint\r\n\twindows function\t\"SetDIBitsToDevice\"\t\t\t\t\t\t(wt_handle,wt_int,wt_int,wt_dword,wt_dword,wt_int,wt_int,wt_uint,wt_uint,wt_ptr,wt_ptr,wt_uint)wt_int\r\n\twindows function\t\"StretchDIBits\"\t\t\t\t\t\t\t(wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_ptr,wt_ptr,wt_uint,wt_dword)wt_int\r\n\twindows function\t\"SetStretchBltMode\"\t\t\t\t\t\t(wt_handle,wt_int)wt_int\r\n\twindows function\t\"PatBlt\"\t\t\t\t\t\t\t\t\t(wt_handle,wt_int,wt_int,wt_int,wt_int,wt_dword)wt_bool\r\n\twindows function\t\"BitBlt\"\t\t\t\t\t\t\t\t\t(wt_handle,wt_int,wt_int,wt_int,wt_int,wt_handle,wt_int,wt_int,wt_dword)wt_bool\r\n\twindows function\t\"SetROP2\"\t\t\t\t\t\t\t\t\t(wt_handle,wt_int)wt_int\r\n\twindows function\t\"CreateCompatibleDC\"\t\t\t\t\t\t(wt_handle)wt_handle\r\n\twindows function\t\"DeleteDC\"\t\t\t\t\t\t\t\t(wt_handle)wt_bool\r\n\twindows function\t\"CreateBitmap\"\t\t\t\t\t\t\t(wt_int,wt_int,wt_uint,wt_uint,wt_ptr)wt_handle\r\n\twindows function\t\"CreateBitmapIndirect\"\t\t\t\t\t(wt_ptr)wt_handle\r\n\twindows function\t\"CreateDIBitmap\"\t\t\t\t\t\t\t(wt_handle,wt_ptr,wt_dword,wt_ptr,wt_ptr,wt_uint)wt_handle\r\n\twindows function\t\"CreateDIBSection\"\t\t\t\t\t\t(wt_handle,wt_ptr,wt_uint,wt_ptr,wt_handle,wt_dword)wt_handle\r\n\twindows function\t\"StretchBlt\"\t\t\t\t\t\t\t\t(wt_handle,wt_int,wt_int, wt_int,wt_int,wt_handle, wt_int,wt_int,wt_int, wt_int,wt_dword)wt_bool\r\n\twindows function\t\"PlgBlt\"\t\t\t\t\t\t\t\t(wt_handle,wt_ptr,wt_handle, wt_int,wt_int,wt_int,wt_int, wt_handle, wt_int,wt_int)wt_bool\r\n\twindows function\t\"SetTextColor\"  as settextcolour\t\t\t(wt_handle,wt_dword)wt_dword\r\n\twindows function\t\"SetTextAlign\"\t\t\t\t\t\t\t(wt_handle,wt_uint)wt_uint\r\n\twindows function\t\"SetTextJustification\"\t\t\t\t\t(wt_handle,wt_int,wt_int)wt_bool\r\n\twindows function\t\"SetBkColor\"  as setbkcolour\t\t\t\t(wt_handle,wt_dword)wt_dword\r\n\twindows function\t\"SetBkMode\"\t\t\t\t\t\t\t\t(wt_handle,wt_int)wt_int\r\n\twindows function\t\"GetBkColor\"  as getbkcolour\t\t\t\t(wt_handle)wt_dword\r\n\twindows function\t\"GetBkMode\"\t\t\t\t\t\t\t\t(wt_handle)wt_int\r\n\twindows function\t\"StartDocA\" as startdoc\t\t\t\t\t(wt_handle,wt_ptr)wt_int\r\n\twindows function\t\"StartPage\"\t\t\t\t\t\t\t\t(wt_handle)wt_int\r\n\twindows function\t\"EndPage\"\t\t\t\t\t\t\t\t\t(wt_handle)wt_int\r\n\twindows function\t\"EndDoc\"\t\t\t\t\t\t\t\t\t(wt_handle)wt_int\r\n\twindows function\t\"AbortDoc\"\t\t\t\t\t\t\t\t(wt_handle)wt_int\r\n\twindows function\t\"GetViewportOrgEx\"\t\t\t\t\t\t(wt_handle,wt_ptr)wt_bool\r\n\twindows function\t\"GetDIBits\"\t\t\t\t\t\t\t\t(wt_handle,wt_handle,wt_uint,wt_uint,wt_ptr,wt_ptr,wt_uint)wt_int\r\n\twindows function\t\"GetDIBColorTable\" as getdibcolourtable\t(wt_handle,wt_uint,wt_uint,wt_ptr)wt_uint\r\n\twindows function\t\"SetDIBColorTable\" as setdibcolourtable\t(wt_handle,wt_uint,wt_uint,wt_ptr)wt_uint\r\n\twindows function\t\"GetTextAlign\"\t\t\t\t\t\t\t(wt_handle)wt_uint\r\nend\r\n!=========================================\r\n\r\nimportdll comdlg32=\r\n\twindows function\t\"GetOpenFileNameA\"\t\t\t\t\t\t(wt_ptr)wt_bool\r\n\twindows function\t\"GetSaveFileNameA\"\t\t\t\t\t\t(wt_ptr)wt_bool\r\nend\r\n\r\n!endexport\r\n\r\nproc start=\r\n!CPL \"START WINAPI\"\r\nend\r\n\r\nproc main=\r\nCPL \"MAIN\"\r\na:=ws_rect(0,0,0,0)\r\nend\r\n";
static char *	l_winconsts="!Windows win32 constants\r\n\r\n!export\r\n\r\nglobal const driverversion =  0\r\nglobal const technology =  2\r\nglobal const horzsize =  4\r\nglobal const vertsize =  6\r\nglobal const horzres =  8\r\nglobal const vertres =  10\r\nglobal const bitspixel =  12\r\nglobal const bitplanes =  14\r\nglobal const numbrushes =  16\r\nglobal const numpens =  18\r\nglobal const nummarkers =  20\r\nglobal const numfonts =  22\r\nglobal const numcolours =  24\r\nglobal const pdevicesize =  26\r\nglobal const curvecaps =  28\r\nglobal const linecaps =  30\r\nglobal const polygonalcaps =  32\r\nglobal const textcaps =  34\r\nglobal const clipcaps =  36\r\nglobal const rastercaps =  38\r\nglobal const aspectx =  40\r\nglobal const aspecty =  42\r\nglobal const aspectxy =  44\r\nglobal const logpixelsx =  88\r\nglobal const logpixelsy =  90\r\nglobal const sizepalette =  104\r\nglobal const numreserved =  106\r\nglobal const colourres =  108\r\nglobal const physicalwidth =  110\r\nglobal const physicalheight =  111\r\nglobal const physicaloffsetx =  112\r\nglobal const physicaloffsety =  113\r\nglobal const scalingfactorx =  114\r\nglobal const scalingfactory =  115\r\nglobal const fw_dontcare =  0\r\nglobal const fw_thin =  100\r\nglobal const fw_extralight =  200\r\nglobal const fw_ultralight =  200\r\nglobal const fw_light =  300\r\nglobal const fw_normal =  400\r\nglobal const fw_regular =  400\r\nglobal const fw_medium =  500\r\nglobal const fw_semibold =  600\r\nglobal const fw_demibold =  600\r\nglobal const fw_bold =  700\r\nglobal const fw_extrabold =  800\r\nglobal const fw_ultrabold =  800\r\nglobal const fw_heavy =  900\r\nglobal const fw_black =  900\r\nglobal const cs_vredraw =  1\r\nglobal const cs_hredraw =  2\r\nglobal const cs_keycvtwindow =  4\r\nglobal const cs_dblclks =  8\r\nglobal const cs_owndc =  32\r\nglobal const cs_classdc =  64\r\nglobal const cs_parentdc =  128\r\nglobal const cs_nokeycvt =  256\r\nglobal const cs_noclose =  512\r\nglobal const cs_savebits =  2048\r\nglobal const cs_bytealignclient =  4096\r\nglobal const cs_bytealignwindow =  8192\r\nglobal const cs_publicclass =  16384\r\nglobal const sw_hide =  0\r\nglobal const sw_shownormal =  1\r\nglobal const sw_normal =  1\r\nglobal const sw_showminimized =  2\r\nglobal const sw_showmaximized =  3\r\nglobal const sw_maximize =  3\r\nglobal const sw_shownoactivate =  4\r\nglobal const sw_show =  5\r\nglobal const sw_minimize =  6\r\nglobal const sw_showminnoactive =  7\r\nglobal const sw_showna =  8\r\nglobal const sw_restore =  9\r\nglobal const sw_showdefault =  10\r\nglobal const sw_max =  10\r\nglobal const pm_noremove =  0\r\nglobal const pm_remove =  1\r\nglobal const pm_noyield =  2\r\nglobal const wm_null =  0\r\nglobal const wm_create =  1\r\nglobal const wm_destroy =  2\r\nglobal const wm_move =  3\r\nglobal const wm_size =  5\r\nglobal const wm_activate =  6\r\nglobal const wa_inactive =  0\r\nglobal const wa_active =  1\r\nglobal const wa_clickactive =  2\r\nglobal const wm_setfocus =  7\r\nglobal const wm_killfocus =  8\r\nglobal const wm_enable =  10\r\nglobal const wm_setredraw =  11\r\nglobal const wm_settext =  12\r\nglobal const wm_gettext =  13\r\nglobal const wm_gettextlength =  14\r\nglobal const wm_paint =  15\r\nglobal const wm_close =  16\r\nglobal const wm_queryendsession =  17\r\nglobal const wm_quit =  18\r\nglobal const wm_queryopen =  19\r\nglobal const wm_erasebkgnd =  20\r\nglobal const wm_syscolourchange =  21\r\nglobal const wm_endsession =  22\r\nglobal const wm_showwindow =  24\r\nglobal const wm_wininichange =  26\r\nglobal const wm_devmodechange =  27\r\nglobal const wm_activateapp =  28\r\nglobal const wm_fontchange =  29\r\nglobal const wm_timechange =  30\r\nglobal const wm_cancelmode =  31\r\nglobal const wm_setcursor =  32\r\nglobal const wm_mouseactivate =  33\r\nglobal const wm_childactivate =  34\r\nglobal const wm_queuesync =  35\r\nglobal const wm_getminmaxinfo =  36\r\nglobal const wm_drawitem =  43\r\nglobal const wm_notify =  78\r\nglobal const wm_contextmenu =  123\r\nglobal const wm_geticon =  127\r\nglobal const wm_seticon =  128\r\nglobal const wm_nchittest =  132\r\n\r\nglobal const wm_nclbuttondown\t= 161\r\nglobal const wm_nclbuttonup\t= 162\r\nglobal const wm_nclbuttondblclick\t= 163\r\n\r\nglobal const wm_menurbuttonup\t= 290\r\n\r\nglobal const wm_parentnotify =  528\r\nglobal const wm_dropfiles =  563\r\nglobal const wm_enteridle =  289\r\nglobal const wm_user =  1024\r\nglobal const wm_mdicreate =  544\r\nglobal const wm_mdidestroy =  545\r\nglobal const wm_mdiactivate =  546\r\nglobal const wm_mdirestore =  547\r\nglobal const wm_mdinext =  548\r\nglobal const wm_mdimaximize =  549\r\nglobal const wm_mditile =  550\r\nglobal const wm_mdicascade =  551\r\nglobal const wm_mdiiconarange =  552\r\nglobal const wm_mdigetactive =  553\r\nglobal const wm_mdisetmenu =  560\r\nglobal const wm_entersizemove =  561\r\nglobal const wm_exitsizemove =  562\r\nglobal const wm_mdirefrshmenu =  564\r\nglobal const wm_lbuttondblclk =  515\r\nglobal const wm_rbuttondblclk =  518\r\nglobal const wm_lbuttondown =  513\r\nglobal const wm_rbuttondown =  516\r\nglobal const wm_mbuttondown =  519\r\nglobal const wm_mousemove =  512\r\nglobal const wm_lbuttonup =  514\r\nglobal const wm_rbuttonup =  517\r\nglobal const wm_mbuttonup =  520\r\nglobal const wm_mbuttondblclk =  521\r\nglobal const wm_mousewheel =  522\r\nglobal const snd_filename =  131072\r\nglobal const snd_async =  1\r\nglobal const dt_singleline =  32\r\nglobal const dt_centre =  1\r\nglobal const dt_vcentre =  4\r\nglobal const ws_overlapped =  0\r\nglobal const ws_popup =  2147483648\r\nglobal const ws_child =  1073741824\r\nglobal const ws_minimize =  536870912\r\nglobal const ws_visible =  268435456\r\nglobal const ws_disabled =  134217728\r\nglobal const ws_clipsiblings =  67108864\r\nglobal const ws_clipchildren =  33554432\r\nglobal const ws_maximize =  16777216\r\nglobal const ws_caption =  12582912\r\nglobal const ws_border =  8388608\r\nglobal const ws_dlgframe =  4194304\r\nglobal const ws_hscroll =  1048576\r\nglobal const ws_vscroll =  2097152\r\nglobal const ws_sysmenu =  524288\r\nglobal const ws_thickframe =  262144\r\nglobal const ws_group =  131072\r\nglobal const ws_tabstop =  0\r\nglobal const ws_scrollbars =  3145728\r\nglobal const ws_minimizebox =  131072\r\nglobal const ws_maximizebox =  65536\r\nglobal const ws_tiled =  0\r\nglobal const ws_iconic =  536870912\r\nglobal const ws_sizebox =  262144\r\nglobal const ws_overlappedwindow =  13565952\r\nglobal const ws_tiledwindow =  13565952\r\nglobal const ws_popupwindow =  -2138570752\r\nglobal const ws_childwindow =  1073741824\r\nglobal const ws_ex_acceptfiles =  16\r\nglobal const ws_ex_appwindow =  262144\r\nglobal const ws_ex_clientedge =  512\r\nglobal const ws_ex_contexthelp =  1024\r\nglobal const ws_ex_controlparent =  65536\r\nglobal const ws_ex_dlgmodalframe =  1\r\nglobal const ws_ex_left =  0\r\nglobal const ws_ex_leftscrollbar =  16384\r\nglobal const ws_ex_ltrreading =  0\r\nglobal const ws_ex_mdichild =  64\r\nglobal const ws_ex_noparentnotify =  4\r\nglobal const ws_ex_overlappedwindow =  768\r\nglobal const ws_ex_palettewindow =  392\r\nglobal const ws_ex_right =  4096\r\nglobal const ws_ex_rightscrollbar =  0\r\nglobal const ws_ex_rtlreading =  8192\r\nglobal const ws_ex_staticedge =  131072\r\nglobal const ws_ex_toolwindow =  128\r\nglobal const ws_ex_topmost =  8\r\nglobal const ws_ex_transparent =  32\r\nglobal const ws_ex_windowedge =  256\r\n\r\nglobal const gw_hwndfirst =  0\r\nglobal const gw_hwndlast =  1\r\nglobal const gw_hwndnext =  2\r\nglobal const gw_hwndprev =  3\r\nglobal const gw_owner =  4\r\nglobal const gw_child =  5\r\nglobal const gw_enabledpopup =  6\r\nglobal const cb_geteditsel =  320\r\nglobal const cb_limittext =  321\r\nglobal const cb_seteditsel =  322\r\nglobal const cb_addstring =  323\r\nglobal const cb_deletestring =  324\r\nglobal const cb_dir =  325\r\nglobal const cb_getcount =  326\r\nglobal const cb_getcursel =  327\r\nglobal const cb_getlbtext =  328\r\nglobal const cb_getlbtextlen =  329\r\nglobal const cb_insertstring =  330\r\nglobal const cb_resetcontent =  331\r\nglobal const cb_findstring =  332\r\nglobal const cb_findstringexact =  344\r\nglobal const cb_selectstring =  333\r\nglobal const cb_setcursel =  334\r\nglobal const cb_showdropdown =  335\r\nglobal const cb_getitemdata =  336\r\nglobal const cb_setitemdata =  337\r\nglobal const cb_getdroppedcontrolrect =  338\r\nglobal const cb_setitemheight =  339\r\nglobal const cb_getitemheight =  340\r\nglobal const cb_setextendedui =  341\r\nglobal const cb_getextendedui =  342\r\nglobal const cb_getdroppedstate =  343\r\nglobal const cb_setlocale =  345\r\nglobal const cb_getlocale =  346\r\nglobal const cb_gettopindex =  347\r\nglobal const cb_settopindex =  348\r\nglobal const cb_gethorizontalextent =  349\r\nglobal const cb_sethorizontalextent =  350\r\nglobal const cb_getdroppedwidth =  351\r\nglobal const cb_setdroppedwidth =  352\r\nglobal const cb_initstorage =  353\r\nglobal const cb_multipleaddstring =  355\r\nglobal const bm_click =  245\r\nglobal const bm_getcheck =  240\r\nglobal const bm_getimage =  246\r\nglobal const bm_getstate =  242\r\nglobal const bm_setcheck =  241\r\nglobal const bm_setimage =  247\r\nglobal const bm_setstate =  243\r\nglobal const bm_setstyle =  244\r\nglobal const cf_bitmap =  2\r\nglobal const cf_dib =  8\r\nglobal const cf_palette =  9\r\nglobal const cf_enhmetafile =  14\r\nglobal const cf_metafilepict =  3\r\nglobal const cf_oemtext =  7\r\nglobal const cf_text =  1\t\t\t!used in sys\r\nglobal const cf_unicodetext =  13\r\nglobal const cf_dif =  5\r\nglobal const cf_dspbitmap =  130\r\nglobal const cf_dspenhmetafile =  142\r\nglobal const cf_dspmetafilepict =  131\r\nglobal const cf_dsptext =  129\r\nglobal const cf_gdiobjfirst =  768\r\nglobal const cf_gdiobjlast =  1023\r\nglobal const cf_hdrop =  15\r\nglobal const cf_locale =  16\r\nglobal const cf_ownerdisplay =  128\r\nglobal const cf_pendata =  10\r\nglobal const cf_privatefirst =  512\r\nglobal const cf_privatelast =  767\r\nglobal const cf_riff =  11\r\nglobal const cf_sylk =  4\r\nglobal const cf_wave =  12\r\nglobal const cf_tiff =  6\r\n\r\nglobal const tcif_text =  1\r\nglobal const tcif_image =  2\r\nglobal const tcif_param =  8\r\nglobal const tcif_rtlreading =  4\r\n\r\nglobal const wm_keydown =  256\r\nglobal const wm_keyup =  257\r\nglobal const wm_char =  258\r\nglobal const wm_syschar =  262\r\nglobal const wm_sysdeadchar =  263\r\nglobal const wm_syskeydown =  260\r\nglobal const wm_syskeyup =  261\r\nglobal const mf_insert =  0\r\nglobal const mf_change =  128\r\nglobal const mf_append =  256\r\nglobal const mf_delete =  512\r\nglobal const mf_remove =  4096\r\nglobal const mf_bycommand =  0\r\nglobal const mf_byposition =  1024\r\nglobal const mf_separator =  2048\r\nglobal const mf_enabled =  0\r\nglobal const mf_grayed =  1\r\nglobal const mf_greyed =  1\r\nglobal const mf_disabled =  2\r\nglobal const mf_unchecked =  0\r\nglobal const mf_checked =  8\r\nglobal const mf_usecheckbitmaps =  512\r\nglobal const mf_string =  0\r\nglobal const mf_bitmap =  4\r\nglobal const mf_ownerdraw =  256\r\nglobal const mf_popup =  16\r\nglobal const mf_menubarbreak =  32\r\nglobal const mf_menubreak =  64\r\nglobal const mf_unhilite =  0\r\nglobal const mf_hilite =  128\r\nglobal const mf_sysmenu =  8192\r\nglobal const mf_help =  16384\r\nglobal const mf_mouseselect =  32768\r\n\r\n!global const bn_clicked =  0\r\n!global const bn_dblclk =  5\r\n!global const bn_disable =  4\r\n!global const bn_doubleclicked =  5\r\n!global const bn_hilite =  2\r\n!global const bn_killfocus =  7\r\n!global const bn_paint =  1\r\n!global const bn_pushed =  2\r\n!global const bn_setfocus =  6\r\n!global const bn_unhilite =  3\r\n!global const bn_unpushed =  3\r\n!global const en_setfocus =  256\r\n!global const en_killfocus =  512\r\n!global const en_change =  768\r\n!global const en_update =  1024\r\n!global const en_errspace =  1280\r\n!global const en_maxtext =  1281\r\n!global const en_hscroll =  1537\r\n!global const en_vscroll =  1538\r\n!global const lbn_errspace =  -2\r\n!global const lbn_selchange =  1\r\n!global const lbn_dblclk =  2\r\n!global const lbn_selcancel =  3\r\n!global const lbn_setfocus =  4\r\n!global const lbn_killfocus =  5\r\n!global const cbn_errspace =  -1\r\n!global const cbn_selchange =  1\r\n!global const cbn_dblclk =  2\r\n!global const cbn_setfocus =  3\r\n!global const cbn_killfocus =  4\r\n!global const cbn_editchange =  5\r\n!global const cbn_editupdate =  6\r\n!global const cbn_dropdown =  7\r\n!global const cbn_closeup =  8\r\n!global const cbn_selendok =  9\r\n!global const cbn_selendcancel =  10\r\n!\r\n!global const cbs_autohscroll =  64\r\n!global const cbs_disablenoscroll =  2048\r\n!global const cbs_dropdown =  2\r\n!global const cbs_dropdownlist =  3\r\n!global const cbs_hasstrings =  512\r\n!global const cbs_lowercase =  16384\r\n!global const cbs_nointegralheight =  1024\r\n!global const cbs_oemconvert =  128\r\n!global const cbs_ownerdrawfixed =  16\r\n!global const cbs_ownerdrawvariable =  32\r\n!global const cbs_simple =  1\r\n!global const cbs_sort =  256\r\n!global const cbs_uppercase =  8192\r\n\r\nglobal const wm_command =  273\r\nglobal const wm_menuselect =  287\r\nglobal const wm_cut =  768\r\nglobal const wm_copy =  769\r\nglobal const wm_paste =  770\r\nglobal const wm_clear =  771\r\nglobal const wm_undo =  772\r\nglobal const em_getsel =  176\r\nglobal const em_setsel =  177\r\nglobal const em_scroll =  181\r\nglobal const em_linescroll =  182\r\nglobal const em_scrollcaret =  183\r\nglobal const em_getmodify =  184\r\nglobal const em_setmodify =  185\r\nglobal const em_getlinecount =  186\r\nglobal const em_lineindex =  187\r\nglobal const em_sethandle =  188\r\nglobal const em_gethandle =  189\r\nglobal const em_getthumb =  190\r\nglobal const em_linelength =  193\r\nglobal const em_replacesel =  194\r\nglobal const em_getline =  196\r\nglobal const em_limittext =  197\r\nglobal const em_canundo =  198\r\nglobal const em_undo =  199\r\nglobal const em_fmtlines =  200\r\nglobal const em_linefromchar =  201\r\nglobal const em_settabstops =  203\r\nglobal const em_setpasswordchar =  204\r\nglobal const em_emptyundobuffer =  205\r\nglobal const em_getfirstvisibleline =  206\r\nglobal const em_setreadonly =  207\r\nglobal const em_setwordbreakproc =  208\r\nglobal const em_getwordbreakproc =  209\r\nglobal const em_getpasswordchar =  210\r\nglobal const em_setlimittext =  197\r\nglobal const em_getseltext =  1086\r\nglobal const em_setcharformat =  1092\r\nglobal const em_getcharformat =  1082\r\nglobal const em_settextmode =  1113\r\nglobal const em_gettextmode =  1114\r\nglobal const em_gettextex =  1118\r\nglobal const em_gettextlengthex =  1119\r\nglobal const tm_plaintext =  1\r\nglobal const tm_richtext =  2\r\nglobal const tm_singlelevelundo =  4\r\nglobal const tm_multilevelundo =  8\r\nglobal const tm_singlecodepage =  16\r\nglobal const tm_multicodepage =  32\r\nglobal const scf_word =  2\r\nglobal const scf_selection =  1\r\nglobal const sb_getborders =  1031\r\nglobal const sb_getparts =  1030\r\nglobal const sb_getrect =  1034\r\nglobal const sb_gettextw =  1037\r\nglobal const sb_gettextlengthw =  1036\r\nglobal const sb_settextw =  1035\r\nglobal const sb_gettexta =  1026\r\nglobal const sb_gettextlengtha =  1027\r\nglobal const sb_settexta =  1025\r\nglobal const sb_gettext =  1026\r\nglobal const sb_gettextlength =  1027\r\nglobal const sb_settext =  1025\r\nglobal const sb_setminheight =  1032\r\nglobal const sb_setparts =  1028\r\nglobal const sb_simple =  1033\r\nglobal const wm_setfont =  48\r\nglobal const wm_getfont =  49\r\nglobal const gm_advanced =  2\r\nglobal const transparent =  1\r\nglobal const opaque =  2\r\nglobal const mwt_identity =  1\r\nglobal const cw_usedefault =  0x8000'0000\r\nglobal const idc_arrow =  32512\r\nglobal const idc_ibeam =  32513\r\nglobal const idc_wait =  32514\r\nglobal const idc_cross =  32515\r\nglobal const idc_uparrow =  32516\r\nglobal const idc_sizenwse =  32642\r\nglobal const idc_sizenesw =  32643\r\nglobal const idc_sizewe =  32644\r\nglobal const idc_sizens =  32645\r\nglobal const idc_sizeall =  32646\r\nglobal const idc_no =  32648\r\nglobal const idc_appstarting =  32650\r\nglobal const idc_help =  32651\r\nglobal const idi_application =  32512\r\nglobal const idi_hand =  32513\r\nglobal const idi_question =  32514\r\nglobal const idi_exclamation =  32515\r\nglobal const idi_asterisk =  32516\r\nglobal const idi_winlogo =  32517\r\nglobal const idc_size =  32640\r\nglobal const idc_icon =  32641\r\nglobal const arrowpointer =  32512\r\nglobal const ibeampointer =  32513\r\nglobal const waitpointer =  32514\r\nglobal const crosspointer =  32515\r\nglobal const uparrowpointer =  32516\r\nglobal const sizenwsepointer =  32642\r\nglobal const sizeneswpointer =  32643\r\nglobal const sizewepointer =  32644\r\nglobal const sizenspointer =  32645\r\nglobal const sizeallpointer =  32646\r\nglobal const nopointer =  32648\r\nglobal const appstartingpointer =  32650\r\nglobal const helpicon =  32651\r\nglobal const applicationicon =  32512\r\nglobal const handicon =  32513\r\nglobal const questionicon =  32514\r\nglobal const exclamationicon =  32515\r\nglobal const asteriskicon =  32516\r\nglobal const winlogoicon =  32517\r\nglobal const sizepointer =  32640\r\nglobal const iconicon =  32641\r\nglobal const sm_cymin =  29\r\nglobal const sm_cxmin =  28\r\nglobal const sm_arrange =  56\r\nglobal const sm_cleanboot =  67\r\nglobal const sm_cmetrics =  76\r\nglobal const sm_cmousebuttons =  43\r\nglobal const sm_cxborder =  5\r\nglobal const sm_cyborder =  6\r\nglobal const sm_cxcursor =  13\r\nglobal const sm_cycursor =  14\r\nglobal const sm_cxdlgframe =  7\r\nglobal const sm_cydlgframe =  8\r\nglobal const sm_cxdoubleclk =  36\r\nglobal const sm_cydoubleclk =  37\r\nglobal const sm_cxdrag =  68\r\nglobal const sm_cydrag =  69\r\nglobal const sm_cxedge =  45\r\nglobal const sm_cyedge =  46\r\nglobal const sm_cxfixedframe =  7\r\nglobal const sm_cyfixedframe =  8\r\nglobal const sm_cxframe =  32\r\nglobal const sm_cyframe =  33\r\nglobal const sm_cxfullscreen =  16\r\nglobal const sm_cyfullscreen =  17\r\nglobal const sm_cxhscroll =  21\r\nglobal const sm_cyhscroll =  3\r\nglobal const sm_cxhthumb =  10\r\nglobal const sm_cxicon =  11\r\nglobal const sm_cyicon =  12\r\nglobal const sm_cxiconspacing =  38\r\nglobal const sm_cyiconspacing =  39\r\nglobal const sm_cxmaximized =  61\r\nglobal const sm_cymaximized =  62\r\nglobal const sm_cxmaxtrack =  59\r\nglobal const sm_cymaxtrack =  60\r\nglobal const sm_cxmenucheck =  71\r\nglobal const sm_cymenucheck =  72\r\nglobal const sm_cxmenusize =  54\r\nglobal const sm_cymenusize =  55\r\nglobal const sm_cxminimized =  57\r\nglobal const sm_cyminimized =  58\r\nglobal const sm_cxminspacing =  47\r\nglobal const sm_cyminspacing =  48\r\nglobal const sm_cxmintrack =  34\r\nglobal const sm_cymintrack =  35\r\nglobal const sm_cxscreen =  0\r\nglobal const sm_cyscreen =  1\r\nglobal const sm_cxsize =  30\r\nglobal const sm_cysize =  31\r\nglobal const sm_cxsizeframe =  32\r\nglobal const sm_cysizeframe =  33\r\nglobal const sm_cxsmicon =  49\r\nglobal const sm_cysmicon =  50\r\nglobal const sm_cxsmsize =  52\r\nglobal const sm_cysmsize =  53\r\nglobal const sm_cxvscroll =  2\r\nglobal const sm_cyvscroll =  20\r\nglobal const sm_cyvthumb =  9\r\nglobal const sm_cycaption =  4\r\nglobal const sm_cykanjiwindow =  18\r\nglobal const sm_cymenu =  15\r\nglobal const sm_cysmcaption =  51\r\nglobal const sm_dbcsenabled =  42\r\nglobal const sm_debug =  22\r\nglobal const sm_menudropalignment =  40\r\nglobal const sm_mideastenabled =  74\r\nglobal const sm_mousepresent =  19\r\nglobal const sm_mousewheelpresent =  75\r\nglobal const sm_network =  63\r\nglobal const sm_penwindows =  41\r\nglobal const sm_reserved1 =  24\r\nglobal const sm_reserved2 =  25\r\nglobal const sm_reserved3 =  26\r\nglobal const sm_reserved4 =  27\r\nglobal const sm_secure =  44\r\nglobal const sm_showsounds =  70\r\nglobal const sm_slowmachine =  73\r\nglobal const sm_swapbutton =  23\r\nglobal const arw_bottomleft =  0\r\nglobal const arw_bottomright =  1\r\nglobal const arw_hide =  8\r\nglobal const arw_topleft =  2\r\nglobal const arw_topright =  3\r\nglobal const arw_down =  4\r\nglobal const arw_left =  0\r\nglobal const arw_right =  0\r\nglobal const arw_up =  4\r\nglobal const white_brush =  0\r\nglobal const ltgray_brush =  1\r\nglobal const gray_brush =  2\r\nglobal const dkgray_brush =  3\r\nglobal const black_brush =  4\r\nglobal const null_brush =  5\r\nglobal const hollow_brush =  5\r\nglobal const white_pen =  6\r\nglobal const black_pen =  7\r\nglobal const null_pen =  8\r\nglobal const oem_fixed_font =  10\r\nglobal const ansi_fixed_font =  11\r\nglobal const ansi_var_font =  12\r\nglobal const system_font =  13\r\nglobal const device_default_font =  14\r\nglobal const default_palette =  15\r\nglobal const system_fixed_font =  16\r\nglobal const stock_last =  16\r\n\r\n!global const sbm_setpos =  224\r\n!global const sbm_getpos =  225\r\n!global const sbm_setrange =  226\r\n!global const sbm_setrangeredraw =  230\r\n!global const sbm_getrange =  227\r\n!global const sbm_enable_arrows =  228\r\n!global const sbs_horz =  0\r\n!global const sbs_vert =  1\r\n!global const sbs_topalign =  2\r\n!global const sbs_leftalign =  2\r\n!global const sbs_bottomalign =  4\r\n!global const sbs_rightalign =  4\r\n!global const sbs_sizeboxtopleftalign =  2\r\n!global const sbs_sizeboxbottomrightalign =  4\r\n!global const sbs_sizebox =  8\r\n\r\nglobal const wm_hscroll =  276\r\nglobal const wm_vscroll =  277\r\n\r\n!global const sb_horz =  0\r\n!global const sb_hoz =  0\r\n!global const sb_vert =  1\r\n!global const sb_ctl =  2\r\n!global const sb_both =  3\r\n!global const sb_lineup =  0\r\n!global const sb_lineleft =  0\r\n!global const sb_linedown =  1\r\n!global const sb_lineright =  1\r\n!global const sb_pageup =  2\r\n!global const sb_pageleft =  2\r\n!global const sb_pagedown =  3\r\n!global const sb_pageright =  3\r\n!global const sb_thumbposition =  4\r\n!global const sb_thumbtrack =  5\r\n!global const sb_top =  6\r\n!global const sb_left =  6\r\n!global const sb_bottom =  7\r\n!global const sb_right =  7\r\n!global const sb_endscroll =  8\r\n!global const sif_disablenoscroll =  8\r\n!global const sif_page =  2\r\n!global const sif_pos =  4\r\n!global const sif_range =  1\r\n!global const sif_trackpos =  16\r\n!global const sif_all =  23\r\n\r\nglobal const wm_ctlcolourmsgbox =  306\r\nglobal const wm_ctlcolouredit =  307\r\nglobal const wm_ctlcolourlistbox =  308\r\nglobal const wm_ctlcolourbtn =  309\r\nglobal const wm_ctlcolourdlg =  310\r\nglobal const wm_ctlcolourscrollbar =  311\r\nglobal const wm_ctlcolourstatic =  312\r\nglobal const wm_timer =  275\r\n\r\nglobal const srccopy =  13369376\r\nglobal const srcpaint =  15597702\r\nglobal const srcand =  8913094\r\nglobal const srcinvert =  6684742\r\nglobal const srcerase =  4457256\r\n\r\nglobal const notsrccopy =  3342344\r\nglobal const notsrcerase =  1114278\r\nglobal const mergecopy =  12583114\r\nglobal const mergepaint =  12255782\r\nglobal const patcopy =  15728673\r\nglobal const patpaint =  16452105\r\nglobal const patinvert =  5898313\r\nglobal const dstinvert =  5570569\r\nglobal const blackness =  66\r\nglobal const whiteness =  16711778\r\n\r\nglobal const r2_black =  1\r\nglobal const r2_notmergepen =  2\r\nglobal const r2_masknotpen =  3\r\nglobal const r2_notcopypen =  4\r\nglobal const r2_maskpennot =  5\r\nglobal const r2_not =  6\r\nglobal const r2_xorpen =  7\r\nglobal const r2_notmaskpen =  8\r\nglobal const r2_maskpen =  9\r\nglobal const r2_notxorpen =  10\r\nglobal const r2_nop =  11\r\nglobal const r2_mergenotpen =  12\r\nglobal const r2_copypen =  13\r\nglobal const r2_mergepennot =  14\r\nglobal const r2_mergepen =  15\r\nglobal const r2_white =  16\r\nglobal const r2_last =  16\r\n\r\nglobal const gdi_error =  4294967295\r\nglobal const hgdi_error =  4294967295\r\nglobal const clr_invalid =  4278190080\r\nglobal const clr_default =  4278190080\r\nglobal const clr_none =  4294967295\r\nglobal const ofn_readonly =  1\r\nglobal const ofn_overwriteprompt =  2\r\nglobal const ofn_hidereadonly =  4\r\nglobal const ofn_nochangedir =  8\r\nglobal const ofn_showhelp =  16\r\nglobal const ofn_enablehook =  32\r\nglobal const ofn_enabletemplate =  64\r\nglobal const ofn_enabletemplatehandle =  128\r\nglobal const ofn_novalidate =  256\r\nglobal const ofn_allowmultiselect =  512\r\nglobal const ofn_extensiondifferent =  1024\r\nglobal const ofn_pathmustexist =  2048\r\nglobal const ofn_filemustexist =  4096\r\nglobal const ofn_createprompt =  8192\r\nglobal const ofn_shareaware =  16384\r\nglobal const ofn_noreadonlyreturn =  32768\r\nglobal const ofn_notestfilecreate =  65536\r\nglobal const ofn_nonetworkbutton =  131072\r\nglobal const ofn_nolongnames =  262144\r\nglobal const ofn_explorer =  524288\r\nglobal const ofn_nodereferencelinks =  1048576\r\nglobal const ofn_longnames =  2097152\r\nglobal const ofn_sharefallthrough =  2\r\nglobal const ofn_sharenowarn =  1\r\nglobal const ofn_sharewarn =  0\r\n!global const gmem_fixed =  0\r\n!global const gmem_moveable =  2\r\n!global const gmem_nocompact =  16\r\n!global const gmem_nodiscard =  32\r\n!global const gmem_zeroinit =  64\r\n!global const gmem_modify =  128\r\n!global const gmem_discardable =  256\r\n!global const gmem_not_banked =  4096\r\n!global const gmem_share =  8192\r\n!global const gmem_ddeshare =  8192\r\n!global const gmem_notify =  16384\r\n!global const gmem_lower =  4096\r\n!global const gmem_valid_flags =  32626\r\n!global const gmem_invalid_handle =  32768\r\n!global const gmem_clipboard =  8194\r\n!global const ghnd =  66\r\n!global const gptr =  64\r\n!global const pd_allpages =  0\r\n!global const pd_collate =  16\r\n!global const pd_disableprinttofile =  524288\r\n!global const pd_enableprinthook =  4096\r\n!global const pd_enableprinttemplate =  16384\r\n!global const pd_enableprinttemplatehandle =  65536\r\n!global const pd_enablesetuphook =  8192\r\n!global const pd_enablesetuptemplate =  32768\r\n!global const pd_enablesetuptemplatehandle =  131072\r\n!global const pd_hideprinttofile =  1048576\r\n!global const pd_nopagenums =  8\r\n!global const pd_noselection =  4\r\n!global const pd_nowarning =  128\r\n!global const pd_pagenums =  2\r\n!global const pd_printsetup =  64\r\n!global const pd_printtofile =  32\r\n!global const pd_returndc =  256\r\n!global const pd_returndefault =  1024\r\n!global const pd_returnic =  512\r\n!global const pd_selection =  1\r\n!global const pd_showhelp =  2048\r\n!global const pd_usedevmodecopies =  262144\r\n!global const pd_usedevmodecopiesandcollate =  262144\r\nglobal const dib_rgb_colours =  0\r\nglobal const dib_pal_colours =  1\r\nglobal const dib_pal_indices =  2\r\nglobal const dib_pal_physindices =  2\r\nglobal const dib_pal_logindices =  4\r\nglobal const stm_seticon =  368\r\nglobal const stm_setimage =  370\r\nglobal const lr_loadfromfile =  16\r\nglobal const image_bitmap =  0\r\nglobal const image_icon =  1\r\nglobal const lr_copydeleteorg =  8\r\nglobal const lr_copyreturnorg =  4\r\nglobal const lr_monochrome =  1\r\nglobal const lr_createdibsection =  8192\r\nglobal const lr_defaultsize =  64\r\nglobal const ss_icon =  3\r\nglobal const ss_bitmap =  14\r\nglobal const gcl_menuname =  -8\r\nglobal const gcl_hbrbackground =  -10\r\nglobal const gcl_hcursor =  -12\r\nglobal const gcl_hicon =  -14\r\nglobal const gcl_hmodule =  -16\r\nglobal const gcl_cbwndextra =  -18\r\nglobal const gcl_cbclsextra =  -20\r\nglobal const gcl_wndproc =  -24\r\nglobal const gcl_style =  -26\r\nglobal const gcw_atom =  -32\r\nglobal const colour_scrollbar =  0\r\nglobal const colour_background =  1\r\nglobal const colour_desktop =  1\r\nglobal const colour_activecaption =  2\r\nglobal const colour_inactivecaption =  3\r\nglobal const colour_menu =  4\r\nglobal const colour_window =  5\r\nglobal const colour_windowframe =  6\r\nglobal const colour_menutext =  7\r\nglobal const colour_windowtext =  8\r\nglobal const colour_captiontext =  9\r\nglobal const colour_activeborder =  10\r\nglobal const colour_inactiveborder =  11\r\nglobal const colour_appworkspace =  12\r\nglobal const colour_highlight =  13\r\nglobal const colour_highlighttext =  14\r\nglobal const colour_btnface =  15\r\nglobal const colour_3dface =  15\r\nglobal const colour_btnshadow =  16\r\nglobal const colour_3dshadow =  16\r\nglobal const colour_graytext =  17\r\nglobal const colour_btntext =  18\r\nglobal const colour_inactivecaptiontext =  19\r\nglobal const colour_btnhighlight =  20\r\nglobal const colour_3dhilight =  20\r\nglobal const colour_3ddkshadow =  21\r\nglobal const colour_3dlight =  22\r\nglobal const colour_infotext =  23\r\nglobal const colour_infobk =  24\r\nglobal const colour_tooltipbk =  24\r\nglobal const mk_lbutton =  1\r\nglobal const mk_rbutton =  2\r\nglobal const mk_shift =  4\r\nglobal const mk_control =  8\r\nglobal const mk_mbutton =  16\r\nglobal const cbm_createdib =  2\r\nglobal const cbm_init =  4\r\nglobal const cc_enablehook =  16\r\nglobal const cc_enabletemplate =  32\r\nglobal const cc_enabletemplatehandle =  64\r\nglobal const cc_fullopen =  2\r\nglobal const cc_preventfullopen =  4\r\nglobal const cc_rgbinit =  1\r\nglobal const cc_showhelp =  8\r\nglobal const cc_solidcolour =  128\r\nglobal const cf_screenfonts =  1\r\nglobal const cf_printerfonts =  2\r\nglobal const cf_effects =  256\r\nglobal const size_restored =  0\r\nglobal const size_minimized =  1\r\nglobal const size_maximized =  2\r\nglobal const size_maxshow =  3\r\nglobal const size_maxhide =  4\r\n!global const gwl_wndproc =  -4\r\n!global const gwl_hinstance =  -6\r\n!global const gwl_hwndparent =  -8\r\n!global const gwl_style =  -16\r\n!global const gwl_exstyle =  -20\r\nglobal const gwl_userdata =  -21\r\nglobal const gwl_id =  -12\r\nglobal const ta_top =  0\r\nglobal const ta_left =  0\r\nglobal const ta_noupdatecp =  0\r\nglobal const ta_updatecp =  1\r\nglobal const ta_right =  2\r\nglobal const ta_centre =  6\r\nglobal const vta_centre =  6\r\nglobal const ta_bottom =  8\r\nglobal const ta_baseline =  24\r\nglobal const vta_baseline =  24\r\nglobal const ta_rtlreading =  256\r\nglobal const aligntop =  0\r\nglobal const alignbottom =  8\r\nglobal const alignbaseline =  24\r\nglobal const aligncentre =  6\r\nglobal const alignleft =  0\r\nglobal const alignright =  2\r\n\r\nglobal const em_exgetsel =  1076\r\nglobal const em_exlimittext =  1077\r\nglobal const em_exlinefromchar =  1078\r\nglobal const em_exsetsel =  1079\r\nglobal const em_getparaformat =  1085\r\nglobal const em_setparaformat =  1095\r\nglobal const em_streamin =  1097\r\nglobal const em_streamout =  1098\r\nglobal const em_gettextrange =  1099\r\nglobal const em_findtext =  1080\r\nglobal const em_findtextex =  1103\r\n\r\n!global const ttf_idishwnd =  1\r\n!global const ttf_centretip =  2\r\n!global const ttf_rtlreading =  4\r\n!global const ttf_subclass =  16\r\n!global const ttf_track =  32\r\n!global const ttf_absolute =  128\r\n!global const ttf_transparent =  256\r\n!global const ttf_di_setitem =  32768\r\n\r\nglobal const hwnd_top =  0\r\nglobal const hwnd_bottom =  1\r\nglobal const hwnd_topmost =  -1\r\nglobal const hwnd_notopmost =  -2\r\n\r\nglobal const normalwind =  0\r\nglobal const modalwind =  -1\r\nglobal const dialogwind =  -2\r\nglobal const minimize =  2\r\nglobal const maximize =  3\r\nglobal const shiftmask =  1\r\nglobal const controlmask =  2\r\nglobal const altmask =  4\r\nglobal const windowcolour =  15\r\nglobal const ps_geometric =  65536\r\nglobal const ps_cosmetic =  0\r\nglobal const ps_alternate =  8\r\nglobal const ps_solid =  0\r\nglobal const ps_dash =  1\r\nglobal const ps_dot =  2\r\nglobal const ps_dashdot =  3\r\nglobal const ps_dashdotdot =  4\r\nglobal const ps_null =  5\r\nglobal const ps_insideframe =  6\r\nglobal const ps_userstyle =  7\r\nglobal const ps_endcap_round =  0\r\nglobal const ps_endcap_square =  256\r\nglobal const ps_endcap_flat =  512\r\nglobal const ps_join_bevel =  4096\r\nglobal const ps_join_miter =  8192\r\nglobal const ps_join_round =  0\r\nglobal const ps_style_mask =  15\r\nglobal const ps_endcap_mask =  3840\r\nglobal const ps_type_mask =  983040\r\nglobal const bs_solid =  0\r\nglobal const bs_hollow =  1\r\nglobal const bs_null =  1\r\nglobal const bs_hatched =  2\r\nglobal const bs_pattern =  3\r\nglobal const bs_dibpattern =  5\r\nglobal const bs_dibpatternpt =  6\r\nglobal const bs_pattern8x8 =  7\r\nglobal const bs_dibpattern8x8 =  8\r\nglobal const hs_horizontal =  0\r\nglobal const hs_vertical =  1\r\nglobal const hs_fdiagonal =  2\r\nglobal const hs_bdiagonal =  3\r\nglobal const hs_cross =  4\r\nglobal const hs_diagcross =  5\r\n\r\n!global const gl_points =  0\r\n!global const gl_lines =  1\r\n!global const gl_line_loop =  2\r\n!global const gl_line_strip =  3\r\n!global const gl_triangles =  4\r\n!global const gl_triangle_strip =  5\r\n!global const gl_triangle_fan =  6\r\n!global const gl_quads =  7\r\n!global const gl_quad_strip =  8\r\n!global const gl_polygon =  9\r\n\r\nglobal const spi_getworkarea =  48\r\n\r\n!endexport\r\n\r\nproc start=\r\n!CPL \"START WINCONSTS\"\r\nend\r\n\r\n";
static char *	libnames[14]={"sys",
"files",
"clib",
"oslib",
"oslibsw",
"winlib",
"linlib",
"console",
"consolesw",
"wincon",
"lincon",
"condata",
"winapi",
"winconsts"
};
static char * *	libtext[14]={&l_sys,
&l_files,
&l_clib,
&l_oslib,
&l_oslibsw,
&l_winlib,
&l_linlib,
&l_console,
&l_consolesw,
&l_wincon,
&l_lincon,
&l_condata,
&l_winapi,
&l_winconsts
};

// From module: start
/* Local Static Variables */
static char *	pmnames[4]={"end","option","sourcefile","libfile"};
static int32	passes=0;
static byte	fshowoptions;
static byte	fshowmodules;
static byte	fshowfiles;
static byte	fshowtokens;
static byte	fshowast;
static byte	fshowst;
static byte	fshowstflat;
static byte	fshowtypes;
static byte	fshowpcl;
static byte	fshowdis;
static byte	fshowtiming;
static byte	fshowpaths;
static byte	fshowlog;
static byte	fgenpc;
static byte	fgenqa;
static char *	optionnames[25]={"load",
"lex",
"qa",
"parse",
"gen",
"pc",
"c",
"options",
"modules",
"files",
"ast",
"st",
"stflat",
"dis",
"paths",
"types",
"pcl",
"s",
"d",
"time",
"v",
"v2",
"h",
"help",
"ext"
};
static char *	infile;
static int32	logdest=0;
static int32	dopause=0;
static int32	totalpclopcodes=0;
static int32	totallines=0;
static int32	nstringobjects=0;
static byte	loadstatus;
static byte	parsestatus;
static byte	namestatus;
static byte	genstatus;
static byte	compilestatus;
static int32	clockstart;
static int32	modulelevel;
static char *	outfile;

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
        return ((a>>j)&~(-1<<(i-j)+1));
    }
    else {
        return ((a>>i)&~(-1<<(j-i)+1));
    }
}

uint64 m_anddotslice	(uint64 a,int32 i,int32 j) {
    if (i >= j) {
        return (((a>>j)&~(-1<<(i-j)+1))<<j);
    }
    else {
        return (((a>>i)&~(-1<<(j-i)+1))<<i);
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
            if ((aa&-268435456) != 0 || aa < 1048576) {
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


// From module: mm_nosdll
int64 os_calldll_wint	(void * fnaddr,int64 (*params)[],int32 nparams) {
    return 0;
}

double os_calldll_wreal	(void * fnaddr,int64 (*params)[],int32 nparams) {
    return 0.0;
}

void os_dummycall	(double a,double b,double c,double d) {
}


// From module: var_types

// From module: pq_common

// From module: qc_tables

// From module: var_decls

// From module: qc_lex
void lexreadtoken	(void) {
int32	c;
int32	csum;
int32	hsum;
int32	commentseen;
byte *	pstart;
byte *	pnext;
byte *	p;
    if (debug) {
        printf("%s\n","/LEXREADTOKEN");
    }
    nextlx.subcode = 0;
L1:;
    switch (*lxsptr++) {
    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
    case 'g':
    case 'h':
    case 'i':
    case 'j':
    case 'k':
    case 'l':
    case 'm':
    case 'n':
    case 'o':
    case 'p':
    case 'q':
    case 'r':
    case 's':
    case 't':
    case 'u':
    case 'v':
    case 'w':
    case 'x':
    case 'y':
    case 'z':
    case '$':
    case '_':
        nextlx.svalue = (char *)(lxsptr-1);
doname:
L3:;
        hsum = csum = (uchar)*nextlx.svalue;
        nextlx.hashvalue = 0;
L4:;
        switch (c = *lxsptr++) {
        case 'A':
        case 'B':
        case 'C':
        case 'D':
        case 'E':
        case 'F':
        case 'G':
        case 'H':
        case 'I':
        case 'J':
        case 'K':
        case 'L':
        case 'M':
        case 'N':
        case 'O':
        case 'P':
        case 'Q':
        case 'R':
        case 'S':
        case 'T':
        case 'U':
        case 'V':
        case 'W':
        case 'X':
        case 'Y':
        case 'Z':
            c += ' ';
            *(lxsptr-1) = c;
            csum += c;
            hsum = (hsum<<3)+csum;
            break;
        case 'a':
        case 'b':
        case 'c':
        case 'd':
        case 'e':
        case 'f':
        case 'g':
        case 'h':
        case 'i':
        case 'j':
        case 'k':
        case 'l':
        case 'm':
        case 'n':
        case 'o':
        case 'p':
        case 'q':
        case 'r':
        case 's':
        case 't':
        case 'u':
        case 'v':
        case 'w':
        case 'x':
        case 'y':
        case 'z':
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
        case '_':
        case '$':
            csum += c;
            hsum = (hsum<<3)+csum;
            break;
        case '"':
            --lxsptr;
            if (nextlx.svalue+1 == (char *)lxsptr && ((uchar)*nextlx.svalue == 'F' || (uchar)*nextlx.svalue == 'f')) {
                readrawstring();
                return;
            }
            goto L5;
            break;
        default:;
            --lxsptr;
            goto L5;
        }
        goto L4;
L5:;
        nextlx.symbol = rawnamesym;
        nextlx.length = lxsptr-(byte *)nextlx.svalue;
        nextlx.hashvalue = hsum<<5^csum;
        return;
        break;
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
    case 'G':
    case 'H':
    case 'I':
    case 'J':
    case 'K':
    case 'L':
    case 'M':
    case 'N':
    case 'O':
    case 'P':
    case 'Q':
    case 'R':
    case 'S':
    case 'T':
    case 'U':
    case 'V':
    case 'W':
    case 'X':
    case 'Y':
    case 'Z':
        nextlx.svalue = (char *)(lxsptr-1);
        *nextlx.svalue += ' ';
        goto L3;
        break;
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
        c = *(lxsptr-1);
        if ((*lxsptr==' ') || (*lxsptr==')') || (*lxsptr==cr) || (*lxsptr==',') || (*lxsptr=='|')) {
            nextlx.symbol = intconstsym;
            nextlx.subcode = tint;
            nextlx.value = (c-'0');
        } else if ((*lxsptr=='x') || (*lxsptr=='X')) {
            if ((c=='0')) {
                ++lxsptr;
                readnumber(16);
            } else if ((c=='1')) {
                lxerror("Bad base");
            }
            else {
                ++lxsptr;
                readnumber(c-'0');
            }
        }
        else {
            --lxsptr;
            readnumber(10);
        }
        return;
        break;
    case '!':
docomment:
L12:;
L13:;
        switch (c = *lxsptr++) {
        case 13:
            ++lxsptr;
            goto L14;
            break;
        case 10:
            goto L14;
            break;
        case etx:
        case 0:
            --lxsptr;
            goto L14;
            break;
        default:;
        }
        goto L13;
L14:;
        ++nextlx.lineno;
        nextlx.symbol = eolsym;
        return;
        break;
    case '#':
        goto L12;
        break;
    case '\\':
        commentseen = 0;
L15:;
        switch (*lxsptr++) {
        case cr:
            ++nextlx.lineno;
            ++lxsptr;
            goto L16;
            break;
        case lf:
            ++nextlx.lineno;
            goto L16;
            break;
        case etx:
        case 0:
            nextlx.symbol = eofsym;
            --lxsptr;
            return;
            break;
        case ' ':
        case tab:
            break;
        case '!':
            commentseen = 1;
            break;
        default:;
            if (!commentseen) {
                lxerror("\\ not followed by eol");
            }
        }
        goto L15;
L16:;
L17:;
        switch (*lxsptr++) {
        case cr:
            ++nextlx.lineno;
            ++lxsptr;
            break;
        case lf:
            ++nextlx.lineno;
            break;
        case ' ':
        case tab:
            break;
        default:;
            --lxsptr;
            goto L18;
        }
        goto L17;
L18:;
        break;
    case '{':
        nextlx.symbol = lcurlysym;
        return;
        break;
    case '}':
        nextlx.symbol = rcurlysym;
        return;
        break;
    case '.':
        switch (*lxsptr) {
        case '.':
            ++lxsptr;
            if (*lxsptr == '.') {
                ++lxsptr;
                nextlx.symbol = ellipsissym;
            }
            else {
                nextlx.symbol = rangesym;
                nextlx.subcode = j_makerange;
            }
            return;
            break;
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
            --lxsptr;
            readrealnumber(0,0,10);
            return;
            break;
        default:;
            p = lxsptr-2;
            if (p < lxstart || *p == cr || *p == lf) {
                nextlx.symbol = lexdotsym;
            }
            else {
                nextlx.symbol = dotsym;
            }
            return;
        }
        break;
    case ',':
        nextlx.symbol = commasym;
        return;
        break;
    case ';':
        nextlx.symbol = semisym;
        return;
        break;
    case ':':
        switch (*lxsptr) {
        case '=':
            ++lxsptr;
            if ((*lxsptr=='=')) {
                ++lxsptr;
                nextlx.symbol = deepcopysym;
                nextlx.subcode = j_deepcopyx;
            }
            else {
                nextlx.symbol = assignsym;
                nextlx.subcode = j_assignx;
            }
            break;
        case ':':
            ++lxsptr;
            if ((*lxsptr=='=')) {
                ++lxsptr;
                nextlx.symbol = deepcopysym;
                nextlx.subcode = j_deepcopyx;
            }
            else {
                nextlx.symbol = dcolonsym;
            }
            break;
        default:;
            nextlx.symbol = colonsym;
        }
        return;
        break;
    case '(':
        nextlx.symbol = lbracksym;
        return;
        break;
    case ')':
        nextlx.symbol = rbracksym;
        return;
        break;
    case '[':
        nextlx.symbol = lsqsym;
        return;
        break;
    case ']':
        nextlx.symbol = rsqsym;
        return;
        break;
    case '|':
        if (*lxsptr == '|') {
            ++lxsptr;
            nextlx.symbol = dbarsym;
        }
        else {
            nextlx.symbol = barsym;
        }
        return;
        break;
    case '^':
        nextlx.symbol = ptrsym;
        nextlx.subcode = j_ptrto;
        return;
        break;
    case '@':
        if (*lxsptr == '@') {
            ++lxsptr;
            nextlx.symbol = datsym;
        }
        else {
            nextlx.symbol = atsym;
        }
        return;
        break;
    case '?':
        nextlx.symbol = questionsym;
        return;
        break;
    case '~':
        nextlx.symbol = curlsym;
        return;
        break;
    case '+':
        nextlx.symbol = opsym;
        if (*lxsptr == '+') {
            ++lxsptr;
            nextlx.symbol = incrsym;
            nextlx.subcode = j_preincrx;
            return;
        }
        else {
            nextlx.subcode = j_add;
        }
        return;
        break;
    case '-':
        nextlx.symbol = opsym;
        if (*lxsptr == '-') {
            ++lxsptr;
            nextlx.symbol = incrsym;
            nextlx.subcode = j_predecrx;
            return;
        }
        else {
            nextlx.subcode = j_sub;
        }
        return;
        break;
    case '*':
        nextlx.symbol = opsym;
        if (*lxsptr == '*') {
            ++lxsptr;
            nextlx.subcode = j_power;
        }
        else {
            nextlx.subcode = j_mul;
        }
        return;
        break;
    case '/':
        nextlx.symbol = opsym;
        if ((*lxsptr=='/')) {
            ++lxsptr;
            nextlx.subcode = j_ddiv;
        }
        else {
            nextlx.subcode = j_div;
        }
        return;
        break;
    case '%':
        nextlx.symbol = opsym;
        nextlx.subcode = j_idiv;
        return;
        break;
    case '=':
        if ((*lxsptr=='>')) {
            nextlx.symbol = sendtosym;
            ++lxsptr;
        } else if ((*lxsptr=='=')) {
            nextlx.symbol = opsym;
            nextlx.subcode = j_isequal;
            ++lxsptr;
        }
        else {
            nextlx.symbol = opsym;
            nextlx.subcode = j_eq;
        }
        return;
        break;
    case '<':
        nextlx.symbol = opsym;
        switch (*lxsptr) {
        case '=':
            ++lxsptr;
            nextlx.subcode = j_le;
            break;
        case '>':
            ++lxsptr;
            nextlx.subcode = j_ne;
            break;
        case '<':
            ++lxsptr;
            nextlx.subcode = j_shl;
            break;
        default:;
            nextlx.subcode = j_lt;
        }
        return;
        break;
    case '>':
        nextlx.symbol = opsym;
        switch (*lxsptr) {
        case '=':
            ++lxsptr;
            nextlx.subcode = j_ge;
            break;
        case '>':
            ++lxsptr;
            nextlx.subcode = j_shr;
            break;
        default:;
            nextlx.subcode = j_gt;
        }
        return;
        break;
    case '&':
        if ((*lxsptr=='&')) {
            ++lxsptr;
            nextlx.symbol = opsym;
            nextlx.subcode = j_andand;
        } else if ((*lxsptr=='.')) {
            ++lxsptr;
            nextlx.symbol = anddotsym;
            nextlx.subcode = 0;
        }
        else {
            nextlx.symbol = addrsym;
            nextlx.subcode = j_addrof;
        }
        return;
        break;
    case '\'':
    case '`':
        lxreadstring('\'');
        return;
        break;
    case '"':
        lxreadstring('"');
        return;
        break;
    case ' ':
    case tab:
        break;
    case cr:
        ++lxsptr;
        ++nextlx.lineno;
        nextlx.symbol = eolsym;
        return;
        break;
    case lf:
        ++nextlx.lineno;
        nextlx.symbol = eolsym;
        return;
        break;
    case etx:
    case 0:
        if (macrolevel) {
            unstackmacro();
        }
        else {
            nextlx.symbol = eofsym;
            --lxsptr;
            return;
        }
        break;
    default:;
        nextlx.symbol = errorsym;
        nextlx.value = c;
        return;
    }
    goto L1;
}

static void lxreadstring	(int32 termchar) {
char *	dest;
int32	c;
int32	d;
int32	av_1;
    if (termchar == '"') {
        nextlx.symbol = stringconstsym;
        nextlx.subcode = tstring;
    }
    else {
        nextlx.symbol = charconstsym;
        nextlx.subcode = tint;
    }
    nextlx.svalue = (char *)lxsptr;
    dest = (char *)lxsptr;
    while (1) {
        switch (c = *lxsptr++) {
        case '\\':
            c = *lxsptr;
            if (c >= 'A' && c <= 'Z') {
                c += ' ';
            }
            ++lxsptr;
            switch (c) {
            case 'a':
                c = 7;
                break;
            case 'b':
                c = 8;
                break;
            case 'c':
            case 'r':
                c = cr;
                break;
            case 'e':
                c = 26;
                break;
            case 'f':
                c = 12;
                break;
            case 'l':
            case 'n':
                c = lf;
                break;
            case 's':
                c = 27;
                break;
            case 't':
                c = 9;
                break;
            case 'v':
                c = 11;
                break;
            case 'w':
                *dest++ = cr;
                c = lf;
                break;
            case 'x':
                c = 0;
                av_1 = 2;
                while (av_1--) {
                    if ((d = *lxsptr++=='A') || (d = *lxsptr++=='B') || (d = *lxsptr++=='C') || (d = *lxsptr++=='D') || (d = *lxsptr++=='E') || (d = *lxsptr++=='F')) {
                        c = (c*16+d-'A')+10;
                    } else if ((d = *lxsptr++=='a') || (d = *lxsptr++=='b') || (d = *lxsptr++=='c') || (d = *lxsptr++=='d') || (d = *lxsptr++=='e') || (d = *lxsptr++=='f')) {
                        c = (c*16+d-'a')+10;
                    } else if ((d = *lxsptr++=='0') || (d = *lxsptr++=='1') || (d = *lxsptr++=='2') || (d = *lxsptr++=='3') || (d = *lxsptr++=='4') || (d = *lxsptr++=='5') || (d = *lxsptr++=='6') || (d = *lxsptr++=='7') || (d = *lxsptr++=='8') || (d = *lxsptr++=='9')) {
                        c = c*16+d-'0';
                    }
                    else {
                        lxerror("Bad \\x code");
                    }
                }
                break;
            case 'y':
                c = 16;
                break;
            case 'z':
            case '0':
                c = 0;
                break;
            case '"':
            case 'Q':
                c = '"';
                break;
            case '\\':
                c = '\\';
                break;
            case '\'':
                c = '\'';
                break;
            default:;
                printf("%d %c %s %d\n",c,(char)c,"NEXTLX.LINENO=",nextlx.lineno);
                lxerror("Unknown string escape");
            }
            break;
        case '"':
        case '\'':
            if (c == termchar) {
                if (*lxsptr == c) {
                    ++lxsptr;
                }
                else {
                    goto L27;
                }
            }
            break;
        case cr:
        case lf:
        case etx:
        case 0:
            printf("%s %d\n","NEXTLX.LINENO=",nextlx.lineno);
            lxerror("String not terminated");
            break;
        default:;
        }
        *dest++ = c;
    }
L27:;
    nextlx.length = dest-nextlx.svalue;
    *(nextlx.svalue+nextlx.length) = 0;
}

static void readnumber	(int32 base) {
byte *	pstart;
byte *	dest;
int32	numtype;
int32	c;
char *	p;
    dest = pstart = lxsptr;
    if (base == 10) {
L37:;
        switch (c = *lxsptr++) {
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
            *dest++ = c;
            break;
        case '_':
        case '\'':
        case '`':
            break;
        default:;
            --lxsptr;
            goto L38;
        }
        goto L37;
L38:;
    }
    else {
        dest = scannumber(base);
        c = *lxsptr;
    }
    numtype = 0;
    switch (c) {
    case '.':
        if (*(lxsptr+1) != '.') {
            readrealnumber((char *)pstart,dest-pstart,base);
            return;
        }
        break;
    case 'e':
    case 'E':
        if (base <= 10) {
            readrealnumber((char *)pstart,dest-pstart,base);
            return;
        }
        break;
    case 'p':
    case 'P':
        if (base == 16) {
            readrealnumber((char *)pstart,dest-pstart,base);
            return;
        }
        break;
    case 'i':
    case 'I':
        ++lxsptr;
        numtype = tint;
        break;
    case 'w':
    case 'W':
    case 'u':
    case 'U':
        ++lxsptr;
        numtype = tword;
        break;
    case 'L':
    case 'l':
        ++lxsptr;
        numtype = tlongint;
        break;
    default:;
    }
    stringtonumber((char *)pstart,dest-pstart,base,numtype);
}

static void readrealnumber	(char * intstart,int32 intlen,int32 base) {
byte *	fractstart;
int32	fractlen;
int32	expon;
int32	i;
int32	c;
double	basex;
double	x;
enum {maxrealdigits = 500};
char	realstr[500];
int32	av_2;
int32	av_3;
int32	av_4;
    fractstart = 0;
    fractlen = 0;
    expon = 0;
    if (*lxsptr == '.') {
        fractstart = ++lxsptr;
        fractlen = scannumber(base)-fractstart;
    }
    if ((*lxsptr=='e') || (*lxsptr=='E')) {
        if (base != 16) {
            ++lxsptr;
            expon = readexponent(base);
        }
    } else if ((*lxsptr=='p') || (*lxsptr=='P')) {
        if (base == 16) {
            ++lxsptr;
            expon = readexponent(base);
        }
    }
    if (intlen+fractlen > maxrealdigits) {
        lxerror("Real too long");
    }
    if (intlen) {
        memcpy((int32 *)&realstr,(int32 *)intstart,intlen);
    }
    if (fractlen) {
        memcpy((int32 *)(&realstr[1-1]+intlen),(int32 *)fractstart,fractlen);
    }
    basex = base;
    expon -= fractlen;
    x = 0.0;
    for (i=1; i<=intlen+fractlen; ++i) {
        c = (uchar)realstr[i-1];
        if (c >= '0' && c <= '9') {
            x = x*basex+c-48.0;
        }
        else if (c > 'a') {
            x = (x*basex+c-97.0)+10.0;
        }
        else {
            x = (x*basex+c-65.0)+10.0;
        }
    }
    if (expon >= 0) {
        av_3 = expon;
        while (av_3--) {
            x *= basex;
        }
    }
    else {
        av_4 = -expon;
        while (av_4--) {
            x /= basex;
        }
    }
    nextlx.symbol = realconstsym;
    nextlx.subcode = treal;
    nextlx.xvalue = x;
}

static int32 readexponent	(int32 base) {
byte *	numstart;
byte *	numend;
int32	expon;
int32	length;
int32	neg;
    neg = 0;
    if ((*lxsptr=='+')) {
        ++lxsptr;
    } else if ((*lxsptr=='-')) {
        ++lxsptr;
        neg = 1;
    }
    numstart = lxsptr;
    length = scannumber(base)-numstart;
    if (length == 0) {
        lxerror("Bad expon");
    }
    stringtonumber((char *)numstart,length,base,0);
    return (neg?(-nextlx.value):(nextlx.value));
}

static void lxerror	(char * mess) {
    printf("%d %s %s %s %s\n",nextlx.lineno,"LEX ERROR",mess,"in",stmodule->name);
    abortprogram("Stopping");
}

void printsymbol	(lexrec * lp) {
lexrec	l;
    l = *lp;
    printf("%-18s",symbolnames[l.symbol-1]);
    if ((l.symbol==rawnamesym)) {
        printstrn(l.svalue,l.length);
        printf("%s%d%s"," (",l.hashvalue,")");
    } else if ((l.symbol==namesym)) {
        printstrn(l.symptr->name,l.symptr->namelen);
    } else if ((l.symbol==intconstsym)) {
        if ((l.subcode==tint)) {
            printf("%lld %s",l.value,"int");
        } else if ((l.subcode==tword)) {
            printf("%llu %s",l.uvalue,"word");
        }
        else {
            printf("%lld",l.value);
        }
    } else if ((l.symbol==realconstsym)) {
        printf("%f",l.xvalue);
    } else if ((l.symbol==stringconstsym)) {
        printf("%s","\"");
        printstrn(l.svalue,l.length);
        printf("%s","\"");
    } else if ((l.symbol==charconstsym)) {
        printf("%s","'");
        printstrn(l.svalue,l.length);
        printf("%s","'");
    } else if ((l.symbol==longintconstsym)) {
        printstrn(l.svalue,l.length);
        printf("%s","L");
    } else if ((l.symbol==opsym) || (l.symbol==assignsym) || (l.symbol==addrsym) || (l.symbol==ptrsym) || (l.symbol==deepcopysym) || (l.symbol==rangesym)) {
        printf("%s",jtagnames[l.subcode]);
    }
    else {
        if (!!l.subcode) {
            printf("%s %d","#",l.subcode);
        }
    }
    printf("\n");
}

static void stringtonumber	(char * s,int32 length,int32 base,int32 numtype) {
int64	a;
uint64	b;
int32	c;
int32	av_5;
int32	av_6;
    while (length >= 2 && (uchar)*s == '0') {
        ++s;
        --length;
    }
    if (numtype == tlongint || length > maxnumlen[base-1] || length == maxnumlen[base-1] && strncmp(s,maxnumlist[base-1],length) > 0) {
        nextlx.symbol = longintconstsym;
        nextlx.svalue = s;
        nextlx.length = length;
        if (numtype && numtype != tlongint) {
            lxerror("Can't apply width override to longint");
        }
        return;
    }
    a = 0;
    if (base <= 10) {
        av_5 = length;
        while (av_5--) {
            a = a*base+(uchar)*s++-'0';
        }
    }
    else {
        av_6 = length;
        while (av_6--) {
            c = (uchar)*s++;
            if (c >= 'a') {
                a = (a*base+c-'a')+10;
            }
            else if (c >= 'A') {
                a = (a*base+c-'A')+10;
            }
            else {
                a = a*base+c-'0';
            }
        }
    }
    nextlx.symbol = intconstsym;
    nextlx.value = a;
    if (numtype) {
        nextlx.subcode = numtype;
        return;
    }
    b = (uint64)a;
    if (b < 9223372036854775807ll) {
        nextlx.subcode = tint;
    }
    else {
        nextlx.subcode = tword;
    }
}

void lexsetup	(void) {
int32	i;
static int32	n;
    for (i=1; i<=16; ++i) {
        maxnumlen[i-1] = strlen(maxnumlist[i-1]);
    }
    inithashtable();
    n = 0;
    for (i=0; i<=hstmask; ++i) {
        if (!!hashtable[i].name) {
            ++n;
        }
    }
}

void printstrn	(char * s,int32 length) {
    if (length) {
        printf("%.*s",length,s);
    }
}

static byte * scannumber	(int32 base) {
byte *	dest;
int32	c;
    dest = lxsptr;
L89:;
    switch (c = *lxsptr++) {
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
        *dest++ = c;
        if (c >= '0'+base) {
            lxerror("Digit out of range");
        }
        break;
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
        if (base == 16) {
            *dest++ = c;
        }
        else {
            --lxsptr;
            goto L90;
        }
        break;
    case '_':
    case '\'':
    case '`':
        break;
    default:;
        --lxsptr;
        goto L90;
    }
    goto L89;
L90:;
    return dest;
}

static void readrawstring	(void) {
char *	dest;
int32	c;
    nextlx.symbol = stringconstsym;
    nextlx.subcode = tstring;
    nextlx.svalue = (char *)++lxsptr;
    dest = (char *)lxsptr;
L91:;
    switch (c = *lxsptr++) {
    case '"':
        if (*lxsptr == '"') {
            *dest++ = '"';
            ++lxsptr;
        }
        else {
            *(lxsptr-1) = 0;
            goto L92;
        }
        break;
    case cr:
    case lf:
    case etx:
    case 0:
        lxerror("Raw string not terminated");
        --lxsptr;
        goto L92;
        break;
    default:;
        *dest++ = c;
    }
    goto L91;
L92:;
    nextlx.length = dest-nextlx.svalue;
}

static int32 lookup	(void) {
int32	j;
int32	wrapped;
    ++NLOOKUPS;
    j = nextlx.hashvalue&hstmask;
    lxsymptr = &hashtable[j];
    wrapped = 0;
    while (1) {
        if (lxsymptr->name == 0) {
            goto L94;
        }
        if (lxsymptr->namelen == nextlx.length) {
            if (memcmp(lxsymptr->name,nextlx.svalue,nextlx.length) == 0) {
                return 1;
            }
        }
        ++NCLASHES;
        ++lxsymptr;
        if (++j >= hstsize) {
            if (wrapped) {
                abortprogram("HASHTABLE FULL");
            }
            wrapped = 1;
            lxsymptr = &hashtable[0];
            j = 0;
        }
    }
L94:;
    lxsymptr->name = nextlx.svalue;
    lxsymptr->namelen = nextlx.length;
    lxsymptr->symbol = rawnamesym;
    return 0;
}

static int32 gethashvaluez	(char * s) {
int32	c;
int32	csum;
int32	hsum;
    if ((uchar)*s == 0) {
        return 0;
    }
    hsum = csum = (uchar)*s++;
    while (1) {
        c = (uchar)*s++;
        if (c == 0) {
            goto L96;
        }
        csum += c;
        hsum = (hsum<<3)+csum;
    }
L96:;
    return hsum<<5^csum;
}

static void inithashtable	(void) {
int32	i;
    memset((int32 *)&hashtable,0,11010048);
    for (i=1; i<=323; ++i) {
        nextlx.svalue = stnames[i-1];
        nextlx.length = strlen(nextlx.svalue);
        nextlx.hashvalue = gethashvaluez(nextlx.svalue);
        if (!!lookup()) {
            printf("%s\n",stnames[i-1]);
            abortprogram("Duplicate symbol table entry");
        }
        lxsymptr->symbol = stsymbols[i-1];
        if ((stsymbols[i-1]==unitnamesym)) {
            lxsymptr->index = stsubcodes[i-1];
            lxsymptr->subcode = unitnamesym;
            lxsymptr->symbol = rawnamesym;
        }
        else {
            lxsymptr->subcode = stsubcodes[i-1];
        }
    }
}

static int32 dolexdirective	(int32 index) {
strec *	symptr;
byte *	p;
char *	file;
int32	i;
    if ((index==definedir)) {
        lexreadtoken();
        if (nextlx.symbol != rawnamesym) {
            lxerror("define: name expected");
        }
        if (!!lookup() && lxsymptr->symbol == lexmacronamesym) {
            printstrn(nextlx.svalue,nextlx.length);
            lxerror("Macro already defined");
        }
        symptr = lxsymptr;
        lexreadtoken();
        if (!(nextlx.symbol == opsym && nextlx.subcode == j_eq)) {
            lxerror("\"=\" expected");
        }
        p = lxsptr;
        lexreadline();
        addmacro(symptr,(char *)p,lxsptr-p);
        lexreadtoken();
        if (nextlx.symbol != eolsym) {
            lxerror("Bad define");
        }
        return 0;
    } else if ((index==strincludedir)) {
        lexreadtoken();
        if (nextlx.symbol != stringconstsym) {
            lxerror("strincl: string expected");
        }
        file = nextlx.svalue;
        nextlx.svalue = (char *)readfile(file);
        if (nextlx.svalue == 0) {
            for (i=1; i<=nextra; ++i) {
                if (strcmp(file,extrafiles[i-1]) == 0) {
                    nextlx.svalue = extratext[i-1];
                    goto L107;
                }
            }
L107:;
            if (nextlx.svalue == 0) {
                printf("%s\n",file);
                lxerror("Can't find strinclude file");
            }
        }
        nextlx.symbol = stringconstsym;
        nextlx.subcode = tstring;
        nextlx.length = rfsize;
        *(nextlx.svalue+rfsize) = 0;
        return 1;
    } else if ((index==exportdir)) {
    } else if ((index==endexportdir)) {
    }
    else {
        printf("%s\n",sourcedirnames[index-1]);
        lxerror("Directive not implemented");
    }
    return 0;
}

static void lexreadline	(void) {
L110:;
    switch (*lxsptr) {
    case cr:
    case lf:
        return;
        break;
    case etx:
    case 0:
        --lxsptr;
        return;
        break;
    default:;
        ++lxsptr;
    }
    goto L110;
}

void startlex	(char * caption,char * sourcecode) {
    lxsptr = (byte *)sourcecode;
    nextlx.lineno = 1;
    nextlx.symbol = semisym;
    nextlx.subcode = 0;
}

char * convertzstring	(char * s,int32 length) {
static char	str[300];
    if (length > 300) {
        abortprogram("convertzstr");
    }
    memcpy((int32 *)&str,(int32 *)s,length);
    str[length+1-1] = 0;
    return (char *)&str;
}

strec * addnamestr	(char * name) {
lexrec	oldlx;
    oldlx = nextlx;
    nextlx.hashvalue = gethashvaluez(name);
    nextlx.length = strlen(name);
    nextlx.svalue = (char *)pcm_alloc(nextlx.length+1);
    memcpy((int32 *)nextlx.svalue,(int32 *)name,nextlx.length+1);
    lookup();
    nextlx = oldlx;
    return lxsymptr;
}

void PS1	(char * caption) {
    printf("%s%s",caption,":::");
    printsymbol(&lx);
}

void PS2	(char * caption) {
    printf("%s%s%s","\t",caption,":##");
    printsymbol(&nextlx);
}

void PS	(char * caption) {
    PS1(caption);
    PS2(caption);
}

void lex	(void) {
int32	lineno;
int32	n;
char *	p;
    lx = nextlx;
reenter:
L112:;
    lexreadtoken();
    if (lx.symbol == namesym) {
        *(lx.symptr->name+lx.length) = 0;
    }
    switch (nextlx.symbol) {
    case eolsym:
        if ((lx.symbol==commasym) || (lx.symbol==lsqsym) || (lx.symbol==lbracksym)) {
            goto L112;
        } else if ((lx.symbol==semisym)) {
            goto L112;
        }
        else {
            nextlx.symbol = semisym;
        }
        break;
    case rawnamesym:
        if (!lookup()) {
            nextlx.symbol = namesym;
            nextlx.symptr = lxsymptr;
            return;
        }
        nextlx.symbol = lxsymptr->symbol;
        nextlx.subcode = lxsymptr->subcode;
        switch (nextlx.symbol) {
        case lexmacronamesym:
            stackmacro(lxsymptr->macrovalue);
            goto L112;
            break;
        case ksourcedirsym:
            if (!dolexdirective(nextlx.subcode)) {
                goto L112;
            }
            break;
        case rawnamesym:
            if (nextlx.subcode == unitnamesym && (lx.symbol == intconstsym || lx.symbol == realconstsym)) {
                if ((lx.symbol==intconstsym)) {
                    if ((lxsymptr->index==million_unit)) {
                        lx.value *= 1000000;
                    } else if ((lxsymptr->index==billion_unit)) {
                        lx.value *= 1000000000;
                    } else if ((lxsymptr->index==thousand_unit)) {
                        lx.value *= 1000;
                    }
                    else {
                        lxerror("Can't do this unit index");
                    }
                }
                else {
                    lxerror("Unit suffix after float not implem");
                }
                goto L112;
            }
            else {
                nextlx.symbol = namesym;
                nextlx.svalue = (char *)lxsymptr;
            }
            break;
        case namesym:
            lxerror("NEXT NAME!!!");
            break;
        case kifsym:
        case kcasesym:
        case kswitchsym:
        case kdocasesym:
        case kdoswitchsym:
        case kforsym:
        case kforallsym:
        case kdosym:
        case ktosym:
        case kprocsym:
        case kfunctionsym:
        case kmethodsym:
        case kimportmodulesym:
        case kunlesssym:
        case krecordsym:
        case kstructsym:
        case kunionsym:
        case ktypesym:
        case kwhilesym:
        case kclasssym:
        case ktrysym:
        case ktabledatasym:
            if (lx.symbol == kendsym) {
                lx.subcode = nextlx.symbol;
                goto L112;
            }
            break;
        case opsym:
            goto L120;
            break;
        case sysconstsym:
            if ((nextlx.subcode==nil_const) || (nextlx.subcode==con_const)) {
                nextlx.symbol = intconstsym;
                nextlx.value = 0;
                nextlx.subcode = tint;
            } else if ((nextlx.subcode==pi_const)) {
                nextlx.symbol = realconstsym;
                nextlx.xvalue = 3.1415926535897931;
                nextlx.subcode = treal;
            } else if ((nextlx.subcode==tab_const)) {
                nextlx.symbol = stringconstsym;
                nextlx.subcode = tstring;
                nextlx.svalue = "\t";
                nextlx.length = 1;
            }
            else {
                lxerror("sysconst?");
            }
            break;
        default:;
        }
        break;
    case lexdotsym:
        goto L112;
        break;
    case eofsym:
        break;
    case stringconstsym:
        if (lx.symbol == stringconstsym) {
            n = nextlx.length+lx.length;
            p = (char *)pcm_alloc(n+1);
            memcpy((int32 *)p,(int32 *)lx.svalue,lx.length);
            memcpy((int32 *)(p+lx.length),(int32 *)nextlx.svalue,nextlx.length);
            *(p+n) = 0;
            lx.svalue = p;
            lx.length = n;
            goto L112;
        }
        break;
    case opsym:
doopsym:
L120:;
        if (nextlx.subcode == j_in && lx.symbol == opsym && lx.subcode == j_notl) {
            lx.subcode = j_notin;
            goto L112;
        }
        break;
    default:;
    }
}

void showhashtablesize	(void) {
int32	i;
int32	n;
    n = 0;
    for (i=0; i<=hstmask; ++i) {
        if (!!hashtable[i].name) {
            ++n;
        }
    }
    printf("%s %d %d\n","FINAL HASHTABLE",n,hstsize);
}

static void addmacro	(strec * symptr,char * value,int32 length) {
char *	s;
int32	i;
    s = value;
    for (i=1; i<=length; ++i) {
        if (((uchar)*s=='"')) {
        } else if (((uchar)*s=='#') || ((uchar)*s=='!')) {
            length = i-1;
            if (length == 0) {
                lxerror("Null macro");
            }
            goto L132;
        }
        ++s;
    }
L132:;
    symptr->symbol = lexmacronamesym;
    *(symptr->name+symptr->namelen) = 0;
    s = (char *)pcm_alloc(length+2);
    memcpy((int32 *)s,(int32 *)value,length);
    *(s+length+1) = etx;
    *(s+length+2) = 0;
    symptr->macrovalue = s;
}

static void stackmacro	(char * s) {
    if (macrolevel >= maxmacrodepth) {
        lxerror("Too many nested macros");
    }
    ++macrolevel;
    macrostack[macrolevel-1] = lxsptr;
    lxsptr = (byte *)pcm_copyheapstring(s);
}

static void unstackmacro	(void) {
    if (macrolevel <= 0) {
        lxerror("unstack macro?");
    }
    lxsptr = macrostack[macrolevel---1];
}

static int32 testlookup	(void) {
int32	j;
int32	wrapped;
billy:
    ++NLOOKUPS;
    j = nextlx.hashvalue&hstmask;
    lxsymptr = &hashtable[j];
    lxsymptr->name = nextlx.svalue;
    return 0;
}


// From module: support
void prterror	(char * mess) {
    printf("%s %s\n","Print error:",mess);
    os_getch();
    exit(1);
}

void serror	(char * mess) {
    printf("%s %s %s %d %s\n","Syntax error:",mess,"on line",lx.lineno,stmodule->name);
    os_getch();
    printf("\n");
    printf("\n");
    printf("\n");
    printf("\n");
    printf("\n");
    printf("%s\n","--");
    exit(1);
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
    exit(1);
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
    exit(1);
}

int32 testelem	(byte (*p)[],int32 n) {
    return (!!((*p)[n>>3]&bytemasks[n&7])?1:0);
}

void setelem	(byte (*p)[],int32 n) {
    (*p)[n>>3] |= bytemasks[n&7];
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

void initpcdest	(void) {
    pcalloc = 16384;
    pcstart = (byte *)pcm_alloc(pcalloc);
    pcend = pcstart+pcalloc;
    pcdest = pcstart;
}

int32 getpcpos	(void) {
    return pcdest-pcstart;
}

void setpcpos	(int32 pos) {
    pcdest = pcstart+pos;
}

int32 writepcdata	(char * filename) {
int32	nbytes;
void *	f;
    f = fopen(filename,"wb");
    if (f == 0) {
        printf("%s %s\n","Couldn't create",filename);
        exit(1);
    }
    fwrite((int32 *)pcstart,pcdest-pcstart,1,f);
    fclose(f);
    return 1;
}

void writezstring	(char * s) {
int32	i;
int32	n;
int32	av_1;
    outpcbyte(zstring);
    n = strlen(s);
    av_1 = n;
    while (av_1--) {
        outpcbyte((uchar)*s++);
    }
    outpcbyte(0);
}

void writezint	(int64 x) {
byte *	p;
int32	av_2;
    if (x >= 0 && x <= zmax) {
        outpcbyte(x);
    }
    else if (x >= 240 && x < 480) {
        outpcbyte(zint240);
        outpcbyte((x-240));
    }
    else if (x >= 480 && x < 720) {
        outpcbyte(zint480);
        outpcbyte((x-480));
    }
    else if (x >= 720 && x < 960) {
        outpcbyte(zint720);
        outpcbyte((x-720));
    }
    else if (x >= -127 && x < 0) {
        outpcbyte(zint1);
        outpcbyte(-x);
    }
    else if (x >= -32768 && x <= 32767) {
        outpcbyte(zint2);
        outpcword16(x);
    }
    else if (x > -2147483648ll && x <= 2147483647) {
        outpcbyte(zint4);
        outpcword(x);
    }
    else {
        p = (byte *)&x;
        outpcbyte(zint8);
        av_2 = 8;
        while (av_2--) {
            outpcbyte(*p++);
        }
    }
}

void writezint4	(int32 x) {
    outpcbyte(zint4);
    outpcword(x);
}

void writezrange	(byte * p) {
int32	av_3;
    outpcbyte(zint8);
    av_3 = 8;
    while (av_3--) {
        outpcbyte(*p++);
    }
}

void writezreal	(double x) {
byte *	p;
int32 *	q;
int32	av_4;
int32	av_5;
    p = (byte *)&x;
    q = (int32 *)&x;
    if (q != 0) {
        outpcbyte(zreal8);
        av_4 = 8;
        while (av_4--) {
            outpcbyte(*p++);
        }
    }
    else {
        outpcbyte(zreal4);
        p += 4;
        av_5 = 4;
        while (av_5--) {
            outpcbyte(*p++);
        }
    }
}

void writezeof	(void) {
    outpcbyte(zeof);
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

void gs_additem	(strbuffer * dest,char * s) {
char *	d;
int32	lastchar;
int32	nextchar;
    d = dest->strptr;
    if (!!dest->length) {
        lastchar = (uchar)*(d+dest->length-1);
        nextchar = (uchar)*s;
        if (!!isalphanum(lastchar) && !!isalphanum(nextchar)) {
            strbuffer_add(dest," ",-1);
        }
    }
    strbuffer_add(dest,s,-1);
}

static int32 isalphanum	(int32 c) {
    if (c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9') {
        return 1;
    }
    return 0;
}

void strbuffer_add	(strbuffer * dest,char * s,int32 n) {
int32	newlen;
int32	oldlen;
char *	newptr;
    if (n == -1) {
        n = strlen(s);
    }
    oldlen = dest->length;
    if (oldlen == 0) {
        dest->strptr = (char *)pcm_alloc(n+1);
        dest->allocated = allocbytes;
        dest->length = n;
        memcpy((int32 *)dest->strptr,(int32 *)s,n);
        *(dest->strptr+n) = 0;
        return;
    }
    newlen = oldlen+n;
    if (newlen+1 > dest->allocated) {
        newptr = (char *)pcm_alloc(newlen+1);
        memcpy((int32 *)newptr,(int32 *)dest->strptr,newlen+1);
        dest->strptr = newptr;
        dest->allocated = allocbytes;
    }
    memcpy((int32 *)(dest->strptr+oldlen),(int32 *)s,n);
    *(dest->strptr+newlen) = 0;
    dest->length = newlen;
}

void gs_init	(strbuffer * dest) {
    pcm_clearmem(dest,12);
}

void gs_str	(strbuffer * dest,char * s) {
    strbuffer_add(dest,s,-1);
}

void gs_strn	(strbuffer * dest,char * s,int32 length) {
    strbuffer_add(dest,s,length);
}

void gs_strvar	(strbuffer * dest,strbuffer * s) {
    strbuffer_add(dest,s->strptr,-1);
}

void gs_strint	(strbuffer * dest,int64 a) {
char	str[256];
    sprintf((char *)&str,"%lld",a);
    strbuffer_add(dest,(char *)&str,-1);
}

void gs_strln	(strbuffer * dest,char * s) {
    gs_str(dest,s);
    gs_line(dest);
}

void gs_strsp	(strbuffer * dest,char * s) {
    gs_str(dest,s);
    gs_str(dest," ");
}

void gs_line	(strbuffer * dest) {
    strbuffer_add(dest,"\r\n",-1);
}

int32 gs_getcol	(strbuffer * dest) {
    return dest->length;
}

void gs_leftstr	(strbuffer * dest,char * s,int32 w,int32 padch) {
int32	col;
int32	i;
int32	n;
int32	slen;
char	str[256];
    col = dest->length;
    strcpy((char *)&str,s);
    slen = strlen(s);
    n = w-slen;
    if (n > 0) {
        for (i=1; i<=n; ++i) {
            str[slen+i-1] = padch;
        }
        str[slen+n+1-1] = 0;
    }
    gs_str(dest,(char *)&str);
}

void gs_leftint	(strbuffer * dest,int32 a,int32 w,int32 padch) {
char	str[256];
    sprintf((char *)&str,"%d",a);
    gs_leftstr(dest,(char *)&str,w,padch);
}

void gs_padto	(strbuffer * dest,int32 col,int32 ch) {
int32	i;
int32	n;
char	str[256];
    n = col-dest->length;
    if (n <= 0) {
        return;
    }
    for (i=1; i<=n; ++i) {
        str[i-1] = ch;
    }
    str[n+1-1] = 0;
    gs_str(dest,(char *)&str);
}

void gs_println	(strbuffer * dest,void * f) {
    if (f == 0) {
        printf("%.*s\r\n",dest->length,dest->strptr);
    }
    else {
        fprintf(f,"%.*s\r\n",dest->length,dest->strptr);
    }
}

static void gs_copytostr	(strbuffer * source,char * s) {
    if (!!source->length) {
        memcpy((int32 *)s,(int32 *)source->strptr,source->length);
        *(s+source->length) = 0;
    }
    else {
        *s = 0;
    }
}

static void outpcbyte	(int32 x) {
int32	newalloc;
int32	oldbytes;
byte *	pcnew;
    if (pcdest >= pcend) {
        newalloc = pcalloc*2;
        pcnew = (byte *)pcm_alloc(newalloc);
        oldbytes = pcdest-pcstart;
        memcpy((int32 *)pcnew,(int32 *)pcstart,oldbytes);
        pcstart = pcnew;
        pcend = pcstart+newalloc;
        pcdest = pcstart+oldbytes;
        pcalloc = newalloc;
    }
    *pcdest++ = x;
}

static void outpcword	(int32 x) {
byte *	p=(byte *)&x;
    outpcbyte(*p++);
    outpcbyte(*p++);
    outpcbyte(*p++);
    outpcbyte(*p);
}

static void outpcword16	(int32 x) {
byte *	p=(byte *)&x;
    outpcbyte(*p++);
    outpcbyte(*p);
}


// From module: qc_lib
static strec * newstrec	(void) {
strec *	p;
    p = (strec *)pcm_alloc(84);
    memset((int32 *)p,0,84);
    p->lineno = lx.lineno;
    p->attribs.ax_moduleno = currmoduleno;
    return p;
}

void initqclib	(void) {
int32	i;
    for (i=1; i<=36; ++i) {
        jtagpriotable[oplist[i-1]] = oppriolist[i-1];
    }
    for (i=1; i<=23; ++i) {
        exprstarterset[D_exprstarterset[i-1]] = 1;
    }
    for (i=1; i<=6; ++i) {
        typestarterset[D_typestarterset[i-1]] = 1;
    }
    hostlvset[host_iconvlc] = 1;
    hostlvset[host_iconvuc] = 1;
    condopset[j_eq] = 1;
    condopset[j_ne] = 1;
    condopset[j_lt] = 1;
    condopset[j_le] = 1;
    condopset[j_ge] = 1;
    condopset[j_gt] = 1;
}

strec * getduplnameptr	(strec * owner,strec * symptr,int32 id) {
strec *	p;
strec *	q;
    p = newstrec();
    p->name = symptr->name;
    p->namelen = symptr->namelen;
    p->symbol = namesym;
    p->owner = owner;
    p->nameid = id;
    if (id == frameid || id == paramid) {
        p->attribs.ax_frame = 1;
    }
    if (!!(q = symptr->nextdupl)) {
        q->prevdupl = p;
    }
    p->nextdupl = q;
    p->prevdupl = symptr;
    symptr->nextdupl = p;
    return p;
}

void adddef	(strec * owner,strec * p) {
strec *	q;
    q = p;
    while (q = q->nextdupl) {
        if (q->owner == owner) {
            printf("%s %s %s\n",q->name,"in",owner->name);
            serror("Duplicate name");
        }
    }
    p->nextdef = owner->deflist;
    owner->deflist = p;
}

void adddef_nodupl	(strec * owner,strec * p) {
    p->nextdef = owner->deflist;
    owner->deflist = p;
}

void printst	(void * f,strec * p,int32 level) {
strec *	q;
    if (p->symbol != namesym) {
        printf("%s\n","PRINTST not name\n\n\n");
        exit(0);
    }
    printstrec(f,p,level);
    q = p->deflist;
    while (q != 0) {
        printst(f,q,level+1);
        q = q->nextdef;
    }
}

static void printstrec	(void * f,strec * p,int32 level) {
attribrec	attrs;
byte *	q;
strbuffer	v;
strbuffer *	d=&v;
int32	col;
int32	offset;
char	str[256];
int32	av_1;
    gs_init(d);
    offset = 0;
    av_1 = level;
    while (av_1--) {
        gs_str(d,"    ");
        offset += 4;
    }
    gs_str(d,":");
    gs_leftstr(d,p->name,28-offset,'-');
    gs_leftstr(d,namenames[p->nameid],12,'.');
    col = gs_getcol(d);
    attrs = p->attribs;
    gs_str(d,"[");
    if ((getscope(p)==localscope)) {
        gs_str(d,"Loc ");
    } else if ((getscope(p)==exportscope)) {
        gs_str(d,"Glob ");
    } else if ((getscope(p)==importscope)) {
        gs_str(d,"Ext ");
    }
    if (!!attrs.ax_static) {
        gs_str(d,"Stat");
    }
    if (!!attrs.ax_fflang) {
        gs_strsp(d,fflangnames[attrs.ax_fflang]);
    }
    if (!!attrs.ax_byrefmode) {
        gs_str(d,"byref ");
    }
    if (!!attrs.ax_align) {
        gs_str(d,"@@");
        gs_strint(d,attrs.ax_align);
        gs_str(d," ");
    }
    if (!!attrs.ax_optional) {
        gs_str(d,"Opt ");
    }
    if (!!attrs.ax_varparams) {
        gs_str(d,"Var ");
    }
    if (!!attrs.ax_used) {
        gs_str(d,"Used ");
    }
    if (!!attrs.ax_forward) {
        gs_str(d,"Fwd ");
    }
    if (!!attrs.ax_frame) {
        gs_str(d,"Frm ");
    }
    if (!!attrs.ax_autovar) {
        gs_str(d,"AV ");
    }
    if (!!attrs.ax_nparams) {
        sprintf((char *)&str,"Pm:%d ",attrs.ax_nparams);
        gs_str(d,(char *)&str);
    }
    if (!!attrs.ax_moduleno) {
        sprintf((char *)&str,"Modno#%d ",attrs.ax_moduleno);
        gs_str(d,(char *)&str);
    }
    gs_str(d,"]");
    gs_padto(d,col+10,'=');
    if (!!p->owner) {
        sprintf((char *)&str,"(%s)",p->owner->name);
        gs_leftstr(d,(char *)&str,18,'+');
    }
    else {
        gs_leftstr(d,"()",18,'+');
    }
    if ((p->mode==tvoid)) {
        gs_str(d,"Void ");
    } else if ((p->mode==tvariant)) {
        gs_str(d,"Var ");
    }
    else {
        gs_strsp(d,strmode(p->mode,1));
    }
    if ((p->nameid==fieldid) || (p->nameid==paramid)) {
        gs_str(d,"Offset:");
        gs_strint(d,p->offset);
        sprintf((char *)&str,"%.*s",p->uflags.ulength,&(p->uflags).codes);
        gs_str(d," UFLAGS:");
        gs_str(d,(char *)&str);
        gs_strint(d,p->uflags.ulength);
    } else if ((p->nameid==genfieldid)) {
        gs_str(d,"Index:");
        gs_strint(d,p->offset);
    } else if ((p->nameid==procid)) {
        gs_str(d,"Index:");
        gs_strint(d,p->index);
        gs_str(d," Address:");
        sprintf((char *)&str,"%p",p->address);
        gs_str(d,(char *)&str);
    } else if ((p->nameid==dllprocid)) {
        gs_str(d,"Index/PCaddr:");
        gs_strint(d,p->index);
        if (!!p->truename) {
            gs_str(d," Truename:");
            gs_str(d,p->truename);
        }
    } else if ((p->nameid==constid)) {
        gs_str(d,"Const:");
        gs_strvar(d,strexpr(p->code));
    } else if ((p->nameid==typeid)) {
        if (!!p->base_class) {
            gs_str(d,"Baseclass:");
            gs_str(d,ttname[p->base_class]);
        }
    } else if ((p->nameid==enumid)) {
        gs_str(d,"Enum:");
        gs_strint(d,p->index);
    } else if ((p->nameid==dllmoduleid)) {
        gs_str(d,"DLL#:");
        gs_strint(d,p->attribs.ax_dllindex);
    }
    gs_str(d," ");
    if (!!p->equiv) {
        if ((p->nameid==aliasid)) {
            gs_str(d,"Alias for:");
            gs_str(d,p->equiv->name);
        } else if ((p->nameid==linkid)) {
            gs_str(d,"Link to:");
            gs_str(d,getdottedname(p->equiv));
        }
        else {
            gs_str(d,"@");
            gs_str(d,p->equiv->name);
        }
        gs_str(d," ");
    }
    gs_str(d,"Lineno:");
    gs_strint(d,p->lineno);
    gs_println(d,f);
}

void printstflat	(void * f) {
int32	i;
strec *	p;
lexrec *	lx;
    fprintf(f,"%s\n","GLOBAL SYMBOL TABLE:");
    for (i=0; i<=131070; ++i) {
        p = &hashtable[i];
        if (!!p->name) {
            if ((p->symbol==rawnamesym) || (p->symbol==lexmacronamesym)) {
                fprintf(f,"%d %p %s %s %s %s\n",i,(void*)(p),":",p->name,symbolnames[p->symbol-1],namenames[p->nameid]);
                if (p->symbol == lexmacronamesym) {
                    fprintf(f,"%s %s\n","\t\t\t",p->macrovalue);
                }
                p = p->nextdupl;
                while (p) {
                    fprintf(f,"%s %p %s %s %s %p %s %s%s\n","\t",(void*)(p),p->name,symbolnames[p->symbol-1],namenames[p->nameid],(void*)(p->prevdupl),"(From",(!!p->owner?(p->owner)->name:"-"),")");
                    p = p->nextdupl;
                }
            }
        }
    }
}

static unitrec * newunitrec	(void) {
unitrec *	p;
    p = (unitrec *)pcm_alloc(40);
    memset((int32 *)p,0,40);
    p->lineno = lx.lineno;
    return p;
}

unitrec * createname	(strec * p) {
unitrec *	u;
    u = newunitrec();
    u->tag = j_name;
    u->def = p;
    return u;
}

unitrec * createunit0	(int32 tag) {
unitrec *	u;
    u = newunitrec();
    u->tag = tag;
    return u;
}

unitrec * createunit1	(int32 tag,unitrec * p) {
unitrec *	u;
    u = newunitrec();
    u->tag = tag;
    u->a = p;
    return u;
}

unitrec * createunit2	(int32 tag,unitrec * p,unitrec * q) {
unitrec *	u;
    u = newunitrec();
    u->tag = tag;
    u->a = p;
    u->b = q;
    return u;
}

unitrec * createunit3	(int32 tag,unitrec * p,unitrec * q,unitrec * r) {
unitrec *	u;
    u = newunitrec();
    u->tag = tag;
    u->a = p;
    u->b = q;
    u->c = r;
    return u;
}

unitrec * createconstunit	(uint64 a,int32 t) {
unitrec *	u;
    u = newunitrec();
    u->tag = j_const;
    u->value = a;
    u->valuemode = t;
    return u;
}

unitrec * createstringconstunit	(char * s,int32 length) {
unitrec *	u;
    u = newunitrec();
    u->tag = j_const;
    u->svalue = s;
    u->valuemode = tstring;
    if (length == -1) {
        u->slength = strlen(s);
    }
    else {
        u->slength = length;
    }
    return u;
}

int32 getoptocode	(int32 opc) {
static int16	opctotable[247];
int32	n;
int32	opcto;
int32	i;
char	str[20];
    opcto = opctotable[opc];
    if (opcto) {
        return opcto;
    }
    strcpy((char *)&str,jtagnames[opc]);
    strcat((char *)&str,"to");
    for (i=0; i<=246; ++i) {
        if (strcmp(jtagnames[i],(char *)&str) == 0) {
            opctotable[opc] = i;
            return i;
        }
    }
    printf("%s\n",jtagnames[opc]);
    serror("Can't find -to version");
    return 0;
}

int32 checkpackedtype	(int32 m) {
    switch (ttbasetype[m]) {
    case ti8:
    case ti16:
    case ti32:
    case ti64:
    case tu8:
    case tu16:
    case tu32:
    case tu64:
    case tr32:
    case tr64:
    case trefpacked:
    case tstring:
    case tstruct:
    case tstringz:
    case tarray:
    case trefm:
    case tintm:
    case twordm:
        return 1;
        break;
    default:;
    }
    serror("Invalid Packed type");
    return 0;
}

void checkunpackedtype	(int32 t) {
    if (ttbasetype[t] > tvariant) {
        serror("Pack type not allowed");
    }
}

int32 checkdlltype	(int32 m) {
    switch (ttbasetype[m]) {
    case ti8:
    case ti16:
    case ti32:
    case ti64:
    case tu8:
    case tu16:
    case tu32:
    case tu64:
    case tr32:
    case tr64:
    case tintm:
    case twordm:
    case trefm:
    case trefpacked:
    case tstring:
    case tstruct:
    case tstringz:
        return 1;
        break;
    default:;
    }
    printf("%s\n",ttname[m]);
    serror("Invalid DLL param/ret type");
    return 0;
}

int32 createtype	(strec * d) {
    if (d->nameid == typeid) {
        return d->mode;
    }
    return createusertype(d);
}

int32 createusertype	(strec * stname) {
    ++ntypes;
    ttname[ntypes] = stname->name;
    ttnamedef[ntypes] = stname;
    ttbasetype[ntypes] = tvoid;
    ttusercat[ntypes] = user_cat;
    return ntypes;
}

int32 createusertypefromstr	(char * name) {
strec *	stname;
    stname = getduplnameptr(stmodule,addnamestr(name),typeid);
    adddef(stmodule,stname);
    return createusertype(stname);
}

int64 getconstvalue	(unitrec * p,int32 ID) {
    if (p && p->tag == j_const) {
        return p->value;
    }
    printf("%s %d\n","ID=",ID);
    printf("%s %d\n","ID=",ID);
    printf("%s %d\n","ID=",ID);
    printf("%s %d\n","ID=",ID);
    serror("GCV Not constant");
    return 0;
}

int32 getrangelwb	(unitrec * p) {
    if (p->tag == j_makerange) {
        return p->range_lower;
    }
    else {
        serror("getrangelwb");
    }
    return 0;
}

int32 getrangeupb	(unitrec * p) {
    if (p->tag == j_makerange) {
        return p->range_upper;
    }
    else {
        serror("getrangeupb");
    }
    return 0;
}

unitrec * getrangelwbunit	(unitrec * p) {
    if (p->tag == j_makerange) {
        return p->a;
    }
    else {
        return createunit1(j_lwb,p);
    }
}

unitrec * getrangeupbunit	(unitrec * p) {
    if (p->tag == j_makerange) {
        return p->b;
    }
    else {
        return createunit1(j_upb,p);
    }
}

int32 createarraymode	(int32 target,int32 lower,int32 length,int32 typedefx) {
int32	atype;
int32	k;
int32	m;
    if ((target==tbit) || (target==tbit2) || (target==tbit4)) {
        atype = tbits;
    }
    else {
        atype = tarray;
    }
    if (typedefx == 0) {
        for (k=tlast; k<=ntypes; ++k) {
            if (ttusercat[k] == anon_cat && ttbasetype[k] == atype && tttarget[k] == target && ttlower[k] == lower && ttlength[k] == length) {
                return k;
            }
        }
        m = createusertypefromstr(nextautotype());
    }
    else {
        m = typedefx;
    }
    ttbasetype[m] = atype;
    ttlower[m] = lower;
    ttlength[m] = length;
    tttarget[m] = target;
    return m;
}

char * nextautotype	(void) {
static char	str[32];
    sprintf((char *)&str,"$T%d",++autotypeno);
    return (char *)&str;
}

int32 createstringmode	(int32 t,int32 length,int32 typedefx) {
int32	k;
int32	m;
    if (typedefx == 0) {
        for (k=tlast; k<=ntypes; ++k) {
            if (ttusercat[k] == anon_cat && ttbasetype[k] == t && ttlength[k] == length) {
                return k;
            }
        }
        m = createusertypefromstr(nextautotype());
    }
    else {
        m = typedefx;
    }
    ttbasetype[m] = t;
    ttlower[m] = ((t == tstring)?1:0);
    ttsize[m] = length;
    ttlength[m] = length;
    return m;
}

int32 createrefpackmode	(int32 target,int32 typedefx) {
int32	k;
int32	m;
    if ((target==tvariant)) {
        return trefvar;
    } else if ((target==tbit) || (target==tbit2) || (target==tbit4)) {
        serror("CREATEREFBIT");
    }
    if (typedefx == 0) {
        for (k=tlast; k<=ntypes; ++k) {
            if (ttusercat[k] == anon_cat && ttbasetype[k] == trefpacked && tttarget[k] == target) {
                return k;
            }
        }
        m = createusertypefromstr(nextautotype());
    }
    else {
        m = typedefx;
    }
    tttarget[m] = target;
    ttbasetype[m] = trefpacked;
    return m;
}

int32 getscope	(strec * p) {
strec *	owner;
    if (p == 0) {
        return localscope;
    }
    if ((p->nameid==moduleid) || (p->nameid==programid)) {
        return exportscope;
    } else if ((p->nameid==extmoduleid) || (p->nameid==dllmoduleid)) {
        return importscope;
    }
    while (1) {
        owner = p->owner;
        if ((owner->nameid==moduleid) || (owner->nameid==extmoduleid) || (owner->nameid==dllmoduleid)) {
            goto L73;
        }
        p = owner;
    }
L73:;
    if ((owner->nameid==moduleid)) {
        return (!!p->attribs.ax_global?exportscope:localscope);
    }
    else {
        return importscope;
    }
    return 0;
}

void setnameptr	(unitrec * p) {
    p->def->code = p;
}

void printcode_all	(void * f,char * caption) {
int32	i;
strec *	p;
    for (i=1; i<=nmodules; ++i) {
        printcode(f,caption,i);
    }
}

void printcode	(void * f,char * caption,int32 n) {
int32	i;
strec *	p;
    p = moduletable[n].stmodule->deflist;
    fprintf(f,"%s %s %s\n",caption,"MODULE:",moduletable[n].name);
    while (p) {
        if ((p->nameid==procid)) {
            if (getscope(p) != importscope) {
                fprintf(f,"%s%s %s\n",p->name,"=",(!!p->attribs.ax_global?"Global":"Local"));
                fprintf(f,"%s %s %d\n","<<<",p->name,fdebug);
                printunit(f,p->code,0,"1");
                fprintf(f,"%s\n",">>>");
                fprintf(f,"\n");
            }
        }
        p = p->nextdef;
    }
}

void printunit	(void * dev,unitrec * p,int32 level,char * prefix) {
unitrec *	q;
strec *	d;
int32	t;
char *	idname;
    if (p == 0) {
        return;
    }
    if (!!p->lineno) {
        currlineno = p->lineno;
    }
    fprintf(dev,"%p %s",(void*)(p),": ");
    fprintf(dev,"%s",getprefix(level,prefix,p));
    idname = jtagnames[p->tag];
    fprintf(dev,"%s%s",idname,": ");
    if (fdebug) {
        printf("%s %s\n","IDNAME=",idname);
    }
    if ((p->tag==j_name)) {
        d = p->def;
        fprintf(dev,"%s %s",d->name,namenames[d->nameid]);
        if (!!d->code) {
            fprintf(dev,"%s%s%s"," {",jtagnames[d->code->tag],"}");
        }
        fprintf(dev,"%s%s"," ",getdottedname(d));
        fprintf(dev,"%s",(!!p->dottedname?" {Dotted}":""));
        if (!!p->c) {
            fprintf(dev,"%s %p"," Lastcall:",(void*)(p->c));
        }
    } else if ((p->tag==j_labeldef)) {
        fprintf(dev,"%s\n",p->def->name);
    } else if ((p->tag==j_const)) {
        t = p->valuemode;
        if ((t==tstring)) {
            if (p->slength > 256) {
                fprintf(dev,"%s%s %s%d","\"","(LONGSTR)","\" *",p->slength);
            }
            else {
                fprintf(dev,"%s%s%s%d","\"",p->svalue,"\" *",p->slength);
            }
        } else if ((t==tint)) {
            fprintf(dev,"%lld",p->value);
        } else if ((t==tword)) {
            fprintf(dev,"%llu",p->uvalue);
        } else if ((t==treal)) {
            fprintf(dev,"%f",p->xvalue);
        } else if ((t==trange)) {
            fprintf(dev,"%d%s%d",p->range_lower,"..",p->range_upper);
        }
        else {
            printf("%s\n",ttname[t]);
            serror("PRINTUNIT BAD CONST");
        }
        fprintf(dev,"%s%s"," ",stdtypenames[t]);
    } else if ((p->tag==j_longint)) {
        fprintf(dev,"%s %s %d",p->svalue,"Len:",p->slength);
    } else if ((p->tag==j_callhostfn) || (p->tag==j_callhostproc)) {
        fprintf(dev,"%s",hostfnnames[p->opcode]);
    } else if ((p->tag==j_typeconst)) {
        fprintf(dev,"%s",ttname[p->valuemode]);
    } else if ((p->tag==j_operator)) {
        fprintf(dev,"%s",jtagnames[p->opcode]+2);
    } else if ((p->tag==j_convert)) {
        fprintf(dev,"%s",ttname[p->valuemode]);
    }
    fprintf(dev,"\n");
    printunitlist(dev,p->a,level+1,"1");
    printunitlist(dev,p->b,level+1,"2");
    printunitlist(dev,p->c,level+1,"3");
}

static void printunitlist	(void * dev,unitrec * p,int32 level,char * prefix) {
    if (p == 0) {
        return;
    }
    while (p) {
        printunit(dev,p,level,prefix);
        p = p->nextunit;
    }
}

static char * getprefix	(int32 level,char * prefix,unitrec * p) {
static char	str[512];
char	indentstr[512];
int32	av_2;
    indentstr[1-1] = 0;
    if (level > 10) {
        level = 10;
    }
    av_2 = level;
    while (av_2--) {
        strcat((char *)&indentstr,"- - ");
    }
    strcpy((char *)&str,getlineinfok());
    strcat((char *)&str,(char *)&indentstr);
    strcat((char *)&str,prefix);
    if (!!(uchar)*prefix) {
        strcat((char *)&str," ");
    }
    return (char *)&str;
}

char * getdottedname	(strec * p) {
static char	str[256];
char	str2[256];
strec *	owner;
    strcpy((char *)&str,p->name);
    owner = p->owner;
    while (owner && owner->nameid != programid) {
        strcpy((char *)&str2,(char *)&str);
        strcpy((char *)&str,owner->name);
        strcat((char *)&str,".");
        strcat((char *)&str,(char *)&str2);
        owner = owner->owner;
    }
    return (char *)&str;
}

static char * getlineinfok	(void) {
static char	str[40];
    sprintf((char *)&str,"%04d ",currlineno);
    return (char *)&str;
}

strec * getavname	(strec * owner,int32 id) {
strec *	p;
char	str[32];
char *	name;
    if (id == frameid && owner->nameid != procid) {
        serror("Auto frame var not in proc");
    }
    if (id == frameid) {
        sprintf((char *)&str,"av$%d",++nextavindex);
    }
    else {
        sprintf((char *)&str,"sv$%d",++nextsvindex);
    }
    name = pcm_copyheapstring((char *)&str);
    addnamestr(name);
    p = getduplnameptr(owner,addnamestr(name),id);
    p->mode = tint;
    p->attribs.ax_autovar = 1;
    adddef(owner,p);
    return p;
}

void unionstr_clear	(uflagsrec * u) {
    u->ulength = 0;
}

void unionstr_append	(uflagsrec * u,int32 c) {
    if (u->ulength == 7) {
        serror("Uflags overflow/a");
    }
    ++u->ulength;
    (u)->codes[u->ulength-1] = c;
}

void unionstr_concat	(uflagsrec * u,uflagsrec * v) {
int32	ulen;
int32	vlen;
int32	i;
    ulen = u->ulength;
    vlen = v->ulength;
    if (ulen+vlen > 7) {
        serror("Uflags overflow/c");
    }
    for (i=1; i<=vlen; ++i) {
        (u)->codes[i+ulen-1] = (v)->codes[i-1];
    }
    u->ulength = (ulen+vlen);
}

int32 unionstr_last	(uflagsrec * u) {
    if (!!u->ulength) {
        return (u)->codes[u->ulength-1];
    }
    return 0;
}

void unionstr_copy	(uflagsrec * u,uflagsrec * v) {
    memcpy((int32 *)u,(int32 *)v,8);
}

void unionstr_print	(uflagsrec * u) {
    printstrn((char *)&(u)->codes,u->ulength);
}

int32 createrecordmode	(strec * owner,int32 t,int32 typedefx) {
int32	m;
    if (typedefx == 0) {
        m = createusertype(owner);
    }
    else {
        m = typedefx;
    }
    ttbasetype[m] = t;
    return m;
}

int32 createenummode	(strec * owner,int32 typedefx) {
int32	m;
    if (typedefx == 0) {
        m = createusertype(owner);
    }
    else {
        m = typedefx;
    }
    ttbasetype[m] = tenum;
    return m;
}

void convertstring	(char * s,char * t) {
int32	c;
    while (c = (uchar)*s++) {
        switch (c) {
        case '"':
            *t++ = '\\';
            *t++ = '"';
            break;
        case 10:
            *t++ = '\\';
            *t++ = 'n';
            break;
        case 13:
            *t++ = '\\';
            *t++ = 'c';
            break;
        case 9:
            *t++ = '\\';
            *t++ = 't';
            break;
        case '\\':
            *t++ = '\\';
            *t++ = '\\';
            break;
        case 7:
        case 8:
        case 26:
        case 27:
            *t++ = '<';
            *t++ = (c/ 10+'0');
            *t++ = (c% 10+'0');
            *t++ = '>';
            break;
        default:;
            *t++ = c;
        }
    }
    *t = 0;
}

strbuffer * strexpr	(unitrec * p) {
    gs_init(exprstr);
    jeval(exprstr,p);
    return exprstr;
}

static void jeval	(strbuffer * dest,unitrec * p) {
unitrec *	q;
char	str[500];
    if ((p->tag==j_const)) {
        if ((p->valuemode==tstring)) {
            if (p->slength > 250) {
                strcpy((char *)&str,"LONGSTR)");
            }
            else {
                convertstring(p->svalue,(char *)&str);
            }
            gs_additem(dest,"\"");
            gs_additem(dest,(char *)&str);
            gs_additem(dest,"\"");
            return;
        } else if ((p->valuemode==tint)) {
            sprintf((char *)&str,"%lld",p->value);
        } else if ((p->valuemode==tword)) {
            sprintf((char *)&str,"%llu",p->uvalue);
        } else if ((p->valuemode==treal)) {
            sprintf((char *)&str,"%f",p->xvalue);
        } else if ((p->valuemode==trange)) {
            sprintf((char *)&str,"%d..%d",p->range_lower,p->range_upper);
        }
        else {
            printf("%s\n",ttname[p->valuemode]);
            nxerror("EVAL/CONST",p);
        }
        gs_additem(dest,(char *)&str);
    } else if ((p->tag==j_name)) {
        gs_additem(dest,p->def->name);
    } else if ((p->tag==j_andl) || (p->tag==j_orl) || (p->tag==j_andand) || (p->tag==j_eq) || (p->tag==j_ne) || (p->tag==j_lt) || (p->tag==j_le) || (p->tag==j_gt) || (p->tag==j_ge) || (p->tag==j_add) || (p->tag==j_sub) || (p->tag==j_mul) || (p->tag==j_div) || (p->tag==j_idiv) || (p->tag==j_fdiv) || (p->tag==j_ddiv) || (p->tag==j_rem) || (p->tag==j_iand) || (p->tag==j_ior) || (p->tag==j_ixor) || (p->tag==j_shl) || (p->tag==j_shr) || (p->tag==j_in) || (p->tag==j_notin) || (p->tag==j_inrev) || (p->tag==j_min) || (p->tag==j_max) || (p->tag==j_addptr) || (p->tag==j_subptr) || (p->tag==j_concat) || (p->tag==j_atan2) || (p->tag==j_power) || (p->tag==j_xorl) || (p->tag==j_isequal) || (p->tag==j_divrem) || (p->tag==j_append)) {
        strcpy((char *)&str,getopcjname(p->tag));
        gs_additem(dest,"(");
        jeval(dest,p->a);
        gs_additem(dest,(char *)&str);
        jeval(dest,p->b);
        gs_additem(dest,")");
    } else if ((p->tag==j_neg) || (p->tag==j_abs) || (p->tag==j_inot) || (p->tag==j_chr) || (p->tag==j_asc) || (p->tag==j_sqrt) || (p->tag==j_sqr) || (p->tag==j_cube) || (p->tag==j_sign) || (p->tag==j_sin) || (p->tag==j_cos) || (p->tag==j_tan) || (p->tag==j_asin) || (p->tag==j_acos) || (p->tag==j_atan) || (p->tag==j_ln) || (p->tag==j_lg) || (p->tag==j_log) || (p->tag==j_exp) || (p->tag==j_round) || (p->tag==j_floor) || (p->tag==j_ceil) || (p->tag==j_fract) || (p->tag==j_fmod) || (p->tag==j_lwb) || (p->tag==j_upb) || (p->tag==j_len) || (p->tag==j_bounds) || (p->tag==j_bitwidth) || (p->tag==j_bytesize) || (p->tag==j_gettype) || (p->tag==j_getbasetype) || (p->tag==j_getelemtype) || (p->tag==j_isvoid) || (p->tag==j_isdef) || (p->tag==j_isint) || (p->tag==j_isreal) || (p->tag==j_isstring) || (p->tag==j_isrange) || (p->tag==j_islist) || (p->tag==j_isrecord) || (p->tag==j_isarray) || (p->tag==j_isset) || (p->tag==j_ispointer) || (p->tag==j_minvalue) || (p->tag==j_maxvalue) || (p->tag==j_min1) || (p->tag==j_max1) || (p->tag==j_notl) || (p->tag==j_istruel) || (p->tag==j_isnone) || (p->tag==j_ismutable)) {
        strcpy((char *)&str,getopcjname(p->tag));
        gs_additem(dest,(char *)&str);
        gs_additem(dest,"(");
        jeval(dest,p->a);
        gs_additem(dest,")");
    } else if ((p->tag==j_callfn) || (p->tag==j_callproc)) {
        jeval(dest,p->a);
        gs_additem(dest,"(");
        q = p->b;
        while (q) {
            jeval(dest,q);
            q = q->nextunit;
            if (q) {
                gs_additem(dest,",");
            }
        }
        gs_additem(dest,")");
    } else if ((p->tag==j_callhostfn)) {
        gs_additem(dest,"Host<");
        gs_additem(dest,hostfnnames[p->opcode]+5);
        gs_additem(dest,">(");
        q = p->b;
        while (q) {
            jeval(dest,q);
            q = q->nextunit;
            if (q) {
                gs_additem(dest,",");
            }
        }
        gs_additem(dest,")");
    } else if ((p->tag==j_index) || (p->tag==j_dotindex) || (p->tag==j_slice) || (p->tag==j_dotslice)) {
        jeval(dest,p->a);
        if (p->tag == j_dotindex || p->tag == j_dotslice) {
            gs_additem(dest,".");
        }
        gs_additem(dest,"[");
        jeval(dest,p->b);
        gs_additem(dest,"]");
    } else if ((p->tag==j_keyindex) || (p->tag==j_dotkeyindex)) {
        jeval(dest,p->a);
        if (p->tag == j_dotkeyindex) {
            gs_additem(dest,".");
        }
        gs_additem(dest,"{");
        jeval(dest,p->b);
        gs_additem(dest,"}");
    } else if ((p->tag==j_dot)) {
        jeval(dest,p->a);
        gs_additem(dest,".");
        jeval(dest,p->b);
    } else if ((p->tag==j_makelist) || (p->tag==j_makesetlist) || (p->tag==j_makeconstr) || (p->tag==j_makedict)) {
        gs_additem(dest,((p->tag == j_makelist || p->tag == j_makeconstr)?"(":"["));
        q = p->a;
        while (q) {
            jeval(dest,q);
            q = q->nextunit;
            if (q) {
                gs_additem(dest,",");
            }
        }
        gs_additem(dest,((p->tag == j_makelist)?")":"]"));
    } else if ((p->tag==j_makerange)) {
        gs_additem(dest,"(");
        jeval(dest,p->a);
        gs_additem(dest,"..");
        jeval(dest,p->b);
        gs_additem(dest,")");
    } else if ((p->tag==j_assignx)) {
        jeval(dest,p->a);
        gs_additem(dest,":=");
        jeval(dest,p->b);
    } else if ((p->tag==j_ifx)) {
        gs_additem(dest,"(");
        jeval(dest,p->a);
        gs_additem(dest,"|");
        jeval(dest,p->b);
        gs_additem(dest,"|");
        jeval(dest,p->c);
        gs_additem(dest,")");
    } else if ((p->tag==j_typeconst)) {
        gs_additem(dest,strmode(p->valuemode,1));
    } else if ((p->tag==j_classconst)) {
        gs_additem(dest,p->def->name);
        gs_additem(dest,">");
    } else if ((p->tag==j_convert)) {
        gs_additem(dest,"(");
        jeval(dest,p->a);
        gs_additem(dest,")");
    } else if ((p->tag==j_keyvalue)) {
        jeval(dest,p->a);
        gs_additem(dest,":");
        jeval(dest,p->b);
    } else if ((p->tag==j_longint)) {
        gs_additem(dest,(char *)p->a);
        gs_additem(dest,"L");
    } else if ((p->tag==j_ptr)) {
        jeval(dest,p->a);
        gs_additem(dest,"^");
    } else if ((p->tag==j_ptrto)) {
        gs_additem(dest,"^");
        jeval(dest,p->a);
    } else if ((p->tag==j_clamp)) {
        gs_additem(dest,"(");
        jeval(dest,p->a);
        gs_additem(dest,",");
        jeval(dest,p->b);
        gs_additem(dest,",");
        jeval(dest,p->c);
        gs_additem(dest,")");
    } else if ((p->tag==j_block)) {
        gs_additem(dest,"<JBLOCK>");
    }
    else {
        printf("%s\n",jtagnames[p->tag]);
        gerror("CAN'T DO JEVAL",p);
    }
}

char * getopcjname	(int32 opc) {
int32	i;
char	str[20];
    for (i=1; i<=35; ++i) {
        if (opc == opc_codes[i-1]) {
            return opc_names[i-1];
        }
    }
    return jtagnames[opc]+2;
}

char * strmode	(int32 m,int32 expand) {
static char	str[4096];
    istrmode(m,expand,(char *)&str);
    return (char *)&str;
}

void istrmode	(int32 m,int32 expand,char * dest) {
strec *	d;
strec *	q;
int32	value;
int32	needcomma;
int32	x;
int32	i;
    if (m < tlast) {
        strcpy(dest,ttname[m]);
        return;
    }
    if ((ttbasetype[m]==trefvar) || (ttbasetype[m]==trefpacked)) {
        strcpy(dest,"ref ");
        if (ttbasetype[tttarget[m]] == tstruct) {
            strcat(dest,ttname[tttarget[m]]);
        }
        else {
            istrmode(tttarget[m],0,dest+strlen(dest));
        }
    } else if ((ttbasetype[m]==tstring)) {
        strcpy(dest,"string*");
        sprintf(dest+strlen(dest),"%d",ttlength[m]);
    } else if ((ttbasetype[m]==tstringz)) {
        strcpy(dest,"stringz*");
        sprintf(dest+strlen(dest),"%d",ttlength[m]);
    } else if ((ttbasetype[m]==tset)) {
        strcpy(dest,"set*");
        sprintf(dest+strlen(dest),"%d",ttlength[m]);
    } else if ((ttbasetype[m]==tarray)) {
        if (!!ttlength[m]) {
            sprintf(dest,"[%d..%d]",ttlower[m],(ttlength[m]-ttlower[m])-1);
        }
        else {
            sprintf(dest,"[%d:]",ttlower[m]);
        }
        istrmode(tttarget[m],0,dest+strlen(dest));
    } else if ((ttbasetype[m]==tenum)) {
        strcpy(dest,"enum(");
        d = ttnamedef[m];
        value = 1;
        needcomma = 0;
        q = d->deflist;
        while (q) {
            if (needcomma) {
                strcat(dest,",");
            }
            needcomma = 1;
            strcat(dest,q->name);
            x = q->index;
            if (x != value) {
                value = x;
                sprintf(dest+strlen(dest),"%d",value);
            }
            ++value;
            q = q->nextdef;
        }
        strcat(dest,")");
    } else if ((ttbasetype[m]==trecord) || (ttbasetype[m]==tstruct)) {
        if (!expand) {
            strcpy(dest,ttname[m]);
            return;
        }
        strcat(dest,ttname[ttbasetype[m]]);
        strcat(dest,"(");
        d = ttnamedef[m];
        needcomma = 0;
        q = d->deflist;
        while (q) {
            if (needcomma) {
                strcat(dest,",");
            }
            needcomma = 1;
            istrmode(q->mode,0,dest+strlen(dest));
            strcat(dest," ");
            strcat(dest,q->name);
            q = q->nextdef;
        }
        strcat(dest,")");
    } else if ((ttbasetype[m]==tvoid)) {
        strcpy(dest,ttname[m]);
    } else if ((ttbasetype[m]==tuser)) {
        strcpy(dest,ttname[m]);
    }
    else {
        serror("NEWSTRMODE");
    }
}

int32 countunits	(unitrec * p) {
int32	n;
    n = 0;
    while (p) {
        ++n;
        p = p->nextunit;
    }
    return n;
}

strec * finddefstr	(strec * owner,char * name) {
strec *	d;
    d = owner->deflist;
    while (d) {
        if (strcmp(d->name,name) == 0) {
            return d;
        }
        d = d->nextdef;
    }
    return 0;
}

static void purgesymbol	(strec * p,strec * prev,int32 del) {
strec *	q;
    if ((p->nameid==fieldid)) {
        return;
    }
    purgesymbollist(p->deflist,0,del);
    if (prev) {
        prev->nextdef = p->nextdef;
    }
    else {
        p->owner->deflist = p->nextdef;
    }
    q = p->prevdupl;
    q->nextdupl = p->nextdupl;
    if (del) {
        pcm_free(p,84);
    }
}

void purgesymbollist	(strec * p,int32 ismodule,int32 del) {
strec *	q;
strec *	prev;
    prev = 0;
    while (p) {
        q = p->nextdef;
        if (ismodule == 0 || !p->attribs.ax_global) {
            purgesymbol(p,prev,del);
        }
        else {
            prev = p;
        }
        p = q;
    }
}

void purgeprocs	(strec * p,int32 del) {
    while (p) {
        if (p->nameid == procid) {
            purgeproc(p,del);
        }
        p = p->nextdef;
    }
}

void purgeproc	(strec * p,int32 del) {
strec *	q;
strec *	prev;
strec *	r;
    q = p->deflist;
    prev = 0;
    while (q) {
        r = q->nextdef;
        if (q->nameid == frameid) {
            purgesymbol(q,prev,del);
        }
        else {
            prev = q;
        }
        q = r;
    }
}

void printmodelist	(void * f) {
enum {wtypeno = 4};
enum {wname = 13};
enum {wbasetype = 13};
enum {wbitsize = 3};
enum {wtarget = 12};
enum {wnamedef = 4};
enum {wlower = 3};
enum {wupper = 3};
enum {wlength = 4};
enum {wsize = 5};
enum {wusercat = 4};
enum {wused = 4};
enum {wmode = 32};
char	str[256];
char *	mstr;
strbuffer	destv;
strbuffer *	dest=&destv;
int32	m;
    gs_init(dest);
    gs_leftstr(dest,"#",wtypeno,32);
    gs_leftstr(dest,"Name",wname,32);
    gs_leftstr(dest,"Base",wbasetype,32);
    gs_leftstr(dest,"Bit",wbitsize,32);
    gs_leftstr(dest,"Target",wtarget,32);
    gs_leftstr(dest,"Def",wnamedef,32);
    gs_leftstr(dest,"Lwb",wlower,32);
    gs_leftstr(dest,"Upb",wupper,32);
    gs_leftstr(dest,"Len",wlength,32);
    gs_leftstr(dest,"Size",wsize,32);
    gs_leftstr(dest,"Cat",wusercat,32);
    gs_leftstr(dest,"Used",wused,32);
    gs_leftstr(dest,"Mode",wmode,32);
    gs_println(dest,f);
    for (m=0; m<=ntypes; ++m) {
        gs_init(dest);
        gs_leftint(dest,m,wtypeno,32);
        gs_leftstr(dest,ttname[m],wname,32);
        gs_leftstr(dest,ttname[ttbasetype[m]],wbasetype,32);
        gs_leftint(dest,ttbitwidth[m],wbitsize,32);
        if (!!tttarget[m]) {
            gs_leftstr(dest,ttname[tttarget[m]],wtarget,32);
        }
        else {
            gs_leftstr(dest,"-",wtarget,32);
        }
        if (!!ttnamedef[m]) {
            gs_leftstr(dest,"+",wnamedef,32);
        }
        else {
            gs_leftstr(dest,"-",wnamedef,32);
        }
        if ((ttbasetype[m]==tstring) || (ttbasetype[m]==tset) || (ttbasetype[m]==tarray) || (ttbasetype[m]==trecord) || (ttbasetype[m]==tstruct) || (ttbasetype[m]==tenum)) {
            gs_leftint(dest,ttlower[m],wlower,32);
            gs_leftint(dest,ttlower[m]+ttlength[m]-1,wlower,32);
            gs_leftint(dest,ttlength[m],wlength,32);
        }
        else {
            gs_leftstr(dest,"",wlower,32);
            gs_leftstr(dest,"",wlower,32);
            gs_leftstr(dest,"",wlength,32);
        }
        gs_leftint(dest,ttsize[m],wsize,32);
        gs_leftint(dest,ttusercat[m],wusercat,32);
        mstr = strmode(m,1);
        if (strlen(mstr) < 16) {
            gs_str(dest,mstr);
        }
        else {
            gs_println(dest,f);
            gs_init(dest);
            gs_str(dest,mstr);
        }
        gs_println(dest,f);
    }
    fprintf(f,"\n");
}

void printgenfieldtable	(void * f,char * caption) {
int32	i;
    fprintf(f,"%s %d\n",caption,ngenfieldnames);
    for (i=1; i<=ngenfieldnames; ++i) {
        fprintf(f,"%d %s %d %d\n",i,genfieldnames[i-1].def->name,genfieldnames[i-1].dataindex,genfieldnames[i-1].datalength);
    }
    fprintf(f,"\n");
    fprintf(f,"%s %d\n","Genfielddata:",ngenfielddata);
    for (i=1; i<=ngenfielddata; ++i) {
        fprintf(f,"%d %s %s %d\n",genfielddata[i-1].fieldindex,ttname[genfielddata[i-1].recordtype],ttname[genfielddata[i-1].fieldtype],genfielddata[i-1].offset);
    }
    fprintf(f,"\n");
}

void addtoproclist	(strec * d) {
procrec *	pp;
    ++nproclist;
    pp = (procrec *)pcm_alloc(8);
    pp->nextproc = proclist;
    proclist = pp;
    pp->def = d;
}


// From module: qc_parselib
strec * px_typecheck	(strec * owner,strec * stname,int32 add) {
strec *	d;
int32	m;
    d = resolvetopname(owner,stname,0);
    if (d) {
        return d;
    }
    if (!add) {
        return 0;
    }
    d = getduplnameptr(stmodule,stname,typeid);
    adddef(stmodule,d);
    m = createusertype(d);
    ttbasetype[m] = tvoid;
    d->mode = m;
    return d;
}

strec * resolvetopname	(strec * owner,strec * stnewname,int32 fmodule) {
strec *	powner;
strec *	def;
strec *	p;
int32	score;
int32	s;
    score = 0;
    def = 0;
    p = stnewname->nextdupl;
    while (p) {
        powner = p->owner;
        switch (powner->nameid) {
        case procid:
            if (powner == owner) {
                return p;
            }
            break;
        case moduleid:
            if (score < 3) {
                def = p;
                score = 1;
            }
            break;
        case extmoduleid:
            if (!!(currmodule)->importmap[powner->attribs.ax_moduleno-1]) {
                s = ((powner == stsysmodule)?2:1);
                if (s > score && !!p->attribs.ax_global) {
                    def = p;
                    score = s;
                }
            }
            break;
        case dllmoduleid:
            if (score < 2) {
                def = p;
                score = s;
            }
            break;
        case typeid:
            if (typeallowed && score < 2) {
                def = p;
                score = 2;
            }
            break;
        default:;
            if (fmodule) {
                if ((p->nameid==extmoduleid) || (p->nameid==dllmoduleid) || (p->nameid==moduleid)) {
                    if (score <= 3) {
                        def = p;
                        score = 4;
                    }
                }
            }
        }
        p = p->nextdupl;
    }
    return def;
}

void px_name	(strec * stmod,strec * stproc,unitrec * p,int32 fmodule) {
strec *	d;
strec *	owner;
strec *	pdef;
unitrec *	q;
char *	name;
    ++ALLNAMES;
    owner = ((stproc == 0)?stmod:stproc);
    pdef = p->def;
    d = resolvetopname(owner,pdef,fmodule);
    if (d) {
        p->def = d;
        ++ALLFOUNDNAMES;
        if ((d->nameid==constid)) {
            q = d->code;
            p->tag = j_const;
            p->value = q->value;
            p->valuemode = d->mode;
            p->slength = q->slength;
        } else if ((d->nameid==enumid)) {
            nxerror("FOUND ENUMID",p);
        } else if ((d->nameid==staticid)) {
            if (d->owner == stmodule && stproc) {
                name = d->name;
            }
        } else if ((d->nameid==typeid)) {
            p->tag = j_typeconst;
            p->valuemode = p->def->mode;
        } else if ((d->nameid==aliasid)) {
            nxerror("FOUND ALIAS",p);
        }
    }
    else {
        ++ALLNOTFOUNDNAMES;
        if (owner->nameid == procid) {
            p->def = getduplnameptr(owner,p->def,frameid);
            adddef(owner,p->def);
            p->def->mode = tvariant;
        }
        else {
            printf("%s %d %s\n",pdef->name,p->lineno,jtagnames[p->tag]);
            nxerror("Undefined",p);
        }
    }
}

void evalbinop	(unitrec * p,unitrec * a,unitrec * b) {
int64	x;
int64	y;
int64	z;
int32	xt;
int32	yt;
    xt = a->valuemode;
    yt = b->valuemode;
    if (xt == yt && yt == treal) {
        evalbinop_real(p,a,b);
        return;
    }
    if (!(xt == yt && yt == tint)) {
        return;
    }
    x = a->value;
    y = b->value;
    switch (p->tag) {
    case j_add:
        z = x+y;
        break;
    case j_sub:
        z = x-y;
        break;
    case j_mul:
        z = x*y;
        break;
    case j_idiv:
        z = x/ y;
        break;
    case j_makerange:
        z = y<<32|x&4294967295ll;
        makenewconst(p,z,trange);
        return;
        break;
    default:;
        return;
    }
    makenewconst(p,z,tint);
}

static void evalbinop_real	(unitrec * p,unitrec * a,unitrec * b) {
double	x;
double	y;
double	z;
    x = a->xvalue;
    y = b->xvalue;
    switch (p->tag) {
    case j_add:
        z = x+y;
        break;
    case j_sub:
        z = x-y;
        break;
    case j_mul:
        z = x*y;
        break;
    case j_div:
        z = x/ y;
        break;
    default:;
        return;
    }
    makenewconst(p,*(int64*)&z,treal);
}

static void makenewconst	(unitrec * p,int64 value,int32 t) {
int32	a;
int32	b;
    p->tag = j_const;
    p->value = value;
    p->valuemode = t;
    p->a = 0;
    p->b = 0;
}

strec * finddupl	(strec * owner,strec * lst) {
strec *	d;
    d = lst;
    while (d) {
        if (d->owner == owner || owner == 0) {
            if (d->nameid == aliasid || d->nameid == linkid) {
                return d->equiv;
            }
            return d;
        }
        d = d->nextdupl;
    }
    return 0;
}

void px_dot	(strec * stmod,strec * stproc,unitrec * p) {
strec *	qdef;
strec *	rdef;
strec *	d;
strec *	newd;
strec *	e;
strec *	fielddef;
strec *	owner;
unitrec *	q;
unitrec *	r;
int32	nfields;
    owner = ((stproc == 0)?stmod:stproc);
    q = p->a;
    r = p->b;
    rdef = r->def;
    if ((q->tag==j_name)) {
        d = q->def;
    } else if ((q->tag==j_typeconst)) {
        d = q->def;
        goto L16;
    }
    else {
        rdef = r->def;
        goto L17;
    }
    switch (d->nameid) {
    case dllmoduleid:
    case moduleid:
    case extmoduleid:
    case typeid:
    case procid:
    case dllprocid:
dotype:
L16:;
        newd = finddupl(d,rdef->nextdupl);
        if (newd) {
            switch (newd->nameid) {
            case enumid:
                p->tag = j_const;
                p->value = newd->index;
                p->valuemode = tint;
                break;
            case constid:
                q = newd->code;
                if ((q->tag==j_const)) {
                    p->tag = j_const;
                    p->value = q->value;
                    p->valuemode = newd->mode;
                    p->a = p->b = 0;
                }
                else {
                    nxerror("Rxdot:const?",p);
                }
                break;
            case typeid:
                p->tag = j_typeconst;
                p->valuemode = newd->mode;
                p->def = newd;
                break;
            case staticid:
                p->tag = j_name;
                p->def = newd;
                break;
            case procid:
            case dllprocid:
                p->tag = j_name;
                p->def = newd;
                p->a = p->b = 0;
                p->dottedname = 1;
                break;
            case linkid:
                do {
                    newd = newd->equiv;
                } while (!(newd->nameid != linkid));
                p->tag = j_name;
                p->def = newd;
                break;
            default:;
                printf("%s %s %s\n",namenames[newd->nameid],".",newd->name);
                nxerror("Rxdot:.name not allowed here",p);
            }
        }
        else {
            printf("%s%s%s\n",d->name,".",rdef->name);
            nxerror("Can't resolve",p);
        }
        break;
    case frameid:
    case staticid:
    case paramid:
    case fieldid:
    case genfieldid:
doexprdot:
L17:;
        nfields = 0;
        fielddef = 0;
        e = rdef->nextdupl;
        while (e) {
            if ((e->nameid==fieldid) || (e->nameid==constid) || (e->nameid==procid) || (e->nameid==typeid) || (e->nameid==staticid) || (e->nameid==dllprocid) || (e->nameid==linkid)) {
                ++nfields;
                fielddef = e;
            }
            e = e->nextdupl;
        }
        if ((nfields==0)) {
            printf("%s\n",rdef->name);
            nxerror("Can't find field",p);
        }
        else {
            if (rdef->nameid != genfieldid) {
                rdef->nameid = genfieldid;
                genfieldnames[++ngenfieldnames-1].def = rdef;
                rdef->offset = ngenfieldnames;
            }
        }
        break;
    default:;
        printf("%s\n",namenames[d->nameid]);
        nxerror("RXDOT:Unknown nameid",p);
    }
}

void evalmonop	(unitrec * p) {
int32	xt;
int64	x;
int64	z;
    if ((p->a->tag==j_const)) {
    } else if ((p->a->tag==j_typeconst)) {
        if ((p->tag==j_bytesize)) {
            printf("%s\n","EVALMONOP TYPE.BYTES");
            return;
        } else if ((p->tag==j_len)) {
            return;
        }
    }
    else {
        return;
    }
    x = p->a->value;
    xt = p->a->valuemode;
    if (!(xt == tint)) {
        return;
    }
    switch (p->tag) {
    case j_neg:
        z = -x;
        break;
    default:;
        return;
    }
    makenewconst(p,z,tint);
}

int32 checkdict	(unitrec * p) {
int32	nkeywords;
int32	isconst;
unitrec *	q;
int32	n;
    if (p->tag != j_makesetlist) {
        return 0;
    }
    n = nkeywords = 0;
    isconst = 1;
    q = p->a;
    while (q) {
        ++n;
        if ((q->tag==j_keyvalue)) {
            ++nkeywords;
        }
        q = q->nextunit;
    }
    if (nkeywords == 0) {
        return 0;
    }
    if (nkeywords != n) {
        nxerror("Dict: not all key:values",0);
    }
    p->tag = j_makedict;
    return 1;
}

void checkconstlist	(unitrec * p) {
unitrec *	q;
strec *	stname;
int32	n;
    q = p->a;
    n = 0;
    while (q) {
        ++n;
        if ((q->tag==j_const)) {
        } else if ((q->tag==j_name)) {
            if (!q->def->attribs.ax_autovar) {
                return;
            }
        }
        else {
            return;
        }
        q = q->nextunit;
    }
    if (n == 0) {
        return;
    }
    stname = getavname(stmodule,staticid);
    stname->mode = tvariant;
    stname->code = createunit2(p->tag,p->a,p->b);
    stname->attribs.ax_equals = 1;
    p->tag = j_name;
    p->def = stname;
}


// From module: qc_parse
int32 parsemodule	(int32 n) {
modulerec	m;
strec *	p;
strec *	owner;
int32	globalflag;
int32	status;
    initparser();
    m = moduletable[n];
    stmodule = moduletable[n].stmodule;
    startlex("PARSEMODULE",m.sourcecode);
    owner = stmodule;
    lex();
    status = readmoduledefs(owner);
    if (!status) {
        return 0;
    }
    p = stmodule->deflist;
    while (p) {
        if (p->nameid == procid && !!p->attribs.ax_forward) {
            printf("%s %s\n",p->name,namenames[p->nameid]);
            serror("Proc not defined");
        }
        p = p->nextdef;
    }
    return status;
}

int32 readmoduledefs	(strec * owner) {
strec *	p;
int32	globalflag;
int32	i;
int32	found;
    globalflag = 0;
    while (1) {
        switch (lx.symbol) {
        case kglobalsym:
            if (globalflag) {
                serror("global global?");
            }
            globalflag = 1;
            lex();
            break;
        case kprocsym:
        case kfunctionsym:
        case kmethodsym:
            readprocdef(owner,globalflag,0);
            ++NPROCS;
            globalflag = 0;
            break;
        case kvarsym:
            readvardef(owner,globalflag,0,staticid);
            globalflag = 0;
            break;
        case kimportmodulesym:
            readimportmodule(owner);
            break;
        case ktypesym:
            readtypedef(owner,globalflag);
            globalflag = 0;
            break;
        case kconstsym:
            readconstdef(owner,globalflag);
            globalflag = 0;
            break;
        case kclasssym:
        case krecordsym:
            readclassdef(owner,globalflag);
            globalflag = 0;
            break;
        case kenumsym:
            lex();
            readenumtype(owner,0,0);
            break;
        case ktabledatasym:
            readtabledef(owner,globalflag);
            globalflag = 0;
            break;
        case docstringsym:
            serror("DOCSTRING");
            break;
        case kimportsym:
            if (globalflag) {
                serror("glob/import?");
            }
            lex();
            if (lx.symbol == opsym && lx.subcode == j_mul) {
                lex();
            }
            checksymbol(namesym);
            found = 0;
            for (i=1; i<=nmodules; ++i) {
                if (strcmp(lx.symptr->name,moduletable[i].name) == 0) {
                    found = 1;
                    goto L9;
                }
            }
L9:;
            if (!found) {
                printf("%s\n",lx.symptr->name);
                serror("Import stmt out of position?");
            }
            lex();
            break;
        case kcondcompsym:
            if ((lx.subcode==windowsff)) {
                if (!!os_iswindows()) {
                    lex();
                }
                else {
skiptoeol:
L11:;
                    do {
                        lex();
                    } while (!(lx.symbol == semisym || lx.symbol == eofsym));
                }
            } else if ((lx.subcode==linuxff)) {
                if (!os_iswindows()) {
                    lex();
                }
                else {
                    goto L11;
                }
            }
            else {
                serror("condcomp");
            }
            break;
        case semisym:
            lex();
            break;
        case eofsym:
            goto L5;
            break;
        default:;
            PS1("symbol");
            serror("Not allowed at module level");
        }
    }
L5:;
    return 1;
}

static void initparser	(void) {
char *	tabledataname="";
    if (!nullunit) {
        nullunit = createunit0(j_null);
    }
    try_level = 0;
    currproc = 0;
    varattribs = 0;
    intabledata = 0;
    inreadprint = 0;
    inparamlist = 0;
    inrecordbody = 0;
    inimportmodule = 0;
    labelseen = 0;
    ndollar = 0;
}

static void skipsemi	(void) {
    while (lx.symbol == semisym) {
        lex();
    }
}

static void addalias	(strec * stold,strec * stnew) {
    stnew = getduplnameptr(stold->owner,stnew,aliasid);
    adddef(stold->owner,stnew);
    stnew->equiv = stold;
}

static unitrec * makeblock	(unitrec * p) {
    return createunit1(j_block,p);
}

static void convertstmtexpr	(unitrec * p) {
    if ((p->tag==j_assignx)) {
        p->tag = j_assign;
    } else if ((p->tag==j_callfn)) {
        p->tag = j_callproc;
    } else if ((p->tag==j_callmfn)) {
        printf("%s\n","MAKE CALLMPROC1");
        p->tag = j_callmproc;
    }
    else {
        serror("NOT STMT EXPR");
    }
}

static void checkequals	(void) {
    if (!(lx.symbol == opsym && lx.subcode == j_eq)) {
        serror("\"=\" expected");
    }
}

static int32 getcurrline	(void) {
    return lx.lineno;
}

static int32 checkbegin	(int32 fbrack) {
int32	closesym;
    skipsemi();
    if (lx.symbol == lbracksym && fbrack) {
        closesym = rbracksym;
        lex();
    }
    else if (lx.symbol == kbeginsym) {
        closesym = kendsym;
        lex();
    }
    else if (lx.symbol == lcurlysym) {
        closesym = rcurlysym;
        lex();
    }
    else {
        closesym = kendsym;
    }
    return closesym;
}

static void checkbeginend	(int32 closesym,int32 kwd,int32 startline) {
    skipsemi();
    if (closesym == rbracksym || closesym == rcurlysym) {
        checksymbol(closesym);
    }
    else {
        checkend(closesym,kwd,startline,0);
    }
    lex();
}

static void checkend	(int32 endsym,int32 endkwd1,int32 endkwd2,int32 startline) {
char	str[100];
    if (endsym == lx.symbol && lx.symbol == rbracksym) {
        return;
    }
    if (lx.symbol != kendsym) {
        strcpy((char *)&str,"Bad 'end' ");
error:
L26:;
        if (startline) {
            sprintf((char *)(&str+strlen((char *)&str))," (from line %d)",startline);
        }
        serror((char *)&str);
    }
    if (lx.subcode == 0) {
        return;
    }
    if (!(endkwd1 && endkwd1 == lx.subcode || endkwd2 && endkwd2 == lx.subcode)) {
        strcpy((char *)&str,"Mismatched 'end'");
        goto L26;
    }
}

static void addgenfield	(strec * d) {
    if (d->nameid == genfieldid) {
        return;
    }
    if (ngenfieldnames >= maxgenfields) {
        serror("Too many genfields");
    }
    d->nameid = genfieldid;
    genfieldnames[++ngenfieldnames-1].def = d;
    d->offset = ngenfieldnames;
}

static void readvardef	(strec * owner,int32 isglobal,int32 isstatic,int32 varid) {
int32	nvars;
int32	m;
strec *	stname;
    lex();
    m = tvariant;
    nvars = 0;
    while (lx.symbol == namesym) {
        ++nvars;
        stname = getduplnameptr(owner,lx.symptr,varid);
        stname->mode = m;
        stname->attribs.ax_global = isglobal;
        stname->attribs.ax_static = isstatic;
        adddef(owner,stname);
        lex();
        if (lx.symbol == assignsym || lx.symbol == opsym && lx.subcode == j_eq) {
            if (lx.symbol == assignsym) {
                if (varid == staticid) {
                    serror("Need = on static not :=");
                }
            }
            else {
                if (varid == frameid) {
                    stname->nameid = staticid;
                    stname->attribs.ax_frame = 0;
                }
            }
            lex();
            stname->code = readexpression();
            stname->attribs.ax_equals = 1;
        }
        if (lx.symbol != commasym) {
            goto L29;
        }
        lex();
    }
L29:;
    if (nvars == 0) {
        serror("No vars declared");
    }
}

static void readconstdef	(strec * owner,int32 isglobal) {
int32	nconsts;
int32	deft;
int32	t;
strec *	stname;
    lex();
    deft = tvoid;
    nconsts = 0;
    while (lx.symbol == namesym) {
        stname = getduplnameptr(owner,lx.symptr,constid);
        lex();
        checkequals();
        lex();
        stname->code = readconstexpr((unitrec *)owner,1);
        if (deft == tvoid) {
            t = stname->code->valuemode;
        }
        else {
            t = deft;
        }
        stname->mode = t;
        ++nconsts;
        stname->attribs.ax_global = isglobal;
        adddef(owner,stname);
        if (lx.symbol != commasym) {
            goto L32;
        }
        lex();
    }
L32:;
    if (nconsts == 0) {
        serror("No consts declared");
    }
}

static unitrec * readexpression	(void) {
    return readfactor(8);
}

static unitrec * readfactor	(int32 level) {
unitrec *	p;
unitrec *	q;
unitrec *	r;
int32	opc;
int32	opprio;
int32	lineno;
    if (level <= 1) {
        p = readterm();
    }
    else {
        p = readfactor(level-1);
    }
L33:;
    switch (lx.symbol) {
    case opsym:
    case assignsym:
    case rangesym:
    case addrsym:
    case deepcopysym:
        opc = lx.subcode;
        opprio = jtagpriotable[opc];
        lineno = lx.lineno;
        if (opprio != level) {
            goto L34;
        }
        lex();
        if (opc == j_assignx || opc == j_deepcopyx) {
            q = readexpression();
        }
        else if (opc == j_power) {
            q = readfactor(level);
        }
        else {
            q = readfactor(level-1);
        }
        p = createunit2(opc,r = p,q);
        p->lineno = lineno;
        if (opprio == 6) {
            converteqeq(p);
        }
        if (r->tag == j_const && q->tag == j_const) {
            evalbinop(p,r,q);
        }
        break;
    default:;
        goto L34;
    }
    goto L33;
L34:;
    return p;
}

static unitrec * readterm	(void) {
unitrec *	p;
unitrec *	q;
unitrec *	r;
byte *	pbyte;
uint64	a;
int32	oldipl;
int32	opc;
int32	oldinrp;
int32	lineno;
int32	shift;
int32	av_1;
    lineno = lx.lineno;
    switch (lx.symbol) {
    case namesym:
        p = createname(lx.symptr);
        p->lineno = lx.lineno;
        if (nextlx.symbol == lbracksym) {
            createproccall(stmodule,lx.symptr,p);
        }
        else {
            px_name(stmodule,currproc,p,(int32)(nextlx.symbol == dotsym));
        }
        lex();
        break;
    case intconstsym:
    case realconstsym:
        p = createconstunit(lx.value,lx.subcode);
        lex();
        break;
    case stringconstsym:
        p = createstringconstunit(lx.svalue,lx.length);
        p->slength = lx.length;
        lex();
        break;
    case longintconstsym:
        *(lx.svalue+lx.length) = 0;
        p = createunit0(j_longint);
        p->svalue = lx.svalue;
        p->slength = lx.length;
        lex();
        break;
    case charconstsym:
        a = 0;
        shift = 0;
        pbyte = (byte *)lx.svalue;
        av_1 = lx.length;
        while (av_1--) {
            a = a|(uint64)*pbyte<<shift;
            shift += 8;
            ++pbyte;
        }
        p = createconstunit(a,tint);
        lex();
        break;
    case lbracksym:
        p = readlbrack();
        if (p->tag == j_makelist) {
            checkconstlist(p);
        }
        break;
    case lsqsym:
        oldipl = inparamlist;
        inparamlist = 0;
        p = readlsqbrack();
        inparamlist = oldipl;
        break;
    case stdtypesym:
    case krefsym:
        p = readcast();
        break;
    case opsym:
        p = readopc();
        break;
    case incrsym:
        opc = lx.subcode;
        lex();
        p = createunit1(opc,readterm());
        break;
    case ksprintsym:
        p = readsprint();
        break;
    case ksreadsym:
    case ksreadlnsym:
        p = readsread();
        break;
    case ptrsym:
    case addrsym:
        opc = lx.subcode;
        lex();
        p = createunit1(opc,readterm());
        if (p->a->tag == j_callfn) {
            if (!!p->a->b) {
                serror("Params not allowed");
            }
            p->a = p->a->a;
        }
        break;
    case compilervarsym:
        p = readcompilervar();
        break;
    case kerrorsym:
        p = createconstunit(lx.subcode,tint);
        lex();
        break;
    case dollarsym:
        if (intabledata) {
            p = createstringconstunit(tabledataname,-1);
        }
        else {
            if (ndollar <= 0) {
                serror("[$] No array");
            }
            p = createunit1(j_upb,dollarstack[ndollar-1]);
        }
        lex();
        break;
    case kapplyopsym:
        p = readapplyop(1);
        break;
    case kcastsym:
        p = readcastx();
        break;
    case ktypeconstsym:
        lex();
        checksymbol(lbracksym);
        lex();
        p = createunit0(j_typeconst);
        p->valuemode = readtypespec(0,0);
        checksymbol(rbracksym);
        lex();
        break;
    case kclampsym:
        lex();
        checksymbol(lbracksym);
        lex();
        p = readexpression();
        checksymbol(commasym);
        lex();
        q = readexpression();
        if (lx.symbol == rbracksym && q->tag == j_makerange) {
            r = q->b;
            q = q->a;
        }
        else {
            checksymbol(commasym);
            lex();
            r = readexpression();
            checksymbol(rbracksym);
        }
        lex();
        p = createunit3(j_clamp,p,q,r);
        break;
    case khostfnsym:
        p = readhostparams(0,1);
        break;
    default:;
        printf("%s\n",symbolnames[lx.symbol-1]);
        serror("readterm?");
    }
L41:;
    switch (lx.symbol) {
    case lbracksym:
        lex();
        oldinrp = inreadprint;
        inreadprint = 0;
        q = readslist(1,1);
        checksymbol(rbracksym);
        lex();
        p = createunit2(((p->tag == j_dot)?j_callmfn:j_callfn),p,q);
        p = testconstruct(p);
        inreadprint = oldinrp;
        break;
    case ptrsym:
        p = createunit1(j_ptr,p);
        lex();
        break;
    case lsqsym:
        p = readindex(p,0);
        break;
    case lcurlysym:
        p = readkeyindex(p,0);
        break;
    case dotsym:
        p = readdotsuffix(p);
        break;
    case colonsym:
        if (inreadprint) {
            goto L42;
        }
        lex();
        q = readexpression();
        p = createunit2((inparamlist?j_keyword:j_keyvalue),p,q);
        break;
    case incrsym:
        if ((lx.subcode==j_preincrx)) {
            opc = j_postincrx;
        } else if ((lx.subcode==j_predecrx)) {
            opc = j_postdecrx;
        }
        lex();
        p = createunit1(opc,p);
        break;
    case anddotsym:
        lex();
        checksymbol(lsqsym);
        lex();
        q = readexpression();
        if (q->tag == j_makerange) {
            p = createunit2(j_anddotslice,p,q);
        }
        else {
            p = createunit2(j_anddotindex,p,q);
        }
        checksymbol(rsqsym);
        lex();
        break;
    default:;
        goto L42;
    }
    goto L41;
L42:;
    p->lineno = lineno;
    return p;
}

static unitrec * readlbrack	(void) {
unitrec *	plower;
unitrec *	ulist;
unitrec *	ulistx;
unitrec *	p;
unitrec *	q;
unitrec *	r;
int32	lcmode;
int32	oldirp;
    lex();
    plower = 0;
    lcmode = tlist;
    ulist = ulistx = 0;
    if (lx.symbol == atsym) {
        lex();
        oldirp = inreadprint;
        inreadprint = 1;
        plower = readexpression();
        inreadprint = oldirp;
        checksymbol(colonsym);
        lex();
    }
    else if (lx.symbol == intconstsym && nextlx.symbol == colonsym) {
        plower = createconstunit(lx.value,lx.subcode);
        lex();
        lex();
    }
    else if (lx.symbol == opsym && nextlx.symbol == rbracksym) {
        p = createunit0(j_operator);
        p->opcode = lx.subcode;
        lex();
        lex();
        return p;
    }
    else if (lx.symbol == opsym && nextlx.symbol == assignsym) {
        p = createunit0(j_operator);
        p->opcode = getoptocode(lx.subcode);
        lex();
        lex();
        checksymbol(rbracksym);
        lex();
        return p;
    }
    if (lx.symbol == stdtypesym && lx.subcode == tstring) {
        lcmode = tstring;
        lex();
        checksymbol(colonsym);
        lex();
    }
    if ((lx.symbol==rbracksym)) {
        lex();
        p = createunit0(j_makelist);
        if (plower != 0) {
            p->b = plower;
        }
        return p;
    }
    else {
        p = readexpression();
    }
    if ((lx.symbol==rbracksym)) {
        lex();
        if (plower) {
            return createunit2(j_keyvalue,plower,p);
        }
        else {
            return p;
        }
    } else if ((lx.symbol==semisym)) {
        ulist = ulistx = p;
        do {
            lex();
            addlistunit(&ulist,&ulistx,(unitrec * *)readexpression());
        } while (!(lx.symbol != semisym));
        checksymbol(rbracksym);
        lex();
        p = ulist;
        while (p) {
            if ((p->tag==j_assignx)) {
                p->tag = j_assign;
            } else if ((p->tag==j_deepcopyx)) {
                p->tag = j_deepcopy;
            } else if ((p->tag==j_preincrx)) {
                p->tag = j_preincr;
            } else if ((p->tag==j_predecrx)) {
                p->tag = j_predecr;
            } else if ((p->tag==j_postincrx)) {
                p->tag = j_postincr;
            } else if ((p->tag==j_postdecrx)) {
                p->tag = j_postdecr;
            } else if ((p->tag==j_callfn)) {
                p->tag = j_callproc;
            } else if ((p->tag==j_callmfn)) {
                p->tag = j_callmproc;
            }
            p = p->nextunit;
        }
        return createunit1(j_exprlist,ulist);
    } else if ((lx.symbol==commasym)) {
        if (nextlx.symbol == rbracksym) {
            lex();
            lex();
            return createunit2(j_makelist,p,plower);
        }
        ulist = ulistx = p;
        do {
            lex();
            if (lx.symbol == rbracksym) {
                goto L70;
            }
            if (lx.symbol == commasym) {
                serror(",, null expr not allowed");
            }
            addlistunit(&ulist,&ulistx,(unitrec * *)readexpression());
            skipsemi();
        } while (!(lx.symbol != commasym));
L70:;
        checksymbol(rbracksym);
        lex();
        return createunit2(j_makelist,ulist,plower);
    } else if ((lx.symbol==barsym)) {
        lex();
        q = readexpression();
        if (lx.symbol == barsym) {
            lex();
            r = readexpression();
            checksymbol(rbracksym);
            lex();
            return createunit3(j_ifx,p,q,r);
        }
        addlistunit(&ulist,&ulistx,(unitrec * *)q);
        checksymbol(commasym);
        if (nextlx.symbol != barsym) {
            do {
                lex();
                addlistunit(&ulist,&ulistx,(unitrec * *)readexpression());
            } while (!(lx.symbol != commasym));
            checksymbol(barsym);
        }
        else {
            lex();
        }
        lex();
        r = readexpression();
        checksymbol(rbracksym);
        lex();
        return createunit3(j_selectx,p,ulist,r);
    } else if ((lx.symbol==dbarsym) || (lx.symbol==kforsym) || (lx.symbol==kforallsym)) {
        serror("READLISTCOMP");
    }
    else {
        serror("(x ...");
    }
    return 0;
}

static void addlistunit	(unitrec * * ulist,unitrec * * ulistx,unitrec * * p) {
    if (*ulist == 0) {
        *ulist = *ulistx = (unitrec *)p;
    }
    else {
        (*ulistx)->nextunit = (unitrec *)p;
    }
    *ulistx = (unitrec *)p;
}

static void addlistparam	(strec * * ulist,strec * * ulistx,strec * * p) {
    if (*ulist == 0) {
        *ulist = *ulistx = (strec *)p;
    }
    else {
        (*ulistx)->nextparam = (strec *)p;
    }
    *ulistx = (strec *)p;
}

static unitrec * readlsqbrack	(void) {
unitrec *	ulist;
unitrec *	ulistx;
unitrec *	p;
unitrec *	q;
    lex();
    ulist = ulistx = 0;
    if (lx.symbol == rsqsym) {
        lex();
        p = createunit1(j_makesetlist,0);
        if (!checkdict(p)) {
            checkconstlist(p);
        }
        return p;
    }
    while (1) {
        addlistunit(&ulist,&ulistx,(unitrec * *)readexpression());
        skipsemi();
        if (lx.symbol != commasym) {
            goto L78;
        }
        lex();
        if (lx.symbol == rsqsym) {
            goto L78;
        }
    }
L78:;
    checksymbol(rsqsym);
    lex();
    p = createunit1(j_makesetlist,ulist);
    if (!checkdict(p)) {
        checkconstlist(p);
    }
    return p;
}

static unitrec * readcast	(void) {
unitrec *	p;
int32	t;
int32	opc;
    t = readtypespec(0,0);
    if ((lx.symbol==atsym) || (lx.symbol==lbracksym)) {
    }
    else {
        if (t == tvoid) {
            p = createunit0(j_typeval);
        }
        else {
            p = createunit0(j_typeconst);
        }
        p->valuemode = t;
        return p;
    }
    checkunpackedtype(t);
    if (lx.symbol == atsym) {
        lex();
        opc = j_typepun;
    }
    else {
        opc = j_convert;
    }
    checksymbol(lbracksym);
    p = readlbrack();
    if (p->tag == j_makelist) {
        p->tag = j_makeconstr;
    }
    p = createunit1(opc,p);
    p->valuemode = t;
    return p;
}

static unitrec * readopc	(void) {
unitrec *	p;
unitrec *	q;
int32	opc;
int32	opc2;
    opc = lx.subcode;
    lex();
    if ((opc==j_add)) {
        return readterm();
    } else if ((opc==j_sub)) {
        opc = j_neg;
    } else if ((opc==j_min) || (opc==j_max) || (opc==j_atan2) || (opc==j_concat) || (opc==j_append)) {
        checksymbol(lbracksym);
        lex();
        p = readexpression();
        if (lx.symbol == commasym) {
            lex();
            q = readexpression();
            checksymbol(rbracksym);
            lex();
            return createunit2(opc,p,q);
        }
        else {
            checksymbol(rbracksym);
            lex();
            if ((opc==j_min)) {
                opc2 = j_min1;
            } else if ((opc==j_max)) {
                opc2 = j_max1;
            }
            else {
                serror("readopc");
            }
            return createunit1(opc,p);
        }
    }
    if (lx.symbol == assignsym) {
        serror("op:= not allowed");
    }
    p = createunit1(opc,readterm());
    evalmonop(p);
    return p;
}

static unitrec * readsprint	(void) {
int32	oldinreadprint;
int32	opc;
int32	isfprint;
unitrec *	pformat;
unitrec *	pdev;
unitrec *	printlist;
unitrec *	printlistx;
unitrec *	p;
    oldinreadprint = inreadprint;
    inreadprint = 1;
    opc = lx.subcode;
    lex();
    checksymbol(lbracksym);
    lex();
    if ((opc==j_sfprint) || (opc==j_cprint)) {
        isfprint = 1;
    }
    else {
        isfprint = 0;
    }
    printlist = printlistx = 0;
    pformat = pdev = nullunit;
    if (lx.symbol == atsym) {
        lex();
        pdev = readexpression();
        if (lx.symbol == commasym) {
            lex();
        }
        else {
            goto L89;
        }
    }
    if (isfprint) {
        pformat = readexpression();
        if (lx.symbol == commasym) {
            lex();
        }
        else {
            goto L89;
        }
    }
    if (lx.symbol == rbracksym) {
        goto L89;
    }
    while (1) {
        if (lx.symbol == commasym) {
            addlistunit(&printlist,&printlistx,(unitrec * *)createunit0(j_nogap));
        }
        else {
            p = readexpression();
            if (lx.symbol == colonsym) {
                lex();
                p = createunit2(j_fmtitem,p,readexpression());
            }
            addlistunit(&printlist,&printlistx,(unitrec * *)p);
        }
        if (lx.symbol != commasym) {
            goto L91;
        }
        lex();
    }
L91:;
    checksymbol(rbracksym);
finish:
L89:;
    lex();
    inreadprint = oldinreadprint;
    if ((opc == j_print || opc == j_fprint) && printlist == 0) {
        serror("No print items");
    }
    if (isfprint) {
        if (pformat->tag == j_null) {
            serror("No fmt str");
        }
        return createunit3(opc,pdev,pformat,printlist);
    }
    else {
        return createunit2(opc,pdev,printlist);
    }
}

static unitrec * readsread	(void) {
int32	oldinreadprint;
int32	opc;
unitrec *	pformat;
unitrec *	pdev;
unitrec *	p;
unitrec *	readlist;
unitrec *	readlistx;
    oldinreadprint = inreadprint;
    inreadprint = 1;
    opc = lx.subcode;
    lex();
    checksymbol(lbracksym);
    lex();
    readlist = readlistx = 0;
    pformat = pdev = nullunit;
    if (lx.symbol == atsym) {
        if (opc == j_read) {
            serror("@ on read");
        }
        lex();
        pdev = readexpression();
        if (lx.symbol == commasym) {
            lex();
        }
        else {
            goto L92;
        }
    }
    if (lx.symbol == rbracksym) {
        goto L92;
    }
    while (1) {
        p = readexpression();
        if (lx.symbol == colonsym) {
            lex();
            p = createunit2(j_fmtitem,p,readexpression());
        }
        addlistunit(&readlist,&readlistx,(unitrec * *)p);
        if (lx.symbol != commasym) {
            goto L94;
        }
        lex();
    }
L94:;
    checksymbol(rbracksym);
finish:
L92:;
    lex();
    inreadprint = oldinreadprint;
    if (opc == j_read && readlist == 0) {
        serror("No read items");
    }
    return createunit2(opc,pdev,readlist);
}

static unitrec * readcompilervar	(void) {
char	str[100];
    if ((lx.subcode==j_cvlineno)) {
        return createconstunit(lx.lineno,tint);
    } else if ((lx.subcode==j_cvstrlineno)) {
        sprintf((char *)&str,"%d",lx.lineno);
    } else if ((lx.subcode==j_cvmodulename)) {
        strcpy((char *)&str,moduletable[currmoduleno].name);
    } else if ((lx.subcode==j_cvfilename)) {
        strcpy((char *)&str,moduletable[currmoduleno].filename);
    } else if ((lx.subcode==j_cvfunction)) {
        strcpy((char *)&str,(currproc?(currproc)->name:"<none>"));
    }
    else {
        serror("compiler var not impl");
    }
    lex();
    return createstringconstunit(pcm_copyheapstring((char *)&str),-1);
}

static unitrec * readcastx	(void) {
int32	opc;
unitrec *	pexpr;
unitrec *	p;
int32	ptype;
    lex();
    if (lx.symbol == atsym) {
        lex();
        opc = j_typepun;
    }
    else {
        opc = j_convert;
    }
    checksymbol(lbracksym);
    lex();
    pexpr = readexpression();
    checksymbol(commasym);
    lex();
    ptype = readtypespec(0,0);
    checksymbol(rbracksym);
    lex();
    p = createunit1(opc,pexpr);
    p->valuemode = ptype;
    return p;
}

void checksymbol	(int32 symbol) {
char	str[100];
    if (lx.symbol != symbol) {
        sprintf((char *)&str,"%s expected, not %s",symbolnames[symbol-1],symbolnames[lx.symbol-1]);
        serror((char *)&str);
    }
}

static int32 readtypespec	(strec * owner,int32 typedefx) {
strec *	d;
int32	length;
int32	lower;
int32	upper;
int32	t;
int32	kwd;
unitrec *	x;
unitrec *	pupper;
unitrec *	plx;
int32	p;
enum {maxdim = 10};
int32	lowerdims[10];
int32	lengthdims[10];
int32	ndims;
int32	i;
int32	n;
    if ((lx.symbol==lsqsym)) {
arraybounds:
        lex();
        ndims = 0;
        inreadprint = 1;
        while (1) {
            lower = arraylbound;
            length = 0;
            if (lx.symbol == rsqsym || lx.symbol == commasym) {
            }
            else {
                x = readconstexpr((unitrec *)owner,0);
                if (x->tag == j_makerange) {
                    serror("Array dims not constant");
                    lower = getrangelwb(x);
                    upper = getrangeupb(x);
                    length = (upper-lower)+1;
                }
                else if (x->tag == j_const && x->valuemode == trange) {
                    lower = x->range_lower;
                    upper = x->range_upper;
                    length = (upper-lower)+1;
                }
                else {
                    if ((lx.symbol==rsqsym) || (lx.symbol==commasym)) {
                        if (!!isconstexpr(x)) {
                            length = getconstvalue(x,0);
                        }
                        else {
                            serror("array1");
                        }
                    } else if ((lx.symbol==colonsym)) {
                        lower = getconstvalue(x,0);
                        lex();
                        if (!(lx.symbol == commasym || lx.symbol == rsqsym)) {
                            length = readconstexprvalue((unitrec *)owner);
                        }
                    }
                }
            }
            lowerdims[++ndims-1] = lower;
            lengthdims[ndims-1] = length;
            if (lx.symbol != commasym) {
                goto L103;
            }
            lex();
        }
L103:;
        inreadprint = 0;
        checksymbol(rsqsym);
        lex();
        t = readtypespec(owner,0);
        checkpackedtype(t);
        for (i=ndims; i>=1; --i) {
            t = createarraymode(t,lowerdims[i-1],lengthdims[i-1],((i == 1)?typedefx:0));
        }
        return t;
    } else if ((lx.symbol==stdtypesym)) {
        t = lx.subcode;
        lex();
        if ((t==tstring) || (t==tset) || (t==tstringz)) {
            if (lx.symbol == opsym && lx.subcode == j_mul) {
                lex();
                n = readconstexprvalue((unitrec *)owner);
                if (n <= 0) {
                    serror("Bad *N");
                }
                p = createstringmode(t,n,typedefx);
            }
            else {
                p = t;
            }
        }
        else {
            p = t;
        }
    } else if ((lx.symbol==namesym)) {
        d = px_typecheck(owner,lx.symptr,0);
        if (d == 0) {
            printf("%s\n",lx.symptr->name);
            serror("Unknown type");
        }
        if (d->mode == tvoid) {
            p = createtype(d);
        }
        else {
            p = d->mode;
        }
        lex();
    } else if ((lx.symbol==kvarsym)) {
        p = tvariant;
        lex();
    } else if ((lx.symbol==kenumsym)) {
        lex();
        p = readenumtype(owner,typedefx,0);
    } else if ((lx.symbol==lbracksym)) {
        p = readenumtype(owner,typedefx,0);
    } else if ((lx.symbol==krecordsym)) {
        serror("Use 'record name=', not 'type name=record'");
    } else if ((lx.symbol==kstructsym)) {
        kwd = lx.symbol;
        lex();
        if (lx.symbol == datsym) {
            if (owner == 0) {
                serror("record@@");
            }
            lex();
            checksymbol(intconstsym);
            if ((lx.value==1) || (lx.value==2) || (lx.value==4) || (lx.value==8) || (lx.value==16)) {
            }
            else {
                serror("record@@ bad align");
            }
            owner->attribs.ax_align = lx.value;
            lex();
        }
        p = readstructdef(owner,typedefx,kwd);
    } else if ((lx.symbol==kunionsym)) {
        serror("Top-level union not allowed");
    } else if ((lx.symbol==krefsym)) {
        lex();
        if ((lx.symbol==kprocsym) || (lx.symbol==kfunctionsym) || (lx.symbol==kmethodsym)) {
            serror("CAN'T DO REF PROC");
        }
        else {
            t = readtypespec(owner,0);
            checkpackedtype(t);
        }
        p = createrefpackmode(t,typedefx);
    }
    else {
        serror("Bad type starter");
    }
    return p;
}

static unitrec * readhostparams	(unitrec * lhs,int32 isfn) {
int32	fnindex;
int32	oldinrp;
unitrec *	p;
unitrec *	q;
    fnindex = lx.subcode;
    lex();
    checksymbol(lbracksym);
    lex();
    oldinrp = inreadprint;
    inreadprint = 0;
    q = readslist(1,1);
    checksymbol(rbracksym);
    lex();
    inreadprint = oldinrp;
    if (lhs) {
        lhs->nextunit = q;
        q = lhs;
    }
    p = createunit1((isfn?j_callhostfn:j_callhostproc),q);
    p->opcode = fnindex;
    return p;
}

static unitrec * readslist	(int32 iscall,int32 donulls) {
unitrec *	ulist;
unitrec *	ulistx;
int32	oldinparamlist;
    ulist = ulistx = 0;
    skipsemi();
    if (lx.symbol == rbracksym) {
        return ulist;
    }
    oldinparamlist = inparamlist;
    inparamlist = iscall;
    while (1) {
        skipsemi();
        if ((lx.symbol==commasym)) {
            if (donulls) {
                addlistunit(&ulist,&ulistx,(unitrec * *)createunit0(j_null));
            }
            else {
                serror("null comma expr not allowed");
            }
            lex();
        } else if ((lx.symbol==rbracksym)) {
            if (donulls) {
                addlistunit(&ulist,&ulistx,(unitrec * *)nullunit);
            }
            goto L130;
        }
        else {
            addlistunit(&ulist,&ulistx,(unitrec * *)readexpression());
            if (lx.symbol == commasym) {
                lex();
                if (lx.symbol == rbracksym) {
                    goto L130;
                }
            }
            else {
                skipsemi();
                if (lx.symbol == rbracksym) {
                    goto L130;
                }
            }
        }
    }
L130:;
    inparamlist = oldinparamlist;
    return ulist;
}

static unitrec * readindex	(unitrec * p,int32 dot) {
unitrec *	q;
    lex();
    while (1) {
        if (ndollar >= maxdollarstack) {
            serror("Too many nested a[$]");
        }
        dollarstack[++ndollar-1] = p;
        q = readexpression();
        --ndollar;
        if (q->tag == j_makerange) {
            p = createunit2((dot?j_dotslice:j_slice),p,q);
        }
        else {
            p = createunit2((dot?j_dotindex:j_index),p,q);
        }
        if (lx.symbol != commasym) {
            goto L134;
        }
        lex();
    }
L134:;
    checksymbol(rsqsym);
    lex();
    return p;
}

static unitrec * readkeyindex	(unitrec * p,int32 dot) {
unitrec *	q;
unitrec *	r;
    lex();
    q = readexpression();
    r = 0;
    if (lx.symbol == commasym) {
        lex();
        r = readexpression();
    }
    checksymbol(rcurlysym);
    lex();
    return p = createunit3((dot?j_dotkeyindex:j_keyindex),p,q,r);
}

static unitrec * readdotsuffix	(unitrec * p) {
unitrec *	q;
int32	t;
    while (lx.symbol == dotsym) {
        lex();
        switch (lx.symbol) {
        case lsqsym:
            p = readindex(p,1);
            break;
        case lcurlysym:
            p = readkeyindex(p,1);
            break;
        case namesym:
            p = createunit2(j_dot,p,createname(lx.symptr));
            px_dot(stmodule,currproc,p);
            lex();
            break;
        case opsym:
            p = createunit1(lx.subcode,p);
            lex();
            break;
        case lbracksym:
            lex();
            p = createunit2(j_dotattr,p,readexpression());
            checksymbol(rbracksym);
            lex();
            break;
        case ktypesym:
            if ((p->tag==j_typeconst)) {
            } else if ((p->tag==j_typeval)) {
                p->tag = j_typeconst;
            }
            else {
                p = createunit1(j_gettype,p);
            }
            lex();
            break;
        case khostfnsym:
            p = readhostparams(p,1);
            break;
        case stdtypesym:
            t = lx.subcode;
            lex();
            checksymbol(lsqsym);
            lex();
            q = readexpression();
            checksymbol(rsqsym);
            lex();
            if ((t==tu8) || (t==tu16) || (t==tu32)) {
            }
            else {
                serror("Bad .type[]");
            }
            p = createunit2(j_byteindex,p,q);
            p->valuemode = t;
            break;
        default:;
            serror("Unknown dot suffix");
        }
    }
    return p;
}

int32 isconstexpr	(unitrec * p) {
    return (int32)(p->tag == j_const);
}

static unitrec * readconstexpr	(unitrec * owner,int32 needconst) {
unitrec *	p;
    p = readexpression();
    if (needconst > 0) {
        if (p->tag != j_const) {
            printf("%s %s %s\n",jtagnames[p->tag],jtagnames[p->a->tag],jtagnames[p->b->tag]);
            serror("RCE:Not const expr");
        }
        if (needconst == 2) {
            serror("RCE/NEEDCONST=2");
        }
    }
    return p;
}

static int64 readconstexprvalue	(unitrec * owner) {
unitrec *	p;
    p = readexpression();
    if (p->tag != j_const) {
        serror("RCEV:Not const expr");
    }
    return getconstvalue(p,0);
}

static int32 readconstint	(void) {
int64	x;
    if (lx.symbol == intconstsym) {
        x = lx.value;
        lex();
        return x;
    }
    serror("Can't do complex expr");
    return 0;
}

static void readprocdef	(strec * procowner,int32 isglobal,int32 fflang) {
int32	kwd;
int32	startline;
int32	closesym;
strec *	stproc;
strec *	q;
    kwd = lx.symbol;
    stproc = readprocdecl(procowner,isglobal,fflang);
    checkequals();
    lex();
    startline = getcurrline();
    closesym = checkbegin(0);
    currproc = stproc;
    nextavindex = 0;
    stproc->code = readblock(stproc);
    checkbeginend(closesym,kwd,startline);
    stproc->attribs.ax_equals = 1;
    if (labelseen) {
        q = stproc->deflist;
        while (q) {
            if (q->nameid == labelid && !!q->attribs.ax_forward) {
                printf("%s %s %s\n",q->name,"in",currproc->name);
                serror("Label not defined");
            }
            q = q->nextdef;
        }
    }
    currproc = 0;
}

strec * readprocdecl	(strec * procowner,int32 isglobal,int32 fflang) {
int32	kwd;
int32	varparams;
int32	try_level;
int32	prettype;
int32	nparams;
char *	metadata;
char *	truename;
strec *	pequiv;
strec *	stproc;
strec *	owner;
strec *	paramlist;
strec *	nameptr;
    kwd = lx.symbol;
    pequiv = 0;
    metadata = "";
    truename = 0;
    varparams = 0;
    try_level = 0;
    lex();
    if (lx.symbol == stringconstsym) {
        truename = pcm_copyheapstring(lx.svalue);
        convlcstring(lx.svalue);
        lx.symptr = addnamestr(lx.svalue);
    }
    else {
        checksymbol(namesym);
    }
    nameptr = lx.symptr;
    stproc = createprocdef(procowner,nameptr,((procowner->nameid == dllmoduleid)?dllprocid:procid),truename);
    if (getscope(procowner) != importscope && procowner->nameid == typeid) {
        addgenfield(nameptr);
    }
    owner = stproc;
    currproc = stproc;
    lex();
    if (lx.symbol == namesym && strcmp(lx.symptr->name,"as") == 0) {
        lex();
        checksymbol(namesym);
        addalias(stproc,lx.symptr);
        lex();
    }
    paramlist = 0;
    prettype = tvoid;
    nparams = 0;
    if (lx.symbol == opsym && lx.subcode == j_lt) {
        if (stproc->nameid == dllprocid) {
            serror("Metadata on dllproc");
        }
        lex();
        checksymbol(stringconstsym);
        stproc->metadata = lx.svalue;
        lex();
        if (!(lx.symbol == opsym && (lx.subcode == j_gt || lx.subcode == j_ge))) {
            serror("\">\" expected");
        }
        if (lx.symbol == opsym && lx.subcode == j_ge) {
            lx.subcode = j_eq;
        }
        else {
            lex();
        }
    }
    if (lx.symbol == lbracksym) {
        lex();
        if (lx.symbol != rbracksym) {
            paramlist = readparams(stproc,fflang,&varparams,&nparams);
            checksymbol(rbracksym);
        }
        lex();
        if (lx.symbol == colonsym || lx.symbol == sendtosym) {
            lex();
            prettype = readtypespec(owner,0);
        }
        else if (!!typestarterset[lx.symbol] || lx.symbol == namesym) {
            prettype = readtypespec(owner,0);
        }
    }
    else if (lx.symbol == colonsym || lx.symbol == sendtosym) {
        lex();
        prettype = readtypespec(owner,0);
    }
    if (!(prettype != tvoid || kwd != kfunctionsym && kwd != kmethodsym)) {
        prettype = tvariant;
    }
    if (prettype != tvoid && kwd != kfunctionsym && kwd != kmethodsym) {
        serror("Proc can't return value");
    }
    if (prettype != tvoid && fflang != 0 && fflang != qlangff) {
        checkdlltype(prettype);
    }
    else if (prettype != tvoid && (fflang == 0 || fflang == qlangff)) {
        checkunpackedtype(prettype);
    }
    stproc->paramlist = paramlist;
    stproc->attribs.ax_nparams = nparams;
    stproc->mode = prettype;
    if (lx.symbol == atsym) {
        lex();
        checksymbol(namesym);
        lex();
        stproc->attribs.ax_at = 1;
    }
    stproc->code = 0;
    if ((fflang==clangff) || (fflang==windowsff) || (fflang==mlangff)) {
        if (procowner->nameid != dllmoduleid) {
            printf("%s\n",stproc->name);
            serror("FF should be in dll import");
        }
    }
    else {
        if ((procowner->nameid==moduleid) || (procowner->nameid==extmoduleid)) {
        } else if ((procowner->nameid==dllmoduleid)) {
            serror("Need FF specifier");
        }
    }
    stproc->attribs.ax_global = isglobal;
    stproc->attribs.ax_varparams = varparams;
    stproc->attribs.ax_fflang = fflang;
    if (procowner == stmodule && stproc->namelen == 5 && strcmp(stproc->name,"start") == 0 || stproc->namelen == 4 && strcmp(stproc->name,"main") == 0) {
        stproc->attribs.ax_global = 1;
    }
    return stproc;
}

static strec * readparams	(strec * owner,int32 fflang,int32 * varparams,int32 * inparams) {
strec *	stlist;
strec *	stlistx;
strec *	stname;
strec *	d;
int32	foptional;
int32	fbyref;
int32	pmode;
int32	fparam;
int32	nparams;
unitrec *	pdefvalue;
char	str[30];
    stlist = stlistx = 0;
    foptional = 0;
    fbyref = 0;
    pdefvalue = 0;
    pmode = tvoid;
    stname = 0;
    fparam = 0;
    nparams = 0;
    if (fflang == 0) {
        fflang = qlangff;
    }
    while (1) {
        switch (lx.symbol) {
        case questionsym:
            if (foptional) {
                serror("??");
            }
            lex();
            foptional = 1;
            break;
        case addrsym:
            if (fbyref) {
                serror("& &");
            }
            lex();
            fbyref = 1;
            break;
        case stdtypesym:
        case lsqsym:
        case kvarsym:
        case krefsym:
        case kenumsym:
        case krecordsym:
dotype:
L161:;
            pmode = readtypespec(0,0);
            if (fflang != qlangff) {
dotype2:
L162:;
                checkdlltype(pmode);
            }
            else {
                checkunpackedtype(pmode);
            }
            fparam = 1;
            break;
        case namesym:
            if (fflang != qlangff) {
                d = px_typecheck(owner,lx.symptr,0);
                if (d) {
                    pmode = d->mode;
                    lex();
                    goto L162;
                }
            }
            if (ttbasetype[pmode] == tvoid && fflang != qlangff) {
                serror("FF needs typed params");
            }
            if (nextlx.symbol == namesym) {
                goto L161;
            }
            stname = getduplnameptr(owner,lx.symptr,paramid);
            adddef(owner,stname);
            fparam = 1;
            lex();
            break;
        case commasym:
        case rbracksym:
            if (!fparam) {
                serror("No param");
            }
            ++nparams;
            if (stname == 0) {
                sprintf((char *)&str,"$%d",nparams);
                stname = getduplnameptr(owner,addnamestr((char *)&str),paramid);
                adddef(owner,stname);
            }
            stname->mode = ((ttbasetype[pmode] == tvoid)?tvariant:pmode);
            if (fbyref) {
                stname->attribs.ax_byrefmode = 1;
            }
            if (foptional) {
                stname->attribs.ax_optional = 1;
            }
            if (pdefvalue != 0) {
                stname->code = pdefvalue;
                stname->attribs.ax_equals = 1;
            }
            addlistparam(&stlist,&stlistx,(strec * *)stname);
            if (lx.symbol == rbracksym) {
                goto L160;
            }
            else {
                lex();
                fparam = 0;
                stname = 0;
                pdefvalue = 0;
                fbyref = foptional = 0;
            }
            break;
        case assignsym:
            lex();
            pdefvalue = readexpression();
            break;
        case opsym:
            if (lx.subcode != j_eq) {
                serror("param op?");
            }
            lex();
            pdefvalue = readexpression();
            break;
        case ellipsissym:
            if (fparam) {
                serror("... param");
            }
            *varparams = 1;
            lex();
            checksymbol(rbracksym);
            goto L160;
            break;
        default:;
            printf("%s\n",symbolnames[lx.symbol-1]);
            serror("param");
        }
    }
L160:;
    *inparams = nparams;
    return stlist;
}

static unitrec * readblock	(strec * owner) {
int32	lineno;
int32	globalflag;
unitrec *	ulist;
unitrec *	ulistx;
unitrec *	p;
    skipsemi();
    lineno = lx.lineno;
    ulist = ulistx = 0;
    while (1) {
        switch (lx.symbol) {
        case kstaticsym:
            lex();
            checksymbol(kvarsym);
            readvardef(owner,0,1,staticid);
            break;
        case kprocsym:
        case kfunctionsym:
        case kmethodsym:
            readprocdef(owner,0,0);
            globalflag = 0;
            break;
        case kvarsym:
            readvardef(owner,0,0,frameid);
            break;
        case ktypesym:
            readtypedef(owner,0);
            break;
        case kconstsym:
            readconstdef(owner,0);
            break;
        case kclasssym:
        case krecordsym:
            readclassdef(owner,globalflag);
            globalflag = 0;
            break;
        case docstringsym:
            serror("DOCSTRING");
            break;
        case kenumsym:
            lex();
            readenumtype(owner,0,0);
            break;
        case eofsym:
            printf("%s\n",owner->name);
            serror("Unexpected EOF in proc");
            break;
        case rbracksym:
        case kelsifsym:
        case kelsesym:
        case kuntilsym:
        case kwhensym:
        case kelsecasesym:
        case kelseswitchsym:
        case kexceptsym:
        case kendsym:
        case rcurlysym:
            goto L164;
            break;
        case semisym:
            lex();
            break;
        default:;
            p = readexecstmt(owner);
            if ((int64)(int32)p == 1 || p == 0) {
                serror("READEXEC RETURNED 1 OR NIL");
            }
            addlistunit(&ulist,&ulistx,(unitrec * *)p);
        }
    }
L164:;
    return createunit1(j_block,ulist);
}

static unitrec * readexecstmt	(strec * owner) {
unitrec *	p;
unitrec *	q;
strec *	stname;
    switch (lx.symbol) {
    case namesym:
        if ((nextlx.symbol==colonsym)) {
            stname = createlabel(lx.symptr,1);
            p = createunit0(j_labeldef);
            p->def = stname;
            p->trylevel = try_level;
            stname->offset = try_level;
            setnameptr(p);
            lex();
            lx.symbol = semisym;
        }
        else {
            p = readstmtexpr(owner);
        }
        break;
    case kgotosym:
        p = readgoto(owner,j_goto);
        break;
    case kifsym:
        p = readif(owner);
        break;
    case kunlesssym:
        p = readunless(owner);
        break;
    case kcasesym:
    case kdocasesym:
    case kswitchsym:
    case kdoswitchsym:
        p = readswitchcase(owner);
        break;
    case kforsym:
        p = readfor(owner);
        break;
    case kforallsym:
        p = readforall(owner);
        break;
    case ktosym:
        p = readto(owner);
        break;
    case kdosym:
        p = readdo(owner);
        break;
    case kwhilesym:
        p = readwhile(owner);
        break;
    case krepeatsym:
        p = readrepeat(owner);
        break;
    case kloopsym:
        p = readloopcontrol(owner);
        break;
    case kreturnsym:
        p = readreturn(owner);
        break;
    case kstopsym:
        p = readstop(owner);
        break;
    case kprintsym:
        p = readprint(owner);
        break;
    case kreadsym:
        p = readread(owner);
        break;
    case ktrysym:
        p = readtry(owner);
        break;
    case kraisesym:
        p = readraise(owner);
        break;
    case opsym:
        if (lx.subcode == j_mul && nextlx.symbol == lbracksym) {
            serror("KTERM?");
        }
        else {
            p = readstmtexpr(owner);
        }
        break;
    case kswapsym:
        lex();
        checksymbol(lbracksym);
        lex();
        p = readexpression();
        checksymbol(commasym);
        lex();
        q = readexpression();
        checksymbol(rbracksym);
        lex();
        p = createunit2(j_swap,p,q);
        break;
    case lbracksym:
    case incrsym:
    case ksreadsym:
    case ksreadlnsym:
        p = readstmtexpr(owner);
        break;
    case khostfnsym:
        p = readhostparams(0,0);
        break;
    case kevalsym:
        lex();
        p = createunit1(j_eval,readexpression());
        break;
    default:;
        printf("%s\n",symbolnames[lx.symbol-1]);
        serror("Stmt error");
    }
    return p;
}

static unitrec * readstmtexpr	(strec * owner) {
int32	opc;
unitrec *	p;
unitrec *	q;
    if (lx.symbol == opsym && nextlx.symbol == assignsym) {
        opc = lx.subcode;
        if ((opc==j_sub)) {
            opc = j_neg;
        }
        lex();
        lex();
        return createunit1(getoptocode(opc),readterm());
    }
    p = readterm();
    if ((p->tag==j_preincrx)) {
        p->tag = j_preincr;
    } else if ((p->tag==j_predecrx)) {
        p->tag = j_predecr;
    } else if ((p->tag==j_postincrx)) {
        p->tag = j_postincr;
    } else if ((p->tag==j_postdecrx)) {
        p->tag = j_postdecr;
    }
    if ((lx.symbol==assignsym)) {
        lex();
        q = readexpression();
        p = createunit2(j_assign,p,q);
        return readcondsuffix(p);
    } else if ((lx.symbol==deepcopysym)) {
        lex();
        q = readexpression();
        p = createunit2(j_deepcopy,p,q);
        return readcondsuffix(p);
    } else if ((lx.symbol==opsym)) {
        if (nextlx.symbol == assignsym) {
            opc = getoptocode(lx.subcode);
            lex();
            lex();
            return createunit2(opc,p,readexpression());
        }
        else {
            serror("op expr not allowed as statement");
        }
    } else if ((lx.symbol==kswapsym)) {
        lex();
        return createunit2(j_swap,p,readterm());
    }
    if ((p->tag==j_assignx)) {
        p->tag = j_assign;
    } else if ((p->tag==j_deepcopyx)) {
        p->tag = j_deepcopy;
    } else if ((p->tag==j_callfn)) {
        p->tag = j_callproc;
        p = readcondsuffix(p);
    } else if ((p->tag==j_callmfn)) {
        p->tag = j_callmproc;
        p = readcondsuffix(p);
    }
    else {
        if ((p->tag==j_preincr) || (p->tag==j_predecr) || (p->tag==j_postincr) || (p->tag==j_postdecr) || (p->tag==j_callproc) || (p->tag==j_sread) || (p->tag==j_sreadln)) {
        }
        else {
            printf("%s\n",jtagnames[p->tag]);
            serror("Expression not allowed as statement");
        }
    }
    return p;
}

static unitrec * readcondsuffix	(unitrec * p) {
    if ((lx.symbol==kifsym) || (lx.symbol==kwhensym)) {
        lex();
        return createunit2(j_if,readexpression(),createunit1(j_block,p));
    } else if ((lx.symbol==kunlesssym)) {
        lex();
        return createunit2(j_if,createunit1(j_notl,readexpression()),createunit1(j_block,p));
    }
    else {
        return p;
    }
    return 0;
}

static unitrec * readif	(strec * owner) {
int32	line;
int32	kwd;
int32	lineno;
unitrec *	pthen;
unitrec *	pcond;
unitrec *	plist;
unitrec *	plistx;
unitrec *	pelse;
unitrec *	p;
unitrec *	pelsif;
    line = lx.lineno;
    kwd = lx.symbol;
    lex();
    pcond = readexpression();
    skipsemi();
    checksymbol(kthensym);
    lex();
    pthen = readblock(owner);
    if (lx.symbol == kelsifsym) {
        lineno = lx.lineno;
        plist = plistx = createunit2(j_elsif,pcond,pthen);
        while (lx.symbol == kelsifsym) {
            lineno = lx.lineno;
            lex();
            pcond = readexpression();
            checksymbol(kthensym);
            lex();
            pthen = readblock(owner);
            pelsif = createunit2(j_elsif,pcond,pthen);
            pelsif->lineno = lineno;
            addlistunit(&plist,&plistx,(unitrec * *)pelsif);
        }
        if ((lx.symbol==kelsesym)) {
            lex();
            pelse = readblock(owner);
            checkend(kendsym,kwd,0,0);
            lex();
        } else if ((lx.symbol==kelsecasesym) || (lx.symbol==kelseswitchsym)) {
            lx.symbol = kwd;
            pelse = makeblock(readswitchcase(owner));
        }
        else {
            pelse = createunit0(j_block);
            checkend(kendsym,kwd,0,0);
            lex();
        }
        p = createunit2(j_longif,plist,pelse);
        p->lineno = line;
        return p;
    }
    if ((lx.symbol==kelsesym)) {
        lex();
        pelse = readblock(owner);
        checkend(kendsym,kwd,0,0);
        lex();
    }
    else {
        pelse = createunit0(j_block);
        checkend(kendsym,kwd,0,0);
        lex();
    }
    p = createunit3(j_if,pcond,pthen,pelse);
    p->lineno = line;
    return p;
}

static unitrec * readgoto	(strec * owner,int32 gototag) {
strec *	d;
unitrec *	p;
    if (lx.subcode == 1) {
        lex();
        checksymbol(ktosym);
    }
    lex();
    if (lx.symbol == namesym && nextlx.symbol != ptrsym && nextlx.symbol != lsqsym && nextlx.symbol != dotsym) {
        d = createlabel(lx.symptr,0);
        p = createname(d);
        lex();
    }
    else {
        serror("GOTO LABEL EXPR");
    }
    return readcondsuffix(createunit1(gototag,p));
}

static unitrec * readunless	(strec * owner) {
int32	line;
unitrec *	pcond;
unitrec *	pthen;
unitrec *	pelse;
unitrec *	p;
    line = lx.lineno;
    lex();
    pcond = readexpression();
    checksymbol(kthensym);
    lex();
    pthen = readblock(owner);
    if (lx.symbol == kelsesym) {
        lex();
        pelse = readblock(owner);
    }
    else {
        pelse = createunit0(j_block);
    }
    checkend(kendsym,kunlesssym,0,0);
    lex();
    p = createunit3(j_if,createunit1(j_notl,pcond),pthen,pelse);
    p->lineno = line;
    return p;
}

static unitrec * readswitchcase	(strec * owner) {
int32	line;
int32	kwd;
int32	opc;
int32	lineno;
unitrec *	pexpr;
unitrec *	pwhenlist;
unitrec *	pwhenlistx;
unitrec *	pwhen;
unitrec *	pwhenx;
unitrec *	pelse;
unitrec *	p;
unitrec *	pthen;
unitrec *	pwhenthen;
unitrec *	q;
    line = lx.lineno;
    kwd = lx.symbol;
    opc = lx.subcode;
    lex();
    pexpr = readexpression();
    pwhenlist = pwhenlistx = 0;
    skipsemi();
    while (lx.symbol == kwhensym) {
        lineno = lx.lineno;
        lex();
        pwhen = pwhenx = 0;
        while (1) {
            p = readexpression();
            p->lineno = lineno;
            addlistunit(&pwhen,&pwhenx,(unitrec * *)p);
            if (lx.symbol != commasym) {
                goto L195;
            }
            lex();
        }
L195:;
        checksymbol(kthensym);
        lex();
        pthen = readblock(owner);
        pwhenthen = createunit2(j_whenthen,pwhen,pthen);
        pwhenthen->lineno = lineno;
        addlistunit(&pwhenlist,&pwhenlistx,(unitrec * *)pwhenthen);
    }
    if ((lx.symbol==kelsesym)) {
        lex();
        pelse = readblock(owner);
        checkend(kendsym,kwd,0,0);
        lex();
    } else if ((lx.symbol==kelsifsym)) {
        lx.symbol = kwd;
        pelse = makeblock(readif(owner));
    } else if ((lx.symbol==kelsecasesym) || (lx.symbol==kelseswitchsym)) {
        lx.symbol = kwd;
        pelse = makeblock(readswitchcase(owner));
    }
    else {
        pelse = makeblock(0);
        checkend(kendsym,kwd,0,0);
        lex();
    }
    p = createunit3(opc,pexpr,pwhenlist,pelse);
    p->lineno = line;
    return p;
}

static unitrec * readstop	(strec * owner) {
unitrec *	p;
int32	i;
    lex();
    if (!!exprstarterset[lx.symbol]) {
        p = createunit1(j_stop,readexpression());
    }
    else {
        p = createunit0(j_stop);
    }
    return readcondsuffix(p);
}

static unitrec * readreturn	(strec * owner) {
unitrec *	p;
    lex();
    if (!!exprstarterset[lx.symbol]) {
        p = createunit1(j_return,readexpression());
    }
    else {
        p = createunit0(j_return);
    }
    return readcondsuffix(p);
}

static unitrec * readdo	(strec * owner) {
unitrec *	p;
int32	line;
    line = lx.lineno;
    lex();
    p = readblock(owner);
    checkend(kendsym,kdosym,0,0);
    lex();
    p = createunit1(j_do,p);
    p->lineno = line;
    return p;
}

static unitrec * readto	(strec * owner) {
int32	line;
int32	id;
unitrec *	p;
unitrec *	pcount;
unitrec *	pbody;
    line = lx.lineno;
    lex();
    pcount = readexpression();
    checksymbol(kdosym);
    lex();
    pbody = readblock(owner);
    checkend(kendsym,ktosym,kdosym,0);
    lex();
    id = frameid;
    if (owner->nameid != procid) {
        id = staticid;
    }
    p = createunit3(j_to,pcount,pbody,createname(getavname(owner,id)));
    p->lineno = line;
    return p;
}

static unitrec * readwhile	(strec * owner) {
int32	line;
int32	id;
unitrec *	pcond;
unitrec *	pa;
unitrec *	pb;
unitrec *	pc;
unitrec *	pbody;
unitrec *	p;
    line = lx.lineno;
    lex();
    pcond = readexpression();
    if (lx.symbol == commasym) {
        convertstmtexpr(pcond);
        pa = pcond;
        lex();
        pb = readexpression();
        checksymbol(commasym);
        lex();
        pc = readstmtexpr(owner);
        checksymbol(kdosym);
        lex();
        pbody = readblock(owner);
        checkend(kendsym,kwhilesym,kdosym,0);
        lex();
        pa->nextunit = pb;
        pb->nextunit = pc;
        p = createunit2(j_cfor,pa,pbody);
        p->lineno = line;
        return p;
    }
    checksymbol(kdosym);
    lex();
    pbody = readblock(owner);
    checkend(kendsym,kwhilesym,kdosym,0);
    lex();
    p = createunit2(j_while,pcond,pbody);
    p->lineno = line;
    return p;
}

static unitrec * readrepeat	(strec * owner) {
int32	line;
unitrec *	pbody;
unitrec *	pcond;
unitrec *	p;
    line = lx.lineno;
    lex();
    pbody = readblock(owner);
    checksymbol(kuntilsym);
    lex();
    pcond = readexpression();
    p = createunit2(j_repeat,pbody,pcond);
    p->lineno = line;
    return p;
}

static unitrec * readloopcontrol	(strec * owner) {
int32	opc;
unitrec *	p;
    opc = lx.subcode;
    lex();
    if (lx.symbol == namesym && strcmp(lx.symptr->name,"all") == 0) {
        lex();
        p = createunit1(opc,createconstunit(0,tint));
    }
    else if (!!exprstarterset[lx.symbol]) {
        p = createunit1(opc,readconstexpr((unitrec *)owner,1));
    }
    else {
        p = createunit0(opc);
    }
    return readcondsuffix(p);
}

static unitrec * readprint	(strec * owner) {
int32	oldinreadprint;
int32	opc;
int32	isfprint;
int32	fshowname;
int32	length;
unitrec *	pformat;
unitrec *	pdev;
unitrec *	printlist;
unitrec *	printlistx;
unitrec *	p;
unitrec *	q;
strbuffer *	expr;
char *	s;
    oldinreadprint = inreadprint;
    inreadprint = 1;
    opc = lx.subcode;
    if ((opc==j_fprint) || (opc==j_fprintln) || (opc==j_cprint) || (opc==j_cprintln)) {
        isfprint = 1;
    }
    else {
        isfprint = 0;
    }
    lex();
    printlist = printlistx = 0;
    pformat = pdev = 0;
    if (lx.symbol == atsym) {
        lex();
        pdev = readexpression();
        if (lx.symbol == commasym) {
            lex();
        }
        else {
            goto L205;
        }
    }
    if (isfprint) {
        if (!exprstarterset[lx.symbol] && opc == j_cprintln) {
            goto L205;
        }
        pformat = readexpression();
        if (lx.symbol == commasym) {
            lex();
        }
        else {
            goto L205;
        }
    }
    if (!exprstarterset[lx.symbol]) {
        goto L205;
    }
    while (1) {
        if (lx.symbol == commasym) {
            addlistunit(&printlist,&printlistx,(unitrec * *)createunit0(j_nogap));
        }
        else {
            fshowname = 0;
            if (lx.symbol == opsym && lx.subcode == j_eq) {
                fshowname = 1;
                lex();
            }
            p = readexpression();
            if (lx.symbol == colonsym) {
                lex();
                p = createunit2(j_fmtitem,p,readexpression());
            }
            if (fshowname) {
                expr = strexpr(p);
                length = expr->length;
                strbuffer_add(expr,"=?",-1);
                s = expr->strptr;
                iconvucn(s,length);
                *(s+length+1) = 0;
                addlistunit(&printlist,&printlistx,(unitrec * *)(q = createstringconstunit(s,-1)));
            }
            addlistunit(&printlist,&printlistx,(unitrec * *)p);
        }
        if (lx.symbol != commasym) {
            goto L207;
        }
        lex();
    }
L207:;
finish:
L205:;
    inreadprint = oldinreadprint;
    if (opc == j_print && printlist == 0) {
        serror("No print items");
    }
    if (opc == j_fprint && printlist == 0 && pformat == 0) {
        serror("No print items");
    }
    if (opc == j_cprint && printlist == 0 && pformat == 0) {
        serror("No cprint items");
    }
    if (isfprint) {
        if (pformat == 0 && opc != j_cprintln) {
            serror("No fmt str");
        }
        return createunit3(opc,pdev,pformat,printlist);
    }
    else {
        return createunit2(opc,pdev,printlist);
    }
}

static unitrec * readread	(strec * owner) {
int32	oldinreadprint;
int32	opc;
unitrec *	pformat;
unitrec *	pdev;
unitrec *	readlist;
unitrec *	readlistx;
unitrec *	p;
    oldinreadprint = inreadprint;
    inreadprint = 1;
    opc = lx.subcode;
    lex();
    readlist = readlistx = 0;
    pformat = pdev = 0;
    if (lx.symbol == atsym) {
        if (opc == j_read) {
            serror("@ on read");
        }
        lex();
        pdev = readexpression();
        if (lx.symbol == commasym) {
            lex();
        }
        else {
            goto L208;
        }
    }
    if (!exprstarterset[lx.symbol]) {
        goto L208;
    }
    while (1) {
        p = readexpression();
        if (lx.symbol == colonsym) {
            lex();
            p = createunit2(j_fmtitem,p,readexpression());
        }
        addlistunit(&readlist,&readlistx,(unitrec * *)p);
        if (lx.symbol != commasym) {
            goto L210;
        }
        lex();
    }
L210:;
finish:
L208:;
    inreadprint = oldinreadprint;
    if (opc == j_read && readlist == 0) {
        serror("No read items");
    }
    return createunit2(opc,pdev,readlist);
}

static unitrec * readtry	(strec * owner) {
unitrec *	ptry;
unitrec *	pexceptlist;
unitrec *	pexceptlistx;
unitrec *	px;
unitrec *	q;
unitrec *	exlist;
unitrec *	exlistx;
    ++try_level;
    lex();
    ptry = readblock(owner);
    pexceptlist = pexceptlistx = 0;
    while (lx.symbol == kexceptsym) {
        lex();
        exlist = exlistx = 0;
        while (1) {
            addlistunit(&exlist,&exlistx,(unitrec * *)readconstexpr((unitrec *)owner,1));
            if (lx.symbol != commasym) {
                goto L215;
            }
            lex();
        }
L215:;
        checksymbol(kthensym);
        lex();
        px = readblock(owner);
        addlistunit(&pexceptlist,&pexceptlistx,(unitrec * *)createunit2(j_except,exlist,px));
    }
    checkend(kendsym,ktrysym,0,0);
    lex();
    --try_level;
    return createunit2(j_try,ptry,pexceptlist);
}

static unitrec * readraise	(strec * owner) {
unitrec *	p;
    lex();
    p = readexpression();
    return createunit1(j_raise,p);
}

static unitrec * readfor	(strec * owner) {
int32	line;
int32	opc;
int32	down;
unitrec *	pstep;
unitrec *	pvar;
unitrec *	pcond;
unitrec *	pfrom;
unitrec *	pto;
unitrec *	pelse;
unitrec *	prange;
unitrec *	prangex;
unitrec *	pautovar;
unitrec *	pbody;
unitrec *	p;
strbuffer *	S;
    line = lx.lineno;
    lex();
    pvar = readterm();
    if (pvar->tag != j_name) {
        serror("For: name expected");
    }
    opc = j_forup;
    pstep = 0;
    pcond = 0;
    if (lx.symbol == opsym) {
        if (lx.subcode == j_inrev) {
            down = j_fordown;
        }
        else if (lx.subcode != j_in) {
            serror("in/inrev expected");
        }
        lex();
        prange = readexpression();
        pfrom = getrangelwbunit(prange);
        pto = getrangeupbunit(prange);
    }
    else {
        if (lx.symbol == assignsym) {
            lex();
            pfrom = readexpression();
        }
        else {
            pfrom = createconstunit(1,tint);
        }
        checksymbol(ktosym);
        opc = ((lx.subcode == 1)?j_fordown:j_forup);
        lex();
        pto = readexpression();
        if (lx.symbol == kbysym) {
            if (opc == j_fordown) {
                serror("downto/by");
            }
            opc = j_forstep;
            lex();
            pstep = readconstexpr((unitrec *)owner,0);
            if (pstep->tag == j_const && pstep->value == 1) {
                opc = j_forup;
                pstep = 0;
            }
            else if (pstep->tag == j_const && pstep->value == -1) {
                opc = j_fordown;
                pstep = 0;
            }
        }
        else {
            pstep = 0;
        }
    }
    if (lx.symbol == kwhensym) {
        lex();
        pcond = readexpression();
    }
    checksymbol(kdosym);
    lex();
    pbody = readblock(owner);
    if (lx.symbol == kelsesym) {
        lex();
        pelse = readblock(owner);
    }
    else {
        pelse = 0;
    }
    checkend(kendsym,kforsym,kdosym,0);
    lex();
    if (pcond != 0) {
        pbody = makeblock(createunit2(j_if,pcond,pbody));
    }
    pautovar = 0;
    if (!(pto->tag == j_const || pto->tag == j_name && !!pto->def->attribs.ax_frame)) {
        if (opc == j_forstep) {
            serror("for: 'by' uses complex limit");
        }
        pautovar = createname(getavname(owner,11));
    }
    pvar->nextunit = pfrom;
    pfrom->nextunit = pto;
    pto->nextunit = pstep;
    pbody->nextunit = pelse;
    p = createunit3(opc,pvar,pbody,pautovar);
    p->lineno = line;
    return p;
}

static unitrec * readforall	(strec * owner) {
int32	opc;
int32	line;
int32	isforall;
unitrec *	pindex;
unitrec *	pvar;
unitrec *	pcond;
unitrec *	plist;
unitrec *	pbody;
unitrec *	pelse;
unitrec *	p;
unitrec *	pfor;
unitrec *	pautovar;
    line = lx.lineno;
    opc = lx.subcode;
    isforall = (int32)(opc == j_forall);
    lex();
    pvar = readterm();
    pindex = 0;
    if (lx.symbol == commasym) {
        lex();
        pindex = pvar;
        pvar = readterm();
        if (pindex->tag != j_name || !pindex->def->attribs.ax_frame) {
            serror("forall var not simple local name");
        }
    }
    if (pvar->tag != j_name) {
        serror("forall var not name");
    }
    pcond = 0;
    if (lx.symbol == opsym && (lx.subcode == j_in || lx.subcode == j_inrev)) {
        if (lx.subcode == j_inrev) {
            opc = ((opc == j_forall)?j_forallrev:j_foreachrev);
        }
        lex();
        plist = readexpression();
    }
    else {
        serror("in/inrev expected");
    }
    if (lx.symbol == kwhensym) {
        lex();
        pcond = readexpression();
    }
    checksymbol(kdosym);
    lex();
    pbody = readblock(owner);
    if (lx.symbol == kelsesym) {
        lex();
        pelse = readblock(owner);
    }
    else {
        pelse = 0;
    }
    checkend(kendsym,kforallsym,kdosym,0);
    lex();
    if (pindex == 0) {
        pindex = createname(getavname(owner,11));
    }
    if (pcond != 0) {
        pbody = createunit2(j_if,pcond,pbody);
        pbody->lineno = line;
    }
    pautovar = createname(getavname(currproc,11));
    pindex->nextunit = pvar;
    pvar->nextunit = plist;
    pbody->nextunit = pelse;
    pfor = createunit3(opc,pindex,pbody,pautovar);
    pfor->lineno = line;
    return pfor;
}

void readtypedef	(strec * owner,int32 isglobal) {
strec *	sttype;
strec *	stname;
int32	t;
int32	m;
    lex();
    checksymbol(namesym);
    stname = lx.symptr;
    lex();
    checkequals();
    lex();
    sttype = px_typecheck(owner,stname,0);
    if (sttype == 0) {
        sttype = getduplnameptr(owner,stname,typeid);
        adddef(owner,sttype);
        m = createusertype(sttype);
    }
    else {
        m = sttype->mode;
    }
    t = readtypespec(sttype,m);
    sttype->attribs.ax_global = isglobal;
    sttype->mode = t;
}

static int32 readstructdef	(strec * owner,int32 typedefx,int32 kwd) {
int32	m;
int32	startline;
int32	closesym;
int32	t;
strec *	recordowner;
strec *	d;
    recordowner = owner;
    if (!typedefx) {
        if (lx.symbol == namesym) {
            owner = getduplnameptr(owner,lx.symptr,typeid);
            lex();
            checkequals();
            lex();
        }
        else {
            owner = getduplnameptr(stmodule,addnamestr(nextautotype()),typeid);
        }
        adddef(recordowner,owner);
        checksymbol(lbracksym);
        lex();
    }
    else {
        owner = ttnamedef[typedefx];
        startline = getcurrline();
        closesym = checkbegin(1);
    }
    m = createrecordmode(owner,tstruct,typedefx);
    owner->mode = m;
    unionstr_clear(&unionstring);
    unionstr_clear(&unionpend);
L222:;
    while (1) {
        if (lx.symbol == namesym) {
            d = px_typecheck(owner,lx.symptr,0);
            if (d) {
                lex();
                readstructfields(owner,d->mode);
                goto L222;
            }
        }
        if ((lx.symbol==kstructsym) || (lx.symbol==kunionsym)) {
            unionstr_append(&unionpend,((lx.symbol == kstructsym)?'S':'U'));
            unionlastvar = 0;
            lex();
        } else if ((lx.symbol==kendsym)) {
            if (!!unionstring.ulength) {
                checkend(kendsym,((unionstr_last(&unionstring) == 'S')?kstructsym:kunionsym),0,0);
                lex();
                if (unionlastvar == 0 || !!unionpend.ulength) {
                    serror("Empty union group");
                }
                if ((unionstr_last(&(unionlastvar)->uflags)=='E') || (unionstr_last(&(unionlastvar)->uflags)=='*')) {
                }
                else {
                    unionstr_append(&(unionlastvar)->uflags,'*');
                }
                unionstr_append(&(unionlastvar)->uflags,'E');
                unionstring.ulength--;
            }
            else {
                goto L223;
            }
        } else if ((lx.symbol==semisym)) {
            lex();
        }
        else {
            if (!!typestarterset[lx.symbol]) {
                t = readtypespec(owner,0);
                checkpackedtype(t);
                readstructfields(owner,t);
            }
            else {
                goto L223;
            }
        }
    }
L223:;
    if (!typedefx) {
        checksymbol(rbracksym);
        lex();
    }
    else {
        checkbeginend(closesym,kwd,startline);
    }
    return m;
}

void readstructfields	(strec * owner,int32 m) {
int32	nvars;
strec *	stname;
    nvars = 0;
    while (lx.symbol == namesym) {
        stname = getduplnameptr(owner,lx.symptr,fieldid);
        stname->mode = m;
        ++nvars;
        if (!!unionpend.ulength) {
            unionstr_copy(&(stname)->uflags,&unionpend);
            unionstr_concat(&unionstring,&unionpend);
            unionstr_clear(&unionpend);
        }
        else {
            unionstr_clear(&(stname)->uflags);
        }
        unionlastvar = stname;
        if (getscope(owner) != importscope) {
            addgenfield(lx.symptr);
        }
        adddef(owner,stname);
        lex();
        if (lx.symbol == atsym) {
            lex();
            stname->attribs.ax_at = 1;
            stname->equiv = readequivfield(owner);
        }
        else if (lx.symbol == datsym) {
            lex();
            checksymbol(intconstsym);
            if ((lx.value==1) || (lx.value==2) || (lx.value==4) || (lx.value==8)) {
                stname->attribs.ax_align = lx.value;
            } else if ((lx.value==0)) {
                stname->attribs.ax_align = 255;
            }
            else {
                serror("@@ bad align");
            }
            lex();
        }
        if (lx.symbol != commasym) {
            goto L232;
        }
        lex();
    }
L232:;
    if (nvars == 0) {
        serror("No fields declared");
    }
}

void readtabledef	(strec * owner,int32 isglobal) {
int32	i;
int32	ncols;
int32	nrows;
int32	enums;
int32	nextenumvalue;
int32	firstval;
int32	lastval;
int32	startline;
int32	closesym;
int32	vartype;
unitrec *	plower;
char *	enumtypename;
strec *	stvar;
strec *	stenum;
strec *	stgen;
enum {maxcols = 20};
strec *	varnameptrs[20];
strec *	plist[20];
strec *	plistx[20];
enum {maxrows = 500};
int32	enumvalues[500];
    lex();
    enums = 0;
    enumtypename = 0;
    if (lx.symbol == lbracksym) {
        enums = 1;
        lex();
        if (lx.symbol == namesym) {
            enumtypename = lx.symptr->name;
            lex();
        }
        checksymbol(rbracksym);
        lex();
    }
    nextenumvalue = 1;
    nrows = 0;
    ncols = 0;
    while (lx.symbol == namesym) {
        if (++ncols > maxcols) {
            serror("tabledata/too many columns");
        }
        varnameptrs[ncols-1] = lx.symptr;
        lex();
        if (lx.symbol == commasym) {
            lex();
        }
        else {
            goto L241;
        }
    }
L241:;
    checkequals();
    lex();
    skipsemi();
    startline = getcurrline();
    closesym = checkbegin(0);
    skipsemi();
    firstval = lastval = 0;
    for (i=1; i<=ncols; ++i) {
        plist[i-1] = plistx[i-1] = 0;
    }
    intabledata = 1;
    while (1) {
        skipsemi();
        checksymbol(lbracksym);
        lex();
        if (++nrows > maxrows) {
            serror("tabledata:too many rows");
        }
        if (enums) {
            checksymbol(namesym);
            stgen = lx.symptr;
            tabledataname = stgen->name;
            lex();
            if (lx.symbol == opsym && lx.subcode == j_eq) {
                lex();
                nextenumvalue = readconstint();
            }
            enumvalues[nrows-1] = nextenumvalue;
            stenum = getduplnameptr(owner,stgen,constid);
            stenum->mode = tint;
            stenum->code = createconstunit(nextenumvalue,tint);
            stenum->attribs.ax_global = isglobal;
            adddef(owner,stenum);
            if (nrows == 1) {
                firstval = nextenumvalue;
            }
            lastval = nextenumvalue;
            ++nextenumvalue;
            if (ncols) {
                checksymbol(commasym);
            }
            lex();
        }
        for (i=1; i<=ncols; ++i) {
            addlistunit((unitrec * *)&plist[i-1],(unitrec * *)&plistx[i-1],(unitrec * *)readexpression());
            if (i == ncols) {
                checksymbol(rbracksym);
            }
            else {
                checksymbol(commasym);
            }
            lex();
        }
        if (lx.symbol != commasym) {
            goto L247;
        }
        lex();
        if (lx.symbol == closesym) {
            goto L247;
        }
    }
L247:;
    intabledata = 0;
    skipsemi();
    checkbeginend(closesym,ktabledatasym,startline);
    if (nrows == 0) {
        serror("No table data");
    }
    vartype = tvariant;
    for (i=1; i<=ncols; ++i) {
        stvar = getduplnameptr(owner,varnameptrs[i-1],staticid);
        if (enums) {
            plower = createconstunit(enumvalues[1-1],tint);
        }
        else {
            plower = 0;
        }
        stvar->code = createunit2(j_makelist,(unitrec *)plist[i-1],plower);
        stvar->attribs.ax_global = isglobal;
        adddef(owner,stvar);
    }
}

void readclassdef	(strec * owner,int32 isglobal) {
int32	kwd;
int32	baseclass;
int32	m;
int32	startline;
int32	closesym;
int32	mrec;
int32	normalexit;
strec *	nameptr;
strec *	sttype;
strec *	newd;
strec *	d;
strec *	e;
    kwd = lx.symbol;
    lex();
    checksymbol(namesym);
    nameptr = lx.symptr;
    lex();
    baseclass = 0;
    if (lx.symbol == lbracksym) {
        lex();
        baseclass = readtypespec(owner,0);
        checksymbol(rbracksym);
        lex();
    }
    checkequals();
    lex();
    sttype = getduplnameptr(owner,nameptr,typeid);
    adddef(owner,sttype);
    m = createusertype(sttype);
    mrec = createrecordmode(owner,trecord,m);
    sttype->mode = mrec;
    sttype->base_class = baseclass;
    closesym = checkbegin(1);
    startline = getcurrline();
    readclassbody(sttype,kwd);
    checkbeginend(closesym,kwd,startline);
    if (baseclass) {
        d = ttnamedef[baseclass]->deflist;
        while (d) {
            e = sttype->deflist;
            normalexit = 1;
            while (e) {
                if (strcmp(d->name,e->name) == 0) {
                    normalexit = 0;
                    goto L261;
                }
                e = e->nextdef;
            }
L261:;
            if (normalexit) {
                if ((d->nameid==procid) || (d->nameid==linkid)) {
                    newd = getduplnameptr(sttype,d,linkid);
                    newd->equiv = d;
                }
                else {
                    newd = getduplnameptr(sttype,d,d->nameid);
                    duplfield(d,newd);
                }
                adddef(sttype,newd);
            }
            d = d->nextdef;
        }
    }
    sttype->attribs.ax_global = isglobal;
}

static void readclassbody	(strec * owner,int32 classkwd) {
int32	kwd;
L264:;
    switch (lx.symbol) {
    case kconstsym:
        readconstdef(owner,0);
        break;
    case kvarsym:
        readrecordfields(owner);
        break;
    case kmethodsym:
    case kfunctionsym:
    case kprocsym:
        kwd = lx.symbol;
        if (getscope(owner) == importscope) {
            readprocdecl(owner,0,0);
        }
        else {
            readprocdef(owner,0,0);
        }
        break;
    case kclasssym:
        lex();
        serror("CLASS CLASS");
        break;
    case krecordsym:
        lex();
        serror("CLASS RECORD");
        break;
    case ktypesym:
        lex();
        serror("CLASS TYPE");
        break;
    case kendsym:
    case rbracksym:
    case rcurlysym:
        goto L265;
        break;
    case eofsym:
        serror("Class eof?");
        goto L265;
        break;
    case semisym:
        lex();
        break;
    default:;
        PS("symbol");
        if (!!typestarterset[lx.symbol]) {
            serror("Packed types not allowed in class");
        }
        else {
            serror("Unknown class decl");
        }
    }
    goto L264;
L265:;
}

static int32 readenumtype	(strec * owner,int32 typedefx,int32 isglobal) {
strec *	enumowner;
strec *	stname;
strec *	nameptr;
int32	isanon;
int32	index;
int32	startline;
int32	closesym;
    enumowner = owner;
    isanon = 0;
    if (!typedefx) {
        if (lx.symbol == namesym) {
            stname = getduplnameptr(owner,(strec *)lx.symptr->name,typeid);
            owner = stname;
            lex();
            checkequals();
            lex();
            adddef(enumowner,owner);
        }
        else {
            isanon = 1;
        }
        checksymbol(lbracksym);
        lex();
    }
    else {
        owner = ttnamedef[typedefx];
        startline = getcurrline();
        closesym = checkbegin(1);
    }
    index = 1;
    while (lx.symbol == namesym) {
        nameptr = lx.symptr;
        lex();
        if (lx.symbol == opsym && lx.subcode == j_eq) {
            lex();
            index = readconstint();
        }
        if (!isanon) {
            stname = getduplnameptr(owner,nameptr,enumid);
            stname->index = index;
            stname->mode = tint;
            adddef(owner,stname);
        }
        else {
            stname = getduplnameptr(enumowner,nameptr,constid);
            stname->code = createconstunit(index,tint);
            stname->mode = tint;
            adddef(enumowner,stname);
        }
        ++index;
        stname->attribs.ax_global = isglobal;
        if (lx.symbol != commasym) {
            goto L268;
        }
        lex();
    }
L268:;
    if (!typedefx) {
        checksymbol(rbracksym);
        lex();
    }
    else {
        checkbeginend(closesym,kenumsym,startline);
    }
    if (!isanon) {
        return createenummode(owner,typedefx);
    }
    else {
        return tvoid;
    }
}

static void duplfield	(strec * p,strec * q) {
    if (!!p->code) {
        serror("DUPLFIELD");
    }
    q->attribs = p->attribs;
    q->address = p->address;
    q->uflags = p->uflags;
    q->mode = p->mode;
}

void readrecordfields	(strec * owner) {
int32	m;
int32	nvars;
strec *	stname;
    lex();
    m = tvariant;
    nvars = 0;
    while (lx.symbol == namesym) {
        stname = getduplnameptr(owner,lx.symptr,fieldid);
        stname->mode = m;
        ++nvars;
        if (getscope(owner) != importscope) {
            addgenfield(lx.symptr);
        }
        adddef(owner,stname);
        lex();
        if (lx.symbol == atsym) {
            lex();
            stname->attribs.ax_at = 1;
            stname->equiv = readequivfield(owner);
        }
        if (lx.symbol != commasym) {
            goto L271;
        }
        lex();
    }
L271:;
    if (nvars == 0) {
        serror("No fields declared");
    }
}

static void readimportmodule	(strec * owner) {
int32	isnew;
int32	startline;
int32	closesym;
strec *	d;
strec *	stname;
strec *	stname0;
    lex();
    if (lx.symbol == stringconstsym) {
        stname = addnamestr(lx.svalue);
    }
    else {
        checksymbol(namesym);
        stname = lx.symptr;
    }
    lex();
    checkequals();
    lex();
    isnew = 1;
    d = stname->nextdupl;
    while (d) {
        if (d->nameid == dllmoduleid) {
            stname = d;
            isnew = 0;
            goto L274;
        }
        d = d->nextdupl;
    }
L274:;
    if (isnew) {
        stname = getduplnameptr(stprogram,stname,dllmoduleid);
        if (!!strcmp(stname->name,"sys")) {
            stsysmodule = stname;
        }
        adddef(stprogram,stname);
        if (ndlltable >= maxdlllib) {
            serror("Too many DLL libs");
        }
        dlltable[++ndlltable-1] = stname->name;
        stname->attribs.ax_dllindex = ndlltable;
    }
    startline = getcurrline();
    closesym = checkbegin(0);
    readimportbody(stname);
    checkbeginend(closesym,kimportmodulesym,startline);
}

static void readimportbody	(strec * owner) {
int32	lineno;
int32	fflang;
    lineno = lx.lineno;
    while (1) {
        skipsemi();
        switch (lx.symbol) {
        case kfflangsym:
            fflang = lx.subcode;
            lex();
            if ((lx.symbol==kprocsym) || (lx.symbol==kfunctionsym) || (lx.symbol==kmethodsym)) {
                readprocdecl(owner,0,fflang);
            }
            break;
        case kprocsym:
        case kfunctionsym:
        case kmethodsym:
            readprocdecl(owner,0,0);
            break;
        case kvarsym:
            readvardef(owner,0,0,staticid);
            break;
        case ktypesym:
            readtypedef(owner,0);
            break;
        case kconstsym:
            readconstdef(owner,0);
            break;
        case kclasssym:
        case krecordsym:
            readclassdef(owner,0);
            break;
        case eofsym:
            goto L276;
            break;
        case kendsym:
            goto L276;
            break;
        default:;
            PS1("symbol");
            serror("Not allowed in importmodule");
        }
    }
L276:;
}

static strec * createlabel	(strec * stname,int32 islabeldef) {
strec *	d;
    labelseen = 1;
    d = finddupl(currproc,stname->nextdupl);
    if (d) {
        if (d->nameid != labelid) {
            serror("Not a label");
        }
        if (islabeldef) {
            if (!d->attribs.ax_forward) {
                printf("%s\n",d->name);
                serror("Dupl label");
            }
            d->attribs.ax_forward = 0;
        }
    }
    else {
        d = getduplnameptr(currproc,stname,labelid);
        adddef_nodupl(currproc,d);
        if (!islabeldef) {
            d->attribs.ax_forward = 1;
        }
    }
    return d;
}

static strec * createprocdef	(strec * owner,strec * stname,int32 id,char * truename) {
strec *	d;
strec *	e;
procrec *	pp;
unitrec *	u;
    d = finddupl(owner,stname->nextdupl);
    if (d) {
        if (d->nameid != procid && d->nameid != dllprocid) {
            printf("%s %s\n",d->name,namenames[d->nameid]);
            serror("def:Not a proc");
        }
        if (!d->attribs.ax_forward) {
            printf("%s\n",d->name);
            serror("Dupl proc");
        }
        addtoproclist(d);
        d->attribs.ax_forward = 0;
    }
    else {
        d = getduplnameptr(owner,stname,id);
        adddef_nodupl(owner,d);
        addtoproclist(d);
        d->index = 0;
        if (id == dllprocid) {
            if (ndllproctable >= maxdllproc) {
                serror("Too many DLL procs");
            }
            ++ndllproctable;
            if (!truename) {
                truename = d->name;
            }
            d->truename = truename;
            dllproctable[ndllproctable-1].name = truename;
            dllproctable[ndllproctable-1].dllindex = owner->attribs.ax_dllindex;
            d->index = ndllproctable;
        }
        else {
            e = stname->nextdupl;
            while (e) {
                if (e->nameid == procid && !!e->callchain) {
                    u = e->callchain;
                    do {
                        if (u->tag != j_name) {
                            serror("CPD1");
                        }
                        u->def = d;
                        u = u->c;
                    } while (!(u == 0));
                }
                e = e->nextdupl;
            }
        }
    }
    return d;
}

static void createproccall	(strec * owner,strec * stname,unitrec * p) {
strec *	d;
    d = resolvetopname(owner,stname,0);
    if (d) {
retry:
L285:;
        if ((d->nameid==procid)) {
            if (d->owner->nameid == extmoduleid) {
                if (!!d->callchain) {
                    if (d->attribs.ax_extmodno != currmoduleno) {
                        d->callchain = 0;
                    }
                }
                p->c = d->callchain;
                d->callchain = p;
                d->attribs.ax_extmodno = currmoduleno;
            }
        } else if ((d->nameid==dllprocid)) {
        } else if ((d->nameid==typeid)) {
        } else if ((d->nameid==aliasid)) {
            d = d->equiv;
            goto L285;
        }
        else {
            printf("%s\n",namenames[d->nameid]);
            serror("call:Not a proc");
        }
    }
    else {
        d = getduplnameptr(owner,stname,procid);
        adddef_nodupl(owner,d);
        d->attribs.ax_forward = 1;
    }
    p->def = d;
}

static strec * readequivfield	(strec * owner) {
strec *	p;
strec *	d;
    checksymbol(namesym);
    d = lx.symptr;
    lex();
    p = owner->deflist;
    while (p) {
        if (strcmp(p->name,d->name) == 0) {
            return p;
        }
        p = p->nextdef;
    }
    printf("%s\n",d->name);
    serror("Can't find @ field");
    return 0;
}

static unitrec * testconstruct	(unitrec * p) {
unitrec *	q;
unitrec *	paramlist;
unitrec *	r;
strec *	d;
int32	mode;
    q = p->a;
    d = q->def;
    if (!(q->tag == j_name && d->nameid == typeid)) {
        return p;
    }
    paramlist = p->b;
    mode = d->mode;
    p->tag = j_makeconstr;
    p->a = paramlist;
    p->b = 0;
    r = createunit1(j_convert,p);
    r->valuemode = mode;
    return r;
}

static void converteqeq	(unitrec * p) {
int32	leftop;
int32	rightop;
unitrec *	w;
unitrec *	y1;
unitrec *	y2;
unitrec *	z;
    if (!condopset[p->tag]) {
        return;
    }
    if (!condopset[p->a->tag]) {
        return;
    }
    w = p->a;
    y1 = w->b;
    y2 = y1;
    z = p->b;
    leftop = w->tag;
    rightop = p->tag;
    p->tag = j_andl;
    p->b = createunit2(rightop,y2,z);
    converteqeq(w);
    converteqeq(y2);
    converteqeq(z);
}

static unitrec * readapplyop	(int32 inexpr) {
unitrec *	p;
unitrec *	a;
unitrec *	b;
    lex();
    checksymbol(lbracksym);
    lex();
    p = readexpression();
    checksymbol(commasym);
    lex();
    a = readexpression();
    b = 0;
    if (lx.symbol == commasym) {
        lex();
        b = readexpression();
    }
    checksymbol(rbracksym);
    lex();
    return createunit3((inexpr?j_applyopx:j_applyop),p,a,b);
}


// From module: qc_pcllib
void initpcl	(int32 size) {
int32	i;
int32	j;
int32	nn;
    npccode = size*2;
    pccode = (int64 (*)[])pcm_alloc((npccode+16)*8);
    linetable = (uint16 (*)[])pcm_allocz(npccode*2);
    pcindex = 0;
    mlineno = 0;
    for (i=1; i<=klastcmd; ++i) {
        nn = 0;
        for (j=1; j<=4; ++j) {
            if (cmdfmt[i][j-1] == 0) {
                goto L8;
            }
            ++nn;
        }
L8:;
        cmdnopnds[i] = nn;
    }
}

void initpcldata	(void) {
int32	i;
int32	j;
int32	nn;
    for (i=1; i<=klastcmd; ++i) {
        nn = 0;
        for (j=1; j<=4; ++j) {
            if (cmdfmt[i][j-1] == 0) {
                goto L16;
            }
            ++nn;
        }
L16:;
        cmdnopnds[i] = nn;
    }
}

void initpclgen	(void) {
int32	i;
    for (i=1; i<=999; ++i) {
        labeltable[i-1] = i+1;
    }
    labeltable[maxlabels-1] = 0;
    nextfreelabel = 1;
}

static void writepcl3	(int32 pc) {
char	str[512];
byte	fmt[4];
int32	cmdcode;
int32	a;
int32	needcomma;
int32	i;
int32	lineno;
int64 *	ptr;
strec *	d;
    ptr = &(*pccode)[pc-1];
    cmdcode = *ptr++;
    memcpy((int32 *)&fmt,(int32 *)&cmdfmt[cmdcode],4);
    lineno = (*linetable)[pc];
    sprintf((char *)&str,"%5d: %05d ",lineno,pc);
    gs_str(pcl,(char *)&str);
    if ((cmdcode==kprocstart)) {
        gs_str(pcl,"PROC:");
        d = (strec *)(int32)*ptr;
        gs_str(pcl,d->name);
        gs_strln(pcl,":");
        return;
    } else if ((cmdcode==kprocend)) {
        gs_line(pcl);
        return;
    }
    if (!!(*labelmap)[pc-1]) {
        sprintf((char *)&str,"L%d:",pc);
        gs_strln(pcl,(char *)&str);
        gs_str(pcl,"             ");
    }
    strcpy((char *)&str,cmdnames[cmdcode]+1);
    a = 1;
    if ((cmdcode==kcallhost)) {
        a = 2;
        strcat((char *)&str,".");
        strcat((char *)&str,hostfnnames[*ptr++]+5);
    }
    gs_leftstr(pcl," ",11,'-');
    gs_leftstr(pcl,(char *)&str,23,32);
    gs_str(pcl,"     ");
    needcomma = 0;
    for (i=a; i<=4; ++i) {
        if ((fmt[i-1]==cnone)) {
            goto L27;
        }
        else {
            if (needcomma) {
                gs_str(pcl,", ");
            }
            strcpy((char *)&str,writepclopnd3(fmt[i-1],*ptr++,i,cmdcode));
            gs_str(pcl,(char *)&str);
            needcomma = 1;
        }
    }
L27:;
    gs_line(pcl);
}

static char * writepclopnd3	(int32 fmt,int64 x,int32 n,int32 cmdcode) {
static char	str[512];
static char	str2[512];
strec *	d;
char *	suffix;
int32	slen;
    d = (strec *)(int32)x;
    if ((fmt==cnone)) {
        return "None";
    } else if ((fmt==cint32) || (fmt==cint) || (fmt==cword) || (fmt==creal)) {
        if ((fmt==cint32)) {
            suffix = "i";
        } else if ((fmt==cint)) {
            suffix = "d";
        } else if ((fmt==cword)) {
            suffix = "w";
        }
        else {
            suffix = "";
        }
        sprintf((char *)&str,"%lld%s",x,suffix);
    } else if ((fmt==crange)) {
        sprintf((char *)&str,"%lld..%lld",x&4294967295ll,x>>32);
    } else if ((fmt==cstring)) {
        slen = strlen((char *)(int32)x);
        if (slen >= 255) {
            slen = 255;
        }
        memcpy((int32 *)&str,(int32 *)(char *)(int32)x,slen);
        str[slen+1-1] = 0;
        convertstring((char *)&str,(char *)&str2);
        sprintf((char *)&str,"\"%s\"",&str2);
    } else if ((fmt==cmemory)) {
        strcpy((char *)&str,">>[");
        strcat((char *)&str,getdottedname(d));
        strcat((char *)&str,"]");
    } else if ((fmt==cframe)) {
        sprintf((char *)&str,"[%s:%d]",getdottedname(d),d->index);
    } else if ((fmt==cproc)) {
        sprintf((char *)&str,"[&%s] %d",getdottedname(d),d->index);
    } else if ((fmt==cdllproc)) {
        sprintf((char *)&str,"[DLL:%s %p]",getdottedname(d),d);
    } else if ((fmt==cgenfield)) {
        sprintf((char *)&str,"GENFIELD:%d",(int32)x);
    } else if ((fmt==cfield)) {
        sprintf((char *)&str,".%s",d->name);
    } else if ((fmt==ctype)) {
        sprintf((char *)&str,"T:%s <%d>",ttname[x],(int32)x);
    } else if ((fmt==clabel)) {
        sprintf((char *)&str,"L%d",(int32)x);
    } else if ((fmt==coperator)) {
        sprintf((char *)&str,"OP:%s",cmdnames[x]);
    }
    else {
        sprintf((char *)&str,"<%d %s>",fmt,opndnames[fmt]);
    }
    return (char *)&str;
}

strbuffer * writepccode	(char * caption,int32 n) {
int32	cmd;
int32	pc;
int32	i;
int32	lastline;
int32	line;
int32	lab;
    gs_init(pcl);
    gs_str(pcl,"PROC ");
    gs_str(pcl,caption);
    gs_str(pcl,"/MODULE:");
    gs_str(pcl,moduletable[n].name);
    gs_str(pcl,"/");
    gs_strint(pcl,n);
    gs_str(pcl,"/");
    gs_strint(pcl,moduletable[n].pcindex);
    gs_strln(pcl,":");
    gs_line(pcl);
    pccode = moduletable[n].pccode;
    pcindex = moduletable[n].pcindex;
    pc = 1;
    linetable = moduletable[n].linetable;
    labelmap = (byte (*)[])zalloctable(pcindex,4);
    while (pc <= pcindex) {
        cmd = (*pccode)[pc-1];
        if (cmdfmt[cmd][1-1] == clabel) {
            lab = (*pccode)[pc+1-1];
            (*labelmap)[lab-1] = 1;
        }
        pc += cmdnopnds[cmd]+1;
    }
    pc = 1;
    while (pc <= pcindex) {
        cmd = (*pccode)[pc-1];
        writepcl3(pc);
        pc += cmdnopnds[cmd]+1;
    }
    gs_line(pcl);
    return pcl;
}

void genpc	(int32 opc) {
    if (pcindex >= npccode) {
        printf("%s %d %d\n","PCINDEX=",pcindex,npccode);
        gerror("pccode overflow",0);
    }
    (*pccode)[++pcindex-1] = opc;
    lastopc = &(*pccode)[pcindex-1];
    (*linetable)[pcindex] = mlineno;
}

void genopnd_int	(int64 x) {
    (*pccode)[++pcindex-1] = x;
}

void genopnd_s	(strec * d) {
    (*pccode)[++pcindex-1] = (int64)(int32)d;
}

void genpc_int	(int32 opc,int64 a) {
    genpc(opc);
    (*pccode)[++pcindex-1] = a;
}

void genpc_int2	(int32 opc,int64 a,int64 b) {
    genpc(opc);
    (*pccode)[++pcindex-1] = a;
    (*pccode)[++pcindex-1] = b;
}

void genpc_int4	(int32 opc,int64 a,int64 b,int64 c,int64 d) {
    genpc(opc);
    genopnd_int(a);
    genopnd_int(b);
    genopnd_int(c);
    genopnd_int(d);
}

void genpc_s	(int32 opc,strec * d) {
    genpc(opc);
    (*pccode)[++pcindex-1] = (int64)(int32)d;
}

void genpc_str	(int32 opc,char * s,int32 length) {
    ++totalstrings;
    genpc(opc);
    genopnd_str(s,length);
}

void genopnd_str	(char * s,int32 length) {
    ++totalstrings;
    genopnd_int((int64)(int32)s);
}

void genpc_lab	(int32 opc,int32 a) {
int32	lastpc;
    genpc(opc);
    if (a >= 0) {
        (*pccode)[++pcindex-1] = a;
        return;
    }
    a = -a;
    lastpc = labeltable[a-1];
    (*pccode)[++pcindex-1] = lastpc;
    labeltable[a-1] = pcindex;
}

int32 isframe_s	(strec * p) {
    return p->attribs.ax_frame;
}

void converttype	(int32 m) {
strec *	d;
int32	first;
int32	a;
int32	b;
int32	nbits;
int32	recordsize;
int32	index;
enum {maxfield = 256};
strec *	fieldlist[256];
int32	nofields;
    if (!!ttsize[m]) {
        return;
    }
    if ((ttbasetype[m]==tstring) || (ttbasetype[m]==tstringz)) {
        ttsize[m] = ttlength[m];
    } else if ((ttbasetype[m]==tset)) {
        ttsize[m] = ttlength[m]/ 32;
        if (ttsize[m]*32 < ttlength[m]) {
            ++ttsize[m];
        }
    } else if ((ttbasetype[m]==tarray)) {
        if ((ttbasetype[m]==tbit) || (ttbasetype[m]==tbit2) || (ttbasetype[m]==tbit4)) {
            nbits = ttlength[m]*ttbitwidth[tttarget[m]];
            ttsize[m] = (nbits-1)/ 8+1;
        }
        else {
            converttype(tttarget[m]);
            ttsize[m] = ttlength[m]*ttsize[tttarget[m]];
        }
    } else if ((ttbasetype[m]==tenum)) {
        first = 1;
        a = b = 0;
        d = ttnamedef[m]->deflist;
        while (d) {
            if (d->nameid == enumid) {
                if (first) {
                    first = 0;
                    a = b = d->index;
                }
                else {
                    a = m_imin(a,d->index);
                    b = m_imax(b,d->index);
                }
            }
            d = d->nextdef;
        }
        ttlower[m] = a;
        ttlength[m] = (b-a)+1;
    } else if ((ttbasetype[m]==trefpacked)) {
        converttype(tttarget[m]);
        ttsize[m] = 8;
    } else if ((ttbasetype[m]==trecord)) {
        nofields = 0;
        d = ttnamedef[m]->deflist;
        while (d) {
            if (d->nameid == fieldid) {
                if (ttbasetype[d->mode] > tvariant) {
                    printf("%s\n",ttname[m]);
                    gerror("Packtype in record",0);
                }
                if (nofields >= maxfield) {
                    gerror("CT: too many fields",0);
                }
                fieldlist[++nofields-1] = d;
                converttype(d->mode);
            }
            d = d->nextdef;
        }
        nallfields = nfields = 0;
        recordsize = scanrecord((strec * (*)[])&fieldlist,nofields);
        ttlower[m] = 1;
        ttsize[m] = recordsize;
        ttlength[m] = nfields;
    } else if ((ttbasetype[m]==tstruct)) {
        nofields = 0;
        d = ttnamedef[m]->deflist;
        while (d) {
            if (d->nameid == fieldid) {
                if (ttbasetype[m] == tstruct) {
                    if ((ttbasetype[d->mode]==tvariant)) {
                        printf("%s\n",ttname[m]);
                        gerror("Var in struct",0);
                    } else if ((ttbasetype[d->mode]==tintm) || (ttbasetype[d->mode]==twordm) || (ttbasetype[d->mode]==trefm)) {
                        printf("%s\n",ttname[m]);
                        gerror("Intm/etc in struct",0);
                    }
                }
                else {
                    if (ttbasetype[d->mode] != tvariant) {
                        printf("%s\n",ttname[m]);
                        gerror("Packtype in record",0);
                    }
                }
                if (nofields >= maxfield) {
                    gerror("CT: too many fields",0);
                }
                fieldlist[++nofields-1] = d;
                converttype(d->mode);
            }
            d = d->nextdef;
        }
        nallfields = nfields = 0;
        index = nofields;
        recordsize = scanstruct(1,(strec * (*)[])&fieldlist,nofields,&index,0,2);
        d = ttnamedef[m];
        if ((d->attribs.ax_align==2)) {
            while (recordsize&1) {
                ++recordsize;
            }
        } else if ((d->attribs.ax_align==4)) {
            while (recordsize&3) {
                ++recordsize;
            }
        } else if ((d->attribs.ax_align==8)) {
            while (recordsize&7) {
                ++recordsize;
            }
        }
        ttlower[m] = 1;
        ttsize[m] = recordsize;
        ttlength[m] = nfields;
    }
}

static int32 scanstruct	(int32 fstruct,strec * (*flist)[],int32 flistlen,int32 * index,int32 nextoffset,int32 countmode) {
int32	startoffset;
int32	maxsize;
int32	exitflag;
int32	size;
int32	star;
int32	alignment;
strec *	d;
strec *	e;
uflagsrec	flags;
    startoffset = nextoffset;
    maxsize = 0;
    exitflag = 0;
    while (!exitflag && *index >= 1) {
        d = (*flist)[*index-1];
        flags = d->uflags;
        if (((flags).codes[1-1]=='S')) {
            shiftflagsleft(&(d)->uflags);
            size = scanstruct(1,flist,flistlen,index,nextoffset,countmode);
        } else if (((flags).codes[1-1]=='U')) {
            shiftflagsleft(&(d)->uflags);
            size = scanstruct(0,flist,flistlen,index,nextoffset,(countmode?1:0));
        } else if (((flags).codes[1-1]=='E')) {
            shiftflagsleft(&(d)->uflags);
            if (d->uflags.ulength == 0) {
                --*index;
            }
            exitflag = 1;
            size = 0;
        } else if (((flags).codes[1-1]=='*')) {
            shiftflagsleft(&(d)->uflags);
            star = 1;
            goto L93;
        }
        else {
            star = 0;
dofield:
L93:;
            if (!!d->attribs.ax_at) {
                e = d->equiv;
                d->offset = e->offset;
                size = 0;
            }
            else if (!!d->attribs.ax_equals) {
                gerror("Can't init a field",0);
            }
            else {
                size = ttsize[d->mode];
                alignment = d->attribs.ax_align;
                if (alignment == 255) {
                    alignment = (size>8?size:8);
                }
                if ((alignment==2)) {
                    while (nextoffset&1) {
                        ++nextoffset;
                    }
                } else if ((alignment==4)) {
                    while (nextoffset&3) {
                        ++nextoffset;
                    }
                } else if ((alignment==8)) {
                    while (nextoffset&7) {
                        ++nextoffset;
                    }
                }
                d->offset = nextoffset;
                if (countmode) {
                    ++nfields;
                }
            }
            if (!star) {
                --*index;
            }
            ++nallfields;
        }
        if (fstruct) {
            nextoffset += size;
        }
        else {
            maxsize = (maxsize>size?maxsize:size);
            countmode = 0;
        }
    }
    return (fstruct?(nextoffset-startoffset):maxsize);
}

static int32 scanrecord	(strec * (*flist)[],int32 flistlen) {
int32	size;
int32	index;
int32	nextoffset;
strec *	d;
strec *	e;
    nextoffset = 0;
    for (index=flistlen; index>=1; --index) {
        d = (*flist)[index-1];
        if (!!d->attribs.ax_at) {
            e = d->equiv;
            d->offset = e->offset;
            size = 0;
        }
        else if (!!d->attribs.ax_equals) {
            gerror("Can't init a field",0);
        }
        else {
            size = varsize;
            d->offset = nextoffset;
            ++nfields;
        }
        ++nallfields;
        nextoffset += size;
    }
    return nextoffset;
}

static void shiftflagsleft	(uflagsrec * flags) {
int32	i;
int32	av_1;
    if (!!flags->ulength) {
        for (i=1; i<=flags->ulength-1; ++i) {
            (flags)->codes[i-1] = (flags)->codes[i+1-1];
        }
        (flags)->codes[flags->ulength-1] = 0;
        --flags->ulength;
    }
}

static void GSTEST	(int32 id) {
int32	i;
    printf("%s %d\n","ID=",id);
    for (i=1; i<=10000; ++i) {
        gs_strln(pcl,"bartsimpson");
    }
}


// From module: qc_pclgen
static void do_tag	(unitrec * p) {
int32	oldmlineno;
int32	opc;
int32	n;
int32	m;
int32	t;
int32	lowerx;
int32	lab1;
int32	lab2;
unitrec *	a;
unitrec *	b;
unitrec *	c;
strec *	d;
strec *	owner;
int64	x;
int64	aa;
char *	s;
double	fsize;
    a = p->a;
    b = p->b;
    c = p->c;
    oldmlineno = mlineno;
    mlineno = p->lineno;
    switch (p->tag) {
    case j_print:
    case j_println:
        do_print(p,a,b);
        break;
    case j_fprint:
    case j_fprintln:
        do_fprint(p,a,b,c);
        break;
    case j_read:
    case j_readln:
        do_read(p,a,b);
        break;
    case j_assign:
    case j_deepcopy:
        do_assign(p,a,b);
        break;
    case j_to:
        do_to(p,a,b,c);
        break;
    case j_while:
        do_while(p,a,b,c);
        break;
    case j_repeat:
        do_repeat(p,a,b);
        break;
    case j_forstep:
    case j_forup:
    case j_fordown:
        do_forstep(p,a,b,c);
        break;
    case j_forall:
    case j_foreach:
    case j_forallrev:
    case j_foreachrev:
        do_forall(p,a,b,c);
        break;
    case j_do:
        do_do(p,a);
        break;
    case j_cfor:
        do_cfor(p,a,b);
        break;
    case j_if:
        do_if(p,a,b,c);
        break;
    case j_longif:
        do_longif(p,a,b);
        break;
    case j_callproc:
        do_callproc(p,a,b);
        break;
    case j_callhostproc:
    case j_callhostfn:
        do_callhostproc(p,a);
        break;
    case j_return:
        do_return(p,a);
        break;
    case j_preincr:
    case j_predecr:
    case j_postincr:
    case j_postdecr:
        do_preincr(p,a);
        break;
    case j_swap:
        evalref(a);
        evalref(b);
        genpc(kswap);
        break;
    case j_exit:
    case j_restart:
    case j_redo:
    case j_next:
        do_exit(p,a);
        break;
    case j_labeldef:
        d = p->def;
        if (d->index == 0) {
            d->index = pcindex+1;
        }
        else {
            definefwdlabel(&(d)->index);
        }
        break;
    case j_goto:
        do_goto(p,a);
        break;
    case j_stop:
        if (a) {
            do_tag(a);
        }
        else {
            genpc(kpushz_void);
        }
        genpc(kstop);
        break;
    case j_switch:
    case j_doswitch:
        do_switch(p,a,b,c);
        break;
    case j_case:
    case j_docase:
        do_case(p,a,b,c);
        break;
    case j_try:
        do_try(p,a,b);
        break;
    case j_raise:
        do_tag(a);
        genpc(kraise);
        break;
    case j_applyop:
        do_applyop(p,a,b,c);
        break;
    case j_callmproc:
        do_callmproc(p,a,b,0);
        break;
    case j_eval:
        do_tag(a);
        genpc_int(kfree,1);
        break;
    case j_const:
        x = p->value;
        switch (p->valuemode) {
        case tstring:
            s = p->svalue;
            if (p->slength == 0) {
                genpc(kpushz_str);
            }
            else {
                genpc_str(kpush_cs,s,p->slength);
            }
            break;
        case tint:
        case ti64:
            genpc_int(kpush_ci,p->value);
            break;
        case treal:
        case tr64:
            if (p->xvalue == 0.0) {
                genpc_int(kpushz,treal);
            }
            else {
                genpc_int(kpush_cr,p->value);
            }
            break;
        case tword:
        case tu64:
            genpc_int(kpush_cw,p->value);
            break;
        case trange:
            genpc_int(kpush_cn,p->value);
            break;
        default:;
            printf("%s\n",ttname[p->valuemode]);
            gerror("CONST: Can't push this type",p);
        }
        break;
    case j_name:
        d = p->def;
        switch (d->nameid) {
        case procid:
            gerror("PUSH PROC NAME",0);
            break;
        case staticid:
            genpc_s(kpush_m,d);
            break;
        case frameid:
            genpc_s(kpush_f,d);
            break;
        case paramid:
            genpc_s(kpush_f,d);
            if (!!d->attribs.ax_byrefmode) {
                genpc(kpushptr);
            }
            break;
        case labelid:
            if (d->index == 0) {
                d->index = createfwdlabel();
            }
            genpc_lab(kpush_al,d->index);
            break;
        case dllprocid:
            genpc_s(kpush_ad,d);
            break;
        default:;
            printf("%s %s\n",namenames[d->nameid],d->name);
            gerror("Name?",p);
        }
        break;
    case j_andand:
    case j_eq:
    case j_ne:
    case j_lt:
    case j_le:
    case j_gt:
    case j_ge:
    case j_add:
    case j_sub:
    case j_mul:
    case j_div:
    case j_fdiv:
    case j_ddiv:
    case j_rem:
    case j_iand:
    case j_ior:
    case j_ixor:
    case j_shl:
    case j_shr:
    case j_in:
    case j_notin:
    case j_inrev:
    case j_min:
    case j_max:
    case j_addptr:
    case j_subptr:
    case j_concat:
    case j_append:
    case j_atan2:
    case j_power:
    case j_isequal:
    case j_divrem:
dobinop:
        if (b == 0) {
            gerror("Binop: opnd missing",p);
        }
        opc = getpclop(p->tag);
        do_tag(a);
        do_tag(b);
        genpc(opc);
        break;
    case j_addto:
    case j_subto:
    case j_multo:
    case j_divto:
    case j_idivto:
    case j_fdivto:
    case j_iandto:
    case j_iorto:
    case j_ixorto:
    case j_shlto:
    case j_shrto:
    case j_minto:
    case j_maxto:
    case j_appendto:
    case j_concatto:
        opc = getpclop(p->tag);
        evalref(a);
        do_tag(b);
        genpc(opc);
        break;
    case j_idiv:
        do_idiv(a,b);
        break;
    case j_andl:
        do_and(a,b);
        break;
    case j_orl:
        do_or(a,b);
        break;
    case j_xorl:
        do_tag(a);
        if (!islogical(a)) {
            genpc(kistrue);
        }
        do_tag(b);
        if (!islogical(a)) {
            genpc(kistrue);
        }
        genpc(kixor);
        break;
    case j_ptr:
        if (a->tag == j_const) {
            gerror("pushptr/const",0);
        }
        do_tag(a);
        genpc(kpushptr);
        break;
    case j_notl:
        do_tag(a);
        if (!islogical(a)) {
            genpc(kistrue);
        }
        genpc(knot);
        break;
    case j_bytesize:
        if (a->tag == j_typeconst) {
            m = a->valuemode;
            if (ttbasetype[m] >= tbit && ttbasetype[m] <= tbit4) {
                fsize = ttbitwidth[m]/ 8.0;
                genpc_int(kpush_cr,*(int64*)&fsize);
            }
            else {
                genpushint(ttsize[m]);
            }
        }
        else {
            do_tag(a);
            genpc(getpclop(j_bytesize));
        }
        break;
    case j_bitwidth:
        if (a->tag == j_typeconst) {
            m = a->valuemode;
            genpushint(ttbitwidth[m]);
        }
        else {
            do_tag(a);
            genpc(getpclop(j_bitwidth));
        }
        break;
    case j_minvalue:
        if (a->tag == j_typeconst) {
            if ((ttbasetype[a->valuemode]==tword) || (ttbasetype[a->valuemode]==tbit) || (ttbasetype[a->valuemode]==tbit4) || (ttbasetype[a->valuemode]==tu8) || (ttbasetype[a->valuemode]==tu16) || (ttbasetype[a->valuemode]==tu32) || (ttbasetype[a->valuemode]==tu64)) {
                aa = 0;
            } else if ((ttbasetype[a->valuemode]==ti8)) {
                aa = -128;
            } else if ((ttbasetype[a->valuemode]==ti16)) {
                aa = -32768;
            } else if ((ttbasetype[a->valuemode]==ti32)) {
                aa = -2147483648ll;
            } else if ((ttbasetype[a->valuemode]==ti64) || (ttbasetype[a->valuemode]==tint)) {
                aa = -9223372036854775808ll;
            }
            else {
                goto L9;
            }
            genpushint(aa);
        }
        else {
dominval:
L9:;
            do_tag(a);
            genpc(getpclop(j_minvalue));
        }
        break;
    case j_maxvalue:
        if (a->tag == j_typeconst) {
            opc = kpush_ci;
            if ((ttbasetype[a->valuemode]==tbit)) {
                aa = 1;
            } else if ((ttbasetype[a->valuemode]==tbit2)) {
                aa = 3;
            } else if ((ttbasetype[a->valuemode]==tbit4)) {
                aa = 15;
            } else if ((ttbasetype[a->valuemode]==tu8)) {
                aa = 255;
            } else if ((ttbasetype[a->valuemode]==tu16)) {
                aa = 65535;
            } else if ((ttbasetype[a->valuemode]==tu32)) {
                aa = 4294967295ll;
            } else if ((ttbasetype[a->valuemode]==tu64) || (ttbasetype[a->valuemode]==tword)) {
                aa = -1;
                opc = kpush_cw;
            } else if ((ttbasetype[a->valuemode]==ti8)) {
                aa = 127;
            } else if ((ttbasetype[a->valuemode]==ti16)) {
                aa = 32767;
            } else if ((ttbasetype[a->valuemode]==ti32)) {
                aa = 2147483647;
            } else if ((ttbasetype[a->valuemode]==ti64) || (ttbasetype[a->valuemode]==tint)) {
                aa = 9223372036854775807ll;
            }
            else {
                goto L23;
            }
            if (opc == kpush_ci) {
                genpushint(aa);
            }
            else {
                genpc_int(opc,aa);
            }
        }
        else {
domaxval:
L23:;
            do_tag(a);
            genpc(getpclop(j_maxvalue));
        }
        break;
    case j_neg:
    case j_abs:
    case j_inot:
    case j_chr:
    case j_asc:
    case j_sqrt:
    case j_sqr:
    case j_cube:
    case j_sign:
    case j_sin:
    case j_cos:
    case j_tan:
    case j_asin:
    case j_acos:
    case j_atan:
    case j_ln:
    case j_lg:
    case j_log:
    case j_exp:
    case j_round:
    case j_floor:
    case j_ceil:
    case j_fract:
    case j_fmod:
    case j_lwb:
    case j_upb:
    case j_len:
    case j_bounds:
    case j_gettype:
    case j_getbasetype:
    case j_getelemtype:
    case j_isvoid:
    case j_isdef:
    case j_isint:
    case j_isreal:
    case j_isstring:
    case j_isrange:
    case j_islist:
    case j_isrecord:
    case j_isarray:
    case j_isset:
    case j_ispointer:
    case j_min1:
    case j_max1:
    case j_istruel:
    case j_isnone:
    case j_ismutable:
        opc = getpclop(p->tag);
        do_tag(a);
        genpc(opc);
        break;
    case j_dictitems:
        genpc(kpushz_void);
        do_tag(a);
        callhostfn(host_dictitems,1);
        break;
    case j_callfn:
        if (a->tag == j_name) {
            d = a->def;
            if (d->mode == tvoid) {
                printf("%s\n",d->name);
                gerror("Proc return value",0);
            }
        }
        do_callproc(p,a,b);
        break;
    case j_callmfn:
        do_callmproc(p,a,b,1);
        break;
    case j_preincrx:
        evalref(a);
        genpc(kincrload);
        break;
    case j_postincrx:
        evalref(a);
        genpc(kloadincr);
        break;
    case j_predecrx:
        evalref(a);
        genpc(kdecrload);
        break;
    case j_postdecrx:
        evalref(a);
        genpc(kloaddecr);
        break;
    case j_index:
    case j_slice:
        do_tag(a);
        do_tag(b);
        genpc(kpushix);
        break;
    case j_dotindex:
    case j_dotslice:
        do_tag(a);
        do_tag(b);
        genpc(kpushdotix);
        break;
    case j_byteindex:
        do_tag(a);
        do_tag(b);
        genpc_int(kpushbyteix,p->valuemode);
        break;
    case j_keyindex:
    case j_dotkeyindex:
        do_tag(b);
        do_tag(a);
        if (c) {
            do_tag(c);
            genpc(kpushkeyixd);
        }
        else {
            genpc(kpushkeyix);
        }
        break;
    case j_dot:
        do_tag(a);
        d = b->def;
        if ((d->nameid==procid) || (d->nameid==dllprocid)) {
            owner = d->owner;
            if ((owner->nameid==moduleid) || (owner->nameid==dllmoduleid)) {
                t = 0;
            }
            else {
                t = owner->mode;
            }
            genpc_int(kpushdotm,t);
            genopnd_s(d);
        } else if ((d->nameid==typeid)) {
            genpc_int2(kpushdott,d->owner->mode,d->mode);
        }
        else {
            genpc_int(kpushdot,d->offset);
        }
        break;
    case j_makelist:
    case j_makeconstr:
        if (b) {
            lowerx = getconstvalue(b,100);
        }
        else {
            lowerx = 1;
        }
        if (a == 0) {
            if (lowerx == 1) {
                genpc(kpushz_list);
            }
            else {
                genpc_int(kpushz_listl,lowerx);
            }
        }
        else {
            n = 0;
            while (a) {
                ++n;
                do_tag(a);
                a = a->nextunit;
            }
            genpc_int2(kmakelist,n,lowerx);
        }
        break;
    case j_makesetlist:
        if (a == 0) {
            genpc(kpushz_set);
        }
        else {
            n = 0;
            while (a) {
                ++n;
                do_tag(a);
                a = a->nextunit;
            }
            genpc_int(kmakeset,n);
        }
        break;
    case j_makedict:
        n = 0;
        while (a) {
            ++n;
            do_tag(a);
            a = a->nextunit;
        }
        genpc_int(kmakedict,n);
        break;
    case j_makerange:
        do_tag(a);
        do_tag(b);
        genpc(kmakerange);
        break;
    case j_assignx:
    case j_deepcopyx:
        do_assign(p,a,b);
        break;
    case j_ifx:
        lab1 = createfwdlabel();
        lab2 = createfwdlabel();
        genjumpcond(0,a,lab1);
        do_tag(b);
        genjumpl(lab2);
        definefwdlabel(&lab1);
        do_tag(c);
        genpc(knop);
        definefwdlabel(&lab2);
        break;
    case j_convert:
        do_convert(p->valuemode,a);
        break;
    case j_typepun:
        do_tag(a);
        genpc_int(ksoftconv,p->valuemode);
        break;
    case j_ptrto:
        if (a->tag == j_ptr) {
            do_tag(a->a);
        }
        else {
            evalref(a);
        }
        break;
    case j_addrof:
        evalref(a);
        genpc(kconvptr);
        break;
    case j_typeconst:
        genpc_int(kpush_t,p->valuemode);
        break;
    case j_selectx:
        do_selectx(a,b,c);
        break;
    case j_exprlist:
        while (a && !!a->nextunit) {
            do_tag(a);
            a = a->nextunit;
        }
        do_tag(a);
        break;
    case j_listcomp:
        genpc(kpushz_list);
        do_tag(a);
        break;
    case j_typeval:
        genpc_int(kpushz,p->valuemode);
        break;
    case j_keyvalue:
        do_tag(a);
        do_tag(b);
        break;
    case j_longint:
        genpc_str(kpush_cs,p->svalue,p->slength);
        genpc_int(khardconv,tlongint);
        break;
    case j_sprint:
        do_print(p,a,b);
        break;
    case j_sfprint:
        do_fprint(p,a,b,c);
        break;
    case j_clamp:
        do_clamp(a,b,c);
        break;
    case j_applyopx:
        do_applyopx(a,b,c);
        break;
    case j_operator:
        opc = getpclop(p->opcode);
        genpc_int2(kpush_op,opc,noperands);
        break;
    default:;
        printf("%s\n",jtagnames[p->tag]);
        gerror("E:CAN'T EVALUATE",p);
    }
    mlineno = oldmlineno;
}

int32 codegen	(int32 n) {
static modulerec	m;
strec *	d;
    m = moduletable[n];
    linetable = m.linetable;
    initgenpcl(m.sourcelen);
    convertalltypes();
    d = m.stmodule->deflist;
    while (d) {
        if (d->nameid == procid && getscope(d) != importscope) {
            do_procdef(d);
        }
        d = d->nextdef;
    }
    genstartproc(m.stmodule);
    genpc(kendmodule);
    m.pccode = pccode;
    m.npccode = npccode;
    m.pcindex = pcindex;
    m.linetable = linetable;
    moduletable[n] = m;
    return 1;
}

void convertalltypes	(void) {
int32	i;
    for (i=nconvertedtypes+1; i<=ntypes; ++i) {
        converttype(i);
    }
    nconvertedtypes = ntypes;
}

static void scanidata	(strec * p) {
strec *	d;
enum {maxidata = 2000};
strec *	defs[2000];
int32	i;
int32	ndefs;
    genidata(p);
    d = p->deflist;
    if (!d) {
        return;
    }
    ndefs = 0;
    while (d) {
        ++ndefs;
        if (ndefs > maxidata) {
            gerror("Too many idata defs",0);
        }
        defs[ndefs-1] = d;
        d = d->nextdef;
    }
    for (i=ndefs; i>=1; --i) {
        d = defs[i-1];
        if (!!d->attribs.ax_autovar) {
            scanidata(d);
        }
    }
    for (i=ndefs; i>=1; --i) {
        d = defs[i-1];
        if (!d->attribs.ax_autovar) {
            scanidata(d);
        }
    }
}

static void genidata	(strec * p) {
unitrec *	e;
    if (p->nameid == moduleid) {
        return;
    }
    if (getscope(p) == importscope) {
        return;
    }
    if (p->nameid == staticid) {
        e = p->code;
        if (e == 0) {
            return;
        }
        do_tag(e);
        genpc_s(kzpop_m+p->attribs.ax_frame,p);
    }
}

static void initgenpcl	(int32 sourcelen) {
strec *	dgen;
    initpcl(m_imax(sourcelen/ 2,1000));
    loopindex = 0;
    stcurrproc = 0;
    st_startproc = getduplnameptr(stmodule,addnamestr("$startproc"),procid);
    st_startproc->attribs.ax_global = 1;
    st_startproc->mode = tvoid;
    adddef_nodupl(stmodule,st_startproc);
}

void doprogramstartup	(void) {
int32	i;
strec *	d;
    initpcl(1000);
    for (i=1; i<=nmodules; ++i) {
        d = finddefstr(moduletable[i].stmodule,"$startproc");
        if (d == 0) {
            printf("%s\n",moduletable[i].name);
            gerror("Can't find $startproc",0);
        }
        genpc_s(kcall,d);
        genopnd_int(0);
    }
    stopseq = &(*pccode)[pcindex+1-1];
    genpushint(0);
    genpc(kstop);
    raiseseq = &(*pccode)[pcindex+1-1];
    genpc(kraise);
    genpc(kraise);
    genpc(kraise);
    genpc(kraise);
    genpc(kendmodule);
    moduletable[0].pccode = pccode;
    moduletable[0].npccode = npccode;
    moduletable[0].pcindex = pcindex;
    moduletable[0].linetable = linetable;
}

static void do_block	(unitrec * p) {
unitrec *	q;
    q = p->a;
    while (q) {
        do_tag(q);
        q = q->nextunit;
    }
}

static void do_print	(unitrec * p,unitrec * a,unitrec * b) {
int32	issprint;
unitrec *	x;
    issprint = (int32)(p->tag == j_sprint);
    if (issprint) {
        callhostfn(host_strstartprint,0);
    }
    else {
        if (a) {
            do_tag(a);
            callhostfn(host_startprint,0);
        }
        else {
            callhostfn(host_startprintcon,0);
        }
    }
    x = b;
    while (x) {
        if ((x->tag==j_fmtitem)) {
            do_tag(x->b);
            do_tag(x->a);
            callhostfn(host_print,0);
        } else if ((x->tag==j_nogap)) {
            callhostfn(host_printnogap,0);
        }
        else {
            genpc(kpushz_void);
            do_tag(x);
            callhostfn(host_print,0);
        }
        x = x->nextunit;
    }
    if (p->tag == j_println) {
        callhostfn(host_println,0);
    }
    if (issprint) {
        genpc(kpushz_void);
        callhostfn(host_strendprint,1);
    }
    else {
        callhostfn(host_endprint,0);
    }
}

static void do_fprint	(unitrec * p,unitrec * a,unitrec * b,unitrec * c) {
int32	issfprint;
unitrec *	x;
    issfprint = (int32)(p->tag == j_sfprint);
    if (issfprint) {
        callhostfn(host_strstartprint,0);
    }
    else {
        if (a) {
            do_tag(a);
            callhostfn(host_startprint,0);
        }
        else {
            callhostfn(host_startprintcon,0);
        }
    }
    do_tag(b);
    callhostfn(host_setformat,0);
    x = c;
    while (x) {
        if ((x->tag==j_fmtitem)) {
            do_tag(x->b);
            do_tag(x->a);
            callhostfn(host_print,0);
        } else if ((x->tag==j_nogap)) {
            callhostfn(host_printnogap,0);
        }
        else {
            genpc(kpushz_void);
            do_tag(x);
            callhostfn(host_print,0);
        }
        x = x->nextunit;
    }
    if (p->tag == j_fprintln) {
        callhostfn(host_println,0);
    }
    if (issfprint) {
        genpc(kpushz_void);
        callhostfn(host_strendprint,1);
    }
    else {
        callhostfn(host_endprint,0);
    }
}

static void do_read	(unitrec * p,unitrec * a,unitrec * b) {
unitrec *	x;
unitrec *	xloop;
    if (p->tag == j_readln) {
        if (a) {
            do_tag(a);
            callhostfn(host_readln,0);
        }
        else {
            genpc(kpushz_void);
            callhostfn(host_readln,0);
        }
    }
    xloop = b;
    while (xloop) {
        x = xloop;
        genpc(kpushz_void);
        if (x->tag == j_fmtitem) {
            do_tag(x->b);
            callhostfn(host_sread,1);
            x = x->a;
        }
        else {
            genpc(kpushz_void);
            callhostfn(host_sread,1);
        }
        if (x->tag == j_name) {
            genpc_s(kpop_m+x->def->attribs.ax_frame,x->def);
            x->def->attribs.ax_used = 1;
        }
        else {
            evalref(x);
            genpc(kpopptr);
        }
        xloop = xloop->nextunit;
    }
}

static void do_assign	(unitrec * p,unitrec * a,unitrec * b) {
int32	fstore;
int32	n;
unitrec *	q;
strec *	d;
    fstore = (int32)(p->tag == j_assignx)|(int32)(p->tag == j_deepcopyx);
    do_tag(b);
    if ((p->tag==j_deepcopy) || (p->tag==j_deepcopyx)) {
        genpc(kcopy);
    }
    switch (a->tag) {
    case j_name:
        d = a->def;
        if ((d->nameid==frameid)) {
            d->attribs.ax_used = 1;
        } else if ((d->nameid==staticid)) {
        } else if ((d->nameid==paramid)) {
            if (!!d->attribs.ax_byrefmode) {
                genpc_s(kpush_f,d);
                genpc((fstore?kstoreptr:kpopptr));
                return;
            }
        }
        else {
            printf("%s %s\n",namenames[d->nameid],d->name);
            gerror("Can't assign to",0);
        }
        genpc_s((fstore?kstore_m:kpop_m)+d->attribs.ax_frame,d);
        break;
    case j_index:
    case j_dotindex:
    case j_slice:
    case j_dotslice:
    case j_keyindex:
    case j_dotkeyindex:
    case j_byteindex:
        evalref(a);
        genpc((fstore?kstoreptr:kpopptr));
        break;
    case j_makelist:
        q = a->a;
        if (q == 0) {
            gerror("assign to ()?",0);
        }
        else {
            n = 0;
            while (q) {
                ++n;
                evalref(q);
                q = q->nextunit;
            }
            genpc_int2(kmakelist,n,1);
        }
        genpc((fstore?kstoreptr:kpopptr));
        break;
    case j_dot:
        evalref(a->a);
        d = a->b->def;
        genpc_int(kpushdotref,d->offset);
        genpc((fstore?kstoreptr:kpopptr));
        break;
    case j_ptr:
        evalref(a);
        genpc((fstore?kstoreptr:kpopptr));
        break;
    case j_ifx:
        evalref(a);
        genpc((fstore?kstoreptr:kpopptr));
        break;
    default:;
        printf("%s\n",jtagnames[a->tag]);
        gerror("DOASSIGN?",p);
    }
}

static void do_to	(unitrec * p,unitrec * a,unitrec * b,unitrec * c) {
int32	lab_a;
int32	lab_b;
int32	lab_c;
int32	lab_d;
strec *	temp;
    lab_a = definelabel();
    temp = c->def;
    do_tag(a);
    genpc_s(kzpop_f,temp);
    lab_b = createfwdlabel();
    lab_c = createfwdlabel();
    lab_d = createfwdlabel();
    stacklooplabels(&lab_a,&lab_b,&lab_c,&lab_d);
    if (a->tag != j_const) {
        do_tag(a);
        genpc_int(kpush_ci,0);
        genpc_lab(kjumple,lab_d);
    }
    else if (a->value <= 0) {
        genpc_lab(kjump,lab_d);
    }
    definefwdlabel(&lab_b);
    do_block(b);
    definefwdlabel(&lab_c);
    genpc_lab(kto_f,lab_b);
    genopnd_s(temp);
    definefwdlabel(&lab_d);
    unstacklooplabels();
}

static void do_while	(unitrec * p,unitrec * a,unitrec * b,unitrec * c) {
int32	lab_ab;
int32	lab_c;
int32	lab_d;
    lab_ab = createfwdlabel();
    lab_c = createfwdlabel();
    lab_d = createfwdlabel();
    stacklooplabels(&lab_ab,&lab_ab,&lab_c,&lab_d);
    genjumpl(lab_c);
    definefwdlabel(&lab_ab);
    do_block(b);
    definefwdlabel(&lab_c);
    genjumpcond(1,a,lab_ab);
    if (c) {
        do_block(c);
    }
    definefwdlabel(&lab_d);
    unstacklooplabels();
}

static void do_repeat	(unitrec * p,unitrec * a,unitrec * b) {
int32	lab_ab;
int32	lab_c;
int32	lab_d;
    lab_ab = definelabel();
    lab_c = createfwdlabel();
    lab_d = createfwdlabel();
    stacklooplabels(&lab_ab,&lab_ab,&lab_c,&lab_d);
    do_block(a);
    definefwdlabel(&lab_c);
    genjumpcond(0,b,lab_ab);
    definefwdlabel(&lab_d);
    unstacklooplabels();
}

static void do_forstep	(unitrec * p,unitrec * pvar,unitrec * pbody,unitrec * pautovar) {
unitrec *	pfrom;
unitrec *	pto;
unitrec *	pstep;
unitrec *	pelse;
unitrec *	plimit;
strec *	dvar;
strec *	limitvar;
int32	lab_a;
int32	lab_b;
int32	lab_c;
int32	lab_d;
int32	lab_e;
int32	opc;
int32	step;
int32	fromval;
int32	limit;
int32	jumpinto;
    pfrom = pvar->nextunit;
    pto = pfrom->nextunit;
    pstep = pto->nextunit;
    pelse = pbody->nextunit;
    dvar = pvar->def;
    dvar->attribs.ax_used = 1;
    plimit = 0;
    if ((p->tag==j_forup)) {
        step = 1;
        if (plimit) {
            limitvar = (strec *)plimit;
        }
    } else if ((p->tag==j_fordown)) {
        step = -1;
        if (plimit) {
            limitvar = (strec *)plimit;
        }
    }
    else {
        step = getconstvalue(pstep,101);
        if (step != 1 && step != -1) {
            gerror("Can't do for with odd step",0);
        }
    }
    jumpinto = 1;
    lab_a = definelabel();
    lab_b = createfwdlabel();
    lab_c = createfwdlabel();
    lab_d = createfwdlabel();
    lab_e = (pelse?createfwdlabel():lab_d);
    stacklooplabels(&lab_a,&lab_b,&lab_c,&lab_d);
    if (pfrom->tag == j_const) {
        fromval = pfrom->value;
        if (pto->tag == j_const) {
            limit = pto->value;
            pto->valuemode = tint;
            if (step == -1 && fromval >= limit || step == 1 && fromval <= limit) {
                jumpinto = 0;
            }
        }
        if (jumpinto) {
            if (step < 0) {
                ++fromval;
            }
            else {
                --fromval;
            }
            pfrom->value = fromval;
        }
        genpushint(pfrom->value);
        genpc_s(kpop_m+dvar->attribs.ax_frame,dvar);
    }
    else {
        do_tag(pfrom);
        genpc_s(kpop_m+dvar->attribs.ax_frame,dvar);
        genpc_s(((step < 0)?kincrto_m:kdecrto_m)+dvar->attribs.ax_frame,dvar);
    }
    if (pautovar) {
        if (pto->tag == j_name && !!pto->def->attribs.ax_frame) {
            pautovar = 0;
        }
    }
    if (pautovar) {
        do_tag(pto);
        limitvar = pautovar->def;
        genpc_s(kzpop_f,limitvar);
        pto = pautovar;
    }
    else {
        limitvar = pto->def;
    }
    if (jumpinto) {
        genjumpl(lab_c);
    }
    definefwdlabel(&lab_b);
    do_block(pbody);
    definefwdlabel(&lab_c);
    if (pto->tag == j_const) {
        opc = ((step < 0)?kford_fci:kfor_fci);
    }
    else {
        opc = ((step < 0)?kford_ff:kfor_ff);
    }
    genpc_lab(opc,lab_b);
    genopnd_s(dvar);
    genopnd_s(limitvar);
    if (pelse) {
        definefwdlabel(&lab_e);
        do_block(pelse);
    }
    definefwdlabel(&lab_d);
    unstacklooplabels();
}

static void do_forall	(unitrec * p,unitrec * pindex,unitrec * pbody,unitrec * pautovar) {
int32	lab_a;
int32	lab_b;
int32	lab_c;
int32	lab_d;
int32	lab_e;
int32	step;
unitrec *	pvar;
unitrec *	plist;
unitrec *	pelse;
unitrec *	q;
strec *	indexvar;
strec *	vardef;
strec *	autodef;
    pvar = pindex->nextunit;
    vardef = pvar->def;
    plist = pvar->nextunit;
    pelse = pbody->nextunit;
    indexvar = pindex->def;
    step = ((p->tag == j_forall || p->tag == j_foreach)?1:-1);
    lab_a = definelabel();
    lab_b = createfwdlabel();
    lab_c = createfwdlabel();
    lab_d = createfwdlabel();
    lab_e = (pelse?createfwdlabel():lab_d);
    stacklooplabels(&lab_a,&lab_b,&lab_c,&lab_d);
    do_tag(plist);
    genpc(kbounds);
    genpc(kexpandrange);
    autodef = pautovar->def;
    if (step == 1) {
        genpc_s(kzpop_f,autodef);
        genpc(kdecr);
        genpc_s((!!indexvar->attribs.ax_autovar?kzpop_f:kpop_f),indexvar);
    }
    else {
        genpc(kincr);
        genpc_s((!!indexvar->attribs.ax_autovar?kzpop_f:kpop_f),indexvar);
        genpc_s(kzpop_f,autodef);
    }
    genjumpl(lab_c);
    definefwdlabel(&lab_b);
    do_tag(plist);
    do_tag(pindex);
    if (p->tag == j_forall || p->tag == j_forallrev) {
        genpc(kpushix);
    }
    else {
        genpc(kpushdotix);
    }
    genpc_s(kpop_f,vardef);
    if (pbody->tag == j_block) {
        do_block(pbody);
    }
    else {
        do_tag(pbody);
    }
    definefwdlabel(&lab_c);
    genpc_lab(((step == 1)?kfor_ff:kford_ff),lab_b);
    genopnd_s(indexvar);
    genopnd_s(autodef);
    if (pelse) {
        definefwdlabel(&lab_e);
        do_block(pelse);
    }
    definefwdlabel(&lab_d);
    unstacklooplabels();
}

static void do_do	(unitrec * p,unitrec * a) {
int32	lab_abc;
int32	lab_d;
int32	lab_test;
    lab_abc = definelabel();
    lab_d = createfwdlabel();
    stacklooplabels(&lab_abc,&lab_abc,&lab_abc,&lab_d);
    do_block(a);
    genjumpl(lab_abc);
    definefwdlabel(&lab_d);
    unstacklooplabels();
}

static void do_cfor	(unitrec * p,unitrec * a,unitrec * b) {
int32	lab_a;
int32	lab_b;
int32	lab_c;
int32	lab_d;
int32	lab_test;
unitrec *	pinit;
unitrec *	pcond;
unitrec *	pstep;
    lab_a = definelabel();
    lab_b = createfwdlabel();
    lab_c = createfwdlabel();
    lab_d = createfwdlabel();
    lab_test = createfwdlabel();
    stacklooplabels(&lab_a,&lab_b,&lab_c,&lab_d);
    pinit = a;
    pcond = pinit->nextunit;
    pstep = pcond->nextunit;
    do_tag(pinit);
    genjumpl(lab_test);
    definefwdlabel(&lab_b);
    do_block(b);
    definefwdlabel(&lab_c);
    do_tag(pstep);
    definefwdlabel(&lab_test);
    genjumpcond(1,pcond,lab_b);
    definefwdlabel(&lab_d);
    unstacklooplabels();
}

static void do_if	(unitrec * p,unitrec * a,unitrec * b,unitrec * pelse) {
int32	lab1;
int32	lab2;
    lab1 = createfwdlabel();
    if (pelse) {
        lab2 = createfwdlabel();
    }
    genjumpcond(0,a,lab1);
    do_block(b);
    if (pelse) {
        genjumpl(lab2);
        definefwdlabel(&lab1);
        do_block(pelse);
        definefwdlabel(&lab2);
    }
    else {
        definefwdlabel(&lab1);
    }
}

static void do_longif	(unitrec * p,unitrec * a,unitrec * b) {
unitrec *	q;
int32	labend;
int32	lab2;
    labend = createfwdlabel();
    q = a;
    while (q) {
        lab2 = createfwdlabel();
        genjumpcond(0,q->a,lab2);
        do_block(q->b);
        q = q->nextunit;
        if (q || b) {
            genjumpl(labend);
        }
        definefwdlabel(&lab2);
    }
    if (b) {
        do_block(b);
    }
    definefwdlabel(&labend);
}

static void do_callproc	(unitrec * p,unitrec * a,unitrec * b) {
strec *	d;
strec *	pm;
unitrec *	pbody;
unitrec *	q;
unitrec *	x;
unitrec *	r;
int32	naparams;
int32	fkeyword;
int32	nparams;
int32	i;
int32	j;
int32	k;
int32	ffcode;
int32	isfn;
int32	fbyref;
unitrec *	cparams[64];
strec *	dparams[64];
int32	dparamsdone=0;
char *	name;
int64 *	pc;
    if (a->tag != j_name) {
        do_callptr(p,a,b);
        return;
    }
    d = a->def;
    if ((d->nameid==procid) || (d->nameid==dllprocid)) {
    }
    else {
        printf("%s\n",d->name);
        gerror("Callproc: not proc",p);
    }
    pbody = d->code;
    if ((ffcode = d->attribs.ax_fflang==windowsff) || (ffcode = d->attribs.ax_fflang==clangff) || (ffcode = d->attribs.ax_fflang==mlangff)) {
        do_calldll(p,a,b);
        return;
    }
    isfn = (int32)(d->mode != tvoid);
    if (isfn) {
        genpc(kpushz_void);
    }
    nparams = d->attribs.ax_nparams;
    memset((int32 *)&cparams,0,4*nparams);
    fkeyword = 0;
    x = b;
    naparams = 0;
    while (x) {
        ++naparams;
        if (naparams > nparams) {
            printf("%s %d %d\n",d->name,naparams,nparams);
            gerror("Too many params",p);
        }
        if (fkeyword && x->tag != j_keyword) {
            gerror("Normal param follows keyword param",p);
        }
        if ((x->tag==j_keyword)) {
            if (!(x->a->tag == j_name)) {
                gerror("Kwd: not name",0);
            }
            fkeyword = 1;
            name = x->a->def->name;
            if (!dparamsdone) {
                extractparams(d,(strec * (*)[])&dparams);
                dparamsdone = 1;
            }
            j = 0;
            k = 0;
            for (j=1; j<=nparams; ++j) {
                if (strcmp(dparams[j-1]->name,name) == 0) {
                    k = j;
                    goto L106;
                }
            }
L106:;
            if (k == 0) {
                printf("%s %s %s\n",name,"in",d->name);
                gerror("Can't find keyword param",0);
            }
            if (!!cparams[k-1]) {
                printf("%s\n",name);
                gerror("Param already set",0);
            }
            cparams[k-1] = x->b;
        } else if ((x->tag==j_null)) {
        }
        else {
            cparams[naparams-1] = x;
        }
        x = x->nextunit;
    }
    for (i=1; i<=nparams; ++i) {
        x = cparams[i-1];
        if (x == 0) {
            if (!dparamsdone) {
                extractparams(d,(strec * (*)[])&dparams);
                dparamsdone = 1;
            }
            pm = dparams[i-1];
            if (!!pm->code) {
                cparams[i-1] = pm->code;
            }
            else if (!pm->attribs.ax_optional) {
                printf("%s %s %s\n",pm->name,"in",d->name);
                gerror("Param not optional",0);
            }
        }
    }
    for (i=nparams; i>=1; --i) {
        x = cparams[i-1];
        if (!dparamsdone) {
            extractparams(d,(strec * (*)[])&dparams);
            dparamsdone = 1;
        }
        fbyref = dparams[i-1]->attribs.ax_byrefmode;
        if (x == 0) {
            if (fbyref) {
                gerror("&void param",0);
            }
            genpc(kpushz_void);
        }
        else {
            if (fbyref) {
                evalref(x);
            }
            else {
                do_tag(x);
            }
        }
    }
    genpc_s(kcall,d);
    genopnd_int(0);
    for (i=1; i<=nparams; ++i) {
        r = cparams[i-1];
        if (r == 0 || !!issimpleparam(r)) {
            if ((*lastopc==kaddsp)) {
                ++*(lastopc+1);
            } else if ((*lastopc==kcall)) {
                if (*(lastopc+2) < 8) {
                    ++*(lastopc+2);
                }
                else {
                    genpc_int(kaddsp,1);
                }
            }
            else {
                genpc_int(kaddsp,1);
            }
        }
        else {
            if ((*lastopc==kfree)) {
                ++*(lastopc+1);
            }
            else {
                genfree(1);
            }
        }
    }
    if (isfn && p->tag == j_callproc) {
        genfree(1);
    }
}

static void do_callhostproc	(unitrec * p,unitrec * a) {
int32	calledasfn;
int32	isfn;
int32	index;
int32	nap;
int32	i;
int32	nparams;
int32	fparams;
unitrec *	plist[10];
unitrec *	q;
int32	av_1;
    calledasfn = (int32)(p->tag == j_callhostfn);
    index = p->opcode;
    isfn = hostisfn[index];
    if (calledasfn && !isfn) {
        gerror("Host proc is not function",0);
    }
    if (isfn && index == host_getparam && a && a->nextunit == 0 && a->tag == j_const && a->value == 0) {
        genpushint(nprocparamvars);
        return;
    }
    if (isfn) {
        genpc(kpushz_void);
    }
    q = a;
    nap = 0;
    while (q) {
        if (nap >= 10) {
            gerror("far too many host params",0);
        }
        ++nap;
        plist[nap-1] = q;
        q = q->nextunit;
    }
    if (index == host_allparams && a == 0) {
        nparams = 1;
    }
    else {
        nparams = nap;
    }
    if (nparams == 0 && !!hostlvset[index]) {
        gerror("LV hostfn: needs 1+ params",0);
    }
    fparams = hostnparams[index];
    if (nparams > fparams) {
        gerror("Hostfn too many params",0);
    }
    av_1 = fparams-nparams;
    while (av_1--) {
        genpc(kpushz_void);
    }
    for (i=nparams; i>=1; --i) {
        if (i == 1 && !!hostlvset[index]) {
            evalref(plist[i-1]);
        }
        else if (i == 1 && index == host_allparams && nap == 0) {
            isfn = (int32)(stcurrproc->mode != tvoid);
            genpc_s(kpush_ap,stcurrproc);
        }
        else {
            do_tag(plist[i-1]);
        }
    }
    callhostfn(index,calledasfn);
}

static void do_return	(unitrec * p,unitrec * a) {
int32	isfn;
    isfn = (int32)(stcurrproc->mode != tvoid);
    if (a == 0) {
        if (isfn) {
            gerror("Fn needs return value",p);
        }
        if (trylevel) {
            genpc_int(kaddsp,trylevel);
        }
        if (nprocframevars) {
            genjumpl(retindex);
        }
        else {
            genpc(kreturn);
        }
        return;
    }
    if (!isfn) {
        printf("%s\n",stcurrproc->name);
        gerror("Can't return value from proc",0);
    }
    do_tag(a);
    genpc_s(kzpop_f,stretval);
    if (trylevel) {
        genpc_int(kaddsp,trylevel);
    }
    if (nprocframevars) {
        genjumpl(retindex);
    }
    else {
        genpc(kreturn);
    }
}

static void genstartproc	(strec * dmodule) {
int32	retadjust;
int32	lab1;
int32	lab2;
strec *	stmain;
strec *	ststart;
    if (st_startproc == 0) {
        gerror("$startproc not present",0);
    }
    retadjust = 0;
    genpc_s(kprocstart,st_startproc);
    genopnd_int(0);
    st_startproc->index = pcindex+1;
    scanidata(dmodule);
    stmain = finddefstr(stmodule,"main");
    ststart = finddefstr(stmodule,"start");
    if (stmain && ststart) {
        lab1 = createfwdlabel();
        lab2 = createfwdlabel();
        genpc(kpushz_void);
        genpc_str(kpush_cs,stmodule->name,stmodule->namelen);
        callhostfn(host_ismain,1);
        genpc_lab(kjumpfalse,lab1);
        genpc_s(kcall,stmain);
        genopnd_int(0);
        genpc_lab(kjump,lab2);
        definefwdlabel(&lab1);
        genpc_s(kcall,ststart);
        genopnd_int(0);
        definefwdlabel(&lab2);
    }
    else if (stmain) {
        lab1 = createfwdlabel();
        genpc(kpushz_void);
        genpc_str(kpush_cs,stmodule->name,stmodule->namelen);
        callhostfn(host_ismain,1);
        genpc_lab(kjumpfalse,lab1);
        genpc_s(kcall,stmain);
        genopnd_int(0);
        definefwdlabel(&lab1);
    }
    else if (ststart) {
        genpc_s(kcall,ststart);
        genopnd_int(0);
    }
    genpc(kreturn);
    genpc(kprocend);
}

static void do_procdef	(strec * p) {
int32	nfreevars;
int32	nnofreevars;
    if (p == st_startproc) {
        return;
    }
    stcurrproc = p;
    retindex = createfwdlabel();
    genprocentry(p,&nfreevars,&nnofreevars);
    if (p->code == 0) {
        printf("%s %s %s\n","EMPTY PROC BODY",p->name,scopenames[getscope(p)]);
    }
    else {
        do_block(p->code);
    }
    if (p->mode != tvoid) {
        if (p->owner->nameid != typeid) {
            if (!checkblockreturn(p->code)) {
                printf("%s\n",p->name);
                gerror("Function needs explicit return statement",p->code);
            }
        }
    }
    definefwdlabel(&retindex);
    genprocexit(nfreevars,nnofreevars);
    genpc(kprocend);
}

static void genprocentry	(strec * p,int32 * nfreevars,int32 * nnofreevars) {
int32	nparamvars;
int32	nframevars;
int32	isfn;
int32	hasretval;
int32	fv;
int32	nallocvars;
int32	ninitvars;
int32	i;
int32	j;
int32	nextoffset;
strec *	d;
strec *	varlist[64];
int32	fvlist[64];
unitrec *	expr;
strec *	locals[256];
int32	nlocals;
    d = p->deflist;
    isfn = (int32)(p->mode != tvoid);
    if (isfn) {
        stretval = getduplnameptr(p,addnamestr("$retval"),paramid);
        stretval->mode = p->mode;
        stretval->attribs.ax_autovar = 1;
        adddef_nodupl(p,stretval);
    }
    else {
        stretval = 0;
    }
    nparamvars = nframevars = 0;
    hasretval = 0;
    d = p->deflist;
    nlocals = 0;
    while (d) {
        if (nlocals >= maxlocals) {
            gerror("Too many locals",0);
        }
        locals[++nlocals-1] = d;
        d = d->nextdef;
    }
    for (i=nlocals; i>=1; --i) {
        d = locals[i-1];
        if ((d->nameid==frameid)) {
            if (nframevars >= maxparams) {
                mlineno = d->lineno;
                printf("%s %s %d\n",p->name,d->name,nframevars);
                gerror("Too many frame vars",0);
            }
            ++nframevars;
            varlist[nframevars-1] = d;
            fvlist[nframevars-1] = 0;
        } else if ((d->nameid==paramid)) {
            d->index = ++nparamvars;
            if (!!d->attribs.ax_autovar) {
                hasretval = 1;
            }
        }
    }
    nprocframevars = nframevars;
    nprocparamvars = nparamvars-isfn;
    nallocvars = ninitvars = 0;
    *nfreevars = *nnofreevars = 0;
    for (i=1; i<=nframevars; ++i) {
        d = varlist[i-1];
        expr = 0;
        if (!!d->code) {
            expr = d->code;
        }
        if (!!d->attribs.ax_autovar) {
            fv = 0;
            ++nallocvars;
            ++*nnofreevars;
        }
        else if (expr) {
            fv = 1;
            ++nallocvars;
            ++*nfreevars;
        }
        else {
            fv = 2;
            ++ninitvars;
            ++*nfreevars;
        }
        fvlist[i-1] = fv;
    }
    nextoffset = 0;
    for (i=2; i>=0; --i) {
        for (j=1; j<=nframevars; ++j) {
            if (fvlist[j-1] == i) {
                nextoffset -= 1;
                varlist[j-1]->index = nextoffset;
            }
        }
    }
    genpc_s(kprocstart,p);
    genopnd_int((nparamvars-hasretval));
    p->index = pcindex+1;
    if (!!(ninitvars+nallocvars)) {
        genpc_int(kstackframe,(ninitvars+nallocvars));
    }
    for (i=1; i<=nframevars; ++i) {
        d = varlist[i-1];
        if (!!d->code) {
            do_tag(d->code);
            d->attribs.ax_used = 1;
            genpc_s(kzpop_f,d);
        }
    }
}

static void genprocexit	(int32 nfree,int32 nnofree) {
    if (nnofree) {
        genpc_int(kaddsp,nnofree);
    }
    if (nfree) {
        genfree(nfree);
    }
    genpc(kreturn);
}

static void do_preincr	(unitrec * p,unitrec * a) {
int32	isincr;
    isincr = (int32)(p->tag == j_preincr)|(int32)(p->tag == j_postincr);
    if (a->tag == j_name && a->def->nameid != paramid) {
        genpc_s((isincr?kincrto_m:kdecrto_m)+a->def->attribs.ax_frame,a->def);
    }
    else {
        evalref(a);
        genpc((isincr?kincrptr:kdecrptr));
    }
}

static void do_exit	(unitrec * p,unitrec * a) {
int32	k;
int32	index;
int32	n;
    if ((p->tag==j_restart)) {
        k = 1;
    } else if ((p->tag==j_redo)) {
        k = 2;
    } else if ((p->tag==j_next)) {
        k = 3;
    } else if ((p->tag==j_exit)) {
        k = 4;
    }
    if (a) {
        index = a->value;
    }
    else {
        index = 1;
    }
    n = findlooplabel(k,index);
    if (n == 0) {
        gerror("Bad exit/loop index",p);
    }
    else {
        if (trylevel > looptrylevel) {
            genpc_int(kaddsp,(trylevel-looptrylevel));
        }
        genjumpl(n);
    }
}

static void do_goto	(unitrec * p,unitrec * a) {
strec *	d;
int32	ntries;
int32	lab;
    if ((a->tag==j_name)) {
        d = a->def;
        if (d->index == 0) {
            d->index = createfwdlabel();
        }
        if ((d->nameid==labelid)) {
            ntries = trylevel-d->offset;
            if (ntries < 0) {
                gerror("Jumping into try block",0);
            }
            else if (ntries) {
                genpc_int(kaddsp,ntries);
            }
            genpc_lab(kjump,d->index);
        }
        else {
            printf("%s\n",d->name);
            gerror("Not label name",0);
        }
    }
    else {
        gerror("GOTO PTR",0);
    }
}

static void do_switch	(unitrec * p,unitrec * pindex,unitrec * pwhenthen,unitrec * pelse) {
int32	minlab;
int32	maxlab;
int32	x;
int32	y;
int32	i;
int32	n;
unitrec *	w;
unitrec *	wt;
    minlab = 1000000;
    maxlab = -1000000;
    n = 0;
    wt = pwhenthen;
    while (wt) {
        w = wt->a;
        while (w) {
            if ((w->tag==j_const)) {
                if ((w->valuemode==trange)) {
                    x = w->range_lower;
                    y = w->range_upper;
dorange:
L188:;
                    for (i=x; i<=y; ++i) {
                        minlab = (minlab<i?minlab:i);
                        maxlab = (maxlab>i?maxlab:i);
                    }
                } else if ((w->valuemode==tint)) {
                    x = y = w->value;
                    goto L188;
                }
                else {
                    gerror("Switch when1: not const int",w);
                }
            } else if ((w->tag==j_typeconst)) {
                x = y = w->valuemode;
                goto L188;
            }
            else {
                printf("%s %p %s\n","STREXPR(W)=",(void*)(strexpr(w)),jtagnames[w->tag]);
                gerror("Switch when2: not const",w);
            }
            w = w->nextunit;
        }
        wt = wt->nextunit;
    }
    if (maxlab-minlab <= maxswitchrange) {
        do_simpleswitch(p,pindex,pwhenthen,pelse,minlab,maxlab);
        return;
    }
    gerror("COMPLEX SWITCH/NOT COMPLETE",0);
}

static void do_simpleswitch	(unitrec * p,unitrec * pindex,unitrec * pwhenthen,unitrec * pelse,int32 a,int32 b) {
unitrec *	w;
unitrec *	wt;
unitrec *	q;
int32	loopsw;
int32	n;
int32	offset;
int32	x;
int32	y;
int32	x0;
int32	i;
int32	labstmt;
int32	elselab;
int32	labels[513];
int32	lab_a;
int32	lab_b;
int32	lab_c;
int32	lab_d;
    loopsw = (int32)(p->tag == j_doswitch);
    n = (b-a)+1;
    offset = a-1;
    if (loopsw) {
        lab_a = definelabel();
        lab_d = createfwdlabel();
        stacklooplabels(&lab_a,&lab_a,&lab_a,&lab_d);
    }
    else {
        lab_d = createfwdlabel();
    }
    do_tag(pindex);
    genpc_int2(kswitch,n,a);
    for (i=1; i<=n; ++i) {
        genpc_lab(kjumplabel,0);
        labels[i-1] = pcindex;
    }
    genpc_lab(kjumplabel,0);
    labels[n+1-1] = pcindex;
    wt = pwhenthen;
    while (wt) {
        labstmt = definelabel();
        w = wt->a;
        while (w) {
            if ((w->tag==j_const)) {
                if (w->valuemode == trange) {
                    x0 = w->range_lower;
                    y = w->range_upper;
                }
                else {
                    x0 = y = w->value;
                }
            } else if ((w->tag==j_typeconst)) {
                x0 = y = w->valuemode;
            }
            for (x=x0; x<=y; ++x) {
                i = x-offset;
                if (!!(*pccode)[labels[i-1]-1]) {
                    printf("%d %c\n",x,(char)x);
                    gerror("Dupl switch value",0);
                }
                (*pccode)[labels[i-1]-1] = labstmt;
            }
            w = w->nextunit;
        }
        do_block(wt->b);
        if (!loopsw) {
            genjumpl(lab_d);
        }
        else {
            genjumpl(lab_a);
        }
        wt = wt->nextunit;
    }
    if (pelse) {
        if (pelse->nextunit == 0) {
            q = pelse;
        }
        else {
            q = 0;
        }
        if (loopsw && q && q->tag == j_exit && (q->a == 0 || getconstvalue(q->a,102) == 1)) {
            elselab = lab_d;
            pelse = 0;
        }
        else {
            elselab = createfwdlabel();
        }
    }
    else {
        elselab = (loopsw?lab_a:lab_d);
    }
    if (pelse) {
        definefwdlabel(&elselab);
        do_block(pelse);
    }
    for (i=1; i<=n; ++i) {
        if ((*pccode)[labels[i-1]-1] == 0) {
            (*pccode)[labels[i-1]-1] = elselab;
        }
    }
    (*pccode)[labels[n+1-1]-1] = elselab;
    if (loopsw) {
        genjumpl(lab_a);
        definefwdlabel(&lab_d);
        unstacklooplabels();
    }
    else {
        definefwdlabel(&lab_d);
    }
}

static void do_case	(unitrec * p,unitrec * pindex,unitrec * pwhenthen,unitrec * pelse) {
int32	lab_a;
int32	lab_d;
int32	loopsw;
int32	fmult;
int32	labnextwhen;
int32	labstmtstart;
unitrec *	w;
unitrec *	wt;
    loopsw = (int32)(p->tag == j_docase);
    if (loopsw) {
        lab_a = definelabel();
        lab_d = createfwdlabel();
        stacklooplabels(&lab_a,&lab_a,&lab_a,&lab_d);
    }
    else {
        lab_d = createfwdlabel();
    }
    do_tag(pindex);
    wt = pwhenthen;
    while (wt) {
        w = wt->a;
        fmult = (int32)(w->nextunit != 0);
        labnextwhen = createfwdlabel();
        if (fmult) {
            labstmtstart = createfwdlabel();
        }
        while (w) {
            do_tag(w);
            w = w->nextunit;
            if (w) {
                genpc_lab(kjumptesteq,labstmtstart);
            }
            else {
                genpc_lab(kjumptestne,labnextwhen);
            }
        }
        if (fmult) {
            definefwdlabel(&labstmtstart);
        }
        do_block(wt->b);
        if (!loopsw) {
            genjumpl(lab_d);
        }
        else {
            genjumpl(lab_a);
        }
        definefwdlabel(&labnextwhen);
        wt = wt->nextunit;
    }
    genfree(1);
    if (pelse) {
        do_block(pelse);
    }
    if (loopsw) {
        genjumpl(lab_a);
        definefwdlabel(&lab_d);
        unstacklooplabels();
    }
    else {
        definefwdlabel(&lab_d);
    }
}

static void do_try	(unitrec * p,unitrec * a,unitrec * b) {
int32	labend;
int32	labx;
unitrec *	ptry;
unitrec *	x;
unitrec *	pexcept;
unitrec *	pexcode;
    ++trylevel;
    labend = createfwdlabel();
    ptry = a;
    labx = createfwdlabel();
    pexcept = b;
    if (pexcept == 0) {
        gerror("try: no except",0);
    }
    else if (!!pexcept->nextunit) {
        gerror("Try:multiple except block not implemented",0);
    }
    while (pexcept) {
        pexcode = pexcept->a;
        if (pexcode == 0 || !!pexcode->nextunit) {
            gerror("Try:multiple except codes not implemented",0);
        }
        genpc_lab(kpush_try,labx);
        genopnd_int(getconstvalue(pexcode,103));
        genopnd_int(1);
        do_block(ptry);
        genjumpl(labend);
        definefwdlabel(&labx);
        do_block(pexcept->b);
        definefwdlabel(&labend);
        pexcept = pexcept->nextunit;
    }
    genpc_int(kaddsp,1);
    --trylevel;
}

static void do_applyop	(unitrec * p,unitrec * a,unitrec * b,unitrec * c) {
int32	lab;
    if (c) {
        evalref(b);
        do_tag(c);
        do_tag(a);
        genpc_int(kapplyop,2);
    }
    else {
        evalref(b);
        do_tag(a);
        genpc_int(kapplyop,1);
    }
    lab = createfwdlabel();
    genpc_lab(kjump,lab);
    genpc(knop);
    definefwdlabel(&lab);
}

static void evalref	(unitrec * p) {
strec *	d;
int32	lab1;
int32	lab2;
    switch (p->tag) {
    case j_const:
        gerror("ref on const",0);
        break;
    case j_name:
        d = p->def;
        if ((d->nameid==procid)) {
            genpc_s(kpush_ap,d);
        } else if ((d->nameid==staticid)) {
            genpc_s(kpush_am,d);
        } else if ((d->nameid==frameid)) {
            genpc_s(kpush_af,d);
            d->attribs.ax_used = 1;
        } else if ((d->nameid==paramid)) {
            if (!!d->attribs.ax_byrefmode) {
                genpc_s(kpush_f,d);
            }
            else {
                genpc_s(kpush_af,d);
            }
        } else if ((d->nameid==labelid)) {
            if (d->index == 0) {
                d->index = createfwdlabel();
            }
            genpc_lab(kpush_al,d->index);
        } else if ((d->nameid==dllprocid)) {
            genpc_s(kpush_ad,d);
        }
        else {
            printf("%s\n",namenames[d->nameid]);
            gerror("&name",0);
        }
        break;
    case j_index:
    case j_slice:
        evalref(p->a);
        do_tag(p->b);
        genpc(kpushixref);
        break;
    case j_dotindex:
        evalref(p->a);
        do_tag(p->b);
        genpc(kpushdotixref);
        break;
    case j_dotslice:
        do_tag(p->a);
        do_tag(p->b);
        genpc(kpushdotix);
        break;
    case j_byteindex:
        evalref(p->a);
        do_tag(p->b);
        genpc_int(kpushbyteixref,p->valuemode);
        break;
    case j_keyindex:
    case j_dotkeyindex:
        do_tag(p->b);
        evalref(p->a);
        genpc(kpushkeyixref);
        break;
    case j_ptr:
        do_tag(p->a);
        break;
    case j_dot:
        evalref(p->a);
        d = p->b->def;
        genpc_int(kpushdotref,d->offset);
        break;
    case j_ifx:
        lab1 = createfwdlabel();
        lab2 = createfwdlabel();
        genjumpcond(0,p->a,lab1);
        evalref(p->b);
        genjumpl(lab2);
        definefwdlabel(&lab1);
        evalref(p->c);
        genpc(knop);
        definefwdlabel(&lab2);
        break;
    default:;
        printf("%s %s %d\n",jtagnames[p->tag],"MLINENO=",mlineno);
        gerror("EVALREF: Can't do tag",0);
    }
}

static int32 getpclop	(int32 opc) {
int32	i;
    for (i=1; i<=100; ++i) {
        if (pcl_jcodes[i-1] == opc) {
            noperands = pcl_nopnds[i-1];
            return pcl_kcodes[i-1];
        }
    }
    printf("%s\n",jtagnames[opc]);
    gerror("PCL:GETOPC No Op",0);
    return 0;
}

static void genjumpl	(int32 lab) {
    genpc_lab(kjump,lab);
}

static int32 definelabel	(void) {
int32	lab=pcindex+1;
    return lab;
}

static int32 createfwdlabel	(void) {
int32	lab;
    if (nextfreelabel == 0) {
        printf("%d\n",maxlabels);
        gerror("Too many labels",0);
    }
    lab = nextfreelabel;
    nextfreelabel = labeltable[lab-1];
    labeltable[lab-1] = 0;
    return -lab;
}

static void definefwdlabel	(int32 * oldlab) {
int32	pc;
int32	nextpc;
int32	newlab;
int32	index;
    index = *oldlab;
    if (index >= 0) {
        gerror("deffwdlabel?",0);
    }
    index = -index;
    newlab = pcindex+1;
    pc = labeltable[index-1];
    while (pc) {
        nextpc = (*pccode)[pc-1];
        (*pccode)[pc-1] = newlab;
        pc = nextpc;
    }
    labeltable[index-1] = nextfreelabel;
    nextfreelabel = index;
    *oldlab = newlab;
}

static void stacklooplabels	(int32 * a,int32 * b,int32 * c,int32 * d) {
    if (loopindex >= maxloopindex) {
        gerror("Too many nested loops",0);
    }
    ++loopindex;
    loopstack[loopindex-1][1-1] = a;
    loopstack[loopindex-1][2-1] = b;
    loopstack[loopindex-1][3-1] = c;
    loopstack[loopindex-1][4-1] = d;
    trylevelstack[loopindex-1] = trylevel;
}

static void unstacklooplabels	(void) {
    --loopindex;
}

static int32 findlooplabel	(int32 k,int32 n) {
int32	i;
    if (n == 0) {
        i = 1;
    }
    else {
        i = loopindex-(n-1);
    }
    if (i < 1 || i > loopindex) {
        gerror("Bad loop index",0);
    }
    looptrylevel = trylevelstack[i-1];
    return *loopstack[i-1][k-1];
}

static int32 issimpleparam	(unitrec * p) {
    switch (p->tag) {
    case j_const:
        return 1;
        break;
    case j_len:
    case j_lwb:
    case j_upb:
    case j_eq:
    case j_ne:
    case j_lt:
    case j_le:
    case j_ge:
    case j_gt:
    case j_inot:
    case j_predecrx:
    case j_preincrx:
    case j_postdecrx:
    case j_postincrx:
        return 1;
        break;
    default:;
    }
    return 0;
}

static void genjumpcond	(int32 opc,unitrec * p,int32 lab) {
int32	oldmlineno;
int32	lab2;
unitrec *	q;
unitrec *	r;
int64	x;
    oldmlineno = mlineno;
    mlineno = p->lineno;
    q = p->a;
    r = p->b;
    switch (p->tag) {
    case j_andl:
        if ((opc==0)) {
            genjumpcond(0,q,lab);
            genjumpcond(0,r,lab);
        } else if ((opc==1)) {
            lab2 = createfwdlabel();
            genjumpcond(0,q,lab2);
            genjumpcond(1,r,lab);
            definefwdlabel(&lab2);
        }
        break;
    case j_orl:
        if ((opc==0)) {
            lab2 = createfwdlabel();
            genjumpcond(1,q,lab2);
            genjumpcond(0,r,lab);
            definefwdlabel(&lab2);
        } else if ((opc==1)) {
            genjumpcond(1,q,lab);
            genjumpcond(1,r,lab);
        }
        break;
    case j_notl:
        if ((opc==0)) {
            genjumpcond(1,q,lab);
        } else if ((opc==1)) {
            genjumpcond(0,q,lab);
        }
        break;
    case j_istruel:
        genjumpcond(opc,q,lab);
        break;
    case j_eq:
    case j_ne:
    case j_lt:
    case j_le:
    case j_ge:
    case j_gt:
        gcomparejump(opc,p,q,r,lab);
        break;
    case j_name:
        do_tag(p);
        genpc_lab((opc?kjumptrue:kjumpfalse),lab);
        break;
    case j_const:
        x = p->value;
        if (p->valuemode == tint) {
            goto L246;
        }
        if (x != 0 && opc == 1 || x == 0 && opc == 0) {
            genjumpl(lab);
        }
        break;
    default:;
doelse:
L246:;
        if ((p->tag==j_isdef) || (p->tag==j_isvoid)) {
            do_tag(q);
            if (opc == 1) {
                genpc_lab(((p->tag == j_isdef)?kjumpdef:kjumpvoid),lab);
            }
            else {
                genpc_lab(((p->tag == j_isdef)?kjumpvoid:kjumpdef),lab);
            }
        }
        else {
            do_tag(p);
            genpc_lab((opc?kjumptrue:kjumpfalse),lab);
        }
    }
    mlineno = oldmlineno;
}

static void gcomparejump	(int32 jumpopc,unitrec * p,unitrec * lhs,unitrec * rhs,int32 lab) {
int32	cond;
int32	opc;
    cond = p->tag;
    if (jumpopc == 0) {
        cond = reversecond(cond);
    }
    if ((cond==j_eq)) {
        opc = kjumpeq;
    } else if ((cond==j_ne)) {
        opc = kjumpne;
    } else if ((cond==j_lt)) {
        opc = kjumplt;
    } else if ((cond==j_le)) {
        opc = kjumple;
    } else if ((cond==j_ge)) {
        opc = kjumpge;
    } else if ((cond==j_gt)) {
        opc = kjumpgt;
    }
    do_tag(lhs);
    do_tag(rhs);
    genpc_lab(opc,lab);
}

static int32 reversecond	(int32 op) {
    if ((op==j_eq)) {
        return j_ne;
    } else if ((op==j_ne)) {
        return j_eq;
    } else if ((op==j_lt)) {
        return j_ge;
    } else if ((op==j_le)) {
        return j_gt;
    } else if ((op==j_ge)) {
        return j_lt;
    } else if ((op==j_gt)) {
        return j_le;
    }
    return 0;
}

static void do_convert	(int32 m,unitrec * p) {
int32	n;
int32	elemmode;
int32	i;
int32	lowerx;
int32	lbound;
enum {maxunits = 50};
unitrec *	plist[50];
    if (p->tag != j_makelist && p->tag != j_makeconstr) {
doconv:
        do_tag(p);
        genpc_int(khardconv,m);
        return;
    }
    n = unitstoarray(p->a,(unitrec * (*)[])&plist,maxunits);
    if ((ttbasetype[m]==trecord) || (ttbasetype[m]==tstruct)) {
        if (n < ttlength[m]) {
            printf("%s\n",ttname[m]);
            gerror("Too few fields",p);
        }
        else if (n > ttlength[m]) {
            printf("%s\n",ttname[m]);
            gerror("Too many fields",p);
        }
        for (i=1; i<=n; ++i) {
            do_tag(plist[i-1]);
        }
        genpc_int2(((ttbasetype[m] == trecord)?kmakerecord:kmakestruct),n,m);
    } else if ((ttbasetype[m]==tlist)) {
        if (!!p->b) {
            lowerx = getconstvalue(p->b,104);
        }
        else {
            lowerx = 1;
        }
        if (n == 0) {
            genpc_int(kpushz_listl,lowerx);
        }
        else {
            for (i=1; i<=n; ++i) {
                do_tag(plist[i-1]);
            }
            genpc_int2(kmakelist,n,lowerx);
        }
    } else if ((ttbasetype[m]==tarray)) {
        for (i=1; i<=n; ++i) {
            do_tag(plist[i-1]);
        }
        if (m == tarray) {
            if (!!p->b) {
                lbound = getconstvalue(p->b,105);
            }
            else {
                lbound = 1;
            }
            if (n == 0) {
                genpc_int2(kpushz_arrayl,ti32,lbound);
            }
            else {
                genpc_int4(kmakearray,n,lbound,tarray,ti32);
            }
        }
        else {
            elemmode = tttarget[m];
            if (!!p->b) {
                gerror("2:Can't override lwb",0);
            }
            lbound = ttlower[m];
            if (!!ttlength[m]) {
                if (n < ttlength[m]) {
                    printf("%s\n",ttname[m]);
                    gerror("Too few elements",p);
                }
                else if (n > ttlength[m]) {
                    printf("%s\n",ttname[m]);
                    gerror("Too many elements",p);
                }
                if (n == 0) {
                    genpc_int2(kpushz_arrayl,elemmode,lbound);
                }
                else {
                    genpc_int4(kmakearray,n,lbound,m,elemmode);
                }
            }
            else {
                if (n == 0) {
                    genpc_int2(kpushz_arrayl,elemmode,lbound);
                }
                else {
                    genpc_int4(kmakearray,n,lbound,m,elemmode);
                }
            }
        }
    }
    else {
        do_tag(p);
        genpc_int(khardconv,m);
    }
}

static void do_selectx	(unitrec * pindex,unitrec * pplist,unitrec * pelse) {
int32	n;
int32	labend;
int32	i;
int32	lab;
int32	elselab;
unitrec *	x;
unitrec *	plist[512];
int32	labels[513];
    n = unitstoarray(pplist,(unitrec * (*)[])&plist,maxswitchrange);
    if (n > maxswitchrange) {
        gerror("Selectx too complex",0);
    }
    labend = createfwdlabel();
    do_tag(pindex);
    genpc_int2(kswitch,n,1);
    for (i=1; i<=n; ++i) {
        genpc_lab(kjumplabel,0);
        labels[i-1] = pcindex;
    }
    genpc_lab(kjumplabel,0);
    labels[n+1-1] = pcindex;
    i = 1;
    for (i=1; i<=n; ++i) {
        x = plist[i-1];
        lab = definelabel();
        (*pccode)[labels[i-1]-1] = lab;
        do_tag(x);
        genjumpl(labend);
    }
    elselab = definelabel();
    (*pccode)[labels[n+1-1]-1] = elselab;
    do_tag(pelse);
    genpc(knop);
    definefwdlabel(&labend);
}

static void do_calldll	(unitrec * p,unitrec * a,unitrec * b) {
strec *	d;
strec *	pm;
unitrec *	pbody;
unitrec *	q;
unitrec *	x;
int32	naparams;
int32	fkeyword;
int32	nparams;
int32	i;
int32	j;
int32	k;
int32	resmode;
int32	m;
int32	varparams;
int32	isfn;
int32	fbyref;
int32	langcode;
unitrec *	cparams[64];
strec *	dparams[64];
int32	dparamsdone=0;
char *	name;
    d = a->def;
    if (!!(varparams = d->attribs.ax_varparams)) {
        do_calldllvar(p,a,b);
        return;
    }
    resmode = d->mode;
    isfn = (int32)(resmode != tvoid);
    if (p->tag == j_callfn && !isfn) {
        gerror("DLL: needs function",0);
    }
    if (p->tag == j_callproc) {
        isfn = 0;
        resmode = tvoid;
    }
    if (isfn) {
        genpc(kpushz_void);
    }
    fkeyword = 0;
    naparams = 0;
    nparams = d->attribs.ax_nparams;
    memset((int32 *)&cparams,0,4*nparams);
    x = b;
    while (x) {
        ++naparams;
        cparams[naparams-1] = 0;
        if (fkeyword && x->tag != j_keyword) {
            gerror("Normal param follows keyword param",p);
        }
        if ((x->tag==j_keyword)) {
            fkeyword = 1;
            name = x->a->def->name;
            if (!dparamsdone) {
                extractparams(d,(strec * (*)[])&dparams);
                dparamsdone = 1;
            }
            k = 0;
            for (j=1; j<=nparams; ++j) {
                if (strcmp(dparams[j-1]->name,name) == 0) {
                    k = j;
                    goto L299;
                }
            }
L299:;
            if (k == 0) {
                printf("%s %s %s\n",name,"in",d->name);
                gerror("Can't find keyword param",0);
            }
            if (!!cparams[k-1]) {
                printf("%s\n",name);
                gerror("Param already set",0);
            }
            cparams[k-1] = x->b;
        } else if ((x->tag==j_null)) {
        }
        else {
            cparams[naparams-1] = x;
        }
        x = x->nextunit;
    }
    for (i=1; i<=nparams; ++i) {
        x = cparams[i-1];
        if (x == 0) {
            if (!dparamsdone) {
                extractparams(d,(strec * (*)[])&dparams);
                dparamsdone = 1;
            }
            pm = dparams[i-1];
            if (!!pm->code) {
                cparams[i-1] = pm->code;
            }
            else if (!pm->attribs.ax_optional) {
                printf("%s %s %s\n",pm->name,"in",d->name);
                gerror("Param not optional",0);
            }
        }
    }
    genpc(kstartdll);
    for (i=1; i<=nparams; ++i) {
        x = cparams[i-1];
        if (!dparamsdone) {
            extractparams(d,(strec * (*)[])&dparams);
            dparamsdone = 1;
        }
        m = dparams[i-1]->mode;
        fbyref = dparams[i-1]->attribs.ax_byrefmode;
        if (fbyref) {
            gerror("Byref on dll call",0);
        }
        if (x == 0) {
            gerror("Dll missing param - no default",0);
        }
        else {
            do_tag(x);
            genpc_int(kpushdll,m);
        }
    }
    if ((d->attribs.ax_fflang==windowsff)) {
        langcode = 'W';
    } else if ((d->attribs.ax_fflang==clangff)) {
        langcode = 'C';
    } else if ((d->attribs.ax_fflang==mlangff)) {
        langcode = 'M';
    }
    else {
        gerror("Bad FF?",0);
    }
    genpc_s(kcalldll,d);
    genopnd_int(langcode);
    genopnd_int(resmode);
}

static int32 islogical	(unitrec * p) {
    if ((p->tag==j_istruel) || (p->tag==j_notl) || (p->tag==j_andl) || (p->tag==j_orl) || (p->tag==j_xorl)) {
        return 1;
    }
    return 0;
}

static void do_and	(unitrec * x,unitrec * y) {
int32	a;
int32	b;
    a = createfwdlabel();
    b = createfwdlabel();
    genjumpcond(0,x,a);
    genjumpcond(0,y,a);
    genpc_int(kpush_ci,1);
    genjumpl(b);
    definefwdlabel(&a);
    genpc_int(kpush_ci,0);
    genpc(knop);
    definefwdlabel(&b);
}

static void do_or	(unitrec * x,unitrec * y) {
int32	a;
int32	b;
    a = createfwdlabel();
    b = createfwdlabel();
    genjumpcond(1,x,a);
    genjumpcond(1,y,a);
    genpc_int(kpush_ci,0);
    genjumpl(b);
    definefwdlabel(&a);
    genpc_int(kpush_ci,1);
    genpc(knop);
    definefwdlabel(&b);
}

static void do_callptr	(unitrec * p,unitrec * pproc,unitrec * pparams) {
int32	n;
int32	i;
int32	j;
unitrec *	params[64];
unitrec *	x;
unitrec *	q;
    genpc(kpushz_void);
    n = 0;
    q = pparams;
    while (q) {
        params[++n-1] = q;
        q = q->nextunit;
    }
    for (i=n; i>=1; --i) {
        x = params[i-1];
        if (x->tag == j_null) {
            genpc_int(kpushz,tvoid);
        }
        else {
            do_tag(x);
        }
    }
    if (pproc->tag != j_ptr) {
        gerror("Callptr?",0);
    }
    do_tag(pproc->a);
    genpc_int2(kcallptr,n,0);
    if (n) {
        genfree(n);
    }
    if (p->tag == j_callproc) {
        genfree(1);
    }
}

static void do_callmproc	(unitrec * p,unitrec * pproc,unitrec * pparams,int32 calledasfn) {
int32	n;
int32	isfn;
int32	i;
unitrec *	params[64];
unitrec *	x;
unitrec *	pleft;
    isfn = (int32)(p->tag == j_callmfn);
    if (isfn) {
        genpc(kpushz_void);
    }
    n = unitstoarray(pparams,(unitrec * (*)[])&params,maxparams);
    for (i=n; i>=1; --i) {
        x = params[i-1];
        if (x->tag == j_null) {
            genpc(kpushz_void);
        }
        else {
            do_tag(x);
        }
    }
    if (pproc->tag != j_dot) {
        gerror("Callmproc/not dot",0);
    }
    pleft = pproc->a;
    evalref(pleft);
    do_tag(pproc);
    genpc_int2(kcallptr,(n+1),0);
    genfree(n+1);
    if (!calledasfn) {
        genfree(1);
    }
}

static int32 checkblockreturn	(unitrec * p) {
unitrec *	q;
unitrec *	r;
    if (p == 0) {
        return 0;
    }
    if (p->tag != j_block) {
        gerror("CBR?",0);
    }
    q = p->a;
    if (q == 0) {
        return 0;
    }
    while (r = q->nextunit) {
        q = r;
    }
    if ((q->tag==j_return)) {
        return 1;
    } else if ((q->tag==j_if)) {
        return (int32)(!!checkblockreturn(q->b) && !!checkblockreturn(q->c));
    } else if ((q->tag==j_longif)) {
        r = q->a;
        while (r) {
            if (!checkblockreturn(r->b)) {
                return 0;
            }
            r = r->nextunit;
        }
        return checkblockreturn(q->b);
    }
    return 0;
}

static void genfree	(int32 n) {
    genpc_int(kfree,n);
}

static void do_clamp	(unitrec * x,unitrec * a,unitrec * b) {
    do_tag(x);
    do_tag(a);
    genpc(kmax);
    do_tag(b);
    genpc(kmin);
}

static void do_applyopx	(unitrec * x,unitrec * a,unitrec * b) {
int32	lab;
    if (b) {
        do_tag(a);
        do_tag(b);
        do_tag(x);
        genpc_int(kapplyop,2);
    }
    else {
        do_tag(a);
        do_tag(x);
        genpc_int(kapplyop,1);
    }
    lab = createfwdlabel();
    genpc_lab(kjump,lab);
    genpc(knop);
    definefwdlabel(&lab);
}

static void do_calldllvar	(unitrec * p,unitrec * a,unitrec * b) {
strec *	d;
strec *	pm;
unitrec *	pbody;
unitrec *	q;
unitrec *	x;
int32	naparams;
int32	fkeyword;
int32	nparams;
int32	i;
int32	j;
int32	k;
int32	resmode;
int32	m;
int32	isfn;
int32	t;
int32	langcode;
unitrec *	cparams[64];
strec *	dparams[64];
int32	dparamsdone=0;
char *	name;
    d = a->def;
    resmode = d->mode;
    isfn = (int32)(resmode != tvoid);
    if (p->tag == j_callfn && !isfn) {
        gerror("DLL: needs function",0);
    }
    if (p->tag == j_callproc) {
        isfn = 0;
        resmode = tvoid;
    }
    if (isfn) {
        genpc(kpushz_void);
    }
    nparams = d->attribs.ax_nparams;
    extractparams(d,(strec * (*)[])&dparams);
    genpc(kstartdll);
    x = b;
    naparams = 0;
    i = 1;
    while (x) {
        ++naparams;
        do_tag(x);
        if (i <= nparams) {
            t = dparams[i-1]->mode;
        }
        else {
            t = tvoid;
        }
        genpc_int(kpushdll,t);
        x = x->nextunit;
        ++i;
    }
    if ((d->attribs.ax_fflang==windowsff)) {
        langcode = 'W';
    } else if ((d->attribs.ax_fflang==clangff)) {
        langcode = 'C';
    } else if ((d->attribs.ax_fflang==mlangff)) {
        langcode = 'M';
    }
    else {
        gerror("Bad FF?",0);
    }
    genpc_s(kcalldll,d);
    genopnd_int(langcode);
    genopnd_int(resmode);
}

static void callhostfn	(int32 fnindex,int32 calledasfn) {
    genpc_int(kcallhost,fnindex);
    if (!!hostisfn[fnindex] && !calledasfn) {
        genfree(1);
    }
}

static void extractparams	(strec * d,strec * (*params)[]) {
strec *	p;
int32	i;
    p = d->paramlist;
    i = 0;
    while (p) {
        (*params)[++i-1] = p;
        p = p->nextparam;
    }
}

static int32 unitstoarray	(unitrec * p,unitrec * (*plist)[],int32 maxunits) {
int32	n;
    n = 0;
    while (p) {
        if (n >= maxunits) {
            gerror("UTO Too many units",0);
        }
        (*plist)[++n-1] = p;
        p = p->nextunit;
    }
    return n;
}

static void do_idiv	(unitrec * a,unitrec * b) {
int64	x;
int32	n;
    if (b == 0) {
        gerror("Idiv?",0);
    }
    do_tag(a);
    if (b->tag == j_const && b->valuemode == tint) {
        if ((x = b->value==0)) {
            gerror("div 0",0);
        } else if ((x = b->value==1)) {
            return;
        }
    }
    do_tag(b);
    genpc(kidiv);
}

static int32 ispoweroftwo	(int32 x) {
int32	a;
int32	n;
int32	av_2;
    a = 1;
    n = 0;
    av_2 = 30;
    while (av_2--) {
        ++n;
        a = a<<1;
        if (a == x) {
            return n;
        }
    }
    return 0;
}

static void genpushint	(uint64 a) {
    genpc_int(kpush_ci,a);
}


// From module: q_libs
char * getintlib	(char * name) {
int32	i;
char *	source;
char *	newsource;
    for (i=1; i<=14; ++i) {
        if (strcmp(name,libnames[i-1]) == 0) {
            source = *libtext[i-1];
            rfsize = strlen(source);
            newsource = (char *)pcm_alloc(rfsize+4);
            memcpy((int32 *)newsource,(int32 *)source,rfsize);
            *(newsource+rfsize) = 26;
            *(newsource+rfsize+1) = 0;
            return newsource;
        }
    }
    return 0;
}


// From module: start
void start	(void) {
int32	i;
int32	status;
int32	t;
int32	N;
int32	tx;
int32	stopcode;
char *	file;
strec	S;
modulerec	m;
    printf("%s\n","Q Compiler 5.18");
    starttiming();
    initdata();
    getinputoptions();
    if (!!fshowoptions) {
        showoptions();
    }
    if (fverbose == 0) {
        printf("%s %s %s %s\n","Compiling",infile,"to",outfile);
    }
    initlogfile();
    if (!!(passes&mload)) {
        do_loadmodules();
    }
    if (!!(passes&mloadqa)) {
        do_loadqafile();
    }
    if (!!(passes&mwriteqa)) {
        do_writeqafile();
    }
    if (!!(passes&(mparse|mcodegen))) {
        do_compilemodules();
    }
    if (!!fshowpcl) {
        showpcl("PCL",1);
    }
    if (!!(passes&mwritepc)) {
        do_writepcfile();
    }
    showoutput();
    if (!!fshowlog) {
        closelogfile();
    }
    stopcode = 0;
    exit(stopcode);
}

static void do_loadmodules	(void) {
    if ((ninputfiles==0)) {
        loaderror("No sourcefile specified","");
    } else if ((ninputfiles==1)) {
    }
    if (fverbose) {
        printf("%s %s\n","Loading:",infile);
    }
    loadstart = os_clock();
    loadstatus = loadmainmodule(infile);
    loadend = os_clock();
}

static int32 loadmainmodule	(char * filespec) {
char	modulename[100];
char	path[300];
byte *	source;
int32	status;
modulerec	m;
int32	i;
int32	flag;
    pcm_clearmem(&moduletable[0],94);
    moduletable[0].name = "PROGRAM";
    moduletable[0].filename = "<->";
    moduletable[0].sourcecode = "<program>";
    moduletable[0].sourcelen = strlen(moduletable[0].sourcecode);
    stprogram = getduplnameptr(0,addnamestr("$prog"),programid);
    moduletable[0].stmodule = stprogram;
    source = readfile(filespec);
    if (source == 0) {
        loaderror("Can't load main file:",filespec);
        return 0;
    }
    strcpy((char *)&modulename,extractbasefile(filespec));
    strcpy((char *)&path,extractpath(filespec));
    if (!!(uchar)path[1-1]) {
        ++nsearchdirs;
        for (i=nsearchdirs; i>=2; --i) {
            searchdirs[i-1] = searchdirs[i-1-1];
        }
        searchdirs[1-1] = pcm_copyheapstring((char *)&path);
    }
    loadmodule((char *)&modulename,filespec,(char *)source,rfsize,moduleid,&flag);
    return 1;
}

static int32 loadmodule	(char * modulename,char * filespec,char * source,int32 length,int32 id,int32 * exportflag) {
modulerec	m;
enum {maximports = maxmodule};
char *	importnames[50];
byte	importflags[51];
int32	importmoduleno[50];
int32	nimports;
int32	i;
int32	status;
int32	k;
int32	flag;
int32	j;
    pcm_clearmem(&m,94);
    m.name = pcm_copyheapstring(modulename);
    m.filename = pcm_copyheapstring(filespec);
    m.sourcecode = source;
    m.sourcelen = length;
    stmodule = getduplnameptr(stprogram,addnamestr(m.name),id);
    adddef(stprogram,stmodule);
    m.stmodule = stmodule;
    if (npendingmodules >= maxmodule) {
        loaderror("Too many pendingmodules",modulename);
    }
    pendingmodules[++npendingmodules-1] = m.name;
    nimports = readimportlist(&m,(char * (*)[])&importnames,(byte (*)[])&importflags,maximports);
    ++modulelevel;
    for (i=1; i<=nimports; ++i) {
        flag = 0;
        if (fverbose == 2) {
            printf("%s %s\n","Load import for",modulename);
        }
        k = loadimport(importnames[i-1],&flag);
        if (flag) {
            importflags[i] = 1;
        }
        (m).importmap[k-1] = 1;
        importmoduleno[i-1] = k;
    }
    for (i=1; i<=nimports; ++i) {
        if (!!importflags[i]) {
            k = importmoduleno[i-1];
            for (j=1; j<=nmodules; ++j) {
                if (!!(moduletable[k]).importmap[j-1]) {
                    (m).importmap[j-1] = 1;
                }
            }
        }
    }
    --modulelevel;
    --npendingmodules;
    if (nmodules >= maxmodule) {
        loaderror("Too many modules",modulename);
    }
    m.level = modulelevel;
    moduletable[++nmodules] = m;
    m.stmodule->attribs.ax_moduleno = nmodules;
    *exportflag = importflags[0];
    return nmodules;
}

static int32 loadimport	(char * modulename,int32 * exportflag) {
int32	i;
char *	ifilespec;
char	filespec[300];
char *	source;
char *	newname;
    newname = modulename;
    for (i=1; i<=nmodules; ++i) {
        if (strcmp(moduletable[i].name,newname) == 0) {
            return i;
        }
    }
    for (i=1; i<=npendingmodules; ++i) {
        if (strcmp(pendingmodules[i-1],newname) == 0) {
            loaderror("Circular/mutual import",modulename);
        }
    }
    source = getmodulestr(modulename,(char *)&filespec);
    return loadmodule(newname,(char *)&filespec,source,rfsize,extmoduleid,exportflag);
}

static int32 readimportlist	(modulerec * m,char * (*importnames)[],byte (*importflags)[],int32 maximports) {
int32	n;
int32	flag;
int32	exportflag;
char *	s;
char	name[100];
char	libname[100];
    startlex("IMPORTS",m->sourcecode);
    exportflag = 0;
    n = 0;
L27:;
    while (1) {
        lexreadtoken();
        if ((nextlx.symbol==eofsym)) {
            goto L28;
        } else if ((nextlx.symbol==semisym) || (nextlx.symbol==eolsym)) {
        } else if ((nextlx.symbol==rawnamesym)) {
            flag = 0;
            if (!!checkname("import",0)) {
                lexreadtoken();
                if (nextlx.symbol == opsym && nextlx.subcode == j_mul) {
                    flag = 1;
                    lexreadtoken();
                }
                if (nextlx.symbol != rawnamesym) {
                    abortprogram("import: modulename expected");
                }
                if (++n > maximports) {
                    abortprogram("too many imports");
                }
                strcpy((char *)&name,convertzstring(nextlx.svalue,nextlx.length));
                (*importnames)[n-1] = pcm_copyheapstring((char *)&name);
                (*importflags)[n] = flag;
            }
            else if (!!checkname("export",0) || !!checkname("endexport",0)) {
                exportflag = 1;
                goto L27;
            }
            else if (!!checkname("$windows",0)) {
                if (!!os_iswindows()) {
                    goto L27;
                }
                else {
skipthisline:
L37:;
                    do {
                        lexreadtoken();
                    } while (!(nextlx.symbol == eolsym || nextlx.symbol == eofsym));
                }
            }
            else if (!!checkname("$linux",0)) {
                if (!os_iswindows()) {
                    goto L27;
                }
                else {
                    goto L37;
                }
            }
            else {
                goto L28;
            }
        }
        else {
            goto L28;
        }
    }
L28:;
    (*importflags)[0] = exportflag;
    return n;
}

static void initlogfile	(void) {
    if (!fshowlog) {
        return;
    }
    if ((logdest==2)) {
        remove("qq.log");
        logdev = fopen("qq.log","w");
    } else if ((logdest==0) || (logdest==1)) {
        logdev = 0;
    }
}

static void closelogfile	(void) {
char	str[100];
int32	pos;
    if (logdest == 2) {
        fclose(logdev);
        sprintf((char *)&str,"\\m\\ed.bat  %s","qq.log");
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
    lexsetup();
    inittypetables();
    initsearchdirs();
    initqclib();
    initpclgen();
    initpcldata();
}

static void initsearchdirs	(void) {
char	str1[300];
char	str2[300];
int32	i;
    searchdirs[++nsearchdirs-1] = "";
    strcpy((char *)&str1,os_gethostname());
    if (!!(uchar)str1[1-1]) {
        strcpy((char *)&str2,extractpath((char *)&str1));
        searchdirs[++nsearchdirs-1] = pcm_copyheapstring((char *)&str2);
    }
    strcpy((char *)&str1,os_getmpath());
    if (!!(uchar)str1[1-1]) {
        searchdirs[++nsearchdirs-1] = pcm_copyheapstring((char *)&str1);
    }
}

static char * getmodulestr	(char * modulename,char * filespec) {
char *	ifile;
byte *	source;
    if (1) {
        source = (byte *)getintlib(modulename);
        if (source) {
            strcpy(filespec,"<Internal>");
            return (char *)source;
        }
    }
    ifile = findmodule(modulename);
    if (ifile == 0) {
        if (!1) {
            source = (byte *)getintlib(modulename);
            if (source) {
                strcpy(filespec,"<Internal>");
                return (char *)source;
            }
        }
        loaderror("Can't locate import module:",modulename);
    }
    strcpy(filespec,ifile);
    source = readfile(filespec);
    if (source == 0) {
        loaderror("?Read file error:",(char *)&filespec);
    }
    return (char *)source;
}

static char * findmodule	(char * modulename) {
char	file[300];
static char	filespec[300];
char *	s;
int32	i;
    strcpy((char *)&file,modulename);
    strcpy((char *)&file,addext((char *)&file,".q"));
    if (fverbose == 2) {
        printf("%s %s\n","Locating:",(char*)(&file));
    }
    for (i=1; i<=nsearchdirs; ++i) {
        strcpy((char *)&filespec,searchdirs[i-1]);
        strcat((char *)&filespec,(char *)&file);
        if (fverbose == 2) {
            printf("%s%d%s %s %s %s\n","\t",i,": Checking File",(char*)(&filespec),"in",searchdirs[i-1]);
        }
        if (!!checkfile((char *)&filespec)) {
            if (fverbose == 2) {
                printf("%s %s\n","\tFound:",(char*)(&filespec));
            }
            return (char *)&filespec;
        }
    }
    return 0;
}

static int32 checkname	(char * name,int32 length) {
    if (length == 0) {
        length = strlen(name);
    }
    if (nextlx.length == length && memcmp(nextlx.svalue,name,length) == 0) {
        return 1;
    }
    return 0;
}

static void getinputoptions	(void) {
enum {slash = '-'};
int32	i;
int32	j;
int32	k;
int32	paramno;
int32	pmtype;
int32	sw;
char *	name;
char *	value;
char *	ext;
    paramno = 2;
    while (pmtype = nextparam(&paramno,&name,&value)) {
        if ((pmtype==pm_option)) {
            convlcstring(name);
            for (sw=1; sw<=25; ++sw) {
                if (strcmp(name,optionnames[sw-1]) == 0) {
                    do_option(sw,value);
                    goto L59;
                }
            }
            printf("%s %s\n","Unknown option:",name);
            exit(99);
L59:;
        } else if ((pmtype==pm_sourcefile)) {
            if (ninputfiles >= maxmodule) {
                loaderror("Too many input files","");
            }
            inputfiles[++ninputfiles] = pcm_copyheapstring(name);
        }
    }
    if (passes == 0) {
        passes = 113;
    }
    if (!!fshowlog && logdest == 0) {
        logdest = 2;
    }
    if (ninputfiles == 0) {
        printf("%s\n","Usage:");
        printf("%s%s %s\n","\t",sysparams[1-1],"filename[.q]");
        printf("%s%s %s\n","\t",sysparams[1-1],"-help");
        exit(0);
    }
    infile = inputfiles[1];
    ext = extractext(infile,0);
    convlcstring(ext);
    if (strcmp(ext,"q") == 0) {
    }
    else if (strcmp(ext,"qa") == 0) {
        if (!!(passes&mload)) {
            passes &= ~mload;
            passes |= mloadqa;
        }
    }
    else {
        loaderror("Unknown file extension:",infile);
    }
    outfile = pcm_copyheapstring(changeext(infile,"pc"));
}

static void do_option	(int32 sw,char * value) {
int32	length;
    if ((sw==load_sw)) {
        passes = mload;
    } else if ((sw==qa_sw)) {
        passes = 5;
    } else if ((sw==lex_sw)) {
        passes = 9;
    } else if ((sw==parse_sw)) {
        passes = 17;
    } else if ((sw==gen_sw)) {
        passes = 49;
    } else if ((sw==writepc_sw) || (sw==writepc2_sw)) {
        passes = 113;
    } else if ((sw==options_sw)) {
        fshowoptions = 1;
    } else if ((sw==modules_sw)) {
        fshowmodules = 1;
    } else if ((sw==files_sw)) {
        fshowfiles = 1;
    } else if ((sw==ast_sw)) {
        fshowast = 1;
        fshowlog = 1;
    } else if ((sw==st_sw)) {
        fshowst = 1;
        fshowlog = 1;
    } else if ((sw==stflat_sw)) {
        fshowstflat = 1;
        fshowlog = 1;
    } else if ((sw==pcl_sw)) {
        fshowpcl = 1;
        fshowlog = 1;
    } else if ((sw==types_sw)) {
        fshowtypes = 1;
        fshowlog = 1;
    } else if ((sw==dis_sw)) {
        fshowdis = 1;
        fshowlog = 1;
    } else if ((sw==paths_sw)) {
        fshowpaths = 1;
    } else if ((sw==s_sw)) {
        logdest = 1;
        fshowlog = 1;
    } else if ((sw==d_sw)) {
        logdest = 2;
        if (!fshowlog) {
            fshowast = 1;
            fshowst = 1;
            fshowpcl = 1;
        }
        fshowlog = 1;
    } else if ((sw==time_sw)) {
        fshowtiming = 1;
    } else if ((sw==v_sw)) {
        fverbose = 1;
    } else if ((sw==v2_sw)) {
        fverbose = 2;
    } else if ((sw==help_sw) || (sw==help2_sw)) {
        showhelp();
    }
}

static int32 nextparam	(int32 * paramno,char * * name,char * * value) {
static int32	infile=0;
static char *	filestart=0;
static char *	fileptr=0;
static byte	colonnext=0;
byte *	q;
char *	item;
char *	fileext;
int32	length;
static char	str[300];
reenter:
L88:;
    if (infile) {
        if (readnextfileitem(&fileptr,&item) == 0) {
            free(filestart);
            infile = 0;
            goto L88;
        }
    }
    else {
        if (!!colonnext || *paramno > nsysparams) {
            return pm_end;
        }
        item = sysparams[*paramno-1];
        ++*paramno;
        length = strlen(item);
        if ((uchar)*item != '-' && (uchar)*(item+length-1) == ':') {
            printf("%s\n","//COLON SEEN");
            colonnext = 1;
            *(item+length-1) = 0;
        }
        if ((uchar)*item == '@') {
            filestart = fileptr = (char *)readfile(item+1);
            if (filestart == 0) {
                printf("%s %s\n","Can't open",item);
                exit(0);
            }
            infile = 1;
            goto L88;
        }
        if ((uchar)*item == ':') {
            return pm_end;
        }
    }
    *value = 0;
    if ((uchar)*item == '-') {
        *name = item+1;
        q = (byte *)strchr(item,':');
        if (!q) {
            q = (byte *)strchr(item,'=');
        }
        if (q) {
            *value = (char *)(q+1);
            *q = 0;
        }
        return pm_option;
    }
    fileext = extractext(item,0);
    *name = item;
    if ((uchar)*fileext == 0) {
        strcpy((char *)&str,*name);
        *name = addext((char *)&str,".q");
    }
    return pm_sourcefile;
}

static int32 readnextfileitem	(char * * fileptr,char * * item) {
char *	p;
char *	pstart;
char *	pend;
int32	i;
int32	n;
static char	str[256];
    p = *fileptr;
    while (1) {
        if (((uchar)*p==' ') || ((uchar)*p=='	') || ((uchar)*p==13) || ((uchar)*p==10)) {
            ++p;
        } else if (((uchar)*p==26) || ((uchar)*p==0)) {
            return 0;
        }
        else {
            goto L90;
        }
    }
L90:;
    if ((uchar)*p == '"') {
        pstart = ++p;
        while (1) {
            if (((uchar)*p==0) || ((uchar)*p==26)) {
                printf("%s\n","Unexpected EOF in @file");
                exit(0);
            } else if (((uchar)*p=='"')) {
                pend = p++;
                if ((uchar)*p == ',') {
                    ++p;
                }
                goto L96;
            }
            ++p;
        }
L96:;
    }
    else {
        pstart = p;
        while (1) {
            if (((uchar)*p==0) || ((uchar)*p==26)) {
                pend = p;
                goto L101;
            } else if (((uchar)*p==' ') || ((uchar)*p=='	') || ((uchar)*p==',') || ((uchar)*p==13) || ((uchar)*p==10)) {
                pend = p++;
                goto L101;
            }
            ++p;
        }
L101:;
    }
    n = pend-pstart;
    if (n >= 256) {
        printf("%s\n","@file item too long");
        exit(0);
    }
    memcpy((int32 *)&str,(int32 *)pstart,n);
    str[n+1-1] = 0;
    *item = (char *)&str;
    *fileptr = p;
    return 1;
}

static int32 do_parse	(int32 n) {
int32	status;
    status = parsemodule(n);
    return status;
}

static int32 do_codegen	(int32 n) {
int32	status;
    status = codegen(n);
    return status;
}

static void showast	(void) {
int32	i;
    if (logdest) {
        if (logdest == 2) {
            fprintf(logdev,"\n");
        }
        for (i=1; i<=nmodules; ++i) {
            printcode(logdev,"AST",i);
        }
        fprintf(logdev,"\n");
    }
}

static void showstflat	(char * caption) {
    if (logdest) {
        if (logdest == 2) {
        }
        fprintf(logdev,"%s %s\n","PROC",caption);
        printstflat(logdev);
        fprintf(logdev,"\n");
    }
}

static void showsttree	(void) {
    if (logdest) {
        if (logdest == 2) {
        }
        fprintf(logdev,"%s\n","PROC ST");
        printst(logdev,moduletable[0].stmodule,0);
        fprintf(logdev,"\n");
    }
}

static void showpcl	(char * caption,int32 phase) {
strbuffer *	pclstr;
int32	i;
    if (logdest) {
        if (logdest == 2) {
        }
        for (i=0; i<=nmodules; ++i) {
            pclstr = writepccode(caption,i);
            gs_println(pclstr,logdev);
        }
    }
}

static void showgenfields	(void) {
    if (logdest) {
        printgenfieldtable(logdev,"PROC Generic Fields Table");
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
        printf("%s\n","DONE INITPC");
        fprintf(logdev,"%s\n","SHOW ALL PC PCL2");
        fprintf(logdev,"%s\n",caption);
        showpcl("ALL MODULES",2);
    }
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
int32	av_1;
    printf("%s %d\n","Modules:",nmodules);
    for (i=nmodules; i>=1; --i) {
        m = moduletable[i];
        j = (nmodules-i)+1;
        printf("%d%s",j,":");
        av_1 = m.level;
        while (av_1--) {
            printf("%s","  ");
        }
        printf("%s %d %p\n",m.name,m.sourcelen,(void*)(m.stmodule));
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

static void do_compilemodules	(void) {
int32	i;
int32	status;
    compilestatus = 1;
    for (i=1; i<=nmodules; ++i) {
        compilestatus = compilemodule(i);
        if (!compilestatus) {
            return;
        }
    }
}

static int32 compilemodule	(int32 n) {
int32	status;
modulerec	m;
int32	ntokens=0;
char *	s;
    currmoduleno = n;
    currmodule = &moduletable[n];
    m = moduletable[n];
    m.stmodule->nameid = moduleid;
    status = do_parse(n);
    totallines += lx.lineno;
    if (status && !!(passes&mcodegen)) {
        status = do_codegen(n);
        if (status) {
            totalpclopcodes += pcindex;
            purgesymbollist(stmodule->deflist,1,0);
        }
    }
    m.stmodule->nameid = extmoduleid;
    return status;
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

static void do_writeqafile	(void) {
void *	f;
int32	i;
int32	k;
int32	nextra;
modulerec	m;
char	filename[300];
char *	file;
int32	offsets[50];
int32	infooffsets[50];
int32	extraoffsets[10];
char *	text;
    strcpy((char *)&filename,moduletable[nmodules].name);
    strcpy((char *)&filename,changeext((char *)&filename,".qa"));
    f = fopen((char *)&filename,"wb");
    if (f == 0) {
        printf("%s %s\n","Couldn't create",(char*)(&filename));
        exit(1);
    }
    nextra = ninputfiles-1;
    if (nextra > maxextra) {
        loaderror("qa:too many extra files","");
    }
    for (i=1; i<=nextra; ++i) {
        extrafiles[i-1] = inputfiles[i+1];
    }
    fprintf(f,"%s\n","qa file");
    fprintf(f,"%s %d %d\n","modules",nmodules,nextra);
    for (i=1; i<=nmodules; ++i) {
        m = moduletable[i];
        fprintf(f,"%s %s%s","module",m.name," ");
        for (k=1; k<=nmodules; ++k) {
            if (!!(m).importmap[k-1]) {
                fprintf(f,"%d %s",k,"");
            }
        }
        fprintf(f,"%s","0 ");
        infooffsets[i-1] = ftell(f);
        fprintf(f,"%s\n","# #                         ");
    }
    for (i=1; i<=nextra; ++i) {
        fprintf(f,"%s%s%s","file \"",extrafiles[i-1],"\" ");
        infooffsets[i+nmodules-1] = ftell(f);
        fprintf(f,"%s\n","# #              ");
    }
    fprintf(f,"%s\n","end");
    fprintf(f,"%s\n","!This generated file should not be manually edited if the above");
    fprintf(f,"%s\n","!byte offsets and sizes are to be valid.");
    for (i=1; i<=nmodules; ++i) {
        m = moduletable[i];
        fprintf(f,"%s\n","======================");
        fprintf(f,"%s %s %d%s%d\n","module ",m.name,i,"/",nmodules);
        fprintf(f,"%s\n","======================");
        offsets[i-1] = ftell(f);
        fwrite((int32 *)m.sourcecode,1,m.sourcelen,f);
    }
    printf("%s %d\n","NEXTRA=",nextra);
    for (i=1; i<=nextra; ++i) {
        m = moduletable[i];
        fprintf(f,"%s\n","======================");
        fprintf(f,"%s%s%s%d\n","file ",extrafiles[i-1],"/",nextra);
        fprintf(f,"%s\n","======================");
        extraoffsets[i-1] = ftell(f);
        text = (char *)readfile(extrafiles[i-1]);
        if (text == 0) {
            printf("%s\n",extrafiles[i-1]);
            loaderror("qa/can't load extra file","");
        }
        extrasizes[i-1] = rfsize;
        fwrite((int32 *)text,1,rfsize,f);
    }
    fprintf(f,"%s\n","======================");
    fprintf(f,"%s\n","end");
    fprintf(f,"%s\n","======================");
    for (i=1; i<=nmodules; ++i) {
        fseek(f,infooffsets[i-1],seek_set);
        fprintf(f,"%d %d",offsets[i-1],moduletable[i].sourcelen);
    }
    for (i=1; i<=nextra; ++i) {
        fseek(f,infooffsets[i+nmodules-1],seek_set);
        fprintf(f,"%d %d",extraoffsets[i-1],extrasizes[i-1]);
    }
    fclose(f);
    if (fverbose) {
        printf("%s %s\n","Finished writing",(char*)(&filename));
    }
}

static void checkkeyword	(char * kwd) {
int32	length;
    lexreadtoken();
    length = strlen(kwd);
    if (!(nextlx.symbol == rawnamesym && nextlx.length == length && memcmp(nextlx.svalue,kwd,length) == 0)) {
        loaderror("QA: expected:",kwd);
    }
}

static int32 readinttoken	(void) {
    lexreadtoken();
    if (nextlx.symbol != intconstsym) {
        loaderror("Int expected","");
    }
    return nextlx.value;
}

static int32 do_loadqafile	(void) {
char	modulename[100];
char	name[100];
byte *	source;
int32	status;
int32	i;
int32	k;
int32	offset;
int32	length;
modulerec	m;
char *	p;
    infile = inputfiles[1];
    pcm_clearmem(&moduletable[0],94);
    moduletable[0].name = "PROGRAM";
    moduletable[0].filename = "<->";
    moduletable[0].sourcecode = "<program>";
    moduletable[0].sourcelen = strlen(moduletable[0].sourcecode);
    stprogram = getduplnameptr(0,addnamestr("$prog"),programid);
    moduletable[0].stmodule = stprogram;
    source = readfile(infile);
    if (source == 0) {
        loaderror("Can't load qa file:",infile);
        return 0;
    }
    startlex("QA",(char *)source);
    checkkeyword("qa");
    checkkeyword("file");
    lexreadtoken();
    checkkeyword("modules");
    nmodules = readinttoken();
    nextra = readinttoken();
    if (nmodules > maxmodule || nextra > maxextra) {
        loaderror("QA:Too many modules","");
    }
    lexreadtoken();
    for (i=1; i<=nmodules; ++i) {
        checkkeyword("module");
        lexreadtoken();
        memcpy((int32 *)&name,(int32 *)nextlx.svalue,nextlx.length);
        name[nextlx.length+1-1] = 0;
        pcm_clearmem(&m,94);
        m.name = pcm_copyheapstring((char *)&name);
        m.filename = m.name;
        while (1) {
            k = readinttoken();
            if (k == 0) {
                goto L177;
            }
            (m).importmap[k-1] = 1;
        }
L177:;
        p = m.sourcecode = (char *)(source+readinttoken());
        m.sourcelen = readinttoken();
        m.level = 0;
        *(p+m.sourcelen) = 26;
        *(p+m.sourcelen+1) = 0;
        lexreadtoken();
        stmodule = getduplnameptr(stprogram,addnamestr(m.name),((i < nmodules)?extmoduleid:moduleid));
        adddef(stprogram,stmodule);
        m.stmodule = stmodule;
        stmodule->attribs.ax_moduleno = i;
        moduletable[i] = m;
    }
    for (i=1; i<=nextra; ++i) {
        checkkeyword("file");
        lexreadtoken();
        memcpy((int32 *)&name,(int32 *)nextlx.svalue,nextlx.length);
        name[nextlx.length+1-1] = 0;
        extrafiles[i-1] = pcm_copyheapstring((char *)&name);
        extratext[i-1] = p = (char *)(source+readinttoken());
        extrasizes[i-1] = length = readinttoken();
        *(p+length) = 0;
        lexreadtoken();
    }
    return 1;
}

static void showinttiming	(char * caption) {
int32	t;
int32	i;
    t = (os_clock()-clockstart);
    printf("%lld %s %s\n",(t*1000)/ os_getclockspersec(),"msec",caption);
}

static void do_writepcfile	(void) {
void *	f;
int32	i;
int32	length;
int32	symbolpos;
int32	currpos;
modulerec	m;
char	filename[300];
char *	file;
char *	s;
char *	t;
    strcpy((char *)&filename,moduletable[nmodules].name);
    strcpy((char *)&filename,changeext((char *)&filename,".pc"));
    initpcdest();
    stringtable = (char * (*)[])alloctable(maxpcstrings,4);
    doprogramstartup();
    writezint('P');
    writezint('C');
    writezint(26);
    writezint(0);
    writezint(kkpclversion);
    writezstring("403");
    writezint(kkmoduletable);
    writezint(nmodules);
    for (i=1; i<=nmodules; ++i) {
        writezstring(moduletable[i].name);
    }
    writezint(kkdlltable);
    writezint(ndlltable);
    for (i=1; i<=ndlltable; ++i) {
        writezstring(dlltable[i-1]);
    }
    writezint(kkdllproctable);
    writezint(ndllproctable);
    for (i=1; i<=ndllproctable; ++i) {
        writezstring(dllproctable[i-1].name);
        writezint(dllproctable[i-1].dllindex);
    }
    writezint(kksymboltable);
    symbolpos = getpcpos();
    writezint4(0);
    for (i=1; i<=nmodules; ++i) {
        showpcsymbol(moduletable[i].stmodule);
    }
    for (i=tlast; i<=ntypes; ++i) {
        showpcsymbol(ttnamedef[i]);
    }
    for (i=0; i<=nmodules; ++i) {
        writesymbols(i);
    }
    currpos = getpcpos();
    setpcpos(symbolpos);
    writezint4(nsymbols);
    setpcpos(currpos);
    writezint(kktypetable);
    writezint(((ntypes-tlast)+1));
    for (i=tlast; i<=ntypes; ++i) {
        writezint(i);
        writezstring(ttname[i]);
        writezint(ttnamedef[i]->bcindex);
        writezint(ttbasetype[i]);
        writezint(tttarget[i]);
        writezint(ttlower[i]);
        writezint(ttlength[i]);
        writezint(ttsize[i]);
    }
    fixup_genfields();
    writezint(kkgenfieldnames);
    writezint(ngenfieldnames);
    for (i=1; i<=ngenfieldnames; ++i) {
        writezstring(genfieldnames[i-1].def->name);
        writezint(genfieldnames[i-1].dataindex);
        writezint(genfieldnames[i-1].datalength);
    }
    writezint(kkgenfielddata);
    writezint(ngenfielddata);
    for (i=1; i<=ngenfielddata; ++i) {
        writezint(genfielddata[i-1].fieldindex);
        writezint(genfielddata[i-1].recordtype);
        writezint(genfielddata[i-1].fieldtype);
        writezint(genfielddata[i-1].offset);
    }
    writezint(kkstringtable);
    writezint(nstrings);
    for (i=1; i<=nstrings; ++i) {
        writezstring((*stringtable)[i-1]);
    }
    writestructfields();
    for (i=0; i<=nmodules; ++i) {
        writepccode2pc(i);
    }
    writezint(kkend);
    writezeof();
    length = getpcpos();
    writepcdata((char *)&filename);
    if (fverbose) {
        printf("%s %s %d %s\n","Finished writing",(char*)(&filename),length,"bytes");
    }
}

static void writesymbols	(int32 mx) {
int64 *	p;
int64 *	pccode;
strec *	d;
int32	cmd;
int32	i;
int32	index;
int32	av_2;
    pccode = p = (int64 *)moduletable[mx].pccode;
    while (1) {
        cmd = *p++;
        for (i=1; i<=cmdnopnds[cmd]; ++i) {
            switch (cmdfmt[cmd][i-1]) {
            case cproc:
                d = (strec *)(int32)*p;
                showpcsymbol(d);
                *p = d->bcindex;
                break;
            case cdllproc:
                d = (strec *)(int32)*p;
                *p = d->index;
                break;
            case cmemory:
                d = (strec *)(int32)*p;
                showpcsymbol(d);
                *p = d->bcindex;
                break;
            case cframe:
                d = (strec *)(int32)*p;
                *p = (d->index*varsize);
                break;
            case cint32:
                if (cmd == kcall && i == 2) {
                    *p = *p*varsize;
                }
                else if (cmd == kcallptr && i == 2) {
                    *p = *p*varsize;
                }
                else if (cmd == kaddsp && i == 1) {
                    *p = *p*varsize;
                }
                break;
            case cstring:
                if (nstrings >= maxpcstrings) {
                    loaderror("pc: too many strings","");
                }
                *p = addstringtotable((char *)(int32)*p);
                break;
            default:;
            }
            ++p;
        }
        if (cmd == kendmodule || cmd == 0) {
            goto L227;
        }
    }
L227:;
}

static void showpcsymbol	(strec * d) {
int32	a;
int32	b;
char	c;
    a = b = 0;
    if (d->bcindex == 0) {
        d->bcindex = ++nsymbols;
        if ((d->nameid==procid)) {
            c = 'P';
            a = d->index;
        } else if ((d->nameid==staticid)) {
            c = 'S';
        } else if ((d->nameid==moduleid) || (d->nameid==extmoduleid)) {
            c = 'M';
            a = d->attribs.ax_moduleno;
        } else if ((d->nameid==typeid)) {
            c = 'T';
            a = d->mode;
        } else if ((d->nameid==programid)) {
            return;
        }
        else {
            loaderror("SHOWPCSYM?","");
        }
        writezint((uchar)c);
        writezstring(d->name);
        writezint(d->owner->bcindex);
        writezint(a);
        writezint(b);
        if ((uchar)c == 'P') {
            if (!!d->metadata) {
                writezstring(d->metadata);
            }
            else {
                writezstring("");
            }
        }
    }
}

static int32 addstringtotable	(char * s) {
int32	i;
    (*stringtable)[++nstrings-1] = s;
    return nstrings;
}

static void writepccode2pc	(int32 mx) {
int64 *	p;
int64 *	pccode;
uint16 (*linetable)[];
strec *	d;
int32	cmd;
int32	i;
int32	index;
int32	pcindex;
int32	startindex;
int32	av_3;
    writezint(kkpccode);
    writezint(mx);
    writezint(moduletable[mx].pcindex);
    pccode = p = (int64 *)moduletable[mx].pccode;
    linetable = moduletable[mx].linetable;
    pcindex = 1;
    while (1) {
        cmd = *p++;
        writezint((*linetable)[pcindex]);
        writezint(cmd);
        startindex = pcindex++;
        for (i=1; i<=cmdnopnds[cmd]; ++i) {
            switch (cmdfmt[cmd][i-1]) {
            case clabel:
            case cproc:
            case cdllproc:
            case cmemory:
            case cframe:
            case cint32:
            case cint:
            case cword:
            case cstring:
            case ctype:
            case cgenfield:
                writezint(*p);
                break;
            case creal:
                writezreal(*(double*)&*p);
                break;
            case crange:
                writezrange((byte *)p);
                break;
            default:;
                printf("%s\n",opndnames[cmdfmt[cmd][i-1]]);
                loaderror("writepc2bc/opnd?","");
            }
            ++pcindex;
            ++p;
        }
        if (cmd == kendmodule || cmd == 0) {
            goto L243;
        }
    }
L243:;
}

static void writestructfields	(void) {
int32	structpos;
int32	currpos;
int32	nstructfields;
int32	i;
int32	j;
int32	t;
strec *	d;
strec *	e;
enum {maxfields = 100};
strec *	fieldlist[100];
byte	ignore[100];
int32	nfields;
    writezint(kkstructtable);
    structpos = getpcpos();
    writezint4(0);
    nstructfields = 0;
    for (t=tlast; t<=ntypes; ++t) {
        if (ttbasetype[t] == tstruct) {
            d = ttnamedef[t]->deflist;
            nfields = 0;
            while (d) {
                if (d->nameid == fieldid && !d->attribs.ax_at) {
                    ++nfields;
                    if (nfields > maxfields) {
                        loaderror("wsf: too many fields in struct","");
                    }
                    fieldlist[nfields-1] = d;
                    ignore[nfields-1] = 0;
                }
                d = d->nextdef;
            }
            for (i=1; i<=nfields; ++i) {
                d = fieldlist[i-1];
                for (j=i+1; j<=nfields; ++j) {
                    if (i != j) {
                        e = fieldlist[j-1];
                        if (d->offset == e->offset) {
                            ignore[i-1] = 1;
                        }
                    }
                }
            }
            for (i=nfields; i>=1; --i) {
                if (!ignore[i-1]) {
                    d = fieldlist[i-1];
                    ++nstructfields;
                    writezint(t);
                    writezstring(d->name);
                    writezint(d->mode);
                    writezint(d->offset);
                }
            }
        }
    }
    currpos = getpcpos();
    setpcpos(structpos);
    writezint4(nstructfields);
    setpcpos(currpos);
}

static void fixup_genfields	(void) {
int32	recordtype;
int32	fieldtype;
int32	i;
strec *	d;
strec *	p;
strec *	p0;
    ngenfielddata = 0;
    for (i=1; i<=ngenfieldnames; ++i) {
        d = genfieldnames[i-1].def;
        genfieldnames[i-1].dataindex = ngenfielddata+1;
        p = d->nextdupl;
        while (p) {
            if (p->nameid == fieldid) {
                recordtype = p->owner->mode;
                fieldtype = p->mode;
            }
            else if (p->nameid == procid && p->owner->nameid != moduleid && p->owner->nameid != extmoduleid) {
                recordtype = p->owner->mode;
                fieldtype = trefproc;
            }
            else if (p->nameid == linkid && p->owner->nameid != moduleid) {
                p0 = p;
                do {
                    p = p->equiv;
                } while (!(p->nameid != linkid));
                recordtype = p0->owner->mode;
                fieldtype = trefproc;
            }
            else {
                goto L281;
            }
            ++ngenfielddata;
            genfielddata[ngenfielddata-1].fieldindex = i;
            genfielddata[ngenfielddata-1].recordtype = recordtype;
            genfielddata[ngenfielddata-1].fieldtype = fieldtype;
            genfielddata[ngenfielddata-1].offset = p->offset;
skip:
L281:;
            p = p->nextdupl;
        }
        genfieldnames[i-1].datalength = (ngenfielddata-genfieldnames[i-1].dataindex)+1;
    }
}

static void showoptions	(void) {
int32	i;
    printf("\n");
    printf("%s\n","Options:");
    printf("%s\n","--------");
    if (!!(passes&mload)) {
        printf("%s\n","-load");
    }
    if (!!(passes&mparse)) {
        printf("%s\n","-parse");
    }
    if (!!(passes&mcodegen)) {
        printf("%s\n","-gen");
    }
    if (!!(passes&mwritepc)) {
        printf("%s\n","-pc");
    }
    if (!!fshowoptions) {
        printf("%s\n","-options");
    }
    if (!!fshowmodules) {
        printf("%s\n","-modules");
    }
    if (!!fshowast) {
        printf("%s\n","-ast");
    }
    if (!!fshowst) {
        printf("%s\n","-st");
    }
    if (!!fshowstflat) {
        printf("%s\n","-stflat");
    }
    if (!!fshowtypes) {
        printf("%s\n","-types");
    }
    if (!!fshowpcl) {
        printf("%s\n","-pcl");
    }
    if (!!fshowdis) {
        printf("%s\n","-dis");
    }
    if (!!fshowtiming) {
        printf("%s\n","-time");
    }
    if (!!fshowlog) {
        printf("%s\n","-d");
    }
    if (!!fshowlog) {
        printf("%s\n","<showlog>");
    }
    printf("\n");
}

static void showoutput	(void) {
    if (!!fshowmodules) {
        showmodules();
    }
    if (!!fshowast) {
        showast();
    }
    if (!!fshowst) {
        showsttree();
    }
    if (!!fshowtiming) {
        showtiming();
    }
}

static void showhelp	(void) {
static char *	helptext="THIS IS THE HELP TEXT";
    printf("%s\n",helptext);
    exit(0);
}

/* Notes:

   qcc64.c is intended to be compiled as a 64-bit application. qcc32.c 32-bit
   version on request.

   To test, download hello.q or create such a file containing:

       proc start =
          println "Hello, World."
       end

   Then then use:

       qcc hello                            # Windows
       ./qcc hello                          # Linux

   to turn hello.q into hello.pc. Execute it with the interpreter (see
   details on pcc64.c.)

   For other examples, browse:

       https://github.com/bartg/langs/tree/master/Q-Examples
       https://github.com/bartg/langs/tree/master/qlang

   for .q source files.

   This composite C file was created from the multiple modules,  auto-translated
   from the implementation language ('M') into C and then collated into a
   single file.

   The original modules are named:

        mm_clib
        mm_mainc
        mm_nos
        mm_mlib
        mm_nosdll
        var_types
        pq_common
        qc_tables
        var_decls
        qc_lex
        support
        qc_lib
        qc_parselib
        qc_parse
        qc_pcllib
        qc_pclgen
        q_libs
        start
*/
