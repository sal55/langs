global type pcl = ref pclrec

global record pstrec =
	ichar name
	ref pstrec owner
	ref pstrec nextpst
!	int32 offset
	word32 mode
	word32 size
	byte id
	byte flags:(isglobal:1, isexport:1, isimport:1, iscallback:1,
		isframe:1, isequiv:1, isstart:1)
	union
		int16 offset
		int16 labelno
	end
	int index
	int count
	int lvalue

end

global tabledata() [0:]ichar pstnames =
	(no_name = 0,		$),
	(proc_name,			$),
	(dllproc_name,		$),
	(istatic_name,		$),
	(zstatic_name,		$),
	(param_name,		$),
	(frame_name,		$),
	(module_name,		$),
end

global tabledata() \
		[0:]ichar stdnames,
		[0:]byte stdbits,
		[0:]byte stdcodes,
		[0:]byte stdtabtype,
		[0:]byte stdtabtype2,
		[0:]byte stdpcltype,
		[0:]byte stdcat,
		[0:]byte stdcat2 =
!    type        name    bits   code   tabtype     tabtype2    pcltype     cat         cat2
    (tvoid=0,    "void",    0,    0,   tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),

    (tc64,       "c64",    64,   'C',  tc64,       tc64,       tu64,       d64_cat,    d64_cat  ),
    (tu64,       "u64",    64,   'U',  tu64,       tu64,       tu64,       d64_cat,    d64_cat  ),
    (tu128,      "u128",  128,   'U',  tu128,      tu128,      tu128,      wide_cat,   wide_cat ),
    (ti64,       "i64",    64,   'I',  ti64,       ti64,       ti64,       d64_cat,    d64_cat  ),
    (ti128,      "i128",  128,   'I',  ti128,      ti128,      ti128,      wide_cat,   wide_cat ),
    (tr32,       "r32",    32,   'R',  tr32,       tr32,       tr32,       x32_cat,    short_cat),
    (tr64,       "r64",    64,   'R',  tr64,       tr64,       tr64,       x64_cat,    d64_cat  ),
    (tdecimal,   "dec",    64,   'D',  tdecimal,   tvar,       tdecimal,   var_cat,    var_cat  ),

    (trange,     "range", 128,   'G',  trange,     trange,     tu128,      wide_cat,   wide_cat ),
    (tstring,    "str",    64,     0,  tstring,    tvar,       tstring,    var_cat,    var_cat  ),
    (tset,       "set",    64,     0,  tset,       tvar,       tset,       var_cat,    var_cat  ),
    (tref,       "ref",    64,   'P',  tref,       tref,       tu64,       d64_cat,    d64_cat  ),
    (trefchar,   "ichar",  64,   'P',  trefchar,   tref,       tu64,       d64_cat,    d64_cat  ),
    (trefbit,    "refbt", 128,   'Q',  trefbit,    trefbit,    tu128,      wide_cat,   wide_cat ),
    (tarray,     "array",   0,   'A',  tblock,     tblock,     tblock,     block_cat,  block_cat),
    (tslice,     "slice", 128,     0,  tslice,     tslice,     tu128,      wide_cat,   wide_cat ),
    (tbits,      "bits",    0,     0,  tblock,     tblock,     tblock,     block_cat,  block_cat),
    (tlist,      "list",   64,     0,  tlist,      tvar,       tlist,      var_cat,    var_cat  ),
    (tdict,      "dict",   64,     0,  tdict,      tvar,       tdict,      var_cat,    var_cat  ),
    (trecord,    "rec",     0,     0,  tblock,     tblock,     tblock,     block_cat,  block_cat),
    (ttagunion,  "tagun",   0,     0,  tblock,     tblock,     tblock,     block_cat,  block_cat),

    (tblock,     "block",   0,     0,  tblock,     tblock,     tblock,     block_cat,  block_cat),

    (tenum,      "enum",   64,     0,  ti64,       ti64,       tu64,       d64_cat,    d64_cat  ),
    (trecordx,   "recx",   64,     0,  trecordx,   tvar,       trecordx,   var_cat,    var_cat  ),

    (tshort,     "short",   0,     0,  tshort,     tshort,     tvoid,      void_cat,   void_cat ),
    (tvar,       "var",    64,     0,  tvar,       tvar,       tvar,       void_cat,   void_cat ),

    (tc8,        "c8",      8,   'C',  tshort,     tshort,     tu8,        short_cat,  short_cat),
    (tc16,       "c16",    16,   'C',  tshort,     tshort,     tu16,       short_cat,  short_cat),
    (ti8,        "i8",      8,   'I',  tshort,     tshort,     ti8,        short_cat,  short_cat),
    (ti16,       "i16",    16,   'I',  tshort,     tshort,     ti16,       short_cat,  short_cat),
    (ti32,       "i32",    32,   'I',  tshort,     tshort,     ti32,       short_cat,  short_cat),
    (tu8,        "u8",      8,   'U',  tshort,     tshort,     tu8,        short_cat,  short_cat),
    (tu16,       "u16",    16,   'U',  tshort,     tshort,     tu16,       short_cat,  short_cat),
    (tu32,       "u32",    32,   'U',  tshort,     tshort,     tu32,       short_cat,  short_cat),

    (tu1,        "u1",      1,   'B',  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (tu2,        "u2",      2,   'B',  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (tu4,        "u4",      4,   'B',  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),

    (tauto,      "auto",    0,     0,  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (tany,       "any",     0,     0,  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (tproc,      "proc",    0,     0,  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (tlabel,     "label",   0,     0,  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (tgen,       "gen",     0,     0,  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (ttype,      "type",   64,     0,  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (tbitfield,  "bitfl",   8,     0,  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (ttuple,     "tuple",   0,     0,  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (tpending,   "pend",    0,     0,  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),

    (tparam1,    "pm1",     0,     0,  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (tparam2,    "pm2",     0,     0,  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (tparam3,    "pm3",     0,     0,  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (tparam4,    "pm4",     0,     0,  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),

    (tlast,      "last ",   0,     0,  tlast,      tlast,      tlast,      void_cat,   void_cat ),
end

global const tuser	= tlast

global const tint	= ti64
global const tword	= tu64
global const treal	= tr64
global const tdec	= tdecimal
global const tfirstnum	= tc64
global const tlastnum	= tdecimal

global const tfirsttabletype	= tc64			!bounds used in optables
global const tlasttabletype		= tenum

!global const tfirstnumtype = ti8
!global const tlastnumtype  = tr64
global const maxtuplesize = 4

global int trefproc
global int treflabel

global []int typerank=(
	1,	!tc64
	2,	!tu64
	4,	!tu128
	3,	!ti64
	5,	!ti128
	6,	!tr32
	7,	!tr64
	8)	!tdecimal


global tabledata() [0:]ichar typecatnames =
	(void_cat=0,	"void"),
	(short_cat,		"short"),		!u8/u16/u32 i8/i16/i64 normally widened to u64/i64
	(d64_cat,		"d64"),			!i64, u64, pointers, r64 as data; anything of that size
	(x32_cat,		"x32"),			!r32, which are not normally widened to r64
	(x64_cat,		"x64"),			!r64
	(wide_cat,		"wide"),		!u128/i128, also used for slices, anything of that size
	(block_cat,		"block"),		!N-byte block of any size, that is not 1/2/4/8/16 bytes
	(var_cat,		"var"),			!u64 pointer/refernce to flex string/array etc
end

!global const plastcat=var_cat

global tabledata() [0:]ichar opndnames =
	(no_opnd=0,			$),
	(mem_opnd,			$),
	(memaddr_opnd,		$),
	(int_opnd,			$),
	(int128_opnd,		$),
	(real_opnd,			$),
	(real32_opnd,		$),
	(string_opnd,		$),		!data string
	(label_opnd,		$),
	(reg_opnd,			$),
	(xreg_opnd,			$),
	(metastring_opnd,	$),		!for comments, names of ext. functions etc
end

global record pclrec =
	union
		int64 value
		real64 xvalue
		real32 xvalue32
		ref int128 pvalue128
		ichar svalue
		ref pstrec def
		int tempno
		int labelno
	end
	byte opndtype
	ref pclrec nextpcl
	byte opcode
!	byte flags:(isglobal:1, isvariadic:1, nestedcall:1)
	byte flags:(isglobal:1, isvariadic:1)
	byte align

	word16 opindex				!spec-op
	union
		byte oldmode			!for ifix/ifloat
		byte newmode			!for truncate
!		byte memmode			!for pushptr etc
	end
	byte spare1

	int32 pos:(lineno:24, fileno:8)
	int32 mode
	int32 size
	int32 spare2
	[16]byte spare3

	union						!two 32-bit params used according to opcode
		struct					!pointer ops
			int32 scale			!scale factor for offset
			int32 extra			!extra constant byte offset, already scaled
		end
		struct					!call/etc
			int16 nargs			!number of args
!			int16 nslots		!number of 64-bit stack slots incl any alignment slot
			int16 nvariadics	!for ffi, parameter number at which variadics start
			union
				int16 nmult			!for callmult
				int16 fnindex		!for sysfn
			end
		end
		struct					!switch
			int32 minlab
			int32 maxlab
		end

!following single values set by genpc_n or genpc_cond or genpc_op
		int32 index				!general sequential index for setparam/temp etc
!		int32 fnindex			!sysfn index number
		int32 cond				!pcl condition code for jumpcc etc
		int32 step				!always +ve fixed step size for forup/fordown; also INCR
		int32 truncmode			!convert/truncate: truncated mode

		struct
			int32 x				!common access to these two params
			int32 y
		end
	end
end

global tabledata() [0:]ichar opndnames_ma =
	(a_none=0,	$),
	(a_reg,		$),		! Ri
	(a_imm,		$),		! d including def name, label etc
	(a_mem,		$),		! any memory modes: [d], [R], [R*4+R2+d+imm] etc
	(a_cond,	$),		! a condition code for jcc/setcc
	(a_xreg,	$),		! xmm register
	(a_wreg,	$),		! Wide integer register, means Ri and Ri+1
!	(a_string,	$),		! immediate string (for comments)
end

!Stack operands are:
!	Xa			1st of 1
!   Xb, Ya		1st/2nd of 2
!   Xc, Yb, Za	1st/2nd/3rd of 3
!Extra info:
! op		opindex
! fn		fnindex
! cc		cond code
! n         nargs for calls
! sx		scale and offset for ptr/offset ops

global tabledata() [0:]ichar pclnames, [0:]byte pcluseindex =
	(k_none = 0,		$,	0),	!      (0 0) 
	(k_comment,			$,	0),	!      (0 0) 
	(k_blank,			$,	0),	!      (0 0) 
	(k_end,				$,	0),	!      (0 0) 
	(k_debug,			$,	0),	!      (0 0) 
	(k_test,			$,	0),	!      (0 0) 

	(k_procdef,			$,	0),	!      (0 0) 
	(k_procend,			$,	0),	!      (0 0) 
	(k_procentry,		$,	0),	!      (0 0) 
	(k_label,			$,	0),	!      (0 0) 
	(k_labelname,		$,	0),	!      (0 0) 
	(k_frame,			$,	0),	!      (0 0) 
	(k_param,			$,	0),	!      (0 0) 
	(k_istatic,			$,	0),	!      (0 0) 
	(k_zstatic,			$,	0),	!      (0 0) 
	(k_initmemz,		$,	0),	!      (0 0) 
	(k_freemem,			$,	0),	!      (0 0) 
	(k_equiv,			$,	0),	!      (0 0) 
	(k_extern,			$,	0),	!      (0 0) 
	(k_endextern,		$,	0),	!      (0 0) 
	(k_info,			$,	0),	!      (0 0) 

	(k_startmult,		$,	0),	!      (0 0) 
	(k_resetmult,		$,	0),	!      (0 0) 
	(k_endmult,			$,	0),	!      (0 0) 

	(k_pushint,			$,	0),	!      (0 1)
	(k_pushint128,		$,	0),	!      (0 1)
	(k_pushreal,		$,	0),	!      (0 1)
	(k_pushreal32,		$,	0),	!      (0 1)
	(k_pushstring,		$,	0),	!      (0 1)
	(k_pushmem,			$,	0),	!      (0 1)
	(k_pushmemaddr,		$,	0),	!      (0 1)
	(k_popmem,			$,	0),	!      (1 0) 
	(k_storemem,		$,	0),	!      (1 1) 
	(k_opnd,			$,	0),	!      (0 0) 
	(k_addtoptr,		$,	0),	!      (1 1) 
	(k_suboffset,		$,	0),	!      (2 1) 
	(k_pushptroff,		$,	0),	!      (2 1) 
	(k_popptroff,		$,	0),	!      (3 0) 
	(k_storeptroff,		$,	0),	!      (3 1) 
	(k_pushptr,			$,	0),	!      (1 1) 
	(k_popptr,  		$,	0),	!      (2 0) 
	(k_storeptr,		$,	0),	!      (2 1) 
	(k_free,			$,	0),	!      (1 0) 
	(k_unstack,			$,	0),	!      (1 0) 
	(k_eval,			$,	0),	!      (1 0) 

	(k_callproc,		$,	0),	! n    (n 0) 
	(k_callfn,			$,	0),	! n    (n 1) 
	(k_callmult,		$,	0),	! n m  (n m) 
	(k_callprocptr,		$,	0),	! n    (n+1 0) 
	(k_callfnptr,		$,	0),	! n    (n+1 1) 
	(k_retproc,			$,	0),	!      (0 0) 
	(k_retfn,			$,	0),	!      (0 0) 
	(k_retmult,			$,	0),	!      (0 0) 
	(k_syscallfn,		$,	0),	! n fn (n 1) 
	(k_syscallproc,		$,	0),	! n fn (n 0) 
	(k_setret,			$,	0),	!      (1 1) 
	(k_setalign,		$,	0),	! n    (0 0) 

	(k_jump,			$,	0),	!      (0 0) goto L
	(k_jumpcc,			$,	1),	! cc   (2 0) goto L when cc
	(k_jumptrue,		$,	1),	!      (1 0) goto L when Xa is true
	(k_jumpfalse,		$,	1),	!      (1 0) L
	(k_jumpptr,			$,	0),	!      (1 0) goto Xa
	(k_jumpinrange,		$,	0),	!      (3 0) goto L when Xc in Yb..Za
	(k_jumpnotinrange,	$,	0),	!      (3 0) goto L when Xc not in Yb..Za
	(k_setjumpeq,		$,	0),	!      (2 1) goto L when 
	(k_setjumpeqx,		$,	0),	!      (2 2) 
	(k_setjumpne,		$,	0),	!      (2 2) 
	(k_setcc,			$,	1),	! cc   (2 1) X:=Xb cc Ya
	(k_casejumpeq,		$,	0),	!      (0 0) 

	(k_to,				$,	0),	!      (1 0) L
	(k_forup,			$,	0),	!      (2 0) L
	(k_fordown,			$,	0),	!      (2 0) L

	(k_swap,			$,	0),	!      (2 0) 
	(k_bin,				$,	1),	! op   (2 1) 
	(k_unary,			$,	1),	! op   (1 1) 
	(k_binto,			$,	1),	! op   (2 0) 
	(k_unaryto,			$,	1),	! op   (1 0) 
	(k_incr,			$,	1),	!      (1 0) 
	(k_incrx,			$,	1),	!      (1 1) 
	(k_convert,			$,	1),	!      (1 1) 
	(k_typepun,			$,	1),	!      (1 1) 
	(k_makerange,		$,	0),	!      (2 1) 
	(k_makeslice,		$,	0),	!      (2 1) 
	(k_makeset,			$,	0),	!      (n 1) 
	(k_makearray,		$,	0),	!      (n 1) 
	(k_dotindex,		$,	0),	!      (0 0) 
	(k_dotslice,		$,	0),	!      (0 0) 
	(k_popdotindex,		$,	0),	!      (0 0) 
	(k_storedotindex,	$,	0),	!      (0 0) 
	(k_storedotslice,	$,	0),	!      (0 0) 
	(k_switch,			$,	0),	!      (0 0) 
	(k_switchlabel,		$,	0),	!      (0 0) 
	(k_db,				$,	0),	!      (0 0) 
	(k_dw,				$,	0),	!      (0 0) 
	(k_dd,				$,	0),	!      (0 0) 
	(k_dq,				$,	0),	!      (0 0) 
	(k_assem,			$,	0),	!      (0 0) 
	(k_dummy,			$,	0),	!      (0 0) 
end

global tabledata() []ichar sysfnnames, []byte sysfnparams, []byte sysfnres =
!	(sysfn_pushcallback,		$,	0,	0),
!	(sysfn_popcallback,			$,	0,	0),
	(sysfn_mul_i128,			$,	2,	1),	
	(sysfn_idiv_i128,			$,	2,	0),
	(sysfn_dotindex,			$,	2,	0),
	(sysfn_dotslice,			$,	3,	0),
	(sysfn_popdotindex,			$,	2,	0),
	(sysfn_popdotslice,			$,	3,	0),
	(sysfn_power_i64,			$,	2,	0),

	(sysfn_cmp_block,			$,	2,	1),

	(sysfn_init,				$,	0,	0),
	(sysfn_initstatics,			$,	0,	0),
	(sysfn_stop,				$,	1,	0),
	(sysfn_print_startfile,		$,	0,	0),
	(sysfn_print_startstr,		$,	0,	0),
	(sysfn_print_startptr,		$,	0,	0),
	(sysfn_print_startcon,		$,	0,	0),
	(sysfn_print_setfmt,		$,	0,	0),
	(sysfn_print_nogap,			$,	0,	0),
	(sysfn_print_i64,			$,	0,	0),
	(sysfn_print_u64,			$,	0,	0),
	(sysfn_print_r64,			$,	0,	0),
	(sysfn_print_r32,			$,	0,	0),
	(sysfn_print_i128,			$,	0,	0),
	(sysfn_print_u128,			$,	0,	0),
	(sysfn_print_str,			$,	0,	0),
	(sysfn_print_strsl,			$,	0,	0),
	(sysfn_print_ptr,			$,	0,	0),
	(sysfn_print_c8,			$,	0,	0),
	(sysfn_print_newline,		$,	0,	0),
	(sysfn_print_end,			$,	0,	0),
	(sysfn_read_i64,			$,	0,	0),
	(sysfn_read_r64,			$,	0,	0),
	(sysfn_read_str,			$,	0,	0),
	(sysfn_read_fileline,		$,	0,	0),
	(sysfn_read_strline,		$,	0,	0),
	(sysfn_read_conline,		$,	0,	0),
!	(sysfn_fn_addresses,		$,	0,	0),

	(sysfn_get_nprocs,			$,	0,	0),
	(sysfn_get_nexports,		$,	0,	0),
	(sysfn_get_procname,		$,	0,	0),
	(sysfn_get_procaddr,		$,	0,	0),
	(sysfn_get_procexport,		$,	0,	0),

	(sysfn_nprocs,				$,	0,	0),
	(sysfn_nexports,			$,	0,	0),
	(sysfn_procnames,			$,	0,	0),
	(sysfn_procaddrs,			$,	0,	0),
	(sysfn_procexports,			$,	0,	0),

	(sysfn_sin,					$,	0,	0),
	(sysfn_cos,					$,	0,	0),
	(sysfn_tan,					$,	0,	0),
	(sysfn_asin,				$,	0,	0),
	(sysfn_acos,				$,	0,	0),
	(sysfn_atan,				$,	0,	0),
	(sysfn_ln,					$,	0,	0),
	(sysfn_lg,					$,	0,	0),
	(sysfn_log,					$,	0,	0),
	(sysfn_exp,					$,	0,	0),
	(sysfn_floor,				$,	0,	0),
	(sysfn_ceil,				$,	0,	0),
	(sysfn_fract,				$,	0,	0),
	(sysfn_round,				$,	0,	0),
	(sysfn_lenstr_stringz,		$,	0,	0),

	(sysfn_atan2,				$,	0,	0),
	(sysfn_fmod,				$,	0,	0),

!var support

	(sysfn_initmemz_var,		$,	0,	0),
	(sysfn_freemem_var,			$,	0,	0),
	(sysfn_free_var,			$,	0,	0),
	(sysfn_share_var,			$,	0,	0),
	(sysfn_unshare_var,			$,	0,	0),
	(sysfn_dupl_var,			$,	0,	0),
	(sysfn_popmem_var,			$,	0,	0),
	(sysfn_storemem_var,		$,	0,	0),

	(sysfn_add_dec,         $,	0,	0),
	(sysfn_sub_dec,         $,	0,	0),
	(sysfn_mul_dec,         $,	0,	0),
	(sysfn_div_dec,         $,	0,	0),
	(sysfn_irem_dec,        $,	0,	0),
	(sysfn_shr_dec,         $,	0,	0),
!	(sysfn_notin_dec,       $,	0,	0),
	(sysfn_min_dec,         $,	0,	0),
	(sysfn_max_dec,         $,	0,	0),
	(sysfn_neg_dec,         $,	0,	0),
	(sysfn_abs_dec,         $,	0,	0),
	(sysfn_istruel_dec,     $,	0,	0),
	(sysfn_sqr_dec,         $,	0,	0),
	(sysfn_sqrt_dec,        $,	0,	0),
	(sysfn_addto_dec,       $,	0,	0),
	(sysfn_subto_dec,       $,	0,	0),
	(sysfn_multo_dec,       $,	0,	0),
	(sysfn_divto_dec,       $,	0,	0),
	(sysfn_idivto_dec,      $,	0,	0),
	(sysfn_iremto_dec,      $,	0,	0),
	(sysfn_shlto_dec,       $,	0,	0),
	(sysfn_shrto_dec,       $,	0,	0),
	(sysfn_minto_dec,       $,	0,	0),
	(sysfn_maxto_dec,       $,	0,	0),
	(sysfn_negto_dec,       $,	0,	0),
	(sysfn_absto_dec,       $,	0,	0),

	(sysfn_add_var,				$,	0,	0),
	(sysfn_sub_var,				$,	0,	0),
	(sysfn_mul_var,				$,	0,	0),
	(sysfn_div_var,				$,	0,	0),
	(sysfn_idiv_var,			$,	0,	0),
	(sysfn_irem_var,			$,	0,	0),
	(sysfn_power_var,			$,	0,	0),
	(sysfn_eq_var,				$,	0,	0),
	(sysfn_ne_var,				$,	0,	0),
	(sysfn_lt_var,				$,	0,	0),
	(sysfn_le_var,				$,	0,	0),
	(sysfn_ge_var,				$,	0,	0),
	(sysfn_gt_var,				$,	0,	0),
	(sysfn_isequal_var,			$,	0,	0),
	(sysfn_iand_var,			$,	0,	0),
	(sysfn_ior_var,				$,	0,	0),
	(sysfn_ixor_var,			$,	0,	0),
	(sysfn_shl_var,				$,	0,	0),
	(sysfn_shr_var,				$,	0,	0),
	(sysfn_andl_var,			$,	0,	0),
	(sysfn_orl_var,				$,	0,	0),
	(sysfn_append_var,			$,	0,	0),
	(sysfn_concat_var,			$,	0,	0),
	(sysfn_min_var,				$,	0,	0),
	(sysfn_max_var,				$,	0,	0),
	(sysfn_in_var,				$,	0,	0),
	(sysfn_notin_var,			$,	0,	0),

	(sysfn_neg_var,				$,	0,	0),
	(sysfn_abs_var,				$,	0,	0),
	(sysfn_inot_var,			$,	0,	0),
	(sysfn_notl_var,			$,	0,	0),
	(sysfn_istruel_var,		$,	0,	0),
	(sysfn_sqr_var,			$,	0,	0),
	(sysfn_sqrt_var,			$,	0,	0),
	(sysfn_sin_var,				$,	0,	0),
	(sysfn_cos_var,				$,	0,	0),
	(sysfn_tan_var,				$,	0,	0),
	(sysfn_asin_var,			$,	0,	0),
	(sysfn_acos_var,			$,	0,	0),
	(sysfn_atan_var,			$,	0,	0),
	(sysfn_exp_var,				$,	0,	0),
	(sysfn_ln_var,				$,	0,	0),
	(sysfn_log_var,				$,	0,	0),
	(sysfn_round_var,			$,	0,	0),
	(sysfn_floor_var,			$,	0,	0),
	(sysfn_ceil_var,			$,	0,	0),
	(sysfn_fract_var,			$,	0,	0),
	(sysfn_asc_var,				$,	0,	0),
	(sysfn_chr_var,				$,	0,	0),
	(sysfn_lwb_var,				$,	0,	0),
	(sysfn_upb_var,				$,	0,	0),
	(sysfn_len_var,				$,	0,	0),
	(sysfn_bounds_var,			$,	0,	0),
!	(sysfn_share_var,			$,	0,	0),
!	(sysfn_unshare_var,			$,	0,	0),
!	(sysfn_free_var,			$,	0,	0),
!	(sysfn_dupl_var,			$,	0,	0),

	(sysfn_addto_var,			$,	0,	0),
	(sysfn_subto_var,			$,	0,	0),
	(sysfn_multo_var,			$,	0,	0),
	(sysfn_divto_var,			$,	0,	0),
	(sysfn_idivto_var,			$,	0,	0),
	(sysfn_iremto_var,			$,	0,	0),
	(sysfn_iandto_var,			$,	0,	0),
	(sysfn_iorto_var,			$,	0,	0),
	(sysfn_ixorto_var,			$,	0,	0),
	(sysfn_shlto_var,			$,	0,	0),
	(sysfn_shrto_var,			$,	0,	0),
	(sysfn_andto_var,			$,	0,	0),
	(sysfn_orto_var,			$,	0,	0),
	(sysfn_andlto_var,			$,	0,	0),
	(sysfn_orlto_var,			$,	0,	0),
	(sysfn_appendto_var,		$,	0,	0),
	(sysfn_concatto_var,		$,	0,	0),
	(sysfn_minto_var,			$,	0,	0),
	(sysfn_maxto_var,			$,	0,	0),

	(sysfn_negto_var,			$,	0,	0),
	(sysfn_absto_var,			$,	0,	0),
	(sysfn_inotto_var,			$,	0,	0),
	(sysfn_notlto_var,			$,	0,	0),
	(sysfn_istruelto_var,		$,	0,	0),
	(sysfn_incrto_var,			$,	0,	0),
	(sysfn_decrto_var,			$,	0,	0),

	(sysfn_new_var,				$,	0,	0),
	(sysfn_print_var,			$,	0,	0),
	(sysfn_tostr_var,			$,	0,	0),
	(sysfn_getdot_var,			$,	0,	0),
	(sysfn_putdot_var,			$,	0,	0),
	(sysfn_getindex_var,		$,	0,	0),
	(sysfn_putindex_var,		$,	0,	0),
	(sysfn_getdotindex_var,		$,	0,	0),
	(sysfn_putdotindex_var,		$,	0,	0),
	(sysfn_getslice_var,		$,	0,	0),
	(sysfn_putslice_var,		$,	0,	0),
	(sysfn_getdotslice_var,		$,	0,	0),
	(sysfn_putdotslice_var,		$,	0,	0),
	(sysfn_getkeyindex_var,		$,	0,	0),
	(sysfn_putkeyindex_var,		$,	0,	0),
	(sysfn_insert_var,			$,	0,	0),
	(sysfn_delete_var,			$,	0,	0),
	(sysfn_resize_var,			$,	0,	0),

	(sysfn_make_int,			$,	0,	0),
	(sysfn_make_real,			$,	0,	0),
	(sysfn_make_string,			$,	0,	0),
	(sysfn_make_dec,			$,	0,	0),
	(sysfn_make_list,			$,	0,	0),
	(sysfn_make_listz,			$,	0,	0),
	(sysfn_make_array,			$,	0,	0),
	(sysfn_make_range,			$,	0,	0),

	(sysfn_var_to_int,			$,	0,	0),
	(sysfn_var_to_real,			$,	0,	0),
	(sysfn_var_to_string,		$,	0,	0),

end

global [sysfnnames.len]byte sysfnmap

!!---
!List of all generic operations
global tabledata() [0:]ichar genopnames =
	(error_op=0,		$),

!binary
	(add_op,			$),
	(sub_op,			$),
	(mul_op,			$),
	(div_op,			$),
	(idiv_op,			$),
	(irem_op,			$),
	(iand_op,			$),
	(ior_op,			$),
	(ixor_op,			$),
	(shl_op,			$),
	(shr_op,			$),
	(in_op,				$),
	(notin_op,			$),
	(min_op,			$),
	(max_op,			$),
	(concat_op,			$),
	(append_op,			$),

!unary
	(neg_op,			$),
	(abs_op,			$),
	(inot_op,			$),

!unary maths
	(sqr_op,			$),
	(sqrt_op,			$),
	(sin_op,			$),
	(cos_op,			$),
	(tan_op,			$),
	(asin_op,			$),
	(acos_op,			$),
	(atan_op,			$),
	(ln_op,				$),
	(lg_op,				$),
	(log_op,			$),
	(exp_op,			$),
	(round_op,			$),
	(floor_op,			$),
	(ceil_op,			$),
	(fract_op,			$),
	(sign_op,			$),

!binary maths
	(atan2_op,			$),
	(power_op,			$),
	(fmod_op,			$),

!unary properties
	(lwb_op,			$),
	(upb_op,			$),
	(len_op,			$),
	(bounds_op,			$),
	(lenstr_op,			$),
	(bitwidth_op,		$),
	(bytesize_op,		$),
	(typestr_op,		$),
	(minvalue_op,		$),
	(maxvalue_op,		$),
	(sliceptr_op,		$),

!inplace increment
	(incr_op,			$),
	(decr_op,			$),
	(incrload_op,		$),
	(decrload_op,		$),
	(loadincr_op,		$),
	(loaddecr_op,		$),

!inplace binary
	(addto_op,			$),
	(subto_op,			$),
	(multo_op,			$),
	(divto_op,			$),
	(idivto_op,			$),
	(iremto_op,			$),
	(iandto_op,			$),
	(iorto_op,			$),
	(ixorto_op,			$),
	(shlto_op,			$),
	(shrto_op,			$),
	(appendto_op,		$),
	(concatto_op,		$),
	(minto_op,			$),
	(maxto_op,			$),

!inplace unary
	(negto_op,			$),
	(absto_op,			$),
	(inotto_op,			$),

!conversions
	(softconv_op,		$),
	(widen_op,			$),
	(float_op,			$),
	(fix_op,			$),
	(todec_op,			$),
	(decto_op,			$),
	(softtrunc_op,		$),
	(fwiden,			$),
	(fnarrow,			$),

	(truncate_op,		$),
	(arraytoslice_op,	$),
	(ichartostring_op,	$),
	(stringtostring_op,	$),
	(tostr_op,			$),
	(tostrfmt_op,		$),

!comparisons
	(eq_op,				$),
	(ne_op,				$),
	(lt_op,				$),
	(le_op,				$),
	(ge_op,				$),
	(gt_op,				$),
	(same_op,			$),

!logical; binary/unary
	(andl_op,			$),
	(orl_op,			$),
	(notl_op,			$),
	(istruel_op,		$),

!inplace logical
	(andlto_op,			$),
	(orlto_op,			$),
	(notlto_op,			$),
	(istruelto_op,		$),

!indexing and slicing (load value)
	(index_op,			$),
	(slice_op,			$),
	(dot_op,			$),
	(dotindex_op,		$),
	(dotslice_op,		$),

!indexing and slicing (store value)
	(indexto_op,		$),
	(sliceto_op,		$),
	(dotto_op,			$),
	(dotindexto_op,		$),
	(dotsliceto_op,		$),

!misc
	(assign_op,			$),
	(makerange_op,		$),
!	(pushint_op,		$),
!	(pushreal_op,		$),
!	(pushstring_op,		$),
!	(pushmem_op,		$),
!	(popmem_op,			$),
end

global const firstgenop = genopnames.lwb
global const lastgenop = genopnames.upb

!!---
global tabledata() [0:]ichar specopnames =
	(op_error=0,          $),
	(op_add_i64,          $),
	(op_add_i128,         $),
	(op_add_r32,          $),
	(op_add_r64,          $),
	(op_add_dec,          $),
	(op_add_var,          $),
	(op_add_refoff,       $),

	(op_sub_i64,          $),
	(op_sub_i128,         $),
	(op_sub_r32,          $),
	(op_sub_r64,          $),
	(op_sub_dec,          $),
	(op_sub_ref,          $),
	(op_sub_var,          $),
	(op_sub_refoff,       $),

	(op_mul_i64,          $),
	(op_mul_i128,         $),
	(op_mul_r32,          $),
	(op_mul_r64,          $),
	(op_mul_dec,          $),
	(op_mul_var,          $),

	(op_div_r32,          $),
	(op_div_r64,          $),
	(op_div_dec,          $),

	(op_idiv_u64,         $),
	(op_idiv_i64,         $),
	(op_idiv_u128,        $),
	(op_idiv_i128,        $),
	(op_idiv_dec,         $),

	(op_irem_u64,         $),
	(op_irem_i64,         $),
	(op_irem_u128,        $),
	(op_irem_i128,        $),
	(op_irem_dec,         $),

	(op_iand_i64,         $),
	(op_iand_i128,        $),
	(op_iand_var,         $),

	(op_ior_i64,          $),
	(op_ior_i128,         $),
	(op_ior_var,          $),

	(op_ixor_i64,         $),
	(op_ixor_i128,        $),
	(op_ixor_var,         $),

!	(op_shl_u64,          $),
	(op_shl_i64,          $),
	(op_shl_u128,         $),
	(op_shl_i128,         $),
	(op_shl_dec,          $),

	(op_shr_u64,          $),
	(op_shr_i64,          $),
	(op_shr_u128,         $),
	(op_shr_i128,         $),
	(op_shr_dec,          $),

	(op_min_u64,          $),
	(op_min_i64,          $),
	(op_min_u128,         $),
	(op_min_i128,         $),
	(op_min_r32,          $),
	(op_min_r64,          $),
	(op_min_dec,          $),

	(op_max_u64,          $),
	(op_max_i64,          $),
	(op_max_u128,         $),
	(op_max_i128,         $),
	(op_max_r32,          $),
	(op_max_r64,          $),
	(op_max_dec,          $),

	(op_concat_var,       $),

	(op_append_var,       $),

	(op_neg_i64,          $),
	(op_neg_i128,         $),
	(op_neg_r32,          $),
	(op_neg_r64,          $),
	(op_neg_dec,          $),

	(op_abs_i64,          $),
	(op_abs_i128,         $),
	(op_abs_r32,          $),
	(op_abs_r64,          $),
	(op_abs_dec,          $),

	(op_inot_i64,         $),
	(op_inot_i128,        $),
	(op_inot_var,         $),

	(op_sqr_i64,          $),
	(op_sqr_i128,         $),
	(op_sqr_r32,          $),
	(op_sqr_r64,          $),
	(op_sqr_dec,          $),

	(op_sqrt_i64,         $),
	(op_sqrt_i128,        $),
	(op_sqrt_r32,         $),
	(op_sqrt_r64,         $),
	(op_sqrt_dec,         $),

	(op_sin_r32,          $),
	(op_sin_r64,          $),
	(op_sin_dec,          $),

	(op_cos_r32,          $),
	(op_cos_r64,          $),
	(op_cos_dec,          $),

	(op_tan_r32,          $),
	(op_tan_r64,          $),
	(op_tan_dec,          $),

	(op_asin_r32,         $),
	(op_asin_r64,         $),
	(op_asin_dec,         $),

	(op_acos_r32,         $),
	(op_acos_r64,         $),
	(op_acos_dec,         $),

	(op_atan_r32,         $),
	(op_atan_r64,         $),
	(op_atan_dec,         $),

	(op_ln_r32,           $),
	(op_ln_r64,           $),
	(op_ln_dec,           $),

	(op_lg_r32,           $),
	(op_lg_r64,           $),
	(op_lg_dec,           $),

	(op_log_r32,          $),
	(op_log_r64,          $),
	(op_log_dec,          $),

	(op_exp_r32,          $),
	(op_exp_r64,          $),
	(op_exp_dec,          $),

	(op_round_r32,        $),
	(op_round_r64,        $),
	(op_round_dec,        $),

	(op_floor_r32,        $),
	(op_floor_r64,        $),
	(op_floor_dec,        $),

	(op_ceil_r32,         $),
	(op_ceil_r64,         $),
	(op_ceil_dec,         $),

	(op_fract_r32,        $),
	(op_fract_r64,        $),
	(op_fract_dec,        $),

	(op_sign_i64,         $),
	(op_sign_r32,         $),
	(op_sign_r64,         $),
	(op_sign_dec,         $),

	(op_atan2_r32,        $),
	(op_atan2_r64,        $),
	(op_atan2_dec,        $),

	(op_power_u64,        $),
	(op_power_i64,        $),
	(op_power_u128,       $),
	(op_power_i128,       $),
	(op_power_r32,        $),
	(op_power_r64,        $),
	(op_power_dec,        $),

	(op_fmod_r32,         $),
	(op_fmod_r64,         $),
	(op_fmod_dec,         $),

	(op_lwb_var,          $),

	(op_upb_slice,        $),
	(op_upb_var,          $),

	(op_len_slice,        $),
	(op_len_var,          $),

	(op_bounds_slice,     $),
	(op_bounds_var,       $),

	(op_lenstr_var,       $),

	(op_sliceptr_slice,   $),

	(op_incr_short,       $),		!stands for all short types
	(op_incr_i64,         $),
	(op_incr_i128,        $),
	(op_incr_dec,         $),
	(op_incr_ref,         $),
	(op_incr_enum,        $),

	(op_decr_short,       $),
	(op_decr_i64,         $),
	(op_decr_i128,        $),
	(op_decr_dec,         $),
	(op_decr_ref,         $),
	(op_decr_enum,        $),

	(op_incrload_short,   $),
	(op_incrload_i64,     $),
	(op_incrload_i128,    $),
	(op_incrload_dec,     $),
	(op_incrload_ref,     $),
	(op_incrload_enum,    $),

	(op_decrload_short,   $),
	(op_decrload_i64,     $),
	(op_decrload_i128,    $),
	(op_decrload_dec,     $),
	(op_decrload_ref,     $),
	(op_decrload_enum,    $),

	(op_loadincr_short,   $),
	(op_loadincr_i64,     $),
	(op_loadincr_i128,    $),
	(op_loadincr_dec,     $),
	(op_loadincr_ref,     $),
	(op_loadincr_enum,    $),

	(op_loaddecr_short,   $),
	(op_loaddecr_i64,     $),
	(op_loaddecr_i128,    $),
	(op_loaddecr_r32,     $),
	(op_loaddecr_r64,     $),
	(op_loaddecr_dec,     $),
	(op_loaddecr_ref,     $),
	(op_loaddecr_enum,    $),

	(op_addto_short,      $),
	(op_addto_i64,        $),
	(op_addto_i128,       $),
	(op_addto_r32,        $),
	(op_addto_r64,        $),
	(op_addto_dec,        $),
	(op_addto_var,        $),
	(op_addto_refoff,     $),

	(op_subto_short,      $),
	(op_subto_i64,        $),
	(op_subto_i128,       $),
	(op_subto_r32,        $),
	(op_subto_r64,        $),
	(op_subto_dec,        $),
	(op_subto_refoff,     $),

	(op_multo_short,      $),
	(op_multo_i64,        $),
	(op_multo_i128,       $),
	(op_multo_r32,        $),
	(op_multo_r64,        $),
	(op_multo_dec,        $),

	(op_divto_r32,        $),
	(op_divto_r64,        $),
	(op_divto_dec,        $),

	(op_idivto_short,     $),
	(op_idivto_u64,       $),
	(op_idivto_i64,       $),
	(op_idivto_u128,      $),
	(op_idivto_i128,      $),
	(op_idivto_dec,       $),

	(op_iremto_short,     $),
	(op_iremto_u64,       $),
	(op_iremto_i64,       $),
	(op_iremto_u128,      $),
	(op_iremto_i128,      $),
	(op_iremto_dec,       $),

	(op_iandto_short,     $),
	(op_iandto_i64,       $),
	(op_iandto_i128,      $),

	(op_iorto_short,      $),
	(op_iorto_i64,        $),
	(op_iorto_i128,       $),

	(op_ixorto_short,     $),
	(op_ixorto_i64,       $),
	(op_ixorto_i128,      $),

	(op_shlto_short,      $),
	(op_shlto_i64,        $),
	(op_shlto_u128,       $),
	(op_shlto_i128,       $),
	(op_shlto_dec,        $),

	(op_shrto_short,      $),
	(op_shrto_u64,        $),
	(op_shrto_i64,        $),
	(op_shrto_u128,       $),
	(op_shrto_i128,       $),
	(op_shrto_dec,        $),

	(op_appendto_var,     $),

	(op_concatto_var,     $),

	(op_minto_u64,        $),
	(op_minto_i64,        $),
	(op_minto_u128,       $),
	(op_minto_i128,       $),
	(op_minto_r32,        $),
	(op_minto_r64,        $),
	(op_minto_dec,        $),

	(op_maxto_u64,        $),
	(op_maxto_i64,        $),
	(op_maxto_u128,       $),
	(op_maxto_i128,       $),
	(op_maxto_r32,        $),
	(op_maxto_r64,        $),
	(op_maxto_dec,        $),

	(op_negto_short,      $),
	(op_negto_i64,        $),
	(op_negto_i128,       $),
	(op_negto_r32,        $),
	(op_negto_r64,        $),
	(op_negto_dec,        $),

	(op_absto_short,      $),
	(op_absto_i64,        $),
	(op_absto_i128,       $),
	(op_absto_r32,        $),
	(op_absto_r64,        $),
	(op_absto_dec,        $),

	(op_inotto_short,     $),
	(op_inotto_i64,       $),
	(op_inotto_i128,      $),

	(op_softconv,         $),
	(op_softtrunc_128_64, $),
	(op_widen_u64_u128,   $),
	(op_widen_i64_i128,   $),
	(op_float_u64_r32,    $),
	(op_float_u64_r64,    $),
	(op_float_i64_r32,    $),
	(op_float_i64_r64,    $),
	(op_fix_r32_u64,      $),
	(op_fix_r32_i64,      $),
	(op_fix_r64_u64,      $),
	(op_fix_r64_i64,      $),
	(op_todec_u64_dec,    $),
	(op_todec_i64_dec,    $),
	(op_todec_r32_dec,    $),
	(op_todec_r64_dec,    $),
	(op_decto_dec_u64,    $),
	(op_decto_dec_i64,    $),
	(op_decto_dec_r32,    $),
	(op_decto_dec_r64,    $),

	(op_fwiden_r32_r64,   $),
	(op_fnarrow_r64_r32,  $),

!	(op_truncate_u64,     $),
!	(op_truncate_i64,     $),
!	(op_truncate_u128,    $),
!	(op_truncate_i128,    $),

	(op_truncate_i128,   $),		!hard truncation from i128 to 8/16/32/64
	(op_truncate_i64,    $),		!hard truncation from i64 to 8/16/32

	(op_ichartostring,    $),

	(op_tostr_c64,        $),
	(op_tostr_u64,        $),
	(op_tostr_i64,        $),
	(op_tostr_u128,       $),
	(op_tostr_i128,       $),
	(op_tostr_r32,        $),
	(op_tostr_r64,        $),
	(op_tostr_dec,        $),
	(op_tostr_ref,        $),
	(op_tostr_enum,       $),
	(op_tostr_var,        $),

	(op_tostrfmt_c64,     $),
	(op_tostrfmt_u64,     $),
	(op_tostrfmt_i64,     $),
	(op_tostrfmt_u128,    $),
	(op_tostrfmt_i128,    $),
	(op_tostrfmt_r32,     $),
	(op_tostrfmt_r64,     $),
	(op_tostrfmt_dec,     $),
	(op_tostrfmt_ref,     $),
	(op_tostrfmt_enum,    $),
	(op_tostrfmt_var,     $),

	(op_eq_i64,           $),
	(op_eq_i128,          $),
	(op_eq_r32,           $),
	(op_eq_r64,           $),
	(op_eq_dec,           $),
	(op_eq_ref,           $),
	(op_eq_enum,          $),
	(op_eq_var,           $),
	(op_eq_block,         $),

	(op_ne_i64,           $),
	(op_ne_i128,          $),
	(op_ne_r32,           $),
	(op_ne_r64,           $),
	(op_ne_dec,           $),
	(op_ne_ref,           $),
	(op_ne_enum,          $),
	(op_ne_var,           $),
	(op_ne_block,         $),

	(op_lt_u64,           $),
	(op_lt_i64,           $),
	(op_lt_u128,          $),
	(op_lt_i128,          $),
	(op_lt_r32,           $),
	(op_lt_r64,           $),
	(op_lt_dec,           $),
	(op_lt_ref,           $),
	(op_lt_enum,          $),
	(op_lt_var,           $),

	(op_le_u64,           $),
	(op_le_i64,           $),
	(op_le_u128,          $),
	(op_le_i128,          $),
	(op_le_r32,           $),
	(op_le_r64,           $),
	(op_le_dec,           $),
	(op_le_ref,           $),
	(op_le_enum,          $),
	(op_le_var,           $),

	(op_ge_u64,           $),
	(op_ge_i64,           $),
	(op_ge_u128,          $),
	(op_ge_i128,          $),
	(op_ge_r32,           $),
	(op_ge_r64,           $),
	(op_ge_dec,           $),
	(op_ge_ref,           $),
	(op_ge_enum,          $),
	(op_ge_var,           $),

	(op_gt_u64,           $),
	(op_gt_i64,           $),
	(op_gt_u128,          $),
	(op_gt_i128,          $),
	(op_gt_r32,           $),
	(op_gt_r64,           $),
	(op_gt_dec,           $),
	(op_gt_ref,           $),
	(op_gt_enum,          $),
	(op_gt_var,           $),

	(op_same_var,         $),

	(op_andl_i64,         $),

	(op_orl_i64,          $),

	(op_notl_i64,         $),

	(op_istruel_i64,      $),
	(op_istruel_i128,     $),
	(op_istruel_r32,      $),
	(op_istruel_r64,      $),
	(op_istruel_dec,      $),
	(op_istruel_ref,      $),
	(op_istruel_enum,     $),
	(op_istruel_var,      $),

	(op_andlto_i64,       $),

	(op_orlto_i64,        $),

	(op_notlto_i64,       $),

	(op_istruelto_i64,    $),

	(op_dummy,            $),
end

global ref pclrec allpclcode

global int labelno=0
global [sysfnnames.len]int sysfnlabels
global [sysfnnames.len]int sysfnproclabels

global int mlineno
global byte fshowpst
