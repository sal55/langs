import* mm_pcl

!import mm_decls
!import* mm_pcl

global tabledata() 	[0:]ichar stdnames,
		[0:]byte stdbits,
		[0:]byte stdcodes,
		[0:]byte stdtopcl =
!    type        name    bits   code   tabtype     tabtype2    pcltype     cat         cat2
    (tvoid=0,    "void",    0,    0,	tpvoid),

    (tc64,       "c64",    64,   'C',	tpu64),
    (tu64,       "u64",    64,   'U',	tpu64),
    (tu128,      "u128",  128,   'U',	tpu128),
    (ti64,       "i64",    64,   'I',	tpi64),
    (ti128,      "i128",  128,   'I',	tpi128),
    (tr32,       "r32",    32,   'R',	tpr32),
    (tr64,       "r64",    64,   'R',	tpr64),

    (trange,     "range", 128,   'G',	tpu128),  
    (tref,       "ref",    64,   'P',	tpu64),  
    (trefchar,   "ichar",  64,   'P',	tpu64),  
    (tarray,     "array",   0,   'A',	tpblock),  
    (tslice,     "slice", 128,     0,	tpu128),  
    (trecord,    "rec",     0,     0,	tpblock),  

    (tblock,     "block",   0,     0,	tpblock),  
    (tshort,     "short",   0,     0,	tpvoid),  

    (tc8,        "c8",      8,   'C',	tpu8),  
    (tc16,       "c16",    16,   'C',	tpu16),  
    (ti8,        "i8",      8,   'I',	tpi8),  
    (ti16,       "i16",    16,   'I',	tpi16),  
    (ti32,       "i32",    32,   'I',	tpi32),  
    (tu8,        "u8",      8,   'U',	tpu8),
    (tu16,       "u16",    16,   'U',	tpu16),
    (tu32,       "u32",    32,   'U',	tpu32),  

    (tenum,      "enum",   64,     0,	tpu64),  

    (tauto,      "auto",    0,     0,	tpvoid),  
    (tany,       "any",     0,     0,	tpvoid),  
    (tproc,      "proc",    0,     0,	tpvoid),  
    (tlabel,     "label",   0,     0,	tpvoid),  
    (ttype,      "type",   64,     0,	tpvoid),  
    (tbitfield,  "bitfl",   8,     0,	tpvoid),  
    (ttuple,     "tuple",   0,     0,	tpvoid),  
    (tpending,   "pend",    0,     0,	tpvoid),  

    (tlast,      "last ",   0,     0,	tpvoid),  
end

global const tuser	= tlast

global const tint	= ti64
global const tword	= tu64
global const treal	= tr64
global const tfirstnum	= tc64
global const tlastnum	= tr64

global const tfirsttabletype	= tc64			!bounds used in optables
global const tlasttabletype		= tenum

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
	7)	!tr64

global tabledata() []ichar sysfnnames, []byte sysfnparams, []byte sysfnres =
	(sysfn_init,				$,	0,	0),
	(sysfn_print_startfile,		$,	0,	0),
	(sysfn_print_startstr,		$,	0,	0),
	(sysfn_print_startptr,		$,	0,	0),
	(sysfn_print_startcon,		$,	0,	0),
	(sysfn_print_setfmt,		$,	0,	0),
	(sysfn_print_nogap,			$,	0,	0),
	(sysfn_print_space,			$,	0,	0),
	(sysfn_print_i64,			$,	0,	0),
	(sysfn_print_i64_nf,		$,	0,	0),
	(sysfn_print_u64,			$,	0,	0),
	(sysfn_print_r64,			$,	0,	0),
	(sysfn_print_r32,			$,	0,	0),
	(sysfn_print_i128,			$,	0,	0),
	(sysfn_print_u128,			$,	0,	0),
	(sysfn_print_str,			$,	0,	0),
	(sysfn_print_str_nf,		$,	0,	0),
	(sysfn_print_strsl,			$,	0,	0),
	(sysfn_print_ptr,			$,	0,	0),
	(sysfn_print_ptr_nf,		$,	0,	0),
	(sysfn_print_c8,			$,	0,	0),
	(sysfn_print_newline,		$,	0,	0),
	(sysfn_print_end,			$,	0,	0),
	(sysfn_read_i64,			$,	0,	0),
	(sysfn_read_r64,			$,	0,	0),
	(sysfn_read_str,			$,	0,	0),
	(sysfn_read_fileline,		$,	0,	0),
	(sysfn_read_strline,		$,	0,	0),
	(sysfn_read_conline,		$,	0,	0),

	(sysfn_getnprocs,			$,	0,	1),		!access functions
!	(sysfn_getnexports,			$,	0,	1),
	(sysfn_getprocname,			$,	0,	1),
	(sysfn_getprocaddr,			$,	0,	1),
!	(sysfn_getprocexport,		$,	0,	1),
end
!
global [sysfnnames.len]psymbol sysfnhandlers

!global [sysfnnames.len]int sysfnproclabels

global int mlineno
global byte fshowpst


!!---
global tabledata() [0:]ichar jtagnames, [0:]byte jisexpr =
!Basic units; these don't follow normal rules of params needing to be units or lists
!jisexpr=1/2 when unit returns a value; 1 means unary, 2 binary op,
! 3 means returns a value, but is not a unary or binary op 

!a,b,c are unitrec refs, which can be a single unit, or a linked-list chain
!(usually in forward order)
!	L means .a/b/c pointing to a unitlist; L can be nil for an empty list
!	u means .a/b/c pointing to a single unit
!	u/nil means can be nil


![a=u] means a is a unit/list, or is nil

	(j_none=0,		$,		0), ! For tagname lookups when tag is zero
	(j_const,		$,		3), ! value/etc=value, typeno=type code
	(j_null,		$,		3), ! Place holder unit: means 'param no present' when used where a param is expected
	(j_name,		$,		3), ! def=nameptr
	(j_namelv,		$,		3), ! def=nameptr
	(j_block,		$,		0), ! a=L
	(j_stmtblock,	$,		0), ! a=L
	(j_decimal,		$,		3), ! svalue=str, slength
	(j_assem,		$,		0), ! svalue=str, slength
	(j_assemmacro,	$,		0), !
	(j_assemreg,	$,		0), !
	(j_assemxreg,	$,		0), !
	(j_assemmem,	$,		0), !
	(j_strinclude,	$,		0), !

!Logical Operators

	(j_andl,		$,		2), ! a b	This group are for conditional expressions (no result)
	(j_orl,			$,		2), ! a b
	(j_xorl,		$,		2), ! a b
	(j_notl,		$,		1), ! a
	(j_istruel,		$,		1), ! a

!Expressions and Operators

	(j_makelist,	$,		3), ! a=L, b=[u], length=N; element list/lower bound expr
	(j_makerange,	$,		3), ! a b
	(j_makeset,		$,		3), ! a=L, length=N
	(j_makedict,	$,		3), !
	(j_makeslice,	$,		3), !
	(j_exprlist,	$,		3), ! a=u...	List of expressions, as (a;b;c,	0), rather than (a,b,c)
	(j_multexpr,	$,		3), !
	(j_returnmult,	$,		3), !

	(j_keyword,		$,		3), ! def=st entry
	(j_keyvalue,	$,		3), ! a b
	(j_assign,		$,		3), ! a b
	(j_deepcopy,	$,		3), ! a b
	(j_callfn,		$,		3), ! a b
!	(j_applyop,		$,		0), ! opcode b c
!	(j_applyopx,	$,		1), ! opcode b c
	(j_new,			$,		3), ! newmode=T, a=L, length=N
	(j_destroy,		$,		0), ! a=L, length=N
!	(j_clear,		$,		0), !

!	(j_setcc,		$,		2), ! a b
	(j_cmp,			$,		2), ! a b
	(j_cmpchain,	$,		1), ! a b
	(j_bin,			$,		2), ! a b
	(j_unary,		$,		1), ! a b
	(j_binto,		$,		2), ! a b
	(j_unaryto,		$,		1), ! a b
	(j_incr,		$,		3), ! a	++a

	(j_inrev,		$,		2), ! a b
	(j_inrange,		$,		2), ! a b
	(j_inset,		$,		2), ! a b
	(j_clamp,		$,		2), ! a b

	(j_flexptr,		$,		3), ! a b
	(j_stringz,		$,		3), ! a b
	(j_sliceptr,	$,		3), ! a b

	(j_index,		$,		3), ! a b		a[b]
	(j_indexlv,		$,		3), ! a b		a[b]
	(j_slice,		$,		3), ! a b		a[b]
	(j_dot,			$,		3), ! a b opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(j_dotlv,		$,		3), ! a b opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(j_dotindex,	$,		3), ! a b		a[b]
	(j_dotslice,	$,		3), ! a b		a[b]
	(j_anddotslice,	$,		3), ! a b		a[b]
	(j_anddotindex,	$,		3), ! a b		a[b]

	(j_ptr,			$,		3), ! a		a^
	(j_ptrlv,		$,		3), ! a		a^
	(j_addrof,		$,		3), ! a		&a
	(j_addroffirst,	$,		3), ! a		&a
	(j_convert,		$,		3), ! typeno=T a		T(a)			T
	(j_shorten,		$,		3), !
	(j_autocast,	$,		3), ! typeno=T a		T(a)			T
	(j_typepun,		$,		3), ! typeno=T a		T@(a)			T
	(j_typeconst,	$,		3), ! typeno=T			typeconst(T)
	(j_operator,	$,		3), ! opcode=opc
	(j_upper,		$,		3), ! a		$					T

	(j_bitwidth,	$,		1), ! a
	(j_bytesize,	$,		1), ! a
	(j_typeof,		$,		3), ! a
	(j_typestr,		$,		1), ! a
!	(j_sliceptr,	$,		1), ! a
	(j_bitfield,	$,		3), ! a

	(j_minvalue,	$,		3), ! a
	(j_maxvalue,	$,		3), ! a

!Translator Variables

	(j_cvlineno,	$,		3), ! 
	(j_cvstrlineno,	$,		3), ! 
	(j_cvmodulename,$,		3), ! 
	(j_cvfilename,	$,		3), ! 
	(j_cvfunction,	$,		3), ! 
	(j_cvdate,		$,		3), ! 
	(j_cvtime,		$,		3), ! 
	(j_cvversion,	$,		3), ! 
	(j_cvtypename,	$,		3), ! 
	(j_cvtargetbits,$,		3), ! 
	(j_cvtargetsize,$,		3), ! 
	(j_cvtargetcode,$,		3), ! 
	(j_cvnil,		$,		3), ! 
	(j_cvpi,		$,		3), ! 
	(j_cvtrue,		$,		3), ! 
	(j_cvfalse,		$,		3), ! 

	(j_whenthen,	$,		0), ! a=L b=u
	(j_elsif,		$,		0), ! opcode=condcode, a
	(j_fmtitem,		$,		3), ! a b  x/fmtstr
	(j_nogap,		$,		3), ! 
	(j_space,		$,		3), ! 

!Statements

	(j_callproc,	$,		0), ! a=fn b=L, length
	(j_return,		$,		0), ! a=x/nil
	(j_syscall,		$,		0), ! a=x or nil

!	(j_assign,		$,		3), ! a b
	(j_to,			$,		0), ! a=N, b=body, c=tempvar/nil, def=name
	(j_if,			$,		3), ! condcode a=then b=else
	(j_longif,		$,		3), ! a=(elsif ...) b=else		L is series of kelsif pairs
	(j_forup,		$,		0), ! 
	(j_fordown,		$,		0), !
	(j_forall,		$,		0), !
	(j_forallrev,	$,		0), !
	(j_while,		$,		0), ! a=x b=u
	(j_repeat,		$,		0), ! a=u b=x
	(j_goto,		$,		0), ! a=x
	(j_labeldef,	$,		0), ! def=nameptr
	(j_restart,		$,		0), ! [a=x]
	(j_redo,		$,		0), ! [a=x]
	(j_next,		$,		0), ! [a=x]
	(j_exit,		$,		0), ! [a=x]
	(j_do,			$,		0), ! [a=u
	(j_case,		$,		3), ! a=x b=L [c=else]		L is series of whenthen pairs
	(j_docase,		$,		0), ! a=x b=L [c=else]
	(j_switch,		$,		3), ! a=x b=L [c=else]
	(j_doswitch,	$,		0), ! a=x b=L [c=else]
	(j_swap,		$,		0), ! a b
	(j_select,		$,		3), ! Not implemented
	(j_recase,		$,		0), ! Not implemented
!	(j_recaseelse,	$,		0), ! Not implemented

	(j_print,		$,		0), ! [a=dev] b=L
	(j_println,		$,		0), ! [a=dev] b=L
	(j_fprint,		$,		0), ! [a=dev] b=fmtstr c=L
	(j_fprintln,	$,		0), ! [a=dev] b=fmtstr c=L
	(j_cprint,		$,		0), ! [a=dev] b=fmtstr c=L
	(j_cprintln,	$,		0), ! [a=dev] b=fmtstr c=L
	(j_sprint,		$,		0), !         b=L 
	(j_sfprint,		$,		0), !         b=L
	(j_read,		$,		0), ! [a=dev] b=L
	(j_readln,		$,		0), ! [a=dev] b=L
	(j_sread,		$,		0), ! [a=dev] b=L
	(j_sreadln,		$,		0), ! [a=dev] b=L
	(j_stop,		$,		0), ! [a=x]
	(j_try,			$,		0), ! a=try block; b=except list
	(j_except,		$,		0), ! a=except block; b=exception code list (constants)
	(j_yield,		$,		0), ! "
	(j_raise,		$,		0), ! "
!	(j_callhostproc,$,		0), ! "
	(j_eval,		$,		3), ! "
	(j_stack,		$,		0), ! "
	(j_unstack,		$,		0), ! "
	(j_empty,		$,		1), ! "

	(j_dummy,		$,		3)
end

global tabledata() []ichar bitfieldnames=
	(bf_msb,		$),
	(bf_lsb,		$),
	(bf_msbit,		$),
	(bf_lsbit,		$),
	(bf_msw,		$),
	(bf_lsw,		$),
	(bf_odd,		$),
	(bf_even,		$),
end

global tabledata() [0:]ichar optypenames =
	(no_op=0,		$),
	(bin_op,		$),
	(mon_op,		$),
	(prop_op,		$),
end

!!---
global tabledata() []ichar symbolnames,
					[]byte symboloptypes,
					[]byte symbolgenops,
					[]byte symbolgentoops,
					[]byte symbolopprios,
					[]byte exprstarter =
!First half are basic tokens returned by lexreadtoken()
	(errorsym,			$,			0,	0,	0,	0,	0),		! Lex error
	(dotsym,			".",		0,	0,	0,	0,	0),		! "."
	(lexdotsym,			$,			0,	0,	0,	0,	0),		! ".", used at bol to prefix lexical 
	(anddotsym,			"&.",		0,	0,	0,	0,	1),		! "&."
	(commasym,			",",		0,	0,	0,	0,	0),		! ","
	(semisym,			";",		0,	0,	0,	0,	0),		! ";"
	(colonsym,			":",		0,	0,	0,	0,	0),		! ":"
	(dcolonsym,			"::",		0,	0,	0,	0,	0),		! "::"
	(assignsym,			":=",		bin_op,	0,	0,	1,	0),		! :=
	(deepcopysym,		"::=",		0,	0,	0,	1,	0),		! ::=
	(sendtosym,			"=>",		0,	0,	0,	0,	0),		! =>
	(lbracksym,			"(",		0,	0,	0,	0,	1),		! (
	(rbracksym,			")",		0,	0,	0,	0,	0),		! )
	(lsqsym,			"[",		0,	0,	0,	0,	1),		! [
	(rsqsym,			"]",		0,	0,	0,	0,	0),		! ]
	(lcurlysym,			"{",		0,	0,	0,	0,	0),		! {
	(rcurlysym,			"}",		0,	0,	0,	0,	0),		! }
	(ptrsym,			"^",		0,	0,	0,	0,	0),		! ^
	(barsym,			"|",		0,	0,	0,	0,	0),		! |
	(dbarsym,			"||",		0,	0,	0,	0,	0),		! ||
	(atsym,				"@",		0,	0,	0,	0,	0),		! @
	(datsym,			"@@",		0,	0,	0,	0,	0),		! @@
	(questionsym,		"?",		0,	0,	0,	0,	0),		! ?
	(addrsym,			"&",		0,	0,	0,	0,	1),		! &
	(daddrsym,			"&&",		0,	0,	0,	0,	0),		! &&
	(curlsym,			"~",		0,	0,	0,	0,	0),		! ~
	(rangesym,			"..",		bin_op,	0,	0,	5,	0),		! ..
	(ellipsissym,		"...",		0,	0,	0,	0,	0),		! ...
	(hashsym,			"#",		0,	0,	0,	0,	0),		! #

!	(opsym,				$,		0,	0,	0,	0,	0),		! Any operator or property tag (use sets to distinguish)

	(addsym,			"+",		bin_op,		kadd,		kaddto,		4,	1),
	(subsym,			"-",		bin_op,		ksub,		ksubto,		4,	1),
	(mulsym,			"*",		bin_op,		kmul,		kmulto,		3,	0),
	(divsym,			"/",		bin_op,		kdiv,		kdivto,		3,	0),
	(idivsym,			"%",		bin_op,		kidiv,		kidivto,	3,	0),
	(iremsym,			"rem",		bin_op,		kirem,		kiremto,	3,	0),
	(iandsym,			"iand",		bin_op,		kiand,		kiandto,	4,	0),
	(iorsym,			"ior",		bin_op,		kior,		kiorto,		4,	0),
	(ixorsym,			"ixor",		bin_op,		kixor,		kixorto,	4,	0),
	(shlsym,			"<<",		bin_op,		kshl,		kshlto,		3,	0),
	(shrsym,			">>",		bin_op,		kshr,		kshrto,		3,	0),
	(minsym,			"min",		bin_op,		kmin,		kminto,		4,	1),
	(maxsym,			"max",		bin_op,		kmax,		kmaxto,		4,	1),
	(andlsym,			"and",		bin_op,		kandl,		kandlto,	7,	0),
	(orlsym,			"or",		bin_op,		korl,		korlto,		8,	0),
	(xorlsym,			"xor",		bin_op,		0,			0,			8,	0),

	(eqsym,				"=",		bin_op,		keq,		0,			6,	1),
	(cmpsym,			"cmp",		bin_op,		0,			0,			6,	1),
!	(appendsym,			"append",	bin_op,		kappend,	kappendto,	4,	0),
!	(concatsym,			"concat",	bin_op,		kconcat,	kconcatto,	4,	0),
	(powersym,			"**",		bin_op,		kpower,		0,			2,	0),
	(samesym,			"==",		bin_op,		ksame,		0,			6,	0),
	(insym,				"in",		bin_op,		kin,		0,			6,	0),
	(notinsym,			"notin",	bin_op,		knotin,		0,			6,	0),
	(inrevsym,			"inrev",	0,			0,			0,			0,	0),

	(negsym,			"$neg",		mon_op,		kneg,		0,			0,	1),
	(notlsym,			"not",		mon_op,		knotl,		knotlto,	0,	1),
	(istruelsym,		"istrue",	mon_op,		kistruel,	kistruelto,	0,	1),
	(inotsym,			"inot",		mon_op,		kinot,		kinotto,	0,	1),
	(abssym,			"abs",		mon_op,		kabs,		kabsto,		0,	1),
	(signsym,			"sign",		mon_op,		ksign,		0,			0,	1),
	(sqrtsym,			"sqrt",		mon_op,		ksqrt,		0,			0,	1),
	(sqrsym,			"sqr",		mon_op,		ksqr,		0,			0,	1),

	(propsym,				$,		prop_op,	0,			0,			0,	0),
	(mathsopsym,		$,		0,	0,	0,	0,	1),		! sin etc
	(maths2opsym,		$,		0,	0,	0,	0,	1),		! atan2 etc

	(bitfieldsym,		$,		0,	0,	0,	0,	0),		! Special bit selections
	(eolsym,			$,		0,	0,	0,	0,	0),		! End of line
	(eofsym,			$,		0,	0,	0,	0,	0),		! Eof seen
	(rawxnamesym,		$,		0,	0,	0,	0,	0),		! unassigned name, case-sensitive, that is never a reserved word
	(docstringsym,		$,		0,	0,	0,	0,	0),		! ! #comment used as documentation string
	(incrsym,			$,		0,	0,	0,	0,	1),		! 1/2 = ++/--; later may add +2 for x++/x--
	(intconstsym,		$,		0,	0,	0,	0,	1),		! 123 32 bits signed
	(decimalconstsym,	$,		0,	0,	0,	0,	1),		! 123 or 123.4 decimal
	(realconstsym,		$,		0,	0,	0,	0,	1),		! 123.4 64 bits
	(charconstsym,		$,		0,	0,	0,	0,	1),		! 'A' or 'ABCD'
	(wcharconstsym,		$,		0,	0,	0,	0,	1),		! 'A'W or 'ABCD'W (but don't have a syntax yet)
	(stringconstsym,	$,		0,	0,	0,	0,	1),		! "ABC"
	(astringconstsym,	$,		0,	0,	0,	0,	1),		! A"ABC"
	(wstringconstsym,	$,		0,	0,	0,	0,	1),		! "ABC"W

!Second half are tokens that can be yielded after a name lookup::
	(unitnamesym,		$,		0,	0,	0,	0,	0),		! 
	(namesym,			$,		0,	0,	0,	0,	1),		! identifier symbol
	(ksourcedirsym,		$,		0,	0,	0,	0,	0),		! 
	(kstrincludesym,	$,		0,	0,	0,	0,	1),		! 
!	(lexmacronamesym,	$,		0,	0,	0,	0,	0),		! 
	(regsym,			$,		0,	0,	0,	0,	0),		! x64 registers
	(xregsym,			$,		0,	0,	0,	0,	0),		! XMM registers
	(fregsym,			$,		0,	0,	0,	0,	0),		! ST registers
	(mregsym,			$,		0,	0,	0,	0,	0),		! MMX registers
	(jmpccsym,			$,		0,	0,	0,	0,	0),		! 
	(setccsym,			$,		0,	0,	0,	0,	0),		! 
	(movccsym,			$,		0,	0,	0,	0,	0),		! 
	(segnamesym,		$,		0,	0,	0,	0,	0),		! 
	(asmopcodesym,		$,		0,	0,	0,	0,	0),		! MOV etc

	(stdtypesym,		$,		0,	0,	0,	0,	1),		! INT, CHAR etc
	(machinetypesym,	$,		0,	0,	0,	0,	1),		! INTM etc
!	(packtypesym,		$,		0,	0,	0,	0,	0),		! Byte etc
	(ktypeofsym,		$,		0,	0,	0,	0,	0),		! TYPEOF
	(ksubrangesym,		$,		0,	0,	0,	0,	0),		! SUBRANGE
	(koutsym,			$,		0,	0,	0,	0,	0),		! OUT
	(kicharsym,			$,		0,	0,	0,	0,	1),		! ICHAR
	(kifsym,			$,		0,	0,	0,	0,	1),		! 
	(kthensym,			$,		0,	0,	0,	0,	0),		! 
	(kelsifsym,			$,		0,	0,	0,	0,	0),		! 
	(kelsesym,			$,		0,	0,	0,	0,	0),		! 
	(kelsecasesym,		$,		0,	0,	0,	0,	0),		! 
	(kelseswitchsym,	$,		0,	0,	0,	0,	0),		! 
	(kelseselectsym,	$,		0,	0,	0,	0,	0),		! 
	(kendsym,			$,		0,	0,	0,	0,	0),		! 
	(kunlesssym,		$,		0,	0,	0,	0,	0),		! 
	(kcasesym,			$,		0,	0,	0,	0,	1),		! CASE
	(kdocasesym,		$,		0,	0,	0,	0,	0),		! DOCASE
	(krecasesym,		$,		0,	0,	0,	0,	0),		! RECASE
	(kwhensym,			$,		0,	0,	0,	0,	0),		! 
	(kforsym,			$,		0,	0,	0,	0,	0),		! FOR
	(ktosym,			$,		0,	0,	0,	0,	0),		! TO/DOWNTO
	(kbysym,			$,		0,	0,	0,	0,	0),		! 
	(kdosym,			$,		0,	0,	0,	0,	0),		! 
	(kwhilesym,			$,		0,	0,	0,	0,	0),		! 
	(krepeatsym,		$,		0,	0,	0,	0,	0),		! 
	(kuntilsym,			$,		0,	0,	0,	0,	0),		! 
	(kreturnsym,		$,		0,	0,	0,	0,	0),		! 
	(kstopsym,			$,		0,	0,	0,	0,	0),		! 
	(kloopsym,			$,		0,	0,	0,	0,	0),		! EXIT/NEXT/LOOP/REDO/RESTART
	(kstepsym,			$,		0,	0,	0,	0,	0),		! STEP
	(kgotosym,			$,		0,	0,	0,	0,	0),		! GO/GOTO
	(kswitchsym,		$,		0,	0,	0,	0,	0),		! SWITCH
	(kdoswitchsym,		$,		0,	0,	0,	0,	0),		! DOSWITCH
	(kprintsym,			$,		0,	0,	0,	0,	0),		! PRINT/PRINTLN/FPRINT/FPRINTLN
	(ksprintsym,		$,		0,	0,	0,	0,	0),		! SPRINT/SFPRINT
	(kreadsym,			$,		0,	0,	0,	0,	0),		! READ/READLN
	(ksreadsym,			$,		0,	0,	0,	0,	0),		! SREAD
	(ksreadlnsym,		$,		0,	0,	0,	0,	0),		! SREADLN
	(kprocsym,			$,		0,	0,	0,	0,	0),		! PROC
	(kfunctionsym,		$,		0,	0,	0,	0,	0),		! FUNCTION
!	(kmethodsym,		$,		0,	0,	0,	0,	0),		! METHOD
	(klabelsym,			$,		0,	0,	0,	0,	0),		! LABEL
	(krecordsym,		$,		0,	0,	0,	0,	0),		! RECORD
	(kstructsym,		$,		0,	0,	0,	0,	0),		! STRUCT
	(kunionsym,			$,		0,	0,	0,	0,	0),		! UNION
	(kimportsym,		$,		0,	0,	0,	0,	0),		! IMPORT
	(kimportmodulesym,	$,		0,	0,	0,	0,	0),		! IMPORTDLL/IMPORTMODULE
	(kimportpathsym,	$,		0,	0,	0,	0,	0),		! IMPORTPATH
	(kmapmodulesym,		$,		0,	0,	0,	0,	0),		! MAPMODULE
	(ktypesym,			$,		0,	0,	0,	0,	0),		! TYPE
	(ktypealiassym,		$,		0,	0,	0,	0,	0),		! TYPEALIAS
	(kextendtypesym,	$,		0,	0,	0,	0,	0),		! EXTENDTYPE
	(krefsym,			$,		0,	0,	0,	0,	1),		! REF
	(kmutsym,			$,		0,	0,	0,	0,	0),		! MUT
	(kletsym,			$,		0,	0,	0,	0,	0),		! LET
	(kslicesym,			$,		0,	0,	0,	0,	0),		! SLICE/SLICE2D
	(karraysym,			$,		0,	0,	0,	0,	0),		! ARRAY
	(kdictsym,			$,		0,	0,	0,	0,	0),		! DICT
!	(kflexsym,			$,		0,	0,	0,	0,	0),		! FLEX
	(kmacrosym,			$,		0,	0,	0,	0,	0),		! MACRO
	(kexpandsym,		$,		0,	0,	0,	0,	0),		! EXPAND
	(koperatorsym,		$,		0,	0,	0,	0,	0),		! OPERATOR
	(kconstsym,			$,		0,	0,	0,	0,	0),		! 
	(kenumsym,			$,		0,	0,	0,	0,	0),		! 
	(knewsym,			$,		0,	0,	0,	0,	0),		! NEW
	(kdestroysym,		$,		0,	0,	0,	0,	0),		! DESTROY
	(kclearsym,			$,		0,	0,	0,	0,	0),		! CLEAR
	(kclasssym,			$,		0,	0,	0,	0,	0),		! CLASS
!	(kdirectivesym,		$,		0,	0,	0,	0,	0),		! TARGET/MODULE
	(kfflangsym,		$,		0,	0,	0,	0,	0),		! JLANG CLANG WINDOWS HOST
	(kglobalsym,		$,		0,	0,	0,	0,	0),		! global
	(kstaticsym,		$,		0,	0,	0,	0,	0),		! STATIC

	(ktrysym,			$,		0,	0,	0,	0,	0),		! 
	(kexceptsym,		$,		0,	0,	0,	0,	0),		! 
	(kfinallysym,		$,		0,	0,	0,	0,	0),		! 
	(kraisesym,			$,		0,	0,	0,	0,	0),		! 
	(kyieldsym,			$,		0,	0,	0,	0,	0),		! 
	(kcastsym,			$,		0,	0,	0,	0,	1),		! CAST
	(ktypeconstsym,		$,		0,	0,	0,	0,	0),		! TYPECONST
	(compilervarsym,	$,		0,	0,	0,	0,	1),		! $lineno etc
	(dollarsym,			$,		0,	0,	0,	0,	1),		! to be used for current array upperbound; also tabledata names
	(kevalsym,			$,		0,	0,	0,	0,	0),		! EVAL
	(ktabledatasym,		$,		0,	0,	0,	0,	0),		! tabledata
	(kstacksym,			$,		0,	0,	0,	0,	0),		! STACK/UNSTACK
	(kclampsym,			$,		0,	0,	0,	0,	1),			! CLAMP
	(kswapsym,			$,		0,	0,	0,	0,	0),		! SWAP
	(kerrorsym,			$,		0,	0,	0,	0,	0),		! PC_ERROR etc
!	(sysconstsym,		$,		0,	0,	0,	0,	0),		! nil, etc
	(kassemsym,			$,		0,	0,	0,	0,	0),		! ASM/ASSEM
	(ksyscallsym,		$,		0,	0,	0,	0,	1),		! $get_procname etc
!	(kemitcsym,			$,		0,	0,	0,	0,	0),		! EMITC
	(kemptysym,			$,		0,	0,	0,	0,	0),		! EMPTY

	(kdummysym,			$,		0,	0,	0,	0,	0),		!
end

global tabledata() []ichar sourcedirnames =
	(includedir,	$),
	(strincludedir,	$),
	(binincludedir,	$),
	(textincludedir,$),
	(defineunitdir,	$),
!	(emitcdir,		$),
	(cclibdir,		$),
end

!global tabledata() =
!	(nil_const),
!	(pi_const),
!	(tab_const),
!	(con_const)
!end

global tabledata() [0:]ichar fflangnames=
	(noff=0,		$), ! 
	(windowsff,		$), ! 
	(clangff,		$), ! 
	(mlangff,		$), ! 
	(callbackff,	$), ! 
end

global tabledata() [0:]ichar scopenames=
	(local_scope=0,		$), ! 
	(program_scope,		$), ! 
	(export_scope,		$), ! 
	(exportq_scope,		$), ! 
end

global tabledata() =
	(thousand_unit),
	(million_unit),
	(billion_unit),
	(kilo_unit),
	(mega_unit),
	(giga_unit)
end

global tabledata() [0:]ichar parammodenames=
	(var_param=0,		"Var "),
	(in_param,			"In "),
	(out_param,			"Out "),
	(optional_param,	"Opt "),
end

!global tabledata() [0:]ichar namecatnames, [0:]byte qualifiedname =
!	(normal_cat=0,	 	"-",		0),
!	(proc_cat, 			"proc",		1),
!	(globalproc_cat,	"gproc",	1),
!	(dllproc_cat,		"dllproc",	0),
!	(dllmodule_cat,		"dllmodule",0),
!	(dllvar_cat,		"dllvar",	0),
!	(static_cat,		"static",	1),
!	(frame_cat,			"frame",	1),
!end

global tabledata() [0:]ichar namenames
	(nullid=0,		$),		!Not assigned (sometimes converted to genfieldid)
	(programid,		$),		!Main root
	(moduleid,		$),		!Current or imported module
	(dllmoduleid,	$),		!
	(typeid,		$),		!Type name in type, proc or module
	(procid,		$),		!Proc/method/function/op name
	(dllprocid,		$),		!Dll Proc/function name
	(dllvarid,		$),		!Dll variable name
	(genprocid,		$),		!generic proc name
	(generatorid,	$),		!generator proc name
	(constid,		$),		!Named constant in type, proc or module
	(staticid,		$),		!Static in type or proc or module
	(frameid,		$),		!Local var
	(paramid,		$),		!Local param
	(fieldid,		$),		!Field of Record or Class
	(genfieldid,	$),		!Generic Field of Record or Class
	(enumid,		$),		!Enum name, part of enum type only
	(labelid,		$),		!Label name in proc only
	(blockid,		$),		!Codeblock label name in proc only
	(aliasid,		$),		!Alias to another name
	(macroid,		$),		!Name of macro
	(macroparamid,	$),		!Macro formal parameter name
	(linkid,		$),		!Name in class defined in a base class
	(functionopid,	$),		!Function-operator
end

!global tabledata() [0:]ichar pstnames =
!	(no_name = 0,		$),
!	(proc_name,			$),
!	(dllproc_name,		$),
!	(istatic_name,		$),
!	(zstatic_name,		$),
!	(param_name,		$),
!	(frame_name,		$),
!end

!!---
global tabledata []ichar stnames, []int stsymbols, []int stsubcodes=

	("if",			kifsym,			j_if),
	("then",		kthensym,		0),
	("elsif",		kelsifsym,		j_if),
	("else",		kelsesym,		0),
	("elsecase",	kelsecasesym,	j_case),
	("elseswitch",	kelseswitchsym,	j_switch),
	("case",		kcasesym,		j_case),
	("docase",		kdocasesym,		j_docase),
	("recase",		krecasesym,		j_recase),
	("when",		kwhensym,		0),
	("for",			kforsym,		0),
	("forall",		kforsym,		0),
	("to",			ktosym,			0),
	("downto",		ktosym,			1),
	("by",			kbysym,			0),
	("do",			kdosym,			0),
	("end",			kendsym,		0),
	("while",		kwhilesym,		0),
	("repeat",		krepeatsym,		0),
	("until",		kuntilsym,		0),
	("always",		kuntilsym,		1),
	("return",		kreturnsym,		0),
	("yield",		kyieldsym,		0),
	("stop",		kstopsym,		0),
	("restart",		kloopsym,		j_restart),
	("redo",		kloopsym,		j_redo),
	("loop",		kloopsym,		j_redo),
	("next",		kloopsym,		j_next),
	("exit",		kloopsym,		j_exit),
	("$step",		kstepsym,		0),
	("goto",		kgotosym,		0),
	("go",			kgotosym,		1),
	("switch",		kswitchsym,		j_switch),
	("doswitch",	kdoswitchsym,	j_doswitch),
	("tabledata",	ktabledatasym,	0),
	("clamp",		kclampsym,		0),
	("eval",		kevalsym,		0),
!	("extendtype",	kextendtypesym,	0),
!	("$windows",	kcondcompsym,	windowsff),
!	("$linux",		kcondcompsym,	linuxff),

	("print",		kprintsym,		j_print),
	("println",		kprintsym,		j_println),
	("fprint",		kprintsym,		j_fprint),
	("fprintln",	kprintsym,		j_fprintln),
	("cprint",		kprintsym,		j_cprint),
	("cprintln",	kprintsym,		j_cprintln),
	("sprint",		ksprintsym,		j_sprint),
	("sfprint",		ksprintsym,		j_sfprint),

!	("stack",		kstacksym,		j_stack),
!	("unstack",		kstacksym,		j_unstack),

	("cp",			kprintsym,		j_print),
	("cpl",			kprintsym,		j_println),

	("read",		kreadsym,		j_read),
	("readln",		kreadsym,		j_readln),
	("cast",		kcastsym,		j_convert),

	("proc",		kprocsym,		0),
	("function",	kfunctionsym,	0),
	("threadedproc",		kprocsym,		1),
!	("threadedfunction",	kfunctionsym,	1),
!	("method",		kmethodsym,		0),

	("type",		ktypesym,		0),
	("class",		kclasssym,		0),
	("record",		krecordsym,		0),
	("struct",		kstructsym,		0),
	("union",		kunionsym,		0),
	("ref",			krefsym,		0),
	("pointer",		krefsym,		0),
	("returning",	sendtosym,		0),
	("var",			kmutsym,		0),
	("mut",			kmutsym,		0),
	("let",			kletsym,		0),

	("include",		ksourcedirsym,	includedir),
!	("strinclude",	ksourcedirsym,	strincludedir),
	("strinclude",	kstrincludesym,	0),
	("bininclude",	ksourcedirsym,	binincludedir),
!	("textinclude",	ksourcedirsym,	textincludedir),
!	("defineunit",	ksourcedirsym,	defineunitdir),
	("macro",		kmacrosym,		0),
!	("expand",		kexpandsym,		0),
!	("operator",	koperatorsym,	0),
!	("emitc",		ksourcedirsym,	emitcdir),
	("cclib",		ksourcedirsym,	cclibdir),

	("assem",		kassemsym,		1),
	("asm",			kassemsym,		0),

	("static",		kstaticsym,		0),
	
	("const",		kconstsym,		0),
!	("table",		kconstsym,		1),
	("enum",		kenumsym,		0),

	("$get_nprocs",		ksyscallsym,		sysfn_getnprocs),
	("$getnprocs",		ksyscallsym,		sysfn_getnprocs),

	("$get_procname",	ksyscallsym,		sysfn_getprocname),
	("$getprocname",	ksyscallsym,		sysfn_getprocname),

	("$get_procaddr",	ksyscallsym,		sysfn_getprocaddr),
	("$getprocaddr",	ksyscallsym,		sysfn_getprocaddr),

!	("$get_nexports",	ksyscallsym,		sysfn_get_nexports),
!	("$get_procexport",	ksyscallsym,		sysfn_get_procexport),

!	("$nprocs",			ksyscallsym,		sysfn_nprocs),
!	("$nexports",		ksyscallsym,		sysfn_nexports),
!	("$procnames",		ksyscallsym,		sysfn_procnames),
!	("$procaddrs",		ksyscallsym,		sysfn_procaddrs),
!	("$procexports",	ksyscallsym,		sysfn_procexports),

	("importdll",	kimportmodulesym,	0),
	("importlib",	kimportmodulesym,	0),
	("import",		kimportsym,			0),
	("importx",		kimportsym,			'X'),
	("importd",		kimportsym,			'D'),
	("importpath",	kimportpathsym,		0),
	("mapmodule",	kmapmodulesym,		0),
	("unless",		kunlesssym,			0),

	("try",			ktrysym,		0),
	("except",		kexceptsym,		0),
	("finally",		kfinallysym,	0),
	("raise",		kraisesym,		0),
	("out",			koutsym,		0),

	("new",			knewsym,		j_new),
	("destroy",		kdestroysym,	j_destroy),
!	("clear",		kclearsym,		j_clear),

	("global",		kglobalsym,		program_scope),
	("exportq",		kglobalsym,		exportq_scope),
	("export",		kglobalsym,		export_scope),

	("clang",		kfflangsym,		clangff),
	("mlang",		kfflangsym,		mlangff),
	("windows",		kfflangsym,		windowsff),
	("callback",	kfflangsym,		callbackff),

	("swap",		kswapsym,		0),

	("void",		stdtypesym,		tvoid),

	("int",			stdtypesym,		tint),

	("word",		stdtypesym,		tword),

	("real",		stdtypesym,		treal),

	("ichar",		kicharsym,		0),

	("int8",		stdtypesym,		ti8),
	("int16",		stdtypesym,		ti16),
	("int32",		stdtypesym,		ti32),
	("int64",		stdtypesym,		ti64),
	("int128",		stdtypesym,		ti128),

	("i8",			stdtypesym,		ti8),
	("i16",			stdtypesym,		ti16),
	("i32",			stdtypesym,		ti32),
	("i64",			stdtypesym,		ti64),
	("i128",		stdtypesym,		ti128),

	("real32",		stdtypesym,		tr32),
	("real64",		stdtypesym,		tr64),
	("r32",			stdtypesym,		tr32),
	("r64",			stdtypesym,		tr64),

	("float32",		stdtypesym,		tr32),
	("float64",		stdtypesym,		tr64),

	("byte",		stdtypesym,		tu8),
!	("u1",			stdtypesym,		tu1),
!	("u2",			stdtypesym,		tu2),
!	("u4",			stdtypesym,		tu4),
	("u8",			stdtypesym,		tu8),
	("u16",			stdtypesym,		tu16),
	("u32",			stdtypesym,		tu32),
	("u64",			stdtypesym,		tu64),
	("u128",		stdtypesym,		tu128),

	("word8",		stdtypesym,		tu8),
	("word16",		stdtypesym,		tu16),
	("word32",		stdtypesym,		tu32),
	("word64",		stdtypesym,		tu64),
	("word128",		stdtypesym,		tu128),

!	("bit",			stdtypesym,		tu1),
!	("bit2",		stdtypesym,		tu2),
!	("bit4",		stdtypesym,		tu4),

	("char",		stdtypesym,		tc8),
	("wchar",		stdtypesym,		tc16),
	("char64",		stdtypesym,		tc64),

!	("range",		stdtypesym,		trange),
	("auto",		stdtypesym,		tauto),
!	("label",		stdtypesym,		tlabel),

!	("flex",		stdtypesym,		tvar),

	("intm",		machinetypesym,	'I'),
	("intp",		machinetypesym,	'i'),
	("wordm",		machinetypesym,	'W'),
	("wordp",		machinetypesym,	'w'),
	("slice",		kslicesym,		tslice),
!	("slice2d",		kslicesym,		tslice2d),
!!	("flex",		kslicesym,		tflex),
	("typeof",		ktypeofsym,			0),

	("million",		unitnamesym,	million_unit),
	("billion",		unitnamesym,	billion_unit),
	("thousand",	unitnamesym,	thousand_unit),
	("kb",			unitnamesym,	kilo_unit),
	("mb",			unitnamesym,	mega_unit),
	("gb",			unitnamesym,	giga_unit),

	("$lineno",		compilervarsym,	j_cvlineno),
	("$strlineno",	compilervarsym,	j_cvstrlineno),
	("$filename",	compilervarsym,	j_cvfilename),
	("$modulename",	compilervarsym,	j_cvmodulename),
	("$function",	compilervarsym,	j_cvfunction),
	("$date",		compilervarsym,	j_cvdate),
	("$time",		compilervarsym,	j_cvtime),
	("$version",	compilervarsym,	j_cvversion),
	("$typename",	compilervarsym,	j_cvtypename),
	("$targetbits",	compilervarsym,	j_cvtargetbits),
	("$targetsize",	compilervarsym,	j_cvtargetsize),
!	("$targetname",	compilervarsym,	j_cvtargetname),
	("$targetcode",	compilervarsym,	j_cvtargetcode),
	("nil",			compilervarsym,	j_cvnil),
	("pi",			compilervarsym,	j_cvpi),
	("true",		compilervarsym,	j_cvtrue),
	("false",		compilervarsym,	j_cvfalse),
	("$",			dollarsym,		0),

	("and",			andlsym,		0),
	("or",			orlsym,			0),
	("xor",			xorlsym,		0),
	("iand",		iandsym,		0),
	("ior",			iorsym,			0),
	("ixor",		ixorsym,		0),
	("in",			insym,			kin),
	("notin",		notinsym,		knotin),
	("inrev",		inrevsym,		0),
	("rem",			iremsym,		0),
!	("divrem",		idivremsym,		0),
	("min",			minsym,			0),
	("max",			maxsym,			0),

	("not",			notlsym,		0),
	("inot",		inotsym,		0),
	("istrue",		istruelsym,		0),
	("abs",			abssym,			kabs),
	("$neg",		negsym,			0),

!	("asc",			opsym,			j_asc),
!	("tochr",		opsym,			j_chr),
	("sqr",			sqrsym,			0),
	("sqrt",		sqrtsym,		0),
	("sign",		signsym,		0),

	("sin",			mathsopsym,		ksin),
	("cos",			mathsopsym,		kcos),
	("tan",			mathsopsym,		ktan),
	("asin",		mathsopsym,		kasin),
	("acos",		mathsopsym,		kacos),
	("atan",		mathsopsym,		katan),
!	("sign",		mathsopsym,		ksign),
	("ln",			mathsopsym,		kln),
	("log",			mathsopsym,		klog),
!	("lg",			mathsopsym,		klg),
	("exp",			mathsopsym,		kexp),
	("round",		mathsopsym,		kround),
	("floor",		mathsopsym,		kfloor),
	("ceil",		mathsopsym,		kceil),
	("fract",		mathsopsym,		kfract),

	("atan2",		maths2opsym,	katan2),
	("fmod",		maths2opsym,	kfmod),

!	("append",		appendsym,		0),
!	("concat",		concatsym,		0),
!	("flexptr",		flexptrsym,		0),
!	("sliceptr",	propsym,		ksliceptr),
!	("stringz",		stringzsym,		0),

	("len",			propsym,	klen),
	("lwb",			propsym,	klwb),
	("upb",			propsym,	kupb),
	("bounds",		propsym,	kbounds),
	("lenstr",		propsym,	klenstr),
	("bitwidth",	propsym,	kbitwidth),
	("bytes",		propsym,	kbytesize),
	("minvalue",	propsym,	kminvalue),
	("maxvalue",	propsym,	kmaxvalue),
	("typestr",		propsym,	ktypestr),

!	("len",			propsym,	0),
!	("lwb",			propsym,	0),
!	("upb",			propsym,	0),
!	("bounds",		propsym,	0),
!	("lenstr",		propsym,	0),
!	("bitwidth",	propsym,	0),
!	("bytes",		propsym,	0),
!	("minvalue",	propsym,	0),
!	("maxvalue",	propsym,	0),
!	("typestr",		propsym,	ktypestr),

	("msb",			bitfieldsym,	bf_msb),
	("lsb",			bitfieldsym,	bf_lsb),
	("msbit",		bitfieldsym,	bf_msbit),
	("lsbit",		bitfieldsym,	bf_lsbit),
	("msw",			bitfieldsym,	bf_msw),
	("lsw",			bitfieldsym,	bf_lsw),
	("odd",			bitfieldsym,	bf_odd),
	("even",		bitfieldsym,	bf_even),

	("endif",		kendsym,	kifsym),
	("fi",			kendsym,	kifsym),
	("endcase",		kendsym,	kcasesym),
	("esac",		kendsym,	kcasesym),
	("enddocase",	kendsym,	kdocasesym),
	("endswitch",	kendsym,	kswitchsym),
	("enddoswitch",	kendsym,	kdoswitchsym),
	("endfor",		kendsym,	kforsym),
	("od",			kendsym,	kdosym),
	("endproc",		kendsym,	kprocsym),
	("endfunction",	kendsym,	kfunctionsym),
	("endwhile",	kendsym,	kwhilesym),
	("endto",		kendsym,	ktosym),
	("enddo",		kendsym,	kdosym),
	("endunless",	kendsym,	kunlesssym),
	("endimportmodule",	kendsym,kimportmodulesym),
	("endtry",		kendsym,	ktrysym),
	("endrecord",	kendsym,	krecordsym),
	("endassem",	kendsym,	kassemsym),

	("$caligned",	atsym,			1),
	("empty",		kemptysym,		0),
	("clear",		kemptysym,		0),

!	("nil",			knilsym,		0),
!	("con",			sysconstsym,	con_const),
!	("pi",			sysconstsym,	pi_const),

	("$$dummy",		0,				0)
end

global []int D_typestarterset= (stdtypesym,lsqsym,krefsym,kenumsym,krecordsym,
		kicharsym, ktypeofsym, kslicesym, kdictsym)

!list of genops that have an int result, used to populate intresult[]
[]byte intresultlist = (
	kin, knotin, klwb, kupb, klen, klenstr, kbitwidth,
	kbytesize, keq, kne, klt, kle, kge, kgt,
	kandl, korl, knotl, kistruel)

!!---
!Table of soft conversions between numeric types. In this form, easy to
!maintain that using a 2d table directly
global [,3]int16 softconvmappings =(
	(tc64,	tc64,	ksoftconv),
	(tc64,	tu64,	ksoftconv),
	(tc64,	tu128,	kwidenw),
	(tc64,	ti64,	ksoftconv),
	(tc64,	ti128,	kwidenw),
	(tc64,	tr32,	kfloat),
	(tc64,	tr64,	kfloat),

	(tu64,	tc64,	ksoftconv),
	(tu64,	tu64,	ksoftconv),
	(tu64,	tu128,	kwidenw),
	(tu64,	ti64,	ksoftconv),
	(tu64,	ti128,	kwidenw),
	(tu64,	tr32,	kfloat),
	(tu64,	tr64,	kfloat),

	(tu128,	tc64,	ksofttruncw),
	(tu128,	tu64,	ksofttruncw),
	(tu128,	tu128,	ksoftconv),
	(tu128,	ti64,	ksofttruncw),
	(tu128,	ti128,	ksoftconv),
	(tu128,	tr32,	kerror),
	(tu128,	tr64,	kerror),

	(ti64,	tc64,	ksoftconv),
	(ti64,	tu64,	ksoftconv),
	(ti64,	tu128,	kwidenw),
	(ti64,	ti64,	ksoftconv),
	(ti64,	ti128,	kwidenw),
	(ti64,	tr32,	kfloat),
	(ti64,	tr64,	kfloat),

	(ti128,	tc64,	ksofttruncw),
	(ti128,	tu64,	ksofttruncw),
	(ti128,	tu128,	ksoftconv),
	(ti128,	ti64,	ksofttruncw),
	(ti128,	ti128,	ksoftconv),
	(ti128,	tr32,	kerror),
	(ti128,	tr64,	kerror),

	(tr32,	tc64,	kfix),
	(tr32,	tu64,	kfix),
	(tr32,	tu128,	kerror),
	(tr32,	ti64,	kfix),
	(tr32,	ti128,	kerror),
	(tr32,	tr32,	ksoftconv),
	(tr32,	tr64,	kfwiden),

	(tr64,	tc64,	kfix),
	(tr64,	tu64,	kfix),
	(tr64,	tu128,	kerror),
	(tr64,	ti64,	kfix),
	(tr64,	ti128,	kerror),
	(tr64,	tr32,	kfnarrow),
	(tr64,	tr64,	ksoftconv))


!this 2d array maps [genop, basetype] to a spec-op
!It is filled in at runtime from genspecmappings

!global [firstgenop..lastgenop, tfirsttabletype..tlasttabletype]int16 optypetable

global [pclnames.lwb..pclnames.upb]byte intresult

global [tfirstnum..tlastnum, tfirstnum..tlastnum]int64 softconvtable

!global [specopnames.lwb..specopnames.upb]byte specoptogen
!global [specopnames.lwb..specopnames.upb]byte specoptotype

global proc inittypetables=
	int genop, s,t, a, specop

!populate the table with given combinations
!	for i in genspecmappings.bounds do
!		genop:=genspecmappings[i,1]
!		t:=genspecmappings[i,2]
!		specop:=genspecmappings[i,3]
!
!		optypetable[genop,t] := specop
!
!!build cross-reference tables
!		specoptogen[specop]:=genop
!		specoptotype[specop]:=t
!	od
!
!!now scan trying to fix some empty slots
!	for genop in firstgenop..lastgenop do
!		for t in tfirsttabletype..tlasttabletype do
!			if optypetable[genop,t]=0 then
!!CPL "OPTYPE0",STDNAMES[T];!STRMODE(T)
!				case t
!				when tc64 then
!					if a:=optypetable[genop,tu64] then optypetable[genop,tc64]:=a
!					elsif a:=optypetable[genop,ti64] then optypetable[genop,tc64]:=a
!					fi
!				when tu64 then
!					if a:=optypetable[genop,ti64] then optypetable[genop,tu64]:=a fi
!				when tu128 then
!					if a:=optypetable[genop,ti128] then optypetable[genop,tu128]:=a fi
!				esac
!			fi
!		od
!	od
!
!populate intresultlist
	for i in intresultlist.bounds do
		intresult[intresultlist[i]]:=1
	od

!do softconversions
	for i in softconvmappings.bounds do
		s:=softconvmappings[i,1]
		t:=softconvmappings[i,2]
!		specop:=softconvmappings[i,3]

		softconvtable[s,t]:=softconvmappings[i,3]
	od
end

