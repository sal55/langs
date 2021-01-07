import bb_pclcommon

!!---
global tabledata() [0:]ichar jtagnames, [0:]byte jisexpr =

    (j_none=0,      $,      0),
    (j_const,       $,      3),
    (j_null,        $,      3),
    (j_name,        $,      3),
    (j_namelv,      $,      3),
    (j_block,       $,      0),
    (j_stmtblock,   $,      0),
    (j_decimal,     $,      3),
    (j_assem,       $,      0),
    (j_assemmacro,  $,      0),
    (j_assemreg,    $,      0),
    (j_assemxreg,   $,      0),
    (j_assemmem,    $,      0),

!Logical Operators

    (j_andl,        $,      2),
    (j_orl,         $,      2),
    (j_xorl,        $,      2),
    (j_notl,        $,      1),
    (j_istruel,     $,      1),

!Expressions and Operators

    (j_makelist,    $,      3),
    (j_makerange,   $,      3),
    (j_makeset,     $,      3),
    (j_makedict,    $,      3),
    (j_makeslice,   $,      3),
    (j_exprlist,    $,      3),
    (j_multexpr,    $,      3),
    (j_returnmult,  $,      3),

    (j_keyword,     $,      3),
    (j_keyvalue,    $,      3),
    (j_assign,      $,      3),
    (j_deepcopy,    $,      3),
    (j_callfn,      $,      3),
!   (j_applyop,     $,      0),
!   (j_applyopx,    $,      1),
    (j_new,         $,      3),
    (j_destroy,     $,      0),
    (j_clear,       $,      0),

!   (j_setcc,       $,      2),
    (j_cmp,         $,      2),
    (j_cmpchain,    $,      1),
    (j_bin,         $,      2),
    (j_unary,       $,      1),
    (j_binto,       $,      2),
    (j_unaryto,     $,      1),
    (j_incr,        $,      3),

    (j_inrev,       $,      2),
    (j_inrange,     $,      2),
    (j_inset,       $,      2),
    (j_clamp,       $,      2),

    (j_flexptr,     $,      3),
    (j_stringz,     $,      3),
    (j_sliceptr,    $,      3),

    (j_index,       $,      3),
    (j_indexlv,     $,      3),
    (j_slice,       $,      3),
    (j_dot,         $,      3),
    (j_dotlv,       $,      3),
    (j_dotindex,    $,      3),
    (j_dotslice,    $,      3),
    (j_anddotslice, $,      3),
    (j_anddotindex, $,      3),

    (j_ptr,         $,      3),
    (j_ptrlv,       $,      3),
    (j_addrof,      $,      3),
    (j_addroffirst, $,      3),
    (j_convert,     $,      3),
    (j_shorten,     $,      3),
    (j_autocast,    $,      3),
    (j_typepun,     $,      3),
    (j_typeconst,   $,      3),
    (j_operator,    $,      3),
    (j_upper,       $,      3),

    (j_bitwidth,    $,      1),
    (j_bytesize,    $,      1),
    (j_typeof,      $,      3),
    (j_typestr,     $,      1),
!   (j_sliceptr,    $,      1),
    (j_bitfield,    $,      3),

    (j_minvalue,    $,      3),
    (j_maxvalue,    $,      3),

!Translator Variables

    (j_cvlineno,    $,      3),
    (j_cvstrlineno, $,      3),
    (j_cvmodulename,$,      3),
    (j_cvfilename,  $,      3),
    (j_cvfunction,  $,      3),
    (j_cvdate,      $,      3),
    (j_cvtime,      $,      3),
    (j_cvversion,   $,      3),
    (j_cvtypename,  $,      3),
    (j_cvtargetbits,$,      3),
    (j_cvtargetsize,$,      3),
    (j_cvtargetcode,$,      3),
    (j_cvnil,       $,      3),
    (j_cvpi,        $,      3),
    (j_cvtrue,      $,      3),
    (j_cvfalse,     $,      3),

    (j_whenthen,    $,      0),
    (j_elsif,       $,      0),
    (j_fmtitem,     $,      3),
    (j_nogap,       $,      3),

!Statements

    (j_callproc,    $,      0),
    (j_return,      $,      0),
    (j_syscall,     $,      0),

!   (j_assign,      $,      3),
    (j_to,          $,      0),
    (j_if,          $,      3),
    (j_longif,      $,      3),
    (j_forup,       $,      0),
    (j_fordown,     $,      0),
    (j_forall,      $,      0),
    (j_forallrev,   $,      0),
    (j_while,       $,      0),
    (j_repeat,      $,      0),
    (j_goto,        $,      0),
    (j_labeldef,    $,      0),
    (j_restart,     $,      0),
    (j_redo,        $,      0),
    (j_next,        $,      0),
    (j_exit,        $,      0),
    (j_do,          $,      0),
    (j_case,        $,      3),
    (j_docase,      $,      0),
    (j_switch,      $,      3),
    (j_doswitch,    $,      0),
    (j_swap,        $,      0),
    (j_select,      $,      3),
    (j_recase,      $,      0),
!   (j_recaseelse,  $,      0),

    (j_print,       $,      0),
    (j_println,     $,      0),
    (j_fprint,      $,      0),
    (j_fprintln,    $,      0),
    (j_cprint,      $,      0),
    (j_cprintln,    $,      0),
    (j_sprint,      $,      0),
    (j_sfprint,     $,      0),
    (j_read,        $,      0),
    (j_readln,      $,      0),
    (j_sread,       $,      0),
    (j_sreadln,     $,      0),
    (j_stop,        $,      0),
    (j_try,         $,      0),
    (j_except,      $,      0),
    (j_yield,       $,      0),
    (j_raise,       $,      0),
!   (j_callhostproc,$,      0),
    (j_eval,        $,      3),
    (j_emitc,       $,      1),
    (j_stack,       $,      0),
    (j_unstack,     $,      0),

    (j_dummy,       $,      3)
end

global tabledata() []ichar bitfieldnames=
    (bf_msb,        $),
    (bf_lsb,        $),
    (bf_msbit,      $),
    (bf_lsbit,      $),
    (bf_msw,        $),
    (bf_lsw,        $),
    (bf_odd,        $),
    (bf_even,       $),
end

global tabledata() [0:]ichar optypenames =
    (no_op=0,       $),
    (bin_op,        $),
    (mon_op,        $),
    (prop_op,       $),
end

!!---
global tabledata() []ichar symbolnames,
                    []byte symboloptypes,
                    []byte symbolgenops,
                    []byte symbolgentoops,
                    []byte symbolopprios,
                    []byte exprstarter =
!First half are basic tokens returned by lexreadtoken()
    (errorsym,          $,          0,  0,  0,  0,  0),     ! Lex error
    (dotsym,            ".",        0,  0,  0,  0,  0),     ! "."
    (lexdotsym,         $,          0,  0,  0,  0,  0),     ! ".", used at bol to prefix lexical 
    (anddotsym,         "&.",       0,  0,  0,  0,  1),     ! "&."
    (commasym,          ",",        0,  0,  0,  0,  0),     ! ","
    (semisym,           ";",        0,  0,  0,  0,  0),     ! ";"
    (colonsym,          ":",        0,  0,  0,  0,  0),     ! ":"
    (dcolonsym,         "::",       0,  0,  0,  0,  0),     ! "::"
    (assignsym,         ":=",       bin_op, assign_op,  0,  1,  0),     ! :=
    (deepcopysym,       "::=",      0,  0,  0,  1,  0),     ! ::=
    (sendtosym,         "=>",       0,  0,  0,  0,  0),     ! =>
    (lbracksym,         "(",        0,  0,  0,  0,  1),     ! (
    (rbracksym,         ")",        0,  0,  0,  0,  0),     ! )
    (lsqsym,            "[",        0,  0,  0,  0,  1),     ! [
    (rsqsym,            "]",        0,  0,  0,  0,  0),     ! ]
    (lcurlysym,         "{",        0,  0,  0,  0,  0),     ! {
    (rcurlysym,         "}",        0,  0,  0,  0,  0),     ! }
    (ptrsym,            "^",        0,  0,  0,  0,  0),     ! ^
    (barsym,            "|",        0,  0,  0,  0,  0),     ! |
    (dbarsym,           "||",       0,  0,  0,  0,  0),     ! ||
    (atsym,             "@",        0,  0,  0,  0,  0),     ! @
    (datsym,            "@@",       0,  0,  0,  0,  0),     ! @@
    (questionsym,       "?",        0,  0,  0,  0,  0),     ! ?
    (addrsym,           "&",        0,  0,  0,  0,  1),     ! &
    (daddrsym,          "&&",       0,  0,  0,  0,  0),     ! &&
    (curlsym,           "~",        0,  0,  0,  0,  0),     ! ~
    (rangesym,          "..",       bin_op, makerange_op,   0,  5,  0),     ! ..
    (ellipsissym,       "...",      0,  0,  0,  0,  0),     ! ...
    (hashsym,           "#",        0,  0,  0,  0,  0),     ! #

!   (opsym,             $,      0,  0,  0,  0,  0),     ! Any operator or property tag (use sets to distinguish)

    (addsym,            "+",        bin_op,     add_op,     addto_op,   4,  1),
    (subsym,            "-",        bin_op,     sub_op,     subto_op,   4,  1),
    (mulsym,            "*",        bin_op,     mul_op,     multo_op,   3,  0),
    (divsym,            "/",        bin_op,     div_op,     divto_op,   3,  0),
    (idivsym,           "%",        bin_op,     idiv_op,    idivto_op,  3,  0),
    (iremsym,           "rem",      bin_op,     irem_op,    iremto_op,  3,  0),
    (iandsym,           "iand",     bin_op,     iand_op,    iandto_op,  4,  0),
    (iorsym,            "ior",      bin_op,     ior_op,     iorto_op,   4,  0),
    (ixorsym,           "ixor",     bin_op,     ixor_op,    ixorto_op,  4,  0),
    (shlsym,            "<<",       bin_op,     shl_op,     shlto_op,   3,  0),
    (shrsym,            ">>",       bin_op,     shr_op,     shrto_op,   3,  0),
    (minsym,            "min",      bin_op,     min_op,     minto_op,   4,  1),
    (maxsym,            "max",      bin_op,     max_op,     maxto_op,   4,  1),
    (andlsym,           "and",      bin_op,     andl_op,    andlto_op,  7,  0),
    (orlsym,            "or",       bin_op,     orl_op,     orlto_op,   8,  0),
    (xorlsym,           "xor",      bin_op,     0,          0,          8,  0),

    (eqsym,             "=",        bin_op,     eq_op,      0,          6,  1),
    (cmpsym,            "cmp",      bin_op,     0,          0,          6,  1),
    (appendsym,         "append",   bin_op,     append_op,  appendto_op,4,  0),
    (concatsym,         "concat",   bin_op,     concat_op,  concatto_op,4,  0),
    (powersym,          "**",       bin_op,     power_op,   0,          2,  0),
    (samesym,           "==",       bin_op,     same_op,    0,          6,  0),
    (insym,             "in",       bin_op,     in_op,      0,          6,  0),
    (notinsym,          "notin",    bin_op,     notin_op,   0,          6,  0),
    (inrevsym,          "inrev",    0,          0,          0,          0,  0),

    (negsym,            "$neg",     mon_op,     neg_op,     0,          0,  1),
    (notlsym,           "not",      mon_op,     notl_op,    notlto_op,  0,  1),
    (istruelsym,        "istrue",   mon_op,     istruel_op, istruelto_op,   0,  1),
    (inotsym,           "inot",     mon_op,     inot_op,    inotto_op,  0,  1),
    (abssym,            "abs",      mon_op,     abs_op,     absto_op,   0,  1),
    (signsym,           "sign",     mon_op,     sign_op,    0,          0,  1),
    (sqrtsym,           "sqrt",     mon_op,     sqrt_op,    0,          0,  1),
    (sqrsym,            "sqr",      mon_op,     sqr_op,     0,          0,  1),

    (propsym,               $,          prop_op,    0,          0,          0,  0),
    (mathsopsym,        $,      0,  0,  0,  0,  1),     ! sin etc
    (maths2opsym,       $,      0,  0,  0,  0,  1),     ! atan2 etc

    (bitfieldsym,       $,      0,  0,  0,  0,  0),     ! Special bit selections
    (eolsym,            $,      0,  0,  0,  0,  0),     ! End of line
    (eofsym,            $,      0,  0,  0,  0,  0),     ! Eof seen
    (rawxnamesym,       $,      0,  0,  0,  0,  0),     ! unassigned name, case-sensitive, that is never a reserved word
    (docstringsym,      $,      0,  0,  0,  0,  0),     ! ! #comment used as documentation string
    (incrsym,           $,      0,  0,  0,  0,  1),     ! 1/2 = ++/--; later may add +2 for x++/x--
    (intconstsym,       $,      0,  0,  0,  0,  1),     ! 123 32 bits signed
    (decimalconstsym,   $,      0,  0,  0,  0,  1),     ! 123 or 123.4 decimal
    (realconstsym,      $,      0,  0,  0,  0,  1),     ! 123.4 64 bits
    (charconstsym,      $,      0,  0,  0,  0,  1),     ! 'A' or 'ABCD'
    (wcharconstsym,     $,      0,  0,  0,  0,  1),     ! 'A'W or 'ABCD'W (but don't have a syntax yet)
    (stringconstsym,    $,      0,  0,  0,  0,  1),     ! "ABC"
    (astringconstsym,   $,      0,  0,  0,  0,  1),     ! A"ABC"
    (wstringconstsym,   $,      0,  0,  0,  0,  1),     ! "ABC"W

!Second half are tokens that can be yielded after a name lookup::
    (unitnamesym,       $,      0,  0,  0,  0,  0),     ! 
    (namesym,           $,      0,  0,  0,  0,  1),     ! identifier symbol
    (ksourcedirsym,     $,      0,  0,  0,  0,  0),     ! 
!   (lexmacronamesym,   $,      0,  0,  0,  0,  0),     ! 
    (regsym,            $,      0,  0,  0,  0,  0),     ! x64 registers
    (xregsym,           $,      0,  0,  0,  0,  0),     ! XMM registers
    (fregsym,           $,      0,  0,  0,  0,  0),     ! ST registers
    (mregsym,           $,      0,  0,  0,  0,  0),     ! MMX registers
    (jmpccsym,          $,      0,  0,  0,  0,  0),     ! 
    (setccsym,          $,      0,  0,  0,  0,  0),     ! 
    (movccsym,          $,      0,  0,  0,  0,  0),     ! 
    (segnamesym,        $,      0,  0,  0,  0,  0),     ! 
    (asmopcodesym,      $,      0,  0,  0,  0,  0),     ! MOV etc

    (stdtypesym,        $,      0,  0,  0,  0,  1),     ! INT, CHAR etc
    (machinetypesym,    $,      0,  0,  0,  0,  1),     ! INTM etc
!   (packtypesym,       $,      0,  0,  0,  0,  0),     ! Byte etc
    (ktypeofsym,        $,      0,  0,  0,  0,  0),     ! TYPEOF
    (ksubrangesym,      $,      0,  0,  0,  0,  0),     ! SUBRANGE
    (koutsym,           $,      0,  0,  0,  0,  0),     ! OUT
    (kicharsym,         $,      0,  0,  0,  0,  1),     ! ICHAR
    (kifsym,            $,      0,  0,  0,  0,  1),     ! 
    (kthensym,          $,      0,  0,  0,  0,  0),     ! 
    (kelsifsym,         $,      0,  0,  0,  0,  0),     ! 
    (kelsesym,          $,      0,  0,  0,  0,  0),     ! 
    (kelsecasesym,      $,      0,  0,  0,  0,  0),     ! 
    (kelseswitchsym,    $,      0,  0,  0,  0,  0),     ! 
    (kelseselectsym,    $,      0,  0,  0,  0,  0),     ! 
    (kendsym,           $,      0,  0,  0,  0,  0),     ! 
    (kunlesssym,        $,      0,  0,  0,  0,  0),     ! 
    (kcasesym,          $,      0,  0,  0,  0,  1),     ! CASE
    (kdocasesym,        $,      0,  0,  0,  0,  0),     ! DOCASE
    (krecasesym,        $,      0,  0,  0,  0,  0),     ! RECASE
    (kwhensym,          $,      0,  0,  0,  0,  0),     ! 
    (kforsym,           $,      0,  0,  0,  0,  0),     ! FOR
    (ktosym,            $,      0,  0,  0,  0,  0),     ! TO/DOWNTO
    (kbysym,            $,      0,  0,  0,  0,  0),     ! 
    (kdosym,            $,      0,  0,  0,  0,  0),     ! 
    (kwhilesym,         $,      0,  0,  0,  0,  0),     ! 
    (krepeatsym,        $,      0,  0,  0,  0,  0),     ! 
    (kuntilsym,         $,      0,  0,  0,  0,  0),     ! 
    (kreturnsym,        $,      0,  0,  0,  0,  0),     ! 
    (kstopsym,          $,      0,  0,  0,  0,  0),     ! 
    (kloopsym,          $,      0,  0,  0,  0,  0),     ! EXIT/NEXT/LOOP/REDO/RESTART
    (kgotosym,          $,      0,  0,  0,  0,  0),     ! GO/GOTO
    (kswitchsym,        $,      0,  0,  0,  0,  0),     ! SWITCH
    (kdoswitchsym,      $,      0,  0,  0,  0,  0),     ! DOSWITCH
    (kprintsym,         $,      0,  0,  0,  0,  0),     ! PRINT/PRINTLN/FPRINT/FPRINTLN
    (ksprintsym,        $,      0,  0,  0,  0,  0),     ! SPRINT/SFPRINT
    (kreadsym,          $,      0,  0,  0,  0,  0),     ! READ/READLN
    (ksreadsym,         $,      0,  0,  0,  0,  0),     ! SREAD
    (ksreadlnsym,       $,      0,  0,  0,  0,  0),     ! SREADLN
    (kprocsym,          $,      0,  0,  0,  0,  0),     ! PROC
    (kfunctionsym,      $,      0,  0,  0,  0,  0),     ! FUNCTION
!   (kmethodsym,        $,      0,  0,  0,  0,  0),     ! METHOD
    (klabelsym,         $,      0,  0,  0,  0,  0),     ! LABEL
    (krecordsym,        $,      0,  0,  0,  0,  0),     ! RECORD
    (kstructsym,        $,      0,  0,  0,  0,  0),     ! STRUCT
    (kunionsym,         $,      0,  0,  0,  0,  0),     ! UNION
    (ktaggedunionsym,   $,      0,  0,  0,  0,  0),     ! TAGGEDUNION
    (kimportsym,        $,      0,  0,  0,  0,  0),     ! IMPORT
    (kimportmodulesym,  $,      0,  0,  0,  0,  0),     ! IMPORTDLL/IMPORTMODULE
    (kimportpathsym,    $,      0,  0,  0,  0,  0),     ! IMPORTPATH
    (kmapmodulesym,     $,      0,  0,  0,  0,  0),     ! MAPMODULE
    (ktypesym,          $,      0,  0,  0,  0,  0),     ! TYPE
    (ktypealiassym,     $,      0,  0,  0,  0,  0),     ! TYPEALIAS
    (kextendtypesym,    $,      0,  0,  0,  0,  0),     ! EXTENDTYPE
    (krefsym,           $,      0,  0,  0,  0,  1),     ! REF
    (kmutsym,           $,      0,  0,  0,  0,  0),     ! MUT
    (kletsym,           $,      0,  0,  0,  0,  0),     ! LET
    (kslicesym,         $,      0,  0,  0,  0,  0),     ! SLICE/SLICE2D
    (karraysym,         $,      0,  0,  0,  0,  0),     ! ARRAY
    (kdictsym,          $,      0,  0,  0,  0,  0),     ! DICT
!   (kflexsym,          $,      0,  0,  0,  0,  0),     ! FLEX
    (kmacrosym,         $,      0,  0,  0,  0,  0),     ! MACRO
    (kexpandsym,        $,      0,  0,  0,  0,  0),     ! EXPAND
    (koperatorsym,      $,      0,  0,  0,  0,  0),     ! OPERATOR
    (kconstsym,         $,      0,  0,  0,  0,  0),     ! 
    (kenumsym,          $,      0,  0,  0,  0,  0),     ! 
    (knewsym,           $,      0,  0,  0,  0,  0),     ! NEW
    (kdestroysym,       $,      0,  0,  0,  0,  0),     ! DESTROY
    (kclearsym,         $,      0,  0,  0,  0,  0),     ! CLEAR
    (kclasssym,         $,      0,  0,  0,  0,  0),     ! CLASS
!   (kdirectivesym,     $,      0,  0,  0,  0,  0),     ! TARGET/MODULE
    (kfflangsym,        $,      0,  0,  0,  0,  0),     ! JLANG CLANG WINDOWS HOST
    (kglobalsym,        $,      0,  0,  0,  0,  0),     ! global
    (kstaticsym,        $,      0,  0,  0,  0,  0),     ! STATIC

    (ktrysym,           $,      0,  0,  0,  0,  0),     ! 
    (kexceptsym,        $,      0,  0,  0,  0,  0),     ! 
    (kfinallysym,       $,      0,  0,  0,  0,  0),     ! 
    (kraisesym,         $,      0,  0,  0,  0,  0),     ! 
    (kyieldsym,         $,      0,  0,  0,  0,  0),     ! 
    (kcastsym,          $,      0,  0,  0,  0,  1),     ! CAST
    (ktypeconstsym,     $,      0,  0,  0,  0,  0),     ! TYPECONST
    (compilervarsym,    $,      0,  0,  0,  0,  1),     ! $lineno etc
    (dollarsym,         $,      0,  0,  0,  0,  1),     ! to be used for current array upperbound; also tabledata names
    (kevalsym,          $,      0,  0,  0,  0,  0),     ! EVAL
    (ktabledatasym,     $,      0,  0,  0,  0,  0),     ! tabledata
    (kstacksym,         $,      0,  0,  0,  0,  0),     ! STACK/UNSTACK
    (kclampsym,         $,      0,  0,  0,  0,  1),         ! CLAMP
    (kswapsym,          $,      0,  0,  0,  0,  0),     ! SWAP
    (kerrorsym,         $,      0,  0,  0,  0,  0),     ! PC_ERROR etc
!   (sysconstsym,       $,      0,  0,  0,  0,  0),     ! nil, etc
    (kassemsym,         $,      0,  0,  0,  0,  0),     ! ASM/ASSEM
    (ksyscallsym,       $,      0,  0,  0,  0,  1),     ! $get_procname etc
    (kemitcsym,         $,      0,  0,  0,  0,  0),     ! EMITC

    (kdummysym,         $,      0,  0,  0,  0,  0),     !
end

global tabledata() []ichar sourcedirnames =
    (includedir,    $),
    (strincludedir, $),
    (binincludedir, $),
    (textincludedir,$),
    (defineunitdir, $),
    (emitcdir,      $),
    (cclibdir,      $),
end

global tabledata() [0:]ichar fflangnames=
    (noff=0,        $),
    (windowsff,     $),
    (clangff,       $),
    (mlangff,       $),
    (callbackff,    $),
end

global tabledata() [0:]ichar scopenames=
    (local_scope=0,     $),
    (program_scope,     $),
    (export_scope,      $),
    (exportq_scope,     $),
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
    (var_param=0,       "Var "),
    (in_param,          "In "),
    (out_param,         "Out "),
    (optional_param,    "Opt "),
end

global tabledata() [0:]ichar namecatnames, [0:]byte qualifiedname =
    (normal_cat=0,      "-",        0),
    (proc_cat,          "proc",     1),
    (globalproc_cat,    "gproc",    1),
    (dllproc_cat,       "dllproc",  0),
    (dllmodule_cat,     "dllmodule",0),
    (dllvar_cat,        "dllvar",   0),
    (static_cat,        "static",   1),
    (frame_cat,         "frame",    1),
end

global tabledata() [0:]ichar namenames, [0:]byte pclidtable, [0:]byte defaultnamecat =
    (nullid=0,      $,  0,              0),             !Not assigned (sometimes converted to genfieldid)
    (programid,     $,  0,              0),             !Main root
    (moduleid,      $,  module_name,    0),             !Current or imported module
    (dllmoduleid,   $,  0,              dllmodule_cat),     !
    (typeid,        $,  0,              0),             !Type name in type, proc or module
    (procid,        $,  proc_name,      proc_cat),      !Proc/method/function/op name
    (dllprocid,     $,  dllproc_name,   dllproc_cat),   !Dll Proc/function name
    (dllvarid,      $,  0,              dllvar_cat),    !Dll variable name
    (genprocid,     $,  0,              proc_cat),      !generic proc name
    (generatorid,   $,  0,              proc_cat),      !generator proc name
    (constid,       $,  0,              0),             !Named constant in type, proc or module
    (staticid,      $,  zstatic_name,   static_cat),    !Static in type or proc or module
    (frameid,       $,  frame_name,     frame_cat),     !Local var
    (paramid,       $,  param_name,     frame_cat),     !Local param
    (fieldid,       $,  0,              0),             !Field of Record or Class
    (genfieldid,    $,  0,              0),             !Generic Field of Record or Class
    (enumid,        $,  0,              0),             !Enum name, part of enum type only
    (labelid,       $,  label_name,     0),             !Label name in proc only
    (blockid,       $,  0,              0),             !Codeblock label name in proc only
    (aliasid,       $,  0,              0),             !Alias to another name
    (macroid,       $,  0,              0),             !Name of macro
    (macroparamid,  $,  0,              0),             !Macro formal parameter name
    (linkid,        $,  0,              0),             !Name in class defined in a base class
    (functionopid,  $,  0,              0),             !Function-operator
end

!!---
global tabledata []ichar stnames, []int stsymbols, []int stsubcodes=

    ("if",          kifsym,         j_if),
    ("then",        kthensym,       0),
    ("elsif",       kelsifsym,      j_if),
    ("else",        kelsesym,       0),
    ("elsecase",    kelsecasesym,   j_case),
    ("elseswitch",  kelseswitchsym, j_switch),
    ("case",        kcasesym,       j_case),
    ("docase",      kdocasesym,     j_docase),
    ("recase",      krecasesym,     j_recase),
    ("when",        kwhensym,       0),
    ("for",         kforsym,        0),
    ("forall",      kforsym,        0),
    ("to",          ktosym,         0),
    ("downto",      ktosym,         1),
    ("by",          kbysym,         0),
    ("do",          kdosym,         0),
    ("end",         kendsym,        0),
    ("while",       kwhilesym,      0),
    ("repeat",      krepeatsym,     0),
    ("until",       kuntilsym,      0),
    ("always",      kuntilsym,      1),
    ("return",      kreturnsym,     0),
    ("yield",       kyieldsym,      0),
    ("stop",        kstopsym,       0),
    ("restart",     kloopsym,       j_restart),
    ("redo",        kloopsym,       j_redo),
    ("loop",        kloopsym,       j_redo),
    ("next",        kloopsym,       j_next),
    ("exit",        kloopsym,       j_exit),
    ("goto",        kgotosym,       0),
    ("go",          kgotosym,       1),
    ("switch",      kswitchsym,     j_switch),
    ("doswitch",    kdoswitchsym,   j_doswitch),
    ("tabledata",   ktabledatasym,  0),
    ("clamp",       kclampsym,      0),
    ("eval",        kevalsym,       0),
    ("extendtype",  kextendtypesym, 0),

    ("print",       kprintsym,      j_print),
    ("println",     kprintsym,      j_println),
    ("fprint",      kprintsym,      j_fprint),
    ("fprintln",    kprintsym,      j_fprintln),
    ("cprint",      kprintsym,      j_cprint),
    ("cprintln",    kprintsym,      j_cprintln),
    ("sprint",      ksprintsym,     j_sprint),
    ("sfprint",     ksprintsym,     j_sfprint),

    ("stack",       kstacksym,      j_stack),
    ("unstack",     kstacksym,      j_unstack),

    ("cp",          kprintsym,      j_print),
    ("cpl",         kprintsym,      j_println),

    ("read",        kreadsym,       j_read),
    ("readln",      kreadsym,       j_readln),
    ("cast",        kcastsym,       j_convert),

    ("proc",        kprocsym,       0),
    ("function",    kfunctionsym,   0),
    ("threadedproc",        kprocsym,       1),
    ("threadedfunction",    kfunctionsym,   1),

    ("type",        ktypesym,       0),
    ("class",       kclasssym,      0),
    ("record",      krecordsym,     0),
    ("struct",      kstructsym,     0),
    ("union",       kunionsym,      0),
    ("taggedunion", ktaggedunionsym,0),
    ("ref",         krefsym,        0),
    ("var",         kmutsym,        0),
    ("mut",         kmutsym,        0),
    ("let",         kletsym,        0),

    ("include",     ksourcedirsym,  includedir),
    ("strinclude",  ksourcedirsym,  strincludedir),
    ("bininclude",  ksourcedirsym,  binincludedir),
    ("textinclude", ksourcedirsym,  textincludedir),
    ("defineunit",  ksourcedirsym,  defineunitdir),
    ("macro",       kmacrosym,      0),
!   ("expand",      kexpandsym,     0),
    ("operator",    koperatorsym,   0),
    ("emitc",       ksourcedirsym,  emitcdir),
    ("cclib",       ksourcedirsym,  cclibdir),

    ("assem",       kassemsym,      1),
    ("asm",         kassemsym,      0),

    ("static",      kstaticsym,     0),
    
    ("const",       kconstsym,      0),
!   ("table",       kconstsym,      1),
    ("enum",        kenumsym,       0),

    ("$get_nprocs",     ksyscallsym,        sysfn_get_nprocs),
    ("$get_procname",   ksyscallsym,        sysfn_get_procname),
    ("$get_procaddr",   ksyscallsym,        sysfn_get_procaddr),

    ("$get_nexports",   ksyscallsym,        sysfn_get_nexports),
    ("$get_procexport", ksyscallsym,        sysfn_get_procexport),

    ("importdll",   kimportmodulesym,   0),
    ("importlib",   kimportmodulesym,   0),
    ("import",      kimportsym,         0),
    ("importx",     kimportsym,         'X'),
    ("importd",     kimportsym,         'D'),
    ("importpath",  kimportpathsym,     0),
    ("mapmodule",   kmapmodulesym,      0),
    ("unless",      kunlesssym,         0),

    ("try",         ktrysym,        0),
    ("except",      kexceptsym,     0),
    ("finally",     kfinallysym,    0),
    ("raise",       kraisesym,      0),
    ("out",         koutsym,        0),

    ("new",         knewsym,        j_new),
    ("destroy",     kdestroysym,    j_destroy),
    ("clear",       kclearsym,      j_clear),

    ("global",      kglobalsym,     program_scope),
    ("exportq",     kglobalsym,     exportq_scope),
    ("export",      kglobalsym,     export_scope),

    ("clang",       kfflangsym,     clangff),
    ("mlang",       kfflangsym,     mlangff),
    ("windows",     kfflangsym,     windowsff),
    ("callback",    kfflangsym,     callbackff),

    ("swap",        kswapsym,       0),

    ("void",        stdtypesym,     tvoid),

    ("int",         stdtypesym,     tint),

    ("word",        stdtypesym,     tword),

    ("real",        stdtypesym,     treal),

    ("ichar",       kicharsym,      0),

    ("int8",        stdtypesym,     ti8),
    ("int16",       stdtypesym,     ti16),
    ("int32",       stdtypesym,     ti32),
    ("int64",       stdtypesym,     ti64),
    ("int128",      stdtypesym,     ti128),

    ("i8",          stdtypesym,     ti8),
    ("i16",         stdtypesym,     ti16),
    ("i32",         stdtypesym,     ti32),
    ("i64",         stdtypesym,     ti64),
    ("i128",        stdtypesym,     ti128),

    ("real32",      stdtypesym,     tr32),
    ("real64",      stdtypesym,     tr64),
    ("r32",         stdtypesym,     tr32),
    ("r64",         stdtypesym,     tr64),

    ("float32",     stdtypesym,     tr32),
    ("float64",     stdtypesym,     tr64),

    ("byte",        stdtypesym,     tu8),
    ("u1",          stdtypesym,     tu1),
    ("u2",          stdtypesym,     tu2),
    ("u4",          stdtypesym,     tu4),
    ("u8",          stdtypesym,     tu8),
    ("u16",         stdtypesym,     tu16),
    ("u32",         stdtypesym,     tu32),
    ("u64",         stdtypesym,     tu64),
    ("u128",        stdtypesym,     tu128),

    ("word8",       stdtypesym,     tu8),
    ("word16",      stdtypesym,     tu16),
    ("word32",      stdtypesym,     tu32),
    ("word64",      stdtypesym,     tu64),
    ("word128",     stdtypesym,     tu128),

    ("bit",         stdtypesym,     tu1),
    ("bit2",        stdtypesym,     tu2),
    ("bit4",        stdtypesym,     tu4),

    ("char",        stdtypesym,     tc8),
    ("wchar",       stdtypesym,     tc16),
    ("char64",      stdtypesym,     tc64),

    ("array",       karraysym,      tlist),

    ("string",      stdtypesym,     tstring),
    ("set",         stdtypesym,     tset),
    ("dict",        kdictsym,       0),
    ("decimal",     stdtypesym,     tdecimal),
!   ("variant",     stdtypesym,     tvar),
    ("generator",   stdtypesym,     tgen),

    ("$t",          stdtypesym,     tparam1),
    ("$u",          stdtypesym,     tparam2),
    ("$v",          stdtypesym,     tparam3),
    ("$w",          stdtypesym,     tparam4),

    ("range",       stdtypesym,     trange),
    ("auto",        stdtypesym,     tauto),
!   ("label",       stdtypesym,     tlabel),

!   ("flex",        stdtypesym,     tvar),

    ("intm",        machinetypesym, 'I'),
    ("intp",        machinetypesym, 'i'),
    ("wordm",       machinetypesym, 'W'),
    ("wordp",       machinetypesym, 'w'),
    ("slice",       kslicesym,      tslice),
!   ("slice2d",     kslicesym,      tslice2d),
!!  ("flex",        kslicesym,      tflex),
    ("typeof",      ktypeofsym,         0),

    ("million",     unitnamesym,    million_unit),
    ("billion",     unitnamesym,    billion_unit),
    ("thousand",    unitnamesym,    thousand_unit),
    ("kb",          unitnamesym,    kilo_unit),
    ("mb",          unitnamesym,    mega_unit),
    ("gb",          unitnamesym,    giga_unit),

    ("$lineno",     compilervarsym, j_cvlineno),
    ("$strlineno",  compilervarsym, j_cvstrlineno),
    ("$filename",   compilervarsym, j_cvfilename),
    ("$modulename", compilervarsym, j_cvmodulename),
    ("$function",   compilervarsym, j_cvfunction),
    ("$date",       compilervarsym, j_cvdate),
    ("$time",       compilervarsym, j_cvtime),
    ("$version",    compilervarsym, j_cvversion),
    ("$typename",   compilervarsym, j_cvtypename),
    ("$targetbits", compilervarsym, j_cvtargetbits),
    ("$targetsize", compilervarsym, j_cvtargetsize),
!   ("$targetname", compilervarsym, j_cvtargetname),
    ("$targetcode", compilervarsym, j_cvtargetcode),
    ("nil",         compilervarsym, j_cvnil),
    ("pi",          compilervarsym, j_cvpi),
    ("true",        compilervarsym, j_cvtrue),
    ("false",       compilervarsym, j_cvfalse),
    ("$",           dollarsym,      0),

    ("and",         andlsym,        0),
    ("or",          orlsym,         0),
    ("xor",         xorlsym,        0),
    ("iand",        iandsym,        0),
    ("ior",         iorsym,         0),
    ("ixor",        ixorsym,        0),
    ("in",          insym,          in_op),
    ("notin",       notinsym,       notin_op),
    ("inrev",       inrevsym,       0),
    ("rem",         iremsym,        0),
!   ("divrem",      idivremsym,     0),
    ("min",         minsym,         0),
    ("max",         maxsym,         0),

    ("not",         notlsym,        0),
    ("inot",        inotsym,        0),
    ("istrue",      istruelsym,     0),
    ("abs",         abssym,         abs_op),
    ("$neg",        negsym,         0),

!   ("asc",         opsym,          j_asc),
!   ("tochr",       opsym,          j_chr),
    ("sqr",         sqrsym,         0),
    ("sqrt",        sqrtsym,        0),
    ("sign",        signsym,        0),

    ("sin",         mathsopsym,     sin_op),
    ("cos",         mathsopsym,     cos_op),
    ("tan",         mathsopsym,     tan_op),
    ("asin",        mathsopsym,     asin_op),
    ("acos",        mathsopsym,     acos_op),
    ("atan",        mathsopsym,     atan_op),
!   ("sign",        mathsopsym,     sign_op),
    ("ln",          mathsopsym,     ln_op),
    ("log",         mathsopsym,     log_op),
    ("lg",          mathsopsym,     lg_op),
    ("exp",         mathsopsym,     exp_op),
    ("round",       mathsopsym,     round_op),
    ("floor",       mathsopsym,     floor_op),
    ("ceil",        mathsopsym,     ceil_op),
    ("fract",       mathsopsym,     fract_op),

    ("atan2",       maths2opsym,    atan2_op),
    ("fmod",        maths2opsym,    fmod_op),

    ("append",      appendsym,      0),
    ("concat",      concatsym,      0),
!   ("flexptr",     flexptrsym,     0),
    ("sliceptr",    propsym,        sliceptr_op),
!   ("stringz",     stringzsym,     0),

    ("len",         propsym,    len_op),
    ("lwb",         propsym,    lwb_op),
    ("upb",         propsym,    upb_op),
    ("bounds",      propsym,    bounds_op),
    ("lenstr",      propsym,    lenstr_op),
    ("bitwidth",    propsym,    bitwidth_op),
    ("bytes",       propsym,    bytesize_op),
    ("minvalue",    propsym,    minvalue_op),
    ("maxvalue",    propsym,    maxvalue_op),
    ("typestr",     propsym,    typestr_op),

    ("msb",         bitfieldsym,    bf_msb),
    ("lsb",         bitfieldsym,    bf_lsb),
    ("msbit",       bitfieldsym,    bf_msbit),
    ("lsbit",       bitfieldsym,    bf_lsbit),
    ("msw",         bitfieldsym,    bf_msw),
    ("lsw",         bitfieldsym,    bf_lsw),
    ("odd",         bitfieldsym,    bf_odd),
    ("even",        bitfieldsym,    bf_even),

    ("endif",       kendsym,    kifsym),
    ("fi",          kendsym,    kifsym),
    ("endcase",     kendsym,    kcasesym),
    ("esac",        kendsym,    kcasesym),
    ("enddocase",   kendsym,    kdocasesym),
    ("endswitch",   kendsym,    kswitchsym),
    ("enddoswitch", kendsym,    kdoswitchsym),
    ("endfor",      kendsym,    kforsym),
    ("od",          kendsym,    kdosym),
    ("endproc",     kendsym,    kprocsym),
    ("endfunction", kendsym,    kfunctionsym),
    ("endwhile",    kendsym,    kwhilesym),
    ("endto",       kendsym,    ktosym),
    ("enddo",       kendsym,    kdosym),
    ("endunless",   kendsym,    kunlesssym),
    ("endimportmodule", kendsym,kimportmodulesym),
    ("endtry",      kendsym,    ktrysym),
    ("endrecord",   kendsym,    krecordsym),
    ("endassem",    kendsym,    kassemsym),

    ("$caligned",   atsym,      1),

!   ("nil",         knilsym,        0),
!   ("con",         sysconstsym,    con_const),
!   ("pi",          sysconstsym,    pi_const),

    ("$$dummy",     0,              0)
end

global []int D_typestarterset= (stdtypesym,lsqsym,krefsym,kenumsym,krecordsym,
        kicharsym, ktypeofsym, kslicesym, kdictsym)

!!---
!Table used to populate optypetable
global [,3]int16 genspecmappings = (
    (add_op,        ti64,      op_add_i64),
    (add_op,        ti128,     op_add_i128),
    (add_op,        tr32,      op_add_r32),
    (add_op,        tr64,      op_add_r64),
    (add_op,        tdec,      op_add_dec),
    (add_op,        tvar,      op_add_var),

    (sub_op,        ti64,      op_sub_i64),
    (sub_op,        ti128,     op_sub_i128),
    (sub_op,        tr32,      op_sub_r32),
    (sub_op,        tr64,      op_sub_r64),
    (sub_op,        tdec,      op_sub_dec),
    (sub_op,        tref,      op_sub_ref),
    (sub_op,        tvar,      op_sub_var),

    (mul_op,        ti64,      op_mul_i64),
    (mul_op,        ti128,     op_mul_i128),
    (mul_op,        tr32,      op_mul_r32),
    (mul_op,        tr64,      op_mul_r64),
    (mul_op,        tdec,      op_mul_dec),
    (mul_op,        tvar,      op_mul_var),

    (div_op,        tr32,      op_div_r32),
    (div_op,        tr64,      op_div_r64),
    (div_op,        tdec,      op_div_dec),

    (idiv_op,       tu64,      op_idiv_u64),
    (idiv_op,       ti64,      op_idiv_i64),
    (idiv_op,       tu128,     op_idiv_u128),
    (idiv_op,       ti128,     op_idiv_i128),
    (idiv_op,       tdec,      op_idiv_dec),

    (irem_op,       tu64,      op_irem_u64),
    (irem_op,       ti64,      op_irem_i64),
    (irem_op,       tu128,     op_irem_u128),
    (irem_op,       ti128,     op_irem_i128),
    (irem_op,       tdec,      op_irem_dec),

    (iand_op,       ti64,      op_iand_i64),
    (iand_op,       ti128,     op_iand_i128),
    (iand_op,       tvar,      op_iand_var),

    (ior_op,        ti64,      op_ior_i64),
    (ior_op,        ti128,     op_ior_i128),
    (ior_op,        tvar,      op_ior_var),

    (ixor_op,       ti64,      op_ixor_i64),
    (ixor_op,       ti128,     op_ixor_i128),
    (ixor_op,       tvar,      op_ixor_var),

    (shl_op,        tu64,      op_shl_i64),
    (shl_op,        ti64,      op_shl_i64),
    (shl_op,        tu128,     op_shl_i128),
    (shl_op,        ti128,     op_shl_i128),
    (shl_op,        tdec,      op_shl_dec),

    (shr_op,        tu64,      op_shr_u64),
    (shr_op,        ti64,      op_shr_i64),
    (shr_op,        tu128,     op_shr_u128),
    (shr_op,        ti128,     op_shr_i128),
    (shr_op,        tdec,      op_shr_dec),

    (min_op,        tu64,      op_min_u64),
    (min_op,        ti64,      op_min_i64),
    (min_op,        tu128,     op_min_u128),
    (min_op,        ti128,     op_min_i128),
    (min_op,        tr32,      op_min_r32),
    (min_op,        tr64,      op_min_r64),
    (min_op,        tdec,      op_min_dec),

    (max_op,        tu64,      op_max_u64),
    (max_op,        ti64,      op_max_i64),
    (max_op,        tu128,     op_max_u128),
    (max_op,        ti128,     op_max_i128),
    (max_op,        tr32,      op_max_r32),
    (max_op,        tr64,      op_max_r64),
    (max_op,        tdec,      op_max_dec),

    (concat_op,     tvar,      op_concat_var),

    (append_op,     tvar,      op_append_var),

    (neg_op,        ti64,      op_neg_i64),
    (neg_op,        ti128,     op_neg_i128),
    (neg_op,        tr32,      op_neg_r32),
    (neg_op,        tr64,      op_neg_r64),
    (neg_op,        tdec,      op_neg_dec),

    (abs_op,        ti64,      op_abs_i64),
    (abs_op,        ti128,     op_abs_i128),
    (abs_op,        tr32,      op_abs_r32),
    (abs_op,        tr64,      op_abs_r64),
    (abs_op,        tdec,      op_abs_dec),

    (inot_op,       ti64,      op_inot_i64),
    (inot_op,       ti128,     op_inot_i128),
    (inot_op,       tvar,      op_inot_var),

    (sqr_op,        ti64,      op_sqr_i64),
    (sqr_op,        ti128,     op_sqr_i128),
    (sqr_op,        tr32,      op_sqr_r32),
    (sqr_op,        tr64,      op_sqr_r64),
    (sqr_op,        tdec,      op_sqr_dec),

!   (sqrt_op,       ti64,      op_sqrt_i64),
!   (sqrt_op,       ti128,     op_sqrt_i128),
    (sqrt_op,       tr32,      op_sqrt_r32),
    (sqrt_op,       tr64,      op_sqrt_r64),
    (sqrt_op,       tdec,      op_sqrt_dec),

    (sin_op,        tr32,      op_sin_r32),
    (sin_op,        tr64,      op_sin_r64),
    (sin_op,        tdec,      op_sin_dec),

    (cos_op,        tr32,      op_cos_r32),
    (cos_op,        tr64,      op_cos_r64),
    (cos_op,        tdec,      op_cos_dec),

    (tan_op,        tr32,      op_tan_r32),
    (tan_op,        tr64,      op_tan_r64),
    (tan_op,        tdec,      op_tan_dec),

    (asin_op,       tr32,      op_asin_r32),
    (asin_op,       tr64,      op_asin_r64),
    (asin_op,       tdec,      op_asin_dec),

    (acos_op,       tr32,      op_acos_r32),
    (acos_op,       tr64,      op_acos_r64),
    (acos_op,       tdec,      op_acos_dec),

    (atan_op,       tr32,      op_atan_r32),
    (atan_op,       tr64,      op_atan_r64),
    (atan_op,       tdec,      op_atan_dec),

    (ln_op,         tr32,      op_ln_r32),
    (ln_op,         tr64,      op_ln_r64),
    (ln_op,         tdec,      op_ln_dec),

    (lg_op,         tr32,      op_lg_r32),
    (lg_op,         tr64,      op_lg_r64),
    (lg_op,         tdec,      op_lg_dec),

    (log_op,        tr32,      op_log_r32),
    (log_op,        tr64,      op_log_r64),
    (log_op,        tdec,      op_log_dec),

    (exp_op,        tr32,      op_exp_r32),
    (exp_op,        tr64,      op_exp_r64),
    (exp_op,        tdec,      op_exp_dec),

    (round_op,      tr32,      op_round_r32),
    (round_op,      tr64,      op_round_r64),
    (round_op,      tdec,      op_round_dec),

    (floor_op,      tr32,      op_floor_r32),
    (floor_op,      tr64,      op_floor_r64),
    (floor_op,      tdec,      op_floor_dec),

    (ceil_op,       tr32,      op_ceil_r32),
    (ceil_op,       tr64,      op_ceil_r64),
    (ceil_op,       tdec,      op_ceil_dec),

    (fract_op,      tr32,      op_fract_r32),
    (fract_op,      tr64,      op_fract_r64),
    (fract_op,      tdec,      op_fract_dec),

    (sign_op,       ti64,      op_sign_i64),
    (sign_op,       tr32,      op_sign_r32),
    (sign_op,       tr64,      op_sign_r64),
    (sign_op,       tdec,      op_sign_dec),

    (atan2_op,      tr32,      op_atan2_r32),
    (atan2_op,      tr64,      op_atan2_r64),
    (atan2_op,      tdec,      op_atan2_dec),

    (power_op,      tu64,      op_power_u64),
    (power_op,      ti64,      op_power_i64),
    (power_op,      tu128,     op_power_u128),
    (power_op,      ti128,     op_power_i128),
    (power_op,      tr32,      op_power_r32),
    (power_op,      tr64,      op_power_r64),
    (power_op,      tdec,      op_power_dec),

    (fmod_op,       tr32,      op_fmod_r32),
    (fmod_op,       tr64,      op_fmod_r64),
    (fmod_op,       tdec,      op_fmod_dec),

    (lwb_op,        tvar,      op_lwb_var),

    (upb_op,        tvar,      op_upb_var),

    (len_op,        tvar,      op_len_var),

    (bounds_op,     tvar,      op_bounds_var),

    (lenstr_op,     tvar,      op_lenstr_var),

    (sliceptr_op,   tslice,    op_sliceptr_slice),

    (incr_op,       tu8,       op_incr_short),
    (incr_op,       ti64,      op_incr_i64),
    (incr_op,       ti128,     op_incr_i128),
    (incr_op,       tdec,      op_incr_dec),
    (incr_op,       tref,      op_incr_ref),
    (incr_op,       tenum,     op_incr_enum),

    (decr_op,       tu8,       op_decr_short),
    (decr_op,       ti64,      op_decr_i64),
    (decr_op,       ti128,     op_decr_i128),
    (decr_op,       tdec,      op_decr_dec),
    (decr_op,       tref,      op_decr_ref),
    (decr_op,       tenum,     op_decr_enum),

    (incrload_op,   tu8,       op_incrload_short),
    (incrload_op,   ti64,      op_incrload_i64),
    (incrload_op,   ti128,     op_incrload_i128),
    (incrload_op,   tdec,      op_incrload_dec),
    (incrload_op,   tref,      op_incrload_ref),
    (incrload_op,   tenum,     op_incrload_enum),

    (decrload_op,   tu8,       op_decrload_short),
    (decrload_op,   ti64,      op_decrload_i64),
    (decrload_op,   ti128,     op_decrload_i128),
    (decrload_op,   tdec,      op_decrload_dec),
    (decrload_op,   tref,      op_decrload_ref),
    (decrload_op,   tenum,     op_decrload_enum),

    (loadincr_op,   tu8,       op_loadincr_short),
    (loadincr_op,   ti64,      op_loadincr_i64),
    (loadincr_op,   ti128,     op_loadincr_i128),
    (loadincr_op,   tdec,      op_loadincr_dec),
    (loadincr_op,   tref,      op_loadincr_ref),
    (loadincr_op,   tenum,     op_loadincr_enum),

    (loaddecr_op,   tu8,       op_loaddecr_short),
    (loaddecr_op,   ti64,      op_loaddecr_i64),
    (loaddecr_op,   ti128,     op_loaddecr_i128),
    (loaddecr_op,   tr32,      op_loaddecr_r32),
    (loaddecr_op,   tr64,      op_loaddecr_r64),
    (loaddecr_op,   tdec,      op_loaddecr_dec),
    (loaddecr_op,   tref,      op_loaddecr_ref),
    (loaddecr_op,   tenum,     op_loaddecr_enum),

    (addto_op,      tu8,       op_addto_short),
    (addto_op,      ti64,      op_addto_i64),
    (addto_op,      ti128,     op_addto_i128),
    (addto_op,      tr32,      op_addto_r32),
    (addto_op,      tr64,      op_addto_r64),
    (addto_op,      tdec,      op_addto_dec),
    (addto_op,      tvar,      op_addto_var),

    (subto_op,      tu8,       op_subto_short),
    (subto_op,      ti64,      op_subto_i64),
    (subto_op,      ti128,     op_subto_i128),
    (subto_op,      tr32,      op_subto_r32),
    (subto_op,      tr64,      op_subto_r64),
    (subto_op,      tdec,      op_subto_dec),

    (multo_op,      tu8,       op_multo_short),
    (multo_op,      ti64,      op_multo_i64),
    (multo_op,      ti128,     op_multo_i128),
    (multo_op,      tr32,      op_multo_r32),
    (multo_op,      tr64,      op_multo_r64),
    (multo_op,      tdec,      op_multo_dec),

    (divto_op,      tr32,      op_divto_r32),
    (divto_op,      tr64,      op_divto_r64),
    (divto_op,      tdec,      op_divto_dec),

    (idivto_op,     tu8,       op_idivto_short),
    (idivto_op,     tu64,      op_idivto_u64),
    (idivto_op,     ti64,      op_idivto_i64),
    (idivto_op,     tu128,     op_idivto_u128),
    (idivto_op,     ti128,     op_idivto_i128),
    (idivto_op,     tdec,      op_idivto_dec),

    (iremto_op,     tu64,      op_iremto_u64),
    (iremto_op,     ti64,      op_iremto_i64),
    (iremto_op,     tu128,     op_iremto_u128),
    (iremto_op,     ti128,     op_iremto_i128),
    (iremto_op,     tdec,      op_iremto_dec),

    (iandto_op,     tu8,       op_iandto_short),
    (iandto_op,     ti64,      op_iandto_i64),
    (iandto_op,     ti128,     op_iandto_i128),

    (iorto_op,      tu8,       op_iorto_short),
    (iorto_op,      ti64,      op_iorto_i64),
    (iorto_op,      ti128,     op_iorto_i128),

    (ixorto_op,     tu8,       op_ixorto_short),
    (ixorto_op,     ti64,      op_ixorto_i64),
    (ixorto_op,     ti128,     op_ixorto_i128),

    (shlto_op,      tu8,       op_shlto_short),
    (shlto_op,      tu64,      op_shlto_i64),
    (shlto_op,      ti64,      op_shlto_i64),
    (shlto_op,      tu128,     op_shlto_u128),
    (shlto_op,      ti128,     op_shlto_i128),
    (shlto_op,      tdec,      op_shlto_dec),

    (shrto_op,      tu8,       op_shrto_short),
    (shrto_op,      tu64,      op_shrto_u64),
    (shrto_op,      ti64,      op_shrto_i64),
    (shrto_op,      tu128,     op_shrto_u128),
    (shrto_op,      ti128,     op_shrto_i128),
    (shrto_op,      tdec,      op_shrto_dec),

    (appendto_op,   tvar,      op_appendto_var),

    (concatto_op,   tvar,      op_concatto_var),

    (minto_op,      tu64,      op_minto_u64),
    (minto_op,      ti64,      op_minto_i64),
    (minto_op,      tu128,     op_minto_u128),
    (minto_op,      ti128,     op_minto_i128),
    (minto_op,      tr32,      op_minto_r32),
    (minto_op,      tr64,      op_minto_r64),
    (minto_op,      tdec,      op_minto_dec),

    (maxto_op,      tu64,      op_maxto_u64),
    (maxto_op,      ti64,      op_maxto_i64),
    (maxto_op,      tu128,     op_maxto_u128),
    (maxto_op,      ti128,     op_maxto_i128),
    (maxto_op,      tr32,      op_maxto_r32),
    (maxto_op,      tr64,      op_maxto_r64),
    (maxto_op,      tdec,      op_maxto_dec),

    (negto_op,      ti64,      op_negto_i64),
    (negto_op,      ti128,     op_negto_i128),
    (negto_op,      tr32,      op_negto_r32),
    (negto_op,      tr64,      op_negto_r64),
    (negto_op,      tdec,      op_negto_dec),

    (absto_op,      ti64,      op_absto_i64),
    (absto_op,      ti128,     op_absto_i128),
    (absto_op,      tr32,      op_absto_r32),
    (absto_op,      tr64,      op_absto_r64),
    (absto_op,      tdec,      op_absto_dec),

    (inotto_op,     ti64,      op_inotto_i64),
    (inotto_op,     ti128,     op_inotto_i128),

!   (truncate_op,   tu64,      op_truncate_u64),
!   (truncate_op,   ti64,      op_truncate_i64),
!   (truncate_op,   tu128,     op_truncate_u128),
!   (truncate_op,   ti128,     op_truncate_i128),

    (tostr_op,      tc64,      op_tostr_c64),
    (tostr_op,      tu64,      op_tostr_u64),
    (tostr_op,      ti64,      op_tostr_i64),
    (tostr_op,      tu128,     op_tostr_u128),
    (tostr_op,      ti128,     op_tostr_i128),
    (tostr_op,      tr32,      op_tostr_r32),
    (tostr_op,      tr64,      op_tostr_r64),
    (tostr_op,      tdec,      op_tostr_dec),
    (tostr_op,      tref,      op_tostr_ref),
    (tostr_op,      tenum,     op_tostr_enum),
    (tostr_op,      tvar,      op_tostr_var),

    (tostrfmt_op,   tc64,      op_tostrfmt_c64),
    (tostrfmt_op,   tu64,      op_tostrfmt_u64),
    (tostrfmt_op,   ti64,      op_tostrfmt_i64),
    (tostrfmt_op,   tu128,     op_tostrfmt_u128),
    (tostrfmt_op,   ti128,     op_tostrfmt_i128),
    (tostrfmt_op,   tr32,      op_tostrfmt_r32),
    (tostrfmt_op,   tr64,      op_tostrfmt_r64),
    (tostrfmt_op,   tdec,      op_tostrfmt_dec),
    (tostrfmt_op,   tref,      op_tostrfmt_ref),
    (tostrfmt_op,   tenum,     op_tostrfmt_enum),
    (tostrfmt_op,   tvar,      op_tostrfmt_var),


    (eq_op,         ti64,      op_eq_i64),
    (eq_op,         ti128,     op_eq_i128),
    (eq_op,         tr32,      op_eq_r32),
    (eq_op,         tr64,      op_eq_r64),
    (eq_op,         tdec,      op_eq_dec),
    (eq_op,         tref,      op_eq_ref),
    (eq_op,         tenum,     op_eq_enum),
    (eq_op,         tvar,      op_eq_var),
    (eq_op,         tblock,    op_eq_block),

    (ne_op,         ti64,      op_ne_i64),
    (ne_op,         ti128,     op_ne_i128),
    (ne_op,         tr32,      op_ne_r32),
    (ne_op,         tr64,      op_ne_r64),
    (ne_op,         tdec,      op_ne_dec),
    (ne_op,         tref,      op_ne_ref),
    (ne_op,         tenum,     op_ne_enum),
    (ne_op,         tvar,      op_ne_var),
    (ne_op,         tblock,    op_ne_block),

    (lt_op,         tu64,      op_lt_u64),
    (lt_op,         ti64,      op_lt_i64),
    (lt_op,         tu128,     op_lt_u128),
    (lt_op,         ti128,     op_lt_i128),
    (lt_op,         tr32,      op_lt_r32),
    (lt_op,         tr64,      op_lt_r64),
    (lt_op,         tdec,      op_lt_dec),
    (lt_op,         tenum,     op_lt_enum),
    (lt_op,         tvar,      op_lt_var),
    (lt_op,         tref,      op_lt_ref),

    (le_op,         tu64,      op_le_u64),
    (le_op,         ti64,      op_le_i64),
    (le_op,         tu128,     op_le_u128),
    (le_op,         ti128,     op_le_i128),
    (le_op,         tr32,      op_le_r32),
    (le_op,         tr64,      op_le_r64),
    (le_op,         tdec,      op_le_dec),
    (le_op,         tenum,     op_le_enum),
    (le_op,         tvar,      op_le_var),
    (le_op,         tref,      op_le_ref),

    (ge_op,         tu64,      op_ge_u64),
    (ge_op,         ti64,      op_ge_i64),
    (ge_op,         tu128,     op_ge_u128),
    (ge_op,         ti128,     op_ge_i128),
    (ge_op,         tr32,      op_ge_r32),
    (ge_op,         tr64,      op_ge_r64),
    (ge_op,         tdec,      op_ge_dec),
    (ge_op,         tenum,     op_ge_enum),
    (ge_op,         tvar,      op_ge_var),
    (ge_op,         tref,      op_ge_ref),

    (gt_op,         tu64,      op_gt_u64),
    (gt_op,         ti64,      op_gt_i64),
    (gt_op,         tu128,     op_gt_u128),
    (gt_op,         ti128,     op_gt_i128),
    (gt_op,         tr32,      op_gt_r32),
    (gt_op,         tr64,      op_gt_r64),
    (gt_op,         tdec,      op_gt_dec),
    (gt_op,         tenum,     op_gt_enum),
    (gt_op,         tvar,      op_gt_var),
    (gt_op,         tref,      op_gt_ref),

    (same_op,       tvar,      op_same_var),

    (andl_op,       ti64,      op_andl_i64),

    (orl_op,        ti64,      op_orl_i64),

    (notl_op,       ti64,      op_notl_i64),

    (istruel_op,    ti64,      op_istruel_i64),
    (istruel_op,    ti128,     op_istruel_i128),
    (istruel_op,    tr32,      op_istruel_r32),
    (istruel_op,    tr64,      op_istruel_r64),
    (istruel_op,    tdec,      op_istruel_dec),
!   (istruel_op,    tref,      op_istruel_ref),
    (istruel_op,    tenum,     op_istruel_enum),
    (istruel_op,    tvar,      op_istruel_var),

    (andlto_op,     ti64,      op_andlto_i64),

    (orlto_op,      ti64,      op_orlto_i64),

    (notlto_op,     ti64,      op_notlto_i64),

    (istruelto_op,  ti64,      op_istruelto_i64))


!list of genops that have an int result, used to populate intresult[]
[]byte intresultlist = (
    in_op, notin_op, lwb_op, upb_op, len_op, lenstr_op, bitwidth_op,
    bytesize_op, eq_op, ne_op, lt_op, le_op, ge_op, gt_op,
    andl_op, orl_op, notl_op, istruel_op)

!!---
!Table of soft conversions between numeric types. In this form, easy to
!maintain than using a 2d table directly
global [,3]int16 softconvmappings =(
    (tc64,  tc64,   op_softconv),
    (tc64,  tu64,   op_softconv),
    (tc64,  tu128,  op_widen_u64_u128),
    (tc64,  ti64,   op_softconv),
    (tc64,  ti128,  op_widen_u64_u128),
    (tc64,  tr32,   op_float_u64_r32),
    (tc64,  tr64,   op_float_u64_r64),
    (tc64,  tdec,   op_todec_u64_dec),

    (tu64,  tc64,   op_softconv),
    (tu64,  tu64,   op_softconv),
    (tu64,  tu128,  op_widen_u64_u128),
    (tu64,  ti64,   op_softconv),
    (tu64,  ti128,  op_widen_u64_u128),
    (tu64,  tr32,   op_float_u64_r32),
    (tu64,  tr64,   op_float_u64_r64),
    (tu64,  tdec,   op_todec_u64_dec),

    (tu128, tc64,   op_softtrunc_128_64),
    (tu128, tu64,   op_softtrunc_128_64),
    (tu128, tu128,  op_softconv),
    (tu128, ti64,   op_softtrunc_128_64),
    (tu128, ti128,  op_softconv),
    (tu128, tr32,   op_error),
    (tu128, tr64,   op_error),
    (tu128, tdec,   op_error),

    (ti64,  tc64,   op_softconv),
    (ti64,  tu64,   op_softconv),
    (ti64,  tu128,  op_widen_i64_i128),
    (ti64,  ti64,   op_softconv),
    (ti64,  ti128,  op_widen_i64_i128),
    (ti64,  tr32,   op_float_i64_r32),
    (ti64,  tr64,   op_float_i64_r64),
    (ti64,  tdec,   op_todec_i64_dec),

    (ti128, tc64,   op_softtrunc_128_64),
    (ti128, tu64,   op_softtrunc_128_64),
    (ti128, tu128,  op_softconv),
    (ti128, ti64,   op_softtrunc_128_64),
    (ti128, ti128,  op_softconv),
    (ti128, tr32,   op_error),
    (ti128, tr64,   op_error),
    (ti128, tdec,   op_error),

    (tr32,  tc64,   op_fix_r32_u64),
    (tr32,  tu64,   op_fix_r32_u64),
    (tr32,  tu128,  op_error),
    (tr32,  ti64,   op_fix_r32_i64),
    (tr32,  ti128,  op_error),
    (tr32,  tr32,   op_softconv),
    (tr32,  tr64,   op_fwiden_r32_r64),
    (tr32,  tdec,   op_todec_r32_dec),

    (tr64,  tc64,   op_fix_r64_u64),
    (tr64,  tu64,   op_fix_r64_u64),
    (tr64,  tu128,  op_error),
    (tr64,  ti64,   op_fix_r64_i64),
    (tr64,  ti128,  op_error),
    (tr64,  tr32,   op_fnarrow_r64_r32),
    (tr64,  tr64,   op_softconv),
    (tr64,  tdec,   op_todec_r64_dec),

    (tdec,  tc64,   op_decto_dec_u64),
    (tdec,  tu64,   op_decto_dec_u64),
    (tdec,  tu128,  op_error),
    (tdec,  ti64,   op_decto_dec_i64),
    (tdec,  ti128,  op_error),
    (tdec,  tr32,   op_error),
    (tdec,  tr64,   op_error),
    (tdec,  tdec,   op_softconv))

!this 2d array maps [genop, basetype] to a spec-op
!It is filled in at runtime from genspecmappings

global [firstgenop..lastgenop, tfirsttabletype..tlasttabletype]int16 optypetable

global [firstgenop..lastgenop]byte intresult

global [tfirstnum..tlastnum, tfirstnum..tlastnum]int64 softconvtable

global [specopnames.lwb..specopnames.upb]byte specoptogen
global [specopnames.lwb..specopnames.upb]byte specoptotype

global proc inittypetables=
    int genop, s,t, a, specop

!populate the table with given combinations
    for i in genspecmappings.bounds do
        genop:=genspecmappings[i,1]
        t:=genspecmappings[i,2]
        specop:=genspecmappings[i,3]

        optypetable[genop,t] := specop

!build cross-reference tables
        specoptogen[specop]:=genop
        specoptotype[specop]:=t
    od

!now scan trying to fix some empty slots
    for genop in firstgenop..lastgenop do
        for t in tfirsttabletype..tlasttabletype do
            if optypetable[genop,t]=0 then
                case t
                when tc64 then
                    if a:=optypetable[genop,tu64] then optypetable[genop,tc64]:=a
                    elsif a:=optypetable[genop,ti64] then optypetable[genop,tc64]:=a
                    fi
                when tu64 then
                    if a:=optypetable[genop,ti64] then optypetable[genop,tu64]:=a fi
                when tu128 then
                    if a:=optypetable[genop,ti128] then optypetable[genop,tu128]:=a fi
                esac
            fi
        od
    od

!populate intresultlist
    for i in intresultlist.bounds do
        intresult[intresultlist[i]]:=1
    od

!do softconversions
    for i in softconvmappings.bounds do
        s:=softconvmappings[i,1]
        t:=softconvmappings[i,2]
        specop:=softconvmappings[i,3]

        softconvtable[s,t]:=specop
    od
end
