global tabledata() [0:]ichar stdtypenames, [0:]byte stdtypewidths,
         [0:]byte stdtypesigned, [0:]byte stdexpandtypes,
         [0:]byte stdtypecat, [0:]ichar stdtypemnames =
    (tnone=0,       "none",     0,  0,  0,      0,      ""),        !error or not set or not used
    (tvoid,         "void",     0,  0,  0,      0,      "void"),

    (tschar,        "schar",    8,  1,  tsint,  'I',    "i8"),      ! This ordering is important
    (tsshort,       "short",    16, 1,  tsint,  'I',    "i16"), !
    (tsint,         "int",      32, 1,  0,      'I',    "i32"), !
    (tsllong,       "llong",    64, 1,  0,      'I',    "i64"), !

    (tbool,         "bool",     8,  0,  tuint,  'U',    "byte"),        ! As is this
    (tuchar,        "uchar",    8,  0,  tuint,  'U',    "byte"),        !
    (tushort,       "ushort",   16, 0,  tuint,  'U',    "u16"), !
    (tuint,         "uint",     32, 0,  0,      'U',    "u32"), !
    (tullong,       "ullong",   64, 0,  0,      'U',    "u64"), !

    (tfloat,        "float",    32, 0,  0,      'R',    "r32"), ! And tfloat must be >= integer types
    (tdouble,       "double",   64, 0,  0,      'R',    "r64"), !
    (tldouble,      "ldouble",  128,0,  0,      'R',    "r64"), !

    (tcomplex,      "complex",  128,0,  0,      0,      ""),    !

    (tenum,         "enum",     0,  0,  0,      0,      ""),        !
    (tref,          "ref",      64, 0,  0,      0,      ""),    ! 
    (tproc,         "proc",     64, 0,  0,      0,      ""),    !
    (tlabel,        "label",    64, 0,  0,      0,      ""),    !

    (tarray,        "array",    0,  0,  0,      0,      ""),        !
    (tstruct,       "struct",   0,  0,  0,      0,      ""),        !
    (tunion,        "union",    0,  0,  0,      0,      ""),        !

!User-defined types go here
    (tlast,         $,          0,  0,  0,      0,      "")     !   !

end

global const tfirstnum=tschar, tlastnum=tldouble
global const tfirstint=tschar, tlastint=tullong
global const tfirstreal=tfloat, tlastreal=tldouble

global const tptroffset = tsllong       !for 64-bit target

global tabledata() []ichar typespecnames, []int32 typespectypes, []byte typespecsizes =
    (ts_void,       $,  tvoid,      0),
!   (ts_char,       $,  tuchar,     1),
    (ts_char,       $,  tschar,     1),
    (ts_short,      $,  0,          2),
    (ts_long,       $,  0,          4),
    (ts_int,        $,  tsint,      4),
    (ts_float,      $,  tfloat,     4),
    (ts_double,     $,  tdouble,    8),
    (ts_signed,     $,  0,          0),
    (ts_unsigned,   $,  0,          0),
    (ts_bool,       $,  tbool,      1),
    (ts_complex,    $,  tcomplex,   0),
    (ts_user,       $,  0,          0),
    (ts_struct,     $,  0,          0),
    (ts_union,      $,  0,          0),
    (ts_enum,       $,  0,          4),
    (ts_atomic,     $,  0,          0)
end

global tabledata() [0:]ichar pmflagnames=
    (pm_normal=0,       $),     ! Normal param
    (pm_notset,         $),     ! ()     (applied to one dummy tnone param)
    (pm_empty,          $),     ! (void) (applied to one dummy tnone param)
    (pm_variadic,       $)      ! (...) or (t,u,v,...) (applied to dummy or first param)
end

!scope here refers to linkage across modules
global tabledata() [0:]ichar scopenames=
    (no_scope=0,        "-"),       ! 
    (function_scope,    "Fn"),      !within a function (note import/exported names can be declared in a block scope)
    (local_scope,       "Loc"),     !file-scope/not exported 
    (imported_scope,    "Imp"),     !imported from another module
    (exported_scope,    "Exp")      !file-scope/exported
end

!Call conventions
global tabledata() []ichar ccnames=

    (open_cc=0,     $), ! Not set: either own or clang, depending on whether fn was defined
    (own_cc,        $), ! Internal (x86/x64)
    (clang_cc,      $), ! External (all x64; clang only x86)
    (stdcall_cc,    $), ! (x86 non-clang)
    (callback_cc,   $), ! Internal when called from External

    (dummy_cc,      $)  ! 
end

global tabledata() [0:]ichar linkagenames=
    (none_ss=0,     $),
    (static_ss,     $),
    (auto_ss,       $),
    (register_ss,   $),
    (extern_ss,     $),
    (typedef_ss,    $)
end

global tabledata() []ichar typequalnames=
    (const_qual,    $),
    (volatile_qual, $),
    (restrict_qual, $),
    (atomic_qual,   $)
end

global tabledata() []ichar fnspecnames=
    (inline_fnspec,     $),
    (noreturn_fnspec,   $),
    (callback_fnspec,   $),
end

global tabledata() =
    (pdm_date),
    (pdm_time),
    (pdm_file),
    (pdm_line),
    (pdm_func),
    (pdm_cdecl),
    (pdm_bcc),
    (pdm_stdc)
end

global tabledata() [0:]ichar jtagnames=

    (j_none=0,      $), !
    (j_const,       $), !
    (j_null,        $), !
    (j_name,        $), !
!   (j_nameaddr,    $), !
    (j_widenmem,    $), !
    (j_funcname,    $), !
    (j_block,       $), !
    (j_tempdecl,    $), !
    (j_decl,        $), !
!   (j_typeof,      $), !
!   (j_makeref,     $), !

!Statements

    (j_callproc,    $), ! 
    (j_return,      $), ! 
    (j_returnx,     $), ! 

    (j_assign,      $), ! 
    (j_if,          $), ! 
    (j_for,         $), ! 
    (j_while,       $), ! 
    (j_dowhile,     $), ! 
    (j_goto,        $), ! 
    (j_labelstmt,   $), ! 
    (j_casestmt,    $), ! 
    (j_defaultstmt, $), ! 
    (j_break,       $), ! [
    (j_continue,    $), ! [
    (j_switch,      $), ! 
    (j_breaksw,     $), ! [
!   (j_eval,        $), ! 

!Expressions and Operators

!Logical Operators

    (j_andl,        "&& andl"), ! 
    (j_orl,         "|| orl"), ! 
    (j_notl,        "! notl"), ! 
    (j_istruel,     $), ! 

!Expressions and Operators

    (j_makelist,    $), ! 
    (j_exprlist,    $), ! 

!   (j_assignx,     $), ! 
    (j_callfn,      $), ! 
    (j_ifx,         $), ! 

!Binary Ops

    (j_andand,      "&&"), ! a 

    (j_eq,          "=="), ! a 
    (j_ne,          "!="), ! a 
    (j_lt,          "<"), ! a 
    (j_le,          "<="), ! a 
    (j_gt,          ">"), ! a 
    (j_ge,          ">="), ! a 

    (j_add,         "+ add"), ! 
    (j_sub,         "- sub"), ! 
    (j_mul,         "* mul"), ! 
    (j_div,         "/ div"), ! 
    (j_rem,         "% mod"), ! 
    (j_iand,        "& iand"), ! 
    (j_ior,         "| ior"), ! 
    (j_ixor,        "^ ixor"), ! 
    (j_shl,         "<<"), ! a 
    (j_shr,         ">>"), ! a 

    (j_dot,         $), ! 
    (j_idot,        $), ! 
!   (j_dotref,      $), ! 
    (j_index,       $), ! 

    (j_ptr,         "ptr"), ! 
!   (j_ptroffset,   "ptroffset *"), ! 
    (j_addptr,      "addptr"), ! 
    (j_subptr,      "subptr"), ! 
    (j_addrof,      "addrof &"), ! 
    (j_convert,     $), ! 
    (j_scale,       $), ! 

!Monadic Ops

    (j_neg,         "- neg"), ! 
    (j_abs,         "abs"), ! 
    (j_inot,        "~ inot"), ! a

!In-place operators

    (j_addto,       "+="), ! a b    a+:=b
    (j_subto,       "-="), ! a b
    (j_multo,       "*="), ! a b
    (j_divto,       "/="), ! a b
    (j_remto,       "%="), ! a b
    (j_iandto,      "&="), ! a b
    (j_iorto,       "|="), ! a b
    (j_ixorto,      "^="), ! a b
    (j_shlto,       "<<="), ! a b
    (j_shrto,       ">>="), ! a b

    (j_sqrt,        "sqrt"), ! a

    (j_preincr,     "++ preincr"), ! a  ++a
    (j_predecr,     "-- preincr"), ! a  --a
    (j_postincr,    "++ postincr"), ! a a++
    (j_postdecr,    "-- postdecr"), ! a a--

    (j_cputime,     "cputime"), ! a a--

    (j_dummy,       $)
end

global tabledata() []ichar symbolnames, []ichar shortsymbolnames, []byte symboltojtag=

!First half are basic tokens returned by lexreadtoken()
    (errorsym,          $,  "",     0),         ! Lex error
    (dotsym,            $,  ".",    j_dot),     ! "."
    (idotsym,           $,  "->",   j_idot),    ! "->"
    (lexhashsym,        $,  "#",    0),         ! "#" as first symbol on line
    (hashsym,           $,  "#",    0),         ! "#" within macro def
    (lithashsym,        $,  "#",    0),         ! "#" literal hash (not stringify op)
    (hashhashsym,       $,  "##",   0),         ! "##" within macro def
    (commasym,          $,  ",",    0),         ! ","
    (semisym,           $,  ";",    0),         ! ";"
    (colonsym,          $,  ":",    0),         ! ":"
    (assignsym,         $,  "=",    j_assign),  ! =
    (assignsym2,        $,  ":=",   j_assign),  ! =
    (lbracksym,         $,  "(",    0),         ! (
    (rbracksym,         $,  ")",    0),         ! )
    (lsqsym,            $,  "[",    0),         !    [
    (rsqsym,            $,  "]",    0),         ! ]
    (lcurlysym,         $,  "{",    0),         ! {
    (rcurlysym,         $,  "}",    0),         ! }
    (questionsym,       $,  "?",    0),         ! ?
    (curlsym,           $,  "~",    0),         ! ~
    (ellipsissym,       $,  "...",  0),         ! ...
    (backslashsym,      $,  "\\",   0),         ! \
    (addsym,            $,  "+",    j_add),     ! +
    (subsym,            $,  "-",    j_sub),     !
    (mulsym,            $,  "*",    j_mul),     !
    (divsym,            $,  "/",    j_div),     !
    (remsym,            $,  "%",    j_rem),     !
    (iorsym,            $,  "|",    j_ior),     !
    (iandsym,           $,  "&",    j_iand),    !
    (ixorsym,           $,  "^",    j_ixor),    !
    (orlsym,            $,  "||",   j_orl),     !
    (andlsym,           $,  "&&",   j_andl),    !
    (shlsym,            $,  "<<",   j_shl),     !
    (shrsym,            $,  ">>",   j_shr),     !
    (inotsym,           $,  "~",    j_inot),    !
    (notlsym,           $,  "!",    j_notl),    !
    (incrsym,           $,  "++",   j_preincr), !
    (decrsym,           $,  "--",   j_predecr), !
    (abssym,            $,  "abs",  j_abs),     !

    (eqsym,             $,  "==",   j_eq),      !
    (nesym,             $,  "!=",   j_ne),      !
    (ltsym,             $,  "<",    j_lt),      !
    (lesym,             $,  "<=",   j_le),      !
    (gesym,             $,  ">=",   j_ge),      !
    (gtsym,             $,  ">",    j_gt),      !

    (addtosym,          $,  "+=",   j_addto),   !
    (subtosym,          $,  "-=",   j_subto),   !
    (multosym,          $,  "*=",   j_multo),   !
    (divtosym,          $,  "/=",   j_divto),   !
    (remtosym,          $,  "%=",   j_remto),   !
    (iortosym,          $,  "|=",   j_iorto),   !
    (iandtosym,         $,  "&=",   j_iandto),  !
    (ixortosym,         $,  "^=",   j_ixorto),  !
    (shltosym,          $,  "<<=",  j_shlto),   !
    (shrtosym,          $,  ">>=",  j_shrto),   !
    (sqrtsym,           $,  "sqrt", j_sqrt),    !

    (eolsym,            $,  "",     0),         !
    (eofsym,            $,  "",     0),         !
    (rawnumbersym,      $,  "n",    0),         !
    (intconstsym,       $,  "n",    0),         !
    (realconstsym,      $,  "n",    0),         !
    (charconstsym,      $,  "s",    0),         !
    (wcharconstsym,     $,  "s",    0),         !
    (stringconstsym,    $,  "s",    0),         !
    (wstringconstsym,   $,  "s",    0),         !
    (whitespacesym,     $,  "w",    0),         !
!   (placeholdersym,    $,  "<PH>", 0),         !
    (placeholdersym,    $,  "", 0),         !
    (kstrincludesym,    $,  "k",    0),         !

!Second half are tokens that can be yielded after a name lookup:
    (namesym,           $,  "k",    0),         ! identifier symbol
    (ksourcedirsym,     $,  "k",    0),         ! 
    (predefmacrosym,    $,  "k",    0),         ! __LINE__ etc

    (ktypespecsym,      $,  "k",    0),         ! INT, SHORT
    (kifsym,            $,  "k",    0),         ! IF
    (kelsesym,          $,  "k",    0),         ! ELSE
    (kcasesym,          $,  "k",    0),         ! CASE
    (kdefaultsym,       $,  "k",    0),         ! DEFAULT
    (kforsym,           $,  "k",    0),         ! FOR
    (kwhilesym,         $,  "k",    0),         ! WHILE
    (kdosym,            $,  "k",    0),         ! DO
    (kreturnsym,        $,  "k",    0),         ! RETURN
    (kbreaksym,         $,  "k",    0),         ! BREAK
    (kcontinuesym,      $,  "k",    0),         ! CONTINUE
    (kgotosym,          $,  "k",    0),         ! GO/GOTO
    (kswitchsym,        $,  "k",    0),         ! SWITCH
    (kstructsym,        $,  "k",    0),         ! STRUCT
    (kunionsym  ,       $,  "k",    0),         ! UNION
    (klinkagesym,       $,  "k",    0),         ! STATIC etc
    (ktypequalsym,      $,  "k",    0),         ! CONST etc
    (kfnspecsym,        $,  "k",    0),         ! INLINE etc
    (kalignassym,       $,  "k",    0),         ! _ALIGNAS
    (kenumsym,          $,  "k",    0),         ! ENUM
!   (kcallconvsym,      $,  "k",    0),         ! CLANG etc
    (ksizeofsym,        $,  "k",    0),         ! SIZEOF
    (klengthofsym,      $,  "k",    0),         ! LENGTHOF
    (kdefinedsym,       $,  "k",    0),         ! DEFINED
    (kgenericsym,       $,  "k",    0),         ! _GENERIC
    (kalignofsym,       $,  "k",    0),         ! _ALIGNOF
    (kshowmodesym,      $,  "k",    0),         ! SHOWMODE
    (kshowtypesym,      $,  "k",    0),         ! SHOWTYPE
    (ktypeofsym,        $,  "k",    0),         ! TYPEOF
    (kstrtypesym,       $,  "k",    0),         ! STRTYPE
    (kmccassertsym,     $,  "k",    0),         !
    (kcputimesym,       $,  "k",    0),         !
    (kconstantsym,      $,  "k",    0),         !CONSTANT
    (kstructinfosym,    $,  "k",    0),         !STRUCTINFO

    (kdummysym,         $,  "",     0)          !
end

global tabledata() []ichar sourcedirnames =
    (definedir,     $),
    (emitdir,       $),
    (ifdir,         $),
    (elifdir,       $),
    (elsedir,       $),
    (endifdir,      $),
    (includedir,    $),
    (ifdefdir,      $),
    (ifndefdir,     $),
    (undefdir,      $),
    (errordir,      $),
    (warningdir,    $),
    (messagedir,    $),
    (pausedir,      $),
    (debugondir,    $),
    (debugoffdir,   $),
    (showmacrodir,  $),
    (blankdir,      $),
    (linedir,       $),
!   (strincludedir, $),
    (pragmadir,     $)
end

global tabledata() [0:]ichar namespacenames=
    (ns_none=0,     $),         !not set
    (ns_general,    $),         !variables, functions, typedefs, enum names
    (ns_tags,       $),         !struct, union, enum tags
    (ns_labels,     $),         !label names
    (ns_fields,     $)          !field names
end

global tabledata() [0:]ichar namenames, [0:]int32 namespaces=
    (nullid=0,      $,      ns_none),       !Not assigned, or keyword/macro defined by .symbol
    (macroid,       $,      ns_none),       !
    (programid,     $,      ns_none),       !Main root
    (moduleid,      $,      ns_none),       !
    (extmoduleid,   $,      ns_none),       !
    (typeid,        $,      ns_general),    !Type name in type, proc or module
    (procid,        $,      ns_general),    !Proc/method/function/op name
    (staticid,      $,      ns_general),    !Static in type or proc or module
    (frameid,       $,      ns_general),    !Local var
    (paramid,       $,      ns_general),    !Local param
    (fieldid,       $,      ns_fields),     !Field of Record or Class
    (enumid,        $,      ns_general),    !Enum name, part of enum type only
!   (macroparamid,  $,      0),             !
    (enumtagid,     $,      ns_tags),       !
    (structtagid,   $,      ns_tags),       !
    (constantid,    $,      ns_general),    !
    (labelid,       $,      ns_labels)      !Label name in proc only
end

global tabledata []ichar stnames, []int32 stsymbols, []int32 stsubcodes=

    ("if",          kifsym,         j_if),
    ("else",        kelsesym,       0),
    ("case",        kcasesym,       0),
    ("default",     kdefaultsym,    0),
    ("for",         kforsym,        0),
    ("do",          kdosym,         0),
    ("while",       kwhilesym,      0),
    ("return",      kreturnsym,     0),
    ("break",       kbreaksym,      0),
    ("continue",    kcontinuesym,   0),
    ("goto",        kgotosym,       0),
    ("switch",      kswitchsym,     0),

    ("struct",      kstructsym,     0),
    ("union",       kunionsym,      0),

    ("include",     ksourcedirsym,  includedir),
    ("define",      ksourcedirsym,  definedir),
    ("elif",        ksourcedirsym,  elifdir),
    ("ifdef",       ksourcedirsym,  ifdefdir),
    ("ifndef",      ksourcedirsym,  ifndefdir),
    ("endif",       ksourcedirsym,  endifdir),
    ("undef",       ksourcedirsym,  undefdir),
    ("error",       ksourcedirsym,  errordir),
    ("warning",     ksourcedirsym,  warningdir),
    ("message",     ksourcedirsym,  messagedir),
    ("MESSAGE",     ksourcedirsym,  messagedir),
    ("pragma",      ksourcedirsym,  pragmadir),
    ("line",        ksourcedirsym,  linedir),
    ("pause",       ksourcedirsym,  pausedir),
    ("debugon",     ksourcedirsym,  debugondir),
    ("debugoff",    ksourcedirsym,  debugoffdir),
    ("showmacro",   ksourcedirsym,  showmacrodir),
!   ("strinclude",  ksourcedirsym,  strincludedir),
    ("strinclude",  kstrincludesym, 0),

    ("auto",        klinkagesym,        auto_ss),
    ("register",    klinkagesym,        register_ss),
    ("static",      klinkagesym,        static_ss),
    ("extern",      klinkagesym,        extern_ss),
    ("typedef",     klinkagesym,        typedef_ss),
    
    ("const",       ktypequalsym,   const_qual),
    ("volatile",    ktypequalsym,   volatile_qual),
    ("restrict",    ktypequalsym,   restrict_qual),
    ("_Atomic",     ktypequalsym,   atomic_qual),

    ("inline",      kfnspecsym,     inline_fnspec),
    ("_Noreturn",   kfnspecsym,     noreturn_fnspec),
    ("$callback",   kfnspecsym,     callback_fnspec),

    ("_Alignas",    kalignassym,    0),

    ("enum",        kenumsym,       0),

!   ("$stdcall",    kcallconvsym,   stdcall_cc),
!   ("$callback",   kcallconvsym,   callback_cc),
!   ("$windows",    kcallconvsym,   stdcall_cc),
!!  ("$clang",      kcallconvsym,   clang_cc),

    ("void",        ktypespecsym,   ts_void),
    ("char",        ktypespecsym,   ts_char),
    ("short",       ktypespecsym,   ts_short),
    ("long",        ktypespecsym,   ts_long),
    ("int",         ktypespecsym,   ts_int),
    ("float",       ktypespecsym,   ts_float),
    ("double",      ktypespecsym,   ts_double),
    ("signed",      ktypespecsym,   ts_signed),
    ("unsigned",    ktypespecsym,   ts_unsigned),

    ("_Bool",       ktypespecsym,   ts_char),

    ("_Complex",    ktypespecsym,   ts_complex),

    ("__DATE__",    predefmacrosym, pdm_date),
    ("__FILE__",    predefmacrosym, pdm_file),
    ("__LINE__",    predefmacrosym, pdm_line),
!   ("__STDC__",    predefmacrosym, pdm_stdc),
    ("__TIME__",    predefmacrosym, pdm_time),
!   ("__cdecl",     predefmacrosym, pdm_cdecl),
    ("__BCC__",     predefmacrosym, pdm_bcc),
    ("__func__",    predefmacrosym, pdm_func),
    ("__FUNCTION__",    predefmacrosym, pdm_func),

!   ("not",         notlsym,        0),
    ("sizeof",      ksizeofsym,     0),
    ("$sqrt",       sqrtsym,        0),
    ("defined",     kdefinedsym,    0),
    ("_Generic",    kgenericsym,    0),
    ("_Alignof",    kalignofsym,    0),
    ("$showmode",   kshowmodesym,   0),
    ("$showtype",   kshowtypesym,   0),
    ("typeof",      ktypeofsym,     0),
    ("strtype",     kstrtypesym,    0),
    ("_Static_assert",  kmccassertsym,  0),
    ("cputime",     kcputimesym,    0),
!   ("constant",    kconstantsym,   0),
    ("structinfo",  kstructinfosym, 0),

    ("$$dummy",     0,              0)
end

global tabledata() [0:]ichar convnames =

    (no_conv=0, $),
    (soft_c,    $),         !no conversion needed, just type change
    (hard_c,    $),         !explicit conversion, done as uwiden or narrow to match sizes

    (swiden_c,  $),         !widen with sign-extension  (1/2/4 to 4/8)
    (uwiden_c,  $),         !widen with zero-extension  (1/2/4 to 4/8)
    (sfloat_c,  $),         !signed int to float        (1/2/4/8 to 4/8)
    (ufloat_c,  $),         !unsigned int to float      (1/2/4/8 to 4/8)
    (sfix_c,    $),         !float to signed int        (4/8 to 1/2/4/8)
    (ufix_c,    $),         !float to unsigned int      (4/8 to 1/2/4/8)
    (fwiden_c,  $),         !float to wider float       (4 to 8)
    (fnarrow_c, $),         !float to narrower float    (8 to 4)
    (narrow_c,  $),         !narrow without truncation  (8/4/2 to 4/2/1)
    (truncate_c,$),         !narrow and truncate        (8/4/2 to 4/2/1)
    (bool_c,    $)          !int to bool                (1/2/4/8 to 1)
end

!take two basic numeric types and determine which is more dominant
!zeros mean not supported (error, not both numbers etc)
!(table could have been 16x16 but means checking both basic types being in-range first)

!dominantmode[s,t] returns the dominant type of s and t, widened to int/uint as needed
global [0:32,0:32]byte dominantmode

!conversionops[s,t] gives conversion op to convert numeric types s to t
global [0:16,0:16]byte conversionops

!table used to set up dominanttable[]
!3rd entry is the more dominant of the first two (wided as needed to int/unsigned int)
global [][3]byte dominantsetuptable=(
    (tschar,    tschar,     tsint),
    (tschar,    tsshort,    tsint),
    (tschar,    tsint,      tsint),
    (tschar,    tsllong,    tsllong),
    (tschar,    tbool,      tsint),
    (tschar,    tuchar,     tsint),
    (tschar,    tushort,    tsint),
    (tschar,    tuint,      tsint),
    (tschar,    tullong,    tsllong),
    (tschar,    tfloat,     tfloat),
    (tschar,    tdouble,    tdouble),
    (tschar,    tldouble,   tldouble),
    (tsshort,   tschar,     tsint),
    (tsshort,   tsshort,    tsint),
    (tsshort,   tsint,      tsint),
    (tsshort,   tsllong,    tsllong),
    (tsshort,   tbool,      tsint),
    (tsshort,   tuchar,     tsint),
    (tsshort,   tushort,    tsint),
    (tsshort,   tuint,      tsint),
    (tsshort,   tullong,    tsllong),
    (tsshort,   tfloat,     tfloat),
    (tsshort,   tdouble,    tdouble),
    (tsshort,   tldouble,   tldouble),
    (tsint,     tschar,     tsint),
    (tsint,     tsshort,    tsint),
    (tsint,     tsint,      tsint),
    (tsint,     tsllong,    tsllong),
    (tsint,     tbool,      tsint),
    (tsint,     tuchar,     tsint),
    (tsint,     tushort,    tsint),
    (tsint,     tuint,      tuint),
    (tsint,     tullong,    tsllong),
    (tsint,     tfloat,     tfloat),
    (tsint,     tdouble,    tdouble),
    (tsint,     tldouble,   tldouble),
    (tsllong,   tschar,     tsllong),
    (tsllong,   tsshort,    tsllong),
    (tsllong,   tsint,      tsllong),
    (tsllong,   tsllong,    tsllong),
    (tsllong,   tbool,      tsllong),
    (tsllong,   tuchar,     tsllong),
    (tsllong,   tushort,    tsllong),
    (tsllong,   tuint,      tsllong),
    (tsllong,   tullong,    tullong),
    (tsllong,   tfloat,     tfloat),
    (tsllong,   tdouble,    tdouble),
    (tsllong,   tldouble,   tldouble),
    (tbool,     tschar,     tsint),
    (tbool,     tsshort,    tsint),
    (tbool,     tsint,      tsint),
    (tbool,     tsllong,    tsllong),
    (tbool,     tbool,      tuint),
    (tbool,     tuchar,     tuint),
    (tbool,     tushort,    tuint),
    (tbool,     tuint,      tuint),
    (tbool,     tullong,    tullong),
    (tbool,     tfloat,     tfloat),
    (tbool,     tdouble,    tdouble),
    (tbool,     tldouble,   tldouble),
    (tuchar,    tschar,     tsint),
    (tuchar,    tsshort,    tsint),
    (tuchar,    tsint,      tsint),
    (tuchar,    tsllong,    tsllong),
    (tuchar,    tbool,      tnone),
    (tuchar,    tuchar,     tuint),
    (tuchar,    tushort,    tuint),
    (tuchar,    tuint,      tuint),
    (tuchar,    tullong,    tullong),
    (tuchar,    tfloat,     tfloat),
    (tuchar,    tdouble,    tdouble),
    (tuchar,    tldouble,   tldouble),
    (tushort,   tschar,     tsint),
    (tushort,   tsshort,    tsint),
    (tushort,   tsint,      tsint),
    (tushort,   tsllong,    tsllong),
    (tushort,   tbool,      tuint),
    (tushort,   tuchar,     tuint),
    (tushort,   tushort,    tuint),
    (tushort,   tuint,      tuint),
    (tushort,   tullong,    tullong),
    (tushort,   tfloat,     tfloat),
    (tushort,   tdouble,    tdouble),
    (tushort,   tldouble,   tldouble),
    (tuint,     tschar,     tsint),
    (tuint,     tsshort,    tsint),
    (tuint,     tsint,      tuint),
    (tuint,     tsllong,    tsllong),
    (tuint,     tbool,      tuint),
    (tuint,     tuchar,     tuint),
    (tuint,     tushort,    tuint),
    (tuint,     tuint,      tuint),
    (tuint,     tullong,    tullong),
    (tuint,     tfloat,     tfloat),
    (tuint,     tdouble,    tdouble),
    (tuint,     tldouble,   tldouble),
    (tullong,   tschar,     tullong),
    (tullong,   tsshort,    tullong),
    (tullong,   tsint,      tullong),
    (tullong,   tsllong,    tullong),
    (tullong,   tbool,      tullong),
    (tullong,   tuchar,     tullong),
    (tullong,   tushort,    tullong),
    (tullong,   tuint,      tullong),
    (tullong,   tullong,    tullong),
    (tullong,   tfloat,     tfloat),
    (tullong,   tdouble,    tdouble),
    (tullong,   tldouble,   tldouble),
    (tfloat,    tschar,     tdouble),
    (tfloat,    tsshort,    tdouble),
    (tfloat,    tsint,      tdouble),
    (tfloat,    tsllong,    tdouble),
    (tfloat,    tbool,      tdouble),
    (tfloat,    tuchar,     tdouble),
    (tfloat,    tushort,    tdouble),
    (tfloat,    tuint,      tdouble),
    (tfloat,    tullong,    tdouble),
    (tfloat,    tfloat,     tfloat),
    (tfloat,    tdouble,    tdouble),
    (tfloat,    tldouble,   tldouble),
    (tdouble,   tschar,     tdouble),
    (tdouble,   tsshort,    tdouble),
    (tdouble,   tsint,      tdouble),
    (tdouble,   tsllong,    tdouble),
    (tdouble,   tbool,      tdouble),
    (tdouble,   tuchar,     tdouble),
    (tdouble,   tushort,    tdouble),
    (tdouble,   tuint,      tdouble),
    (tdouble,   tullong,    tdouble),
    (tdouble,   tfloat,     tdouble),
    (tdouble,   tdouble,    tdouble),
    (tdouble,   tldouble,   tldouble),
    (tldouble,  tschar,     tdouble),
    (tldouble,  tsshort,    tdouble),
    (tldouble,  tsint,      tdouble),
    (tldouble,  tsllong,    tdouble),
    (tldouble,  tbool,      tdouble),
    (tldouble,  tuchar,     tdouble),
    (tldouble,  tushort,    tdouble),
    (tldouble,  tuint,      tdouble),
    (tldouble,  tullong,    tdouble),
    (tldouble,  tfloat,     tdouble),
    (tldouble,  tdouble,    tdouble),
    (tldouble,  tldouble,   tldouble),
)

!table used to set up conversionops
global [][3]byte convsetuptable=(
    (tschar,    tschar,     swiden_c),
    (tschar,    tsshort,    swiden_c),
    (tschar,    tsint,      swiden_c),
    (tschar,    tsllong,    swiden_c),
    (tschar,    tbool,      bool_c),
    (tschar,    tuchar,     soft_c),
    (tschar,    tushort,    swiden_c),
    (tschar,    tuint,      swiden_c),
    (tschar,    tullong,    swiden_c),
    (tschar,    tfloat,     sfloat_c),
    (tschar,    tdouble,    sfloat_c),
    (tschar,    tldouble,   sfloat_c),

    (tsshort,   tschar,     truncate_c),
    (tsshort,   tsshort,    no_conv),
    (tsshort,   tsint,      swiden_c),
    (tsshort,   tsllong,    swiden_c),
    (tsshort,   tbool,      bool_c),
    (tsshort,   tuchar,     truncate_c),
    (tsshort,   tushort,    soft_c),
    (tsshort,   tuint,      swiden_c),
    (tsshort,   tullong,    swiden_c),
    (tsshort,   tfloat,     sfloat_c),
    (tsshort,   tdouble,    sfloat_c),
    (tsshort,   tldouble,   sfloat_c),
    (tsint,     tschar,     truncate_c),

    (tsint,     tsshort,    truncate_c),

    (tsint,     tsint,      no_conv),
    (tsint,     tsllong,    swiden_c),
    (tsint,     tbool,      bool_c),
    (tsint,     tuchar,     truncate_c),
    (tsint,     tushort,    truncate_c),
    (tsint,     tuint,      soft_c),
    (tsint,     tullong,    swiden_c),
    (tsint,     tfloat,     sfloat_c),
    (tsint,     tdouble,    sfloat_c),
    (tsint,     tldouble,   sfloat_c),

    (tsllong,   tschar,     truncate_c),
!   (tsllong,   tschar,     narrow_c),

    (tsllong,   tsshort,    truncate_c),
    (tsllong,   tsint,      truncate_c),
    (tsllong,   tsllong,    no_conv),
    (tsllong,   tbool,      bool_c),

    (tsllong,   tuchar,     truncate_c),
!   (tsllong,   tuchar,     narrow_c),

    (tsllong,   tushort,    truncate_c),
    (tsllong,   tuint,      truncate_c),
    (tsllong,   tullong,    soft_c),
    (tsllong,   tfloat,     sfloat_c),
    (tsllong,   tdouble,    sfloat_c),
    (tsllong,   tldouble,   sfloat_c),
    (tbool,     tschar,     soft_c),
    (tbool,     tsshort,    uwiden_c),
    (tbool,     tsint,      uwiden_c),
    (tbool,     tsllong,    uwiden_c),
    (tbool,     tbool,      no_conv),
    (tbool,     tuchar,     soft_c),
    (tbool,     tushort,    uwiden_c),
    (tbool,     tuint,      uwiden_c),
    (tbool,     tullong,    uwiden_c),
    (tbool,     tfloat,     ufloat_c),
    (tbool,     tdouble,    ufloat_c),
    (tbool,     tldouble,   ufloat_c),
    (tuchar,    tschar,     soft_c),
    (tuchar,    tsshort,    uwiden_c),
    (tuchar,    tsint,      uwiden_c),
    (tuchar,    tsllong,    uwiden_c),
    (tuchar,    tbool,      bool_c),
    (tuchar,    tuchar,     soft_c),
    (tuchar,    tushort,    uwiden_c),
    (tuchar,    tuint,      uwiden_c),
    (tuchar,    tullong,    uwiden_c),
    (tuchar,    tfloat,     ufloat_c),
    (tuchar,    tdouble,    ufloat_c),
    (tuchar,    tldouble,   ufloat_c),

    (tushort,   tschar,     truncate_c),
    (tushort,   tsshort,    soft_c),
    (tushort,   tsint,      uwiden_c),
    (tushort,   tsllong,    uwiden_c),
    (tushort,   tbool,      bool_c),
    (tushort,   tuchar,     truncate_c),
    (tushort,   tushort,    no_conv),
    (tushort,   tuint,      uwiden_c),
    (tushort,   tullong,    uwiden_c),
    (tushort,   tfloat,     ufloat_c),
    (tushort,   tdouble,    ufloat_c),
    (tushort,   tldouble,   ufloat_c),

    (tuint,     tschar,     truncate_c),
    (tuint,     tsshort,    truncate_c),
    (tuint,     tsint,      soft_c),
    (tuint,     tsllong,    uwiden_c),
    (tuint,     tbool,      bool_c),
    (tuint,     tuchar,     truncate_c),
    (tuint,     tushort,    truncate_c),
    (tuint,     tuint,      no_conv),
    (tuint,     tullong,    uwiden_c),
    (tuint,     tfloat,     ufloat_c),
    (tuint,     tdouble,    ufloat_c),
    (tuint,     tldouble,   ufloat_c),

    (tullong,   tschar,     truncate_c),
    (tullong,   tsshort,    truncate_c),
    (tullong,   tsint,      truncate_c),
    (tullong,   tsllong,    soft_c),
    (tullong,   tbool,      bool_c),
    (tullong,   tuchar,     truncate_c),
    (tullong,   tushort,    truncate_c),
    (tullong,   tuint,      truncate_c),
    (tullong,   tullong,    no_conv),
    (tullong,   tfloat,     ufloat_c),
    (tullong,   tdouble,    ufloat_c),
    (tullong,   tldouble,   ufloat_c),

    (tfloat,    tschar,     sfix_c),
    (tfloat,    tsshort,    sfix_c),
    (tfloat,    tsint,      sfix_c),
    (tfloat,    tsllong,    sfix_c),
    (tfloat,    tbool,      ufix_c),
    (tfloat,    tuchar,     ufix_c),
    (tfloat,    tushort,    ufix_c),
    (tfloat,    tuint,      ufix_c),
    (tfloat,    tullong,    ufix_c),
    (tfloat,    tfloat,     no_conv),
    (tfloat,    tdouble,    fwiden_c),
    (tfloat,    tldouble,   fwiden_c),

    (tdouble,   tschar,     sfix_c),
    (tdouble,   tsshort,    sfix_c),
    (tdouble,   tsint,      sfix_c),
    (tdouble,   tsllong,    sfix_c),
    (tdouble,   tbool,      ufix_c),
    (tdouble,   tuchar,     ufix_c),
    (tdouble,   tushort,    ufix_c),
    (tdouble,   tuint,      ufix_c),
    (tdouble,   tullong,    ufix_c),
    (tdouble,   tfloat,     fnarrow_c),
    (tdouble,   tdouble,    no_conv),
    (tdouble,   tldouble,   no_conv),

    (tldouble,  tschar,     sfix_c),
    (tldouble,  tsshort,    sfix_c),
    (tldouble,  tsint,      sfix_c),
    (tldouble,  tsllong,    sfix_c),
    (tldouble,  tbool,      ufix_c),
    (tldouble,  tuchar,     ufix_c),
    (tldouble,  tushort,    ufix_c),
    (tldouble,  tuint,      ufix_c),
    (tldouble,  tullong,    ufix_c),
    (tldouble,  tfloat,     fnarrow_c),
    (tldouble,  tdouble,    no_conv),
    (tldouble,  tldouble,   no_conv),
)

global []int badexprs=(
j_const,
j_name,
j_ifx,
j_andl,
j_orl,
j_notl,
j_istruel,
j_exprlist,
j_andand,
j_eq,
j_ne,
j_lt,
j_le,
j_ge,
j_gt,
j_add,
j_sub,
j_mul,
j_div,
j_rem,
j_iand,
j_ior,
j_ixor,
j_shl,
j_shr,
j_dot,
j_idot,
j_index,
j_ptr,
j_addptr,
j_subptr,
j_neg,
j_abs,
j_inot)

