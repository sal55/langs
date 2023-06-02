!x64 Assembler Tables

global enumdata []ichar symbolnames=
    (errorsym,          $),     ! Lex error
    (commasym,          $),     ! ","
    (colonsym,          $),     ! ":"
    (dcolonsym,         $),     ! "::"
    (lsqsym,            $),     ! [
    (rsqsym,            $),     ! ]

    (addsym,            $),     ! +
    (subsym,            $),     ! -
    (mulsym,            $),     ! *

    (eqsym,             $),     ! =

    (eolsym,            $),     ! End of line
    (eofsym,            $),     ! Eof seen

    (hashsym,           $),     ! #

    (intconstsym,       $),     ! 123 64 bits signed
    (realconstsym,      $),     ! 123.4 64 bits
    (stringconstsym,    $),     ! "ABC"

    (namesym,           $),     ! raw name
    (namedconstsym,     $),     ! name = expr
    (fwdlocalsym,       $),     ! name
    (localsym,          $),     ! name::
    (importedsym,       $),     ! name*
    (exportedsym,       $),     ! name:::

    (kopcodesym,        $),     ! mov etc
    (kregsym,           $),     ! d0, r5, eax etc
    (kxregsym,          $),     ! xmm0 etc
    (kfregsym,          $),     ! st0 etc
    (kmregsym,          $),     ! mmx1 etc
    (kjmpccsym,         $),     ! jz etc
    (ksetccsym,         $),     ! setz etc
    (kmovccsym,         $),     ! cmovz etc
    (kprefixsym,        $),     ! dword etc
    (ksegnamesym,       $),     ! idata etc
    (kimportlibsym,     $),     ! importlib
    (kimportdllsym,     $),     ! importdll

    (kdummysym,         $)      !
end

global enumdata []ichar mclnames, []byte mclnopnds, []byte mclcodes =
    (m_comment,         $,      0,      0),     !
    (m_blank,           $,      0,      0),     !
    (m_end,             $,      0,      0),     !

    (m_labelx,          $,      1,      0),     !
    (m_nop,             $,      0,      0x90),      !
    (m_param,           $,      1,      0),     !
    (m_assem,           $,      1,      0),     !
    (m_proc,            $,      1,      0),     !

    (m_mov,             $,      2,      0),     !
    (m_push,            $,      1,      0),     !
    (m_pop,             $,      1,      0),     !
    (m_lea,             $,      2,      0),     !
    (m_cmovcc,          $,      2,      0),     !

    (m_movd,            $,      2,      0),     !
    (m_movq,            $,      2,      0),     !

    (m_movsx,           $,      2,      0),     !
    (m_movzx,           $,      2,      0),     !
    (m_movsxd,          $,      2,      0),     !

    (m_call,            $,      1,      0xE8),      !
    (m_ret,             $,      0,      0xC3),  !
    (m_retn,            $,      1,      0),     !
    (m_leave,           $,      0,      0xC9),      !

    (m_jmp,             $,      1,      0xE9),  !
    (m_jmpcc,           $,      2,      0),     !
    (m_xchg,            $,      2,      0),     !

    (m_add,             $,      2,      0),     !
    (m_sub,             $,      2,      5),     !
    (m_adc,             $,      2,      2),     !
    (m_sbb,             $,      2,      3),     !
    (m_imul,            $,      1,      5),     !
    (m_mul,             $,      1,      4),     !
    (m_imul2,           $,      2,      0),     !
    (m_imul3,           $,      3,      0),     !

    (m_idiv,            $,      1,      7),     !
    (m_div,             $,      1,      6),     !

    (m_and,             $,      2,      0x04),  !
    (m_or,              $,      2,      0x01),  !
    (m_xor,             $,      2,      0x06),  !
    (m_test,            $,      2,      0),     !

    (m_cmp,             $,      2,      0x07),  !

    (m_shl,             $,      2,      0x04),  !
    (m_sar,             $,      2,      0x07),  !
    (m_shr,             $,      2,      0x05),  !
    (m_rol,             $,      2,      0x00),  !
    (m_ror,             $,      2,      0x01),  !
    (m_rcl,             $,      2,      0x02),  !
    (m_rcr,             $,      2,      0x03),  !

    (m_neg,             $,      1,      3),     !
    (m_not,             $,      1,      2),     !

    (m_inc,             $,      1,      0),     !
    (m_dec,             $,      1,      1),     !

    (m_cbw,             $,      0,      0), !
    (m_cwd,             $,      0,      0), !
    (m_cdq,             $,      0,      0),     !
    (m_cqo,             $,      0,      0),     !
    (m_setcc,           $,      2,      0),     !

    (m_bsf,             $,      2,      0xBC),  !
    (m_bsr,             $,      2,      0xBD),  !

    (m_sqrtsd,          $,      2,      0x51),  !
    (m_sqrtss,          $,      2,      0x51),  !
    (m_addss,           $,      2,      0x58),  !
    (m_subss,           $,      2,      0x5C),  !
    (m_mulss,           $,      2,      0x59),  !
    (m_divss,           $,      2,      0x5E),  !

    (m_addsd,           $,      2,      0x58),  !
    (m_subsd,           $,      2,      0x5C),  !
    (m_mulsd,           $,      2,      0x59),  !
    (m_divsd,           $,      2,      0x5E),  !

    (m_comiss,          $,      2,      0),     !
    (m_comisd,          $,      2,      0),     !
    (m_xorpd,           $,      2,      0x57),  !
    (m_xorps,           $,      2,      0x57),  !
    (m_andpd,           $,      2,      0x54),  !
    (m_andps,           $,      2,      0x54),  !
    (m_pxor,            $,      2,      0xEF),  !
    (m_pand,            $,      2,      0xDB),  !
    (m_cvtss2si,        $,      2,      0),     !
    (m_cvtsd2si,        $,      2,      0),     !
    (m_cvttss2si,       $,      2,      0),     !
    (m_cvttsd2si,       $,      2,      0),     !

    (m_cvtsi2ss,        $,      2,      0),     !
    (m_cvtsi2sd,        $,      2,      0),     !

    (m_cvtsd2ss,        $,      2,      0),     !
    (m_cvtss2sd,        $,      2,      0),     !

    (m_movdqa,          $,      2,      0x66),  !
    (m_movdqu,          $,      2,      0xF3),  !

    (m_pcmpistri,       $,      3,      0x63),      !
    (m_pcmpistrm,       $,      3,      0x62),      !

    (m_fld,             $,      1,      0),     !
    (m_fst,             $,      1,      2),     !
    (m_fstp,            $,      1,      3),     !

    (m_fild,            $,      1,      0),     !
    (m_fist,            $,      1,      2),     !
    (m_fistp,           $,      1,      3),     !

    (m_fadd,            $,      0,      0xC1),  !
    (m_fsub,            $,      0,      0xE9),  !
    (m_fmul,            $,      0,      0xC9),  !
    (m_fdiv,            $,      0,      0xF9),  !
    (m_fsqrt,           $,      0,      0xFA),  !
    (m_fsin,            $,      0,      0xFE),  !
    (m_fcos,            $,      0,      0xFF),  !
    (m_fsincos,         $,      0,      0xFB),  !
    (m_fptan,           $,      0,      0xF2),  !
    (m_fpatan,          $,      0,      0xF3),  !
    (m_fabs,            $,      0,      0xE1),  !
    (m_fchs,            $,      0,      0xE0),  !

    (m_minss,           $,      2,      0x5D),  !
    (m_maxss,           $,      2,      0x5F),  !
    (m_minsd,           $,      2,      0x5D),  !
    (m_maxsd,           $,      2,      0x5F),  !

    (m_db,              $,      1,      0),     !
    (m_dw,              $,      1,      0),     !
    (m_dd,              $,      1,      0),     !
    (m_dq,              $,      1,      0),     !
    (m_ddoffset,        $,      1,      0),     !

    (m_segment,         $,      1,      0),     !
    (m_isegment,        $,      0,      0),     !
    (m_zsegment,        $,      0,      0),     !
    (m_csegment,        $,      0,      0),     !

    (m_align,           $,      1,      0),     !
    (m_resb,            $,      1,      1),     !
    (m_resw,            $,      1,      2),     !
    (m_resd,            $,      1,      4),     !
    (m_resq,            $,      1,      8),     !

    (m_xlat,            $,      0,      0xD7),  !
    (m_loopnz,          $,      1,      0xE0),  !
    (m_loopz,           $,      1,      0xE1),  !
    (m_loopcx,          $,      1,      0xE2),  !
    (m_jecxz,           $,      1,      0xE3),  !
    (m_jrcxz,           $,      1,      0xE3),  !

    (m_cmpsb,           $,      0,      0),     !
    (m_cmpsw,           $,      0,      0),     !
    (m_cmpsd,           $,      0,      0),     !
    (m_cmpsq,           $,      0,      0),     !

    (m_rdtsc,           $,      0,      0x31),  !
    (m_popcnt,          $,      2,      0),     !

    (m_finit,           $,      0,      0),     !

    (m_fldz,            $,      0,      0xEE),  !
    (m_fld1,            $,      0,      0xE8),  !
    (m_fldpi,           $,      0,      0xEB),  !
    (m_fld2t,           $,      0,      0xE9),  !
    (m_fld2e,           $,      0,      0xEA),  !
    (m_fldlg2,          $,      0,      0xEC),  !
    (m_fldln2,          $,      0,      0xED),  !

    (m_halt,            $,      0,      0xF4),  !
end

global enumdata [0:]ichar regnames, [0:]byte regcodes =
    (rnone=0,   $,  0),         !
    (r0,        $,  0),         !d0 rax
    (r1,        $,  10),        !d1 r10
    (r2,        $,  11),        !d2 r11
    (r3,        $,  7),         !d3 rdi
    (r4,        $,  3),         !d4 rbx
    (r5,        $,  6),         !d5 rsi
    (r6,        $,  12),        !d6 r12
    (r7,        $,  13),        !d7 r13
    (r8,        $,  14),        !d8 r14
    (r9,        $,  15),        !d9 r15
    (r10,       $,  1),         !d10 rcx
    (r11,       $,  2),         !d11 rdx
    (r12,       $,  8),         !d12 r8
    (r13,       $,  9),         !d13 r9
    (r14,       $,  5),         !d14 rbp
    (r15,       $,  4),         !d15 rsp

    (r16,       $,  4),         !b0h ah
    (r17,       $,  7),         !b1h bh
    (r18,       $,  5),         !b10h ch
    (r19,       $,  6),         !b11h dh
end

global const rframe = r14
global const rstack = r15


!I use my own register designations Dn, An, Wn, Bn (8,4,2,1 bytes),
!which have a more sensible order than the official ones.
!The mapping is shown against Dn. Some (not all) of the official register
!names are used too

!Regindex is the ordinal value used to represent the register: 1..16
!This table is intended for initialising the global symbol table

global tabledata []ichar dregnames, []byte regsizes, []byte regindices =
    ("d0",      8,  r0),        !rax    d0..d9 are for general use
    ("d1",      8,  r1),        !r10    d0..d2 are volatile in ABI
    ("d2",      8,  r2),        !r11

    ("d3",      8,  r3),        !rdi    d3..d9 are preserved across funcs in ABI
    ("d4",      8,  r4),        !rbx
    ("d5",      8,  r5),        !rsi
    ("d6",      8,  r6),        !r12
    ("d7",      8,  r7),        !r13
    ("d8",      8,  r8),        !r14
    ("d9",      8,  r9),        !r15

    ("d10",     8,  r10),       !rcx    d10..d13 are win64 ABI register passing regs
    ("d11",     8,  r11),       !rdx    ..
    ("d12",     8,  r12),       !r8     ..
    ("d13",     8,  r13),       !r9     ..

    ("d14",     8,  r14),       !rbp    frame pointer
    ("d15",     8,  r15),       !rsp    stack pointer

    ("a0",      4,  r0),
    ("a1",      4,  r1),
    ("a2",      4,  r2),
    ("a3",      4,  r3),
    ("a4",      4,  r4),
    ("a5",      4,  r5),
    ("a6",      4,  r6),
    ("a7",      4,  r7),
    ("a8",      4,  r8),
    ("a9",      4,  r9),
    ("a10",     4,  r10),
    ("a11",     4,  r11),
    ("a12",     4,  r12),
    ("a13",     4,  r13),
    ("a14",     4,  r14),
    ("a15",     4,  r15),

    ("w0",      2,  r0),
    ("w1",      2,  r1),
    ("w2",      2,  r2),
    ("w3",      2,  r3),
    ("w4",      2,  r4),
    ("w5",      2,  r5),
    ("w6",      2,  r6),
    ("w7",      2,  r7),
    ("w8",      2,  r8),
    ("w9",      2,  r9),
    ("w10",     2,  r10),
    ("w11",     2,  r11),
    ("w12",     2,  r12),
    ("w13",     2,  r13),
    ("w14",     2,  r14),
    ("w15",     2,  r15),


    ("b0",      1,  r0),
    ("b1",      1,  r1),
    ("b2",      1,  r2),
    ("b3",      1,  r3),
    ("b4",      1,  r4),
    ("b5",      1,  r5),
    ("b6",      1,  r6),
    ("b7",      1,  r7),
    ("b8",      1,  r8),
    ("b9",      1,  r9),
    ("b10",     1,  r10),
    ("b11",     1,  r11),
    ("b12",     1,  r12),
    ("b13",     1,  r13),
    ("b14",     1,  r14),
    ("b15",     1,  r15),
    ("b16",     1,  r16),
    ("b17",     1,  r17),
    ("b18",     1,  r18),
    ("b19",     1,  r19),

    ("rax",     8,  r0),
    ("rbx",     8,  r4),
    ("rcx",     8,  r10),
    ("rdx",     8,  r11),
    ("rsi",     8,  r5),
    ("rdi",     8,  r3),
    ("rbp",     8,  r14),
    ("rsp",     8,  r15),
    ("r8",      8,  r12),
    ("r9",      8,  r13),
    ("r10",     8,  r1),
    ("r11",     8,  r2),
    ("r12",     8,  r6),
    ("r13",     8,  r7),
    ("r14",     8,  r8),
    ("r15",     8,  r9),

    ("eax",     4,  r0),
    ("ebx",     4,  r4),
    ("ecx",     4,  r10),
    ("edx",     4,  r11),
    ("esi",     4,  r5),
    ("edi",     4,  r3),
    ("ebp",     4,  r14),
    ("esp",     4,  r15),
    ("r8d",     4,  r12),
    ("r9d",     4,  r13),
    ("r10d",    4,  r1),
    ("r11d",    4,  r2),
    ("r12d",    4,  r6),
    ("r13d",    4,  r7),
    ("r14d",    4,  r8),
    ("r15d",    4,  r9),

    ("ax",      2,  r0),
    ("bx",      2,  r4),
    ("cx",      2,  r10),
    ("dx",      2,  r11),
    ("si",      2,  r5),
    ("di",      2,  r3),
    ("bp",      2,  r14),
    ("sp",      2,  r15),
    ("r8w",     2,  r12),
    ("r9w",     2,  r13),
    ("r10w",    2,  r1),
    ("r11w",    2,  r2),
    ("r12w",    2,  r6),
    ("r13w",    2,  r7),
    ("r14w",    2,  r8),
    ("r15w",    2,  r9),


    ("al",      1,  r0),
    ("bl",      1,  r4),
    ("cl",      1,  r10),
    ("dl",      1,  r11),

    ("ah",      1,  r16),
    ("bh",      1,  r17),
    ("ch",      1,  r18),
    ("dh",      1,  r19),

    ("sil",     1,  r5),
    ("dil",     1,  r3),
    ("bpl",     1,  r14),
    ("spl",     1,  r15),

    ("r8b",     1,  r12),
    ("r9b",     1,  r13),
    ("r10b",    1,  r1),
    ("r11b",    1,  r2),
    ("r12b",    1,  r6),
    ("r13b",    1,  r7),
    ("r14b",    1,  r8),
    ("r15b",    1,  r9),

end

global []ichar xregnames = (
    "xmm0",             ! x0..x3 are used for parameter passing in ABI
    "xmm1",
    "xmm2",
    "xmm3",

    "xmm4",             ! x4..x5 are volatile
    "xmm5",

    "xmm6",             ! x6..x15 are preserved across functions in ABI
    "xmm7",
    "xmm8",
    "xmm9",
    "xmm10",
    "xmm11",
    "xmm12",
    "xmm13",
    "xmm14",
    "xmm15")

global []ichar fregnames = (
    "st0",
    "st1",
    "st2",
    "st3",
    "st4",
    "st5",
    "st6",
    "st7")

global []ichar mregnames = (
    "mmx0",
    "mmx1",
    "mmx2",
    "mmx3",
    "mmx4",
    "mmx5",
    "mmx6",
    "mmx7")

global enumdata [0:]ichar condnames =

    (ov_cond    = 0,    "o"),
    (nov_cond   ,   "no"),

    (ltu_cond   ,   "b"),
    (geu_cond   ,   "ae"),

    (eq_cond    ,   "z"),
    (ne_cond    ,   "nz"),

    (leu_cond   ,   "be"),
    (gtu_cond   ,   "a"),

    (s_cond     ,   "s"),
    (ns_cond    ,   "ns"),

    (p_cond     ,   "p"),
    (np_cond    ,   "np"),

    (lt_cond    ,   "l"),
    (ge_cond    ,   "ge"),

    (le_cond    ,   "le"),
    (gt_cond    ,   "g"),
end

global tabledata []ichar jmpccnames, []byte jmpcccodes =
    ("jo",      ov_cond),
    ("jno",     nov_cond),
    ("jb",      ltu_cond),
    ("jae",     geu_cond),
    ("jz",      eq_cond),
    ("jnz",     ne_cond),
    ("jbe",     leu_cond),
    ("ja",      gtu_cond),
    ("js",      s_cond),
    ("jns",     ns_cond),
    ("jp",      p_cond),
    ("jnp",     np_cond),
    ("jl",      lt_cond),
    ("jge",     ge_cond),
    ("jle",     le_cond),
    ("jg",      gt_cond),
    ("jc",      ltu_cond),
    ("jnc",     geu_cond),
end

global tabledata []ichar setccnames, []byte setcccodes =
    ("seto",    ov_cond),
    ("setno",   nov_cond),
    ("setb",    ltu_cond),
    ("setae",   geu_cond),
    ("setz",    eq_cond),
    ("setnz",   ne_cond),
    ("setbe",   leu_cond),
    ("seta",    gtu_cond),
    ("sets",    s_cond),
    ("setns",   ns_cond),
    ("setp",    p_cond),
    ("setnp",   np_cond),
    ("setl",    lt_cond),
    ("setge",   ge_cond),
    ("setle",   le_cond),
    ("setg",    gt_cond),
    ("setc",    ltu_cond),
    ("setnc",   geu_cond),
end

global tabledata []ichar cmovccnames, []byte cmovcccodes =
    ("cmovo",   ov_cond),
    ("cmovno",  nov_cond),
    ("cmovb",   ltu_cond),
    ("cmovae",  geu_cond),
    ("cmovz",   eq_cond),
    ("cmovnz",  ne_cond),
    ("cmovbe",  leu_cond),
    ("cmova",   gtu_cond),
    ("cmovs",   s_cond),
    ("cmovns",  ns_cond),
    ("cmovp",   p_cond),
    ("cmovnp",  np_cond),
    ("cmovl",   lt_cond),
    ("cmovge",  ge_cond),
    ("cmovle",  le_cond),
    ("cmovg",   gt_cond),
    ("cmovc",   ltu_cond),
    ("cmovnc",  geu_cond),
end

global tabledata []ichar prefixnames, []byte prefixsizes =
    ("byte",    1),     
    ("word",    2),
    ("word16",  2),
    ("word32",  4),
    ("dword",   4),
    ("word64",  8),
    ("qword",   8),
    ("tword",   10),
    ("word80",  10),
    ("word128", 16)
end

global enumdata [0:]ichar reftypenames =    !use during pass2
    (extern_ref=0,      $),     !is external
    (fwd_ref,           $),     !not yet reached
    (back_ref,          $),     !has been reached
end
