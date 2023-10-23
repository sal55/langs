!Z80 Assembler tables

global enumdata mclnames, mclcodes =
    (m_nop,     $,  0),
    (m_ld,      $,  0),
    (m_push,    $,  0),
    (m_pop,     $,  0),
    (m_jr,      $,  0),
    (m_djnz,    $,  0),
    (m_inc,     $,  0),
    (m_dec,     $,  0),
    (m_jp,      $,  0),
    (m_call,    $,  0),
    (m_ret,     $,  0),
    (m_rst,     $,  0),
    (m_syscall, $,  0),
    (m_halt,    $,  0),

    (m_add,     $,  000B),
    (m_adc,     $,  001B),
    (m_sub,     $,  010B),
    (m_sbc,     $,  011B),
    (m_and,     $,  100B),
    (m_xor,     $,  101B),
    (m_or,      $,  110B),
    (m_cmp,     $,  111B),

    (m_neg,     $,  0),
    (m_not,     $,  0),

    (m_shl,     $,  100B),
    (m_sar,     $,  101B),
    (m_shr,     $,  111B),
    (m_rol,     $,  000B),
    (m_rcl,     $,  010B),
    (m_ror,     $,  001B),
    (m_rcr,     $,  011B),

    (m_bit,     $,  0),
    (m_set,     $,  0),
    (m_res,     $,  0),

    (m_rrd,     $,  0),
    (m_rrl,     $,  0),

    (m_in,      $,  0),
    (m_out,     $,  0),
    (m_ldd,     $,  0),
    (m_lddr,    $,  0),
    (m_ldi,     $,  0),
    (m_ldir,    $,  0),
    (m_ex,      $,  0),
    (m_exx,     $,  0),
    (m_di,      $,  0),
    (m_ci,      $,  0),
    (m_scf,     $,  0),
    (m_ccf,     $,  0),
    (m_cpi,     $,  0),
    (m_cpir,    $,  0),
    (m_cpd,     $,  0),
    (m_cpdr,    $,  0),

    (m_label,   $,  0),
    (m_db,      $,  0),
    (m_dw,      $,  0),
    (m_resb,    $,  0),
    (m_align,   $,  0),
    (m_base,    $,  0),

    (m_comment, $,  0),
    (m_blank,   $,  0),
    (m_test,    $,  0),
    (m_end,     $,  0),
end

global enumdata regnames, regsyms, regcodes =
    (reg_a,     "a",    reg8sym,    7),
    (reg_b,     "b",    reg8sym,    0),
    (reg_c,     "c",    reg8sym,    1),
    (reg_d,     "d",    reg8sym,    2),
    (reg_e,     "e",    reg8sym,    3),
    (reg_h,     "h",    reg8sym,    4),
    (reg_l,     "l",    reg8sym,    5),
    (reg_bc,    "bc",   reg16sym,   0),
    (reg_de,    "de",   reg16sym,   0),
    (reg_hl,    "hl",   reg16sym,   0),
    (reg_sp,    "sp",   reg16sym,   0),
    (reg_ix,    "ix",   regixsym,   0),
    (reg_iy,    "iy",   regixsym,   0),
    (reg_f,     "f",    regmiscsym, 0),
    (reg_af,    "af",   regmiscsym, 0),
    (reg_int,   "intreg",   regmiscsym, 0), ! interrupt
    (reg_ref,   "refresh",  regmiscsym, 0), ! refresh
end

global var regnamex:=(0: "b", "c", "d", "e", "h", "l", "(hl)", "a")

global var shiftnames:=(0: "rol", "ror", "rcl", "rcr", "shl", "sar", "-", "shr")

global var arithnames:=(0: "add", "adc", "sub", "sbc", "and", "xor", "or", "cmp")

global enumdata condnames, condcodes =
    (un_cond=0, "-",    0),
    (nz_cond,   "nz",   1),     ! not zero          ZERO=0
    (nc_cond,   "nc",   2),     ! no carry          CARRY=0
    (po_cond,   "po",   3),     ! parity odd        (P/V=0)
    (p_cond,    "p",    4),     ! positive          SIGN=0

    (z_cond,    "z",    5),     ! zero flag set     ZERO=1
    (cy_cond,   "cy",   6),     ! carray flag set   CARRY=1
    (pe_cond,   "pe",   7),     ! parity even       P/V=1
    (m_cond,    "m",    8),     ! minus             SIGN=1
end

global const no_cond = po_cond  ! no overflow       P/V=0
global const ov_cond = pe_cond  ! signed overflow   P/V=1

global enumdata symbolnames =
    (errorsym,          $),
    (commasym,          $),
    (colonsym,          $),
    (lsqsym,            $),
    (rsqsym,            $),
    (addsym,            $),
    (subsym,            $),
    (mulsym,            $),
    (divsym,            $),
    (eqsym,             $),
    (eolsym,            $),
    (eofsym,            $),
    (hashsym,           $),
    (intconstsym,       $),
    (realconstsym,      $),
    (charconstsym,      $),
    (stringconstsym,    $),
    (namesym,           $),
    (forwardsym,        $),
    (labelsym,          $),
    (namedconstsym,     $),
!   (fwdsym,            $),
!   (localsym,          $),
    (opcodesym,         $),
    (reg8sym,           $),
    (reg16sym,          $),
    (regixsym,          $),
    (regmiscsym,        $),
    (condcodesym,       $),
    (syscallsym,        $),
end

global enumdata opndnames =
    (no_opnd=0,     $),
    (reg8_opnd,     $),     ! A B C D E H L (opndreg)
    (reg16_opnd,    $),     ! BC DE HL SP (opndreg)
    (regix_opnd,    $),     ! IX IY (opndreg)
    (regmisc_opnd,  $),     ! F AF INT REF (opndreg)
    (ireg_opnd,     $),     ! [BC] [DE] [HL] (opndreg)
    (iregix_opnd,   $),     ! [IX+D] [IY+D] (opndreg, opndvalue)
    (imm_opnd,      $),     ! Name, N, Name+N (opndst, opndvalue, or both)
    (mem_opnd,      $),     ! [Name], [N], [Name+N] (opndst, opndvalue, or both)
end

global enumdata syscallnames =
    (sys_printstr,  $),
    (sys_printi16,  $),
    (sys_printu16,  $),
    (sys_println,   $),
    (sys_showregs,  $),
end
