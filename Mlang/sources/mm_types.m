global enumdata  [0:]ichar stdnames,
        [0:]byte stdbits,
        [0:]byte stdcat =
!    type        name         bits  code    cat
    (tvoid=0,     "void",        0,   voidcat),

    (tc64,        "c64",        64,   d64cat),
    (tu64,        "u64",        64,   d64cat),
    (ti64,        "i64",        64,   d64cat),
    (tr32,        "r32",        32,   x32cat),
    (tr64,        "r64",        64,   x64cat),

    (tbool64,     "bool64",     64,   d64cat),
    (tref,        "ref",        64,   d64cat),

    (trecord,     "rec",         0,   blockcat),
    (trange,      "range",     128,   blockcat),

    (tarray,      "array",       0,   blockcat),
    (tslice,      "slice",     128,   blockcat),

    (tc8,         "c8",          8,   shortcat),
    (tbool8,      "b8",          8,   shortcat),
    (ti8,         "i8",          8,   shortcat),
    (ti16,        "i16",        16,   shortcat),
    (ti32,        "i32",        32,   shortcat),
    (tu8,         "u8",          8,   shortcat),
    (tu16,        "u16",        16,   shortcat),
    (tu32,        "u32",        32,   shortcat),

    (tu1,         "u1",          1,   bitcat),
    (tu2,         "u2",          2,   bitcat),
    (tu4,         "u4",          3,   bitcat),

    (trefchar,    "ichar",      64,   d64cat),
    (trefbit,     "refbit",    128,   blockcat),

    (tauto,       "auto",        0,   voidcat),
    (tany,        "any",         0,   voidcat),
    (tproc,       "proc",        0,   voidcat),
    (tlabel,      "label",       0,   voidcat),
    (ttype,       "type",       64,   voidcat),
    (tbitfield,   "bitfl",       8,   voidcat),
    (ttuple,      "tuple",       0,   voidcat),
    (tpending,    "pend",        0,   voidcat),

    (tlast,       "last ",       0,   voidcat),
end

global enumdata [0:]ichar catnames =
    (voidcat=0,     $),         ! Not set

    (d64cat,        $),         ! Any 64-bit value other than x64, including pointers
    (x32cat,        $),         ! 32-bit float
    (x64cat,        $),         ! 64-bit float when can't be treated as d64

    (shortcat,      $),         ! 8/16/32-bit types, maybe zero/sign-extended to d64
    (bitcat,        $),
    (blockcat,      $),         ! 64-bit pointer to block data
end

global const tuser	= tlast

global const tint	= ti64
global const tword	= tu64
global const treal	= tr64
global const tbool	= tbool64

global const tfirstnum	= tc64
global const tlastnum	= tr64

global const tfirstshort	= tc8
global const tlastshort		= tu32

global const maxtuplesize = 4

global int trefproc
global int treflabel
