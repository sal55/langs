global tabledata()  [0:]ichar stdnames,
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

    (tenum,      "enum",   64,     0,  ti64,       ti64,       tu64,       d64_cat,    d64_cat  ),
    (trecordx,   "recx",   64,     0,  trecordx,   tvar,       trecordx,   var_cat,    var_cat  ),

    (tu1,        "u1",      1,   'B',  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (tu2,        "u2",      2,   'B',  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (tu4,        "u4",      4,   'B',  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),

    (tauto,      "auto",    0,     0,  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (tany,       "any",     0,     0,  tvoid,      tvoid,      tvoid,      void_cat,   void_cat ),
    (tproc,      "proc",    0,     0,  tvoid,      tvoid,      tu64,       void_cat,   void_cat ),
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
  
global tabledata() [0:]ichar typecatnames =
    (void_cat=0,    "void"),
    (short_cat,     "short"),       !u8/u16/u32 i8/i16/i64 normally widened to u64/i64
    (d64_cat,       "d64"),         !i64, u64, pointers, r64 as data; anything of that size
    (x32_cat,       "x32"),         !r32, which are not normally widened to r64
    (x64_cat,       "x64"),         !r64
    (wide_cat,      "wide"),        !u128/i128, also used for slices, anything of that size
    (block_cat,     "block"),       !N-byte block of any size, that is not 1/2/4/8/16 bytes
    (var_cat,       "var"),         !u64 pointer/refernce to flex string/array etc
end
