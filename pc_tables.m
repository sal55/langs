import* pc_core

!type system

global tabledata()  [0:]ichar pstdnames,
                    [0:]byte pstdbits, [0:]byte pstdcat =
!    type         name    bits    cat
    (tpvoid=0,    "void",    0,   void_cat),

    (tpu8,        "u8",      8,   short_cat),
    (tpu16,       "u16",    16,   short_cat),
    (tpu32,       "u32",    32,   short_cat),
    (tpu64,       "u64",    64,   d64_cat),
    (tpu128,      "u128",  128,   wide_cat),

    (tpi8,        "i8",      8,   short_cat),
    (tpi16,       "i16",    16,   short_cat),
    (tpi32,       "i32",    32,   short_cat),
    (tpi64,       "i64",    64,   d64_cat),
    (tpi128,      "i128",  128,   wide_cat),

    (tpr32,       "r32",    32,   x32_cat),
    (tpr64,       "r64",    64,   x64_cat),

    (tpblock,     "block",   0,   block_cat),

end

global tabledata() [0:]ichar typecatnames =
    (void_cat=0,    "void"),
    (short_cat,     "short"),       !u8/u16/u32 i8/i16/i64 normally widened to u64/i64
    (d64_cat,       "d64"),         !i64, u64, pointers, r64 as data; anything of that size
    (x32_cat,       "x32"),         !r32, which are not normally widened to r64
    (x64_cat,       "x64"),         !r64
    (wide_cat,      "wide"),        !u128/i128, also used for slices, anything of that size
    (block_cat,     "block"),       !N-byte block of any size, that is not 1/2/4/8/16 bytes
end

global tabledata() [0:]ichar opndnames =
    (no_opnd=0,         $),
    (mem_opnd,          $),
    (memaddr_opnd,      $),
    (label_opnd,        $),
    (int_opnd,          $),
    (real_opnd,         $),
    (int128_opnd,       $),
    (real32_opnd,       $),
    (string_opnd,       $),
    (istring_opnd,      $),
    (metastring_opnd,   $),
end

!Stack operands are:
!   Xa              1st of 1
!   Xb, Ya          1st/2nd of 2
!   Xc, Yb, Za      1st/2nd/3rd of 3
!   Xd, Yc, Zb, Wa  1st/2nd/3rd/4th of 4
! X is always the 'left-most' operand, but will be at offset 0 1 2 3 from top of stack
! a (as in Xa, Ya, Za, Wa) is always the top of stack

!Immediate operand:
!   A           (various)
!Extra info:
!   op          opindex
!   cc          cond code
!   t[:size]    type (:size for block types)
!   u           secondary type for some ops (convert etc)
!   n           nargs for calls
!   s x         scale and offset for ptr/offset ops
!   x y         min/max lab index for switch
!   B           Secondary operand in a following kopnd instruction
!   C           Tertiary operand in a following kopnd instruction

!Stack usage is represented by (a b):
! a is the number of stack elements that will be popped
! b is the number of new stack elements that will be pushed
! Something like (1 1) can mean the same element stays in place


global tabledata() []ichar pclnames,
            []byte pclhasopnd,          !1+ has operand; 2=name defines a name; 3=defines local/param
            []byte pclhastype,
            []byte pclextra =
!                          Op T X
    (knop,              $,  0,0,0), ! (0 0)
    (kstop,             $,  0,0,0), ! (1 0) Stop Xa
    (kcomment,          $,  1,0,0), ! (0 0) Comment A (a string)

    (kimportdll,        $,  1,0,0), ! (0 0) A Use external dll A
!   (kimport,           $,  1,0,0), ! (0 0) A Import symbol A
!   (kexport,           $,  1,0,0), ! (0 0) A Export symbol A (or declare using ::)
    (kistatic,          $,  2,1,0), ! (0 0) (A,t) Define idata label (must be followed by correct kdata ops)
    (kzstatic,          $,  2,1,0), ! (0 0) (A,t) Define zdata labe and reserve sufficient space

    (kprocdef,          $,  2,1,0), ! (0 0) (A,t) Define proc A, of given return type; g=1 if exported
    (kprocentry,        $,  0,0,0), ! (0 0)
    (kend,              $,  0,0,0), ! (0 0)

    (klocal,            $,  3,1,0), ! (0 0) (A,t) Define local A of type t
    (kparam,            $,  3,1,0), ! (0 0) (A,t) Define param A of type t
    (klabel,            $,  1,0,0), ! (0 0) (L,g) Define name L as a local; g=1 if exported (uses "::" in pcs)
    (klabelname,        $,  2,0,0), ! (0 0) (A)

    (kpush,             $,  1,1,0), ! (0 1) (X,t)   Push operand X of type t; X is anything pushable
    (kpop,              $,  1,1,0), ! (1 0) (L,t)   pop to label X
    (kstore,            $,  1,1,0), ! (1 1) (L,t)   store to label X but stays on the stack

    (kopnd,             $,  1,1,0), ! (0 0) (X,t) Define auxialiary operand X (not sure about extra stuff yet)

    (kpushptroff,       $,  0,1,2), ! (2 1) Xa:=(Xb+Ya*scale+offset)^ using given type
    (kpopptroff,        $,  0,1,2), ! (3 0) (Yb*scale+Za+offset)^:=Xc
    (kstoreptroff,      $,  0,1,2), ! (3 1) (Yb*scale+Za+offset)^:=Xc, Xc stays as Xa
!   (kindex,            $,  0,0,0), ! (0 0)
!   (kpopindex,         $,  0,0,0), ! (0 0)
!   (kstoreindex,       $,  0,0,0), ! (0 0)

    (kpushptr,          $,  0,1,0), ! (1 1) Xa:=Xa^
    (kpopptr,           $,  0,1,0), ! (2 0) Ya^:=Xb
    (kstoreptr,         $,  0,1,0), ! (2 1) Ya^:=Xb, keep Xb on stack as Xa

    (kdotindex,         $,  0,1,0), ! (2 1) Xa:=Xb.[Ya] Assume ti64
    (kpopdotindex,      $,  0,1,0), ! (3 0) Yb^.[Za]:=Xc
    (kstoredotindex,    $,  0,1,0), ! (3 1) Yb^.[Za]:=Xc, keep Xc as Xa

    (kdotslice,         $,  0,1,0), ! (3 1) Xa:=Xc.[Yb..Za]
    (kpopdotslice,      $,  0,1,0), ! (4 0) Yc^.[Zb..Wa]:=Xd
    (kstoredotslice,    $,  0,1,0), ! (4 1) Yc^.[Zb..Wa]:=Xd, keep

    (kfree,             $,  0,1,0), ! (1 0) Pop Xa
    (keval,             $,  0,1,0), ! (1 0) Evaluate Xa [load to an actual register], then pop

    (kcallproc,         $,  1,0,1), ! (n 0) (A,nargs) Call &A with nargs, then pop args
    (kcallprocptr,      $,  0,0,1), ! (n+1 0) (nargs) Call Xa with nargs, then pop args
    (kretproc,          $,  0,0,0), ! (0 0) Return from proc

    (kcallfn,           $,  1,0,1), ! (n 1) (A, nargs), Call &A, then pop args, leave retval
    (kcallfnptr,        $,  0,0,1), ! (n+1 1) (nargs) Call Xa, then pops args, leave retval
    (kretfn,            $,  0,0,0), ! (0 0) Return from function with Xa=retval

    (kjump,             $,  1,0,0), ! (0 0) (L) goto L
    (kjumpptr,          $,  0,0,0), ! (1 0) goto Xa

    (kjumpeq,           $,  1,1,0), ! (2 0) (L,t) goto L when Xb = Ya
    (kjumpne,           $,  1,1,0), ! (2 0) (L,t) goto L when <>
    (kjumplt,           $,  1,1,0), ! (2 0) (L,t) goto L when Xb < Ya
    (kjumple,           $,  1,1,0), ! (2 0) (L,t) goto L when <=
    (kjumpge,           $,  1,1,0), ! (2 0) (L,t) goto L when >=
    (kjumpgt,           $,  1,1,0), ! (2 0) (L) goto L when >

    (kjumptrue,         $,  1,1,0), ! (1 0) (L,t) goto L when Xa is true
    (kjumpfalse,        $,  1,1,0), ! (1 0) (L,t) goto L when Xa is false

    (kjumpinrange,      $,  1,1,0), ! (3 0) (L,t) goto L when Xc in Yb..Za
    (kjumpnotinrange,   $,  1,1,0), ! (3 0) (L,t) goto L when Xc not in Yb..Za

    (ksetjumpeq,        $,  1,1,0), ! (2 1) (L,t) goto L when Xb=Ya; pop Y, leave Xa
    (ksetjumpeqx,       $,  1,1,0), ! (0 0) (L,t) goto L when Xb=Ya; pop both
    (ksetjumpne,        $,  1,1,0), ! (0 0) (L,t) goto L when Xb<>Ya; pop both

    (ksetcc,            $,  0,1,1), ! (2 1) (t,cc) Xa:=Xb cc Ya

    (kcasejumpeq,       $,  1,1,1), ! (2 1) (L,t) goto L when Xb=Ya; pop Ya, leave Xa

    (kselectcc,         $,  0,1,1), ! (4 1) (t,cc) Xa:=(Zb op Wa|Xd|Yc)
    (kselecttrue,       $,  0,2,0), ! (3 1) (t) Xa:=(Za|Xc|Yb)

    (kto,               $,  1,0,0), ! (0 0) (L)(B,t) --B (aux); goto L when B<>0 

    (kforup,            $,  1,0,1), ! (0 0) (L,n)(B,t)(C,t) B+:=n; goto L when B<=C
    (kfordown,          $,  1,0,1), ! (0 0) (L,n)(B,t)(C,t) B-:=n; goto L when B>=C

    (kswap,             $,  0,1,1), ! (1 0) (t) swap(Xb^,Yb^)

    (kmakeslice,        $,  0,1,0), ! (2 1) (t) Xa:=slice(Xb, Ya)

    (kswitch,           $,  1,0,2), ! (1 0) (L,x,y)(B) L=jumptab; B=elselab; x/y=min/max values
    (kswitchlabel,      $,  1,0,0), ! (0 0) (L) jumptable entry
    (kendswitch,        $,  0,0,0), ! (0 0) Mark end of switch jumptable

    (kclear,            $,  0,1,0), ! (1 0) (t) Clear Xa^

    (kcsegment,         $,  0,0,0), ! (0 0) Switch to that segment (usually automatic, so these override)
    (kisegment,         $,  0,0,0), ! (0 0) ..
    (kzsegment,         $,  0,0,0), ! (0 0) ..
    (krosegment,        $,  0,0,0), ! (0 0) ..

    (kdata,             $,  1,1,0), ! (0 0) (t) X define inline data of various kinds
    (kreserve,          $,  0,1,0), ! (0 0) (t) Reserve space big enough for t
    (kassem,            $,  0,0,0), ! (0 0) to be worked out....

    (kadd,              $,  0,1,0), ! (2 1) (t) Xa := Xb + Ya
    (ksub,              $,  0,1,0), ! (2 1) (t)
    (kmul,              $,  0,1,0), ! (2 1) (t)
    (kdiv,              $,  0,1,0), ! (2 1) (t)
    (kidiv,             $,  0,1,0), ! (2 1) (t)
    (kirem,             $,  0,1,0), ! (2 1) (t)
    (kiand,             $,  0,1,0), ! (2 1) (t)
    (kior,              $,  0,1,0), ! (2 1) (t)
    (kixor,             $,  0,1,0), ! (2 1) (t)
    (kshl,              $,  0,1,0), ! (2 1) (t)
    (kshr,              $,  0,1,0), ! (2 1) (t)
    (kin,               $,  0,1,0), ! (2 1) (t)
    (knotin,            $,  0,1,0), ! (2 1) (t)
    (kmin,              $,  0,1,0), ! (2 1) (t)
    (kmax,              $,  0,1,0), ! (2 1) (t)
    (keq,               $,  0,1,0), ! (2 1) (t)
    (kne,               $,  0,1,0), ! (2 1) (t)
    (klt,               $,  0,1,0), ! (2 1) (t)
    (kle,               $,  0,1,0), ! (2 1) (t)
    (kge,               $,  0,1,0), ! (2 1) (t)
    (kgt,               $,  0,1,0), ! (2 1) (t)
    (ksame,             $,  0,1,0), ! (2 1) (t)
    (kandl,             $,  0,1,0), ! (2 1) (t)
    (korl,              $,  0,1,0), ! (2 1) (t)
    (kaddrefoff,        $,  0,1,0), ! (2 1) (t) Xa := Xb + Ya
    (ksubrefoff,        $,  0,1,0), ! (2 1) (t)

    (kneg,              $,  0,1,0), ! (1 1) (t) Xa:=-Xa
    (kabs,              $,  0,1,0), ! (1 1) (t)
    (kinot,             $,  0,1,0), ! (1 1) (t)
    (knotl,             $,  0,1,0), ! (1 1) (t)
    (kistruel,          $,  0,1,0), ! (1 1) (t)
    (ksqr,              $,  0,1,0), ! (1 1) (t)

    (ksqrt,             $,  0,1,0), ! (1 1) (t) Xa:=sqrt(Xa)
    (ksin,              $,  0,1,0), ! (1 1) (t)
    (kcos,              $,  0,1,0), ! (1 1) (t)
    (ktan,              $,  0,1,0), ! (1 1) (t)
    (kasin,             $,  0,1,0), ! (1 1) (t)
    (kacos,             $,  0,1,0), ! (1 1) (t)
    (katan,             $,  0,1,0), ! (1 1) (t)
    (kln,               $,  0,1,0), ! (1 1) (t)
    (klog,              $,  0,1,0), ! (1 1) (t)
    (kexp,              $,  0,1,0), ! (1 1) (t)
    (kround,            $,  0,1,0), ! (1 1) (t)
    (kfloor,            $,  0,1,0), ! (1 1) (t)
    (kceil,             $,  0,1,0), ! (1 1) (t)
    (kfract,            $,  0,1,0), ! (1 1) (t)
    (ksign,             $,  0,1,0), ! (1 1) (t)
    (katan2,            $,  0,1,0), ! (1 1) (t)
    (kpower,            $,  0,1,0), ! (1 1) (t)
    (kfmod,             $,  0,1,0), ! (1 1) (t)

    (kincr,             $,  0,1,0), ! (1 0) (t) ++Xa^
    (kdecr,             $,  0,1,0), ! (1 0) (t) --Xa^
    (kincrload,         $,  0,1,0), ! (1 1) (t) Xa:=(++Xa)^
    (kdecrload,         $,  0,1,0), ! (1 1) (t) Xa:=(--Xa)^
    (kloadincr,         $,  0,1,0), ! (1 1) (t) Xa:=Xa++^
    (kloaddecr,         $,  0,1,0), ! (1 1) (t) Xa:=Xa--^

    (kaddto,            $,  0,1,0), ! (2 0) (t) Xa^ +:= Ya
    (ksubto,            $,  0,1,0), ! (2 0) (t)
    (kmulto,            $,  0,1,0), ! (2 0) (t)
    (kdivto,            $,  0,1,0), ! (2 0) (t)
    (kidivto,           $,  0,1,0), ! (2 0) (t)
    (kiremto,           $,  0,1,0), ! (2 0) (t)
    (kiandto,           $,  0,1,0), ! (2 0) (t)
    (kiorto,            $,  0,1,0), ! (2 0) (t)
    (kixorto,           $,  0,1,0), ! (2 0) (t)
    (kshlto,            $,  0,1,0), ! (2 0) (t)
    (kshrto,            $,  0,1,0), ! (2 0) (t)
    (kminto,            $,  0,1,0), ! (2 0) (t)
    (kmaxto,            $,  0,1,0), ! (2 0) (t)
    (kandlto,           $,  0,1,0), ! (2 0) (t)
    (korlto,            $,  0,1,0), ! (2 0) (t)
    (kaddrefoffto,      $,  0,1,0), ! (2 0) (t) (t) Xa^ +:= Ya
    (ksubrefoffto,      $,  0,1,0), ! (2 0) (t)

    (knegto,            $,  0,1,0), ! (1 0) (t) -:=Xa^
    (kabsto,            $,  0,1,0), ! (1 0) (t)
    (kinotto,           $,  0,1,0), ! (1 0) (t)
    (knotlto,           $,  0,1,0), ! (1 0) (t)
    (kistruelto,        $,  0,1,0), ! (1 0) (t)

    (kconvert,          $,  0,2,1), ! (1 1) (t,u)
    (ktypepun,          $,  0,2,1), ! (1 1) (t,u)

!   (ksoftconv,         $,  0,2,1), ! (1 1) (t,u) Xa:=cast(Xa,u) ??
    (kwiden,            $,  0,2,1), ! (1 1) (t,u) Xa:=cast(Xa,u) Widen int type, from t to wider int u
    (knarrow,           $,  0,2,1), ! (1 1) (t,u) Xa:=cast(Xa,u) I think reduces i128/u128 t to i64/u64 u
    (kfloat,            $,  0,2,1), ! (1 1) (t,u) Xa:=cast(Xa,u) Int t to real u
    (kfix,              $,  0,2,1), ! (1 1) (t,u) Xa:=cast(Xa,u) Real t to int u
!   (ksofttrunc,        $,  0,2,1), ! (1 1) (t,u) Xa:=cast(Xa,u) I think used for i128/u128 t to u64/i64 u
    (ktruncate,         $,  0,2,1), ! (1 1) (t,u) Xa:=cast(Xa,u) Mask to width of u, but type remains as t
    (kfwiden,           $,  0,2,1), ! (1 1) (t,u) Xa:=cast(Xa,u) r32 to r64
    (kfnarrow,          $,  0,2,1), ! (1 1) (t,u) Xa:=cast(Xa,u) r64 to r32

!ops used internally by M compiler until they can be replaced
!(usually they will be turned into something else, constants etc)
    (klen,              $,  0,0,0),
    (klwb,              $,  0,0,0),
    (kupb,              $,  0,0,0),
    (kbounds,           $,  0,0,0),
    (klenstr,           $,  0,0,0),
    (kbitwidth,         $,  0,0,0),
    (kbytesize,         $,  0,0,0),
    (kbytes,            $,  0,0,0),
    (kminvalue,         $,  0,0,0),
    (kmaxvalue,         $,  0,0,0),
    (ktypestr,          $,  0,0,0),

    (klast,             $,  0,0,0), ! (0 0)
end
