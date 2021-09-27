import* pci_core

!type system

export tabledata()  [0:]ichar pstdnames,
        [0:]byte psize, [0:]byte psigned, [0:]byte pfloat, [0:]byte pcat =
!    type         name    bits SZ   S F Cat
    (tpvoid=0,    "void",       0,  0,0, voidcat),

    (tpu8,        "u8",         1,  0,0, shortcat),
    (tpu16,       "u16",        2,  0,0, shortcat),
    (tpu32,       "u32",        4,  0,0, shortcat),
    (tpu64,       "u64",        8,  0,0, d64cat),
    (tpu128,      "u128",       16, 0,0, widecat),

    (tpi8,        "i8",         1,  1,0, shortcat),
    (tpi16,       "i16",        2,  1,0, shortcat),
    (tpi32,       "i32",        4,  1,0, shortcat),
    (tpi64,       "i64",        8,  1,0, d64cat),
    (tpi128,      "i128",       16, 1,0, widecat),

    (tpr32,       "r32",        4,  0,1, x32cat),
    (tpr64,       "r64",        8,  0,1, x64cat),

    (tpblock,     "block",      0,  0,0, blockcat),

    (tplast,      "$last",      0,  0,0, voidcat),
end

global tabledata() [0:]ichar catnames =
    (voidcat=0,     $),
    (d64cat,        $),
    (x32cat,        $),
    (x64cat,        $),
    (shortcat,      $),
    (widecat,       $),
    (blockcat,      $),
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
end

!Stack operands are:
!   Xa              1st of 1
!   Xb, Ya          1st/2nd of 2
!   Xc, Yb, Za      1st/2nd/3rd of 3
!   Xd, Yc, Zb, Wa  1st/2nd/3rd/4th of 4
! X is always the 'left-most' operand, but will be at offset 0, 1 2 from top of stack
! a (as in Xa, Ya, Za, Wa) is always the top of stack

!Immediate operand:
!   A           (various)
!   B           Secondary operand in a following kopnd instruction
!   C           Tertiary operand in a following kopnd instruction

!Stack usage is represented by (a b):
! a is the number of stack elements that will be popped
! b is the number of new stack elements that will be pushed
! Something like (1 1) can mean the same element stays in place

export tabledata() [0:]ichar pclnames,
            [0:]byte pclhasopnd,            !1+ has operand; 2=name defines a name; 3=defines local/param; 4=imported func
            [0:]byte pclhastype,
            [0:]byte pclextra =

!                          Op T X
    (kzero=0,           $,  0,0,0), ! (0 0)
    (knop,              $,  0,0,0), ! (0 0)
    (kstop,             $,  0,0,0), ! (1 0) Stop Xa
    (kcomment,          $,  1,0,0), ! (0 0) Comment A (a string)

    (kimportdll,        $,  1,0,0), ! (0 0) A Use external dll A
    (kistatic,          $,  2,1,0), ! (0 0) (A,t) Define idata label (must be followed by correct kdata ops)
    (kzstatic,          $,  2,1,0), ! (0 0) (A,t) Define zdata labe and reserve sufficient space
    (kequiv,            $,  2,0,0), ! (0 0) (A) Define equivalence to this var?

    (kprocdef,          $,  2,1,0), ! (0 0) (A,t) Define proc A, of given return type
    (kprocentry,        $,  0,0,0), ! (0 0)
    (kendproc,          $,  0,0,0), ! (0 0)
    (kendprogram,       $,  0,0,0), ! (0 0)
    (kextproc,          $,  4,1,0), ! (0 0) (t)
    (kextparam,         $,  0,1,0), ! (0 0) (t) Anonymous params
    (kextvariadics,     $,  0,1,0), ! (0 0) (t) ... parameter
    (kendextproc,       $,  0,0,0), ! (0 0)
    (kprocrts,          $,  2,1,0), ! (0 0) (A,t) Alternate to procdef but for rts functions

    (klocal,            $,  3,1,0), ! (0 0) (A,t) Define local A of type t
    (kparam,            $,  3,1,0), ! (0 0) (A,t) Define param A of type t
    (klabel,            $,  1,0,0), ! (0 0) (L) Define numbered label L
    (klabelname,        $,  2,0,0), ! (0 0) (A) Define named label

    (kpush,             $,  1,1,0), ! (0 1) (X,t)   Push operand X of type t; X is anything pushable
    (kpop,              $,  1,1,0), ! (1 0) (L,t)   pop to label X
    (kstore,            $,  1,1,0), ! (1 1) (L,t)   store to label X but stays on the stack
    (kpushnc,           $,  1,1,0), ! (0 1) (X,t)   Push optimised for blocks (no copying)
    (kpushlabel,        $,  1,0,0), ! (0 1) (L)     Push address of label L

    (kopnd,             $,  1,0,0), ! (0 0) (X) Define auxiliary operand X (not sure about extra stuff yet)
    (ktype,             $,  0,1,0), ! (0 0) (t) Define auxiliary type t
    (kduplstack,        $,  0,1,0), ! (1 2) (t) Ya':=Xa; X stays on stack
    (kswapstack,        $,  0,1,1), ! (1 1) (t,N) Swap Xa with element +N away

    (kpushptroff,       $,  0,1,2), ! (2 1) (t,scale,offset) Xa:=(Xb+Ya*scale+offset)^ using given type
    (kpopptroff,        $,  0,1,2), ! (3 0) (t,scale,offset) (Yb+Za*scale+offset)^:=Xc
    (kstoreptroff,      $,  0,1,2), ! (3 1) (t,scale,offset) (Yb+Za*scale+offset)^:=Xc, Xc stays as Xa

    (kpushptr,          $,  0,1,0), ! (1 1) Xa:=Xa^
    (kpopptr,           $,  0,1,0), ! (2 0) Ya^:=Xb
    (kstoreptr,         $,  0,1,0), ! (2 1) Ya^:=Xb, keep Xb on stack as Xa

    (kdotindex,         $,  0,1,0), ! (2 1) Xa:=Xb.[Ya]
    (kpopdotindex,      $,  0,1,0), ! (3 0) Yb^.[Za]:=Xc
    (kstoredotindex,    $,  0,1,0), ! (3 1) Yb^.[Za]:=Xc, keep Xc as Xa

    (kdotslice,         $,  0,1,0), ! (3 1) Xa:=Xc.[Yb..Za]
    (kpopdotslice,      $,  0,1,0), ! (4 0) Yc^.[Zb..Wa]:=Xd
    (kstoredotslice,    $,  0,1,0), ! (4 1) Yc^.[Zb..Wa]:=Xd, keep

    (kpopstack,         $,  0,1,0), ! (1 0) Pop Xa
    (keval,             $,  0,1,0), ! (1 0) Evaluate Xa [load to an actual register], then pop

    (kcallproc,         $,  1,0,0), ! (n 0) (A) Call &A with nargs, then pop args
    (kcallprocptr,      $,  0,0,0), ! (n+1 0) Call Xa with nargs, then pop args
    (kretproc,          $,  0,0,0), ! (0 0) Return from proc

    (kcallfn,           $,  1,1,0), ! (n 1) (A, t), Call &A, then pop args, leave retval
    (kcallfnptr,        $,  0,1,0), ! (n+1 1) (t) Call Xa, then pops args, leave retval
    (kretfn,            $,  0,1,0), ! (0 0) (t) Return from function with Xa=retval

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

!   (ksetcc,            $,  0,1,1), ! (2 1) (t,cc) Xa:=Xb cc Ya
    (kseteq,            $,  0,1,0), ! (2 1) (t) Xa:=Xb = Ya
    (ksetne,            $,  0,1,0), ! (2 1) (t) Xa:=Xb <> Ya
    (ksetlt,            $,  0,1,0), ! (2 1) (t) Xa:=Xb < Ya
    (ksetle,            $,  0,1,0), ! (2 1) (t) Xa:=Xb <= Ya
    (ksetge,            $,  0,1,0), ! (2 1) (t) Xa:=Xb >= Ya
    (ksetgt,            $,  0,1,0), ! (2 1) (t) Xa:=Xb > Ya

    (kcasejumpeq,       $,  1,1,1), ! (2 1) (L,t) goto L when Xb=Ya; pop Ya, leave Xa

    (kselecteq,         $,  0,1,0), ! (4 1) (t) Xa:=(Zb = Wa|Xd|Yc)
    (kselectne,         $,  0,1,0), ! (4 1) (t) Xa:=(Zb <> Wa|Xd|Yc)
    (kselectlt,         $,  0,1,0), ! (4 1) (t) Xa:=(Zb < Wa|Xd|Yc)
    (kselectle,         $,  0,1,0), ! (4 1) (t) Xa:=(Zb <= Wa|Xd|Yc)
    (kselectge,         $,  0,1,0), ! (4 1) (t) Xa:=(Zb >= Wa|Xd|Yc)
    (kselectgt,         $,  0,1,0), ! (4 1) (t) Xa:=(Zb > Wa|Xd|Yc)

    (kselecttrue,       $,  0,1,0), ! (3 1) (t) Xa:=(Za|Xc|Yb)

    (kto,               $,  1,0,0), ! (0 0) (L)(B,t) --B (aux); goto L when B<>0 

    (kforup,            $,  1,1,1), ! (0 0) (L,t,n)(B,t)(C,t) B+:=n; goto L when B<=C
    (kfordown,          $,  1,1,1), ! (0 0) (L,t,n)(B,t)(C,t) B-:=n; goto L when B>=C

    (kswap,             $,  0,1,0), ! (2 0) (t) swap(Xb^,Yb^)

    (kmakeslice,        $,  0,1,0), ! (2 1) (t) Xa:=slice(Xb, Ya)

    (kswitch,           $,  1,0,2), ! (1 0) (L,x,y)(B) L=jumptab; B=elselab; x/y=min/max values
    (kswitchlabel,      $,  1,0,0), ! (0 0) (L) jumptable entry
    (kendswitch,        $,  0,0,0), ! (0 0) Mark end of switch jumptable

    (kclear,            $,  0,1,0), ! (1 0) (t) Clear Xa^

    (kcsegment,         $,  0,0,0), ! (0 0) Switch to that segment (usually automatic, so these override)
    (kisegment,         $,  0,0,0), ! (0 0) ..
    (kzsegment,         $,  0,0,0), ! (0 0) ..
    (krosegment,        $,  0,0,0), ! (0 0) ..

    (kdata,             $,  1,1,0), ! (0 0) (X,t) Define inline data of various kinds

    (kdb,               $,  1,0,0), ! (0 0) (X) Define a u8 data value
    (kdw,               $,  1,0,0), ! (0 0) (X) u16 value: ...
    (kdd,               $,  1,0,0), ! (0 0) (X) u32 value: u32/i32/r32, depends on operand
    (kdq,               $,  1,0,0), ! (0 0) (X) u64 value: u64/i64/r64/string/addr/label, depends on operan
    (kdstring,          $,  1,0,0), ! (0 0) (S) u8 sequence from string literal (no terminator)
    (kdstringz,         $,  1,0,0), ! (0 0) (S) u8 sequence from string literal, nul added

    (kreserve,          $,  0,1,0), ! (0 0) (t) Reserve space big enough for t
    (kassem,            $,  0,0,0), ! (0 0)     To be worked out....

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
    (kaddrefoff,        $,  0,1,2), ! (2 1) (t,scale,offset) Xa := Xb + Ya*scale + offset
    (ksubrefoff,        $,  0,1,2), ! (2 1) (t,scale,offset) Xa := Xb - Ya*scale + offset
    (ksubref,           $,  0,1,1), ! (2 1) (t,scale) Xa := (Xb - Ya)/scale

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

    (kincr,             $,  0,1,1), ! (1 0) (t,step) Xa^+:=step
    (kdecr,             $,  0,1,1), ! (1 0) (t,step) Xa^-:=step
    (kincrload,         $,  0,1,1), ! (1 1) (t,step) Xa:=(Xa+:=step)^
    (kdecrload,         $,  0,1,1), ! (1 1) (t,step) Xa:=(Xa-:=step)^
    (kloadincr,         $,  0,1,1), ! (1 1) (t,step) Xa:=Xa++^ (difficult to express step)
    (kloaddecr,         $,  0,1,1), ! (1 1) (t,step) Xa:=Xa--^

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
    (kaddrefoffto,      $,  0,1,2), ! (2 0) (t,scale,offset) Xa^ +:= Ya
    (ksubrefoffto,      $,  0,1,2), ! (2 0) (t,scale,offset) Xa^ -:= Ya

    (knegto,            $,  0,1,0), ! (1 0) (t) -:=Xa^
    (kabsto,            $,  0,1,0), ! (1 0) (t)
    (kinotto,           $,  0,1,0), ! (1 0) (t)
    (knotlto,           $,  0,1,0), ! (1 0) (t)
    (kistruelto,        $,  0,1,0), ! (1 0) (t)

    (ktypepun,          $,  0,2,1), ! (1 1) (t,u)
    (ksoftconv,         $,  0,2,0), ! (1 1) (t,u) Xa:=cast(Xa,u) ??

    (kwiden,            $,  0,2,0), ! (1 1) (t,u) Xa:=cast(Xa,u) Widen int type, from t to wider int u
    (knarrow,           $,  0,2,0), ! (1 1) (t,u) Xa:=cast(Xa,u) I think reduces i128/u128 t to i64/u64 u
    (kfloat,            $,  0,2,0), ! (1 1) (t,u) Xa:=cast(Xa,t) Int u to real t
    (kfix,              $,  0,2,0), ! (1 1) (t,u) Xa:=cast(Xa,t) Real u to int t
    (ktruncate,         $,  0,2,0), ! (1 1) (t,u) Xa:=cast(Xa,u) Mask to width of u, but type is widend to i64/u64
    (kfwiden,           $,  0,2,0), ! (1 1) (t,u) Xa:=cast(Xa,u) r32 to r64
    (kfnarrow,          $,  0,2,0), ! (1 1) (t,u) Xa:=cast(Xa,u) r64 to r32
    (ksofttruncw,       $,  0,2,0), ! (1 1) (t,u) Xa:=cast(Xa,u) 128 bits to 64
    (kwidenw,           $,  0,2,0), ! (1 1) (t,u) Xa:=cast(Xa,u) 64 bits to 128

!These ones are currently still needed by some or all PCL targets

    (kstartmult,        $,  0,0,0),
    (kresetmult,        $,  0,0,0),
    (kendmult,          $,  0,0,0),
    (ksetret,           $,  0,1,0), ! (0 0) (t) Set Xa as return value of type t
    (ksetretmult,       $,  0,0,1), ! (0 0) (n) Set N return values
    (ksetargs,          $,  0,0,2), ! (nargs, nvars)

!these are special ones used for reflection

    (kgetnprocs,        $,  0,0,0), ! (0 1) Get number of functions in function table
    (kgetprocname,      $,  0,0,0), ! (1 1) Xa:=Getprocname(Xa) Name of nth function (1-based)
    (kgetprocaddr,      $,  0,0,0), ! (1 1) Xa:=Getprocaddr(Xa) Addr of nth function (1-based)

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
    (kerror,            $,  0,0,0),
    (karraytoslice,     $,  0,0,0),
    (kichartoslice,     $,  0,0,0),
    (ksofttruncshort,   $,  0,0,0),
    (kcharaxtoichar,    $,  0,0,0),
    (ksliceptr,         $,  0,0,0),

    (klast,             $,  0,0,0), ! (0 0)
end

!Some pcl ops are implemented via functions
!A few of these use special pcl instructions in module representing its 'runtime'
!Others may need generating as inline asm code

global tabledata() []ichar rtsnames =
    (rts_rts_unimpl,        $),
    (rts_power_i64,         $),
    (rts_float_u64r64,      $),
    (rts_mul_i128,          $),
    (rts_div_i128,          $),
    (rts_div_u128,          $),
end
