Describes my Three-Address-Code IL ('TCL'), extracted from my compiler.

!-lines are comments. The main 'enumdata' block defines a set of enumerations (kmove etc), and parallel arrays

!tclhastype:
! 0         no type info
! 1         tcl.mode (t)
! 2         tcl.mode & tcl.mode2 (t & u)
! 3         Uses arg types only (eg. CALL)

global enumdata [0:]ichar tclnames,
                [0:]byte tclwrite,          !1/2: known fixed no. of temp dests; 0=not used, or depends on context
                [0:]byte tclhastype,
                [0:]byte tcltempptr =       !1/2/3 means temp in opnd 1/2/both must be T^

!TCL opcodes
! T lvalue          T3
! M lvalue          x, T (x is static/local/proc)
! P lvalue          x, T3^

! a b c d           Rvalues: T3, x, &x, 123 4.56 "ABC" L123
! D                 Data Rvalue: x, &x, 123 4.56 "ABC" L123 <datastr>

! L Label index     Labels
! d symbol          M (ST entry)

!                    Wr Types T^         (a b c)
    (knop=0,    $+1,  0,  0,  0),  !     (- - -)
    (kcomment,  $+1,  0,  0,  0),  !     (a - -)
  
    (kmove,     $+1,  0,  1,  0),  !     (M b -)    M := b
    (keval,     $+1,  0,  1,  0),  !     (a - -)
  
    (kaddpx,    $+1,  1,  1,  0),  ! s,n (T b c)    T := b + c*s + n
    (kiloadx,   $+1,  1,  1,  0),  ! s,n (T b c)    T :=(b + c*s + n)^
    (kistorex,  $+1,  0,  1,  0),  ! s,n (b c r)    (a + b*s + n)^ := c
  
    (kcall,     $+1,  0,  3,  0),  ! r,n (a- - -)   ([T ...] F [r ...]) r=nret, n=nargs
    (kretproc,  $+1,  0,  0,  0),  !     (- - -)    return
    (kretfn,    $+1,  0,  1,  0),  !     (a - -)    return a
    (kretmult,  $+1,  0,  3,  0),  ! n   (a ...)    return n values

    (kjump,     $+1,  0,  0,  0),  !     (L - -)    goto L
    (kjumpcc,   $+1,  0,  1,  0),  ! cc  (L b c)    goto L when b cc c
    (kjumpt,    $+1,  0,  1,  0),  !     (L b -)    goto L when istrue(b)
    (kjumpf,    $+1,  0,  1,  0),  !     (L b -)    goto L when not istrue(b)
    (kijump,    $+1,  0,  1,  0),  !     (a - -)    goto a
    (ksetcc,    $+1,  1,  1,  0),  ! cc  (T b c)    T := b cc c
  
    (kto,       $+1,  0,  1,  0),  !     (L b -)    --b; goto L when b<>0
    (kforup,    $+1,  0,  1,  0),  ! n   (L b c)    b+:=n; goto L when b <= c
    (kfordown,  $+1,  0,  1,  0),  ! n   (L b c)    b-:=n; goto L when b >= c

    (kiswap,    $+1,  0,  1,  3),  !     (P P -)    swap(P, P)
  
    (kadd,      $+1,  1,  1,  0),  !     (T b c)    T := b + c
    (ksub,      $+1,  1,  1,  0),  !     (T b c)    T := b - c
    (kmul,      $+1,  1,  1,  0),  !     (T b c)    T := b * c
    (kdiv,      $+1,  1,  1,  0),  !     (T b c)    T := b / c (float only)
    (kidiv,     $+1,  1,  1,  0),  !     (T b c)    T := b / c (int only; b % c)
    (kirem,     $+1,  1,  1,  0),  !     (T b c)    T := b irem c
    (kbitand,   $+1,  1,  1,  0),  !     (T b c)    T := b iand c
    (kbitor,    $+1,  1,  1,  0),  !     (T b c)    T := b ior c
    (kbitxor,   $+1,  1,  1,  0),  !     (T b c)    T := b ixor c
    (kshl,      $+1,  1,  1,  0),  !     (T b c)    T := b << c
    (kshr,      $+1,  1,  1,  0),  !     (T b c)    T := b >> c
    (kmin,      $+1,  1,  1,  0),  !     (T b c)    T := min(b, c)
    (kmax,      $+1,  1,  1,  0),  !     (T b c)    T := max(b, c)
  
    (katan2,    $+1,  1,  1,  0),  !     (T b c)    T := atan2(b, c)
    (kpower,    $+1,  1,  1,  0),  !     (T b c)    T := b ** c
    (kfmod,     $+1,  1,  1,  0),  !     (T b c)
  
    (ksubpx,    $+1,  1,  1,  0),  ! s   (T b c)    T := b - c*s
    (ksubp,     $+1,  1,  1,  0),  ! s   (T b c)    T := (b - c)/s

    (kneg,      $+1,  1,  1,  0),  !     (T b -)    T := -b
    (kabs,      $+1,  1,  1,  0),  !     (T b -)    T := abs b
    (kbitnot,   $+1,  1,  1,  0),  !     (T b -)    T := inot b
    (knot,      $+1,  1,  1,  0),  !     (T b -)    T := not b
    (ktoboolt,  $+1,  1,  2,  0),  !     (T b -)    T := istrue b
    (ktoboolf,  $+1,  1,  2,  0),  !     (T b -)    T := not istrue b
  
    (ksqr,      $+1,  1,  1,  0),  !     (T b -)    T := sqr(b)
  
    (ksqrt,     $+1,  1,  1,  0),  !     (T b -)    T := sqrt(b)
    (ksin,      $+1,  1,  1,  0),  !     (T b -)    T := sin(b)
    (kcos,      $+1,  1,  1,  0),  !     (T b -)    T := cos(b)
    (ktan,      $+1,  1,  1,  0),  !     (T b -)    T := tan(b)
    (kasin,     $+1,  1,  1,  0),  !     (T b -)    T := asin(b)
    (kacos,     $+1,  1,  1,  0),  !     (T b -)    T := asin(b)
    (katan,     $+1,  1,  1,  0),  !     (T b -)    T := atan(b)
 
    (klog,      $+1,  1,  1,  0),  !     (T b -)    T := log(b)
    (klog10,    $+1,  1,  1,  0),  !     (T b -)    T := log10(b)
    (kexp,      $+1,  1,  1,  0),  !     (T b -)    T := exp(b)
    (kround,    $+1,  1,  1,  0),  !     (T b -)    T := round(b)
    (kceil,     $+1,  1,  1,  0),  !     (T b -)    T := ceil(b)
    (kfloor,    $+1,  1,  1,  0),  !     (T b -)    T := floor(b)
    (kfract,    $+1,  1,  1,  0),  !     (T b -)    T := fract(b)
    (ksign,     $+1,  1,  1,  0),  !     (T b -)    T := sign(b)

    (kfloat,    $+1,  1,  2,  0),  !     (T b -)    T := float(b)
    (kfix,      $+1,  1,  2,  0),  !     (T b -)    T := fix(b)
    (ktruncate, $+1,  1,  2,  0),  !     (T b -)    T := u(b)
    (kfwiden,   $+1,  1,  2,  0),  !     (T b -)    T := r64(b)
    (kfnarrow,  $+1,  1,  2,  0),  !     (T b -)    T := r32(b)
    (kwiden,    $+1,  1,  2,  0),  !     (T b -)    T := t(b)
  
    (ktypepun,  $+1,  1,  2,  0),  !     (T b -)    T := t(u@(b))
  
    (kaddto,    $+1,  0,  1,  1),  !     (P b -)    P +:= b
    (ksubto,    $+1,  0,  1,  1),  !     (P b -)    P -:= b
    (kmulto,    $+1,  0,  1,  1),  !     (P b -)    P *:= b
    (kdivto,    $+1,  0,  1,  1),  !     (P b -)    P /:= b (float)
    (kidivto,   $+1,  0,  1,  1),  !     (P b -)    P /:= b (int: %:= b)
    (kiremto,   $+1,  0,  1,  1),  !     (P b -)    P irem:= b
    (kbitandto, $+1,  0,  1,  1),  !     (P b -)    P iand:= b
    (kbitorto,  $+1,  0,  1,  1),  !     (P b -)    P ior:= b
    (kbitxorto, $+1,  0,  1,  1),  !     (P b -)    P ixor:= b
    (kshlto,    $+1,  0,  2,  1),  !     (P b -)    P <<:= b
    (kshrto,    $+1,  0,  2,  1),  !     (P b -)    P >>:= b
    (kminto,    $+1,  0,  1,  1),  !     (P b -)    P min:= b
    (kmaxto,    $+1,  0,  1,  1),  !     (P b -)    P max:= b
    (kaddpxto,  $+1,  0,  1,  1),  ! s   (P b -)    P +:= b*s
    (ksubpxto,  $+1,  0,  1,  1),  !     (P b -)    P -:= b*s
 
    (knegto,    $+1,  0,  1,  1),  !     (P - -)    -:=P
    (kabsto,    $+1,  0,  1,  1),  !     (P - -)    abs:=P
    (kbitnotto, $+1,  0,  1,  1),  !     (P - -)    inot:=P
    (knotto,    $+1,  0,  1,  1),  !     (P - -)    not:=P
    (ktoboolto, $+1,  0,  1,  1),  !     (P - -)    istrue+:=P
  
    (kincrto,   $+1,  0,  1,  1),  !     (P - -)    ++P
    (kdecrto,   $+1,  0,  1,  1),  !     (P - -)    --P
    (kincrload, $+1,  1,  1,  2),  !     (T P -)    T := ++P
    (kdecrload, $+1,  1,  1,  2),  !     (T P -)    T := --P
    (kloadincr, $+1,  1,  1,  2),  !     (T P -)    T := P++
    (kloaddecr, $+1,  1,  1,  2),  !     (T P -)    T := P--
  
    (kswitch,   $+1,  0,  1,  0),  ! x,y (L L2 c)   switch on c; L=jumptable, L2=else label
    (kswitchu,  $+1,  0,  1,  0),  ! x,y (L L2 c)   switch on c; L=jumptable, L2=else label; unchecked
    (kswlabel,  $+1,  0,  0,  0),  !     (L - -)    label for switch jump table

    (kstop,     $+1,  0,  0,  0),  !
    (klabel,    $+1,  0,  0,  0),  !     (L - -)
  
    (kdata,     $+1,  0,  1,  0),  !
  
    (kloadbit,  $+1,  1,  1,  0),  !     (T b c)    T := b.[c]
    (kloadbf,   $+1,  1,  1,  0),  !     (T b c d)  T := b.[c..d]
    (kstorebit, $+1,  0,  1,  1),  !     (P b c)    P.[b] := c
    (kstorebf,  $+1,  0,  1,  1),  !     (P b c d)  P.[b..c] := d
    (kidivrem,  $+1,  2,  1,  0),  !     (T T c d)  (T1, T2) := C divrem d

    (kjumpin,   $+1,  0,  1,  0),  !     (L b c d)  goto L when b in c..d
    (kjumpout,  $+1,  0,  1,  0),  !     (L b c d)  goto L when b not in c..d
  
    (kclear,    $+1,  0,  1,  1),  !     (P - -)    clear P
  
    (klast,     $+1,  0,  0,  0),  !

end
