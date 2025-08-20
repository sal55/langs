Describes stack-based IL. Extracted from my actual compiler code. "!" starts a line-comment

!The top 4 stack operands are designated as:
!
!   - - - - - Z          1 operand used in IL instruction
!   - - - - Y Z          2 operands
!   - - - X Y Z          3 operands
!   - - W X Y Z          4 operands
!
!The stack notionally grows from left to right. Z is always top-of-stack
!
!Results may be shown as being stored in one of those same operands, eg.
!
!     Y := Y + Z            or:
!     Y +:= Z
!
!Here, Z is popped so that the Y operand becomes the new top-of-stack Z.
!But usually the new stack top is designated as Z':
!
!     Z' := Y + Z

!Immediate operand:
!   A           (various)
!Extra info:
!   op          opindex
!   fn          fnindex
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

const MA = memaddr_opnd
const M  = mem_opnd
const L  = label_opnd
const S  = string_opnd
const A  = any_opnd

export enumdata [0:]ichar pclnames,
                [0:]byte pclhastype,
                [0:]byte pclextra,
                [0:]byte pclhasopnd,
                [0:]byte pclargs =

!                       t  x op args    (a  b)
    (knop=0,       $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) ?

    (kload,        $+1, 1, 1, A, 0),  ! (0 - 1) (M L t i   ) Z' := M &M L &L 123 4.5 "abc"; i=1 for in-place ref
    (kiload,       $+1, 1, 0, 0, 0),  ! (1 - 1) (t         ) Z' := Z^
    (kiloadx,      $+1, 1, 2, 0, 0),  ! (2 - 1) (t d       ) Z' := (Y + Z*s + d)^

    (kstore,       $+1, 1, 0, M, 0),  ! (1 - 0) (M t       ) M := Z
    (kistore,      $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ := Y
    (kistorex,     $+1, 1, 2, 0, 0),  ! (3 - 0) (t s d     ) (Y + Z*s + d)^ := X
    (kstorem,      $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' :=(Y, Z) for mem:16

    (kdupl,        $+1, 0, 0, 0, 0),  ! (1 - 2) (          ) Z' := Y' := Z
    (kdouble,      $+1, 0, 0, 0, 0),  ! (1 - 2) (          ) Count extra instance of Z
    (kswapstk,     $+1, 0, 2, 0, 0),  ! (2 - 2) (a b       ) Swap(stack(a, 0), stack(b)); 1/2/3/4 = Z/Y/X/W
    (kunload,      $+1, 1, 0, 0, 0),  ! (1 - 0) (t         ) Pop stack

    (kopnd,        $+1, 1, 0, A, 0),  ! (0 - 0) (M L C t   ) Define auxiliary operand M or L
    (ktype,        $+1, 1, 0, 0, 0),  ! (0 - 0) (t         ) Define auxiliary type t

    (kloadbit,     $+1, 1, 0, 0, 2),  ! (2 - 1) (t         ) Z' := Y.[Z]
    (kloadbf,      $+1, 1, 0, 0, 2),  ! (3 - 1) (t         ) Z' := X.[Y..Z]
    (kstorebit,    $+1, 1, 0, 0, 2),  ! (3 - 0) (t         ) Y^.[Z] := X
    (kstorebf,     $+1, 1, 0, 0, 2),  ! (4 - 0) (t         ) X^.[Y..Z] := W

    (kcallp,       $+1, 0, 2,MA, 9),  ! (n - 0) (M n v     ) Call &M with nargs, then pop args; v = varargs
    (kicallp,      $+1, 0, 2, 0, 9),  ! (n - 0) (n v       ) Call Z with nargs, then pop args (a=n+1)
    (kretproc,     $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) Return from proc
    (kcallf,       $+1, 1, 2,MA, 9),  ! (n - 1) (M t n v   ) Call &M, then pop args, leave retval; v = varrgs
    (kicallf,      $+1, 1, 2, 0, 9),  ! (n - 1) (t n v     ) Call Z, then pops args, leave retval (a=n+1)
    (kretfn,       $+1, 1, 0, 0, 0),  ! (0 - 0) (t         ) Return from func with Z=retval

    (kjump,        $+1, 0, 0, L, 0),  ! (0 - 0) (L         ) goto L
    (kijump,       $+1, 1, 0, 0, 0),  ! (1 - 0) (          ) goto Z
    (kjumpcc,      $+1, 1, 1, L, 0),  ! (2 - n) (L t c p   ) goto L when Y c Z; p=1: Z':=Y (b=0/1)
    (kjumpt,       $+1, 1, 0, L, 0),  ! (1 - 0) (L t       ) goto L when Z is true
    (kjumpf,       $+1, 1, 0, L, 0),  ! (1 - 0) (L t       ) goto L when Z is false
    (kjumpret,     $+1, 1, 0, L, 0),  ! (1 - 0) (L t       ) goto L, common return point; deal with any ret value on stack
    (kjumpretm,    $+1, 1, 0, L, 0),  ! (a - 0) (L t n     ) goto L, common return point; deal with any ret value on stack

    (ksetcc,       $+1, 1, 0, 0, 0),  ! (2 - 1) (t c       ) Z' := Y cc Z

    (kstop,        $+1, 0, 0, 0, 0),  ! (1 - 0) (          ) Stop Z

    (kto,          $+1, 1, 0, L, 0),  ! (0 - 0) (L t       ) --B (aux); goto L when B<>0 
    (kforup,       $+1, 1, 1, L, 0),  ! (0 - 0) (L t n     ) B+:=n; goto L when B<=C
    (kfordown,     $+1, 1, 1, L, 0),  ! (0 - 0) (L t n     ) B-:=n; goto L when B>=C

    (kiswap,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) swap(Y^,Z^)

    (kswitch,      $+1, 1, 2, L, 0),  ! (1 - 0) (L t x y   ) L=jumptab; B=elselab; x/y=min/max values
    (kswitchu,     $+1, 0, 2, L, 0),  ! (1 - 0) (L x y     ) L=jumptab; B=elselab; x/y=min/max values
    (kswlabel,     $+1, 0, 0, L, 0),  ! (0 - 0) (L         ) jumptable entry
    (kendsw,       $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) Mark end of switch jumptable

    (kclear,       $+1, 1, 0, 0, 0),  ! (1 - 0) (t         ) Clear Z^

    (kassem,       $+1, 0, 0, A, 0),  ! (0 - 0) (x         ) To be worked out....

    (kadd,         $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y + Z

    (ksub,         $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y - Z
    (kmul,         $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y * Z
    (kdiv,         $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y / Z
    (kidiv,        $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y % Z
    (kirem,        $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y rem Z
    (kidivrem,     $+1, 1, 0, 0, 0),  ! (2 - 2) (t         ) Z' := divrem(Y, Z)
    (kbitand,      $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y iand Z
    (kbitor,       $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y ior Z
    (kbitxor,      $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y ixor Z
    (kshl,         $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y << Z
    (kshr,         $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y >> Z
    (kmin,         $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := min(Y, Z)
    (kmax,         $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := max(Y, Z)
    (kaddpx,       $+1, 1, 2, 0, 0),  ! (2 - 1) (t s d     ) Z' := Y + Z*s + d
    (ksubpx,       $+1, 1, 2, 0, 0),  ! (2 - 1) (t s d     ) Z' := Y - Z*s + s
    (ksubp,        $+1, 1, 1, 0, 0),  ! (2 - 1) (t s       ) Z' := (Y - Z)/s

    (kneg,         $+1, 1, 0, 0, 0),  ! (1 - 1) (t         ) Z' := -Z
    (kabs,         $+1, 1, 0, 0, 0),  ! (1 - 1) (t         ) Z' := abs Z
    (kbitnot,      $+1, 1, 0, 0, 0),  ! (1 - 1) (t         ) Z' := inot Z
    (knot,         $+1, 1, 0, 0, 0),  ! (1 - 1) (t         ) Z' := not Z
    (ktoboolt,     $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := istrue Z; u is of type u; result is type t
    (ktoboolf,     $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := not istrue Z
    (ksqr,         $+1, 1, 0, 0, 0),  ! (1 - 1) (t         ) Z' := sqr Z

    (ksqrt,        $+1, 1, 0, 0, 0),  ! (1 - 1) (t         ) Z' := sqrt Z
    (ksin,         $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := sin Z
    (kcos,         $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := cos Z
    (ktan,         $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := tan Z
    (kasin,        $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := asin Z
    (kacos,        $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := acos Z
    (katan,        $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := atan Z
    (klog,         $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := log Z
    (klog10,       $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := log10 Z
    (kexp,         $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := round Z
    (kround,       $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := round Z
    (kfloor,       $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := floor Z
    (kceil,        $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := ceil Z
    (ksign,        $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := sign Z

    (katan2,       $+1, 1, 0, 0, 2),  ! (2 - 1) (t         ) Z' := atan2(Y, Z)
    (kpower,       $+1, 1, 0, 0, 2),  ! (2 - 1) (t         ) Z' := Y ** Z
    (kfmod,        $+1, 1, 0, 0, 2),  ! (2 - 1) (t         ) Z' := fmod(Y, Z)

    (kincrto,      $+1, 1, 1, 0, 0),  ! (1 - 0) (t n       ) Z^ +:= n
    (kdecrto,      $+1, 1, 1, 0, 0),  ! (1 - 0) (t n       ) Z^ -:= n
    (kincrload,    $+1, 1, 1, 0, 0),  ! (1 - 1) (t n       ) Z' := (Z +:= n)^
    (kdecrload,    $+1, 1, 1, 0, 0),  ! (1 - 1) (t n       ) Z' := (Z -:= n)^
    (kloadincr,    $+1, 1, 1, 0, 0),  ! (1 - 1) (t n       ) Z' := Z++^ (difficult to express step)
    (kloaddecr,    $+1, 1, 1, 0, 0),  ! (1 - 1) (t n       ) Z' := Z--^

    (kaddto,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ +:= Y
    (ksubto,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ -:= Y
    (kmulto,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ *:= Y
    (kdivto,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ /:= Y
    (kidivto,      $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ %:= Y
    (kiremto,      $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ rem:= Y
    (kbitandto,    $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ iand:= Y
    (kbitorto,     $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ ior:= Y
    (kbitxorto,    $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ ixor:= Y
    (kshlto,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ <<:= Y
    (kshrto,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ >>:= Y
    (kminto,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ min:= Y
    (kmaxto,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ max:= Y
    (kaddpxto,     $+1, 1, 1, 0, 0),  ! (2 - 0) (t s       ) Z^ +:= Y*s
    (ksubpxto,     $+1, 1, 1, 0, 0),  ! (2 - 0) (t s       ) Z^ -:= Y*s

    (knegto,       $+1, 1, 0, 0, 0),  ! (1 - 0) (t         ) -:= Z^
    (kabsto,       $+1, 1, 0, 0, 0),  ! (1 - 0) (t         ) abs:= Z^
    (kbitnotto,    $+1, 1, 0, 0, 0),  ! (1 - 0) (t         ) inot-:= Z^
    (knotto,       $+1, 1, 0, 0, 0),  ! (1 - 0) (t         ) not:= Z^
    (ktoboolto,    $+1, 1, 0, 0, 0),  ! (1 - 0) (t         ) istrue:= Z^

    (ktypepun,     $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := t(u@(Z^))
    (kfloat,       $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) Int   to real t
    (kfix,         $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) Real   to int t
    (ktruncate,    $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,u) Mask to width of u, but type is widened to t
    (kwiden,       $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) Mask to width of u, but type is widened to t
    (kfwiden,      $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) r32 to r64
    (kfnarrow,     $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) r64 to r32

    (kstartmx,     $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) -
    (kresetmx,     $+1, 1, 0, 0, 0),  ! (0 - 0) (t         ) -
    (kendmx,       $+1, 1, 0, 0, 0),  ! (0 - 0) (t         ) -

    (kproc,        $+1, 0, 0, M, 0),  ! (0 - 0) (M         ) ?
    (ktcproc,      $+1, 0, 0, M, 0),  ! (0 - 0) (M         ) ?
    (kendproc,     $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) ?
    (kistatic,     $+1, 1, 0, M, 0),  ! (0 - 0) (M t       ) Define idata label (must be followed by correct DATA ops)
    (kzstatic,     $+1, 1, 0, M, 0),  ! (0 - 0) (M t       ) Define zdata label and reserve sufficient space
    (kdata,        $+1, 1, 0, A, 0),  ! (0 - 0) (M L C t   ) Constant data. For block types, there can be multiple C values
    (kinitdswx,    $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) Following two ops initialise doswitchx jumptable

    (klabel,       $+1, 0, 0, L, 0),  ! (0 - 0) (          ) ?
    (klabeldef,    $+1, 0, 0,MA, 0),  ! (0 - 0) (          ) ?
    (ksetjmp,      $+1, 0, 0, 0, 0),  ! (1 - 0) (          ) For C
    (klongjmp,     $+1, 0, 0, 0, 0),  ! (1 - 1) (          ) For C

    (ksetcall,     $+1, 0, 1, 0, 0),  ! (0 - 0) (n s       ) n=args, s=1 for simple call

    (ksetarg,      $+1, 0, 2, 0, 0),  ! (0 - 0) (n1 n2     ) n1=arg no (LTR) n2=int or real arg no (maybe neg for real)
    (kloadall,     $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) ?

    (keval,        $+1, 0, 0, 0, 0),  ! (1 - 0) (          ) Evaluate Z [load to an actual register], then pop
    (kcomment,     $+1, 0, 0, 0, 0),  ! (0 - 0) (C         ) Comment C (a string)
    (kendprog,     $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) End-of-program marker.
!------------------------- -
!these are only used in textual PCL code

    (kparam,       $+1, 1, 0, M, 0),  ! (0 - 0) (M t       ) Define param
    (klocal,       $+1, 1, 0, M, 0),  ! (0 - 0) (M t       ) Define local
    (krettype,     $+1, 1, 0, 0, 0),  ! (0 - 0) (t         ) Define return type
    (kvariadic,    $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) Variadic C function
    (kaddlib,      $+1, 0, 0, S, 0),  ! (0 - 0) (S         ) Define import library
    (kextproc,     $+1, 0, 0, M, 0),  ! (0 - 0) (M         ) Define imported proc
end
