## 'PCL' Intermediate Language

### The Types

PCL only understands the primitive types shown below.

````
    void            Type not used or not set

    u8              Unsigned integers
    u16
    u32
    u64

    i8              Signed integers
    i16
    i32
    i64

    r32             Floating point
    r64

    block           Fixed-size data block of N bytes (represents all aggregate types)

    vector          Placeholder for machine vector types)

    addr            Machine address (alias for u64 etc)
````

### Opcode List

````
Opcode       Stack    Inline/    Description
             I - O    Attribs

knop         0 - 0               No-op (has internal code zero)

    Load
kload        0 - 1    A t        Z' := A     <M &M L &L C S>
kiload       1 - 1    t          Z' := Z^
kiloadx      2 - 1    t d        Z' := (Y + Z*s + d)^

    Store
kstore       1 - 0    A t        A := Z      <M>
kistore      2 - 0    t          Z^ := Y
kistorex     3 - 0    t s d      Y + Z*s + d)^ := X
kiswap       2 - 0    t          swap(Y^, Z^)

    Stack
kdupl        1 - 2               (Y', Z') := Z
kdouble      1 - 2               (Y', Z') := Z (count Z twice internally)
kswapstk     2 - 2    a b        swap(stack[a], stack[b]); index 1/2/3/4 = Z/Y/X/W
kunload      1 - 0    t          Pop stack

    Auxiliary operands
kopnd        0 - 0    A2/A3 t    Define auxiliary operand
ktype        0 - 0    t          Define auxiliary type

    Bits and Bitfields
kloadbit     2 - 1    t          Z' := Y.[Z]
kloadbf      3 - 1    t          Z' := X.[Y..Z]
kstorebit    3 - 0    t          Y^.[Z] := X
kstorebf     4 - 0    t          X^.[Y..Z] := W

    Call and Return
kcallp       n - 0    &M n v     Call &M with n args, then pop n args; v = varargs 
kicallp    n+1 - 0    n v        Call Z with nargs, then pop args (a=n+1)
kretproc     0 - 0               Return from proc
kcallf       n - 1    &M t n v   Call &M, then pop args, leave retval; v = varrgs <&M>
kicallf    n+1 - 1    t n v      Call Z, then pops args, leave retval
kretfn       0 - 0    t          Return from func with Z=retval

    Unconditional branching
kjump        0 - 0    L          goto L
kijump       1 - 0               goto Z

    Conditional branching
kjumpcc      2 - n    L t c p    goto L when Y c Z; p=1: Z':=Y, otherwise pop both
kjumpt       1 - 0    L t        goto L when Z is true
kjumpf       1 - 0    L t        goto L when Z is false

    Set return values
kjumpret     1 - 0    L t        goto L, common return point; deal with any ret value on stack
kjumpretm    a - 0    L t n      goto L, common return point; deal with multiple return values

ksetcc       2 - 1    t c        Z' := Y c Z (will be 1 or 0 for true/false)

kstop        1 - 0               Stop Z

    Looping
kto          0 - 0    L t        --M2 (aux); goto L when M2 <> 0 
kforup       0 - 0    L t n      M2 := n; goto L when M2 <= A3
kfordown     0 - 0    L t n      M2 -:= n; goto L when M2 >= A3

    Switch
kswitch      1 - 0    L x y      L=jumptab; B=elselab; x/y=min/max values
kswitchu     1 - 0    L x y      Implemented using computed-goto
kswlabel     0 - 0    L          jumptable entry
kendsw       0 - 0               Mark end of switch jumptable

kclear       1 - 0    t          Clear memory at Z^ to zeros

kassem       0 - 0    x          Inline assembly instruction

    Binary operators
kadd         2 - 1    t          Z' := Y + Z
ksub         2 - 1    t          Z' := Y - Z
kmul         2 - 1    t          Z' := Y * Z
kdiv         2 - 1    t          Z' := Y / Z
kidiv        2 - 1    t          Z' := Y % Z
kirem        2 - 1    t          Z' := Y rem Z
kidivrem     2 - 2    t          Z' := divrem(Y, Z)
kbitand      2 - 1    t          Z' := Y iand Z
kbitor       2 - 1    t          Z' := Y ior Z
kbitxor      2 - 1    t          Z' := Y ixor Z
kshl         2 - 1    t          Z' := Y << Z
kshr         2 - 1    t          Z' := Y >> Z
kmin         2 - 1    t          Z' := min(Y, Z)
kmax         2 - 1    t          Z' := max(Y, Z)
kaddpx       2 - 1    t s d      Z' := Y + Z*s + d
ksubpx       2 - 1    t s d      Z' := Y - Z*s + s
ksubp        2 - 1    t s        Z' := (Y - Z)/s

    Unary operators
kneg         1 - 1    t          Z' := -Z
kabs         1 - 1    t          Z' := abs Z
kbitnot      1 - 1    t          Z' := inot Z
knot         1 - 1    t          Z' := not Z
ktoboolt     1 - 1    t          Z' := istrue Z
ktoboolf     1 - 1    t          Z' := not istrue Z
ksqr         1 - 1    t          Z' := sqr Z

    Maths operators (unary)
ksqrt        1 - 1    t          Z' := sqrt Z
ksin         1 - 1    t          Z' := sin Z
kcos         1 - 1    t          Z' := cos Z
ktan         1 - 1    t          Z' := tan Z
kasin        1 - 1    t          Z' := asin Z
kacos        1 - 1    t          Z' := acos Z
katan        1 - 1    t          Z' := atan Z
klog         1 - 1    t          Z' := log Z
klog10       1 - 1    t          Z' := log10 Z
kexp         1 - 1    t          Z' := round Z
kround       1 - 1    t          Z' := round Z
kfloor       1 - 1    t          Z' := floor Z
kceil        1 - 1    t          Z' := ceil Z
ksign        1 - 1    t          Z' := sign Z

    Maths operators (binary)
katan2       2 - 1    t          Z' := atan2(Y, Z)
kpower       2 - 1    t          Z' := Y ** Z
kfmod        2 - 1    t          Z' := fmod(Y, Z)

    Increment (in-place increment)
kincrto      1 - 0    t n        Z^ +:= n
kdecrto      1 - 0    t n        Z^ -:= n
kincrload    1 - 1    t n        Z' := (Z +:= n)^
kdecrload    1 - 1    t n        Z' := (Z -:= n)^
kloadincr    1 - 1    t n        Z' := Z++^ (difficult to express step)
kloaddecr    1 - 1    t n        Z' := Z--^

    In-place binary operators
kaddto       2 - 0    t          Z^ +:= Y
ksubto       2 - 0    t          Z^ -:= Y
kmulto       2 - 0    t          Z^ *:= Y
kdivto       2 - 0    t          Z^ /:= Y
kidivto      2 - 0    t          Z^ %:= Y
kiremto      2 - 0    t          Z^ rem:= Y
kbitandto    2 - 0    t          Z^ iand:= Y
kbitorto     2 - 0    t          Z^ ior:= Y
kbitxorto    2 - 0    t          Z^ ixor:= Y
kshlto       2 - 0    t          Z^ <<:= Y
kshrto       2 - 0    t          Z^ >>:= Y
kminto       2 - 0    t          Z^ min:= Y
kmaxto       2 - 0    t          Z^ max:= Y
kaddpxto     2 - 0    t s d      Z^ +:= Y
ksubpxto     2 - 0    t s d      Z^ -:= Y

    Inplace unary operatirs
knegto       1 - 0    t          -:= Z^
kabsto       1 - 0    t          abs:= Z^
kbitnotto    1 - 0    t          inot-:= Z^
knotto       1 - 0    t          not:= Z^
ktoboolto    1 - 0    t          istrue:= Z^

    Type conversions
ktypepun     1 - 1    t u        Z' := t(u@(Z^))
kfloat       1 - 1    t u        Z' := cast(Z,t) Int   to real t
kfix         1 - 1    t u        Z' := cast(Z,t) Real   to int t
ktruncate    1 - 1    t u        Z' := cast(Z,u) Mask to width of u, but type is widened to t
kwiden       1 - 1    t u        Z' := cast(Z,t) Mask to width of u, but type is widened to t
kfwiden      1 - 1    t u        Z' := cast(Z,t) r32 to r64
kfnarrow     1 - 1    t u        Z' := cast(Z,t) r64 to r32

    Handle multple paths that yield one result
kstartmx     0 - 0    x          -
kresetmx     0 - 0    x          -
kendmx       0 - 0    x          -

    Define functions
kdefproc     0 - 0    M          ?
ktcproc      0 - 0    M          ?
kendproc     0 - 0               ?

    Define static variables
kistatic     0 - 0    M t        Define idata label (must be followed by correct db etc ops)
kzstatic     0 - 0    M t        Define zdata label and reserve sufficient space

    Data initialisation
kdata        0 - 0    A t        Constant data. For block types, there can be multiple C values

    Labels
klabel       0 - 0               ?
klabeldef    0 - 0               ?

ksetjmp      1 - 0               For C
klongjmp     1 - 1               For C

    Call-hinting
ksetcall     0 - 0    n          ?
ksetarg      0 - 0    n          ?
    
    Misc
kloadall     0 - 0               ?
keval        1 - 0               Evaluate Z [load to an actual register], then pop
kcomment     0 - 0    S          Comment S (a string)
kendprog     0 - 0               End-of-program marker.
````

#### Key

`Z` is the top of stack; elements below are `Y`, `X` and `W`. It notionally grows left to right.

### Number of Opcodes

There are about 150, which is quite a lot. Probably half could be dispensed with as their functionality can usually be achieved by sequences of the remaining instructions.

Ultimately, halving the opcode count would not dramatically simplify the PCL implementation, but it would the host compiler's job harder.

### Hint Instructions

**setcall, setarg**

Used to mark function calls. These make it easier to generate code for 64-bit ABIs which require stack alignment. On displays of PCL code, these are usually suppressed to reduce their clutter.

They could have been eliminated, but it would have made PCL implementation harder as strict tracking of stack usage would be needed plus backtracking to find where in the PCL code it starts to evaluate each argument.

This method is a little impure but is also much simpler, with only a few extra lines needed in the host.

**startmx, resetmx, endmx**

These are needed when there are multiple paths that can be taken to produce a single-value result. (A simple example is the `a ? b : c` operator in C; my language has more examples.)

They are needed to reset the operand stack, since the compiler does a linear pass and needs to scan all branches. And also to ensure the various results end up in the same register at the end.

(I think this is connected with 'phi' functions in SSA code.)

**Note** For a pure stack implementation (PCL is interpreted, or translated to stack-based native code), the `*mx`hints are not needed: the value will end up on top of the stack whatever happens!
