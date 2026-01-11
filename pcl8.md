### PCL v8 Intermediate Language

This documents some details of the stack-based IL I now use in my lower-level language compilers. 

Note that I am not offering a working product that anyone can download and use. Just describing the design of an IL which I have refined and which seems to work well for me.

I don't go into details of how the IL is generated, or what comes next.

#### Overview

PCL is a complete representation of a whole program or library. It describes three kinds of entities:
* Symbols, a list of named variables and functions, both local and imported, forming the Symbol Table or ST
* Data, mainly initialisation data for static variables
* Code, which is sequences of executable instructions for a Stack VM

The primary data structure is the ST. Each initialised variable in the ST is a sequence of one or more DATA non-executable instructions.

Each local function in the ST has a sequence of executable IL instructions, the ones that are listed below

There is also an auxiliary data stucture, a list of import libraries, which may be needed for the backend to complete its job.

PCL is intended for whole-program compilers but can also be used for independently compiled modules (perhaps with more limited output options).

#### Generating PCL

This is done via a small library and API that is expected to be compiled-in to the front-end compiler.

The API is not documented ATM, it is defined by its source module. There is no viable textual form of the IL as there was in a previous version, only what is displayed for debugging purposes.

#### PCL IL Instructions

More info about instruction layouts and a key to the table is provided later. But I will mention that Load and Store are used in place of Push and Pop, because the latter tended to get confused with hardware Push and Pop instructions.

##### Main IL Instructions:
````
Opcode   Inline   Type  Attrs    Function                Note

LOAD     mem      t              Z' := A                 Push variable to IL stack
         &mem     t              Z' := &A
         int      t              Z' := 123
         real     t              Z' := 123.4
         string   t              Z' := "abc"             Push reference to a string
         label    t              Z' := L                 Push label reference
ILOAD             t              Z' := Z^                Push value at pointer  
ILOADX            t     s, d     Z' := (Y + Z*s + d)^    Complex address load

STORE    mem      t              A := Z                  Pop to variable
ISTORE            t              Z^ := Y                 Pop via pointer
ISTOREX           t     s, d     (Y + Z*s + d)^ := X     Complex address store

ADDPX             t     s, d     Z := Y + Z*s + d        Calc address only

ISWAP             t              swap(Y^, Z^)            Exchange values
CLEAR             t              clear Z^                Set object to zeroes

CALLP    &mem           n, v     &A(...)                 Call proc A with n args; v = variadic
CALLF    &mem     t     n, v     Z' := &A(...)           Call func A with nargs; v = variadic
ICALLP            t     n, v     Z(...)
ICALLF            t     n, v     Z' := Z(...)
RETP                             return                  Return from proc
RETF              t     n        return Z+               Return n values from func

JUMP     label                   goto L
IJUMP                            goto Z                  Z is a label pointer
JUMPCC   label    t     cc pop1  goto L when Y cc Z      Conditional jump
                                                         pop1=1: pop only Z when false
JUMPT    label    t              goto L when Z
JUMPF    label    t              goto L when not Z
JUMPRET  label    t              goto L, pop stack       Jump to common return point

SETCC             t              Z' := Y cc Z

ADD               t              Z' := Y + Z
SUB               t              Z' := Y - Z
MUL               t              Z' := Y * Z
DIV               t              Z' := Y / Z             Floating point divide
IDIV              t              Z' := Y % Z             Integer divide
IREM              t              Z' := Y rem Z           Integer remainder
IDIVREM           t              Y', Z' := Y%Z, Y rem Z  Div and remainder together

BITAND            t              Z' := Y iand Z
BITOR             t              Z' := Y ior Z
BITXOR            t              Z' := Y ixor Z
SHL               t              Z' := Y << Z
SHR               t              Z' := Y >> Z
MIN               t              Z' := min(Y, Z)
MAX               t              Z' := max(Y, Z)
POWER             t              Z' := Y ** Z

NEG               t              Z' := -Z
ABS               t              Z' := abs Z
BITNOT            t              Z' := inot Z
NOT               t              Z' := not Z
SQRT              t              Z' := sqrt Z

TOBOOLT           t u            Z' := istrue Z          Z is of type u; result is type t
TOBOOLF           t u            Z' := not Z             Z is of type u; result is type t

MATHS             t     op       Z' := op(Z)             op is a maths function
MATHS2            t     op       Z' := op(Y, Z)

INCRLOAD          t     n        Z' := ++(Z^)            Increment amount is n (can be < 0)
LOADINCR          t     n        Z' := (Z^)++

TYPEPUN           t u            Z' := cast@(Z, t)       Z has type u; interpret as type t
                                                         (int/float only; sizes must match)
FLOAT             t u            Z' := cast(Z, t)        Int (u) to float (t)
FIX               t u            Z' := cast(Z, t)        Float (u) to int (t)
WIDEN             t u            Z' := cast(Z, t)        Widen integer type from u to t
FWIDEN            t u            Z' := cast(Z, t)        Widen float type (r32 to r64)
FNARROW           t u            Z' := cast(Z, t)        Narrow float type (r64 to r32)

JUMPCC   label    t     cc pop1  goto L when Y cc Z      Conditional jump
JUMPRET  label    t     n        goto L                  Jump to common return point; n ret values

DUPL                             Y' := Z' := Z           Duplicate top of stack
UNLOAD            t              Discard Z
````
##### Data Instruction
This one is used internally to represent static data, including jump tables for 'switch':
````
DATA     &mem     t                                      For data only
         int      t
         real     t
         string   t
         label    t
````
The string operand is a reference to a string stored elsewhere. For an actual value string, a sequence of int `data` items can be used, either 8 bits at a time or packed into workds.

(In my API, such string data, which can be zero-terminated or not, is generated uses a function call like this; `p` is a reference to an AST node here:)
````
pgen(kdata, pgendata(p.svalue, p.slength))
````

##### Miscellaneous

**Hints** 
````
STARTMX                                                  New resetmx/endmx sequence
RESETMX           t
ENDMX             t

SETCALL                 n                                Start of call sequence with n args
SETARG            t     n                                Mark argument n

LOADALL                                                  Ensure all pcl stack values are pushed (the backend may use
                                                         lazily loaded operands)
DOUBLE                           Y' := Z' := Z           Emulate 'dupl' without duplicating (backend can treat as DUPL,
                                                         or take advantage). Restrictions apply
````
**Directives Etc**
````
NOP                        
TYPE              t                                      Auxiliary op following CALLF/ICALLF
COMMENT  string
EVAL
LABEL    label                   L:                       Define label
````
#### Keys to Instruction Tables

##### Some Symbols
````
&      Address-of
^      Postfix pointer dereference
:=     Assignment or Push
...    Argument list
````
##### Instruction Format
Each instruction has these fields:
````
Opcode                One of the capitalised codes above
Operand Type          Which kind of operand is used, including None
Operands              Where used, one of `mem &mem int real string label`
Type and Size         Shown as `t` where used
Secondary type        Shown as `u` where used
Attributes            Optional 1 or 2 integers, which are variously named as shown
````

##### Stack Operands
PCL instructions are for a stack machine. The top stack element is always called Z; the next is Y if used, and the one below is X.

If X, Y or Z appear in the Function column, these can be assumed to be consumed, or popped from the stack. (With the odd exception described in the Note).

Any newly pushed result is shown as Z' (or, where there are two new results, both Y' and Z')

##### Inline Operands

Any Instruction may operate on values on the stack (X Y Z) or with operands that are part of the instruction, or both. Where inline operands are used, they will one of these six. The second column shows the forms used under Function:
````
    mem              A                 Contents of static, global or local variable
    &mem             &A                Address of such a variable, or address of a function
    int              123               Integer constant
    real             123.4             Floating point constant
    string           "abc"             A reference to a string stored elsewhere
    label            L                 A numeric label (eg. L56)
````
##### Attributes
There can also be some extra bits of info. This lists all shown:
````
    n          Count (eg. number of call arguments)
    s          Scale factor for complex pointer instructions (eg. byte-size of a pointer target type)
    d          Extra byte-offset for complex pointers (so it is not scaled)
    cc         Condition code, one of EQ NE LT LE GE GT
    v          A value of 1 (rather than 0) means a called function uses C-style variadic arguments, which have
               special ABI rules
    pop1       A value of 1 rather than 0 means that only Z is popped for JUMPcc rather than X Y, but only if
               false; ie. the jumps is not taken. This is used for chain sequences of compares against the
               same X
    op         For maths functions, one of SIN COS TAN ASIN .... (in my HLLs such functions are built-in operators)
````
#### The Type System
These the types used:
````
void            No type info
i8              Signed integer
i16
i32
i64
u8              Unsigned integer
u16
u32
u64
r32             Floating point
r64
mem:N           ('Block') Any aggregate type of N bytes
(vector         Reserved type)
````
**Record/Array** types are represented by an N-byte block. Alignment info for the whole block is deduced from the overall size. Struct layout info does not exist; this is up to the front-end.

(This would be at odds with SYS V ABI where structs passed by-value have incomprehensible passing rules. However I understand that neither LLVM nor Cranelift get involved with this either.)

**Blocks** are notionally manipulated by-value. However, a LOAD of a block value will only load a reference; it will not copy, unless this is pushing a function argument where the ABI requires by-value passing of structs. But this is up to the backend; the front end can assume it is all by-value unless it has extra info.

(If used for compiling C, as v7 was, then the backend must make copies of structs passed as arguments, as the language requires it.)

**Vector** is reserved for the short-vectors used in SIMD-style instructions and registers. But I don't do anything with these yet since none of my HLLs support such types.

While this type system is PCL's, my front end builds on top of it. Otherwise there is the problem that there are different denotations for an 'i64' type for example, in front and back ends.

Note there is no type defining a machine address or pointer. The front end can define an alias for that in terms of `u64` for example, or perhaps 'u16' for small devices.

#### Platform ABI

I have tried to make it so what whoever/whatever generated the IL, does not need to know how the ABI works, and can assume a pure stack machine where everything is manipulated by value. That would be the headache of the backend.

But as noted above, there is currently some leakage. And the potential difficulties are acknowledged with the SETCALL/SETARG hint instructions, to simplify the backend's job for ABIs like Win64 and SYS V.

One assumption that is made currently, is that function arguments are evaluated right-to-left. This should not matter; the back end could reorder them, but it is awkward. Alternately the PCL backend (the bit past the API) could be interrogated for such details.

#### Startmx/Resetmx/Endmx

These are necessary hints used when one of several paths is taken to evaluate some result. This is a 2-way example in HLL syntax:
````
    (c | a | b)                  # select a or b depending on C
     c ? a : b                   # in C syntax
````
(My HLLs have several such constructions and in general are N-way.)

With a pure stack machine, the result will always be at the top of the stack whatever the path. But that is not the case with a register-based target; results may be in diverse registers.

So `startmx resetmx endmx` are used to mark each path. Example PCL for my `(c | a | b)` example, when all have i64 types:
````
    startmx                                       Stack is ()
    load      c                          i64      Stack is (c)
    jumpf     L3                         i64      Stack is ()
    load      a                          i64      Stack is (a)
    resetmx                              i64      Reset to ()
    jump      L2                                  
L3: 
    load      b                          i64      Stack is (b)
    endmx                                i64      
L2:                                               Stack is either (a) or (b)
````
Effectively, `startmx` remembers the current stack state ('()' here), and each `resetmx` will restore it to that state.

These hints are not needed for PCL that is interpreted, or translated directly to stack machine. Unless it still involves the backend keeping track of the stack level as it performs a linear pass through PCL; then it needs those reset points.

(I think this problem is related to 'phi nodes' used in SSA representations, but I know little about it.)

#### Multiple Function Return Values

Inside the callee, this works the same way as normal returns: all N values are pushed, and `jumpret` with n = N is used to jump to the common return point. (A normal `jump` won't do as `jumpret` has to pop those N values off the stack so that the following code can be processed.)

The common return point still needs `retfn`; in the backend, this might serve to get all the values into the correct registers.

At the call-site, it's a little different:

* For 1 return value, a normal `callf` or `icallf` is used. The instruction type gives the type (and hence location) of the return value.
* For N return values, there are N auxiliary `type` instructions following. Each gives the type of the corresponding return value, in LTR order.

#### Example

I won't give an example of generating IL for some trivial hard-coded program. That would not be typical of how it is used. But this is an example of what PCL looks like in practice, for this function in my HLL:
````
func bitsinbyte(int b)int =
    int c, m

    c := 0
    m := 1
    while m < 256 do
        if b iand m then
            ++c
        end
        m <<:= 1
    end

    c
end
````
And here is the PCL IL as displayed by my diagnostic routines:
````
Proc bitops.bitsinbyte(i64 b)i64 =
    i64 c
    i64 m

    load      0                          i64      
    store     c                          i64      
    load      1                          i64      
    store     m                          i64      
    jump      L3                                  
L2: 
    load      b                          i64      
    load      m                          i64      
    bitand                               i64      
    jumpf     L6                         i64      
    load      c                          i64      
    load      1                          i64      
    add                                  i64      
    store     c                          i64      
L6: 
    load      m                          i64      
    load      1                          i64      
    shl                                  i64      
    store     m                          i64      
L3: 
    load      m                          i64      
    load      256                        i64      
    jumplt    L2                         i64      
    load      c                          i64      
    jumpret   L1                         i64 /1   
L1: 
    retfn                                i64 /1     
End
````
Those /1 near end represent the `n` attribute, in this case the number of return values.

Typical code generated for x64 is this:
````
bitsinbyte:
    R.b = D10                        # non-standard x64 register names
    R.c = D3
    R.m = D4
    push      R.c
    push      R.m

    xor       R.c, 	R.c
    mov       D0, 	1
    mov       R.m, 	D0
    jmp       L3
L2:
    mov       D0, 	R.b
    and       D0, 	R.m
    jz        L6
    inc       R.c
L6:
    shl       R.m, 	1
L3:
    cmp       R.m, 	256
    jl        L2
    mov       D0, 	R.c
    pop       R.m
    pop       R.c
    ret       
````
Not great, but not that terrible either. However, this is specific to my back end, which does not optimise. I believe the IL contains all the info needed to allow
all the usual optimisations.




