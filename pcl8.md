## PCL v8 Intermediate Language

This documents some details of the IL I now use in my lower-level language compilers.

### Overview

PCL is a complete representation of a whole program or library. It describes three kinds of entities:
* Declarations, stored in a linear Symbol Table (ST). This is a linear list of variables and functions, both local and imported
* Data, mainly static initialisation data for static variables
* Code, which is sequences of executable instructions for a Stack VM

The primary data structure is the ST (declarations). Each initialised variable in the ST will a sequence of one or more DATA non-executable instructions.

Each local function has sequence of one or more of the executable IL instructions that are listed below

There is one more auxiliary data stucture which is list of import libraries, which may be needed for the backend to do its job.

PCL is intended for whole-program compilers with a backend (the bit that comes after the PCL stage) that directly generates execubles, or even runs the generated code, without extraneous tools such as linkers.

However this describes only the form of the IL; it does not dictate what happens next. It just strives to include enough information to make that job possible.

### Generating PCL

This is done via a small library and API that is expected to be compiled-in to the front-end compiler. Although the front and back ends are demarcated (eg. PCL has its own ST and type system), there is some leakage between the two.

The API is not documented ATM, it is defined by its source module.

(I am not offering a working product, just describing the design of an IL which I have refined and which seems to work well.)

### PCL IL Instructions

More info about instruction layouts and a key to the table is provided later.

#### Main IL Instructions:
````
Opcode   Inline   Type  Attrs    Function                Note

LOAD     mem      t              Z' := A                 Push variable to IL stack
         &mem     t              Z' := &A
         int      t              Z' := 123
         real     t              Z' := 123.4
         string   t              Z' := "abc"
         label    t              Z' := L
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
RETF                    n        return Z+               Return n varues from func

JUMP     label                   goto L
IJUMP                            goto Z                  Z is a label pointer
JUMPCC   label    t     cc pop1  goto L when Y cc Z      Conditional jump
                                                         pop1=1: pop only Z when false
JUMPT    label    t              goto L when Z
JUMPF    label    t              goto L when not Z
JUMPRET  label    t              goto L                  Jump to common return point

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

TYPEPUN           t u            Z' := t(u@(Z^))
FLOAT             t u            Z' := cast(Z, t)        Int (u) to float (t)
FIX               t u            Z' := cast(Z, t)        Float (u) to int (t)
WIDEN             t u            Z' := cast(Z, t)        Widen integer type from u to t
FWIDEN            t u            Z' := cast(Z, t)        Widen float type (r32 to r64)
FNARROW           t u            Z' := cast(Z, t)        Narrow float type (r64 to r32)

JUMPCC   label    t     cc pop1  goto L when Y cc Z      Conditional jump
JUMPRET  label    t              goto L                  Jump to common return point

DUPL                             Y' := Z' := Z           Duplicate top of stack
DOUBLE                           Y' := Z' := Z           Emulate 'dupl' without duplicating
UNLOAD            t              Discard Z
````
#### Data Instruction
This one is used internally to represent static data. This includes jump tables for 'switch':
````
DATA     &mem     t                                      For data only
         int      t
         real     t
         string   t
         label    t
````

#### Miscellenous

**Hints** The following are hint instructions useful when target is register-based code. Not needed when IL is interpreted, or target is also a stack machine:
````
STARTMX                                                  New resetmx/endmx sequence
RESETMX           t
ENDMX             t

SETCALL                 n                                Start of call sequence with n args
SETARG            t     n                                Mark argument n

LOADALL                                                  Ensure all pcl stack values are pushed
````
**Directives Etc**
````
TYPE              t                                      Auxiliary op following CALLF/ICALLF
NOP                        

COMMENT  string
EVAL
LABEL    label                   L:                       Define label
````
### Keys to Instruction Tables

#### Instruction Format
Each Instruction has these fields:
````
Opcode                One of the capitalised codes above
Operand Type          Which kind of operand is used, including None
Operands              Where used, one of `mem &mem int real string label`
                      Under 'Function`, uses generic examples `A &A 123 123.4 "abc" L`
Type and Size         Shown as `t` where used
Secondary type        Shown as `u` where used
Attributes            Optional 1 or 2 integers, which are variously named as shown
````

#### Stack Operands
PCL instructions are for a stack machine. The top stack element is always called Z; the next is Y if used, and the one below is X.

If X, Y or Z appear in the Function columns, these can be assumed to be consumed, or popped from the stack. (With the odd exception described in the Note).

Any newly pushed result is shown as Z' (or, where there are two new results, both Y' and Z')

#### Inline Operands

Any Instruction may operate on values on the stack (X Y Z) or with operands that are part of the instruction, or both. Where inline operands are used, the will one of these six. The right column shows the forms used under Function:
````
    mem              A                 Contents of static, global or local variable
    &mem             &A                Address of such a variable, or address of a function
    int              123               Integer constant
    real             123.4             Floating point constant
    string           "abc"             A reference to a string stored elsewhere
    label            L                 A numeric label (eg. L56)

#### Attributes
````
There can also be some extra bits of info. This lists all shown:
````
    n          Count (eg, call arguments)
    s          Scale factor to complex pointer instructions (eg. byte-size of a pointer target type)
    d          Extra byte-offset for complex pointers (so it is not scaled)
    cc         Condition code, one of EQ NE LT LE GE GT
    v          A value of 1 (rather than 0) means a called function uses C-style variadic arguments, which have
               special ABI rules
    pop1       A value of rather than zero means that only Z is popped for JUMPcc rather than X Y, but only if
               false; ie. the jumps is not taken. This used used for chain sets of compared
    op         For maths functions, one of SIN COS TAN ASIN .... (in my HLLs such functions are built-in operators)

#### Type System
These the types used:
````
void            Means no type
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
block           Any aggregate type of N bytes
(vector         Reserved type)
````
* Record/Array types are reprented by an N-byte block. Alignment info for the whole block is deduced from the overall size. Struct layout info does not exist; this is up to the front-end.

(This would be at odds with SYS V ABI where structs passed by-value have incomprehensible passing rules. However I understand that neither LLVM nor Cranelift get involved with this either.)

* Blocks are notionally manipulated by-value. However, a LOAD of a block value will only load a reference; it will not copy, unless this is pushing a function argument where the ABI requires by-value passing of structs. I decided to leave this up to the backend. 

The front end either can assume it is by-value, or can use knowledge of the back-end to say that blocks will be passed by reference. (So in my HLL, they will be passed by reference, but if compiling C, structs (not arrays) must be passed by value. However my C compiler only used v7)

* A 'Vector' is reserved for the short-vectors used in SIMD-style instructions and registers. But I don't do anything with these yet since none of my HLLs support such types.

#### Platform ABI

I have tried to make it so what whoever/whatever generated the IL, does not need to know how the ABI works, and can assume a pure stack machine where everyhing is passed by value. That would be the headache of the backend.

But as noted above, there is currently some leakage.

Also, there is the existence of of SETCALL/SETARG hint instructions, to specifically simply the backend's job for ABIs like Win64 and SYS V.

(SYS V for ARM64 is particularly complex, requiring even more hinting, which lead to me abandoning that target. If I revisit it, I might use by own ABI and only use the official one for the FFI.)

#### STARTMX/RESETMX/ENDMX

These are necessary hints used when one of several paths is taken to evaluate some result. For example, this is a 2-way example:
````
    (c | a | b)                  # select a or b depending on C
     c ? a : e                   # in C syntax
````
(My HLLs have several such constructions, and in general are N-way.)

With a pure stack machine, the result will always be at the top of the stack whatever the path. But that is not the case with a register-based target; results may be in diverse registers.

So `startmx resetmx endmx` are used to guard each path. (This needs some PCL examples.)

These hints are not needed for PCL is interpreted, or translated directly to stack machine. Unless it still involves the backend keeping track of the stack level as it performs a linear pass through PCL; the stack must be reset at `resetmx` and `endmx`.

(I think this problem is related to 'phi nodes' used in SSA representations, but I know little about it.)
