## PCL v8 Intermediate Language

This documents some details of the IL I now use in my lower-level language compilers.

### Disambiguation

All my 'PCL' languages are stack-based.

* **PCL** is also the name of the bytecode language I use for my dynamic language interpreters. There is no connection other than it is also stack-based, and was an inspiration for my static ILs.
* **PCL v7** Used in my previous compiler, and also my C-subset compiler. This one had a broader scope than v8
* **PCL v6** (The v6 is the compiler version; before v6 I didn't use ILs.) v6 was very specific to my language and how its execution model worked. v7 was more general.
* **TCL** This is the name of a 3AC-based IL that I have also tried to use. That has some useful characteristics, and it makes some things simpler. But in the end the stack-based version won out.


### Characteristics

* All these products are designed for whole-program compilers
* PCL v8 represents a whole program primarily as a symbol table or ST, which is a simple list of global variables and functions
* Each function has its own PCL IL instruction sequence of executable code
* Variable can use IL sequences of DATA instructions to represent the any initialisation expressions (not executable)
* The HLL front-end turns its AST, ST and type tables into a PCL ST and IL structures, by building them via a special API

Once in PCL form, the following possibilities exist, here for a Win64 target which is the main one:
* Generate EXE file executable directly (Windows binary)
* DLL file (relocatable binary)
* OBJ file
* ASM file in either my private format, or in AT&T form
* MX private binary format
* Run the code direcly without an discrete executable

v7 could also interpret the PCL code, or turn it into linear C code; those have been dropped here.

It could also dump PCL code into a textual format that was a standalone language. That has been dropped too; it can still be dumped, but it is for debugging purposes only.

(For the recent Z80 8-bit target, there is only ASM output, which is processed further with my own tools.)

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

#### Hints
The following are hint instructions useful when target is register-based code. Not needed when IL is interpreted, or target is also a stack machine:
````
STARTMX                                                  New resetmx/endmx sequence
RESETMX           t
ENDMX             t

SETCALL                 n                                Start of call sequence with n args
SETARG            t     n                                Mark argument n

LOADALL                                                  Ensure all pcl stack values are pushed
````
#### Directives and Miscellaneous:
````
TYPE              t                                      Auxiliary op following CALLF/ICALLF
NOP                        

COMMENT  string
EVAL
LABEL    label                   L:                       Define label
````

### The PCL API

This is not documented other that within the source code. The API is not pure: these is a mix of functions and global variables, and there can be leakage either way.

### PCL Support Library

Most instructions generate inline code. More complex ones may need a support library. There is no special provision for this ATM, just workarounds:

* For ones like POWER/i64, this is handled by by std library of my language, which is usually compiled with my apps. The backend calls into the front-end to scan for a particular function, eg. `msyslib.power_i64`, and turns it into a call to that.
* Sometimes, a special SYSCALL op is used (not in the lists; it's target-specific), an approach used for the Z80 target where there is no library yet. Then the backend generates code to call into emulator

### Deployment

The PCL backend needs to be integrated into the front-end. I no longer support a standalone product.

### Back-End Strategies

For x64 and ARM64, this gets ugly. While there is no proper optimisation, the architecture and ABIs involved make things complicated.

A simple approach such as emulating the stack behaviour of the IL is not that simple either because of the ABIs, and would be too inefficient.

However for Z80, I did end up using the stack, as there is no official ABI, and it is not much slower than the more complex approach.
