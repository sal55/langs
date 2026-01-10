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

