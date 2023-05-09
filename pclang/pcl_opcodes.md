## PCL Instruction Reference

PCL is a Stack-based virtual machine. This set of 0- or 1-address instructions can be either interpreted, or translated to register-based native code, with these provisos:

* Interpreted code can't fully deal with function pointers passed to or from FFis (there are other issues; see elsewhere)
* Native code for intended targets needs additional hinting instructions listed in Auxiliary section (specifically, `setcall/setarg` for calls, and `setmx/resetmx/endmx` for multiple paths used to evaluate a result).

### Syntax

* PCL source is case-sensitive (this is unique among my languages)
* It had been hoped that it had zero reserved words (allowing opcodes and types to be used as identifiers), but to allow more user-friendly optional types, the dozen or so type names are reserved (I may have to rename `mem`)
* Line comments are introduced with `!` or `;`
* There are no scopes; all names have program-wide scope except those starting with ".":
* Identifiers can have embedded ".", but a leading "." allows more readable local names. So `.x` in function `F` means the same as `F.x`, and `.x` can be reused in other functions.


### PCL Instruction List

Opcode      | Inline    | Stack        | Description
---         | ---       | ---          | ---
**Declare**|            |              |
`proc`        | \[t\] m\[\*\]  |  --          | Define proc/function m (* means exported)
`param`       | t m       |  --          | Define parameter m 
`local`       | t m       |  --          | Define local variable m
`rettype`     | t         |  --          | Define return type
`end`         | --        |  --          | End of proc/function
`extproc`     | m         |  --          | Declare imported proc/function m
`extparam`    | t         |  --          | Param of import
`extvariadic` | n         |  --          | Variadic start int
`extend`      | --        |  --          | End of extproc block
`istatic`     | t m       |  --          | Define initialised static variable m
`zstatic`     | t m       |  --          | Define unitialised static variable m
`data`        | t m/s/l/n |  --          | Define data for istatic variable
`linkdll`     | m/s       |  --          | Name of DLL as eg `msvcrt` or `"msvcrt"`
**Load/Store**|         |              |
`load`        | \[t\] m     |  (0 - 1)     | `X' := M`
`loadref`     | \[t\] m/l   |  (0 - 1)     | `X' := &M` or `&L`
`loadimm`     | \[t\] n/s   |  (0 - 1)     | `X' := N` or `S`
`store`       | \[t\] m     |  (1 - 0)     | `M := X`
`unload`      | t         |  (1 - 0)     | Pop X
`double`      | \[t\]       |  (1 - 2)     | Dupl X: `(X) => (X,X)` (see notes)
`dupl`        | \[t\]       |  (1 - 2)     | Dupl X: `(X) => (X,X)`
`swapopnds`   | \[t\]       |  (2 - 2)     | Swap X, Y: `(X,Y) => (Y,X)`
`swapmem`     | t         |  (2 - 0)     | `Swap(X^, Y^)`
`clear`       | t         |  (1 - 0)     | clear `X^` to zeros
`iload`       | t         |  (1 - 1)     | `X' := X^`
`istore`      | t         |  (2 - 0)     | `Y^ := X`
`iloadx`      | t s off   |  (2 - 1)     | `X' := (X + Y*scale + offset)^`
`istorex`     | t s off   |  (3 - 0)     | `(Y + Z*scale + offset)^ := X`
`addptrx`     | t s off   |  (2 - 1)     | `X' := X + Y*scale + offset`
`subptrx`     | t s off   |  (2 - 1)     | `X' := X - Y*scale + offset`
`subptr`      | t s       |  (2 - 1)     | `X' := (X - Y)*scale`
`loadbit`     | --        |  (2 - 1)     | `X' := X.[Y]`
`storebit`    | --        |  (3 - 0)     | `Y^.[Z] := X`
`loadbf`      | --        |  (3 - 1)     | `X' := X.[Y..Z]`
`storebf`     | --        |  (4 - 0)     | `X^.[Y..Z] := W`
**Control Flow**|       |              |
`callp`       | m n \[v\]   |  (n - 0)     | `M(...)`
`callf`       | t m n \[v\] |  (n - 1)     | `X' := M(...)`
`icallp`      | n \[v\]     |  (n - 0)     | `X^(...)`
`icallf`      | t n \[v\]   |  (n+1 - 1)   | `X' := X^(...)`
`return`      | --        |  ( - )       |  
`stop`        | --        |  (1 - 0)     | Stop execution with return code X
`jump`        | l         |  (0 - 0)     | `Goto L`
`ijump`       | --        |  (1 - 0)     | `Goto X`
`jumpeq`      | t l \[p1\]  |  (2 - 0/1)   | `Goto L when X = Y; popone: leave X on stack`
`jumpne`      | t l \[p1\]  |  (2 - 0/1)   | `Goto L when X <> Y; "`
`jumplt`      | t l \[p1\]  |  (2 - 0/1)   | `Goto L when X < Y`
`jumple`      | t l \[p1\]  |  (2 - 0/1)   | `Goto L when X <= Y`
`jumpge`      | t l \[p1\]  |  (2 - 0/1)   | `Goto L when X >= Y`
`jumpgt`      | t l \[p1\]  |  (2 - 0/1)   | `Goto L when X > Y`
`jumpt`       | t l       |  (1 - 0)     | `Goto L when X is true (X is always int)`
`jumpf`       | t l       |  (1 - 0)     | `Goto L when X is false`
`forup`       | l m \[s\]   |  (0 - 0)     | `M+:=s; goto L when A1<=A2` (uses 2 x aux opnds)
`fordown`     | l m \[s\]   |  (0 - 0)     | `M-:=s; goto L when A1>=A2` (uses 2 aux)
`to`          | l         |  (0 - 0)     | `--M;   goto L when A1<>0` (uses 1 aux)
`switch`      | L min max |  (1 - 0)     | `L=jumptab, L2=elselab (1 aux)
`swlabel`     | L         |  --          | Jumptable entry
`endsw`       | --        |  --          | Mark end of jumptable (allow segment change etc)
**Arithmetic**|           |              |
`add`         | t         |  (2 - 1)     | `X' := X + Y` (Similar for following)`
`sub`         | t         |  (2 - 1)     | 
`mul`         | t         |  (2 - 1)     | 
`div`         | t         |  (2 - 1)     |
`rem`         | t         |  (2 - 1)     | (Integer remainder)
`divrem`      | t         |  (2 - 2)     | `(X', Y') := (X % Y, X rem Y)` (`%` = int divide)
`bitand`      | t         |  (2 - 1)     | `X' := X iand Y` (bitwise AND)
`bitor`       | t         |  (2 - 1)     | 
`bitxor`      | t         |  (2 - 1)     | 
`shl`         | t         |  (2 - 1)     | (Shift left)
`shr`         | t         |  (2 - 1)     | (Shift right)
`min`         | t         |  (2 - 1)     | `X' := min(X, Y)`
`max`         | t         |  (2 - 1)     | 
`eq`          | t         |  (2 - 1)     | `X' := X = Y`
`ne`          | t         |  (2 - 1)     | 
`lt`          | t         |  (2 - 1)     | 
`le`          | t         |  (2 - 1)     | 
`ge`          | t         |  (2 - 1)     | 
`gt`          | t         |  (2 - 1)     | 
`power`       | t         |  (2 - 1)     | `X' := X ** Y`
`atan2`       | t         |  (2 - 1)     | 
`neg`         | t         |  (1 - 1)     | `X' := -X`
`abs`         | t         |  (1 - 1)     | `X' := abs(X)`
`bitnot`      | t         |  (1 - 1)     | Bitwise invert
`not`         | t         |  (1 - 1)     | `X' := (X=0 | 1 | 0)` (Logical)
`notnot`      | t         |  (1 - 1)     | `X' := (X=0 | 0 | 1)`
`sqr`         | t         |  (1 - 1)     | `X' := X*X`
`sign`        | t         |  (1 - 1)     | `X' := -1,0,1` according to `X<0, X=0, X>0`
`sqrt`        | t         |  (1 - 1)     | 
`sin`         | t         |  (1 - 1)     | `X' := sin(X)`
`cos`         | t         |  (1 - 1)     | 
`tan`         | t         |  (1 - 1)     | 
`asin`        | t         |  (1 - 1)     | 
`acos`        | t         |  (1 - 1)     | 
`atan`        | t         |  (1 - 1)     | 
`log`         | t         |  (1 - 1)     | Natural log
`log10`       | t         |  (1 - 1)     | Base-10 log
`exp`         | t         |  (1 - 1)     | 
`round`       | t         |  (1 - 1)     | 
`floor`       | t         |  (1 - 1)     | 
`ceil`        | t         |  (1 - 1)     | 
`fract`       | t         |  (1 - 1)     | 
**In-place**|           |              |
`addto`       | t         |  (2 - 0)     | `X^ +:= Y` (similar for following)
`subto`       | t         |  (2 - 0)     | 
`multo`       | t         |  (2 - 0)     | 
`divfto`      | t         |  (2 - 0)     | 
`divto`       | t         |  (2 - 0)     | 
`remto`       | t         |  (2 - 0)     | 
`bitandto`    | t         |  (2 - 0)     | 
`bitorto`     | t         |  (2 - 0)     | 
`bitxorto`    | t         |  (2 - 0)     | 
`shlto`       | t         |  (2 - 0)     | 
`shrto`       | t         |  (2 - 0)     | 
`minto`       | t         |  (2 - 0)     | `X^ := min(X^, Y)`
`maxto`       | t         |  (2 - 0)     | 
`addpxto`     | t         |  (2 - 0)     | `X^ +:= Y*s + d`; `X^` points to T; Y is i64
`subpxto`     | t         |  (2 - 0)     | `X^ -:= Y*s + d`; `X^` points to T; Y is i64
`negto`       | t         |  (1 - 0)     | `X^ := -X^`
`absto`       | t         |  (1 - 0)     | 
`bitnotto`    | t         |  (1 - 0)     | 
`notto`       | t         |  (1 - 0)     | `X^ := (X^=0 | 1 | 0)`
`notnotto`    | t         |  (1 - 0)     | `X^ := (X^=0 | 0 | 1)`
`incrto`      | t         |  (0 - 0)     | `X^ +:=s`; default s is 1
`incrload`    | t         |  (0 - 0)     | `X^ +:=s; X' := X^`
`loadincr`    | t         |  (0 - 0)     | `X' := X^; X^ +:= s`
`decrto`      | t         |  (0 - 0)     | `X^ -:=s`; default s is 1
`decrload`    | t         |  (0 - 0)     | `X^ -:=s; X' := X^`
`loaddecr`    | t         |  (0 - 0)     | `X' := X^; X^ -:= s`
**Conversion**|         |              |
`float`       | t         |  (1 - 1)     | `X' := T(X)` (convert int to float)
`fix`         | t         |  (1 - 1)     | `X' := T(X)` (convert float to int)
`truncate`    | t         |  (1 - 1)     | `X' := T(X)` (truncate integer to narrow type)
`fwiden`      | \[t\]       |  (1 - 1)     | `X' := r64(x)` (from r32)
`fnarrow`     | \[t\]       |  (1 - 1)     | `X' := r32(X)` (from r64)
`typepun`     | --        |  (0 - 0)     | `X' := T@(X)`
`widen`       | t         |  (0 - 0)     | `X' := Widen(X)` Widen from narrow type t
**Auxiliary**|          |              |
`opnd`        | \[t\] m/l/n |  --          | auxiliary op
`assem`       | \[s\]       |  --          |   Ignored in discrete PCL code
`setcall`     | n         |  --          | Init call seq with n args
`setarg`      | n         |  --          | Mark X as n'th argument of call
`setret`      | --        |  --          | Mark X as return value
`startmx`     | --        |  --          | Multi-path markers
`resetmx`     | --        |  --          |  
`endmx`       | --        |  --          |  
**Debug**   |           |              |
`printi64`    | --        |  (1 - 0)     | print X as decimal
`printu64`    | --        |  (1 - 0)     | 
`printr64`    | --        |  (1 - 0)     | 
`printr32`    | --        |  (1 - 0)     | 
`printstr`    | --        |  (1 - 0)     | print X as string
`printhex`    | --        |  (1 - 0)     | print X as hex (eg. ptr)
`printsp`     | --        |  (0 - 0)     | print SP
`test`        | --        |  (0 - 0)     | 
`debug`       | --        |  (0 - 0)     | debug 1/0 to turn it on/off

### Stack Operation

Under **Stack**, `(A - B)` denotes what is pushed and popped from the stack for each operation:

**A** is the number of relevant stack entries before the instruction, which are usually popped

**B** is how many stack there are afterwards; these are usually pushed

So `(2 - 1)` for `add` means that it consumes two stack operands, then pushes a new one, the result.

In the description, input operands are designated by `X Y Z`, sometimes `W`. Output operands, the values written, are shown as `X'` and sometimes `Y'`.

How each relates to the top of the stack depends on the `A` value in `(A - B)` as follows:
````
  A = 1       Uses X            X is the last pushed
  A = 2       Uses X, Y         Y is last pushed
  A = 3       Uses X, Y, Z      Z is last pushed
  A = 4       Uses W, X, Y, Z   Z is last pushed
````

### Inline Operands
The syntax is:
````
    Opcode [Type] [Name/Number/String/Label/Charstring] [Attributes]
````
Each operand can be optional depending on Opcode:

**Type** See the set of allowed types below

**Name** The name of a function (defined with `proc` or `extproc`) or variable (defined with `istatic`, `zstatic`, `param` or `local`). it is marked with `m` in the tables.

**Number** An integer or float literal like `12345` or `67.89`

**String** A zero-terminated string literal like `"ABC\nDEF"`; here the operand is a pointer to string data stored elsewhere (PCL takes care of that)

**Label** Labels are numeric, and specified like this: `#1234`. Label numbers are arbitrary, but it is suggested they are kept small and allocated incrementally (because the value is used to index internal tables and they have a limited size)

**Charstring** This is a string literal like `ABCDEF`, non-zero-terminated, and as immediate data. This is only used in `data` instructions.

**Attributes** These are small integer values specific to somecodes. For `callp` for example, `n` is the number of arguments used, and `v` (optional) is the argument number from which any args are variadic

### Types

Type  | Description
--- | ---
`i64` | 64-bit signed integer; these are the primary types
`u64` | 64-bit unsigned
`r64` | 64-bit float
`r32` | 32-bit float
`i8` | Narrow signed integers which are secondary types
`i16` |
`i32` |
`u8` | Unsigned
`u16` |
`u32` |

The Stack comprises 64-bit elements only, which represent only one of `i64 u64 r64 r32` (the last occupies the bottom half of an element).

There is also a block type, denoted as `u8:n`, which declares an type of `n` bytes. Block types are represent on the stack by reference, which uses a `u64` type. It can also be denoted as `T:n`, so that `i64:10` declares a block type of 80 bytes.

Secondary types are mostly used with memory operations such as `iloadx`, in-place ops like `addto`, or sometimes for conversions.

### Block Handling

A Block is fixed-size memory object, used to represent arrays and records/structs in HLL code. It's treated as a type here, but PCL mainly handles blocks by reference:

* Loading a block will load its address, so that it occupies a `u64` stack slot
* Storing a block will copy the data to the destination

This behaviour applies to all block sizes - no exceptions. ABIs mostly work like this, but they have exceptions for certain sizes, so on Windows, blocks of 1/2/4/8 bytes are manipulated by value.

This means that when generating PCL code, if you want behaviour matching the ABI, structs/arrays of those sizes must be represented as `u8/u16/u32/u64` types.

In addition, for functions returning block types, a dummy first parameter must be passed. The caller should fill this in with the address of a memory region that will contain the result. The copying however is taken care of (and the function also returns that block reference).

So block handling within the ABI is exposed, but it is necessary because the HLL needs to be aware of the semantics involved. Since blocks are passed by reference, callees could modify the caller's data.

I'm thinking of adding the ability to manipulate blocks by value, but it's likely to be in the form of special instructions rather than a new type. Multiple function returns already do that. Value blocks will be represented a multiple stack entries.

### Imported Functions

These must appear in an `extproc ... extend` block (see examples). The type info is necessary in order to correctly call the function.

### Exported Functions

These are declared like this:
````
proc cube*
````
The `*` means it is exported (note `main` is always exported).

### Program Entry and Exit

There must be a function called `main` in order to have a runnable program. This needs to be exported, but the `*` is not necessary as `main` is special.

`main` should end with a `stop` instruction; normal return won't work. (I will try and fix it so that `stop` is added automatically.)

Note that this `main` does not take any parameters such as the `(nargs, args)` you find in C. Under Windows, command like parameters can be obtained via `__getmainargs()` or, all in one string, using `GetCommandLine`.

### Notes

`dupl` and `double` do the same things in the interpreter or any target where PCL's stack is an actual one.

For a register-target where the PCL stack is compile-time only, `double` steps a count on the same stack entry rather than create two entries. This is more efficient (reuses the same register), but can only be used where the 'duplicate' is unaffected by the next instruction, ie. not part of the top `A` slots in `(A - B)`.

