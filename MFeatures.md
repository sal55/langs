## M Language Summary

'M' is currently my personal language for low-level programming.

The following discusses a bunch of interesting features, often relative to the C language.

It is not a formal language reference, or a standard, or tutorial. There is little discussion of semantics
or undefined behaviours or finer points of grammar.

-- Bart

### Overview

'M' is a lower level systems language. It is roughly at the level of C, but has a quite different syntax, and many modern conveniences.

It has been around a long time, and has been implemented on one platform at time, from 8-bit microprocessors through 3 generations of x86, evolving along the way. It was an in-house tool and now is a personal language. 

The current implementation is for x64 running Windows. M programs used to run on Linux via a transpiler tool, but that has fallen out of use.

### The 'Q' Language

This is a companion, dynamic and interpreted scripting language. This is not documented, however it shares pretty much the same syntax as 'M'. Most of what's described below applies to both.

### Highlights

* Whole-program compiler. This affects the language design
* 64-bit-based (64-bit default `int` and float types and promotions)
* N-based arrays, but defaults to 1-based
* Case-insensitive
* Non-brace syntax
* Expression-based
* Module scheme; no external build system needed
* Fast, single file, self-containing compiler, includes run-from-source as native code or via interpretation

### Syntax

This was heavily inspired by Algol68, but with a number of differences, like no keyword stropping or embedded whitespace in identifiers. It is now evolved into its own style.

Keywords like `fi od esac` are still used, but conventional `end` keywords can be used too. There are no `begin...end` blocks.

Braces are not used at all . `{...}` is reserved for anonymous functions; not used in M (but they appear in Q.)

See various examples around this site. M source files have extension ".m", or ".ma" for amalgamations of source files.

### Hello, World
````
proc main =
    println "Hello, World!"
end
````
Here's a meatier example:
````
proc main =
    for i to 20 do
        println i, sqrt i
    od
end
````
Demonstrates:
* No standard libraries are needed for language features
* `proc` is used for functions with no return value, otherwise it is `func`
* One form of `for` loop, which here starts from 1 if not specified
* `For`-loop indices can be automatically declared (if so, they are also read-only)
* `sqrt` is a built-in, and is an operator, hence parentheses are optional
* Automatic conversion from integer to float
* `println` is built-in, and needs no format strings or codes
* It will add spaces between print items (`,,` will suppress them)
* The `od` shows the Algol68 influence, but `end`, `end do` or `end for` will all work too
* Since there are no declarations, both these examples are also valid programs in my Q scripting language

### Case-insensitive

This my preference since I started coding on case-insensitive OSes, file systems and languages.

This means that `int Int INT` are all the same identifier, so it is necessary to be a little more creative in naming, compared with C.

For imports of case-sensitive external functions there are two possibilities:
````
   func "MessageBoxA"(...)
````
Use a string literal for the name, then any choice of capitalisation style can be subsequently used. Or:

````
   func `MessageBoxA(..)
````
Backtick preserves case in identifiers, but it means it has to be typed like this, with backtick everywhere. (This may be necessary when imported symbols clash when case is ignored.)


Another use for backtick is to allow reserved words as identifiers:
````
int `Int, `INT
````
However it is ugly, and mainly used for machine translation into this syntax.


### Source File Encoding and Unicode Support

Source files are except to be either ASCII, or UTF8. Support for Unicode is as follows:
````
    Identifiers         A-Z and a-z only plus _ $ 0-9
    Comments            UTF8 supported
    String literals     UTF8 contents supported
                        16- or 24-bit Unicode characters via escape codes
````
Support of UTF8-encoded Unicode within string data is up to the libraries used. For M, strings are zero-terminated sequences of `u8` bytes.

### Line-oriented and Semicolons

M uses semicolons to separate statements. But because M source is line oriented (like most languages I guess), nearly every line would end with an unnecessary semicolon.

For that reason newlines are converted to semicolons unless a line obviously continues to the next. For example, when it ends with:
````
    ( or [ or ,
    Binary operator
    \ line continuation
````
(Comments are disregarded.)

With this scheme, semicolons are extremely uncommon and mainly to separate statements on one line.

### Comments and Doc-strings

There are only line comments, which start with `!` and continue to end of line. ("!" was used in the DEC Algol and Fortran languages that I first coded in.)

Block comments are considered to be an editor function, which can emulate them with "!".

At one time there were also doc-strings, line comments starting with `#`, that were associated with a function.

A compiler option were write all the doc-strings, with their linked function signature, to a file.

Currently they have been removed. (Examples of code in this document however will use '#' for comments as it's more universally recognised.)

### Module Scheme

The current scheme is a recent addition. [Modules24](Modules24.md) which describes the scheme in detail.

There is a 2-level structure: programs consist of sub-programs which consist of modules.

In short, all project info is provided by a list of directives at the start of the lead module. Usually, they will be the only thing in the lead module. The directives are:
````
project =
    module name             # Module name forming part of this main subprogram
    import name             # Lead module of another subprogram used as a library here
    linkdll name            # Dynamic shared library if it is not clear from importdll statements
    $sourcepath "path"      # temporary directive until path control is sorted out
end
````
Given:
````
  module parse
````
the name of the module's source file will be "parse.m". The base filename, minus extension,must be a valid M identifier.


### Declarations and Definitions

All user-identifiers in an M program (across all modules) are either:

* Defined in this program
* Declared in an `importdll` block, and imported from an external library.

In an M program, each name is defined in exactly one place, and only once. The module scheme will find it.

There are no separate declarations, not in the same program.

(If the name is exported into another program, M or not, it will need a declaration there. Sometimes, eg. when creating DLLs, the compiler will automatically suitable declarations from the definitions.)

This means it much harder than it is in C, to have name `abc` be declared one way on one module, another way in a different module.

### Scope Control

All top-level names outside of function or record can be exported using these attributes before the definition:
````
         # (None) Local name not exported; equivalent of 'static' in C
global   # Make the name visible to other modules in this subprogram
export   # Additionally export it to other subprograms
         # For the main subprogram when creating DLLs, export it from the library
````

This apples to: functions, variables, named constants, enumerations, records, types, and macros, and makes them available to other parts of the program.

The names of imported functions are automatically reexported from their containing module (as usually they are declared in their own module and used elsewhere).

This is unlike C where such attributes, I believe called 'linkage', apply only to symbols with associated storage, such as functions and variables. Shared types for example need to to use shared header files.

### 'Units' and 'S-units'

The syntax is expression-based, which means expressions and statements are interchangeable.

I use 'unit' to refer to an individual expression or statement. While 'sunit' is a semicolon-separated sequence of units and definitions.

In the bits of syntax described later, `u` indicates a single unit is allowed, while `s` allows a sequence of them, which can be empty, for example, here are simplified IF and REPEAT statements:
````
  if s then s [else s] end
  repeat s until u
````

Both kinds can return values, but often this will have a `void` type:

* The value of an sunit is that of its last unit
* An empty sunit is `void`
* A definition has a `void` type if an attempt is made to use it
* Units such as loops have `void` results

Generally it's just common sense.

### Entry Point

This is the function `main`:
````
proc main =

end
````
It is special since it will be automatically exported. Also, a `stop 0` is inserted at the end, so running into the end will execute `stop 0` (see below). Its normal `return` is not used.

`main` can be located in any module of the main subprogram. (Other subprograms could contain their own `main`.)

### 'Start' Functions

Any module can have an optional `start` function. If present, it will be called automatically before any other code in the `main` function.

If multiple modules have `start`, then their call order is determined by the module order in the project info. If subprograms have `start` functions too; it gets complicated.

There is a set of rules which explain it exactly, but it is simpler not to have to depend on that. Use other init routines called explicitly, or also start() explicitly (and earlier than normal) but be prepared to deal with a duplicate call.


### Command Line Parameters
`main` takes no parameters. Two global variables, exported from the standard library, give information about parameters:
````
    ncmdparams         How many parameters were provided
    cmdparams          Pointer to array of parameter strings
````
So in an invocation like this:
````
    prog A B
````
`ncmdparams` is 2, `cmdparams[1]` contains string A, and `cmdparams[2]` contains B. (No derefs are needed.)

With no arguments, `ncmdparams` is 0.

`cmdparams[0]` will always contain the name of the program as it was typed, in this case `prog`.

In cases where programs are run via an interpreter, or run from source, for example:
````
    ms prog a b                # ms compiles prog.m and runs it
    pc prog a b                # pci interprets prog.pcl
    aa -r prog a b             # aa -r assembles prog.asm then runs it
````
then `cmdparams[0]` will contain `prog` in all cases. A special mechanism is used to move the 'parameter window` along so that applications think they have been launched normally.

However programs which directly call functions like `__getmainargs` or `GetCommandLine` will see all inputs.


### Termination
This is by any of these, at any point in the program:
````
stop               # same as stop 0
stop n             # terminate with exit-code n
`exit(n)           # Use C function
ExitProcess(n)     # Use WinAPI function
````
(`exit` is a reserved word, so the backtick is needed.)

Running into the end of `main` will execute `stop 0`.

### 'Comma Operator'

Sometimes you want to write several units but the syntax only allows one. This is where the equivalent of C's comma operator is used but it looks like this: `(u; u; u)`. So semicolons are needed, and it needs parentheses.

There is no actual comma operator in M; it is just syntax.

Note that `(u;...)` does not quite yield an sunit, although it has mostly the same effect; name definitions are not allowed in the sequence, only executable code.

### Out-of-Order Definitions

All user-identifiers in an M program can be defined in any order. This applies not just to functions, which is common (although not in C!) but also to variables, types, etc. So you never need to worry about refering to something which is defined later on in a module, and exported from some obscure module.

The compiler will find it via the module scheme. This is possible for example:
````
const a = b + c           # a will be 22
const b = c + 2           # b will be 12
const c = 10
````
But this will report an error:
````
const a = b
const b = a
````

### Include

The language supports textual inclusion of code:
````
include "file"             # defaults to .m extension unless specified
````
This is uncommon though.


### N-Based

C is famously zero-based, in terms of its arrays. M is a mixture:
````
                    Default     Override?

    Arrays          1-based     Yes
    For-loops       1-based     Yes         Default used in 'for i to B do...'
    Bit-indexing    0-based     No          A.[i] and A.[i..j]
    N-way select    1-based     No          As used in (n | a, b c | z)
    Enumerations    1-based     Yes
````
Arrays are 1-based unless overridden, mostly commonly with 0. Bit indexing is 0-based due to convention (bit 0 is the least significant.)

An example of an unusual lower bound:
````
['A'..'Z']int counts
...
if c in counts.bounds then ++counts[c] fi
````
The array has 26 elements; in C it would either use 65 wasted elements, or indices need to be offset.

### Variable Definitions

(I'm itching to write 'declarations', but I want to be consistent with using 'definitions' for the names of entities that belong to this program, and declarations for those that are imported.)

The official syntax for defining variables is:
````
var T a, b:=0, c              # T is any type; initialisation is optional
````

However, `var` is optional, and because of that, virtually never used as it adds too much clutter.

The original intention was that T could be omitted, to allow for simple type inference (that's not done; only for `const`).

Also, `var` is what is used in the Q language, which at one point allowed also `var T`.

There was an attempt to use `let` for read-only or assign-once variables, but that was poorly supported.

### Numeric Types
There is a choice of denotations:

````
Regular:    int8   int16  int32  int64      (signed)
            word8  word16 word32 word64     (unsigned)
            char8  char64
            bool8  bool64
            real32 real64                   (floating point)

Compact:    i8 i16 i32 i64
            u8 u16 u32 u64
            c8 c64
            r32 r64
````
And there are aliases commonly used:
````
    byte      u8
    char      c8
    int       i64
    word      u64
    real      r64
````

`char` is a thin wrapper around `byte` or `u8`. It allows `Print` for example to assume that a `ref char` type represents a string.

When 32-bit targets were supported, there were also `intp intm wordp wordm` for signed or unsigned integer types that corresponds to the targets pointer size (p) or word size (m) respectively.

Those are no longer used since I settled on 64-bit targets.

### Pointers and Void
If T is any type, then `ref T` denotes a pointer to that type, equivalent to `T*` in C.

There can also be pointers to things that are either not types by themselves, or not allowed in the syntax:
````
 ref void              # Like C's void*
 ref proc...           # function pointers
 ref func...
 ref label             # label pointer (in C, exists as an extension)
````

The `ref char` type is recognised as special, and also has the `ichar` alias. String literals for example will have `ref char/ichar` type.

#### Null Pointer Constant

This is the special value `nil`, with type `ref void`. Zero can't be used as in C.

### Array Types
Arrays are defined like this:
````
 [10]int A
 [10,20]int B, C                # B and C are the same size and shape
````
Since no `comma operator` exists, multi-dimensional indexing is sweeter: `B[i, j]`

There are two big differences from C:
* Arrays can have a lower bound other than 1 (the default)
* Arrays are manipulated by value.

Arrays can be unbounded like in C, but that's mainly when they are a pointer target. Bounds can be specified in many ways:
````
 []int A                 # Unbounded, lowerbound (lwb) is 1
 [0:]int B               # Unbounded with lwb 0
 [10]int C               # Bounds are 1..10 inclusive
 [0:10]int D             # Bounds are 0..9
 [0..9]int E             # Same using range syntax ([x:] is legal syntax, [x..] is not)
 [3:]int F = (10,20,30)  # Bounds are 3..5; length determined by init data
````
Flat (not Iliffe-vector) multi-dimensionsal arrays are limited: all bounds except the leftmost
must be specified.

Arrays bounds, when given, must be compile-time expressions.

Arrays can be copied directly, if they have the same element type and length, but LWB can vary:
````
 [10]int A
 [0..9]int B

 A := B
````
Arrays can be passed and return by value to functions, a least notionally. Except for certain sizes, ABIs demand that they are passed by reference.

In my implementaton, my back end is responsible for doing the implicit copying to enable that (or will do when it gets around to it).

In practice, arrays are passed as pointers (define a parameter type `ref[]int X`), or using pass-by-reference (parameter type is `[]int &X`).

An alternative to passing arrays, which usually need an accompanying length, is using slices, described below.

### Record Types

M's record or struct types differ from C:

* Each must be a named user type; there are no ad hoc anonymous records
* There is no concept of a 'struct tag'
* There is no automatic padding of elements, or of overall struct size
* They can contain other entities besides fields; see Encapsulation below.

Example showing two styles:
````
    record date = (int d, m, y)
    record date =
        int d, m
        int y
    end
````

#### Record Layout Control:
I've introduced `struct/union` - C terms, but used here only for this purpose - to control layout of fields:
````
    record R =
        union
           int32 a
           int32 b
           struct
              byte f,g,h,i
           end
        end
    end
````
This record is only 4 bytes in size, because the space is shared: `a b f` are all at offset 0. In the past I used `@` to share space in the record. This still works; the same example would be:
````
    record R =
        int32 a
        int32 b @ a
        byte  f @ a
        byte  g @ a+1    # always uses byte offset
        byte  h @ a+2
        byte  i @ a+3
   end
````

#### Record Field Alignment
The compiler does not automatically insert padding to give ideal alignment to fields. This must be done manually.

However, there is a feature used to match the layout of external structs which usually follow C rules. This can be used to emulate such alignment:
````
    record rec = $caligned
        byte a
        int64 b
    end
````
Now, the size is 16 bytes, and `b` starts at offset +8. Without `$caligned`, the overall size would be 9 bytes, and `b` starts at offset +1.

### Self-referential types

This is mainly about records containing references to themselves, or via circular references. C has some problems with that that involve struct tags, and forward declarations. None of that applies here:
````
record node1 =
    int data
    ref node2 p
end

record node2 =
    int data
    ref node1 p
end
````
This is where out-of-order is useful. 


### Bitfields in Records
These correspond roughly to C's bit fields in structs:
````
    record demo =
        byte flags: (a:1, b:1, c:4)
    end
````
* Bit fields must be inside a conventional member
* Fields are allocated from bit 0
* An error is reported if it overflows the field (unlike C, where it just allocates another member)
* All flags in that group can be accessed as one via the containing field
* All bitfield types are unsigned. (There may an exception for one that includes the sign bit of an `i64` containing field.)

### Type Aliases
These are very simple:
````
type newtype = typespec
````
Unlike C, this is restricted:
* `type` must be on the left! (In C, it is more flexible...)
* Only one new type name can be defined at a time, not various combinations of pointers and arrays involving the basic new type.
* You can't define an alias for a function signature

But like C, these are aliases for the type on the right, unless it is a record type, however that uses a different syntax anyway (`record newtype = ...`). 


### Static

`static` is only used on variables inside functions. It is not allowed outside, and has no connection with whether names are exported or not.

### Namespaces

With the module scheme, comes namespaces. The module name can be used to qualify a name:
````
module modulename
....
a := modulename.F()
````
A shorter alias can specified (which also makes it easier to change module names without revising code):
````
module modulename as mn
....
a := mn.F()
````
But this may not be needed: here just F() can be used if it is not ambiguous (which would be reported). The full name may be needed:
* When two imported modules both export F
* When there is a local F defined which shadows the imported one (this is not reported, shadowing is normal)

Namespaces are also created with Records, and Functions; see Encapsulation.

Subprograms create their own namespace, as their internal modules are private. So here:
````
import lib
````
`lib` is the namespace for all names exported from `lib` (using `export` not `global`) regardless of which internal module the name resides in.


### Min/Max Values of Types

C has a header full of macros defining limits for all the integer types. Including special sets for the ones in `stdint.h`. In M it's much simpler:

````
X.min, X.max     # Smallest and largest values of X or typeof(X)
X.bounds         # Equivalent to X.min..X.max
````

The last two can used as the bounds of an array definition, or the RHS of the 'in' operator. (Note `typeof` is not a feature of M.)

### Sizeof and Bounds

Various property-yielding operators are available:
````
X.bytes        # size of of type in bytes (sizeof in C)
X.bitwidth     # width in bits of primitive type (CHAR_BIT * sizeof in C)
X.len          # Length of an array whose size is known
X.lwb          # Lower bound
X.upb          # Upper bound
X.bounds       # X.lwb..X.upb
````
When X is an expression, then its type is used.

X.bounds does not yield a value (a range is not a type in M, only in Q), but this can be used in the places range synyax can be.


Slices are a little different:
* .bytes refers to the slice descriptor not the slice itself
* .lwb works for a type (it is known at compile time)
* .len/.upb/bounds depend on runtime length, not contained within the type

### Named Constants

Something that's always been missing from C is the concept of a named constant: applying a name to a literal value.

The possibilities (`#define, const T, enum {}`) all have something missing: proper scoping, or odd evalation rules, limited type, not allowed as a compile-time constant etc.

A named constant in M is defined like this:
````
  const A = 100
  const int B = 200            # the type is usually infered
  const C = 300.0
````
These are properly scoped; evaluated once; can have arbitrary numeric type; and can't have `&` applied.

It is also possible to do `const D = "ABC"`, but there `&` can be applied.


### Enumerations
Enums in M look like this in the simplest form:
````
    enumdata =
        red,
        green,
        blue
    end
````

`red, green, blue` have values `1, 2, 3` respectively. But `enumdata` is normally used like this:
````
    enumdata = []ichar colournames, []u32 colourvalues =
        (red,     $,    0xFF'00'00),
        (green,   $,    0x00'FF'00),
        (blue,    $,    0x00'00'FF),            # trailing commas are OK
    end
````
It defines a set of enumerations, and a corresponding parallel set of arrays at the same time.

`$` is a device which returns the last-created enum name as a string, eg. `"red"`, to avoid having to repeat the enum name.

Enums starting at any value are possible: use `red = N`, and `[N:]` for the array bounds. But they must be sequential from that point on.

C normally uses X-macros for this stuff.

The set of enumerations do not form a new data type. The set of names is in the same namespace as other entities in this scope. However, enums can also be encapsulated inside a record for example:
````
record colours =
    enumdata =
        red,
        green,
        blue
    end
end
````
Now they have to be accessed as `colours.red` etc.

### Tabledata

This is like `enumdata` but there are no enumerations involved; just parallel arrays:
````
tabledata []ichar names, []int values =
    ("one",   100),
    ("two",   200),
    ("three", 300),
end
````

### Encapsulation

Records can contain other entites besides fields: types, nested record types, named constants, enumerations, macros, and functions.

Those belong to the record type, and do not need an instance of the record to exist.

That can also be accessed from outside, using the record type name as a qualifier. An example with enums was given above, here's one with functions:
````
record rect = 
    real width, height

    func area(rect R)real =
        R.width * R.height
    end
end
````
That function can be called in two ways:
````
rect r := (20, 30)

println rect.area(r)          # directly accessing the function
println r.area()              # or via the instance, using method call syntax
````
Or, a record type can simply be a container for all sorts of unrelated stuff, where instances are never needed. However I've done very little with this, and I don't claim this to be a form of OOP; there's too much still missing.

(Functions can also be used as containers in the same way, but probably not to be recommended.)


### Mixed Arithmetic and Promotions
This is mixed arithmetic involving signed and unsigned integers. The rules for C are complex; when `int`
is a 32-bit type, they look like this:
````
       i8  i16 i32 i64  u8  u16 u32 u64

   i8   S   S   S   S    S   S   u   u
  i16   S   S   S   S    S   S   u   u
  i32   S   S   S   S    S   S   u   u
  i64   S   S   S   S    S   S   S   u

   u8   S   S   S   S    S   S   u   u
  u16   S   S   S   S    S   S   u   u
  u32   u   u   u   S    u   u   u   u
  u64   u   u   u   u    u   u   u   u
````
S/u mean the result is signed or unsigned. In M, the table has S everywhere, except for u64/u64 on the bottom right with a solitary u.

Promotions of narrower types `i8 i16 i32 u8 u16 u32` are always to i64.

### Type Conversions and Type-Punning

The syntax for converting X to type T is this:
````
   cast(X, T)           # always works
   T(X)                 # works when T is simple (ambiguous otherwise)
````
Often, just `cast(X)` will do, and the compiler will work out what type to cast to to make it work.

M also has special syntax for type-punning:
````
   cast@(X, T)
   T@(X)
````
No conversion are performed, this reinterprets the bits of `X` as being of type `T`. Mainly this is for scalar types.

Type-punning works on rvalues too: `println int@(X+1.0)`.

### Extra Operators

````
x ** y            # power (higher, right-to-left precedence than * and /)
min(x, y)         # min/max (can also be written in-fix)
max(x, y)
abs(x)
sqr(x)            # x * x
````
All the above are overloaded for multiple integer and float types. A special one is this:
````
(q, r) := x divrem y  # integers only, sets q to x/y and r to x rem y
````
And there are a bunch of maths ops built-in as operators: `sqrt sin cos tan asin acos atan...`


### 'In' operator
This is used in conditionals:
````
if x in a .. b then             # true when x >= a and x <= b
if x not in a .. b then         # invert the result with 'not'
if x in [a, b, c] then          # true when x = a or x = b or x = c
````
`x` will always be evaluated once. The range can also be `T.bounds` or `X.bounds` (either the range of an integer type,
or the bounds of an array).

### '$' Symbol

This is multi-purpose depending on context
````
 (red, $)            # stringify the last enum name
 print x, $          # add trailing space which is otherwise messy
 A[$-1]              # $ is A.upb
````

### = and :=
Throughout the language, `:=` used for runtime assignments, and `=` (when not used for equality) is used for things which are defined at compile-time or at least before execution starts:
````
const C = 78            # set a compile-time; uses =

proc F =                # F is defined at compile-time!
   static int A = 34    # 'assigned' once at 'compile-time'
   int B := 56          # happens at runtime so needs :=
end
````

### Swap

This simply exchanges two values:
````
 swap(A[i], A[j+1])
````
It could be done via multiple assignment:
````
 (A[i], A[j+1]) := (A[j+1], A[i[)

````
but more terms need to be written; it is more error prone, and it is not obvious an exchange is being done without looking carefully. It's also harder to generate efficent code with a simple compiler.


### Chained Comparisons
Any of = <> < <= >= > can appear in a chain:
````
    if a = b = c
    if a <= b < c
````
Middle terms are evaluated once only. The whole yields bool. It is suggested that if any of `< <= >= >` appear, then they all face the same way, otherwise the meaning is obscure.

To get the meaning it would have in C, requires breaking it up with parentheses: `a = (b = c)`.

(Since `in` was introduced, then the most common use for this is checking for equality of N different terms. So it might be  restricted in future. Another problem is that some combinations like `a <> b <> b` are confusing and anti-intuitive.)

### Augmented Assignment

These work as they do in C, but with some extras:
````
 x min:= y               # for this purpose, infix style is needed
 x max:= y
````
They also work for unary operators:
````
-:= x        x := -x
abs:= x      (TODO)
inot:= x     x := inot x
not:= x      x := not x
istruel:= x  (TODO)
````
### ++ and --
These work as they do in C, but are restricted to integers and pointers

### Bit and Bitfield Indexing

The language favors special operators for bit access rather than special types (or, in C, needing special purpose macros):
````
    A.[i]           Extracts bit i from `i64` value A. It will be 0 or 1. Bit 0 is least significant
                    and bit 63 the most significant

    A.[i..j]        Extracts the bitfield i..j inclusive from A. Usually i<j so that A.[0..7] extracts
                    the bottom byte, but for constant i and j, the order can be reveresed: A[7..0]

    A.[i] := x      Store x in bit i. x should be 0 or 1 only (for constant x, it will be masked, but
                    not for variable x)

    A.[i..j] := x   Store value x in bitfield i..j. Again, x should not exceed the field width.
````
Some other variations were tried in the past, but currently are not used:
````
    A.byte[i]       Extract byte i from A, with i being 0 to 7. I think other types were possible like
                    A.u16[i].
    A&.[i]          Similar to A.[i], but the bit is not shifted down. The result of A&.[8] will
                    be 0, or 256, instead of 0, or 1.
````
See also Bitfields in Records. I haven't found a way to have named offsets to access bitfields within an integer. The nearest is:

````
    macro offset = 0..23
    int a
    a.[offset]
````
The square brackets are needed (macro replacement only works on top-level names).

#### Some Built-in Bitfields
````
    A.even          1 when A is even (read-only)
    A.odd           1 when A is odd, same as A.[0] (read-only)
    A.lsbit         A.[0]
    A.msbit         A.[63]
    A.lsb           A.[0..7]
    A.msb           A.[56..63]
    A.lsw           A.[0..31]
    A.msw           A.[32..63]
````
All are read/write except the first two.

### Integer Constants
These have values from `0` to `18446744073709551615`. Values up to `9223372036854775807` have type `i64`, above that `u64`.

In some contexts, when an `i64` constant is combined with a `u64` operand, then the constant is considered to be `u64` too. The ensures the whole operation is unsigned. Otherwise, because i64 is dominant, in an operation like this:

    u64 a = 0xFFFF'FFFF'FFFF'FFFF
    if a > 5 then ...

The comparison would be performed using i64, which means comparing -1 with 5, giving the wrong result.

There are some bugs associated with the value `-9223372036854775808` (because there are two tokens, and the constant part is processed as a `u64` value). Instead use `i64.min` to enter that value.

Constants can use separators `_` or `'` (underscore or single quote) in any combination, but they can't start with a separator.

Multiplier suffixes can be applied to integer constants:
````
    3 million              # value is 3'000'000
    3 billion              # value is 3'000'000'000
    3 million billion      # value is 3'000'000'000'000'000
````
(These names live in a separate namespace so they can still be used as normal identifiers.)

Only decimal, hex and binary are supported within constants:
````
    1234       Decimal
    0x1234     Hex
    2x10111    Binary
    10111B     Binary (alternative)
````
Printing of integers in any base 2-16 is supported, eg. `print a:"x3"` displays `a` in base 3, with dedicated `"h"` and `"b"` codes for hex and binary.

#### Floating Point Constants
These are written as follows:
````
    1.234
    1e6
    1.2e-4
    0.5
````
`.6` would be invalid; it needs at least '0' before a decimal point


### String Constants
These are written in double quotes "....". They cannot span multiple lines (ie. include raw newlines).
Escape codes for embedded special characters are listed below.

Adjacent strings can be combined with `+`; see Compiler-time String Operators below.

#### String Constant Prefixes

These are special forms of strings with a single character prefix in each case:

````
    R"ABC"           # Both are raw strings without escape codes
    F"ABC"           # F is more suited to file paths that use "\"

    S"ABC"           # Create zero-terminated string data object
    B"ABC"           # Create binary string data object
````
See Data Strings below.

### Character Constants

They look like this:
````
    'A'                            # Has type char, with value 65
    'AB' up to 'ABCDEFGH'          # Has type u64 (not char64)
````

`'ABC'` has the leftmost character (`A` here), in the least significant position in the word. The intention is that when stored in memory, it has the same layout as the string `"ABC"`. (However this is only valid for little-endian; I've never used big-endian.)

Unused parts of a word will be zeros. Escape codes (`\u \v`) can be used to specify Unicode characters, which are stored as multi-byte UTF8 sequences. Multiple Unicode characters can be specified,  subject to fitting within 64 bits. 
### String Escape Codes
These can be used also for character constants, but not raw strings:

````
    \a           Char 7 (bell/alert)
    \b           Char 8 (backspace)
    \c \r        Char 13 (carriage return)
    \f           Char 12 (formfeed)
    \l \n        Char 10 (line feed, or Linux newline)
    \e           Char 27 (Escape)
    \t           Char 9 (tab)
    \udddd       Specify 16-bit hex Unicode character value
    \vdddddd     Specify 24-bit hex Unicode character value
    \w           Char 13, 10 (Windows CR/LF sequence)
    \xdd         Specify 8-bit hex ASCII, UTF8 component, or raw binary value
    \y           Char 16 (backwards tab in my apps)
    \z \0        Char 0 (embedded zero; will need care)
    \" ""        Char '"' (one double quote; used in string constant)
    \\           Char "\" (backslash)
    \'           Char "'" (single quote, used in char constant)
````

Unicode characters are converted to UTF8 sequences.

### Data Strings and Embedding

There are String (zero-terminated text) and Binary (raw data) objects which can be specified via special string constants or loaded from files:
````
    s"ABC"            3 bytes plus zero terminator, 4 bytes in all
    b"ABC"            3 bytes only, no terminator

    sinclude(file)    Should be a text file (no embedded zeros), zero terminator added
    binclude(file)    Any text or binary file, no zero terminator added
````

String data can be used wherever a normal string is expected (its type is ref char). Binary data has type `ref byte` but is usually malleable to whatever data it initialises.

Both String and Binary data can be used to initialise arrays:
````
    []char S = s"ABC"          4 bytes total
    []char T = b"ABC"          3 bytes total
    []char U = sinclude(file)  Length is size of file + terminator

    []byte V = b"XYZ"
    []byte W = binclude(file)  Length is the length of the file

    []real x = binclude(file)  Length is the byte-length of the file / 8
````

Note can `[]char S` can't be initialised with a regular string, as the types won't match (`[]char` needed, but a string contant is `ref char`).

For initialising anything other than byte-arrays, binary files should contain suitable data in correct endian format.

Here's a novelty use of embedding:
````
println sinclude($filename)
````
This prints the current source file, which will have been embedded in the executable.

### Compile-time String Operators

`+` and `*` can work with strings known at compile-time. These are either literals, or named constants yielding strings:
````
"ABC" + "DEF"           # yields "ABCDEF"
"ABC"*5                 # "ABCABCABCABCABC"

const euro  = "\u20AC"
const price = "4.99"
ichar mess = euro + price

const A = "*"
const B = "--"
const pattern = (A+B)*5+A     # *--*--*--*--*--*
````
They also work with Data Strings, but string/binary can't be mixed:
````
ichar twofiles = sinclude("file1") + sinclude("file2")
````

### Data Initialisation

This is initialising variables when they are defined. Simple scalar variables are easy. But aggregate types like arrays and records are much more restricted than they are in C.

If such a variable is initialised then all elements must provided; there can't be fewer. And the parentheses used must exactly match the structure of the type:

````
    record date=
        int d,m,y
    end

    []date A := ((16,3,25), (17,3,25), (18,3,25))       # valid
    []date A := ((16,3), (17,3,25), (18,3,25))          # not valid (missing element)
    []date A := (16,3,25, 17,3,25, 18,3,25)             # not valid (bad grouping)
````
C is incredibly lax here. Too many {} is an error; too few is fine. For arrays, it can be useful to define only the first few elements, amd have the rest zeroed, but it can also hide errors (missing values).

I'd considered a feature like this:
````
   [100]int A := (1, 2, 3, ...)
````
which explicitly asks for remaining elements to be zeroed. But I haven't tried it yet.

For zeroing all elements of an aggregate type, then it's done using 'clear': `clear A`.

### Operator and Precedences
There are fewer levels than in C, despite there being more operators (Level 1 is highest with tightest binding):
````
Level   Operators at that level

1       **                               #  (right to left)
2       * / % rem << >>
3       + - iand ior ixor min max        # (bitwise and/or/xor)

5       = <> < <= >= > in

6       and                              # (logical and/or)
7       or

(8      :=                               # (right to left))
````
* Three main groups are shown:
  * First includes the arithmetic ops taught in schools
  * Second are comparions
  * Third are logical operators

* `min max` normally use function-like syntax rather than infix; but `max(x, y)` and `x max y` will both work
* The "." in `a.b` is not a operator; it has syntax that binds more tightly than even unary operators
* Level 4, not shown, is for ".." which constructs ranges. It is syntax more than operator.


### Two-way, N-way and other Conditional Selections
C has two-way selection with `c ? a : b`. M has that and more:
````
    (c | a | b)                     # two-way select (yield a when c is true, else b)
    if c then b else c fi           # also two-way select

    (c | a, b, c, ... | z)          # select 'th expresson, or z if out of range (1-based)
````
Note the first two are exactly equivalent: `(||)` is a compact way to write `if-then-else-fi`. Other conditional statements can also act as selections, yielding the first true branch:
````
    if c1 then elsif ... else ... fi
    switch x ... else ... end
    case x ... else ... end
````
With all these, the 'else' (or final `| x`) is mandatory when the whole thing is expected to yield a value. Otherwise it can be omitted.

All such constructs can be used as L-values:
````
   if c then a else b fi := 0          # set either a or b to zero
````
If & is applied outside, it will be propagated inside:
````
    &(c | b | c)                       # becomes (c | &b | &c)
````

### Multiple Assignments
These work like this; multiple terms must be parenthesised:
````
    (a, b, c) := (x, y, z)        # Terms must match in number
````
Assignments are done through an intermediate value so that this can be used for rotations and exchanged.

Nested lists are not supported. There are a couple of special forms:
````
    (a, b, c) := f()              # f() must return multiple values
    (a, b) := x divrem z          # Some special ones like this
````
General deconstruction, of an array or record into its elements for example, are not supported. (The Q language has all these features.)

### Address-of Operator

Address-of used `&` like C, with mostly the same rules. An array by itself does not turn into a reference, so usually needs it. If A is an array with type []T then:
````
  A          # Has type []T (with bounds if not unbounded)
  &A         # Has type ref[]T
  &A[A.lwb]  # Or &A[1] etc if lwb is known; has type ref T
  &.A        # Short-cut way to get pointer to first element
  A          # In some cases (A is []char and need ref char), this will do what is necessary
````

### Pointer Dereferencing

Dereferencing is done with the post-fix operator `^` (Pascal style). If P has type `ref T`, then `P^` yields a type `T`.

Pointer dereferencing used to be very strict, but now the rules are relaxed; `^` can be omitted in the terms on the left, so that you only need what's on the right:
````
P^[i]         P[i]
Q^.m          Q.m
S^^.m         S.m
T^(x)         T()
````
There is a reduction in transparency (is Q a pointer or not?), but the gain in readability is significant. (Also, code can be more easily ported to my Q language where explicit derefs are uncommon.)

C also allows some derefs to be dropped, but through various language quirks: `P[i]` because `P` has type `T*` not `T(*)[]`; `Q->m` rather thaan `(*Q).m` via that odd operator; and `T(x)` through other arcane rules.

In M, the type system requires the derefs, but the compiler will implicitly add derefs to make it correct, if there are too few.

It can't however add them for examples like `P`, since, if `P` is a double pointer example, then  `P P^ P^^` are all meaningful.

### Piping
This is an experimental feature where a function like `F(G(x))` can be written like this for increased readability:
````
  x -> F -> G
````
It also works when functions take more than one parameter, but remembering whether the piped argument will be the first or the last is troublesome.

### Function Syntax
Various examples:

````
func F(int a, b, real x)int =   # return type is written last
    ...
    return 0                    # just 0 will do in this case
end
````

Examples taking no arguments:
````
func G:int =
func G()int =                     
````
Either `:` or `=>` precedes the return type when no parameter list (not even `()`) appears. They can be optionally added between `)` and the return type.

There's a special syntax for one-liners:
````
fun triple(real x)real = x*3

fun fib(int n)int = (n<3 | 1 | fib(n-1) + fib(n-2))
````
(The body is `u` rather than `s` followed by `end`.)

Functions returning no value must use `proc`:
````
proc H =
end
````
Function nesting is not fully supported. They can be written, but you can't access the local variables and parameters of the containing function. There are some other limitations, but I'd need to look at it in more detail.


### Pass-by-reference
Reference parameters use `&` in front of the parameter name. This means address-of is applied automatically to arguements, and dereferencing is done automatically to the parameter within the function.

This shows 3 ways of updating a variable in the caller:
````
func F(int a)int =
    return a+1
end

proc G(ref int p) =
    ++p^
end

proc H(int &a) =
    ++a
end

# They are each called as follows:
    int x := 100
    x := F(x)
    G(&x)
    H(x)
````
`H` using pass-by-reference is tidiest.
* Reference parameters can't have default value (see next section)
* There are no promotions; the type must match exactly. (If `int` is passed to a function expecting
`real`, it is converted. But with by-reference, the actual expected type is `ref real`; `ref int` won't cut it.)


### Default parameter values

They are specified like this:
````
proc  F(int a, b, c = 0) ...
````
This allows the third arg to be omitted at the call-site, but the compiler will generate the 0 value needed. Calls are always made internally with the full number of arguments.

For complex expressions, any names used, for example `x` in `c = x + 1`, are resolved locally: `x` is whatever is visible at the function definition, not at the call site.

Only trailing parameters can be omitted, for a conventional call not using keyword arguments. So this is not possible:
````
    proc F(int a, b = 0, c) ...
    F(10,,30)
````
### Keyword arguments and Default parameter values

This is being able to name arguments, so that they can be passed in any order:
````
    F(c:100, a:200)
````
This calls the above function. `b` is not provided; that has a default. Positional (regular unnamed parameters) and named can be mixed, but positional must come first:
````
    F(200,c:100)        # same call
    F(c:100, 200)       # not allowed, as it is not known which parameter the 200 is for.
````
Keyword parameters can be applied to FFI functions, even when the definitions in the original language don't use them.


### Multiple function return values
```
    func F(int a, b)int, int =
        return (a*a, b*b)
    end
````
These need a multiple return type, a simple list of types. An explicit `return` keyword is needed and the return values must be in parentheses.
Limitations:
````
    - Only up to three return values
    - Only scalar types such as ints, reals and pointers can be returned
````
Consuming all, some or none of the return values should be allowed via Multiple Assignment:
````
    (x, y) := F(2, 3)       # for my example, x is 4, and y is 9
    x := F(2, 3)            # x is 4; the y is discarded
    F(2, 3)                 # both return values are discarded.
````

(Currently, either all or none of the return values must be used; I'll need to fix that.)


### Block Scopes

These do not exist in M. Each function has only one function-wide scope.

In C, there can an unlimited number of blocks, each with their own private scope, which can be nested so all shadowing other. Plus it has three 'namespaces'. None of that exists here.

### If Statements
General syntax is:
````
if s then       # uses last unit of s for condition
    s
elsif s then    # 0 or more elsif parts
    s
else            # optional (unless whole thing yields a value)
    s
end             # or 'end if', or 'fi'
````
An experimental feaure is to omit that first condition and `then`:
````
if
elsif c1 then
elsif c2 then
....
````
Now, all the conditions use `elsif`, simplifying maintenance (add, insert move, comment etc) as all are the same rank; the first is not a special case. I had first tried this with `case`, but I think this is better.

There's also:
````
unless s then
    s
else            #optional
    s
end
````
Sometimes the logic is more intuitive like else. But there is no `elsif` (or `elsunless`) as I couldn't figure out what it meant.

### Loops
There are a few:

#### Endless Loops
````
do
    s
end
````
#### Repeat N Times
````
to n do
    s
end
````
#### While/Repeat Loops
````
while s [, s] do
    s
end
````
(While has an optional increment. This was left over from a proof of concept I where suggested C used `while` for linked list traversal rather than its for-loop.)

````
repeat
    s
until u
````
#### For Loops Over an Integer Range
````
for i := u to u by u do s end        # full syntax
for i := u downto u by u do s end 
````
The `:= u` can be omitted (starts with 1). The `by u` can be omitted (steps by 1). (In the Algol68 original syntax, even more could be left out to end up with `do` and `to` loops, but here they are independent loops.)


#### For Loops Over a Set of Values
````
for x in u do s end                  #
for i,x in u do s end                # x is value, i is the index
````
`u` can be an array with known bounds, or a slice. But it can also be a range:
````
for i in 1..10 do
for i in A.bounds do
````
Then it will iterate over integers and is an alternative syntax for that. (But reverse interation is not possible.)

Both kinds of for-loop can have an optional `else` part:
````
for x in A do
    if x = n then
        y := n
        exit
    end
else
    println "N not found"
end
````
The `else` part is executed on normal termination only.

There is also an optional condition:
````
for i in 1..10 when i.odd do
````
The body is only executed when the condition is true for that iteration. Here, that means for values
of `1 3 5 7 9`.


### Switch and Case
These statements are similar:

````
switch x           # u
when u,... then    # one or more exprs
    s
when u,... then
    s
else               # optional unless whole returns a value
    s
end

case x
when u,... then
    s
when u,... then
    s
else               # optional
    s
end
````
The control expresion is compared for equality against then 'when' expresions, and the corresponding block is executed when there is a match. There is no 'fallthrough' as in C.

Differences:
* `switch` only works with `int` types; `case` works with any value for which `=` is meaningful.
* `switch` when-values must be compile-time expressions; `case` ones can be runtime expressions
* `switch` notionally does all comparisons in parallel; `case` tests sequentially
* `switch` when-values must be unique; this is not checked for `case`
* `switch` allows a range for any when-value; example: `when 'A'..'Z', `0'..'9'  then`.

### Looping Switch and Case

These are `doswitch` and `docase`. Basically `switch/case` wrapped in a `do-end` loop, but it saves an indent level, and makes the intent clearer.

Loop exiting requires any of `goto exit return stop`

There is also a special version `doswitchu` (I need to work on the name). This uses 'computed goto' for the jumps, and can be more efficient for certain use-cases.

Normal `doswitch` has one dispatch point at the start, with each branch looping back there. But `doswitch` has a dedicated dispatch point after every branch. I believe it's faster because branch prediction works better with multiple jump sites rather than one. 

With `doswitchu`, range checking is not done; the control expression must be on the range of lowest to highest when value, but not outside. Gaps are allowed: control goes to the `else` branch, which becomes mandatory.

### Composite if/switch/case
This is a feature that may raise some eyebrows. It looks something like this; start with any of those, say `if`:
````
if c1 then
elsif c2 then
elsecase x1
when a, b then
when c then
elseswitch x2
when 'A'..'F' then
elsif x2>=128 then
end
````
It allows you to switch from one form to another in a long sequence of assorted tests, without  getting an increasing amount of indentation (the process is linear, not nested) and without needing a stack `end`s at the finish.


### Control Flow statements

These are:
````
<call>                # function calls
goto u
return [u]
stop [u]              # See 'Termination'
exit [n]              # break in C
redoloop [n]          # 
nextloop [n]          # continue in C
````
#### Goto

This works as expected, but the language also has label pointers, allowing arrays of labels, so `goto table[i]` is possible.

An unusual feature (from Algol68) is to be able to omit the `goto`; these lines do the same thing:
````
    goto finish
    finish
````

#### Return Keyword
This can be optional in functions when the last unit yields the return value anyway. But without it, sometimes a simple value can look lonely and without apparent contxt.

But it is needed for early returns, or for returning multiple values.

#### Exit, Redoloop, Nextloop

These are loop controls which work inside all loops, plus `doswitch` and `docase`.

An optional index makes it work with nested loops: 1 (default) is this loop, 2 is the next outer loop and so on.

An index of 0 (`all` is an alias for that) means the outermost loop.

### Conditional Suffixes
All those control flow statements can be made conditional by adding one of these two suffixes:
````
when u
unless u
````
Example
````
    return when p = nil
    showstatus() unless fquiet
````
This is equivalent to just wrapping it in an `if` or `unless` statement, but this tidier and makes the statement more prominent (not indented etc).

### Slices
In brief, a slice is a data type consisting of (pointer, length). On the 64-bit target, it takes up 128 bits.

It can be used for a view into another array or string, or can be used to own its own dynamic but fixed-size (once allocated) data. It's flexible. (A growable array would need a third element, such as C++'s 'vector', but I haven't attempted that.)

A slice definition looks like this:
````
slice[]int S
````
Slices have a lower bound (1 here, but can be anything), but this not stored, since it is fixed at compile-time.

Indexing of slices is always from its lower bound even if it is a view of elements in the middle of some array.

Some examples:
````
    [10]int A := (10,20,30,40,50,60,70,80,90,100)
    slice[]int S := A[3..7]          # View into A

    println S.len                    # will be 5

    for i, x in S do
        println i, x                 # shows 1 30, 2 40, ... 5 70
    end

````
Slices can provide a counted string type:
````
    type string = slice[]char

    ichar str := "ABCDEFGHIJ"
    string T := str[4..7]

    println T
    println T[2..3]
````
(Actually, slices are counted as experimental. There are a lot of interaction possibilies, and they are not used enough to iron out issues. One reason was to keep programs transpilable to C, where the transpiler didn't support them, so they were avoided.) 

### Operator Constants

Writing `(+)`, or any other operator inside brackets, returns an internal ordinal (an integer) representing that operator.

This is no big deal, you could just use `'+'` for example, but it is a nice touch. And in Q, the value is recognised by the language:
`mapss((+), 3, 4)` yields `7`.

### Type Constants
Similarly, writing `(int)` (or `int.type` for simple types), yields an internal ordinal for the type. It can be used like this:
````
    if T = int.type then
````
where `T` has such an ordinal value.

### Type Reflection

There is some use for this even in a static language:
````
    X.type       Returns the type ordinal as used above
    X.typestr    Returns a string such as `"i64"` and so on
````
`X` can be a type or expression. Parentheses may be needed around `X`, when it is a number for example.

If unshare of the result type of an expression, this can display it:
````
    int a, b; real c

    println (a+b*c).typestr           # displays 'r64'
    println "ABCD".typestr            # 'ichar'
````

### Function Tables
M used to generate a table of all functions used in a program. Info about them could be obtained with:
````
n := $getnprocs()           Number of functions in the table (int)
s := $getprocname(i)        Name of i'th function (i is 1..$getnprocs()) (ichar)
a := $getprocaddr(i)        Address of i'th function (ref void)
````
However, the table used to take a lot of space, about 10% of an EXE file. So now only marked functions are put into the table:
````
proc F*=
end

proc G=
end
````
Here, only `F`, marked with `*`, will be present in the table. Normally it is sets of handler functions which are marked like this. Then some init routine will scan them and set up variable handler tables, without needing to maintain them manually.

### Label Pointers and Computed Goto
Example:
````
    static[]ref label jumptable = (L1, L2, L3)
    int i:=2

    goto jumptable[i]
L1: println "ONE"
L2: println "TWO"
L3: println "THREE"
````
Output is TWO then THREE here. This can be used to manually implement 'computed goto'.

This is not needed so much since the `doswitchu` feature exists, so you don't have Fortran-style code with labels and gotos everywhere, but there are a couple of advantages:

If using this in a typical bytecode dispatch loop, the `doswitchu` version may involve two memory access:
````
   doswitchu pc.opcode
````
This fetches the opcode first from bytecode data, then uses that to access the jumptable.

With explicit label pointers, opcode can be replaced by the actual labels. Dispatch is then with `goto pc.labeladdr`. So it can be marginally faster.

The other advantage is that label pointers can be transpiled to C (will need the gcc extensions) while `doswitchu` translates to regular C-style `switch'.

### Macros

These are a recent addition. They are simple and limited compared to C macros. Differences from C macro:

* Macro names are part of the normal system of scoped names (they can be shadowed etc)
* They can be imported and exported (but can't be name-qualified in use)
* They are expanded at a later stage, during name resolution, and involve AST manipulation not tokens
* Macro bodies must be well-formed terms and expressions only, not random bits of syntax
* If parameters are taken, exactly that number must be provided
* Only top-level names can be candidates for macro expansion, so `A` in `A.B.C` could be expanded if it was a macro, but not 'B' even if there was a macro of that name. (This would screw up too much; C would allow B to be expanded!)

Even with a simple implementation, there are complexities with missing macro args with those that might go with an expanded function name. The suggestion is to keep things simple. They tend to be used sparingly (often to work with inline assembly).

Example:
````
global macro pr(a,b) = (a<<16 ior b)
````

### Compiler Variables
These are predefined values such as:
````
$lineno
$linestr
$filename        # etc (boring stuff)
pi               # 3.14...
nil              # A ref void constant with a value of all-zeros
````

### Equivalence

This is a feature I borrowed from Fortran. It's been simplified, but not yet dropped:
````
[4]u16 A
int B @ A
````
It allows variables to exist at the same location in memory.

### Read and Print
These have always been built-in statements. There's quite a lot to say about them probably too much to go into here.

Basic examples:
````
    println a, b, c         # display a, b, c, whatever they are, with a space between

    readln a, b             # wait for a line of input, then read 2 variables from that buffer
    read c                  # and a third from the same buffer
````
`read` mainly works with numbers (I'd have to check the support library).


### Inline Assembly
Multiline form:
````
assem
    mov rax, 1234
    mov rbx, rax
end
````
One-line form:
````
    asm push rax
````

Assembly blocks can return values; if written where some value is expected, then it is assumed the value will be in rax or xmm0.

Assembly syntax is pretty much that of my AA assembler. I will not be documenting that.

Although there is no proper optimiser in the main compiler, some register allocation of variables goes on. But that is disabled when inline assembly is used within a function (as I won't know whether variable X is in a register or in memory, or I could clobber registers used for variables).

But I believe I can fix that (variables accessed in assembly have the address modes adjusted as needed, and I can avoid using non-volatile registers).

### ImportDLL and the FFI

Most languages need an FFI (except C, since 'Foreign' usually means C functions, so it's already on home ground).

Here, FFI functions (I don't support importing variables yet) are defined inside an `importdll` block:

````
importdll msvcrt =
    func puts(ichar)int32
end

importdll $windows =
    func "MessageBoxA"(int32 a = 0, ichar message, caption = "Caption", int32 flags = 0)int32
end
````
The library name is that of an actual one (`msvcrt.dll`, which is specified as an import in the EXE), or a dummy one if it starts with "$". This is used when the DLL name is unknown or unclear, or the set of functions exist across multiple DLLs.

But then the actual DLL must be one of those specified with `linkdll` in the project info of the lead module.

It will search all DLLs until it finds each imported symbol. (The M compiler automatically looks inside `msvcrt gdi32 user32 kernel32` DLLs.)

Notice that the MessageBoxA declaration includes parameter names and default values. This allows the use of keyword parameters even though the original MS function didn't support such a feature:
````
    messageboxa(message: "Hello There")
````

#### The Import Module

Usually such an importdll block is written in a dedicated module that only contains that interface. Each imported name is also exported from the module so that it is accessible to other modules (and I believe to other subprograms), and also without needing to qualify the name.

If `puts` from `msvcrt` is defined inside a module `mclib` for example, then it can optionally be called as `mclib.puts`, not msvcrt.puts. The DLL name is not the owner module.

(Actually, the situation with `puts` is more complex: it *is* defined inside `mclib`, but that is part of a subprogram. Subprograms hide their modules. So the qualified name is `msyslib.puts`.

Here, I could define a short alias, so that it would be `sys.puts`, but fortunately M doesn't need the full name anyway.)

#### Creating Import Modules

For external libraries, then in C the necessary headers are usually provided for you. Other languages including mine have to create bindings in those languages.

For M, it is a huge amount of painstaking work, mitigated a little by being able to automate part using my C compiler to translate them. Once done however, the results are superior. Take the SDL library as an example:

* In C, it comprises 75 header files totalling over 50K lines of code
* If 50 modules each hav3 `#include <sdl.h>`, then those same headers must be processed 50 times for a full build, 2500K lines in all
* A set of bindings for my languages will flatten all those declarations into one module containing an `importdll` block, of under 3K lines
* The 3K line module is only processed once per build, no matter how many modules use it.

(Such flattening could be done in C as well, to result in a single, flat compact header file, which is also going to be easier to manage and locate. But nobody seems to do that.)

### M Implementation and Compiler

There is only one implementation (mine, excluding previous versions) and I like to make it work in a certain way.

The C language likes to distance itself from implementations (as there are so many diverse ones) but I don't need to do that here.

* M is implemented in itself, or a previous version, in a chain going back decades. The original will have used assembly. No other languages were used. So it is 'self-hosted'.

* It is now a 'whole-program' compiler: all source files must be recompiled to build a new version of a program. Here a 'program' means a single binary like one EXE or DLL file.

* I like single-file, self-contained language tools, and `mm.exe`, the M compiler for Windows, is the same. It is currently about 400KB.

* The compiler can directly emit EXE, DLL, OBJ, ASM, MX (private format) PCL (IL) files, or it can run programs without producing an EXE, or (with limitations) interpret them by executing the IL.

* It can also produce a single-source-file amalgamations. This bundles all source+support file for an application into one tidy, readable source file. This can be directly compiled without unpacking. (I tend to use this for simpler backups of the source files).

* It is quite fast

So quite a lot is packed in.

(Much of this is thanks to a new backend that is an independent library, which is also used in my C compiler.

So many of these features apply also to that product, provided the program is a single C source file. Otherwise independent compilation: into .asm files them combining via another product of mine into EXE etc.)

Examples:
````
c:\mx>mm hello                         # Normal compilation
Compiling hello.m to hello.exe

c:\mx>mm -r hello                      # Run from source (as native code)
Compiling hello.m to hello.(run)
Hello World!

c:\mx>mm -i hello                      # Run from source (interpret IL)
Compiling hello.m to hello.(int)
Hello World!

c:\mx>mm -a hello && aa -r hello       # Compile to ASM and run that from source
Compiling hello.m to hello.asm         # (The 'aa' assembler use the IL backend too)
Assembling hello.asm to hello.(run)
Hello World!

c:\mx>mm -r mm -r hello                # Build and run itself
Compiling mm.m to mm.(run)
Compiling hello.m to hello.(run)
Hello World! 10:55:56
````
By copying the lead module `mm.m` to `ms.m` and compiling to `ms.exe`, that will apply `-r` and `-q` (for less verbose) automatically. Now it is possible to test self-hosting properly:
````
c:\mx>tim ms ms ms ms ms ms ms ms ms ms ms ms ms ms ms ms hello
Hello World!
Time: 0.990
````
This compiled and ran 15 successive generations of itself in memory, then was applied to `hello.m`. It took one second, on a low-spec PC. (If counting, the first `ms` is the one already built!)

So, it is nippy. But this is necessary when committed to whole-program builds, since it will scale better on larger projects.

#### Applying M to Other Targets

This is unlikely. The changes need to make M work on a small 8 or 16 bit device have recently been considered.

(That would only be as a cross-compiler; I can't write compilers any more that run in 64KB!)

But, this was when redesigning the IL implementing the backend, to help clear of it of various assumptions that had been accrued.

So, it is a possibility *if* I decided on such a project. The changes to M will be small (eg. `int` may be `i16` again; auto-promotions may be discarded); the
hard work is in the backend.

### C Features that are not in M

* **VLAs**, including variably modified types or whatever they are called. My view is that the feature is too complicated, and unsuited even for C

* **Compound Literals.** This is the construct `(T){...}`. I do have such constructs, which are written as `(...)` with no need for the cast, but they have limitations in where they can be used, and whether they allow non-compile-time expressions

* **Designated Initialisers** Here I think keyword arguments, a related feature, would have been a more useful for C. (I have 'designated initialisers' in my Q language; I've never used them.)

* **Octal Literals**

* **Variadic Functions**. M can call such functions via its FFI, but cannot define them itself.

There had been a proposal like this:
````
proc F(int a, b, c ...) =           # Note no comma after 'c'
````
where `c` represented any number of arguments of the *same type*. There was some way of determining how many and indexing them. Then I realised it was just syntactic sugar for:
````
proc F(int a, b, slice[]int c) =
````
At the call-site, it would need extra parentheses, so not quite as sweet.
````
  F(10, 20, (30, 40, 50))
````
(This is not working ATM; see Compound Literals above.)

* **New C23 Features**

I don't know what most of them are. I have deliberately kept M at a certain level, certainly regarding its type system. For anything higher level, I use my next language up.

### Miscellaneous

Although I did say this is not a formal reference, there are a few facts that could do with being listed, necessary if someone wants to try writing code:

* To call a function with args needs to be `F()`, like C, not `F` (which similarly yields a reference)
* C's `const` does not exist in the language. My `let` was a weak attempt at it. M's `const` defines names with a compile-time expression only
* If a variable is initialised like this at any point in a function `int a := 0`, the scope of `a` is function-wide, but the assignment happens right here. `a` can also be assigned to before the declaration!
* Bitwise logic ops are called `iand ior ixor inot`; the binary ones have the same precedence as `+ -`
* Logic ops are called `and or not istrue`

This will be added to as needed...
