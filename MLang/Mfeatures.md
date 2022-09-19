## M/Q Languages Reference

M is my systems programming language, Q is my dynamic scripting language, but they share much of the same syntax and the same features, so this reference can serve for both. Most topics apply to both languages unless it specifies one or the other.

However, while this document describes most of the features of M, including those common to Q, special features of Q are described elsewhere.

It is not a formal reference, mainly it documents the current state of the languages for my own purposes, But hopefully it can give enough of a picture for anyone else too.

Although languages are quite low level for their class, they are quite rich in features; there's quite a lot to describe!

### Overview

* Algol68-inspired syntax without braces and without begin-end either
* Line-oriented and largely semicolon-free
* Case-insenstive
* Module scheme
* Out-of-order definitions throughout
* Defaults to 64-bit integers, constants, and evaluation
* Expression based (expressions and statements are interchangeable)
* Built-in `print` and `read` statements
* Very fast, single-file and self-contained whole-program compiler (M)
* Single-file interpreter and very fast bytecode compiler (Q)
* Can create compilable one-file source amalgamations of projects
* Does not need a build system like `make` (M)

### Some Terms

**Program** For M, this is a single binary executable or shared library. That is, one EXE, DLL or ML file. In both languages, a Program can be thought of as a collection of Modules (but see Subprograms below).

**Module** A single source file. (Note source file names, excluding paths and extensions, must be valid identifiers.)

**Lead Module** Out of the modules that comprise the Program, this is the one submitted to the compiler, and which needs to contain the `main` entry point (but see Header Module)

**Main Module** The one where the entry point `main` resides. It will either be the Lead Module, or the next one if that contains header stuff only.

**Header Block** A set of directives describing the program structure, usually modules, subprograms, and any imported DLL/ML libraries. This must go at the top of the Lead Module. Not needed for simple one-module programs.

**Header Module** When the Lead Module only contains a Header Block and no code or data, it forms a Header Module. (In that case, the `main` entry point must be in the first module listed.)

**Subprogram** A Subprogram is a related collection of Modules, and a Program is technically a collection of Subprograms. See docs for Modules. In practice, there is usually just one explicit Subprogram (one set of modules), plus the standard library forming another, which does not need specifying. 

**Unit** A single expression or statement (these are usually interchangeable, although most statements don't return a value other than `void`)

**Sunit** A series of Units separated by semicolons (but rarely seen as newlines work as separators too). This is sometimes also refered to as a Block.

### Syntax

An Algol-style syntax is used, but without begin-end. Algol68 was a particular inspiration. See [Examples](Examples).

#### Identifiers

These use `A-Z a-z 0-9 _ $`, and can't start with a digit.

The base filename of a source file must also be a valid identifier (not all filenames are). Because that name becomes the module name: filename "\path\bignum.m" will become module `bignum`.

#### Case Insensitive

Identifiers, and keywords, type names and other reserved words are case-insensitive.

To access case-sensitive names used with external libraries, there are two schemes. One is to declare the name as a string, but can then be used in any case. The other is to use the \` prefix:
```
    proc "MessageBoxA" (...)
    proc `exit(int32)

    messageboxa(...)           # can use any case
    `exit(1)                   # must use `
```
Backtick has other benefits:

* It preserves case in a name
* It allows the use of reserved works in an identifier (like the `exit` above, which is a keyword in M)


#### Semicolons

While semicolons are ostensibly required to separate units, in practice these are rare in source code. This is because newlines are converted to semicolons, except when:

* A line-continuation character `\` is used at the end of the line
* The line clearly continues onto the next because it ends with one of:
       `( [ ,` (opening brackets or comma) or a binary operator

Their main use is separating units on the same line. Extra semicolons are generally well tolerated as these rules mean they can occur in unexpected places, such as after `then` here:

    if cond then       # ";" inserted here

#### Comments

Single-line comments start with `!` until end-of-line (`!` came from the DEC Fortran and Algol I used in the late 70s.)

There are no block comments, which I consider an editor function. Nested line comments using `!` are not a problem.

`#` has also just been introced for line comments too, but there is a clash with doc-strings (see below) when they are nested; this needs looking at.

#### Doc-strings

Doc-strings are comments starting with `##` at the beginning of a line. Such comments just before and/or within a function, are associated with that function.

The `-docs` compiler option will write out a text file listing all such functions, with their signatures (not ready) and their doc-strings.

#### Character Set

This is UTF8, but the language uses only ASCII within reserved words and identifiers. Non-ASCII characters are allowed in comments, string constants and char constants.

(However a char constant occupying more than 1 byte will have `u64` type rather than `char`.)

Apart from this, the language does not itself deal with Unicode at all. That is all up to the editors used to create source code, and the libraries used to process strings.

#### Block Endings

You will notice in example code that `end` and so on are used to close blocks. Actually M/Q are quite flexible here, in allowing anyone to choose their preferred style.

The block ending can be any of (example shown for `if` statement):

    end                 # Unchecked
    end if              # Will check the keyword matches
    endif               # Optional space

Some statements can have alternate block endings:

* Any loops using `do` (all of them except `repeat`) can also close with `end do` or `enddo`, or just `od`
* `if` statements can close with `fi`
* `case` statements can close with `esac`

(`od fi esac` are the original Algol68 endings.)


### Program Entry Point
This is the `main` function, which is always exported (ie. no `global` or `export` needed.) `main` takes no parameters.

This needs to be present in the Main Module (see Terms).

M will insert a call to a start-up routine in M's runtime module, to set up command-line parameters etc as global variables (`ncmdparams`, and `cmdparams`, the latter being an array of strings). It will also insert calls to `start()` functions in every module that has one (see below), before executing user code in `main`.

So given this `main` routine:
```
proc main =
   <my code>
end
```
Special code is inserted by the compiler so that it does this:
```
proc main =
   msys.start()              # unless -nosys option used
   <call any start() routines in other modules>
   <my code>
   stop 0
end
```
`main` can be present in other modules, but only the one in the lead module becomes the entry point. Other `main` routines can be called (they will need qualifiers, such as `B.main()`), but are otherwise ignored, unless that module is compiled directly to form its own program.

#### Start() Functions
If present in a module, it will be called automatically by start-up code. No `global` attribute needed.

The ordering of such calls when `start()` is present in multiple modules can be important. That order follows that of the `module` directives in the Header, except that the one in msys.m (a standard library) is done first, and any in the Main Module itself is done last.


#### Program Exit Point
Programs can be terminated by running into the end of the `main` function, with return code 0. For a different return code, or to exit from anywhere in the program, use:
````
    stop N
````
Note that `main` is a proc not a function; it has no return value itself. The compiler inserts `stop 0` at the end, so that code actually runs into that.


### Data Types (M)

M's types are basic, and fixed length, like C's:

Type  |   Bit-width/Length   |    UA | Notes 
--- | --- | --- | ---
void  |  |   |  Pointer target only
-- |  |  | 
int   |     `8 16 32 64 ---`    |    |  Signed integers
word   |    `8 16 32 64 ---` |     |    Unsigned integers
char    |   `8 -- -- 64 ---`  |   |  Character
bool     |  `8  --  -- 64 ---`  |  |   Boolean
real     |  `-  -- 32 64 ---` |    |  Float
-- |  |  | 
ref     |   `-  --  -- 64 ---` |   A  |  Pointer to T 
slice   |   `-  --  --  -- 128` |   A |   Slice of T (pointer and length)
range    |  `-  --  --  -- 128`  |  |   Internal
-- |  |  | 
record  |   Fixed size       |  U  |  Record of mixed T
array   |   Fixed length | A | Array of T
array   |   Unbounded | A | Pointer target only
-- |  |  | 
proc   |                   |  A  |  Pointer target only (func signature)
label   |                 |   A  |  Pointer target only
-- |  |  | 
tuple  |    Fixed length   |    A  |  Internal
type   |              |      |       Internal

* U in the UA column means this is a named user type; each instance be for a different record
* A in the UA column means this is an anonymous type; each instance will be a different pointer, array, func etc
* Some types are only allowed as pointer targets
* Non-float numeric types shorter than 64 bits are 'storage' types, suitable for optimising memory usage and record layouts

Q has its own higher level types (flex strings and arrays for example), but also supports the above 'T' types because they are used in APIs, and are needed when working with M data. Q is described elsewhere.

#### Numeric Types

The official set of types are these (either long or short forms can be used):
````
    int8     i8      Signed integers
    int16    i16
    int32    i32
    int64    i64

    word8    u8      Unsigned integers
    word16   u16
    word32   u32
    word64   u64

    real32   r32     Floating point
    real64   r64

    char             Character type is a thin wrapper around u8/byte/word8
    char64
    bool8
    bool64
````    
Common aliases:
````
    int    =  int64
    word   =  word64
    real   =  real64
    byte   =  word8
    bool   =  bool64
````
Types that varied between 32/64-bit targets, and 128-bit types, have been dropped from previous M versions. (It no longer targets 32-bits, and  128-bytes were just not that useful, only for implementing 128-bit types!)

Most of these also work in Q, when a fixed 'pack' type is required.

#### Arrays (M)
These are defined like this:

    [10]int A                   Fixed length
    [3,4]int B                  2D array ([3][4] would also work)
    []int C := (10, 20, 30)     Array length set by init data

* Indexing is by using `A[i]` or `B[i,j]` (or `B[i][j]` is you prefer C-style)
* Indexing starts from 1 unless specified otherwise; see below
* Use `A.len` for the length; `A.lwb` for the lower bound; `A.upb` for the upper bound; or `A.bounds` in for-loops only
* Use `A[$]` as an alternative to `A[A.upb]`
* Arrays are manipulated by value, and can be passed and returned notionally by value, except that the Win64 ABI implements that with references. No actual copying is done, which is something to be aware of (see Block Handling)
* Arrays can only be initialised with the exact number of elements, not fewer like C
* To initialise an array to zero:
  * Define as static (zeroed on program start only)
  * Use `[10]int A := empty`
  * Use `clear A`
* Dynamic arrays must be created manually with pointers and allocations


##### Array Bounds
The general pattern is:

    []int A             # unbounded, or set with init data; 1-based
    [0:]int A           # The same, but starting from 0 (any lower bound is possible)
    [10]int A           # Length 10, indexed 1..10 inclusive
    [0:10]int A         # Length 10, indexed 0..9
    [0..9]int A         # Length 10, indexed 0..9


#### Slices (M)
A slice is a 16-byte descriptor consisting of a pointer and a length. One can be declared like this:

    slice[]int S       # Lower bound 1; length and data undefined

Slices are indexed from 1; this can be changed using `slice[0:]int` for example, but that's uncommon. When creating a slice *value* from an array, for example `A[3..6]`, the slice will have a lower bound of 1 with no override, and the length will be 4.

Slices can be initialised, or slice values created, in various ways:

    S := A[3..6]          The example above
    S := (P, N)           Construct directly from a pointer and length
    S := A                Slice to entire array
    S := S[2..4]          Slice to a slice

Those values can also be directly passed to functions that expect slices.

with `slice[]char`, then counted strings and substrings become available:

    slice[]char T := "ABC"          Create a 3-char slice
    type string = slice[]char
    string U := "DEF"               Pretend M has proper string types

Lengths and bounds can be obtained just as with normal arrays.

#### Pointers (M)
These are defined with `ref`:

    ref int P

and dereferenced with `^`:

    print P

The `^` dereference can omitted for `A^[i], P^.m, F^(x)`, so that you just write `A[i], P.m, F(x)`. This loses some transparency, but it makes for clearer code, and makes some code identical to the Q equivalent where pointers are used much less.

Pointers also exist to functions, and to labels.


##### & Address-of Operator (M)
This works as it does in C: `&A` yields the address of `A`. If `A` has type `T`, then `&A` will have type `ref T`.

`&` can also be applied to:

    &F          F is function, yields a function pointer
    F           It yields a functon pointer even without &
    &L          L is label, yields a label pointer


### Constants
#### Int Constants

These are the literal values:

    0 to 18446744073709551615         (0 to 2**64-1)
    18446744073709551616+             (Q only)

These have the following types:

    0      to 2**63-1         int64 (see note)
    2**63  to 2**64-1         word64
    2**64  to ...             M:overflow; Q: creates a Bignum constant

Previously in M, bigger numbers resulted in `i128`, `u128` types, or a tentative Bignum type, but those were never added.

In Q only, adding an `L` suffix, as in `1234L` forces a Bignum type to be created.

Note: in some contexts, when an int64 constant is combined with a word64 operand, then the constant is considered to be word64 too. The ensures the whole operation is unsigned. Otherwise, because int64 is dominant, in an operation like this:

    word64 a = 0xFFFF'FFFF'FFFF'FFFF
    if a > 5 then ...

The comparison is performed using int64, which means comparing -1 with 5, giving the wrong result.

#### Separators

Real and Integer constants can include any _ or ' separators between digits (underscore or single quote).

#### Constant Factors

These are multipliers applied to integer constants:

    3 thousand             # value is 3'000
    3 million              # value is 3'000'000
    3 billion              # value is 3'000'000'000
    3 million billion      # value is 3'000'000'000'000'000

There used to be more such factors, such as `3 msec` (0.003), `3 km, 3000 m` (both 3000), and could applied to real constants too. But these are all that are left while I think about making them a proper, configurable language feature.

(These names live in a separate namespace so that can still be used as normal identifiers.)

#### Bases

Bases of 2-16 for both integer and real constants have been dropped. Now only binary (`2x100`), decimal (`100`), and hex (`0x100`) are supported for integer constants. For reals, binary and hex are TODO.

Printing of integers in any base is still supported, and probably a lot more useful, eg. `print a:"x3"` displays `a` in base 3.

#### Real Constants
These are written as follows:

    1.234
    1e6
    0.5
    .6        # illegal; use at least '0' before a decimal point

#### Char Constants
Char constants look like this:
````
    'A'                            char, with value 65
    'AB' up to 'ABCDEFGH'          u64 (not char64)
````

Note that `'A'` has type `char`, not `int`, unless it is multibyte.

`'ABC'` has the leftmost character (`A` here), in the least significant position in the word. The intention is that when stored in memory, it has the same layout as the string `"ABC"`. (However this is only valid for little-endian; I've never used big-endian.)

Unused parts of a word will be zeros.

#### String Constants

These are the usual "ABC", and are zero-terminated. Such a string in M has type `ref char` or `ichar`; in Q it has type `stringz`; here I need to bring these into line.

##### String Escape Codes
These are very similar to C, most escape codes in string constants start with a `"\"` followed by a letter code (case insensitive):

    \a           Char 7 (bell/alert)
    \b           Char 8 (backspace)
    \c \r        Char 13 (carriage return)
    \e           Char 26 (etx?)
    \f           Char 12 (formfeed)
    \l \n        Char 10 (line feed, or Linux newline)
    \s           Char 27 (eScape)
    \t           Char 9 (tab)
    \u           Reserved for Unicode specifier
    \v           Char 11 (vertical tab)
    \w           Char 13, 10 (Windows CR/LF sequence)
    \xdd         Two-digit hex code gives char
    \y           Char 16 (backwards tab in my apps)
    \z \0        Char 0 (embedded zero; will need care)
    \" \q        Char '"' (one double quote)
    ""           Char '"' (one double quote)
    \\           Char "\" (backslash)
    \'           Char "'" (single quote)


##### Raw Strings

Raw strings ignore escape codes, so that `"\"` is a normal character:

    F"C:\demo\mm.exe"

It uses an F prefix, since those `"\"` are typically used for file paths on Windows.

##### Initialising Char Arrays (M)
The type system demands that these are initialised like this:

    []char S = ('A', 'B', 'C', 0)

which is inconvenient. C used a bodge in the type system to get around this and allow `"ABC"`; M uses two special string constants:

    []char S = a"ABC"         # Same as ('A', 'B', 'C')
    []char S = z"ABC"         # Same as ('A', 'B', 'C', 0)

A-strings (it can be `a` or `A`) are not zero-terminated; Z-strings are.

#### Pointer Constants
There is only one, called `nil`, which on all targets supported, has a 64-bit value of all-zeros.


### Named Constants

This is applying a name to an integer, real or string constant, or a compile-time expression:

    const A = 100           # type is inferred; A mean 100
    const B = A + 1         # B means 101
    const real C = A + B    # C means 201.0
    const string S = "ABC"

Such named constants have these attributes:

* They are values, not objects
* They do not have locations in memory, or not accessible by user code
* They don't have an address; & can't be applied
* There values can't be modified
* They can't be assigned to, or passed as by-reference parameters

Real constants may need to be reside in a memory location, but this is not accessible to user-code. Integers are usually immediate operands.

Strings will also blur the line, since the memory is accessible; however the compiler (M) ought to put them into read-only memory, but it doesn't yet. In Q, string constants have an immutable flag set.

### Compile-Time Expressions

This is any expression involving:

* Constant values (sometimes called literals)
* Named constants, which should always reduce to a compile-time value (not Q's Bignum constants)
* Enumeration values
* The results of any operations known at compile-time such ``A.bytes`

It should always reduce down to a single integer or float value.

They are required in these contexts (all integers here):

* Array bounds in M; pack-array user-type bounds in Q
* Switch values
* Initialisation data for static objects etc

### Enumerations
The simple enum feature: `enum (red, green, blue)` has been removed. The equivalent is now:
````
    enumdata =
        red,
        green,
        blue
    end
````

`red, green, blue` have values `1, 2, 3` respectively. But `enumdata` is normally used like the `tabledata` feature below, for example:
````
    enumdata = []ichar colournames, []u32 colourvalues =
        (red,     $,    0xFF'00'00),
        (green,   $,    0x00'FF'00),
        (blue,    $,    0x00'00'FF),            # trailing commas are OK
    end
````
It defines a set of enumerations, and a corresponding parallel set of arrays at the same time.

* `$` is a device which returns the last-created enum name as a string, eg. `"red"`, to avoid having to repeat them all as strings. (I know, this should be done automatically, but this crude approach works)
* For 0-based arrays, define the array bounds using `[0:]`, but also the first enum value must be set to zero: `red = 0`

Older M versions allowed an umbrella type to be applied:
````
    enumdata(colours) []ichar colournames, []u32 colourvalues =
    ....
````
But this did very little, as proper enum types are not supported. All it did was to put the enum names into a namespace, so that they had to be written as `colours.red` etc. This feature was never used, so is temporarily unavailable. (A proper type system would rarely need those qualifiers, but as is clear, M likes to keep that part simple.)

### Variables
These are normally declared like this (M):

    int a, b:=100, c
    [3]int d := (10, 20, 30), e
    ref int p, q

All variables in a list share the same type. M actually has keywords to introduce variables, but these are little used because they're extra clutter:

    var int a, b
    mut int c, d           # var/mut are interchangeable

The advantage of using `var` is that the type can be specified *after* the name, as seems to be the fashion:

    var a:int:=10, b:=20   # All variables must share the same type; init values follow the type

Another would be that the type can be inferred, but this is not yet implemented (the type becomes the tentative `auto`):

    var a:=30

One more is that it can be replaced with `let`:

    let int a := 40        # initialisation is necessary

But this is mostly for show at the minute; read-only controls are poorly developed. Also, because `let` variables must be initialised immediately, they often must be declared in-place, but I prefer them at the top of a function.


#### Static Variables

Variables at file scope will be 'static'. Inside a function, they will need a `static` attribute: `static int count`.

* Static variables have a fixed, absolute address (ie. not on the stack frame or heap)
* They have an initial value of all-zeros (M) or `void` (Q) unless initialised
* The initialisation must be done with `=` not `:=`, as it is a compile-time identity (M)
* Initialisation data must be a compile-time expression or, when pointers are involved, load-time (M)
* Inside a fuction, such variables keep their values between calls


### Clearing Values (M)

Static arrays and records will be all-zeros unless initialised. To set others to all-zeros, without needing to initialise every element as M requires, use either of:

    [1000]int A := empty, B, C          # M only
    clear B
    C := empty

I added these three methods to see which one worked best. I mainly use `clear`, but decided to keep the other two in.

### Read-only Variables

These are declared with `let` as described above. As I said, this is more for documentation as the language does not handle this fully.

Some variables will use `let` implicitly:

* For-loop indices, which are not declared elsewhere, will be read-only
* The data arrays of `enumdata` and `tabledata` are planned to be read-only too

### User-Defined Types (M)

M has:

    type T = [4]byte
    type U = [4]byte

which always creates an alias for a type, not a new, distinct type. So `T` and `U` and `[4]byte` are all the same, compatible type.

The only true unique type is created with `record` (and in Q, when a type defines a `struct`).

#### Records (M, and Packed Records or Structs in Q)

These are the only true user-defined types. They are be defined in two ways:
````
    record date = (int d, m, y)
    record date =
        int d, m, y
    end
````
The first form is more suited to one line, but either can be written across one line or many.

Records can contain other things besides those fields, which only exist when an instance of a record is created (eg. by defining variables of that record):

* Named constants
* Actual functions rather than just function pointers (something called 'methods', but I still distinguish between functions and procs)
* Type aliases, nested records and enumerations (to be checked...)
* Not included (yet) are static variables, of which there would be one instance belong to the type; not per instance

This does make them look a little like classes, but there are no real OOP features; it's just a way of encapsulting things, or they can be used to create a namespace.

##### Layout Control
I've introduced `struct/union` - C terms, but used here only for this purpose - to control layout of fields:
```
    record R =
        union
           int32 a
           int32 b
           struct
              byte f,g,h,i
           end
        end
    end
```
This record is only 4 bytes in size, because the space is shared: `a b f` are all at offset 0. In the past I used `@` to share space in the record. This still works; the same example would be:
```
    record R =
        int32 a
        int32 b @ a
        byte  f @ a
        byte  g @ a+1    # always uses byte offset
        byte  h @ a+2
        byte  i @ a+3
   end
```

#### Field Alignment
The compiler does not automatically insert padding to give ideal alignment to fields. This must be done manually.

However, there is a feature used to match the layout of external structs which usually follow C rules. This can be used to emulate such alignment:

    record rec = $caligned
        byte a
        int64 b
    end

Now, the size is 16 bytes, and `b` starts at offset +8. Without `$caligned`, the overall size would be 9 bytes, and `b` starts at offset +1.

### Tabledata
`tabledata` is used to define parallel data arrays. Older versions did the job of `enumdata` too (using `tabledata()`), but those have been split. It works now like this:
```
tabledata []ichar names, []int values =
    ("one",   100),
    ("two",   200),
    ("three", 300),
end
```
This defines and initialises an array of strings, and a corresponding one of bytes, when it is better to have them separate, but it can be any combination of arrays. It simplifies maintenance, as entries can be added, deleted or moved very easily. Allowing a trailing comma on the last entry facilitates this.


### Functions
Only regular functions exist. No lambdas or anything in any way exotic. All functions in the program (and imported from outside) will be known to the compiler.

#### Func and Proc
Q doesn't usually declare parameter or return types. However, the examples will work on both since Q allows type hints.

Functions are firmly split into 'functions' and 'procedures':
```
    func F(int a, b)int =          # Always returns a value
        a+b
    end

    proc P(int a, b) =             # Never returns a value
        print a+b
    end
```

Options:
* `function` can be used in place of `func`, and `procedure` in place of `proc`
* The function body is an Sunit terminated by `end` (or `end proc` etc).
* A function return type can be written as `T` like above or `:T` or `=> T`
* For functions with no parameters (and `()` is not used), it must be `:T` or `=> T`

There is a shorter form for functions:
```
    fun F(int a,b)int = a+b
```
where the body is a single Unit, which works better on a single line. For procedures, macros can sometimes be used for a short form, as their bodies are always a single unit anyway:
```
macro P(a, b) = print a+b
```


#### Return Values

This only applies to actual functions. When the body is an Sunit, the value returned is that of the last Unit. No explicit `Return` is needed, but is harmless if used and can make it clearer:
```
func fib(int n)int=
    if n<3 then
        1
    else 
        fib(n-1)+fib(n-2)
    fi
end
```
An explicit `Return` is needed for early returns, or when they are conditional (`return 0 when p=nil`), or for multiple return values.


#### Multiple Return Values (M)
Up to 3 return values are allowed, which must all have scalar types:
```
func sumprod(int a,b) => int, int =
    return (a+b, a*b)
end
```
The list of return types is not parenthesised; the return values must be. (Possibly something else to fix as well as the need to write `return`.)

The return values can be consumed like this:
```
(x, y) := sumprod(a, b)        # consume both
    x  := sumprod(a, b)        # discard the second
          sumprod(a, b)        # discard both
```

#### Default Parameters
Function arguments can be omitted, but only trailing ones (instead of `f(a, b, c)` , you can use `f(a, b)` but not `f(a, ,c)`).

Such optional parameters need a default value specified:
```
func F(int a, b=10, c=20) ...
```
This can be called as any of `F(x) F(x, y) F(x, y, z)`. The first two are equivalent to `F(x, 10, 20) F(x, y, 30)`, so all parameters are always passed, but the compiler will fill in the values.

* The default argument expression should be kept simple
* It cannot refer to other parameters
* Although evaluated at the call-site, the scope rules used to resolve names are those at the function definition. (Hence the suggestion to keep it simple.)

In Q, optional parameters can also be specified like this: func F(a, ?b, ?c) with no specific value. Missing values are passed as Void values, which can be tested within the callee.

#### Keyword Parameters
Default parameters are a necessity for keyword parameters, and one of their advantages in able to omit some arguments (the other is being able to pass arguments in any order, that is, not needing to know the order).

The example F function above can be called as:

    F(a, b:y)
    F(a:x)
    F(z:c, a:x)
    F(y:b, z:c)     # not allowed, `b` is not optional
    F(y:b, a)       # not allowed, positional arg follows keyword arg
    F(x, y:b, y:c)  # not allowed, same arg is repeated


#### Reference Parameters
M passes parameters by-value, but by-reference is possible using `&`:
```
proc F(int a, &b) =         # a is by-value; b is by-reference
    a := 10                 # modifies local a
    b := 20                 # modifies caller's data
end
...
int x, y
F(x, y)                     # passes x by-value; applies & to y
```
In Q, it's a little more elaborate: simple types are passed by value, complex ones use a reference anyway. So the latter can already be modified by a caller, if they are mutable.

But they can *only* be modified; it is not possible to completely replace the caller's variable by assigning to it in the callee. That will need a proper reference parameter, also denoted with &.

#### Function Pointers
There are no proper first class functions in either language. But there are function pointers, or function references:
```
    proc F = println "Hello"

    P := F                 # P is reference to F
    P()                    # calls F indirectly, display Hello
```
* In M, P needs to be defined as a suitable function pointer (`ref proc P` for my example), to ensure correct type checking
* But an omission means M can't correctly match P with signature of F; it may be necessary to use `P := cast(F)`

In Q:
* Function references actually refer to a symbol table entry. (Print P will show the name of the function; in M it's the address)
* With function references, default and keyword parameters are not available, as these need to be dealt with at compile-time


### Assignment
This is done with `:=`:

    A := B

`:=` can return a value, so that assignments can appear in expressions, and they can be chained:

    A := B := C                 # equivalent to A := (B := C)

Because what's returned has the value and type of the LHS, it is suggested all terms in a chained assignment have the same type, to avoid surprises.

#### Multiple Assignment
There are two kinds:

     (A, B) := (C, D)

This first kind is equivalent to: `T1:=C; T2:=D; A:=T1; B:=T2`, which allows this, only possible with intermediate values:

     (A, B) := (B, A)       # exchange values

The other kind is:

     (A, B) := F()

when the RHS is a function returning multiple values; see above.

One restriction at the minute is that all types must match precisely (M).

### Properties
X is an expression, but most of these also work with X as a type:

```
X.lwb         Inclusive lower bound of array or slice.
              In Q, also of string, list, range, record, most things that are not scalars
X.upb         Inclusive upper bound
X.len         Length (number of elements, not byte-size)
X.bounds      X.lwb..X.upb (in M, used in for-loops; in Q, returns a Range type)

X.bytes       Object size in bytes
X.bitwidth    For scalar types, number of bits
X.min         Minimum value of X's type (must be integer)
X.max         Maximum value of X's type (must be integer)
```

Lots more are listed in Q docs.

### Binary Operators


Operator | Precedence | Description
--- | --- | ---
`**` | 8| Raise to power (right to left precedence)
-- | |
`*` | 7 |Multiply
`/` |7 | M:float/integer divide; Q: float divide
`%` |7 |Integer divide
`rem` |7 | Integer remainder (modulo)
`<<` | 7 | Shift bits left
`>>` | 7 | Shift bits right
-- | |
`+` |6 | Add
`-` | 6 | Subtract
`iand` | 6 | Bitwise And
`ior` | 6 | Bitwise Or
`ixor` | 6 | Bitwise Xor
`min` | 6 | Minimum
`max` | 6 | Maximum
-- | |
`..` | 5 | M: range syntax; Q: construct range value
-- | |
`=` | 4 | Equals
`<>` | 4 | Not equals
`<` | 4 | Less than
`>` | 4 | Greater then
`<=` | 4 | Less than or equal
`>=` | 4 | Greater than or equal
`in` | 4 | M: see range/set constructs; Q: inclusion operator
`notin` | 4 | Also `not in`; opposite of `in`
-- | |
`and` | 3 | Logical And (short circuit operators used in conditional exprs)
-- | |
`or` | 2 | Logical Or
-- | |
`:=` |1 | Assign (right to left precedence)

* Precedence level 8 is the highest (binds most tightly); the lowest is 1
* `..` is just syntax in M, but is parsed like an operator. In Q, it returns an actual Range value
* Most of these work in infix form (`A op B`). Some also work with function-like syntax: `min(A, B)` and `max(A, B)`.
* A few don't have infix forms, and are used like this:
```
fmod(A, B)
atan2(A, B)
clamp(X, A, B)      # 3-way operator equivalent to min(max(X, A), B)
```
#### Augmented assignment
Many operators can be used with `:=` for augmented assignments:
```
a +:= b         # roughly equivalent to a := a + b, but a is evaluated once
a -:= b
a *:= b
a /:= b
a %:= b
a rem:= b       # check this one
a <<:= b
a >>:= b
a iand:= b
a ior:= b
a ixor:= b
a min:= b
a max:= b
a and:= b
a or:= b
```
Unusually, this is also allowed for some unary operators:
```
-:= a          # a := -a
abs:= a
inot:= a
not:= a
istrue:= a
```

Augmented assignments do not return values as they do in C. This was considered too confusing in expressions like `A +:= B -:= C` when all are of mixed types.

### Unary Operators

    +         # Plus, usually a no-op
    -         # Negate
    inot      # Bitwise Not
    abs       # Absolute value
    
    ++        # increment (used as prefix or postfix)
    --        # decrement (used as prefix or postfix)

    sqr       # square (x*x)
    sqrt      # square root
    sign      # sign(x) yields -1, 0, -1 for x<0, x=0, x>0
    sin       # Maths functions are built-in operators
    cos
    tan
    asin
    acos
    atan
    ln
    lg
    log
    exp
    round
    floor
    ceil
    fract

### Chained Compares
The compare operators are `=, <>, <, <=, >=` and `>`.

When combined like this:

    if a = b = c

then that means:

    if a = b and b = c

Other examples:

    if a <= b < c            # a <= b and b < c
    if a > b < c = d         # if a > b and b < c and c <= d
    
It is recommended however that if any angle brackets are involved, they all point the same way to make them easier to understand.

(Middle terms such as `b` in `a==b==c` may be evaluated once or twice depending on compiler. Try and avoid side-effects here just in case.)

If you need to use the `1/0` or `true/false` return value of `a=b`, then break it up using parentheses:

    if (a = b) = (c = d)

### Piping Operator
This is experimental syntax in M:

    a -> f -> g

is equivalent to:

    f(g(a))

But I haven't used it yet, and haven't figured out how to deal with functions that have extra parameters.

### Dot Operator
This is used both for member selection, and selecting names within namespaces. The latter is done at compile-time, the former at runtime:

    M.a            # M is a module (or subprogram; see module scheme docs)
    R.b            # R is a record: access elements such as named constants, enums, functions, but not members
                   # which only exist within instances of R
    F.b            # F is a function; it is possible to access local names within F, including static variables,
                   # but not normally local stack-frame variables which can only be accessed from within F

    X.m            # X is an instance of a record, so access a conventional field

The last can also be used for calling methods (functions defined inside the record): `X.f()` or `X.g(Y)`, but this is not a OOP language so those are poorly developed.
        
### Swap
Any two fixed size compatible values can be exchanged like this:

    swap(a, b)

This is more efficient that doing it via `(a,b):=(b,a)`, as terms are written once with less chance of error.

(I will need check that this works correctly for M's block types: arrays and records.)

### Promotions

Both languages work internally with `i64/u64` integer types when evaluating expressions. So narrower integer types are widened to `i64`, `u64` or `char64`.

(Q also widens `real32` floats to `real64`, but M works directly with either.)

This usually applies to any term of an expression, except for terms like `X.type`, `X.typestr`, `X.bytes`, `X.bitwidth` where the non-promoted type of `X` is used.

### Mixed Sign Arithmetic

All integer types are widened to 64 bits for arithmetic. Then the rules are:

* At least one operand is i64: use i64
* Otherwise: use u64

`char64` is treated as `u64` for this purpose.

### Type Conversion and Type Punning
Simple type conversions are written like this:

    int(x)

although it has to be a conversion that is allowed. However, an ambiguity in the current syntax means that elaborate types (more than one token is the simple rule, although `ref T` is usually OK) have to use this more general form:

    cast(x, int)

There is also type punning, which is a re-interpretation of a type without changing anything:

    real x := 1.1

    println int64(x)       # display 1
    println int64@(x)      # display 4607632778762754458 (0x3FF199999999999A)

The `@` symbol makes it type punning (equivalent to `*(int64_t*)&x` in C, but unlike C, can be applied to rvalues too, that is, expressions: `int@(x+y)`).

Again, for complex types, use `cast@(x, T)`.

Sometimes, it can be difficult to get on top of which precise conversion is needed, as happens with pointer types. Then, `cast` can be used to automate it:

    ref int64 p
    ref []int q

    p := cast(q)             # omit the target type

`cast` will apply whatever cast is required. This is handy when the necessary type needs to be tracked down, or when it is likely to change.

(There are issues at present with getting function pointer types to match properly. So here `cast()` is an easy way to do that.)

### Expression List
This is the equivalent of C's comma-operator:

    a + (x; y; z)

but is written with ";", and with mandatory parentheses. `x` and `y` are evaluated first (usually they will have some side-effects), then `z`, which is the result of the expression list.

(Here, also, `x, y, z` can be statements.)

### Conditional Statements
#### If Statement
The general form is:

````
    if cond then
        stmt1
        stmt2
    elsif cond2 then
        stmt3
    else
        stmt4
        stmt5
    end
````
* There can be any number of `elsif` blocks, or none
* `else` is optional, *unless* the whole statement is expected to return a value (see below)
* Conditional expressions are actually Sunits, which means being able to do: `if c:=getnext(); c<>0 then` without needing to wrap those into an expression list.

#### Short-form If

`if` statements can return values, so could be used in expressions:

    print if n=1 then "File" else "Files" fi

but here there is a special syntax for `if` which is better suited:

    print (n=1 | "File" | "Files")

They are interchangeable, except the short form always needs an `else` part. (This is probably what Algol68 had in mind in using `if...fi` as the symmetry matches `(...)`.)

#### Unless Statement
This form has the opposite logic to `if`:

    unless cond then
        stmts....
    else                   # optional
        stmts....
    end
 
Note that `elsif` is not allowed here (I couldn't figure out what it meant).

### Switch Statement
Switch statements use an integer index and are designed to map to a jump table:

    switch x
    when a, b then
        ....
    when c then
        ....
    else
        ....
    end switch

`x` is an integer expression; `a`, `b` and `c` should be constant int expressions, and should not span a greater range than is suitable for a jump table.

Test expressions can be ranges:

    switch c
    when 'A'..'Z','a'..'z', '_' then
        ....

There is a limit of a few hundred values between smallest and largest in a switch. If exceeded, try using a `case` statement.

Both `switch` and `case` return return values, so can be used in expressions, or as the last unit of a function so that it provides the return value. For this purpose, an `else` branch is needed.

### Case Statement
When the index expression `X` of a `switch` is not an integer, or the `when` expressions are not compile-time expressions, or the span of values is too wide, then a `case` statement can be used instead:

    case x
    when a, b then
        ....
    when c then
        ....
    else
        ....
    esac

This will sequentially test `x` against `a`, `b` and `c` in turn, until the first match. (Currently ranges can't be used in M)

#### Alternative to `if-elsif` chains:

One variation of `case` is when the test expression is omitted:

    case
    when a=b, c=d then
        ....
    when e<0 then
        ....
    else
        ....
    esac

Then it will evaluate expressions until one returns true, then it will execute that when-block. This can be used as a tidier alternative to `if-then-elsif-else`, which is easier to maintain as the first test is not a special case.

### Looping `switch` and `case`

Both switch and case statements have looping equivalents:

    doswitch p++^
    when a,b,c then
    when ...
    else
    end switch

    docase x
        ...
    end

At the end of each when or else block, it will jump back to the start to repeat. Some means is usually needed to exit at some point (`exit, goto, return, stop`). Such loops don't return values.

### Loops
All loops support `exit redo next` controls described under Control Flow.

#### Endless Loop
```
do
    stmts
end          # also end do, or just od
```
This loops forever, so needs to be terminated with any of `exit goto return stop`.
#### Repeat N Times Loop
```
to N do
    stmts
end
```
N is any signed integer expression. When N<=0, no iterations are done.

#### Repeat Until Loop
```
repeat
    stmts
until cond
```
Loop is executed at least once.

#### While Loop
```
while cond do
    stmts
end
```
Loop is executed zero or more times. An extra touch is this, where an iteration expression can written next to the condition:
```
while P, P:=P.nextitem do
    stmts
end
```
I use this for linked list traversal.
#### For Loops - Iterate over an Integer Range
```
    for i := a to b do
        stmts
    end
```

`i` iterates over the integer range `a` to `b` inclusve.

* To iterate downwards (decrement rather than increment), use `downto` instead of `to`
* To iterate with step `c` instead of `,` use `a to b step c`. (Currently, the step must be a constant, and it's always positive even iterating downwards)
* The loop index variable does not need declaring. One will be created to suit the types needed, and it will be read-only (as though declared with `let`)
* There can be an `else` block as part of the loop. This will be executed on normal termination, but not if `exit` or other means are used to jump out of the loop
* A start value of 1 can omitted, as that is the default; write: `for i to b do`
* For conditional execution of any iteration, use `for i:=a to b when cond do`

#### For Loops - Iterate over Values in an Object
```
    for x in A do
        stmts
    end
```

`x` iterates over the values in `A`. But `A` can also be a range construct (which is an object in Q, but not in M):
```
    for x in a..b do
        stmts
    end
```
then those values are just `a` to `b` inclusive, equivalent to the other kind of `for`-loop.

Most points above also apply here. In addition:

* To iterate in reverse, use `inrev` instead of `in` (but not implemented)
* If the index is needed, use: `for i, x in A do`
* To iterate over the bounds of `A` rather than its values, use `for i in A.bounds` or `for i to A.len` or `for i:=A.lwb to A.upb` (lots of ways).
* When iterating over values, the loop variable will be an rvalue: it can't be used to modify that entry (eg. like using `&` for reference params). Use the index just mentioned, and write to the element that way


#### `doonce` Block
This is an experimental feature, a loop that always executes exactly once unless you use loop controls:
```
doonce
    stmts
end
```
The purpose is to allow the use of loop controls - `exit redo next`, explained later - within an ordinary block:

    exit     # Jump to the end of the block
    next     # Jump to to end, but it will not repeat so is equivalent to exit
    redo     # Jump back to the beginning (so is possible to loop)

### Control Flow Statements
#### Goto
This can be written as:
````
    goto lab
    go to lab
    lab
````
The last is handy if you don't like the idea of using `goto`, as it makes it less obvious.

Labels need to be written with two colons:

    lab::

(":" is heavily used elsewhere, so there would be too many ambiguities.)


#### Label Pointers

If L is a label, then:
```
    ref label P        # (if using M)
    p := L             # p is a reference to the label
    goto P             # jump via a label pointer; no deref needed
...
L::
```
Arrays of label pointers are sometimes a faster dispatch method than using `switch`.

#### Redo, Next, Exit
These are loop controls can be used in any loop:

    exit         # Exit the loop (and bypass any else-block of a for-loop)
    redo         # Jump to the start of the loop (repeat with the same index in for-loops)
    next         # Jump to the end of the loop and do the next iteration, if any

Normally they work within the current loop, but can apply to surrounding loops too, by means of an index:

    exit         # current loop
    exit 1       # Also current loop, as they are numbered from 1 outwards
    exit N       # The N'th loop counting outwards from the current one
    exit 0       # 0 is short-hand for the outmost loop ...
    exit all     # ... which can also be written as `all`

(Majority of such control works within the current loop; the rest tend to use `all`)

#### Return
This is in two forms:
```
return           # From a proc
return X         # From a function
```
For procs, it is not necessary to write `return` if control runs into the end of the proc anyway. It is necessary for an earlier or conditional return.

For functions, again it is not necessary to use `return`, just `X`, if control flow would run into the end of the function:
```
    func f:int =
        case x
        when a then 10
        when b then 20
        else        30
        end
    end
```
Here, there is no unit to execute after leaving the `case` statement. `return` is needed for early returns, conditionals, and for multiple returns values.

#### Stop

    stop             # same as stop 0
    stop N

Used anywhere, this halts the program, and returns N or 0 as the program exit code.
#### Recase
Another experimental feature used in `case` statements:

    case x
    when a then
    when b then
        s1;
        recase a
    esac

**recase** here will jump to the branch of the case statement that deals with 'a', equivalent to reentering the case statement with x = a (but x is not actually changed).

Currently this doesn't work with nested case statements; you can't jump to an outer case block.

#### Conditional Suffix
All control flow statements can be made conditional:

    goto L unless c='A'
    return when a=b

This is exactly equivalent to writing `if a=b then return fi` say, but it is more convenient: there is no extra indented block, looks better written on one line, and makes the control flow statement more prominent.

### Print
Printing is handled with built-in statements. (Most languages like to use libraries, but that can require advanced language features to get the convenience expected of Print: variable length, mixed-type argument lists for example, and operator overloading for stringifying.)

#### Print/Println
Examples:
```
int a := 123
real b := 4.5
ichar c := "ABC"
char d := 'D'

print a, b, c          # Output: 123 4.500000 ABC
                       # Automatic space between items of one print statement

print a, b
print c                # Output: 123 4.500000ABC
                       # No automatic spacing between different print items

print a,,b,,c          # Output: 1234.500000ABC
                       # ,, inhibits the automatic space

print a, b, $
print c                # Output: 123 4.500000 ABC
                       # '$' used as first and/or last item ensures one space
                       # without needing an untidy print a, b,," "

print =a, =b, =c       # Output: A= 123 B= 4.500000 C= ABC
                       # '=' before an item adds a label showing the expression

print d                # Output: D
```

None of the above end with a newline. Changing `print` to `println`, or adding `println` after, will add a newline.

#### Print Item Formatting
Using the same declarations above:
```
                        # Output (with actual alignment):
print a:"H"             # 7B                 (Hex)
print a:"B"             # 1111011            (Binary)
print a:"JR 8"          #      123           (Decimal, right-justified in 8-char field)
```
The codes used here will need to be documented elsewhere. But, since the compiler knows the types of the expressions, sensible defaults will be used even with no item format control.

#### Fprint/Fprintln
This is formatted printing using a format string, with `#` marking where each item is go:
```
fprint "Fib(#) = #", n, fib(n)   # Output with n=10 might be: Fib(10) = 55
```

#### Print to File
If `F` is a handle to an open file, then it's just this:
```
print @F, a, b, c
fprint @f, "# # #", a, b, c
```

#### Print to String
```
[100]char str
print @str, a, b, c
```
The string ends up containing: `123 4.50000 ABC`, zero-terminated.

(With the original Q scripting language, working within a graphical application, this same approach to printing could do even more:)

    print @W, a, b, c           # W was a graphics windows; print into current text position
    print @BM, a, b, c          # BM was an image handle; print into the image
    print @P, a, b, c           # P was a handler to an open printer/plotter, or LPT1, COM1 etc

Current Q also has more possibilties, but the Print requirements of M are not that ambitious: print to console, file, string.)

#### Print using `printf`

`printf` and other C functions can of course be used directly:

    puts("Hello")        # no semicolon needed!

This can be useful for a minimal EXE, since normal print requires the relevant support library. The library for C functions is external.

### Read
Like `print`, `read/readln` are built-in statements. They are not as accomplished as `print`, but are also used less often.
```
int a, b, c
print "Enter three numbers: "
readln a, b, c
println "Their sum is", a+b+c
```
Input is reasonably flexible (eg. `10, 20, 30` or `10 20 30`, with or without commas). Missing numbers are treated as zeros, rather then silently wait for more input as happens with C functions.

`readln` reads a whole line of input, then any items following, or from subsequent `read` statements, consume that line. The above could have been written as:
```
readln
read a
read b, c
```
To read from file handle F, it's `readln @F, a, b, c`.

### Range Syntax (M)

A range is a pair of ints defining a sequence, such as `1..10`, which means 1 to 10 inclusive, and always stepping by 1.

In M, it's not a type in itself, but it can be used as a construct in lots of contexts:

    [1..10]int a                 # array bounds
    
    switch c
    when 'A'..'Z' then           # switch ranges
    ....
    
    if a in 1..10 then ...       # test for being in range


### Set Testing (M)
Like a Range, this is just syntax in M, and is only used in conditional expressions:

    if x in [a, b, c]

This is equivalent to `if x=a or x=b or x=c`

(In Q, both ranges and sets form proper first class objects.)

### 'Equivalence' (M)
This feature allows two variables to share memory:

    real x
    int a @ x

Here, `a` shares the same memory as `x`. Note that a should not be larger than x (unless that is the intention, or perhaps `x` is itself equivalenced to a larger variable).

Currently the expression after `@` must be simple. (Previous versions allowed indexing and other expressions, but I am considering whether to replace this old feature completely. It was originally taken from Fortran.)

### Bit and Bitfield Indexing

If A is an integer, then it can be indexed like this:

    print A.[i]      # Extract the i'th bit as 0 or 1
    A.[i] := x       # Set the i'th bit to x; x must be 0 or 1

The index goes from 0 (least significant) to 63.

An arbitrary bitfield can be extracted using:

    A.[i..j]         # Extract the bitfield of bits i..j inclusive
                     # When compile-time expressions, i/j can be in either order, eg. 0..7 or 7..0
    A.[i..j] := x    # insert x into that bitfield; x must not exceed the width of the field

All extracted bitfields are treated as unsigned values.


#### Predefined Bit/Bitfield Codes

    A.msb          Top byte (most significant)
    A.lsb          Bottom byte (least significant)
    A.msbit        Top bit
    A.lsbit        Bottom bit
    A.msw          Top half
    A.lsw          Bottom half
    A.odd          1 when bottom bit is 1 (read-only)
    A.even         1 when bottom bit is 0 (read-only)

Most of these are convenient aliases for A.[63] and so on, however `.odd` and `.even` are a little different.

### Bitfields Within Records
Named bitfields, a little like C's but better defined and with more checks, are possible inside records:
```
record rec =
    u16 flags : (a:4, b:1, c:2)
end
```
* Bitfields must belong to a containing field which is a normal integer type
* They are allocated starting from bit 0 of that type
* Overflowing the containing field is an error
* The bitfields can be accessed individually; given a variable `X` of type `rec`:
```
    X.a          # extract first 4 bits of X.flags
    X.b := 1     # set bit 4 of X.flags (equivalent to X.flags.[4] := 1)
```
* The bitfields can all be read or written in one go using `X.flags`

### 2-Way Selection (If)
This is the equivalent of C's `a?b:c` operator, either of these can be used, but the shorter one is better in an expression.
````
    if a then b else c fi
    (a | b | c)
````
* The parentheses are necessary
* Only one of `b` or `c` is evaluated according to whether `a` is `true` or `false`
* With `&(a | b | c)`, the `&` propagates inside
* Can be used on left of an assignment: `(a | b | c) := d` (assign `d` to either `b` or `c` depending on `a`). (This works in Q; in M it needs attention.)

### N-Way Selection
This selects one value of many:
```
    (n | a, b, c, ... | z)
```

* `n` is one-based, so `n = 1` selects `a` in my example
* Only one of the expressions is evaluated, so it is unlike indexing a list: `(a, b, c)[n]`, where all elements have to be evaluated before the choice is made.
* Also unlike indexing a list, `n` is bounds-checked; it will select the default (`z` in my example) if out of bounds
* Like 2-way select, `&` should propagate inside, and it should be usable on the LHS of an assignment, but I haven't checked because I don't commonly need that
* There is no long-form (for example, `select n then a, b, c else z end`), even though elements could all be statements.

(This use is not recommended, although it quite matches how Algol68's `case` statement works, because you can't easily connect each statement with its ordinal position 1, 2, 3 ...)


### Block Handling (M)

'Blocks' are Array and Record types (and Slices), which have an arbitrary N-byte size. While normally manipulated and passed by value, the Windows ABI says that such objects are passed by reference.

M doesn't specifically make copies, so care needs to be taken that callees do not write into their caller's data. M doesn't have C's `const`; it does have parameter attributes like `in` to avoid that, but like 'let', this area is poorly developed.

So just be aware that blocks ostensibly passed by value, are actually passed by reference, and could be modified. Manipulations not involving passing or returning from functions *are* done by value.

(The ABI also says that Blocks of exactly 1, 2, 4 or 8 bytes are passed by value. This is actually done when calling FFI functions, but within M, I will have to check what it does.

The funny thing is that M versions for the first 30 years passed and returned blocks actually by value, as I used private call conventions, but that feature was never used; it was too inefficient. I always used pointers and references.)



### Type Constants
Here:

    (int)

The internal code for the type is turned into a type-constant, which can be assigned to an integer.

Such values can be manipulated, passed, compared etc. This is more useful in Q, but in M, if `X` contains some type-code:

    case X
    when (int) then print "Int"
    when (real) then print "Real"
    end

(For more elaborate types, use for example: `type T = [4]byte` then write `(T)`.)

### Operator Constants
Similarly to type constants:

    (+)

creates an integer constant representing `+` or `add`. This is not that big a deal, since it's possible to just write `'+'` to turn it into a code to be passed and compared, but it is nicer to have language support (where it may have to match something inside).

(In Q, both type- and operator-constants have a special type, and when printing show the type/operator name, not just a number. There, `opc:=(+); print mapss(opc, 10, 20)` displays 30. It's better do things the same way in both.)

### Module Scheme
M has a module/import scheme that really needs its own docs. But in brief, if a module comprises modules A.m, B.m, C.m, then module A.m starts like this:
````
    module B
    module C
````
You don't need to repeat this stuff in every module, only in one place. For a single-module program, nothing is needed. M's standard library is available automatically. A module doesn't list itself.

Anything declared with a `global` attribute in any module is accessable in every other module. You don't even need to qualify the name, eg. rather than `b.f()`, just use `f()`. 

Bigger projects tend to have the first or lead module containing only lists of modules (Header Modules). There is more to the module scheme, but I need to write those docs.

### Globals and Exports
In M, nothing is exported or shared from module unless done explicily using `global` or `export` in front of the definiton. The following can be exported and shared:

* Functions
* Types
* Records
* Enum names
* Named constants
* Variables
* Macros

Anything that is named and defined at module level. Any named entity is always defined in exactly one place.

**global** makes the name visible from any module in the subprogram

**export** additionally makes the name visible to modules in other subprograms

When this subprogram is the main or only one of the program, then `export` makes names visible to other programs; this program then becomes a library like a DLL.

* `main` procedures in the Main Module is automatically exported from the program; no `export` is needed.

### DLL/LIB Exports

(Note: DLL creation is buggy at the moment, hence the use of LIB alternative format, using .ml files. But it might be fixed at some point.)

When compiled to a DLL or ML shared library, an additional Exports module is created. This contains an `importdll` or `importlib` block that defines the interface. Another M program can simply include that module to use that library.

With third party libraries, such a module needs to be painstakingly written by, usually, translating C header files.

Example, a library bignum.m is written, that I want to make into DLL:
```
    mm -dll bignum              # generate bignum.dll and bignum_exp.m
```
Inside a program that wants to use that DLL:
```
    module bignum_exp
```

And, that's it. Although there are a some flaws to sort out other than the buggy DLL file:

* I want that exports module to be called bignum.m, but that will overwrite the library module itself, also called bignum.m
* Ideally I don't want to generate such a file at all; the information can be put into bignum.dll itself,  then I can just do:
```
  dlllink bignum
```
It can look inside bignum.dll to pick up (by calling a special function) that necessary info. Anyway this is speculation that doesn't belong in a reference of what currently works.

* DLL exports can easily share functions
* LIB exports can easily share functions and variables
* But neither right now can easily share user types, records, named constants, enums, macros (since it involves re-generating definitions for those names in the export file)

I'm working on that last aspect, especially in sharing between DLL/LIB and Q code.

### Conditional Compilation
Neither language has conditional code unless this counts:
```
const flag = 1
if flag then
    stmts1
else
    stmts2
fi
```
Because `flags` is constant, only stmts1 or stmts2 will be compiled into the program, not both. However both blocks must coexist until the code-gen pass, so they cannot have different declarations of the same variable for example.

Some conditional directives are used in header blocks, but the approach I use most often is this:

* Use a Header Module, which contains only module info. Say this is called A.m.
* For a different configuration, create B.m which contains a slightly different set of modules
* Compile either A.m or B.m, to result in A.exe or B.exe.


### DLL Imports
Use of an external DLL library requires an interface to be created in M or Q code. If a DLL `jpeg.dll` is to be used for example:
```
importdll jpeg =
    func loadjpeg(ichar filename, int &w, &h)ref byte
end
```

Given this, the language knows that jpeg.dll is needed (although header blocks can also give that info, and a dummy name used here, because sometimes it's not that simple; some libraries are split across multiple DLLs).

Typically such a block (usually bigger than this), is put into its own module, say `jpeg.m`. Then any program that want to use that library will just do this:
```
module jpeg
```
It doesn't need to be concerned with the details. By some magic, any imported DLL function names inside jpeg.m are automatically re-exported from that module, and a program can just say:

    pdata := loadjpeg("file.jpg", w, h)

For bigger libraries, creating these 'bindings' is a lot of work, especially working from C header files and/or poor docs. There is a tool I have used to do it semiautomatically, but there's still a lot of tidying up to do. 

(When tried on GTK2, which is described by 550 C headers over 330,000 lines of code, it produces a 25,000-line `importdll` block, which includes 3000 lines of C macros needing to be sorted out by hand. The result, however, would be a single file interface to that whole library.)

### LIB Imports
This is pretty much the same, but it uses `importlib` instead, and `.ml` files not `.dll`. LIB files can do more, make it easier to access variables across the FFI, have a shared environment with the host application, and there are plans to share more than just functions and variables. But this is on-going development.


### Standard Library (M)
This comprises the following modules:

Module | Description
--- | ---
msys | Support library
mlib | File and memory functions
mwindows | OS-specific functions
mclib | Import some C functions
mwindll | Runtime DLL access

`msys` contains support for language features, mainly functions which are called implicitly (for example, to implement `print`).

`mlib` contains functions called explicitly, mainly related to files and memory allocation.

`mwindows` are those functions which need to different across OSes (this is for when I supported Window and Linux; then there would be `mlinux` that would be used inside, and there was also `mnos` that worked on either, but with some functions disabled.)

`mclib` contains bindings to the few dozen C functions that are used

`mwindll` contains my solution to the LIBFF issue described elsewhere

This suite of modules are formed into a subprogram, which export a set of function names, and would be added using these lines:
```
syssubprog mlibx
sysmodule msys
sysmodule mlib
sysmodule mwindows
sysmodule mclib
sysmodule mwindll
```
However this inclusion is done automatically unless the `-nosys` option is used.

No docs exist for these yet (something to be done, a good test for my doc-strings feature), only sources. 

### Macros
Macros with parameters are a recent additon to both languages.

They are not done on the scale of C's macros, which work textually, and can be used to define arbitrary bits of syntax.

My macros define only well-formed bits of expressions, and can only be invoked from inside functions. Because they are handled in a later stage in the compilation, the body of a macro cannot define new symbols (ie. new declarations).
```
macro hashc(hsum,c) = hsum<<4-hsum+c

hashc(a, b)
```
The body is single unit, although an expression list can be used to allow an Sunit, so any sequence of expressions and statements. But it must be meaningful when it is expanded inside an expression.

I tend to use them for simple aliasing, small in-line functions, and also to help out with M's inline assembler

Macros can be imported and exported like any other function.

### Naked Functions (M)
Naked functions are those without normal entry/exit code. There are stack-frames, so parameters and locals are not allowed. Only local static variables. They look like this:
```
threadedproc fn =
    ...
end
```
They are called `threaded` because they were designed to implemented threaded code for my interpreter. Threaded functions don't do normal call and return, they just directly jump to the next and never return.

### Constructors (M)

These are list expressions such as `(10, 20, 30)`. There use is restricted (this is something that needs to be rolled across the language, but it has low priority):

     []int A = (x, y, z)         # (static array) x, y, z must be compile time expressions
     []int A := (x, y, z)        # This works but is done inefficienty; x,y,z can be variables
     A := (x, y, z)              # The same
     f((x, y, z))                # Not allowed

### Inline Assembler (M)
This is designed to be very simple to use:
```
int a, b, c
assem                          # block of instructions
    mov D0, [a]                # The compilers makes this [Dframe+a]
    mov D1, [b]
    add D0, D1
    mov [c], D0
end

asm mov [stackptr], Dstack     # one-off instruction
```
For versions of `mm.exe` which have an optimiser and place variables into registers, the optimiser is turned off for functions which use inline assembler. Then you know exactly where everything is.

Mostly when inline assembly is used, it comprises all of the function body.

### Compiler Variables (M)
These are special built-in variables and constants:

```
$lineno          Int     Constant representing the current line number in this source file
$strlineno       String  String version
$filename        String  Current file name
$module          String
$function        String
$date            String  Date compiled
$time            String  Time compiled
$version         String
pi               Real    Value of pi
infinity         Real
true             Bool    True
false            Bool    False
```

### "$" Symbol

`$` is a keyword used in several contexts:

**enumdata** Here, `$` yields the name of the last created enum name as a string constant (see enumdata section)

**print** Use of `$` as the first and/or last item to be printed, inserts a single space without needing to worry about suppressing the automatic spacing. (Normally, spaces are inserted between items in a print statement, not between successive print statements.)

**Arrays** Used within an array index for array `A`, `$` yields the upper bound of the array, or `A.upb` (or `A.len` when `A.lwb` is 1). Example: `A[$]` or `A[$-3]`.

### Embedding Text and Binary Files

`include` is an ordinary textual include, and is only needed in M when you actually want to include code from another file. (For example, a file generated from a program.)

`strinclude` can be used in an expression, and can include any text file as a string constant. (I use this to incorporate the sources for M libraries into the M compiler; or the C header sources into my C compiler.)

Example in a program called prog.m, this prints itself:

    proc start =
        println strinclude "prog.m"
    end

The source file prog.m does not need to be present at runtime; it becomes part of the executable.

`bininclude` (M) is a variation used to initialise a byte-array, and can refer to any file including binaries. However it's done inefficiently at present.

(Q can use `strinclude` for both text and binary fles, as its counted string types can have embedded zeros.)

### LIBFF
LIBFF is the name of the C/ASM library which allows calls to dynamic library functions to be synthesised at runtime. It's quite complex, and needs building with gcc (which is not easy). I'm not even sure how it could be used from a language like mine.

However the necessary functionality is built-in to M inside the `mwindll` library module. It uses this function:
```
func os_calldllfunction(
    ref proc fnaddr,                  # Address of the DLL function
    int retcode,                      # 'I' or 'R' (int/float)
    nargs,                            # number of 
    ref[]u64 args,                    # array of 64-bit values for each argument
    ref[]byte argcodes)               # 'I'/'R' code for each argument
     => word64
```
The function address must be known (eg. use os_getdllprocaddr() from `mwindows`).

All arguments (ints, pointers, floats etc) are put into an array of u64, using type-punning or as required.

It is necessary to say whether any argument and the return type is float/not-float, as that affects how the passing is done within the ABI.

This is achieved within a 60-line function thanks to the use of in-line assembly.

### Function Tables (M)
There is a limited amount of reflection in that all functions (names and addresses) in the program are written to the executable, and can be accessed via special functions.

This allows finding out the name of a function from a function pointer. But what I most use it for is building, at runtime, tables of function pointers for special handlers, by looking for specially formatted function names. The special functions provided for this are:
```
    $get_nprocs()           # returns number of functions in this program
    $get_procname(n)        # return name of n'th function (1-based)
    $get_procaddr(n)        # return address of n'th function
```
Note: this scheme can contain duplicate function names, as the same name `F` in module `A` and module `B` is not distinguished. I will fix this at some point. But current use deals mainly with names in specific formats, unlikely to clash.

(Q has its own schemes.)

### Hello World
```
    proc main =
        println "Hello, World!"
    end
```
Also possible, since both languages indulge the practice common in scripting languages, is allowing code outside a formal function:
```
println "Hello, World!"
```
Actually the compiler collects such code into a proper `main` function. This style should only be used for short scripts with no actual functions, as the rules for global scopes makes scoping errors much more likely.

The following is a graphical version using Win32:
```
messageboxa(message:"Hello, World!")
```
(Demonstrates keyword args superimposed on an external FFI function in a language that doesn't have the feature, and calling it in a case-dismissive manner.)

The above programs, for M, all generate an EXE of 40-50KB, since M's library is automatically included. For a minimal executable (2.5KB here) it is necessary to avoid using built-in print:
```
sysmodule mclib
puts("Hello, World!")
```
Compile this as `mm -nosys hello`. (`-nosys` disables the automatic inclusion also of the MCLIB module that defines some C functions, so it has to be reinstated)
