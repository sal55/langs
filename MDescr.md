## M Language Description - 6.3 March 2024

An informal description of the latest version, also highlighting things that need attention.

It's not a proper reference or user manual, but mainly for my benefit as developer and maintainer.

Looking through it, it gives a picture of an untidy, sprawling language that needs to be tightened up. The same applies to this document!

I will need to go through it in detail and test that the features mentioned actually work as stated. If not, then fix them, or remove them.

What I hadn't realised was how much behaviour has simply been assumed. Little of that is specified in detail. Doing so would result in something as big as the 700 pages of the C standard. But that doesn't suit this informal, personal language.

### Overview

Quite a few things have been changed over the last few years. This is a summary of 6.3:
````
- Expression-based rather than Statement-based
- Targets x64 processor running Windows using the Win64 ABI, even internally
- Uses 'PCL' stack-based intermediate language with 64-bit primary integer type
- Simplified 2024 Module scheme
- Supports EXE DLL directly, and OBJ, MX/ML via ASM and AA assembler
- Supports -himem and -rip options (set automatically for DLL/OBJ outputs)
- Has -regs and `-peep` optimiser settings
- Can export to either M or Q for DLL/ML, but both need more work
- Can run from source
````
### Syntax

(For those not familiar, see examples in the `.m` files elsewhere on this site.)

#### Block Endings

These are the endings for the final block in a statement or a declaration block:
````
    end             # Works for any statement or declaration
    end if          #
    end while       # etc, keyword should match opening keyword

    fi              # Only works with `if` (these three are from Algol68)
    esac            # Only works with `case`
    od              # Works with any loop where do opens the loop body
````

So `if` can end with any of `end`, `end if`, `fi`. `endif` and similar variations have been dropped.

#### Semicolons and Newlines

Newlines get converted internally to semicolons unless the line obviously continues onto the next, for example it ends with:
````
    ( or [ or ,
    Binary operator
    \ line continuation
````
Actual semicolons are rare in source code unless used to separate multiple units on one line.

Extra semicolons due to newlines are usually harmless and well tolerated, since a simple `if` statement written across 5 lines may be seen as this:
````
    if a=b then; c:=d; else; e:=f; fi;
````
This would be tricky to express in a formal grammar. It's a good thing there isn't one...
#### Case Insensitive
Generally case insensive, unless an identifier starts with a backtick, then case is preserved. Backtick also allows reserved words to be used as identifiers:
````
    int `int, `Int, `INT
````
For interfacing to external routines that use a particular pattern of upper/lower case, then the function name can be written as a string containing the right case. But subsequently any case can be used.

#### Comments

There are only single-line comments, and they start with `!` or `#`. The status of `#` however is unclear.

Comments starting `##` has been used for doc-strings, but that feature has been removed for now. `#` is useful in shared code, but '#' might be
too useful a character to duplicate the job of `!`.

There are no block comments (this is considered an editor function), nor ones in the middle of a line.

### N-Based
Primarily 1-based, but N-based including 0-based if usually allowed. A few things are 0-based only:
````
                    Default     Override?

    Arrays          1-based     Yes
    For-loops       1-based     Yes         Default used in 'for i to B do...'
    Bit-indexing    0-based     No          A.[i] and A.[i..j]
    N-way select    1-based     No          As used in (n | a, b c | z)
    Enumerations    1-based     Yes
````
In practice, a lower bound  other than 1 or 0 is rare.


### Entry Point, Main and Start Functions

Function names `main` and `start` are special. They need to be declared with `proc`, and take no parameters (this needs to be checked).

They automatically have `global` or `export` attributes applied as needed.

The program entry point will be the `main` function in either the lead module, or the second. (This needs checking: ATM if both have `main`, the second is used.)

Other modules can have `main`; those are ignored, although you can call them explicitly. (This allows those modules to be compiled as independent programs with their own `main` function.)

Any module can also have a `start` function. If present, it will be called automatically when the program starts. The order in which those functions are called depends on the order of the `module` directives in the lead module, except that the one containing the entry point will be done last. (This needs checking.)

(For `start` functions within the modules of a subprogram, I'd need to check. Subprograms really need to be done first. There the hierarchy may be important, but it's not something I want to get into. I will just check what is actually does, and document that.)

#### Open Main Code

This allows you to dispense with a `main` function, so that short throwaway programs may be written in the manner of a scripting language.

However, it's crashing at the moment. Also, if that code uses short identifiers, these will be in the common namespace of that module, possibly clashing with undeclared locals within functions, with unexpected results.

I might then just drop the feature (so `hello.m` will need to be 3 lines instead of just one), or restrict it so that no other functions are allowed in that module, only open code.


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
    pci prog a b               # pci interprets prog.pcl
    ms prog a b                # ms compiles prog.m and runs it
````
then `cmdparams[0]` will contain `prog` in both cases. A special mechanism is used to move the 'parameter window` along so that applications think they have been launched normally.

However programs which directly call functions like `__getmainargs` or `GetCommandLine` will see all inputs.

### Program Exit
This can be done with any of these:
````
    stop                    Terminate with return code 0
    stop N                  Use return code N
    `exit(N)                Use C function (note exit is an M reserved word)
    exitprocess(N)          Use WinAPI function
````

The `main` entry point function will have `stop 0` injected at the end, so just running into the end of `main` will terminate too.

### Modules

See [Modules24](Modules24.md) which describes the scheme in detail. In short, all project info is provided by a list of directives at the start of the lead module. Usually, they will be the only thing in the lead module. The directives are:

    module name             # module name forming part of the main subprogram
    import name             # read the lead module and other modules of another subprogram
    linkdll name            # Dynamic shared library if it is not clear from importdll statements
    linklib name            # 'ML' library (this may be dropped)
    $sourcepath string      # temporary directive until path control is sorted out

### Scopes

Scopes affect visibility of top-level names.

The following scopes exist:

    Program scope       The entire program
    Subprogram scope    Shared by all modules in a subprogram
    Module scope        Only in that module
    Function scope      Only in that function
    Record scope        Only in that record
    Macro scope         Only in that macro

There are no Block scopes in this language; each Function contains only a single, Function-wide scope.

    Kinds of Names      Scopes they can exist in

    Module              Program
    Subprogram          Program
    Function            Subprogram, Module, Record (no nested functions)
    Variable            Subprogram, Module, Function, Record
    Type/Record         Subprogram, Module, Function, Record
    Named Constant      Subprogram, Module, Function, Record
    Enumeration         Subprogram, Module, Function, Record
    Macro               Subprogram, Module, Function, Record
    Parameter           Function
    Stack Local         Function
    Static Local        Function
    Macro Parameter     Macro
    Label               Function
    Field               Record
    DLL Module          Program
    DLL Function        Subpodule
    DLL Variable        Subprogram (DLL variables not fully supported by M's FFI)

(This is quite heavy going; perhaps just have the two lists of scopes/names, and mark names for which attributes can be used, since other
kinds of visibility are inherent.)

#### Attributes

From inside a Module M which is part of Subprogram S, these attributes can be applied to any name defined at module-scope:

````
    global              This attribute makes it visible to other modules in the Subprogram
    export              This additionally makes it visible other other Subprograms, or to
                        outside the program is this is the main subprogram; it then becomes
                        a library
````
From anywhere in the same Subprogram, `M.abc` can access an entity `abc` inside module `M`, provided it has `global` or `export` attribute.

From anywhere in other Subprograms, `S.abc` can access `abc` in any module of S, provided it has `export` attribute.

No actual names can be defined inside a Subprogram; only inside a Module. Subprogram names are a little abstract, and are kept outside the global Symbol Table, since they will clash with module names: `S` will have the same name as the leadmodule of the Subprogram.

In most cases (at least 99% in my code), `M.abc` or `S.abc` can be written as just `abc`, unless there is a clash and disambiguation is needed.

(Rewrite the above. And below!)

From outside a Record definition `R`, with a possible instance `X`:
````
    R.name              Allows access to all names in R except Fields, which only come into
                        existence when an instance is created
    X.name              Allows access to all names including Fields
````
From outside a Function F:
````
    F.name              Allows access to all names except Parameters, Stack Locals and Labels.
````
There are no Private/Public attributes to control access to names inside Records; everything is Public. That applies to Functions too; access like this is unusual.

#### `static`

This isn't to do with scope. It can applied to a variable inside a function so that there is a single instance shared by all invocations. Then it will keep its value between calls.

### Namespaces

Namespaces can be created within modules, functions and records. Subprograms also create a namespace, used to access exported names from its modules.

Qualifed names such as `M.F()` to call function `F` exported from module `M` are usually not needed. Only if there is ambiguity, for example if another function also exports `F`, or `F` is a function in this module and you want the one from `M`.

Restriction: out-of-order definitions make things difficult with user types, even making it hard to recognise declarations:
````
    A B, C
````
If there are two consecutive identifiers such as `A B`, it is assumed that A is the name of a user-type, and B a variable of that type. But type names
can't be written like this to disambiguate:
````
    M.A B, C
````
There is a mechanism in place for this, but it's not fully implemented. So type ambiguities can't be resolved.

### Out-of-Order Definitions

All definitions in M programs can be written in any order. This applies to everything, not just functions. So it is possible to define all the locals of a function at the end, rather then the start.

The order of modules in the project info can be important, but these are directives rather than definitions.

Sometimes this gives unexpected results, for example if the declaration for a local is forgotten, or a `for`-loop is written that auto-declares its index, it may end up inadvertently using a module-scope variable, perhaps one used toward the end of a module
if an open `main` function is used (that is, outside of `proc main ... end`).

Such open code may be dropped.

### Data Types

#### Simple Types
````
    Long        Short   Aliases     Notes

    int8        i8                  Signed integers
    int16       i16
    int32       i32
    int64       i64     int

    word8       u8      byte        Unsigned integers
    word16      u16
    word32      u32
    word64      u64     word

    real32      r32                 Floating point
    real64      r64     real

    char8       -       char        Character type is a thin wrapper around u8/byte/word8
    char64      -
    bool8       -
    bool64      -       bool

    void        -                   Normally a pointer target only
    label       -                   Pointer target only
    ichar       -                   Alias for 'ref char'
````

No variables of type `void` can be declared. (Just checked and apparently it is possible! But they will generate errors further down the line.)

`void` can be used in `ref void`, and comes up as an internal type (a `while` loop returns a `void` result for example).

#### Other Types

These are all 'placeholder' types, requiring extra syntax and more info to form a custom anonymous type, except for records which must be named types:
````
    Syntax

    ref T               Pointer to type T
    [bounds]T           Array of T
    slice[bounds]T      Slice of T
    proc(params)        Pointer target only
    func(params)T       Pointer target only
    record name = ...   User-defined only
````

#### Type Syntax

Elaborate types can be simply written left to right:
````
    ref [10]ref int A       Pointer to array 10 of pointer to int
    array [3,4,5]int B      3D array of int dimensions 3 x 4 x 5
    [3][4][5]int B          Alternative
````

The `array` prefix optional (something that may be dropped).

Function types (as used in function pointers) use slightly different syntax from definitions:
````
    ref proc(int,int)       P
    ref func(int a,b,c)int  Q
````
Parameter names are optional, but if they used, they allow default values, keyword arguments and by-reference passing.

(All this is currently not working for function pointers.)

#### Type Aliases

`type` creates a single-token named alias for a type:
````
    type quad = [4]byte
    type elemtype = int32
    type T = int
    type U = int
````
It never creates a new type; `T U int` all refer to the same `int` type.

### Constants and Literals

#### Integer Constants

These have values from `0` to `18446744073709551615`. Values up to `9223372036854775807` have type `i64`, above that `u64`.

In some contexts, when an `i64` constant is combined with a `u64` operand, then the constant is considered to be `u64` too. The ensures the whole operation is unsigned. Otherwise, because i64 is dominant, in an operation like this:

    u64 a = 0xFFFF'FFFF'FFFF'FFFF
    if a > 5 then ...

The comparison would be performed using i64, which means comparing -1 with 5, giving the wrong result.

There are some bugs associated with the term `-9223372036854775808` (because there are two tokens, and constant part is processed as a `u64` value). Instead use `i64.min` to enter that value.

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
    0.5
````
`.6` would be invalid; it needs at least '0' before a decimal point

#### Character Constants

They look like this:
````
    'A'                            char, with value 65
    'AB' up to 'ABCDEFGH'          u64 (not char64)
````

Note that `'A'` has type `char`, unless it is multibyte.

`'ABC'` has the leftmost character (`A` here), in the least significant position in the word. The intention is that when stored in memory, it has the same layout as the string `"ABC"`. (However this is only valid for little-endian; I've never used big-endian.)

Unused parts of a word will be zeros.

#### String Constants

These are the usual zero-terminated byte sequence. A string has type `ref char` or `ichar`. Escape codes are as follows (all case insensitive):
````
    \a           Char 7 (bell/alert)
    \b           Char 8 (backspace)
    \c \r        Char 13 (carriage return)
    \f           Char 12 (formfeed)
    \l \n        Char 10 (line feed, or Linux newline)
    \e           Char 27 (Escape)
    \t           Char 9 (tab)
    \u           Reserved for Unicode specifier
    \v           Char 11 (vertical tab)
    \w           Char 13, 10 (Windows CR/LF sequence)
    \xdd         Two-digit hex code gives char
    \y           Char 16 (backwards tab in my apps)
    \z \0        Char 0 (embedded zero; will need care)
    \" \q ""     Char '"' (one double quote)
    ""           Char '"' (one double quote)
    \\           Char "\" (backslash)
    \'           Char "'" (single quote)
````
Raw strings: these ignore escape codes so that `"\"` is a normal character:
````
    F"C:\demo\mm.exe"
````
Initialising Char Arrays

The type system demands that these are initialised like this:
````
    []char S = ('A', 'B', 'C', 0)
````
which is inconvenient. Here two special string constants can be used instead:
````
    []char S = a"ABC"         # Same as ('A', 'B', 'C')
    []char S = z"ABC"         # Same as ('A', 'B', 'C', 0)
````
A-strings (it can be `a` or `A`) are not zero-terminated; Z-strings are.

#### Pointer Constants

There is only one, called `nil`, with type `ref void`. It is always all-bits zero on supported targets.

### Include and Embedding Files
````
    include "filespec"          Incorporate in-line source code from another file "file.m"
                                Any path given is absolute, or relative to this module. Default extension is ".m"

    strinclude("filespec")      Brackets are optional. Incorporate (embed) any text file as a string constant, can be used in any
                                expression that allows string constants.

    static []byte zipexe = bininclude("zip.exe")
````

This embeds a binary file, but it is currently inefficient. It can only be used as data for a byte-array, and is implemented internally as:
````
    static []byte zipexe = (0x49, 0x5A, ...)     # 135,000 entries
````
It needs a special byte-string repesentation, like a counted string, to be efficient. (In the Q language, `strinclude` will do both text and binary files.)

### Named Constants
````
    const a = 100           # Simple type inference makes a an int type
    const b = 101.1         # b is a real
    const int c = 202.2     # Explicit type forces rhs to be int
    const d = a + b         # d is real (dominant type)

    const e = "ABC"         # These work, but 'const' is really for numbers
    const f = nil
    const g = &x            # should be an error, even if x is static           
````
The RHS should be a compile-time expression only, not an address which is known at run-time.

While `e = "ABC"` works, `const` should be limited to ints and reals.

It should be impossible to apply `&` to a const name, or pass it by-reference.

### Array Constructors

By these I mean constructing an array by enumerating the values:
````
    [3]int A    = (10, 20, 30)      # Exactly 3 elements must be provided if the bounds are 3
    []int B     = (10,20,30,40)     # Any number can be provided; this defines B's bounds as 1..4
    []int C     = ()                # Empty array ([0] also allowed)
    [1]int D    = (10,)             # One-element needs a trailing comma
    []int E     = (10,)             #
````
All the above use `=`, so variables must be at file scope. Within a function, `static` is needed in order to use `=`.

With `=`, all the values must be compile-time expressions or static addresses of variables and functions.

Initialising non-static arrays needs `:=`, but the values can be runtime expressions (check this still works).

I thought that array constructors in arbitrary contexts were not fully supported (as RHS of an assignment, or to pass to a function), however they seem to work if all elements are compile-time expressions.

Unintialised local arrays will have undefined values.

For N-dimensional arrays, all dimensions needed to be specified except for the first:
````
    [,2]int J := ((10,20), (30,40), (50,60))        # Final dimensions are [3,2]
````
The layout of the parentheses must exactly match the the type.

### Record Constructors

These are more or less the same as array constructors. All elements of the record must be provided.

For `union` fields, where a union contains multiple types all at the same offset, the type of the first field in the union should be initialised (to be checked).

For records with one field, initialise with `(x,)`. Records with zero fields can't be initialised with `()` ATM.

Other restrictions apply as per arrays.

Initialising via field names doesn't exist. They're in my Q language; in M, if they were added, it would look like this:
````
    date h := (month:10, year:1999)          # .day will be zero
````

### Variable Declarations
````
    int a, b:=23, c
````
These forms are rarely used or were experimental:
````
    var int a, b, c             # var is made optional
    mut int a, b, c             # mut also means var
    var a, b, c                 # missing type defaults to 'auto'. This was supposed to be infered, but
                                # never got round to it. `var a,b,c` is, conveniently Q syntax
    let int a := 100            # assign-once variable. Very poorly implemented, only works for
                                # simple scalar types
    var a:int:=0, b, c          # Tried out name:type ordering (not available for parameters)
                                # b and c share the same type
````
I didn't take to `let` because it required initialising on declaration, which often means declaring in the middle of code.

The while Read-Only thing is poorly handled in my languages, which are keen on having mutable everything. But I am thinking of at leasting moving data like string constants to read-only segments (I don't have one in my back-end, so it may need to go into the code segment).

### Clearing

Arrays and records can't be cleared by initialising to a constructor with a single zero byte as in C. Any init data must match the type exactly. So alternate ways were introduced; here `T` represents some aggregate type:
````
    T A := empty

    T A
    A := empty

    T A
    clear A
````
I mostly use `clear`, so I may drop `empty`. `clear` can also be used like this:
````
    ref T P := ... allocate memory ...
    clear P^
````
However it is easy to write `P` instead of `P^`, so the pointer is cleared, but not the data. So I may disallow clearing of a simple scalar type.

The above can only be used for fixed-size ojects. The alternative is to use my `pcm_clear` function or call C's `memset()` routine directly.

### Arrays
* Indexing is by using `A[i]` or `B[i,j]` (or `B[i][j]` will also work)
* Indexing starts from 1 unless specified otherwise; see below
* Use one of `A.len A.lwb A.upb A.bounds` for info about the dimensions
* Use `A[$]` as an alternative to `A[A.upb]`
* Arrays are manipulated by value, and can be passed and returned notionally by value, except that the Win64 ABI implements that with references. No actual copying is done, which is something to be aware of (see Block Handling)
* Arrays can only be initialised with the exact number of elements, not fewer like in C
* To initialise an array A to zero, simple use `clear A`. Initialising/assigning using `A := empty` will likely be dropped.
* Dynamic arrays must be created manually with pointers and allocations (here slices are better)

#### Array Bounds
The general pattern is:
````
    []              Unbounded with lower bound 1; either size is from init data, or array must be a pointer targer, otherwise length is zero
    [N]             N elements indexed 1..10 inclusive
    [A..B]          B-A+1 elements indexed A..B inclusive (eg. [0..9]int has 10 elements)
    [A:N]           N elements indexed A..A+N-1 inclusive (eg. [0:10]int also has 10 elements indexed 0..9) 
    [A:]            Unbounded but with with lower bound A
````
#### N-Dimensional Arrays

Flat arrays of 2 or more dimensions (that is, with all elements in one contiguous block), are only supported for fixed sizes for the trailing dimensions. Only the first dimension can be set at runtime, as happens with 1D unbounded arrays.

To initialise a 2D array, again only the first dimension can be omitted:

[,3]int A = ((1,2,3), (4,5,6))          # define a [2,3] array. [][3] will also work.

The trailing dimensions are needed so the compiler can do the address mode calculations properly.

Non-flat multi-dim arrays (using pointers, or Iliffe vectors) are possible:
````
    ref[]ref[]int A
````
Once initialised and the pointers are set up (the language won't do this), then whatever the lengths, access to an element can be done with
````
    A^[i]^[j]
````
Because `^[` can be dropped, this can be written as `A[i][j]`, or reduced further to `A[i,j]`. This kind of 2D array allows each row to be a different size; it is up to the application to keep track of those.

### Slices (M)
A slice is a 16-byte descriptor consisting of a pointer and a length. One can be declared like this:
````
    slice[]int S       # Lower bound 1; length and data undefined
    slice[A:]int T     # Lower bound A; length and data undefined
````
Slices are indexed from 1; this can be changed using `slice[0:]int` for example, but that's uncommon. When creating a slice *value* from an array, for example `A[3..6]`, the slice will have a lower bound of 1 with no override, and the length will be 4.

Slices can be initialised, or slice values created, in various ways:
````
    S := A[3..6]          The example above
    S := (P, N)           Construct directly from a pointer and length (this one is under review as I want
                          to be able to allow: S := (10,20,30,40), initialise slice with a constructor)
    S := A                Slice to entire array
    S := S[2..4]          Slice to a slice
````
Slices only work for 1D arrays, or the first dimension of multi-dim arrays. A slice lower bound is set at compile-time.

As implemented, an arbitrary slice of an array, like `A[i..j]`, will yield a slice index from 1. This can only be changed with a cast, or copying/passing to something expecting a different slice lower bound.

### Pointers (M)
These are defined with `ref T`:
````
    ref int P                Pointer to data
    ref proc Q               Pointer to function (in this case, a proc taking no params)
    ref label R              Pointer to label
````
and dereferenced with `^`:
````
    print P^
````
The `^` dereference can omitted for `A^[i], P^.m, F^(x)`, so that you just write `A[i], P.m, F(x)`. Multiple `^` can be dropped: `Q^^.m` becomes `Q.m`.

This loses some transparency, but it makes for clearer code, and makes some code identical to the Q equivalent where pointers are used much less.

### Records
These always need to be a named user-type. There are no anonymous, ad hoc record types. There are two definition styles:
````
    record date = (int d, m, y)
    record date =
        int d, m
        int y
    end
````
Records can contain other things besides those fields:

* Named constants
* Actual functions rather than just function pointers
* Type aliases, nested records and enumerations (to be checked...)
* Not included (yet) are static variables, of which there would be one instance belong to the type; not per instance

This does make them look a little like classes, but there are no real OOP features; it's just a way of encapsulating things, or they can be used to create a namespace.

#### Layout Control
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
This record is only 4 bytes in size, because the space is shared: `a b f` are all at offset 0. In the past I used `@` to share space in the record.
This still works; the same example would be:
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

#### Field Alignment
The compiler does not automatically insert padding to give ideal alignment to fields. This must be done manually.

However, there is a feature used to match the layout of external structs which usually follow C rules. This can be used to emulate such alignment:
````
    record rec = $caligned
        byte a
        int64 b
    end
````
Now, the size is 16 bytes, and `b` starts at offset +8. Without `$caligned`, the overall size would be 9 bytes, and `b` starts at offset +1.

### Enumerations
The simple enum feature: `enum (red, green, blue)` has been removed. The equivalent is now:
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

* `$` is a device which returns the last-created enum name as a string, eg. `"red"`, to avoid having to repeat them all as strings. (I know, this should be done automatically, but this crude approach works)
* For 0-based arrays, define the array bounds using `[0:]`, but also the first enum value must be set to zero: `red = 0`
* No override of an enums value is allowed except for the first. That would cause problems for matching parallel arrays. (If there are
  no parallel arrays and arbitrary values are need, use `const` instead.)

Older M versions allowed an umbrella type to be applied:
````
    enumdata(colours) []ichar colournames, []u32 colourvalues =
    ....
````
But this did very little, as proper enum types are not supported. All it did was to put the enum names into a namespace, so that they had to be written as `colours.red` etc. This feature was never used, so is temporarily unavailable. (A proper type system would rarely need those qualifiers, but as is clear, M likes to keep that part simple.)

### Tabledata
`tabledata` is used to define parallel data arrays (previously it did the job of `enumdata` too). It works now like this:
````
tabledata []ichar names, []int values =
    ("one",   100),
    ("two",   200),
    ("three", 300),
end
````
This defines and initialises an array of strings, and a corresponding one of int, when it is better to have them separate rather than one array of records.

It can be any combination of arrays. It simplifies maintenance, as entries can be added, deleted or moved very easily. Allowing a trailing comma on the last entry facilitates this.


### Functions
````
    proc name(params) =                 # non-value-returning
        sunit
    end

    func name(params)T =                # value-returning
        sunit
    end

    fun name(params)T = unit            # more suited to one-liners

    sub name(params) = unit             # may be dropped; see below

````
T is the return type. And optional `:` or `=>` can be just just before it.

`sub` is not so useful, so may be dropped especially since the `sub` keyword is too useful an identifier. For example in `add sub mul div`,
and it clashes with the `sub` instruction in inline assembly, although I believe I got that sussed.

#### Parameter Lists

An empty parameter list can be written as (), or omitted completely. In this case, `:` or `=>` should be used
before any return type:
````
    fun name:T = unit
````
Names in parameter lists can share the same type:
````
    (int a,b,c, real x)
````
Parameter names are always needed, except for FFI declarations.

Currently, M restricts integer parameters or return values narrower than 64 bits. They are only allowed in FFIs.

#### Reference Parameters

These are written with a & before the name:
````
    (int a, &b, c)              # b is passed by-reference
````
By-reference means that & is automatically inserted on arguments, and derefs are automatically applied within the function.

There are some restrictions:

* It is not possible to have default values for reference parameters (so they can't be optional)
* The type of the argument must exactly match; you can pass a byte variable to a function expecting an int,
which is normally widened. The parameter type is really `ref int`, which is incompatible with `ref byte`.
* Variables passed by reference cannot be stored in registers (this affects the way code can be optimised)

(I used to have attributes `in`, `out`, `inout` for parameters, but they were never used. I think `in` and `out` still work,
but I will remove them.)

#### FFI Parameter Lists

These occur in `importdll` blocks, which can contain function *declarations*.

Here, parameter names are optional. Without them, only the types are written, and types cannot be shared:
````
    (int, int, int)
````
Parameter names are recommended however:

* They allow sharing of a common type
* They can be used for keyword parameters
* They can have default values applied
* They can be declared by-reference

This is regardless how they might have been declared in their original source code.

#### Default Parameter Values

They are specified like this:
````
    (int a, b, c = 0)
````
This allows the third arg to be omitted at the call-site, but the compiler will generate the 0 value needed. Calls are always
made internally with the full number of arguments.

For complex expressions, any names used, for example `x` in `c = x + 1`, are resolved locally: `x` is whatever is visible at
the function definition, not at the call site.

Only trailing parameters can be omitted. So this is not possible:
````
    proc F(int a, b = 0, c) ...
    F(10,,30)
````
(I've just checked, and the compiler will not reject this call. In Q it is rejected. So I will need to fix that. A default
value for a middle parameter is OK; that's used for keyword parameters.)

#### Keyword Parameters

This is being able to name arguments, so that they can be passed in any order:
````
    F(c:100, a:200)
````
This calls the above function. `b` is not provided; that has a default. Positional (regular unnamed parameters) and named can
be mixed, but positional must come first:
````
    F(200,c:100)        # same call
    F(c:100, 200)       # not allowed, as it is not known which parameter the 200 is for.
````
Keyword parameters can be applied to FFI functions, even when the original definitions don't use them.

#### Variadic Functions

These are functions with an arbitrary number of parameterd. They are not supported in M, only within the FFI for C-style
variadic functions.

There had been a proposal for a similar feature, but using a fixed type for each variadic parameter:
````
    proc F(int a, b, x ...)
````
Here, `x` is the first of N possible arguments, including zero, each of type `int`. This would have been syntactic sugar around this:
````
    proc F(int a, b, slice[]int x)
````
which could then have been called as:
````
    F(10, 20, 30, 40, 50)
````
which dispenses with the need to write the slice as (30, 40, 50). Within the function, `x` is accessed as a regular slice.

I decided there was no great benefit in eliminating explicit slice syntax.


### Assignment

The LHS can only have a set number of forms:
````
    name :=                     Simple variable
    term^ :=                    Pointer
    term.m :=                   Record field
    term[expr] :=               Array element
    term.[expr] :=              Bit or bitfield of integer (depends whether expr is a range)
    (term, term, ...) :=        Multiple assignment
````
It can also be a more complex term that yields any the of above, for example:
````
    if a then b else p^ fi :=   2-way select, assign to b or p^
````
But these are rarely used, and have fallen into disuse. They need to be checked out.

For multiple assignments like this:
````
    (a, b, c) := (x, y, z)
````
The counts must match on both sides. The usual promotions are applied. Note that this doesn't simply do `a:=x; b:=y, c:=z`, RHS values
are notionally copied to temporaries, so that this works as expected:
````
    (a, b) := (b, a)
````
This exchanges the two values. Or values can be rotated: (a, b, c) := (b, c, a).

Multi-assignment lists can't be nested: (a, (b, c) := (x, (y, x)). There usually isn't much point.

Sometimes the multiple assignment is only on the LHS:
````
    (a, b, c) := F()
    (a, b)    := F()
    a         := F()
                 F()
````
Here, F should return multiple values. Either all three are unpacked to the three values, or only two and one is discarded, or two
are discarded, or all of them. (F can also return more than 3 values here.)

Further examples are:
````
    (ptr, length) := S      # when S is a slice, it unpacks
    (d, r) := a divrem b    # does divide and remainder in one operation
````
I'd need to check whether these still work (every new implementation leaves the hard or little-used features until last, and often I
forget).

It is not possible to do this:
````
    (a, b, c) := X          # X is a record of 3 fields, or array of 3 (or perhaps more) elements.
````
This is stuff that Q should be used for. It needs to be allowed for the F() examples, as there is no alternative, but X can easily be
written as (X.d, X.m, X.y) for example.

### Promotions and Conversions

All integer arithmetic is done on `int` or `word` types. Any narrow types are widened to 64 bits for any evaluation:

`u8 u16 u32 i8 i16 i32` can all be converted to the primary `i64` type with no loss of information.

Floats are evaluated using either r32 or r64. There is no promotion of r32 to r64 except for mixed r32 and r64, or passing r32 to anything
expecting r64.

### Operators

#### Binary Operators
````
x + y       int, real, pointer+int, ichar (constant only)
x - y       int, real, pointer, pointer-int
x * y       int, real
x / y       int, real (in Q, / is real only)
x % y       int (integer division)
x rem y     int
x divrem y  int; yields two values (x % y, x rem y)
x ** y      int, real
x iand y    int
x ior y
x ixor y
x << y
x >> y
x min y     or min(x,y); int
x max y
x and y     bool
x or y
x atan2 y   or atan2(x, y)
````
For mixed int/real, int operand is converted to real. `int` here means either `i64` or `u64`.
````
x = y       Any type, yield bool
x <> y
x < y       int, real, pointer
x <= y
x >= y
x > y
````
These take any type but need to be compatible. Promotions or conversions between ints and floats are done as required.
````
x in y      int in range/set only, yield bool
x notin y
x not in y

x .. y      Only used in certain contexts, counts as syntax, not a true operator
````
#### Unary Operators
````
- x         int, real
abs x       int, real
inot x      int
not x       Any type
istrue x    Any type
sign  x     int, real (returns -1, 0 or 1, always i64)

&x          any type, yields pointer  (these two are syntax rather than operators)
x^          pointer, yields target type

sqr x       int, real
sqrt x      real
sin x
cos x
tan x
asin x
acos x
atan x
log x
log10 x
exp x
fmod x
round x
floor x
ceil x
````
Note: this last group are implemented as built-in operators rather than functions. So they don't need parentheses around their operand,
but it looks better if they are used.

#### Chained Operators

Any of = <> < <= >= > can appear in a chain:
````
    if a = b = c
    if a <= b < c
````
Middle terms are evaluated only only. The whole yields bool. It is suggest that if any of `< <= >= >` appear, then they
all face the same, otherwise the meaning is obscure.


#### Augmented Assignment

Augmented assignments to not return any value so cannot 

Binary
````
x +:= y     int, real, pointer +:= int
x -:= y     int, real, pointer -:= int
x *:= y     int, real
x /:= y     int, real
x %:= y     int
x rem:= y   int
x iand:= y  int
x ior:= y   int
x ixor:= y  int
x <<:= y    int
x >>:= y    int
x min:= y   int, real
x max:= y   int, real
````
The RHS is promoted or converted to LHS type. These do in-place updates in memory, so sizes can be 8-32 bits.

Unary:
````
++ x        int, pointer
-- x
x ++
x --
-:= x       int, real; x := -x
abs:= x     int, real
inot:= x    int
not:= x     bool
istruel:=x  bool
````
#### Properties

`x` can be an expression, or a type
````
x.len       array, slice    Number of elements in array or slice
x.lwb                       Lower bound
x.upb                       Upper bound
x.bounds    array           Same as x.lwb..x.upb in some contexts
x.bytes     Any type        Bytes of memory occupied by x
x.bitwidth  int, real       Number of bits used by that numeric type
x.min       int             Mininum value of type
x.max                       Maximum value of type
````
For `.bytes .bitwith .min .max`, if x is simple variable name, it is not promoted to i64 or u64 (CHECK THIS FOR FIRST TWO).

#### Binary Operator Priorities

1 is highest precedence:
````
Level   Operators at that level

1       ** (right to left)
2       * / % rem divrem << >>
3       + - iand ior ixor min max
4       ..
5       = <> < <= >= > in
6       and
7       or
8       := (right to left)
````
* `min max` normally use function-like syntax
* The "." in `a.b` is not a operator; it has syntax that binds more tightly than even unary operators

#### Unary Operator Priorities

This is evaluation order rather than precedences. Some operators go before the operand (prefix), and some after that (postfix):

* Any postfix operators are applied first, in left to right order
* Then any prefix are applied, in right to left order

So for `sqrt -p^^`, `p` is deferenced twice (first `^` then second), then it is negated, then `sqrt` is applied.


### Statements

`s` is an Sunit (sequence of units); `u` is single unit. A unit is a single expression or statement.

#### `If` Statement (all can actually return values):

    if s then s end
    if s then s else s end
    if s then s elsif s then s else s end       # etc

#### 2-Way Select (uses an `if` syntax).

`a b c` are all single units:
````
    x := if c then a else b fi                  # either fi, end or end if can be used
    x := (c | a | b )                           # compact syntax
````
#### N-way Select:
````
    x := (n | a, b c, ... | z)                  # There is only a compact version
````
`n` is a 1-based index, select one of `a`, `b`, `c` etc. Use `z` if out of range. Only one expression will be evaluated.


#### `Unless Statement`
````
    unless s then e end
    unless s then s else s end
````
#### `Switch` Statement
````
    switch x            # integer index
    when a, b then      # must be compile-time expressions
        s
    when c then
        s
    else                # optional else part
        s
    end
````
All the values in when-lists should not span a range of more than 500 from lowest to highest. If exceeded, `case` should be used.

Any `u` following `when` can be a range, eg. `when '0'..'9'`.

#### `Case` Statement

This has mostly the same syntax as `Switch`:
````
    case c              # Can be any time for which = and <> can be used
    when a, b then      # Can be runtime expressions
        s
    when c
        s
    else                # optional
        s
    end                 # end case or esac can be used
````
Differences from `switch`:

* Tests are done sequeniall, one at a time, instead of all in parallel
* Duplicate `when` expressions allowed (or rather, they are not detected)
* No ranges allowed
* Control expression can be any type
* `when` expressions can be runtime values

#### Empty `Case` Statement

This is an alternative to an `if-elsif` chain:
````
    case
    when a, b then
    when c then
    etc
````
This equivalent to:
````
    if a or b then
    elsif c then
    etc
````
`a b c` can be any conditional expressions.

#### Composite `if`\`switch`\`case

These three statements can be chained together insteading of nesting them using `elsif elseswitch elsecase` like this:

````
    if w then
    elsecase x then
    when a, b then
    elsif y then
    elseswitch z then
    when d then
    when e then
    else
    end if               # always matches the opening keyword
````
This is equivalent to:

````
    if w then
    else
        case x then
        when a, b then
        else
            if y then
            else
                switch z then
                when d then
                when e then
                else
                end switch
            end if
        else case
    end if
````
#### `Do` Loop
````
    do                  # endless loop
        s
    end                 # also end do or od
````
#### `To` Loop
````
    to u do             # repeat N times
        s
    end                 # also end to or od
````
#### `While` Loop
````
    while s1 do
        s2
    end                 # also end while or od

    while s1, s2 do     # s2 is optionaly executed at end of iteration
        s3
    end
````
#### `Repeat` Loop
````
    repeat
        s
    until u
````
#### `For` Loops

Iterate over integer range:
````
    for i := a to b do          # Simple iteration over a to b inclusive
    for i to n do               # Start from 1 if start value omitted
    for i := a downto b do      # Count downwards (usually a > b)
    for i := a to b by c do     # Step index by c (must be positive constant) instead of 1
    for i to n when u do        # Execute iteration only when u us True
````
Iterate over values:
````
    for i in a .. b do          # This one is same as for i:=a to b
    for x in A do               # Iterate over A's values. A's length must be available
    for i in A.bounds do        # Iterate over A.lwb to A.upb inclusive
    for i, x in A do            # x iterates over values, i also iterates over index

    for i in a..b do
        s1
    else                        # optional else branch executed on normal exit, not on early break
        s2
    end                         # Can also end with 'end for' or 'od'
````
The loop index (`i` or `x` in examples) will be automatically declared as local if the name is not already in scope. In that case,
it will choose a suitable type (`i64` for `i`, the array element for `x`), and will declare with `let`, which means it cannot
be updated within the loop.

The final value will be accessible after the loop.


#### `Doswitch/DoCase` Loops

These are looping versions of `switch` and `case`, for example:
````
    doswitch x
    when a, b then
        s
    when c then
        s
    else
        s
    end 
````
Use `doswitchu` for a switch implemented with `computed goto`, which can be faster for some applications. However, out of range
when-values are not trapped; `else` detects only values within gaps of the overall range.

It is necessary to use `goto return exit` or `stop` to break out of the loop.

For loop controls, see Control Flow.


### Control Flow
````
    goto label                      Jump to label
    goto expr                       Jump to label pointer which is stored in some variable or table
    recase x                        In nearest case unit only, jump to `when x` block
    return                          In a proc
    return expr                     In a function
    return (expr,...)
    stop                            stop 0
    stop expr                       Terminate program with that exit code
    exit [loop index, all]
    redoloop [loop index, all]
    nextloop [loop index, all]
````
Loop indices are number from 1 (innermost) to outermost. Index 0 (or `all`) can also be used to refer to outermost loop.

Note: `goto label` (no suffix) can also be written as just `label`, without the `goto`.

`goto` itself can be written `go to`, although that's been the case forever, but I've used it.


All can have a conditional suffix:
````
    goto L when a=b
    return unless x=0
````
(`when` is used instead of `if`, since `if` could also start any expression that follows the keyword so would be ambiguous.)

### Read and Print
````
    print a, b, c           Display 3 items with space in between items, to console
    print a,,b, c           No space between a and b
    println a, b            Add newline at end
    print a, $              Display a and final space (same as print a,," ")
    print $, a              Display space then a (same as print " ",,a)
    print @f, a, b          Write a, b to open file handle f
    print @s, a, b          Write a, b to char array s
    print @0, a, b          Same as console print
    print =a, =b            Display items with label, eg. A=12 B=24

    fprint "A=# b=#", a, b  Using a format string (fprint has -ln and @ variations too)

    print a:"z 10 jr h"     Display a within a field of width 10, right-justified, zero-padded at left,
                            and in hex if this is a number


    readln a, b, c          Wait for line of input from console then read vars a, b, c from that line buffer
    read d, e               Read d, e from current position in same buffer
    readln                  Read line of input only, use read to read items from it
    readln @f               Read line from open file f into buffer
    readln @s               Read line from string s into buffer

    read a:"h"              Read integer as hex
````
(Formatting codes for `read` and `print` are documented inside msys.m.)

Note: there is only one line buffer for reading. It's not possible to interleave `read` from more than one source at a time.
The whole line must be consumed before switching.

`print` statements can be nested:
````
    print "A", f(), "B"
````
Here, `f()` can itself execute `print` statements, whose output follows "A", before its return value is printed before "C".

### Ranges and Sets

Ranges written `a .. b` are used like this:
````
    [a..b]int x             Specify array bounds
    for i in a..b do        For-loop iteration
    if x in a..b then       Testing something is in range
    when a..b then          Part of switch statement
````
The range is always inclusive. `a`/`b` can any integer expressions, including enum values. For array bounds, they need to be compile-time expressions.

`A.bounds` yields a range `A.lwb .. A.upb`, but it can only be used like this for now:
````
    for i in A.bounds do    (Using for i in A will iterate over A's values)
````
Ranges are not first-class objects in M (they are in Q).

A Set is used only in conditions like this:
````
    if x in [a, b, c] then
````
Equivalent to: `x=a or x=b or x=c`, except that `x` is evaluated once. (In Q, sets are first-class objects, bitsets than can contains ints and ranges.)


### Bits and bitfields

The language favours special operators for bit access rather than special types:
````
    A.[i]           Extracts bit i from `i64` value A. It will be 0 or 1. bit 0 is least significant
                    and bit 63 the most significant

    A.[i..j]        Extracts the bitfield i..j inclusive from A. Usually i<j that that A.[0..7] extracts
                    the bottom byte, but for constant i and y, the order can be reveresed: A[7..0]

    A.[i] := x      Store x in bit i. x should be 0 or 1 only (for constant x, it will be masked, but
                    not for variable x)
    A.[i..j] := x   Store value x in bitfield i..j. Again, x should not exceed the field width.
````
Some other variations were tried in the past, but currently are not used:
````
    A.byte[i]       Extract byte i from i, with being 0 to 7. I think other types were possible like
                    A.u16[i].
    A&.[i]          Similar to A.[i], but the bit is not shifted down. The result of A&.[8] will
                    be 0, or 256, instead of 0, or 1.
````
Bitfield objects also exist, but only inside records:
````
    record demo =
        byte flags: (a:1, b:1, c:4)
    end
````
Bitfield can only be defined inside a regular field. In my example, 6 bits are used out of 8 (they are allocated
from bit 0 up):
````
    demo x
    x.flags:=0      Access all 8 bits once
    x.b             Access bit b only
````
Bitfields can only store unsigned value, but are promoted to i64 when extracted. (Unless extracting all 64 bits of a u64
field, but these points need checking.)

I haven't find a way to use named bitfields like this, to access the bitfields in an integer. The nearest is this:
````
    macro offset = 0..23
    int a
    a.[offset]              The square brackets are needed
````
(I can't define `offset` as `[0..23]` then do `a.offset`, since name resolution of `.offset` doesn't work with the macro expansion
needed. Allowing macro expansion here, would interfere with record field accesses right across the program, for example if there
was a record that had a conventional field called 'offset'.

This is an issue also with C's macro scheme; write `#define x 1234`, and any `p.x` or `p->x` access would be screwed up.
But usually C macro names are in upper case.)

(The Q language has bit arrays, with elements of 1, 2, and 4 bits (types `u1 u2 u4`); bit pointers; bit slices of arbitrary
length and Pascal-style bitsets.

Some of those could make sense in systems language like M, but they are little used and probably not worth the effort.)

#### Some Built-in Bit/Bitfields
````
    A.even          1 if A is even (bit 0 is 0) (read-only)
    A.odd           1 if A is odd (value of bit 0) (read-only)
    A.lsbit         A.[0]
    A.msbit         A.[63]
    A.lsb           A.[0..7]
    A.msb           A.[56..63]
    A.lsw           A.[0..31]
    A.msw           A.[32..63]
````
All are read/write except the first two

### Importing via FFI

Interfaces are required at the boundaries between a program in M, and external libraries. Those libraries are generally
in a shared library.

To use the library, M need a set of bindings - function declarations written in M syntax, and inside an `importdll` block:
````
importdll msvcrt =
    func puts(ichar)int32
end

importdll $windows =
    func "MessageBoxA"(int32 a = 0, ichar message, caption = "Caption", int32 flags = 0)int32
end
````
The library name is that of an actual one (`msvcrt.dll`, which is specified as an import in the EXE), or a dummy one if it
starts with "$". This is used when the DLL name is unknown or unclear, or the set of functions exist across multiple DLLS.

But then the actual must be one of those specified with `linkdll` in the lead module. It will search all DLLs until it finds each imported
symbol. (The M compiler automatically looks inside `msvcrt gdi32 user32 kernel32` DLLs.)

Notice that the MessageBoxA declaration includes parameter names and default values. This allows the use of keyword parameters even though the original
MS function didn't support such a feature:
````
    messageboxa(message: "Hello There")
````
#### The Import Module

Usually such an import block is written in a dedicated module that only contains that interface. Each imported name
is also exported from the module so that it is accessible to other modules (and I believe to other subprograms), and also without needing to qualify the name.

If `puts` from `msvcrt` is defined inside a module `clib` for example, then it can optionally be called as `clib.puts`, not msvcrt.puts. The
DLL name is not the owner module.

(This applies to modules in a subprogram. From outside a subprogram called `S`, then `S.puts` should be used. Actually `puts` is defined
inside a module `mclib`, which part of a subprogram `msyslib`, so `msyslib.puts` is the full name, but that is not working so needs
checking out.)


#### Creating Import Modules

Creating those bindings for an arbitrary library is a big obstacle. There are three main approaches:

* Write them manually. This is not as labourious as it sounds: you just define functions, plus types, enums etc that are needed,
not all 10,000 functions in a big library. But this is not suitable for creatung an import module for general use.

* Use some automatical tool to generate them. I have a feature in my M compiler that can do 90% of the work in translating an API
expressed as C headers, into M syntax. (And Q syntax also; I expect most libraries to be used from Q rather than M, but the same issues apply.)

* If the library is itself written in M, then then the DLL is generated, it will automatically create the import module suitable for M or Q. But
even these needs more work. (If could also generate a C header file I suppose, but that part is not useful to me.)_

### Conditional Compilation

There is no conditional compilation within the language. Although, if `c` is constant in `if c then a then b end`, then only one branch will generate code.

For different program configurations, the usual approach is to use an alternate lead module with a different set of module names.

### Standard Library
This comprises the following modules:
````
    Module      Description

    msyslib     Lead module used with 'import'
    msys        Support library
    mlib        File and memory functions
    mwindows    OS-specific functions
    clib        Import some C functions
    mwindll     Runtime DLL access. This provides a simple solution to the 'LIBFF' problem.
````
`msyslib.m` contains only module info:
````
    module msys
    module mlib
    module mclib
    module mwindows
    module mwindll
````
It would normally be specified in an application using:
````
    import msyslib
````
But the standard library always is always imported unless disabled. In any case, `import` wouldn't work as it has to look in a special
place for the library source files.

The standard library is statically compiled into the app. It ought to be usable as a separate DLL, but the auto-generation of the
interface module that would be needed needs some work. ATM simplest to build it in to app; it adds 30KB, but means no DLL dependency.

`-nosys` will compile without the library, and `-minsys` with a minimal test one. These options are mainly used for compiler development.

I don't at the moment have docs for the standard library, only source code. Functions and other entities marked `export` are visible from
applications.

### Macros
````
`macro name(params) = unit
````
A single unit is not that limited, as you can just do `(sunit)` is a sequence of units is needed.


### Naked Functions
````
`threadedproc`
````
Threaded functions can't have parameters or local stack-frame variables (statics are OK). There is no entry nor exit code generated.
They can be called, but there is no return. Normally you jump to a threaded function, and jump from that to the next, using ASM code.


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

### Compiler Constants

Built-in constants.
```
                Type        Meaning or value

nil             ref void    Null pointer value, all zeros
pi              real        3.14159...
infinity        real        IEEE754 +infinity
true            bool        True or 1
false           bool        False or 1
$targetbits     int         64 (machine word size in bits)
$targetsize     int         8 (word size in bytes)
$targetcode     ichar       wx64  (Windows x64)
$lineno         int         Current source line number
$strlineno      ichar       As a string
$filename       ichar       Sourcefile with path if one provided to lead module or via $sourcepath
$modulename     ichar       Module name only
$function       ichar       Name of current function (or module name if outside function)
$date           ichar       Date compiled
$time           ichar       Time compiled
$version        ichar       Compiler version info
```

### Function Tables
M used to generate a table of all function used in a program. Info about them could be obtained with:
````
n := $getnprocs()           Number function in the table (int)
s := $getprocname(i)        Name of i'th function (i is 1..$getnprocs()) (ichar)
a := $getprocaddr(i)        Address of i'th function as a (ref void)
````
However, the table used to take a lot of space, about 10% of an EXE file. So now only marked function 
are put into the table:
````
proc F*=
end

proc G=
end
````
Here, only `F`, marked with `*`, will be present in the table. Normally it is sets of handler functions which are marked
like this. Then sometime init routine will scan them and set up variable handler tables, without needing to maintain them manually.

### Block Parameters and Return Types

This language used to pass arrays and record by actual value. That is, older implementations with their own ABIs. (Although
I never once made use of that feature.)

Modern ABIs will internally used references to do that. To make it appear as though they are passed by value (eg. allow callees
to modify the data without affecting the caller's copy), requires making copies.

ATM M doesn't do that (except for arrays/records that are 1/2/4/8 bytes in size: the ABI says they are passed via registers).

An easy way around that is to say that that is how the language works. (Python has something similar: callees can modify lists
belonging to callers.)

However I think this should be addressed: a copy should be made. If someone wants the reference-like behaviour, then they
explicitly use by-reference parameters.

### Miscellaneous Features

##### Clamp

`clamp(x, a, b)` is a built-in 3-operand op equivalent to `min(max(x, a), b)`.

##### Last Array Element

`A[$]` is equivalent to `A[A.upb]`, so accesses the last element of `A`. `A[$-1]` will be next-to-last.

##### Equivalence
````
    real x
    int a @ x
````
`@` indicates that `a` shared the same memory as `x`. Both should be the same class (eg. both static, both stack-frame, but not checked ATM).

`a` can be smaller than `x`, but not bigger.

This feature used to be more elaborate, for example:
````
    [10]byte a
    u16 w @ a[3]
````
So accessing `w` is the same as accessing `a[3]` and `a[4]`. I had been meaning to drop it completely, but it is still useful, for example this
is from an interpreter:
````
    [stacksize]int      stack
    [stacksize]real     xstack @stack
    [stacksize]word     ustack @stack
    [stacksize]ref void pstack @stack
````

##### Operator Constants

Writing `(+)`, or any other operator inside brackets, returns an internal ordinal (an integer) representing that operator.

This is no big deal, you could just use `'+'` for example, but it is a nice touch. And in Q, the value is recognised by the language:
`mapss((+), 3, 4)` yields `7`.

##### Type Constants

Similarly, writing `(int)` (or `int.type` for simple types), yields an internal ordinal for the type. It can be used like this:
````
    if T = int.type then
````
where `T` has such an ordinal value.

##### Type Reflection

There is some use for this even in a static language:
````
    X.type       Returns the type ordinal as used above
    X.typestr    Returns a string such as `"i64"` and so on
````
`X` can be type or expression. Parentheses may be needed around `X`, when it is a number for example.

### Compiler Outputs

Assume the lead module is called prog.m:

````
    Option      Output          Notes

    -exe        prog.exe        Default option
    -dll        prog.dll        Also prog.q or prog_lib.m depends on config
    -obj        prog.obj        Generates prog.asm then vnvokes aa.exe assembler
    -ml         prog.ml         Via assembler; private shared library format
    -mx         prog.mx         Via assembler; private executable format
    -asm        prog.asm
    -ma         prog.ma         Single, compilable amalgamated source file representing whole program
    -run        <Run prog>      Generates in-memory code and runs the program
    -list       prog.list       To help my IDE
    -proj       prog.prog

    -pcl        prog.pci        Using mmp.exe only, this is the default option there

    -c          prog.c          Using mc.exe only
    -gcc        prog.exe        Using mc.exe: generate prog.c then invoke gcc
    -opt        prog.exe        Using mc.exe: generate prog.c then invoke gcc -O3
    -tcc        prog.exe        Using mc.exe: generate prog.c then invoke tcc
    -mcc        prog.exe        Using mc.exe: generate prog.c then invoke mcc
````
`mmp.exe` is experimenatl. `mc.exe` is deprecated, and will only work for a language subset.


### Amalgamations

The -ma options takes the lead module prog.m, and produces a single prog.ma text file that contains all
source files and all supported files. The file can be compiled directly:
````
c:\cx>mm -ma cc
Compiling cc.m to cc.ma
Writing cc.ma

c:\cx>mm cc.ma
Compiling cc.ma to cc.exe
````
Note: embedded binary files won't work at the minute. Any file paths are flattened, so that any clashing file
names won't work.

Standard library source files are not included; they must be part of mm.exe, so there is a small risk they may be a
different version to that expected by the code in the MA file.

### Run from Source
Using the `-run` compiler option will run programs from source just like a scriting language.

To void using `-run`, it is possible to copy the compiler `mm.exe` to `ms.exe`; it will detect this name, and invoke `-run` automatically:
````
c:\cx>ms cc hello
Compiling cc.m to memory
Compiling hello.c to hello.exe
````

### Interpreting
There is an experimental M compiler called `mmp.exe`. It produces a discrete IL source file with extension `.pcl`.

There is a separate interpreter program called `pci.exe`. This is the same `cc` project above run via the interpreter:
````
c:\cx>mmp cc
M7 Compiling cc.m to cc.pcl

c:\cx>pci cc hello                              # runs cc.pcl
Compiling hello.c to hello.exe
````
It works the same, just 20 times slower: it takes the interpreted C compiler 5.5 seconds to compile sql.c to sql.exe instead
of 0.6 seconds; it's still faster than `gcc -O0` which takes 8.5 seconds! (However the project uses windows.h and gcc's version is
much bigger. So the timing of my /interpreted/ C compiler is on a par with gcc-O0.)

### Transpiling to C
There is program called `mc.exe` which tried to convert M programs into monolithic C source files.

It doesn't do the full job, and doesn't support such features as:

* Statements used as expressions (returning values), or statements embedded within expressions
* Slices
* Inline assembly
* The MWINDLL.M module that relies on inline assembler is replaced with a more limited MWINDLLC.M module

There are dozens of small issues too. However, it usually suffices to convert my main language projects, since I deliberately
avoid troublesome features, and serves for experimental work or to apply optimisation to a project to see what the upper limit
of its performance is, or to compare with other products.

### High-loading and Position-Independent Code

Until recently, the `mm` and `aa` products only generated code that ran in low-memory. That is within the first 2GB of virtual memory,
which could be accessed with positive values of a signed displacement.

This caused some problems: DLLs generally have to be relocated, and might be moved anywhere. While if I generated OBJ files, those
would need to be processed by external linkers. The only one available was gcc's 'ld', and that now likes to load programs into
arbitrary addresses in high memory, requiring more than 32 bits of offsets.

Fixing this required two steps: using RIP-relative address mkodes for absolute addresses not involving registers; and avoiding
address modes that combined registers with the 32-bit absolute address of a static object.

The first is enabled in the M compiler with the `-rip` option, or can be applied to the assembler. This is not enough to run code high,
but it means programs might be 1% smaller since the rip-relative address modes have shorter instructions.

To do it all requires the `-himem` option that affects the code generator. This often means extra instructions which offsets the savings of -rip.

Unless a smaller low-loading EXE is required, these options are applied automatically when `-dll` or `-obj` options are selected.


### Generating DLL Libraries
An M 'program' can compiled to a dynamic library. The `-dll` is used to produce a `.dll` file instead of `.exe`.

Libraries need to export names, and that is done using the `export` attribute on functions rather than `global`.

Variables can also be exported (but currently M has trouble importing variables from a DLL).

As well as a DLL file, a `.m` or `.q` import module is written (depending on which is configured within `mm.exe`; currently it can't do both).

If the library lead module is `bignum.m` then either `bignum.q` or `bignum_lib.m` is written (the latter to avoid overwriting `bignum.m`).
Then the DLL can be used in either language by simply adding `module bignum`, or `module_lib`. (Q of course has bignums built-in; this is just
an example.)

Other entities can be exported: named constants, records, user types, macros, enumerations. These are not tangible objects that are handled
by the DLL mechanisms, but by the language. In theory they should written to that exported import module, but it is not yet working 100%.

(The import module of course is only usable from my two languages. To use the DLL from C, could benefit from a `bignum.h` file being generated,
but since I'm the only user and I don't need to write C, I haven't done that.

C headers would need writing manually, but the `bignum_lib.m` or even `bignum.q` files containg a tidy summary of what would be needed in the C file.)

### ML and MX Files

For a while, my DLL files were buggy. I assumed it was because of the high-loading issue, but that's only part of it. There was also a big
in the base-reloc tables, now fixed.

But while they were out of the running, I developed my own far simpler shared library format called ML files. These could be used directly from
Q programs, but not directly from EXE programs, except via a mechanism using functions similar to `LoadLibrary/GetProcAddress`.

Or they could be used directly from MX programs. MX is a version of ML used to represent whole programs: MX does the job of EXE, and ML does the
job of DLL.

Except since Windows doesn't understand MX, that need a stub program to launch, and small 12KB program called `runmx.exe`.

Now that DLLs and also OBJs are working, there is less need for ML and MX formats, but they still have interesting possibilities:

* ML files make it much easier for M to access exported variables
* MX formats might be less visible to AV software, which will see only the RUNMX program; the MX part is just data
* The RUNMX program also exists as 1K line C program
* If I ever target Linux directly, then I don't need to generate ELF files, as the MX format is portable. I just need to compile
  runmx.c on Linux

In any case, half the work needed to for MX/ML, is still required to enable run-from-source.

### Self-hosting
The M compiler is self-hosting as it is written in itself. It can compile itself like this, where `mm.m` is the lead module of the project,
containing the main configuration:
````
c:\mx>mm mm
New dest= mm2.m
Compiling mm.m to mm2.exe
````
If there is already an `mm.exe` present as it the case here, it renamed the output to avoid a clash (Windows disallow overwriting
a running executable).

A more dramatic example is possible by first copying `ms.exe` to `mm.exe`, and copying `mm.m` to `ms.m`. Now each new generation can be
reinvoked to compile again:
````
c:\mx>ms ms ms ms ms ms hello
Compiling ms.m to memory
Compiling ms.m to memory
Compiling ms.m to memory
Compiling ms.m to memory
Compiling ms.m to memory
Compiling hello.m to memory
Hello World!
````
This compiles 5 new generations of the M compiler and runs each immediately. This took 0.37 seconds in all.

For this demo, the `Compiling` message is enabled to show what's happening. Usually `ms` hides it to better give the illusion of a true scripting
language.

### Using Linux
This is only practical via the MC transpiler. That normally generates C code for Windows, but has the -linux option to generate code
for Linux (it replaces the OS-specific mwindows.m module with mlinux.m).

This allows cross-compiling of M programs on Windows, to C programs for Linux, subject to the limitations above.

If applied to the MC project itself, I can have an MC compiler running on Linux, where I normally call it MU to avoid confusion, which
can build M programs directly under Linux, by using intermediate C and transparently invoking a C compiler. Preferably Tiny C, otherwise
gcc is so slow that it would be like running into a brick wall after transpiling M to C.

It was been applied to the QQ compiler, and at one time I was planning to port my Q apps (editor etc) to Linux, but lost interest.
(That was using RPi boards that was tedious; it would be simpler until WSL, but there is also less point.)

### Interfacing to Q
This is a work-in-progress. I want to get to the point where I can write a Q application such substantial helper parts in M,
or write an M application with some embedded Q parts.

But I'm not currently writing either kind of application.

There have been all sorts of projects, including creating a hybrid M-Q language (which ended up have disadvantages of both rather than the advantages!).
And a version of Q that had embedded M source file. That one just got too hairy.

In the end I realised a simple, purer M language, and a pure scripting language, were better.

### Optimising
There is no real optimiser. There are weak attempts enabled by these options:
````
-peep       (Also -o1 or -O1 soon) Peephole optimiser
-regs       (Also -o2 soon) Store locals in registers
-opt        (Also -o3 soon) Do both
````
The main affect on real programs is to make it smaller. Speed is little effected. Optimising might make programs 10% faster, max, but slows
down compilation by 10%. 

However some benchmarks will run faster. And any specific function called from Q for example will benefit from the `-regs` option.

Note: using of `-regs` will not affect functions that use inline assembly. This is to ensure predictable locations of locals and params.


### Building, and Project Files
There is no separate build process necessary to turn an M application into a single `.exe` or `.dll` file, you just do this on the `prog.m` lead module:
````
    mm prog                    # -exe is default option
    mm prog -dll
````
However, the project will still usually consist of dozens of source and support files. That will still need a development process.
I don't consider such a thing to be part of a language, or even a compiler: everyone will have their own process and their own prefered tools.

I normally work with a tiny 36KB IDE. It works from a project file listing the modules, other files that form part, misc related files, and that give run and input options for testing runs.

So there is a connection. The project will list the modules in `prog.m`, and the IDE might use a project file `prog.pro` containing
some common information.

I haven't joined these together, except there are compiler options `-proj` and `-list` to produce info files usable from my IDE only.


### Working with other Languages and Tools
There are ways to mix M with other languages, other than using DLL or generating DLL. It involves using OBJ files and tools like gcc:
````
c:\cx>mm -obj cc
Compiling cc.m to cc.obj
c:\cx>gcc cc.obj -occ.exe
c:\cx>cc hello
Compiling hello.c to hello.exe
````
This turns my C compiler into an object file which is then linked by gcc. Or an M module like this (test.m):
````
export proc fred=
    println "M says 'Hi!"
end
````
can be statically linked into a C program like this:
````
extern void fred(void);

int main(void) {
    fred();
}
````
Example:
````
c:\c>mm -obj test
Compiling test.m to test.obj
c:\c>gcc c.c test.obj -oc.exe
c:\c>c
M says 'Hi!
````

### Compiler Stress Testing

Very, very little of this is done. For example what happens here:
````
[1 billion]int A         # at module scope
````
As it happens, nothing, except that it produces an EXE file with a 'zdata` (bss) section which is -589MB in size. Windows refuses to run it.

It should either detect it, or deal with it sensibly. There are dozens of such cases that need to be tested for and properly handled.

But because this is a personal tool, it is not that important. I will either not write such code, or I will soon find out it doesn't work!

That is the compiler however; a language spec should give some idea of what's allowed and what isn't. Why shouldn't someone have a billion-int
array, especially with enough RAM?

Getting back to that array, this: `[500 million]int32 A` produces a valid section size, but it still won't run. But then, the same program in C compiled with gcc won't work either. So the limiting factor here might be the EXE format. A 250M i32 array works.

There are a number of hard limits within the compiler, and these should really be documented above. At least, they are mostly tested for.
