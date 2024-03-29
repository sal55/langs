## M Language and Compiler Reference

This summaries the 2024/6.24 version of of my 'M' systems language.

It is a collection of lists, tables, facts, examples and speculation on possible new ideas.

I want to ensure that the implementation works as described here. (Often I'm loathe to use
a feature as I can't remember how well it is supported if at all. Sometimes features had once
worked but no longer.)

So they will either be fixed, or they will work subject to limitations, or removed completely.


### Overview
The language has been volatile recently as I try different ideas. This summarises 6.4:
````
- Expression-based rather than Statement-based
- Targets x64 processor running Windows using the Win64 ABI
- 64-bit default floats, ints, pointers
- Uses 'PCL' stack-based intermediate language
- Simplified 2024 Module scheme
- Supports EXE DLL directly, and OBJ, MX/ML via ASM and AA assembler
- Can generate high-loading and relocatable code for DLL/OBJ targets
- Has `-regs` and `-peep` 'optimiser' settings (however there is no real optimiser)
- Can export to either M or Q import modules for DLL, but both need more work
- Can run from source
````

### Hello, World
````
proc main =
    println "Hello, World!"
end
````
Build the `hello.m` program using:
````
mm hello
````
This produces a 27KB binary (which includes the standard library, needed to
support `println`). For a cut-down binary, use `mm -minsys hello` to leave out most of the
library (a basic `println` is still supported), then the binary is 2.5KB.

### Whole program compiler
The language is designed for whole-program compilation. A program is a single
EXE or DLL binary, or a single ASM or OBJ file when that output is chosen.

Other languages allow independent compilation of modules, here the granularity is the whole program instead.


### Standard Library
The standard library is collection of 5-6 modules (see below), which is usually
compiled into the application. The alternative was for it to be a standalone DLL,
an extra dependency when producing applications.

It adds about 25K to any program, but most programs need at least some of it.

### Compiler Inputs
There is only one input file on the command line:
````
    .m  file    Lead module of an application
    .ma file    Amalgamated file containing entire program sources
````
Other inputs needed to build a program are dealt with during compilation:
````
    .m files        Remaining modules are discovered via the module system
    include files   Via include directives
    sinclude files  Embedded text or data are via sinclude/binclude features
    binclude files
    DLL files       These are either infered from 'importdll` blocks or
                    listed with 'linkdll' directived in the project info
````
DLL files are only needed for producing an EXE, and are used to discover where any imported
symbol is located, info required for import tables inside the EXE.

M doesn't require an import to be precisely matched with the exact DLL file, so all submitted DLLs are searched.

(I can change this and make it stricter, as it has to be in the Q language, then actual
DLLs are not needed when compiling, allowing cross-compiling or remote building. But
for now it's fine and makes writing code simpler.)

Some DLLs are automatically included and don't need specifying:
````
    msvcrt.dll
    user32.dll
    kernel32.dll
    gdi32.dll
````
### Compiler Outputs
````
    Output  Option  Ext     Description

    EXE     -exe    .exe    (Default) Executable binary in Windows PE format
    DLL     -dll    .dll    Shared dynamic library, relocatable code
    OBJ     -obj    .obj    Object file in COFF format (via ```
    ASM     -asm    .asm    Produce a single ASM file representing the whole program
    MA      -ma     .ma     Produce a single-file, compilable source amalgamation
    RUN     -run    --      Compiler and run immediately; no file is written

    ML      -ml     .ml     Generate ASM first, then run `aa` with that option
    MX      -mx     .mx     These are a private binary format
    PCL     -pcl    .pcl    (Default) Needs `mmp.exe` M compiler
    C       -c      .pcl    Needs `mc.exe` M->C transpiler
````

### Compiler Binaries
````
    Binary      Dev Folder      Description

    mm.exe      c:\mx64         Main M compiler
    ms.exe      c:\mx64         Copy of mm.exe; `-run -q` options are applied

    mmp.exe     c:\px           Compile M to standalone .pcl IL file
    pci.exe     c:\px           Interpret .pcl file
    aa.exe      c:\ax           x64 assembler, produces EXE, DLL, OBJ, ML, MX files
    mc.exe      c:\mcx          Transpile M to single C source file

````
The main compiler is mm.exe, 0.3 to 0.4MB depending on configuration, which is
self-contained and independent when compiling M projects to EXE or DLL.

OBJ, ML, MX outputs require `aa.exe` (invoked automatically).

The `ms.exe` version (M-script) can used to run M programs just like a script.
The `-run -q` options otherwise needed are applied automatically.

Interpreting M code requires using `mmp.exe` and `pci.exe` instead.

Transpiling to C requires iuse of `mc.exe` instead

`mc` and `mmp` are auxiliary products which may not support the latest version, and may
dropped in the future.

### Targets
The primary target is x64 running Windows. Linux is possible via C only.
````
    x64         Use `mm.exe`
    C           Use `mc.exe` (limited)
    PCL         Use `mmp.exe` for experimental M interpreter
    Run         Use `mm.exe -run`, or `ms.exe`, to run an M program as a script
````

### Units, Sunits and Decls

A 'Unit' is either an expression or excecutable statement. Any unit can return a value,
but for some statements just has 'void' type so it can't be used.

A 'Decl' is any declaration/definition that introduces one or more new identifiers. It is a non-executable statement.

An 'Sunit' is a sequence of Units or Decls separated with semicolons or newlines (since newlines usually transform into semicolons).

Note that a construct like `(u; u; u)` doesn't create an Sunit, which must be
specifically mentioned in the syntax descriptions to be allowed.
It can mostly be used as one, but it can't contain declarations.

In this reference, `unit`, `sunit`, or just `u` and `s`, possibly numbered, may
appear in the descriptions.

### Data Types

Numeric types, which can have long and short designations plus common aliases:

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

    char8       c8      char        Character type is a thin wrapper around u8/byte/word8
    char64      c64
    bool8       -
    bool64      -       bool

    void        -                   Only allowed following 'ref'
    label       -                   Pointer target only
    ichar       -                   Alias for 'ref char'
````
Other types; here `T` represents any other type

````
    Name        Syntax          Notes

    ref         ref T           Pointer to T
    array       [bounds]T       Array of T
    slice       slice[]T        Slice of T; (a lower bound can be specified)
    record      record ...      Record of mixed-type fields
    proc        proc ...        Only allowed following 'ref' to define function pointers
                func ...
    label       label           Only allowed following 'ref' for label pointers
````
These are all 'placeholder' types, requiring extra syntax and more info to form a custom anonymous type, except for records which must be named types:

### Name IDs
These are the different kinds of identifier the compiler deals witj:
````
    Module
    Subprogram          The lead module name of a subprogram is also the subprogram name
    Function*
    Static Variable*    Either at module scope or static local
    Type/Record*
    Named Constant*
    Enumeration*
    Macro*
    Parameter
    Local Variable
    Macro Parameter
    Label
    Record Field        Variable part of a record instance
    DLL Module
    DLL Function
    DLL Variable        Not fully accessible from M code
````
Ones marked `*`, when defined at module scope, can have `global` or `export` attributes to make them visible to other modules or also to other subprograms.

Any DLL names are imported from external libraries, but are also exported from
the containing module, the one with the enclosing `importdll-end` block.


### Scopes and `global` and `export` Attributes

Scopes affect visibility of top-level names.

The following scopes exist, all lexical scopes:
````
    Program scope       The entire program
    Subprogram scope    Shared by all modules in a subprogram
    Module scope        Only in that module
    Function scope      Only in that function
    Record scope        Only in that record
    Macro scope         Only in that macro
````

**Block Scopes** There are no local block scopes in the language. Each function
body only has a single scope for all identifiers.

**Global Entities** Any name with a `global` attribute (only allowed at module scope) is
visible to all modules in that subprogram.

**Exported Entities** Any name with an `export` attribute is also
visible to other subprograms, or exported from the program when compiled as a DLL library.

### Module Scheme Directives
See [Modules24](Modules24.md) which describes the scheme in detail. In short, all project info is provided by a list of directives at the start of the lead module. Usually, they will be the only thing in the lead module. The directives are:
````
project =
    module name             # module name forming part of the main subprogram
    import name             # read the lead module and other modules of another subprogram
    linkdll name            # Dynamic shared library if it is not clear from importdll statements
    linklib name            # 'ML' library (this may be dropped)
    $sourcepath "path"      # temporary directive until path control is sorted out
end
````
The `project-end` block is a recent addition. This may be extended to allow an executable name that overrides that of the lead module.

### N-Based
M is primarily 1-based, but N-based including 0-based if usually allowed. A few things are 0-based only:
````
                    Default     Override?

    Arrays          1-based     Yes
    For-loops       1-based     Yes         Default used in 'for i to B do...'
    Bit-indexing    0-based     No          A.[i] and A.[i..j]
    N-way select    1-based     No          As used in (n | a, b c | z)
    Enumerations    1-based     Yes
````
In practice, a lower bound  other than 1 or 0 is rare.

### Command line params
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

### Entry point
The main subprogram should be a `main` function located in any module. (Typically, the
lead module contains only a project header, and `main` is in the following module.)

The `main` name is special; it will automatically be exported. To avoid ambiguity, at most
one `main` function is allowed in the subprogram.

It should be a `proc` that takes no parameters and returns no result.

Other subprograms can have their own `main`; this is ignored. (This can be used whe
then subprogram is compiled independently.)

### Program Exit
This can be done with any of these:
````
    stop                    Terminate with return code 0
    stop N                  Use return code N
    `exit(N)                Use C function (note exit is an M reserved word)
    exitprocess(N)          Use WinAPI function
````

The `main` entry point function will have `stop 0` injected at the end, so just running into the end of `main` will terminate too.

### Start Functions and Module Order
Each module can have an optional `start` procedure, which like `main` is exported
and takes no parameters, but it is given global scope.

If present, it is automatically called from start-up code when the program is launched.

The caller order depends on the `module` and `import` directives in the project header
according to these rules:

* Within a subprogram, all the modules are processed in top-down order as specified,
  except that the module with `main` is done last. If there is no `main()`, then
  it is assumed would have gone into the top module, so that is done last.

* With multiple subprograms: all the subprograms are processed in the top-down order
  specified, except the main subprogram (the one using `module` not `import`) is
  done last.

The automatically added `msyslib` module is notionally at the top of the list, so it is
processed first. That is, all its `start` routines are called according to the above order.

Subprograms should not have cyclical or mutual dependencies.

### Out-of-Order Definitions

All definitions in M programs can be written in any order. This applies to everything, not just functions. So it is possible to define all the locals of a function at the end, rather then the start.

The order of modules in the project info can be important, but these are directives rather than definitions.

### Attributes
Used in front of any definition of variables, functions and so on.

````
    global      Make visible to all modules in this subprogram
    export      Make visible to other subprogram. For the top
                program build into a DLL, export from the library
    static      For variables inside a function, use static storage
````

### Reserved Words
These include: keywords, type names and aliases, named operators and properties.

There are quite a few because of multiple type aliases (`word8 u8 byte`) and lots of
built-in operators that other languages provide via a library.

````
    abs          acos         and          asin         asm          
    assem        atan         atan2        binclude     bitwidth     
    bool         bool64       bool8        bounds       by           
    byte         bytes        c64          c8           case         
    cast         ceil         char         char64       char8        
    clamp        clear        const        cos          divrem       
    do           docase       doswitch     doswitchu    downto       
    else         elsecase     elseswitch   elsif        end          
    enumdata     esac         eval         even         exit         
    exp          export       false        fi           floor        
    fmod         for          fprint       fprintln     fract        
    fun          func         function     global       goto         
    i16          i32          i64          i8           iand         
    ichar        if           import       importdll    in           
    include      infinity     inot         inrev        int          
    int16        int32        int64        int8         ior          
    istrue       ixor         label        len          let          
    linkdll      log          log10        lsb          lsbit        
    lsw          lwb          macro        max          min          
    module       msb          msbit        msw          nextloop     
    nil          not          notin        od           odd          
    or           pi           print        println      proc         
    r32          r64          range        read         readln       
    real         real32       real64       recase       record       
    redoloop     ref          rem          repeat       return       
    round        sign         sin          sinclude     slice        
    sliceptr     sprint       sqr          sqrt         static       
    stop         struct       swap         switch       tabledata    
    tan          then         threadedproc to           true         
    type         typestr      u16          u32          u64          
    u8           union        unless       until        upb          
    var          void         when         while        word         
    word16       word32       word64       word8        xor          
    project
````

Not included:
````
    - Words that can still be used as identifiers, such as `million`
    - Words that start with `$`, since those are reserved by the language
    - The reserved words (opcodes, registers and directives) of the inline assembler
````

Any of the above can be used as an identifier, but need to be written with a backtick:
````
    int `int, `and, `goto
    `goto := `and 
````

(A few keywords need to be shared between the main language, and the inline assembler,
such as `and` and `not`; this is sorted out internally.)

### Semicolons and Newlines

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

### Comments

Only line comments are supported. They start with `!` and continue to end-of-line:
````
    ! A comment
    int a,b,c      ! Another comment
````
Block comments for multiple lines are expected to use editor support.

### Case Insensitivity
The syntax is case insensitive, so that `abc ABC Abc` are all the same identifier.

For defining case-sensitive function names in the FFI, write them inside a string
constant. Subsequently, and style of case can be used.

Alternatively, a back-tick can be used to preserve case:
````
    int abc, `Abc, `ABC
    abc := `Abc + `ABC
````
But the back-tick needs to used in every instance of the name. Back-tick also
allows the use of reserved words as identifiers.


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

### String Constants

These are written in double quotes "....". They cannot span multiple lines. Escape codes for embedded special characters are listed below.

Adjacent strings, either constants or named constants, can be combined with `+`; see Concatenating Strings below.

### String Escape Codes
These can be used also for character constants.

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
Raw strings: these ignore escape codes so that `"\"` is a normal character:
````
    F"C:\demo\mm.exe"           # F for file name 
    r"C:\demo\mm.exe"           # R for raw string; any case can be used
````

### String and Data Objects

There are String (zero-terminated text) and Binary (raw-data) objects which
can be specified via string constants or loaded from files:
````
    s"ABC"            3 bytes plus zero terminator, 4 bytes in all
    b"ABC"            3 bytes only, no terminator

    sinclude(file)    Should be a text file (no embedded zeros), zero terminator added
    binclude(file)    Any text or binary file, no zero terminator added
````

String data can be used wherever a normal string is expected (its type is ref char).
Binary data has type `ref byte` but is usually malleable to whatever data it initialises.

Both String and Binary data can be used to initialise arrays:
````
    []char S = s"ABC"          4 bytes total
    []char T = b"ABC"          3 bytes total
    []char U = sinclude(file)  Length is size of file + terminator

    []byte V = b"XYZ"
    []byte W = binclude(file)  Length is the length of the file

    []real x = binclude(file)  Length is the byte-length of the file / 8
````
For anything other than byte-arrays, binary files should contain suitable data
in correct endian format

### Initialising Char Arrays

The type system demands that these are initialised like this:
````
    []char S = ('A', 'B', 'C', 0)
````
which is inconvenient. But here either string or binary data objects can
be used; see above. One is zero-terminated, the other not.

### Unicode Support
````
    Identifiers         A-Z and a-z only plus the usual other characters
    Comments            UTF8 supported
    String literals     UTF8 contents supported
````
UTF8 content will need editor support to be add it directly. Otherwise
Unicode and UTF8 sequences can be specified directly using string escape codes - see above.

Apart from allowing UTF8 sequences in string data, the language provides no support.
This is up to applications to use the right libraries, the right OS settings to make it
possible.

Strings are considered to be only sequences of 8-bit bytes or 8-bit char arrays.

#### Pointer Constants

There is only one, called `nil`, with type `ref void`. It is always all-bits zero on supported targets.

### File inclusion and embedding
````
    include "filespec"      Incorporate in-line source code from another file "file.m"
    sinclude("filespec")    Brackets are optional. Embed any text file as a string data object
    binclude("filespec")    Embed any file as a binary data object, that can initialise any array.
````
`sinclude` adds a zero terminator which is counted as part of the data.

While `sinclude` is mainly for text, and `binclude` for binary, either can have
embedded zeros, or either can be all text. 

`sinclude` produces a string data object that can be used wherever a string literal
is allowed.

### Contenating Strings and Data Objects

Strings are not first-class types, but `+` can be used at compile-time to concatenate
string constants:
````
    "abc" + "def"                    abcdef
    "abc" + s"def"                   abcdef
    "<"+ sinclude("file1")+">"       <...>
    binclude("f1") + binclude("f2")  combine two files into one data block
````
It's not possible to mix string/s-data with binary data.

Combining strings is useful when:

* Long strings are split across multiple lines
* Named string constants are used, using `const` or `macro`. Example:
````
    const euro  = "\u20AC"
    const price = "4.99"
    ichar mess = euro + price
````

### Array bounds
The general pattern is:
````
    []              Unbounded with lower bound 1; either size is from init data, or array must be a pointer targer, otherwise length is zero
    [N]             N elements indexed 1..10 inclusive
    [A..B]          B-A+1 elements indexed A..B inclusive (eg. [0..9]int has 10 elements)
    [A:N]           N elements indexed A..A+N-1 inclusive (eg. [0:10]int also has 10 elements indexed 0..9) 
    [A:]            Unbounded but with with lower bound A
    [B.bounds]      Use B's bounds
````
### Array and Record Constructors

By these I mean constructing an array by enumerating the values:
````
    [3]int A    = (10,20,30)        # Exactly 3 elements must be provided if the bounds are 3
    []int B     = (10,20,30,40)     # Any number can be provided; this defines B's bounds as 1..4
    []int C     = ()                # Empty array ([0] also allowed)
    [1]int D    = (10,)             # One-element needs a trailing comma
    []int E     = (10,)             # Length is 1
````
All the above use `=`, so all must use static storage: either at file-scope, or using `static' if inside a function.
And all the values must be compile-time expressions or static addresses of variables and functions.

Initialising non-static arrays needs `:=`, but the values can be runtime expressions (check this still works).

I thought that array constructors in arbitrary contexts were not fully supported (as RHS of an assignment, or to pass to a function), however they seem to work if all elements are compile-time expressions.

Unintialised local arrays will have undefined values.

For N-dimensional arrays, all dimensions needed to be specified except for the first:
````
    [,2]int J := ((10,20), (30,40), (50,60))        # Final dimensions are [3,2]
````
The layout of the parentheses must exactly match the the type.

### Clearing

Arrays and records can't be cleared by initialising to a constructor with a single zero byte as in C. Any init data must match the type exactly.
The language provides an explicit `clear` instruction:

````
    [100]int A
    clear A
````
I used to have the special symbol `empty` with could be used to initialise or assign to the
array of record, but I found I only used `clear` so it's been dropped.

`clear` can also be used like this:
````
    ref T P := ... allocate memory ...
    clear P^
````
`clear` only works on arrays and records (otherwise it's too easy to
write `clear P` instead of `clear P^`.

It also can only be used for fixed-size ojects. For dynamic objects, use M's `pcm_clear` function or C's `memset()` routine.

### Variable declarations
````
    int a, b:=23, c
````
These other forms are rarely used:
````
    var int a, b, c             # var is made optional
    let int a := 100            # assign-once variable. Very poorly implemented, only works for
                                # simple scalar types
````
 The `var` form allows the type to be omitted and so infered, but that never got done.

I didn't take to `let` because it required initialising on declaration, which often means declaring in the middle of code.

The while Read-Only thing is poorly handled in my languages, which are keen on having mutable everything. But I am thinking of at leasting moving data like string constants to read-only segments (I don't have one in my back-end, so it may need to go into the code segment).

### Array and Slice Indexing
A slice is a 16-byte descriptor consisting of a pointer and a length. One can be declared like this:
````
    slice[]int S       # Lower bound 1; length and data undefined
    slice[M:]int T     # Lower bound M; length and data undefined
````
Slices are indexed from 1; this can be changed using `slice[0:]int` for example, but that's uncommon. When creating a slice *value* from an array, for example `A[3..6]`, the slice will have a lower bound of 1 with no override, and the length will be 4.

Slices can be initialised, or slice values created, in various ways:
````
    S := A[3..6]          A is a normal array
    S := (P, N)           Construct directly from a pointer and length (this one is under review as I want
                          to be able to allow: S := (10,20,30,40), initialise slice with a constructor)
    S := A                Slice to entire array
    S := S[2..4]          Slice to a slice
````
Slices only work for 1D arrays, or the first dimension of multi-dim arrays. A slice lower bound is set at compile-time.

As implement, an arbitrary slice of an array, like `A[i..j]`, will yield a slice index from 1. This can only be changed with a cast, or copying/passing to something expecting a different slice lower bound.

### N-dimensional arrays

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


### Slices
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


### Pointers
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

#### Record Layout Control
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


### Enumerations and Parallel Data
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

A set of enumerations do not form a new data type.

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

#### Multiple Return Values
````
    func F(int a, b)int, int =
        return (a+b, a-b)
    end
````
These need a multiple return type, a simple list of types. An explicit `return` keyword is needed, the return values must be in parentheses.
Limitations:
````
    - Only up to three return values
    - Only scalar types such as ints, reals and pointers can be returned
````
Consuming all, some or none of the return values is covered under Multiple Assignment, but is typically like this:
````
    (x, y) := F(10, 20)            # for my example, x is 30, and y is -10
````

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

The LHS has a set number of simple forms:
````
    name :=                     Simple variable
    term^ :=                    Pointer
    term.m :=                   Record field
    term[expr] :=               Array element
    term.[expr] :=              Bit or bitfield of integer (depends whether expr is a range)
    (term, term, ...) :=        Multiple assignment
````
It can also be a more complex term that yields any of the of above, for example:
````
    if a then b else p^ fi :=   2-way select, assign to b or p^
````
But these are rarely used, and have fallen into disuse. They need to be checked out.

#### Multiple Assignments

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
    (d, r) := a divrem b    # does divide and remainder in one operation; LHS needs to be 2 values
````

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


### Binary Operators

Not all type combinations are supported in the code geneator, especially for augmented assignments. These will be supported in
due course. But usually there is an alternative way to write the expression.

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
x = y       Any type, including arrays and records, yields bool
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
all face the same way, otherwise the meaning is obscure.


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

All statements can return values, except that loops only have a `void` return type, so it can't be used anywhere.

##### `If` Statement

    if s then s end
    if s then s else s end
    if s then s elsif s then s else s end       # etc

#### 2-Way Select

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
There is no `elsunless` as I couldn't figure out what it meant.

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
All the values in when-lists should not span a range of more than 1000 from lowest to highest. If exceeded, `case` should be used. (See Hard-Coded Limits.)

Any `u` following `when` can be a range, eg. `when '0'..'9'`.

#### `Case` Statement

This has mostly the same syntax as `Switch`:
````
    case x              # Can be any time for which = and <> can be used
    when a, b then      # Can be runtime expressions
        s
    when c
        s
    else                # optional
        s
    end                 # end case or esac can be used
````
Differences from `switch`:

* Tests are done sequentially, one at a time, instead of all in parallel
* Duplicate `when` expressions allowed (or rather, they are not detected)
* No ranges are allowed (I will look at adding these, but they will need implementing as `x in a..b` rather than testing every element in the range)
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

(THIS IS PARSED BUT HAS NO CODEGEN SUPPORT. I may instead looking at this other possibility:
````
    if
    elsif a or b then
    elsif c then
    etc
````
So `if elsif` is an indicator that is should be treated as `if`. This may be much simpler to implement.)


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

This version has never been implemented:
````
    for i inrev a do           # Iterate from last to first element
````


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
Control flow statements are:
````
    goto
    return
    exit
    redoloop
    nextloop
    stop
    recase
````
All would return a `void` value, but that couldn't be used anyway since it's not going to hang about to do anything with it.

All can have a conditional suffix:
````
    goto L when a=b
    return unless x=0
````
(`when` is used instead of `if`, since `if` could also start any expression that follows the keyword so would be ambiguous.)

In this case, the `goto` or `return` keywords can't be optional.

#### `goto`
````
    goto lab            # normal goto; lab must be somewhere in this function only
    lab                 # the goto can be omitted
    goto u              # the label can also be any expression yielding a label pointer
````
Label pointers can be used like this:
````
    static []ref label table = (L1, L2, L3)

    goto table[i]
    ...
L1:
L2:
L3:
````
(THERE'S A PROBLEM INITIALISING THAT TABLE: 'not const')

#### `return`
For procs without a return value, `return` is optional at the end, and is only needed for an early return.

For functions with a single return value, again it only needed for an early return. It can be omitted when the return value is
the last value of function body:
````
func F(int a)int =
    if a then
        10
    else
        20
    fi
end
````
Functions with multiple return values always need `return`,  since the special tuple construct needed is not recognised by the type system;
it is internal.

#### `stop`

See Program Exit above. `stop` terminates the program with an optional exit code (default is 0).

#### Loop Controls
````
    exit     [index]         # terminate the loop early
    redoloop [index]         # redo this iteration
    nextloop [index]         # proceed to next iteration
````
These can have an optional index to deal with nested loops:
````
    1       Current or innermost loop
    2       Next outer loop
    0       Outermost loop
    all     Also outermost loop
````

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
    fprint "##", "#", 123   Display '#123' (how to display a literal '#')

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
The whole line must either be consumed or abandoned before switching.

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

`A.bounds` yields a range `A.lwb .. A.upb`, and can be used wherever a range is allowed.

(When A is a type, then only a user-defined type will yield the bounds, so it must
be an array. For a built-in integer type, A.type yields the numeric range.)

Ranges are not first-class objects in M (they are in Q).

A Set is used only in conditions like this:
````
    if x in [a, b, c] then
````
Equivalent to: `x=a or x=b or x=c`, except that `x` is evaluated once. (In Q, sets are first-class objects, bitsets than can contains ints and ranges.)



### Bits and Bitfields

The language favors special operators for bit access rather than special types:
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
A bitfield can only be defined inside a regular field. In my example, 6 bits are used out of 8 (they are allocated
from bit 0 up):
````
    demo x
    x.flags:=0      Access all 8 bits once
    x.b             Access bit b only
````
Bitfields can only store unsigned values, but are promoted to i64 when extracted. (Unless extracting all 64 bits of a u64
field, but these points need checking.)

Initialising or assigning to a record only works with regular fields, although binary can be used, eg. 1101_0_1B to initialise my `flags` field.

I haven't find a way to use named bitfields like this, to access the bitfields in an integer. The nearest is this:
````
    macro offset = 0..23
    int a
    a.[offset]              The square brackets are needed
````
#### Some Built-in Bit/Bitfields
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

But then the actual DLL must be one of those specified with `linkdll` in the lead module. It will search all DLLs until it finds each imported
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


### Standard Library Modules
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

Macros with no parameters:
````
    macro M = F         # F is a function taking a parameter
    M                   # Same as F, address of F
    &M                  # Same as &F
    M(10)               # Same as F(10)

    macro M = F(20)
    M                   # Same as M(20)
    &M                  # Should be same as &F(20), but not working
    M(30)               # Should be F(20)(30), an error but not reporting; the (30) is ignored
````
With parameters:
````
    macro M(a) = F
    M                   # Error: missing macro arg
    M(20)               # F (macro param not used)
    M(20)(30)           # F(30) (param not used)

    macro M(a) = F(a)
    M                   # Missing macro arg
    M(30)               # F(30)
    M(30,40)            # Too many macro args
    M(30)(40)           # Same error as F(30)(40) (F does not return a function pointer)
````

Macros can get very very hairy, and I don't want to try and fix them. They mostly work fine and they are used sparingly anyway.


### Naked Functions
````
`threadedproc`
````
Threaded functions can't have parameters or local stack-frame variables (statics are OK). There is no entry nor exit code generated.
They can be called, but there is no return. Normally you jump to a threaded function, and jump from that to the next, using ASM code.


### Built-in constants and properties

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



### Misc features

#### Clamp

`clamp(x, a, b)` is a built-in 3-operand op equivalent to `min(max(x, a), b)`.

#### Last Array Element

`A[$]` is equivalent to `A[A.upb]`, so accesses the last element of `A`. `A[$-1]` will be next-to-last.

#### Equivalence
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

#### Operator Constants

Writing `(+)`, or any other operator inside brackets, returns an internal ordinal (an integer) representing that operator.

This is no big deal, you could just use `'+'` for example, but it is a nice touch. And in Q, the value is recognised by the language:
`mapss((+), 3, 4)` yields `7`.

#### Type Constants

Similarly, writing `(int)` (or `int.type` for simple types), yields an internal ordinal for the type. It can be used like this:
````
    if T = int.type then
````
where `T` has such an ordinal value.

#### Type Reflection

There is some use for this even in a static language:
````
    X.type       Returns the type ordinal as used above
    X.typestr    Returns a string such as `"i64"` and so on
````
`X` can be type or expression. Parentheses may be needed around `X`, when it is a number for example.

#### Field offsets.

If `T` is a record type, then `T.m` yields the offset of field `m` within the records.

This can be used in inline assembly:
````
	record date = (var day, month, year)

	asm mov rax, [rbx + date.month]
````
Or used to set up constants: `const month = data.month`.

### Compiler Options: Outputs

Assume the lead module is called prog.m:

````
    Option      Output          Notes

    -exe        prog.exe        Default option
    -dll        prog.dll        Also prog.q or prog_lib.m depends on config
    -obj        prog.obj        Generates prog.asm then vnvokes aa.exe assembler
    -asm        prog.asm
    -ma         prog.ma         Single, compilable amalgamated source file representing whole program
    -run        <Run prog>      Generates in-memory code and runs the program
    -list       prog.list       To help my IDE
    -proj       prog.prog


### Compiler Options: Misc
```
    -peep                   Apply small peephole optimiser
    -regs                   Keep first few locals in registers
    -opt                    Do both the above

    -rip                    Use RIP mode for low-mem EXE outputs
    -himem                  Use RIP+HIMEM for DLL/OBJ outputs (default for -dll/-obj)

    -h -help                Help summary
    -q                      No message; default is `Compiling ... to ...`
    -v                      More verbose: 'Finished'; AA invocations; size info
    -vv                     Extra verbose: show files/paths used for inputs

    -time                   Give timing stats
    -size                   Give size stats

    -out:file               Change output filename (extension will use default if omitted). Same path as
    -outpath:path           Write output to different path, using same filename
    -unused                 Display list of all unused variables

    -sys                    (Default) Use normal msyslib.m
    -minsys                 Use smaller msystemp.m to allow benchmarks to use 'print' etc
    -nosys                  Use no syslib

````
The rest are mostly for development:
````
    -load                   Load modules only
    -parse                  Parse only (to AST1)
    -fixup                  Fixup user types
    -name                   Name resolve (to AST2)
    -type                   Type analysis (to AST3)
    -pcl                    To IL (to PCL)
    -mcl                    To native code (to MCL)

    -modules                Display modules
    -st                     Display ST tree
    -stflat                 Display flat ST (hashtable with dupl entries)
    -showpcl                Display PCL
    -showasm                Display MCL
    -ast1                   Display AST1
    -ast2                   Display AST2
    -ast3                   Display AST3
    -showss                 Display GENSS output
    -types                  Display type table
    -shortnames             Use shorter, not-so-qualified names in ASM display

    -ext                    Use discrete std lib files in development directory, not built-in

    -getst                  Write project.list file for IDE
    -getproj                Write project.proj file for IDE
````


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


### Run from source
Using the `-run` compiler option will run programs from source just like a scriting language.

To avoid using `-run`, it is possible to copy the compiler `mm.exe` to `ms.exe`; it will detect this name, and invoke `-run -q` automatically:
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
These are the options for `mc.exe`:
````
    -c          prog.c          (mc.exe)    Transpile to single C source file
    -gcc        prog.exe        (mc.exe)    Generate prog.c then invoke gcc
    -opt        prog.exe        (mc.exe)    Generate prog.c then invoke gcc -O3
    -tcc        prog.exe        (mc.exe)    Generate prog.c then invoke tcc
    -mcc        prog.exe        (mc.exe)    Generate prog.c then invoke mcc
````


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

That was intended to provide function return values. But the allows an asm block anywhere, so
it needs to work.

Assembly syntax is pretty much that of my AA assembly. I will not be documenting that.


### High-loading and Position-indepedent code

Normally `mm` generates x64 code that only runs in low-memory below 2GB.

It is not position independent. But when generating DLL or OBJ files via `-dll`
or `-obj`, the `-himem` option is turned on which generates relocatable code,
and also lets in run above 2GB.

This is necessary for OBJ files since it will need an external linker which is
likely to load the program at a high address.


### Self-hosting

The M compiler is written in its own language, and each new version has been implemented
with an older one in a chain going back to the 1980s.

The first version for x86 I think was written in assembly, and I probably wrote
the assembler. In any case, no other HLLs have ever been involved.

### Using Linux
This is only viable via the M to C transpiler as there is no native target of of M
that works under Linux, either x64 using SYS V, or ARM64.

But I want to drop that product at some point; it is not satisfactory. Currently it works like this:
````
    On Windows:

    mc -linux -c mc -out:mu       # Create a file mu.c using module for OS-specific stuff

    On Linux:
    gcc mu.c -omu -lm -ldl        # Create compiler binary

    ./mu hello                    # Compile hello.m to hello using intermediate C and gcc
    ./hello                       # Run it
    ./mu -opt hello               # Use gcc-O3 to compile the output
    ./mu -tcc hello               # Use Tiny C instead (a far better match in compile-speed)
````

### Interfacing to Q
This is a WIP.

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

### Building Programs and Project Files
There is no separate build process necessary to turn an M application into a single `.exe` or `.dll` file, you just do this on the `prog.m` lead module:
````
    mm prog                    # -exe is default option
    mm prog -dll
````
However, the project will still usually consist of dozens of source and support files.
There will be some some process the developer uses to browser, edit, build and do test runs.

I believe that lies outside the language and even compiler. In my case I use a tiny 36KB
IDE which works from a project file. That file contains some of the same info as the project
header in the lead module.

I haven't yet joined those together. Right now the M compiler has options `-prog` and `-list`
to produce some info that my IDE can pick up. Integrating them better is a WIP.

### External Libraries
These are nearly always accessed via DLLs: shared, dynamically loaded libraies.

Static linking with code from other languages is usually not done, but it is possible; see below.

### Working with other languages and tools
M programs are designed to be self-sufficient. External libraries can be used from DLL
shared libraries. And a DLL created from an M program could be called from another language.
But the M-program or library will be 100% M code.

However there are ways to create programs that statically combine mix M and non-M code
within the same binary. That involves generating OBJ files, and means using external
tools to process those files and combine them with ones from other languages.


### Hard-coded limits
Some constants I've seen in the source code. Most will be checked.

````
    Number of user types
    Max params in a function 100/32        (Two different limits; to be fixed)
    Max locals in a function    256        (In code generator)
    Number of modules           200
    Number of subprograms        30
    Number of source files
    Number of DLL imports     50/50        (Set in to places; fix)
    Number of DLL procs        1000
    Switch-when cases           500
    Switch-when spread         1000
    Loop nesting                 50
    Case nesting    
    Include file nesting         20
    Max params in a macro        50
    Nested $ as in A[$]          10         (Nested Array indexing all using $)
    Nested for loops             10         (To be able to detect using the same index var)
    Max array dims               30         Eg.A[n1,n2,...n30]int A
    Enum/Tabledata max cols      20
    Enum/Tabledata max rows     500
    Max fields                  200         (In TX pass)
    Max func return values      3/4         (Check)
    Nested call depth            16         (In code generator)
    Max symbols for COFF?       32K         (Check)
    Max imported symbols       3000         (Generating EXE)
    Max exported symbols  1000/2000         (Generating DLL; check)
````
### Error Reporting

This is not great. When a poor error report, or even lack of detection, has caused grief, then I should really fix it.
But too often I don't do that; once I've found the cause, I just carry on.


### Compilation Speed
On my low-end PC, the mm compiler generally has a throughput of 0.5M lines per second.

Adding an IL stage slowed it down a little, and invoking its optimiser slows it another 10% or so.

Some code patterns or certain stress tests are problematical. But generally the
speed is currently healthy. A self-build can take as little as 80 msec.


### Grammar

There isn't a formal or even informal grammar. The syntax of an M program is
roughly:
````
program    = subprogram+                   One or more subprograms

subprogram = leadmodule module*            Zero or more modules

leadmodule = [header] [sunit]

module     = sunit

header     = PROJECT = projectdir* END     Zero or more project directives

sunit      = unit/decl |;                  Zero or more unit or decl separated by ;

decl       = <declaration or definition>

unit       = <expression or executable statement>

````

That describes the top level.

### Releases
This is not a priority as M is a personal tool, and I can't support it as general product anyway.

But even if I wanted to make it available, the `mm.exe` binary probably won't
make it past the AV software that Windows systems are usually bristling with.

These are the possibilities if I wanted to make my compiler and language available to anyone else:

* Transpile it to single C source file using the `mc.exe` tool.

This is workable, but I had been hoping to get rid of that tool. The conversion is not perfect, it
will not handle all features of it. I have to avoid using certain features in the compiler so that transpilation
remains viable.

However, this option also adds the ability to optimise the compiler, making it faster. And it allows a path
to get M working on Linux, by applying `mc` to itself (see Using Linux)

* I can suppy the binary in my private MX format as `mm.mx`, which should appear as data
to AV software. To run it however requires a program called `runmx.exe`. That one
already exists as a native C file, `runmx.c`.

So `runmx.c` is compiled locally to `runmx.exe` (this is for Windows only). The compiler is then invoked like this:
````
    runmx mm hello           # invoke mm.mx and compile hello.m
````
This is can trivially scripted to simplify it, but it's not as convenient as mm.exe.

* M produces ASM in my own syntax. If I could generate NASM format instead, then I can supply `mm.asm` as a
single file, and NASM is run locally to produce `mm.exe`. Someone familiar with NASM should already know how to get
generated EXE files past the local AV.
