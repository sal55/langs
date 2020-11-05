
## **M** Systems Language: Summary of Features

M is a systems language first devised for 8-bit Z80 systems in early 1980s, since evolved and now targeting 64-bit Windows machines using x64 processor.

It is also now known as 'Mosaic' (named by a Reddit user).

The following is not a formal reference but is a random collection of topics.

### The M Compiler

This is written in M. It builds to a single executable file that I usually call mm.exe, currently some 0.6MB, which includes a small set of libraries. (That is, the sources for the libraries are part of the executable.) This gives a tidy self-contained compiler.

### Dependencies

There are none to build M sources to a binary executables, other than:

* A Windows OS
* msvcrt.dll (part of Windows OS)

### Whole Program Compiler and Output Files

A 'program' in this sense is the collection of modules and files that form a single .exe or .dll file. M will always compile all modules from scratch. (Effectively, the 'compilation unit', or granularity, moves from one module, to one program.)

Output files will always be a single file that will be one of:

**.exe**    (Default output)
**.dll** -dll option (also generates an .exp file, an interface file that can be directly imported within a M module)
**.asm** -asm or -c option (in the syntax used by my own **ax** assembler/linker)
**.ma** -ma option (combines all source and support files into a single .ma text file, which mm can directly compile)

(There is no direct .obj file output. This can be done by generating an .asm file then using 'ax -obj'. OBJ files will an external linker to process further.)


### No Make or Project Files

M builds a project by submitting only the lead module to the compiler. It will then locate all necessary modules (by following import links), to produce an executable named after the lead module.

### Linking with external libraries

M by default generates executables that are dynamically linked to DLLs. To statically link to such libraries, requires:
* Getting mm to generate .asm (direct .obj output not supported)
* Using ax (companion assembler/linker) to produce .obj files
* Linking via the C compiler gcc with .o or .a files

This requires the external library to exist in the form of gcc-compatible .o object files, or .a archive files. It further requires a .a library to contain the actual library functions, and not simply be an interface to a DLL file. 

### Compilation Speed
I've taken my eye off the ball recently, but it will still compile at hundreds of thousands of lines per second on my slowish PC with a conventional hard drive.

### Optimisation

A recent compiler update provides a modest optimiser.

Even unoptimised, the M compiler can build itself from scratch (some 40Kloc in 30+ modules) in about 0.2 seconds.

### C Target

Some version have supported a C target. The current compiler doesn't do so, and would not anywaty support all features in the language.

I might add it again, but it will be as an extra target of M's intermediate language. A C target allows M programs to run on Linux, or
to benefit from optimising C compilers.

### M Syntax 
Originally inspired by Algol-68, but has evolved its own style. Best described by looking at example programs.

### Modules and Imports
This is another big difference from C (and C++ is only now acquiring modules). M has no header files and has no need for declarations as well as definitions.

You define a function, variable, named constant, type, enum, macro etc in its own module. To export it, add a 'global' attribute in front, otherwise it is private to that module. 

To use those exported names from module A in another module B, that is when you write 'import A' inside B.

Names exported across modules are not also exported from the program. When made into a DLL, only names defined as 'export' rather than 'global' are exported from the DLL.

### Circular and Mutual Imports
Previous versions of the module system required modules to be in a strict top-down hierarchy. That was too restrictive. The current scheme allows imports in any order including circular imports: A can import B, and B can import A.

(There is a downside: the order in which default initialisation routines are called is no longer determinate.)

### Out of Order Definitions
Unlike C, functions can be defined in any order in a module. If a function F calls G(), and G is defined later on, you don't need a forward or prototype declaration for G.

Actually, this also applies to other named entities, so you could for example choose to define all the local variables at the end of a function! (Only import statements need to go at the start of a module, to avoid scanning the entire file for the preliminary pass to determine all the modules.)

### Block Scopes
Another big departure from C, is that inside a function, there is only a single scope; there are no block scopes. Further, there is a single name space (no crazy tag namespaces and even label namespaces of C)

Since functions are best kept small, there is no real need for multiple scopes and overloading the same identifiers.

### Semicolons
While M ostensibly requires semicolons to separate statements, in practice these are rare in M source code. This is because newlines are converted to semicolons, except when:

* A line-continuation character \ is used at the end of the line
* The line clearly continues onto the next because it ends with one of:
       "(" "\[" "," or a binary operator

### Comments
M only has single-line comments starting with "!" until end-of-line. ("!" came from the DEC Fortran and Algol I used in the late 70s.)

It has had block comments in the past, but I believe those should be an editor function (which can use multiple "!" comments). (This also makes things simpler for highlighting editors, as it doesn't need to keep track of context from 1000s of lines before.)

This document will use '#' for comments in examples as it is clearer and more familiar.

### Doc Strings
Doc strings are line comments starting with #, just before and/or inside a function.

A compiler option causes such documented functions to be written to a text file, with function signature shown plus the comments for each. (Feature not complete.)

### Character Set
I haven't yet ventured into Unicode. Source code is written in ASCII, but can also be UTF8. UTF8 sequences can be part of comments or strings.

Newlines in source files must use CR/LF or LF sequences.
      
### Case-Insensitive
Another departure from most languages, especially those associated with C and/or Unix. M is case-insensitive, which means that all of these names are identical to it:

    abc abC aBc aBC Abc AbC ABc ABC

Identifiers in M use: A-Z a-z 0-9 _ $ and can't start with a digit.

(To access case-sensitive names used with external libraries, there are two schemes. One is to declare the name as a string, but can then be used in any case. The other is to use the \` prefix, which also allows names that are reserved words:

    clang proc `exit(int)

Here, it is necessary because 'exit' is a loop control statement in M. This time, \`exit needs to be used everywhere.)

### Program Entry Point
This is usually the start() function, which is always global (ie. exported, no 'global' needed.) start() takes no parameters.

M will insert a call to a start-up routine in M's runtime module, to set up command-line parameters etc as global variables (nsysparams, and sysparams, the latter being an array of strings).

main() can also be used as an entry point, especially if generating .obj files to be linked via gcc, as gcc will not recognise 'start'.

### The $init function
If encountered in a module, it will be called automatically by start-up code. No 'global' attribute needed. However, because of non-determinate module import order, if such a routine depends on another $init function being called first, then this must be handled manually (with flags and direct invocation etc).

### Include, Strinclude and Bininclude
**include** is an ordinary textual include, and is only needed in M when you actually want to include code (not headers) from another file. (For example, a file generated from a program.)

**strinclude** can be used in an expression, and can include any text file as a string constant. (I use this to incorporate the sources for M libraries into the M compiler; or the C header sources into my C compiler.)

Example in a program called prog.m, this prints itself:

    proc start =
        println strinclude "prog.m"
    end

The source file prog.m does not need to be present at runtime; it becomes part of the executable.

**bininclude** is a variation used to initialise a byte-array, and can refer to any file including binaries. However it's done inefficiently at present.

### Conditional Compilation
M has no preprocessor and no conditional directives for code (there used to be, but I didn't like them).

Conditional code is handled at the module rather than line level, via module mapping.



### Function Tables
There is a limited amount of reflection in that all functions (names and addresses) in the program are written to the executable, and can be accessed via special functions.

This allows finding out the name of a function from a function pointer. But what I most use it for is building, at runtime, tables of functions pointers for special handlers. For example, handlers for the commands of, say, an editor, may have functions with names such as 'ed_left', 'ed_delcharright':

   proc ed_left(txdescr td) = ...
   proc ed_delcharright(txdescr td) = ...
   ...
 
By searching for functions that start with "ed_", and picking up the name of the command it deals with ('left' or 'delcharright'), then a list of handlers for the commands can be created, with defaults for missing commands. This makes it easy to maintain such handlers without updating tables of such functions.

### Data Types
M is low-level so has mainly simple, fixed-size types: scalars, records (ie. structs) and fixed-length arrays with a size known at compile-time.

Dynamic arrays with a length known at run-time can be created with pointers and allocations, but the size remains fixed.

A new type recently added are slices or views into arrays and strings, which can do more along those lines (see below) but I haven't done much with them yet.

### Numeric Types

The official set of types are these (either long or short forms can be used):

    int8     i8      Signed integers
    int16    i16
    int32    i32
    int64    i64
    int128   i128
    
    word8    u8      Unsigned integers
    word16   u16
    word32   u32
    word64   u64
    word128  u128
    
    real32   r32     Floating point
    real64   r64

    char     (c8)
    char64   (c64)

Common aliases:

    int    =  int64
    word   =  word64
    real   =  real64
    byte   =  word8

(There had been also machine types for 32/64 bit ints, words and pointers, but since I've settled on 64 bit targets with everything 64 bits, those has been dropped.)

### Type of Integer Constants
These are defaults before any casts are applied, depending on the magnitude of the constant:

    0      to 2**63-1         int64
    2**63  to 2**64-1         word64
    2**64  to 2**127-1        int128
    2**127 to 2**128-1        word128
    2**128 and above          decimal type (not implemented in this version)

(No suffixes are used to force a particular type, except that in the next M version -L is used to force a decimal type for integers and floats. To force a type, just use a cast, eg. word64(0).)

### Numeric Separators

Numeric constants including floating point can use ' _ or \` to separate groups of digits:

    1'000'000
    5`678
    0.142_857

These can all be mixed up, or duplicated: 124'878\_\_\_\_000, so should be used sensibly.

### Numeric Scaling

M allows scale factors such as 'million', used like this:

    4 million           # 4000000

There used to be others (like m, cm and km to scale lengths to the same units), but this part of the language will be reviewed to try and have user-definable scale factors.

Such names are in their own namespace; 'million' can still be used as an identifier.

### Number Bases

Number bases from 2 to 16 can be used for integers and floats, eg:

    2x100    # base 2; 4
    100B     # Alternative way to write binary
    3x121    # base 3; 16
    8x377    # octal: 255
    12xBBB   # base 12: 1727 (not sure if bases 10x to 15x work in this language)
    0xFFF    # hex: 4095
    16xFFF   # also hex 4095 for completeness
    5x3.4    # base 5: 3.8 (3 and 4/5)
    
### Numeric Limits and Type Sizes
Given any numeric type T or expression X, then:

    T.minvalue
    T.maxvalue
    X.minvalue
    X.maxvalue

will yield the smallest and largest values possible. (Next version, where a 'range' (see below) is an actual type, then I may introduce T.bounds to mean 'T.minvalue..T.maxvalue'.)

For the fixed size of a type or expression:

    T.bytes
    X.bytes

For array lengths and bounds, there is a whole set of properties that can be extracted; see section on Lengths and Bounds.

### Character Constants
M uses ASCII, so 'A' has the value 65, but it has type 'char'.

Multi-character constants are possible, with these types: 

    'A'                                    c8
    'AB' up to 'ABCDEFGH'                  u64
    'ABCDEFGHI' up to 'ABCDEFGHIJKLMNOP'   u128

### Unicode String and Char Constants
Not sure how to tackle Unicode support yet, so these constants are not supported. UTF8 sequences can be used, but to work with those, any routines called need to support UTF8. A 'print' on a UTF8 string ends up calling C's printf for normal (not wide) characters, so these also depends on Windows, locale, codepages and the like.

### Type Reflection
The expression X.type returns an internal type code, so that it can be used as 'if X.type = int.type' for example. Although types are static, the result type of an expression may not be obvious. It can also be turned into a string:

    println X.typestr

may display "i64", or whatever arbitrary type X happens to be.

### Type Syntax
This is Algol68-based, with types written left-to-right:

    ref int A            # point to int
    [10]int B            # Array 10 of int
    ref[]ref real C      # Point to array of pointer to int

Compared to C (with its convoluted inside-out type-syntax with the name of the thing being declared somewhere in the middle), it's really easy with none of its quirks. See sample source files.

### Type Aliases
These are written as, for example:

    type intptr = ref int
    type matrix = [4,4]int real

Like C, these do not introduce a new type.

### Pointers and Arrays
Unlike C, you can't index a pointer like an array, and you can't dereference an array like a pointer. Offsets to pointers can be done, but are written as '(P+i)^' rather than using P\[i\] like C, making it clear something underhand is going on.

### Auto Dereference
M used to be a more transparent language needing explicit derefs, but that has been relaxed. Now, a dereference op (a postfix '^' like Pascal) can be omitted in many cases:

    P^[i]   can be written as P[i]      (P is pointer to array)
    P^.m    can be written as P.m       (P is pointer to record)
    P^()    can be written as P()       (P is pointer to function)

Less transparent, but cleaner code. However sometimes ^ is still needed:

    print P^           otherwise it will print the pointer
    ++P^.m             otherwise it's parsed as (++P)^.m (working on this)  

### Array Bounds

    [5]int A                # Array of length 5
    []int A = (10,20,30)    # Unbounded array whose length is set by the data
    ref []int A             # Unbounded array which is a pointer target

So far these arrays have used the default lower bound of 1. Other bounds are possible:

    [0:]int A = (10,20,30)  # Bounds are 0..2, length is 3
    ref [12:]int A          # pointer to array indexed from 12, length unbounded
    [6..10]int A            # Bounds are 6 to 10 inclusive, length 5
    [6:10]int A             # Bound are 6 to 15 inclusive, length 10

The general pattern is:

    [length]
    [lower .. upper]
    [lower : length]
    [lower :]               # Unbounded array

For multi-dimensions, use:

    [10,20]int A
    [,20]int A              # First unbounded

(There is limited support for passing multi-dimensional arrays to functions; the dimensions must be constants, except the first can be unbounded.)

### Value Arrays
M arrays are always handled by value (C converts them always to a pointer, with a schism in the type system to ensure that). So:

    [10]int A,B
    A := B

However, there is little support for passing value arrays to functions or returning such arrays. (I used to allow that long ago, but it was never used.) Better ways to pass arrays are by pointer, by reference, or via a slice.

### Array Indexing
M allows multi-dimensional indexing using the more fluid A\[i,j,k\] instead of the A\[i\]\[j\]\[k\] used in many languages. Although A\[i\]\[j\[k\] is legal too.

### Records
These are structs, eg:

    record date =
         int day, month
         int year
    end

    date d,e,f

Offsets can be specified directly (eg. int x @ a for x to be at the same offset as a), but now 'union' and 'struct' are used as a better way of controlling layout:

    record R =
        union
           int32 a
           int32 b
           struct
              byte f,g,h,i
           end
        end
    end

Here, the struct occupies 4 consecutive bytes. There is no padding added to ensure alighnment; this must be done manually.

Struct and union behave like anonymous structs and unions in C.

Records in M must always be declared as named user types like the above; not anywhere, or anonymously, like in C. There is no concept of a struct tag.

### Matching Foreign C Structs

If a record is constructed to match a struct in a C interface, then field offsets must match exactly. However C can inject padding for alignment. This is tricky to duplicate, so a special attribute can be used:

    record rec = $caligned
        byte a
        int64 b
    end

Such a record now occupies 16 bytes instead of 9, and b starts at offset +8 instead of +1.

### Records as Classes
Records can contain other definitions such as types, named constants, and functions, which do not form part of any instance of the record. But I've done little with this, so I'm not sure exactly what is supported by M.

The concept is easy however: it can provide simple encapsulation.

### Pointers and Strings

Pointers work as they might do in C. In a type spec, 'ref' is used to means pointer-to:

     ref int P = nil         # Nil is the equivalent of C's NULL

Low-level strings are zero-terminated sequences like C, but here they're sequences of a char type which is distinct from a byte:

     ref char S
     ichar T                 # ichar is a synonym for 'ref char'

Such strings are generally passed as 'ref char' (or ichar) types, rather than ref[]char which allows normal indexing.

### Char Array Initialisation

    []char S = ('A','B','C')

This would be a lot of work to write like this. C allows such an array to be initialised from "ABC", but "ABC" has the wrong type, char\* rather than char\[\] (a C quirk allows it). In M, special string constants exist:

    []char S = a"ABC"             # 3-char array, non-terminated
    []char S = z"ABC"             # 4-char array, zero-terminated

### Raw String Constants

These are:

    F"C:\ABC\DEF.G"

where string escapes are ignored.

### Statements and Expressions are the Same
This is another concept from Algol68 - any expression can be used as a statement, and any statement can be used as an expression, and yield a value, although usually that would be 'void'.

(Name definitions are not classed as statements however.)

'Statements' that can usefully yield a value are: 'if' (long and short versions), 'switch' and 'case', although they will require an 'else' part.

Any sequence of statements can be easily turned into a single expression by writing as (s1; s2; s3), or sometimes a sequence can be written anyway without the brackets:

    if c:=nextch(); d:=c; c<>0 then                # uses the value of the last expression
    
### Functions and Procs
M likes to make a stronger distinction between functions that return a value, and those that don't. The latter are defined with 'proc'

    function add(int a,b)int =
        return a+b
    end

    proc addto(int &a, b) =
        a +:= b
    end

    proc abort(ichar message) =
         println "Aborting:",message
         stop 1
    end

Here the '&' signifies a reference parameter. Functions require a return type, and they need to return a value. Alternate syntax:

    function add(int a,b) => int = {a+b}

This demonstrates:

   * Parameter names with the same type can share the type spec (unlike C)
   * => can be optionally used denote a return type
   * {...} braces can be used around a function body, the only place they are used. ({,}  were part of a more general syntax for deferred code, such as lamdbas, but never got around to that. Function bodies are certainly deferred.)

   * 'Return' is actually optional; just the value will do

### Function Parameters
M has default and keyword parameters:

    proc createwindow(int dimx=640, dimy=480, posx=0, posy=0, border=1) ...

    createwindow(dimx:1920, dimy:1080)

Since functions from external libraries need to be recreated in M, it is also possible to add defaults, and use keyword arguments, to existing functions where that was not available in the original language:

    windows function "MessageBoxA" (
             ref void hwnd=nil,
             ichar message="Hello", caption="Default Caption",
             int flags=0)int
             
This can then be called as:

   messageboxa(message:"My Message")

### Reference Parameters
As shown above, '&' means a reference parameter, allowing a callee to modify data in the caller:

    proc setlength1(int length) = {length^ := 123}
    proc setlength2(int &length) = {length := 123}
    ....
    int length
    setlength1(&length)
    setlength2(length)

setlength1 uses explicit & and ^ operators, setlength2 does things implicitly. This also means it's not possible to pass a nil pointer.

### Nested Functons
Functions can be nested, but in a limited manner because a nested function can't access the stack-frame variables of its enclosing functions (that is difficult to implement). But it can still access static variable, named constants etc of those functions.

### Multiple Return Values
An experimental feature limited to scalar return types:

    function fn => int, int, int =
       return (10, 20, 30)
    end

    (a,b,c) := fn3()         # At present exact type match needed (will not convert to match)

Return values can be ignored:

    (a,b) := fn3()   # discard last
    a := fn3()       # discard last two
    fn3()            # discard all


### Define Variables
This is fairly standard; inside a function (anywhere in the function actually):

    int a, b:=123, c
    var int a, b:=123, c        # 'var' can optionally be a prefix

I liked the idea of requiring variable definitions to start with a keyword, but I also found it was a pain having to type this prefix, so it is now optional.

At the moment, non-static variables are not automatically initialised to anything, like C.

### Readonly Variables
I've never been a fan of C's 'const' attribute, which really complicates the type system. M never had anything like that until recently, when it was possible to use 'let' instead of 'var':

    let int A := 100         # (type should be optional, but that is not supported yet)

Here, the initialisation is mandatory, as A can't be used as an lvalue like in an assignment. This provides some weak protection, but won't do much for more complex variables, such as arrays or pointers to data structures. Let is experimental.


(Also experimental are 'in', 'out' and 'inout' attributes for function parameters. 'out' vaguely corresponds to '&' used for reference parameters. I haven't played with these attributes yet, and I'm not sure whether an 'in' parameter should be equivalent to 'let'.)

### Static Variables

All variables defined outside a function are static. They are either set to all zeros,
or can be initialised with an expression or construct that must be a compile-time constant:

    int A = 100         # "=" must be used, not the ":=" of assignment

Inside a function, a 'static' prefix is needed.

### Named Constants
This is a very simple feature, naming compile-time expressions:

    const A     = B+C
    const int B = 100
    const C     = 200

When no type is used, it is infered from the expression. (The example demonstrates out-of-order definitions.) Such constants can be exported using 'global const'.

Use of A, B or C in source code are synonyms for the constants 300, 100 and 200.

'const' is useful for numeric types, but less so for anything else.

### Enums
These are a little like C:

    enum (A, B, C=10, D)         # A=1, B=2, C=10, D=11

Or can also part of a type:

    type colours = enum (red, green, blue)
    type lights  = enum (red, amber, green)

Here, the names need to be qualified: you have to write colours.green (2), or lights.green (3). However the type system isn't that sophisticated, so the actual types are merely ints, and nothing stops you using colours.green or lights.green interchangeably.

Typed enums are not used much, and actually, enums themselves are rare because I normally use the **tabledata** feature next:

### Tabledata
This is an unusual feature that defines sets of enums, and parallel data arrays, at the same time:

    tabledata() []ichar colournames, []word colourvalues =
        (red,       $,      0xFF'00'00),
        (green,     $,      0x00'FF'00),
        (blue,      "Blue", 0x00'00'FF),
    end

This defines enums red=1, green=2, blue=3. And an array \[1..3\]ichar with the values ("red","green","Blue"). And an array \[1..3\]colourvalues with the given numbers.

The "$" is a device which returns the name of the last enum defined, so it saves having to duplicate each enum name, but you can just use a regular string.

In this form, entries can be added, deleted or moved very easily. Notice the trailing comma on the last entry to facilitate this.

The arrays can be zero-based (or some other value): use \[0:\] on the array declarations, and start with red=0. (But don't try to override the other enum values, as the array mapping can't cope with that.)

The () in tabledata() can contain a type name to contain the enums, as suggested above, eg:

    tabledata(colours) ....

Or the () can be omitted completely, then it just defines parallel arrays, no enums:

tabledata []ichar names, []int values =
    ("one",   100),
    ("two",   200),
    ("three", 300),
end

### Lengths and Bounds

The following can be applied to arrays and slices:

    .len       # length of array or slice
    .lwb       # lower bound
    .upb       # upper bound
    .bounds    # returns a range lower..upper (compile-time only)

The following works for any type:

    .bytes     # byte-size in any type or expr

And this for primitive types:

    .bitwidth

### Binary Operators and Precedence Levels

(Low precedence

    :=      1   assign

    +       6   (binary) add
    -       6   (binary) subtract
    *       7   multiply
    /       7   divide (both integer and floating point)
    rem     7   remainder (modulo)
    **      8   raise to power
    iand    6   bitwise and
    ior     6   bitwise or
    ixor    6   bitwise xor
    <<      7   shift left
    >>      7   shift right
    in      4   (see range/set constructs)
    notin   4   also 'not in'; opposite of 'in'
    min     6   binary minimum
    max     6   binary maximum
    
    and     3   logical and (short circuit operators used in conditional exprs)
    or      2   logical or
    xor     2   logical xor

    =       4   equals
    <>      4   not equals
    <       4   less than
    >       4   greater then
    <=      4   less than or equal
    >=      4   greater than or equal

    ..      5   Make range

### Unary Operators

    +       -   plus
    -       -   (unary) negate
    inot    -   bitwise not
    abs     -   absolute value
    
    ++      -   increment (used as prefix or postfix)
    --      -   decrement (used as prefix or postfix)

    sqr     -   square (x*x)
    sqrt    -   square root
    sign
    sin
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
    fmod
    atan2                (Used as atan2(x,y))

Note that many of these are functions in other languages, but are operators here. That means parentheses are not needed (but usually advised), but also they are properly overloaded.

For example, **abs** can be applied to ints or reals, and will give the expected answer (try using abs() in C on a float rather than int):

    int a,b; real x,y
    b := abs a
    y := abs x

### Precedence Levels

    8   **                             # highest
    
    7   * / rem << >>
    6   + - iand ior ixor
    5   ..
    4   = <> < <= >= > not notin xor
    3   and
    2   or

    1   :=                             # lowest

Disregarding "\*\*" and ".." which don't exist in C, there are six levels, compared to a 11 in C.

### Array Indexing

If A is an array, then you can index it as:

    A[i]

If A is a pointer to array, it must be dereferenced first:

    A^[i]           # (M now makes such a deref optional)

Where there are multi-dimensions of a flat array, then the indexing goes like this:

    A[i,j,k]
    A[i][j][k]          # C style alternative

With slicing, then a slice can be created using:

    A[i..j]

(This results in a slice that can be indexed like an array, so A[i..j][k], which can be written as: A[i..j,k]. Or it can be further sliced: A\[i..j, k..l\])

As a convenience, the special symbol $ used in an array index, returns the upper bound of that array. So A[$] is the last value, equivalent to A\[A.upb\].

### Pointer Dereferencing

This is done with the "^" symbol, applying as as suffix to a term like in Pascal:

    P^
    (P+i)^

And can be used consistently with multiple derefs:

    Q^^
    R^.S^.T                # (In such instances, that ^ can be omitted for cleaner code.)
    R.S.T                  # Provided ^ is followed by "." or "[", it can be omitted for cleaner code

Each ^ can be cancelled by one &, so that &Q^^ is equal to Q, and &&Q^ equals &Q.

When mixing ^ with ++ and --, it needs to be used like this:

    P++^        # Increment P then deref the original value
    ++P^        # Increment P then deref the new value


### Field Selection

Little to say about this, except it uses the common "." notation:

    pt.x + pt.y

When used with a pointer to a record P, it's like this: P^.x or the ^ can be omitted for P.x.

Note that "." is also used for selecting names from a namespace. A namespace might be a module, function or record. That use is detected and resolved at compile time:

    a.b.c

It's not possible to tell, until resolved, whether both dots select fields, or just the second, or neither.
        
### Min/Max and Clamp

min/max are built-in operators, and can be used like this:

    c := min(a,b)
    a max:=b

They will work on numeric types.

Clamp is used as follows:

    c:=clamp(x,a,b)

and is equivalent to c:=min(max(x,a),b)

### Assignment and Equality

Assignment always uses ":=" for assignment, and "=" for equality. This reduces the potential for mistakes as is common when "=" and "==" are used.

Assignments can of course be used inside an expressions which is how the mixup arises.

### Augmented Assignments

These are assigments such as:

    a +:= b
 
which means a := a + b. In M, these are only allowed as standalone statements, as their use inside expressions is confusing.

Such assignments are allowed for +, -, \*, /, iand, ior, ixor, min and max:

    a min:= b

Unusually, M allows augmented assignments for some unary operators too:

       -:= a       # a := -a
     abs:= a       # a := abs a
     not:= a       # a := not a
    inot:= a       # a := inot a
 
### Multiple Assignment

This is similar to what is used in Python:

    (a, b, c) := (b, c, a)

    (a, b) := (10, 20)

    (a, b) := (b, a)

The last will do the swap operation, but not as efficiently with complex terms as each is evaluated twice.

This can also be used when the right-hand-size is a function returning multiple values:

    (a, b) = fn()

### Expression List

This is the equivalent of C's comma-operator:

    a + (x; y; z)

but is written with ";", and with mandatory parentheses. x and y are evaluated first (usually they will have some side-effects), then z, which is the result of the expression list.

(Here, also, x, y, z can be statements.)

### Chained Compare Operators

The compare operators are =, <>, <, <=, >= and >.

When combined like this:

    if a = b = c

then that means:

    if a = b and b = c

Other examples:

    if a <= b < c            # a <= b and b < c
    if a > b < c = d         # if a > b and b < c and c <= d
    
If you need to use the 1/0 return value of a=b, then break it up using parentheses:

    if (a = b) = (c = d)

(Middle terms such as in a==b==c are evaluated once only.)


### Swap

Any two fixed size compatible values can be exchanged like this:

    swap(a, b)

This is more efficient that doing it via (a,b):=(b,a), as terms are written once with less chance of error.

### Promotions

Like C, narrow integer types are widened, but to int64 rather than int32 as is common for C (word64 for unsigned).

The same applies to the char type, widened to C64. But real32 floats are not widened to real64.

This usually applies to any term of an expression, except for terms like X.type, X.typestr, X.bytes, X.bitwidth where the non-promoted type of X is used.


### Mixed Sign Arithmetic

In M, this will be done in signed mode: the unsigned operand is converted to signed. The rules here (combined with promotion) are simple compared with C:

    Unsigned + Unsigned   => Unsigned
    Unsigned + Signed     => Signed
    Signed   + Unsigned   => Signed
    Signed   + Signed     => Signed

There are no exceptions, and they do not depend on the width of the types.

### Type Conversion and Punning

Type conversions are written like this:

Simple type conversions are written like this:

    int(x)

although it has to be a conversion that is allowed. However, an ambiguity in the current syntax means that elaborate types (more than one token) have to use this more general form:

    cast(x, int)

There is also type punning, which is a re-interpretation of a type without changing anything:

    real x := 1.1

    println int64(x)       # display 1
    println int64@(x)      # display 4607632778762754458 (0x3FF199999999999A)

The @ symbol makes it type punning (equivalent to \*(int64_t\*)&x in C, unlike C, can be applied to rvalues too, that is, expressiomns: int@(x+y)).

Again, for complex types, use cast@(x,int64).

Sometimes, it can be difficult to get on top of which precise conversion is needed, as happens with pointer types. Then, 'cast' can be used to automate it:

    ref int64 p
    ref []int q

    p := cast(q)             # omit the target type

'cast' will apply whatever cast is required. This is handy when the necessary type needs to be tracked down, or when it is likely to change.

(There are issues at present with getting function pointer types to match properly. So here cast() is an easy way to do that.)

### Conditional Statements

This is mainly the **if** statement:

    if cond then
       stmt1
       stmt2
    elsif cond then
       ....
    elsif cond then
       ...
    else
       ...
    fi

With **elsif** and **else** both optional so that a simple if-statement is:

    if cond then
       stmts...
    fi

(You can use **end**, **end if** or **endif** in place of **fi**)

There is also:

    unless cond then
        stmts....
    else                   # optional
        stmts....
    end
 
with the opposite logic. (Sometimes this helps, sometimes not.)

When you have this pattern: if x=a then.. elsif x=b then ..., then consider using the **case** statement which is designed for exactly that.

Sometimes, if, case and switch can be combine to form a composite statement:

    if cond then
    elsif cond2
    elsecase x
    when a then
    when b,c then
    else
    fi              # final block delimiter needs to match opening keyword
 

### Conditional Suffixes

Some control-flow statements can have a conditional suffix. For example:

    goto finish when x=0
    return 3 unless n<0

The possible condition keywords can be:

    when expr
    unless expr          # this one has the opposite logic

('if' used to be allowed but in the revised language that has an ambiguity.)

The statements where such a suffix is allowed are:

    return
    goto
    stop
    exit
    redo
    restart
    next

### Goto

This can be written as:
````
    goto label
    go to label
    label
````
The last is handy if you don't like the head of using 'goto', as it makes it less obvious.


Labels need to be written with two colons:

    lab::

(":" is heavily used, but if ambiguities can be sorted, it will be possible to is use lab:")

A mild variation is the experimental feature **recase**:

    case x
    when a then
    when b then
        s1;
        recase a
    esac

**recase** here will jump to the branch of the case statement that deals with 'a', equivalent to reentering the case statement with x = a (but x is not actually changed).


### Loops

Modern languages seem to be lacking in looping constructs even though, as mere syntax, they have little cost. M offers:

    do ... od                      # endless loop
    to n do ... od                 # repeat n times
    
    for i:=a to b do ... od        # iterate from a to b (see below)
    for i in A do ... od

    while x do ... od
    repeat ... until x

There are also looping versions of **switch** and **case** statements.

There is no equivalent of C's open 'for' loop which encourages all sorts of weird  and wonderful constructions, usually all on the same line.

Loop controls include **restart**, **redo**, **next** and **exit**, and can be used to any level of nested loop.


### For Loops

The full syntax for iterating over an integer range is:

    for i := a to b by c when d do
      ....
    else
      ....
    end

This iterates the loop variable of over a to b inclusive, stepping by c. 'when d' can be used to conditionally execute any particular iteration. The 'else' part executes on normal termination.

The loop index does not need defining; it will be auto-declared using the equivalent of 'let', so that you can change it inside the loop. (To that, declare it outside.)

But many parts are optional, and a more typical loop is:

    for i := a to b do
        ....
    end

Even shorter forms include:

    for i to b do ...            # start from 1
    to b do ...                  # this is now the repeat-n-times loop
    do ...                       # and this is the endless loop
    
Note that such a loop will always count upwards. To count downwards, use 'downto' instead of 'to' (or 'inrev' instead of 'in')

Alternative syntax is:

    for i in a..b do ...
    for i in A.bounds do ...     # equivalent to for in in A.lwb..A.upb do

For iterating over values, use:

    for x in A do ...            # A must be indexable: array, slice or string

The loop index is auto-declared to be a suitable type for the elements of A. To get the index, use:

    for i,x in A do ...          # i is the index (.lwb to .upb) x is the value

x is an rvalue, so you can't change an element of A. This needs to be done as A\[i\] := ...

### Loop Controls

* **restart** Restart the loop (mainly applies to for loops)
* **redo** Repeat this iteration
* **next** Continue to next iteration
* **exit** Break out of the loop

This can be used with nested loops by supplying a loop index:

    exit         # Exit from this inner loop
    exit 1       # Same thing
    exit 2       # Exit from next outer loop
    exit 0       # Exit from outermost loop
    exit all     # Same thing

All these controls can also be used with a conditional suffix:

    exit when c=0

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

x is an integer expression; a,b and c should be constant int expressions, and should not span a greater range than is suitable for a jump table.

Test expressions can be ranges:

    switch c
    when 'A'..'Z','a'..'z', '_' then
        ....

There is a limit of a few hundred values between smallest and largest in a switch. If exceeded, try using a **case** statement.

### Case Statement

When x is not an integer, or a, b are not integers or not constants, or the range is too wide, then a **case** statement can be used in place of switch:

    case x
    when a, b then
        ....
    when c then
        ....
    else
        ....
    esac

Case statements will sequentially test x against a, b and c in turn, until the first match. (Currently ranges can't be used.)

One variation on case is when the test expression is omitted:

    case
    when a=b, c=d then
        ....
    when e<0 then
        ....

Then it will evaluate expressions until one returns true, then it will execute that when-block.

### Looping Switch and Case

Both switch and case statements have looping equivalents:

    doswitch p++^
    when a,b,c then
    when ...
    else
    end switch

    docase x
        ...
    end

At the end of each when or else block, it will jump back to the start to repeat. Some means is usually needed to exit at some point (exit, goto, return, stop).

### Print and Println

M still retains print and println as statements:

    println a,b,c

This displays a, b and c, whatever type they happen to be, then adds a newline. **print** doesn't add the newline.

To print to a file open as handle f, use:

    println @f, a,b,c

The values are written with spaces in between. To suppress the space, use a double comma:

    print a,,b,c

This gets very ugly and is not satisfactory, but it will do for informal printing. For that purpose, there is also the "=" prefix:

    println =a,=b,=c

which adds "A=", "B=" and "C=" before each value respectively.

Otherwise, it is necessary to use formatted print, which looks like this:

    fprintln "#-#.(#)",a,b,c

If a is 10, b="abc" and c is a pointer with value 0x0001234, then the output will be 10-abc.(0001234). To output an actual "#", use:

    fprint "###", "#",name,"#"      # when name is "abc", will display #abc#

Individual formatting is done like this:

    fprintln "#-#.(#)",a:"z10",b,c

In this example, a is shown in a field 10 chars wide, with leading zeros.

There also ways to print into a string:

    println @&.str, "One","Two"

(Both print and read need further docs to use effectively, but these are the basics.)

### Read and Readln

Read and Readln are also available, but I'd need to research more how they work. They are used as follows:

    int a,b
    real c

    print "?"
    readln a,b,c

or:

    readln
    read a,b,c

**readln** reads the next whole line into a buffer, from the console (or from
a file using readln @f). Then read statements consume inputs from that buffer.

That's why they can be part of the readln, or follow later.

(Reading into a string isn't ready yet; 'readstr' is used for that. See extract.m in Examples.)


### Range Construct

A range is a pair of ints defining a sequence, such as 1..10, which means to 1 to 10 inclusive, and always stepping by 1.

It's not a type in itself, but it can be used as a construct:

* For array bounds
* In switch-when expressions
* With the **in** operator
* As part of a Set constructor.

Examples:

    [1..10]int a
    
    switch c
    when 'A'..'Z' then
    ....
    
    if a in [32..63] then ...
    
    ... ['0'..'9','-','+','.']
    

### Set Construct

Like a range, a set is not a proper type (both rangea and sets are full types in the companion script language)/

A limited versions of sets allow the syntax to be used in conditionals like this:

    if x in [a, b, c]

It doesn't yet allow ranges as in "\['A'..'Z','0'..'9'\]". It it still handy for quickly comparing an expression (evaluated once) with a handful of values.


### Equivalence


This feature allows two variables to share memory:

    real x
    int a @ x

Here, a shares the same memory as x. Note that a should not be smaller than x (unless that is the intention, or perhaps x is itself equivalenced to a larger variable).

Currently the expression after @ must be simple. (Previous versions allows indexing and other expressions, but I am considering whether to replace this old feature completely. It was originally taken from Fortran.)


### Foreign Functions

Foreign functions are any that are not directly part of this program. That is, not defined in any of the source modules that are compiled together to build this executable.

They must reside in an external DLL.

To use them, they must be declared in an **importdll** block, for example:

    importdll msvcrt =
        clang proc puts(ichar)
        clang function printf(ichar, ...)int
    end

Such names will be made available to the rest of the module. And also, if this module is imported elsewhere, they are exported to those other modules.

Each declaration must start with the language the function uses. Or rather the call convention used. Currently there are three possibilities:

* **clang**
* **windows**
* **mlang**

**windows** is not a language, it is to show the Windows call convention is used rather then normal C.

(Since M mainly works on Win64, and since it now complies with the Win64 ABI, then all languages ought to use the same call convention.

So "clang" and "windows" are equivalent. 'mlang' would be too, except M deviates from the ABI for certain types.)

Where a foreign function has a case-senstive name, it must be in quotes:

    import windowsdlls =
        windows function "MessageBoxA" (int=0, ichar message, caption="Title", int=0)int
    end

But then it can be used like this:

    messageboxa(message:"hello")

This also demonstrates adding optional parameters with default values, and  using keyword parameters.

Foreign function can also be written as \`MessageBoxA, but then all instances must be written the same way. The advantage is that this can distinguish between names that can clash when converted to lower case, or names that clash with a reserved word.

(There used to be an option to supply an alias to function, such as:

    function "MesssageBoxA" as "messagebox" ...

but it's missing at the moment. You can try using macros:)

    macro messagebox = messageboxa

### Creating DLL Files

The ability to directly generate DLL files is a recent development:

* Use the -dll option to generate DLL rather than EXE.

* Use **export** rather then **global** to mark names that will be exported from the DLL.

* **export** can also be used on variables, but currently M doesn't have the right mechanisms (within generted EXE) to do the correct fixups for imported DLL variables. (But they will be available to other languages.)

* **export** can also be used on types, macros, named constants. These aren't physically part of the DLL, but they end up in the exports file

* As well as lib.dll, a lib.exp file is also generated. This is an M module (needs .exp to avoid ovewriting a possible lib.m module), that contains a fully fillled in importdll block.

* Use the library from another M module by writing 'importx lib' (when the DLL is lib.dll and the export file is lib.exp)

* (Shortly M will also generate a .ms file to allow the library to be directly used from its companion scripting language.)

* If docstrings have been provided, then an extra -docs option will generate lib.txt containing descriptions of exported functions. (Not complete; function signatures not yet generated, only names.)


### Compiler Variables

These are special built-in variables that can be used to determine various aspects of the compilation:

    $lineno         Current line number as an int constant
    $strlineno      Line number as a string
    $modulename     Name of module
    $filename       Filename of module
    $function       Current function name
    $date           String constants containing date or time of compilation
    $time    

### Standard Libraries

* **msys**    Support library implicitly imported in every module (to omit for minimal programs, use -nosys, and an entry point other than start or main)
* **mlib**    Small runtime library
* **clib**    Interface to some C library functions
* **oslib**   Selection of functions implemented by the OS
* **osdll**   DLL function handling when names/params not known until runtime

### **nil** Pointer Constant

This is designated by **nil**, which has type 'ref void'. It is used to set pointers to 'unused', or not pointing at anything.

### System Constants

There are a small number of predefined constants, such as **pi**, but I'm going to revise this part of the language.

### Inline Assembly

The M language has always had inline assembler available in a straightforward manner:

    assem                   # block of assem instructions
        mov D0,[a]
        add D0,4
    end

    asm inc dword [b]       # one instruction at a time

Since M's register usage is unsophisticated, interaction with M is simple.

(That is not quit the case when optimisation is used. But in that case, optimisation is disabled for functions using inline ASM.)


### Using the M Compiler

Given a program consisting of multiple modules, of which the lead module (containing the start() function) is prog.m, and the M compiler is called mm.exe then use:

    mm prog

assuming mm is in a suitable search path.

This will compile prog.m and **all** linked imported files, into prog.exe.

For other options, try 'mm -help'.

### Grouped Imports

Sometimes there are modules A, B, C which work together to provide some functionality, but you need to import all of them. It is tider to put those imports into a new module X:

    import A
    import B
    import C

Then just do this in your program:

    import* X

This will make all the modules imported by X, also known to this program.

X needn't be an an empty module; it can have useful content. So if A already imports B and C, you can do this:

    import* A


### Naked Functions

What are sometimes called naked functions, are those with no entry or exit code.

In M, they are written like this:

    threadedproc fn =
     ....
    end

They are called threadproc because they were created to implented threaded-code handlers for an interpreter. There, you would jump to such a procedure, and then jump to the next, without using normal call and return.

For such reasons, parameters and local frame variables are not possible (only static ones).

### Macros

There is a new, simple implementation of macros, example:

    macro getopnd = (pcptr+1)^
    macro dist(x,y) = sqrt(x*x+y*y)

That second form can be used for inline functions. The body of a macro must always be a single, well-formed expression or term. However, a sequence of statements can trivially be turned into an expression:

     (s1; s2; s3)

So multi-line macro bodies are possible. What is not allowed are definitions (as macros are expanded in a later pass after the symbol table is completed).

### Bit Indexing

If A is an integer, then it can be indexed like this:

    print A.[i]                # 1 or 0
    A.[j] := 0

Indexing starts at zero, and goes up to 7, 15, 32, 63 or 127 depending on the size of A.

### Bitfield Indexing

If A is an integer, then an arbitrary bitfield can be extracted using:

    A.[i..j]               # i/j can be either order, eg. 0..7 or 7..0
    A.[i..j] := x          # insert x into that bitfield

(Both bit/bitfield indexing are incomplete. 128-bit operands not currently support, and some non-constant expressions for i/j may not be supported.)

### Standard Bit/Bitfield Codes

    A.msb          Top byte (most significant)
    A.lsb          Bottom byte (least significant)
    A.msbit        Top bit
    A.lsbit        Bottom bit
    A.msw          Top half
    A.lsw          Bottom half
    A.odd          1 when bottom bit is 1 (read-only)
    A.even         1 when bottom bit is 0 (read-only)

### Address-of Operator

This is like it is in C, but it is a little different for arrays:

    [10]int A
    &A           # type is ref[10]int
    &A[1]        # type is ref int

When you need a pointer to the first array element, then having to know the first element, or type &A[A.lwb], is a nuisance. Then it is possible to type:

    &.A

(This is a rare instance where C is more convenient. But this is a stop-gap until the correct address-of op is done automatically.)

& will prograte inside expressions such as &(c|a|b), so it is equivalent to (c|&a|&b).


### Stop Statement

    stop             Stop the program (like exit(0) in C)
    stop N           Stop with return code N (like exit(N))

### Slices

This is a new feature and quite a big subject to describe. Some examples:

    [10]int A           # normal array
    slice[]int S        # A slice

A slice is composite type consisting of (pointer, length). A slice can be initialised like this:

    S := (P,100)        # Construct from a normal pointer
    S := (&.A, A.len)   # slice to whole of A, but...
    S := A              # ... the conversion is automatic anyway.
    S := A[3..6]        # Slice to subarray

With a function F taking a slice type, it is possible to call it as:

    F(S)
    F(A)
    F(A[3..6])
    F((P,100))

While F might look like this:

    proc F(slice[]int S)=
       for i in A do
           println a,A[i]
       od
    end

or (using 'forall' which will change):

    proc F(slice[]int S)=
        forall i,x in A do
            println i,x
        od
    end

Applied to char arrays, this provides counted strings.

### 2-way Selection

Like C's ?: operator, M's is written like this:

     (A | B | C)

Evaluate either B or C depending on whether A is true or false. Unlike C, this can be used as an lvalue:

    (A | B | C) := 0          # set either B or C to zero
    &(A | B | C)              # evaluate &B or &C as & propagates inside.

In M, (a|b|c) is just another way of writing a regular if-statement. Nothing stops you using the regular form:

    print if a then "true" else "false" fi
    if A then B else C fi := 0 

Actually, a long-form 'if' can be used too:

   print if a then b elsif c then d ...

but is not recommended as an expression.

### N-way Selection

This extension to 2-way selects is written is:

     (n | a, b, c, ... | z)

This evaluates a, b, c ... according to n = 1, 2, 3 ... If out of range, z is evaluated. Again, this can be an lvalue. (There is no long form.)

For more complex selections, normal switch and case statements can be used.

### Initialisation of Arrays and Records

For static variables, scalars, arrays and records can only be initialised with compile-time expressions, or those involving addresses of static variables (there are limits on how complex the latter can be).

For non-static (ie. stack frame), they can be initialised in the same way they are assigned to.

Assigning to arrays and records is a recent feature, although there are some limitations: char arrays can't be initialised or assigned to this way.

This is becomes the assignment is done an element at a time, and would be inefficient to assign a string to an char-array by loading and storing a character at a time. Otherwise it can be:

    []int A := (x,y,z)      # x,y,z represent runtime expressions; A will have a length of 3
    date D := (x,y,z)
    D := (p,q,r)


### Creating Amalgamated Files

Apart from .exe, .dll and .asm output formats, there is one more: an 'amalgamated' file, with extension .ma.

This is not a true amalgamation as the result isn't a single module, but a simple collection of files (and support files) preceded by directory info that lists all the files. This makes it easy to copy, upload, transmit a project, without needing to use a binary format.

Also, M can directly compile a project in its amalgamated form. If the lead module of a project is prog.m, then:

    mm -ma prog          # collects all modules and support files into prog.ma
    
    mm prog.ma           # compile the project from that single file, into prog.exe

In this context, 'support files' are those incorporated using 'include', 'strinclude' and 'bininclude' directives.

#### Companion Dynamic Language

There is a companion language called Q, which is dynamically typed and interpreted. It shared largely the same syntax.

But this is being replace by a new scripting language that will be better integrated, more dynamic, and more easily embeddable.


### Shortcomings


* Not known to external tools so syntax highlighting either can't be applied, or will be wrong

* Most external libraries that might be compatible, will have C APIs. That requires interfaces for any library to be written as an **importdll** block in M. That is a lot of work. (Sometimes, you can create a smaller, tidier set of interface functions in C, then the interface in M to those functions will be smaller. But then you have an extra C dependency.)

* There is a limited amount of source code in M (currently some 100Kloc), so the tools will not have got the testing they would get if applied to a billion lines of C code. So there will be inevitable bugs, corner cases that have never been tested etc as well as language features that don't work as well as expected.

* There is a limited optimiser. The performance of generated code is midway between optimised gcc and Tiny C. (See Benchmarks article elsewhere on this site.)

