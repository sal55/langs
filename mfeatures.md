
## **M** Systems Language: Summary of Features

M is a systems language first devised for 8-bit Z80 systems in early 1980s, since evolved and now targeting 64-bit Windows machines using x64 processor.

The following is not a formal reference but is a random collection of features, ideas and aims that may be useful to others. Or to help understand M source code.

This is an early draft, and there will be typos and omissions, mistakes and probably duplicates. It is also disorganised, but should be enough to get an idea of the language. I can't promise everything mentioned works in the actual M compiler.

### The M Compiler

This is written in M. It builds to a single executable file that I usually call mm.exe, currently some 0.6MB, which includes a small set of libraries. (That is, the sources for the libraries are part of the executable.)

This gives a tidy self-contained compiler.

### Dependencies

There are none, when mm builds a .exe file, other than needing a Windows computer. M programs make use of the C standard library, via the MSVCRT.DLL library that comes with every Windows system.

However, there is no direct support for generating .dll files; these require that mm generates a .obj file instead, and an external linker (eg. gcc) used to create the .dll.

### Whole Program Compiler

A 'program' in this sense is the collection of modules and files that form a single .exe or .dll file. M will always compile all modules from scratch. (Effectively, the 'compilation unit', or granularity, moves from one module, to one program.)

### No Make or Project Files
M builds a project by submitting only the lead module. It will then locate all necessary modules (by following import links), to produce an executable named after the lead module.

### Compilation Speed
I've taken my eye off the ball recently, but it will still compile at hundreds of thousands of lines per second on my slowish PC with a conventional hard drive.

### Optimisation
I don't have an optimiser right now, and the code is poor. However this doesn't affect these compiler projects too much. The M compiler can still build itself from scratch (some 40Kloc in 30+ modules) in about 0.2 seconds, even though mm.exe is built as unoptimised code. 

### C Target
At various times, I have supported a C target, so generating a monolithic C source file instead of .exe or .obj or .asm. This gives the advantage of being able to compile programs for Linux, and to take advantage of optimising C compilers like gcc.

But it is currently dropped as it requires a considerable effort. Also some features are problematical, or are not supported. (Also, unless Tiny C is used to compile the output file, compilation hits a brick wall as soon as gcc is invoked, as that compiler is much slower.)

### ASM Target
This was the main target before mm could directly generate .exe or .obj files directly. It was in the syntax for my own very fast assembler (some 2M to 3M lines per second). Although no longer needed, it has handy for debugging, or for perusing the output. This is enabled using the -c option (use 'mm -help').

The .asm output is again a single, monolithic file, which can actually be assembled into .exe or .obj - you need the 'AX' assembler/linker project, also written in M.

### M Syntax 
Originally inspired by Algol-68, but has evolved its own style. Best described by looking at example programs.

### Modules and Imports
This is another big difference from C (and C++ is only now acquiring modules). M has no header files and has not nee for declarations as well as definitions.

You define a function, variable, named constant, type, enum, macro etc in its own module. To export it, add a 'global' attribute in front, otherwise it is private to that module. 

To use those exported names from module A in another module B, that is when you write 'import A' inside B.

### Circular and Mutual Imports
Previous versions of the module system required modules to be in a strict top-down hierarchy. That was too restrictive. The current scheme allows imports in any order including circular imports: A can import B, and B can import A.

(There is a downside: the order in which default initialisation routines are called is no longer determinate.)

### Out of Order Definitions
Unlike C, functions can be defined in any order in a module. If a function F calls G(), and G is defined later on, you don't need a forward or prototype declaration for G.

Actually, this also applies to other named entities, so you could for example choose to define all the local variables at the end of a function!

### Block Scopes
Another big departure from C, is that inside a function, there is only a single scope; there are no block scopes. Further, there is a single name space (no crazy tag namespaces and even label namespaces of C: you could write: 'L: int L; goto L;')

Since functions are best kept small, there is no real need for multiple scopes and overloading the same identifiers.

### Semicolons
While M ostensibly requires semicolons to separate statements, in practice these are rare in M source code. This is because newlines are converted to semicolons, except when:

    * A line-continuation character \ is used at the end of the line
    * The line clearly continues onto the next because it ends with one of:
       "(" "[" "," or a binary operator

### Comments
M only has single-line comments starting with "!" until end-of-line. ("!" came from the DEC Fortran and Algol I used in the late 70s.)

It has had block comments in the past, but I believe those should be an editor function (which can use multiple "!" comments). (This also makes things simpler for highlighting editors, as it doesn't need to keep track of context from 1000s of lines before.)

This document will use '#' for comments in examples as it is clearer and more familiar.

### Doc Strings
This is something I've played with, and is in my other language, but temporarily missing from M. Doc strings are line comments starting with #, just before and/or inside a function.

A compiler option causes such documented functions to be written to a text file, with function signature shown plus the comments for each.

### Character Set
I haven't yet ventured into Unicode. Source code is written in ASCII, but can also be UTF8. UTF8 sequences can be part of comments or strings.

Newlines in source files must use CR/LF or LF sequences.
      
### Case-Insensitive
Another departure from most languages, especially those associated with C and/or Unix. M is case-insensitive, which means that all of these names are identical to it:

    abc abC aBc aBC Abc AbC ABc ABC

Identifiers in M use: A-Z a-z 0-9 _ $ and can't start with a digit.

(To access case-sensitive names used with external libraries, there are two schemes. One is to declare the name as a string, but can then be used in any case. The other is to use the \` prefix, which also allows names that are reserved words:

    clang proc `exit(int)

Here, it is necessary because 'exit' is a loop control statement in M.)

### Program Entry Point
This is usually the start() function, which is always global (ie. exported, no 'global' needed.) start() takes no parameters.

M will insert a call to a start-up routine in M's runtime module, to set up command-line parameters etc as global variables (nsysparams, and sysparams, the latter being an array of strings).

main() can also be used as an entry point, but no special code is injected. (Used sometimes to create a minimal program.)

### The $init function
If encountered in a module, it will be called automatically by start-up code. No 'global' attribute needed. However, because of non-determinate module import order, if such a routine depends on another $init function being called first, then this must be handled manually (with flags and direct invocation etc).

### Include, Strinclude and Bininclude
**include** is an ordinary textual include, and is only needed in M when you actually want to include code (not headers) from another file. (For example, a file generated from a program.)

**strinclude** can be used in an expression, and can include any text file as a string constant. (I use this to incorporate the sources for M libraries into the M compiler; or the C header sources into my C compiler.)

Example in a program called prog.m, this prints itself:

    proc start =
        println strinclude "prog.m"
    end
    
**bininclude** is a variation used to initialise a byte-array, and can refer to any file including binaries. However it's done inefficiently at present.

### Conditional Compilation
M has no preprocessor and no conditional directives for code (there used to be, but I didn't like them).

Conditional code is handled at the module rather than line level. It can look like this:

    mapmodule pc_assem => pc_assemc when ctarget

Where, when 'import pc_assem' is encountered, it will instead import 'pc_assemc' (in this case a dummy module with empty functions). 'ctarget' can be a built-in flag or one set with compiler options.

This keeps the contents of each module clean. (Look at some C system headers to see what happens with most code is a patchwork of #if/#ifdef blocks.) 

### Function Tables
There is a limited amount of reflection in that all functions (names and addresses) in the program are written to the executable, and can be accessed via special functions.

This allows finding out the name of a function from a function pointer. But what I most use it for is building, at runtime, tables of functions pointers for special handlers. For example, handlers for the commands of, say, an editor, may have functions with names such as 'ed_left', 'ed_delcharright'. By searching for a function with that name (in practice then store in a table to avoid a search), I can add functionality as needed, without needing to maintain tables of pointers.

### Data Types
M is low-level so has mainly simple, fixed-size types: scalars, records (ie. structs) and fixed-length arrays with a size known at compile-time.

Dynamic arrays with a length known at run-time can be created with pointers and allocations, but the size remains fixed.

A new type recently added are slices or views into arrays and strings, which can do more along those lines (see below) but I haven't done much with them yet.

In current development (although shelved for a while) is a version with higher level types. (M did have variant types briefly, allowing flex lists, big nums and such, but I decided they were not a good match for M in that form.)

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

(There had been also machine types intm and wordm which were 32 or 64 bits depending on target, also intp and wordp for widths matching those of a native pointer, but since I'm concentrating on 64-bit machines, and assuming 64-bit pointers, those will be dropped.)

### Type of Integer Constants
These are defaults before any casts are applied, depending on the magnitude of the constant:

    0 to 2** 63-1              int64
    2**63 to 2** 64-1          word64
    2**64 to 2** 127-1         int128
    2**127 to 2**128-1         word128
    2**128 and above          'decimal' type (not implemented in this version)

(No suffixes are used, except that in the next M version -L is used to force a decimal type for integers and floats.)

### Numeric Separators

Numeric constants including floating point can use ' _ or \` to separate groups of digits.

### Numeric Scaling

M allows scale factors such as 'million', used like this:

    4 million           # 4000000

There used to be others (like m, cm and km to scale lengths to the same units), but this part of the language will be reviewed to try and have user-definable scale factors.

Such names are in their own namespace; 'million' can still be used as an identifier.

### Number Bases

Number bases from 2 to 16 can be used for integers and floats, eg:

    2x100    # base 2; 4
    3x121    # base 3; 16
    8x377    # octal: 255
    12xBBB   # base 12: 1727 (not sure if bases 10x to 15x work in this language)
    0xFFF    # hex
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

Multi-character constants are possible, with these types (which I'm going to tweak today actually so that most will have int types): 

    'A'                        c8
    up to 'ABCDEFGH'           u64
    up to 'ABCDEFGHIJKLMNOP'   u128

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

Compared to C (with its convoluted inside-out type-syntax with the name of the thing being declared somewhere in the middle), it's really easy with none its quirks. See sample source files.

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

(There are is limited support for passing multi-dimensional arrays to functions; the dimensions must be constants, except the first can be unbounded.)

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

Here the '&' signifies a reference parameter. Functions require a return type, and they need to return a value. Alternate syntax:

    function add(int a,b) => int = {a+b}

This demonstrates:

   * Parameter names with the same type can share the type (unlike C)
   * => can be optionally used denote a return type
   * {...} braces can be used around a function body, the only place they are used. ({,}  were part of a more general syntax for deferred code, such as lamdbas, but never got around to that. Function bodies are certainly deferred.)

   * 'Return' is actually optional.

### Function Parameters
M has default and keyword parameters:

    proc createwindow(int dimx=640, dimy=480, posx=0, posy=0, border=1) ...

    createwindow(dimx:1920, dimy:1080)

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
An experimental feature limited to 3 scalar return types:

    function fn => int, int, int =
       return (10, 20, 30)
    end

    (a,b,c) := fn3()

Return values can be ignored:

    (a,b) := fn3()   # discard last
    a := fn3()       # discard last two
    fn3()            # discard all

### Define Variables
This is fairly standard; inside a function (anywhere in the function actually):

    int a, b:=123, c

M syntax actually required 'var' because I liked the idea of all definitions/declarations starting with a keyword:

    var int a, b:=123, c

but I tried this for a few months, and it was a pain. So now 'var' is optional. Note that writing this, which needs 'var' but without the type:

    var A, B

is not an error. Currently omitting a type makes A and B variants. I want to drop variants; in the next language version, omitting the type like this makes A and B have 'auto' type, which means it is inferred from initialisation or from the first assignment.

At the moment, non-static variables are not automatically initialised to anything, like C.

### Readonly Variables
I've never been a fan of C's 'const' attribute, which really complicates the type system. M never had anything like that until recently, when it was possible to use 'let' instead of 'var':

    let A := 100

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

The arrays can be zero-based (or some other value): use [0:] on the array declarations, and start with red=0. (But don't try to override the other enum values, as the array mapping can't cope with that.

The () in tabledata() can contain a type name to contain the enums, as suggested above, eg:

    tabledata(colours) ....

Or the () can be omitted completely, then it just defines parallel arrays, no enums.

### Lengths and Bounds

The following can be applied to arrays and slices:

    .len       # length of array or slice
    .lwb       # lower bound
    .upb       # upper bound
    .bounds    # returns a range lower..upper (compile-time only)

The following for any type:

    .bytes     # byte-size in any type or expr

And this for primitive types:

    .bitwidth

### Binary Operators and Precedence Levels

    :=      9   assign

    +       4   (binary) add
    -       4   (binary) subtract
    *       3   multiply
    /       3   divide (both integer and floating point)
    rem     3   remainder (modulo)
    **      2   raise to power
    iand    4   bitwise and
    ior     4   bitwise or
    ixor    4   bitwise xor
    <<      3   shift left
    >>      3   shift right
    in      6   (see range/set constructs)
    notin   6   also 'not in'; opposite of 'in'
    min     4   binary minimum
    max     4   binary maximum
    clamp   -   clamp(x,a,b) restricts x to being within a..b

    and     7   logical and (short circuit operators used in conditional exprs)
    or      8   logical or
    not     -   logical not

    =       6   equals
    <>      6   not equals
    <       6   less than
    >       6   greater then
    <=      6   less than or equal
    >=      6   greater than or equal

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

Note that many of these are functions in other languages, but are operators here. That means parenthese are not needed (but usually advised), but also they are properly overloaded.

For example, **abs** can be applied to ints or reals, and will give the expected answer (try using abs() in C on a float rather than int):

    int a; real x
    a := abs a
    x := abs y

### Precedence Levels

    2   **                             # highest
    3   * / rem << >>
    4   + - iand ior ixor
    5   ..
    6   = <> < <= >= > not notin
    7   and
    8   or

    9   :=                             # lowest

Disregarding "\*\*" and ".." which don't exist in C, there are six levels, compared to a dozen in C.

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

Each ^ can be cancelled by one &, so that &Q^^ is equal to Q, and &&Q^ equals &Q.

When mixing ^ with ++ and --, it needs to be used like this:

    P++^        # Increment P then deref the original value
    ++P^        # Increment P then deref the new value


### Field Selection

Little to say about this, except it uses the common "." notation:

    pt.x + pt.y

When used with a pointer to a record P, it's like this: P^.x. (Again, M also allows P.x. Maybe I should have kept ^ mandatory to keep the docs simpler...)

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

    (a, b, c) = (b, c, a)

    (a, b) = (10, 20)

    a, b = b, a

The last will do the swap operation, but not as efficiently with complex terms as each is evaluated twice.

This can also be used when the right-hand-size is a function returning multiple values:

    a, b = fn()

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

(Note: middle terms are evaluated twice. This will be fixed eventually, but most uses have simple terms.)

### Swap

Any two fixed size compatible values can be exchanged like this:

    swap(a, b)

This is more efficient that doing it via (a,b):=(b,a), as terms are written once with less chance of error.

### Promotions

Like C, narrow integer types are widened, but to int64 rather than int32 as is common for C.

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

although it has to be a conversion that is allowed. However, an ambiguity in the current syntax means that elaborate types have to use this more general form:

    cast(x, int)

There is also type punning, which is a re-interpretation of a type without changing anything:

    real x := 1.1

    println int64(x)       # display 1
    println int64@(x)      # display 4607632778762754458 (0x3FF199999999999A)

The @ symbol makes it type punning (equivalent to \*(int64_t\*)&x in C).

Again, for complex types, use cast@(x,int64).

Sometimes, it can be difficult to get on top of which precise conversion is needed, as happens with pointer types. Then, 'cast' can be used to automate it:

    ref int64 p
    ref []int q

    p := cast(q)

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

With elsif and else both optional so that a simple if-statement is:

    if cond then
       stmts...
    fi

(Don't worry, you can use **end**, **end if** or **endif** in place of **fi**)

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
 
But use sparingly as it looks funny.
 
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

Not sure if 'goto' is mentioned anywhere, but is definititely part of the language. It seems to be out of favour these days. A mild variation is the experimental feature **recase**:

    case x
    when a then
    when b then
        s1;
        recase a
    esac

**recase** here will jump to the branch of the case statement that deals with 'a', equivalent to reentering the case statement with x = a (but x is not actually changed).

### Loops

Modern languages seem to be lacking in looping constructs even though, as mere syntax, they have little cost. M offers:

    do ... od                   # endless loop
    to n do ... od             # repeat n times
    for i:=a to b do ... od    # iterate from a to b
    forall x in a do ... od    # iterate over values in a
    while x do ... od
    repeat ... until x

There are also looping versions of **switch** and **case** statements.

There is no equivalent of C's open 'for' loop which encourages all sorts of weird  and wonderful constructions, usually all on the same line. (There was something similar based on 'while', but it was never used.)

Loop controls include **restart**, **redo**, **next** and **exit**, and can be used to any level of nested loop.

Some loops (for) can have an **else** part, which is executed on normal exit (abnormal ones include **goto**, **exit**, **return** and **stop**).

### For Loops

The full syntax is:

    for i := a to b by c when d do
      ....
    else
      ....
    end

This iterates the loop variable of over a to b inclusive, stepping by c. 'when d' can be used to conditionally execute any particular iteration. The 'else' part executes on normal termination.

But many parts are optional, and a more typical loop is:

    for i := a to b do
        ....
    end

Even shorter forms include:

    for i to b do             # start from 1
    to b do                   # this is now the repeat-n-times loop
    do                        # and this is the endless loop
    
Note that such a loop will always count upwards. To count downwards, use 'downto' instead of 'to'.
    
There is an alternative syntax:

    for i in a..b do          # expression after .. is either a range construct ...
    for i in A do             # ... or applies .lwb and .upb to create a range

**forall** is also available to iterate over values rather than indices, and currently works fine with slices:

    forall x in S do          # iterate over values in S
    forall i,x in S do        # same but expose the index in 'i'

This is of limited use for normal arrays unless the array bounds are known to the compiler. (Note that in the revised language,
'forall' is likely to be dropped, and I will just have 'for' to do both kinds of iteration. Details to be worked out.)

Note: loop index variables don't need to be declared. They are auto-defined as 'let', so that you can't assign to them. To be able
to assign to them, declare them outside.

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

Case statements will sequentially test x against a, b and c in turn, until the first match.

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

Otherwise, it is necessary to use formatted printed, which looks like this:

    fprintln "#-#.(#)",a,b,c

If a is 10, b="abc" and c is a pointer with value 0x0001234, then the output will be 10-abc.(0001234).

Individual formatting is done like this:

    fprintln "#-#.(#)",a:"z10",b,c

In this example, a is shown in a field 10 chars wide, with leading zeros.

There also ways to print into a string:

    println @&.str, "One","Two"

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


### Range Construct

A range is a pair of ints defining a sequence, such as 1..10, which means to 1 to 10 inclusive, and always stepping by 1.

It's not a type in itself (it will be in the next version), but it can be used as a construct:

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

Like a range, a set is not a proper type (I will try and have one in the next version).

Here, sets are used to construct int values of 64 or 128 bits. Such a construct can be used with the **in** operator:

    if c in ['A'..'Z', 'a'..z']
    if a in [1,2,4,8,16]

Since they are just integers, logical operations can be done between then.

Also, set inclusion can also be done with dot indexing:

    ['A'..'Z','a'..'z'].[c]
    const alpha = ['A'..'Z','a'..'z'].[c]
    alpha.[c]
    
However these will not do the range checking that **in** will do.

### Equivalence

This feature allows two variables to share memory:

    real x
    int a @ &x

Here, a shares the same memory as x. Note that a should not be smaller than x (unless that is the intention, or perhaps x is itself equivalenced to a larger variable).

The expressions available are simple: &x or &x+3, with the offset always in bytes. Possibly, indexing can be added to allow:

### Label Pointers

This is a possible feature (also implemented in C by gcc), where you can take the address of a label, store it in arrays and so on, and use that later in an indirect goto.

But at the moment its't not implemented. (For some kinds of programs, it results in faster execution that using a switch.)

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

**windows** is not a language, it is to show the Windows call convention is used
rather then normal C.

(On Win64, both Windows and C use the same call convention. M uses its own.)

Where a foreign function has a case-senstive name, it must be in quotes:

    import windowsdlls =
        windows function "MessageBoxA" (int=0, ichar message, caption="Title", int=0)int
    end

But then it can be used like this:

    messageboxa(message:"hello")

This also demonstrates adding optional parameters with default values, and  using keyword parameters.

(There used to be an option to supply an alias to function, such as:

    function "MesssageBoxA" as "messagebox" ...

but it's missing at the moment. You can try using macros.)

### Compiler Variables

These are special built-in variables that can be used to determine various aspects of the compilation:

    $lineno         Current line number as an int constant
    $strlineno      Line number as a string
    $modulename
    $filename
    $function       Current function name
    $date           String constants containing date or time
    $time
    $version

### Standard Libraries

* **msys**    Support library implicitly imported in every module
* **mlib**    Small runtime library
* **clib**    Interface to some C library functions
* **oslib**   Selection of functions implemented by the OS
* **osdll**   DLL function handling when names/params not known until runtime

### **nil** Pointer Constant

This is designated by **nil**, which has type 'ref void'.

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

That second form can be used for inline functions. The body of a macro must always be a single, well-formed expression or term. However, a sequence of statements can be trivially be turned into an expression:

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

This like it is in C, but it is a little different for arrays:

    [10]int A
    &A           # type is ref[10]int
    &A[1]        # type is ref int

When you need a pointer to the first array element, then having to know the first element, or type &A[A.lwb], is a nuisance. Then it is possible to type:

    &.A

(This is a rare instance where C is more convenient. But this is a stop-gap until the correct address-of op is done automatically.)

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

Scalars can be initialised in the same way they can be assigned to. But it is not possible to assign to arrays and records from an array/record constructor:

     [3]int A

     A := (10,20,x)           # not allowed, even if x is constant

They can be initialised from a construct, but only for static variables (ie. using '=' rather than ':='), and the constructs must consist of compile-time expressions, or static addresses in the case of pointers:

     static [3]int A = (10,20,30)    # Note '=' not ':='

Outside of a function. 'static' is not needed.

### Creating Amalgamated Files

Apart from .exe, .obj and .asm output formats, there is one more: an 'amalgamated' file, with extension .ma.

This is not a true amalgamation as the result isn't a single module, but a simple collection of files (and support files) preceded by directory info that lists all the files. This makes it easy to copy, upload, transmit a project, without needing to use a binary format.

Also, M can directly compile a project in its amalgamated form. If the lead module of a project is prog.m, then:

    mm -ma prog          # collects all modules and support files into prog.ma
    
    mm prog.ma           # compile the project from that single file, into prog.exe

In this context, 'support files' are those incorporated using 'include', 'strinclude' and 'bininclude' directives.

### Bugs and Limitations

There will be loads. This one problem with languages used by too few people, it does not get exercised enough. It is easy to get around limitations or bugs, or there will be ranges of features that haven't be used enough to know how well or otherwise they work.

### Shortcomings

Lots of nice features listed, but there are plenty of issues too:

* Not known to external tools so syntax highlighting either can't be applied, or will be wrong

* Most external libraries that might be compatible, will have C APIs. That requires interfaces for any library to be written as an **importdll** block in M. That is a lot of work. (Sometimes, you can create a smaller, tidier set of interface functions in C, then the interface in M to those functions will be smaller. But then you have an extra C dependency.)

* There is a limited amount of source code in M (currently some 100Kloc), so the tools will not have got the testing they would get if applied to a billion lines of C code. So there will be inevitable bugs, corner cases that have never been tested etc as well as language features that don't work as well as expected.

* There is no optimiser.

Basically, the main problem is that it is not C. Even though C is a ghastly language, it is everywhere, there are loads of tools for it, and huge numbers of people are familiar with it.

