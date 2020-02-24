'M' Systems Language Summary

M is a systems language first devised for 8-bit Z80 systems in early 1980s, since evolved and now targeting 64-bit Windows machines using x64 processor.

The following is not a formal reference but is a random collection of features, ideas and aims that may be useful to others. Or to help understand M source code.


**The M Compiler**

This is written in M. It build to a single executable file that I usually call mm.exe, currently some 0.6MB, which includes it small set of libraries. (That is, the sources for the libraries are part of the executable.)

This gives a tidy self-contained compiler.

**Dependencies**

There are none, when mm builds a .exe file, other than needing a Windows computer. M programs make use of the C standard library, via the MSVCRT.DLL library that comes with every Windows system.

However, there is no direct support for .dll files; these require that mm generates a .obj file instead, and an external linker (eg. gcc) used to create the .dll.  **Whole Program Compiler** A 'program' in this sense is the collection of modules and file that form a single .exe or .dll file. M will always compile all modules from scratch. (Effectively, the 'compilation unit', or granularity, moves from one module, to one program.)

**No Make or Project Files** Build a project by submitting only the lead module. It will then locate all necessary modules (by following import links), to produce an executable named after the lead module.

**Compilation Speed** I've taken my eye off the ball recently, but it will still compile at hundreds of thousands of lines per second on my slowish PC with a conventional hard drive.

**Optimisation** I don't have an optimiser right now, and the code is poor. However this doesn't affect these compiler projects too much. The M compiler can still build itself from scratch (from 40Kloc in 30+ modules) in about 0.2 seconds, even though mm.exe is built as unoptimised code. 

**C Target** At various times, I have supported a C target, so generating a monolithic C source file instead of .exe or .obj or .asm. This gives the advantage of being able to compile programs for Linux, and to take advantage of optimising C compilers like gcc.

But it is currently dropped as it requires a considerable effort. Also some features are problematical, or are not supported. (Also, unless Tiny C is used to compile the output file, compilation hits a brick wall as soon as gcc is invoked, as that compiler is much slower.)

**ASM Target** This was the main target before mm could directly generate .exe or .obj files directly. It was in the syntax for my own very fast assembler (some 2M to 3M lines per second). Although no longer needed, it has handy for debugging, or for perusing the output. This is enabled using the -c option (use 'mm -help').

The .asm output is again a single, monolithic file, which can actually be assembled into .exe or .obj - you need the 'AX' assembler/linker project, also written in M.

**M Syntax ** Originally inspired by Algol-68, but has evolved its own style. Best described by looking at example programs.

**Modules and Imports** This is another big difference from C (and C++ is only now acquiring modules). M has no header files and has not need for declarations as well as definitions.

You define a function, variable, named constant, type, enum, macro etc in its own module. To export it, add a 'global' attribute in front, otherwise it is private to that module. 

To use those exported names from module A in another module B, that is when you write 'import A' inside B.

**Circular and Mutual Imports** Previous versions of the module system required modules to be in a strict top-down hierarchy. That was too constricting. The current scheme allows imports in any order including circular imports: A can import B, and B can import A.

(There is a downside: the order in which default initialisation routines are called is no longer determinate.)

**Out of Order Definitions** Unlike C, functions can be defined in any order in a module. If a function F calls G(), and G is defined later on, you don't need forward or prototype declaration for G.

Actually, this also applies to other named entities, so you could for example choose to define all the local variables at the end of a function!

**Block Scopes**Another big departure from C, is that inside a function, there is only a single scope; there are no block scopes. Further, there is a single name space (no crazy tag namespaces and even label namespaces of C: you could write: 'L: int L; goto L;')

Since functions are best kept small, there is no real need for multiple scopes and overloading the same identifiers.

**Semicolons** While M ostensibly requires semicolons to separate statements, in practice these are rare in M source code. This is because newlines are converted to semicolons, except when:    * A line-continuation character \ is used at the end of the line    * The line clearly continues onto the next because it ends with one of:      "(" "[" "," or a binary operator

**Comments** M only has single line comments starting with "!" until end-of-line. ("!" came from the DEC Fortran and Algol I used in the late 70s).

It has had block comments in the past, but I believe those should be an editor function (which can use multiple "!" comments). (This also makes things simpler for highlighting editors, as it doesn't need to track context from 1000s of lines before.) 

**Doc Strings** This is something I've played with, and is in my other language, but temporarily missing from M. Doc strings are line comments starting with #, just before and/or inside a function.

A compiler option causes such documented functions to be written to a text file, with function signature shown plus the comments for each.

**Character Set** I haven't yet ventured into Unicode. Source code is written in ASCII, but can also be UTF8. UTF8 sequences can be part of comments or strings.

Newlines in source files must use CR/LF or LF sequences.
      
**Case-Insensitive** Another departure from most languages, especially those associated with C and/or Unix. M is case-insensitive, which means that all of these names are treated identically:

   abc abC aBc aBC Abc AbC ABc ABC

Identifiers in M use: A-Z a-z 0-9 _ $ and can't start with a digit.

(To access case-sensitive names used with external libraries, there are two schemes. One is to declare the name as a string, but can then be used in any case. The other is to use the ` prefix, which also allows names that are reserved words:

    clang proc `exit(int)

Here, it is necessary because 'exit' is a loop control statement in M.)

**Program Entry Point** This is usually the start() function, which is always global (ie. exported, no 'global' needed.) start() takes no parameters.

M will insert a call to a start-up routine in M's runtime module, to set up command-line parameters etc as global variables (nsysparams, and sysparams, the latter being an array of strings).

main() can also be used as an entry point, but no special code is injected. (Used sometimes to create a minimal program.)

**The $init function** If encountered in a module, it will be called automatically by start-up code. No 'global' attribute needed. However, because of non-determinate module import order, if such a routine depends on another $init function being called first, then this must be handled manually (with flags and direct invocation etc).

**Include, Strinclude and Bininclude** 'include' is an ordinary textual include, and is only needed in M when you actually want to include code (not headers) from another file. (For example, a file generated from a program.)  Strinclude can be used in an expression, and can include any text file as a string constant. (I use this to incorporate the sources for M libraries into the M compiler; or the C header sources into my C compiler.)

Example in a program called prog.m: println strinclude "prog.m"; a program that prints itself.

Bininclude is a variation used to initialise a byte-array, and can refer to any file including binaries. However it's done inefficiently at present.

**Conditional Compilation** M has no preprocessor and no conditional directives for code (there used to be, but I didn't like them).

Conditional code is handled at the module rather than line level. It can look like this:

  mapmodule pc_assem => pc_assemc when ctarget

Where, when 'import pc_assem' is encountered, it will instead import 'pc_assemc' (in this case a dummy module with empty functions). 'ctarget' can be a built-in flag or one set with compiler options/

This keeps the contents of each module clean. (Look at some C system headers to see what happens with most code is a patchwork of #if/#ifdef blocks.) 

**Function Tables** There is a limited amount of reflection in that all functions (names and addresses) in the program are written to the executable, and can be accessed via special functions.

This allows finding out the name of a function from a function pointer. But what I most use it for is building, at runtime, tables of functions pointers for special handlers. For example, handlers for the commands of, say, an editor, may have functions with names such as 'ed_left', 'ed_delcharright'. By searching for a function with that name (in practice then store in a table to avoid a search), I can add functionality as needed, without needing to maintain tables of pointers.

**Data Types** M is low-level so has mainly simple, fixed-size types: scalars, records (ie. structs) and fixed-length arrays with a size known at compile-time.

Dynamic arrays with a length known at run-time can be created with pointers and allocations, but the size remains fixed.

A new type recently added are slices or views into arrays and strings, which can done more along those lines (see below) but I haven't done much with them yet.

In current development (although shelved for a while) is a version with higher level types. (M did have variant types briefly, allowing flex lists, big nums and such, but I decided they were not a good match for M in that form.)

**Numeric Types**

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

(There had been also machine types intm and wordm which were 32 or 64 bits depending on target, also intp and wordp for widths matching those of a native pointer, but since I'm concentrating on 64-bit machines, and assuming 64-bit pointers, those will be dropped.

Note that the first version of M for x64, actually used 32-bit pointers, which could gave a 5% performance advantage, but it proved troublesome interfacing with 64-bit pointers from outside.)

**Type of Integer Constants** These are defaults before any casts are applied:

  0 to 2**63-1              int64
  2**63 to 2**64-1          word64
  2**64 to 2**127-1         int128
  2**127 to 2**128-1        word128
  2**128 and above          'decimal' type (not implemented in this version)

(No suffixes are used, except that in the next M version -L is used to force a decimal type for integers and floats.)


**Numeric Separators** ------------------
**Numeric Scaling** ----------------


**Number Bases**



**Numeric Limits and Type Sizes** Given any numeric type T or expression X, then:

    T.minvalue
    T.maxvalue
    X.minvalue
    X.maxvalue

will yield the smallest and largest values possible. (Next version, where a 'range' (see below) is an actual type, then I may introduce T.bounds to mean 'T.minvalue..T.maxvalue.)

For the fixed size of a type or expression:

    T.bytes
    X.bytes

For array lengths and bounds, there is a whole set of properties that can be extracted; see section on Lengths and Bounds.

**Character Constants** M uses ASCII, so 'A' has the value 65, but it has type 'char'.

Multi-character constants are possible, with the these types (which I'm going to tweak today actually so that most will have int types): 

   'A'                        c8
   up to 'ABCDEFGH'           u64
   up to 'ABCDEFGHIJKLMNOP'   u128

**Unicode String and Char Constants** Not sure how to tackle Unicode support yet.

**Type Reflection** The expression X.type returns an internal type code, so that it can be used as 'if X.type = int.type' for example. Although types are static, the result type of an expression may not be obvious. It can also be turned into a string:

   println X.typestr

may display "i64", or whatever arbitrary type X happens to be.

**Type Syntax** This is Algol68-based, with types written left-to-right:

   ref int A            # point to int
   [10]int B            # Array 10 of int
   ref[]ref real C      # Point to array of pointer to int

Compared to C (with its convoluted inside-out type-syntax with the name of the thing being declared somewhere in the middle), it's really easy with none its quirks. See sample source files.

**Type Aliases** These are written as, for example:

    type intptr = ref int
    type matrix = [4,4]int real

Like C, these do not introduce a new type.

**Pointers and Arrays** Unlike C, you can't index a pointer like an array, and you can't dereference an array like a pointer. Offsets to pointers can be done, but are written as '(P+i)^' rather than using P[i] like C, making it clear something underhand is going on.

**Auto Dereference** M used to be a more transparent language needing explicit derefs, but that has been relaxed. Now, a dereference op (a postfix '^' like Pascal) can be omitted in many cases:

    P^[i]   can be written as P[i]      (P is pointer to array)
    P^.m    can be written as P.m       (P is pointer to record)
    P^()    can be written as P()       (P is pointer to function)

Less transparent, but cleaner code. However sometimes ^ is still needed:

    print P^           otherwise it will print the pointer
    ++P^.m             otherwise it's parsed as (++P)^.m (working on this)  

**Array Bounds** Arrays normally are index from 1, but any lower bound can be used:

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

(There are is limited support for passing multi-dimensional arrays to functions; the dimenions must be constants, except the first can be unbounded.)

**Value Arrays** M arrays are always handled by value (C converts them always to a pointer, with a schism in the type system to ensure that). So:

   [10]int A,B
   A := B

However, there is little support for passing value arrays to functions or returning such arrays. (I used to allow that long ago, but it was never used.) Better ways to pass arrays are by pointer, by reference, or via a slice.

**Array Indexing** M allows multi-dimensional indexing using the more fluid A[i,j,k]
instead of the A[i][j][k] used in many languages. Although A[i][j[k] is legal too.

**Records** These are structs, eg:

    record date =
         int day, month
         int year
    end

    date d,e,f

Offsets can be specified directly (eg. int x @ a for x to be at the same offset as a), but now 'union' and 'struct' are used as better way of controlling layout: 

    record r2 =
        union
           int a
           int b
           struct
              byte f,g,h,i
           end
        end
    end

Here, the struct occupies 8 consecutive bytes (int is 64 bits).

Struct and union behave like anonymous structs and unions in C.

Records in M must always be declared as named user types like the above; not anyway like in C. There is no concept of a struct tag.

**Records as Classes** Records can contain other definitions such as types, named constants, and functions, which do not form part of any instance of the record. But I've done little with this, so I'm not sure exactly what is supported by M.

The concept is easy however, it can provide simple encapsulation.

**Pointers** -------------------

**Strings**


**Statements and Expressions are the Same** This is another concept from Algol68 - any expression can be used as a statement, and any statement can be used as an expression, and yield a value, although usually that would be 'void'.

(Name definitions are not classed as statements however.)

'Statements' that can usefully yield a value are: 'if' (long and short versions), 'switch' and 'case', although they will require an 'else' part.

**Functions and Procs** M likes to make a stronger distinction between functions that return a value, and those that don't. The latter are defined with 'proc'

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

**Function Parameters** M has default and keyword parameters:

    proc createwindow(int dimx=640, dimy=480, posx=0, posy=0, border=1) ...

    createwindow(dimx:1920, dimy:1080)

**Reference Parameters** As showed above, '&' means a reference parameter, allowing a callee to modify data in the caller:

    proc setlength1(int length) = {length^ := 123}
    proc setlength2(int &length) = {length := 123}
    ....
    int length
    setlength1(&length)
    setlength2(length)

setlength1 uses explicit & and ^ operator, setlength2 uses implicit operators. This also means it's not possible to pass a nil pointer.

**Nested Functons** Functions can be nested, but in a limited manner because a nested function can't access the stack-frame varkables of its enclosing functions. But it can still access static variable, named constants etc of those functions.

**Multiple Return Values** An experimental feature limited to 3 scalar return types:

   function fn => int, int, int =
      return (10, 20, 30)
   end

   (a,b,c) := fn3()

Return values can be ignored:

   (a,b) := fn3()   # discard last
   a := fn3()       # discard last two
   fn3()            # discard all


**Lengths and Bounds**

*Define Variables** This is fairly standard; inside a function (anywhere in the function actually):

  int a, b:=123, c

M syntax actually required 'var':

  var int a, b:=123, c

but I tried this for a few months, and it was a pain. So now 'var' is optional. Note that writing this, which needs 'var':

  var A, B

is not an error. Currently omitting a type makes A and B variants. I want to drop variants; in the next language version, omitting the type like this makes A and B have 'auto' type, which means it is inferred from initialisation or from the first assignment.

At the moment, variables are not automatically initialised to anything, like C.


**Readonly Variables** I've never been a fan of C's 'const' attribute, which really complicates the type system. M never had anything like until recently, when it was possible to use 'let' instead of 'var':

    let A:=100

Here, the initialisation is mandatory, as A can't be used as an lvalue like in an assignment. This provides some weak protection, but won't do much for more complex variables, such as arrays or pointers to data structures. Let is experimental.

(Also experimental are 'in', 'out' and 'inout' attributes for function parameters. 'out' vaguely corresponds to '&' used for reference parameters. I haven't played with this attributes yet, and I'm not sure whether an 'in' parameter should be equivalent to 'let'.)

**Named Constants** This is very simple feature, naming compile-time expressions:

   const A     = B+C
   const int B = 100
   const C     = 200

When no type is used, it is infered from the expression. (The example demonstrates out-of-order definitions.) Such constants can be exported using 'global const.

Use of A, B or C in source are synonyms for the constants 300, 100 and 200.

'const' is useful for numeric types, but less so for anything else.

**Enums** These are a little like C:

   enum (A, B, C=10, D)         # A=1, B=2, C=10, D=11

Or can also part of a type:

   type colours = enum (red, green, blue)
   type lights  = enum (red, amber, green)

Here, the names need to be qualified: you have to write colours.green (2), or lights.green (3). However the type system isn't that sophisticated, so the actual types are merely ints, and nothing stops you using colours.green or lights.green interchangeably.

Type-d enums are not used much, and actually, enums themselves are rare because I normally use the **tabledata** features next:

**Tabledata** This is an unusual feature that defined sets of enums, and parallel data arrays, at the same time:

    tabledata() []ichar colournames, []word colourvalues =
        (red,       $,      0xFF'00'00),
        (green,     $,      0x00'FF'00),
        (blue,      "Blue", 0x00'00'FF),
    end

This defines enums red=1, green=2, blue=3. And an array [1..3]ichar with the values ("red","green","Blue"). And an array [1..3]colourvalues with the given numbers.

The "$" is a device which returns the name of the last enum defined, so it saves having to duplicate each enum name, but you can just use a regular string.

In this form, entries can be added, deleted or moved very easily. Notice the trailing comma on the last entry to facilitate this.

The arrays can be zero-based (or some other value): use [0:] on the array declarations, and start with red=0. (But don't try override the other enum values, as the array mapping can't cope with that.

The () in tabledata() can contain a type name to contain the enums, as suggested above, eg:

    tabledata(colours) ....
