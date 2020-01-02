## M Language Docs

I've no idea about writing a proper language reference or even a tutorial. The following is just a random collection of topics. Since I don't really expect anyone else to want to use this language, or even learn something about it, this is just to refresh my own memory about what features there are and how they work. Or to give notes as to what might need attention.

### Overview

* M is a low level systems programming language first developed in early 80s to compile on and for 8-bit microprocessors.

* The non-brace syntax was inspired by Algol 68, Pascal and Ada.

* It's been through several generations since then and now targets x64 processors running Windows.

* It has also recently become a hybrid language, with a high-level 'variant' data type so that it can also be used for scripting.

* It can and has been used as a superior replacement for C.

### Current Aims

Although I don't really have a need for any language now, the project is being kept alive as a completely self-contained language implementation, one kept simple and easy to understand by anybody. It has these characteristics:

* Very fast whole-project compiler

* Written in itself and builds to a one self-contained executable (no other files needed)

* Small size (by PC standards) of around 700KB, including its standard libraries

* M compiler can be distributed as one .exe file, or one M source file. (To bootstrap from source code, requires an existing M compiler binary. See 'C Target' below.)

* The language is devoid of new-fangled language ideas of the last few decades. It is a more sophisticated version of the same 1980s language, designed to do a job of work. There are no features requiring degrees in computer science to understand.

### Self-Contained Compiler

This is true in that:

* The compiler is written in M

* All the libraries are written in M

* When built, the one executable includes the sources of the libraries, so that only the one file is needed to build a project. The compiler will direct turn the .m source files of a project into a .exe executable.

* The only dependencies for building the M compiler, compiling programs with it, or running those programs, are a Windows OS and the msvcrt.dll C library, which is present on every OS.

External dependencies are needed in these cases:

* When generating a DLL file rather than .exe. This is because M doesn't yet directly support the .dll files (like .exe but more complex with extra sections). It is necessary to generate a .obj file, then use something like gcc to produce the .dll.

### Targeting C Source Code

This has been supported at various times, to get M code to run on various combinations of Windows/Linux, 32/64 bits, x86/ARM, but has now been dropped completely. It was too much effort to keep supporting, and anyway a growing number of M features were not supported for a C target, or not capable of being supported.

Now I can write M source with no such worries. Except that:

* M can only target Windows/x64/64 bits

* I can't pass M's C output through an optimising C compiler (M does not optimise) to get extra performance

* No one else can build the M compiler from source (it would have been a C version) with a C compiler; they will need the M compiler binary, which will have AV issues.

### How the M Compiler Works

M is a whole-project compiler. It will always compile all the source modules of a project at the same time, and produce a 'program': a single executable file, or sometimes a shared library file (.exe or .dll). If targetting C source, it will produce a single C source file for the entire program.

Any other parts of an application, such as external libraries, are not considered part of the program unit. M can only use such libraries in the form of DLL files.

### Modules, Imports and Namespaces

M uses a module import system:

* Define anything (function, variable, type, record, enum, named constant, table) with **global** in front, and it will be exported from this module. No need to have another declaration in a header:
````
    global function dist(real x,y)real=
        return sqrt(sqr x + sqr y)
    end
````
* To import such names defined in module A and exported from there, use:
````
    import A
    ....
    print A.dist(a,b)
    print dist(a,b)       # alternative when there is no ambiguity
````
So, modules create namespaces, although the use of a qualifier such as "A." is optional when there is only one definition visible from this scope.

(Namespaces are also created by records - non-instance names - and functions, but that's an experimentatl feature is little used so won't be mentioned further.)

### Circular Imports

While imported modules normally form a hierarchical tree, M doesn't make any such restrictions: circular and mutual imports are possible; in A.m:
````
    import B
````
In B.m:
````
    import A
````
However, circular definitions are not allowed, for example:
````
       const a = b
       const b = a     # either in same module, or anywhere else
````

### Identifiers

Identifier names consist of A-Z, a-z, 0-9 and _ and $ (can't start with a digit).

Identifiers are case-insensitive throughout M source code, so abc, Abc and ABC (as well as abC, aBc, aBC, AbC and ABc) are all equivalent.

For case-sensitive imported names from external libraries, the name must be specifed as a string with the correct case, but then can be referred to in any desired style.

(There is no solution yet for when two different imported names can clash when case is ignored.)

### Syntax

This is best learned by looking at example code.

M uses a syntax loosely based on Algol68, but which I think has evolved to be an improvement on that language, which these days looks dreadful.

M doesn't use {,} or begin-end for compound statements. Sequences of statements are delimited by keywords such as **then** ... **fi** or **then** ... **else** or **do** ... **od**. This will be apparent from the examples.

M offers a choice of block endings, for example:
 
    if ... then ... fi
    if ... then ... endif
    if ... then ... end fi
    while ... do ... od
    while ... do ... endwhile
    while ... do ... end while

Although it is suggested a consistent style is used.

### Character Set

Input files can be 8-bit ASCII, or UTF.

Most program code outside of comments and quoted strings, is expected to have characters from 32 to 126, plus newlines (using cr,lf or just lf) and tabs.

Comments and quoted strings can have UTF8 sequences.

String constants here are 8-bit, and can contain unprocessed UTF8 sequences. There is the L"..." reserved for Unicode, but it doesn't do anything with that yet. When it does, then any UTF8 sequences will be expanded to a 16-bit character codes (since M works on Windows which uses 16-bit wide characters).

### Comments

Only line comments are implemented. They are introduced by "!" or "'#", and continue to the end of the line.

Comments can be nested (for example, commenting out a block of lines some of which are already comments)

Multi-line comments are not supported, other than using multiple line comments, nor are intraline comments (ie. a comment in the middle of a line). Block-commenting is considered to be an editor function.

### Semicolons and Newlines

Newlines are usually interpreted as semicolons, unless the last symbol on the line was one of "(", "\[" or ",".

This generally works well, but may need tweaking in the compiler (by making it tolerant of extra semicolons) if a style like:

    if cond
    then

is preferred over:

   if cond then

Otherwise, the "\\" line continuation character needs to be used to continue one line into the next.

### Type Declarations

These follow Algol68 style and are written strictly left to right using these basic elements:

* **ref** means pointer to
* **\[\]** or **\[n\]** means array of
* *type* is a built-in type name or a user-type name created with **record** or **type**

So, a pointer to array 10 or array 20 of int might be:

    ref [10,20]int

### Out of Order Definitions

Most things can be defined in any order in a module, including functions, variables and types.

It is even possible to declare all local function variables at the end of the function. They don't need to be in an execution path.

In fact, it is possible to refer to names which are defined not only anywhere in this file, but in any module (provided they have been exported from there so are visible).

No forward declarations of any kind are needed, except import statements which must always be first thing in the module, mixed only with blank lines and comments.

### Scopes

Within a function, there is just one scope for all variables, parameters, types, constants, labels defined within the function.

(This is in contrast to some other languages which introduce new scopes at each block. And with C, where struct and enum tags additionally have their own namespace, as do labels.)

### Entry Point

The usual entry point to an M program is the 'start' proc which takes no parameters. This will always be global.

This is a special name, and the compiler will add a call to an initialisation routine just inside start(), which accesses the command line parameters and sets up globals nsysparams and sysparams\[\].

(The function name 'main', always global, is also accepted by M's linker, but this do the expected setup. This is useful for minimal programs that don't include M's runtime modules, when using the -nosys option.)

### Include, Strinclude and Bininclude

The **include** directive is nothing to do with modules or headers, but is sometimes used to incorporate source code from another file. (When that code has been generated by a program for example.)

    include "file"                # include code from 'file.m' (default extension)
    include "file."               # include code from 'file'

**strinclude** is a little different: it takes any text file, and turns it into a string constant within the M source code. Any control characters are turned into string escapes:

    ichar s = strinclude "stdio.h"

(This is used within my C compiler to incorporate all the standard header files into the C compiler executable.

At present, strinclude doesn't work with text that has embedded zeros. For binary files, there is 'bininclude', but that is rather inefficient, and is used to initialise a byte-array. M's new variant type has a string type that can contain binary data, so it is hoped that can be adapted to work with strinclude.)

### Data Types

These include:

* **integers**
* **floating point**
* **pointers**
* **fixed-length arrays**
* **records** 
* **slices**

The above are 'packed' types, flat types of a width or length known at compile-time.

Records are collections of any packed types, including other records. But they also go beyond what is possible in a C struct by allowing named constants, types and functions to be defined inside them.

Arrays are sequences of the same packed type, including records and other arrays. Arrays can be defined with a fixed length only, or unbounded, but only as a pointer target (eg. for passing to functions).

Then there are:

* **variants**

These have been added from my separate scripting language. They have a dynamic, tagged type and can deal with higher-level, more flexible data.  More on variants below.

So M's types are split between low-level, fixed-length, packed/flat types, and variants. A bit of an odd mix, but I'll see how it goes.

### Numeric Data Types

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
    
    real32   r32
    real64   r64
    real128  r128

Aliases:

    int    =  int64
    word   =  word64
    real   =  real64
    byte   =  word8
    char   =  word8

Machine alias that depend on whether a target is 32 or 64 bits:

    intm   =  int32 or int64
    wordm  =  word32 or word64


### Pointer Types

Pointer types start with "ref", and be in one of these forms:

    ref void        # generic pointer type (can point to object or function)
    ref proc        # procs and functions are not types, but it is possible
    ref function    #   to have pointers to them
    ref T           # T can be any type, including other pointers

The type ref char (really, ref byte or ref word8), is special in M. The type of a string constant will be ref char. And printing of a ref char or ref byte value will assume it is a string.

This type also has the alias 'ichar'.

### Numeric Constants

Integer constants are written in the usual decimal and hex formats, the latter using 0xddd style like C.

But other bases from 2 to 16 are allowed:

    2x1011    # binary (11)
    4x1231    # base 4
    8x777     # octal (511)
    12xBBB    # base 12 (1727)
 
Separators are allowed within numbers, which can be chosen from \' \` and \_. There are no restrictions on how these can be mixed and used; just use common sense.

There are a small number of scale factors that can follow a constant:

    4 million        # 4'000'000
    5 thousand       # 5'000
    6 billion        # 6'000'000

There used to be other scale factors, some of which worked with floating point, such as 5 mm (5.0) or 6.43 m (6430.0 when mm is the default length unit), or 90 deg (pi/2 radians) but I'm not sure whether to reinstate these, or to allow user-defined scale factors.

These named factors use their own namespace so do not impinge on variable names.

### Types of Numeric Constants

In the case of integers, the following are the types of an integer constant, which depend on value:
````
    int64                 # Up 2**63-1
    word64                # Up to 2**64-1
    int128                # Up to 2**127-1
    word128               # Up to 2**128-1
    decimal               # 2**128 or over (decimal is a variant type only)
````

There are no suffixes to control the type of a constant, except for -L. To modify the the type, use a cast:
````
    int16(0)
    int128(0)
    0L                    # L means decimal type
````

Real constants at the moment will be r64, which has to be changed with a cast (that is usually automatic if values needs to be r32 in any context). Too large, too small or too precise real constants will overflow/underflow with no error, just wrong values. However, by using a -L suffix, then such constants can be formed into a decimal constant:
```
    123.456e+1000000         # yields incorrect result
    123.456e+1000000L        # yields decimal constant
```
It's just that M doesn't automatically detect that an -L is needed; only for integers.

The range of a word64 constant is quite narrow: numbers that are too large to fit into i64, but you don't want to jump straight into the inefficient i128 type with poor support.

Note that all numeric constants are always positive. You can get a negative value by using a minus signe: "-5612". Or by using a cast: int8(255) yields -128.

### Numeric Limits and Type Sizes

Given any type T or expression X, then:

    T.minvalue
    T.maxvalue
    X.minvalue
    X.maxvalue

will yield the smallest and largest values possible. (I'm thinking of allowing also .valuerange which returns a Range consruct, so that byte.valuerange returns 0..255.)

For a type or expression size, use:

    T.bytes
    X.bytes

For array lengths and bounds, there is a whole set of properties that can be extracted; see section on Lengths and Bounds.

(Doesn't work on decimal, but the limits there are very wide, approx +/- 10^(+/- 2000'000'000))

### Character Constants

M uses ASCII. So 'A' is equivalent to the number 65.

Multiple characters like 'ABC' will return 0x434241, ordered so that it's the same pattern in memory as the string "ABC" would make. (This is for little-endian machines, which I believe the main targets of M use: x86 and ARM, if it ever works on that.)

Character constants can be up to 128 bits wide; here constants are all 64 bits exect the last:
```
    'A'
    'ABCD'
    'ABCDEFGH'
    'ABCDEFGHIJKLMNOP'      # 128 bits
```

### User Types

Records have their own syntax. So user types are mainly for aliases, or to define enums that belong to a type:

    type intptr = ref int
    type colours = enum(red,green,blue)

Here, those enums have to be written as colours.red, colour.green etc.

The type system isn't that sophisticated. You can't do this:

    colours col = red

You have to keep it simple:

    int col = colours.red.

### Function Pointers

A function by itself is not a type, but it is possible to create a pointer to a function:

    ref function (int,int)real           # pointer to function taking 2 ints, returns real
    ref proc                             # pointer to proc taking no parameters

### typeof()

typeof() can be applied to a variable or expression, and the result can be used as though it was a type.

### Address-of

This uses the "&" operator like C.

& and ^ (for pointer dereference), or ^ and &, will always cancel each other out, provided they are applied one after the other. So they won't do here: &A^[I].

### Named Constants

A very simple concept where a particular constant, of any integer, float or string type, is given a name. Then the name is used in place of the constant.

Named constants can't be assigned to, they can't have their address taken, but they can be used in other const expressions, as array bounds, or as switch values (if they are ints anyway).

### Enums

These are just ways of defining sequences named constants which are linked in some way. Example:

    enum (red, green, blue)

Numbering starts from 1, so this is equivalent to:

    const red = 1
    const green = 2
    const blue = 3

But it's more convenient when adding, deleting or moving enums. The numbering need not be sequential:

    enum (a=0, b, c, d=100, e)

So a=0, b=1, c=2, d=100 and e=101. 

Note that no special scope is created for enums; because 'red' is used above, it can't appear in another enum in the same scope. To create a scope for enum, wrap it in a user-type. See User Types.

For more advanced ways of creating enums, see Table Data.

### Strings

String are enclosed in double quotes, like "abc", and allow the usual set of escape codes (these are case-insensitive):

    \n     10      newline char for Linux
    \l     10      linefeed
    \c     13      carriage return
    \r     13
    \t     9       tab
    \v     11      vertical tab
    \f     12      formfeed
    \w     13,10   cr followed by lf
    \a     7       bell or alert
    \b     8       backspace
    \y     16      (used as backwards tab in one of my libs)
    \"     "       Embedded double quotes
    \q     "       Embedded double quotes
    \'     '       Embedded single quote
    \xdd   dd      Two-digit hex code
    \udddd dddd    Four-digit hex code (reserved for L-strings)

There are some special string prefixes:

    f"..."          Raw string (no escape codes recognised)
    r"..."          Alternative raw string
    a"..."          Array string
    z"..."          Array string, zero-terminated
    L"..."          To be used for wide-character strings

An array string is one where the characters map to char-array sequences. They are needed here:

    []char s = A"abc"
    []char t = ('a','b','c')        # alternative without using A"..."

as otherwise the type of "abc" will not match the \[\]char type. (This minor detail is ignored in C.)

Successive string constants, such as "abc" "def", are combined into one string "abcdef". At present, it's not possible to combine raw strings with array strings.

### Variables

Nothing much to say about this. Declarations are in Algol style:

    int a,b:=10,c

declares three ints, of which b is initialised.

All variables are mutable, and this can be indicated like this:

    mut int a,b,c

But usually I never bother with 'mut'; it is just for completeness.

There are 'let' that can be used in place of 'mut':

    let int a:=123, b:=456           # initialisation is required

These are intended as read-only values, so cannot be reassigned, but M doesn't really deal with that vigorously: it complains about a let variable on the left of an assignment, or taking its address, but that's about it.
   


### Static Variables

Module-level variables are always static (reside at a fixed location in bss or data segments) rather then be on the stack (or on the heap). A 'static' prefix would be an error.

Inside a function, 'static' is needed for variables that are either initialised to zeros, or set once to compile-time data, so they will retains their values on re-entry.

Initialising a local variable with "=" rather than ":=" will make it static.

('static' will also be used inside records, for variables belong to the type rather than be a normal field.)

### Initialising Variables

This is done with either "=" or ":=":

Use "=" for module-level variables, or static variables in functions. This initial value must be determined at compile time (or load time if addresses are involved) and can be overwritten as the program runs.

Use ":=" for variables inside functions which will be re-initialised on each re-entry.

Initialisation data for structs and arrays is limited when ":=" is used (unlike C).

While variables can be defined anywhere in a function scope, initialised ones should be declared at the top of the function. This is because the initialisation is always done directly on entry, not at the point where they are declared, so declaring them elsewhere would be misleading.

### Pointers

Pointers work as they do in C, except they are nothing to do with arrays and cannot be indexed:

    ref int p       # declare a pointer to int
    int q

    p := &a         # & is used to take the address of an object
    print p^        # ^ is used to dereference
    print (p+i)^    # offsets must be done like this; i is scaled

If A is a pointer to array, R is a pointer to a record, and F is a pointer to a function, then dereferencing is consistently done like this:

    A^[i]
    R^.m
    F^(x)

### Arrays

These refer mainly to flat arrays of fixed, compile-time bounds:

    [10]int A                    # 10-element array, 40 bytes
    []int B = (10,20,30)         # 3-element array (defined by init data); 12 bytes
    [10,20]int C                 # 10x20 2D array, 800 bytes
    array[30]int D               # 'array' optional unless ambiguous

The default lower-bound is 1, but it can be anything.

A simple way of creating a dynamic array is as a pointer to array (NOT a pointer to the element type as in C):

    ref[]int P
    int N = 100
    P := malloc(N*int.bytes)

Then this can be indexed as P^\[i\], with bounds of 1..N. However, slices are a better way of doing this.

Arrays are treated as value types throughout, however support for manipulating value arrays is limited (restricted to assignment, or passing arrays of 1, 2, 4, 8, or 16 bytes).

(Old versions of M implemented pass-by-value of arrays and records of any size. But my recollection is that that feature was never used, over over decades. But value arrays play a important part in keeping the type system consistent. See what happens in C when you have the discontinuity caused when value arrays are written out.)

(An 'array' prefix might be needed inside a function, as "\[" can be denote the start of an set constructor).

### Array Bounds

    []int A = (10,20,30)    # Unbounded array whose length is set by the data
    ref []int A             # Unbounded array which is a pointer target
    [5]int A                # Array of length 5

So far these arrays have used the default lower bound of 1. Other bounds are possible:

    [0:]int A = (10,20,30)  # Bounds are 0..2, length is 3
    ref [12:]int A          # pointer to array indexed from 12, length unbounded
    [6..10]int A            # Bounds are 6 to 10 inclusive, length 5
    [6:10]int A             # Bound are 6 to 15 inclusive, length 10

The general pattern, with length optional, is:

    [length]
    [lower : length]
    [lower .. upper]

When lower is not specified, it will be 1. When length is omitted, it will be unbounded, or determined from init data.

For multi-dimensions, use:

    [10,20]int A
    [,20]int A              # First unbounded

### Multi-dimensional arrays

True multi-dimensional arrays, where all the data is in represented by a single contiguous block of memory, requires that all dimensions are fixed at compile-time:

    [4,5,6]int A

A has 4x5x6 or 120 elements, of 4 bytes each so some 480 bytes in total.

Passsing multi-dim arrays to functions must also use the the same fixed sizes, except for the first. The first or only dimension of an array can be unbounded to allow for pointers for different-sized arrays:

    proc F(ref[4,5,6]int A)
    proc G(ref[,5,6]int A)

The 5 and 6 must be fixed as they are needed for the compiler to work out how to do index calculations inside the function. The first dimensions is not needed for that.

Passing rectangular or cubic arrays of variant sizes is not possible. (It is now possible in C, but using some advanced feature involving variale types, something I don't even have in my dynamic Q language.)

If such arrays are truly dynamic, then it might be better to implement them as pointers, which also allows different lengths of each row:

    proc H(ref[]ref[]ref[] A)

Although A now needs indexing as A^\[i\]^\[j\]^\[k\] instead as A^\[i,j,k\]. It is however clear what is happening and exactly how such an array is passed.

### Slices and Views

A slice is basically a 2-element type consisting of (pointer, length). The pointer is to a single element.

This can be used to set up a *view* into an existing array, or part of an array, including substrings. The slice can then be passed to functions (instead of separate pointer and length), and it can used as a lightweight way to do counted strings.

Slices are at present experimental.

### Defining Records

Basic syntax:

    record date =
        int day
        int month
        int year
    end

Then use like this:

    date d,e,f

Offsets can specified directly:

    record r =
        int a1, a2, a3
        date d
        int x @ a2
        int y @ a3
        int z @ 0
     end

But a better way of organising fields are by using **struct** and **union**:

    record r2 =
        union
           int a
           int b
           struct
              byte f,g,h,i
           end
        end
    end

Here, the struct occupies 4 consecutive bytes. But the fields a, b, and the struct all occupy the same 4 bytes. The total record size if 4 bytes.

(Struct/union behave like anonymous structs and unions in C.)

Records can only be defined with a **record** statement. Structs as they are in C, which can be defined anywhere, even at the same time as defining variables of that struct, are not possiible. Records will always have a name that is the name of the type.

### Records and Classes

M has no OOP features, however it does have very lightweight classes, although so lightweight (and so little used so far), then little more will be said about them. Except:

* You can use the keyword **class** as well as **record**

* Records can include, as well as regular fields, elements that do not form part of any instance of the record. That includes types, further records, named constants, and actual functions

### Functions and Procs

A function is a subroutine that always returns a value, and a proc is one that never does.

That distinction has been partly lost in some languages (using a 'void' return value in place of a 'proc'), but I have found it useful to emphasis that.

They are defined like this:

    function sumint (int a,b)int =
        return a+b
    end

    proc iaddto (ref int a, int b)=
        a^ +:=b
    end

Other variations on specifying a function header:

    function getdow:int =               # when no parameters
    function getdow()int =              # alternate
    function getdow=>int =              # alternate
    function sumint(int a,b)=>int =     # alternate
    function sumint(int a,b)typeof(a)=  # ensure return type is same as param

M allows optional paramters with default values, keyword and reference parameters:

    function tostring(int a, base=10, width=0)ref char =

This can be called as:

    tostring(a)
    tostring(a,10)
    tostring(base:16, a:10, width:12)

As a bit of fun, M also allows C-style braces around function bodies:

    global function sumint(int a,b)int= {return a+b}

For reference params, the iaddto proc can be written like this:

    proc iaddto (int &a, b) =
        a +:= b
    end

It makes it tidier, and makes it harder to pass a nil or illegal pointer. It can also be called as iaddto(a,b) instead of iaddto(&a,b). However, some transparency is lost.

Functions and procs are not types, but a particular function header can be the target of a function pointer, which is a type. See Pointer Types.

### Nested Functions and Procs

Functions can be nested, but in a limited manner because a nested function can't access the stack frame variables and parameters of its containing function.

But it can access named constants, types, enums, records, and static variables of the containing function, and it can be kept local to that function rathern than reside somewhere else in the file. So they can still be worth having.

### Multiple Return Values

This is an experimental feature where a function has several discrete return values, rather than one multi-valued type.

If there are three return values, then they can be assigned to up to three variables:

    a, b, c = fn()    # assign all
    a, b = fn ()      # assign first two and discard last
    a = fn()          # discard last two
    fn()              # discard all

(A multi-return-valued function can't be used in a multiple assignment like this:

    a, b, c = fn(), y, z

fn() must either return only a single value, or it must supply all the values on the right-hand-side.)

Such a function is defined like this:

    function getminmax(ref[]int a, int n)int,int =
        ...
        return x,y
    end

### Lengths and Bounds

The following can be applied to arrays and slices:

    .len    # length of array or slice
    .lwb    # lower bound
    .upb    # upper bound
    .bounds    # returns a range lower..upper (compile-time only)

The following for any type:

    .bytes    # byte-size in any type or expr

And this for primitive types:

    .bitwidth

### Operators and Precedence Levels


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

    +       -   (unary) plus
    -       -   (unary) negate
    inot    -   bitwise not
    abs     -   absolute value
    
    ++      -   increment (used as prefix or postfix)
    --      -   decrement (used as prefix or postfix)

    ..      5   Make range

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
    atan2

Note that many of these are functions in other languages, but are operators here. That means parenthese are not needed (but usually advised), but also they are properly overloaded.

For example, **abs** can be applied to ints or reals, and will give the expected answer (try using abs() in C on a float rather than int):

    int a; real x
    a := abs a
    x := abs y

This particular op can also be used like this, for those example:

    abs:= a
    abs:= x

### Precedence Levels

    2   **                             # highest
    3   * / rem << >>
    4   + - iand ior ixor
    5   ..
    6   = <> < <= >= > not notin
    7   and
    8   or
    9   :=                             # lowest

(Don't ask why they start at 2 and are in the wrong order. That will be fixed in a later edition...)

Disregarding "\*\*" and ".." which exist in C, there are seven levels, compared to a dozen in C.

Unary ops don't have precedence. Evaluation goes like this:

      op1 op2 X op3 op4
      
If op1 to op4 are unary operators, then they are applied in the order op3, op4, op2, op1. So inside out

This sounds peculiar, but apparently is the same in other languages. In practice it will be intuitive.

### Array Indexing

This is very simple. If A is an array, then you can index it as:

    A[i]

If A is a pointer to array, it must be dereferenced first:

    A^[i]

Where there are mult-dimensions of a flat array (no pointers to arrays inside), then the indexing goes like this:

    A[i,j,k]
    A[i][j][k]          # C style alternative

When slicing gets added, then a slice can be created using:

    A[i..j]

(This results in a slice that can be indexed like an array, so A\[i..j\]\[k\], which can be written as: A\[i..j,k\]. Or it can be further sliced: A\[i..j, k..l\])

As a convenience, the special symbol $ used in an array index, returns the upper bound of that array. So A\[$\] is the last value, equivalent to A\[A.upb\].

### Pointer Dereferencing

This is done with the "^" symbols, applying as as suffix to a term:

    P^
    (P+i)^

And can be used consistently with multiple defers:

    Q^^
    R^.S^.T

Each ^ can be cancelled by one &, so that &Q^^ is equal to Q, and &&Q^ equals &Q.

When mixing ^ with ++ and --, it needs to be used like this:

    P++^        # Increment P then deref the original value
    ++P^        # Increment P then deref the new value


### Field Selection

Little to say about this, except it used the usual "." notation:

    pt.x + pt.y

When used with a pointer to a record P, it's like this: P^.x.

Note that "." is also used for selecting names from a namespace. A namespace might be a module, function or record. That use is detected and resolved at compile time:

    a.b.c

It's not possible to tell, until resolved, whether both dots select fields, or just the second, or neither.

        
### 2-way and N-way Selection Operators

The 2-way operator This is the equivalent of  C's ?: operator, and is written like this:

    (a | b | c)

always with parentheses. It's equivalent to if a then b else c fi

An extension of that is the N-way operator:

    (n | a, b, c, ... | z)

This select's the nth (1-based) element from the list, or the default z when n is out of range. Only one element is evaluated.

Both operators can be used on the left size of an assignment:

    (cond | a | b) := 0

Here, either a or b is set to zero. Some operations will also propagate inside, so that:

    &(a | b | c)

is the same as:

    (a | &b | &c)

### Bit Indexing and Slicing Operators

Int types can have their bits accessed as follows:

    int A
    A.[i]          # bit i (0 is lsb, 63 is msb)
    A.[i..j]       # bitfield i..j
 
These can be used as lvalues, but in a restricted form (otherwise arbitrary bitfield pointers are needed).

### Min/Max and Clamp

min/max are built-in operators, and can be used like this:

    c := min(a,b)
    a max:=b

The will work on numeric types.

Clamp is used as follows:

    c:=clamp(x,a,b)

and is equivalent to c:=min(max(x,a),b)

### Assignment and Equality

Assignment always uses ":=" for assignment, and "=" for equality. This reduces the potential for mistakes as is common when "=" and "==" are used.

Assignments can of course be used inside an expressions which is how the mixup arises.

### Augmented Assignments

These are assigments such:

    a +:= b
 
which means a := a+b. In M, these are only allowed as standalone statements, as their use inside expressions is confusing.

Such assignments are allowed for +, -, \*, /, iand, ior, ixor, min and max.

Unusually, M allows augmented assignments for some unary operators too:

    -:= a          # a := -a
    abs:= a        # a := abs a
    inot:= a       # a := inot a
 
### Multiple Assignment

This is similar to what is used in Python:

    a, b, c = b, c, a
    a, b = 10, 20
    a, b = b, a

The last will do the swap operation, but not as efficiently with complex terms as each is evaluated twice.

This can also be used when the right-hand-size is a function returning multiple values:

    a, b = fn()

### Expression List

This is the equivalent of C's comma-operator:

    a + (x; y; z)

but is written with ";", and with mandatory parentheses. x and y are evaluated first (usually they will have some side-effects), then z, which is the result of the expression list.

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

Any two fixed size compatible objects can have their values can be exchanged like this:

   swap(a, b)

### Promotions

In C, small integer values are widened to int, before use in calculations.

In M, that doesn't happen. 8-bit operands are evaluated with 8 bits. Mixed-size will widen one operand to match the other, so that 8+16 bits is done as 16 bits.

However, integer constants will always be 64 bits.

(**Note:** I'm thinking of changing this so that all integer operations are performed as 64-bits. Then there will be less unexpected behaviour. And it matches the Q language which does the same. At present:

    byte a=255, b = 1

    println a+b       # displays 0, as the 8-bit result wraps
    println a+1       # displays 256, as 8 is promoted to the 64-width of '1'

The current behaviour is desirable in 8 and 16-bit processors, but M is now targeted at modern 64-bit ones.)

### Mixed Sign Arithmetic

In M, this will be done in signed mode: the unsigned operand is converted to signed. The rules here (combined with promotion) are simple compared with C:

    Unsigned + Unsigned   => Unsigned
    Unsigned + Signed     => Signed
    Signed   + Unsigned   => Signed
    Signed   + Signed     => Signed

There are no exceptions, and they do not depend on the width of the types.

### Type Conversion and Punning

Type conversions are written like this:

    int(x)

although it has to be a conversion that is allowed. Usually between integers, floats and pointers.

The type involved needs to be simple (not an array type for example).

There is also type punning, which is a re-interpretation of a type without changing anything:

    real x := 1.1

    println int64(x)       # display 1
    println int64@(x)      # display 4607632778762754458 (0x3FF199999999999A)

The @ symbol makes it type punning (equivalent to \*(int64_t\*)&x in C, however that will only work with lvalues in C).

Sometimes, it can be difficult to get on top of which precise conversion is needed, as happens with pointer types. Then, 'cast' can be used to automate it:

    ref int64 p
    ref []int q

    p := cast(q)

'cast' will apply whatever cast is required. This is handy when the necessary type needs to be tracked down, or when it is likely to change.

### Conditional Statements

Well, this is mainly the **if** statement:

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
    elsif 
    elsecase x
    when a then
    when b,c then
    else
    fi
 
But use sparingly as it looks funny.
 

### Conditional Suffixes

Some control-flow statements can have a conditional suffix. For example:

    goto finish when x=0
    return 3 if n<0

The possible condition keywords can be:

    when expr
    if expr
    unless expr          # this one has the opposite logic

The statements where such a suffix is allowed are:

    return
    goto
    stop
    exit
    redo
    restart
    next

### Loops

Modern languages seem to be lacking in looping constructs even though, as mere syntax, they have little cost. M offers:

    do ... od                   # endless loop
    to n do ... od             # repeat n times
    for i:=a to b do ... od    # iterate from a to b
    forall x in a do ... od    # iterate over values in a (experimental)
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

This iterates the loop variable of over a to b inclusive, stepping by c. 'when d' can be used to conditionally execute any particular iteration.

The 'else' part executes on normal termination. (Note there is a bug here at present: the else part is only executed after at least one iteration of the loop body.)

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

    for i in a..b do
    
where a range construct is used, which has the advantage of being able to write this:

    for i in x.bounds do
    
(There is also an experimental version called forall, which iterates over values.)

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

### Table Data

Table data is way of declared linked, parallel arrays of data, usually with enums declared in the first column. Example:

    tabledata() []ichar colournames, []word colourvalues =
        (red,       $,      0xFF'00'00),
        (green,     $,      0x00'FF'00),
        (blue,      "Blue", 0x00'00'FF),
    end

This defines enums red=1, green=2, blue=3. And an array [1..3]ichar with the values ("red","green","Blue"). And an array [1..3]colourvalues with the given numbers.

The "$" is a device which returns the name of the last enum defined, so it saves having to duplicate each enum name, but you can just use a regular string.

In this form, entries can be added, deleted or moved very easily. Notice the trailing comma on the last entry to facilitate this.

The arrays can be zero based (or some other value): use [0:] on the array declarations, and start with red=0. (But don't try override the other enum values, as the array mapping can't cope with that.

The () in tabledata() can contain a type name to contain the enums, eg:

    tabledata(colours) ....
        (red, ...

which means you have to use colours.red etc. But I've never actually done this, so don't know if it still works.

**tabledata** can also be used without():

    tabledata []ichar table 1, []int table2 =
        ("abc",         100),
        ("def",         200),
    end

Here, no enums are declared; it just declares and initialises any numbers of parallel arrays of different types.

### Print and Println

M still retains print and println as statements:

    println a,b,c

This displays a, b and c, whatever type they happen to be, then adds a newline.
**print** doesn't add the newline.

To print to a file open as handle f, use:

    println @f, a,b,c

The values are written with spaces in between. To suppose the space, use a double comma:

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

The default print format for basic types is:

    int, word    Decimal
    real         Decimal floating point
    ref byte     As zero-terminated string
    ref T        Any other pointer, as a hex address
    record       Not allowed (some versions just printed out all the fields)
    array        Not allowed

(Note: the special treatment of 'ref byte' as a string is causing some problems. I may remove that, but that will then need a format control to display a string rather than pointer address.)

### Read and Readln

Read and Readln are two more statements that I have neglected to implement (but are part of Q, and had always been part of previous versions of M).

They are used as follows:

    int a,b
    real c

    readln a,b,c

or:

    readln
    read a,b,c

**readln** reads the next whole line into a buffer, from the console (or from
a file using readln @f). Then read statements consume inputs from that buffer.

That's why they can be part of the readln, or follow later.


### Range Construct

A range is a pair of ints defining a sequence, such as 1..10, which means to 1 to 10 inclusive, and always stepping by 1.

It's not a type in itself (as it is in the Q language), but it can be used as a construct:

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

Like a range, a set is not a proper type as it is the Q language, one that can be of any size.

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

    [128]byte s
    int16 @ &s[2]

This can also be used to have different names for the same variable.

### Label Pointers

This is a possible feature (also implemented in C by gcc), where you can take the address of a label, store it in arrays and so on, and use that later in an indirect goto.

But at the moment is't not implemented. (For some kinds of programs, it results in faster execution than using a switch.)

### Doc Strings

Another possible feature, that is already used in the Q language.

Doc-strings would take the form of #-comments directly following a function header. A compile option will write a file of function headers together with their doc-strings.

### Function Tables

There is no longer metadata that can be specified per-function (and that can be picked up by a program).

However, each M program stores a list of all function names and their addresses, in a way that can be accessed in a program by using special functions.

Although no type or parameter info, and no metadata, can be retrieved, sometimes it is possible to encode information in the name of a function.

I use this method in an interpreter where certain handlers start with "j_", and contain extra info in the name:

    n:=m$getnprocs()

    for i to n do
        name:=m$getprocname(i)
	    if name^='j' and (name+1)^='_' then
            initjhandler(name,m$getprocaddr(i))
        fi
    od

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

* **mlang**
* **clang**
* **windows**

**windows** is not a language, so there it is to show the Windows call convention is used
rather then normal C.

(On Win64, both Windows and C use the same call convention. M uses its own.)

Where a foreign function has a case-senstive name, it must be in quotes:

    import windowsdlls =
        windows function "MessageBoxA" (int=0, ichar message, caption="Title", int=0)int
    end

But then it can be used like this:

    messageboxa(message:"hello")

This also demonstrates adding optional parameters with default values, and  using keyword parameters.

(Q has an option to supply an alias to function, such as:

    function "MesssageBoxA" as "messagebox" ...

but it's missing from M at the moment. You can try using macros.)

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
    $targetbits     32 or 64
    $targetcode     One of X32, X64, C32, C64

### Standard Libraries

* **msys**    Support library implicitly imported in every module
* **mlib**    Small runtime library
* **clib**    Interface to some C library functions
* **oslib**    Selection of functions implemented by the OS
* **osdll**    DLL function handling when names/params not known until runtime

Note that many of these are mapped to a different actual module:

    msys maps to msys.m or msysc.c depending on x64 or c32/c64 target
    oslib maps to oswindows, oslinux, or osnos depending on OS target
    osdll on oswin64dll, oslin32dll, oslin64dll, or osnosdll depending on OS target and target size

### System Constants

There are a small number of predefined constants (some have been there in the distant past so are included for nostalgic reasons):

    pi
    halfpi
    twopi
    radian    # 180/pi, number of degrees in a radian
    nil
    infinity

### Standalone expressions

Expressions such as:

    a
    a+b
    a=b
    f()+1

are not allowed. They need to be written with an **eval** prefix otherwise they are assumed to be errors. The following are fine:

    ++a
    a:=b
    f()

### Inline Assembly

The M language has always had inline assembly available in a straightforward manner:

    assem                   # block of assem instructions
        mov D0,[a]
        add D0,4
    end

    asm inc dword [b]       # one instruction at a time

Since M's register usage is unsophisticated, interaction with M is simple.

Note that when targetting C, inline assembly is not available.

### Using the M Compiler

Given a program consisting of multiple modules, of which the lead module (containing the start() function) is prog.m, and the M compiler is called mm.exe then use:

    mm prog

assuming mm is in a suitable search path.

This will compile prog.m and **all** linked imported files, into prog.exe.

mm.exe is a single files that doesn't need other support files, and can simply be copied to wherever the source code happen to be. 

Other options for mm include:

    -c              Generate asm only
    -exe            Generate exe (default)
    -obj            Generate obj file rather than exe
    -run            Run the resulting executable

    -c32            Generate C source file 32-bit target
    -c64            Generate C for 64-bit target
    -x64            Generate asm for x64 target (default)

    -windows        Include libraries (for 'oslib' and 'osdll') for Windows
    -linux          Include libraries for Linux
    -nos            Include libraries that woth on other (will miss some functionality)

    -out:file       Name output file

### Compilation Speed

M compiles .m code to .asm at some hundreds of thousands of lines per second usually, on an ordinary PC. Assembling to .exe is an extra step, perhaps adding 50% to overall compile time.

Being a whole-project compiler, it *has* to be quick. Compiling itself (.m files to .exe) takes about 0.25 seconds on my rather slow PC.

That pretty good (not much longer than takes to press and release a key on the keyboard).

This is for about 37K lines of code, which includes an integrated assembler and linker.

Compilation involves compiling to actual ASM source code internally, then re-processing and re-parsing that, which is an inefficient way of doing things, but highly convenient. At some point that big intermediate step will be removed, but for now, it makes things much simpler.

### Conditional Imports

    $windows import abc
    $linux   import def

The $windows line is only processed for a Windows target, and the $linux line for a Linux target.

### Grouped Imports

Sometimes there are of modules A, B, C which work together to provide some functionality, but you need to import all of them. It is tider to put those imports into a new module X:

    import A
    import B
    import C

Then just do this in your program:

    import* X

This will make all the modules imported by X, also known to this program.

X didn't be an an empty module; it can have useful content. So if A already imports B and C, you can do this:

    import* A

### Naked Functions

What are sometimes called naked functions, are those without no entry or exit code.

In M, they are written like this:

    threadedproc fn =
     ....
    end

They are called threadproc because they were created to implented threaded-code handlers for an interpreter. There, you would jump to such a procedure, and then jump to the next, without using normal call and return.

For such reasons, parameters and local frame variables are not possible (only static ones).

### Macros

Simple macros without parameters were a feature in the past, but are not gradually being deprecated.

One reason is that they don't work well with a whole-program compiler. A macro will have program-wide scope, but it also depends on compilation order, which is not always clear.

But at the minute, they still exist. They just need to be given unique names:

    define getopnda = (pcptr+offseta)^

They are most of use with the inline assembler.

### Properties

Ways of getting information about a type or expression:

    .len        Length of array
    .lwb        Lower bound of array
    .upb        Upper bound of array
    .bytes      Byte-size of type or value (sizeof in C)
    .bitwidth   Number of bits in primitive type
    .minvalue   Lowest value of type, or type of expresssion (ints only)
    .maxvalue   Highest value of type, or type of expresssion
    .type       Internal code for the type (an integer)
    .typestr    Name of the type

Used like this:

    for i := a.lwb to a.upb do

### Type Constants

Free-standing types such as 'int' are no longer allowed in expressions, they have to be written as 'int.type'. They can be used like this:

    if a.type = int.type then ...

### Operator Constants

An operator (+, - etc) enclosed in parenthese, returns an internal code the operator as an int:

    op := (+)
    ...
    case op
    when (+) then
    ...
This can be done in other ways (eg. just using '+') but it's handy to have it built-in, and it matches the same feature in Q, where has some support, eg:

    applyop(op,3,7)         # result 7 when op is ('+')

Note that augmented ops such as +:=, which are two tokens, will have their own code when written as (+:=).

This feature is not available for ops such as len or []. (If ever operator overloading is added, then they must be treated as other operators.)


