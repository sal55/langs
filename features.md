**M Language Features**

A list of M features mainly expressed as improvements (IMO) over the equivalents in C.

**Block Delimiters**

C braces eliminated in favour of Algol68-style blocks:
```
if a = b then
    x := y
    w := z
endif          ! or 'end', 'end if' or 'fi'
```

C's {,} I consider full of problems:

* Too thin and anaemic to be used across multiple lines
    
* The same } delimiter is used for all types of blocks (for, if etc) leading to errors
    
* {,} are optional for 0 or 1 statements, so that a missing brace doesn't immediately
      register, and is not detected if there is another missing or misaligned brace
      
* {,} clutter the syntax as there is nowhere to tidily put them out of the way
    
* There are myriad different placement styles
    
* With optional braces, it is tempting to add, say, a printf statement just before the
      x=300 line here, which would be incorrect:
```
    if (cond)
        x=300;
```
* And this kind of error is hard to spot: 
```
    if (cond)
        x = 100;
    else
        y = 200;
        z = 300;
```
* Or it leads to a dangling else problem:
````
    if (cond1)
        if (cond2)
            stmt1;
    else
        stmt2;
````

**Semicolons**

While M also uses semicolons, they are used to *separate* not terminate statements.
And because M interprets most end-of-lines as semicolons, it means they are virtually
eliminated from source code.

**Comments to end-of-line**

M uses the single character "!" in place of C's "//". ("#" is also recognised, but that
tends to be used for doc-strings.)

Both C's "//" and "/*...*/" are full of obscure bugs and quirks (see the C Quirks list).

**Numeric Literals**

M allows numbers to be written in any base from 2 to 16, using a consistent syntax:
```
    2x1101
    8x377
    0x7FFF
```
It also allows leading zeros on decimal numbers, and allows separators:
```
    0102          # means 102 not 66
    0x7FFF_FFFF   # using separator
    1000'000      # alternates
```

C only allows the '0x' scheme for hexadecimal. Octal numbers require leading zeros, which
can lead to surprises if someone enters 0163 (to line up figures in columns for example).
Binary literals may or may not be supported. And long numbers can't be broken up with
separators.


**Numeric Multipliers**

Numeric literals in M can be followed by one of a set of special multiplier names:
```
5 million           # means 5000000
6 billion           # means 6000000000
7.5 m               # means 7500 when the standard unit is 'mm'
20 msec             # means 0.020 when standard unit is 'second'
```
Such names can still be used as normal identifiers.


**Basic Numeric Types**

M uses the following simple set of numeric types:
```
    int           # signed int
    word          # unsigned int
    char          # (unsigned) char type
    real          # floating point type
 
    dint          # double the width of int
    dword         # double the width of word

```
Default widths are not specified but on any consumer machine it will be 32-bit for int/word,
and 64-bit for real. char is always 8-bit.

C uses char (which might be signed, or unsigned, with a default that can be signed or
unsighed); signed; unsigned and int. ints can be short, long, or long long, but none of
these is guaranteed to actually do anything. If stdint.h is available, then it might
have uint32_t etc. It's a mess.

For specific widths, M uses the following set of designations which are built-in:
```
            Alias
 int8                 Signed integers of 8, 16, 32, 64 bits
 int16
 int32      int
 int64      dint
 
 word8      byte      Unsigned integers
 word16
 word32     word
 word64     dword
 
 char8      char      Character types (always unsigned)
 char16
 char32
 
 real32               Floating point
 real64     real
 ```
 
 A 'byte' type is provided for convenience. Unlike C's char, this is always unsigned.
 
 Unlike C's char, with M's char you can't do char+char or char\*int.
 
 **Numeric Type Limits**
 
Attributes .minvalue and .maxvalue applied to type or expression, give the limits of
the type:
```
    byte.maxvalue          # 255
    int16.minvalue         # -32768
    word a
    a.maxvalue             # 4'294'967'296
```
In C, there are headers containing a big bunch of macros such as INT_MAX. You can't just apply
an attribute to a type (which can be a typedef or macro), or to an expression.

**Named Constants**

A named constant is simply a name attached to a literal value, such as an integer, float or string.

It's something that C just doesn't have, instead making do with a mish-mash of using '#define',
'enum' or defining a normal variable but with 'const'. M's named constants:

* Obey normal scope rules (unlike C's #define)
    
* Can have a type (unlike C's #define, while C's 'enum' only does int)
    
* Cannot have address-of applied (unlike C's const variables, where the values
      can also be changed with devious code) since they are just like actual literals
      
* Can be used for array dimensions or as switch when/case expressions (unlike C's const
      variables)


**Arrays**

M arrays differ from C's on the following points:

* Arrays can have any lower bound, not just zero
    
* Arrays are always handled by value; arrays names don't 'decay' to pointers
    
* Arrays can be passed to functions by value (I'll have to check the feature is complete.
      I know it isn't with the C target. However arrays rarely are passed by value.)
    
* Arrays and pointers are completely distinct
    
* When passed to a function by pointer, then it is a pointer to the array, not an element
    
* You can't do A^ or (A+i)^ (ie. *A or *(A+i) in C) when A is an array
    
* You can only do A[i] on an actual array (not on a pointer as in C)
    
* An array's length can be determined with A.len (not sizeof(A)/sizeof(A[0]) as in C)
    
* There are also A.lwb, A.upb, A.bounds


Array dimensions can be specified as follows:
```
    [length]int A            # bounds 1 to length
    [lower..upper]int A      # bounds lower to upper
    [lower:length]int A      # bounds lower..lower+length-1
    []int A                  # bounds 1..(unbounded)
    [lower:]int A            # bounds lower..(unbounded)
```
Multi-dimensional arrays are also indexed as A[i,j,k], which is easier to type and
looks nicer than C's A[i][j][k]. (Although the latter will also work in M).

**Pointers**

M's pointers are quite distinct from arrays:
```
    ref int p
```
This is a pointer to an int, or a block of ints. It can be dereferenced using p^ or
(p+offset)^, but never with p[i]. Pointers to dynamic arrays must be done something like
this:
```
    ref[]int p := malloc(1000*int.bytes)
```
This must always be indexed as p^[i].


**Type Declarations**

M uses an Algol68-inspired left-to-right type syntax:
```
    int A           # int
    ref int B       # pointer to int
    [10]int C       # array 10 of int
    ref[]int D      # pointer to array of int
```
This is very easy to write and to read. An example in C from K&R2 p.122:
```
    char (*(**x[3])())[5];
```
x is 'array 3 of pointer to function returning pointer to array 5 of char'. In M:
```
    [3]ref function => ref [5]char x
```
The M was directly transcribed from the English. In fact it can also be written:
```
    [3]pointer to function => pointer to [5]char x
```
It wouldn't be hard to allow 'array' and 'returning' to have something that reads
exactly like English. And apart from the readability:
```
    ref int p, q
```
defines exactly two pointers. But writing 'int\* p,q' in C gets you one pointer
and one int, not two!
    
**Function Syntax**

All function definitions start with either **function** or **proc**. This makes them stand
out, unlike C where it is hard to discern where functions start and end as the beginning
of function definition looks exactly like the start of a variable declaration.

M also uses **proc** for functions that return void in C. It makes it much more obvious what
it does.
    
**Function Parameters**

There are these differences from C:

* Parameters can be optional with a default value provided
    
* Parameters can be specified by 'keyword'
    
* Value arrays can be passed
    
* Proper reference parameters are available

In addition, when several parameters share the same type, it is not necessary to repeat
the type:
```
    function fn(int a,b,c, real x,y)int ...
```

**Function Definitions**

M functions can be declared in any order in a module. No forward declarations (or
prototypes) are needed. The signature of any function is written only in one place.

To share a function with other modules, a 'global' attribute is added:
```
    global proc fn(int a,b,c) ...
```
**Importing and Exporting Names**

One thing about C is that any names (of variables or functions) defined at file-scope
are automatically exported, unless 'static' is used.

With M, that never happens unless 'global' is added at the start.

**Module System**

M uses has a module system where, to share resources such as types, functions, variables,.
enums, structs, and named constants, then 'global' is added in front to export them.

To use the globals in module A from another B, then use:
```
    import A
```
in module B.

All resource are only ever declared or defined once. No headers are needed (they exist
but are handled automatically).

To go with the modules, a namespace scheme can be used, so then if function fn() is
exported from A, then A.fn() can be written to disambiguate it from any other fn().
(But this is optional is there is only one global fn().)

**Standard Libraries**

There are far fewer standard libraries compared with C, and they are handled with imports:
```
import clib           # access C library functions
import mlib           # M's own memory allocation and file functions
import oslib          # OS-specific functions
import osdll          # Do runtime accesses to dynamic library functions
```
A program written using only C library functions just needs import clib. (clib.m
at the minute only contains a selection of C functions. But they are easy to add.)

**Name Scopes**

C gives you one level of scope outside a function, but goes completely crazy inside
functions with an infinite number of possible scopes, one per block. (And in C Quirks,
I showed that up to five scopes can exist inside one block, for the *same* identifier.)

M has one scope at module scope and **one** scope inside functions. Very simple.

C also allows a separate namespace for struct tags. And a separate namespace for labels;
M has labels sharing the same namespace as other names in a function. And struct tags
don't exist.

**Struct Definitions**

M is stricter than C in that named structs can only be defined as part of their own
type definition. C allows this:
```
   typedef struct _S {
        struct {int x,y;} P, Q;
   } S;
```
An anonymous struct for P and Q. In M, it must be defined like this:
```
    type Point = struct
        int x,y
    end
    type S = struct
        Point P,Q
    end
```
However, unnamed unions and structs, used to control layout, are allowed.

**Encapsulation via Structs**

Another feature of M's struct is the ability to define named constants, types,
enums, static variables and functions within the struct.

Then a struct starts to look a little like a class, but it stops there:
there is no inheritance, there are no methods, and no entities that are stored
with instances.

It's just a convenient way to package a bunch of resources together. However it
means having to qualify each use of the name:
```
    type foo = struct
        const size=100
        proc sayhello = println "Hello" end
    end
    print foo.size          # 100
    foo.sayhello()          # "Hello"
```
**Loop Features**

M has a dedicated interative loop:
```
    for i:=a to b do ...
```
which is simpler than writing out C's: 'for (i=a; i<=b; ++c)' where you have to write
the loop variable 3 times, tell it the compare op to use (<=), and even tell it to
increment the loop variable!

Parts of this can be left out to give these loops:
```
    for i to b do ...           # count from 1
    to n do  ....               # loop n times
    do ...                      # loop endlessly
```
Or parts can be added:
```
   for i to b when A[i] do ...  # iterate only when A[i] is true
```
There are also the while and repeat loops. However other features not found in C are:

* for and while loops can have an 'else' part executed on normal exit
    
* Apart from exit and next (C's break and continue) there are 'restart' and 'redo'
      loop controls
      
* Loop controls can be used in nested loops to any level
    
* exit (C's break) can be used inside a switch


**Switch and Case Statements**

Unlike C, 'switch' is a proper structured, well-formed statement (see C Quirks for C's
version):

* 'break' is not needed after each case-block; there is no fall-through like C
   
* 'when' expressions (case labels in C) can include a list or range instead of
     having to be written individually:
        when 10,12,14 then
        when 20..30 then
        
* A 'doswitch' version loops after each when-block
   
* There is a version called 'case' (and 'docase') that works with non-integer and non-constant
     expressions


**Label Expressions**

It is possible to have pointers to labels, and to goto a pointer expression (gcc has this
as an extension)

**If/Unless Statements**

If statements have a proper 'elsif' clause unlike C (which uses if-else-if which requires some care).

The 'unless' is the reverse of 'if' as expected (but doesn't use elsif).

(M uses 'else' across several statements: if, unless, case, and switch (apart from loops).
It's possible actually to chain them together, to avoid extra indent levels:
```
    if cond then
    elsecase
    when ...
    elseswitch ...
    else
    end
```
Although this is rarely used.)

Conditional statements can also be applied in suffix form, which sometimes reads better:
```
    stop if a=b
    exit when i=5
    return unless cond
```
They can only be applied to simple statements like the above.

**Print Statements**

These are actual statements in M:
```
    println a,b,c         # might display 10 20 32.5
    println =a,=b,=c      # might show A=10 B=20 C=32.5
```
No C-style format codes are needed because the compiler knows what the types are!
This means also they don't need to be maintained as types change.

(Print/println are not fully implemented in terms of formatting control for field widths
etc. Nor are read/readln implemented. Most of this stuff is covered in the companion
Q language.)

**Operators**

M uses ":=" for assignment, and "=" for equality. This saves the mixups that can occur in C.

M also uses only 6 operator precedence levels, of which 5 correspond to the 10 levels of
C's binary operators. And they are easy to remember:
```
   1 **                   (pow() in C)
   2 * / rem << >>        (rem is % in C)
   3 + - iand ior ixor    (last three are & | ^ in C)
   4 = <> < <= => >       (= is == on C, and <> is !-)
   5 and                  (&& in C)
   6 or                   (|| in C)
```
Operators not in C include:
```
    x ** y        pow(x,y) in C
    min(x,y)      minimum of x,y; can be used also as x min:= y
    max(x,y)      maximum of x,y
    swap(x,y)     exchange x,y (must be lvalues)
    sqr(x)        x*x
```
Maths functions such as sin and cos are actually operators in M.

C can combine assignments with some binary operators, such as += (+:= in M). M can do
the same with these unary ops:
```
  -:=a          ! a:=-a
  abs:=a        ! a:=abs a
  not:=a        ! a:=not a          (!a in C)
  inot:=a       ! a:=inot a         (~a in C)
```

**Conditional Expressions**

C's 'a ? b : c' is written in M as '(a | b | c)'. The parentheses are needed.

There is also: '(n | a, b, c, ... | z)'. This returns the n'th expression from
the list, or z is n is out of range (1-based). Only one expression is evaluated.

Unlike C, both above expressions can be used on the left-hand-side of an assignment:
```
    (cond|x|y) := 0
```

**Chained Compares**

Chained comparisons such as a=b=c, a\<b\<c\>=d are allowed. The former means
a==b && b==c in C. For a C-style compare, then a=(b=c) is needed to break it up.
(In the current implementation, middle terms are evaluated twice.)

**Dereferencing**

In M, dereferencing of pointers requires exactly the right number of dereference
ops as there are in the type.

This quite unlike C which a lot of the time doesn't seem to care: A is a normal array
of int; B is a pointer to int; yet both are accessed as A[i] or B[i].

Or f is a function, and g is a function pointer, but both are called as f() or g().
Or you can choose to do (\*\*\*\*\*\*\*f)(); C doesn't care!

M also uses the postfix "^" symbol to dereference, like Pascal, which means less need
for parentheses:
```
    A^[i]            # A *must* be a pointer to an array
    f^()             # f *must* be a pointer to function
    P^.m             # P *must* be a pointer to a struct
```

In C, the last would be (\*P).m, which is ugly enough
that the special notation P->m is used instead. But
if you have Q as a pointer to pointer to struct, then
you now write is as: (\*\*Q).m or (\*Q)->m. It's a mess.
M will just use: Q^^.m.

**Address-of and Dereference Ops**

M allows address-of and dereference operators to cancel each other out with none of the restrictions
present in C:
```
    &p^^        # same as p^
    &&&p^^^     # same as p
```
Each & cancels one ^. In C, only the &\*p pattern cancels. Not & &\*\*p or \*\*& &p; it has
to be & \*&\*p (successive &s also need to be separated by white space).

Furthermore, & can be used outside a conditional expression:
```
    &(cond | a | b)          # same as (cond | &a | &b)
    &(n | a,b,c | z)         # same as (n | &a,&b,&c | &z)
```
This can't be done in C.

**'Comma' Operator**

This doesn't exist in M (not using commas anyway), leading to less confusion
and removing the temptation to make use of it.

In M, comma is used exclusively as a list separator. (And as a list separator,
M allows a trailing comma on the last element of a list in an expression to ease
development. Not however at the end of a parameter list.)

When the equivalent of a C comma expression such as a,b,c is needed in M, then it's
written like this:
```
    while (a; b; c) do...
```
Parenthese are needed, and semicolons are used as separators.

**Standalone Expressions**

C allows any expression to written as a standalone statement, whether it's
meaningful or not.

Increments, assignments are function calls are things that can occur in expressions
(returning a value) but are also useful by themselves (the return value if any
is ignored). But C also allows:
```
    a;
    a==b;
    a+b;
```
The chances are that the above are errors, but it is legal C so it cannot be detected
(although doubtless there are obscure options on some compilers to do that).

In M, such expressions are illegal:
```
    ++a            # OK
    a:=b           # OK
    f(a)           # OK
    a              # error
    a=b            # error (this one happens a lot)
    a+b            # error
```
If such an expression *is* needed as statement, then it is written like this:
```
    eval a+b
```

**Mixed Arithmetic**

Mixed signed/unsigned arithmetic will use signed, leading to fewer problems
compaed with C's decision to use unsigned. (Not currently enforced with a C target)

**Bit and Bitfield Extraction**

The syntax A.[i] can be used to extract a bit from an integer (shifting and
masking at the same time).

Similarly, A.[i..j] extracts a bit-field. (With the C target, i..j need to
be in the right order when not constant.)

(Currently only supported for rvalues. Use as lvalues would require bit/bitfield
pointers.)

**Enum Names**

Enum names can be 'open' as they are in C, or can be tied to an enum type.
Then, the same name can be used in several enum types, but needs qualifying, eg.
colours.green, lights.green. (The type system could have taken care of this
but is not sophisticated enough to do so.)

**Nested Functions**

There is a simple implementation of nested functions. There are restrictions:
a nested function can't access non-static frame variables of outer functions.

**Function Metadata**

(Not fully implemented.) This is the ability to have a list of function
names (with addresses and any string metadata) as part of the modules data.
This allows runtime scanning of the functions in a module.

**Include and Strinclude**

'include' files work as they do in C, but are rarely used because 'import' is
used instead ('import' is implemented on top of include). But there is a new
'strinclude' which turns any text file into a string constant, inserting
escape codes as needed.

**Table Data**

There is also an odd feature which can define sets of parallel data, including
auto-generating enum constants. It's used to define linked tables which are otherwise
awkward in C:
```
tabledata() []ichar colournames, []ref proc colourhandlers =
    (red,    $,    &do_red),
    (green,  $,    &do_green),
    (blue,   $,    &do_blue)
end
```
This defines red=1, green=2 etc; a set of matching names ("$" is a device
that expands the last defined enum to a string such as "red"); and an array
of function pointers.

**Casts**

Casts still exist but are written T(X) instead of (T)X. This makes it
clearer exactly what the cast is operating on.

When a cast is necessary, then for convenience it is possible to write:
 cast(X), which then applies whatever cast is required. This aids maintenance
when types change.

Typing punning is possible in C using the construct \*(T\*)&X. This can
be written using T@(X), just like a normal cast, but with @. In both cases,
however, it will only work on lvalues.

**Machine Types**

Special types intm, wordm and wordp exist where the width depends on the
target (either 32 or 64 bits). A bit of a kludge but were needed to get
the same code to compile to x64 asm or 32-bit C target.

**Sizes**

How C compares with M in determining byte and bit sizes of types and objects:
```
    C:                      M:
    
    sizeof(X)               X.bytes          Size in bytes
    sizeof(A)/sizeof A[0]   A.len            Elements in array
    CHAR_BIT                char.bitwidth    Size in bits
    CHAR_BIT*sizeof(int)    int.bitwidth
```
For arrays, there are also, as mentioned above, A.lwb, A.upb for lower
and upper bounds. And A.bounds; this returns a 'range' type (see below).

**Range Type**

This is a type consisting of a pair of ints. It is little used and incomplete
so may dropped, but it works like this:
```
    eval 100..200      # a range value (64 bits in total)
    range R:=30..40    # declare a range
    R.lwb              # lower limit (30)
    R.upb              # upper limit (40)
    R.len              # inclusive range (11)
    if A in R then    # check whether an int value in in range
```

**Stop**

A 'stop' statement is used to immediately terminate a program. stop 100 is the
same as exit(100); in C, but is built-in:
```
    stop            # exit(0) in C
    stop(100)       # exit(100)
```
**Equivalence**

This is like the old Fortran feature where two variables can share the same memory:
```
real x
int a @ x
```
**Inline Assembly**

Inline assembler is available for the x64 target. This co-exists easily
with the high-level code as it's not necessary to worry about registers,
or official calling conventions, or optimisations. Contrast with gcc version.
(Not available for C target.),

**Typeof**

'typeof(a)' can be used to access the type of a variable for use in a declaration
or cast.

**Foreign Functions**

The existence of other languages and conventions is acknowledged. Attributes such
as "clang" and "windows" exist when declaring foreign functions.

**Raw String Literals**

There is a raw string syntax: F"C:\m\abc.m" where the "\" escape character is
treated as an normal character.

**Char Constants**

Multi-character literals such as 'ABC' are well-defined: the layout in memory
exactly matches that of the equivalent string "ABC". Such literals can have up to
eight 8-bit characters, yielding a 64-bit unsigned type.

**Program Entry Point**

While there is usually a 'main' entry point for compatibility, this main() function
is in a library. The entry point in applications is expected to be 'start', taking no
parameters. (Command line params are set up in a global array of strings.)
